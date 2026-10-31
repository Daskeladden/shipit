;;; test-shipit-test-http.el --- Tests answer url.el requests themselves -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; shipit-test-http.el answers the requests a test makes at url.el,
;; where every request of shipit passes, in place of the network.
;; These tests hold its answers to the shape url.el gives: shipit's own
;; request functions must read them as they read a real one.

;;; Code:

(require 'ert)
(require 'shipit-http)
(require 'shipit-test-http)

(ert-deftest test-http-answers-a-request-that-waits ()
  "GIVEN an answer for the endpoint of a repository
WHEN shipit asks for it and waits
THEN it reads the body and the status of the answer, AND the request is
recorded."
  (shipit-test-http-answering '(("/repos/o/r\\'" 200 ((name . "r"))))
    (should (equal '(((name . "r")) . 200)
                   (shipit--url-retrieve-sync "https://api.github.com/repos/o/r"
                                              "GET" nil nil)))
    (should (equal '(("GET" "https://api.github.com/repos/o/r"))
                   shipit-test-http-requests))))

(ert-deftest test-http-answers-a-request-that-does-not-wait ()
  "GIVEN an answer for one endpoint and an error for another
WHEN shipit asks for each without waiting
THEN the first reaches the success callback with its body, AND the
second reaches the error callback."
  (shipit-test-http-answering '(("/ok\\'" 200 [1 2]) ("/gone\\'" 404 ""))
    (let (got failed)
      (shipit--url-retrieve-async "https://api.github.com/ok" "GET" nil nil
                                  (lambda (data) (setq got data)) #'ignore)
      (shipit--url-retrieve-async "https://api.github.com/gone" "GET" nil nil
                                  #'ignore (lambda (err) (setq failed err)))
      (should (equal '(1 2) got))
      (should failed))))

(ert-deftest test-http-fails-when-a-response-is-left-alive ()
  "GIVEN code that asks, and does not kill the buffer of the answer
WHEN the form ends
THEN it signals, AND the buffer is gone, so that it does not fail the
next form."
  (should-error
   (shipit-test-http-answering '(("/thing" 200 ((ok . t))))
     (url-retrieve-synchronously "https://example.test/thing")))
  (should-not (shipit-test-http-live-responses)))

(ert-deftest test-http-fails-on-a-request-it-has-no-answer-for ()
  "GIVEN answers that do not cover a request
WHEN the code asks for it, and swallows the error it gets
THEN the form fails all the same, naming the address."
  (let ((err (should-error
              (shipit-test-http-answering '(("/ok\\'" 200 []))
                (ignore-errors
                  (shipit--url-retrieve-sync "https://api.github.com/other"
                                             "GET" nil nil))))))
    (should (string-match-p "https://api\\.github\\.com/other"
                            (error-message-string err)))))

(ert-deftest test-http-a-function-answers-from-what-was-sent ()
  "GIVEN an answer whose body is a function
WHEN shipit sends two requests to its endpoint, each with its own body
THEN the function gets what each request sent, AND each answer is what
the function gave for it."
  (shipit-test-http-answering
      `(("/graphql\\'" 200 ,(lambda (sent)
                              `((page . ,(cdr (assq 'page (json-read-from-string sent))))))))
    (let (got)
      (dolist (page '(1 2))
        (shipit--url-retrieve-async "https://api.github.com/graphql" "POST" nil
                                    (json-encode `((page . ,page)))
                                    (lambda (data) (push data got)) #'ignore))
      (should (equal '(((page . 1)) ((page . 2))) (reverse got))))))

(ert-deftest test-http-an-answer-gives-its-headers ()
  "GIVEN an answer with an ETag header
WHEN shipit asks for it
THEN the headers of the response hold that ETag."
  (shipit-test-http-answering '(("/refs\\'" 200 [] (("ETag" . "W/\"abc\""))))
    (let (got)
      (shipit--url-retrieve-async-with-headers
       "https://api.github.com/refs" "GET" nil nil
       (lambda (_data headers) (setq got headers)) #'ignore)
      (should (equal (cdr (assoc "ETag" got)) "W/\"abc\"")))))

(ert-deftest test-http-headers-can-come-from-a-function ()
  "GIVEN an answer whose headers are a function
WHEN shipit asks for it twice, and the function gives another ETag
the second time
THEN each response has the ETag that the function gave for it."
  (let ((etag "W/\"one\"")
        (got nil))
    (shipit-test-http-answering
        `(("/refs\\'" 200 [] ,(lambda () `(("ETag" . ,etag)))))
      (dotimes (_ 2)
        (shipit--url-retrieve-async-with-headers
         "https://api.github.com/refs" "GET" nil nil
         (lambda (_data headers) (push (cdr (assoc "ETag" headers)) got)) #'ignore)
        (setq etag "W/\"two\"")))
    (should (equal (reverse got) '("W/\"one\"" "W/\"two\"")))))

(ert-deftest test-http-a-request-for-what-did-not-change-is-not-modified ()
  "GIVEN an answer with an ETag
WHEN shipit asks for it with that ETag in If-None-Match
THEN the answer is 304 Not Modified, without a body, AND the request
is recorded with its headers
WHEN shipit asks with another ETag
THEN the answer is the body, with status 200."
  (shipit-test-http-answering '(("/refs\\'" 200 [1] (("ETag" . "W/\"abc\""))))
    (let ((statuses nil))
      (dolist (etag '("W/\"abc\"" "W/\"old\""))
        (let ((url-request-extra-headers `(("If-None-Match" . ,etag))))
          (with-current-buffer (url-retrieve-synchronously "https://api.github.com/refs")
            (goto-char url-http-end-of-headers)
            (push (list url-http-response-status
                        (string-trim (buffer-substring (point) (point-max))))
                  statuses)
            (kill-buffer))))
      (should (equal (reverse statuses) '((304 "") (200 "[1]"))))
      (should (equal (cdr (assoc "If-None-Match" (car shipit-test-http-headers)))
                     "W/\"abc\"")))))

(ert-deftest test-http-a-request-with-status-reads-not-modified ()
  "GIVEN an answer with an ETag
WHEN shipit asks for it with its status, once without that ETag and
once with it
THEN the first gets 200, the body and the ETag, AND the second gets 304
and no body
WHEN shipit asks with its headers only, with that ETag
THEN it reaches the error callback, as an answer without a body."
  (shipit-test-http-answering '(("/refs\\'" 200 [1] (("ETag" . "W/\"abc\""))))
    (let ((got nil)
          (failed nil))
      (dolist (sent '(nil (("If-None-Match" . "W/\"abc\""))))
        (shipit--url-retrieve-async-with-status
         "https://api.github.com/refs" "GET" sent nil
         (lambda (status data headers)
           (push (list status data (cdr (assoc "ETag" headers))) got))
         #'ignore))
      (should (equal (reverse got) '((200 (1) "W/\"abc\"") (304 nil "W/\"abc\""))))
      (shipit--url-retrieve-async-with-headers
       "https://api.github.com/refs" "GET" '(("If-None-Match" . "W/\"abc\"")) nil
       #'ignore (lambda (err) (setq failed err)))
      (should (equal failed "HTTP 304")))))

(provide 'test-shipit-test-http)
;;; test-shipit-test-http.el ends here
