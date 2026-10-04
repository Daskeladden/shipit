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

(provide 'test-shipit-test-http)
;;; test-shipit-test-http.el ends here
