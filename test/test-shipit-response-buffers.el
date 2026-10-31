;;; test-shipit-response-buffers.el --- A request leaves no buffer behind -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; url.el hands the buffer of a response to the code that asked, and
;; never kills it.  Code that does not kill it either leaves one buffer
;; for each request, for the life of Emacs.  A session that polled its
;; notifications for a day held several hundred of them.

;;; Code:

(require 'ert)
(require 'url-cache)
(require 'shipit-test-http)
(require 'shipit-gh-etag)
(require 'shipit-http)
(require 'shipit-comment-github)

(defconst test-shipit-response--issue "/repos/o/r/issues/1"
  "The endpoint the tests ask for.")

(defconst test-shipit-response--key "o/r/repos/o/r/issues/1"
  "The key under which the ETag cache holds `test-shipit-response--issue'.")

(defconst test-shipit-response--issue-url
  (concat "https://api.github.com" test-shipit-response--issue)
  "The address of `test-shipit-response--issue'.")

(defun test-shipit-response--write-url-cache (&optional body)
  "Write the file url.el keeps of an earlier answer for the issue.
The answer has BODY, or {} when BODY is nil.  shipit drops what it
holds for an address when that file is gone."
  (let ((file (url-cache-create-filename test-shipit-response--issue-url)))
    (make-directory (file-name-directory file) t)
    (write-region (concat "HTTP/1.1 200 OK\n\n" (or body "{}"))
                  nil file nil 'silent)))

(defmacro test-shipit-response--with-cache (entry &rest body)
  "Run BODY with an ETag cache that holds ENTRY for the issue, or nothing."
  (declare (indent 1))
  `(let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
         (url-cache-directory (expand-file-name "url/cache" user-emacs-directory))
         (shipit-github-token "token"))
     (when ,entry
       (puthash test-shipit-response--key ,entry shipit-gh-etag--persistent-cache)
       (test-shipit-response--write-url-cache))
     ,@body))

(defconst test-shipit-response--cached
  '(:etag "W/\"abcdef\"" :json ((number . 1)) :timestamp 0)
  "An entry of the ETag cache for the issue.")

(ert-deftest test-shipit-response-a-request-that-waits-leaves-no-buffer ()
  "GIVEN GitHub answers a request with data
WHEN shipit asks and waits for the answer
THEN it gets the data, AND the buffer of the answer is gone."
  (test-shipit-response--with-cache nil
    (shipit-test-http-answering '(("/repos/o/r/issues/1" 200 ((number . 1))))
      (let ((result (shipit-gh-etag-get-json test-shipit-response--issue nil "token" t)))
        (should (equal 1 (cdr (assq 'number (plist-get result :json)))))
        (should-not (shipit-test-http-live-responses))))))

(ert-deftest test-shipit-response-an-unchanged-answer-that-is-waited-for-leaves-no-buffer ()
  "GIVEN shipit holds the data of an earlier answer
AND GitHub answers that nothing changed
WHEN shipit asks and waits for the answer
THEN it gets the data it held, AND the buffer of the answer is gone."
  (test-shipit-response--with-cache test-shipit-response--cached
    (shipit-test-http-answering '(("/repos/o/r/issues/1" 304 ""))
      (let ((result (shipit-gh-etag-get-json test-shipit-response--issue nil "token")))
        (should (equal 1 (cdr (assq 'number (plist-get result :json)))))
        (should-not (shipit-test-http-live-responses))))))

(ert-deftest test-shipit-response-a-request-that-does-not-wait-leaves-no-buffer ()
  "GIVEN GitHub answers a request with data
WHEN shipit asks without waiting, as a poll of the notifications does
THEN the callback gets the data, AND the buffer of the answer is gone."
  (test-shipit-response--with-cache nil
    (shipit-test-http-answering '(("/repos/o/r/issues/1" 200 ((number . 1))))
      (let ((result nil))
        (shipit-gh-etag-get-json-async test-shipit-response--issue nil "token"
                                       (lambda (answer) (setq result answer)) t)
        (should (equal 1 (cdr (assq 'number (plist-get result :json)))))
        (should-not (shipit-test-http-live-responses))))))

(ert-deftest test-shipit-response-an-unchanged-answer-that-is-not-waited-for-leaves-no-buffer ()
  "GIVEN shipit holds the data of an earlier answer
AND GitHub answers that nothing changed
WHEN shipit asks without waiting
THEN the callback gets the data it held, AND the buffer of the answer
is gone."
  (test-shipit-response--with-cache test-shipit-response--cached
    (shipit-test-http-answering '(("/repos/o/r/issues/1" 304 ""))
      (let ((result nil))
        (shipit-gh-etag-get-json-async test-shipit-response--issue nil "token"
                                       (lambda (answer) (setq result answer)))
        (should (equal 1 (cdr (assq 'number (plist-get result :json)))))
        (should-not (shipit-test-http-live-responses))))))

(ert-deftest test-shipit-response-a-copy-that-url-el-takes-from-its-cache-leaves-no-buffer ()
  "GIVEN url.el holds a copy of an earlier answer for the issue
AND GitHub answers that nothing changed
WHEN url.el takes the copy from its cache, as it does on that answer
THEN no buffer is made for the copy, AND the answer still says that
nothing changed, so that shipit uses the data it holds."
  (test-shipit-response--with-cache nil
    (test-shipit-response--write-url-cache)
    (let ((before (buffer-list))
          (answer "HTTP/1.1 304 Not Modified\n\n"))
      (with-temp-buffer
        (insert answer)
        (url-cache-extract (url-cache-create-filename test-shipit-response--issue-url))
        (should (equal answer (buffer-string)))
        (should-not (remq (current-buffer) (seq-difference (buffer-list) before)))))))

(ert-deftest test-shipit-response-an-unchanged-answer-to-a-plain-request-gives-the-copy ()
  "GIVEN url.el holds a copy of an earlier answer for the issue
AND GitHub answers that nothing changed
WHEN shipit asks for the issue with a plain request and waits
THEN it gets the data of the copy, AND no buffer of the copy is left."
  (test-shipit-response--with-cache nil
    (test-shipit-response--write-url-cache "{\"number\": 1}")
    (let ((before (buffer-list)))
      (shipit-test-http-answering '(("/repos/o/r/issues/1" 304 ""))
        (let ((result (shipit--url-retrieve-sync test-shipit-response--issue-url
                                                 "GET" nil nil)))
          (should (eq 304 (cdr result)))
          (should (equal 1 (alist-get 'number (car result))))
          (should-not (seq-difference (buffer-list) before)))))))

(ert-deftest test-shipit-response-the-reactions-of-comments-leave-no-buffer ()
  "GIVEN two comments, AND GitHub answers with the reactions of each
WHEN shipit fetches the reactions of both and waits for them
THEN the reactions are kept, AND the buffers of both answers are gone."
  (test-shipit-response--with-cache nil
    (let ((shipit--reaction-cache (make-hash-table :test 'equal)))
      (shipit-test-http-answering
          '(("/issues/comments/[0-9]+/reactions" 200 (((content . "heart")))))
        (shipit-comment-github--fetch-reactions-batch
         '(:repo "o/r")
         '(((id . 111) (shipit-comment-type . "issue"))
           ((id . 222) (shipit-comment-type . "issue")))
         nil)
        (should (equal "heart"
                       (cdr (assq 'content
                                  (car (gethash (shipit--reaction-cache-key "o/r" 111 nil)
                                                shipit--reaction-cache))))))
        (should-not (shipit-test-http-live-responses))))))

(provide 'test-shipit-response-buffers)
;;; test-shipit-response-buffers.el ends here
