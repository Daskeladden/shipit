;;; test-shipit-pr-mouse-preview.el --- The PR preview on mouse-over -*- lexical-binding: t; -*-

;;; Commentary:
;; When the mouse rests on a pull request link in a shipit buffer, the
;; preview of the pull request opens next to it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)

(defun test-mouse-preview--with-backend (plist body)
  "Run BODY with a PR backend registered from PLIST."
  (let ((shipit-pr-backend 'test-mouse-preview)
        (shipit-pr-backends (list (cons 'test-mouse-preview plist))))
    (funcall body)))

;;;; Fetching a pull request in the background

(ert-deftest test-shipit-pr-fetch-async-without-the-operation-starts-nothing ()
  "GIVEN a backend with no :fetch-pr-async, as GitLab has none
WHEN a pull request is fetched in the background
THEN nothing starts, the answer is nil, AND the callback is not called."
  (let ((called nil))
    (test-mouse-preview--with-backend
     '(:name "Waits")
     (lambda ()
       (should-not (shipit-pr--fetch-pr-async "o/r" 7 (lambda (_) (setq called t))))
       (should-not called)))))

(ert-deftest test-shipit-pr-fetch-async-passes-the-config-and-the-number ()
  "GIVEN a backend with a :fetch-pr-async
WHEN pull request 7 of o/r is fetched in the background
THEN the operation gets the config of o/r, the number and the callback,
AND the answer is non-nil."
  (let ((got nil)
        (callback (lambda (_) nil)))
    (test-mouse-preview--with-backend
     (list :name "Async"
           :fetch-pr-async (lambda (config number cb) (setq got (list config number cb))))
     (lambda ()
       (should (shipit-pr--fetch-pr-async "o/r" 7 callback))
       (should (equal "o/r" (plist-get (nth 0 got) :repo)))
       (should (equal 7 (nth 1 got)))
       (should (eq callback (nth 2 got)))))))

(ert-deftest test-shipit-pr-github-fetch-async-gives-the-pull-request ()
  "GIVEN the GitHub backend, and a background request that answers with PR 7
WHEN pull request 7 of o/r is fetched in the background
THEN the request asks the pulls endpoint, AND the callback gets the alist."
  (let ((endpoint nil)
        (got 'unset))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (requested _params _token callback &rest _)
                 (setq endpoint requested)
                 (funcall callback '(:json ((number . 7)) :status 200)))))
      (shipit-pr-github--fetch-pr-async '(:repo "o/r") 7 (lambda (pr) (setq got pr)))
      (should (equal "/repos/o/r/pulls/7" endpoint))
      (should (equal '((number . 7)) got)))))

(ert-deftest test-shipit-pr-github-fetch-async-gives-nil-when-the-request-fails ()
  "GIVEN the GitHub backend, and a background request that fails, as it
does for the number of an issue
WHEN the number is fetched in the background
THEN the callback gets nil."
  (let ((got 'unset))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (_requested _params _token callback &rest _)
                 (funcall callback '(:error . (error http 404))))))
      (shipit-pr-github--fetch-pr-async '(:repo "o/r") 9 (lambda (pr) (setq got pr)))
      (should-not got))))

(provide 'test-shipit-pr-mouse-preview)
;;; test-shipit-pr-mouse-preview.el ends here
