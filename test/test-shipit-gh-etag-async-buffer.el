;;; test-shipit-gh-etag-async-buffer.el --- An async request leaves no buffer behind -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; `url-retrieve' hands its callback a response buffer and leaves the
;; killing of it to the caller.  The async ETag request never did, so
;; every poll of the notifications left a buffer holding the whole
;; response.  A session that stays up for days ends with thousands of
;; them, and every garbage collection walks them all.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-gh-etag)

(defun test-sgeab--request (response)
  "Run an async ETag request answered with RESPONSE.
Return (RESULT . BUFFER): what the callback received, and the response
buffer `url-retrieve' gave it."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        result buffer)
    (cl-letf (((symbol-function 'shipit-gh-etag--ensure-cache-loaded) #'ignore)
              ((symbol-function 'shipit-gh-etag--proactive-cache-cleanup) #'ignore)
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq buffer (generate-new-buffer " *http test*"))
                 (with-current-buffer buffer
                   (set-buffer-multibyte nil)
                   (insert response)
                   (funcall callback nil))
                 buffer)))
      (shipit-gh-etag-get-json-async "/notifications" nil "token"
                                     (lambda (r) (setq result r))))
    (cons result buffer)))

(ert-deftest test-sgeab-response-buffer-is-killed-after-200 ()
  "GIVEN an async ETag request
WHEN the server answers 200 with a JSON body
THEN the callback gets the parsed body and the response buffer is gone."
  (pcase-let ((`(,result . ,buffer)
               (test-sgeab--request
                "HTTP/1.1 200 OK\nETag: W/\"abcdef\"\n\n[{\"id\": \"1\"}]")))
    (should (equal (plist-get result :status) 200))
    (should (equal (plist-get result :json) '(((id . "1")))))
    (should-not (buffer-live-p buffer))))

(ert-deftest test-sgeab-response-buffer-is-killed-after-an-error-status ()
  "GIVEN an async ETag request
WHEN the server answers with an error status
THEN the callback is told and the response buffer is gone."
  (pcase-let ((`(,result . ,buffer)
               (test-sgeab--request "HTTP/1.1 502 Bad Gateway\n\n")))
    (should (equal (plist-get result :status) 502))
    (should-not (buffer-live-p buffer))))

(ert-deftest test-sgeab-response-buffer-is-killed-when-the-callback-fails ()
  "GIVEN an async ETag request whose callback signals an error
WHEN the response arrives
THEN the response buffer is gone all the same."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        buffer)
    (cl-letf (((symbol-function 'shipit-gh-etag--ensure-cache-loaded) #'ignore)
              ((symbol-function 'shipit-gh-etag--proactive-cache-cleanup) #'ignore)
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq buffer (generate-new-buffer " *http test*"))
                 (with-current-buffer buffer
                   (insert "HTTP/1.1 502 Bad Gateway\n\n")
                   (ignore-errors (funcall callback nil)))
                 buffer)))
      (shipit-gh-etag-get-json-async "/notifications" nil "token"
                                     (lambda (_) (error "Callback failed"))))
    (should-not (buffer-live-p buffer))))

(provide 'test-shipit-gh-etag-async-buffer)
;;; test-shipit-gh-etag-async-buffer.el ends here
