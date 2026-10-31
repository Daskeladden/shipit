;;; test-shipit-gh-etag-url-cache.el --- shipit's url-cache-extract stays -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; url.el answers 304 Not Modified with `url-cache-extract', which
;; reads the cached copy of the answer.  shipit asks GitHub with its
;; own ETags and keeps no such copy, so its ETag layer replaces that
;; function with one that leaves the answer as it is.  url-http loads
;; url-cache.el only at the first 304, and that load put the function
;; of url.el back: the 304 then failed in the process filter, and the
;; callback of the request was never called.

;;; Code:

(require 'ert)
(require 'shipit-gh-etag)

(ert-deftest test-sgeuc-url-cache-extract-stays-shipits ()
  "GIVEN shipit's ETag layer
WHEN url.el loads url-cache.el, as url-http does at the first 304
THEN `url-cache-extract' is still the one of shipit."
  (require 'url-cache)
  (should (string-match-p "shipit-gh-etag"
                          (symbol-file 'url-cache-extract 'defun))))

(ert-deftest test-sgeuc-a-missing-copy-leaves-the-answer ()
  "GIVEN an answer 304 Not Modified, and no cached copy of it
WHEN url.el extracts the copy
THEN nothing fails, AND the answer stays in its buffer."
  (with-temp-buffer
    (insert "HTTP/1.1 304 Not Modified\n\n")
    (let ((url-cache-directory (make-temp-file "shipit-url-cache-" t)))
      (unwind-protect
          (progn
            (url-cache-extract (url-cache-create-filename "https://api.github.com/x"))
            (should (string-prefix-p "HTTP/1.1 304" (buffer-string))))
        (delete-directory url-cache-directory t)))))

(provide 'test-shipit-gh-etag-url-cache)
;;; test-shipit-gh-etag-url-cache.el ends here
