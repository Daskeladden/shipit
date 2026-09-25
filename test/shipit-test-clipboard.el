;;; shipit-test-clipboard.el --- Put things on the clipboard in tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; `yank-media' reads the clipboard, which a batch run does not have.
;; A test puts what it wants to paste there with
;; `shipit-test-clipboard-holding', the clipboard being the boundary
;; stood in for.

;;; Code:

(require 'cl-lib)

(defun shipit-test-clipboard--reader (contents)
  "A reader of a clipboard holding CONTENTS, an alist of (TYPE . DATA)."
  (lambda (_selection type)
    (if (eq type 'TARGETS)
        (vconcat '(TARGETS) (mapcar #'car contents))
      (cdr (assq type contents)))))

(defmacro shipit-test-clipboard-holding (contents &rest body)
  "Run BODY with the clipboard holding CONTENTS, an alist of (TYPE . DATA).
TYPE is a MIME type symbol such as `image/png'.  `yank-media' asks
`gui-get-selection' what the clipboard holds, and Emacs 29 reads the
data itself through `gui-backend-get-selection', so both answer.
Another program owns the clipboard, as when something is copied there."
  (declare (indent 1))
  (let ((reader (make-symbol "reader")))
    `(let ((,reader (shipit-test-clipboard--reader ,contents)))
       (cl-letf (((symbol-function 'gui-get-selection) ,reader)
                 ((symbol-function 'gui-backend-get-selection) ,reader)
                 ((symbol-function 'gui-backend-selection-owner-p) #'ignore))
         ,@body))))

(provide 'shipit-test-clipboard)
;;; shipit-test-clipboard.el ends here
