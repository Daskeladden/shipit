;;; shipit-test-meow.el --- Stand in for meow in tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; meow is not installed where the tests run.  A file of shipit says
;; what it wants of meow in a `with-eval-after-load' form, which runs
;; when meow says that it is loaded.  `shipit-test-with-meow-loaded'
;; says so in its place, meow being the boundary stood in for.

;;; Code:

(require 'cl-lib)

(defmacro shipit-test-with-meow-loaded (&rest body)
  "Run BODY as when meow has just loaded.
`meow-mode-state-list' and `undo-tree-incompatible-major-modes' hold
only what shipit put there.  `cl-progv' binds the two as the variables
of the packages that they are, whatever the file of BODY declares."
  (declare (indent 0))
  `(cl-progv '(meow-mode-state-list undo-tree-incompatible-major-modes) '(nil nil)
     (cl-assert (not (featurep 'meow)) nil "The real meow is loaded")
     (unwind-protect
         (progn (provide 'meow) ,@body)
       (setq features (delq 'meow features)))))

(provide 'shipit-test-meow)
;;; shipit-test-meow.el ends here
