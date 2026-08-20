;;; test-shipit-magit-requires.el --- Guard magit-section requires -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;;; Commentary:

;; Regression tests for a class of bug that only manifests in
;; native-compiled builds.
;;
;; `magit-insert-section' and friends are MACROS.  A file that expands
;; them without requiring `magit-section' compiles correctly only by
;; accident -- when some earlier file in the same batch happened to pull
;; magit-section in.  Emacs native-compiles each file in an isolated
;; subprocess, where that accident does not happen: the macro is unknown,
;; so the compiler emits an ordinary function call and evaluates the
;; argument list.  `(magit-insert-section (general-comment id) ...)' then
;; runs `(general-comment id)' and signals `void-function general-comment'.
;;
;; The section object is never created, so every downstream
;; `(oset section ...)' receives nil, surfacing as
;;   eieio-oset@closql-object: Wrong type argument: ..., nil
;;
;; Every file that expands these macros must require magit-section itself.

;;; Code:

(require 'ert)

(defconst test-shipit-magit--lisp-dir
  (expand-file-name
   "../lisp"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding the shipit sources under test.")

(defconst test-shipit-magit--macro-re
  (rx "(" (or "magit-insert-section-body"
              "magit-insert-section"
              "magit-insert-heading")
      (or " " "\n" "("))
  "Regexp matching an expansion site of a magit-section macro.")

(defconst test-shipit-magit--require-re
  (rx bol "(require '" (or "magit-section" "magit") ")")
  "Regexp matching a top-level require that defines the magit macros.")

(defun test-shipit-magit--source-files ()
  "Return the shipit source files that must be checked."
  (seq-remove (lambda (f) (string-match-p "autoloads" f))
              (directory-files test-shipit-magit--lisp-dir t "\\.el\\'")))

(defun test-shipit-magit--file-contents (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun test-shipit-magit--expands-macros-p (contents)
  "Return non-nil when CONTENTS expands a magit-section macro."
  (string-match-p test-shipit-magit--macro-re contents))

(defun test-shipit-magit--requires-magit-p (contents)
  "Return non-nil when CONTENTS requires magit-section at top level."
  (string-match-p test-shipit-magit--require-re contents))

(defun test-shipit-magit--offenders ()
  "Return files expanding magit macros without requiring magit-section."
  (seq-filter
   (lambda (file)
     (let ((contents (test-shipit-magit--file-contents file)))
       (and (test-shipit-magit--expands-macros-p contents)
            (not (test-shipit-magit--requires-magit-p contents)))))
   (test-shipit-magit--source-files)))

(ert-deftest test-shipit-magit-requires-declared ()
  "Every file expanding magit-section macros must require magit-section."
  (let ((offenders (mapcar #'file-name-nondirectory
                           (test-shipit-magit--offenders))))
    (should (equal offenders nil))))

;;; Isolated compilation -- the condition native-comp actually hits

(defun test-shipit-magit--compile-isolated (file)
  "Byte-compile FILE in a subprocess that preloads nothing.
Returns the compiler output as a string."
  (let ((out (make-temp-file "shipit-isolated" nil ".elc")))
    (unwind-protect
        (with-temp-buffer
          (call-process
           (expand-file-name invocation-name invocation-directory)
           nil t nil
           "-batch" "-Q"
           "-L" test-shipit-magit--lisp-dir
           "--eval" (prin1-to-string
                     `(setq package-user-dir
                            ,(expand-file-name
                              "../.packages" test-shipit-magit--lisp-dir)))
           "--eval" "(package-initialize)"
           "--eval" (prin1-to-string
                     `(byte-compile-file ,file ,out)))
          (buffer-string))
      (ignore-errors (delete-file out)))))

(defun test-shipit-magit--unknown-macro-warning-p (output)
  "Return non-nil when OUTPUT warns that a magit macro is an unknown function."
  (string-match-p "the function .magit-insert-\\(section\\|heading\\)."
                  output))

(ert-deftest test-shipit-magit-compiles-in-isolation ()
  "Each file must expand magit macros when compiled alone.
This mirrors the isolated subprocess Emacs uses for native compilation."
  (dolist (file (test-shipit-magit--source-files))
    (let ((contents (test-shipit-magit--file-contents file)))
      (when (test-shipit-magit--expands-macros-p contents)
        (let ((output (test-shipit-magit--compile-isolated file)))
          (should (equal
                   (list (file-name-nondirectory file)
                         (test-shipit-magit--unknown-macro-warning-p output))
                   (list (file-name-nondirectory file) nil))))))))

(provide 'test-shipit-magit-requires)
;;; test-shipit-magit-requires.el ends here
