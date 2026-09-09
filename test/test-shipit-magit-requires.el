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
;;
;; EIEIO's `oref' and `oset' are the same trap with a quieter landing.
;; Unexpanded, `(oref section start)' is a call whose arguments are
;; evaluated, so it signals `void-variable start' -- which, raised inside
;; a magit washer, abandons the render halfway and leaves the section
;; tree without its children.

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

(defconst test-shipit-magit--eieio-macro-re
  (rx "(" (or "oref-default" "oset-default" "oref" "oset" "with-slots")
      (or " " "\n"))
  "Regexp matching an expansion site of an EIEIO slot macro.")

(defconst test-shipit-magit--eieio-require-re
  (rx bol "(require '" (or "eieio" "magit-section" "magit") ")")
  "Regexp matching a top-level require that defines the EIEIO macros.")

(defun test-shipit-magit--source-files ()
  "Return the shipit source files that must be checked."
  (seq-remove (lambda (f) (string-match-p "autoloads" f))
              (directory-files test-shipit-magit--lisp-dir t "\\.el\\'")))

(defun test-shipit-magit--file-contents (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun test-shipit-magit--expanders (macro-re)
  "Return the source files carrying an expansion site matching MACRO-RE."
  (seq-filter (lambda (file)
                (string-match-p macro-re
                                (test-shipit-magit--file-contents file)))
              (test-shipit-magit--source-files)))

(defun test-shipit-magit--offenders (macro-re require-re)
  "Return the files matching MACRO-RE that lack a require matching REQUIRE-RE."
  (seq-remove (lambda (file)
                (string-match-p require-re
                                (test-shipit-magit--file-contents file)))
              (test-shipit-magit--expanders macro-re)))

(ert-deftest test-shipit-magit-requires-declared ()
  "Every file expanding magit-section macros must require magit-section."
  (let ((offenders (mapcar #'file-name-nondirectory
                           (test-shipit-magit--offenders
                            test-shipit-magit--macro-re
                            test-shipit-magit--require-re))))
    (should (equal offenders nil))))

(ert-deftest test-shipit-eieio-requires-declared ()
  "GIVEN the files that expand an EIEIO slot macro
WHEN each is checked for a top-level require defining it
THEN it has one.

`oref' and `oset' are macros, and a file that expands them without
requiring `eieio' compiles by the same accident as the magit macros
above: only when something else in the batch has pulled EIEIO in.
Compiled alone, `(oref section start)' becomes an ordinary call that
evaluates its arguments, and signals `void-variable start' the first
time it runs."
  (let ((offenders (mapcar #'file-name-nondirectory
                           (test-shipit-magit--offenders
                            test-shipit-magit--eieio-macro-re
                            test-shipit-magit--eieio-require-re))))
    (should (equal offenders nil))))

;;; Isolated compilation -- the condition native-comp actually hits

(defconst test-shipit-magit--package-dir
  (expand-file-name "../.packages" test-shipit-magit--lisp-dir)
  "Where the third-party packages the sources build against are installed.")

(defvar test-shipit-magit--isolated-cache (make-hash-table :test 'equal)
  "Isolated-compilation output, keyed by source file.
One subprocess per file, however many checks read its warnings.")

(defun test-shipit-magit--compile-isolated (file)
  "Byte-compile FILE in a subprocess that preloads nothing.
The dependencies go on the load path by hand rather than through
`package-initialize', which loads EIEIO along the way and would hide a
missing `eieio' require.  The subprocess Emacs native-compiles in
loads nothing either.
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
                     `(dolist (dir (directory-files
                                    ,test-shipit-magit--package-dir t "\\`[^.]"))
                        (when (file-directory-p dir)
                          (add-to-list 'load-path dir))))
           "--eval" (prin1-to-string
                     `(byte-compile-file ,file ,out)))
          (buffer-string))
      (ignore-errors (delete-file out)))))

(defun test-shipit-magit--isolated-warnings (file)
  "Return the isolated-compilation output for FILE, compiling it once."
  (or (gethash file test-shipit-magit--isolated-cache)
      (puthash file (test-shipit-magit--compile-isolated file)
               test-shipit-magit--isolated-cache)))

(defun test-shipit-magit--unknown-macro-warning-p (output)
  "Return non-nil when OUTPUT warns that a magit macro is an unknown function."
  (string-match-p "the function .magit-insert-\\(section\\|heading\\)."
                  output))

(defun test-shipit-magit--unknown-eieio-warning-p (output)
  "Return non-nil when OUTPUT warns that an EIEIO slot macro is an unknown function."
  (string-match-p "the function .\\(oref\\|oset\\)\\(-default\\)?."
                  output))

(ert-deftest test-shipit-magit-compiles-in-isolation ()
  "GIVEN a source file that expands a magit-section macro
WHEN it is byte-compiled alone, as native compilation does
THEN the macro expands instead of compiling to a call."
  (dolist (file (test-shipit-magit--expanders test-shipit-magit--macro-re))
    (let ((output (test-shipit-magit--isolated-warnings file)))
      (should (equal
               (list (file-name-nondirectory file)
                     (test-shipit-magit--unknown-macro-warning-p output))
               (list (file-name-nondirectory file) nil))))))

(ert-deftest test-shipit-eieio-compiles-in-isolation ()
  "GIVEN a source file that expands an EIEIO slot macro
WHEN it is byte-compiled alone, as native compilation does
THEN the macro expands instead of compiling to a call."
  (dolist (file (test-shipit-magit--expanders test-shipit-magit--eieio-macro-re))
    (let ((output (test-shipit-magit--isolated-warnings file)))
      (should (equal
               (list (file-name-nondirectory file)
                     (test-shipit-magit--unknown-eieio-warning-p output))
               (list (file-name-nondirectory file) nil))))))

;;; Declarations of functions in libraries we do not own

(defconst test-shipit-magit--declared-re
  (rx "(declare-function" (+ space) (group (+ (not (any " \n"))))
      (+ space) "\"" (group (+ (not (any "\"")))) "\"")
  "Regexp matching a declaration of a function defined elsewhere.")

(defconst test-shipit-magit--checkable-libraries
  '("magit" "magit-section" "magit-diff" "transient")
  "Libraries whose declarations can be checked.
Ours are loaded by the tests anyway; these are the ones the compiler
cannot see and takes our word for.")

(defun test-shipit-magit--declarations ()
  "Return (FUNCTION . LIBRARY) for each declaration we can check."
  (let (found)
    (dolist (file (test-shipit-magit--source-files) (nreverse found))
      (let ((contents (test-shipit-magit--file-contents file))
            (start 0))
        (while (string-match test-shipit-magit--declared-re contents start)
          (let ((function (match-string 1 contents))
                (library (match-string 2 contents)))
            (when (member library test-shipit-magit--checkable-libraries)
              (push (cons function library) found)))
          (setq start (match-end 0)))))))

(ert-deftest test-shipit-declared-functions-exist ()
  "GIVEN the functions we declare from libraries we do not own
WHEN each is looked for, with the library loaded
THEN it is there.

A declaration is a promise to the compiler, which then says nothing
about calls to it.  Promising a function that does not exist buys
silence for a call that fails the moment it runs.  That is how three
calls to a magit section accessor that magit has never had survived:
one declaration covered the file it was in, and the others were never
questioned."
  (dolist (library test-shipit-magit--checkable-libraries)
    (require (intern library) nil t))
  (let ((missing (seq-remove (lambda (declaration)
                               (fboundp (intern (car declaration))))
                             (test-shipit-magit--declarations))))
    (should (equal '() missing))))

(provide 'test-shipit-magit-requires)
;;; test-shipit-magit-requires.el ends here
