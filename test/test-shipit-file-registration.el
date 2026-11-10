;;; test-shipit-file-registration.el --- New files must be registered -*- lexical-binding: t; -*-

;;; Commentary:
;; A new file is not picked up by being required.  It has to be
;; registered in two places, and forgetting either fails in a way that
;; looks like something else:
;;
;;   * `shipit-dev-utils.el' reloads only the files its list names.  A
;;     file that is merely required by another loads once and never
;;     reloads from source, so edits to it silently do nothing, and a
;;     genuinely new file raises `void-function' after a reload that
;;     reported success.
;;
;;   * The Makefile runs only the test files its target names, so an
;;     unregistered test file passes by never running.

;;; Code:

(require 'ert)
(require 'seq)

(defconst test-registration--root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(defconst test-registration--test-exceptions '("test-stubs.el")
  "Test files that hold no tests and so are never run on their own.
`test-stubs.el' mocks `request' and magit for the files that load it.")

(defconst test-registration--reload-exceptions '()
  "Files deliberately absent from the reload list.
Empty: `shipit-init.el' held the only exception and is gone.")

(defun test-registration--file-text (relative)
  "Return the contents of RELATIVE to the repository root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative test-registration--root))
    (buffer-string)))

(defun test-registration--names-in (text)
  "Return every quoted shipit source file name mentioned in TEXT."
  (let (names (start 0))
    (while (string-match "\"\\(shipit[a-z0-9-]*\\.el\\)\"" text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    (delete-dups names)))

(defun test-registration--sources ()
  "Return the shipit source file names that must be reloadable."
  (seq-remove
   (lambda (name)
     (or (string-match-p "autoloads" name)
         (member name test-registration--reload-exceptions)))
   (mapcar #'file-name-nondirectory
           (directory-files
            (expand-file-name "lisp" test-registration--root) t "\\.el\\'"))))

(ert-deftest test-shipit-every-source-file-is-reloadable ()
  "GIVEN the sources under `lisp/'
WHEN the reload list in `shipit-dev-utils.el' is read
THEN every one of them is named there, so `shipit-reload' picks up
edits to any file rather than leaving some at the version that
happened to load first."
  (let* ((listed (test-registration--names-in
                  (test-registration--file-text "shipit-dev-utils.el")))
         (missing (seq-remove (lambda (name) (member name listed))
                              (test-registration--sources))))
    (should (equal '() (sort missing #'string<)))))

(defun test-registration--indirectly-loaded ()
  "Return test files that another test file loads itself.
Several are pulled in with `load-file\=' rather than named in the
Makefile.  They run, so the Makefile not naming them is not a fault."
  (let (names)
    (dolist (file (directory-files
                   (expand-file-name "test" test-registration--root)
                   t "\\`test-.*\\.el\\'")
                  (delete-dups names))
      (let ((text (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string)))
            (self (file-name-nondirectory file))
            (start 0))
        (while (string-match "\\(test-[a-z0-9-]+\\.el\\)" text start)
          (let ((found (match-string 1 text)))
            (unless (equal found self) (push found names)))
          (setq start (match-end 0)))))))

(defun test-registration--baseline (name)
  "Return the lines of the baseline file NAME under `test/fixtures'."
  (let ((path (expand-file-name (concat "test/fixtures/" name)
                                test-registration--root)))
    (when (file-readable-p path)
      (seq-remove #'string-empty-p
                  (split-string (test-registration--file-text
                                 (concat "test/fixtures/" name))
                                "\n" t)))))

(ert-deftest test-shipit-every-test-file-is-run ()
  "GIVEN the test files under `test/'
WHEN the Makefile's test target is read
THEN every one added since the baseline is loaded there.  A test file
nobody runs reports nothing and is indistinguishable from a passing
one.

Forty-one files predate this check and are listed in
`test/fixtures/unrun-tests-baseline.txt'.  They are not registered
because nobody knows yet whether they still pass; the list is meant
to shrink.  Anything not on it must be run."
  (let* ((makefile (test-registration--file-text "Makefile"))
         (indirect (test-registration--indirectly-loaded))
         (known (test-registration--baseline "unrun-tests-baseline.txt"))
         (on-disk (mapcar #'file-name-nondirectory
                          (directory-files
                           (expand-file-name "test" test-registration--root)
                           t "\\`test-.*\\.el\\'")))
         (missing (seq-remove
                   (lambda (name)
                     (or (string-match-p (regexp-quote name) makefile)
                         (member name indirect)
                         (member name known)
                         (member name test-registration--test-exceptions)))
                   on-disk)))
    (should (equal '() (sort missing #'string<)))))

(ert-deftest test-shipit-unrun-test-baseline-is-not-stale ()
  "GIVEN the baseline of test files nobody runs
WHEN the Makefile is read
THEN no file on it is registered after all, so the list shrinks as
files are adopted rather than quietly outliving the problem."
  (let* ((makefile (test-registration--file-text "Makefile"))
         (known (test-registration--baseline "unrun-tests-baseline.txt"))
         (indirect (test-registration--indirectly-loaded))
         (adopted (seq-filter
                   (lambda (name)
                     (or (string-match-p (regexp-quote name) makefile)
                         (member name indirect)))
                   known)))
    (should (equal '() (sort adopted #'string<)))))

;;; The README and the package header

(defun test-registration--emacs-version (relative pattern)
  "The Emacs version that PATTERN finds in the file RELATIVE, its group 1.
Finding none is an error."
  (let ((text (test-registration--file-text relative)))
    (unless (string-match pattern text)
      (error "No Emacs version in %s" relative))
    (match-string 1 text)))

(ert-deftest test-shipit-readme-names-the-emacs-of-the-package-header ()
  "GIVEN the Emacs version that the package header requires
WHEN the install section of the README is read
THEN it names the same version."
  (should (equal (test-registration--emacs-version
                  "lisp/shipit.el" "Package-Requires: ((emacs \"\\([0-9.]+\\)\")")
                 (test-registration--emacs-version
                  "README.md" "^Emacs \\([0-9.]+\\)\\+"))))

;;; The hook points of the README

(defun test-registration--readme-section (heading)
  "The text of the section of the README under HEADING, a line as `## Name'.
It ends before the next heading of that level.  Finding none is an error."
  (let* ((text (test-registration--file-text "README.md"))
         (start (or (string-match (concat "^" (regexp-quote heading) "$") text)
                    (error "No section %s in the README" heading))))
    (substring text start (string-match "^## " text (1+ start)))))

(defun test-registration--readme-hook-points ()
  "The hook points that the README names, each as (NAME . FILE).
A hook point is a row of a table in the section Extending shipit.  Its
first cell names it, and its second cell names its file.  Finding none
is an error."
  (let ((text (test-registration--readme-section "## Extending shipit"))
        (start 0)
        (rows '()))
    (while (string-match "^| `\\(shipit-[a-z0-9-]+\\)` | `\\(shipit[a-z0-9-]*\\)` |"
                         text start)
      (push (cons (match-string 1 text) (match-string 2 text)) rows)
      (setq start (match-end 0)))
    (or (nreverse rows)
        (error "No hook point in the README"))))

(defun test-registration--defined-in-p (name file)
  "Non-nil when lisp/FILE.el defines the variable or the function NAME."
  (string-match-p
   (format "^(def\\(?:var\\|var-local\\|custom\\|un\\) %s[ \n]" (regexp-quote name))
   (test-registration--file-text (format "lisp/%s.el" file))))

(ert-deftest test-shipit-readme-hook-points-are-in-the-code ()
  "GIVEN the hook points that the README names for other packages, each
with the file that the README names for it
WHEN the files are read
THEN each file defines its hook point, so the README names nothing
that is gone or that moved to another file."
  (should (equal '()
                 (seq-remove (lambda (row)
                               (test-registration--defined-in-p (car row) (cdr row)))
                             (test-registration--readme-hook-points)))))

(provide 'test-shipit-file-registration)
;;; test-shipit-file-registration.el ends here
