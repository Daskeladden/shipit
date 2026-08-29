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

(defconst test-registration--reload-exceptions '("shipit-init.el")
  "Files deliberately absent from the reload list.
`shipit-init.el' is not loaded as a file there; it is called as a
function once the rest is loaded.")

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
         (known (test-registration--baseline "unrun-tests-baseline.txt"))
         (on-disk (mapcar #'file-name-nondirectory
                          (directory-files
                           (expand-file-name "test" test-registration--root)
                           t "\\`test-.*\\.el\\'")))
         (missing (seq-remove
                   (lambda (name)
                     (or (string-match-p (regexp-quote name) makefile)
                         (member name known)))
                   on-disk)))
    (should (equal '() (sort missing #'string<)))))

(ert-deftest test-shipit-unrun-test-baseline-is-not-stale ()
  "GIVEN the baseline of test files nobody runs
WHEN the Makefile is read
THEN no file on it is registered after all, so the list shrinks as
files are adopted rather than quietly outliving the problem."
  (let* ((makefile (test-registration--file-text "Makefile"))
         (known (test-registration--baseline "unrun-tests-baseline.txt"))
         (adopted (seq-filter
                   (lambda (name) (string-match-p (regexp-quote name) makefile))
                   known)))
    (should (equal '() (sort adopted #'string<)))))

(provide 'test-shipit-file-registration)
;;; test-shipit-file-registration.el ends here
