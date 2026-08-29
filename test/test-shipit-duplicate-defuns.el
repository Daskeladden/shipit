;;; test-shipit-duplicate-defuns.el --- Guard against shadowed definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;;; Commentary:

;; A `defun' that appears twice in one file is not an error: at load
;; time the later form silently replaces the earlier one.  Nothing
;; warns, and the dead copy still reads like live code -- so later
;; improvements land in whichever copy the author happened to find and
;; never take effect.
;;
;; That is exactly how `shipit-issue-jira--api-request-async' kept
;; leaking its response buffer on every failed request: the leak had
;; already been fixed, 350 lines above the definition that actually won.
;;
;; Every top-level function name must be defined exactly once per file.

;;; Code:

(require 'ert)
(require 'seq)

(defconst test-shipit-dup--lisp-dir
  (expand-file-name
   "../lisp"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding the shipit sources under test.")

(defun test-shipit-dup--source-files ()
  "Return the shipit source files that must be checked."
  (seq-remove (lambda (f) (string-match-p "autoloads" f))
              (directory-files test-shipit-dup--lisp-dir t "\\.el\\'")))

(defun test-shipit-dup--defun-names (file)
  "Return every top-level function name defined in FILE, in order."
  (let ((names '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^(defun[ \t\n]+\\([^ \t\n()]+\\)" nil t)
        (push (match-string 1) names)))
    (nreverse names)))

(defun test-shipit-dup--shadowed (names)
  "Return the sorted subset of NAMES that appears more than once."
  (let ((counts (make-hash-table :test 'equal))
        (shadowed '()))
    (dolist (name names)
      (puthash name (1+ (gethash name counts 0)) counts))
    (maphash (lambda (name count)
               (when (> count 1) (push name shadowed)))
             counts)
    (sort shadowed #'string<)))

(ert-deftest test-shipit-no-shadowed-defuns ()
  "GIVEN every shipit source file
WHEN its top-level function definitions are collected
THEN no name is defined twice, so no definition can silently shadow
an earlier one at load time."
  (dolist (file (test-shipit-dup--source-files))
    (should (equal (list (file-name-nondirectory file) '())
                   (list (file-name-nondirectory file)
                         (test-shipit-dup--shadowed
                          (test-shipit-dup--defun-names file)))))))

(provide 'test-shipit-duplicate-defuns)
;;; test-shipit-duplicate-defuns.el ends here
