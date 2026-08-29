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

;;;; Handler-less condition-case

(defun test-shipit-dup--forms (file)
  "Return every top-level form read from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (error nil))
      (nreverse forms))))

(defun test-shipit-dup--handlerless (forms)
  "Return the `condition-case' variables in FORMS that have no handler."
  (let (found)
    ;; Walk car and cdr rather than mapping over the form: source is
    ;; full of dotted pairs, such as header alists, and `mapc' stops on
    ;; the first one.
    (letrec ((walk (lambda (form)
                     (when (consp form)
                       (when (and (eq (car form) 'condition-case)
                                  (proper-list-p form)
                                  (null (nthcdr 3 form)))
                         (push (nth 1 form) found))
                       (funcall walk (car form))
                       (funcall walk (cdr form))))))
      (mapc (lambda (form) (funcall walk form)) forms))
    (nreverse found)))

(ert-deftest test-shipit-no-handlerless-condition-case ()
  "GIVEN every shipit source file
WHEN its `condition-case' forms are read
THEN each has at least one handler.

A `condition-case' with none catches nothing, which is worth knowing
on its own.  Worse, it is what a misplaced closing paren looks like:
the handler that was meant to follow ends up inside the body instead,
where the error variable is not bound, so the form runs on every call
and fails with `void-variable'.  That is how the comment tree in the
diff view came to raise on every call, which nothing noticed because
the only test covering it was one nobody ran."
  (dolist (file (test-shipit-dup--source-files))
    (should (equal (list (file-name-nondirectory file) '())
                   (list (file-name-nondirectory file)
                         (test-shipit-dup--handlerless
                          (test-shipit-dup--forms file)))))))

(provide 'test-shipit-duplicate-defuns)
;;; test-shipit-duplicate-defuns.el ends here
