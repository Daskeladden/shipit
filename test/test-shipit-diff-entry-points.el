;;; test-shipit-diff-entry-points.el --- What the diff commands do at point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Difftastic is a separate binary.  Its package loads without it,
;; leaving `difftastic-executable' nil, and the failure then surfaces
;; far away as "Wrong type argument: stringp, nil" out of
;; `process-lines'.  The command behind `d t' says so itself instead.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-diff)

(defvar difftastic-executable nil
  "Declared special so the tests can bind it.
Difftastic itself is not loaded here, and a plain `let' on an unbound
symbol would only bind it lexically, which the code under test cannot
see.")

(defmacro test-sdep--with-difftastic (executable &rest body)
  "Run BODY with difftastic loaded and its binary set to EXECUTABLE.
Errors signalled in BODY are available as `raised'."
  (declare (indent 1))
  `(let ((difftastic-executable ,executable)
         (raised nil))
     (ignore raised)
     (cl-letf (((symbol-function 'difftastic-files) (lambda (&rest _) nil)))
       (condition-case err (progn ,@body)
         (error (setq raised (error-message-string err))))
       ,@nil)
     raised))

(defmacro test-sdep--in-file-line (properties &rest body)
  "Run BODY with point on a line carrying PROPERTIES."
  (declare (indent 1))
  `(with-temp-buffer
     (insert (apply #'propertize "     +a line\n" ,properties))
     (goto-char (point-min))
     (forward-char 5)
     ,@body))

(ert-deftest test-sdep-difftastic-without-a-binary-says-which-setting ()
  "GIVEN difftastic loaded but `difftastic-executable' unset
WHEN the difftastic diff runs on a file
THEN it names the setting rather than failing inside `process-lines'."
  (let ((raised (test-sdep--with-difftastic nil
                  (test-sdep--in-file-line '(shipit-file-path "a.c")
                    (shipit--show-difftastic-diff-at-point)))))
    (should (string-match-p "difftastic-executable" raised))))

(ert-deftest test-sdep-difftastic-with-a-missing-binary-says-so ()
  "GIVEN `difftastic-executable' naming a binary that is not installed
WHEN the difftastic diff runs on a file
THEN it says the binary was not found."
  (let ((raised (test-sdep--with-difftastic "difft-that-is-not-installed"
                  (test-sdep--in-file-line '(shipit-file-path "a.c")
                    (shipit--show-difftastic-diff-at-point)))))
    (should (string-match-p "not found" raised))))

(provide 'test-shipit-diff-entry-points)
;;; test-shipit-diff-entry-points.el ends here
