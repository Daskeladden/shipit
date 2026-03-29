#!/usr/bin/env emacs --script
;;; check-syntax.el --- Comprehensive syntax validation for shipit

;;; Commentary:
;; This script validates all Emacs Lisp files for syntax errors
;; including parentheses balance, quotes, and other parsing issues.

;;; Code:

(defvar check-syntax-errors 0 "Count of syntax errors found.")

(defun check-syntax--validate-file (file)
  "Validate FILE for syntax errors. Returns t if valid, nil if errors found."
  (let ((errors '()))
    (with-temp-buffer
      (condition-case err
          (progn
            (insert-file-contents file)
            ;; Test parentheses balance
            (goto-char (point-min))
            (condition-case paren-err
                (check-parens)
              (error 
               (push (format "Parentheses error: %s" paren-err) errors)))
            
            ;; Test sexp parsing
            (goto-char (point-min))
            (condition-case sexp-err
                (while (< (point) (point-max))
                  (forward-sexp 1))
              (error 
               (push (format "Sexp parsing error at position %d: %s" (point) sexp-err) errors)))
            
            ;; Test byte compilation syntax
            (goto-char (point-min))
            (condition-case byte-err
                (let ((byte-compile-error-on-warn nil))
                  (byte-compile-from-buffer (current-buffer) file))
              (error 
               (push (format "Byte compilation error: %s" byte-err) errors))))
        (error 
         (push (format "File reading error: %s" err) errors))))
    
    (when errors
      (message "❌ %s:" (file-name-nondirectory file))
      (dolist (error errors)
        (message "  • %s" error))
      (setq check-syntax-errors (+ check-syntax-errors (length errors)))
      nil)))

(defun check-syntax--main ()
  "Main function to check all .el files."
  (message "🔍 Checking syntax for all Emacs Lisp files...")
  (let* ((lisp-dir (expand-file-name "lisp" (or (getenv "PWD") default-directory)))
         (el-files (directory-files lisp-dir t "\\.el$"))
         (valid-files 0))
    
    (message "Found %d .el files in %s" (length el-files) lisp-dir)
    
    (dolist (file el-files)
      (when (check-syntax--validate-file file)
        (setq valid-files (1+ valid-files))
        (message "✅ %s" (file-name-nondirectory file))))
    
    (message "\n📊 SUMMARY:")
    (message "• Valid files: %d/%d" valid-files (length el-files))
    (message "• Syntax errors: %d" check-syntax-errors)
    
    (if (> check-syntax-errors 0)
        (progn
          (message "\n❌ SYNTAX VALIDATION FAILED")
          (kill-emacs 1))
      (progn
        (message "\n✅ ALL FILES PASSED SYNTAX VALIDATION")
        (kill-emacs 0)))))

;; Run the checker
(check-syntax--main)

;;; check-syntax.el ends here