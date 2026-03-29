#!/usr/bin/env emacs --script
;;; validate-syntax.el --- Reliable syntax validation for Emacs Lisp files

;;; Commentary:
;; This script provides reliable syntax validation that correctly handles
;; comments and other Emacs Lisp syntax edge cases.

;;; Code:

(defun validate-syntax--check-file (file)
  "Reliably validate FILE syntax. Returns t if valid, error message if not."
  (with-temp-buffer
    (condition-case err
        (progn
          (insert-file-contents file)
          (emacs-lisp-mode)
          
          ;; Method 1: check-parens with proper comment handling
          (let ((parse-sexp-ignore-comments t))
            (check-parens))
          
          ;; Method 2: forward-sexp parsing with proper comment handling  
          (goto-char (point-min))
          (let ((parse-sexp-ignore-comments t))
            (while (< (point) (point-max))
              (forward-sexp 1)))
          
          ;; Method 3: byte-compilation syntax check
          (byte-compile-from-buffer (current-buffer) file)
          
          t) ; All checks passed
      (error (format "%s" err)))))

(defun validate-syntax--main ()
  "Validate all .el files in the lisp directory."
  (let* ((lisp-dir (expand-file-name "lisp"))
         (el-files (directory-files lisp-dir t "\\.el$"))
         (errors 0))
    
    (message "🔍 Validating syntax for %d Emacs Lisp files..." (length el-files))
    
    (dolist (file el-files)
      (let ((result (validate-syntax--check-file file)))
        (if (eq result t)
            (message "✅ %s" (file-name-nondirectory file))
          (message "❌ %s: %s" (file-name-nondirectory file) result)
          (setq errors (1+ errors)))))
    
    (message "\n📊 SUMMARY: %d errors found" errors)
    (if (> errors 0)
        (progn
          (message "❌ SYNTAX VALIDATION FAILED")
          (kill-emacs 1))
      (progn
        (message "✅ ALL FILES PASSED SYNTAX VALIDATION")  
        (kill-emacs 0)))))

;; Test the validation function first
(message "🧪 Testing syntax validation with known cases...")

;; Test 1: Valid syntax
(with-temp-buffer
  (insert "(defun test () 'valid)")
  (emacs-lisp-mode)
  (let ((result (validate-syntax--check-file (buffer-name))))
    (message "Test 1 - Valid syntax: %s" (if (eq result t) "✅ PASS" "❌ FAIL"))))

;; Test 2: Comment with unbalanced parens (should pass)
(with-temp-buffer
  (insert ";; Comment with ( ( ( unbalanced\n(defun test () 'valid)")
  (emacs-lisp-mode)  
  (let ((result (validate-syntax--check-file (buffer-name))))
    (message "Test 2 - Comment parens: %s" (if (eq result t) "✅ PASS" "❌ FAIL"))))

;; Test 3: Real syntax error (should fail)
(with-temp-buffer
  (insert "(defun test (\n  (let ((x 1)")  ; Missing closing parens
  (emacs-lisp-mode)
  (let ((result (validate-syntax--check-file (buffer-name))))
    (message "Test 3 - Real error: %s" (if (eq result t) "❌ FAIL" "✅ PASS"))))

(message "\n🚀 Running validation on actual files...")

;; Run the main validation
(validate-syntax--main)

;;; validate-syntax.el ends here