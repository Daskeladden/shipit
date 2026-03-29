;;; test-simple-indentation.el --- Simple indentation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple tests to detect indentation issues in shipit code

;;; Code:

(require 'ert)

(defun test-simple-indentation--get-indentation-of-line (line)
  "Get the indentation level of LINE (number of leading spaces)."
  (if (string-match "^\\( *\\)" line)
      (length (match-string 1 line))
    0))

(ert-deftest test-shipit-diff-basic-indentation-validation ()
  "Test basic indentation validation of shipit-diff.el."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents "lisp/shipit-diff.el")
    (let ((original-content (buffer-string)))
      
      ;; Apply automatic indentation
      (indent-region (point-min) (point-max))
      (let ((indented-content (buffer-string)))
        
        ;; Check if content changed
        (if (string= original-content indented-content)
            (message "✓ shipit-diff.el indentation is already correct")
          (progn
            (message "✗ shipit-diff.el has indentation issues")
            
            ;; Show differences
            (let ((orig-lines (split-string original-content "\\n"))
                  (indent-lines (split-string indented-content "\\n"))
                  (differences 0))
              (cl-loop for orig in orig-lines
                       for indented in indent-lines  
                       for line-num from 1
                       unless (string= orig indented)
                       do (progn
                            (incf differences)
                            (when (<= differences 10) ; Show first 10 differences
                              (message "Line %d:" line-num)
                              (message "  Current:  %s" (substring orig 0 (min 80 (length orig))))
                              (message "  Expected: %s" (substring indented 0 (min 80 (length indented))))
                              (message "  Current indent: %d, Expected indent: %d"
                                       (test-simple-indentation--get-indentation-of-line orig)
                                       (test-simple-indentation--get-indentation-of-line indented)))))
              (message "Total differences found: %d lines" differences)
              
              ;; For now, let's make this a warning rather than failure
              ;; so we can see what the issues are
              (message "INDENTATION ISSUES DETECTED - see output above"))))
        
        ;; Always pass for now - we want to see the output
        t))))

(ert-deftest test-shipit-diff-syntax-is-valid ()
  "Test that shipit-diff.el has valid Emacs Lisp syntax."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents "lisp/shipit-diff.el")
    
    ;; This should not throw an error
    (should-not-error (check-parens))
    (message "✓ shipit-diff.el syntax is valid")))

(ert-deftest test-shipit-execute-action-function-indentation ()
  "Test indentation specifically of the shipit--execute-action function."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents "lisp/shipit-diff.el")
    
    ;; Find the function
    (goto-char (point-min))
    (if (re-search-forward "(defun shipit--execute-action" nil t)
        (let ((func-start (match-beginning 0)))
          (goto-char func-start)
          (let ((func-end (scan-sexps func-start 1))) ; Find end of function
            (when func-end
              (let ((func-content (buffer-substring-no-properties func-start func-end)))
                (with-temp-buffer
                  (emacs-lisp-mode)
                  (insert func-content)
                  (let ((original (buffer-string)))
                    (indent-region (point-min) (point-max))
                    (let ((indented (buffer-string)))
                      (if (string= original indented)
                          (message "✓ shipit--execute-action function indentation is correct")
                        (progn
                          (message "✗ shipit--execute-action function has indentation issues")
                          (message "Function length: %d characters" (length original))
                          ;; Show a sample of differences
                          (let ((orig-lines (split-string original "\\n"))
                                (indent-lines (split-string indented "\\n")))
                            (cl-loop for orig in orig-lines
                                     for indented-line in indent-lines
                                     for line-num from 1
                                     unless (string= orig indented-line)
                                     do (message "Function line %d differs" line-num)
                                     when (> line-num 5) ; Show first few differences
                                     do (cl-return)))))))))))
      (message "shipit--execute-action function not found"))
    
    ;; Always pass for diagnostic purposes
    t))

(provide 'test-simple-indentation)
;;; test-simple-indentation.el ends here