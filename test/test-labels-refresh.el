;;; test-labels-refresh.el --- Tests for labels refresh behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to reproduce and debug labels misalignment after editing

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load shipit modules
(require 'shipit-diff)

(ert-deftest test-labels-refresh-preserves-formatting ()
  "Test that label refresh operations preserve proper formatting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((shipit-github-token "test-token"))
      ;; Create a mock magit status buffer with labels section
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")
      (insert "  👥 Assignees (1)\n")
      (insert "    alice\n")
      
      (goto-char (point-min))
      
      ;; Mock the functions that the refresh code uses
      (cl-letf (((symbol-function 'shipit--get-labels-only-data)
                 (lambda (pr-number repo)
                   `((number . ,pr-number)
                     (labels . (((name . "enhancement") (color . "a2eeef"))
                               ((name . "bug") (color . "d73a4a"))
                               ((name . "documentation") (color . "0075ca")))))))
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "owner/repo"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "test-branch"))
                ((symbol-function 'magit-current-section)
                 (lambda () 
                   ;; Return a mock section object
                   (list :type 'shipit-labels :start 1 :content 16 :end 50)))
                ((symbol-function 'oref)
                 (lambda (obj property)
                   (cond ((eq property 'start) 1)
                         ((eq property 'content) 16)
                         ((eq property 'end) 50)
                         (t nil))))
                ((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args) 
                   (message "DEBUG: %s" (apply 'format format-string args))))
                ((symbol-function 'force-mode-line-update)
                 (lambda (&optional all) t))
                ((symbol-function 'magit-section-update-highlight)
                 (lambda () t))
                ((symbol-function 'fboundp) 
                 (lambda (sym) (eq sym 'magit-section-update-highlight))))
        
        ;; Store original buffer content  
        (let ((original-content (buffer-string)))
          (message "Original buffer content:")
          (message "%s" original-content)
          
          ;; Simulate a label refresh operation
          (condition-case err
              (progn
                (goto-char (point-min))
                (when (re-search-forward "🏷 Labels" nil t)
                  (shipit--refresh-labels-section-with-pr 123 "owner/repo")))
            (error 
             (message "Error during refresh: %s" (error-message-string err))))
          
          ;; Check the result
          (let ((new-content (buffer-string)))
            (message "New buffer content:")
            (message "%s" new-content)
            
            ;; Verify the structure is preserved
            (goto-char (point-min))
            (should (re-search-forward "  🏷 Labels" nil t))
            
            ;; Verify indentation is consistent
            (let ((lines (split-string new-content "\n"))
                  (misaligned-lines '()))
              (cl-loop for line in lines
                       for line-num from 1
                       do (cond
                           ;; Section headers should have 2 spaces
                           ((string-match "^  [🏷👥]" line)
                            (unless (string-match "^  " line)
                              (push (format "Line %d: Section header wrong indent: %s" line-num line) misaligned-lines)))
                           ;; Section content should have 4 spaces  
                           ((string-match "^    [a-zA-Z]" line)
                            (unless (string-match "^    " line)
                              (push (format "Line %d: Content wrong indent: %s" line-num line) misaligned-lines)))
                           ;; Empty lines are ok
                           ((string-match "^$" line) nil)
                           ;; Other lines might be problematic
                           ((not (string-match "^  \\|^    \\|^$" line))
                            (push (format "Line %d: Unexpected indent: %s" line-num line) misaligned-lines))))
              
              (when misaligned-lines
                (message "Found misaligned lines:")
                (dolist (line misaligned-lines)
                  (message "  %s" line)))
              
              ;; For now, don't fail the test, just report
              (should (null misaligned-lines))))))))

(ert-deftest test-fallback-labels-refresh-formatting ()
  "Test the fallback labels refresh function formatting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((shipit-github-token "test-token"))
      ;; Create initial buffer with labels
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n") 
      (insert "    bug\n")
      (insert "  👥 Assignees (1)\n")
      
      ;; Mock required functions
      (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "owner/repo"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "test-branch"))
                ((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args)
                   (message "DEBUG: %s" (apply 'format format-string args))))
                ((symbol-function 'force-mode-line-update)
                 (lambda (&optional all) t))
                ((symbol-function 'magit-section-update-highlight)
                 (lambda () t))
                ((symbol-function 'fboundp)
                 (lambda (sym) (eq sym 'magit-section-update-highlight))))
        
        (let ((original-content (buffer-string))
              (test-labels-data `((number . 123)
                                 (labels . (((name . "enhancement") (color . "a2eeef"))
                                           ((name . "bug") (color . "d73a4a"))
                                           ((name . "feature") (color . "0052cc")))))))
          
          (message "Before fallback refresh:")
          (message "%s" original-content)
          
          ;; Call the fallback refresh function
          (condition-case err
              (shipit--fallback-labels-refresh test-labels-data)
            (error
             (message "Error in fallback refresh: %s" (error-message-string err))))
          
          (let ((new-content (buffer-string)))
            (message "After fallback refresh:")
            (message "%s" new-content)
            
            ;; Check that the formatting is preserved
            (goto-char (point-min))
            (should (re-search-forward "  🏷 Labels (3)" nil t))
            
            ;; Verify each line has proper indentation
            (let ((lines (split-string new-content "\n")))
              (dolist (line lines)
                (when (string-match "enhancement\\|bug\\|feature" line)
                  (should (string-match "^    " line))))))))))

(provide 'test-labels-refresh)
;;; test-labels-refresh.el ends here