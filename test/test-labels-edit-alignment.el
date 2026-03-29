;;; test-labels-edit-alignment.el --- Test labels alignment after edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that specifically catch labels misalignment after edit operations
;; These tests simulate the actual user workflow of editing labels

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun test-labels--create-mock-magit-buffer ()
  "Create a mock magit status buffer with proper shipit sections."
  ;; Create a realistic magit status buffer structure
  (insert "  📋 PR #123: Add new feature\n")
  (insert "    Status: Open\n")
  (insert "    Author: alice\n")
  (insert "\n")
  (insert "  🏷 Labels (2)\n")
  (insert "    enhancement\n") 
  (insert "    bug\n")
  (insert "\n")
  (insert "  👥 Assignees (1)\n")
  (insert "    alice\n")
  (insert "\n"))

(defun test-labels--check-section-alignment (buffer-content section-name expected-indent)
  "Check that SECTION-NAME in BUFFER-CONTENT has correct indentation.
Returns list of misaligned lines."
  (let ((lines (split-string buffer-content "\n"))
        (in-section nil)
        (misaligned-lines '()))
    
    (cl-loop for line in lines
             for line-num from 1
             do (cond
                 ;; Found our target section
                 ((string-match (format "🏷 %s" section-name) line)
                  (setq in-section t)
                  ;; Check section header alignment
                  (unless (string-match "^  " line)
                    (push (format "Line %d: Section header misaligned: '%s'" line-num line) 
                          misaligned-lines)))
                 
                 ;; Found another section, exit our target section
                 ((and in-section (string-match "^  [📋🏷👥👤💬💭🔧]" line))
                  (setq in-section nil))
                 
                 ;; We're in our target section, check content alignment
                 ((and in-section 
                       (not (string-match "^$" line)) ; Skip empty lines
                       (string-match "[a-zA-Z]" line)) ; Has actual content
                  (unless (string-match (format "^%s" (make-string expected-indent ? )) line)
                    (push (format "Line %d: Content misaligned in %s: '%s' (expected %d spaces)" 
                                  line-num section-name line expected-indent)
                          misaligned-lines)))))
    
    (reverse misaligned-lines)))

(ert-deftest test-labels-alignment-after-fallback-refresh ()
  "Test that fallback refresh maintains proper labels alignment."
  (with-temp-buffer
    (let ((shipit-github-token "test-token")
          (inhibit-read-only t))
      
      ;; Create initial buffer
      (test-labels--create-mock-magit-buffer)
      (let ((original-content (buffer-string)))
        
        ;; Check initial alignment
        (let ((initial-issues (test-labels--check-section-alignment original-content "Labels" 4)))
          (should (null initial-issues)))
        
        ;; Mock functions for the fallback refresh
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
                   (lambda (sym) (eq sym 'magit-section-update-highlight)))
                  ((symbol-function 'shipit--format-label)
                   (lambda (label)
                     (if (listp label)
                         (cdr (assq 'name label))
                       (format "%s" label)))))
          
          ;; Create test data with different labels (simulating an edit)
          (let ((updated-labels-data `((number . 123)
                                      (labels . (((name . "enhancement") (color . "a2eeef"))
                                                ((name . "bug") (color . "d73a4a"))
                                                ((name . "feature") (color . "0052cc"))
                                                ((name . "documentation") (color . "0075ca")))))))
            
            ;; Simulate the fallback refresh operation
            (message "Testing fallback refresh...")
            (shipit--fallback-labels-refresh updated-labels-data)
            
            ;; Check alignment after the operation
            (let* ((refreshed-content (buffer-string))
                   (alignment-issues (test-labels--check-section-alignment refreshed-content "Labels" 4)))
              
              (message "Content after refresh:")
              (message "%s" refreshed-content)
              
              (when alignment-issues
                (message "Alignment issues found:")
                (dolist (issue alignment-issues)
                  (message "  %s" issue)))
              
              ;; The test should pass - no alignment issues
              (should (null alignment-issues))
              
              ;; Also verify the labels were actually updated
              (should (string-match "feature" refreshed-content))
              (should (string-match "documentation" refreshed-content))
              (should (string-match "🏷 Labels (4)" refreshed-content))))))))

(ert-deftest test-labels-alignment-after-targeted-refresh ()
  "Test that targeted refresh maintains proper labels alignment."
  (with-temp-buffer 
    (let ((shipit-github-token "test-token")
          (inhibit-read-only t))
      
      ;; Create initial buffer
      (test-labels--create-mock-magit-buffer)
      
      ;; Mock the magit section system
      (cl-letf (((symbol-function 'shipit--get-labels-only-data)
                 (lambda (pr-number repo)
                   `((number . ,pr-number)
                     (labels . (((name . "enhancement") (color . "a2eeef"))
                               ((name . "bug") (color . "d73a4a"))
                               ((name . "urgent") (color . "ff0000")))))))
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "owner/repo"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "test-branch"))
                ((symbol-function 'magit-current-section)
                 (lambda () 
                   ;; Return a mock section that will trigger fallback
                   nil)) ; This will cause the targeted refresh to use fallback
                ((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args)
                   (message "DEBUG: %s" (apply 'format format-string args))))
                ((symbol-function 'shipit--format-label)
                 (lambda (label)
                   (if (listp label)
                       (cdr (assq 'name label))
                     (format "%s" label)))))
        
        (let ((original-content (buffer-string)))
          ;; Verify initial state is aligned
          (let ((initial-issues (test-labels--check-section-alignment original-content "Labels" 4)))
            (should (null initial-issues)))
          
          ;; Simulate targeted refresh (which should fall back to fallback refresh)
          (message "Testing targeted refresh with fallback...")
          (shipit--refresh-labels-section-with-pr 123 "owner/repo")
          
          ;; Check final alignment
          (let* ((final-content (buffer-string))
                 (final-issues (test-labels--check-section-alignment final-content "Labels" 4)))
            
            (message "Final content:")
            (message "%s" final-content)
            
            (when final-issues
              (message "Final alignment issues:")
              (dolist (issue final-issues)
                (message "  %s" issue)))
            
            ;; Should have no alignment issues
            (should (null final-issues))
            
            ;; Should have updated content
            (should (string-match "urgent" final-content))
            (should (string-match "🏷 Labels (3)" final-content)))))))

(ert-deftest test-labels-alignment-with-pre-existing-misalignment ()
  "Test that refresh can handle and fix pre-existing misalignment."
  (with-temp-buffer
    (let ((shipit-github-token "test-token")
          (inhibit-read-only t))
      
      ;; Create a buffer with intentional misalignment
      (insert "  📋 PR #123: Add new feature\n")
      (insert "    Status: Open\n")
      (insert "\n")
      (insert "  🏷 Labels (2)\n")
      (insert "  enhancement\n")     ; Intentionally misaligned (2 spaces instead of 4)
      (insert "      bug\n")         ; Intentionally over-indented (6 spaces)
      (insert "\n")
      (insert "   👥 Assignees (1)\n") ; Intentionally misaligned section header
      (insert "    alice\n")
      
      (let ((misaligned-content (buffer-string)))
        ;; Verify we start with misalignment
        (let ((initial-issues (test-labels--check-section-alignment misaligned-content "Labels" 4)))
          (should (not (null initial-issues))) ; Should have issues initially
          (message "Initial misalignment detected: %d issues" (length initial-issues)))
        
        ;; Mock functions
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
                   (lambda (sym) (eq sym 'magit-section-update-highlight)))
                  ((symbol-function 'shipit--format-label)
                   (lambda (label)
                     (if (listp label)
                         (cdr (assq 'name label))
                       (format "%s" label)))))
          
          ;; Apply fallback refresh with the more robust boundary detection
          (let ((corrected-labels-data `((number . 123)
                                        (labels . (((name . "enhancement") (color . "a2eeef"))
                                                  ((name . "bug") (color . "d73a4a")))))))
            
            (message "Applying fallback refresh to fix misalignment...")
            (shipit--fallback-labels-refresh corrected-labels-data)
            
            ;; Check if the misalignment was fixed
            (let* ((corrected-content (buffer-string))
                   (final-issues (test-labels--check-section-alignment corrected-content "Labels" 4)))
              
              (message "Content after correction:")
              (message "%s" corrected-content)
              
              (when final-issues
                (message "Remaining issues after correction:")
                (dolist (issue final-issues)
                  (message "  %s" issue)))
              
              ;; The refresh should fix the alignment
              (should (null final-issues)))))))))

(provide 'test-labels-edit-alignment)
;;; test-labels-edit-alignment.el ends here