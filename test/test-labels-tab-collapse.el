;;; test-labels-tab-collapse.el --- Test labels TAB collapse behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'shipit-diff)

(ert-deftest test-labels-section-tab-collapse-boundaries ()
  "Test that TAB collapse on labels section has correct boundaries."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create realistic magit buffer with multiple sections
      (insert "  📋 PR #123: Add feature\n")
      (insert "    Status: Open\n")
      (insert "\n")
      (insert "  🏷 Labels (3)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")  
      (insert "    feature\n")
      (insert "\n")
      (insert "  👥 Assignees (1)\n")
      (insert "    alice\n")
      (insert "\n")
      (insert "  💬 Comments (2)\n")
      (insert "    Comment 1\n")
      (insert "    Comment 2\n")
      
      ;; Mock magit section functions
      (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "owner/repo"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "test-branch"))
                ((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args)
                   (message "DEBUG: %s" (apply 'format format-string args))))
                ((symbol-function 'shipit--format-label)
                 (lambda (label)
                   (if (listp label)
                       (cdr (assq 'name label))
                     (format "%s" label)))))
        
        ;; Test fallback refresh and check boundaries
        (let ((test-data '((number . 123)
                          (labels . (((name . "enhancement"))
                                    ((name . "bug"))
                                    ((name . "feature"))
                                    ((name . "documentation")))))))
          
          (message "Testing fallback refresh boundary detection...")
          (shipit--fallback-labels-refresh test-data)
          
          (let ((final-content (buffer-string)))
            (message "Content after refresh:\n%s" final-content)
            
            ;; Find the labels section
            (goto-char (point-min))
            (when (re-search-forward "🏷 Labels" nil t)
              (let ((labels-start (line-beginning-position)))
                (forward-line 1) ; Move to first label
                
                ;; Find where labels section should end (before next section)
                (let ((labels-content-start (point))
                      (expected-labels-end (save-excursion
                                           (when (re-search-forward "^  [📋👥💬]" nil t)
                                             (match-beginning 0)))))
                  
                  (when expected-labels-end
                    ;; Check that labels content is properly bounded
                    (let ((labels-region (buffer-substring labels-content-start expected-labels-end)))
                      (message "Labels content region:\n%s" labels-region)
                      
                      ;; Should contain only labels, not assignees or comments
                      (should (string-match "enhancement" labels-region))
                      (should (string-match "bug" labels-region))  
                      (should (string-match "feature" labels-region))
                      (should (string-match "documentation" labels-region))
                      
                      ;; Should NOT contain content from next sections
                      (should-not (string-match "alice" labels-region))
                      (should-not (string-match "Comment" labels-region))
                      (should-not (string-match "Assignees" labels-region))
                      
                      (message "✓ Labels section boundaries are correct"))))))))))

(ert-deftest test-section-boundary-after-targeted-refresh ()
  "Test section boundaries after targeted refresh operations."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create buffer
      (insert "  📋 PR #123: Add feature\n")
      (insert "    Status: Open\n")
      (insert "\n")
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")
      (insert "\n")
      (insert "  👥 Assignees (1)\n")
      (insert "    alice\n")
      
      ;; Position cursor in labels section for targeted refresh
      (goto-char (point-min))
      (re-search-forward "🏷 Labels")
      
      (cl-letf (((symbol-function 'shipit--get-labels-only-data)
                 (lambda (pr-number repo)
                   `((number . ,pr-number)
                     (labels . (((name . "enhancement"))
                               ((name . "bug"))
                               ((name . "urgent")))))))
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "owner/repo"))
                ((symbol-function 'magit-get-current-branch)
                 (lambda () "test-branch"))
                ((symbol-function 'magit-current-section)
                 (lambda () nil)) ; Force fallback
                ((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args)
                   (message "DEBUG: %s" (apply 'format format-string args))))
                ((symbol-function 'shipit--format-label)
                 (lambda (label)
                   (if (listp label)
                       (cdr (assq 'name label))
                     (format "%s" label)))))
        
        ;; Trigger targeted refresh (should use fallback)
        (shipit--refresh-labels-section-with-pr 123 "owner/repo")
        
        (let ((refreshed-content (buffer-string)))
          (message "After targeted refresh:\n%s" refreshed-content)
          
          ;; Test boundary detection
          (goto-char (point-min))
          (when (re-search-forward "🏷 Labels (3)" nil t)
            (let ((labels-start (line-beginning-position)))
              (forward-line 1)
              (let ((labels-content-start (point))
                    (next-section-start (save-excursion
                                        (when (re-search-forward "^  👥" nil t)
                                          (match-beginning 0)))))
                
                (when next-section-start
                  (let ((labels-only-content (buffer-substring labels-content-start next-section-start)))
                    (message "Labels-only content:\n%s" labels-only-content)
                    
                    ;; Verify clean boundaries
                    (should (string-match "enhancement" labels-only-content))
                    (should (string-match "urgent" labels-only-content))
                    (should-not (string-match "alice" labels-only-content))
                    (should-not (string-match "Assignees" labels-only-content))
                    
                    (message "✓ Targeted refresh maintains clean section boundaries"))))))))))

(provide 'test-labels-tab-collapse)