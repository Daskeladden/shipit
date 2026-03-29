;;; test-user-edit-workflow.el --- Test user edit workflow -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest test-user-label-edit-workflow ()
  "Test the exact workflow when user edits labels."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create realistic magit buffer
      (insert "  📋 PR #123: Add feature\n")
      (insert "    Status: Open\n")
      (insert "\n")
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")  
      (insert "\n")
      (insert "  👥 Assignees (1)\n")
      (insert "    alice\n")
      
      ;; Position cursor on labels section as user would
      (goto-char (point-min))
      (re-search-forward "🏷 Labels")
      (forward-line 1)
      (beginning-of-line)
      
      (let ((before-edit (buffer-string)))
        (message "Before edit (cursor at point %d):\n%s" (point) before-edit)
        
        ;; Mock the complete edit workflow functions
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
          
          ;; Simulate fallback refresh as if section object failed
          (let ((updated-pr-data '((number . 123)
                                   (labels . (((name . "enhancement"))
                                             ((name . "bug"))
                                             ((name . "feature")))))))
            (shipit--fallback-labels-refresh updated-pr-data)
            
            (let ((after-edit (buffer-string)))
              (message "After edit:\n%s" after-edit)
              
              ;; Check for proper alignment
              (should (string-match "    enhancement" after-edit))
              (should (string-match "    bug" after-edit))
              (should (string-match "    feature" after-edit))
              (should (string-match "🏷 Labels (3)" after-edit))
              
              ;; Verify no malformed lines
              (should-not (string-match "^  [a-zA-Z]" after-edit)) ; No 2-space labels
              (should-not (string-match "^[a-zA-Z]" after-edit))   ; No 0-space labels
              
              (message "✓ User edit workflow maintains proper alignment"))))))))

(provide 'test-user-edit-workflow)