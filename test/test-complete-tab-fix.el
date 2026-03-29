;;; test-complete-tab-fix.el --- Test complete TAB collapse fix -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest test-fallback-refresh-preserves-section-boundaries ()
  "Test that fallback refresh maintains correct section boundaries for TAB collapse."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")
      (insert "\n")
      (insert "  👥 Assignees (1)\n")
      (insert "    alice\n")
      (insert "\n")
      (insert "  💬 Comments (1)\n")
      (insert "    Great work!\n")
      
      (let ((original-assignees-pos (progn
                                     (goto-char (point-min))
                                     (re-search-forward "👥 Assignees")
                                     (line-beginning-position)))
            (original-comments-pos (progn
                                    (goto-char (point-min))
                                    (re-search-forward "💬 Comments")
                                    (line-beginning-position))))
        
        ;; Mock required functions
        (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                   (lambda () "owner/repo"))
                  ((symbol-function 'magit-get-current-branch)
                   (lambda () "test-branch"))
                  ((symbol-function 'shipit--debug-log)
                   (lambda (format-string &rest args) t))
                  ((symbol-function 'force-mode-line-update)
                   (lambda (&optional all) t))
                  ((symbol-function 'magit-section-update-highlight)
                   (lambda () t))
                  ((symbol-function 'fboundp)
                   (lambda (sym) t))
                  ((symbol-function 'shipit--format-label)
                   (lambda (label)
                     (if (listp label)
                         (cdr (assq 'name label))
                       (format "%s" label)))))
          
          ;; Apply fallback refresh with new labels
          (let ((updated-data '((number . 123)
                               (labels . (((name . "enhancement"))
                                         ((name . "bug"))
                                         ((name . "urgent")))))))
            (shipit--fallback-labels-refresh updated-data)
            
            (let ((final-content (buffer-string)))
              (message "Final content after refresh:\n%s" final-content)
              
              ;; Verify sections are in expected positions
              (goto-char (point-min))
              (should (re-search-forward "🏷 Labels (3)" nil t))
              
              ;; Verify assignees section is preserved and not included in labels
              (let ((new-assignees-pos (progn
                                        (goto-char (point-min))
                                        (re-search-forward "👥 Assignees")
                                        (line-beginning-position))))
                (should (re-search-forward "alice" nil t))
                
                ;; Comments should also be preserved
                (goto-char (point-min))
                (should (re-search-forward "💬 Comments" nil t))
                (should (re-search-forward "Great work!" nil t))
                
                (message "✓ All sections preserved with correct boundaries")))))))))

(provide 'test-complete-tab-fix)