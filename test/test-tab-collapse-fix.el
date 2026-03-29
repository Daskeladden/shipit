;;; test-tab-collapse-fix.el --- Test TAB collapse fix -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest test-labels-section-boundary-fix ()
  "Test that labels section boundary detection excludes labels emoji."
  (with-temp-buffer
    (insert "  🏷 Labels (2)\n")
    (insert "    enhancement\n")
    (insert "    bug\n")
    (insert "\n")
    (insert "  👥 Assignees (1)\n")
    (insert "    alice\n")
    
    ;; Position after labels header
    (goto-char (point-min))
    (re-search-forward "🏷 Labels")
    (forward-line 1)
    (let ((content-start (point)))
      
      ;; Test the fixed regex (should find assignees, not labels)
      (let ((boundary-end (save-excursion
                           (if (re-search-forward "^[ ]*[📋👥👤💬💭🔧⚪❌🟡✅🔵⚫⚠📝📁🌟]" nil t)
                               (match-beginning 0)
                             (point-max)))))
        
        ;; Should find the assignees section, not go back to labels header
        (let ((content-region (buffer-substring content-start boundary-end)))
          (message "Detected content region:\n%s" content-region)
          
          ;; Should contain only labels content
          (should (string-match "enhancement" content-region))
          (should (string-match "bug" content-region))
          
          ;; Should NOT include assignees content
          (should-not (string-match "alice" content-region))
          (should-not (string-match "Assignees" content-region))
          
          (message "✓ Boundary detection correctly excludes next section"))))))

(provide 'test-tab-collapse-fix)