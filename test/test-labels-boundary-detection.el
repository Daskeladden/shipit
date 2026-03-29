;;; test-labels-boundary-detection.el --- Test boundary detection robustness -*- lexical-binding: t; -*-

(require 'ert)
(require 'shipit-diff)

(ert-deftest test-boundary-detection-with-misaligned-sections ()
  "Test that boundary detection works even with misaligned sections."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create buffer with intentionally misaligned next section
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")
      (insert "   👥 Assignees (1)\n")  ; Intentionally misaligned (3 spaces instead of 2)
      (insert "    alice\n")
      
      ;; Mock functions
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
        
        (let ((original-content (buffer-string)))
          (message "Original buffer with misaligned next section:")
          (message "%s" original-content)
          
          ;; Apply fallback refresh with new labels
          (let ((test-data '((number . 123)
                            (labels . (((name . "enhancement"))
                                      ((name . "bug"))
                                      ((name . "feature")))))))
            (shipit--fallback-labels-refresh test-data)
            
            (let ((final-content (buffer-string)))
              (message "Final buffer after refresh:")
              (message "%s" final-content)
              
              ;; Verify that:
              ;; 1. Labels section was updated correctly
              (should (string-match "🏷 Labels (3)" final-content))
              (should (string-match "feature" final-content))
              
              ;; 2. The misaligned next section is still there and wasn't corrupted
              (should (string-match "👥 Assignees" final-content))
              (should (string-match "alice" final-content))
              
              ;; 3. The boundary detection worked correctly
              ;; (if it failed, the assignees section might have been deleted)
              (should (string-match "   👥 Assignees (1)" final-content))
              
              (message "✓ Boundary detection handled misaligned sections correctly"))))))))

(ert-deftest test-boundary-detection-robustness ()
  "Test boundary detection with various spacing issues."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Test multiple spacing scenarios
      (dolist (next-section-spacing '("  " "   " "    " " "))
        (erase-buffer)
        
        ;; Create buffer with variable spacing
        (insert "  🏷 Labels (1)\n")
        (insert "    test-label\n")
        (insert (format "%s👥 Assignees (0)\n" next-section-spacing))
        (insert "    No assignees\n")
        
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
          
          (let ((test-data '((number . 123)
                            (labels . (((name . "new-label")))))))
            (shipit--fallback-labels-refresh test-data)
            
            (let ((result-content (buffer-string)))
              ;; Should handle any spacing and preserve the next section
              (should (string-match "new-label" result-content))
              (should (string-match "👥 Assignees" result-content))
              (message "✓ Handled spacing: '%s'" next-section-spacing))))))))

(provide 'test-labels-boundary-detection)
;;; test-labels-boundary-detection.el ends here