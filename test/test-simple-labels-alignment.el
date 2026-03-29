;;; test-simple-labels-alignment.el --- Simple labels alignment test -*- lexical-binding: t; -*-

(require 'ert)
(require 'shipit-diff)

(defun test-check-labels-alignment (buffer-content)
  "Check if labels section in BUFFER-CONTENT has proper 4-space indentation.
Returns list of misaligned lines."
  (let ((lines (split-string buffer-content "\n"))
        (in-labels-section nil)
        (issues '()))
    (dolist (line lines)
      (cond
       ;; Found labels section
       ((string-match "🏷 Labels" line)
        (setq in-labels-section t)
        (unless (string-match "^  " line)
          (push (format "Labels header misaligned: %s" line) issues)))
       ;; Found another section, exit labels
       ((and in-labels-section (string-match "^  [📋👥💬]" line))
        (setq in-labels-section nil))
       ;; In labels section, check content
       ((and in-labels-section
             (string-match "[a-zA-Z]" line)
             (not (string-match "^$" line)))
        (unless (string-match "^    " line)
          (push (format "Label content misaligned: %s" line) issues)))))
    issues))

(ert-deftest test-fallback-refresh-maintains-alignment ()
  "Test that fallback refresh maintains proper alignment."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create initial buffer
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")
      (insert "  👥 Assignees (1)\n")
      (insert "    alice\n")
      
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
        
        ;; Check initial alignment
        (let ((initial-issues (test-check-labels-alignment (buffer-string))))
          (should (null initial-issues)))
        
        ;; Apply fallback refresh
        (let ((test-data '((number . 123)
                          (labels . (((name . "enhancement"))
                                    ((name . "bug"))
                                    ((name . "feature")))))))
          (shipit--fallback-labels-refresh test-data)
          
          ;; Check alignment after refresh
          (let ((final-issues (test-check-labels-alignment (buffer-string))))
            (when final-issues
              (message "Alignment issues after refresh:")
              (dolist (issue final-issues)
                (message "  %s" issue)))
            (should (null final-issues))))))))

(provide 'test-simple-labels-alignment)
;;; test-simple-labels-alignment.el ends here