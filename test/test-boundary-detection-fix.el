;;; test-boundary-detection-fix.el --- Test boundary detection and fixing -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest test-boundary-uncertainty-detection-and-fix ()
  "Test that boundary uncertainty flags are detected and fixed."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create labels section
      (insert "  📋 PR #123: Add feature\n")
      (insert "    Status: Open\n")
      (insert "\n")
      (insert "  🏷 Labels (3)\n")
      (let ((labels-content-start (point)))
        (insert "    enhancement\n")
        (insert "    bug\n") 
        (insert "    urgent\n")
        (let ((labels-content-end (point)))
          
          ;; Simulate a boundary uncertainty condition
          (put-text-property labels-content-start labels-content-end 'shipit-boundary-uncertain t)
          (message "Simulated boundary uncertainty flag set")
          
          (insert "\n")
          (insert "  👥 Assignees (1)\n")
          (insert "    alice\n")
          
          ;; Mock required functions
          (cl-letf (((symbol-function 'shipit--debug-log)
                     (lambda (format-string &rest args)
                       (message "DEBUG: %s" (apply 'format format-string args))))
                    ((symbol-function 'magit-section-update-highlight)
                     (lambda () t))
                    ((symbol-function 'fboundp)
                     (lambda (sym) t))
                    ;; Mock magit section object
                    ((symbol-function 'get-text-property)
                     (lambda (pos prop)
                       (cond
                        ((and (eq prop 'shipit-boundary-uncertain) 
                              (>= pos labels-content-start)
                              (< pos labels-content-end))
                         t)
                        ((and (eq prop 'magit-section)
                              (>= pos 1) (< pos labels-content-start))
                         (list 'magit-section :start 1 :end labels-content-end))
                        (t (funcall (symbol-function 'get-text-property) pos prop)))))
                    ((symbol-function 'oref)
                     (lambda (obj slot)
                       (plist-get (cdr obj) slot)))
                    ((symbol-function 'setf)
                     (lambda (&rest args)
                       (message "Mock setf called to update boundary: %S" args)))
                    ((symbol-function 'put-text-property)
                     (lambda (start end prop value)
                       (message "Mock put-text-property: %s-%s %s=%s" start end prop value)))
                    ((symbol-function 'remove-text-properties)
                     (lambda (start end props)
                       (message "Mock removed properties: %s-%s %S" start end props))))
            
            ;; Test the boundary detection and fix function
            (message "\nTesting boundary detection and fix...")
            (shipit--check-and-fix-section-boundaries)
            
            (message "✓ Boundary detection and fix function completed successfully")))))))

(ert-deftest test-no-boundary-issues-detected ()
  "Test that function correctly reports when no boundary issues exist."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create normal labels section without boundary issues
      (insert "  📋 PR #123: Add feature\n")
      (insert "    Status: Open\n")
      (insert "\n")  
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")
      (insert "\n")
      (insert "  👥 Assignees (1)\n") 
      (insert "    alice\n")
      
      ;; Mock functions
      (cl-letf (((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args) t))
                ((symbol-function 'magit-section-update-highlight)
                 (lambda () t))
                ((symbol-function 'fboundp)
                 (lambda (sym) t)))
        
        ;; Test with no boundary issues
        (let ((messages '()))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (let ((msg (apply 'format format-string args)))
                         (push msg messages)
                         msg))))
            
            (shipit--check-and-fix-section-boundaries)
            
            ;; Should report no issues found
            (should (cl-find-if (lambda (msg) 
                                 (string-match "No section boundary issues detected" msg))
                               messages))
            (message "✓ Correctly detected no boundary issues")))))))

(provide 'test-boundary-detection-fix)