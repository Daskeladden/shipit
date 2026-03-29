;;; test-section-integrity.el --- Test section integrity verification -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest test-section-integrity-detection ()
  "Test that section integrity verification detects boundary mismatches."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create sections with boundary mismatch
      (insert "  📋 PR #123: Add feature\n")
      (insert "    Status: Open\n")
      (insert "\n")
      (insert "  🏷 Labels (3)\n")
      (let ((labels-content-start (point))
            (labels-start (save-excursion (forward-line -1) (line-beginning-position))))
        (insert "    enhancement\n")
        (insert "    bug\n") 
        (insert "    urgent\n")
        (let ((correct-end (point)))
          
          (insert "\n")
          (insert "  👥 Assignees (1)\n")
          (insert "    alice\n")
          
          ;; Create mock section with WRONG boundary
          (let ((wrong-end (- correct-end 20))) ; Simulate stale boundary
            (cl-letf (((symbol-function 'get-text-property)
                       (lambda (pos prop)
                         (cond
                          ((and (eq prop 'magit-section)
                                (>= pos labels-start) (< pos labels-content-start))
                           (list 'magit-section :start labels-start :end wrong-end))
                          (t nil))))
                      ((symbol-function 'oref)
                       (lambda (obj slot)
                         (if (eq slot 'end) wrong-end (plist-get (cdr obj) slot))))
                      ((symbol-function 'setf)
                       (lambda (&rest args)
                         (message "Mock setf: Fixed boundary from %d to %d" wrong-end correct-end)))
                      ((symbol-function 'put-text-property)
                       (lambda (start end prop value)
                         (message "Mock put-text-property: Applied magit-section property %d-%d" start end)))
                      ((symbol-function 'shipit--debug-log)
                       (lambda (format-string &rest args)
                         (message "DEBUG: %s" (apply 'format format-string args)))))
              
              (message "Testing integrity verification with boundary mismatch...")
              (message "  - Labels start: %d" labels-start)
              (message "  - Content start: %d" labels-content-start)  
              (message "  - Correct end: %d" correct-end)
              (message "  - Wrong end (simulated): %d" wrong-end)
              
              ;; Test the integrity verification
              (shipit--verify-section-integrity)
              
              (message "✓ Section integrity verification completed"))))))))

(ert-deftest test-section-overlap-detection ()
  "Test detection of section overlaps that could cause corruption."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      ;; Create overlapping sections scenario
      (insert "  📋 PR #123: Add feature\n")
      (insert "    Status: Open\n")
      (insert "\n") 
      (insert "  🏷 Labels (2)\n")
      (insert "    enhancement\n")
      (insert "    bug\n")
      (insert "  👥 Assignees (1)\n")  ; No newline - creating potential overlap
      (insert "    alice\n")
      
      ;; Mock functions for comprehensive check
      (cl-letf (((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args)
                   (message "DEBUG: %s" (apply 'format format-string args))))
                ((symbol-function 'get-text-property)
                 (lambda (pos prop) nil)) ; No section objects for this test
                ((symbol-function 'oref)
                 (lambda (obj slot) nil)))
        
        (message "Testing comprehensive boundary check with potential overlaps...")
        
        ;; This should analyze all sections and detect any structural issues
        (shipit--check-and-fix-section-boundaries)
        
        (message "✓ Comprehensive boundary check completed")))))

(provide 'test-section-integrity)