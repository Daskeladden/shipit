;;; test-text-property-regression.el --- Regression tests for text properties -*- lexical-binding: t; -*-

;; These tests would have caught the missing text properties bug

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load shipit modules
(require 'shipit-pr-sections)
(require 'shipit-diff)

;;; Code:

(ert-deftest test-diff-comment-has-required-text-properties ()
  "Test that diff hierarchical comments set shipit-line-number text property.
This test would have FAILED before the fix was applied."
  (with-temp-buffer
    (let ((test-comment `((id . 12345)
                         (body . "Test comment")
                         (path . "src/test.js")
                         (line . 42)
                         (user . ((login . "testuser")))
                         (created_at . "2023-01-01T12:00:00Z")))
          (threads (make-hash-table :test 'equal))
          (pr-number 123))
      
      ;; Mock required functions to prevent errors
      (cl-letf (((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args) nil))
                ((symbol-function 'shipit--format-timestamp)
                 (lambda (timestamp) "2 hours ago"))
                ((symbol-function 'shipit--format-comment-reactions)
                 (lambda (comment &optional compact) nil))
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "test/repo"))
                ((symbol-function 'shipit--get-pr-head-sha)
                 (lambda (pr-number) "abc123"))
                ((symbol-function 'shipit--insert-comment-body-only)
                 (lambda (comment indent keymap &optional file-path repo pr-number)
                   (insert "Comment body text\n"))))
        
        ;; Insert comment using the DIFF function that had the bug
        (shipit--insert-diff-hierarchical-comment test-comment threads 0 pr-number)
        
        ;; Check that the required text properties exist
        (goto-char (point-min))
        (let ((found-shipit-line-number nil)
              (found-shipit-file-path nil))
          
          ;; Search through buffer for text properties
          (while (< (point) (point-max))
            (let ((line-prop (get-text-property (point) 'shipit-line-number))
                  (file-prop (get-text-property (point) 'shipit-file-path)))
              (when line-prop (setq found-shipit-line-number line-prop))
              (when file-prop (setq found-shipit-file-path file-prop)))
            (forward-char 1))
          
          ;; These assertions would FAIL before the fix
          (should found-shipit-line-number)
          (should found-shipit-file-path)
          (should (= found-shipit-line-number 42))
          (should (string= found-shipit-file-path "src/test.js")))))))

(ert-deftest test-status-comment-has-required-text-properties ()
  "Test that status hierarchical comments set shipit-line-number text property.
This test should PASS both before and after the fix."
  (with-temp-buffer
    (let ((test-comment `((id . 67890)
                         (body . "Status comment")
                         (path . "lib/helper.py")
                         (line . 99)
                         (user . ((login . "reviewer")))
                         (created_at . "2023-01-01T15:00:00Z")))
          (threads (make-hash-table :test 'equal))
          (file-path "lib/helper.py")
          (is-outdated nil)
          (depth 0)
          (show-context nil)
          (repo "test/repo")
          (pr-number 456))
      
      ;; Mock required functions to prevent errors
      (cl-letf (((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args) nil))
                ((symbol-function 'shipit--format-timestamp)
                 (lambda (timestamp) "3 hours ago"))
                ((symbol-function 'shipit--format-comment-reactions)
                 (lambda (comment &optional compact) nil))
                ((symbol-function 'shipit--insert-comment-body-only)
                 (lambda (comment indent keymap file-path repo pr-number)
                   (insert "Status comment body\n"))))
        
        ;; Insert comment using the STATUS function (this worked before)
        (shipit--insert-status-hierarchical-comment 
         test-comment threads file-path is-outdated depth show-context repo pr-number)
        
        ;; Check that the required text properties exist
        (goto-char (point-min))
        (let ((found-shipit-line-number nil)
              (found-shipit-file-path nil))
          
          ;; Search through buffer for text properties
          (while (< (point) (point-max))
            (let ((line-prop (get-text-property (point) 'shipit-line-number))
                  (file-prop (get-text-property (point) 'shipit-file-path)))
              (when line-prop (setq found-shipit-line-number line-prop))
              (when file-prop (setq found-shipit-file-path file-prop)))
            (forward-char 1))
          
          ;; These assertions should PASS both before and after the fix
          (should found-shipit-line-number)
          (should found-shipit-file-path)
          (should (= found-shipit-line-number 99))
          (should (string= found-shipit-file-path "lib/helper.py")))))))

(ert-deftest test-user-workflow-text-property-detection ()
  "Test realistic user workflow that depends on text properties being set correctly."
  (with-temp-buffer
    (let ((test-comment `((id . 11111)
                         (body . "Review comment")
                         (path . "main.go")
                         (line . 123)
                         (user . ((login . "maintainer")))
                         (created_at . "2023-01-01T09:00:00Z")))
          (api-calls nil))
      
      ;; Mock all the functions
      (cl-letf (((symbol-function 'shipit--debug-log)
                 (lambda (format-string &rest args) nil))
                ((symbol-function 'shipit--format-timestamp)
                 (lambda (timestamp) "1 hour ago"))
                ((symbol-function 'shipit--format-comment-reactions)
                 (lambda (comment &optional compact) nil))
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "test/repo"))
                ((symbol-function 'shipit--get-pr-head-sha)
                 (lambda (pr-number) "def456"))
                ((symbol-function 'shipit--insert-comment-body-only)
                 (lambda (comment indent keymap &optional file-path repo pr-number)
                   (insert "Review comment content\n")))
                
                ;; Mock reply functions to track which API is called
                ((symbol-function 'shipit--reply-to-inline-comment)
                 (lambda (pr-number parent-id body)
                   (push 'inline-api api-calls)))
                ((symbol-function 'shipit--add-general-comment-to-pr)
                 (lambda (pr-number body)
                   (push 'general-api api-calls)))
                ((symbol-function 'read-string)
                 (lambda (prompt) "Reply text")))
        
        ;; Insert comment using DIFF function (the one that was buggy)
        (shipit--insert-diff-hierarchical-comment test-comment (make-hash-table) 0 789)
        
        ;; Position cursor somewhere in the comment text
        (goto-char (point-min))
        (search-forward "Review comment")
        
        ;; Execute the user action that depends on text properties
        (shipit--simple-comment-dialog 789 "test/repo")
        
        ;; Verify inline API was called (this would FAIL before the fix)
        (should (member 'inline-api api-calls))
        (should-not (member 'general-api api-calls))))))

(ert-deftest test-broken-version-would-use-general-api ()
  "Test that simulates the broken version behavior.
This demonstrates what would happen before the fix."
  (with-temp-buffer
    (let ((api-calls nil))
      
      ;; Mock the functions
      (cl-letf (((symbol-function 'shipit--reply-to-inline-comment)
                 (lambda (pr-number parent-id body)
                   (push 'inline-api api-calls)))
                ((symbol-function 'shipit--add-general-comment-to-pr)
                 (lambda (pr-number body)
                   (push 'general-api api-calls)))
                ((symbol-function 'read-string)
                 (lambda (prompt) "Reply text"))
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "test/repo")))
        
        ;; Insert text WITHOUT the required text properties 
        ;; (simulates what the broken diff function did)
        (insert "Comment text without shipit-line-number property\n")
        (goto-char (point-min))
        
        ;; Execute user action - should fall back to general API
        (shipit--simple-comment-dialog 999 "test/repo")
        
        ;; Before the fix, this is what would happen
        (should (member 'general-api api-calls))
        (should-not (member 'inline-api api-calls))))))

(provide 'test-text-property-regression)
;;; test-text-property-regression.el ends here