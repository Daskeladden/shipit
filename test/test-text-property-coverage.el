;;; test-text-property-coverage.el --- Tests to verify text properties are set correctly -*- lexical-binding: t; -*-

;; Tests that would have caught the missing text properties in diff section

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load shipit modules
(require 'shipit-pr-sections)
(require 'shipit-http)
(require 'shipit-core)

;;; Code:

(defmacro test-with-comment-insertion (&rest body)
  "Execute BODY with mocked comment insertion setup."
  `(let ((test-inserted-comments nil))
     (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                (lambda () "test/repo"))
               
               ;; Mock functions that the comment insertion depends on
               ((symbol-function 'shipit--format-timestamp)
                (lambda (timestamp) "2 hours ago"))
               
               ((symbol-function 'shipit--format-comment-reactions)
                (lambda (comment &optional compact) nil))
               
               ((symbol-function 'shipit--insert-comment-body-only)
                (lambda (comment indent keymap &optional file-path repo pr-number)
                  ;; Record what parameters were passed
                  (push (list 'body-insert comment indent keymap file-path repo pr-number) test-inserted-comments)
                  (insert (format "   Comment body: %s\n" (or (cdr (assq 'body comment)) "")))))
               
               ;; Mock magit functions to prevent errors
               ((symbol-function 'magit-insert-section)
                (macro . (lambda (spec &rest body)
                          `(progn ,@body))))
               
               ((symbol-function 'magit-insert-section-body)
                (macro . (lambda (&rest body)
                          `(progn ,@body))))
               
               ((symbol-function 'magit-insert-heading)
                (lambda (heading)
                  (insert heading "\n")))
               
               ((symbol-function 'shipit--get-pr-head-sha)
                (lambda (pr-number) "abc123def"))
               
               ((symbol-function 'shipit--debug-log)
                (lambda (format-string &rest args) nil)))
       ,@body)))

(ert-deftest test-status-section-sets-text-properties ()
  "Test that status section comment insertion sets required text properties."
  (test-with-comment-insertion
    (with-temp-buffer
      ;; Create a test comment with file and line info
      (let ((test-comment `((id . 12345)
                           (body . "Test comment body")
                           (path . "src/main.js") 
                           (line . 42)
                           (user . ((login . "testuser")))
                           (created_at . "2023-01-01T12:00:00Z")))
            (threads (make-hash-table :test 'equal))
            (file-path "src/main.js")
            (is-outdated nil)
            (depth 0)
            (show-context nil)
            (repo "test/repo")
            (pr-number 123))
        
        ;; Insert comment using status section function
        (shipit--insert-status-hierarchical-comment 
         test-comment threads file-path is-outdated depth show-context repo pr-number)
        
        ;; Check that text properties were set correctly
        (goto-char (point-min))
        
        ;; Find a position where shipit-comment property should be set
        (let ((found-props nil))
          (while (< (point) (point-max))
            (let ((props (text-properties-at (point))))
              (when (plist-get props 'shipit-comment)
                (push (list :position (point)
                           :file-path (plist-get props 'shipit-file-path)
                           :line-number (plist-get props 'shipit-line-number)
                           :repo (plist-get props 'shipit-repo)
                           :pr-number (plist-get props 'shipit-pr-number))
                      found-props)))
            (forward-char 1))
          
          ;; Should find at least one place with complete text properties
          (should (> (length found-props) 0))
          
          ;; Verify the properties are correct
          (let ((props (car found-props)))
            (should (string= (plist-get props :file-path) "src/main.js"))
            (should (= (plist-get props :line-number) 42))
            (should (string= (plist-get props :repo) "test/repo"))
            (should (= (plist-get props :pr-number) 123))))))))

(ert-deftest test-diff-section-sets-text-properties ()
  "Test that diff section comment insertion sets required text properties."
  (test-with-comment-insertion
    (with-temp-buffer
      ;; Create a test comment with file and line info
      (let ((test-comment `((id . 67890)
                           (body . "Diff comment body")
                           (path . "lib/utils.js")
                           (line . 15)
                           (user . ((login . "reviewer")))
                           (created_at . "2023-01-01T15:00:00Z")))
            (threads (make-hash-table :test 'equal))
            (depth 0)
            (pr-number 456))
        
        ;; Insert comment using diff section function
        (shipit--insert-diff-hierarchical-comment test-comment threads depth pr-number)
        
        ;; Check that text properties were set correctly
        (goto-char (point-min))
        
        ;; Find a position where shipit-comment property should be set
        (let ((found-props nil))
          (while (< (point) (point-max))
            (let ((props (text-properties-at (point))))
              (when (plist-get props 'shipit-comment)
                (push (list :position (point)
                           :file-path (plist-get props 'shipit-file-path)
                           :line-number (plist-get props 'shipit-line-number)
                           :repo (plist-get props 'shipit-repo)
                           :pr-number (plist-get props 'shipit-pr-number))
                      found-props)))
            (forward-char 1))
          
          ;; Should find at least one place with complete text properties
          (should (> (length found-props) 0))
          
          ;; Verify the properties are correct
          (let ((props (car found-props)))
            (should (string= (plist-get props :file-path) "lib/utils.js"))
            (should (= (plist-get props :line-number) 15))
            (should (string= (plist-get props :repo) "test/repo"))
            (should (= (plist-get props :pr-number) 456))))))))

(ert-deftest test-real-user-workflow-text-property-detection ()
  "Test the complete user workflow: comment insertion → text property detection → API call."
  (test-with-comment-insertion
    (with-temp-buffer
      (let ((test-reply-api-calls nil)
            (test-comment `((id . 11111)
                           (body . "Original comment")
                           (path . "test.py")
                           (line . 99)
                           (user . ((login . "author")))
                           (created_at . "2023-01-01T10:00:00Z"))))
        
        ;; Mock the API functions to track calls
        (cl-letf (((symbol-function 'shipit--reply-to-inline-comment)
                   (lambda (pr-number parent-comment-id body)
                     (push (list 'inline-reply pr-number parent-comment-id body) test-reply-api-calls)
                     (message "Called inline reply API")))
                  
                  ((symbol-function 'shipit--add-general-comment-to-pr)
                   (lambda (pr-number body)
                     (push (list 'general-comment pr-number body) test-reply-api-calls)
                     (message "Called general comment API")))
                  
                  ((symbol-function 'shipit--refresh-after-comment-operation)
                   (lambda (is-inline) nil))
                  
                  ((symbol-function 'shipit--in-general-comments-section-p)
                   (lambda () nil))
                  
                  ((symbol-function 'read-string)
                   (lambda (prompt) "Test reply")))
          
          ;; Step 1: Insert comment using DIFF section (the bug was here)
          (shipit--insert-diff-hierarchical-comment test-comment (make-hash-table) 0 789)
          
          ;; Step 2: Position cursor on comment text
          (goto-char (point-min))
          (search-forward "Comment body:")
          
          ;; Step 3: Execute the real user action (simple comment dialog)
          ;; This should detect text properties and use inline API
          (shipit--simple-comment-dialog 789 "test/repo")
          
          ;; Step 4: Verify inline API was called (not general API)
          (let ((inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                                test-reply-api-calls))
                (general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                                 test-reply-api-calls)))
            ;; This test would FAIL before the fix because diff section
            ;; didn't set shipit-line-number text property
            (should (= (length inline-calls) 1))
            (should (= (length general-calls) 0))
            
            ;; Verify correct parameters
            (should (= (nth 1 (car inline-calls)) 789))  ; PR number
            (should (= (nth 2 (car inline-calls)) 11111)) ; Parent comment ID  
            (should (string= (nth 3 (car inline-calls)) "Test reply")))))))) ; Reply body

(ert-deftest test-missing-text-properties-fallback-to-general ()
  "Test that missing text properties correctly fall back to general comments."
  (with-temp-buffer
    (let ((test-reply-api-calls nil))
      ;; Mock the API functions to track calls
      (cl-letf (((symbol-function 'shipit--reply-to-inline-comment)
                 (lambda (pr-number parent-comment-id body)
                   (push (list 'inline-reply pr-number parent-comment-id body) test-reply-api-calls)
                   (message "Called inline reply API")))
                
                ((symbol-function 'shipit--add-general-comment-to-pr)
                 (lambda (pr-number body)
                   (push (list 'general-comment pr-number body) test-reply-api-calls)
                   (message "Called general comment API")))
                
                ((symbol-function 'shipit--refresh-after-comment-operation)
                 (lambda (is-inline) nil))
                
                ((symbol-function 'shipit--in-general-comments-section-p)
                 (lambda () nil))
                
                ((symbol-function 'read-string)
                 (lambda (prompt) "General reply"))
                
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "test/repo")))
        
        ;; Insert text WITHOUT shipit text properties (simulates the bug)
        (insert "Some comment text without properties\n")
        (goto-char (point-min))
        
        ;; Execute comment dialog - should fall back to general comment
        (shipit--simple-comment-dialog 999 "test/repo")
        
        ;; Verify general API was called (not inline API)
        (let ((inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                              test-reply-api-calls))
              (general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                               test-reply-api-calls)))
          (should (= (length inline-calls) 0))
          (should (= (length general-calls) 1))
          
          ;; Verify correct parameters
          (should (= (nth 1 (car general-calls)) 999))  ; PR number
          (should (string= (nth 2 (car general-calls)) "General reply"))))))) ; Reply body

(provide 'test-text-property-coverage)
;;; test-text-property-coverage.el ends here