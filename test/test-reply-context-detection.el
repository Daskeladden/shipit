;;; test-reply-context-detection.el --- Tests for inline vs general reply context detection -*- lexical-binding: t; -*-

;; Tests to ensure replies are posted as inline comments when appropriate
;; and as general comments when appropriate

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load shipit modules
(require 'shipit-diff)
(require 'shipit-http)
(require 'shipit-core)

;;; Code:

(defvar test-reply-api-calls nil
  "List of API calls made during reply tests for validation.")

(defmacro test-with-reply-context (&rest body)
  "Execute BODY with mocked reply context setup."
  `(let ((test-reply-api-calls nil))
     (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                (lambda () "test/repo"))
               
               ;; Mock API calls to track which endpoint was used
               ((symbol-function 'shipit--reply-to-inline-comment)
                (lambda (pr-number parent-comment-id body)
                  (push (list 'inline-reply pr-number parent-comment-id body) test-reply-api-calls)
                  (message "Called inline reply API")))
               
               ((symbol-function 'shipit--add-general-comment-to-pr)
                (lambda (pr-number body)
                  (push (list 'general-comment pr-number body) test-reply-api-calls)
                  (message "Called general comment API")))
               
               ((symbol-function 'shipit--refresh-after-comment-operation)
                (lambda (is-inline)
                  (push (list 'refresh-called is-inline) test-reply-api-calls))))
       ,@body)))

(ert-deftest test-inline-comment-reply-uses-inline-api ()
  "Test that replying to an inline comment uses inline reply API."
  (test-with-reply-context
    ;; Set up context for inline comment reply
    (let ((file-path "src/main.js")
          (line-number 42)
          (in-general nil)  ; Not in general comments section
          (comment-info (list 12345 "Original comment body")))
      
      ;; Execute reply action - this should use inline reply API
      (shipit--execute-action 'reply "My reply" 123 "test/repo" comment-info
                              file-path line-number in-general nil nil)
      
      ;; Verify inline reply API was called
      (let ((inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                            test-reply-api-calls)))
        (should (= (length inline-calls) 1))
        (should (= (nth 1 (car inline-calls)) 123))  ; PR number
        (should (= (nth 2 (car inline-calls)) 12345)) ; Parent comment ID
        (should (string= (nth 3 (car inline-calls)) "My reply"))) ; Body
      
      ;; Verify general comment API was NOT called
      (let ((general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                             test-reply-api-calls)))
        (should (= (length general-calls) 0)))
      
      ;; Verify refresh was called with inline=t
      (let ((refresh-calls (cl-remove-if-not (lambda (call) (eq (car call) 'refresh-called))
                                             test-reply-api-calls)))
        (should (= (length refresh-calls) 1))
        (should (eq (nth 1 (car refresh-calls)) t))))))  ; is-inline=t

(ert-deftest test-general-comment-reply-uses-general-api ()
  "Test that replying to a general comment uses general comment API."
  (test-with-reply-context
    ;; Set up context for general comment reply
    (let ((file-path nil)      ; No file path for general comments
          (line-number nil)    ; No line number for general comments  
          (in-general t)       ; In general comments section
          (comment-info (list 67890 "Original general comment")))
      
      ;; Execute reply action - this should use general comment API
      (shipit--execute-action 'reply "My general reply" 123 "test/repo" comment-info
                              file-path line-number in-general nil nil)
      
      ;; Verify general comment API was called
      (let ((general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                             test-reply-api-calls)))
        (should (= (length general-calls) 1))
        (should (= (nth 1 (car general-calls)) 123))  ; PR number
        (should (string= (nth 2 (car general-calls)) "My general reply"))) ; Body
      
      ;; Verify inline reply API was NOT called
      (let ((inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                            test-reply-api-calls)))
        (should (= (length inline-calls) 0)))
      
      ;; Verify refresh was called with inline=nil
      (let ((refresh-calls (cl-remove-if-not (lambda (call) (eq (car call) 'refresh-called))
                                             test-reply-api-calls)))
        (should (= (length refresh-calls) 1))
        (should (eq (nth 1 (car refresh-calls)) nil))))))  ; is-inline=nil

(ert-deftest test-edge-case-missing-file-path-uses-general-api ()
  "Test that inline comment without file-path falls back to general comment API."
  (test-with-reply-context
    ;; Set up context with missing file-path (edge case that might cause the bug)
    (let ((file-path nil)      ; Missing file path - should cause fallback
          (line-number 42)     ; Has line number but no file
          (in-general nil)     ; Not explicitly in general section
          (comment-info (list 11111 "Inline comment with missing file context")))
      
      ;; Execute reply action - should fall back to general comment API
      (shipit--execute-action 'reply "Reply to broken inline context" 123 "test/repo" comment-info
                              file-path line-number in-general nil nil)
      
      ;; Verify it fell back to general comment API (not inline)
      (let ((general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                             test-reply-api-calls))
            (inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                            test-reply-api-calls)))
        (should (= (length general-calls) 1))
        (should (= (length inline-calls) 0))))))

(ert-deftest test-edge-case-missing-line-number-uses-general-api ()
  "Test that inline comment without line-number falls back to general comment API."
  (test-with-reply-context
    ;; Set up context with missing line-number (edge case that might cause the bug)
    (let ((file-path "src/test.js")  ; Has file path
          (line-number nil)          ; Missing line number - should cause fallback
          (in-general nil)           ; Not explicitly in general section
          (comment-info (list 22222 "Inline comment with missing line number")))
      
      ;; Execute reply action - should fall back to general comment API
      (shipit--execute-action 'reply "Reply to broken line context" 123 "test/repo" comment-info
                              file-path line-number in-general nil nil)
      
      ;; Verify it fell back to general comment API (not inline)
      (let ((general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                             test-reply-api-calls))
            (inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                            test-reply-api-calls)))
        (should (= (length general-calls) 1))
        (should (= (length inline-calls) 0))))))

(ert-deftest test-in-general-flag-overrides-file-context ()
  "Test that in-general=t overrides file/line context and uses general API."
  (test-with-reply-context
    ;; Set up context with file/line info but in-general=t (should override)
    (let ((file-path "src/override.js")  ; Has file context
          (line-number 100)              ; Has line context  
          (in-general t)                 ; But explicitly in general section - should override
          (comment-info (list 33333 "General comment with misleading file context")))
      
      ;; Execute reply action - should use general comment API due to in-general=t
      (shipit--execute-action 'reply "General reply despite file context" 123 "test/repo" comment-info
                              file-path line-number in-general nil nil)
      
      ;; Verify it used general comment API (not inline) due to in-general override
      (let ((general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                             test-reply-api-calls))
            (inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                            test-reply-api-calls)))
        (should (= (length general-calls) 1))
        (should (= (length inline-calls) 0))))))

(ert-deftest test-inline-comment-text-properties-provide-context ()
  "Test that inline comments have correct text properties for context detection."
  (test-with-reply-context
    ;; Create a mock inline comment with proper text properties
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        ;; Insert a mock inline comment with text properties
        (insert "    └─ @user commented on src/main.js:42\n")
        (insert "    Some inline comment text\n")
        
        ;; Add text properties that should be present on inline comments
        (let ((start (point-min))
              (end (point-max)))
          (add-text-properties start end
                               '(shipit-comment t
                                 shipit-comment-id 12345
                                 shipit-file-path "src/main.js"
                                 shipit-line-number 42
                                 shipit-comment-body "Some inline comment text")))
        
        ;; Position cursor on the comment
        (goto-char (point-min))
        (forward-line 1) ; Move to comment text line
        
        ;; Test that text properties are present
        (should (get-text-property (point) 'shipit-comment))
        (should (string= (get-text-property (point) 'shipit-file-path) "src/main.js"))
        (should (= (get-text-property (point) 'shipit-line-number) 42))
        (should (= (get-text-property (point) 'shipit-comment-id) 12345))
        
        ;; Mock the context detection functions
        (cl-letf (((symbol-function 'shipit--in-general-comments-section-p)
                   (lambda () nil))  ; Not in general section
                  ((symbol-function 'shipit--get-comment-at-point)
                   (lambda () (list 12345 "Some inline comment text")))
                  ((symbol-function 'derived-mode-p)
                   (lambda (&rest modes) (memq 'magit-status-mode modes))))
          
          ;; Test what the mini-editor would detect
          (let ((stored-file-path (get-text-property (point) 'shipit-file-path))
                (stored-line-number (get-text-property (point) 'shipit-line-number))
                (in-general nil)  ; shipit--in-general-comments-section-p returns nil
                (in-diff nil))    ; Not in diff mode
            
            ;; This simulates the logic in shipit--comment-mini-editor
            (let ((effective-file-path (or stored-file-path nil)) ; No file-path from diff mode
                  (effective-line-number (or stored-line-number nil))) ; No line-number from diff mode
              
              ;; The condition that determines inline vs general reply
              (let ((should-be-inline (and effective-file-path effective-line-number (not in-general))))
                (should should-be-inline)
                (should (string= effective-file-path "src/main.js"))
                (should (= effective-line-number 42))))))))))

(ert-deftest test-general-comment-has-no-file-properties ()
  "Test that general comments lack file/line properties."
  (test-with-reply-context
    ;; Create a mock general comment without file properties
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        ;; Insert a mock general comment with text properties
        (insert "    └─ @user commented\n")
        (insert "    Some general comment text\n")
        
        ;; Add text properties that should be present on general comments (no file/line)
        (let ((start (point-min))
              (end (point-max)))
          (add-text-properties start end
                               '(shipit-comment t
                                 shipit-comment-id 67890
                                 shipit-comment-body "Some general comment text")))
        
        ;; Position cursor on the comment
        (goto-char (point-min))
        (forward-line 1) ; Move to comment text line
        
        ;; Test that file properties are NOT present
        (should (get-text-property (point) 'shipit-comment))
        (should (null (get-text-property (point) 'shipit-file-path)))
        (should (null (get-text-property (point) 'shipit-line-number)))
        (should (= (get-text-property (point) 'shipit-comment-id) 67890))
        
        ;; Mock the context detection functions for general comments
        (cl-letf (((symbol-function 'shipit--in-general-comments-section-p)
                   (lambda () t))  ; In general section
                  ((symbol-function 'shipit--get-comment-at-point)
                   (lambda () (list 67890 "Some general comment text"))))
          
          ;; Test what the mini-editor would detect
          (let ((stored-file-path (get-text-property (point) 'shipit-file-path))
                (stored-line-number (get-text-property (point) 'shipit-line-number))
                (in-general t)   ; shipit--in-general-comments-section-p returns t
                (in-diff nil))   ; Not in diff mode
            
            ;; This simulates the logic in shipit--comment-mini-editor
            (let ((effective-file-path (or stored-file-path nil)) ; No file-path from anywhere
                  (effective-line-number (or stored-line-number nil))) ; No line-number from anywhere
              
              ;; The condition that determines inline vs general reply
              (let ((should-be-inline (and effective-file-path effective-line-number (not in-general))))
                (should-not should-be-inline)  ; Should be general, not inline
                (should (null effective-file-path))
                (should (null effective-line-number))))))))))

(ert-deftest test-inline-comment-reply-fix-integration ()
  "Integration test that inline comment replies now use inline API after fix."
  (test-with-reply-context
    ;; Create a realistic inline comment with the NEW shipit-line-number property
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        ;; Insert a mock inline comment with ALL required text properties
        (insert "    └─ @user commented on src/main.js:42\n")
        (insert "    Some inline comment text\n")
        
        ;; Add text properties that inline comments NOW have (including line number)
        (let ((start (point-min))
              (end (point-max)))
          (add-text-properties start end
                               '(shipit-comment t
                                 shipit-comment-id 12345
                                 shipit-file-path "src/main.js"
                                 shipit-line-number 42
                                 shipit-comment-body "Some inline comment text")))
        
        ;; Position cursor on the comment for reply
        (goto-char (point-min))
        (forward-line 1) ; Move to comment text line
        
        ;; Mock the context functions to simulate the actual mini-editor behavior
        (cl-letf (((symbol-function 'shipit--in-general-comments-section-p)
                   (lambda () nil))  ; Not in general section
                  ((symbol-function 'shipit--get-comment-at-point)
                   (lambda () (list 12345 "Some inline comment text")))
                  ((symbol-function 'shipit-get-pull-request)
                   (lambda (pr-number) `((number . ,pr-number) (title . "Test PR"))))
                  ((symbol-function 'shipit--get-current-file-line)
                   (lambda () nil))  ; No diff-mode detection
                  ((symbol-function 'derived-mode-p)
                   (lambda (&rest modes) (memq 'magit-status-mode modes))))
          
          ;; Simulate the actual reply process with stored context
          (let* ((stored-file-path (get-text-property (point) 'shipit-file-path))
                 (stored-line-number (get-text-property (point) 'shipit-line-number))
                 (in-general nil)    ; Not in general section  
                 (comment-info (list 12345 "Some inline comment text"))
                 ;; These are the effective values that shipit--comment-mini-editor calculates
                 (effective-file-path stored-file-path)    ; Now "src/main.js" 
                 (effective-line-number stored-line-number)) ; Now 42
            
            ;; Execute the reply using the same logic as shipit--execute-action
            (shipit--execute-action 'reply "My inline reply" 123 "test/repo" comment-info
                                    effective-file-path effective-line-number in-general nil nil)
            
            ;; Verify inline reply API was called (not general comment API)
            (let ((inline-calls (cl-remove-if-not (lambda (call) (eq (car call) 'inline-reply))
                                                  test-reply-api-calls))
                  (general-calls (cl-remove-if-not (lambda (call) (eq (car call) 'general-comment))
                                                   test-reply-api-calls)))
              
              ;; This test should PASS after our fix (before fix it would fail)
              (should (= (length inline-calls) 1))     ; Used inline API ✅
              (should (= (length general-calls) 0))    ; Did NOT use general API ✅
              
              ;; Verify correct parameters were passed to inline API
              (should (= (nth 1 (car inline-calls)) 123))    ; PR number
              (should (= (nth 2 (car inline-calls)) 12345))  ; Parent comment ID  
              (should (string= (nth 3 (car inline-calls)) "My inline reply"))))) ; Body
          
          ;; Also verify that the refresh was called with inline=t
          (let ((refresh-calls (cl-remove-if-not (lambda (call) (eq (car call) 'refresh-called))
                                                 test-reply-api-calls)))
            (should (= (length refresh-calls) 1))
            (should (eq (nth 1 (car refresh-calls)) t)))))))  ; inline refresh

(provide 'test-reply-context-detection)
;;; test-reply-context-detection.el ends here