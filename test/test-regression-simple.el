;;; test-regression-simple.el --- Simple test that would catch the text property bug -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load shipit modules
(require 'shipit-pr-sections)

;;; Code:

(ert-deftest test-simple-comment-dialog-detects-missing-properties ()
  "Test that shipit--simple-comment-dialog correctly detects missing text properties.
This test demonstrates the exact bug that users experienced."
  (with-temp-buffer
    (let ((api-calls nil))
      
      ;; Mock the API functions to track which one gets called
      (cl-letf (((symbol-function 'shipit--reply-to-inline-comment)
                 (lambda (pr-number parent-id body)
                   (push 'inline-reply api-calls)
                   (message "Called inline reply API")))
                
                ((symbol-function 'shipit--add-general-comment-to-pr)
                 (lambda (pr-number body)
                   (push 'general-comment api-calls)
                   (message "Called general comment API")))
                
                ((symbol-function 'read-string)
                 (lambda (prompt) "Test reply"))
                
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "test/repo")))
        
        ;; Test case 1: WITH proper text properties (should use inline API)
        (erase-buffer)
        (insert "Comment with properties")
        (add-text-properties (point-min) (point-max)
                           `(shipit-comment t
                            shipit-comment-id 12345
                            shipit-file-path "src/main.js"
                            shipit-line-number 42))
        (goto-char (point-min))
        (setq api-calls nil)
        
        (shipit--simple-comment-dialog 123 "test/repo")
        
        ;; Should use inline reply API
        (should (member 'inline-reply api-calls))
        (should-not (member 'general-comment api-calls))
        
        ;; Test case 2: WITHOUT shipit-line-number property (should use general API)
        ;; This simulates the bug in diff section
        (erase-buffer)
        (insert "Comment missing line number property")
        (add-text-properties (point-min) (point-max)
                           `(shipit-comment t
                            shipit-comment-id 67890
                            shipit-file-path "src/main.js"
                            ;; shipit-line-number MISSING - this was the bug!
                            ))
        (goto-char (point-min))
        (setq api-calls nil)
        
        (shipit--simple-comment-dialog 123 "test/repo")
        
        ;; Should fall back to general comment API
        (should (member 'general-comment api-calls))
        (should-not (member 'inline-reply api-calls))
        
        ;; Test case 3: WITHOUT shipit-file-path property (should use general API)
        (erase-buffer)
        (insert "Comment missing file path property")
        (add-text-properties (point-min) (point-max)
                           `(shipit-comment t
                            shipit-comment-id 11111
                            ;; shipit-file-path MISSING
                            shipit-line-number 99))
        (goto-char (point-min))
        (setq api-calls nil)
        
        (shipit--simple-comment-dialog 123 "test/repo")
        
        ;; Should fall back to general comment API
        (should (member 'general-comment api-calls))
        (should-not (member 'inline-reply api-calls))
        
        ;; Test case 4: WITHOUT any shipit properties (should use general API)
        (erase-buffer)
        (insert "Comment with no shipit properties at all")
        ;; No text properties added
        (goto-char (point-min))
        (setq api-calls nil)
        
        (shipit--simple-comment-dialog 123 "test/repo")
        
        ;; Should use general comment API
        (should (member 'general-comment api-calls))
        (should-not (member 'inline-reply api-calls))))))

(ert-deftest test-demonstrate-bug-scenario ()
  "Test that demonstrates the exact scenario that was broken.
Before the fix, diff section comments would be missing shipit-line-number,
causing replies to be posted as general comments instead of inline replies."
  (with-temp-buffer
    (let ((api-calls nil))
      
      ;; Mock functions
      (cl-letf (((symbol-function 'shipit--reply-to-inline-comment)
                 (lambda (pr-number parent-id body)
                   (push 'inline-reply api-calls)
                   (message "Called inline reply API")))
                
                ((symbol-function 'shipit--add-general-comment-to-pr)
                 (lambda (pr-number body)
                   (push 'general-comment api-calls)
                   (message "Called general comment API")))
                
                ((symbol-function 'read-string)
                 (lambda (prompt) "My reply"))
                
                ((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "test/repo")))
        
        ;; Simulate what the BROKEN diff section was doing:
        ;; Setting some shipit properties but NOT shipit-line-number
        (insert "This is an inline comment in diff buffer")
        (add-text-properties (point-min) (point-max)
                           `(shipit-comment t
                            shipit-comment-id 99999
                            shipit-comment-body "Original comment text"
                            ;; shipit-file-path and shipit-line-number were MISSING
                            ;; in the broken diff section code
                            keymap ,(make-sparse-keymap)))
        
        (goto-char (point-min))
        
        ;; User clicks and tries to reply - this should detect missing properties
        (shipit--simple-comment-dialog 456 "test/repo")
        
        ;; Before the fix, this would incorrectly use general comment API
        ;; After the fix, diff section sets proper properties so inline API is used
        ;; But since we're simulating the broken state, it should use general API
        (should (member 'general-comment api-calls))
        (should-not (member 'inline-reply api-calls))))))

(provide 'test-regression-simple)
;;; test-regression-simple.el ends here