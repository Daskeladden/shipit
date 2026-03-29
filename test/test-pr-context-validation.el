;;; test-pr-context-validation.el --- Tests to prevent wrong PR API requests -*- lexical-binding: t; -*-

;; Tests to ensure functions only operate on displayed PR and never
;; accidentally send API requests using the wrong PR context

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load shipit modules
(require 'shipit-pr-sections)
(require 'shipit-http)
(require 'shipit-core)

;;; Code:

(defvar test-api-requests-made nil
  "List of API requests made during tests for validation.")

(defvar test-current-branch "feature/wrong-branch" 
  "Simulated current git branch - different from displayed PR.")

(defvar test-displayed-pr-number 123
  "The PR number that should be displayed and used.")

(defvar test-current-branch-pr-number 456
  "The PR number for current branch - should NOT be used.")

(defmacro test-with-pr-context (&rest body)
  "Execute BODY with mocked PR context setup."
  `(let ((test-api-requests-made nil)
         (shipit--current-displayed-pr nil))
     (cl-letf (((symbol-function 'magit-get-current-branch)
                (lambda () test-current-branch))
               ((symbol-function 'shipit--get-repo-from-remote) 
                (lambda () "test/repo"))
               
               ;; Mock PR data functions to track which PR number was requested
               ((symbol-function 'shipit-get-pull-request)
                (lambda (pr-number)
                  (push (list 'shipit-get-pull-request pr-number) test-api-requests-made)
                  (cond 
                   ((= pr-number test-displayed-pr-number)
                    `((number . ,test-displayed-pr-number) (title . "Displayed PR")))
                   ((= pr-number test-current-branch-pr-number) 
                    `((number . ,test-current-branch-pr-number) (title . "Current Branch PR")))
                   (t nil))))
               
               ((symbol-function 'shipit--get-current-pr-data)
                (lambda (branch repo)
                  (push (list 'shipit--get-current-pr-data branch repo) test-api-requests-made)
                  `((number . ,test-current-branch-pr-number) (title . "Wrong PR"))))
               
               ;; Mock API request functions to track usage
               ((symbol-function 'shipit--api-request)
                (lambda (endpoint &rest args)
                  (push (list 'shipit--api-request endpoint args) test-api-requests-made)
                  '((data . "mocked"))))
               
               ;; Mock review API functions
               ((symbol-function 'shipit-post-review)
                (lambda (pr-number event &optional body comments)
                  (push (list 'shipit-post-review pr-number event body comments) test-api-requests-made)
                  '((message . "success"))))
               
               ((symbol-function 'shipit-dismiss-review)
                (lambda (pr-number &optional message)
                  (push (list 'shipit-dismiss-review pr-number message) test-api-requests-made)
                  '((message . "success"))))
               
               ;; Mock inline comment functions
               ((symbol-function 'shipit--insert-inline-comments-for-file-revision)
                (lambda ()
                  (push (list 'inline-comments-for-file-revision-called) test-api-requests-made)))
               
               ((symbol-function 'shipit--insert-diff-hierarchical-comments)
                (lambda (pr-number repo comments)
                  (push (list 'inline-comments-inserted pr-number repo) test-api-requests-made)))
               
               ;; Mock SHA fetching
               ((symbol-function 'shipit--get-pr-head-sha)
                (lambda (pr-number)
                  (push (list 'shipit--get-pr-head-sha pr-number) test-api-requests-made)
                  (if (= pr-number test-displayed-pr-number)
                      "correct-sha-123"
                    "wrong-sha-456")))
               
               ;; Mock other required functions
               ((symbol-function 'shipit--comment-mini-editor)
                (lambda (pr repo)
                  (push (list 'comment-mini-editor-called 
                             (cdr (assq 'number pr)) repo) test-api-requests-made)))
               
               ((symbol-function 'shipit--simple-comment-dialog)
                (lambda (pr-number repo)
                  (push (list 'simple-comment-dialog-called pr-number repo) test-api-requests-made)))
               
               ;; Mock magit functions to prevent void function errors
               ((symbol-function 'magit-get-upstream-branch) (lambda (branch) nil))
               ((symbol-function 'magit-diff-setup-buffer) 
                (lambda (&rest args) 
                  (push (list 'magit-diff-setup-buffer args) test-api-requests-made)))
               ((symbol-function 'run-with-timer) 
                (lambda (delay repeat func) (funcall func)))
               ((symbol-function 'shipit--ref-exists-locally) (lambda (ref) t))
               ((symbol-function 'magit-current-section) (lambda () t))
               ((symbol-function 'derived-mode-p) (lambda (&rest modes) (memq 'magit-status-mode modes))))
       ,@body)))

(ert-deftest test-interactive-commenting-uses-displayed-pr-only ()
  "Test that interactive commenting only uses displayed PR, never current branch PR."
  (test-with-pr-context
    ;; Test without displayed PR - should get user-error
    (condition-case err
        (shipit--add-comment-interactive)
      (user-error
       (should (string-match-p "No PR currently displayed" (error-message-string err)))))
    
    ;; Set displayed PR and test again
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    (shipit--add-comment-interactive)
    
    ;; Verify correct PR was used
    (let ((pr-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit-get-pull-request)) 
                                         test-api-requests-made)))
      (should (= (length pr-requests) 1))
      (should (= (cadr (car pr-requests)) test-displayed-pr-number)))
    
    ;; Verify current branch PR was NEVER requested
    (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data)) 
                                             test-api-requests-made)))
      (should (= (length branch-requests) 0)))))

(ert-deftest test-dwim-actions-use-displayed-pr-only ()
  "Test that DWIM actions only operate on displayed PR."
  (test-with-pr-context
    ;; Set up displayed PR context
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    
    ;; Mock being in a magit status buffer with PR comment context
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        ;; Add text properties to simulate being on a comment line
        (insert "test content")
        (put-text-property (point-min) (point-max) 'shipit-line-comments t)
        (goto-char (point-min))
        
        ;; Call DWIM function
        (shipit-dwim)
        
        ;; Verify it used displayed PR number
        (let ((dialog-calls (cl-remove-if-not (lambda (req) (eq (car req) 'simple-comment-dialog-called))
                                             test-api-requests-made)))
          (should (= (length dialog-calls) 1))
          (should (= (cadr (car dialog-calls)) test-displayed-pr-number)))
        
        ;; Verify current branch PR was NOT requested
        (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data))
                                                 test-api-requests-made)))
          (should (= (length branch-requests) 0)))))))

(ert-deftest test-general-comments-refresh-uses-displayed-pr ()
  "Test that general comments refresh only works with displayed PR."
  (test-with-pr-context
    ;; Set up displayed PR context  
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    
    ;; Mock being in magit-status-mode and call refresh
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        (shipit--refresh-general-comments-section)
        
        ;; Verify it requested the displayed PR
        (let ((pr-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit-get-pull-request))
                                             test-api-requests-made)))
          (should (>= (length pr-requests) 1))
          (should (= (cadr (car pr-requests)) test-displayed-pr-number)))
        
        ;; Verify current branch PR was NOT requested
        (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data))
                                                 test-api-requests-made)))
          (should (= (length branch-requests) 0)))))))

(ert-deftest test-no-fallback-to-current-branch-pr ()
  "Test that functions never fall back to current branch PR - they error instead."
  (test-with-pr-context
    ;; All these functions should error rather than fall back to current branch PR
    (should-error (shipit--add-comment-interactive) :type 'user-error)
    
    ;; Test DWIM with line comments context but no displayed PR
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        (insert "test")
        (put-text-property (point-min) (point-max) 'shipit-line-comments t)
        (goto-char (point-min))
        (should-error (shipit-dwim) :type 'user-error)))
    
    ;; Verify that NO PR requests were made (no fallback occurred)
    (should (= (length test-api-requests-made) 0))))

(ert-deftest test-error-messages-are-clear ()
  "Test that error messages clearly indicate what the user should do."
  (test-with-pr-context
    ;; Test interactive commenting error message
    (condition-case err
        (shipit--add-comment-interactive)
      (user-error
       (should (string-match-p "No PR currently displayed.*Please view a PR first" 
                             (error-message-string err)))))
    
    ;; Test DWIM error message
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        (insert "test")
        (put-text-property (point-min) (point-max) 'shipit-line-comments t)
        (goto-char (point-min))
        (condition-case err
            (shipit-dwim)
          (user-error  
           (should (string-match-p "No PR currently displayed.*Please view a PR first"
                                 (error-message-string err)))))))))

(ert-deftest test-mini-editor-uses-displayed-pr-only ()
  "Test that the mini-editor (used for reviews) only uses displayed PR."
  (test-with-pr-context
    ;; Set up displayed PR context
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    
    ;; Direct test of shipit--comment-mini-editor function
    ;; This function currently calls shipit--get-current-pr-data (BUG!)
    (let ((pr-data `((number . ,test-displayed-pr-number) (title . "Test PR"))))
      (shipit--comment-mini-editor pr-data "test/repo"))
    
    ;; Verify that the mini-editor should NOT have called shipit--get-current-pr-data
    ;; This test will FAIL initially, proving the bug exists
    (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data))
                                             test-api-requests-made)))
      (should (= (length branch-requests) 0)))))

(ert-deftest test-inline-comments-use-displayed-pr-only ()
  "Test that inline comment operations only use displayed PR."
  (test-with-pr-context
    ;; Set up displayed PR context
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    
    ;; Mock revision mode for inline comments
    (with-temp-buffer
      (let ((major-mode 'magit-revision-mode)
            (magit-buffer-revision "abc123"))
        
        ;; Mock inline comment functionality
        (cl-letf (((symbol-function 'shipit--insert-diff-hierarchical-comments)
                   (lambda (pr-number repo comments)
                     (push (list 'inline-comments-inserted pr-number repo) test-api-requests-made))))
          
          ;; This should use displayed PR for inline comments
          (shipit--insert-inline-comments-for-file-revision))))
    
    ;; Verify inline comments used displayed PR
    (let ((inline-requests (cl-remove-if-not (lambda (req) (eq (car req) 'inline-comments-inserted))
                                            test-api-requests-made)))
      (should (> (length inline-requests) 0))
      (dolist (req inline-requests)
        (should (= (cadr req) test-displayed-pr-number))))
    
    ;; Verify NO current branch PR was used
    (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data))
                                             test-api-requests-made)))
      (should (= (length branch-requests) 0)))))

(ert-deftest test-cache-refresh-uses-displayed-pr-only ()
  "Test that cache refresh operations only target displayed PR."
  (test-with-pr-context
    ;; Set up displayed PR context
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    
    ;; Direct test of shipit--refresh-after-comment-operation
    ;; This function currently calls shipit--get-current-pr-data (BUG!)
    (shipit--refresh-after-comment-operation t)
    
    ;; Verify that cache refresh should NOT have called shipit--get-current-pr-data
    ;; This test will FAIL initially, proving the bug exists
    (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data))
                                             test-api-requests-made)))
      (should (= (length branch-requests) 0)))))

(ert-deftest test-sha-fetching-uses-displayed-pr-only ()
  "Test that SHA fetching operations use displayed PR only."
  (test-with-pr-context
    ;; Set up displayed PR context
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    
    ;; Test SHA fetching
    (let ((sha (shipit--get-pr-head-sha test-displayed-pr-number)))
      (should (string= sha "correct-sha-123")))
    
    ;; Verify that SHA fetching used displayed PR
    (let ((sha-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-pr-head-sha))
                                          test-api-requests-made)))
      (should (= (length sha-requests) 1))
      (should (= (cadr (car sha-requests)) test-displayed-pr-number)))
    
    ;; Verify NO current branch PR was used
    (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data))
                                             test-api-requests-made)))
      (should (= (length branch-requests) 0)))))

(ert-deftest test-api-request-validation-comprehensive ()
  "Comprehensive test ensuring no API requests use wrong PR context."
  (test-with-pr-context
    ;; Set up displayed PR context
    (setq shipit--current-displayed-pr (list test-displayed-pr-number "test/repo"))
    
    ;; Test multiple operations
    (shipit--add-comment-interactive)
    
    (with-temp-buffer
      (let ((major-mode 'magit-status-mode))
        (shipit--refresh-general-comments-section)))
    
    ;; Verify ALL PR requests used the correct displayed PR number
    (let ((pr-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit-get-pull-request))
                                         test-api-requests-made)))
      (dolist (req pr-requests)
        (should (= (cadr req) test-displayed-pr-number))))
    
    ;; Verify NO current branch PR requests were made
    (let ((branch-requests (cl-remove-if-not (lambda (req) (eq (car req) 'shipit--get-current-pr-data))
                                             test-api-requests-made)))
      (should (= (length branch-requests) 0)))
    
    ;; Verify no API requests contained the wrong PR number
    (let ((all-requests (mapcar #'cdr test-api-requests-made)))
      (dolist (req all-requests)
        (when (and (listp req) (numberp (car req)))
          (should-not (= (car req) test-current-branch-pr-number)))))))

(provide 'test-pr-context-validation)
;;; test-pr-context-validation.el ends here