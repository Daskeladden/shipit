;;; test-integration.el --- Integration tests for shipit -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests that verify cross-module dependencies and function existence
;; These tests use minimal mocking to catch missing implementations

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load external dependencies for integration testing
;; Integration tests MUST have all dependencies available
(require 'magit)    ; Fail if magit is not available
(require 'dash)     ; Fail if dash is not available

;; Load all shipit modules
(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-buffer)
(require 'shipit-diff)
(require 'shipit-pr-sections)
(require 'shipit-commands)
(require 'shipit-cache)
(require 'shipit-checks)
(require 'shipit-render)
(require 'shipit-actions)
(require 'shipit-issues-buffer)
(require 'shipit-editor)
;; Load backend modules so they register themselves
(require 'shipit-pr-github)
(require 'shipit-comment-github)
(require 'shipit-issue-github)
(require 'shipit-issue-jira)
(require 'shipit-issues)
(require 'shipit-gitlab-http)
(require 'shipit-notifications)
(require 'shipit-repo-buffer)

(ert-deftest test-required-dependencies-available ()
  "Verify that all required external dependencies are available."
  ;; Test that Magit is properly loaded and functional
  (should (featurep 'magit))
  (should (fboundp 'magit-get-current-branch))
  (should (fboundp 'magit-insert-section))
  (should (fboundp 'magit-insert-heading))
  (should (fboundp 'magit-insert-section-body))
  
  ;; Test that other core dependencies are available
  (should (featurep 'dash)))

(ert-deftest test-all-declared-functions-exist ()
  "Verify that all functions declared with declare-function actually exist."
  (let ((missing-functions '())
        (files '("lisp/shipit-core.el"
                 "lisp/shipit-http.el"
                 "lisp/shipit-buffer.el"
                 "lisp/shipit-diff.el"
                 "lisp/shipit-pr-sections.el"
                 "lisp/shipit-commands.el"
                 "lisp/shipit-cache.el"
                 "lisp/shipit-checks.el"
                 "lisp/shipit-render.el")))
    
    (dolist (file files)
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Find all declare-function statements
          (while (re-search-forward "(declare-function \\([^[:space:]]+\\)" nil t)
            (let ((func-name (match-string 1)))
              (unless (fboundp (intern func-name))
                (push (format "%s declared in %s" func-name file) missing-functions)))))))
    
    (when missing-functions
      (ert-fail (format "Missing declared functions:\n%s" 
                        (string-join missing-functions "\n"))))))

(ert-deftest test-comment-operations-integration ()
  "Integration test for comment add/edit/delete operations."
  ;; GIVEN mocked GitHub API layer and a displayed PR context
  ;; WHEN calling add/edit/delete/reply comment operations
  ;; THEN all operations succeed without error
  (let ((shipit-github-token "test-token")
        (shipit--current-displayed-pr '(1 "owner/repo")))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &optional method)
                 (cond
                  ;; add-inline-comment and reply POST to /comments
                  ((string-match "/comments$" endpoint)
                   '((id . 12345) (body . "test comment")
                     (user . ((login . "test"))) (created_at . "2025-01-01")))
                  ;; edit-comment PATCHes /comments/ID or /reviews/ID
                  ((or (string-match "/comments/[0-9]+$" endpoint)
                       (string-match "/reviews/[0-9]+$" endpoint))
                   '((id . 12345) (body . "updated")))
                  ;; delete returns success
                  (t '((success . t))))))
              ((symbol-function 'shipit--api-request)
               (lambda (endpoint &optional params)
                 '(((id . 123) (body . "test comment") (line . 10) (path . "test.el")))))
              ;; Mock head SHA lookup to avoid ETag HTTP chain
              ((symbol-function 'shipit--get-pr-head-sha)
               (lambda (_pr-number) "abc123"))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'magit-get-current-branch)
               (lambda () "test-branch"))
              ((symbol-function 'shipit--get-current-pr-data)
               (lambda (branch repo) '((number . 1) (head . ((sha . "abc123"))))))
              ((symbol-function 'magit-refresh)
               (lambda () t))
              ((symbol-function 'shipit--display-inline-comments)
               (lambda (&optional force) t))
              ;; Mock direct HTTP for delete-comment which uses url-retrieve-sync
              ((symbol-function 'shipit--url-retrieve-sync)
               (lambda (_url _method &optional _headers _data _raw) 204))
              ;; Mock PR fetch used by refresh-after-comment-operation
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_pr-number &optional _repo)
                 '((number . 1) (head . ((sha . "abc123")))))))

    ;; Test that all comment functions can be called without error
    (should (shipit--add-comment-to-pr 1 "test.el" 10 "test comment" "RIGHT"))
    (should (shipit--edit-comment 12345 "updated comment" t nil))
    (should (shipit--delete-comment 12345))
    (should (shipit--reply-to-inline-comment 1 12345 "reply"))
    (should (shipit--add-general-comment-to-pr 1 "general comment"))

    ;; Test refresh function
    (should (functionp 'shipit--refresh-after-comment-operation))
    (shipit--refresh-after-comment-operation t)   ; inline comment
    (shipit--refresh-after-comment-operation nil) ; general comment
    )))

(ert-deftest test-pr-data-integration ()
  "Integration test for PR data fetching and processing."
  ;; GIVEN mocked backend fetch-pr and a displayed PR context
  ;; WHEN extracting PR head SHA and fetching PR data
  ;; THEN both return the expected values
  (let ((shipit-github-token "test-token")
        (shipit--current-displayed-pr '(123 "owner/repo")))
    (cl-letf (;; Mock the GitHub PR backend's fetch-pr to avoid ETag HTTP chain
              ((symbol-function 'shipit-pr-github--fetch-pr)
               (lambda (_config _number)
                 '((number . 123) (title . "Test PR")
                   (head . ((sha . "abc123"))))))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'magit-get-current-branch)
               (lambda () "test-branch"))
              ((symbol-function 'shipit--get-current-pr-data)
               (lambda (_branch _repo)
                 '((number . 123) (title . "Test PR")
                   (head . ((sha . "abc123")))))))

      ;; Test PR head SHA extraction
      (should (string= (shipit--get-pr-head-sha 123) "abc123"))

      ;; Test PR data fetching
      (let ((pr-data (shipit--get-current-pr-data "test-branch" "owner/repo")))
        (should pr-data)
        (should (= (cdr (assq 'number pr-data)) 123))))))

(ert-deftest test-magit-integration-functions-exist ()
  "Verify all magit integration functions exist."
  (should (fboundp 'shipit--display-inline-comments))
  (should (fboundp 'shipit--refresh-after-comment-operation))
  (should (fboundp 'shipit--clear-inserted-comments)))

(ert-deftest test-http-functions-exist ()
  "Verify all HTTP API functions exist."
  (should (fboundp 'shipit--api-request))
  (should (fboundp 'shipit--api-request-post))
  (should (fboundp 'shipit--add-comment-to-pr))
  (should (fboundp 'shipit--edit-comment))
  (should (fboundp 'shipit--delete-comment))
  (should (fboundp 'shipit--reply-to-inline-comment))
  (should (fboundp 'shipit--add-general-comment-to-pr))
  (should (fboundp 'shipit--get-pr-head-sha)))

(ert-deftest test-diff-functions-exist ()
  "Verify all diff-related functions exist."
  (should (fboundp 'shipit--execute-action))
  (should (fboundp 'shipit--position-cursor-on-diff-line))
  (should (fboundp 'shipit--in-general-comments-section-p)))

(ert-deftest test-cache-integration ()
  "Test that cache clearing works across modules."
  ;; Set up some cached data
  (setq shipit--cached-inline-comments '(test-data))
  (setq shipit--inline-comments-fetched t)
  (setq shipit--cached-general-comments '(test-data))
  
  ;; Clear branch cache should clear everything
  (shipit--clear-branch-cache)
  
  ;; Verify caches are cleared
  (should (null shipit--cached-inline-comments))
  (should (null shipit--inline-comments-fetched))
  (should (null shipit--cached-general-comments)))

;;; Bug #3: url-retrieve-async redirect handling
;; url.el follows redirects automatically. The :redirect key in the status
;; plist is informational — the buffer contains the FINAL response.
;; Treating :redirect as an error rejects valid responses.

(ert-deftest test-url-retrieve-async-redirect-treated-as-success ()
  "GIVEN a URL that redirects (url.el follows it automatically)
WHEN shipit--url-retrieve-async receives the response with :redirect in status
THEN the success callback should be called (not the error callback)."
  (let ((success-called nil)
        (error-called nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _args)
                 ;; Simulate url.el calling our callback with :redirect status
                 ;; (url.el already followed the redirect; buffer has final response)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\n")
                   (insert "Content-Type: application/json\n\n")
                   (insert "{\"id\": 42}")
                   (funcall callback (list :redirect "https://new-url.example.com"))))))
      (shipit--url-retrieve-async
       "https://old-url.example.com/api"
       "GET"
       '(("Accept" . "application/json"))
       nil
       (lambda (data) (setq success-called data))
       (lambda (err) (setq error-called err)))
      ;; THEN success should be called with parsed JSON, not error
      (should success-called)
      (should-not error-called)
      (should (equal 42 (cdr (assq 'id success-called)))))))

;;; Bug #4: General comment reply uses wrong endpoint
;; General comments live at /issues/{N}/comments.
;; Replies to general comments should create new issue comments,
;; not pull request review comments at /pulls/{N}/comments.

(ert-deftest test-general-comment-reply-uses-issues-endpoint ()
  "GIVEN a general comment on a PR
WHEN replying to the general comment via the GitHub backend
THEN the reply should be posted to /issues/ endpoint (not /pulls/)."
  (let ((shipit-github-token "test-token")
        (shipit--current-displayed-pr '(42 "owner/repo"))
        (captured-endpoint nil)
        (shipit--cached-general-comments
         '(((id . 100) (body . "parent comment") (user . ((login . "test")))))))
    (cl-letf (((symbol-function 'shipit-comment--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :reply-to-comment
                             (lambda (_config _number _parent-id _body &optional _is-inline)
                               ;; This is the mock — we check what endpoint would be used
                               ;; by checking what shipit-comment-github--reply-to-comment does
                               '((id . 200) (body . "reply")))
                             :add-general-comment
                             (lambda (_config _number _body)
                               '((id . 200) (body . "reply"))))
                       '(:repo "owner/repo"))))
              ((symbol-function 'shipit--api-request-post)
               (lambda (endpoint _data &optional _method)
                 (setq captured-endpoint endpoint)
                 '((id . 200) (body . "reply"))))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit--refresh-after-comment-operation) #'ignore)
              ((symbol-function 'shipit--debug-log) #'ignore)
              ((symbol-function 'shipit--lookup-discussion-id)
               (lambda (id) id)))
      ;; Call the backend function directly to check the endpoint
      (shipit-comment-github--reply-to-comment
       '(:repo "owner/repo") 42 100 "my reply")
      ;; THEN the endpoint should be /issues/ for general comment replies
      (should captured-endpoint)
      (should (string-match-p "/issues/" captured-endpoint)))))

;;; Bug #1 + #6: Jira notification mark-read via local tracking

(ert-deftest test-jira-notification-mark-read-local-tracking ()
  "GIVEN a Jira backend notification (no :mark-notification-read operation)
WHEN shipit--mark-notification-read is called
THEN the notification is tracked locally and filtered from subsequent polls."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--mention-prs nil)
        (shipit--last-notification-count 1)
        (shipit-github-token "test-token"))
    ;; GIVEN a Jira notification in the activities hash
    (let ((activity '((number . 42)
                      (type . "issue")
                      (repo . "owner/repo")
                      (subject . "Test Jira issue")
                      (reason . "assigned")
                      (backend-id . jira))))
      (puthash "owner/repo:issue:42" activity shipit--notification-pr-activities)
      (cl-letf (((symbol-function 'shipit--resolve-backend-by-id)
                 (lambda (_backend-id _activity)
                   ;; Return a Jira backend without :mark-notification-read
                   (cons (list :name "Jira") '(:repo "owner/repo"))))
                ((symbol-function 'shipit--update-modeline-indicator) #'ignore)
                ((symbol-function 'shipit--clear-notifications-cache) #'ignore)
                ((symbol-function 'shipit--debug-log) #'ignore))
        ;; WHEN marking the notification as read
        (shipit--mark-notification-read 42 "owner/repo" t "issue")
        ;; THEN it should be removed from activities
        (should (= 0 (hash-table-count shipit--notification-pr-activities)))
        ;; AND tracked in locally-marked-read
        (should (gethash "owner/repo:issue:42" shipit--locally-marked-read-notifications))))))

;;; Bug #2: fetch-checks fetches both check-runs and statuses independently

(ert-deftest test-fetch-checks-independent-check-runs-and-statuses ()
  "GIVEN check-runs endpoint fails but statuses endpoint succeeds
WHEN shipit-pr-github--fetch-checks is called
THEN statuses are still returned (not silently empty)."
  (let ((shipit-github-token "test-token"))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &rest _args)
                 ;; check-runs endpoint fails
                 (error "API error")))
              ((symbol-function 'shipit--api-request-paginated)
               (lambda (_endpoint)
                 ;; statuses endpoint succeeds
                 '(((state . "success") (context . "ci/build")))))
              ((symbol-function 'shipit--debug-log) #'ignore))
      (let ((result (shipit-pr-github--fetch-checks '(:repo "owner/repo") "abc123")))
        ;; THEN statuses should still be returned
        (should result)
        (should (= 1 (length result)))
        (should (equal "success" (cdr (assq 'state (car result)))))))))

;;; Bug #5: Async 304 recursive call preserves callback

(ert-deftest test-async-304-recursive-force-fresh-preserves-callback ()
  "GIVEN an async ETag request receives 304 but cache has no JSON
WHEN the function makes a recursive force-fresh call
THEN the original callback is passed through."
  (let ((shipit-github-token "test-token")
        (shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (recursive-call-args nil))
    ;; Pre-populate cache with an entry that has NO :json
    (puthash "/repos/owner/repo/pulls" (list :etag "\"abc\"") shipit-gh-etag--persistent-cache)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _args)
                 ;; First call: simulate 304 response
                 (with-temp-buffer
                   (insert "HTTP/1.1 304 Not Modified\n\n")
                   (funcall callback nil))))
              ((symbol-function 'shipit-gh-etag--ensure-cache-loaded) #'ignore)
              ((symbol-function 'shipit-gh-etag--proactive-cache-cleanup) #'ignore)
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; Intercept the recursive call
      (let ((original-fn (symbol-function 'shipit-gh-etag-get-json-async)))
        (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
                   (lambda (endpoint params token callback &optional force-fresh)
                     (if force-fresh
                         ;; Capture the recursive call args
                         (setq recursive-call-args
                               (list :endpoint endpoint :callback callback :force-fresh force-fresh))
                       ;; First call — use original function
                       (funcall original-fn endpoint params token callback force-fresh)))))
          (shipit-gh-etag-get-json-async
           "/repos/owner/repo/pulls" nil nil #'my-callback)))
      ;; THEN the recursive call should have the original callback
      (should recursive-call-args)
      (should (plist-get recursive-call-args :force-fresh))
      (should (eq #'my-callback (plist-get recursive-call-args :callback))))))

;;; Bug #7: create-issue-extended fallback drops fields

(ert-deftest test-create-issue-extended-fallback-uses-title-and-body ()
  "GIVEN a backend without :create-issue-extended
WHEN shipit-issues--create-issue-extended is called with labels and assignees
THEN falls back to :create-issue with title and body only."
  (let ((create-called-with nil))
    (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 ;; Backend with :create-issue but NO :create-issue-extended
                 (cons (list :create-issue
                             (lambda (_config title body)
                               (setq create-called-with (list :title title :body body))
                               '((id . 1) (title . "Test"))))
                       '(:repo "owner/repo"))))
              ((symbol-function 'shipit--debug-log) #'ignore))
      (shipit-issues--create-issue-extended
       "owner/repo"
       '((title . "Test Issue")
         (body . "Description")
         (labels . ("bug" "urgent"))
         (assignees . ("user1"))))
      ;; THEN :create-issue was called with title and body
      (should create-called-with)
      (should (equal "Test Issue" (plist-get create-called-with :title)))
      (should (equal "Description" (plist-get create-called-with :body))))))

(provide 'test-integration)
;;; test-integration.el ends here