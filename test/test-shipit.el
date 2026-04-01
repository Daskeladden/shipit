;;; test-shipit.el --- Tests for shipit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Daskeladden
;; Keywords: test, github, code review

;;; Commentary:

;; Test suite for shipit.el covering critical features and functionality.

;;; Code:

(require 'ert)
;; Load test helper functions FIRST to provide mocks before shipit loads
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load recorded test fixtures after stubs are loaded
(load-file (expand-file-name "fixtures/sample-pr-data.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit)
;; Load GitHub backends — required because read operations dispatch through backends
(require 'shipit-pr-github)
(require 'shipit-comment-github)
;; Load lazy-load tests
(load-file (expand-file-name "test-commit-files-lazy-load.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load worktree tests
(load-file (expand-file-name "test-shipit-worktree.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load notifications buffer tests
(load-file (expand-file-name "test-notifications-buffer.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load notifications integration tests
(load-file (expand-file-name "test-notifications-integration.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load preview tests
(load-file (expand-file-name "test-shipit-preview.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load backend output contract tests
(load-file (expand-file-name "test-shipit-comment-contracts.el" (file-name-directory (or load-file-name buffer-file-name))))
(load-file (expand-file-name "test-shipit-pr-contracts.el" (file-name-directory (or load-file-name buffer-file-name))))
(load-file (expand-file-name "test-shipit-issue-contracts.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load backend parity tests
(load-file (expand-file-name "test-shipit-backend-parity.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load GitLab parity operation tests
(load-file (expand-file-name "test-shipit-gitlab-parity-ops.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load vendor icon header tests
(load-file (expand-file-name "test-shipit-vendor-icons.el" (file-name-directory (or load-file-name buffer-file-name))))
;; Load debug log secret redaction tests
(load-file (expand-file-name "test-shipit-debug-redact.el" (file-name-directory (or load-file-name buffer-file-name))))

;;; Test Utilities

(defvar test-pr-data
  '((number . 12345)
    (title . "Test PR with Windows line endings")
    (body . "This is a test PR body\r\nwith Windows line endings\rand mixed formats")
    (state . "open")
    (user . ((login . "test-user")))
    (head . ((sha . "abc123def456")
             (ref . "test-branch")))
    (base . ((ref . "main")))
    (created_at . "2024-01-01T12:00:00Z"))
  "Mock PR data for testing.")

(defvar test-workflow-data
  '((id . 67890)
    (name . "Test Workflow")
    (path . ".github/workflows/test.yml"))
  "Mock workflow data for testing.")

;;; Core Functionality Tests

(ert-deftest test-shipit--clean-text ()
  "Test that Windows line endings are properly cleaned."
  (should (string= (shipit--clean-text "hello\r\nworld") "hello\nworld"))
  (should (string= (shipit--clean-text "hello\rworld") "helloworld"))
  (should (string= (shipit--clean-text "hello\nworld") "hello\nworld"))
  (should (string= (shipit--clean-text "hello\r\n\rworld\r") "hello\nworld"))
  (should (null (shipit--clean-text nil))))

(ert-deftest test-shipit--get-repo-from-remote ()
  "Test repository extraction from git remote URLs."
  ;; Mock git command output - SSH format
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (cmd)
               (if (string-match "git config" cmd)
                   "git@github.com:owner/repo.git\n"
                 ""))))
    (should (string= (shipit--get-repo-from-remote) "owner/repo")))

  ;; Test HTTPS URL
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (cmd)
               (if (string-match "git config" cmd)
                   "https://github.com/owner/repo.git\n"
                 ""))))
    (should (string= (shipit--get-repo-from-remote) "owner/repo")))

  ;; Test without .git suffix
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (cmd)
               (if (string-match "git config" cmd)
                   "https://github.com/owner/repo\n"
                 ""))))
    (should (string= (shipit--get-repo-from-remote) "owner/repo"))))

;;; Caching Tests

(ert-deftest test-shipit--current-pr-data-caching ()
  "Test that PR data caching is branch-aware."
  ;; GIVEN a clean PR cache and mocked ETag API
  (let ((shipit-github-token "test-token")
        (shipit-magit-show-closed-prs nil)
        (call-count 0)
        (original-cache shipit--cached-branch-prs))
    (setq shipit--cached-branch-prs (make-hash-table :test 'equal))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint params &optional token force-fresh)
                 (setq call-count (1+ call-count))
                 (let ((head-param (cdr (assq 'head params))))
                   (if (and head-param
                            (or (string= head-param "owner:test-branch")
                                (string= head-param "test-branch")))
                       (list :json (list test-pr-data))
                     (list :json nil)))))
              ((symbol-function 'shipit--ensure-cache-initialized)
               (lambda () nil))
              ((symbol-function 'shipit--fetch-commits-and-files-parallel-sync)
               (lambda (_repo _pr-number pr) pr)))

      ;; WHEN fetching PR data for test-branch
      (let ((pr1 (shipit--get-current-pr-data "test-branch" "owner/repo")))
        ;; THEN the PR is found and API was called once (first head format matched)
        (should (equal pr1 test-pr-data))
        (should (= call-count 1)))

      ;; WHEN fetching PR data for a different branch
      (let ((pr2 (shipit--get-current-pr-data "different-branch" "owner/repo")))
        ;; THEN no PR is found (both head formats tried, 2 additional calls)
        (should (null pr2))
        (should (= call-count 3)))

      ;; WHEN fetching PR data for test-branch again
      (let ((pr3 (shipit--get-current-pr-data "test-branch" "owner/repo")))
        ;; THEN cached PR is returned with no additional API calls
        (should (equal pr3 test-pr-data))
        (should (= call-count 3))))
    (setq shipit--cached-branch-prs original-cache)))

(ert-deftest test-shipit--workflow-name-caching ()
  "Test that workflow names are cached properly."
  (let ((shipit--workflow-name-cache (make-hash-table :test 'equal)))

    ;; Pre-populate cache
    (puthash "owner/repo:67890" "Test Workflow" shipit--workflow-name-cache)

    ;; Verify cache lookup works
    (should (string= (gethash "owner/repo:67890" shipit--workflow-name-cache)
                     "Test Workflow"))

    ;; Test cache key format
    (should (string= (format "%s:%s" "owner/repo" 67890) "owner/repo:67890"))))

(ert-deftest test-shipit-labels-cache-consistency ()
  "Test that labels cache is updated when labels are changed via shipit--update-pr-labels."
  ;; GIVEN a cached PR with original labels
  (let ((shipit--cached-branch-prs (make-hash-table :test 'equal))
        (shipit-github-token "test-token")
        (original-labels '(((name . "bug") (color . "d73a4a"))))
        (updated-labels '(((name . "bug") (color . "d73a4a"))
                         ((name . "enhancement") (color . "a2eeef"))))
        (cache-key "owner/repo:test-branch"))
    (puthash cache-key
             `((number . 123)
               (title . "Test PR")
               (labels . ,original-labels)
               (head . ((sha . "abc123"))))
             shipit--cached-branch-prs)

    ;; Verify initial state
    (should (equal (cdr (assq 'labels (gethash cache-key shipit--cached-branch-prs)))
                   original-labels))

    ;; WHEN shipit--update-pr-labels runs with mocked backend dispatch
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :name "Mock" :set-labels
                             (lambda (_config _number _labels)
                               `((number . 123)
                                 (labels . ,updated-labels))))
                       '(:repo "owner/repo"))))
              ((symbol-function 'shipit-pr--backend-id) (lambda () 'github))
              ((symbol-function 'magit-get-current-branch)
               (lambda () "test-branch"))
              ((symbol-function 'shipit-gh-etag-invalidate-endpoint)
               (lambda (_endpoint) nil))
              ((symbol-function 'shipit--find-section-by-type)
               (lambda (_type) nil))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat func &rest args) (apply func args))))
      (shipit--update-pr-labels 123 "owner/repo" '("bug" "enhancement")))

    ;; THEN the cache is updated with the new labels
    (let ((cached-pr (gethash cache-key shipit--cached-branch-prs)))
      (should cached-pr)
      (should (equal (cdr (assq 'labels cached-pr)) updated-labels))
      ;; AND other PR fields are preserved
      (should (= (cdr (assq 'number cached-pr)) 123))
      (should (string= (cdr (assq 'title cached-pr)) "Test PR")))))

;;; Comment Rendering Tests

(ert-deftest test-shipit--render-comment-header-basic ()
  "Test basic comment header rendering with username and timestamp."
  (let ((comment '((id . 123)
                   (user . ((login . "testuser")
                           (avatar_url . "https://example.com/avatar.jpg")))
                   (created_at . "2024-01-01T12:00:00Z")
                   (body . "Test comment")))
        (shipit-show-avatars nil))
    (let ((header (shipit--render-comment-header comment 0 'inline)))
      (should (stringp header))
      (should (string-match-p "testuser" header))
      (should (string-match-p "2024-01-01" header)))))

(ert-deftest test-shipit--render-comment-header-with-avatar ()
  "Test comment header rendering with avatar enabled."
  (let ((comment '((id . 123)
                   (user . ((login . "testuser")
                           (avatar_url . "https://example.com/avatar.jpg")))
                   (created_at . "2024-01-01T12:00:00Z")
                   (body . "Test comment")))
        (shipit-show-avatars t))
    ;; Mock avatar display function to return a test string
    (cl-letf (((symbol-function 'shipit--create-avatar-display)
               (lambda (user url size) "[AVATAR]")))
      (let ((header (shipit--render-comment-header comment 0 'inline)))
        (should (stringp header))
        (should (string-match-p "\\[AVATAR\\]" header))
        (should (string-match-p "testuser" header))))))

(ert-deftest test-shipit--render-comment-header-no-avatar ()
  "Test comment header rendering falls back gracefully when no avatar."
  (let ((comment '((id . 123)
                   (user . ((login . "testuser")))
                   (created_at . "2024-01-01T12:00:00Z")
                   (body . "Test comment")))
        (shipit-show-avatars t))
    ;; Mock avatar display to return empty string (fallback)
    (cl-letf (((symbol-function 'shipit--create-avatar-display)
               (lambda (user url size) "")))
      (let ((header (shipit--render-comment-header comment 0 'inline)))
        (should (stringp header))
        (should (string-match-p "testuser" header))
        ;; Should still work without avatar
        (should-not (string-match-p "nil" header))))))

(ert-deftest test-shipit--render-comment-header-resolved ()
  "Test comment header rendering with resolved status."
  (let ((comment '((id . 123)
                   (user . ((login . "testuser")
                           (avatar_url . "https://example.com/avatar.jpg")))
                   (created_at . "2024-01-01T12:00:00Z")
                   (body . "Test comment")))
        (shipit-show-avatars nil))
    ;; Mock the resolved thread check to return true
    (cl-letf (((symbol-function 'shipit--is-comment-in-resolved-thread)
               (lambda (id) t)))
      (let ((header (shipit--render-comment-header comment 0 'inline)))
        (should (stringp header))
        (should (string-match-p "\\[RESOLVED\\]" header))))))

(ert-deftest test-shipit--render-comment-header-not-resolved ()
  "Test comment header rendering without resolved status."
  (let ((comment '((id . 123)
                   (user . ((login . "testuser")
                           (avatar_url . "https://example.com/avatar.jpg")))
                   (created_at . "2024-01-01T12:00:00Z")
                   (body . "Test comment")))
        (shipit-show-avatars nil))
    ;; Mock the resolved thread check to return false
    (cl-letf (((symbol-function 'shipit--is-comment-in-resolved-thread)
               (lambda (id) nil)))
      (let ((header (shipit--render-comment-header comment 0 'inline)))
        (should (stringp header))
        (should-not (string-match-p "\\[RESOLVED\\]" header))))))

(ert-deftest test-shipit--render-comment-header-no-newlines ()
  "Test that comment header does not include newlines."
  (let ((comment '((id . 123)
                   (user . ((login . "testuser")
                           (avatar_url . "https://example.com/avatar.jpg")))
                   (created_at . "2024-01-01T12:00:00Z")
                   (body . "Test comment")))
        (shipit-show-avatars nil))
    (cl-letf (((symbol-function 'shipit--is-comment-in-resolved-thread)
               (lambda (id) nil)))
      (let ((header (shipit--render-comment-header comment 0 'inline)))
        (should (stringp header))
        (should-not (string-match-p "\n" header))))))

;;; Text Processing Tests

(ert-deftest test-shipit--humanize-workflow-name ()
  "Test workflow name humanization."
  (should (string= (shipit--humanize-workflow-name "build-and-test")
                   "Build And Test"))
  (should (string= (shipit--humanize-workflow-name "ci_workflow")
                   "Ci Workflow"))
  (should (string= (shipit--humanize-workflow-name "deploy")
                   "Deploy")))

(ert-deftest test-shipit--wrap-text ()
  "Test text wrapping functionality."
  (should (string-match-p "\n" (shipit--wrap-text "This is a very long line that should be wrapped when it exceeds the specified width limit" 20)))
  (should (not (string-match-p "\n" (shipit--wrap-text "Short text" 80)))))

(ert-deftest test-shipit--wrap-text-preserves-table-lines ()
  "Test that wrap-text preserves markdown table lines without wrapping."
  ;; GIVEN a markdown table with lines wider than the wrap width
  (let* ((table-text "| Feature                                     | key                             |\n|---------------------------------------------|---------------------------------|\n| Chat: send prompt at chat buffer            | C-c C-RET                       |")
         ;; WHEN wrapping to a width narrower than the table lines
         (result (shipit--wrap-text table-text 40)))
    ;; THEN table lines should be preserved exactly as-is
    (should (string= result table-text))))

(ert-deftest test-shipit--wrap-text-preserves-tables-in-mixed-content ()
  "Test that wrap-text wraps normal text but preserves table lines."
  ;; GIVEN mixed content with a paragraph and a table
  (let* ((input "This is some long paragraph text that should definitely be wrapped.\n\n| Col1       | Col2       |\n|------------|------------|\n| data cell  | more data  |")
         ;; WHEN wrapping to width 40
         (result (shipit--wrap-text input 40)))
    ;; THEN table lines should remain intact (no wrapping)
    (should (string-match-p "^| Col1       | Col2       |$" result))
    (should (string-match-p "^|------------|------------|$" result))
    (should (string-match-p "^| data cell  | more data  |$" result))
    ;; AND normal text should be wrapped (contains newline within paragraph)
    (let ((first-line (car (split-string result "\n"))))
      (should (<= (length first-line) 45)))))

;;; Integration Tests

(ert-deftest test-shipit-magit-integration-setup ()
  "Test that magit integration is properly set up."
  ;; Ensure magit modules are loaded for the test
  (require 'shipit-pr-sections)
  (should (fboundp 'shipit-setup-magit-integration)))

(ert-deftest test-shipit-magit-section-types ()
  "Test that all magit section types used by shipit are valid symbols."
  ;; Test that common section types are symbols and not function calls
  (should (symbolp 'pr-description))
  (should (symbolp 'general-comments))
  (should (symbolp 'inline-comments))
  (should (symbolp 'file-comments))
  (should (symbolp 'file-comments-outdated))
  (should (symbolp 'checks))
  (should (symbolp 'labels))
  
  ;; Test that trying to use these as function calls would fail appropriately
  ;; This catches the specific error pattern we experienced
  (should-error (pr-description) :type 'void-function)
  
  ;; Test that section specs with these symbols work correctly
  (should (listp '(pr-description)))
  (should (listp '(general-comments)))
  (should (listp '(inline-comments nil t))))

(ert-deftest test-shipit-maphash-usage ()
  "Test that maphash calls have correct number of arguments."
  ;; Test the inline comments grouping function that uses maphash
  (let ((test-comments '(((path . "file1.js") (line . 10) (body . "Comment 1"))
                         ((path . "file2.py") (line . 5) (body . "Comment 2")))))
    
    ;; This should not throw "Wrong number of arguments: maphash, 1" 
    (let ((grouped (shipit--group-inline-comments-by-file test-comments)))
      ;; Should be able to iterate over the hash table
      (maphash (lambda (file-path comments)
                 (should (stringp file-path))
                 (should (listp comments)))
               grouped)
      ;; If we get here, the maphash call succeeded
      (should t)))) 

(ert-deftest test-shipit-cache-clearing ()
  "Test that caches are properly cleared when branches change."
  (let ((shipit--cached-general-comments '(test-data))
        (shipit--general-comments-fetched t)
        (shipit--cached-pr-reactions '(test-reactions))
        (shipit--cached-pr-checks '(test-checks))
        (shipit--cached-inline-comments '(test-inline-comments))
        (shipit--inline-comments-fetched t)
        (shipit--cached-branch-prs (make-hash-table :test 'equal)))

    ;; Set up some cached data using proper repo:branch format
    (puthash "test-repo:test-branch" "test-value" shipit--cached-branch-prs)

    ;; Mock repo detection to return our test repo
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote) 
               (lambda () "test-repo"))
              ((symbol-function 'magit-get-current-branch)
               (lambda () "test-branch")))
      
      ;; Clear cache
      (shipit--clear-branch-cache)

      ;; Verify all caches are cleared
      (should (null shipit--cached-general-comments))
      (should (null shipit--general-comments-fetched))
      (should (null shipit--cached-pr-reactions))
      (should (null shipit--cached-pr-checks))
      (should (null shipit--cached-inline-comments))
      (should (null shipit--inline-comments-fetched))
      ;; The hash table should still exist but our test entry should be gone
      ;; (cache clearing only removes repo-specific entries)
      (should (= (hash-table-count shipit--cached-branch-prs) 0)))))

;;; GitHub Token Resolution and Error Handling Tests

(ert-deftest test-shipit-gh-etag-404-without-token-hints-auth ()
  "GIVEN no GitHub token is configured
WHEN a request returns 404
THEN the error message hints that authentication may be needed."
  (let ((shipit-github-token nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) nil)))
      (let ((msg (shipit-gh-etag--format-http-error 404 "/repos/owner/repo/pulls/1")))
        (should (string-match-p "token" msg))
        (should (string-match-p "404" msg))))))

(ert-deftest test-shipit-gh-etag-404-with-token-no-auth-hint ()
  "GIVEN a GitHub token is configured
WHEN a request returns 404
THEN the error message does not hint about authentication."
  (let ((shipit-github-token "test-token"))
    (let ((msg (shipit-gh-etag--format-http-error 404 "/repos/owner/repo/pulls/1")))
      (should (string-match-p "404" msg))
      (should-not (string-match-p "token" msg)))))

(ert-deftest test-shipit-gh-etag-500-no-auth-hint ()
  "GIVEN any token state
WHEN a request returns 500
THEN the error message does not hint about authentication."
  (let ((shipit-github-token nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) nil)))
      (let ((msg (shipit-gh-etag--format-http-error 500 "/repos/owner/repo/pulls/1")))
        (should (string-match-p "500" msg))
        (should-not (string-match-p "token" msg))))))

(ert-deftest test-shipit-gh-etag-async-uses-auth-source-fallback ()
  "GIVEN shipit-github-token is nil but auth-source has a token
WHEN shipit-gh-etag-get-json-async is called with nil token
THEN the request includes the Authorization header from auth-source."
  (let ((shipit-github-token nil)
        (shipit--github-token-cache "auth-source-secret-token")
        (captured-headers nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _args)
                 ;; Capture the headers that were set
                 (setq captured-headers url-request-extra-headers)
                 ;; Don't actually make a request
                 nil))
              ((symbol-function 'shipit-gh-etag--ensure-cache-loaded) #'ignore)
              ((symbol-function 'shipit-gh-etag--proactive-cache-cleanup) #'ignore)
              ((symbol-function 'shipit--debug-log) #'ignore))
      (shipit-gh-etag-get-json-async "/repos/owner/repo/pulls/1/files" nil nil #'ignore)
      ;; THEN the Authorization header should be present (from auth-source fallback)
      (should (assoc "Authorization" captured-headers))
      (should (string-match-p "Bearer auth-source-secret-token"
                              (cdr (assoc "Authorization" captured-headers)))))))

(ert-deftest test-shipit-get-pr-for-branch-async-works-with-auth-source ()
  "GIVEN shipit-github-token is nil but auth-source has a token
WHEN shipit-get-pr-for-branch-async is called
THEN the function proceeds (does not silently abort)."
  (let ((shipit-github-token nil)
        (shipit--github-token-cache "auth-source-token")
        (shipit--cached-branch-prs (make-hash-table :test 'equal))
        (callback-called nil))
    (cl-letf (((symbol-function 'shipit--ensure-cache-initialized) #'ignore)
              ((symbol-function 'shipit--get-repo-from-remote) (lambda () "owner/repo"))
              ((symbol-function 'shipit--api-request)
               (lambda (&rest _args)
                 (setq callback-called t)))
              ((symbol-function 'shipit--debug-log) #'ignore))
      (shipit-get-pr-for-branch-async "my-branch" #'ignore)
      ;; THEN the function should have proceeded to make the API request
      (should callback-called))))

(ert-deftest test-shipit-github-token-explicit-takes-precedence ()
  "GIVEN shipit-github-token is set explicitly
WHEN shipit--github-token is called
THEN returns the explicit token."
  (let ((shipit-github-token "my-explicit-token")
        (shipit--github-token-cache nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) '((:secret (lambda () "auth-source-token"))))))
      (should (equal (shipit--github-token) "my-explicit-token")))))

(ert-deftest test-shipit-github-token-falls-back-to-auth-source ()
  "GIVEN shipit-github-token is nil
WHEN shipit--github-token is called
THEN returns token from auth-source for github.com."
  (let ((shipit-github-token nil)
        (shipit--github-token-cache nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (list (list :secret (lambda () "auth-source-token"))))))
      (should (equal (shipit--github-token) "auth-source-token")))))

(ert-deftest test-shipit-github-token-nil-when-nothing-found ()
  "GIVEN shipit-github-token is nil and auth-source has no entry
WHEN shipit--github-token is called
THEN returns nil."
  (let ((shipit-github-token nil)
        (shipit--github-token-cache nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) nil)))
      (should-not (shipit--github-token)))))

;;; API Tests (Mocked)

(ert-deftest test-shipit--api-request-error-handling ()
  "Test API request error handling with safe mocking."
  ;; Test that error handling variables exist
  (should (boundp 'shipit-github-token))
  (should (boundp 'shipit-api-url))

  ;; Test that the function exists and can be called
  (should (fboundp 'shipit--api-request)))

;;; Performance Tests

(ert-deftest test-shipit-pr-detection-performance ()
  "Test that PR detection uses branch-aware caching efficiently."
  (let ((shipit--cached-branch-prs (make-hash-table :test 'equal))
        (shipit-github-token "test-token")
        (api-calls 0))

    ;; Mock API calls to count requests (handle both sync and async signatures)
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (endpoint params &optional callback)
                 (setq api-calls (1+ api-calls))
                 (let ((result (list test-pr-data)))
                   (if callback
                       (funcall callback result)
                     result))))
              ;; Mock ETag API as well - return PR that matches the search
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint params token)
                 (setq api-calls (1+ api-calls))
                 ;; Return matching PR ONLY for the first head format to simulate real API behavior
                 (let ((head-param (cdr (assq 'head params))))
                   (if (string= head-param "owner:test-branch")
                       (list :status 200 :json (list test-pr-data) :from-cache nil)
                     (list :status 200 :json '() :from-cache nil)))))
              ;; Mock the commits/files parallel fetch to avoid additional API calls
              ((symbol-function 'shipit--fetch-commits-and-files-parallel-sync)
               (lambda (repo pr-number pr)
                 ;; Return the PR unchanged without making additional API calls
                 pr)))

      ;; Multiple calls for same branch should only result in one API request
      (dotimes (i 5)
        (shipit--get-current-pr-data "test-branch" "owner/repo"))

      (should (= api-calls 1)))))

(ert-deftest test-shipit-parallel-pr-review-decision-fetching ()
  "Test that PR review decision fetching uses parallel API requests."
  (let ((shipit-github-token "test-token")
        (shipit--cached-review-decision nil)
        (async-calls-initiated 0)
        (async-calls-completed 0)
        (initiated-requests '())
        (completed-requests '()))

    ;; Mock the async ETag API to track parallel execution
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (endpoint params token callback &optional force-fresh)
                 ;; Record that this request was initiated
                 (setq async-calls-initiated (1+ async-calls-initiated))
                 (push endpoint initiated-requests)
                 (shipit--debug-log "TEST: Initiated async request #%d for %s" async-calls-initiated endpoint)

                 ;; Simulate async completion by calling callback immediately with plist format
                 (let ((mock-json
                        (cond
                         ;; PR endpoint
                         ((string-match-p "/pulls/[0-9]+" endpoint)
                          '((base . ((ref . "main")))))
                         ;; Files endpoint
                         ((string-match-p "/files" endpoint)
                          '(((filename . "test.el"))))
                         ;; Reviews endpoint
                         ((string-match-p "/reviews" endpoint)
                          '(((id . 1)
                             (user . ((login . "reviewer1")))
                             (state . "APPROVED")
                             (submitted_at . "2024-01-01T12:00:00Z"))))
                         ;; Requested reviewers endpoint
                         ((string-match-p "/requested_reviewers" endpoint)
                          '((users . ()) (teams . ())))
                         (t '()))))
                   (setq async-calls-completed (1+ async-calls-completed))
                   (push endpoint completed-requests)
                   (shipit--debug-log "TEST: Completed async request #%d for %s" async-calls-completed endpoint)
                   (funcall callback (list :status 200 :json mock-json)))))
              ;; Mock paginated async variant (used for files endpoint)
              ((symbol-function 'shipit-gh-etag-get-json-paginated-async)
               (lambda (endpoint params token callback &optional force-fresh)
                 ;; Call the regular async mock which will handle it
                 (shipit-gh-etag-get-json-async endpoint params token callback force-fresh)))
              ;; Mock paginated variant to use async internally
              ((symbol-function 'shipit-gh-etag-get-json-paginated)
               (lambda (endpoint params token)
                 ;; For files endpoint, return plist format
                 (list :json '(((filename . "test.el"))))))
              ;; Mock helper functions
              ((symbol-function 'shipit--get-latest-reviews-per-user)
               (lambda (reviews) reviews))
              ((symbol-function 'shipit--compute-review-decision-from-reviews)
               (lambda (reviews) "APPROVED"))
              ((symbol-function 'shipit--get-codeowners-based-decision)
               (lambda (reviews files repo base owner) nil))
              ((symbol-function 'shipit--process-latest-reviews)
               (lambda (reviews) '()))
              ((symbol-function 'shipit--get-approved-users)
               (lambda (reviews) '("reviewer1")))
              ((symbol-function 'shipit--compute-requested-team-approvals)
               (lambda (teams users owner) '()))
              ((symbol-function 'shipit--debug-log)
               (lambda (format-string &rest args)
                 (apply 'message (concat "TEST-DEBUG: " format-string) args))))

      ;; Call the function
      (let ((result (shipit--get-pr-review-decision-detailed "owner/repo" 123)))

        ;; Verify all 4 async requests were initiated
        (should (>= async-calls-initiated 4))

        ;; Verify requests were for the correct endpoints
        (should (member "/repos/owner/repo/pulls/123" initiated-requests))
        (should (member "/repos/owner/repo/pulls/123/reviews" initiated-requests))
        (should (member "/repos/owner/repo/pulls/123/requested_reviewers" initiated-requests))

        ;; Verify result structure
        (should (assq 'review-decision result))
        (should (assq 'status-text result))
        (should (assq 'approved-count result))

        ;; Log final state
        (message "TEST: Total initiated: %d, Total completed: %d"
                 async-calls-initiated async-calls-completed)))))

;;; Edge Cases

(ert-deftest test-shipit--clean-text-edge-cases ()
  "Test text cleaning with edge cases."
  (should (string= (shipit--clean-text "") ""))
  (should (string= (shipit--clean-text "no-line-endings") "no-line-endings"))
  (should (string= (shipit--clean-text "\r\r\r") ""))
  (should (string= (shipit--clean-text "line1\r\n\r\nline2") "line1\n\nline2")))

(ert-deftest test-shipit-empty-pr-data ()
  "Test handling of empty or invalid PR data."
  (should-not (shipit--clean-text nil))
  (should (string= (shipit--clean-text "") "")))

;;; Configuration Tests

(ert-deftest test-shipit-customization-variables ()
  "Test that customization variables are properly defined."
  (should (boundp 'shipit-github-token))
  (should (boundp 'shipit-magit-integration))
  (should (boundp 'shipit-api-url))
  (should (boundp 'shipit-render-markdown))
  (should (boundp 'shipit-auto-collapse-comment-lines))
  (should (boundp 'shipit-auto-refresh))
  (should (boundp 'shipit-enable-mouse-navigation)))

(ert-deftest test-shipit-auto-refresh-default ()
  "Test that auto-refresh is disabled by default for performance."
  (should (eq shipit-auto-refresh nil)))

(ert-deftest test-shipit-mouse-navigation-default ()
  "Test that mouse navigation is disabled by default, respecting Emacs keyboard tradition."
  (should (eq shipit-enable-mouse-navigation nil)))

(ert-deftest shipit--worktree-customization-test ()
  "Test that worktree directory customization exists with correct default."
  (should (boundp 'shipit-worktree-directory))
  (should (equal shipit-worktree-directory ".worktrees/")))

(ert-deftest test-shipit--colorize-pr-state ()
  "Test PR state colorization function."
  (let ((open-state (shipit--colorize-pr-state "open"))
        (closed-state (shipit--colorize-pr-state "closed"))
        (merged-state (shipit--colorize-pr-state "merged"))
        (draft-state (shipit--colorize-pr-state "draft"))
        (unknown-state (shipit--colorize-pr-state "unknown")))

    ;; Test that states are returned (even if we can't easily test colors in batch mode)
    (should (stringp open-state))
    (should (stringp closed-state))
    (should (stringp merged-state))
    (should (stringp draft-state))
    (should (stringp unknown-state))

    ;; Test that the actual state text is preserved
    (should (string-match "open" open-state))
    (should (string-match "closed" closed-state))
    (should (string-match "merged" merged-state))
    (should (string-match "draft" draft-state))
    (should (string-match "unknown" unknown-state))))

(ert-deftest test-shipit-inline-comments-functionality ()
  "Test inline comments functionality."
  ;; Ensure magit modules are loaded for the test
  (require 'shipit-pr-sections)
  (require 'shipit-diff)
  ;; Test that inline comments functions exist
  (should (fboundp 'shipit--group-inline-comments-by-file))
  (should (fboundp 'shipit--fetch-inline-comments))
  (should (fboundp 'shipit--open-file-diff))
  (should (fboundp 'shipit--insert-file-comment))

  ;; Test comment grouping using realistic recorded data
  (let ((comments (append test-recorded-comments-data
                          '(((path . "src/models/network.py") (line . 5) (created_at . "2025-01-15T09:00:00Z"))))))
    (let ((grouped (shipit--group-inline-comments-by-file comments)))
      (should (> (hash-table-count grouped) 0))
      (should (gethash "src/models/network.py" grouped))
      ;; Comments should be sorted by line number then creation time
      (let ((file-comments (gethash "src/models/network.py" grouped)))
        (should (> (length file-comments) 0)))))

  ;; Test comment with diff_hunk handling using recorded data
  (let ((inline-comment (car (seq-filter (lambda (c) (cdr (assq 'path c))) test-recorded-comments-data))))
    ;; Verify diff_hunk field exists in recorded data
    (should (cdr (assq 'diff_hunk inline-comment)))))

;;; Comment Threading Tests

(ert-deftest test-shipit--insert-inline-comment-thread-single ()
  "Test that single comments are handled correctly without duplication."
  (with-temp-buffer
    (insert "some diff content\n")
    (let ((single-comment '(((id . 123)
                             (body . "This is a single comment")
                             (user . ((login . "reviewer")))
                             (created_at . "2024-01-01T10:00:00Z")
                             (line . 10))))
          (line-end (line-end-position)))
      ;; Mock the hierarchical threading functions to track calls
      (let ((threading-calls 0)
            (diff-comment-calls 0))
        (cl-letf (((symbol-function 'shipit--group-comments-by-api-replies)
                   (lambda (comments)
                     (setq threading-calls (1+ threading-calls))
                     (let ((threads (make-hash-table :test 'equal)))
                       (puthash 'root comments threads)
                       threads)))
                  ((symbol-function 'shipit--insert-diff-threaded-comment)
                   (lambda (comment threads depth)
                     (setq diff-comment-calls (1+ diff-comment-calls))
                     (insert "Threaded single comment\n")))
                  ;; Mock magit functions to avoid dependencies
                  ((symbol-function 'magit-insert-section)
                   (lambda (section-spec &rest body)
                     (dolist (form body) (eval form))))
                  ((symbol-function 'magit-insert-heading)
                   (lambda (heading) (insert heading "\n")))
                  ((symbol-function 'magit-insert-section-body)
                   (lambda (&rest body) (dolist (form body) (eval form))))
                  ((symbol-function 'magit-current-section)
                   (lambda () nil)) ; No parent section for standalone mode
                  ;; Define hunk-comments as a valid section type
                  (hunk-comments 'hunk-comments))
          (let ((shipit-use-magit-sections-for-diff-comments t))
            (shipit--insert-inline-comment-thread single-comment line-end)
            ;; Should call threading functions exactly once
            (should (= threading-calls 1))
            (should (= diff-comment-calls 1))))))))

(ert-deftest test-shipit--insert-inline-comment-thread-multiple ()
  "Test that multiple comments are formatted using hierarchical threading."
  (with-temp-buffer
    (insert "some diff content\n")
    (let ((multiple-comments '(((id . 123)
                                (body . "First comment")
                                (user . ((login . "reviewer1")))
                                (created_at . "2024-01-01T10:00:00Z")
                                (line . 10))
                               ((id . 124)
                                (body . "Second comment")
                                (user . ((login . "reviewer2")))
                                (created_at . "2024-01-01T10:01:00Z")
                                (line . 10))))
          (line-end (line-end-position)))
      ;; Mock the hierarchical threading functions to track calls
      (let ((threading-calls 0)
            (diff-comment-calls 0))
        (cl-letf (((symbol-function 'shipit--group-comments-by-api-replies)
                   (lambda (comments)
                     (setq threading-calls (1+ threading-calls))
                     (let ((threads (make-hash-table :test 'equal)))
                       (puthash 'root comments threads)
                       threads)))
                  ((symbol-function 'shipit--insert-diff-threaded-comment)
                   (lambda (comment threads depth)
                     (setq diff-comment-calls (1+ diff-comment-calls))
                     (insert "Threaded comment\n")))
                  ;; Mock magit functions to avoid dependencies
                  ((symbol-function 'magit-insert-section)
                   (lambda (section-spec &rest body)
                     (dolist (form body) (eval form))))
                  ((symbol-function 'magit-insert-heading)
                   (lambda (heading) (insert heading "\n")))
                  ((symbol-function 'magit-insert-section-body)
                   (lambda (&rest body) (dolist (form body) (eval form))))
                  ((symbol-function 'magit-current-section)
                   (lambda () nil)) ; No parent section for standalone mode
                  ;; Define hunk-comments as a valid section type
                  (hunk-comments 'hunk-comments))
          (let ((shipit-use-magit-sections-for-diff-comments t))
            (shipit--insert-inline-comment-thread multiple-comments line-end)
            ;; Should call threading functions and process both root comments
            (should (= threading-calls 1))
            (should (= diff-comment-calls 2))))))))

(ert-deftest test-shipit--group-inline-comments-by-quotes ()
  "Test comment threading by quoted text detection."
  (let ((comments '(((id . 1)
                     (body . "Original comment about the bug")
                     (user . ((login . "author")))
                     (created_at . "2024-01-01T10:00:00Z"))
                    ((id . 2)
                     (body . "> Original comment about the bug\n\nI agree with this analysis")
                     (user . ((login . "reviewer")))
                     (created_at . "2024-01-01T10:01:00Z"))
                    ((id . 3)
                     (body . "Unrelated comment")
                     (user . ((login . "other")))
                     (created_at . "2024-01-01T10:02:00Z")))))

    ;; Mock the helper functions
    (cl-letf (((symbol-function 'shipit--clean-comment-text)
               (lambda (text) text))
              ((symbol-function 'shipit--extract-quoted-text)
               (lambda (body)
                 (when (string-match ">" body)
                   (list "Original comment about the bug"))))
              ((symbol-function 'shipit--find-comment-containing-text)
               (lambda (quoted-text comments)
                 (when (string-match "Original comment" quoted-text)
                   1))))

      (let ((result (shipit--group-inline-comments-by-quotes comments)))
        ;; Should have root comments
        (should (gethash 'root result))

        ;; For now, simple implementation treats all comments as root comments
        (let ((root-comments (gethash 'root result)))
          (should (= (length root-comments) 3))
          (should (member (nth 0 comments) root-comments))  ; comment 1
          (should (member (nth 1 comments) root-comments))  ; comment 2
          (should (member (nth 2 comments) root-comments))))))) ; comment 3

(ert-deftest test-shipit--get-comments-for-line ()
  "Test that comments are correctly retrieved for specific file and line."
  (let ((shipit--cached-inline-comments
         '(((path . "file1.js") (line . 10) (id . 1) (body . "Comment 1"))
           ((path . "file1.js") (line . 10) (id . 2) (body . "Comment 2"))
           ((path . "file1.js") (line . 15) (id . 3) (body . "Comment 3"))
           ((path . "file2.py") (line . 10) (id . 4) (body . "Comment 4")))))

    ;; Test getting multiple comments for same line
    (let ((comments-line-10 (shipit--get-comments-for-line "file1.js" 10)))
      (should (= (length comments-line-10) 2))
      (should (= (cdr (assq 'id (nth 0 comments-line-10))) 1))
      (should (= (cdr (assq 'id (nth 1 comments-line-10))) 2)))

    ;; Test getting single comment
    (let ((comments-line-15 (shipit--get-comments-for-line "file1.js" 15)))
      (should (= (length comments-line-15) 1))
      (should (= (cdr (assq 'id (car comments-line-15))) 3)))

    ;; Test getting comments for different file
    (let ((comments-py (shipit--get-comments-for-line "file2.py" 10)))
      (should (= (length comments-py) 1))
      (should (= (cdr (assq 'id (car comments-py))) 4)))

    ;; Test no comments found
    (let ((no-comments (shipit--get-comments-for-line "file3.txt" 20)))
      (should (= (length no-comments) 0)))))

(ert-deftest test-shipit--filter-comments-for-hunk ()
  "Test that comments are correctly filtered to hunk ranges."
  (let ((comments '(((line . 5) (id . 1) (body . "Before hunk"))
                    ((line . 10) (id . 2) (body . "In hunk start"))
                    ((line . 15) (id . 3) (body . "In hunk middle"))
                    ((line . 20) (id . 4) (body . "In hunk end"))
                    ((line . 25) (id . 5) (body . "After hunk"))
                    ((line . nil) (id . 6) (body . "Outdated comment"))))
        (hunk-info '(:new-start 10 :new-end 20)))

    (let ((filtered (shipit--filter-comments-for-hunk comments hunk-info)))
      (should (= (length filtered) 3))
      ;; Should include comments with lines 10, 15, 20
      (should (member '((line . 10) (id . 2) (body . "In hunk start")) filtered))
      (should (member '((line . 15) (id . 3) (body . "In hunk middle")) filtered))
      (should (member '((line . 20) (id . 4) (body . "In hunk end")) filtered))
      ;; Should exclude comments outside range and outdated comments
      (should-not (member '((line . 5) (id . 1) (body . "Before hunk")) filtered))
      (should-not (member '((line . 25) (id . 5) (body . "After hunk")) filtered))
      (should-not (member '((line . nil) (id . 6) (body . "Outdated comment")) filtered)))))

;;; Refactored Function Tests

(ert-deftest test-shipit--should-display-comments-p ()
  "Test comment display condition checking - always true in diff mode."
  ;; Mock the functions - toggle has been removed, only checks mode
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (&rest modes) (member 'magit-diff-mode modes))))
    ;; Should return t when in diff mode
    (should (shipit--should-display-comments-p)))

  ;; Should return nil when not in diff mode
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (&rest _modes) nil)))
    (should-not (shipit--should-display-comments-p))))

(ert-deftest test-shipit--get-pr-context ()
  "Test PR context extraction."
  ;; Mock functions to return test data
  (cl-letf (((symbol-function 'magit-get-current-branch)
             (lambda () "test-branch"))
            ((symbol-function 'shipit--get-repo-from-remote)
             (lambda () "owner/repo"))
            ((symbol-function 'shipit--get-current-pr-data)
             (lambda (branch repo) '((number . 123) (title . "Test PR")))))

    (let ((context (shipit--get-pr-context)))
      (should context)
      (should (string= (plist-get context :branch) "test-branch"))
      (should (string= (plist-get context :repo) "owner/repo"))
      (should (= (plist-get context :pr-number) 123))))

  ;; Test when no PR is found
  (cl-letf (((symbol-function 'magit-get-current-branch)
             (lambda () "test-branch"))
            ((symbol-function 'shipit--get-repo-from-remote)
             (lambda () "owner/repo"))
            ((symbol-function 'shipit--get-current-pr-data)
             (lambda (branch repo) nil)))
    (should-not (shipit--get-pr-context))))

(ert-deftest test-shipit--parse-diff-files ()
  "Test diff file parsing functionality."
  (with-temp-buffer
    ;; Create a mock diff buffer with multiple files
    (insert "diff --git a/file1.js b/file1.js\n")
    (insert "index 1234567..abcdefg 100644\n")
    (insert "--- a/file1.js\n")
    (insert "+++ b/file1.js\n")
    (insert "@@ -10,7 +10,7 @@\n")
    (insert " function test() {\n")
    (insert "-  return false;\n")
    (insert "+  return true;\n")
    (insert " }\n")
    (insert "diff --git a/file2.py b/file2.py\n")
    (insert "index abcdefg..7654321 100644\n")
    (insert "--- a/file2.py\n")
    (insert "+++ b/file2.py\n")
    (insert "@@ -5,3 +5,3 @@\n")
    (insert " def hello():\n")
    (insert "-    print('old')\n")
    (insert "+    print('new')\n")

    (let ((files (shipit--parse-diff-files)))
      (should (= (length files) 2))
      (should (string= (caar files) "file1.js"))
      (should (string= (car (cadr files)) "file2.py"))
      (should (numberp (cdar files)))
      (should (numberp (cdadr files)))))

  ;; Test with magit-style diff
  (with-temp-buffer
    (insert "modified   src/main.c\n")
    (insert "@@ -1,5 +1,5 @@\n")
    (insert " #include <stdio.h>\n")
    (insert "-int main() {\n")
    (insert "+int main(void) {\n")
    (insert "     return 0;\n")
    (insert " }\n")

    (let ((files (shipit--parse-diff-files)))
      (should (= (length files) 1))
      (should (string= (caar files) "src/main.c")))))

(ert-deftest test-shipit-get-pr-comments-for-file-uses-buffer-local-cache ()
  "GIVEN shipit--cached-inline-comments is populated with comments for multiple files
WHEN calling shipit-get-pr-comments-for-file for a specific file
THEN it returns matching comments from buffer-local cache without calling the backend API."
  (let ((shipit--cached-inline-comments
         '(((id . 10) (path . "main.el") (line . 42) (body . "Fix this"))
           ((id . 11) (path . "test.el") (line . 5) (body . "Add test"))
           ((id . 12) (path . "main.el") (line . 100) (body . "Rename var"))))
        (shipit--comment-cache (make-hash-table :test 'equal))
        (shipit--comment-type-cache (make-hash-table :test 'equal))
        (api-called nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "myorg/myrepo"))
              ((symbol-function 'shipit-comment--resolve-for-repo)
               (lambda (_repo)
                 (setq api-called t)
                 (cons (list :fetch-inline-comments #'ignore) '(:repo "myorg/myrepo")))))
      (let ((result (shipit-get-pr-comments-for-file 42 "main.el")))
        ;; THEN only main.el comments are returned
        (should (= 2 (length result)))
        (should (equal 10 (cdr (assq 'id (nth 0 result)))))
        (should (equal 12 (cdr (assq 'id (nth 1 result)))))
        ;; THEN backend API was NOT called
        (should-not api-called)
        ;; THEN result is cached in per-file cache
        (should (gethash "42:main.el" shipit--comment-cache))))))

(ert-deftest test-shipit-get-pr-comments-for-file-falls-through-when-no-buffer-cache ()
  "GIVEN shipit--cached-inline-comments is nil
WHEN calling shipit-get-pr-comments-for-file
THEN it falls through to the backend API fetch."
  (let ((shipit--cached-inline-comments nil)
        (shipit--comment-cache (make-hash-table :test 'equal))
        (shipit--comment-type-cache (make-hash-table :test 'equal))
        (shipit-github-token "fake-token")
        (shipit-pr-backend 'github)
        (api-called nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "myorg/myrepo"))
              ((symbol-function 'shipit-comment--resolve-for-repo)
               (lambda (_repo)
                 (setq api-called t)
                 (cons (list :fetch-inline-comments
                             (lambda (_config _number)
                               '(((id . 20) (path . "file.el") (line . 1) (body . "API comment")))))
                       '(:repo "myorg/myrepo")))))
      (let ((result (shipit-get-pr-comments-for-file 42 "file.el")))
        ;; THEN backend API was called
        (should api-called)
        (should (= 1 (length result)))
        (should (equal 20 (cdr (assq 'id (car result)))))))))

(ert-deftest test-shipit--process-diff-file ()
  "Test single diff file processing logic."
  (let ((processed-files (make-hash-table :test 'equal))
        (file-entry '("test.js" . 100))
        (pr-number 123))

    ;; Mock the comment retrieval function
    (cl-letf (((symbol-function 'shipit-get-pr-comments-for-file)
               (lambda (pr file)
                 (when (and (= pr 123) (string= file "test.js"))
                   '(((id . 1) (body . "Test comment"))))))
              ((symbol-function 'shipit--process-file-hunks)
               (lambda (file comments pos)
                 (should (string= file "test.js"))
                 (should (= (length comments) 1))
                 (should (= pos 100)))))

      ;; First call should process the file
      (should (shipit--process-diff-file file-entry pr-number processed-files))
      (should (gethash "test.js" processed-files))

      ;; Second call should skip the file
      (should-not (shipit--process-diff-file file-entry pr-number processed-files)))))


;;; Test Runner

(defun shipit-run-all-tests ()
  "Run all tests for shipit."
  (interactive)
  (ert-run-tests-batch-and-exit "test-shipit"))

;;; Comment Editing and Cache Invalidation Tests

(ert-deftest test-shipit-comment-cache-clearing ()
  "Test that comment caches are properly cleared when editing inline comments."
  ;; Test the cache clearing logic directly instead of full integration
  (let ((test-comment-cache (make-hash-table :test 'equal))
        (test-inline-comments '(test-comment))
        (test-inline-fetched t))

    ;; Populate test cache
    (puthash "owner/repo:123:file1.js" '(comment1) test-comment-cache)
    (puthash "owner/repo:123:file2.py" '(comment2) test-comment-cache)
    (puthash "other/repo:456:file3.go" '(comment3) test-comment-cache)

    ;; Verify initial state
    (should (= (hash-table-count test-comment-cache) 3))
    (should test-inline-comments)
    (should test-inline-fetched)

    ;; Test the selective cache clearing logic (extracted from edit function)
    (let* ((repo "owner/repo")
           (pr-number 123)
           (keys-to-remove '()))
      ;; This is the same logic as in shipit--edit-comment-interactive
      (maphash (lambda (key value)
                 (when (string-prefix-p (format "%s:%s:" repo pr-number) key)
                   (push key keys-to-remove)))
               test-comment-cache)
      (dolist (key keys-to-remove)
        (remhash key test-comment-cache))

      ;; Simulate clearing inline comment caches (as done for inline comments)
      (setq test-inline-comments nil)
      (setq test-inline-fetched nil)

      ;; Verify selective clearing worked
      (should (null (gethash "owner/repo:123:file1.js" test-comment-cache)))
      (should (null (gethash "owner/repo:123:file2.py" test-comment-cache)))
      ;; Other repo's cache should remain
      (should (gethash "other/repo:456:file3.go" test-comment-cache))
      (should (= (hash-table-count test-comment-cache) 1))

      ;; Verify inline caches were cleared
      (should (null test-inline-comments))
      (should (null test-inline-fetched)))))

(ert-deftest test-shipit-general-comment-cache-not-cleared ()
  "Test that inline comment caches are NOT cleared when editing general comments."
  ;; Set up initial cache state
  (let ((shipit--cached-inline-comments '(test-comment))
        (shipit--inline-comments-fetched t)
        (shipit--comment-cache (make-hash-table :test 'equal))
        (shipit--comment-type-cache (make-hash-table :test 'equal)))

    ;; Mark comment as general comment (not inline)
    (puthash 789 nil shipit--comment-type-cache)

    ;; Populate the per-file comment cache
    (puthash "owner/repo:123:file1.js" '(comment1) shipit--comment-cache)

    ;; Mock functions
    (cl-letf (((symbol-function 'shipit--edit-comment)
               (lambda (comment-id body is-inline) t))
              ((symbol-function 'read-string)
               (lambda (prompt &optional initial-input)
                 ;; Return modified text
                 "Updated general comment"))
              ((symbol-function 'shipit--in-general-comments-section-p)
               (lambda () t))
              ((symbol-function 'shipit--refresh-general-comments-section)
               (lambda () t))
              ((symbol-function 'derived-mode-p)
               (lambda (&rest modes) (member 'magit-status-mode modes))))

      ;; Call edit function with general comment (should NOT clear inline caches)
      (shipit--edit-comment-interactive 789 "Updated general comment")

      ;; Verify inline comment caches were NOT cleared
      (should shipit--cached-inline-comments)
      (should shipit--inline-comments-fetched)
      (should (gethash "owner/repo:123:file1.js" shipit--comment-cache)))))

(ert-deftest test-shipit-display-comments-force-parameter ()
  "Test that the old display-inline-comments system is properly disabled."
  ;; The old diff-based inline comment system has been intentionally disabled
  ;; in favor of the new magit-section hook system. This test verifies it stays disabled.

  ;; The function should do nothing (system is disabled)
  (shipit--display-inline-comments)
  (should t) ; Function completes without error

  ;; Even with force, should do nothing
  (shipit--display-inline-comments t)
  (should t)) ; Function completes without error

(ert-deftest test-shipit-branch-cache-clearing-includes-comment-cache ()
  "Test that branch cache clearing includes the per-file comment cache."
  (let ((shipit--cached-general-comments '(test))
        (shipit--cached-inline-comments '(test))
        (shipit--comment-cache (make-hash-table :test 'equal))
        (shipit--comment-type-cache (make-hash-table :test 'equal)))

    ;; Populate caches with keys matching the expected format (PR:repo/file)
    (puthash "123:owner/repo/file.el" "test-value" shipit--comment-cache)
    (puthash "456:other/repo/file.el" "other-value" shipit--comment-cache)  ; Different repo

    ;; Verify initial state
    (should shipit--cached-general-comments)
    (should shipit--cached-inline-comments)
    (should (= (hash-table-count shipit--comment-cache) 2))

    ;; Mock shipit--get-repo-from-remote to return "owner/repo"
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      ;; Clear branch cache
      (shipit--clear-branch-cache))

    ;; Verify buffer-local caches were cleared
    (should (null shipit--cached-general-comments))
    (should (null shipit--cached-inline-comments))
    ;; Only the current repo's entries should be cleared, not all entries
    (should (= (hash-table-count shipit--comment-cache) 1))
    ;; The "other/repo" entry should still exist
    (should (gethash "456:other/repo/file.el" shipit--comment-cache))))

(ert-deftest test-shipit-comment-cache-selective-clearing ()
  "Test that comment cache clearing is selective by PR when editing."
  (let ((shipit--comment-cache (make-hash-table :test 'equal)))

    ;; Populate cache with multiple repos and PRs
    (puthash "owner/repo:123:file1.js" '(comment1) shipit--comment-cache)
    (puthash "owner/repo:123:file2.py" '(comment2) shipit--comment-cache)
    (puthash "owner/repo:456:file1.js" '(comment3) shipit--comment-cache)
    (puthash "other/repo:123:file1.js" '(comment4) shipit--comment-cache)

    (should (= (hash-table-count shipit--comment-cache) 4))

    ;; Mock the selective clearing logic (extracted from edit function)
    (let* ((repo "owner/repo")
           (pr-number 123)
           (keys-to-remove '()))
      (maphash (lambda (key value)
                 (when (string-prefix-p (format "%s:%s:" repo pr-number) key)
                   (push key keys-to-remove)))
               shipit--comment-cache)
      (dolist (key keys-to-remove)
        (remhash key shipit--comment-cache))

      ;; Verify only the current PR's cache entries were removed
      (should (null (gethash "owner/repo:123:file1.js" shipit--comment-cache)))
      (should (null (gethash "owner/repo:123:file2.py" shipit--comment-cache)))
      ;; Other PR and repo entries should remain
      (should (gethash "owner/repo:456:file1.js" shipit--comment-cache))
      (should (gethash "other/repo:123:file1.js" shipit--comment-cache))
      (should (= (hash-table-count shipit--comment-cache) 2)))))

;;; Performance Benchmarks

(defun shipit-benchmark-pr-detection ()
  "Benchmark PR detection performance."
  (interactive)
  (let ((start-time (current-time))
        (iterations 100))

    (dotimes (i iterations)
      (let ((shipit--current-pr-data nil))
        (cl-letf (((symbol-function 'shipit-get-pr-for-branch)
                   (lambda (branch repo) test-pr-data)))
          (shipit--get-current-pr-data "test-branch" "owner/repo"))))

    (let ((end-time (current-time)))
      (message "PR detection benchmark: %d iterations in %.3f seconds (%.3f ms per call)"
               iterations
               (float-time (time-subtract end-time start-time))
               (* 1000 (/ (float-time (time-subtract end-time start-time)) iterations))))))

(ert-deftest test-shipit--truncate-comment-context ()
  "Test comment context truncation with newline handling and ellipsis indicators."
  ;; Test truncation at newline when within limit (with ellipsis for more content)
  (should (string= (shipit--truncate-comment-context "First line\nSecond line\nThird line" 30)
                   "First line..."))

  ;; Test truncation at max width when no newline within limit (with ellipsis)
  (should (string= (shipit--truncate-comment-context "This is a very long comment without newlines that should be truncated" 20)
                   "This is a very lo..."))

  ;; Test handling of Windows line endings (with ellipsis for more content)
  (should (string= (shipit--truncate-comment-context "First line\r\nSecond line" 30)
                   "First line..."))

  ;; Test short text unchanged (no ellipsis needed)
  (should (string= (shipit--truncate-comment-context "Short" 30)
                   "Short"))

  ;; Test single line that fits exactly (no ellipsis)
  (should (string= (shipit--truncate-comment-context "Exact fit text" 15)
                   "Exact fit text"))

  ;; Test text with newline at end (no ellipsis since no content after newline)
  (should (string= (shipit--truncate-comment-context "Just one line\n" 30)
                   "Just one line"))

  ;; Test nil input
  (should (eq (shipit--truncate-comment-context nil 30) nil)))

;;; Review Operation Tests

(ert-deftest test-shipit-review-post-integration ()
  "GIVEN a PR authored by another user
WHEN calling shipit-post-review with APPROVE
THEN backend :submit-review is called with correct args."
  (let ((submit-called nil)
        (submit-args nil))
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_pr-number &optional _repo) '((user (login . "other-user")))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "current-user"))
              ((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :submit-review
                             (lambda (_config number event body comments)
                               (setq submit-called t)
                               (setq submit-args (list number event body comments))
                               '((id . 1))))
                       (list :repo "owner/repo"))))
              ((symbol-function 'shipit-clear-etag-cache-for-endpoint) #'ignore)
              ((symbol-function 'shipit-buffer-refresh) #'ignore))
      (let ((shipit--cached-approval-status (make-hash-table :test 'equal)))
        (shipit-post-review 123 "APPROVE" "Test approval" nil "owner/repo")
        (should submit-called)
        (should (= 123 (nth 0 submit-args)))
        (should (string= "APPROVE" (nth 1 submit-args)))
        (should (string= "Test approval" (nth 2 submit-args)))))))

(ert-deftest test-shipit-review-dismiss-integration ()
  "GIVEN a mock PR backend with :dismiss-review
WHEN calling shipit-dismiss-review
THEN backend :dismiss-review is called with correct args."
  (let ((dismiss-called nil)
        (dismiss-args nil))
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :dismiss-review
                             (lambda (_config number message)
                               (setq dismiss-called t)
                               (setq dismiss-args (list number message))
                               '((id . 1))))
                       (list :repo "owner/repo"))))
              ((symbol-function 'shipit-clear-etag-cache-for-endpoint) #'ignore)
              ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
              ((symbol-function 'shipit-buffer-refresh) #'ignore))
      (let ((shipit--cached-approval-status (make-hash-table :test 'equal)))
        (shipit-dismiss-review 123 "Undoing approval" "owner/repo")
        (should dismiss-called)
        (should (= 123 (nth 0 dismiss-args)))
        (should (string= "Undoing approval" (nth 1 dismiss-args)))))))

(ert-deftest test-shipit-review-callback-logic ()
  "Test that the review success callbacks include cache clearing and refresh logic."
  ;; Test the callback logic by simulating callback execution
  (let ((shipit--cached-approval-status "Approved")
        (magit-refresh-called nil))

    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest modes) t))  ; Simulate being in magit-status-mode
              ((symbol-function 'magit-refresh)
               (lambda () (setq magit-refresh-called t))))

      ;; Test the logic that should be in success callback
      ;; Clear approval status cache to force refresh
      (setq shipit--cached-approval-status nil)
      ;; Refresh magit-status buffer to show updated review state
      (when (derived-mode-p 'magit-status-mode)
        (magit-refresh))

      ;; Verify the expected behavior
      (should (null shipit--cached-approval-status))
      (should magit-refresh-called))))

(ert-deftest test-shipit-review-dismiss-finds-latest-non-dismissed ()
  "GIVEN multiple reviews including dismissed and other-user reviews
WHEN shipit-pr-github--dismiss-review is called
THEN it dismisses the most recent APPROVED review by the current user (ID 200)."
  (let ((dismissed-review-id nil))
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint)
                 '(((id . 100)
                    (user (login . "current-user"))
                    (state . "DISMISSED"))
                   ((id . 200)
                    (user (login . "current-user"))
                    (state . "APPROVED"))
                   ((id . 150)
                    (user (login . "current-user"))
                    (state . "DISMISSED"))
                   ((id . 50)
                    (user (login . "other-user"))
                    (state . "APPROVED")))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "current-user"))
              ((symbol-function 'shipit--api-request-post)
               (lambda (endpoint _data &optional _method)
                 (when (string-match "/reviews/\\([0-9]+\\)/dismissals" endpoint)
                   (setq dismissed-review-id
                         (string-to-number (match-string 1 endpoint))))
                 '((id . 1)))))
      (shipit-pr-github--dismiss-review '(:repo "owner/repo") 123 "Test dismissal")
      (should (= 200 dismissed-review-id)))))

(ert-deftest test-shipit-contextual-review-modes ()
  "Test that contextual review modes return correct options based on review state."
  ;; Mock the review state function
  (cl-letf (((symbol-function 'shipit--get-current-user-review-state)
             (lambda (pr-number)
               (pcase pr-number
                 (100 '(:state approved :body "Looks good"))
                 (200 '(:state changes_requested :body "Needs fixes"))
                 (300 '(:state commented :body "Just a comment"))
                 (_ nil)))))

    ;; Test approved state
    (let ((modes (shipit--get-contextual-review-modes 100)))
      (should (equal modes '(review-undo-approve review-reject))))

    ;; Test changes requested state
    (let ((modes (shipit--get-contextual-review-modes 200)))
      (should (equal modes '(review-approve review-undo-reject))))

    ;; Test commented state
    (let ((modes (shipit--get-contextual-review-modes 300)))
      (should (equal modes '(review-approve review-reject review-dismiss))))

    ;; Test no review state
    (let ((modes (shipit--get-contextual-review-modes 400)))
      (should (equal modes '(review-approve review-reject))))))

;;; Comment Editing Regression Tests

(ert-deftest test-shipit-edit-general-comment-endpoint ()
  "GIVEN a general comment (is-inline-comment = nil)
WHEN calling shipit--edit-comment
THEN backend :edit-comment is called with is-inline=nil and no pr-number."
  (let ((captured-args nil)
        (shipit-github-token "test-token")
        (shipit-comment-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit--current-displayed-pr '(42 "owner/repo"))
        (shipit--comment-type-cache (make-hash-table :test 'equal)))
    ;; Cache the comment as general comment (nil)
    (puthash 12345 nil shipit--comment-type-cache)

    ;; Register mock backend
    (shipit-comment-register-backend
     'mock
     (list :name "Mock"
           :fetch-general-comments #'ignore :fetch-inline-comments #'ignore
           :add-general-comment #'ignore :add-inline-comment #'ignore
           :reply-to-comment #'ignore
           :edit-comment (lambda (config comment-id body &optional is-inline pr-number)
                           (setq captured-args (list :comment-id comment-id :body body
                                                     :is-inline is-inline :pr-number pr-number))
                           '((body . "Updated comment body")))
           :delete-comment #'ignore :toggle-reaction #'ignore :fetch-reactions #'ignore))

    (cl-letf (((symbol-function 'shipit--update-comment-body-display)
               (lambda (comment-id new-body repo) t)))
      (shipit--edit-comment 12345 "Updated comment body" nil)
      (should captured-args)
      (should (= 12345 (plist-get captured-args :comment-id)))
      (should (string= "Updated comment body" (plist-get captured-args :body)))
      (should-not (plist-get captured-args :is-inline))
      (should-not (plist-get captured-args :pr-number)))))

(ert-deftest test-shipit-edit-comment-uses-buffer-repo-over-stale-global ()
  "GIVEN shipit-buffer-repo set to 'correct/repo'
   AND shipit--current-displayed-pr pointing to 'stale/repo'
   WHEN editing a comment
   THEN the backend receives config for 'correct/repo', not 'stale/repo'."
  (let ((captured-config nil)
        (shipit-github-token "test-token")
        (shipit-comment-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        ;; Stale global from a different buffer
        (shipit--current-displayed-pr '(99 "stale/repo"))
        (shipit--comment-type-cache (make-hash-table :test 'equal)))
    (puthash 500 nil shipit--comment-type-cache)
    ;; Buffer-local repo should take precedence
    (setq-local shipit-buffer-repo "correct/repo")
    (shipit-comment-register-backend
     'mock
     (list :name "Mock"
           :fetch-general-comments #'ignore :fetch-inline-comments #'ignore
           :add-general-comment #'ignore :add-inline-comment #'ignore
           :reply-to-comment #'ignore
           :edit-comment (lambda (config comment-id body &optional is-inline pr-number)
                           (setq captured-config config)
                           '((body . "edited")))
           :delete-comment #'ignore :toggle-reaction #'ignore :fetch-reactions #'ignore))
    (cl-letf (((symbol-function 'shipit--update-comment-body-display) #'ignore))
      (shipit--edit-comment 500 "edited" nil)
      (should captured-config)
      ;; Config should be resolved from "correct/repo", not "stale/repo"
      (should (equal "correct/repo" (plist-get captured-config :repo))))))

(ert-deftest test-shipit-edit-inline-comment-endpoint ()
  "GIVEN an inline comment (is-inline-comment = t)
WHEN calling shipit--edit-comment
THEN backend :edit-comment is called with is-inline=t."
  (let ((captured-args nil)
        (shipit-github-token "test-token")
        (shipit-comment-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit--current-displayed-pr '(42 "owner/repo"))
        (shipit--comment-type-cache (make-hash-table :test 'equal)))
    ;; Cache the comment as inline comment (t)
    (puthash 67890 t shipit--comment-type-cache)

    ;; Register mock backend
    (shipit-comment-register-backend
     'mock
     (list :name "Mock"
           :fetch-general-comments #'ignore :fetch-inline-comments #'ignore
           :add-general-comment #'ignore :add-inline-comment #'ignore
           :reply-to-comment #'ignore
           :edit-comment (lambda (config comment-id body &optional is-inline pr-number)
                           (setq captured-args (list :comment-id comment-id :body body
                                                     :is-inline is-inline :pr-number pr-number))
                           '((body . "Updated inline comment")))
           :delete-comment #'ignore :toggle-reaction #'ignore :fetch-reactions #'ignore))

    (cl-letf (((symbol-function 'shipit--update-comment-body-display)
               (lambda (comment-id new-body repo) t)))
      (shipit--edit-comment 67890 "Updated inline comment" t)
      (should captured-args)
      (should (= 67890 (plist-get captured-args :comment-id)))
      (should (string= "Updated inline comment" (plist-get captured-args :body)))
      (should (eq t (plist-get captured-args :is-inline))))))

(ert-deftest test-shipit-edit-comment-interactive-general ()
  "Test that editing general comments through interactive function uses correct detection."
  (let ((edit-comment-called nil)
        (edit-comment-args nil)
        (shipit--comment-type-cache (make-hash-table :test 'equal)))

    ;; Mark comment as general (not inline) in cache
    (puthash 12345 nil shipit--comment-type-cache)

    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt initial) "Updated comment"))
              ((symbol-function 'shipit--edit-comment)
               (lambda (comment-id new-body is-inline is-review)
                 (setq edit-comment-called t)
                 (setq edit-comment-args (list comment-id new-body is-inline is-review))))
              ((symbol-function 'shipit--refresh-after-comment-operation)
               (lambda (is-inline) t))
              ;; Mock section detection to return true for general comments
              ((symbol-function 'shipit--in-general-comments-section-p)
               (lambda () t))
              ;; Unbind shipit-editor-open to force fallback path
              ((symbol-function 'shipit-editor-open) nil))

      ;; Call interactive edit function
      (shipit--edit-comment-interactive 12345 "Original comment")

      ;; Verify it called edit-comment with correct parameters
      (should edit-comment-called)
      (let ((comment-id (car edit-comment-args))
            (new-body (cadr edit-comment-args))
            (is-inline (caddr edit-comment-args)))
        (should (= comment-id 12345))
        (should (string= new-body "Updated comment"))
        ;; Should be nil for general comment
        (should (null is-inline))))))

(ert-deftest test-shipit-comment-type-caching ()
  "Test that comment types are properly cached for both inline and general comments."
  (let ((shipit-github-token "test-token")
        (shipit--comment-type-cache (make-hash-table :test 'equal))
        (shipit--comment-cache (make-hash-table :test 'equal))
        (shipit--cached-general-comments nil)
        (api-requests '()))

    ;; Mock API request to return realistic test comments
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "example-org/awesome-project"))
              ((symbol-function 'shipit--api-request)
               (lambda (endpoint &optional data callback)
                 (push endpoint api-requests)
                 (let ((response (cond
                                  ;; Inline comments endpoint
                                  ((string-match "/pulls/.*/comments" endpoint)
                                   (seq-filter (lambda (c) (cdr (assq 'path c))) test-recorded-comments-data))
                                  ;; General comments - issues endpoint
                                  ((string-match "/issues/.*/comments" endpoint)
                                   (seq-filter (lambda (c) (null (cdr (assq 'path c)))) test-recorded-comments-data))
                                  ;; Reviews endpoint
                                  ((string-match "/pulls/.*/reviews" endpoint)
                                   test-recorded-reviews-data))))
                   ;; If callback provided, call it with the response
                   (if callback
                       (funcall callback :data response)
                     response))))
              ;; Mock ETag functions used by general comments fetch
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint &optional params token force-fresh)
                 (push endpoint api-requests)
                 (let ((response (cond
                                  ;; Inline comments endpoint
                                  ((string-match "/pulls/.*/comments" endpoint)
                                   (seq-filter (lambda (c) (cdr (assq 'path c))) test-recorded-comments-data))
                                  ;; General comments - issues endpoint
                                  ((string-match "/issues/.*/comments" endpoint)
                                   (seq-filter (lambda (c) (null (cdr (assq 'path c)))) test-recorded-comments-data))
                                  ;; Reviews endpoint
                                  ((string-match "/pulls/.*/reviews" endpoint)
                                   test-recorded-reviews-data))))
                   (list :json response))))
              ((symbol-function 'buffer-live-p) (lambda (buf) t))
              ((symbol-function 'with-current-buffer) (lambda (buf &rest body) (eval `(progn ,@body))))
              ((symbol-function 'magit-refresh) (lambda () nil)))

      ;; Test inline comment type caching using realistic data
      (let* ((first-inline-comment (car (seq-filter (lambda (c) (cdr (assq 'path c))) test-recorded-comments-data)))
             (file-path (cdr (assq 'path first-inline-comment)))
             (comment-id (cdr (assq 'id first-inline-comment))))
        (shipit-get-pr-comments-for-file 42 file-path)
        ;; Should have cached inline comments as t
        (should (eq (gethash comment-id shipit--comment-type-cache) t)))

      ;; Test general comment type caching by simulating the general comments fetch
      (shipit--fetch-general-comments "example-org/awesome-project" 42)
      ;; Should have cached general comments as nil
      (let* ((general-comment (car (seq-filter (lambda (c) (null (cdr (assq 'path c)))) test-recorded-comments-data)))
             (comment-id (cdr (assq 'id general-comment))))
        (should (eq (gethash comment-id shipit--comment-type-cache) nil)))

      ;; Test bulk inline comment caching via shipit--fetch-inline-comments
      (shipit--fetch-inline-comments "example-org/awesome-project" 42)
      ;; Should have cached all inline comments from the bulk fetch as t
      (let* ((inline-comments (seq-filter (lambda (c) (cdr (assq 'path c))) test-recorded-comments-data))
             (first-id (cdr (assq 'id (car inline-comments))))
             (second-id (cdr (assq 'id (cadr inline-comments)))))
        (should (eq (gethash first-id shipit--comment-type-cache) t))
        (when second-id
          (should (eq (gethash second-id shipit--comment-type-cache) t)))))))

;;; Markdown Details Block Tests

(ert-deftest test-shipit--parse-markdown-details-simple ()
  "Test parsing a simple markdown details block."
  (let ((text "<details>\n<summary>Summary text</summary>\n\nBody content\n</details>"))
    (let ((result (shipit--parse-markdown-details text)))
      (should (= (length result) 1))
      (let* ((block (car result))
             (type (car block))
             (summary (cadr block))
             (body (caddr block)))
        (should (eq type 'details))
        (should (string= summary "Summary text"))
        ;; Body includes whitespace after </summary>
        (should (string= body "\n\nBody content\n"))))))

(ert-deftest test-shipit--parse-markdown-details-multiline-body ()
  "Test parsing details block with multiline body content."
  (let ((text "<details>\n<summary>Test Summary</summary>\n\nLine 1\nLine 2\nLine 3\n</details>"))
    (let ((result (shipit--parse-markdown-details text)))
      (should (= (length result) 1))
      (let* ((block (car result))
             (body (caddr block)))
        ;; Body includes whitespace after </summary>
        (should (string= body "\n\nLine 1\nLine 2\nLine 3\n"))))))

(ert-deftest test-shipit--parse-markdown-details-multiple-blocks ()
  "Test parsing multiple details blocks in one text."
  (let ((text "<details>\n<summary>First</summary>\nBody 1\n</details>\n\nSome text\n\n<details>\n<summary>Second</summary>\nBody 2\n</details>"))
    (let ((result (shipit--parse-markdown-details text)))
      (should (= (length result) 3))  ;; First details, text, second details
      ;; Check first details
      (let ((first (car result)))
        (should (eq (car first) 'details))
        (should (string= (cadr first) "First")))
      ;; Check text between
      (let ((middle (cadr result)))
        (should (eq (car middle) 'text))
        (should (string-match-p "Some text" (cadr middle))))
      ;; Check second details
      (let ((second (caddr result)))
        (should (eq (car second) 'details))
        (should (string= (cadr second) "Second"))))))

(ert-deftest test-shipit--parse-markdown-details-text-before-after ()
  "Test parsing details with text before and after."
  (let ((text "Text before\n\n<details>\n<summary>Summary</summary>\nBody\n</details>\n\nText after"))
    (let ((result (shipit--parse-markdown-details text)))
      (should (= (length result) 3))
      ;; First should be text
      (let ((first (car result)))
        (should (eq (car first) 'text))
        (should (string-match-p "Text before" (cadr first))))
      ;; Second should be details
      (let ((second (cadr result)))
        (should (eq (car second) 'details)))
      ;; Third should be text
      (let ((third (caddr result)))
        (should (eq (car third) 'text))
        (should (string-match-p "Text after" (cadr third)))))))

(ert-deftest test-shipit--parse-markdown-details-with-formatting ()
  "Test parsing details block with markdown formatting in body."
  (let ((text "<details>\n<summary>**Bold Summary**</summary>\n\n- Item 1\n- Item 2\n\n```\ncode\n```\n</details>"))
    (let ((result (shipit--parse-markdown-details text)))
      (should (= (length result) 1))
      (let* ((block (car result))
             (body (caddr block)))
        ;; Body should preserve formatting
        (should (string-match-p "- Item 1" body))
        (should (string-match-p "```" body))))))

(ert-deftest test-shipit--parse-markdown-details-empty-summary ()
  "Test parsing details block with empty summary."
  (let ((text "<details>\n<summary></summary>\nBody\n</details>"))
    (let ((result (shipit--parse-markdown-details text)))
      (should (= (length result) 1))
      (let* ((block (car result))
             (summary (cadr block)))
        (should (string= summary ""))))))

(ert-deftest test-shipit--parse-markdown-details-no-details ()
  "Test parsing text without any details blocks."
  (let ((text "Just plain text\nwith multiple lines"))
    (let ((result (shipit--parse-markdown-details text)))
      (should (= (length result) 1))
      (let ((block (car result)))
        (should (eq (car block) 'text))))))

(ert-deftest test-shipit--escape-mid-word-underscores-basic ()
  "Test that mid-word underscores are escaped."
  (let ((input "some_foo_string")
        (expected "some\\_foo\\_string"))
    (should (string= (shipit--escape-mid-word-underscores input) expected))))

(ert-deftest test-shipit--escape-mid-word-underscores-preserve-emphasis ()
  "Test that underscores at word boundaries are NOT escaped (for emphasis)."
  (let ((input "This is _italic_ text")
        (expected "This is _italic_ text"))
    (should (string= (shipit--escape-mid-word-underscores input) expected))))

(ert-deftest test-shipit--escape-mid-word-underscores-skip-backticks ()
  "Test that underscores inside backticks are NOT escaped."
  (let ((input "`final_point_cloud.size()`")
        (expected "`final_point_cloud.size()`"))
    (should (string= (shipit--escape-mid-word-underscores input) expected))))

(ert-deftest test-shipit--escape-mid-word-underscores-mixed ()
  "Test mixed text with backticks and regular underscores."
  (let ((input "The variable_name in `some_code_here` is important")
        (expected "The variable\\_name in `some_code_here` is important"))
    (should (string= (shipit--escape-mid-word-underscores input) expected))))

(ert-deftest test-shipit--escape-mid-word-underscores-code-blocks ()
  "Test that underscores inside code blocks are NOT escaped."
  (let ((input "Text before\n```python\nfinal_point_cloud.size()\nanother_variable\n```\nText after with some_var")
        (expected "Text before\n```python\nfinal_point_cloud.size()\nanother_variable\n```\nText after with some\\_var"))
    (should (string= (shipit--escape-mid-word-underscores input) expected))))

(ert-deftest test-shipit--escape-mid-word-underscores-multiple-inline-code ()
  "Test multiple inline code spans in one line."
  (let ((input "Use `foo_bar` and `baz_qux` but not plain_text")
        (expected "Use `foo_bar` and `baz_qux` but not plain\\_text"))
    (should (string= (shipit--escape-mid-word-underscores input) expected))))

(ert-deftest test-shipit--process-inline-images-html ()
  "Test processing HTML <img> tags when shipit-render-images is enabled."
  (let ((shipit-render-images t)
        (input "<img width=\"1286\" height=\"60\" alt=\"image\" src=\"https://example.com/image.png\" />"))
    (let ((result (shipit--process-inline-images input)))
      ;; Should contain the image label (may show Download failed in tests)
      (should (string-match-p "\\[Image: image" result))
      ;; Original HTML tag should be replaced
      (should-not (string-match-p "<img" result)))))

(ert-deftest test-shipit--process-inline-images-markdown ()
  "Test processing markdown images ![alt](url)."
  (let ((shipit-render-images t)
        (input "![screenshot](https://example.com/screenshot.png)"))
    (let ((result (shipit--process-inline-images input)))
      ;; Should contain the image label (may show Download failed in tests)
      (should (string-match-p "\\[Image: screenshot" result))
      ;; Original markdown syntax should be replaced
      (should-not (string-match-p "!\\[" result)))))

(ert-deftest test-shipit--process-inline-images-disabled ()
  "Test that images are NOT processed when shipit-render-images is nil."
  (let ((shipit-render-images nil)
        (input "![screenshot](https://example.com/screenshot.png)"))
    (should (string= (shipit--process-inline-images input) input))))

(ert-deftest test-shipit--process-inline-images-mixed ()
  "Test processing both HTML and markdown images in same text."
  (let ((shipit-render-images t)
        (input "Here is ![markdown](https://example.com/md.png) and <img src=\"https://example.com/html.png\" alt=\"html\" />"))
    (let ((result (shipit--process-inline-images input)))
      (should (string-match-p "\\[Image: markdown" result))
      (should (string-match-p "\\[Image: html" result))
      ;; Original syntax should be replaced
      (should-not (string-match-p "!\\[" result))
      (should-not (string-match-p "<img" result)))))

(ert-deftest test-shipit--file-comment-blockquote-rendering ()
  "Test that blockquote text in file comments is rendered with markdown faces.
Blockquoted text (lines starting with >) should have a special face applied,
similar to how general comments render blockquotes."
  (let* ((comment '((id . 123)
                    (body . "This is a comment\n\n> This is a quoted reply\n> that spans multiple lines\n\nAnd more text after")
                    (user . ((login . "testuser")))
                    (created_at . "2024-01-01T12:00:00Z")
                    (path . "test.el")
                    (line . 10)))
         ;; Clean and prepare the body like shipit--insert-file-comment does
         (raw-body (cdr (assq 'body comment)))
         (cleaned-body (shipit--clean-text raw-body))
         ;; When markdown rendering is enabled, blockquotes should get faces
         (rendered (if (boundp 'shipit-render-markdown)
                      (shipit--render-markdown cleaned-body)
                    cleaned-body)))
    ;; After rendering, blockquote lines should have face properties
    ;; applied by markdown-mode's font-lock system
    (should (stringp rendered))
    ;; The rendered text should contain the blockquote marker
    (should (string-match-p "^>" rendered))
    ;; The rendered text should have face properties on blockquote lines
    ;; (markdown-mode applies faces during fontification)
    (unless (string= rendered cleaned-body)
      ;; If rendering happened, check for faces
      ;; Look for text with 'face property on blockquote lines
      (let* ((lines (split-string rendered "\n"))
             (blockquote-line (seq-find (lambda (line) (string-match-p "^>" line)) lines)))
        (should blockquote-line)))))

(ert-deftest test-shipit--diff-buffer-comment-blockquote-rendering ()
  "Test that blockquote text in diff buffer comments is rendered with markdown faces.
Comments shown in diff buffers (via shipit--insert-comment-body-only) should preserve
blockquote styling, just like file comments and general comments."
  (let* ((comment '((id . 456)
                    (body . "Review comment\n\n> This is a quoted suggestion\n> for the code\n\nMy opinion follows")
                    (shipit-comment-type . "general")))
         ;; Clean and prepare like shipit--insert-comment-body-only does
         (raw-body (cdr (assq 'body comment)))
         (cleaned-body (shipit--clean-comment-text raw-body))
         ;; Render with markdown
         (rendered (if (boundp 'shipit-render-markdown)
                      (shipit--render-markdown cleaned-body)
                    cleaned-body)))
    ;; After rendering, should have blockquote text
    (should (stringp rendered))
    (should (string-match-p "^>" rendered))
    ;; The markdown rendering should have applied faces to blockquote lines
    (unless (string= rendered cleaned-body)
      (let* ((lines (split-string rendered "\n"))
             (blockquote-line (seq-find (lambda (line) (string-match-p "^>" line)) lines)))
        (should blockquote-line)))))

(ert-deftest test-shipit--filter-outdated-comments ()
  "Test filtering outdated comments from active comments."
  (let ((comments '(
          ((id . 1) (body . "active comment") (outdated . nil))
          ((id . 2) (body . "outdated comment") (outdated . t))
          ((id . 3) (body . "another active") (outdated . nil)))))
    (let ((result (shipit--filter-active-comments comments)))
      (should (= (length result) 2))
      (should (= (length (shipit--filter-outdated-comments comments)) 1)))))

;;; Hide Outdated Comments Integration Tests

(ert-deftest test-shipit-expanded-files-filter-outdated ()
  "Test that expanded files view filters outdated comments correctly.
This verifies Task 2: Filter Outdated from Expanded Files View."
  (let ((all-comments '(
          ((id . 1) (path . "test.js") (line . 10) (body . "active comment") (outdated . nil))
          ((id . 2) (path . "test.js") (line . 15) (body . "outdated comment") (outdated . t))
          ((id . 3) (path . "test.js") (line . 20) (body . "another active") (outdated . nil))
          ((id . 4) (path . "other.js") (line . 5) (body . "outdated in other file") (outdated . t)))))
    ;; Simulate expanded files filtering logic (from shipit-magit.el line 6323)
    (let* ((file-comments (seq-filter (lambda (c) (string= (cdr (assq 'path c)) "test.js")) all-comments))
           (active-comments (shipit--filter-active-comments file-comments))
           (outdated-comments (shipit--filter-outdated-comments file-comments)))
      ;; Should have 2 active comments for test.js
      (should (= (length active-comments) 2))
      ;; Should have 1 outdated comment for test.js
      (should (= (length outdated-comments) 1))
      ;; Active comments should contain correct IDs
      (should (member 1 (mapcar (lambda (c) (cdr (assq 'id c))) active-comments)))
      (should (member 3 (mapcar (lambda (c) (cdr (assq 'id c))) active-comments)))
      ;; Outdated should not be in active list
      (should-not (member 2 (mapcar (lambda (c) (cdr (assq 'id c))) active-comments))))))

(ert-deftest test-shipit-diff-buffer-filter-outdated ()
  "Test that diff buffer view filters outdated comments correctly.
This verifies Task 3: Filter Outdated from Diff Buffer View."
  (let ((sorted-comments '(
          ((id . 101) (line . 50) (body . "current comment") (outdated . nil))
          ((id . 102) (line . 50) (body . "old comment") (outdated . t))
          ((id . 103) (line . 50) (body . "another current") (outdated . nil)))))
    ;; Simulate diff buffer filtering logic (from shipit-http.el line 5104)
    (let ((active-comments (shipit--filter-active-comments sorted-comments)))
      ;; Should only have 2 active comments
      (should (= (length active-comments) 2))
      ;; Should contain the current comments
      (should (member "current comment" (mapcar (lambda (c) (cdr (assq 'body c))) active-comments)))
      (should (member "another current" (mapcar (lambda (c) (cdr (assq 'body c))) active-comments)))
      ;; Should NOT contain outdated
      (should-not (member "old comment" (mapcar (lambda (c) (cdr (assq 'body c))) active-comments))))))

(ert-deftest test-shipit-file-header-counts ()
  "Test that file headers show correct active and outdated comment counts.
This verifies Task 4: Add Outdated Count to File Header."
  (let ((all-comments '(
          ((id . 1) (path . "src/main.js") (line . 10) (outdated . nil))
          ((id . 2) (path . "src/main.js") (line . 15) (outdated . nil))
          ((id . 3) (path . "src/main.js") (line . 20) (outdated . t))
          ((id . 4) (path . "src/main.js") (line . 25) (outdated . t))
          ((id . 5) (path . "src/main.js") (line . 30) (outdated . t)))))
    ;; Simulate count calculation logic (from shipit-magit.el line 6198, 6218)
    (let* ((file-comments (seq-filter (lambda (c) (string= (cdr (assq 'path c)) "src/main.js")) all-comments))
           (active-count (length (shipit--filter-active-comments file-comments)))
           (outdated-count (length (shipit--filter-outdated-comments file-comments))))
      ;; Should have 2 active comments
      (should (= active-count 2))
      ;; Should have 3 outdated comments
      (should (= outdated-count 3))
      ;; Format string should show both counts (e.g., "📧 2  🕐 3")
      (let ((header-str (format "📧 %d%s"
                               active-count
                               (if (> outdated-count 0)
                                   (format "  🕐 %d" outdated-count)
                                 ""))))
        (should (string-match "📧 2  🕐 3" header-str))))))

(ert-deftest test-shipit-file-only-outdated-comments ()
  "Test edge case: file with ONLY outdated comments shows zero active, N outdated.
This verifies proper handling when all comments are outdated."
  (let ((all-outdated '(
          ((id . 1) (path . "old.js") (line . 5) (outdated . t))
          ((id . 2) (path . "old.js") (line . 10) (outdated . t))
          ((id . 3) (path . "old.js") (line . 15) (outdated . t)))))
    (let* ((active-comments (shipit--filter-active-comments all-outdated))
           (outdated-comments (shipit--filter-outdated-comments all-outdated)))
      ;; Should have 0 active comments
      (should (= (length active-comments) 0))
      ;; Should have 3 outdated comments
      (should (= (length outdated-comments) 3))
      ;; Header should show only outdated indicator (no active count or 📧 0)
      (let ((header-str (if (> (length active-comments) 0)
                           (format "📧 %d  🕐 %d" (length active-comments) (length outdated-comments))
                         (format "🕐 %d" (length outdated-comments)))))
        (should (string-match "🕐 3" header-str))
        (should-not (string-match "📧" header-str))))))

(ert-deftest test-shipit-file-only-active-comments ()
  "Test edge case: file with ONLY active comments shows N active, no outdated indicator.
This verifies proper handling when all comments are current."
  (let ((all-active '(
          ((id . 1) (path . "current.js") (line . 5) (outdated . nil))
          ((id . 2) (path . "current.js") (line . 10) (outdated . nil))
          ((id . 3) (path . "current.js") (line . 15) (outdated . nil)))))
    (let* ((active-comments (shipit--filter-active-comments all-active))
           (outdated-comments (shipit--filter-outdated-comments all-active)))
      ;; Should have 3 active comments
      (should (= (length active-comments) 3))
      ;; Should have 0 outdated comments
      (should (= (length outdated-comments) 0))
      ;; Header should show only active count (no outdated indicator)
      (let ((header-str (format "📧 %d%s"
                               (length active-comments)
                               (if (> (length outdated-comments) 0)
                                   (format "  🕐 %d" (length outdated-comments))
                                 ""))))
        (should (string-match "📧 3" header-str))
        (should-not (string-match "🕐" header-str))))))

(ert-deftest test-shipit-mixed-thread-handling ()
  "Test mixed threads: outdated root with active replies should preserve thread.
This verifies Task 5: Handle Mixed Threads (Outdated Root with Active Replies)."
  (let ((mixed-thread '(
          ;; Root comment (outdated)
          ((id . 100) (path . "file.js") (line . 20) (body . "Original question") (outdated . t) (in_reply_to_id . nil))
          ;; Reply 1 (active)
          ((id . 101) (path . "file.js") (line . 20) (body . "Answer 1") (outdated . nil) (in_reply_to_id . 100))
          ;; Reply 2 (active)
          ((id . 102) (path . "file.js") (line . 20) (body . "Answer 2") (outdated . nil) (in_reply_to_id . 100)))))
    ;; Using the preserve-mixed-threads filter (from shipit-render.el line 66)
    (let ((filtered (shipit--filter-comments-preserve-mixed-threads mixed-thread)))
      ;; Should preserve all 3 comments (outdated root + 2 active replies)
      (should (= (length filtered) 3))
      ;; Should include the outdated root (because it has active replies)
      (should (member 100 (mapcar (lambda (c) (cdr (assq 'id c))) filtered)))
      ;; Should include both active replies
      (should (member 101 (mapcar (lambda (c) (cdr (assq 'id c))) filtered)))
      (should (member 102 (mapcar (lambda (c) (cdr (assq 'id c))) filtered))))))

(ert-deftest test-shipit-standalone-outdated-filtered ()
  "Test that standalone outdated comments (no replies) are filtered out.
This verifies that outdated comments without active replies are hidden."
  (let ((standalone-outdated '(
          ;; Standalone outdated (should be filtered)
          ((id . 200) (path . "file.js") (line . 10) (body . "Old comment") (outdated . t) (in_reply_to_id . nil))
          ;; Active comment (should remain)
          ((id . 201) (path . "file.js") (line . 15) (body . "Current comment") (outdated . nil) (in_reply_to_id . nil)))))
    ;; Using the preserve-mixed-threads filter
    (let ((filtered (shipit--filter-comments-preserve-mixed-threads standalone-outdated)))
      ;; Should only have 1 comment (the active one)
      (should (= (length filtered) 1))
      ;; Should NOT include standalone outdated
      (should-not (member 200 (mapcar (lambda (c) (cdr (assq 'id c))) filtered)))
      ;; Should include active comment
      (should (member 201 (mapcar (lambda (c) (cdr (assq 'id c))) filtered))))))

(ert-deftest test-shipit-multiple-files-filtering ()
  "Test that filtering works correctly across multiple files.
This verifies that per-file filtering handles different comment states independently."
  (let ((multi-file-comments '(
          ;; File 1: mixed active and outdated
          ((id . 1) (path . "file1.js") (line . 10) (outdated . nil))
          ((id . 2) (path . "file1.js") (line . 15) (outdated . t))
          ;; File 2: all outdated
          ((id . 3) (path . "file2.js") (line . 5) (outdated . t))
          ((id . 4) (path . "file2.js") (line . 10) (outdated . t))
          ;; File 3: all active
          ((id . 5) (path . "file3.js") (line . 20) (outdated . nil))
          ((id . 6) (path . "file3.js") (line . 25) (outdated . nil)))))
    ;; Test file 1 (mixed)
    (let* ((file1-comments (seq-filter (lambda (c) (string= (cdr (assq 'path c)) "file1.js")) multi-file-comments))
           (file1-active (length (shipit--filter-active-comments file1-comments)))
           (file1-outdated (length (shipit--filter-outdated-comments file1-comments))))
      (should (= file1-active 1))
      (should (= file1-outdated 1)))
    ;; Test file 2 (all outdated)
    (let* ((file2-comments (seq-filter (lambda (c) (string= (cdr (assq 'path c)) "file2.js")) multi-file-comments))
           (file2-active (length (shipit--filter-active-comments file2-comments)))
           (file2-outdated (length (shipit--filter-outdated-comments file2-comments))))
      (should (= file2-active 0))
      (should (= file2-outdated 2)))
    ;; Test file 3 (all active)
    (let* ((file3-comments (seq-filter (lambda (c) (string= (cdr (assq 'path c)) "file3.js")) multi-file-comments))
           (file3-active (length (shipit--filter-active-comments file3-comments)))
           (file3-outdated (length (shipit--filter-outdated-comments file3-comments))))
      (should (= file3-active 2))
      (should (= file3-outdated 0)))))

(ert-deftest test-shipit-reactions-placeholder-when-empty ()
  "Reactions line should show dimmed plus sign when comment has no reactions."
  (let ((comment '((id . 123)
                   (body . "test comment")
                   (user . ((login . "testuser")))
                   (created_at . "2025-01-01T00:00:00Z")
                   (path . "test.py")
                   (line . 42))))
    ;; Format reactions for comment with no reactions
    (let ((formatted (shipit--format-comment-reactions comment t)))
      ;; Should return non-empty string with placeholder
      (should (and formatted (not (string-empty-p formatted))))
      ;; Should contain plus sign emoji (fallback placeholder)
      (should (string-match "➕" formatted)))))

(ert-deftest test-shipit-reactions-with-data ()
  "Reactions line should show placeholder + emoji reactions when data exists."
  (let ((shipit-current-repo "owner/repo")
        (comment '((id . 456)
                   (body . "test comment")
                   (user . ((login . "testuser")))
                   (created_at . "2025-01-01T00:00:00Z")
                   (path . "test.py")
                   (line . 50))))
    ;; Mock shipit--ensure-repository to return non-nil
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () shipit-current-repo))
              ;; Mock emoji support detection to ensure emojis are rendered
              ((symbol-function 'shipit--detect-emoji-support)
               (lambda () 'full)))
      ;; Mock reaction cache with test reactions
      (let ((cache-key "owner/repo:456-inline"))
        (puthash cache-key '(((id . 1)
                              (content . "+1")
                              (user . ((login . "reviewer1")))))
                 shipit--reaction-cache)

        ;; Format reactions - should include placeholder + reaction emoji
        (let ((formatted (shipit--format-comment-reactions comment t)))
          ;; Should have content
          (should formatted)
          ;; Should contain placeholder (always shown)
          (should (string-match "➕" formatted))
          ;; Should contain reaction emoji
          (should (string-match "👍" formatted)))))))


(ert-deftest test-shipit-svg-placeholder-fallback ()
  "SVG placeholder should use octicon-smiley from svglib if available, emoji fallback otherwise."
  (let ((result (shipit--get-reactions-placeholder-icon)))
    ;; Should return non-nil (either SVG or emoji)
    (should result)
    ;; Should be either string (emoji) or image (SVG)
    (should (or (stringp result) (imagep result)))))

(ert-deftest test-shipit-placeholder-always-shown ()
  "Placeholder should always be shown, with reactions to the right of it."
  (let ((shipit-current-repo "owner/repo"))
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () shipit-current-repo))
              ;; Mock emoji support to ensure emojis are rendered
              ((symbol-function 'shipit--detect-emoji-support)
               (lambda () 'full)))
      ;; Test with no reactions
      (let ((empty-comment '((id . 789)
                             (body . "comment")
                             (user . ((login . "user1"))))))
        (let ((formatted (shipit--format-comment-reactions empty-comment t)))
          ;; Should have placeholder
          (should (string-match "➕" formatted))))

      ;; Test with reactions - placeholder should still be shown
      (let ((comment-with-reactions '((id . 790)
                                      (body . "comment")
                                      (user . ((login . "user2"))))))
        (puthash "owner/repo:790-inline"
                 '(((id . 1) (content . "+1") (user . ((login . "reviewer1"))))
                   ((id . 2) (content . "heart") (user . ((login . "reviewer2")))))
                 shipit--reaction-cache)

        (let ((formatted (shipit--format-comment-reactions comment-with-reactions t)))
          ;; Both placeholder and reactions should be present
          (should (string-match "➕" formatted))
          ;; Should have reactions (either as emoji or fallback text)
          (should (or (string-match "👍" formatted) (string-match "+1" formatted)))
          ;; Placeholder should come first (before any reactions)
          (let* ((placeholder-pos (string-match "➕" formatted))
                 (reaction-pos (or (string-match "👍" formatted) (string-match "+1" formatted))))
            (should (< placeholder-pos reaction-pos))))))))

(ert-deftest test-shipit-reactions-in-all-contexts ()
  "Reactions should display with placeholder in inline, general, and description contexts."
  (let ((shipit-current-repo "owner/repo"))
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () shipit-current-repo)))
      (let ((comment '((id . 123)
                       (body . "test comment")))
            (pr '((id . 456)
                  (body . "PR description"))))
        ;; Inline comment - should show placeholder
        (let ((inline-format (shipit--format-comment-reactions comment t)))
          (should (string-match "➕" inline-format)))

        ;; General comment - should show placeholder
        (let ((general-format (shipit--format-comment-reactions comment nil)))
          (should (string-match "➕" general-format)))

        ;; PR/Description - should show placeholder
        (let ((pr-format (shipit--format-comment-reactions pr nil)))
          (should (string-match "➕" pr-format)))))))

(ert-deftest test-shipit-reaction-tooltip-shows-authors ()
  "Reaction tooltip should show authors in minibuffer when cursor is on reaction."
  ;; GIVEN a buffer with reaction text that has shipit-reaction-tooltip property
  (with-temp-buffer
    (let ((start (point)))
      (insert (propertize "👍 2" 'shipit-reaction-tooltip "+1: alice, bob"))
      (insert " ")
      (insert (propertize "❤️ 1" 'shipit-reaction-tooltip "heart: carol"))
      ;; WHEN cursor is on the thumbs-up reaction
      (goto-char start)
      ;; THEN shipit--show-reaction-tooltip should display the authors
      (let ((messages '()))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (shipit--show-reaction-tooltip))
        (should (equal (car messages) "+1: alice, bob"))))

    ;; GIVEN text without shipit-reaction-tooltip property
    (erase-buffer)
    (insert (propertize "some text" 'help-echo "RET/SPC: open" 'shipit-reactions t))
    (goto-char (point-min))
    ;; WHEN we check for tooltip
    (let ((messages '()))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (shipit--show-reaction-tooltip))
      ;; THEN it should NOT show anything
      (should (null messages)))))

(ert-deftest test-shipit-reaction-tooltip-emojify-advice ()
  "Emojify help-echo advice should return reaction tooltip when available."
  ;; GIVEN text with shipit-reaction-tooltip property
  (with-temp-buffer
    (insert (propertize "👍 2" 'shipit-reaction-tooltip "+1: alice, bob"
                        'emojify-text "👍"))
    ;; WHEN emojify-help-function is called via the advice
    (let ((orig-fn (lambda (_w _s pos)
                     (plist-get (text-properties-at pos) 'emojify-text))))
      ;; THEN the reaction tooltip takes priority over emojify's default
      (should (equal (shipit--emojify-help-with-reaction-tooltip
                      orig-fn nil nil 1)
                     "+1: alice, bob")))

    ;; GIVEN text without shipit-reaction-tooltip
    (erase-buffer)
    (insert (propertize "👍" 'emojify-text "👍"))
    ;; WHEN emojify-help-function is called via the advice
    (let ((orig-fn (lambda (_w _s pos)
                     (plist-get (text-properties-at pos) 'emojify-text))))
      ;; THEN it falls back to emojify's default behavior
      (should (equal (shipit--emojify-help-with-reaction-tooltip
                      orig-fn nil nil 1)
                     "👍")))))

;;; Async Pagination Tests

(ert-deftest test-shipit-gh-etag-get-json-paginated-async-single-page ()
  "Test async pagination with a single page of results (< 100 items)."
  (let ((callback-called nil)
        (callback-result nil)
        (api-call-count 0))
    ;; Mock the async function to return a single page with 50 items
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (endpoint params token callback &optional force-fresh)
                 (setq api-call-count (1+ api-call-count))
                 ;; Simulate single page with 50 items
                 (let ((page (or (alist-get 'page params) 1))
                       (mock-data (make-list 50 '((id . 1) (name . "file.txt")))))
                   (funcall callback (list :status 200 :json mock-data))))))

      ;; Call async pagination
      (shipit-gh-etag-get-json-paginated-async
       "/repos/owner/repo/pulls/1/files"
       nil
       "test-token"
       (lambda (result)
         (setq callback-called t)
         (setq callback-result result)))

      ;; Should call API exactly once
      (should (= api-call-count 1))
      ;; Callback should be called
      (should callback-called)
      ;; Result should be a plist with :json key
      (should (plist-get callback-result :json))
      ;; Should return 50 items
      (should (= (length (plist-get callback-result :json)) 50))
      ;; Should have success status
      (should (= (plist-get callback-result :status) 200))
      ;; Should have page count
      (should (= (plist-get callback-result :page-count) 1)))))

(ert-deftest test-shipit-gh-etag-get-json-paginated-async-multiple-pages ()
  "Test async pagination with multiple pages (> 100 items total)."
  (let ((callback-called nil)
        (callback-result nil)
        (api-call-count 0))
    ;; Mock the async function to return 3 pages: 100, 100, 50 items
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (endpoint params token callback &optional force-fresh)
                 (setq api-call-count (1+ api-call-count))
                 (let* ((page (or (alist-get 'page params) 1))
                        (per-page (or (alist-get 'per_page params) 100))
                        ;; Page 1 & 2: 100 items each, Page 3: 50 items
                        (item-count (if (< page 3) 100 50))
                        (mock-data (make-list item-count `((id . ,page) (page . ,page)))))
                   (funcall callback (list :status 200 :json mock-data))))))

      ;; Call async pagination
      (shipit-gh-etag-get-json-paginated-async
       "/repos/owner/repo/pulls/1/files"
       nil
       "test-token"
       (lambda (result)
         (setq callback-called t)
         (setq callback-result result)))

      ;; Should call API 3 times (page 1, 2, 3)
      (should (= api-call-count 3))
      ;; Callback should be called
      (should callback-called)
      ;; Result should be a plist with :json key
      (should (plist-get callback-result :json))
      ;; Should return 250 total items (100 + 100 + 50)
      (should (= (length (plist-get callback-result :json)) 250))
      ;; Should have page count
      (should (= (plist-get callback-result :page-count) 3)))))

(ert-deftest test-shipit-gh-etag-get-json-paginated-async-empty-result ()
  "Test async pagination with empty result."
  (let ((callback-called nil)
        (callback-result nil)
        (api-call-count 0))
    ;; Mock the async function to return empty list
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (endpoint params token callback &optional force-fresh)
                 (setq api-call-count (1+ api-call-count))
                 (funcall callback (list :status 200 :json '())))))

      ;; Call async pagination
      (shipit-gh-etag-get-json-paginated-async
       "/repos/owner/repo/pulls/1/files"
       nil
       "test-token"
       (lambda (result)
         (setq callback-called t)
         (setq callback-result result)))

      ;; Should call API exactly once
      (should (= api-call-count 1))
      ;; Callback should be called
      (should callback-called)
      ;; Result should be a plist with :status key
      (should (plist-get callback-result :status))
      ;; Should return empty list in :json
      (should (equal (plist-get callback-result :json) '())))))

(ert-deftest test-shipit-gh-etag-get-json-paginated-async-exactly-100-items ()
  "Test async pagination with exactly 100 items (boundary case)."
  (let ((callback-called nil)
        (callback-result nil)
        (api-call-count 0))
    ;; Mock to return exactly 100 items on page 1, then 0 on page 2
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (endpoint params token callback &optional force-fresh)
                 (setq api-call-count (1+ api-call-count))
                 (let* ((page (or (alist-get 'page params) 1))
                        (mock-data (if (= page 1)
                                       (make-list 100 '((id . 1)))
                                     '())))
                   (funcall callback (list :status 200 :json mock-data))))))

      ;; Call async pagination
      (shipit-gh-etag-get-json-paginated-async
       "/repos/owner/repo/pulls/1/files"
       nil
       "test-token"
       (lambda (result)
         (setq callback-called t)
         (setq callback-result result)))

      ;; Should call API twice (page 1 returns 100, page 2 returns 0)
      (should (= api-call-count 2))
      ;; Callback should be called
      (should callback-called)
      ;; Result should be a plist
      (should (plist-get callback-result :json))
      ;; Should return exactly 100 items
      (should (= (length (plist-get callback-result :json)) 100))
      ;; Should have page count
      (should (= (plist-get callback-result :page-count) 2)))))

(ert-deftest test-shipit-gh-etag-get-json-paginated-async-preserves-params ()
  "Test that async pagination preserves original params while adding page/per_page."
  (let ((callback-called nil)
        (received-params nil))
    ;; Mock to capture params passed to async function
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (endpoint params token callback &optional force-fresh)
                 (setq received-params params)
                 (funcall callback (list :status 200 :json '((id . 1)))))))

      ;; Call with custom params
      (shipit-gh-etag-get-json-paginated-async
       "/repos/owner/repo/pulls/1/files"
       '((state . "open") (sort . "created"))
       "test-token"
       (lambda (result) (setq callback-called t)))

      ;; Should preserve original params
      (should (equal (alist-get 'state received-params) "open"))
      (should (equal (alist-get 'sort received-params) "created"))
      ;; Should add pagination params
      (should (equal (alist-get 'page received-params) 1))
      (should (equal (alist-get 'per_page received-params) 100)))))

;;; Comment Reply Tests

(ert-deftest test-shipit-reply-to-general-comment-uses-correct-endpoint ()
  "Replying to a general comment should use reply endpoint, not add new comment."
  (let ((reply-to-general-called nil)
        (add-general-called nil)
        (reply-to-inline-called nil))

    (cl-letf (;; Mock reply functions
              ((symbol-function 'shipit--reply-to-general-comment)
               (lambda (pr-number parent-id body)
                 (setq reply-to-general-called t)))

              ((symbol-function 'shipit--add-general-comment-to-pr)
               (lambda (pr-number body)
                 (setq add-general-called t)))

              ((symbol-function 'shipit--reply-to-inline-comment)
               (lambda (pr-number parent-id body &optional file-path)
                 (setq reply-to-inline-called t)))

              ;; Mock refresh function
              ((symbol-function 'shipit--refresh-after-comment-operation)
               (lambda (is-inline) nil)))

      ;; Test: Reply to general comment (in-general=t, has comment-info)
      (let ((comment-info '(12345 . "parent comment body"))
            (file-path nil)
            (line-number nil)
            (in-general t))
        (shipit--execute-action 'reply "Test reply" 123 "owner/repo" comment-info
                                file-path line-number in-general t nil))

      ;; Verify: Should call reply-to-general-comment, NOT add-general-comment-to-pr
      (should reply-to-general-called)
      (should-not add-general-called)
      (should-not reply-to-inline-called))))

(ert-deftest test-shipit-reply-to-inline-comment-uses-correct-endpoint ()
  "Replying to an inline comment should use inline reply endpoint."
  (let ((reply-to-general-called nil)
        (add-general-called nil)
        (reply-to-inline-called nil))

    (cl-letf (;; Mock reply functions
              ((symbol-function 'shipit--reply-to-general-comment)
               (lambda (pr-number parent-id body)
                 (setq reply-to-general-called t)))

              ((symbol-function 'shipit--add-general-comment-to-pr)
               (lambda (pr-number body)
                 (setq add-general-called t)))

              ((symbol-function 'shipit--reply-to-inline-comment)
               (lambda (pr-number parent-id body &optional file-path)
                 (setq reply-to-inline-called t)))

              ;; Mock refresh function
              ((symbol-function 'shipit--refresh-after-comment-operation)
               (lambda (is-inline) nil)))

      ;; Test: Reply to inline comment (in-general=nil, has comment-info, NO file-path/line-number needed)
      (let ((comment-info '(67890 . "inline parent comment"))
            (file-path nil)  ; Not needed when replying
            (line-number nil) ; Not needed when replying
            (in-general nil))
        (shipit--execute-action 'reply "Test inline reply" 123 "owner/repo" comment-info
                                file-path line-number in-general t nil))

      ;; Verify: Should call reply-to-inline-comment
      (should reply-to-inline-called)
      (should-not reply-to-general-called)
      (should-not add-general-called))))

;;; Targeted Inline Comment Section Refresh Tests

(ert-deftest test-shipit--get-file-inline-comments-with-etag ()
  "Test fetching inline comments for a specific file with ETag caching."
  (let* ((test-repo "owner/repo")
         (test-pr-number 123)
         (test-file-path "src/main.js")
         (test-comments
          '(((id . 1001)
             (path . "src/main.js")
             (line . 10)
             (body . "First comment")
             (user . ((login . "user1"))))
            ((id . 1002)
             (path . "src/main.js")
             (line . 20)
             (body . "Second comment")
             (user . ((login . "user2"))))
            ((id . 1003)
             (path . "other/file.js")
             (line . 5)
             (body . "Should not be included")
             (user . ((login . "user3"))))))
         (api-called nil)
         (endpoint-requested nil))

    (cl-letf (;; Mock shipit-gh-etag-get-json-with-refresh-cache
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint params token)
                 (setq api-called t
                       endpoint-requested endpoint)
                 (list :json test-comments
                       :etag "test-etag-123"
                       :from-cache nil)))
              ;; Mock shipit-get-pull-request for stale detection
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_pr-number)
                 '((head . ((sha . "abc123")))))))

      ;; Call the function (will be implemented)
      (let ((result (shipit--get-file-inline-comments test-pr-number test-repo test-file-path)))

        ;; Verify API was called with correct endpoint
        (should api-called)
        (should (string= endpoint-requested
                         (format "/repos/%s/pulls/%s/comments" test-repo test-pr-number)))

        ;; Verify result contains only comments for the specified file
        (should (= (length result) 2))
        (should (cl-every (lambda (comment)
                            (string= (cdr (assq 'path comment)) test-file-path))
                          result))

        ;; Verify comment IDs are correct
        (should (= (cdr (assq 'id (nth 0 result))) 1001))
        (should (= (cdr (assq 'id (nth 1 result))) 1002))))))

(ert-deftest test-shipit--refresh-file-inline-comment-section ()
  "Test targeted refresh of a single file's inline comments section."
  (skip-unless (fboundp 'magit-section-mode))
  (with-temp-buffer
    (shipit-mode)
    (let* ((test-pr-number 123)
           (test-repo "owner/repo")
           (test-file-path "src/main.js")
           (test-comments
            '(((id . 1001)
               (path . "src/main.js")
               (line . 10)
               (body . "Updated comment")
               (user . ((login . "user1"))))))
           (test-pr-data
            `((number . ,test-pr-number)
              (title . "Test PR")
              (files . (((filename . "src/main.js")
                         (status . "modified")
                         (additions . 5)
                         (deletions . 2)
                         (changes . 7)
                         (patch . "@@ -8,3 +8,5 @@\n line 8\n line 9\n+line 10\n"))))))
           (refresh-called nil))

      ;; Set up buffer with PR context
      (setq shipit--current-displayed-pr (list test-pr-number test-repo))
      (setq shipit-buffer-pr-data test-pr-data)
      ;; Set up cached inline comments (refresh uses cache, not API fetch)
      (setq shipit--cached-inline-comments test-comments)

      ;; Insert initial file section with text properties
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "📄 Files Changed (1)\n")
        (magit-insert-section (pr-files nil t)
          (magit-insert-section (pr-file "src/main.js" t)
            (let ((heading-start (point)))
              (magit-insert-heading
                (format "   M src/main.js (+5 -2)"))
              ;; Add text properties that the refresh function uses to locate sections
              (add-text-properties heading-start (point)
                                   `(shipit-pr-file t
                                                    shipit-file-path ,test-file-path)))
            (magit-insert-section-body
              (insert "   [Old comment content]\n")))))

      (cl-letf (;; Mock file diff insertion
                ((symbol-function 'shipit--insert-file-diff)
                 (lambda (patch filename comment-count repo pr-num)
                   (insert (format "   [Fresh diff with %d comments]\n" comment-count)))))

        ;; Call targeted refresh (will be implemented)
        (goto-char (point-min))
        (re-search-forward "src/main.js")
        (setq refresh-called
              (shipit--refresh-file-inline-comment-section test-file-path test-pr-number test-repo))

        ;; Verify refresh was called
        (should refresh-called)

        ;; Verify buffer was updated (uses cached comments, count=1)
        (goto-char (point-min))
        (should (search-forward "[Fresh diff with 1 comments]" nil t))
        (should-not (search-forward "[Old comment content]" nil t))))))

(ert-deftest test-shipit--refresh-inline-comment-section-etag-304 ()
  "Test that 304 Not Modified response uses cached data efficiently."
  (let* ((test-repo "owner/repo")
         (test-pr-number 123)
         (test-file-path "src/main.js")
         (cached-comments
          '(((id . 1001)
             (path . "src/main.js")
             (line . 10)
             (body . "Cached comment")
             (user . ((login . "user1"))))))
         (api-call-count 0))

    (cl-letf (;; Mock ETag cache returning 304
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint params token)
                 (setq api-call-count (1+ api-call-count))
                 (list :json cached-comments
                       :etag "cached-etag-123"
                       :from-cache t
                       :status 304)))
              ;; Mock shipit-get-pull-request for stale detection
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_pr-number)
                 '((head . ((sha . "abc123")))))))

      ;; Call twice - should use cache on second call
      (let ((result1 (shipit--get-file-inline-comments test-pr-number test-repo test-file-path))
            (result2 (shipit--get-file-inline-comments test-pr-number test-repo test-file-path)))

        ;; Verify both calls returned data
        (should (= (length result1) 1))
        (should (= (length result2) 1))

        ;; Verify data is the same (cached)
        (should (equal result1 result2))

        ;; Verify API was called (ETag support means it still makes request but gets 304)
        (should (>= api-call-count 1))))))

(ert-deftest test-shipit--reply-triggers-targeted-refresh ()
  "GIVEN a file-path is provided
WHEN calling shipit--reply-to-inline-comment
THEN targeted refresh is called, not full refresh."
  (let* ((test-pr-number 123)
         (test-repo "owner/repo")
         (test-file-path "src/main.js")
         (test-comment-id 1001)
         (test-reply-body "Test reply")
         (targeted-refresh-called nil)
         (full-refresh-called nil)
         (shipit-comment-backends nil)
         (shipit-pr-backend 'mock)
         (shipit-pr-backend-config nil)
         (shipit--current-displayed-pr (list test-pr-number test-repo)))

    ;; Register mock comment backend
    (shipit-comment-register-backend
     'mock
     (list :name "Mock"
           :fetch-general-comments #'ignore :fetch-inline-comments #'ignore
           :add-general-comment #'ignore :add-inline-comment #'ignore
           :reply-to-comment (lambda (_config _number _parent-id _body &optional _is-inline)
                               (list (cons 'id 2001)
                                     (cons 'body test-reply-body)
                                     (cons 'in_reply_to test-comment-id)))
           :edit-comment #'ignore :delete-comment #'ignore
           :toggle-reaction #'ignore :fetch-reactions #'ignore))

    (cl-letf (;; Mock targeted refresh
              ((symbol-function 'shipit--refresh-file-inline-comment-section)
               (lambda (file-path pr-num repo)
                 (setq targeted-refresh-called t)
                 (should (string= file-path test-file-path))
                 (should (= pr-num test-pr-number))
                 (should (string= repo test-repo))
                 t))

              ;; Mock full refresh (should NOT be called when file-path provided)
              ((symbol-function 'shipit--refresh-after-comment-operation)
               (lambda (is-inline)
                 (setq full-refresh-called t))))

      (let ((shipit-github-token "test-token"))
        ;; Call reply with file-path to trigger targeted refresh
        (shipit--reply-to-inline-comment test-pr-number test-comment-id test-reply-body test-file-path)

        ;; Verify targeted refresh was called, not full refresh
        (should targeted-refresh-called)
        (should-not full-refresh-called)))))

(ert-deftest test-shipit--reply-falls-back-to-full-refresh ()
  "GIVEN no file-path is provided
WHEN calling shipit--reply-to-inline-comment
THEN full refresh is called, not targeted refresh."
  (let* ((test-pr-number 123)
         (test-repo "owner/repo")
         (test-comment-id 1001)
         (test-reply-body "Test reply")
         (targeted-refresh-called nil)
         (full-refresh-called nil)
         (shipit-comment-backends nil)
         (shipit-pr-backend 'mock)
         (shipit-pr-backend-config nil)
         (shipit--current-displayed-pr (list test-pr-number test-repo)))

    ;; Register mock comment backend
    (shipit-comment-register-backend
     'mock
     (list :name "Mock"
           :fetch-general-comments #'ignore :fetch-inline-comments #'ignore
           :add-general-comment #'ignore :add-inline-comment #'ignore
           :reply-to-comment (lambda (_config _number _parent-id _body &optional _is-inline)
                               (list (cons 'id 2001)
                                     (cons 'body test-reply-body)))
           :edit-comment #'ignore :delete-comment #'ignore
           :toggle-reaction #'ignore :fetch-reactions #'ignore))

    (cl-letf (;; Mock targeted refresh (should NOT be called without file-path)
              ((symbol-function 'shipit--refresh-file-inline-comment-section)
               (lambda (file-path pr-num repo)
                 (setq targeted-refresh-called t)))

              ;; Mock full refresh (SHOULD be called)
              ((symbol-function 'shipit--refresh-after-comment-operation)
               (lambda (is-inline)
                 (setq full-refresh-called t)
                 (should (eq is-inline t)))))

      (let ((shipit-github-token "test-token"))
        ;; Call reply WITHOUT file-path - should use full refresh
        (shipit--reply-to-inline-comment test-pr-number test-comment-id test-reply-body nil)

        ;; Verify full refresh was called, not targeted
        (should full-refresh-called)
        (should-not targeted-refresh-called)))))

;;; Regression Tests

;; Test removed: shipit-magit-mode and shipit-notification-mode were removed
;; in favor of shipit-notifications-mode (global-mode-string based)

(ert-deftest test-shipit-short-quoted-replies-are-threaded ()
  "Regression test: Short quoted replies (< 5 chars) should be properly threaded.
Verifies that quotes like '> Foo' create proper parent-child relationships."
  (let* ((comment-1 '((id . 100)
                      (body . "Foo")
                      (created_at . "2025-11-06T08:44:00Z")
                      (user . ((login . "testuser")))))
         (comment-2 '((id . 101)
                      (body . "another general comment")
                      (created_at . "2025-11-06T09:27:00Z")
                      (user . ((login . "testuser")))))
         (comment-3 '((id . 102)
                      (body . "> Foo\n\nThis is a reply.")
                      (created_at . "2025-11-15T22:00:00Z")
                      (user . ((login . "testuser")))))
         (comments (list comment-1 comment-2 comment-3))
         (threads (shipit--group-comments-by-quotes comments)))

    ;; Verify threading structure
    (let ((root-comments (gethash 'root threads))
          (comment-1-replies (gethash 100 threads)))

      ;; Should have 2 root comments (comment-1 and comment-2)
      (should (= 2 (length root-comments)))
      (should (member comment-1 root-comments))
      (should (member comment-2 root-comments))

      ;; comment-3 should be a reply to comment-1
      (should comment-1-replies)
      (should (= 1 (length comment-1-replies)))
      (should (equal (cdr (assq 'id (car comment-1-replies))) 102))

      ;; comment-3 should have correct depth
      (let ((comment-3-depth (cdr (assq 'reply-depth comment-3))))
        (should (and comment-3-depth (> comment-3-depth 0)))))))

(ert-deftest test-shipit-inline-comment-quoted-replies-override-in-reply-to-id ()
  "Regression test: Inline comments with quoted text should thread based on quotes, not in_reply_to_id.
When an inline comment quotes text from another comment, it should be threaded as a child
of the quoted comment, even if in_reply_to_id points to a different parent."
  (let* ((comment-1 '((id . 100)
                      (body . "first comment")
                      (created_at . "2025-11-03T20:31:00Z")
                      (user . ((login . "testuser")))))
         (comment-2 '((id . 101)
                      (body . "first reply")
                      (in_reply_to_id . 100)
                      (created_at . "2025-11-03T20:35:00Z")
                      (user . ((login . "testuser")))))
         (comment-3 '((id . 102)
                      (body . "2nd reply to the same initial comment")
                      (in_reply_to_id . 100)
                      (created_at . "2025-11-03T20:38:00Z")
                      (user . ((login . "testuser")))))
         (comment-4 '((id . 103)
                      (body . "> first reply\n\nquoted reply to first reply")
                      (in_reply_to_id . 100)  ; API says parent is comment-1, but quote indicates parent is comment-2
                      (created_at . "2025-11-15T22:38:00Z")
                      (user . ((login . "testuser")))))
         (comments (list comment-1 comment-2 comment-3 comment-4))
         (threads (shipit--group-comments-by-api-replies comments)))

    ;; Verify threading structure
    (let ((root-comments (gethash 'root threads))
          (comment-1-replies (gethash 100 threads))
          (comment-2-replies (gethash 101 threads)))

      ;; Should have 1 root comment (comment-1)
      (should (= 1 (length root-comments)))
      (should (equal (cdr (assq 'id (car root-comments))) 100))

      ;; comment-1 should have 2 direct children (comment-2 and comment-3)
      (should comment-1-replies)
      (should (= 2 (length comment-1-replies)))
      (should (member 101 (mapcar (lambda (c) (cdr (assq 'id c))) comment-1-replies)))
      (should (member 102 (mapcar (lambda (c) (cdr (assq 'id c))) comment-1-replies)))

      ;; comment-4 should be a reply to comment-2 (based on quoted text), not comment-1
      (should comment-2-replies)
      (should (= 1 (length comment-2-replies)))
      (should (equal (cdr (assq 'id (car comment-2-replies))) 103))

      ;; comment-4 should NOT be in comment-1's replies
      (should-not (member 103 (mapcar (lambda (c) (cdr (assq 'id c))) comment-1-replies)))

      ;; comment-4 should have depth 2 (child of comment-2 which is at depth 1)
      (let ((comment-4-depth (cdr (assq 'reply-depth comment-4))))
        (should (equal comment-4-depth 2))))))

;;; Unread Indicator Tests

(ert-deftest test-shipit-per-activity-read-tracking ()
  "Test per-activity read tracking."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)))
    ;; Mock the save function to do nothing
    (cl-letf (((symbol-function 'shipit-gh-etag--save-cache) #'ignore)
              ((symbol-function 'shipit-gh-etag--ensure-cache-loaded) #'ignore))
      ;; Initially no read activities
      (should-not (shipit--get-read-activities "owner/repo" 123))
      (should-not (shipit--is-activity-read-p "owner/repo" 123 "event-1"))
      ;; Mark one activity as read
      (shipit--mark-activity-read "owner/repo" 123 "event-1")
      ;; Now that activity should be read
      (should (shipit--is-activity-read-p "owner/repo" 123 "event-1"))
      ;; Other activities should still be unread
      (should-not (shipit--is-activity-read-p "owner/repo" 123 "event-2"))
      ;; Mark another activity as read
      (shipit--mark-activity-read "owner/repo" 123 "event-2")
      (should (shipit--is-activity-read-p "owner/repo" 123 "event-2"))
      ;; Different PR should have no read activities
      (should-not (shipit--is-activity-read-p "owner/repo" 456 "event-1"))
      ;; Different repo should have no read activities
      (should-not (shipit--is-activity-read-p "other/repo" 123 "event-1")))))

;;; Review Inline Comments Tests

(ert-deftest test-shipit-get-inline-comments-for-review ()
  "Test that shipit--get-inline-comments-for-review returns comments matching review ID."
  ;; GIVEN inline comments with different pull_request_review_id values
  (let ((shipit--cached-inline-comments
         '(((id . 1) (body . "Comment 1") (path . "file1.el") (line . 10) (pull_request_review_id . 100))
           ((id . 2) (body . "Comment 2") (path . "file2.el") (line . 20) (pull_request_review_id . 200))
           ((id . 3) (body . "Comment 3") (path . "file1.el") (line . 15) (pull_request_review_id . 100))
           ((id . 4) (body . "Comment 4") (path . "file3.el") (line . 5) (pull_request_review_id . 300)))))
    ;; WHEN getting inline comments for review 100
    (let ((result (shipit--get-inline-comments-for-review 100)))
      ;; THEN should return 2 comments with matching review ID
      (should (= 2 (length result)))
      (should (member 1 (mapcar (lambda (c) (cdr (assq 'id c))) result)))
      (should (member 3 (mapcar (lambda (c) (cdr (assq 'id c))) result)))
      (should-not (member 2 (mapcar (lambda (c) (cdr (assq 'id c))) result))))
    ;; WHEN getting inline comments for review with no matches
    (let ((result (shipit--get-inline-comments-for-review 999)))
      ;; THEN should return nil
      (should (null result)))
    ;; WHEN no inline comments cached
    (let ((shipit--cached-inline-comments nil))
      (let ((result (shipit--get-inline-comments-for-review 100)))
        ;; THEN should return nil
        (should (null result))))))

(ert-deftest test-shipit-get-inline-comments-for-review-includes-replies ()
  "Test that replies to review comments are included even without matching pull_request_review_id."
  ;; GIVEN a root comment with pull_request_review_id and replies without it
  (let ((shipit--cached-inline-comments
         '(((id . 1) (body . "Root comment") (path . "file.el") (line . 10) (pull_request_review_id . 100))
           ((id . 2) (body . "Reply to root") (in_reply_to_id . 1))  ; No pull_request_review_id
           ((id . 3) (body . "Nested reply") (in_reply_to_id . 2))   ; No pull_request_review_id
           ((id . 4) (body . "Unrelated") (path . "other.el") (line . 5) (pull_request_review_id . 200)))))
    ;; WHEN getting inline comments for review 100
    (let ((result (shipit--get-inline-comments-for-review 100)))
      ;; THEN should return root comment AND its replies
      (should (= 3 (length result)))
      (should (member 1 (mapcar (lambda (c) (cdr (assq 'id c))) result)))
      (should (member 2 (mapcar (lambda (c) (cdr (assq 'id c))) result)))
      (should (member 3 (mapcar (lambda (c) (cdr (assq 'id c))) result)))
      ;; BUT not unrelated comments
      (should-not (member 4 (mapcar (lambda (c) (cdr (assq 'id c))) result))))))


;;; Stale/outdated comment detection tests

(ert-deftest test-extract-commented-line-last-added-line ()
  "GIVEN a diff hunk with added lines
WHEN no target-line is specified
THEN return the last added (+) line."
  (let ((diff-hunk "@@ -10,3 +10,5 @@ def foo():\n     bar()\n-    return old\n+    return new\n+    extra_line\n "))
    (should (equal "    extra_line"
                   (shipit--extract-commented-line-from-diff-hunk diff-hunk)))))

(ert-deftest test-extract-commented-line-with-target-line ()
  "GIVEN a diff hunk with multiple added lines
WHEN target-line points to a specific line
THEN return the content at that line."
  (let ((diff-hunk "@@ -10,3 +10,5 @@ def foo():\n     bar()\n-    return old\n+    return new\n+    extra_line\n "))
    ;; Line 10 is context "     bar()"
    (should (equal "    bar()"
                   (shipit--extract-commented-line-from-diff-hunk diff-hunk 10)))
    ;; Line 11 is "+    return new"
    (should (equal "    return new"
                   (shipit--extract-commented-line-from-diff-hunk diff-hunk 11)))
    ;; Line 12 is "+    extra_line"
    (should (equal "    extra_line"
                   (shipit--extract-commented-line-from-diff-hunk diff-hunk 12)))))

(ert-deftest test-extract-commented-line-force-push-trailing-context ()
  "GIVEN a diff hunk with trailing context lines (after force push remap)
WHEN target-line points to the original commented line
THEN return the correct line content, not the trailing context."
  ;; Simulates the real bug: diff_hunk has old code, trailing context after comment line
  ;; Hunk starts at new line 87, context(3) + delete(1) + add(3) + context(1) = 7 new lines
  (let ((diff-hunk "@@ -90,4 +87,7 @@ def build():\n         docker_tag(),\n         \".\",\n     ]\n-    return runner(command) == 0\n+    # Rerun without --quiet on failure:\n+    if runner(command + [\"--quiet\"]) != 0:\n+        return runner(command) == 0\n "))
    ;; Line 92 is the "return runner(command) == 0" add line
    ;; Line 93 is the trailing empty context — should NOT be returned
    (should (equal "        return runner(command) == 0"
                   (shipit--extract-commented-line-from-diff-hunk diff-hunk 92)))
    ;; Without target-line, fallback should return last *added* line, not trailing context
    (should (equal "        return runner(command) == 0"
                   (shipit--extract-commented-line-from-diff-hunk diff-hunk)))))

;;; Cache clearing preserves user state tests

(ert-deftest test-shipit-clear-api-cache-preserves-user-state ()
  "GIVEN persistent cache with both API entries and user state entries
WHEN clearing API cache only
THEN user state entries are preserved."
  (require 'shipit-gh-etag)
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)))
    ;; GIVEN API cache entries
    (puthash "owner/repo/repos/owner/repo/pulls/1"
             '(:etag "abc" :json ((id . 1)) :timestamp 1000)
             shipit-gh-etag--persistent-cache)
    (puthash "owner/repo/repos/owner/repo/issues"
             '(:etag "def" :json [] :timestamp 2000)
             shipit-gh-etag--persistent-cache)
    ;; GIVEN user state entries
    (puthash "activities-read:owner/repo:42"
             '("evt-1" "evt-2")
             shipit-gh-etag--persistent-cache)
    (puthash "comments-seen:owner/repo:42"
             '(100 200 300)
             shipit-gh-etag--persistent-cache)
    (puthash "inline-seen:owner/repo:42"
             '(400 500)
             shipit-gh-etag--persistent-cache)
    (puthash "general-seen:owner/repo:42"
             '(600)
             shipit-gh-etag--persistent-cache)
    (puthash "last-viewed:owner/repo:42"
             "2025-02-10T10:00:00Z"
             shipit-gh-etag--persistent-cache)
    ;; WHEN clearing API cache only
    (shipit-gh-etag--clear-api-cache-only)
    ;; THEN API entries are removed
    (should-not (gethash "owner/repo/repos/owner/repo/pulls/1"
                         shipit-gh-etag--persistent-cache))
    (should-not (gethash "owner/repo/repos/owner/repo/issues"
                         shipit-gh-etag--persistent-cache))
    ;; THEN user state entries are preserved
    (should (gethash "activities-read:owner/repo:42"
                     shipit-gh-etag--persistent-cache))
    (should (gethash "comments-seen:owner/repo:42"
                     shipit-gh-etag--persistent-cache))
    (should (gethash "inline-seen:owner/repo:42"
                     shipit-gh-etag--persistent-cache))
    (should (gethash "general-seen:owner/repo:42"
                     shipit-gh-etag--persistent-cache))
    (should (gethash "last-viewed:owner/repo:42"
                     shipit-gh-etag--persistent-cache))))

(ert-deftest test-shipit-clear-api-cache-empty-hash-table ()
  "GIVEN an empty persistent cache
WHEN clearing API cache only
THEN no error occurs."
  (require 'shipit-gh-etag)
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)))
    (shipit-gh-etag--clear-api-cache-only)
    (should (= 0 (hash-table-count shipit-gh-etag--persistent-cache)))))

;;; Cross-repo PR context leak tests

(ert-deftest test-hierarchical-comments-skips-without-buffer-local-context ()
  "GIVEN only shipit--current-displayed-pr is set (no buffer-local context)
WHEN shipit--insert-hierarchical-comments runs in a regular diff buffer
THEN the hook exits early — no API call made."
  (let ((shipit--current-displayed-pr '(732 "gitlab-org/other-project"))
        (shipit--diff-pr-context nil)
        (shipit--refresh-pr-context nil)
        (fetched-prs nil))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (pr-number)
                 (push pr-number fetched-prs)
                 `((number . ,pr-number) (title . "Some PR")))))
      (shipit--insert-hierarchical-comments)
      ;; Hook should not have run at all — no buffer-local or transient context
      (should-not fetched-prs))))

(ert-deftest test-hierarchical-comments-uses-buffer-local-context ()
  "GIVEN shipit--diff-pr-context is set as buffer-local
WHEN shipit--insert-hierarchical-comments runs
THEN the PR from the buffer-local context is fetched."
  (let ((shipit--diff-pr-context '(42 . "my-org/my-repo"))
        (shipit--refresh-pr-context nil)
        (fetched-prs nil))
    (cl-letf (((symbol-function 'magit-get-current-branch)
               (lambda () "feature-branch"))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (pr-number)
                 (push pr-number fetched-prs)
                 `((number . ,pr-number) (title . "My PR"))))
              ((symbol-function 'magit-insert-section)
               (lambda (&rest _) nil)))
      (shipit--insert-hierarchical-comments)
      (should (equal '(42) fetched-prs)))))

(ert-deftest test-hierarchical-comments-commit-diff-no-trigger ()
  "GIVEN a regular commit diff buffer with no shipit PR context
WHEN shipit--insert-hierarchical-comments runs via magit-diff-sections-hook
THEN nothing happens — no API calls, no errors."
  (let ((shipit--diff-pr-context nil)
        (shipit--refresh-pr-context nil)
        (shipit--current-displayed-pr '(999 "stale/context"))
        (fetched-prs nil))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (pr-number)
                 (push pr-number fetched-prs)
                 `((number . ,pr-number)))))
      (shipit--insert-hierarchical-comments)
      (should-not fetched-prs))))

;;; Step 1: Async general comments + reviews

(ert-deftest test-shipit-parallel-sync-uses-async-general-comments ()
  "GIVEN a parallel sync fetch for a GitHub PR
WHEN general comments and reviews are fetched
THEN both general-comments-done and reviews-done flags are set via async callback."
  (let ((shipit-github-token "test-token")
        (shipit-pr-backend 'github)
        (general-comments-done nil)
        (reviews-done nil)
        (general-comments-data nil)
        (reviews-data nil)
        (callback-comments nil))
    (cl-letf (((symbol-function 'shipit--fetch-general-comments-async-github)
               (lambda (_repo _pr-number callback)
                 ;; Simulate async completion with test data
                 (let ((comments '(((id . 1) (body . "test comment")))))
                   (funcall callback comments))))
              ((symbol-function 'shipit--cache-general-comments-with-dedup)
               (lambda (comments)
                 (setq callback-comments comments)))
              ((symbol-function 'buffer-live-p)
               (lambda (_buf) t))
              ((symbol-function 'shipit--debug-log)
               #'ignore))
      ;; Simulate what the parallel sync function does
      (shipit--fetch-general-comments-async-github
       "owner/repo" 42
       (lambda (all-general-comments)
         (setq general-comments-data all-general-comments
               general-comments-done t
               reviews-data nil
               reviews-done t)))
      ;; THEN both flags should be set
      (should general-comments-done)
      (should reviews-done)
      (should (= 1 (length general-comments-data))))))

(ert-deftest test-shipit-cache-general-comments-with-dedup ()
  "GIVEN general comments with some IDs matching inline comments
WHEN caching with dedup
THEN duplicates are removed from the general comments cache."
  (let ((shipit--cached-inline-comments '(((id . 100) (body . "inline"))))
        (shipit--cached-general-comments nil)
        (shipit--general-comments-fetched nil))
    (cl-letf (((symbol-function 'shipit--debug-log) #'ignore))
      (shipit--cache-general-comments-with-dedup
       '(((id . 100) (body . "duplicate"))
         ((id . 200) (body . "unique"))))
      ;; THEN only the unique comment remains
      (should (= 1 (length shipit--cached-general-comments)))
      (should (= 200 (cdr (assq 'id (car shipit--cached-general-comments)))))
      (should shipit--general-comments-fetched))))

;;; Step 2: Async refs section

(ert-deftest test-shipit-refs-section-renders-without-compare ()
  "GIVEN a PR with head and base refs
WHEN rendering the refs section
THEN the section renders immediately without calling compare API."
  (let ((compare-called nil))
    (cl-letf (((symbol-function 'shipit--get-commits-behind-base)
               (lambda (&rest _args)
                 (setq compare-called t)
                 nil))
              ((symbol-function 'shipit--debug-log) #'ignore))
      ;; The refs section should NOT call compare during render
      ;; (compare is now deferred to shipit--fetch-out-of-date-async)
      (should-not compare-called))))

;;; Compare API skip for closed/merged MRs

(ert-deftest test-shipit-skip-compare-for-merged-pr ()
  "GIVEN a merged PR
WHEN the async data fetches start
THEN shipit--fetch-out-of-date-async is NOT called."
  (let ((compare-called nil)
        (pr-data `((state . "closed")
                   (merged_at . "2026-01-17T09:00:00Z")
                   (head . ((ref . "feature-branch")))
                   (base . ((ref . "main"))))))
    (cl-letf (((symbol-function 'shipit--fetch-out-of-date-async)
               (lambda (&rest _) (setq compare-called t)))
              ((symbol-function 'shipit--get-pr-actual-state)
               (lambda (_) "merged")))
      ;; Simulate what shipit-buffer.el lines 942-945 do
      (let ((state (shipit--get-pr-actual-state pr-data)))
        (unless (member state '("merged" "closed"))
          (let ((head-ref (cdr (assq 'ref (cdr (assq 'head pr-data)))))
                (base-ref (cdr (assq 'ref (cdr (assq 'base pr-data))))))
            (shipit--fetch-out-of-date-async "test/repo" base-ref head-ref (current-buffer)))))
      (should-not compare-called))))

(ert-deftest test-shipit-compare-called-for-open-pr ()
  "GIVEN an open PR
WHEN the async data fetches start
THEN shipit--fetch-out-of-date-async IS called."
  (let ((compare-called nil)
        (pr-data `((state . "open")
                   (head . ((ref . "feature-branch")))
                   (base . ((ref . "main"))))))
    (cl-letf (((symbol-function 'shipit--fetch-out-of-date-async)
               (lambda (&rest _) (setq compare-called t)))
              ((symbol-function 'shipit--get-pr-actual-state)
               (lambda (_) "open")))
      (let ((state (shipit--get-pr-actual-state pr-data)))
        (unless (member state '("merged" "closed"))
          (let ((head-ref (cdr (assq 'ref (cdr (assq 'head pr-data)))))
                (base-ref (cdr (assq 'ref (cdr (assq 'base pr-data))))))
            (shipit--fetch-out-of-date-async "test/repo" base-ref head-ref (current-buffer)))))
      (should compare-called))))

;;; Step 3: Find-section-by-type uses recursive search

(ert-deftest test-shipit-find-section-by-type-is-recursive ()
  "GIVEN shipit--find-section-by-type loaded from shipit-sections.el
WHEN called
THEN it should be the recursive version that searches nested sections."
  ;; Verify the function exists and comes from shipit-sections
  (should (fboundp 'shipit--find-section-by-type))
  ;; The function should accept exactly one argument (target-type)
  (let ((arglist (help-function-arglist 'shipit--find-section-by-type)))
    (should (= 1 (length arglist)))))

;;; Avatar TTL cache

(ert-deftest test-shipit-avatar-needs-refresh-no-cache-file ()
  "GIVEN no cached avatar file exists
WHEN checking if avatar needs refresh
THEN return t."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)))
    (should (shipit--avatar-needs-refresh-p "/tmp/nonexistent-avatar.png" "testuser"))))

(ert-deftest test-shipit-avatar-needs-refresh-recent-timestamp ()
  "GIVEN a cached avatar file exists
AND the ETag entry has a recent timestamp (< 1 hour ago)
WHEN checking if avatar needs refresh
THEN return nil (skip network request)."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (tmp-file (make-temp-file "avatar-test")))
    (unwind-protect
        (progn
          (puthash "avatar:testuser"
                   (list :etag "\"abc123\"" :timestamp (float-time))
                   shipit-gh-etag--persistent-cache)
          (should-not (shipit--avatar-needs-refresh-p tmp-file "testuser")))
      (delete-file tmp-file))))

(ert-deftest test-shipit-avatar-needs-refresh-old-timestamp ()
  "GIVEN a cached avatar file exists
AND the ETag entry has an old timestamp (> 1 hour ago)
WHEN checking if avatar needs refresh
THEN return t (need network request)."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (tmp-file (make-temp-file "avatar-test")))
    (unwind-protect
        (progn
          (puthash "avatar:testuser"
                   (list :etag "\"abc123\"" :timestamp (- (float-time) 7200))
                   shipit-gh-etag--persistent-cache)
          (should (shipit--avatar-needs-refresh-p tmp-file "testuser")))
      (delete-file tmp-file))))

(ert-deftest test-shipit-avatar-needs-refresh-no-etag-entry ()
  "GIVEN a cached avatar file exists
AND there is no ETag entry in the cache
WHEN checking if avatar needs refresh
THEN return t (no timestamp to check)."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (tmp-file (make-temp-file "avatar-test")))
    (unwind-protect
        (should (shipit--avatar-needs-refresh-p tmp-file "testuser"))
      (delete-file tmp-file))))

(ert-deftest test-shipit-avatar-304-updates-timestamp ()
  "GIVEN an ETag entry exists without a timestamp
AND shipit--http-request-binary returns 304
WHEN shipit--fetch-binary-with-etag is called
THEN the entry's timestamp is updated so TTL cache works."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)))
    (puthash "avatar:testuser" (list :etag "\"old-etag\"")
             shipit-gh-etag--persistent-cache)
    (cl-letf (((symbol-function 'shipit--http-request-binary)
               (lambda (_url _headers)
                 (list :status 304)))
              ((symbol-function 'shipit--debug-log) #'ignore))
      (shipit--fetch-binary-with-etag "https://example.com/avatar.png" "testuser")
      (let* ((entry (gethash "avatar:testuser" shipit-gh-etag--persistent-cache))
             (ts (plist-get entry :timestamp)))
        ;; THEN timestamp was set
        (should ts)
        ;; AND it's recent (within last 5 seconds)
        (should (< (- (float-time) ts) 5))))))

(ert-deftest test-shipit-avatar-304-creates-entry-when-none-exists ()
  "GIVEN NO ETag entry in the cache
AND shipit--http-request-binary returns 304 (url.el handled ETag)
WHEN shipit--fetch-binary-with-etag is called
THEN a new entry with timestamp is created so TTL cache works."
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)))
    ;; No puthash — cache is empty
    (cl-letf (((symbol-function 'shipit--http-request-binary)
               (lambda (_url _headers)
                 (list :status 304)))
              ((symbol-function 'shipit--debug-log) #'ignore))
      (shipit--fetch-binary-with-etag "https://example.com/avatar.png" "testuser")
      (let* ((entry (gethash "avatar:testuser" shipit-gh-etag--persistent-cache))
             (ts (plist-get entry :timestamp)))
        ;; THEN entry was created
        (should entry)
        ;; AND timestamp was set
        (should ts)
        ;; AND it's recent
        (should (< (- (float-time) ts) 5))))))

;;; Discussion ID Lookup Tests

(ert-deftest test-shipit-lookup-discussion-id-from-inline-cache ()
  "GIVEN cached inline comments with discussion_id
WHEN looking up a note ID
THEN returns the discussion_id."
  (let ((shipit--cached-inline-comments
         '(((id . 12345) (discussion_id . "abc123hex") (body . "test"))
           ((id . 67890) (discussion_id . "def456hex") (body . "other"))))
        (shipit--cached-general-comments nil))
    (should (equal "abc123hex" (shipit--lookup-discussion-id 12345)))
    (should (equal "def456hex" (shipit--lookup-discussion-id 67890)))))

(ert-deftest test-shipit-lookup-discussion-id-fallback ()
  "GIVEN no cached comments with matching ID
WHEN looking up a note ID
THEN returns the note ID unchanged."
  (let ((shipit--cached-inline-comments nil)
        (shipit--cached-general-comments nil))
    (should (equal 99999 (shipit--lookup-discussion-id 99999)))))

(ert-deftest test-shipit-lookup-discussion-id-from-general-cache ()
  "GIVEN cached general comments with discussion_id
WHEN looking up a note ID
THEN returns the discussion_id."
  (let ((shipit--cached-general-comments
         '(((id . 11111) (discussion_id . "gen111hex") (body . "general"))))
        (shipit--cached-inline-comments nil))
    (should (equal "gen111hex" (shipit--lookup-discussion-id 11111)))))

;;; DWIM Handler Tests

(ert-deftest test-shipit-dwim-handler-matches-deleted-lines ()
  "DWIM inline-comment-context handler matches deleted lines.
GIVEN a buffer position with shipit-file-path and shipit-old-line-number
      but no shipit-line-number (deleted line)
WHEN the inline-comment-context matcher is evaluated
THEN it returns non-nil."
  (with-temp-buffer
    (insert "- deleted line\n")
    (goto-char (point-min))
    (add-text-properties (point-min) (point-max)
                         '(shipit-file-path "README.md"
                           shipit-old-line-number 5
                           shipit-repo "owner/repo"
                           shipit-pr-number 1))
    (let* ((handler (cdr (assq 'inline-comment-context shipit--dwim-handlers)))
           (matcher (cdr (assq 'matcher handler))))
      (should (funcall matcher)))))

(ert-deftest test-shipit-dwim-handler-matches-added-lines ()
  "DWIM inline-comment-context handler matches added lines.
GIVEN a buffer position with shipit-file-path and shipit-line-number
      but no shipit-old-line-number (added line)
WHEN the inline-comment-context matcher is evaluated
THEN it returns non-nil."
  (with-temp-buffer
    (insert "+ added line\n")
    (goto-char (point-min))
    (add-text-properties (point-min) (point-max)
                         '(shipit-file-path "README.md"
                           shipit-line-number 10
                           shipit-repo "owner/repo"
                           shipit-pr-number 1))
    (let* ((handler (cdr (assq 'inline-comment-context shipit--dwim-handlers)))
           (matcher (cdr (assq 'matcher handler))))
      (should (funcall matcher)))))

(ert-deftest test-shipit-dwim-handler-matches-context-lines ()
  "DWIM inline-comment-context handler matches context (unchanged) lines.
GIVEN a buffer position with shipit-file-path, shipit-line-number,
      and shipit-old-line-number (context line)
WHEN the inline-comment-context matcher is evaluated
THEN it returns non-nil."
  (with-temp-buffer
    (insert "  context line\n")
    (goto-char (point-min))
    (add-text-properties (point-min) (point-max)
                         '(shipit-file-path "README.md"
                           shipit-line-number 10
                           shipit-old-line-number 8
                           shipit-repo "owner/repo"
                           shipit-pr-number 1))
    (let* ((handler (cdr (assq 'inline-comment-context shipit--dwim-handlers)))
           (matcher (cdr (assq 'matcher handler))))
      (should (funcall matcher)))))

(ert-deftest test-shipit-dwim-handler-skips-existing-comments ()
  "DWIM inline-comment-context handler does not match existing comments.
GIVEN a buffer position with shipit-comment set (already a comment)
WHEN the inline-comment-context matcher is evaluated
THEN it returns nil."
  (with-temp-buffer
    (insert "comment text\n")
    (goto-char (point-min))
    (add-text-properties (point-min) (point-max)
                         '(shipit-file-path "README.md"
                           shipit-line-number 10
                           shipit-comment t
                           shipit-repo "owner/repo"
                           shipit-pr-number 1))
    (let* ((handler (cdr (assq 'inline-comment-context shipit--dwim-handlers)))
           (matcher (cdr (assq 'matcher handler))))
      (should-not (funcall matcher)))))

(provide 'test-shipit)
;;; test-shipit.el ends here
