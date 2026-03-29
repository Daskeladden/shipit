;;; test-shipit-pr-contracts.el --- Contract tests for PR backends -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies that GitHub and GitLab PR backends return data
;; satisfying the output contracts defined in shipit-pr-backends.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-pr-gitlab)

;;; Test helpers

(defvar test-pr-contract-github-config '(:repo "owner/repo")
  "Minimal GitHub config for PR contract tests.")

(defvar test-pr-contract-gitlab-config
  '(:repo "group/project" :project-path "group/project"
    :api-url "https://gitlab.example.com")
  "Minimal GitLab config for PR contract tests.")

;;; Canned API responses — GitHub

(defconst test-pr-contract--github-pr
  '((number . 42)
    (title . "Fix the widget")
    (body . "Description here")
    (state . "open")
    (user . ((login . "octocat") (avatar_url . "https://example.com/a.png")))
    (head . ((ref . "fix-widget") (sha . "abc123")))
    (base . ((ref . "main")))
    (html_url . "https://github.com/owner/repo/pull/42")
    (created_at . "2025-01-01T00:00:00Z"))
  "Canned GitHub PR response.")

(defconst test-pr-contract--github-file
  '((filename . "src/widget.el")
    (status . "modified")
    (additions . 10)
    (deletions . 3)
    (patch . "@@ -1,3 +1,10 @@"))
  "Canned GitHub file entry.")

(defconst test-pr-contract--github-commit
  '((sha . "abc123def456")
    (commit . ((message . "Fix widget rendering")
               (author . ((name . "Octocat") (date . "2025-01-01T00:00:00Z")))))
    (html_url . "https://github.com/owner/repo/commit/abc123def456"))
  "Canned GitHub commit entry.")

(defconst test-pr-contract--github-review
  '((user . ((login . "reviewer")))
    (state . "APPROVED")
    (body . "LGTM"))
  "Canned GitHub review entry.")

(defconst test-pr-contract--github-check-run
  '((name . "CI / tests")
    (status . "completed")
    (conclusion . "success")
    (html_url . "https://github.com/owner/repo/actions/runs/1"))
  "Canned GitHub check run entry.")

;;; Canned API responses — GitLab

(defconst test-pr-contract--gitlab-mr
  '((iid . 10)
    (title . "Fix the widget")
    (description . "MR description")
    (state . "opened")
    (author . ((username . "gluser") (avatar_url . "https://gl.example.com/a.png")))
    (source_branch . "fix-widget")
    (target_branch . "main")
    (sha . "def456")
    (diff_refs . ((start_sha . "aaa") (head_sha . "def456") (base_sha . "bbb")))
    (web_url . "https://gitlab.example.com/group/project/-/merge_requests/10")
    (created_at . "2025-02-01T00:00:00Z")
    (labels . [])
    (assignees . [])
    (draft . :json-false))
  "Canned GitLab MR response.")

(defconst test-pr-contract--gitlab-change
  '((new_path . "src/widget.el")
    (old_path . "src/widget.el")
    (new_file . :json-false)
    (deleted_file . :json-false)
    (renamed_file . :json-false)
    (diff . "@@ -1,3 +1,10 @@\n+new line\n+another"))
  "Canned GitLab MR change entry.")

(defconst test-pr-contract--gitlab-commit
  '((id . "abc123def456789")
    (message . "Fix widget rendering")
    (author_name . "GL User")
    (created_at . "2025-02-01T00:00:00Z")
    (web_url . "https://gitlab.example.com/group/project/-/commit/abc123"))
  "Canned GitLab commit entry.")

(defconst test-pr-contract--gitlab-pipeline-job
  '((name . "test-unit")
    (stage . "test")
    (status . "success")
    (web_url . "https://gitlab.example.com/-/jobs/1")
    (started_at . "2025-02-01T00:00:00Z")
    (finished_at . "2025-02-01T00:05:00Z"))
  "Canned GitLab pipeline job entry.")

;;; GitHub contract tests

(ert-deftest test/pr-github-fetch-pr-contract ()
  ;; GIVEN canned PR data from GitHub ETag API
  ;; WHEN fetching a PR via the GitHub backend
  ;; THEN result satisfies the :fetch-pr contract
  (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
             (lambda (_endpoint &optional _params _token)
               (list :json test-pr-contract--github-pr :status 200))))
    (let ((result (shipit-pr-github--fetch-pr
                   test-pr-contract-github-config 42)))
      (shipit-pr--verify-contract :fetch-pr result))))

(ert-deftest test/pr-github-search-contract ()
  ;; GIVEN canned search results
  ;; WHEN searching PRs via the GitHub backend
  ;; THEN result satisfies the :search contract
  (cl-letf (((symbol-function 'shipit--build-advanced-search-query)
             (lambda (_args _repo) "is:pr"))
            ((symbol-function 'shipit--extract-limit-from-args)
             (lambda (_args) 50))
            ((symbol-function 'shipit--extract-sort-from-args)
             (lambda (_args) nil))
            ((symbol-function 'shipit--search-prs-with-encoded-query)
             (lambda (_repo _query _per-page _limit _sort)
               (list test-pr-contract--github-pr))))
    (let ((result (shipit-pr-github--search
                   test-pr-contract-github-config '("--state=open"))))
      (shipit-pr--verify-contract :search result))))

(ert-deftest test/pr-github-fetch-files-contract ()
  ;; GIVEN canned file data from GitHub ETag API
  ;; WHEN fetching files via the GitHub backend
  ;; THEN result satisfies the :fetch-files contract
  (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
             (lambda (_endpoint &optional _params _token)
               (list :json (list test-pr-contract--github-file) :status 200))))
    (let ((result (shipit-pr-github--fetch-files
                   test-pr-contract-github-config 42)))
      (shipit-pr--verify-contract :fetch-files result))))

(ert-deftest test/pr-github-fetch-commits-contract ()
  ;; GIVEN canned commit data from GitHub API
  ;; WHEN fetching commits via the GitHub backend
  ;; THEN result satisfies the :fetch-commits contract
  (cl-letf (((symbol-function 'shipit--api-request-paginated)
             (lambda (_endpoint)
               (list test-pr-contract--github-commit))))
    (let ((result (shipit-pr-github--fetch-commits
                   test-pr-contract-github-config 42)))
      (shipit-pr--verify-contract :fetch-commits result))))

(ert-deftest test/pr-github-fetch-reviews-contract ()
  ;; GIVEN canned review data from GitHub API
  ;; WHEN fetching reviews via the GitHub backend
  ;; THEN result satisfies the :fetch-reviews contract
  (cl-letf (((symbol-function 'shipit--get-pr-reviews-for-comments)
             (lambda (_repo _number)
               (list test-pr-contract--github-review))))
    (let ((result (shipit-pr-github--fetch-reviews
                   test-pr-contract-github-config 42)))
      (shipit-pr--verify-contract :fetch-reviews result))))

(ert-deftest test/pr-github-fetch-checks-contract ()
  ;; GIVEN canned check run + status data from GitHub API
  ;; WHEN fetching checks via the GitHub backend
  ;; THEN result satisfies the :fetch-checks contract
  (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
             (lambda (_endpoint &optional _params _token)
               (list :json `((check_runs . ,(vector test-pr-contract--github-check-run)))
                     :status 200)))
            ((symbol-function 'shipit--api-request-paginated)
             (lambda (_endpoint) nil)))
    (let ((result (shipit-pr-github--fetch-checks
                   test-pr-contract-github-config "abc123")))
      (shipit-pr--verify-contract :fetch-checks result))))

;;; GitLab contract tests

(ert-deftest test/pr-gitlab-fetch-pr-contract ()
  ;; GIVEN canned MR data from GitLab API
  ;; WHEN fetching a PR via the GitLab backend
  ;; THEN result satisfies the :fetch-pr contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-pr-contract--gitlab-mr)))
    (let ((result (shipit-pr-gitlab--fetch-pr
                   test-pr-contract-gitlab-config 10)))
      (shipit-pr--verify-contract :fetch-pr result))))

(ert-deftest test/pr-gitlab-search-contract ()
  ;; GIVEN canned search results from GitLab API
  ;; WHEN searching MRs via the GitLab backend
  ;; THEN result satisfies the :search contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               (vector test-pr-contract--gitlab-mr))))
    (let ((result (shipit-pr-gitlab--search
                   test-pr-contract-gitlab-config '(:state "open"))))
      (shipit-pr--verify-contract :search result))))

(ert-deftest test/pr-gitlab-fetch-files-contract ()
  ;; GIVEN canned changes from GitLab API
  ;; WHEN fetching files via the GitLab backend
  ;; THEN result satisfies the :fetch-files contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               `((changes . ,(vector test-pr-contract--gitlab-change))))))
    (let ((result (shipit-pr-gitlab--fetch-files
                   test-pr-contract-gitlab-config 10)))
      (shipit-pr--verify-contract :fetch-files result))))

(ert-deftest test/pr-gitlab-fetch-commits-contract ()
  ;; GIVEN canned commits from GitLab API
  ;; WHEN fetching commits via the GitLab backend
  ;; THEN result satisfies the :fetch-commits contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               (vector test-pr-contract--gitlab-commit))))
    (let ((result (shipit-pr-gitlab--fetch-commits
                   test-pr-contract-gitlab-config 10)))
      (shipit-pr--verify-contract :fetch-commits result))))

(ert-deftest test/pr-gitlab-fetch-reviews-contract ()
  ;; GIVEN canned approvals from GitLab API
  ;; WHEN fetching reviews via the GitLab backend
  ;; THEN result satisfies the :fetch-reviews contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               '((approved_by . [((user . ((username . "approver"))))])))))
    (let ((result (shipit-pr-gitlab--fetch-reviews
                   test-pr-contract-gitlab-config 10)))
      (shipit-pr--verify-contract :fetch-reviews result))))

(ert-deftest test/pr-gitlab-fetch-checks-contract ()
  ;; GIVEN canned pipeline + jobs from GitLab API
  ;; WHEN fetching checks via the GitLab backend
  ;; THEN result satisfies the :fetch-checks contract
  (let ((call-count 0))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (setq call-count (1+ call-count))
                 (if (string-match "pipelines\\?" path)
                     ;; pipelines list
                     (vector '((id . 100)))
                   ;; jobs for pipeline
                   (vector test-pr-contract--gitlab-pipeline-job)))))
      (let ((result (shipit-pr-gitlab--fetch-checks
                     test-pr-contract-gitlab-config "def456")))
        (shipit-pr--verify-contract :fetch-checks result)))))

;;; File content caching tests (common dispatch layer)

(ert-deftest test/get-file-line-content-caches-not-found ()
  "GIVEN a backend whose :get-file-line-content returns nil (file not found)
WHEN shipit--get-file-line-content is called twice for the same path
THEN the backend should only be called once (negative result cached)."
  (let ((backend-call-count 0)
        (shipit--file-content-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'shipit-pr--get-backend)
               (lambda () (list :get-file-line-content
                                (lambda (_repo _ref _path &optional _ln)
                                  (setq backend-call-count (1+ backend-call-count))
                                  nil)))))
      ;; First call - should invoke backend
      (should-not (shipit--get-file-line-content
                   "owner/repo" "abc123" "deleted-file.py" 1))
      (should (= backend-call-count 1))
      ;; Second call - should NOT invoke backend (cached :not-found)
      (should-not (shipit--get-file-line-content
                   "owner/repo" "abc123" "deleted-file.py" 1))
      (should (= backend-call-count 1)))))

(ert-deftest test/get-file-line-content-caches-error ()
  "GIVEN a backend whose :get-file-line-content signals an error
WHEN shipit--get-file-line-content is called twice for the same path
THEN the backend should only be called once (error cached as :not-found)."
  (let ((backend-call-count 0)
        (shipit--file-content-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'shipit-pr--get-backend)
               (lambda () (list :get-file-line-content
                                (lambda (_repo _ref _path &optional _ln)
                                  (setq backend-call-count (1+ backend-call-count))
                                  (error "HTTP 404 Not Found"))))))
      ;; First call - should invoke backend and cache error
      (should-not (shipit--get-file-line-content
                   "owner/repo" "abc123" "deleted-file.py" 1))
      (should (= backend-call-count 1))
      ;; Second call - should NOT invoke backend (cached :not-found)
      (should-not (shipit--get-file-line-content
                   "owner/repo" "abc123" "deleted-file.py" 1))
      (should (= backend-call-count 1)))))

(ert-deftest test/get-file-line-content-caches-success ()
  "GIVEN a backend whose :get-file-line-content returns file content
WHEN shipit--get-file-line-content is called for different lines
THEN the backend should only be called once (content cached)."
  (let ((backend-call-count 0)
        (shipit--file-content-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'shipit-pr--get-backend)
               (lambda () (list :get-file-line-content
                                (lambda (_repo _ref _path &optional _ln)
                                  (setq backend-call-count (1+ backend-call-count))
                                  "line1\nline2\nline3\n")))))
      ;; First call - should invoke backend
      (should (equal "line2"
                     (shipit--get-file-line-content
                      "owner/repo" "abc123" "real-file.py" 2)))
      (should (= backend-call-count 1))
      ;; Second call for different line - should use cache
      (should (equal "line1"
                     (shipit--get-file-line-content
                      "owner/repo" "abc123" "real-file.py" 1)))
      (should (= backend-call-count 1)))))

(provide 'test-shipit-pr-contracts)
;;; test-shipit-pr-contracts.el ends here
