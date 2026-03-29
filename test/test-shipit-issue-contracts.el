;;; test-shipit-issue-contracts.el --- Contract tests for issue backends -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies that GitHub, GitLab, and Jira issue backends return data
;; satisfying the output contracts defined in shipit-issue-backends.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-gitlab)
(require 'shipit-issue-jira)

;;; Test helpers

(defvar test-issue-github-config '(:repo "owner/repo")
  "Minimal GitHub config for issue contract tests.")

(defvar test-issue-gitlab-config
  '(:repo "group/project" :project-path "group/project"
    :api-url "https://gitlab.example.com")
  "Minimal GitLab config for issue contract tests.")

(defvar test-issue-jira-config
  '(:base-url "https://jira.example.com" :project-keys ("PRJ"))
  "Minimal Jira config for issue contract tests.")

;;; Canned API responses — GitHub

(defconst test-issue-contract--github-issue
  '((number . 99)
    (title . "Bug report")
    (state . "open")
    (body . "Something broke")
    (user . ((login . "reporter") (avatar_url . "https://example.com/a.png")))
    (html_url . "https://github.com/owner/repo/issues/99")
    (created_at . "2025-01-01T00:00:00Z")
    (updated_at . "2025-01-01T12:00:00Z"))
  "Canned GitHub issue response.")

(defconst test-issue-contract--github-comment
  '((id . 7001)
    (body . "A comment")
    (user . ((login . "commenter") (avatar_url . "https://example.com/b.png")))
    (created_at . "2025-01-02T00:00:00Z")
    (updated_at . "2025-01-02T00:00:00Z"))
  "Canned GitHub issue comment.")

;;; Canned API responses — GitLab

(defconst test-issue-contract--gitlab-issue
  '((iid . 55)
    (title . "Feature request")
    (state . "opened")
    (description . "Please add X")
    (author . ((username . "gluser") (avatar_url . "https://gl.example.com/a.png")))
    (web_url . "https://gitlab.example.com/group/project/-/issues/55")
    (created_at . "2025-02-01T00:00:00Z")
    (updated_at . "2025-02-01T12:00:00Z")
    (labels . [])
    (assignees . []))
  "Canned GitLab issue response.")

(defconst test-issue-contract--gitlab-note
  '((id . 8001)
    (body . "A note")
    (author . ((username . "glcommenter") (avatar_url . "https://gl.example.com/b.png")))
    (created_at . "2025-02-02T00:00:00Z")
    (updated_at . "2025-02-02T00:00:00Z")
    (system . :json-false))
  "Canned GitLab issue note.")

;;; Canned API responses — Jira

(defconst test-issue-contract--jira-issue
  '((key . "PRJ-42")
    (fields . ((summary . "Task to do")
               (status . ((name . "Open")))
               (description . "Do the thing")
               (reporter . ((displayName . "Jira User")))
               (assignee . nil)
               (labels . [])
               (created . "2025-03-01T00:00:00.000+0000")
               (updated . "2025-03-01T12:00:00.000+0000")
               (comment . ((comments . []))))))
  "Canned Jira issue response.")

(defconst test-issue-contract--jira-comment
  '((id . "10001")
    (body . "A Jira comment")
    (author . ((displayName . "Jira Commenter")))
    (created . "2025-03-02T00:00:00.000+0000")
    (updated . "2025-03-02T00:00:00.000+0000"))
  "Canned Jira comment response.")

(defconst test-issue-contract--jira-search-response
  `((issues . ,(vector test-issue-contract--jira-issue))
    (total . 1))
  "Canned Jira search response.")

;;; GitHub contract tests

(ert-deftest test/issue-github-fetch-issue-contract ()
  ;; GIVEN canned issue data from GitHub ETag API
  ;; WHEN fetching an issue via the GitHub backend
  ;; THEN result satisfies the :fetch-issue contract
  (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
             (lambda (_endpoint &optional _params _token)
               (list :json test-issue-contract--github-issue :status 200)))
            ((symbol-function 'shipit--api-request-paginated)
             (lambda (_endpoint) nil)))
    (let ((result (shipit-issue-github--fetch-issue
                   test-issue-github-config 99)))
      (shipit-issue--verify-contract :fetch-issue result))))

(ert-deftest test/issue-github-fetch-comments-contract ()
  ;; GIVEN canned comments from GitHub API
  ;; WHEN fetching comments via the GitHub backend
  ;; THEN result satisfies the :fetch-comments contract
  (cl-letf (((symbol-function 'shipit--api-request-paginated)
             (lambda (_endpoint)
               (list test-issue-contract--github-comment))))
    (let ((result (shipit-issue-github--fetch-comments
                   test-issue-github-config 99)))
      (shipit-issue--verify-contract :fetch-comments result))))

(ert-deftest test/issue-github-search-contract ()
  ;; GIVEN canned search results
  ;; WHEN searching issues via the GitHub backend
  ;; THEN result satisfies the :search contract
  (cl-letf (((symbol-function 'shipit-issues--build-search-query)
             (lambda (_args _repo) "is:issue"))
            ((symbol-function 'shipit--extract-limit-from-args)
             (lambda (_args) 50))
            ((symbol-function 'shipit--extract-sort-from-args)
             (lambda (_args) nil))
            ((symbol-function 'shipit--search-prs-with-encoded-query)
             (lambda (_repo _query _per-page _limit _sort)
               (list test-issue-contract--github-issue))))
    (let ((result (shipit-issue-github--search
                   test-issue-github-config '("--state=open"))))
      (shipit-issue--verify-contract :search result))))

(ert-deftest test/issue-github-create-issue-contract ()
  ;; GIVEN a mock POST that returns a created issue
  ;; WHEN creating an issue via the GitHub backend
  ;; THEN result satisfies the :create-issue contract
  (cl-letf (((symbol-function 'shipit--api-request-post)
             (lambda (_endpoint _data &optional _method)
               test-issue-contract--github-issue)))
    (let ((result (shipit-issue-github--create-issue
                   test-issue-github-config "Bug report" "Something broke")))
      (shipit-issue--verify-contract :create-issue result))))

;;; GitLab contract tests

(ert-deftest test/issue-gitlab-fetch-issue-contract ()
  ;; GIVEN canned issue data from GitLab API
  ;; WHEN fetching an issue via the GitLab backend
  ;; THEN result satisfies the :fetch-issue contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               test-issue-contract--gitlab-issue)))
    (let ((result (shipit-issue-gitlab--fetch-issue
                   test-issue-gitlab-config 55)))
      (shipit-issue--verify-contract :fetch-issue result))))

(ert-deftest test/issue-gitlab-fetch-comments-contract ()
  ;; GIVEN canned notes from GitLab API
  ;; WHEN fetching comments via the GitLab backend
  ;; THEN result satisfies the :fetch-comments contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               (list test-issue-contract--gitlab-note))))
    (let ((result (shipit-issue-gitlab--fetch-comments
                   test-issue-gitlab-config 55)))
      (shipit-issue--verify-contract :fetch-comments result))))

(ert-deftest test/issue-gitlab-search-contract ()
  ;; GIVEN canned search results from GitLab API
  ;; WHEN searching issues via the GitLab backend
  ;; THEN result satisfies the :search contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               (list test-issue-contract--gitlab-issue))))
    (let ((result (shipit-issue-gitlab--search
                   test-issue-gitlab-config '("--state=open"))))
      (shipit-issue--verify-contract :search result))))

(ert-deftest test/issue-gitlab-create-issue-contract ()
  ;; GIVEN a mock POST that returns a created issue
  ;; WHEN creating an issue via the GitLab backend
  ;; THEN result satisfies the :create-issue contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-issue-contract--gitlab-issue)))
    (let ((result (shipit-issue-gitlab--create-issue
                   test-issue-gitlab-config "Feature request" "Please add X")))
      (shipit-issue--verify-contract :create-issue result))))

;;; Jira contract tests

(ert-deftest test/issue-jira-fetch-issue-contract ()
  ;; GIVEN canned issue data from Jira API
  ;; WHEN fetching an issue via the Jira backend
  ;; THEN result satisfies the :fetch-issue contract
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config _path)
               test-issue-contract--jira-issue)))
    (let ((result (shipit-issue-jira--fetch-issue
                   test-issue-jira-config "PRJ-42")))
      (shipit-issue--verify-contract :fetch-issue result))))

(ert-deftest test/issue-jira-fetch-comments-contract ()
  ;; GIVEN canned comments from Jira API
  ;; WHEN fetching comments via the Jira backend
  ;; THEN result satisfies the :fetch-comments contract
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config _path)
               `((comments . ,(vector test-issue-contract--jira-comment))))))
    (let ((result (shipit-issue-jira--fetch-comments
                   test-issue-jira-config "PRJ-42")))
      (shipit-issue--verify-contract :fetch-comments result))))

(ert-deftest test/issue-jira-search-contract ()
  ;; GIVEN canned search results from Jira API
  ;; WHEN searching issues via the Jira backend
  ;; THEN result satisfies the :search contract
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config _path)
               test-issue-contract--jira-search-response)))
    (let ((result (shipit-issue-jira--search
                   test-issue-jira-config '("--state=open"))))
      (shipit-issue--verify-contract :search result))))

(ert-deftest test/issue-jira-create-issue-contract ()
  ;; GIVEN mocked Jira POST (create) and GET (re-fetch)
  ;; WHEN creating an issue via the Jira backend
  ;; THEN result satisfies the :create-issue contract
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
             (lambda (_config _path _data)
               '((key . "PRJ-99"))))
            ((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config _path)
               test-issue-contract--jira-issue)))
    (let ((result (shipit-issue-jira--create-issue
                   test-issue-jira-config "Task to do" "Do the thing")))
      (shipit-issue--verify-contract :create-issue result))))

(provide 'test-shipit-issue-contracts)
;;; test-shipit-issue-contracts.el ends here
