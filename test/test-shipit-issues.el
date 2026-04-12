;;; test-shipit-issues.el --- Tests for shipit-issues -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for GitHub Issues search and viewing functionality.

;;; Code:

(require 'ert)
(require 'shipit)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issues)
(require 'shipit-issues-buffer)

;;; Query Building Tests

(ert-deftest test-shipit-issues--build-search-query-basic ()
  "GIVEN no filter arguments
WHEN building issue search query
THEN query contains repo and is:issue but never is:pr."
  (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
             (lambda () "owner/repo")))
    (let ((query (shipit-issues--build-search-query nil "owner/repo")))
      (should (member "repo:owner/repo" query))
      (should (member "is:issue" query))
      (should-not (cl-find-if (lambda (p) (string-match-p "is:pr" p)) query)))))

(ert-deftest test-shipit-issues--build-search-query-state ()
  "GIVEN a --state=open argument
WHEN building issue search query
THEN query includes state:open."
  (let ((query (shipit-issues--build-search-query '("--state=open") "owner/repo")))
    (should (member "state:open" query))
    (should (member "is:issue" query))))

(ert-deftest test-shipit-issues--build-search-query-state-all ()
  "GIVEN --state=all argument
WHEN building issue search query
THEN query does not include any state filter."
  (let ((query (shipit-issues--build-search-query '("--state=all") "owner/repo")))
    (should-not (cl-find-if (lambda (p) (string-prefix-p "state:" p)) query))))

(ert-deftest test-shipit-issues--build-search-query-author ()
  "GIVEN an --author=someuser argument
WHEN building issue search query
THEN query includes author:someuser."
  (let ((query (shipit-issues--build-search-query '("--author=someuser") "owner/repo")))
    (should (member "author:someuser" query))))

(ert-deftest test-shipit-issues--build-search-query-assignee ()
  "GIVEN an --assignee=someuser argument
WHEN building issue search query
THEN query includes assignee:someuser."
  (let ((query (shipit-issues--build-search-query '("--assignee=someuser") "owner/repo")))
    (should (member "assignee:someuser" query))))

(ert-deftest test-shipit-issues--build-search-query-label ()
  "GIVEN a --label=bug argument
WHEN building issue search query
THEN query includes label:\"bug\"."
  (let ((query (shipit-issues--build-search-query '("--label=bug") "owner/repo")))
    (should (member "label:\"bug\"" query))))

(ert-deftest test-shipit-issues--build-search-query-milestone ()
  "GIVEN a --milestone=v1.0 argument
WHEN building issue search query
THEN query includes milestone:\"v1.0\"."
  (let ((query (shipit-issues--build-search-query '("--milestone=v1.0") "owner/repo")))
    (should (member "milestone:\"v1.0\"" query))))

(ert-deftest test-shipit-issues--build-search-query-mentions ()
  "GIVEN a --mentions=user argument
WHEN building issue search query
THEN query includes mentions:user."
  (let ((query (shipit-issues--build-search-query '("--mentions=user") "owner/repo")))
    (should (member "mentions:user" query))))

(ert-deftest test-shipit-issues--build-search-query-title ()
  "GIVEN a --title=something argument
WHEN building issue search query
THEN query includes 'something in:title'."
  (let ((query (shipit-issues--build-search-query '("--title=something") "owner/repo")))
    (should (member "something in:title" query))))

(ert-deftest test-shipit-issues--build-search-query-body ()
  "GIVEN a --body=content argument
WHEN building issue search query
THEN query includes 'content in:body'."
  (let ((query (shipit-issues--build-search-query '("--body=content") "owner/repo")))
    (should (member "content in:body" query))))

(ert-deftest test-shipit-issues--build-search-query-dates ()
  "GIVEN date range arguments
WHEN building issue search query
THEN query includes correct date filters."
  (let ((query (shipit-issues--build-search-query
                '("--created-after=2024-01-01" "--updated-before=2024-12-31")
                "owner/repo")))
    (should (member "created:>2024-01-01" query))
    (should (member "updated:<2024-12-31" query))))

(ert-deftest test-shipit-issues--build-search-query-no-pr-filters ()
  "GIVEN PR-only filters like --draft and --conflicts
WHEN building issue search query
THEN those filters are silently ignored."
  (let ((query (shipit-issues--build-search-query
                '("--draft" "--conflicts" "--reviewer=someone")
                "owner/repo")))
    (should-not (cl-find-if (lambda (p) (string-match-p "draft:" p)) query))
    (should-not (cl-find-if (lambda (p) (string-match-p "status:failure" p)) query))
    (should-not (cl-find-if (lambda (p) (string-match-p "review" p)) query))))

(ert-deftest test-shipit-issues--build-search-query-number ()
  "GIVEN a --number=42 argument
WHEN building issue search query
THEN query includes the number directly."
  (let ((query (shipit-issues--build-search-query '("--number=42") "owner/repo")))
    (should (member "42" query))))

(ert-deftest test-shipit-issues--build-search-query-multiple-filters ()
  "GIVEN multiple filter arguments
WHEN building issue search query
THEN all filters are included."
  (let ((query (shipit-issues--build-search-query
                '("--state=open" "--author=alice" "--label=enhancement")
                "owner/repo")))
    (should (member "state:open" query))
    (should (member "author:alice" query))
    (should (member "label:\"enhancement\"" query))
    (should (member "is:issue" query))))

;;; API Endpoint Tests

(ert-deftest test-shipit-issues--fetch-issue-endpoint ()
  "GIVEN a repo and issue number
WHEN fetching an issue
THEN the correct API endpoint is called."
  (let ((called-endpoint nil))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint &rest _args)
                 (setq called-endpoint endpoint)
                 (list :json '((number . 42) (title . "Test issue"))))))
      (shipit-issues--fetch-issue "owner/repo" 42)
      (should (string= called-endpoint "/repos/owner/repo/issues/42")))))

(ert-deftest test-shipit-issues--fetch-comments-endpoint ()
  "GIVEN a repo and issue number
WHEN fetching issue comments
THEN the correct API endpoint is called."
  (let ((called-endpoint nil))
    (cl-letf (((symbol-function 'shipit--api-request-paginated)
               (lambda (endpoint &rest _args)
                 (setq called-endpoint endpoint)
                 '())))
      (shipit-issues--fetch-comments "owner/repo" 42)
      (should (string= called-endpoint "/repos/owner/repo/issues/42/comments")))))

;;; Buffer Name Tests

(ert-deftest test-shipit-issue-buffer-name ()
  "GIVEN a repo and issue number
WHEN generating buffer name
THEN it returns *shipit-issue: owner/repo#42*."
  (should (string= (shipit-issue-buffer-name "owner/repo" 42)
                    "*shipit-issue: owner/repo#42*")))

(ert-deftest test-shipit-issue-buffer-name-different-numbers ()
  "GIVEN different repos and numbers
WHEN generating buffer names
THEN each name is unique."
  (should-not (string= (shipit-issue-buffer-name "owner/repo" 1)
                        (shipit-issue-buffer-name "owner/repo" 2)))
  (should-not (string= (shipit-issue-buffer-name "a/b" 1)
                        (shipit-issue-buffer-name "c/d" 1))))

;;; Buffer Creation Tests

(ert-deftest test-shipit-issues-open-buffer-creates-buffer ()
  "GIVEN an issue number and repo
WHEN opening issue buffer
THEN a buffer with the correct name is created in shipit-issue-mode."
  (cl-letf (((symbol-function 'shipit-issues--fetch-issue)
             (lambda (_repo _num)
               '((number . 42) (title . "Test") (state . "open")
                 (body . "Body text") (user . ((login . "testuser")))
                 (created_at . "2024-01-01T00:00:00Z")
                 (updated_at . "2024-01-02T00:00:00Z"))))
            ((symbol-function 'shipit-issues--fetch-comments-head-tail-async)
             (lambda (_repo _num _pp _hn _tn _cb) nil))
            ((symbol-function 'shipit--fetch-pr-reactions-sync)
             (lambda (_repo _num) '()))
            ((symbol-function 'shipit-issues--fetch-reactions)
             (lambda (_repo _num) nil))
            ((symbol-function 'shipit-issue--get-backend)
             (lambda () nil))
            ((symbol-function 'shipit--get-repo-root)
             (lambda () "/tmp"))
            ((symbol-function 'shipit--get-repo-from-remote)
             (lambda () "owner/repo"))
            ((symbol-function 'shipit-pr-github--get-repo-subscription)
             (lambda (_config) nil)))
    (let ((buf (get-buffer "*shipit-issue: owner/repo#42*")))
      (when buf (kill-buffer buf)))
    (unwind-protect
        (progn
          (shipit-issues-open-buffer 42 "owner/repo")
          (let ((buf (get-buffer "*shipit-issue: owner/repo#42*")))
            (should buf)
            (with-current-buffer buf
              (should (eq major-mode 'shipit-issue-mode)))))
      (let ((buf (get-buffer "*shipit-issue: owner/repo#42*")))
        (when buf (kill-buffer buf))))))

;;; Empty Results Handling Tests

(ert-deftest test-shipit-issues--select-from-results-empty ()
  "GIVEN empty search results
WHEN selecting from results
THEN a 'No issues found' message is shown."
  (let ((msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (shipit-issues--select-from-results nil "owner/repo")
      (should (string-match-p "No issues found" msg)))))

;;; Annotation Tests

(ert-deftest test-shipit-issues--annotate-open-state ()
  "GIVEN an issue completion candidate with open state
WHEN annotating
THEN shows Open state."
  (let ((shipit-issues--completion-table (make-hash-table :test 'equal)))
    (puthash "#42    Test issue"
             (list :number 42 :repo "owner/repo"
                   :data '((state . "open")
                           (user . ((login . "alice")))
                           (updated_at . "2024-01-01T00:00:00Z")))
             shipit-issues--completion-table)
    (let ((result (shipit-issues--annotate "#42    Test issue")))
      (should result)
      (should (string-match-p "Open" result))
      (should (string-match-p "@alice" result)))))

(ert-deftest test-shipit-issues--annotate-closed-state ()
  "GIVEN an issue completion candidate with closed state
WHEN annotating
THEN shows Closed state."
  (let ((shipit-issues--completion-table (make-hash-table :test 'equal)))
    (puthash "#10    Closed issue"
             (list :number 10 :repo "owner/repo"
                   :data '((state . "closed")
                           (user . ((login . "bob")))
                           (updated_at . "2024-06-15T00:00:00Z")))
             shipit-issues--completion-table)
    (let ((result (shipit-issues--annotate "#10    Closed issue")))
      (should result)
      (should (string-match-p "Closed" result)))))

;;; PR Reference Action Menu — DWIM Tests

(require 'shipit-render)

(ert-deftest test-shipit-reference-open-dwim-opens-pr-when-pr ()
  "GIVEN reference is a PR (has pull_request key)
WHEN user presses 'o' in the action menu
THEN shipit--open-pr-same-repo is called."
  (let ((shipit-issues-enabled t)
        (called nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?o))
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &optional _params _token _force)
                 (list :json '((number . 42) (pull_request . ((url . "x"))))
                       :status 200)))
              ((symbol-function 'shipit--open-pr-same-repo)
               (lambda (num _repo) (setq called (list 'pr num))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (num _repo) (setq called (list 'issue num)))))
      (shipit--pr-reference-action-menu 42 "owner/repo")
      (should (equal called '(pr 42))))))

(ert-deftest test-shipit-reference-open-dwim-opens-issue-when-issue ()
  "GIVEN reference is an issue (no pull_request key) and issues enabled
WHEN user presses 'o' in the action menu
THEN shipit-issues-open-buffer is called."
  (let ((shipit-issues-enabled t)
        (called nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?o))
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &optional _params _token _force)
                 (list :json '((number . 42) (title . "Bug"))
                       :status 200)))
              ((symbol-function 'shipit--open-pr-same-repo)
               (lambda (num _repo) (setq called (list 'pr num))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (num _repo &optional backend-id &rest _)
                 (setq called (list 'issue num backend-id)))))
      (shipit--pr-reference-action-menu 42 "owner/repo")
      (should (equal called '(issue 42 github))))))

(ert-deftest test-shipit-reference-open-dwim-falls-back-to-browser-without-issues ()
  "GIVEN reference is an issue but shipit-issues-enabled is nil
WHEN user presses 'o' in the action menu
THEN browser is opened."
  (let ((shipit-issues-enabled nil)
        (opened-url nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?o))
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &optional _params _token _force)
                 (list :json '((number . 42) (title . "Bug"))
                       :status 200)))
              ((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (shipit--pr-reference-action-menu 42 "owner/repo")
      (should (string-match-p "issues/42" opened-url)))))

(ert-deftest test-shipit-reference-preview-dwim-shows-pr-preview-when-pr ()
  "GIVEN reference is a PR
WHEN user presses 'p' in the action menu
THEN shipit--show-pr-preview is called."
  (let ((called nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?p))
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &optional _params _token _force)
                 (list :json '((number . 42) (pull_request . ((url . "x"))))
                       :status 200)))
              ((symbol-function 'shipit--show-pr-preview)
               (lambda (num _repo) (setq called (list 'pr-preview num))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (num _repo) (setq called (list 'issue num)))))
      (shipit--pr-reference-action-menu 42 "owner/repo")
      (should (equal called '(pr-preview 42))))))

(ert-deftest test-shipit-reference-preview-dwim-shows-issue-preview-when-issue ()
  "GIVEN reference is an issue and issues enabled
WHEN user presses 'p' in the action menu
THEN shipit--show-issue-preview is called."
  (let ((shipit-issues-enabled t)
        (called nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?p))
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &optional _params _token _force)
                 (list :json '((number . 42) (title . "Bug"))
                       :status 200)))
              ((symbol-function 'shipit--show-pr-preview)
               (lambda (num _repo) (setq called (list 'pr-preview num))))
              ((symbol-function 'shipit--show-issue-preview)
               (lambda (num _repo) (setq called (list 'issue-preview num)))))
      (shipit--pr-reference-action-menu 42 "owner/repo")
      (should (equal called '(issue-preview 42))))))

(ert-deftest test-shipit-show-issue-preview-fetches-and-renders ()
  "GIVEN an issue number and repo
WHEN shipit--show-issue-preview is called
THEN it fetches issue data and creates a preview frame."
  (let ((fetched-args nil)
        (frame-args nil))
    (cl-letf (((symbol-function 'shipit-issues--fetch-issue)
               (lambda (repo num)
                 (setq fetched-args (list repo num))
                 '((number . 42) (title . "Test bug")
                   (state . "open") (body . "Description")
                   (user . ((login . "alice")))
                   (created_at . "2024-01-01T00:00:00Z")
                   (updated_at . "2024-01-02T00:00:00Z"))))
              ((symbol-function 'shipit--create-issue-preview-frame)
               (lambda (num data)
                 (setq frame-args (list num (cdr (assq 'title data)))))))
      (shipit--show-issue-preview 42 "owner/repo")
      (should (equal fetched-args '("owner/repo" 42)))
      (should (equal frame-args '(42 "Test bug"))))))

(ert-deftest test-shipit-reference-open-dwim-skips-fetch-with-pr-type-hint ()
  "GIVEN type hint is 'pr (from URL-detected PR reference)
WHEN user presses 'o' in the action menu
THEN shipit--open-pr-same-repo is called without fetching."
  (let ((called nil)
        (fetched nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?o))
              ((symbol-function 'shipit-issues--fetch-issue)
               (lambda (_repo _num) (setq fetched t) nil))
              ((symbol-function 'shipit--open-pr-same-repo)
               (lambda (num _repo) (setq called (list 'pr num)))))
      (shipit--pr-reference-action-menu 42 "owner/repo" 'pr)
      (should (equal called '(pr 42)))
      (should-not fetched))))

(ert-deftest test-shipit-reference-issues-url-detects-pr-via-api ()
  "GIVEN no type hint (from /issues/ URL which could be a PR)
WHEN user presses 'o' in the action menu
THEN GitHub API fetch detects pull_request key and opens as PR."
  (let ((called nil)
        (fetched nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?o))
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &optional _params _token _force)
                 (setq fetched t)
                 (list :json '((number . 42) (title . "Some PR")
                               (pull_request . ((url . "https://api.github.com/repos/owner/repo/pulls/42"))))
                       :status 200)))
              ((symbol-function 'shipit--open-pr-same-repo)
               (lambda (num _repo) (setq called (list 'pr num)))))
      (shipit--pr-reference-action-menu 42 "owner/repo")
      (should fetched)
      (should (equal called '(pr 42))))))

(ert-deftest test-shipit-reference-issues-url-detects-issue-via-api ()
  "GIVEN no type hint (from /issues/ URL which is an actual issue)
WHEN user presses 'o' in the action menu
THEN GitHub API fetch finds no pull_request key and opens as issue."
  (let ((shipit-issues-enabled t)
        (called nil)
        (fetched nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?o))
              ((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (_endpoint &optional _params _token _force)
                 (setq fetched t)
                 (list :json '((number . 42) (title . "Some issue"))
                       :status 200)))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (num _repo &optional backend-id &rest _)
                 (setq called (list 'issue num backend-id)))))
      (shipit--pr-reference-action-menu 42 "owner/repo")
      (should fetched)
      (should (equal called '(issue 42 github))))))

;;; Candidate Sort Tests

(ert-deftest test-shipit-issues--build-candidates-sorts-jira-numerically ()
  "GIVEN Jira issues with string keys like PROJ-719, PROJ-8019, PROJ-12112
WHEN building candidates
THEN they are sorted by numeric suffix descending."
  (let* ((issues '(((number . "PROJ-719") (title . "Old issue"))
                   ((number . "PROJ-12112") (title . "New issue"))
                   ((number . "PROJ-8019") (title . "Mid issue"))))
         (lookup (make-hash-table :test 'equal))
         (candidates (shipit-issues--build-candidates issues "repo" lookup)))
    (should (string-prefix-p "PROJ-12112" (nth 0 candidates)))
    (should (string-prefix-p "PROJ-8019" (nth 1 candidates)))
    (should (string-prefix-p "PROJ-719" (nth 2 candidates)))))

(ert-deftest test-shipit-issues--display-sort-jira-numerically ()
  "GIVEN candidate strings with Jira keys in arbitrary order
WHEN the display-sort-function sorts them
THEN they are ordered by numeric suffix descending."
  (let* ((candidates '("PROJ-719  Old issue"
                        "PROJ-12112 New issue"
                        "PROJ-8019 Mid issue"))
         (sorted (shipit-issues--sort-candidates candidates)))
    (should (string-prefix-p "PROJ-12112" (nth 0 sorted)))
    (should (string-prefix-p "PROJ-8019" (nth 1 sorted)))
    (should (string-prefix-p "PROJ-719" (nth 2 sorted)))))

;;; Dynamic Fetch Tests

(ert-deftest test-shipit-issues--dynamic-fetch-jira-key ()
  "GIVEN input matching a Jira key like PRJ-42
WHEN calling dynamic-fetch
THEN :fetch-issue is called with the string key, not :search."
  (let* ((fetch-called nil)
         (search-called nil)
         (backend (list :fetch-issue (lambda (_config id)
                                       (setq fetch-called id)
                                       '((number . "PRJ-42") (title . "Test")))
                        :search (lambda (_config _args)
                                  (setq search-called t)
                                  nil)))
         (config '(:project-keys ("PRJ"))))
    (let ((results (shipit-issues--dynamic-fetch backend config "PRJ-42")))
      (should (equal fetch-called "PRJ-42"))
      (should-not search-called)
      (should (= 1 (length results))))))

(ert-deftest test-shipit-issues--dynamic-fetch-github-number ()
  "GIVEN input matching a GitHub issue number like #123
WHEN calling dynamic-fetch
THEN :fetch-issue is called with numeric id."
  (let* ((fetch-called nil)
         (backend (list :fetch-issue (lambda (_config id)
                                       (setq fetch-called id)
                                       '((number . 123) (title . "Test")))
                        :search (lambda (_config _args) nil)))
         (config '(:repo "owner/repo")))
    (let ((results (shipit-issues--dynamic-fetch backend config "#123")))
      (should (equal fetch-called 123))
      (should (= 1 (length results))))))

(ert-deftest test-shipit-issues--dynamic-fetch-bare-digits-github ()
  "GIVEN bare digit input like 42 with a GitHub-style config (no project-keys)
WHEN calling dynamic-fetch
THEN :fetch-issue is called with numeric id."
  (let* ((fetch-called nil)
         (backend (list :fetch-issue (lambda (_config id)
                                       (setq fetch-called id)
                                       '((number . 42) (title . "Test")))
                        :search (lambda (_config _args) nil)))
         (config '(:repo "owner/repo")))
    (let ((results (shipit-issues--dynamic-fetch backend config "42")))
      (should (equal fetch-called 42))
      (should (= 1 (length results))))))

(ert-deftest test-shipit-issues--dynamic-fetch-bare-digits-jira ()
  "GIVEN bare digit input like 42 with a Jira config containing project-keys
WHEN calling dynamic-fetch
THEN :fetch-issue is called with constructed key PRJ-42."
  (let* ((fetch-called nil)
         (backend (list :fetch-issue (lambda (_config id)
                                       (setq fetch-called id)
                                       '((number . "PRJ-42") (title . "Test")))
                        :search (lambda (_config _args) nil)))
         (config '(:project-keys ("PRJ"))))
    (let ((results (shipit-issues--dynamic-fetch backend config "42")))
      (should (equal fetch-called "PRJ-42"))
      (should (= 1 (length results))))))

(ert-deftest test-shipit-issues--dynamic-fetch-plain-text ()
  "GIVEN plain text input like 'login bug'
WHEN calling dynamic-fetch
THEN :search is called with --title= args, not :fetch-issue."
  (let* ((fetch-called nil)
         (search-args nil)
         (backend (list :fetch-issue (lambda (_config _id)
                                       (setq fetch-called t)
                                       nil)
                        :search (lambda (_config args)
                                  (setq search-args args)
                                  '(((number . 1) (title . "login bug fix"))))))
         (config '(:repo "owner/repo")))
    (let ((results (shipit-issues--dynamic-fetch backend config "login bug")))
      (should-not fetch-called)
      (should (member "--title=login bug" search-args))
      (should (= 1 (length results))))))

;;; Match String Normalization Tests

(ert-deftest test-shipit-issues--match-string-bare-digits-github ()
  "GIVEN bare digit input and GitHub config (no project-keys)
WHEN normalizing for candidate matching
THEN digits are prefixed with #."
  (should (equal "#42" (shipit-issues--match-string "42" nil))))

(ert-deftest test-shipit-issues--match-string-bare-digits-jira ()
  "GIVEN bare digit input and Jira config with project-keys
WHEN normalizing for candidate matching
THEN digits are prefixed with project key."
  (should (equal "PRJ-42" (shipit-issues--match-string "42" '(:project-keys ("PRJ"))))))

(ert-deftest test-shipit-issues--match-string-hash-number ()
  "GIVEN #-prefixed number input
WHEN normalizing for candidate matching
THEN input is returned as-is."
  (should (equal "#123" (shipit-issues--match-string "#123" nil))))

(ert-deftest test-shipit-issues--match-string-plain-text ()
  "GIVEN plain text input
WHEN normalizing for candidate matching
THEN input is returned as-is."
  (should (equal "login bug" (shipit-issues--match-string "login bug" nil))))

;;; Dynamic Search Args Tests

(ert-deftest test-shipit-issues--dynamic-search-args-jira-key ()
  "GIVEN input matching a Jira key like PRJ-42
WHEN building dynamic search args
THEN args contain --number=PRJ-42."
  (let ((args (shipit-issues--dynamic-search-args "PRJ-42")))
    (should (member "--number=PRJ-42" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--dynamic-search-args-github-number ()
  "GIVEN input matching a GitHub issue number like #123
WHEN building dynamic search args
THEN args contain --number=123."
  (let ((args (shipit-issues--dynamic-search-args "#123")))
    (should (member "--number=123" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--dynamic-search-args-plain-text ()
  "GIVEN plain text input like 'login bug'
WHEN building dynamic search args
THEN args contain --title=login bug."
  (let ((args (shipit-issues--dynamic-search-args "login bug")))
    (should (member "--title=login bug" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--dynamic-search-args-bare-digits ()
  "GIVEN bare digit input like 42
WHEN building dynamic search args
THEN args contain --number=42."
  (let ((args (shipit-issues--dynamic-search-args "42")))
    (should (member "--number=42" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--dynamic-search-args-embedded-digits-end ()
  "GIVEN text with digits at the end like 'foo bar 1234'
WHEN building dynamic search args
THEN args contain --number=1234."
  (let ((args (shipit-issues--dynamic-search-args "foo bar 1234")))
    (should (member "--number=1234" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--dynamic-search-args-embedded-digits-start ()
  "GIVEN text with digits at the start like '1234 foo bar'
WHEN building dynamic search args
THEN args contain --number=1234."
  (let ((args (shipit-issues--dynamic-search-args "1234 foo bar")))
    (should (member "--number=1234" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--dynamic-search-args-embedded-jira-key ()
  "GIVEN text with embedded Jira key like 'fix PRJ-42 bug'
WHEN building dynamic search args
THEN args contain --number=PRJ-42."
  (let ((args (shipit-issues--dynamic-search-args "fix PRJ-42 bug")))
    (should (member "--number=PRJ-42" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--dynamic-search-args-embedded-hash-number ()
  "GIVEN text with embedded #number like 'fix #42 bug'
WHEN building dynamic search args
THEN args contain --number=42."
  (let ((args (shipit-issues--dynamic-search-args "fix #42 bug")))
    (should (member "--number=42" args))
    (should (member "--limit=50" args))))

(ert-deftest test-shipit-issues--match-string-embedded-digits-jira ()
  "GIVEN text with embedded digits and Jira config
WHEN normalizing for candidate matching
THEN returns the constructed Jira key."
  (should (equal "PRJ-1234"
                  (shipit-issues--match-string "foo bar 1234"
                                               '(:project-keys ("PRJ"))))))

(ert-deftest test-shipit-issues--match-string-embedded-digits-github ()
  "GIVEN text with embedded digits and GitHub config
WHEN normalizing for candidate matching
THEN returns #number."
  (should (equal "#1234"
                  (shipit-issues--match-string "foo bar 1234" nil))))

;;; Candidate Sort Tests

(ert-deftest test-shipit-issues--candidate-sort-key-github ()
  "GIVEN a GitHub candidate string like '#42 Fix login'
WHEN extracting the sort key
THEN returns the numeric issue number."
  (should (equal 42 (shipit-issues--candidate-sort-key "#42 Fix login")))
  (should (equal 8019 (shipit-issues--candidate-sort-key "#8019 Add feature"))))

(ert-deftest test-shipit-issues--candidate-sort-key-jira ()
  "GIVEN a Jira candidate string like 'PRJ-42 Fix login'
WHEN extracting the sort key
THEN returns the numeric suffix."
  (should (equal 42 (shipit-issues--candidate-sort-key "PRJ-42 Fix login")))
  (should (equal 12112 (shipit-issues--candidate-sort-key "PROJ-12112 Something"))))

(ert-deftest test-shipit-issues--candidate-sort-key-unknown ()
  "GIVEN a candidate string with no recognizable issue ID
WHEN extracting the sort key
THEN returns 0."
  (should (equal 0 (shipit-issues--candidate-sort-key "some random text"))))

(ert-deftest test-shipit-issues--sort-candidates-jira-numeric ()
  "GIVEN Jira candidates with numeric suffixes of varying lengths
WHEN sorting via display-sort-function
THEN candidates are sorted by numeric suffix descending."
  (let ((candidates '("PROJ-719 Old issue"
                      "PROJ-8019 Medium issue"
                      "PROJ-12112 Recent issue"
                      "PROJ-12099 Another recent")))
    (should (equal '("PROJ-12112 Recent issue"
                     "PROJ-12099 Another recent"
                     "PROJ-8019 Medium issue"
                     "PROJ-719 Old issue")
                   (shipit-issues--sort-candidates candidates)))))

(ert-deftest test-shipit-issues--sort-candidates-github-numeric ()
  "GIVEN GitHub candidates with varying issue numbers
WHEN sorting via display-sort-function
THEN candidates are sorted by issue number descending."
  (let ((candidates '("#42 Old issue"
                      "#100 Medium issue"
                      "#1234 Recent issue")))
    (should (equal '("#1234 Recent issue"
                     "#100 Medium issue"
                     "#42 Old issue")
                   (shipit-issues--sort-candidates candidates)))))

;;; Repo Reader Tests

(ert-deftest test-shipit-issues--read-repo-combines-sources ()
  "GIVEN a current repo, subscribed repos, and repo-backends entries
WHEN calling shipit-issues--read-repo
THEN all sources are combined and deduplicated."
  (let ((shipit-issue-subscribed-repos '("org/frontend" "org/backend"))
        (shipit-issue-repo-backends '(("org/infra" :backend jira)
                                      ("org/frontend" :backend github)))
        (shipit-issues--watched-repos-cache nil)
        (shipit-issues--watched-repos-time (current-time))
        (selected nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "org/current"))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq selected candidates)
                 "org/current")))
      (shipit-issues--read-repo "Repo: " nil nil)
      ;; All four unique repos should be present
      (should (member "org/current" selected))
      (should (member "org/frontend" selected))
      (should (member "org/backend" selected))
      (should (member "org/infra" selected))
      ;; No duplicates (org/frontend appears in both subscribed and repo-backends)
      (should (= 4 (length selected))))))

(ert-deftest test-shipit-issues--read-repo-includes-watched-repos ()
  "GIVEN GitHub API returns watched repos
WHEN calling shipit-issues--read-repo
THEN watched repos appear in candidates."
  (let ((shipit-issue-subscribed-repos nil)
        (shipit-issue-repo-backends nil)
        (shipit-issues--watched-repos-cache nil)
        (shipit-issues--watched-repos-time nil)
        (selected nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "org/current"))
              ((symbol-function 'shipit-gh-etag-get-json-paginated)
               (lambda (_endpoint &rest _args)
                 (list :status 200
                       :json '(((full_name . "org/watched-a"))
                               ((full_name . "org/watched-b"))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq selected candidates)
                 "org/current")))
      (shipit-issues--read-repo "Repo: " nil nil)
      (should (member "org/current" selected))
      (should (member "org/watched-a" selected))
      (should (member "org/watched-b" selected))
      (should (= 3 (length selected))))))

(ert-deftest test-shipit-issues--read-repo-caches-watched-repos ()
  "GIVEN watched repos were fetched recently
WHEN calling shipit-issues--read-repo again
THEN the API is not called a second time."
  (let ((shipit-issue-subscribed-repos nil)
        (shipit-issue-repo-backends nil)
        (shipit-issues--watched-repos-cache '("org/cached"))
        (shipit-issues--watched-repos-time (current-time))
        (api-called nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () nil))
              ((symbol-function 'shipit-gh-etag-get-json-paginated)
               (lambda (_endpoint &rest _args)
                 (setq api-called t)
                 nil))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (car candidates))))
      (shipit-issues--read-repo "Repo: " nil nil)
      (should-not api-called))))

(ert-deftest test-shipit-issues--read-repo-excludes-regexp-keys ()
  "GIVEN repo-backends with regexp keys like \"org/.*\"
WHEN calling shipit-issues--read-repo
THEN regexp keys are excluded from candidates."
  (let ((shipit-issue-subscribed-repos nil)
        (shipit-issue-repo-backends '(("org/.*" :backend jira)
                                      ("org/literal" :backend github)))
        (shipit-issues--watched-repos-cache nil)
        (shipit-issues--watched-repos-time (current-time))
        (selected nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq selected candidates)
                 "org/literal")))
      (shipit-issues--read-repo "Repo: " nil nil)
      (should (member "org/literal" selected))
      (should-not (member "org/.*" selected))
      (should (= 1 (length selected))))))

(ert-deftest test-shipit-issues--read-repo-without-current-repo ()
  "GIVEN no current git repo but subscribed repos exist
WHEN calling shipit-issues--read-repo
THEN only subscribed repos and repo-backends are offered."
  (let ((shipit-issue-subscribed-repos '("org/frontend"))
        (shipit-issue-repo-backends '(("org/infra" :backend jira)))
        (shipit-issues--watched-repos-cache nil)
        (shipit-issues--watched-repos-time (current-time))
        (selected nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq selected candidates)
                 "org/frontend")))
      (shipit-issues--read-repo "Repo: " nil nil)
      (should (member "org/frontend" selected))
      (should (member "org/infra" selected))
      (should (= 2 (length selected))))))

;;; Advanced Search Repo Arg Tests

(ert-deftest test-shipit-issues--execute-advanced-search-extracts-repo-arg ()
  "GIVEN args including --repo=org/other
WHEN executing advanced search
THEN search uses org/other and strips --repo= from args."
  (let ((search-repo nil)
        (search-args nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "org/current"))
              ((symbol-function 'shipit-issues--display-search-results)
               (lambda (repo args)
                 (setq search-repo repo
                       search-args args))))
      (shipit-issues--execute-advanced-search
       '("--repo=org/other" "--state=open" "--label=bug"))
      (should (string= "org/other" search-repo))
      ;; --repo= should be stripped from search args
      (should-not (cl-find-if (lambda (a) (string-prefix-p "--repo=" a)) search-args))
      ;; Other args should be preserved
      (should (member "--state=open" search-args))
      (should (member "--label=bug" search-args)))))

(ert-deftest test-shipit-issues--execute-advanced-search-falls-back-to-remote ()
  "GIVEN args without --repo=
WHEN executing advanced search
THEN search falls back to shipit--get-repo-from-remote."
  (let ((search-repo nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "org/detected"))
              ((symbol-function 'shipit-issues--display-search-results)
               (lambda (repo _args)
                 (setq search-repo repo))))
      (shipit-issues--execute-advanced-search '("--state=open"))
      (should (string= "org/detected" search-repo)))))

(ert-deftest test-shipit-issues--execute-advanced-search-no-repo-message ()
  "GIVEN no --repo= arg and no git remote
WHEN executing advanced search
THEN a message is shown about missing repo."
  (let ((msg nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (shipit-issues--execute-advanced-search '("--state=open"))
      (should (string-match-p "No repo" msg)))))

;;; Issue Buffer Reactions Guard Tests

(ert-deftest test-shipit-issue-buffer-reactions-guard-uses-capability-check ()
  "GIVEN a backend with :fetch-reactions
WHEN opening an issue buffer
THEN reactions are fetched (not skipped by a hard-coded backend check)."
  (let ((reactions-fetched nil))
    (cl-letf (((symbol-function 'shipit-issues--fetch-issue)
               (lambda (_repo _num)
                 '((number . 42) (title . "Test") (state . "open")
                   (body . "Body text") (user . ((login . "testuser")))
                   (created_at . "2024-01-01T00:00:00Z")
                   (updated_at . "2024-01-02T00:00:00Z"))))
              ((symbol-function 'shipit-issues--fetch-comments-head-tail-async)
               (lambda (_repo _num _pp _hn _tn _cb) nil))
              ((symbol-function 'shipit-issues--fetch-reactions)
               (lambda (_repo _num)
                 (setq reactions-fetched t)
                 '()))
              ((symbol-function 'shipit-issue--get-backend)
               (lambda ()
                 (list :name "Test" :fetch-reactions #'ignore)))
              ((symbol-function 'shipit--repo-get-subscription)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo) (cons (list :name "Test") '(:repo "owner/repo"))))
              ((symbol-function 'shipit-issues--fetch-pinned-comment-async)
               (lambda (_repo _num cb) (funcall cb nil)))
              ((symbol-function 'shipit-issue--backend-has-reactions-p)
               (lambda (backend) (not (null (plist-get backend :fetch-reactions)))))
              ((symbol-function 'shipit--get-repo-root)
               (lambda () "/tmp"))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-pr-github--get-repo-subscription)
               (lambda (_config) nil)))
      (let ((buf (get-buffer "*shipit-issue: owner/repo#99*")))
        (when buf (kill-buffer buf)))
      (unwind-protect
          (progn
            (shipit-issues-open-buffer 99 "owner/repo")
            (should reactions-fetched))
        (let ((buf (get-buffer "*shipit-issue: owner/repo#99*")))
          (when buf (kill-buffer buf)))))))

(ert-deftest test-shipit-issue-buffer-reactions-skipped-without-capability ()
  "GIVEN a backend without :fetch-reactions
WHEN opening an issue buffer
THEN reactions fetch is skipped."
  (let ((reactions-fetched nil))
    (cl-letf (((symbol-function 'shipit-issues--fetch-issue)
               (lambda (_repo _num)
                 '((number . 42) (title . "Test") (state . "open")
                   (body . "Body text") (user . ((login . "testuser")))
                   (created_at . "2024-01-01T00:00:00Z")
                   (updated_at . "2024-01-02T00:00:00Z"))))
              ((symbol-function 'shipit-issues--fetch-comments-head-tail-async)
               (lambda (_repo _num _pp _hn _tn _cb) nil))
              ((symbol-function 'shipit-issues--fetch-reactions)
               (lambda (_repo _num)
                 (setq reactions-fetched t)
                 '()))
              ((symbol-function 'shipit-issue--get-backend)
               (lambda ()
                 (list :name "Jira")))
              ((symbol-function 'shipit--repo-get-subscription)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo) (cons (list :name "Test") '(:repo "owner/repo"))))
              ((symbol-function 'shipit-issues--fetch-pinned-comment-async)
               (lambda (_repo _num cb) (funcall cb nil)))
              ((symbol-function 'shipit-issue--backend-has-reactions-p)
               (lambda (backend) (not (null (plist-get backend :fetch-reactions)))))
              ((symbol-function 'shipit--get-repo-root)
               (lambda () "/tmp"))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (let ((buf (get-buffer "*shipit-issue: owner/repo#98*")))
        (when buf (kill-buffer buf)))
      (unwind-protect
          (progn
            (shipit-issues-open-buffer 98 "owner/repo")
            (should-not reactions-fetched))
        (let ((buf (get-buffer "*shipit-issue: owner/repo#98*")))
          (when buf (kill-buffer buf)))))))

(ert-deftest test-shipit-get-pr-reactions-returns-cached-without-token ()
  "GIVEN reactions are in the cache
AND shipit-github-token is nil (e.g. GitLab issue context)
WHEN shipit--get-pr-reactions is called
THEN cached reactions are returned (not nil)."
  (require 'shipit-http)
  (let ((shipit--reaction-cache (make-hash-table :test 'equal))
        (shipit-github-token nil)
        (shipit-current-repo nil))
    (puthash "pr-42" '(((content . "+1") (user . ((login . "alice")))))
             shipit--reaction-cache)
    (let ((result (shipit--get-pr-reactions 42)))
      (should result)
      (should (= 1 (length result)))
      (should (equal "+1" (cdr (assq 'content (car result))))))))

(provide 'test-shipit-issues)
;;; test-shipit-issues.el ends here
