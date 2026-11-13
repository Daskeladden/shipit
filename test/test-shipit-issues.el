;;; test-shipit-issues.el --- Tests for shipit-issues -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for GitHub Issues search and viewing functionality.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'shipit)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-gitlab)
(require 'shipit-issue-jira)
(require 'shipit-issues)
(require 'shipit-issues-buffer)
(require 'shipit-test-http)

(defconst test-shipit-issues--watching-nothing
  '(("/user/subscriptions" 200 []))
  "GitHub's answer when the user watches no repository.")

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
             (lambda () (list :id-to-string #'shipit-issue-github--id-to-string)))
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
          (shipit-test-http-answering (list shipit-test-http-not-starred)
            (shipit-issues-open-buffer 42 "owner/repo"))
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

;;; Read Issue Tests

(ert-deftest test-shipit-issues--read-issue-returns-the-pick ()
  "GIVEN search results
WHEN the user picks one with completion
THEN its info plist comes back rather than a buffer opening."
  (let ((results '(((number . "PRJ-4") (title . "Fix sensor"))
                   ((number . "PRJ-2") (title . "Docs"))))
        (opened nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (seq-find (lambda (c) (string-prefix-p "PRJ-2" c))
                           (all-completions "" table))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (&rest _) (setq opened t))))
      (let ((info (shipit-issues--read-issue "Pick: " results "owner/repo")))
        (should (equal "PRJ-2" (plist-get info :number)))
        (should (equal "owner/repo" (plist-get info :repo)))
        (should-not opened)))))

(ert-deftest test-shipit-issues--select-from-results-opens-the-pick ()
  "GIVEN search results
WHEN selecting from them
THEN the chosen issue opens in REPO."
  (let ((results '(((number . 42) (title . "Login bug"))))
        (opened nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _) (car (all-completions "" table))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (number repo) (setq opened (cons number repo)))))
      (shipit-issues--select-from-results results "owner/repo")
      (should (equal opened '(42 . "owner/repo"))))))

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

;;; The issue of a row

(defun test-shipit-issues--search-rows (repo numbers)
  "The rows that the issue search of REPO shows for the issues NUMBERS."
  (shipit-issues--build-candidates
   (mapcar (lambda (number) `((number . ,number) (title . "Fix the door")))
           numbers)
   repo (make-hash-table :test 'equal)))

(ert-deftest test-shipit-issues--row-names-its-jira-issue ()
  "A row of a Jira issue carries its repository and the key of the issue."
  ;; GIVEN the Jira issue PRJ-7 of acme/door, found by the issue search
  ;; WHEN the search makes a row of it
  ;; THEN the row carries the repository and the key of the issue, from
  ;; its first to its last character
  (let ((row (car (test-shipit-issues--search-rows "acme/door" '("PRJ-7")))))
    (dolist (position (list 0 (1- (length row))))
      (should (equal "acme/door" (get-text-property position 'shipit-repo row)))
      (should (equal "PRJ-7" (get-text-property position 'shipit-issue-number row))))))

(ert-deftest test-shipit-issues--row-names-its-github-issue ()
  "A row of a GitHub issue carries its repository and the number of the issue."
  ;; GIVEN the GitHub issue 12 of acme/door, found by the issue search
  ;; WHEN the search makes a row of it
  ;; THEN the row carries the repository and the number of the issue
  (let ((row (car (test-shipit-issues--search-rows "acme/door" '(12)))))
    (should (equal "acme/door" (get-text-property 0 'shipit-repo row)))
    (should (eql 12 (get-text-property 0 'shipit-issue-number row)))))

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
      (shipit-test-http-answering test-shipit-issues--watching-nothing
        (shipit-issues--read-repo "Repo: " nil nil))
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
      (shipit-test-http-answering test-shipit-issues--watching-nothing
        (shipit-issues--read-repo "Repo: " nil nil))
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
      (shipit-test-http-answering test-shipit-issues--watching-nothing
        (shipit-issues--read-repo "Repo: " nil nil))
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
                 (list :name "Test" :fetch-reactions #'ignore
                       :id-to-string #'shipit-issue-github--id-to-string)))
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
                 (list :name "Jira" :id-to-string (lambda (id) (format "%s" id)))))
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

;;; Embark: the state of many issues

(defconst test-shipit-issues--jira-site
  '(("acme/door" :backend jira :base-url "https://acme.atlassian.net"
     :project-keys ("PRJ")))
  "An entry that keeps the issues of acme/door on a Jira site.")

(defun test-shipit-issues--jira-moves-to-done (resolutions)
  "Jira's list of the moves of an issue whose one move goes to Done.
The move takes a resolution and allows RESOLUTIONS, in this order."
  `((transitions
     . [((id . "31") (name . "Done")
         (to . ((name . "Done") (id . "3")
                (statusCategory . ((key . "done")))))
         (hasScreen . t)
         (fields
          . ((resolution
              . ((required . t)
                 (key . "resolution")
                 (allowedValues
                  . ,(vconcat (mapcar (lambda (name) `((name . ,name)))
                                      resolutions))))))))])))

(defun test-shipit-issues--jira-moves-address (key)
  "The pattern of the address at which Jira lists the moves of KEY with their fields.
KEY can be a regexp that matches several keys."
  (format "/rest/api/2/issue/%s/transitions\\?expand=transitions\\.fields\\'" key))

(defun test-shipit-issues--jira-answers (moves-of-keys)
  "Jira's answers for the issues of MOVES-OF-KEYS, each (KEY . MOVES).
MOVES are the moves of the issue KEY, as Jira lists them.  A move is
answered with no content."
  (append (mapcar (lambda (entry)
                    (list (test-shipit-issues--jira-moves-address (car entry))
                          200 (cdr entry)))
                  moves-of-keys)
          '(("/rest/api/2/issue/[^/]+/transitions\\'" 200 ""))))

(defvar test-shipit-issues--prompts nil
  "The prompts that the user saw, oldest first.")

(defun test-shipit-issues--user-says-nothing (prompt &rest _)
  "Record PROMPT, which the user was not to see, and give no answer."
  (setq test-shipit-issues--prompts
        (append test-shipit-issues--prompts (list prompt)))
  (error "The user has no answer for %S" prompt))

(defmacro test-shipit-issues--with-no-user (&rest body)
  "Run BODY with a user who answers no prompt, and the prompts recorded."
  `(let ((test-shipit-issues--prompts nil)
         (completing-read-function #'test-shipit-issues--user-says-nothing))
     ,@body))

(defmacro test-shipit-issues--with-jira (answers &rest body)
  "Run BODY with the issues of acme/door on a Jira site that answers ANSWERS.
Auth-source has a login for the site."
  (declare (indent 1))
  `(let ((shipit-issue-repo-backends test-shipit-issues--jira-site))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest _)
                  (list (list :user "someone@example.com" :secret "secret")))))
       (shipit-test-http-answering ,answers
         ,@body))))

(ert-deftest test-shipit-issues-embark-change-status-refuses-a-row-without-its-issue ()
  "A row that does not name its issue is refused with a message that names the row."
  ;; GIVEN a row of text that does not say which issue it is, as another
  ;; list of issues can have
  ;; WHEN the state action runs on it
  ;; THEN the user is told that the row does not name its issue
  ;; AND the user sees no prompt and no tracker gets a request
  (shipit-test-http-answering nil
    (test-shipit-issues--with-no-user
     (let ((err (should-error (shipit-issues-embark-change-status
                               '("PRJ-1 Fix the door")))))
       (should (eq 'user-error (car err)))
       (should (string-search "PRJ-1 Fix the door" (error-message-string err)))
       (should-not test-shipit-issues--prompts)
       (should-not shipit-test-http-requests)))))

(ert-deftest test-shipit-issues-embark-change-status-refuses-rows-of-two-repositories ()
  "Rows of two repositories are refused before anything is asked or sent."
  ;; GIVEN a row of an issue of acme/door and a row of one of acme/window
  ;; WHEN the state action runs on both rows
  ;; THEN the user is told that the rows are of more than one repository
  ;; AND the user sees no prompt and no tracker gets a request
  (shipit-test-http-answering nil
    (test-shipit-issues--with-no-user
     (let ((err (should-error
                 (shipit-issues-embark-change-status
                  (append (test-shipit-issues--search-rows "acme/door" '(12))
                          (test-shipit-issues--search-rows "acme/window" '(13))))
                 :type 'user-error)))
       (should (string-search "more than one repository" (error-message-string err)))
       (should-not test-shipit-issues--prompts)
       (should-not shipit-test-http-requests)))))

(ert-deftest test-shipit-issues-embark-change-status-refuses-a-tracker-without-states ()
  "A tracker that cannot change the state of an issue is refused at once."
  ;; GIVEN two issues of acme/door, whose issues are on GitLab, where
  ;; shipit cannot change the state of an issue
  ;; WHEN the state action runs on their rows
  ;; THEN the user is told that the tracker of acme/door cannot change
  ;; the state of an issue
  ;; AND the user sees no prompt and no tracker gets a request
  (let ((shipit-issue-repo-backends '(("acme/door" :backend gitlab))))
    (shipit-test-http-answering nil
      (test-shipit-issues--with-no-user
       (let ((err (should-error
                   (shipit-issues-embark-change-status
                    (test-shipit-issues--search-rows "acme/door" '(12 13)))
                   :type 'user-error)))
         (should (equal "The issue tracker of acme/door cannot change the state of an issue"
                        (error-message-string err)))
         (should-not test-shipit-issues--prompts)
         (should-not shipit-test-http-requests))))))

(ert-deftest test-shipit-issues-embark-change-status-says-when-no-issue-can-move ()
  "When no issue offers a move, the user is told so and is asked nothing."
  ;; GIVEN two Jira issues of acme/door that offer no move at all
  ;; WHEN the state action runs on their rows
  ;; THEN the user is told that none of the 2 issues can change its state
  ;; AND the user sees no prompt
  ;; AND Jira was only asked for the moves of the two issues
  (test-shipit-issues--with-jira
      (list (list (test-shipit-issues--jira-moves-address "PRJ-[12]")
                  200 '((transitions . []))))
    (test-shipit-issues--with-no-user
     (let ((err (should-error
                 (shipit-issues-embark-change-status
                  (test-shipit-issues--search-rows "acme/door" '("PRJ-1" "PRJ-2")))
                 :type 'user-error)))
       (should (equal "None of the 2 issues can change its state"
                      (error-message-string err)))
       (should-not test-shipit-issues--prompts)
       (should (equal '("GET" "GET") (mapcar #'car shipit-test-http-requests)))))))

(defvar test-shipit-issues--sorting nil
  "How each prompt that the user saw sorts its candidates, oldest first.
Each is the display sort function and the cycle sort function of the
prompt.")

(defun test-shipit-issues--user-notes-the-sorting (_prompt collection &optional predicate &rest _)
  "Note how COLLECTION sorts its candidates, and pick the first that PREDICATE keeps."
  (let ((metadata (completion-metadata "" collection predicate)))
    (setq test-shipit-issues--sorting
          (append test-shipit-issues--sorting
                  (list (list (completion-metadata-get metadata 'display-sort-function)
                              (completion-metadata-get metadata 'cycle-sort-function)))))
    (car (all-completions "" collection predicate))))

(ert-deftest test-shipit-issues-embark-change-status-keeps-the-order-of-its-prompts ()
  "Both prompts of the state action tell the completion UI to keep their order."
  ;; GIVEN the Jira issue PRJ-1 of acme/door, whose move to Done takes a
  ;; resolution and allows Done and Won't Do
  ;; WHEN the state action runs on its row, and the user picks the
  ;; first move offered, Done, and then the first resolution
  ;; THEN the user saw two prompts
  ;; AND neither prompt lets the completion UI sort its candidates
  (let ((test-shipit-issues--sorting nil)
        (completing-read-function #'test-shipit-issues--user-notes-the-sorting))
    (test-shipit-issues--with-jira
        (test-shipit-issues--jira-answers
         `(("PRJ-1" . ,(test-shipit-issues--jira-moves-to-done '("Done" "Won't Do")))))
      (shipit-issues-embark-change-status
       (test-shipit-issues--search-rows "acme/door" '("PRJ-1")))
      (should (equal test-shipit-issues--sorting
                     '((identity identity) (identity identity)))))))

(defvar test-shipit-issues--offered nil
  "The prompts that the user saw, oldest first, each as (PROMPT CANDIDATES).")

(defun test-shipit-issues--user-picks-the-first (prompt collection &optional predicate &rest _)
  "Note PROMPT and the candidates of COLLECTION that PREDICATE keeps.
Picks the first of them."
  (let ((candidates (all-completions "" collection predicate)))
    (setq test-shipit-issues--offered
          (append test-shipit-issues--offered (list (list prompt candidates))))
    (car candidates)))

(ert-deftest test-shipit-issues-embark-change-status-offers-only-the-common-resolutions ()
  "The resolution prompt offers only the resolutions that every move allows."
  ;; GIVEN PRJ-2, whose move to Done allows Won't Do, Duplicate and
  ;; Cannot Reproduce, in this order
  ;; AND PRJ-1, whose move to Done allows Cannot Reproduce, Done and
  ;; Duplicate, in this order
  ;; AND the default resolution Done
  ;; AND the issue search shows PRJ-2 in the first row
  ;; WHEN the state action runs on both rows, and the user picks
  ;; `Done (2 of 2)'
  ;; THEN the resolution prompt offers Duplicate and Cannot Reproduce,
  ;; in the order of PRJ-2
  (let ((test-shipit-issues--offered nil)
        (completing-read-function #'test-shipit-issues--user-picks-the-first)
        (shipit-issue-jira-default-resolution "Done"))
    (test-shipit-issues--with-jira
        (test-shipit-issues--jira-answers
         `(("PRJ-2" . ,(test-shipit-issues--jira-moves-to-done
                        '("Won't Do" "Duplicate" "Cannot Reproduce")))
           ("PRJ-1" . ,(test-shipit-issues--jira-moves-to-done
                        '("Cannot Reproduce" "Done" "Duplicate")))))
      (let ((rows (test-shipit-issues--search-rows "acme/door" '("PRJ-1" "PRJ-2"))))
        (should (equal "PRJ-2" (get-text-property 0 'shipit-issue-number (car rows))))
        (shipit-issues-embark-change-status rows))
      (should (equal (nth 1 test-shipit-issues--offered)
                     '("Resolution: " ("Duplicate" "Cannot Reproduce")))))))

(ert-deftest test-shipit-issues-embark-change-status-refuses-when-no-resolution-is-common ()
  "Moves that allow no common resolution are refused before anything is sent."
  ;; GIVEN PRJ-1, whose move to Done allows only Won't Do
  ;; AND PRJ-2, whose move to Done allows only Duplicate
  ;; WHEN the state action runs on both rows, and the user picks
  ;; `Done (2 of 2)'
  ;; THEN the user is told that the issues allow no common resolution
  ;; for Done
  ;; AND the user saw no resolution prompt
  ;; AND Jira was only asked for the moves of the two issues
  (let ((test-shipit-issues--offered nil)
        (completing-read-function #'test-shipit-issues--user-picks-the-first))
    (test-shipit-issues--with-jira
        (test-shipit-issues--jira-answers
         `(("PRJ-1" . ,(test-shipit-issues--jira-moves-to-done '("Won't Do")))
           ("PRJ-2" . ,(test-shipit-issues--jira-moves-to-done '("Duplicate")))))
      (let ((err (should-error
                  (shipit-issues-embark-change-status
                   (test-shipit-issues--search-rows "acme/door" '("PRJ-1" "PRJ-2")))
                  :type 'user-error)))
        (should (equal "The issues allow no common resolution for Done"
                       (error-message-string err)))
        (should (equal (mapcar #'car test-shipit-issues--offered)
                       '("New state for 2 issues: ")))
        (should (equal '("GET" "GET") (mapcar #'car shipit-test-http-requests)))))))

(defvar embark-general-map)
(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-multitarget-actions)
(defvar embark-candidate-collectors)

(defvar embark-default-action-overrides)

(ert-deftest test-shipit-issues-embark-rows-keep-the-general-actions ()
  "A row of the issue search offers the general actions of embark too."
  ;; GIVEN embark with its general actions, of which E exports the rows
  ;; WHEN the issue search sets itself up for embark
  ;; THEN a row offers the export of embark
  ;; AND a row still offers s, the change of state
  (let ((embark-general-map (make-sparse-keymap))
        (embark-target-finders nil)
        (embark-keymap-alist nil)
        (embark-multitarget-actions nil)
        (embark-default-action-overrides nil)
        (embark-candidate-collectors nil))
    (define-key embark-general-map (kbd "E") 'embark-export)
    (unwind-protect
        (progn
          (shipit-issues--embark-setup)
          (should (eq 'embark-export
                      (lookup-key shipit-issues-embark-map (kbd "E"))))
          (should (eq 'shipit-issues-embark-change-status
                      (lookup-key shipit-issues-embark-map (kbd "s"))))
          (should (eq 'shipit-issues-embark-map
                      (cdr (assq 'shipit-issue embark-keymap-alist)))))
      (set-keymap-parent shipit-issues-embark-map nil))))

(defun test-shipit-issues--rows (repo numbers)
  "The rows that the issue search of REPO shows for the issues NUMBERS."
  (all-completions "" (shipit-issues--make-completion-table
                       (mapcar (lambda (number)
                                 `((number . ,number) (title . "Fix the door")))
                               numbers)
                       repo (make-hash-table :test 'equal) nil)))

(ert-deftest test-shipit-issues-embark-open-shows-the-issue-of-the-row ()
  "The open action shows the issue that the row names, of the row's repository."
  ;; GIVEN the issue PRJ-95 of acme/door, already open in a buffer of its own
  ;; AND a row of the issue search for it
  ;; AND a current buffer that is about another repository
  ;; WHEN the open action runs on the row
  ;; THEN the window shows the buffer of PRJ-95 of acme/door
  ;; AND the tracker gets no request, because the buffer was open already
  (test-shipit-issues--with-jira nil
    (let ((issue (get-buffer-create
                  (shipit-issue-buffer-name "acme/door" "PRJ-95"))))
      (unwind-protect
          (with-temp-buffer
            (setq-local shipit-buffer-repo "other/place")
            (save-window-excursion
              (shipit-issues-embark-open
               (test-shipit-issues--rows "acme/door" '("PRJ-95")))
              (should (eq issue (window-buffer (selected-window))))
              (should-not shipit-test-http-requests)))
        (kill-buffer issue)))))

(ert-deftest test-shipit-issues-embark-return-on-a-row-opens-its-issue ()
  "The default action of embark on a row of the issue search opens its issue."
  ;; GIVEN embark with its general actions and a default action of its own
  ;; WHEN the issue search sets itself up for embark
  ;; THEN the default action on a row opens the issue of the row
  ;; AND o on a row does the same
  ;; AND embark gives the rows to that action in one call
  (let ((embark-general-map (make-sparse-keymap))
        (embark-target-finders nil)
        (embark-keymap-alist nil)
        (embark-multitarget-actions nil)
        (embark-default-action-overrides (list '(file . find-file)))
        (embark-candidate-collectors nil))
    (unwind-protect
        (progn
          (shipit-issues--embark-setup)
          (should (eq 'shipit-issues-embark-open
                      (alist-get 'shipit-issue embark-default-action-overrides)))
          (should (eq 'shipit-issues-embark-open
                      (lookup-key shipit-issues-embark-map (kbd "o"))))
          (should (memq 'shipit-issues-embark-open embark-multitarget-actions)))
      (set-keymap-parent shipit-issues-embark-map nil))))

;;; Embark: browse and copy on rows, and the row as a target

(defmacro test-shipit-issues--with-own-kill-ring (&rest body)
  "Run BODY with an empty kill ring of its own.
The clipboard of the system is left alone."
  (declare (indent 0))
  `(let ((kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil)
         (interprogram-paste-function nil))
     ,@body))

(defun test-shipit-issues--row-action (key)
  "The action that KEY runs on the rows of an issue list."
  (lookup-key shipit-issues-embark-map (kbd key)))

(defun test-shipit-issues--said-by (key rows)
  "The last line that the action on KEY says when it runs on ROWS.
It copies into a kill ring of its own."
  (test-shipit-issues--with-own-kill-ring
    (ert-with-message-capture said
      (funcall (test-shipit-issues--row-action key) rows)
      (car (last (split-string said "\n" t))))))

(ert-deftest test-shipit-issues-embark-copy-url-tells-what-it-copied ()
  "Copy URL shows the address of one row, and only the count of several."
  ;; GIVEN the rows of PRJ-1 and PRJ-2 of acme/door, whose issues are in
  ;; Jira
  (test-shipit-issues--with-jira nil
    ;; WHEN u runs on the row of PRJ-1 alone
    ;; THEN the message shows the address that was copied
    (should (equal "Copied: https://acme.atlassian.net/browse/PRJ-1"
                   (test-shipit-issues--said-by
                    "u" (test-shipit-issues--rows "acme/door" '("PRJ-1")))))
    ;; WHEN u runs on both rows
    ;; THEN the message tells that 2 addresses were copied
    (should (equal "Copied 2 addresses"
                   (test-shipit-issues--said-by
                    "u" (test-shipit-issues--rows "acme/door" '("PRJ-1" "PRJ-2")))))))

(ert-deftest test-shipit-issues-embark-copy-key-tells-what-it-copied ()
  "Copy key shows the key of one row, and only the count of several."
  ;; GIVEN the row of the GitHub issue 12 of acme/window
  ;; WHEN w runs on it
  ;; THEN the message shows the key that was copied, #12
  (should (equal "Copied: #12"
                 (test-shipit-issues--said-by
                  "w" (test-shipit-issues--rows "acme/window" '(12)))))
  ;; GIVEN the rows of the GitHub issues 12, 13 and 14 of acme/window
  ;; WHEN w runs on the three rows
  ;; THEN the message tells that 3 keys were copied
  (should (equal "Copied 3 keys"
                 (test-shipit-issues--said-by
                  "w" (test-shipit-issues--rows "acme/window" '(12 13 14))))))

(ert-deftest test-shipit-issues-embark-row-actions-stop-at-a-row-without-its-issue ()
  "Browse and copy stop at a row that does not name its issue, before they act."
  ;; GIVEN a row of text that does not say which issue it is, as another
  ;; list of issues can have: alone, and after the row of PRJ-1 of
  ;; acme/door, whose issues are in Jira
  ;; WHEN b, u and w each run on those rows
  ;; THEN each time the user is told that the row does not name its issue
  ;; AND the browser shows nothing and nothing is copied, not even for
  ;; PRJ-1
  (test-shipit-issues--with-jira nil
    (let* ((bare "PRJ-9 Fix the door")
           (browsed nil)
           (browse-url-handlers nil)
           (browse-url-browser-function (lambda (url &rest _) (push url browsed))))
      (test-shipit-issues--with-own-kill-ring
        (dolist (rows (list (list bare)
                            (append (test-shipit-issues--rows "acme/door" '("PRJ-1"))
                                    (list bare))))
          (dolist (key '("b" "u" "w"))
            (let ((err (should-error (funcall (test-shipit-issues--row-action key) rows)
                                     :type 'user-error)))
              (should (string-search bare (error-message-string err))))))
        (should-not browsed)
        (should-not kill-ring)))))

(defun test-shipit-issues--target-at (position)
  "The target that the row finder gives with point at POSITION, or nil."
  (save-excursion
    (goto-char position)
    (shipit-issues-embark-target)))

(ert-deftest test-shipit-issues-embark-target-is-not-the-header-of-an-issue ()
  "Text that names an issue but is no row of an issue list is no row target."
  ;; GIVEN a buffer with the header of the issue PRJ-1 of acme/door, which
  ;; names its issue and its repository in its text properties, as an
  ;; issue buffer does, but is no row of an issue list
  ;; WHEN embark asks for the target on the header
  ;; THEN the row finder gives nothing
  (with-temp-buffer
    (insert (propertize "PRJ-1 Fix the door"
                        'shipit-issue-number "PRJ-1"
                        'shipit-repo "acme/door"))
    (should-not (test-shipit-issues--target-at (point-min)))))

(ert-deftest test-shipit-issues-embark-target-covers-the-whole-row ()
  "The row finder gives the whole row from its first and its last character."
  ;; GIVEN a buffer with plain text, then the row of PRJ-1 of acme/door,
  ;; then a note without properties, as an export of embark shows a row
  (with-temp-buffer
    (insert "see ")
    (let ((start (point)))
      (insert (car (test-shipit-issues--rows "acme/door" '("PRJ-1"))))
      (let ((end (point)))
        (insert "  To Do\n")
        ;; WHEN embark asks for the target at the first and at the last
        ;; character of the row
        ;; THEN each time the target is of the kind `shipit-issue' and
        ;; covers the row and nothing else
        ;; AND its value is the row, which still names its issue and its
        ;; repository
        (dolist (position (list start (1- end)))
          (pcase-let ((`(,kind ,row ,from . ,to) (test-shipit-issues--target-at position)))
            (should (eq 'shipit-issue kind))
            (should (equal (cons start end) (cons from to)))
            (should (equal "PRJ-1" (get-text-property 0 'shipit-issue-number row)))
            (should (equal "acme/door" (get-text-property 0 'shipit-repo row)))))
        ;; WHEN embark asks for the target on the note after the row
        ;; THEN the row finder gives nothing
        (should-not (test-shipit-issues--target-at end))))))

(ert-deftest test-shipit-issues-embark-target-finds-a-row-that-fills-the-buffer ()
  "The row finder gives the whole row when no text comes before or after it."
  ;; GIVEN a buffer that holds only the row of the GitHub issue 12 of
  ;; acme/window
  ;; WHEN embark asks for the target at its first and at its last character
  ;; THEN each time the target covers the whole buffer
  (with-temp-buffer
    (insert (car (test-shipit-issues--rows "acme/window" '(12))))
    (dolist (position (list (point-min) (1- (point-max))))
      (let ((target (test-shipit-issues--target-at position)))
        (should (eq 'shipit-issue (car target)))
        (should (equal (cons (point-min) (point-max)) (cddr target)))))))

(ert-deftest test-shipit-issues-embark-setup-puts-the-row-finder-first ()
  "The setup moves the row finder to the front, also when it is in the list."
  ;; GIVEN embark's target finders with another finder in front of the
  ;; row finder, as when another part of shipit set itself up later
  ;; WHEN the issue search sets itself up for embark
  ;; THEN the row finder is the first of the target finders, and it is
  ;; in the list once
  ;; AND the other finders keep their order behind it
  (let ((embark-general-map (make-sparse-keymap))
        (embark-target-finders (list 'shipit-code-refs-embark-target
                                     'shipit-issues-embark-target
                                     'embark-target-url-at-point))
        (embark-keymap-alist nil)
        (embark-multitarget-actions nil)
        (embark-default-action-overrides nil)
        (embark-candidate-collectors nil))
    (unwind-protect
        (progn
          (shipit-issues--embark-setup)
          (should (equal '(shipit-issues-embark-target
                           shipit-code-refs-embark-target
                           embark-target-url-at-point)
                         embark-target-finders)))
      (set-keymap-parent shipit-issues-embark-map nil))))

;;; Embark: the rows in the region

(defun test-shipit-issues--row (repo number)
  "The row that the issue search of REPO shows for the issue NUMBER."
  (car (test-shipit-issues--rows repo (list number))))

(defun test-shipit-issues--insert-rows (rows)
  "Insert ROWS, each with a note after it, as an export of embark shows them.
Returns where each row is in the buffer, as (START . END), in the order
of ROWS."
  (mapcar (lambda (row)
            (let ((start (point)))
              (insert row)
              (prog1 (cons start (point))
                (insert "  To Do\n"))))
          rows))

(defun test-shipit-issues--mark-region (from to)
  "Make the region active from FROM to TO, with point at TO.
Fails when Emacs does not take it as an active region."
  (set-mark from)
  (goto-char to)
  (should (use-region-p)))

(ert-deftest test-shipit-issues-embark-copy-key-takes-each-row-inside-a-value ()
  "A value that holds several rows gives the issue of each to an action, in order."
  ;; GIVEN one value that holds the row of the GitHub issue 13 of
  ;; acme/window, a note, and then the row of its issue 12, as the target
  ;; of a region over the two rows holds them
  ;; WHEN w runs on that value
  ;; THEN the kill ring holds #13 and #12, one per line, in the order of
  ;; the text, and nothing for the note between them
  (let ((value (concat (test-shipit-issues--row "acme/window" 13)
                       "  To Do\n"
                       (test-shipit-issues--row "acme/window" 12))))
    (test-shipit-issues--with-own-kill-ring
      (funcall (test-shipit-issues--row-action "w") (list value))
      (should (equal "#13\n#12" (car kill-ring))))))

(ert-deftest test-shipit-issues-embark-a-marked-row-without-its-issue-is-a-bug ()
  "A row that has the mark of a row but lacks its number or repository is an error."
  ;; GIVEN a row that has the mark of a row of an issue list but no issue
  ;; number, and one that has the mark but no repository, as only a bug
  ;; in the code that made the row gives
  ;; WHEN w runs on each of them
  ;; THEN each time Emacs signals an error, and not a user error
  ;; AND the error shows the row
  ;; AND nothing is copied
  (let ((rows (list (propertize "PRJ-9 Fix the door"
                                'shipit-issue-row t 'shipit-repo "acme/door")
                    (propertize "PRJ-9 Fix the door"
                                'shipit-issue-row t 'shipit-issue-number "PRJ-9"))))
    (test-shipit-issues--with-own-kill-ring
      (dolist (row rows)
        (let ((err (should-error (funcall (test-shipit-issues--row-action "w")
                                          (list row)))))
          (should (eq 'error (car err)))
          (should (string-search "PRJ-9 Fix the door" (error-message-string err)))))
      (should-not kill-ring))))

(ert-deftest test-shipit-issues-embark-target-of-a-region-covers-its-rows ()
  "A region over rows is one target from the start of its first row to the end of its last."
  ;; GIVEN a buffer that shows the rows of the GitHub issues 13 and 12 of
  ;; acme/window, each with a note after it, as an export of embark does
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (pcase-let ((`(,first ,second)
                   (test-shipit-issues--insert-rows
                    (list (test-shipit-issues--row "acme/window" 13)
                          (test-shipit-issues--row "acme/window" 12)))))
        ;; WHEN the region covers exactly the row of 13 and embark asks
        ;; for the target
        ;; THEN the target is of the kind `shipit-issue' and covers that
        ;; row and nothing else
        ;; AND its value is that row, with its text properties
        (test-shipit-issues--mark-region (car first) (cdr first))
        (let ((target (shipit-issues-embark-target)))
          (should (eq 'shipit-issue (car target)))
          (should (equal first (cddr target)))
          (should (equal-including-properties
                   (buffer-substring (car first) (cdr first))
                   (cadr target))))
        ;; WHEN the region runs from the start of the row of 13 to the end
        ;; of the row of 12, with point at its start and the mark at its
        ;; end
        ;; THEN the target covers both rows, from the start of the row of
        ;; 13 to the end of the row of 12
        (test-shipit-issues--mark-region (cdr second) (car first))
        (should (equal (cons (car first) (cdr second))
                       (cddr (shipit-issues-embark-target))))))))

(ert-deftest test-shipit-issues-embark-region-without-rows-has-no-candidates ()
  "The collector of the region gives nothing when the region touches no row."
  ;; GIVEN a buffer that shows the row of the GitHub issue 12 of
  ;; acme/window with a note after it
  ;; AND an active region over the note alone, which starts where the
  ;; row ends
  ;; WHEN embark collects the candidates for "act on all"
  ;; THEN the collector gives nothing
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (let ((row (car (test-shipit-issues--insert-rows
                       (list (test-shipit-issues--row "acme/window" 12))))))
        (test-shipit-issues--mark-region (cdr row) (point-max))
        (should-not (shipit-issues-embark-region-candidates))))))

(ert-deftest test-shipit-issues-embark-setup-puts-the-region-collector-first ()
  "The setup moves the collector of the region to the front, also when it is in the list."
  ;; GIVEN embark's candidate collectors with another collector in front
  ;; of the collector of the rows in the region
  ;; WHEN the issue search sets itself up for embark
  ;; THEN the collector of the region is the first of the candidate
  ;; collectors, and it is in the list once
  ;; AND the other collectors keep their order behind it
  (let ((embark-general-map (make-sparse-keymap))
        (embark-target-finders nil)
        (embark-keymap-alist nil)
        (embark-multitarget-actions nil)
        (embark-default-action-overrides nil)
        (embark-candidate-collectors (list 'embark-selected-candidates
                                           'shipit-issues-embark-region-candidates
                                           'embark-minibuffer-candidates)))
    (unwind-protect
        (progn
          (shipit-issues--embark-setup)
          (should (equal '(shipit-issues-embark-region-candidates
                           embark-selected-candidates
                           embark-minibuffer-candidates)
                         embark-candidate-collectors)))
      (set-keymap-parent shipit-issues-embark-map nil))))

(provide 'test-shipit-issues)
;;; test-shipit-issues.el ends here
