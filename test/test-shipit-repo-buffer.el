;;; test-shipit-repo-buffer.el --- Tests for repo landing page buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the repository landing page buffer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)

;;; Test helpers

(defvar test-repo-buffer--api-calls nil
  "List of API calls captured during tests.")

(defun test-repo-buffer--mock-api-request (endpoint &rest _args)
  "Mock API request capturing ENDPOINT."
  (push endpoint test-repo-buffer--api-calls)
  (cond
   ((string-match "/repos/[^/]+/[^/]+/languages" endpoint)
    '(("Emacs Lisp" . 8000) ("Python" . 1500) ("Shell" . 500)))
   ((string-match "/repos/[^/]+/[^/]+/readme" endpoint)
    `((content . ,(base64-encode-string "# Hello World\n\nThis is the README."))
      (encoding . "base64")
      (name . "README.md")))
   ((string-match "/repos/\\([^/]+/[^/]+\\)$" endpoint)
    `((full_name . "octocat/hello-world")
      (description . "A test repository")
      (owner . ((login . "octocat")
                (avatar_url . "https://example.com/avatar.png")))
      (default_branch . "main")
      (language . "Emacs Lisp")
      (html_url . "https://github.com/octocat/hello-world")))
   (t nil)))

;;; Backend dispatch tests

(ert-deftest test-repo-buffer-fetch-repo-info-dispatches ()
  "GIVEN a GitHub backend with :fetch-repo-info registered
WHEN calling the operation via backend dispatch
THEN it returns the repo info alist."
  (let* ((test-repo-buffer--api-calls nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (resolved (shipit-pr--resolve-for-repo "octocat/hello-world"))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-repo-info)))
    (should fetch-fn)
    (cl-letf (((symbol-function 'shipit--api-request)
               #'test-repo-buffer--mock-api-request))
      (let ((result (funcall fetch-fn config)))
        (should (equal "octocat/hello-world" (cdr (assq 'full_name result))))
        (should (equal "A test repository" (cdr (assq 'description result))))))))

(ert-deftest test-repo-buffer-fetch-readme-decodes-base64 ()
  "GIVEN a GitHub backend with :fetch-readme registered
WHEN calling the operation
THEN it decodes the base64 content and returns the README text."
  (let* ((shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (resolved (shipit-pr--resolve-for-repo "octocat/hello-world"))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-readme)))
    (should fetch-fn)
    (cl-letf (((symbol-function 'shipit--api-request)
               #'test-repo-buffer--mock-api-request))
      (let ((result (funcall fetch-fn config)))
        (should (stringp result))
        (should (string-match-p "Hello World" result))))))

(ert-deftest test-repo-buffer-fetch-readme-returns-nil-on-missing ()
  "GIVEN a repo with no README
WHEN calling :fetch-readme
THEN it returns nil."
  (let* ((shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (resolved (shipit-pr--resolve-for-repo "octocat/hello-world"))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-readme)))
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _args) nil)))
      (let ((result (funcall fetch-fn config)))
        (should (null result))))))

;;; Buffer lifecycle tests

(ert-deftest test-repo-buffer-creates-buffer-with-correct-name ()
  "GIVEN a repo name
WHEN opening the repo buffer
THEN a buffer with the correct name is created."
  (require 'shipit-repo-buffer)
  (let ((buf-name "*shipit-repo: octocat/hello-world*"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (cl-letf (((symbol-function 'shipit-repo-buffer-refresh) #'ignore))
      (shipit-open-repo-buffer "octocat/hello-world")
      (unwind-protect
          (progn
            (should (get-buffer buf-name))
            (with-current-buffer buf-name
              (should (equal shipit-repo-buffer-repo "octocat/hello-world"))))
        (when (get-buffer buf-name)
          (kill-buffer buf-name))))))

(ert-deftest test-repo-buffer-reuses-existing-buffer ()
  "GIVEN an existing repo buffer
WHEN opening the same repo again
THEN the existing buffer is reused."
  (require 'shipit-repo-buffer)
  (let ((buf-name "*shipit-repo: octocat/hello-world*"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (cl-letf (((symbol-function 'shipit-repo-buffer-refresh) #'ignore))
      (shipit-open-repo-buffer "octocat/hello-world")
      (let ((first-buffer (get-buffer buf-name)))
        (shipit-open-repo-buffer "octocat/hello-world")
        (unwind-protect
            (should (eq first-buffer (get-buffer buf-name)))
          (when (get-buffer buf-name)
            (kill-buffer buf-name)))))))

;;; Section rendering tests

(ert-deftest test-repo-buffer-header-renders-owner-and-description ()
  "GIVEN repo data with owner and description
WHEN inserting the header section
THEN the buffer contains the owner name and description."
  (require 'shipit-repo-buffer)
  (let ((repo-data `((full_name . "octocat/hello-world")
                     (description . "A test repository")
                     (owner . ((login . "octocat")
                               (avatar_url . "https://example.com/avatar.png")))
                     (default_branch . "main")
                     (language . "Emacs Lisp")
                     (html_url . "https://github.com/octocat/hello-world"))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (shipit-repo-buffer--insert-header-section repo-data))
        (let ((content (buffer-string)))
          (should (string-match-p "octocat/hello-world" content))
          (should (string-match-p "octocat" content))
          (should (string-match-p "A test repository" content)))))))

(ert-deftest test-repo-buffer-readme-renders-content ()
  "GIVEN README content
WHEN inserting the readme section
THEN the buffer contains the rendered README."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (shipit-repo-buffer--insert-readme-section "# Hello World\n\nSome content.")
      (let ((content (buffer-string)))
        (should (string-match-p "README" content))
        (should (string-match-p "Hello World" content))))))

(ert-deftest test-repo-buffer-missing-readme-shows-message ()
  "GIVEN no README content
WHEN inserting the readme section
THEN the buffer shows a 'No README found' message."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (shipit-repo-buffer--insert-readme-section nil)
      (let ((content (buffer-string)))
        (should (string-match-p "No README found" content))))))

(ert-deftest test-repo-buffer-backend-without-fetch-repo-info-signals-error ()
  "GIVEN a backend without :fetch-repo-info
WHEN trying to refresh the repo buffer
THEN an error is signaled."
  (require 'shipit-repo-buffer)
  (let* ((shipit-pr-backends nil)
         (shipit-pr-backend 'test-bare)
         (shipit-pr-backend-config nil))
    (shipit-pr-register-backend
     'test-bare
     (list :name "Test"
           :fetch-pr #'ignore
           :search #'ignore
           :create-pr #'ignore
           :merge-pr #'ignore
           :update-pr #'ignore
           :fetch-reviews #'ignore
           :submit-review #'ignore
           :fetch-review-decision #'ignore
           :fetch-files #'ignore
           :fetch-commits #'ignore
           :fetch-checks #'ignore
           :browse-url #'ignore))
    (should-error
     (shipit-repo-buffer--fetch-repo-data "test/repo")
     :type 'error)))

;;; URL classification tests

(ert-deftest test-classify-url-github-pr ()
  "GIVEN a GitHub PR URL
WHEN classifying the URL
THEN it returns :type pr with repo and number."
  (require 'shipit-render)
  (let ((result (shipit--classify-url "https://github.com/owner/repo/pull/123")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (equal (plist-get result :repo) "owner/repo"))
    (should (= (plist-get result :number) 123))))

(ert-deftest test-classify-url-github-issue ()
  "GIVEN a GitHub issue URL
WHEN classifying the URL
THEN it returns :type issue with repo and number."
  (require 'shipit-render)
  (let ((result (shipit--classify-url "https://github.com/owner/repo/issues/456")))
    (should result)
    (should (eq (plist-get result :type) 'issue))
    (should (equal (plist-get result :repo) "owner/repo"))
    (should (= (plist-get result :number) 456))))

(ert-deftest test-classify-url-github-repo ()
  "GIVEN a GitHub repo URL
WHEN classifying the URL
THEN it returns :type repo with repo."
  (require 'shipit-render)
  (let ((result (shipit--classify-url "https://github.com/owner/repo")))
    (should result)
    (should (eq (plist-get result :type) 'repo))
    (should (equal (plist-get result :repo) "owner/repo"))))

(ert-deftest test-classify-url-gitlab-mr ()
  "GIVEN a GitLab merge request URL
WHEN classifying the URL
THEN it returns :type pr with repo and number."
  (require 'shipit-render)
  (let ((result (shipit--classify-url "https://gitlab.com/group/project/-/merge_requests/789")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (equal (plist-get result :repo) "group/project"))
    (should (= (plist-get result :number) 789))))

(ert-deftest test-classify-url-gitlab-issue ()
  "GIVEN a GitLab issue URL
WHEN classifying the URL
THEN it returns :type issue with repo and number."
  (require 'shipit-render)
  (let ((result (shipit--classify-url "https://gitlab.com/group/project/-/issues/42")))
    (should result)
    (should (eq (plist-get result :type) 'issue))
    (should (equal (plist-get result :repo) "group/project"))
    (should (= (plist-get result :number) 42))))

(ert-deftest test-classify-url-non-shipit ()
  "GIVEN a non-shipit URL
WHEN classifying the URL
THEN it returns nil."
  (require 'shipit-render)
  (should (null (shipit--classify-url "https://example.com/page"))))

(ert-deftest test-classify-url-github-extra-path-segments ()
  "GIVEN a GitHub URL with extra path segments
WHEN classifying the URL
THEN it returns nil."
  (require 'shipit-render)
  (should (null (shipit--classify-url "https://github.com/owner/repo/tree/main")))
  (should (null (shipit--classify-url "https://github.com/owner/repo/blob/main/file.el"))))

(ert-deftest test-classify-url-github-repo-trailing-slash ()
  "GIVEN a GitHub repo URL with trailing slash
WHEN classifying the URL
THEN it returns :type repo with repo."
  (require 'shipit-render)
  (let ((result (shipit--classify-url "https://github.com/owner/repo/")))
    (should result)
    (should (eq (plist-get result :type) 'repo))
    (should (equal (plist-get result :repo) "owner/repo"))))

(ert-deftest test-classify-url-gitlab-nested-group ()
  "GIVEN a GitLab URL with nested group path
WHEN classifying the URL
THEN it returns the full group/subgroup/project as repo."
  (require 'shipit-render)
  (let ((result (shipit--classify-url "https://gitlab.com/group/subgroup/project/-/merge_requests/10")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (equal (plist-get result :repo) "group/subgroup/project"))
    (should (= (plist-get result :number) 10))))

;;; Backend-level classify-url tests

(ert-deftest test-classify-url-github-backend-pr ()
  "GIVEN a GitHub backend registered
WHEN classifying a GitHub PR URL via the backend function
THEN returns :type pr with repo and number."
  (let ((result (shipit-pr-github--classify-url "https://github.com/owner/repo/pull/42")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (equal (plist-get result :repo) "owner/repo"))
    (should (= (plist-get result :number) 42))))

(ert-deftest test-classify-url-github-backend-issue ()
  "GIVEN a GitHub backend registered
WHEN classifying a GitHub issue URL via the backend function
THEN returns :type issue with repo and number."
  (let ((result (shipit-pr-github--classify-url "https://github.com/owner/repo/issues/99")))
    (should result)
    (should (eq (plist-get result :type) 'issue))
    (should (equal (plist-get result :repo) "owner/repo"))
    (should (= (plist-get result :number) 99))))

(ert-deftest test-classify-url-github-backend-repo ()
  "GIVEN a GitHub backend registered
WHEN classifying a GitHub repo URL via the backend function
THEN returns :type repo with repo."
  (let ((result (shipit-pr-github--classify-url "https://github.com/owner/repo")))
    (should result)
    (should (eq (plist-get result :type) 'repo))
    (should (equal (plist-get result :repo) "owner/repo"))))

(ert-deftest test-classify-url-github-backend-non-github ()
  "GIVEN a GitHub backend registered
WHEN classifying a non-GitHub URL via the backend function
THEN returns nil."
  (should (null (shipit-pr-github--classify-url "https://gitlab.com/group/project/-/merge_requests/1"))))

(ert-deftest test-classify-url-gitlab-backend-mr ()
  "GIVEN a GitLab backend registered
WHEN classifying a GitLab MR URL via the backend function
THEN returns :type pr with repo and number."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr-gitlab--classify-url "https://gitlab.com/group/project/-/merge_requests/55")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (equal (plist-get result :repo) "group/project"))
    (should (= (plist-get result :number) 55))))

(ert-deftest test-classify-url-gitlab-backend-issue ()
  "GIVEN a GitLab backend registered
WHEN classifying a GitLab issue URL via the backend function
THEN returns :type issue with repo and number."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr-gitlab--classify-url "https://gitlab.com/group/project/-/issues/10")))
    (should result)
    (should (eq (plist-get result :type) 'issue))
    (should (equal (plist-get result :repo) "group/project"))
    (should (= (plist-get result :number) 10))))

(ert-deftest test-classify-url-gitlab-backend-non-gitlab ()
  "GIVEN a GitLab backend registered
WHEN classifying a non-GitLab URL via the backend function
THEN returns nil."
  (require 'shipit-pr-gitlab)
  (should (null (shipit-pr-gitlab--classify-url "https://github.com/owner/repo/pull/1"))))

(ert-deftest test-classify-url-dispatcher-returns-first-match ()
  "GIVEN both backends registered
WHEN classifying a GitHub PR URL via the dispatcher
THEN the dispatcher returns the first non-nil result."
  (let ((result (shipit-pr--classify-url "https://github.com/owner/repo/pull/7")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (equal (plist-get result :repo) "owner/repo"))
    (should (= (plist-get result :number) 7))))

(ert-deftest test-classify-url-dispatcher-unknown-url ()
  "GIVEN both backends registered
WHEN classifying an unknown URL via the dispatcher
THEN returns nil."
  (should (null (shipit-pr--classify-url "https://example.com/page"))))

;;; Details block rendering tests

(ert-deftest test-repo-buffer-readme-details-blocks-parsed ()
  "GIVEN readme text containing <details><summary>Click</summary>body</details>
WHEN parsed by shipit--parse-markdown-details
THEN returns list with a details element containing correct summary and body."
  (require 'shipit-render)
  (let* ((text "<details>\n<summary>Click</summary>\nbody\n</details>")
         (result (shipit--parse-markdown-details text)))
    (should (= (length result) 1))
    (let ((block (car result)))
      (should (eq (car block) 'details))
      (should (equal (cadr block) "Click"))
      (should (string-match-p "body" (caddr block))))))

(ert-deftest test-repo-buffer-readme-dispatches-to-details-renderer ()
  "GIVEN readme text containing a <details> block
WHEN inserting the readme section
THEN shipit--insert-body-with-details is called instead of plain rendering."
  (require 'shipit-repo-buffer)
  (require 'shipit-render)
  (let ((details-called nil))
    (cl-letf (((symbol-function 'shipit--insert-body-with-details)
               (lambda (body indent)
                 (setq details-called (list body indent))
                 (insert "   [details rendered]\n"))))
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (shipit-repo-buffer--insert-readme-section
           "Some text\n<details>\n<summary>Click</summary>\nbody\n</details>")
          (should details-called)
          (should (= (cadr details-called) 3)))))))

;;; Language percentage tests

(ert-deftest test-shipit-repo-buffer-format-language-percentages ()
  "GIVEN a languages alist with multiple languages and byte counts
WHEN calling shipit-repo-buffer--format-language-percentages
THEN returns sorted list of (NAME . PERCENT) pairs summing to ~100%."
  (require 'shipit-repo-buffer)
  (let ((languages '(("Python" . 5000)
                     ("Shell" . 1000)
                     ("Emacs Lisp" . 4000))))
    (let ((result (shipit-repo-buffer--format-language-percentages languages)))
      ;; Sorted by bytes descending
      (should (equal (car (nth 0 result)) "Python"))
      (should (equal (car (nth 1 result)) "Emacs Lisp"))
      (should (equal (car (nth 2 result)) "Shell"))
      ;; Percentages correct
      (should (= (cdr (nth 0 result)) 50.0))
      (should (= (cdr (nth 1 result)) 40.0))
      (should (= (cdr (nth 2 result)) 10.0)))))

(ert-deftest test-shipit-repo-buffer-format-language-percentages-single ()
  "GIVEN a languages alist with one language
WHEN calling shipit-repo-buffer--format-language-percentages
THEN returns a single entry at 100%."
  (require 'shipit-repo-buffer)
  (let ((languages '(("Rust" . 12345))))
    (let ((result (shipit-repo-buffer--format-language-percentages languages)))
      (should (= (length result) 1))
      (should (equal (car (car result)) "Rust"))
      (should (= (cdr (car result)) 100.0)))))

(ert-deftest test-shipit-repo-buffer-format-language-percentages-symbol-keys ()
  "GIVEN a languages alist with symbol keys (as returned by json-read)
WHEN calling shipit-repo-buffer--format-language-percentages
THEN converts symbols to strings in the result."
  (require 'shipit-repo-buffer)
  (let ((languages '((C++ . 3000) (Shell . 1000))))
    (let ((result (shipit-repo-buffer--format-language-percentages languages)))
      (should (= (length result) 2))
      (should (stringp (car (nth 0 result))))
      (should (equal (car (nth 0 result)) "C++"))
      (should (= (cdr (nth 0 result)) 75.0)))))

(ert-deftest test-shipit-repo-buffer-format-language-percentages-nil ()
  "GIVEN nil languages
WHEN calling shipit-repo-buffer--format-language-percentages
THEN returns nil."
  (require 'shipit-repo-buffer)
  (should (null (shipit-repo-buffer--format-language-percentages nil))))

(ert-deftest test-shipit-repo-buffer-insert-languages-section ()
  "GIVEN formatted language percentages
WHEN inserting the languages section
THEN a collapsible section is rendered with correct heading and content."
  (require 'shipit-repo-buffer)
  (let ((languages '(("Python" . 50.0)
                     ("Emacs Lisp" . 40.0)
                     ("Shell" . 10.0))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-languages-section languages))
        (let ((content (buffer-string)))
          (should (string-match-p "Languages (3)" content))
          (should (string-match-p "Python" content))
          (should (string-match-p "50.0%" content))
          (should (string-match-p "Emacs Lisp" content))
          (should (string-match-p "Shell" content))
          (should (string-match-p "10.0%" content)))))))

(ert-deftest test-shipit-repo-buffer-fetch-languages-dispatches ()
  "GIVEN a GitHub backend with :fetch-languages registered
WHEN calling the operation via backend dispatch
THEN it returns the languages alist."
  (let* ((test-repo-buffer--api-calls nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (resolved (shipit-pr--resolve-for-repo "octocat/hello-world"))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-languages)))
    (should fetch-fn)
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (endpoint &rest _args)
                 (push endpoint test-repo-buffer--api-calls)
                 '(("Python" . 5000) ("Shell" . 1000)))))
      (let ((result (funcall fetch-fn config)))
        (should (equal result '(("Python" . 5000) ("Shell" . 1000))))
        (should (member "/repos/octocat/hello-world/languages"
                        test-repo-buffer--api-calls))))))

;;; PR/Issue/Discussion section tests

(ert-deftest test-repo-buffer-prs-section-renders-open-closed-subsections ()
  "GIVEN open and closed PR lists
WHEN inserting the PRs section
THEN the buffer contains Open/Closed subsection headings with correct counts."
  (require 'shipit-repo-buffer)
  (let ((open-prs `(((number . 42) (title . "Add feature X"))
                    ((number . 37) (title . "Fix bug Y"))))
        (closed-prs `(((number . 10) (title . "Old PR")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-prs-section
           open-prs closed-prs nil "test/repo"))
        (let ((content (buffer-string)))
          (should (string-match-p "Open (2)" content))
          (should (string-match-p "Closed (1)" content))
          (should (string-match-p "#42" content))
          (should (string-match-p "Add feature X" content))
          (should (string-match-p "#37" content))
          (should (string-match-p "Fix bug Y" content))
          (should (string-match-p "#10" content))
          (should (string-match-p "Old PR" content)))))))

(ert-deftest test-repo-buffer-prs-section-uses-correct-icon-name ()
  "GIVEN PR data
WHEN inserting the PRs section
THEN the icon function is called with 'pull-request' (not 'pr')."
  (require 'shipit-repo-buffer)
  (let ((icon-names nil)
        (open-prs `(((number . 1) (title . "Test")))))
    (cl-letf (((symbol-function 'shipit--get-pr-field-icon)
               (lambda (name fallback)
                 (push name icon-names)
                 fallback)))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-prs-section
             open-prs nil nil "test/repo"))
          (should (member "pull-request" icon-names)))))))

(ert-deftest test-repo-buffer-prs-section-heading-uses-backend-label ()
  "GIVEN a backend with :pr-type-label 'Merge Request'
WHEN inserting the PRs section
THEN the heading uses 'Merge Requests' instead of 'Pull Requests'."
  (require 'shipit-repo-buffer)
  (let ((open-prs `(((number . 1) (title . "Test"))))
        (shipit-pr-backend 'test-mr)
        (shipit-pr-backend-config nil)
        (shipit-pr-backends nil))
    (shipit-pr-register-backend
     'test-mr
     (list :name "TestMR"
           :pr-type-label "Merge Request"
           :fetch-pr #'ignore :search #'ignore :create-pr #'ignore
           :merge-pr #'ignore :update-pr #'ignore :fetch-reviews #'ignore
           :submit-review #'ignore :fetch-review-decision #'ignore
           :fetch-files #'ignore :fetch-commits #'ignore
           :fetch-checks #'ignore :browse-url #'ignore))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-prs-section
           open-prs nil nil "test/repo"))
        (should (string-match-p "Merge Requests" (buffer-string)))))))

(ert-deftest test-repo-buffer-prs-section-nil-shows-nothing ()
  "GIVEN nil open AND nil closed PR data
WHEN inserting the PRs section
THEN no section is inserted."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test")
        (shipit-repo-buffer--insert-prs-section nil nil nil "test/repo"))
      (should-not (string-match-p "Pull Requests" (buffer-string))))))

(ert-deftest test-repo-buffer-prs-section-load-more-shown-when-has-more ()
  "GIVEN closed PRs with has-more flag set
WHEN inserting the PRs section
THEN a 'load more' line is shown in the closed subsection."
  (require 'shipit-repo-buffer)
  (let ((closed-prs `(((number . 5) (title . "Old PR")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-prs-section
           nil closed-prs t "test/repo"))
        (should (string-match-p "load more" (buffer-string)))))))

(ert-deftest test-repo-buffer-open-prs-load-more-shown-when-has-more ()
  "GIVEN open PRs with open-has-more flag set
WHEN inserting the PRs section
THEN a 'load more' line is shown in the open subsection."
  (require 'shipit-repo-buffer)
  (let ((open-prs `(((number . 1) (title . "PR")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-prs-section
           open-prs nil nil "test/repo" t))
        (should (string-match-p "load more" (buffer-string)))))))

(ert-deftest test-repo-buffer-open-issues-load-more-shown-when-has-more ()
  "GIVEN open issues with open-has-more flag set
WHEN inserting the issues section
THEN a 'load more' line is shown in the open subsection."
  (require 'shipit-repo-buffer)
  (let ((open-issues `(((number . 1) (title . "Issue")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-issues-section
           open-issues nil nil "test/repo" t))
        (should (string-match-p "load more" (buffer-string)))))))

(ert-deftest test-repo-buffer-plus-loads-more-open-prs ()
  "GIVEN point on an open PR item with open-prs-has-more set
WHEN invoking + (load-more-at-point)
THEN load-more is called with 'open-prs."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--load-more)
               (lambda (type &optional _count) (setq called-with type))))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-prs-section
             `(((number . 1) (title . "PR")))
             nil nil "test/repo" t))
          (goto-char (point-min))
          (text-property-search-forward 'shipit-repo-pr-number)
          (setq shipit-repo-buffer-open-prs-has-more t)
          (shipit-repo-buffer--load-more-at-point)
          (should (eq called-with 'open-prs)))))))

(ert-deftest test-repo-buffer-prs-section-load-more-not-shown-when-no-more ()
  "GIVEN closed PRs with has-more flag nil
WHEN inserting the PRs section
THEN no 'load more' line is shown."
  (require 'shipit-repo-buffer)
  (let ((closed-prs `(((number . 5) (title . "Old PR")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-prs-section
           nil closed-prs nil "test/repo"))
        (should-not (string-match-p "load more" (buffer-string)))))))

(ert-deftest test-repo-buffer-prs-section-nil-number-no-crash ()
  "GIVEN a PR with nil number
WHEN inserting the PRs section
THEN no error is signaled (uses %s not %d)."
  (require 'shipit-repo-buffer)
  (let ((open-prs `(((number . nil) (title . "Numberless PR")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-prs-section
           open-prs nil nil "test/repo"))
        (should (string-match-p "Numberless PR" (buffer-string)))))))

(ert-deftest test-repo-buffer-issues-section-renders-open-closed-subsections ()
  "GIVEN open and closed issue lists
WHEN inserting the issues section
THEN the buffer contains Open/Closed subsection headings with counts."
  (require 'shipit-repo-buffer)
  (let ((open-issues `(((number . 10) (title . "Bug report"))
                       ((number . 8) (title . "Feature request"))))
        (closed-issues `(((number . 3) (title . "Fixed bug")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-issues-section
           open-issues closed-issues nil "test/repo"))
        (let ((content (buffer-string)))
          (should (string-match-p "Open (2)" content))
          (should (string-match-p "Closed (1)" content))
          (should (string-match-p "#10" content))
          (should (string-match-p "Bug report" content))
          (should (string-match-p "#3" content))
          (should (string-match-p "Fixed bug" content)))))))

(ert-deftest test-repo-buffer-issues-section-uses-correct-icon-name ()
  "GIVEN issue data
WHEN inserting the issues section
THEN the icon function is called with 'issue-open' (not 'issue')."
  (require 'shipit-repo-buffer)
  (let ((icon-names nil)
        (open-issues `(((number . 1) (title . "Test")))))
    (cl-letf (((symbol-function 'shipit--get-pr-field-icon)
               (lambda (name fallback)
                 (push name icon-names)
                 fallback)))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-issues-section
             open-issues nil nil "test/repo"))
          (should (member "issue-open" icon-names)))))))

(ert-deftest test-repo-buffer-issues-section-nil-shows-nothing ()
  "GIVEN nil open AND nil closed issue data
WHEN inserting the issues section
THEN no section is inserted."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test")
        (shipit-repo-buffer--insert-issues-section nil nil nil "test/repo"))
      (should-not (string-match-p "Issues" (buffer-string))))))

(ert-deftest test-repo-buffer-issues-section-load-more-shown-when-has-more ()
  "GIVEN closed issues with has-more flag set
WHEN inserting the issues section
THEN a 'load more' line is shown."
  (require 'shipit-repo-buffer)
  (let ((closed-issues `(((number . 1) (title . "Old issue")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-issues-section
           nil closed-issues t "test/repo"))
        (should (string-match-p "load more" (buffer-string)))))))

(ert-deftest test-repo-buffer-discussions-section-renders-items ()
  "GIVEN a list of discussion alists
WHEN inserting the discussions section
THEN the buffer contains discussion numbers and titles."
  (require 'shipit-repo-buffer)
  (let ((discussions `(((number . 5) (title . "How to use X?"))
                       ((number . 3) (title . "RFC: new API")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-discussions-section discussions "test/repo"))
        (let ((content (buffer-string)))
          (should (string-match-p "#5" content))
          (should (string-match-p "How to use X?" content))
          (should (string-match-p "#3" content))
          (should (string-match-p "RFC: new API" content)))))))

(ert-deftest test-repo-buffer-discussions-section-nil-shows-nothing ()
  "GIVEN nil discussion data
WHEN inserting the discussions section
THEN no section is inserted."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test")
        (shipit-repo-buffer--insert-discussions-section nil "test/repo"))
      (should-not (string-match-p "Discussions" (buffer-string))))))

(ert-deftest test-repo-buffer-discussions-section-uses-correct-icon-name ()
  "GIVEN discussion data
WHEN inserting the discussions section
THEN the icon function is called with 'discussion' (not 'comment')."
  (require 'shipit-repo-buffer)
  (let ((icon-names nil)
        (discussions `(((number . 1) (title . "Test")))))
    (cl-letf (((symbol-function 'shipit--get-pr-field-icon)
               (lambda (name fallback)
                 (push name icon-names)
                 fallback)))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-discussions-section discussions "test/repo"))
          (should (member "discussion" icon-names)))))))

(ert-deftest test-repo-buffer-sections-hidden-when-features-disabled ()
  "GIVEN shipit-issues-enabled and shipit-discussions-enabled are nil
WHEN refreshing the repo buffer
THEN no issues or discussions sections are present."
  (require 'shipit-repo-buffer)
  (let ((shipit-issues-enabled nil)
        (shipit-discussions-enabled nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--fetch-repo-data)
               (lambda (_repo)
                 (list :data '((full_name . "test/repo")
                               (owner . ((login . "test")))
                               (html_url . "https://github.com/test/repo"))
                       :readme nil :languages nil)))
              ((symbol-function 'shipit-repo-buffer--fetch-open-prs)
               (lambda (_repo) '(((number . 1) (title . "PR")))))
              ((symbol-function 'shipit-repo-buffer--fetch-closed-prs)
               (lambda (_repo) nil))
              ((symbol-function 'shipit-repo-buffer--fetch-open-issues)
               (lambda (_repo) '(((number . 2) (title . "Issue")))))
              ((symbol-function 'shipit-repo-buffer--fetch-closed-issues)
               (lambda (_repo) nil))
              ((symbol-function 'shipit-repo-buffer--fetch-discussions)
               (lambda (_repo) '(((number . 3) (title . "Discussion")))))
              ((symbol-function 'shipit-pr-github--get-repo-subscription)
               (lambda (_config) nil)))
      (with-temp-buffer
        (shipit-repo-mode)
        (setq shipit-repo-buffer-repo "test/repo")
        (shipit-repo-buffer-refresh)
        (let ((content (buffer-string)))
          (should-not (string-match-p "Issues" content))
          (should-not (string-match-p "Discussions" content)))))))

(ert-deftest test-repo-buffer-ret-on-pr-item-dispatches ()
  "GIVEN a PR item with text property shipit-repo-pr-number
WHEN invoking RET on the item
THEN shipit-open-pr-buffer is called with the correct number and repo."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-open-pr-buffer)
               (lambda (number &optional repo &rest _)
                 (setq called-with (list number repo)))))
      (with-temp-buffer
        (shipit-repo-mode)
        (setq shipit-repo-buffer-repo "test/repo")
        (let ((inhibit-read-only t))
          (insert (propertize "#42  Some PR title"
                              'shipit-repo-pr-number 42
                              'shipit-repo-item-repo "test/repo")))
        (goto-char (point-min))
        (shipit-repo-buffer--ret-action)
        (should (equal called-with '(42 "test/repo")))))))

(ert-deftest test-repo-buffer-ret-on-issue-item-dispatches ()
  "GIVEN an issue item with text property shipit-repo-issue-number
WHEN invoking RET on the item
THEN shipit-issues-open-buffer is called with the correct number and repo."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-issues-open-buffer)
               (lambda (number &optional repo &rest _)
                 (setq called-with (list number repo)))))
      (with-temp-buffer
        (shipit-repo-mode)
        (setq shipit-repo-buffer-repo "test/repo")
        (let ((inhibit-read-only t))
          (insert (propertize "#10  Bug report"
                              'shipit-repo-issue-number 10
                              'shipit-repo-item-repo "test/repo")))
        (goto-char (point-min))
        (shipit-repo-buffer--ret-action)
        (should (equal called-with '(10 "test/repo")))))))

(ert-deftest test-repo-buffer-ret-on-discussion-item-dispatches ()
  "GIVEN a discussion item with text property shipit-repo-discussion-number
WHEN invoking RET on the item
THEN shipit-discussions-open-buffer is called with the correct number and repo."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-discussions-open-buffer)
               (lambda (number &optional repo)
                 (setq called-with (list number repo)))))
      (with-temp-buffer
        (shipit-repo-mode)
        (setq shipit-repo-buffer-repo "test/repo")
        (let ((inhibit-read-only t))
          (insert (propertize "#5  How to use X?"
                              'shipit-repo-discussion-number 5
                              'shipit-repo-item-repo "test/repo")))
        (goto-char (point-min))
        (shipit-repo-buffer--ret-action)
        (should (equal called-with '(5 "test/repo")))))))

(ert-deftest test-repo-buffer-plus-on-load-more-dispatches ()
  "GIVEN a line with shipit-repo-load-more text property
WHEN invoking + on the line
THEN shipit-repo-buffer--load-more is called with the correct type."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--load-more)
               (lambda (type &optional _count)
                 (setq called-with type))))
      (with-temp-buffer
        (shipit-repo-mode)
        (setq shipit-repo-buffer-repo "test/repo")
        (let ((inhibit-read-only t))
          (insert (propertize "   ... load more ...\n"
                              'shipit-repo-load-more 'prs)))
        (goto-char (point-min))
        (shipit-repo-buffer--load-more-at-point)
        (should (eq called-with 'prs))))))

(ert-deftest test-repo-buffer-prs-section-has-text-properties ()
  "GIVEN PR data
WHEN inserting the PRs section
THEN each item line has shipit-repo-pr-number and shipit-repo-item-repo text properties."
  (require 'shipit-repo-buffer)
  (let ((open-prs `(((number . 42) (title . "Add feature X")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (repo-root)
          (magit-insert-heading "Test")
          (shipit-repo-buffer--insert-prs-section
           open-prs nil nil "test/repo"))
        (goto-char (point-min))
        (let ((found-number nil)
              (found-repo nil))
          (while (and (not found-number) (not (eobp)))
            (when (get-text-property (point) 'shipit-repo-pr-number)
              (setq found-number (get-text-property (point) 'shipit-repo-pr-number))
              (setq found-repo (get-text-property (point) 'shipit-repo-item-repo)))
            (forward-char 1))
          (should (equal found-number 42))
          (should (equal found-repo "test/repo")))))))

(ert-deftest test-repo-buffer-section-visibility-collapses-new-sections ()
  "GIVEN repo-prs, repo-issues, repo-discussions section types
WHEN checking section visibility
THEN all return 'hide (collapsed by default)."
  (require 'shipit-repo-buffer)
  (dolist (type '(repo-prs repo-issues repo-discussions))
    (let ((section (make-instance 'magit-section)))
      (oset section type type)
      (should (eq 'hide (shipit-repo-buffer--section-visibility section))))))

;;; Section-aware + (load more) tests

(ert-deftest test-repo-buffer-plus-from-pr-item-in-closed-section-loads-more ()
  "GIVEN point on a PR item line inside a closed PRs subsection with has-more
WHEN invoking + (load-more-at-point)
THEN shipit-repo-buffer--load-more is called with 'prs."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--load-more)
               (lambda (type &optional _count) (setq called-with type))))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-prs-section
             nil
             `(((number . 10) (title . "Old PR")))
             t "test/repo"))
          ;; Move to the PR item line (not the load-more line)
          (goto-char (point-min))
          (text-property-search-forward 'shipit-repo-pr-number)
          (setq shipit-repo-buffer-closed-prs-has-more t)
          (shipit-repo-buffer--load-more-at-point)
          (should (eq called-with 'prs)))))))

(ert-deftest test-repo-buffer-plus-from-issue-item-in-closed-section-loads-more ()
  "GIVEN point on an issue item line inside a closed issues subsection with has-more
WHEN invoking + (load-more-at-point)
THEN shipit-repo-buffer--load-more is called with 'issues."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--load-more)
               (lambda (type &optional _count) (setq called-with type))))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-issues-section
             nil
             `(((number . 5) (title . "Old issue")))
             t "test/repo"))
          (goto-char (point-min))
          (text-property-search-forward 'shipit-repo-issue-number)
          (setq shipit-repo-buffer-closed-issues-has-more t)
          (shipit-repo-buffer--load-more-at-point)
          (should (eq called-with 'issues)))))))

(ert-deftest test-repo-buffer-plus-from-open-subsection-shows-message ()
  "GIVEN point on a PR item in the open subsection (no has-more)
WHEN invoking + (load-more-at-point)
THEN no load-more is dispatched and a message is shown."
  (require 'shipit-repo-buffer)
  (let ((called nil)
        (messages nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--load-more)
               (lambda (_type) (setq called t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-prs-section
             `(((number . 42) (title . "New PR")))
             nil nil "test/repo"))
          (goto-char (point-min))
          (text-property-search-forward 'shipit-repo-pr-number)
          (setq shipit-repo-buffer-closed-prs-has-more nil)
          (shipit-repo-buffer--load-more-at-point)
          (should-not called)
          (should (cl-some (lambda (m) (string-match-p "load more" m)) messages)))))))

(ert-deftest test-repo-buffer-plus-outside-section-shows-message ()
  "GIVEN point outside any PR/issue section
WHEN invoking + (load-more-at-point)
THEN no load-more is dispatched and a message is shown."
  (require 'shipit-repo-buffer)
  (let ((called nil)
        (messages nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--load-more)
               (lambda (_type) (setq called t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test repo")
            (insert "   Some text\n")))
        (goto-char (point-min))
        (shipit-repo-buffer--load-more-at-point)
        (should-not called)
        (should messages)))))

(ert-deftest test-repo-buffer-plus-with-prefix-arg-passes-count ()
  "GIVEN point in a PRs section with has-more
WHEN invoking C-u 100 + (load-more-at-point with prefix arg 100)
THEN shipit-repo-buffer--load-more is called with type and count 100."
  (require 'shipit-repo-buffer)
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--load-more)
               (lambda (type &optional count)
                 (setq called-with (list type count)))))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (repo-root)
            (magit-insert-heading "Test")
            (shipit-repo-buffer--insert-prs-section
             nil
             `(((number . 10) (title . "Old PR")))
             t "test/repo"))
          (goto-char (point-min))
          (text-property-search-forward 'shipit-repo-pr-number)
          (setq shipit-repo-buffer-closed-prs-has-more t)
          (let ((current-prefix-arg 100))
            (shipit-repo-buffer--load-more-at-point))
          (should (equal called-with '(prs 100))))))))

(ert-deftest test-repo-buffer-fetch-more-uses-count-arg ()
  "GIVEN a count argument of 50
WHEN fetching more closed PRs
THEN the new limit is current-count + 50 (not the default)."
  (require 'shipit-repo-buffer)
  (let ((search-args nil)
        (shipit-repo-buffer-closed-prs '(((number . 1)) ((number . 2))))
        (shipit-repo-buffer-repo "test/repo")
        (shipit-repo-buffer-closed-limit 5)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :search (lambda (_config args)
                                      (setq search-args args)
                                      nil))
                       nil))))
      (shipit-repo-buffer--fetch-more 'prs 50)
      ;; current-count=2 + count=50 = 52
      (should (member "--limit=52" search-args)))))

(ert-deftest test-repo-buffer-load-more-preserves-filter ()
  "GIVEN an active title filter 'bug'
WHEN loading more closed PRs
THEN the filter remains active after refresh."
  (require 'shipit-repo-buffer)
  (let ((refresh-called nil))
    (cl-letf (((symbol-function 'shipit-repo-buffer--fetch-more) #'ignore)
              ((symbol-function 'shipit-repo-buffer--refresh-filtered-section)
               (lambda () (setq refresh-called t))))
      (with-temp-buffer
        (shipit-repo-mode)
        (setq shipit-repo-buffer--filter-text "bug")
        (setq shipit-repo-buffer-closed-prs '(((number . 1) (title . "Fix bug"))))
        (setq shipit-repo-buffer-closed-prs-has-more t)
        (shipit-repo-buffer--load-more 'prs)
        (should refresh-called)
        (should (equal shipit-repo-buffer--filter-text "bug"))))))

;;; Filter tests

(ert-deftest test-repo-buffer-filter-by-title ()
  "GIVEN items with various titles
WHEN filtering by title text 'bug'
THEN only items with 'bug' in the title match."
  (require 'shipit-repo-buffer)
  (let ((shipit-repo-buffer--filter-text "bug")
        (shipit-repo-buffer--filter-mode nil))
    (should (shipit-repo-buffer--item-matches-filter-p
             '((title . "Fix the bug") (number . 1))))
    (should (shipit-repo-buffer--item-matches-filter-p
             '((title . "BUG report") (number . 2))))
    (should-not (shipit-repo-buffer--item-matches-filter-p
                 '((title . "Add feature") (number . 3))))))

(ert-deftest test-repo-buffer-filter-by-title-empty ()
  "GIVEN empty filter text
WHEN checking if items match
THEN all items match."
  (require 'shipit-repo-buffer)
  (let ((shipit-repo-buffer--filter-text "")
        (shipit-repo-buffer--filter-mode nil))
    (should (shipit-repo-buffer--item-matches-filter-p
             '((title . "Any title") (number . 1))))))

(ert-deftest test-repo-buffer-filter-mine ()
  "GIVEN items authored by different users
WHEN filtering by 'mine' mode
THEN only items authored by the current user match."
  (require 'shipit-repo-buffer)
  (let ((shipit-repo-buffer--filter-text "")
        (shipit-repo-buffer--filter-mode 'mine))
    (cl-letf (((symbol-function 'shipit--get-github-username)
               (lambda () "octocat")))
      (should (shipit-repo-buffer--item-matches-filter-p
               '((title . "My PR") (number . 1) (user . ((login . "octocat"))))))
      (should-not (shipit-repo-buffer--item-matches-filter-p
                   '((title . "Someone else") (number . 2) (user . ((login . "other")))))))))

(ert-deftest test-repo-buffer-filter-mine-matches-git-user-name ()
  "GIVEN Jira-style items where login is the display name
WHEN filtering by 'mine' mode
THEN items match against git user.name (not just GitHub username)."
  (require 'shipit-repo-buffer)
  (let ((shipit-repo-buffer--filter-text "")
        (shipit-repo-buffer--filter-mode 'mine)
        (shipit-repo-buffer--my-usernames nil))
    (cl-letf (((symbol-function 'shipit--get-github-username)
               (lambda () "octocat"))
              ((symbol-function 'shell-command-to-string)
               (lambda (cmd)
                 (if (string-match-p "user\\.name" cmd)
                     "Octo Cat\n"
                   ""))))
      ;; Jira item with display name as login
      (should (shipit-repo-buffer--item-matches-filter-p
               '((title . "Jira issue") (number . "PROJ-1")
                 (user . ((login . "Octo Cat"))))))
      ;; GitHub item still works
      (should (shipit-repo-buffer--item-matches-filter-p
               '((title . "GH PR") (number . 1)
                 (user . ((login . "octocat"))))))
      ;; Someone else doesn't match
      (should-not (shipit-repo-buffer--item-matches-filter-p
                   '((title . "Other") (number . 2)
                     (user . ((login . "someone-else")))))))))

(ert-deftest test-repo-buffer-clear-filter-restores-all ()
  "GIVEN a filter is active
WHEN clearing the filter
THEN filter text is empty and mode is nil."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (shipit-repo-mode)
    (setq shipit-repo-buffer--filter-text "bug")
    (setq shipit-repo-buffer--filter-mode 'mine)
    (cl-letf (((symbol-function 'shipit-repo-buffer--refresh-filtered-section)
               #'ignore))
      (shipit-repo-buffer--clear-filter)
      (should (string-empty-p shipit-repo-buffer--filter-text))
      (should (null shipit-repo-buffer--filter-mode)))))

(ert-deftest test-repo-buffer-in-items-section-p-in-prs ()
  "GIVEN point inside a repo-prs section
WHEN checking shipit-repo-buffer--in-items-section-p
THEN it returns non-nil."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test")
        (shipit-repo-buffer--insert-prs-section
         `(((number . 1) (title . "PR")))
         nil nil "test/repo"))
      (goto-char (point-min))
      (text-property-search-forward 'shipit-repo-pr-number)
      (should (shipit-repo-buffer--in-items-section-p)))))

(ert-deftest test-repo-buffer-in-items-section-p-outside ()
  "GIVEN point outside any items section
WHEN checking shipit-repo-buffer--in-items-section-p
THEN it returns nil."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test repo")
        (insert "   Some text\n")))
    (goto-char (point-min))
    (should-not (shipit-repo-buffer--in-items-section-p))))

(ert-deftest test-repo-buffer-filter-outside-section-errors ()
  "GIVEN point outside any items section
WHEN invoking f (shipit-repo-buffer-filter)
THEN a user-error is signaled."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test repo")
        (insert "   Some text\n")))
    (goto-char (point-min))
    (should-error (shipit-repo-buffer-filter) :type 'user-error)))

(ert-deftest test-repo-buffer-load-more-preserves-cursor-position ()
  "GIVEN point at position 50 in the buffer
WHEN loading more items (refresh moves point to beginning)
THEN point is restored to the original position."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test")
        (shipit-repo-buffer--insert-prs-section
         nil
         `(((number . 1) (title . "PR one"))
           ((number . 2) (title . "PR two"))
           ((number . 3) (title . "PR three")))
         t "test/repo")))
    ;; Move to the 3rd PR item
    (goto-char (point-min))
    (text-property-search-forward 'shipit-repo-pr-number)
    (text-property-search-forward 'shipit-repo-pr-number)
    (text-property-search-forward 'shipit-repo-pr-number)
    (let ((pos-before (point)))
      (cl-letf (((symbol-function 'shipit-repo-buffer--fetch-more) #'ignore)
                ((symbol-function 'shipit-repo-buffer--refresh-filtered-section)
                 (lambda ()
                   ;; Simulate what the real refresh does: move point to
                   ;; beginning of section (save-excursion collapses to
                   ;; content-pos after delete+reinsert)
                   (goto-char (point-min)))))
        (shipit-repo-buffer--load-more 'prs)
        (should (= (point) pos-before))))))

(ert-deftest test-repo-buffer-filter-mine-empty-shows-subsection-headers ()
  "GIVEN issues section with open and closed issues
WHEN filtering by 'mine' and no items match
THEN Open (0) and Closed (0) subsection headers still appear."
  (require 'shipit-repo-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (shipit-repo-buffer-repo "test/repo")
          (shipit-repo-buffer-open-issues
           '(((number . "PROJ-1") (title . "Issue") (user . ((login . "someone"))))))
          (shipit-repo-buffer-closed-issues
           '(((number . "PROJ-2") (title . "Old") (user . ((login . "someone"))))))
          (shipit-repo-buffer-open-issues-has-more nil)
          (shipit-repo-buffer-closed-issues-has-more nil)
          (shipit-repo-buffer--filter-mode 'mine)
          (shipit-repo-buffer--filter-text "")
          (shipit-repo-buffer--my-usernames '("not-someone")))
      (magit-insert-section (repo-root)
        (magit-insert-heading "Test")
        (magit-insert-section (repo-issues nil nil)
          (magit-insert-heading "Issues")
          (insert "   placeholder\n")))
      ;; Point is inside issues section
      (goto-char (point-min))
      (search-forward "placeholder")
      (cl-letf (((symbol-function 'shipit--find-section-by-type)
                 (lambda (type)
                   (when (eq type 'repo-issues)
                     (let ((section (magit-current-section)))
                       (while (and section
                                   (not (eq (oref section type) 'repo-issues)))
                         (setq section (oref section parent)))
                       section)))))
        (shipit-repo-buffer--refresh-issues-section-filtered)
        (let ((text (buffer-string)))
          (should (string-match-p "Open (0)" text))
          (should (string-match-p "Closed (0)" text)))))))

;;; Discussion filter tests

(ert-deftest test-shipit-repo-buffer-discussion-filter-twice ()
  "GIVEN a discussions section with items
WHEN filtering twice in succession
THEN the section is found and refreshed both times."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-repo-buffer-repo "octocat/hello-world")
          (shipit-repo-buffer-discussions
           '(((number . 1) (title . "Alpha topic") (user . ((login . "alice"))))
             ((number . 2) (title . "Beta topic") (user . ((login . "bob"))))
             ((number . 3) (title . "Gamma topic") (user . ((login . "alice")))))))
      (magit-insert-section (root)
        (shipit-repo-buffer--insert-discussions-section
         shipit-repo-buffer-discussions "octocat/hello-world"))
      ;; WHEN filtering by "Alpha"
      (setq shipit-repo-buffer--filter-text "Alpha")
      (setq shipit-repo-buffer--filter-mode nil)
      (setq shipit-repo-buffer--filter-section-type 'repo-discussions)
      (shipit-repo-buffer--refresh-filtered-section)
      ;; THEN only Alpha is shown
      (let ((text (buffer-string)))
        (should (string-match-p "Alpha" text))
        (should-not (string-match-p "Beta" text)))
      ;; WHEN filtering again by "Beta"
      (setq shipit-repo-buffer--filter-text "Beta")
      (shipit-repo-buffer--refresh-filtered-section)
      ;; THEN only Beta is shown
      (let ((text (buffer-string)))
        (should (string-match-p "Beta" text))
        (should-not (string-match-p "Alpha" text))))))

(ert-deftest test-shipit-repo-buffer-discussion-section-type-on-item ()
  "GIVEN a discussions section with items
WHEN point is on a discussion item line
THEN section-type-at-point returns repo-discussions."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-repo-buffer-repo "octocat/hello-world")
          (discussions
           '(((number . 1) (title . "Alpha topic") (user . ((login . "alice")))))))
      (magit-insert-section (root)
        (shipit-repo-buffer--insert-discussions-section
         discussions "octocat/hello-world"))
      ;; Move point to the item line
      (goto-char (point-min))
      (search-forward "Alpha" nil t)
      ;; THEN section type is repo-discussions
      (should (eq 'repo-discussions
                  (shipit-repo-buffer--section-type-at-point))))))

(ert-deftest test-shipit-repo-buffer-discussion-magit-section-prop-after-refresh ()
  "GIVEN a discussions section with items
WHEN refreshing the section body via filter
THEN re-inserted item lines still have the magit-section text property
     so magit-current-section resolves correctly."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-repo-buffer-repo "octocat/hello-world")
          (shipit-repo-buffer-discussions
           '(((number . 1) (title . "Alpha topic") (user . ((login . "alice"))))
             ((number . 2) (title . "Beta topic") (user . ((login . "bob")))))))
      (magit-insert-section (root)
        (shipit-repo-buffer--insert-discussions-section
         shipit-repo-buffer-discussions "octocat/hello-world"))
      ;; Check magit-section property before refresh
      (goto-char (point-min))
      (search-forward "Alpha" nil t)
      (let ((prop-before (get-text-property (point) 'magit-section)))
        (should prop-before)
        ;; Refresh with empty filter (show all)
        (setq shipit-repo-buffer--filter-text "")
        (setq shipit-repo-buffer--filter-mode nil)
        (setq shipit-repo-buffer--filter-section-type 'repo-discussions)
        (shipit-repo-buffer--refresh-filtered-section)
        ;; Check magit-section property after refresh
        (goto-char (point-min))
        (search-forward "Alpha" nil t)
        (let ((prop-after (get-text-property (point) 'magit-section)))
          ;; THEN item lines still have magit-section property
          (should prop-after))))))

(provide 'test-shipit-repo-buffer)
;;; test-shipit-repo-buffer.el ends here
