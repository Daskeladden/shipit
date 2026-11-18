;;; test-shipit-repo-buffer-lists.el --- The lists of the repo buffer load in the background -*- lexical-binding: t; -*-

;;; Commentary:

;; The repo buffer shows the header and the README first, and then
;; fills its lists of pull requests, issues and discussions as GitHub
;; or GitLab answers.  These tests open the buffer with its command
;; against a forge that answers at url.el and at curl, and they decide
;; when the answers to the background requests arrive.
;;
;; Each scenario runs on GitHub and on GitLab.  GitLab has no
;; discussions.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'seq)
(require 'shipit-test-http)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-pr-gitlab)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-gitlab)
(require 'shipit-issues)
(require 'shipit-discussions-graphql)
(require 'shipit-repo-buffer)

;;; The forges

(defconst test-repo-lists--github
  '(:backend github :config nil :repo "acme/coffee"
             :prs "Pull Requests" :discussions t)
  "A repository on GitHub.")

(defconst test-repo-lists--gitlab
  '(:backend gitlab
             :config (:api-url "https://gitlab.example.com"
                               :project-path "cafe/espresso" :token "test-token")
             :repo "cafe/espresso" :prs "Merge Requests" :discussions nil)
  "A project on GitLab.")

(defconst test-repo-lists--readme "# Coffee\n\nHow we brew it in the kitchen.\n"
  "The README of each repository.")

(defconst test-repo-lists--default-counts
  '(:open-prs 1234 :closed-prs 40210 :open-issues 567 :closed-issues 3
              :discussions 312)
  "How many items each list of the repository has.")

;;; The items of a list

(defun test-repo-lists--title (list number)
  "The title of item NUMBER of LIST."
  (format "%s %d" list number))

(defun test-repo-lists--param (address name)
  "The number of the query parameter NAME of ADDRESS, or nil."
  (when (string-match (format "[?&]%s=\\([0-9]+\\)" name) address)
    (string-to-number (match-string 1 address))))

(defun test-repo-lists--numbers (address total)
  "The item numbers of the page ADDRESS asks for, of a list of TOTAL."
  (let* ((per-page (or (test-repo-lists--param address "per_page") 20))
         (page (or (test-repo-lists--param address "page") 1))
         (first (1+ (* (1- page) per-page)))
         (last (min total (* page per-page))))
    (when (<= first last)
      (number-sequence first last))))

(defun test-repo-lists--github-list (address)
  "The list that the search of GitHub at ADDRESS asks for."
  (let ((open (string-match-p "state%3Aopen" address)))
    (if (string-match-p "is%3Apr" address)
        (if open 'open-prs 'closed-prs)
      (if open 'open-issues 'closed-issues))))

(defun test-repo-lists--gitlab-list (address)
  "The list that the request to GitLab at ADDRESS asks for."
  (let ((open (string-match-p "state=opened" address)))
    (if (string-match-p "/merge_requests" address)
        (if open 'open-prs 'closed-prs)
      (if open 'open-issues 'closed-issues))))

(defun test-repo-lists--status (counts list)
  "The status of the answer for LIST: 200, or the failure in COUNTS."
  (or (alist-get list (plist-get counts :failures)) 200))

;;; GitHub's answers

(defun test-repo-lists--github-item (list number)
  "Item NUMBER of LIST, as GitHub's search gives it."
  `((number . ,number)
    (title . ,(test-repo-lists--title list number))
    (state . ,(if (memq list '(open-prs open-issues)) "open" "closed"))
    (user (login . "barista"))
    (labels . [])
    (html_url . ,(format "https://github.com/acme/coffee/issues/%d" number))))

(defun test-repo-lists--github-search (counts)
  "The body of GitHub's search for the lists of COUNTS, at the address asked."
  (let* ((list (test-repo-lists--github-list shipit-test-http-address))
         (total (plist-get counts (intern (format ":%s" list)))))
    `((total_count . ,total)
      (incomplete_results . :json-false)
      (items . ,(vconcat
                 (mapcar (lambda (number) (test-repo-lists--github-item list number))
                         (test-repo-lists--numbers shipit-test-http-address total)))))))

(defun test-repo-lists--discussion (number)
  "Discussion NUMBER, as GitHub's GraphQL gives it."
  `((id . ,(format "D_%d" number))
    (number . ,number)
    (title . ,(test-repo-lists--title 'discussions number))
    (author (login . "barista"))
    (category (name . "Q&A") (emoji . ":pray:") (isAnswerable . t))
    (comments (totalCount . 0))
    (createdAt . "2026-11-01T10:00:00Z")
    (updatedAt . "2026-11-01T10:00:00Z")))

(defun test-repo-lists--first (request)
  "How many discussions REQUEST, the body of a GraphQL query, asks for."
  (let ((variables (alist-get 'variables (json-read-from-string request))))
    (alist-get 'first variables)))

(defun test-repo-lists--graphql (counts)
  "A function that answers a GraphQL query for the discussions of COUNTS."
  (lambda (request)
    (if (string-match-p "DISCUSSION" request)
        (let* ((total (plist-get counts :discussions))
               (shown (min total (test-repo-lists--first request))))
          `((data (search (discussionCount . ,total)
                          (nodes . ,(vconcat
                                     (mapcar #'test-repo-lists--discussion
                                             (and (> shown 0) (number-sequence 1 shown)))))
                          (pageInfo (hasNextPage . ,(if (< shown total) t :json-false))
                                    (endCursor . "cursor"))))))
      '((data)))))

(defun test-repo-lists--github-answers (counts)
  "GitHub's answers for the repository acme/coffee with the lists of COUNTS."
  (list
   (list "/repos/acme/coffee\\'" 200
         '((full_name . "acme/coffee")
           (description . "Coffee for the whole kitchen")
           (owner (login . "acme"))
           (html_url . "https://github.com/acme/coffee")
           (default_branch . "main")))
   (list "/repos/acme/coffee/readme" 200
         `((name . "README.md")
           (encoding . "base64")
           (content . ,(base64-encode-string test-repo-lists--readme))))
   (list "/repos/acme/coffee/languages" 200 '((Lisp . 100)))
   (list "/repos/acme/coffee/subscription" 404 "{}")
   shipit-test-http-not-starred
   (list "/search/issues"
         (lambda ()
           (test-repo-lists--status
            counts (test-repo-lists--github-list shipit-test-http-address)))
         (lambda (_request) (test-repo-lists--github-search counts)))
   (list "/graphql\\'" 200 (test-repo-lists--graphql counts))))

;;; GitLab's answers

(defun test-repo-lists--gitlab-item (list number)
  "Item NUMBER of LIST, as GitLab gives a merge request or an issue."
  `((id . ,(+ 900000 number))
    (iid . ,number)
    (title . ,(test-repo-lists--title list number))
    (state . ,(if (memq list '(open-prs open-issues)) "opened" "closed"))
    (author (username . "barista") (name . "Barista"))
    (labels . [])
    (assignees . [])
    (web_url . ,(format "https://gitlab.example.com/cafe/espresso/-/issues/%d" number))))

(defun test-repo-lists--gitlab-total (counts)
  "The total of the list at the address asked, from COUNTS."
  (plist-get counts (intern (format ":%s" (test-repo-lists--gitlab-list
                                           shipit-test-http-address)))))

(defun test-repo-lists--gitlab-page (counts)
  "The body of GitLab's answer for a page of the lists of COUNTS."
  (let ((list (test-repo-lists--gitlab-list shipit-test-http-address)))
    (vconcat (mapcar (lambda (number) (test-repo-lists--gitlab-item list number))
                     (test-repo-lists--numbers shipit-test-http-address
                                               (test-repo-lists--gitlab-total counts))))))

(defun test-repo-lists--gitlab-answers (counts)
  "GitLab's answers for the project cafe/espresso with the lists of COUNTS."
  (list
   (list "/projects/cafe%2Fespresso\\'" 200
         '((id . 77)
           (path_with_namespace . "cafe/espresso")
           (description . "Coffee for the whole kitchen")
           (namespace (name . "cafe"))
           (web_url . "https://gitlab.example.com/cafe/espresso")
           (default_branch . "main")))
   (list "/repository/tree" 200 '[((name . "README.md") (type . "blob"))])
   (list "/repository/files/README.md/raw" 200 test-repo-lists--readme)
   (list "/projects/cafe%2Fespresso/languages" 200 '((Lisp . 100.0)))
   (list "/notification_settings" 200 '((level . "participating")))
   (list "/projects\\?starred=true" 200 [])
   (list "/projects/cafe%2Fespresso/\\(merge_requests\\|issues\\)\\?"
         (lambda ()
           (test-repo-lists--status
            counts (test-repo-lists--gitlab-list shipit-test-http-address)))
         (lambda (_request) (test-repo-lists--gitlab-page counts))
         (lambda ()
           `(("X-Total" . ,(number-to-string (test-repo-lists--gitlab-total counts))))))))

;;; Opening the repo buffer

(defun test-repo-lists--answers (forge counts)
  "The answers of FORGE for a repository with the lists of COUNTS."
  (if (eq (plist-get forge :backend) 'github)
      (test-repo-lists--github-answers counts)
    (test-repo-lists--gitlab-answers counts)))

(defun test-repo-lists--buffer-name (forge)
  "The name of the repo buffer of the repository of FORGE."
  (format "*shipit-repo: %s*" (plist-get forge :repo)))

(defmacro test-repo-lists--on (forge counts &rest body)
  "Run BODY against FORGE with a repository whose lists have COUNTS.
The answers to the requests that run in the background wait until BODY
gives them with `shipit-test-http-release'."
  (declare (indent 2))
  `(let* ((forge ,forge)
          (answers (test-repo-lists--answers forge ,counts))
          (shipit-github-token "test-token")
          (shipit-issues-enabled t)
          (shipit-discussions-enabled t)
          (shipit-issue-backend (plist-get forge :backend))
          (shipit-issue-backend-config (plist-get forge :config))
          (shipit-issue-repo-backends nil)
          (shipit-pr-backend (plist-get forge :backend))
          (shipit-pr-backend-config (plist-get forge :config))
          (shipit-test-http-hold t))
     (unwind-protect
         (shipit-test-http-answering answers
           (cl-letf (((symbol-function 'call-process)
                      (shipit-test-http-curl-stand-in answers)))
             ,@body))
       (when (get-buffer (test-repo-lists--buffer-name forge))
         (kill-buffer (test-repo-lists--buffer-name forge))))))

(defun test-repo-lists--open (forge)
  "Open the repo buffer of the repository of FORGE with its command."
  (shipit-open-repo-buffer (plist-get forge :repo)
                           (plist-get forge :backend)
                           (plist-get forge :config)))

(defun test-repo-lists--expand (forge)
  "Expand each section of the repo buffer of FORGE, as the user does to read it.
A collapsed section gets its body only when it is expanded."
  (with-current-buffer (test-repo-lists--buffer-name forge)
    (magit-section-show-level-4-all)))

(defun test-repo-lists--text (forge)
  "The text of the repo buffer of FORGE, with each section expanded."
  (test-repo-lists--expand forge)
  (with-current-buffer (test-repo-lists--buffer-name forge)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun test-repo-lists--shows-p (forge regexp)
  "Whether the repo buffer of FORGE shows a line that REGEXP matches."
  (string-match-p regexp (test-repo-lists--text forge)))

(defun test-repo-lists--count (forge regexp)
  "How many lines of the repo buffer of FORGE REGEXP matches."
  (let ((text (test-repo-lists--text forge))
        (count 0)
        (start 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun test-repo-lists--items (forge list)
  "How many items of LIST the repo buffer of FORGE shows."
  (test-repo-lists--count forge (format "#[0-9]+  %s [0-9]+$" list)))

(defun test-repo-lists--list-requests (forge)
  "The requests for lists that the repo buffer of FORGE made, as addresses."
  (seq-filter (lambda (address)
                (string-match-p "/search/issues\\|/graphql\\|/merge_requests\\?\\|/issues\\?"
                                address))
              (mapcar #'cadr shipit-test-http-requests)))

(defun test-repo-lists--held-lists ()
  "How many requests for lists wait for their answer."
  (length (shipit-test-http-held-addresses)))

(defun test-repo-lists--list-count (forge)
  "How many lists the repo buffer of FORGE asks for."
  (if (plist-get forge :discussions) 5 4))

(defun test-repo-lists--press (forge key title)
  "Press KEY in the repo buffer of FORGE on the item whose title is TITLE."
  (test-repo-lists--expand forge)
  (with-current-buffer (test-repo-lists--buffer-name forge)
    (goto-char (point-min))
    (search-forward title)
    (call-interactively (keymap-lookup shipit-repo-mode-map key))))

(defmacro test-repo-lists--deftest (name docstring &rest body)
  "Define the test NAME with DOCSTRING and BODY once for each forge.
BODY sees the forge as `forge'."
  (declare (indent 1) (doc-string 2))
  `(progn
     ,@(mapcar
        (lambda (variant)
          `(ert-deftest ,(intern (format "%s-on-%s" name (car variant))) ()
             ,docstring
             (let ((forge ,(cdr variant)))
               ,@body)))
        '((github . test-repo-lists--github)
          (gitlab . test-repo-lists--gitlab)))))

;;; Scenarios

(test-repo-lists--deftest test-shipit-repo-buffer-shows-its-readme-before-its-lists
  "The header and README show while the lists are still loading."
  ;; GIVEN a repository whose lists have not answered yet
  (test-repo-lists--on forge test-repo-lists--default-counts
    ;; WHEN the user opens its repo buffer
    (test-repo-lists--open forge)
    ;; THEN the header and the README show
    (should (test-repo-lists--shows-p forge "Coffee for the whole kitchen"))
    (should (test-repo-lists--shows-p forge "How we brew it in the kitchen"))
    ;; AND the headings of the lists say that they are loading
    (should (test-repo-lists--shows-p
             forge (format "%s (loading" (plist-get forge :prs))))
    (should (test-repo-lists--shows-p forge "Issues (loading"))
    (when (plist-get forge :discussions)
      (should (test-repo-lists--shows-p forge "Discussions (loading")))
    ;; AND the requests for all lists went out before any answered
    (should (= (test-repo-lists--held-lists) (test-repo-lists--list-count forge)))
    (shipit-test-http-release)))

(test-repo-lists--deftest test-shipit-repo-buffer-shows-the-totals-of-its-lists
  "Each list shows its first page and the total GitHub or GitLab counts."
  ;; GIVEN a repository with 1234 open and 40210 closed pull requests,
  ;; 567 open and 3 closed issues, and 312 discussions
  (test-repo-lists--on forge test-repo-lists--default-counts
    (test-repo-lists--open forge)
    ;; WHEN the lists answer, the last one asked first
    (dolist (address (reverse (shipit-test-http-held-addresses)))
      (shipit-test-http-release (regexp-quote address)))
    ;; THEN the headings give the totals
    (should (test-repo-lists--shows-p
             forge (format "%s (1234 open)" (plist-get forge :prs))))
    (should (test-repo-lists--shows-p forge "Open (100 of 1234)"))
    (should (test-repo-lists--shows-p forge "Closed (5 of 40210)"))
    (should (test-repo-lists--shows-p forge "Issues (567 open)"))
    (should (test-repo-lists--shows-p forge "Open (100 of 567)"))
    (should (test-repo-lists--shows-p forge "Closed (3)"))
    ;; AND each list shows its first page
    (should (= 100 (test-repo-lists--items forge "open-prs")))
    (should (= 5 (test-repo-lists--items forge "closed-prs")))
    (should (= 100 (test-repo-lists--items forge "open-issues")))
    (should (= 3 (test-repo-lists--items forge "closed-issues")))
    ;; AND a list that shows all its items offers no more
    (should (= 3 (test-repo-lists--count forge "to load more")))
    (when (plist-get forge :discussions)
      (should (test-repo-lists--shows-p forge "Discussions (25 of 312)"))
      (should (= 25 (test-repo-lists--items forge "discussions"))))))

(test-repo-lists--deftest test-shipit-repo-buffer-loads-the-next-page-of-a-list
  "+ in a list asks for its next page only."
  ;; GIVEN the repo buffer shows the first 100 of 1234 open pull requests
  (test-repo-lists--on forge test-repo-lists--default-counts
    (test-repo-lists--open forge)
    (shipit-test-http-release)
    (let ((asked (length (test-repo-lists--list-requests forge))))
      ;; WHEN the user types + in that list
      (test-repo-lists--press forge "+" "open-prs 1")
      (shipit-test-http-release)
      ;; THEN shipit asks for the second page only
      (let ((new (nthcdr asked (test-repo-lists--list-requests forge))))
        (should (= 1 (length new)))
        (should (equal 2 (test-repo-lists--param (car new) "page"))))
      ;; AND the list shows 200 of 1234
      (should (test-repo-lists--shows-p forge "Open (200 of 1234)"))
      (should (= 200 (test-repo-lists--items forge "open-prs"))))))

(test-repo-lists--deftest test-shipit-repo-buffer-says-which-list-failed
  "A list that fails to load says so, and the other lists show."
  ;; GIVEN a repository whose open issues fail to load with HTTP 502
  (test-repo-lists--on forge (append '(:failures ((open-issues . 502)))
                                     test-repo-lists--default-counts)
    (test-repo-lists--open forge)
    ;; WHEN the lists answer
    (shipit-test-http-release)
    ;; THEN the open issues say that they failed and why
    (should (test-repo-lists--shows-p forge "Open (failed: HTTP 502)"))
    ;; AND the other lists show
    (should (= 100 (test-repo-lists--items forge "open-prs")))
    (should (= 3 (test-repo-lists--items forge "closed-issues")))
    (should (test-repo-lists--shows-p forge "Closed (3)"))))

(test-repo-lists--deftest test-shipit-repo-buffer-refreshed-while-loading-shows-each-list-once
  "A refresh while the lists load shows each list once."
  ;; GIVEN the repo buffer is still loading its lists
  (test-repo-lists--on forge test-repo-lists--default-counts
    (test-repo-lists--open forge)
    (should (= (test-repo-lists--held-lists) (test-repo-lists--list-count forge)))
    ;; WHEN the user refreshes it, and the old and the new answers arrive
    (with-current-buffer (test-repo-lists--buffer-name forge)
      (call-interactively (keymap-lookup shipit-repo-mode-map "g")))
    (shipit-test-http-release)
    ;; THEN each list shows once
    (should (= 1 (test-repo-lists--count
                  forge (format "%s (1234 open)" (plist-get forge :prs)))))
    (should (= 1 (test-repo-lists--count forge "Open (100 of 1234)")))
    (should (= 100 (test-repo-lists--items forge "open-prs")))
    (should (= 3 (test-repo-lists--items forge "closed-issues")))))

(test-repo-lists--deftest test-shipit-repo-buffer-closed-while-loading-ignores-late-answers
  "Answers that arrive after the buffer was closed change nothing."
  ;; GIVEN the repo buffer is still loading its lists
  (test-repo-lists--on forge test-repo-lists--default-counts
    (test-repo-lists--open forge)
    (should (= (test-repo-lists--held-lists) (test-repo-lists--list-count forge)))
    ;; WHEN the user closes it, and then the answers arrive
    (kill-buffer (test-repo-lists--buffer-name forge))
    (shipit-test-http-release)
    ;; THEN no error happens and no repo buffer comes back
    (should-not (get-buffer (test-repo-lists--buffer-name forge)))))

(test-repo-lists--deftest test-shipit-repo-buffer-drops-the-heading-of-an-empty-list
  "A list without items loses its heading when it answers."
  ;; GIVEN a repository without closed issues and without discussions
  (test-repo-lists--on forge (list :open-prs 2 :closed-prs 1 :open-issues 4
                                   :closed-issues 0 :discussions 0)
    ;; WHEN the user opens its repo buffer
    (test-repo-lists--open forge)
    ;; THEN the issues say that they are loading
    (should (test-repo-lists--shows-p forge "Issues (loading"))
    ;; WHEN the lists answer
    (shipit-test-http-release)
    ;; THEN the issues show no closed heading
    (should (test-repo-lists--shows-p forge "Open (4)"))
    (should-not (test-repo-lists--shows-p forge "Closed (0"))
    ;; AND no discussions heading shows
    (should-not (test-repo-lists--shows-p forge "Discussions"))))

;;; More cases

(defmacro test-repo-lists--on-overriding (forge counts overrides &rest body)
  "Run BODY as `test-repo-lists--on' does, with OVERRIDES before the answers.
OVERRIDES are answers that win over those of the forge."
  (declare (indent 3))
  (let ((answers-of (make-symbol "answers-of")))
    `(let ((,answers-of (symbol-function 'test-repo-lists--answers)))
       (cl-letf (((symbol-function 'test-repo-lists--answers)
                  (lambda (forge counts)
                    (append ,overrides (funcall ,answers-of forge counts)))))
         (test-repo-lists--on ,forge ,counts ,@body)))))

(defun test-repo-lists--prs-section (forge)
  "The section of the pull requests in the repo buffer of FORGE."
  (with-current-buffer (test-repo-lists--buffer-name forge)
    (seq-find (lambda (section) (eq (oref section type) 'repo-prs))
              (oref magit-root-section children))))

(test-repo-lists--deftest test-shipit-repo-buffer-keeps-a-list-whose-next-page-fails
  "A next page that fails leaves the list as it was and says why."
  ;; GIVEN the repo buffer shows 100 of 1234 open pull requests
  ;; AND their second page fails with HTTP 502
  (test-repo-lists--on-overriding forge test-repo-lists--default-counts
      (list (list "[?&]page=2\\(&\\|\\'\\)" 502 "{}"))
    (test-repo-lists--open forge)
    (shipit-test-http-release)
    (ert-with-message-capture messages
      ;; WHEN the user types + in that list
      (test-repo-lists--press forge "+" "open-prs 1")
      (shipit-test-http-release)
      ;; THEN the list still shows its first 100
      (should (test-repo-lists--shows-p forge "Open (100 of 1234)"))
      (should (= 100 (test-repo-lists--items forge "open-prs")))
      ;; AND the echo area says why no more came
      (should (string-match-p "HTTP 502" messages)))))

(ert-deftest test-shipit-repo-buffer-shows-a-gitlab-list-without-its-total ()
  "A GitLab list without a total shows its count and offers more."
  ;; GIVEN a GitLab project whose open merge requests come without a
  ;; total, as GitLab answers for more than 10000
  (let ((forge test-repo-lists--gitlab)
        (counts test-repo-lists--default-counts))
    (test-repo-lists--on-overriding forge counts
        (list (list "/projects/cafe%2Fespresso/merge_requests\\?state=opened" 200
                    (lambda (_request) (test-repo-lists--gitlab-page counts))))
      (test-repo-lists--open forge)
      ;; WHEN the lists answer
      (shipit-test-http-release)
      ;; THEN the open merge requests show the count of their first page
      (should (test-repo-lists--shows-p forge "Open (100)$"))
      (should (= 100 (test-repo-lists--items forge "open-prs")))
      ;; AND offer more below the last of them
      (should (test-repo-lists--shows-p forge "open-prs 100\n.*to load more")))))

(test-repo-lists--deftest test-shipit-repo-buffer-keeps-a-list-open-that-the-user-opened
  "A section the user expanded while it loaded stays expanded."
  ;; GIVEN the user expanded the pull requests while they load
  (test-repo-lists--on forge test-repo-lists--default-counts
    (test-repo-lists--open forge)
    (should (test-repo-lists--shows-p
             forge (format "%s (loading" (plist-get forge :prs))))
    (with-current-buffer (test-repo-lists--buffer-name forge)
      (magit-section-show (test-repo-lists--prs-section forge)))
    ;; WHEN the lists answer
    (shipit-test-http-release)
    ;; THEN the pull requests are still expanded
    (should-not (oref (test-repo-lists--prs-section forge) hidden))))

(defun test-repo-lists--line-at-point (forge)
  "The line at point in the repo buffer of FORGE."
  (with-current-buffer (test-repo-lists--buffer-name forge)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(test-repo-lists--deftest test-shipit-repo-buffer-keeps-point-when-more-arrive
  "Point stays on its item when the next page of the list arrives."
  ;; GIVEN point on the third closed pull request
  (test-repo-lists--on forge test-repo-lists--default-counts
    (test-repo-lists--open forge)
    (shipit-test-http-release)
    ;; WHEN the user types + there and the next page arrives
    (test-repo-lists--press forge "+" "closed-prs 3")
    (shipit-test-http-release)
    ;; THEN point is still on the third closed pull request
    (should (= 10 (test-repo-lists--items forge "closed-prs")))
    (should (string-match-p "closed-prs 3\\'" (test-repo-lists--line-at-point forge)))))

(test-repo-lists--deftest test-shipit-repo-buffer-keeps-the-filter-when-more-arrive
  "The filter of a list holds for its next page too."
  ;; GIVEN the open pull requests filtered by the title "open-prs 1"
  (test-repo-lists--on forge test-repo-lists--default-counts
    (test-repo-lists--open forge)
    (shipit-test-http-release)
    (test-repo-lists--expand forge)
    (with-current-buffer (test-repo-lists--buffer-name forge)
      (setq shipit-repo-buffer--filter-section-type 'repo-prs)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "open-prs 1")))
        (call-interactively #'shipit-repo-buffer--set-filter)))
    (should (= 12 (test-repo-lists--items forge "open-prs")))
    ;; WHEN the user types + in them and the next page arrives
    (test-repo-lists--press forge "+" "open-prs 1")
    (shipit-test-http-release)
    ;; THEN only the matching of the 200 show: 1, 10 to 19, and 100 to 199
    (should (= 111 (test-repo-lists--items forge "open-prs")))
    ;; AND the filter is still set
    (with-current-buffer (test-repo-lists--buffer-name forge)
      (should (equal "open-prs 1" shipit-repo-buffer--filter-text)))))

(test-repo-lists--deftest test-shipit-repo-buffer-without-issues-and-discussions-asks-for-prs-only
  "With issues and discussions turned off, only the pull requests load."
  ;; GIVEN issues and discussions are turned off
  (test-repo-lists--on forge test-repo-lists--default-counts
    (let ((shipit-issues-enabled nil)
          (shipit-discussions-enabled nil))
      ;; WHEN the user opens the repo buffer and the lists answer
      (test-repo-lists--open forge)
      (shipit-test-http-release)
      ;; THEN only the pull requests show
      (should (test-repo-lists--shows-p
               forge (format "%s (1234 open)" (plist-get forge :prs))))
      (should-not (test-repo-lists--shows-p forge "Issues"))
      (should-not (test-repo-lists--shows-p forge "Discussions"))
      ;; AND only they were asked for
      (should (= 2 (length (test-repo-lists--list-requests forge)))))))

(defun test-repo-lists--tracker-search (counts)
  "The `:search' of a tracker with the issues of COUNTS.
It answers while the caller waits, as a tracker without
`:search-page-async' does."
  (lambda (_config args)
    (let* ((open (member "--state=open" args))
           (list (if open 'open-issues 'closed-issues))
           (limit (string-to-number
                   (substring (seq-find (lambda (arg) (string-prefix-p "--limit=" arg)) args) 8)))
           (total (plist-get counts (intern (format ":%s" list)))))
      (mapcar (lambda (number) (test-repo-lists--github-item list number))
              (and (> (min limit total) 0) (number-sequence 1 (min limit total)))))))

(defun test-repo-lists--run-timers ()
  "Run the timers that are due, as Emacs does when it waits for input."
  (accept-process-output nil 0.05))

(ert-deftest test-shipit-repo-buffer-loads-the-issues-of-a-tracker-that-cannot-search-in-the-background ()
  "A tracker without a search in the background fills its list after the draw."
  ;; GIVEN a GitHub repository whose issues are in a tracker that
  ;; answers only while the caller waits
  (let ((forge test-repo-lists--github)
        (counts test-repo-lists--default-counts))
    (test-repo-lists--on forge counts
      (let ((shipit-issue-backend 'tracker)
            (shipit-issue-backend-config nil)
            (shipit-issue-backends
             (cons (list 'tracker :name "Tracker" :search (test-repo-lists--tracker-search counts))
                   shipit-issue-backends)))
        ;; WHEN the user opens the repo buffer
        (test-repo-lists--open forge)
        ;; THEN the issues say that they are loading
        (should (test-repo-lists--shows-p forge "Issues (loading"))
        ;; WHEN the tracker answers
        (test-repo-lists--run-timers)
        (shipit-test-http-release)
        ;; THEN the issues show their first pages, without totals
        (should (test-repo-lists--shows-p forge "Open (100)$"))
        (should (= 100 (test-repo-lists--items forge "open-issues")))
        (should (= 3 (test-repo-lists--items forge "closed-issues")))
        ;; AND the open issues offer more
        (should (test-repo-lists--shows-p forge "open-issues 100\n.*to load more"))))))

(ert-deftest test-shipit-repo-buffer-shows-a-tracker-that-cannot-be-asked-as-failed ()
  "A list whose request cannot start says why, and the other lists load."
  ;; GIVEN a GitHub repository whose issues are in a tracker that
  ;; signals as soon as it is asked
  (let ((forge test-repo-lists--github))
    (test-repo-lists--on forge test-repo-lists--default-counts
      (let ((shipit-issue-backend 'tracker)
            (shipit-issue-backend-config nil)
            (shipit-issue-backends
             (cons (list 'tracker :name "Tracker"
                         :search-page-async (lambda (&rest _) (error "Tracker is down")))
                   shipit-issue-backends)))
        ;; WHEN the user opens the repo buffer and the other lists answer
        (test-repo-lists--open forge)
        (shipit-test-http-release)
        ;; THEN the issues say why they failed
        (should (test-repo-lists--shows-p forge "Open (failed: Tracker is down)"))
        ;; AND the discussions show
        (should (test-repo-lists--shows-p forge "Discussions (25 of 312)"))))))

(ert-deftest test-shipit-repo-buffer-says-why-github-gave-no-discussions ()
  "A discussion search that GitHub refuses says GitHub's reason."
  ;; GIVEN GitHub answers the search for discussions with an error
  (let ((forge test-repo-lists--github))
    (test-repo-lists--on-overriding forge test-repo-lists--default-counts
        (list (list "/graphql\\'" 200
                    '((errors . [((message . "Discussions are turned off"))]))))
      ;; WHEN the lists answer
      (test-repo-lists--open forge)
      (shipit-test-http-release)
      ;; THEN the discussions say why they failed
      (should (test-repo-lists--shows-p
               forge "Discussions (failed: Discussions are turned off)")))))

;;; Newest first

(defconst test-repo-lists--sorted-counts
  '(:open-prs 250 :closed-prs 40 :open-issues 250 :closed-issues 40 :discussions 0)
  "How many items each list of the repository with sorted lists has.")

(defun test-repo-lists--newest-first-p (address)
  "Whether ADDRESS asks GitHub or GitLab for the newest items first.
GitHub sorts as `sort' and `order' say.  GitLab sorts as `order_by' and
`sort' say, and gives the newest first when they are missing."
  (if (string-match-p "/search/issues" address)
      (and (string-match-p "[?&]sort=created\\(&\\|\\'\\)" address)
           (string-match-p "[?&]order=desc\\(&\\|\\'\\)" address))
    (not (or (string-match-p "[?&]order_by=updated_at" address)
             (string-match-p "[?&]sort=asc" address)))))

(defun test-repo-lists--sorted-numbers (address total)
  "The item numbers of the page ADDRESS asks for, of a list of TOTAL.
In this repository the oldest items were updated last, so a list by
last update starts at #1."
  (let ((places (test-repo-lists--numbers address total)))
    (if (test-repo-lists--newest-first-p address)
        (mapcar (lambda (place) (- (1+ total) place)) places)
      places)))

(defun test-repo-lists--sorted-answers (forge counts)
  "The answers of FORGE for the lists of COUNTS, sorted as each request asks."
  (if (eq (plist-get forge :backend) 'github)
      (list (list "/search/issues" 200
                  (lambda (_request)
                    (let* ((list (test-repo-lists--github-list shipit-test-http-address))
                           (total (plist-get counts (intern (format ":%s" list)))))
                      `((total_count . ,total)
                        (incomplete_results . :json-false)
                        (items . ,(vconcat
                                   (mapcar (lambda (number)
                                             (test-repo-lists--github-item list number))
                                           (test-repo-lists--sorted-numbers
                                            shipit-test-http-address total)))))))))
    (list (list "/projects/cafe%2Fespresso/\\(merge_requests\\|issues\\)\\?" 200
                (lambda (_request)
                  (let ((list (test-repo-lists--gitlab-list shipit-test-http-address)))
                    (vconcat (mapcar (lambda (number)
                                       (test-repo-lists--gitlab-item list number))
                                     (test-repo-lists--sorted-numbers
                                      shipit-test-http-address
                                      (test-repo-lists--gitlab-total counts))))))
                (lambda ()
                  `(("X-Total" . ,(number-to-string (test-repo-lists--gitlab-total counts)))))))))

(defmacro test-repo-lists--on-sorting (forge &rest body)
  "Run BODY against FORGE with a repository whose lists sort as asked.
Its lists have `test-repo-lists--sorted-counts' items."
  (declare (indent 1))
  `(test-repo-lists--on-overriding ,forge test-repo-lists--sorted-counts
       (test-repo-lists--sorted-answers forge counts)
     ,@body))

(defun test-repo-lists--numbers-shown (forge list)
  "The numbers of the items of LIST in the repo buffer of FORGE, top to bottom."
  (let ((text (test-repo-lists--text forge))
        (regexp (format "#\\([0-9]+\\)  %s [0-9]+$" list))
        (start 0)
        (numbers nil))
    (while (string-match regexp text start)
      (push (string-to-number (match-string 1 text)) numbers)
      (setq start (match-end 0)))
    (nreverse numbers)))

(test-repo-lists--deftest test-shipit-repo-buffer-shows-the-newest-open-items-first
  "The open lists show their newest items first."
  ;; GIVEN a repository with 250 open pull requests and 250 open
  ;; issues, whose oldest ones were updated last
  (test-repo-lists--on-sorting forge
    ;; WHEN the user opens its repo buffer and the lists answer
    (test-repo-lists--open forge)
    (shipit-test-http-release)
    ;; THEN the open pull requests show #250 down to #151
    (should (equal (number-sequence 250 151 -1)
                   (test-repo-lists--numbers-shown forge "open-prs")))
    ;; AND the open issues show #250 down to #151
    (should (equal (number-sequence 250 151 -1)
                   (test-repo-lists--numbers-shown forge "open-issues")))))

(test-repo-lists--deftest test-shipit-repo-buffer-loads-the-next-older-items
  "+ adds the next older items below the newest ones."
  ;; GIVEN the repo buffer shows the open pull requests #250 down to #151
  (test-repo-lists--on-sorting forge
    (test-repo-lists--open forge)
    (shipit-test-http-release)
    ;; WHEN the user types + in that list
    (test-repo-lists--press forge "+" "open-prs ")
    (shipit-test-http-release)
    ;; THEN #150 down to #51 follow below #151
    (should (equal (number-sequence 250 51 -1)
                   (test-repo-lists--numbers-shown forge "open-prs")))))

(ert-deftest test-shipit-repo-buffer-loads-a-count-of-older-items ()
  "C-u N + adds the N next older items below the newest ones.
It runs on GitHub only.  On GitLab, C-u N + shows no more than the
first 100 merge requests."
  ;; GIVEN the repo buffer shows the open pull requests #250 down to #151
  (let ((forge test-repo-lists--github))
    (test-repo-lists--on-sorting forge
      (test-repo-lists--open forge)
      (shipit-test-http-release)
      ;; WHEN the user types C-u 50 + in that list
      (let ((current-prefix-arg 50))
        (test-repo-lists--press forge "+" "open-prs "))
      ;; THEN #150 down to #101 follow below #151
      (should (equal (number-sequence 250 101 -1)
                     (test-repo-lists--numbers-shown forge "open-prs"))))))

(test-repo-lists--deftest test-shipit-repo-buffer-shows-the-newest-closed-items-first
  "The closed lists show their newest items first."
  ;; GIVEN a repository with 40 closed pull requests and 40 closed
  ;; issues, whose oldest ones were updated last
  (test-repo-lists--on-sorting forge
    ;; WHEN the user opens its repo buffer and the lists answer
    (test-repo-lists--open forge)
    (shipit-test-http-release)
    ;; THEN the closed pull requests show #40 down to #36
    (should (equal (number-sequence 40 36 -1)
                   (test-repo-lists--numbers-shown forge "closed-prs")))
    ;; AND the closed issues show #40 down to #36
    (should (equal (number-sequence 40 36 -1)
                   (test-repo-lists--numbers-shown forge "closed-issues")))))

(provide 'test-shipit-repo-buffer-lists)
;;; test-shipit-repo-buffer-lists.el ends here
