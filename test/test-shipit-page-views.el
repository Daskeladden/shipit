;;; test-shipit-page-views.el --- Views of a pull request page and an issue page -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; v in a pull request page or an issue page shows the next view of the
;; page, and V asks for a view by its name.  The normal view is the
;; page as it always was.  The overview of a pull request shows what
;; waits for its author: the failing checks, the reviews that ask for
;; changes and the threads that wait for an answer.  The overview of an
;; issue shows its status, its assignee and its linked pull requests.
;;
;; The pages are opened as by hand.  GitHub is answered at url.el, and
;; the Jira issue is one of the Jira sandbox.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'magit-section)
(require 'shipit)
(require 'shipit-core)
(require 'shipit-buffer)
(require 'shipit-pr-sections)
(require 'shipit-pr-github)
(require 'shipit-comment-github)
(require 'shipit-checks)
(require 'shipit-issues)
(require 'shipit-issues-buffer)
(require 'shipit-sections)
(require 'shipit-issue-github)
(require 'shipit-pr-linked-issue)
(require 'shipit-test-http)
(require 'test-shipit-jira-sandbox)

(defvar shipit-page-view)
(defvar shipit-page-views)

;;; My pull requests, as GitHub gives them

(defconst test-page-views--waiting-pr
  '(:title "Brew the coffee"
    :checks (("grind beans" . "failure") ("boil water" . "success"))
    :reviews (("alice" "CHANGES_REQUESTED" "Use fresh beans.")
              ("frank" "APPROVED" "Looks good."))
    :threads ((:resolved nil :line 2 :comments ((41 "bob" "Why two cups?")))
              (:resolved t :line 1 :comments ((42 "gina" "Typo in the name.")))
              (:resolved nil :line 3 :comments ((43 "hank" "Rename the pot?")
                                                (44 "me" "Done, it is the kettle now."))))
    :comments ((51 "carol" "Any news on the mugs?")
               (52 "me" "Mugs come Monday.")
               (53 "ivan" "Can we use the blue mugs?")))
  "A pull request of mine with something that waits for me of each kind.
One check fails, one review asks for changes, the thread of bob waits
for my answer and the comment of ivan came after my last one.  The
other check passes, the other review approves, the thread of gina is
resolved, I answered the thread of hank last and the comment of carol
came before my last one.")

(defconst test-page-views--quiet-pr
  '(:title "Grind the beans"
    :checks (("grind beans" . "success") ("boil water" . "success"))
    :reviews (("frank" "APPROVED" "Looks good."))
    :threads ((:resolved t :line 1 :comments ((42 "gina" "Typo in the name.")))
              (:resolved nil :line 3 :comments ((43 "hank" "Rename the pot?")
                                                (44 "me" "Done, it is the kettle now."))))
    :comments ((51 "carol" "Any news on the mugs?")
               (52 "me" "Mugs come Monday.")))
  "A pull request of mine where nothing waits for me.")

(defconst test-page-views--patch
  "@@ -0,0 +1,3 @@\n+fill the pot\n+boil two cups\n+pour"
  "The change of the one file of each pull request.")

(defun test-page-views--time (day id)
  "A time on DAY of January 2026, a later one for a higher ID."
  (format "2026-01-%02dT10:%02d:00Z" day (mod id 60)))

(defun test-page-views--pull-request (number pr)
  "PR as GitHub gives it, as the pull request NUMBER of o/r."
  `((number . ,number) (title . ,(plist-get pr :title)) (state . "open")
    (body . "Pour it slowly.") (user (login . "me"))
    (created_at . "2026-01-01T10:00:00Z") (updated_at . "2026-01-01T10:00:00Z")
    (head (sha . "abc") (ref . "coffee") (repo (full_name . "o/r")))
    (base (sha . "def") (ref . "main") (repo (full_name . "o/r")))
    (html_url . ,(format "https://github.com/o/r/pull/%d" number))))

(defun test-page-views--check-runs (pr)
  "The check runs of PR, as GitHub gives them."
  `((total_count . ,(length (plist-get pr :checks)))
    (check_runs
     . ,(vconcat
         (cl-loop for (name . conclusion) in (plist-get pr :checks)
                  for id from 21
                  collect `((id . ,id) (name . ,name) (status . "completed")
                            (conclusion . ,conclusion)
                            (html_url . ,(format "https://github.com/o/r/runs/%d" id))
                            (started_at . "2026-01-01T10:00:00Z")
                            (completed_at . "2026-01-01T10:01:00Z")))))))

(defconst test-page-views--check-suites
  '((total_count . 1)
    (check_suites . [((id . 11) (status . "completed") (conclusion . "failure")
                      (app (name . "Actions")) (head_sha . "abc"))]))
  "The one check suite of each pull request.")

(defun test-page-views--reviews (pr)
  "The reviews of PR, as GitHub gives them."
  (vconcat
   (cl-loop for (login state body) in (plist-get pr :reviews)
            for id from 31
            collect `((id . ,id) (user (login . ,login)) (state . ,state) (body . ,body)
                      (submitted_at . ,(test-page-views--time 2 id))))))

(defun test-page-views--review-comment (thread comment root)
  "COMMENT of THREAD as GitHub gives it; ROOT is the id of its first comment."
  (pcase-let ((`(,id ,login ,body) comment)
              (line (plist-get thread :line)))
    `((id . ,id) (user (login . ,login)) (body . ,body)
      (path . "brew.sh") (line . ,line) (original_line . ,line) (side . "RIGHT")
      (commit_id . "abc") (original_commit_id . "abc")
      (diff_hunk . ,test-page-views--patch)
      (created_at . ,(test-page-views--time 2 id))
      (updated_at . ,(test-page-views--time 2 id))
      ,@(unless (eql id root) `((in_reply_to_id . ,root))))))

(defun test-page-views--review-comments (pr)
  "The comments of the review threads of PR, as GitHub gives them."
  (vconcat
   (cl-loop for thread in (plist-get pr :threads)
            for root = (car (car (plist-get thread :comments)))
            append (mapcar (lambda (comment)
                             (test-page-views--review-comment thread comment root))
                           (plist-get thread :comments)))))

(defun test-page-views--thread-node (thread)
  "THREAD as the GraphQL API of GitHub gives it."
  (pcase-let ((`(,root ,login ,_) (car (plist-get thread :comments))))
    `((id . ,(format "thread-%d" root))
      (isResolved . ,(if (plist-get thread :resolved) t :json-false))
      (comments (nodes . [((id . ,(format "comment-%d" root)) (databaseId . ,root)
                           (path . "brew.sh") (line . ,(plist-get thread :line))
                           (author (login . ,login)))])))))

(defun test-page-views--issue-comments (comments)
  "COMMENTS, a list of (ID LOGIN BODY), as GitHub gives the comments of an issue."
  (vconcat
   (cl-loop for (id login body) in comments
            collect `((id . ,id) (user (login . ,login)) (body . ,body)
                      (created_at . ,(test-page-views--time 3 id))
                      (updated_at . ,(test-page-views--time 3 id))))))

(defconst test-page-views--files
  `[((filename . "brew.sh") (status . "modified") (sha . "f1")
     (additions . 3) (deletions . 0) (changes . 3)
     (patch . ,test-page-views--patch))]
  "The one file that each pull request changes.")

(defun test-page-views--graphql (prs)
  "An answer to a GraphQL query about PRS, an alist of NUMBER and PR.
It gives the review threads of a pull request; any other query finds
nothing."
  (lambda (sent)
    (or (cl-loop for (number . pr) in prs
                 when (and (string-match-p "reviewThreads" sent)
                           (string-match-p (format "pullRequest(number: %d)" number) sent))
                 return `((data (repository
                                 (pullRequest
                                  (reviewThreads
                                   (nodes . ,(vconcat (mapcar #'test-page-views--thread-node
                                                              (plist-get pr :threads))))))))))
        '((data)))))

(defun test-page-views--pr-answers (number pr)
  "GitHub's answers about PR, the pull request NUMBER of o/r."
  (let ((at (lambda (format-string) (format format-string number))))
    `((,(funcall at "/repos/o/r/pulls/%d\\'") 200 ,(test-page-views--pull-request number pr))
      (,(funcall at "/pulls/%d/comments") 200 ,(test-page-views--review-comments pr))
      (,(funcall at "/pulls/%d/reviews") 200 ,(test-page-views--reviews pr))
      (,(funcall at "/pulls/%d/requested_reviewers") 200 ((users . []) (teams . [])))
      (,(funcall at "/pulls/%d/commits") 200 [])
      (,(funcall at "/pulls/%d/files") 200 ,test-page-views--files)
      (,(funcall at "/issues/%d/comments") 200
       ,(test-page-views--issue-comments (plist-get pr :comments)))
      (,(funcall at "/issues/%d/timeline") 200 [])
      ("/check-suites/11/check-runs" 200 ,(test-page-views--check-runs pr)))))

(defconst test-page-views--repository-answers
  `(("/repos/o/r/subscription\\'" 404 "")
    ,shipit-test-http-not-starred
    ("/reactions" 200 [])
    ("/repos/o/r/contents/" 404 "")
    ("/commits/[^/]+/check-suites" 200 ,test-page-views--check-suites)
    ("/repos/o/r/compare/" 200 ((status . "ahead") (ahead_by . 1) (behind_by . 0)
                                (commits . []) (files . [])))
    ("/user\\'" 200 ((login . "me"))))
  "GitHub's answers about the repository o/r and about me.")

(defun test-page-views--github (prs)
  "GitHub's answers about PRS, an alist of NUMBER and PR, all of o/r."
  (append (cl-loop for (number . pr) in prs
                   append (test-page-views--pr-answers number pr))
          (list (list "/graphql\\'" 200 (test-page-views--graphql prs)))
          test-page-views--repository-answers))

(defmacro test-page-views--as-me (&rest body)
  "Run BODY as the user me of GitHub, with the normal view and fresh caches.
No avatar is drawn."
  (declare (indent 0))
  `(let ((shipit-github-token "not-a-token")
         (shipit--github-token-cache nil)
         (shipit--cached-current-user nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-show-avatars nil)
         (shipit-show-pr-checks t)
         (shipit-page-view 'normal)
         (shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
         (shipit-gh-etag--refresh-cache nil)
         (shipit--cached-approval-status (make-hash-table :test 'equal))
         (shipit--comment-cache (make-hash-table :test 'equal))
         (shipit--reaction-cache (make-hash-table :test 'equal)))
     ,@body))

(defun test-page-views--open-pr (number)
  "Open the pull request NUMBER of o/r, as by hand; return its buffer."
  (let ((page (shipit-open-pr-buffer number "o/r" 'github nil)))
    (test-page-views--settle)
    page))

(defun test-page-views--settle ()
  "Let the timers of the page run."
  (dotimes (_ 3) (accept-process-output nil 0.01)))

(defmacro test-page-views--with-prs (prs &rest body)
  "Answer GitHub about PRS, an alist of NUMBER and PR; run BODY as me.
Each page opened in BODY is killed at its end."
  (declare (indent 1))
  `(test-page-views--as-me
     (let ((pages-before (buffer-list)))
       (unwind-protect
           (shipit-test-http-answering (test-page-views--github ,prs)
             ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer pages-before)
             (when (with-current-buffer buffer
                     (derived-mode-p 'shipit-mode 'shipit-issue-mode))
               (kill-buffer buffer))))))))

;;; An issue, as GitHub gives it

(defconst test-page-views--issue
  '((number . 9) (title . "Order more cups") (state . "open")
    (body . "We run out by noon.") (user (login . "dave"))
    (assignees . [((login . "zoe"))]) (assignee (login . "zoe"))
    (labels . []) (comments . 1)
    (created_at . "2026-01-01T10:00:00Z") (updated_at . "2026-01-01T10:00:00Z")
    (html_url . "https://github.com/o/r/issues/9"))
  "An issue of o/r that zoe works on.")

(defconst test-page-views--linked-pr
  '((total_count . 1) (incomplete_results . :json-false)
    (items . [((number . 12) (title . "Buy cups") (state . "open")
               (body . "Fixes #9.") (user (login . "zoe"))
               (pull_request (url . "https://api.github.com/repos/o/r/pulls/12")
                             (html_url . "https://github.com/o/r/pull/12"))
               (html_url . "https://github.com/o/r/pull/12")
               (repository_url . "https://api.github.com/repos/o/r")
               (created_at . "2026-01-02T10:00:00Z")
               (updated_at . "2026-01-02T10:00:00Z"))]))
  "The search of GitHub that finds the pull request which names the issue.")

(defconst test-page-views--issue-answers
  `(("/repos/o/r/issues/9\\'" 200 ,test-page-views--issue)
    ("/issues/9/comments" 200 ,(test-page-views--issue-comments
                                '((61 "erin" "Two boxes?"))))
    ("/issues/9/events" 200 [])
    ("/search/issues" 200 ,test-page-views--linked-pr)
    ("/graphql\\'" 200 ((data))))
  "GitHub's answers about the issue 9 of o/r.")

(defun test-page-views--open-issue ()
  "Open the issue 9 of o/r, as by hand; return its buffer."
  (let ((page (shipit-issues-open-buffer 9 "o/r")))
    (test-page-views--settle)
    page))

;;; What the user does and sees

(defun test-page-views--press (key)
  "Press KEY in this page, as the user does.
The page binds each key it has no use for to `undefined'."
  (let ((command (key-binding (kbd key) t)))
    (should (commandp command))
    (should-not (eq command 'undefined))
    (call-interactively command)
    (test-page-views--settle)))

(defun test-page-views--pick-from (key choice)
  "Press KEY and pick CHOICE where it asks; return the choices it offered."
  (let* ((offered nil)
         (completing-read-function
          (lambda (_prompt collection &rest _)
            (setq offered (all-completions "" collection))
            choice)))
    (test-page-views--press key)
    offered))

(defun test-page-views--menu-command (menu key)
  "The command that MENU has on KEY, or nil when it has none there."
  (condition-case nil
      (plist-get (cdr (transient-get-suffix menu key)) :command)
    (error nil)))

(defun test-page-views--menu-has-the-view-keys (menu)
  "Check that MENU has v and V, each with the command this page has on it."
  (dolist (key '("v" "V"))
    (let ((command (key-binding (kbd key))))
      (should-not (memq command '(nil undefined)))
      (should (eq (test-page-views--menu-command menu key) command)))))

(defun test-page-views--shown ()
  "The text of this page that the user sees, without the invisible parts."
  (let ((pos (point-min))
        (parts nil))
    (while (< pos (point-max))
      (let ((next (next-single-char-property-change pos 'invisible nil (point-max))))
        (unless (invisible-p pos)
          (push (buffer-substring-no-properties pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun test-page-views--first-lines (count)
  "The first COUNT lines of this page that the user sees and that have text."
  (seq-take (seq-remove #'string-blank-p (split-string (test-page-views--shown) "\n"))
            count))

(defun test-page-views--top-is (&rest patterns)
  "Check that the first lines with text of this page match PATTERNS, one each.
The match ignores case."
  (let ((case-fold-search t))
    (cl-mapc (lambda (pattern line) (should (string-match-p pattern line)))
             patterns
             (test-page-views--first-lines (length patterns)))))

(defun test-page-views--shows (&rest texts)
  "Check that this page shows each of TEXTS."
  (let ((shown (test-page-views--shown)))
    (dolist (text texts)
      (should (string-search text shown)))))

(defun test-page-views--hides (&rest texts)
  "Check that this page shows none of TEXTS."
  (let ((shown (test-page-views--shown)))
    (dolist (text texts)
      (should-not (string-search text shown)))))

(defun test-page-views--shows-checks-only-at-the-end (&rest checks)
  "Check that this page shows each of CHECKS in its Checks section only.
The section is the last part of the overview."
  (let* ((shown (test-page-views--shown))
         (at (let ((case-fold-search nil))
               (string-match "^[^ \n]+ Checks (" shown))))
    (should at)
    (dolist (check checks)
      (should-not (string-search check (substring shown 0 at)))
      (should (string-search check (substring shown at))))))

(defun test-page-views--asked-github ()
  "How many requests GitHub got so far."
  (length shipit-test-http-requests))

;;; The scenarios

(ert-deftest test-page-views-v-shows-the-overview-of-my-pull-request ()
  "The overview shows what waits for the author of a pull request."
  ;; GIVEN my pull request, with a failing check and a passing one, a
  ;;   review that asks for changes and one that approves, a thread
  ;;   that waits for my answer, a resolved one and one that I answered
  ;;   last, and a general comment after my last one and one before it
  ;; WHEN I press v
  ;; THEN the page shows the title, a line with the state and a line
  ;;   with the review status, and then the review that asks for
  ;;   changes, the thread that waits and the comment that waits
  ;; AND then the checks: the failing one, which shows there only, and
  ;;   the group of the passing ones, folded
  ;; AND nothing else: not the approval, the other threads and
  ;;   comments, the description, the files, the commits or the
  ;;   activity
  (test-page-views--with-prs (list (cons 7 test-page-views--waiting-pr))
    (with-current-buffer (test-page-views--open-pr 7)
      (test-page-views--press "v")
      (test-page-views--top-is "brew the coffee" "state: *open"
                               "approval: .*changes requested")
      (test-page-views--shows "alice" "Use fresh beans."
                              "Why two cups?" "Can we use the blue mugs?")
      (test-page-views--shows-checks-only-at-the-end "grind beans" "Successful checks (1)")
      (test-page-views--hides "boil water" "Looks good."
                              "Typo in the name." "Rename the pot?"
                              "Any news on the mugs?" "Pour it slowly."
                              "Files Changed" "Commits" "Activity"))))

(ert-deftest test-page-views-v-again-shows-the-normal-view-at-once ()
  "The normal view comes back as it was, without asking GitHub again."
  ;; GIVEN my pull request in the normal view
  ;; WHEN I press v for the overview and v again
  ;; THEN the page shows the normal view exactly as before
  ;; AND GitHub was not asked again for either switch
  (test-page-views--with-prs (list (cons 7 test-page-views--waiting-pr))
    (with-current-buffer (test-page-views--open-pr 7)
      (let ((normal (test-page-views--shown))
            (asked (test-page-views--asked-github)))
        (test-page-views--press "v")
        (should-not (equal normal (test-page-views--shown)))
        (test-page-views--press "v")
        (should (equal normal (test-page-views--shown)))
        (should (= asked (test-page-views--asked-github)))))))

(ert-deftest test-page-views-the-menu-picks-a-view-by-its-name ()
  "The menu of a page names v and V, and V picks a view by its name."
  ;; GIVEN my pull request in the normal view
  ;; WHEN I open the menu with SPC
  ;; THEN it has v for the next view and V to pick a view, and so has
  ;;   the menu of an issue
  ;; WHEN I press V
  ;; THEN it offers each view by its name, normal and overview
  ;; WHEN I pick overview
  ;; THEN the page shows the overview
  (test-page-views--with-prs (list (cons 7 test-page-views--waiting-pr))
    (with-current-buffer (test-page-views--open-pr 7)
      (test-page-views--menu-has-the-view-keys 'shipit-pr-menu)
      (with-temp-buffer
        (shipit-issue-mode)
        (test-page-views--menu-has-the-view-keys 'shipit-issue-menu))
      (should (equal '("normal" "overview")
                     (sort (test-page-views--pick-from "V" "overview") #'string<)))
      (test-page-views--shows "Why two cups?")
      (test-page-views--hides "Files Changed" "Pour it slowly."))))

(ert-deftest test-page-views-the-next-page-opens-in-the-view-i-chose ()
  "A page opens in the view that the user chose last."
  ;; GIVEN I chose the overview on my pull request
  ;; WHEN I open another pull request of mine
  ;; THEN it opens in the overview
  (test-page-views--with-prs (list (cons 7 test-page-views--waiting-pr)
                                   (cons 8 test-page-views--waiting-pr))
    (with-current-buffer (test-page-views--open-pr 7)
      (test-page-views--press "v"))
    (with-current-buffer (test-page-views--open-pr 8)
      (test-page-views--shows "grind beans" "Why two cups?")
      (test-page-views--hides "Files Changed" "Pour it slowly."))))

(ert-deftest test-page-views-the-overview-says-when-nothing-waits-for-me ()
  "The overview of a pull request where nothing waits says so in one line."
  ;; GIVEN my pull request, whose checks all pass, that no review asks
  ;;   to change, and whose threads and comments wait for no answer
  ;; WHEN I press v
  ;; THEN the page shows the title, the line with the state and the
  ;;   line with the review status, a line that says Nothing waits for
  ;;   you, and the heading of the checks, folded as they all pass
  ;; AND none of the checks, reviews, threads or comments
  (test-page-views--with-prs (list (cons 10 test-page-views--quiet-pr))
    (with-current-buffer (test-page-views--open-pr 10)
      (test-page-views--press "v")
      (test-page-views--top-is "grind the beans" "state: *open" "approval: .*approved")
      (test-page-views--shows "Nothing waits for you")
      (test-page-views--shows-checks-only-at-the-end "all passed")
      (test-page-views--hides "Successful checks" "grind beans" "boil water"
                              "Looks good." "Typo in the name."
                              "Rename the pot?" "Any news on the mugs?"))))

(ert-deftest test-page-views-v-shows-the-overview-of-an-issue ()
  "The overview of an issue shows its status, its assignee and its linked PRs."
  ;; GIVEN an issue with a description, a comment, an assignee and a
  ;;   linked pull request
  ;; WHEN I press v
  ;; THEN the page shows the title, the status, the assignee and the
  ;;   linked pull request
  ;; AND not the description, the comment or the activity
  (test-page-views--as-me
    (shipit-test-http-answering (append test-page-views--issue-answers
                                        test-page-views--repository-answers)
      (let ((page (test-page-views--open-issue)))
        (unwind-protect
            (with-current-buffer page
              (test-page-views--press "v")
              (test-page-views--shows "Order more cups" "Open" "zoe" "Buy cups")
              (test-page-views--hides "We run out by noon." "Two boxes?" "Activity"))
          (kill-buffer page))))))

(ert-deftest test-page-views-v-shows-the-overview-of-a-jira-issue ()
  "The overview of a Jira issue shows its status, its assignee and its linked PRs."
  ;; GIVEN an issue of Jira with a description and an assignee
  ;; WHEN I press v
  ;; THEN the page shows the title, the status, the assignee and the
  ;;   linked pull requests
  ;; AND not the description or the activity
  (skip-unless (executable-find "python3"))
  (let ((auth-sources nil)
        (shipit-page-view 'normal)
        (shipit-show-avatars nil))
    (test-jira-sandbox--with-page-opened-by
        (let ((config (shipit-jira-sandbox-start)))
          (shipit-issue-jira--update-description config "PRJ-1" "We run out by noon.")
          (shipit-issues-open-buffer "PRJ-1" shipit-jira-sandbox-repo))
        page
      (with-current-buffer page
        (test-page-views--press "v")
        (test-page-views--shows "A bug to try shipit on" "To Do" "Sandbox User"
                                "Linked PRs")
        (test-page-views--hides "We run out by noon." "Activity")))))

(ert-deftest test-page-views-a-view-of-my-own-joins-the-cycle ()
  "A view that the user adds to the setting comes after the others."
  ;; GIVEN a view of my own named discussion, which shows the
  ;;   description and the comments of a pull request
  ;; WHEN I press v twice on my pull request
  ;; THEN the page shows the description and the general comments
  ;; AND no checks and no files
  ;; WHEN I press v once more
  ;; THEN the page shows the normal view again
  (test-page-views--with-prs (list (cons 7 test-page-views--waiting-pr))
    (let ((shipit-page-views (append shipit-page-views
                                     '((discussion :pr (description comments))))))
      (with-current-buffer (test-page-views--open-pr 7)
        (let ((normal (test-page-views--shown)))
          (test-page-views--press "v")
          (test-page-views--press "v")
          (test-page-views--shows "Pour it slowly." "Any news on the mugs?"
                                  "Can we use the blue mugs?")
          (test-page-views--hides "grind beans" "boil water" "Files Changed")
          (test-page-views--press "v")
          (should (equal normal (test-page-views--shown))))))))

(provide 'test-shipit-page-views)
;;; test-shipit-page-views.el ends here
