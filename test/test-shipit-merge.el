;;; test-shipit-merge.el --- Tests for PR merge menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for merge action menu.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-http)
(require 'shipit-pr-actions)

;;; Test helpers

(defun test-merge--make-minimal-plist ()
  "Return a minimal valid PR backend plist with all required keys set to `ignore'."
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

(defmacro test-merge--with-mock-github (&rest body)
  "Run BODY with a mock GitHub-like environment."
  (declare (indent 0))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo")
         (shipit--merge-methods-cache (make-hash-table :test 'equal)))
     ,@body))

;;; Tests — shipit-pr-github--fetch-merge-methods

(ert-deftest test-merge-fetch-methods-all-enabled ()
  "GIVEN GitHub API returns all three merge method booleans as true
WHEN calling shipit-pr-github--fetch-merge-methods
THEN all three method strings are returned."
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint)
                 '((allow_merge_commit . t)
                   (allow_squash_merge . t)
                   (allow_rebase_merge . t)))))
      (let* ((config (list :repo "owner/repo"))
             (methods (shipit-pr-github--fetch-merge-methods config)))
        (should (member "merge" methods))
        (should (member "squash" methods))
        (should (member "rebase" methods))
        (should (= 3 (length methods)))))))

(ert-deftest test-merge-fetch-methods-squash-only ()
  "GIVEN GitHub API returns only squash enabled (others are :json-false)
WHEN calling shipit-pr-github--fetch-merge-methods
THEN only \"squash\" is returned."
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint)
                 '((allow_merge_commit . :json-false)
                   (allow_squash_merge . t)
                   (allow_rebase_merge . :json-false)))))
      (let* ((config (list :repo "owner/repo"))
             (methods (shipit-pr-github--fetch-merge-methods config)))
        (should (equal '("squash") methods))))))

(ert-deftest test-merge-fetch-methods-cached ()
  "GIVEN the cache already has methods for the repo
WHEN calling shipit-pr-github--fetch-merge-methods
THEN no API call is made and the cached value is returned."
  (test-merge--with-mock-github
    (puthash "owner/repo" '("merge") shipit--merge-methods-cache)
    (let ((api-called nil))
      (cl-letf (((symbol-function 'shipit--api-request)
                 (lambda (_endpoint)
                   (setq api-called t)
                   '((allow_merge_commit . t)))))
        (let* ((config (list :repo "owner/repo"))
               (methods (shipit-pr-github--fetch-merge-methods config)))
          (should (equal '("merge") methods))
          (should (not api-called)))))))

(ert-deftest test-merge-fetch-methods-nil-field-treated-as-enabled ()
  "GIVEN GitHub API returns nil (missing) for all method fields
WHEN calling shipit-pr-github--fetch-merge-methods
THEN all three methods are returned (nil is not :json-false)"
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint)
                 '())))
      (let* ((config (list :repo "owner/repo"))
             (methods (shipit-pr-github--fetch-merge-methods config)))
        (should (member "merge" methods))
        (should (member "squash" methods))
        (should (member "rebase" methods))
        (should (= 3 (length methods)))))))

;;; Tests — shipit--merge-guard

(ert-deftest test-merge-guard-rejects-merged-pr ()
  "GIVEN a PR with state=closed and merged_at set (merged PR)
WHEN calling shipit--merge-guard
THEN a user-error is signaled indicating the PR is already merged."
  (let ((pr-data '((state . "closed")
                   (merged_at . "2025-01-01T00:00:00Z")
                   (draft . :json-false))))
    (should-error (shipit--merge-guard pr-data) :type 'user-error)))

(ert-deftest test-merge-guard-rejects-closed-pr ()
  "GIVEN a PR with state=closed and no merged_at (closed without merge)
WHEN calling shipit--merge-guard
THEN a user-error is signaled indicating the PR is closed."
  (let ((pr-data '((state . "closed")
                   (draft . :json-false))))
    (should-error (shipit--merge-guard pr-data) :type 'user-error)))

(ert-deftest test-merge-guard-rejects-draft-pr ()
  "GIVEN a PR with state=open and draft=t (GitHub returns open for drafts)
WHEN calling shipit--merge-guard
THEN a user-error is signaled indicating the PR is a draft."
  (let ((pr-data '((state . "open")
                   (draft . t))))
    (should-error (shipit--merge-guard pr-data) :type 'user-error)))

(ert-deftest test-merge-guard-accepts-open-pr ()
  "GIVEN a PR with state=open and draft=:json-false (ready for review)
WHEN calling shipit--merge-guard
THEN no error is signaled and the function returns nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false))))
    (should (eq nil (shipit--merge-guard pr-data)))))

;;; Tests — shipit--merge-pr-execute and shipit--merge-get-methods

(ert-deftest test-merge-dispatch-calls-backend ()
  "GIVEN a backend registered with :merge-pr that records its args
WHEN calling shipit--merge-pr-execute with repo, number, and method
THEN the backend :merge-pr is called with the correct args
     AND shipit-buffer-refresh is called."
  (let* ((recorded-args nil)
         (refresh-called nil)
         (mock-backend (append (list :merge-pr
                                     (lambda (config number method)
                                       (setq recorded-args
                                             (list config number method))))
                               (test-merge--make-minimal-plist)))
         (mock-config (list :repo "owner/repo")))
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons mock-backend mock-config)))
              ((symbol-function 'shipit-buffer-refresh)
               (lambda ()
                 (setq refresh-called t))))
      (shipit--merge-pr-execute "owner/repo" 42 "squash")
      (should (equal (nth 0 recorded-args) mock-config))
      (should (equal (nth 1 recorded-args) 42))
      (should (equal (nth 2 recorded-args) "squash"))
      (should refresh-called))))

(ert-deftest test-merge-unsupported-backend-errors ()
  "GIVEN a backend registered without :fetch-merge-methods
WHEN calling shipit--merge-get-methods
THEN a user-error is signaled about unsupported backend."
  (let* ((mock-backend (test-merge--make-minimal-plist))
         (mock-config (list :repo "owner/repo")))
    ;; Remove :fetch-merge-methods key to simulate unsupported backend
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons mock-backend mock-config))))
      (should-error (shipit--merge-get-methods "owner/repo")
                    :type 'user-error))))

;;; Tests — DWIM integration

(ert-deftest test-merge-dwim-includes-merge-for-open-pr ()
  "GIVEN an open non-draft PR in the header DWIM context
WHEN building the action list
THEN Merge is among the actions."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo"))
    (shipit-pr-register-backend 'mock (test-merge--make-minimal-plist))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_number &optional _repo)
                 '((number . 42)
                   (state . "open")
                   (draft . :json-false)
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (user . ((login . "other-user"))))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "test-user"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "Merge" collection))
                 "Merge"))
              ((symbol-function 'shipit-merge) #'ignore))
      (with-temp-buffer
        (insert (propertize "PR #42" 'shipit-pr-number 42 'shipit-repo "owner/repo"))
        (goto-char (point-min))
        (shipit--pr-header-actions)))))

(ert-deftest test-merge-dwim-excludes-merge-for-closed-pr ()
  "GIVEN a closed PR in the header DWIM context
WHEN building the action list
THEN Merge is not among the actions."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo"))
    (shipit-pr-register-backend 'mock (test-merge--make-minimal-plist))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_number &optional _repo)
                 '((number . 42)
                   (state . "closed")
                   (merged_at . "2026-01-01T00:00:00Z")
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (user . ((login . "other-user"))))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "test-user"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should-not (member "Merge" collection))
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (with-temp-buffer
        (insert (propertize "PR #42" 'shipit-pr-number 42 'shipit-repo "owner/repo"))
        (goto-char (point-min))
        (shipit--pr-header-actions)))))

;;; Tests — merge readiness gating

(ert-deftest test-merge-suffixes-hidden-when-blocked ()
  "GIVEN a PR with mergeable_state=blocked
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable_state . "blocked")
                   (mergeable . :json-false))))
    (should-not (shipit--merge-ready-p pr-data))))

(ert-deftest test-merge-suffixes-shown-when-clean ()
  "GIVEN a PR with mergeable_state=clean
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns non-nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable_state . "clean")
                   (mergeable . t))))
    (should (shipit--merge-ready-p pr-data))))

(ert-deftest test-merge-suffixes-shown-when-mergeable-fallback ()
  "GIVEN a PR with no mergeable_state but mergeable=t
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns non-nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable . t))))
    (should (shipit--merge-ready-p pr-data))))

(ert-deftest test-merge-ready-but-no-methods ()
  "GIVEN a PR with mergeable_state=clean but all merge methods disabled
WHEN checking merge readiness
THEN shipit--merge-ready-p returns non-nil but fetch-merge-methods returns empty."
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _)
                 '((allow_merge_commit . :json-false)
                   (allow_squash_merge . :json-false)
                   (allow_rebase_merge . :json-false)))))
      (let ((pr-data '((state . "open")
                       (draft . :json-false)
                       (mergeable_state . "clean")
                       (mergeable . t))))
        (should (shipit--merge-ready-p pr-data))
        (should-not (shipit-pr-github--fetch-merge-methods
                     '(:repo "owner/repo")))))))

(ert-deftest test-merge-suffixes-hidden-when-dirty ()
  "GIVEN a PR with mergeable_state=dirty (merge conflict)
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable_state . "dirty")
                   (mergeable . :json-false))))
    (should-not (shipit--merge-ready-p pr-data))))

;;; Tests — cache invalidation

(ert-deftest test-merge-hard-refresh-clears-cache ()
  "GIVEN cached merge methods for a repo
WHEN shipit-buffer-refresh-hard runs
THEN the cache entry for the buffer's repo is removed."
  (require 'shipit-buffer)
  (let ((shipit--merge-methods-cache (make-hash-table :test 'equal))
        (shipit-buffer-repo "owner/repo")
        (shipit-buffer-pr-number 42)
        (shipit-buffer-pr-data '((number . 42))))
    (puthash "owner/repo" '("merge" "squash") shipit--merge-methods-cache)
    (cl-letf (((symbol-function 'shipit-clear-all-caches) #'ignore)
              ((symbol-function 'shipit-gh-etag-clear-all) #'ignore)
              ((symbol-function 'shipit-buffer-refresh) #'ignore)
              ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
      (shipit-buffer-refresh-hard))
    (should-not (gethash "owner/repo" shipit--merge-methods-cache))))

;;; Merging through GitHub's asynchronous merge API
;;
;; M-m on the page of a pull request, as a user does it, against
;; GitHub answers that stand in at the level of url.el.  The page is
;; the one of test-shipit-page-views.

(require 'shipit-test-http)
(require 'test-shipit-page-views)

(defvar test-merge-async--merged nil
  "Non-nil once GitHub merged the pull request of a scenario.")

(defconst test-merge-async--repository
  '((allow_merge_commit . t) (allow_squash_merge . t) (allow_rebase_merge . t))
  "The repository o/r, which allows each merge method.")

(defun test-merge-async--pull-request (&optional stack)
  "The pull request 12 of o/r, ready to merge, as GitHub gives it now.
It reads as merged once `test-merge-async--merged' is set.  STACK is
the stack object it carries, or nil."
  (lambda (_sent)
    (append (when test-merge-async--merged
              '((state . "closed") (merged . t) (merged_at . "2026-01-02T10:00:00Z")))
            '((mergeable_state . "clean") (mergeable . t) (draft . :json-false))
            (when stack `((stack . ,stack)))
            (test-page-views--pull-request 12 '(:title "Brew the coffee")))))

(defun test-merge-async--merge-answers (status message)
  "GitHub's answers to a merge of pull request 12 that ends in STATUS.
GitHub takes the request and says it is pending; asked again, it gives
STATUS, such as merged, enqueued or failed, with MESSAGE."
  `(("/pulls/12/merge-async\\'" 202
     ((status . "pending")
      (details (message . "Merge request enqueued.") (uuid . "u-1")
               (merge_method . "squash") (merge_action . "default")
               (expected_head_sha . "abc"))))
    ("/pulls/12/merge-async/u-1\\'" 200
     ,(lambda (_sent)
        (when (equal status "merged") (setq test-merge-async--merged t))
        `((status . ,status) (details (message . ,message)))))))

(defconst test-merge-async--stack
  '((id . 900) (number . 7) (size . 2) (position . 2)
    (base . ((ref . "main") (sha . "aaa"))))
  "The stack object of pull request 12, the upper of two.")

(defconst test-merge-async--stack-resource
  '((id . 900) (number . 7) (open . t) (base . ((ref . "main")))
    (pull_requests . [((number . 11) (state . "open") (draft . :json-false)
                       (merged_at . nil) (head . ((ref . "grind") (sha . "a1")))
                       (title . "Grind the beans") (user . ((login . "me"))))
                      ((number . 12) (state . "open") (draft . :json-false)
                       (merged_at . nil) (head . ((ref . "coffee") (sha . "abc")))
                       (title . "Brew the coffee") (user . ((login . "me"))))]))
  "The stack 7 of o/r: #11 at the bottom, #12 on top of it.")

(defmacro test-merge-async--on-github (answers &rest body)
  "Run BODY as me, with GitHub answering ANSWERS first, then about #12 of o/r.
Each page opened in BODY is killed at its end."
  (declare (indent 1))
  `(test-page-views--as-me
     (let ((test-merge-async--merged nil)
           (shipit--merge-methods-cache (make-hash-table :test 'equal))
           (pages-before (buffer-list)))
       (unwind-protect
           (shipit-test-http-answering
               (append ,answers
                       (list (list "/repos/o/r\\'" 200 test-merge-async--repository))
                       (test-page-views--github '((12 :title "Brew the coffee"))))
             ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer pages-before)
             (when (with-current-buffer buffer (derived-mode-p 'shipit-mode))
               (kill-buffer buffer))))))))

(defun test-merge-async--squash-merge (page)
  "Press M-m and s on PAGE, answer yes, and return the question asked.
Then wait until shipit says how the merge ended, five seconds at most."
  (let ((questions nil)
        (start (with-current-buffer (messages-buffer) (point-max))))
    (with-current-buffer page
      (set-window-buffer (selected-window) page)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (question) (push question questions) t)))
        (execute-kbd-macro (kbd "M-m s"))))
    (let ((deadline (+ (float-time) 5)))
      (while (and (not (test-merge-async--said-since start "#12 \\(merged\\|was added\\|failed\\)"))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (car questions)))

(defun test-merge-async--said-since (start regexp)
  "Return the first line of *Messages* after START that matches REGEXP."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char start)
      (and (re-search-forward regexp nil t)
           (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun test-merge-async--last-said (regexp)
  "Return the last line of *Messages* that matches REGEXP."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (and (re-search-backward regexp nil t)
           (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun test-merge-async--state-shown (page)
  "Return the state that PAGE shows for its pull request."
  (with-current-buffer page
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward "State: +\\([a-z]+\\)" nil t)
           (match-string-no-properties 1)))))

(defun test-merge-async--merge-request ()
  "Return the body of the merge request sent to GitHub, as an alist, or nil."
  (cl-loop for (method address) in shipit-test-http-requests
           for body in shipit-test-http-bodies
           when (and (equal method "PUT") (string-suffix-p "/merge-async" address))
           return (json-read-from-string body)))

(ert-deftest test-merge-async-squash-merges-the-pull-request ()
  ;; GIVEN the page of pull request #12 of o/r, open and ready to merge,
  ;;   whose head is the commit abc
  ;; WHEN M-m and s are pressed and the question is answered with yes,
  ;;   and GitHub takes the merge and soon says it merged
  ;; THEN shipit asked GitHub's asynchronous merge API to squash #12,
  ;;   at the commit abc
  ;; AND shipit says "PR #12 merged via squash"
  ;; AND the page shows #12 as merged
  (test-merge-async--on-github
      (append (test-merge-async--merge-answers "merged" "Pull request was merged.")
              (list (list "/repos/o/r/pulls/12\\'" 200 (test-merge-async--pull-request))))
    (let ((page (test-page-views--open-pr 12)))
      (test-merge-async--squash-merge page)
      (let ((request (test-merge-async--merge-request)))
        (should (equal "squash" (cdr (assq 'merge_method request))))
        (should (equal "abc" (cdr (assq 'sha request)))))
      (should (test-merge-async--last-said "PR #12 merged via squash"))
      (should (equal "merged" (test-merge-async--state-shown page))))))

(ert-deftest test-merge-async-says-when-queued ()
  ;; GIVEN the page of pull request #12, and a base branch with a merge
  ;;   queue, so GitHub puts #12 into the queue
  ;; WHEN M-m and s are pressed and the question is answered with yes
  ;; THEN shipit says "PR #12 was added to the merge queue"
  ;; AND the page still shows #12 as open
  (test-merge-async--on-github
      (append (test-merge-async--merge-answers "enqueued" "Pull request was added to the merge queue.")
              (list (list "/repos/o/r/pulls/12\\'" 200 (test-merge-async--pull-request))))
    (let ((page (test-page-views--open-pr 12)))
      (test-merge-async--squash-merge page)
      (should (test-merge-async--last-said "PR #12 was added to the merge queue"))
      (should (equal "open" (test-merge-async--state-shown page))))))

(ert-deftest test-merge-async-says-why-it-failed ()
  ;; GIVEN the page of pull request #12, whose branch gets a new commit
  ;;   before GitHub merges it, so GitHub gives up
  ;; WHEN M-m and s are pressed and the question is answered with yes
  ;; THEN shipit says "Merge of PR #12 failed: Head branch was modified."
  ;; AND the page still shows #12 as open
  (test-merge-async--on-github
      (append (test-merge-async--merge-answers "failed" "Head branch was modified.")
              (list (list "/repos/o/r/pulls/12\\'" 200 (test-merge-async--pull-request))))
    (let ((page (test-page-views--open-pr 12)))
      (test-merge-async--squash-merge page)
      (should (test-merge-async--last-said
               "Merge of PR #12 failed: Head branch was modified\\."))
      (should (equal "open" (test-merge-async--state-shown page))))))

(ert-deftest test-merge-async-names-the-pull-requests-below ()
  ;; GIVEN the page of pull request #12, the upper of a stack of two, on
  ;;   top of the open pull request #11
  ;; WHEN M-m and s are pressed
  ;; THEN the question says that the merge takes #11 below it too
  ;; WHEN it is answered with yes, and GitHub merges
  ;; THEN shipit asked GitHub's asynchronous merge API to squash #12
  (test-merge-async--on-github
      (append (test-merge-async--merge-answers "merged" "Pull request was merged.")
              (list (list "/repos/o/r/pulls/12\\'" 200
                          (test-merge-async--pull-request test-merge-async--stack))
                    (list "/repos/o/r/stacks/7\\'" 200 test-merge-async--stack-resource)))
    (let* ((page (test-page-views--open-pr 12))
           (question (test-merge-async--squash-merge page)))
      (should (string-match-p "#11" question))
      (should (equal "squash" (cdr (assq 'merge_method
                                         (test-merge-async--merge-request))))))))

;;; Merging a draft

(defvar test-merge-draft--ready nil
  "Non-nil once GitHub marked the draft of a scenario ready for review.")

(defun test-merge-draft--pull-request ()
  "The pull request 12 of o/r, a draft until `test-merge-draft--ready'."
  (let ((ready (test-merge-async--pull-request)))
    (lambda (sent)
      (append (unless test-merge-draft--ready
                '((draft . t) (mergeable_state . "draft")))
              (funcall ready sent)))))

(defun test-merge-draft--graphql (sent)
  "GitHub's answer to the GraphQL request SENT, which can mark #12 ready."
  (cond ((string-match-p "markPullRequestReadyForReview" sent)
         (setq test-merge-draft--ready t)
         '((data (markPullRequestReadyForReview (pullRequest (isDraft . :json-false))))))
        ((string-match-p "GetPullRequestId" sent)
         '((data (repository (pullRequest (id . "PR_12"))))))
        (t (funcall (test-page-views--graphql '((12 :title "Brew the coffee"))) sent))))

(defun test-merge-draft--answers ()
  "GitHub's answers about the draft #12, which it merges once asked."
  (append (test-merge-async--merge-answers "merged" "Pull request was merged.")
          (list (list "/repos/o/r/pulls/12\\'" 200 (test-merge-draft--pull-request))
                (list "/graphql\\'" 200 #'test-merge-draft--graphql))))

(defun test-merge-draft--press (page keys answer)
  "Press KEYS on PAGE, give ANSWER to each question, and return the questions.
The questions come oldest first.  Then wait until shipit says how a
merge ended, five seconds at most."
  (let ((questions nil)
        (start (with-current-buffer (messages-buffer) (point-max))))
    (with-current-buffer page
      (set-window-buffer (selected-window) page)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (question) (push question questions) answer)))
        (execute-kbd-macro (kbd keys))))
    (let ((deadline (+ (float-time) 5)))
      (while (and answer
                  (not (test-merge-async--said-since start "#12 \\(merged\\|was added\\|failed\\)"))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (nreverse questions)))

(ert-deftest test-merge-draft-marks-ready-then-merges ()
  ;; GIVEN the page of pull request #12, a draft
  ;; WHEN M-m is pressed
  ;; THEN shipit asks whether to mark #12 ready for review and merge it
  ;; WHEN that is answered with yes, s is pressed, the merge question is
  ;;   answered with yes, and GitHub merges
  ;; THEN #12 was marked ready for review on GitHub
  ;; AND shipit says "PR #12 merged via squash"
  (let ((test-merge-draft--ready nil))
    (test-merge-async--on-github (test-merge-draft--answers)
      (let* ((page (test-page-views--open-pr 12))
             (questions (test-merge-draft--press page "M-m s" t)))
        (should (string-match-p "PR #12 is a draft\\..*ready for review.*merge"
                                (car questions)))
        (should test-merge-draft--ready)
        (should (test-merge-async--last-said "PR #12 merged via squash"))))))

(ert-deftest test-merge-draft-stays-a-draft-on-no ()
  ;; GIVEN the page of pull request #12, a draft
  ;; WHEN M-m is pressed and the question is answered with no
  ;; THEN #12 stays a draft on GitHub, and GitHub is not asked to merge it
  ;; AND shipit says "PR #12 stays a draft"
  (let ((test-merge-draft--ready nil))
    (test-merge-async--on-github (test-merge-draft--answers)
      (let ((page (test-page-views--open-pr 12)))
        (test-merge-draft--press page "M-m" nil)
        (should-not test-merge-draft--ready)
        (should-not (test-merge-async--merge-request))
        (should (test-merge-async--last-said "PR #12 stays a draft"))))))

(provide 'test-shipit-merge)
;;; test-shipit-merge.el ends here
