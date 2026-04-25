;;; test-notifications-types.el --- Tests for type-aware notifications -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for notification type handling (PR vs Issue), key format,
;; extraction helpers, and backend notification support.

;;; Code:

(require 'ert)
(require 'shipit-notifications)

;;; Key Helper Tests

(ert-deftest test-shipit--notification-activity-key-pr ()
  "GIVEN a PR notification
WHEN building activity key
THEN format is repo:pr:number."
  (should (equal "owner/repo:pr:42"
                 (shipit--notification-activity-key "owner/repo" "pr" 42))))

(ert-deftest test-shipit--notification-activity-key-issue ()
  "GIVEN an Issue notification
WHEN building activity key
THEN format is repo:issue:number."
  (should (equal "owner/repo:issue:42"
                 (shipit--notification-activity-key "owner/repo" "issue" 42))))

(ert-deftest test-shipit--notification-activity-key-distinct ()
  "GIVEN same repo and number but different types
WHEN building activity keys
THEN keys are distinct (no collision)."
  (let ((pr-key (shipit--notification-activity-key "owner/repo" "pr" 42))
        (issue-key (shipit--notification-activity-key "owner/repo" "issue" 42)))
    (should-not (equal pr-key issue-key))))

;;; Extraction Helper Tests

(ert-deftest test-shipit--extract-notification-number-pr ()
  "GIVEN a PullRequest subject URL
WHEN extracting number
THEN returns the PR number."
  (should (= 123 (shipit--extract-notification-number
                   "https://api.github.com/repos/owner/repo/pulls/123"
                   "PullRequest"))))

(ert-deftest test-shipit--extract-notification-number-issue ()
  "GIVEN an Issue subject URL
WHEN extracting number
THEN returns the issue number."
  (should (= 456 (shipit--extract-notification-number
                   "https://api.github.com/repos/owner/repo/issues/456"
                   "Issue"))))

(ert-deftest test-shipit--extract-notification-number-invalid ()
  "GIVEN an invalid subject URL
WHEN extracting number
THEN returns nil."
  (should-not (shipit--extract-notification-number
               "https://api.github.com/repos/owner/repo/commits/abc"
               "Commit")))

;;; Type Mapping Tests

(ert-deftest test-shipit--notification-type-from-subject-pr ()
  "GIVEN PullRequest subject type
WHEN mapping to internal type
THEN returns \"pr\"."
  (should (equal "pr" (shipit--notification-type-from-subject "PullRequest"))))

(ert-deftest test-shipit--notification-type-from-subject-issue ()
  "GIVEN Issue subject type
WHEN mapping to internal type
THEN returns \"issue\"."
  (should (equal "issue" (shipit--notification-type-from-subject "Issue"))))

(ert-deftest test-shipit--notification-type-from-subject-unknown ()
  "GIVEN unknown subject type
WHEN mapping to internal type
THEN returns nil."
  (should-not (shipit--notification-type-from-subject "NoSuchThing")))

(ert-deftest test-shipit--notification-type-from-subject-release ()
  "GIVEN Release subject type
WHEN mapping to internal type
THEN returns \"release\"."
  (should (equal "release" (shipit--notification-type-from-subject "Release"))))

(ert-deftest test-shipit--notification-type-from-subject-checksuite ()
  "GIVEN CheckSuite subject type
WHEN mapping to internal type
THEN returns \"check\"."
  (should (equal "check" (shipit--notification-type-from-subject "CheckSuite"))))

(ert-deftest test-shipit--notification-type-from-subject-commit ()
  "GIVEN Commit subject type
WHEN mapping to internal type
THEN returns \"commit\"."
  (should (equal "commit" (shipit--notification-type-from-subject "Commit"))))

(ert-deftest test-shipit--extract-notification-number-release ()
  "GIVEN a Release subject URL
WHEN extracting number
THEN returns the numeric release id."
  (should (= 12345678 (shipit--extract-notification-number
                       "https://api.github.com/repos/owner/repo/releases/12345678"
                       "Release"))))

(ert-deftest test-shipit--extract-notification-number-commit ()
  "GIVEN a Commit subject URL
WHEN extracting number
THEN returns nil because commits have SHAs, not numbers.
The caller will fall back to the thread id to build a unique
activity key."
  (should-not (shipit--extract-notification-number
               "https://api.github.com/repos/owner/repo/commits/abcdef0123"
               "Commit")))

(ert-deftest test-process-notifications-release-included ()
  "GIVEN a Release notification from the GitHub API
WHEN processed
THEN it ends up in the activities hash with type \"release\",
a numeric id, a derived browse-url, and the repo html url."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil)
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (notifications
         (list
          `((id . "notif-release-1")
            (subject . ((type . "Release")
                        (title . "v1.2.3")
                        (url . "https://api.github.com/repos/owner/repo/releases/99")))
            (reason . "subscribed")
            (unread . t)
            (repository . ((full_name . "owner/repo")
                           (html_url . "https://github.com/owner/repo")))
            (updated_at . "2026-04-24T08:00:00Z")))))
    (shipit--process-notifications notifications)
    (should (= 1 (hash-table-count shipit--notification-pr-activities)))
    (let ((activity (gethash "owner/repo:release:99"
                             shipit--notification-pr-activities)))
      (should activity)
      (should (equal "release" (cdr (assq 'type activity))))
      (should (equal "v1.2.3" (cdr (assq 'subject activity))))
      (should (equal "https://github.com/owner/repo/releases"
                     (cdr (assq 'browse-url activity)))))))

(ert-deftest test-notifications-buffer-renders-release ()
  "GIVEN a release activity in the notifications table
WHEN the buffer is rendered
THEN a line is produced with the repo and subject (it must not be
silently dropped like before)."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:release:99"
             '((repo . "owner/repo")
               (number . 99)
               (type . "release")
               (subject . "v1.2.3")
               (reason . "subscribed")
               (browse-url . "https://github.com/owner/repo/releases")
               (updated-at . "2026-04-24T08:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "owner/repo" nil t))
              (goto-char (point-min))
              (should (search-forward "v1.2.3" nil t))))
        (kill-buffer buf)))))

(ert-deftest test-notifications-buffer-open-release-uses-browse-url ()
  "GIVEN a release notification at point
WHEN `shipit-notifications-buffer-open' is invoked
THEN browse-url is called with the activity's browse-url so the
user lands on the repo releases page instead of getting
\"Unknown notification type\"."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:release:99"
             '((repo . "owner/repo")
               (number . 99)
               (type . "release")
               (subject . "v1.2.3")
               (reason . "subscribed")
               (browse-url . "https://github.com/owner/repo/releases")
               (updated-at . "2026-04-24T08:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create))
          (opened-url nil))
      (unwind-protect
          (cl-letf (((symbol-function 'browse-url)
                     (lambda (url &rest _) (setq opened-url url))))
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "v1.2.3")
              (shipit-notifications-buffer-open)
              (should (equal "https://github.com/owner/repo/releases"
                             opened-url))))
        (kill-buffer buf)))))

(ert-deftest test-notifications-buffer-icon-type-for-deployment ()
  "GIVEN a workflow activity with reason `approval_requested`
WHEN picking its icon-type
THEN it is \"deployment\" (rocket icon), matching GitHub's UI for
deployment review requests — not \"workflow\" (gear)."
  (require 'shipit-notifications-buffer)
  (should (equal "deployment"
                 (shipit-notifications-buffer--icon-type-for
                  "workflow" nil nil "approval_requested"))))

(ert-deftest test-notifications-buffer-icon-type-for-ordinary-workflow ()
  "GIVEN a workflow activity without `approval_requested` reason
WHEN picking its icon-type
THEN it falls through to plain \"workflow\" (gear)."
  (require 'shipit-notifications-buffer)
  (should (equal "workflow"
                 (shipit-notifications-buffer--icon-type-for
                  "workflow" nil nil "ci_activity"))))

(ert-deftest test-notifications-buffer-icon-type-for-pr-draft ()
  "GIVEN a draft PR activity
WHEN picking its icon-type
THEN it is \"pr-draft\" — the existing behaviour must be preserved."
  (require 'shipit-notifications-buffer)
  (should (equal "pr-draft"
                 (shipit-notifications-buffer--icon-type-for
                  "pr" t nil "review_requested"))))

(ert-deftest test-notifications-buffer-icon-type-for-closed-draft-uses-closed ()
  "GIVEN a PR that was originally a draft and is now closed
\(`isDraft' from GitHub stays t even after closing\)
WHEN picking its icon-type
THEN return `pr-closed' — terminal state wins over draft history,
so users see the red closed-PR icon instead of the grey draft icon."
  (require 'shipit-notifications-buffer)
  (should (equal "pr-closed"
                 (shipit-notifications-buffer--icon-type-for
                  "pr" t "closed" "review_requested"))))

(ert-deftest test-notifications-buffer-icon-type-for-merged-draft-uses-merged ()
  "GIVEN a PR with the draft flag set whose state is merged
WHEN picking its icon-type
THEN return `pr-merged' — terminal state wins over draft history."
  (require 'shipit-notifications-buffer)
  (should (equal "pr-merged"
                 (shipit-notifications-buffer--icon-type-for
                  "pr" t "merged" "mention"))))

(ert-deftest test-notifications-buffer-icon-type-for-open-draft-uses-draft ()
  "GIVEN a draft PR that is still open
WHEN picking its icon-type
THEN return `pr-draft' — the draft icon still applies while the PR
is open, only terminal states (merged/closed) override."
  (require 'shipit-notifications-buffer)
  (should (equal "pr-draft"
                 (shipit-notifications-buffer--icon-type-for
                  "pr" t "open" "review_requested"))))


(ert-deftest test-notifications-buffer-open-workflow-browses-url ()
  "GIVEN a workflow notification at point whose activity has a
browse-url
WHEN `shipit-notifications-buffer-open' is invoked
THEN `browse-url' is called with that URL.

Note: for approval_requested workflow-run notifications, GitHub's
REST API does not expose the specific run id (subject.url is nil),
so the stored URL is the repo's /actions page.  GitHub's web UI
highlights pending approvals at the top of that page."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "zivid/zivid-sdk:workflow:23639728638"
             '((repo . "zivid/zivid-sdk")
               (number . 23639728638)
               (type . "workflow")
               (subject . "Deploy to prod")
               (reason . "approval_requested")
               (browse-url . "https://github.com/zivid/zivid-sdk/actions")
               (updated-at . "2026-04-22T09:28:41Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create))
          (opened-url nil))
      (unwind-protect
          (cl-letf (((symbol-function 'browse-url)
                     (lambda (url &rest _) (setq opened-url url))))
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "Deploy to prod")
              (shipit-notifications-buffer-open)
              (should (equal "https://github.com/zivid/zivid-sdk/actions"
                             opened-url))))
        (kill-buffer buf)))))

(ert-deftest test-process-notifications-workflow-browse-url-points-at-run ()
  "GIVEN a WorkflowRun notification whose subject URL is
/repos/OWNER/REPO/actions/runs/24770889847
WHEN processed
THEN the derived browse-url points at the specific run
(https://github.com/OWNER/REPO/actions/runs/24770889847), matching
the link GitHub's web UI opens when clicking the notification —
not the repo-wide /actions page."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil)
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (notifications
         (list
          `((id . "notif-wf-1")
            (subject . ((type . "WorkflowRun")
                        (title . "Deploy to prod")
                        (url . "https://api.github.com/repos/zivid/zivid-sdk/actions/runs/24770889847")))
            (reason . "approval_requested")
            (unread . t)
            (repository . ((full_name . "zivid/zivid-sdk")
                           (html_url . "https://github.com/zivid/zivid-sdk")))
            (updated_at . "2026-04-22T11:28:00Z")))))
    (shipit--process-notifications notifications)
    (let ((activity (gethash "zivid/zivid-sdk:workflow:24770889847"
                             shipit--notification-pr-activities)))
      (should activity)
      (should (equal "https://github.com/zivid/zivid-sdk/actions/runs/24770889847"
                     (cdr (assq 'browse-url activity)))))))

(ert-deftest test-shipit--closest-run-by-time ()
  "GIVEN runs with different updated_at
WHEN picking the closest to a target timestamp
THEN the nearest one wins (either side)."
  (let ((runs '(((id . 1) (updated_at . "2026-04-22T10:00:00Z"))
                ((id . 2) (updated_at . "2026-04-22T11:30:00Z"))
                ((id . 3) (updated_at . "2026-04-22T13:00:00Z")))))
    (should (equal 2 (cdr (assq 'id (shipit--closest-run-by-time
                                     runs "2026-04-22T11:28:00Z")))))
    (should (equal 1 (cdr (assq 'id (shipit--closest-run-by-time
                                     runs "2026-04-22T09:00:00Z")))))
    (should (equal 3 (cdr (assq 'id (shipit--closest-run-by-time
                                     runs "2026-04-22T14:00:00Z")))))))

(ert-deftest test-enrich-workflow-one-repo-updates-url-when-waiting ()
  "GIVEN an activity for approval-requested with a non-specific browse-url,
and a waiting run whose updated_at matches the notification
WHEN shipit--enrich-workflow-one-repo runs
THEN the activity's browse-url is updated to the waiting run's html_url."
  (let* ((shipit--notification-pr-activities (make-hash-table :test 'equal))
         (activity '((repo . "owner/repo")
                     (number . 42)
                     (type . "workflow")
                     (reason . "approval_requested")
                     (updated-at . "2026-04-22T09:28:00Z")
                     (browse-url . "https://github.com/owner/repo/actions")
                     (notification . ((id . "42")))))
         (key "owner/repo:workflow:42"))
    (puthash key activity shipit--notification-pr-activities)
    (shipit--enrich-workflow-one-repo
     "owner/repo" (list (cons key activity))
     '(((id . 99)
        (updated_at . "2026-04-22T09:28:10Z")
        (html_url . "https://github.com/owner/repo/actions/runs/99"))))
    (should (equal "https://github.com/owner/repo/actions/runs/99"
                   (cdr (assq 'browse-url activity))))))

(ert-deftest test-enrich-workflow-one-repo-opt-in-marks-read ()
  "GIVEN an activity for approval-requested but no waiting runs in the repo,
and `shipit-notifications-auto-mark-resolved-approvals' is enabled
WHEN shipit--enrich-workflow-one-repo runs
THEN the notification is marked read (side-effected via backend) and the
entry is removed from the activities hash."
  (let* ((shipit--notification-pr-activities (make-hash-table :test 'equal))
         (shipit-notifications-auto-mark-resolved-approvals t)
         (marked nil)
         (activity '((repo . "owner/repo")
                     (number . 42)
                     (type . "workflow")
                     (reason . "approval_requested")
                     (notification . ((id . "42")))))
         (key "owner/repo:workflow:42"))
    (puthash key activity shipit--notification-pr-activities)
    (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
               (lambda (_config id) (setq marked id))))
      (shipit--enrich-workflow-one-repo "owner/repo"
                                        (list (cons key activity))
                                        nil))
    (should (equal "42" marked))
    (should-not (gethash key shipit--notification-pr-activities))))

(ert-deftest test-enrich-workflow-one-repo-opt-out-keeps-notification ()
  "GIVEN no waiting runs and the defcustom disabled
WHEN enrichment runs
THEN the notification stays in the hash and no mark-read is called —
the opt-in gate must be respected."
  (let* ((shipit--notification-pr-activities (make-hash-table :test 'equal))
         (shipit-notifications-auto-mark-resolved-approvals nil)
         (marked nil)
         (activity '((repo . "owner/repo")
                     (number . 42)
                     (type . "workflow")
                     (reason . "approval_requested")
                     (notification . ((id . "42")))))
         (key "owner/repo:workflow:42"))
    (puthash key activity shipit--notification-pr-activities)
    (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
               (lambda (_config id) (setq marked id))))
      (shipit--enrich-workflow-one-repo "owner/repo"
                                        (list (cons key activity))
                                        nil))
    (should-not marked)
    (should (gethash key shipit--notification-pr-activities))))

(ert-deftest test-notifications-buffer-type-filter-restricts-pool ()
  "GIVEN activities of two types
WHEN `shipit-notifications-buffer--type-filter' is set to one of them
THEN the buffer only renders that type."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:pr:1"
             '((repo . "owner/repo") (number . 1) (type . "pr")
               (subject . "PR one") (reason . "review_requested")
               (updated-at . "2026-04-24T08:00:00Z"))
             shipit--notification-pr-activities)
    (puthash "owner/repo:workflow:2"
             '((repo . "owner/repo") (number . 2) (type . "workflow")
               (subject . "Deploy two") (reason . "approval_requested")
               (updated-at . "2026-04-24T08:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local shipit-notifications-buffer--type-filter "workflow")
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (should (search-forward "Deploy two" nil t))
            (goto-char (point-min))
            (should-not (search-forward "PR one" nil t)))
        (kill-buffer buf)))))

(ert-deftest test-notifications-buffer-candidate-types-from-hash ()
  "GIVEN activities of types pr + workflow
WHEN gathering type-filter candidates
THEN the sorted distinct type list is returned."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:pr:1"
             '((repo . "owner/repo") (number . 1) (type . "pr"))
             shipit--notification-pr-activities)
    (puthash "owner/repo:workflow:2"
             '((repo . "owner/repo") (number . 2) (type . "workflow"))
             shipit--notification-pr-activities)
    (puthash "owner/repo:pr:3"
             '((repo . "owner/repo") (number . 3) (type . "pr"))
             shipit--notification-pr-activities)
    (should (equal '("pr" "workflow")
                   (shipit-notifications-buffer--candidate-types)))))

(ert-deftest test-notifications-buffer-M-n-bound-to-load-more ()
  "GIVEN the notifications buffer
WHEN looking up M-n in its mode map
THEN it runs `shipit-notifications-buffer-page-forward'."
  (require 'shipit-notifications-buffer)
  (should (eq 'shipit-notifications-buffer-page-forward
              (lookup-key shipit-notifications-buffer-mode-map (kbd "M-n")))))

(ert-deftest test-notifications-buffer-M-p-bound-to-page-back ()
  "GIVEN the notifications buffer
WHEN looking up M-p in its mode map
THEN it runs `shipit-notifications-buffer-page-back'."
  (require 'shipit-notifications-buffer)
  (should (eq 'shipit-notifications-buffer-page-back
              (lookup-key shipit-notifications-buffer-mode-map (kbd "M-p")))))

(ert-deftest test-notifications-buffer-page-back-refuses-at-page-1 ()
  "GIVEN page-limit=1 in all scope
WHEN invoking `shipit-notifications-buffer-page-back'
THEN a user-error is signalled — there is nothing to drop."
  (require 'shipit-notifications-buffer)
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local shipit-notifications-buffer--display-scope 'all)
          (setq-local shipit-notifications-buffer--page-limit 1)
          (should-error (shipit-notifications-buffer-page-back)
                        :type 'user-error))
      (kill-buffer buf))))

(ert-deftest test-notifications-buffer-first-page-resets-current ()
  "GIVEN a buffer in all scope at page 5
WHEN `shipit-notifications-buffer-first-page' is invoked
THEN current-page becomes 1 and a refresh is triggered."
  (require 'shipit-notifications-buffer)
  (cl-letf* ((refreshed nil)
             ((symbol-function 'shipit-notifications-buffer-refresh)
              (lambda (&rest _) (setq refreshed t))))
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local shipit-notifications-buffer--display-scope 'all)
            (setq-local shipit-notifications-buffer--current-page 5)
            (shipit-notifications-buffer-first-page)
            (should (= 1 shipit-notifications-buffer--current-page))
            (should refreshed))
        (kill-buffer buf)))))

(ert-deftest test-notifications-buffer-last-page-uses-total-count ()
  "GIVEN total-count=250 (=> 3 pages) in all scope at page 1
WHEN `shipit-notifications-buffer-last-page' is invoked
THEN current-page becomes 3."
  (require 'shipit-notifications-buffer)
  (cl-letf* ((refreshed nil)
             ((symbol-function 'shipit-notifications-buffer-refresh)
              (lambda (&rest _) (setq refreshed t))))
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local shipit-notifications-buffer--display-scope 'all)
            (setq-local shipit-notifications-buffer--current-page 1)
            (setq-local shipit-notifications-buffer--total-count 250)
            (shipit-notifications-buffer-last-page)
            (should (= 3 shipit-notifications-buffer--current-page))
            (should refreshed))
        (kill-buffer buf)))))

(ert-deftest test-notifications-buffer-last-page-refuses-without-total ()
  "GIVEN total-count is nil (probe not returned yet)
WHEN `shipit-notifications-buffer-last-page' is invoked
THEN it signals a user-error instead of guessing."
  (require 'shipit-notifications-buffer)
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local shipit-notifications-buffer--display-scope 'all)
          (setq-local shipit-notifications-buffer--total-count nil)
          (should-error (shipit-notifications-buffer-last-page)
                        :type 'user-error))
      (kill-buffer buf))))

(ert-deftest test-notifications-buffer-goto-page-validates-bounds ()
  "GIVEN total-count=250 (=> 3 pages) and scope=all
WHEN `shipit-notifications-buffer-goto-page' is called with 5
THEN it signals a user-error (page exceeds last)."
  (require 'shipit-notifications-buffer)
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq-local shipit-notifications-buffer--display-scope 'all)
          (setq-local shipit-notifications-buffer--total-count 250)
          (should-error (shipit-notifications-buffer-goto-page 5)
                        :type 'user-error))
      (kill-buffer buf))))

(ert-deftest test-process-notifications-commit-falls-back-to-thread-id ()
  "GIVEN a Commit notification whose subject URL has a SHA, not a number
WHEN processed
THEN the activity key uses the thread id as a numeric fallback so that
every notification is unique — no silent drops."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil)
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (notifications
         (list
          `((id . "555")
            (subject . ((type . "Commit")
                        (title . "Add feature foo")
                        (url . "https://api.github.com/repos/owner/repo/commits/abcdef0123")))
            (reason . "ci_activity")
            (unread . t)
            (repository . ((full_name . "owner/repo")
                           (html_url . "https://github.com/owner/repo")))
            (updated_at . "2026-04-24T08:00:00Z")))))
    (shipit--process-notifications notifications)
    (should (= 1 (hash-table-count shipit--notification-pr-activities)))
    (let ((activity (gethash "owner/repo:commit:555"
                             shipit--notification-pr-activities)))
      (should activity)
      (should (equal "commit" (cdr (assq 'type activity))))
      (should (equal "https://github.com/owner/repo/commit/abcdef0123"
                     (cdr (assq 'browse-url activity)))))))

;;; Mixed PR + Issue Processing Tests

(ert-deftest test-process-notifications-mixed-types ()
  "GIVEN notifications with both PR and Issue of same number
WHEN processing notifications
THEN both are stored with distinct keys."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil)
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (notifications
         (list
          ;; PR notification
          `((id . "notif-1")
            (subject . ((type . "PullRequest")
                        (title . "PR Title")
                        (url . "https://api.github.com/repos/owner/repo/pulls/42")))
            (reason . "review_requested")
            (unread . t)
            (repository . ((full_name . "owner/repo")))
            (updated_at . "2025-01-29T10:00:00Z"))
          ;; Issue notification with same number
          `((id . "notif-2")
            (subject . ((type . "Issue")
                        (title . "Issue Title")
                        (url . "https://api.github.com/repos/owner/repo/issues/42")))
            (reason . "mention")
            (unread . t)
            (repository . ((full_name . "owner/repo")))
            (updated_at . "2025-01-29T11:00:00Z")))))

    (shipit--process-notifications notifications)

    ;; THEN both entries exist with distinct keys
    (should (= 2 (hash-table-count shipit--notification-pr-activities)))
    (should (gethash "owner/repo:pr:42" shipit--notification-pr-activities))
    (should (gethash "owner/repo:issue:42" shipit--notification-pr-activities))

    ;; Verify types are correctly set
    (let ((pr-activity (gethash "owner/repo:pr:42" shipit--notification-pr-activities))
          (issue-activity (gethash "owner/repo:issue:42" shipit--notification-pr-activities)))
      (should (equal "pr" (cdr (assq 'type pr-activity))))
      (should (equal "issue" (cdr (assq 'type issue-activity))))
      (should (equal "PR Title" (cdr (assq 'subject pr-activity))))
      (should (equal "Issue Title" (cdr (assq 'subject issue-activity)))))))

;;; Buffer Type Property Tests

(ert-deftest test-notifications-buffer-renders-type-property ()
  "GIVEN a notification with type field
WHEN rendered in the buffer
THEN section value contains the correct type."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:issue:99"
             '((repo . "owner/repo")
               (number . 99)
               (type . "issue")
               (subject . "Bug report")
               (reason . "mention")
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "#99")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (equal "issue" (cdr (assq 'type activity)))))))
        (kill-buffer buf)))))

;;; Action Menu Type Tests

(ert-deftest test-notification-actions-pr-type ()
  "GIVEN a PR notification at point
WHEN getting section data
THEN type is \"pr\"."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:pr:42"
             '((repo . "owner/repo")
               (number . 42)
               (type . "pr")
               (subject . "PR Title")
               (reason . "review_requested")
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "#42")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (equal "pr" (cdr (assq 'type activity)))))))
        (kill-buffer buf)))))

(ert-deftest test-notification-actions-issue-type ()
  "GIVEN an Issue notification at point
WHEN getting section data
THEN type is \"issue\"."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:issue:42"
             '((repo . "owner/repo")
               (number . 42)
               (type . "issue")
               (subject . "Issue Title")
               (reason . "mention")
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "#42")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (equal "issue" (cdr (assq 'type activity)))))))
        (kill-buffer buf)))))

(ert-deftest test-shipit--notification-actions-includes-auto-mark-shortcut-pr ()
  "GIVEN a PR activity
WHEN `shipit--notification-actions' is invoked
THEN the action choices offered to the user contain
`Manage auto-mark rules…' so the dwim menu has a discoverable
shortcut into the rules transient."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 ;; Pick a benign action so dispatch does nothing destructive.
                 "Mark as read"))
              ((symbol-function 'shipit--mark-notification-read)
               (lambda (&rest _) nil)))
      (shipit--notification-actions
       '((repo . "owner/repo") (number . 1) (type . "pr")))
      (should (member "Manage auto-mark rules…" captured-actions)))))

(ert-deftest test-shipit--notification-actions-includes-auto-mark-shortcut-issue ()
  "GIVEN an Issue activity
WHEN `shipit--notification-actions' is invoked
THEN the action choices contain `Manage auto-mark rules…'."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Mark as read"))
              ((symbol-function 'shipit--mark-notification-read)
               (lambda (&rest _) nil)))
      (shipit--notification-actions
       '((repo . "owner/repo") (number . 2) (type . "issue")))
      (should (member "Manage auto-mark rules…" captured-actions)))))

(ert-deftest test-shipit--notification-actions-dispatches-auto-mark-shortcut ()
  "GIVEN the user picks `Manage auto-mark rules…' from the dwim menu
WHEN dispatch runs
THEN `shipit-notifications-buffer-auto-mark-menu' is invoked."
  (let ((called 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Manage auto-mark rules…"))
              ((symbol-function 'shipit-notifications-buffer-auto-mark-menu)
               (lambda (&rest _) (cl-incf called))))
      (shipit--notification-actions
       '((repo . "owner/repo") (number . 1) (type . "pr")))
      (should (= called 1)))))

(ert-deftest test-notification-buffer-stores-backend-text-properties ()
  "GIVEN a Jira notification with backend-id and backend-config
WHEN rendered in the buffer
THEN text has shipit-notification-backend-id and backend-config properties."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (config '(:base-url "https://jira.example.com" :project-keys ("PRJ"))))
    (puthash "MyJira:issue:PRJ-42"
             `((repo . "MyJira")
               (number . "PRJ-42")
               (type . "issue")
               (subject . "Bug report")
               (reason . "updated")
               (source . jira)
               (backend-id . jira)
               (backend-config . ,config)
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "PRJ-42")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (eq 'jira (cdr (assq 'backend-id activity))))
                (should (equal config (cdr (assq 'backend-config activity)))))))
        (kill-buffer buf)))))

;;; Backend Notification Support Tests

(ert-deftest test-shipit-issue--backend-has-notifications-p-true ()
  "GIVEN a backend plist with :notifications key
WHEN checking for notification support
THEN returns non-nil."
  (require 'shipit-issue-backends)
  (let ((plist '(:name "Test" :notifications #'ignore)))
    (should (shipit-issue--backend-has-notifications-p plist))))

(ert-deftest test-shipit-issue--backend-has-notifications-p-false ()
  "GIVEN a backend plist without :notifications key
WHEN checking for notification support
THEN returns nil."
  (require 'shipit-issue-backends)
  (let ((plist '(:name "Test")))
    (should-not (shipit-issue--backend-has-notifications-p plist))))

;;; Jira Notification JQL Tests

(ert-deftest test-jira-build-notifications-jql ()
  "GIVEN Jira config with project keys
WHEN building notifications JQL
THEN includes project filter and updated clause."
  (require 'shipit-issue-jira)
  (let ((config '(:base-url "https://jira.example.com" :project-keys ("PRJ" "TEAM")))
        (since-time "2025-01-29 10:00"))
    (let ((jql (shipit-issue-jira--build-notifications-jql config since-time)))
      (should (string-match-p "project in" jql))
      (should (string-match-p "PRJ" jql))
      (should (string-match-p "TEAM" jql))
      (should (string-match-p "updated >=" jql))
      (should (string-match-p "2025-01-29 10:00" jql)))))

(ert-deftest test-jira-fetch-notifications-async-callback ()
  "GIVEN the Jira async fetch helper, mocking the underlying API
WHEN `shipit-issue-jira--fetch-notifications-async' runs
THEN the user-supplied callback is invoked with normalized
activity alists derived from the fake JSON."
  (require 'shipit-issue-jira)
  (let ((received nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-async)
               (lambda (_config _path callback)
                 (funcall callback
                          '((issues . (((key . "PRJ-1")
                                        (fields . ((summary . "First")
                                                   (updated . "2026-04-25T10:00:00.000+0000"))))
                                       ((key . "PRJ-2")
                                        (fields . ((summary . "Second")
                                                   (updated . "2026-04-25T11:00:00.000+0000")))))))))))
      (shipit-issue-jira--fetch-notifications-async
       '(:base-url "https://x" :project-keys ("PRJ") :display-name "MyJira")
       nil
       (lambda (activities) (setq received activities))))
    (should (= 2 (length received)))
    (should (equal "PRJ-1" (cdr (assq 'number (car received)))))
    (should (equal "First" (cdr (assq 'subject (car received)))))))

(ert-deftest test-poll-one-backend-prefers-async ()
  "GIVEN a backend with both :notifications and :notifications-async
WHEN polling
THEN the async function is used and the sync function is not called."
  (let ((sync-called nil)
        (async-called nil)
        (entry '("repo" :backend test-backend))
        (shipit-issue-backends nil)
        (shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (push (cons 'test-backend
                (list :name "Test"
                      :notifications (lambda (_c _s) (setq sync-called t) nil)
                      :notifications-async (lambda (_c _s cb)
                                             (setq async-called t)
                                             (when cb (funcall cb nil)))))
          shipit-issue-backends)
    (shipit--poll-one-backend entry nil)
    (should async-called)
    (should-not sync-called)))

(ert-deftest test-jira-issue-to-activity ()
  "GIVEN a raw Jira issue response
WHEN converting to activity alist
THEN has correct fields."
  (require 'shipit-issue-jira)
  (let* ((config '(:project-keys ("PRJ") :display-name "MyJira"))
         (issue '((key . "PRJ-42")
                  (fields . ((summary . "Fix the bug")
                             (updated . "2025-01-29T12:00:00.000+0000")))))
         (activity (shipit-issue-jira--issue-to-activity config issue)))
    (should (equal "MyJira" (cdr (assq 'repo activity))))
    (should (equal "PRJ-42" (cdr (assq 'number activity))))
    (should (equal "issue" (cdr (assq 'type activity))))
    (should (equal "Fix the bug" (cdr (assq 'subject activity))))
    (should (equal "updated" (cdr (assq 'reason activity))))))

(ert-deftest test-jira-issue-to-activity-includes-backend-info ()
  "GIVEN a raw Jira issue response and config
WHEN converting to activity alist
THEN includes backend-id and backend-config for buffer routing."
  (require 'shipit-issue-jira)
  (let* ((config '(:project-keys ("PRJ") :display-name "MyJira"
                   :base-url "https://jira.example.com"))
         (issue '((key . "PRJ-42")
                  (fields . ((summary . "Fix the bug")
                             (updated . "2025-01-29T12:00:00.000+0000")))))
         (activity (shipit-issue-jira--issue-to-activity config issue)))
    (should (eq 'jira (cdr (assq 'backend-id activity))))
    (should (equal config (cdr (assq 'backend-config activity))))))

(ert-deftest test-jira-notifications-since-time ()
  "GIVEN a since timestamp
WHEN computing notifications since time
THEN subtracts 5 minute overlap margin."
  (require 'shipit-issue-jira)
  ;; With nil since, should return roughly 1 hour ago
  (let ((result (shipit-issue-jira--notifications-since-time nil)))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}$" result))))

;;; Modeline Icon Preservation Tests

(ert-deftest test-modeline-indicator-uses-emoji-bell ()
  "GIVEN notifications enabled with no mentions
WHEN shipit--update-modeline-indicator builds the modeline string
THEN the string contains the bell emoji and count."
  (let ((shipit-notifications-enabled t)
        (shipit--mention-count 0)
        (shipit--modeline-string nil)
        (shipit--notification-count 0))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (shipit--update-modeline-indicator 5)
      (should shipit--modeline-string)
      (should (string-match-p "🔔" shipit--modeline-string))
      (should (string-match-p "5" shipit--modeline-string)))))

(ert-deftest test-modeline-indicator-with-mentions-uses-emoji-bell ()
  "GIVEN non-zero mention count
WHEN shipit--update-modeline-indicator builds the modeline string
THEN the string contains @ mention count, bell emoji, and total count."
  (let ((shipit-notifications-enabled t)
        (shipit--mention-count 3)
        (shipit--modeline-string nil)
        (shipit--notification-count 0))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (shipit--update-modeline-indicator 10)
      (should shipit--modeline-string)
      (should (string-match-p "@3" shipit--modeline-string))
      (should (string-match-p "🔔" shipit--modeline-string))
      (should (string-match-p "10" shipit--modeline-string)))))

(ert-deftest test-modeline-indicator-with-emoji-fallback ()
  "GIVEN an emoji fallback icon (no display property)
WHEN shipit--update-modeline-indicator builds the modeline string
THEN the emoji text is present in the result."
  (let ((shipit-notifications-enabled t)
        (shipit--mention-count 0)
        (shipit--modeline-string nil)
        (shipit--notification-count 0))
    (cl-letf (((symbol-function 'shipit--get-notification-icon)
               (lambda (emoji) emoji))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (shipit--update-modeline-indicator 7)
      ;; THEN modeline string contains the count
      (should shipit--modeline-string)
      (should (string-match-p "7" shipit--modeline-string)))))

;;; Activity Section Rendering Tests

(ert-deftest test-issue-activity-section-renders-changelog ()
  "GIVEN issue data with changelog entries
WHEN rendering the activity section
THEN changelog events appear with author and change description."
  (require 'shipit-issues-buffer)
  (let ((issue-data
         `((id . "PRJ-42")
           (number . "PRJ-42")
           (title . "Test issue")
           (state . "In Progress")
           (body . "test body")
           (user . ((login . "Alice") (avatar_url . nil)))
           (labels . ())
           (created_at . "2024-01-15T10:30:00.000+0000")
           (updated_at . "2024-01-20T14:00:00.000+0000")
           (changelog . (((id . "100")
                           (created_at . "2024-01-16T10:00:00.000+0000")
                           (user . ((login . "Alice") (avatar_url . nil)))
                           (items . (((field . "status")
                                      (from . "To Do")
                                      (to . "In Progress")))))
                          ((id . "101")
                           (created_at . "2024-01-17T11:00:00.000+0000")
                           (user . ((login . "Bob") (avatar_url . nil)))
                           (items . (((field . "assignee")
                                      (from . "")
                                      (to . "Alice"))))))))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (_root 'root)
          (shipit-issue--insert-activity-section "test/repo" issue-data)))
      ;; THEN buffer contains Activity heading with count
      (goto-char (point-min))
      (should (search-forward "Activity" nil t))
      ;; THEN assignee change is shown (newest first)
      (should (search-forward "Assignee" nil t))
      ;; THEN status change is shown
      (should (search-forward "Status" nil t)))))

(ert-deftest test-issue-activity-section-empty-state-without-changelog ()
  "GIVEN issue data without changelog key and loading-p=nil
WHEN rendering the activity section
THEN Activity (0) heading is shown with empty state message.
Callers that will fetch asynchronously pass loading-p=t and get a
loading placeholder instead; see
`test-issue-activity-section-loading-state-when-async'."
  (require 'shipit-issues-buffer)
  (let ((issue-data
         '((id . "PRJ-42")
           (number . "PRJ-42")
           (title . "Test issue")
           (state . "Open")
           (body . "test")
           (user . ((login . "Alice") (avatar_url . nil)))
           (labels . ())
           (created_at . "2024-01-15T10:30:00.000+0000")
           (updated_at . "2024-01-15T10:30:00.000+0000"))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (_root 'root)
          (shipit-issue--insert-activity-section "test/repo" issue-data nil))
        (goto-char (point-min))
        (should (search-forward "Activity (0)" nil t))
        (should (search-forward "No activity yet" nil t))))))

(ert-deftest test-issue-activity-section-loading-state-when-async ()
  "GIVEN issue data without changelog key and loading-p=t
WHEN rendering the activity section
THEN the heading shows a loading indicator and the body says
\"Loading activity…\" — the async events fetch will replace this
placeholder once it completes."
  (require 'shipit-issues-buffer)
  (let ((issue-data
         '((id . "42")
           (number . 42)
           (title . "Popular issue")
           (state . "Open")
           (body . "test"))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (_root 'root)
          (shipit-issue--insert-activity-section "owner/repo" issue-data t))
        (goto-char (point-min))
        (should (search-forward "Activity (…)" nil t))
        (goto-char (point-min))
        (should (search-forward "Loading activity" nil t))
        (goto-char (point-min))
        (should-not (search-forward "No activity yet" nil t))))))

(ert-deftest test-issue-format-changelog-items-status-change ()
  "GIVEN a changelog item for status change
WHEN formatting
THEN shows 'changed Status: X -> Y'."
  (require 'shipit-issues-buffer)
  (let ((items '(((field . "status") (from . "To Do") (to . "In Progress")))))
    (let ((result (shipit-issue--format-changelog-items items)))
      (should (string-match-p "Status" result))
      (should (string-match-p "To Do" result))
      (should (string-match-p "In Progress" result)))))

(ert-deftest test-issue-format-changelog-items-description-change ()
  "GIVEN a changelog item for description change
WHEN formatting
THEN shows 'updated Description' without from/to."
  (require 'shipit-issues-buffer)
  (let ((items '(((field . "description") (from . "old text") (to . "new text")))))
    (let ((result (shipit-issue--format-changelog-items items)))
      (should (string-match-p "Description" result))
      (should-not (string-match-p "old text" result)))))

;;; Source Icon Tests

(ert-deftest test-shipit-notification-source-icon-gitlab ()
  "GIVEN source is gitlab (non-graphic mode)
WHEN getting source icon
THEN returns \"GL\" with GitLab orange color."
  (require 'shipit-render)
  (let ((icon (shipit--get-notification-source-icon 'gitlab)))
    (should (string-match-p "GL" icon))))

(ert-deftest test-shipit-notification-source-icon-jira ()
  "GIVEN source is jira (non-graphic mode)
WHEN getting source icon
THEN returns \"JR\" with Jira blue color."
  (require 'shipit-render)
  (let ((icon (shipit--get-notification-source-icon 'jira)))
    (should (string-match-p "JR" icon))))

(ert-deftest test-shipit-notification-source-icon-github ()
  "GIVEN source is nil (GitHub, the default)
WHEN getting source icon
THEN returns \"GH\"."
  (require 'shipit-render)
  (let ((icon (shipit--get-notification-source-icon nil)))
    (should (string-match-p "GH" icon))))

(ert-deftest test-shipit-notification-buffer-shows-source-icon ()
  "GIVEN a GitLab notification with source=gitlab
WHEN rendered in the buffer
THEN the line contains \"GL\" source indicator."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "g/p:issue:5"
             '((repo . "g/p")
               (number . 5)
               (type . "issue")
               (subject . "GitLab issue")
               (reason . "assign")
               (source . gitlab)
               (backend-id . gitlab)
               (updated-at . "2026-02-15T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              ;; Should contain GL source indicator
              (should (search-forward "GL" nil t))))
        (kill-buffer buf)))))

(ert-deftest test-notification-activity-section-type ()
  "GIVEN the notification-activity section type
   WHEN checking its magit-section property
   THEN it is registered as a magit section."
  (should (get 'notification-activity 'magit-section)))

;;; Activity Timeline Filter Tests

(ert-deftest test-filter-events-since-timestamp ()
  "GIVEN a list of timeline events and a cutoff timestamp
   WHEN shipit-notifications-buffer--filter-events-since is called
   THEN only events at or after the cutoff are returned."
  (require 'shipit-notifications-buffer)
  (let ((events '(((event . "commented") (created_at . "2026-03-20T10:00:00Z"))
                  ((event . "reviewed") (created_at . "2026-03-21T14:00:00Z"))
                  ((event . "commented") (created_at . "2026-03-22T08:00:00Z"))))
        (cutoff "2026-03-21T12:00:00Z"))
    (let ((filtered (shipit-notifications-buffer--filter-events-since events cutoff)))
      (should (= (length filtered) 2))
      (should (equal "2026-03-21T14:00:00Z"
                     (cdr (assq 'created_at (car filtered))))))))

(ert-deftest test-filter-events-since-uses-submitted-at ()
  "GIVEN events with submitted_at instead of created_at
   WHEN filtering since a cutoff
   THEN submitted_at is used for comparison."
  (require 'shipit-notifications-buffer)
  (let ((events '(((event . "reviewed") (submitted_at . "2026-03-22T08:00:00Z"))))
        (cutoff "2026-03-21T00:00:00Z"))
    (let ((filtered (shipit-notifications-buffer--filter-events-since events cutoff)))
      (should (= (length filtered) 1)))))

(ert-deftest test-filter-events-since-empty ()
  "GIVEN events all before the cutoff
   WHEN filtering
   THEN empty list is returned."
  (require 'shipit-notifications-buffer)
  (let ((events '(((event . "commented") (created_at . "2026-03-20T10:00:00Z"))))
        (cutoff "2026-03-21T00:00:00Z"))
    (let ((filtered (shipit-notifications-buffer--filter-events-since events cutoff)))
      (should (= (length filtered) 0)))))

;;; Activity Sub-Section Insertion Tests

(ert-deftest test-insert-notification-activity-section ()
  "GIVEN timeline events and a cutoff timestamp
   WHEN shipit-notifications-buffer--insert-activity-section is called
   THEN a notification-activity magit section is created with events."
  (require 'shipit-notifications-buffer)
  (require 'shipit-pr-sections)
  (let ((events '(((event . "commented")
                   (created_at . "2026-03-22T08:00:00Z")
                   (user . ((login . "alice")))
                   (body . "looks good"))
                  ((event . "reviewed")
                   (state . "approved")
                   (submitted_at . "2026-03-22T09:00:00Z")
                   (user . ((login . "bob"))))))
        (cutoff "2026-03-22T00:00:00Z"))
    (with-temp-buffer
      (magit-insert-section (notifications-root)
        (magit-insert-section (notification-entry nil)
          (magit-insert-heading "Test notification")
          (let ((magit-insert-section--parent
                 magit-insert-section--current))
            (shipit-notifications-buffer--insert-activity-section
             events cutoff "owner/repo" 42))))
      (goto-char (point-min))
      ;; Find the notification-activity section
      (let ((found nil))
        (dolist (child (oref (car (oref magit-root-section children)) children))
          (when (eq (oref child type) 'notification-activity)
            (setq found t)))
        (should found)))))

;;; Total-count probe parsing

(ert-deftest test-shipit--notifications-count-from-response-link-last ()
  "GIVEN response headers with a `rel=\"last\"' Link of page=42 and
any non-empty JSON body
WHEN extracting the total count for a per_page=1 probe
THEN the count is 42 (one item per page, so last page = total items)."
  (let ((headers '(("Link" . "<https://api.github.com/notifications?per_page=1&page=2>; rel=\"next\", <https://api.github.com/notifications?per_page=1&page=42>; rel=\"last\"")))
        (json '(((id . "1")))))
    (should (= 42 (shipit--notifications-count-from-response headers json)))))

(ert-deftest test-shipit--notifications-count-from-response-single-item ()
  "GIVEN headers without a `rel=\"last\"' entry and a one-item body
WHEN extracting the count
THEN it falls back to (length json), i.e. 1."
  (let ((headers '(("Link" . "<https://…>; rel=\"next\"")))
        (json '(((id . "only")))))
    (should (= 1 (shipit--notifications-count-from-response headers json)))))

(ert-deftest test-shipit--notifications-count-from-response-empty ()
  "GIVEN headers with no Link and an empty JSON body
WHEN extracting the count
THEN it returns 0."
  (should (= 0 (shipit--notifications-count-from-response nil nil)))
  (should (= 0 (shipit--notifications-count-from-response '() '()))))

;;; Auto-mark-read rules

(ert-deftest test-shipit--auto-mark-rule-empty-rule-matches-anything ()
  "GIVEN an empty plist rule
WHEN matched against any activity
THEN the rule matches (no conditions to fail)."
  (should (shipit--auto-mark-rule-matches-activity-p
           '() '((type . "pr") (pr-state . "open")))))

(ert-deftest test-shipit--auto-mark-rule-state-merged-matches ()
  "GIVEN rule (:state merged)
WHEN matched against PR activities of various states
THEN only merged PRs match; closed/open do not; non-PR activities
without `pr-state' do not match."
  (let ((rule '(:state merged)))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((type . "pr") (pr-state . "merged"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((type . "pr") (pr-state . "closed"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((type . "pr") (pr-state . "open"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((type . "issue") (number . 1))))))

(ert-deftest test-shipit--auto-mark-rule-state-accepts-symbol-or-string ()
  "GIVEN rule with :state as symbol vs string
WHEN matched
THEN both forms work identically."
  (should (shipit--auto-mark-rule-matches-activity-p
           '(:state merged) '((type . "pr") (pr-state . "merged"))))
  (should (shipit--auto-mark-rule-matches-activity-p
           '(:state "merged") '((type . "pr") (pr-state . "merged")))))

(ert-deftest test-shipit--auto-mark-rule-state-draft-requires-open-and-draft ()
  "GIVEN rule (:state draft)
WHEN matched
THEN only PRs with `pr-state' = `open' AND `draft' = t match."
  (let ((rule '(:state draft)))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((type . "pr") (pr-state . "open") (draft . t))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((type . "pr") (pr-state . "open"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((type . "pr") (pr-state . "merged") (draft . t))))))

(ert-deftest test-shipit--auto-mark-rule-state-open-excludes-drafts ()
  "GIVEN rule (:state open)
WHEN matched
THEN open non-draft PRs match; open-draft PRs do not — `open'
in the rule mirrors the icon-picker semantics."
  (let ((rule '(:state open)))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((type . "pr") (pr-state . "open"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((type . "pr") (pr-state . "open") (draft . t))))))

(ert-deftest test-shipit--auto-mark-rule-type-condition ()
  "GIVEN a :type rule
WHEN matched
THEN only activities with that internal type match."
  (let ((rule '(:type "workflow")))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((type . "workflow"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((type . "pr"))))))

(ert-deftest test-shipit--auto-mark-rule-reason-condition ()
  "GIVEN a :reason rule
WHEN matched
THEN only activities with that GitHub reason string match."
  (let ((rule '(:reason "subscribed")))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((reason . "subscribed"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((reason . "mention"))))))

(ert-deftest test-shipit--auto-mark-rule-title-regex ()
  "GIVEN rule (:title REGEX)
WHEN matched
THEN the regex is applied to the activity's `subject' field."
  (let ((rule '(:title "^chore: bump")))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((subject . "chore: bump foo from 1.0 to 1.1"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((subject . "fix: critical bug"))))
    ;; Missing subject shouldn't crash, just fail to match.
    (should-not (shipit--auto-mark-rule-matches-activity-p rule '()))))

(ert-deftest test-shipit--auto-mark-rule-repo-case-insensitive ()
  "GIVEN a :repo rule with mixed-case slug
WHEN matched
THEN repo comparison is case-insensitive (GitHub slugs are)."
  (let ((rule '(:repo "Owner/Foo")))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((repo . "owner/foo"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((repo . "owner/bar"))))))

(ert-deftest test-shipit--auto-mark-rule-conditions-and-together ()
  "GIVEN rule with multiple conditions
WHEN matched
THEN the activity must satisfy ALL conditions for the rule to fire."
  (let ((rule '(:repo "owner/foo" :type "workflow")))
    (should (shipit--auto-mark-rule-matches-activity-p
             rule '((repo . "owner/foo") (type . "workflow"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((repo . "owner/foo") (type . "pr"))))
    (should-not (shipit--auto-mark-rule-matches-activity-p
                 rule '((repo . "owner/bar") (type . "workflow"))))))

(ert-deftest test-shipit--auto-mark-rules-apply-marks-matching ()
  "GIVEN a state rule and a title-regex rule
and 4 activities (merged PR, open PR, bump PR, issue)
WHEN `shipit--auto-mark-rules-apply' runs
THEN merged PR and bump PR are marked; open PR and issue are not;
return value equals 2."
  (let ((marked '())
        (shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit-notifications-auto-mark-read-rules
         '((:state merged) (:title "^chore: bump"))))
    (cl-letf (((symbol-function 'shipit--mark-notification-read)
               (lambda (number repo &optional _no-refresh type)
                 (push (list number repo type) marked))))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr")
                 (subject . "Big feature") (pr-state . "open"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:pr:2"
               '((repo . "owner/foo") (number . 2) (type . "pr")
                 (subject . "Old work") (pr-state . "merged"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:pr:3"
               '((repo . "owner/foo") (number . 3) (type . "pr")
                 (subject . "chore: bump dep from 1 to 2")
                 (pr-state . "open"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:issue:4"
               '((repo . "owner/foo") (number . 4) (type . "issue")
                 (subject . "Something wrong"))
               shipit--notification-pr-activities)
      (let ((count (shipit--auto-mark-rules-apply)))
        (should (= count 2))
        (should (member '(2 "owner/foo" "pr") marked))
        (should (member '(3 "owner/foo" "pr") marked))
        (should-not (member '(1 "owner/foo" "pr") marked))
        (should-not (member '(4 "owner/foo" "issue") marked))))))

(ert-deftest test-shipit--auto-mark-rules-apply-no-rules-is-noop ()
  "GIVEN empty rules list and a populated hash
WHEN apply runs
THEN it returns 0 and never calls `shipit--mark-notification-read'."
  (let ((called nil)
        (shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit-notifications-auto-mark-read-rules nil))
    (cl-letf (((symbol-function 'shipit--mark-notification-read)
               (lambda (&rest _) (setq called t))))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr")
                 (pr-state . "merged"))
               shipit--notification-pr-activities)
      (should (= 0 (shipit--auto-mark-rules-apply)))
      (should-not called))))

;;; Mark-thread-read dispatch

(ert-deftest test-shipit--dispatch-mark-thread-read-uses-backend-plist ()
  "GIVEN a PR backend that exposes `:mark-notification-read'
WHEN `shipit--dispatch-mark-thread-read' is invoked for a repo
THEN the backend's plist function is called with the resolved
config and the thread-id (no direct backend-specific call)."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :name "Mock"
                             :mark-notification-read
                             (lambda (config id)
                               (setq called-with (list config id))))
                       (list :repo "mock/repo")))))
      (shipit--dispatch-mark-thread-read "mock/repo" "12345")
      (should (equal called-with
                     (list (list :repo "mock/repo") "12345"))))))

(ert-deftest test-shipit--dispatch-mark-thread-read-noop-when-backend-misses ()
  "GIVEN a PR backend without `:mark-notification-read'
WHEN the dispatcher runs
THEN it logs and returns nil — never raises and never falls back
to a backend-specific symbol."
  (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
             (lambda (_repo) (cons (list :name "Mock") (list :repo "x")))))
    (should (null (shipit--dispatch-mark-thread-read "x" "1")))))

(ert-deftest test-shipit--dispatch-mark-thread-read-tolerates-resolve-error ()
  "GIVEN `shipit-pr--resolve-for-repo' signals (no backend registered)
WHEN the dispatcher runs
THEN it returns nil without raising — the auto-mark feature
should never break notification fetching."
  (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
             (lambda (_repo) (error "no backend"))))
    (should (null (shipit--dispatch-mark-thread-read "x" "1")))))

(provide 'test-notifications-types)
;;; test-notifications-types.el ends here
