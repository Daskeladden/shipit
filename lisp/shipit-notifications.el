;;; shipit-notifications.el --- Notifications integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Notifications integration for shipit
;; Provides background polling, visual indicators, and notification management

;;; Code:
(require 'cl-lib)
(eval-when-compile
  (require 'transient))

;; Load transient if available at runtime, otherwise define stubs
(if (locate-library "transient")
    (require 'transient)
  ;; Stub for transient-quit-one when transient is not available
  (defun transient-quit-one ()
    "Stub for transient-quit-one when transient is not available."
    (message "Transient not available - notifications menu disabled")))

(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-pr-backends)

;; Forward declarations for functions defined later in this file
(declare-function shipit--check-notifications-background-async "shipit-notifications")
(declare-function shipit--notifications-header-actions "shipit-notifications")
(declare-function shipit-discussion--resolve-for-repo "shipit-discussion-backends")

;; Magit section types - declare for byte compiler
(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-heading "magit-section")

;; Load magit at runtime only (not during byte-compilation)
(require 'magit nil t)
(require 'magit-section nil t)
(require 'shipit-discussion-backends nil t)

;;; Backend-aware URL Helpers

(defun shipit--browse-pr-url (repo number)
  "Return browser URL for PR/MR NUMBER in REPO using active backend."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :browse-url) config number)))

(defun shipit--browse-issue-url (repo number)
  "Return browser URL for issue NUMBER in REPO using active backend."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :browse-issue-url)))
    (if fn
        (funcall fn config number)
      ;; Fallback: derive from PR browse URL by replacing the PR path
      (let ((pr-url (funcall (plist-get backend :browse-url) config number)))
        (replace-regexp-in-string "/pull/" "/issues/" pr-url)))))

;;; Customization

(defcustom shipit-notifications-enabled t
  "Enable notifications polling."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-notifications-poll-frequency 300
  "Notification polling frequency in seconds. Default: 5 minute."
  :type 'integer
  :group 'shipit)

(defcustom shipit-notifications-visual-indicator 'modeline
  "Where to show notification indicators.
- modeline: Show count in modeline via `global-mode-string' and `shipit--modeline-string'.
- none: No visual indicators (polling still works).
Note: `shipit--modeline-string' can be read by custom modeline segments (e.g. doom-modeline)."
  :type '(choice (const :tag "Modeline" modeline)
                 (const :tag "None" none))
  :group 'shipit)

(defcustom shipit-notifications-scope 'unread
  "Scope of notifications to monitor."
  :type '(choice (const :tag "All (including watched repos)" all)
                 (const :tag "Participating only (mentions/assigns)" participating)
                 (const :tag "Unread only (including watched)" unread)
                 (const :tag "Unread participating only" unread-participating))
  :group 'shipit)

(defcustom shipit-notifications-column-widths '((repo . 50) (pr . 10) (title . 80) (reason . 30))
  "Column widths for notification display formatting.
- repo: Repository name (owner/repo)
- pr: PR number (without the # prefix)
- title: PR title/subject
- reason: Notification reason (mention, subscribed, review_requested, etc.)
Note: Time column is right-aligned to window edge."
  :type '(alist :key-type (choice (const repo) (const pr) (const title) (const reason))
                :value-type integer)
  :group 'shipit)

(defcustom shipit-notifications-column-spacing 2
  "Number of spaces between notification columns."
  :type 'integer
  :group 'shipit)

(defcustom shipit-notification-alert-reasons '(mention)
  "List of notification reasons that trigger desktop alerts.
Common reasons: mention, review_requested, assign, comment, team_mention."
  :type '(repeat symbol)
  :group 'shipit)

(defcustom shipit-notification-alert-backend 'message
  "Backend for showing notification alerts.
Options: message (minibuffer), alert (alert.el), dbus (Linux), nil (disabled)."
  :type '(choice (const :tag "Minibuffer message" message)
                 (const :tag "alert.el package" alert)
                 (const :tag "D-Bus (Linux)" dbus)
                 (const :tag "Disabled" nil))
  :group 'shipit)

(defcustom shipit-notification-alert-batch-seconds 5
  "Seconds to wait before batching multiple alerts into one."
  :type 'integer
  :group 'shipit)

;;; Variables

(defvar shipit--notifications-timer nil
  "Timer for background notifications polling.")

(defvar shipit--modeline-string nil
  "String to display in modeline for shipit status.")

(defvar shipit--notification-count 0
  "Current notification count for modeline display.")

(defvar shipit--processing-all-scope nil
  "Bound to t while `shipit--process-notifications' is running on a
payload from the `all' scope.  The modeline indicator skips its
update in that case so an all-scope buffer refresh doesn't blank the
bell with an artificially low unread-count — the unread-scope
background poll keeps it accurate.")

(defvar shipit--mention-count 0
  "Current mention count for modeline display.")

(defvar shipit--mention-prs nil
  "List of PRs where user is mentioned with direct links.")

(defvar shipit--notifications-last-modified nil
  "Last-Modified header from previous GET /notifications request.")

(defvar shipit--notifications-poll-interval nil
  "X-Poll-Interval from GitHub notifications API in seconds.")

(defvar shipit--notifications-paused nil
  "Flag indicating notifications polling is temporarily paused (e.g., during PR load).")

(defvar shipit--notifications-pause-depth 0
  "Nesting depth counter for pause/resume calls to handle nested PR loads.")

(defvar shipit--notification-data nil
  "Cached notification data from GitHub API.")

(defvar shipit--notification-pr-activities nil
  "Hash table of repo:type:number -> activity data.")

(defvar shipit--last-notification-count 0
  "Last known notification count for change detection.")

(defvar shipit--pending-alerts '()
  "Pending alerts to be batched.")

(defvar shipit--alert-timer nil
  "Timer for batching alerts.")

(defvar shipit--alerted-notification-ids (make-hash-table :test 'equal)
  "Notification IDs we've already alerted on.")

(defvar shipit--locally-marked-read-notifications (make-hash-table :test 'equal)
  "Notification IDs that we've marked as read locally.
These are filtered from API responses to prevent race conditions
where GitHub's eventual consistency hasn't propagated yet.
Key is notification ID, value is timestamp when marked read.")

(defvar shipit--backend-last-poll-time nil
  "ISO8601 timestamp of last backend notification poll.")

;;; Key and Type Helpers

(defun shipit--notification-activity-key (repo type number)
  "Build activity key from REPO, TYPE, and NUMBER.
Format: \"repo:type:number\" to prevent collisions between PR and Issue."
  (format "%s:%s:%s" repo type number))

(defun shipit--dispatch-mark-thread-read (repo thread-id)
  "Mark notification THREAD-ID as read via the PR backend for REPO.
Resolves the backend through `shipit-pr--resolve-for-repo\=' and
calls its `:mark-notification-read\=' plist function with the
resolved config.  Logs and returns nil when the backend does not
expose `:mark-notification-read\=' — local hash removal stays
the caller\='s responsibility either way."
  (let* ((resolved (ignore-errors (shipit-pr--resolve-for-repo repo)))
         (backend (car-safe resolved))
         (config (cdr-safe resolved))
         (mark-fn (and backend (plist-get backend :mark-notification-read))))
    (cond
     (mark-fn (funcall mark-fn config thread-id))
     (t (shipit--debug-log
         "No :mark-notification-read on backend for %s; skipping thread %s"
         repo thread-id)
        nil))))

(defun shipit--thread-id-as-number (thread-id)
  "Return THREAD-ID parsed as a number, or nil if it is not all digits.
GitHub thread ids are numeric strings; used as a fallback activity
key for subject types without an intrinsic number (e.g. Commit)."
  (and thread-id
       (stringp thread-id)
       (> (length thread-id) 0)
       (cl-every (lambda (c) (and (>= c ?0) (<= c ?9))) thread-id)
       (string-to-number thread-id)))

(defun shipit--extract-notification-number (subject-url subject-type)
  "Extract number from SUBJECT-URL based on SUBJECT-TYPE.
PR URLs have /pulls/N, Issue URLs have /issues/N,
Discussion URLs have /discussions/N, Release URLs have /releases/N,
CheckSuite URLs have /check-suites/N, WorkflowRun URLs have /runs/N.
Returns nil for Commit (SHA, not a number) and other types without
an intrinsic numeric id — callers fall back to the thread id."
  (cond
   ((string= subject-type "PullRequest")
    (when (and subject-url (string-match "/pulls/\\([0-9]+\\)" subject-url))
      (string-to-number (match-string 1 subject-url))))
   ((string= subject-type "Issue")
    (when (and subject-url (string-match "/issues/\\([0-9]+\\)" subject-url))
      (string-to-number (match-string 1 subject-url))))
   ((string= subject-type "Discussion")
    (when (and subject-url (string-match "/discussions/\\([0-9]+\\)" subject-url))
      (string-to-number (match-string 1 subject-url))))
   ((string= subject-type "Release")
    (when (and subject-url (string-match "/releases/\\([0-9]+\\)" subject-url))
      (string-to-number (match-string 1 subject-url))))
   ((string= subject-type "CheckSuite")
    (when (and subject-url (string-match "/check-suites/\\([0-9]+\\)" subject-url))
      (string-to-number (match-string 1 subject-url))))
   ((string= subject-type "WorkflowRun")
    (when (and subject-url (string-match "/runs/\\([0-9]+\\)" subject-url))
      (string-to-number (match-string 1 subject-url))))
   (t nil)))

(defun shipit--notification-type-from-subject (subject-type)
  "Map GitHub SUBJECT-TYPE to a short internal type string.
Known types are \"pr\", \"issue\", \"discussion\", \"release\",
\"check\", \"commit\", \"workflow\", \"alert\", and \"invitation\".
Returns nil for genuinely unknown types so the caller can log and
skip them rather than guess."
  (pcase subject-type
    ("PullRequest" "pr")
    ("Issue" "issue")
    ("Discussion" "discussion")
    ("Release" "release")
    ("CheckSuite" "check")
    ("Commit" "commit")
    ("WorkflowRun" "workflow")
    ("RepositoryVulnerabilityAlert" "alert")
    ("RepositoryAdvisory" "alert")
    ("RepositoryInvitation" "invitation")
    (_ nil)))

(defcustom shipit-notifications-auto-mark-resolved-approvals nil
  "If non-nil, approval-requested workflow notifications whose run is
no longer in status=waiting are automatically marked read at fetch
time.  This keeps the list free of stale approval requests where
the review has already been handled (by you or another reviewer)
or the run has been cancelled.

Disabled by default — enabling it causes shipit to issue PATCH
requests to /notifications/threads/ during each refresh for
resolved approval notifications, which is a silent write to
GitHub.  Turn it on if you want the list pruned automatically."
  :type 'boolean
  :group 'shipit)

(defun shipit--closest-run-by-time (runs target-iso)
  "Return the run from RUNS whose updated_at is closest to TARGET-ISO.
RUNS is a list of run alists; TARGET-ISO is an ISO-8601 timestamp
string.  Used to pick which of several currently-waiting runs a
particular approval-requested notification most likely refers to."
  (when (and runs target-iso)
    (let ((target (float-time (date-to-time target-iso)))
          (best nil)
          (best-dist most-positive-fixnum))
      (dolist (run runs)
        (let ((upd (cdr (assq 'updated_at run))))
          (when upd
            (let ((dist (abs (- (float-time (date-to-time upd)) target))))
              (when (< dist best-dist)
                (setq best run best-dist dist))))))
      best)))

(defun shipit--workflow-approval-notifications-by-repo ()
  "Return hash of repo → list of (key . activity) for approval-requested workflows."
  (let ((by-repo (make-hash-table :test 'equal)))
    (maphash
     (lambda (key activity)
       (when (and (equal (cdr (assq 'type activity)) "workflow")
                  (equal (cdr (assq 'reason activity)) "approval_requested"))
         (let ((repo (cdr (assq 'repo activity))))
           (when (and repo (stringp repo))
             (push (cons key activity) (gethash repo by-repo))))))
     shipit--notification-pr-activities)
    by-repo))

(defun shipit--set-activity-browse-url (activity url)
  "Destructively update ACTIVITY's browse-url cell to URL."
  (let ((cell (assq 'browse-url activity)))
    (when cell (setcdr cell url))))

(defun shipit--enrich-workflow-one-repo (repo entries runs)
  "Apply waiting-run ENRICHMENT to REPO's ENTRIES using fetched RUNS.
If RUNS is non-empty, update each entry's browse-url to the closest
waiting run.  If RUNS is empty and the auto-mark defcustom is set,
mark each notification thread read and drop it from the activities
hash."
  (if runs
      (dolist (entry entries)
        (let* ((activity (cdr entry))
               (target (cdr (assq 'updated-at activity)))
               (run (or (shipit--closest-run-by-time runs target) (car runs)))
               (url (and run (cdr (assq 'html_url run)))))
          (when url
            (shipit--set-activity-browse-url activity url))))
    (when shipit-notifications-auto-mark-resolved-approvals
      (dolist (entry entries)
        (let* ((activity (cdr entry))
               (notif (cdr (assq 'notification activity)))
               (thread-id (cdr (assq 'id notif))))
          (when thread-id
            (shipit--debug-log "Auto-marking resolved approval: %s %s" repo thread-id)
            (shipit--dispatch-mark-thread-read repo thread-id)
            (remhash (car entry) shipit--notification-pr-activities)))))))

(defun shipit--enrich-workflow-notifications ()
  "Post-process approval-requested WorkflowRun notifications.
For each repo that has such notifications, query the waiting runs
and either (a) enrich the activities with the specific run URL or
(b) auto-mark them read if the run is no longer waiting and the
user opted into `shipit-notifications-auto-mark-resolved-approvals'.
Re-renders the notifications buffer after each repo's enrichment
so URL updates surface as soon as they arrive."
  (let ((by-repo (shipit--workflow-approval-notifications-by-repo)))
    (maphash
     (lambda (repo entries)
       (shipit--api-request
        (format "/repos/%s/actions/runs" repo)
        (list (list "status" "waiting") (list "per_page" "30"))
        (lambda (data)
          (let ((runs (when data (append (cdr (assq 'workflow_runs data)) nil))))
            (shipit--enrich-workflow-one-repo repo entries runs)
            (shipit--rerender-notifications-buffer-if-visible)))))
     by-repo)))

(defun shipit--notification-browse-url (notification)
  "Return a browse URL for NOTIFICATION, or nil.
Derives a usable HTML URL for subject types that don't open in a
shipit buffer (Release, CheckSuite, Commit, Alert, …) so that
pressing RET in the notifications buffer still opens something
sensible in the browser."
  (let* ((repo-info (cdr (assq 'repository notification)))
         (repo-full (cdr (assq 'full_name repo-info)))
         (repo-html (or (cdr (assq 'html_url repo-info))
                        (and repo-full (format "https://github.com/%s" repo-full))))
         (subject (cdr (assq 'subject notification)))
         (subj-type (cdr (assq 'type subject)))
         (subj-url (cdr (assq 'url subject))))
    (pcase subj-type
      ("Release"
       (and repo-html (concat repo-html "/releases")))
      ("CheckSuite"
       (and repo-html (concat repo-html "/actions")))
      ("WorkflowRun"
       (or (and subj-url repo-html
                (string-match "/runs/\\([0-9]+\\)" subj-url)
                (concat repo-html "/actions/runs/" (match-string 1 subj-url)))
           (and repo-html (concat repo-html "/actions"))))
      ("Commit"
       (or (and subj-url repo-html
                (string-match "/commits/\\([0-9a-f]+\\)" subj-url)
                (concat repo-html "/commit/" (match-string 1 subj-url)))
           repo-html))
      ("RepositoryVulnerabilityAlert"
       (and repo-html (concat repo-html "/security")))
      ("RepositoryAdvisory"
       (and repo-html (concat repo-html "/security/advisories")))
      ("RepositoryInvitation"
       (and repo-html (concat repo-html "/invitations")))
      (_ repo-html))))

;;; Alert Toggle Functions (must be defined before transient menu)

(defun shipit--alert-toggle-desc (label reason)
  "Return toggle description for LABEL and REASON."
  (format "[%s] %s alerts"
          (if (memq reason shipit-notification-alert-reasons) "x" " ")
          label))

(defun shipit--toggle-alert-reason (reason)
  "Toggle REASON in alert reasons list."
  (if (memq reason shipit-notification-alert-reasons)
      (setq shipit-notification-alert-reasons
            (delq reason shipit-notification-alert-reasons))
    (push reason shipit-notification-alert-reasons))
  (message "%s alerts %s"
           (capitalize (symbol-name reason))
           (if (memq reason shipit-notification-alert-reasons) "enabled" "disabled")))

(defun shipit--toggle-alert-mention ()
  "Toggle mention alerts."
  (interactive)
  (shipit--toggle-alert-reason 'mention))

(defun shipit--toggle-alert-review-requested ()
  "Toggle review_requested alerts."
  (interactive)
  (shipit--toggle-alert-reason 'review_requested))

(defun shipit--toggle-alert-assign ()
  "Toggle assign alerts."
  (interactive)
  (shipit--toggle-alert-reason 'assign))

(defun shipit--toggle-alert-comment ()
  "Toggle comment alerts."
  (interactive)
  (shipit--toggle-alert-reason 'comment))

(defun shipit--set-alert-backend ()
  "Set the alert backend."
  (interactive)
  (let ((choice (completing-read "Alert backend: "
                                 '("message" "alert" "dbus" "disabled") nil t)))
    (setq shipit-notification-alert-backend
          (pcase choice
            ("message" 'message)
            ("dbus" 'dbus)
            ("alert" 'alert)
            ("disabled" nil)))
    (message "Alert backend set to: %s" choice)))

;;; Transient Menu

;;;###autoload
(when (fboundp 'transient-define-prefix)
  (transient-define-prefix shipit-notifications-menu ()
    "Configure and manage GitHub notifications.
Layout: [repo] [#pr] [title...]               [reason] [time]"
    :man-page "shipit-notifications"
    ["Notification Settings"
     ("f" "Set poll frequency" shipit--set-poll-frequency)
     ("i" "Toggle visual indicators" shipit--toggle-indicators)
     ("s" "Set notification scope" shipit--set-scope)
     ("e" "Enable/disable notifications" shipit--toggle-notifications)]
    ["Alert Rules"
     ("am" shipit--toggle-alert-mention
      :description (lambda () (shipit--alert-toggle-desc "Mention" 'mention)))
     ("ar" shipit--toggle-alert-review-requested
      :description (lambda () (shipit--alert-toggle-desc "Review requested" 'review_requested)))
     ("aa" shipit--toggle-alert-assign
      :description (lambda () (shipit--alert-toggle-desc "Assign" 'assign)))
     ("ac" shipit--toggle-alert-comment
      :description (lambda () (shipit--alert-toggle-desc "Comment" 'comment)))
     ("ab" "Set alert backend" shipit--set-alert-backend)]
    ["Display Formatting"
     ("w" "Adjust column widths (repo, pr, title, reason)" shipit--adjust-column-widths)
     ("r" "Set reason column width only" shipit--set-reason-width)
     ("x" "Adjust column spacing" shipit--adjust-column-spacing)]
    ["Actions"
     ("p" "Poll now" shipit--poll-notifications-now)
     ("c" "Clear notifications" shipit--clear-notifications)
     ("v" "View all notifications" shipit--view-notifications)
     ("q" "Quit" transient-quit-one)]))

;;; Interactive Functions

(defun shipit--set-poll-frequency ()
  "Set notification polling frequency."
  (interactive)
  (let ((new-frequency (read-number "Poll frequency (seconds): " shipit-notifications-poll-frequency)))
    (setq shipit-notifications-poll-frequency new-frequency)
    (message "Notification polling set to %d seconds" new-frequency)
    (when shipit-notifications-enabled
      (shipit--restart-notifications-polling))))

(defun shipit--toggle-indicators ()
  "Toggle visual notification indicators."
  (interactive)
  (let ((choices '(("modeline" . modeline)
                   ("section" . section)
                   ("both" . both)
                   ("none" . none))))
    (let ((choice (completing-read "Visual indicator: " (mapcar #'car choices) nil t)))
      (setq shipit-notifications-visual-indicator (cdr (assoc choice choices)))
      (message "Visual indicators set to: %s" choice))))

(defun shipit--set-scope ()
  "Set notification monitoring scope."
  (interactive)
  (let ((choices '(("all" . all)
                   ("participating" . participating)
                   ("unread" . unread)
                   ("unread-participating" . unread-participating))))
    (let ((choice (completing-read "Notification scope: " (mapcar #'car choices) nil t)))
      (setq shipit-notifications-scope (cdr (assoc choice choices)))
      (message "Notification scope set to: %s" choice))))


(defun shipit--toggle-notifications ()
  "Enable or disable notifications polling."
  (interactive)
  (setq shipit-notifications-enabled (not shipit-notifications-enabled))
  (if shipit-notifications-enabled
      (progn
        (shipit--start-notifications-polling)
        (message "Notifications enabled"))
    (progn
      (shipit--stop-notifications-polling)
      (shipit--clear-modeline-indicator)
      (message "Notifications disabled"))))

(defun shipit--poll-notifications-now ()
  "Poll notifications immediately, bypassing cache to get fresh data."
  (interactive)
  (message "Polling notifications (bypassing cache)...")
  (shipit--check-notifications-background-async t)
  (message "Notifications updated with fresh data"))

(defun shipit--clear-notifications ()
  "Clear all cached notification data."
  (interactive)
  (setq shipit--notification-data nil
        shipit--notification-pr-activities (make-hash-table :test 'equal)
        shipit--last-notification-count 0)
  (message "Notifications cleared"))

(defun shipit--view-notifications ()
  "View all current notifications in a dedicated buffer."
  (interactive)
  (require 'shipit-notifications-buffer)
  (let ((buf (shipit-notifications-buffer-create)))
    (shipit-notifications-buffer-refresh)
    (pop-to-buffer buf)))

(defun shipit--view-notifications-filtered (filter-text)
  "View notifications filtered by FILTER-TEXT."
  (require 'shipit-notifications-buffer)
  (let ((buf (shipit-notifications-buffer-create)))
    (with-current-buffer buf
      (setq shipit-notifications-buffer--filter-text filter-text))
    (shipit-notifications-buffer-refresh)
    (pop-to-buffer buf)))

(defun shipit--adjust-column-widths ()
  "Adjust notification column widths interactively."
  (interactive)
  (let ((current-widths shipit-notifications-column-widths)
        (columns '(("Repository" . repo) ("PR Number" . pr) ("Title" . title) ("Reason" . reason))))
    (dolist (col columns)
      (let* ((name (car col))
             (key (cdr col))
             (current (cdr (assq key current-widths)))
             (new-width (read-number (format "%s column width (current: %d): " name current) current)))
        (setf (alist-get key shipit-notifications-column-widths) new-width)))
    (message "Column widths updated. Time column width is automatic (2h, 3d, 1w format).")
  ))

(defun shipit--adjust-column-spacing ()
  "Adjust spacing between notification columns."
  (interactive)
  (let ((new-spacing (read-number (format "Column spacing (current: %d): "
                                          shipit-notifications-column-spacing)
                                  shipit-notifications-column-spacing)))
    (setq shipit-notifications-column-spacing new-spacing)
    (message "Column spacing set to %d spaces. Refresh notifications to see changes." new-spacing)
  ))

(defun shipit--set-reason-width ()
  "Set the width of the reason column."
  (interactive)
  (let ((current-width (cdr (assq 'reason shipit-notifications-column-widths)))
        (new-width nil))
    (setq new-width (read-number (format "Reason column width (current: %d, common reasons: mention, comment, review_requested): " current-width) current-width))
    (setf (alist-get 'reason shipit-notifications-column-widths) new-width)
    (message "Reason column width set to %d. Refresh notifications to see changes." new-width)))

(defun shipit--update-column-widths-config (&optional silent)
  "Update column widths configuration to new format if needed.
If SILENT is non-nil, don't show message (for automatic updates)."
  (interactive)
  (let ((updated nil))
    ;; Ensure reason column exists in configuration
    (unless (assq 'reason shipit-notifications-column-widths)
      (setf (alist-get 'reason shipit-notifications-column-widths) 20)
      (setq updated t))
    ;; Remove old unread column if it exists
    (when (assq 'unread shipit-notifications-column-widths)
      (setq shipit-notifications-column-widths
            (delq (assq 'unread shipit-notifications-column-widths)
                  shipit-notifications-column-widths))
      (setq updated t))
    (when (and updated (not silent))
      (message "Column widths configuration updated: %S" shipit-notifications-column-widths))))

;;;###autoload
(defun shipit-check-modeline-setup ()
  "Check if modeline integration is properly configured."
  (interactive)
  (message "Shipit Modeline Setup Check:")
  (message "  shipit-notifications-mode active: %s" (if (bound-and-true-p shipit-notifications-mode) "✓ YES" "✗ NO"))
  (message "  notifications enabled: %s" (if (bound-and-true-p shipit-notifications-enabled) "✓ YES" "✗ NO"))
  (message "  notifications initialized: %s" (if (bound-and-true-p shipit--notifications-timer) "✓ YES" "✗ NO"))
  (message "  visual indicator setting: %s" (if (bound-and-true-p shipit-notifications-visual-indicator)
                                                (format "✓ %s" shipit-notifications-visual-indicator) "✗ NOT SET"))
  (message "  notification count: %s" (if (bound-and-true-p shipit--notification-count)
                                          (format "%d" shipit--notification-count) "0"))
  (message "  modeline string: %s" (if (bound-and-true-p shipit--modeline-string)
                                       (format "✓ '%s'" shipit--modeline-string) "✗ NONE"))
  (message "  github token set: %s" (if (shipit--github-token) "✓ YES" "✗ NO")))

;;;###autoload
(defun shipit-test-modeline-indicator ()
  "Test the modeline indicator with fake notification count."
  (interactive)
  (let ((test-count (read-number "Test notification count: " 3)))
    (shipit--update-modeline-indicator test-count)
    (message "Modeline indicator set to %d notifications. Check your modeline!" test-count)
    (message "Run M-x shipit--clear-modeline-indicator to clear the test.")))

;;;###autoload
(defun shipit-debug-modeline ()
  "Debug modeline integration issues."
  (interactive)
  (message "=== Shipit Modeline Debug ===")
  (message "shipit-notifications-mode active: %s" shipit-notifications-mode)
  (message "shipit--mode-line-format result: '%s'" (shipit--mode-line-format))
  (message "shipit--modeline-string: %s" (if shipit--modeline-string
                                             (format "'%s'" shipit--modeline-string) "nil"))
  (message "minor-mode-alist entry: %s" (assq 'shipit-notifications-mode minor-mode-alist))
  (message "mode-line-format includes minor-mode-alist: %s" (member 'minor-mode-alist mode-line-format))
  (message "Current buffer mode-line-format: %S" (truncate-string-to-width (format "%S" mode-line-format) 200)))

;;; Core Notification Functions

;;;###autoload
(defun shipit-notifications-init ()
  "Initialize the notification system."
  (interactive)
  ;; CRITICAL: Set system-time-locale globally to ensure all HTTP date headers
  ;; use ASCII characters. This prevents "Multibyte text in HTTP request" errors
  ;; when url.el constructs If-Modified-Since headers with non-English locales.
  (setq system-time-locale "C")
  (unless shipit--notification-pr-activities
    (setq shipit--notification-pr-activities (make-hash-table :test 'equal)))
  (when shipit-notifications-enabled
    ;; Set up modeline integration
    (when (eq shipit-notifications-visual-indicator 'modeline)
      ;; When doom-modeline is active, its shipit-notifications segment
      ;; handles display directly (preserving local-map for click handling).
      ;; Only add to global-mode-string for standard mode-line setups.
      (unless (bound-and-true-p doom-modeline-mode)
        (unless global-mode-string
          (setq global-mode-string '("")))
        (unless (member 'shipit--modeline-string global-mode-string)
          (add-to-list 'global-mode-string 'shipit--modeline-string t))))
    ;; Start polling
    (shipit--start-notifications-polling)
    ;; Perform immediate first poll to populate notifications on startup
    (run-with-timer 1 nil (lambda () (shipit--check-notifications-background-async t)))
    (shipit--debug-log "Notifications system initialized with immediate first poll")))

(defun shipit--start-notifications-polling ()
  "Start background notifications polling respecting GitHub's X-Poll-Interval.
Now uses asynchronous requests to prevent Emacs stuttering."
  (when shipit--notifications-timer
    (cancel-timer shipit--notifications-timer))
  (when shipit-notifications-enabled
    ;; Use GitHub's X-Poll-Interval if available, otherwise use our configured frequency
    (let ((poll-interval (or shipit--notifications-poll-interval
                             shipit-notifications-poll-frequency)))
      (when (and shipit--notifications-poll-interval
                 (< shipit--notifications-poll-interval shipit-notifications-poll-frequency))
        (shipit--debug-log "⚠️ GitHub X-Poll-Interval (%ds) is less than configured frequency (%ds). Using GitHub's interval."
                           shipit--notifications-poll-interval shipit-notifications-poll-frequency))
      (setq shipit--notifications-timer
            (run-with-timer 0 poll-interval (lambda () (shipit--check-notifications-background-async t))))
      (shipit--debug-log "Started ASYNC notifications polling every %d seconds (GitHub X-Poll-Interval: %s)"
                         poll-interval (or shipit--notifications-poll-interval "not set")))))

(defun shipit--stop-notifications-polling ()
  "Stop background notifications polling."
  (when shipit--notifications-timer
    (cancel-timer shipit--notifications-timer)
    (setq shipit--notifications-timer nil)
    (shipit--debug-log "Stopped notifications polling")))

(defun shipit--restart-notifications-polling ()
  "Restart notifications polling with current settings."
  (shipit--stop-notifications-polling)
  (shipit--start-notifications-polling))

(defun shipit--pause-notifications-polling ()
  "Pause notifications polling (e.g., during PR load).
Uses nesting depth counter to handle nested pause/resume calls."
  (setq shipit--notifications-pause-depth (1+ shipit--notifications-pause-depth))
  (when (= shipit--notifications-pause-depth 1)  ; Only pause on first call
    (setq shipit--notifications-paused t)
    (shipit--stop-notifications-polling)
    (shipit--debug-log "⏸️  PAUSED notifications polling (depth: %d)" shipit--notifications-pause-depth)))

(defun shipit--resume-notifications-polling ()
  "Resume notifications polling (e.g., after PR load completes).
Uses nesting depth counter to handle nested pause/resume calls."
  (setq shipit--notifications-pause-depth (max 0 (1- shipit--notifications-pause-depth)))
  (when (= shipit--notifications-pause-depth 0)  ; Only resume when all pauses are cleared
    (setq shipit--notifications-paused nil)
    (when shipit-notifications-enabled
      (shipit--start-notifications-polling))
    (shipit--debug-log "▶️  RESUMED notifications polling")))

(defun shipit--notification-params-for-scope (scope)
  "Return the query-param alist that matches SCOPE.
SCOPE is one of `all', `participating', `unread',
`unread-participating'."
  (pcase scope
    ;; GitHub caps `/notifications' at per_page=50; asking for more is
    ;; silently truncated server-side and breaks the buffer's page math.
    ('all '((all . "true") (per_page . 50)))
    ('participating '((participating . "true") (per_page . 50)))
    ('unread '((per_page . 50)))
    ('unread-participating '((participating . "true") (per_page . 50)))
    (_ '((per_page . 50)))))

(defun shipit--notifications-count-from-response (headers json)
  "Return the total notification count from a per_page=1 probe response.
HEADERS is the alist produced by
`shipit--url-retrieve-async-with-headers'.  JSON is the parsed
body (a list of notifications, possibly empty).
With per_page=1, the last-page number returned in the pagination
Link header equals the total item count.  Falls back to body
length when no Link header is present (0 or 1 items).
HTTP header names are case-insensitive per RFC 9110, so the Link
lookup ignores case."
  (let* ((link-pair (and headers
                         (cl-find-if
                          (lambda (h)
                            (and (stringp (car h))
                                 (string-equal-ignore-case
                                  (car h) "Link")))
                          headers)))
         (last-page (and link-pair
                         (shipit--parse-link-header-last-page
                          (list (cons "Link" (cdr link-pair)))))))
    (or last-page (length (or json '())))))

(defun shipit--fetch-notifications-total-count-async (scope callback &optional repo before since)
  "Fetch the total notification count for SCOPE asynchronously.
Fires a lightweight per_page=1 request to /notifications, then
calls CALLBACK with an integer (or nil on failure).
REPO, when a non-empty OWNER/REPO string, scopes the probe to
that repository per-repo endpoint.  BEFORE and SINCE are ISO
timestamp strings that, when non-nil, time-window the probe so
the returned count matches the filtered view."
  (require 'shipit-http)
  (condition-case setup-err
      (let* ((scope-params (shipit--notification-params-for-scope scope))
             (probe-params (cons '(per_page . 1)
                                 (assq-delete-all 'per_page
                                                  (copy-sequence scope-params))))
             (probe-params (if (and before (stringp before) (> (length before) 0))
                               (cons (cons 'before before) probe-params)
                             probe-params))
             (probe-params (if (and since (stringp since) (> (length since) 0))
                               (cons (cons 'since since) probe-params)
                             probe-params))
             (qstr (mapconcat (lambda (p)
                                (format "%s=%s" (car p) (cdr p)))
                              probe-params "&"))
             (endpoint (if (and repo (stringp repo) (> (length repo) 0))
                           (format "/repos/%s/notifications" repo)
                         "/notifications"))
             (url (concat (or shipit-api-url "https://api.github.com")
                          endpoint "?" qstr))
             (token (ignore-errors (shipit--github-token)))
             (headers (delq nil
                            (list '("Accept" . "application/vnd.github+json")
                                  (when token
                                    (cons "Authorization"
                                          (format "Bearer %s" token)))))))
        (shipit--debug-log "PROBE: scope=%s url=%s token=%s"
                           scope url (if token "present" "MISSING"))
        (if (not token)
            (funcall callback nil)
          ;; Force a fresh 200 with the Link header on every probe.
          ;; GitHub's /notifications endpoint is Last-Modified-aware and
          ;; will return 304 (no body, routed to the error callback of
          ;; `shipit--url-retrieve-async-with-headers') as soon as url.el
          ;; has a cache entry — which it will on the 2nd+ call.
          ;; `url-automatic-caching nil' alone is not enough: url.el
          ;; still reads the cache file and derives an
          ;; `If-Modified-Since' from it.  Delete the cache file too.
          (when (fboundp 'url-cache-create-filename)
            (let ((cache-file (url-cache-create-filename url)))
              (when (and cache-file (file-exists-p cache-file))
                (ignore-errors (delete-file cache-file))
                (shipit--debug-log "PROBE: deleted stale url cache %s"
                                   cache-file))))
          (let ((url-automatic-caching nil))
            (shipit--url-retrieve-async-with-headers
             url "GET" headers nil
             (lambda (json response-headers)
               (let ((link (cdr (assoc "Link" response-headers)))
                     (count (shipit--notifications-count-from-response
                             response-headers json)))
                 (shipit--debug-log "PROBE: response Link=%S -> count=%S"
                                    link count)
                 (funcall callback count)))
             (lambda (err)
               (shipit--debug-log "PROBE: request failed: %s" err)
               (funcall callback nil))))))
    (error
     (shipit--debug-log "PROBE: setup failed: %s"
                        (error-message-string setup-err))
     (funcall callback nil))))

(defun shipit--check-notifications-background (&optional force-fresh scope-override max-pages repo start-page before since)
  "Check GitHub notifications in background using ETag caching with pagination.
FORCE-FRESH bypasses the ETag cache when non-nil.
SCOPE-OVERRIDE replaces `shipit-notifications-scope' for this call.
MAX-PAGES caps the number of pages fetched (default 10).
REPO, a non-empty OWNER/REPO, targets the per-repo endpoint.
START-PAGE is the first page to request (default 1).
BEFORE and SINCE are ISO-8601 timestamp strings passed as the
GitHub query parameters of the same name when non-nil; use them
to time-travel past GitHub's default 2-week window."
  (condition-case err
      (let* ((scope (or scope-override shipit-notifications-scope))
             (base-params (shipit--notification-params-for-scope scope))
             (params (if (and repo (stringp repo) (> (length repo) 0))
                         (cons (cons :repo repo) base-params)
                       base-params))
             (params (if (and before (stringp before) (> (length before) 0))
                         (cons (cons 'before before) params)
                       params))
             (params (if (and since (stringp since) (> (length since) 0))
                         (cons (cons 'since since) params)
                       params))
             (all-notifications
              (shipit--fetch-all-notifications params nil force-fresh max-pages start-page))
             (shipit--processing-all-scope (eq scope 'all)))
        (shipit--process-notifications all-notifications)
        ;; Schedule backend poll on next idle tick rather than running it
        ;; inline — backend fetches (Jira, RSS, GitLab) are sync and block
        ;; the buffer refresh by 2-3s.  The user already sees the GitHub
        ;; data and the backends merge in shortly after.
        (when (featurep 'shipit-issue-backends)
          (run-with-idle-timer 0 nil #'shipit--poll-backend-notifications)))
    (error
     (shipit--debug-log "Background notifications check failed: %s" (error-message-string err)))))

(defun shipit--fetch-all-notifications (params &optional since-watermark force-fresh max-pages start-page)
  "Fetch notifications following GitHub's protocol.
Uses Last-Modified headers and respects X-Poll-Interval.
If SINCE-WATERMARK is provided, filters to only notifications after that timestamp.
If FORCE-FRESH is non-nil, bypasses ETag caching.
MAX-PAGES caps the number of pages fetched (defaults to 10).
START-PAGE is the first page to request (defaults to 1); combined
with MAX-PAGES=1 this fetches a single specific page — used by
the notifications buffer windowed pagination."
  (let* ((resolved (shipit-pr--resolve-for-repo
                    (or (shipit--get-repo-from-remote) "")))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-notifications))
         (all-notifications '())
         (first-page (or start-page 1))
         (page first-page)
         (has-more t)
         (page-cap (+ first-page (1- (or max-pages 10)))))

    (while (and has-more (<= page page-cap))
      (let* ((page-params (append params `((page . ,page))))
             (page-notifications
              (if fetch-fn
                  ;; Dispatch through PR backend
                  (funcall fetch-fn config page-params force-fresh)
                ;; Fallback: direct call (shouldn't happen but be safe)
                (let ((result (shipit-gh-etag-get-json-with-refresh-cache
                               "/notifications" page-params shipit-github-token force-fresh)))
                  (when result (plist-get result :json))))))

        (shipit--debug-log "Page %d: result=%s json-count=%d"
                           page (not (null page-notifications))
                           (if page-notifications (length page-notifications) 0))

        (if (and page-notifications (> (length page-notifications) 0))
            (progn
              (setq all-notifications (append all-notifications page-notifications))
              (setq page (1+ page))
              (shipit--debug-log "Fetched page %d: %d notifications" (1- page) (length page-notifications))
              ;; Always try next page - GitHub might return less than per_page even when more exist
              ;; We'll stop when we get an empty page or hit the safety limit
              )
          (setq has-more nil))))

    (shipit--debug-log "Total notifications fetched: %d across %d pages" (length all-notifications) (1- page))
    all-notifications))

(defun shipit--fetch-notifications-with-headers (endpoint params &optional since-watermark)
  "Fetch notifications using If-Modified-Since following GitHub protocol.
Returns plist with :json, :status, :headers."
  (unless (shipit--github-token)
    (error "GitHub token not found.  Set `shipit-github-token' or add github.com to auth-source"))

  (let* ((url (concat shipit-api-url endpoint))
         (query-params (when params
                         (mapconcat (lambda (param)
                                      (format "%s=%s" (car param) (cdr param)))
                                    params "&")))
         (full-url (if query-params (concat url "?" query-params) url))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         ;; Add If-Modified-Since if we have a previous Last-Modified
         (headers (if shipit--notifications-last-modified
                      (cons `("If-Modified-Since" . ,shipit--notifications-last-modified) headers)
                    headers))
         (headers (remove nil headers)))

    (shipit--debug-log "🔍 Fetching notifications with headers: %s" full-url)
    (when shipit--notifications-last-modified
      (shipit--debug-log "   If-Modified-Since: %s" shipit--notifications-last-modified))

    (condition-case err
        (let* ((result (shipit--url-retrieve-with-headers full-url "GET" headers nil))
               (json-data (plist-get result :json))
               (status-code (plist-get result :status))
               (response-headers (plist-get result :headers)))

          (shipit--debug-log "   Response status: %s" status-code)
          (when response-headers
            (shipit--debug-log "   Response headers: %s" (mapcar #'car response-headers)))

          `(:json ,json-data :status ,status-code :headers ,response-headers))
      (error
       (shipit--debug-log "Notifications fetch error: %s" (error-message-string err))
       nil))))

(defvar shipit--rerender-in-progress nil
  "Set non-nil while a notifications-buffer rerender is running.
Prevents async callbacks (PR/workflow/backend enrichment) from
re-entering the renderer mid-render.  Profilers caught O(N)
recursion: every per-notification `magit-section-hide' yielded
to `accept-process-output', a queued enrichment callback fired,
called `--rerender-notifications-buffer-if-visible', which
re-rendered the entire buffer mid-render.")

(defvar-local shipit--rerender-pending nil
  "Non-nil when a rerender was requested while one was in progress.
Drained at the end of the active rerender; coalesces a burst of
async callbacks into a single follow-up redraw.")

(defun shipit--rerender-notifications-buffer-if-visible ()
  "Re-render the shipit notifications buffer if it exists.
Used by deferred enrichment paths to surface freshly-resolved
draft/state/author icons and workflow run URLs without forcing
the user to press `g'.

Re-entry guard: if a rerender is already in progress (e.g. an
async enrichment callback fires while the buffer is rendering),
just mark the buffer as needing another render and return.  The
outer rerender drains the flag and runs once more — a single
follow-up regardless of how many callbacks landed."
  (let ((buf (and (boundp 'shipit-notifications-buffer-name)
                  (get-buffer shipit-notifications-buffer-name))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (cond
         (shipit--rerender-in-progress
          (setq shipit--rerender-pending t))
         (t
          (let ((shipit--rerender-in-progress t))
            (setq shipit--rerender-pending nil)
            (when (fboundp 'shipit-notifications-buffer--rerender)
              (shipit-notifications-buffer--rerender))
            (when shipit--rerender-pending
              (setq shipit--rerender-pending nil)
              (when (fboundp 'shipit-notifications-buffer--rerender)
                (shipit-notifications-buffer--rerender))))))))))

(defun shipit--process-notifications (notifications)
  "Process notifications and extract PR and Issue activity."
  (let ((activities (make-hash-table :test 'equal))
        (total-count 0)
        (unread-count 0)
        (mention-prs '())
        (mention-count 0)
        (skipped-locally-read 0))

    ;; Clean up old locally-marked-read entries (older than 2x poll interval)
    ;; Only expire GitHub notification-id entries (numeric-looking keys).
    ;; Backend entries (activity-keys like "repo:type:number") persist
    ;; since backends like RSS/Jira have no server-side read tracking.
    (let ((cutoff-time (- (float-time) (* 2 shipit-notifications-poll-frequency)))
          (keys-to-remove '()))
      (maphash (lambda (id timestamp)
                 (when (and (< timestamp cutoff-time)
                            (not (string-match-p ":" (format "%s" id))))
                   (push id keys-to-remove)))
               shipit--locally-marked-read-notifications)
      (dolist (id keys-to-remove)
        (remhash id shipit--locally-marked-read-notifications)))

    (shipit--debug-log "Processing %d total notifications (filtering %d locally marked read)"
                       (length notifications)
                       (hash-table-count shipit--locally-marked-read-notifications))

    (dolist (notification notifications)
      (let* ((notification-id (cdr (assq 'id notification)))
             (subject (cdr (assq 'subject notification)))
             (subject-type (cdr (assq 'type subject)))
             (subject-url (cdr (assq 'url subject)))
             (reason (cdr (assq 'reason notification)))
             (unread (cdr (assq 'unread notification))))

        ;; Skip notifications that we've locally marked as read
        (if (and unread (gethash notification-id shipit--locally-marked-read-notifications))
            (progn
              (shipit--debug-log "Skipping locally-marked-read notification: %s" notification-id)
              (setq skipped-locally-read (1+ skipped-locally-read)))

          ;; Debug: Log all notification types and reasons
          (shipit--debug-log "Notification type: %s, reason: %s, unread: %s" subject-type reason unread)

          ;; Process all supported notification subject types.  Types
          ;; without an intrinsic numeric id (e.g. Commit's SHA) fall
          ;; back to the thread id so the activity key is still unique.
          (let ((internal-type (shipit--notification-type-from-subject subject-type)))
            (when internal-type
              (let* ((repo-info (cdr (assq 'repository notification)))
                     (repo-full-name (cdr (assq 'full_name repo-info)))
                     (updated-at (cdr (assq 'updated_at notification)))
                     (extracted-number (shipit--extract-notification-number subject-url subject-type))
                     (thread-id-number (shipit--thread-id-as-number notification-id))
                     (number (or extracted-number thread-id-number))
                     (browse-url (shipit--notification-browse-url notification)))

                (if (and repo-full-name number)
                    (let* ((activity-key (shipit--notification-activity-key repo-full-name internal-type number))
                           (activity `((repo . ,repo-full-name)
                                       (number . ,number)
                                       (type . ,internal-type)
                                       (updated-at . ,updated-at)
                                       (subject . ,(cdr (assq 'title subject)))
                                       (reason . ,reason)
                                       (browse-url . ,browse-url)
                                       (notification . ,notification))))
                      (puthash activity-key activity activities)
                      (setq total-count (1+ total-count))
                      ;; GitHub returns `unread' as JSON true/false; with
                      ;; `json-read' that becomes `t' or `:json-false'.
                      ;; A bare `(when unread ...)' counts both as unread
                      ;; because `:json-false' is non-nil — explicitly
                      ;; reject it.
                      (when (and unread (not (eq unread :json-false)))
                        (setq unread-count (1+ unread-count)))

                      ;; Check if we should alert on this notification
                      (when (shipit--should-alert-p activity)
                        (shipit--queue-alert activity))

                      ;; Track mentions specifically
                      (when (string= reason "mention")
                        (push `((repo . ,repo-full-name)
                                (number . ,number)
                                (type . ,internal-type)
                                (subject . ,(cdr (assq 'title subject)))
                                (url . ,(if (string= internal-type "pr")
                                                     (shipit--browse-pr-url repo-full-name number)
                                                   (shipit--browse-issue-url repo-full-name number))))
                              mention-prs)
                        (setq mention-count (1+ mention-count))
                        (shipit--debug-log "Found mention in %s: %s #%s" internal-type repo-full-name number))

                      (shipit--debug-log "Added %s notification: %s #%s (reason: %s)" internal-type repo-full-name number reason))
                  (shipit--debug-log "Skipped notification: repo=%s number=%s url=%s"
                                     repo-full-name number subject-url))))))))

    ;; Merge GitHub notifications into the global table (preserving backend entries)
    (shipit--merge-github-notifications activities)
    ;; Apply auto-mark rules that don't depend on PR-state (type, reason,
    ;; title, repo) immediately after merge.  State-dependent rules will
    ;; be re-applied at the end of `shipit--apply-pr-enrichment'; the
    ;; mark-read API is idempotent so the duplicate fire is harmless.
    (shipit--auto-mark-rules-apply)
    ;; Enrichments are async — the GraphQL PR enrichment kicks off here and
    ;; rerenders on completion; workflow enrichment fires per-repo HTTP
    ;; requests that are also async and rerender on completion.
    (shipit--enrich-pr-notifications)
    (shipit--enrich-workflow-notifications)
    (setq shipit--mention-prs mention-prs)
    (setq shipit--mention-count mention-count)

    ;; Recompute the unread count AFTER auto-mark rules have run, so the
    ;; modeline reflects the post-auto-mark state immediately instead of
    ;; showing the pre-auto-mark count and only catching up on the next
    ;; poll.  Walks the global hash so backend entries (Jira, RSS, etc.)
    ;; count too — those don't pass through the github fetch loop above.
    (let ((post-auto-mark-unread 0))
      (when (and (boundp 'shipit--notification-pr-activities)
                 shipit--notification-pr-activities)
        (maphash
         (lambda (_k activity)
           (let* ((notif (cdr (assq 'notification activity)))
                  (notif-unread (cdr (assq 'unread notif)))
                  (notif-id (cdr (assq 'id notif))))
             (when (and notif-unread
                        (not (eq notif-unread :json-false))
                        (not (gethash notif-id
                                      shipit--locally-marked-read-notifications)))
               (setq post-auto-mark-unread (1+ post-auto-mark-unread)))))
         shipit--notification-pr-activities))
      ;; Skip the modeline update when this run was processing an `all'
      ;; scope payload — that fetch may have wiped out unread github
      ;; entries the user actually cares about while populating the
      ;; activities hash with read items.  The next unread-scope poll
      ;; refreshes accurately.
      (unless (and (boundp 'shipit--processing-all-scope)
                   shipit--processing-all-scope)
        (when (not (eq post-auto-mark-unread shipit--last-notification-count))
          (shipit--handle-new-notifications post-auto-mark-unread))
        (setq shipit--last-notification-count post-auto-mark-unread))

      (shipit--debug-log "Processed %d notifications (%d unread pre-mark, %d unread post-mark, %d mentions, scope=%s, all-scope=%s)"
                         total-count unread-count post-auto-mark-unread mention-count
                         (and (boundp 'shipit-notifications-scope) shipit-notifications-scope)
                         (and (boundp 'shipit--processing-all-scope)
                              shipit--processing-all-scope)))))

(defalias 'shipit--process-pr-notifications 'shipit--process-notifications
  "Backward compatibility alias.")

(defun shipit--enrich-pr-notifications ()
  "Enrich PR notifications with draft/state/author via GraphQL.
Fetches all PR metadata in a single request grouped by repo."
  (let ((pr-by-repo (make-hash-table :test 'equal)))
    ;; Collect PR notifications grouped by repo
    (maphash (lambda (_key activity)
               (when (string= (cdr (assq 'type activity)) "pr")
                 (let ((repo (cdr (assq 'repo activity)))
                       (number (cdr (assq 'number activity))))
                   (when (and repo number)
                     (push number (gethash repo pr-by-repo))))))
             shipit--notification-pr-activities)
    ;; Skip if no PR notifications
    (when (> (hash-table-count pr-by-repo) 0)
      (let ((query (shipit--build-pr-enrichment-query pr-by-repo)))
        (shipit--graphql-request
         query nil
         (lambda (response)
           (let ((data (and response (cdr (assq 'data response)))))
             (when data
               (shipit--apply-pr-enrichment data pr-by-repo)
               (shipit--auto-mark-rules-apply)
               (shipit--rerender-notifications-buffer-if-visible)))))))))

(defun shipit--build-pr-enrichment-query (pr-by-repo)
  "Build a GraphQL query to fetch PR metadata for all repos.
PR-BY-REPO is a hash table of repo-name → list of PR numbers."
  (let ((repo-queries '())
        (repo-idx 0))
    (maphash
     (lambda (repo numbers)
       (let* ((parts (split-string repo "/"))
              (owner (car parts))
              (name (cadr parts))
              (pr-fields (mapconcat
                          (lambda (num)
                            (format "p%d: pullRequest(number: %d) { isDraft state author { login avatarUrl } }"
                                    num num))
                          numbers "\n    ")))
         (push (format "r%d: repository(owner: \"%s\", name: \"%s\") {\n    %s\n  }"
                       repo-idx owner name pr-fields)
               repo-queries)
         (setq repo-idx (1+ repo-idx))))
     pr-by-repo)
    (format "{\n  %s\n}" (mapconcat #'identity (nreverse repo-queries) "\n  "))))

(defun shipit--apply-pr-enrichment (result pr-by-repo)
  "Apply GraphQL RESULT to enrich notification activities.
PR-BY-REPO maps repo names to PR number lists.
RESULT contains repo aliases (r0, r1...) with PR aliases (p123...)."
  (let ((repo-idx 0))
    (maphash
     (lambda (repo numbers)
       (let ((repo-data (cdr (assq (intern (format "r%d" repo-idx)) result))))
         (when repo-data
           (dolist (number numbers)
             (let* ((pr-data (cdr (assq (intern (format "p%d" number)) repo-data)))
                    (is-draft (eq (cdr (assq 'isDraft pr-data)) t))
                    (state (cdr (assq 'state pr-data)))
                    (author-obj (cdr (assq 'author pr-data)))
                    (author (when author-obj (cdr (assq 'login author-obj))))
                    (avatar-url (when author-obj (cdr (assq 'avatarUrl author-obj))))
                    (activity-key (shipit--notification-activity-key repo "pr" number))
                    (activity (gethash activity-key shipit--notification-pr-activities)))
               (when (and activity pr-data)
                 ;; Add enrichment data to the activity alist
                 (let ((enriched activity))
                   (push (cons 'draft is-draft) enriched)
                   (push (cons 'pr-state (downcase (or state ""))) enriched)
                   (push (cons 'pr-author author) enriched)
                   (push (cons 'pr-avatar-url avatar-url) enriched)
                   (puthash activity-key enriched shipit--notification-pr-activities)))))))
       (setq repo-idx (1+ repo-idx)))
     pr-by-repo)))

;;; Auto-mark-read rules

(defcustom shipit-notifications-auto-mark-read-rules nil
  "Rules that automatically mark matching notifications as read.
A list of plists; each plist combines one or more conditions that
must all match for the rule to fire.  Activities matching ANY rule
are marked as read after each fetch + enrichment pass.

Available condition keys:
  :state    one of `merged'/`closed'/`open'/`draft' as symbol or
            string.  PR-only — non-PR activities never match.
  :draft    t or nil — match the activity's `draft' flag.
  :type     activity type string (pr/issue/workflow/etc.).
  :reason   GitHub reason string (subscribed, mention, etc.).
  :title    regex tested against the activity's `subject'.
  :repo     OWNER/REPO slug, case-insensitive.
  :not      sub-rule plist; matches when the sub-rule does NOT match.
            Example: (:not (:title \"mythos\")) auto-marks anything
            whose title does not contain \"mythos\".  Composes with
            sibling conditions: (:state merged :not (:title \"mythos\"))
            auto-marks merged PRs whose title lacks \"mythos\".

Each rule's conditions are combined with AND; multiple rules act
as OR — match any rule and the notification is marked.  Each
refresh applies the rules after PR enrichment so state-based
rules see fresh GraphQL data.  Set to nil to disable."
  :type '(repeat (plist :key-type
                        (choice (const :state)
                                (const :draft)
                                (const :type)
                                (const :reason)
                                (const :title)
                                (const :repo)
                                (const :not))
                        :value-type sexp))
  :group 'shipit)

(defun shipit--auto-mark-state-condition-matches-p (val activity)
  "Return non-nil when ACTIVITY satisfies a `:state' VAL condition.
VAL may be a symbol or a string.  Activities without `pr-state'
never match.  `draft' = open + isDraft; `open' = open + not-draft;
`merged' / `closed' = direct match on `pr-state'."
  (let ((wanted (if (symbolp val) (symbol-name val) val))
        (state (cdr (assq 'pr-state activity)))
        (draft (cdr (assq 'draft activity))))
    (cond
     ((or (null state)
          (and (stringp state) (string-empty-p state))) nil)
     ((equal wanted "draft") (and (equal state "open") draft))
     ((equal wanted "open") (and (equal state "open") (not draft)))
     (t (equal state wanted)))))

(defun shipit--auto-mark-condition-matches-p (key val activity)
  "Return non-nil when ACTIVITY satisfies the single (KEY VAL) condition.
The special key `:not' takes a sub-rule plist as VAL and matches when
that sub-rule does NOT match — e.g. (:not (:title \"mythos\")) matches
activities whose title does not contain \"mythos\"."
  (pcase key
    (:state (shipit--auto-mark-state-condition-matches-p val activity))
    (:draft (eq (and (cdr (assq 'draft activity)) t) (and val t)))
    (:type (equal (cdr (assq 'type activity)) val))
    (:reason (equal (cdr (assq 'reason activity)) val))
    (:title (let ((subject (cdr (assq 'subject activity))))
              (and (stringp subject)
                   (stringp val)
                   (string-match-p val subject))))
    (:repo (let ((repo (cdr (assq 'repo activity))))
             (and (stringp repo)
                  (stringp val)
                  (string-equal-ignore-case repo val))))
    (:jira-component
     (let ((comps (cdr (assq 'jira-components activity))))
       (and comps
            (stringp val)
            (cl-some (lambda (c)
                       (and (stringp c) (string-equal-ignore-case c val)))
                     comps))))
    (:not (not (shipit--auto-mark-rule-matches-activity-p val activity)))
    (_ nil)))

(defun shipit--auto-mark-rule-matches-activity-p (rule activity)
  "Return non-nil when ACTIVITY satisfies every condition in RULE.
RULE is a plist of conditions; an empty rule matches everything
(no conditions to fail), which is intentional — users can use it
as a kill-switch when scoped behind another condition."
  (cl-loop for (key val) on rule by #'cddr
           always (shipit--auto-mark-condition-matches-p key val activity)))

(defun shipit--auto-mark-rules-apply ()
  "Mark every UNREAD activity matching any auto-mark rule as read.
Walks `shipit--notification-pr-activities', collects activities
that match any rule in `shipit-notifications-auto-mark-read-rules',
then marks each via `shipit--mark-notification-read'.  Returns the
number of activities marked.

Already-read items are skipped — re-marking them would be a no-op
on the server but their cache entry would still be removed, which
would hide them from the `all' scope view.  No-op when the rules
list is empty so adding the hook has zero cost for users who do
not configure it."
  (let ((rules shipit-notifications-auto-mark-read-rules)
        (count 0))
    (when (and rules
               (boundp 'shipit--notification-pr-activities)
               shipit--notification-pr-activities)
      (let ((to-mark '()))
        (maphash
         (lambda (_k activity)
           (let* ((notification (cdr (assq 'notification activity)))
                  (unread (if notification
                              ;; GitHub: `unread' is t or :json-false; only
                              ;; t means unread.
                              (let ((u (cdr (assq 'unread notification))))
                                (and u (not (eq u :json-false))))
                            ;; Backend activity (Jira, RSS, GitLab todos, ...)
                            ;; has no GitHub notification thread.  Presence
                            ;; in the hash means unread --
                            ;; `shipit--mark-notification-read' removes the
                            ;; entry on mark.
                            t)))
             (when (and unread
                        (cl-some (lambda (rule)
                                   (shipit--auto-mark-rule-matches-activity-p
                                    rule activity))
                                 rules))
               (push activity to-mark))))
         shipit--notification-pr-activities)
        (dolist (a to-mark)
          (let ((number (or (cdr (assq 'number a))
                            (cdr (assq 'pr-number a))))
                (repo (cdr (assq 'repo a)))
                (type (or (cdr (assq 'type a)) "pr")))
            (when (and number repo)
              (shipit--mark-notification-read number repo t type)
              (cl-incf count))))))
    count))

(defun shipit-notifications-apply-auto-mark-rules ()
  "Interactively apply `shipit-notifications-auto-mark-read-rules' now.
Same logic the post-enrichment hook fires automatically; useful
when you have just edited the rules and want them to take effect
without waiting for the next refresh."
  (interactive)
  (let ((n (shipit--auto-mark-rules-apply)))
    (message (if (zerop n)
                 "Auto-mark rules: nothing matched"
               (format "Auto-mark rules: marked %d notification%s as read"
                       n (if (= n 1) "" "s"))))))

(defun shipit--handle-new-notifications (count)
  "Handle new notifications - update visual indicators."
  (when (eq shipit-notifications-visual-indicator 'modeline)
    (shipit--update-modeline-indicator count)))

(defun shipit--update-modeline-indicator (count)
  "Update modeline with notification count, showing both mentions and total."
  (setq shipit--notification-count count)
  (setq shipit--modeline-string
        (when (and shipit-notifications-enabled (> count 0))
          (if (> shipit--mention-count 0)
              ;; Show both mention count (@) and total count (🔔)
              ;; Use emoji directly — SVG icons render poorly in mode-line
              (propertize (concat " @" (number-to-string shipit--mention-count) "🔔" (number-to-string count))
                          'font-lock-face 'error  ; More prominent when mentions exist
                          'help-echo (format "%d mentions, %d total notifications - click for mention actions"
                                             shipit--mention-count count)
                          
                          'local-map (let ((map (make-sparse-keymap)))
                                       (define-key map [mode-line mouse-1] #'shipit--mention-actions)
                                       map))
            ;; Standard notification display (no mentions)
            ;; Use emoji directly — SVG icons render poorly in mode-line
            (propertize (concat " 🔔" (number-to-string count))
                        'font-lock-face 'warning
                        'help-echo (format "%d notifications - click to view" count)
                        
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line mouse-1] #'shipit--view-notifications)
                                     map)))))
  (force-mode-line-update t))

(defun shipit--clear-modeline-indicator ()
  "Clear the modeline notification indicator."
  (setq shipit--modeline-string nil)
  (setq shipit--notification-count 0)
  (setq shipit--mention-count 0)
  (setq shipit--mention-prs nil)
  (force-mode-line-update t))


;;; Magit Section Definitions

(defun pr-notification (&rest _args)
  "Magit section identifier for individual PR notifications.")
(put 'pr-notification 'magit-section t)

;;; Notification Section for Magit Status

;;; Magit Integration

(defun shipit--maybe-insert-notifications-section ()
  "Insert notifications section if there are notifications to show.
This function is now a no-op - notifications are shown in dedicated buffer."
  ;; No longer inserts section - notifications available via C-c C-s N v
  nil)

;;; Helper Functions


(defun shipit--format-time-ago (timestamp)
  "Format TIMESTAMP as a fixed-width 'time ago' string like ' 2h', ' 3d', ' 1w'.
Always returns a 3-character string for consistent column alignment."
  (if (not timestamp)
      " ? "
    (let* ((now (float-time))
           (then (float-time (date-to-time timestamp)))
           (diff (- now then))
           (minutes (/ diff 60))
           (hours (/ minutes 60))
           (days (/ hours 24))
           (weeks (/ days 7)))
      (cond
       ((< minutes 1) "now")
       ((< minutes 60) (format "%2dm" (floor minutes)))
       ((< hours 24) (format "%2dh" (floor hours)))
       ((< days 7) (format "%2dd" (floor days)))
       (t (format "%2dw" (floor weeks)))))))

;;; Minor Mode for Global Mode String Integration

;;;###autoload
(define-minor-mode shipit-notifications-mode
  "Minor mode for shipit GitHub integration with modeline notifications."
  :global t
  :group 'shipit
  (unless global-mode-string
    (setq global-mode-string '("")))
  (if (not shipit-notifications-mode)
      (progn
        (setq global-mode-string
              (delq 'shipit--modeline-string global-mode-string))
        (shipit--stop-notifications-polling))
    (when (eq shipit-notifications-visual-indicator 'modeline)
      (add-to-list 'global-mode-string 'shipit--modeline-string t))
    (when shipit-notifications-enabled
      (shipit-notifications-init))))

;;; Auto-initialization

;; NOTE: Auto-initialization has been removed because it causes issues
;; with function definition order. Call (shipit-notifications-init) explicitly
;; in your config instead.


;;; Region-based Multi-select

(defun shipit--get-notifications-in-region ()
  "Get list of notifications within active region.
Each element is (NUMBER REPO TYPE)."
  (when (region-active-p)
    (let ((region-start (region-beginning))
          (region-end (region-end))
          (notifications '()))
      (save-excursion
        (goto-char region-start)
        (while (< (point) region-end)
          (let ((pr-number (get-text-property (point) 'shipit-pr-number))
                (repo (get-text-property (point) 'shipit-repo))
                (type (or (get-text-property (point) 'shipit-notification-type) "pr")))
            (when (and pr-number repo)
              (push (list pr-number repo type) notifications)))
          (forward-line 1)))
      (delete-dups notifications))))

(defun shipit--mark-region-notifications-read ()
  "Mark all notifications in selected region as read.
Each notification requires a separate API call as bulk operations
for specific notification subsets are not universally supported."
  (let* ((notifications (shipit--get-notifications-in-region))
         (count (length notifications)))
    (when (> count 0)
      (if (y-or-n-p (format "Mark %d selected notifications as read? " count))
          (progn
            (message "Marking %d selected notifications as read..." count)
            (deactivate-mark)
            ;; Process individually — selective bulk operations not universally supported
            (shipit--mark-notifications-batch-async notifications))
        (message "Batch mark cancelled")))))

(defun shipit--mark-notifications-batch-async (notifications)
  "Mark notifications as read asynchronously with progress updates."
  (let ((total-count (length notifications))
        (marked-count 0)
        (failed-count 0)
        (current-index 0))

    ;; Process notifications with timer to avoid blocking
    (cl-labels ((process-next ()
                  (if (< current-index total-count)
                      (let* ((notification (nth current-index notifications))
                             (pr-number (car notification))
                             (repo (cadr notification))
                             (type (or (caddr notification) "pr")))
                        (setq current-index (1+ current-index))

                        ;; Show progress
                        (message "Marking notifications as read... %d/%d"
                                 current-index total-count)

                        ;; Mark this notification (no individual refresh)
                        (condition-case err
                            (progn
                              (shipit--debug-log "Attempting to mark %s %s as read..." type pr-number)
                              (shipit--mark-notification-read pr-number repo t type)
                              (setq marked-count (1+ marked-count))
                              (shipit--debug-log "Successfully marked %s %s as read" type pr-number))
                          (error
                           (setq failed-count (1+ failed-count))
                           (shipit--debug-log "Failed to mark %s %s as read: %s"
                                              type pr-number (error-message-string err))
                           (message "Failed to mark %s %s as read: %s" type pr-number (error-message-string err))))

                        ;; Schedule next processing with small delay
                        (run-with-timer 0.1 nil #'process-next))

                    ;; All done - show final results
                    (let ((success-msg (format "✅ Marked %d notifications as read" marked-count))
                          (failure-msg (if (> failed-count 0)
                                           (format ", %d failed" failed-count)
                                         "")))
                      (message "%s%s" success-msg failure-msg)
                      ;; Clear notifications from ETag cache so next poll gets fresh data
                      (shipit--clear-notifications-cache)
                      ;; Immediately update the display to remove marked notifications
                      (shipit--update-notifications-display-immediately)))
      ;; Start the async processing
      (process-next))))


(defun shipit--update-notifications-display-immediately ()
  "Immediately update the notifications section display.
Removes marked notifications from the display without full buffer refresh."
  (shipit--debug-log "🔄 UPDATE-NOTIF: Starting update")
  (let ((buf (car (cl-remove-if-not
                   (lambda (b) (string-match-p "\\*shipit: " (buffer-name b)))
                   (buffer-list)))))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (lines-to-delete '()))
          (save-excursion
            ;; Find notification lines to delete
            (goto-char (point-min))
            (while (< (point) (point-max))
              (let ((next-prop (next-single-property-change (point) 'shipit-notification-content)))
                (goto-char (or next-prop (point-max))))
              (when (get-text-property (point) 'shipit-notification-content)
                (let* ((number (get-text-property (point) 'shipit-pr-number))
                       (repo (get-text-property (point) 'shipit-repo))
                       (type (or (get-text-property (point) 'shipit-notification-type) "pr"))
                       (key (shipit--notification-activity-key repo type number)))
                  (unless (gethash key shipit--notification-pr-activities)
                    (push (cons (line-beginning-position) (1+ (line-end-position)))
                          lines-to-delete)))))

            ;; Update count
            (goto-char (point-min))
            (when (get-text-property (point) 'notification-count)
              (let ((new-count (hash-table-count shipit--notification-pr-activities)))
                (when (re-search-forward "([0-9]+" nil t)
                  (replace-match (format "(%d" new-count) nil nil))))

            ;; Delete lines in reverse order
            (dolist (del (reverse lines-to-delete))
              (delete-region (car del) (cdr del)))))
          (shipit--debug-log "✅ UPDATE-NOTIF: Complete")))))))


(defun shipit--refresh-notifications-force-fresh ()
  "Force refresh notifications by bypassing ETag cache.
Used after marking notifications as read to get updated state from GitHub."
  (shipit--debug-log "🔄 Force refreshing notifications (bypassing cache)")
  (condition-case err
      (let* ((params (shipit--notification-params-for-scope shipit-notifications-scope))
             ;; Force fresh fetch to bypass ETag cache
             (all-notifications (shipit--fetch-all-notifications params nil t)))
        (shipit--process-notifications all-notifications)
        ;; Poll backend notifications if backends are configured
        (when (featurep 'shipit-issue-backends)
          (shipit--poll-backend-notifications)))
    (error
     (shipit--debug-log "Force refresh failed: %s" (error-message-string err)))))


(defun shipit--clear-notifications-cache ()
  "Clear ETag cache for notifications endpoints to force fresh requests.
This ensures that after marking notifications as read, the next poll
gets the updated state from GitHub instead of serving stale cached data."
  (when (and (boundp 'shipit-gh-etag--persistent-cache)
             (hash-table-p shipit-gh-etag--persistent-cache))
    (let ((cleared-count 0))
      (maphash (lambda (key value)
                 (when (string-match-p "/notifications" key)
                   (remhash key shipit-gh-etag--persistent-cache)
                   (setq cleared-count (1+ cleared-count))))
               shipit-gh-etag--persistent-cache)
      (shipit--debug-log "🧹 Cleared %d notification cache entries" cleared-count))))


;;; Mention Actions

;;;###autoload
(defun shipit--mention-actions ()
  "Handle click actions on mention indicator in modeline."
  (interactive)
  (let ((actions '("View all notifications" "View mentions only")))
    (let ((choice (completing-read "Notification action: " actions nil t)))
      (cond
       ((string= choice "View all notifications")
        (shipit--view-notifications))
       ((string= choice "View mentions only")
        (shipit--view-notifications-filtered "mention"))
       (t (message "No action selected"))))))

(defun shipit--show-mention-list ()
  "Show list of all PRs where user is mentioned."
  (if (> (length shipit--mention-prs) 0)
      (with-current-buffer (get-buffer-create "*Shipit Mentions*")
        (read-only-mode -1)
        (erase-buffer)
        (insert (propertize "PR Mentions\n" 'font-lock-face 'header-line))
        (insert (propertize "==================\n\n" 'font-lock-face 'header-line))

        (dolist (mention shipit--mention-prs)
          (let* ((repo (cdr (assq 'repo mention)))
                 (pr-number (cdr (assq 'number mention)))
                 (subject (cdr (assq 'subject mention)))
                 (url (cdr (assq 'url mention)))
                 (line-start (point)))
            (insert (format "[%s %s] %s\n" repo pr-number subject))
            ;; Add clickable properties to the line
            (add-text-properties line-start (1- (point))
                                 `(shipit-mention-url ,url

                                                      help-echo ,(format "Click to open %s %s in browser" repo pr-number)
                                                      keymap ,(let ((map (make-sparse-keymap)))
                                                                (define-key map (kbd "RET")
                                                                            (lambda () (interactive) (browse-url ,url)))
                                                                (define-key map [mouse-1]
                                                                            (lambda () (interactive) (browse-url ,url)))
                                                                map)))))

        (insert "\n\nPress RET or click to open PR in browser\n")
        (read-only-mode 1)
        (goto-char (point-min))
        (switch-to-buffer-other-window (current-buffer)))
    (message "No mentions found")))

(defun shipit--open-first-mention ()
  "Open the first PR mention directly in browser."
  (if (> (length shipit--mention-prs) 0)
      (let* ((first-mention (car shipit--mention-prs))
             (url (cdr (assq 'url first-mention)))
             (repo (cdr (assq 'repo first-mention)))
             (number (cdr (assq 'number first-mention))))
        (browse-url url)
        (message "Opened %s %s in browser" repo number))
    (message "No mentions found")))

(defun shipit--mark-mentions-read ()
  "Mark all mention notifications as read on GitHub."
  (if (> (length shipit--mention-prs) 0)
      (let ((mention-notifications '()))
        ;; Extract mention notifications from shipit--notification-pr-activities
        (maphash (lambda (_key activity)
                   (when (string= (cdr (assq 'reason activity)) "mention")
                     (push (list (cdr (assq 'number activity))
                                 (cdr (assq 'repo activity))
                                 (or (cdr (assq 'type activity)) "pr"))
                           mention-notifications)))
                 shipit--notification-pr-activities)

        (if mention-notifications
            (if (y-or-n-p (format "Mark %d mentions as read? " (length mention-notifications)))
                (progn
                  (message "Marking %d mentions as read..." (length mention-notifications))
                  (shipit--mark-notifications-batch-async mention-notifications)
                  ;; Clear local mention data
                  (setq shipit--mention-prs nil)
                  (setq shipit--mention-count 0)
                  (shipit--clear-modeline-indicator))
              (message "Mark mentions cancelled"))
          (message "No mention notifications found to mark as read")))
    (message "No mentions found")))

;;; Notification PR Actions

(defun shipit--notification-actions (&optional activity)
  "Handle dwim actions for notification entries (PR or Issue).
ACTIVITY is the notification activity alist.  When nil, looks it up
from the section at point."
  (interactive)
  (let* ((activity (or activity
                       (when (fboundp 'shipit-notifications-buffer--activity-at-point)
                         (shipit-notifications-buffer--activity-at-point))))
         (pr-number (or (cdr (assq 'number activity))
                        (cdr (assq 'pr-number activity))))
         (repo (cdr (assq 'repo activity)))
         (type (or (cdr (assq 'type activity)) "pr"))
         (actions '()))

    ;; If region is active, show batch operations
    (if (region-active-p)
        (let ((region-count (length (shipit--get-notifications-in-region))))
          (when (> region-count 0)
            (push (format "Mark %d selected as read" region-count) actions)))
      ;; Otherwise show type-aware actions
      (pcase type
        ("pr"
         (push "Manage auto-mark rules…" actions)
         (push "Open PR" actions)
         (push "Preview PR" actions)
         (push "Mark as read" actions)
         (push "Configure notifications" actions))
        ("discussion"
         (push "Manage auto-mark rules…" actions)
         (push "Open Discussion" actions)
         (push "Open in browser" actions)
         (push "Mark as read" actions)
         (push "Configure notifications" actions))
        (_
         (push "Manage auto-mark rules…" actions)
         (push "Open Issue" actions)
         (push "Mark as read" actions)
         (push "Configure notifications" actions))))

    (let ((choice (completing-read "Notification action: " actions nil t)))
      (cond
       ((string= choice "Open PR")
        (shipit--open-notification-pr pr-number repo activity))
       ((string= choice "Open Issue")
        (shipit--open-notification-issue pr-number repo activity))
       ((string= choice "Open Discussion")
        (shipit--open-notification-discussion pr-number repo))
       ((string= choice "Open in browser")
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (browse-fn (plist-get backend :browse-discussion-url)))
          (if browse-fn
              (browse-url (funcall browse-fn config pr-number))
            (condition-case nil
                (let* ((disc-resolved (shipit-discussion--resolve-for-repo repo))
                       (disc-backend (car disc-resolved))
                       (disc-config (cdr disc-resolved))
                       (disc-browse-fn (plist-get disc-backend :browse-url)))
                  (when disc-browse-fn
                    (browse-url (funcall disc-browse-fn disc-config pr-number))))
              (error nil)))))
       ((string= choice "Mark as read")
        (shipit--mark-notification-read pr-number repo nil type))
       ((string= choice "Preview PR")
        (shipit--preview-pr pr-number repo))
       ((string= choice "Configure notifications")
        (shipit-notifications-menu))
       ((string= choice "Manage auto-mark rules…")
        (require 'shipit-notifications-buffer)
        (shipit-notifications-buffer-auto-mark-menu))
       ((string-match "Mark \\([0-9]+\\) selected as read" choice)
        (shipit--mark-region-notifications-read))
       (t (message "No action selected"))))))

(defalias 'shipit--notification-pr-actions 'shipit--notification-actions
  "Backward compatibility alias.")

(defun shipit--open-notification-pr (pr-number repo &optional activity)
  "Open the PR/MR from notification.
ACTIVITY is the notification alist for backend data.
Falls back to text properties at point when ACTIVITY is nil.
With prefix arg (C-u), open in browser instead."
  (let* ((backend-id (or (cdr (assq 'backend-id activity))
                         (get-text-property (point) 'shipit-notification-backend-id)))
         (backend-config (or (cdr (assq 'backend-config activity))
                             (get-text-property (point) 'shipit-notification-backend-config)))
         (stored-url (or (cdr (assq 'browse-url activity))
                         (get-text-property (point) 'shipit-notification-browse-url))))
    (cond
     ;; C-u prefix — open in browser using stored URL
     (current-prefix-arg
      (let ((activity-key (shipit--notification-activity-key repo "pr" pr-number)))
        (when (gethash activity-key shipit--notification-pr-activities)
          (shipit--mark-notification-read pr-number repo t "pr")))
      (browse-url (or stored-url (shipit--browse-pr-url repo pr-number)))
      (message "Opened PR #%d in browser" pr-number))
     ;; Default — open via shipit-open-pr-buffer (works for all backends)
     (t
      (require 'shipit-buffer)
      (shipit-open-pr-buffer pr-number repo backend-id backend-config)))))

(defun shipit--open-notification-issue (issue-number repo &optional activity)
  "Open issue ISSUE-NUMBER from REPO in shipit issue buffer.
ACTIVITY is the notification alist for backend data.
Falls back to text properties at point when ACTIVITY is nil.
Automatically marks the notification as read.
C-u prefix opens in browser instead."
  (let* ((activity-key (shipit--notification-activity-key repo "issue" issue-number))
         (stored-url (or (cdr (assq 'browse-url activity))
                         (get-text-property (point) 'shipit-notification-browse-url)))
         (backend-id (or (cdr (assq 'backend-id activity))
                         (get-text-property (point) 'shipit-notification-backend-id)))
         (backend-config (or (cdr (assq 'backend-config activity))
                             (get-text-property (point) 'shipit-notification-backend-config))))
    ;; Auto-clear notification if it exists
    (when (gethash activity-key shipit--notification-pr-activities)
      (shipit--debug-log "Auto-clearing notification for Issue %s" issue-number)
      (shipit--mark-notification-read issue-number repo t "issue"))
    (cond
     ;; C-u prefix — open in browser
     (current-prefix-arg
      (browse-url (or stored-url
                      (shipit--browse-issue-url repo issue-number))))
     ;; Default — open in shipit issue buffer (works for all backends)
     (t
      (require 'shipit-issues-buffer)
      (shipit-issues-open-buffer issue-number repo backend-id backend-config)
      ;; Return the issue buffer so callers (notification activity nav) can
      ;; schedule follow-up work against it.
      (get-buffer (shipit-issue-buffer-name repo issue-number))))))

(defun shipit--open-notification-discussion (number repo)
  "Open discussion NUMBER from REPO in shipit discussion buffer.
Automatically marks the notification as read.
C-u prefix opens in browser instead."
  (let ((activity-key (shipit--notification-activity-key repo "discussion" number)))
    ;; Auto-clear notification if it exists
    (when (gethash activity-key shipit--notification-pr-activities)
      (shipit--debug-log "Auto-clearing notification for Discussion %s" number)
      (shipit--mark-notification-read number repo t "discussion"))
    (if current-prefix-arg
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (browse-fn (plist-get backend :browse-discussion-url)))
          (if browse-fn
              (browse-url (funcall browse-fn config number))
            (condition-case nil
                (let* ((disc-resolved (shipit-discussion--resolve-for-repo repo))
                       (disc-backend (car disc-resolved))
                       (disc-config (cdr disc-resolved))
                       (disc-browse-fn (plist-get disc-backend :browse-url)))
                  (when disc-browse-fn
                    (browse-url (funcall disc-browse-fn disc-config number))))
              (error nil))))
      (require 'shipit-discussions-buffer)
      (shipit-discussions-open-buffer number repo))))

(defun shipit--open-pr-cross-repo (pr-number repo)
  "Open PR in shipit, switching repositories if necessary."
  (let ((current-repo (shipit--get-repo-from-remote)))
    (if (string= repo current-repo)
        ;; Same repo - open directly (pass repo to ensure consistency)
        (shipit--open-pr-same-repo pr-number repo)
      ;; Different repo - switch context and open
      (progn
        (message "Switching to repository %s for PR #%d..." repo pr-number)
        (shipit--switch-to-repo-and-open-pr repo pr-number)))))

(defun shipit--open-pr-same-repo (pr-number &optional repo)
  "Open PR in current repository context.
REPO parameter specifies which repository to use for the PR lookup."
  (let ((pr-data (shipit-get-pull-request pr-number repo)))
    (if pr-data
        (let ((target-repo (or repo (shipit--get-repo-from-remote))))
          (shipit--display-selected-pr pr-data target-repo))
      (user-error "Could not load PR #%d" pr-number))))


(defun shipit--switch-to-repo-and-open-pr (repo pr-number)
  "Switch to repository context and open PR."
  (shipit--debug-log "🔄 Attempting to switch to repo %s and open PR #%d" repo pr-number)

  ;; Validate inputs
  (unless pr-number
    (shipit--debug-log "❌ ERROR: shipit--switch-to-repo-and-open-pr called with nil pr-number")
    (user-error "No PR number provided"))
  (unless repo
    (shipit--debug-log "❌ ERROR: shipit--switch-to-repo-and-open-pr called with nil repo")
    (user-error "No repository provided"))

  (condition-case err
      ;; Fetch PR data from the target repository and display it
      (let ((pr-data (let ((shipit-current-repo repo))
                       (shipit--debug-log "🔍 Fetching PR #%d data from repo %s..." pr-number repo)
                       (shipit-get-pull-request pr-number))))
        (if pr-data
            (progn
              (shipit--debug-log "✅ Successfully fetched PR #%d data, displaying..." pr-number)
              (shipit--display-selected-pr pr-data repo))
          (shipit--debug-log "❌ ERROR: shipit-get-pull-request returned nil for PR #%d in repo %s" pr-number repo)
          (user-error "Could not load PR #%d from repository %s - API request failed" pr-number repo)))
    (error
     (shipit--debug-log "❌ CRITICAL ERROR in shipit--switch-to-repo-and-open-pr: %s" (error-message-string err))
     (shipit--debug-log "❌ BACKTRACE:")
     (shipit--debug-log "%s" (with-output-to-string (backtrace)))
     (user-error "Failed to open PR #%d from %s: %s" pr-number repo (error-message-string err)))))

(defun shipit--open-pr-in-browser-url (repo pr-number)
  "Open PR in browser and auto-clear notifications."
  (let ((url (shipit--browse-pr-url repo pr-number))
        (activity-key (shipit--notification-activity-key repo "pr" pr-number)))

    ;; Auto-clear notification for this PR if it exists
    (when (gethash activity-key shipit--notification-pr-activities)
      (shipit--debug-log "Auto-clearing notification for PR #%d opened in browser" pr-number)
      (shipit--mark-notification-read pr-number repo t "pr"))

    (browse-url url)
    (message "Opened PR #%d in browser" pr-number)))

(defun shipit--resolve-backend-by-id (backend-id activity)
  "Resolve backend plist and config from BACKEND-ID and ACTIVITY.
Returns (BACKEND-PLIST . CONFIG-PLIST) like `shipit-issue--resolve-for-repo'.
Uses backend-config from the activity if present."
  (let* ((backend-plist (or (cdr (assq backend-id shipit-issue-backends))
                            (error "No issue backend registered for `%s'" backend-id)))
         (activity-config (cdr (assq 'backend-config activity)))
         (config (or activity-config (list :backend backend-id))))
    (cons backend-plist config)))

(defun shipit--mark-notification-read (number repo &optional no-refresh type)
  "Mark notification as read via the appropriate backend.
NUMBER is the PR or issue number, REPO is the repository name.
If NO-REFRESH is non-nil, skip the UI refresh (useful for batch operations).
TYPE is \"pr\" or \"issue\" (defaults to \"pr\" for backward compatibility)."
  (let* ((type (or type "pr"))
         (activity-key (shipit--notification-activity-key repo type number))
         (activity (gethash activity-key shipit--notification-pr-activities))
         (notification (when activity (cdr (assq 'notification activity))))
         (notification-id (when notification (cdr (assq 'id notification)))))

    (shipit--debug-log "Looking for notification ID for %s #%s in repo %s" type number repo)
    (shipit--debug-log "   activity-key: %s" activity-key)
    (shipit--debug-log "   activity found: %s" (if activity "YES" "NO"))

    (if activity
        (let ((reason (cdr (assq 'reason activity))))
          ;; Mark as read on the server.
          ;; GitHub notifications (those with a `notification' key) go directly
          ;; to the GitHub Notifications API — this is a notification-level
          ;; operation, NOT an issue backend operation.
          ;; Backend notifications (GitLab todos, etc.) dispatch through their
          ;; backend's :mark-notification-read function.
          (let ((activity-notification-id (when notification (cdr (assq 'id notification))))
                (activity-backend-id (cdr (assq 'backend-id activity))))
            (cond
             ;; GitHub notification: dispatch through PR backend
             (activity-notification-id
              (shipit--debug-log "   notification thread %s — dispatching via PR backend"
                                 activity-notification-id)
              (shipit--dispatch-mark-thread-read repo activity-notification-id))
             ;; Backend notification: dispatch through backend registry
             (activity-backend-id
              (require 'shipit-issue-backends)
              (let* ((resolved (shipit--resolve-backend-by-id activity-backend-id activity))
                     (backend-plist (car resolved))
                     (backend-name (plist-get backend-plist :name))
                     (mark-fn (plist-get backend-plist :mark-notification-read)))
                (shipit--debug-log "   backend notification: %s (has mark-fn: %s)"
                                   backend-name (if mark-fn "YES" "NO"))
                (if mark-fn
                    (funcall mark-fn (cdr resolved) activity)
                  (shipit--debug-log "Backend %s for %s does not support :mark-notification-read (local only)"
                                     backend-name repo))))
             ;; Unknown source
             (t
              (shipit--debug-log "   no notification key or backend-id — local removal only"))))
          ;; Track locally so subsequent polls filter it out.
          ;; GitHub notifications are tracked by notification-id (used in shipit--process-notifications).
          ;; All notifications are tracked by activity-key (used in shipit--merge-backend-notifications).
          (when notification-id
            (puthash notification-id (float-time) shipit--locally-marked-read-notifications))
          (puthash activity-key (float-time) shipit--locally-marked-read-notifications)
          ;; Remove from local cache
          (remhash activity-key shipit--notification-pr-activities)
          ;; Update mention tracking if this was a mention
          (when (string= reason "mention")
            (setq shipit--mention-prs
                  (cl-remove-if (lambda (m)
                                  (and (equal (cdr (assq 'repo m)) repo)
                                       (equal (cdr (assq 'number m)) number)))
                                shipit--mention-prs))
            (setq shipit--mention-count (length shipit--mention-prs)))
          ;; Update notification count immediately
          (let ((new-count (hash-table-count shipit--notification-pr-activities)))
            (setq shipit--last-notification-count new-count)
            ;; Update visual indicators immediately (modeline only, no expensive refresh)
            (shipit--update-modeline-indicator new-count))
          ;; Only show message and refresh if not in batch mode
          (unless no-refresh
            (message "Marked %s %s as read"
                     (if (string= type "pr") "PR" "Issue") number)
            ;; Clear notifications from ETag cache so next poll gets fresh data
            (shipit--clear-notifications-cache)
            ;; Refresh the standalone notifications buffer if it exists
            (when (get-buffer "*shipit-notifications*")
              (require 'shipit-notifications-buffer)
              (shipit-notifications-buffer-refresh))))
      ;; Notification not found - likely already read
      (if no-refresh
          ;; In batch mode, just log and continue (don't interrupt the loop)
          (shipit--debug-log "Notification for %s %s not found (likely already read)" type number)
        ;; In interactive mode, show error to user
        (user-error "Could not find notification ID for %s %s"
                    (if (string= type "pr") "PR" "Issue") number)))))

(defun shipit--preview-pr (pr-number repo)
  "Show preview popup for PR-NUMBER in REPO."
  (message "Fetching PR #%d from %s..." pr-number repo)
  ;; Use shipit--show-pr-preview which sets shipit--current-pr-preview-data
  ;; for timeline events and keybinding handlers
  (shipit--show-pr-preview pr-number repo))

(defun shipit--notifications-header-actions ()
  "Handle dwim actions for notification section header."
  (interactive)
  (let ((notification-count (hash-table-count shipit--notification-pr-activities))
        (actions '()))

    (push "Set filter" actions)
    (push "Update notifications" actions)
    (when (> notification-count 0)
      (push "Mark all as read" actions))
    (push "Configure notifications" actions)

    (let ((choice (completing-read "Notifications action: " actions nil t)))
      (cond
       ((string= choice "Set filter")
        (shipit--set-notifications-filter))
       ((string= choice "Update notifications")
        (shipit--poll-notifications-now))
       ((string= choice "Mark all as read")
        (shipit--mark-all-notifications-read))
       ((string= choice "Configure notifications")
        (shipit-notifications-menu))
       (t (message "No action selected"))))))

(defun shipit--mark-all-notifications-read ()
  "Mark all notifications as read using their respective backends."
  (interactive)
  (let ((count (hash-table-count shipit--notification-pr-activities)))
    (when (> count 0)
      (if (y-or-n-p (format "Mark all %d notifications as read? " count))
          (progn
            (message "Marking %d notifications as read..." count)
            (let ((marked-count 0))
              (maphash (lambda (_key activity)
                         (let* ((notif (cdr (assq 'notification activity)))
                                (notif-id (when notif (cdr (assq 'id notif))))
                                (activity-repo (cdr (assq 'repo activity)))
                                (activity-backend-id (cdr (assq 'backend-id activity))))
                           (cond
                            ;; GitHub notification: dispatch through PR backend
                            (notif-id
                             (let* ((resolved (shipit-pr--resolve-for-repo
                                               (or activity-repo "")))
                                    (pr-backend (car resolved))
                                    (pr-config (cdr resolved))
                                    (mark-fn (plist-get pr-backend
                                                        :mark-notification-read)))
                               (when mark-fn
                                 (funcall mark-fn pr-config notif-id))))
                            ;; Backend notification: dispatch through issue backend
                            (activity-backend-id
                             (require 'shipit-issue-backends)
                             (let* ((resolved (shipit--resolve-backend-by-id activity-backend-id activity))
                                    (mark-fn (plist-get (car resolved) :mark-notification-read)))
                               (when mark-fn
                                 (funcall mark-fn (cdr resolved) activity)))))
                           (setq marked-count (1+ marked-count))))
                       shipit--notification-pr-activities)
              (clrhash shipit--notification-pr-activities)
              (setq shipit--last-notification-count 0)
              (shipit--clear-modeline-indicator)
              (message "Marked %d notifications as read" marked-count)
              ;; Clear cache after marking all as read
              (shipit--clear-notifications-cache)
              (when (fboundp 'magit-refresh)
                (magit-refresh))))
        (message "Mark all cancelled")))))

;; ASYNC NOTIFICATION POLLING FUNCTIONS
;; These functions replace the synchronous versions to prevent Emacs stuttering

(defun shipit--notifications-buffer-in-all-scope-p ()
  "Return non-nil when any visible notifications buffer is in `all' scope.
The background poll fetches the global `unread' scope and `merges'
the result into the activity cache by replacing every GitHub entry —
which would wipe out read items the user is currently viewing in the
`all' scope.  Callers should skip the poll when this returns non-nil."
  (and (get-buffer "*shipit-notifications*")
       (with-current-buffer "*shipit-notifications*"
         (and (boundp 'shipit-notifications-buffer--display-scope)
              (eq shipit-notifications-buffer--display-scope 'all)))))

(defun shipit--check-notifications-background-async (&optional force-fresh)
  "Check GitHub notifications asynchronously to prevent Emacs stuttering.
If FORCE-FRESH is non-nil, bypasses ETag cache to get fresh data."
  (cond
   ((shipit--notifications-buffer-in-all-scope-p)
    (shipit--debug-log
     "Skipping background poll: notifications buffer is in `all' scope"))
   (t
    (condition-case err
        (let* ((params (shipit--notification-params-for-scope shipit-notifications-scope)))
          ;; Fetch first page asynchronously
          (shipit--fetch-all-notifications-async
           params
           nil
           force-fresh
           (lambda (all-notifications)
             (shipit--process-notifications all-notifications)
             ;; Poll backend notifications if backends are configured
             (when (featurep 'shipit-issue-backends)
               (shipit--poll-backend-notifications)))))
      (error
       (shipit--debug-log "Async background notifications check failed: %s" (error-message-string err)))))))

(defun shipit--fetch-all-notifications-async (params &optional since-watermark force-fresh callback)
  "Fetch notifications asynchronously with pagination.
CALLBACK will be called with the complete list of notifications when done."
  (let ((all-notifications '()))
    (shipit--fetch-notifications-page-async
     1 params since-watermark force-fresh all-notifications callback)))

(defun shipit--fetch-notifications-page-async (page params since-watermark force-fresh accumulated-notifications callback)
  "Fetch a single page of notifications asynchronously."
  (let ((page-params (append params `((page . ,page)))))
    (require 'shipit-gh-etag)
    (shipit-gh-etag-get-json-async
     "/notifications"
     page-params
     shipit-github-token
     (lambda (result)
       (let* ((raw-json (when result (plist-get result :json)))
              ;; Guard against error responses like {"message":"Not Found"}
              ;; which parse as ((message . "Not Found")) — a list of cons cells,
              ;; not a list of alists.  Valid notifications are alists whose car
              ;; is itself a cons cell, e.g. ((id . "123") (subject ...) ...).
              (page-notifications (when (and raw-json (listp raw-json)
                                             (consp (car-safe (car-safe raw-json))))
                                    raw-json)))
         (when (and raw-json (not page-notifications))
           (shipit--debug-log "Async page %d: ignoring error response: %S" page raw-json))
         (shipit--debug-log "Async page %d: result=%s json-count=%d"
                            page (not (null result))
                            (if page-notifications (length page-notifications) 0))

         (if (and page-notifications (> (length page-notifications) 0) (< page 10))
             (progn
               ;; More notifications available, fetch next page
               (let ((new-accumulated (append accumulated-notifications page-notifications)))
                 (shipit--debug-log "Fetched async page %d: %d notifications" page (length page-notifications))
                 (shipit--fetch-notifications-page-async
                  (1+ page) params since-watermark force-fresh new-accumulated callback)))
           ;; No more pages or hit limit, return all notifications
           (let ((final-notifications (if page-notifications
                                          (append accumulated-notifications page-notifications)
                                        accumulated-notifications)))
             (shipit--debug-log "Async total notifications fetched: %d across %d pages"
                                (length final-notifications) page)
             (funcall callback final-notifications)))))
     force-fresh)))

;; Note: The existing shipit--start-notifications-polling function now uses async by default

(defun shipit-notifications--copy-pr-url ()
  "Copy the URL of the PR at point to the clipboard.
Used in notifications section to avoid dependency on shipit-commands."
  (interactive)
  (let* ((activity (when (fboundp 'shipit-notifications-buffer--activity-at-point)
                     (shipit-notifications-buffer--activity-at-point)))
         (pr-number (when activity
                      (or (cdr (assq 'number activity))
                          (cdr (assq 'pr-number activity)))))
         (repo (when activity (cdr (assq 'repo activity)))))
    (if (and pr-number repo)
        (let ((url (shipit--browse-pr-url repo pr-number)))
          (kill-new url)
          (message "Copied: %s" url))
      (user-error "No PR at point"))))

;;; Alert Functions

(defun shipit--should-alert-p (notification)
  "Return non-nil if NOTIFICATION should trigger an alert."
  (let* ((reason (cdr (assq 'reason notification)))
         (notif-obj (cdr (assq 'notification notification)))
         (notif-id (when notif-obj (cdr (assq 'id notif-obj)))))
    (and shipit-notification-alert-backend
         shipit-notification-alert-reasons
         reason
         (memq (intern reason) shipit-notification-alert-reasons)
         notif-id
         (not (gethash notif-id shipit--alerted-notification-ids)))))

(defun shipit--mark-alerted (notification)
  "Mark NOTIFICATION as alerted."
  (let* ((notif-obj (cdr (assq 'notification notification)))
         (notif-id (when notif-obj (cdr (assq 'id notif-obj)))))
    (when notif-id
      (puthash notif-id t shipit--alerted-notification-ids))))

(defun shipit--queue-alert (notification)
  "Queue NOTIFICATION for alert, batching if needed."
  (shipit--debug-log "ALERT: Queueing alert for %s#%d reason=%s"
                     (cdr (assq 'repo notification))
                     (cdr (assq 'number notification))
                     (cdr (assq 'reason notification)))
  (shipit--mark-alerted notification)
  (push notification shipit--pending-alerts)
  ;; Cancel existing timer and start new one
  (when shipit--alert-timer
    (cancel-timer shipit--alert-timer))
  (setq shipit--alert-timer
        (run-with-timer shipit-notification-alert-batch-seconds nil
                        #'shipit--fire-alerts))
  (shipit--debug-log "🔔 ALERT: Timer set for %d seconds, %d pending alerts"
                     shipit-notification-alert-batch-seconds
                     (length shipit--pending-alerts)))

(defun shipit--fire-alerts ()
  "Fire batched alerts."
  (when shipit--pending-alerts
    (let ((alerts (nreverse shipit--pending-alerts)))
      (setq shipit--pending-alerts nil)
      (setq shipit--alert-timer nil)
      (if (= (length alerts) 1)
          (shipit--show-single-alert (car alerts))
        (shipit--show-batched-alert alerts)))))

(defun shipit--show-single-alert (notification)
  "Show alert for single NOTIFICATION."
  (let* ((reason (cdr (assq 'reason notification)))
         (repo (cdr (assq 'repo notification)))
         (number (cdr (assq 'number notification)))
         (subject (cdr (assq 'subject notification)))
         (title (format "%s in %s#%d"
                        (shipit--format-reason-for-alert reason)
                        repo number))
         (body (or subject "")))
    (shipit--send-alert title body repo number)))

(defun shipit--format-reason-for-alert (reason)
  "Format REASON for display in alert."
  (pcase reason
    ("mention" "Mention")
    ("review_requested" "Review requested")
    ("assign" "Assigned")
    ("comment" "Comment")
    ("team_mention" "Team mention")
    ("author" "Activity on your PR")
    (_ (capitalize (or reason "notification")))))

(defun shipit--show-batched-alert (notifications)
  "Show combined alert for multiple NOTIFICATIONS."
  (let* ((count (length notifications))
         (title (format "%d new notifications" count))
         (body (mapconcat
                (lambda (n)
                  (format "- %s in %s#%d"
                          (shipit--format-reason-for-alert (cdr (assq 'reason n)))
                          (cdr (assq 'repo n))
                          (cdr (assq 'number n))))
                (seq-take notifications 5)  ; Limit to 5 lines
                "\n")))
    (when (> count 5)
      (setq body (concat body (format "\n... and %d more" (- count 5)))))
    (shipit--send-alert title body nil nil)))

(defun shipit--send-alert (title body repo pr-number)
  "Send alert with TITLE and BODY. REPO and PR-NUMBER for click action."
  (shipit--debug-log "🔔 ALERT: Sending alert - backend=%s title=%s" shipit-notification-alert-backend title)
  (pcase shipit-notification-alert-backend
    ('message (shipit--send-alert-via-message title body))
    ('dbus (shipit--send-alert-via-dbus title body))
    ('alert (shipit--send-alert-via-alert title body repo pr-number))
    (_ (shipit--debug-log "🔔 ALERT: Backend disabled or unknown"))))

(defun shipit--send-alert-via-message (title body)
  "Send alert using minibuffer message."
  (message "🔔 %s: %s" title body))

(defun shipit--send-alert-via-dbus (title body)
  "Send alert using D-Bus notifications (Linux)."
  (if (require 'notifications nil t)
      (notifications-notify :title title :body body :app-name "Shipit")
    (shipit--debug-log "D-Bus notifications not available")))

(defun shipit--send-alert-via-alert (title body repo pr-number)
  "Send alert using alert.el package."
  (shipit--debug-log "🔔 ALERT: Using alert.el backend")
  (if (require 'alert nil t)
      (progn
        (shipit--debug-log "🔔 ALERT: Calling alert with title=%s body=%s" title body)
        (alert body
               :title title
               :category 'shipit
               :id 'shipit-notification))
    (shipit--debug-log "🔔 ALERT: alert.el not installed")))

;;; Backend Notification Polling

(defun shipit--autodiscover-notification-backends ()
  "Auto-discover notification backends via registered :autodiscover functions.
Iterates all issue backends that support :notifications and have :autodiscover.
Skips backends already present in `shipit-issue-repo-backends'."
  (let ((result '())
        (existing-backend-ids
         (mapcar (lambda (e) (plist-get (cdr e) :backend))
                 shipit-issue-repo-backends)))
    (dolist (entry shipit-issue-backends)
      (let* ((id (car entry))
             (plist (cdr entry))
             (discover-fn (plist-get plist :autodiscover)))
        (when (and discover-fn
                   (not (memq id existing-backend-ids))
                   (shipit-issue--backend-has-notifications-p plist))
          (let ((found (funcall discover-fn)))
            (when found
              (push found result))))))
    (nreverse result)))

(defun shipit--poll-one-backend (entry since)
  "Poll a single backend ENTRY using SINCE as the watermark.
Uses the backend's :notifications-async function when available so
slow HTTP (e.g. Jira) does not block the UI; otherwise falls back
to the sync :notifications function."
  (let* ((config-plist (cdr entry))
         (backend-id (plist-get config-plist :backend))
         (backend-plist (cdr (assq backend-id shipit-issue-backends)))
         (async-fn (and backend-plist
                        (plist-get backend-plist :notifications-async))))
    (when (and backend-plist
               (shipit-issue--backend-has-notifications-p backend-plist))
      (condition-case err
          (if async-fn
              (funcall async-fn config-plist since
                       (lambda (activities)
                         (when activities
                           (shipit--merge-backend-notifications activities)
                           (shipit--rerender-notifications-buffer-if-visible))))
            (let ((activities (shipit-issue--fetch-notifications
                               backend-plist config-plist since)))
              (when activities
                (shipit--merge-backend-notifications activities))))
        (error
         (shipit--debug-log "Backend notification poll failed for %s: %s"
                            backend-id (error-message-string err)))))))

(defun shipit--poll-backend-notifications ()
  "Poll configured backends that have :notifications and merge results.
Each backend is scheduled on its own idle timer with a small
stagger so a slow backend (Jira can take 3+ seconds) does not
pile up with the others into one long UI freeze."
  (when (featurep 'shipit-issue-backends)
    (let* ((since shipit--backend-last-poll-time)
           (all-entries (append shipit-issue-repo-backends
                               (shipit--autodiscover-notification-backends)))
           (delay 0.0))
      (setq shipit--backend-last-poll-time
            (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
      (dolist (entry all-entries)
        (let ((entry-cell entry)
              (since-snapshot since))
          (run-with-idle-timer
           delay nil
           (lambda ()
             (shipit--poll-one-backend entry-cell since-snapshot)
             (shipit--rerender-notifications-buffer-if-visible))))
        (setq delay (+ delay 0.1))))))

(defun shipit--merge-github-notifications (github-activities)
  "Merge GITHUB-ACTIVITIES into the global hash table.
Replaces existing GitHub entries, preserves non-GitHub backend entries."
  (unless shipit--notification-pr-activities
    (setq shipit--notification-pr-activities (make-hash-table :test 'equal)))
  ;; Remove old GitHub entries (those without a backend source)
  (let ((keys-to-remove nil))
    (maphash (lambda (key activity)
               (unless (cdr (assq 'backend-id activity))
                 (push key keys-to-remove)))
             shipit--notification-pr-activities)
    (dolist (key keys-to-remove)
      (remhash key shipit--notification-pr-activities)))
  ;; Add current GitHub entries
  (maphash (lambda (key activity)
             (puthash key activity shipit--notification-pr-activities))
           github-activities))

(defun shipit--merge-backend-notifications (activities)
  "Merge backend notification ACTIVITIES into the global hash table.
Each activity should be an alist with keys: number, type, subject, reason, repo."
  (unless shipit--notification-pr-activities
    (setq shipit--notification-pr-activities (make-hash-table :test 'equal)))
  (dolist (activity activities)
    (let* ((repo (cdr (assq 'repo activity)))
           (type (or (cdr (assq 'type activity)) "issue"))
           (number (cdr (assq 'number activity)))
           (key (shipit--notification-activity-key repo type number)))
      (if (gethash key shipit--locally-marked-read-notifications)
          (shipit--debug-log "Skipping locally-marked-read backend notification: %s" key)
        (puthash key activity shipit--notification-pr-activities))))
  ;; Update indicators
  (let ((new-count (hash-table-count shipit--notification-pr-activities)))
    (when (not (eq new-count shipit--last-notification-count))
      (shipit--handle-new-notifications new-count))
    (setq shipit--last-notification-count new-count)))

(provide 'shipit-notifications)
;;; shipit-notifications.el ends here
