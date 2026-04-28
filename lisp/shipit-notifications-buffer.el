;;; shipit-notifications-buffer.el --- Dedicated notifications buffer -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 shipit contributors

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
;; Standalone buffer for viewing notifications using magit sections.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'shipit-core)
(require 'magit-section)

;; Forward declarations
(declare-function shipit--mark-notification-read "shipit-notifications")
(declare-function shipit--notification-pr-actions "shipit-notifications")
(declare-function shipit--notification-actions "shipit-notifications")
(declare-function shipit--notification-activity-key "shipit-notifications")
(declare-function shipit--check-notifications-background "shipit-notifications")
(declare-function shipit--fetch-notifications-total-count-async "shipit-notifications")
(declare-function shipit-subscriptions--backends-with-watched-repos "shipit-subscriptions-buffer")
(declare-function shipit--update-modeline-indicator "shipit-notifications")
(declare-function shipit--format-time-ago "shipit-notifications")
(declare-function shipit--get-notification-type-icon "shipit-render")
(declare-function shipit--get-notification-source-icon "shipit-render")
(declare-function shipit-open-actions-list "shipit-actions-list")
(declare-function shipit--debug-log "shipit-core")
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--render-body "shipit-render")
(declare-function shipit--api-request "shipit-http")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")
(declare-function shipit--insert-activity-event "shipit-pr-sections")
(declare-function shipit--fetch-timeline-events-async "shipit-http")
(declare-function shipit--fetch-recent-timeline-events-async "shipit-http")
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit-toggle-timestamp-format "shipit-commands")
(declare-function shipit--format-timestamp "shipit-core")
(declare-function shipit--browse-pr-url "shipit-notifications")
(declare-function shipit--browse-issue-url "shipit-notifications")
(declare-function shipit--open-notification-pr "shipit-notifications")
(declare-function shipit-pr--dispatch-activity-navigation "shipit-pr-actions")
(declare-function shipit-buffer--on-section-ready "shipit-buffer")
(declare-function shipit-issue-jira--fetch-components "shipit-issue-jira")
(declare-function shipit-issue-jira--fetch-last-activity-async "shipit-issue-jira")
(declare-function shipit--open-notification-issue "shipit-notifications")
(declare-function shipit--open-notification-discussion "shipit-notifications")

(defconst shipit-notifications-buffer-name "*shipit-notifications*"
  "Name of the notifications buffer.")

(defvar-local shipit-notifications-buffer--filter-text ""
  "Current filter text for the notifications buffer.")

(defvar-local shipit-notifications-buffer--display-scope 'unread
  "Buffer-local display scope for the notifications buffer.
One of `unread' (GitHub's unread feed, usually a single short page)
or `all' (include read notifications, paginated on demand via
`shipit-notifications-buffer-page-forward').  Kept buffer-local so
toggling in the buffer does not affect the background poll's
`shipit-notifications-scope' setting.")

(defvar-local shipit-notifications-buffer--current-page 1
  "Number of pages currently loaded into the notifications buffer.
Meaningful mainly when `shipit-notifications-buffer--display-scope'
is `all'; incremented by `shipit-notifications-buffer-page-forward'.
Starts at 1 and resets to 1 on every scope toggle so switching
views doesn't silently fan out 10 pages of network requests.")

(defvar-local shipit-notifications-buffer--total-count nil
  "Last known total notification count for the current scope.
Populated asynchronously by `shipit--fetch-notifications-total-count-async'
after each refresh; nil means the probe has not returned yet (or failed).")

(defvar-local shipit-notifications-buffer--selected-repo nil
  "Buffer-local repo allow-list for the notifications buffer.
Nil means all repos.  A list of `OWNER/REPO' strings restricts
the view to those repos.  Single-entry lists keep the
server-side fast path: the fetch + total-count probe target
that repo's per-repo notifications endpoint.  Two or more
entries fall back to the all-notifications endpoint with
client-side filtering.  Comparison is case-insensitive.")

(defvar-local shipit-notifications-buffer--excluded-repos nil
  "List of repo strings hidden from the notifications buffer.
Inverse of `--selected-repo': the selector picks one to show, the
exclusion list picks any number to hide.  Comparison is
case-insensitive.  Toggled via prefix arg on the repo set/clear
commands.")

(defvar-local shipit-notifications-buffer--selected-type nil
  "Buffer-local type allow-list for the notifications buffer.
Nil/empty means all types pass.  Otherwise a list of internal
type strings (`pr', `issue', `discussion', ...) restricts the
view to activities whose type is in the list.")

(defvar-local shipit-notifications-buffer--excluded-types nil
  "List of types hidden from the notifications buffer.
Inverse of `--selected-type'.  Toggled via prefix arg on the
type set/clear commands.")

(defcustom shipit-notifications-actionable-reasons
  '("mention" "team_mention" "review_requested" "assign"
    "security_alert")
  "Reason strings treated as `actionable\=' for the buffer toggle.
When `shipit-notifications-buffer--actionable-only\=' is non-nil,
the buffer hides every activity whose `reason\=' is not a member
of this list — distilling the inbox to items that arguably need
the user\='s attention.

The default values are GitHub reason strings.  Notifications that
flow in from other backends (GitLab todos, Jira mentions, RSS) use
their own reason strings which need to be added explicitly here
to participate in the actionable-only filter.  See the
`reason\=' field on activities in the
`shipit--notification-pr-activities\=' hash for the strings each
backend produces."
  :type '(repeat string)
  :group 'shipit)

(defvar-local shipit-notifications-buffer--actionable-only nil
  "Tri-state actionable filter for the notifications buffer.
nil — show all notifications.
t   — show only actionable rows (reason is in
        `shipit-notifications-actionable-reasons').
`non-actionable' — show only non-actionable rows (the inverse).")

(defvar-local shipit-notifications-buffer--group-by-repo nil
  "When non-nil, render notifications nested under per-repo sections.
Each repo becomes a collapsible `notification-repo' magit-section
whose children are the usual `notification-entry' rows.  Repos are
ordered by the `updated-at' of their newest entry, so the most
recently active repo is at the top.")

(defvar-local shipit-notifications-buffer--excluded-reasons nil
  "List of reasons hidden from the notifications buffer.
Inverse of `--selected-reason'.")

(defvar-local shipit-notifications-buffer--selected-reason nil
  "Buffer-local client-side reason filter for the notifications buffer.
Nil means all reasons; a string like \"mention\" or \"review_requested\"
narrows to that specific GitHub reason.  Combines with every other
filter (the actionable-only toggle is the broader preset of this).")

(defvar-local shipit-notifications-buffer--excluded-jira-components nil
  "List of Jira components hidden from the notifications buffer.
Inverse of `--selected-jira-component'.")

(defvar-local shipit-notifications-buffer--selected-jira-component nil
  "Buffer-local Jira component filter for the notifications buffer.
Nil means no filtering (Jira and non-Jira activities both render).
A string narrows the view to Jira activities whose `jira-components'
list (case-insensitive) contains that component, plus non-Jira
activities — so applying the filter doesn't hide your GitHub
notifications, only narrows the Jira slice.")

(defvar-local shipit-notifications-buffer--excluded-states nil
  "List of PR states hidden from the notifications buffer.
Inverse of `--selected-state'.")

(defvar-local shipit-notifications-buffer--selected-state nil
  "Buffer-local client-side PR-state filter for the notifications buffer.
Nil means no filtering.  Set to one of `open'/`closed'/`merged'/`draft'
to show only PRs in that state — non-PR activities (issues, workflows,
releases, etc.) are also hidden when the filter is active so picking
`draft' actually shows just drafts, not drafts + everything stateless.
Filter values mirror the icon-picker: `draft' = open + isDraft;
`open' = open + not-draft.")

(defvar-local shipit-notifications-buffer--snoozed-items nil
  "Alist of (KEY . EXPIRES-FLOAT-TIME) for snoozed activities.
KEY is the same activity-key used everywhere else
(`REPO:TYPE:NUMBER').  EXPIRES-FLOAT-TIME is the float-time at
which the snooze lapses; activities are filtered from view until
then, after which auto-prune drops the entry on the next render.")

(defvar-local shipit-notifications-buffer--before-filter nil
  "Buffer-local server-side `before' timestamp filter.
Nil means no time filter (GitHub's default of ~last 2 weeks applies).
When set to an ISO-8601 timestamp string, it is passed as GitHub's
`before' query parameter so only notifications updated before that
moment are returned.  Used to time-travel back past GitHub's default
2-week window.")

(defvar-local shipit-notifications-buffer--since-filter nil
  "Buffer-local server-side `since' timestamp filter.
Nil means no time filter.  When set, passed as GitHub's `since'
query parameter so only notifications updated after that moment
are returned.  Pairs with `--before-filter' to form a time window.")

(defvar shipit-notifications-buffer--watched-repos-cache nil
  "Session cache of subscribed/watched repos for the repo-filter picker.
Populated on first open of the repo-filter picker via the backends'
`:fetch-watched-repos'; reused for later picker invocations so the
GraphQL round-trip only happens once per session.  Cleared by
`shipit-notifications-buffer-refresh-watched-repos-cache'.")

;; Section types
(defun notification-entry (&rest _args)
  "Magit section identifier for notification entries.")
(put 'notification-entry 'magit-section t)

(defun notification-snoozed-group (&rest _args)
  "Magit section identifier for the collapsible snoozed-group footer.")
(put 'notification-snoozed-group 'magit-section t)

(defun notification-repo (&rest _args)
  "Magit section identifier for the per-repo wrapper used when
`shipit-notifications-buffer--group-by-repo' is non-nil.")
(put 'notification-repo 'magit-section t)

(defun notification-activity (&rest _args)
  "Magit section identifier for notification activity timeline.")
(put 'notification-activity 'magit-section t)

(defun notification-jira-last-activity (&rest _args)
  "Magit section identifier for the lazily-fetched Jira last-activity row.")
(put 'notification-jira-last-activity 'magit-section t)

(defun notification-description (&rest _args)
  "Magit section identifier for notification description.")
(put 'notification-description 'magit-section t)

;;; Keymap

(defvar shipit-notifications-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'shipit-notifications-buffer-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "f") #'shipit-notifications-buffer-filter-menu)
    (define-key map (kbd "RET") #'shipit-notifications-buffer-open)
    (define-key map (kbd "M-;") #'shipit-notifications-buffer-action)
    (define-key map (kbd "TAB") #'shipit-notifications-buffer-toggle-section)
    (define-key map [tab] #'shipit-notifications-buffer-toggle-section)
    (define-key map (kbd "m") #'shipit-notifications-buffer-mark-menu)
    (define-key map (kbd "n") #'magit-section-forward)
    (define-key map (kbd "p") #'magit-section-backward)
    (define-key map (kbd "M-n") #'shipit-notifications-buffer-page-forward)
    (define-key map (kbd "M-p") #'shipit-notifications-buffer-page-back)
    (define-key map (kbd "M-<") #'shipit-notifications-buffer-first-page)
    (define-key map (kbd "M->") #'shipit-notifications-buffer-last-page)
    (define-key map (kbd "P") #'shipit-notifications-buffer-goto-page)
    (define-key map (kbd "L") #'shipit-toggle-timestamp-format)
    (define-key map (kbd "M-w") #'shipit-notifications-buffer-copy-url)
    (define-key map (kbd "w") #'shipit-notifications-buffer-watch)
    ;; Direct shortcuts mirroring the filter transient (still accessible via f).
    (define-key map (kbd "s") #'shipit-notifications-buffer-toggle-scope)
    (define-key map (kbd "!") #'shipit-notifications-buffer-toggle-actionable-only)
    (define-key map (kbd "G") #'shipit-notifications-buffer-toggle-group-by-repo)
    (define-key map (kbd "t") #'shipit-notifications-buffer-set-filter)
    (define-key map (kbd "c") #'shipit-notifications-buffer-clear-filter)
    (define-key map (kbd "r") #'shipit-notifications-buffer-set-repo)
    (define-key map (kbd "R") #'shipit-notifications-buffer-clear-repo)
    (define-key map (kbd "z") #'shipit-notifications-buffer-snooze-at-point)
    (define-key map (kbd "Z") #'shipit-notifications-buffer-clear-snoozes)
    (define-key map (kbd "o") #'shipit-notifications-buffer-set-jira-component)
    (define-key map (kbd "O") #'shipit-notifications-buffer-clear-jira-component)
    (define-key map (kbd "y") #'shipit-notifications-buffer-set-type)
    (define-key map (kbd "Y") #'shipit-notifications-buffer-clear-type)
    (define-key map (kbd "e") #'shipit-notifications-buffer-set-state)
    (define-key map (kbd "E") #'shipit-notifications-buffer-clear-state)
    (define-key map (kbd "a") #'shipit-notifications-buffer-set-since-filter)
    (define-key map (kbd "A") #'shipit-notifications-buffer-clear-since-filter)
    (define-key map (kbd "b") #'shipit-notifications-buffer-set-before-filter)
    (define-key map (kbd "B") #'shipit-notifications-buffer-clear-before-filter)
    (define-key map (kbd "x") #'shipit-notifications-buffer-clear-all-filters)
    map)
  "Keymap for `shipit-notifications-buffer-mode'.")

;;; Mode

(define-derived-mode shipit-notifications-buffer-mode magit-section-mode "Shipit-Notifications"
  "Major mode for viewing notifications.

\\{shipit-notifications-buffer-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-notifications-buffer-refresh)
  (setq-local truncate-lines t)
  (setq-local tab-width 8)
  (shipit--apply-section-defaults))

;;; Buffer management

(defun shipit-notifications-buffer-create ()
  "Create or return the singleton notifications buffer."
  (let ((buf (get-buffer shipit-notifications-buffer-name)))
    (unless buf
      (setq buf (get-buffer-create shipit-notifications-buffer-name))
      (with-current-buffer buf
        (shipit-notifications-buffer-mode)))
    buf))

(defun shipit-notifications-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the notifications buffer content.
Interactive invocations bypass the ETag cache so the fetch is
guaranteed to return fresh server state -- a manual `g' is a
manual poll, distinct from the periodic auto-poller (which keeps
ETag caching enabled to be polite).  Re-renders using the buffer
local display scope, current page, and filters.  IGNORE-AUTO and
NOCONFIRM are for compatibility with `revert-buffer'."
  (interactive)
  (let ((force-fresh (called-interactively-p 'any))
        (scope shipit-notifications-buffer--display-scope)
        (page shipit-notifications-buffer--current-page)
        ;; Server-side fast path only when exactly one repo is selected.
        ;; With 0 or 2+ entries we fetch the unfiltered feed and rely on
        ;; client-side `--matches-repo-p' to narrow.
        (repo (let ((sel shipit-notifications-buffer--selected-repo))
                (and (consp sel) (= 1 (length sel)) (car sel))))
        (before shipit-notifications-buffer--before-filter)
        (since shipit-notifications-buffer--since-filter)
        (buf (current-buffer)))
    (message "Fetching notifications%s..." (if force-fresh " (forced)" ""))
    ;; Invalidate the cached total so the header doesn't keep showing a
    ;; stale repo-specific total after e.g. the repo filter was cleared.
    (setq shipit-notifications-buffer--total-count nil)
    (when (fboundp 'shipit--check-notifications-background)
      ;; In `all' scope we deliberately fetch a single page (windowed
      ;; pagination — `page-forward'/`page-back' shift the window).
      ;; In `unread' scope, page math is irrelevant and we want every
      ;; unread item populated up front so the buffer reflects the same
      ;; state as the background poll.
      (let ((max-pages (if (eq scope 'all) 1 10)))
        (shipit--check-notifications-background
         force-fresh scope max-pages repo page before since)))
    (when (fboundp 'shipit--fetch-notifications-total-count-async)
      (shipit--fetch-notifications-total-count-async
       scope
       (lambda (count)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq shipit-notifications-buffer--total-count count)
             (shipit-notifications-buffer--rerender))))
       repo before since)))
  (shipit-notifications-buffer--rerender)
  (message "Notifications refreshed"))

(defun shipit-notifications-buffer--rerender ()
  "Re-render the notifications buffer without fetching new data."
  (let ((buf (shipit-notifications-buffer-create)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (shipit-notifications-buffer--render)
        (goto-char (min pos (point-max)))))))

(defun shipit-notifications-buffer--prune-expired-snoozes ()
  "Drop expired entries from `--snoozed-items'.  Returns the live alist."
  (let ((now (float-time)))
    (setq shipit-notifications-buffer--snoozed-items
          (cl-remove-if (lambda (cell) (<= (cdr cell) now))
                        shipit-notifications-buffer--snoozed-items))))

(defun shipit-notifications-buffer--matches-snooze-p (activity)
  "Return non-nil when ACTIVITY is NOT currently snoozed.
A nil entry in `--snoozed-items' or an entry whose timestamp has
elapsed lets the activity through; an unexpired snooze hides it."
  (let* ((repo (cdr (assq 'repo activity)))
         (type (or (cdr (assq 'type activity)) "pr"))
         (number (or (cdr (assq 'number activity))
                     (cdr (assq 'pr-number activity))))
         (key (and repo number
                   (format "%s:%s:%s" repo type number)))
         (cell (and key (assoc key shipit-notifications-buffer--snoozed-items)))
         (result (or (null cell)
                     (<= (cdr cell) (float-time)))))
    (when (and shipit-notifications-buffer--snoozed-items
               (not result))
      (shipit--debug-log "Snooze: hiding key=%S" key))
    result))

;;; Rendering

(defvar shipit-notifications-buffer--render-pool nil
  "Dynamic cache of the repo-filtered activity pool during one render.
Bound by `shipit-notifications-buffer--render' so header helpers and
the list renderer share a single hash walk per render instead of
repeating it for every count call.  Nil outside the render.

Declared here (before the binder) so the byte-compiler treats it as
special; otherwise the `let' inside `--render' binds it lexically
and downstream readers like `--repo-filtered-activities' see the
global nil and fall through to their own filter chain.")

(defun shipit-notifications-buffer--render ()
  "Render the notifications buffer content.
Bind `shipit-notifications-buffer--render-pool' once per render so
the callers that need the structurally-filtered activity pool
share a single hash walk + filter."
  (shipit-notifications-buffer--prune-expired-snoozes)
  (let ((shipit-notifications-buffer--render-pool
         (seq-filter (lambda (a)
                       (and (shipit-notifications-buffer--matches-snooze-p a)
                            (shipit-notifications-buffer--matches-repo-p a)
                            (shipit-notifications-buffer--matches-type-p a)
                            (shipit-notifications-buffer--matches-state-p a)
                            (shipit-notifications-buffer--matches-reason-p a)
                            (shipit-notifications-buffer--matches-actionable-filter-p a)
                            (shipit-notifications-buffer--matches-jira-component-p a)))
                     (shipit-notifications-buffer--all-activities))))
    (magit-insert-section (notifications-root)
      (shipit-notifications-buffer--insert-header)
      (shipit-notifications-buffer--insert-notifications))))

(defun shipit-notifications-buffer--all-activities ()
  "Return the list of all activities in the global hash (unfiltered)."
  (let ((all '()))
    (when (and (boundp 'shipit--notification-pr-activities)
               shipit--notification-pr-activities)
      (maphash (lambda (_k v) (push v all))
               shipit--notification-pr-activities))
    all))

(defun shipit-notifications-buffer--matches-repo-p (activity)
  "Return non-nil if ACTIVITY passes the repo allow-list and exclusion list.
- Allow-list (`--selected-repo'): when nil/empty, every repo passes.
  When non-empty, the activity's repo must match at least one entry.
- Deny-list (`--excluded-repos'): when the activity's repo matches an
  entry, hide regardless of the allow-list.
Comparison is case-insensitive."
  (let ((allow shipit-notifications-buffer--selected-repo)
        (deny shipit-notifications-buffer--excluded-repos)
        (repo (cdr (assq 'repo activity))))
    (and (or (null allow)
             (and (stringp repo)
                  (cl-some (lambda (a)
                             (and (stringp a)
                                  (string-equal-ignore-case repo a)))
                           allow)))
         (not (and (stringp repo)
                   (cl-some (lambda (d)
                              (and (stringp d)
                                   (string-equal-ignore-case repo d)))
                            deny))))))

(defun shipit-notifications-buffer--matches-type-p (activity)
  "Return non-nil if ACTIVITY passes the type allow- and deny-lists."
  (let ((allow shipit-notifications-buffer--selected-type)
        (deny shipit-notifications-buffer--excluded-types)
        (ty (cdr (assq 'type activity))))
    (and (or (null allow) (and ty (member ty allow)))
         (not (and ty (member ty deny))))))

(defun shipit-notifications-buffer--matches-reason-p (activity)
  "Return non-nil if ACTIVITY passes the reason allow- and deny-lists."
  (let ((allow shipit-notifications-buffer--selected-reason)
        (deny shipit-notifications-buffer--excluded-reasons)
        (r (cdr (assq 'reason activity))))
    (and (or (null allow) (and r (member r allow)))
         (not (and r (member r deny))))))

(defun shipit-notifications-buffer--matches-jira-component-p (activity)
  "Return non-nil if ACTIVITY passes the Jira component allow/deny lists.
Non-Jira activities pass through (no components to match).  Jira
activities pass only if (a) the allow-list is empty or the
issue's components include one of the allowed ones, AND (b) none
of the issue's components is in the deny-list.
Comparison is case-insensitive."
  (let ((allow shipit-notifications-buffer--selected-jira-component)
        (deny shipit-notifications-buffer--excluded-jira-components))
    (or (not (eq (cdr (assq 'source activity)) 'jira))
        (and (null allow) (null deny))
        (let ((comps (cdr (assq 'jira-components activity))))
          (and (or (null allow)
                   (cl-some (lambda (c)
                              (and (stringp c)
                                   (cl-some
                                    (lambda (a)
                                      (and (stringp a)
                                           (string-equal-ignore-case c a)))
                                    allow)))
                            comps))
               (not (and comps
                         (cl-some
                          (lambda (c)
                            (and (stringp c)
                                 (cl-some
                                  (lambda (d)
                                    (and (stringp d)
                                         (string-equal-ignore-case c d)))
                                  deny)))
                          comps))))))))

(defun shipit-notifications-buffer--matches-actionable-filter-p (activity)
  "Return non-nil if ACTIVITY passes the actionable filter.
Mode is read from `shipit-notifications-buffer--actionable-only':
nil = pass everything, t = pass only actionable, `non-actionable' =
pass only non-actionable rows."
  (let ((mode shipit-notifications-buffer--actionable-only))
    (cond
     ((null mode) t)
     (t
      (let* ((r (cdr (assq 'reason activity)))
             (actionable (and (stringp r)
                              (member r shipit-notifications-actionable-reasons))))
        (if (eq mode 'non-actionable)
            (not actionable)
          actionable))))))

(defun shipit-notifications-buffer--matches-state-p (activity)
  "Return non-nil if ACTIVITY passes the state allow- and deny-lists.
State values are `open' / `merged' / `closed' / `draft'; non-PR
activities pass through only when both lists are empty (any
selection or exclusion implicitly hides non-PR rows because they
have no PR state to compare against)."
  (let ((allow shipit-notifications-buffer--selected-state)
        (deny shipit-notifications-buffer--excluded-states))
    (cond
     ((and (null allow) (null deny)) t)
     ((not (equal (cdr (assq 'type activity)) "pr")) nil)
     (t
      (let* ((state (cdr (assq 'pr-state activity)))
             (draft (cdr (assq 'draft activity)))
             (effective (cond
                         ((or (null state)
                              (and (stringp state) (string-empty-p state)))
                          nil)
                         ((and (equal state "open") draft) "draft")
                         ((equal state "open") "open")
                         (t state))))
        (and effective
             (or (null allow) (member effective allow))
             (not (member effective deny))))))))

(defun shipit-notifications-buffer--repo-filtered-activities ()
  "Activities after the structural (repo + type + state) filters.
Reuses `shipit-notifications-buffer--render-pool' when set so the
hash walk + filters happen once per render instead of three times
(header shown-count, header loaded-count, list renderer)."
  (or shipit-notifications-buffer--render-pool
      (seq-filter (lambda (a)
                    (and (shipit-notifications-buffer--matches-snooze-p a)
                         (shipit-notifications-buffer--matches-repo-p a)
                         (shipit-notifications-buffer--matches-type-p a)
                         (shipit-notifications-buffer--matches-state-p a)
                         (shipit-notifications-buffer--matches-reason-p a)
                         (shipit-notifications-buffer--matches-actionable-filter-p a)))
                  (shipit-notifications-buffer--all-activities))))

(defun shipit-notifications-buffer--loaded-count ()
  "Return the number of activities visible under the repo filter.
Ignores the text filter — this is the denominator when a text
filter is active, so the user sees `<matches>/<loaded>'."
  (length (shipit-notifications-buffer--repo-filtered-activities)))

(defun shipit-notifications-buffer--shown-count ()
  "Return the number of activities currently rendered in the buffer.
Applies both the repo filter and the text filter."
  (let ((pool (shipit-notifications-buffer--repo-filtered-activities)))
    (if (string-empty-p shipit-notifications-buffer--filter-text)
        (length pool)
      (length (seq-filter #'shipit-notifications-buffer--matches-filter-p pool)))))

(defun shipit-notifications-buffer--pending-mark-count ()
  "Return the count of notifications locally marked but not yet on the server.
Only meaningful for the `unread' scope where the local cache holds the
entire unread set — for the paginated `all' scope, total > loaded just
means there are more pages, not that anything is pending sync."
  (when (eq shipit-notifications-buffer--display-scope 'unread)
    (let ((total shipit-notifications-buffer--total-count)
          (loaded (length (shipit-notifications-buffer--all-activities))))
      (if (and total (> total loaded)) (- total loaded) 0))))

(defun shipit-notifications-buffer--insert-header ()
  "Insert the buffer header, including scope, page info, and count.
Fraction semantics: when a text filter is active the denominator is
the number of activities loaded in the buffer (so the user sees how
many local items match); otherwise the denominator is the
probe-derived server total for the current scope, when known."
  (insert (propertize "Notifications" 'font-lock-face 'bold))
  (shipit-notifications-buffer--insert-meta-bracket)
  (shipit-notifications-buffer--insert-filter-bracket
   "repo" shipit-notifications-buffer--selected-repo nil)
  (shipit-notifications-buffer--insert-filter-bracket
   "type" shipit-notifications-buffer--selected-type nil)
  (shipit-notifications-buffer--insert-filter-bracket
   "reason" shipit-notifications-buffer--selected-reason 'font-lock-keyword-face)
  (shipit-notifications-buffer--insert-filter-bracket
   "component" shipit-notifications-buffer--selected-jira-component 'font-lock-keyword-face)
  (shipit-notifications-buffer--insert-filter-bracket
   "state" shipit-notifications-buffer--selected-state 'font-lock-keyword-face)
  (when shipit-notifications-buffer--actionable-only
    (shipit-notifications-buffer--insert-filter-bracket
     "actionable"
     (if (eq shipit-notifications-buffer--actionable-only 'non-actionable)
         "inverse"
       "on")
     'font-lock-keyword-face))
  (shipit-notifications-buffer--insert-filter-bracket
   "before" shipit-notifications-buffer--before-filter 'shipit-timestamp-face)
  (shipit-notifications-buffer--insert-filter-bracket
   "since" shipit-notifications-buffer--since-filter 'shipit-timestamp-face)
  (unless (string-empty-p shipit-notifications-buffer--filter-text)
    (shipit-notifications-buffer--insert-filter-bracket
     "filter" shipit-notifications-buffer--filter-text nil))
  (when shipit-notifications-buffer--snoozed-items
    (shipit-notifications-buffer--insert-filter-bracket
     "snoozed"
     (number-to-string (length shipit-notifications-buffer--snoozed-items))
     'font-lock-keyword-face))
  (insert "\n\n"))

(defun shipit-notifications-buffer--insert-filter-bracket (label value face)
  "Insert a `  [LABEL: VALUE]' bracket when VALUE is non-nil/non-empty.
LABEL and brackets render in `font-lock-comment-face'; VALUE
renders in FACE if non-nil, else also in `font-lock-comment-face'."
  (when (and value (not (and (stringp value) (string-empty-p value))))
    (insert (propertize (format "  [%s: " label)
                        'font-lock-face 'font-lock-comment-face))
    (insert (propertize (format "%s" value)
                        'font-lock-face (or face 'font-lock-comment-face)))
    (insert (propertize "]" 'font-lock-face 'font-lock-comment-face))))

(defun shipit-notifications-buffer--insert-meta-bracket ()
  "Insert the `  [scope, page: X/Y, shown/total]' header bracket.
Page numbers render in `font-lock-constant-face'; the rest stays
in `font-lock-comment-face' so only the active value pops."
  (let* ((shown (shipit-notifications-buffer--shown-count))
         (loaded (shipit-notifications-buffer--loaded-count))
         (fetched (length (shipit-notifications-buffer--all-activities)))
         (total shipit-notifications-buffer--total-count)
         (text-filter-active (not (string-empty-p
                                   shipit-notifications-buffer--filter-text)))
         ;; Any client-side selector/filter that narrows the visible set
         ;; below what was fetched on this page.  When this is non-nil the
         ;; per-page count is no longer "50 of 50 fetched" so we annotate
         ;; the header with the breakdown.
         (client-narrowed (< shown fetched))
         (per-page 50)
         (total-pages (when total (max 1 (ceiling (/ (float total) per-page)))))
         ;; In `all' scope, show the absolute server-side range of items
         ;; we're viewing (e.g. `[2201-2250]/2819') so the user can tell
         ;; where the current page sits in the full feed.
         (range-part
          (when (and total
                     (not text-filter-active)
                     (eq shipit-notifications-buffer--display-scope 'all)
                     (> fetched 0))
            (let* ((page-start (1+ (* per-page
                                      (1- shipit-notifications-buffer--current-page))))
                   (page-end (min total (+ page-start fetched -1))))
              (format ", [%d-%d]/%d" page-start page-end total))))
         (narrow-suffix
          (when (and client-narrowed range-part)
            (format " (%d shown after filters)" shown)))
         (count-part (cond
                      (range-part (concat range-part (or narrow-suffix "")))
                      ((and text-filter-active total)
                       (format ", %d/%d (of %d)" shown loaded total))
                      (text-filter-active (format ", %d/%d" shown loaded))
                      (total (format ", %d/%d" shown total))
                      (t (format ", %d shown" shown))))
         (cmt 'font-lock-comment-face))
    (insert (propertize "  [" 'font-lock-face cmt))
    (insert (propertize (symbol-name shipit-notifications-buffer--display-scope)
                        'font-lock-face cmt))
    (when (eq shipit-notifications-buffer--display-scope 'all)
      (insert (propertize ", page: " 'font-lock-face cmt))
      (insert (propertize
               (if total-pages
                   (format "%d/%d"
                           shipit-notifications-buffer--current-page
                           total-pages)
                 (format "%d" shipit-notifications-buffer--current-page))
               'font-lock-face 'font-lock-constant-face)))
    (let ((pending (shipit-notifications-buffer--pending-mark-count)))
      (insert (propertize count-part 'font-lock-face cmt))
      (when (and pending (> pending 0))
        (insert (propertize (format ", %d pending refresh" pending)
                            'font-lock-face cmt))))
    (insert (propertize "]" 'font-lock-face cmt))))

(defun shipit-notifications-buffer--insert-notifications ()
  "Insert all notification entries as magit sections.
Applies the repo filter and then the text filter, using the same
helpers as the header counts so the two stay in sync.  Honors
`shipit-notifications-buffer--group-by-repo': when on, entries are
nested under per-repo wrapper sections."
  (require 'shipit-notifications)
  (let ((activities (shipit-notifications-buffer--repo-filtered-activities)))
    (setq activities (sort activities
                           (lambda (a b)
                             (string> (or (cdr (assq 'updated-at a)) "")
                                      (or (cdr (assq 'updated-at b)) "")))))
    (when (not (string-empty-p shipit-notifications-buffer--filter-text))
      (setq activities (seq-filter
                        #'shipit-notifications-buffer--matches-filter-p
                        activities)))
    (cond
     ((null activities)
      (insert (propertize "  No notifications\n"
                          'font-lock-face 'font-lock-comment-face)))
     (shipit-notifications-buffer--group-by-repo
      (shipit-notifications-buffer--insert-grouped-by-repo activities))
     (t
      (dolist (activity activities)
        (shipit-notifications-buffer--insert-notification activity))))
    ;; Snoozed-group footer (unread scope only).  Shows what the user
    ;; deferred so they can see + unsnooze without the `Z' completion
    ;; round-trip.  Collapsed by default.
    (when (and (eq shipit-notifications-buffer--display-scope 'unread)
               shipit-notifications-buffer--snoozed-items)
      (shipit-notifications-buffer--insert-snoozed-group))))

(defun shipit-notifications-buffer--insert-snoozed-group ()
  "Insert a collapsed `notification-snoozed-group' section listing snoozes.
Each row uses the standard `notification-entry' format so the
existing keymap (RET, `m m', `z' to unsnooze, ...) keeps working;
the heading shows a remaining-time annotation per row.  Activities
whose key has expired but not yet been pruned are skipped."
  (let* ((live (shipit-notifications-buffer--prune-expired-snoozes))
         (snoozed-keys (mapcar #'car live))
         (rows (when snoozed-keys
                 (let ((all (shipit-notifications-buffer--all-activities))
                       (matches '()))
                   (dolist (a all)
                     (let* ((repo (cdr (assq 'repo a)))
                            (type (or (cdr (assq 'type a)) "pr"))
                            (number (or (cdr (assq 'number a))
                                        (cdr (assq 'pr-number a))))
                            (key (and repo number
                                      (format "%s:%s:%s" repo type number))))
                       (when (member key snoozed-keys)
                         (push a matches))))
                   (nreverse matches)))))
    (when rows
      (insert "\n")
      (let ((heading (format "🕓 Snoozed (%d)" (length rows))))
        (magit-insert-section (notification-snoozed-group nil t)
          (magit-insert-heading
            (propertize heading 'font-lock-face 'magit-section-heading))
          (dolist (a rows)
            (shipit-notifications-buffer--insert-notification a)))))))

(defun shipit-notifications-buffer--group-activities-by-repo (activities)
  "Group ACTIVITIES into a list of (REPO . ACTIVITIES) cons cells.
Repos are ordered by the `updated-at' of their newest activity
descending, so the most recently active repo lands first.
ACTIVITIES are assumed to be pre-sorted by updated-at desc, so
within each group the order is preserved."
  (let ((by-repo (make-hash-table :test 'equal))
        (repo-order '()))
    (dolist (a activities)
      (let* ((repo (or (cdr (assq 'repo a)) "<unknown>")))
        (unless (gethash repo by-repo)
          (push repo repo-order))
        (push a (gethash repo by-repo))))
    ;; Reverse each bucket since we pushed in reverse.
    (mapcar (lambda (repo)
              (cons repo (nreverse (gethash repo by-repo))))
            ;; Repos are walked in order of first appearance in the
            ;; pre-sorted ACTIVITIES list, which equals
            ;; "newest-activity-first" — so just reverse the push
            ;; order.
            (nreverse repo-order))))

(defun shipit-notifications-buffer--insert-grouped-by-repo (activities)
  "Render ACTIVITIES nested under per-repo magit-sections.
Each `notification-repo' wrapper carries the repo string as its
section value so callers can identify it later."
  (dolist (cell (shipit-notifications-buffer--group-activities-by-repo
                 activities))
    (let* ((repo (car cell))
           (entries (cdr cell))
           (count (length entries))
           (heading (format "%s  (%d)"
                            (propertize repo 'font-lock-face
                                        'font-lock-constant-face)
                            count)))
      (magit-insert-section (notification-repo repo)
        (magit-insert-heading heading)
        (dolist (a entries)
          (shipit-notifications-buffer--insert-notification a))))))

(defun shipit-notifications-buffer--matches-filter-p (activity)
  "Check if ACTIVITY matches the current filter."
  (let ((filter (downcase shipit-notifications-buffer--filter-text))
        (repo (downcase (or (cdr (assq 'repo activity)) "")))
        (subject (downcase (or (cdr (assq 'subject activity)) "")))
        (reason (downcase (or (cdr (assq 'reason activity)) ""))))
    (or (string-match-p filter repo)
        (string-match-p filter subject)
        (string-match-p filter reason))))

(defun shipit-notifications-buffer--insert-notification (activity)
  "Insert a single notification ACTIVITY as a magit section."
  (let* ((heading (shipit-notifications-buffer--format-heading activity))
         (start (point))
         (sect (magit-insert-section (notification-entry activity)
                 (magit-insert-heading heading))))
    ;; Add text properties on the heading for backward compat with
    ;; shipit--notification-actions which reads data via get-text-property
    (let ((repo (cdr (assq 'repo activity)))
          (number (or (cdr (assq 'number activity))
                      (cdr (assq 'pr-number activity))))
          (type (or (cdr (assq 'type activity)) "pr"))
          (notification (cdr (assq 'notification activity)))
          (source (cdr (assq 'source activity)))
          (browse-url (cdr (assq 'browse-url activity)))
          (backend-id (cdr (assq 'backend-id activity)))
          (backend-config (cdr (assq 'backend-config activity)))
          (heading-end (oref sect content)))
      (add-text-properties start (or heading-end (point))
                           `(shipit-repo ,repo
                             shipit-pr-number ,number
                             shipit-notification-type ,type
                             shipit-notification-source ,source
                             shipit-notification-browse-url ,browse-url
                             shipit-notification-backend-id ,backend-id
                             shipit-notification-backend-config ,backend-config
                             shipit-notification ,notification)))
    (magit-section-hide sect)))

(defun shipit-notifications-buffer--icon-type-for (type is-draft pr-state reason)
  "Return the icon-type key to use for a notification heading.
TYPE is the internal type string.  IS-DRAFT, PR-STATE and REASON
are extracted from the activity alist.  WorkflowRuns with
REASON=\"approval_requested\" map to \"deployment\" so the rocket
icon matches GitHub's web UI for deployment review requests."
  (cond
   ;; Terminal PR states (merged/closed) take precedence over the draft
   ;; flag — `isDraft' stays t on closed-while-draft PRs but the user
   ;; cares about the latest state, not the historical one.
   ((and (string= type "pr") (equal pr-state "merged")) "pr-merged")
   ((and (string= type "pr") (equal pr-state "closed")) "pr-closed")
   ((and (string= type "pr") is-draft) "pr-draft")
   ((and (string= type "workflow") (equal reason "approval_requested"))
    "deployment")
   (t type)))

(defun shipit-notifications-buffer--format-heading (activity)
  "Format the heading line for notification ACTIVITY."
  (let* ((repo (cdr (assq 'repo activity)))
         (number (or (cdr (assq 'number activity))
                     (cdr (assq 'pr-number activity))))
         (type (or (cdr (assq 'type activity)) "pr"))
         (subject (string-trim (or (cdr (assq 'subject activity)) "")))
         (reason (or (cdr (assq 'reason activity)) "unknown"))
         (updated-at (cdr (assq 'updated-at activity)))
         (time-ago (shipit--format-timestamp updated-at))
         (source (cdr (assq 'source activity)))
         (source-icon (shipit--get-notification-source-icon source))
         (is-draft (cdr (assq 'draft activity)))
         (pr-state (cdr (assq 'pr-state activity)))
         (icon-type (shipit-notifications-buffer--icon-type-for
                     type is-draft pr-state reason))
         (type-icon (shipit--get-notification-type-icon
                     icon-type (pcase icon-type
                                 ("pr" "PR")
                                 ("pr-draft" "DR")
                                 ("pr-merged" "MG")
                                 ("pr-closed" "CL")
                                 ("issue" "IS")
                                 ("discussion" "💬")
                                 ("rss" "RS")
                                 ("release" "🚀")
                                 ("deployment" "🚀")
                                 ("check" "CS")
                                 ("commit" "CM")
                                 ("workflow" "WF")
                                 ("alert" "AL")
                                 ("invitation" "IN")
                                 (_ "??"))))
         (repo-width (or (cdr (assq 'repo shipit-notifications-column-widths)) 30))
         (pr-width (or (cdr (assq 'pr shipit-notifications-column-widths)) 5))
         (title-width-cfg (or (cdr (assq 'title shipit-notifications-column-widths)) 45))
         (reason-width (or (cdr (assq 'reason shipit-notifications-column-widths)) 12))
         (spacing (or (bound-and-true-p shipit-notifications-column-spacing) 2))
         (spacer (make-string spacing ?\s))
         (number-str (cond
                      ((member type '("rss" "release" "check" "commit"
                                      "workflow" "alert" "invitation"))
                       "")
                      ((integerp number) (format "#%d" number))
                      (t (format "%s" number))))
         (pr-str (truncate-string-to-width number-str (1+ pr-width) nil ?\s))
         (title-width (max 10 (- title-width-cfg (1+ (string-width source-icon)))))
         (left-part (format "%s %s %s %s%s%s%s%s"
                            source-icon
                            type-icon
                            (propertize (truncate-string-to-width repo repo-width nil ?\s)
                                        'font-lock-face 'font-lock-constant-face)
                            pr-str
                            spacer
                            (propertize
                             (truncate-string-to-width subject title-width nil ?\s)
                             'shipit-notification-title t)
                            spacer
                            (propertize (truncate-string-to-width reason reason-width nil ?\s)
                                        'font-lock-face 'font-lock-keyword-face)))
         (icon-cols 4)
         (fixed-left-width (+ icon-cols 1 repo-width 1 (1+ pr-width)
                              spacing title-width spacing reason-width))
         (window-width (or (window-body-width) 120))
         (time-field-width 16)  ; fixed width to survive format toggles
         (time-text (let* ((ts-width (string-width time-ago))
                           (pad (max 0 (- time-field-width ts-width))))
                      (concat (make-string pad ?\s) time-ago)))
         (padding (max 2 (- window-width fixed-left-width time-field-width 3))))
    (concat left-part
            (make-string padding ?\s)
            (propertize time-text
                        'font-lock-face 'shipit-timestamp-face
                        'shipit-raw-timestamp (get-text-property 0 'shipit-raw-timestamp time-ago)))))

(defun shipit-notifications-buffer--insert-activity-details (activity)
  "Insert expandable details for a PR/Issue/Discussion ACTIVITY."
  (let* ((repo (or (cdr (assq 'repo activity)) ""))
         (number (or (cdr (assq 'number activity))
                     (cdr (assq 'pr-number activity))))
         (type (or (cdr (assq 'type activity)) "pr"))
         (subject (or (cdr (assq 'subject activity)) ""))
         (reason (or (cdr (assq 'reason activity)) ""))
         (updated (cdr (assq 'updated-at activity)))
         (url (cdr (assq 'browse-url activity)))
         (type-label (pcase type
                       ("pr" "Pull Request")
                       ("issue" "Issue")
                       ("discussion" "Discussion")
                       ("release" "Release")
                       ("check" "Check Suite")
                       ("commit" "Commit")
                       ("workflow" "Workflow Run")
                       ("alert" "Security Alert")
                       ("invitation" "Repository Invitation")
                       (_ type)))
         (hide-number-p (member type '("release" "check" "commit" "workflow"
                                       "alert" "invitation" "rss")))
         (number-str (cond
                      (hide-number-p "")
                      ((integerp number) (format "#%d" number))
                      (t (format "%s" number)))))
    (insert "    " (propertize type-label 'font-lock-face 'bold)
            (if (string-empty-p number-str) "" (concat " " number-str))
            " in " repo "\n")
    (insert "    " subject "\n")
    (let ((author (or (cdr (assq 'author activity))
                      (cdr (assq 'pr-author activity)))))
      (when author
        (insert "    Author: "
                (propertize author 'font-lock-face 'shipit-username-face)
                "\n")))
    (when (and reason (not (string-empty-p reason)))
      (insert "    Reason: " (propertize reason 'font-lock-face 'font-lock-keyword-face) "\n"))
    (let ((components (cdr (assq 'jira-components activity))))
      (when components
        (insert "    Component: "
                (mapconcat (lambda (n)
                             (propertize n 'font-lock-face 'font-lock-constant-face))
                           components ", ")
                "\n")))
    (when updated
      (insert "    Updated: " (propertize (shipit--format-timestamp updated)
                                          'font-lock-face 'shipit-timestamp-face) "\n"))
    (when url
      (insert "    " (propertize url 'font-lock-face 'link) "\n"))
    (when (eq (cdr (assq 'source activity)) 'jira)
      (shipit-notifications-buffer--insert-jira-last-activity activity))))

(defun shipit-notifications-buffer--format-last-activity (last)
  "Format LAST as `<author> <verb>: <summary>' (no timestamp).
Caller is responsible for right-aligning the timestamp column.
Returns nil when LAST is nil so callers can substitute their own
empty-state placeholder."
  (when last
    (let* ((author (or (plist-get last :author) "Unknown"))
           (summary (or (plist-get last :summary) ""))
           (verb (pcase (plist-get last :type)
                   ('comment "commented")
                   ('change "changed")
                   (_ "did"))))
      (concat (propertize author 'font-lock-face 'shipit-username-face)
              " " verb
              (if (string-empty-p summary) "" (concat ": " summary))))))

(defun shipit-notifications-buffer--insert-aligned-row (indent left timestamp)
  "Insert a single row at the current point with TIMESTAMP right-aligned.
INDENT is the leading whitespace string, LEFT is the propertized
content for the left column, TIMESTAMP is an ISO string or nil.
Layout matches the PR Activity section: indent + truncated-left +
1 space + 16-char right-aligned timestamp + 3-char magit reserve."
  (let* ((indent-width (length indent))
         (window-width (- (max 80 (or (window-width) 80))
                          (or left-margin 0)))
         (timestamp-width 16)
         (magit-reserve 3)
         (timestamp-col (- window-width timestamp-width magit-reserve 1))
         (left-max-width (max 20 (- timestamp-col indent-width)))
         (truncated (truncate-string-to-width (or left "") left-max-width))
         (padded-left
          (let* ((w (string-width truncated))
                 (pad (max 0 (- left-max-width w))))
            (concat truncated (make-string pad ?\s))))
         (ts-text (when timestamp
                    (substring-no-properties (shipit--format-timestamp timestamp))))
         (padded-ts
          (if ts-text
              (let* ((tw (string-width ts-text))
                     (pad (max 0 (- timestamp-width tw))))
                (propertize (concat (make-string pad ?\s) ts-text)
                            'font-lock-face 'shipit-timestamp-face))
            (make-string timestamp-width ?\s))))
    (insert indent padded-left " " padded-ts "\n")))

(defun shipit-notifications-buffer--insert-jira-last-activity (activity)
  "Insert a `notification-jira-last-activity' section and async-fill its body.
Heading mirrors the PR activity section so the look stays
consistent (icon + Activity label + indented body).  Spawns one
HTTP request and replaces the placeholder body when the response
arrives.  Stale markers (from buffer re-renders) are tolerated."
  (require 'shipit-issue-jira)
  (let* ((key (cdr (assq 'number activity)))
         (config (cdr (assq 'backend-config activity)))
         (buf (current-buffer))
         (heading-indent 4)
         (icon (shipit--get-pr-field-icon "activity" "\U0001f4a5"))
         (heading (format "%s%s Activity"
                          (make-string heading-indent ?\s) icon))
         (body-indent (make-string (+ heading-indent 3) ?\s))
         (sect (magit-insert-section (notification-jira-last-activity activity)
                 (magit-insert-heading heading)
                 (shipit-notifications-buffer--insert-aligned-row
                  body-indent
                  (propertize "loading..."
                              'font-lock-face 'font-lock-comment-face)
                  nil))))
    (shipit--debug-log "Last-activity dispatch: key=%s base-url=%s fbound=%s"
                       key
                       (and config (plist-get config :base-url))
                       (fboundp 'shipit-issue-jira--fetch-last-activity-async))
    (cond
     ((not (and key config))
      (shipit--debug-log "Last-activity dispatch: skipping -- missing %s"
                         (if (not key) "key" "config")))
     ((not (fboundp 'shipit-issue-jira--fetch-last-activity-async))
      (shipit--debug-log "Last-activity dispatch: fetcher not bound"))
     (t
      (condition-case err
          (shipit-issue-jira--fetch-last-activity-async
           config key
           (lambda (last)
             (shipit--debug-log "Last-activity callback for %s: type=%s"
                                key (plist-get last :type))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (condition-case inner
                     (shipit-notifications-buffer--replace-jira-last-activity-body
                      sect body-indent last)
                   (error
                    (shipit--debug-log "Last-activity render failed: %S" inner)))))))
        (error
         (shipit--debug-log "Last-activity dispatch raised: %S" err)))))))

(defun shipit-notifications-buffer--replace-jira-last-activity-body (section indent last)
  "Replace SECTION's body with the formatted LAST activity.
INDENT is the body indent used at insertion time so the
replacement matches the surrounding column layout.  Renders the
timestamp right-aligned with `shipit-timestamp-face' so the row
matches the PR activity section."
  (let* ((content (oref section content))
         (end (oref section end))
         (left (or (shipit-notifications-buffer--format-last-activity last)
                   (propertize "(no activity)"
                               'font-lock-face 'font-lock-comment-face)))
         (timestamp (and last (plist-get last :timestamp))))
    (when (and content end (markerp content) (markerp end)
               (marker-position content) (marker-position end))
      (let ((inhibit-read-only t)
            (content-pos (marker-position content)))
        (save-excursion
          (delete-region content-pos (marker-position end))
          (goto-char content-pos)
          (shipit-notifications-buffer--insert-aligned-row
           indent left timestamp)
          (oset section end (point-marker)))))))

(defun shipit-notifications-buffer--insert-description (description)
  "Insert DESCRIPTION as expandable body content.
Strips HTML tags and renders as plain text."
  (let ((text (shipit-notifications-buffer--html-to-text description)))
    (dolist (line (split-string text "\n" t))
      (insert "    " (string-trim line) "\n"))))

(defun shipit-notifications-buffer--html-to-text (html)
  "Convert HTML string to plain text."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    ;; <br> and <br/> → newline
    (while (re-search-forward "<br\\s-*/?>" nil t)
      (replace-match "\n"))
    ;; <p>...</p> → content + blank line
    (goto-char (point-min))
    (while (re-search-forward "</p>" nil t)
      (replace-match "\n"))
    ;; <strong>...</strong> → bold markers
    (goto-char (point-min))
    (while (re-search-forward "<strong>\\(.*?\\)</strong>" nil t)
      (replace-match (propertize (match-string 1) 'font-lock-face 'bold)))
    ;; Strip remaining tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match ""))
    ;; Decode HTML entities
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t) (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">"))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t) (replace-match "\""))
    (string-trim (buffer-string))))

;;; Section toggle with lazy loading

(defun shipit-notifications-buffer--reinforce-invisibility-overlays (section)
  "Add a `display' override to SECTION's invisibility overlays.
Text with the `display' text property (e.g. SVG icons rendered via
`(propertize \" \" \='display IMAGE)\=') is shown by Emacs even when
the underlying text has `invisible' set — the display spec wins.
That makes per-entry icons leak through a hidden parent (visible
as a single leftover icon at the start of where the row used to be).

Walk overlays inside SECTION and, on each `magit-section\='
invisibility overlay, set `display \"\"\=' so the overlay\='s display
spec wins over any text-property image specs.  Also set a high
`priority' so the overlay decisively wins against competing
overlays."
  (let ((beg (oref section content))
        (end (oref section end)))
    (when (and beg end (markerp beg) (markerp end))
      (dolist (ov (overlays-in (marker-position beg) (marker-position end)))
        (when (eq (overlay-get ov 'invisible) t)
          (overlay-put ov 'display "")
          (overlay-put ov 'priority 200))))))

(defun shipit-notifications-buffer--toggle-with-icon-fix (section)
  "Toggle SECTION via `magit-section-toggle' and reinforce overlays.
After collapsing, we override any text-property `display' specs
inside the body (SVG icons) so they do not leak past the
invisibility overlay."
  (magit-section-toggle section)
  (when (slot-value section 'hidden)
    (shipit-notifications-buffer--reinforce-invisibility-overlays section)))

(defun shipit-notifications-buffer-toggle-section ()
  "Toggle notification section, lazy-loading details on first expand."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ;; Child sub-sections — just toggle
     ((and section (memq (oref section type)
                         '(notification-activity notification-description)))
      (magit-section-toggle section))
     ;; Notification entry — lazy-load on first expand
     ((and section (eq (oref section type) 'notification-entry))
      (if (shipit-notifications-buffer--section-has-content-p section)
          (shipit-notifications-buffer--toggle-with-icon-fix section)
        (let ((activity (oref section value)))
          (shipit-notifications-buffer--load-section-content section activity))))
     ;; Repo wrapper (group-by-repo) — toggle and reinforce so icons in
     ;; entry rows don't leak through invisibility.
     ((and section (eq (oref section type) 'notification-repo))
      (shipit-notifications-buffer--toggle-with-icon-fix section))
     ;; Other sections — default toggle (but not root)
     (t (when (and section (oref section parent))
          (magit-section-toggle section))))))

(defun shipit-notifications-buffer--section-has-content-p (section)
  "Return non-nil if SECTION has body content (children or text)."
  (let ((content-pos (oref section content))
        (end-pos (oref section end)))
    (and content-pos end-pos
         (> (marker-position end-pos) (marker-position content-pos)))))

(defun shipit-notifications-buffer--load-section-content (section activity)
  "Load and insert content for notification SECTION from ACTIVITY."
  (let* ((type (or (cdr (assq 'type activity)) "pr"))
         (description (cdr (assq 'description activity))))
    (if (and (string= type "rss")
             (stringp description)
             (not (string-empty-p description)))
        ;; RSS — insert description synchronously (already have it)
        (shipit-notifications-buffer--insert-content-into-section
         section
         (lambda () (shipit-notifications-buffer--insert-description description)))
      ;; PR/Issue/Discussion — fetch details async then render
      (shipit-notifications-buffer--fetch-and-insert-details section activity))))

(defun shipit-notifications-buffer--extend-parent-ends (section new-pos)
  "Walk parent chain from SECTION and extend each parent's end to NEW-POS.
Needed when SECTION is nested (e.g., grouped-by-repo) and we just
inserted body content past the parent's existing end marker.

Two cases are handled:

1. Parent is shown — extending the end marker is enough; a future
   collapse will create a fresh invisibility overlay covering the
   updated range.

2. Parent is *already collapsed* when the new content arrives (typical
   for async paths: user expands an entry, collapses the wrapper, then
   the timeline-events callback fires).  The existing invisibility
   overlay was created with the old end position, so without
   extending it the new content would visually leak past the wrapper.
   Walk overlays at the parent's `content' position, find the
   `magit-section' invisibility overlay, and move its end to NEW-POS."
  (let ((parent (oref section parent)))
    (while parent
      (let ((pend (oref parent end)))
        (when (and pend (markerp pend)
                   (< (marker-position pend) new-pos))
          (oset parent end (copy-marker new-pos))
          (when (slot-value parent 'hidden)
            (let ((cstart (oref parent content)))
              (when (and cstart (markerp cstart))
                (dolist (ov (overlays-at (marker-position cstart)))
                  (when (eq (overlay-get ov 'invisible) t)
                    (move-overlay ov (overlay-start ov) new-pos))))))))
      (setq parent (oref parent parent)))))

(defun shipit-notifications-buffer--insert-content-into-section (section insert-fn)
  "Insert content into SECTION by calling INSERT-FN, then show it.
Also extends ancestor sections' end markers when the new content
runs past them, so collapsing a parent (e.g., a notification-repo
wrapper when group-by-repo is on) properly hides the expanded body."
  (let* ((inhibit-read-only t)
         (saved-pos (point))
         (content-pos (oref section content))
         (end-pos (oref section end)))
    (when (and content-pos end-pos)
      (save-excursion
        (goto-char content-pos)
        (let ((magit-insert-section--parent section))
          (funcall insert-fn))
        (oset section end (point-marker))
        (shipit-notifications-buffer--extend-parent-ends section (point)))
      (oset section hidden nil)
      (goto-char saved-pos))))

(defun shipit-notifications-buffer--fetch-and-insert-details (section activity)
  "Fetch PR/Issue details for ACTIVITY and insert into SECTION."
  (let* ((repo (cdr (assq 'repo activity)))
         (number (or (cdr (assq 'number activity))
                     (cdr (assq 'pr-number activity))))
         (type (or (cdr (assq 'type activity)) "pr"))
         (updated-at (cdr (assq 'updated-at activity))))
    ;; Show basic details immediately
    (shipit-notifications-buffer--insert-content-into-section
     section
     (lambda () (shipit-notifications-buffer--insert-activity-details activity)))
    ;; PR-only: fetch rich PR metadata (labels, branch, author, draft state)
    (when (and (string= type "pr") repo (integerp number))
      (condition-case err
          (let* ((resolved (shipit-pr--resolve-for-repo repo))
                 (backend (car resolved))
                 (config (cdr resolved))
                 (fetch-fn (plist-get backend :fetch-pr)))
            (when fetch-fn
              (message "Fetching PR details...")
              (let ((pr-data (funcall fetch-fn config number)))
                (when pr-data
                  (shipit-notifications-buffer--update-pr-details section pr-data)))
              (message nil)))
        (error
         (shipit--debug-log "Failed to fetch PR details: %s" (error-message-string err)))))
    ;; PR and Issue: fetch activity timeline async and insert events
    ;; since the notification was triggered.
    (when (and (or (string= type "pr") (string= type "issue"))
               repo (integerp number) updated-at)
      (condition-case err
          (let ((buf (current-buffer))
                (sect section))
            (shipit--fetch-recent-timeline-events-async
             repo number
             (lambda (events)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((inhibit-read-only t))
                     (save-excursion
                       (goto-char (marker-position (oref sect end)))
                       (let ((magit-insert-section--parent sect))
                         (shipit-notifications-buffer--insert-activity-section
                          events updated-at repo number))
                       (oset sect end (point-marker))
                       (shipit-notifications-buffer--extend-parent-ends
                        sect (point)))))))))
        (error
         (shipit--debug-log "Failed to fetch timeline events: %s"
                            (error-message-string err)))))))

(defun shipit-notifications-buffer--update-pr-details (section pr-data)
  "Update SECTION with richer PR-DATA."
  (let* ((inhibit-read-only t)
         (content-pos (oref section content))
         (end-pos (oref section end))
         (state (cdr (assq 'state pr-data)))
         (merged (cdr (assq 'merged pr-data)))
         (labels (cdr (assq 'labels pr-data)))
         (head (cdr (assq 'head pr-data)))
         (branch (when head (cdr (assq 'ref head))))
         (user (cdr (assq 'user pr-data)))
         (author (when user (cdr (assq 'login user))))
         (body-text (cdr (assq 'body pr-data)))
         (draft (let ((d (cdr (assq 'draft pr-data))))
                  (and d (not (eq d :json-false))))))
    (when (and content-pos end-pos)
      (save-excursion
        (goto-char (marker-position end-pos))
        (let ((magit-insert-section--parent section))
          (when author
            (let* ((avatar-url (when user (cdr (assq 'avatar_url user))))
                   (avatar (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                    avatar-url (fboundp 'shipit--create-avatar-display))
                               (concat (shipit--create-avatar-display author avatar-url 16) " ")
                             "")))
              (insert "    Author: " avatar
                      (propertize author 'font-lock-face 'shipit-username-face)
                      "\n")))
          (when (and body-text (stringp body-text) (not (string-empty-p body-text)))
            (let ((desc-sect (magit-insert-section (notification-description body-text)
                               (magit-insert-heading "    Description")
                               (let ((body-start (point))
                                     (rendered (shipit--render-body body-text)))
                                 (insert rendered)
                                 (let ((fill-column (- (or (window-body-width) 80) 6)))
                                   (fill-region body-start (point)))
                                 (indent-rigidly body-start (point) 6))
                               (insert "\n"))))
              (magit-section-hide desc-sect)))
          (when state
            (insert "    Status: "
                    (propertize (cond (merged "merged")
                                     ((string= state "closed") "closed")
                                     (draft "draft")
                                     (t "open"))
                                'font-lock-face (cond (merged 'magit-tag-face)
                                            ((string= state "closed") 'error)
                                            (draft 'shadow)
                                            (t 'success)))
                    "\n"))
          (when branch
            (insert "    Branch: "
                    (propertize branch 'font-lock-face 'magit-branch-remote)
                    "\n"))
          (when (and labels (> (length labels) 0))
            (insert "    Labels: "
                    (mapconcat (lambda (l)
                                 (propertize (cdr (assq 'name l))
                                             'font-lock-face 'magit-tag-face))
                               labels ", ")
                    "\n")))
        (oset section end (point-marker))
        (shipit-notifications-buffer--extend-parent-ends section (point))))))

;;; Activity timeline

(defun shipit-notifications-buffer--adjust-cutoff (cutoff-timestamp margin-seconds)
  "Subtract MARGIN-SECONDS from CUTOFF-TIMESTAMP (ISO 8601).
Returns an adjusted ISO 8601 timestamp string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (date-to-time cutoff-timestamp)
                                     margin-seconds)
                      t))

(defun shipit-notifications-buffer--filter-events-since (events cutoff-timestamp)
  "Filter EVENTS to those near or after CUTOFF-TIMESTAMP.
Applies a 5-minute margin before the cutoff to catch events that
triggered the notification.  Uses created_at, submitted_at, or
updated_at from each event."
  (let ((adjusted (shipit-notifications-buffer--adjust-cutoff cutoff-timestamp 300)))
    (seq-filter
     (lambda (event)
       (let ((ts (or (cdr (assq 'created_at event))
                     (cdr (assq 'submitted_at event))
                     (cdr (assq 'updated_at event)))))
         (and ts (not (string< ts adjusted)))))
     events)))

(defun shipit-notifications-buffer--insert-activity-section (events cutoff repo pr-number)
  "Insert a notification-activity sub-section with EVENTS since CUTOFF.
REPO and PR-NUMBER provide context for event rendering.
Caller must bind `magit-insert-section--parent' to the parent section.
Body content is indented 2 columns past the heading start."
  (let* ((filtered (shipit-notifications-buffer--filter-events-since events cutoff))
         (count (length filtered))
         (author-mapping (make-hash-table :test 'equal))
         (heading-indent 4)
         (heading (format "%s%s Activity (%d since notification)"
                          (make-string heading-indent ?\s)
                          (shipit--get-pr-field-icon "activity" "💥")
                          count))
)
    (let ((sect (magit-insert-section (notification-activity filtered)
                  (magit-insert-heading heading)
                  (if (> count 0)
                      (dolist (event filtered)
                        (shipit--insert-activity-event event repo pr-number author-mapping heading-indent))
                    (insert (make-string (+ heading-indent 3) ?\s) "(no recent activity)\n")))))
      sect)))

;;; Actions

(defun shipit-notifications-buffer-copy-url ()
  "Copy the notification URL to the kill ring.
If a region is active, copy the region text instead (standard M-w behavior)."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (let* ((repo (get-text-property (point) 'shipit-repo))
           (number (get-text-property (point) 'shipit-pr-number))
           (stored-url (get-text-property (point) 'shipit-notification-browse-url))
           (type (get-text-property (point) 'shipit-notification-type))
           (url (or stored-url
                    (when (and repo number)
                      (if (string= type "issue")
                          (shipit--browse-issue-url repo number)
                        (shipit--browse-pr-url repo number))))))
      (if url
          (progn
            (kill-new url)
            (message "Copied: %s" url))
        ;; No notification — fall back to global M-w binding
        (let ((fallback (global-key-binding (kbd "M-w"))))
          (when fallback
            (call-interactively fallback)))))))

(defun shipit-notifications-buffer-watch ()
  "Open subscription transient for the repo of the notification at point."
  (interactive)
  (let ((activity (shipit-notifications-buffer--activity-at-point)))
    (if activity
        (let ((repo (cdr (assq 'repo activity))))
          (if repo
              (progn
                (setq-local shipit-repo-buffer-repo repo)
                (shipit-repo-subscription))
            (user-error "No repo context for this notification")))
      (user-error "No notification at point"))))

(defun shipit-notifications-buffer--activity-at-point ()
  "Return the notification activity alist at point, or nil.
Walks up from child sections, then searches backward if needed."
  (let ((section (magit-current-section)))
    ;; Walk up parents first
    (let ((s section))
      (while (and s (not (eq (oref s type) 'notification-entry)))
        (setq s (oref s parent)))
      (when s (setq section s)))
    ;; If not found (plain text in section body), search backward
    (when (or (not section) (eq (oref section type) 'notifications-root))
      (save-excursion
        (while (and (not (bobp))
                    (let ((s (get-text-property (point) 'magit-section)))
                      (not (and s (eq (oref s type) 'notification-entry)))))
          (forward-line -1))
        (let ((s (get-text-property (point) 'magit-section)))
          (when (and s (eq (oref s type) 'notification-entry))
            (setq section s)))))
    (when (and section (eq (oref section type) 'notification-entry))
      (oref section value))))

(defun shipit-notifications-buffer--get-notification-at-point ()
  "Get notification data at point as plist (:repo :pr-number :type :notification)."
  (let ((activity (shipit-notifications-buffer--activity-at-point)))
    (when activity
      (list :repo (cdr (assq 'repo activity))
            :pr-number (or (cdr (assq 'number activity))
                           (cdr (assq 'pr-number activity)))
            :type (or (cdr (assq 'type activity)) "pr")
            :notification (cdr (assq 'notification activity))))))

(defun shipit-notifications-buffer--activity-props-at-point ()
  "Return a navigation props plist if point is on an activity line, else nil.
An activity line carries a `shipit-event-type' text property; the returned
plist has the keys consumed by `shipit-pr--dispatch-activity-navigation'."
  (when-let* ((event-type (get-text-property (point) 'shipit-event-type)))
    (list :event-type event-type
          :comment-id (get-text-property (point) 'shipit-activity-comment-id)
          :commit-sha (get-text-property (point) 'shipit-activity-commit-sha)
          :review-state (get-text-property (point) 'shipit-review-state)
          :crossref-repo (get-text-property (point) 'shipit-crossref-repo)
          :crossref-number (get-text-property (point) 'shipit-crossref-number)
          :crossref-url (get-text-property (point) 'shipit-crossref-url)
          :crossref-title (get-text-property (point) 'shipit-crossref-title)
          :inline-comment-path (get-text-property (point) 'shipit-inline-comment-path))))

(defun shipit-notifications-buffer--required-pr-section (event-type review-state)
  "Return the PR-buffer section that must be rendered to navigate to an activity.
EVENT-TYPE and REVIEW-STATE are the activity props.  Returns nil for events
that don't depend on a single async section (cross-references etc.)."
  (cond
   ((and (string= event-type "reviewed") (string= review-state "approved"))
    'approval)
   ((string= event-type "reviewed") 'activity)
   ((string= event-type "commented") 'general-comments)
   ((string= event-type "line-commented") 'files)
   ((string= event-type "committed") 'commits)
   (t nil)))

(defun shipit-notifications-buffer--schedule-activity-nav (buffer props)
  "In BUFFER, dispatch activity navigation using PROPS once the buffer is ready.
Waits on the specific PR section that holds the target activity (general
comments, files, activity timeline, commits, approval) so navigation
lands the moment that section finishes rendering — not after every async
section has settled.  Falls back to `shipit-issue-buffer-ready-hook' for
issue buffers whose comments are still loading.  Dispatches immediately
when nothing relevant is pending."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((event-type (plist-get props :event-type))
             (review-state (plist-get props :review-state))
             (pr-section (and (boundp 'shipit-buffer--pending-async-sections)
                              (shipit-notifications-buffer--required-pr-section
                               event-type review-state)))
             (issue-pending (and (derived-mode-p 'shipit-issue-mode)
                                 (boundp 'shipit-issue-buffer-ready-hook))))
        (cond
         (pr-section
          (shipit-buffer--on-section-ready
           pr-section
           (lambda () (shipit-pr--dispatch-activity-navigation props))))
         (issue-pending
          (let ((fn-sym (make-symbol "shipit-nav-on-ready")))
            (fset fn-sym
                  (lambda ()
                    (remove-hook 'shipit-issue-buffer-ready-hook fn-sym t)
                    (shipit-pr--dispatch-activity-navigation props)))
            (add-hook 'shipit-issue-buffer-ready-hook fn-sym nil t)))
         (t
          (shipit-pr--dispatch-activity-navigation props)))))))


(defun shipit-notifications-buffer-open ()
  "Open the notification at point directly.
Opens PR/Issue/Discussion buffer, or browser for RSS.
If point is on an activity line within an expanded PR notification,
also navigates to that activity inside the opened PR buffer.
With prefix arg (C-u), show action menu instead."
  (interactive)
  (if current-prefix-arg
      (shipit-notifications-buffer-action)
    (let ((activity (shipit-notifications-buffer--activity-at-point))
          (activity-props (shipit-notifications-buffer--activity-props-at-point)))
      (if (not activity)
          (user-error "No notification at point")
        (let* ((type (or (cdr (assq 'type activity)) "pr"))
               (repo (cdr (assq 'repo activity)))
               (number (or (cdr (assq 'number activity))
                           (cdr (assq 'pr-number activity)))))
          (pcase type
            ("pr"
             (let ((buffer (shipit--open-notification-pr number repo activity)))
               (when (and activity-props (bufferp buffer))
                 (shipit-notifications-buffer--schedule-activity-nav
                  buffer activity-props))))
            ("issue"
             (let ((buffer (shipit--open-notification-issue number repo activity)))
               (when (and activity-props (bufferp buffer))
                 (shipit-notifications-buffer--schedule-activity-nav
                  buffer activity-props))))
            ("discussion" (shipit--open-notification-discussion number repo))
            ("workflow"
             ;; Workflow runs (esp. approval_requested) belong in the
             ;; actions buffer where the user can review/approve from
             ;; Emacs.  The run-id comes from `browse-url' because
             ;; `number' falls back to the notification thread-id
             ;; when GitHub doesn't expose a specific run id, and
             ;; that thread-id is not a valid run-id.
             (let* ((url (cdr (assq 'browse-url activity)))
                    (run-id (and url
                                 (string-match "/runs/\\([0-9]+\\)" url)
                                 (string-to-number (match-string 1 url)))))
               (cond
                ((and repo run-id)
                 (require 'shipit-actions-list)
                 (shipit-open-actions-list repo run-id))
                (url (browse-url url))
                (t (message "No browse URL for workflow run")))))
            ("check"
             (if-let* ((url (cdr (assq 'browse-url activity))))
                 (browse-url url)
               (message "No browse URL for check suite")))
            ("rss" (when-let* ((url (cdr (assq 'browse-url activity))))
                     (browse-url url)))
            (_
             (if-let* ((url (cdr (assq 'browse-url activity))))
                 (browse-url url)
               (message "No browse URL for notification type: %s" type)))))))))

(defun shipit-notifications-buffer-action ()
  "Show action menu for notification at point.
For RSS entries, opens the link in browser directly."
  (interactive)
  (let ((activity (shipit-notifications-buffer--activity-at-point)))
    (if (not activity)
        (user-error "No notification at point")
      (if (eq (cdr (assq 'source activity)) 'rss)
          (shipit-notifications-buffer--rss-action activity)
        (if (fboundp 'shipit--notification-actions)
            (shipit--notification-actions activity)
          (message "Notification actions not available"))))))

(defun shipit-notifications-buffer--rss-action (activity)
  "Handle action for RSS notification ACTIVITY."
  (let* ((url (cdr (assq 'browse-url activity)))
         (section (magit-current-section))
         (actions (list "Open in browser" "Mark as read"))
         (choice (completing-read "RSS action: " actions nil t)))
    (cond
     ((string= choice "Open in browser")
      (when url (browse-url url)))
     ((string= choice "Mark as read")
      (let ((number (cdr (assq 'number activity)))
            (repo (cdr (assq 'repo activity))))
        (when (fboundp 'shipit--mark-notification-read)
          (shipit--mark-notification-read number repo t "rss"))
        (when section
          (let ((inhibit-read-only t))
            (delete-region (oref section start) (oref section end))
            (when-let* ((parent (oref section parent)))
              (oset parent children
                    (delq section (oref parent children)))))))))))

;;; Mark as read

(defun shipit-notifications-buffer-mark-read ()
  "Mark notification at point as read, or all notifications in region."
  (interactive)
  (if (use-region-p)
      (shipit-notifications-buffer--mark-region-read)
    (shipit-notifications-buffer--mark-single-read)))

(defun shipit-notifications-buffer--mark-single-read ()
  "Mark the single notification at point as read and remove the section."
  (let* ((data (shipit-notifications-buffer--get-notification-at-point))
         (activity (shipit-notifications-buffer--activity-at-point))
         ;; Find the notification-entry section (not root)
         (section (let ((s (magit-current-section)))
                    (while (and s (not (eq (oref s type) 'notification-entry)))
                      (setq s (oref s parent)))
                    s)))
    ;; If walk-up failed, try backward search
    (unless section
      (save-excursion
        (while (and (not (bobp))
                    (let ((s (get-text-property (point) 'magit-section)))
                      (not (and s (eq (oref s type) 'notification-entry)))))
          (forward-line -1))
        (setq section (get-text-property (point) 'magit-section))))
    (if data
        (let ((repo (plist-get data :repo))
              (pr-number (plist-get data :pr-number))
              (type (or (plist-get data :type) "pr")))
          (shipit--debug-log "NOTIF-BUFFER: Marking %s %s#%s as read" type repo pr-number)
          (when (fboundp 'shipit--mark-notification-read)
            (shipit--mark-notification-read pr-number repo t type))
          ;; Remove the section in-place instead of full re-render
          (when (and section (eq (oref section type) 'notification-entry))
            (let ((inhibit-read-only t))
              (delete-region (oref section start) (oref section end))
              (when-let* ((parent (oref section parent)))
                (oset parent children
                      (delq section (oref parent children)))))))
      (user-error "No notification at point"))))

(defun shipit-notifications-buffer--mark-region-read ()
  "Mark all notifications in the active region as read."
  (let ((beg (region-beginning))
        (end (region-end))
        (notifications '())
        (seen (make-hash-table :test 'equal)))
    ;; Walk lines in region, collect unique notifications
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((section (get-text-property (point) 'magit-section))
               (type (when section (oref section type))))
          (shipit--debug-log "mark-region: line=%d section-type=%s"
                             (line-number-at-pos) type)
          (when (and section (eq type 'notification-entry))
            (let* ((activity (oref section value))
                   (repo (cdr (assq 'repo activity)))
                   (number (or (cdr (assq 'number activity))
                               (cdr (assq 'pr-number activity))))
                   (ntype (or (cdr (assq 'type activity)) "pr"))
                   (key (format "%s:%s:%s" repo ntype number)))
              (unless (gethash key seen)
                (puthash key t seen)
                (push (list number repo ntype) notifications)))))
        (forward-line 1)))
    (when notifications
      (let ((count (length notifications)))
        (dolist (notif (nreverse notifications))
          (when (fboundp 'shipit--mark-notification-read)
            (shipit--mark-notification-read (car notif) (cadr notif) t (caddr notif))))
        (deactivate-mark)
        (message "Marked %d notifications as read" count)
        (shipit-notifications-buffer--rerender)))))

(defun shipit-notifications-buffer--collect-entries-under (section)
  "Walk SECTION's tree and return (NUMBER REPO TYPE) for every entry.
Recursive so it works whether entries are direct children of the
root (flat list) or nested under `notification-repo' wrappers
(group-by-repo)."
  (let ((acc '()))
    (cl-labels
        ((walk (s)
           (cond
            ((eq (oref s type) 'notification-entry)
             (let ((a (oref s value)))
               (push (list (or (cdr (assq 'number a))
                               (cdr (assq 'pr-number a)))
                           (cdr (assq 'repo a))
                           (or (cdr (assq 'type a)) "pr"))
                     acc)))
            (t
             (dolist (c (oref s children))
               (walk c))))))
      (walk section))
    (nreverse acc)))

(defun shipit-notifications-buffer--containing-repo-section ()
  "Return the `notification-repo' wrapper containing point, or nil."
  (let ((s (magit-current-section)))
    (while (and s (not (eq (oref s type) 'notification-repo)))
      (setq s (oref s parent)))
    s))

(defun shipit-notifications-buffer-mark-all-read ()
  "Mark visible notifications as read.
When point sits inside a `notification-repo' wrapper (only present
in group-by-repo mode), the action scopes to that repo only.
Otherwise it marks every entry in the buffer."
  (interactive)
  (let* ((scope (or (shipit-notifications-buffer--containing-repo-section)
                    (and (bound-and-true-p magit-root-section)
                         magit-root-section)))
         (notifications (and scope
                             (shipit-notifications-buffer--collect-entries-under
                              scope)))
         (scope-suffix (cond
                        ((null scope) "")
                        ((eq scope magit-root-section) "")
                        (t (format " in %s" (oref scope value))))))
    (if notifications
        (progn
          (shipit-notifications-buffer--clear-auto-mark-preview)
          (shipit-notifications-buffer--preview-entries-under scope)
          (redisplay t)
          (unwind-protect
              (when (yes-or-no-p (format "Mark %d notification%s%s as read? "
                                         (length notifications)
                                         (if (= 1 (length notifications)) "" "s")
                                         scope-suffix))
                (dolist (notif notifications)
                  (when (fboundp 'shipit--mark-notification-read)
                    (shipit--mark-notification-read
                     (car notif) (cadr notif) t (caddr notif))))
                (message "Marked %d notification%s%s as read"
                         (length notifications)
                         (if (= 1 (length notifications)) "" "s")
                         scope-suffix)
                (shipit-notifications-buffer-refresh))
            (shipit-notifications-buffer--clear-auto-mark-preview)))
      (message "No notifications to mark as read"))))

(defun shipit-notifications-buffer--collect-resolved-prs ()
  "Return a list of (NUMBER REPO TYPE) tuples for resolved PRs.
Resolved means `pr-state' is `merged' or `closed' — the same set
the GitHub-discussion bookmarklet targets.  Walks the global hash
directly so off-screen / unfetched-page entries are also found."
  (let ((resolved '()))
    (when (and (boundp 'shipit--notification-pr-activities)
               shipit--notification-pr-activities)
      (maphash
       (lambda (_k a)
         (let ((state (cdr (assq 'pr-state a))))
           (when (member state '("merged" "closed"))
             (push (list (or (cdr (assq 'number a))
                             (cdr (assq 'pr-number a)))
                         (cdr (assq 'repo a))
                         (or (cdr (assq 'type a)) "pr"))
                   resolved))))
       shipit--notification-pr-activities))
    resolved))

(defun shipit-notifications-buffer-customize-auto-mark-rules ()
  "Open a customize buffer for `shipit-notifications-auto-mark-read-rules'.
Convenience wrapper so the filter transient can offer a discoverable
entry point to edit rules without dropping into M-x customize."
  (interactive)
  (customize-variable 'shipit-notifications-auto-mark-read-rules))

;;; Auto-mark rules editor

(defface shipit-auto-mark-preview-face
  '((t :inherit font-lock-warning-face :strike-through t))
  "Face for notification rows previewed as auto-mark targets.
Strike-through reinforces the visual that these rows would
disappear once the rule fires."
  :group 'shipit)

(defface shipit-auto-mark-preview-match-face
  '((t :inherit lazy-highlight :weight bold))
  "Face for the actual characters that match the regex in a
row that is being previewed as an auto-mark target.  Inherits
from `lazy-highlight' so the visual cue is familiar from
isearch — but bolded so it pops over the row's strike-through."
  :group 'shipit)

(defvar-local shipit-notifications-buffer--auto-mark-preview-overlays nil
  "Overlays placed on rows that match the regex being previewed.
Buffer-local because preview is per-buffer, and torn down when
the prompt closes (success or abort).")

(defun shipit-notifications-buffer--clear-auto-mark-preview ()
  "Remove every preview overlay from the current buffer."
  (dolist (ov shipit-notifications-buffer--auto-mark-preview-overlays)
    (delete-overlay ov))
  (setq shipit-notifications-buffer--auto-mark-preview-overlays nil))

(defun shipit-notifications-buffer--title-region-in-section (section)
  "Return (BEG . END) for the title text inside SECTION, or nil.
Title cells are marked with a `shipit-notification-title' text
property at render time; this walks the section bounds and
returns the contiguous run carrying that property."
  (let* ((beg (oref section start))
         (end (oref section end))
         (start (text-property-any beg end 'shipit-notification-title t)))
    (when start
      (let ((stop (or (next-single-property-change
                       start 'shipit-notification-title nil end)
                      end)))
        (cons start stop)))))

(defun shipit-notifications-buffer--add-preview-row-overlay (section)
  "Install the strike-through whole-row overlay on SECTION."
  (let ((ov (make-overlay (oref section start) (oref section end))))
    (overlay-put ov 'face 'shipit-auto-mark-preview-face)
    (overlay-put ov 'priority 100)
    (push ov shipit-notifications-buffer--auto-mark-preview-overlays)))

(defun shipit-notifications-buffer--add-preview-match-overlays (section regex)
  "Install per-match character overlays inside SECTION's title region.
Walks the title sub-region only — repo / reason / timestamp text
in the same row stays unhighlighted even if it accidentally
matches REGEX, so the preview reflects what the rule actually
checks (i.e., the activity's `subject')."
  (let ((title (shipit-notifications-buffer--title-region-in-section
                section)))
    (when title
      (save-excursion
        (goto-char (car title))
        (while (re-search-forward regex (cdr title) t)
          ;; Skip empty matches (e.g. a regex like ".*" matching at
          ;; every position) so we don't loop forever or spam zero-
          ;; width overlays.
          (when (> (match-end 0) (match-beginning 0))
            (let ((mov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put mov 'face 'shipit-auto-mark-preview-match-face)
              (overlay-put mov 'priority 110)
              (push mov
                    shipit-notifications-buffer--auto-mark-preview-overlays))))))))

(defun shipit-notifications-buffer--apply-auto-mark-preview (regex)
  "Highlight notification rows whose subject matches REGEX.
Each matching row gets a strike-through overlay; the actual
characters that the regex picks up inside the title also get a
brighter sub-match overlay so the user can see *what* matched.
Clears any previous preview first.  An invalid (mid-typing) regex
is silently skipped — the user will get a fresh preview as soon as
the input parses again, instead of a flurry of error messages."
  (shipit-notifications-buffer--clear-auto-mark-preview)
  (when (and regex
             (stringp regex)
             (not (string-empty-p regex))
             (bound-and-true-p magit-root-section)
             (condition-case nil
                 (progn (string-match-p regex "") t)
               (error nil)))
    (dolist (child (oref magit-root-section children))
      (when (eq (oref child type) 'notification-entry)
        (let* ((activity (oref child value))
               (subj (cdr (assq 'subject activity))))
          (when (and (stringp subj) (string-match-p regex subj))
            (shipit-notifications-buffer--add-preview-row-overlay child)
            (shipit-notifications-buffer--add-preview-match-overlays
             child regex)))))))

(defun shipit-notifications-buffer--apply-auto-mark-rule-preview (rule)
  "Highlight notification rows that RULE (a plist) would match.
Strike-throughs the row and, when RULE includes a `:title' regex,
also highlights the matching characters in the title.  Invalid
mid-typing rules are silently skipped — the preview refreshes once
the input parses again."
  (shipit-notifications-buffer--clear-auto-mark-preview)
  (when (and rule
             (listp rule)
             (keywordp (car-safe rule))
             (zerop (mod (length rule) 2))
             (bound-and-true-p magit-root-section)
             (fboundp 'shipit--auto-mark-rule-matches-activity-p))
    (let ((title-regex (plist-get rule :title)))
      (dolist (child (oref magit-root-section children))
        (when (eq (oref child type) 'notification-entry)
          (let ((activity (oref child value)))
            (when (condition-case nil
                      (shipit--auto-mark-rule-matches-activity-p rule activity)
                    (error nil))
              (shipit-notifications-buffer--add-preview-row-overlay child)
              (when (and (stringp title-regex)
                         (not (string-empty-p title-regex))
                         (condition-case nil
                             (progn (string-match-p title-regex "") t)
                           (error nil)))
                (shipit-notifications-buffer--add-preview-match-overlays
                 child title-regex)))))))))

(defun shipit-notifications-buffer--read-auto-mark-rule-with-preview (default)
  "Read an auto-mark rule from the minibuffer, prefilled with DEFAULT.
Live updates strike-through preview on the notifications buffer for
notifications matching the typed rule.  Returns the input string.
Cleans up overlays on exit (confirm or abort)."
  (let ((original-buffer (current-buffer))
        (timer nil)
        (minibuf nil)
        (last-input nil)
        result)
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq minibuf (current-buffer))
              (add-hook
               'post-command-hook
               (lambda ()
                 (when timer (cancel-timer timer))
                 (setq timer
                       (run-with-idle-timer
                        shipit-notifications-filter-live-delay nil
                        (lambda ()
                          (when (and (buffer-live-p minibuf)
                                     (buffer-live-p original-buffer))
                            (let ((input (with-current-buffer minibuf
                                           (minibuffer-contents-no-properties))))
                              (unless (equal input last-input)
                                (setq last-input input)
                                (let ((parsed (condition-case nil
                                                  (read input)
                                                (error nil))))
                                  (with-current-buffer original-buffer
                                    (shipit-notifications-buffer--apply-auto-mark-rule-preview
                                     parsed))))))))))
               nil t))
          (setq result
                (read-from-minibuffer
                 "Add rule (live preview): "
                 (prin1-to-string default)
                 nil nil
                 'shipit-notifications-buffer--auto-mark-edit-history)))
      (when (buffer-live-p original-buffer)
        (with-current-buffer original-buffer
          (shipit-notifications-buffer--clear-auto-mark-preview))))
    result))

(defun shipit-notifications-buffer--candidate-reasons ()
  "Return the sorted distinct list of reason strings in the hash."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (a (shipit-notifications-buffer--all-activities))
      (let ((r (cdr (assq 'reason a))))
        (when (and r (stringp r) (> (length r) 0))
          (puthash r t seen))))
    (sort (hash-table-keys seen) #'string<)))

(defun shipit-notifications--save-auto-mark-rules (new-list)
  "Set `shipit-notifications-auto-mark-read-rules' to NEW-LIST and persist.
Wraps `customize-save-variable' so tests can stub the persistence
path while still exercising the value update."
  (customize-save-variable
   'shipit-notifications-auto-mark-read-rules new-list))

(defun shipit-notifications-buffer--read-title-regex-with-preview ()
  "Read a regex from the minibuffer with live overlay preview.
Strike-through highlights rows in the notifications buffer that
would be auto-marked, debounced by `shipit-notifications-filter-live-delay'.
Cleans up overlays whether the prompt is confirmed or aborted.

For richer regex editing tooling, see also `re-builder' (built-in)
or the third-party `visual-regexp' package."
  (let ((original-buffer (current-buffer))
        (timer nil)
        (minibuf nil)
        (last-input nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq minibuf (current-buffer))
              (add-hook
               'post-command-hook
               (lambda ()
                 (when timer (cancel-timer timer))
                 (setq timer
                       (run-with-idle-timer
                        shipit-notifications-filter-live-delay nil
                        (lambda ()
                          (when (and (buffer-live-p minibuf)
                                     (buffer-live-p original-buffer))
                            (let ((input (with-current-buffer minibuf
                                           (minibuffer-contents-no-properties))))
                              (unless (equal input last-input)
                                (setq last-input input)
                                (with-current-buffer original-buffer
                                  (shipit-notifications-buffer--apply-auto-mark-preview
                                   input)))))))))
               nil t))
          (read-string "Title regex (live preview): "))
      (when (buffer-live-p original-buffer)
        (with-current-buffer original-buffer
          (shipit-notifications-buffer--clear-auto-mark-preview))))))

(defun shipit-notifications-buffer--read-auto-mark-value (key)
  "Read a value for condition KEY with key-appropriate completion.
Returns the value (correctly typed for the rule plist).  KEY is
one of the auto-mark condition keywords.  For `:not' the value is
itself a single-condition sub-rule read recursively."
  (pcase key
    (:state
     (intern (completing-read
              "State: " '("merged" "closed" "open" "draft") nil t)))
    (:type
     (let ((cands (shipit-notifications-buffer--candidate-types)))
       (completing-read "Type: " cands nil t)))
    (:repo
     (let ((cands (shipit-notifications-buffer--candidate-repos)))
       (completing-read "Repo: " cands nil t)))
    (:reason
     (let ((cands (shipit-notifications-buffer--candidate-reasons)))
       (completing-read "Reason: " cands nil t)))
    (:jira-component
     (let ((cands (shipit-notifications-buffer--candidate-jira-components)))
       (completing-read "Jira component: " cands nil nil)))
    (:title
     (shipit-notifications-buffer--read-title-regex-with-preview))
    (:draft
     (intern (completing-read "Draft: " '("t" "nil") nil t)))
    (:not
     (shipit-notifications-buffer--read-auto-mark-condition
      "Negate condition: " nil))))

(defun shipit-notifications-buffer--read-auto-mark-condition (prompt include-not)
  "Read one (KEY VALUE) condition from the minibuffer and return it as a list.
PROMPT is shown for the key choice.  When INCLUDE-NOT is non-nil,
`:not' is offered as a key option; selecting it recursively reads
an inner condition (no further nesting)."
  (let* ((keys (append '(":state" ":type" ":repo" ":reason"
                         ":jira-component" ":title" ":draft")
                       (and include-not '(":not"))))
         (chosen (completing-read (or prompt "Condition: ") keys nil t))
         (key (intern chosen))
         (value (shipit-notifications-buffer--read-auto-mark-value key)))
    (list key value)))

(defun shipit-notifications-buffer-add-auto-mark-rule ()
  "Add an auto-mark rule via a guided minibuffer flow.
Prompts for one condition at a time and keeps prompting (with a
y-or-n confirmation) until you decline; all collected conditions
end up in the same rule plist and are combined with AND when the
matcher evaluates the rule.

Per-condition keys: `:state', `:type', `:repo', `:reason', `:title',
`:draft', `:not'.  Choosing `:not' recursively prompts for an inner
condition (one level deep), producing a sub-form like
\(:not (:title \"mythos\")) that matches activities where the
inner condition does NOT match.  For `:title' the regex input
shows live overlay preview in the notifications buffer.

Saves the rule and applies it immediately so any matching
notifications are marked as read in this buffer without waiting
for the next poll."
  (interactive)
  (let ((conds nil)
        (more t)
        (n 0))
    (while more
      (cl-incf n)
      (let* ((prompt (if (= n 1) "Condition: "
                       (format "Condition #%d: " n)))
             (cond (shipit-notifications-buffer--read-auto-mark-condition
                    prompt t))
             (k (car cond))
             (v (cadr cond)))
        (when (or v (eq k :draft) (eq k :not))
          (setq conds (append conds cond)))
        (setq more (y-or-n-p "Add another condition (AND)? "))))
    (when conds
      (let ((existing shipit-notifications-auto-mark-read-rules))
        (shipit-notifications--save-auto-mark-rules
         (append existing (list conds)))
        ;; Apply immediately so the user sees the rule take effect
        ;; without waiting for the next poll cycle.
        (let ((n (and (fboundp 'shipit--auto-mark-rules-apply)
                      (shipit--auto-mark-rules-apply))))
          (message "Added auto-mark rule: %S (%s)"
                   conds
                   (cond
                    ((and n (> n 0))
                     (format "marked %d notification%s as read"
                             n (if (= n 1) "" "s")))
                    (n "no notifications matched yet")
                    (t "saved"))))
        (when (fboundp 'shipit--rerender-notifications-buffer-if-visible)
          (shipit--rerender-notifications-buffer-if-visible))
        (when (get-buffer-window "*shipit-auto-mark-rules*" t)
          (shipit-notifications-buffer-list-auto-mark-rules))))))

(defun shipit-notifications-buffer-add-auto-mark-rule-from-point ()
  "Add an auto-mark rule prefilled from the notification at point.
Reads `:repo' from the notification's repo and `:title' from a
regex-escaped version of the subject, then prompts you in the
minibuffer with the rule expression so you can tweak it (loosen the
title regex, swap to `:not', add other conditions) before saving.
Saves, applies immediately, and rerenders."
  (interactive)
  (let* ((activity (shipit-notifications-buffer--activity-at-point)))
    (unless activity
      (user-error "No notification at point"))
    (let* ((repo (cdr (assq 'repo activity)))
           (subject (or (cdr (assq 'subject activity)) ""))
           ;; No regex anchors — `string-match-p' (used by the rule
           ;; matcher) already treats the title regex as a substring,
           ;; and an unanchored default is easier to trim down to a
           ;; prefix or keyword without deleting escape syntax.
           (title-regex (regexp-quote subject))
           (default `(:repo ,repo :title ,title-regex))
           (input (shipit-notifications-buffer--read-auto-mark-rule-with-preview
                   default))
           (parsed (condition-case err
                       (read input)
                     (error
                      (user-error "Cannot parse rule: %s"
                                  (error-message-string err))))))
      (unless (and (listp parsed)
                   (keywordp (car-safe parsed))
                   (zerop (mod (length parsed) 2)))
        (user-error
         "Rule must be a plist like (:repo \"owner/name\" :title \"...\")"))
      (let ((existing shipit-notifications-auto-mark-read-rules))
        (shipit-notifications--save-auto-mark-rules
         (append existing (list parsed)))
        (let ((n (and (fboundp 'shipit--auto-mark-rules-apply)
                      (shipit--auto-mark-rules-apply))))
          (message "Added auto-mark rule: %S (%s)"
                   parsed
                   (cond
                    ((and n (> n 0))
                     (format "marked %d notification%s as read"
                             n (if (= n 1) "" "s")))
                    (n "no notifications matched yet")
                    (t "saved"))))
        (when (fboundp 'shipit--rerender-notifications-buffer-if-visible)
          (shipit--rerender-notifications-buffer-if-visible))
        (when (get-buffer-window "*shipit-auto-mark-rules*" t)
          (shipit-notifications-buffer-list-auto-mark-rules))))))

(defvar shipit-notifications-buffer--auto-mark-edit-history nil
  "Minibuffer history for editing auto-mark rules in-place.")

(defun shipit-notifications-buffer--edit-auto-mark-rule-at (idx)
  "Edit the rule at IDX (0-based) by prompting in the minibuffer.
Pre-fills with the current rule's sexp.  Replaces the entry on save."
  (let* ((rules shipit-notifications-auto-mark-read-rules)
         (current (nth idx rules))
         (input (read-from-minibuffer
                 (format "Edit rule %d: " (1+ idx))
                 (prin1-to-string current)
                 nil nil
                 'shipit-notifications-buffer--auto-mark-edit-history))
         (parsed (condition-case err
                     (read input)
                   (error
                    (user-error "Cannot parse rule: %s"
                                (error-message-string err))))))
    (unless (and (listp parsed)
                 (keywordp (car-safe parsed))
                 (zerop (mod (length parsed) 2)))
      (user-error
       "Rule must be a plist like (:repo \"owner/name\") or (:state merged :draft t)"))
    (let ((new-rules (append (cl-subseq rules 0 idx)
                             (list parsed)
                             (cl-subseq rules (1+ idx)))))
      (shipit-notifications--save-auto-mark-rules new-rules)
      (message "Updated rule %d: %S" (1+ idx) parsed))))

(defun shipit-notifications-buffer-edit-auto-mark-rule-at-point ()
  "Edit the auto-mark rule on the current line of the rules-list buffer."
  (interactive)
  (let ((idx (get-text-property (point) 'shipit-auto-mark-rule-idx)))
    (if idx
        (progn
          (shipit-notifications-buffer--edit-auto-mark-rule-at idx)
          (when (get-buffer "*shipit-auto-mark-rules*")
            (shipit-notifications-buffer-list-auto-mark-rules)))
      (message "No auto-mark rule on this line"))))

(defun shipit-notifications-buffer-edit-auto-mark-rule ()
  "Pick a rule by its current value and edit it via the minibuffer."
  (interactive)
  (let ((rules shipit-notifications-auto-mark-read-rules))
    (if (null rules)
        (message "No auto-mark rules to edit")
      (let* ((labels (mapcar (lambda (r) (format "%S" r)) rules))
             (choice (completing-read "Edit rule: " labels nil t))
             (idx (cl-position choice labels :test #'equal)))
        (when idx
          (shipit-notifications-buffer--edit-auto-mark-rule-at idx)
          (when (get-buffer "*shipit-auto-mark-rules*")
            (shipit-notifications-buffer-list-auto-mark-rules)))))))

(defun shipit-notifications-buffer-delete-auto-mark-rule-at-point ()
  "Delete the auto-mark rule on the current line of the rules-list buffer."
  (interactive)
  (let ((idx (get-text-property (point) 'shipit-auto-mark-rule-idx)))
    (cond
     ((not idx)
      (message "No auto-mark rule on this line"))
     ((yes-or-no-p (format "Remove rule %d? " (1+ idx)))
      (shipit-notifications-buffer--remove-auto-mark-rule-at idx)
      (message "Removed rule %d" (1+ idx))
      (when (get-buffer "*shipit-auto-mark-rules*")
        (shipit-notifications-buffer-list-auto-mark-rules))))))

(defun shipit-notifications-buffer-list-auto-mark-rules ()
  "Show current auto-mark rules in a help-style buffer.
Each rule line carries `shipit-auto-mark-rule-idx' as a text property
so RET/e edits the rule at point and d deletes it.  + adds a new rule."
  (interactive)
  (let ((rules shipit-notifications-auto-mark-read-rules))
    (with-help-window "*shipit-auto-mark-rules*"
      (with-current-buffer "*shipit-auto-mark-rules*"
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Active auto-mark-read rules:\n")
          (insert (propertize
                   "Keys: RET/e edit rule at point, a add, d delete, q quit\n\n"
                   'face 'font-lock-comment-face))
          (if (null rules)
              (insert "  (no rules configured)\n")
            (let ((idx 0))
              (dolist (rule rules)
                (let ((start (point)))
                  (insert (format "  %2d. %S\n" (1+ idx) rule))
                  (add-text-properties
                   start (point)
                   `(shipit-auto-mark-rule-idx ,idx mouse-face highlight)))
                (cl-incf idx)))))))
    ;; with-help-window puts the buffer in help-mode; install our own
    ;; buffer-local key bindings on top so RET / e / d / + work.
    (when (get-buffer "*shipit-auto-mark-rules*")
      (with-current-buffer "*shipit-auto-mark-rules*"
        (use-local-map (copy-keymap (current-local-map)))
        (local-set-key (kbd "RET")
                       #'shipit-notifications-buffer-edit-auto-mark-rule-at-point)
        (local-set-key (kbd "e")
                       #'shipit-notifications-buffer-edit-auto-mark-rule-at-point)
        (local-set-key (kbd "d")
                       #'shipit-notifications-buffer-delete-auto-mark-rule-at-point)
        (local-set-key (kbd "a")
                       #'shipit-notifications-buffer-add-auto-mark-rule)
        (local-set-key (kbd "+")
                       #'shipit-notifications-buffer-add-auto-mark-rule)))))

(defun shipit-notifications-buffer-toggle-auto-mark-rules-list ()
  "Toggle the auto-mark rules-list buffer in another window/frame.
When the list buffer is shown anywhere, dismiss it via `quit-window'
so it works whether `with-help-window' chose a side window, a
separate frame, or the only window of a frame; otherwise re-render
and pop it up.  Re-rendering on each open keeps the view in sync
with rule edits made from the transient."
  (interactive)
  (let ((win (get-buffer-window "*shipit-auto-mark-rules*" t)))
    (if win
        (with-selected-window win (quit-window))
      (shipit-notifications-buffer-list-auto-mark-rules))))

(defun shipit-notifications-buffer--remove-auto-mark-rule-at (idx)
  "Persist the rules list with the entry at IDX removed (0-based)."
  (let* ((rules shipit-notifications-auto-mark-read-rules)
         (new-list (append (cl-subseq rules 0 idx)
                           (cl-subseq rules (1+ idx)))))
    (shipit-notifications--save-auto-mark-rules new-list)))

(defun shipit-notifications-buffer-remove-auto-mark-rule ()
  "Pick a rule by its current value and remove it."
  (interactive)
  (let ((rules shipit-notifications-auto-mark-read-rules))
    (if (null rules)
        (message "No auto-mark rules to remove")
      (let* ((labels (mapcar (lambda (r) (format "%S" r)) rules))
             (choice (completing-read "Remove rule: " labels nil t))
             (idx (cl-position choice labels :test #'equal)))
        (when idx
          (shipit-notifications-buffer--remove-auto-mark-rule-at idx)
          (message "Removed: %s" choice))))))

(defun shipit-notifications-buffer-clear-auto-mark-rules ()
  "Clear every auto-mark rule (with confirmation)."
  (interactive)
  (let ((n (length shipit-notifications-auto-mark-read-rules)))
    (cond
     ((zerop n) (message "No auto-mark rules to clear"))
     ((yes-or-no-p (format "Clear all %d auto-mark rule%s? "
                           n (if (= n 1) "" "s")))
      (shipit-notifications--save-auto-mark-rules nil)
      (message "Auto-mark rules cleared")))))

;;;###autoload (autoload 'shipit-notifications-buffer-mark-menu "shipit-notifications-buffer" nil t)
(transient-define-prefix shipit-notifications-buffer-mark-menu ()
  "Mark notifications read or manage auto-mark rules."
  [["Mark"
    ("m" "Mark this (or region)"
     shipit-notifications-buffer-mark-read)
    ("M" "Mark all in buffer"
     shipit-notifications-buffer-mark-all-read)
    ("Z" "Mark merged/closed as read"
     shipit-notifications-buffer-mark-resolved-read)
    ("A" "Mark actionable as read"
     shipit-notifications-buffer-mark-actionable-read)
    ("N" "Mark non-actionable as read"
     shipit-notifications-buffer-mark-non-actionable-read)]
   ["Auto-mark rules"
    ("+" "Add rule from notification at point"
     shipit-notifications-buffer-add-auto-mark-rule-from-point)
    ("a" "Manage auto-mark rules…"
     shipit-notifications-buffer-auto-mark-menu)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'shipit-notifications-buffer-auto-mark-menu "shipit-notifications-buffer" nil t)
(transient-define-prefix shipit-notifications-buffer-auto-mark-menu ()
  "Manage auto-mark-read rules without leaving the notifications buffer."
  [["Add"
    ("a" "Add rule (guided)"
     shipit-notifications-buffer-add-auto-mark-rule)
    ("+" "Add rule from notification at point"
     shipit-notifications-buffer-add-auto-mark-rule-from-point)]
   ["Manage"
    ("l" "Show rules list"
     shipit-notifications-buffer-toggle-auto-mark-rules-list)
    ("d" "Remove a rule"
     shipit-notifications-buffer-remove-auto-mark-rule)
    ("X" "Clear all rules"
     shipit-notifications-buffer-clear-auto-mark-rules)]
   ["Advanced"
    ("c" "Open customize buffer"
     shipit-notifications-buffer-customize-auto-mark-rules)
    ("q" "Back" transient-quit-one)]])

(defun shipit-notifications-buffer--preview-entries-under (section)
  "Add strike-through preview overlays to every notification-entry in SECTION.
Walks the section tree, so it works for both flat and group-by-repo
layouts.  Reuses the auto-mark preview overlay list — a single
`shipit-notifications-buffer--clear-auto-mark-preview' undoes it."
  (cl-labels
      ((walk (s)
         (cond
          ((eq (oref s type) 'notification-entry)
           (shipit-notifications-buffer--add-preview-row-overlay s))
          (t
           (dolist (c (oref s children))
             (walk c))))))
    (walk section)))

(defun shipit-notifications-buffer--preview-resolved-rows ()
  "Add strike-through overlays to notification rows whose PR is resolved.
Recursively walks the section tree so the preview also lights up
in `group-by-repo' mode where entries are nested under
`notification-repo' wrappers.  Reuses the auto-mark preview
overlay list so a single
`shipit-notifications-buffer--clear-auto-mark-preview' undoes it."
  (when (bound-and-true-p magit-root-section)
    (cl-labels
        ((walk (s)
           (cond
            ((eq (oref s type) 'notification-entry)
             (let* ((activity (oref s value))
                    (state (cdr (assq 'pr-state activity))))
               (when (member state '("merged" "closed"))
                 (shipit-notifications-buffer--add-preview-row-overlay s))))
            (t
             (dolist (c (oref s children))
               (walk c))))))
      (walk magit-root-section))))

(defun shipit-notifications-buffer--activity-key-tuple (a)
  "Return (NUMBER REPO TYPE) for activity A."
  (list (or (cdr (assq 'number a))
            (cdr (assq 'pr-number a)))
        (cdr (assq 'repo a))
        (or (cdr (assq 'type a)) "pr")))

(defun shipit-notifications-buffer--collect-by-predicate (predicate)
  "Walk the global activity hash and return (NUMBER REPO TYPE) for each
ACTIVITY that PREDICATE returns non-nil for.  Use this when the
intent is a cross-repo cleanup (e.g. mark every resolved PR);
for a `mark-what-I-can-see' action use
`--collect-visible-by-predicate' instead."
  (let ((acc '()))
    (when (and (boundp 'shipit--notification-pr-activities)
               shipit--notification-pr-activities)
      (maphash
       (lambda (_k a)
         (when (funcall predicate a)
           (push (shipit-notifications-buffer--activity-key-tuple a) acc)))
       shipit--notification-pr-activities))
    acc))

(defun shipit-notifications-buffer--collect-visible-by-predicate (predicate)
  "Return (NUMBER REPO TYPE) for visible activities matching PREDICATE.
Visible means the same set the buffer is rendering: the
selector + structural filters that produce the render pool,
plus the text filter on top.  When the user invokes a bulk
mark this is what they expect to act on -- not entries
hidden by their selectors."
  (let ((acc '()))
    (dolist (a (shipit-notifications-buffer--repo-filtered-activities))
      (when (and (or (string-empty-p shipit-notifications-buffer--filter-text)
                     (shipit-notifications-buffer--matches-filter-p a))
                 (funcall predicate a))
        (push (shipit-notifications-buffer--activity-key-tuple a) acc)))
    (nreverse acc)))

(defun shipit-notifications-buffer--preview-rows-by-predicate (predicate)
  "Add strike-through preview overlays to notification rows where
PREDICATE returns non-nil for the row's activity."
  (when (bound-and-true-p magit-root-section)
    (cl-labels
        ((walk (s)
           (cond
            ((eq (oref s type) 'notification-entry)
             (when (funcall predicate (oref s value))
               (shipit-notifications-buffer--add-preview-row-overlay s)))
            (t
             (dolist (c (oref s children))
               (walk c))))))
      (walk magit-root-section))))

(defun shipit-notifications-buffer--mark-by-predicate (predicate label &optional visible-only)
  "Mark every activity for which PREDICATE returns non-nil as read.
Previews matching rows with a strike-through overlay before the y/n
prompt; tears down the overlay whether you confirm or abort.  LABEL
is a short noun used in the prompt and confirmation message.
When VISIBLE-ONLY is non-nil, only sweep activities currently
rendered in the buffer (post-scope, post-selector, post-filter);
otherwise sweep the full activity hash."
  (let ((targets (if visible-only
                     (shipit-notifications-buffer--collect-visible-by-predicate
                      predicate)
                   (shipit-notifications-buffer--collect-by-predicate
                    predicate))))
    (cond
     ((null targets)
      (message "No %s notifications to mark as read" label))
     (t
      (shipit-notifications-buffer--clear-auto-mark-preview)
      (shipit-notifications-buffer--preview-rows-by-predicate predicate)
      (redisplay t)
      (unwind-protect
          (when (yes-or-no-p
                 (format "Mark %d %s notification%s as read? "
                         (length targets) label
                         (if (= 1 (length targets)) "" "s")))
            (dolist (notif targets)
              (when (fboundp 'shipit--mark-notification-read)
                (shipit--mark-notification-read
                 (car notif) (cadr notif) t (caddr notif))))
            (message "Marked %d %s notification%s as read"
                     (length targets) label
                     (if (= 1 (length targets)) "" "s"))
            (shipit-notifications-buffer-refresh))
        (shipit-notifications-buffer--clear-auto-mark-preview))))))

(defun shipit-notifications-buffer--actionable-activity-p (activity)
  "Return non-nil if ACTIVITY's reason is actionable.
Actionable reasons are listed in `shipit-notifications-actionable-reasons'."
  (let ((r (cdr (assq 'reason activity))))
    (and (stringp r)
         (member r shipit-notifications-actionable-reasons))))

(defun shipit-notifications-buffer-mark-actionable-read (&optional arg)
  "Mark notifications with an actionable reason as read.
Actionable reasons are listed in `shipit-notifications-actionable-reasons'
\(mentions, review_requested, etc.).
Operates on currently visible rows by default; with prefix ARG
sweeps the full activity hash regardless of scope/selectors."
  (interactive "P")
  (shipit-notifications-buffer--mark-by-predicate
   #'shipit-notifications-buffer--actionable-activity-p
   "actionable"
   (not arg)))

(defun shipit-notifications-buffer-mark-non-actionable-read (&optional arg)
  "Mark notifications with a non-actionable reason as read.
Non-actionable reasons are everything not in
`shipit-notifications-actionable-reasons' \(subscribed, etc.) — the
ones that pile up during routine triage.
Operates on currently visible rows by default; with prefix ARG
sweeps the full activity hash regardless of scope/selectors."
  (interactive "P")
  (shipit-notifications-buffer--mark-by-predicate
   (lambda (a)
     (not (shipit-notifications-buffer--actionable-activity-p a)))
   "non-actionable"
   (not arg)))

(defun shipit-notifications-buffer-mark-resolved-read ()
  "Mark all merged and closed PR notifications as read.
Walks the full activity hash (not just visible rows) so users can
clean up resolved noise without paging through every entry first.
Previews the targets with a strike-through overlay (same visual as
the auto-mark regex preview) before the y/n prompt; the overlay is
torn down whether you confirm or abort."
  (interactive)
  (let ((resolved (shipit-notifications-buffer--collect-resolved-prs)))
    (cond
     ((null resolved)
      (message "No resolved PRs to mark as read"))
     (t
      (shipit-notifications-buffer--clear-auto-mark-preview)
      (shipit-notifications-buffer--preview-resolved-rows)
      (redisplay t)
      (unwind-protect
          (when (yes-or-no-p
                 (format "Mark %d resolved PR%s (merged/closed) as read? "
                         (length resolved)
                         (if (= 1 (length resolved)) "" "s")))
            (dolist (notif resolved)
              (when (fboundp 'shipit--mark-notification-read)
                (shipit--mark-notification-read
                 (car notif) (cadr notif) t (caddr notif))))
            (message "Marked %d resolved PR%s as read"
                     (length resolved)
                     (if (= 1 (length resolved)) "" "s"))
            (shipit-notifications-buffer-refresh))
        (shipit-notifications-buffer--clear-auto-mark-preview))))))

;;; Scope / pagination

(defun shipit-notifications-buffer-toggle-scope ()
  "Flip between `unread' and `all' display scopes.
Resets `shipit-notifications-buffer--current-page' to 1 so that
switching views never silently fans out 10 pages of API requests,
then refreshes."
  (interactive)
  (setq shipit-notifications-buffer--display-scope
        (if (eq shipit-notifications-buffer--display-scope 'all) 'unread 'all))
  (setq shipit-notifications-buffer--current-page 1)
  (message "Notifications scope: %s"
           shipit-notifications-buffer--display-scope)
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-page-forward ()
  "Advance to the next page of notifications in `all' scope.
Windowed: the buffer replaces the current view with only page N+1,
it does not accumulate.  Refuses in `unread' scope where GitHub's
own feed is already scoped to what's unread, and refuses past the
last page when the total count is known."
  (interactive)
  (unless (eq shipit-notifications-buffer--display-scope 'all)
    (user-error "Next-page only applies in 'all' scope (current: %s)"
                shipit-notifications-buffer--display-scope))
  (let ((max-page (shipit-notifications-buffer--max-page)))
    (when (and max-page
               (>= shipit-notifications-buffer--current-page max-page))
      (user-error "Already on the last page (%d)" max-page)))
  (cl-incf shipit-notifications-buffer--current-page)
  (message "Loading page %d..." shipit-notifications-buffer--current-page)
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-page-back ()
  "Go back one page in the notifications buffer.
Windowed: replaces the current view with only page N-1, does not
keep the dropped page's entries.  Only meaningful in `all' scope.
Refuses when already at page 1."
  (interactive)
  (unless (eq shipit-notifications-buffer--display-scope 'all)
    (user-error "Prev-page only applies in 'all' scope (current: %s)"
                shipit-notifications-buffer--display-scope))
  (when (<= shipit-notifications-buffer--current-page 1)
    (user-error "Already at page 1"))
  (cl-decf shipit-notifications-buffer--current-page)
  (message "Going back to page %d..." shipit-notifications-buffer--current-page)
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer--max-page ()
  "Return the known last page number, or nil if the total probe has not returned.
Uses 50 per page since GitHub caps `/notifications' at that regardless of
the requested `per_page'."
  (let ((total shipit-notifications-buffer--total-count))
    (when (and total (> total 0))
      (max 1 (ceiling (/ (float total) 50))))))

(defun shipit-notifications-buffer-first-page ()
  "Jump to page 1 in `all' scope."
  (interactive)
  (unless (eq shipit-notifications-buffer--display-scope 'all)
    (user-error "Page navigation only applies in 'all' scope (current: %s)"
                shipit-notifications-buffer--display-scope))
  (when (= shipit-notifications-buffer--current-page 1)
    (user-error "Already at page 1"))
  (setq shipit-notifications-buffer--current-page 1)
  (message "Going to page 1...")
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-last-page ()
  "Jump to the last page in `all' scope.
Requires the total-count probe to have returned so we know the last
page number.  Until then, refuses with a hint that the probe is
still in flight."
  (interactive)
  (unless (eq shipit-notifications-buffer--display-scope 'all)
    (user-error "Page navigation only applies in 'all' scope (current: %s)"
                shipit-notifications-buffer--display-scope))
  (let ((last (shipit-notifications-buffer--max-page)))
    (unless last
      (user-error "Total-count probe has not returned yet; try again after refresh"))
    (when (= shipit-notifications-buffer--current-page last)
      (user-error "Already at the last page (%d)" last))
    (setq shipit-notifications-buffer--current-page last)
    (message "Going to last page %d..." last)
    (shipit-notifications-buffer-refresh)))

(defun shipit-notifications-buffer-goto-page (page)
  "Jump to a specific PAGE (1..N) in `all' scope.
When the total-count probe has returned, the prompt shows the
maximum page so you can enter any value in [1, last]; invalid
numbers signal a user-error."
  (interactive
   (list (read-number
          (let ((last (shipit-notifications-buffer--max-page)))
            (if last
                (format "Page (1-%d): " last)
              "Page: ")))))
  (unless (eq shipit-notifications-buffer--display-scope 'all)
    (user-error "Page navigation only applies in 'all' scope (current: %s)"
                shipit-notifications-buffer--display-scope))
  (unless (and (integerp page) (>= page 1))
    (user-error "Page must be a positive integer (got %S)" page))
  (let ((last (shipit-notifications-buffer--max-page)))
    (when (and last (> page last))
      (user-error "Page %d exceeds last page %d" page last)))
  (setq shipit-notifications-buffer--current-page page)
  (message "Going to page %d..." page)
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-clear-filter ()
  "Clear the live text filter."
  (interactive)
  (setq shipit-notifications-buffer--filter-text "")
  (shipit-notifications-buffer--rerender))

;;; Repo filter (server-side)

(defun shipit-notifications-buffer--gather-watched-repos ()
  "Fetch watched repos from all PR backends, caching for the session.
Returns a sorted list of `OWNER/REPO' strings.  Uses
`shipit-notifications-buffer--watched-repos-cache' so the first
picker invocation pays the round-trip but later ones are instant."
  (require 'shipit-subscriptions-buffer)
  (unless shipit-notifications-buffer--watched-repos-cache
    (let ((repos '()))
      (dolist (entry (shipit-subscriptions--backends-with-watched-repos))
        (let* ((backend-plist (cdr entry))
               (config (list :repo ""))
               (fetch-fn (plist-get backend-plist :fetch-watched-repos)))
          (dolist (repo (condition-case err
                            (funcall fetch-fn config)
                          (error
                           (shipit--debug-log
                            "Watched-repos fetch failed: %S" err)
                           nil)))
            (let ((name (cdr (assq 'full_name repo))))
              (when (and name (stringp name) (> (length name) 0))
                (push name repos))))))
      (setq shipit-notifications-buffer--watched-repos-cache
            (sort (delete-dups repos) #'string<))))
  shipit-notifications-buffer--watched-repos-cache)

(defun shipit-notifications-buffer--candidate-repos ()
  "Return candidate repos for the repo-filter picker.
Union of subscribed repos (from the PR backends) and repos
currently visible in the notifications hash, so the picker works
even before the backend fetch returns."
  (let ((from-hash '()))
    (dolist (a (shipit-notifications-buffer--all-activities))
      (let ((r (cdr (assq 'repo a))))
        (when (and r (stringp r) (> (length r) 0))
          (push r from-hash))))
    (let ((watched (ignore-errors
                     (shipit-notifications-buffer--gather-watched-repos))))
      (sort (delete-dups (append watched from-hash)) #'string<))))

(defun shipit-notifications-buffer-set-repo (&optional arg)
  "Prompt for a repo to scope the notifications buffer to server-side.
Candidates come from the PR backends' `:fetch-watched-repos' plus
any repos already present in the hash.  Picking the clear-label
entry unscopes the filter.  Triggers a refresh so both the main
fetch and the total-count probe hit the per-repo endpoint.

With prefix ARG the prompt instead toggles the chosen repo in
`--excluded-repos' (the inverse list-valued filter): repos in the
exclusion list are hidden regardless of the selector."
  (interactive "P")
  (if arg
      (let* ((candidates (delete-dups
                          (append shipit-notifications-buffer--excluded-repos
                                  (shipit-notifications-buffer--candidate-repos))))
             (choice (completing-read "Toggle repo in exclusion list: "
                                      candidates nil nil)))
        (when (and (stringp choice) (not (string-empty-p choice)))
          (if (cl-find choice shipit-notifications-buffer--excluded-repos
                       :test #'string-equal-ignore-case)
              (progn
                (setq shipit-notifications-buffer--excluded-repos
                      (cl-remove choice
                                 shipit-notifications-buffer--excluded-repos
                                 :test #'string-equal-ignore-case))
                (message "Repo %s removed from exclusion list" choice))
            (push choice shipit-notifications-buffer--excluded-repos)
            (message "Repo %s excluded" choice)))
        (shipit-notifications-buffer--rerender))
    (let* ((candidates (delete-dups
                        (append shipit-notifications-buffer--selected-repo
                                (shipit-notifications-buffer--candidate-repos))))
           (choice (completing-read "Toggle repo in selector: "
                                    candidates nil t)))
      (when (and (stringp choice) (not (string-empty-p choice)))
        (if (cl-find choice shipit-notifications-buffer--selected-repo
                     :test #'string-equal-ignore-case)
            (progn
              (setq shipit-notifications-buffer--selected-repo
                    (cl-remove choice
                               shipit-notifications-buffer--selected-repo
                               :test #'string-equal-ignore-case))
              (message "Repo %s removed from selector" choice))
          (push choice shipit-notifications-buffer--selected-repo)
          (message "Repo %s added to selector" choice)))
      (setq shipit-notifications-buffer--current-page 1)
      (shipit-notifications-buffer-refresh))))

(defun shipit-notifications-buffer-clear-repo (&optional arg)
  "Clear the server-side repo filter and refresh.
With prefix ARG, instead clear the repo exclusion list."
  (interactive "P")
  (cond
   (arg
    (setq shipit-notifications-buffer--excluded-repos nil)
    (message "Repo exclusion list cleared")
    (shipit-notifications-buffer--rerender))
   (t
    (setq shipit-notifications-buffer--selected-repo nil)
    (setq shipit-notifications-buffer--current-page 1)
    (message "Repo selector cleared")
    (shipit-notifications-buffer-refresh))))

(defun shipit-notifications-buffer--candidate-jira-components ()
  "Return sorted distinct Jira component names for completion.
Combines components present on currently-loaded Jira activities with
the full project component list fetched via
`shipit-issue-jira--fetch-components'.  The activity sweep gives
instant candidates; the API fetch fills in components that aren't
on any currently-loaded notification.  API failures fall through
silently (cached hash candidates remain)."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (a (shipit-notifications-buffer--all-activities))
      (when (eq (cdr (assq 'source a)) 'jira)
        (dolist (c (cdr (assq 'jira-components a)))
          (when (and (stringp c) (> (length c) 0))
            (puthash c t seen)))))
    (let ((configs '())
          (config-keys (make-hash-table :test 'equal)))
      (dolist (a (shipit-notifications-buffer--all-activities))
        (when (eq (cdr (assq 'source a)) 'jira)
          (let* ((cfg (cdr (assq 'backend-config a)))
                 (key (and cfg (or (plist-get cfg :base-url)
                                   (prin1-to-string cfg)))))
            (when (and key (not (gethash key config-keys)))
              (puthash key t config-keys)
              (push cfg configs)))))
      (when (fboundp 'shipit-issue-jira--fetch-components)
        (dolist (cfg configs)
          (condition-case err
              (dolist (c (shipit-issue-jira--fetch-components cfg))
                (when (and (stringp c) (> (length c) 0))
                  (puthash c t seen)))
            (error
             (when (fboundp 'shipit--debug-log)
               (shipit--debug-log "fetch-components failed: %s"
                                  (error-message-string err))))))))
    (sort (hash-table-keys seen) #'string<)))

(defun shipit-notifications-buffer-set-jira-component (&optional arg)
  "Toggle a Jira component in the allow-list (or deny-list with prefix ARG)."
  (interactive "P")
  (let ((cands (shipit-notifications-buffer--candidate-jira-components)))
    (cond
     ((null cands)
      (user-error "No Jira components found among current notifications"))
     (t
      (let ((choice (completing-read
                     (if arg "Toggle component in deny-list: "
                       "Toggle component in allow-list: ")
                     cands nil t)))
        (when (and (stringp choice) (not (string-empty-p choice)))
          (let ((sym (if arg
                         'shipit-notifications-buffer--excluded-jira-components
                       'shipit-notifications-buffer--selected-jira-component)))
            (set sym
                 (if (member choice (symbol-value sym))
                     (remove choice (symbol-value sym))
                   (cons choice (symbol-value sym))))
            (message "Component %s %s: %S"
                     choice (if arg "deny-list" "allow-list")
                     (symbol-value sym)))
          (shipit-notifications-buffer--rerender)))))))

(defun shipit-notifications-buffer-clear-jira-component (&optional arg)
  "Clear the Jira component allow-list (or deny-list with prefix ARG)."
  (interactive "P")
  (if arg
      (progn
        (setq shipit-notifications-buffer--excluded-jira-components nil)
        (message "Component deny-list cleared"))
    (setq shipit-notifications-buffer--selected-jira-component nil)
    (message "Component allow-list cleared"))
  (shipit-notifications-buffer--rerender))

(defun shipit-notifications-buffer--candidate-types ()
  "Return the sorted list of types currently present in the hash.
Used as completion candidates for the type-filter picker so the
user only sees types that would actually return matches."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (a (shipit-notifications-buffer--all-activities))
      (let ((ty (cdr (assq 'type a))))
        (when (and ty (stringp ty) (> (length ty) 0))
          (puthash ty t seen))))
    (sort (hash-table-keys seen) #'string<)))

(defun shipit-notifications-buffer-set-type (&optional arg)
  "Toggle a type in the allow-list (or deny-list with prefix ARG).
Candidates come from types currently in the hash."
  (interactive "P")
  (let* ((cands (shipit-notifications-buffer--candidate-types))
         (choice (completing-read
                  (if arg "Toggle type in deny-list: "
                    "Toggle type in allow-list: ")
                  cands nil t)))
    (when (and (stringp choice) (not (string-empty-p choice)))
      (let ((sym (if arg
                     'shipit-notifications-buffer--excluded-types
                   'shipit-notifications-buffer--selected-type)))
        (set sym
             (if (member choice (symbol-value sym))
                 (remove choice (symbol-value sym))
               (cons choice (symbol-value sym))))
        (message "Type %s %s: %S"
                 choice (if arg "deny-list" "allow-list")
                 (symbol-value sym)))
      (shipit-notifications-buffer--rerender))))

(defun shipit-notifications-buffer-clear-type (&optional arg)
  "Clear the type allow-list (or deny-list with prefix ARG)."
  (interactive "P")
  (if arg
      (progn
        (setq shipit-notifications-buffer--excluded-types nil)
        (message "Type deny-list cleared"))
    (setq shipit-notifications-buffer--selected-type nil)
    (message "Type allow-list cleared"))
  (shipit-notifications-buffer--rerender))

(defun shipit-notifications-buffer--candidate-states ()
  "Return the canonical list of state-filter values.
Static set matching the icon-picker: open / merged / closed / draft."
  '("open" "merged" "closed" "draft"))

(defun shipit-notifications-buffer-set-state (&optional arg)
  "Toggle a PR state in the allow-list (or deny-list with prefix ARG)."
  (interactive "P")
  (let* ((cands (shipit-notifications-buffer--candidate-states))
         (choice (completing-read
                  (if arg "Toggle state in deny-list: "
                    "Toggle state in allow-list: ")
                  cands nil t)))
    (when (and (stringp choice) (not (string-empty-p choice)))
      (let ((sym (if arg
                     'shipit-notifications-buffer--excluded-states
                   'shipit-notifications-buffer--selected-state)))
        (set sym
             (if (member choice (symbol-value sym))
                 (remove choice (symbol-value sym))
               (cons choice (symbol-value sym))))
        (message "State %s %s: %S"
                 choice (if arg "deny-list" "allow-list")
                 (symbol-value sym)))
      (shipit-notifications-buffer--rerender))))

(defun shipit-notifications-buffer-clear-state (&optional arg)
  "Clear the state allow-list (or deny-list with prefix ARG)."
  (interactive "P")
  (if arg
      (progn
        (setq shipit-notifications-buffer--excluded-states nil)
        (message "State deny-list cleared"))
    (setq shipit-notifications-buffer--selected-state nil)
    (message "State allow-list cleared"))
  (shipit-notifications-buffer--rerender))

(defun shipit-notifications-buffer-set-reason (&optional arg)
  "Toggle a reason in the allow-list (or deny-list with prefix ARG)."
  (interactive "P")
  (let* ((cands (shipit-notifications-buffer--candidate-reasons))
         (choice (completing-read
                  (if arg "Toggle reason in deny-list: "
                    "Toggle reason in allow-list: ")
                  cands nil t)))
    (when (and (stringp choice) (not (string-empty-p choice)))
      (let ((sym (if arg
                     'shipit-notifications-buffer--excluded-reasons
                   'shipit-notifications-buffer--selected-reason)))
        (set sym
             (if (member choice (symbol-value sym))
                 (remove choice (symbol-value sym))
               (cons choice (symbol-value sym))))
        (message "Reason %s %s: %S"
                 choice (if arg "deny-list" "allow-list")
                 (symbol-value sym)))
      (shipit-notifications-buffer--rerender))))

(defun shipit-notifications-buffer-clear-reason (&optional arg)
  "Clear the reason allow-list (or deny-list with prefix ARG)."
  (interactive "P")
  (if arg
      (progn
        (setq shipit-notifications-buffer--excluded-reasons nil)
        (message "Reason deny-list cleared"))
    (setq shipit-notifications-buffer--selected-reason nil)
    (message "Reason allow-list cleared"))
  (shipit-notifications-buffer--rerender))

(defun shipit-notifications-buffer--reason-description ()
  "Describe the reason allow- and deny-lists for the transient."
  (let* ((allow shipit-notifications-buffer--selected-reason)
         (deny shipit-notifications-buffer--excluded-reasons)
         (allow-part (if allow
                         (format "Reason: %s" (mapconcat #'identity allow ","))
                       "Reason: <all reasons>")))
    (if deny
        (format "%s  -%s" allow-part (mapconcat #'identity deny ","))
      allow-part)))

(defun shipit-notifications-buffer--jira-component-description ()
  "Describe the Jira component allow- and deny-lists for the transient."
  (let* ((allow shipit-notifications-buffer--selected-jira-component)
         (deny shipit-notifications-buffer--excluded-jira-components)
         (allow-part (if allow
                         (format "Component: %s"
                                 (mapconcat #'identity allow ","))
                       "Component: <all components>")))
    (if deny
        (format "%s  -%s" allow-part (mapconcat #'identity deny ","))
      allow-part)))

(defun shipit-notifications-buffer-toggle-actionable-only (&optional inverse)
  "Toggle the actionable-only filter and re-render.
Without prefix: cycle nil → t (only actionable).
With prefix arg INVERSE: cycle nil → `non-actionable' (only the
opposite — useful when you want to see only the noisy rows the
actionable filter would hide).  Calling again with the same
arg flips back off."
  (interactive "P")
  (setq shipit-notifications-buffer--actionable-only
        (cond
         (inverse
          (if (eq shipit-notifications-buffer--actionable-only 'non-actionable)
              nil
            'non-actionable))
         (t
          (if (eq shipit-notifications-buffer--actionable-only t) nil t))))
  (message "Actionable filter: %s"
           (pcase shipit-notifications-buffer--actionable-only
             ('nil "off")
             ('t "actionable only")
             ('non-actionable "non-actionable only")))
  (shipit-notifications-buffer--rerender))

(defun shipit-notifications-buffer--actionable-only-description ()
  "Describe the actionable-only toggle for the transient."
  (format "Actionable: %s"
          (pcase shipit-notifications-buffer--actionable-only
            ('nil "off")
            ('t "only")
            ('non-actionable "inverse"))))

(defun shipit-notifications-buffer-toggle-group-by-repo ()
  "Toggle whether notifications are nested under per-repo sections."
  (interactive)
  (setq shipit-notifications-buffer--group-by-repo
        (not shipit-notifications-buffer--group-by-repo))
  (message "Group by repo: %s"
           (if shipit-notifications-buffer--group-by-repo "on" "off"))
  (shipit-notifications-buffer--rerender))

(defun shipit-notifications-buffer--group-by-repo-description ()
  "Describe the group-by-repo toggle for the transient."
  (format "Group by repo: %s"
          (if shipit-notifications-buffer--group-by-repo "on" "off")))

(defun shipit-notifications-buffer--read-iso-timestamp (prompt)
  "Prompt for a date/time and return an ISO-8601 timestamp string.
Uses `org-read-date' when available (accepts inputs like
-1m or 2026-01-15); falls back to plain read-string.
Suppresses Org calendar popup so the diary subsystem is not
loaded — many users do not have a diary file configured."
  (if (fboundp 'org-read-date)
      (let ((org-read-date-popup-calendar nil)
            (org-read-date-prefer-future nil))
        (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                            (org-read-date t t nil prompt) t))
    (read-string prompt)))

(defun shipit-notifications-buffer-set-before-filter ()
  "Prompt for a timestamp and set the server-side `before' filter.
Accepts org-read-date-style inputs (e.g. -1m, 2026-01-15).
Resets current-page to 1 so navigation restarts in the new window,
then refreshes."
  (interactive)
  (let ((ts (shipit-notifications-buffer--read-iso-timestamp
             "Show notifications updated before: ")))
    (setq shipit-notifications-buffer--before-filter ts)
    (setq shipit-notifications-buffer--current-page 1)
    (message "Before filter: %s" ts)
    (shipit-notifications-buffer-refresh)))

(defun shipit-notifications-buffer-clear-before-filter ()
  "Clear the `before' filter and refresh."
  (interactive)
  (setq shipit-notifications-buffer--before-filter nil)
  (setq shipit-notifications-buffer--current-page 1)
  (message "Before filter cleared")
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-set-since-filter ()
  "Prompt for a timestamp and set the server-side `since' filter.
Resets current-page to 1, then refreshes."
  (interactive)
  (let ((ts (shipit-notifications-buffer--read-iso-timestamp
             "Show notifications updated since: ")))
    (setq shipit-notifications-buffer--since-filter ts)
    (setq shipit-notifications-buffer--current-page 1)
    (message "Since filter: %s" ts)
    (shipit-notifications-buffer-refresh)))

(defun shipit-notifications-buffer-clear-since-filter ()
  "Clear the `since' filter and refresh."
  (interactive)
  (setq shipit-notifications-buffer--since-filter nil)
  (setq shipit-notifications-buffer--current-page 1)
  (message "Since filter cleared")
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-snooze-at-point (&optional arg)
  "Snooze (or unsnooze) the notification at point.
On an unsnoozed row, snoozes for
`shipit-notifications-snooze-default-hours'.  On an already-snoozed
row (visible only via the `Snoozed' group footer in unread scope),
unsnoozes it instead.

Numeric prefix ARG overrides the snooze duration in hours.  `C-u'
or any non-numeric prefix prompts for the value (default
pre-filled).  `Z' clears or lists all active snoozes."
  (interactive "P")
  (let ((activity (shipit-notifications-buffer--activity-at-point)))
    (cond
     ((null activity)
      (user-error "No notification at point"))
     (t
      (let* ((repo (cdr (assq 'repo activity)))
             (type (or (cdr (assq 'type activity)) "pr"))
             (number (or (cdr (assq 'number activity))
                         (cdr (assq 'pr-number activity))))
             (key (format "%s:%s:%s" repo type number))
             (existing (assoc key shipit-notifications-buffer--snoozed-items)))
        (cond
         ((and existing (> (cdr existing) (float-time)))
          (setq shipit-notifications-buffer--snoozed-items
                (assoc-delete-all
                 key shipit-notifications-buffer--snoozed-items))
          (message "Unsnoozed %s" key)
          (shipit-notifications-buffer--rerender)
          (shipit-notifications-buffer--refresh-modeline-count))
         (t
          (let* ((hours (cond
                         ((numberp arg) arg)
                         (arg (read-number "Snooze for how many hours? "
                                           shipit-notifications-snooze-default-hours))
                         (t shipit-notifications-snooze-default-hours)))
                 (expires (+ (float-time) (* hours 60 60))))
            (setq shipit-notifications-buffer--snoozed-items
                  (cons (cons key expires)
                        (assoc-delete-all
                         key shipit-notifications-buffer--snoozed-items)))
            (shipit--debug-log "Snooze: stored key=%S expires=%S list=%S"
                               key expires
                               shipit-notifications-buffer--snoozed-items)
            (message "Snoozed %s for %s hour%s"
                     key hours (if (= hours 1) "" "s"))
            (shipit-notifications-buffer--rerender)
            (shipit-notifications-buffer--refresh-modeline-count)))))))))

(defun shipit-notifications-buffer--refresh-modeline-count ()
  "Recompute and apply the modeline bell count after a snooze/unsnooze.
Walks `shipit--count-unread-activities' which subtracts the
buffer-local snooze list, then pushes the value through
`shipit--update-modeline-indicator'."
  (when (and (fboundp 'shipit--count-unread-activities)
             (fboundp 'shipit--update-modeline-indicator))
    (let ((n (shipit--count-unread-activities)))
      (setq shipit--last-notification-count n)
      (shipit--update-modeline-indicator n))))

(defun shipit-notifications-buffer--format-snooze-remaining (expires)
  "Format the snooze remaining at EXPIRES (float-time) as `Nh Mm'."
  (let* ((seconds (max 0 (round (- expires (float-time)))))
         (hours (/ seconds 3600))
         (mins (/ (mod seconds 3600) 60)))
    (cond
     ((and (= hours 0) (= mins 0)) "<1m")
     ((zerop hours) (format "%dm" mins))
     ((zerop mins) (format "%dh" hours))
     (t (format "%dh %dm" hours mins)))))

(defun shipit-notifications-buffer-clear-snoozes (&optional arg)
  "List active snoozes and unsnooze a chosen item.
With prefix ARG clear every active snooze in one go.  Otherwise
the prompt shows `KEY (Nh Mm left)' for each entry; selecting one
unsnoozes that item.  Picking the special `<clear all>' entry
clears every snooze, same as the prefix variant."
  (interactive "P")
  (shipit-notifications-buffer--prune-expired-snoozes)
  (let ((live shipit-notifications-buffer--snoozed-items))
    (cond
     ((null live)
      (message "No active snoozes"))
     (arg
      (setq shipit-notifications-buffer--snoozed-items nil)
      (message "Cleared %d snooze%s" (length live)
               (if (= (length live) 1) "" "s"))
      (shipit-notifications-buffer--rerender)
      (shipit-notifications-buffer--refresh-modeline-count))
     (t
      (let* ((clear-label "<clear all>")
             (rows (mapcar (lambda (cell)
                             (format "%s (%s left)"
                                     (car cell)
                                     (shipit-notifications-buffer--format-snooze-remaining
                                      (cdr cell))))
                           live))
             (choice (completing-read "Unsnooze: "
                                      (cons clear-label rows) nil t)))
        (cond
         ((string= choice clear-label)
          (setq shipit-notifications-buffer--snoozed-items nil)
          (message "Cleared %d snooze%s" (length live)
                   (if (= (length live) 1) "" "s")))
         (t
          (let ((key (car (split-string choice " " t))))
            (setq shipit-notifications-buffer--snoozed-items
                  (assoc-delete-all
                   key shipit-notifications-buffer--snoozed-items))
            (message "Unsnoozed %s" key))))
        (shipit-notifications-buffer--rerender)
        (shipit-notifications-buffer--refresh-modeline-count))))))

(defun shipit-notifications-buffer-clear-all-filters ()
  "Clear every filter (text, repo, type, state, actionable, before, since).
Resets `current-page\=' to 1 since the visible window changes,
then issues a single refresh so the server-side repo/before/since
clearing takes effect alongside the client-side resets."
  (interactive)
  (setq shipit-notifications-buffer--filter-text ""
        shipit-notifications-buffer--selected-repo nil
        shipit-notifications-buffer--excluded-repos nil
        shipit-notifications-buffer--selected-type nil
        shipit-notifications-buffer--excluded-types nil
        shipit-notifications-buffer--selected-state nil
        shipit-notifications-buffer--excluded-states nil
        shipit-notifications-buffer--selected-reason nil
        shipit-notifications-buffer--excluded-reasons nil
        shipit-notifications-buffer--actionable-only nil
        shipit-notifications-buffer--before-filter nil
        shipit-notifications-buffer--since-filter nil
        shipit-notifications-buffer--selected-jira-component nil
        shipit-notifications-buffer--excluded-jira-components nil
        shipit-notifications-buffer--current-page 1)
  (message "All filters cleared")
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-refresh-watched-repos-cache ()
  "Force a refetch of the repo-filter candidate cache next time."
  (interactive)
  (setq shipit-notifications-buffer--watched-repos-cache nil)
  (message "Repo-filter candidate cache cleared"))

;;; Filter

(defun shipit-notifications-buffer--scope-description ()
  "Describe the current scope toggle for the transient."
  (format "Scope: %s  (toggle unread/all)"
          shipit-notifications-buffer--display-scope))

(defun shipit-notifications-buffer--load-more-description ()
  "Describe the next-page action for the transient."
  (if (eq shipit-notifications-buffer--display-scope 'all)
      (format "Next page (goto %d)"
              (1+ shipit-notifications-buffer--current-page))
    "Next page (unavailable in 'unread' scope)"))

(defun shipit-notifications-buffer--text-filter-description ()
  "Describe the text-filter action for the transient."
  (if (string-empty-p shipit-notifications-buffer--filter-text)
      "Text filter…"
    (format "Text filter: %s" shipit-notifications-buffer--filter-text)))

(defun shipit-notifications-buffer--repo-description ()
  "Describe the repo allow- and deny-lists for the transient."
  (let* ((allow shipit-notifications-buffer--selected-repo)
         (deny shipit-notifications-buffer--excluded-repos)
         (allow-part (if allow
                         (format "Repo: %s" (mapconcat #'identity allow ","))
                       "Repo: <all repos>")))
    (if deny
        (format "%s  -%s" allow-part (mapconcat #'identity deny ","))
      allow-part)))

(defun shipit-notifications-buffer--type-description ()
  "Describe the type allow- and deny-lists for the transient."
  (let* ((allow shipit-notifications-buffer--selected-type)
         (deny shipit-notifications-buffer--excluded-types)
         (allow-part (if allow
                         (format "Type: %s" (mapconcat #'identity allow ","))
                       "Type: <all types>")))
    (if deny
        (format "%s  -%s" allow-part (mapconcat #'identity deny ","))
      allow-part)))

(defun shipit-notifications-buffer--state-description ()
  "Describe the state allow- and deny-lists for the transient."
  (let* ((allow shipit-notifications-buffer--selected-state)
         (deny shipit-notifications-buffer--excluded-states)
         (allow-part (if allow
                         (format "State: %s" (mapconcat #'identity allow ","))
                       "State: <all states>")))
    (if deny
        (format "%s  -%s" allow-part (mapconcat #'identity deny ","))
      allow-part)))

(defun shipit-notifications-buffer--before-filter-description ()
  "Describe the before-filter action for the transient."
  (if shipit-notifications-buffer--before-filter
      (format "Before: %s" shipit-notifications-buffer--before-filter)
    "Before: <none>"))

(defun shipit-notifications-buffer--since-filter-description ()
  "Describe the since-filter action for the transient."
  (if shipit-notifications-buffer--since-filter
      (format "Since: %s" shipit-notifications-buffer--since-filter)
    "Since: <none>"))

;;;###autoload (autoload 'shipit-notifications-buffer-filter-menu "shipit-notifications-buffer" nil t)
(transient-define-prefix shipit-notifications-buffer-filter-menu ()
  "View and filter controls for the notifications buffer.
The two columns mirror the conceptual split: View groups
adjust how the buffer presents what is loaded (scope, paging,
grouping), Filter groups narrow the activity set that is shown."
  [["View / Scope"
    ("s" shipit-notifications-buffer-toggle-scope
     :description shipit-notifications-buffer--scope-description)
    ("G" shipit-notifications-buffer-toggle-group-by-repo
     :description shipit-notifications-buffer--group-by-repo-description
     :transient t)]
   ["View / Pages (all scope)"
    ("<" "First page" shipit-notifications-buffer-first-page)
    (">" "Last page" shipit-notifications-buffer-last-page)
    ("P" "Go to page…" shipit-notifications-buffer-goto-page)]]
  [["Selector / Repo (server-side)"
    ("r" shipit-notifications-buffer-set-repo
     :description shipit-notifications-buffer--repo-description)
    ("R" "Clear repo selection" shipit-notifications-buffer-clear-repo)]
   ["Filter / Time window"
    ("b" shipit-notifications-buffer-set-before-filter
     :description shipit-notifications-buffer--before-filter-description)
    ("B" "Clear before filter" shipit-notifications-buffer-clear-before-filter)
    ("a" shipit-notifications-buffer-set-since-filter
     :description shipit-notifications-buffer--since-filter-description)
    ("A" "Clear since filter" shipit-notifications-buffer-clear-since-filter)]]
  [["Selector / Type / State"
    ("y" shipit-notifications-buffer-set-type
     :description shipit-notifications-buffer--type-description)
    ("Y" "Clear type selection" shipit-notifications-buffer-clear-type)
    ("e" shipit-notifications-buffer-set-state
     :description shipit-notifications-buffer--state-description)
    ("E" "Clear state selection" shipit-notifications-buffer-clear-state)]
   ["Selector / Reason"
    ("n" shipit-notifications-buffer-set-reason
     :description shipit-notifications-buffer--reason-description)
    ("N" "Clear reason selection" shipit-notifications-buffer-clear-reason)]
   ["Selector / Jira"
    ("o" shipit-notifications-buffer-set-jira-component
     :description shipit-notifications-buffer--jira-component-description)
    ("O" "Clear component selection"
     shipit-notifications-buffer-clear-jira-component)]
   ["Filter / Text"
    ("t" shipit-notifications-buffer-set-filter
     :description shipit-notifications-buffer--text-filter-description)
    ("c" "Clear text filter" shipit-notifications-buffer-clear-filter)
    ("!" shipit-notifications-buffer-toggle-actionable-only
     :description shipit-notifications-buffer--actionable-only-description
     :transient t)]]
  [["Mark / auto-mark"
    ("m" "Mark menu…"
     shipit-notifications-buffer-mark-menu)]
   ["Refresh"
    ("g" "Refresh now" shipit-notifications-buffer-refresh)
    ("x" "Clear all filters" shipit-notifications-buffer-clear-all-filters)
    ("q" "Quit" transient-quit-one)]])

(defcustom shipit-notifications-filter-live-delay 0.25
  "Idle delay in seconds before re-rendering on live text-filter typing.
Raising this makes fast typing less laggy at the cost of a slightly
delayed preview; lowering it makes the preview feel more immediate
at the cost of extra renders per keystroke."
  :type 'number
  :group 'shipit)

(defun shipit-notifications-buffer-set-filter ()
  "Set or clear the filter for notifications with live updates as you type.
Updates are debounced by `shipit-notifications-filter-live-delay' so
bursty typing re-renders the notifications buffer once per pause,
not once per keystroke; the text filter is also checked against
the cached last-rendered value so unchanged input is a no-op."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (timer nil)
         (minibuf nil)
         (last-rendered nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuf (current-buffer))
          (add-hook 'post-command-hook
                    (lambda ()
                      (when timer (cancel-timer timer))
                      (setq timer
                            (run-with-idle-timer
                             shipit-notifications-filter-live-delay nil
                             (lambda ()
                               (condition-case nil
                                   (when (and (buffer-live-p minibuf)
                                              (minibufferp minibuf))
                                     (let ((current-input
                                            (with-current-buffer minibuf
                                              (minibuffer-contents-no-properties))))
                                       (when (and (buffer-live-p original-buffer)
                                                  (not (equal current-input
                                                               last-rendered)))
                                         (setq last-rendered current-input)
                                         (with-current-buffer original-buffer
                                           (setq shipit-notifications-buffer--filter-text
                                                 current-input)
                                           (shipit-notifications-buffer--rerender)))))
                                 (error nil))))))
                    nil t))
      (let ((new-filter (read-string "Filter notifications: "
                                     shipit-notifications-buffer--filter-text)))
        (setq shipit-notifications-buffer--filter-text new-filter)
        (shipit-notifications-buffer--rerender)))))

;;; Debug / perf monitoring helpers

(defvar shipit-notifications-buffer--render-times nil
  "Recent render durations (seconds), most-recent first.
Populated when `shipit-notifications-buffer-debug-monitor' is on; the
last 20 entries are kept.")

(defvar shipit-notifications-buffer--render-monitor-active nil
  "Non-nil when render-time monitoring is enabled.")

(defun shipit-notifications-buffer--monitor-render-advice (orig-fn &rest args)
  "Record wall-clock time of a single render call."
  (let ((t0 (current-time)))
    (unwind-protect
        (apply orig-fn args)
      (push (float-time (time-since t0))
            shipit-notifications-buffer--render-times)
      (when (> (length shipit-notifications-buffer--render-times) 20)
        (setq shipit-notifications-buffer--render-times
              (cl-subseq shipit-notifications-buffer--render-times 0 20))))))

(defun shipit-notifications-buffer-debug-monitor (&optional disable)
  "Toggle render-time monitoring on/off.
With prefix arg, force DISABLE.  When on, every full render of the
notifications buffer records its wall-clock time; recent samples
show up in `shipit-notifications-buffer-debug-stats'."
  (interactive "P")
  (cond
   ((or disable shipit-notifications-buffer--render-monitor-active)
    (advice-remove 'shipit-notifications-buffer--render
                   #'shipit-notifications-buffer--monitor-render-advice)
    (setq shipit-notifications-buffer--render-monitor-active nil)
    (message "shipit notifications: render monitor OFF"))
   (t
    (advice-add 'shipit-notifications-buffer--render :around
                #'shipit-notifications-buffer--monitor-render-advice)
    (setq shipit-notifications-buffer--render-monitor-active t)
    (setq shipit-notifications-buffer--render-times nil)
    (message "shipit notifications: render monitor ON"))))

(defun shipit-notifications-buffer-debug-stats ()
  "Show a snapshot of state likely to explain notifications-buffer slowness.
Reports: scope/page, activity-cache size, total-count, queue depths,
recent render durations, GC metrics, and notifications-buffer
overlay/section/byte counts (regardless of which buffer is current
when invoked)."
  (interactive)
  (let* ((nbuf (get-buffer "*shipit-notifications*"))
         (scope (when nbuf
                  (buffer-local-value
                   'shipit-notifications-buffer--display-scope nbuf)))
         (page (when nbuf
                 (buffer-local-value
                  'shipit-notifications-buffer--current-page nbuf)))
         (total (when nbuf
                  (buffer-local-value
                   'shipit-notifications-buffer--total-count nbuf)))
         (loaded (length (shipit-notifications-buffer--all-activities)))
         (buf-size (when nbuf (with-current-buffer nbuf (buffer-size))))
         (overlays (when nbuf
                     (with-current-buffer nbuf
                       (length (overlays-in (point-min) (point-max))))))
         (sections (when nbuf
                     (with-current-buffer nbuf
                       (let ((c 0) (pos (point-min)))
                         (while (< pos (point-max))
                           (when (get-text-property pos 'magit-section)
                             (cl-incf c))
                           (setq pos (or (next-single-property-change
                                          pos 'magit-section nil (point-max))
                                         (point-max))))
                         c))))
         (url-queue-len (length (and (boundp 'url-queue) url-queue)))
         (etag-cache-size (and (boundp 'shipit-gh-etag--persistent-cache)
                               (hash-table-p shipit-gh-etag--persistent-cache)
                               (hash-table-count shipit-gh-etag--persistent-cache)))
         (locally-marked (and (boundp 'shipit--locally-marked-read-notifications)
                              (hash-table-p shipit--locally-marked-read-notifications)
                              (hash-table-count shipit--locally-marked-read-notifications)))
         (render-times shipit-notifications-buffer--render-times)
         (gc-elapsed gc-elapsed)
         (gcs-done gcs-done))
    (with-help-window "*shipit-notifications-debug*"
      (with-current-buffer "*shipit-notifications-debug*"
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "shipit notifications debug stats\n"
                              'face 'bold))
          (insert (format "%s\n\n" (format-time-string "%FT%T")))
          (insert (format "Scope:           %S\n" scope))
          (insert (format "Page:            %S\n" page))
          (insert (format "Server total:    %S\n" total))
          (insert (format "Loaded in cache: %d\n" loaded))
          (insert (format "Section count:   %S (in *shipit-notifications*)\n" sections))
          (insert (format "Overlay count:   %S (in *shipit-notifications*)\n" overlays))
          (insert (format "Buffer size:     %S bytes\n" buf-size))
          (insert (format "url-queue depth: %d\n" url-queue-len))
          (insert (format "ETag cache size: %S entries\n" etag-cache-size))
          (insert (format "Locally-marked:  %S ids\n" locally-marked))
          (insert "\n")
          (insert (propertize "Recent render times (sec, newest first):\n"
                              'face 'bold))
          (cond
           ((not shipit-notifications-buffer--render-monitor-active)
            (insert "  (monitor off — run M-x shipit-notifications-buffer-debug-monitor)\n"))
           ((null render-times)
            (insert "  (no samples yet)\n"))
           (t
            (dolist (s render-times)
              (insert (format "  %.3f\n" s)))
            (insert (format "  avg: %.3f, max: %.3f, min: %.3f, n=%d\n"
                            (/ (apply #'+ render-times) (length render-times))
                            (apply #'max render-times)
                            (apply #'min render-times)
                            (length render-times)))))
          (insert "\n")
          (insert (propertize "GC since Emacs start:\n" 'face 'bold))
          (insert (format "  gc-elapsed: %.2f s\n" gc-elapsed))
          (insert (format "  gcs-done:   %d\n" gcs-done))
          (insert (format "  gc-cons-threshold: %s\n" gc-cons-threshold)))))))

;; The general-purpose profiler lives in `shipit-debug.el' as
;; `shipit-debug-profile' (also bound under R in `shipit-debug-menu').
;; The notifications-specific debug-monitor and debug-stats stay here.

(provide 'shipit-notifications-buffer)
;;; shipit-notifications-buffer.el ends here
