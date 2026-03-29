;;; shipit-checks.el --- checks module -*- lexical-binding: t; -*-

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
;; CI/Checks fetching & rendering for shipit

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'shipit-lib)   ; For shipit--defer-refresh
(require 'shipit-core)  ; For utility functions like shipit--humanize-workflow-name
(require 'shipit-render)
(require 'shipit-pr-backends)

;; Forward declarations
(defvar shipit--general-comments-fetched) ; buffer-local from shipit-core.el
(defvar shipit--inline-comments-fetched) ; buffer-local from shipit-core.el
(defvar shipit--resolved-comments-hash) ; hash table from shipit-comments.el
(defvar shipit--cached-review-decision) ; buffer-local from shipit-cache.el
(declare-function shipit--ensure-cache-initialized "shipit-cache")
(declare-function shipit-auto-detect-repository "shipit-commands")
(declare-function shipit-open-actions-run "shipit-actions")
(declare-function shipit-actions--fetch-job-logs-raw "shipit-actions")
(declare-function shipit-actions--split-log-by-markers "shipit-actions")
(declare-function shipit-actions--insert-steps "shipit-actions")
(declare-function shipit-actions--step-label-width "shipit-actions")
(declare-function shipit-actions--step-url "shipit-actions")
(declare-function shipit-actions-timestamps-menu "shipit-actions")
(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-section-hide "magit-section")
(declare-function magit-section-show "magit-section")
(declare-function magit-section-toggle "magit-section")
(declare-function progn "magit-section")

(defvar shipit-actions--job-logs)
(defvar shipit-actions--repo)
(defvar shipit-actions--step-time-format)

;; Conditional magit requirements for checks integration
(eval-when-compile
  (when (locate-library "magit")
    (require 'magit)
    (require 'magit-section)))

;; At runtime, magit must be available for checks to work
;; Only require if actually using magit functionality

(defvar-local shipit--checks-fetched nil
  "Whether PR checks have been fetched.")

(defvar-local shipit--checks-loading nil
  "Flag to indicate if checks are currently being loaded.")

(defvar shipit--checks-timeout-timer nil
  "Timer for checks loading timeout, cancelled when loading completes.")

(defvar shipit--collected-runs '()
  "Temporary storage for enhanced check runs during processing.")

(defvar shipit--expected-suite-count 0
  "Expected number of suites to process.")

(defvar shipit--processed-suite-count 0
  "Number of suites processed so far.")

(defvar shipit--current-buffer nil
  "Current buffer to update when processing is complete.")

(defvar-local shipit--workflow-name-cache (make-hash-table :test 'equal)
  "Cache mapping workflow IDs to workflow names.")

(defvar shipit-checks-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'shipit--load-checks-on-demand)
    (define-key map (kbd "SPC") 'shipit--load-checks-on-demand)
    map)
  "Keymap for lazy-loaded checks section.")

(defun shipit--load-checks-on-demand ()
  "Load checks for the current PR on demand using the original working implementation.
Called when user expands lazy section."
  (interactive)
  ;; Try text properties first, fall back to buffer-local variables
  (let ((repo (or (get-text-property (point) 'shipit-repo)
                  (bound-and-true-p shipit-buffer-repo)))
        (pr-number (or (get-text-property (point) 'shipit-pr-number)
                       (bound-and-true-p shipit-buffer-pr-number))))
    (if (and repo pr-number)
        (progn
          (message "Loading checks for PR #%s..." pr-number)
          (setq shipit--checks-loading t)
          ;; Use the original working implementation that has proper workflow names
          (shipit--fetch-pr-checks repo pr-number))
      (message "Could not determine PR information for loading checks"))))

(defun shipit--auto-load-checks-on-expand (section)
  "Automatically load checks when checks section is expanded.
Called by magit-section-visibility-hook when section visibility changes."
  (when (and section
             (eq (oref section type) 'checks))
    ;; Check if section is now visible (expanded)
    (when (condition-case nil (oref section hidden) (error t))
      ;; Section was hidden, now being shown - this is an expand action
      nil)
    ;; Use magit-section-show-p or check hidden slot
    (unless (condition-case nil (oref section hidden) (error nil))
      ;; Check if this is a lazy-loaded section that hasn't been fetched yet
      (when (and (not shipit--checks-fetched)
                 (not shipit--checks-loading))
        ;; Use buffer-local variables for repo and pr-number
        (let ((repo (bound-and-true-p shipit-buffer-repo))
              (pr-number (bound-and-true-p shipit-buffer-pr-number)))
          (when (and repo pr-number)
            (message "Loading checks for PR #%s..." pr-number)
            (setq shipit--checks-loading t)
            (shipit--fetch-pr-checks repo pr-number)))))))

(defun shipit--get-checks-status-emoji (checks)
  "Return emoji representing overall status of CHECKS."
  (if (not checks)
      "⏳"  ; No checks
    (let ((has-failing nil)
          (has-in-progress nil)
          (has-pending nil)
          (has-cancelled nil)
          (has-successful nil))
      ;; Categorize all checks
      (dolist (check checks)
        (let* ((status (cdr (assq 'status check)))
               (conclusion (cdr (assq 'conclusion check))))
          (cond
           ;; Handle explicit conclusions first (priority)
           ((string= conclusion "failure") (setq has-failing t))
           ((string= conclusion "cancelled") (setq has-cancelled t))
           ((string= conclusion "success") (setq has-successful t))
           ;; Handle status-based categorization
           ((member status '("in_progress" "running")) (setq has-in-progress t))
           ((member status '("queued" "requested" "waiting")) (setq has-pending t))
           ;; Default for unknown statuses
           ((not (member status '("completed"))) (setq has-pending t)))))
      ;; Return emoji based on priority (failures > in-progress > pending > cancelled > success)
      (cond
       (has-failing (shipit--get-check-status-icon nil "failure" "❌"))         ; Any failures = red X
       (has-in-progress (shipit--get-check-status-icon "in_progress" nil "🟡"))     ; In progress = yellow dot
       (has-pending (shipit--get-check-status-icon "queued" nil "🔵"))         ; Pending = blue dot
       (has-cancelled (shipit--get-check-status-icon nil "cancelled" "⚪"))       ; Cancelled = circle
       (has-successful (shipit--get-check-status-icon nil "success" "✅"))      ; All successful = green checkmark
       (t (shipit--get-check-status-icon nil nil "❓"))))))               ; Unknown = question mark

;; Define magit section types for checks
(defun checks (&rest _args)
  "Magit section identifier for PR checks.")
(put 'checks 'magit-section t)

(defun in-progress-checks (&rest _args)
  "Magit section identifier for in-progress checks.")
(put 'in-progress-checks 'magit-section t)

(defun pending-checks (&rest _args)
  "Magit section identifier for pending checks.")
(put 'pending-checks 'magit-section t)

(defun cancelled-checks (&rest _args)
  "Magit section identifier for cancelled checks.")
(put 'cancelled-checks 'magit-section t)

(defun failing-checks (&rest _args)
  "Magit section identifier for failing checks.")
(put 'failing-checks 'magit-section t)

(defun skipped-checks (&rest _args)
  "Magit section identifier for skipped checks.")
(put 'skipped-checks 'magit-section t)

(defun successful-checks (&rest _args)
  "Magit section identifier for successful checks.")
(put 'successful-checks 'magit-section t)

(defun workflow (&rest _args)
  "Magit section identifier for workflow sections.")
(put 'workflow 'magit-section t)

(defun check-item (&rest _args)
  "Magit section identifier for individual check items (jobs).")
(put 'check-item 'magit-section t)

;;; Check item step/log expansion

(defvar-local shipit--check-item-logs (make-hash-table :test 'equal)
  "Hash table mapping job-id to parsed step logs for check items.")

(defvar-local shipit--check-item-job-data (make-hash-table :test 'equal)
  "Hash table mapping job-id to job API data for check items.")

(defun shipit--expand-check-item (section)
  "Expand check-item SECTION to show steps and logs.
Fetches job data and logs on first expand, uses cache thereafter."
  (let* ((check (oref section value))
         (job-id (cdr (assq 'id check))))
    (if (or (null job-id) (oref section children))
        (magit-section-toggle section)
      (let ((buf (current-buffer))
            (sect section)
            (repo (bound-and-true-p shipit-buffer-repo)))
        (if (null repo)
            (magit-section-toggle section)
          (message "Fetching steps...")
          (shipit--api-request
           (format "/repos/%s/actions/jobs/%s" repo job-id)
           nil
           (lambda (job-data)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (puthash job-id job-data shipit--check-item-job-data)
                 (let ((shipit-actions--repo repo))
                   (shipit-actions--fetch-job-logs-raw
                    job-id
                    (lambda (log-text)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (when log-text
                            (puthash job-id
                                     (shipit-actions--split-log-by-markers
                                      log-text job-data)
                                     shipit--check-item-logs))
                          (shipit--render-check-item-steps
                           sect job-data)))))))))))))))

(defun shipit--render-check-item-steps (section job-data)
  "Render steps into check-item SECTION using JOB-DATA and cached logs."
  (let* ((inhibit-read-only t)
         (content-pos (oref section content))
         (end-pos (oref section end))
         (saved-line (line-number-at-pos))
         (saved-col (current-column))
         (win (get-buffer-window (current-buffer)))
         (win-start (when win (window-start win)))
         ;; Save which steps/groups are expanded before re-render
         (expanded (shipit--save-check-step-visibility section)))
    (when (and content-pos end-pos)
      (delete-region content-pos end-pos)
      (goto-char content-pos)
      (let* ((magit-insert-section--parent section)
             (indent "          ")
             (prefix-width (+ (length indent) 2))
             (steps (cdr (assq 'steps job-data)))
             (step-name-col (+ prefix-width
                               (if steps
                                   (cl-reduce #'max steps
                                              :key #'shipit-actions--step-label-width
                                              :initial-value 0)
                                 0)))
             (bar-col (max step-name-col 30))
             (shipit-actions--job-logs shipit--check-item-logs))
        (oset section children nil)
        (if steps
            (shipit-actions--insert-steps job-data bar-col indent)
          (insert indent "(no steps)\n")))
      (let ((new-end (point-marker)))
        (oset section end new-end)
        (oset section hidden nil)
        (shipit--update-ancestor-ends section new-end))
      ;; Restore step visibility and point position
      (shipit--restore-check-step-visibility section expanded)
      (goto-char (point-min))
      (forward-line (1- saved-line))
      (move-to-column saved-col)
      (when (and win win-start)
        (set-window-start win win-start t)))))

(defun shipit--toggle-check-step-time-format (section)
  "Toggle step start time format and re-render SECTION."
  (setq shipit-actions--step-time-format
        (if (eq shipit-actions--step-time-format 'absolute) 'relative 'absolute))
  (message "Step times: %s" shipit-actions--step-time-format)
  (when (oref section children)
    (let* ((check (oref section value))
           (job-id (cdr (assq 'id check)))
           (job-data (gethash job-id shipit--check-item-job-data)))
      (when job-data
        (shipit--render-check-item-steps section job-data)))))

(defun shipit--toggle-check-log-timestamps ()
  "Toggle timestamp display in check-item log output and re-render."
  (interactive)
  (setq shipit-actions--show-timestamps
        (not shipit-actions--show-timestamps))
  (message "Log timestamps %s" (if shipit-actions--show-timestamps "on" "off"))
  ;; Find the enclosing check-item section and re-render its steps
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'check-item)))
      (setq section (oref section parent)))
    (when (and section (oref section children))
      (let* ((check (oref section value))
             (job-id (cdr (assq 'id check)))
             (job-data (gethash job-id shipit--check-item-job-data)))
        (when job-data
          (shipit--render-check-item-steps section job-data))))))

(defun shipit--save-check-step-visibility (section)
  "Save visibility of child step sections under SECTION.
Returns a list of step numbers that are expanded (not hidden)."
  (let ((expanded nil))
    (dolist (child (oref section children))
      (when (and (eq (oref child type) 'actions-step)
                 (not (oref child hidden)))
        (let ((step (oref child value)))
          (push (cdr (assq 'number step)) expanded))))
    expanded))

(defun shipit--restore-check-step-visibility (section expanded)
  "Restore visibility of step sections under SECTION.
EXPANDED is a list of step numbers to show."
  (when expanded
    (dolist (child (oref section children))
      (when (and (eq (oref child type) 'actions-step)
                 (memq (cdr (assq 'number (oref child value))) expanded))
        (magit-section-show child)))))

(defun shipit--update-ancestor-ends (section new-end)
  "Update end markers of SECTION's ancestors to include NEW-END."
  (let ((parent (oref section parent)))
    (while parent
      (when (< (marker-position (oref parent end))
               (marker-position new-end))
        (oset parent end (copy-marker new-end)))
      (setq parent (oref parent parent)))))

;;;###autoload
(defun shipit--clear-branch-cache ()
  "Clear all cached data when switching branches or repos."
  ;; Ensure caches are initialized first
  (shipit--ensure-cache-initialized)
  ;; Clear fetched flags (still buffer-local booleans)
  (setq shipit--general-comments-fetched nil
        shipit--checks-fetched nil
        shipit--inline-comments-fetched nil)
  ;; Clear buffer-local caches (these are lists, not hash tables)
  (setq shipit--cached-general-comments nil
        shipit--cached-pr-reactions nil
        shipit--cached-pr-checks nil
        shipit--cached-inline-comments nil
        shipit--cached-review-decision nil)
  ;; Keep resolved comments hash for performance - it will be rebuilt with fresh data
  ;; Clear ONLY current repository's entries from shared hash table caches
  (let ((current-repo (shipit--get-repo-from-remote)))
    (when current-repo
      ;; Clear approval status for current repo only
      (when (boundp 'shipit--cached-approval-status)
        (let ((removed-count 0))
          (maphash (lambda (key _value)
                     (when (string-prefix-p (format "%s:" current-repo) key)
                       (remhash key shipit--cached-approval-status)
                       (setq removed-count (1+ removed-count))))
                   shipit--cached-approval-status)
          (shipit--debug-log "Cleared %d approval status entries for repo %s" removed-count current-repo)))

      ;; Clear branch PRs for current repo only
      (when (boundp 'shipit--cached-branch-prs)
        (let ((removed-count 0))
          (maphash (lambda (key _value)
                     (when (string-prefix-p (format "%s:" current-repo) key)
                       (remhash key shipit--cached-branch-prs)
                       (setq removed-count (1+ removed-count))))
                   shipit--cached-branch-prs)
          (shipit--debug-log "Cleared %d branch PR entries for repo %s" removed-count current-repo)))

      ;; Clear comment caches for current repo only
      (when (boundp 'shipit--comment-cache)
        (let ((removed-count 0))
          (maphash (lambda (key _value)
                     (when (string-match-p (format "^[0-9]+:%s/" current-repo) key) ; PR:repo/file format
                       (remhash key shipit--comment-cache)
                       (setq removed-count (1+ removed-count))))
                   shipit--comment-cache)
          (shipit--debug-log "Cleared %d comment cache entries for repo %s" removed-count current-repo)))

      ;; DON'T clear reaction cache on refresh - ETag caching handles freshness automatically
      ;; Only clear reaction cache when switching repos/branches, not on manual refresh
      (shipit--debug-log "Preserving reaction cache for ETag efficiency (cleared only on repo/branch switch)")

      ;; Clear file content cache used for stale comment detection
      (when (boundp 'shipit--file-content-cache)
        (let ((removed-count 0))
          (maphash (lambda (key _value)
                     (when (string-prefix-p (format "%s:" current-repo) key)
                       (remhash key shipit--file-content-cache)
                       (setq removed-count (1+ removed-count))))
                   shipit--file-content-cache)
          (shipit--debug-log "Cleared %d file content cache entries for repo %s" removed-count current-repo))))))

(defun shipit--insert-checks-section-placeholder (_repo _pr-number)
  "Insert a placeholder checks section while data loads.
REPO and PR-NUMBER are kept for API compatibility but unused."
  (magit-insert-section (checks nil t)  ; hidden by default
    (magit-insert-heading
      (format "%s Checks (loading...)" (shipit--get-pr-field-icon "checks" "🧪")))
    (magit-insert-section-body
      (insert "   Fetching checks...\n")
      (insert "\n"))))

(defun shipit--fetch-checks-async (repo pr-number pr-data callback)
  "Fetch PR checks asynchronously for PR-NUMBER in REPO.
PR-DATA is the PR data alist (used to get head SHA/ref).
Calls CALLBACK with the processed checks list when complete.
If checks are disabled or PR is closed, calls callback with nil."
  (shipit--debug-log "ASYNC-CHECKS: Starting async checks fetch for PR #%s in %s" pr-number repo)

  (cond
   ;; Check if checks are disabled
   ((not shipit-show-pr-checks)
    (shipit--debug-log "ASYNC-CHECKS: Checks disabled, calling callback with nil")
    (funcall callback nil))

   ;; Check cache first
   (shipit--checks-fetched
    (shipit--debug-log "ASYNC-CHECKS: Using cached checks data (%d checks)" (length shipit--cached-pr-checks))
    (funcall callback shipit--cached-pr-checks))

   ;; Need to fetch
   (t
    ;; Check PR state from provided pr-data
    (let ((pr-state (cdr (assq 'state pr-data))))
      (cond
       ;; Closed/merged PR - skip checks
       ((or (string= pr-state "closed") (string= pr-state "merged"))
        (shipit--debug-log "ASYNC-CHECKS: PR is %s, skipping checks" pr-state)
        (funcall callback 'closed))

       ;; Open PR - fetch checks using pr-data we already have
       (t
        (shipit--debug-log "ASYNC-CHECKS: PR is open, fetching checks")
        (shipit--fetch-checks-async-with-pr-data repo pr-data callback)))))))

(defun shipit--fetch-checks-async-with-pr-data (repo pr-data callback)
  "Continue async checks fetch with PR-DATA already available.
Resolves the backend here and threads it through all async helpers.
CALLBACK is called with the check results when complete."
  (let* ((head (cdr (assq 'head pr-data)))
         (head-sha (cdr (assq 'sha head)))
         (head-ref (cdr (assq 'ref head))))
    (if (and head-sha head-ref)
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved)))
          (if (plist-get backend :fetch-check-suites-async)
              ;; Async path: paginated suite fetch + parallel runs fetch
              (shipit--fetch-check-suites-async backend config head-ref head-sha 1 100 '() callback)
            ;; Generic path: use required :fetch-checks
            (shipit--debug-log "ASYNC-CHECKS: No :fetch-check-suites-async, using :fetch-checks")
            (let ((checks (funcall (plist-get backend :fetch-checks) config head-sha)))
              (funcall callback (or checks '())))))
      ;; No valid head info
      (shipit--debug-log "ASYNC-CHECKS: No head SHA/ref available")
      (funcall callback nil))))

(defun shipit--fetch-check-suites-async (backend config head-ref head-sha page per-page all-suites callback)
  "Fetch check suites asynchronously with pagination via BACKEND dispatch.
CONFIG provides repo context.  CALLBACK is called with the check results."
  (shipit--debug-log "ASYNC-CHECKS: Fetching check suites page %d for %s" page head-ref)
  (funcall (plist-get backend :fetch-check-suites-async)
           config head-ref page per-page
           (lambda (data)
             (if data
                 (let* ((check-suites (cdr (assq 'check_suites data)))
                        (total-count (cdr (assq 'total_count data)))
                        (combined-suites (append all-suites (or check-suites '()))))
                   (if (and check-suites
                            (> (length check-suites) 0)
                            total-count
                            (< (length combined-suites) total-count))
                       ;; More pages available
                       (shipit--fetch-check-suites-async backend config head-ref head-sha
                                                         (1+ page) per-page combined-suites callback)
                     ;; All suites fetched, now fetch check runs for each suite
                     (if combined-suites
                         (shipit--fetch-check-runs-for-suites-async backend config combined-suites callback)
                       ;; No check suites found
                       (shipit--debug-log "ASYNC-CHECKS: No check suites found")
                       (funcall callback '()))))
               ;; Error or no data - try SHA fallback
               (shipit--debug-log "ASYNC-CHECKS: Check suites request failed, trying SHA fallback")
               (shipit--fetch-check-suites-by-sha-async backend config head-sha 1 100 '() callback)))))

(defun shipit--fetch-check-suites-by-sha-async (backend config head-sha page per-page all-suites callback)
  "Fetch check suites by SHA asynchronously (fallback) via BACKEND dispatch.
CALLBACK is called with the check results when complete."
  (shipit--debug-log "ASYNC-CHECKS: Fetching check suites by SHA page %d" page)
  (funcall (plist-get backend :fetch-check-suites-async)
           config head-sha page per-page
           (lambda (data)
             (if data
                 (let* ((check-suites (cdr (assq 'check_suites data)))
                        (total-count (cdr (assq 'total_count data)))
                        (combined-suites (append all-suites (or check-suites '()))))
                   (if (and check-suites
                            (> (length check-suites) 0)
                            total-count
                            (< (length combined-suites) total-count))
                       ;; More pages
                       (shipit--fetch-check-suites-by-sha-async backend config head-sha
                                                                (1+ page) per-page combined-suites callback)
                     ;; All suites fetched
                     (if combined-suites
                         (shipit--fetch-check-runs-for-suites-async backend config combined-suites callback)
                       (funcall callback '()))))
               ;; Error case - return empty checks
               (funcall callback '())))))

(defun shipit--fetch-check-runs-for-suites-async (backend config check-suites callback)
  "Fetch check runs for all CHECK-SUITES asynchronously via BACKEND dispatch.
CALLBACK is called with the processed check results when complete.
This function first fetches workflow names (GitHub-only), then check runs."
  (shipit--debug-log "ASYNC-CHECKS: Fetching check runs for %d suites" (length check-suites))
  (if (= (length check-suites) 0)
      (funcall callback '())
    ;; Phase 1: Extract suite info and identify which need workflow name fetches
    (let ((suite-info-list
           (mapcar (lambda (suite)
                     (let* ((suite-id (cdr (assq 'id suite)))
                            (app (cdr (assq 'app suite)))
                            (app-name (when app (cdr (assq 'name app))))
                            (workflow-run (cdr (assq 'workflow_run suite)))
                            (workflow-run-name (when workflow-run
                                                 (cdr (assq 'name workflow-run))))
                            (workflow-id (when workflow-run
                                           (cdr (assq 'workflow_id workflow-run)))))
                       (list :suite-id suite-id
                             :app-name app-name
                             :workflow-run-name workflow-run-name
                             :workflow-id workflow-id)))
                   check-suites)))
      ;; Phase 2: Fetch missing workflow names, then fetch check runs
      (shipit--fetch-workflow-names-then-runs-async backend config suite-info-list callback))))

(defun shipit--fetch-workflow-names-then-runs-async (backend config suite-info-list callback)
  "Fetch workflow names then check runs via BACKEND dispatch.
Workflow name fetching is gated behind :fetch-workflow-info-async
\(GitHub-only).  CALLBACK is called with the final check runs."
  (let ((repo (plist-get config :repo))
        (workflow-info-fn (plist-get backend :fetch-workflow-info-async))
        (workflow-ids-to-fetch '()))
    ;; Only collect workflow IDs if the backend supports fetching them
    (when workflow-info-fn
      (dolist (suite-info suite-info-list)
        (let* ((app-name (plist-get suite-info :app-name))
               (workflow-id (plist-get suite-info :workflow-id))
               (cache-key (when workflow-id (format "%s:%s" repo workflow-id))))
          (shipit--debug-log "ASYNC-WORKFLOW: suite app-name=%s workflow-id=%s cache-key=%s cached=%s"
                             app-name workflow-id cache-key
                             (when cache-key (gethash cache-key shipit--workflow-name-cache)))
          (when (and (equal app-name "GitHub Actions")
                     workflow-id
                     (not (gethash cache-key shipit--workflow-name-cache))
                     (not (member workflow-id workflow-ids-to-fetch)))
            (push workflow-id workflow-ids-to-fetch)))))
    (shipit--debug-log "ASYNC-WORKFLOW: %d workflow IDs to fetch" (length workflow-ids-to-fetch))
    (if (null workflow-ids-to-fetch)
        ;; No workflow names to fetch, proceed directly to check runs
        (shipit--fetch-check-runs-with-names-async backend config suite-info-list callback)
      ;; Fetch missing workflow names in parallel via backend
      (let ((fetch-remaining (length workflow-ids-to-fetch))
            (fetch-completed 0))
        (shipit--debug-log "ASYNC-CHECKS: Fetching %d workflow names" fetch-remaining)
        (dolist (workflow-id workflow-ids-to-fetch)
          (let ((cache-key (format "%s:%s" repo workflow-id)))
            (funcall workflow-info-fn config workflow-id
                     (lambda (data)
                       (when data
                         (let ((workflow-name (cdr (assq 'name data))))
                           (when workflow-name
                             (puthash cache-key workflow-name shipit--workflow-name-cache)
                             (shipit--debug-log "ASYNC-CHECKS: Cached workflow: %s" workflow-name))))
                       (setq fetch-completed (1+ fetch-completed))
                       (when (= fetch-completed fetch-remaining)
                         (shipit--fetch-check-runs-with-names-async
                          backend config suite-info-list callback))))))))))


(defun shipit--fetch-check-runs-with-names-async (backend config suite-info-list callback)
  "Fetch check runs for all suites with workflow names resolved.
BACKEND is the backend plist, CONFIG provides repo context,
SUITE-INFO-LIST contains suite metadata,
CALLBACK is called with the final check runs."
  (let ((all-runs '())
        (repo (plist-get config :repo))
        (suites-remaining (length suite-info-list))
        (suites-processed 0))
    (if (= suites-remaining 0)
        (funcall callback '())
      (dolist (suite-info suite-info-list)
        (let* ((suite-id (plist-get suite-info :suite-id))
               (app-name (plist-get suite-info :app-name))
               (workflow-id (plist-get suite-info :workflow-id))
               (workflow-run-name (plist-get suite-info :workflow-run-name))
               (cache-key (when workflow-id (format "%s:%s" repo workflow-id)))
               (cached-workflow-name (when cache-key
                                       (gethash cache-key shipit--workflow-name-cache))))
          (funcall (plist-get backend :fetch-suite-check-runs-async)
                   config suite-id
                   (lambda (data)
                     (let ((runs (when data (cdr (assq 'check_runs data)))))
                       (dolist (run runs)
                         (let* ((check-name (cdr (assq 'name run)))
                                (workflow-name (or cached-workflow-name
                                                   (if (equal app-name "GitHub Actions")
                                                       "GitHub Actions"
                                                     (or app-name "Unknown"))))
                                (resolved-run-name (or workflow-run-name
                                                       (when (and (equal app-name "GitHub Actions") check-name)
                                                         (shipit--extract-workflow-from-check-name check-name))))
                                (enhanced-run run))
                           (push (cons 'workflow-name workflow-name) enhanced-run)
                           (push (cons 'workflow-run-name resolved-run-name) enhanced-run)
                           (push enhanced-run all-runs)))
                       (setq suites-processed (1+ suites-processed))
                       (when (= suites-processed suites-remaining)
                         (shipit--process-async-checks-complete backend config all-runs callback))))))))))

(defun shipit--process-async-checks-complete (backend config check-runs callback)
  "Process completed async check runs and call CALLBACK.
BACKEND is the backend plist, CONFIG provides repo context."
  (shipit--debug-log "ASYNC-CHECKS: Processing %d check runs" (length check-runs))

  (if (or (null check-runs) (= (length check-runs) 0))
      ;; Empty check runs - call callback immediately
      (progn
        (setq shipit--cached-pr-checks '())
        (setq shipit--checks-fetched t)
        (setq shipit--checks-loading nil)
        (funcall callback '()))
    ;; Non-empty - collect unique run IDs that need workflow name fetching
    (let ((run-ids-to-fetch (make-hash-table :test 'equal)))
      ;; Find all unique run IDs that need fetching
      (dolist (check check-runs)
        (let* ((raw-workflow-name (or (cdr (assq 'workflow-name check))
                                      (cdr (assq 'workflow_name check))))
               (html-url (cdr (assq 'html_url check))))
          (when (and (or (null raw-workflow-name)
                         (string= raw-workflow-name "GitHub Actions"))
                     html-url
                     (string-match "/actions/runs/\\([0-9]+\\)" html-url))
            (let ((run-id (match-string 1 html-url)))
              (unless (gethash run-id shipit--workflow-run-cache)
                (puthash run-id t run-ids-to-fetch))))))

      (let ((ids-list '()))
        (maphash (lambda (k _v) (push k ids-list)) run-ids-to-fetch)
        (shipit--debug-log "ASYNC-CHECKS: Need to fetch %d unique workflow names" (length ids-list))

        (if (null ids-list)
            ;; No workflow names to fetch - process immediately
            (shipit--finalize-async-checks check-runs callback)
          ;; Fetch workflow names in parallel, then process
          (shipit--fetch-workflow-names-for-runs-async
           backend config ids-list check-runs callback))))))

(defun shipit--fetch-workflow-names-for-runs-async (backend config run-ids check-runs callback)
  "Fetch workflow names for RUN-IDS and process CHECK-RUNS, then call CALLBACK.
BACKEND is the backend plist, CONFIG provides repo context."
  (let* ((ids-remaining (length run-ids))
         (ids-completed 0))
    (shipit--debug-log "ASYNC-WORKFLOW-FETCH: Fetching %d workflow names" ids-remaining)
    (let ((action-run-fn (plist-get backend :fetch-action-run-info-async)))
      (if (not action-run-fn)
          ;; Backend doesn't support action run info (e.g. GitLab) - skip
          (progn
            (shipit--debug-log "ASYNC-WORKFLOW-FETCH: Backend lacks :fetch-action-run-info-async, skipping")
            (shipit--save-workflow-run-cache)
            (shipit--finalize-async-checks check-runs callback))
        (dolist (run-id run-ids)
          (funcall action-run-fn config run-id
                   (lambda (data)
                     (when data
                       (let ((workflow-name (or (cdr (assq 'name data))
                                                (when-let ((path (cdr (assq 'path data))))
                                                  (shipit--workflow-path-to-name path))
                                                "GitHub Actions")))
                         (puthash run-id workflow-name shipit--workflow-run-cache)))
                     (setq ids-completed (1+ ids-completed))
                     (when (= ids-completed ids-remaining)
                       (shipit--debug-log "ASYNC-WORKFLOW-FETCH: All %d workflow names fetched" ids-remaining)
                       (shipit--save-workflow-run-cache)
                       (shipit--finalize-async-checks check-runs callback)))))))))

(defun shipit--finalize-async-checks (check-runs callback)
  "Finalize processing of CHECK-RUNS after workflow names are cached, call CALLBACK."
  (let ((checks (mapcar (lambda (check)
                          (let* ((name (cdr (assq 'name check)))
                                 (status (cdr (assq 'status check)))
                                 (conclusion (cdr (assq 'conclusion check)))
                                 (html-url (cdr (assq 'html_url check)))
                                 (raw-workflow-name (or (cdr (assq 'workflow-name check))
                                                        (cdr (assq 'workflow_name check))))
                                 (raw-run-name (cdr (assq 'workflow-run-name check)))
                                 ;; Resolve workflow name from cache
                                 (workflow-name (if (and raw-workflow-name
                                                         (not (string= raw-workflow-name "GitHub Actions")))
                                                    raw-workflow-name
                                                  (if (and html-url (string-match "/actions/runs/\\([0-9]+\\)" html-url))
                                                      (let ((run-id (match-string 1 html-url)))
                                                        (or (gethash run-id shipit--workflow-run-cache)
                                                            (shipit--extract-workflow-from-check-name name)))
                                                    (shipit--extract-workflow-from-check-name name))))
                                 ;; Resolve run name
                                 (workflow-run-name (or raw-run-name
                                                        (shipit--extract-workflow-from-check-name name))))
                            `((name . ,name)
                              (id . ,(cdr (assq 'id check)))
                              (status . ,status)
                              (conclusion . ,conclusion)
                              (html_url . ,html-url)
                              (workflow-name . ,workflow-name)
                              (workflow-run-name . ,workflow-run-name))))
                        check-runs)))
    ;; Sort by workflow/run/name
    (setq checks (sort checks
                       (lambda (a b)
                         (string< (concat (or (cdr (assq 'workflow-name a)) "")
                                          "/" (or (cdr (assq 'workflow-run-name a)) "")
                                          "/" (cdr (assq 'name a)))
                                  (concat (or (cdr (assq 'workflow-name b)) "")
                                          "/" (or (cdr (assq 'workflow-run-name b)) "")
                                          "/" (cdr (assq 'name b)))))))
    ;; Update cache
    (setq shipit--cached-pr-checks checks)
    (setq shipit--checks-fetched t)
    (setq shipit--checks-loading nil)
    ;; Call the callback
    (shipit--debug-log "ASYNC-CHECKS: Finalized with %d processed checks" (length checks))
    (funcall callback checks)))

(defun shipit--insert-checks-section (repo pr-number)
  "Insert PR checks section based on shipit-show-pr-checks setting."
  (if shipit-show-pr-checks
      (shipit--insert-pr-checks-enabled repo pr-number)
    (shipit--insert-pr-checks-disabled)))

(defun shipit--insert-pr-checks-disabled ()
  "Insert disabled checks section."
  (magit-insert-section (checks nil t)
                        (magit-insert-heading (format "%s Checks (disabled)" (shipit--get-pr-field-icon "checks" "⚪")))
                        (progn
                         (insert "   PR checks display is disabled. ")
                         (insert "Enable with M-x customize-variable RET shipit-show-pr-checks\n")
                         (insert "\n"))))

(defun shipit--insert-pr-checks-enabled (repo pr-number)
  "Insert enabled PR checks section with full functionality."
  (let* ((pr-data (shipit-get-pull-request pr-number repo))
         (pr-state (when pr-data (cdr (assq 'state pr-data)))))
    (if (string= pr-state "closed")
        ;; Skip checks for closed PRs
        (magit-insert-section (checks nil t)
                              (magit-insert-heading (format "%s Checks (skipped for %s PR)"
                                                            (shipit--get-pr-field-icon "checks" "🧪")
                                                            "closed"))
                              (progn
                                (insert "\n")))
      ;; Normal checks loading for open PRs - LAZY LOADING for performance
      (shipit--debug-log "CHECKS-SECTION: shipit--checks-fetched=%s, cached-checks-length=%s"
                         shipit--checks-fetched
                         (if (boundp 'shipit--cached-pr-checks) (length shipit--cached-pr-checks) "not-bound"))
      (if shipit--checks-fetched
          ;; Display cached checks
          (progn
            (shipit--debug-log "CHECKS-SECTION: Rendering cached checks section with %d checks" (length shipit--cached-pr-checks))
            (magit-insert-section (checks nil t)
                                  (magit-insert-heading (format "%s Checks (%d)"
                                                                (shipit--get-pr-field-icon "checks" (shipit--get-checks-status-emoji shipit--cached-pr-checks))
                                                                (length shipit--cached-pr-checks)))
                                  (progn
                                   (if (= (length shipit--cached-pr-checks) 0)
                                       (progn
                                         (shipit--debug-log "CHECKS-SECTION: Inserting 'No checks found' message")
                                         (insert "   No checks found\n"))
                                     ;; ELSE: Group checks by status
                                   (let* ((failing-checks '())
                                          (in-progress-checks '())
                                          (pending-checks '())
                                          (cancelled-checks '())
                                          (skipped-checks '())
                                          (successful-checks '()))
                                     ;; Categorize checks
                                     (dolist (check shipit--cached-pr-checks)
                                       (let* ((status (cdr (assq 'status check)))
                                              (conclusion (cdr (assq 'conclusion check)))
                                              (name (cdr (assq 'name check))))
                                         ;; Debug: show status and conclusion for cancelled/failed checks
                                         (cond
                                          ;; First, handle explicit conclusions (these take priority)
                                          ((string= conclusion "success")
                                           (push check successful-checks))
                                          ((or (string= conclusion "failure")
                                               (string= conclusion "timed_out"))
                                           (push check failing-checks))
                                          ((string= conclusion "cancelled")
                                           (push check cancelled-checks))
                                          ((or (string= conclusion "skipped")
                                               (string= conclusion "neutral"))
                                           (push check skipped-checks))
                                          ;; Then handle status-based categorization (for jobs without conclusion)
                                          ((string= status "in_progress")
                                           (push check in-progress-checks))
                                          ((or (string= status "queued")
                                               (string= status "pending"))
                                           (push check pending-checks))
                                          ;; Handle cancelled status even without conclusion
                                          ((string= status "cancelled")
                                           (push check cancelled-checks))
                                          ;; Fallback for unknown statuses
                                          (t
                                           (push check failing-checks))))) ; Group unknown with failing for visibility

                                     ;; Display in-progress checks first (most important to see)
                                     (when in-progress-checks
                                       (magit-insert-section (in-progress-checks)  ; not collapsed by default
                                                             (magit-insert-heading (format "   %s In Progress (%d)" (shipit--get-check-status-icon "in_progress" nil "🟡") (length in-progress-checks)))
                                                             (progn
                                                              (shipit--insert-check-list in-progress-checks))))

                                     ;; Display pending checks second
                                     (when pending-checks
                                       (magit-insert-section (pending-checks)  ; not collapsed by default
                                                             (magit-insert-heading (format "   %s Pending (%d)" (shipit--get-check-status-icon "queued" nil "🔵") (length pending-checks)))
                                                             (progn
                                                              (shipit--insert-check-list pending-checks))))

                                     ;; Display cancelled checks third
                                     (when cancelled-checks
                                       (magit-insert-section (cancelled-checks)  ; collapsed by default
                                                             (magit-insert-heading (format "   %s Cancelled (%d)" (shipit--get-check-status-icon nil "cancelled" "⚪") (length cancelled-checks)))
                                                             (progn
                                                              (shipit--insert-check-list cancelled-checks))))

                                     ;; Display failing checks fourth
                                     (when failing-checks
                                       (magit-insert-section (failing-checks)  ; collapsed by default
                                                             (magit-insert-heading (format "   %s Failing checks (%d)" (shipit--get-check-status-icon nil "failure" "❌") (length failing-checks)))
                                                             (progn
                                                              (shipit--insert-check-list failing-checks))))

                                     ;; Display skipped checks fifth
                                     (when skipped-checks
                                       (magit-insert-section (skipped-checks)  ; collapsed by default
                                                             (magit-insert-heading (format "   %s Skipped checks (%d)" (shipit--get-check-status-icon nil "skipped" "⚪") (length skipped-checks)))
                                                             (progn
                                                              (shipit--insert-check-list skipped-checks))))

                                     ;; Display successful checks last
                                     (when successful-checks
                                       (magit-insert-section (successful-checks)  ; collapsed by default
                                                             (magit-insert-heading (format "   %s Successful checks (%d)" (shipit--get-check-status-icon nil "success" "✅") (length successful-checks)))
                                                             (progn
                                                               (shipit--insert-check-list successful-checks))))))
                                   (insert "\n"))))
        ;; ELSE: No cached data - LAZY LOADING: show collapsed section, load on demand
        (let ((checks-section nil))
          (shipit--debug-log "CHECKS-SECTION: Rendering lazy loading section (no cached data)")
          (setq checks-section
                (magit-insert-section (checks nil t)
                  (magit-insert-heading (format "%s Checks" (shipit--get-pr-field-icon "checks" "🧪")))
                  ;; Body content - shown when expanded
                  (let ((body-start (point)))
                    (insert (format "   Press RET to load checks from %s\n"
                                    (or (ignore-errors (plist-get (shipit-pr--get-backend) :name)) "CI")))
                    ;; Add keymap and properties for on-demand loading via RET/SPC
                    (add-text-properties body-start (point)
                                         `(keymap ,shipit-checks-keymap
                                                  shipit-repo ,repo
                                                  shipit-pr-number ,pr-number)))))
          ;; Explicitly hide the section
          (when checks-section
            (magit-section-hide checks-section)))))))

(defun shipit--group-checks-hierarchically (checks)
  "Group CHECKS into 3-level hierarchy: workflow-name > workflow-run-name > jobs.
CHECKS are alists in the common shipit check shape (see
`shipit-pr--output-contracts').  The `workflow-name' field provides the
top-level grouping (GitHub: workflow name, GitLab: pipeline stage).
Returns a nested hash table structure."
  (let ((workflow-groups (make-hash-table :test 'equal)))
    (dolist (check checks)
      (let* ((workflow-name (or (cdr (assq 'workflow-name check)) "Unknown Workflow"))
             (workflow-run-name (or (cdr (assq 'workflow-run-name check)) workflow-name))
             (run-groups (or (gethash workflow-name workflow-groups)
                             (let ((new-table (make-hash-table :test 'equal)))
                               (puthash workflow-name new-table workflow-groups)
                               new-table)))
             (existing-runs (gethash workflow-run-name run-groups)))
        (puthash workflow-run-name (cons check existing-runs) run-groups)))
    ;; Sort checks within each run
    (maphash (lambda (_workflow run-table)
               (maphash (lambda (run-name checks-list)
                          (puthash run-name
                                   (sort checks-list
                                         (lambda (a b)
                                           (string< (cdr (assq 'name a))
                                                    (cdr (assq 'name b)))))
                                   run-table))
                        run-table))
             workflow-groups)
    workflow-groups))

(defun shipit--insert-check-list (checks)
  "Insert a list of checks with 3-level hierarchy: workflow > run > jobs."
  (let ((workflow-groups (shipit--group-checks-hierarchically checks))
        (workflow-names '()))
    ;; Collect and sort workflow names
    (maphash (lambda (workflow-name _run-table)
               (push workflow-name workflow-names))
             workflow-groups)
    (setq workflow-names (sort workflow-names (lambda (a b) (string< (downcase a) (downcase b)))))
    ;; Display each workflow as a sub-section
    (dolist (workflow-name workflow-names)
      (let* ((run-table (gethash workflow-name workflow-groups))
             (run-names '())
             (total-checks 0))
        ;; Collect run names and count total checks
        (maphash (lambda (run-name run-checks)
                   (push run-name run-names)
                   (setq total-checks (+ total-checks (length run-checks))))
                 run-table)
        (setq run-names (sort run-names (lambda (a b) (string< (downcase a) (downcase b)))))
        ;; Check if workflow-name equals the only run-name (single-run workflow)
        (let ((single-run-p (and (= (length run-names) 1)
                                 (string= workflow-name (car run-names)))))
          (if single-run-p
              ;; Single run that matches workflow name - flatten to 2 levels
              (let* ((run-checks (gethash (car run-names) run-table))
                     (run-id (shipit--extract-run-id-from-checks run-checks)))
                (magit-insert-section (workflow `((name . ,workflow-name)
                                                  (run-id . ,run-id)))
                  (magit-insert-heading
                   (format "      %s (%d)" workflow-name (length run-checks)))
                  (shipit--insert-check-items run-checks "        ")))
            ;; Multiple runs or run-name differs - show 3 levels
            (magit-insert-section (workflow workflow-name)
              (magit-insert-heading
               (format "      %s (%d)" workflow-name total-checks))
              (dolist (run-name run-names)
                (let* ((run-checks (gethash run-name run-table))
                       (run-id (shipit--extract-run-id-from-checks run-checks)))
                  (magit-insert-section (workflow-run `((name . ,run-name)
                                                        (run-id . ,run-id)))
                    (magit-insert-heading
                     (format "        %s (%d)" run-name (length run-checks)))
                    (shipit--insert-check-items run-checks "          ")))))))))))

(defun shipit--try-open-check-workflow ()
  "If point is on a workflow or workflow-run section, open the actions buffer.
Returns non-nil if handled."
  (when (fboundp 'magit-current-section)
    (let ((section (magit-current-section)))
      (when section
        (let ((type (oref section type))
              (value (oref section value)))
          (when (and (memq type '(workflow workflow-run))
                     (consp value))
            (let ((run-id (cdr (assq 'run-id value)))
                  (repo (bound-and-true-p shipit-buffer-repo)))
              (when (and run-id repo)
                (shipit-open-actions-run repo run-id)
                t))))))))

(defun shipit--extract-run-id-from-url (html-url)
  "Extract the Actions run ID from HTML-URL.
Returns nil if URL doesn't contain an Actions run path."
  (when (and html-url (string-match "/actions/runs/\\([0-9]+\\)" html-url))
    (match-string 1 html-url)))

(defun shipit--extract-run-id-from-checks (checks)
  "Extract the run ID from the first check in CHECKS that has one."
  (cl-some (lambda (check)
             (shipit--extract-run-id-from-url (cdr (assq 'html_url check))))
           checks))

(defun shipit--open-check-in-actions (check-name html-url)
  "Open the Actions run for CHECK-NAME from HTML-URL.
If CHECK-NAME is non-nil, auto-expand that job in the actions buffer."
  (let ((run-id (shipit--extract-run-id-from-url html-url))
        (repo (bound-and-true-p shipit-buffer-repo)))
    (if (and run-id repo)
        (shipit-open-actions-run repo run-id check-name)
      (when html-url
        (browse-url html-url)))))


(defun shipit--find-run-id-in-children (section)
  "Find a run-id from child sections of SECTION."
  (cl-some (lambda (child)
             (let ((val (oref child value)))
               (cond
                ((and (consp val) (cdr (assq 'run-id val)))
                 (cdr (assq 'run-id val)))
                ((and (consp val) (cdr (assq 'html_url val)))
                 (shipit--extract-run-id-from-url (cdr (assq 'html_url val)))))))
           (oref section children)))

(defun shipit--insert-check-items (checks indent)
  "Insert individual check items as collapsible magit sections with INDENT prefix."
  (dolist (check checks)
    (let* ((name (cdr (assq 'name check)))
           (status (cdr (assq 'status check)))
           (conclusion (cdr (assq 'conclusion check)))
           (html-url (cdr (assq 'html_url check)))
           (status-face (cond ((string= conclusion "success") 'success)
                              ((string= conclusion "failure") 'error)
                              ((string= conclusion "cancelled") 'warning)
                              ((string= conclusion "skipped") 'magit-dimmed)
                              ((string= status "in_progress") 'magit-process-ng)
                              (t 'default)))
           (status-symbol (shipit--get-check-status-icon status conclusion
                            (cond ((string= conclusion "success") "✓")
                                  ((string= conclusion "failure") "✗")
                                  ((string= conclusion "cancelled") "!")
                                  ((string= conclusion "skipped") "−")
                                  ((string= status "in_progress") "…")
                                  (t "?")))))
      (let ((sect (magit-insert-section (check-item check)
                    (magit-insert-heading
                      (concat indent
                              (propertize status-symbol 'face status-face)
                              " "
                              (propertize name 'face (if html-url 'link 'default))
                              "\n")))))
        (magit-section-hide sect)))))

(defun shipit--fetch-pr-checks (repo pr-number)
  "Fetch PR checks status with pagination support and refresh magit buffer."
  (let ((current-buffer (current-buffer)))
    (shipit--debug-log "Fetching PR info for checks: PR #%s in %s" pr-number repo)
    (condition-case err
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (data (funcall (plist-get backend :fetch-pr) config pr-number)))
          (if data
              ;; Success - process the PR data
              (when (buffer-live-p current-buffer)
                (with-current-buffer current-buffer
                  (let ((head-sha (cdr (assq 'sha (cdr (assq 'head data))))))
                    (if head-sha
                        (shipit--fetch-all-check-runs-with-data repo data current-buffer)
                      (shipit--display-checks-error "Could not get PR head commit")))))
            (shipit--display-checks-error "No PR data returned")))
      (error
       (shipit--debug-log "Check fetch error: %s" (error-message-string err))
       (shipit--display-checks-error "Error fetching PR data")))))

(defun shipit--fetch-all-check-runs-with-data (repo pr-data current-buffer)
  "Fetch all check runs using check-suites with PR data already available."
  (let* ((head (cdr (assq 'head pr-data)))
         (head-sha (cdr (assq 'sha head)))
         (head-ref (cdr (assq 'ref head))))
    (if (and head-sha head-ref)
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (target-buffer current-buffer))
          (if (plist-get backend :fetch-check-suites-async)
              ;; Async path: paginated suite fetch + parallel runs fetch
              (shipit--fetch-check-suites-async
               backend (cdr resolved) head-ref head-sha 1 100 '()
               (lambda (all-checks)
                 (when (buffer-live-p target-buffer)
                   (with-current-buffer target-buffer
                     (setq shipit--cached-pr-checks (or all-checks '()))
                     (setq shipit--checks-fetched t)
                     (setq shipit--checks-loading nil)
                     (shipit--display-checks (or all-checks '()))))))
            ;; Generic path: use required :fetch-checks
            (shipit--debug-log "CHECKS: No :fetch-check-suites, using :fetch-checks")
            (let* ((config (cdr resolved))
                   (checks (funcall (plist-get backend :fetch-checks) config head-sha)))
              (when (buffer-live-p target-buffer)
                (with-current-buffer target-buffer
                  (setq shipit--cached-pr-checks (or checks '()))
                  (setq shipit--checks-fetched t)
                  (setq shipit--checks-loading nil)
                  (shipit--display-checks (or checks '())))))))
      ;; No valid head info, display error
      (shipit--display-checks-error "Could not get PR head commit"))))

(defun shipit--try-check-suites-with-ref (backend config head-ref head-sha current-buffer)
  "Try check-suites with branch ref.
Fallback to SHA, then to check-runs if both fail.
BACKEND is the backend plist, CONFIG provides repo context."
  (shipit--fetch-all-check-suites backend config head-ref head-sha current-buffer 1 100 '()))

(defun shipit--fetch-all-check-suites (backend config head-ref head-sha current-buffer page per-page all-check-suites)
  "Fetch all check suites with pagination, fallback from ref to SHA to check-runs.
BACKEND is the backend plist, CONFIG provides repo context."
  (if (not head-ref)
      ;; Invalid head-ref, try SHA fallback
      (if head-sha
          (shipit--fetch-all-check-suites-sha backend config head-sha current-buffer page per-page all-check-suites)
        (shipit--display-checks '()))
    (shipit--debug-log "Fetching check suites for ref %s page %d" head-ref page)
    (funcall (plist-get backend :fetch-check-suites-async)
             config head-ref page per-page
             (lambda (data)
               (if data
                   ;; Success with branch ref
                   (let* ((check-suites (cdr (assq 'check_suites data)))
                          (total-count (cdr (assq 'total_count data)))
                          (combined-suites (append all-check-suites (or check-suites '()))))
                     (if (and check-suites
                              (> (length check-suites) 0)
                              total-count
                              (< (length combined-suites) total-count))
                         ;; More pages available, fetch next page
                         (shipit--fetch-all-check-suites backend config head-ref head-sha current-buffer
                                                         (1+ page) per-page combined-suites)
                       ;; No more pages or empty page, process all suites
                       (if (> (length combined-suites) 0)
                           (shipit--process-check-suites-simple backend config combined-suites current-buffer)
                         ;; No check suites found, try SHA fallback
                         (shipit--debug-log "No check suites found with branch ref, trying SHA...")
                         (shipit--fetch-all-check-suites-sha backend config head-sha current-buffer 1 per-page '()))))
                 ;; Failed with branch ref, try SHA
                 (shipit--fetch-all-check-suites-sha backend config head-sha current-buffer 1 per-page '()))))))

(defun shipit--fetch-all-check-suites-sha (backend config head-sha current-buffer page per-page all-check-suites)
  "Fetch all check suites using SHA with pagination.
Fallback to check-runs if none found.
BACKEND is the backend plist, CONFIG provides repo context."
  (if (not head-sha)
      ;; Invalid head-sha, display empty results
      (shipit--display-checks '())
    (funcall (plist-get backend :fetch-check-suites-async)
             config head-sha page per-page
             (lambda (data)
               (if data
                   ;; Success with SHA
                   (let* ((check-suites (cdr (assq 'check_suites data)))
                          (total-count (cdr (assq 'total_count data)))
                          (combined-suites (append all-check-suites (or check-suites '()))))
                     (if (and check-suites
                              (> (length check-suites) 0)
                              total-count
                              (< (length combined-suites) total-count))
                         ;; More pages available, fetch next page
                         (shipit--fetch-all-check-suites-sha backend config head-sha current-buffer
                                                             (1+ page) per-page combined-suites)
                       ;; No more pages or empty page, process all suites
                       (if (> (length combined-suites) 0)
                           (shipit--process-check-suites-simple backend config combined-suites current-buffer)
                         ;; No check suites found, fallback to check-runs
                         (shipit--fetch-check-runs-with-fallback backend config head-sha current-buffer 1 100 '()))))
                 ;; API error, use check-runs fallback
                 (shipit--fetch-check-runs-with-fallback backend config head-sha current-buffer 1 100 '()))))))

(defun shipit--process-check-suites-simple (backend config check-suites current-buffer)
  "Process check suites using BATCH PROCESSING to eliminate N+1 API calls.
BACKEND is the backend plist, CONFIG provides repo context."
  ;; Handle case where check-suites is empty
  (if (not (and check-suites (listp check-suites) (> (length check-suites) 0)))
      ;; No check suites found, display empty results immediately
      (shipit--display-checks '())
    ;; BATCH PROCESSING: Process all suites in parallel (eliminates N+1 API calls)
    (shipit--process-check-suites-batch backend config check-suites current-buffer)))

(defun shipit--process-check-suites-batch (backend config check-suites current-buffer)
  "Process check suites using parallel batch processing (eliminates N+1 API calls).
BACKEND is the backend plist, CONFIG provides repo context."
  (shipit--debug-log "Starting batch processing of %d check suites..." (length check-suites))

  ;; Phase 1: Extract all suite data in parallel (no API calls)
  ;; Note: workflow_run.name is the RUN name (instance), not the workflow name
  ;; The actual workflow name comes from the workflow ID API or cache
  (let ((suite-data-list
         (mapcar (lambda (suite)
                   (let* ((suite-id (cdr (assq 'id suite)))
                          (workflow-run (cdr (assq 'workflow_run suite)))
                          ;; workflow_run.name is the run name, NOT the workflow name
                          (workflow-run-name (when workflow-run (cdr (assq 'name workflow-run))))
                          (app (cdr (assq 'app suite)))
                          (app-name (when app (cdr (assq 'name app))))
                          (workflow-id (when workflow-run (cdr (assq 'workflow_id workflow-run)))))
                     ;; Return processed suite info
                     (list :suite-id suite-id
                           :workflow-run workflow-run
                           :workflow-run-name workflow-run-name
                           :app-name app-name
                           :workflow-id workflow-id)))
                 check-suites)))

    ;; Phase 2: Batch fetch all missing workflow names in parallel
    (shipit--batch-fetch-workflow-names backend config suite-data-list current-buffer)))

(defun shipit--batch-fetch-workflow-names (backend config suite-data-list current-buffer)
  "Batch fetch all missing workflow names, then fetch check runs.
BACKEND is the backend plist, CONFIG provides repo context."
  (let ((repo (plist-get config :repo))
        (workflow-info-fn (plist-get backend :fetch-workflow-info-async)))
    ;; Identify suites that need workflow name API calls
    (let ((suites-needing-fetch '())
          (suites-ready '()))

      ;; Categorize suites: those needing API calls vs. those ready to process
      (dolist (suite-info suite-data-list)
        (let* ((app-name (plist-get suite-info :app-name))
               (workflow-id (plist-get suite-info :workflow-id))
               (cache-key (when workflow-id (format "%s:%s" repo workflow-id))))

          (shipit--debug-log "WORKFLOW-NAMES: suite app-name=%s workflow-id=%s cache-key=%s cached=%s"
                             app-name workflow-id cache-key
                             (when cache-key (gethash cache-key shipit--workflow-name-cache)))

          (if (and workflow-info-fn
                   (equal app-name "GitHub Actions") workflow-id
                   (not (gethash cache-key shipit--workflow-name-cache)))
              ;; Needs API fetch
              (push suite-info suites-needing-fetch)
            ;; Ready to process (cached or non-GitHub Actions or backend lacks workflow info)
            (push suite-info suites-ready))))

      (shipit--debug-log "WORKFLOW-NAMES: %d suites need fetch, %d ready"
                         (length suites-needing-fetch) (length suites-ready))

      (if (null suites-needing-fetch)
          ;; No API calls needed, process directly
          (shipit--batch-fetch-check-runs backend config suite-data-list current-buffer)
        ;; Fetch missing workflow names in parallel
        (let ((fetch-remaining (length suites-needing-fetch))
              (fetch-completed 0))
          (shipit--debug-log "WORKFLOW-NAMES: Fetching %d workflow names"
                             (length suites-needing-fetch))
          (dolist (suite-info suites-needing-fetch)
            (let* ((workflow-id (plist-get suite-info :workflow-id))
                   (cache-key (format "%s:%s" repo workflow-id)))
              (funcall workflow-info-fn config workflow-id
                       (lambda (data)
                         (if data
                             (let ((workflow-name (cdr (assq 'name data))))
                               (puthash cache-key workflow-name shipit--workflow-name-cache)
                               (shipit--debug-log "Cached workflow name: %s" workflow-name))
                           (shipit--debug-log "Failed to fetch workflow name for ID %s" workflow-id))
                         (setq fetch-completed (1+ fetch-completed))
                         (when (= fetch-completed fetch-remaining)
                           (shipit--batch-fetch-check-runs backend config suite-data-list current-buffer)))))))))))


(defun shipit--batch-fetch-check-runs (backend config suite-data-list current-buffer)
  "Batch fetch check runs for all suites asynchronously.
BACKEND is the backend plist, CONFIG provides repo context."
  (shipit--debug-log "Batch fetching check runs for %d suites..." (length suite-data-list))
  (let ((repo (plist-get config :repo))
        (all-check-runs '())
        (suites-remaining (length suite-data-list))
        (suites-processed 0))
    (if (= suites-remaining 0)
        (when (buffer-live-p current-buffer)
          (with-current-buffer current-buffer
            (setq shipit--cached-pr-checks '())
            (setq shipit--checks-fetched t)
            (setq shipit--checks-loading nil)
            (shipit--display-checks '())))
      (dolist (suite-info suite-data-list)
        (let* ((suite-id (plist-get suite-info :suite-id))
               (workflow-id (plist-get suite-info :workflow-id))
               (app-name (plist-get suite-info :app-name))
               (workflow-run-name (plist-get suite-info :workflow-run-name))
               (cache-key (when workflow-id (format "%s:%s" repo workflow-id)))
               (cached-workflow-name (when cache-key
                                       (gethash cache-key shipit--workflow-name-cache))))
          (funcall (plist-get backend :fetch-suite-check-runs-async)
                   config suite-id
                   (lambda (data)
                     (when data
                       (let ((check-runs (cdr (assq 'check_runs data))))
                         (dolist (run check-runs)
                           (let* ((check-name (cdr (assq 'name run)))
                                  (workflow-name (or cached-workflow-name
                                                     (if (string= app-name "GitHub Actions")
                                                         "GitHub Actions"
                                                       (or app-name "Unknown"))))
                                  (resolved-run-name (or workflow-run-name
                                                         (when (and (string= app-name "GitHub Actions") check-name)
                                                           (shipit--extract-workflow-from-check-name check-name))))
                                  (enhanced-run run))
                             (push (cons 'workflow-name workflow-name) enhanced-run)
                             (push (cons 'workflow-run-name resolved-run-name) enhanced-run)
                             (push enhanced-run all-check-runs)))))
                     (setq suites-processed (1+ suites-processed))
                     (when (= suites-processed suites-remaining)
                       (shipit--debug-log "Batch processing complete: %d total check runs" (length all-check-runs))
                       (when (buffer-live-p current-buffer)
                         (with-current-buffer current-buffer
                           (setq shipit--cached-pr-checks all-check-runs)
                           (setq shipit--checks-fetched t)
                           (setq shipit--checks-loading nil)
                           (shipit--display-checks all-check-runs)))))))))))

(defun shipit--fetch-check-runs-with-fallback (backend config head-sha current-buffer page per-page all-check-runs)
  "Fetch check runs using fallback approach with improved workflow name extraction.
BACKEND is the backend plist, CONFIG provides repo context."
  (if (not head-sha)
      (shipit--display-checks '())
    (let ((commit-check-runs-fn (plist-get backend :fetch-commit-check-runs-async)))
      (if (not commit-check-runs-fn)
          ;; Backend doesn't support commit check runs - display empty
          (when (buffer-live-p current-buffer)
            (with-current-buffer current-buffer
              (shipit--display-checks '())))
        (funcall commit-check-runs-fn config head-sha page per-page
                 (lambda (data)
                   (if data
                       (let* ((check-runs (cdr (assq 'check_runs data)))
                              (total-count (cdr (assq 'total_count data)))
                              (enhanced-runs (mapcar (lambda (check)
                                                       (let* ((check-name (cdr (assq 'name check)))
                                                              (workflow-name (shipit--extract-workflow-from-check-name check-name)))
                                                         (if (assq 'workflow_name check)
                                                             check
                                                           (cons (cons 'workflow_name workflow-name) check))))
                                                     (or check-runs '())))
                              (combined-runs (append all-check-runs enhanced-runs)))
                         (if (and total-count (< (length combined-runs) total-count))
                             (shipit--fetch-check-runs-with-fallback backend config head-sha current-buffer
                                                                     (+ page 1) per-page combined-runs)
                           (when (buffer-live-p current-buffer)
                             (with-current-buffer current-buffer
                               (shipit--display-checks combined-runs)))))
                     (when (buffer-live-p current-buffer)
                       (with-current-buffer current-buffer
                         (shipit--display-checks '()))))))))))

(defun shipit--display-checks-error (error-msg)
  "Display error message in the checks section."
  ;; Clear the loading flag
  (setq shipit--checks-loading nil)
  (shipit--debug-log "Check fetch error: %s - using full magit refresh instead of manual buffer manipulation" error-msg)
  ;; Instead of manual buffer manipulation that can cause corruption,
  ;; use deferred refresh to avoid cascading refreshes
  )

(defun shipit--display-checks (check-runs)
  "Display check runs by caching them and refreshing the magit status buffer."
  ;; Handle case where check-runs is empty or nil
  (shipit--debug-log "shipit--display-checks called with %s checks" (if check-runs (number-to-string (length check-runs)) "nil"))
  (if (or (null check-runs) (= (length check-runs) 0))
      (progn
        (shipit--debug-log "No check runs found - clearing loading flag and setting empty cache")
        (setq shipit--cached-pr-checks '())
        (setq shipit--checks-loading nil)
        (setq shipit--checks-fetched t)
        (shipit--debug-log "CHECKS-EMPTY: Set shipit--checks-fetched=t, cached-pr-checks=empty")
        ;; Refresh to show "No checks found"
        (when (derived-mode-p 'shipit-mode)
          (shipit--debug-log "CHECKS-EMPTY: Calling shipit-buffer-refresh")
          (shipit-buffer-refresh)))
    ;; Normal processing for non-empty check-runs
    ;; Store checks in cache with workflow names
    ;; Note: check 'workflow-name (hyphen) first, then 'workflow_name (underscore) for compatibility
    (let ((processed-checks (mapcar (lambda (check)
                                      (let* ((name (cdr (assq 'name check)))
                                             (_status (cdr (assq 'status check)))
                                             (_conclusion (cdr (assq 'conclusion check)))
                                             (html-url (cdr (assq 'html_url check)))
                                             ;; Get workflow name - check both key formats
                                             (raw-workflow-name (or (cdr (assq 'workflow-name check))
                                                                    (cdr (assq 'workflow_name check))))
                                             ;; Get workflow run name (the run instance name)
                                             (raw-run-name (cdr (assq 'workflow-run-name check)))
                                             ;; Resolve workflow name if needed
                                             (workflow-name (if (and raw-workflow-name
                                                                     (not (string= raw-workflow-name "GitHub Actions")))
                                                                raw-workflow-name
                                                              ;; For GitHub Actions, try to fetch workflow name from run
                                                              (if (and html-url (string-match "/actions/runs/\\([0-9]+\\)" html-url))
                                                                    (let ((run-id (match-string 1 html-url)))
                                                                      ;; Try cached, then sync fetch, then extract from name
                                                                      (or (shipit--get-workflow-name-from-run-id run-id)
                                                                          (shipit--fetch-workflow-name-for-run-id-sync run-id)
                                                                          (shipit--extract-workflow-from-check-name name)))
                                                                  (shipit--extract-workflow-from-check-name name))))
                                             ;; Resolve run name - use raw if available, else extract from check name
                                             (workflow-run-name (or raw-run-name
                                                                    (shipit--extract-workflow-from-check-name name))))
                                        ;; Return enhanced check data with both workflow-name and workflow-run-name
                                        (let ((enhanced check))
                                          (push (cons 'workflow-name workflow-name) enhanced)
                                          (push (cons 'workflow-run-name workflow-run-name) enhanced)
                                          enhanced)))
                                    check-runs)))
      ;; Sort checks alphabetically by workflow name + check name
      (setq processed-checks (sort processed-checks
                                   (lambda (a b)
                                     (let* ((a-name (cdr (assq 'name a)))
                                            (a-workflow (cdr (assq 'workflow-name a)))
                                            (b-name (cdr (assq 'name b)))
                                            (b-workflow (cdr (assq 'workflow-name b))))
                                       (string< (concat a-workflow "/" a-name)
                                                (concat b-workflow "/" b-name))))))
      ;; Cache the processed checks
      (setq shipit--cached-pr-checks processed-checks)
      ;; Clear loading flag and set fetched flag
      (setq shipit--checks-loading nil)
      (setq shipit--checks-fetched t)
      ;; DEFENSIVE: Cancel any active timeout timer since checks were displayed successfully
      (when shipit--checks-timeout-timer
        (shipit--debug-log "CHECKS-DISPLAY: Defensively cancelling timeout timer - checks displayed successfully")
        (cancel-timer shipit--checks-timeout-timer)
        (setq shipit--checks-timeout-timer nil))

      ;; Trigger async fetching of missing workflow names
      (shipit--fetch-missing-workflow-names check-runs)

      ;; Refresh the buffer to show the cached checks
      (when (derived-mode-p 'shipit-mode)
        (shipit--debug-log "CHECKS-DISPLAY: Calling shipit-buffer-refresh")
        (shipit-buffer-refresh)))))

;; Cache for workflow run ID to workflow name mapping (persistent)
(defvar shipit--workflow-run-cache (make-hash-table :test 'equal)
  "Cache mapping workflow run IDs to workflow names.
This cache is persisted to disk since workflow run names never change.")

(defvar shipit--workflow-run-cache-file
  (expand-file-name "shipit-workflow-run-cache.el" user-emacs-directory)
  "File path for persisting workflow run cache.")

(defun shipit--load-workflow-run-cache ()
  "Load workflow run cache from disk."
  (when (file-exists-p shipit--workflow-run-cache-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents shipit--workflow-run-cache-file)
          (let ((data (read (current-buffer))))
            (when (hash-table-p data)
              (setq shipit--workflow-run-cache data)
              (shipit--debug-log "CACHE: Loaded %d workflow run names from disk"
                                 (hash-table-count data)))))
      (error
       (shipit--debug-log "CACHE: Failed to load workflow run cache: %s"
                          (error-message-string err))))))

(defun shipit--save-workflow-run-cache ()
  "Save workflow run cache to disk."
  (when (> (hash-table-count shipit--workflow-run-cache) 0)
    (condition-case err
        (with-temp-file shipit--workflow-run-cache-file
          (let ((print-level nil)
                (print-length nil))
            (prin1 shipit--workflow-run-cache (current-buffer))))
      (error
       (shipit--debug-log "CACHE: Failed to save workflow run cache: %s"
                          (error-message-string err))))))

;; Load cache on startup
(shipit--load-workflow-run-cache)

;; Save cache periodically and on exit
(add-hook 'kill-emacs-hook #'shipit--save-workflow-run-cache)

(defun shipit--fetch-missing-workflow-names (check-runs)
  "Asynchronously fetch missing workflow names for GitHub Actions checks."
  (let ((missing-run-ids '()))
    ;; Collect run IDs that need workflow names fetched
    (dolist (check check-runs)
      (let* ((raw-workflow-name (cdr (assq 'workflow_name check)))
             (html-url (cdr (assq 'html_url check))))
        (when (and (or (null raw-workflow-name)
                       (string= raw-workflow-name "GitHub Actions"))
                   html-url
                   (string-match "/actions/runs/\\([0-9]+\\)" html-url))
          (let ((run-id (match-string 1 html-url)))
            ;; Only fetch if not already cached
            (unless (gethash run-id shipit--workflow-run-cache)
              (push run-id missing-run-ids))))))

    ;; Fetch missing workflow names asynchronously
    (when missing-run-ids
      (shipit--debug-log "Fetching workflow names for %d missing run IDs" (length missing-run-ids))
      (dolist (run-id missing-run-ids)
        (shipit--fetch-workflow-name-for-run-id-async run-id)))))

(defun shipit--get-workflow-name-from-run-id (run-id)
  "Get workflow name from run ID from cache, or return default if not cached.
For async fetching, use shipit--fetch-workflow-name-for-run-id directly."
  (let ((cached-name (gethash run-id shipit--workflow-run-cache)))
    cached-name))  ; Return nil if not cached, let caller handle fallback

(defun shipit--fetch-workflow-name-for-run-id-async (run-id)
  "Fetch workflow name for run ID asynchronously and refresh display when complete."
  (let* ((repo (or shipit-current-repo
                   (shipit-auto-detect-repository))))
    (when repo
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (action-run-fn (plist-get backend :fetch-action-run-info-async)))
        (when action-run-fn
          (funcall action-run-fn config run-id
                   (lambda (data)
                     (let ((workflow-name (if data
                                              (let* ((name (cdr (assq 'name data)))
                                                     (path (cdr (assq 'path data))))
                                                (or name
                                                    (when path
                                                      (shipit--workflow-path-to-name path))
                                                    "GitHub Actions"))
                                            "GitHub Actions")))
                       (puthash run-id workflow-name shipit--workflow-run-cache)
                       (shipit--debug-log "Cached workflow name for run %s: %s" run-id workflow-name)
                       (when shipit--checks-fetched
                         (shipit--debug-log "Refreshing display after workflow name fetch"))))))))))

(defun shipit--fetch-workflow-name-for-run-id-sync (run-id)
  "Fetch workflow name for run ID via backend and cache it."
  (let* ((repo (or shipit-current-repo
                   (shipit-auto-detect-repository))))
    (if (not repo)
        "GitHub Actions"
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (data (funcall (plist-get backend :fetch-action-run-info) config run-id)))
        (if data
            (let* ((workflow-name (cdr (assq 'name data)))
                   (workflow-path (cdr (assq 'path data)))
                   (display-name (or workflow-name
                                     (when workflow-path
                                       (shipit--workflow-path-to-name workflow-path))
                                     "GitHub Actions")))
              (puthash run-id display-name shipit--workflow-run-cache)
              display-name)
          (puthash run-id "GitHub Actions" shipit--workflow-run-cache)
          "GitHub Actions")))))

(defun shipit--workflow-path-to-name (workflow-path)
  "Convert workflow file path to a readable name."
  (when workflow-path
    (let* ((filename (file-name-sans-extension (file-name-nondirectory workflow-path)))
           (readable-name (replace-regexp-in-string "[-_]" " " filename)))
      ;; Capitalize each word
      (mapconcat (lambda (word)
                   (concat (upcase (substring word 0 1))
                           (substring word 1)))
                 (split-string readable-name " ") " "))))

(defun shipit--extract-workflow-from-check-name (check-name)
  "Extract workflow name from check name using common patterns."
  (cond
   ;; Pattern: "Workflow Name / Job Name" - extract the part before "/"
   ((string-match "^\\([^/]+\\)\\s-*/\\s-*" check-name)
    (string-trim (match-string 1 check-name)))
   ;; Pattern: Matrix builds with brackets like "Name [matrix][options]"
   ((string-match "^\\([^[]+\\)\\s-*\\[" check-name)
    (string-trim (match-string 1 check-name)))
   ;; Pattern: "Workflow Name (job details)" - extract the part before "("
   ((string-match "^\\([^(]+\\)\\s-*(" check-name)
    (string-trim (match-string 1 check-name)))
   ;; Fallback: return the check name as-is for async workflow name fetching to improve
   (t check-name)))



;;; Hook handlers for shipit-buffer dispatch

(defun shipit--check-section-expand-handler (section)
  "Expand handler for check-item sections.
Returns non-nil if SECTION was handled."
  (when (and (eq (oref section type) 'check-item)
             (null (oref section children)))
    (shipit--expand-check-item section)
    t))

(defun shipit--check-section-url-handler (section)
  "URL handler for check/workflow/step sections.
Returns a URL string for SECTION, or nil."
  (shipit--check-url-at-point-for-section section))

(defun shipit--check-section-log-timestamps-handler (section)
  "Timestamp handler for check-item sections.
Shows the timestamps transient menu when inside a check-item.
Returns non-nil if SECTION was handled."
  (let ((s section))
    ;; Walk up to find if we're inside a check-item
    (while (and s (not (memq (oref s type)
                             '(check-item actions-step actions-log-group))))
      (setq s (oref s parent)))
    (when s
      (shipit-actions-timestamps-menu)
      t)))

(defun shipit--check-url-at-point-for-section (section)
  "Return the GitHub URL for SECTION, or nil."
  (let ((repo (bound-and-true-p shipit-buffer-repo)))
    (when (and section repo)
      (pcase (oref section type)
        ('check-item
         (cdr (assq 'html_url (oref section value))))
        ('actions-step
         (shipit-actions--step-url section))
        ((or 'workflow 'workflow-run)
         (let* ((value (oref section value))
                (run-id (when (consp value)
                          (cdr (assq 'run-id value)))))
           (unless run-id
             (setq run-id (shipit--find-run-id-in-children section)))
           (when run-id
             (format "https://github.com/%s/actions/runs/%s"
                     repo run-id))))))))

;; Register hooks
(when (featurep 'magit-section)
  (add-hook 'magit-section-visibility-hook #'shipit--auto-load-checks-on-expand))
(add-hook 'shipit-buffer-section-expand-functions #'shipit--check-section-expand-handler)
(add-hook 'shipit-buffer-section-url-functions #'shipit--check-section-url-handler)
(add-hook 'shipit-buffer-section-log-timestamps-functions #'shipit--check-section-log-timestamps-handler)

(provide 'shipit-checks)
;;; shipit-checks.el ends here
