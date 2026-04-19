;;; shipit-actions.el --- GitHub Actions run viewer -*- lexical-binding: t; -*-

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
;; Displays GitHub Actions workflow runs as nested magit sections:
;; Run header → Jobs → Steps → Log lines.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ansi-color)
(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-render)
(require 'transient)

;; Forward declarations
(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-section-show "magit-section")
(declare-function magit-section-hide "magit-section")
(declare-function magit-current-section "magit-section")
(declare-function magit-show-commit "magit-diff")
(declare-function shipit-register-dwim-handler "shipit-pr-actions")
(declare-function shipit-dwim "shipit-pr-actions")
(declare-function shipit-open-actions-list "shipit-actions-list")
(declare-function shipit-buffer--copy-visible-region "shipit-buffer")
(declare-function shipit-actions-list--insert-summary-content "shipit-actions-list")
(declare-function shipit-actions-list--fetch-summaries-for-jobs "shipit-actions-list")
(declare-function shipit-actions-list--render-summary-in-section "shipit-actions-list")
(declare-function shipit-actions-list--render-steps-in-section "shipit-actions-list")
(declare-function shipit--render-check-item-steps "shipit-checks")

(eval-when-compile
  (when (locate-library "magit")
    (require 'magit)
    (require 'magit-section)))

;; Magit section types
(defun actions-root (&rest _args)
  "Magit section identifier for actions root.")
(put 'actions-root 'magit-section t)

(defun actions-header (&rest _args)
  "Magit section identifier for actions header.")
(put 'actions-header 'magit-section t)

(defun actions-job (&rest _args)
  "Magit section identifier for actions job.")
(put 'actions-job 'magit-section t)

(defun actions-step (&rest _args)
  "Magit section identifier for actions step.")
(put 'actions-step 'magit-section t)

(defun actions-step-log (&rest _args)
  "Magit section identifier for actions step log.")
(put 'actions-step-log 'magit-section t)

(defun actions-log-group (&rest _args)
  "Magit section identifier for collapsible log sub-group.")
(put 'actions-log-group 'magit-section t)

(defun actions-run-summary (&rest _args)
  "Magit section identifier for run summary.")
(put 'actions-run-summary 'magit-section t)

;;; Buffer-local state

(defvar-local shipit-actions--repo nil
  "Repository owner/name for this actions buffer.")

(defvar-local shipit-actions--run-id nil
  "Workflow run ID for this actions buffer.")

(defvar-local shipit-actions--job-id nil
  "Job name to auto-expand after loading, or nil.")

(defvar-local shipit-actions--run-data nil
  "Cached run metadata alist.")

(defvar-local shipit-actions--jobs-data nil
  "Cached jobs list for the run.")

(defvar-local shipit-actions--job-logs (make-hash-table :test 'eql)
  "Hash table mapping job-id to alist of (step-number . log-items).
Each job's logs are fetched lazily on first expand.")

(defvar-local shipit-actions--job-logs-state (make-hash-table :test 'eql)
  "Hash table mapping job-id to fetch state: nil, `pending', t, or `error'.")

(defvar-local shipit-actions--show-timestamps nil
  "Whether to show timestamps in log output.")

(defvar-local shipit-actions--show-gap-coloring nil
  "Whether to color-code timestamps by gap from previous line.")

(defvar-local shipit-actions--step-time-format 'relative
  "Format for step start times: `relative' (3m ago) or `absolute' (05:23:01).")

(defvar-local shipit-actions--run-summaries (make-hash-table :test 'eql)
  "Hash table mapping run-id to list of (job-name . markdown) pairs.")

;;; Keymap

(defvar shipit-actions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'shipit-actions-dwim)
    (define-key map (kbd "q") #'shipit-actions-quit)
    (define-key map (kbd "g") #'shipit-actions-refresh)
    (define-key map (kbd "b") #'shipit-actions-browse)
    (define-key map (kbd "L") #'shipit-actions-timestamps-menu)
    (define-key map (kbd "S") #'shipit-actions-save-raw-log)
    (define-key map (kbd "TAB") #'shipit-actions-toggle-section)
    (define-key map (kbd "W") #'shipit-actions-list-from-run)
    (define-key map (kbd "M-w") #'shipit-actions-copy-url)
    (define-key map (kbd "M-;") #'shipit-dwim)
    map)
  "Keymap for `shipit-actions-mode'.")

;;; Major mode

(define-derived-mode shipit-actions-mode magit-section-mode "Shipit-Actions"
  "Major mode for viewing GitHub Actions workflow runs.

\\{shipit-actions-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (shipit-actions-refresh)))
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local magit-section-preserve-visibility nil)
  (shipit--apply-section-defaults))

;;; Entry point

;;;###autoload
(defun shipit-open-actions-run (repo run-id &optional job-id)
  "Open a shipit buffer for Actions run RUN-ID in REPO.
When JOB-ID is non-nil, auto-expand that job after loading."
  (let* ((buf-name (format "*shipit-actions-run: %s #%s*" repo run-id))
         (existing (get-buffer buf-name)))
    (if existing
        (progn
          (switch-to-buffer existing)
          (when job-id
            (setq shipit-actions--job-id job-id)
            (shipit-actions--auto-expand-job)))
      (let ((buffer (generate-new-buffer buf-name)))
        (with-current-buffer buffer
          (shipit-actions-mode)
          (setq shipit-actions--repo repo
                shipit-actions--run-id run-id
                shipit-actions--job-id job-id)
          (shipit-actions--fetch-and-render))
        (switch-to-buffer buffer)))))

;;; Data fetching

(defun shipit-actions--fetch-and-render ()
  "Fetch run data and jobs, then render the buffer."
  (let ((buffer (current-buffer))
        (repo shipit-actions--repo)
        (run-id shipit-actions--run-id))
    (message "Loading Actions run #%s..." run-id)
    (shipit-actions--fetch-run
     repo run-id
     (lambda (run-data)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq shipit-actions--run-data run-data)
           (shipit-actions--fetch-jobs
            repo run-id
            (lambda (jobs-data)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (setq shipit-actions--jobs-data jobs-data)
                  (shipit-actions--render)
                  (shipit-actions--auto-expand-job)
                  (message "Actions run #%s loaded." run-id)))))))))))

(defun shipit-actions--auto-expand-job ()
  "If `shipit-actions--job-id' is set, expand the matching job section."
  (when shipit-actions--job-id
    (let ((job-name shipit-actions--job-id))
      (setq shipit-actions--job-id nil)
      (when (bound-and-true-p magit-root-section)
        (shipit-actions--find-and-expand-job magit-root-section job-name)))))

(defun shipit-actions--find-and-expand-job (root job-name)
  "Walk ROOT section tree to find and expand the job named JOB-NAME."
  (let ((found nil))
    (dolist (child (oref root children))
      (unless found
        (dolist (grandchild (oref child children))
          (unless found
            (when (and (eq (oref grandchild type) 'actions-job)
                       (let ((job (oref grandchild value)))
                         (string= (cdr (assq 'name job)) job-name)))
              (goto-char (oref grandchild start))
              (magit-section-show grandchild)
              (shipit-actions--maybe-fetch-job-logs)
              (setq found t))))))))

(defun shipit-actions--fetch-run (repo run-id callback)
  "Fetch workflow run data for RUN-ID in REPO.
Calls CALLBACK with the parsed alist."
  (let ((endpoint (format "/repos/%s/actions/runs/%s" repo run-id)))
    (shipit--api-request endpoint nil callback)))

(defun shipit-actions--fetch-jobs (repo run-id callback)
  "Fetch all jobs for workflow run RUN-ID in REPO with pagination.
Calls CALLBACK with the complete jobs list."
  (shipit-actions--fetch-jobs-page repo run-id 1 nil callback))

(defun shipit-actions--fetch-jobs-page (repo run-id page accumulated callback)
  "Fetch PAGE of jobs for RUN-ID in REPO, accumulating into ACCUMULATED.
Calls CALLBACK with the complete jobs list when all pages are fetched."
  (let ((endpoint (format "/repos/%s/actions/runs/%s/jobs" repo run-id)))
    (shipit--api-request
     endpoint `(("per_page" 100) ("page" ,page))
     (lambda (data)
       (let* ((jobs (append (cdr (assq 'jobs data)) nil))
              (total (cdr (assq 'total_count data)))
              (all-jobs (append accumulated jobs)))
         (if (and total (< (length all-jobs) total))
             (shipit-actions--fetch-jobs-page
              repo run-id (1+ page) all-jobs callback)
           (funcall callback all-jobs)))))))

(defun shipit-actions--fetch-run-logs-zip (callback)
  "Fetch run-level logs ZIP for the current run.
Downloads to a temp file, calls CALLBACK with the temp file path."
  (let* ((repo shipit-actions--repo)
         (run-id shipit-actions--run-id)
         (endpoint (format "/repos/%s/actions/runs/%s/logs" repo run-id))
         (url (concat (or shipit-api-url "https://api.github.com") endpoint))
         (temp-file (make-temp-file "shipit-actions-log-" nil ".zip"))
         (url-request-method "GET")
         (url-request-extra-headers
          (remove nil (list (shipit--get-auth-header)
                            '("Accept" . "application/vnd.github.v3+json")))))
    (shipit--debug-log "Fetching run logs ZIP: %s" url)
    (setq system-time-locale "C")
    (url-retrieve
     url
     (lambda (status)
       (condition-case err
           (let ((error-status (plist-get status :error)))
             (if error-status
                 (progn
                   (shipit--debug-log "Log ZIP fetch error: %S" error-status)
                   (message "Failed to fetch logs: %S" error-status)
                   (funcall callback nil))
               (goto-char (point-min))
               (set-buffer-multibyte nil)
               (goto-char (point-min))
               (search-forward "\n\n" nil t)
               (let ((coding-system-for-write 'no-conversion))
                 (write-region (point) (point-max) temp-file nil 'silent))
               (shipit--debug-log "Log ZIP saved to %s (%d bytes)"
                                  temp-file (- (point-max) (point)))
               (funcall callback temp-file)))
         (error
          (shipit--debug-log "Log ZIP callback error: %S" err)
          (message "Log fetch error: %s" (error-message-string err))))
       (when (buffer-live-p (current-buffer))
         (kill-buffer (current-buffer)))))))

(defun shipit-actions--extract-job-log-from-zip (zip-file job-name)
  "Extract the log file for JOB-NAME from ZIP-FILE.
The run-level ZIP has flat files like `N_Job Name.txt' per job.
Returns the raw log text for the matching job, or nil."
  (let* ((file-list (shell-command-to-string
                     (format "unzip -l %s 2>/dev/null"
                             (shell-quote-argument zip-file))))
         (entry-path nil))
    (dolist (line (split-string file-list "\n"))
      (when (string-match
             (format "\\([0-9]+_%s\\.txt\\)"
                     (regexp-quote job-name))
             line)
        (setq entry-path (match-string 1 line))))
    (shipit--debug-log "ZIP entry for '%s': %S" job-name entry-path)
    (when entry-path
      (shell-command-to-string
       (format "unzip -p %s %s 2>/dev/null"
               (shell-quote-argument zip-file)
               (shell-quote-argument entry-path))))))

(defun shipit-actions--fetch-job-logs-raw (job-id callback)
  "Fetch raw logs for JOB-ID.
Calls CALLBACK with the raw log text string (for saving to file)."
  (let* ((repo shipit-actions--repo)
         (endpoint (format "/repos/%s/actions/jobs/%s/logs" repo job-id))
         (url (concat (or shipit-api-url "https://api.github.com") endpoint))
         (url-request-method "GET")
         (url-request-extra-headers
          (remove nil (list (shipit--get-auth-header)
                            '("Accept" . "application/vnd.github.v3+json")))))
    (shipit--debug-log "Fetching job logs: %s" url)
    (setq system-time-locale "C")
    (url-retrieve
     url
     (lambda (status)
       (shipit--debug-log "Job logs status: %S" status)
       (condition-case err
           (let ((error-status (plist-get status :error)))
             (if error-status
                 (progn
                   (shipit--debug-log "Job logs error: %S" error-status)
                   (funcall callback nil))
               (goto-char (point-min))
               (let* ((http-status (when (re-search-forward
                                          "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                                     (string-to-number (match-string 1))))
                      (headers-end (progn (goto-char (point-min))
                                          (search-forward "\n\n" nil t)))
                      (body (when (and headers-end
                                       (< (point) (point-max)))
                              (set-buffer-multibyte t)
                              (decode-coding-region
                               (point) (point-max) 'utf-8)
                              (buffer-substring-no-properties
                               (point) (point-max)))))
                 (shipit--debug-log "Job logs HTTP %s, body length: %s"
                                    http-status (length (or body "")))
                 (if (and body (> (length body) 0))
                     (funcall callback body)
                   (funcall callback nil)))))
         (error
          (shipit--debug-log "Job logs callback error: %S" err)
          (funcall callback nil)))
       (when (buffer-live-p (current-buffer))
         (kill-buffer (current-buffer)))))))

;;; Rendering

(defun shipit-actions--render (&optional saved-visibility)
  "Render the entire actions buffer.
If SAVED-VISIBILITY is non-nil, restore section states from it."
  (let ((inhibit-read-only t)
        (run shipit-actions--run-data)
        (jobs shipit-actions--jobs-data))
    (erase-buffer)
    (magit-insert-section (actions-root)
      (shipit-actions--insert-run-header run)
      (insert "\n")
      (shipit-actions--insert-jobs-section jobs)
      (insert "\n")
      (shipit-actions--insert-summary-section run))
    (when saved-visibility
      (shipit-actions--restore-section-visibility saved-visibility))
    (unless saved-visibility
      (goto-char (point-min)))))

(defun shipit-actions--insert-run-header (run)
  "Insert the run header section from RUN data."
  (let* ((name (or (cdr (assq 'name run)) "Unknown workflow"))
         (status (or (cdr (assq 'status run)) "unknown"))
         (conclusion (cdr (assq 'conclusion run)))
         (event (or (cdr (assq 'event run)) "unknown"))
         (run-number (cdr (assq 'run_number run)))
         (head-branch (cdr (assq 'head_branch run)))
         (head-sha (cdr (assq 'head_sha run)))
         (head-commit (cdr (assq 'head_commit run)))
         (commit-msg (when head-commit
                       (cdr (assq 'message head-commit))))
         (created (cdr (assq 'created_at run)))
         (updated (cdr (assq 'updated_at run)))
         (duration (shipit-actions--format-duration created updated))
         (display-status (or conclusion status)))
    (magit-insert-section (actions-header)
      (insert (propertize (format "%s %s"
                                  (shipit--get-pr-field-icon "checks" "🔧")
                                  name)
                          'font-lock-face 'magit-section-heading)
              "\n")
      (insert (format "%s Status:  %s %s"
                      (shipit-actions--status-icon display-status)
                      (propertize display-status
                                  'font-lock-face (shipit-actions--status-face display-status))
                      (if duration
                          (format "  %s %s"
                                  (shipit--get-pr-field-icon "clock" "⏱")
                                  (propertize duration 'font-lock-face 'shipit-timestamp-face))
                        ""))
              "\n")
      (insert (format "%s Trigger: %s"
                      (shipit--get-pr-field-icon "activity" "⚡")
                      event)
              (if head-branch
                  (format "    %s %s"
                          (shipit--get-pr-field-icon "branch" "🌿")
                          (propertize head-branch 'font-lock-face 'shipit-filename-face))
                "")
              "\n")
      (when run-number
        (insert (format "%s Run #%s"
                        (shipit--get-pr-field-icon "metadata" "ℹ")
                        run-number)
                "\n"))
      (when (and head-sha commit-msg)
        (let ((line (format "%s %s — %s"
                            (shipit--get-pr-field-icon "repo" "📦")
                            (propertize (substring head-sha 0 (min 7 (length head-sha)))
                                        'font-lock-face 'shipit-filename-face)
                            (car (split-string commit-msg "\n")))))
          (insert (propertize line 'shipit-commit-sha head-sha) "\n"))))))

(defun shipit-actions--job-label-width (job)
  "Return the display width of JOB's label (name + step count)."
  (let* ((name (cdr (assq 'name job)))
         (steps (cdr (assq 'steps job)))
         (step-count (length steps)))
    (+ (length name)
       (if (> step-count 0)
           (length (format " (%d steps)" step-count))
         0))))

(defun shipit-actions--step-label-width (step)
  "Return the display width of STEP's label (just the name)."
  (length (cdr (assq 'name step))))

(defun shipit-actions--insert-jobs-section (jobs)
  "Insert jobs section with nested steps for each job."
  (let* ((max-secs (shipit-actions--max-duration jobs))
         ;; Compute unified bar column across jobs AND steps so bars align.
         ;; Job prefix: "  " + icon + " " = 4 chars
         ;; Step prefix: "    " + icon + " " = 6 chars
         (max-job-content (+ 4 (cl-reduce #'max jobs
                                           :key #'shipit-actions--job-label-width
                                           :initial-value 0)))
         (max-step-content
          (let ((max-w 0))
            (dolist (job jobs)
              (dolist (step (cdr (assq 'steps job)))
                (setq max-w (max max-w (+ 6 (shipit-actions--step-label-width step))))))
            max-w))
         (bar-col (max max-job-content max-step-content)))
    (magit-insert-section (actions-header nil nil)
      (magit-insert-heading
        (propertize "Jobs" 'font-lock-face 'magit-section-heading))
      (dolist (job jobs)
        (shipit-actions--insert-job-section job max-secs bar-col)))))

(defun shipit-actions--insert-summary-section (run)
  "Insert a collapsible Summary section for RUN."
  (let* ((run-id (cdr (assq 'id run)))
         (cached (gethash run-id shipit-actions--run-summaries))
         (sentinel (make-symbol "missing"))
         (has-cache (not (eq (gethash run-id shipit-actions--run-summaries sentinel)
                             sentinel)))
         (icon (shipit--get-pr-field-icon "comment" "\U0001f4cb"))
         (sect (magit-insert-section (actions-run-summary run)
                 (magit-insert-heading
                   (concat icon " "
                           (propertize "Summary" 'font-lock-face 'magit-section-heading)
                           "\n"))
                 (when has-cache
                   (shipit-actions-list--insert-summary-content cached "  ")))))
    (magit-section-hide sect)))

(defun shipit-actions--expand-summary-section (section)
  "Expand summary SECTION in the actions detail buffer."
  (let* ((run (oref section value))
         (run-id (cdr (assq 'id run)))
         (sentinel (make-symbol "missing"))
         (has-cache (not (eq (gethash run-id shipit-actions--run-summaries sentinel)
                             sentinel))))
    (if has-cache
        (magit-section-toggle section)
      (let ((buf (current-buffer))
            (sect section))
        (message "Fetching summaries...")
        (shipit-actions-list--fetch-summaries-for-jobs
         shipit-actions--repo
         shipit-actions--jobs-data
         (lambda (summaries)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (puthash run-id summaries shipit-actions--run-summaries)
               (shipit-actions-list--render-summary-in-section
                sect summaries "  ")))))))))

(defun shipit-actions--max-duration (items)
  "Return the maximum duration in seconds across ITEMS (jobs or steps)."
  (let ((max-secs 0))
    (dolist (item items)
      (let ((secs (shipit-actions--duration-seconds
                   (cdr (assq 'started_at item))
                   (cdr (assq 'completed_at item)))))
        (when (and secs (> secs max-secs))
          (setq max-secs secs))))
    (when (> max-secs 0) max-secs)))

(defun shipit-actions--insert-job-section (job max-secs bar-col)
  "Insert a JOB as a collapsible section containing its steps.
MAX-SECS is the longest job duration, BAR-COL is the unified column for bars."
  (let* ((name (cdr (assq 'name job)))
         (job-id (cdr (assq 'id job)))
         (status (cdr (assq 'status job)))
         (conclusion (cdr (assq 'conclusion job)))
         (display-status (or conclusion status))
         (started (cdr (assq 'started_at job)))
         (completed (cdr (assq 'completed_at job)))
         (duration (shipit-actions--format-duration started completed))
         (secs (shipit-actions--duration-seconds started completed))
         (bar (shipit-actions--duration-bar secs max-secs display-status))
         (steps (cdr (assq 'steps job)))
         (step-count (length steps))
         (label-width (shipit-actions--job-label-width job))
         (padding (make-string (max 0 (- bar-col 4 label-width)) ?\s))
         (log-state (gethash job-id shipit-actions--job-logs-state))
         (sect (magit-insert-section (actions-job job)
                 (magit-insert-heading
                   (format "  %s %s%s%s  %s  %s"
                           (shipit-actions--status-icon display-status)
                           (propertize name 'font-lock-face 'magit-section-secondary-heading)
                           (if (> step-count 0)
                               (propertize (format " (%d steps)" step-count)
                                           'font-lock-face 'shadow)
                             "")
                           padding
                           (or bar "")
                           (if duration
                               (propertize duration 'font-lock-face 'shipit-timestamp-face)
                             "")))
                 (cond
                 ((eq log-state t)
                  (shipit-actions--insert-steps job bar-col))
                 ((shipit-actions--job-in-progress-p job)
                  (insert "    (job still running — logs available after completion)\n"))
                 ((eq log-state 'error)
                  (insert "    (failed to fetch logs)\n"))
                 (t
                  (insert "    Loading steps...\n"))))))
    (magit-section-hide sect)))

(defun shipit-actions--insert-steps (job bar-col &optional indent)
  "Insert step sections for JOB using cached log data.
BAR-COL is the unified column for bars.
INDENT is the prefix string (default \"    \")."
  (let* ((steps (cdr (assq 'steps job)))
         (job-id (cdr (assq 'id job)))
         (step-logs (gethash job-id shipit-actions--job-logs))
         (sorted (sort (copy-sequence steps)
                       (lambda (a b)
                         (< (cdr (assq 'number a))
                            (cdr (assq 'number b))))))
         (max-secs (shipit-actions--max-duration sorted)))
    (dolist (step sorted)
      (shipit-actions--insert-step step step-logs max-secs bar-col indent))))

(defun shipit-actions--insert-step (step step-logs max-secs bar-col &optional indent)
  "Insert a single STEP section with log content from STEP-LOGS.
MAX-SECS is the longest step duration, BAR-COL is the unified column for bars.
INDENT is the prefix string (default \"    \")."
  (let* ((prefix (or indent "    "))
         (prefix-width (+ (length prefix) 2))
         (number (cdr (assq 'number step)))
         (name (cdr (assq 'name step)))
         (conclusion (or (cdr (assq 'conclusion step))
                         (cdr (assq 'status step))))
         (started (cdr (assq 'started_at step)))
         (completed (cdr (assq 'completed_at step)))
         (duration (shipit-actions--format-duration started completed))
         (secs (shipit-actions--duration-seconds started completed))
         (bar (shipit-actions--duration-bar secs max-secs conclusion))
         (padding (make-string (max 0 (- bar-col prefix-width (length name))) ?\s))
         (log-items (cdr (assq number step-logs)))
         (start-time (shipit-actions--format-step-time started))
         (fixed-part (format "%s%s %s%s  %s  %s"
                             prefix
                             (shipit-actions--status-icon conclusion)
                             name
                             padding
                             (or bar "")
                             (if duration
                                 (propertize duration 'font-lock-face 'shipit-timestamp-face)
                               "")))
         (time-str (if start-time
                       (propertize start-time 'font-lock-face 'shipit-timestamp-face)
                     ""))
         (win-width (or (when-let* ((win (get-buffer-window (current-buffer))))
                          (window-body-width win))
                        80))
         (gap (max 1 (- win-width (length fixed-part) (length time-str) 3)))
         (sect (magit-insert-section (actions-step step)
                 (magit-insert-heading
                   (concat fixed-part
                           (make-string gap ?\s)
                           time-str))
                 (let ((log-indent (concat prefix "    ")))
                   (if log-items
                       (shipit-actions--insert-step-log-content log-items log-indent)
                     (insert log-indent "(no log output)\n"))))))
    (magit-section-hide sect)))

;;; Lazy log fetching

(defun shipit-actions--job-in-progress-p (job)
  "Return non-nil if JOB has not completed yet."
  (let ((status (cdr (assq 'status job))))
    (member status '("in_progress" "queued" "waiting" "requested" "pending"))))

(defun shipit-actions--ensure-job-logs (job)
  "Ensure logs are fetched for JOB, then re-render the buffer."
  (let* ((job-id (cdr (assq 'id job)))
         (state (gethash job-id shipit-actions--job-logs-state)))
    (when (shipit-actions--job-in-progress-p job)
      (message "Job still running — logs available after completion"))
    (unless (memq state '(pending t))
      (puthash job-id 'pending shipit-actions--job-logs-state)
      (let ((buffer (current-buffer)))
        (shipit-actions--fetch-job-logs-raw
         job-id
         (lambda (log-text)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (if log-text
                   (progn
                     (puthash job-id
                              (shipit-actions--split-log-by-markers log-text job)
                              shipit-actions--job-logs)
                     (puthash job-id t shipit-actions--job-logs-state)
                     (shipit--debug-log "Fetched logs for job %s" job-id))
                 (puthash job-id 'error shipit-actions--job-logs-state)
                 (message "Failed to fetch job logs"))
               (let ((visibility (shipit-actions--save-section-visibility))
                     (line (line-number-at-pos))
                     (col (current-column)))
                 ;; Force the job we just fetched to be visible
                 (push (cons (cons 'job (cdr (assq 'name job))) nil) visibility)
                 (shipit-actions--render visibility)
                 (goto-char (point-min))
                 (forward-line (1- line))
                 (move-to-column col))))))))))

;;; Log parsing

(defun shipit-actions--split-timestamp (line)
  "Split LINE into (TIMESTAMP . TEXT).
Timestamps look like: 2026-03-08T12:44:20.9855756Z
Returns (TIMESTAMP . TEXT) or (nil . LINE) if no timestamp.
Strips leading BOM (U+FEFF) which GitHub Actions logs sometimes include."
  (let ((clean (replace-regexp-in-string "\\`\uFEFF" "" line)))
    (if (string-match
         "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9:.]+Z\\) "
         clean)
        (cons (match-string 1 clean)
              (substring clean (match-end 0)))
      (cons nil clean))))

(defun shipit-actions--normalize-api-timestamp (ts)
  "Normalize API timestamp TS so string comparison works with log timestamps.
API timestamps like \"2026-03-08T12:44:25Z\" lack fractional seconds,
while log timestamps have them: \"2026-03-08T12:44:25.3528669Z\".
Naive string comparison gives wrong results because ASCII \\='.\\=' < \\='Z\\=',
so \"12:44:25.3Z\" < \"12:44:25Z\".  Fix by padding API timestamps
to \"2026-03-08T12:44:25.0000000Z\"."
  (if (and ts (string-match "\\([0-9]\\)Z\\'" ts))
      (concat (substring ts 0 (match-end 1)) ".0000000Z")
    ts))

(defun shipit-actions--split-log-by-markers (log-text job)
  "Split LOG-TEXT into per-step sections using structural markers.
JOB is the job alist containing step metadata.
Returns an alist of (STEP-NUMBER . LOG-ITEMS)."
  (let* ((steps (cdr (assq 'steps job)))
         (sorted (sort (copy-sequence steps)
                       (lambda (a b)
                         (< (cdr (assq 'number a))
                            (cdr (assq 'number b))))))
         (setup-step nil)
         (user-steps nil)
         (post-steps nil)
         (buckets (make-hash-table :test 'eql))
         (current-step nil)
         (user-idx 0)
         (lines (split-string (or log-text "") "\n")))
    ;; Classify API steps into setup / user / post / complete
    (dolist (step sorted)
      (let ((name (cdr (assq 'name step)))
            (number (cdr (assq 'number step)))
            (started (cdr (assq 'started_at step)))
            (conclusion (cdr (assq 'conclusion step))))
        (when (and started (not (equal started :null)))
          (cond
           ((string-match-p "\\`Set up " name)
            (setq setup-step number))
           ((string-match-p "\\`\\(Post \\|Complete \\)" name)
            (push number post-steps))
           ((equal conclusion "skipped") nil)
           (t
            (push number user-steps))))))
    (setq user-steps (nreverse user-steps))
    (setq post-steps (nreverse post-steps))
    (setq current-step (or setup-step 0))
    ;; Single pass: detect step transitions via markers
    (dolist (line lines)
      (let* ((parts (shipit-actions--split-timestamp line))
             (text (cdr parts)))
        (cond
         ((string-match-p "\\`##\\[group\\]Run " text)
          (when (< user-idx (length user-steps))
            (setq current-step (nth user-idx user-steps))
            (cl-incf user-idx)))
         ((string-match-p "\\`Post job cleanup\\." text)
          (when post-steps
            (setq current-step (car post-steps))))))
      (push line (gethash current-step buckets nil)))
    ;; Build result alist
    (cl-remove-if
     #'null
     (mapcar (lambda (step)
               (let* ((num (cdr (assq 'number step)))
                      (raw-lines (nreverse (gethash num buckets nil))))
                 (when raw-lines
                   (cons num
                         (shipit-actions--parse-step-log-text
                          (string-join raw-lines "\n"))))))
             sorted))))

(defun shipit-actions--parse-step-log-text (log-text)
  "Parse a single step's LOG-TEXT into structured items.
Returns a list of items where each item is either (TIMESTAMP . TEXT)
or (:group NAME . CHILDREN) for collapsible sub-groups."
  (let ((current-lines nil)
        (stack nil))
    (dolist (line (split-string (or log-text "") "\n"))
      (let* ((parts (shipit-actions--split-timestamp line))
             (text (cdr parts)))
        (cond
         ((string-match "^##\\[group\\]\\(.*\\)" text)
          (push (cons (string-trim (match-string 1 text)) current-lines)
                stack)
          (setq current-lines nil))
         ((string-match "^##\\[endgroup\\]" text)
          (when stack
            (let* ((frame (pop stack))
                   (group-name (car frame))
                   (parent-lines (cdr frame))
                   (group-item (cons :group
                                     (cons group-name
                                           (nreverse current-lines)))))
              (setq current-lines parent-lines)
              (push group-item current-lines))))
         ((string-match "^##\\[\\(command\\|error\\|warning\\|debug\\|notice\\)\\]\\(.*\\)" text)
          (push (cons (car parts)
                      (format "[%s]%s" (match-string 1 text) (match-string 2 text)))
                current-lines))
         ((string-match "^##\\[" text) nil)
         (t (push parts current-lines)))))
    (nreverse current-lines)))

;;; Log rendering

(defun shipit-actions--format-time-only (ts)
  "Format ISO timestamp TS as HH:MM:SS for display, or nil."
  (when (and ts (not (eq ts :null))
             (string-match "T\\([0-9]+:[0-9]+:[0-9]+\\)" ts))
    (match-string 1 ts)))

(defun shipit-actions--format-step-time (ts)
  "Format step start timestamp TS based on `shipit-actions--step-time-format'."
  (when (and ts (not (eq ts :null)))
    (if (eq shipit-actions--step-time-format 'absolute)
        (shipit-actions--format-time-only ts)
      (shipit-actions--format-time-ago ts))))

(defun shipit-actions--format-time-ago (ts)
  "Format ISO timestamp TS as relative time like \"3m ago\"."
  (when (and ts (not (eq ts :null)))
    (let* ((then (float-time (date-to-time ts)))
           (now (float-time))
           (diff (- now then))
           (minutes (floor (/ diff 60)))
           (hours (floor (/ diff 3600)))
           (days (floor (/ diff 86400))))
      (cond
       ((< diff 60) "just now")
       ((< minutes 60) (format "%dm ago" minutes))
       ((< hours 24) (format "%dh ago" hours))
       ((< days 30) (format "%dd ago" days))
       (t (shipit-actions--format-time-only ts))))))

(defun shipit-actions--format-timestamp-short (ts)
  "Format timestamp TS as YYYY-MM-DD HH:MM:SS.mmm for display."
  (if (and ts (string-match
               "\\([0-9-]+\\)T\\([0-9:.]+\\)Z"
               ts))
      (format "%s %s" (match-string 1 ts) (match-string 2 ts))
    ""))

(defun shipit-actions--insert-step-log-content (log-items &optional indent)
  "Insert LOG-ITEMS (structured list from the parser).
Each item is either (TIMESTAMP . TEXT) or (:group NAME . CHILDREN).
Renders ANSI colors, optionally shows timestamps, and creates
collapsible magit sections for sub-groups.
INDENT is the prefix string (default \"        \")."
  (let ((log-indent (or indent "        "))
        (prev-ts nil))
    (dolist (item log-items)
      (if (and (consp item) (eq (car item) :group))
          ;; Sub-group — collapsible section
          (let* ((name (cadr item))
                 (children (cddr item))
                 (sect (magit-insert-section (actions-log-group name)
                         (magit-insert-heading
                           (format "%s%s %s"
                                   log-indent
                                   (shipit--get-pr-field-icon "metadata" "▶")
                                   (propertize name 'font-lock-face 'bold)))
                         (shipit-actions--insert-log-lines children log-indent))))
            ;; Track last timestamp from group children for next item
            (let ((last-child (car (last children))))
              (when (and last-child (car last-child))
                (setq prev-ts (car last-child))))
            (magit-section-hide sect))
        ;; Regular line
        (shipit-actions--insert-log-line item log-indent prev-ts)
        (when (car item)
          (setq prev-ts (car item)))))))

(defun shipit-actions--insert-log-lines (lines &optional indent)
  "Insert a list of LINES (timestamp . text) cons cells.
INDENT is the prefix string (default \"        \").
Tracks previous timestamp for gap coloring."
  (let ((prev-ts nil))
    (dolist (entry lines)
      (shipit-actions--insert-log-line entry indent prev-ts)
      (when (car entry)
        (setq prev-ts (car entry))))))

(defun shipit-actions--insert-log-line (entry &optional indent prev-ts)
  "Insert a single log ENTRY (TIMESTAMP . TEXT).
INDENT is the prefix string (default \"        \").
PREV-TS is the previous line's timestamp for gap coloring."
  (let ((ts (car entry))
        (text (ansi-color-apply (cdr entry))))
    (insert (or indent "        "))
    (when shipit-actions--show-timestamps
      (let ((face (if (and shipit-actions--show-gap-coloring prev-ts ts)
                      (let* ((t1 (shipit-actions--timestamp-to-seconds prev-ts))
                             (t2 (shipit-actions--timestamp-to-seconds ts))
                             (delta (if (and t1 t2) (- t2 t1) 0)))
                        (if (> delta 0)
                            (shipit-actions--gap-face delta)
                          'shipit-timestamp-face))
                    'shipit-timestamp-face)))
        (insert (propertize (shipit-actions--format-timestamp-short ts)
                            'font-lock-face face)
                " ")))
    (insert (shipit-actions--annotate-log-line text) "\n")))

(defun shipit-actions--annotate-log-line (text)
  "Apply syntax highlighting to GitHub Actions annotations in TEXT.
Highlights [command], [error], [warning], [debug], and [notice] prefixes."
  (cond
   ((string-match "^\\[command\\]\\(.*\\)" text)
    (propertize (match-string 1 text) 'font-lock-face '(:foreground "#0366d6")))
   ((string-match "^\\[error\\]\\(.*\\)" text)
    (propertize (concat "Error: " (match-string 1 text))
                'font-lock-face '(:foreground "#cb2431")))
   ((string-match "^\\[warning\\]\\(.*\\)" text)
    (propertize (concat "Warning: " (match-string 1 text))
                'font-lock-face '(:foreground "#fd7e14")))
   ((string-match "^\\[notice\\]\\(.*\\)" text)
    (propertize (concat "Notice: " (match-string 1 text))
                'font-lock-face '(:foreground "#fd7e14")))
   ((string-match "^\\[debug\\]\\(.*\\)" text)
    (propertize (match-string 1 text) 'font-lock-face 'shadow))
   (t text)))

;;; Commands

(defun shipit-actions-save-raw-log ()
  "Save raw log for the job at point to a file."
  (interactive)
  (let ((job (shipit-actions--job-at-point)))
    (unless job
      (user-error "No job at point"))
    (let* ((job-id (cdr (assq 'id job)))
           (job-name (cdr (assq 'name job)))
           (default-name (format "actions-log-%s-%s.txt"
                                 shipit-actions--run-id
                                 (replace-regexp-in-string
                                  "[^a-zA-Z0-9_-]" "_" job-name)))
           (output-file (read-file-name "Save log to: "
                                        nil nil nil default-name)))
      (message "Fetching raw log for %s..." job-name)
      (shipit-actions--fetch-job-logs-raw
       job-id
       (lambda (log-text)
         (if log-text
             (progn
               (with-temp-file output-file
                 (insert log-text))
               (message "Log saved to %s" output-file))
           (message "Failed to fetch raw log")))))))

(defun shipit-actions--job-at-point ()
  "Return the job alist for the section at point, or nil."
  (let ((section (magit-current-section)))
    (while (and section
                (not (eq (oref section type) 'actions-job)))
      (setq section (oref section parent)))
    (when (and section (eq (oref section type) 'actions-job))
      (oref section value))))

;;; Navigation

(defun shipit-actions--maybe-fetch-job-logs ()
  "If the section at point is an expanded job, trigger lazy log fetch."
  (let ((section (magit-current-section)))
    (when (and section
               (eq (oref section type) 'actions-job)
               (not (oref section hidden)))
      (let ((job (oref section value)))
        (when job
          (shipit-actions--ensure-job-logs job))))))

(defun shipit-actions-toggle-section ()
  "Toggle section at point.  Fetches job logs on first expand."
  (interactive)
  (let ((section (magit-current-section)))
    (if (eq (oref section type) 'actions-run-summary)
        (shipit-actions--expand-summary-section section)
      (magit-section-toggle section)
      (shipit-actions--maybe-fetch-job-logs))))

(defun shipit-actions-dwim ()
  "Context-sensitive action at point.
On a commit SHA: open magit-revision buffer.
On a job or step: toggle expansion (fetches logs on first expand)."
  (interactive)
  (let ((sha (get-text-property (point) 'shipit-commit-sha)))
    (if sha
        (magit-show-commit sha)
      (let ((section (magit-current-section)))
        (when section
          (pcase (oref section type)
            ((or 'actions-job 'actions-step 'actions-log-group)
             (magit-section-toggle section)
             (shipit-actions--maybe-fetch-job-logs))
            (_ nil)))))))

(defun shipit-actions-list-from-run ()
  "Open the workflows list for the current repo."
  (interactive)
  (shipit-open-actions-list shipit-actions--repo))

(defun shipit-actions-quit ()
  "Bury the actions buffer."
  (interactive)
  (quit-window))


(defun shipit-actions--job-bar-col (job)
  "Compute the bar column for JOB's steps."
  (let ((job-w (+ 4 (shipit-actions--job-label-width job)))
        (step-w 0))
    (dolist (step (cdr (assq 'steps job)))
      (setq step-w (max step-w (+ 6 (shipit-actions--step-label-width step)))))
    (max job-w step-w)))

(defun shipit-actions--save-step-visibility (job-section)
  "Save visibility of step children under JOB-SECTION."
  (let ((expanded nil))
    (dolist (child (oref job-section children))
      (when (and (eq (oref child type) 'actions-step)
                 (not (oref child hidden)))
        (push (cdr (assq 'number (oref child value))) expanded)))
    expanded))

(defun shipit-actions--restore-step-visibility (job-section expanded)
  "Restore step visibility under JOB-SECTION from EXPANDED list."
  (when expanded
    (dolist (child (oref job-section children))
      (when (and (eq (oref child type) 'actions-step)
                 (memq (cdr (assq 'number (oref child value))) expanded))
        (magit-section-show child)))))

(defun shipit-actions-refresh ()
  "Re-fetch and re-render the current view."
  (interactive)
  (setq shipit-actions--run-data nil
        shipit-actions--jobs-data nil
        shipit-actions--job-logs (make-hash-table :test 'eql)
        shipit-actions--job-logs-state (make-hash-table :test 'eql))
  (shipit-actions--fetch-and-render))

(defun shipit-actions-browse ()
  "Open current run in browser."
  (interactive)
  (let ((url (when shipit-actions--run-data
               (cdr (assq 'html_url shipit-actions--run-data)))))
    (if url
        (browse-url url)
      (message "No URL available"))))

;;; Section visibility save/restore

(defun shipit-actions--save-section-visibility ()
  "Save visibility state of all sections.  Returns an alist of (KEY . HIDDEN)."
  (when (bound-and-true-p magit-root-section)
    (shipit-actions--collect-visibility magit-root-section nil)))

(defun shipit-actions--collect-visibility (section state)
  "Collect visibility of SECTION and children into STATE list.
Returns the updated STATE."
  (let ((type (oref section type))
        (value (oref section value)))
    (when (memq type '(actions-step actions-log-group actions-job))
      (let ((key (cond
                  ((and (eq type 'actions-step) (listp value))
                   (cons 'step (cdr (assq 'number value))))
                  ((eq type 'actions-log-group)
                   (cons 'group value))
                  ((and (eq type 'actions-job) (listp value))
                   (cons 'job (cdr (assq 'name value)))))))
        (when key
          (push (cons key (oref section hidden)) state)))))
  (dolist (child (oref section children))
    (setq state (shipit-actions--collect-visibility child state)))
  state)

(defun shipit-actions--restore-section-visibility (state)
  "Restore section visibility from STATE alist."
  (when (and state (bound-and-true-p magit-root-section))
    (shipit-actions--apply-visibility magit-root-section state)))

(defun shipit-actions--apply-visibility (section state)
  "Apply saved visibility STATE to SECTION and children."
  (let ((type (oref section type))
        (value (oref section value)))
    (when (memq type '(actions-step actions-log-group actions-job))
      (let* ((key (cond
                   ((and (eq type 'actions-step) (listp value))
                    (cons 'step (cdr (assq 'number value))))
                   ((eq type 'actions-log-group)
                    (cons 'group value))
                   ((and (eq type 'actions-job) (listp value))
                    (cons 'job (cdr (assq 'name value))))))
             (saved (assoc key state)))
        (when saved
          (if (cdr saved)
              (magit-section-hide section)
            (magit-section-show section))))))
  (dolist (child (oref section children))
    (shipit-actions--apply-visibility child state)))

;;; Utilities

(defun shipit-actions--duration-seconds (start end)
  "Return duration in seconds between START and END ISO timestamps, or nil."
  (when (and start end
             (not (equal start :null))
             (not (equal end :null)))
    (float-time (time-subtract (date-to-time end) (date-to-time start)))))

(defun shipit-actions--format-duration (start end)
  "Format duration between START and END ISO timestamps.
Returns a human-readable string like \"2m 34s\", or nil."
  (let ((seconds (shipit-actions--duration-seconds start end)))
    (when seconds
      (let ((mins (floor (/ seconds 60)))
            (secs (floor (mod seconds 60))))
        (cond
         ((>= mins 60)
          (format "%dh %dm" (/ mins 60) (mod mins 60)))
         ((> mins 0)
          (format "%dm %ds" mins secs))
         (t
          (format "%ds" secs)))))))

(defun shipit-actions--duration-bar (seconds max-seconds status)
  "Return a proportional duration bar string.
SECONDS is this item's duration, MAX-SECONDS is the longest duration
in the group, STATUS is the conclusion/status for color."
  (when (and seconds max-seconds (> max-seconds 0))
    (let* ((max-width 20)
           (ratio (/ (float seconds) max-seconds))
           (filled (max 1 (round (* ratio max-width))))
           (color (pcase status
                    ("success" "#28a745")
                    ("failure" "#cb2431")
                    ("cancelled" "#999999")
                    ("skipped" "#999999")
                    ("in_progress" "#fd7e14")
                    (_ "#0366d6")))
           (bar (make-string filled ?█))
           (empty (make-string (- max-width filled) ?░)))
      (concat (propertize bar 'font-lock-face `(:foreground ,color))
              (propertize empty 'font-lock-face 'shadow)))))

(defun shipit-actions--status-icon (display-status)
  "Return an SVG or emoji status icon for DISPLAY-STATUS.
DISPLAY-STATUS is typically (or conclusion status) from the API."
  (let ((conclusion-statuses '("success" "failure" "cancelled" "skipped" "neutral"))
        (status-statuses '("in_progress" "queued" "waiting")))
    (let ((api-status (if (member display-status status-statuses) display-status nil))
          (api-conclusion (if (member display-status conclusion-statuses) display-status nil))
          (emoji (pcase display-status
                   ("success" "✅")
                   ("failure" "❌")
                   ("cancelled" "⚪")
                   ("skipped" "⚪")
                   ("in_progress" "🟡")
                   ("queued" "🔵")
                   ("waiting" "🔵")
                   (_ "❓"))))
      (shipit--get-check-status-icon api-status api-conclusion emoji))))

(defun shipit-actions--status-face (status)
  "Return a face for STATUS text label."
  (pcase status
    ("success" '(:foreground "#28a745"))
    ("failure" '(:foreground "#cb2431"))
    ("cancelled" '(:foreground "#999999"))
    ("skipped" '(:foreground "#999999"))
    ("in_progress" '(:foreground "#fd7e14"))
    ("queued" '(:foreground "#0366d6"))
    ("waiting" '(:foreground "#0366d6"))
    (_ 'shadow)))

;;; Timestamp gap coloring

(defface shipit-timestamp-gap-mild-face
  '((t :foreground "#d4a0a0"))
  "Face for timestamps with a mild gap (>= 5s by default)."
  :group 'shipit)

(defface shipit-timestamp-gap-moderate-face
  '((t :foreground "#e06060"))
  "Face for timestamps with a moderate gap (>= 30s by default)."
  :group 'shipit)

(defface shipit-timestamp-gap-high-face
  '((t :foreground "#ef3030"))
  "Face for timestamps with a high gap (>= 5m by default)."
  :group 'shipit)

(defface shipit-timestamp-gap-extreme-face
  '((t :foreground "#ff0000" :weight bold))
  "Face for timestamps with an extreme gap (>= 30m by default)."
  :group 'shipit)

(defcustom shipit-actions-timestamp-gap-thresholds
  '((5 . shipit-timestamp-gap-mild-face)
    (30 . shipit-timestamp-gap-moderate-face)
    (300 . shipit-timestamp-gap-high-face)
    (1800 . shipit-timestamp-gap-extreme-face))
  "Alist of (SECONDS . FACE) for timestamp gap coloring.
Entries must be sorted by SECONDS ascending.  The face from the
last entry whose threshold is met is used.  Below the first
threshold, `shipit-timestamp-face' is used."
  :type '(alist :key-type integer :value-type face)
  :group 'shipit)

(defun shipit-actions--gap-face (delta-secs)
  "Return the face for a timestamp gap of DELTA-SECS seconds."
  (if (<= delta-secs 0)
      'shipit-timestamp-face
    (let ((face 'shipit-timestamp-face))
      (dolist (entry shipit-actions-timestamp-gap-thresholds face)
        (when (>= delta-secs (car entry))
          (setq face (cdr entry)))))))

(defun shipit-actions--timestamp-to-seconds (ts)
  "Parse ISO 8601 timestamp TS into epoch seconds as a float.
Returns nil if TS is nil or unparseable."
  (when (and ts (stringp ts))
    (float-time (date-to-time ts))))

;;; Timestamps transient

(defun shipit-actions-toggle-log-timestamps ()
  "Toggle log line timestamp visibility and re-render."
  (interactive)
  (setq shipit-actions--show-timestamps
        (not shipit-actions--show-timestamps))
  (message "Log timestamps %s" (if shipit-actions--show-timestamps "on" "off"))
  (shipit-actions--re-render-current-job))

(defun shipit-actions-cycle-step-time-format ()
  "Cycle step start time between relative and absolute, then re-render."
  (interactive)
  (setq shipit-actions--step-time-format
        (if (eq shipit-actions--step-time-format 'absolute) 'relative 'absolute))
  (message "Step times: %s" shipit-actions--step-time-format)
  (shipit-actions--re-render-current-job))

(defun shipit-actions-toggle-gap-coloring ()
  "Toggle timestamp gap coloring and re-render."
  (interactive)
  (setq shipit-actions--show-gap-coloring
        (not shipit-actions--show-gap-coloring))
  (message "Gap coloring %s" (if shipit-actions--show-gap-coloring "on" "off"))
  (shipit-actions--re-render-current-job))

(defun shipit-actions--re-render-current-job ()
  "Re-render the enclosing job section, preserving position.
Works in `shipit-actions-mode', `shipit-actions-list-mode', and
the Checks section of the PR buffer."
  (let ((section (magit-current-section))
        (line (line-number-at-pos))
        (col (current-column)))
    (while (and section
                (not (memq (oref section type)
                           '(actions-job actions-list-run-job check-item))))
      (setq section (oref section parent)))
    (when (and section (oref section children))
      (cond
       ;; Checks section in PR buffer
       ((eq (oref section type) 'check-item)
        (let* ((check (oref section value))
               (job-id (cdr (assq 'id check)))
               (job-data (when (boundp 'shipit--check-item-job-data)
                           (gethash job-id shipit--check-item-job-data))))
          (when job-data
            (shipit--render-check-item-steps section job-data))))
       ;; Actions list buffer
       ((eq (oref section type) 'actions-list-run-job)
        (let ((expanded (shipit-actions--save-step-visibility section)))
          (shipit-actions-list--render-steps-in-section section)
          (shipit-actions--restore-step-visibility section expanded)
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column col)))
       ;; Actions run buffer
       (t
        (let* ((job (oref section value))
               (inhibit-read-only t)
               (content-pos (oref section content))
               (end-pos (oref section end))
               (expanded (shipit-actions--save-step-visibility section))
               (win (get-buffer-window (current-buffer)))
               (win-start (when win (window-start win))))
          (when (and content-pos end-pos)
            (delete-region content-pos end-pos)
            (goto-char content-pos)
            (let ((magit-insert-section--parent section))
              (oset section children nil)
              (shipit-actions--insert-steps
               job (shipit-actions--job-bar-col job)))
            (oset section end (point-marker))
            (oset section hidden nil)
            (shipit-actions--restore-step-visibility section expanded)
            (goto-char (point-min))
            (forward-line (1- line))
            (move-to-column col)
            (when (and win win-start)
              (set-window-start win win-start t)))))))))

(transient-define-prefix shipit-actions-timestamps-menu ()
  "Timestamps menu for GitHub Actions buffer."
  ["Timestamps"
   ("L" "Toggle visibility" shipit-actions-toggle-log-timestamps :transient t)
   ("l" "Cycle format" shipit-actions-cycle-step-time-format :transient t)
   ("c" "Toggle gap coloring" shipit-actions-toggle-gap-coloring :transient t)
   ("N" "Navigation..." shipit-actions-nav-menu)])

;;; Gap navigation

(defvar-local shipit-actions--nav-min-threshold 0
  "Index into `shipit-actions-timestamp-gap-thresholds' for navigation.
0 means any gap (>= first threshold), higher values skip smaller gaps.")

(defun shipit-actions--nav-faces-at-or-above ()
  "Return list of gap faces at or above the current navigation threshold."
  (let ((thresholds shipit-actions-timestamp-gap-thresholds)
        (idx shipit-actions--nav-min-threshold)
        (faces nil))
    (dotimes (i (length thresholds))
      (when (>= i idx)
        (push (cdr (nth i thresholds)) faces)))
    (nreverse faces)))

(defun shipit-actions--nav-threshold-label ()
  "Return a human-readable label for the current navigation threshold."
  (let* ((entry (nth shipit-actions--nav-min-threshold
                     shipit-actions-timestamp-gap-thresholds))
         (secs (if entry (car entry) 0)))
    (cond
     ((>= secs 3600) (format ">= %dh" (/ secs 3600)))
     ((>= secs 60) (format ">= %dm" (/ secs 60)))
     (t (format ">= %ds" secs)))))

(defun shipit-actions--line-has-gap-face-p ()
  "Return non-nil if the current line has a gap face at or above threshold."
  (let ((faces (shipit-actions--nav-faces-at-or-above))
        (end (line-end-position))
        (pos (line-beginning-position))
        (found nil))
    (while (and (not found) (< pos end))
      (let ((face (get-text-property pos 'font-lock-face)))
        (when (memq face faces)
          (setq found t)))
      (setq pos (next-single-property-change pos 'font-lock-face nil end)))
    found))

(defun shipit-actions-nav-next-gap ()
  "Jump to next line with a timestamp gap at or above current threshold."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (shipit-actions--line-has-gap-face-p)))
      (forward-line 1))
    (if (eobp)
        (progn
          (goto-char start)
          (message "No more gaps (%s)" (shipit-actions--nav-threshold-label)))
      (beginning-of-line)
      (message "Gap: %s" (shipit-actions--nav-threshold-label)))))

(defun shipit-actions-nav-prev-gap ()
  "Jump to previous line with a timestamp gap at or above current threshold."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (shipit-actions--line-has-gap-face-p)))
      (forward-line -1))
    (if (and (bobp) (not (shipit-actions--line-has-gap-face-p)))
        (progn
          (goto-char start)
          (message "No more gaps (%s)" (shipit-actions--nav-threshold-label)))
      (beginning-of-line)
      (message "Gap: %s" (shipit-actions--nav-threshold-label)))))

(defun shipit-actions-nav-set-threshold (idx)
  "Set navigation threshold to index IDX in the thresholds list."
  (setq shipit-actions--nav-min-threshold idx)
  (message "Navigation threshold: %s" (shipit-actions--nav-threshold-label)))

(defun shipit-actions-nav-threshold-1 ()
  "Set navigation threshold to level 1 (any gap)."
  (interactive)
  (shipit-actions-nav-set-threshold 0))

(defun shipit-actions-nav-threshold-2 ()
  "Set navigation threshold to level 2 (moderate)."
  (interactive)
  (shipit-actions-nav-set-threshold 1))

(defun shipit-actions-nav-threshold-3 ()
  "Set navigation threshold to level 3 (high)."
  (interactive)
  (shipit-actions-nav-set-threshold 2))

(defun shipit-actions-nav-threshold-4 ()
  "Set navigation threshold to level 4 (extreme)."
  (interactive)
  (shipit-actions-nav-set-threshold 3))

(transient-define-prefix shipit-actions-nav-menu ()
  "Gap navigation menu for GitHub Actions log timestamps."
  ["Navigation"
   ("n" "Next gap" shipit-actions-nav-next-gap :transient t)
   ("p" "Previous gap" shipit-actions-nav-prev-gap :transient t)
   ("C-n" "Next line" next-line :transient t)
   ("C-p" "Previous line" previous-line :transient t)
   ("C-l" "Recenter" recenter-top-bottom :transient t)
   ("L" "Timestamps..." shipit-actions-timestamps-menu)]
  ["Minimum gap"
   ("1" "Any (>= 5s)" shipit-actions-nav-threshold-1 :transient t)
   ("2" "Moderate (>= 30s)" shipit-actions-nav-threshold-2 :transient t)
   ("3" "High (>= 5m)" shipit-actions-nav-threshold-3 :transient t)
   ("4" "Extreme (>= 30m)" shipit-actions-nav-threshold-4 :transient t)])

;;; Transient menu

(transient-define-prefix shipit-actions-menu ()
  "Actions menu for GitHub Actions buffer."
  ["Actions"
   ("S" "Save raw log" shipit-actions-save-raw-log)
   ("L" "Timestamps..." shipit-actions-timestamps-menu)
   ("b" "Open in browser" shipit-actions-browse)
   ("g" "Refresh" shipit-actions-refresh)]
  ["Navigation"
   ("n" "Next gap" shipit-actions-nav-next-gap :transient t)
   ("p" "Previous gap" shipit-actions-nav-prev-gap :transient t)
   ("N" "Gap navigation..." shipit-actions-nav-menu)]
  ["Buffer"
   ("q" "Quit" shipit-actions-quit)])

(when (fboundp 'shipit-register-dwim-handler)
  (shipit-register-dwim-handler
   'actions-buffer
   (lambda () (derived-mode-p 'shipit-actions-mode))
   #'shipit-actions-menu))

;;; Shared URL helpers

(defun shipit-actions--step-url (section)
  "Return the GitHub URL for an actions-step SECTION, or nil.
Builds the URL from the parent section's html_url and step number."
  (let* ((step (oref section value))
         (step-number (cdr (assq 'number step)))
         (parent (oref section parent))
         (job-url (when parent
                    (cdr (assq 'html_url (oref parent value))))))
    (when (and job-url step-number)
      (format "%s#step:%d:1" job-url step-number))))

(defun shipit-actions-copy-url ()
  "Copy the URL of the item at point, or visible region text."
  (interactive)
  (if (use-region-p)
      (shipit-buffer--copy-visible-region)
    (or (shipit-actions-copy-url-at-point (shipit-actions--url-at-point))
        (message "No URL available at point"))))

(defun shipit-actions--url-at-point ()
  "Return the URL for the section at point in the actions detail buffer."
  (let ((section (magit-current-section))
        (repo shipit-actions--repo)
        (run-id shipit-actions--run-id))
    (when (and section repo)
      (pcase (oref section type)
        ('actions-header
         (when run-id
           (format "https://github.com/%s/actions/runs/%s" repo run-id)))
        ('actions-job
         (cdr (assq 'html_url (oref section value))))
        ('actions-step
         (shipit-actions--step-url section))
        ('actions-run-summary
         (when run-id
           (format "https://github.com/%s/actions/runs/%s" repo run-id)))))))

(defun shipit-actions-copy-url-at-point (url)
  "Copy URL to the kill ring and show a message.
Returns non-nil if URL was copied."
  (when url
    (kill-new url)
    (message "Copied: %s" url)
    t))

(provide 'shipit-actions)
;;; shipit-actions.el ends here
