;;; shipit-actions-list.el --- GitHub Actions workflows listing -*- lexical-binding: t; -*-

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
;; Lists GitHub Actions workflows and their recent runs in a magit-section buffer.
;; Supports expanding workflows to see runs, filtering by workflow, and
;; navigating to individual run details.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'mule-util)
(require 'transient)
(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-render)
(require 'shipit-actions)

;; Forward declarations
(declare-function shipit-buffer--copy-visible-region "shipit-buffer")
(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-section-show "magit-section")
(declare-function magit-section-hide "magit-section")
(declare-function magit-current-section "magit-section")
(declare-function magit-section-toggle "magit-section")
(declare-function shipit-actions-nav-next-gap "shipit-actions")
(declare-function shipit-actions-nav-prev-gap "shipit-actions")
(declare-function shipit-actions-nav-menu "shipit-actions")
(declare-function shipit-actions-timestamps-menu "shipit-actions")

(eval-when-compile
  (when (locate-library "magit")
    (require 'magit)
    (require 'magit-section)))

;; Magit section types
(defun actions-list-root (&rest _args)
  "Magit section identifier for actions list root.")
(put 'actions-list-root 'magit-section t)

(defun actions-list-workflows (&rest _args)
  "Magit section identifier for actions list workflows.")
(put 'actions-list-workflows 'magit-section t)

(defun actions-list-workflow (&rest _args)
  "Magit section identifier for actions list workflow.")
(put 'actions-list-workflow 'magit-section t)

(defun actions-list-run (&rest _args)
  "Magit section identifier for actions list run.")
(put 'actions-list-run 'magit-section t)

(defun actions-list-run-job (&rest _args)
  "Magit section identifier for actions list run job.")
(put 'actions-list-run-job 'magit-section t)

(defun actions-list-runners (&rest _args)
  "Magit section identifier for actions list runners.")
(put 'actions-list-runners 'magit-section t)

(defun actions-list-runner-group (&rest _args)
  "Magit section identifier for actions list runner group.")
(put 'actions-list-runner-group 'magit-section t)

(defun actions-list-runner (&rest _args)
  "Magit section identifier for actions list runner.")
(put 'actions-list-runner 'magit-section t)

(defun actions-list-run-summary (&rest _args)
  "Magit section identifier for run summary in actions list.")
(put 'actions-list-run-summary 'magit-section t)

;;; Customization

(defcustom shipit-actions-runs-per-page 10
  "Number of workflow runs to fetch per page."
  :type 'integer
  :group 'shipit)

;;; Buffer-local state

(defvar-local shipit-actions-list--repo nil
  "Repository owner/name for this actions list buffer.")

(defvar-local shipit-actions-list--workflows nil
  "List of workflow alists for this buffer.")

(defvar-local shipit-actions-list--workflow-runs (make-hash-table :test 'eql)
  "Hash table mapping workflow-id to list of run alists.")

(defvar-local shipit-actions-list--all-runs nil
  "List of all workflow runs (across all workflows).")

(defvar-local shipit-actions-list--runners nil
  "List of runner alists for this buffer.")

(defvar-local shipit-actions-list--run-jobs (make-hash-table :test 'eql)
  "Hash table mapping run-id to list of job alists.")

(defvar-local shipit-actions-list--job-logs (make-hash-table :test 'eql)
  "Hash table mapping job-id to alist of (step-number . log-items).")

(defvar-local shipit-actions-list--run-summaries (make-hash-table :test 'eql)
  "Hash table mapping run-id to list of (job-name . markdown) pairs.")

(defvar-local shipit-actions-list--filter-text nil
  "When non-nil, only show workflows whose name matches this text.")

(defvar-local shipit-actions-list--hide-inactive nil
  "When non-nil, hide workflows with no active runs.")

(defun shipit-actions-list--workflow-matches-filter-p (workflow filter-text)
  "Return non-nil if WORKFLOW name matches FILTER-TEXT.
Case-insensitive substring match.  Nil or empty FILTER-TEXT matches all."
  (or (null filter-text)
      (string-empty-p filter-text)
      (let ((name (or (cdr (assq 'name workflow)) "")))
        (string-match-p (regexp-quote (downcase filter-text))
                        (downcase name)))))

(defun shipit-actions-list--filter-active-p ()
  "Return non-nil if a text filter is currently active."
  (and shipit-actions-list--filter-text
       (not (string-empty-p shipit-actions-list--filter-text))))

;;; Keymap

(defvar shipit-actions-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'shipit-actions-list-dwim)
    (define-key map (kbd "TAB") #'shipit-actions-list-toggle-section)
    (define-key map [tab] #'shipit-actions-list-toggle-section)
    (define-key map (kbd "q") #'shipit-actions-list-quit)
    (define-key map (kbd "g") #'shipit-actions-list-refresh)
    (define-key map (kbd "b") #'shipit-actions-list-browse)
    (define-key map (kbd "f") #'shipit-actions-list-filter-menu)
    (define-key map (kbd "+") #'shipit-actions-list-fetch-more)
    (define-key map (kbd "M-w") #'shipit-actions-list-copy-url)
    (define-key map (kbd "L") #'shipit-actions-timestamps-menu)
    (define-key map (kbd "M-;") #'shipit-dwim)
    map)
  "Keymap for `shipit-actions-list-mode'.")

;;; Major mode

(define-derived-mode shipit-actions-list-mode magit-section-mode "Shipit-Actions-List"
  "Major mode for listing GitHub Actions workflows and runs.

\\{shipit-actions-list-mode-map}"
  :group 'shipit
  (setq-local shipit-actions-list--workflow-runs (make-hash-table :test 'eql))
  (setq-local shipit-actions-list--run-jobs (make-hash-table :test 'eql))
  (setq-local shipit-actions-list--job-logs (make-hash-table :test 'eql))
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (shipit-actions-list-refresh)))
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t))

;;; Entry point

;;;###autoload
(defun shipit-open-actions-list (repo)
  "Open a shipit buffer listing Actions workflows for REPO.
REPO should be in \"owner/name\" format.
Creates or reuses a buffer named *shipit-actions: REPO*."
  (interactive
   (list (or (ignore-errors (shipit--get-repo-from-remote))
             (read-string "Repository (owner/name): "))))
  (let* ((buf-name (format "*shipit-actions: %s*" repo))
         (existing (get-buffer buf-name)))
    (if existing
        (progn
          (switch-to-buffer existing)
          existing)
      (let ((buffer (generate-new-buffer buf-name)))
        (with-current-buffer buffer
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo repo)
          (shipit-actions-list--fetch-and-render))
        (switch-to-buffer buffer)
        buffer))))

;;; API fetching

(defun shipit-actions-list--fetch-workflows (repo callback)
  "Fetch all workflows for REPO, paginating if needed.
Call CALLBACK with complete workflow list."
  (shipit-actions-list--fetch-workflows-page repo 1 nil callback))

(defun shipit-actions-list--fetch-workflows-page (repo page accumulated callback)
  "Fetch page PAGE of workflows for REPO, accumulating results.
ACCUMULATED is the list of workflows collected so far.
Call CALLBACK with complete list when all pages are fetched."
  (shipit--api-request
   (format "/repos/%s/actions/workflows" repo)
   (list (list "per_page" 100) (list "page" page))
   (lambda (data)
     (let* ((workflows (when data
                         (append (cdr (assq 'workflows data)) nil)))
            (total (when data (cdr (assq 'total_count data))))
            (all (append accumulated workflows)))
       (if (and total (> total (length all)))
           (shipit-actions-list--fetch-workflows-page
            repo (1+ page) all callback)
         (funcall callback all))))))

(defun shipit-actions-list--fetch-runs-by-status (repo status callback)
  "Fetch runs for REPO filtered by STATUS, call CALLBACK with runs list."
  (shipit--api-request
   (format "/repos/%s/actions/runs" repo)
   (list (list "status" status) (list "per_page" 100))
   (lambda (data)
     (let ((runs (when data
                   (append (cdr (assq 'workflow_runs data)) nil))))
       (funcall callback runs)))))

(defun shipit-actions-list--merge-runs (recent active)
  "Merge RECENT and ACTIVE run lists, deduplicating by run id.
Result is sorted newest first by created_at."
  (let ((seen (make-hash-table :test 'eql))
        (result nil))
    (dolist (run (append active recent))
      (let ((id (cdr (assq 'id run))))
        (unless (gethash id seen)
          (puthash id t seen)
          (push run result))))
    (sort (nreverse result)
          (lambda (a b)
            (string> (or (cdr (assq 'created_at a)) "")
                     (or (cdr (assq 'created_at b)) ""))))))

(defun shipit-actions-list--fetch-all-runs (repo callback)
  "Fetch recent runs and all active runs for REPO.
Merges both result sets (deduped) and calls CALLBACK with the combined list."
  (let ((recent-runs nil)
        (active-runs nil)
        (pending 2))
    (cl-flet ((maybe-finish ()
                (cl-decf pending)
                (when (zerop pending)
                  (funcall callback
                           (shipit-actions-list--merge-runs
                            recent-runs active-runs)))))
      (shipit--api-request
       (format "/repos/%s/actions/runs" repo)
       (list (list "per_page" shipit-actions-runs-per-page))
       (lambda (data)
         (setq recent-runs
               (when data (append (cdr (assq 'workflow_runs data)) nil)))
         (maybe-finish)))
      (shipit-actions-list--fetch-runs-by-status
       repo "in_progress"
       (lambda (runs)
         (setq active-runs (append active-runs runs))
         (shipit-actions-list--fetch-runs-by-status
          repo "queued"
          (lambda (queued)
            (setq active-runs (append active-runs queued))
            (maybe-finish))))))))

(defun shipit-actions-list--fetch-workflow-runs (workflow-id callback)
  "Fetch recent runs for WORKFLOW-ID, call CALLBACK with runs.
Note: caller is responsible for caching in the buffer-local hash table,
since this callback runs outside the actions list buffer context."
  (let ((repo shipit-actions-list--repo))
    (shipit--api-request
     (format "/repos/%s/actions/workflows/%s/runs" repo workflow-id)
     (list (list "per_page" shipit-actions-runs-per-page))
     (lambda (data)
       (let ((runs (when data
                     (append (cdr (assq 'workflow_runs data)) nil))))
         (funcall callback runs))))))

(defun shipit-actions-list--fetch-runners (repo callback)
  "Fetch self-hosted runners for REPO, call CALLBACK with runners list.
Tries repo-level endpoint first, falls back to org-level on 404."
  (let ((repo-endpoint (format "/repos/%s/actions/runners" repo))
        (api-base (or shipit-api-url "https://api.github.com"))
        (headers (list (shipit--get-auth-header)
                       '("Accept" . "application/vnd.github.v3+json"))))
    (shipit--url-retrieve-async
     (concat api-base repo-endpoint)
     "GET" headers nil
     (lambda (data)
       (let ((runners (when data
                        (append (cdr (assq 'runners data)) nil))))
         (shipit--debug-log "Runners (repo) for %s: total_count=%s runners=%s"
                            repo (cdr (assq 'total_count data)) (length runners))
         (funcall callback runners)))
     (lambda (_error-msg)
       (shipit--debug-log "Repo runners 404 for %s, trying org endpoint" repo)
       (let ((org (car (split-string repo "/"))))
         (shipit-actions-list--fetch-org-runners org api-base headers callback))))))

(defun shipit-actions-list--fetch-org-runners (org api-base headers callback)
  "Fetch org-level self-hosted runners for ORG.
API-BASE is the GitHub API URL, HEADERS are auth headers.
Uses `shipit-org-runners-token' when set, otherwise HEADERS.
Calls CALLBACK with runners list or nil on error."
  (let ((url (concat api-base (format "/orgs/%s/actions/runners" org))))
    (let ((url-request-method "GET")
          (url-request-extra-headers
           (let ((runners-token (shipit--org-runners-token)))
             (if runners-token
                 (list `("Authorization" . ,(format "token %s" runners-token))
                       '("Accept" . "application/vnd.github.v3+json"))
               headers)))
          (url-request-data nil))
      (url-retrieve
       url
       (lambda (_status)
         (goto-char (point-min))
         (let* ((status-line (buffer-substring (point-min) (line-end-position)))
                (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                               (string-to-number (match-string 1 status-line))))
                (headers-end (search-forward "\n\n" nil t))
                (body (when headers-end
                        (buffer-substring-no-properties (point) (point-max)))))
           (if (and status-code (>= status-code 200) (< status-code 300))
               (let* ((json-array-type 'list)
                      (json-object-type 'alist)
                      (json-key-type 'symbol)
                      (data (when body
                              (goto-char headers-end)
                              (json-read)))
                      (runners (when data
                                 (append (cdr (assq 'runners data)) nil))))
                 (shipit--debug-log "Runners (org) for %s: total_count=%s runners=%s"
                                    org (cdr (assq 'total_count data)) (length runners))
                 (funcall callback runners))
             (shipit--debug-log "Org runners error for %s: HTTP %s body=%s"
                                org status-code
                                (when body (substring body 0 (min 500 (length body)))))
             (funcall callback nil)))
         (kill-buffer (current-buffer)))))))

(defun shipit-actions-list--fetch-and-render ()
  "Fetch workflows, all runs, and runners, then render."
  (let ((buf (current-buffer))
        (repo shipit-actions-list--repo))
    (message "Fetching actions for %s..." repo)
    (shipit-actions-list--fetch-workflows
     repo
     (lambda (workflows)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq shipit-actions-list--workflows workflows)
           (shipit-actions-list--fetch-all-runs
            repo
            (lambda (runs)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (setq shipit-actions-list--all-runs runs)
                  (shipit-actions-list--fetch-runners
                   repo
                   (lambda (runners)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (setq shipit-actions-list--runners runners)
                         (shipit-actions-list--render)
                         (goto-char (point-min))
                         (message "Actions for %s loaded." repo)))))))))))))))

(defun shipit-actions-list--fetch-run-jobs (run-id callback)
  "Fetch jobs for RUN-ID, call CALLBACK with jobs list.
Caches results in `shipit-actions-list--run-jobs'."
  (let ((repo shipit-actions-list--repo)
        (buf (current-buffer)))
    (shipit-actions--fetch-jobs
     repo run-id
     (lambda (jobs-vec)
       (let ((jobs (append jobs-vec nil)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (puthash run-id jobs shipit-actions-list--run-jobs)))
         (funcall callback jobs))))))

;;; Helpers

(defun shipit-actions-list--format-age (iso-timestamp)
  "Format ISO-TIMESTAMP as relative age like \"3h ago\".
Returns nil for nil or :null timestamps."
  (when (and iso-timestamp (not (eq iso-timestamp :null)))
    (let* ((then (float-time (date-to-time iso-timestamp)))
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
       (t (format-time-string "%Y-%m-%d" (seconds-to-time then)))))))

;;; Job rendering

(defun shipit-actions-list--insert-run-jobs (jobs)
  "Insert JOB entries as children of the current run section.
Each job shows a status icon, name, duration bar, and duration text.
JOBS is a list of job alists."
  (let* ((max-secs (shipit-actions--max-duration jobs))
         (max-label (shipit-actions-list--max-job-name-width jobs)))
    (dolist (job jobs)
      (shipit-actions-list--insert-run-job job max-secs max-label))))

(defun shipit-actions-list--max-job-name-width (jobs)
  "Return the maximum job name width across JOBS."
  (let ((max-w 0))
    (dolist (job jobs)
      (let ((w (length (or (cdr (assq 'name job)) ""))))
        (when (> w max-w) (setq max-w w))))
    max-w))

(defun shipit-actions-list--insert-run-job (job max-secs max-label)
  "Insert a single JOB as a child section.
MAX-SECS is the longest job duration for proportional bars.
MAX-LABEL is the widest job name for alignment."
  (let* ((name (or (cdr (assq 'name job)) ""))
         (status (cdr (assq 'status job)))
         (conclusion (cdr (assq 'conclusion job)))
         (display-status (or conclusion status))
         (started (cdr (assq 'started_at job)))
         (completed (cdr (assq 'completed_at job)))
         (secs (shipit-actions--duration-seconds started completed))
         (duration-text (shipit-actions--format-duration started completed))
         (icon (shipit-actions--status-icon display-status))
         (bar (when max-secs
                (shipit-actions--duration-bar secs max-secs display-status)))
         (padding (make-string (max 0 (- max-label (length name))) ?\s)))
    (magit-insert-section (actions-list-run-job job)
      (magit-insert-heading
        (concat "        " icon " "
                name padding
                (if bar (concat "  " bar) "")
                (if duration-text
                    (concat "  " (propertize duration-text 'face 'shipit-timestamp-face))
                  "")
                "\n")))))

;;; Run summary

(defun shipit-actions-list--insert-run-summary-section (run)
  "Insert a collapsed Summary section for RUN."
  (let* ((run-id (cdr (assq 'id run)))
         (cached (gethash run-id shipit-actions-list--run-summaries))
         (sentinel (make-symbol "missing"))
         (has-cache (not (eq (gethash run-id shipit-actions-list--run-summaries sentinel)
                             sentinel)))
         (icon (shipit--get-pr-field-icon "comment" "\U0001f4cb"))
         (sect (magit-insert-section (actions-list-run-summary run)
                 (magit-insert-heading
                   (concat "        " icon " "
                           (propertize "Summary" 'face 'magit-section-secondary-heading)
                           "\n"))
                 (when has-cache
                   (shipit-actions-list--insert-summary-content cached "            ")))))
    (magit-section-hide sect)))

(defun shipit-actions-list--insert-summary-content (summaries indent)
  "Insert rendered SUMMARIES content with INDENT prefix per line.
SUMMARIES is a list of (job-name . content) pairs, or nil."
  (if (null summaries)
      (insert indent (propertize "No annotations available" 'face 'shadow) "\n")
    (dolist (entry summaries)
      (let ((job-name (car entry))
            (content (cdr entry)))
        (insert indent (propertize job-name 'face 'bold) "\n")
        (dolist (line (split-string content "\n"))
          (insert indent line "\n"))
        (insert "\n")))))

(defun shipit-actions-list--fetch-run-summaries (run-id callback)
  "Fetch check-run summaries and annotations for all jobs of RUN-ID.
Calls CALLBACK with list of (job-name . content) pairs."
  (shipit-actions-list--fetch-summaries-for-jobs
   shipit-actions-list--repo
   (gethash run-id shipit-actions-list--run-jobs)
   callback))

(defun shipit-actions-list--fetch-summaries-for-jobs (repo jobs callback)
  "Fetch check-run summaries and annotations for JOBS in REPO.
Calls CALLBACK with list of (job-name . content) pairs.
Collects output.summary (when available) and annotations."
  (let ((remaining (length jobs))
        (results nil))
    (if (or (null jobs) (= remaining 0))
        (funcall callback nil)
      (dolist (job jobs)
        (let ((job-id (cdr (assq 'id job)))
              (job-name (or (cdr (assq 'name job)) "unknown")))
          (shipit--api-request
           (format "/repos/%s/check-runs/%s" repo job-id)
           nil
           (lambda (data)
             (let* ((output (cdr (assq 'output data)))
                    (summary (cdr (assq 'summary output)))
                    (ann-count (or (cdr (assq 'annotations_count output)) 0)))
               (shipit--debug-log "check-run %s/%s summary-type=%s annotations=%s"
                                  job-name job-id (type-of summary) ann-count)
               (if (> ann-count 0)
                   (shipit--api-request
                    (format "/repos/%s/check-runs/%s/annotations" repo job-id)
                    nil
                    (lambda (annotations)
                      (let ((content (shipit-actions-list--build-job-content
                                      summary annotations)))
                        (when content
                          (push (cons job-name content) results))
                        (cl-decf remaining)
                        (when (= remaining 0)
                          (funcall callback (nreverse results))))))
                 (when (and (stringp summary) (not (string-empty-p summary)))
                   (push (cons job-name summary) results))
                 (cl-decf remaining)
                 (when (= remaining 0)
                   (funcall callback (nreverse results))))))))))))

(defun shipit-actions-list--build-job-content (summary annotations)
  "Build combined content string from SUMMARY and ANNOTATIONS.
Returns nil if there is no content."
  (let ((parts nil))
    (when (and (stringp summary) (not (string-empty-p summary)))
      (push summary parts))
    (when (and (listp annotations) annotations)
      (dolist (ann annotations)
        (let ((level (cdr (assq 'annotation_level ann)))
              (message (cdr (assq 'message ann)))
              (title (cdr (assq 'title ann))))
          (when (and (stringp message) (not (string-empty-p message)))
            (push (concat (shipit-actions-list--annotation-prefix level)
                          (if (and (stringp title) (not (string-empty-p title)))
                              (concat title ": ")
                            "")
                          message)
                  parts)))))
    (when parts
      (string-join (nreverse parts) "\n\n"))))

(defun shipit-actions-list--annotation-prefix (level)
  "Return a prefix string for annotation LEVEL."
  (pcase level
    ("failure" "Error: ")
    ("warning" "Warning: ")
    ("notice" "Note: ")
    (_ "")))

(defun shipit-actions-list--expand-summary-section (section indent)
  "Expand summary SECTION, fetching data if needed.
INDENT is the prefix string for content lines."
  (let* ((run (oref section value))
         (run-id (cdr (assq 'id run)))
         (sentinel (make-symbol "missing"))
         (has-cache (not (eq (gethash run-id shipit-actions-list--run-summaries sentinel)
                             sentinel))))
    (if has-cache
        (magit-section-toggle section)
      (let ((buf (current-buffer))
            (sect section))
        (message "Fetching summaries...")
        (shipit-actions-list--fetch-run-summaries
         run-id
         (lambda (summaries)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (puthash run-id summaries shipit-actions-list--run-summaries)
               (shipit-actions-list--render-summary-in-section
                sect summaries indent)))))))))

(defun shipit-actions-list--render-summary-in-section (section summaries indent)
  "Render SUMMARIES into SECTION in-place with INDENT prefix."
  (let* ((inhibit-read-only t)
         (content-pos (oref section content))
         (end-pos (oref section end)))
    (when (and content-pos end-pos)
      (magit-section-show section)
      (delete-region content-pos end-pos)
      (goto-char content-pos)
      (shipit-actions-list--insert-summary-content summaries indent)
      (oset section end (point-marker)))))

;;; Step rendering

(defun shipit-actions-list--job-steps (job)
  "Return the steps list from JOB alist."
  (let ((steps (cdr (assq 'steps job))))
    (when steps (append steps nil))))

(defun shipit-actions-list--render-steps-in-section (section)
  "Render steps into SECTION using cached log data.
Binds `shipit-actions--job-logs' so step rendering picks up logs."
  (let* ((job (oref section value))
         (steps (shipit-actions-list--job-steps job))
         (inhibit-read-only t)
         (start (oref section start))
         (content-pos (oref section content))
         (end-pos (oref section end))
         (win (get-buffer-window (current-buffer)))
         (win-start (when win (window-start win))))
    (when (and content-pos end-pos)
      (delete-region content-pos end-pos)
      (goto-char content-pos)
      (let* ((magit-insert-section--parent section)
             (indent "            ")
             (prefix-width (+ (length indent) 2))
             (step-name-col (+ prefix-width
                               (cl-reduce #'max steps
                                          :key #'shipit-actions--step-label-width
                                          :initial-value 0)))
             ;; Ensure step bars start past job bars (job prefix=10 + job names + gap)
             (run-section (oref section parent))
             (sibling-jobs (when run-section
                             (gethash (cdr (assq 'id (oref run-section value)))
                                      shipit-actions-list--run-jobs)))
             (job-bar-col (+ 10 (shipit-actions-list--max-job-name-width
                                 (or sibling-jobs (list job)))
                             4))
             (bar-col (max step-name-col job-bar-col))
             (shipit-actions--job-logs shipit-actions-list--job-logs))
        (oset section children nil)
        (shipit-actions--insert-steps job bar-col indent))
      (oset section end (point-marker))
      (oset section hidden nil)
      (goto-char start)
      (when (and win win-start)
        (set-window-start win win-start)))))

(defun shipit-actions-list--insert-steps-into-section (section)
  "Insert steps for job SECTION, fetching logs first.
On first expand, fetches logs then renders steps with log content.
On subsequent expands, uses cached logs."
  (let* ((job (oref section value))
         (job-id (cdr (assq 'id job)))
         (steps (shipit-actions-list--job-steps job)))
    (when steps
      (if (gethash job-id shipit-actions-list--job-logs)
          (shipit-actions-list--render-steps-in-section section)
        (let ((buf (current-buffer))
              (sect section))
          (message "Fetching logs for job...")
          (let ((shipit-actions--repo shipit-actions-list--repo))
            (shipit-actions--fetch-job-logs-raw
             job-id
             (lambda (log-text)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when log-text
                     (puthash job-id
                              (shipit-actions--split-log-by-markers log-text job)
                              shipit-actions-list--job-logs))
                   (shipit-actions-list--render-steps-in-section sect)))))))))))

(defun shipit-actions-list--job-has-steps-p (section)
  "Return non-nil if the job SECTION has steps data and no children yet."
  (let ((job (oref section value)))
    (and (shipit-actions-list--job-steps job)
         (null (oref section children)))))

;;; In-place section updates

(defun shipit-actions-list--insert-jobs-into-section (section jobs)
  "Insert JOBS directly into the existing run SECTION in-place.
Avoids full buffer re-render."
  (let* ((inhibit-read-only t)
         (run (oref section value))
         (content-pos (oref section content))
         (end-pos (oref section end)))
    (when (and content-pos end-pos)
      (delete-region content-pos end-pos)
      (goto-char content-pos)
      (let ((magit-insert-section--parent section))
        (oset section children nil)
        (shipit-actions-list--insert-run-jobs jobs)
        (shipit-actions-list--insert-run-summary-section run))
      (oset section end (point-marker))
      (oset section hidden nil))))

;;; Rendering

(defun shipit-actions-list--runs-for-workflow (workflow-id)
  "Return cached runs for WORKFLOW-ID, or nil if not yet fetched.
Only returns data from the per-workflow cache.  The global all-runs
list is used exclusively by the All Workflows section."
  (gethash workflow-id shipit-actions-list--workflow-runs))

(defun shipit-actions-list--insert-all-workflows-entry ()
  "Insert the All Workflows collapsible section."
  (let* ((total (length shipit-actions-list--all-runs))
         (active (cl-count-if #'shipit-actions-list--active-run-p
                              shipit-actions-list--all-runs))
         (icon (shipit--get-pr-field-icon "activity" "\U0001f4a5"))
         (count-str (shipit-actions-list--format-workflow-counts active total))
         (sect (magit-insert-section (actions-list-workflow 'all)
                 (magit-insert-heading
                   (concat "  " icon " "
                           (propertize "All workflows" 'face 'magit-section-secondary-heading)
                           (if count-str
                               (concat "  " (propertize count-str 'face 'shadow))
                             "")
                           "\n"))
                 (shipit-actions-list--insert-runs shipit-actions-list--all-runs t))))
    (magit-section-hide sect)))

(defun shipit-actions-list--active-run-p (run)
  "Return non-nil if RUN is active (in progress, queued, or waiting)."
  (let ((status (cdr (assq 'status run))))
    (member status '("in_progress" "queued" "waiting"))))

(defun shipit-actions-list--workflow-has-active-runs-p (workflow-id)
  "Return non-nil if WORKFLOW-ID has any active runs."
  (let ((cached (shipit-actions-list--workflow-has-cached-runs-p workflow-id)))
    (cl-some (lambda (run)
               (and (or cached
                        (equal (cdr (assq 'workflow_id run)) workflow-id))
                    (shipit-actions-list--active-run-p run)))
             (if cached
                 (shipit-actions-list--runs-for-workflow workflow-id)
               shipit-actions-list--all-runs))))

(defun shipit-actions-list--workflow-counts (workflow-id)
  "Return (ACTIVE . FETCHED) run counts for WORKFLOW-ID.
ACTIVE counts from per-workflow cache if available, else from all-runs.
FETCHED is nil if not yet fetched, or the count of cached runs."
  (let* ((cached (shipit-actions-list--workflow-has-cached-runs-p workflow-id))
         (runs (if cached
                   (shipit-actions-list--runs-for-workflow workflow-id)
                 shipit-actions-list--all-runs))
         (active (cl-count-if
                  (lambda (run)
                    (and (or cached
                             (equal (cdr (assq 'workflow_id run)) workflow-id))
                         (shipit-actions-list--active-run-p run)))
                  runs))
         (fetched (when cached (length runs))))
    (cons active fetched)))

(defun shipit-actions-list--format-workflow-counts (active fetched)
  "Format ACTIVE and FETCHED run counts for a workflow heading.
FETCHED is nil when runs haven't been loaded yet, or a number."
  (cond
   ((and (> active 0) fetched (> fetched 0))
    (format "%d active | %d runs" active fetched))
   ((> active 0)
    (format "%d active" active))
   ((and fetched (> fetched 0))
    (format "%d runs" fetched))
   (fetched
    "no runs")
   (t nil)))

(defun shipit-actions-list--insert-workflow-entry (workflow)
  "Insert a single WORKFLOW collapsible section."
  (let* ((name (cdr (assq 'name workflow)))
         (id (cdr (assq 'id workflow)))
         (runs (shipit-actions-list--runs-for-workflow id))
         (counts (shipit-actions-list--workflow-counts id))
         (active (car counts))
         (fetched (cdr counts))
         (latest-run (car runs))
         (status-icon (if latest-run
                         (let ((display-status (or (cdr (assq 'conclusion latest-run))
                                                   (cdr (assq 'status latest-run)))))
                           (shipit-actions--status-icon display-status))
                       (shipit--get-pr-field-icon "checks" "\U0001f527")))
         (count-str (shipit-actions-list--format-workflow-counts active fetched)))
    (let ((sect (magit-insert-section (actions-list-workflow workflow)
                  (magit-insert-heading
                    (concat "  " status-icon " "
                            (propertize name 'face 'magit-section-secondary-heading)
                            (if count-str
                                (concat "  " (propertize count-str 'face 'shadow))
                              "")
                            "\n"))
                  (shipit-actions-list--insert-runs runs))))
      (magit-section-hide sect))))

(defun shipit-actions-list--workflow-visible-p (workflow)
  "Return non-nil if WORKFLOW should be shown given current filters."
  (and (shipit-actions-list--workflow-matches-filter-p
        workflow shipit-actions-list--filter-text)
       (or (not shipit-actions-list--hide-inactive)
           (shipit-actions-list--workflow-has-active-runs-p
            (cdr (assq 'id workflow))))))

(defun shipit-actions-list--filter-status-text ()
  "Return a status string describing active filters, or nil."
  (let ((parts nil))
    (when shipit-actions-list--hide-inactive
      (push "active only" parts))
    (when (shipit-actions-list--filter-active-p)
      (push (format "filter: %s" shipit-actions-list--filter-text) parts))
    (when parts
      (format " [%s]" (string-join parts " | ")))))

(defun shipit-actions-list--insert-workflows-section ()
  "Insert Workflows heading with workflow entries.
Respects text filter and hide-inactive toggle."
  (magit-insert-section (actions-list-workflows)
    (magit-insert-heading
      (concat (propertize "Workflows" 'face 'magit-section-heading)
              (when-let ((status (shipit-actions-list--filter-status-text)))
                (propertize status 'face 'shadow))))
    (unless (or (shipit-actions-list--filter-active-p)
                shipit-actions-list--hide-inactive)
      (shipit-actions-list--insert-all-workflows-entry))
    (let ((sorted (sort (copy-sequence shipit-actions-list--workflows)
                        (lambda (a b)
                          (string< (downcase (or (cdr (assq 'name a)) ""))
                                   (downcase (or (cdr (assq 'name b)) "")))))))
      (dolist (workflow sorted)
        (when (shipit-actions-list--workflow-visible-p workflow)
          (shipit-actions-list--insert-workflow-entry workflow))))))

(defun shipit-actions-list--workflow-name-for-run (run)
  "Return the workflow name for RUN, or nil if unknown."
  (let ((wf-id (cdr (assq 'workflow_id run))))
    (when wf-id
      (cl-loop for wf in shipit-actions-list--workflows
               when (equal (cdr (assq 'id wf)) wf-id)
               return (cdr (assq 'name wf))))))

(defun shipit-actions-list--run-column-data (run show-workflow)
  "Extract column data from RUN as a plist.
When SHOW-WORKFLOW is non-nil, include workflow name."
  (let* ((run-number (cdr (assq 'run_number run)))
         (branch (or (cdr (assq 'head_branch run)) ""))
         (created (cdr (assq 'created_at run)))
         (updated (cdr (assq 'updated_at run)))
         (duration (or (shipit-actions--format-duration created updated) ""))
         (age (or (shipit-actions-list--format-age created) ""))
         (head-commit (cdr (assq 'head_commit run)))
         (commit-msg (if head-commit
                         (let ((msg (cdr (assq 'message head-commit))))
                           (if msg
                               (truncate-string-to-width
                                (car (split-string msg "\n")) 40 nil nil "...")
                             ""))
                       ""))
         (wf-name (if show-workflow
                      (or (shipit-actions-list--workflow-name-for-run run) "")
                    nil)))
    (list :num (format "#%s" run-number)
          :wf wf-name
          :branch branch
          :commit commit-msg
          :duration duration
          :age age
          :secs (shipit-actions--duration-seconds created updated)
          :display-status (or (cdr (assq 'conclusion run))
                              (cdr (assq 'status run))))))

(defun shipit-actions-list--compute-column-widths (col-data-list)
  "Compute max column widths from COL-DATA-LIST (list of plists)."
  (let ((num-w 1) (wf-w 0) (branch-w 6) (dur-w 3) (age-w 3) (max-secs 0))
    (dolist (cols col-data-list)
      (setq num-w (max num-w (length (plist-get cols :num))))
      (when (plist-get cols :wf)
        (setq wf-w (max wf-w (length (plist-get cols :wf)))))
      (setq branch-w (max branch-w (length (plist-get cols :branch))))
      (setq dur-w (max dur-w (length (plist-get cols :duration))))
      (setq age-w (max age-w (length (plist-get cols :age))))
      (when-let* ((secs (plist-get cols :secs)))
        (setq max-secs (max max-secs secs))))
    (list :num num-w :wf wf-w :branch branch-w :dur dur-w :age age-w
          :max-secs max-secs)))

(defun shipit-actions-list--format-run-line (cols widths)
  "Format a run line from column data COLS using WIDTHS for alignment."
  (let* ((num (plist-get cols :num))
         (wf (plist-get cols :wf))
         (branch (plist-get cols :branch))
         (commit (plist-get cols :commit))
         (duration (plist-get cols :duration))
         (age (plist-get cols :age))
         (secs (plist-get cols :secs))
         (display-status (plist-get cols :display-status))
         (max-secs (plist-get widths :max-secs))
         (bar (shipit-actions--duration-bar secs max-secs display-status))
         (bar-w (if bar 22 0))  ;; 20 chars + 2 spacing
         (num-w (plist-get widths :num))
         (wf-w (plist-get widths :wf))
         (dur-w (plist-get widths :dur))
         (age-w (plist-get widths :age))
         (dur-padded (concat (make-string (max 0 (- dur-w (length duration))) ?\s)
                             duration))
         (age-padded (concat (make-string (max 0 (- age-w (length age))) ?\s)
                             age))
         (right-part (concat (if bar (concat bar "  ") "")
                             (propertize dur-padded 'face 'shipit-timestamp-face)
                             "  "
                             (propertize age-padded 'face 'shipit-timestamp-face)))
         (right-len (+ bar-w dur-w 2 age-w))
         (win-width (or (when-let* ((win (get-buffer-window (current-buffer))))
                          (window-body-width win))
                        80))
         ;; 7 = "    X " prefix (4 indent + icon + space) + 1 magit section indent
         (left-used (+ 7 num-w 2))
         (_ (when (and wf (> wf-w 0))
              (setq left-used (+ left-used wf-w 2))))
         (remaining (- win-width left-used right-len 2))
         (branch-max (min (length branch) (max 10 (/ remaining 3))))
         (commit-max (max 0 (- remaining branch-max 2)))
         (branch-trunc (truncate-string-to-width branch branch-max nil nil "..."))
         (commit-trunc (if (> (length commit) 0)
                           (truncate-string-to-width commit commit-max nil nil "...")
                         ""))
         (mid-part (concat (propertize branch-trunc 'face 'shipit-filename-face)
                           (if (> (length commit-trunc) 0)
                               (concat "  " commit-trunc)
                             "")))
         (mid-len (+ (length branch-trunc)
                     (if (> (length commit-trunc) 0) (+ 2 (length commit-trunc)) 0)))
         (gap (max 1 (- win-width left-used mid-len right-len 2))))
    (concat (propertize num 'face 'magit-section-secondary-heading)
            (make-string (max 1 (- num-w (length num) -2)) ?\s)
            (when (and wf (> wf-w 0))
              (concat (propertize (truncate-string-to-width wf wf-w nil nil "...")
                                  'face 'magit-tag-face)
                      (make-string (max 1 (- wf-w (min (length wf) wf-w) -2)) ?\s)))
            mid-part
            (make-string gap ?\s)
            right-part)))

(defun shipit-actions-list--pad-right (str width)
  "Pad STR with spaces to WIDTH."
  (let ((len (length str)))
    (if (< len width)
        (concat str (make-string (- width len) ?\s))
      str)))

(defun shipit-actions-list--insert-run-header (widths show-workflow)
  "Insert a header line for runs using WIDTHS for alignment.
SHOW-WORKFLOW controls whether workflow column is included."
  (let* ((num-w (plist-get widths :num))
         (wf-w (plist-get widths :wf))
         (dur-w (plist-get widths :dur))
         (age-w (plist-get widths :age))
         (win-width (or (when-let* ((win (get-buffer-window (current-buffer))))
                          (window-body-width win))
                        80))
         (max-secs (plist-get widths :max-secs))
         (bar-w (if (and max-secs (> max-secs 0)) 22 0))
         (right-part (concat (if (> bar-w 0)
                                 (make-string bar-w ?\s)
                               "")
                             (shipit-actions-list--pad-right "Duration" dur-w)
                             "  "
                             (shipit-actions-list--pad-right "Age" age-w)))
         ;; 7 = "    X " prefix (4 indent + icon + space) + 1 magit section indent
         (prefix "       ")
         (left-part (concat (shipit-actions-list--pad-right "#" num-w)
                            "  "
                            (when (and show-workflow (> wf-w 0))
                              (concat (shipit-actions-list--pad-right "Workflow" wf-w)
                                      "  "))
                            "Branch"))
         (left-len (+ (length prefix) (length left-part)))
         (right-len (length right-part))
         (gap (max 1 (- win-width left-len right-len)))
         (header (concat prefix
                         (propertize left-part 'face 'shadow)
                         (make-string gap ?\s)
                         (propertize right-part 'face 'shadow)
                         "\n")))
    (insert header)))

(defun shipit-actions-list--insert-run (run cols widths)
  "Insert a single RUN as a collapsible section.
COLS is pre-computed column data plist, WIDTHS is column widths plist."
  (let* ((status (cdr (assq 'status run)))
         (conclusion (cdr (assq 'conclusion run)))
         (display-status (or conclusion status))
         (icon (shipit-actions--status-icon display-status))
         (run-id (cdr (assq 'id run)))
         (cached-jobs (gethash run-id shipit-actions-list--run-jobs))
         (line (shipit-actions-list--format-run-line cols widths))
         (sect (magit-insert-section (actions-list-run run nil)
                 (magit-insert-heading
                   (concat "    " icon " " line "\n"))
                 (when cached-jobs
                   (shipit-actions-list--insert-run-jobs cached-jobs)
                   (shipit-actions-list--insert-run-summary-section run)))))
    (magit-section-hide sect)))

(defun shipit-actions-list--insert-runs (runs &optional show-workflow)
  "Insert RUN sections as children of current section.
When SHOW-WORKFLOW is non-nil, each run shows its workflow name."
  (when runs
    (let* ((col-data (mapcar (lambda (r)
                               (shipit-actions-list--run-column-data r show-workflow))
                             runs))
           (widths (shipit-actions-list--compute-column-widths col-data)))
      (shipit-actions-list--insert-run-header widths show-workflow)
      (cl-mapc (lambda (run cols)
                 (shipit-actions-list--insert-run run cols widths))
               runs col-data))))

(defun shipit-actions-list--runner-status-text (runner)
  "Return the display status text for RUNNER.
Returns \"busy\" if busy, otherwise the runner's status field."
  (let ((status (cdr (assq 'status runner)))
        (busy (cdr (assq 'busy runner))))
    (cond
     (busy "busy")
     (t (or status "offline")))))

(defun shipit-actions-list--runner-status-face (status-text)
  "Return a face for runner STATUS-TEXT."
  (pcase status-text
    ("online" '(:foreground "#28a745"))
    ("busy" '(:foreground "#fd7e14"))
    (_ '(:foreground "#999999"))))

(defun shipit-actions-list--insert-runner (runner)
  "Insert a single RUNNER line."
  (let* ((name (or (cdr (assq 'name runner)) ""))
         (os (or (cdr (assq 'os runner)) ""))
         (status-text (shipit-actions-list--runner-status-text runner))
         (icon (shipit-actions--status-icon
                (if (string= status-text "online") "success" "cancelled")))
         (face (shipit-actions-list--runner-status-face status-text)))
    (magit-insert-section (actions-list-runner runner)
      (magit-insert-heading
        (concat "    " icon " "
                (propertize name 'face 'magit-section-secondary-heading)
                "  "
                (propertize os 'face 'shadow)
                "  "
                (propertize status-text 'face face)
                "\n")))))

(defun shipit-actions-list--insert-runners-section ()
  "Insert the Runners section."
  (magit-insert-section (actions-list-runners)
    (magit-insert-heading
      (propertize "Runners" 'face 'magit-section-heading))
    (if shipit-actions-list--runners
        (let ((sect (magit-insert-section (actions-list-runner-group 'self-hosted)
                      (magit-insert-heading
                        (format "  %s"
                                (propertize "Self-hosted" 'face 'magit-section-secondary-heading)))
                      (dolist (runner shipit-actions-list--runners)
                        (shipit-actions-list--insert-runner runner)))))
          (magit-section-show sect))
      (insert "  (no self-hosted runners)\n"))))

(defun shipit-actions-list--render ()
  "Render the actions list buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (actions-list-root)
      (insert (propertize (format "Actions: %s" shipit-actions-list--repo)
                          'face 'magit-header-line)
              "\n\n")
      (shipit-actions-list--insert-workflows-section)
      (insert "\n")
      (shipit-actions-list--insert-runners-section))))

;;; Interactive commands

(defun shipit-actions-list--workflow-section-id (section)
  "Return the workflow identity for SECTION (workflow id or \\='all)."
  (let ((val (oref section value)))
    (if (eq val 'all) 'all (cdr (assq 'id val)))))

(defun shipit-actions-list--find-and-expand-run (run-id workflow-id)
  "Find the run section with RUN-ID under workflow WORKFLOW-ID and expand it.
WORKFLOW-ID is a numeric workflow id or \\='all."
  (cl-labels ((walk (section)
                (when (and (eq (oref section type) 'actions-list-run)
                           (equal (cdr (assq 'id (oref section value))) run-id))
                  (let ((parent (oref section parent)))
                    (when (and parent
                               (eq (oref parent type) 'actions-list-workflow)
                               (equal (shipit-actions-list--workflow-section-id parent)
                                      workflow-id))
                      (magit-section-show parent)
                      (magit-section-show section)
                      (goto-char (oref section start))
                      (throw 'found t))))
                (dolist (child (oref section children))
                  (walk child))))
    (catch 'found
      (walk magit-root-section))))

(defun shipit-actions-list--workflow-has-cached-runs-p (wf-id)
  "Return non-nil if WF-ID has been fetched (even if result was empty)."
  (let ((sentinel (make-symbol "missing")))
    (not (eq (gethash wf-id shipit-actions-list--workflow-runs sentinel) sentinel))))

(defun shipit-actions-list--workflow-needs-fetch-p (section)
  "Return non-nil if workflow SECTION has not yet had its runs fetched."
  (let* ((val (oref section value))
         (wf-id (if (eq val 'all) nil (cdr (assq 'id val)))))
    (and wf-id
         (not (shipit-actions-list--workflow-has-cached-runs-p wf-id)))))


(defun shipit-actions-list--save-expanded-workflows ()
  "Return list of workflow IDs whose sections are currently expanded."
  (let ((expanded nil))
    (when (bound-and-true-p magit-root-section)
      (dolist (child (oref magit-root-section children))
        (when (eq (oref child type) 'actions-list-workflows)
          (dolist (wf-section (oref child children))
            (when (and (eq (oref wf-section type) 'actions-list-workflow)
                       (not (oref wf-section hidden)))
              (push (shipit-actions-list--workflow-section-id wf-section)
                    expanded))))))
    expanded))

(defun shipit-actions-list--restore-expanded-workflows (workflow-ids)
  "Expand workflow sections matching WORKFLOW-IDS."
  (dolist (wf-id workflow-ids)
    (shipit-actions-list--find-and-expand-workflow wf-id)))

(defun shipit-actions-list--find-and-expand-workflow (workflow-id)
  "Find the workflow section for WORKFLOW-ID, expand it and move point there."
  (cl-labels ((walk (section)
                (when (and (eq (oref section type) 'actions-list-workflow)
                           (equal (shipit-actions-list--workflow-section-id section)
                                  workflow-id))
                  (magit-section-show section)
                  (goto-char (oref section start))
                  (throw 'found t))
                (dolist (child (oref section children))
                  (walk child))))
    (catch 'found
      (walk magit-root-section))))

(defun shipit-actions-list--lazy-fetch-workflow-runs (section)
  "Fetch runs for the workflow at SECTION and expand it."
  (let* ((val (oref section value))
         (wf-id (cdr (assq 'id val)))
         (buf (current-buffer))
         (win (get-buffer-window buf))
         (saved-win-start (when win (window-start win))))
    (message "Fetching runs for workflow...")
    (shipit-actions-list--fetch-workflow-runs
     wf-id
     (lambda (runs)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (puthash wf-id runs shipit-actions-list--workflow-runs)
           (shipit-actions-list--render)
           (shipit-actions-list--find-and-expand-workflow wf-id)
           (when (and win (window-live-p win))
             (set-window-point win (point))
             (set-window-start win saved-win-start t))))))))


(defun shipit-actions-list-toggle-section ()
  "Toggle section at point.
Fetches run jobs on first expand of a run section.
Expands job steps on first expand of a job section.
Fetches runs on first expand of a workflow section with no runs."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (pcase (oref section type)
        ('actions-list-run
         (shipit-actions-list--toggle-run section))
        ('actions-list-run-job
         (if (shipit-actions-list--job-has-steps-p section)
             (shipit-actions-list--insert-steps-into-section section)
           (magit-section-toggle section)))
        ('actions-list-run-summary
         (shipit-actions-list--expand-summary-section section "            "))
        ('actions-list-workflow
         (if (shipit-actions-list--workflow-needs-fetch-p section)
             (shipit-actions-list--lazy-fetch-workflow-runs section)
           (magit-section-toggle section)))
        (_
         (magit-section-toggle section))))))


(defun shipit-actions-list--toggle-run (section)
  "Toggle a run SECTION, fetching jobs on first expand."
  (let* ((run (oref section value))
         (run-id (cdr (assq 'id run))))
    (if (gethash run-id shipit-actions-list--run-jobs)
        (magit-section-toggle section)
      (let* ((buf (current-buffer))
             (sect section)
             (win (get-buffer-window buf))
             (saved-win-start (when win (window-start win))))
        (message "Fetching jobs for run #%s..." (cdr (assq 'run_number run)))
        (shipit-actions-list--fetch-run-jobs
         run-id
         (lambda (jobs)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (shipit-actions-list--insert-jobs-into-section sect jobs))
             (when (and win (window-live-p win))
               (set-window-point win (marker-position (oref sect start)))
               (set-window-start win saved-win-start)))))))))

(defun shipit-actions-list-dwim ()
  "Context-sensitive action at point.
On a run-job section: open the run viewer with that job expanded.
On a run section: lazy-fetch jobs then toggle.
Otherwise: toggle the section."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (pcase (oref section type)
        ('actions-list-run-job
         (let* ((job (oref section value))
                (run-section (oref section parent))
                (run (oref run-section value))
                (run-id (cdr (assq 'id run)))
                (job-name (cdr (assq 'name job))))
           (shipit-open-actions-run shipit-actions-list--repo run-id job-name)))
        ('actions-list-run
         (shipit-actions-list--toggle-run section))
        (_
         (magit-section-toggle section))))))

(defun shipit-actions-list-quit ()
  "Quit the actions list buffer."
  (interactive)
  (quit-window))

(defun shipit-actions-list-refresh ()
  "Refresh the actions list buffer."
  (interactive)
  (setq shipit-actions-list--workflows nil)
  (setq shipit-actions-list--all-runs nil)
  (setq shipit-actions-list--runners nil)
  (setq shipit-actions-list--workflow-runs (make-hash-table :test 'eql))
  (setq shipit-actions-list--run-summaries (make-hash-table :test 'eql))
  (shipit-actions-list--fetch-and-render))

(defun shipit-actions-list-browse ()
  "Open the GitHub Actions page in a web browser."
  (interactive)
  (browse-url (format "https://github.com/%s/actions" shipit-actions-list--repo)))

(defun shipit-actions-list--url-at-point ()
  "Return the URL for the section at point, or nil."
  (let ((section (magit-current-section)))
    (when section
      (pcase (oref section type)
        ((or 'actions-list-run 'actions-list-run-job)
         (cdr (assq 'html_url (oref section value))))
        ('actions-step
         (shipit-actions--step-url section))
        ('actions-list-workflow
         (let ((wf (oref section value)))
           (when (and (listp wf) (cdr (assq 'path wf)))
             (format "https://github.com/%s/actions/workflows/%s"
                     shipit-actions-list--repo
                     (file-name-nondirectory (cdr (assq 'path wf)))))))))))

(defun shipit-actions-list-copy-url ()
  "Copy the URL of the item at point, or visible region text.
With active region, copies only visible text (collapsed sections skipped)."
  (interactive)
  (if (use-region-p)
      (shipit-buffer--copy-visible-region)
    (or (shipit-actions-copy-url-at-point (shipit-actions-list--url-at-point))
        (message "No URL available at point"))))

(defun shipit-actions-list--find-workflow-section (section)
  "Walk up from SECTION to find the nearest workflow section."
  (while (and section (not (eq (oref section type) 'actions-list-workflow)))
    (setq section (oref section parent)))
  section)

(defun shipit-actions-list-filter ()
  "Filter workflows by name with live updates as you type.
Empty input clears the filter."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (last-input (or shipit-actions-list--filter-text ""))
         (timer nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-command-hook
                    (lambda ()
                      (let ((input (minibuffer-contents-no-properties)))
                        (unless (string= input last-input)
                          (setq last-input input)
                          (when timer (cancel-timer timer))
                          (setq timer
                                (run-with-idle-timer 0.1 nil
                                                     (lambda ()
                                                       (condition-case nil
                                                           (when (buffer-live-p original-buffer)
                                                             (with-current-buffer original-buffer
                                                               (setq shipit-actions-list--filter-text input)
                                                               (shipit-actions-list--render)
                                                               (redisplay t)))
                                                         (error nil))))))))
                    nil t))
      (let ((new-filter (read-string "Filter workflows: "
                                     (or shipit-actions-list--filter-text ""))))
        (setq shipit-actions-list--filter-text
              (if (string-empty-p new-filter) nil new-filter))
        (shipit-actions-list--render)
        (message (if shipit-actions-list--filter-text
                     (format "Filter: %s" shipit-actions-list--filter-text)
                   "Filter cleared"))))))

(defun shipit-actions-list-clear-filter ()
  "Clear all filters (text filter and hide-inactive)."
  (interactive)
  (setq shipit-actions-list--filter-text nil)
  (setq shipit-actions-list--hide-inactive nil)
  (shipit-actions-list--render)
  (message "Filters cleared"))

(defun shipit-actions-list-toggle-hide-inactive ()
  "Toggle hiding workflows with no active runs."
  (interactive)
  (setq shipit-actions-list--hide-inactive
        (not shipit-actions-list--hide-inactive))
  (shipit-actions-list--render)
  (message (if shipit-actions-list--hide-inactive
               "Showing active workflows only"
             "Showing all workflows")))

(transient-define-prefix shipit-actions-list-filter-menu ()
  "Filter workflows."
  ["Filter Workflows"
   [("f" "By name" shipit-actions-list-filter)
    ("h" "Toggle active only" shipit-actions-list-toggle-hide-inactive)]
   [("x" "Clear filters" shipit-actions-list-clear-filter)
    ("q" "Quit" transient-quit-one)]])

(defun shipit-actions-list-fetch-more (count)
  "Fetch COUNT more runs for the workflow at point.
With prefix argument, prompt for count.
Without prefix, fetches `shipit-actions-runs-per-page' more runs."
  (interactive (list (cond
                      ((numberp current-prefix-arg) current-prefix-arg)
                      (current-prefix-arg
                       (read-number "Fetch how many more runs? "
                                    shipit-actions-runs-per-page))
                      (t shipit-actions-runs-per-page))))
  (let* ((section (shipit-actions-list--find-workflow-section
                    (magit-current-section)))
         (wf (when section (oref section value)))
         (wf-id (when (and wf (listp wf)) (cdr (assq 'id wf)))))
    (shipit-actions-list--fetch-more-runs wf-id count)))

(defun shipit-actions-list--fetch-more-runs (workflow-id count)
  "Fetch COUNT more runs for WORKFLOW-ID (or all if nil).
Appends to existing runs and re-renders."
  (let* ((buf (current-buffer))
         (repo shipit-actions-list--repo)
         (existing (if workflow-id
                       (shipit-actions-list--runs-for-workflow workflow-id)
                     shipit-actions-list--all-runs))
         (page (1+ (/ (length existing) count)))
         (endpoint (if workflow-id
                       (format "/repos/%s/actions/workflows/%s/runs" repo workflow-id)
                     (format "/repos/%s/actions/runs" repo)))
         (params (list (list "per_page" count)
                       (list "page" page))))
    (shipit--api-request
     endpoint
     params
     (lambda (data)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((new-runs (when data
                             (append (cdr (assq 'workflow_runs data)) nil))))
             (if new-runs
                 (progn
                   (setq shipit-actions-list--all-runs
                         (append shipit-actions-list--all-runs new-runs))
                   (when workflow-id
                     (puthash workflow-id
                              (append (gethash workflow-id
                                               shipit-actions-list--workflow-runs)
                                      new-runs)
                              shipit-actions-list--workflow-runs))
                   (let ((expanded-wfs (shipit-actions-list--save-expanded-workflows))
                         (win (get-buffer-window buf))
                         (win-start (when (get-buffer-window buf)
                                      (window-start (get-buffer-window buf)))))
                     (shipit-actions-list--render)
                     (shipit-actions-list--restore-expanded-workflows expanded-wfs)
                     (when (and win (window-live-p win) win-start)
                       (set-window-start win win-start t)))
                   (message "Fetched %d more runs." (length new-runs)))
               (message "No more runs to fetch.")))))))))


(transient-define-prefix shipit-actions-list-menu ()
  "Actions list menu."
  ["Actions"
   ("f" "Filter..." shipit-actions-list-filter-menu)
   ("L" "Timestamps..." shipit-actions-timestamps-menu)
   ("b" "Open in browser" shipit-actions-list-browse)
   ("g" "Refresh" shipit-actions-list-refresh)]
  ["Navigation"
   ("n" "Next gap" shipit-actions-nav-next-gap :transient t)
   ("p" "Previous gap" shipit-actions-nav-prev-gap :transient t)
   ("N" "Gap navigation..." shipit-actions-nav-menu)]
  ["Buffer"
   ("q" "Quit" shipit-actions-list-quit)])

(require 'shipit-pr-actions)
(shipit-register-dwim-handler
 'actions-list-buffer
 (lambda () (derived-mode-p 'shipit-actions-list-mode))
 #'shipit-actions-list-menu)

(provide 'shipit-actions-list)
;;; shipit-actions-list.el ends here
