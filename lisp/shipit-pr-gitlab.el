;;; shipit-pr-gitlab.el --- GitLab MR backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

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
;; GitLab MR backend for the pluggable PR system.
;; Normalizes GitLab MR data to common shipit alists for UI consumption.

;;; Code:

(require 'seq)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-gitlab-http)

;;; Normalization — MR to common shipit PR alist

(defun shipit-pr-gitlab--normalize-mr (gitlab-mr)
  "Normalize a GitLab MR alist GITLAB-MR to common shipit PR shape."
  (let* ((state-raw (cdr (assq 'state gitlab-mr)))
         (state (if (string= state-raw "opened") "open" "closed"))
         (merged-at (cdr (assq 'merged_at gitlab-mr)))
         (author (cdr (assq 'author gitlab-mr)))
         (draft-val (cdr (assq 'draft gitlab-mr)))
         (draft (shipit-pr-gitlab--truthy-p draft-val))
         (labels-raw (cdr (assq 'labels gitlab-mr)))
         (labels (shipit-pr-gitlab--normalize-labels labels-raw))
         (assignees-raw (cdr (assq 'assignees gitlab-mr)))
         (assignees (shipit-pr-gitlab--normalize-assignees assignees-raw))
         ;; diff_refs.start_sha = target branch tip (shipit base.sha)
         (diff-refs (cdr (assq 'diff_refs gitlab-mr)))
         (base-sha (cdr (assq 'start_sha diff-refs))))
    `((number . ,(cdr (assq 'iid gitlab-mr)))
      (title . ,(cdr (assq 'title gitlab-mr)))
      (body . ,(cdr (assq 'description gitlab-mr)))
      (state . ,state)
      (merged_at . ,merged-at)
      (draft . ,draft)
      (html_url . ,(cdr (assq 'web_url gitlab-mr)))
      (user . ((login . ,(cdr (assq 'username author)))
               (avatar_url . ,(cdr (assq 'avatar_url author)))))
      (head . ((ref . ,(cdr (assq 'source_branch gitlab-mr)))
               (sha . ,(cdr (assq 'sha gitlab-mr)))))
      (base . ((ref . ,(cdr (assq 'target_branch gitlab-mr)))
               ,@(when base-sha `((sha . ,base-sha)))))
      (comments . ,(or (cdr (assq 'user_notes_count gitlab-mr)) 0))
      (labels . ,labels)
      (assignees . ,assignees)
      (created_at . ,(cdr (assq 'created_at gitlab-mr)))
      (updated_at . ,(cdr (assq 'updated_at gitlab-mr))))))

(defun shipit-pr-gitlab--truthy-p (val)
  "Return t if VAL is truthy, nil otherwise.
Handles :json-false from json.el."
  (and val (not (eq val :json-false))))

(defun shipit-pr-gitlab--normalize-labels (labels-raw)
  "Normalize GitLab LABELS-RAW (vector of strings) to common shipit shape.
Returns list of ((name . LABEL) ...)."
  (when labels-raw
    (mapcar (lambda (label)
              (if (stringp label)
                  `((name . ,label))
                `((name . ,(cdr (assq 'name label))))))
            (append labels-raw nil))))

(defun shipit-pr-gitlab--normalize-assignees (assignees-raw)
  "Normalize GitLab ASSIGNEES-RAW (vector of user objects) to common shipit shape.
Returns list of ((login . USERNAME) (avatar_url . URL))."
  (when assignees-raw
    (mapcar (lambda (assignee)
              `((login . ,(cdr (assq 'username assignee)))
                (avatar_url . ,(cdr (assq 'avatar_url assignee)))))
            (append assignees-raw nil))))

;;; Normalization — Pipeline jobs to common shipit check shape

(defun shipit-pr-gitlab--normalize-pipeline-job (job)
  "Normalize a GitLab pipeline JOB to common shipit check shape.
Maps stage to `workflow-name' for hierarchical grouping in the UI."
  (let* ((gitlab-status (cdr (assq 'status job)))
         (mapped (shipit-pr-gitlab--map-job-status gitlab-status)))
    `((name . ,(cdr (assq 'name job)))
      (status . ,(car mapped))
      (conclusion . ,(cdr mapped))
      (html_url . ,(cdr (assq 'web_url job)))
      (started_at . ,(cdr (assq 'started_at job)))
      (completed_at . ,(cdr (assq 'finished_at job)))
      (workflow-name . ,(or (cdr (assq 'stage job)) "Pipeline")))))

(defun shipit-pr-gitlab--map-job-status (gitlab-status)
  "Map GitLab job GITLAB-STATUS to (shipit-status . shipit-conclusion)."
  (pcase gitlab-status
    ((or "created" "pending" "waiting_for_resource")
     (cons "queued" nil))
    ("running"
     (cons "in_progress" nil))
    ("success"
     (cons "completed" "success"))
    ("failed"
     (cons "completed" "failure"))
    ("canceled"
     (cons "completed" "cancelled"))
    ("skipped"
     (cons "completed" "skipped"))
    ("manual"
     (cons "completed" "action_required"))
    (_
     (cons "queued" nil))))

;;; Normalization — MR changes to common shipit file shape

(defun shipit-pr-gitlab--normalize-file (change)
  "Normalize a GitLab MR CHANGE entry to common shipit file shape."
  (let* ((new-file (shipit-pr-gitlab--truthy-p (cdr (assq 'new_file change))))
         (deleted-file (shipit-pr-gitlab--truthy-p (cdr (assq 'deleted_file change))))
         (renamed-file (shipit-pr-gitlab--truthy-p (cdr (assq 'renamed_file change))))
         (status (cond (new-file "added")
                       (deleted-file "removed")
                       (renamed-file "renamed")
                       (t "modified")))
         (diff-text (or (cdr (assq 'diff change)) ""))
         (additions (shipit-pr-gitlab--count-diff-lines diff-text "+"))
         (deletions (shipit-pr-gitlab--count-diff-lines diff-text "-")))
    `((filename . ,(cdr (assq 'new_path change)))
      (status . ,status)
      (additions . ,additions)
      (deletions . ,deletions)
      (patch . ,(unless (string-empty-p diff-text) diff-text))
      ,@(when renamed-file
          `((previous_filename . ,(cdr (assq 'old_path change))))))))

(defun shipit-pr-gitlab--count-diff-lines (diff-text prefix)
  "Count lines in DIFF-TEXT starting with PREFIX (+ or -)."
  (let ((count 0)
        (pos 0))
    (while (string-match (concat "^" (regexp-quote prefix) "[^" prefix "]")
                         diff-text pos)
      (setq count (1+ count))
      (setq pos (1+ (match-beginning 0))))
    count))

;;; Normalization — Commits

(defun shipit-pr-gitlab--normalize-commit (commit)
  "Normalize a GitLab COMMIT to common shipit commit shape."
  `((sha . ,(cdr (assq 'id commit)))
    (commit . ((message . ,(cdr (assq 'message commit)))
               (author . ((name . ,(cdr (assq 'author_name commit)))
                          (date . ,(cdr (assq 'created_at commit)))))))
    (html_url . ,(cdr (assq 'web_url commit)))))

;;; Required backend operations (13 keys)

(defun shipit-pr-gitlab--fetch-pr (config number)
  "Fetch MR NUMBER using CONFIG."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching MR !%s" number)
    (when data
      (shipit-pr-gitlab--normalize-mr data))))

(defun shipit-pr-gitlab--search (config args)
  "Search MRs using CONFIG with transient ARGS.
ARGS may be raw transient args (list of strings like \"--state=open\")
or a plist (:state \"open\" ...).  Raw transient args are converted
to plist via `shipit--transient-args-to-search-plist'."
  (let* ((search-args (if (and args (stringp (car args)))
                          (shipit--transient-args-to-search-plist args)
                        args))
         (project (shipit-gitlab--project-path config))
         (query-params (shipit-pr-gitlab--build-search-params search-args))
         (path (format "/projects/%s/merge_requests?%s" project query-params))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: searching MRs with %s" query-params)
    (when (and data (or (vectorp data) (listp data)))
      (mapcar #'shipit-pr-gitlab--normalize-mr (append data nil)))))

(defun shipit-pr-gitlab--build-search-params (args)
  "Build URL query params from search ARGS plist.
Recognized keys: :search, :state, :author, :label, :draft."
  (let ((params '()))
    (when-let* ((search (plist-get args :search)))
      (push (format "search=%s" (url-hexify-string search)) params))
    (when-let* ((state (plist-get args :state)))
      ;; Map common shipit states to GitLab: open→opened
      (let ((gl-state (pcase state
                        ("open" "opened")
                        (_ state))))
        (push (format "state=%s" gl-state) params)))
    (when-let* ((author (plist-get args :author)))
      (push (format "author_username=%s" (url-hexify-string author)) params))
    (when-let* ((label (plist-get args :label)))
      (push (format "labels=%s" (url-hexify-string label)) params))
    (when (plist-get args :draft)
      (push "wip=yes" params))
    (push "per_page=100" params)
    (string-join (nreverse params) "&")))

(defun shipit-pr-gitlab--strip-github-query-syntax (query)
  "Strip GitHub-specific search syntax from QUERY, returning the plain term.
Removes repo:..., is:pr, in:title prefixes and decodes + as space."
  (let ((decoded (url-unhex-string (replace-regexp-in-string "\\+" " " query))))
    (string-trim
     (replace-regexp-in-string
      "\\b\\(?:repo:[^ ]+\\|is:pr\\|in:title\\)\\b" "" decoded))))

(defun shipit-pr-gitlab--search-raw (config query page per-page)
  "Search MRs using CONFIG with raw QUERY string at PAGE with PER-PAGE results.
QUERY may contain GitHub-flavored syntax which is stripped to extract
the plain search term.  Returns alist with `total_count' and `items'."
  (let* ((search-term (shipit-pr-gitlab--strip-github-query-syntax query))
         (project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests?search=%s&per_page=%d&page=%d&state=all"
                       project (url-hexify-string search-term) per-page page))
         (data (shipit-gitlab--api-request config path))
         (items (when (and data (or (vectorp data) (listp data)))
                  (mapcar #'shipit-pr-gitlab--normalize-mr (append data nil)))))
    `((total_count . 999)
      (items . ,items))))

(defun shipit-pr-gitlab--fetch-files (config number)
  "Fetch changed files for MR NUMBER using CONFIG."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/changes" project number))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching files for MR !%s" number)
    (when data
      (let ((changes (cdr (assq 'changes data))))
        (mapcar #'shipit-pr-gitlab--normalize-file (append changes nil))))))

(defun shipit-pr-gitlab--fetch-commits (config number)
  "Fetch commits for MR NUMBER using CONFIG."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/commits" project number))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching commits for MR !%s" number)
    (when data
      (mapcar #'shipit-pr-gitlab--normalize-commit (append data nil)))))

(defun shipit-pr-gitlab--fetch-reviews (config number)
  "Fetch approvals for MR NUMBER using CONFIG, mapped to review shape."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/approvals" project number))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching approvals for MR !%s" number)
    (when data
      (let ((approved-by (cdr (assq 'approved_by data))))
        (mapcar (lambda (entry)
                  (let ((user (cdr (assq 'user entry))))
                    `((user . ((login . ,(cdr (assq 'username user)))))
                      (state . "APPROVED"))))
                (append approved-by nil))))))

(defun shipit-pr-gitlab--fetch-review-decision (config number)
  "Fetch review decision for MR NUMBER using CONFIG.
Returns alist with `status-text', `completed-reviews', `pending-users',
`pending-teams', and `latest-reviews' keys."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/approvals" project number))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching review decision for MR !%s" number)
    (when data
      (let* ((approved (eq (cdr (assq 'approved data)) t))
             (approvals-left (cdr (assq 'approvals_left data)))
             (approved-by (append (cdr (assq 'approved_by data)) nil))
             (completed-reviews
              (mapcar (lambda (entry)
                        (let ((user (cdr (assq 'user entry))))
                          `((state . "APPROVED")
                            (user . ((login . ,(cdr (assq 'username user)))
                                     (avatar_url . ,(cdr (assq 'avatar_url user))))))))
                      approved-by))
             (approved-count (length completed-reviews))
             (status-text
              (cond
               (approved
                (format "✅ Approved (%d)" approved-count))
               ((and approvals-left (> approvals-left 0))
                (format "⏳ Review Required (%d approved, %d more needed)"
                        approved-count approvals-left))
               (t (format "📝 No Review Decision (%d)" approved-count)))))
        `((status-text . ,status-text)
          (completed-reviews . ,completed-reviews)
          (latest-reviews . ,completed-reviews)
          (pending-users . nil)
          (pending-teams . nil))))))

(defun shipit-pr-gitlab--fetch-checks (config ref)
  "Fetch CI pipeline jobs for commit REF using CONFIG."
  (let* ((project (shipit-gitlab--project-path config))
         (pipelines-path (format "/projects/%s/pipelines?sha=%s" project ref))
         (pipelines (shipit-gitlab--api-request config pipelines-path))
         (all-jobs '()))
    (shipit--debug-log "GitLab PR backend: fetching checks for %s" ref)
    ;; API may return an error alist (e.g. 403 Forbidden) instead of a vector
    (when (and pipelines (vectorp pipelines))
      (dolist (pipeline (append pipelines nil))
        (let* ((pipeline-id (cdr (assq 'id pipeline)))
               (jobs-path (format "/projects/%s/pipelines/%s/jobs" project pipeline-id))
               (jobs (shipit-gitlab--api-request config jobs-path)))
          (when (and jobs (vectorp jobs))
            (dolist (job (append jobs nil))
              (push (shipit-pr-gitlab--normalize-pipeline-job job) all-jobs))))))
    (nreverse all-jobs)))

(defun shipit-pr-gitlab--browse-url (config number)
  "Return browser URL for MR NUMBER using CONFIG."
  (let* ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
         (project-path (plist-get config :project-path)))
    (format "%s/%s/-/merge_requests/%s"
            (string-trim-right api-url "/")
            project-path
            number)))

(defun shipit-pr-gitlab--browse-issue-url (config number)
  "Return browser URL for issue NUMBER using CONFIG."
  (let* ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
         (project-path (plist-get config :project-path)))
    (format "%s/%s/-/issues/%s"
            (string-trim-right api-url "/") project-path number)))

(defun shipit-pr-gitlab--browse-commit-url (config sha)
  "Return browser URL for commit SHA using CONFIG."
  (let* ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
         (project-path (plist-get config :project-path)))
    (format "%s/%s/-/commit/%s"
            (string-trim-right api-url "/") project-path sha)))

(defun shipit-pr-gitlab--browse-user-url (config username)
  "Return browser URL for USERNAME's GitLab profile."
  (let ((api-url (or (plist-get config :api-url) "https://gitlab.com")))
    (format "%s/%s" (string-trim-right api-url "/") username)))

(defun shipit-pr-gitlab--get-current-username (config)
  "Return the authenticated GitLab username using CONFIG."
  (let ((user-data (shipit-gitlab--api-request config "/user")))
    (cdr (assq 'username user-data))))

(defun shipit-pr-gitlab--extract-username-from-email (_config email)
  "Extract GitLab username from noreply EMAIL, or nil."
  (when (string-match "^[0-9]+-\\([^@]+\\)@users\\.noreply\\." email)
    (match-string 1 email)))

(defun shipit-pr-gitlab--create-pr (config title body base head)
  "Create a new MR in project specified by CONFIG.
TITLE, BODY (description), BASE (target branch), HEAD (source branch)."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests" project))
         (data `((source_branch . ,head)
                 (target_branch . ,base)
                 (title . ,title)
                 (description . ,body)))
         (result (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab PR backend: creating MR in project %s" project)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--merge-pr (config number method)
  "Merge MR NUMBER using CONFIG with merge METHOD.
METHOD maps: \"squash\" sets squash=true, others use default merge."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/merge" project number))
         (data (when (string= method "squash")
                 '((squash . t)))))
    (shipit--debug-log "GitLab PR backend: merging MR !%s via %s" number method)
    (shipit-gitlab--api-request-method config path data "PUT")))

(defun shipit-pr-gitlab--update-pr (config number data)
  "Update MR NUMBER using CONFIG with DATA alist.
Translates common shipit field names to GitLab equivalents."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (gitlab-data (shipit-pr-gitlab--translate-update-data data)))
    (shipit--debug-log "GitLab PR backend: updating MR !%s" number)
    (let ((result (shipit-gitlab--api-request-method config path gitlab-data "PUT")))
      (when result
        (shipit-pr-gitlab--normalize-mr result)))))

(defun shipit-pr-gitlab--translate-update-data (data)
  "Translate common shipit update DATA to GitLab field names."
  (let ((result '()))
    (dolist (pair data)
      (let ((key (car pair))
            (val (cdr pair)))
        (pcase key
          ('body (push `(description . ,val) result))
          ('state (push `(state_event . ,(if (string= val "closed") "close" "reopen"))
                        result))
          ('title (push `(title . ,val) result))
          (_ (push pair result)))))
    (nreverse result)))

(defun shipit-pr-gitlab--submit-review (config number event body _comments)
  "Submit a review for MR NUMBER using CONFIG.
EVENT: \"APPROVE\" posts approval, others post a note with BODY."
  (let ((project (shipit-gitlab--project-path config)))
    (shipit--debug-log "GitLab PR backend: submitting review for MR !%s event=%s" number event)
    (if (string= event "APPROVE")
        (let ((path (format "/projects/%s/merge_requests/%s/approve" project number)))
          (shipit-gitlab--api-request-method config path nil "POST"))
      ;; COMMENT or REQUEST_CHANGES → post a note
      (let ((path (format "/projects/%s/merge_requests/%s/notes" project number))
            (data `((body . ,(or body "")))))
        (shipit-gitlab--api-request-method config path data "POST")))))

;;; Optional operations

(defun shipit-pr-gitlab--search-repos (config query)
  "Search GitLab projects matching QUERY.
Returns list of alists with `full_name' and `description' keys.
Tries with `search_namespaces=true' first; falls back to plain
search if the server returns an error (some endpoints require auth
for namespace search)."
  (let* ((encoded (url-hexify-string query))
         (data (or (shipit-pr-gitlab--search-repos-try
                    config (format "/projects?search=%s&search_namespaces=true&per_page=100" encoded))
                   (shipit-pr-gitlab--search-repos-try
                    config (format "/projects?search=%s&per_page=100" encoded)))))
    (shipit--debug-log "GitLab project search for %S: %d results"
                       query (length data))
    data))

(defun shipit-pr-gitlab--search-repos-try (config path)
  "Try a GitLab project search at PATH.
Returns list of alists with `full_name' and `description' keys, or
nil when the server returns an error object instead of an array."
  (let ((data (shipit-gitlab--api-request config path)))
    (cond
     ((vectorp data)
      (mapcar (lambda (project)
                `((full_name . ,(cdr (assq 'path_with_namespace project)))
                  (description . ,(cdr (assq 'description project)))))
              (append data nil)))
     (data
      (shipit--debug-log "GitLab project search error: %S" data)
      nil)
     (t nil))))

(defun shipit-pr-gitlab--search-repos-request (config query)
  "Return (URL HEADERS NORMALIZER) for a GitLab project search.
Used by `shipit--read-repo' to search via curl.
When QUERY contains a slash, uses the Groups API to search within
a specific group/namespace (with subgroups).  Otherwise uses the
global projects search with `order_by=similarity'."
  (let* ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
         (base (string-trim-right api-url "/"))
         (slash-pos (string-match "/" query))
         (url (if slash-pos
                  ;; "group/project" → search within group via Groups API
                  (let ((group (url-hexify-string (substring query 0 slash-pos)))
                        (project (substring query (1+ slash-pos))))
                    (format "%s/api/v4/groups/%s/projects?search=%s&include_subgroups=true&simple=true&per_page=100"
                            base group (url-hexify-string project)))
                ;; Plain query → global projects search
                (format "%s/api/v4/projects?search=%s&order_by=similarity&simple=true&per_page=100"
                        base (url-hexify-string query))))
         (auth (shipit-gitlab--auth-header config))
         (headers (when auth (list auth)))
         (normalizer
          (lambda (data)
            (when (vectorp data)
              (mapcar (lambda (project)
                        `((full_name . ,(cdr (assq 'path_with_namespace project)))
                          (description . ,(cdr (assq 'description project)))))
                      (append data nil))))))
    (list url headers normalizer)))

;;; Timeline — resource state events + system notes → shipit timeline events

(defun shipit-pr-gitlab--fetch-timeline (config number)
  "Fetch timeline events for MR NUMBER using CONFIG.
Combines resource state events, system notes, and user comments,
sorted by created_at."
  (let* ((project (shipit-gitlab--project-path config))
         (state-events (shipit-pr-gitlab--fetch-state-events config project number))
         (parsed-notes (shipit-pr-gitlab--fetch-all-notes config project number))
         (system-notes (car parsed-notes))
         (user-comments (cdr parsed-notes))
         (all-events (append state-events system-notes user-comments)))
    (shipit--debug-log "GitLab PR backend: timeline for MR !%s: %d state + %d system + %d comments"
                       number (length state-events) (length system-notes) (length user-comments))
    (sort all-events (lambda (a b)
                       (string< (or (cdr (assq 'created_at a)) "")
                                (or (cdr (assq 'created_at b)) ""))))))

(defun shipit-pr-gitlab--fetch-state-events (config project number)
  "Fetch resource state events for MR NUMBER in PROJECT using CONFIG.
Returns list of shipit timeline events."
  (let* ((path (format "/projects/%s/merge_requests/%s/resource_state_events"
                       project number))
         (raw (shipit-gitlab--api-request config path)))
    (when (and raw (or (vectorp raw) (listp raw)))
      (delq nil (mapcar #'shipit-pr-gitlab--normalize-state-event
                        (append raw nil))))))

(defun shipit-pr-gitlab--normalize-state-event (event)
  "Normalize a GitLab resource state EVENT to common shipit timeline shape."
  (let* ((state (cdr (assq 'state event)))
         (event-type (pcase state
                       ("closed" "closed")
                       ("reopened" "reopened")
                       ("merged" "merged")
                       (_ nil)))
         (user (cdr (assq 'user event))))
    (when event-type
      `((event . ,event-type)
        (actor . ((login . ,(cdr (assq 'username user)))
                  (avatar_url . ,(cdr (assq 'avatar_url user)))))
        (created_at . ,(cdr (assq 'created_at event)))
        (id . ,(cdr (assq 'id event)))))))

(defun shipit-pr-gitlab--classify-notes (notes)
  "Classify NOTES into system events and user comments.
NOTES is a list of GitLab note alists.
Returns (SYSTEM-EVENTS . USER-COMMENTS) where SYSTEM-EVENTS are
parsed system notes and USER-COMMENTS are normalized comment events."
  (let ((system-events '())
        (user-comments '()))
    (dolist (note notes)
      (let ((sys (cdr (assq 'system note))))
        (if (and sys (not (eq sys :json-false)))
            ;; System note → parse into typed event(s)
            (let ((parsed (shipit-pr-gitlab--normalize-system-note note)))
              (when parsed
                ;; committed notes return a list of events; others a single alist
                ;; List of events: (((...) (...)) (...)), first car is itself an alist
                ;; Single event: ((...) (...)), first car is a cons pair (key . val)
                (if (and (consp (car parsed)) (consp (caar parsed)))
                    (dolist (evt parsed)
                      (push evt system-events))
                  (push parsed system-events))))
          ;; User comment → commented event
          (push (shipit-pr-gitlab--normalize-user-comment note) user-comments))))
    (cons (nreverse system-events) (nreverse user-comments))))

(defun shipit-pr-gitlab--fetch-all-notes (config project number)
  "Fetch all notes for MR NUMBER in PROJECT using CONFIG.
Returns (SYSTEM-EVENTS . USER-COMMENTS) where SYSTEM-EVENTS are
parsed system notes and USER-COMMENTS are normalized comment events."
  (let* ((path (format "/projects/%s/merge_requests/%s/notes?sort=asc"
                       project number))
         (raw (shipit-gitlab--api-request config path))
         (notes (append raw nil)))
    (shipit-pr-gitlab--classify-notes notes)))

(defun shipit-pr-gitlab--normalize-system-note (note)
  "Normalize a GitLab system NOTE to common shipit timeline shape.
Parses the note body to determine event type.
Returns a single event alist, or a list of events for committed notes."
  (let* ((body (or (cdr (assq 'body note)) ""))
         (author (cdr (assq 'author note)))
         (parsed (shipit-pr-gitlab--parse-system-note-body body))
         (actor `((login . ,(cdr (assq 'username author)))
                  (avatar_url . ,(cdr (assq 'avatar_url author)))))
         (created-at (cdr (assq 'created_at note)))
         (note-id (cdr (assq 'id note))))
    (when parsed
      (if (string= "committed" (car parsed))
          ;; Committed: expand into one event per parsed commit
          (let ((commits (cdr (assq 'commits (cdr parsed)))))
            (if commits
                (mapcar (lambda (commit)
                          `((event . "committed")
                            (sha . ,(cdr (assq 'sha commit)))
                            (message . ,(cdr (assq 'message commit)))
                            (actor . ,actor)
                            (created_at . ,created-at)
                            (id . ,note-id)))
                        commits)
              ;; No parseable commits — single summary event
              (list `((event . "committed")
                      (message . ,body)
                      (actor . ,actor)
                      (created_at . ,created-at)
                      (id . ,note-id)))))
        ;; All other event types: single event
        `((event . ,(car parsed))
          ,@(cdr parsed)
          (actor . ,actor)
          (created_at . ,created-at)
          (id . ,note-id))))))

(defun shipit-pr-gitlab--normalize-user-comment (note)
  "Normalize a GitLab user NOTE to a commented timeline event."
  (let ((author (cdr (assq 'author note))))
    `((event . "commented")
      (body . ,(cdr (assq 'body note)))
      (actor . ((login . ,(cdr (assq 'username author)))
                (avatar_url . ,(cdr (assq 'avatar_url author)))))
      (created_at . ,(cdr (assq 'created_at note)))
      (id . ,(cdr (assq 'id note))))))

(defun shipit-pr-gitlab--parse-system-note-body (body)
  "Parse system note BODY text into (EVENT-TYPE . extra-alist).
Returns nil for unrecognized note types."
  (cond
   ((string-match "\\`added ~\\(.+\\) label" body)
    `("labeled" (label . ((name . ,(match-string 1 body))))))
   ((string-match "\\`removed ~\\(.+\\) label" body)
    `("unlabeled" (label . ((name . ,(match-string 1 body))))))
   ((string-match "\\`assigned to @\\(.+\\)" body)
    `("assigned" (assignee . ((login . ,(match-string 1 body))))))
   ((string-match "\\`unassigned @\\(.+\\)" body)
    `("unassigned" (assignee . ((login . ,(match-string 1 body))))))
   ((string-match "\\`requested review from @\\(.+\\)" body)
    `("review_requested" (requested_reviewer . ((login . ,(match-string 1 body))))))
   ((string-match "\\`changed title from \\*\\*\\(.+?\\)\\*\\* to \\*\\*\\(.+?\\)\\*\\*" body)
    `("renamed" (old-title . ,(match-string 1 body))
               (new-title . ,(match-string 2 body))))
   ((string-match "\\`added \\([0-9]+\\) commit" body)
    `("committed" (commits . ,(shipit-pr-gitlab--parse-commit-lines body))))
   ((string-match "\\`changed the description" body)
    '("description_changed"))))

(defun shipit-pr-gitlab--parse-commit-lines (body)
  "Extract commit SHA/message pairs from system note BODY.
GitLab uses HTML: <li>SHA - message</li> for individual commits
and <li>SHA1...SHA2 - N commits from branch ...</li> for ranges.
Returns list of ((sha . SHA) (message . MSG)) alists."
  (let ((commits '())
        (pos 0))
    ;; Match <li>SHA - message</li> (single commit, not a range with ...)
    (while (string-match "<li>\\([0-9a-f]\\{7,40\\}\\) - \\(.+?\\)</li>" body pos)
      (let ((sha (match-string 1 body))
            (msg (match-string 2 body)))
        (push `((sha . ,sha) (message . ,msg)) commits))
      (setq pos (match-end 0)))
    (nreverse commits)))

;;; File content — Repository Files API

(defun shipit-pr-gitlab--fetch-file-content (config file-path ref)
  "Fetch FILE-PATH content at REF via GitLab Repository Files API.
CONFIG provides project and auth info.
Returns file content as a decoded string, or nil on error."
  (let* ((project (shipit-gitlab--project-path config))
         (encoded-path (url-hexify-string file-path))
         (path (format "/projects/%s/repository/files/%s?ref=%s"
                       project encoded-path ref))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching file content %s at %s" file-path ref)
    (when data
      (let ((content (cdr (assq 'content data)))
            (encoding (cdr (assq 'encoding data))))
        (when (and content (string= encoding "base64"))
          (decode-coding-string
           (base64-decode-string (replace-regexp-in-string "\n" "" content))
           'utf-8))))))

;;; Compare — merge base computation

(defun shipit-pr-gitlab--fetch-compare (config base head)
  "Fetch compare data between BASE and HEAD commits using CONFIG.
Uses GitLab's repository compare API.  Normalizes the response so
that `merge_base_commit' uses `sha' instead of GitLab's `id' key."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/repository/compare?from=%s&to=%s"
                       project base head))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching compare %s...%s" base head)
    (when data
      (let* ((mbc-raw (cdr (assq 'merge_base_commit data)))
             (mbc-id (cdr (assq 'id mbc-raw))))
        ;; Normalize: GitLab uses `id', consumers expect `sha'
        `((merge_base_commit . ((sha . ,mbc-id))))))))

(defun shipit-pr-gitlab--refspec-for-pr (_config _pr-number head-ref)
  "Return GitLab refspec for an MR.
GitLab fetches source branch by name (HEAD-REF), not by MR ref."
  head-ref)

(defun shipit-pr-gitlab--remote-for-fetch (_config _repo)
  "Return remote for fetching a GitLab MR.
Always returns \"origin\" — GitLab MRs are within the same project."
  "origin")

;;; Async backend wrappers
;; True async via url-retrieve — Emacs stays responsive while API data loads.

(defun shipit-pr-gitlab--fetch-review-decision-async (config number callback)
  "Fetch review decision for MR NUMBER using CONFIG asynchronously.
Calls CALLBACK with the review-info alist."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/approvals" project number)))
    (shipit-gitlab--api-request-async
     config path
     (lambda (data)
       (if (not data)
           (funcall callback nil)
         (let* ((approved (eq (cdr (assq 'approved data)) t))
                (approvals-left (cdr (assq 'approvals_left data)))
                (approved-by (append (cdr (assq 'approved_by data)) nil))
                (completed-reviews
                 (mapcar (lambda (entry)
                           (let ((user (cdr (assq 'user entry))))
                             `((state . "APPROVED")
                               (user . ((login . ,(cdr (assq 'username user)))
                                        (avatar_url . ,(cdr (assq 'avatar_url user))))))))
                         approved-by))
                (approved-count (length completed-reviews))
                (status-text
                 (cond
                  (approved
                   (format "✅ Approved (%d)" approved-count))
                  ((and approvals-left (> approvals-left 0))
                   (format "⏳ Review Required (%d approved, %d more needed)"
                           approved-count approvals-left))
                  (t (format "📝 No Review Decision (%d)" approved-count)))))
           (funcall callback
                    `((status-text . ,status-text)
                      (completed-reviews . ,completed-reviews)
                      (latest-reviews . ,completed-reviews)
                      (pending-users . nil)
                      (pending-teams . nil)))))))))

(defun shipit-pr-gitlab--fetch-timeline-async (config number callback)
  "Fetch timeline events for MR NUMBER using CONFIG asynchronously.
Fires state-events and notes requests concurrently, combines when both complete.
Calls CALLBACK with the sorted list of events."
  (let* ((project (shipit-gitlab--project-path config))
         (remaining (cons 2 nil))
         (state-events (cons nil nil))
         (notes-result (cons nil nil)))
    ;; Request 1: state events
    (shipit-gitlab--api-request-async
     config
     (format "/projects/%s/merge_requests/%s/resource_state_events" project number)
     (lambda (data)
       (setcar state-events
               (when (and data (or (vectorp data) (listp data)))
                 (delq nil (mapcar #'shipit-pr-gitlab--normalize-state-event
                                   (append data nil)))))
       (setcar remaining (1- (car remaining)))
       (when (= 0 (car remaining))
         (shipit-pr-gitlab--combine-timeline-results
          state-events notes-result callback number))))
    ;; Request 2: notes
    (shipit-gitlab--api-request-async
     config
     (format "/projects/%s/merge_requests/%s/notes?sort=asc" project number)
     (lambda (data)
       (setcar notes-result
               (when data
                 (shipit-pr-gitlab--classify-notes (append data nil))))
       (setcar remaining (1- (car remaining)))
       (when (= 0 (car remaining))
         (shipit-pr-gitlab--combine-timeline-results
          state-events notes-result callback number))))))

(defun shipit-pr-gitlab--combine-timeline-results (state-events notes-result callback number)
  "Combine STATE-EVENTS and NOTES-RESULT, sort and call CALLBACK.
NUMBER is the MR number for debug logging."
  (let* ((parsed-notes (car notes-result))
         (system-notes (car parsed-notes))
         (user-comments (cdr parsed-notes))
         (all-events (append (car state-events) system-notes user-comments)))
    (shipit--debug-log "GitLab PR backend: async timeline for MR !%s: %d state + %d system + %d comments"
                       number
                       (length (car state-events))
                       (length system-notes)
                       (length user-comments))
    (funcall callback
             (sort all-events (lambda (a b)
                                (string< (or (cdr (assq 'created_at a)) "")
                                         (or (cdr (assq 'created_at b)) "")))))))

(defun shipit-pr-gitlab--fetch-commits-async (config number callback)
  "Fetch commits for MR NUMBER using CONFIG asynchronously.
Calls CALLBACK with the list of normalized commits."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/commits" project number)))
    (shipit-gitlab--api-request-async
     config path
     (lambda (data)
       (funcall callback
                (when data
                  (mapcar #'shipit-pr-gitlab--normalize-commit
                          (append data nil))))))))

(defun shipit-pr-gitlab--fetch-files-async (config number callback)
  "Fetch files for MR NUMBER using CONFIG asynchronously.
Calls CALLBACK with (files . truncated-p) to match the expected format."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/changes" project number)))
    (shipit-gitlab--api-request-async
     config path
     (lambda (data)
       (if (not data)
           (funcall callback (cons nil nil))
         (let* ((changes (cdr (assq 'changes data)))
                (files (mapcar #'shipit-pr-gitlab--normalize-file
                               (append changes nil))))
           (funcall callback (cons files nil))))))))

;;; Award Emoji (Reactions)

(defun shipit-pr-gitlab--mr-award-path (config number)
  "Build API path for MR NUMBER award_emoji using CONFIG."
  (concat "/projects/" (shipit-gitlab--project-path config)
          "/merge_requests/" (format "%s" number) "/award_emoji"))

(defun shipit-pr-gitlab--fetch-reactions (config number)
  "Fetch award emojis (reactions) for MR NUMBER using CONFIG.
Returns normalized reaction alists with shipit-standard content types."
  (require 'shipit-comment-gitlab)
  (let* ((path (shipit-pr-gitlab--mr-award-path config number))
         (raw (shipit-gitlab--api-request config path))
         (emojis (append raw nil)))
    (shipit--debug-log "GitLab PR backend: fetched %d reactions for MR #%s"
                       (length emojis) number)
    (mapcar #'shipit-comment-gitlab--normalize-reaction emojis)))

(defun shipit-pr-gitlab--add-reaction (config number reaction)
  "Add REACTION to MR NUMBER using CONFIG.
REACTION is a shipit-standard content string (e.g. \"+1\").
Returns the normalized reaction alist."
  (require 'shipit-comment-gitlab)
  (let* ((emoji-name (or (car (rassoc reaction shipit-comment-gitlab--emoji-to-content))
                         reaction))
         (path (shipit-pr-gitlab--mr-award-path config number))
         (data `((name . ,emoji-name)))
         (raw (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab PR backend: added reaction %s to MR #%s" emoji-name number)
    (when raw
      (shipit-comment-gitlab--normalize-reaction raw))))

(defun shipit-pr-gitlab--delete-reaction (config number reaction-id)
  "Delete reaction REACTION-ID from MR NUMBER using CONFIG.
Returns t on success."
  (let ((path (concat (shipit-pr-gitlab--mr-award-path config number)
                      (format "/%s" reaction-id))))
    (shipit--debug-log "GitLab PR backend: deleting reaction %s from MR #%s" reaction-id number)
    (shipit-gitlab--api-request-method config path nil "DELETE")
    t))

;;; Parity operations — Labels

(defun shipit-pr-gitlab--add-label (config number label-name)
  "Add LABEL-NAME to MR NUMBER using CONFIG.
Fetches current labels, appends the new one, PUTs the full list.
Returns normalized MR."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current (append (cdr (assq 'labels mr)) nil))
         (new-labels (delete-dups (append current (list label-name))))
         (data `((labels . ,(mapconcat #'identity new-labels ","))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: adding label %s to MR !%s" label-name number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--remove-label (config number label-name)
  "Remove LABEL-NAME from MR NUMBER using CONFIG.
Fetches current labels, removes the one, PUTs the filtered list.
Returns normalized MR."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current (append (cdr (assq 'labels mr)) nil))
         (new-labels (delete label-name current))
         (data `((labels . ,(mapconcat #'identity new-labels ","))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: removing label %s from MR !%s" label-name number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--fetch-labels (config number)
  "Fetch labels for MR NUMBER using CONFIG.
Returns minimal PR structure with labels list."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching labels for MR !%s" number)
    (when mr
      (let ((normalized (shipit-pr-gitlab--normalize-mr mr)))
        `((number . ,(cdr (assq 'number normalized)))
          (labels . ,(cdr (assq 'labels normalized)))
          (title . ,(cdr (assq 'title normalized)))
          (state . ,(cdr (assq 'state normalized))))))))

(defun shipit-pr-gitlab--set-labels (config number label-names)
  "Set LABEL-NAMES (a list) on MR NUMBER using CONFIG.
Replaces all existing labels.  Returns normalized MR."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (data `((labels . ,(mapconcat #'identity label-names ","))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: setting %d labels on MR !%s"
                       (length label-names) number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--fetch-available-labels (config)
  "Fetch all labels for the project in CONFIG with pagination."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/labels?per_page=100" project)))
    (shipit--debug-log "GitLab PR backend: fetching available labels")
    (shipit-gitlab--api-request-paginated config path)))

;;; Parity operations — Reviewers

(defun shipit-pr-gitlab--resolve-user-id (config username)
  "Resolve USERNAME to a GitLab user ID using CONFIG.
Searches project members for the given username."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/members/all?query=%s&per_page=10"
                       project (url-hexify-string username)))
         (members (shipit-gitlab--api-request config path)))
    (when members
      (let ((match (cl-find-if
                    (lambda (m) (string= (cdr (assq 'username m)) username))
                    (append members nil))))
        (when match
          (cdr (assq 'id match)))))))

(defun shipit-pr-gitlab--add-reviewer (config number username)
  "Add USERNAME as reviewer on MR NUMBER using CONFIG.
Resolves username to user ID, fetches current reviewers, appends.
Returns normalized MR."
  (let* ((user-id (shipit-pr-gitlab--resolve-user-id config username))
         (project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current-ids (mapcar (lambda (r) (cdr (assq 'id r)))
                              (append (cdr (assq 'reviewers mr)) nil)))
         (new-ids (delete-dups (append current-ids (when user-id (list user-id)))))
         (data `((reviewer_ids . ,(vconcat new-ids))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: adding reviewer %s (id=%s) to MR !%s"
                       username user-id number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--remove-reviewer (config number username)
  "Remove USERNAME from reviewers on MR NUMBER using CONFIG.
Resolves username to user ID, filters current list.
Returns normalized MR."
  (let* ((user-id (shipit-pr-gitlab--resolve-user-id config username))
         (project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current-ids (mapcar (lambda (r) (cdr (assq 'id r)))
                              (append (cdr (assq 'reviewers mr)) nil)))
         (new-ids (cl-remove user-id current-ids))
         (data `((reviewer_ids . ,(vconcat new-ids))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: removing reviewer %s (id=%s) from MR !%s"
                       username user-id number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--add-reviewers-batch (config number usernames)
  "Add USERNAMES (a list) as reviewers on MR NUMBER using CONFIG.
Resolves usernames, merges with current reviewers.
Returns normalized MR."
  (let* ((user-ids (delq nil (mapcar (lambda (u)
                                       (shipit-pr-gitlab--resolve-user-id config u))
                                     usernames)))
         (project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current-ids (mapcar (lambda (r) (cdr (assq 'id r)))
                              (append (cdr (assq 'reviewers mr)) nil)))
         (new-ids (delete-dups (append current-ids user-ids)))
         (data `((reviewer_ids . ,(vconcat new-ids))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: adding %d reviewers to MR !%s"
                       (length usernames) number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--fetch-requested-reviewers (config number)
  "Fetch requested reviewers for MR NUMBER using CONFIG.
Returns alist with `users' key containing reviewer lists."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/approval_state" project number))
         (data (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching requested reviewers for MR !%s" number)
    (when data
      (let* ((rules (append (cdr (assq 'rules data)) nil))
             (eligible (cl-mapcan
                        (lambda (rule)
                          (mapcar (lambda (u)
                                    `((login . ,(cdr (assq 'username u)))))
                                  (append (cdr (assq 'eligible_approvers rule)) nil)))
                        rules)))
        `((users . ,eligible))))))

;;; Parity operations — Assignees

(defun shipit-pr-gitlab--add-assignee (config number username)
  "Add USERNAME as assignee on MR NUMBER using CONFIG.
Resolves username to user ID, merges with current assignees.
Returns normalized MR."
  (let* ((user-id (shipit-pr-gitlab--resolve-user-id config username))
         (project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current-ids (mapcar (lambda (a) (cdr (assq 'id a)))
                              (append (cdr (assq 'assignees mr)) nil)))
         (new-ids (delete-dups (append current-ids (when user-id (list user-id)))))
         (data `((assignee_ids . ,(vconcat new-ids))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: adding assignee %s (id=%s) to MR !%s"
                       username user-id number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--remove-assignee (config number username)
  "Remove USERNAME from assignees on MR NUMBER using CONFIG.
Resolves username to user ID, filters current list.
Returns normalized MR."
  (let* ((user-id (shipit-pr-gitlab--resolve-user-id config username))
         (project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current-ids (mapcar (lambda (a) (cdr (assq 'id a)))
                              (append (cdr (assq 'assignees mr)) nil)))
         (new-ids (cl-remove user-id current-ids))
         (data `((assignee_ids . ,(vconcat new-ids))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: removing assignee %s (id=%s) from MR !%s"
                       username user-id number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--add-assignees-batch (config number usernames)
  "Add USERNAMES (a list) as assignees on MR NUMBER using CONFIG.
Resolves usernames, merges with current assignees.
Returns normalized MR."
  (let* ((user-ids (delq nil (mapcar (lambda (u)
                                       (shipit-pr-gitlab--resolve-user-id config u))
                                     usernames)))
         (project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path))
         (current-ids (mapcar (lambda (a) (cdr (assq 'id a)))
                              (append (cdr (assq 'assignees mr)) nil)))
         (new-ids (delete-dups (append current-ids user-ids)))
         (data `((assignee_ids . ,(vconcat new-ids))))
         (result (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab PR backend: adding %d assignees to MR !%s"
                       (length usernames) number)
    (when result
      (shipit-pr-gitlab--normalize-mr result))))

(defun shipit-pr-gitlab--fetch-pr-assignees (config number)
  "Fetch assignees for MR NUMBER using CONFIG.
Returns normalized assignee list."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s" project number))
         (mr (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab PR backend: fetching assignees for MR !%s" number)
    (when mr
      (shipit-pr-gitlab--normalize-assignees (cdr (assq 'assignees mr))))))

(defun shipit-pr-gitlab--fetch-available-assignees (config)
  "Fetch project members eligible for assignment using CONFIG."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/members/all?per_page=100" project))
         (members (shipit-gitlab--api-request-paginated config path)))
    (shipit--debug-log "GitLab PR backend: fetching available assignees")
    (mapcar (lambda (m)
              `((login . ,(cdr (assq 'username m)))
                (avatar_url . ,(cdr (assq 'avatar_url m)))))
            (or members '()))))

;;; Parity operations — Dismiss review & Fetch commit

(defun shipit-pr-gitlab--dismiss-review (config number _message)
  "Dismiss (unapprove) the current user's approval on MR NUMBER.
MESSAGE is ignored — GitLab unapprove has no message parameter.
Returns the API result."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%s/unapprove" project number)))
    (shipit--debug-log "GitLab PR backend: unapproving MR !%s" number)
    (shipit-gitlab--api-request-method config path nil "POST")))

(defun shipit-pr-gitlab--fetch-commit (config sha)
  "Fetch commit data for SHA using CONFIG.
Returns commit alist with sha, commit, files, and html_url."
  (let* ((project (shipit-gitlab--project-path config))
         (commit-path (format "/projects/%s/repository/commits/%s" project sha))
         (diff-path (format "/projects/%s/repository/commits/%s/diff" project sha))
         (commit-data (shipit-gitlab--api-request config commit-path))
         (diffs-raw (shipit-gitlab--api-request config diff-path))
         (diffs (append diffs-raw nil)))
    (shipit--debug-log "GitLab PR backend: fetching commit %s" sha)
    (when commit-data
      `((sha . ,(cdr (assq 'id commit-data)))
        (commit . ((message . ,(cdr (assq 'message commit-data)))
                   (author . ((name . ,(cdr (assq 'author_name commit-data)))
                              (email . ,(cdr (assq 'author_email commit-data)))
                              (date . ,(cdr (assq 'created_at commit-data)))))))
        (html_url . ,(cdr (assq 'web_url commit-data)))
        (files . ,(mapcar #'shipit-pr-gitlab--normalize-file diffs))))))

;;; Review threads — discussions with position data

(defun shipit-pr-gitlab--fetch-review-threads (config number)
  "Fetch review threads for MR NUMBER using CONFIG.
Returns list of normalized thread objects."
  (let* ((project-path (shipit-gitlab--project-path config))
         (path (format "/projects/%s/merge_requests/%d/discussions"
                       (url-hexify-string project-path) number))
         (discussions (shipit-gitlab--api-request config path))
         (threads nil))
    (shipit--debug-log "GitLab PR backend: fetching review threads for MR !%s" number)
    (when (vectorp discussions)
      (setq discussions (append discussions nil)))
    (dolist (disc discussions)
      (let* ((notes (cdr (assq 'notes disc)))
             (notes-list (if (vectorp notes) (append notes nil) notes))
             (first-note (car notes-list))
             (position (cdr (assq 'position first-note))))
        ;; Only include discussions that have position data (diff comments)
        (when position
          (let* ((disc-id (cdr (assq 'id disc)))
                 (new-path (cdr (assq 'new_path position)))
                 (new-line (cdr (assq 'new_line position)))
                 (old-line (cdr (assq 'old_line position)))
                 ;; A discussion is resolved when all resolvable notes are resolved
                 (is-resolved (cl-every
                               (lambda (n)
                                 (let ((resolvable (cdr (assq 'resolvable n)))
                                       (resolved (cdr (assq 'resolved n))))
                                   (or (not (eq resolvable t))
                                       (eq resolved t))))
                               notes-list))
                 ;; Build comment nodes in the shape the consumer expects
                 (comment-nodes
                  (mapcar (lambda (n)
                            `((databaseId . ,(cdr (assq 'id n)))
                              (body . ,(cdr (assq 'body n)))
                              (author . ((login . ,(cdr (assq 'username
                                                              (cdr (assq 'author n)))))))
                              (createdAt . ,(cdr (assq 'created_at n)))))
                          notes-list)))
            (push `((id . ,disc-id)
                    (isResolved . ,(if is-resolved t nil))
                    (path . ,new-path)
                    (line . ,(or new-line old-line))
                    (originalLine . ,old-line)
                    (comments . ((nodes . ,comment-nodes))))
                  threads)))))
    (nreverse threads)))

(defun shipit-pr-gitlab--fetch-resolved-threads (config number)
  "Fetch resolved threads for MR NUMBER using CONFIG.
Returns list of resolved thread objects."
  (let ((all-threads (shipit-pr-gitlab--fetch-review-threads config number)))
    (cl-remove-if-not (lambda (thread)
                        (eq t (cdr (assq 'isResolved thread))))
                      all-threads)))

;;; Branch protection

(defun shipit-pr-gitlab--fetch-branch-protection (config base-ref)
  "Fetch branch protection rules for BASE-REF using CONFIG.
Returns alist matching the shipit protection shape.
Uses GitLab approval_rules API to determine required approvals
and protected_branches API for access-level info."
  (let ((project (shipit-gitlab--project-path config)))
    (shipit--debug-log "GitLab PR backend: fetching branch protection for %s" base-ref)
    (condition-case err
        (let* ((rules-path (format "/projects/%s/approval_rules" project))
               (rules-raw (shipit-gitlab--api-request config rules-path)))
          (if (not rules-raw)
              ;; API returned nil — inaccessible or no rules configured
              (progn
                (shipit--debug-log "GitLab branch protection: no data for %s" base-ref)
                (list (cons 'required-approving-review-count nil)
                      (cons 'require-code-owner-reviews nil)
                      (cons 'dismiss-stale-reviews nil)
                      (cons 'api-accessible nil)))
            (let* ((rules (when (vectorp rules-raw) (append rules-raw nil)))
                   (max-approvals (shipit-pr-gitlab--max-approvals-required rules))
                   (branch-path (format "/projects/%s/protected_branches/%s"
                                        project (url-hexify-string base-ref)))
                   (branch-data (shipit-gitlab--api-request config branch-path))
                   (code-owner-approval
                    (when branch-data
                      (shipit-pr-gitlab--truthy-p
                       (cdr (assq 'code_owner_approval_required branch-data))))))
              (list (cons 'required-approving-review-count (or max-approvals 0))
                    (cons 'require-code-owner-reviews (eq code-owner-approval t))
                    (cons 'dismiss-stale-reviews nil)
                    (cons 'api-accessible t)))))
      (error
       (shipit--debug-log "GitLab branch protection API error: %S" err)
       (list (cons 'required-approving-review-count nil)
             (cons 'require-code-owner-reviews nil)
             (cons 'dismiss-stale-reviews nil)
             (cons 'api-accessible nil))))))

(defun shipit-pr-gitlab--max-approvals-required (rules)
  "Return the maximum approvals_required from RULES list.
RULES is a list of approval rule alists.  Returns nil if RULES is empty."
  (when rules
    (let ((max-val 0))
      (dolist (rule rules)
        (let ((count (cdr (assq 'approvals_required rule))))
          (when (and count (numberp count) (> count max-val))
            (setq max-val count))))
      max-val)))

;;; CODEOWNERS

(defun shipit-pr-gitlab--fetch-codeowners (config base-ref)
  "Fetch CODEOWNERS file content for BASE-REF using CONFIG.
Tries CODEOWNERS, .gitlab/CODEOWNERS, docs/CODEOWNERS.
Returns decoded content string or nil if not found."
  (let* ((project (shipit-gitlab--project-path config))
         (paths '("CODEOWNERS" ".gitlab/CODEOWNERS" "docs/CODEOWNERS")))
    (shipit--debug-log "GitLab PR backend: fetching CODEOWNERS for %s" base-ref)
    (catch 'found
      (dolist (file-path paths)
        (let* ((encoded-path (replace-regexp-in-string "/" "%2F" file-path))
               (api-path (format "/projects/%s/repository/files/%s?ref=%s"
                                 project encoded-path base-ref))
               (data (shipit-gitlab--api-request config api-path)))
          (when-let* ((content (cdr (assq 'content data))))
            (shipit--debug-log "GitLab CODEOWNERS found at %s" file-path)
            (throw 'found (base64-decode-string content)))))
      nil)))

;;; Check suites — pipeline stages as check suites

(defun shipit-pr-gitlab--fetch-check-suites (config ref page per-page)
  "Fetch check suites for commit REF using CONFIG.
PAGE and PER-PAGE are accepted for API compatibility; GitLab does not
paginate stages so page 2+ returns empty results.
Groups pipeline jobs by stage, returning one suite per stage."
  (if (> page 1)
      ;; GitLab stages are not paginated; page 2+ is always empty
      '((check_suites) (total_count . 0))
    (let* ((project (shipit-gitlab--project-path config))
           (pipelines-path (format "/projects/%s/pipelines?sha=%s&per_page=1"
                                   project ref))
           (pipelines (shipit-gitlab--api-request config pipelines-path)))
      (shipit--debug-log "GitLab PR backend: fetching check suites for %s" ref)
      (if (not (and pipelines (vectorp pipelines) (> (length pipelines) 0)))
          '((check_suites) (total_count . 0))
        (let* ((pipeline (aref pipelines 0))
               (pipeline-id (cdr (assq 'id pipeline)))
               (jobs-path (format "/projects/%s/pipelines/%s/jobs?per_page=%d"
                                  project pipeline-id per-page))
               (jobs-raw (shipit-gitlab--api-request config jobs-path))
               (jobs (when (and jobs-raw (vectorp jobs-raw))
                       (append jobs-raw nil)))
               (suites (shipit-pr-gitlab--group-jobs-into-suites jobs pipeline-id)))
          `((check_suites . ,suites)
            (total_count . ,(length suites))))))))

(defun shipit-pr-gitlab--group-jobs-into-suites (jobs pipeline-id)
  "Group JOBS by stage field into check suite alists.
PIPELINE-ID is used to build composite suite IDs."
  (let ((stage-table (make-hash-table :test 'equal)))
    ;; Group jobs by stage
    (dolist (job jobs)
      (let* ((stage (or (cdr (assq 'stage job)) "Pipeline"))
             (existing (gethash stage stage-table)))
        (puthash stage (cons job existing) stage-table)))
    ;; Build one suite per stage
    (let ((suites '()))
      (maphash
       (lambda (stage-name stage-jobs)
         (let* ((mapped-statuses (mapcar (lambda (j)
                                           (shipit-pr-gitlab--map-job-status
                                            (cdr (assq 'status j))))
                                         stage-jobs))
                (aggregate (shipit-pr-gitlab--aggregate-stage-status mapped-statuses))
                (suite `((id . ,(format "%s:%s" stage-name pipeline-id))
                         (name . ,stage-name)
                         (status . ,(car aggregate))
                         (conclusion . ,(cdr aggregate))
                         (check_runs_count . ,(length stage-jobs)))))
           (push suite suites)))
       stage-table)
      (nreverse suites))))

(defun shipit-pr-gitlab--aggregate-stage-status (mapped-statuses)
  "Aggregate MAPPED-STATUSES into a single (status . conclusion) pair.
MAPPED-STATUSES is a list of (status . conclusion) cons cells.
Worst status wins: in_progress > queued > completed.
Worst conclusion wins: failure > cancelled > skipped > success."
  (let ((has-in-progress nil)
        (has-queued nil)
        (worst-conclusion nil))
    (dolist (pair mapped-statuses)
      (let ((status (car pair))
            (conclusion (cdr pair)))
        (cond
         ((string= status "in_progress") (setq has-in-progress t))
         ((string= status "queued") (setq has-queued t)))
        (when conclusion
          (setq worst-conclusion
                (shipit-pr-gitlab--worse-conclusion worst-conclusion conclusion)))))
    (cond
     (has-in-progress (cons "in_progress" nil))
     (has-queued (cons "queued" nil))
     (t (cons "completed" worst-conclusion)))))

(defun shipit-pr-gitlab--worse-conclusion (a b)
  "Return the worse of conclusions A and B.
Priority: failure > cancelled > action_required > skipped > success."
  (let ((priority '(("failure" . 5)
                    ("cancelled" . 4)
                    ("action_required" . 3)
                    ("skipped" . 2)
                    ("success" . 1))))
    (if (not a) b
      (if (not b) a
        (if (>= (or (cdr (assoc a priority)) 0)
                (or (cdr (assoc b priority)) 0))
            a b)))))

(defun shipit-pr-gitlab--fetch-suite-check-runs (config suite-id)
  "Fetch check runs for check suite SUITE-ID using CONFIG.
SUITE-ID has the format \"STAGE-NAME:PIPELINE-ID\".
Returns jobs in that stage as check runs."
  (let* ((parts (split-string suite-id ":"))
         (stage-name (car parts))
         (pipeline-id (cadr parts))
         (project (shipit-gitlab--project-path config))
         (jobs-path (format "/projects/%s/pipelines/%s/jobs?per_page=100"
                            project pipeline-id))
         (jobs-raw (shipit-gitlab--api-request config jobs-path))
         (jobs (when (and jobs-raw (vectorp jobs-raw))
                 (append jobs-raw nil)))
         (stage-jobs (cl-remove-if-not
                      (lambda (j) (string= (cdr (assq 'stage j)) stage-name))
                      jobs))
         (runs (mapcar #'shipit-pr-gitlab--job-to-check-run stage-jobs)))
    (shipit--debug-log "GitLab PR backend: fetching suite check runs for %s (%d jobs)"
                       suite-id (length runs))
    `((check_runs . ,runs))))

(defun shipit-pr-gitlab--job-to-check-run (job)
  "Convert a GitLab pipeline JOB to a check run alist."
  (let* ((gitlab-status (cdr (assq 'status job)))
         (mapped (shipit-pr-gitlab--map-job-status gitlab-status)))
    `((id . ,(cdr (assq 'id job)))
      (name . ,(cdr (assq 'name job)))
      (status . ,(car mapped))
      (conclusion . ,(cdr mapped))
      (html_url . ,(cdr (assq 'web_url job)))
      (started_at . ,(cdr (assq 'started_at job)))
      (completed_at . ,(cdr (assq 'finished_at job))))))

;;; Async check operations
;; Async variants for the checks pipeline — used by shipit-checks.el
;; to avoid calling shipit--api-request (GitHub HTTP) directly.

(defun shipit-pr-gitlab--fetch-check-suites-async (config ref page per-page callback)
  "Fetch check suites for commit REF using CONFIG asynchronously.
PAGE and PER-PAGE control pagination.  Calls CALLBACK with parsed data.
GitLab stages are not paginated; page 2+ returns empty results."
  (if (> page 1)
      (funcall callback '((check_suites) (total_count . 0)))
    (let* ((project (shipit-gitlab--project-path config))
           (pipelines-path (format "/projects/%s/pipelines?sha=%s&per_page=1"
                                   project ref)))
      (shipit--debug-log "GitLab PR backend: async fetch check suites for %s" ref)
      (shipit-gitlab--api-request-async
       config pipelines-path
       (lambda (pipelines)
         (if (not (and pipelines (vectorp pipelines) (> (length pipelines) 0)))
             (funcall callback '((check_suites) (total_count . 0)))
           (let* ((pipeline (aref pipelines 0))
                  (pipeline-id (cdr (assq 'id pipeline)))
                  (jobs-path (format "/projects/%s/pipelines/%s/jobs?per_page=%d"
                                     project pipeline-id per-page)))
             (shipit-gitlab--api-request-async
              config jobs-path
              (lambda (jobs-raw)
                (let* ((jobs (when (and jobs-raw (vectorp jobs-raw))
                               (append jobs-raw nil)))
                       (suites (shipit-pr-gitlab--group-jobs-into-suites jobs pipeline-id)))
                  (funcall callback
                           `((check_suites . ,suites)
                             (total_count . ,(length suites))))))))))))))

(defun shipit-pr-gitlab--fetch-suite-check-runs-async (config suite-id callback)
  "Fetch check runs for check suite SUITE-ID using CONFIG asynchronously.
SUITE-ID has format \"STAGE-NAME:PIPELINE-ID\".  Calls CALLBACK with data."
  (let* ((parts (split-string suite-id ":"))
         (stage-name (car parts))
         (pipeline-id (cadr parts))
         (project (shipit-gitlab--project-path config))
         (jobs-path (format "/projects/%s/pipelines/%s/jobs?per_page=100"
                            project pipeline-id)))
    (shipit--debug-log "GitLab PR backend: async fetch suite check runs for %s" suite-id)
    (shipit-gitlab--api-request-async
     config jobs-path
     (lambda (jobs-raw)
       (let* ((jobs (when (and jobs-raw (vectorp jobs-raw))
                      (append jobs-raw nil)))
              (stage-jobs (cl-remove-if-not
                           (lambda (j) (string= (cdr (assq 'stage j)) stage-name))
                           jobs))
              (runs (mapcar #'shipit-pr-gitlab--job-to-check-run stage-jobs)))
         (funcall callback `((check_runs . ,runs))))))))

;;; Reference overlays

(declare-function shipit--pr-reference-action-menu "shipit-render")
(declare-function shipit--hash-reference-action-menu "shipit-render")

(defun shipit-pr-gitlab--create-reference-overlays (repo search-start search-limit inc-found inc-overlay)
  "Create overlays for GitLab MR, issue, and !NNN references.
REPO is the repository context.  SEARCH-START and SEARCH-LIMIT bound the region.
INC-FOUND and INC-OVERLAY are thunks called to increment counters.
Uses `shipit-pr--resolve-for-repo' to get the API URL for pattern matching,
so it naturally only matches its own URLs."
  (condition-case err
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (config (cdr resolved))
             (api-url (or (plist-get config :api-url) "https://gitlab.com"))
             (escaped-url (regexp-quote (string-trim-right api-url "/"))))
        ;; MR URLs: {api-url}/{project}/-/merge_requests/{number}
        (save-excursion
          (goto-char search-start)
          (while (re-search-forward
                  (concat escaped-url "/\\([^ \t\n)]+\\)/-/merge_requests/\\([0-9]+\\)")
                  search-limit t)
            (funcall inc-found)
            (let ((url-repo (match-string-no-properties 1))
                  (ref-num (match-string-no-properties 2))
                  (start (match-beginning 0))
                  (end (match-end 0))
                  (backtick-count 0))
              (save-excursion
                (beginning-of-line)
                (while (< (point) start)
                  (when (= (char-after) ?`)
                    (setq backtick-count (1+ backtick-count)))
                  (forward-char)))
              (unless (= (mod backtick-count 2) 1)
                (let* ((ref-num-int (string-to-number ref-num))
                       (url-repo-copy url-repo)
                       (ov (make-overlay start end))
                       (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap (current-local-map))
                  (funcall inc-overlay)
                  (define-key keymap (kbd "RET")
                    (lambda ()
                      (interactive)
                      (shipit--pr-reference-action-menu ref-num-int url-repo-copy 'pr)))
                  (overlay-put ov 'face 'markdown-plain-url-face)
                  (overlay-put ov 'keymap keymap)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'help-echo (format "MR !%s - RET: actions" ref-num))
                  (shipit--debug-log "Created overlay for GitLab MR URL %s/merge_requests/%s at %d-%d"
                                     url-repo ref-num start end))))))
        ;; Issue URLs: {api-url}/{project}/-/issues/{number}
        (save-excursion
          (goto-char search-start)
          (while (re-search-forward
                  (concat escaped-url "/\\([^ \t\n)]+\\)/-/issues/\\([0-9]+\\)")
                  search-limit t)
            (funcall inc-found)
            (let ((url-repo (match-string-no-properties 1))
                  (ref-num (match-string-no-properties 2))
                  (start (match-beginning 0))
                  (end (match-end 0))
                  (backtick-count 0))
              (save-excursion
                (beginning-of-line)
                (while (< (point) start)
                  (when (= (char-after) ?`)
                    (setq backtick-count (1+ backtick-count)))
                  (forward-char)))
              (unless (= (mod backtick-count 2) 1)
                (let* ((ref-num-int (string-to-number ref-num))
                       (url-repo-copy url-repo)
                       (ov (make-overlay start end))
                       (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap (current-local-map))
                  (funcall inc-overlay)
                  (define-key keymap (kbd "RET")
                    (lambda ()
                      (interactive)
                      (shipit--hash-reference-action-menu ref-num-int url-repo-copy)))
                  (overlay-put ov 'face 'markdown-plain-url-face)
                  (overlay-put ov 'keymap keymap)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'help-echo (format "Issue #%s - RET: actions" ref-num))
                  (shipit--debug-log "Created overlay for GitLab issue URL %s/issues/%s at %d-%d"
                                     url-repo ref-num start end))))))
        ;; Shorthand MR references: !NNN (only when GitLab is active backend)
        (when (eq (shipit-pr--backend-id) 'gitlab)
          (save-excursion
            (goto-char search-start)
            (while (re-search-forward "!\\([0-9]+\\)" search-limit t)
              (funcall inc-found)
              (let ((ref-num (match-string-no-properties 1))
                    (start (match-beginning 0))
                    (end (match-end 0))
                    (backtick-count 0))
                (save-excursion
                  (beginning-of-line)
                  (while (< (point) start)
                    (when (= (char-after) ?`)
                      (setq backtick-count (1+ backtick-count)))
                    (forward-char)))
                (unless (= (mod backtick-count 2) 1)
                  (let* ((ref-num-int (string-to-number ref-num))
                         (repo-copy repo)
                         (ov (make-overlay start end))
                         (keymap (make-sparse-keymap)))
                    (set-keymap-parent keymap (current-local-map))
                    (funcall inc-overlay)
                    (define-key keymap (kbd "RET")
                      (lambda ()
                        (interactive)
                        (shipit--pr-reference-action-menu ref-num-int repo-copy 'pr)))
                    (overlay-put ov 'face 'markdown-plain-url-face)
                    (overlay-put ov 'keymap keymap)
                    (overlay-put ov 'evaporate t)
                    (overlay-put ov 'help-echo (format "MR !%s - RET: actions" ref-num))
                    (shipit--debug-log "Created overlay for GitLab !%s at %d-%d"
                                       ref-num start end))))))))
    (error
     (shipit--debug-log "ERROR in shipit-pr-gitlab--create-reference-overlays: %s" err))))

;;; URL classification

(defun shipit-pr-gitlab--classify-url (url)
  "Classify URL as a GitLab resource.
Returns plist (:type TYPE :repo REPO :number N) or nil."
  (cond
   ;; MR: /group/.../-/merge_requests/N (with optional trailing path/query/fragment)
   ((string-match
     "\\`https?://[^/]+/\\(.+\\)/-/merge_requests/\\([0-9]+\\)\\(?:[/?#]\\|\\'\\)"
     url)
    (list :type 'pr
          :repo (match-string 1 url)
          :number (string-to-number (match-string 2 url))))
   ;; Issue: /group/.../-/issues/N (with optional trailing path/query/fragment)
   ((string-match
     "\\`https?://[^/]+/\\(.+\\)/-/issues/\\([0-9]+\\)\\(?:[/?#]\\|\\'\\)"
     url)
    (list :type 'issue
          :repo (match-string 1 url)
          :number (string-to-number (match-string 2 url))))
   ;; Repo: /group/project or /group/subgroup/project (no /-/ segment)
   ((and (string-match-p "gitlab" url)
         (not (string-match-p "/-/" url))
         (string-match
          "\\`https?://[^/]+/\\(.+?\\)/?\\'"
          url))
    (list :type 'repo
          :repo (match-string 1 url)))))

;;; Repo info, README, and languages

(defun shipit-pr-gitlab--fetch-repo-info (config)
  "Fetch repository metadata using CONFIG.
Normalizes GitLab fields to match the expected format."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s" project))
         (data (shipit-gitlab--api-request config path)))
    (when data
      (let* ((namespace (cdr (assq 'namespace data)))
             (owner-name (or (cdr (assq 'name namespace)) ""))
             (owner-avatar (cdr (assq 'avatar_url namespace))))
        ;; Normalize to GitHub-compatible format
        `((full_name . ,(or (cdr (assq 'path_with_namespace data)) ""))
          (description . ,(cdr (assq 'description data)))
          (owner . ((login . ,owner-name)
                    (avatar_url . ,owner-avatar)))
          (default_branch . ,(cdr (assq 'default_branch data)))
          (html_url . ,(cdr (assq 'web_url data))))))))

(defun shipit-pr-gitlab--fetch-readme (config)
  "Fetch repository README using CONFIG.
Uses the repository tree to find the README filename, then fetches raw content."
  (let* ((project (shipit-gitlab--project-path config))
         (tree-path (format "/projects/%s/repository/tree?per_page=100" project))
         (tree (shipit-gitlab--api-request config tree-path))
         (readme-entry (when tree
                         (cl-find-if
                          (lambda (entry)
                            (let ((name (cdr (assq 'name entry))))
                              (and name
                                   (string-match-p "\\`[Rr][Ee][Aa][Dd][Mm][Ee]" name)
                                   (equal (cdr (assq 'type entry)) "blob"))))
                          tree))))
    (when readme-entry
      (let* ((filename (cdr (assq 'name readme-entry)))
             (raw-path (format "/projects/%s/repository/files/%s/raw"
                               project (url-hexify-string filename)))
             (url (shipit-gitlab--build-url config raw-path))
             (auth (shipit-gitlab--auth-header config))
             (args (shipit-gitlab--curl-args url auth "GET" nil))
             (buf (generate-new-buffer " *shipit-gitlab-readme*")))
        (shipit--debug-log "GitLab API: GET %s (auth=%s, raw)" url (if auth "yes" "NO"))
        (unwind-protect
            (let ((exit-code (apply #'call-process "curl" nil buf nil args)))
              (when (eq exit-code 0)
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max)))))
          (kill-buffer buf))))))
(defun shipit-pr-gitlab--fetch-languages (config)
  "Fetch repository languages using CONFIG.
Returns alist of (LANGUAGE . PERCENTAGE)."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/languages" project)))
    (shipit-gitlab--api-request config path)))

;;; Registration

(shipit-pr-register-backend
 'gitlab
 (list :name "GitLab"
       :detect-url-pattern "gitlab"
       :inject-project-path t
       :pr-type-label "Merge Request"
       :pr-type-short-label "MR"
       :emoji-fallback "🦊"
       :icon-spec '("gitlab" "simple" . "#FC6D26")
       :icon-fallback-text "GL"
       :editor-extra-keys '(("!" . shipit-editor--trigger-mr-reference))
       :editor-reference-hints "#: Issue, !: MR"
       :fetch-pr #'shipit-pr-gitlab--fetch-pr
       :search #'shipit-pr-gitlab--search
       :search-raw #'shipit-pr-gitlab--search-raw
       :create-pr #'shipit-pr-gitlab--create-pr
       :merge-pr #'shipit-pr-gitlab--merge-pr
       :update-pr #'shipit-pr-gitlab--update-pr
       :fetch-reviews #'shipit-pr-gitlab--fetch-reviews
       :submit-review #'shipit-pr-gitlab--submit-review
       :fetch-review-decision #'shipit-pr-gitlab--fetch-review-decision
       :fetch-files #'shipit-pr-gitlab--fetch-files
       :fetch-commits #'shipit-pr-gitlab--fetch-commits
       :fetch-checks #'shipit-pr-gitlab--fetch-checks
       :fetch-check-suites #'shipit-pr-gitlab--fetch-check-suites
       :fetch-suite-check-runs #'shipit-pr-gitlab--fetch-suite-check-runs
       :fetch-compare #'shipit-pr-gitlab--fetch-compare
       :fetch-file-content #'shipit-pr-gitlab--fetch-file-content
       :browse-url #'shipit-pr-gitlab--browse-url
       :fetch-timeline #'shipit-pr-gitlab--fetch-timeline
       :fetch-review-decision-async #'shipit-pr-gitlab--fetch-review-decision-async
       :fetch-timeline-async #'shipit-pr-gitlab--fetch-timeline-async
       :fetch-commits-async #'shipit-pr-gitlab--fetch-commits-async
       :fetch-files-async #'shipit-pr-gitlab--fetch-files-async
       :fetch-reactions #'shipit-pr-gitlab--fetch-reactions
       :add-reaction #'shipit-pr-gitlab--add-reaction
       :delete-reaction #'shipit-pr-gitlab--delete-reaction
       :add-label #'shipit-pr-gitlab--add-label
       :remove-label #'shipit-pr-gitlab--remove-label
       :fetch-labels #'shipit-pr-gitlab--fetch-labels
       :set-labels #'shipit-pr-gitlab--set-labels
       :fetch-available-labels #'shipit-pr-gitlab--fetch-available-labels
       :add-reviewer #'shipit-pr-gitlab--add-reviewer
       :remove-reviewer #'shipit-pr-gitlab--remove-reviewer
       :add-reviewers-batch #'shipit-pr-gitlab--add-reviewers-batch
       :fetch-requested-reviewers #'shipit-pr-gitlab--fetch-requested-reviewers
       :add-assignee #'shipit-pr-gitlab--add-assignee
       :remove-assignee #'shipit-pr-gitlab--remove-assignee
       :add-assignees-batch #'shipit-pr-gitlab--add-assignees-batch
       :fetch-pr-assignees #'shipit-pr-gitlab--fetch-pr-assignees
       :fetch-available-assignees #'shipit-pr-gitlab--fetch-available-assignees
       :dismiss-review #'shipit-pr-gitlab--dismiss-review
       :fetch-commit #'shipit-pr-gitlab--fetch-commit
       :fetch-review-threads #'shipit-pr-gitlab--fetch-review-threads
       :fetch-resolved-threads #'shipit-pr-gitlab--fetch-resolved-threads
       :fetch-branch-protection #'shipit-pr-gitlab--fetch-branch-protection
       :fetch-codeowners #'shipit-pr-gitlab--fetch-codeowners
       :search-repos #'shipit-pr-gitlab--search-repos
       :search-repos-request #'shipit-pr-gitlab--search-repos-request
       :refspec-for-pr #'shipit-pr-gitlab--refspec-for-pr
       :remote-for-fetch #'shipit-pr-gitlab--remote-for-fetch
       :browse-issue-url #'shipit-pr-gitlab--browse-issue-url
       :browse-commit-url #'shipit-pr-gitlab--browse-commit-url
       :browse-user-url #'shipit-pr-gitlab--browse-user-url
       :get-current-username #'shipit-pr-gitlab--get-current-username
       :extract-username-from-email #'shipit-pr-gitlab--extract-username-from-email
       :fetch-check-suites-async #'shipit-pr-gitlab--fetch-check-suites-async
       :fetch-suite-check-runs-async #'shipit-pr-gitlab--fetch-suite-check-runs-async
       :fetch-repo-info #'shipit-pr-gitlab--fetch-repo-info
       :fetch-readme #'shipit-pr-gitlab--fetch-readme
       :fetch-languages #'shipit-pr-gitlab--fetch-languages
       :classify-url #'shipit-pr-gitlab--classify-url
       :create-reference-overlays #'shipit-pr-gitlab--create-reference-overlays
       :hash-insert-reference-fn #'shipit-editor-insert-issue-reference
       :browse-repo-url (lambda (config)
                          (let ((url (or (plist-get config :url) "https://gitlab.com")))
                            (format "%s/%s" url (plist-get config :repo))))))

(provide 'shipit-pr-gitlab)
;;; shipit-pr-gitlab.el ends here
