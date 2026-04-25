;;; shipit-issue-gitlab.el --- GitLab issue backend -*- lexical-binding: t; -*-

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
;; GitLab issue backend for the pluggable issue system.
;; Reuses shipit-gitlab-http.el for API plumbing.
;; Normalizes GitLab API responses to common shipit alists
;; consumed by the UI layer.
;;
;; Config plist keys (same as shipit-gitlab-http):
;;   :api-url       — GitLab instance (default "https://gitlab.com")
;;   :project-path  — "group/project" (or :project-id for numeric)
;;   :token         — optional; falls back to auth-source

;;; Code:

(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-gitlab-http)

;;; Normalization — GitLab → shipit alist format

(defun shipit-issue-gitlab--normalize-state (state)
  "Normalize GitLab STATE to shipit convention.
\"opened\" → \"open\", everything else passes through."
  (if (equal state "opened") "open" state))

(defun shipit-issue-gitlab--normalize-labels (labels)
  "Normalize GitLab LABELS (list of strings) to ((name . X)) alist format."
  (mapcar (lambda (l) `((name . ,l))) (append labels nil)))

(defun shipit-issue-gitlab--normalize-assignees (assignees)
  "Normalize GitLab ASSIGNEES array to ((login . username)) format."
  (mapcar (lambda (a)
            `((login . ,(cdr (assq 'username a)))
              (avatar_url . ,(cdr (assq 'avatar_url a)))))
          (append assignees nil)))

(defun shipit-issue-gitlab--normalize-issue (gitlab-data)
  "Normalize GitLab issue GITLAB-DATA to shipit alist format.
Maps GitLab fields to the keys expected by shipit UI:
  iid → number, id
  title → title
  state (\"opened\"→\"open\")
  description → body
  author.username → user.login
  web_url → html_url
  labels (strings) → labels ((name . x) alists)"
  (let* ((author (cdr (assq 'author gitlab-data)))
         (assignees-raw (cdr (assq 'assignees gitlab-data)))
         (labels-raw (cdr (assq 'labels gitlab-data)))
         (description (or (cdr (assq 'description gitlab-data)) "")))
    `((id . ,(cdr (assq 'iid gitlab-data)))
      (number . ,(cdr (assq 'iid gitlab-data)))
      (title . ,(cdr (assq 'title gitlab-data)))
      (state . ,(shipit-issue-gitlab--normalize-state
                 (cdr (assq 'state gitlab-data))))
      (body . ,description)
      (user . ((login . ,(cdr (assq 'username author)))
               (avatar_url . ,(cdr (assq 'avatar_url author)))))
      (html_url . ,(cdr (assq 'web_url gitlab-data)))
      (assignees . ,(shipit-issue-gitlab--normalize-assignees assignees-raw))
      (labels . ,(shipit-issue-gitlab--normalize-labels labels-raw))
      (comments . ,(or (cdr (assq 'user_notes_count gitlab-data)) 0))
      (created_at . ,(cdr (assq 'created_at gitlab-data)))
      (updated_at . ,(cdr (assq 'updated_at gitlab-data))))))

;;; Comment normalization

(defun shipit-issue-gitlab--system-note-p (note)
  "Return non-nil if NOTE is a GitLab system note."
  (let ((system (cdr (assq 'system note))))
    (and system (not (eq system :json-false)))))

(defun shipit-issue-gitlab--filter-user-notes (notes)
  "Filter NOTES to exclude system notes."
  (seq-remove #'shipit-issue-gitlab--system-note-p notes))

(defun shipit-issue-gitlab--normalize-comment (note)
  "Normalize a single GitLab NOTE to shipit comment format."
  (let ((author (cdr (assq 'author note))))
    `((id . ,(cdr (assq 'id note)))
      (body . ,(cdr (assq 'body note)))
      (user . ((login . ,(cdr (assq 'username author)))
               (avatar_url . ,(cdr (assq 'avatar_url author)))))
      (created_at . ,(cdr (assq 'created_at note)))
      (updated_at . ,(cdr (assq 'updated_at note))))))

;;; Backend API functions

(defun shipit-issue-gitlab--build-issue-path (config id)
  "Build API path for issue ID using CONFIG.
Uses concat to avoid format-string issues with URL-encoded project paths."
  (concat "/projects/" (shipit-gitlab--project-path config) "/issues/" (format "%s" id)))

(defun shipit-issue-gitlab--fetch-issue (config id)
  "Fetch GitLab issue ID using CONFIG and normalize to shipit format.
Also fetches timeline for changelog."
  (let* ((path (shipit-issue-gitlab--build-issue-path config id))
         (raw (shipit-gitlab--api-request config path)))
    (shipit--debug-log "GitLab issue backend: fetched issue #%s" id)
    (when raw
      (let* ((issue (shipit-issue-gitlab--normalize-issue raw))
             (changelog (shipit-issue-gitlab--fetch-timeline config id)))
        (if changelog
            (append issue `((changelog . ,changelog)))
          issue)))))

(defun shipit-issue-gitlab--fetch-comments (config id)
  "Fetch comments for GitLab issue ID using CONFIG.
Filters out system notes and returns normalized comments."
  (let* ((path (concat (shipit-issue-gitlab--build-issue-path config id)
                       "/notes?sort=asc"))
         (raw (shipit-gitlab--api-request config path))
         (notes (append raw nil))
         (user-notes (shipit-issue-gitlab--filter-user-notes notes)))
    (shipit--debug-log "GitLab issue backend: fetched %d notes (%d user) for #%s"
                       (length notes) (length user-notes) id)
    (mapcar #'shipit-issue-gitlab--normalize-comment user-notes)))

(defun shipit-issue-gitlab--fetch-comments-async (config id callback)
  "Fetch comments for GitLab issue ID using CONFIG asynchronously.
Calls CALLBACK with normalized comment list."
  (let ((path (concat (shipit-issue-gitlab--build-issue-path config id)
                      "/notes?sort=asc")))
    (shipit-gitlab--api-request-async
     config path
     (lambda (data)
       (if (not data)
           (funcall callback nil)
         (let* ((notes (append data nil))
                (user-notes (shipit-issue-gitlab--filter-user-notes notes)))
           (funcall callback
                    (mapcar #'shipit-issue-gitlab--normalize-comment
                            user-notes))))))))

(defun shipit-issue-gitlab--search (config args)
  "Search GitLab issues using CONFIG with transient ARGS.
Maps transient args to GitLab API query parameters."
  (let* ((params (shipit-issue-gitlab--build-search-params args))
         (path (concat "/projects/" (shipit-gitlab--project-path config)
                       "/issues?" params))
         (raw (shipit-gitlab--api-request config path))
         (issues (append raw nil)))
    (shipit--debug-log "GitLab issue backend: search returned %d results" (length issues))
    (mapcar #'shipit-issue-gitlab--normalize-issue issues)))

(defun shipit-issue-gitlab--build-search-params (args)
  "Build GitLab API query string from transient ARGS."
  (let ((params '()))
    (dolist (arg args)
      (cond
       ((string-prefix-p "--state=" arg)
        (let ((state (substring arg 8)))
          (cond
           ((string= state "open")
            (push "state=opened" params))
           ((string= state "closed")
            (push "state=closed" params))
           ((string= state "all")
            (push "state=all" params)))))
       ((string-prefix-p "--author=" arg)
        (let ((author (substring arg 9)))
          (when (> (length author) 0)
            (push (format "author_username=%s" author) params))))
       ((string-prefix-p "--assignee=" arg)
        (let ((assignee (substring arg 11)))
          (when (> (length assignee) 0)
            (push (format "assignee_username=%s" assignee) params))))
       ((string-prefix-p "--label=" arg)
        (let ((label (substring arg 8)))
          (when (> (length label) 0)
            (push (format "labels=%s" label) params))))
       ((string-prefix-p "--title=" arg)
        (let ((title (substring arg 8)))
          (when (> (length title) 0)
            (push (format "search=%s" title) params))))
       ((string-prefix-p "--limit=" arg)
        (let ((limit (substring arg 8)))
          (when (> (length limit) 0)
            (push (format "per_page=%s" limit) params))))))
    (if params
        (string-join (nreverse params) "&")
      "")))

(defun shipit-issue-gitlab--create-issue (config title body)
  "Create a new GitLab issue using CONFIG with TITLE and BODY.
Returns normalized issue alist."
  (let* ((path (concat "/projects/" (shipit-gitlab--project-path config)
                       "/issues"))
         (data `((title . ,title) (description . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab issue backend: created issue")
    (when raw
      (shipit-issue-gitlab--normalize-issue raw))))

(defun shipit-issue-gitlab--add-comment (config issue-id body)
  "Add a comment to GitLab issue ISSUE-ID using CONFIG.
BODY is the comment text.  Returns normalized comment alist."
  (let* ((path (concat (shipit-issue-gitlab--build-issue-path config issue-id)
                       "/notes"))
         (data `((body . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab issue backend: added comment to #%s" issue-id)
    (when raw
      (shipit-issue-gitlab--normalize-comment raw))))

;;; Reference Patterns

(defun shipit-issue-gitlab--reference-patterns (config)
  "Return reference patterns for GitLab issues based on CONFIG.
Returns patterns for #N numeric references and full GitLab URLs."
  (let ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
        (project-path (plist-get config :project-path))
        (patterns nil))
    ;; #N numeric pattern
    (push (list "#\\([0-9]+\\)" 1 #'string-to-number) patterns)
    ;; Full URL pattern: https://host/group/project/-/issues/N
    (when project-path
      (let ((escaped-url (regexp-quote (string-trim-right api-url "/")))
            (escaped-path (regexp-quote project-path)))
        (push (list (format "%s/%s/-/issues/\\([0-9]+\\)"
                            escaped-url escaped-path)
                    1
                    #'string-to-number)
              patterns)))
    (nreverse patterns)))

;;; URL and Display

(defun shipit-issue-gitlab--browse-url (config id)
  "Return browser URL for GitLab issue ID using CONFIG."
  (let ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
        (project-path (plist-get config :project-path)))
    (format "%s/%s/-/issues/%s"
            (string-trim-right api-url "/")
            project-path
            id)))

(defun shipit-issue-gitlab--id-to-string (id)
  "Convert numeric ID to display string \"#42\"."
  (format "#%s" id))

(defun shipit-issue-gitlab--string-to-id (str)
  "Convert display string \"#42\" to numeric id."
  (string-to-number (replace-regexp-in-string "^#" "" str)))

;;; Timeline — issue activity changelog

(defconst shipit-issue-gitlab--timeline-event-types
  '("closed" "reopened" "labeled" "unlabeled" "assigned" "unassigned")
  "System note event types to include in the activity changelog.")

(defun shipit-issue-gitlab--parse-system-note (body)
  "Parse system note BODY into a changelog item alist.
Returns ((field . STR) (from . STR) (to . STR)) or nil."
  (cond
   ((string-match "\\`closed" body)
    '((field . "status") (from . "open") (to . "closed")))
   ((string-match "\\`reopened" body)
    '((field . "status") (from . "closed") (to . "open")))
   ((string-match "\\`added ~\\(.+\\) label" body)
    `((field . "labels") (from . "") (to . ,(match-string 1 body))))
   ((string-match "\\`removed ~\\(.+\\) label" body)
    `((field . "labels") (from . ,(match-string 1 body)) (to . "")))
   ((string-match "\\`assigned to @\\(.+\\)" body)
    `((field . "assignee") (from . "") (to . ,(match-string 1 body))))
   ((string-match "\\`unassigned @\\(.+\\)" body)
    `((field . "assignee") (from . ,(match-string 1 body)) (to . "")))
   ((string-match "\\`changed title from \\*\\*\\(.+?\\)\\*\\* to \\*\\*\\(.+?\\)\\*\\*" body)
    `((field . "title") (from . ,(match-string 1 body)) (to . ,(match-string 2 body))))
   ((string-match "\\`changed the description" body)
    '((field . "description")))
   ((string-match "\\`changed milestone to %\\(.+\\)" body)
    `((field . "milestone") (from . "") (to . ,(match-string 1 body))))
   ((string-match "\\`removed milestone" body)
    '((field . "milestone") (from . "removed") (to . "")))
   ((string-match "\\`set status to \\(.+\\)" body)
    `((field . "status") (from . "") (to . ,(replace-regexp-in-string "\\*\\*" "" (match-string 1 body)))))
   ((string-match "\\`mentioned in merge request !\\([0-9]+\\)" body)
    `((field . "mentioned") (from . "") (to . ,(format "!%s" (match-string 1 body)))))
   ((string-match "\\`created branch \\[`\\(.+?\\)`\\]" body)
    `((field . "branch") (from . "") (to . ,(match-string 1 body))))
   ((string-match "\\`created branch \\(.+\\) to address this issue" body)
    `((field . "branch") (from . "") (to . ,(match-string 1 body))))))

(defun shipit-issue-gitlab--normalize-timeline-event (note)
  "Normalize a GitLab system NOTE to a changelog entry.
Returns an alist with id, created_at, user, items — or nil for skipped types."
  (let* ((body (or (cdr (assq 'body note)) ""))
         (author (cdr (assq 'author note)))
         (item (shipit-issue-gitlab--parse-system-note body)))
    (when item
      `((id . ,(format "system-%s" (cdr (assq 'id note))))
        (created_at . ,(cdr (assq 'created_at note)))
        (user . ((login . ,(cdr (assq 'username author)))
                 (avatar_url . ,(cdr (assq 'avatar_url author)))))
        (items . (,item))))))

(defun shipit-issue-gitlab--fetch-state-events (config id)
  "Fetch resource state events for issue ID using CONFIG.
Returns normalized changelog entries."
  (let* ((path (concat (shipit-issue-gitlab--build-issue-path config id)
                       "/resource_state_events"))
         (raw (shipit-gitlab--api-request config path)))
    (when (and raw (or (vectorp raw) (listp raw)))
      (delq nil
            (mapcar (lambda (event)
                      (let* ((state (cdr (assq 'state event)))
                             (user (cdr (assq 'user event)))
                             (item (pcase state
                                     ("closed"
                                      '((field . "status") (from . "open") (to . "closed")))
                                     ("reopened"
                                      '((field . "status") (from . "closed") (to . "open")))
                                     (_ nil))))
                        (when item
                          `((id . ,(format "state-%s" (cdr (assq 'id event))))
                            (created_at . ,(cdr (assq 'created_at event)))
                            (user . ((login . ,(cdr (assq 'username user)))
                                     (avatar_url . ,(cdr (assq 'avatar_url user)))))
                            (items . (,item))))))
                    (append raw nil))))))

(defun shipit-issue-gitlab--normalize-user-comment (note)
  "Normalize a GitLab user NOTE to a commented changelog entry."
  (let ((author (cdr (assq 'author note))))
    `((id . ,(format "comment-%s" (cdr (assq 'id note))))
      (created_at . ,(cdr (assq 'created_at note)))
      (user . ((login . ,(cdr (assq 'username author)))
               (avatar_url . ,(cdr (assq 'avatar_url author)))))
      (items . (((field . "comment")))))))

(defun shipit-issue-gitlab--fetch-timeline (config id)
  "Fetch timeline for issue ID using CONFIG.
Returns normalized changelog entries or nil on failure."
  (condition-case err
      (let* ((path (concat (shipit-issue-gitlab--build-issue-path config id)
                           "/notes?sort=asc"))
             (raw (shipit-gitlab--api-request config path))
             (notes (append raw nil))
             (system-notes (seq-filter #'shipit-issue-gitlab--system-note-p notes))
             (user-notes (seq-remove #'shipit-issue-gitlab--system-note-p notes))
             (note-entries (delq nil (mapcar #'shipit-issue-gitlab--normalize-timeline-event
                                            system-notes)))
             (comment-entries (mapcar #'shipit-issue-gitlab--normalize-user-comment user-notes))
             (state-entries (shipit-issue-gitlab--fetch-state-events config id))
             (all-entries (append (or state-entries '()) note-entries comment-entries)))
        (shipit--debug-log "GitLab issue backend: timeline for #%s: %d state + %d note + %d comment entries"
                           id (length (or state-entries '())) (length note-entries) (length comment-entries))
        ;; Sort by created_at
        (sort all-entries (lambda (a b)
                           (string< (or (cdr (assq 'created_at a)) "")
                                    (or (cdr (assq 'created_at b)) "")))))
    (error
     (shipit--debug-log "GitLab issue backend: timeline fetch failed for #%s: %s" id err)
     nil)))

;;; Reactions

(defun shipit-issue-gitlab--fetch-reactions (config issue-iid)
  "Fetch reactions for issue ISSUE-IID using CONFIG.
Returns a list of normalized reaction alists."
  (require 'shipit-comment-gitlab)
  (let* ((path (concat (shipit-issue-gitlab--build-issue-path config issue-iid)
                       "/award_emoji"))
         (raw (shipit-gitlab--api-request config path))
         (emojis (append raw nil)))
    (shipit--debug-log "GitLab issue backend: fetched %d reactions for issue #%s"
                       (length emojis) issue-iid)
    (mapcar #'shipit-comment-gitlab--normalize-reaction emojis)))

(defun shipit-issue-gitlab--content-to-emoji-name (content)
  "Convert shipit-standard CONTENT name to GitLab award_emoji name.
E.g. \"+1\" → \"thumbsup\", \"laugh\" → \"laughing\"."
  (or (car (rassoc content shipit-comment-gitlab--emoji-to-content))
      content))

(defun shipit-issue-gitlab--add-reaction (config issue-iid reaction)
  "Add REACTION to issue ISSUE-IID using CONFIG.
REACTION is a shipit-standard content string (e.g. \"+1\").
Returns the normalized reaction alist."
  (require 'shipit-comment-gitlab)
  (let* ((emoji-name (shipit-issue-gitlab--content-to-emoji-name reaction))
         (path (concat (shipit-issue-gitlab--build-issue-path config issue-iid)
                       "/award_emoji"))
         (data `((name . ,emoji-name)))
         (raw (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab issue backend: added reaction %s to issue #%s" emoji-name issue-iid)
    (when raw
      (shipit-comment-gitlab--normalize-reaction raw))))

(defun shipit-issue-gitlab--remove-reaction (config issue-iid reaction-id)
  "Remove reaction REACTION-ID from issue ISSUE-IID using CONFIG.
Returns t on success."
  (let* ((path (concat (shipit-issue-gitlab--build-issue-path config issue-iid)
                       (format "/award_emoji/%s" reaction-id))))
    (shipit--debug-log "GitLab issue backend: removing reaction %s from issue #%s" reaction-id issue-iid)
    (shipit-gitlab--api-request-method config path nil "DELETE")
    t))

;;; Parity operations — Comment editing & reactions

(defun shipit-issue-gitlab--current-issue-iid ()
  "Return the issue IID from the current buffer context.
Uses `shipit-issue-buffer-number' which is set in issue buffers."
  (or (and (boundp 'shipit-issue-buffer-number) shipit-issue-buffer-number)
      (error "GitLab issue comment operation requires an active issue context")))

(defun shipit-issue-gitlab--edit-comment (config comment-id body)
  "Edit comment COMMENT-ID on a GitLab issue using CONFIG.
BODY is the new comment text.  Returns normalized comment alist.
Requires an active issue buffer context for the issue IID."
  (let* ((issue-iid (shipit-issue-gitlab--current-issue-iid))
         (path (format "%s/notes/%s"
                       (shipit-issue-gitlab--build-issue-path config issue-iid)
                       comment-id))
         (data `((body . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab issue backend: edited comment %s on issue #%s"
                       comment-id issue-iid)
    (when raw
      (shipit-issue-gitlab--normalize-comment raw))))

(defun shipit-issue-gitlab--toggle-reaction (config comment-id reaction)
  "Toggle REACTION on issue comment COMMENT-ID using CONFIG.
REACTION is a shipit-standard content string (e.g. \"+1\").
Posts to the note's award_emoji endpoint.
Requires an active issue buffer context for the issue IID."
  (require 'shipit-comment-gitlab)
  (let* ((issue-iid (shipit-issue-gitlab--current-issue-iid))
         (emoji-name (shipit-issue-gitlab--content-to-emoji-name reaction))
         (path (format "%s/notes/%s/award_emoji"
                       (shipit-issue-gitlab--build-issue-path config issue-iid)
                       comment-id))
         (data `((name . ,emoji-name))))
    (shipit--debug-log "GitLab issue backend: toggling reaction %s on note %s"
                       emoji-name comment-id)
    (shipit-gitlab--api-request-method config path data "POST")))

(defun shipit-issue-gitlab--update-description (config issue-iid body)
  "Update the description of issue ISSUE-IID using CONFIG.
BODY is the new description text.  Returns normalized issue."
  (let* ((path (shipit-issue-gitlab--build-issue-path config issue-iid))
         (data `((description . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab issue backend: updated description for issue #%s" issue-iid)
    (when raw
      (shipit-issue-gitlab--normalize-issue raw))))

;;; Parity operations — Creation fields

(defun shipit-issue-gitlab--creation-fields (_config)
  "Return field descriptors for GitLab issue creation."
  (list
   (list :name 'title :label "Title" :type 'string :required t
         :fetch-options nil)
   (list :name 'body :label "Description" :type 'text :required nil
         :fetch-options nil)
   (list :name 'labels :label "Labels" :type 'multi-select :required nil
         :fetch-options (lambda (cfg)
                          (shipit-issue-gitlab--fetch-label-names cfg)))
   (list :name 'assignees :label "Assignees" :type 'multi-select :required nil
         :fetch-options (lambda (cfg)
                          (shipit-issue-gitlab--fetch-assignee-names cfg)))))

(defun shipit-issue-gitlab--resolve-user-id (config username)
  "Resolve USERNAME to a GitLab user ID using CONFIG."
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

(defun shipit-issue-gitlab--fetch-label-names (config)
  "Fetch available label names for the project in CONFIG."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/labels?per_page=100" project))
         (labels (shipit-gitlab--api-request-paginated config path)))
    (mapcar (lambda (l) (cdr (assq 'name l))) (or labels '()))))

(defun shipit-issue-gitlab--fetch-assignee-names (config)
  "Fetch available assignee usernames for the project in CONFIG."
  (let* ((project (shipit-gitlab--project-path config))
         (path (format "/projects/%s/members/all?per_page=100" project))
         (members (shipit-gitlab--api-request-paginated config path)))
    (mapcar (lambda (m) (cdr (assq 'username m))) (or members '()))))

(defun shipit-issue-gitlab--create-issue-extended (config fields)
  "Create a GitLab issue with extended FIELDS.
FIELDS is an alist with keys: title, body, labels, assignees.
Returns normalized issue."
  (let* ((path (concat "/projects/" (shipit-gitlab--project-path config)
                       "/issues"))
         (data `((title . ,(cdr (assq 'title fields)))
                 (description . ,(or (cdr (assq 'body fields)) "")))))
    (let ((labels (cdr (assq 'labels fields))))
      (when labels
        (push `(labels . ,(mapconcat #'identity labels ",")) data)))
    (let ((assignees (cdr (assq 'assignees fields))))
      (when assignees
        ;; GitLab needs user IDs for assignees, but we accept usernames.
        ;; For now, pass as assignee_ids=0 to trigger the username search.
        ;; The API also accepts a comma-separated "labels" string.
        ;; For assignees, we need to resolve usernames to IDs.
        (let ((ids (delq nil (mapcar
                              (lambda (u)
                                (shipit-issue-gitlab--resolve-user-id config u))
                              assignees))))
          (when ids
            (push `(assignee_ids . ,(vconcat ids)) data)))))
    (shipit--debug-log "GitLab issue backend: creating issue (extended)")
    (let ((raw (shipit-gitlab--api-request-method config path data "POST")))
      (when raw
        (shipit-issue-gitlab--normalize-issue raw)))))

;;; Notification Polling (Todos + Events API)

(defcustom shipit-gitlab-notifications-include-events t
  "When non-nil, include GitLab Events API results in notifications.
Events provide broader activity coverage (comments, merges, closes)
beyond the Todos API's actionable items."
  :type 'boolean
  :group 'shipit)

(defvar shipit-issue-gitlab--user-cache nil
  "Cached (API-URL . USERNAME) cons for the current user.")

(defvar shipit-issue-gitlab--project-cache (make-hash-table :test 'equal)
  "Hash table mapping project_id → path_with_namespace.")

(defvar shipit-issue-gitlab--project-ids nil
  "Ordered list of project IDs from membership query.
Ordered by last_activity_at (most recent first).")

;;; Current user

(defun shipit-issue-gitlab--fetch-current-username (config)
  "Return the authenticated user's username for CONFIG.
Caches the result keyed by API URL."
  (let ((api-url (or (plist-get config :api-url) "https://gitlab.com")))
    (if (and shipit-issue-gitlab--user-cache
             (equal api-url (car shipit-issue-gitlab--user-cache)))
        (cdr shipit-issue-gitlab--user-cache)
      (let* ((raw (shipit-gitlab--api-request config "/user"))
             (username (cdr (assq 'username raw))))
        (when username
          (setq shipit-issue-gitlab--user-cache (cons api-url username)))
        username))))

;;; Project cache

(defun shipit-issue-gitlab--ensure-project-cache (config)
  "Populate project cache from GET /projects?membership=true.
Only fetches if both the hash table and ordered ID list are populated.
Builds an ordered list of project IDs sorted by last activity
\(most recent first)."
  (when (or (= 0 (hash-table-count shipit-issue-gitlab--project-cache))
            (null shipit-issue-gitlab--project-ids))
    (clrhash shipit-issue-gitlab--project-cache)
    (let* ((raw (shipit-gitlab--api-request
                 config "/projects?membership=true&per_page=100&order_by=last_activity_at"))
           (projects (append raw nil))
           (ids nil))
      (dolist (proj projects)
        (let ((id (cdr (assq 'id proj)))
              (path (cdr (assq 'path_with_namespace proj))))
          (when (and id path)
            (puthash id path shipit-issue-gitlab--project-cache)
            (push id ids))))
      (setq shipit-issue-gitlab--project-ids (nreverse ids))
      (shipit--debug-log "GitLab events: cached %d projects"
                         (hash-table-count shipit-issue-gitlab--project-cache)))))

(defun shipit-issue-gitlab--project-path-from-id (project-id)
  "Look up path_with_namespace for PROJECT-ID from cache."
  (gethash project-id shipit-issue-gitlab--project-cache))

;;; Todo conversion

(defun shipit-issue-gitlab--map-todo-action (action)
  "Map GitLab todo ACTION string to a notification reason."
  (pcase action
    ("assigned" "assign")
    ((or "mentioned" "directly_addressed") "mention")
    ((or "approval_required" "review_requested") "review_requested")
    ("build_failed" "ci_activity")
    (_ "updated")))

(defun shipit-issue-gitlab--api-url-from-web-url (web-url)
  "Extract the GitLab instance base URL from WEB-URL.
E.g. \"https://gitlab.com/g/p/-/issues/1\" → \"https://gitlab.com\"."
  (when (and web-url (string-match "\\`\\(https?://[^/]+\\)" web-url))
    (match-string 1 web-url)))

(defun shipit-issue-gitlab--todo-to-activity (todo)
  "Convert a single GitLab TODO alist to a notification activity alist."
  (let* ((todo-id (cdr (assq 'id todo)))
         (target (cdr (assq 'target todo)))
         (iid (cdr (assq 'iid target)))
         (title (cdr (assq 'title target)))
         (web-url (cdr (assq 'web_url target)))
         (target-type (cdr (assq 'target_type todo)))
         (action (cdr (assq 'action_name todo)))
         (project (cdr (assq 'project todo)))
         (repo (cdr (assq 'path_with_namespace project)))
         (updated (cdr (assq 'updated_at todo)))
         (type (if (equal target-type "MergeRequest") "pr" "issue"))
         (api-url (or (shipit-issue-gitlab--api-url-from-web-url web-url)
                      "https://gitlab.com")))
    `((number . ,iid)
      (type . ,type)
      (subject . ,(or title ""))
      (reason . ,(shipit-issue-gitlab--map-todo-action action))
      (repo . ,repo)
      (browse-url . ,web-url)
      (updated-at . ,updated)
      (source . gitlab)
      (backend-id . gitlab)
      (gitlab-todo-id . ,todo-id)
      (backend-config . (:api-url ,api-url :project-path ,repo)))))

;;; Event conversion

(defconst shipit-issue-gitlab--note-target-types
  '("Note" "DiffNote" "DiscussionNote")
  "GitLab event target types that represent comments.
These require looking at note.noteable_type for the parent object.")

(defun shipit-issue-gitlab--map-event-action (action-name)
  "Map GitLab event ACTION-NAME to a notification reason string."
  (pcase action-name
    ("commented on" "comment")
    ("merged"       "merged")
    ("closed"       "closed")
    ("reopened"     "reopened")
    ("approved"     "approved")
    ("opened"       "opened")
    (_              action-name)))

(defun shipit-issue-gitlab--event-noteable-type (event)
  "Return the effective noteable type for EVENT.
For Note/DiffNote/DiscussionNote events, reads note.noteable_type.
For direct events (MergeRequest, Issue), returns target_type."
  (let ((tt (cdr (assq 'target_type event))))
    (if (member tt shipit-issue-gitlab--note-target-types)
        (cdr (assq 'noteable_type (cdr (assq 'note event))))
      tt)))

(defun shipit-issue-gitlab--event-noteable-iid (event)
  "Return the MR/issue IID for EVENT.
For note events, reads note.noteable_iid.
For direct events, reads target_iid."
  (let ((tt (cdr (assq 'target_type event))))
    (if (member tt shipit-issue-gitlab--note-target-types)
        (cdr (assq 'noteable_iid (cdr (assq 'note event))))
      (cdr (assq 'target_iid event)))))

(defun shipit-issue-gitlab--event-to-activity (event &optional api-url)
  "Convert a GitLab EVENT alist to a notification activity alist.
Handles both direct events (MergeRequest, Issue) and note events
\(DiffNote, Note, DiscussionNote) by resolving the parent object.
API-URL is the GitLab instance base URL."
  (let* ((noteable-type (shipit-issue-gitlab--event-noteable-type event))
         (iid (shipit-issue-gitlab--event-noteable-iid event))
         (title (cdr (assq 'target_title event)))
         (project-id (cdr (assq 'project_id event)))
         (action-name (cdr (assq 'action_name event)))
         (created-at (cdr (assq 'created_at event)))
         (repo (shipit-issue-gitlab--project-path-from-id project-id))
         (type (if (equal noteable-type "MergeRequest") "pr" "issue"))
         (base-url (or api-url "https://gitlab.com"))
         (mr-or-issue (if (equal noteable-type "MergeRequest")
                          "merge_requests" "issues"))
         (browse-url (when (and repo iid)
                       (format "%s/%s/-/%s/%s"
                               base-url repo mr-or-issue iid))))
    `((number . ,iid)
      (type . ,type)
      (subject . ,(or title ""))
      (reason . ,(shipit-issue-gitlab--map-event-action action-name))
      (repo . ,repo)
      (browse-url . ,browse-url)
      (updated-at . ,created-at)
      (source . gitlab)
      (backend-id . gitlab)
      (backend-config . (:api-url ,base-url :project-path ,repo)))))

(defconst shipit-issue-gitlab--max-event-projects 25
  "Maximum number of projects to poll for events.
Projects are polled in order of most recent activity.")

(defun shipit-issue-gitlab--event-relevant-p (event)
  "Return non-nil if EVENT is relevant for notifications.
Includes MergeRequest/Issue events and Note events that belong
to a MergeRequest or Issue."
  (let ((noteable-type (shipit-issue-gitlab--event-noteable-type event))
        (iid (shipit-issue-gitlab--event-noteable-iid event)))
    (and iid
         (member noteable-type '("MergeRequest" "Issue")))))

(defun shipit-issue-gitlab--fetch-events (config &optional since)
  "Fetch recent project events, filtered to MR/Issue.
Uses per-project GET /projects/:id/events to capture activity by
other users on projects the authenticated user belongs to.
SINCE is ISO8601; converted to YYYY-MM-DD for the `after' param.
GitLab's `after' is exclusive, so we subtract one day.
Polls at most `shipit-issue-gitlab--max-event-projects' projects."
  (let* ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
         (date-param (when since
                       (when (string-match
                              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" since)
                         ;; Subtract 1 day: GitLab's `after' is exclusive
                         (let* ((y (string-to-number (match-string 1 since)))
                                (m (string-to-number (match-string 2 since)))
                                (d (string-to-number (match-string 3 since)))
                                (prev (encode-time 0 0 0 (1- d) m y)))
                           (format-time-string "%Y-%m-%d" prev)))))
         (_ (shipit-issue-gitlab--ensure-project-cache config))
         (project-ids (seq-take shipit-issue-gitlab--project-ids
                                shipit-issue-gitlab--max-event-projects))
         (all-events nil))
    (dolist (project-id project-ids)
      (let* ((path (format "/projects/%s/events?per_page=25%s"
                           project-id
                           (if date-param (concat "&after=" date-param) "")))
             (raw (shipit-gitlab--api-request config path))
             (events (append raw nil)))
        (setq all-events (nconc all-events events))))
    (let ((relevant (seq-filter #'shipit-issue-gitlab--event-relevant-p
                                all-events)))
      (shipit--debug-log
       "GitLab events: fetched %d events from %d projects (%d relevant)"
       (length all-events) (length project-ids) (length relevant))
      (mapcar (lambda (ev)
                (shipit-issue-gitlab--event-to-activity ev api-url))
              relevant))))

;;; Combined notifications

(defun shipit-issue-gitlab--activity-key (activity)
  "Return a dedup key for ACTIVITY: \"type:repo:number\"."
  (format "%s:%s:%s"
          (cdr (assq 'type activity))
          (or (cdr (assq 'repo activity)) "")
          (cdr (assq 'number activity))))

(defun shipit-issue-gitlab--fetch-todos (config)
  "Fetch pending GitLab todos as notification activities.
CONFIG is the backend config plist.
Filters to MergeRequest and Issue target types client-side."
  (let* ((path "/todos?state=pending&per_page=100")
         (raw (shipit-gitlab--api-request config path))
         (todos (append raw nil))
         (relevant (seq-filter
                    (lambda (todo)
                      (member (cdr (assq 'target_type todo))
                              '("MergeRequest" "Issue")))
                    todos)))
    (shipit--debug-log "GitLab notifications: fetched %d todos (%d relevant)"
                       (length todos) (length relevant))
    (mapcar #'shipit-issue-gitlab--todo-to-activity relevant)))

(defun shipit-issue-gitlab--fetch-todos-async (config callback)
  "Async variant of `shipit-issue-gitlab--fetch-todos'.
Calls CALLBACK with a list of activity alists when the API
responds.  Calls back with nil on error."
  (shipit-gitlab--api-request-async
   config "/todos?state=pending&per_page=100"
   (lambda (raw)
     (let* ((todos (when raw (append raw nil)))
            (relevant (seq-filter
                       (lambda (todo)
                         (member (cdr (assq 'target_type todo))
                                 '("MergeRequest" "Issue")))
                       todos)))
       (shipit--debug-log "GitLab notifications (async): fetched %d todos (%d relevant)"
                          (length (or todos '())) (length (or relevant '())))
       (when callback
         (funcall callback
                  (mapcar #'shipit-issue-gitlab--todo-to-activity relevant)))))))

(defun shipit-issue-gitlab--fetch-events-async (config since callback)
  "Async variant of `shipit-issue-gitlab--fetch-events'.
Issues per-project requests in parallel and calls CALLBACK with
the merged activity list once all responses arrive.  Callback gets
nil on error or when no projects are configured."
  (let* ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
         (date-param (when since
                       (when (string-match
                              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" since)
                         (let* ((y (string-to-number (match-string 1 since)))
                                (m (string-to-number (match-string 2 since)))
                                (d (string-to-number (match-string 3 since)))
                                (prev (encode-time 0 0 0 (1- d) m y)))
                           (format-time-string "%Y-%m-%d" prev))))))
    (shipit-issue-gitlab--ensure-project-cache config)
    (let* ((project-ids (seq-take shipit-issue-gitlab--project-ids
                                  shipit-issue-gitlab--max-event-projects))
           (pending (length project-ids))
           (all-events nil))
      (if (zerop pending)
          (when callback (funcall callback nil))
        (dolist (project-id project-ids)
          (let* ((path (format "/projects/%s/events?per_page=25%s"
                               project-id
                               (if date-param (concat "&after=" date-param) ""))))
            (shipit-gitlab--api-request-async
             config path
             (lambda (raw)
               (when raw (setq all-events (nconc all-events (append raw nil))))
               (setq pending (1- pending))
               (when (zerop pending)
                 (let ((relevant (seq-filter #'shipit-issue-gitlab--event-relevant-p
                                             all-events)))
                   (shipit--debug-log
                    "GitLab events (async): fetched %d events from %d projects (%d relevant)"
                    (length all-events) (length project-ids) (length relevant))
                   (when callback
                     (funcall callback
                              (mapcar (lambda (ev)
                                        (shipit-issue-gitlab--event-to-activity ev api-url))
                                      relevant)))))))))))))

(defun shipit-issue-gitlab--fetch-notifications-async (config &optional since callback)
  "Async variant of `shipit-issue-gitlab--fetch-notifications'.
Fetches todos and (optionally) events in parallel, then merges and
invokes CALLBACK with the combined activity list."
  (let ((todo-activities nil)
        (event-activities nil)
        (todos-done nil)
        (events-done (not shipit-gitlab-notifications-include-events)))
    (cl-labels
        ((finish ()
           (when (and todos-done events-done)
             (let ((merged (make-hash-table :test 'equal)))
               (dolist (a (or event-activities '()))
                 (puthash (shipit-issue-gitlab--activity-key a) a merged))
               (dolist (a (or todo-activities '()))
                 (puthash (shipit-issue-gitlab--activity-key a) a merged))
               (let ((result nil))
                 (maphash (lambda (_k v) (push v result)) merged)
                 (shipit--debug-log
                  "GitLab notifications (async): %d todos + %d events = %d combined"
                  (length (or todo-activities '()))
                  (length (or event-activities '()))
                  (length result))
                 (when callback (funcall callback result)))))))
      (shipit-issue-gitlab--fetch-todos-async
       config
       (lambda (acts) (setq todo-activities acts todos-done t) (finish)))
      (when shipit-gitlab-notifications-include-events
        (shipit-issue-gitlab--fetch-events-async
         config since
         (lambda (acts) (setq event-activities acts events-done t) (finish)))))))

(defun shipit-issue-gitlab--fetch-notifications (config &optional since)
  "Fetch GitLab notifications combining Todos and Events.
CONFIG is the backend config plist.  SINCE is an ISO8601 timestamp
used for incremental event polling.
When `shipit-gitlab-notifications-include-events' is non-nil, fetches
both Todos and Events, deduplicating by activity key with Todos
taking priority."
  (let ((todo-activities (shipit-issue-gitlab--fetch-todos config)))
    (if (not shipit-gitlab-notifications-include-events)
        todo-activities
      (let* ((event-activities (shipit-issue-gitlab--fetch-events config since))
             (merged (make-hash-table :test 'equal)))
        ;; Events first — Todos overlay and win on key collisions
        (dolist (activity event-activities)
          (puthash (shipit-issue-gitlab--activity-key activity)
                   activity merged))
        (dolist (activity todo-activities)
          (puthash (shipit-issue-gitlab--activity-key activity)
                   activity merged))
        (let ((result nil))
          (maphash (lambda (_key val) (push val result)) merged)
          (shipit--debug-log "GitLab notifications: %d todos + %d events = %d combined"
                             (length todo-activities)
                             (length event-activities)
                             (length result))
          result)))))

;;; Mark todo as done

(defun shipit-issue-gitlab--mark-todo-done (config todo-id)
  "Mark GitLab todo TODO-ID as done using CONFIG.
Calls POST /todos/:id/mark_as_done."
  (let ((path (format "/todos/%s/mark_as_done" todo-id)))
    (shipit--debug-log "GitLab: marking todo %s as done" todo-id)
    (shipit-gitlab--api-request-method config path nil "POST")))

(defun shipit-issue-gitlab--mark-notification-read (_config activity)
  "Mark a GitLab notification (todo) as done.
Extracts the todo ID and backend config from ACTIVITY."
  (let ((todo-id (cdr (assq 'gitlab-todo-id activity)))
        (backend-config (cdr (assq 'backend-config activity))))
    (when (and todo-id backend-config)
      (shipit-issue-gitlab--mark-todo-done backend-config todo-id))))

(defun shipit-issue-gitlab--autodiscover ()
  "Probe auth-source for gitlab.com credentials.
Returns (\"gitlab.com\" . config-plist) when credentials found, nil otherwise."
  (let* ((api-url "https://gitlab.com")
         (config (list :backend 'gitlab :api-url api-url))
         (auth (shipit-gitlab--auth-header config)))
    (when auth
      (cons "gitlab.com" config))))

;;; Register the GitLab issue backend

(shipit-issue-register-backend
 'gitlab
 (list :name "GitLab"
       :fetch-issue #'shipit-issue-gitlab--fetch-issue
       :fetch-comments #'shipit-issue-gitlab--fetch-comments
       :fetch-comments-async #'shipit-issue-gitlab--fetch-comments-async
       :search #'shipit-issue-gitlab--search
       :create-issue #'shipit-issue-gitlab--create-issue
       :reference-patterns #'shipit-issue-gitlab--reference-patterns
       :browse-url #'shipit-issue-gitlab--browse-url
       :id-to-string #'shipit-issue-gitlab--id-to-string
       :string-to-id #'shipit-issue-gitlab--string-to-id
       :add-comment #'shipit-issue-gitlab--add-comment
       :edit-comment #'shipit-issue-gitlab--edit-comment
       :toggle-reaction #'shipit-issue-gitlab--toggle-reaction
       :update-description #'shipit-issue-gitlab--update-description
       :creation-fields #'shipit-issue-gitlab--creation-fields
       :create-issue-extended #'shipit-issue-gitlab--create-issue-extended
       :fetch-reactions #'shipit-issue-gitlab--fetch-reactions
       :add-reaction #'shipit-issue-gitlab--add-reaction
       :remove-reaction #'shipit-issue-gitlab--remove-reaction
       :notifications #'shipit-issue-gitlab--fetch-notifications
       :notifications-async #'shipit-issue-gitlab--fetch-notifications-async
       :mark-notification-read #'shipit-issue-gitlab--mark-notification-read
       :autodiscover #'shipit-issue-gitlab--autodiscover
       :icon-spec '("gitlab" "simple" . "#FC6D26")
       :icon-fallback-text "GL"))

(provide 'shipit-issue-gitlab)
;;; shipit-issue-gitlab.el ends here
