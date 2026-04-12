;;; shipit-issue-github.el --- GitHub issue backend -*- lexical-binding: t; -*-

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
;; GitHub issue backend for the pluggable issue system.
;; Wraps existing shipit API functions into the backend interface.
;; No data normalization needed — GitHub's API format IS the normalized format.

;;; Code:

(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-http)

;; Forward declarations
(declare-function shipit--search-prs-with-encoded-query "shipit-commands")
(declare-function shipit-pr-github--mark-notification-read "shipit-pr-github")
(declare-function shipit-issues--build-search-query "shipit-issues")
(declare-function shipit--extract-limit-from-args "shipit-commands")
(declare-function shipit--extract-sort-from-args "shipit-commands")
(declare-function shipit-gh-etag-get-json-paginated "shipit-gh-etag")

(defun shipit-issue-github--fetch-issue (config id)
  "Fetch issue ID using CONFIG.
CONFIG must contain :repo.  Also fetches timeline for changelog."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s" repo id))
         (result (shipit-gh-etag-get-json-with-refresh-cache endpoint nil shipit-github-token))
         (issue (plist-get result :json))
         (changelog (shipit-issue-github--fetch-timeline config id)))
    (shipit--debug-log "GitHub backend: fetched issue #%s from %s" id repo)
    (if changelog
        (append issue `((changelog . ,changelog)))
      issue)))

(defun shipit-issue-github--fetch-comments (config id)
  "Fetch comments for issue ID using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/comments" repo id)))
    (shipit--debug-log "GitHub backend: fetching comments for issue #%s from %s" id repo)
    (shipit--api-request-paginated endpoint)))

(defun shipit-issue-github--fetch-comments-async (config id callback)
  "Fetch comments for issue ID using CONFIG asynchronously.
Calls CALLBACK with the list of comments."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/comments" repo id)))
    (shipit--debug-log "GitHub backend: fetching comments async for issue #%s from %s" id repo)
    (shipit--api-request-paginated-async endpoint callback)))

(defun shipit-issue-github--fetch-comments-head-tail-async (config id per-page head-n tail-n callback)
  "Fetch only head and tail comments for issue ID using CONFIG.
PER-PAGE controls the page size. HEAD-N and TAIL-N are how many
to show from each end.  Calls CALLBACK with a plist:
  :head       - first HEAD-N comments
  :tail       - last TAIL-N comments
  :hidden     - fetched but not displayed comments
  :total      - estimated total comment count
  :unfetched  - list of unfetched page numbers
  :per-page   - page size used
Uses the Link header to jump to the last page instead of fetching
all intermediate pages."
  (let* ((repo (plist-get config :repo))
         (base-url (format "%s/repos/%s/issues/%s/comments"
                           (or shipit-api-url "https://api.github.com") repo id))
         (url1 (format "%s?per_page=%d&page=1" base-url per-page))
         (headers `(("Accept" . "application/vnd.github+json")
                    ("Authorization" . ,(format "Bearer %s" (shipit--github-token)))
                    ("X-GitHub-Api-Version" . "2022-11-28"))))
    (shipit--debug-log "GitHub: fetching head+tail comments for issue #%s" id)
    (shipit--url-retrieve-async-with-headers
     url1 "GET" headers nil
     (lambda (page1-data response-headers)
       (let ((last-page (shipit--parse-link-header-last-page response-headers))
             (page1 (or page1-data '())))
         (if (or (null last-page) (<= last-page 1))
             ;; Single page: we have everything
             (let ((total (length page1)))
               (if (<= total (+ head-n tail-n))
                   ;; Few enough to show all -- no split needed
                   (funcall callback
                            (list :head page1 :tail nil
                                  :hidden-head nil :hidden-tail nil
                                  :total total :unfetched nil :per-page per-page))
                 ;; Split into head + hidden + tail
                 (funcall callback
                          (list :head (seq-take page1 head-n)
                                :tail (seq-subseq page1 (- total tail-n))
                                :hidden-head (seq-subseq page1 head-n (- total tail-n))
                                :hidden-tail nil
                                :total total :unfetched nil :per-page per-page))))
           ;; Multi-page: also fetch last page
           (let ((url-last (format "%s?per_page=%d&page=%d" base-url per-page last-page)))
             (shipit--url-retrieve-async
              url-last "GET" headers nil
              (lambda (last-data)
                (let* ((last-page-data (or last-data '()))
                       (total (+ (* (1- last-page) per-page) (length last-page-data)))
                       (head (seq-take page1 head-n))
                       (tail (seq-subseq last-page-data
                                         (max 0 (- (length last-page-data) tail-n))))
                       (hidden-from-page1 (seq-drop page1 head-n))
                       (hidden-from-last (seq-take last-page-data
                                                   (max 0 (- (length last-page-data) tail-n))))
                       (hidden-head hidden-from-page1)
                       (hidden-tail hidden-from-last)
                       (unfetched (cl-loop for p from 2 below last-page collect p)))
                  (funcall callback
                           (list :head head
                                 :tail tail
                                 :hidden-head hidden-head
                                 :hidden-tail hidden-tail
                                 :total total
                                 :unfetched unfetched
                                 :per-page per-page))))
              (lambda (err)
                (shipit--debug-log "GitHub: failed to fetch last page: %s" err)
                ;; Fall back to page 1 only
                (funcall callback
                         (list :head (seq-take page1 head-n)
                               :tail nil
                               :hidden-head (seq-drop page1 head-n)
                               :hidden-tail nil
                               :total (length page1)
                               :unfetched nil
                               :per-page per-page))))))))
     (lambda (err)
       (shipit--debug-log "GitHub: head+tail comment fetch failed: %s" err)
       (funcall callback (list :head nil :tail nil :hidden-head nil :hidden-tail nil
                               :total 0 :unfetched nil :per-page per-page))))))

(defun shipit-issue-github--fetch-pinned-comment-async (config issue-number callback)
  "Fetch the pinned comment for ISSUE-NUMBER via GraphQL.
CALLBACK receives a plist with :comment :pinned-by :pinned-at, or
nil if no comment is pinned.  Uses `shipit--url-retrieve-async' for
non-blocking HTTP."
  (let* ((repo (plist-get config :repo))
         (parts (split-string repo "/"))
         (owner (car parts))
         (name (cadr parts))
         (query "query($owner: String!, $name: String!, $number: Int!) { repository(owner: $owner, name: $name) { issue(number: $number) { pinnedIssueComment { pinnedBy { login avatarUrl } pinnedAt issueComment { databaseId body createdAt author { login avatarUrl } } } } } }")
         (payload (json-encode `((query . ,query)
                                 (variables . ((owner . ,owner)
                                               (name . ,name)
                                               (number . ,issue-number))))))
         (url (concat (or shipit-api-url "https://api.github.com") "/graphql"))
         (headers `(("Authorization" . ,(format "Bearer %s" (shipit--github-token)))
                    ("Accept" . "application/vnd.github.v3+json")
                    ("Content-Type" . "application/json"))))
    (shipit--url-retrieve-async
     url "POST" headers payload
     (lambda (response)
       (let* ((data (cdr (assq 'data response)))
              (issue (cdr (assq 'issue (cdr (assq 'repository data)))))
              (pinned (cdr (assq 'pinnedIssueComment issue))))
         (if pinned
             (let* ((comment-raw (cdr (assq 'issueComment pinned)))
                    (author (cdr (assq 'author comment-raw)))
                    (pinned-by-obj (cdr (assq 'pinnedBy pinned)))
                    (comment `((id . ,(cdr (assq 'databaseId comment-raw)))
                               (body . ,(cdr (assq 'body comment-raw)))
                               (created_at . ,(cdr (assq 'createdAt comment-raw)))
                               (user . ((login . ,(cdr (assq 'login author)))
                                        (avatar_url . ,(cdr (assq 'avatarUrl author))))))))
               (funcall callback (list :comment comment
                                       :pinned-by (cdr (assq 'login pinned-by-obj))
                                       :pinned-at (cdr (assq 'pinnedAt pinned)))))
           (funcall callback nil))))
     (lambda (_err)
       (funcall callback nil)))))

(defun shipit-issue-github--search (config args)
  "Search issues using CONFIG with transient ARGS.
Builds GitHub search query from ARGS and delegates to the GitHub search API."
  (let* ((repo (plist-get config :repo))
         (query-parts (shipit-issues--build-search-query args repo))
         (limit (shipit--extract-limit-from-args args))
         (sort-by (shipit--extract-sort-from-args args))
         (per-page (min 50 limit)))
    (shipit--search-prs-with-encoded-query repo query-parts per-page limit sort-by)))

(defun shipit-issue-github--create-issue (config title body)
  "Create a new issue in the repo specified by CONFIG.
TITLE is the issue title, BODY is the issue body.
Returns the created issue alist (already in normalized format)."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues" repo))
         (data `((title . ,title) (body . ,body))))
    (shipit--debug-log "GitHub backend: creating issue in %s" repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-issue-github--add-comment (config issue-id body)
  "Add a comment to issue ISSUE-ID using CONFIG.
CONFIG must contain :repo.  BODY is the comment text.
Returns the created comment alist."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/comments" repo issue-id))
         (data `((body . ,body))))
    (shipit--debug-log "GitHub backend: adding comment to issue #%s in %s" issue-id repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-issue-github--edit-comment (config comment-id body)
  "Edit comment COMMENT-ID using CONFIG.
CONFIG must contain :repo.  BODY is the new comment text.
Returns the updated comment alist."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/comments/%s" repo comment-id))
         (data `((body . ,body))))
    (shipit--debug-log "GitHub backend: editing comment %s in %s" comment-id repo)
    (shipit--api-request-post endpoint data "PATCH")))

(defun shipit-issue-github--toggle-reaction (config comment-id reaction)
  "Toggle REACTION on comment COMMENT-ID using CONFIG.
CONFIG must contain :repo.  REACTION is a GitHub reaction string.
Returns the reaction alist."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/comments/%s/reactions" repo comment-id))
         (data `((content . ,reaction))))
    (shipit--debug-log "GitHub backend: toggling reaction %s on comment %s" reaction comment-id)
    (shipit--api-request-post endpoint data)))

(defun shipit-issue-github--update-description (config issue-number body)
  "Update the description of issue ISSUE-NUMBER using CONFIG.
CONFIG must contain :repo.  BODY is the new description text.
Returns the updated issue alist."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s" repo issue-number))
         (data `((body . ,body))))
    (shipit--debug-log "GitHub backend: updating description for issue #%s in %s" issue-number repo)
    (shipit--api-request-post endpoint data "PATCH")))

(defconst shipit-issue-github--timeline-event-types
  '("closed" "reopened" "labeled" "unlabeled" "assigned" "unassigned"
    "milestoned" "demilestoned" "renamed" "locked" "unlocked")
  "Timeline event types to include in the activity changelog.")

(defun shipit-issue-github--timeline-event-to-item (event)
  "Map timeline EVENT to a changelog item alist.
Returns ((field . STR) (from . STR) (to . STR)) or nil."
  (let ((type (cdr (assq 'event event))))
    (pcase type
      ("closed"
       (let* ((reason (cdr (assq 'state_reason event)))
              (to (if (and reason (not (equal reason "completed")))
                      (format "closed (%s)" reason)
                    "closed")))
         `((field . "status") (from . "open") (to . ,to))))
      ("reopened"
       '((field . "status") (from . "closed") (to . "open")))
      ("labeled"
       `((field . "labels") (from . "") (to . ,(cdr (assq 'name (cdr (assq 'label event)))))))
      ("unlabeled"
       `((field . "labels") (from . ,(cdr (assq 'name (cdr (assq 'label event))))) (to . "")))
      ("assigned"
       `((field . "assignee") (from . "") (to . ,(cdr (assq 'login (cdr (assq 'assignee event)))))))
      ("unassigned"
       `((field . "assignee") (from . ,(cdr (assq 'login (cdr (assq 'assignee event))))) (to . "")))
      ("milestoned"
       `((field . "milestone") (from . "") (to . ,(cdr (assq 'title (cdr (assq 'milestone event)))))))
      ("demilestoned"
       `((field . "milestone") (from . ,(cdr (assq 'title (cdr (assq 'milestone event))))) (to . "")))
      ("renamed"
       (let ((rename (cdr (assq 'rename event))))
         `((field . "title") (from . ,(cdr (assq 'from rename))) (to . ,(cdr (assq 'to rename))))))
      ("locked"
       '((field . "locked") (from . "") (to . "locked")))
      ("unlocked"
       '((field . "locked") (from . "locked") (to . ""))))))

(defun shipit-issue-github--normalize-timeline-event (event)
  "Normalize a single timeline EVENT to a changelog entry.
Returns an alist with id, created_at, user, items — or nil for skipped types."
  (let ((type (cdr (assq 'event event))))
    (when (member type shipit-issue-github--timeline-event-types)
      (let ((actor (cdr (assq 'actor event)))
            (item (shipit-issue-github--timeline-event-to-item event)))
        (when item
          `((id . ,(format "%s-%s" type (cdr (assq 'created_at event))))
            (created_at . ,(cdr (assq 'created_at event)))
            (user . ((login . ,(cdr (assq 'login actor)))
                     (avatar_url . ,(cdr (assq 'avatar_url actor)))))
            (items . (,item))))))))

(defun shipit-issue-github--normalize-timeline (timeline)
  "Normalize GitHub TIMELINE events to changelog format.
TIMELINE is a vector or list of event alists from the timeline API.
Returns a list of changelog entries, or nil if empty."
  (when (and timeline (> (length timeline) 0))
    (let ((events (append timeline nil)))
      (delq nil (mapcar #'shipit-issue-github--normalize-timeline-event events)))))

(defun shipit-issue-github--fetch-timeline (config id)
  "Fetch timeline for issue ID using CONFIG.
Returns normalized changelog entries or nil on failure."
  (condition-case err
      (let* ((repo (plist-get config :repo))
             (endpoint (format "/repos/%s/issues/%s/timeline" repo id))
             (raw (shipit--api-request-paginated endpoint)))
        (shipit--debug-log "GitHub backend: fetched timeline for issue #%s (%d events)"
                           id (length raw))
        (shipit-issue-github--normalize-timeline raw))
    (error
     (shipit--debug-log "GitHub backend: timeline fetch failed for #%s: %s" id err)
     nil)))

(defun shipit-issue-github--reference-patterns (_config)
  "Return reference patterns for GitHub issues.
CONFIG is unused since GitHub patterns are fixed."
  '(("#\\([0-9]+\\)" 1 string-to-number)))

(defun shipit-issue-github--browse-url (config id)
  "Return browser URL for issue ID using CONFIG."
  (format "https://github.com/%s/issues/%s" (plist-get config :repo) id))

(defun shipit-issue-github--id-to-string (id)
  "Convert numeric ID to display string \"#42\"."
  (format "#%s" id))

(defun shipit-issue-github--string-to-id (str)
  "Convert display string \"#42\" to numeric id."
  (string-to-number (replace-regexp-in-string "^#" "" str)))

;;; Reactions

(defun shipit-issue-github--fetch-reactions (config issue-number)
  "Fetch reactions for issue ISSUE-NUMBER using CONFIG.
CONFIG must contain :repo.  Returns a list of reaction alists."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/reactions" repo issue-number)))
    (shipit--debug-log "GitHub backend: fetching reactions for issue #%s from %s" issue-number repo)
    (shipit--api-request-paginated endpoint)))

(defun shipit-issue-github--fetch-reactions-async (config issue-number callback)
  "Fetch reactions for issue ISSUE-NUMBER asynchronously using CONFIG.
CONFIG must contain :repo.  Calls CALLBACK with the list of reaction
alists when complete.  Non-blocking — the HTTP requests run via
`url-retrieve' so the main thread stays responsive."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/reactions" repo issue-number)))
    (shipit--debug-log "GitHub backend: async fetching reactions for issue #%s from %s"
                       issue-number repo)
    (shipit--api-request-paginated-async endpoint callback)))

(defun shipit-issue-github--add-reaction (config issue-number reaction)
  "Add REACTION to issue ISSUE-NUMBER using CONFIG.
CONFIG must contain :repo.  REACTION is a GitHub reaction string.
Returns the reaction alist."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/reactions" repo issue-number))
         (data `((content . ,reaction))))
    (shipit--debug-log "GitHub backend: adding reaction %s to issue #%s" reaction issue-number)
    (shipit--api-request-post endpoint data)))

(defun shipit-issue-github--remove-reaction (config issue-number reaction-id)
  "Remove reaction REACTION-ID from issue ISSUE-NUMBER using CONFIG.
CONFIG must contain :repo.  Returns t on success."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/reactions/%s" repo issue-number reaction-id))
         (url (concat shipit-api-url endpoint))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "GitHub backend: removing reaction %s from issue #%s" reaction-id issue-number)
    (let ((status-code (shipit--url-retrieve-sync url "DELETE" headers nil t)))
      (unless (and (numberp status-code) (memq status-code '(200 204)))
        (error "GitHub delete issue reaction failed with status %s" status-code))
      t)))

;;; Creation field metadata

(defun shipit-issue-github--creation-fields (_config)
  "Return field descriptors for GitHub issue creation."
  (list
   (list :name 'title :label "Title" :type 'string :required t
         :fetch-options nil)
   (list :name 'body :label "Description" :type 'text :required nil
         :fetch-options nil)
   (list :name 'labels :label "Labels" :type 'multi-select :required nil
         :fetch-options #'shipit-issue-github--fetch-label-names)
   (list :name 'assignees :label "Assignees" :type 'multi-select :required nil
         :fetch-options #'shipit-issue-github--fetch-assignee-names)))

(defun shipit-issue-github--fetch-label-names (config)
  "Fetch available label names for the repo in CONFIG."
  (require 'shipit-diff)
  (let ((repo (plist-get config :repo)))
    (mapcar (lambda (label) (cdr (assq 'name label)))
            (shipit--get-available-labels repo))))

(defun shipit-issue-github--fetch-assignee-names (config)
  "Fetch available assignee usernames for the repo in CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--get-available-assignees repo)))

(defun shipit-issue-github--create-issue-extended (config fields)
  "Create a GitHub issue with extended FIELDS.
FIELDS is an alist with keys: title, body, labels, assignees.
Returns the created issue alist."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues" repo))
         (data `((title . ,(cdr (assq 'title fields)))
                 (body . ,(or (cdr (assq 'body fields)) "")))))
    (let ((labels (cdr (assq 'labels fields))))
      (when labels
        (push `(labels . ,labels) data)))
    (let ((assignees (cdr (assq 'assignees fields))))
      (when assignees
        (push `(assignees . ,assignees) data)))
    (shipit--debug-log "GitHub backend: creating issue (extended) in %s" repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-issue-github--mark-notification-read (_config activity)
  "Mark a GitHub notification as read.
Extracts the thread ID from ACTIVITY and calls the GitHub API
via the PR backend's mark-notification-read function."
  (let* ((notification (cdr (assq 'notification activity)))
         (notification-id (when notification (cdr (assq 'id notification)))))
    (unless notification
      (error "Cannot mark GitHub notification as read: activity missing 'notification' key (keys: %S)"
             (mapcar #'car activity)))
    (unless notification-id
      (error "Cannot mark GitHub notification as read: notification missing 'id' (keys: %S)"
             (mapcar #'car notification)))
    (require 'shipit-pr-github)
    (shipit-pr-github--mark-notification-read nil notification-id)))

;; Register the GitHub backend
(shipit-issue-register-backend
 'github
 (list :name "GitHub"
       :fetch-issue #'shipit-issue-github--fetch-issue
       :fetch-comments #'shipit-issue-github--fetch-comments
       :fetch-comments-async #'shipit-issue-github--fetch-comments-async
       :fetch-comments-head-tail-async #'shipit-issue-github--fetch-comments-head-tail-async
       :fetch-pinned-comment-async #'shipit-issue-github--fetch-pinned-comment-async
       :search #'shipit-issue-github--search
       :create-issue #'shipit-issue-github--create-issue
       :reference-patterns #'shipit-issue-github--reference-patterns
       :browse-url #'shipit-issue-github--browse-url
       :id-to-string #'shipit-issue-github--id-to-string
       :string-to-id #'shipit-issue-github--string-to-id
       :add-comment #'shipit-issue-github--add-comment
       :edit-comment #'shipit-issue-github--edit-comment
       :toggle-reaction #'shipit-issue-github--toggle-reaction
       :update-description #'shipit-issue-github--update-description
       :fetch-reactions #'shipit-issue-github--fetch-reactions
       :fetch-reactions-async #'shipit-issue-github--fetch-reactions-async
       :add-reaction #'shipit-issue-github--add-reaction
       :remove-reaction #'shipit-issue-github--remove-reaction
       :creation-fields #'shipit-issue-github--creation-fields
       :create-issue-extended #'shipit-issue-github--create-issue-extended
       :mark-notification-read #'shipit-issue-github--mark-notification-read
       :icon-spec '("mark-github" "octicons" . "#888888")
       :icon-fallback-text "GH"))

;;; Watched Repos (GitHub-specific)

(defvar shipit-issues--watched-repos-cache nil
  "Cached list of watched repo names from GitHub API.")

(defvar shipit-issues--watched-repos-time nil
  "Time when `shipit-issues--watched-repos-cache' was last populated.")

(defconst shipit-issues--watched-repos-ttl 300
  "Seconds before the watched-repos cache expires.")

(defun shipit-issues--fetch-watched-repos ()
  "Return list of repo full-names the user watches on GitHub.
Uses a TTL cache to avoid repeated API calls."
  (when (or (null shipit-issues--watched-repos-cache)
            (null shipit-issues--watched-repos-time)
            (> (float-time (time-subtract nil shipit-issues--watched-repos-time))
               shipit-issues--watched-repos-ttl))
    (condition-case err
        (let* ((result (shipit-gh-etag-get-json-paginated
                        "/user/subscriptions"
                        '(("per_page" . "100"))
                        shipit-github-token))
               (repos (plist-get result :json)))
          (setq shipit-issues--watched-repos-cache
                (mapcar (lambda (r) (cdr (assq 'full_name r))) repos)
                shipit-issues--watched-repos-time (current-time))
          (shipit--debug-log "Fetched %d watched repos from GitHub"
                             (length shipit-issues--watched-repos-cache)))
      (error
       (shipit--debug-log "Failed to fetch watched repos: %S" err)
       (unless shipit-issues--watched-repos-cache
         (setq shipit-issues--watched-repos-cache nil)))))
  shipit-issues--watched-repos-cache)

(provide 'shipit-issue-github)
;;; shipit-issue-github.el ends here
