;;; shipit-comment-github.el --- GitHub comment backend -*- lexical-binding: t; -*-

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
;; GitHub comment backend for the pluggable comment system.
;; Pure API wrappers — no caching, no UI side effects.
;; Orchestrator functions in shipit-http.el keep their caching/UI logic
;; but delegate the raw API call to these functions.

;;; Code:

(require 'shipit-core)
(require 'shipit-comment-backends)
(require 'shipit-http)

;;; Required operations

(defun shipit-comment-github--fetch-general-comments (config number)
  "Fetch general comments for PR NUMBER using CONFIG.
Returns combined issue comments and review comments."
  (let* ((repo (plist-get config :repo))
         (issue-comments (shipit--get-issue-comments repo number))
         (review-comments (shipit--get-pr-reviews-for-comments repo number)))
    (shipit--debug-log "GitHub comment backend: fetched %d issue + %d review comments for PR #%s"
                       (length issue-comments) (length review-comments) number)
    (append issue-comments review-comments)))

(defun shipit-comment-github--fetch-inline-comments (config number)
  "Fetch inline (review) comments for PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/comments" repo number)))
    (shipit--debug-log "GitHub comment backend: fetching inline comments for PR #%s" number)
    (shipit--api-request-paginated endpoint)))

(defun shipit-comment-github--add-general-comment (config number body)
  "Add general comment BODY to PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/comments" repo number))
         (data `((body . ,body))))
    (shipit--debug-log "GitHub comment backend: adding general comment to PR #%s" number)
    (shipit--api-request-post endpoint data)))

(defun shipit-comment-github--add-inline-comment (config number file line body side &optional _old-line)
  "Add inline comment to PR NUMBER on FILE at LINE using CONFIG.
BODY is the comment text, SIDE is \"LEFT\" or \"RIGHT\".
_OLD-LINE is unused (GitLab-specific)."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/comments" repo number))
         (commit-sha (shipit--get-pr-head-sha number))
         (data (if line
                   `((body . ,body) (commit_id . ,commit-sha)
                     (path . ,file) (line . ,line) (side . ,(or side "RIGHT")))
                 `((body . ,body) (commit_id . ,commit-sha)
                   (path . ,file) (subject_type . "file")))))
    (shipit--debug-log "GitHub comment backend: adding inline comment on %s:%s for PR #%s"
                       file (or line "file-level") number)
    (shipit--api-request-post endpoint data)))

(defun shipit-comment-github--reply-to-comment (config number parent-id body &optional is-inline)
  "Reply to comment PARENT-ID on PR NUMBER with BODY using CONFIG.
IS-INLINE non-nil means this is a review comment reply (uses in_reply_to).
Otherwise it is a general comment reply (posts new issue comment)."
  (let* ((repo (plist-get config :repo)))
    (if is-inline
        ;; Inline review comment: use PR comments endpoint with in_reply_to
        (let ((endpoint (format "/repos/%s/pulls/%s/comments" repo number))
              (data `((body . ,body) (in_reply_to . ,parent-id))))
          (shipit--debug-log "GitHub comment backend: replying to inline comment %s on PR #%s" parent-id number)
          (shipit--api-request-post endpoint data))
      ;; General comment: post new issue comment (no threading support)
      (let ((endpoint (format "/repos/%s/issues/%s/comments" repo number))
            (data `((body . ,body))))
        (shipit--debug-log "GitHub comment backend: replying to general comment %s on PR #%s" parent-id number)
        (shipit--api-request-post endpoint data)))))

(defun shipit-comment-github--edit-comment (config comment-id body &optional is-inline pr-number)
  "Edit comment COMMENT-ID to have BODY using CONFIG.
If PR-NUMBER is non-nil, uses pull request review endpoint.
If IS-INLINE, uses pull request review comment endpoint.
Otherwise, uses issues comments endpoint for general comments."
  (let* ((repo (plist-get config :repo))
         (endpoint (cond
                    (pr-number (format "/repos/%s/pulls/%s/reviews/%s" repo pr-number comment-id))
                    (is-inline (format "/repos/%s/pulls/comments/%s" repo comment-id))
                    (t (format "/repos/%s/issues/comments/%s" repo comment-id))))
         (data `((body . ,body))))
    (shipit--debug-log "GitHub comment backend: editing %s comment %s"
                       (cond (pr-number "review") (is-inline "inline") (t "general")) comment-id)
    (shipit--api-request-post endpoint data "PATCH")))

(defun shipit-comment-github--delete-comment (config comment-id &optional is-inline)
  "Delete comment COMMENT-ID using CONFIG.
If IS-INLINE, uses pull request review comment endpoint.
Signals error on non-200/204 response.  Returns t on success."
  (let* ((repo (plist-get config :repo))
         (endpoint (if is-inline
                       (format "/repos/%s/pulls/comments/%s" repo comment-id)
                     (format "/repos/%s/issues/comments/%s" repo comment-id)))
         (url (concat shipit-api-url endpoint))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "GitHub comment backend: deleting %s comment %s"
                       (if is-inline "inline" "general") comment-id)
    (let ((status-code (shipit--url-retrieve-sync url "DELETE" headers nil t)))
      (unless (and (numberp status-code) (memq status-code '(200 204)))
        (error "GitHub delete comment failed with status %s" status-code))
      t)))

(defun shipit-comment-github--toggle-reaction (config comment-id reaction &optional is-inline)
  "Toggle REACTION on comment COMMENT-ID using CONFIG.
If IS-INLINE, uses pull request review comment endpoint."
  (let* ((repo (plist-get config :repo))
         (endpoint (if is-inline
                       (format "/repos/%s/pulls/comments/%s/reactions" repo comment-id)
                     (format "/repos/%s/issues/comments/%s/reactions" repo comment-id)))
         (data `((content . ,reaction))))
    (shipit--debug-log "GitHub comment backend: toggling reaction %s on comment %s" reaction comment-id)
    (shipit--api-request-post endpoint data)))

(defun shipit-comment-github--fetch-reactions (config comment-id &optional is-inline)
  "Fetch reactions for comment COMMENT-ID using CONFIG.
If IS-INLINE, uses pull request review comment endpoint."
  (let* ((repo (plist-get config :repo))
         (endpoint (if is-inline
                       (format "/repos/%s/pulls/comments/%s/reactions" repo comment-id)
                     (format "/repos/%s/issues/comments/%s/reactions" repo comment-id))))
    (shipit--debug-log "GitHub comment backend: fetching reactions for comment %s" comment-id)
    (shipit--api-request-paginated endpoint)))

;;; Async operations

(defun shipit-comment-github--fetch-general-comments-async (config pr-number callback)
  "Fetch general comments asynchronously for PR-NUMBER using CONFIG.
Delegates to `shipit--fetch-general-comments-async-github' for
async pagination of issue comments and reviews."
  (let ((repo (plist-get config :repo)))
    (shipit--fetch-general-comments-async-github repo pr-number callback)))

;;; Batch operations

(defun shipit-comment-github--handle-reaction-page
    (_status comment-id cache-key base-endpoint page results)
  "Handle a single reaction page, fetching more pages if needed.
Appends reactions to the cache and recursively fetches the next page
when the current page is full.  Marks the comment as done in RESULTS
when all pages are fetched."
  (condition-case err
      (progn
        (goto-char (point-min))
        (re-search-forward "\n\n")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (reactions (json-read)))
          ;; Page 1: replace cache; page 2+: append (pagination)
          (if (= page 1)
              (puthash cache-key (or reactions '()) shipit--reaction-cache)
            (let ((existing (gethash cache-key shipit--reaction-cache)))
              (puthash cache-key (append (or existing '()) (or reactions '()))
                       shipit--reaction-cache)))
          ;; Stop after page 1: display counts come from the comment's
          ;; `reactions' summary object, not the enumerated list. The
          ;; first-page sample is only used for tooltip reactor names.
          (puthash comment-id t results)))
    (error
     (shipit--debug-log "[reactions-batch] Failed page %d for comment %s: %s"
                        page comment-id (error-message-string err))
     (unless (gethash cache-key shipit--reaction-cache)
       (puthash cache-key '() shipit--reaction-cache))
     (puthash comment-id t results))))

(defun shipit-comment-github--fetch-reactions-batch (config comments is-inline &optional done-callback)
  "Fetch reactions for COMMENTS in parallel and cache them.
CONFIG provides :repo.  IS-INLINE selects the reaction endpoint type.
Uses `url-retrieve' with ETag caching for fast parallel fetching.
Skips review comments (they use GraphQL).

When DONE-CALLBACK is nil, blocks until all responses arrive (10s
timeout), preserving existing behavior for callers that render
immediately afterwards.  When DONE-CALLBACK is non-nil, dispatches
all requests and returns immediately; DONE-CALLBACK is invoked once
all responses have arrived (no UI thread blocking)."
  (let* ((repo (plist-get config :repo))
         (start-time (float-time))
         (pending-requests nil)
         (results (make-hash-table :test 'equal))
         (system-time-locale "C")
         (url-request-method "GET")
         (url-request-extra-headers
          (list '("User-Agent" . "emacs-url")
                '("Accept" . "application/vnd.github+json")
                `("Authorization" . ,(format "Bearer %s" (shipit--github-token)))))
         (url-automatic-caching t)
         (url-cache-directory (expand-file-name "url/cache" user-emacs-directory)))

    (shipit--debug-log "[reactions-batch] Launching %d parallel requests%s"
                       (length comments)
                       (if done-callback " (async)" ""))

    (dolist (comment comments)
      (let* ((comment-id (cdr (assq 'id comment)))
             (is-review (equal (cdr (assq 'shipit-comment-type comment)) "review"))
             (endpoint (if is-inline
                          (format "%s/repos/%s/pulls/comments/%s/reactions?per_page=100"
                                  shipit-api-url repo comment-id)
                        (format "%s/repos/%s/issues/comments/%s/reactions?per_page=100"
                                shipit-api-url repo comment-id)))
             (cache-key (shipit--reaction-cache-key repo comment-id is-inline)))
        (when (and comment-id (not is-review))
          (push
           (cons comment-id
                 (url-retrieve
                  endpoint
                  #'shipit-comment-github--handle-reaction-page
                  (list comment-id cache-key endpoint 1 results)
                  t t))
           pending-requests))))

    (cond
     (done-callback
      ;; Async path: schedule a poll-timer that fires DONE-CALLBACK once
      ;; results matches dispatched count (or after 10s safety timeout).
      ;; Polling via run-at-time avoids blocking the UI thread.
      (let* ((expected (length pending-requests))
             (deadline (+ (float-time) 10.0))
             (poll-fn (make-symbol "shipit-reactions-batch-poll")))
        (fset poll-fn
              (lambda ()
                (cond
                 ((>= (hash-table-count results) expected)
                  (shipit--debug-log "[reactions-batch] async complete %d/%d in %.3fs"
                                     (hash-table-count results) expected
                                     (- (float-time) start-time))
                  (funcall done-callback))
                 ((>= (float-time) deadline)
                  (shipit--debug-log "[reactions-batch] async timed out %d/%d after %.3fs"
                                     (hash-table-count results) expected
                                     (- (float-time) start-time))
                  (funcall done-callback))
                 (t
                  (run-at-time 0.1 nil poll-fn)))))
        (run-at-time 0.1 nil poll-fn)))
     (t
      ;; Sync path: block until all responses arrive (10s timeout).
      (let ((wait-start (float-time)))
        (while (and (< (- (float-time) wait-start) 10.0)
                    (< (hash-table-count results) (length pending-requests)))
          (accept-process-output nil 0.1)))
      (shipit--debug-log "[reactions-batch] Completed %d/%d in %.3fs"
                         (hash-table-count results) (length pending-requests)
                         (- (float-time) start-time))))))

;;; Optional operations

(defun shipit-comment-github--delete-reaction (config comment-id reaction-id &optional is-inline)
  "Delete reaction REACTION-ID from comment COMMENT-ID using CONFIG.
If IS-INLINE, uses pull request review comment endpoint.
Signals error on non-200/204 response.  Returns t on success."
  (let* ((repo (plist-get config :repo))
         (base (if is-inline
                   (format "/repos/%s/pulls/comments/%s/reactions" repo comment-id)
                 (format "/repos/%s/issues/comments/%s/reactions" repo comment-id)))
         (endpoint (format "%s/%s" base reaction-id))
         (url (concat shipit-api-url endpoint))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "GitHub comment backend: deleting reaction %s from comment %s" reaction-id comment-id)
    (let ((status-code (shipit--url-retrieve-sync url "DELETE" headers nil t)))
      (unless (and (numberp status-code) (memq status-code '(200 204)))
        (error "GitHub delete reaction failed with status %s" status-code))
      t)))

(defun shipit-comment-github--add-review-reaction (config node-id reaction)
  "Add REACTION to review NODE-ID using GraphQL via CONFIG."
  (shipit--debug-log "GitHub comment backend: adding review reaction %s to %s" reaction node-id)
  (shipit--add-review-reaction-graphql node-id reaction))

(defun shipit-comment-github--remove-review-reaction (config node-id reaction)
  "Remove REACTION from review NODE-ID using GraphQL via CONFIG."
  (shipit--debug-log "GitHub comment backend: removing review reaction %s from %s" reaction node-id)
  (shipit--remove-review-reaction-graphql node-id reaction))

(defun shipit-comment-github--fetch-review-reactions (config node-id)
  "Fetch reactions for review NODE-ID using GraphQL via CONFIG."
  (shipit--debug-log "GitHub comment backend: fetching review reactions for %s" node-id)
  (shipit--fetch-review-reactions-graphql node-id))

(defun shipit-comment-github--reply-to-inline (config number parent-id body file-path)
  "Reply to inline comment PARENT-ID on PR NUMBER with BODY using CONFIG.
FILE-PATH is the file containing the inline comment."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/comments" repo number))
         (data `((body . ,body) (in_reply_to . ,parent-id))))
    (shipit--debug-log "GitHub comment backend: replying to inline comment %s on %s"
                       parent-id file-path)
    (shipit--api-request-post endpoint data)))

(defun shipit-comment-github--post-add-comment (config comment-id is-inline repo pr-number)
  "Invalidate ETag caches and pre-fetch reactions after adding a comment.
COMMENT-ID is the new comment's ID.  IS-INLINE distinguishes inline vs general.
REPO and PR-NUMBER identify the PR."
  ;; Invalidate ETag cache for comments endpoint
  (let ((list-endpoint (if is-inline
                           (format "/repos/%s/pulls/%s/comments" repo pr-number)
                         (format "/repos/%s/issues/%s/comments" repo pr-number))))
    (when (fboundp 'shipit-gh-etag-invalidate)
      (shipit-gh-etag-invalidate list-endpoint '((per_page . 100))))
    (when (fboundp 'shipit-gh-etag--invalidate-refresh-cache)
      (shipit-gh-etag--invalidate-refresh-cache list-endpoint '((per_page . 100)))))
  ;; Pre-fetch reactions for the new comment to optimise next refresh
  (when comment-id
    (shipit--debug-log "🔄 POST-ADD-COMMENT: Pre-fetching reactions for comment %s" comment-id)
    (condition-case err
        (let* ((endpoint (if is-inline
                             (format "/repos/%s/pulls/comments/%s/reactions" repo comment-id)
                           (format "/repos/%s/issues/comments/%s/reactions" repo comment-id)))
               (cache-key (shipit--reaction-cache-key repo comment-id is-inline))
               (etag-result (shipit-gh-etag-get-json endpoint nil shipit-github-token t))
               (reactions (when etag-result (plist-get etag-result :json))))
          (puthash cache-key (or reactions '()) shipit--reaction-cache)
          (shipit--debug-log "🔄 POST-ADD-COMMENT: Cached %d reactions" (length (or reactions '()))))
      (error
       (shipit--debug-log "⚠️ POST-ADD-COMMENT: Failed to pre-fetch reactions: %s" (error-message-string err))))))

(defun shipit-comment-github--post-edit-comment (config comment-id is-inline repo pr-number)
  "Invalidate ETag caches after editing a comment.
COMMENT-ID is the edited comment's ID.  IS-INLINE distinguishes inline vs general.
REPO and PR-NUMBER identify the PR."
  ;; Invalidate the specific comment endpoint
  (let ((endpoint (if is-inline
                      (format "/repos/%s/pulls/comments/%s" repo comment-id)
                    (format "/repos/%s/issues/comments/%s" repo comment-id))))
    (when (fboundp 'shipit-clear-etag-cache-for-endpoint)
      (shipit-clear-etag-cache-for-endpoint endpoint)))
  ;; Invalidate list endpoints
  (when pr-number
    (let ((list-endpoint (if is-inline
                             (format "/repos/%s/pulls/%s/comments" repo pr-number)
                           (format "/repos/%s/issues/%s/comments" repo pr-number))))
      (when (fboundp 'shipit-gh-etag-invalidate)
        (shipit-gh-etag-invalidate list-endpoint '((per_page . 100))))
      (when (fboundp 'shipit-gh-etag--invalidate-refresh-cache)
        (shipit-gh-etag--invalidate-refresh-cache list-endpoint '((per_page . 100)))))))

(defun shipit-comment-github--post-delete-comment (_config comment-id is-inline repo pr-number)
  "Invalidate ETag caches after deleting a comment.
COMMENT-ID is the deleted comment's ID.  IS-INLINE distinguishes inline vs general.
REPO and PR-NUMBER identify the PR."
  (when pr-number
    (let ((list-endpoint (if is-inline
                             (format "/repos/%s/pulls/%s/comments" repo pr-number)
                           (format "/repos/%s/issues/%s/comments" repo pr-number))))
      (when (fboundp 'shipit-gh-etag-invalidate)
        (shipit-gh-etag-invalidate list-endpoint '((per_page . 100))))
      (when (fboundp 'shipit-gh-etag--invalidate-refresh-cache)
        (shipit-gh-etag--invalidate-refresh-cache list-endpoint '((per_page . 100)))))))

;; Register the GitHub comment backend
(shipit-comment-register-backend
 'github
 (list :name "GitHub"
       :fetch-general-comments #'shipit-comment-github--fetch-general-comments
       :fetch-inline-comments #'shipit-comment-github--fetch-inline-comments
       :add-general-comment #'shipit-comment-github--add-general-comment
       :add-inline-comment #'shipit-comment-github--add-inline-comment
       :reply-to-comment #'shipit-comment-github--reply-to-comment
       :edit-comment #'shipit-comment-github--edit-comment
       :delete-comment #'shipit-comment-github--delete-comment
       :toggle-reaction #'shipit-comment-github--toggle-reaction
       :fetch-reactions #'shipit-comment-github--fetch-reactions
       :fetch-general-comments-async #'shipit-comment-github--fetch-general-comments-async
       :fetch-reactions-batch #'shipit-comment-github--fetch-reactions-batch
       :reply-to-inline #'shipit-comment-github--reply-to-inline
       :delete-reaction #'shipit-comment-github--delete-reaction
       :add-review-reaction #'shipit-comment-github--add-review-reaction
       :remove-review-reaction #'shipit-comment-github--remove-review-reaction
       :fetch-review-reactions #'shipit-comment-github--fetch-review-reactions
       :post-add-comment #'shipit-comment-github--post-add-comment
       :post-edit-comment #'shipit-comment-github--post-edit-comment
       :post-delete-comment #'shipit-comment-github--post-delete-comment))

(provide 'shipit-comment-github)
;;; shipit-comment-github.el ends here
