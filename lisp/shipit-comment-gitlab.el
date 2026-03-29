;;; shipit-comment-gitlab.el --- GitLab MR comment backend -*- lexical-binding: t; -*-

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
;; GitLab MR comment backend for the pluggable comment system.
;; Covers general notes and inline (diff) comments on merge requests.
;; Uses shipit-gitlab-http.el for API plumbing.

;;; Code:

(require 'seq)
(require 'shipit-core)
(require 'shipit-comment-backends)
(require 'shipit-gitlab-http)

;;; Emoji name mapping

(defconst shipit-comment-gitlab--emoji-to-content
  '(("thumbsup" . "+1")
    ("thumbsdown" . "-1")
    ("laughing" . "laugh")
    ("confused" . "confused")
    ("heart" . "heart")
    ("tada" . "hooray")
    ("rocket" . "rocket")
    ("eyes" . "eyes"))
  "Map GitLab award_emoji names to shipit-standard reaction content types.")

(defun shipit-comment-gitlab--normalize-reaction (emoji)
  "Normalize a GitLab award_emoji EMOJI to shipit reaction format.
Maps name to content and user.username to user.login."
  (let* ((gl-name (cdr (assq 'name emoji)))
         (content (or (cdr (assoc gl-name shipit-comment-gitlab--emoji-to-content))
                      gl-name))
         (gl-user (cdr (assq 'user emoji))))
    `((id . ,(cdr (assq 'id emoji)))
      (content . ,content)
      (user . ((login . ,(cdr (assq 'username gl-user))))))))

;;; Shared discussions cache

(defvar-local shipit-comment-gitlab--discussions-cache nil
  "Cached raw discussions for the current MR. Cleared on refresh.")

(defun shipit-comment-gitlab--fetch-discussions (config number)
  "Fetch discussions for MR NUMBER using CONFIG, with caching.
Returns cached result on subsequent calls.  Call
`shipit-comment-gitlab--clear-discussions-cache' to invalidate."
  (or shipit-comment-gitlab--discussions-cache
      (let* ((path (format "%s/discussions?per_page=100"
                           (shipit-comment-gitlab--build-mr-path config number)))
             (raw (shipit-gitlab--api-request-paginated config path)))
        (setq shipit-comment-gitlab--discussions-cache (or raw '())))))

(defun shipit-comment-gitlab--clear-discussions-cache ()
  "Clear the discussions cache so next fetch hits the API."
  (setq shipit-comment-gitlab--discussions-cache nil))

;;; Helpers

(defun shipit-comment-gitlab--build-mr-path (config number)
  "Build API path prefix for MR NUMBER using CONFIG."
  (format "/projects/%s/merge_requests/%s"
          (shipit-gitlab--project-path config) number))

(defun shipit-comment-gitlab--system-note-p (note)
  "Return non-nil if NOTE is a GitLab system note."
  (let ((system (cdr (assq 'system note))))
    (and system (not (eq system :json-false)))))

;;; Normalization

(defun shipit-comment-gitlab--normalize-note (note &optional in-reply-to-id discussion-id)
  "Normalize a GitLab MR NOTE to shipit comment format.
IN-REPLY-TO-ID is nil for root notes, or the parent note ID for replies.
DISCUSSION-ID is the GitLab discussion ID (needed for reply endpoint)."
  (let ((author (cdr (assq 'author note))))
    `((id . ,(cdr (assq 'id note)))
      (body . ,(cdr (assq 'body note)))
      (user . ((login . ,(cdr (assq 'username author)))
               (avatar_url . ,(cdr (assq 'avatar_url author)))))
      (created_at . ,(cdr (assq 'created_at note)))
      (updated_at . ,(cdr (assq 'updated_at note)))
      (in_reply_to_id . ,in-reply-to-id)
      (discussion_id . ,discussion-id)
      (shipit-comment-type . "issue"))))

(defun shipit-comment-gitlab--normalize-diff-note (note in-reply-to-id &optional discussion-id)
  "Normalize a GitLab diff NOTE to shipit inline comment format.
IN-REPLY-TO-ID is nil for root notes, or the parent note ID for replies.
DISCUSSION-ID is the GitLab discussion ID (needed for reply endpoint)."
  (let* ((author (cdr (assq 'author note)))
         (position (cdr (assq 'position note)))
         (new-line (cdr (assq 'new_line position)))
         (old-line (cdr (assq 'old_line position)))
         (new-path (cdr (assq 'new_path position)))
         (old-path (cdr (assq 'old_path position))))
    `((id . ,(cdr (assq 'id note)))
      (body . ,(cdr (assq 'body note)))
      (user . ((login . ,(cdr (assq 'username author)))
               (avatar_url . ,(cdr (assq 'avatar_url author)))))
      (created_at . ,(cdr (assq 'created_at note)))
      (updated_at . ,(cdr (assq 'updated_at note)))
      (path . ,(or new-path old-path))
      (line . ,(or new-line old-line))
      (original_line . ,(or new-line old-line))
      (side . ,(if new-line "RIGHT" "LEFT"))
      (diff_hunk . "")
      (in_reply_to_id . ,in-reply-to-id)
      (discussion_id . ,discussion-id)
      (pull_request_review_id . ,in-reply-to-id))))

;;; Required operations

(defun shipit-comment-gitlab--general-discussion-p (discussion)
  "Return non-nil if DISCUSSION is a general (non-inline) discussion.
A discussion is general if its first non-system note has no position field.
Discussions where the first non-system note has a position are inline."
  (let ((notes (append (cdr (assq 'notes discussion)) nil)))
    (catch 'found
      (dolist (note notes)
        (unless (shipit-comment-gitlab--system-note-p note)
          (throw 'found (not (cdr (assq 'position note))))))
      nil)))

(defun shipit-comment-gitlab--fetch-general-comments (config number)
  "Fetch general comments for MR NUMBER using CONFIG.
Uses the shared discussions cache to avoid duplicate API calls.
Filters out inline discussions and system notes."
  (let* ((discussions (shipit-comment-gitlab--fetch-discussions config number))
         (general-comments '()))
    (dolist (discussion discussions)
      (when (shipit-comment-gitlab--general-discussion-p discussion)
        (let* ((disc-id (cdr (assq 'id discussion)))
               (notes (append (cdr (assq 'notes discussion)) nil))
               (first-note-id nil))
          (dolist (note notes)
            (unless (shipit-comment-gitlab--system-note-p note)
              (let ((note-id (cdr (assq 'id note)))
                    (reply-to (when first-note-id first-note-id)))
                (unless first-note-id
                  (setq first-note-id note-id))
                (push (shipit-comment-gitlab--normalize-note note reply-to disc-id)
                      general-comments)))))))
    (shipit--debug-log "GitLab comment: fetched %d general comments from %d discussions for MR !%s"
                       (length general-comments) (length discussions) number)
    (nreverse general-comments)))

(defun shipit-comment-gitlab--fetch-inline-comments (config number)
  "Fetch inline (diff) comments for MR NUMBER using CONFIG.
Uses the shared discussions cache to avoid duplicate API calls.
Extracts notes with diff positions."
  (let* ((discussions (shipit-comment-gitlab--fetch-discussions config number))
         (inline-comments '()))
    (dolist (discussion discussions)
      (let* ((disc-id (cdr (assq 'id discussion)))
             (notes (append (cdr (assq 'notes discussion)) nil))
             (first-note-id nil))
        (dolist (note notes)
          (when (and (cdr (assq 'position note))
                     (not (shipit-comment-gitlab--system-note-p note)))
            (let* ((note-id (cdr (assq 'id note)))
                   ;; First diff note in discussion is the root (no in_reply_to_id).
                   ;; Subsequent notes reply to the first note's id.
                   (reply-to (when first-note-id first-note-id)))
              (unless first-note-id
                (setq first-note-id note-id))
              (push (shipit-comment-gitlab--normalize-diff-note note reply-to disc-id)
                    inline-comments))))))
    (shipit--debug-log "GitLab comment: fetched %d inline comments for MR !%s"
                       (length inline-comments) number)
    (nreverse inline-comments)))

(defun shipit-comment-gitlab--add-general-comment (config number body)
  "Add general comment BODY to MR NUMBER using CONFIG."
  (let* ((path (format "%s/notes"
                       (shipit-comment-gitlab--build-mr-path config number)))
         (data `((body . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab comment: added general comment to MR !%s" number)
    (when raw
      (shipit-comment-gitlab--normalize-note raw))))

(defun shipit-comment-gitlab--fetch-diff-refs (config number)
  "Fetch diff_refs for MR NUMBER using CONFIG.
Returns alist with base_sha, head_sha, start_sha."
  (let* ((mr-path (shipit-comment-gitlab--build-mr-path config number))
         (mr-data (shipit-gitlab--api-request config mr-path)))
    (cdr (assq 'diff_refs mr-data))))

(defun shipit-comment-gitlab--add-inline-comment (config number file line body side &optional old-line)
  "Add inline comment to MR NUMBER on FILE at LINE using CONFIG.
BODY is the comment text.  SIDE is \"RIGHT\", \"LEFT\", or \"CONTEXT\".
OLD-LINE is the old-version line number (for context/deleted lines)."
  (let* ((diff-refs (shipit-comment-gitlab--fetch-diff-refs config number))
         (base-sha (cdr (assq 'base_sha diff-refs)))
         (head-sha (cdr (assq 'head_sha diff-refs)))
         (start-sha (cdr (assq 'start_sha diff-refs))))
    (unless (and base-sha head-sha start-sha)
      (error "GitLab: could not fetch diff_refs for MR !%s" number))
    (let* ((path (format "%s/discussions"
                         (shipit-comment-gitlab--build-mr-path config number)))
           (line-fields (cond
                         ;; Deleted line: old_line only
                         ((equal side "LEFT")
                          `((old_line . ,(or old-line line))))
                         ;; Context (unchanged) line: both old and new
                         ((equal side "CONTEXT")
                          `((new_line . ,line)
                            (old_line . ,(or old-line line))))
                         ;; Added line (RIGHT): new_line only
                         (t
                          `((new_line . ,line)))))
           (position `((position_type . "text")
                       (base_sha . ,base-sha)
                       (head_sha . ,head-sha)
                       (start_sha . ,start-sha)
                       (new_path . ,file)
                       (old_path . ,file)
                       ,@line-fields))
           (data `((body . ,body) (position . ,position)))
           (raw (shipit-gitlab--api-request-method config path data "POST")))
      (if raw
          (let* ((notes (append (cdr (assq 'notes raw)) nil))
                 (first-note (car notes))
                 (disc-id (cdr (assq 'id raw))))
            (shipit--debug-log "GitLab comment: added inline comment on %s:%s for MR !%s"
                               file (or line "file-level") number)
            (when first-note
              ;; First note in new discussion has no parent — pass nil for in-reply-to
              (shipit-comment-gitlab--normalize-diff-note first-note nil disc-id)))
        (error "GitLab: failed to create inline comment on %s:%s for MR !%s"
               file (or line "file-level") number)))))

(defun shipit-comment-gitlab--reply-to-comment (config number parent-id body &optional _is-inline)
  "Reply to discussion PARENT-ID on MR NUMBER with BODY using CONFIG."
  (let* ((path (format "%s/discussions/%s/notes"
                       (shipit-comment-gitlab--build-mr-path config number)
                       parent-id))
         (data `((body . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab comment: replied to discussion %s on MR !%s"
                       parent-id number)
    (when raw
      (shipit-comment-gitlab--normalize-note raw))))

(defun shipit-comment-gitlab--edit-comment (config comment-id body &optional _is-inline pr-number)
  "Edit note COMMENT-ID to have BODY using CONFIG.
PR-NUMBER is required to construct the API path."
  (unless pr-number
    (error "GitLab edit-comment requires pr-number"))
  (let* ((path (format "%s/notes/%s"
                       (shipit-comment-gitlab--build-mr-path config pr-number)
                       comment-id))
         (data `((body . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "PUT")))
    (shipit--debug-log "GitLab comment: edited note %s on MR !%s" comment-id pr-number)
    (when raw
      (shipit-comment-gitlab--normalize-note raw))))

(defun shipit-comment-gitlab--current-mr-number ()
  "Return the MR number from the currently displayed PR context."
  (or (car-safe shipit--current-displayed-pr)
      (error "GitLab comment operation requires an active MR context")))

(defun shipit-comment-gitlab--delete-comment (config comment-id &optional _is-inline)
  "Delete note COMMENT-ID using CONFIG."
  (let* ((pr-number (shipit-comment-gitlab--current-mr-number))
         (path (format "%s/notes/%s"
                       (shipit-comment-gitlab--build-mr-path config pr-number)
                       comment-id)))
    (shipit--debug-log "GitLab comment: deleting note %s from MR !%s"
                       comment-id pr-number)
    (shipit-gitlab--api-request-method config path nil "DELETE")
    t))

(defun shipit-comment-gitlab--toggle-reaction (config comment-id reaction &optional _is-inline)
  "Toggle REACTION on note COMMENT-ID using CONFIG."
  (let* ((pr-number (shipit-comment-gitlab--current-mr-number))
         (path (format "%s/notes/%s/award_emoji"
                       (shipit-comment-gitlab--build-mr-path config pr-number)
                       comment-id))
         (data `((name . ,reaction))))
    (shipit--debug-log "GitLab comment: toggling reaction %s on note %s"
                       reaction comment-id)
    (shipit-gitlab--api-request-method config path data "POST")))

(defun shipit-comment-gitlab--fetch-reactions (config comment-id &optional _is-inline)
  "Fetch reactions for note COMMENT-ID using CONFIG.
Returns normalized reactions with `content' and `user.login' keys."
  (let* ((pr-number (shipit-comment-gitlab--current-mr-number))
         (path (format "%s/notes/%s/award_emoji"
                       (shipit-comment-gitlab--build-mr-path config pr-number)
                       comment-id))
         (raw (shipit-gitlab--api-request config path))
         (emojis (append raw nil)))
    (shipit--debug-log "GitLab comment: fetched %d reactions for note %s"
                       (length emojis) comment-id)
    (mapcar #'shipit-comment-gitlab--normalize-reaction emojis)))

;;; Optional operations

(defun shipit-comment-gitlab--delete-reaction (config comment-id reaction-id &optional _is-inline)
  "Delete reaction REACTION-ID from note COMMENT-ID using CONFIG."
  (let* ((pr-number (shipit-comment-gitlab--current-mr-number))
         (path (format "%s/notes/%s/award_emoji/%s"
                       (shipit-comment-gitlab--build-mr-path config pr-number)
                       comment-id reaction-id)))
    (shipit--debug-log "GitLab comment: deleting reaction %s from note %s"
                       reaction-id comment-id)
    (shipit-gitlab--api-request-method config path nil "DELETE")
    t))

(defun shipit-comment-gitlab--fetch-reactions-batch (config comments is-inline)
  "Fetch and cache reactions for COMMENTS.
When few comments (<= 5), fetches reactions eagerly via per-note API.
For many comments, caches empty lists to avoid latency; reactions
are then fetched on-demand when the user interacts.
CONFIG, COMMENTS and IS-INLINE match the batch function signature."
  (let ((repo (plist-get config :repo)))
    (if (<= (length comments) 5)
        ;; Few comments: fetch eagerly
        (dolist (comment comments)
          (let* ((comment-id (cdr (assq 'id comment)))
                 (cache-key (shipit--reaction-cache-key repo comment-id is-inline)))
            (when comment-id
              (condition-case nil
                  (let ((reactions (shipit-comment-gitlab--fetch-reactions
                                   config comment-id is-inline)))
                    (puthash cache-key (or reactions '()) shipit--reaction-cache))
                (error
                 (puthash cache-key '() shipit--reaction-cache))))))
      ;; Many comments: defer to on-demand
      (dolist (comment comments)
        (let* ((comment-id (cdr (assq 'id comment)))
               (cache-key (shipit--reaction-cache-key repo comment-id is-inline)))
          (when comment-id
            (puthash cache-key '() shipit--reaction-cache)))))))

;;; Parity operations — reply-to-inline

(defun shipit-comment-gitlab--reply-to-inline (config number parent-id body _file-path)
  "Reply to inline discussion PARENT-ID on MR NUMBER with BODY.
PARENT-ID is the GitLab discussion ID.  FILE-PATH is unused
\(GitLab routes replies via discussion, not file path\).
Returns normalized note."
  (let* ((path (format "%s/discussions/%s/notes"
                       (shipit-comment-gitlab--build-mr-path config number)
                       parent-id))
         (data `((body . ,body)))
         (raw (shipit-gitlab--api-request-method config path data "POST")))
    (shipit--debug-log "GitLab comment: replied to inline discussion %s on MR !%s"
                       parent-id number)
    (when raw
      (shipit-comment-gitlab--normalize-note raw))))

;;; Post-comment hooks — cache invalidation after mutations

(defun shipit-comment-gitlab--post-add-comment (_config _comment-id _is-inline _repo _pr-number)
  "Clear discussions cache after adding a comment.
Called by the dispatcher after a successful add/reply operation."
  (shipit-comment-gitlab--clear-discussions-cache)
  (shipit--debug-log "GitLab comment: cleared discussions cache after add"))

(defun shipit-comment-gitlab--post-edit-comment (_config _comment-id _is-inline _repo _pr-number)
  "Clear discussions cache after editing a comment.
Called by the dispatcher after a successful edit operation."
  (shipit-comment-gitlab--clear-discussions-cache)
  (shipit--debug-log "GitLab comment: cleared discussions cache after edit"))

(defun shipit-comment-gitlab--post-delete-comment (_config _comment-id _is-inline _repo _pr-number)
  "Clear discussions cache after deleting a comment.
Called by the dispatcher after a successful delete operation."
  (shipit-comment-gitlab--clear-discussions-cache)
  (shipit--debug-log "GitLab comment: cleared discussions cache after delete"))

;;; Registration

(shipit-comment-register-backend
 'gitlab
 (list :name "GitLab"
       :inject-project-path t
       :fetch-general-comments #'shipit-comment-gitlab--fetch-general-comments
       :fetch-inline-comments #'shipit-comment-gitlab--fetch-inline-comments
       :add-general-comment #'shipit-comment-gitlab--add-general-comment
       :add-inline-comment #'shipit-comment-gitlab--add-inline-comment
       :reply-to-comment #'shipit-comment-gitlab--reply-to-comment
       :edit-comment #'shipit-comment-gitlab--edit-comment
       :delete-comment #'shipit-comment-gitlab--delete-comment
       :toggle-reaction #'shipit-comment-gitlab--toggle-reaction
       :fetch-reactions #'shipit-comment-gitlab--fetch-reactions
       :fetch-reactions-batch #'shipit-comment-gitlab--fetch-reactions-batch
       :delete-reaction #'shipit-comment-gitlab--delete-reaction
       :reply-to-inline #'shipit-comment-gitlab--reply-to-inline
       :clear-discussions-cache #'shipit-comment-gitlab--clear-discussions-cache
       :post-add-comment #'shipit-comment-gitlab--post-add-comment
       :post-edit-comment #'shipit-comment-gitlab--post-edit-comment
       :post-delete-comment #'shipit-comment-gitlab--post-delete-comment))

(provide 'shipit-comment-gitlab)
;;; shipit-comment-gitlab.el ends here
