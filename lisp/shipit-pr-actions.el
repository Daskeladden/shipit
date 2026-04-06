;;; shipit-pr-actions.el --- Interactive PR operations -*- lexical-binding: t; -*-

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

;;; This file is part of shipit — code review integration for Magit.

;;; Commentary:
;; Interactive PR operations: draft/close, description editing, approval,
;; assignees, reviewers, comments, DWIM dispatch, and refresh.
;; Extracted from shipit-magit.el.

;;; Code:
(require 'cl-lib)
(require 'shipit-lib)
(require 'shipit-core)
(require 'shipit-sections)
(require 'shipit-http)
(require 'shipit-pr-backends)
(require 'shipit-render)
(require 'transient)

(eval-when-compile
  (require 'magit-section))
(require 'magit-section)

;; Forward declarations
(declare-function shipit--debug-log "shipit-core")
(declare-function shipit-open-pr-buffer "shipit-buffer")
(declare-function shipit-buffer-refresh "shipit-buffer")
(declare-function shipit-editor-open "shipit-editor")
(declare-function shipit--edit-comment-interactive "shipit-commands")
(declare-function shipit-mark-activities-read "shipit-commands")
(declare-function shipit--display-inline-comments "shipit-pr-diff")
(declare-function shipit--show-pr-file-patch "shipit-pr-diff")
(declare-function shipit--open-pr-file-diff "shipit-pr-diff")
(declare-function shipit--open-pr-file-ediff "shipit-pr-diff")
(declare-function shipit--get-current-user-review-state "shipit-http")
(declare-function shipit--add-comment-to-pr "shipit-http")
(declare-function shipit--add-general-comment-to-pr "shipit-http")
(declare-function shipit--ensure-repository "shipit-core")
(declare-function shipit--remove-reaction-from-pr "shipit-http")
(declare-function shipit--add-reaction-to-pr "shipit-http")
(declare-function shipit--remove-reaction-from-comment "shipit-http")
(declare-function shipit--add-reaction-to-comment "shipit-http")
(declare-function shipit--user-has-reaction "shipit-http")
(declare-function shipit--in-general-comments-section-p "shipit-core")
(declare-function shipit--get-repo-root "shipit-worktree")
(declare-function shipit-comment--fetch-reactions "shipit-comments")
(declare-function shipit--insert-assignees-section "shipit-pr-sections")
(declare-function shipit--insert-files-content "shipit-pr-sections")
(declare-function shipit-preview--refresh-files-section-only "shipit-preview")
(declare-function shipit--find-labels-section "shipit-diff")
(declare-function shipit--refresh-labels-section-targeted "shipit-diff")
(declare-function shipit--browse-pr-url "shipit-notifications")
(declare-function shipit-actions-timestamps-menu "shipit-actions")
(declare-function shipit-actions-nav-menu "shipit-actions")

;; Forward declared variables
(defvar shipit-buffer-repo)
(defvar shipit-buffer-pr-data)
(defvar shipit-buffer-pr-number)
(defvar shipit--current-displayed-pr)
(defvar shipit--cached-inline-comments)
(defvar shipit--cached-general-comments)
(defvar shipit--inline-comments-fetched)
(defvar shipit--comment-cache)
(defvar shipit-current-repo)
(defvar shipit-reaction-choices)
(defvar shipit--explicit-pr-operation)
(defvar shipit-post-action-hook)

;;; Region A: Reactions/Comment dialog

(defun shipit--toggle-reaction-interactive (comment-id)
  "Interactively toggle a reaction on comment with COMMENT-ID or PR description.
If COMMENT-ID is nil, attempts to find it from text properties or operates on PR description."
  (interactive)
  ;; If comment-id not provided, try to get it from text properties
  (unless comment-id
    (setq comment-id (get-text-property (point) 'shipit-comment-id)))

  (let* ((choice (completing-read "Toggle reaction: " shipit-reaction-choices))
         (reaction-type (cdr (assoc choice shipit-reaction-choices))))
    (when reaction-type
      ;; Check if this is a PR description (no comment-id but has pr-number and pr-body)
      (if (and (not comment-id)
               (get-text-property (point) 'shipit-pr-description)
               (get-text-property (point) 'shipit-pr-number))
          ;; PR DESCRIPTION PATH - use PR reaction functions
          (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
                 ;; Get repo from buffer context first, fall back to git remote detection
                 (current-repo (or (get-text-property (point) 'shipit-repo)
                                   (when (boundp 'shipit-buffer-repo) shipit-buffer-repo)
                                   (cadr shipit--current-displayed-pr)
                                   (shipit--ensure-repository)))
                 (resolved (shipit-pr--resolve-for-repo current-repo))
                 (pr-backend (car resolved))
                 (pr-config (cdr resolved))
                 (reactions (funcall (plist-get pr-backend :fetch-reactions)
                                     pr-config pr-number))
                 (current-user (shipit--get-current-user))
                 (has-reaction (cl-find-if (lambda (r)
                                             (and (string= (cdr (assq 'content r)) reaction-type)
                                                  (string= (cdr (assq 'login (cdr (assq 'user r)))) current-user)))
                                           reactions)))
            (if has-reaction
                (shipit--remove-reaction-from-pr pr-number reaction-type)
              (shipit--add-reaction-to-pr pr-number reaction-type)))
        ;; COMMENT PATH - original logic for comments
        (let* ((in-general-section (shipit--in-general-comments-section-p))
               (is-inline (not in-general-section))
               ;; Get repo from buffer context first, fall back to git remote detection
               (current-repo (or (get-text-property (point) 'shipit-repo)
                                 (when (boundp 'shipit-buffer-repo) shipit-buffer-repo)
                                 (cadr shipit--current-displayed-pr)
                                 (shipit--ensure-repository)))
               (comment-obj (or (cl-find-if (lambda (c) (equal (cdr (assq 'id c)) comment-id))
                                            shipit--cached-general-comments)
                                (cl-find-if (lambda (c) (equal (cdr (assq 'id c)) comment-id))
                                            shipit--cached-inline-comments)))
               (pr-number (or (get-text-property (point) 'shipit-pr-number)
                              (when comment-obj (cdr (assq 'shipit-pr-number comment-obj)))
                              (when (boundp 'shipit-buffer-pr-number) shipit-buffer-pr-number)
                              (when (and shipit--current-displayed-pr (listp shipit--current-displayed-pr))
                                (cdr (assq 'number shipit--current-displayed-pr))))))
          (shipit-comment--fetch-reactions current-repo comment-id is-inline)
          (setq shipit-current-repo current-repo)
          (let ((has-reaction (shipit--user-has-reaction comment-id reaction-type is-inline)))
            (if has-reaction
                (shipit--remove-reaction-from-comment comment-id reaction-type current-repo pr-number is-inline comment-obj)
              (shipit--add-reaction-to-comment comment-id reaction-type current-repo pr-number is-inline comment-obj))))))))

(defun shipit--react-to-comment (comment-id)
  "Add a reaction to comment with COMMENT-ID."
  (shipit--toggle-reaction-interactive comment-id))


(defun shipit--simple-comment-dialog (pr-number repo)
  "Open editor for adding a comment to PR-NUMBER in REPO.
Uses the new shipit-editor with live preview and PR reference completion."
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (line-number (get-text-property (point) 'shipit-line-number))
         (old-line-number (get-text-property (point) 'shipit-old-line-number))
         (is-inline (and file-path (or line-number old-line-number)))
         (type (if is-inline 'inline-comment 'general-comment))
         (side (cond (old-line-number (if line-number "CONTEXT" "LEFT"))
                     (t "RIGHT")))
         ;; Capture current position for live preview
         (section-marker (point-marker)))
    (if (fboundp 'shipit-editor-open)
        (shipit-editor-open
         (list :type type
               :source-buffer (current-buffer)
               :pr-number pr-number
               :repo repo
               :file-path file-path
               :line-number (or line-number old-line-number)
               :old-line-number old-line-number
               :side side
               :section-marker section-marker))
      ;; Fallback to simple read-string if editor not loaded
      (let ((comment-text (read-string "Comment: ")))
        (when (and comment-text (not (string-empty-p comment-text)))
          (if is-inline
              (progn
                (shipit--add-comment-to-pr pr-number file-path
                                           (or line-number old-line-number)
                                           comment-text side nil old-line-number)
                (message "Comment added"))
            (progn
              (shipit--add-general-comment-to-pr pr-number comment-text)
              (message "Comment added"))))))))

;;; Region B: PR context helpers

(defun shipit--get-pr-description-at-point ()
  "Get PR description information at point."
  (let ((pr-number (get-text-property (point) 'shipit-pr-number))
        (pr-body (get-text-property (point) 'shipit-pr-body)))
    (when pr-number
      (list pr-number pr-body))))

(defun shipit--get-pr-header-at-point ()
  "Get PR header information at point for review."
  (let ((pr-number (get-text-property (point) 'shipit-pr-number)))
    (when pr-number
      pr-number)))

(defun shipit--get-contextual-review-modes (pr-number)
  "Get contextual review modes based on current user's review state for PR-NUMBER."
  (let ((review-state (shipit--get-current-user-review-state pr-number)))
    (pcase (plist-get review-state :state)
      ('approved '(review-undo-approve review-reject))  ; If approved, offer undo or change to reject
      ('changes_requested '(review-approve review-undo-reject))  ; If rejected, offer approve or undo
      ('commented '(review-approve review-reject review-dismiss))  ; If only commented, offer approve/reject/dismiss
      (_ '(review-approve review-reject)))))  ; No review yet, offer approve/reject

;;; Region C: Refresh and context detection

(defvar-local shipit--current-operation-comment-id nil
  "Comment ID for the current operation, used for targeted refresh.")

(defun shipit--refresh-specific-comment (comment-id)
  "Refresh only the specific comment with COMMENT-ID in diff buffer.
Returns t if successful, nil if comment not found or refresh failed."
  (when (and comment-id (derived-mode-p 'magit-diff-mode 'magit-revision-mode))
    (save-excursion
      (goto-char (point-min))
      ;; Find the comment by searching for its ID in text properties
      (let ((found-pos nil)
            (search-pos (point-min)))
        (while (and (not found-pos) (< search-pos (point-max)))
          (when (equal (get-text-property search-pos 'shipit-comment-id) comment-id)
            (setq found-pos search-pos))
          (setq search-pos (next-single-property-change search-pos 'shipit-comment-id nil (point-max))))

        (when found-pos
          (goto-char found-pos)
          ;; Try to find the magit section containing this comment
          (let ((section (when (fboundp 'magit-current-section) (magit-current-section))))
            (if (and section (eq (oref section type) 'shipit-comment))
                ;; TODO: Fetch updated comment data and replace section content
                ;; For now, return nil to trigger fallback
                nil
              ;; TODO: Handle simple comment insertion refresh
              nil)))))))

(defun shipit--refresh-after-comment-operation (is-inline-comment)
  "Unified refresh function for add/edit/delete comment operations.
IS-INLINE-COMMENT indicates if this was an inline comment operation."
  (when is-inline-comment
    ;; Clear cached inline comments to fetch fresh data
    (setq shipit--cached-inline-comments nil
          shipit--inline-comments-fetched nil)
    ;; Clear the per-file comment cache for all files in this PR
    (let* ((repo (shipit--get-repo-from-remote))
           (pr-data (when (car-safe shipit--current-displayed-pr)
                      (shipit-get-pull-request (car shipit--current-displayed-pr))))
           (pr-number (when pr-data (cdr (assq 'number pr-data)))))
      (when (and repo pr-number)
        ;; Clear all cache entries for this PR
        (let ((keys-to-remove '()))
          (when (and shipit--comment-cache (hash-table-p shipit--comment-cache))
            (maphash (lambda (key _value)
                       (when (string-prefix-p (format "%s:" pr-number) key)
                         (push key keys-to-remove)))
                     shipit--comment-cache))
          (dolist (key keys-to-remove)
            (remhash key shipit--comment-cache))))))

  ;; Context-aware refresh logic - prioritize comment type over current buffer
  (cond
   ;; For inline comments in diff modes, try targeted refresh first
   ((and is-inline-comment (derived-mode-p 'magit-diff-mode 'magit-revision-mode))
    ;; Try to get the specific comment-id from the current operation context
    (let ((comment-id (or (and (boundp 'shipit--current-operation-comment-id) shipit--current-operation-comment-id)
                          ;; Fallback: try to get from text properties at point
                          (get-text-property (point) 'shipit-comment-id))))
      (if (and comment-id (fboundp 'shipit--refresh-specific-comment))
          (unless (shipit--refresh-specific-comment comment-id)
            ;; Fallback to full refresh if targeted fails
            (when (fboundp 'shipit--display-inline-comments)
              (shipit--display-inline-comments t)))
        ;; No comment-id available, use full refresh
        (when (fboundp 'shipit--display-inline-comments)
          (shipit--display-inline-comments t)))))
   ;; For inline comments elsewhere (including shipit-mode), refresh the buffer
   (is-inline-comment
    ;; Refresh shipit-mode buffer
    (when (derived-mode-p 'shipit-mode)
      (shipit-refresh)))
   ;; For general comments in shipit-mode, refresh the buffer
   ((derived-mode-p 'shipit-mode)
    (shipit-refresh))
   ;; If we're in a diff mode but not inline comment, still refresh inline comments
   ((derived-mode-p 'magit-diff-mode 'magit-revision-mode)
    ;; Force refresh of inline comments in the current diff buffer
    (when (fboundp 'shipit--display-inline-comments)
      (shipit--display-inline-comments t)))
   ;; Default fallback - no-op
   (t nil)))

(defun shipit--refresh-after-comment-operation-with-hook (is-inline-comment action context)
  "Wrapper for refresh that runs post-action hook after refresh completes.
IS-INLINE-COMMENT indicates if this was an inline comment operation.
ACTION and CONTEXT are passed to the post-action hook."
  (shipit--refresh-after-comment-operation is-inline-comment)
  ;; Run post-action hook after refresh, with delay to ensure inline comments are displayed
  (run-with-timer (if is-inline-comment 0.5 0.3) nil
                  (lambda ()
                    ;; Add is-inline-comment to context for proper buffer selection
                    (let ((enhanced-context (if context
                                                (cons `(is-inline-comment . ,is-inline-comment) context)
                                              `((is-inline-comment . ,is-inline-comment)))))
                      (run-hook-with-args 'shipit-post-action-hook action enhanced-context nil)))))


(defun shipit--in-pr-section-p ()
  "Return non-nil if point is inside any part of the shipit PR section area."
  (let ((text-props (or (get-text-property (point) 'shipit-pr-header)
                        (get-text-property (point) 'shipit-pr-description)
                        (get-text-property (point) 'shipit-pr-file)
                        (get-text-property (point) 'shipit-commit)
                        (get-text-property (point) 'shipit-file)
                        (get-text-property (point) 'shipit-comment)
                        (get-text-property (point) 'shipit-labels)
                        (get-text-property (point) 'shipit-assignees)
                        (get-text-property (point) 'shipit-reviewers)
                        (get-text-property (point) 'shipit-checks)
                        (get-text-property (point) 'shipit-inline-comments)
                        (get-text-property (point) 'shipit-general-comments)))
        (magit-section-match (shipit--in-magit-pr-section-p)))
    ;; Since shipit content is text-based with properties rather than proper magit sections,
    ;; we rely primarily on text properties but keep magit section detection for individual
    ;; sections like 'checks', 'labels' etc. that do exist as magit section objects
    (or text-props magit-section-match)))

(defun shipit--in-magit-pr-section-p ()
  "Return non-nil if point is inside any shipit magit section."
  (condition-case _err
      (when (fboundp 'magit-current-section)
        (let ((section (magit-current-section)))
          (when section
            ;; Use explicit section matching (most reliable approach)
            ;; NOTE: When adding new magit sections to shipit, add them to this list
            ;; so the context-aware 'q' key can detect them properly
            (let ((matches (or (magit-section-match '(shipit-pr) section)
                               (magit-section-match '(shipit-description) section)
                               (magit-section-match '(shipit-reviews) section)
                               (magit-section-match '(shipit-commit) section)
                               (magit-section-match '(pr-commits) section)
                               (magit-section-match '(pr-files) section)
                               (magit-section-match '(labels) section)
                               (magit-section-match '(assignees) section)
                               (magit-section-match '(reviewers) section)
                               (magit-section-match '(checks) section))))
              matches))))
    (error nil)))

;;; Region D: Main actions block

(defun shipit--toggle-draft-status ()
  "Toggle the draft status of the current PR."
  (interactive)
  (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (current-draft (get-text-property (point) 'shipit-draft-state))
         (new-draft (if (eq current-draft :json-false) :json-true :json-false))
         (action (if (eq current-draft :json-false) "mark as draft" "mark as ready")))
    (when (and pr-number repo)
      (if (yes-or-no-p (format "Do you want to %s PR #%s? " action pr-number))
          (progn
            (message "Updating PR draft status...")
            (shipit--update-pr-draft-status repo pr-number (eq new-draft :json-true))
            (message "Draft status updated. Refreshing...")
            (shipit-refresh))
        (message "Cancelled")))))

(defun shipit--update-pr-draft-status (repo pr-number draft)
  "Update the draft status of PR-NUMBER in REPO to DRAFT (boolean) using GraphQL."
  (condition-case err
      (let* ((pr-id (shipit--get-pr-id repo pr-number)))
        (if (not pr-id)
            (progn
              (message "Error: Could not get PR node ID")
              nil)
          (let* ((mutation (if draft
                               "mutation ConvertPullRequestToDraft($pullRequestId: ID!) {
  convertPullRequestToDraft(input: {pullRequestId: $pullRequestId}) {
    pullRequest {
      isDraft
      number
    }
  }
}"
                             "mutation MarkPullRequestReadyForReview($pullRequestId: ID!) {
  markPullRequestReadyForReview(input: {pullRequestId: $pullRequestId}) {
    pullRequest {
      isDraft
      number
    }
  }
}"))
                 (variables `((pullRequestId . ,pr-id)))
                 (response (shipit--graphql-request mutation variables)))
            (if (assq 'errors response)
                (progn
                  (message "Error updating draft status: %s" (cdr (assq 'errors response)))
                  nil)
              ;; Clear cache to force refresh of PR data
              (shipit--clear-branch-cache)
              response))))
    (error
     (message "Error updating draft status: %s" (error-message-string err))
     nil)))


(defun shipit--url-at-point ()
  "Return URL at point, or nil if none.
Checks for plain URLs and markdown-style [text](url) links.
Handles wrapped/indented text by searching nearby context."
  (or (thing-at-point 'url)
      ;; Check if we're inside a markdown link [text](url)
      ;; Search in a region around point to handle wrapped text
      (save-excursion
        (let* ((pos (point))
               ;; Search a reasonable range around point (handles wrapping)
               (search-start (max (point-min) (- pos 200)))
               (search-end (min (point-max) (+ pos 200))))
          (goto-char search-start)
          (catch 'found
            (while (re-search-forward "\\[\\([^]]*\\)\\](\\([^)]+\\))" search-end t)
              (when (and (<= (match-beginning 0) pos)
                         (>= (match-end 0) pos))
                (throw 'found (match-string 2))))
            nil)))))

(defun shipit--description-dwim-actions ()
  "Handle dwim actions for PR description section.
If point is on a URL, includes Open URL option in the action menu."
  (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (pr-body (get-text-property (point) 'shipit-pr-body))
         (can-edit (shipit--can-edit-pr-p pr-number repo))
         (url-at-point (shipit--url-at-point))
         (actions '()))

    ;; Add Open URL option if on a URL
    (when url-at-point
      (push "Open URL" actions))

    ;; Always add react option
    (push "React to description" actions)

    ;; Add edit option if it's the current user's PR
    (when can-edit
      (push "Edit description" actions))

    ;; Show action menu using completing-read
    (let ((choice (completing-read "Choose action: " actions nil t)))
      (cond
       ((string= choice "Open URL")
        (browse-url url-at-point))
       ((string= choice "React to description")
        (shipit--react-to-description))
       ((string= choice "Edit description")
        (shipit--edit-description))
       (t (message "No action selected"))))))

(defun shipit--react-to-description ()
  "Toggle reaction on PR description (uses shared toggle function)."
  ;; Use the shared toggle function with pr-number as identifier
  (shipit--toggle-reaction-interactive nil))

(defun shipit--can-edit-pr-p (pr-number repo)
  "Check if current user can edit this PR."
  (let* ((pr-data (shipit-get-pull-request pr-number repo))
         (pr-author-obj (cdr (assq 'user pr-data)))
         (pr-author (when pr-author-obj (cdr (assq 'login pr-author-obj))))
         (current-user (shipit--get-current-user)))
    (cond
     ;; PR author can always edit their own PR
     ((and pr-author current-user (string= pr-author current-user))
      t)
     ;; Repository admin/maintainer can edit any PR (could add this check later)
     ;; ((shipit--is-repo-admin-p current-user repo) t)
     ;; Others cannot edit
     (t nil))))

(defvar shipit--cached-current-user nil
  "Cached username to avoid repeated API calls.")

(defun shipit--get-current-user ()
  "Get current username from the active backend API with caching."
  (or shipit--cached-current-user
      (condition-case err
          (let* ((backend (shipit-pr--get-backend))
                 (fn (plist-get backend :get-current-username))
                 (config (or shipit-pr-backend-config (list)))
                 (username (when fn (funcall fn config))))
            (when username
              (setq shipit--cached-current-user username))
            username)
        (error
         (shipit--debug-log "Failed to get current user: %s"
                            (error-message-string err))
         nil))))

(defun shipit--close-pr (pr-number repo &optional comment)
  "Close PR-NUMBER in REPO, optionally adding COMMENT first.
Dispatches to the active PR backend's :update-pr and comment backend."
  (when comment
    (let* ((resolved (shipit-comment--resolve-for-repo repo))
           (cb (car resolved))
           (cc (cdr resolved))
           (add-fn (plist-get cb :add-general-comment)))
      (funcall add-fn cc pr-number comment)))
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (update-fn (plist-get backend :update-pr))
         (result (funcall update-fn config pr-number '((state . "closed")))))
    (if result
        (progn
          (message "PR #%d closed" pr-number)
          ;; Invalidate all caches for this PR endpoint (persistent + refresh)
          (let ((endpoint (format "/repos/%s/pulls/%d" repo pr-number)))
            (when (fboundp 'shipit-gh-etag-invalidate-endpoint)
              (shipit-gh-etag-invalidate-endpoint endpoint)))
          ;; Also clear repo-level ETag cache to be thorough
          (when (fboundp 'shipit-clear-etag-cache-for-repo)
            (shipit-clear-etag-cache-for-repo repo))
          ;; Clear url.el disk cache files for GitHub API
          (when (fboundp 'shipit-clear-stale-cache-files)
            (shipit-clear-stale-cache-files))
          ;; Refresh the display (only if in shipit buffer - may be called from editor)
          (when (and (fboundp 'shipit-buffer-refresh)
                     (derived-mode-p 'shipit-mode))
            (shipit-buffer-refresh)))
      (user-error "Failed to close PR #%d" pr-number))))

(defun shipit--pr-header-actions ()
  "Handle dwim actions for PR header section."
  (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (pr-data (shipit-get-pull-request pr-number repo))
         (pr-state (shipit--get-pr-actual-state pr-data))
         (pr-author-obj (cdr (assq 'user pr-data)))
         (pr-author (when pr-author-obj (cdr (assq 'login pr-author-obj))))
         (current-user (shipit--get-current-user))
         (is-author (and pr-author current-user (string= pr-author current-user)))
         (is-open (string= pr-state "open"))
         (actions '()))
    ;; Build action list based on context
    (push "Open on GitHub" actions)
    (push "Copy PR URL" actions)
    ;; Author-only options
    (when is-author
      (push "Edit title" actions))
    ;; Close option only for author and open PRs
    (when (and is-author is-open)
      (push "Close PR" actions)
      (push "Close PR with comment" actions))
    ;; Merge option for open PRs (available to all users, not just author)
    (when is-open
      (push "Merge" actions))
    ;; Show action menu
    (let ((choice (completing-read "PR action: " (nreverse actions) nil t)))
      (cond
       ((string= choice "Open on GitHub")
        (let ((url (cdr (assq 'html_url pr-data))))
          (if url
              (browse-url url)
            (user-error "No URL found for PR"))))
       ((string= choice "Copy PR URL")
        (let ((url (cdr (assq 'html_url pr-data))))
          (if url
              (progn
                (kill-new url)
                (message "Copied: %s" url))
            (user-error "No URL found for PR"))))
       ((string= choice "Edit title")
        (let* ((current-title (or (cdr (assq 'title pr-data)) ""))
               (new-title (read-string "New PR title: " current-title)))
          (unless (string= new-title current-title)
            (shipit--edit-pr-title pr-number new-title repo))))
       ((string= choice "Close PR")
        (when (yes-or-no-p (format "Close PR #%d? " pr-number))
          (shipit--close-pr pr-number repo)))
       ((string= choice "Close PR with comment")
        (shipit-editor-open
         (list :type 'close-pr-comment
               :source-buffer (current-buffer)
               :pr-number pr-number
               :repo repo)))
       ((string= choice "Merge")
        (shipit-merge))
       (t (message "No action selected"))))))

(defun shipit--edit-description ()
  "Edit PR description using the shipit editor.
Uses the new shipit-editor with live preview and PR reference completion."
  (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (current-body (or (get-text-property (point) 'shipit-pr-body) "")))
    (shipit--debug-log "EDIT-DESC: pr=%s repo=%s editor-fboundp=%s"
                       pr-number repo (fboundp 'shipit-editor-open))
    (if (fboundp 'shipit-editor-open)
        (shipit-editor-open
         (list :type 'description
               :source-buffer (current-buffer)
               :pr-number pr-number
               :repo repo
               :initial-content current-body))
      ;; Fallback to old mini-editor if editor not loaded
      (shipit--debug-log "EDIT-DESC: Falling back to mini-editor")
      (shipit--description-mini-editor pr-number repo current-body))))

(defun shipit--description-mini-editor (pr-number repo current-description)
  "Mini-editor for editing PR description."
  (let* ((buffer-name "*shipit-edit-description*")
         (buffer (get-buffer-create buffer-name)))

    ;; Set up the edit buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or current-description ""))
      (goto-char (point-min))
      (text-mode)

      ;; Add local keybindings
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "C-c C-c")
                     `(lambda ()
                        (interactive)
                        (shipit--save-description-edit ,pr-number ,repo)))
      (local-set-key (kbd "C-c C-k")
                     `(lambda ()
                        (interactive)
                        (kill-buffer ,buffer-name)))

      ;; Show helpful message
      (message "Edit description. Save: C-c C-c, Cancel: C-c C-k"))

    ;; Switch to edit buffer
    (switch-to-buffer buffer)))

(defun shipit--save-description-edit (pr-number repo)
  "Save the edited PR description."
  (let ((new-description (buffer-string)))
    (condition-case err
        (progn
          ;; Update PR description via GitHub API
          (shipit--update-pr-description pr-number repo new-description)
          (kill-buffer)
          (message "Description updated successfully")
          ;; Refresh the PR to show changes
          (shipit-refresh))
      (error
       (message "Failed to update description: %s" (error-message-string err))))))

(defun shipit--update-pr-description (pr-number repo new-description)
  "Update PR description via backend dispatch."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (update-fn (plist-get backend :update-pr)))
    (funcall update-fn config pr-number `((body . ,new-description)))))

(defun shipit--edit-pr-title (pr-number new-title &optional repo)
  "Update title of PR-NUMBER to NEW-TITLE via backend dispatch.
REPO defaults to the current displayed PR's repo."
  (let* ((repo (or repo (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (update-fn (plist-get backend :update-pr)))
    (funcall update-fn config pr-number `((title . ,new-title)))
    (message "PR title updated")
    (shipit--clear-branch-cache)
    (setq shipit--user-mutated-pr t)
    (when (derived-mode-p 'shipit-mode)
      (shipit-buffer-refresh))))

(defun shipit--approval-dwim-actions ()
  "Handle dwim actions for PR approval section."
  (let* ((pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (when (boundp 'shipit-current-pr) shipit-current-pr)))
         (repo (or (get-text-property (point) 'shipit-repo)
                   (when (boundp 'shipit-current-repo) shipit-current-repo)))
         (actions '()))

    (unless pr-number
      (user-error "No PR context found"))

    ;; Add approval/rejection options
    (push "Approve PR" actions)
    (push "Request changes" actions)
    (push "Comment only (no approval state)" actions)
    (push "Dismiss your review" actions)
    (push "Manage reviewers" actions)

    ;; Show action menu and temporarily set shipit-current-pr for the called functions
    (let ((choice (completing-read "Choose action: " actions nil t))
          (shipit-current-pr pr-number)
          (shipit-current-repo repo))
      (cond
       ((string= choice "Approve PR")
        (shipit-approve))
       ((string= choice "Request changes")
        (shipit-request-changes))
       ((string= choice "Comment only (no approval state)")
        (shipit-comment-review))
       ((string= choice "Dismiss your review")
        (shipit-dismiss-review-interactive))
       ((string= choice "Manage reviewers")
        (shipit--manage-reviewers pr-number repo))
       (t (message "No action selected"))))))

(defun shipit-approve ()
  "Approve the current pull request.
Opens the shipit editor for an optional review comment."
  (interactive)
  (let* ((pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (when (boundp 'shipit-current-pr) shipit-current-pr)))
         (text-prop-repo (get-text-property (point) 'shipit-repo))
         (buffer-repo (bound-and-true-p shipit-buffer-repo))
         (repo (or text-prop-repo buffer-repo)))
    (shipit--debug-log "APPROVE: text-prop-repo=%S, buffer-repo=%S, using repo=%S, pr=%S"
                       text-prop-repo buffer-repo repo pr-number)
    (if pr-number
        (shipit-editor-open
         (list :type 'review
               :review-event "APPROVE"
               :source-buffer (current-buffer)
               :pr-number pr-number
               :repo repo))
      (user-error "No PR context found"))))

(defun shipit-request-changes ()
  "Request changes on the current pull request.
Opens the shipit editor to explain what changes are needed."
  (interactive)
  (let* ((pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (when (boundp 'shipit-current-pr) shipit-current-pr)))
         (repo (or (get-text-property (point) 'shipit-repo)
                   (bound-and-true-p shipit-buffer-repo))))
    (if pr-number
        (shipit-editor-open
         (list :type 'review
               :review-event "REQUEST_CHANGES"
               :source-buffer (current-buffer)
               :pr-number pr-number
               :repo repo))
      (user-error "No PR context found"))))

(defun shipit-comment-review ()
  "Add a review comment without approval state.
Opens the shipit editor to write the review comment."
  (interactive)
  (let* ((pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (when (boundp 'shipit-current-pr) shipit-current-pr)))
         (repo (or (get-text-property (point) 'shipit-repo)
                   (bound-and-true-p shipit-buffer-repo))))
    (if pr-number
        (shipit-editor-open
         (list :type 'review
               :review-event "COMMENT"
               :source-buffer (current-buffer)
               :pr-number pr-number
               :repo repo))
      (user-error "No PR context found"))))

(defun shipit-dismiss-review-interactive ()
  "Dismiss your review for the current pull request."
  (interactive)
  (let* ((pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (when (boundp 'shipit-current-pr) shipit-current-pr)))
         (repo (or (get-text-property (point) 'shipit-repo)
                   (bound-and-true-p shipit-buffer-repo)))
         (dismiss-msg (read-string "Dismissal message (optional): ")))
    (if pr-number
        (shipit-dismiss-review pr-number
                               (if (string-empty-p dismiss-msg) nil dismiss-msg)
                               repo)
      (user-error "No PR context found"))))

(defun shipit--assignees-dwim-actions ()
  "Handle dwim actions for PR assignees section."
  (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (actions '()))

    ;; Add assign/unassign options
    (push "Assign yourself to PR" actions)
    (push "Manage assignees" actions)

    ;; Show action menu
    (let ((choice (completing-read "Choose action: " actions nil t)))
      (cond
       ((string= choice "Assign yourself to PR")
        (shipit--assign-self-to-pr pr-number repo))
       ((string= choice "Manage assignees")
        (shipit--manage-assignees pr-number repo))
       (t (message "No action selected"))))))

(defun shipit--assign-self-to-pr (pr-number repo)
  "Assign current user to the PR."
  (let ((current-user (shipit--get-current-user)))
    (if current-user
        (shipit--add-assignee-to-pr pr-number repo current-user)
      (user-error "Could not determine current user"))))

(defun shipit--get-fresh-assignees (pr-number repo)
  "Fetch fresh assignees data for PR-NUMBER in REPO, bypassing cache.
Dispatches to the active PR backend's :fetch-pr-assignees."
  (shipit--debug-log "ASSIGNEES-FRESH: Fetching fresh assignees for PR #%s" pr-number)
  (condition-case err
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (fetch-fn (plist-get backend :fetch-pr-assignees)))
        (funcall fetch-fn config pr-number))
    (error
     (shipit--debug-log "ASSIGNEES-FRESH: Error fetching: %s" (error-message-string err))
     nil)))

(defun shipit--manage-assignees (pr-number repo)
  "Interactive assignee management interface."
  ;; Always fetch fresh assignees to avoid stale cache issues
  (let* ((current-assignees (shipit--get-fresh-assignees pr-number repo))
         (current-usernames (mapcar (lambda (a) (cdr (assq 'login a))) current-assignees))
         (action (completing-read "Assignee action: "
                                  '("Add assignee" "Remove assignee")
                                  nil t)))
    (cond
     ((string= action "Add assignee")
      (let* ((available-users (shipit--get-available-assignees repo))
             (unassigned-users (seq-filter (lambda (user)
                                             (not (member user current-usernames)))
                                           available-users)))
        (if unassigned-users
            (let ((username (completing-read "Assign user: " unassigned-users nil t)))
              (shipit--add-assignee-to-pr pr-number repo username))
          (message "All available users are already assigned"))))
     ((string= action "Remove assignee")
      (if current-usernames
          (let ((username (completing-read "Remove assignee: " current-usernames nil t)))
            (shipit--remove-assignee-from-pr pr-number repo username))
        (message "No assignees to remove")))
     (t (message "No action selected")))))

(defun shipit--refresh-assignees-section-targeted (new-assignees pr-number repo)
  "Refresh only the assignees section with NEW-ASSIGNEES data.
PR-NUMBER and REPO provide context for text properties."
  (shipit--refresh-section-targeted
   'assignees
   "Assignees (\\([0-9]+\\))"
   (length new-assignees)
   (lambda (section)
     (if (= (length new-assignees) 0)
         (let ((start (point)))
           (insert "   No assignees\n")
           (add-text-properties start (point)
                                `(shipit-assignees t
                                  shipit-pr-number ,pr-number
                                  shipit-repo ,repo
                                  shipit-assignees-data nil
                                  magit-section ,section)))
       (dolist (assignee new-assignees)
         ;; Insert each assignee as a child section
         (magit-insert-section (assignee assignee)
           (let* ((user (cdr (assq 'login assignee)))
                  (avatar-url (cdr (assq 'avatar_url assignee)))
                  (start (point)))
             (insert (format "   %s%s\n"
                             (if shipit-show-avatars
                                 (concat (shipit--create-avatar-display user avatar-url 16) " ")
                               "")
                             (propertize user 'face 'shipit-username-face)))
             (add-text-properties start (1- (point))
                                  `(shipit-assignees t
                                    shipit-pr-number ,pr-number
                                    shipit-repo ,repo
                                    shipit-assignees-data ,new-assignees
                                    shipit-assignee-user ,user

                                    help-echo "RET: manage assignees")))))))))

(defun shipit--add-assignee-to-pr (pr-number repo username)
  "Add USERNAME as assignee to PR.
Dispatches to the active PR backend's :add-assignee."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (add-fn (plist-get backend :add-assignee))
         (target-buffer (current-buffer))
         (response-data (funcall add-fn config pr-number username)))
    (if response-data
        (progn
          ;; Invalidate ETag cache for PR endpoint so refresh gets fresh data
          (shipit-gh-etag-invalidate-endpoint (shipit-api-pr repo pr-number))
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (let ((new-assignees (cdr (assq 'assignees response-data))))
                (shipit--debug-log "ADD-ASSIGNEE: response-data keys: %S"
                                   (mapcar #'car response-data))
                (shipit--debug-log "ADD-ASSIGNEE: new-assignees: %S" new-assignees)
                ;; Update cache
                (let* ((branch (magit-get-current-branch))
                       (cache-key (format "%s:%s" repo branch)))
                  (when (and (boundp 'shipit--cached-branch-prs)
                             (hash-table-p shipit--cached-branch-prs))
                    (let ((cached-pr (gethash cache-key shipit--cached-branch-prs)))
                      (when cached-pr
                        (setf (cdr (assq 'assignees cached-pr)) new-assignees)
                        (puthash cache-key cached-pr shipit--cached-branch-prs)))))
                ;; Use targeted refresh
                (shipit--debug-log "ADD-ASSIGNEE: Calling targeted refresh with %d assignees"
                                   (length new-assignees))
                (shipit--refresh-assignees-section-targeted new-assignees pr-number repo))))
          (message "Added %s as assignee to PR #%s" username pr-number))
      (message "Error adding assignee - API call failed"))))

(defun shipit--remove-assignee-from-pr (pr-number repo username)
  "Remove USERNAME as assignee from PR.
Dispatches to the active PR backend's :remove-assignee."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (remove-fn (plist-get backend :remove-assignee))
         (target-buffer (current-buffer))
         (response-data (funcall remove-fn config pr-number username)))
    (if response-data
        (progn
          ;; Invalidate ETag cache for PR endpoint so refresh gets fresh data
          (shipit-gh-etag-invalidate-endpoint (shipit-api-pr repo pr-number))
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (let ((new-assignees (cdr (assq 'assignees response-data))))
                ;; Update cache
                (let* ((branch (magit-get-current-branch))
                       (cache-key (format "%s:%s" repo branch)))
                  (when (and (boundp 'shipit--cached-branch-prs)
                             (hash-table-p shipit--cached-branch-prs))
                    (let ((cached-pr (gethash cache-key shipit--cached-branch-prs)))
                      (when cached-pr
                        (setf (cdr (assq 'assignees cached-pr)) new-assignees)
                        (puthash cache-key cached-pr shipit--cached-branch-prs)))))
                ;; Use targeted refresh
                (shipit--refresh-assignees-section-targeted new-assignees pr-number repo))))
          (message "Removed %s as assignee from PR #%s" username pr-number))
      (message "Error removing assignee - API call failed"))))

(defun shipit--get-available-assignees (repo)
  "Get list of users who can be assigned to PRs in REPO.
Dispatches to the active PR backend's :fetch-available-assignees."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-available-assignees)))
    (if fetch-fn
        (let ((assignees-data (funcall fetch-fn config)))
          (mapcar (lambda (assignee) (cdr (assq 'login assignee))) assignees-data))
      '())))

;;; Reviewer Management

(defun shipit--get-current-reviewers (pr-number repo)
  "Get list of currently requested reviewer usernames for PR-NUMBER in REPO."
  (let* ((requested-data (shipit--get-requested-reviewers repo pr-number))
         (requested-users (when requested-data (cdr (assq 'users requested-data)))))
    (mapcar (lambda (user) (cdr (assq 'login user))) (or requested-users '()))))

(defun shipit--add-reviewer-to-pr (pr-number repo username)
  "Request review from USERNAME on PR-NUMBER in REPO.
Dispatches to the active PR backend's :add-reviewer."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (add-fn (plist-get backend :add-reviewer))
         (target-buffer (current-buffer))
         (response-data (funcall add-fn config pr-number username)))
    (if response-data
        (progn
          ;; Invalidate ETag cache for PR endpoint so refresh gets fresh data
          (shipit-gh-etag-invalidate-endpoint (shipit-api-pr repo pr-number))
          (shipit-gh-etag-invalidate-endpoint (shipit-api-pr-requested-reviewers repo pr-number))
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (shipit-buffer-refresh)))
          (message "Requested review from %s on PR #%s" username pr-number))
      (message "Error requesting reviewer - API call failed"))))

(defun shipit--remove-reviewer-from-pr (pr-number repo username)
  "Remove USERNAME from requested reviewers on PR-NUMBER in REPO.
Dispatches to the active PR backend's :remove-reviewer."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (remove-fn (plist-get backend :remove-reviewer))
         (target-buffer (current-buffer))
         (response-data (funcall remove-fn config pr-number username)))
    (if response-data
        (progn
          ;; Invalidate ETag cache for PR endpoint so refresh gets fresh data
          (shipit-gh-etag-invalidate-endpoint (shipit-api-pr repo pr-number))
          (shipit-gh-etag-invalidate-endpoint (shipit-api-pr-requested-reviewers repo pr-number))
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (shipit-buffer-refresh)))
          (message "Removed %s from reviewers on PR #%s" username pr-number))
      (message "Error removing reviewer - API call failed"))))

(defun shipit--manage-reviewers (pr-number repo)
  "Manage reviewers for PR-NUMBER in REPO."
  (let* ((current-reviewers (shipit--get-current-reviewers pr-number repo))
         (action (completing-read "Reviewer action: "
                                  '("Add reviewer" "Remove reviewer")
                                  nil t)))
    (cond
     ((string= action "Add reviewer")
      ;; Use same list as assignees - collaborators can be reviewers
      (let* ((available-users (shipit--get-available-assignees repo))
             (unrequested-users (seq-filter (lambda (user)
                                              (not (member user current-reviewers)))
                                            available-users)))
        (if unrequested-users
            (let ((username (completing-read "Request review from: " unrequested-users nil t)))
              (shipit--add-reviewer-to-pr pr-number repo username))
          (message "All available users are already requested"))))
     ((string= action "Remove reviewer")
      (if current-reviewers
          (let ((username (completing-read "Remove reviewer: " current-reviewers nil t)))
            (shipit--remove-reviewer-from-pr pr-number repo username))
        (message "No pending reviewers to remove")))
     (t (message "No action selected")))))

(defun shipit--get-comment-author-at-point ()
  "Get the author login of the comment at current point by finding it in cached comments."
  (let* ((comment-id (get-text-property (point) 'shipit-comment-id))
         (pr-number (car-safe shipit--current-displayed-pr))
         (repo (shipit--get-repo-from-remote)))
    (when (and comment-id pr-number repo)
      ;; Look in inline comments first
      (let ((inline-author (shipit--find-comment-author-in-cache comment-id shipit--cached-inline-comments)))
        (if inline-author
            inline-author
          ;; Look in general comments
          (shipit--find-comment-author-in-cache comment-id shipit--cached-general-comments))))))

(defun shipit--find-comment-author-in-cache (comment-id comments)
  "Find the author of COMMENT-ID in COMMENTS list."
  (when comments
    (let ((comment (seq-find (lambda (c) (equal (cdr (assq 'id c)) comment-id)) comments)))
      (when comment
        (let ((user-obj (cdr (assq 'user comment))))
          (when user-obj
            (cdr (assq 'login user-obj))))))))

(defun shipit--resolve-comment-interactive (comment-id)
  "Mark comment thread as resolved interactively."
  (if (y-or-n-p "Mark this comment thread as resolved? ")
      (progn
        (message "Resolving comment thread...")
        (shipit--resolve-comment-thread comment-id)
        (message "Comment thread marked as resolved"))
    (message "Cancelled")))

(defun shipit--resolve-comment-thread (comment-id)
  "Mark the thread containing COMMENT-ID as resolved using GitHub API."
  ;; This would need to use GitHub's GraphQL API to resolve threads
  ;; For now, we'll provide a basic implementation that can be enhanced
  (let* ((repo (shipit--get-repo-from-remote))
         (pr-number (car-safe shipit--current-displayed-pr)))
    (when (and repo pr-number)
      ;; GitHub's thread resolution requires GraphQL API
      ;; This is a placeholder - the actual implementation would need
      ;; to find the thread ID and use GraphQL mutation
      (message "Thread resolution would require GraphQL API implementation")
      ;; TODO: Implement actual thread resolution via GraphQL
      )))

(defun shipit--inline-comment-actions ()
  "Handle dwim actions for inline comments (comments on specific file lines).
If point is on a URL, includes Open URL option in the action menu."
  (let* ((comment-id (get-text-property (point) 'shipit-comment-id))
         (url-at-point (shipit--url-at-point))
         (actions '()))

    ;; Add Open URL option if on a URL
    (when url-at-point
      (push "Open URL" actions))

    ;; Always available actions
    (push "Add new comment" actions)
    (push "Reply to comment" actions)
    (push "React to comment" actions)
    (push "Edit comment" actions)
    (push "Delete comment" actions)
    (push "Set as resolved" actions)

    (let ((choice (completing-read "Comment action: " actions nil t)))
      (cond
       ((string= choice "Open URL")
        (browse-url url-at-point))
       ((string= choice "Add new comment")
        (shipit--add-general-comment-directly))
       ((string= choice "Reply to comment")
        (shipit--reply-to-comment comment-id))
       ((string= choice "React to comment")
        (shipit--react-to-comment comment-id))
       ((string= choice "Edit comment")
        (let ((current-body (get-text-property (point) 'shipit-comment-body)))
          (shipit--edit-comment-interactive comment-id current-body)))
       ((string= choice "Delete comment")
        (shipit--delete-comment-interactive comment-id))
       ((string= choice "Set as resolved")
        (shipit--resolve-comment-interactive comment-id))
       (t (message "No action selected"))))))

(defun shipit--general-comment-actions ()
  "Handle dwim actions for general comments (PR-level comments).
If point is on a URL, includes Open URL option in the action menu."
  (let* ((comment-id (get-text-property (point) 'shipit-comment-id))
         (comment-type (get-text-property (point) 'shipit-comment-type))
         (is-review-comment (string= comment-type "review"))
         (url-at-point (shipit--url-at-point))
         (actions '()))

    ;; Add Open URL option if on a URL
    (when url-at-point
      (push "Open URL" actions))

    (push "Add new comment" actions)
    (push "Reply to comment" actions)
    (push "React to comment" actions)

    ;; Only offer edit/delete for regular comments, not review comments
    (unless is-review-comment
      (push "Edit comment" actions)
      (push "Delete comment" actions))

    ;; Add resolve action (available to all users)
    (push "Set as resolved" actions)

    (let ((choice (completing-read "Comment action: " actions nil t)))
      (cond
       ((string= choice "Open URL")
        (browse-url url-at-point))
       ((string= choice "Add new comment")
        (shipit--add-general-comment-directly))
       ((string= choice "Reply to comment")
        (shipit--reply-to-comment comment-id))
       ((string= choice "React to comment")
        (shipit--react-to-comment comment-id))
       ((string= choice "Edit comment")
        (let ((current-body (get-text-property (point) 'shipit-comment-body)))
          (shipit--edit-comment-interactive comment-id current-body)))
       ((string= choice "Delete comment")
        (shipit--delete-comment-interactive comment-id))
       ((string= choice "Set as resolved")
        (shipit--resolve-comment-interactive comment-id))
       (t (message "No action selected"))))))

;; Placeholder functions for comment actions (to be implemented)
(defun shipit--get-comment-depth (comment-id)
  "Get the reply depth of comment with COMMENT-ID from cached comments.
Returns 0 if not found."
  (let ((depth 0)
        (found nil))
    (when (and (boundp 'shipit--cached-general-comments) shipit--cached-general-comments)
      (dolist (comment shipit--cached-general-comments)
        (when (equal (cdr (assq 'id comment)) comment-id)
          (setq found t)
          (setq depth (or (cdr (assq 'reply-depth comment)) 0)))))
    (shipit--debug-log "GET-COMMENT-DEPTH: comment-id=%s found=%s depth=%s cache-size=%s"
                       comment-id found depth
                       (if (boundp 'shipit--cached-general-comments)
                           (length shipit--cached-general-comments)
                         "unbound"))
    depth))

(defun shipit--reply-to-comment (comment-id)
  "Reply to comment with COMMENT-ID using shipit-editor with live preview."
  (let* ((comment-body (shipit--clean-text (get-text-property (point) 'shipit-comment-body)))
         (comment-type (get-text-property (point) 'shipit-comment-type))
         (review-state (get-text-property (point) 'shipit-review-state))
         (repo (or (bound-and-true-p shipit-buffer-repo)
                   (shipit--get-repo-from-remote)))
         (pr-number (or (bound-and-true-p shipit-buffer-pr-number)
                        (when (car-safe shipit--current-displayed-pr)
                          (car shipit--current-displayed-pr))))
         (file-path (get-text-property (point) 'shipit-file-path))
         (section-marker (point-marker))
         ;; Get parent's depth so reply can be indented correctly
         (parent-depth (shipit--get-comment-depth comment-id))
         ;; Pre-populate with quoted parent for general comments (enables threading)
         ;; Include parent-comment-id for reliable embedded marker threading
         ;; For inline comments, GitHub API handles threading via in_reply_to
         (initial-content (if (and comment-body (not file-path))
                              (shipit--format-quoted-reply comment-body comment-id)
                            "")))
    ;; Debug logging
    (shipit--debug-log 'comments "Reply to comment - comment-id: %S, type: %S, review-state: %S, repo: %S, pr-number: %S"
                       comment-id comment-type review-state repo pr-number)
    (if (and pr-number repo comment-id)
        ;; Use shipit-editor for reply with live preview
        (if (fboundp 'shipit-editor-open)
            (shipit-editor-open
             (list :type 'reply
                   :source-buffer (current-buffer)
                   :pr-number pr-number
                   :repo repo
                   :parent-comment-id comment-id
                   :parent-depth parent-depth
                   :file-path file-path
                   :section-marker section-marker
                   :initial-content initial-content))
          ;; Fallback to read-string if editor not available
          (let* ((comment-preview (when comment-body
                                    (let ((cleaned (replace-regexp-in-string "[\n\r]+" " " comment-body)))
                                      (if (> (length cleaned) 60)
                                          (concat (substring cleaned 0 57) "...")
                                        cleaned))))
                 (prompt (if comment-preview
                             (format "Reply to: \"%s\"\n> " comment-preview)
                           "Reply: "))
                 (reply-text (read-string prompt)))
            (when (and reply-text (not (string-empty-p (string-trim reply-text))))
              (if file-path
                  (shipit--reply-to-inline-comment pr-number comment-id reply-text file-path)
                (shipit--reply-to-general-comment pr-number comment-id reply-text)))))
      (user-error "Cannot reply: missing PR context or comment ID (comment-id: %S, repo: %S, pr-number: %S)"
                  comment-id repo pr-number))))


(defun shipit--delete-comment-interactive (comment-id)
  "Delete comment with COMMENT-ID."
  (let* ((comment-body (get-text-property (point) 'shipit-comment-body))
         (file-path (get-text-property (point) 'shipit-file-path))
         ;; If file-path is set, it's a PR review comment (inline or file-level)
         ;; and should use the pulls/comments endpoint, not issues/comments
         (is-inline-comment (if file-path t nil))
         (truncated-body (if comment-body
                             (truncate-string-to-width comment-body 50 nil nil "...")
                           "this comment"))
         (confirm-msg (format "Really delete this comment (%s)? " truncated-body)))
    (when (y-or-n-p confirm-msg)
      ;; shipit--delete-comment triggers targeted refresh for file comments
      (shipit--delete-comment comment-id is-inline-comment file-path))))

(defun shipit-delete-comment-at-point ()
  "Delete the comment at point."
  (interactive)
  (let ((comment-id (get-text-property (point) 'shipit-comment-id)))
    (if comment-id
        (shipit--delete-comment-interactive comment-id)
      (user-error "No comment at point"))))

(defun shipit--add-general-comment-directly ()
  "Add a general comment to the PR directly without cycle mode.
Uses the new shipit-editor with live preview and PR reference completion."
  (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo)))
    (if (and pr-number repo)
        (if (fboundp 'shipit-editor-open)
            (shipit-editor-open
             (list :type 'general-comment
                   :source-buffer (current-buffer)
                   :pr-number pr-number
                   :repo repo))
          ;; Fallback to simple read-string if editor not loaded
          (let ((comment-text (read-string "Add general comment: ")))
            (when (and comment-text (not (string-empty-p comment-text)))
              (shipit--add-general-comment-to-pr pr-number comment-text)
              (shipit-refresh))))
      (user-error "No PR context found"))))

(defvar shipit--dwim-handlers '()
  "Registry of dwim handlers. Each handler is (name . ((matcher . func) (action . func))).")

(defun shipit-register-dwim-handler (name matcher action)
  "Register a dwim handler with NAME, MATCHER function, and ACTION function.
MATCHER function takes no args, returns non-nil if this handler should execute.
ACTION function takes no args, executes the handler behavior."
  (let ((handler `(,name . ((matcher . ,matcher) (action . ,action)))))
    (setq shipit--dwim-handlers
          (cons handler (assq-delete-all name shipit--dwim-handlers)))))

(defun shipit-dwim ()
  "Execute the first handler that claims the current context."
  (interactive)
  (let ((handler-found nil))
    (dolist (handler shipit--dwim-handlers)
      (unless handler-found
        (let* ((name (car handler))
               (spec (cdr handler))
               (matcher (cdr (assq 'matcher spec)))
               (action (cdr (assq 'action spec))))
          (when (funcall matcher)
            (shipit--debug-log "DWIM: Handler '%s' claimed context" name)
            (funcall action)
            (setq handler-found t)))))
    (unless handler-found
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log "DWIM: No handler matched. Registered: %s Section: %s"
                           (mapcar #'car shipit--dwim-handlers)
                           (when (fboundp 'magit-current-section)
                             (let ((s (magit-current-section)))
                               (when s (oref s type))))))
      (error "No dwim handler claimed current context - add a handler for this case"))))

;; Register all dwim handlers with mutually exclusive matchers
(shipit-register-dwim-handler
 'subscription
 (lambda () (get-text-property (point) 'shipit-repo-subscription))
 (lambda ()
   (if (fboundp 'shipit-repo-subscription)
       (shipit-repo-subscription)
     (user-error "shipit-repo-buffer not loaded"))))

(shipit-register-dwim-handler
 'description-section
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(shipit-description) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))))
 #'shipit--description-dwim-actions)

(shipit-register-dwim-handler
 'pr-header
 (lambda () (get-text-property (point) 'shipit-pr-header))
 #'shipit--pr-header-actions)

(shipit-register-dwim-handler
 'inline-comment
 (lambda () (and (get-text-property (point) 'shipit-comment)
                 (get-text-property (point) 'shipit-file-path)))
 #'shipit--inline-comment-actions)

;; Handler for inline comment contexts (file/line context where comments can be added)
(shipit-register-dwim-handler
 'inline-comment-context
 (lambda () (and (get-text-property (point) 'shipit-file-path)
                 (or (get-text-property (point) 'shipit-line-number)
                     (get-text-property (point) 'shipit-old-line-number))
                 (not (get-text-property (point) 'shipit-comment))))
 (lambda ()
   (let ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (file-path (get-text-property (point) 'shipit-file-path))
         (line-number (or (get-text-property (point) 'shipit-line-number)
                          (get-text-property (point) 'shipit-old-line-number))))
     (if (and pr-number repo file-path line-number)
         (shipit--simple-comment-dialog pr-number repo)
       (user-error "Missing context for inline comment: pr=%s repo=%s file=%s line=%s"
                   pr-number repo file-path line-number)))))

(shipit-register-dwim-handler
 'general-comment
 (lambda () (and (get-text-property (point) 'shipit-comment)
                 (not (get-text-property (point) 'shipit-file-path))))
 #'shipit--general-comment-actions)

;; Handler for when in general comments section but not directly on comment text
;; Excludes section header (which is handled by general-comments-section handler)
(shipit-register-dwim-handler
 'general-comment-nearby
 (lambda () (and (shipit--in-general-comments-section-p)
                 (not (get-text-property (point) 'shipit-comment))
                 (not (get-text-property (point) 'shipit-file-path))
                 ;; Exclude section header - check if on a magit section of type general-comments
                 (not (and (fboundp 'magit-current-section)
                           (magit-section-match '(general-comments) (magit-current-section))
                           (get-text-property (point) 'shipit-general-comments)))))
 (lambda ()
   ;; Find the nearest comment in the general comments section
   (let ((nearest-comment-pos nil))
     ;; Search backward for a comment first
     (save-excursion
       (while (and (not nearest-comment-pos)
                   (> (point) (point-min))
                   (shipit--in-general-comments-section-p))
         (backward-char 1)
         (when (get-text-property (point) 'shipit-comment)
           (setq nearest-comment-pos (point)))))
     ;; If no comment found backward, search forward
     (unless nearest-comment-pos
       (save-excursion
         (while (and (not nearest-comment-pos)
                     (< (point) (point-max))
                     (shipit--in-general-comments-section-p))
           (forward-char 1)
           (when (get-text-property (point) 'shipit-comment)
             (setq nearest-comment-pos (point))))))
     ;; If we found a nearby comment, move to it and trigger its actions
     (if nearest-comment-pos
         (progn
           (goto-char nearest-comment-pos)
           (message "Moved to nearby comment")
           (shipit--general-comment-actions))
       (message "No comments found in general comments section")))))

(shipit-register-dwim-handler
 'notification-pr
 (lambda () (and (get-text-property (point) 'shipit-notifications)
                 (get-text-property (point) 'shipit-pr-number)))
 #'shipit--notification-pr-actions)

(shipit-register-dwim-handler
 'notifications-section-header
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(notifications) (magit-current-section))
                 (not (get-text-property (point) 'shipit-pr-number))))
 #'shipit--notifications-header-actions)

(shipit-register-dwim-handler
 'activity-event
 (lambda () (get-text-property (point) 'shipit-activity-event))
 #'shipit--activity-event-actions)

(shipit-register-dwim-handler
 'check-item-logs
 (lambda ()
   (and (fboundp 'magit-current-section)
        (let ((s (magit-current-section)))
          (while (and s (not (memq (oref s type)
                                   '(check-item actions-step actions-log-group))))
            (setq s (oref s parent)))
          (and s (memq (oref s type) '(check-item actions-step actions-log-group))))))
 #'shipit-actions-nav-menu)

(shipit-register-dwim-handler
 'activity-section-header
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(pr-activity) (magit-current-section))
                 (not (get-text-property (point) 'shipit-activity-event))))
 #'shipit--activity-section-header-actions)

(shipit-register-dwim-handler
 'inline-comments-section-header
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(inline-comments) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))))
 (lambda () (message "Inline comments are added from file context, not from this section")))

(shipit-register-dwim-handler
 'general-comments-section
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(general-comments) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))))
 #'shipit--add-general-comment-directly)

;; Metadata line handlers (do nothing but provide proper highlighting)
(shipit-register-dwim-handler
 'pr-state-line
 (lambda () (get-text-property (point) 'shipit-pr-state))
 (lambda () (message "PR state: %s" (get-text-property (point) 'help-echo))))

(shipit-register-dwim-handler
 'pr-refs-line
 (lambda () (get-text-property (point) 'shipit-pr-refs))
 (lambda () (message "PR refs: %s" (or (get-text-property (point) 'shipit-refs-line) "Unknown"))))

(shipit-register-dwim-handler
 'pr-created-line
 (lambda () (get-text-property (point) 'shipit-pr-created))
 (lambda () (message "PR created: %s" (or (get-text-property (point) 'shipit-created) "Unknown"))))

(shipit-register-dwim-handler
 'pr-author-line
 (lambda () (get-text-property (point) 'shipit-pr-author))
 (lambda () (message "PR author: %s" (or (get-text-property (point) 'shipit-author) "Unknown"))))

(shipit-register-dwim-handler
 'draft-toggle
 (lambda () (get-text-property (point) 'shipit-pr-draft))
 #'shipit--toggle-draft-status)

(shipit-register-dwim-handler
 'labels-section
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(labels) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))))
 (lambda ()
   (let ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (current-labels (get-text-property (point) 'shipit-pr-labels)))
     (if (and pr-number repo)
         (shipit--manage-labels-interactive pr-number repo current-labels)
       (user-error "Cannot manage labels: missing PR context")))))

(shipit-register-dwim-handler
 'assignees-section
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(assignees) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))))
 #'shipit--assignees-dwim-actions)

(shipit-register-dwim-handler
 'worktree-section
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(shipit-worktree-section) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))))
 (lambda ()
   (let ((pr-number (get-text-property (point) 'shipit-pr-number))
         (pr-data (get-text-property (point) 'shipit-pr-data))
         (repo (get-text-property (point) 'shipit-repo)))
     (if (and pr-number pr-data repo)
         (call-interactively 'shipit-worktree)
       (user-error "Cannot open worktree menu: missing PR context (pr=%s data=%s repo=%s)"
                   pr-number (if pr-data "present" "missing") repo)))))

(shipit-register-dwim-handler
 'approval-section
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(approval) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))))
 #'shipit--approval-dwim-actions)

(defun shipit--get-file-level-comments (file-path)
  "Get all file-level comments (no line number) for FILE-PATH from cache."
  (when (and (boundp 'shipit--cached-inline-comments) shipit--cached-inline-comments)
    (seq-filter (lambda (c)
                  (and (string= (cdr (assq 'path c)) file-path)
                       (null (cdr (assq 'line c)))
                       (null (cdr (assq 'original_line c)))))
                shipit--cached-inline-comments)))

(defun shipit--truncate-comment-body (body)
  "Truncate BODY to 40 chars with ellipsis if needed."
  (if (> (length body) 40)
      (concat (substring body 0 40) "...")
    body))

(defun shipit--file-header-actions ()
  "Show actions for file header - add new or manage existing file comments."
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (repo (get-text-property (point) 'shipit-repo))
         (pr-number (get-text-property (point) 'shipit-pr-number))
         (file-comments (shipit--get-file-level-comments file-path))
         (add-label (concat (propertize "Add" 'face '(:foreground "green")) " "))
         (edit-label (concat (propertize "Edit" 'face '(:foreground "orange")) ": "))
         (delete-label (concat (propertize "Delete" 'face '(:foreground "red")) ": "))
         (options (if file-comments
                      (append
                       (list (concat add-label "new file comment"))
                       (mapcar (lambda (c)
                                 (concat edit-label
                                         (shipit--truncate-comment-body
                                          (or (cdr (assq 'body c)) ""))))
                               file-comments)
                       (mapcar (lambda (c)
                                 (concat delete-label
                                         (shipit--truncate-comment-body
                                          (or (cdr (assq 'body c)) ""))))
                               file-comments))
                    (list (concat add-label "file comment"))))
         (choice (completing-read "File comment action: " options nil t)))
    (cond
     ((string-prefix-p "Add" (substring-no-properties choice))
      (shipit--add-file-level-comment file-path repo pr-number))
     ((string-prefix-p "Edit" (substring-no-properties choice))
      (let* ((body-preview (substring (substring-no-properties choice) 6))
             (comment (seq-find (lambda (c)
                                  (string= body-preview
                                           (shipit--truncate-comment-body
                                            (or (cdr (assq 'body c)) ""))))
                                file-comments)))
        (when comment
          (shipit--edit-inline-comment (cdr (assq 'id comment)) repo pr-number))))
     ((string-prefix-p "Delete" (substring-no-properties choice))
      (let* ((body-preview (substring (substring-no-properties choice) 8))
             (comment (seq-find (lambda (c)
                                  (string= body-preview
                                           (shipit--truncate-comment-body
                                            (or (cdr (assq 'body c)) ""))))
                                file-comments)))
        (when comment
          (shipit--delete-inline-comment (cdr (assq 'id comment)) repo pr-number))))
     (t (message "No action selected")))))

(defun shipit--add-file-level-comment (file-path repo pr-number)
  "Add a file-level comment (no specific line) on FILE-PATH."
  (let ((comment-text (read-string (format "File comment on %s: " file-path))))
    (when (and comment-text (not (string-empty-p comment-text)))
      ;; Use the PR review comment API with subject_type=file
      (shipit--add-comment-to-pr pr-number file-path nil comment-text nil)
      (message "File comment added to %s" file-path))))

;; Handler for pr-file sections - file comment actions
(shipit-register-dwim-handler
 'pr-file-section
 (lambda () (and (fboundp 'magit-current-section)
                 (magit-section-match '(pr-file) (magit-current-section))
                 (not (get-text-property (point) 'shipit-comment))
                 (not (get-text-property (point) 'shipit-line-number))))
 #'shipit--file-header-actions)

(shipit-register-dwim-handler
 'diff-mode
 (lambda () (derived-mode-p 'magit-diff-mode 'magit-revision-mode))
 (lambda ()
   ;; Check context and provide appropriate actions
   (cond
    ;; Case 1: On existing inline comment - provide full comment management actions
    ((and (get-text-property (point) 'shipit-comment)
          (get-text-property (point) 'shipit-file-path))
     (shipit--inline-comment-actions))

    ;; Case 2: On code line - provide comment creation dialog
    (t
     (let ((pr-number (or (car-safe shipit--current-displayed-pr)
                          (user-error "No PR context available in diff mode")))
           (repo (shipit--get-repo-from-remote)))
       (if repo
           (shipit--simple-comment-dialog pr-number repo)
         (user-error "Cannot determine repository from git remote")))))))



;;;###autoload
(defun shipit-refresh ()
  "Refresh shipit PR data and magit buffer.
If a specific PR is displayed, refreshes that PR. Otherwise, refreshes current branch's PR."
  (interactive)
  (setq shipit--explicit-pr-operation t)  ; Mark this as explicit PR operation
  ;; Initialize refresh-level request deduplication cache
  (when (fboundp 'shipit-gh-etag--init-refresh-cache)
    (shipit-gh-etag--init-refresh-cache))
  (let ((displayed-pr-context (shipit--get-displayed-pr-context)))
    (shipit--debug-log "shipit-refresh called: displayed-pr-context=%s" displayed-pr-context)
    (if displayed-pr-context
        (let ((pr-number (car displayed-pr-context))
              (repo (cadr displayed-pr-context)))
          (message "Refreshing PR #%s in %s..." pr-number repo)
          (shipit--debug-log "Manual refresh requested for displayed PR #%s in %s" pr-number repo)
          ;; Store the current PR context for the refresh
          (setq shipit--refresh-pr-context (list pr-number repo))
          ;; Clear shipit caches but preserve ETag cache for efficiency
          (shipit--clear-branch-cache)
          ;; Note: ETag cache preserved - conditional requests will handle freshness
          (setq shipit--manual-refresh-in-progress t)
          ;; Schedule clearing the flag and context as fallback
          (run-with-timer 1.0 nil (lambda ()
                                    (shipit--debug-log "Manual refresh timeout - clearing flag and context")
                                    (setq shipit--manual-refresh-in-progress nil
                                          shipit--refresh-pr-context nil
                                          shipit--explicit-pr-operation nil)))
          (setq shipit--refresh-pr-context nil)
          (message "Shipit cache cleared"))
      ;; Fallback to old behavior if no displayed PR context found
      (message "Refreshing shipit PR data...")
      (shipit--debug-log "Manual shipit refresh requested - no displayed PR context")
      ;; Clear shipit caches but preserve ETag cache for efficiency
      (shipit--clear-branch-cache)
      ;; Note: ETag cache preserved - conditional requests will handle freshness
      (setq shipit--manual-refresh-in-progress t)
      ;; Schedule clearing the flag as fallback
      (run-with-timer 1.0 nil (lambda ()
                                (shipit--debug-log "Manual refresh timeout - clearing flag")
                                (setq shipit--manual-refresh-in-progress nil
                                      shipit--explicit-pr-operation nil)
                                ;; Clear refresh cache on timeout
                                (when (fboundp 'shipit-gh-etag--clear-refresh-cache)
                                  (shipit-gh-etag--clear-refresh-cache))))
      (message "Shipit cache cleared")
      ;; Clear refresh cache after refresh completes
      (when (fboundp 'shipit-gh-etag--clear-refresh-cache)
        (shipit-gh-etag--clear-refresh-cache)))))

;;; Region E: Context functions

(defun shipit--has-pr-data-in-buffer ()
  "Check if the current buffer has shipit PR data visible."
  (save-excursion
    (goto-char (point-min))
    ;; Look for any shipit section, not just the PR header
    (or (text-property-search-forward 'shipit-pr-header)
        (progn (goto-char (point-min)) (text-property-search-forward 'shipit-general-comments))
        (progn (goto-char (point-min)) (text-property-search-forward 'shipit-inline-comments))
        (progn (goto-char (point-min)) (text-property-search-forward 'shipit-labels))
        (progn (goto-char (point-min)) (text-property-search-forward 'shipit-reviews))
        (progn (goto-char (point-min)) (text-property-search-forward 'shipit-assignees))
        (progn (goto-char (point-min)) (text-property-search-forward 'shipit-reviewers)))))

(defun shipit--get-displayed-pr-context ()
  "Get the PR number and repo of the currently displayed PR from buffer text properties.
Returns a list (pr-number repo) or nil if no PR is displayed."
  (save-excursion
    (goto-char (point-min))
    (let ((pr-number nil)
          (repo nil))
      (shipit--debug-log "shipit--get-displayed-pr-context: searching buffer from point-min=%d to point-max=%d" (point-min) (point-max))
      ;; Look for PR context in text properties throughout buffer
      ;; Collect all instances and use the first complete pair
      (catch 'found
        (while (< (point) (point-max))
          (let ((pr-num (get-text-property (point) 'shipit-pr-number))
                (pr-repo (get-text-property (point) 'shipit-repo)))
            (when (or pr-num pr-repo)
              (shipit--debug-log "Found text properties at point %d: pr-number=%s repo=%s" (point) pr-num pr-repo))
            ;; Collect the first non-nil values we find
            (when pr-num (setq pr-number pr-num))
            (when pr-repo (setq repo pr-repo))
            ;; If we have both, we're done
            (when (and pr-number repo)
              (shipit--debug-log "Found complete PR context: pr-number=%s repo=%s" pr-number repo)
              (throw 'found t)))
          (goto-char (next-property-change (point) nil (point-max)))))
      (let ((result (when (and pr-number repo) (list pr-number repo))))
        (shipit--debug-log "shipit--get-displayed-pr-context result: %s" result)
        result))))

;;; Region F: Assignee wrappers

(defun shipit--add-assignee (pr-number username repo)
  "Add USERNAME as assignee to PR-NUMBER in REPO.
Dispatches to the active PR backend's :add-assignee."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (add-fn (plist-get backend :add-assignee)))
    (funcall add-fn config pr-number username)))

(defun shipit--remove-assignee (pr-number username repo)
  "Remove USERNAME from assignees of PR-NUMBER in REPO.
Dispatches to the active PR backend's :remove-assignee."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (remove-fn (plist-get backend :remove-assignee)))
    (funcall remove-fn config pr-number username)))

;;; Region G: Assignees interactive

(defun shipit--assignees-interactive ()
  "Interactive assignee management with completion interface."
  (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (current-assignees (get-text-property (point) 'shipit-assignees-data))
         (current-usernames (when current-assignees
                              (mapcar (lambda (assignee)
                                        (cdr (assq 'login assignee)))
                                      current-assignees))))

    (if (and pr-number repo)
        (progn
          (message "Fetching assignees...")
          (let ((all-assignees (shipit--get-assignees repo)))
            (if all-assignees
                (let* ((assignee-choices
                        (append
                         ;; Current assignees first (with checkmarks)
                         (when current-usernames
                           (mapcar (lambda (username)
                                     (let* ((user-obj (cl-find-if (lambda (assignee)
                                                                    (string= (cdr (assq 'login assignee)) username))
                                                                  all-assignees))
                                            (name (when user-obj (cdr (assq 'name user-obj))))
                                            (display (if name
                                                         (format "✓ %s (%s)%s" username name)
                                                       (format "✓ %s" username))))
                                       (cons display username)))
                                   current-usernames))
                         ;; Then available assignees (without checkmarks)
                         (mapcar (lambda (user)
                                   (let* ((username (cdr (assq 'login user)))
                                          (name (cdr (assq 'name user)))
                                          (display (if name
                                                       (format "  %s (%s)%s" username name)
                                                     (format "  %s" username))))
                                     (cons display username)))
                                 (cl-remove-if (lambda (user)
                                                 (member (cdr (assq 'login user)) (or current-usernames '())))
                                               all-assignees))))
                       (choice (completing-read
                                (format "Select assignee (Current: %s): "
                                        (if current-usernames
                                            (string-join current-usernames ", ")
                                          "none"))
                                assignee-choices nil t)))
                  (when choice
                    (let* ((username (cdr (assoc choice assignee-choices)))
                           (is-currently-assigned (member username current-usernames)))
                      (if is-currently-assigned
                          (progn
                            (shipit--remove-assignee pr-number username repo)
                            (message "Removed %s from assignees" username))
                        (progn
                          (shipit--add-assignee pr-number username repo)
                          (message "Added %s to assignees" username)))
                      ;; Refresh the magit status to show updated assignees
                      (when (fboundp 'magit-refresh)
                        (magit-refresh)))))
              (message "Failed to fetch assignees"))))
      (message "Could not determine PR info from current position"))))

;;; Region H: File action menu

(defun shipit--file-action-menu ()
  "Show context-aware action menu for file at point.
Handles both PR files (shipit-pr-file property) and commit files
(shipit-commit-file property)."
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (commit-sha (get-text-property (point) 'shipit-commit-sha))
         (pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (comment-count (get-text-property (point) 'shipit-comment-count))
         (has-comments (and comment-count (> comment-count 0)))
         (is-commit-file (and file-path commit-sha))
         (is-pr-file (and file-path pr-number repo)))
    (cond
     ;; Commit file actions
     (is-commit-file
      (let* ((actions '(("Open at revision (RET)" . shipit--open-file-at-point)
                        ("Show diff (d)" . shipit--show-commit-file-diff)
                        ("View in ediff (e)" . shipit--ediff-file-at-point)))
             (choice (completing-read
                      (format "Commit file: %s @ %.7s - Action: "
                              file-path commit-sha)
                      (mapcar #'car actions)
                      nil t))
             (action (cdr (assoc choice actions))))
        (when action
          (funcall action))))
     ;; PR file actions
     (is-pr-file
      (let* ((actions (if has-comments
                          '(("View diff (RET)" . shipit--file-at-point)
                            ("Open file (o)" . shipit--open-file-at-point)
                            ("View in ediff (e)" . shipit--ediff-file-at-point)
                            ("Browse comments" . (lambda ()
                                                   (shipit--file-at-point)
                                                   (re-search-forward "💬" nil t))))
                        '(("View diff (RET)" . shipit--file-at-point)
                          ("Open file (o)" . shipit--open-file-at-point)
                          ("View in ediff (e)" . shipit--ediff-file-at-point))))
             (choice (completing-read
                      (format "File: %s%s - Action: "
                              file-path
                              (if has-comments
                                  (format " (%d comment%s)"
                                          comment-count
                                          (if (= comment-count 1) "" "s"))
                                ""))
                      (mapcar #'car actions)
                      nil t))
             (action (cdr (assoc choice actions))))
        (when action
          (if (functionp action)
              (funcall action)
            (eval action)))))
     (t
      (user-error "No file context found at point")))))

;; Register dwim handler for commit file sections
(shipit-register-dwim-handler
 'commit-file-section
 (lambda () (and (get-text-property (point) 'shipit-commit-file)
                 (get-text-property (point) 'shipit-file-path)
                 (get-text-property (point) 'shipit-commit-sha)))
 #'shipit--file-action-menu)

;;; Region I: Activity section header actions

(defun shipit--activity-section-header-actions ()
  "Handle DWIM actions for the Activity section header."
  (interactive)
  (let* ((actions '("Mark all as read"))
         (choice (completing-read "Activity action: " actions nil t)))
    (cond
     ((string= choice "Mark all as read")
      (shipit-mark-activities-read)))))

;;; Region J: Activity event actions

(defun shipit--activity-event-actions ()
  "Handle DWIM actions for activity timeline events."
  ;; Mark this specific activity as read
  (let ((pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr))
        (event-id (get-text-property (point) 'shipit-event-id)))
    (when (and pr-number repo event-id)
      (shipit--mark-activity-read repo pr-number event-id)
      ;; Immediately clear this activity's red dot
      (shipit--clear-unread-indicator-at-point)
      ;; Update the header indicator (remove if no more unread)
      (shipit--update-activity-header-indicator)
      ;; Update section indicators (General Comments, Files Changed, etc.)
      (shipit--update-all-section-indicators)))
  (let* ((comment-id (get-text-property (point) 'shipit-activity-comment-id))
         (commit-sha (get-text-property (point) 'shipit-activity-commit-sha))
         (event-type (get-text-property (point) 'shipit-event-type))
         (review-state (get-text-property (point) 'shipit-review-state))
         (crossref-repo (get-text-property (point) 'shipit-crossref-repo))
         (crossref-number (get-text-property (point) 'shipit-crossref-number))
         (crossref-url (get-text-property (point) 'shipit-crossref-url))
         (crossref-title (get-text-property (point) 'shipit-crossref-title)))
    (shipit--debug-log "ACTIVITY-ACTION: comment-id=%s commit-sha=%s event-type=%s review-state=%s crossref=%s#%s"
                       comment-id commit-sha event-type review-state crossref-repo crossref-number)
    (cond
     ;; For "commented" events (general comments), navigate to the specific comment
     ((and comment-id (string= event-type "commented"))
      (shipit--navigate-to-comment-by-id comment-id))
     ;; For "reviewed" events with approved state, navigate to Approval section
     ((and (string= event-type "reviewed") (string= review-state "approved"))
      (shipit--navigate-to-approval-section))
     ;; For "reviewed" events with other states, navigate to inline comments
     ;; (review ID != review comment IDs, so we can't match directly)
     ((string= event-type "reviewed")
      ;; For reviews, the comment-id is actually the review ID
      ;; Search for any comment belonging to this review
      (shipit--navigate-to-review-comment comment-id))
     ;; For "committed" events, navigate to the commit in Commits section
     ((and commit-sha (string= event-type "committed"))
      (shipit--navigate-to-commit commit-sha))
     ;; For "line-commented" events (synthetic inline comments), navigate to the file
     ((string= event-type "line-commented")
      (let ((path (get-text-property (point) 'shipit-inline-comment-path)))
        (if path
            (shipit--navigate-to-inline-comment-file path comment-id)
          (message "No file path available for this inline comment"))))
     ;; For cross-referenced events, show action menu for the referencing PR
     ((and (string= event-type "cross-referenced") crossref-number)
      (shipit--crossref-actions crossref-repo crossref-number crossref-url crossref-title))
     ;; For other events, just show info
     (t
      (message "No navigation available for this activity event type: %s" event-type)))))

;;; Region K: Crossref actions

(defun shipit--crossref-actions (repo pr-number url title)
  "Show action menu for cross-referenced PR.
REPO is the repository (owner/repo), PR-NUMBER is the PR number,
URL is the browser URL, TITLE is the PR/MR title."
  (let* ((display-name (if repo
                           (format "%s#%d" repo pr-number)
                         (format "#%d" pr-number)))
         (backend-url (when (and repo pr-number)
                        (shipit--browse-pr-url repo pr-number)))
         (actions (list "Open in shipit" "Preview PR" "Open in browser" "Copy URL"))
         (choice (completing-read (format "Cross-reference %s: " display-name) actions nil t)))
    (cond
     ((string= choice "Open in shipit")
      (if (and repo pr-number (fboundp 'shipit--open-pr-cross-repo))
          (shipit--open-pr-cross-repo pr-number repo)
        (message "Cannot open: missing repo or PR number")))
     ((string= choice "Preview PR")
      (if (and repo pr-number (fboundp 'shipit--preview-pr))
          (shipit--preview-pr pr-number repo)
        (message "Cannot preview: missing repo or PR number")))
     ((string= choice "Open in browser")
      (browse-url (or url backend-url)))
     ((string= choice "Copy URL")
      (let ((copy-url (or url backend-url)))
        (if copy-url
            (progn
              (kill-new copy-url)
              (message "Copied: %s" copy-url))
          (message "No URL available"))))
     (t (message "No action selected")))))

;;; --- Suggestion Application ---

(defun shipit-apply-suggestion-at-point ()
  "Apply the suggested code change at point to the local file.
Reads suggestion metadata from text properties and replaces the
target lines in the file.  Requires the PR branch to be checked out."
  (interactive)
  (let ((suggestion-code (get-text-property (point) 'shipit-suggestion-code))
        (file-path (get-text-property (point) 'shipit-suggestion-path))
        (start-line (get-text-property (point) 'shipit-suggestion-start-line))
        (end-line (get-text-property (point) 'shipit-suggestion-end-line)))
    (unless (get-text-property (point) 'shipit-suggestion)
      (user-error "No suggestion at point"))
    (unless (and start-line end-line)
      (user-error "Cannot apply: line numbers unavailable for this suggestion"))
    (let* ((pr-data (bound-and-true-p shipit--current-displayed-pr))
           (head-ref (cdr (assq 'ref (cdr (assq 'head pr-data)))))
           (current-branch (when (fboundp 'magit-get-current-branch)
                             (magit-get-current-branch))))
      (when (and head-ref current-branch
                 (not (string= head-ref current-branch)))
        (user-error "Switch to branch %s first (currently on %s)" head-ref current-branch))
      (let* ((repo-root (shipit--get-repo-root))
             (full-path (expand-file-name file-path repo-root)))
        (unless (file-exists-p full-path)
          (user-error "File not found: %s (is the PR branch checked out?)" file-path))
        (unless (y-or-n-p (format "Apply suggested change to %s:%d? " file-path start-line))
          (user-error "Cancelled"))
        (shipit--apply-suggestion-to-file full-path start-line end-line suggestion-code)
        (message "Applied suggestion to %s:%d" file-path start-line)))))

(defun shipit--apply-suggestion-to-file (file-path start-line end-line new-code)
  "Replace lines START-LINE through END-LINE in FILE-PATH with NEW-CODE."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((lines (split-string (buffer-string) "\n"))
          (new-lines (split-string new-code "\n")))
      (setq lines (append (cl-subseq lines 0 (1- start-line))
                          new-lines
                          (cl-subseq lines end-line)))
      (erase-buffer)
      (insert (mapconcat #'identity lines "\n"))
      (write-region (point-min) (point-max) file-path))))

(defun shipit-suggestion-or-approve ()
  "Apply suggestion at point, or approve PR if not on a suggestion."
  (interactive)
  (if (get-text-property (point) 'shipit-suggestion)
      (shipit-apply-suggestion-at-point)
    (shipit-approve)))

;;; Merge

(defun shipit--merge-readiness-from-pr-data (pr-data)
  "Determine merge readiness from PR-DATA without API calls.
Works across backends by checking common fields."
  (let* ((state (shipit--get-pr-actual-state pr-data))
         (draft (cdr (assq 'draft pr-data)))
         (mergeable-state (cdr (assq 'mergeable_state pr-data))))
    (cond
     ((string= state "merged") "\U0001f389 Merged")
     ((string= state "closed") "\u274c Closed")
     ((and draft (not (eq draft :json-false))) "\U0001f6a7 Draft")
     ;; GitHub mergeable_state
     ((and (stringp mergeable-state) (string= mergeable-state "clean"))
      "\u2705 Ready to Merge")
     ((and (stringp mergeable-state) (string= mergeable-state "blocked"))
      "\U0001f6ab Blocked")
     ((and (stringp mergeable-state) (string= mergeable-state "dirty"))
      "\u26a0 Merge Conflict")
     ((and (stringp mergeable-state) (string= mergeable-state "unstable"))
      "\U0001f504 Checks Running")
     ((and (stringp mergeable-state) (string= mergeable-state "behind"))
      "\u23ea Behind Base")
     ;; Fallback
     (t "\u2753 Unknown"))))

(defun shipit--merge-guard (pr-data)
  "Signal user-error if PR-DATA is not in a mergeable state.
Checks actual state via `shipit--get-pr-actual-state' and the
draft field separately (GitHub returns state=open for drafts)."
  (let ((state (shipit--get-pr-actual-state pr-data))
        (draft (cdr (assq 'draft pr-data))))
    (cond
     ((string= state "merged")
      (user-error "PR is already merged"))
     ((string= state "closed")
      (user-error "PR is closed"))
     ((and draft (not (eq draft :json-false)))
      (user-error "PR is a draft — mark as ready before merging")))))

(defun shipit--merge-get-methods (repo)
  "Get allowed merge methods for REPO via backend.
Signals user-error if backend does not support :fetch-merge-methods."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :fetch-merge-methods)))
    (unless fn
      (user-error "Merge not yet supported for %s backend"
                  (plist-get backend :name)))
    (funcall fn config)))

(defun shipit--merge-ready-p (pr-data)
  "Return non-nil if PR-DATA indicates the PR can be merged.
Checks mergeable_state (normalized across backends at fetch time)."
  (let ((mergeable-state (cdr (assq 'mergeable_state pr-data)))
        (mergeable (cdr (assq 'mergeable pr-data))))
    (or (and (stringp mergeable-state) (string= mergeable-state "clean"))
        (and (null mergeable-state) (eq mergeable t)))))

(defun shipit--merge-pr-execute (repo number method)
  "Merge PR NUMBER in REPO using METHOD.
Dispatches to the backend :merge-pr operation and refreshes the buffer."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :merge-pr)))
    (funcall fn config number method)
    (message "PR #%d merged via %s" number method)
    (shipit-buffer-refresh)))

(defun shipit--merge-method-label (method)
  "Return display label for merge METHOD string."
  (cond
   ((string= method "merge") "Merge commit")
   ((string= method "squash") "Squash and merge")
   ((string= method "rebase") "Rebase and merge")))

(defun shipit--merge-method-key (method)
  "Return transient key for merge METHOD string."
  (cond
   ((string= method "merge") "m")
   ((string= method "squash") "s")
   ((string= method "rebase") "r")))

(defun shipit--merge-confirm-and-execute (method)
  "Confirm and execute merge via METHOD using buffer-local PR context."
  (let ((repo (bound-and-true-p shipit-buffer-repo))
        (number (bound-and-true-p shipit-buffer-pr-number)))
    (when (yes-or-no-p (format "Merge PR #%d via %s? " number method))
      (shipit--merge-pr-execute repo number method))))

(defun shipit-merge--do-merge ()
  "Merge via merge commit."
  (interactive)
  (shipit--merge-confirm-and-execute "merge"))

(defun shipit-merge--do-squash ()
  "Merge via squash."
  (interactive)
  (shipit--merge-confirm-and-execute "squash"))

(defun shipit-merge--do-rebase ()
  "Merge via rebase."
  (interactive)
  (shipit--merge-confirm-and-execute "rebase"))

(defvar shipit--merge-suffix-commands
  '(("merge" . shipit-merge--do-merge)
    ("squash" . shipit-merge--do-squash)
    ("rebase" . shipit-merge--do-rebase))
  "Alist mapping merge method strings to their interactive commands.")

(defun shipit-merge--quit ()
  "Dismiss the merge transient."
  (interactive)
  (transient-quit-one))

(defun shipit--merge-blocking-reasons (repo number pr-data)
  "Return list of strings explaining why PR NUMBER in REPO is blocked.
Checks reviews, status checks, and mergeable state from PR-DATA."
  (let* ((mergeable-state (cdr (assq 'mergeable_state pr-data)))
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (reasons nil))
    ;; Check reviews
    (let ((reviews-fn (plist-get backend :fetch-reviews))
          (base-ref (cdr (assq 'ref (cdr (assq 'base pr-data))))))
      (when reviews-fn
        (let* ((reviews (funcall reviews-fn config number))
               (approved (cl-count-if
                          (lambda (r) (string= (cdr (assq 'state r)) "APPROVED"))
                          reviews))
               (changes (cl-count-if
                         (lambda (r) (string= (cdr (assq 'state r)) "CHANGES_REQUESTED"))
                         reviews))
               (protection (when (and base-ref
                                      (plist-get backend :fetch-branch-protection))
                             (funcall (plist-get backend :fetch-branch-protection)
                                      config base-ref)))
               (required (when protection
                           (cdr (assq 'required-approving-review-count protection)))))
          (when (and (stringp mergeable-state) (string= mergeable-state "blocked"))
            (cond
             ((and required (> required 0))
              (push (format "Reviews: %d/%d required approvals" approved required)
                    reasons))
             ((= approved 0)
              (push "Reviews: no approvals" reasons))))
          (when (> changes 0)
            (push (format "Reviews: %d changes requested" changes) reasons)))))
    ;; Check status checks
    (let ((checks-fn (plist-get backend :fetch-checks)))
      (when checks-fn
        (let* ((checks (funcall checks-fn config number))
               (total (length checks))
               (passed (cl-count-if
                        (lambda (c) (string= (cdr (assq 'conclusion c)) "success"))
                        checks))
               (failed (cl-count-if
                        (lambda (c) (member (cdr (assq 'conclusion c))
                                            '("failure" "timed_out")))
                        checks))
               (pending (- total passed failed)))
          (when (> failed 0)
            (push (format "Checks: %d/%d failed" failed total) reasons))
          (when (> pending 0)
            (push (format "Checks: %d/%d still running" pending total) reasons)))))
    ;; Merge conflicts
    (when (and (stringp mergeable-state) (string= mergeable-state "dirty"))
      (push "Merge conflicts with base branch" reasons))
    ;; Behind base
    (when (and (stringp mergeable-state) (string= mergeable-state "behind"))
      (push "Branch is behind base — update required" reasons))
    (or (nreverse reasons) (list "Merge is blocked"))))

(transient-define-prefix shipit-merge ()
  "Merge the current pull request."
  [:class transient-column
   :description
   (lambda ()
     (let* ((pr-data (bound-and-true-p shipit-buffer-pr-data))
            (number (cdr (assq 'number pr-data)))
            (base-ref (or (cdr (assq 'ref (cdr (assq 'base pr-data))))
                          (cdr (assq 'target_branch pr-data))))
            (readiness (shipit--merge-readiness-from-pr-data pr-data)))
       (format "Merge PR #%s into %s\n\n  Status: %s"
               (or number "?") (or base-ref "?") readiness)))
   :setup-children
   (lambda (_)
     (let* ((pr-data (bound-and-true-p shipit-buffer-pr-data))
            (repo (bound-and-true-p shipit-buffer-repo))
            (number (bound-and-true-p shipit-buffer-pr-number))
            (ready (shipit--merge-ready-p pr-data))
            (methods (when ready (shipit--merge-get-methods repo))))
       (cond
        ;; Ready with methods: show merge options
        (methods
         (mapcar
          (lambda (method)
            (let ((cmd (cdr (assoc method shipit--merge-suffix-commands))))
              (transient-parse-suffix
               transient--prefix
               (list (shipit--merge-method-key method)
                     (shipit--merge-method-label method)
                     cmd))))
          methods))
        ;; Ready but no methods: repo doesn't allow direct merges
        (ready
         (list (transient-parse-suffix
                transient--prefix
                (list "q"
                      (propertize "No direct merge methods available"
                                  'face 'warning)
                      'shipit-merge--quit))))
        ;; Not ready: show specific blocking reasons
        (t
         (let ((reasons (shipit--merge-blocking-reasons repo number pr-data)))
           (mapcar
            (lambda (reason)
              (transient-parse-suffix
               transient--prefix
               (list "q"
                     (propertize reason 'face 'warning)
                     'shipit-merge--quit)))
            reasons))))))]
  (interactive)
  (unless (derived-mode-p 'shipit-mode)
    (user-error "Not in a shipit buffer"))
  (shipit--merge-guard (bound-and-true-p shipit-buffer-pr-data))
  (transient-setup 'shipit-merge))

(provide 'shipit-pr-actions)
;;; shipit-pr-actions.el ends here
