;;; shipit-issues-buffer.el --- Dedicated issue view buffer -*- lexical-binding: t; -*-

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
;; Dedicated buffer for viewing a single GitHub issue with magit sections.
;; Shows header, metadata, description, and comments.

;;; Code:

(require 'magit-section)
(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-render)

;; Forward declarations
(declare-function shipit-repo-subscription "shipit-repo-buffer")
(declare-function shipit--subscription-state-from-api "shipit-repo-buffer")
(declare-function shipit--subscription-state-label "shipit-repo-buffer")
(declare-function shipit-issues--fetch-issue "shipit-issues")
(declare-function shipit-issues--fetch-comments "shipit-issues")
(declare-function shipit-issues--fetch-comments-async "shipit-issues")
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--wrap-text "shipit-render")
(declare-function shipit--create-pr-reference-overlays "shipit-render")
(declare-function shipit--create-commit-sha-overlays "shipit-render")
(declare-function shipit--create-user-mention-overlays "shipit-render")
(declare-function shipit--create-jira-mention-overlays "shipit-issue-jira")
(declare-function shipit--create-custom-url-overlays "shipit-render")
(declare-function shipit--create-generic-url-overlays "shipit-render")
(declare-function shipit--try-overlay-action-at-point "shipit-render")
(declare-function shipit--apply-code-block-backgrounds-in-region "shipit-render")
(declare-function shipit--apply-strikethrough-faces "shipit-render")
(declare-function shipit--insert-comment-body-only "shipit-render")
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit--get-notification-source-icon "shipit-render")
(declare-function shipit--format-timestamp "shipit-core")
(declare-function shipit--create-avatar-display "shipit-render")
(declare-function shipit--insert-body-with-details "shipit-render")
(declare-function shipit--clean-text "shipit-core")
(declare-function shipit--insert-repo-url-line "shipit-buffer")
(declare-function shipit-open-repo-buffer "shipit-repo-buffer")
(declare-function shipit-editor-open "shipit-editor")
(declare-function shipit-issues--toggle-reaction "shipit-issues")
(declare-function shipit-register-dwim-handler "shipit-pr-actions")
(declare-function shipit-comment--fetch-reactions-batch "shipit-comments")
(declare-function shipit-comment--fetch-reactions "shipit-comments")
(declare-function shipit--user-has-reaction "shipit-http")
(declare-function shipit--add-reaction-to-comment "shipit-http")
(declare-function shipit--remove-reaction-from-comment "shipit-http")
(declare-function shipit-dwim "shipit-pr-actions")
(declare-function shipit--fetch-pr-reactions-sync "shipit-http")
(declare-function shipit--insert-description-reactions "shipit-http")
(declare-function shipit--update-description-reactions "shipit-http")
(declare-function shipit--populate-reaction-summary "shipit-http")
(declare-function shipit--prefetch-avatars-async "shipit-render")
(declare-function shipit--add-reaction-to-pr "shipit-http")
(declare-function shipit--remove-reaction-from-pr "shipit-http")
(declare-function shipit--get-current-user "shipit-pr-actions")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")
(declare-function shipit-issues--get-transitions "shipit-issues")
(declare-function shipit-issues--transition-status "shipit-issues")
(declare-function shipit-issue--backend-has-transitions-p "shipit-issue-backends")
(declare-function shipit-issue--backend-has-reactions-p "shipit-issue-backends")
(declare-function shipit-issue--backend-has-comment-reactions-p "shipit-issue-backends")
(declare-function shipit-issues--fetch-reactions "shipit-issues")
(declare-function shipit-issues--fetch-reactions-async "shipit-issues")
(declare-function shipit-issues--fetch-timeline-async "shipit-issues")
(declare-function shipit--refresh-section-targeted "shipit-core")
(declare-function shipit-issues--fetch-comments-head-tail-async "shipit-issues")
(declare-function shipit-issues--fetch-pinned-comment-async "shipit-issues")
(declare-function shipit-issues--add-reaction "shipit-issues")
(declare-function shipit-issues--remove-reaction "shipit-issues")
(declare-function shipit-gh-etag--user-state-key-p "shipit-gh-etag")
(declare-function shipit-toggle-timestamp-format "shipit-commands")
(declare-function shipit--notification-activity-key "shipit-notifications")
(declare-function shipit--mark-notification-read "shipit-notifications")

;; Forward variable declarations
(defvar shipit-render-markdown)
(defvar shipit-show-avatars)
(defvar shipit--dwim-handlers)
(defvar shipit-current-repo)
(defvar shipit--notification-pr-activities)

;;; Section type definitions

(defun shipit-issue (&rest _args)
  "Magit section identifier for issue header.")
(put 'shipit-issue 'magit-section t)

(defun shipit-issue-description (&rest _args)
  "Magit section identifier for issue description.")
(put 'shipit-issue-description 'magit-section t)

(defun shipit-issue-metadata (&rest _args)
  "Magit section identifier for issue metadata.")
(put 'shipit-issue-metadata 'magit-section t)

(defun issue-activity (&rest _args)
  "Magit section identifier for issue activity/changelog container.")
(put 'issue-activity 'magit-section t)

(defun issue-activity-event (&rest _args)
  "Magit section identifier for individual activity event.")
(put 'issue-activity-event 'magit-section t)

(defun issue-comments (&rest _args)
  "Magit section identifier for issue comments container.")
(put 'issue-comments 'magit-section t)

(defun issue-comment (&rest _args)
  "Magit section identifier for individual issue comment.")
(put 'issue-comment 'magit-section t)

;;; Buffer-local variables

(defvar-local shipit-issue-buffer-number nil
  "Issue number displayed in this buffer.")

(defvar-local shipit-issue-buffer-repo nil
  "Repository for the issue displayed in this buffer.")

(defvar-local shipit-issue-buffer-data nil
  "Cached issue data for this buffer.")

(defvar-local shipit-issue--child-items-cache nil
  "Cache of fetched child items for the current issue buffer.")

(defvar-local shipit-issue--linked-items-cache nil
  "Cache of linked items for the current issue buffer.")

(defvar-local shipit-issue--comment-position nil
  "Current comment position as (INDEX . TOTAL) or nil when not on a comment.")

(defvar-local shipit-issue--all-comments nil
  "Complete list of all comments for this issue.
Populated on first filter application via synchronous API fetch.
Used to re-render the comments section with filtered results.")

(defvar-local shipit-issue--active-filters nil
  "Alist of active comment filters.
Each entry is (TYPE . VALUE).  Supported types:
  author, since-days, search, hide-bots,
  min-reactions, min-positive, min-negative.")

(defun shipit-issue--combined-widths ()
  "Compute column widths from all cached child and linked items."
  (shipit-issue--compute-work-item-widths
   (append shipit-issue--child-items-cache
           shipit-issue--linked-items-cache)
   3))

(defvar shipit-issue-header-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "M-;") #'shipit-dwim)
    (define-key map (kbd "M-w") #'shipit-issue-copy-url)
    map)
  "Keymap for shipit issue header line.")

;;; Mode definition

(defvar shipit-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'shipit-issue-buffer-refresh)
    (define-key map (kbd "r") #'shipit-issue-buffer-refresh)
    (define-key map (kbd "o") #'shipit-issue--open-in-browser)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'shipit-issue-buffer-quit-and-kill)
    (define-key map (kbd "M-;") #'shipit-dwim)
    (define-key map (kbd "RET") #'shipit-issue--ret-dwim)
    (define-key map (kbd "L") #'shipit-toggle-timestamp-format)
    ;; Subscription
    (define-key map (kbd "w") #'shipit-repo-subscription)
    (define-key map (kbd "f") #'shipit-issue-filter-comments)
    (define-key map (kbd "l") #'shipit-issue-jump-to-load-more)
    (define-key map (kbd "R") #'shipit-issue--toggle-load-direction)
    map)
  "Keymap for `shipit-issue-mode'.")

;; Ensure bindings exist even when defvar didn't re-evaluate (reload case)
(define-key shipit-issue-mode-map (kbd "M-;") #'shipit-dwim)
(define-key shipit-issue-mode-map (kbd "RET") #'shipit-issue--ret-dwim)
(define-key shipit-issue-mode-map (kbd "L") #'shipit-toggle-timestamp-format)

(define-derived-mode shipit-issue-mode magit-section-mode "Shipit-Issue"
  "Major mode for shipit issue buffers.
Provides a read-only interface for viewing GitHub issues.

\\{shipit-issue-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-issue-buffer-refresh)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local tab-width 8)
  (setq-local magit-root-section nil)
  (shipit--apply-section-defaults)
  (font-lock-mode 1)
  (add-hook 'post-command-hook #'shipit-issue--update-comment-position nil t)
  (setq-local mode-line-process
              '(:eval (shipit-issue--mode-line-comment-position))))

;;; Buffer lifecycle

(defvar-local shipit-issue-buffer-ready-hook nil
  "Buffer-local hook run after an issue buffer's head+tail comments are rendered.
Activity-navigation callers (e.g. opening an issue from a notification)
wait on this to dispatch once the target comment is actually present in
the buffer.")

(defun shipit-issue-buffer-name (repo issue-number)
  "Generate buffer name for REPO and ISSUE-NUMBER."
  (format "*shipit-issue: %s#%s*" repo issue-number))

;;;###autoload
(defun shipit-issues-open-buffer (issue-number &optional repo backend-id backend-config)
  "Open dedicated buffer for issue ISSUE-NUMBER in REPO.
When BACKEND-ID and BACKEND-CONFIG are provided, set them as
buffer-local overrides so the issue fetches use the correct backend
\(e.g. Jira) instead of resolving from `shipit-issue-repo-backends'."
  (interactive
   (list (read-number "Issue number: ")
         (shipit--get-repo-from-remote)))
  (let* ((repo (or repo (shipit--get-repo-from-remote)))
         ;; Resolve backend from repo config when not explicitly provided
         (resolved (unless backend-id
                     (condition-case nil
                         (shipit-issue--resolve-for-repo repo)
                       (error nil))))
         (backend-id (or backend-id
                         (when resolved
                           (car (rassq (car resolved) shipit-issue-backends)))))
         (backend-config (or backend-config
                             (when resolved (cdr resolved))))
         (buffer-name (shipit-issue-buffer-name repo issue-number))
         (existing (get-buffer buffer-name)))
    ;; Auto-clear notification for this issue if one exists
    (when (and (boundp 'shipit--notification-pr-activities)
               shipit--notification-pr-activities)
      (let ((activity-key (shipit--notification-activity-key repo "issue" issue-number)))
        (when (gethash activity-key shipit--notification-pr-activities)
          (shipit--debug-log "Auto-clearing notification for issue %s opened via buffer" issue-number)
          (shipit--mark-notification-read issue-number repo t "issue"))))
    (cond
     (existing
      (shipit--debug-log "Switching to existing issue buffer: %s" buffer-name)
      (with-current-buffer existing
        (setq shipit-issue-buffer-number issue-number
              shipit-issue-buffer-repo repo)
        (when backend-id
          (setq-local shipit-issue-backend backend-id))
        (when backend-config
          (setq-local shipit-issue-backend-config backend-config)))
      (switch-to-buffer existing))
     (t
      (shipit--debug-log "Creating new issue buffer: %s" buffer-name)
      (let ((buffer (generate-new-buffer buffer-name))
            (repo-root (or (shipit--get-repo-root) default-directory)))
        (with-current-buffer buffer
          (shipit-issue-mode)
          (setq default-directory repo-root)
          (setq shipit-issue-buffer-number issue-number
                shipit-issue-buffer-repo repo)
          (when backend-id
            (setq-local shipit-issue-backend backend-id))
          (when backend-config
            (setq-local shipit-issue-backend-config backend-config))
          (shipit-issue-buffer-refresh))
        (switch-to-buffer buffer))))))

(defun shipit-issue-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the issue buffer contents."
  (interactive)
  (let ((repo shipit-issue-buffer-repo)
        (issue-number shipit-issue-buffer-number))
    (shipit--debug-log "Refreshing issue buffer for #%s in %s" issue-number repo)
    (message "Loading issue #%s..." issue-number)
    ;; Invalidate ETag cache so we get fresh data
    (when (fboundp 'shipit-gh-etag-invalidate-endpoint)
      (shipit-gh-etag-invalidate-endpoint
       (format "/repos/%s/issues/%s" repo issue-number)))
    (let ((issue-data (shipit-issues--fetch-issue repo issue-number)))
      (when issue-data
        (setq shipit-issue-buffer-data issue-data)
        ;; Description reactions are fetched asynchronously below, after
        ;; the initial render, so a large reactions list does not block
        ;; the buffer from becoming visible.
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (magit-insert-section (shipit-issue)
            (shipit-issue--insert-header-section repo issue-data issue-number)
            (shipit-issue--insert-metadata-section repo issue-data)
            (shipit-issue--insert-description-section repo issue-data issue-number)
            ;; Child work items -- placeholder, loaded async
            (shipit-issue--insert-child-items-section repo issue-data)
            ;; Linked work items -- collapsible section
            (let ((issuelinks (cdr (assq 'issuelinks issue-data))))
              (when issuelinks
                (let ((columns (shipit-issue--get-work-item-columns)))
                  (shipit-issue--insert-linked-items-section issuelinks columns))))
            (let* ((backend (shipit-issue--get-backend))
                   (timeline-async-p (and backend
                                          (plist-get backend :fetch-timeline-async))))
              (shipit-issue--insert-activity-section
               repo issue-data timeline-async-p))
            (shipit-issue--insert-comments-placeholder repo issue-number))
          (goto-char (min pos (point-max))))
        ;; Fetch description reactions asynchronously so the initial
        ;; render is not blocked. The description line is updated in
        ;; place when the fetch completes.
        (when (shipit-issue--backend-has-reactions-p (shipit-issue--get-backend))
          (let ((target-buffer (current-buffer)))
            (shipit-issues--fetch-reactions-async
             repo issue-number
             (lambda (_reactions)
               (when (buffer-live-p target-buffer)
                 (with-current-buffer target-buffer
                   (shipit-issue--update-description-reactions-display
                    issue-number)))))))
        ;; Fetch activity events asynchronously — the buffer rendered
        ;; with a "Loading activity…" placeholder which we replace when
        ;; the events payload arrives.
        (let ((target-buffer (current-buffer)))
          (shipit-issues--fetch-timeline-async
           repo issue-number
           (lambda (changelog)
             (when (buffer-live-p target-buffer)
               (with-current-buffer target-buffer
                 (shipit-issue--refresh-activity-section changelog))))))
        ;; Fetch pinned comment async and insert before comments section
        (let ((target-buffer (current-buffer)))
          (shipit-issues--fetch-pinned-comment-async
           repo issue-number
           (lambda (pinned-data)
             (when (and pinned-data (buffer-live-p target-buffer))
               (with-current-buffer target-buffer
                 (condition-case err
                     (progn
                       (let ((pinned-comment (plist-get pinned-data :comment)))
                         (when pinned-comment
                           (when (fboundp 'shipit--prefetch-avatars-async)
                             (shipit--prefetch-avatars-async
                              (list pinned-comment)))
                           (when (shipit-issue--backend-has-reactions-p
                                  (shipit-issue--get-backend))
                             (let ((shipit-current-repo repo))
                               (shipit-comment--fetch-reactions-batch
                                (list pinned-comment) repo nil)))))
                       (shipit-issue--insert-pinned-comment-before-comments
                        repo issue-number pinned-data))
                   (error
                    (let ((backtrace
                           (with-output-to-string
                             (let ((standard-output standard-output))
                               (backtrace)))))
                      (shipit--debug-log
                       "PINNED-ERROR: %s\nBACKTRACE:\n%s"
                       (error-message-string err) backtrace)))))))))
        ;; Fetch comments async
        (let ((target-buffer (current-buffer))
              (root-section magit-root-section))
          (shipit-issues--fetch-comments-head-tail-async
           repo issue-number
           shipit-comments-load-more-default
           shipit-comments-initial-count
           shipit-comments-tail-count
           (lambda (result)
             (let ((total (plist-get result :total)))
               (shipit--debug-log "Got head+tail for issue #%s, total=%d"
                                  issue-number total)
               (when (buffer-live-p target-buffer)
                 (with-current-buffer target-buffer
                   ;; Prefetch avatars in parallel — sync avatar fetches
                   ;; were the dominant per-comment cost, writing each cache
                   ;; file up front lets the sync render path skip the HTTP
                   ;; roundtrip entirely.
                   (when (fboundp 'shipit--prefetch-avatars-async)
                     (shipit--prefetch-avatars-async
                      (append (plist-get result :head)
                              (plist-get result :tail))))
                   ;; Fetch reactions only for visible head + tail comments
                   (let* ((visible (append (plist-get result :head)
                                           (plist-get result :tail)))
                          (shipit-current-repo repo)
                          (issue-backend (shipit-issue--get-backend))
                          (issue-reactions-fn
                           (plist-get issue-backend
                                      :fetch-comment-reactions-batch)))
                     (cond
                      ((and visible issue-reactions-fn)
                       (let ((config (cdr (shipit-issue--resolve-for-repo repo))))
                         (funcall issue-reactions-fn config repo visible nil)))
                      ((and visible
                            (shipit-issue--backend-has-reactions-p issue-backend))
                       (shipit-comment--fetch-reactions-batch visible repo nil))))
                   (let ((magit-root-section (or magit-root-section root-section)))
                     (shipit-issue--replace-comments-with-paginated-content
                      repo issue-number result))
                   (run-hooks 'shipit-issue-buffer-ready-hook))))))
          ;; Fetch child items async for epics
          (let ((issue-type (cdr (assq 'issue-type issue-data))))
            (when (and issue-type (string-equal-ignore-case issue-type "Epic"))
              (let* ((resolved (shipit-issue--resolve-for-repo repo))
                     (backend-plist (car resolved))
                     (config (cdr resolved))
                     (fetch-fn (plist-get backend-plist :fetch-children)))
                (when fetch-fn
                  (let ((columns (shipit-issue--get-work-item-columns)))
                    (run-at-time 0 nil
                      (lambda ()
                        (when (buffer-live-p target-buffer)
                          (condition-case err
                              (let ((children (funcall fetch-fn config
                                                       (cdr (assq 'id issue-data)))))
                                (with-current-buffer target-buffer
                                  (let ((magit-root-section (or magit-root-section root-section)))
                                    (shipit-issue--replace-child-items-with-content
                                     children columns))))
                            (error
                             (with-current-buffer target-buffer
                               (shipit-issue--replace-child-items-with-error
                                (error-message-string err))))))))))))))))
    (message "Issue #%s loaded" shipit-issue-buffer-number)))

(defun shipit-issue-buffer-quit-and-kill ()
  "Quit issue buffer and kill it."
  (interactive)
  (let ((buffer (current-buffer)))
    (quit-window)
    (kill-buffer buffer)))

;;; State face helper

;;; Section rendering

(defun shipit-issue--insert-header-section (repo issue-data issue-number)
  "Insert issue header section for ISSUE-NUMBER from REPO with ISSUE-DATA."
  (let* ((title (or (cdr (assq 'title issue-data)) "No title"))
         (state (or (cdr (assq 'state issue-data)) "unknown"))
         (icon-key (if (string= state "open") "issue-open" "issue-closed"))
         (emoji-fallback (if (string= state "open") "🟢" "🔴"))
         (state-face (shipit-issue--state-face state))
         (header-start (point)))
    (magit-insert-heading
      (format "%s %s %s #%s: %s"
              (shipit--get-notification-source-icon shipit-issue-backend)
              (shipit--get-pr-field-icon icon-key emoji-fallback)
              (propertize (capitalize state) 'face state-face)
              issue-number
              (string-trim (shipit--clean-text title))))
    (add-text-properties header-start (point)
                         `(shipit-issue-header t
                                               shipit-repo ,repo
                                               shipit-issue-number ,issue-number
                                               keymap ,shipit-issue-header-keymap
                                               help-echo "M-w: copy issue URL, M-;: shipit-dwim"))))

(defun shipit-issue--insert-metadata-section (repo issue-data)
  "Insert metadata section for ISSUE-DATA in REPO."
  (magit-insert-section (shipit-issue-metadata nil nil)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "metadata" "📋")
              (propertize "Metadata" 'face 'magit-section-heading)))
    (magit-insert-section-body
      ;; Repo URL
      (let ((repo-url (shipit-issue--derive-repo-url issue-data)))
        (when repo-url
          (insert "   ")
          (shipit--insert-repo-url-line repo-url repo)))
      ;; Author
      (let* ((user-obj (cdr (assq 'user issue-data)))
             (user (or (cdr (assq 'login user-obj)) "Unknown"))
             (avatar-url (cdr (assq 'avatar_url user-obj))))
        (insert (format "   %s Author:    %s%s\n"
                        (shipit--get-pr-field-icon "author" "👤")
                        (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                 (fboundp 'shipit--create-avatar-display))
                            (concat (shipit--create-avatar-display user avatar-url 16) " ")
                          "")
                        (propertize user 'face 'shipit-username-face))))
      ;; Type (Jira issues only)
      (let ((issue-type (cdr (assq 'issue-type issue-data))))
        (when issue-type
          (insert (format "   %s Type:      %s\n"
                          (shipit--get-pr-field-icon "type" "📦")
                          (propertize issue-type 'face 'font-lock-type-face)))))
      ;; Parent (Jira epic/parent issue)
      (let ((parent (cdr (assq 'parent issue-data))))
        (when parent
          (let ((key (cdr (assq 'key parent)))
                (summary (cdr (assq 'summary parent)))
                (status (cdr (assq 'status parent))))
            (insert (format "   %s Parent:    %s %s%s\n"
                            (shipit--get-pr-field-icon "parent" "🔼")
                            (propertize key
                                        'face 'link
                                        'shipit-issuelink-key key
                                        'keymap shipit-issue--issuelink-keymap
                                        'help-echo (format "RET: open %s" key))
                            (or summary "")
                            (if status
                                (format " (%s)" (propertize status 'face
                                                           (shipit-issue--state-face status)))
                              ""))))))
      ;; State
      (let* ((state (or (cdr (assq 'state issue-data)) "unknown"))
             (state-face (shipit-issue--state-face state)))
        (insert (format "   %s State:     %s\n"
                        (shipit--get-pr-field-icon "state" "📌")
                        (propertize (capitalize state) 'face state-face))))
      ;; Created
      (let* ((created (or (cdr (assq 'created_at issue-data)) ""))
             (formatted (if (and (fboundp 'shipit--format-timestamp)
                                 (not (string-empty-p created)))
                            (shipit--format-timestamp created)
                          created)))
        (insert (format "   %s Created:   %s\n"
                        (shipit--get-pr-field-icon "created" "📅")
                        (propertize formatted 'face 'shipit-timestamp-face))))
      ;; Updated
      (let* ((updated (or (cdr (assq 'updated_at issue-data)) ""))
             (formatted (if (and (fboundp 'shipit--format-timestamp)
                                 (not (string-empty-p updated)))
                            (shipit--format-timestamp updated)
                          updated)))
        (when (not (string-empty-p updated))
          (insert (format "   %s Updated:   %s\n"
                          (shipit--get-pr-field-icon "updated" "🔄")
                          (propertize formatted 'face 'shipit-timestamp-face)))))
      ;; Assignees
      (let ((assignees (cdr (assq 'assignees issue-data))))
        (let ((names (when (and assignees (> (length assignees) 0))
                       (mapcar (lambda (a) (cdr (assq 'login a))) assignees)))
              (line-start (point)))
          (insert (format "   %s Assignee: %s\n"
                          (shipit--get-pr-field-icon "assignees" "👥")
                          (if names
                              (mapconcat (lambda (n)
                                           (propertize n 'face 'shipit-username-face))
                                         names ", ")
                            (propertize "Unassigned" 'face 'font-lock-comment-face))))
          (add-text-properties line-start (point)
                               '(shipit-issue-assignee t))))
      ;; Labels
      (let ((labels (cdr (assq 'labels issue-data))))
        (when (and labels (> (length labels) 0))
          (let ((label-strs (mapcar (lambda (l)
                                      (let* ((name (cdr (assq 'name l)))
                                             (color (cdr (assq 'color l)))
                                             (face (when color
                                                     `(:foreground ,(concat "#" color)))))
                                        (if face
                                            (propertize name 'face face)
                                          name)))
                                    labels)))
            (insert (format "   %s Labels:    %s\n"
                            (shipit--get-pr-field-icon "labels" "🏷")
                            (mapconcat #'identity label-strs ", "))))))
      ;; Milestone
      (let ((milestone (cdr (assq 'milestone issue-data))))
        (when milestone
          (let ((title (cdr (assq 'title milestone))))
            (when title
              (insert (format "   %s Milestone: %s\n"
                              (shipit--get-pr-field-icon "milestone" "🎯")
                              (propertize title 'face 'font-lock-constant-face)))))))
      ;; Subscription status
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (fn (plist-get backend :get-repo-subscription)))
        (when fn
          (require 'shipit-repo-buffer)
          (let* ((sub-data (funcall fn (cdr resolved)))
                 (state (shipit--subscription-state-from-api sub-data))
                 (label (shipit--subscription-state-label state))
                 (star-fn (plist-get backend :get-repo-starred))
                 (starred (when star-fn (funcall star-fn (cdr resolved)))))
            (insert (propertize
                     (format "   %s Watching:  %s%s\n"
                             (shipit--get-pr-field-icon "notification" "\U0001f514")
                             label
                             (shipit--star-indicator starred))
                     'shipit-repo-subscription t)))))
      (insert "\n"))))

(defun shipit-issue--insert-description-section (repo issue-data issue-number)
  "Insert description section for issue ISSUE-DATA from REPO."
  (let* ((raw-body (cdr (assq 'body issue-data)))
         (clean-body (when (and raw-body (not (string-empty-p raw-body)))
                       (let ((cleaned (shipit--clean-text raw-body)))
                         (unless (string-match-p "\\`[[:space:]]*\\'" cleaned)
                           cleaned)))))
    (magit-insert-section (shipit-issue-description nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "description" "📝")
                (propertize "Description:" 'face 'markdown-metadata-key-face)))
      (magit-insert-section-body
        (let ((description-start (point)))
          (if (not clean-body)
              (insert (propertize "   No description provided\n" 'face 'italic))
            (if (string-match-p "<details>" clean-body)
                (shipit--insert-body-with-details clean-body 3)
              (let* ((rendered (if (and (boundp 'shipit-render-markdown) shipit-render-markdown
                                       (fboundp 'shipit--render-markdown))
                                  (shipit--render-markdown clean-body)
                                clean-body))
                     (wrapped (if (fboundp 'shipit--wrap-text)
                                  (shipit--wrap-text rendered 80 0)
                                rendered)))
                (insert (concat "   " (replace-regexp-in-string "\n" "\n   " wrapped) "\n")))))
          ;; Create overlays for references
          (when (fboundp 'shipit--create-pr-reference-overlays)
            (shipit--create-pr-reference-overlays repo issue-number description-start (point)))
          (when (fboundp 'shipit--create-commit-sha-overlays)
            (shipit--create-commit-sha-overlays repo description-start (point)))
          (when (fboundp 'shipit--create-user-mention-overlays)
            (shipit--create-user-mention-overlays repo description-start (point)))
          (when (fboundp 'shipit--create-jira-mention-overlays)
            (shipit--create-jira-mention-overlays description-start (point)))
          (when (fboundp 'shipit--create-custom-url-overlays)
            (shipit--create-custom-url-overlays description-start (point)))
          (when (fboundp 'shipit--create-generic-url-overlays)
            (shipit--create-generic-url-overlays description-start (point)))
          (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
            (shipit--apply-code-block-backgrounds-in-region description-start (point)))
          (when (fboundp 'shipit--apply-strikethrough-faces)
            (shipit--apply-strikethrough-faces description-start (point)))
          ;; Reactions block (backends with :fetch-reactions support)
          (when (shipit-issue--backend-has-reactions-p (shipit-issue--get-backend))
            ;; Prime summary counts from the issue response so the
            ;; initial render shows accurate numbers without waiting
            ;; for the async page-1 reactor fetch.
            (shipit--populate-reaction-summary
             issue-number (cdr (assq 'reactions issue-data)))
            (shipit--insert-description-reactions
             issue-number repo
             `(shipit-issue-description t
               shipit-issue-number ,issue-number
               shipit-repo ,repo)))
          (insert "\n"))))))

(defcustom shipit-pinned-comment-bg-color "#1a2744"
  "Background color for pinned comment sections.
Set to nil to disable the rounded background."
  :type '(choice color (const nil))
  :group 'shipit)

(defcustom shipit-pinned-comment-preview-lines 4
  "Number of lines to show in the pinned comment preview.
The full comment is available via a collapsed View full comment section."
  :type 'integer
  :group 'shipit)

(defun shipit-issue--insert-pinned-comment-section (repo issue-number pinned-data)
  "Insert a pinned comment section from PINNED-DATA for ISSUE-NUMBER in REPO.
PINNED-DATA is a plist with :comment :pinned-by :pinned-at.
Shows a truncated preview with a collapsible full comment body and
applies a rounded background when `shipit-pinned-comment-bg-color'
is set and the display is graphical."
  (let* ((comment (plist-get pinned-data :comment))
         (pinned-by (plist-get pinned-data :pinned-by))
         (pin-icon (shipit--get-pr-field-icon "pin" "📌"))
         (body (or (cdr (assq 'body comment)) ""))
         (user-obj (cdr (assq 'user comment)))
         (user (or (cdr (assq 'login user-obj)) "Unknown"))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (created (or (cdr (assq 'created_at comment)) ""))
         (timestamp (if (and (fboundp 'shipit--format-timestamp)
                             (not (string-empty-p created)))
                        (shipit--format-timestamp created)
                      created))
         (indent-level 3)
         (wrap-col (or (and (boundp 'shipit-render-wrap-column)
                             shipit-render-wrap-column)
                        120))
         (wrap-width (max 40 (- wrap-col indent-level))))
    (magit-insert-section (issue-pinned-comment comment)
      (magit-insert-heading
        (propertize (format "%s %s"
                            pin-icon
                            (propertize (format "Pinned by %s" pinned-by)
                                        'face 'font-lock-keyword-face))
                    'shipit-icon-color (or shipit-pinned-comment-bg-color "#1a2744")))
      (magit-insert-section-body
        ;; Comment header with avatar and username
        (insert (format "   %s%s  %s\n"
                        (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                 (fboundp 'shipit--create-avatar-display))
                            (concat (shipit--create-avatar-display user avatar-url 16) " ")
                          "")
                        (propertize user 'face 'shipit-username-face)
                        (propertize timestamp 'face 'shipit-timestamp-face)))
        ;; Truncated preview with wrapping
        (let* ((rendered (if (and (boundp 'shipit-render-markdown) shipit-render-markdown
                                  (fboundp 'shipit--render-markdown))
                             (shipit--render-markdown body)
                           body))
               (wrapped (shipit--wrap-text rendered wrap-width indent-level))
               (lines (split-string wrapped "\n"))
               (preview-n shipit-pinned-comment-preview-lines)
               (preview-lines (seq-take lines preview-n))
               (has-more (> (length lines) preview-n)))
          (dolist (line preview-lines)
            (insert "   " line "\n"))
          ;; Collapsible full comment body -- collapsed by default
          (when has-more
            (magit-insert-section (comment-details 'pinned-full)
              (oset magit-insert-section--current hidden t)
              (magit-insert-heading
                (propertize (format "   %s %s"
                        (shipit--get-pr-field-icon "description" "\u25b6")
                        (propertize "View full comment"
                                    'face 'magit-section-heading))
                    'shipit-icon-color (or shipit-pinned-comment-bg-color "#1a2744")))
              (magit-insert-section-body
                (dolist (line (seq-drop lines preview-n))
                  (insert "   " line "\n"))))))
        ;; Reactions
        (when (shipit-issue--backend-has-reactions-p
               (shipit-issue--get-backend))
          (let ((reactions (shipit--format-comment-reactions comment nil)))
            (when (and reactions (not (string-empty-p reactions)))
              (insert "   " reactions "\n"))))
        (insert "\n"))
      ;; Rounded background is applied by the caller after the
      ;; section is fully finalized with a valid end marker.
      )))

(defun shipit-issue--insert-pinned-comment-before-comments (repo issue-number pinned-data)
  "Insert a pinned comment section before the comments section.
Finds the issue-comments section and inserts before it, then updates
the parent's children list so magit section navigation works."
  (let ((comments-section (shipit--find-section-by-type 'issue-comments)))
    (when comments-section
      (let* ((inhibit-read-only t)
             (insert-pos (oref comments-section start))
             (parent (oref comments-section parent))
             new-section)
        (save-excursion
          (goto-char insert-pos)
          (let ((magit-insert-section--parent parent))
            (setq new-section
                  (shipit-issue--insert-pinned-comment-section
                   repo issue-number pinned-data))))
        ;; Apply rounded background now that section has valid end marker
        (when (and new-section shipit-pinned-comment-bg-color
                   (display-graphic-p)
                   (fboundp 'shipit-rounded--apply-to-section)
                   (oref new-section end))
          (shipit-rounded--apply-to-section
           new-section shipit-pinned-comment-bg-color)
          (dolist (child (oref new-section children))
            (shipit-rounded--apply-to-section
             child shipit-pinned-comment-bg-color)))
        ;; Fix children list: magit-insert-section auto-appends to parent,
        ;; so remove the auto-added entry first, then insert at the
        ;; correct position (before comments) for proper n/p navigation.
        (when (and parent new-section)
          (let* ((children (seq-remove (lambda (s) (eq s new-section))
                                       (oref parent children)))
                 (idx (seq-position children comments-section #'eq))
                 (fixed (if idx
                            (append (seq-take children idx)
                                    (list new-section)
                                    (seq-drop children idx))
                          (append children (list new-section)))))
            (oset parent children fixed)))))))

(defun shipit-issue--insert-comments-placeholder (_repo _issue-number)
  "Insert comments placeholder while loading.
The heading \"Comments (loading...)\" is rewritten in place when
the async head+tail arrives — the section itself is preserved so the
user never sees a blank rectangle during async comment insertion."
  (magit-insert-section (issue-comments nil nil)
    (magit-insert-heading
      (shipit-issue--comments-heading-text "(loading...)"))
    (magit-insert-section-body
      (insert "   Loading comments...\n")
      (insert "\n"))))

(defun shipit-issue--comments-heading-text (label)
  "Return the heading text for the issue-comments section.
LABEL is the count/status part (e.g. \"(42)\", \"(5 of 42)\",
\"(loading...)\") — the caller formats it, we prepend the icon and
apply the magit-section-heading face."
  (format "%s %s"
          (shipit--get-pr-field-icon "comment" "\U0001f4ac")
          (propertize (format "Comments %s" label)
                      'face 'magit-section-heading)))

(defun shipit-issue--replace-comments-body-with (heading-label insert-body-fn)
  "Replace the body of the issue-comments section in place.
HEADING-LABEL is the new text for the count portion of the heading
(e.g. \"(42)\" or \"(5 of 42)\").  INSERT-BODY-FN is called with
point at the section's content marker and should insert the new body
content (no outer `magit-insert-section' wrapper — the section object
is preserved).

The heading count is patched via `replace-match' on a narrow capture
(not a full heading line rewrite), so the section's `content' and
`end' markers stay anchored outside the heading.  Window-start is
saved and restored across the replacement because `redisplay' calls
inside `shipit--render-yield' will follow point into the inserted
body and scroll the window; without the save, the user's view ends
up near the Load-more line with nothing but empty space beneath it
until the tail comments render."
  (let ((section (shipit--find-section-by-type 'issue-comments)))
    (when section
      (let ((inhibit-read-only t)
            ;; Suppress progressive yields during the initial body replace.
            ;; With yields, Emacs redisplay auto-scrolls the window to follow
            ;; the growing insertion point, stranding the user's view at the
            ;; tail-comments gap.  Inserting everything in one synchronous
            ;; block means Emacs only redisplays AFTER we return, and it
            ;; keeps the user's window-start intact.
            (shipit-comments-render-chunk-size 0))
        (save-excursion
          ;; 1. Patch the count inside the heading via replace-match on a
          ;; narrow capture — leaves content/end markers intact.
          (goto-char (oref section start))
          (when (re-search-forward "Comments \\(([^)]+)\\)"
                                   (line-end-position) t)
            (replace-match heading-label t t nil 1))
          ;; 2. Clear stale child section records before rebuilding body.
          (oset section children nil)
          ;; 3. Replace body content between content and end markers.
          (let* ((content-pos (and (oref section content)
                                   (marker-position (oref section content))))
                 (end-pos (and (oref section end)
                               (marker-position (oref section end)))))
            (when (and content-pos end-pos)
              (when (> end-pos content-pos)
                (delete-region content-pos end-pos))
              (goto-char content-pos)
              (let ((magit-insert-section--parent section))
                (funcall insert-body-fn))
              (oset section end (point-marker))
              (oset section content (copy-marker content-pos)))))))))

(defun shipit-issue--replace-comments-with-content (repo issue-number comments)
  "Replace comments placeholder body with COMMENTS for ISSUE-NUMBER in REPO."
  (shipit--debug-log "ASYNC: Replacing issue comments body with %d comments"
                     (length comments))
  (shipit-issue--replace-comments-body-with
   (format "(%d)" (length comments))
   (lambda ()
     (shipit-issue--insert-comments-body repo issue-number comments))))

(defun shipit-issue--replace-comments-with-paginated-content (repo issue-number result)
  "Replace comments placeholder body with paginated RESULT for ISSUE-NUMBER.
RESULT is a plist from the head+tail fetcher."
  (let ((total (plist-get result :total)))
    (shipit--debug-log "ASYNC: Replacing issue comments body with paginated content, total=%d"
                       total)
    (shipit-issue--replace-comments-body-with
     (format "(%d)" total)
     (lambda ()
       (shipit-issue--insert-paginated-comments-body repo issue-number result)))))

(defcustom shipit-comments-pagination-threshold 30
  "Paginate issue comments when total count exceeds this threshold.
When the number of comments exceeds this value, only the first
`shipit-comments-initial-count' and last `shipit-comments-tail-count'
are rendered initially.  Hidden comments can be loaded via a
Load N more section.  Set to 0 to disable pagination."
  :type 'integer
  :group 'shipit)

(defcustom shipit-comments-initial-count 10
  "Number of oldest comments to show when pagination is active."
  :type 'integer
  :group 'shipit)

(defcustom shipit-comments-tail-count 10
  "Number of newest comments to show when pagination is active."
  :type 'integer
  :group 'shipit)

(defcustom shipit-comment-filter-auto-fetch t
  "Whether to auto-fetch all comments when a filter is applied.
When non-nil, applying a filter fetches all unfetched comments from
the API first to ensure complete results.  When nil, only loaded
comments are filtered and a message shows how many remain unfetched."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-comments-load-more-default 50
  "Default number of comments to load when pressing RET on Load more.
With C-u N, load exactly N.  With C-u alone, load all remaining."
  :type 'integer
  :group 'shipit)

(defun issue-comments-load-more (&rest _args)
  "Magit section identifier for the load-more placeholder."
  nil)
(put 'issue-comments-load-more 'magit-section t)

(defun issue-pinned-comment (&rest _args)
  "Magit section identifier for a pinned comment."
  nil)
(put 'issue-pinned-comment 'magit-section t)

(defcustom shipit-comments-render-chunk-size 2
  "Number of comments to render between yields to the event loop.
After every N comments are inserted into the issue buffer, the display
is refreshed and the event loop drains via `shipit--render-yield' so
(a) the user sees comments appearing progressively, (b) keystrokes and
scroll events stay responsive during the render, and (c) in-flight
async HTTP responses (avatar prefetches, reactions) can land between
chunks instead of piling up behind a long synchronous render.
Set to 0 to disable yielding."
  :type 'integer
  :group 'shipit)

(defun shipit-issue--insert-comments-chunked (repo issue-number comments)
  "Insert COMMENTS for ISSUE-NUMBER in REPO with progressive display refresh.
Calls `shipit--render-yield' after every `shipit-comments-render-chunk-size'
comments so the buffer stays responsive and partial content appears as it
is inserted.  When `shipit-comments-render-chunk-size' is 0 the yields are
suppressed — callers doing a bulk body replace bind it to 0 so the whole
insert runs synchronously, preventing Emacs from auto-scrolling the window
to follow the growing insertion point."
  (let ((chunk-size shipit-comments-render-chunk-size)
        (counter 0))
    (dolist (comment comments)
      (shipit-issue--insert-single-comment repo issue-number comment)
      (cl-incf counter)
      (when (and (> chunk-size 0)
                 (zerop (mod counter chunk-size)))
        (shipit--render-yield)))))

(defun shipit-issue--insert-paginated-comments-body (repo issue-number result)
  "Insert only the BODY of the paginated comments section.
RESULT has :head :tail :hidden-head :hidden-tail :total :unfetched :per-page.
Does not create a `magit-insert-section' — caller provides the section
context (either a fresh one at initial render time or the existing
section object during targeted body replacement)."
  (let* ((head (plist-get result :head))
         (tail (plist-get result :tail))
         (hidden-head (or (plist-get result :hidden-head) '()))
         (hidden-tail (or (plist-get result :hidden-tail) '()))
         (total (plist-get result :total))
         (unfetched (plist-get result :unfetched))
         (per-page (plist-get result :per-page))
         (hidden-count (+ (length hidden-head) (length hidden-tail)
                          (* (length unfetched) per-page))))
    (cond
     ((= total 0)
      (insert (propertize "   No comments\n" 'face 'font-lock-comment-face)))
     ((= hidden-count 0)
      (shipit-issue--insert-comments-chunked repo issue-number (append head tail)))
     (t
      (shipit-issue--insert-comments-chunked repo issue-number head)
      (shipit-issue--insert-load-more-section
       hidden-head repo issue-number unfetched per-page nil hidden-tail)
      (shipit-issue--insert-comments-chunked repo issue-number tail)))
    (insert "\n")))

(defun shipit-issue--insert-paginated-comments-section (repo issue-number result)
  "Insert full comments section (heading + body) from a head+tail RESULT plist.
Used at initial render time; async refresh uses
`shipit-issue--replace-comments-with-paginated-content' which updates
the body in place."
  (let ((total (plist-get result :total)))
    (magit-insert-section (issue-comments nil nil)
      (magit-insert-heading
        (shipit-issue--comments-heading-text (format "(%d)" total)))
      (magit-insert-section-body
        (shipit-issue--insert-paginated-comments-body repo issue-number result)))))

(defun shipit-issue--insert-load-more-section (hidden-comments repo issue-number
                                                               &optional unfetched-pages per-page direction
                                                               hidden-tail)
  "Insert a Load more section for HIDDEN-COMMENTS in REPO ISSUE-NUMBER.
The section stores the hidden comments and optional UNFETCHED-PAGES,
PER-PAGE, and DIRECTION for lazy API fetching.
DIRECTION is oldest or recent (default oldest)."
  (let* ((count (+ (length hidden-comments) (length (or hidden-tail '()))
                   (* (length unfetched-pages) (or per-page 0))))
         (dir (or direction 'recent))
         (dir-indicator (if (eq dir 'oldest) " [oldest first]" "")))
    (magit-insert-section (issue-comments-load-more
                           (list :comments hidden-comments
                                 :hidden-tail (or hidden-tail '())
                                 :repo repo
                                 :issue-number issue-number
                                 :unfetched unfetched-pages
                                 :per-page per-page
                                 :direction dir))
      (magit-insert-heading
        (propertize (format "   ── Load %d more comment%s%s ──"
                            count (if (= count 1) "" "s") dir-indicator)
                    'face 'font-lock-comment-face)))))

(defun shipit-issue--insert-comments-body (repo issue-number comments)
  "Insert only the BODY of the comments section for COMMENTS.
Does not create a `magit-insert-section' — caller provides the section
context.  Honours `shipit-comments-pagination-threshold' for local
pagination."
  (let* ((total (length comments))
         (threshold shipit-comments-pagination-threshold)
         (head-n shipit-comments-initial-count)
         (tail-n shipit-comments-tail-count)
         (paginate (and (> threshold 0)
                        (> total threshold)
                        (> total (+ head-n tail-n)))))
    (cond
     ((or (null comments) (= total 0))
      (insert (propertize "   No comments\n" 'face 'font-lock-comment-face)))
     (paginate
      (let ((head (seq-take comments head-n))
            (hidden (seq-subseq comments head-n (- total tail-n)))
            (tail (seq-subseq comments (- total tail-n))))
        (shipit-issue--insert-comments-chunked repo issue-number head)
        (shipit-issue--insert-load-more-section hidden repo issue-number)
        (shipit-issue--insert-comments-chunked repo issue-number tail)))
     (t
      (shipit-issue--insert-comments-chunked repo issue-number comments))))
  (insert "\n"))

(defun shipit-issue--insert-comments-section (repo issue-number comments)
  "Insert full comments section (heading + body) for COMMENTS.
Used at initial render time; async refresh uses
`shipit-issue--replace-comments-with-content' which updates the body
in place."
  (magit-insert-section (issue-comments nil nil)
    (magit-insert-heading
      (shipit-issue--comments-heading-text
       (format "(%d)" (length comments))))
    (magit-insert-section-body
      (shipit-issue--insert-comments-body repo issue-number comments))))

(defun shipit-issue--insert-single-comment (repo issue-number comment)
  "Insert a single COMMENT for issue ISSUE-NUMBER in REPO."
  (let* ((shipit-current-repo repo)
         (comment-id (cdr (assq 'id comment)))
         (user-obj (cdr (assq 'user comment)))
         (user (or (cdr (assq 'login user-obj)) "Unknown"))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (created (or (cdr (assq 'created_at comment)) ""))
         (timestamp (if (and (fboundp 'shipit--format-timestamp)
                             (not (string-empty-p created)))
                        (shipit--format-timestamp created)
                      created)))
    (magit-insert-section (issue-comment comment-id)
      (magit-insert-heading
        (format "   %s%s  %s"
                (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                         (fboundp 'shipit--create-avatar-display))
                    (concat (shipit--create-avatar-display user avatar-url 16) " ")
                  "")
                (propertize user 'face 'shipit-username-face)
                (propertize timestamp 'face 'shipit-timestamp-face)))
      (magit-insert-section-body
        ;; Indent 6 to align with username (3 spaces + emoji/avatar + space)
        ;; Skip reactions for backends without reaction support
        (shipit--insert-comment-body-only comment 6 nil nil repo issue-number nil
                                           (not (shipit-issue--backend-has-comment-reactions-p
                                                 (shipit-issue--get-backend))))
        (insert "\n")
        ;; Blank line separator between comments
        (insert "\n")))))

;;; Activity section rendering

(defun shipit-issue--changelog-max-user-width (changelog)
  "Return the max display width of usernames across CHANGELOG entries."
  (let ((max-w 0))
    (dolist (entry changelog max-w)
      (let* ((user-obj (cdr (assq 'user entry)))
             (user (or (cdr (assq 'login user-obj)) "Unknown"))
             (w (string-width user)))
        (when (> w max-w) (setq max-w w))))))

(defun shipit-issue--insert-activity-section (_repo issue-data &optional loading-p)
  "Insert activity/changelog section for ISSUE-DATA.
When LOADING-P is non-nil, render a \"Loading activity…\" placeholder
that `shipit-issue--refresh-activity-section' replaces once the async
events fetch completes.  Otherwise the caller has the final changelog
and we render it directly (empty list → \"No activity yet\")."
  (let ((changelog (cdr (assq 'changelog issue-data))))
    (magit-insert-section (issue-activity nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "activity" "📋")
                (propertize (if loading-p
                                "Activity (…)"
                              (format "Activity (%d)" (length changelog)))
                            'face 'magit-section-heading)))
      (cond
       (loading-p
        (insert (propertize "   Loading activity…\n"
                            'face 'font-lock-comment-face)))
       ((and changelog (> (length changelog) 0))
        (let ((user-col-width (shipit-issue--changelog-max-user-width changelog)))
          (dolist (entry (reverse changelog))
            (shipit-issue--insert-activity-event entry user-col-width))))
       (t
        (insert (propertize "   No activity yet\n"
                            'face 'font-lock-comment-face))))
      (insert "\n"))))

(defun shipit-issue--refresh-activity-section (changelog)
  "Replace the Activity section body with CHANGELOG entries.
Updates the heading count and rebuilds the body from CHANGELOG.
Called from the async events callback once the paginated fetch is done."
  (shipit--refresh-section-targeted
   'issue-activity
   "Activity (\\(…\\|[0-9]+\\))"
   (length (or changelog '()))
   (lambda (_section)
     (if (and changelog (> (length changelog) 0))
         (let ((user-col-width (shipit-issue--changelog-max-user-width changelog)))
           (dolist (entry (reverse changelog))
             (shipit-issue--insert-activity-event entry user-col-width)))
       (insert (propertize "   No activity yet\n"
                           'face 'font-lock-comment-face))))))

(defun shipit-issue--insert-activity-event (entry user-col-width)
  "Insert a single activity ENTRY as a magit subsection.
USER-COL-WIDTH is the fixed column width for aligning usernames.
Timestamps are right-aligned to the window edge."
  (let* ((event-id (cdr (assq 'id entry)))
         (user-obj (cdr (assq 'user entry)))
         (user (or (cdr (assq 'login user-obj)) "Unknown"))
         (created (or (cdr (assq 'created_at entry)) ""))
         (timestamp (if (and (fboundp 'shipit--format-timestamp)
                             (not (string-empty-p created)))
                        (shipit--format-timestamp created)
                      created))
         (items (cdr (assq 'items entry)))
         (description (shipit-issue--format-changelog-items items))
         ;; Pad username to fixed column width
         (user-pad (max 0 (- user-col-width (string-width user))))
         (user-col (concat (propertize user 'face 'shipit-username-face)
                           (make-string user-pad ?\s)))
         ;; Build left part: "   <user-col>  <description>"
         (left-part (format "   %s  %s" user-col description))
         ;; Fixed-width timestamp field (16 chars), right-aligned within column
         (ts-text (substring-no-properties timestamp))
         (ts-width (string-width ts-text))
         (ts-pad (max 0 (- 16 ts-width)))
         (ts-col (propertize (concat (make-string ts-pad ?\s) ts-text)
                             'face 'shipit-timestamp-face
                             'shipit-raw-timestamp created))
         ;; Right-align to window edge
         (win (or (get-buffer-window (current-buffer)) (selected-window)))
         (window-width (max 80 (window-width win)))
         (left-width (string-width left-part))
         (padding-needed (max 1 (- window-width left-width 16)))
         (padding (make-string padding-needed ?\s)))
    (magit-insert-section (issue-activity-event event-id)
      (magit-insert-heading
        (concat left-part padding ts-col)))))

(defun shipit-issue--format-changelog-items (items)
  "Format changelog ITEMS into a human-readable description string."
  (mapconcat
   (lambda (item)
     (let ((field (cdr (assq 'field item)))
           (from (cdr (assq 'from item)))
           (to (cdr (assq 'to item))))
       (cond
        ;; User comments: just show "commented"
        ((equal field "comment")
         (propertize "commented" 'face 'font-lock-keyword-face))
        ;; Mentioned in MR: "mentioned in !NNN"
        ((equal field "mentioned")
         (format "mentioned in %s" (propertize (or to "") 'face 'font-lock-keyword-face)))
        ;; Branch created: "created branch NAME"
        ((equal field "branch")
         (format "created branch %s" (propertize (or to "") 'face 'font-lock-keyword-face)))
        ;; Description changes: no from/to (too verbose)
        ((equal field "description")
         (format "updated %s" (propertize "Description" 'face 'font-lock-keyword-face)))
        ;; Status, assignee, and other field changes
        (t
         (let ((display-field (propertize (capitalize field) 'face 'font-lock-keyword-face))
               (has-from (and from (not (string-empty-p from))))
               (has-to (and to (not (string-empty-p to)))))
           (cond
            ((and has-from has-to)
             (format "changed %s: %s → %s" display-field from to))
            ((and has-from (not has-to))
             (format "removed %s from %s" from display-field))
            (t
             (format "set %s to %s" display-field to))))))))
   items ", "))

;;; Work item column helpers

(defun shipit-issue--get-work-item-columns ()
  "Get work item columns from the current backend, or default.
The backend :work-item-columns value may be a function (called to
get the current value) or a list used directly."
  (let* ((backend (shipit-issue--get-backend))
         (val (and backend (plist-get backend :work-item-columns))))
    (cond
     ((functionp val) (funcall val))
     (val val)
     (t '(work assignee status)))))

;;; Child Work Items section

(defun shipit-issue--insert-child-items-section (_repo issue-data)
  "Insert Child Work Items section if ISSUE-DATA is an Epic.
Shows placeholder; content loaded asynchronously."
  (let ((issue-type (cdr (assq 'issue-type issue-data))))
    (when (and issue-type (string-equal-ignore-case issue-type "Epic"))
      (magit-insert-section (issue-child-items nil nil)
        (magit-insert-heading
          (format "%s %s"
                  (shipit--get-pr-field-icon "children" "📦")
                  (propertize "Child Work Items (loading...)"
                              'face 'magit-section-heading)))
        (magit-insert-section-body
          (insert "   Loading child items...\n")
          (insert "\n"))))))

(defun shipit-issue--ticket-number (item)
  "Extract numeric ticket number from ITEM's key for sorting.
E.g. \"PRJ-42\" → 42."
  (let ((key (or (cdr (assq 'key item)) "")))
    (if (string-match "[0-9]+$" key)
        (string-to-number (match-string 0 key))
      0)))

(defun shipit-issue--replace-child-items-with-content (children columns)
  "Replace child items placeholder with actual CHILDREN data.
COLUMNS is a list of column symbols for the renderer."
  (setq shipit-issue--child-items-cache children)
  (let* ((sorted (sort (copy-sequence children)
                       (lambda (a b)
                         (< (shipit-issue--ticket-number a)
                            (shipit-issue--ticket-number b)))))
         (widths (shipit-issue--combined-widths)))
    (shipit--refresh-section-targeted
     'issue-child-items
     "Child Work Items (\\([^)]*\\))"
     (length sorted)
     (lambda (_section)
       (if sorted
           (dolist (child sorted)
             (magit-insert-section (issue-child-item child)
               (insert (format "   %s\n"
                               (shipit-issue--format-work-item-line
                                child columns widths)))))
         (insert "   No child work items\n"))))
    ;; Re-render linked items with shared widths
    (shipit-issue--refresh-linked-items-section columns widths)))

(defun shipit-issue--replace-child-items-with-error (message)
  "Replace child items placeholder with error MESSAGE."
  (let ((section (shipit--find-section-by-type 'issue-child-items)))
    (when section
      (let ((inhibit-read-only t))
        (save-excursion
          ;; Update heading FIRST (shifts positions)
          (goto-char (oref section start))
          (when (re-search-forward "Child Work Items ([^)]*)" (line-end-position) t)
            (replace-match "Child Work Items (error)"))
          ;; Extract positions AFTER heading update
          (let ((content-pos (marker-position (oref section content)))
                (end-pos (marker-position (oref section end))))
            ;; Clear children and washer
            (oset section children nil)
            (when (slot-boundp section 'washer)
              (oset section washer nil))
            ;; Remove invisible overlays
            (when (and content-pos end-pos)
              (remove-overlays content-pos end-pos 'invisible t))
            ;; Replace body
            (when (and content-pos end-pos (> end-pos content-pos))
              (delete-region content-pos end-pos))
            (goto-char content-pos)
            (insert (format "   Error loading children: %s\n"
                            (propertize message 'face 'error)))
            (insert "\n")
            (oset section end (point-marker))
            (oset section content (copy-marker content-pos))))))))

;;; Linked Work Items section

(defun shipit-issue--insert-linked-items-body (links columns widths)
  "Insert grouped linked item rows for LINKS using COLUMNS and WIDTHS."
  (let ((grouped (shipit-issue--group-issuelinks links)))
    (dolist (group grouped)
      (let ((type-label (car group))
            (entries (cdr group)))
        (magit-insert-section (issue-linked-group nil nil)
          (magit-insert-heading
            (propertize (format "   %s (%d)"
                                (capitalize type-label) (length entries))
                        'face 'magit-section-secondary-heading))
          (dolist (entry entries)
            (magit-insert-section (issue-linked-item entry)
              (insert (format "   %s\n"
                              (shipit-issue--format-work-item-line
                               entry columns widths))))))))))

(defun shipit-issue--insert-linked-items-section (links columns)
  "Insert collapsible Linked Work Items section for LINKS.
COLUMNS is a list of column symbols for the renderer.
Groups links by type and renders each item with the column renderer."
  (when (and links (> (length links) 0))
    (setq shipit-issue--linked-items-cache links)
    (let ((widths (shipit-issue--combined-widths)))
      (magit-insert-section (issue-linked-items nil nil)
        (magit-insert-heading
          (format "%s %s"
                  (shipit--get-pr-field-icon "links" "🔗")
                  (propertize (format "Linked Work Items (%d)" (length links))
                              'face 'magit-section-heading)))
        (shipit-issue--insert-linked-items-body links columns widths)
        (insert "\n")))))

(defun shipit-issue--refresh-linked-items-section (columns widths)
  "Re-render linked items section body with shared WIDTHS.
COLUMNS is the list of column symbols."
  (let ((links shipit-issue--linked-items-cache))
    (when links
      (shipit--refresh-section-targeted
       'issue-linked-items
       "Linked Work Items (\\([^)]*\\))"
       (length links)
       (lambda (_section)
         (shipit-issue--insert-linked-items-body links columns widths)
         (insert "\n"))))))

;;; Issue links helpers

(defun shipit-issue--group-issuelinks (links)
  "Group LINKS by type, returning alist of (TYPE . ENTRIES)."
  (let ((groups nil))
    (dolist (link links)
      (let* ((type (cdr (assq 'type link)))
             (existing (assoc type groups)))
        (if existing
            (setcdr existing (append (cdr existing) (list link)))
          (push (cons type (list link)) groups))))
    (nreverse groups)))

(defvar shipit-issue--issuelink-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-issue--open-linked-issue)
    (define-key map [mouse-1] #'shipit-issue--open-linked-issue)
    map)
  "Keymap for clickable issue link keys.")

(defun shipit-issue--open-linked-issue ()
  "Open the linked issue at point."
  (interactive)
  (let ((key (get-text-property (point) 'shipit-issuelink-key)))
    (when key
      (shipit-issues-open-buffer key shipit-issue-buffer-repo))))

;;; Targeted reactions display update

(defun shipit-issue--update-description-reactions-display (issue-number)
  "Update the issue description reactions display for ISSUE-NUMBER."
  (shipit--update-description-reactions
   issue-number 'shipit-issue-description
   (lambda ()
     (cl-find-if (lambda (b)
                   (with-current-buffer b
                     (and (derived-mode-p 'shipit-issue-mode)
                          (equal shipit-issue-buffer-number issue-number))))
                 (buffer-list)))
   (lambda ()
     (let ((repo (get-text-property (point) 'shipit-repo)))
       `(shipit-issue-description t
         shipit-issue-number ,issue-number
         shipit-repo ,repo)))))

;;; Helpers

(defun shipit-issue--browse-url ()
  "Return the browser URL for the current issue buffer."
  (let* ((repo shipit-issue-buffer-repo)
         (number shipit-issue-buffer-number)
         (resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (browse-fn (plist-get backend :browse-url)))
    (when (and browse-fn number)
      (funcall browse-fn config number))))

(defun shipit-issue--open-in-browser ()
  "Open the current issue in the browser."
  (interactive)
  (let ((url (shipit-issue--browse-url)))
    (if url
        (browse-url url)
      (message "No issue data available"))))

(defun shipit-issue-copy-url ()
  "Copy the issue URL to the kill ring.
If a region is active, copy the region text instead (standard M-w behavior)."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (let ((url (shipit-issue--browse-url)))
      (if url
          (progn
            (kill-new url)
            (message "Copied: %s" url))
        (user-error "No issue at point")))))

;;; RET handler

(defun shipit-issue--work-item-key-at-point ()
  "Return the issue key when point is on a child or linked work item line.
Extracts the key from the magit section value."
  (let ((section (magit-current-section)))
    (when (and section
               (memq (oref section type) '(issue-child-item issue-linked-item)))
      (cdr (assq 'key (oref section value))))))

(defun shipit-issue--work-item-actions (key)
  "Show an action menu for work item KEY.
Actions: open in buffer, open in browser, and optionally change status."
  (let* ((repo shipit-issue-buffer-repo)
         (resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (actions '("Open in buffer" "Open in browser"))
         (actions (if (plist-get backend :get-transitions)
                      (append actions '("Change status"))
                    actions))
         (choice (completing-read (format "%s action: " key) actions nil t)))
    (cond
     ((string= choice "Open in buffer")
      (shipit-issues-open-buffer key repo))
     ((string= choice "Open in browser")
      (let ((browse-fn (plist-get backend :browse-url)))
        (when browse-fn
          (browse-url (funcall browse-fn config key)))))
     ((string= choice "Change status")
      (shipit-issue--work-item-change-status key backend config)))))

(defun shipit-issue--work-item-change-status (key backend config)
  "Change the status of work item KEY using BACKEND and CONFIG.
Fetches available transitions, prompts the user, and executes."
  (let* ((get-fn (plist-get backend :get-transitions))
         (exec-fn (plist-get backend :transition-status))
         (transitions (funcall get-fn config key))
         (names (mapcar (lambda (tr) (cdr (assq 'name tr))) transitions))
         (choice (completing-read (format "Transition %s to: " key) names nil t))
         (selected (seq-find (lambda (tr) (equal choice (cdr (assq 'name tr))))
                             transitions))
         (transition-id (cdr (assq 'id selected))))
    (funcall exec-fn config key transition-id)
    (message "%s transitioned to %s" key choice)))

(defun shipit-issue--load-more-comments ()
  "Load more hidden comments from the Load more section at point.
With no prefix arg, loads `shipit-comments-load-more-default' comments.
With C-u N, loads exactly N comments.
With C-u alone, loads all remaining hidden comments.
Renders from in-memory hidden comments first.  When those are
exhausted and unfetched pages remain, fetches the next page from
the API and adds it to the pool."
  (interactive)
  (let* ((section (magit-current-section))
         (data (oref section value))
         (hidden (plist-get data :comments))
         (repo (plist-get data :repo))
         (issue-number (plist-get data :issue-number))
         (unfetched (plist-get data :unfetched))
         (per-page (plist-get data :per-page))
         (batch-size (cond
                      ((and current-prefix-arg (numberp current-prefix-arg))
                       current-prefix-arg)
                      (current-prefix-arg (+ (length hidden)
                                             (length (or (plist-get data :hidden-tail) '()))
                                             (* (length unfetched) (or per-page 0))))
                      (t shipit-comments-load-more-default))))
    ;; Check primary pool based on direction
    (let* ((dir (or (plist-get data :direction) 'recent))
           (hidden-tail (or (plist-get data :hidden-tail) '()))
           (primary-len (length (if (eq dir 'recent) hidden-tail hidden))))
      (if (or (>= primary-len batch-size) (null unfetched))
          (shipit-issue--load-more-from-memory section data batch-size)
        ;; Need to fetch more from API first
        ;; Recent: fetch last unfetched page; oldest: fetch first
        (let* ((next-page (if (eq dir 'recent)
                              (car (last unfetched))
                            (car unfetched)))
               (remaining-pages (if (eq dir 'recent)
                                    (butlast unfetched)
                                  (cdr unfetched)))
               (target-buffer (current-buffer)))
          (message "Fetching more comments...")
          (let* ((resolved (shipit-issue--resolve-for-repo repo))
                 (config (cdr resolved))
                 (base-url (format "%s/repos/%s/issues/%s/comments"
                                   (or shipit-api-url "https://api.github.com")
                                   repo issue-number))
                 (url (format "%s?per_page=%d&page=%d" base-url per-page next-page))
                 (headers `(("Accept" . "application/vnd.github+json")
                            ("Authorization" . ,(format "Bearer %s" (shipit--github-token)))
                            ("X-GitHub-Api-Version" . "2022-11-28"))))
            (shipit--url-retrieve-async
             url "GET" headers nil
             (lambda (page-data)
               (when (buffer-live-p target-buffer)
                 (with-current-buffer target-buffer
                   ;; Add fetched data to the correct pool based on direction
                   (let ((updated-data (copy-sequence data)))
                     (if (eq dir 'recent)
                         (setq updated-data
                               (plist-put updated-data :hidden-tail
                                          (append (or page-data '()) hidden-tail)))
                       (setq updated-data
                             (plist-put updated-data :comments
                                        (append hidden (or page-data '())))))
                     (setq updated-data
                           (plist-put updated-data :unfetched remaining-pages))
                     (oset section value updated-data))
                   (shipit-issue--load-more-from-memory
                    section (oref section value) batch-size))))
             (lambda (err)
               (message "Failed to fetch comments: %s" err)))))))))

(defun shipit-issue--load-more-from-memory (section data batch-size)
  "Render BATCH-SIZE comments from in-memory DATA in SECTION."
  (let* ((hidden (plist-get data :comments))
         (repo (plist-get data :repo))
         (issue-number (plist-get data :issue-number))
         (unfetched (plist-get data :unfetched))
         (per-page (plist-get data :per-page))
         (hidden-tail (or (plist-get data :hidden-tail) '()))
         (direction (or (plist-get data :direction) 'recent))
         ;; Select pool based on direction:
         ;; oldest-first: consume hidden (old/page1) first
         ;; recent-first: consume hidden-tail (new/last-page) first
         (primary (if (eq direction 'recent) hidden-tail hidden))
         (secondary (if (eq direction 'recent) hidden hidden-tail))
         (to-load (if (eq direction 'recent)
                      (nreverse (seq-take (reverse primary) batch-size))
                    (seq-take primary batch-size)))
         (primary-remaining (if (eq direction 'recent)
                            (seq-subseq primary 0 (max 0 (- (length primary) batch-size)))
                          (seq-drop primary batch-size)))
         ;; If primary insufficient and no unfetched, take from secondary
         (extra-needed (max 0 (- batch-size (length to-load))))
         (to-load (if (and (> extra-needed 0) (null unfetched))
                      (append to-load (seq-take secondary extra-needed))
                    to-load))
         (secondary-remaining (if (and (> extra-needed 0) (null unfetched))
                                  (seq-drop secondary extra-needed)
                                secondary))
         (remaining (if (eq direction 'recent) secondary-remaining primary-remaining))
         (remaining-tail (if (eq direction 'recent) primary-remaining secondary-remaining))
         (inhibit-read-only t)
         (section-start (oref section start))
         (section-end (oref section end))
         (parent (oref section parent)))
    ;; Fetch reactions for the batch before rendering
    (when (and to-load
               (shipit-issue--backend-has-reactions-p
                (shipit-issue--get-backend)))
      (let ((shipit-current-repo repo))
        (shipit-comment--fetch-reactions-batch to-load repo nil)))
    ;; Delete the load-more section
    (delete-region section-start section-end)
    ;; Insert comments and new load-more in the correct order based on direction.
    ;; Oldest-first: [comments] [load-more]  (gap is below new comments)
    ;; Recent-first: [load-more] [comments]  (gap is above new comments)
    (save-excursion
      (goto-char section-start)
      (let ((magit-insert-section--parent parent))
        (if (eq direction 'recent)
            (progn
              (when (or remaining unfetched)
                (shipit-issue--insert-load-more-section
                 remaining repo issue-number unfetched per-page direction))
              (shipit-issue--insert-comments-chunked repo issue-number to-load))
          (shipit-issue--insert-comments-chunked repo issue-number to-load)
          (when (or remaining unfetched)
            (shipit-issue--insert-load-more-section
             remaining repo issue-number unfetched per-page direction)))))
    ;; Fix parent's children list: remove old load-more and sort by
    ;; buffer position so n/p navigation works correctly with the
    ;; newly inserted sections.
    (when parent
      (oset parent children
            (sort (seq-remove (lambda (s) (eq s section))
                              (oref parent children))
                  (lambda (a b)
                    (< (or (oref a start) 0)
                       (or (oref b start) 0))))))
    (message "Loaded %d comment%s%s"
             (length to-load)
             (if (= 1 (length to-load)) "" "s")
             (let ((total-remaining (+ (length remaining)
                                       (* (length unfetched) (or per-page 0)))))
               (if (> total-remaining 0)
                   (format " (%d remaining)" total-remaining)
                 "")))))

(defun shipit-issue--ret-dwim ()
  "Handle RET in issue buffers.
Overlay actions (URLs, references) take priority, then work item
lines, then Load more, then comment DWIM, then section toggle.
With prefix arg (C-u), show action menu for work items or control
how many comments to load."
  (interactive)
  (unless (shipit--try-overlay-action-at-point)
    (let ((work-item-key (shipit-issue--work-item-key-at-point))
          (section (magit-current-section)))
      (cond
       ((and work-item-key current-prefix-arg)
        (shipit-issue--work-item-actions work-item-key))
       (work-item-key
        (shipit-issues-open-buffer work-item-key shipit-issue-buffer-repo))
       ((and section (eq (oref section type) 'issue-comments-load-more))
        (shipit-issue-load-more-menu))
       ((shipit-issue--comment-id-at-point)
        (shipit-dwim))
       (t
        (magit-section-toggle (magit-current-section)))))))

;;; DWIM handler and comment actions

(defun shipit-issue--comment-id-at-point ()
  "Return the issue comment ID at point.
Checks text properties first, then falls back to the magit section value."
  (or (get-text-property (point) 'shipit-comment-id)
      (when (fboundp 'magit-current-section)
        (let ((section (magit-current-section)))
          (when (magit-section-match '(issue-comment) section)
            (oref section value))))))

(defun shipit-issue--comment-actions ()
  "Handle DWIM actions for issue comments.
Presents an action menu for the comment at point."
  (let* ((comment-id (shipit-issue--comment-id-at-point))
         (comment-body (get-text-property (point) 'shipit-comment-body))
         (repo shipit-issue-buffer-repo)
         (issue-number shipit-issue-buffer-number)
         (actions '("Add comment" "Reply to comment" "React to comment" "Edit comment"))
         (choice (completing-read "Issue comment action: " actions nil t)))
    (cond
     ((string= choice "Add comment")
      (shipit-issue--add-comment-directly repo issue-number))
     ((string= choice "Reply to comment")
      (shipit-issue--reply-to-comment repo issue-number comment-id comment-body))
     ((string= choice "React to comment")
      (shipit-issue--react-to-comment repo comment-id))
     ((string= choice "Edit comment")
      (shipit-issue--edit-comment-interactive repo comment-id comment-body)))))

(defun shipit-issue--add-comment-directly (repo issue-number)
  "Add a new comment to issue ISSUE-NUMBER in REPO."
  (shipit-editor-open
   (list :type 'issue-comment
         :source-buffer (current-buffer)
         :pr-number issue-number
         :repo repo)))

(defun shipit-issue--reply-to-comment (repo issue-number _comment-id comment-body)
  "Reply to a comment on issue ISSUE-NUMBER in REPO.
_COMMENT-ID is unused (issue comments are flat, no threading).
COMMENT-BODY is the parent comment text for quoting."
  (let ((quoted (when comment-body
                  (concat "> " (replace-regexp-in-string
                                "\n" "\n> "
                                (shipit--clean-text comment-body))
                          "\n\n"))))
    (shipit-editor-open
     (list :type 'issue-comment
           :source-buffer (current-buffer)
           :pr-number issue-number
           :repo repo
           :initial-content (or quoted "")))))

(defun shipit-issue--react-to-comment (repo comment-id)
  "Toggle a reaction on issue comment COMMENT-ID in REPO.
Fetches current reactions, checks if user already reacted, adds or removes."
  (unless (shipit-issue--backend-has-reactions-p (shipit-issue--get-backend))
    (user-error "Reactions are not supported by the %s backend" shipit-issue-backend))
  (let* ((choice (completing-read "Toggle reaction: " shipit-reaction-choices))
         (reaction-type (cdr (assoc choice shipit-reaction-choices)))
         (issue-number shipit-issue-buffer-number))
    (when reaction-type
      (let ((shipit-current-repo repo))
        ;; Fetch fresh reactions so toggle check is accurate
        (shipit-comment--fetch-reactions repo comment-id nil)
        (if (shipit--user-has-reaction comment-id reaction-type nil)
            (shipit--remove-reaction-from-comment comment-id reaction-type repo issue-number nil nil)
          (shipit--add-reaction-to-comment comment-id reaction-type repo issue-number nil nil)))
      (shipit-issue-buffer-refresh))))

(defun shipit-issue--edit-comment-interactive (repo comment-id comment-body)
  "Edit issue comment COMMENT-ID in REPO.
COMMENT-BODY is the current body to pre-populate."
  (shipit-editor-open
   (list :type 'issue-comment-edit
         :source-buffer (current-buffer)
         :comment-id comment-id
         :repo repo
         :initial-content (or (shipit--clean-text comment-body) ""))))

(defun shipit-issue--edit-description (repo issue-number)
  "Edit the description of issue ISSUE-NUMBER in REPO."
  (let* ((issue-data shipit-issue-buffer-data)
         (raw-body (cdr (assq 'body issue-data)))
         (body (or (and raw-body (shipit--clean-text raw-body)) "")))
    (shipit-editor-open
     (list :type 'issue-description
           :source-buffer (current-buffer)
           :pr-number issue-number
           :repo repo
           :initial-content body))))

(defun shipit-issue--react-to-description ()
  "Toggle a reaction on the issue description."
  (let ((backend (shipit-issue--get-backend)))
    (unless (shipit-issue--backend-has-reactions-p backend)
      (user-error "Reactions are not supported by the %s backend" shipit-issue-backend))
    (let* ((repo shipit-issue-buffer-repo)
           (issue-number shipit-issue-buffer-number)
           (choice (completing-read "Toggle reaction: " shipit-reaction-choices))
           (reaction-type (cdr (assoc choice shipit-reaction-choices))))
      (when reaction-type
        ;; Fetch fresh reactions via issue backend
        (let* ((reactions (shipit-issues--fetch-reactions repo issue-number))
               (current-user (shipit--get-current-user))
               (has-reaction (cl-find-if
                              (lambda (r)
                                (and (string= (cdr (assq 'content r)) reaction-type)
                                     (string= (cdr (assq 'login (cdr (assq 'user r))))
                                              current-user)))
                              reactions)))
          (if has-reaction
              (shipit-issues--remove-reaction repo issue-number
                                              (cdr (assq 'id has-reaction)))
            (shipit-issues--add-reaction repo issue-number reaction-type))
          (shipit-issue--update-description-reactions-display issue-number))))))

(defun shipit-issue--change-status ()
  "Change the status of the current issue via workflow transitions.
Fetches available transitions and presents a completing-read menu."
  (interactive)
  (let* ((repo shipit-issue-buffer-repo)
         (issue-number shipit-issue-buffer-number)
         (transitions (shipit-issues--get-transitions repo issue-number)))
    (if (or (null transitions) (= (length transitions) 0))
        (message "No transitions available for this issue")
      (let* ((choices (mapcar (lambda (tr) (cdr (assq 'name tr))) transitions))
             (choice (completing-read "Change status to: " choices nil t)))
        (when choice
          (let ((transition (cl-find-if (lambda (tr) (equal choice (cdr (assq 'name tr))))
                                        transitions)))
            (when transition
              (shipit-issues--transition-status repo issue-number (cdr (assq 'id transition)))
              (message "Transitioned issue to %s" choice)
              (shipit-issue-buffer-refresh))))))))

(defun shipit-issue--transitions-available-p ()
  "Return non-nil if the current issue backend supports transitions."
  (let* ((repo shipit-issue-buffer-repo)
         (resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved)))
    (shipit-issue--backend-has-transitions-p backend)))

(defun shipit-issue--description-actions ()
  "Handle DWIM actions for issue description section.
Offers to edit description, add a comment, react, or change status."
  (let* ((repo shipit-issue-buffer-repo)
         (issue-number shipit-issue-buffer-number)
         (actions (append '("Edit description" "Add comment" "React to description")
                          (when (shipit-issue--transitions-available-p)
                            '("Change status"))))
         (choice (completing-read "Description action: " actions nil t)))
    (cond
     ((string= choice "Edit description")
      (shipit-issue--edit-description repo issue-number))
     ((string= choice "Add comment")
      (shipit-issue--add-comment-directly repo issue-number))
     ((string= choice "React to description")
      (shipit-issue--react-to-description))
     ((string= choice "Change status")
      (shipit-issue--change-status)))))

(defun shipit-issue--change-assignee ()
  "Change the assignee of the current issue."
  (let* ((backend (shipit-issue--get-backend))
         (config (when (bound-and-true-p shipit-issue-backend-config)
                   shipit-issue-backend-config))
         (fetch-users-fn (plist-get backend :fetch-assignable-users))
         (update-assignee-fn (plist-get backend :update-assignee))
         (issue-number shipit-issue-buffer-number)
         (repo shipit-issue-buffer-repo))
    (unless update-assignee-fn
      (user-error "This backend does not support changing assignees"))
    (let* ((users (if fetch-users-fn
                     (funcall fetch-users-fn config)
                   '()))
           (choices (cons "Unassign" users))
           (selected (completing-read "Assignee: " choices nil t)))
      (funcall update-assignee-fn config issue-number
               (unless (string= selected "Unassign") selected))
      (message "Assignee updated to %s" (if (string= selected "Unassign") "none" selected))
      (shipit-issue-buffer-refresh))))

(defun shipit-issue--metadata-actions ()
  "Handle DWIM actions for issue metadata section.
Offers to change status when transitions are supported."
  (if (shipit-issue--transitions-available-p)
      (shipit-issue--change-status)
    (message "No actions available for metadata")))

(defun shipit-issue--comments-header-actions ()
  "Handle DWIM actions for the issue comments section header.
Offers to add a new comment."
  (let ((repo shipit-issue-buffer-repo)
        (issue-number shipit-issue-buffer-number))
    (shipit-issue--add-comment-directly repo issue-number)))

;; Register DWIM handlers (LIFO: registered last = matched first)

(defun shipit-issue--register-dwim-handlers ()
  "Register issue-buffer DWIM handlers.
Called both at load time (if shipit-magit loaded) and via `with-eval-after-load'."
  (when (fboundp 'shipit-register-dwim-handler)
    ;; Issue metadata — change status (when supported)
    (shipit-register-dwim-handler
     'issue-metadata
     (lambda () (and (derived-mode-p 'shipit-issue-mode)
                     (fboundp 'magit-current-section)
                     (magit-section-match '(shipit-issue-metadata) (magit-current-section))))
     #'shipit-issue--metadata-actions)

    ;; Issue assignee — change assignee (registered after metadata so it matches first)
    (shipit-register-dwim-handler
     'issue-assignee
     (lambda () (and (derived-mode-p 'shipit-issue-mode)
                     (get-text-property (point) 'shipit-issue-assignee)))
     #'shipit-issue--change-assignee)

    ;; Comments section header — add new comment
    (shipit-register-dwim-handler
     'issue-comments-header
     (lambda () (and (derived-mode-p 'shipit-issue-mode)
                     (fboundp 'magit-current-section)
                     (magit-section-match '(issue-comments) (magit-current-section))
                     (not (get-text-property (point) 'shipit-comment-id))))
     #'shipit-issue--comments-header-actions)

    ;; Issue description — add new comment
    (shipit-register-dwim-handler
     'issue-description
     (lambda () (and (derived-mode-p 'shipit-issue-mode)
                     (fboundp 'magit-current-section)
                     (magit-section-match '(shipit-issue-description) (magit-current-section))
                     (not (get-text-property (point) 'shipit-comment-id))))
     #'shipit-issue--description-actions)

    ;; Issue comment — reply, react, edit (registered last = checked first)
    ;; Matches on comment body (shipit-comment-id property) OR heading (issue-comment section)
    (shipit-register-dwim-handler
     'issue-comment
     (lambda () (and (derived-mode-p 'shipit-issue-mode)
                     (shipit-issue--comment-id-at-point)))
     #'shipit-issue--comment-actions)

    ;; Subscription — registered after metadata so it matches first (LIFO)
    (shipit-register-dwim-handler
     'issue-subscription
     (lambda () (and (derived-mode-p 'shipit-issue-mode)
                     (get-text-property (point) 'shipit-repo-subscription)))
     (lambda ()
       (if (fboundp 'shipit-repo-subscription)
           (shipit-repo-subscription)
         (user-error "shipit-repo-buffer not loaded"))))))

;; Register now if shipit-pr-actions is already loaded
(shipit-issue--register-dwim-handlers)
;; Also register when shipit-pr-actions loads later
(with-eval-after-load 'shipit-pr-actions
  (shipit-issue--register-dwim-handlers))

(defun shipit-issue--derive-repo-url (issue-data)
  "Derive the repository URL from ISSUE-DATA.
Uses html_url and strips the issue-specific suffix."
  (when-let* ((issue-url (cdr (assq 'html_url issue-data))))
    (replace-regexp-in-string "/\\(?:issues\\|-/issues\\)/[0-9]+/?$" "" issue-url)))

;;; Comment filters

(defun shipit-issue--extract-author-from-section (section)
  "Extract the comment author from SECTION heading via shipit-username-face."
  (let ((start (oref section start))
        (content (oref section content))
        (result nil))
    (when (and start content)
      (save-excursion
        (goto-char start)
        (while (and (< (point) content) (not result))
          (let ((face (get-text-property (point) 'face)))
            (when (eq face 'shipit-username-face)
              (setq result (buffer-substring-no-properties
                            (point)
                            (or (next-single-property-change (point) 'face nil content)
                                content)))))
          (goto-char (or (next-single-property-change (point) 'face nil content) content)))))
    result))

(defun shipit-issue--extract-timestamp-from-section (section)
  "Extract the raw ISO timestamp from SECTION heading."
  (let ((start (oref section start))
        (content (oref section content)))
    (when (and start content)
      (let ((pos start)
            (result nil))
        (while (and pos (< pos content) (not result))
          (setq result (get-text-property pos 'shipit-raw-timestamp))
          (unless result
            (setq pos (next-single-property-change pos 'shipit-raw-timestamp nil content))))
        result))))

(defun shipit-issue--comment-matches-filters-p (section)
  "Return non-nil if comment SECTION matches all active filters.
Extracts author and timestamp from section text properties.
Searches buffer text for content matching.  Looks up reaction
cache by comment ID for reaction filters."
  (let ((comment-id (oref section value))
        (match t))
    (when shipit-issue--active-filters
      (dolist (filter shipit-issue--active-filters)
        (let ((type (car filter))
              (value (cdr filter)))
          (when (and value match)
            (pcase type
              ('author
               (let ((author (shipit-issue--extract-author-from-section section)))
                 (unless (and author (string-equal-ignore-case value author))
                   (setq match nil))))
              ('since-days
               (let ((ts (shipit-issue--extract-timestamp-from-section section)))
                 (when ts
                   (condition-case nil
                       (let ((cutoff (time-subtract (current-time) (days-to-time value))))
                         (when (time-less-p (date-to-time ts) cutoff)
                           (setq match nil)))
                     (error nil)))))
              ('search
               (let* ((sect-start (oref section start))
                      (sect-end (oref section end))
                      (text (buffer-substring-no-properties sect-start sect-end)))
                 (unless (string-match-p (regexp-quote value) text)
                   (setq match nil))))
              ('hide-bots
               (let ((author (shipit-issue--extract-author-from-section section)))
                 (when (and author (string-match-p "\\[bot\\]$" author))
                   (setq match nil))))
              ('min-reactions
               (when (and comment-id (numberp comment-id))
                 (let* ((cached (gethash (shipit--reaction-cache-key
                                          shipit-issue-buffer-repo comment-id nil)
                                         shipit--reaction-cache))
                        (count (length (or cached '()))))
                   (when (< count value)
                     (setq match nil)))))
              ('min-positive
               (when (and comment-id (numberp comment-id))
                 (let* ((cached (gethash (shipit--reaction-cache-key
                                          shipit-issue-buffer-repo comment-id nil)
                                         shipit--reaction-cache))
                        (positive (seq-count
                                   (lambda (r)
                                     (member (cdr (assq 'content r))
                                             '("+1" "heart" "hooray" "rocket")))
                                   (or cached '()))))
                   (when (< positive value)
                     (setq match nil)))))
              ('min-negative
               (when (and comment-id (numberp comment-id))
                 (let* ((cached (gethash (shipit--reaction-cache-key
                                          shipit-issue-buffer-repo comment-id nil)
                                         shipit--reaction-cache))
                        (negative (seq-count
                                   (lambda (r)
                                     (member (cdr (assq 'content r))
                                             '("-1" "confused")))
                                   (or cached '()))))
                   (when (< negative value)
                     (setq match nil))))))))))
    match))


(defun shipit-issue--fetch-all-comments-sync (repo issue-number)
  "Fetch ALL comments for ISSUE-NUMBER in REPO synchronously.
Returns the complete list. Shows progress messages during fetch."
  (message "Loading all comments for issue #%s..." issue-number)
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-comments)))
    (if fetch-fn
        (let ((comments (funcall fetch-fn config issue-number)))
          (message "Loaded %d comments" (length comments))
          comments)
      (message "Backend does not support comment fetching")
      nil)))

(defun shipit-issue--update-comment-position ()
  "Update the current comment position for mode-line display.
Called via `post-command-hook'."
  (let ((section (magit-current-section))
        (pos nil))
    ;; Walk up to find the issue-comment section
    (let ((s section))
      (while (and s (not (eq (oref s type) 'issue-comment)))
        (setq s (oref s parent)))
      (when (and s (eq (oref s type) 'issue-comment))
        ;; Find the parent comments section
        (let ((comments-parent (oref s parent)))
          (when comments-parent
            (let ((children (oref comments-parent children))
                  (idx 0)
                  (total 0)
                  (found nil))
              (dolist (child children)
                (when (eq (oref child type) 'issue-comment)
                  (cl-incf total)
                  (when (eq child s)
                    (setq found total)))
                (when (eq (oref child type) 'issue-comments-load-more)
                  (let ((data (oref child value)))
                    (cl-incf total
                             (+ (length (plist-get data :comments))
                                (length (or (plist-get data :hidden-tail) '()))
                                (* (length (plist-get data :unfetched))
                                   (or (plist-get data :per-page) 0)))))))
              (when found
                (setq pos (cons found total))))))))
    (setq shipit-issue--comment-position pos)))

(defun shipit-issue--mode-line-comment-position ()
  "Return mode-line string showing comment position."
  (when shipit-issue--comment-position
    (let* ((idx (car shipit-issue--comment-position))
           (total (cdr shipit-issue--comment-position))
           (pct (if (> total 0) (/ (* 100 idx) total) 0))
           (bar-width 10)
           (filled (/ (* bar-width idx) (max total 1)))
           (empty (- bar-width filled))
           (bar (concat (make-string filled ?#)
                        (make-string empty ?-))))
      (format " [%d/%d %s]" idx total bar))))

(defun shipit-issue--comment-matches-filters-p-data (comment)
  "Return non-nil if COMMENT alist matches all active filters.
Works on raw comment data, not magit sections."
  (let ((match t))
    (dolist (filter shipit-issue--active-filters)
      (let ((type (car filter))
            (value (cdr filter)))
        (when (and value match)
          (pcase type
            ('author
             (let ((author (cdr (assq 'login (cdr (assq 'user comment))))))
               (unless (and author (string-equal-ignore-case value author))
                 (setq match nil))))
            ('since-days
             (let ((created (cdr (assq 'created_at comment))))
               (when created
                 (condition-case nil
                     (let ((cutoff (time-subtract (current-time) (days-to-time value))))
                       (when (time-less-p (date-to-time created) cutoff)
                         (setq match nil)))
                   (error nil)))))
            ('search
             (let ((body (or (cdr (assq 'body comment)) "")))
               (unless (string-match-p (regexp-quote value) body)
                 (setq match nil))))
            ('hide-bots
             (let ((author (cdr (assq 'login (cdr (assq 'user comment)))))
                   (user-type (cdr (assq 'type (cdr (assq 'user comment))))))
               (when (or (and author (string-match-p "\\[bot\\]$" author))
                         (equal user-type "Bot"))
                 (setq match nil))))
            ('min-reactions
             (let* ((comment-id (cdr (assq 'id comment)))
                    (cached (gethash (shipit--reaction-cache-key
                                      shipit-issue-buffer-repo comment-id nil)
                                     shipit--reaction-cache))
                    (count (length (or cached '()))))
               (when (< count value)
                 (setq match nil))))
            ('min-positive
             (let* ((comment-id (cdr (assq 'id comment)))
                    (cached (gethash (shipit--reaction-cache-key
                                      shipit-issue-buffer-repo comment-id nil)
                                     shipit--reaction-cache))
                    (positive (seq-count
                               (lambda (r)
                                 (member (cdr (assq 'content r))
                                         '("+1" "heart" "hooray" "rocket")))
                               (or cached '()))))
               (when (< positive value)
                 (setq match nil))))
            ('min-negative
             (let* ((comment-id (cdr (assq 'id comment)))
                    (cached (gethash (shipit--reaction-cache-key
                                      shipit-issue-buffer-repo comment-id nil)
                                     shipit--reaction-cache))
                    (negative (seq-count
                               (lambda (r)
                                 (member (cdr (assq 'content r))
                                         '("-1" "confused")))
                               (or cached '()))))
               (when (< negative value)
                 (setq match nil))))))))
    match))

(defun shipit-issue--apply-comment-filters ()
  "Apply active filters by re-rendering the comments section.
Fetches all comments on first filter if not already loaded."
  (let ((repo shipit-issue-buffer-repo)
        (issue-number shipit-issue-buffer-number))
    ;; Fetch all comments if not yet loaded
    (unless shipit-issue--all-comments
      (setq shipit-issue--all-comments
            (shipit-issue--fetch-all-comments-sync repo issue-number))
      ;; Fetch reactions for all comments
      (when (and shipit-issue--all-comments
                 (shipit-issue--backend-has-reactions-p
                  (shipit-issue--get-backend)))
        (message "Fetching reactions...")
        (let ((shipit-current-repo repo))
          (shipit-comment--fetch-reactions-batch
           shipit-issue--all-comments repo nil))))
    ;; Filter and re-render
    (when shipit-issue--all-comments
      (let* ((filtered (seq-filter
                        #'shipit-issue--comment-matches-filters-p-data
                        shipit-issue--all-comments))
             (total (length shipit-issue--all-comments))
             (shown (length filtered)))
        (shipit-issue--replace-comments-with-filtered-content
         repo issue-number filtered total)
        (shipit-issue--update-filter-status)
        (message "Showing %d of %d comments" shown total)))))

(defun shipit-issue--insert-filtered-comments-body (repo issue-number comments)
  "Insert only the BODY of the filtered comments section.
Does not create a `magit-insert-section'.  The caller provides the
section context and the heading (which includes the \"N of M\"
filter count)."
  (if (null comments)
      (insert (propertize "   No matching comments\n" 'face 'font-lock-comment-face))
    (shipit-issue--insert-comments-chunked repo issue-number comments))
  (insert "\n"))

(defun shipit-issue--insert-filtered-comments-section (repo issue-number comments total)
  "Insert full filtered comments section (heading + body).
TOTAL is the unfiltered comment count; used in the \"N of M\" heading."
  (magit-insert-section (issue-comments nil nil)
    (magit-insert-heading
      (shipit-issue--comments-heading-text
       (format "(%d of %d)" (length comments) total)))
    (magit-insert-section-body
      (shipit-issue--insert-filtered-comments-body repo issue-number comments))))

(defun shipit-issue--replace-comments-with-filtered-content (repo issue-number comments total)
  "Replace comments section body with filtered COMMENTS.
TOTAL is the unfiltered comment count; the heading becomes
\"Comments (shown of total)\"."
  (shipit-issue--replace-comments-body-with
   (format "(%d of %d)" (length comments) total)
   (lambda ()
     (shipit-issue--insert-filtered-comments-body repo issue-number comments))))

(defun shipit-issue--update-filter-status ()
  "Insert or update a filter status line in the comments section.
Shows active filter details, match count, and unloaded count."
  (let ((comments-section (shipit--find-section-by-type 'issue-comments)))
    (when comments-section
      (let ((inhibit-read-only t)
            (content-pos (oref comments-section content)))
        (save-excursion
          ;; Remove existing filter status line if present
          (goto-char content-pos)
          (when (and (< (point) (point-max))
                     (get-text-property (point) 'shipit-filter-status))
            (let ((end (or (next-single-property-change (point) 'shipit-filter-status)
                          (1+ (point)))))
              (delete-region (point) end)))
          ;; Insert new status line if filters are active
          (when shipit-issue--active-filters
            (goto-char content-pos)
            (let* ((filters-desc
                    (mapconcat
                     (lambda (f)
                       (let ((type (car f))
                             (val (cdr f)))
                         (when val
                           (pcase type
                             ('author (format "author=%s" val))
                             ('since-days (format "last %d days" val))
                             ('search (format "search=%s" val))
                             ('hide-bots "hide-bots")
                             ('min-reactions (format "reactions>=%d" val))
                             ('min-positive (format "positive>=%d" val))
                             ('min-negative (format "negative>=%d" val))))))
                     shipit-issue--active-filters " + "))
                   (status-text (format "   Filter: %s\n" (string-trim filters-desc))))
              (insert (propertize status-text
                                 'face 'font-lock-warning-face
                                 'shipit-filter-status t)))))))))


(defun shipit-issue--filter-by-author ()
  "Filter comments to a specific author."
  (interactive)
  (let* ((comments-section (shipit--find-section-by-type 'issue-comments))
         (authors '()))
    ;; Collect authors from visible comments AND in-memory hidden comments
    (when comments-section
      ;; From rendered comment sections
      (dolist (child (oref comments-section children))
        (when (eq (oref child type) 'issue-comment)
          (let ((author (shipit-issue--extract-author-from-section child)))
            (when (and author (not (member author authors)))
              (push author authors))))
        ;; From load-more section's in-memory hidden comments
        (when (eq (oref child type) 'issue-comments-load-more)
          (let ((hidden (plist-get (oref child value) :comments)))
            (dolist (comment hidden)
              (let ((author (cdr (assq 'login (cdr (assq 'user comment))))))
                (when (and author (not (member author authors)))
                  (push author authors))))))))
    (let ((chosen (completing-read "Filter by author: " (sort authors #'string<) nil t)))
      (setf (alist-get 'author shipit-issue--active-filters) chosen)
      (shipit-issue--apply-comment-filters)
      (message "Filtered to author: %s" chosen))))

(defun shipit-issue--filter-by-date ()
  "Filter comments to the last N days."
  (interactive)
  (let ((days (read-number "Show comments from last N days: " 7)))
    (setf (alist-get 'since-days shipit-issue--active-filters) days)
    (shipit-issue--apply-comment-filters)
    (message "Filtered to last %d days" days)))

(defun shipit-issue--filter-by-search ()
  "Filter comments containing a search string."
  (interactive)
  (let ((query (read-string "Search comments for: ")))
    (setf (alist-get 'search shipit-issue--active-filters) query)
    (shipit-issue--apply-comment-filters)
    (message "Filtered to comments containing: %s" query)))

(defun shipit-issue--filter-hide-bots ()
  "Toggle hiding bot comments."
  (interactive)
  (let ((current (alist-get 'hide-bots shipit-issue--active-filters)))
    (if current
        (setf (alist-get 'hide-bots shipit-issue--active-filters) nil)
      (setf (alist-get 'hide-bots shipit-issue--active-filters) t))
    (shipit-issue--apply-comment-filters)
    (message "Bot comments: %s" (if (alist-get 'hide-bots shipit-issue--active-filters)
                                    "hidden" "shown"))))

(defun shipit-issue--filter-by-min-reactions ()
  "Filter to comments with at least N reactions."
  (interactive)
  (let ((n (read-number "Minimum total reactions: " 5)))
    (setf (alist-get 'min-reactions shipit-issue--active-filters) n)
    (shipit-issue--apply-comment-filters)
    (message "Filtered to comments with >= %d reactions" n)))

(defun shipit-issue--filter-by-positive-reactions ()
  "Filter to comments with at least N positive reactions."
  (interactive)
  (let ((n (read-number "Minimum positive reactions: " 3)))
    (setf (alist-get 'min-positive shipit-issue--active-filters) n)
    (shipit-issue--apply-comment-filters)
    (message "Filtered to comments with >= %d positive reactions" n)))

(defun shipit-issue--filter-by-negative-reactions ()
  "Filter to comments with at least N negative reactions."
  (interactive)
  (let ((n (read-number "Minimum negative reactions: " 1)))
    (setf (alist-get 'min-negative shipit-issue--active-filters) n)
    (shipit-issue--apply-comment-filters)
    (message "Filtered to comments with >= %d negative reactions" n)))

(defun shipit-issue--clear-comment-filters ()
  "Clear all comment filters and show all comments."
  (interactive)
  (setq shipit-issue--active-filters nil)
  (shipit-issue--remove-all-filter-overlays)
  ;; Re-render with all comments or original pagination
  (let* ((repo shipit-issue-buffer-repo)
         (issue-number shipit-issue-buffer-number)
         (all shipit-issue--all-comments))
    (when all
      (let ((total (length all)))
        (shipit-issue--replace-comments-with-filtered-content
         repo issue-number all total)))
    (shipit-issue--update-filter-status))
  (message "Comment filters cleared"))

(defun shipit-issue--toggle-load-direction ()
  "Toggle load direction between oldest-first and recent-first.
Only works when point is on a Load more section."
  (interactive)
  (let ((section (magit-current-section)))
    (when (and section (eq (oref section type) 'issue-comments-load-more))
      (let* ((data (oref section value))
             (current-dir (or (plist-get data :direction) 'oldest))
             (new-dir (if (eq current-dir 'oldest) 'recent 'oldest))
             (new-data (plist-put (copy-sequence data) :direction new-dir))
             (inhibit-read-only t)
             (parent (oref section parent))
             (start (oref section start))
             (end (oref section end)))
        ;; Replace the section with updated direction
        (save-excursion
          (delete-region start end)
          (goto-char start)
          (let ((magit-insert-section--parent parent))
            (shipit-issue--insert-load-more-section
             (plist-get new-data :comments)
             (plist-get new-data :repo)
             (plist-get new-data :issue-number)
             (plist-get new-data :unfetched)
             (plist-get new-data :per-page)
             new-dir
             (plist-get new-data :hidden-tail))))
        ;; Fix parent children list
        (when parent
          (oset parent children
                (seq-remove (lambda (s) (eq s section))
                            (oref parent children))))
        (message "Load direction: %s first" new-dir)))))

(defun shipit-issue--add-filter-overlay (section)
  "Add an invisible overlay over SECTION to hide it."
  (let ((ov (make-overlay (oref section start) (oref section end))))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'shipit-filter-overlay t)))

(defun shipit-issue--remove-filter-overlay (section)
  "Remove any filter overlay from SECTION."
  (dolist (ov (overlays-in (oref section start) (oref section end)))
    (when (overlay-get ov 'shipit-filter-overlay)
      (delete-overlay ov))))

(defun shipit-issue--remove-all-filter-overlays ()
  "Remove all filter overlays from the current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'shipit-filter-overlay)
      (delete-overlay ov))))

(defun shipit-issue-jump-to-load-more ()
  "Jump to the Load more section and open the transient menu.
If no Load more section exists, shows a message."
  (interactive)
  (let ((comments-section (shipit--find-section-by-type 'issue-comments)))
    (if (not comments-section)
        (message "No comments section found")
      (let ((load-more (cl-find-if
                        (lambda (c) (eq (oref c type) 'issue-comments-load-more))
                        (oref comments-section children))))
        (if (not load-more)
            (message "All comments are loaded")
          (goto-char (oref load-more start))
          (shipit-issue-load-more-menu))))))

(defun shipit-issue--load-more-default ()
  "Load the default batch of comments."
  (interactive)
  (shipit-issue--load-more-comments))

(defun shipit-issue--load-more-all ()
  "Load all remaining comments by fetching everything from the API."
  (interactive)
  (let ((repo shipit-issue-buffer-repo)
        (issue-number shipit-issue-buffer-number))
    ;; Fetch all comments if not already cached
    (unless shipit-issue--all-comments
      (setq shipit-issue--all-comments
            (shipit-issue--fetch-all-comments-sync repo issue-number))
      (when (and shipit-issue--all-comments
                 (shipit-issue--backend-has-reactions-p
                  (shipit-issue--get-backend)))
        (message "Fetching reactions...")
        (let ((shipit-current-repo repo))
          (shipit-comment--fetch-reactions-batch
           shipit-issue--all-comments repo nil))))
    ;; Re-render with all comments (no pagination)
    (when shipit-issue--all-comments
      (let ((total (length shipit-issue--all-comments)))
        (shipit-issue--replace-comments-with-filtered-content
         repo issue-number shipit-issue--all-comments total)
        (message "All %d comments loaded" total)))))

(defun shipit-issue--load-more-n ()
  "Load N comments (prompt for count)."
  (interactive)
  (let ((n (read-number "Load how many comments: " shipit-comments-load-more-default)))
    (let ((current-prefix-arg n))
      (shipit-issue--load-more-comments))))

(transient-define-prefix shipit-issue-load-more-menu ()
  "Actions for loading more comments."
  [:description
   (lambda ()
     (let* ((section (magit-current-section))
            (data (when (and section (eq (oref section type) 'issue-comments-load-more))
                    (oref section value)))
            (dir (if data (or (plist-get data :direction) 'oldest) 'oldest))
            (count (if data
                       (+ (length (plist-get data :comments))
                          (* (length (plist-get data :unfetched))
                             (or (plist-get data :per-page) 0)))
                     0)))
       (format "Load More Comments (%d remaining, %s first)"
               count dir)))
   ("RET" "Load next batch" shipit-issue--load-more-default)
   ("a" "Load all" shipit-issue--load-more-all)
   ("n" "Load N" shipit-issue--load-more-n)
   ("r" "Toggle direction" shipit-issue--toggle-load-direction :transient t)])

(transient-define-prefix shipit-issue-filter-comments ()
  "Filter issue comments."
  [:description
   (lambda ()
     (if shipit-issue--active-filters
         (format "Comment Filters [%d active]"
                 (seq-count (lambda (f) (cdr f)) shipit-issue--active-filters))
       "Comment Filters"))
   ("a" "By author" shipit-issue--filter-by-author)
   ("d" "Since date" shipit-issue--filter-by-date)
   ("s" "Search text" shipit-issue--filter-by-search)
   ("b" "Hide/show bots" shipit-issue--filter-hide-bots)
   ("r" "Min reactions" shipit-issue--filter-by-min-reactions)
   ("+" "Min positive reactions" shipit-issue--filter-by-positive-reactions)
   ("-" "Min negative reactions" shipit-issue--filter-by-negative-reactions)
   ("c" "Clear all filters" shipit-issue--clear-comment-filters)])

(provide 'shipit-issues-buffer)
;;; shipit-issues-buffer.el ends here
