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
(declare-function shipit--add-reaction-to-pr "shipit-http")
(declare-function shipit--remove-reaction-from-pr "shipit-http")
(declare-function shipit--get-current-user "shipit-pr-actions")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")
(declare-function shipit-issues--get-transitions "shipit-issues")
(declare-function shipit-issues--transition-status "shipit-issues")
(declare-function shipit-issue--backend-has-transitions-p "shipit-issue-backends")
(declare-function shipit-issue--backend-has-reactions-p "shipit-issue-backends")
(declare-function shipit-issues--fetch-reactions "shipit-issues")
(declare-function shipit-issues--fetch-reactions-async "shipit-issues")
(declare-function shipit-issues--fetch-comments-head-tail-async "shipit-issues")
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
  (font-lock-mode 1))

;;; Buffer lifecycle

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
            (shipit-issue--insert-activity-section repo issue-data)
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
                   ;; Fetch reactions only for visible head + tail comments
                   (let* ((visible (append (plist-get result :head)
                                           (plist-get result :tail)))
                          (shipit-current-repo repo))
                     (when visible
                       (shipit-comment--fetch-reactions-batch visible repo nil)))
                   (let ((magit-root-section (or magit-root-section root-section)))
                     (shipit-issue--replace-comments-with-paginated-content
                      repo issue-number result)))))))
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
            (shipit--insert-description-reactions
             issue-number repo
             `(shipit-issue-description t
               shipit-issue-number ,issue-number
               shipit-repo ,repo)))
          (insert "\n"))))))

(defun shipit-issue--insert-comments-placeholder (_repo _issue-number)
  "Insert comments placeholder while loading."
  (magit-insert-section (issue-comments nil nil)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "comment" "💬")
              (propertize "Comments (loading...)" 'face 'magit-section-heading)))
    (magit-insert-section-body
      (insert "   Loading comments...\n")
      (insert "\n"))))

(defun shipit-issue--replace-comments-with-content (repo issue-number comments)
  "Replace comments placeholder with COMMENTS for ISSUE-NUMBER in REPO.
Uses the same pattern as `shipit--replace-general-comments-section-with-content'."
  (shipit--debug-log "ASYNC: Replacing issue comments placeholder with %d comments"
                     (length comments))
  (shipit-issue--replace-comments-section-with
   (lambda () (shipit-issue--insert-comments-section repo issue-number comments))))

(defun shipit-issue--replace-comments-with-paginated-content (repo issue-number result)
  "Replace comments placeholder with paginated RESULT for ISSUE-NUMBER.
RESULT is a plist from the head+tail fetcher."
  (shipit--debug-log "ASYNC: Replacing issue comments with paginated content, total=%d"
                     (plist-get result :total))
  (shipit-issue--replace-comments-section-with
   (lambda () (shipit-issue--insert-paginated-comments-section repo issue-number result))))

(defun shipit-issue--replace-comments-section-with (insert-fn)
  "Replace the issue-comments placeholder by calling INSERT-FN.
INSERT-FN should insert a new issue-comments magit section at point."
  (let ((section (shipit--find-section-by-type 'issue-comments)))
    (when section
      (let* ((inhibit-read-only t)
             (section-start (oref section start))
             (section-end (oref section end))
             (parent-section (oref section parent))
             (children (and parent-section (oref parent-section children)))
             (old-index (and children (seq-position children section #'eq)))
             new-section)
        (when (and section-start section-end)
          (save-excursion
            (delete-region section-start section-end)
            (goto-char section-start)
            (let ((magit-insert-section--parent parent-section))
              (setq new-section (funcall insert-fn))))
          ;; Fix parent's children list to maintain correct order
          (when (and parent-section old-index new-section)
            (let* ((current-children (oref parent-section children))
                   (filtered (seq-remove (lambda (s)
                                           (or (eq s section) (eq s new-section)))
                                         current-children))
                   (fixed (append (seq-take filtered old-index)
                                  (list new-section)
                                  (seq-drop filtered old-index))))
              (oset parent-section children fixed))))))))

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

(defcustom shipit-comments-load-more-default 50
  "Default number of comments to load when pressing RET on Load more.
With C-u N, load exactly N.  With C-u alone, load all remaining."
  :type 'integer
  :group 'shipit)

(defun issue-comments-load-more (&rest _args)
  "Magit section identifier for the load-more placeholder."
  nil)
(put 'issue-comments-load-more 'magit-section t)

(defcustom shipit-comments-render-chunk-size 10
  "Number of comments to render between progressive display refreshes.
After every N comments are inserted into the issue buffer, the display
is refreshed via `shipit--render-yield' so the user sees comments
appearing progressively instead of waiting for the whole batch to
finish.  Set to 0 to disable progressive rendering."
  :type 'integer
  :group 'shipit)

(defun shipit-issue--insert-comments-chunked (repo issue-number comments)
  "Insert COMMENTS for ISSUE-NUMBER in REPO with progressive display refresh.
Calls `shipit--render-yield' after every `shipit-comments-render-chunk-size'
comments so the user sees content appearing progressively."
  (let ((chunk-size shipit-comments-render-chunk-size)
        (counter 0))
    (dolist (comment comments)
      (shipit-issue--insert-single-comment repo issue-number comment)
      (cl-incf counter)
      (when (and (> chunk-size 0)
                 (zerop (mod counter chunk-size)))
        (shipit--render-yield)))))

(defun shipit-issue--insert-paginated-comments-section (repo issue-number result)
  "Insert comments section from a head+tail RESULT plist.
RESULT has :head :tail :hidden :total :unfetched :per-page.
Renders head comments, a Load more section, then tail comments."
  (let* ((head (plist-get result :head))
         (tail (plist-get result :tail))
         (hidden (plist-get result :hidden))
         (total (plist-get result :total))
         (unfetched (plist-get result :unfetched))
         (per-page (plist-get result :per-page))
         (hidden-count (+ (length hidden)
                          (* (length unfetched) per-page))))
    (magit-insert-section (issue-comments nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "comment" "💬")
                (propertize (format "Comments (%d)" total)
                            'face 'magit-section-heading)))
      (magit-insert-section-body
        (cond
         ((= total 0)
          (insert (propertize "   No comments\n" 'face 'font-lock-comment-face)))
         ((= hidden-count 0)
          (shipit-issue--insert-comments-chunked repo issue-number (append head tail)))
         (t
          (shipit-issue--insert-comments-chunked repo issue-number head)
          (shipit-issue--insert-load-more-section
           hidden repo issue-number unfetched per-page)
          (shipit-issue--insert-comments-chunked repo issue-number tail)))
        (insert "\n")))))

(defun shipit-issue--insert-load-more-section (hidden-comments repo issue-number
                                                               &optional unfetched-pages per-page)
  "Insert a Load more section for HIDDEN-COMMENTS in REPO ISSUE-NUMBER.
The section stores the hidden comments and optional UNFETCHED-PAGES
and PER-PAGE for lazy API fetching."
  (let ((count (+ (length hidden-comments)
                  (* (length unfetched-pages) (or per-page 0)))))
    (magit-insert-section (issue-comments-load-more
                           (list :comments hidden-comments
                                 :repo repo
                                 :issue-number issue-number
                                 :unfetched unfetched-pages
                                 :per-page per-page))
      (magit-insert-heading
        (propertize (format "   ── Load %d more comment%s ──"
                            count (if (= count 1) "" "s"))
                    'face 'font-lock-comment-face)))))

(defun shipit-issue--insert-comments-section (repo issue-number comments)
  "Insert comments section with COMMENTS for issue ISSUE-NUMBER in REPO.
When the number of comments exceeds `shipit-comments-pagination-threshold',
only the first `shipit-comments-initial-count' and last
`shipit-comments-tail-count' are rendered.  A Load more section in the
middle lets the user reveal hidden comments on demand."
  (magit-insert-section (issue-comments nil nil)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "comment" "💬")
              (propertize (format "Comments (%d)" (length comments))
                          'face 'magit-section-heading)))
    (magit-insert-section-body
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
      (insert "\n"))))

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
                                           (not (shipit-issue--backend-has-reactions-p
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

(defun shipit-issue--insert-activity-section (_repo issue-data)
  "Insert activity/changelog section for ISSUE-DATA."
  (let ((changelog (cdr (assq 'changelog issue-data))))
    (magit-insert-section (issue-activity nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "activity" "📋")
                (propertize (format "Activity (%d)" (length changelog))
                            'face 'magit-section-heading)))
      (if (and changelog (> (length changelog) 0))
          (let ((user-col-width (shipit-issue--changelog-max-user-width changelog)))
            (dolist (entry (reverse changelog))
              (shipit-issue--insert-activity-event entry user-col-width)))
        (insert (propertize "   No activity yet\n" 'face 'font-lock-comment-face)))
      (insert "\n"))))

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
                                             (* (length unfetched) (or per-page 0))))
                      (t shipit-comments-load-more-default))))
    ;; If we have enough in memory, render directly
    (if (or (>= (length hidden) batch-size) (null unfetched))
        (shipit-issue--load-more-from-memory section data batch-size)
      ;; Need to fetch more from API first
      (let ((next-page (car unfetched))
            (remaining-pages (cdr unfetched))
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
                 ;; Add fetched comments to hidden pool and render
                 (let ((new-hidden (append hidden (or page-data '()))))
                   (oset section value
                         (plist-put (plist-put (copy-sequence data)
                                              :comments new-hidden)
                                    :unfetched remaining-pages))
                   (shipit-issue--load-more-from-memory
                    section (oref section value) batch-size)))))
           (lambda (err)
             (message "Failed to fetch comments: %s" err))))))))

(defun shipit-issue--load-more-from-memory (section data batch-size)
  "Render BATCH-SIZE comments from in-memory DATA in SECTION."
  (let* ((hidden (plist-get data :comments))
         (repo (plist-get data :repo))
         (issue-number (plist-get data :issue-number))
         (unfetched (plist-get data :unfetched))
         (per-page (plist-get data :per-page))
         (to-load (seq-take hidden batch-size))
         (remaining (seq-drop hidden batch-size))
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
    ;; Insert the loaded comments at the same position
    (save-excursion
      (goto-char section-start)
      (let ((magit-insert-section--parent parent))
        (shipit-issue--insert-comments-chunked repo issue-number to-load)
        ;; If more remain or pages unfetched, insert new load-more
        (when (or remaining unfetched)
          (shipit-issue--insert-load-more-section
           remaining repo issue-number unfetched per-page))))
    ;; Fix parent's children list
    (when parent
      (oset parent children
            (seq-remove (lambda (s) (eq s section))
                        (oref parent children))))
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
        (shipit-issue--load-more-comments))
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

(provide 'shipit-issues-buffer)
;;; shipit-issues-buffer.el ends here
