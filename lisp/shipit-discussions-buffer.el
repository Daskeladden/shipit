;;; shipit-discussions-buffer.el --- Discussion view buffer -*- lexical-binding: t; -*-

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
;; Dedicated buffer for viewing a single GitHub Discussion with magit sections.
;; Shows header, metadata, description, and threaded comments.

;;; Code:

(require 'magit-section)
(require 'shipit-core)
(require 'shipit-discussions-graphql)

;; Forward declarations
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--wrap-text "shipit-render")
(declare-function shipit--create-pr-reference-overlays "shipit-render")
(declare-function shipit--create-commit-sha-overlays "shipit-render")
(declare-function shipit--create-user-mention-overlays "shipit-render")
(declare-function shipit--create-custom-url-overlays "shipit-render")
(declare-function shipit--create-generic-url-overlays "shipit-render")
(declare-function shipit--try-overlay-action-at-point "shipit-render")
(declare-function shipit--apply-code-block-backgrounds-in-region "shipit-render")
(declare-function shipit--apply-strikethrough-faces "shipit-render")
(declare-function shipit--apply-blockquote-faces "shipit-render")
(declare-function shipit--create-jira-mention-overlays "shipit-issue-jira")
(declare-function shipit--insert-comment-body-only "shipit-render")
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit--get-reactions-placeholder-icon "shipit-render")
(declare-function shipit--format-timestamp "shipit-core")
(declare-function shipit--create-avatar-display "shipit-render")
(declare-function shipit--clean-text "shipit-core")
(declare-function shipit--reaction-to-emoji "shipit-core")
(declare-function shipit-editor-open "shipit-editor")
(declare-function shipit-register-dwim-handler "shipit-pr-actions")
(declare-function shipit-dwim "shipit-pr-actions")
(declare-function shipit-toggle-timestamp-format "shipit-commands")
(declare-function shipit-discussion--add-reaction "shipit-discussions-graphql")
(declare-function shipit-discussion--remove-reaction "shipit-discussions-graphql")
(declare-function shipit-discussion--toggle-upvote "shipit-discussions-graphql")
(declare-function shipit--insert-repo-url-line "shipit-buffer")
(declare-function shipit-open-repo-buffer "shipit-repo-buffer")
(declare-function shipit-discussion--resolve-for-repo "shipit-discussion-backends")

;; Forward variable declarations
(defvar shipit-render-markdown)
(defvar shipit-show-avatars)
(defvar shipit--dwim-handlers)
(defvar shipit-reaction-choices)
(defvar shipit-use-svglib-icons)

;;; Section type definitions

(defun shipit-discussion (&rest _args)
  "Magit section identifier for discussion root.")
(put 'shipit-discussion 'magit-section t)

(defun shipit-discussion-header (&rest _args)
  "Magit section identifier for discussion header.")
(put 'shipit-discussion-header 'magit-section t)

(defun shipit-discussion-metadata (&rest _args)
  "Magit section identifier for discussion metadata.")
(put 'shipit-discussion-metadata 'magit-section t)

(defun shipit-discussion-description (&rest _args)
  "Magit section identifier for discussion description.")
(put 'shipit-discussion-description 'magit-section t)

(defun discussion-comments (&rest _args)
  "Magit section identifier for discussion comments container.")
(put 'discussion-comments 'magit-section t)

(defun discussion-comment (&rest _args)
  "Magit section identifier for individual discussion comment.")
(put 'discussion-comment 'magit-section t)

(defun discussion-reply (&rest _args)
  "Magit section identifier for discussion reply.")
(put 'discussion-reply 'magit-section t)

;;; Reaction and vote formatting

(defun shipit-discussion--format-reactions (reactions)
  "Format REACTIONS list as a string with placeholder icon and emoji counts.
REACTIONS is a list of alists with `content' and `user' keys."
  (let ((placeholder (shipit--get-reactions-placeholder-icon))
        (reaction-groups (make-hash-table :test 'equal)))
    (dolist (reaction reactions)
      (let* ((content (cdr (assq 'content reaction)))
             (user-obj (cdr (assq 'user reaction)))
             (user (or (cdr (assq 'login user-obj)) "unknown"))
             (existing (gethash content reaction-groups)))
        (puthash content (cons user (or existing '())) reaction-groups)))
    (let ((formatted '()))
      (maphash (lambda (reaction-type users)
                 (let ((emoji (shipit--reaction-to-emoji reaction-type))
                       (count (length users))
                       (user-list (mapconcat #'identity (reverse users) ", ")))
                   (when emoji
                     (push (propertize (format "%s %d" emoji count)
                                       'help-echo (format "%s: %s"
                                                          reaction-type
                                                          user-list))
                           formatted))))
               reaction-groups)
      (if (> (length formatted) 0)
          (concat placeholder " " (mapconcat #'identity formatted " "))
        (concat placeholder " ")))))

(defun shipit-discussion--format-votes (upvote-count &optional viewer-has-upvoted)
  "Format UPVOTE-COUNT as a vote display string with up arrow icon.
When VIEWER-HAS-UPVOTED is non-nil, highlight to show active state."
  (let ((icon (shipit--get-pr-field-icon "upvotes" "▲"))
        (count (or upvote-count 0)))
    (if viewer-has-upvoted
        (propertize (concat icon " " (number-to-string count))
                    'font-lock-face '(:foreground "#fd7e14" :weight bold))
      (concat icon " "
              (propertize (number-to-string count)
                          'font-lock-face (if (> count 0)
                                    'font-lock-constant-face
                                  'shadow))))))

(defun shipit-discussion--insert-vote-reaction-line (indent reactions
                                                            &optional upvote-count
                                                            subject-id
                                                            viewer-has-upvoted)
  "Insert a combined vote + reaction line at INDENT.
REACTIONS is the reaction list, UPVOTE-COUNT is the vote count (nil to skip).
SUBJECT-ID is the GraphQL node ID for the subject.
VIEWER-HAS-UPVOTED highlights the vote when non-nil."
  (let* ((vote-str (when upvote-count
                     (shipit-discussion--format-votes
                      upvote-count viewer-has-upvoted)))
         (reaction-str (shipit-discussion--format-reactions reactions))
         (line-start (point)))
    (insert (concat indent
                    (when vote-str (concat vote-str "  "))
                    reaction-str
                    "\n"))
    (add-text-properties line-start (point)
                         `(shipit-discussion-reactions t
                           ,@(when subject-id
                               `(shipit-discussion-subject-id ,subject-id))))))

;;; Buffer-local variables

(defvar-local shipit-discussion-buffer-number nil
  "Discussion number displayed in this buffer.")

(defvar-local shipit-discussion-buffer-repo nil
  "Repository for the discussion displayed in this buffer.")

(defvar-local shipit-discussion-buffer-data nil
  "Cached discussion data for this buffer.")

(defvar shipit-discussion-header-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "M-;") #'shipit-dwim)
    (define-key map (kbd "M-w") #'shipit-discussion-copy-url)
    map)
  "Keymap for shipit discussion header line.")

;;; Mode definition

(defvar shipit-discussion-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'shipit-discussion-buffer-refresh)
    (define-key map (kbd "r") #'shipit-discussion-buffer-refresh)
    (define-key map (kbd "o") #'shipit-discussion--open-in-browser)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'shipit-discussion-buffer-quit-and-kill)
    (define-key map (kbd "M-;") #'shipit-dwim)
    (define-key map (kbd "RET") #'shipit-discussion--ret-dwim)
    (define-key map (kbd "L") #'shipit-toggle-timestamp-format)
    map)
  "Keymap for `shipit-discussion-mode'.")

(define-derived-mode shipit-discussion-mode magit-section-mode "Shipit-Discussion"
  "Major mode for shipit discussion buffers.
Provides a read-only interface for viewing GitHub Discussions.

\\{shipit-discussion-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-discussion-buffer-refresh)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local tab-width 8)
  (setq-local magit-root-section nil)
  (shipit--apply-section-defaults)
  (shipit--enable-issue-ref-eldoc)
  (font-lock-mode 1))

;;; Buffer lifecycle

(defun shipit-discussion-buffer-name (repo number)
  "Generate buffer name for discussion NUMBER in REPO."
  (format "*shipit-discussion: %s#%s*" repo number))

;;;###autoload
(defun shipit-discussions-open-buffer (number &optional repo)
  "Open dedicated buffer for discussion NUMBER in REPO."
  (interactive
   (list (read-number "Discussion number: ")
         (shipit--get-repo-from-remote)))
  (let* ((repo (or repo (shipit--get-repo-from-remote)))
         (buffer-name (shipit-discussion-buffer-name repo number))
         (existing (get-buffer buffer-name)))
    (cond
     (existing
      (shipit--debug-log "Switching to existing discussion buffer: %s"
                         buffer-name)
      (with-current-buffer existing
        (setq shipit-discussion-buffer-number number
              shipit-discussion-buffer-repo repo))
      (switch-to-buffer existing))
     (t
      (shipit--debug-log "Creating new discussion buffer: %s" buffer-name)
      (let ((buffer (generate-new-buffer buffer-name))
            (repo-root (or (shipit--get-repo-root) default-directory)))
        (with-current-buffer buffer
          (shipit-discussion-mode)
          (setq default-directory repo-root)
          (setq shipit-discussion-buffer-number number
                shipit-discussion-buffer-repo repo)
          (shipit-discussion-buffer-refresh))
        (switch-to-buffer buffer))))))

(defun shipit-discussion-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the discussion buffer contents."
  (interactive)
  (let ((repo shipit-discussion-buffer-repo)
        (number shipit-discussion-buffer-number))
    (shipit--debug-log "Refreshing discussion buffer for #%s in %s"
                       number repo)
    (message "Loading discussion #%s..." number)
    (let ((data (shipit-discussion--fetch repo number)))
      (when data
        (setq shipit-discussion-buffer-data data)
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (magit-insert-section (shipit-discussion)
            (shipit-discussion--insert-header-section repo data number)
            (shipit-discussion--insert-metadata-section repo data)
            (shipit-discussion--insert-description-section repo data)
            (shipit-discussion--insert-comments-section
             repo number (cdr (assq 'comments data))))
          (goto-char (min pos (point-max))))))
    (message "Discussion #%s loaded" number)))

(defun shipit-discussion-buffer-quit-and-kill ()
  "Quit discussion buffer and kill it."
  (interactive)
  (let ((buffer (current-buffer)))
    (quit-window)
    (kill-buffer buffer)))

;;; Section rendering

(defun shipit-discussion--insert-header-section (repo data number)
  "Insert discussion header section for NUMBER from REPO with DATA."
  (let* ((title (or (cdr (assq 'title data)) "No title"))
         (is-answered (cdr (assq 'is_answered data)))
         (state-text (if is-answered "Answered" "Open"))
         (state-face (if is-answered 'success 'warning))
         (icon-key (if is-answered "discussion-answered" "discussion"))
         (emoji-fallback (if is-answered "✅" "💬"))
         (header-start (point)))
    (magit-insert-heading
      (format "%s %s #%s: %s"
              (shipit--get-pr-field-icon icon-key emoji-fallback)
              (propertize state-text 'font-lock-face state-face)
              number
              (string-trim (shipit--clean-text title))))
    (add-text-properties header-start (point)
                         `(shipit-discussion-header t
                           shipit-repo ,repo
                           shipit-discussion-number ,number
                           keymap ,shipit-discussion-header-keymap
                           help-echo "M-w: copy discussion URL, M-;: shipit-dwim"))))

(defun shipit-discussion--insert-metadata-section (repo data)
  "Insert metadata section for discussion DATA in REPO."
  (magit-insert-section (shipit-discussion-metadata nil nil)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "metadata" "📋")
              (propertize "Metadata" 'font-lock-face 'magit-section-heading)))
    (magit-insert-section-body
      ;; Repo URL
      (let ((repo-url (shipit-discussion--derive-repo-url data)))
        (when repo-url
          (insert "   ")
          (shipit--insert-repo-url-line repo-url repo)))
      ;; Author
      (let* ((user-obj (cdr (assq 'user data)))
             (user (or (cdr (assq 'login user-obj)) "Unknown"))
             (avatar-url (cdr (assq 'avatar_url user-obj))))
        (insert (format "   %s Author:    %s%s\n"
                        (shipit--get-pr-field-icon "author" "👤")
                        (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                 (fboundp 'shipit--create-avatar-display))
                            (concat (shipit--create-avatar-display
                                     user avatar-url 16) " ")
                          "")
                        (propertize user 'font-lock-face 'shipit-username-face))))
      ;; Category
      (let* ((category (cdr (assq 'category data)))
             (cat-name (cdr (assq 'name category)))
             (cat-emoji (or (cdr (assq 'emoji category)) "")))
        (when cat-name
          (insert (format "   %s Category:  %s %s\n"
                          (shipit--get-pr-field-icon "category" "📂")
                          cat-emoji
                          (propertize cat-name
                                      'font-lock-face 'font-lock-type-face)))))
      ;; Answered state
      (let ((is-answered (cdr (assq 'is_answered data))))
        (insert (format "   %s State:     %s\n"
                        (shipit--get-pr-field-icon "state" "📌")
                        (if is-answered
                            (propertize "Answered" 'font-lock-face 'success)
                          (propertize "Unanswered" 'font-lock-face 'warning)))))
      ;; Upvotes
      (let ((upvotes (or (cdr (assq 'upvote_count data)) 0)))
        (when (> upvotes 0)
          (insert (format "   %s Upvotes:   %s\n"
                          (shipit--get-pr-field-icon "upvotes" "▲")
                          (propertize (number-to-string upvotes)
                                      'font-lock-face 'font-lock-constant-face)))))
      ;; Created
      (let* ((created (or (cdr (assq 'created_at data)) ""))
             (formatted (if (and (fboundp 'shipit--format-timestamp)
                                 (not (string-empty-p created)))
                            (shipit--format-timestamp created)
                          created)))
        (insert (format "   %s Created:   %s\n"
                        (shipit--get-pr-field-icon "created" "📅")
                        (propertize formatted
                                    'font-lock-face 'shipit-timestamp-face))))
      ;; Updated
      (let* ((updated (or (cdr (assq 'updated_at data)) ""))
             (formatted (if (and (fboundp 'shipit--format-timestamp)
                                 (not (string-empty-p updated)))
                            (shipit--format-timestamp updated)
                          updated)))
        (when (not (string-empty-p updated))
          (insert (format "   %s Updated:   %s\n"
                          (shipit--get-pr-field-icon "updated" "🔄")
                          (propertize formatted
                                      'font-lock-face 'shipit-timestamp-face)))))
      ;; Labels
      (let ((labels (cdr (assq 'labels data))))
        (when (and labels (> (length labels) 0))
          (let ((label-strs
                 (mapcar (lambda (l)
                           (let* ((name (cdr (assq 'name l)))
                                  (color (cdr (assq 'color l)))
                                  (face (when color
                                          `(:foreground
                                            ,(concat "#" color)))))
                             (if face
                                 (propertize name 'font-lock-face face)
                               name)))
                         labels)))
            (insert (format "   %s Labels:    %s\n"
                            (shipit--get-pr-field-icon "labels" "🏷")
                            (mapconcat #'identity label-strs ", "))))))
      (insert "\n"))))

(defun shipit-discussion--insert-description-section (_repo data)
  "Insert description section for discussion DATA."
  (let* ((raw-body (cdr (assq 'body data)))
         (clean-body (when (and raw-body (not (string-empty-p raw-body)))
                       (let ((cleaned (shipit--clean-text raw-body)))
                         (unless (string-match-p "\\`[[:space:]]*\\'" cleaned)
                           cleaned))))
         (discussion-id (cdr (assq 'id data)))
         (upvote-count (or (cdr (assq 'upvote_count data)) 0))
         (viewer-has-upvoted (cdr (assq 'viewer_has_upvoted data)))
         (reactions (cdr (assq 'reactions data))))
    (magit-insert-section (shipit-discussion-description nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "description" "📝")
                (propertize "Description:" 'font-lock-face 'markdown-metadata-key-face)))
      (magit-insert-section-body
        (let ((description-start (point)))
          (if (not clean-body)
              (insert (propertize "   No description provided\n" 'font-lock-face 'italic))
            (let* ((rendered (if (and (boundp 'shipit-render-markdown)
                                      shipit-render-markdown
                                      (fboundp 'shipit--render-markdown))
                                 (shipit--render-markdown clean-body)
                               clean-body))
                   (wrapped (if (fboundp 'shipit--wrap-text)
                                (shipit--wrap-text rendered 80 0)
                              rendered)))
              (insert (concat "   "
                              (replace-regexp-in-string "\n" "\n   " wrapped)
                              "\n"))))
          ;; Create overlays for references
          (when (fboundp 'shipit--create-pr-reference-overlays)
            (shipit--create-pr-reference-overlays
             shipit-discussion-buffer-repo
             shipit-discussion-buffer-number
             description-start (point)))
          (when (fboundp 'shipit--create-commit-sha-overlays)
            (shipit--create-commit-sha-overlays
             shipit-discussion-buffer-repo description-start (point)))
          (when (fboundp 'shipit--create-user-mention-overlays)
            (shipit--create-user-mention-overlays
             shipit-discussion-buffer-repo description-start (point)))
          (when (fboundp 'shipit--create-custom-url-overlays)
            (shipit--create-custom-url-overlays description-start (point)))
          (when (fboundp 'shipit--create-generic-url-overlays)
            (shipit--create-generic-url-overlays description-start (point)))
          (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
            (shipit--apply-code-block-backgrounds-in-region
             description-start (point)))
          (when (fboundp 'shipit--apply-strikethrough-faces)
            (shipit--apply-strikethrough-faces description-start (point)))
          ;; Vote + reaction line
          (shipit-discussion--insert-vote-reaction-line
           "   " reactions upvote-count discussion-id
           viewer-has-upvoted)
          (insert "\n"))))))

(defun shipit-discussion--insert-comments-section (repo number comments)
  "Insert comments section with COMMENTS for discussion NUMBER in REPO."
  (let ((comment-count (if comments (length comments) 0)))
    (magit-insert-section (discussion-comments nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "comment" "💬")
                (propertize (format "Comments (%d)" comment-count)
                            'font-lock-face 'magit-section-heading)))
      (magit-insert-section-body
        (if (or (null comments) (= (length comments) 0))
            (insert "   No comments\n")
          (dolist (comment comments)
            (shipit-discussion--insert-single-comment
             repo number comment)))
        (insert "\n")))))

(defun shipit-discussion--insert-single-comment (repo number comment)
  "Insert a single COMMENT for discussion NUMBER in REPO."
  (let* ((comment-id (cdr (assq 'id comment)))
         (user-obj (cdr (assq 'user comment)))
         (user (or (cdr (assq 'login user-obj)) "Unknown"))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (is-answer (cdr (assq 'is_answer comment)))
         (upvotes (or (cdr (assq 'upvote_count comment)) 0))
         (viewer-has-upvoted (cdr (assq 'viewer_has_upvoted comment)))
         (reactions (cdr (assq 'reactions comment)))
         (created (or (cdr (assq 'created_at comment)) ""))
         (timestamp (if (and (fboundp 'shipit--format-timestamp)
                             (not (string-empty-p created)))
                        (shipit--format-timestamp created)
                      created))
         (replies (cdr (assq 'replies comment))))
    (magit-insert-section (discussion-comment comment-id)
      (magit-insert-heading
        (format "   %s%s  %s%s"
                (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                         (fboundp 'shipit--create-avatar-display))
                    (concat (shipit--create-avatar-display
                             user avatar-url 16) " ")
                  "")
                (propertize user 'font-lock-face 'shipit-username-face)
                (propertize timestamp 'font-lock-face 'shipit-timestamp-face)
                (if is-answer
                    (concat "  " (propertize "✅ Answer"
                                             'font-lock-face 'success))
                  "")))
      (magit-insert-section-body
        ;; Comment body
        (let* ((body (or (cdr (assq 'body comment)) ""))
               (clean (shipit--clean-text body))
               (rendered (if (and (boundp 'shipit-render-markdown)
                                  shipit-render-markdown
                                  (fboundp 'shipit--render-markdown))
                             (shipit--render-markdown clean)
                           clean))
               (wrapped (if (fboundp 'shipit--wrap-text)
                            (shipit--wrap-text rendered 74 0)
                          rendered))
               (body-start (point)))
          (insert (concat "      "
                          (replace-regexp-in-string "\n" "\n      " wrapped)
                          "\n"))
          ;; Apply faces and overlays (same pipeline as PR comments)
          (when (fboundp 'shipit--apply-blockquote-faces)
            (shipit--apply-blockquote-faces body-start (point)))
          (when (fboundp 'shipit--apply-strikethrough-faces)
            (shipit--apply-strikethrough-faces body-start (point)))
          (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
            (shipit--apply-code-block-backgrounds-in-region
             body-start (point)))
          (when (fboundp 'shipit--create-pr-reference-overlays)
            (shipit--create-pr-reference-overlays
             repo number body-start (point)))
          (when (fboundp 'shipit--create-commit-sha-overlays)
            (shipit--create-commit-sha-overlays repo body-start (point)))
          (when (fboundp 'shipit--create-user-mention-overlays)
            (shipit--create-user-mention-overlays
             repo body-start (point)))
          (when (fboundp 'shipit--create-jira-mention-overlays)
            (shipit--create-jira-mention-overlays body-start (point)))
          (when (fboundp 'shipit--create-custom-url-overlays)
            (shipit--create-custom-url-overlays body-start (point)))
          (when (fboundp 'shipit--create-generic-url-overlays)
            (shipit--create-generic-url-overlays body-start (point))))
        ;; Vote + reaction line (root comments have both)
        (shipit-discussion--insert-vote-reaction-line
         "      " reactions upvotes comment-id viewer-has-upvoted)
        ;; Replies
        (when replies
          (insert "\n")
          (dolist (reply replies)
            (shipit-discussion--insert-reply repo number reply)))
        (insert "\n")))))

(defun shipit-discussion--insert-reply (repo number reply)
  "Insert a REPLY for discussion NUMBER in REPO."
  (let* ((reply-id (cdr (assq 'id reply)))
         (user-obj (cdr (assq 'user reply)))
         (user (or (cdr (assq 'login user-obj)) "Unknown"))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (reactions (cdr (assq 'reactions reply)))
         (created (or (cdr (assq 'created_at reply)) ""))
         (timestamp (if (and (fboundp 'shipit--format-timestamp)
                             (not (string-empty-p created)))
                        (shipit--format-timestamp created)
                      created))
         (orange-color (if (eq (frame-parameter nil 'background-mode) 'light)
                           "#d2691e" "#ff8c00"))
         (tree-indicator (propertize "└─ " 'face
                                     `(:foreground ,orange-color)))
         ;; 6 spaces base indent + tree indicator
         (reply-indent "      ")
         (body-indent "         "))
    (magit-insert-section (discussion-reply reply-id)
      (magit-insert-heading
        (format "%s%s%s%s  %s"
                reply-indent
                tree-indicator
                (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                         (fboundp 'shipit--create-avatar-display))
                    (concat (shipit--create-avatar-display
                             user avatar-url 16) " ")
                  "")
                (propertize user 'face
                            `(:foreground ,orange-color :weight bold))
                (propertize timestamp 'font-lock-face 'shipit-timestamp-face)))
      (magit-insert-section-body
        (let* ((body (or (cdr (assq 'body reply)) ""))
               (clean (shipit--clean-text body))
               (rendered (if (and (boundp 'shipit-render-markdown)
                                  shipit-render-markdown
                                  (fboundp 'shipit--render-markdown))
                             (shipit--render-markdown clean)
                           clean))
               (wrapped (if (fboundp 'shipit--wrap-text)
                            (shipit--wrap-text rendered 68 0)
                          rendered))
               (body-start (point)))
          (insert (concat body-indent
                          (replace-regexp-in-string
                           "\n" (concat "\n" body-indent) wrapped)
                          "\n"))
          ;; Apply faces and overlays (same pipeline as PR comments)
          (when (fboundp 'shipit--apply-blockquote-faces)
            (shipit--apply-blockquote-faces body-start (point)))
          (when (fboundp 'shipit--apply-strikethrough-faces)
            (shipit--apply-strikethrough-faces body-start (point)))
          (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
            (shipit--apply-code-block-backgrounds-in-region
             body-start (point)))
          (when (fboundp 'shipit--create-pr-reference-overlays)
            (shipit--create-pr-reference-overlays
             repo number body-start (point)))
          (when (fboundp 'shipit--create-commit-sha-overlays)
            (shipit--create-commit-sha-overlays repo body-start (point)))
          (when (fboundp 'shipit--create-user-mention-overlays)
            (shipit--create-user-mention-overlays
             repo body-start (point)))
          (when (fboundp 'shipit--create-jira-mention-overlays)
            (shipit--create-jira-mention-overlays body-start (point)))
          (when (fboundp 'shipit--create-custom-url-overlays)
            (shipit--create-custom-url-overlays body-start (point)))
          (when (fboundp 'shipit--create-generic-url-overlays)
            (shipit--create-generic-url-overlays body-start (point))))
        ;; Reaction line only (no votes for replies)
        (shipit-discussion--insert-vote-reaction-line
         body-indent reactions nil reply-id)
        (insert "\n")))))

;;; Helpers

(defun shipit-discussion--browse-url ()
  "Return the browser URL for the current discussion buffer."
  (when (and shipit-discussion-buffer-repo shipit-discussion-buffer-number)
    (condition-case nil
        (let* ((resolved (shipit-discussion--resolve-for-repo shipit-discussion-buffer-repo))
               (backend (car resolved))
               (config (cdr resolved))
               (browse-fn (plist-get backend :browse-url)))
          (when browse-fn
            (funcall browse-fn config shipit-discussion-buffer-number)))
      (error nil))))

(defun shipit-discussion--open-in-browser ()
  "Open the current discussion in the browser."
  (interactive)
  (let ((url (shipit-discussion--browse-url)))
    (if url
        (browse-url url)
      (message "No discussion data available"))))

(defun shipit-discussion-copy-url ()
  "Copy the discussion URL to the kill ring.
If a region is active, copy the region text instead (standard M-w behavior)."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (let ((url (shipit-discussion--browse-url)))
      (if url
          (progn
            (kill-new url)
            (message "Copied: %s" url))
        (user-error "No discussion URL available")))))

(defun shipit-discussion--comment-id-at-point ()
  "Return the discussion comment ID at point."
  (when (fboundp 'magit-current-section)
    (let ((section (magit-current-section)))
      (when (magit-section-match '(discussion-comment) section)
        (oref section value)))))

;;; DWIM handlers

(defun shipit-discussion--find-comment (comment-id)
  "Find comment data by COMMENT-ID in the current buffer data."
  (cl-find-if (lambda (c) (equal (cdr (assq 'id c)) comment-id))
              (cdr (assq 'comments shipit-discussion-buffer-data))))

(defun shipit-discussion--comment-actions ()
  "Handle DWIM actions for discussion comments."
  (let* ((comment-id (shipit-discussion--comment-id-at-point))
         (comment-data (shipit-discussion--find-comment comment-id))
         (has-upvoted (cdr (assq 'viewer_has_upvoted comment-data)))
         (repo shipit-discussion-buffer-repo)
         (number shipit-discussion-buffer-number)
         (data shipit-discussion-buffer-data)
         (discussion-id (cdr (assq 'id data)))
         (category (cdr (assq 'category data)))
         (is-answerable (cdr (assq 'is_answerable category)))
         (upvote-label (if has-upvoted "Remove upvote" "Upvote"))
         (actions (append (list "Reply" upvote-label "React" "Add comment")
                          (when is-answerable '("Mark as answer"))))
         (choice (completing-read "Discussion comment action: "
                                  actions nil t)))
    (cond
     ((string= choice "Reply")
      (shipit-discussion--reply-to repo number discussion-id comment-id))
     ((or (string= choice "Upvote") (string= choice "Remove upvote"))
      (shipit-discussion--upvote-toggle comment-id has-upvoted))
     ((string= choice "React")
      (shipit-discussion--toggle-reaction comment-id))
     ((string= choice "Add comment")
      (shipit-discussion--add-comment-interactively
       repo number discussion-id))
     ((string= choice "Mark as answer")
      (shipit-discussion--mark-as-answer-interactive comment-id)))))

(defun shipit-discussion--description-actions ()
  "Handle DWIM actions for discussion description section."
  (let* ((repo shipit-discussion-buffer-repo)
         (number shipit-discussion-buffer-number)
         (data shipit-discussion-buffer-data)
         (discussion-id (cdr (assq 'id data)))
         (has-upvoted (cdr (assq 'viewer_has_upvoted data)))
         (upvote-label (if has-upvoted "Remove upvote" "Upvote"))
         (choice (completing-read "Discussion action: "
                                  (list upvote-label "React" "Add comment")
                                  nil t)))
    (cond
     ((or (string= choice "Upvote") (string= choice "Remove upvote"))
      (shipit-discussion--upvote-toggle discussion-id has-upvoted))
     ((string= choice "React")
      (shipit-discussion--toggle-reaction discussion-id))
     ((string= choice "Add comment")
      (shipit-discussion--add-comment-interactively
       repo number discussion-id)))))

(defun shipit-discussion--comments-header-actions ()
  "Handle DWIM actions for the discussion comments section header."
  (let* ((repo shipit-discussion-buffer-repo)
         (number shipit-discussion-buffer-number)
         (data shipit-discussion-buffer-data)
         (discussion-id (cdr (assq 'id data))))
    (shipit-discussion--add-comment-interactively
     repo number discussion-id)))

(defun shipit-discussion--add-comment-interactively (repo number discussion-id)
  "Add a comment to discussion DISCUSSION-ID (#NUMBER) in REPO."
  (shipit-editor-open
   (list :type 'discussion-comment
         :source-buffer (current-buffer)
         :pr-number number
         :repo repo
         :discussion-id discussion-id)))

(defun shipit-discussion--reply-to (_repo _number discussion-id comment-id)
  "Reply to comment COMMENT-ID in discussion DISCUSSION-ID."
  (shipit-editor-open
   (list :type 'discussion-reply
         :source-buffer (current-buffer)
         :pr-number shipit-discussion-buffer-number
         :repo shipit-discussion-buffer-repo
         :discussion-id discussion-id
         :parent-comment-id comment-id)))

(defun shipit-discussion--mark-as-answer-interactive (comment-id)
  "Mark COMMENT-ID as the accepted answer."
  (when (yes-or-no-p "Mark this comment as the accepted answer? ")
    (shipit-discussion--mark-as-answer comment-id)
    (message "Comment marked as answer")
    (shipit-discussion-buffer-refresh)))

(defun shipit-discussion--toggle-reaction (subject-id)
  "Toggle a reaction on discussion subject SUBJECT-ID."
  (let* ((choice (completing-read "Toggle reaction: " shipit-reaction-choices))
         (reaction-type (cdr (assoc choice shipit-reaction-choices))))
    (when reaction-type
      (shipit-discussion--add-reaction subject-id reaction-type)
      (message "Reaction %s added" choice)
      (shipit-discussion-buffer-refresh))))

(defun shipit-discussion--upvote-toggle (subject-id has-upvoted)
  "Toggle upvote on SUBJECT-ID.  HAS-UPVOTED is the current state."
  (shipit-discussion--toggle-upvote subject-id has-upvoted)
  (message (if has-upvoted "Upvote removed" "Upvoted"))
  (shipit-discussion-buffer-refresh))

(defun shipit-discussion--reply-id-at-point ()
  "Return the discussion reply ID at point."
  (when (fboundp 'magit-current-section)
    (let ((section (magit-current-section)))
      (when (magit-section-match '(discussion-reply) section)
        (oref section value)))))

(defun shipit-discussion--reply-actions ()
  "Handle DWIM actions for discussion replies."
  (let* ((reply-id (shipit-discussion--reply-id-at-point))
         (actions '("React"))
         (choice (completing-read "Discussion reply action: "
                                  actions nil t)))
    (when (string= choice "React")
      (shipit-discussion--toggle-reaction reply-id))))

(defun shipit-discussion--ret-dwim ()
  "Handle RET in discussion buffers.
Overlay actions (URLs, references) take priority, then DWIM handlers,
then section toggle."
  (interactive)
  (or (shipit--try-overlay-action-at-point)
      (and (fboundp 'shipit-dwim)
           (or (shipit-discussion--comment-id-at-point)
               (shipit-discussion--reply-id-at-point))
           (progn (shipit-dwim) t))
      (magit-section-toggle (magit-current-section))))

;; Register DWIM handlers

(defun shipit-discussion--register-dwim-handlers ()
  "Register discussion-buffer DWIM handlers."
  (when (fboundp 'shipit-register-dwim-handler)
    ;; Comments header
    (shipit-register-dwim-handler
     'discussion-comments-header
     (lambda () (and (derived-mode-p 'shipit-discussion-mode)
                     (fboundp 'magit-current-section)
                     (magit-section-match '(discussion-comments)
                                          (magit-current-section))))
     #'shipit-discussion--comments-header-actions)
    ;; Description
    (shipit-register-dwim-handler
     'discussion-description
     (lambda () (and (derived-mode-p 'shipit-discussion-mode)
                     (fboundp 'magit-current-section)
                     (magit-section-match '(shipit-discussion-description)
                                          (magit-current-section))))
     #'shipit-discussion--description-actions)
    ;; Comment
    (shipit-register-dwim-handler
     'discussion-comment
     (lambda () (and (derived-mode-p 'shipit-discussion-mode)
                     (shipit-discussion--comment-id-at-point)))
     #'shipit-discussion--comment-actions)
    ;; Reply
    (shipit-register-dwim-handler
     'discussion-reply
     (lambda () (and (derived-mode-p 'shipit-discussion-mode)
                     (shipit-discussion--reply-id-at-point)))
     #'shipit-discussion--reply-actions)))

;; Register now if shipit-pr-actions is already loaded
(shipit-discussion--register-dwim-handlers)
;; Also register when shipit-pr-actions loads later
(with-eval-after-load 'shipit-pr-actions
  (shipit-discussion--register-dwim-handlers))

(defun shipit-discussion--derive-repo-url (_data)
  "Derive the repository URL for the current discussion buffer.
Uses the discussion backend registry to construct the URL."
  (condition-case nil
      (let* ((resolved (shipit-discussion--resolve-for-repo
                        shipit-discussion-buffer-repo))
             (backend (car resolved))
             (config (cdr resolved))
             (repo-url-fn (plist-get backend :browse-repo-url)))
        (when repo-url-fn
          (funcall repo-url-fn config)))
    (error nil)))

(provide 'shipit-discussions-buffer)
;;; shipit-discussions-buffer.el ends here
