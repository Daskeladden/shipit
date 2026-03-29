;;; shipit-editor.el --- Comment/description editor with live preview -*- lexical-binding: t; -*-

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

;; A dedicated editor for comments and descriptions in shipit.
;; Features:
;; - Multi-line editing with gfm-mode (GitHub Flavored Markdown)
;; - Live preview in the source buffer
;; - PR reference completion with #

;;; Code:

(require 'shipit-core)

;; Try to load gfm-mode from markdown-mode package
(require 'markdown-mode nil t)

;; Forward declarations
(declare-function shipit--search-prs-with-encoded-query "shipit-commands")
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--add-general-comment-to-pr "shipit-http")
(declare-function shipit--add-comment-to-pr "shipit-http")
(declare-function shipit--edit-comment "shipit-http")
(declare-function shipit--edit-pr-description "shipit-http")
(declare-function shipit--reply-to-inline-comment "shipit-http")
(declare-function shipit--reply-to-general-comment "shipit-http")
(declare-function shipit-post-review "shipit-http")
(declare-function shipit--get-comment-type-icon "shipit-render")
(declare-function shipit--replace-general-comments-section-with-content "shipit-pr-sections")
(declare-function shipit--insert-reply-as-child "shipit-pr-sections")
(declare-function shipit-issues--create-issue "shipit-issues")
(declare-function shipit-issues--add-comment "shipit-issues")
(declare-function shipit-issues--edit-comment "shipit-issues")
(declare-function shipit-issues--update-description "shipit-issues")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-issue--resolve-for-repo "shipit-issue-backends")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")
(declare-function shipit-pr--backend-id "shipit-pr-backends")
(declare-function shipit-issue-buffer-refresh "shipit-issues-buffer")
(declare-function shipit--find-section-by-type "shipit-core")
(declare-function shipit-discussion--add-comment "shipit-discussions-graphql")
(declare-function shipit-discussion--reply-to-comment "shipit-discussions-graphql")
(declare-function shipit-discussion--create "shipit-discussions-graphql")
(declare-function shipit-discussion--fetch-categories "shipit-discussions-graphql")
(declare-function shipit-discussions-open-buffer "shipit-discussions-buffer")
(declare-function shipit-discussion-buffer-refresh "shipit-discussions-buffer")

;;; Buffer-local variables

(defvar-local shipit-editor--type nil
  "Type of content being edited.
One of: description, general-comment, inline-comment, reply, review.")

(defvar-local shipit-editor--review-event nil
  "Review event type for review submissions.
One of: APPROVE, REQUEST_CHANGES, COMMENT.")

(defvar-local shipit-editor--source-buffer nil
  "Buffer to update with live preview.")

(defvar-local shipit-editor--pr-number nil
  "PR number for context.")

(defvar-local shipit-editor--repo nil
  "Repository owner/name.")

(defvar-local shipit-editor--file-path nil
  "File path for inline comments.")

(defvar-local shipit-editor--line-number nil
  "Line number for inline comments.")

(defvar-local shipit-editor--old-line-number nil
  "Old line number for context-line inline comments (GitLab).")

(defvar-local shipit-editor--side nil
  "Diff side for inline comments: \"RIGHT\", \"LEFT\", or \"CONTEXT\".")

(defvar-local shipit-editor--comment-id nil
  "Comment ID for editing existing comments.")

(defvar-local shipit-editor--parent-comment-id nil
  "Parent comment ID for replies.")

(defvar-local shipit-editor--discussion-id nil
  "Discussion node ID for discussion comments/replies.")

(defvar-local shipit-editor--parent-depth nil
  "Depth of parent comment for reply indentation.")

(defvar-local shipit-editor--section-marker nil
  "Marker for preview updates in source buffer.")

(defvar-local shipit-editor--initial-content nil
  "Original content before editing.")

(defvar-local shipit-editor--preview-timer nil
  "Timer for debounced preview updates.")

(defvar-local shipit-editor--preview-overlay nil
  "Overlay used for live preview in source buffer.")

(defvar-local shipit-editor--preview-start nil
  "Start position of the preview region in source buffer.")

(defvar-local shipit-editor--preview-end nil
  "End position of the preview region in source buffer.")

(defvar-local shipit-editor--original-preview-content nil
  "Original content of the preview region before editing.")

(defvar-local shipit-editor--on-save nil
  "Callback function to call on save.
Called with the content as argument.  Used for preview-description type.")

(defconst shipit-editor--buffer-name "*shipit-editor*"
  "Name of the editor buffer.")

(defconst shipit-editor--preview-delay 0.15
  "Delay in seconds before updating preview.")

(defconst shipit-editor--completion-delay 0.5
  "Delay in seconds before triggering completion after typing # @ or :.
This allows typing ## for markdown headers or :: without triggering completion.")

(defvar-local shipit-editor--completion-timer nil
  "Timer for debounced completion triggers.")

(defvar-local shipit-editor--completion-char nil
  "The trigger character that started the completion timer.")

(defvar-local shipit-editor--completion-pos nil
  "Position where the trigger character was inserted.")

(defvar-local shipit-editor--commit-messages nil
  "List of commit messages available for insertion.
Each element is a plist with :sha, :short-sha, :message, :first-line.")

(defvar-local shipit-editor--commit-index -1
  "Current index in the commit messages list for M-n/M-p navigation.
-1 means no commit has been inserted yet.")

(defvar-local shipit-editor--commit-insert-start nil
  "Start marker of the last inserted commit message.
Used to replace on subsequent M-n/M-p presses.")

(defvar-local shipit-editor--commit-insert-end nil
  "End marker of the last inserted commit message.
Used to replace on subsequent M-n/M-p presses.")

(defvar-local shipit-editor--pr-backend-id nil
  "Resolved PR backend ID for this editor session.
Set from the source buffer context in `shipit-editor-open'.")

(defvar-local shipit-editor--pr-backend-config nil
  "PR backend config plist for this editor session.
Captured from the source buffer in `shipit-editor-open'.")

;;; Mode definition

(defvar shipit-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'shipit-editor-save)
    (define-key map (kbd "C-c C-k") #'shipit-editor-cancel)
    (define-key map (kbd "#") #'shipit-editor--trigger-pr-reference)
    (define-key map (kbd "@") #'shipit-editor--trigger-user-mention)
    (define-key map (kbd ":") #'shipit-editor--trigger-emoji)
    (define-key map (kbd "M-n") #'shipit-editor-insert-next-commit-message)
    (define-key map (kbd "M-p") #'shipit-editor-insert-prev-commit-message)
    map)
  "Keymap for `shipit-editor-mode'.")

(define-derived-mode shipit-editor-mode gfm-mode "Shipit-Editor"
  "Major mode for editing shipit comments and descriptions.

\\{shipit-editor-mode-map}"
  :group 'shipit
  ;; Ensure our keybindings are available by defining them locally
  ;; This handles cases where the keymap wasn't properly set up
  (local-set-key (kbd "C-c C-c") #'shipit-editor-save)
  (local-set-key (kbd "C-c C-k") #'shipit-editor-cancel)
  (local-set-key (kbd "@") #'shipit-editor--trigger-user-mention)
  (local-set-key (kbd ":") #'shipit-editor--trigger-emoji)
  (local-set-key (kbd "M-n") #'shipit-editor-insert-next-commit-message)
  (local-set-key (kbd "M-p") #'shipit-editor-insert-prev-commit-message)
  ;; Generic header line (overridden by shipit-editor-open with
  ;; backend-aware and context-specific versions)
  (setq-local header-line-format
              (propertize " Shipit Editor - C-c C-c: Save, C-c C-k: Cancel "
                          'face 'mode-line-highlight))
  ;; Set up live preview updates
  (add-hook 'after-change-functions #'shipit-editor--schedule-preview-update nil t)
  ;; Set up completion cancellation on next keypress
  (add-hook 'post-command-hook #'shipit-editor--maybe-cancel-completion nil t))

;;; Editor open

;;;###autoload
(defun shipit-editor-open (context)
  "Open the shipit editor with CONTEXT.
CONTEXT is a plist with:
  :type - \\='description, \\='general-comment, \\='inline-comment, \\='reply
  :source-buffer - buffer to update with live preview
  :initial-content - existing text to edit (for edits)
  :pr-number - PR number for context
  :repo - repository owner/name
  :file-path - for inline comments
  :line-number - for inline comments
  :comment-id - for editing existing comments
  :section-marker - position/marker for preview updates
  :commit-messages - list of commit message plists for M-n/M-p insertion"
  (let ((source-buffer (plist-get context :source-buffer))
        (initial-content (or (plist-get context :initial-content) ""))
        (commit-messages (plist-get context :commit-messages)))
    ;; Create or get the editor buffer
    (let ((editor-buffer (get-buffer-create shipit-editor--buffer-name))
          (preview-info nil)
          (resolved-backend-id (if (buffer-live-p source-buffer)
                                   (with-current-buffer source-buffer
                                     (shipit-pr--backend-id))
                                 (shipit-pr--backend-id)))
          (resolved-backend-config (if (buffer-live-p source-buffer)
                                       (with-current-buffer source-buffer
                                         shipit-pr-backend-config)
                                     shipit-pr-backend-config)))
      ;; Set up preview in source buffer before switching
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq preview-info (shipit-editor--setup-preview-overlay context))
          (shipit--debug-log "EDITOR: preview-info=%S overlay=%S"
                             (if preview-info "SET" "NIL")
                             (plist-get preview-info :overlay))))
      ;; Set up the editor buffer
      (with-current-buffer editor-buffer
        (erase-buffer)
        (shipit-editor-mode)
        ;; Store context
        (setq shipit-editor--type (plist-get context :type)
              shipit-editor--source-buffer source-buffer
              shipit-editor--pr-number (plist-get context :pr-number)
              shipit-editor--repo (plist-get context :repo)
              shipit-editor--file-path (plist-get context :file-path)
              shipit-editor--line-number (plist-get context :line-number)
              shipit-editor--old-line-number (plist-get context :old-line-number)
              shipit-editor--side (plist-get context :side)
              shipit-editor--comment-id (plist-get context :comment-id)
              shipit-editor--parent-comment-id (plist-get context :parent-comment-id)
              shipit-editor--discussion-id (plist-get context :discussion-id)
              shipit-editor--parent-depth (plist-get context :parent-depth)
              shipit-editor--section-marker (plist-get context :section-marker)
              shipit-editor--review-event (plist-get context :review-event)
              shipit-editor--on-save (plist-get context :on-save)
              shipit-editor--initial-content initial-content
              shipit-editor--commit-messages commit-messages
              shipit-editor--commit-index -1
              shipit-editor--pr-backend-id resolved-backend-id
              shipit-editor--pr-backend-config resolved-backend-config)
        ;; Backend-specific trigger keys (set here, not in mode body,
        ;; because each backend defines its own trigger characters)
        (local-set-key (kbd "#") #'shipit-editor--trigger-pr-reference)
        (let ((extra-keys (plist-get (shipit-pr--get-backend) :editor-extra-keys)))
          (dolist (binding extra-keys)
            (local-set-key (kbd (car binding)) (cdr binding))))
        (let ((ref-hints (or (plist-get (shipit-pr--get-backend) :editor-reference-hints)
                             "#: PR")))
          (setq-local header-line-format
                      (propertize (format " Shipit Editor - C-c C-c: Save, C-c C-k: Cancel, %s, @: User, :: Emoji "
                                          ref-hints)
                                  'face 'mode-line-highlight)))
        ;; Update header line based on context (overrides default)
        (cond
         (commit-messages
          (setq-local header-line-format
                      (propertize (format " Shipit Editor - C-c C-c: Save, C-c C-k: Cancel, M-n/M-p: Insert commit (%d available) "
                                          (length commit-messages))
                                  'face 'mode-line-highlight)))
         ((eq (plist-get context :type) 'create-issue)
          (setq-local header-line-format
                      (propertize " New Issue - Line 1: Title, then blank line, then body. C-c C-c: Create, C-c C-k: Cancel "
                                  'face 'mode-line-highlight)))
         ((memq (plist-get context :type) '(issue-comment issue-comment-edit))
          (setq-local header-line-format
                      (propertize " Issue Comment - C-c C-c: Save, C-c C-k: Cancel "
                                  'face 'mode-line-highlight)))
         ((eq (plist-get context :type) 'issue-description)
          (setq-local header-line-format
                      (propertize " Issue Description - C-c C-c: Save, C-c C-k: Cancel "
                                  'face 'mode-line-highlight)))
         ((eq (plist-get context :type) 'create-discussion)
          (setq-local header-line-format
                      (propertize " New Discussion - Line 1: Title, then blank line, then body. C-c C-c: Create, C-c C-k: Cancel "
                                  'face 'mode-line-highlight)))
         ((memq (plist-get context :type) '(discussion-comment discussion-reply))
          (setq-local header-line-format
                      (propertize " Discussion Comment - C-c C-c: Save, C-c C-k: Cancel "
                                  'face 'mode-line-highlight)))
         ((and (eq (plist-get context :type) 'review)
               (plist-get context :review-event))
          (let* ((event (plist-get context :review-event))
                 (label (cond
                         ((string= event "APPROVE") "Approve PR (optional message)")
                         ((string= event "REQUEST_CHANGES") "Request Changes")
                         (t (format "Review (%s)" event)))))
            (setq-local header-line-format
                        (propertize (format " %s - C-c C-c: Submit, C-c C-k: Cancel " label)
                                    'face 'mode-line-highlight)))))
        ;; Store preview overlay info
        (when preview-info
          (setq shipit-editor--preview-overlay (plist-get preview-info :overlay)
                shipit-editor--preview-start (plist-get preview-info :start)
                shipit-editor--preview-end (plist-get preview-info :end)
                shipit-editor--original-preview-content (plist-get preview-info :original-content)))
        ;; Insert initial content
        (insert initial-content)
        ;; Position cursor: for replies go to end (after quote), otherwise beginning
        (if (eq (plist-get context :type) 'reply)
            (goto-char (point-max))
          (goto-char (point-min)))
        ;; Trigger initial preview update
        (shipit-editor--update-preview (current-buffer)))
      ;; Display the editor in a split below
      (let ((window (display-buffer-in-side-window
                     editor-buffer
                     '((side . bottom)
                       (window-height . 0.3)))))
        (select-window window)))))

;;; Live preview

(defun shipit-editor--schedule-preview-update (&rest _args)
  "Schedule a preview update after a short delay.
Called from `after-change-functions'."
  ;; Cancel any pending timer
  (when (timerp shipit-editor--preview-timer)
    (cancel-timer shipit-editor--preview-timer))
  ;; Schedule new update
  (setq shipit-editor--preview-timer
        (run-with-idle-timer shipit-editor--preview-delay nil
                             #'shipit-editor--update-preview
                             (current-buffer))))

(defun shipit-editor--update-preview (editor-buffer)
  "Update the live preview in source buffer from EDITOR-BUFFER content."
  (when (buffer-live-p editor-buffer)
    (with-current-buffer editor-buffer
      (let ((content (buffer-string))
            (type shipit-editor--type)
            (source-buf shipit-editor--source-buffer)
            (overlay shipit-editor--preview-overlay)
            (parent-depth shipit-editor--parent-depth))
        (shipit--debug-log "EDITOR: update-preview called - source-buf=%S overlay=%S overlay-buffer=%S"
                           (buffer-live-p source-buf)
                           overlay
                           (and overlay (overlay-buffer overlay)))
        (when (and (buffer-live-p source-buf)
                   overlay
                   (overlay-buffer overlay))
          (shipit--debug-log "EDITOR: Preview update - content length: %d, has-code-block: %s"
                             (length content) (string-match-p "```" content))
          ;; Log code fence lines to debug language detection
          (when (string-match "```\\([a-zA-Z0-9_+-]*\\)" content)
            (shipit--debug-log "EDITOR: Code fence in content, captured language: '%s'"
                               (match-string 1 content)))
          ;; Replace content in the overlay region and apply code block backgrounds
          (with-current-buffer source-buf
            (let ((inhibit-read-only t)
                  (start (overlay-start overlay))
                  (end (overlay-end overlay)))
              ;; Delete old content
              (delete-region start end)
              ;; Insert rendered markdown
              (goto-char start)
              (let ((rendered (shipit-editor--render-preview-content content type parent-depth)))
                (insert rendered)
                (let ((new-end (point))
                      (section (overlay-get overlay 'shipit-section)))
                  ;; Move overlay to cover new content
                  (move-overlay overlay start new-end)
                  ;; NOTE: Do NOT update section's end marker here!
                  ;; The section end includes blank line + reactions which are
                  ;; outside the preview overlay. Updating it would break collapse.
                  ;; Apply code block backgrounds in the new region
                  (shipit--debug-log "EDITOR: Applying code blocks in region %d-%d, rendered has ```: %s"
                                     start new-end
                                     (string-match-p "```" (buffer-substring-no-properties start new-end)))
                  (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
                    (shipit--apply-code-block-backgrounds-in-region start new-end))
                  ;; Scroll source buffer to show end of preview
                  (shipit-editor--ensure-preview-visible source-buf new-end))))))))))

(defun shipit-editor--ensure-preview-visible (buffer pos)
  "Ensure position POS in BUFFER is visible in its window.
Also highlights the current line with hl-line."
  (let ((window (get-buffer-window buffer)))
    (when window
      (with-selected-window window
        (goto-char pos)
        (recenter -3)
        ;; Enable hl-line-mode to highlight current line
        (unless (bound-and-true-p hl-line-mode)
          (hl-line-mode 1))
        (when (fboundp 'hl-line-highlight)
          (hl-line-highlight))))))

(defun shipit-editor--render-preview-content (content &optional type parent-depth)
  "Render CONTENT as markdown for preview display.
TYPE is the editor type (e.g., \\='preview-description).
PARENT-DEPTH is the reply depth of the parent comment (for replies).
For live preview, we skip full markdown rendering to preserve code fence
language specifiers, which markdown-mode's font-lock can strip.
Emoji shortcodes like :rocket: are rendered as Unicode emoji.
Adds appropriate indentation based on type."
  ;; Strip shipit-reply-to markers before rendering (they're invisible metadata)
  (let* ((clean-content (replace-regexp-in-string
                         "<!-- shipit-reply-to:[0-9]+ -->\\s-*\n?" "" content))
         (indent-3 (memq type '(preview-description preview-general-comment issue-description)))
         (indent-6 (memq type '(general-comment issue-comment issue-comment-edit)))  ; 6 spaces for comment body (3 spaces + avatar width)
         (indent-8 (memq type '(preview-inline-comment preview-file-comment
                                inline-comment)))
         (indent-reply (eq type 'reply))  ; Reply needs nested child formatting
         ;; For replies, calculate depth-based indentation matching shipit--insert-status-hierarchical-comment
         ;; Reply depth = parent_depth + 1
         ;; base-indent (thread-prefix) = 3 + (depth-1)*6 spaces when depth > 0
         ;; tree-indicator = "└─ " (4 chars including trailing space)
         ;; Header: "   " + thread-prefix + tree-indicator = 3 + base-indent + 4
         ;; Body: 3 + len(thread-prefix) + len(tree-indicator) + 3 = 3 + base-indent + 4 + 3
         (reply-depth (if indent-reply
                          (1+ (or parent-depth 0))
                        0))
         (reply-base-indent (if (and indent-reply (> reply-depth 0))
                                (+ 3 (* (1- reply-depth) 6))
                              0))
         ;; Header prefix: 3 spaces + base-indent spaces (tree-indicator added separately)
         (reply-header-indent (if indent-reply
                                  (make-string (+ 3 reply-base-indent) ?\s)
                                ""))
         ;; Body indent: 3 + base-indent + 4 (tree-indicator) + 3
         (reply-body-indent (if indent-reply
                                (make-string (+ 3 reply-base-indent 4 3) ?\s)
                              ""))
         (indent-str (cond (indent-reply reply-body-indent)
                           (indent-8 "        ")  ; 8 spaces for inline comment body
                           (indent-6 "      ")    ; 6 spaces for general comment body
                           (indent-3 "   ")       ; 3 spaces for description/preview-general
                           (t ""))))
    (if (string-empty-p (string-trim clean-content))
        (propertize (concat indent-str "(empty)\n") 'face 'font-lock-comment-face)
      ;; Don't use shipit--render-markdown for preview - it uses markdown-mode
      ;; fontification which can strip language specifiers from code fences.
      ;; Instead, just return the content as-is and let code block backgrounds
      ;; handle syntax highlighting.
      ;; Render emoji shortcodes to Unicode emoji for preview.
      (let ((rendered (shipit-editor--render-emoji-shortcodes clean-content)))
        (cond
         ;; Reply type: render with nested child structure
         (indent-reply
          (concat
           ;; Child connector and header with depth-based indentation
           reply-header-indent "└─   " (propertize "You" 'face 'magit-log-author)
           " " (propertize "(draft)" 'face 'font-lock-comment-face) "\n"
           ;; Body with child indentation
           (mapconcat (lambda (line) (concat indent-str line))
                      (split-string rendered "\n")
                      "\n")
           "\n"))
         ;; Other types with indentation
         ((or indent-3 indent-6 indent-8)
          (concat
           (mapconcat (lambda (line) (concat indent-str line))
                      (split-string rendered "\n")
                      "\n")
           "\n"))
         ;; No indentation
         (t rendered))))))

(defun shipit-editor--setup-preview-overlay (context)
  "Set up preview overlay in current buffer based on CONTEXT.
CONTEXT is a plist with :type, :comment-id, :section-marker, :parent-comment-id, etc.
Returns a plist with :overlay, :start, :end or nil if setup fails."
  (let ((type (plist-get context :type))
        (comment-id (plist-get context :comment-id))
        (section-marker (plist-get context :section-marker))
        (parent-comment-id (plist-get context :parent-comment-id)))
    (shipit--debug-log "EDITOR: Setting up preview overlay for type=%s comment-id=%s section-marker=%s"
                       type comment-id section-marker)
    (save-excursion
      (pcase type
        ('description
         (shipit-editor--setup-description-preview))
        ('preview-description
         (shipit-editor--setup-preview-description-preview))
        ('general-comment
         (shipit-editor--setup-general-comment-preview comment-id))
        ('preview-general-comment
         (shipit-editor--setup-preview-general-comment-preview comment-id))
        ('preview-inline-comment
         (shipit-editor--setup-preview-inline-comment-preview context))
        ('preview-file-comment
         (shipit-editor--setup-preview-file-comment-preview context))
        ('inline-comment
         (shipit-editor--setup-inline-comment-preview comment-id section-marker))
        ('reply
         (shipit-editor--setup-reply-preview parent-comment-id section-marker))
        ('issue-comment
         (shipit-editor--setup-issue-comment-preview nil))
        ('issue-comment-edit
         (shipit-editor--setup-issue-comment-preview comment-id))
        ('issue-description
         (shipit-editor--setup-issue-description-preview))
        ('review
         ;; Reviews don't need live preview in source buffer
         nil)
        (_ nil)))))

(defun shipit-editor--setup-description-preview ()
  "Set up preview overlay for PR description editing.
Returns plist with :overlay :start :end :original-content or nil."
  (goto-char (point-min))
  ;; Find the description body region using text properties
  ;; The shipit-pr-description property is on both header and body,
  ;; so we need to find the body portion (after the header line)
  (let ((desc-start (text-property-any (point-min) (point-max)
                                       'shipit-pr-description t)))
    (shipit--debug-log "EDITOR: Description preview - desc-start=%s" desc-start)
    (when desc-start
      (goto-char desc-start)
      ;; Skip past the header line to get to the body
      (forward-line 1)
      (let ((body-start (point)))
        ;; Find the end of the description content - stop BEFORE the reaction line
        ;; The reaction line has shipit-reactions property, so we stop there
        (let ((region-end body-start)
              (reaction-start (text-property-any body-start (point-max) 'shipit-reactions t)))
          ;; If there's a reaction line, stop before it
          ;; Otherwise scan to end of shipit-pr-description region
          (if reaction-start
              (setq region-end reaction-start)
            (while (and (< region-end (point-max))
                        (get-text-property region-end 'shipit-pr-description))
              (setq region-end (or (next-single-property-change region-end 'shipit-pr-description)
                                   (point-max)))))
          (shipit--debug-log "EDITOR: Description preview - body-start=%d region-end=%d reaction-start=%s"
                             body-start region-end reaction-start)
          (when (> region-end body-start)
            ;; Capture original content for restoration on cancel
            (let* ((original-content (buffer-substring body-start region-end))
                   ;; Create overlay over the body content (excludes reaction line)
                   (overlay (make-overlay body-start region-end nil t nil)))
              (overlay-put overlay 'shipit-editor-preview t)
              (overlay-put overlay 'face '(:background "#2a2a3a"))
              (shipit--debug-log "EDITOR: Description preview overlay created %d-%d (reaction line preserved)"
                                 body-start region-end)
              (list :overlay overlay
                    :start body-start
                    :end region-end
                    :original-content original-content))))))))

(defun shipit-editor--setup-preview-description-preview ()
  "Set up preview overlay for preview buffer description editing.
Returns plist with :overlay :start :end :original-content or nil."
  (goto-char (point-min))
  ;; Find the description region using shipit-preview-description property
  (let ((desc-start (text-property-any (point-min) (point-max)
                                       'shipit-preview-description t)))
    (shipit--debug-log "EDITOR: Preview description - desc-start=%s" desc-start)
    (when desc-start
      (goto-char desc-start)
      (let ((section (magit-current-section)))
        ;; Expand the section if it's collapsed
        (when (and section (oref section hidden))
          (magit-section-show section))
        ;; Use magit section markers for proper structure preservation
        (when section
          (let* ((content-pos (oref section content))
                 (end-pos (oref section end))
                 (body-start (if (markerp content-pos) (marker-position content-pos) content-pos))
                 (body-end (if (markerp end-pos) (marker-position end-pos) end-pos))
                 (created-placeholder nil))
            (shipit--debug-log "EDITOR: Preview description section - content=%d end=%d"
                               body-start body-end)
            ;; If section body is empty, insert placeholder using magit section pattern
            (when (= body-start body-end)
              (let ((inhibit-read-only t))
                (goto-char body-start)
                (insert "   \n")
                (setq body-end (point))
                ;; Update section end marker
                (oset section end (point-marker))
                (setq created-placeholder t)))
            (when (> body-end body-start)
              (let* ((original-content (unless created-placeholder
                                         (buffer-substring body-start body-end)))
                     (overlay (make-overlay body-start body-end nil t nil)))
                (overlay-put overlay 'shipit-editor-preview t)
                (overlay-put overlay 'face '(:background "#2a2a3a"))
                ;; Always store section for cleanup - needed to fix markers on restore
                (overlay-put overlay 'shipit-section section)
                (when created-placeholder
                  (overlay-put overlay 'shipit-new-comment-placeholder t))
                (shipit--debug-log "EDITOR: Preview description overlay created %d-%d placeholder=%s"
                                   body-start body-end created-placeholder)
                (list :overlay overlay
                      :start body-start
                      :end body-end
                      :original-content original-content)))))))))

(defun shipit-editor--setup-general-comment-preview (comment-id)
  "Set up preview overlay for general comment editing.
COMMENT-ID is used when editing existing comment, nil for new comment.
Returns plist with :overlay :start :end or nil."
  (if comment-id
      ;; Editing existing comment - find the magit section
      (shipit-editor--find-inline-comment-section comment-id)
    ;; New comment - create draft section at end of general comments
    (shipit-editor--create-draft-general-comment-section)))

(defun shipit-editor--setup-preview-general-comment-preview (comment-id)
  "Set up preview overlay for preview buffer general comment editing.
COMMENT-ID is used when editing existing comment, nil for new comment.
Returns plist with :overlay :start :end or nil."
  (if comment-id
      ;; Editing existing comment - find it by shipit-comment-id property
      (shipit-editor--find-preview-comment-region comment-id)
    ;; New comment - create placeholder at end of general comments section
    (shipit-editor--create-preview-general-comment-placeholder)))

(defun shipit-editor--find-preview-comment-region (comment-id)
  "Find the region for preview COMMENT-ID and create an overlay.
Returns plist with :overlay :start :end :original-content or nil."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (< (point) (point-max)))
      (let ((prop-id (get-text-property (point) 'shipit-comment-id)))
        (if (and prop-id (equal prop-id comment-id))
            (setq found (point))
          (goto-char (or (next-single-property-change (point) 'shipit-comment-id)
                         (point-max))))))
    (when found
      (goto-char found)
      ;; Find the comment body within this comment region
      ;; Skip past the header line to get to the body
      (let* ((comment-start found)
             (comment-end (or (next-single-property-change (point) 'shipit-comment-id)
                              (point-max)))
             (section (magit-current-section)))
        (when section
          (let* ((content-pos (oref section content))
                 (end-pos (oref section end))
                 (body-start (if (markerp content-pos) (marker-position content-pos) content-pos))
                 (body-end (if (markerp end-pos) (marker-position end-pos) end-pos)))
            (when (> body-end body-start)
              (let* ((original-content (buffer-substring body-start body-end))
                     (overlay (make-overlay body-start body-end nil t nil)))
                (overlay-put overlay 'shipit-editor-preview t)
                (overlay-put overlay 'face '(:background "#2a2a3a"))
                (overlay-put overlay 'shipit-section section)
                (shipit--debug-log "EDITOR: Preview general comment overlay created %d-%d"
                                   body-start body-end)
                (list :overlay overlay
                      :start body-start
                      :end body-end
                      :original-content original-content)))))))))

(defun shipit-editor--create-preview-general-comment-placeholder ()
  "Create a preview placeholder for a new general comment in preview buffer.
Returns plist with :overlay :start :end :original-content or nil."
  (goto-char (point-min))
  ;; Find the general comments section by shipit-preview-general-comments property
  (let ((gc-start (text-property-any (point-min) (point-max)
                                     'shipit-preview-general-comments t)))
    (shipit--debug-log "EDITOR: Preview general comment placeholder - gc-start=%s" gc-start)
    (when gc-start
      (goto-char gc-start)
      (let ((section (magit-current-section)))
        ;; Expand the section if it's collapsed
        (when (and section (oref section hidden))
          (magit-section-show section))
        (when section
          (let* ((content-pos (oref section content))
                 (end-pos (oref section end))
                 (body-start (if (markerp content-pos) (marker-position content-pos) content-pos))
                 (body-end (if (markerp end-pos) (marker-position end-pos) end-pos)))
            (shipit--debug-log "EDITOR: Preview general comment section - content=%d end=%d"
                               body-start body-end)
            ;; Delete the existing "Press RET..." text and replace with placeholder
            (let ((inhibit-read-only t)
                  (original-content (buffer-substring body-start body-end)))
              (delete-region body-start body-end)
              (goto-char body-start)
              (insert "   \n")
              (let* ((new-end (point))
                     (overlay (make-overlay body-start new-end nil t nil)))
                ;; Update section end marker
                (oset section end (point-marker))
                (overlay-put overlay 'shipit-editor-preview t)
                (overlay-put overlay 'face '(:background "#2a2a3a"))
                (overlay-put overlay 'shipit-section section)
                (overlay-put overlay 'shipit-new-comment-placeholder t)
                (shipit--debug-log "EDITOR: Preview general comment placeholder created %d-%d"
                                   body-start new-end)
                (list :overlay overlay
                      :start body-start
                      :end new-end
                      :original-content original-content)))))))))

(defun shipit-editor--setup-preview-inline-comment-preview (context)
  "Set up preview overlay for preview buffer inline comment editing.
CONTEXT contains :comment-id for editing, :section-marker for new comments.
Returns plist with :overlay :start :end :original-content or nil."
  (let ((comment-id (plist-get context :comment-id))
        (section-marker (plist-get context :section-marker)))
    (if comment-id
        ;; Editing existing comment - find the magit section
        (shipit-editor--find-preview-inline-comment-section comment-id)
      ;; New comment - create a draft comment magit section
      (shipit-editor--create-draft-comment-section section-marker))))

(defun shipit-editor--setup-preview-file-comment-preview (context)
  "Set up preview overlay for preview buffer file-level comment editing.
CONTEXT contains :comment-id for editing, :section-marker for new comments.
Returns plist with :overlay :start :end :original-content or nil."
  (let ((comment-id (plist-get context :comment-id))
        (section-marker (plist-get context :section-marker)))
    (if comment-id
        ;; Editing existing comment - find the magit section
        (shipit-editor--find-preview-inline-comment-section comment-id)
      ;; New comment - create a draft comment magit section
      (shipit-editor--create-draft-comment-section section-marker))))

;; shipit-editor--find-preview-inline-comment-section is aliased to the shared function
(defalias 'shipit-editor--find-preview-inline-comment-section
  'shipit-editor--find-inline-comment-section
  "Find the magit section for preview inline comment. Alias to shared function.")

(defun shipit-editor--find-end-of-comment-thread (start-pos)
  "Find the end of the comment thread starting near START-POS.
Returns the position after the last comment in the thread, or START-POS
if no existing comments are found."
  (save-excursion
    (goto-char start-pos)
    ;; Get the line number we're commenting on
    (let ((line-number (get-text-property (point) 'shipit-line-number))
          (file-path (get-text-property (point) 'shipit-file-path))
          (last-comment-end nil))
      ;; Only search if we have both line-number and file-path (inline comment context)
      (when (and line-number file-path)
        ;; Find the boundary of this file section - where shipit-file-path changes or ends
        (let* ((file-section-end (or (next-single-property-change start-pos 'shipit-file-path)
                                     (point-max)))
               (search-limit (min file-section-end (+ start-pos 5000))))
          (while (< (point) search-limit)
            (let ((pos (point))
                  (current-file (get-text-property (point) 'shipit-file-path))
                  (current-line (get-text-property (point) 'shipit-line-number))
                  (current-comment-id (get-text-property (point) 'shipit-comment-id)))
              ;; Stop immediately if file-path is nil or different (left the file section)
              (when (or (null current-file) (not (equal current-file file-path)))
                (goto-char search-limit))
              ;; Check if this is a comment section for our exact line
              (when (and current-comment-id
                         current-file
                         (equal current-line line-number)
                         (equal current-file file-path))
                ;; Found a comment for this line - find its section end
                (let ((section (magit-current-section)))
                  (when section
                    (let ((end-pos (oref section end)))
                      (setq last-comment-end
                            (if (markerp end-pos)
                                (marker-position end-pos)
                              end-pos))))))
              ;; Move to next position
              (let ((next-pos (next-single-property-change pos 'shipit-comment-id)))
                (if (and next-pos (< next-pos search-limit))
                    (goto-char next-pos)
                  (goto-char search-limit)))))))
      ;; Return last comment end if found, otherwise start-pos
      (or last-comment-end start-pos))))

(defun shipit-editor--create-draft-comment-section (section-marker)
  "Create a draft comment magit section at SECTION-MARKER for live preview.
Inserts at the end of the existing comment thread if there are comments.
Returns plist with :overlay :start :end or nil."
  (let* ((start-pos (cond
                     ((and section-marker (markerp section-marker) (marker-position section-marker))
                      (marker-position section-marker))
                     ((and section-marker (numberp section-marker))
                      section-marker)
                     (t (point))))
         ;; Capture the parent section at the ORIGINAL position before moving
         (parent-section (save-excursion
                           (goto-char start-pos)
                           (magit-current-section)))
         ;; Find the end of existing comment thread
         (insert-pos (shipit-editor--find-end-of-comment-thread start-pos)))
    (goto-char insert-pos)
    ;; If we're at start-pos (no existing comments), move to end of current line
    (when (= insert-pos start-pos)
      (end-of-line)
      (setq insert-pos (point)))
    ;; Capture original parent end BEFORE we insert anything
    (let* ((parent-end-pos (when parent-section (oref parent-section end)))
           (original-parent-end (when parent-end-pos
                                  (if (markerp parent-end-pos)
                                      (marker-position parent-end-pos)
                                    parent-end-pos)))
           (inhibit-read-only t)
           (section-start (point))
           (temp-comment-id (format "draft-%s" (format-time-string "%s%N")))
           (section nil)
           (body-text-start nil)
           (body-text-end nil))
      ;; Insert newline before section
      (insert "\n")
      (let ((_content-start (point))  ; silence compiler warning
            ;; Bind parent so magit-insert-section creates proper hierarchy
            (magit-insert-section--parent parent-section))
        ;; Create the magit section with correct parent
        (setq section
              (magit-insert-section (inline-comment temp-comment-id nil)
                ;; Heading with draft indicator - use SVG icon with emoji fallback
                (let ((header-start (point))
                      (icon (shipit--get-comment-type-icon "comment" "💬")))
                  (magit-insert-heading
                    (propertize (format "     %s You (draft) (now)" icon) 'face 'bold))
                  (add-text-properties header-start (point)
                                       `(shipit-comment t
                                         shipit-comment-id ,temp-comment-id)))
                ;; Body section - use magit-insert-section-body for proper collapse behavior
                (magit-insert-section-body
                  ;; Body text placeholder (will be updated by live preview)
                  (setq body-text-start (point))
                  (insert "        ")  ; 8 spaces indent for inline comment body
                  (insert "(empty)\n")
                  (setq body-text-end (point))
                  (add-text-properties body-text-start body-text-end
                                       `(shipit-comment t
                                         shipit-comment-id ,temp-comment-id
                                         shipit-comment-body-text t
                                         face font-lock-comment-face))
                  ;; Blank line before reactions (matches real inline comment format)
                  (let ((blank-start (point)))
                    (insert "\n")
                    (add-text-properties blank-start (point)
                                         `(shipit-comment t
                                           shipit-comment-id ,temp-comment-id)))
                  ;; Reactions line placeholder (aligned with body, 8 spaces for inline)
                  (let ((reactions-start (point))
                        (placeholder (if (fboundp 'shipit--get-reactions-placeholder-icon)
                                         (shipit--get-reactions-placeholder-icon)
                                       "☺")))
                    (insert (format "        %s\n" placeholder))
                    (add-text-properties reactions-start (point)
                                         `(shipit-reactions t
                                           shipit-comment t
                                           shipit-comment-id ,temp-comment-id))))))
        ;; Create overlay over the body text only (for live preview updates)
        ;; NOTE: The overlay covers only body text, NOT blank line + reactions,
        ;; because those are static and shouldn't be replaced during preview updates.
        ;; The section's end marker includes everything for proper collapse behavior.
        (when (and section body-text-start body-text-end)
          (let* ((section-end-pos (oref section end))
                 (section-end (if (markerp section-end-pos) (marker-position section-end-pos) section-end-pos))
                 (overlay (make-overlay body-text-start body-text-end nil t nil)))
            (overlay-put overlay 'shipit-editor-preview t)
            (overlay-put overlay 'face '(:background "#2a2a3a"))
            (overlay-put overlay 'shipit-section section)
            (overlay-put overlay 'shipit-new-comment-placeholder t)
            ;; Track the section start for cleanup (to remove entire section)
            (overlay-put overlay 'shipit-section-start section-start)
            ;; Store the parent section for cleanup to restore its end marker
            (overlay-put overlay 'shipit-parent-section parent-section)
            ;; Store original parent end for restoration on cleanup
            (overlay-put overlay 'shipit-original-parent-end original-parent-end)
            ;; Update parent section's end marker to include the new draft
            ;; This is critical for navigation and collapsing to work properly
            (when parent-section
              (oset parent-section end (copy-marker (point)))
              ;; Ensure the new section is in the parent's children list
              (unless (memq section (oref parent-section children))
                (oset parent-section children
                      (append (oref parent-section children) (list section)))))
            ;; Call magit's internal function to properly set section properties
            (when (fboundp 'magit-section--set-section-properties)
              (magit-section--set-section-properties section))
            (shipit--debug-log "EDITOR: Draft inline comment section body=%d-%d section-end=%d"
                               body-text-start body-text-end section-end)
            (list :overlay overlay
                  :start body-text-start
                  :end body-text-end)))))))

(defun shipit-editor--setup-inline-comment-preview (comment-id section-marker)
  "Set up preview overlay for inline comment editing.
COMMENT-ID is used when editing existing comment, nil for new comment.
SECTION-MARKER is the position where new comments should appear.
Returns plist with :overlay :start :end or nil."
  (if comment-id
      ;; Editing existing comment - find the magit section
      (shipit-editor--find-inline-comment-section comment-id)
    ;; New inline comment - create a draft comment magit section
    (shipit-editor--create-draft-comment-section section-marker)))

(defun shipit-editor--find-inline-comment-section (comment-id)
  "Find the magit section for inline COMMENT-ID and create an overlay.
Works for both shipit buffer and preview buffer.
Returns plist with :overlay :start :end :original-content or nil."
  ;; Inline comments are rendered as magit sections (inline-comment type)
  ;; Find the section by searching for shipit-comment-id property
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (< (point) (point-max)))
      (let ((prop-id (get-text-property (point) 'shipit-comment-id)))
        (if (and prop-id (equal (format "%s" prop-id) (format "%s" comment-id)))
            (setq found (point))
          (goto-char (or (next-single-property-change (point) 'shipit-comment-id)
                         (point-max))))))
    (when found
      (goto-char found)
      (let ((section (magit-current-section)))
        (when section
          (let* ((content-pos (oref section content))
                 (end-pos (oref section end))
                 (body-start (if (markerp content-pos) (marker-position content-pos) content-pos))
                 (body-end (if (markerp end-pos) (marker-position end-pos) end-pos)))
            (when (> body-end body-start)
              (let* ((original-content (buffer-substring body-start body-end))
                     (overlay (make-overlay body-start body-end nil t nil)))
                (overlay-put overlay 'shipit-editor-preview t)
                (overlay-put overlay 'face '(:background "#2a2a3a"))
                (overlay-put overlay 'shipit-section section)
                (shipit--debug-log "EDITOR: Inline comment section overlay created %d-%d for id=%s"
                                   body-start body-end comment-id)
                (list :overlay overlay
                      :start body-start
                      :end body-end
                      :original-content original-content)))))))))

(defun shipit-editor--create-draft-general-comment-section ()
  "Create a draft general comment magit section for live preview.
Finds the general comments section and inserts a draft section at the end.
Uses the same format as real general comments (with avatar, not SVG icon).
Returns plist with :overlay :start :end or nil."
  ;; Find general comments section
  (goto-char (point-min))
  (let ((gc-start (text-property-any (point-min) (point-max)
                                     'shipit-general-comments t)))
    (when gc-start
      (goto-char gc-start)
      (let ((parent-section (magit-current-section)))
        ;; Expand section if collapsed
        (when (and parent-section (oref parent-section hidden))
          (magit-section-show parent-section))
        (when parent-section
          (let* ((end-pos (oref parent-section end))
                 (end-pos-val (if (markerp end-pos) (marker-position end-pos) end-pos))
                 (content-pos (oref parent-section content))
                 (content-pos-val (if (markerp content-pos) (marker-position content-pos) content-pos))
                 (inhibit-read-only t)
                 (temp-comment-id (format "draft-%s" (format-time-string "%s%N")))
                 (section nil)
                 (section-start nil)
                 (body-text-start nil)
                 (body-text-end nil))
            ;; Insert at end of parent section
            (goto-char end-pos-val)
            (setq section-start (point))
            (let ((magit-insert-section--parent parent-section))
              ;; Create general-comment section (not inline-comment)
              (setq section
                    (magit-insert-section (general-comment temp-comment-id)
                      ;; Heading matching real general comment format
                      ;; Use avatar if available (like real general comments)
                      (let* ((header-start (point))
                             ;; Get current user's username for avatar lookup
                             (username (or (and (boundp 'shipit--cached-current-user)
                                                shipit--cached-current-user)
                                           "You"))
                             (display-name (if (string= username "You") "You" username))
                             ;; Try to get avatar for current user (may be cached from viewing comments)
                             (avatar-display (if (and (boundp 'shipit-show-avatars)
                                                      shipit-show-avatars
                                                      (fboundp 'shipit--create-avatar-display))
                                                 (shipit--create-avatar-display username nil 16)
                                               ""))
                             ;; If no avatar loaded, fall back to comment icon
                             (has-avatar (and (not (string= avatar-display ""))
                                              (not (string= avatar-display "👤"))))
                             (icon-or-avatar (if has-avatar
                                                 (concat avatar-display " ")
                                               (concat (if (fboundp 'shipit--get-comment-type-icon)
                                                           (shipit--get-comment-type-icon "comment" "💬")
                                                         "💬") " ")))
                             (timestamp-str "(now)")
                             (draft-str "(draft)"))
                        (magit-insert-heading
                          (concat "   "
                                  icon-or-avatar
                                  (propertize display-name 'face 'shipit-username-face)
                                  " "
                                  (propertize draft-str 'face 'shadow)
                                  " "
                                  (propertize timestamp-str 'face 'shipit-timestamp-face)))
                        (add-text-properties header-start (point)
                                             `(shipit-comment t
                                               shipit-comment-id ,temp-comment-id)))
                      ;; Body section - use magit-insert-section-body for proper collapse behavior
                      (magit-insert-section-body
                        ;; Body text placeholder (will be updated by live preview)
                        (setq body-text-start (point))
                        (insert "      ")  ; 6 spaces indent for general comment body
                        (insert "(empty)\n")
                        (setq body-text-end (point))
                        (add-text-properties body-text-start body-text-end
                                             `(shipit-comment t
                                               shipit-comment-id ,temp-comment-id
                                               shipit-comment-body-text t
                                               face font-lock-comment-face))
                        ;; Blank line before reactions (matches real comment format)
                        (let ((blank-start (point)))
                          (insert "\n")
                          (add-text-properties blank-start (point)
                                               `(shipit-comment t
                                                 shipit-comment-id ,temp-comment-id)))
                        ;; Reactions line placeholder (aligned with username, 6 spaces)
                        (let ((reactions-start (point))
                              (placeholder (if (fboundp 'shipit--get-reactions-placeholder-icon)
                                               (shipit--get-reactions-placeholder-icon)
                                             "☺")))
                          (insert (format "      %s \n" placeholder))
                          (add-text-properties reactions-start (point)
                                               `(shipit-reactions t
                                                 shipit-comment t
                                                 shipit-comment-id ,temp-comment-id)))))))
            ;; Create overlay over the body text only (for live preview updates)
            ;; NOTE: The overlay covers only body text, NOT blank line + reactions,
            ;; because those are static and shouldn't be replaced during preview updates.
            ;; The section's end marker includes everything for proper collapse behavior.
            (when (and section body-text-start body-text-end)
              (let* ((section-end-pos (oref section end))
                     (section-end (if (markerp section-end-pos) (marker-position section-end-pos) section-end-pos))
                     (overlay (make-overlay body-text-start body-text-end nil t nil)))
                (overlay-put overlay 'shipit-editor-preview t)
                (overlay-put overlay 'face '(:background "#2a2a3a"))
                (overlay-put overlay 'shipit-section section)
                (overlay-put overlay 'shipit-new-comment-placeholder t)
                (overlay-put overlay 'shipit-section-start section-start)
                ;; Store the parent section for cleanup to restore its end marker
                (overlay-put overlay 'shipit-parent-section parent-section)
                ;; Store original parent end for restoration on cleanup
                (overlay-put overlay 'shipit-original-parent-end end-pos-val)
                ;; Update parent section's end marker to include the new draft
                ;; This is critical for collapsing to work properly
                (oset parent-section end (copy-marker (point)))
                ;; Ensure the new section is in the parent's children list
                ;; magit-insert-section should have added it, but verify
                (unless (memq section (oref parent-section children))
                  (oset parent-section children
                        (append (oref parent-section children) (list section))))
                ;; Call magit's internal function to properly set section properties
                ;; This is what magit does during normal section creation
                (when (fboundp 'magit-section--set-section-properties)
                  (magit-section--set-section-properties section))
                (shipit--debug-log "EDITOR: Draft general comment section body=%d-%d section-end=%d"
                                   body-text-start body-text-end section-end)
                (list :overlay overlay
                      :start body-text-start
                      :end body-text-end)))))))))

(defun shipit-editor--create-draft-issue-comment-section ()
  "Create a draft issue comment magit section for live preview.
Finds the issue-comments section and inserts a draft section at the end.
Returns plist with :overlay :start :end or nil."
  (let ((parent-section (shipit--find-section-by-type 'issue-comments)))
    (when parent-section
      ;; Expand section if collapsed
      (when (oref parent-section hidden)
        (magit-section-show parent-section))
      (let* ((end-pos (oref parent-section end))
             (end-pos-val (if (markerp end-pos) (marker-position end-pos) end-pos))
             (inhibit-read-only t)
             (temp-comment-id (format "draft-%s" (format-time-string "%s%N")))
             (section nil)
             (section-start nil)
             (body-text-start nil)
             (body-text-end nil))
        ;; Insert at end of parent section
        (goto-char end-pos-val)
        (setq section-start (point))
        (let ((magit-insert-section--parent parent-section))
          (setq section
                (magit-insert-section (issue-comment temp-comment-id)
                  (let* ((header-start (point))
                         (username (or (and (boundp 'shipit--cached-current-user)
                                            shipit--cached-current-user)
                                       "You"))
                         (display-name (if (string= username "You") "You" username))
                         (avatar-display (if (and (boundp 'shipit-show-avatars)
                                                  shipit-show-avatars
                                                  (fboundp 'shipit--create-avatar-display))
                                             (shipit--create-avatar-display username nil 16)
                                           ""))
                         (has-avatar (and (not (string= avatar-display ""))
                                          (not (string= avatar-display "👤"))))
                         (icon-or-avatar (if has-avatar
                                             (concat avatar-display " ")
                                           (concat (if (fboundp 'shipit--get-comment-type-icon)
                                                       (shipit--get-comment-type-icon "comment" "💬")
                                                     "💬") " "))))
                    (magit-insert-heading
                      (concat "   "
                              icon-or-avatar
                              (propertize display-name 'face 'shipit-username-face)
                              " "
                              (propertize "(draft)" 'face 'shadow)
                              " "
                              (propertize "(now)" 'face 'shipit-timestamp-face)))
                    (add-text-properties header-start (point)
                                         `(shipit-comment t
                                           shipit-comment-id ,temp-comment-id)))
                  (magit-insert-section-body
                    (setq body-text-start (point))
                    (insert "      (empty)\n")
                    (setq body-text-end (point))
                    (add-text-properties body-text-start body-text-end
                                         `(shipit-comment t
                                           shipit-comment-id ,temp-comment-id
                                           shipit-comment-body-text t
                                           face font-lock-comment-face))
                    (let ((blank-start (point)))
                      (insert "\n")
                      (add-text-properties blank-start (point)
                                           `(shipit-comment t
                                             shipit-comment-id ,temp-comment-id)))
                    (let ((reactions-start (point))
                          (placeholder (if (fboundp 'shipit--get-reactions-placeholder-icon)
                                           (shipit--get-reactions-placeholder-icon)
                                         "☺")))
                      (insert (format "      %s \n" placeholder))
                      (add-text-properties reactions-start (point)
                                           `(shipit-reactions t
                                             shipit-comment t
                                             shipit-comment-id ,temp-comment-id)))))))
        ;; Create overlay over the body text only
        (when (and section body-text-start body-text-end)
          (let* ((section-end-pos (oref section end))
                 (section-end (if (markerp section-end-pos) (marker-position section-end-pos) section-end-pos))
                 (overlay (make-overlay body-text-start body-text-end nil t nil)))
            (overlay-put overlay 'shipit-editor-preview t)
            (overlay-put overlay 'face '(:background "#2a2a3a"))
            (overlay-put overlay 'shipit-section section)
            (overlay-put overlay 'shipit-new-comment-placeholder t)
            (overlay-put overlay 'shipit-section-start section-start)
            (overlay-put overlay 'shipit-parent-section parent-section)
            (overlay-put overlay 'shipit-original-parent-end end-pos-val)
            (oset parent-section end (copy-marker (point)))
            (unless (memq section (oref parent-section children))
              (oset parent-section children
                    (append (oref parent-section children) (list section))))
            (when (fboundp 'magit-section--set-section-properties)
              (magit-section--set-section-properties section))
            (shipit--debug-log "EDITOR: Draft issue comment section body=%d-%d section-end=%d"
                               body-text-start body-text-end section-end)
            (list :overlay overlay
                  :start body-text-start
                  :end body-text-end)))))))

(defun shipit-editor--setup-issue-comment-preview (comment-id)
  "Set up preview overlay for issue comment editing.
COMMENT-ID is used when editing existing comment, nil for new comment.
Returns plist with :overlay :start :end or nil."
  (if comment-id
      (shipit-editor--find-inline-comment-section comment-id)
    (shipit-editor--create-draft-issue-comment-section)))

(defun shipit-editor--setup-issue-description-preview ()
  "Set up preview overlay for issue description editing.
Finds the shipit-issue-description section and creates overlay on body.
Returns plist with :overlay :start :end :original-content or nil."
  (let ((section (shipit--find-section-by-type 'shipit-issue-description)))
    (when section
      ;; Expand the section if collapsed
      (when (oref section hidden)
        (magit-section-show section))
      (let* ((content-pos (oref section content))
             (end-pos (oref section end))
             (body-start (if (markerp content-pos) (marker-position content-pos) content-pos))
             (body-end (if (markerp end-pos) (marker-position end-pos) end-pos)))
        (when (> body-end body-start)
          (let* ((original-content (buffer-substring body-start body-end))
                 (overlay (make-overlay body-start body-end nil t nil)))
            (overlay-put overlay 'shipit-editor-preview t)
            (overlay-put overlay 'face '(:background "#2a2a3a"))
            (overlay-put overlay 'shipit-section section)
            (shipit--debug-log "EDITOR: Issue description preview overlay created %d-%d"
                               body-start body-end)
            (list :overlay overlay
                  :start body-start
                  :end body-end
                  :original-content original-content)))))))

(defun shipit-editor--setup-reply-preview (parent-comment-id section-marker)
  "Set up preview overlay for a reply to a comment.
PARENT-COMMENT-ID is the comment being replied to.
SECTION-MARKER is used as a starting point to find the parent comment.
Returns plist with :overlay :start :end or nil."
  ;; Find the parent comment's magit section and insert after its END
  ;; This ensures the reply appears after the full comment (header + body + reactions)
  (let ((insert-pos
         (when parent-comment-id
           (save-excursion
             ;; Find the parent comment by its shipit-comment-id property
             (goto-char (point-min))
             (let ((comment-pos nil))
               (while (and (not comment-pos) (< (point) (point-max)))
                 (let ((prop-id (get-text-property (point) 'shipit-comment-id)))
                   (if (and prop-id (equal prop-id parent-comment-id))
                       (setq comment-pos (point))
                     (goto-char (or (next-single-property-change (point) 'shipit-comment-id)
                                    (point-max))))))
               ;; Now find the magit section at this position and get its end
               (when comment-pos
                 (goto-char comment-pos)
                 (let ((section (magit-current-section)))
                   (when section
                     ;; Get the section's end marker position
                     (let ((section-end (oref section end)))
                       (if (markerp section-end)
                           (marker-position section-end)
                         section-end))))))))))
    (when insert-pos
      (goto-char insert-pos)
      (let ((inhibit-read-only t)
            (preview-start (point)))
        (insert "\n")
        (let* ((preview-end (point))
               (overlay (make-overlay preview-start preview-end nil t nil)))
          (overlay-put overlay 'shipit-editor-preview t)
          (overlay-put overlay 'face '(:background "#2a2a3a"))
          (overlay-put overlay 'shipit-new-comment-placeholder t)
          (shipit--debug-log "EDITOR: Reply preview overlay created %d-%d for parent=%s"
                             preview-start preview-end parent-comment-id)
          (list :overlay overlay
                :start preview-start
                :end preview-end))))))

(defun shipit-editor--cleanup-preview-overlay (overlay &optional original-content keep-content)
  "Clean up OVERLAY and restore ORIGINAL-CONTENT if provided.
If ORIGINAL-CONTENT is non-nil, replaces the overlay region with it.
If KEEP-CONTENT is non-nil, keeps the preview content in place (for save)."
  (shipit--debug-log "EDITOR: cleanup-preview-overlay called - overlay=%s original-content=%s keep=%s"
                     (if overlay "YES" "NO")
                     (if original-content (length original-content) "NIL")
                     (if keep-content "YES" "NO"))
  (when (and overlay (overlay-buffer overlay))
    (let ((start (overlay-start overlay))
          (end (overlay-end overlay))
          (buf (overlay-buffer overlay))
          (section (overlay-get overlay 'shipit-section))
          (section-start (overlay-get overlay 'shipit-section-start))
          (preceding-newline-pos (overlay-get overlay 'shipit-preceding-newline-pos)))
      (shipit--debug-log "EDITOR: cleanup - start=%d end=%d buf=%s section=%s placeholder=%s section-start=%s"
                         start end (buffer-name buf) (if section "YES" "NO")
                         (if (overlay-get overlay 'shipit-new-comment-placeholder) "YES" "NO")
                         section-start)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (cond
           ;; Keep content in place (for save) - just delete overlay, keep text
           (keep-content
            (shipit--debug-log "EDITOR: Keeping preview content in place (save mode)")
            ;; Just remove the draft indicator from the heading if possible
            (save-excursion
              (goto-char start)
              (when (re-search-backward "(draft) " (line-beginning-position) t)
                (replace-match ""))))
           ;; Restore original content if provided
           (original-content
            (shipit--debug-log "EDITOR: Restoring original content len=%d" (length original-content))
            (delete-region start end)
            (goto-char start)
            (insert original-content)
            ;; Update section end marker if we have one
            (when section
              (shipit--debug-log "EDITOR: Updating section end to %d" (point))
              (oset section end (copy-marker (point))))
            ;; Re-apply code block backgrounds to restored content
            (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
              (shipit--apply-code-block-backgrounds-in-region start (point)))
            ;; Also delete preceding newline if we added one
            (when preceding-newline-pos
              (shipit--debug-log "EDITOR: Deleting preceding newline at %d" preceding-newline-pos)
              (delete-region preceding-newline-pos (1+ preceding-newline-pos))))
           ;; New comment placeholder with section means delete entire draft section
           ((and (overlay-get overlay 'shipit-new-comment-placeholder) section)
            (let* ((sec-start-marker (oref section start))
                   (sec-end-marker (oref section end))
                   (sec-start (if (markerp sec-start-marker)
                                  (marker-position sec-start-marker)
                                sec-start-marker))
                   (sec-end (if (markerp sec-end-marker)
                                (marker-position sec-end-marker)
                              sec-end-marker))
                   (parent-section (overlay-get overlay 'shipit-parent-section)))
              ;; Also delete the newline before section if we added one
              (when (and section-start (< section-start sec-start))
                (setq sec-start section-start))
              (shipit--debug-log "EDITOR: Cleanup draft section from %d to %d" sec-start sec-end)
              (delete-region sec-start sec-end)
              ;; Restore parent section's end marker to the deletion point
              (when parent-section
                (shipit--debug-log "EDITOR: Restoring parent section end to %d" sec-start)
                (oset parent-section end (copy-marker sec-start)))))
           ;; New comment placeholder without section-start - just delete body
           ((overlay-get overlay 'shipit-new-comment-placeholder)
            (shipit--debug-log "EDITOR: Cleanup placeholder - start=%d end=%d section=%s"
                               start end (if section "YES" "NO"))
            ;; Save start position before delete (markers move after delete)
            (let ((restore-pos start))
              (delete-region start end)
              ;; Also delete preceding newline if we added one
              (when preceding-newline-pos
                (shipit--debug-log "EDITOR: Deleting preceding newline at %d" preceding-newline-pos)
                (delete-region preceding-newline-pos (1+ preceding-newline-pos))
                (setq restore-pos preceding-newline-pos))
              ;; Restore section end marker to where content starts (empty body)
              (when section
                (shipit--debug-log "EDITOR: Restoring section end to pos=%d" restore-pos)
                (oset section end (copy-marker restore-pos)))))))))
    (delete-overlay overlay)))

;;; Smart Completion Triggers (context-aware + debounce)
;;
;; These triggers combine two mechanisms:
;; 1. Context check: instantly reject completion in invalid contexts
;;    - # after # (markdown headers like ##, ###)
;;    - : after space or at start (not after a word for emoji shortcodes)
;;    - @ in email-like contexts (after alphanumeric)
;; 2. Debounce: for valid contexts, wait briefly and cancel if another key pressed

(defun shipit-editor--cancel-completion-timer ()
  "Cancel any pending completion timer."
  (when shipit-editor--completion-timer
    (cancel-timer shipit-editor--completion-timer)
    (setq shipit-editor--completion-timer nil
          shipit-editor--completion-char nil
          shipit-editor--completion-pos nil)))

(defun shipit-editor--maybe-cancel-completion ()
  "Cancel completion if the user typed another character.
Called from `post-command-hook'."
  ;; Only act if we have a pending completion and this isn't the trigger command
  (when (and shipit-editor--completion-timer
             (not (memq this-command '(shipit-editor--trigger-pr-reference
                                       shipit-editor--trigger-mr-reference
                                       shipit-editor--trigger-user-mention
                                       shipit-editor--trigger-emoji))))
    (shipit-editor--cancel-completion-timer)))

(defun shipit-editor--char-before ()
  "Return character before point, or nil if at beginning of buffer."
  (when (> (point) (point-min))
    (char-before)))

(defun shipit-editor--should-complete-pr-reference-p ()
  "Return non-nil if PR reference completion should trigger.
Returns nil if preceded by # (markdown header context)."
  (let ((prev-char (shipit-editor--char-before)))
    (not (eq prev-char ?#))))

(defun shipit-editor--should-complete-user-mention-p ()
  "Return non-nil if user mention completion should trigger.
Returns nil if preceded by alphanumeric (email-like context)."
  (let ((prev-char (shipit-editor--char-before)))
    (not (and prev-char
              (or (and (>= prev-char ?a) (<= prev-char ?z))
                  (and (>= prev-char ?A) (<= prev-char ?Z))
                  (and (>= prev-char ?0) (<= prev-char ?9)))))))

(defun shipit-editor--should-complete-emoji-p ()
  "Return non-nil if emoji completion should trigger.
Returns nil if preceded by : (typing ::) or certain other contexts."
  (let ((prev-char (shipit-editor--char-before)))
    ;; Don't trigger after : (typing ::)
    ;; Do trigger after space, newline, start of buffer, or most other chars
    (not (eq prev-char ?:))))

(defun shipit-editor--trigger-pr-reference ()
  "Handle # key with context-aware completion.
Inserts # immediately. If context is valid for completion, schedules
completion after a short delay. Typing another key cancels completion."
  (interactive)
  (shipit-editor--cancel-completion-timer)
  ;; Check context BEFORE inserting
  (let ((should-complete (shipit-editor--should-complete-pr-reference-p)))
    (insert "#")
    (when should-complete
      (let ((expected-point (point)))
        (setq shipit-editor--completion-char ?#
              shipit-editor--completion-pos (1- (point))
              shipit-editor--completion-timer
              (run-with-idle-timer shipit-editor--completion-delay nil
                                   #'shipit-editor--complete-pr-reference
                                   (current-buffer)
                                   expected-point))))))

(defun shipit-editor--trigger-user-mention ()
  "Handle @ key with context-aware completion.
Inserts @ immediately. If context is valid for completion, schedules
completion after a short delay. Typing another key cancels completion."
  (interactive)
  (shipit-editor--cancel-completion-timer)
  ;; Check context BEFORE inserting
  (let ((should-complete (shipit-editor--should-complete-user-mention-p)))
    (insert "@")
    (when should-complete
      (let ((expected-point (point)))
        (setq shipit-editor--completion-char ?@
              shipit-editor--completion-pos (1- (point))
              shipit-editor--completion-timer
              (run-with-idle-timer shipit-editor--completion-delay nil
                                   #'shipit-editor--complete-user-mention
                                   (current-buffer)
                                   expected-point))))))

(defun shipit-editor--trigger-emoji ()
  "Handle : key with context-aware completion.
Inserts : immediately. If context is valid for completion, schedules
completion after a short delay. Typing another key cancels completion."
  (interactive)
  (shipit-editor--cancel-completion-timer)
  ;; Check context BEFORE inserting
  (let ((should-complete (shipit-editor--should-complete-emoji-p)))
    (insert ":")
    (when should-complete
      (let ((expected-point (point)))
        (setq shipit-editor--completion-char ?:
              shipit-editor--completion-pos (1- (point))
              shipit-editor--completion-timer
              (run-with-idle-timer shipit-editor--completion-delay nil
                                   #'shipit-editor--complete-emoji
                                   (current-buffer)
                                   expected-point))))))

(defun shipit-editor--should-complete-mr-reference-p ()
  "Return non-nil if MR reference completion should trigger.
Returns nil if preceded by ! (like !! patterns)."
  (let ((prev-char (shipit-editor--char-before)))
    (not (eq prev-char ?!))))

(defun shipit-editor--trigger-mr-reference ()
  "Handle ! key with context-aware completion (GitLab MR references).
Inserts ! immediately.  If context is valid, schedules completion."
  (interactive)
  (shipit-editor--cancel-completion-timer)
  (let ((should-complete (shipit-editor--should-complete-mr-reference-p)))
    (insert "!")
    (when should-complete
      (let ((expected-point (point)))
        (setq shipit-editor--completion-char ?!
              shipit-editor--completion-pos (1- (point))
              shipit-editor--completion-timer
              (run-with-idle-timer shipit-editor--completion-delay nil
                                   #'shipit-editor--complete-mr-reference
                                   (current-buffer)
                                   expected-point))))))

(defun shipit-editor--complete-mr-reference (buffer expected-point)
  "Complete MR reference in BUFFER.
EXPECTED-POINT is where point should be for completion to proceed.
Called by the debounce timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shipit-editor--completion-timer nil)
      (when (shipit-editor--should-complete-now-p ?! expected-point)
        (delete-region shipit-editor--completion-pos (1+ shipit-editor--completion-pos))
        (goto-char shipit-editor--completion-pos)
        (shipit-editor-insert-mr-reference))
      (setq shipit-editor--completion-char nil
            shipit-editor--completion-pos nil))))

(defun shipit-editor--should-complete-now-p (trigger-char expected-point)
  "Return non-nil if completion should proceed.
TRIGGER-CHAR is the character that started completion.
EXPECTED-POINT is where point should be (right after trigger char).
Returns nil if user has typed more characters since the trigger."
  (and shipit-editor--completion-pos
       (< shipit-editor--completion-pos (point-max))
       ;; Check point is where we expect (user hasn't typed more)
       (= (point) expected-point)
       ;; Check the trigger char is still there
       (eq (char-after shipit-editor--completion-pos) trigger-char)))

(defun shipit-editor--complete-pr-reference (buffer expected-point)
  "Complete PR reference in BUFFER.
EXPECTED-POINT is where point should be for completion to proceed.
Called by the debounce timer.  Dispatches through backend's
:hash-insert-reference-fn, defaulting to PR reference insertion."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shipit-editor--completion-timer nil)
      (when (shipit-editor--should-complete-now-p ?# expected-point)
        ;; Delete the # we inserted
        (delete-region shipit-editor--completion-pos (1+ shipit-editor--completion-pos))
        (goto-char shipit-editor--completion-pos)
        ;; Dispatch through backend plist
        (let* ((backend-plist (cdr (assq shipit-editor--pr-backend-id shipit-pr-backends)))
               (insert-fn (or (plist-get backend-plist :hash-insert-reference-fn)
                              #'shipit-editor-insert-pr-reference)))
          (funcall insert-fn)))
      (setq shipit-editor--completion-char nil
            shipit-editor--completion-pos nil))))

(defun shipit-editor--complete-user-mention (buffer expected-point)
  "Complete user mention in BUFFER.
EXPECTED-POINT is where point should be for completion to proceed.
Called by the debounce timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shipit-editor--completion-timer nil)
      (when (shipit-editor--should-complete-now-p ?@ expected-point)
        ;; Delete the @ we inserted
        (delete-region shipit-editor--completion-pos (1+ shipit-editor--completion-pos))
        (goto-char shipit-editor--completion-pos)
        ;; Run the actual completion
        (shipit-editor-insert-user-mention))
      (setq shipit-editor--completion-char nil
            shipit-editor--completion-pos nil))))

(defun shipit-editor--complete-emoji (buffer expected-point)
  "Complete emoji in BUFFER.
EXPECTED-POINT is where point should be for completion to proceed.
Called by the debounce timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shipit-editor--completion-timer nil)
      (when (shipit-editor--should-complete-now-p ?: expected-point)
        ;; Delete the : we inserted
        (delete-region shipit-editor--completion-pos (1+ shipit-editor--completion-pos))
        (goto-char shipit-editor--completion-pos)
        ;; Run the actual completion
        (shipit-editor-insert-emoji))
      (setq shipit-editor--completion-char nil
            shipit-editor--completion-pos nil))))

;;; PR / Issue / MR Reference Completion

(defun shipit-editor--resolve-pr-backend-for-repo (repo)
  "Resolve PR backend for REPO using stored editor context.
Uses `shipit-editor--pr-backend-id' and `shipit-editor--pr-backend-config'
which were captured from the source buffer in `shipit-editor-open'."
  (let* ((backend-id (or shipit-editor--pr-backend-id (shipit-pr--backend-id)))
         (backend-plist (or (cdr (assq backend-id shipit-pr-backends))
                            (error "No PR backend registered for `%s'" backend-id)))
         (base-config (or shipit-editor--pr-backend-config
                          shipit-pr-backend-config))
         (config (if base-config
                     (plist-put (copy-sequence base-config) :repo repo)
                   (list :repo repo))))
    ;; Auto-populate :project-path from repo slug when backend requests it
    (when (and (plist-get backend-plist :inject-project-path)
               (not (plist-get config :project-path))
               (not (plist-get config :project-id)))
      (setq config (plist-put config :project-path repo)))
    (cons backend-plist config)))

(defun shipit-editor-insert-pr-reference ()
  "Insert a PR reference at point.
Searches PRs via the PR backend, shows `completing-read', inserts #NUMBER.
Inserts literal # if cancelled or no PRs found."
  (interactive)
  (let ((repo shipit-editor--repo))
    (if (not repo)
        (insert "#")
      (condition-case nil
          (let* ((resolved (shipit-editor--resolve-pr-backend-for-repo repo))
                 (backend-plist (car resolved))
                 (config (cdr resolved))
                 (search-fn (plist-get backend-plist :search))
                 (prs (funcall search-fn config '("--state=open")))
                 (candidates (shipit-editor--format-pr-candidates prs))
                 (selected (completing-read "Insert PR reference: " candidates nil nil)))
            (if (and selected (not (string-empty-p selected)))
                (let ((pr-number (shipit-editor--extract-pr-number selected)))
                  (if pr-number
                      (progn
                        (insert (format "#%d" pr-number))
                        (message "Inserted #%d" pr-number))
                    (insert "#")))
              (insert "#")))
        (quit (insert "#"))))))

(defun shipit-editor--resolve-issue-backend-for-repo (repo)
  "Resolve the issue backend for REPO, preferring the PR backend.
When the PR backend has a matching issue backend registered, use it
with the stored PR backend config.  Otherwise fall back to the standard
issue backend resolution.  Uses `shipit-editor--pr-backend-id' and
`shipit-editor--pr-backend-config' which were captured from the source buffer."
  (let* ((pr-id (or shipit-editor--pr-backend-id (shipit-pr--backend-id)))
         (issue-plist (cdr (assq pr-id shipit-issue-backends))))
    (if issue-plist
        (let* ((base-config (or shipit-editor--pr-backend-config
                                shipit-pr-backend-config))
               (config (if base-config
                           (plist-put (copy-sequence base-config) :repo repo)
                         (list :repo repo))))
          (cons issue-plist config))
      (shipit-issue--resolve-for-repo repo))))

(defun shipit-editor-insert-issue-reference ()
  "Insert an issue reference at point.
Searches issues via the issue backend, shows `completing-read',
inserts #NUMBER.  Inserts literal # if cancelled or no issues found."
  (interactive)
  (let ((repo shipit-editor--repo))
    (if (not repo)
        (insert "#")
      (condition-case nil
          (let* ((resolved (shipit-editor--resolve-issue-backend-for-repo repo))
                 (backend-plist (car resolved))
                 (config (cdr resolved))
                 (search-fn (plist-get backend-plist :search))
                 (issues (funcall search-fn config '("--state=open")))
                 (candidates (shipit-editor--format-issue-candidates issues))
                 (selected (completing-read "Insert issue reference: " candidates nil nil)))
            (if (and selected (not (string-empty-p selected)))
                (let ((issue-number (shipit-editor--extract-pr-number selected)))
                  (if issue-number
                      (progn
                        (insert (format "#%d" issue-number))
                        (message "Inserted #%d" issue-number))
                    (insert "#")))
              (insert "#")))
        (quit (insert "#"))))))

(defun shipit-editor-insert-mr-reference ()
  "Insert a merge request reference at point (GitLab).
Searches MRs via the PR backend, shows `completing-read',
inserts !NUMBER.  Inserts literal ! if cancelled or no MRs found."
  (interactive)
  (let ((repo shipit-editor--repo))
    (if (not repo)
        (insert "!")
      (condition-case nil
          (let* ((resolved (shipit-editor--resolve-pr-backend-for-repo repo))
                 (backend-plist (car resolved))
                 (config (cdr resolved))
                 (search-fn (plist-get backend-plist :search))
                 (mrs (funcall search-fn config '(:state "opened")))
                 (candidates (shipit-editor--format-mr-candidates mrs))
                 (selected (completing-read "Insert MR reference: " candidates nil nil)))
            (if (and selected (not (string-empty-p selected)))
                (let ((mr-number (shipit-editor--extract-mr-number selected)))
                  (if mr-number
                      (progn
                        (insert (format "!%d" mr-number))
                        (message "Inserted !%d" mr-number))
                    (insert "!")))
              (insert "!")))
        (quit (insert "!"))))))

(defun shipit-editor--format-pr-candidates (prs)
  "Format PRS as completion candidates."
  (mapcar (lambda (pr)
            (let ((number (cdr (assq 'number pr)))
                  (title (cdr (assq 'title pr))))
              (format "#%-5d  %s" number title)))
          prs))

(defun shipit-editor--extract-pr-number (candidate)
  "Extract PR number from completion CANDIDATE."
  (when (string-match "#\\([0-9]+\\)" candidate)
    (string-to-number (match-string 1 candidate))))

(defun shipit-editor--get-pr-title (prs number)
  "Get title of PR with NUMBER from PRS list."
  (let ((pr (seq-find (lambda (p) (eq (cdr (assq 'number p)) number)) prs)))
    (when pr (cdr (assq 'title pr)))))

(defun shipit-editor--format-issue-candidates (issues)
  "Format ISSUES as completion candidates.
Each issue is an alist with `number' and `title' keys."
  (mapcar (lambda (issue)
            (let ((number (cdr (assq 'number issue)))
                  (title (cdr (assq 'title issue))))
              (format "#%-5d  %s" number title)))
          issues))

(defun shipit-editor--format-mr-candidates (mrs)
  "Format MRS as completion candidates.
Each MR is an alist with `number' and `title' keys."
  (mapcar (lambda (mr)
            (let ((number (cdr (assq 'number mr)))
                  (title (cdr (assq 'title mr))))
              (format "!%-5d  %s" number title)))
          mrs))

(defun shipit-editor--extract-mr-number (candidate)
  "Extract MR number from completion CANDIDATE."
  (when (string-match "!\\([0-9]+\\)" candidate)
    (string-to-number (match-string 1 candidate))))

;;; Emoji Shortcode Completion

(defvar shipit-editor--emoji-alist
  '(;; GitHub Reactions
    ("+1" . "👍")
    ("-1" . "👎")
    ("laugh" . "😄")
    ("confused" . "😕")
    ("heart" . "❤️")
    ("hooray" . "🎉")
    ("rocket" . "🚀")
    ("eyes" . "👀")
    ;; Faces - positive
    ("smile" . "😄")
    ("grinning" . "😀")
    ("joy" . "😂")
    ("smiley" . "😃")
    ("wink" . "😉")
    ("blush" . "😊")
    ("relaxed" . "☺️")
    ("yum" . "😋")
    ("relieved" . "😌")
    ("heart_eyes" . "😍")
    ("sunglasses" . "😎")
    ("smirk" . "😏")
    ("grin" . "😁")
    ("star_struck" . "🤩")
    ("partying_face" . "🥳")
    ;; Faces - negative/neutral
    ("disappointed" . "😞")
    ("worried" . "😟")
    ("angry" . "😠")
    ("cry" . "😢")
    ("sob" . "😭")
    ("fearful" . "😨")
    ("weary" . "😩")
    ("sleepy" . "😪")
    ("tired_face" . "😫")
    ("grimacing" . "😬")
    ("hushed" . "😯")
    ("flushed" . "😳")
    ("scream" . "😱")
    ("astonished" . "😲")
    ("sleeping" . "😴")
    ("thinking" . "🤔")
    ("face_with_raised_eyebrow" . "🤨")
    ("neutral_face" . "😐")
    ("expressionless" . "😑")
    ("unamused" . "😒")
    ("roll_eyes" . "🙄")
    ("face_with_rolling_eyes" . "🙄")
    ("pensive" . "😔")
    ("frowning" . "😦")
    ;; Gestures
    ("wave" . "👋")
    ("raised_hands" . "🙌")
    ("clap" . "👏")
    ("pray" . "🙏")
    ("handshake" . "🤝")
    ("muscle" . "💪")
    ("point_up" . "☝️")
    ("point_down" . "👇")
    ("point_left" . "👈")
    ("point_right" . "👉")
    ("ok_hand" . "👌")
    ("v" . "✌️")
    ("crossed_fingers" . "🤞")
    ("metal" . "🤘")
    ("call_me_hand" . "🤙")
    ;; Status/Development
    ("white_check_mark" . "✅")
    ("heavy_check_mark" . "✔️")
    ("ballot_box_with_check" . "☑️")
    ("x" . "❌")
    ("heavy_multiplication_x" . "✖️")
    ("warning" . "⚠️")
    ("no_entry" . "⛔")
    ("construction" . "🚧")
    ("rotating_light" . "🚨")
    ("sos" . "🆘")
    ("question" . "❓")
    ("exclamation" . "❗")
    ("bug" . "🐛")
    ("sparkles" . "✨")
    ("star" . "⭐")
    ("star2" . "🌟")
    ("fire" . "🔥")
    ("boom" . "💥")
    ("zap" . "⚡")
    ("bulb" . "💡")
    ("memo" . "📝")
    ("pencil" . "✏️")
    ("pencil2" . "✏️")
    ("hammer" . "🔨")
    ("wrench" . "🔧")
    ("gear" . "⚙️")
    ("link" . "🔗")
    ("lock" . "🔒")
    ("unlock" . "🔓")
    ("key" . "🔑")
    ("mag" . "🔍")
    ("mag_right" . "🔎")
    ;; Objects
    ("package" . "📦")
    ("gift" . "🎁")
    ("bookmark" . "🔖")
    ("books" . "📚")
    ("book" . "📖")
    ("clipboard" . "📋")
    ("pushpin" . "📌")
    ("paperclip" . "📎")
    ("triangular_ruler" . "📐")
    ("computer" . "💻")
    ("keyboard" . "⌨️")
    ("desktop_computer" . "🖥️")
    ("printer" . "🖨️")
    ("hourglass" . "⌛")
    ("stopwatch" . "⏱️")
    ("alarm_clock" . "⏰")
    ("coffee" . "☕")
    ("beer" . "🍺")
    ("beers" . "🍻")
    ("trophy" . "🏆")
    ("medal_sports" . "🏅")
    ("1st_place_medal" . "🥇")
    ("2nd_place_medal" . "🥈")
    ("3rd_place_medal" . "🥉")
    ;; Nature/Animals
    ("sunny" . "☀️")
    ("cloud" . "☁️")
    ("umbrella" . "☂️")
    ("snowflake" . "❄️")
    ("rainbow" . "🌈")
    ("ocean" . "🌊")
    ("seedling" . "🌱")
    ("evergreen_tree" . "🌲")
    ("deciduous_tree" . "🌳")
    ("cactus" . "🌵")
    ("fallen_leaf" . "🍂")
    ("leaves" . "🍃")
    ("turtle" . "🐢")
    ("rabbit" . "🐰")
    ("cat" . "🐱")
    ("dog" . "🐶")
    ("bear" . "🐻")
    ("panda_face" . "🐼")
    ("penguin" . "🐧")
    ("chicken" . "🐔")
    ("hatching_chick" . "🐣")
    ("snake" . "🐍")
    ("whale" . "🐳")
    ("dolphin" . "🐬")
    ("octopus" . "🐙")
    ("bee" . "🐝")
    ("butterfly" . "🦋")
    ;; Symbols/Arrows
    ("arrow_up" . "⬆️")
    ("arrow_down" . "⬇️")
    ("arrow_left" . "⬅️")
    ("arrow_right" . "➡️")
    ("arrow_upper_left" . "↖️")
    ("arrow_upper_right" . "↗️")
    ("arrow_lower_left" . "↙️")
    ("arrow_lower_right" . "↘️")
    ("arrows_clockwise" . "🔃")
    ("arrows_counterclockwise" . "🔄")
    ("leftwards_arrow_with_hook" . "↩️")
    ("arrow_right_hook" . "↪️")
    ("information_source" . "ℹ️")
    ("copyright" . "©️")
    ("registered" . "®️")
    ("tm" . "™️")
    ;; Numbers/Letters
    ("one" . "1️⃣")
    ("two" . "2️⃣")
    ("three" . "3️⃣")
    ("four" . "4️⃣")
    ("five" . "5️⃣")
    ("six" . "6️⃣")
    ("seven" . "7️⃣")
    ("eight" . "8️⃣")
    ("nine" . "9️⃣")
    ("zero" . "0️⃣")
    ("hash" . "#️⃣")
    ("asterisk" . "*️⃣")
    ("a" . "🅰️")
    ("b" . "🅱️")
    ("ab" . "🆎")
    ("o2" . "🅾️")
    ;; Misc
    ("100" . "💯")
    ("tada" . "🎉")
    ("confetti_ball" . "🎊")
    ("balloon" . "🎈")
    ("ribbon" . "🎀")
    ("art" . "🎨")
    ("performing_arts" . "🎭")
    ("musical_note" . "🎵")
    ("headphones" . "🎧")
    ("movie_camera" . "🎥")
    ("camera" . "📷")
    ("video_camera" . "📹")
    ("tv" . "📺")
    ("radio" . "📻")
    ("iphone" . "📱")
    ("telephone" . "☎️")
    ("email" . "📧")
    ("mailbox" . "📫")
    ("newspaper" . "📰")
    ("scroll" . "📜")
    ("page_facing_up" . "📄")
    ("page_with_curl" . "📃")
    ("file_folder" . "📁")
    ("open_file_folder" . "📂")
    ("wastebasket" . "🗑️")
    ("bell" . "🔔")
    ("no_bell" . "🔕")
    ("speech_balloon" . "💬")
    ("thought_balloon" . "💭")
    ("zzz" . "💤"))
  "Alist mapping GitHub emoji shortcodes to Unicode characters.")

(defun shipit-editor-insert-emoji ()
  "Insert an emoji shortcode at point.
Shows completion with available emojis, inserts :shortcode: on selection.
Inserts literal : if cancelled."
  (interactive)
  (condition-case nil
      (let* ((candidates (shipit-editor--format-emoji-candidates))
             (selected (completing-read "Insert emoji: " candidates nil t)))
        (if (and selected (not (string-empty-p selected)))
            (let ((shortcode (shipit-editor--extract-emoji-shortcode selected)))
              (insert (format ":%s:" shortcode))
              (message "Inserted :%s:" shortcode))
          (insert ":")))
    (quit (insert ":"))))

(defun shipit-editor--format-emoji-candidates ()
  "Format emoji alist as completion candidates."
  (mapcar (lambda (pair)
            (format "%s %s" (cdr pair) (car pair)))
          shipit-editor--emoji-alist))

(defun shipit-editor--extract-emoji-shortcode (candidate)
  "Extract shortcode from emoji completion CANDIDATE."
  (when (string-match "^.+? \\(.+\\)$" candidate)
    (match-string 1 candidate)))

(defun shipit-editor--strip-variation-selectors (str)
  "Remove Unicode variation selectors from STR.
Variation selectors (U+FE0E text, U+FE0F emoji) can display as
white boxes in some contexts."
  (replace-regexp-in-string "[\uFE0E\uFE0F]" "" str))

(defun shipit-editor--render-emoji-shortcodes (text)
  "Replace :shortcode: patterns in TEXT with Unicode emoji."
  (let ((result text))
    (dolist (pair shipit-editor--emoji-alist)
      (let ((shortcode (car pair))
            (emoji (shipit-editor--strip-variation-selectors (cdr pair))))
        (setq result (replace-regexp-in-string
                      (format ":%s:" (regexp-quote shortcode))
                      emoji
                      result t t))))
    result))

;;; User Mention Completion

(declare-function shipit--get-available-assignees "shipit-pr-actions")

(defun shipit-editor-insert-user-mention ()
  "Insert a user mention at point.
Shows completion with available users, inserts @username on selection.
Inserts literal @ if cancelled or no users found."
  (interactive)
  (let ((repo shipit-editor--repo))
    (if (not repo)
        (insert "@")
      (condition-case nil
          (let ((users (shipit--get-available-assignees repo)))
            (if (and users (> (length users) 0))
                (let ((selected (completing-read "Mention user: " users nil t)))
                  (if (and selected (not (string-empty-p selected)))
                      (progn
                        (insert (format "@%s" selected))
                        (message "Mentioned @%s" selected))
                    (insert "@")))
              (insert "@")
              (message "No users found in %s" repo)))
        (quit (insert "@"))))))

;;; Save and Cancel

(defun shipit-editor-save ()
  "Save the editor content and close."
  (interactive)
  (let ((content (buffer-string))
        (type shipit-editor--type)
        (pr-number shipit-editor--pr-number)
        (repo shipit-editor--repo)
        (file-path shipit-editor--file-path)
        (line-number shipit-editor--line-number)
        (old-line-number shipit-editor--old-line-number)
        (side (or shipit-editor--side "RIGHT"))
        (comment-id shipit-editor--comment-id)
        (parent-comment-id shipit-editor--parent-comment-id)
        (review-event shipit-editor--review-event)
        (on-save shipit-editor--on-save)
        (source-buffer shipit-editor--source-buffer))
    ;; Check for empty content (reviews and preview-description allow empty)
    (when (and (string-empty-p (string-trim content))
               (not (memq type '(review preview-description issue-description))))
      (user-error "Cannot save empty content"))
    ;; For REQUEST_CHANGES, body is required
    (when (and (eq type 'review)
               (equal review-event "REQUEST_CHANGES")
               (string-empty-p (string-trim content)))
      (user-error "Review body required when requesting changes"))
    ;; Call appropriate API or callback based on type
    (pcase type
      ('preview-description
       ;; For preview, call the on-save callback in source buffer context
       ;; (buffer-local variables like shipit-preview--description live there)
       (when (and on-save (buffer-live-p source-buffer))
         (with-current-buffer source-buffer
           (funcall on-save content))))
      ('preview-general-comment
       ;; For preview general comments, call callback in source buffer context
       ;; (buffer-local variables like shipit-preview--general-comments live there)
       (when (and on-save (buffer-live-p source-buffer))
         (with-current-buffer source-buffer
           (funcall on-save content))))
      ('preview-inline-comment
       ;; For preview inline comments, call callback in source buffer context
       ;; (buffer-local variables like shipit-preview--comments live there)
       (when (and on-save (buffer-live-p source-buffer))
         (with-current-buffer source-buffer
           (funcall on-save content))))
      ('preview-file-comment
       ;; For preview file comments, call callback in source buffer context
       ;; (buffer-local variables like shipit-preview--comments live there)
       (when (and on-save (buffer-live-p source-buffer))
         (with-current-buffer source-buffer
           (funcall on-save content))))
      ('description
       ;; Run in source buffer so backend resolves correctly (buffer-local shipit-pr-backend)
       (if (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (shipit--edit-pr-description pr-number content))
         (shipit--edit-pr-description pr-number content)))
      ('general-comment
       ;; Run in source buffer so backend resolves correctly (buffer-local shipit-pr-backend)
       (if (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (if comment-id
                 (shipit--edit-comment comment-id content nil nil)
               ;; Skip refresh since live preview already shows the content
               (shipit--add-general-comment-to-pr pr-number content t)))
         (if comment-id
             (shipit--edit-comment comment-id content nil nil)
           (shipit--add-general-comment-to-pr pr-number content t))))
      ('inline-comment
       ;; Run in source buffer so backend resolves correctly (buffer-local shipit-pr-backend)
       (if (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (if comment-id
                 (shipit--edit-comment comment-id content t nil)
               (shipit--add-comment-to-pr pr-number file-path line-number content side nil old-line-number)))
         (if comment-id
             (shipit--edit-comment comment-id content t nil)
           (shipit--add-comment-to-pr pr-number file-path line-number content side nil old-line-number))))
      ('reply
       ;; Reply to parent comment - content is already the user's reply text
       ;; (quoted parent is NOT included - user writes just their reply)
       ;; Must run in source-buffer context so cache operations work (buffer-local)
       (when (buffer-live-p source-buffer)
         (if file-path
             ;; Inline comment reply - run in source buffer for cache access
             (with-current-buffer source-buffer
               (shipit--reply-to-inline-comment pr-number parent-comment-id content file-path))
           ;; General comment reply - insert as proper magit child section
           (let ((new-comment (with-current-buffer source-buffer
                                (shipit--reply-to-general-comment pr-number parent-comment-id content t))))
             (when (and new-comment shipit-editor--preview-overlay)
               (let ((overlay shipit-editor--preview-overlay))
                 ;; Delete the preview overlay content first
                 (when (overlay-buffer overlay)
                   (with-current-buffer (overlay-buffer overlay)
                     (let ((inhibit-read-only t))
                       (delete-region (overlay-start overlay) (overlay-end overlay)))))
                 (delete-overlay overlay)
                 (setq shipit-editor--preview-overlay nil))
               ;; Insert reply as proper magit section child of parent
               (with-current-buffer source-buffer
                 (shipit--insert-reply-as-child parent-comment-id new-comment repo pr-number)))))))
      ('create-issue
       (let* ((lines (split-string content "\n"))
              (title (string-trim (car lines)))
              (rest (cdr lines))
              (body-start (cl-position-if
                           (lambda (l) (not (string-empty-p (string-trim l))))
                           rest))
              (body (if body-start
                        (string-join (nthcdr body-start rest) "\n")
                      "")))
         (when (string-empty-p title)
           (user-error "Issue title cannot be empty"))
         (let ((created (shipit-issues--create-issue repo title body)))
           (shipit-issues-open-buffer
            (cdr (assq 'number created)) repo)
           (message "Created issue %s"
                    (cdr (or (assq 'number created) (assq 'id created)))))))
      ('issue-comment
       (shipit-issues--add-comment repo pr-number content)
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           (shipit-issue-buffer-refresh)))
       (message "Comment added"))
      ('issue-comment-edit
       (shipit-issues--edit-comment repo comment-id content)
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           (shipit-issue-buffer-refresh)))
       (message "Comment updated"))
      ('issue-description
       (shipit-issues--update-description repo pr-number content)
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           (shipit-issue-buffer-refresh)))
       (message "Issue description updated"))
      ('discussion-comment
       (let ((discussion-id shipit-editor--discussion-id))
         (shipit-discussion--add-comment discussion-id content)
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (shipit-discussion-buffer-refresh)))
         (message "Discussion comment added")))
      ('discussion-reply
       (let ((discussion-id shipit-editor--discussion-id)
             (reply-to-id parent-comment-id))
         (shipit-discussion--reply-to-comment discussion-id reply-to-id content)
         (when (buffer-live-p source-buffer)
           (with-current-buffer source-buffer
             (shipit-discussion-buffer-refresh)))
         (message "Discussion reply added")))
      ('create-discussion
       (let* ((lines (split-string content "\n"))
              (title (string-trim (car lines)))
              (rest (cdr lines))
              (body-start (cl-position-if
                           (lambda (l) (not (string-empty-p (string-trim l))))
                           rest))
              (body (if body-start
                        (string-join (nthcdr body-start rest) "\n")
                      "")))
         (when (string-empty-p title)
           (user-error "Discussion title cannot be empty"))
         (let* ((categories (shipit-discussion--fetch-categories repo))
                (cat-names (mapcar (lambda (c)
                                     (let ((name (cdr (assq 'name c)))
                                           (emoji (or (cdr (assq 'emoji c)) "")))
                                       (format "%s %s" emoji name)))
                                   categories))
                (selected-cat (completing-read "Category: " cat-names nil t))
                (cat-name (if (string-match "\\`.+? \\(.+\\)\\'" selected-cat)
                              (match-string 1 selected-cat)
                            selected-cat))
                (category (cl-find-if
                           (lambda (c)
                             (string= (cdr (assq 'name c)) cat-name))
                           categories))
                (category-id (cdr (assq 'id category))))
           (unless category-id
             (user-error "Could not find category: %s" cat-name))
           (let ((created (shipit-discussion--create repo title body category-id)))
             (shipit-discussions-open-buffer
              (cdr (assq 'number created)) repo)
             (message "Created discussion #%s"
                      (cdr (assq 'number created)))))))
      ('review
       (let ((body (if (string-empty-p (string-trim content)) nil content)))
         (shipit-post-review pr-number review-event body nil repo)))
      ('close-pr-comment
       ;; Close PR with the comment - ask for confirmation first
       (when (yes-or-no-p (format "Close PR #%d with this comment? " pr-number))
         (let ((comment (if (string-empty-p (string-trim content)) nil content)))
           (shipit--close-pr pr-number repo comment)
           ;; Refresh source buffer since shipit--close-pr won't do it from editor
           (when (buffer-live-p source-buffer)
             (with-current-buffer source-buffer
               (when (fboundp 'shipit-buffer-refresh)
                 (shipit-buffer-refresh))))))))
    ;; Close the editor (don't restore - we're saving)
    (shipit-editor--close nil)
    ;; Refresh source buffer if needed (skip for preview types, callback handles it)
    ;; Also skip for general-comment, reply, close-pr-comment since they handle their own refresh
    (when (and (buffer-live-p source-buffer)
               (not (memq type '(preview-description preview-general-comment
                                 preview-inline-comment preview-file-comment
                                 general-comment reply close-pr-comment
                                 create-issue issue-comment issue-comment-edit
                                 issue-description
                                 discussion-comment discussion-reply
                                 create-discussion))))  ; Skip refresh - API functions handle it
      (with-current-buffer source-buffer
        (cond
         ;; For inline comments, use targeted file section refresh
         ((and (memq type '(inline-comment))
               file-path
               pr-number
               repo
               (fboundp 'shipit--refresh-file-inline-comment-section))
          (shipit--refresh-file-inline-comment-section file-path pr-number repo))
         ;; Fallback to full refresh for other types (description, review, etc.)
         (t
          (when (fboundp 'shipit-buffer-refresh)
            (shipit-buffer-refresh))))))
    (message "Saved")))

(defun shipit-editor-cancel ()
  "Cancel editing and close the editor."
  (interactive)
  ;; Restore original content on cancel
  (shipit-editor--close t)
  (message "Cancelled"))

(defun shipit-editor--commit-insert-active-p ()
  "Return non-nil if the last commit insertion region is still valid.
The region is valid if markers exist and the last command was a commit insert."
  (and shipit-editor--commit-insert-start
       shipit-editor--commit-insert-end
       (marker-position shipit-editor--commit-insert-start)
       (marker-position shipit-editor--commit-insert-end)
       (memq last-command '(shipit-editor-insert-next-commit-message
                            shipit-editor-insert-prev-commit-message))))

(defun shipit-editor--commit-insert-clear ()
  "Clear the commit insertion markers."
  (when shipit-editor--commit-insert-start
    (set-marker shipit-editor--commit-insert-start nil))
  (when shipit-editor--commit-insert-end
    (set-marker shipit-editor--commit-insert-end nil))
  (setq shipit-editor--commit-insert-start nil
        shipit-editor--commit-insert-end nil))

(defun shipit-editor--insert-commit-message (new-index)
  "Insert commit message at NEW-INDEX, replacing previous if cycling.
Returns the commit plist that was inserted."
  (let* ((commit (nth new-index shipit-editor--commit-messages))
         (message-text (plist-get commit :message)))
    ;; If we have an active insertion region, replace it
    (if (shipit-editor--commit-insert-active-p)
        (progn
          (delete-region (marker-position shipit-editor--commit-insert-start)
                         (marker-position shipit-editor--commit-insert-end))
          (goto-char (marker-position shipit-editor--commit-insert-start)))
      ;; Otherwise, clear any stale markers and start fresh
      (shipit-editor--commit-insert-clear)
      (setq shipit-editor--commit-insert-start (point-marker)))
    ;; Insert the new message
    (insert message-text)
    ;; Update end marker
    (if shipit-editor--commit-insert-end
        (set-marker shipit-editor--commit-insert-end (point))
      (setq shipit-editor--commit-insert-end (point-marker)))
    ;; Mark the inserted region so it's visually selected
    ;; Use set-mark and prevent deactivation at end of command
    (set-mark (marker-position shipit-editor--commit-insert-start))
    (setq deactivate-mark nil)
    ;; Update index
    (setq shipit-editor--commit-index new-index)
    commit))

(defun shipit-editor-insert-next-commit-message ()
  "Insert the next commit message at point, replacing if cycling.
Cycles through available commit messages from the branch being prepared as PR.
If called repeatedly, replaces the previously inserted message.
Moving point or editing elsewhere deactivates replacement mode."
  (interactive)
  (if (null shipit-editor--commit-messages)
      (message "No commit messages available")
    (let* ((len (length shipit-editor--commit-messages))
           (new-index (mod (1+ shipit-editor--commit-index) len))
           (commit (shipit-editor--insert-commit-message new-index)))
      (message "Commit %d/%d: %s"
               (1+ new-index) len
               (plist-get commit :short-sha)))))

(defun shipit-editor-insert-prev-commit-message ()
  "Insert the previous commit message at point, replacing if cycling.
Cycles through available commit messages from the branch being prepared as PR.
If called repeatedly, replaces the previously inserted message.
Moving point or editing elsewhere deactivates replacement mode."
  (interactive)
  (if (null shipit-editor--commit-messages)
      (message "No commit messages available")
    (let* ((len (length shipit-editor--commit-messages))
           (new-index (if (and (< shipit-editor--commit-index 0)
                               (not (shipit-editor--commit-insert-active-p)))
                          (1- len)  ; Start at last commit
                        (mod (1- shipit-editor--commit-index) len)))
           (commit (shipit-editor--insert-commit-message new-index)))
      (message "Commit %d/%d: %s"
               (1+ new-index) len
               (plist-get commit :short-sha)))))

(defun shipit-editor--close (&optional restore-original)
  "Close the editor buffer and window.
If RESTORE-ORIGINAL is non-nil, restore the original preview content."
  (shipit--debug-log "EDITOR: close called - restore=%s overlay=%s original-content-len=%s type=%s"
                     restore-original
                     (if shipit-editor--preview-overlay "YES" "NO")
                     (when shipit-editor--original-preview-content
                       (length shipit-editor--original-preview-content))
                     shipit-editor--type)
  ;; Cancel any pending preview timer
  (when (timerp shipit-editor--preview-timer)
    (cancel-timer shipit-editor--preview-timer))
  ;; Clean up preview overlay
  ;; - For preview types on save: just delete overlay (callback's refresh replaced content)
  ;; - If restoring: restore original content
  ;; - If saving general-comment or reply: keep content in place (no refresh needed)
  ;; - Otherwise: delete the draft section
  (when shipit-editor--preview-overlay
    (let* ((is-preview-type (memq shipit-editor--type
                                  '(preview-inline-comment preview-file-comment
                                    preview-general-comment)))
           (skip-buffer-modification (and (not restore-original) is-preview-type)))
      (if skip-buffer-modification
          ;; For preview types on save, just delete overlay - callback already refreshed the buffer
          (delete-overlay shipit-editor--preview-overlay)
        ;; For other types, use full cleanup
        (let ((keep-content (and (not restore-original)
                                 (memq shipit-editor--type '(general-comment reply)))))
          (shipit-editor--cleanup-preview-overlay
           shipit-editor--preview-overlay
           (when restore-original shipit-editor--original-preview-content)
           keep-content)))))
  ;; Kill the buffer (which also closes the window)
  (let ((buffer (get-buffer shipit-editor--buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (when window
          (delete-window window)))
      (kill-buffer buffer))))

(provide 'shipit-editor)
;;; shipit-editor.el ends here
