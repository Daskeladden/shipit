;;; shipit-preview.el --- Preview PR before creation -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 shipit contributors

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
;; Preview what a PR would look like before creating it.

;;; Code:

(require 'shipit-core)
(require 'shipit-render)
(require 'shipit-pr-sections)
(require 'shipit-buffer)
(require 'shipit-pr-backends)
(require 'shipit-comment-backends)
(require 'magit-section)

;; Filter variables from shipit-pr-sections (buffer-local)
(defvar shipit--files-filter-text)
(defvar shipit--files-filter-mode)

;; Inline comments cache from shipit-cache (buffer-local)
(defvar shipit--cached-inline-comments)
(defvar shipit--inline-comments-fetched)

;; Forward declarations from shipit-http
(declare-function shipit--get-current-user "shipit-pr-actions")

;; Buffer-local state
(defvar-local shipit-preview--repo nil "Repository owner/name.")
(defvar-local shipit-preview--branch nil "Current branch name.")
(defvar-local shipit-preview--base-ref nil "Base ref for comparison.")
(defvar-local shipit-preview--title nil "Draft PR title.")
(defvar-local shipit-preview--description nil "Draft PR description.")
(defvar-local shipit-preview--cached-files nil "Cached files for filtering.")
(defvar-local shipit-preview--labels nil "List of selected label names.")
(defvar-local shipit-preview--cached-available-labels nil "Cached available labels from repo.")
(defvar-local shipit-preview--reviewers nil "List of requested reviewer usernames.")
(defvar-local shipit-preview--assignees nil "List of assignee usernames.")
(defvar-local shipit-preview--draft nil "Whether to create as draft PR.")
(defvar-local shipit-preview--comments nil
  "List of local inline comments for the preview PR.
Each comment is a plist with keys:
  :file - file path
  :line - line number in the diff
  :side - LEFT or RIGHT
  :body - comment text
  :id - unique identifier (timestamp-based)")

(defvar-local shipit-preview--general-comments nil
  "List of general comments for the preview PR.
Each comment is a plist with keys:
  :body - comment text
  :id - unique identifier (timestamp-based)")

;; Global state persistence for preview mode
(defvar shipit-preview--saved-state (make-hash-table :test 'equal)
  "Hash table storing preview state per repo:branch.
Key format: \"repo:branch\", value is a plist of saved state.")

(defun shipit-preview--state-key ()
  "Generate cache key for current preview state."
  (format "%s:%s" shipit-preview--repo shipit-preview--branch))

(defun shipit-preview--save-state ()
  "Save current preview state for later restoration."
  (when (and (bound-and-true-p shipit-preview--repo)
             (bound-and-true-p shipit-preview--branch))
    (let* ((key (shipit-preview--state-key))
           (state (list :title shipit-preview--title
                        :description shipit-preview--description
                        :labels shipit-preview--labels
                        :reviewers shipit-preview--reviewers
                        :assignees shipit-preview--assignees
                        :draft shipit-preview--draft
                        :comments shipit-preview--comments
                        :general-comments shipit-preview--general-comments)))
      (puthash key state shipit-preview--saved-state)
      (shipit--debug-log "PREVIEW: Saved state for %s" key))))

(defun shipit-preview--restore-state ()
  "Restore previously saved preview state if available."
  (let* ((key (shipit-preview--state-key))
         (state (gethash key shipit-preview--saved-state)))
    (shipit--debug-log "PREVIEW: Attempting restore for %s, found=%s" key (if state "yes" "no"))
    (when state
      (setq shipit-preview--title (plist-get state :title)
            shipit-preview--description (plist-get state :description)
            shipit-preview--labels (plist-get state :labels)
            shipit-preview--reviewers (plist-get state :reviewers)
            shipit-preview--assignees (plist-get state :assignees)
            shipit-preview--draft (plist-get state :draft)
            shipit-preview--comments (plist-get state :comments)
            shipit-preview--general-comments (plist-get state :general-comments))
      (shipit--debug-log "PREVIEW: Restored state - title=%s labels=%s"
                         shipit-preview--title shipit-preview--labels)
      t)))

(defun shipit-preview--clear-saved-state ()
  "Clear saved state for current preview (called after PR creation)."
  (remhash (shipit-preview--state-key) shipit-preview--saved-state))

(defun shipit-preview--quit ()
  "Quit preview buffer, saving state for later restoration."
  (interactive)
  (shipit-preview--save-state)
  (quit-window))

(defvar shipit-preview-mode-map nil
  "Keymap for `shipit-preview-mode'.")

(unless shipit-preview-mode-map
  (setq shipit-preview-mode-map (make-sparse-keymap))
  (set-keymap-parent shipit-preview-mode-map magit-section-mode-map))

;; Always ensure bindings are set (survives reload)
(define-key shipit-preview-mode-map (kbd "f") #'shipit-files-filter)
(define-key shipit-preview-mode-map (kbd "g") #'shipit-preview--refresh)
(define-key shipit-preview-mode-map (kbd "RET") #'shipit-preview--dwim)
(define-key shipit-preview-mode-map (kbd "C-c C-c") #'shipit-preview--create-pr)
(define-key shipit-preview-mode-map (kbd "M-;") #'shipit-dwim)
(define-key shipit-preview-mode-map (kbd "q") #'shipit-preview--quit)

(define-derived-mode shipit-preview-mode magit-section-mode "Shipit-Preview"
  "Major mode for previewing a PR before creation.

\\{shipit-preview-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-preview--refresh)
  (shipit--apply-section-defaults)
  ;; Initialize filter variables
  (setq-local shipit--files-filter-text "")
  (setq-local shipit--files-filter-mode nil)
  ;; Save state when buffer is killed
  (add-hook 'kill-buffer-hook #'shipit-preview--save-state nil t))

;;; Git data extraction

(defun shipit-preview--get-commits ()
  "Get commits between base ref and HEAD as processed plists.
Returns list in chronological order (oldest first), compatible with
`shipit--render-commits-section'."
  (let* ((range (format "%s..HEAD" shipit-preview--base-ref))
         ;; Use %B for full body, %x1e as record separator (since body can have newlines)
         (format-str "%H%x00%B%x00%an%x00%ae%x00%aI%x1e")
         ;; Use --reverse to get oldest-first order directly
         (output (shell-command-to-string
                  (format "git log --reverse --format='%s' %s 2>/dev/null" format-str range)))
         ;; Split by record separator to get individual commits
         (records (split-string output "\x1e" t))
         (commits nil))
    (dolist (record records)
      (let* ((trimmed-record (string-trim record))
             (parts (split-string trimmed-record "\x00"))
             (sha (nth 0 parts))
             (full-message (string-trim (or (nth 1 parts) "")))
             (author (nth 2 parts))
             (author-email (nth 3 parts))
             (author-date (nth 4 parts))
             ;; Extract first line for display purposes
             (first-line (car (split-string full-message "\n" t))))
        ;; Skip empty records (e.g., trailing newline after last separator)
        (when (and sha (not (string-empty-p sha)))
          (push (list :sha sha
                      :short-sha (substring sha 0 (min 7 (length sha)))
                      :message full-message
                      :first-line (or first-line "")
                      :author author
                      :author-email author-email
                      :author-date author-date
                      :author-github-username nil
                      :author-avatar-url nil
                      :additions 0
                      :deletions 0
                      :files-changed 0)
                commits))))
    ;; With --reverse, git outputs oldest-first, and push reverses it,
    ;; so nreverse gives us oldest-first again
    (nreverse commits)))

(defun shipit-preview--get-file-patch (filename)
  "Get the diff patch for FILENAME between base ref and HEAD."
  (let ((range (format "%s..HEAD" shipit-preview--base-ref)))
    (shell-command-to-string
     (format "git diff %s -- %s 2>/dev/null" range (shell-quote-argument filename)))))

(defun shipit-preview--get-files ()
  "Get files changed between base ref and HEAD as alists.
Returns list compatible with `shipit--render-files-section'."
  (let* ((range (format "%s..HEAD" shipit-preview--base-ref))
         (output (shell-command-to-string
                  (format "git diff --numstat %s 2>/dev/null" range)))
         (lines (split-string output "\n" t))
         (files nil))
    (dolist (line lines)
      (let* ((parts (split-string line "\t"))
             (additions (string-to-number (or (nth 0 parts) "0")))
             (deletions (string-to-number (or (nth 1 parts) "0")))
             (filename (nth 2 parts)))
        (when filename
          (push `((filename . ,filename)
                  (status . "modified")
                  (additions . ,additions)
                  (deletions . ,deletions)
                  (patch . ,(shipit-preview--get-file-patch filename)))
                files))))
    (nreverse files)))

;;; Buffer rendering

(defun shipit-preview--backend-icon ()
  "Return (icon-string . backend-name) for the current PR backend."
  (let* ((backend (shipit-pr--get-backend))
         (backend-name (or (plist-get backend :name) "Unknown"))
         (icon-key (downcase backend-name))
         (emoji-fallback (or (plist-get backend :emoji-fallback) "🔧"))
         (icon (shipit--get-pr-field-icon icon-key emoji-fallback)))
    (cons icon backend-name)))

(defun shipit-preview--insert-header ()
  "Insert the preview header banner."
  (let* ((icon-and-name (shipit-preview--backend-icon))
         (icon (car icon-and-name))
         (backend-name (cdr icon-and-name))
         (pr-label (or (plist-get (shipit-pr--get-backend) :pr-type-short-label) "PR")))
    (magit-insert-section (preview-header)
      (insert (propertize (format "PREVIEW — %s %s %s %s — Not yet created\n"
                                  (shipit--get-notification-source-icon (shipit-pr--backend-id))
                                  icon backend-name pr-label)
                          'font-lock-face '(:foreground "orange" :weight bold)))
      (insert (propertize (format "   %s  |  C-c C-c to create %s\n"
                                  shipit-preview--repo pr-label)
                          'font-lock-face 'font-lock-comment-face))
      (insert "\n"))))

(defun shipit-preview--edit-title ()
  "Edit the PR title interactively."
  (interactive)
  (let ((new-title (read-string "PR Title: "
                                (or shipit-preview--title shipit-preview--branch))))
    (when (and new-title (not (string-empty-p new-title)))
      (setq shipit-preview--title new-title)
      (shipit-preview--refresh))))

(defun shipit-preview--edit-description ()
  "Edit the PR description interactively using shipit-editor with live preview."
  (interactive)
  (let ((current-desc (or shipit-preview--description ""))
        (commits (shipit-preview--get-commits))
        (preview-buffer (current-buffer)))
    (if (fboundp 'shipit-editor-open)
        (shipit-editor-open
         (list :type 'preview-description
               :source-buffer preview-buffer
               :initial-content current-desc
               :commit-messages commits
               ;; Create closure capturing the preview buffer
               :on-save (lambda (content)
                          (shipit-preview--on-description-saved content preview-buffer))))
      ;; Fallback to simple editor if shipit-editor not loaded
      (shipit-preview--edit-description-simple current-desc))))

(defun shipit-preview--on-description-saved (content preview-buffer)
  "Callback when preview description is saved.
CONTENT is the new description text.
PREVIEW-BUFFER is the buffer to update."
  (when (buffer-live-p preview-buffer)
    (with-current-buffer preview-buffer
      (setq shipit-preview--description content)
      (shipit-preview--refresh))))

(defun shipit-preview--edit-description-simple (current-desc)
  "Simple fallback editor for preview description.
CURRENT-DESC is the current description text."
  (let* ((buf (get-buffer-create "*shipit-preview-description*"))
         (preview-buf (current-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (insert current-desc)
      (goto-char (point-min))
      (markdown-mode)
      (setq-local shipit-preview--source-buffer preview-buf)
      (setq-local header-line-format
                  (propertize " Edit PR Description - C-c C-c to save, C-c C-k to cancel "
                              'font-lock-face 'mode-line-highlight))
      (local-set-key (kbd "C-c C-c") #'shipit-preview--save-description)
      (local-set-key (kbd "C-c C-k") #'shipit-preview--cancel-description))
    (pop-to-buffer buf)))

(defun shipit-preview--save-description ()
  "Save the description and close the edit buffer."
  (interactive)
  (let ((desc (buffer-string))
        (source-buf shipit-preview--source-buffer))
    (quit-window t)
    (when (buffer-live-p source-buf)
      (with-current-buffer source-buf
        (setq shipit-preview--description desc)
        (shipit-preview--refresh)))))

(defun shipit-preview--cancel-description ()
  "Cancel editing and close the edit buffer."
  (interactive)
  (quit-window t))

(defun shipit-preview--manage-reviewers ()
  "Manage reviewers using action menu matching normal shipit buffer behavior."
  (let* ((current-usernames shipit-preview--reviewers)
         (action (completing-read "Reviewer action: "
                                  '("Add reviewer" "Remove reviewer")
                                  nil t)))
    (cond
     ((string= action "Add reviewer")
      ;; Use same endpoint as assignees - collaborators can be reviewers
      (let* ((available-users (shipit--get-available-assignees shipit-preview--repo))
             (unrequested-users (seq-filter (lambda (user)
                                              (not (member user current-usernames)))
                                            available-users)))
        (if unrequested-users
            (let ((username (completing-read "Request review from: " unrequested-users nil t)))
              (push username shipit-preview--reviewers)
              (shipit-preview--refresh)
              (message "Added %s to reviewers" username))
          (message "All available users are already requested"))))
     ((string= action "Remove reviewer")
      (if current-usernames
          (let ((username (completing-read "Remove reviewer: " current-usernames nil t)))
            (setq shipit-preview--reviewers (delete username shipit-preview--reviewers))
            (shipit-preview--refresh)
            (message "Removed %s from reviewers" username))
        (message "No reviewers to remove")))
     (t (message "No action selected")))))

(defun shipit-preview--edit-reviewers ()
  "Edit the list of reviewers using the action menu."
  (interactive)
  (shipit-preview--manage-reviewers))

(defun shipit-preview--manage-assignees ()
  "Manage assignees using action menu matching normal shipit buffer behavior."
  (let* ((current-usernames shipit-preview--assignees)
         (action (completing-read "Assignee action: "
                                  '("Add assignee" "Remove assignee" "Assign yourself")
                                  nil t)))
    (cond
     ((string= action "Add assignee")
      (let* ((available-users (shipit--get-available-assignees shipit-preview--repo))
             (unassigned-users (seq-filter (lambda (user)
                                             (not (member user current-usernames)))
                                           available-users)))
        (if unassigned-users
            (let ((username (completing-read "Assign user: " unassigned-users nil t)))
              (push username shipit-preview--assignees)
              (shipit-preview--refresh)
              (message "Added %s to assignees" username))
          (message "All available users are already assigned"))))
     ((string= action "Remove assignee")
      (if current-usernames
          (let ((username (completing-read "Remove assignee: " current-usernames nil t)))
            (setq shipit-preview--assignees (delete username shipit-preview--assignees))
            (shipit-preview--refresh)
            (message "Removed %s from assignees" username))
        (message "No assignees to remove")))
     ((string= action "Assign yourself")
      (let ((current-user (shipit--get-current-user)))
        (if current-user
            (if (member current-user current-usernames)
                (message "You are already assigned to this PR")
              (push current-user shipit-preview--assignees)
              (shipit-preview--refresh)
              (message "Added yourself (%s) to assignees" current-user))
          (user-error "Could not determine current user"))))
     (t (message "No action selected")))))

(defun shipit-preview--edit-assignees ()
  "Edit the list of assignees using the action menu."
  (interactive)
  (shipit-preview--manage-assignees))

(defun shipit-preview--select-labels ()
  "Select labels for the preview PR."
  (interactive)
  (require 'shipit-diff)
  ;; Fetch available labels if not cached
  (unless shipit-preview--cached-available-labels
    (message "Fetching available labels...")
    (setq shipit-preview--cached-available-labels
          (shipit--get-available-labels shipit-preview--repo)))
  (if shipit-preview--cached-available-labels
      (let* ((current-labels (or shipit-preview--labels '()))
             ;; Create label choices with • for selected
             (all-choices (mapcar (lambda (label)
                                    (let* ((name (cdr (assq 'name label)))
                                           (is-selected (member name current-labels)))
                                      (cons (format "%s%s"
                                                    (if is-selected "• " "  ")
                                                    name)
                                            name)))
                                  shipit-preview--cached-available-labels))
             ;; Sort with selected first
             (sorted-choices (sort all-choices
                                   (lambda (a b)
                                     (let ((choice-a (car a))
                                           (choice-b (car b)))
                                       (cond
                                        ((and (string-prefix-p "•" choice-a)
                                              (not (string-prefix-p "•" choice-b))) t)
                                        ((and (string-prefix-p "•" choice-b)
                                              (not (string-prefix-p "•" choice-a))) nil)
                                        (t (string< choice-a choice-b)))))))
             (choices-list (mapcar 'car sorted-choices))
             ;; Let user select labels to toggle
             (selected-choices (completing-read-multiple
                                "Toggle labels (•=selected): "
                                (lambda (string pred action)
                                  (cond
                                   ((eq action 'metadata)
                                    '(metadata (display-sort-function . identity)
                                               (cycle-sort-function . identity)))
                                   (t (complete-with-action action choices-list string pred))))
                                nil nil ""))
             ;; Extract label names
             (selected-names (mapcar (lambda (choice)
                                       (if (string-match "^\\(?:• \\|  \\)\\(.+\\)$" choice)
                                           (match-string 1 choice)
                                         choice))
                                     selected-choices))
             ;; Toggle selected labels
             (final-labels (let ((result (copy-sequence current-labels)))
                             (dolist (label selected-names)
                               (if (member label result)
                                   (setq result (delete label result))
                                 (push label result)))
                             result)))
        (setq shipit-preview--labels final-labels)
        (shipit-preview--refresh)
        (message "Labels: %s"
                 (if final-labels
                     (mapconcat 'identity final-labels ", ")
                   "none")))
    (message "No labels available for this repository")))

(defun shipit-preview--open-file-diff ()
  "Open diff for file at point using local git range."
  (interactive)
  (let ((file-path (get-text-property (point) 'shipit-file-path)))
    (if file-path
        (let ((range (format "%s..HEAD" shipit-preview--base-ref)))
          (message "Opening diff for %s..." file-path)
          (magit-diff-range range nil (list file-path)))
      (message "No file at point"))))

(defun shipit-preview--dwim ()
  "Do-what-I-mean for RET in preview buffer.
Edit title, description, labels, reviewers, or assignees based on cursor position.
Open diff for files."
  (interactive)
  (cond
   ((get-text-property (point) 'shipit-preview-title)
    (shipit-preview--edit-title))
   ((get-text-property (point) 'shipit-preview-description)
    (shipit-preview--edit-description))
   ((get-text-property (point) 'shipit-preview-labels)
    (shipit-preview--select-labels))
   ((get-text-property (point) 'shipit-preview-reviewers)
    (shipit-preview--edit-reviewers))
   ((get-text-property (point) 'shipit-preview-assignees)
    (shipit-preview--edit-assignees))
   ((get-text-property (point) 'shipit-preview-draft)
    (shipit-preview--toggle-draft))
   ((get-text-property (point) 'shipit-preview-general-comment)
    (shipit-preview--manage-general-comment))
   ((get-text-property (point) 'shipit-preview-general-comments)
    (shipit-preview--add-general-comment))
   ((get-text-property (point) 'shipit-file-path)
    (shipit-preview--open-file-diff))
   (t
    ;; Fall back to magit section toggle
    (magit-section-toggle (magit-current-section)))))

(defun shipit-preview--insert-title-section ()
  "Insert the PR title section."
  (let* ((pr-label (shipit--pr-type-label)))
    (magit-insert-section (preview-title)
      (let ((start (point)))
        (magit-insert-heading
          (format "%s %s %s: %s"
                  (shipit--get-notification-source-icon (shipit-pr--backend-id))
                  (shipit--get-pr-field-icon "pull-request" "🔀")
                  (propertize pr-label 'font-lock-face 'magit-section-heading)
                  (propertize (or shipit-preview--title shipit-preview--branch)
                              'font-lock-face 'magit-branch-local)))
        (add-text-properties start (point)
                             '(shipit-preview-title t

                               help-echo "RET to edit title"))))))

(defun shipit-preview--insert-description-section ()
  "Insert the PR description section."
  (magit-insert-section (preview-description nil nil)
    (let ((start (point)))
      (magit-insert-heading
        (format "%s Description %s"
                (shipit--get-pr-field-icon "description" "📝")
                (if (and shipit-preview--description
                         (not (string-empty-p shipit-preview--description)))
                    ""
                  (propertize "(click to add)" 'font-lock-face 'font-lock-comment-face))))
      (add-text-properties start (point)
                           '(shipit-preview-description t
                             
                             help-echo "RET to edit description")))
    (magit-insert-section-body
      (if (and shipit-preview--description
               (not (string-empty-p shipit-preview--description)))
          (let ((rendered (if (and (boundp 'shipit-render-markdown)
                                   shipit-render-markdown
                                   (fboundp 'shipit--render-markdown))
                              (shipit--render-markdown shipit-preview--description)
                            shipit-preview--description)))
            (dolist (line (split-string rendered "\n"))
              (insert (format "   %s\n" line))))
        (let ((start (point)))
          (insert (propertize "   Click here or press RET to add a description\n"
                              'font-lock-face 'font-lock-comment-face))
          (add-text-properties start (point)
                               '(shipit-preview-description t
                                 )))))))

(defun shipit-preview--insert-refs-section ()
  "Insert the refs section showing base <- head."
  (let* ((base-branch (replace-regexp-in-string "^origin/" "" shipit-preview--base-ref))
         (refs-line (format "%s <- %s"
                            (propertize base-branch 'font-lock-face 'magit-branch-remote)
                            (propertize shipit-preview--branch 'font-lock-face 'magit-branch-local))))
    (magit-insert-section (preview-refs)
      (insert (format "%s %-7s %s\n"
                      (shipit--get-pr-field-icon "refs" "🔗")
                      "Refs:"
                      refs-line)))))

(defun shipit-preview--toggle-draft ()
  "Toggle whether this PR will be created as a draft."
  (interactive)
  (setq shipit-preview--draft (not shipit-preview--draft))
  (shipit-preview--refresh)
  (message "PR will be created as %s" (if shipit-preview--draft "draft" "ready for review")))

(defun shipit-preview--insert-draft-section ()
  "Insert the draft status section."
  (magit-insert-section (preview-draft)
    (let ((start (point))
          (status-text (if shipit-preview--draft
                           (propertize "Draft" 'font-lock-face 'font-lock-warning-face)
                         (propertize "Ready for review" 'font-lock-face 'success))))
      (insert (format "%s %-7s %s\n"
                      (shipit--get-pr-field-icon "draft" "📝")
                      "Status:"
                      status-text))
      (add-text-properties start (point)
                           '(shipit-preview-draft t
                             
                             help-echo "RET to toggle draft status")))))

(defun shipit-preview--insert-placeholder-section (type heading message)
  "Insert a placeholder section of TYPE with HEADING and MESSAGE."
  (magit-insert-section (type)
    (magit-insert-heading heading)
    (insert (propertize (format "   %s\n" message)
                        'font-lock-face 'font-lock-comment-face))))

(defun shipit-preview--insert-labels-section ()
  "Insert the labels section with current selections."
  (magit-insert-section (preview-labels nil t)
    (let ((start (point))
          (label-count (length shipit-preview--labels)))
      (magit-insert-heading
        (format "%s Labels (%d)"
                (shipit--get-label-icon "🏷️")
                label-count))
      (add-text-properties start (point)
                           '(shipit-preview-labels t
                             
                             help-echo "RET to add/remove labels")))
    (magit-insert-section-body
      (if shipit-preview--labels
          (dolist (label-name shipit-preview--labels)
            ;; Find the full label data to get color
            (let* ((label-data (seq-find (lambda (l)
                                           (equal (cdr (assq 'name l)) label-name))
                                         shipit-preview--cached-available-labels))
                   (color (when label-data (cdr (assq 'color label-data)))))
              (insert "   ")
              (if color
                  (insert (propertize (format "[%s]" label-name)
                                      'font-lock-face `(:foreground ,(format "#%s" color))))
                (insert (format "[%s]" label-name)))
              (insert "\n")))
        (let ((start (point)))
          (insert (propertize "   Press RET to add labels\n"
                              'font-lock-face 'font-lock-comment-face))
          (add-text-properties start (point)
                               '(shipit-preview-labels t
                                 )))))))

(defun shipit-preview--insert-reviewers-section ()
  "Insert the reviewers section."
  (magit-insert-section (preview-reviewers nil t)
    (let ((start (point))
          (count (length shipit-preview--reviewers)))
      (magit-insert-heading
        (format "%s Reviewers (%d)"
                (shipit--get-pr-field-icon "reviewer" "👤")
                count))
      (add-text-properties start (point)
                           '(shipit-preview-reviewers t
                             
                             help-echo "RET to edit reviewers")))
    (magit-insert-section-body
      (if shipit-preview--reviewers
          (dolist (username shipit-preview--reviewers)
            (insert (format "   @%s\n" (propertize username 'font-lock-face 'shipit-username-face))))
        (let ((start (point)))
          (insert (propertize "   Press RET to add reviewers\n"
                              'font-lock-face 'font-lock-comment-face))
          (add-text-properties start (point)
                               '(shipit-preview-reviewers t
                                 )))))))

(defun shipit-preview--insert-assignees-section ()
  "Insert the assignees section."
  (magit-insert-section (preview-assignees nil t)
    (let ((start (point))
          (count (length shipit-preview--assignees)))
      (magit-insert-heading
        (format "%s Assignees (%d)"
                (shipit--get-pr-field-icon "assignee" "👥")
                count))
      (add-text-properties start (point)
                           '(shipit-preview-assignees t
                             
                             help-echo "RET to edit assignees")))
    (magit-insert-section-body
      (if shipit-preview--assignees
          (dolist (username shipit-preview--assignees)
            (insert (format "   @%s\n" (propertize username 'font-lock-face 'shipit-username-face))))
        (let ((start (point)))
          (insert (propertize "   Press RET to add assignees\n"
                              'font-lock-face 'font-lock-comment-face))
          (add-text-properties start (point)
                               '(shipit-preview-assignees t
                                 )))))))

(defun shipit-preview--insert-general-comments-section ()
  "Insert the general comments section."
  (magit-insert-section (preview-general-comments nil t)
    (let ((start (point))
          (count (length shipit-preview--general-comments)))
      (magit-insert-heading
        (format "%s General Comments (%d)"
                (shipit--get-pr-field-icon "general-comments" "💬")
                count))
      (add-text-properties start (point)
                           '(shipit-preview-general-comments t
                             
                             help-echo "RET to add/manage comments")))
    (magit-insert-section-body
      (if shipit-preview--general-comments
          (dolist (comment shipit-preview--general-comments)
            (let* ((body (plist-get comment :body))
                   (id (plist-get comment :id))
                   (truncated (if (> (length body) 60)
                                  (concat (substring body 0 57) "...")
                                body))
                   (start (point)))
              (insert (format "   %s\n" truncated))
              (add-text-properties start (point)
                                   `(shipit-preview-general-comment t
                                     shipit-comment-id ,id
                                     
                                     help-echo "RET to edit/delete"))))
        (let ((start (point)))
          (insert (propertize "   Press RET to add a comment\n"
                              'font-lock-face 'font-lock-comment-face))
          (add-text-properties start (point)
                               '(shipit-preview-general-comments t
                                 )))))))

(defun shipit-preview--refresh (&optional _ignore-auto _noconfirm)
  "Refresh the preview buffer."
  (interactive)
  (require 'shipit-pr-sections)
  ;; Save position context before refresh
  (let ((saved-file-path (get-text-property (point) 'shipit-file-path))
        (saved-line-number (get-text-property (point) 'shipit-line-number))
        (saved-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (shipit-preview-root)
      ;; Header banner
      (shipit-preview--insert-header)

      ;; Title section
      (shipit-preview--insert-title-section)

      ;; Description section
      (shipit-preview--insert-description-section)

      ;; Refs section
      (shipit-preview--insert-refs-section)

      ;; Draft status section (toggleable)
      (shipit-preview--insert-draft-section)

      ;; Labels section (editable)
      (shipit-preview--insert-labels-section)

      ;; Reviewers section (editable)
      (shipit-preview--insert-reviewers-section)

      ;; Assignees section (editable)
      (shipit-preview--insert-assignees-section)

      ;; General comments section (editable)
      (shipit-preview--insert-general-comments-section)

      ;; Commits section - using shared renderer
      (let ((commits (shipit-preview--get-commits)))
        (shipit--render-commits-section commits shipit-preview--repo nil))

      ;; Files section - using shared renderer with filter support
      ;; Setup comment cache for inline rendering
      (shipit-preview--setup-comment-cache)
      ;; Cache files for filter refresh
      (setq shipit-preview--cached-files (shipit-preview--get-files))
      (shipit--render-files-section shipit-preview--cached-files
                                    shipit-preview--repo nil nil nil))
    ;; Restore position after refresh
    (if saved-file-path
        (shipit-preview--goto-file-line saved-file-path saved-line-number)
      ;; Fallback to approximate position if not on a file
      (goto-char (min saved-point (point-max))))))

(defun shipit-preview--refresh-files-section-only ()
  "Refresh only the files section for fast filter updates."
  (let ((inhibit-read-only t)
        (saved-point (point)))
    ;; Find the files section by text property
    (let ((header-pos (text-property-any (point-min) (point-max) 'shipit-pr-files t)))
      (when header-pos
        (goto-char header-pos)
        (let ((section (magit-current-section)))
          (when (and section
                     (condition-case nil (oref section start) (error nil)))
            (let* ((content-pos (condition-case nil (oref section content) (error nil)))
                   (section-end-pos (condition-case nil (oref section end) (error nil))))
              (when (and content-pos section-end-pos)
                ;; Clear old children
                (oset section children nil)
                ;; Delete content area only
                (delete-region content-pos section-end-pos)
                ;; Re-insert files content
                (goto-char content-pos)
                (let ((magit-insert-section--parent section))
                  ;; Setup comment cache
                  (shipit-preview--setup-comment-cache)
                  ;; Apply filter and render files
                  (let* ((all-files shipit-preview--cached-files)
                         (filtered-files (if (and (boundp 'shipit--files-filter-text)
                                                  (not (string-empty-p shipit--files-filter-text)))
                                             (seq-filter (lambda (file)
                                                           (shipit--file-matches-filter-p file shipit--files-filter-text))
                                                         all-files)
                                           all-files)))
                    (if (> (length filtered-files) 0)
                        (dolist (file filtered-files)
                          (shipit--insert-pr-file-section file shipit-preview--repo nil nil nil))
                      (if (not (string-empty-p (or shipit--files-filter-text "")))
                          (insert "   No files match filter\n")
                        (insert "   No files changed\n")))))
                ;; Update section end
                (oset section end (copy-marker (point)))))))))
    ;; Restore position
    (goto-char (min saved-point (point-max)))))

;;; Local comments

(defun shipit-preview--goto-file-line (file-path line-number)
  "Navigate to FILE-PATH and LINE-NUMBER after a buffer refresh.
If LINE-NUMBER is nil, go to the file header."
  (goto-char (point-min))
  ;; Find the file by text property
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal (get-text-property (point) 'shipit-file-path) file-path)
          (if line-number
              ;; Look for the specific line number
              (if (equal (get-text-property (point) 'shipit-line-number) line-number)
                  (setq found t)
                (forward-char 1))
            ;; File-level: stop at file header (no line number)
            (if (null (get-text-property (point) 'shipit-line-number))
                (setq found t)
              (forward-char 1)))
        (forward-char 1)))))

(defun shipit-preview--add-comment ()
  "Add a local comment at the current diff line."
  (interactive)
  ;; Check if we're in a commit-file context - comments should only be added
  ;; in the main Files Changed section to avoid confusion
  (when (or (get-text-property (point) 'shipit-commit-file)
            (get-text-property (point) 'shipit-commit-sha))
    (user-error "Comments can only be added in the main Files Changed section, not in commit-specific files"))
  (let ((file-path (get-text-property (point) 'shipit-file-path))
        (line-number (get-text-property (point) 'shipit-line-number))
        (side (or (get-text-property (point) 'shipit-diff-side) "RIGHT"))
        (pos (point)))
    (if (and file-path line-number)
        (if (fboundp 'shipit-editor-open)
            (shipit-editor-open
             (list :type 'preview-inline-comment
                   :source-buffer (current-buffer)
                   :repo shipit-preview--repo
                   :section-marker pos
                   :on-save (lambda (content)
                              (shipit-preview--on-inline-comment-added
                               file-path line-number side content))))
          ;; Fallback to simple read-string if editor not loaded
          (let ((body (read-string (format "Comment on %s:%d: " file-path line-number))))
            (shipit-preview--on-inline-comment-added file-path line-number side body)))
      (user-error "Not on a diff line - move to a + or - line to add a comment"))))

(defun shipit-preview--on-inline-comment-added (file-path line-number side content)
  "Callback when a new inline comment is added.
FILE-PATH, LINE-NUMBER, SIDE identify the location, CONTENT is the comment text."
  (when (and content (not (string-empty-p content)))
    (let ((comment (list :id (format-time-string "%s%N")
                         :file file-path
                         :line line-number
                         :side side
                         :body content)))
      (push comment shipit-preview--comments)
      ;; Use targeted files section refresh instead of full buffer refresh
      (shipit-preview--refresh-files-section-only)
      ;; Navigate back to the original position
      (shipit-preview--goto-file-line file-path line-number)
      (message "Comment added to %s:%d" file-path line-number))))

(defun shipit-preview--add-file-comment ()
  "Add a file-level comment (not on a specific line)."
  (interactive)
  ;; Check if we're in a commit-file context - comments should only be added
  ;; in the main Files Changed section to avoid confusion
  (when (or (get-text-property (point) 'shipit-commit-file)
            (get-text-property (point) 'shipit-commit-sha))
    (user-error "Comments can only be added in the main Files Changed section, not in commit-specific files"))
  (let ((file-path (get-text-property (point) 'shipit-file-path))
        (pos (point)))
    (if file-path
        (if (fboundp 'shipit-editor-open)
            (shipit-editor-open
             (list :type 'preview-file-comment
                   :source-buffer (current-buffer)
                   :repo shipit-preview--repo
                   :section-marker pos
                   :on-save (lambda (content)
                              (shipit-preview--on-file-comment-added file-path content))))
          ;; Fallback to simple read-string if editor not loaded
          (let ((body (read-string (format "File comment on %s: " file-path))))
            (shipit-preview--on-file-comment-added file-path body)))
      (user-error "Not on a file - move to a file header to add a file comment"))))

(defun shipit-preview--on-file-comment-added (file-path content)
  "Callback when a new file-level comment is added.
FILE-PATH identifies the file, CONTENT is the comment text."
  (when (and content (not (string-empty-p content)))
    (let ((comment (list :id (format-time-string "%s%N")
                         :file file-path
                         :line nil  ; nil indicates file-level comment
                         :side nil
                         :body content
                         :file-level t)))
      (push comment shipit-preview--comments)
      ;; Use targeted files section refresh instead of full buffer refresh
      (shipit-preview--refresh-files-section-only)
      ;; Navigate back to the file header
      (shipit-preview--goto-file-line file-path nil)
      (message "File comment added to %s" file-path))))

(defun shipit-preview--convert-comments-to-cache-format ()
  "Convert local preview comments to the format expected by shipit--cached-inline-comments.
Returns a list of alists compatible with the inline comment rendering."
  (mapcar (lambda (comment)
            `((id . ,(plist-get comment :id))
              (path . ,(plist-get comment :file))
              (line . ,(plist-get comment :line))
              (side . ,(plist-get comment :side))
              (body . ,(plist-get comment :body))
              (user . ((login . "You (draft)")
                       (avatar_url . nil)))
              (created_at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ"))
              (outdated . nil)
              (is_outdated . nil)))
          shipit-preview--comments))

(defun shipit-preview--setup-comment-cache ()
  "Populate the inline comments cache with local preview comments.
Also sets `shipit--inline-comments-fetched' so comment indicators appear."
  (setq-local shipit--cached-inline-comments
              (shipit-preview--convert-comments-to-cache-format))
  (setq-local shipit--inline-comments-fetched t))

(defun shipit-preview--in-preview-buffer-p ()
  "Return non-nil if currently in a shipit preview buffer."
  (derived-mode-p 'shipit-preview-mode))

(defun shipit-preview--get-comments-for-file (file-path)
  "Get all local comments for FILE-PATH."
  (seq-filter (lambda (c) (equal (plist-get c :file) file-path))
              shipit-preview--comments))

(defun shipit-preview--find-comment-by-id (comment-id)
  "Find a local comment by COMMENT-ID."
  (seq-find (lambda (c) (equal (plist-get c :id) (format "%s" comment-id)))
            shipit-preview--comments))

(defun shipit-preview--edit-comment-by-id (comment-id)
  "Edit the local comment with COMMENT-ID."
  (let ((comment (shipit-preview--find-comment-by-id comment-id)))
    (if comment
        (let* ((file-path (plist-get comment :file))
               (line-number (plist-get comment :line))
               (current-body (plist-get comment :body))
               (is-file-level (plist-get comment :file-level))
               (type (if is-file-level 'preview-file-comment 'preview-inline-comment)))
          (if (fboundp 'shipit-editor-open)
              (shipit-editor-open
               (list :type type
                     :source-buffer (current-buffer)
                     :repo shipit-preview--repo
                     :comment-id comment-id
                     :initial-content current-body
                     :on-save (lambda (content)
                                (shipit-preview--on-comment-edited comment-id content))))
            ;; Fallback to simple read-string if editor not loaded
            (let ((new-body (read-string "Edit comment: " current-body)))
              (shipit-preview--on-comment-edited comment-id new-body))))
      (user-error "Comment not found"))))

(defun shipit-preview--on-comment-edited (comment-id content)
  "Callback when an inline/file comment is edited.
COMMENT-ID is the comment to update, CONTENT is the new text."
  (when (and content (not (string-empty-p content)))
    (let ((comment (shipit-preview--find-comment-by-id comment-id)))
      (when comment
        (let ((file-path (plist-get comment :file))
              (line-number (plist-get comment :line)))
          (plist-put comment :body content)
          ;; Use targeted files section refresh instead of full buffer refresh
          (shipit-preview--refresh-files-section-only)
          (shipit-preview--goto-file-line file-path line-number)
          (message "Comment updated"))))))

(defun shipit-preview--delete-comment-by-id (comment-id)
  "Delete the local comment with COMMENT-ID."
  (let ((comment (shipit-preview--find-comment-by-id comment-id)))
    (if comment
        (let ((file-path (plist-get comment :file))
              (line-number (plist-get comment :line)))
          (when (yes-or-no-p "Delete this comment? ")
            (setq shipit-preview--comments
                  (seq-remove (lambda (c) (equal (plist-get c :id) (format "%s" comment-id)))
                              shipit-preview--comments))
            ;; Use targeted files section refresh instead of full buffer refresh
            (shipit-preview--refresh-files-section-only)
            (shipit-preview--goto-file-line file-path line-number)
            (message "Comment deleted")))
      (user-error "Comment not found"))))

(defun shipit-preview--comment-actions ()
  "Show actions for local comment at point (same style as regular shipit buffer)."
  (interactive)
  (let* ((comment-id (get-text-property (point) 'shipit-comment-id))
         (choice (completing-read "Comment action: "
                                  '("Edit comment" "Delete comment")
                                  nil t)))
    (cond
     ((string= choice "Edit comment")
      (let ((current-body (get-text-property (point) 'shipit-comment-body)))
        (shipit-preview--edit-comment-by-id comment-id)))
     ((string= choice "Delete comment")
      (shipit-preview--delete-comment-by-id comment-id))
     (t (message "No action selected")))))

(defun shipit-preview--get-file-level-comments (file-path)
  "Get all file-level comments (no line number) for FILE-PATH."
  (seq-filter (lambda (c)
                (and (equal (plist-get c :file) file-path)
                     (null (plist-get c :line))))
              shipit-preview--comments))

(defun shipit-preview--truncate-body (body)
  "Truncate BODY to 40 chars with ellipsis if needed."
  (if (> (length body) 40)
      (concat (substring body 0 40) "...")
    body))

(defun shipit-preview--file-header-actions ()
  "Show actions for file header - add new or manage existing file comments."
  (interactive)
  ;; Check if we're in a commit-file context - comments should only be added
  ;; in the main Files Changed section to avoid confusion
  (when (or (get-text-property (point) 'shipit-commit-file)
            (get-text-property (point) 'shipit-commit-sha))
    (user-error "Comments can only be added in the main Files Changed section, not in commit-specific files"))
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (file-comments (shipit-preview--get-file-level-comments file-path))
         (add-label (concat (propertize "Add" 'font-lock-face '(:foreground "green")) " "))
         (edit-label (concat (propertize "Edit" 'font-lock-face '(:foreground "orange")) ": "))
         (delete-label (concat (propertize "Delete" 'font-lock-face '(:foreground "red")) ": "))
         (options (if file-comments
                      (append
                       (list (concat add-label "new file comment"))
                       (mapcar (lambda (c)
                                 (concat edit-label
                                         (shipit-preview--truncate-body (plist-get c :body))))
                               file-comments)
                       (mapcar (lambda (c)
                                 (concat delete-label
                                         (shipit-preview--truncate-body (plist-get c :body))))
                               file-comments))
                    (list (concat add-label "file comment"))))
         (choice (completing-read "File comment action: " options nil t)))
    (cond
     ((string-prefix-p "Add" (substring-no-properties choice))
      (shipit-preview--add-file-comment))
     ((string-prefix-p "Edit" (substring-no-properties choice))
      (let* ((body-preview (substring (substring-no-properties choice) 6))
             (comment (seq-find (lambda (c)
                                  (string= body-preview
                                           (shipit-preview--truncate-body (plist-get c :body))))
                                file-comments)))
        (when comment
          (shipit-preview--edit-comment-by-id (plist-get comment :id)))))
     ((string-prefix-p "Delete" (substring-no-properties choice))
      (let* ((body-preview (substring (substring-no-properties choice) 8))
             (comment (seq-find (lambda (c)
                                  (string= body-preview
                                           (shipit-preview--truncate-body (plist-get c :body))))
                                file-comments)))
        (when comment
          (shipit-preview--delete-comment-by-id (plist-get comment :id)))))
     (t (message "No action selected")))))

(defun shipit-preview--get-visible-file-paths ()
  "Get list of visible file path strings (respecting filter)."
  (let ((files shipit-preview--cached-files))
    (when (and (boundp 'shipit--files-filter-text)
               (not (string-empty-p shipit--files-filter-text)))
      ;; Filter is active - get only matching files
      (setq files (seq-filter (lambda (file)
                                (shipit--file-matches-filter-p file shipit--files-filter-text))
                              files)))
    (mapcar (lambda (f) (cdr (assq 'filename f))) files)))

(defun shipit-preview--make-file-set (file-paths)
  "Create a hash table set from FILE-PATHS for O(1) lookup."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (path file-paths)
      (puthash path t set))
    set))

(defun shipit-preview--count-comments-for-files (file-set &optional file-level-only)
  "Count comments for files in FILE-SET hash table.
If FILE-LEVEL-ONLY is non-nil, count only file-level comments."
  (let ((count 0))
    (dolist (c shipit-preview--comments)
      (when (and (gethash (plist-get c :file) file-set)
                 (or (not file-level-only)
                     (null (plist-get c :line))))
        (setq count (1+ count))))
    count))

(defun shipit-preview--delete-comments-for-files (file-set &optional file-level-only)
  "Delete all comments for files in FILE-SET hash table.
If FILE-LEVEL-ONLY is non-nil, delete only file-level comments."
  (setq shipit-preview--comments
        (seq-remove (lambda (c)
                      (and (gethash (plist-get c :file) file-set)
                           (or (not file-level-only)
                               (null (plist-get c :line)))))
                    shipit-preview--comments)))

(defun shipit-preview--files-changed-header-actions ()
  "Show bulk comment actions for Files Changed section."
  (interactive)
  (let* ((visible-file-paths (shipit-preview--get-visible-file-paths))
         (file-set (shipit-preview--make-file-set visible-file-paths))
         (filter-active (and (boundp 'shipit--files-filter-text)
                             (not (string-empty-p shipit--files-filter-text))))
         (scope-label (if filter-active "filtered files" "all files"))
         (all-count (shipit-preview--count-comments-for-files file-set))
         (file-level-count (shipit-preview--count-comments-for-files file-set t))
         (inline-count (- all-count file-level-count))
         (delete-all-label (concat (propertize "Delete" 'font-lock-face '(:foreground "red"))
                                   (format " all comments (%d) on %s" all-count scope-label)))
         (delete-file-label (concat (propertize "Delete" 'font-lock-face '(:foreground "red"))
                                    (format " file-level comments (%d) on %s" file-level-count scope-label)))
         (delete-inline-label (concat (propertize "Delete" 'font-lock-face '(:foreground "red"))
                                      (format " inline comments (%d) on %s" inline-count scope-label)))
         (options (list delete-all-label delete-file-label delete-inline-label))
         (choice (completing-read "Bulk comment action: " options nil t)))
    (cond
     ((string-prefix-p "Delete all" (substring-no-properties choice))
      (when (yes-or-no-p (format "Delete all %d comments on %s? " all-count scope-label))
        (shipit-preview--delete-comments-for-files file-set)
        (shipit-preview--refresh)
        (message "Deleted %d comments" all-count)))
     ((string-prefix-p "Delete file-level" (substring-no-properties choice))
      (when (yes-or-no-p (format "Delete %d file-level comments on %s? " file-level-count scope-label))
        (shipit-preview--delete-comments-for-files file-set t)
        (shipit-preview--refresh)
        (message "Deleted %d file-level comments" file-level-count)))
     ((string-prefix-p "Delete inline" (substring-no-properties choice))
      (when (yes-or-no-p (format "Delete %d inline comments on %s? " inline-count scope-label))
        ;; Delete inline only (comments with line numbers)
        (setq shipit-preview--comments
              (seq-remove (lambda (c)
                            (and (gethash (plist-get c :file) file-set)
                                 (plist-get c :line)))
                          shipit-preview--comments))
        (shipit-preview--refresh)
        (message "Deleted %d inline comments" inline-count)))
     (t (message "No action selected")))))

;; Register dwim handlers for preview mode (shipit-magit already required above)

;; Handler for Files Changed header (bulk operations)
(shipit-register-dwim-handler
 'preview-files-header
 (lambda ()
   (and (shipit-preview--in-preview-buffer-p)
        (get-text-property (point) 'shipit-pr-files)))
 #'shipit-preview--files-changed-header-actions)

;; Handler for existing comments (edit/delete)
(shipit-register-dwim-handler
 'preview-existing-comment
 (lambda ()
   (and (shipit-preview--in-preview-buffer-p)
        (get-text-property (point) 'shipit-comment-id)
        (shipit-preview--find-comment-by-id
         (get-text-property (point) 'shipit-comment-id))))
 #'shipit-preview--comment-actions)

;; Handler for adding new comments on diff lines
;; Only matches in main Files Changed section, NOT in commit-specific files
(shipit-register-dwim-handler
 'preview-inline-comment
 (lambda ()
   (and (shipit-preview--in-preview-buffer-p)
        (get-text-property (point) 'shipit-file-path)
        (get-text-property (point) 'shipit-line-number)
        (not (get-text-property (point) 'shipit-comment-id))
        ;; Exclude commit-specific files - comments only in main Files Changed
        (not (get-text-property (point) 'shipit-commit-file))
        (not (get-text-property (point) 'shipit-commit-sha))))
 #'shipit-preview--add-comment)

;; Handler for file headers (add/edit/delete file-level comments)
;; Only matches in main Files Changed section, NOT in commit-specific files
(shipit-register-dwim-handler
 'preview-file-comment
 (lambda ()
   (and (shipit-preview--in-preview-buffer-p)
        (get-text-property (point) 'shipit-file-path)
        (not (get-text-property (point) 'shipit-line-number))
        (not (get-text-property (point) 'shipit-comment-id))
        ;; Exclude commit-specific files - comments only in main Files Changed
        (not (get-text-property (point) 'shipit-commit-file))
        (not (get-text-property (point) 'shipit-commit-sha))))
 #'shipit-preview--file-header-actions)

;; Handler for refs section (change target branch)
(shipit-register-dwim-handler
 'preview-refs
 (lambda ()
   (and (shipit-preview--in-preview-buffer-p)
        (fboundp 'magit-current-section)
        (magit-section-match '(preview-refs) (magit-current-section))))
 #'shipit-preview--change-base-ref)

;;; Entry point

(defun shipit-preview--get-upstream-default-branch ()
  "Get the default branch of the upstream remote."
  (let ((result (string-trim
                 (shell-command-to-string
                  "git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null \
| sed 's@^refs/remotes/origin/@@'"))))
    (if (string-empty-p result)
        "main"
      result)))

(defun shipit-preview--get-remote-branches ()
  "Get list of remote branches from origin."
  (let ((output (shell-command-to-string "git branch -r 2>/dev/null")))
    (when (not (string-empty-p output))
      (delq nil
            (mapcar (lambda (line)
                      (let ((trimmed (string-trim line)))
                        (when (and (string-prefix-p "origin/" trimmed)
                                   (not (string-match-p "HEAD" trimmed)))
                          trimmed)))
                    (split-string output "\n" t))))))

(defun shipit-preview--change-base-ref ()
  "Change the base ref for the PR preview.
Prompts user to select from available remote branches."
  (interactive)
  (let* ((branches (shipit-preview--get-remote-branches))
         (current (or shipit-preview--base-ref "origin/main"))
         (selected (completing-read
                    (format "Target branch (current: %s): " current)
                    branches nil t nil nil current)))
    (when (and selected (not (string-equal selected current)))
      (setq shipit-preview--base-ref selected)
      (message "Changed target branch to %s" selected)
      (shipit-preview--refresh))))

(defun shipit-preview--buffer-name (repo branch)
  "Generate buffer name for REPO and BRANCH preview."
  (format "*shipit-preview: %s @ %s*" repo branch))

;;; PR Creation

(defun shipit-preview--branch-exists-on-remote-p (branch)
  "Check if BRANCH exists on the remote.
First checks locally-known tracking refs, then falls back to ls-remote."
  (let ((local-check (string-trim
                      (shell-command-to-string
                       (format "git rev-parse --verify origin/%s 2>/dev/null"
                               (shell-quote-argument branch))))))
    (if (not (string-empty-p local-check))
        t
      ;; Tracking ref not found locally — try network
      (let ((remote-check (string-trim
                           (shell-command-to-string
                            (format "git ls-remote --heads origin %s 2>/dev/null"
                                    (shell-quote-argument branch))))))
        (not (string-empty-p remote-check))))))

(defun shipit-preview--push-branch (branch)
  "Push BRANCH to origin, setting upstream tracking."
  (message "Pushing branch %s to origin..." branch)
  (let ((result (shell-command-to-string
                 (format "git push -u origin %s 2>&1"
                         (shell-quote-argument branch)))))
    (if (string-match-p "error:\\|fatal:" result)
        (progn
          (message "Push failed: %s" result)
          nil)
      (message "Branch %s pushed to origin" branch)
      t)))

(defun shipit-preview--ensure-branch-pushed ()
  "Ensure current branch is pushed to remote. Return t if ready for PR."
  (let ((branch shipit-preview--branch))
    (if (shipit-preview--branch-exists-on-remote-p branch)
        t
      ;; Branch doesn't exist on remote - offer to push
      (if (y-or-n-p (format "Branch '%s' doesn't exist on remote. Push it now? " branch))
          (shipit-preview--push-branch branch)
        (message "Cannot create PR without pushing branch first")
        nil))))

(defun shipit-preview--add-general-comment ()
  "Add a new general comment to the preview PR."
  (interactive)
  (if (fboundp 'shipit-editor-open)
      (shipit-editor-open
       (list :type 'preview-general-comment
             :source-buffer (current-buffer)
             :on-save #'shipit-preview--on-general-comment-added))
    ;; Fallback to simple read-string if editor not loaded
    (let ((body (read-string "Comment: ")))
      (when (and body (not (string-empty-p body)))
        (shipit-preview--on-general-comment-added body)))))

(defun shipit-preview--on-general-comment-added (content)
  "Callback when a new general comment is added.
CONTENT is the comment text."
  (when (and content (not (string-empty-p content)))
    (let ((comment (list :body content
                         :id (format-time-string "%Y%m%d%H%M%S%3N"))))
      (push comment shipit-preview--general-comments)
      (shipit-preview--refresh)
      (message "Comment added"))))

(defun shipit-preview--manage-general-comment ()
  "Manage the general comment at point (edit or delete)."
  (interactive)
  (let* ((comment-id (get-text-property (point) 'shipit-comment-id))
         (comment (cl-find-if (lambda (c) (equal (plist-get c :id) comment-id))
                              shipit-preview--general-comments))
         (action (completing-read "Action: " '("Edit" "Delete") nil t)))
    (cond
     ((string= action "Edit")
      (if (fboundp 'shipit-editor-open)
          (shipit-editor-open
           (list :type 'preview-general-comment
                 :source-buffer (current-buffer)
                 :comment-id comment-id
                 :initial-content (plist-get comment :body)
                 :on-save (lambda (content)
                            (shipit-preview--on-general-comment-edited comment-id content))))
        ;; Fallback to simple read-string if editor not loaded
        (let ((new-body (read-string "Comment: " (plist-get comment :body))))
          (shipit-preview--on-general-comment-edited comment-id new-body))))
     ((string= action "Delete")
      (when (y-or-n-p "Delete this comment? ")
        (setq shipit-preview--general-comments
              (cl-remove-if (lambda (c) (equal (plist-get c :id) comment-id))
                            shipit-preview--general-comments))
        (shipit-preview--refresh)
        (message "Comment deleted"))))))

(defun shipit-preview--on-general-comment-edited (comment-id content)
  "Callback when a general comment is edited.
COMMENT-ID is the comment to update, CONTENT is the new text."
  (when (and content (not (string-empty-p content)))
    (let ((comment (cl-find-if (lambda (c) (equal (plist-get c :id) comment-id))
                               shipit-preview--general-comments)))
      (when comment
        (plist-put comment :body content)
        (shipit-preview--refresh)
        (message "Comment updated")))))

(defun shipit-preview--find-pr-for-branch (branch repo)
  "Find an existing PR/MR for BRANCH in REPO via the PR backend.
Returns normalized PR data if found, nil otherwise."
  (condition-case nil
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (search-fn (plist-get backend :search))
             (results (when search-fn
                        (funcall search-fn config (list :state "all")))))
        (cl-find-if (lambda (pr)
                      (string= (cdr (assq 'ref (cdr (assq 'head pr)))) branch))
                    results))
    (error nil)))

(defun shipit-preview--check-existing-pr ()
  "Check if a PR already exists for the current branch.
Returns the existing PR data if found, nil otherwise."
  (shipit-preview--find-pr-for-branch
   shipit-preview--branch shipit-preview--repo))

(defun shipit-preview--create-pr ()
  "Create the PR/MR with current preview settings."
  (interactive)
  (require 'shipit-http)
  ;; Check if PR already exists
  (let ((existing-pr (shipit-preview--check-existing-pr)))
    (when existing-pr
      (let* ((pr-number (cdr (assq 'number existing-pr)))
             (html-url (cdr (assq 'html_url existing-pr))))
        (if (y-or-n-p (format "PR #%d already exists for this branch. Open it instead? " pr-number))
            (progn
              (kill-buffer)
              (shipit-open-pr-buffer pr-number shipit-preview--repo))
          (user-error "Cannot create PR - one already exists for branch '%s'" shipit-preview--branch)))))
  ;; First ensure the branch is pushed
  (unless (shipit-preview--ensure-branch-pushed)
    (user-error "Branch must be pushed to remote before creating PR"))
  (let* ((base-branch (replace-regexp-in-string "^origin/" "" shipit-preview--base-ref))
         (title (or shipit-preview--title shipit-preview--branch))
         (body (or shipit-preview--description ""))
         (draft shipit-preview--draft)
         (resolved (shipit-pr--resolve-for-repo shipit-preview--repo))
         (backend (car resolved))
         (config (cdr resolved)))
    ;; Confirm with user
    (when (yes-or-no-p
           (format "Create PR '%s' on %s?\n  %s <- %s\n  Status: %s\n  Labels: %s\n  Reviewers: %s\n  Assignees: %s\n  Comments: %s"
                   title
                   shipit-preview--repo
                   base-branch
                   shipit-preview--branch
                   (if draft "🚧 Draft" "✅ Ready for review")
                   (if shipit-preview--labels
                       (mapconcat 'identity shipit-preview--labels ", ")
                     "none")
                   (if shipit-preview--reviewers
                       (mapconcat 'identity shipit-preview--reviewers ", ")
                     "none")
                   (if shipit-preview--assignees
                       (mapconcat 'identity shipit-preview--assignees ", ")
                     "none")
                   (let ((inline-count (length shipit-preview--comments))
                         (general-count (length shipit-preview--general-comments)))
                     (cond
                      ((and (> inline-count 0) (> general-count 0))
                       (format "%d inline, %d general" inline-count general-count))
                      ((> inline-count 0)
                       (format "%d inline" inline-count))
                      ((> general-count 0)
                       (format "%d general" general-count))
                      (t "none")))))
      (message "Creating PR...")
      (let ((response (funcall (plist-get backend :create-pr)
                               config title body base-branch
                               shipit-preview--branch)))
        (if response
            (let ((pr-number (cdr (assq 'number response)))
                  (html-url (cdr (assq 'html_url response))))
              (message "PR #%d created: %s" pr-number html-url)
              ;; Apply labels if any
              (when shipit-preview--labels
                (shipit-preview--apply-labels pr-number))
              ;; Apply reviewers if any
              (when shipit-preview--reviewers
                (shipit-preview--apply-reviewers pr-number))
              ;; Apply assignees if any
              (when shipit-preview--assignees
                (shipit-preview--apply-assignees pr-number))
              ;; Apply inline comments if any
              (when shipit-preview--comments
                (shipit-preview--apply-comments pr-number))
              ;; Apply general comments if any
              (when shipit-preview--general-comments
                (shipit-preview--apply-general-comments pr-number))
              ;; Clear saved state since PR was created successfully
              (shipit-preview--clear-saved-state)
              ;; Open the new PR in shipit
              (when (y-or-n-p "Open PR in shipit? ")
                (kill-buffer)
                (shipit-open-pr-buffer pr-number shipit-preview--repo)))
          (user-error "Failed to create PR - check your permissions and try again"))))))

(defun shipit-preview--apply-labels (pr-number)
  "Apply labels to the newly created PR."
  (let* ((resolved (shipit-pr--resolve-for-repo shipit-preview--repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :set-labels) config pr-number shipit-preview--labels)
    (message "Applied %d labels" (length shipit-preview--labels))))

(defun shipit-preview--apply-reviewers (pr-number)
  "Request reviewers for the newly created PR."
  (let* ((resolved (shipit-pr--resolve-for-repo shipit-preview--repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :add-reviewers-batch) config pr-number shipit-preview--reviewers)
    (message "Requested %d reviewers" (length shipit-preview--reviewers))))

(defun shipit-preview--apply-assignees (pr-number)
  "Apply assignees to the newly created PR."
  (let* ((resolved (shipit-pr--resolve-for-repo shipit-preview--repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :add-assignees-batch) config pr-number shipit-preview--assignees)
    (message "Assigned to %d users" (length shipit-preview--assignees))))

(defun shipit-preview--apply-comments (pr-number)
  "Apply preview comments to the newly created PR-NUMBER."
  (when shipit-preview--comments
    (let* ((resolved (shipit-comment--resolve-for-repo shipit-preview--repo))
           (comment-backend (car resolved))
           (comment-config (cdr resolved))
           (add-fn (plist-get comment-backend :add-inline-comment))
           (comment-count 0)
           (error-count 0))
      (dolist (comment shipit-preview--comments)
        (let ((file (plist-get comment :file))
              (line (plist-get comment :line))
              (side (plist-get comment :side))
              (body (plist-get comment :body)))
          (condition-case err
              (progn
                (funcall add-fn comment-config pr-number file line body side)
                (setq comment-count (1+ comment-count)))
            (error
             (setq error-count (1+ error-count))
             (shipit--debug-log "PREVIEW: Failed to add comment on %s:%s - %s"
                                file (or line "file") (error-message-string err))))))
      (if (> error-count 0)
          (message "Added %d inline comments (%d failed)" comment-count error-count)
        (message "Added %d inline comments" comment-count)))))

(defun shipit-preview--apply-general-comments (pr-number)
  "Apply general comments to the newly created PR-NUMBER."
  (when shipit-preview--general-comments
    (let* ((resolved (shipit-comment--resolve-for-repo shipit-preview--repo))
           (comment-backend (car resolved))
           (comment-config (cdr resolved))
           (add-fn (plist-get comment-backend :add-general-comment))
           (comment-count 0)
           (error-count 0))
      (dolist (comment shipit-preview--general-comments)
        (let ((body (plist-get comment :body)))
          (condition-case err
              (progn
                (funcall add-fn comment-config pr-number body)
                (setq comment-count (1+ comment-count)))
            (error
             (setq error-count (1+ error-count))
             (message "Failed to add general comment - %s"
                      (error-message-string err))))))
      (if (> error-count 0)
          (message "Added %d general comments (%d failed)" comment-count error-count)
        (message "Added %d general comments" comment-count)))))

;;; Auto-fill helpers

(defun shipit-preview--maybe-auto-fill-description (current-description commits)
  "Return description to use, possibly auto-filled from commit message.
CURRENT-DESCRIPTION is the existing description (nil or string).
COMMITS is the list of commits for this PR.

When there is exactly 1 commit and description is empty, returns
the full commit message. Otherwise returns CURRENT-DESCRIPTION unchanged."
  (if (and (= (length commits) 1)
           (or (null current-description)
               (string-empty-p current-description)))
      ;; Single commit with empty description - use commit message
      (plist-get (car commits) :message)
    ;; Keep existing description
    current-description))

;;; Entry point

(defun shipit-preview--get-default-assignees ()
  "Get default assignees based on `shipit-preview-default-assignee'.
Returns a list of usernames, or nil if no default."
  (when (eq shipit-preview-default-assignee 'self)
    (require 'shipit-http)
    (let ((user (shipit--get-current-user)))
      (when user (list user)))))

;;;###autoload
(defun shipit-preview-pr ()
  "Open a preview buffer for the current branch as a PR.
If a PR already exists for this branch, offers to open it instead."
  (interactive)
  (let* ((repo (shipit--get-repo-from-remote))
         (branch (magit-get-current-branch)))
    (unless repo
      (user-error "Not in a git repository with a recognized remote"))
    (unless branch
      (user-error "Not on a branch (detached HEAD?)"))
    ;; Check if PR already exists for this branch
    (let ((existing-pr (shipit-preview--find-pr-for-branch branch repo)))
      (if existing-pr
          (let ((pr-number (cdr (assq 'number existing-pr))))
            (if (y-or-n-p (format "PR #%d already exists for branch '%s'. Open it? "
                                  pr-number branch))
                (shipit-open-pr-buffer pr-number repo)
              (message "PR #%d exists for this branch" pr-number)))
        ;; No existing PR - open preview
        (let ((base-ref (shipit-preview--get-upstream-default-branch))
              (buf-name (shipit-preview--buffer-name repo branch)))
          (with-current-buffer (get-buffer-create buf-name)
            (shipit-preview-mode)
            (setq shipit-preview--repo repo
                  shipit-preview--branch branch
                  shipit-preview--base-ref (concat "origin/" base-ref))
            ;; Try to restore saved state, otherwise use defaults
            (unless (shipit-preview--restore-state)
              (setq shipit-preview--title branch
                    shipit-preview--draft shipit-preview-default-draft
                    shipit-preview--assignees (shipit-preview--get-default-assignees)))
            ;; Auto-fill description from commit message if single commit
            ;; (runs even after restore, in case description was empty)
            (let ((commits (shipit-preview--get-commits)))
              (setq shipit-preview--description
                    (shipit-preview--maybe-auto-fill-description
                     shipit-preview--description commits)))
            (shipit-preview--refresh nil t))
          (switch-to-buffer buf-name))))))

(provide 'shipit-preview)
;;; shipit-preview.el ends here
