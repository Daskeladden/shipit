;;; shipit-issue-create.el --- Rich issue creation buffer -*- lexical-binding: t; -*-

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
;; Rich, section-based buffer for creating issues.
;; Supports editable title, description, labels, assignees, and
;; backend-specific fields like issue type and components (Jira).
;; Draft state persists across quit/reopen.
;; C-c C-c submits the issue via the active backend.

;;; Code:

(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issues)
(require 'shipit-render)
(require 'magit-section)

;; Forward declarations
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-editor-open "shipit-editor")
(declare-function shipit--get-notification-source-icon "shipit-render")

;;; Buffer-local state

(defvar-local shipit-issue-create--repo nil "Repository owner/name.")
(defvar-local shipit-issue-create--backend-id nil "Active backend symbol.")
(defvar-local shipit-issue-create--config nil "Backend config plist.")
(defvar-local shipit-issue-create--title nil "Draft issue title.")
(defvar-local shipit-issue-create--body nil "Draft issue description.")
(defvar-local shipit-issue-create--labels nil "List of selected label names.")
(defvar-local shipit-issue-create--assignees nil "List of selected assignee names.")
(defvar-local shipit-issue-create--issue-type nil "Selected issue type (Jira).")
(defvar-local shipit-issue-create--components nil "List of selected components (Jira).")
(defvar-local shipit-issue-create--field-descriptors nil "List of field descriptor plists.")
(defvar-local shipit-issue-create--cached-options nil
  "Hash table caching fetched options per field name.")
(defvar-local shipit-issue-create--expanded-fields nil
  "List of field names whose sections should render expanded.")

;;; Global state persistence

(defvar shipit-issue-create--drafts (make-hash-table :test 'equal)
  "Hash table storing draft state per repo:backend-id.")

(defun shipit-issue-create--draft-key ()
  "Generate cache key for current draft state."
  (format "%s:%s" shipit-issue-create--repo shipit-issue-create--backend-id))

(defun shipit-issue-create--save-draft ()
  "Save current draft state for later restoration."
  (when (and shipit-issue-create--repo shipit-issue-create--backend-id)
    (let ((key (shipit-issue-create--draft-key))
          (state (list :title shipit-issue-create--title
                       :body shipit-issue-create--body
                       :labels shipit-issue-create--labels
                       :assignees shipit-issue-create--assignees
                       :issue-type shipit-issue-create--issue-type
                       :components shipit-issue-create--components)))
      (puthash key state shipit-issue-create--drafts)
      (shipit--debug-log "Issue create: saved draft for %s" key))))

(defun shipit-issue-create--restore-draft ()
  "Restore previously saved draft state if available.
Returns non-nil if state was restored."
  (let* ((key (shipit-issue-create--draft-key))
         (state (gethash key shipit-issue-create--drafts)))
    (when state
      (setq shipit-issue-create--title (plist-get state :title)
            shipit-issue-create--body (plist-get state :body)
            shipit-issue-create--labels (plist-get state :labels)
            shipit-issue-create--assignees (plist-get state :assignees)
            shipit-issue-create--issue-type (plist-get state :issue-type)
            shipit-issue-create--components (plist-get state :components))
      (shipit--debug-log "Issue create: restored draft for %s" key)
      t)))

(defun shipit-issue-create--clear-draft ()
  "Clear saved draft state for current buffer."
  (remhash (shipit-issue-create--draft-key) shipit-issue-create--drafts))

;;; Mode and keymap

(defvar shipit-issue-create-mode-map nil
  "Keymap for `shipit-issue-create-mode'.")

(unless shipit-issue-create-mode-map
  (setq shipit-issue-create-mode-map (make-sparse-keymap))
  (set-keymap-parent shipit-issue-create-mode-map magit-section-mode-map))

(define-key shipit-issue-create-mode-map (kbd "RET") #'shipit-issue-create--dwim)
(define-key shipit-issue-create-mode-map (kbd "C-c C-c") #'shipit-issue-create--submit)
(define-key shipit-issue-create-mode-map (kbd "g") #'shipit-issue-create--refresh)
(define-key shipit-issue-create-mode-map (kbd "q") #'shipit-issue-create--quit)

(define-derived-mode shipit-issue-create-mode magit-section-mode "Shipit-Issue-Create"
  "Major mode for creating a new issue.

\\{shipit-issue-create-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-issue-create--refresh)
  (setq-local shipit-issue-create--cached-options (make-hash-table :test 'eq))
  (add-hook 'kill-buffer-hook #'shipit-issue-create--save-draft nil t))

;;; Buffer name

(defun shipit-issue-create--buffer-name (repo)
  "Return buffer name for issue creation in REPO."
  (format "*shipit-issue-create: %s*" repo))

;;; Section rendering

(defun shipit-issue-create--insert-header ()
  "Insert the header banner."
  (magit-insert-section (issue-create-header)
    (insert (shipit--get-notification-source-icon shipit-issue-create--backend-id)
            " "
            (propertize "NEW ISSUE" 'face 'magit-section-heading) "\n")
    (insert (propertize (format "   %s  |  C-c C-c to create  |  RET to edit field  |  q to quit"
                                shipit-issue-create--repo)
                        'face 'font-lock-comment-face)
            "\n\n")))

(defun shipit-issue-create--insert-title-section ()
  "Insert the title section."
  (magit-insert-section (issue-create-title)
    (let ((start (point)))
      (magit-insert-heading
        (format "%s %s: %s"
                (shipit--get-pr-field-icon "pull-request" "📋")
                (propertize "Title" 'face 'magit-section-heading)
                (if shipit-issue-create--title
                    (propertize shipit-issue-create--title 'face 'magit-branch-local)
                  (propertize "(click to set)" 'face 'font-lock-comment-face))))
      (add-text-properties start (point)
                           '(shipit-issue-create-title t
                             help-echo "RET to edit title")))))

(defun shipit-issue-create--insert-description-section ()
  "Insert the description section."
  (magit-insert-section (issue-create-description nil nil)
    (let ((start (point)))
      (magit-insert-heading
        (format "%s Description %s"
                (shipit--get-pr-field-icon "description" "📝")
                (if (and shipit-issue-create--body
                         (not (string-empty-p shipit-issue-create--body)))
                    ""
                  (propertize "(click to add)" 'face 'font-lock-comment-face))))
      (add-text-properties start (point)
                           '(shipit-issue-create-description t
                             help-echo "RET to edit description")))
    (magit-insert-section-body
      (if (and shipit-issue-create--body
               (not (string-empty-p shipit-issue-create--body)))
          (dolist (line (split-string shipit-issue-create--body "\n"))
            (insert (format "   %s\n" line)))
        (let ((start (point)))
          (insert (propertize "   Click here or press RET to add a description\n"
                              'face 'font-lock-comment-face))
          (add-text-properties start (point)
                               '(shipit-issue-create-description t)))))))

(defun shipit-issue-create--insert-field-section (field)
  "Insert a section for dynamic FIELD descriptor."
  (let* ((name (plist-get field :name))
         (label (plist-get field :label))
         (field-type (plist-get field :type))
         (value (shipit-issue-create--get-field-value name)))
    ;; Skip title and body — they have dedicated sections
    (unless (memq name '(title body))
      (pcase field-type
        ('select (shipit-issue-create--insert-select-field name label value))
        ('multi-select (shipit-issue-create--insert-multi-select-field name label value))))))

(defun shipit-issue-create--insert-select-field (name label value)
  "Insert a select field section for NAME with LABEL and current VALUE."
  (magit-insert-section (issue-create-field)
    (let ((start (point))
          (icon (shipit-issue-create--field-icon name)))
      (insert (format "%s %-12s %s\n"
                      icon label
                      (if value
                          (propertize value 'face 'magit-branch-local)
                        (propertize "(none)" 'face 'font-lock-comment-face))))
      (add-text-properties start (point)
                           `(shipit-issue-create-field ,name
                             help-echo ,(format "RET to select %s" label))))))

(defun shipit-issue-create--insert-multi-select-field (name label values)
  "Insert a multi-select field section for NAME with LABEL and VALUES list."
  (let ((hidden (not (memq name shipit-issue-create--expanded-fields))))
    (magit-insert-section (issue-create-field nil hidden)
      (let ((start (point))
            (icon (shipit-issue-create--field-icon name))
            (count (length values)))
        (magit-insert-heading
          (format "%s %s (%d)" icon label count))
        (add-text-properties start (point)
                             `(shipit-issue-create-field ,name
                               help-echo ,(format "RET to edit %s" label))))
      (magit-insert-section-body
        (if values
            (dolist (val values)
              (insert (format "   %s\n" val)))
          (let ((start (point)))
            (insert (propertize (format "   Press RET to add %s\n"
                                        (downcase label))
                                'face 'font-lock-comment-face))
            (add-text-properties start (point)
                                 `(shipit-issue-create-field ,name))))))))

(defun shipit-issue-create--field-icon (name)
  "Return an icon string for field NAME."
  (pcase name
    ('labels (shipit--get-pr-field-icon "labels" "🏷️"))
    ('assignees (shipit--get-pr-field-icon "assignees" "👤"))
    ('issue-type (shipit--get-pr-field-icon "type" "📌"))
    ('components (shipit--get-pr-field-icon "components" "🧩"))
    (_ (shipit--get-pr-field-icon "default" "📎"))))

;;; Field value accessors

(defun shipit-issue-create--get-field-value (name)
  "Get the current value for field NAME from buffer-local state."
  (pcase name
    ('title shipit-issue-create--title)
    ('body shipit-issue-create--body)
    ('labels shipit-issue-create--labels)
    ('assignees shipit-issue-create--assignees)
    ('issue-type shipit-issue-create--issue-type)
    ('components shipit-issue-create--components)))

(defun shipit-issue-create--set-field-value (name value)
  "Set the current VALUE for field NAME in buffer-local state."
  (pcase name
    ('title (setq-local shipit-issue-create--title value))
    ('body (setq-local shipit-issue-create--body value))
    ('labels (setq-local shipit-issue-create--labels value))
    ('assignees (setq-local shipit-issue-create--assignees value))
    ('issue-type (setq-local shipit-issue-create--issue-type value))
    ('components (setq-local shipit-issue-create--components value))))

;;; DWIM handler

(defun shipit-issue-create--dwim ()
  "Do-what-I-mean for RET in issue creation buffer.
Dispatches to edit-title, edit-description, or edit-field based on text properties."
  (interactive)
  (let ((field-name (get-text-property (point) 'shipit-issue-create-field)))
    (shipit--debug-log "Issue create DWIM: point=%d title=%s desc=%s field=%s"
                       (point)
                       (get-text-property (point) 'shipit-issue-create-title)
                       (get-text-property (point) 'shipit-issue-create-description)
                       field-name)
    (cond
     ((get-text-property (point) 'shipit-issue-create-title)
      (shipit-issue-create--edit-title))
     ((get-text-property (point) 'shipit-issue-create-description)
      (shipit-issue-create--edit-description))
     (field-name
      (shipit-issue-create--edit-field field-name))
     (t
      (shipit--debug-log "Issue create DWIM: no text property at point, toggling section")
      (magit-section-toggle (magit-current-section))))))

;;; Field editors

(defun shipit-issue-create--edit-title ()
  "Edit the issue title via minibuffer."
  (interactive)
  (let ((new-title (read-string "Issue title: "
                                shipit-issue-create--title)))
    (setq-local shipit-issue-create--title
                (if (string-empty-p new-title) nil new-title))
    (shipit-issue-create--refresh)))

(defun shipit-issue-create--edit-description ()
  "Edit the issue description via the shipit editor."
  (interactive)
  (let ((source-buffer (current-buffer)))
    (shipit-editor-open
     (list :type 'preview-description
           :source-buffer source-buffer
           :initial-content (or shipit-issue-create--body "")
           :repo shipit-issue-create--repo
           :on-save (lambda (content)
                      (setq shipit-issue-create--body content)
                      (shipit-issue-create--refresh))))))

(defun shipit-issue-create--edit-field (field-name)
  "Edit the dynamic field FIELD-NAME."
  (let ((field (seq-find (lambda (f) (eq (plist-get f :name) field-name))
                         shipit-issue-create--field-descriptors)))
    (unless field
      (user-error "Unknown field: %s" field-name))
    (let ((field-type (plist-get field :type)))
      (pcase field-type
        ('select (shipit-issue-create--edit-select-field field))
        ('multi-select (shipit-issue-create--edit-multi-select-field field))))))

(defun shipit-issue-create--fetch-options (field)
  "Fetch and cache options for FIELD descriptor."
  (let* ((name (plist-get field :name))
         (fetch-fn (plist-get field :fetch-options))
         (cached (gethash name shipit-issue-create--cached-options 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((options (when fetch-fn
                       (message "Fetching %s..." (plist-get field :label))
                       (funcall fetch-fn shipit-issue-create--config))))
        (puthash name options shipit-issue-create--cached-options)
        options))))

(defun shipit-issue-create--edit-select-field (field)
  "Edit a select FIELD using completing-read."
  (let* ((name (plist-get field :name))
         (label (plist-get field :label))
         (options (shipit-issue-create--fetch-options field))
         (current (shipit-issue-create--get-field-value name)))
    (if (null options)
        (let ((val (read-string (format "%s: " label) current)))
          (shipit-issue-create--set-field-value
           name (if (string-empty-p val) nil val)))
      (let ((selected (completing-read (format "%s: " label)
                                       options nil t nil nil current)))
        (shipit-issue-create--set-field-value
         name (if (string-empty-p selected) nil selected))))
    (shipit-issue-create--refresh)))

(defun shipit-issue-create--edit-multi-select-field (field)
  "Edit a multi-select FIELD using toggle-style completing-read.
Each RET toggles an item and reopens the list.  C-g finishes selection."
  (let* ((name (plist-get field :name))
         (label (plist-get field :label))
         (options (shipit-issue-create--fetch-options field))
         (current (or (shipit-issue-create--get-field-value name) '())))
    ;; Mark this field expanded so refresh keeps it open
    (cl-pushnew name shipit-issue-create--expanded-fields)
    (if (null options)
        ;; No options available — read comma-separated values
        (let* ((current-str (string-join current ", "))
               (input (read-string (format "%s (comma-separated): " label) current-str)))
          (setq current (when (not (string-empty-p input))
                          (mapcar #'string-trim (split-string input ",")))))
      ;; Toggle loop: RET toggles, C-g finishes
      (condition-case nil
          (while t
            (let* ((choices (shipit-issue-create--build-toggle-choices options current))
                   (choice (completing-read
                            (format "Toggle %s (•=selected, C-g to finish): " label)
                            (mapcar #'car choices) nil t))
                   (value (cdr (assoc choice choices))))
              (when value
                (if (member value current)
                    (setq current (delete value current))
                  (push value current))
                (shipit-issue-create--set-field-value name current)
                (shipit-issue-create--refresh))))
        (quit nil)))
    (shipit-issue-create--set-field-value name current)
    (shipit-issue-create--refresh)))

(defun shipit-issue-create--build-toggle-choices (options current)
  "Build toggle-style choice alist from OPTIONS with CURRENT selections marked."
  (mapcar (lambda (opt)
            (cons (format "%s%s"
                          (if (member opt current) "• " "  ")
                          opt)
                  opt))
          options))

;;; Build fields alist

(defun shipit-issue-create--build-fields-alist ()
  "Build an alist of field values from buffer-local state.
Includes only fields that have values set."
  (let ((alist nil))
    (when shipit-issue-create--title
      (push (cons 'title shipit-issue-create--title) alist))
    (when shipit-issue-create--body
      (push (cons 'body shipit-issue-create--body) alist))
    (when shipit-issue-create--labels
      (push (cons 'labels shipit-issue-create--labels) alist))
    (when shipit-issue-create--assignees
      (push (cons 'assignees shipit-issue-create--assignees) alist))
    (when shipit-issue-create--issue-type
      (push (cons 'issue-type shipit-issue-create--issue-type) alist))
    (when shipit-issue-create--components
      (push (cons 'components shipit-issue-create--components) alist))
    (nreverse alist)))

;;; Submit

(defun shipit-issue-create--submit ()
  "Submit the issue via the active backend."
  (interactive)
  (unless shipit-issue-create--title
    (user-error "Title is required"))
  (let ((fields (shipit-issue-create--build-fields-alist)))
    (when (yes-or-no-p (format "Create issue \"%s\"? " shipit-issue-create--title))
      (condition-case err
          (let* ((repo shipit-issue-create--repo)
                 (result (shipit-issues--create-issue-extended repo fields))
                 (number (cdr (assq 'number result))))
            (unless number
              (error "No issue number returned — creation may have failed"))
            (shipit-issue-create--clear-draft)
            ;; Remove save-draft hook before killing to avoid re-saving
            (remove-hook 'kill-buffer-hook #'shipit-issue-create--save-draft t)
            (message "Created issue %s" number)
            (kill-buffer (current-buffer))
            (shipit-issues-open-buffer number repo))
        (error
         (message "Issue creation failed: %s" (error-message-string err)))))))

;;; Refresh

(defun shipit-issue-create--refresh (&optional _ignore-auto _noconfirm)
  "Refresh the issue creation buffer."
  (interactive)
  (let ((saved-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (shipit-issue-create-root)
      (shipit-issue-create--insert-header)
      (shipit-issue-create--insert-title-section)
      (shipit-issue-create--insert-description-section)
      ;; Dynamic fields from backend
      (dolist (field shipit-issue-create--field-descriptors)
        (shipit-issue-create--insert-field-section field)))
    (goto-char (min saved-point (point-max)))))

;;; Quit

(defun shipit-issue-create--quit ()
  "Quit the issue creation buffer, saving draft state."
  (interactive)
  (shipit-issue-create--save-draft)
  (quit-window t))

;;; Entry point

;;;###autoload
(defun shipit-issue-create-buffer ()
  "Open a rich issue creation buffer for the current repository."
  (interactive)
  (let* ((repo (shipit--get-repo-from-remote))
         (_ (unless repo (user-error "Could not determine repository from remote")))
         (resolved (shipit-issue--resolve-for-repo repo))
         (backend-plist (car resolved))
         (config (cdr resolved))
         (backend-id (or (plist-get config :backend) shipit-issue-backend))
         (fields (shipit-issue--creation-fields backend-plist config))
         (buf-name (shipit-issue-create--buffer-name repo)))
    (with-current-buffer (get-buffer-create buf-name)
      (shipit-issue-create-mode)
      (setq shipit-issue-create--repo repo
            shipit-issue-create--backend-id backend-id
            shipit-issue-create--config config
            shipit-issue-create--field-descriptors fields)
      (shipit-issue-create--restore-draft)
      (shipit-issue-create--refresh))
    (switch-to-buffer buf-name)))

(provide 'shipit-issue-create)
;;; shipit-issue-create.el ends here
