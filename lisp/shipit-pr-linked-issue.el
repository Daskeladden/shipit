;;; shipit-pr-linked-issue.el --- Link PRs to tracker issues -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Adds a collapsible "Issue" section to the PR buffer showing the
;; tracker issue associated with the PR.  Auto-detects by scanning the
;; branch name through the active issue backend's reference patterns.
;; RET/shipit-dwim on the heading opens a transient menu with actions:
;; search & link, clear, open the issue buffer, browse externally,
;; copy URL.  Manual overrides are stored locally per (repo, pr-number)
;; and survive across sessions.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'transient)
(require 'shipit-core)
(require 'shipit-issue-backends)

(declare-function shipit-issues--dynamic-fetch "shipit-issues")
(declare-function shipit-issues--build-candidates "shipit-issues")
(declare-function shipit-issues--sort-candidates "shipit-issues")
(declare-function shipit-issues--annotate "shipit-issues")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-buffer-refresh "shipit-buffer")
(declare-function shipit-issues--get-transitions "shipit-issues")
(declare-function shipit-issues--transition-status "shipit-issues")
(declare-function shipit-issue-create-buffer "shipit-issue-create")
(declare-function shipit-editor-open "shipit-editor")

(defvar-local shipit-pr-linked-issue--pending-link nil
  "Plist holding PR link context inside an issue-creation buffer.
When a creation buffer is opened via `shipit-pr-linked-issue-create-new',
the PR's \(:repo :pr-number\) is stored buffer-locally here.  The
`shipit-issue-created-hook' handler — which fires inside the creation
buffer during submit — reads it from there.

Buffer-local scoping makes the handoff immune to unrelated issue
creations in other buffers.")

;;; Storage

(defcustom shipit-pr-linked-issue-file
  (expand-file-name "shipit-pr-linked-issues.el" user-emacs-directory)
  "File storing manual PR-to-issue link overrides.
Format is an alist keyed by repo string, each value an alist of
\(PR-NUMBER-STRING . ISSUE-KEY-STRING)."
  :type 'file
  :group 'shipit)

(defvar shipit-pr-linked-issue--overrides nil
  "In-memory override table, loaded lazily from `shipit-pr-linked-issue-file'.")

(defvar shipit-pr-linked-issue--loaded nil
  "Non-nil once overrides have been read from disk.")

(defun shipit-pr-linked-issue--load ()
  "Load overrides from `shipit-pr-linked-issue-file' into memory."
  (unless shipit-pr-linked-issue--loaded
    (setq shipit-pr-linked-issue--overrides
          (when (file-readable-p shipit-pr-linked-issue-file)
            (with-temp-buffer
              (insert-file-contents shipit-pr-linked-issue-file)
              (goto-char (point-min))
              (condition-case _ (read (current-buffer)) (error nil)))))
    (setq shipit-pr-linked-issue--loaded t)))

(defun shipit-pr-linked-issue--save ()
  "Write in-memory overrides back to `shipit-pr-linked-issue-file'."
  (with-temp-file shipit-pr-linked-issue-file
    (let ((print-level nil)
          (print-length nil))
      (prin1 shipit-pr-linked-issue--overrides (current-buffer)))))

(defun shipit-pr-linked-issue--get-override (repo pr-number)
  "Return the manually-overridden issue key for REPO PR-NUMBER, or nil."
  (shipit-pr-linked-issue--load)
  (let* ((repo-entry (assoc repo shipit-pr-linked-issue--overrides))
         (pr-key (format "%s" pr-number)))
    (cdr (assoc pr-key (cdr repo-entry)))))

(defun shipit-pr-linked-issue--set-override (repo pr-number issue-key)
  "Set ISSUE-KEY as the manual link for REPO PR-NUMBER.
A nil or empty ISSUE-KEY removes any existing override."
  (shipit-pr-linked-issue--load)
  (let* ((pr-key (format "%s" pr-number))
         (clear (or (null issue-key) (string-empty-p issue-key)))
         (repo-entry (assoc repo shipit-pr-linked-issue--overrides)))
    (if repo-entry
        (let ((pr-entry (assoc pr-key (cdr repo-entry))))
          (cond
           (clear
            (setcdr repo-entry (assoc-delete-all pr-key (cdr repo-entry))))
           (pr-entry
            (setcdr pr-entry issue-key))
           (t
            (setcdr repo-entry (cons (cons pr-key issue-key) (cdr repo-entry))))))
      (unless clear
        (push (cons repo (list (cons pr-key issue-key)))
              shipit-pr-linked-issue--overrides))))
  (shipit-pr-linked-issue--save))

;;; Detection

(defun shipit-pr-linked-issue--extract-from-text (text repo)
  "Scan TEXT for the first issue reference per the active backend for REPO.
Returns the raw key string, or nil."
  (when (and text (stringp text) (not (string-empty-p text)))
    (let ((patterns (ignore-errors
                      (shipit-issue--all-reference-patterns repo)))
          (found nil))
      (dolist (entry patterns)
        (unless found
          (let* ((regex (nth 1 entry))
                 (group (or (nth 2 entry) 1))
                 (extractor (or (nth 3 entry) #'identity)))
            (when (string-match regex text)
              (setq found (funcall extractor (match-string group text)))))))
      found)))

(defun shipit-pr-linked-issue--detect (repo pr-number pr-data)
  "Return the issue key linked to REPO PR-NUMBER via override or auto-detect.
Prefers manual override over auto-detection from the PR's branch name.
PR-DATA is the PR alist (expects `head.ref' for branch name)."
  (or (shipit-pr-linked-issue--get-override repo pr-number)
      (shipit-pr-linked-issue--extract-from-text
       (cdr (assq 'ref (cdr (assq 'head pr-data))))
       repo)))

;;; Issue detail fetch (cached per session)

(defvar shipit-pr-linked-issue--details-cache (make-hash-table :test 'equal)
  "Session cache of fetched issue alists keyed by \"repo|key\".")

(defun shipit-pr-linked-issue--fetch-details (repo issue-key)
  "Fetch the normalized issue alist for ISSUE-KEY in REPO, cached for session."
  (let ((cache-key (format "%s|%s" repo issue-key)))
    (or (gethash cache-key shipit-pr-linked-issue--details-cache)
        (let* ((resolved (shipit-issue--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (fetch-fn (plist-get backend :fetch-issue))
               (issue (when fetch-fn
                        (condition-case err
                            (funcall fetch-fn config issue-key)
                          (error
                           (shipit--debug-log
                            "Linked issue fetch failed for %s: %s"
                            issue-key (error-message-string err))
                           nil)))))
          (when issue
            (puthash cache-key issue shipit-pr-linked-issue--details-cache)
            issue)))))

(defun shipit-pr-linked-issue--invalidate-cache (repo issue-key)
  "Drop the cached details for ISSUE-KEY in REPO."
  (remhash (format "%s|%s" repo issue-key)
           shipit-pr-linked-issue--details-cache))

;;; Interactive issue picker

(defcustom shipit-pr-linked-issue-initial-search-args
  '("--assignee=@me" "--state=open" "--limit=50")
  "Args passed to the issue backend's :search when opening the linker.
Pre-seeds the completion buffer with the user's most relevant issues
before dynamic search kicks in as they type."
  :type '(repeat string)
  :group 'shipit)

(defun shipit-pr-linked-issue--initial-results (backend config)
  "Fetch an initial result list for the linker completion buffer.
Returns nil on failure — the picker still works via dynamic search."
  (let ((search-fn (plist-get backend :search)))
    (when search-fn
      (condition-case err
          (funcall search-fn config shipit-pr-linked-issue-initial-search-args)
        (error
         (shipit--debug-log "Linker initial search failed: %s"
                            (error-message-string err))
         nil)))))

(defun shipit-pr-linked-issue--read-issue-key (repo)
  "Prompt for an issue key via dynamic search over REPO's issue backend.
Returns the selected key string, or nil on abort/empty input."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (lookup (make-hash-table :test 'equal))
         (search-cache (make-hash-table :test 'equal))
         (initial-results (shipit-pr-linked-issue--initial-results
                           backend config))
         (all-candidates (if initial-results
                             (shipit-issues--build-candidates
                              initial-results repo lookup)
                           nil))
         (completion-fn
          (lambda (string predicate action)
            (cond
             ((eq action 'metadata)
              `(metadata
                (display-sort-function . shipit-issues--sort-candidates)
                (cycle-sort-function . identity)
                (annotation-function . shipit-issues--annotate)
                (category . shipit-issue)))
             (t
              (when (and backend
                         (>= (length string) 1)
                         (not (gethash string search-cache)))
                (puthash string t search-cache)
                (let ((results (shipit-issues--dynamic-fetch
                                backend config string)))
                  (when results
                    (let ((new (shipit-issues--build-candidates
                                results repo lookup)))
                      (dolist (c new)
                        (unless (member c all-candidates)
                          (push c all-candidates)))))))
              (complete-with-action action all-candidates string predicate))))))
    (let ((selected (completing-read
                     "Link issue (type to search, blank to abort): "
                     completion-fn nil nil)))
      (cond
       ((or (null selected) (string-empty-p selected)) nil)
       (t (let ((info (gethash selected lookup)))
            (if info
                (format "%s" (plist-get info :number))
              selected)))))))

;;; Rendering

(defun shipit-pr-linked-issue--icon (state)
  "Return the icon for a linked issue with STATE (may be nil)."
  (let ((type (cond
               ((and state (string-match-p
                            "\\(closed\\|done\\|resolved\\|completed\\)"
                            (downcase state)))
                "issue-closed")
               (state "issue-open")
               (t "linked-issue"))))
    (shipit--get-pr-field-icon type "🎫")))

(defun shipit-pr-linked-issue--format-heading (repo pr-number issue-key)
  "Heading string for ISSUE-KEY (may be nil) in REPO PR-NUMBER."
  (if issue-key
      (let* ((issue (shipit-pr-linked-issue--fetch-details repo issue-key))
             (title (cdr (assq 'title issue)))
             (state (cdr (assq 'state issue)))
             (icon (shipit-pr-linked-issue--icon state))
             (overridden (shipit-pr-linked-issue--get-override repo pr-number))
             (tag (if overridden
                      (propertize " [manual]"
                                  'face 'font-lock-comment-face)
                    "")))
        (format "%s Issue:     %s%s%s%s"
                icon
                (propertize issue-key 'face 'link)
                (if title
                    (concat " — "
                            (propertize title 'face 'magit-section-heading))
                  "")
                (if state
                    (concat "  "
                            (propertize (format "[%s]" state) 'face 'bold))
                  "")
                tag))
    (format "%s Issue:     %s"
            (shipit--get-pr-field-icon "linked-issue" "🎫")
            (propertize "— RET to link"
                        'face 'font-lock-comment-face))))

(defun shipit-pr-linked-issue--face (value face)
  "Return VALUE (string) propertized with FACE, or VALUE unchanged if nil/empty."
  (if (and value (stringp value) (not (string-empty-p value)))
      (propertize value 'face face)
    value))

(defun shipit-pr-linked-issue--render-typed-field (repo kind value)
  "Render VALUE (issue type or priority) with icon + coloured text.
KIND is `type' or `priority'.  Dispatches through backend plist keys
\(:issue-type-icon-render / :priority-icon-render and
 :issue-type-color / :priority-color\).  Backends without these keys
fall back to plain `magit-dimmed' text with no icon."
  (if (or (null value) (not (stringp value)) (string-empty-p value))
      value
    (let* ((resolved (shipit-issue--resolve-for-repo repo))
           (backend (car resolved))
           (render-key (if (eq kind 'type)
                           :issue-type-icon-render
                         :priority-icon-render))
           (color-key (if (eq kind 'type)
                          :issue-type-color
                        :priority-color))
           (render-fn (plist-get backend render-key))
           (color-fn (plist-get backend color-key))
           (icon (when render-fn (funcall render-fn value)))
           (color (when color-fn (funcall color-fn value)))
           (text (if color
                     (propertize value 'face `(:foreground ,color :weight bold))
                   (propertize value 'face 'magit-dimmed))))
      (if (and icon (stringp icon) (not (string-empty-p icon)))
          (concat icon " " text)
        text))))

(defun shipit-pr-linked-issue--insert-field (label value &optional extra-props)
  "Insert an aligned LABEL / VALUE line in a section body.
When EXTRA-PROPS is non-nil it is a plist applied to the inserted
region via `add-text-properties'."
  (when (and value (not (string-empty-p (format "%s" value))))
    (let ((start (point)))
      (insert (format "   %-12s %s\n"
                      (format "%s:" label)
                      value))
      (when extra-props
        (add-text-properties start (point) extra-props)))))

(defun shipit-pr-linked-issue--insert-description (body)
  "Insert the issue BODY as a collapsed `pr-linked-issue-description' subsection.
Does nothing when BODY is empty."
  (when (and body (stringp body) (not (string-empty-p body)))
    (magit-insert-section (pr-linked-issue-description nil t)
      (magit-insert-heading
        (propertize "   Description" 'face 'magit-section-heading))
      (magit-insert-section-body
        (dolist (line (split-string body "\n"))
          (insert "     " line "\n"))))))

(defun shipit-pr-linked-issue--insert-body (repo issue-key)
  "Insert detail lines for the linked ISSUE-KEY."
  (let ((issue (shipit-pr-linked-issue--fetch-details repo issue-key)))
    (if (null issue)
        (insert (propertize "   (could not fetch details)\n"
                            'face 'font-lock-comment-face))
      (let* ((resolved (shipit-issue--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (browse-fn (plist-get backend :browse-url))
             (url (when browse-fn (funcall browse-fn config issue-key)))
             (user (cdr (assq 'login (cdr (assq 'user issue)))))
             (assignee (cdr (assq 'login
                                  (car (cdr (assq 'assignees issue))))))
             (created (cdr (assq 'created_at issue)))
             (updated (cdr (assq 'updated_at issue)))
             (body (cdr (assq 'body issue))))
        (shipit-pr-linked-issue--insert-field
         "Type" (shipit-pr-linked-issue--render-typed-field
                 repo 'type (cdr (assq 'issue-type issue))))
        (shipit-pr-linked-issue--insert-field
         "Priority" (shipit-pr-linked-issue--render-typed-field
                     repo 'priority (cdr (assq 'priority issue))))
        (shipit-pr-linked-issue--insert-field
         "Status" (shipit-pr-linked-issue--face
                   (cdr (assq 'state issue)) 'bold)
         '(shipit-pr-linked-issue-status t))
        (shipit-pr-linked-issue--insert-field
         "Reporter" (shipit-pr-linked-issue--face
                     user 'shipit-username-face))
        (shipit-pr-linked-issue--insert-field
         "Assignee" (shipit-pr-linked-issue--face
                     assignee 'shipit-username-face))
        (shipit-pr-linked-issue--insert-field
         "Created" (shipit-pr-linked-issue--face
                    (when created
                      (if (fboundp 'shipit--format-timestamp)
                          (shipit--format-timestamp created) created))
                    'shipit-timestamp-face))
        (shipit-pr-linked-issue--insert-field
         "Updated" (shipit-pr-linked-issue--face
                    (when updated
                      (if (fboundp 'shipit--format-timestamp)
                          (shipit--format-timestamp updated) updated))
                    'shipit-timestamp-face))
        (shipit-pr-linked-issue--insert-field
         "URL" (shipit-pr-linked-issue--face url 'link))
        (shipit-pr-linked-issue--insert-description body)))))

(defun shipit-pr-linked-issue--insert-section (repo pr-number pr-data)
  "Insert the Issue section for REPO PR-NUMBER using PR-DATA.
Skips the section when no issue backend is available for REPO."
  (when (ignore-errors (car (shipit-issue--resolve-for-repo repo)))
    (let* ((issue-key (shipit-pr-linked-issue--detect repo pr-number pr-data))
           (heading (shipit-pr-linked-issue--format-heading
                     repo pr-number issue-key))
           (section-props `(shipit-pr-linked-issue t
                            shipit-pr-linked-issue-key ,issue-key
                            shipit-pr-linked-issue-repo ,repo
                            shipit-pr-linked-issue-pr-number ,pr-number)))
      (magit-insert-section (pr-linked-issue issue-key t)
        (let ((heading-start (point)))
          (magit-insert-heading heading)
          (add-text-properties heading-start (point) section-props))
        (when issue-key
          (magit-insert-section-body
            (let ((body-start (point)))
              (shipit-pr-linked-issue--insert-body repo issue-key)
              ;; Propagate context so DWIM handlers match anywhere in the
              ;; body too.  `add-text-properties' merges, so per-line
              ;; specific flags (e.g. status) are preserved.
              (add-text-properties body-start (point) section-props))))))))

;;; Context helpers for transient actions

(defun shipit-pr-linked-issue--context ()
  "Return plist of `:repo', `:pr-number', `:issue-key' at point, or nil."
  (let ((repo (or (get-text-property
                   (point) 'shipit-pr-linked-issue-repo)
                  (bound-and-true-p shipit-buffer-repo)))
        (pr-number (or (get-text-property
                        (point) 'shipit-pr-linked-issue-pr-number)
                       (bound-and-true-p shipit-buffer-pr-number))))
    (when (and repo pr-number)
      (list :repo repo
            :pr-number pr-number
            :issue-key (get-text-property
                        (point) 'shipit-pr-linked-issue-key)))))

;;; Transient actions

(defun shipit-pr-linked-issue--refresh-buffer ()
  "Re-render the current PR buffer."
  (when (fboundp 'shipit-buffer-refresh)
    (shipit-buffer-refresh)))

(defun shipit-pr-linked-issue-search ()
  "Prompt for an issue via dynamic search and link it to the current PR."
  (interactive)
  (let ((ctx (shipit-pr-linked-issue--context)))
    (unless ctx (user-error "Not on a PR linked-issue section"))
    (let* ((repo (plist-get ctx :repo))
           (pr-number (plist-get ctx :pr-number))
           (selected (shipit-pr-linked-issue--read-issue-key repo)))
      (when selected
        (shipit-pr-linked-issue--set-override repo pr-number selected)
        (shipit-pr-linked-issue--invalidate-cache repo selected)
        (message "Linked PR #%s to %s" pr-number selected)
        (shipit-pr-linked-issue--refresh-buffer)))))

(defun shipit-pr-linked-issue-clear ()
  "Clear the manual override; auto-detection takes over on next render."
  (interactive)
  (let ((ctx (shipit-pr-linked-issue--context)))
    (unless ctx (user-error "Not on a PR linked-issue section"))
    (let ((repo (plist-get ctx :repo))
          (pr-number (plist-get ctx :pr-number)))
      (if (shipit-pr-linked-issue--get-override repo pr-number)
          (progn
            (shipit-pr-linked-issue--set-override repo pr-number nil)
            (message "Cleared manual issue link")
            (shipit-pr-linked-issue--refresh-buffer))
        (message "No manual override set")))))

(defun shipit-pr-linked-issue--require-link ()
  "Return (REPO . KEY) for the linked issue at point.
Signals `user-error' if not on a linked-issue section or no issue is
currently linked."
  (let* ((ctx (shipit-pr-linked-issue--context))
         (_ (unless ctx (user-error "Not on a PR linked-issue section")))
         (repo (plist-get ctx :repo))
         (key (plist-get ctx :issue-key)))
    (unless key (user-error "No linked issue"))
    (cons repo key)))

(defun shipit-pr-linked-issue--require-url ()
  "Return the external browse URL for the linked issue at point.
Signals `user-error' when there is no link or the backend does not
expose a `:browse-url' function."
  (let* ((rk (shipit-pr-linked-issue--require-link))
         (repo (car rk))
         (key (cdr rk))
         (resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (browse-fn (plist-get backend :browse-url)))
    (unless browse-fn (user-error "Backend has no :browse-url"))
    (funcall browse-fn config key)))

(defun shipit-pr-linked-issue-open ()
  "Open the linked issue in a shipit issue buffer."
  (interactive)
  (let ((rk (shipit-pr-linked-issue--require-link)))
    (require 'shipit-issues-buffer)
    (shipit-issues-open-buffer (cdr rk) (car rk))))

(defun shipit-pr-linked-issue-browse ()
  "Open the linked issue in an external browser."
  (interactive)
  (browse-url (shipit-pr-linked-issue--require-url)))

(defun shipit-pr-linked-issue-copy-url ()
  "Copy the linked issue's external URL to the kill ring."
  (interactive)
  (let ((url (shipit-pr-linked-issue--require-url)))
    (kill-new url)
    (message "Copied: %s" url)))

(defun shipit-pr-linked-issue-refresh-details ()
  "Invalidate cache and re-render the PR buffer so details are refetched."
  (interactive)
  (let ((ctx (shipit-pr-linked-issue--context)))
    (unless ctx (user-error "Not on a PR linked-issue section"))
    (let ((key (plist-get ctx :issue-key))
          (repo (plist-get ctx :repo)))
      (when key (shipit-pr-linked-issue--invalidate-cache repo key))
      (shipit-pr-linked-issue--refresh-buffer))))

(defun shipit-pr-linked-issue--handle-created (number repo)
  "Hook handler: link a newly-created issue NUMBER in REPO to a pending PR.
Runs inside the creation buffer (see `shipit-issue-create--submit'),
so `shipit-pr-linked-issue--pending-link' is read buffer-locally —
only the create buffer opened via `shipit-pr-linked-issue-create-new'
for this repo matches.  Unrelated issue creations from other buffers
have no pending link set here and are ignored."
  (let ((pending shipit-pr-linked-issue--pending-link))
    (when (and pending
               (equal (plist-get pending :repo) repo))
      (let ((pr-number (plist-get pending :pr-number))
            (key (format "%s" number)))
        (shipit-pr-linked-issue--set-override repo pr-number key)
        (shipit-pr-linked-issue--invalidate-cache repo key)
        (message "Linked PR #%s to newly created %s" pr-number key)))))

(add-hook 'shipit-issue-created-hook
          #'shipit-pr-linked-issue--handle-created)

(defun shipit-pr-linked-issue-create-new ()
  "Open the issue creation buffer for the PR's repo; auto-link on submit.
Uses the PR's known repo so the issue backend resolved via
`shipit-issue--resolve-for-repo' matches what the PR section uses —
components, issue type, and other backend-specific fields render
correctly.  Stores the PR context buffer-locally in the creation
buffer so the `shipit-issue-created-hook' handler links the new key
only to this PR, not to unrelated issues created elsewhere."
  (interactive)
  (require 'shipit-issue-create)
  (require 'shipit-editor)
  (let ((ctx (shipit-pr-linked-issue--context)))
    (unless ctx (user-error "Not on a PR linked-issue section"))
    (let* ((repo (plist-get ctx :repo))
           (pr-number (plist-get ctx :pr-number))
           (resolved (shipit-issue--resolve-for-repo repo))
           (backend (car resolved)))
      (if (plist-get backend :creation-fields)
          (shipit-issue-create-buffer repo)
        (shipit-editor-open
         (list :type 'create-issue :repo repo)))
      ;; After the creation buffer is set up, `current-buffer' is the
      ;; creation buffer.  Stashing the PR context buffer-locally
      ;; scopes the handoff to this exact buffer, immune to unrelated
      ;; creations elsewhere in the session.
      (setq-local shipit-pr-linked-issue--pending-link
                  (list :repo repo :pr-number pr-number)))))

(defun shipit-pr-linked-issue-transition ()
  "Transition the linked issue to another status via its backend workflow."
  (interactive)
  (require 'shipit-issues)
  (let ((ctx (shipit-pr-linked-issue--context)))
    (unless ctx (user-error "Not on a PR linked-issue section"))
    (let* ((repo (plist-get ctx :repo))
           (key (plist-get ctx :issue-key)))
      (unless key (user-error "No linked issue"))
      (let* ((resolved (shipit-issue--resolve-for-repo repo))
             (backend (car resolved)))
        (unless (shipit-issue--backend-has-transitions-p backend)
          (user-error "Backend does not support status transitions"))
        (let ((transitions (shipit-issues--get-transitions repo key)))
          (if (or (null transitions) (zerop (length transitions)))
              (message "No transitions available for %s" key)
            (let* ((choices (mapcar (lambda (tr) (cdr (assq 'name tr)))
                                    transitions))
                   (choice (completing-read
                            (format "Transition %s to: " key)
                            choices nil t)))
              (when choice
                (let ((tr (cl-find-if (lambda (tr)
                                        (equal choice (cdr (assq 'name tr))))
                                      transitions)))
                  (when tr
                    (shipit-issues--transition-status
                     repo key (cdr (assq 'id tr)))
                    (shipit-pr-linked-issue--invalidate-cache repo key)
                    (message "Transitioned %s to %s" key choice)
                    (shipit-pr-linked-issue--refresh-buffer)))))))))))

;;; Transient menu

(transient-define-prefix shipit-pr-linked-issue-menu ()
  "Actions for the linked issue section."
  [:description
   (lambda ()
     (let* ((ctx (shipit-pr-linked-issue--context))
            (key (and ctx (plist-get ctx :issue-key))))
       (if key
           (format "Linked issue: %s" key)
         "No linked issue")))
   ["Link"
    ("l" "Link to issue..."      shipit-pr-linked-issue-search)
    ("n" "New issue & link"      shipit-pr-linked-issue-create-new)
    ("c" "Clear manual override" shipit-pr-linked-issue-clear)]
   ["Open"
    ("o" "Open issue buffer"     shipit-pr-linked-issue-open)
    ("b" "Browse externally"     shipit-pr-linked-issue-browse)
    ("y" "Copy URL"              shipit-pr-linked-issue-copy-url)]
   ["Status"
    ("t" "Transition status..."  shipit-pr-linked-issue-transition)]
   ["Misc"
    ("g" "Refresh details"       shipit-pr-linked-issue-refresh-details)]])

;;; DWIM registration

(defun shipit-pr-linked-issue--register-dwim ()
  "Register DWIM handlers for the linked-issue section.
Generic handler (menu) is registered first; status-specific (direct
transition) is registered after so it matches first when point is on
the Status line."
  (when (fboundp 'shipit-register-dwim-handler)
    (shipit-register-dwim-handler
     'pr-linked-issue
     (lambda () (get-text-property (point) 'shipit-pr-linked-issue))
     #'shipit-pr-linked-issue-menu)
    (shipit-register-dwim-handler
     'pr-linked-issue-status
     (lambda () (get-text-property (point) 'shipit-pr-linked-issue-status))
     #'shipit-pr-linked-issue-transition)))

(if (featurep 'shipit-pr-actions)
    (shipit-pr-linked-issue--register-dwim)
  (with-eval-after-load 'shipit-pr-actions
    (shipit-pr-linked-issue--register-dwim)))

(provide 'shipit-pr-linked-issue)
;;; shipit-pr-linked-issue.el ends here
