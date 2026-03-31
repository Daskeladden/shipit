;;; shipit-commands.el --- commands module -*- lexical-binding: t; -*-

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
;; commands internals split from monolithic shipit.el

;;; Code:
(eval-when-compile
  (require 'transient))

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'shipit-core)
(require 'shipit-http)     ; For shipit-get-pr-for-branch and other API functions
(require 'shipit-pr-sections)  ; For PR section functions used by commands
(require 'shipit-checks)   ; For shipit--clear-branch-cache
(require 'shipit-worktree) ; For worktree operations

;; Forward declarations for functions from other modules
(declare-function shipit--refresh-after-comment-operation "shipit-pr-actions")
(declare-function shipit--refresh-files-section-only "shipit-file-filter")
(declare-function shipit--in-general-comments-section-p "shipit-diff")
(declare-function shipit--toggle-reaction-interactive "shipit-http")
(declare-function shipit--update-comment-face-colors "shipit-http")
(declare-function magit-git-lines "magit-git")
(declare-function magit-refresh "magit-mode")
(declare-function shipit--display-selected-pr "shipit-pr-search")
(declare-function shipit-mode "shipit-buffer")
(declare-function shipit-open-pr-buffer "shipit-buffer" (pr-number &optional repo backend-id backend-config))
(declare-function shipit-gh-etag-get-json-with-refresh-cache "shipit-gh-etag")
(declare-function shipit--mark-activity-read "shipit-core")
(declare-function shipit-pr--get-backend "shipit-pr-backends")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")
(declare-function shipit--classify-url "shipit-render")
(declare-function shipit-jira-columns-transient "shipit-issue-jira")
(declare-function shipit--open-classified-url "shipit-render")

;; Forward declarations for variables from other modules
(defvar last-query-parts)
(defvar shipit-inline-comment-highlight-background)
(defvar shipit-inline-comment-left-border)
(defvar shipit-inline-comment-icon-style)
(defvar shipit-inline-comment-faces)
(defvar shipit-inline-comment-username-color)
(defvar shipit-inline-comment-timestamp-color)
(defvar shipit-inline-comment-background-color)
(defvar shipit-comment-wrap-width)
(defvar shipit-render-wrap-column)
(defvar shipit--current-displayed-pr)

(defun shipit-set-repository (repo)
  "Set the current repository to REPO (format: owner/repo)."
  (interactive "sRepository (owner/repo): ")
  (setq shipit-current-repo repo)
  (message "Set repository to: %s" repo))

(defun shipit-auto-detect-repository ()
  "Auto-detect repository from git remote."
  (interactive)
  (if-let* ((repo (shipit--ensure-repository)))
      (message "Auto-detected repository: %s" repo)
    (message "Could not auto-detect repository")))

(defun shipit-toggle-thread-fold ()
  "Toggle folding of comment thread at point."
  (interactive)
  (let* ((thread-key (get-text-property (point) 'thread-key))
         (thread-start (get-text-property (point) 'thread-start)))
    (if (and thread-key thread-start)
        (save-excursion
          (goto-char thread-start)
          (let* ((folded (get-text-property (point) 'thread-folded))
                 (line-end (line-end-position))
                 (content-start (next-single-property-change line-end 'thread-content))
                 (content-end (when (and content-start (> content-start 1))
                                (next-single-property-change content-start 'thread-content nil (point-max)))))
            (if content-start
                (progn
                  (setq buffer-read-only nil)
                  (if folded
                      ;; Unfold: show content
                      (progn
                        (remove-overlays content-start content-end 'invisible)
                        (goto-char thread-start)
                        (when (looking-at "▶")
                          (delete-char 1)
                          (insert "▼"))
                        (put-text-property thread-start line-end 'thread-folded nil))
                    ;; Fold: hide content
                    (let ((overlay (make-overlay content-start content-end)))
                      (overlay-put overlay 'invisible t)
                      (goto-char thread-start)
                      (when (looking-at "▼")
                        (delete-char 1)
                        (insert "▶"))
                      (put-text-property thread-start line-end 'thread-folded t)))
                  (setq buffer-read-only t))
              (message "No thread content found"))))
      (message "Not on a thread header"))))

(defun shipit-fold-all-threads ()
  "Fold all comment threads."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (get-text-property (point) 'thread-start)
        (unless (get-text-property (point) 'thread-folded)
          (shipit-toggle-thread-fold)))
      (forward-line 1))))

(defun shipit-unfold-all-threads ()
  "Unfold all comment threads."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (get-text-property (point) 'thread-start)
        (when (get-text-property (point) 'thread-folded)
          (shipit-toggle-thread-fold)))
      (forward-line 1))))

(defun shipit-next-thread ()
  "Navigate to next comment thread."
  (interactive)
  (let ((next-thread (next-single-property-change (point) 'thread-start)))
    (if (and next-thread (> next-thread 1))
        (goto-char next-thread)
      (message "No more threads"))))

(defun shipit-previous-thread ()
  "Navigate to previous comment thread."
  (interactive)
  (let ((prev-thread (previous-single-property-change (point) 'thread-start)))
    (if (and prev-thread (> prev-thread 1))
        (goto-char prev-thread)
      (message "No previous threads"))))

(defun shipit--edit-comment-interactive (comment-id current-body)
  "Interactively edit comment with COMMENT-ID, starting with CURRENT-BODY.
Uses the new shipit-editor with live preview and PR reference completion."
  (interactive)
  (let* ((clean-body (shipit--clean-text current-body))
         (pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo))
         (file-path (get-text-property (point) 'shipit-file-path))
         (line-number (get-text-property (point) 'shipit-line-number))
         (is-inline (if file-path t nil))
         (type (if is-inline 'inline-comment 'general-comment)))
    (if (fboundp 'shipit-editor-open)
        (shipit-editor-open
         (list :type type
               :source-buffer (current-buffer)
               :pr-number pr-number
               :repo repo
               :file-path file-path
               :line-number line-number
               :comment-id comment-id
               :initial-content clean-body))
      ;; Fallback to simple read-string if editor not loaded
      (let ((new-body (read-string "Edit comment: " clean-body)))
        (when (not (string-equal new-body clean-body))
          (let* ((comment-type (get-text-property (point) 'shipit-comment-type))
                 (is-review (string= comment-type "review"))
                 (is-inline-comment (and (not (shipit--in-general-comments-section-p)) (not is-review))))
            (shipit--edit-comment comment-id new-body is-inline-comment is-review)
            (shipit--refresh-after-comment-operation is-inline-comment)))))))

(defun shipit-clear-cache ()
  "Clear all shipit caches."
  (interactive)
  (shipit--clear-branch-cache)
  (message "Shipit caches cleared"))

(defun shipit-toggle-debug-logging ()
  "Toggle debug logging on/off."
  (interactive)
  (setq shipit-debug-log-enabled (not shipit-debug-log-enabled))
  (message "Shipit debug logging %s" (if shipit-debug-log-enabled "enabled" "disabled")))

(defun shipit-view-debug-log ()
  "View the shipit debug log file."
  (interactive)
  (if (file-exists-p shipit--debug-log-file)
      (find-file-other-window shipit--debug-log-file)
    (message "No debug log file found at %s" shipit--debug-log-file)))

(defun shipit-clear-debug-log ()
  "Clear the debug log file."
  (interactive)
  (when (file-exists-p shipit--debug-log-file)
    (delete-file shipit--debug-log-file)
    (message "Debug log cleared")))

(defun shipit-enable-profiling-only ()
  "Enable debug logging with only profiling category.
This shows performance timing without other debug messages."
  (interactive)
  (setq shipit-debug-log-enabled t)
  (setq shipit-debug-categories '(profiling))
  (message "Profiling enabled (only timing operations will be logged)"))

(defun shipit-toggle-emoji-variation-selectors ()
  "Toggle emoji variation selector stripping."
  (interactive)
  (setq shipit-strip-emoji-variation-selectors (not shipit-strip-emoji-variation-selectors))
  (message "Emoji variation selector stripping %s" 
           (if shipit-strip-emoji-variation-selectors "enabled" "disabled")))

(defun shipit-toggle-auto-refresh ()
  "Toggle auto-refresh mode."
  (interactive)
  (setq shipit-auto-refresh (not shipit-auto-refresh))
  (message "Auto-refresh %s" (if shipit-auto-refresh "enabled" "disabled")))

(defun shipit-toggle-markdown ()
  "Toggle markdown rendering."
  (interactive)
  (setq shipit-render-markdown (not shipit-render-markdown))
  (message "Markdown rendering %s" (if shipit-render-markdown "enabled" "disabled")))

(defun shipit-toggle-avatars ()
  "Toggle avatar display globally."
  (interactive)
  (setq shipit-show-avatars (not shipit-show-avatars))
  (message "Avatar display %s" (if shipit-show-avatars "enabled" "disabled")))

(defun shipit-toggle-round-avatars ()
  "Toggle rounded avatar corners."
  (interactive)
  (setq shipit-round-avatars (not shipit-round-avatars))
  (message "Rounded avatars %s" (if shipit-round-avatars "enabled" "disabled")))

(defun shipit-cycle-inline-comment-icon-style ()
  "Cycle through inline comment icon styles: emoji → text → avatar."
  (interactive)
  (setq shipit-inline-comment-icon-style
        (pcase shipit-inline-comment-icon-style
          ('emoji 'text)
          ('text 'avatar)
          ('avatar 'emoji)
          (_ 'emoji)))
  (message "Inline comment icon style: %s" shipit-inline-comment-icon-style))

(defun shipit-toggle-inline-comment-faces ()
  "Toggle colored faces for inline comment components (username, timestamp, body)."
  (interactive)
  (setq shipit-inline-comment-faces (not shipit-inline-comment-faces))
  (message "Inline comment colored faces: %s" (if shipit-inline-comment-faces "enabled" "disabled")))

(defun shipit-toggle-preview-default-draft ()
  "Toggle whether new PRs default to draft status in preview mode."
  (interactive)
  (setq shipit-preview-default-draft (not shipit-preview-default-draft))
  (message "Preview default draft: %s" (if shipit-preview-default-draft "enabled" "disabled")))

(defun shipit-toggle-preview-default-assignee ()
  "Toggle default assignee between self and none for preview mode."
  (interactive)
  (setq shipit-preview-default-assignee
        (if (eq shipit-preview-default-assignee 'self) 'none 'self))
  (message "Preview default assignee: %s"
           (if (eq shipit-preview-default-assignee 'self) "yourself" "none")))

(defun shipit-toggle-timestamp-format ()
  "Toggle timestamp format between absolute and relative.
Absolute shows \"2025-12-17 14:52\", relative shows \"2h ago\".
Updates all timestamps in buffer immediately without full refresh."
  (interactive)
  (setq shipit-timestamp-format
        (if (eq shipit-timestamp-format 'absolute) 'relative 'absolute))
  (message "Timestamp format: %s"
           (if (eq shipit-timestamp-format 'absolute)
               "absolute (2025-12-17 14:52)"
             "relative (2h ago)"))
  ;; Update timestamps in-place
  (shipit--update-buffer-timestamps))

(defun shipit-cycle-username-color ()
  "Cycle through username color options."
  (interactive)
  (setq shipit-inline-comment-username-color
        (pcase shipit-inline-comment-username-color
          ("blue" "red")
          ("red" "green")
          ("green" "purple")
          ("purple" "orange")
          ("orange" "cyan")
          (_ "blue")))
  (shipit--update-comment-face-colors)
  (message "Username color: %s" shipit-inline-comment-username-color))

(defun shipit-cycle-timestamp-color ()
  "Cycle through timestamp color options."
  (interactive)
  (setq shipit-inline-comment-timestamp-color
        (pcase shipit-inline-comment-timestamp-color
          ("gray" "blue")
          ("blue" "red")
          ("red" "green")
          ("green" "purple")
          ("purple" "orange")
          (_ "gray")))
  (shipit--update-comment-face-colors)
  (message "Timestamp color: %s" shipit-inline-comment-timestamp-color))

(defun shipit-increase-comment-wrap-width ()
  "Increase the comment wrap width by 5 characters."
  (interactive)
  (setq shipit-comment-wrap-width (min 160 (+ (or shipit-comment-wrap-width 80) 5)))
  (message "Comment wrap width: %d characters" shipit-comment-wrap-width))

(defun shipit-decrease-comment-wrap-width ()
  "Decrease the comment wrap width by 5 characters."
  (interactive)
  (setq shipit-comment-wrap-width (max 40 (- (or shipit-comment-wrap-width 80) 5)))
  (message "Comment wrap width: %d characters" shipit-comment-wrap-width))

;; Ensure <return> (GUI Enter key event) is aliased to RET in transient
;; keymaps, matching how transient already aliases <kp-subtract> to -.
;; Without this, function-key-map translation from <return> to RET may
;; not occur when overriding-terminal-local-map is active.
(advice-add 'transient--make-transient-map :filter-return
            (lambda (map)
              (when-let* ((b (keymap-lookup map "RET")))
                (keymap-set map "<return>" b))
              map)
            '((name . shipit--add-return-alias)))

;; Configuration Menu
(transient-define-prefix shipit-config ()
  "Configure shipit options."
  :man-page "shipit"
  ["Inline Comment Settings"
   ("f" shipit-toggle-inline-comment-faces
    :description (lambda ()
                   (format "Colored faces: %s"
                           (if shipit-inline-comment-faces "enabled" "disabled")))
    :transient t)
   ("u" shipit-cycle-username-color
    :description (lambda ()
                   (format "Username color: %s"
                           (or (and (boundp 'shipit-inline-comment-username-color) shipit-inline-comment-username-color) "blue")))
    :transient t)
   ("t" shipit-cycle-timestamp-color
    :description (lambda ()
                   (format "Timestamp color: %s"
                           (or (and (boundp 'shipit-inline-comment-timestamp-color) shipit-inline-comment-timestamp-color) "gray")))
    :transient t)
   ("+" shipit-increase-comment-wrap-width
    :description (lambda ()
                   (format "Wrap width: %d chars"
                           (or (and (boundp 'shipit-comment-wrap-width) shipit-comment-wrap-width) 80)))
    :transient t)
   ("-" shipit-decrease-comment-wrap-width
    :description (lambda ()
                   (format "Wrap width: %d chars"
                           (or (and (boundp 'shipit-comment-wrap-width) shipit-comment-wrap-width) 80)))
    :transient t)
   ]
  ["Display Options"
   ("m" shipit-toggle-markdown
    :description (lambda ()
                   (format "Markdown rendering: %s"
                           (if shipit-render-markdown "enabled" "disabled")))
    :transient t)
   ("e" shipit-toggle-emoji-variation-selectors
    :description (lambda ()
                   (format "Emoji variation selectors: %s"
                           (if shipit-strip-emoji-variation-selectors "strip" "preserve")))
    :transient t)
   ("a" shipit-toggle-auto-refresh
    :description (lambda ()
                   (format "Auto-refresh: %s"
                           (if shipit-auto-refresh "enabled" "disabled")))
    :transient t)]
  ["Mermaid"
   ("M" shipit-toggle-mermaid
    :description (lambda ()
                   (format "Mermaid rendering: %s"
                           (if shipit-render-mermaid "enabled" "disabled")))
    :transient t)
   ("T" shipit-cycle-mermaid-theme
    :description (lambda ()
                   (format "Theme: %s" shipit-mermaid-theme))
    :transient t)
   ("B" shipit-cycle-mermaid-background
    :description (lambda ()
                   (format "Background: %s" shipit-mermaid-background))
    :transient t)
   (">" shipit-increase-mermaid-max-width
    :description (lambda ()
                   (format "Max width: %d px" shipit-mermaid-max-width))
    :transient t)
   ("<" shipit-decrease-mermaid-max-width
    :description (lambda ()
                   (format "Max width: %d px" shipit-mermaid-max-width))
    :transient t)]
  ["Avatar Settings"
   ("A" shipit-toggle-avatars
    :description (lambda ()
                   (format "Show avatars: %s"
                           (if shipit-show-avatars "enabled" "disabled")))
    :transient t)
   ("r" shipit-toggle-round-avatars
    :description (lambda ()
                   (format "Rounded avatars: %s"
                           (if shipit-round-avatars "enabled" "disabled")))
    :transient t)
   ("s" shipit-cycle-inline-comment-icon-style
    :description (lambda ()
                   (format "Inline comment icons: %s"
                           shipit-inline-comment-icon-style))
    :transient t)]
  ["Preview Settings"
   ("p" shipit-toggle-preview-default-draft
    :description (lambda ()
                   (format "Default draft status: %s"
                           (if shipit-preview-default-draft "draft" "ready for review")))
    :transient t)
   ("a" shipit-toggle-preview-default-assignee
    :description (lambda ()
                   (format "Default assignee: %s"
                           (if (eq shipit-preview-default-assignee 'self) "yourself" "none")))
    :transient t)]
  ["Jira"
   ("j" "Work item columns" shipit-jira-columns-transient)]
  ["Debug"
   ("d" shipit-toggle-debug-logging
    :description (lambda ()
                   (format "Debug logging: %s"
                           (if shipit-debug-log-enabled "enabled" "disabled")))
    :transient t)
   ("v" "View debug log" shipit-view-debug-log)
   ("D" "Configure debug categories" shipit-debug-select-categories)]
  ["Actions"
   ("c" "Clear caches" shipit-clear-cache :transient t)
   ("C" "Clear debug log" shipit-clear-debug-log :transient t)]
  ["Exit"
   ("q" "Quit" transient-quit-one)])

;; Repo search reader for transient

(defun shipit--read-repo-select-backend ()
  "Select a backend for repo search.
If only one backend has `:search-repos-request', use it.
If multiple, ask the user.  Returns (ID . CONFIG) or nil."
  (require 'shipit-pr-github nil t)
  (require 'shipit-pr-gitlab nil t)
  (let ((capable '()))
    (dolist (entry shipit-pr-backends)
      (let* ((id (car entry))
             (plist (cdr entry)))
        (when (plist-get plist :search-repos-request)
          (push (list id (plist-get plist :name)) capable))))
    (shipit--debug-log "Repo search: %d backends with :search-repos-request: %S"
                       (length capable) (mapcar #'car capable))
    (let* ((entry (cond
                   ((null capable) nil)
                   ((= 1 (length capable)) (car capable))
                   (t
                    (let* ((names (mapcar #'cadr capable))
                           (selected (completing-read "Search repos on: " names nil t)))
                      (cl-find selected capable
                               :key #'cadr :test #'string=)))))
           (id (car entry))
           (config (if (eq id shipit-pr-backend)
                       (or shipit-pr-backend-config (list))
                     (list))))
      (shipit--debug-log "Repo search: selected backend %s (active: %s)" id shipit-pr-backend)
      (when id
        (cons id config)))))

(defun shipit--read-repo (prompt &optional _initial-input _history)
  "Read a repository with async API-powered search.
As the user types (>= 3 chars), fires async curl after a 0.5s
debounce.  Results appear when the request completes without
blocking typing.  Refinements of a cached query filter locally."
  (require 'shipit-pr-backends)
  (let* ((backend-info (shipit--read-repo-select-backend))
         (backend-id (car backend-info))
         (config (cdr backend-info)))
    (if (not backend-id)
        (read-string "Repository (owner/repo): ")
      (let* ((backend-plist (cdr (assq backend-id shipit-pr-backends)))
             (request-fn (plist-get backend-plist :search-repos-request))
             (api-query (cons nil nil))      ; (cached-query . nil)
             (api-candidates (cons nil nil)) ; (candidates . nil)
             (name-map (make-hash-table :test 'equal))
             (search-timer nil)
             (search-proc nil)
             (fire-search
              (lambda (query &optional is-fallback self)
                (shipit--debug-log "Repo search: async query %S" query)
                (message "Searching repos for \"%s\"..." query)
                ;; Kill any in-flight search
                (when (and search-proc (process-live-p search-proc))
                  (delete-process search-proc))
                (let* ((req (funcall request-fn config query))
                       (url (nth 0 req))
                       (headers (nth 1 req))
                       (normalizer (nth 2 req))
                       (output-buffer (generate-new-buffer " *shipit-repo-search*"))
                       (curl-args (append
                                   (list "-s" "-S" "--max-time" "5")
                                   (cl-loop for (k . v) in headers
                                            append (list "-H" (format "%s: %s" k v)))
                                   (list url))))
                  (setq search-proc
                        (make-process
                         :name "shipit-repo-search"
                         :buffer output-buffer
                         :command (cons "curl" curl-args)
                         :sentinel
                         (lambda (proc _event)
                           (when (eq (process-status proc) 'exit)
                             (let ((buf (process-buffer proc)))
                               (unwind-protect
                                   (when (and (= 0 (process-exit-status proc))
                                              (buffer-live-p buf))
                                     (with-current-buffer buf
                                       (goto-char (point-min))
                                       (let* ((data (condition-case nil (json-read) (error nil)))
                                              (repos (when data (funcall normalizer data)))
                                              (cands (mapcar
                                                      (lambda (repo)
                                                        (let* ((rname (cdr (assq 'full_name repo)))
                                                               (desc (cdr (assq 'description repo)))
                                                               (display (if (and desc (not (string-empty-p desc)))
                                                                            (format "%s — %s" rname desc)
                                                                          rname)))
                                                          (puthash display rname name-map)
                                                          display))
                                                      repos)))
                                         (shipit--debug-log "Repo search: async got %d results for %S" (length cands) query)
                                         (if (and (null cands)
                                                  (not is-fallback)
                                                  (not (string-match-p "/" query)))
                                             ;; No results — try Groups API fallback
                                             (progn
                                               (shipit--debug-log "Repo search: 0 results, trying Groups API fallback for %S" query)
                                               (funcall self (concat query "/") t self))
                                           ;; Normal path: update candidates and refresh
                                           ;; Cache the original query (sans trailing slash for fallbacks)
                                           ;; so refinement detection works from the user's actual input
                                           (setcar api-query (if (and is-fallback (string-suffix-p "/" query))
                                                                 (substring query 0 -1)
                                                               query))
                                           (setcar api-candidates cands)
                                           (when (active-minibuffer-window)
                                             (run-at-time
                                              0 nil
                                              (lambda ()
                                                (when (active-minibuffer-window)
                                                  (with-selected-window (active-minibuffer-window)
                                                    (when (boundp 'vertico--input)
                                                      (setq vertico--input t))
                                                    (cond
                                                     ((fboundp 'vertico--exhibit)
                                                      (vertico--exhibit))
                                                     ((fboundp 'icomplete-exhibit)
                                                      (icomplete-exhibit))
                                                     (t
                                                      (run-hooks 'post-command-hook))))))))))))
                                 ;; Clean up buffer
                                 (when (buffer-live-p buf)
                                   (kill-buffer buf))))))))))))
        (unwind-protect
            (let ((selected
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (add-hook
                          'post-command-hook
                          (lambda ()
                            (let ((input (minibuffer-contents-no-properties)))
                              (when (>= (length input) 3)
                                (let ((cached-q (car api-query)))
                                  ;; Only skip search if input refines cached query AND
                                  ;; some candidates actually match (avoids stale cache
                                  ;; hiding valid new results from a different API path)
                                  (unless (and cached-q
                                               (>= (length input) (length cached-q))
                                               (string-prefix-p cached-q input t)
                                               (car api-candidates)
                                               (cl-some (lambda (c)
                                                          (string-match-p (regexp-quote input) c))
                                                        (car api-candidates)))
                                    ;; Cancel previous debounce timer
                                    (when search-timer (cancel-timer search-timer))
                                    (let ((q input))
                                      (setq search-timer
                                            (run-with-idle-timer
                                             0.3 nil
                                             (lambda ()
                                               (funcall fire-search q nil fire-search))))))))))
                          nil t))
                     (completing-read
                      (or prompt "Repository: ")
                      (lambda (string predicate action)
                        (if (eq action 'metadata)
                            '(metadata (category . shipit-repo))
                          (complete-with-action action (car api-candidates) string predicate)))))))
              (let ((repo-name (or (gethash selected name-map) selected)))
                (format "%s:%s" backend-id repo-name)))
          ;; Cleanup on exit
          (when search-timer (cancel-timer search-timer))
          (when (and search-proc (process-live-p search-proc))
            (delete-process search-proc)))))))

;; Advanced PR Search with Transient Menu

(transient-define-prefix shipit-advanced-pr-search ()
  "Advanced PR search with multiple filter options."
  :man-page "shipit"
  :value '("--state=open" "--limit=100" "--sort=created")
  ["Presets"
   ("p o" "Open PRs" transient-preset := ("--state=open"))
   ("p m" "My open PRs" transient-preset := ("--state=open" "--author=@me"))
   ("p r" "Needs my review" transient-preset := ("--state=open" "--reviewer=@me"))
   ("p a" "Assigned to me" transient-preset := ("--state=open" "--assignee=@me"))
   ("p d" "My drafts" transient-preset := ("--state=open" "--author=@me" "--draft"))]
  ["Filters"
   ("a" "Author" "--author=" :reader shipit--read-github-user)
   ("A" "Assignee" "--assignee=" :reader shipit--read-github-user)
   ("r" "Review requested" "--reviewer=" :reader shipit--read-github-user)
   ("R" "Team review requested" "--team-reviewer=" :reader read-string)
   ("m" "Milestone" "--milestone=" :reader read-string)
   ("l" "Label" "--label=" :reader read-string)
   ("o" "Repository" "--repo=" :reader shipit--read-repo)]
  ["State & Status"
   ("s" "State" "--state=" :choices ("open" "closed" "all"))
   ("d" "Draft" "--draft")
   ("c" "Conflicts" "--conflicts")]
  ["Text Search"
   ("t" "Title contains" "--title=" :reader read-string)
   ("b" "Body contains" "--body=" :reader read-string)
   ("n" "PR Number" "--number=" :reader read-string)]
  ["Date Range"
   (">" "Created after" "--created-after=" :reader read-string)
   ("<" "Created before" "--created-before=" :reader read-string)
   ("u" "Updated after" "--updated-after=" :reader read-string)
   ("U" "Updated before" "--updated-before=" :reader read-string)]
  ["Options"
   ("S" "Sort by" "--sort=" :choices ("created" "updated" "comments"))
   ("L" "Limit results" "--limit=" :reader transient-read-number-N+)]
  ["Actions"
   ("RET" "Search" shipit--execute-advanced-search)
   ("q" "Quit" transient-quit-one)])

(defun shipit--parse-repo-arg (repo-string)
  "Parse REPO-STRING into (BACKEND-ID . REPO-PATH).
Supports \"backend:owner/repo\" format.  Falls back to the active
backend for plain \"owner/repo\"."
  (if (string-match "\\`\\([a-z]+\\):\\(.+\\)\\'" repo-string)
      (cons (intern (match-string 1 repo-string))
            (match-string 2 repo-string))
    (cons (shipit-pr--backend-id) repo-string)))

(defun shipit--execute-advanced-search (&optional args)
  "Execute advanced PR search with ARGS from transient menu."
  (interactive (list (transient-args 'shipit-advanced-pr-search)))
  (shipit--debug-log "Advanced search raw args: %S" args)
  (let* ((repo-arg (seq-find (lambda (a) (string-prefix-p "--repo=" a)) args))
         (repo-raw (if repo-arg
                       (substring repo-arg 7)
                     (shipit--get-repo-from-remote)))
         (parsed (if repo-raw (shipit--parse-repo-arg repo-raw)))
         (backend-id (car parsed))
         (repo (cdr parsed))
         (filtered-args (seq-remove (lambda (a) (string-prefix-p "--repo=" a)) args)))
    (shipit--debug-log "Repository: %s, backend: %s (override: %s)"
                       repo backend-id (if repo-arg "yes" "no"))
    (if (not repo)
        (message "Could not determine repository from remote")
      (shipit--execute-backend-search backend-id repo filtered-args))))

(defun shipit--transient-args-to-search-plist (args)
  "Convert transient ARGS to a plist for backend :search functions.
Maps --state=, --author=, --label=, --draft to backend-neutral keys."
  (let (plist)
    (dolist (arg args)
      (cond
       ((string-prefix-p "--state=" arg)
        (setq plist (plist-put plist :state (substring arg 8))))
       ((string-prefix-p "--author=" arg)
        (setq plist (plist-put plist :author (substring arg 9))))
       ((string-prefix-p "--label=" arg)
        (setq plist (plist-put plist :label (substring arg 8))))
       ((string= "--draft" arg)
        (setq plist (plist-put plist :draft t)))))
    plist))

(defun shipit--execute-backend-search (backend-id repo args)
  "Search MRs/PRs on BACKEND-ID in REPO using transient ARGS.
Passes raw transient ARGS to the backend's :search function."
  (require 'shipit-pr-backends)
  (let* ((backend-plist (cdr (assq backend-id shipit-pr-backends)))
         (search-fn (plist-get backend-plist :search))
         (config (plist-put
                  (copy-sequence
                   (if (eq backend-id shipit-pr-backend)
                       (or shipit-pr-backend-config (list))
                     (list :project-path repo)))
                  :repo repo))
         (type-label (or (plist-get backend-plist :pr-type-label) "PR")))
    (unless search-fn
      (error "Backend %s has no :search function" backend-id))
    (shipit--debug-log "Backend search: %s repo=%s args=%S" backend-id repo args)
    ;; Store query parts for reactive fetching (backend-provided)
    (setq shipit--current-query-parts
          (let ((build-fn (plist-get backend-plist :build-search-query-parts)))
            (when build-fn (funcall build-fn args repo))))
    (let ((results (funcall search-fn config args)))
      (shipit--debug-log "Backend search: got %d results" (length results))
      (if (and results (> (length results) 0))
          (progn
            (message "Found %d %ss matching criteria" (length results) type-label)
            (shipit--select-pr-from-results results repo backend-id config))
        (message "No %ss found matching the specified criteria" type-label)))))

(defun shipit--resolve-user-placeholder (username)
  "Resolve USERNAME placeholder like @me to actual username."
  (if (string= username "@me")
      (or (shipit--get-current-user)
          (error "Cannot resolve @me: not authenticated"))
    username))

(defvar shipit--cached-user-teams nil
  "Cached list of teams the current user belongs to.
Each entry is a string in org/team-slug format.")

(defun shipit--get-user-teams ()
  "Get list of teams the current user belongs to.
Returns list of strings in org/team-slug format, cached after first call."
  (or shipit--cached-user-teams
      (condition-case err
          (let* ((resolved (shipit-pr--resolve-for-repo
                            (or (shipit--get-repo-from-remote) "")))
                 (backend (car resolved))
                 (config (cdr resolved))
                 (fetch-fn (plist-get backend :fetch-user-teams)))
            (when fetch-fn
              (let ((teams-data (funcall fetch-fn config)))
                (shipit--debug-log "Raw teams API response: %S" teams-data)
                (when (and teams-data (listp teams-data))
                  (setq shipit--cached-user-teams
                        (mapcar (lambda (team)
                                  (let ((org (cdr (assq 'login (cdr (assq 'organization team)))))
                                        (slug (cdr (assq 'slug team))))
                                    (format "%s/%s" org slug)))
                                teams-data))
                  (shipit--debug-log "Fetched user teams: %S" shipit--cached-user-teams)
                  shipit--cached-user-teams))))
        (error
         (shipit--debug-log "Failed to fetch user teams: %s" (error-message-string err))
         nil))))

(defun shipit-show-my-teams ()
  "Display the teams the current user belongs to (for debugging)."
  (interactive)
  (setq shipit--cached-user-teams nil)  ; Clear cache to force refetch
  (let ((teams (shipit--get-user-teams)))
    (if teams
        (message "Your teams: %s" (string-join teams ", "))
      (message "No teams found or error fetching teams"))))

(defun shipit--build-advanced-search-query (args repo)
  "Build search query from transient ARGS for REPO."
  (let ((query-parts (list (format "repo:%s" repo) "is:pr")))
    (shipit--debug-log "Building query from args: %S" args)
    (dolist (arg args)
      (shipit--debug-log "Processing arg: %s" arg)
      (cond
       ((string-prefix-p "--author=" arg)
        (let ((author (substring arg 9)))
          (shipit--debug-log "Extracted author from arg '%s': '%s'" arg author)
          (when (> (length author) 0)
            (let* ((resolved-author (shipit--resolve-user-placeholder author))
                   (clean-author (if (string-match "^[0-9]+\\+\\(.+\\)$" resolved-author)
                                    (match-string 1 resolved-author)
                                  resolved-author))
                   (author-query (format "author:%s" clean-author)))
              (shipit--debug-log "Cleaned author: '%s' -> '%s', author query part: '%s'" author clean-author author-query)
              (push author-query query-parts)))))
       ((string-prefix-p "--assignee=" arg)
        (let ((assignee (substring arg 11)))
          (when (> (length assignee) 0)
            (let* ((resolved-assignee (shipit--resolve-user-placeholder assignee))
                   (clean-assignee (if (string-match "^[0-9]+\\+\\(.+\\)$" resolved-assignee)
                                      (match-string 1 resolved-assignee)
                                    resolved-assignee))
                   (assignee-query (format "assignee:%s" clean-assignee)))
              (push assignee-query query-parts)))))
       ((string-prefix-p "--reviewer=" arg)
        (let ((reviewer (substring arg 11)))
          (when (> (length reviewer) 0)
            (let* ((is-me (string= reviewer "@me"))
                   (resolved-reviewer (shipit--resolve-user-placeholder reviewer))
                   (clean-reviewer (if (string-match "^[0-9]+\\+\\(.+\\)$" resolved-reviewer)
                                      (match-string 1 resolved-reviewer)
                                    resolved-reviewer))
                   ;; Build queries for both pending and completed reviews
                   ;; review-requested: PRs waiting for review
                   ;; reviewed-by: PRs already reviewed (approved, changes requested, or commented)
                   (requested-query (format "review-requested:%s" clean-reviewer))
                   (reviewed-query (format "reviewed-by:%s" clean-reviewer))
                   ;; When @me, also include team review requests with OR
                   (team-queries (when is-me
                                   (mapcar (lambda (team)
                                             (format "team-review-requested:%s" team))
                                           (shipit--get-user-teams))))
                   ;; Combine all queries with OR
                   (all-queries (append (list requested-query reviewed-query) team-queries))
                   (combined-query (format "(%s)"
                                           (mapconcat #'identity all-queries " OR "))))
              (shipit--debug-log "Reviewer query: %s" combined-query)
              (push combined-query query-parts)))))
       ((string-prefix-p "--team-reviewer=" arg)
        (let ((team (substring arg 16)))
          (when (> (length team) 0)
            ;; team-review-requested requires org/team-name format
            (push (format "team-review-requested:%s" team) query-parts))))
       ((string-prefix-p "--milestone=" arg)
        (let ((milestone (substring arg 12)))
          (when (> (length milestone) 0)
            (push (format "milestone:\"%s\"" milestone) query-parts))))
       ((string-prefix-p "--label=" arg)
        (let ((label (substring arg 8)))
          (when (> (length label) 0)
            (push (format "label:\"%s\"" label) query-parts))))
       ((string-prefix-p "--state=" arg)
        (let ((state (substring arg 8)))
          (unless (string= state "all")
            (push (format "state:%s" state) query-parts))))
       ((string= "--draft" arg)
        (push "draft:true" query-parts))
       ((string= "--conflicts" arg)
        (push "status:failure" query-parts))
       ((string-prefix-p "--title=" arg)
        (let ((title (substring arg 8)))
          (when (> (length title) 0)
            ;; Don't use quotes - allows partial/word matching instead of exact phrase matching
            (push (format "%s in:title" title) query-parts))))
       ((string-prefix-p "--body=" arg)
        (let ((body (substring arg 7)))
          (when (> (length body) 0)
            ;; Don't use quotes - allows partial/word matching instead of exact phrase matching
            (push (format "%s in:body" body) query-parts))))
       ((string-prefix-p "--number=" arg)
        (let ((number (substring arg 9)))
          (when (> (length number) 0)
            (push number query-parts))))
       ((string-prefix-p "--created-after=" arg)
        (let ((date (substring arg 16)))
          (when (> (length date) 0)
            (push (format "created:>%s" date) query-parts))))
       ((string-prefix-p "--created-before=" arg)
        (let ((date (substring arg 17)))
          (when (> (length date) 0)
            (push (format "created:<%s" date) query-parts))))
       ((string-prefix-p "--updated-after=" arg)
        (let ((date (substring arg 16)))
          (when (> (length date) 0)
            (push (format "updated:>%s" date) query-parts))))
       ((string-prefix-p "--updated-before=" arg)
        (let ((date (substring arg 17)))
          (when (> (length date) 0)
            (push (format "updated:<%s" date) query-parts))))))
    (shipit--debug-log "Final query parts: %S" query-parts)
    query-parts))

(defun shipit--extract-limit-from-args (args)
  "Extract limit value from ARGS, defaulting to 100 if not found."
  (let ((limit-arg (seq-find (lambda (arg) (string-prefix-p "--limit=" arg)) args)))
    (if limit-arg
        (string-to-number (substring limit-arg 8))
      100)))

(defun shipit--extract-sort-from-args (args)
  "Extract sort value from ARGS, defaulting to \"created\" if not found."
  (let ((sort-arg (seq-find (lambda (arg) (string-prefix-p "--sort=" arg)) args)))
    (if sort-arg
        (substring sort-arg 7)
      "created")))

(defvar shipit--current-query-parts nil
  "Current query parts for reactive dynamic fetching.")

(defvar shipit--pr-completion-table nil
  "Hash table mapping PR candidates to their data for annotation.")

(defun shipit--pr-annotate (candidate)
  "Return annotation for PR CANDIDATE.
Shows author, status, labels, draft indicator, and time.
Uses marginalia--fields when available for proper alignment."
  (shipit--debug-log "ANNOTATE: candidate=%s table=%s" candidate (if shipit--pr-completion-table "set" "nil"))
  (if (not shipit--pr-completion-table)
      (progn
        (shipit--debug-log "ANNOTATE: No completion table!")
        nil)
    (let* ((pr-info (gethash candidate shipit--pr-completion-table))
           (pr-data (plist-get pr-info :data)))
      (shipit--debug-log "ANNOTATE: pr-info=%s pr-data=%s" (if pr-info "found" "nil") (if pr-data "found" "nil"))
      (if (not pr-data)
          nil
        (let* ((user (cdr (assq 'login (cdr (assq 'user pr-data)))))
               (state (cdr (assq 'state pr-data)))
               (updated (cdr (assq 'updated_at pr-data)))
               (draft (eq (cdr (assq 'draft pr-data)) t))
               ;; Search API returns merged_at under pull_request, direct PR API at top level
               (merged-at (or (cdr (assq 'merged_at pr-data))
                              (cdr (assq 'merged_at (cdr (assq 'pull_request pr-data))))))
               (labels (cdr (assq 'labels pr-data)))
               (label-names (when labels
                              (mapcar (lambda (l) (cdr (assq 'name l))) labels)))
               (comments (cdr (assq 'comments pr-data)))
               (time-ago (if (and updated (fboundp 'shipit--format-time-ago))
                             (shipit--format-time-ago updated)
                           ""))
               (labels-str (when label-names
                             (mapconcat #'identity (seq-take label-names 3) ",")))
               (comments-str (if (and comments (> comments 0))
                                 (format "💬%d" comments)
                               ""))
               (user-str (if user (format "@%s" user) ""))
               ;; Determine effective state: Merged > Closed > Draft > Open
               (effective-state (cond
                                 (merged-at "merged")
                                 ((string= state "closed") "closed")
                                 (draft "draft")
                                 (t state)))
               (state-str (cond
                           ((string= effective-state "draft") "Draft")
                           ((string= effective-state "open") "Open")
                           ((string= effective-state "merged") "Merged")
                           ((string= effective-state "closed") "Closed")
                           (t (or state ""))))
               (state-face (cond
                            ((string= effective-state "draft") 'warning)
                            ((string= effective-state "open") 'success)
                            ((string= effective-state "merged") '(:foreground "purple" :weight bold))
                            ((string= effective-state "closed") 'error)
                            (t 'shadow))))
          (shipit--debug-log "ANNOTATE: user=%s state=%s updated=%s draft=%s labels=%s"
                             user state updated draft label-names)
          (if (fboundp 'marginalia--fields)
              ;; Use marginalia's field alignment
              ;; Note: Variables must be wrapped in (or VAR "") to prevent
              ;; marginalia--fields from interpreting them as function calls
              (marginalia--fields
               ((or user-str "") :truncate 15 :face 'marginalia-value)
               ((or state-str "") :width 7 :face state-face)
               ((or comments-str "") :width 5 :face 'marginalia-number)
               ((or labels-str "") :truncate 25 :face 'marginalia-type)
               ((or time-ago "") :width -12 :face 'marginalia-date))
            ;; Fallback without marginalia
            (let ((parts '()))
              (when (not (string-empty-p user-str))
                (push (propertize user-str 'face 'font-lock-function-name-face) parts))
              (when (not (string-empty-p state-str))
                (push (propertize state-str 'face state-face) parts))
              (when (not (string-empty-p comments-str))
                (push (propertize comments-str 'face 'font-lock-comment-face) parts))
              (when (and time-ago (not (string-empty-p time-ago)))
                (push (propertize time-ago 'face 'font-lock-comment-face) parts))
              (when (not (string-empty-p labels-str))
                (push (propertize labels-str 'face 'font-lock-keyword-face) parts))
              (when parts
                (concat " " (string-join (nreverse parts) " - "))))))))))

(defun shipit--select-pr-from-results (results repo &optional backend-id backend-config)
  "Present RESULTS in completion framework with reactive dynamic fetching.
BACKEND-ID and BACKEND-CONFIG are passed through to `shipit-open-pr-buffer'."
  (let* (;; Sort results by PR number descending (newest first)
         (all-results (sort (copy-sequence results)
                            (lambda (a b)
                              (> (or (cdr (assq 'number a)) 0)
                                 (or (cdr (assq 'number b)) 0)))))
         ;; Create lookup table for PR data since all-completions strips text properties
         (pr-lookup-table (make-hash-table :test 'equal))
         ;; Pre-build candidates and populate lookup table upfront
         ;; This ensures the table is ready before annotation function is called
         ;; Format: #number Title - author/status shown in annotations
         (candidates (mapcar (lambda (pr)
                               (let* ((number (cdr (assq 'number pr)))
                                      (title (string-trim (or (cdr (assq 'title pr)) "")))
                                      ;; Don't truncate title - show full title for better readability
                                      (candidate (format "#%-5d %s" number title)))
                                 ;; Store PR data in lookup table
                                 (puthash candidate (list :number number :repo repo :data pr) pr-lookup-table)
                                 candidate))
                             all-results))
         (completion-function
          (lambda (string predicate action)
            (if (eq action 'metadata)
                ;; Provide completion metadata with annotation function
                `(metadata (display-sort-function . identity)
                          (cycle-sort-function . identity)
                          (annotation-function . shipit--pr-annotate)
                          (category . shipit-pr))
              ;; Delegate all other actions to standard completion table
              ;; This properly handles filtering, prefix matching, etc.
              (complete-with-action action candidates string predicate)))))

    ;; Set the global lookup table for annotation function access BEFORE completing-read
    (setq shipit--pr-completion-table pr-lookup-table)
    ;; Set repo for embark actions
    (setq shipit--embark-pr-repo repo)

    (unwind-protect
        (let ((selected (completing-read "Select PR (type to filter): " completion-function nil t)))
          (when selected
            ;; Extract PR number robustly - look for #NUMBER anywhere in string
            (let* ((pr-number-from-string (when (string-match "#\\([0-9]+\\)" selected)
                                            (string-to-number (match-string 1 selected))))
                   ;; Look up PR info by number in the hash table
                   (pr-info (when pr-number-from-string
                              (catch 'found
                                (maphash (lambda (_key val)
                                           (when (= (plist-get val :number) pr-number-from-string)
                                             (throw 'found val)))
                                         pr-lookup-table)
                                nil)))
                   (pr-number (plist-get pr-info :number))
                   (pr-repo (plist-get pr-info :repo))
                   (pr-data (plist-get pr-info :data)))
              (shipit--debug-log "Selected: '%s', PR number: %s, repo: %s"
                               selected pr-number pr-repo)
              (cond
               ((not pr-number-from-string)
                (message "Could not extract PR number from selection"))
               ((not pr-data)
                (message "Could not find PR #%d in results" pr-number-from-string))
               (t
                (shipit--display-selected-pr pr-data pr-repo backend-id backend-config))))))
      ;; Clear the global table after selection (even on quit/error)
      (setq shipit--pr-completion-table nil))))

(defun shipit--search-prs-with-encoded-query (_repo query-parts per-page &optional max-results sort-by)
  "Search PRs in REPO with properly encoded QUERY-PARTS using efficient pagination.
Returns up to MAX-RESULTS results by fetching multiple pages if needed.
SORT-BY controls ordering: \"created\", \"updated\", or \"comments\" (default: \"created\")."
  (let* ((max-results (or max-results per-page))
         (sort-by (or sort-by "created"))
         (all-items '())
         (page 1)
         (total-count 0)
         (items-per-page (min per-page 100)) ; GitHub API max per_page is 100
         ;; Encode parts but preserve OR operators for GitHub search
         (encoded-parts (mapcar (lambda (part)
                                  (if (string-match-p " OR " part)
                                      ;; For OR queries, encode individual terms but keep OR as-is
                                      ;; Parentheses are encoded as %28 and %29
                                      (let ((has-open-paren (string-prefix-p "(" part))
                                            (has-close-paren (string-suffix-p ")" part))
                                            (inner (string-trim part "(" ")")))
                                        (concat
                                         (if has-open-paren "%28" "")
                                         (mapconcat (lambda (term)
                                                      (if (string= term "OR")
                                                          "OR"
                                                        (url-hexify-string term)))
                                                    (split-string inner " ")
                                                    "+")
                                         (if has-close-paren "%29" "")))
                                    (url-hexify-string part)))
                                query-parts))
         ;; Join query parts with + (GitHub search uses + for spaces)
         (encoded-query (mapconcat 'identity encoded-parts "+")))

    (shipit--debug-log "Starting efficient pagination: max-results=%d, items-per-page=%d" max-results items-per-page)
    (shipit--debug-log "Encoded query parts: %S" query-parts)
    (shipit--debug-log "After encoding: %S" encoded-parts)
    (shipit--debug-log "Final encoded query: %s" encoded-query)

    (while (and page
                (< (length all-items) max-results)
                (or (= page 1) (and total-count (numberp total-count) (> total-count (* (1- page) items-per-page)))))

      (condition-case err
          (let* ((endpoint (format "/search/issues?q=%s&sort=%s&order=desc&per_page=%d&page=%d&advanced_search=true"
                                   encoded-query sort-by items-per-page page))
                 (response (shipit--api-request endpoint)))

            (shipit--debug-log "Page %d: fetching %d items" page items-per-page)

            (if (and response (listp response))
                (let ((items (cdr (assq 'items response)))
                      (response-total (or (cdr (assq 'total_count response)) 0)))
                  (setq total-count response-total)
                  (shipit--debug-log "Page %d: got %d items, total_count=%s, accumulated=%d"
                                   page (if items (length items) 0) response-total (length all-items))

                  (when items
                    (setq all-items (append all-items items)))

                  ;; Stop if no more items available or reached our limit
                  (when (or (not items)
                            (< (length items) items-per-page)
                            (>= (length all-items) max-results))
                    (shipit--debug-log "Stopping pagination: items=%s, accumulated=%d, max=%d"
                                     (if items (length items) 0) (length all-items) max-results)
                    (setq page nil))) ; Break the loop

              (progn
                (shipit--debug-log "Page %d: Invalid API response (nil or non-list): %S" page response)
                (shipit--debug-log "This usually indicates a timeout, rate limit, or network error")
                (shipit--debug-log "Search API may be rate limited")
                (message "Search API rate limited or timed out - wait a few minutes and try again")
                (setq page nil)))) ; Break the loop

        (error
         (shipit--debug-log "Page %d error: %s" page err)
         (message "Search failed on page %d: %s" page (error-message-string err))
         (setq page nil))) ; Break the loop

      (when page
        (setq page (1+ page))))

    (let ((final-results (seq-take all-items max-results)))
      (shipit--debug-log "Pagination complete: fetched %d pages, total items %d, returning %d"
                       (1- (or page 1)) (length all-items) (length final-results))
      final-results)))

(declare-function shipit--read-github-user "shipit-pr-github")


;;;###autoload
(defun shipit--get-current-worktree-info ()
  "Get PR info if current directory is a shipit worktree.
Returns plist with :pr-number, :repo, :branch, or nil."
  (let ((info-file (expand-file-name ".shipit-pr-info.json" default-directory)))
    (when (file-exists-p info-file)
      (condition-case nil
          (let* ((json-str (with-temp-buffer
                             (insert-file-contents info-file)
                             (buffer-string)))
                 (json-data (json-read-from-string json-str)))
            (list :pr-number (cdr (assq 'pr_number json-data))
                  :repo (cdr (assq 'repo json-data))
                  :branch (cdr (assq 'branch json-data))))
        (error nil)))))

(defun shipit--get-pr-number-from-detached-head ()
  "Find PR/MR number when HEAD is detached by checking ref patterns.
Checks both GitHub refs/pull/*/head and GitLab refs/merge-requests/*/head.
Returns PR/MR number if found, nil otherwise."
  (condition-case nil
      (let* ((head-sha (string-trim (shell-command-to-string "git rev-parse HEAD")))
             ;; Find refs that point to current HEAD (both GitHub and GitLab patterns)
             (refs-output (string-trim
                           (shell-command-to-string
                            (format "git for-each-ref --points-at %s --format='%%(refname)' 'refs/remotes/origin/pr/*' 'refs/pull/*' 'refs/merge-requests/*'"
                                    head-sha)))))
        (shipit--debug-log "Detached HEAD check: sha=%s refs=%s" head-sha refs-output)
        ;; Also check FETCH_HEAD which contains ref after fetch
        (when (string-empty-p refs-output)
          (let ((fetch-head-file (expand-file-name ".git/FETCH_HEAD" (shipit--get-repo-root))))
            (when (file-exists-p fetch-head-file)
              (with-temp-buffer
                (insert-file-contents fetch-head-file)
                (goto-char (point-min))
                ;; GitHub: "<sha>\t\tbranch 'pull/123/head' of ..."
                ;; GitLab: "<sha>\t\tbranch 'merge-requests/123/head' of ..."
                (when (re-search-forward (concat "^" (regexp-quote head-sha)
                                                 ".*\\(?:pull\\|merge-requests\\)/\\([0-9]+\\)/head")
                                         nil t)
                  (setq refs-output (format "refs/matched/%s" (match-string 1))))))))
        ;; Parse PR/MR number from ref patterns
        (when (and refs-output (not (string-empty-p refs-output)))
          (when (string-match "\\(?:pull\\|pr\\|merge-requests\\|matched\\)/\\([0-9]+\\)" refs-output)
            (string-to-number (match-string 1 refs-output)))))
    (error nil)))

;;;###autoload
(defun shipit-open-pr-for-current-branch ()
  "Open the PR for the current branch or worktree in shipit.
Works with:
- Normal branches (looks up PR by branch name)
- Shipit worktrees (reads .shipit-pr-info.json)
- Detached HEAD (checks refs/pull/*/head matching current commit)"
  (interactive)
  ;; Mark as explicit PR operation
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "REOPEN-PR: Before - explicit-op=%s" shipit--explicit-pr-operation))
  (setq shipit--explicit-pr-operation t)
  (shipit--debug-log "REOPEN-PR: After - explicit-op=%s" shipit--explicit-pr-operation)
  ;; Check if API token is configured before proceeding
  (unless (shipit--github-token)
    (user-error "GitHub token not found.  Set `shipit-github-token' or add github.com to auth-source"))
  (let* ((current-branch (magit-get-current-branch))
         (repo (shipit--get-repo-from-remote)))
    (cond
     ;; Case 1: Normal branch - look up PR by branch name
     ((and current-branch repo)
      (message "Loading PR for branch '%s'..." current-branch)
      (let ((captured-branch current-branch)
            (captured-repo repo)
            (target-buffer (current-buffer)))
        (shipit-get-pr-for-branch-async
         current-branch
         (lambda (pr-data)
           (if pr-data
               (let ((pr-number (cdr (assq 'number pr-data))))
                 (message "Opening PR #%d for branch '%s'..." pr-number captured-branch)
                 (run-at-time 0 nil
                              (lambda ()
                                (when (buffer-live-p target-buffer)
                                  (with-current-buffer target-buffer
                                    (shipit--display-selected-pr pr-data captured-repo))))))
             (message "No PR found for branch '%s' in repository '%s'" captured-branch captured-repo)))
         repo)))

     ;; Case 2: Detached HEAD - check for shipit worktree or PR ref
     (repo
      (let ((worktree-info (shipit--get-current-worktree-info)))
        (if worktree-info
            ;; Shipit worktree with .shipit-pr-info.json
            (let ((pr-number (plist-get worktree-info :pr-number))
                  (branch (plist-get worktree-info :branch)))
              (message "Loading PR #%d from worktree (branch '%s')..." pr-number branch)
              (shipit-open-pr-buffer pr-number repo))
          ;; Try to find PR from refs
          (let ((pr-number (shipit--get-pr-number-from-detached-head)))
            (if pr-number
                (progn
                  (message "Loading PR #%d from detached HEAD..." pr-number)
                  (shipit-open-pr-buffer pr-number repo))
              (let* ((resolved (ignore-errors (shipit-pr--resolve-for-repo repo)))
                     (backend (car resolved))
                     (refspec-fn (when backend (plist-get backend :refspec-for-pr))))
                (if refspec-fn
                    (user-error "Detached HEAD: no PR ref found. Try: git fetch origin %s"
                                (funcall refspec-fn "N"))
                  (user-error "Detached HEAD: no PR ref found. Try fetching the PR ref first"))))))))

     (t
      (user-error "Could not determine repository")))))

(defun shipit-copy-pr-url ()
  "Copy the URL of the PR at point to the clipboard.
If a region is active, copy the region text instead (standard M-w behavior).
Gets PR number and repository from text properties at cursor position.
Works in PR listings, notifications, and status sections."
  (interactive)
  (if (use-region-p)
      ;; Region active - use standard copy behavior
      (call-interactively #'kill-ring-save)
    ;; No region - copy PR URL via backend dispatch
    (let* ((pr-number (get-text-property (point) 'shipit-pr-number))
           (repo (get-text-property (point) 'shipit-repo)))
      (if (and pr-number repo)
          (let* ((resolved (shipit-pr--resolve-for-repo repo))
                 (backend (car resolved))
                 (config (cdr resolved))
                 (browse-fn (plist-get backend :browse-url))
                 (url (when browse-fn (funcall browse-fn config pr-number))))
            (if url
                (progn (kill-new url)
                       (message "Copied: %s" url))
              (user-error "Backend does not support browse URL")))
        (user-error "No PR at point. Position cursor on a PR to copy its URL")))))

;; Worktree Transient Menu

(transient-define-prefix shipit-worktree ()
  "Worktree operations for current PR."
  :man-page "shipit"
  ["Worktree Actions"
   ("c" "Checkout in worktree" shipit--checkout-pr-worktree)
   ("s" "Sync worktree" shipit--sync-pr-worktree)
   ("d" "Delete worktree" shipit--delete-pr-worktree)
   ("o" "Open worktree directory" shipit--open-pr-worktree-directory)])

(defun shipit--get-current-pr-number ()
  "Get PR number from current buffer context."
  (get-text-property (point) 'shipit-pr-number))

(defun shipit--get-current-pr-head-sha ()
  "Get PR head SHA from current buffer context."
  (get-text-property (point) 'shipit-pr-head-sha))

(defun shipit--checkout-pr-worktree ()
  "Checkout current PR in a new git worktree."
  (interactive)
  (let* ((pr-number (shipit--get-current-pr-number))
         (pr-data (get-text-property (point) 'shipit-pr-data))
         (pr-repo (cdr (assq 'full_name (cdr (assq 'repo (cdr (assq 'base pr-data)))))))
         (branch-name (cdr (assq 'ref (cdr (assq 'head pr-data)))))
         (pr-head-sha (cdr (assq 'sha (cdr (assq 'head pr-data))))))
    (unless pr-number
      (user-error "No PR at point"))

    (shipit--validate-pr-repo pr-repo)

    (message "Creating worktree for PR #%d..." pr-number)
    (let ((result (shipit--create-worktree-pr pr-number branch-name pr-head-sha)))
      (if result
          (progn
            (message "Worktree created at: %s" (car result))
            (when (y-or-n-p "Open worktree directory? ")
              (shipit--open-worktree-directory (car result))))
        (message "Failed to create worktree. See debug log for details")))))

(defun shipit--sync-pr-worktree ()
  "Sync current PR worktree with latest changes."
  (interactive)
  (let* ((pr-number (shipit--get-current-pr-number))
         (pr-data (get-text-property (point) 'shipit-pr-data))
         (pr-repo (cdr (assq 'full_name (cdr (assq 'repo (cdr (assq 'base pr-data)))))))
         (branch-name (cdr (assq 'ref (cdr (assq 'head pr-data)))))
         (worktree-path (shipit--find-worktree-for-pr pr-number pr-repo)))
    (unless pr-number
      (user-error "No PR at point"))

    (shipit--validate-pr-repo pr-repo)

    (unless worktree-path
      (user-error "No worktree found for PR #%d" pr-number))

    (message "Syncing worktree...")
    (if (shipit--sync-worktree worktree-path branch-name)
        (progn
          (message "Worktree synced successfully")
          ;; Refresh the shipit buffer to show updated sync status
          (when (derived-mode-p 'shipit-mode)
            (shipit-buffer-refresh)))
      (message "Failed to sync worktree. See debug log for details"))))

(defun shipit--delete-pr-worktree ()
  "Delete the worktree for current PR."
  (interactive)
  (let* ((pr-number (shipit--get-current-pr-number))
         (pr-data (get-text-property (point) 'shipit-pr-data))
         (pr-repo (cdr (assq 'full_name (cdr (assq 'repo (cdr (assq 'base pr-data)))))))
         (worktree-path (shipit--find-worktree-for-pr pr-number pr-repo)))
    (unless pr-number
      (user-error "No PR at point"))

    (shipit--validate-pr-repo pr-repo)

    (unless worktree-path
      (user-error "No worktree found for PR #%d" pr-number))

    (when (y-or-n-p (format "Delete worktree at %s? " worktree-path))
      (message "Deleting worktree...")
      (if (shipit--delete-worktree worktree-path)
          (message "Worktree deleted successfully")
        (message "Failed to delete worktree. See debug log for details")))))

(defun shipit--open-pr-worktree-directory ()
  "Open worktree directory in dired or new frame."
  (interactive)
  (let* ((pr-number (shipit--get-current-pr-number))
         (pr-data (get-text-property (point) 'shipit-pr-data))
         (pr-repo (cdr (assq 'full_name (cdr (assq 'repo (cdr (assq 'base pr-data)))))))
         (worktree-path (shipit--find-worktree-for-pr pr-number pr-repo)))
    (unless pr-number
      (user-error "No PR at point"))

    (shipit--validate-pr-repo pr-repo)

    (unless worktree-path
      (user-error "No worktree found for PR #%d" pr-number))

    (if (shipit--open-worktree-directory worktree-path)
        (message "Opened worktree directory")
      (message "Failed to open worktree directory. See debug log for details"))))

(defun shipit--pr-annotate-unified (candidate)
  "Unified annotation function for PR CANDIDATE.
Tries multiple data sources: completion table, then cache."
  (or
   ;; First try the direct lookup table (used by shipit--select-pr-from-results)
   (shipit--pr-annotate candidate)
   ;; Fall back to cache lookup (used by shipit--dynamic-pr-completion)
   (when (fboundp 'shipit--pr-annotate-dynamic)
     (shipit--pr-annotate-dynamic candidate))))

;; Marginalia integration - register annotator for shipit-pr category
;; This provides rich annotations when marginalia is available
;; We only provide our unified annotator and none - no 'builtin' since the
;; metadata annotation-function doesn't use marginalia--fields for alignment
(defvar marginalia-annotator-registry)  ; Silence byte-compiler
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(shipit-pr shipit--pr-annotate-unified none)))

;; Make magit-section-post-command-hook safe when magit-root-section is nil
;; This can happen when the hook leaks to non-magit buffers (e.g., embark-collect)
(defvar magit-root-section)  ; Silence byte-compiler
(with-eval-after-load 'magit-section
  (advice-add 'magit-section-post-command-hook :around
              (lambda (orig-fn)
                "Skip magit-section-post-command-hook if magit-root-section is nil."
                (when (bound-and-true-p magit-root-section)
                  (funcall orig-fn)))))

;; Embark integration - actions for shipit-pr category
(defvar shipit--embark-pr-repo nil
  "Current repo for embark PR actions. Set during PR search.")

(defun shipit--extract-pr-number-from-candidate (candidate)
  "Extract PR number from CANDIDATE string like '#12345 Title'."
  (when (string-match "#\\([0-9]+\\)" candidate)
    (string-to-number (match-string 1 candidate))))

(defun shipit--embark-pr-url (candidate)
  "Get URL for PR CANDIDATE via backend dispatch."
  (let ((pr-number (shipit--extract-pr-number-from-candidate candidate))
        (repo (or shipit--embark-pr-repo (shipit--get-repo-from-remote))))
    (when (and pr-number repo)
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (browse-fn (plist-get backend :browse-url)))
        (when browse-fn
          (funcall browse-fn config pr-number))))))

(defun shipit-embark-copy-pr-url (candidate)
  "Copy PR URL for CANDIDATE to kill ring."
  (interactive "sCandidate: ")
  (let ((url (shipit--embark-pr-url candidate)))
    (if url
        (progn
          (kill-new url)
          (message "Copied: %s" url))
      (message "Could not determine PR URL"))))

(defun shipit-embark-copy-pr-number (candidate)
  "Copy PR number for CANDIDATE to kill ring."
  (interactive "sCandidate: ")
  (let ((pr-number (shipit--extract-pr-number-from-candidate candidate)))
    (if pr-number
        (progn
          (kill-new (number-to-string pr-number))
          (message "Copied: #%d" pr-number))
      (message "Could not extract PR number"))))

(defun shipit-embark-open-pr-browser (candidate)
  "Open PR CANDIDATE in browser."
  (interactive "sCandidate: ")
  (let ((url (shipit--embark-pr-url candidate)))
    (if url
        (browse-url url)
      (message "Could not determine PR URL"))))

(defun shipit-embark-open-pr (candidate)
  "Open PR CANDIDATE in shipit buffer."
  (interactive "sCandidate: ")
  (let ((pr-number (shipit--extract-pr-number-from-candidate candidate))
        (repo (or shipit--embark-pr-repo (shipit--get-repo-from-remote))))
    (if (and pr-number repo)
        (shipit-open-pr-buffer pr-number repo)
      (message "Could not determine PR"))))

(defun shipit-embark-copy-all-pr-urls (candidates)
  "Copy URLs for all CANDIDATES to kill ring as single entry, one per line."
  (let ((urls (delq nil (mapcar #'shipit--embark-pr-url candidates))))
    (if urls
        (let ((text (string-join urls "\n")))
          (kill-new text)
          (message "Copied %d URLs" (length urls)))
      (message "No URLs to copy"))))

(defvar embark-keymap-alist)  ; Silence byte-compiler
(defvar embark-general-map)   ; Silence byte-compiler
(defvar embark-multitarget-actions)  ; Silence byte-compiler
(with-eval-after-load 'embark
  (defvar embark-shipit-pr-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map embark-general-map)
      (define-key map (kbd "o") #'shipit-embark-open-pr)
      (define-key map (kbd "b") #'shipit-embark-open-pr-browser)
      (define-key map (kbd "u") #'shipit-embark-copy-pr-url)
      (define-key map (kbd "U") #'shipit-embark-copy-all-pr-urls)
      (define-key map (kbd "y") #'shipit-embark-copy-pr-number)
      map)
    "Keymap for embark actions on shipit-pr candidates.")
  (add-to-list 'embark-keymap-alist '(shipit-pr . embark-shipit-pr-map))
  ;; Register as multitarget so it receives all candidates with embark-act-all
  (add-to-list 'embark-multitarget-actions #'shipit-embark-copy-all-pr-urls))

(defun shipit-mark-activities-read ()
  "Mark all activities as read for the current PR.
Finds all activity event IDs in the buffer and marks them as read."
  (interactive)
  (let ((pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr)))
    (unless (and pr-number repo)
      (user-error "No PR context available"))
    ;; Find all activity event IDs in the buffer and mark them as read
    (let ((count 0)
          (inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((event-id (get-text-property (point) 'shipit-event-id)))
            (when (and event-id (get-text-property (point) 'shipit-activity-event))
              (shipit--mark-activity-read repo pr-number event-id)
              ;; Clear the red dot on this line visually
              (let ((line-start (line-beginning-position))
                    (line-end (line-end-position)))
                (save-excursion
                  (goto-char line-start)
                  (when (re-search-forward "● " line-end t)
                    (replace-match "  " t t))))
              (setq count (1+ count))))
          (goto-char (or (next-single-property-change (point) 'shipit-event-id)
                         (point-max)))))
      (message "Marked %d activities as read for PR #%s" count pr-number))
    ;; Update the Activity section header indicator (no full refresh needed)
    (when (fboundp 'shipit--update-activity-header-indicator)
      (shipit--update-activity-header-indicator))
    ;; Update all section indicators (Comments, Commits, etc.)
    (when (fboundp 'shipit--update-all-section-indicators)
      (shipit--update-all-section-indicators))))

(defvar shipit-doctor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'shipit-doctor)
    map)
  "Keymap for `shipit-doctor-mode'.")

(define-derived-mode shipit-doctor-mode special-mode "Shipit-Doctor"
  "Major mode for shipit doctor buffer."
  :keymap shipit-doctor-mode-map)

(defun shipit--insert-rate-limit-diagnostics (data)
  "Insert rate limit diagnostics from DATA into current buffer."
  (let* ((resources (cdr (assq 'resources data)))
         (core (cdr (assq 'core resources)))
         (search (cdr (assq 'search resources)))
         (graphql (cdr (assq 'graphql resources))))
    (insert "✓ API connection successful\n\n")
    (dolist (section `(("Core API" . ,core) ("Search API" . ,search) ("GraphQL API" . ,graphql)))
      (let* ((name (car section))
             (info (cdr section))
             (remaining (cdr (assq 'remaining info)))
             (limit (cdr (assq 'limit info)))
             (reset (cdr (assq 'reset info))))
        (when info
          (insert (format "%s:\n" name))
          (insert (format "  Remaining: %d/%d\n" remaining limit))
          (if (zerop remaining)
              (insert (format "  ⚠ RATE LIMITED - Resets: %s\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time reset))))
            (insert (format "  Resets: %s\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time reset)))))
          (insert "\n"))))))

(defun shipit--insert-diagnostics-api-error (err)
  "Insert API error diagnostics from ERR into current buffer."
  (let ((msg (error-message-string err)))
    (insert (format "✗ API error: %s\n" msg))
    (when (string-match "HTTP 401" msg)
      (insert "  Token may be invalid or expired\n"))
    (when (string-match "HTTP 403" msg)
      (insert "  Access forbidden - check token permissions\n"))))

;;;###autoload
(defun shipit-doctor ()
  "Check shipit configuration and API status.
Displays a diagnostic report including:
- Token configuration
- API connectivity
- Rate limit status
- Search API rate limit (separate from core API)"
  (interactive)
  (let ((buffer (get-buffer-create "*shipit-doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Shipit Doctor\n")
        (insert "=============\n\n")

        ;; Check token
        (insert "Token Configuration\n")
        (insert "-------------------\n")
        (let ((token (shipit--github-token)))
          (if token
              (let ((token-preview (if (> (length token) 8)
                                       (concat (substring token 0 4)
                                               "..."
                                               (substring token -4))
                                     "****")))
                (insert (format "✓ Token set: %s%s\n" token-preview
                                (if shipit-github-token "" " (from auth-source)"))))
            (insert (format "✗ Token NOT set - set `shipit-github-token' or add github.com to auth-source\n"))))
        (insert "\n")

        ;; Check API connectivity and rate limits
        (let* ((backend (ignore-errors (shipit-pr--get-backend)))
               (backend-name (or (when backend (plist-get backend :name)) "API"))
               (rate-fn (when backend (plist-get backend :fetch-rate-limit)))
               (user-fn (when backend (plist-get backend :fetch-current-user))))
          (insert (format "%s API Status\n" backend-name))
          (insert (make-string (+ (length backend-name) 11) ?-))
          (insert "\n")
          (if (shipit--github-token)
              (if rate-fn
                  (condition-case err
                      (let ((data (funcall rate-fn nil)))
                        (shipit--insert-rate-limit-diagnostics data))
                    (error
                     (shipit--insert-diagnostics-api-error err)))
                (insert "- Rate limit check: not available for current backend\n"))
            (insert "- Skipped (no token configured)\n"))
          (insert "\n")

          ;; Check current user
          (insert "Authenticated User\n")
          (insert "------------------\n")
          (if (shipit--github-token)
              (if user-fn
                  (condition-case err
                      (let ((data (funcall user-fn nil)))
                        (let ((login (cdr (assq 'login data)))
                              (name (cdr (assq 'name data))))
                          (insert (format "✓ Logged in as: %s" login))
                          (when name
                            (insert (format " (%s)" name)))
                          (insert "\n")))
                    (error
                     (insert (format "✗ Error: %s\n" (error-message-string err)))))
                (insert "- User check: not available for current backend\n"))
            (insert "- Skipped (no token configured)\n")))

        (insert "\n")
        (insert "---\n")
        (insert "Press 'q' to close, 'g' to refresh.\n")

        ;; Use our own mode with isolated keymap
        (shipit-doctor-mode)))

    (display-buffer buffer)))

;;; Mermaid configuration commands

(defun shipit-toggle-mermaid ()
  "Toggle mermaid diagram rendering."
  (interactive)
  (setq shipit-render-mermaid (not shipit-render-mermaid))
  (message "Mermaid rendering %s" (if shipit-render-mermaid "enabled" "disabled")))

(defun shipit-cycle-mermaid-theme ()
  "Cycle through mermaid themes: dark -> default -> forest -> neutral."
  (interactive)
  (setq shipit-mermaid-theme
        (pcase shipit-mermaid-theme
          ("dark" "default")
          ("default" "forest")
          ("forest" "neutral")
          ("neutral" "dark")
          (_ "dark")))
  (message "Mermaid theme: %s" shipit-mermaid-theme))

(defun shipit-cycle-mermaid-background ()
  "Cycle through mermaid background presets: transparent -> white -> #1e1e1e."
  (interactive)
  (setq shipit-mermaid-background
        (pcase shipit-mermaid-background
          ("transparent" "white")
          ("white" "#1e1e1e")
          ("#1e1e1e" "transparent")
          (_ "transparent")))
  (message "Mermaid background: %s" shipit-mermaid-background))

(defun shipit-increase-mermaid-max-width ()
  "Increase mermaid max width by 100 pixels."
  (interactive)
  (setq shipit-mermaid-max-width (min 2000 (+ shipit-mermaid-max-width 100)))
  (message "Mermaid max width: %d px" shipit-mermaid-max-width))

(defun shipit-decrease-mermaid-max-width ()
  "Decrease mermaid max width by 100 pixels."
  (interactive)
  (setq shipit-mermaid-max-width (max 200 (- shipit-mermaid-max-width 100)))
  (message "Mermaid max width: %d px" shipit-mermaid-max-width))

;;;###autoload
(defun shipit-open-url ()
  "Open a forge URL in shipit.
Prompts for a URL, defaulting to kill-ring head if it looks like a URL.
Recognized URLs open in the appropriate shipit buffer.
Unrecognized URLs offer a browser fallback."
  (interactive)
  (let* ((default (when (and kill-ring
                             (string-match-p "\\`https?://" (car kill-ring)))
                    (car kill-ring)))
         (url (read-string "URL: " nil nil default)))
    (unless (string-empty-p url)
      (let ((classified (shipit--classify-url url)))
        (if classified
            (shipit--open-classified-url classified)
          (message "URL not recognized by any shipit backend: %s" url)
          (when (y-or-n-p "Open in browser instead? ")
            (browse-url url)))))))

;;;
;;; File Viewed State
;;;

(defun shipit--set-file-viewed (filename repo pr-number viewed)
  "Set FILENAME in REPO PR-NUMBER to VIEWED state (t or nil).
Updates both the API and local state."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (mark-fn (plist-get backend (if viewed :mark-file-viewed :unmark-file-viewed))))
    (when mark-fn
      (funcall mark-fn config pr-number filename)
      (setq shipit--file-viewed-states
            (cons (cons filename (if viewed "VIEWED" "UNVIEWED"))
                  (assoc-delete-all filename shipit--file-viewed-states)))
      (shipit--refresh-file-viewed-indicator filename (not viewed)))))

(defun shipit--collect-files-in-region ()
  "Collect unique filenames from file lines in the active region."
  (let ((beg (region-beginning))
        (end (region-end))
        (files '())
        (seen (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((f (get-text-property (point) 'shipit-file-path)))
          (when (and f (not (gethash f seen)))
            (puthash f t seen)
            (push f files)))
        (forward-line 1)))
    (nreverse files)))

(defun shipit-toggle-file-viewed ()
  "Toggle the viewed state of the file at point, or all files in region."
  (interactive)
  (if (use-region-p)
      (let* ((files (shipit--collect-files-in-region))
             (repo (get-text-property (region-beginning) 'shipit-repo))
             (pr-number (get-text-property (region-beginning) 'shipit-pr-number)))
        (dolist (f files)
          (let ((viewed (shipit--file-viewed-p f)))
            (shipit--set-file-viewed f repo pr-number (not viewed))))
        (deactivate-mark)
        (message "Toggled %d files" (length files)))
    (let ((filename (get-text-property (point) 'shipit-file-path))
          (repo (get-text-property (point) 'shipit-repo))
          (pr-number (get-text-property (point) 'shipit-pr-number)))
      (unless filename
        (user-error "No file at point"))
      (let ((currently-viewed (shipit--file-viewed-p filename)))
        (shipit--set-file-viewed filename repo pr-number (not currently-viewed))
        (message "%s %s" filename (if currently-viewed "unmarked" "marked as viewed"))))))

(defun shipit-mark-all-files-viewed ()
  "Mark all files as viewed, or unmark all if all are already viewed."
  (interactive)
  (let* ((repo shipit-buffer-repo)
         (pr-number shipit-buffer-pr-number)
         (all-viewed (cl-every (lambda (entry) (equal "VIEWED" (cdr entry)))
                               shipit--file-viewed-states))
         (target (not all-viewed))
         (count 0))
    (dolist (entry shipit--file-viewed-states)
      (let* ((filename (car entry))
             (is-viewed (equal "VIEWED" (cdr entry))))
        (unless (eq is-viewed target)
          (shipit--set-file-viewed filename repo pr-number target)
          (setq count (1+ count)))))
    (message "%s %d files" (if target "Marked" "Unmarked") count)))

(defun shipit--refresh-file-viewed-indicator (_filename _was-viewed)
  "Refresh the files section to update viewed indicators."
  (when (fboundp 'shipit--refresh-files-section-only)
    (shipit--refresh-files-section-only)))

(provide 'shipit-commands)
;;; shipit-commands.el ends here
