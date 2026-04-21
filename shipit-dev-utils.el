;;; shipit-dev-utils.el --- Development utilities for shipit -*- lexical-binding: t; -*-

;;; Commentary:
;; Development utilities for working on shipit

;;; Code:

;;;###autoload
(defun shipit-reload (&optional compile-first)
  "Properly reload shipit from source files.
With prefix argument COMPILE-FIRST, run make clean && make compile first."
  (interactive "P")
  (catch 'shipit-reload-done
    ;; Bootstrap: reload this file first to ensure we're using the latest version
    (let ((this-file (or load-file-name
                         (locate-library "shipit-dev-utils")
                         (expand-file-name "shipit-dev-utils.el"
                                           (file-name-directory (locate-library "shipit-debug"))))))
      (when (and this-file
                 (file-exists-p this-file)
                 (not (bound-and-true-p shipit--reload-bootstrapped)))
        (let ((shipit--reload-bootstrapped t))
          (load-file this-file)
          ;; Call the newly loaded version and exit
          (throw 'shipit-reload-done (funcall 'shipit-reload compile-first)))))
    ;; CRITICAL: Set system-time-locale FIRST, before loading any code
  ;; This prevents "Multibyte text in HTTP request" errors from url.el
  ;; using locale-specific date formatting in If-Modified-Since headers
  (setq system-time-locale "C")
  (let* ((shipit-lib (locate-library "shipit-debug"))
         (lisp-dir (when shipit-lib (file-name-directory shipit-lib)))
         (project-dir (when lisp-dir (file-name-directory (directory-file-name lisp-dir))))
         (shipit-dir (or lisp-dir
                         (expand-file-name "lisp/" (file-name-directory (or load-file-name buffer-file-name)))))
         (project-dir (or project-dir
                          (file-name-directory (or load-file-name buffer-file-name)))))
    (when compile-first
      (message "Recompiling shipit...")
      (let ((default-directory project-dir))
        (shell-command "make clean && make autoloads && make compile")))
    (message "Reloading shipit...")
    
    ;; 1. Clear buffer-local variables and overlays that might cause issues
    ;; IMPORTANT: Only touch shipit-mode buffers, NOT magit-status-mode buffers
    ;; to avoid disrupting magit's section visibility state
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'magit-diff-mode 'magit-revision-mode)
                   (not (derived-mode-p 'magit-status-mode)))
          (condition-case nil
              (progn
                (remove-overlays)
                ;; Clear any shipit-related text properties that might have invalid positions
                (let ((inhibit-read-only t))
                  (remove-text-properties (point-min) (point-max)
                                        '(shipit-comment nil
                                          shipit-comment-id nil
                                          shipit-comment-body nil
                                          thread-key nil
                                          thread-start nil
                                          thread-folded nil))))
            (error nil)))))
    
    ;; 2. Save user customizations before unloading
    (let ((saved-token (when (boundp 'shipit-github-token) shipit-github-token))
          (saved-api-url (when (boundp 'shipit-api-url) shipit-api-url)))
      
      ;; Skip aggressive feature unloading - just reload files to update definitions
      ;; This preserves variable state and avoids void-variable errors

      ;; Cancel any pending url.el requests to avoid callback errors during reload
      (when (fboundp 'url-queue-kill-all)
        (url-queue-kill-all))

      ;; Reset keymaps defined with defvar so they get re-evaluated
      ;; Reset all shipit keymaps so defvar re-evaluates them
      (mapatoms
       (lambda (sym)
         (when (and (boundp sym)
                    (let ((name (symbol-name sym)))
                      (or (string-prefix-p "shipit-" name)
                          (string-prefix-p "shipit--" name)))
                    (keymapp (symbol-value sym)))
           (makunbound sym))))

      ;; Load files in dependency order (core → specific modules)
      (let ((load-files '("shipit-lib.el" "shipit-sections.el" "shipit-core.el" "shipit-cache.el" "shipit-comments.el"
                          "shipit-worktree.el" "shipit-http.el" "shipit-gh-etag.el"
                          "shipit-issue-backends.el" "shipit-issue-titles.el"
                          "shipit-issue-github.el" "shipit-issue-jira.el" "shipit-issue-gitlab.el"
                          "shipit-pr-backends.el" "shipit-pr-github.el"
                          "shipit-gitlab-http.el" "shipit-pr-gitlab.el"
                          "shipit-comment-backends.el" "shipit-comment-github.el" "shipit-comment-gitlab.el"
                          "shipit-render.el" "shipit-diff.el" "shipit-notifications.el" "shipit-notifications-buffer.el"
                          "shipit-checks.el" "shipit-actions.el" "shipit-actions-list.el"
                          "shipit-pr-search.el" "shipit-file-filter.el"
                          "shipit-rounded-section.el"
                          "shipit-pr-sections.el" "shipit-pr-actions.el" "shipit-pr-diff.el"
                          "shipit-pr-linked-issue.el"
                          "shipit-buffer.el" "shipit-commands.el"
                          "shipit-review-mode.el" "shipit-review-revision.el" "shipit-review-side-frame.el"
                          "shipit-preview.el" "shipit-editor.el"
                          "shipit-issue-create.el"
                          "shipit-issues.el" "shipit-issues-buffer.el"
                          "shipit-repo-buffer.el"
                          "shipit-subscriptions-buffer.el"
                          "shipit-discussion-backends.el"
                          "shipit-discussions-graphql.el"
                          "shipit-discussions.el" "shipit-discussions-buffer.el"
                          "shipit-atlassian-board.el" "shipit-atlassian-dashboard.el"
                          "shipit-notifications-rss.el"
                          "shipit-code-refs.el"
                          "shipit-debug.el" "shipit.el")))
        ;; First pass: Delete all .elc bytecode files to force fresh load
        (dolist (file load-files)
          (let ((elc-path (expand-file-name (concat (file-name-sans-extension file) ".elc") shipit-dir)))
            (when (file-exists-p elc-path)
              (delete-file elc-path))))
        ;; Second pass: Remove shipit advice from all functions BEFORE unbinding
        (message "Removing shipit advice...")
        (mapatoms
         (lambda (sym)
           (when (fboundp sym)
             ;; Get advice list and remove any shipit advice
             (advice-mapc
              (lambda (advice _props)
                (when (and (symbolp advice)
                           (let ((name (symbol-name advice)))
                             (or (string-prefix-p "shipit-" name)
                                 (string-prefix-p "shipit--" name))))
                  (advice-remove sym advice)))
              sym))))
        ;; Third pass: Remove shipit functions from hooks BEFORE unbinding
        ;; This prevents "void-function" errors from hooks
        (message "Removing shipit functions from hooks...")
        (dolist (hook '(post-command-hook pre-command-hook after-change-functions
                        before-change-functions magit-status-sections-hook
                        magit-diff-sections-hook magit-revision-sections-hook))
          (when (boundp hook)
            (set hook
                 (seq-remove
                  (lambda (fn)
                    (and (symbolp fn)
                         (let ((name (symbol-name fn)))
                           (or (string-prefix-p "shipit-" name)
                               (string-prefix-p "shipit--" name)))))
                  (symbol-value hook)))))
        ;; Also remove from buffer-local hooks in all buffers
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (dolist (hook '(post-command-hook pre-command-hook))
              (when (local-variable-p hook)
                (set (make-local-variable hook)
                     (seq-remove
                      (lambda (fn)
                        (and (symbolp fn)
                             (let ((name (symbol-name fn)))
                               (or (string-prefix-p "shipit-" name)
                                   (string-prefix-p "shipit--" name)))))
                      (symbol-value hook)))))))
        ;; Fourth pass: Unbind all shipit functions to ensure fresh definitions
        ;; This is critical - load doesn't replace existing fboundp symbols reliably
        (message "Unbinding shipit functions...")
        (mapatoms
         (lambda (sym)
           (when (and (fboundp sym)
                      (let ((name (symbol-name sym)))
                        (or (string-prefix-p "shipit-" name)
                            (string-prefix-p "shipit--" name))))
             (fmakunbound sym))))
        ;; Fifth pass: Load the .el files directly (use load-file to bypass .elc preference)
        (dolist (file load-files)
          (let ((full-path (expand-file-name file shipit-dir)))
            (when (file-exists-p full-path)
              (message "Loading %s..." file)
              (condition-case err
                  ;; Use load-file to load the exact .el file, not .elc
                  (load-file full-path)
                (error (message "Error loading %s: %s" file (error-message-string err))))))))
      
      ;; 6. Regenerate and reload autoloads to pick up any new autoload cookies
      (message "Regenerating autoloads...")
      (let ((default-directory project-dir))
        (shell-command "make autoloads"))
      (let ((autoloads-file (expand-file-name "shipit-autoloads.el" shipit-dir)))
        (when (file-exists-p autoloads-file)
          (message "Reloading autoloads...")
          (condition-case err
              (load-file autoloads-file)
            (error (message "Error reloading autoloads: %s" (error-message-string err))))))
      
      ;; NOW restore user customizations AFTER all files are loaded (including autoloads)
      (when saved-token
        (setq shipit-github-token saved-token))
      (when saved-api-url
        (setq shipit-api-url saved-api-url)))
    
    ;; 3. Functions are already properly reloaded via file loading - no need to manually unbind
    
    ;; 4. Clear ALL shipit caches and global state (but preserve user settings)
    (when (fboundp 'shipit-clear-all-caches)
      (shipit-clear-all-caches))

    ;; Initialize completion cache if not bound
    (unless (boundp 'shipit--completion-cache)
      (setq shipit--completion-cache nil))
    
    ;; 5. Reinitialize if needed
    (when (fboundp 'shipit-enable)
      (condition-case err
          (shipit-enable)
        (error (message "Error enabling shipit: %s" (error-message-string err)))))
    
    ;; NOTE: We no longer add shipit to magit-status-sections-hook
    ;; All PR viewing happens in dedicated shipit buffers (shipit-mode), not magit-status

    ;; 7. Re-setup keybindings after reload
    ;; Set up global keybinding for shipit menu
    (global-set-key (kbd "C-c C-s") #'shipit)

    ;; Force keybinding setup in mode maps - override any existing bindings
    (when (boundp 'magit-status-mode-map)
      (define-key magit-status-mode-map (kbd "C-c C-s") nil)  ; Clear existing binding
      (define-key magit-status-mode-map (kbd "C-c C-s") #'shipit)
      (define-key magit-status-mode-map (kbd "C-c C-d") nil)  ; Clear existing binding
      (define-key magit-status-mode-map (kbd "C-c C-d") #'shipit-debug))
    (when (boundp 'magit-diff-mode-map)
      (define-key magit-diff-mode-map (kbd "C-c C-s") #'shipit)
      (define-key magit-diff-mode-map (kbd "C-c C-d") #'shipit-debug))
    (when (boundp 'magit-revision-mode-map)
      (define-key magit-revision-mode-map (kbd "C-c C-s") #'shipit)
      (define-key magit-revision-mode-map (kbd "C-c C-d") #'shipit-debug))
    
    ;; Also try the mode-based approach as backup
    (when (fboundp 'global-shipit-mode)
      (condition-case err
          (progn
            ;; Ensure the global mode variables exist first
            (unless (boundp 'global-shipit-mode-buffers)
              (setq global-shipit-mode-buffers nil))
            (global-shipit-mode 1))
        (error (message "Error enabling global-shipit-mode: %s" (error-message-string err)))))
    (when (fboundp 'shipit-setup-magit-integration)
      (condition-case err
          (shipit-setup-magit-integration)
        (error (message "Error in shipit-setup-magit-integration: %s" (error-message-string err)))))
    
    ;; Re-establish buffer-local keybindings (including 'g' handler) in all magit buffers
    (when (fboundp 'shipit--ensure-keybindings-in-buffer)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'magit-status-mode)
            (condition-case err
                (shipit--ensure-keybindings-in-buffer)
              (error (message "Error setting up keybindings in buffer %s: %s"
                            (buffer-name) (error-message-string err))))))))

    ;; 8. Auto-refresh all shipit-mode buffers to update stale content and keymaps
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'shipit-mode)
          (condition-case err
              (let ((pr-number shipit-buffer-pr-number)
                    (repo shipit-buffer-repo)
                    (repo-root default-directory)
                    (backend-id shipit-buffer-backend-id)
                    (backend-config shipit-buffer-backend-config))
                (when (and pr-number repo (fboundp 'shipit-mode))
                  ;; Kill the buffer and recreate it fresh - this ensures clean section structure
                  ;; and avoids issues with stale async callbacks
                  (let ((default-directory repo-root))
                    (kill-buffer)
                    (shipit-open-pr-buffer pr-number repo backend-id backend-config))))
            (error (message "Error refreshing shipit buffer %s: %s"
                          (buffer-name) (error-message-string err)))))))

    (message "Shipit reloaded successfully!"))))

;;;###autoload
(defun shipit-compile-and-reload ()
  "Clean, compile and reload shipit."
  (interactive)
  (let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
    (message "Compiling shipit...")
    (shell-command "make clean && make autoloads && make compile")
    (shipit-reload)))

(provide 'shipit-dev-utils)
;;; shipit-dev-utils.el ends here