;;; shipit-dev-utils.el --- Development utilities for shipit -*- lexical-binding: t; -*-

;;; Commentary:
;; Development utilities for working on shipit

;;; Code:

(defconst shipit-dev--reload-hooks
  '(post-command-hook pre-command-hook after-change-functions
    before-change-functions kill-buffer-hook magit-status-sections-hook
    magit-diff-sections-hook magit-revision-sections-hook)
  "The global hooks a reload empties of shipit functions before it unbinds them.
A hook that runs while the files load must not name an unbound
function: `load-file' runs `kill-buffer-hook' when it kills the buffer
it read a file in, so a shipit function there failed every load.
Loading the files adds the functions back.")

(defconst shipit-dev--load-files
  '("shipit-faces.el"
    "shipit-lib.el" "shipit-sections.el" "shipit-core.el" "shipit-cache.el" "shipit-comments.el"
    "shipit-custom-edit.el"
    "shipit-stack.el"
    "shipit-stack-plan.el"
    "shipit-stacked-preview.el"
    "shipit-worktree.el" "shipit-http.el" "shipit-gh-etag.el"
    "shipit-issue-backends.el" "shipit-issue-titles.el"
    "shipit-issue-github.el" "shipit-issue-jira.el" "shipit-issue-gitlab.el" "shipit-issue-gitbug.el"
    "shipit-pr-backends.el" "shipit-pr-github.el"
    "shipit-gitlab-http.el" "shipit-pr-gitlab.el"
    "shipit-comment-backends.el" "shipit-comment-github.el" "shipit-comment-gitlab.el"
    "shipit-render.el" "shipit-diff.el" "shipit-notifications.el"
    "shipit-releases-buffer.el" "shipit-notifications-buffer.el"
    "shipit-checks.el" "shipit-actions.el"
    "shipit-pr-search.el" "shipit-file-filter.el"
    "shipit-rounded-section.el"
    "shipit-pr-sections.el" "shipit-pr-actions.el" "shipit-actions-list.el"
    "shipit-pr-diff.el"
    "shipit-pr-linked-issue.el"
    "shipit-buffer.el" "shipit-commands.el"
    "shipit-review-history.el" "shipit-review-mode.el" "shipit-review-revision.el"
    "shipit-preview.el" "shipit-editor.el"
    "shipit-worktree-move.el"
    "shipit-review-queue.el"
    "shipit-issue-create.el"
    "shipit-issues.el" "shipit-issues-buffer.el"
    "shipit-issue-branch.el"
    "shipit-repo-buffer.el"
    "shipit-subscriptions-buffer.el"
    "shipit-commit-comments-buffer.el"
    "shipit-discussion-backends.el"
    "shipit-discussions-graphql.el"
    "shipit-discussions.el" "shipit-discussions-buffer.el"
    "shipit-canvas.el" "shipit-atlassian-board.el" "shipit-atlassian-history.el"
    "shipit-atlassian-dashboard.el"
    "shipit-atlassian-kanban.el" "shipit-stack-canvas.el"
    "shipit-actions-timeline.el"
    "shipit-notifications-rss.el"
    "shipit-notifications-external.el"
    "shipit-explore.el"
    "shipit-explore-buffer.el"
    "shipit-code-refs.el"
    "shipit-pr-mouse-preview.el"
    "shipit-magit-worktrees.el"
    "shipit-debug.el" "shipit.el")
  "The files of shipit that `shipit-reload' loads, in dependency order.
shipit-faces.el is a leaf (no requires) and must load before
shipit-core.el, which requires it.")

(defvar shipit-dev-reload-extra-files nil
  "Files of other packages that `shipit-reload' loads after shipit's own.
A package on top of shipit adds its files here, as absolute names,
in the order they load.  The reload unbinds every function whose
name starts with shipit-, so such a package loses its functions too.")

(defun shipit-dev--reload-paths (shipit-dir)
  "The files `shipit-reload' loads, in order: shipit's own, in SHIPIT-DIR,
then `shipit-dev-reload-extra-files'."
  (append (mapcar (lambda (file) (expand-file-name file shipit-dir))
                  shipit-dev--load-files)
          shipit-dev-reload-extra-files))

(defun shipit-dev--file ()
  "This file: on the load path, or else at the root of the project,
beside shipit's lisp directory, where the repository keeps it."
  (or (locate-library "shipit-dev-utils")
      (expand-file-name "../shipit-dev-utils.el"
                        (file-name-directory (locate-library "shipit-debug")))))

(defun shipit-dev--shipit-function-p (fn)
  "Non-nil when FN is the symbol of a shipit function."
  (and (symbolp fn) (string-prefix-p "shipit-" (symbol-name fn))))

(defun shipit-dev--remove-shipit-hooks ()
  "Remove the shipit functions from each hook of `shipit-dev--reload-hooks'.
The global value of a hook is emptied.  The buffer a reload is run from
can have a value of its own for a hook, as a terminal buffer has for
`kill-buffer-hook', and then the plain value is that of the buffer."
  (dolist (hook shipit-dev--reload-hooks)
    (when (default-boundp hook)
      (set-default hook (seq-remove #'shipit-dev--shipit-function-p
                                    (default-value hook))))))

(defun shipit-dev--load-file (path)
  "Load the file PATH from source; non-nil when it loaded.
A file that fails is said, and the reload goes on with the next one."
  (let ((file (file-name-nondirectory path)))
    (message "Loading %s..." file)
    (condition-case err
        ;; `load-file' loads the exact .el file, not a .elc beside it.
        (progn (load-file path) t)
      (error (message "Error loading %s: %s" file (error-message-string err))
             nil))))

(defun shipit-dev--run-after-load (path)
  "Run the forms that wait for the feature of the file PATH.
They are the forms of `with-eval-after-load' for the feature that is
named as the file."
  (dolist (function (cdr (assq (intern (file-name-base path)) after-load-alist)))
    (condition-case err
        (funcall function)
      (error (message "Error after loading %s: %s"
                      (file-name-nondirectory path) (error-message-string err))))))

(defun shipit-dev--load-files (paths)
  "Load each of PATHS from source, and then run what waits for them.
A form of `with-eval-after-load' runs each time its file is loaded.  A
reload has unbound every function, so a form that waits for one file
and calls a function of a later one would fail.  The configuration of a
user does, when it starts a package on top of shipit as soon as shipit
is loaded.  So these forms wait until the last file is loaded."
  (let ((loaded nil))
    (let ((after-load-alist nil))
      (dolist (path paths)
        (when (and (file-exists-p path) (shipit-dev--load-file path))
          (push path loaded))))
    (mapc #'shipit-dev--run-after-load (nreverse loaded))))

(defun shipit-dev--own-functions ()
  "Return the functions that this file defines, as `load-history' has them."
  (let ((entry (seq-find (lambda (entry)
                           (and (stringp (car entry))
                                (equal "shipit-dev-utils" (file-name-base (car entry)))))
                         load-history)))
    (delq nil (mapcar (lambda (definition)
                        (and (eq (car-safe definition) 'defun) (cdr definition)))
                      (cdr entry)))))

(defun shipit-dev--functions-to-unbind ()
  "Return the shipit functions that a reload unbinds before it loads them.
The functions of this file stay bound: the reload calls them after it
unbinds the others, and `shipit-reload' is the command of the next
reload."
  (let ((own (shipit-dev--own-functions))
        (functions nil))
    (mapatoms (lambda (symbol)
                (when (and (fboundp symbol)
                           (shipit-dev--shipit-function-p symbol)
                           (not (memq symbol own)))
                  (push symbol functions))))
    functions))

(defvar shipit--reload-bootstrapped nil
  "Non-nil while `shipit-reload' runs the copy of itself that it just loaded.
That copy then does not load this file again.")

;;;###autoload
(defun shipit-reload (&optional compile-first)
  "Properly reload shipit from source files.
With prefix argument COMPILE-FIRST, run make clean && make compile first."
  (interactive "P")
  (catch 'shipit-reload-done
    ;; Bootstrap: reload this file first to ensure we're using the latest version
    (let ((this-file (or load-file-name (shipit-dev--file))))
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

      ;; Load shipit's files in dependency order, then those of packages
      ;; on top of it.
      (let ((load-paths (shipit-dev--reload-paths shipit-dir)))
        ;; First pass: Delete all .elc bytecode files to force fresh load
        (dolist (path load-paths)
          (let ((elc-path (concat (file-name-sans-extension path) ".elc")))
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
        (shipit-dev--remove-shipit-hooks)
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
        (mapc #'fmakunbound (shipit-dev--functions-to-unbind))
        ;; Fifth pass: Load the .el files, and then run what waits for them
        (shipit-dev--load-files load-paths))
      
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
    ;; Prefer `shipit-init' (modern API) -- it eagerly loads all
    ;; modules + warms up auth-source so the first post-reload HTTP
    ;; call has a token.  `shipit-enable' is the legacy name kept for
    ;; folks with old configs; try both.
    (when (or (fboundp 'shipit-init) (fboundp 'shipit-enable))
      (condition-case err
          (if (fboundp 'shipit-init) (shipit-init) (shipit-enable))
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
          ;; Capture the buffer name BEFORE killing -- after the kill
          ;; the current buffer flips to whatever Emacs picks
          ;; (typically `*Messages*'), and that's the name the error
          ;; handler used to report, which made every failure look
          ;; like it was about `*Messages*'.
          (let ((orig-name (buffer-name)))
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
                              orig-name (error-message-string err))))))))

    ;; 8b. Kill cached display buffers whose CONTENTS are baked from the
    ;; reloaded code (legend layout, etc.).  Without this the buffer
    ;; survives the reload with stale text even though every
    ;; function-cell is fresh -- user-facing symptom: \"I don't see
    ;; the new changes\" after a layout commit.
    (dolist (name (list "*shipit diff actions*"))
      (when-let ((buf (get-buffer name)))
        (let ((win (get-buffer-window buf t)))
          (when (window-live-p win)
            (condition-case nil (delete-window win) (error nil))))
        (condition-case nil (kill-buffer buf) (error nil))))
    ;; 8c. Reset cross-session toggles that `defvar' won't touch on
    ;; reload (defvar only initialises unbound vars).  Without this
    ;; the user is stuck on whichever expand state was last toggled
    ;; via `?', and a layout edit to the minimal view never seems
    ;; to take effect because the var is t from a prior session.
    (when (boundp 'shipit-review--actions-legend-expanded)
      (setq shipit-review--actions-legend-expanded nil))

    ;; 9. Re-engage shipit-review-mode in any worktree file buffers so they
    ;; pick up freshly-loaded function definitions.  Killing the buffer
    ;; would lose unsaved edits; toggling the minor mode off + on re-runs
    ;; the setup hooks (overlay refresh, side-frame header update) with
    ;; the new code.  Buffer-local PR context (`shipit-review--pr-data',
    ;; `--head-sha', `--base-sha') is preserved across the toggle.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (bound-and-true-p shipit-review-mode)
                   (buffer-file-name))
          (condition-case err
              (progn
                (shipit-review-mode -1)
                (shipit-review-mode 1))
            (error (message "Error re-engaging review-mode in %s: %s"
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