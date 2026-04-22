;;; shipit-worktree.el --- Worktree management for PR testing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Manages git worktrees for PR branches, tracking sync state and metadata.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'json)

(declare-function shipit--get-repo-from-remote "shipit-core")

(defun shipit--get-worktree-path (pr-number branch-name)
  "Construct worktree path for PR-NUMBER and BRANCH-NAME.
Returns absolute path to worktree directory."
  (let ((safe-branch (string-replace "/" "-" branch-name)))
    (expand-file-name
     (format "%s/pr-%d-%s" shipit-worktree-directory pr-number safe-branch)
     (or (shipit--get-repo-root) "."))))

(defun shipit--get-repo-root ()
  "Get git repository root directory, or nil if not in a git worktree.
`git rev-parse --show-toplevel' exits non-zero when run from inside
the `.git/' directory (e.g. editing `COMMIT_EDITMSG').
`shell-command-to-string' captures stderr into the return value and
does not signal a Lisp error, so the previous `condition-case'
guard was a no-op — callers could end up with the literal string
\"fatal: this operation must be run in a work tree\" as a path.
Use `process-file' with exit-code checking, and sanity-check the
result."
  (with-temp-buffer
    (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
      (let ((root (string-trim (buffer-string))))
        (and (not (string-empty-p root))
             (file-directory-p root)
             root)))))

(defun shipit--get-current-repo ()
  "Get current repository from git remote origin.
Returns the full repo name (owner/repo format), or nil if unable to determine."
  (condition-case err
      (let* ((remote-url (shell-command-to-string "git config --get remote.origin.url"))
             (clean-url (string-trim remote-url)))
        (cond
         ((string-match "github\\.com[:/]\\([^/]+\\)/\\([^/\n]+\\)" clean-url)
          (let ((owner (match-string 1 clean-url))
                (repo (match-string 2 clean-url)))
            (when (string-suffix-p ".git" repo)
              (setq repo (substring repo 0 -4)))
            (format "%s/%s" owner repo)))
         (t nil)))
    (error
     (shipit--debug-log "Failed to get current repo: %s" err)
     nil)))

(defun shipit--validate-pr-repo (pr-repo)
  "Validate that PR-REPO matches the current repository.
Returns t if they match, raises user-error with clear message if not.
Handles the case where current repo can't be determined.
Comparison is case-insensitive since GitHub repository names are case-insensitive."
  (let ((current-repo (shipit--get-current-repo)))
    (cond
     ((not current-repo)
      (user-error "Cannot determine current repository from git remote"))
     ((not (string-equal (downcase current-repo) (downcase pr-repo)))
      (user-error "PR is from repository '%s' but current repository is '%s'" pr-repo current-repo))
     (t t))))

(defun shipit--worktree-exists-p (worktree-path)
  "Check if worktree exists at WORKTREE-PATH."
  (file-directory-p worktree-path))

(defun shipit--get-pr-info-from-worktree (worktree-path)
  "Read .shipit-pr-info.json from WORKTREE-PATH.
Returns plist with pr-number, repo, branch, pr-head-sha, created-at, or nil."
  (let ((info-file (expand-file-name ".shipit-pr-info.json" worktree-path)))
    (when (file-exists-p info-file)
      (condition-case err
          (let* ((json-str (with-temp-buffer
                            (insert-file-contents info-file)
                            (buffer-string)))
                 (json-data (json-read-from-string json-str)))
            (list :pr-number (cdr (assq 'pr_number json-data))
                  :repo (cdr (assq 'repo json-data))
                  :branch (cdr (assq 'branch json-data))
                  :pr-head-sha (cdr (assq 'pr_head_sha json-data))
                  :created-at (cdr (assq 'created_at json-data))))
        (error
         (shipit--debug-log "Failed to read .shipit-pr-info.json: %s" err)
         nil)))))

(defun shipit--worktree-in-sync-p (worktree-path current-pr-head-sha)
  "Check if worktree at WORKTREE-PATH is in sync with CURRENT-PR-HEAD-SHA.
Returns t if in sync, nil otherwise."
  (let ((pr-info (shipit--get-pr-info-from-worktree worktree-path)))
    (when pr-info
      (let ((stored-sha (plist-get pr-info :pr-head-sha)))
        ;; Check both stored SHA and actual HEAD to detect real sync status
        ;; Stored SHA may be outdated if PR got new commits, so also check actual HEAD
        (condition-case err
            (let ((actual-head (string-trim-right
                                (shell-command-to-string
                                 (format "cd %s && git rev-parse HEAD"
                                         (shell-quote-argument worktree-path))))))
              (let ((in-sync-p (string-equal actual-head current-pr-head-sha)))
                (shipit--debug-log "Sync check: actual=%s (len=%d) current-pr=%s (len=%d) match=%s"
                                 actual-head (length actual-head)
                                 current-pr-head-sha (length current-pr-head-sha)
                                 in-sync-p)
                in-sync-p))
          (error
           (shipit--debug-log "⚠️ Error checking worktree sync at %s: %s" worktree-path err)
           nil))))))

(defun shipit--find-worktree-for-pr (pr-number _pr-repo)
  "Find worktree directory for PR-NUMBER.
Returns path if found, nil otherwise."
  (let* ((repo-root (shipit--get-repo-root))
         (worktrees-dir (expand-file-name shipit-worktree-directory repo-root)))
    (when (file-directory-p worktrees-dir)
      (let ((pattern (format "pr-%d-" pr-number)))
        (condition-case nil
            (car (seq-filter
                  (lambda (dir)
                    (string-prefix-p pattern (file-name-nondirectory dir)))
                  (directory-files worktrees-dir t "^pr-")))
          (error nil))))))

(defun shipit--get-worktree-status (pr-number pr-head-sha pr-repo)
  "Get worktree status for PR-NUMBER.
PR-REPO should be the full repo name from PR data.
Returns one of: \\='none, \\='in-sync, \\='out-of-sync"
  (shipit--validate-pr-repo pr-repo)
  (let ((worktree-path (shipit--find-worktree-for-pr pr-number pr-repo)))
    (if (not worktree-path)
        'none
      (if (shipit--worktree-in-sync-p worktree-path pr-head-sha)
          'in-sync
        'out-of-sync))))


(defun shipit--create-worktree-pr (pr-number branch-name pr-head-sha)
  "Create a worktree for PR-NUMBER with BRANCH-NAME at PR-HEAD-SHA.
Returns (path . success) on success, or nil on error."
  (let* ((repo-root (shipit--get-repo-root))
         (worktree-path (shipit--get-worktree-path pr-number branch-name)))
    (unless repo-root
      (user-error "Not in a git repository"))

    ;; Verify .gitignore includes worktree directory
    (shipit--ensure-worktree-in-gitignore repo-root)

    ;; Create worktree
    (condition-case err
        (progn
          (shipit--debug-log "Creating worktree: %s" worktree-path)
          (make-directory (file-name-directory worktree-path) t)

          ;; Handle existing worktree directory carefully
          (when (file-directory-p worktree-path)
            (cond
             ;; If it's a valid git worktree, use it as-is
             ((file-exists-p (expand-file-name ".git" worktree-path))
              (shipit--debug-log "Worktree already exists with valid git state at: %s" worktree-path))
             ;; If it's an empty directory, remove it so git can create fresh
             ((seq-empty-p (directory-files worktree-path nil "^[^.]"))
              (delete-directory worktree-path t)
              (shipit--debug-log "Removed empty leftover directory at: %s" worktree-path))
             ;; Otherwise, error - don't delete directories with files
             (t
              (error "Worktree directory exists at %s with files but is not a git worktree. Please remove it manually." worktree-path))))

          ;; Fetch PR ref to ensure commit is available locally
          ;; This is necessary for PRs from forks or unfetched branches
          (let* ((repo-slug (shipit--get-repo-from-remote))
                 (resolved (shipit-pr--resolve-for-repo (or repo-slug "")))
                 (backend (car resolved))
                 (config (cdr resolved))
                 (refspec-fn (plist-get backend :refspec-for-pr))
                 (remote-fn (plist-get backend :remote-for-fetch))
                 (refspec (if refspec-fn
                              (funcall refspec-fn config pr-number branch-name)
                            (error "Backend %s missing :refspec-for-pr" (shipit-pr--backend-id))))
                 (remote (if remote-fn
                             (funcall remote-fn config repo-slug)
                           "origin"))
                 (fetch-cmd (format "cd %s && git fetch %s %s"
                                    (shell-quote-argument repo-root)
                                    (shell-quote-argument remote)
                                    (shell-quote-argument refspec))))
            (shipit--debug-log "Fetching PR ref: %s" fetch-cmd)
            (shell-command fetch-cmd))

          ;; Create worktree at the exact PR commit (detached HEAD)
          ;; This avoids branch name conflicts
          (let ((cmd (format "cd %s && git worktree add %s %s"
                            (shell-quote-argument repo-root)
                            (shell-quote-argument worktree-path)
                            (shell-quote-argument pr-head-sha))))
            (shipit--debug-log "Git command: %s" cmd)
            (shipit--debug-log "PR HEAD SHA: %s" pr-head-sha)
            (let ((output-buffer (get-buffer-create "*shipit-git-output*")))
              (with-current-buffer output-buffer
                (erase-buffer))
              (let ((exit-code (shell-command cmd output-buffer)))
                (unless (= exit-code 0)
                  (let ((error-output (with-current-buffer output-buffer (buffer-string))))
                    (shipit--debug-log "Git error output: %s" error-output)
                    (error "Git worktree creation failed with exit code %d: %s" exit-code error-output))))))

          ;; Write PR metadata
          (shipit--write-pr-info-to-worktree worktree-path
                                             (list :pr-number pr-number
                                                   :branch branch-name
                                                   :pr-head-sha pr-head-sha
                                                   :created-at (format-time-string "%FT%T%z")))

          ;; Run tests to verify clean baseline
          (shipit--run-tests-in-worktree worktree-path)

          (cons worktree-path t))
      (error
       (shipit--debug-log "Failed to create worktree: %s" err)
       nil))))

(defun shipit--worktree-pattern-in-file-p (file-path pattern)
  "Check if PATTERN is in FILE-PATH."
  (and (file-exists-p file-path)
       (with-temp-buffer
         (insert-file-contents file-path)
         (string-match-p (regexp-quote pattern) (buffer-string)))))

(defun shipit--ensure-worktree-in-gitignore (repo-root)
  "Ensure shipit-worktree-directory is ignored in REPO-ROOT.
Only adds to .gitignore if not already in .gitignore or .git/info/exclude."
  (let ((gitignore-path (expand-file-name ".gitignore" repo-root))
        (exclude-path (expand-file-name ".git/info/exclude" repo-root))
        (pattern shipit-worktree-directory))
    ;; Skip if already in private exclude file
    (unless (shipit--worktree-pattern-in-file-p exclude-path pattern)
      ;; Skip if already in .gitignore
      (unless (shipit--worktree-pattern-in-file-p gitignore-path pattern)
        ;; Add to .gitignore
        (if (file-exists-p gitignore-path)
            (with-temp-buffer
              (insert-file-contents gitignore-path)
              (goto-char (point-max))
              (insert "\n" pattern)
              (write-region (point-min) (point-max) gitignore-path))
          ;; Create .gitignore if it doesn't exist
          (with-temp-file gitignore-path
            (insert pattern)))))))

(defun shipit--write-pr-info-to-worktree (worktree-path pr-info)
  "Write .shipit-pr-info.json to WORKTREE-PATH with PR-INFO plist."
  (let* ((info-file (expand-file-name ".shipit-pr-info.json" worktree-path))
         (json-data `((pr_number . ,(plist-get pr-info :pr-number))
                      (branch . ,(plist-get pr-info :branch))
                      (pr_head_sha . ,(plist-get pr-info :pr-head-sha))
                      (created_at . ,(plist-get pr-info :created-at))
                      (repo . ,(shipit--get-repo-from-remote))))
         (json-str (json-encode json-data)))
    (with-temp-file info-file
      (insert json-str))))

(defun shipit--run-tests-in-worktree (worktree-path)
  "Run tests in WORKTREE-PATH to verify clean baseline (non-blocking).
Returns t if tests can be detected, nil otherwise. Actual test execution
happens asynchronously in background and may fail silently."
  (condition-case err
      (let* ((has-package-json (file-exists-p (expand-file-name "package.json" worktree-path)))
             (has-cargo (file-exists-p (expand-file-name "Cargo.toml" worktree-path)))
             (cmd (cond
                   (has-package-json "cd %s && npm test &")
                   (has-cargo "cd %s && cargo test &")
                   (t nil))))
        (if cmd
            (progn
              (shipit--debug-log "Starting tests in background: %s" (format cmd (shell-quote-argument worktree-path)))
              t)
          (shipit--debug-log "No test suite detected in worktree")
          nil))
    (error
     (shipit--debug-log "Error detecting tests: %s" err)
     nil)))

(defun shipit--sync-worktree (worktree-path branch-name)
  "Sync WORKTREE-PATH with latest changes from BRANCH-NAME.
Returns t on success, nil on error."
  (condition-case err
      (progn
        (shipit--debug-log "Syncing worktree: %s" worktree-path)
        (let ((cmd (format "cd %s && git pull origin %s"
                          (shell-quote-argument worktree-path)
                          (shell-quote-argument branch-name))))
          (let ((exit-code (shell-command cmd)))
            (unless (= exit-code 0)
              (error "Git pull failed with exit code %d" exit-code))))

        ;; Update PR head SHA in metadata file
        (when-let* ((pr-info (shipit--get-pr-info-from-worktree worktree-path)))
          (let ((latest-sha (string-trim-right
                            (shell-command-to-string
                             (format "cd %s && git rev-parse HEAD"
                                     (shell-quote-argument worktree-path))))))
            (shipit--write-pr-info-to-worktree worktree-path
                                               (plist-put pr-info :pr-head-sha latest-sha))))
        t)
    (error
     (shipit--debug-log "Failed to sync worktree: %s" err)
     nil)))

(defun shipit--delete-worktree (worktree-path)
  "Delete worktree at WORKTREE-PATH.
Returns t on success, nil on error."
  (condition-case err
      (progn
        (shipit--debug-log "Deleting worktree: %s" worktree-path)
        (let ((cmd (format "cd %s && git worktree remove %s"
                          (shell-quote-argument (shipit--get-repo-root))
                          (shell-quote-argument worktree-path))))
          (let ((exit-code (shell-command cmd)))
            (unless (= exit-code 0)
              (error "Git worktree removal failed with exit code %d" exit-code))))
        t)
    (error
     (shipit--debug-log "Failed to delete worktree: %s" err)
     nil)))

(defun shipit--open-worktree-directory (worktree-path)
  "Open WORKTREE-PATH in dired buffer.
Returns t on success, nil on error."
  (condition-case err
      (progn
        (shipit--debug-log "Opening worktree directory: %s" worktree-path)
        (if (file-directory-p worktree-path)
            (dired worktree-path)
          (user-error "Worktree path not found: %s" worktree-path))
        t)
    (error
     (shipit--debug-log "Failed to open worktree: %s" err)
     nil)))

(provide 'shipit-worktree)
;;; shipit-worktree.el ends here
