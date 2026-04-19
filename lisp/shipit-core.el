;;; shipit-core.el --- core module -*- lexical-binding: t; -*-

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
;; core internals split from monolithic shipit.el

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'auth-source)

;; CRITICAL: Set system-time-locale globally to ensure HTTP date headers use ASCII.
;; This prevents "Multibyte text in HTTP request" errors from url.el using
;; locale-specific date formatting (e.g., "sø." for Sunday in Norwegian).
(setq system-time-locale "C")

(defvar-local shipit--general-comments-fetched nil
  "Flag indicating whether general comments have been fetched.")

(defvar shipit--general-comments-timer nil
  "Timer for delayed refresh of general comments section.")

(defvar-local shipit--cached-general-comments nil
  "Cached general comments for the current PR.")

(defvar-local shipit--just-deleted-comment-id nil
  "ID of comment that was just deleted.
To filter from API responses during eventual consistency.")

(defvar-local shipit--last-known-branch nil
  "The last known branch to detect branch changes.")

(defvar-local shipit--last-known-repo nil
  "The last known repository to detect repository changes.")


(defvar-local shipit--pr-detection-calls 0
  "Counter for PR detection calls for debugging.")

(defvar-local shipit--last-pr-detection-time nil
  "Timestamp of last PR detection call to prevent rapid redundant calls.")

(defvar-local shipit--pr-detection-in-progress nil
  "Flag to prevent concurrent PR detection calls.")


(defvar-local shipit--manual-refresh-in-progress nil
  "Flag indicating a manual refresh is in progress.")

(defvar-local shipit--sections-with-unread nil
  "List of section types that have unread activities.
Updated when activity data loads, cleared when activities are marked read.")

(defvar-local shipit--new-inline-comments nil
  "List of new inline comments detected since last view.
These are injected as synthetic activity events in the Activity timeline.")

(defvar shipit--comment-insertion-in-progress nil
  "Global flag to prevent reaction fetching during comment insertion.
Could cause buffer corruption.")

(defvar shipit--user-mutated-pr nil
  "Flag indicating user performed a PR mutation (comment, review, etc.).
When set, all activities should be marked as read on next refresh.")

(defvar-local shipit--pr-displayed-in-buffer nil
  "Flag indicating PR info was displayed in this buffer before.")

(defvar-local shipit--explicit-pr-operation nil
  "Flag indicating user is explicitly performing PR operations (not just menu browsing).")

(defvar shipit--refresh-pr-context nil
  "Stores (pr-number repo) context for refreshes.
Maintains displayed PR instead of switching to current branch PR.")

(defvar shipit--refresh-target-file-path nil
  "Stores target file path for diff creation.
Used to filter file-level comments to the specific file being diffed.")

(defvar shipit--current-displayed-pr nil
  "Global storage of (pr-number repo) of the last displayed PR.
For restoration after closing/reopening shipit section.
Made global so it persists across buffer changes and refreshes.")

(defvar-local shipit--diff-pr-context nil
  "Buffer-local (PR-NUMBER . REPO) for shipit-managed diff buffers.
Nil in regular commit/stash/log diffs.  Set by diff openers so the
`magit-diff-sections-hook' knows which PR owns this buffer.")

(defun shipit--in-shipit-context-p ()
  "Return non-nil if we're in a shipit-managed buffer context."
  (derived-mode-p 'shipit-mode))

(defvar shipit--reaction-cache (make-hash-table :test 'equal)
  "Cache for comment reactions to avoid repeated API calls.")

(defvar shipit--reaction-fetch-in-progress (make-hash-table :test 'equal)
  "Track which reactions are currently being fetched to prevent duplicate requests.")

(defvar shipit--reaction-summary-cache (make-hash-table :test 'equal)
  "Per-emoji count summaries for issues/PRs, keyed by \"pr-NUMBER\".
Each value is an alist like ((\"+1\" . 2500) (\"heart\" . 200)).
Populated from the `reactions' object returned in issue/PR GET responses
so we can render accurate counts without paginating every reactor.")

(defvar shipit--codeowners-cache (make-hash-table :test 'equal)
  "Cache for CODEOWNERS file content to avoid repeated API calls.
Key format: 'repo:base-ref-name' (e.g., 'owner/repo:main')")

;; Last-viewed timestamps are stored in shipit-gh-etag--persistent-cache
;; with key format "last-viewed:repo:pr-number"

(defgroup shipit nil
  "Code review interface for Emacs."
  :group 'tools
  :prefix "shipit-")

(defcustom shipit-github-token nil
  "GitHub personal access token.
When nil, falls back to auth-source lookup for github.com."
  :type '(choice (const :tag "None (use auth-source)" nil)
                 (string :tag "Token"))
  :group 'shipit)

(defcustom shipit-org-runners-token nil
  "Fine-grained token with org-level runners read permission.
Used for fetching self-hosted runners when the main token lacks
the required scope.  When nil, falls back to auth-source lookup
for host \"github.com/runners\", then the main token."
  :type '(choice (const :tag "None (use auth-source / main token)" nil)
                 (string :tag "Token"))
  :group 'shipit)

(defvar shipit--github-token-cache nil
  "Cached GitHub token from auth-source.  Reset by `shipit-clear-token-cache'.")

(defvar shipit--github-token-resolving nil
  "Non-nil while `shipit--github-token' is prompting for auth-source credentials.
Prevents re-entrant calls from stacking minibuffer prompts.")

(defun shipit--github-token ()
  "Return the GitHub token, checking `shipit-github-token' first, then auth-source.
Auth-source results are cached so the GPG passphrase is only prompted once.
Re-entrant calls while auth-source is prompting return nil."
  (or shipit-github-token
      shipit--github-token-cache
      (unless shipit--github-token-resolving
        (unwind-protect
            (progn
              (setq shipit--github-token-resolving t)
              (let ((found (car (auth-source-search :host "github.com" :max 1))))
                (when found
                  (let ((secret (plist-get found :secret)))
                    (setq shipit--github-token-cache
                          (if (functionp secret) (funcall secret) secret))))))
          (setq shipit--github-token-resolving nil)))))

(defvar shipit--org-runners-token-cache nil
  "Cached org runners token from auth-source.")

(defun shipit--org-runners-token ()
  "Return the org runners token.
Checks `shipit-org-runners-token' first, then auth-source with
host \"github.com/runners\", then returns nil (caller falls back
to main token)."
  (or shipit-org-runners-token
      shipit--org-runners-token-cache
      (let ((found (car (auth-source-search :host "github.com/runners" :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (setq shipit--org-runners-token-cache
                  (if (functionp secret) (funcall secret) secret)))))))

(defun shipit-clear-token-cache ()
  "Clear the cached GitHub token so the next call re-queries auth-source."
  (interactive)
  (setq shipit--github-token-cache nil)
  (setq shipit--org-runners-token-cache nil))

(defun shipit--warm-up-auth-source ()
  "Decrypt auth-source files synchronously so async requests don't prompt.
Call once at startup before any async HTTP work begins."
  (auth-source-search :host "warmup" :max 1))

(defcustom shipit-api-url "https://api.github.com"
  "API base URL (used by GitHub backend)."
  :type 'string
  :group 'shipit)

(defcustom shipit-per-page 30
  "Number of items to fetch per page from the API."
  :type 'integer
  :group 'shipit)

(defcustom shipit-worktree-directory ".worktrees/"
  "Directory where PR worktrees are created, relative to repository root.
Can be customized per-user or per-project (via CLAUDE.md).
Examples: '.worktrees/', 'worktrees/', '.git/worktrees/'"
  :type 'string
  :group 'shipit)

(defcustom shipit-code-refs-auto-enable t
  "When non-nil, `shipit-init' enables `global-shipit-code-refs-mode'.

This turns on issue-key highlighting (Jira-style keys like
`ZIVID-12345') in every `prog-mode' derivative automatically.
Set to nil in your init before calling `shipit-init' if you prefer
to enable the mode manually or per buffer."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-pr-fontify-hunks t
  "Whether to apply language syntax highlighting inside PR diff hunks.

When non-nil, shipit applies language-appropriate font-lock faces
to the code content of each line in the Files Changed section,
inferred from the filename extension and
`shipit--language-mode-alist'.  Diff add/remove/context backgrounds
stay in place and the syntax foreground is layered on top.

Fontification is synchronous and can add noticeable latency on
large PRs; toggle off with `shipit-pr-toggle-fontify-hunks' (`T f'
in shipit PR buffers) if it becomes a problem."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-issue-linked-prs-auto-detect t
  "Whether to auto-detect linked PRs by searching the PR backend.

When non-nil, the Linked PRs section in a shipit issue buffer runs
an `is:pr <issue-key>' search scoped to the issue buffer's repo
and merges the results with any manually-recorded links from
`shipit-pr-linked-issue-file'.  Disable to show manual links only."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-pr-refine-hunks t
  "Whether to highlight intra-line changes in PR diff hunks.

When non-nil, shipit uses `smerge-refine-regions' to mark the exact
characters that differ between paired `-'/`+' lines with the
`diff-refine-removed' and `diff-refine-added' faces, matching the
fine-diff style of ediff and `magit-diff-refine-hunk'.  Helpful for
spotting case/punctuation changes that are otherwise easy to miss.

Each paired `-'/`+' block invokes the external `diff' program; on
very large PRs this can add latency, in which case set to nil."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-section-visibility-indicators nil
  "Value to use for `magit-section-visibility-indicators' in shipit buffers.

Magit defaults to showing a left-fringe `>' (collapsed) or `v'
(expanded) marker on every expandable section.  Shipit buffers
contain many nested sections where these indicators add visual
noise, so the default here is nil to suppress them in shipit
buffers without changing the global magit default.

The value is applied buffer-locally; any format accepted by
`magit-section-visibility-indicators' works.  Set to the symbol
`inherit' to follow the global magit default instead."
  :type '(choice (const :tag "No indicators" nil)
                 (const :tag "Inherit global magit default" inherit)
                 (sexp  :tag "Custom value"))
  :group 'shipit)

(defun shipit--apply-section-defaults ()
  "Apply shipit buffer-local defaults for `magit-section-mode' customizations.
Call from the body of each shipit mode derived from `magit-section-mode'."
  (unless (eq shipit-section-visibility-indicators 'inherit)
    (setq-local magit-section-visibility-indicators
                shipit-section-visibility-indicators)))

(defvar shipit-current-repo nil
  "Current repository in the format \\='owner/repo\\='.")

(defconst shipit-reaction-choices
  '(("👍 +1" . "+1")
    ("👎 -1" . "-1")
    ("😄 laugh" . "laugh")
    ("😕 confused" . "confused")
    ("❤️ heart" . "heart")
    ("🎉 hooray" . "hooray")
    ("🚀 rocket" . "rocket")
    ("👀 eyes" . "eyes"))
  "Shared alist of emoji labels to reaction content types.
Used by PR and issue reaction toggle commands.")

(defvar shipit-max-comment-nesting-level 3
  "Maximum nesting level for comment threads to prevent excessive indentation.
When comments are nested deeper than this level, they will be flattened
to this maximum level to maintain readability.")

(defvar shipit--internal-result nil
  "Internal variable to prevent void-variable errors during evaluation.")


(defun shipit--parse-repo-from-url (url)
  "Parse owner/repo path from git remote URL.
Matches any git host (GitHub, GitLab, self-hosted) using SSH or HTTPS.
For nested paths (e.g. gitlab.com:org/subgroup/project), returns the full path."
  (let ((clean-url (string-trim url)))
    (cond
     ;; SSH: git@host:path/to/repo.git
     ((string-match "git@[^:]+:\\(.+\\)" clean-url)
      (let ((path (match-string 1 clean-url)))
        (when (string-suffix-p ".git" path)
          (setq path (substring path 0 -4)))
        path))
     ;; HTTPS: https://host/path/to/repo.git
     ((string-match "https?://[^/]+/\\(.+\\)" clean-url)
      (let ((path (match-string 1 clean-url)))
        (when (string-suffix-p ".git" path)
          (setq path (substring path 0 -4)))
        path))
     (t nil))))

(defun shipit--get-repo-from-remote ()
  "Extract repository owner/name from git remote URL."
  (let ((remote-url (shell-command-to-string "git config --get remote.origin.url")))
    (shipit--parse-repo-from-url remote-url)))

(defun shipit--detect-backend-from-remote ()
  "Detect PR backend symbol from the git remote URL.
Iterates registered PR backends looking for a `:detect-url-pattern'
match.  Falls back to `github' when no pattern matches."
  (let ((url (string-trim
              (shell-command-to-string
               "git config --get remote.origin.url 2>/dev/null"))))
    (or (when (boundp 'shipit-pr-backends)
          (cl-loop for (id . plist) in (symbol-value 'shipit-pr-backends)
                   when (let ((pat (plist-get plist :detect-url-pattern)))
                          (and pat (string-match-p pat url)))
                   return id))
        'github)))

(defun shipit--ensure-repository ()
  "Ensure shipit-current-repo is set.
In a shipit buffer, uses the buffer's repo (shipit-buffer-repo).
Otherwise, detects from current directory's git remote.
Returns the repository string or nil if it cannot be determined."
  (shipit--debug-log "🔍 Repository detection - current dir: %s, current repo: %s, buffer-repo: %s"
                     default-directory shipit-current-repo
                     (if (boundp 'shipit-buffer-repo) shipit-buffer-repo "unbound"))

  ;; In a shipit buffer, use the buffer's repo - this is authoritative
  ;; Check both PR buffers (shipit-buffer-repo) and issue buffers (shipit-issue-buffer-repo)
  (let ((buffer-repo (or (and (boundp 'shipit-buffer-repo) shipit-buffer-repo)
                         (and (boundp 'shipit-issue-buffer-repo) shipit-issue-buffer-repo))))
    (if buffer-repo
        (progn
          (unless (string= buffer-repo shipit-current-repo)
            (shipit--debug-log "📌 Using buffer-repo: %s (was: %s)" buffer-repo shipit-current-repo)
            (setq shipit-current-repo buffer-repo))
          shipit-current-repo)
      ;; Not in shipit buffer - detect from git remote
      (condition-case err
          (when-let* ((repo (shipit--get-repo-from-remote)))
            (shipit--debug-log "✅ Detected repository from git remote: %s" repo)
            (unless (string= repo shipit-current-repo)
              (shipit--debug-log "🔄 Updating shipit-current-repo from %s to %s" shipit-current-repo repo)
              (setq shipit-current-repo repo)))
        (error
         (shipit--debug-log "❌ Error during repository detection: %s" (error-message-string err))))

      ;; Fallback: if we can't detect from git remote, use the manually set one
      (or shipit-current-repo
          (progn
            (shipit--debug-log "❌ Could not detect repository from git remote in %s" default-directory)
            (shipit--debug-log "❌ Checked git config --get remote.origin.url")
            (shipit--debug-log "❌ Repository detection failed completely")
            nil)))))

(defun shipit--clean-text (text)
  "Clean TEXT by removing Windows carriage returns, normalizing line endings,
and stripping internal shipit markers (like reply-to references).
Unicode handling is now done at the HTTP response level."
  (when text
    ;; First replace CRLF (\r\n) with LF (\n), then remove any remaining CR (\r)
    (let ((cleaned (replace-regexp-in-string "\r" ""
                     (replace-regexp-in-string "\r\n" "\n" text))))
      ;; Remove shipit-reply-to markers (invisible threading metadata)
      (replace-regexp-in-string "<!-- shipit-reply-to:[0-9]+ -->\\s-*\n?" "" cleaned))))

(defun shipit--highlight-code-snippet (code lang)
  "Apply syntax highlighting to CODE snippet in language LANG."
  (if (and lang (not (string-empty-p lang)))
      (let ((mode (shipit--get-mode-for-language (downcase lang))))
        (with-temp-buffer
          (insert code)
          (delay-mode-hooks
            (funcall mode)
            (font-lock-ensure))
          (buffer-string)))
    ;; Fallback: just add basic highlighting
    (propertize code 'font-lock-face 'font-lock-string-face)))

(defvar shipit-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'shipit-toggle-thread-fold)
    (define-key map (kbd "C-c C-f") 'shipit-fold-all-threads)
    (define-key map (kbd "C-c C-u") 'shipit-unfold-all-threads)
    (define-key map (kbd "n") 'shipit-next-thread)
    (define-key map (kbd "p") 'shipit-previous-thread)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for Shipit Comments mode.")

(defvar-local shipit-magit-integration nil
  "Whether shipit integration is enabled for this buffer.
This is buffer-local so each repository can have independent shipit state.")

(defcustom shipit-magit-show-closed-prs nil
  "Whether to include closed PRs when searching for PRs.
When nil, only open PRs are found.
When t, both open and closed PRs are included."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-auto-refresh nil
  "Whether to automatically refresh PR information.
When nil (default), PR information is only refreshed manually.
When t, PR information is automatically refreshed."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-debug-log-enabled nil
  "Whether to enable debug logging to a file.
When t, debug messages will be written to ~/.emacs.d/shipit-debug.log"
  :type 'boolean
  :group 'shipit)

(defcustom shipit-strip-emoji-variation-selectors t
  "Whether to strip emoji variation selectors to avoid display issues.
When t (default), removes U+FE0F and U+FE0E variation selectors from GitHub API
responses to prevent emojis from displaying as square boxes in terminals or
with incomplete font support. When nil, preserves original emoji formatting."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-dwim-hook nil
  "Hook run when shipit-dwim is invoked.
Functions in this hook are called with the current point position and context information.
Each function receives these arguments:
- point: The buffer position where shipit-dwim was invoked
- context: A symbol indicating the context ('pr-header, 'comment, 'no-data, 'general)
- bounds: Cons cell (start . end) of the relevant region, or nil if not applicable

This can be used to highlight regions, provide visual feedback, or perform other actions.

Example usage with built-in pulse:
  (add-hook 'shipit-dwim-hook 'shipit--highlight-region-with-pulse)

Or create your own highlighting function:
  (defun my-highlight-function (point context bounds)
    (when bounds
      (message \"Highlighting %s region at %s-%s\" context (car bounds) (cdr bounds))
      (pulse-momentary-highlight-region (car bounds) (cdr bounds))))
  (add-hook 'shipit-dwim-hook 'my-highlight-function)"
  :type 'hook
  :group 'shipit)

(defvar shipit--debug-log-file (expand-file-name "shipit-debug.log" user-emacs-directory)
  "Path to the shipit debug log file.")

(defcustom shipit-debug-log-max-lines 1000
  "Maximum number of lines to keep in the debug log file.
When the log exceeds this limit, older entries are removed."
  :type 'integer
  :group 'shipit)

(defcustom shipit-debug-categories '(all)
  "List of debug categories to log. Use 'all to log everything.
Available categories:
  - all: Log everything (default)
  - file-diff: File diff operations
  - reactions: Reaction fetching and display
  - comments: Comment operations
  - api: API requests and responses
  - cache: Cache operations
  - buffer: Buffer refresh and rendering
  - notifications: Notification polling
  - checks: CI checks fetching
  - profiling: Performance timing operations"
  :type '(repeat (choice (const all)
                        (const file-diff)
                        (const reactions)
                        (const comments)
                        (const api)
                        (const cache)
                        (const buffer)
                        (const notifications)
                        (const checks)
                        (const profiling)))
  :group 'shipit)

(defun shipit--redact-secrets (str)
  "Replace known secret patterns in STR with ***."
  (let ((result str))
    ;; GitHub PATs: ghp_, ghu_, gho_ followed by alphanumeric chars
    (setq result (replace-regexp-in-string
                  "gh[puo]_[A-Za-z0-9]+" "***" result))
    ;; GitLab PATs: glpat- followed by alphanumeric, underscore, hyphen
    (setq result (replace-regexp-in-string
                  "glpat-[A-Za-z0-9_-]+" "***" result))
    ;; Bearer <token> — redact the token value after Bearer
    (setq result (replace-regexp-in-string
                  "\\(Bearer \\)\\([^ \")\n]+\\)" "\\1***" result))
    ;; token <value> — GitHub REST auth format (only when it looks like auth)
    (setq result (replace-regexp-in-string
                  "\\(token \\)\\([^ \")\n]+\\)" "\\1***" result))
    ;; PRIVATE-TOKEN: <value> — GitLab header
    (setq result (replace-regexp-in-string
                  "\\(PRIVATE-TOKEN: \\)\\([^ \")\n]+\\)" "\\1***" result))
    ;; Basic <base64> — Basic auth
    (setq result (replace-regexp-in-string
                  "\\(Basic \\)\\([A-Za-z0-9+/=]+\\)" "\\1***" result))
    result))

(defun shipit--debug-log (format-string &rest args)
  "Log a debug message to the shipit debug log file if logging is enabled.
Automatically rotates the log when it exceeds `shipit-debug-log-max-lines`.

If the first argument is a symbol in `shipit-debug-categories`, it's used as
the category and filtered accordingly. Otherwise all arguments are treated as
format string and args.

Examples:
  (shipit--debug-log 'file-diff \"Opening diff for %s\" file)
  (shipit--debug-log \"General message %s\" value)"
  (when shipit-debug-log-enabled
    (let* ((has-category (and args (symbolp format-string)))
           (category (if has-category format-string 'general))
           (actual-format (if has-category (car args) format-string))
           (actual-args (if has-category (cdr args) args))
           (should-log (or (memq 'all shipit-debug-categories)
                          (memq category shipit-debug-categories))))
      (when should-log
        (let ((message (shipit--redact-secrets (apply #'format actual-format actual-args)))
              (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
              (category-str (if has-category (format "[%s] " category) "")))
          (with-temp-buffer
            (when (file-exists-p shipit--debug-log-file)
              (insert-file-contents shipit--debug-log-file))
            (goto-char (point-max))
            (insert (format "[%s] %s%s\n" timestamp category-str message))

            ;; Rotate log if it's too long
            (let ((line-count (count-lines (point-min) (point-max))))
              (when (> line-count shipit-debug-log-max-lines)
                (goto-char (point-min))
                (forward-line (- line-count (/ shipit-debug-log-max-lines 2)))
                (delete-region (point-min) (point))
                (goto-char (point-min))
                (insert (format "[%s] === Log rotated (kept last %d lines) ===\n"
                               timestamp (/ shipit-debug-log-max-lines 2)))))

            (write-region (point-min) (point-max) shipit--debug-log-file nil 'silent)))))))

;; Performance timing helpers

(defmacro shipit--time-operation (description &rest body)
  "Execute BODY and log the execution time with DESCRIPTION.
Only logs if 'profiling or 'all is enabled in `shipit-debug-categories'."
  `(let ((start-time (current-time)))
     (prog1 (progn ,@body)
       (when shipit-debug-log-enabled
         (let ((elapsed-ms (* 1000 (float-time (time-subtract (current-time) start-time)))))
           (shipit--debug-log 'profiling "⏱️ %s: %.2fms" ,description elapsed-ms))))))

;; Hierarchical profiling system for accurate timing analysis
(defvar shipit--profiling-stack nil
  "Stack of currently active profiling operations (operation-name start-time).")

(defvar shipit--profiling-tree nil
  "Tree of profiling data: (name total-time self-time children).")

(defun shipit--profiling-start (operation-name)
  "Start profiling OPERATION-NAME, returns a timer token."
  (when shipit-debug-log-enabled
    (let ((token (list operation-name (current-time) (length shipit--profiling-stack))))
      (push token shipit--profiling-stack)
      token)))

(defun shipit--profiling-end (token)
  "End profiling for TOKEN, records timing in tree."
  (when (and shipit-debug-log-enabled token shipit--profiling-stack)
    (let* ((elapsed-ms (* 1000 (float-time (time-subtract (current-time) (cadr token)))))
           (operation-name (car token))
           (depth (caddr token)))
      ;; Remove from stack
      (pop shipit--profiling-stack)
      ;; Record in tree structure
      (shipit--profiling-record operation-name elapsed-ms depth))))

(defun shipit--profiling-record (name elapsed depth)
  "Record profiling data for NAME with ELAPSED ms at DEPTH level."
  ;; Simple flat recording for now - can be enhanced to build tree
  (shipit--debug-log 'profiling "⏱️ [L%d] %s: %.2fms" depth name elapsed))

(defmacro shipit--time-operation-hier (description &rest body)
  "Execute BODY and track hierarchical timing with DESCRIPTION.
This replaces shipit--time-operation for better profiling."
  `(let ((token (shipit--profiling-start ,description)))
     (prog1 (progn ,@body)
       (when token
         (shipit--profiling-end token)))))

(defun shipit-generate-profiling-report ()
  "Generate hierarchical profiling report from debug log.
Parses timing entries and builds a tree showing parent-child relationships."
  (interactive)
  (if (not shipit--debug-log-file)
      (message "Debug logging not configured")
    (let ((log-buffer (find-file-noselect shipit--debug-log-file)))
      (with-current-buffer log-buffer
        (let ((timings '())
              (output-buffer (get-buffer-create "*shipit-profile-report*")))
          ;; Parse all profiling entries from log (including depth level if present)
          (save-excursion
            (goto-char (point-min))
            ;; Try to match both formats: with [L#] depth and without
            (while (re-search-forward "\\[profiling\\] ⏱️ \\(?:\\[L\\([0-9]+\\)\\] \\)?\\([^:]+\\): \\([0-9.]+\\)ms" nil t)
              (let ((has-depth (match-string 1))
                    (name (match-string 2))  ;; Always group 2
                    (elapsed (string-to-number (match-string 3))))  ;; Always group 3
                (when name
                  (let ((depth (if has-depth (string-to-number has-depth) 0)))
                    (push (list name elapsed depth) timings))))))

          ;; Generate report
          (with-current-buffer output-buffer
            (erase-buffer)
            (insert "=== Shipit Profiling Report ===\n\n")
            (insert (format "Total entries: %d\n\n" (length timings)))

            ;; Sort by elapsed time descending
            (let ((sorted (sort (copy-sequence timings)
                               (lambda (a b) (> (cadr a) (cadr b))))))
              (insert "Top 20 slowest operations:\n")
              (dolist (timing (seq-take sorted 20))
                (let ((indent (make-string (* 2 (caddr timing)) ?\ ))
                      (name (car timing))
                      (elapsed (cadr timing)))
                  (insert (format "%s%s: %.2f ms\n" indent name elapsed)))))

            ;; Total measured time
            (let ((total (apply '+ (mapcar 'cadr timings))))
              (insert (format "\nTotal measured time: %.2f ms\n" total)))

            ;; Check for unaccounted time
            (insert "\nNote: If measured time < actual time, there may be:\n")
            (insert "  - Parallel/async operations not captured\n")
            (insert "  - Operations without timing instrumentation\n")
            (insert "  - Network/IO wait time not tracked\n"))
          (display-buffer output-buffer))))))

;; Helper functions for shipit-dwim-hook

(defun shipit--highlight-region-with-pulse (point context bounds)
  "Highlight shipit region using built-in pulse.
POINT is the cursor position, CONTEXT is the shipit context, BOUNDS is (start . end)."
  (when bounds
    ;; Save the buffer reference to pulse in the correct buffer
    (let ((original-buffer (current-buffer)))
      (run-with-timer 0.01 nil
                      (lambda ()
                        (with-current-buffer original-buffer
                          (pulse-momentary-highlight-region (car bounds) (cdr bounds))))))

(defun shipit--highlight-modified-region-with-pulse (action context bounds)
  "Highlight modified shipit region using built-in pulse.
ACTION is the action type, CONTEXT is additional info, BOUNDS is (start . end)."
  (shipit--debug-log "Post-action hook called: action=%s context=%s bounds=%s" action context bounds)
  (when (memq action '(comment-added comment-edited general-comment-added comment-deleted))
    (let* ((is-inline-comment (cdr (assq 'is-inline-comment context))))
      (shipit--debug-log "Successfully processed %s comment action: %s"
                         (if is-inline-comment "inline" "general") action)
      ;; Content-aware pulsing: search all comment regions for the right type
      (let ((found-match nil)
            (total-comments 0))
        (save-excursion
          (goto-char (point-min))
          ;; First, check if General Comments section exists at all
          (if (re-search-forward "General Comments" nil t)
              (shipit--debug-log "Found General Comments section header")
            (shipit--debug-log "WARNING: No General Comments section found in buffer"))
          (goto-char (point-min))
          (let ((pos nil))
            (while (setq pos (text-property-search-forward 'shipit-comment))
              (setq total-comments (1+ total-comments)))
            (shipit--debug-log "Total comment regions found: %d" total-comments)
            (goto-char (point-min))
            (while (and (not found-match) (setq pos (text-property-search-forward 'shipit-comment)))
              (let* ((start (prop-match-beginning pos))
                     (end (prop-match-end pos))
                     (content (buffer-substring-no-properties start (min end (+ start 100)))))
                (shipit--debug-log "Checking comment content: %s" content)
                (if is-inline-comment
                    ;; For inline comments, look for "Line X:" pattern
                    (when (string-match "Line [0-9]+:" content)
                      (shipit--debug-log "Found matching inline comment, pulsing")
                      (pulse-momentary-highlight-region start end)
                      (setq found-match t))
                  ;; For general comments, avoid "Line X:" pattern
                  (when (not (string-match "Line [0-9]+:" content))
                    (shipit--debug-log "Found matching general comment, pulsing")
                    (pulse-momentary-highlight-region start end)
                    (setq found-match t)))))))
        (unless found-match
          (shipit--debug-log "No matching %s comment found for pulsing"
                             (if is-inline-comment "inline" "general"))))
      (message "Comment %s successfully" (if (eq action 'comment-edited) "updated" "added")))))))

(defvar shipit--expected-suite-count 0
  "Expected number of suites to process.")

(defvar shipit--processed-suite-count 0
  "Number of suites processed so far.")

(defvar shipit--current-buffer nil
  "Current buffer to update when processing is complete.")

(defcustom shipit-use-magit-sections-for-diff-comments t
  "Whether to use magit sections for hierarchical inline comments in diff buffers."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-collapse-descriptions t
  "Whether to collapse PR description sections by default.
When t, PR descriptions will be collapsed and can be expanded by clicking.
When nil, PR descriptions will be expanded by default."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-render-markdown nil
  "Whether to render markdown in descriptions and comments.
Uses markdown-mode's font-lock system for proper rendering if available.
Requires the 'markdown-mode' package to be installed."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-show-empty-review-messages t
  "Whether to show default messages for empty review actions.
When t, empty approvals show '✅ Approved this pull request',
change requests show '❌ Requested changes on this pull request',
and dismissals show '🚫 Dismissed previous review'.
When nil, empty review actions are hidden or show blank content."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-expand-code-urls t
  "Whether to expand code URLs into inline code snippets."
  :type 'boolean
  :group 'shipit)
(define-obsolete-variable-alias 'shipit-expand-github-urls 'shipit-expand-code-urls "2.0")

(defcustom shipit-custom-url-patterns nil
  "Alist of regex patterns to URL templates for custom link rendering.
Each element is (PATTERN . URL-TEMPLATE) where:
- PATTERN is a regexp with capture groups
- URL-TEMPLATE uses \\\\1, \\\\2 etc. for captured groups

Example for Jira tickets:
  \\='((\"PROJ-\\\\([0-9]+\\\\)\" . \"https://jira.example.com/browse/PROJ-\\\\1\"))"
  :type '(alist :key-type regexp :value-type string)
  :group 'shipit)

(defcustom shipit-repo-buffer-closed-limit 5
  "Number of closed PRs/issues to show initially in repo buffer sections.
Use the \"load more\" action to fetch additional closed items."
  :type 'integer
  :group 'shipit)

(defcustom shipit-issues-enabled nil
  "Whether to enable issue tracking search and viewing."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-discussions-enabled nil
  "Whether to enable GitHub Discussions search and viewing."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-issue-backend 'github
  "Default issue tracking backend.
Can be overridden per-repo via `shipit-issue-repo-backends'
or per-directory via .dir-locals.el."
  :type '(choice (const github) (const jira) (symbol :tag "Custom"))
  :group 'shipit)
(put 'shipit-issue-backend 'safe-local-variable #'symbolp)

(defcustom shipit-issue-backend-config nil
  "Default config plist for the issue backend.
Can be overridden per-repo via `shipit-issue-repo-backends'.
GitHub: nil (auto-detects repo).  Jira example:
  (:base-url \"https://jira.example.com\"
   :project-keys (\"PRJ\"))"
  :type 'plist
  :group 'shipit)
(put 'shipit-issue-backend-config 'safe-local-variable #'listp)


(defcustom shipit-issue-repo-backends nil
  "Per-repo issue backend configuration.
Alist of (REPO-PATTERN . CONFIG-PLIST) where:
- REPO-PATTERN is matched against the repo name (owner/name).
  Supports exact match or regexp.
- CONFIG-PLIST must include :backend (a symbol like \\='jira),
  plus any backend-specific keys.

Repos not matching any entry fall back to `shipit-issue-backend'
and `shipit-issue-backend-config'.

Example for use-package:
  :custom
  (shipit-issue-repo-backends
   \\='((\"myorg/jira-proj\" :backend jira
      :base-url \"https://jira.example.com\"
      :project-keys (\"PRJ\"))
     (\"myorg/other\" :backend jira
      :base-url \"https://jira.example.com\"
      :project-keys (\"OTH\"))))"
  :type '(alist :key-type string :value-type plist)
  :group 'shipit)

(defconst shipit-issue-backend-features
  '((jira   . shipit-issue-jira)
    (gitlab . shipit-issue-gitlab))
  "Alist mapping issue backend IDs to their feature (require) names.
Used by `shipit-init' for deferred loading of non-default backends.
GitHub is always loaded, so it is not listed here.")

(defcustom shipit-issue-subscribed-repos nil
  "List of repos available for issue search.
Each entry is a string \"owner/repo\".  These appear in the repo
selector of `shipit-advanced-issue-search' alongside the
current repo (auto-detected from git remote).

Example:
  :custom
  (shipit-issue-subscribed-repos
   \\='(\"myorg/frontend\" \"myorg/backend\" \"myorg/infra\"))"
  :type '(repeat string)
  :group 'shipit)

(defcustom shipit-pr-backend 'auto
  "PR backend to use.
When `auto', detects from the git remote URL (github.com → github,
gitlab.com → gitlab).  Can be overridden per-directory via .dir-locals.el."
  :type '(choice (const auto) (const github) (const gitlab) (symbol :tag "Custom"))
  :group 'shipit)
(put 'shipit-pr-backend 'safe-local-variable #'symbolp)

(defcustom shipit-pr-backend-config nil
  "Default config plist for the PR backend.
GitHub: nil (auto-detects repo).  GitLab example:
  (:base-url \"https://gitlab.example.com\")"
  :type 'plist
  :group 'shipit)
(put 'shipit-pr-backend-config 'safe-local-variable #'listp)

(defcustom shipit-show-pr-checks t
  "Whether to show CI/checks status in PR sections.
When t, shows checks with status categorization and clickable links.
When nil, shows disabled message."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-auto-collapse-comment-lines 42
  "Automatically collapse general comments longer than this many lines.
Comments with more than this number of lines will be collapsed by default.
Set to 0 to disable auto-collapsing."
  :type 'integer
  :group 'shipit)

(defcustom shipit-render-images t
  "Whether to render images inline in comments.
When enabled, image URLs will be downloaded and displayed inline.
Images are cached locally for performance."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-image-max-width nil
  "Maximum display width in pixels for inline images.
When nil, uses the window body width in pixels.
Set to an integer to use a fixed maximum width."
  :type '(choice (const :tag "Window width" nil)
                 (integer :tag "Fixed pixels"))
  :group 'shipit)

(defcustom shipit-image-max-height nil
  "Maximum display height in pixels for inline images.
When nil, uses the window body height in pixels.
Set to an integer to use a fixed maximum height."
  :type '(choice (const :tag "Window height" nil)
                 (integer :tag "Fixed pixels"))
  :group 'shipit)

(defcustom shipit-render-mermaid t
  "Whether to render mermaid diagrams inline.
Requires `mmdc' (mermaid-cli) to be installed.
Install via: npm install -g @mermaid-js/mermaid-cli"
  :type 'boolean
  :group 'shipit)

(defcustom shipit-mermaid-theme "dark"
  "Mermaid diagram theme.
Passed to mmdc via the -t flag."
  :type '(choice (const "default")
                 (const "dark")
                 (const "forest")
                 (const "neutral"))
  :group 'shipit)

(defcustom shipit-mermaid-background "transparent"
  "Background color for mermaid diagram PNGs.
Passed to mmdc via the -b flag.
Examples: \"transparent\", \"white\", \"#1e1e1e\"."
  :type 'string
  :group 'shipit)

(defcustom shipit-mermaid-max-width 800
  "Maximum display width (pixels) for rendered mermaid diagrams."
  :type 'integer
  :group 'shipit)

(defcustom shipit-show-avatars t
  "Whether to show user profile pictures in PR interface.
When enabled, downloads and caches user avatars from the API.
Avatars are stored in ~/.emacs.d/shipit-avatars/ and cached for 1 day."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-pr-preview-recent-activities 5
  "Number of recent activities to show in PR preview popup.
Displays the most recent timeline events (comments, reviews, etc.)
when previewing a pull request from the notifications section."
  :type 'integer
  :group 'shipit)

(defcustom shipit-show-unread-indicators t
  "Whether to show red dot indicators for unread activity items.
When enabled, activities newer than the last time you viewed the PR
will be marked with a red dot."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-round-avatars nil
  "Whether to make user avatars round (circular crop).
When enabled, requires ImageMagick (convert command) to be installed.
If ImageMagick is not available, falls back to square avatars.
Set to nil (default) to use square avatars without external dependencies."
  :type '(choice (const :tag "Square avatars (no dependencies)" nil)
                 (const :tag "Round avatars (requires ImageMagick)" t))
  :group 'shipit)

(defcustom shipit-comment-face 'magit-section-highlight
  "Face to use for displaying inline comments in diff buffers."
  :type '(choice (const :tag "Magit section highlight" magit-section-highlight)
                 (const :tag "Magit diff context highlight" magit-diff-context-highlight)
                 (const :tag "Custom comment face" shipit-comment-face)
                 (const :tag "Default face" default)
                 (face :tag "Custom face"))
  :group 'shipit)

(defcustom shipit-inline-comment-highlight-background nil
  "Background color for inline comments to make them stand out.
When nil (default), uses default background.
Recommended values: \"#fffacd\" (light yellow) or \"#f0f8ff\" (light blue)."
  :type '(choice (const :tag "No highlighting" nil)
                 (const :tag "Light yellow" "#fffacd")
                 (const :tag "Light blue" "#f0f8ff")
                 (string :tag "Custom color"))
  :group 'shipit)

(defcustom shipit-inline-comment-left-border t
  "Whether to show a left border on inline comments for visual separation."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-code-block-background nil
  "Background color for code blocks in markdown to make them stand out.
When nil, inherits from `markdown-code-face' if available.
Set to a color string to override with a custom color."
  :type '(choice (const :tag "Inherit from markdown-mode" nil)
                 (const :tag "GitHub light gray" "#f6f8fa")
                 (const :tag "Light yellow" "#fffacd")
                 (const :tag "Light blue" "#f0f8ff")
                 (string :tag "Custom color"))
  :group 'shipit)

(defcustom shipit-code-block-detect-shebang t
  "When non-nil, detect language from shebang lines in unfenced code blocks.
If a code block has no language identifier after the triple backticks,
check the first line for a shebang and use the interpreter to apply
syntax highlighting."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-inline-comment-icon-style 'emoji
  "Style of icons to use for inline comments.
emoji: Use emoji icons (💬 for comments, 📋 for reviews)
text: Use text markers ([C] for comments, [R] for reviews)
avatar: Use user avatars (requires shipit-show-avatars)"
  :type '(choice (const :tag "Emoji icons" emoji)
                 (const :tag "Text markers" text)
                 (const :tag "User avatars" avatar))
  :group 'shipit)

(defcustom shipit-inline-comment-faces t
  "Whether to apply distinct faces to different parts of inline comments.
When enabled, usernames, timestamps, and comment bodies use distinct colors.
When disabled, all text uses default coloring."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-inline-comment-username-color "blue"
  "Color theme for inline comment usernames.
Choose from: blue (default), red, green, purple, orange, cyan."
  :type '(choice (const :tag "Blue (GitHub style)" "blue")
                 (const :tag "Red" "red")
                 (const :tag "Green" "green")
                 (const :tag "Purple" "purple")
                 (const :tag "Orange" "orange")
                 (const :tag "Cyan" "cyan"))
  :group 'shipit)

(defcustom shipit-inline-comment-timestamp-color "gray"
  "Color theme for inline comment timestamps.
Choose from: gray (default), blue, red, green, purple, orange."
  :type '(choice (const :tag "Gray (default)" "gray")
                 (const :tag "Blue" "blue")
                 (const :tag "Red" "red")
                 (const :tag "Green" "green")
                 (const :tag "Purple" "purple")
                 (const :tag "Orange" "orange"))
  :group 'shipit)

(defcustom shipit-username-foreground "#feacd0"
  "Foreground color for usernames throughout shipit.
Default is #feacd0 (pink). Set to any valid Emacs color."
  :type '(choice (const :tag "Pink (default)" "#feacd0")
                 (const :tag "Blue" "#0366d6")
                 (const :tag "Red" "#cb2431")
                 (const :tag "Green" "#28a745")
                 (const :tag "Purple" "#6f42c1")
                 (const :tag "Cyan" "#0098cc")
                 (string :tag "Custom color"))
  :group 'shipit)

(defcustom shipit-timestamp-foreground "#00d3d0"
  "Foreground color for timestamps and dates throughout shipit.
Default is #00d3d0 (cyan). Set to any valid Emacs color."
  :type '(choice (const :tag "Cyan (default)" "#00d3d0")
                 (const :tag "Gray" "#6a737d")
                 (const :tag "Blue" "#0366d6")
                 (const :tag "Red" "#cb2431")
                 (const :tag "Green" "#28a745")
                 (const :tag "Purple" "#6f42c1")
                 (string :tag "Custom color"))
  :group 'shipit)

(defcustom shipit-timestamp-format 'absolute
  "Format for displaying timestamps in shipit buffers.
Use `shipit-toggle-timestamp-format' or press `L' to toggle.
  - absolute: Show date and time (e.g., \"2025-12-17 14:52\")
  - relative: Show time since (e.g., \"2h ago\", \"3d ago\")"
  :type '(choice (const :tag "Absolute (2025-12-17 14:52)" absolute)
                 (const :tag "Relative (2h ago)" relative))
  :group 'shipit)

(defcustom shipit-filename-foreground "#6ae4b9"
  "Foreground color for filenames and paths throughout shipit.
Default is #6ae4b9 (teal). Set to any valid Emacs color."
  :type '(choice (const :tag "Teal (default)" "#6ae4b9")
                 (const :tag "Green" "#28a745")
                 (const :tag "Cyan" "#0098cc")
                 (const :tag "Blue" "#0366d6")
                 (const :tag "Purple" "#6f42c1")
                 (const :tag "Gray" "#6a737d")
                 (string :tag "Custom color"))
  :group 'shipit)

(defcustom shipit-comment-wrap-width 80
  "Text wrap width for comment bodies (default: 80 characters).
Use + and - to increase/decrease in the config menu."
  :type '(integer :tag "Characters per line")
  :group 'shipit)

(defcustom shipit-render-wrap-column 120
  "Wrap column width for rendering comment bodies (default: 120 characters).
This controls the base width before subtracting indentation.
Used in shipit-render module for wrapping blockquotes and regular text."
  :type '(integer :tag "Characters per line")
  :group 'shipit)

(defcustom shipit-enable-mouse-navigation nil
  "Whether to enable mouse navigation to file diffs when clicking on comments.
When non-nil, clicking anywhere on a comment will navigate to the file diff.
When nil (default), only keyboard navigation (RET) is available.
This respects the Emacs tradition of preferring keyboard over mouse interaction."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-show-activity-timeline t
  "Whether to show the Activity timeline section in PR view.
When enabled, displays chronological events like reviews, comments, labels, etc."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-activity-timeline-order 'reverse-chronological
  "Sort order for activity timeline events.
reverse-chronological: Newest events first (default)
chronological: Oldest events first"
  :type '(choice (const :tag "Newest first" reverse-chronological)
                 (const :tag "Oldest first" chronological))
  :group 'shipit)

(defcustom shipit-activity-show-commits t
  "Whether to show commit events in the activity timeline."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-activity-show-labels t
  "Whether to show label changes in the activity timeline."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-activity-show-assignments t
  "Whether to show assignment changes in the activity timeline."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-use-svglib-icons nil
  "Whether to use SVG icons (via svglib) instead of emoji throughout shipit.
When enabled and svglib is available, renders colored SVG icons for:
- Activity timeline events
- Check/CI status indicators
- Review/approval status
- Comment type indicators
- User/reviewer indicators
Falls back to emoji if svglib is not installed or rendering fails.
This provides better visual consistency and alignment compared to emoji rendering."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-preview-default-draft nil
  "Whether new PRs created from preview mode should default to draft status.
When non-nil, the preview buffer will start with draft mode enabled,
and the confirmation will show \"Create draft PR\" instead of \"Create PR\".
This can be toggled per-PR in the preview buffer before creation."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-preview-default-assignee 'self
  "Default assignee for new PRs created from preview mode.
When \\='self, automatically assigns yourself.
When \\='none, no default assignee is set.
This can be changed per-PR in the preview buffer before creation."
  :type '(choice (const :tag "Assign yourself" self)
                 (const :tag "No default assignee" none))
  :group 'shipit)

(defun shipit--expand-code-urls (text)
  "Expand code URLs in TEXT via the active backend's :expand-code-urls.
Returns TEXT unchanged if the backend has no :expand-code-urls."
  (let ((fn (plist-get (shipit-pr--get-backend) :expand-code-urls)))
    (if fn (funcall fn text) text)))
(defalias 'shipit--expand-github-urls #'shipit--expand-code-urls)

(defun shipit--format-timestamp (timestamp)
  "Format TIMESTAMP string based on `shipit-timestamp-format' setting.
When format is `absolute', shows \"2025-12-17 14:52\".
When format is `relative', shows \"2h ago\", \"3d ago\", etc.
The returned string has `shipit-raw-timestamp' text property for in-place updates."
  (if timestamp
      (let ((parsed-time (parse-time-string timestamp)))
        (if parsed-time
            (let ((formatted (if (eq shipit-timestamp-format 'relative)
                                 (shipit--format-timestamp-relative timestamp)
                               (format-time-string "%Y-%m-%d %H:%M" (apply 'encode-time parsed-time)))))
              (propertize formatted 'shipit-raw-timestamp timestamp))
          ""))
    ""))

(defun shipit--format-timestamp-relative (timestamp)
  "Format TIMESTAMP as relative time (e.g., \"2h ago\", \"3d ago\")."
  (condition-case nil
      (let* ((now (float-time))
             (then (float-time (date-to-time timestamp)))
             (diff (- now then))
             (minutes (/ diff 60))
             (hours (/ minutes 60))
             (days (/ hours 24))
             (weeks (/ days 7))
             (months (/ days 30))
             (years (/ days 365)))
        (cond
         ((< minutes 1) "just now")
         ((< minutes 60) (format "%dm ago" (floor minutes)))
         ((< hours 24) (format "%dh ago" (floor hours)))
         ((< days 7) (format "%dd ago" (floor days)))
         ((< days 30) (format "%dw ago" (floor weeks)))
         ((< days 365) (format "%dmo ago" (floor months)))
         (t (format "%dy ago" (floor years)))))
    (error "")))

(defun shipit--update-buffer-timestamps ()
  "Update all timestamps in current buffer to match `shipit-timestamp-format'.
Finds all text with `shipit-raw-timestamp' property and replaces it in-place.
Preserves field width (for right-aligned timestamps) and all existing text properties."
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (save-excursion
      (while (< pos (point-max))
        (let ((next-change (next-single-property-change pos 'shipit-raw-timestamp nil (point-max))))
          (when-let* ((raw-ts (get-text-property pos 'shipit-raw-timestamp)))
            (let* ((end (or (next-single-property-change pos 'shipit-raw-timestamp) (point-max)))
                   ;; Capture existing field width and properties
                   (old-width (- end pos))
                   (old-props (text-properties-at pos))
                   ;; Get new formatted text
                   (new-text (substring-no-properties (shipit--format-timestamp raw-ts)))
                   (new-width (string-width new-text))
                   ;; If old field was wider, right-align by padding left
                   (padded-text (if (> old-width new-width)
                                    (concat (make-string (- old-width new-width) ?\s) new-text)
                                  new-text))
                   (new-len (length padded-text)))
              (goto-char pos)
              (delete-region pos end)
              (insert padded-text)
              ;; Apply all the old properties to the new text (preserves faces, etc.)
              (set-text-properties pos (+ pos new-len) old-props)
              (setq next-change (+ pos new-len))))
          (setq pos (or next-change (point-max))))))))

(defvar shipit--emoji-support-cache nil
  "Cache for emoji support detection to avoid repeated checks.")

(defun shipit--can-display-emoji-p (emoji)
  "Test if EMOJI can be displayed properly in current Emacs environment."
  (and (display-graphic-p)  ; Only try fancy emoji in graphical Emacs
       (fboundp 'char-displayable-p)
       (char-displayable-p (string-to-char emoji))))

(defun shipit--detect-emoji-support ()
  "Detect emoji support level and cache the result."
  (unless shipit--emoji-support-cache
    (setq shipit--emoji-support-cache
          (cond
           ;; Test with a simple emoji that should work in most environments
           ((shipit--can-display-emoji-p "👍") 'full)
           ;; Test with basic Unicode symbols
           ((shipit--can-display-emoji-p "♥") 'basic)
           ;; Fall back to ASCII
           (t 'ascii))))
  shipit--emoji-support-cache)

(defun shipit-reset-emoji-detection ()
  "Reset emoji support detection cache and re-detect support level.
Useful if display environment changes (e.g., switching from terminal to GUI)."
  (interactive)
  (setq shipit--emoji-support-cache nil)
  (message "Emoji support re-detected: %s" (shipit--detect-emoji-support)))

;;; Per-activity read tracking for unread indicators
;; Uses the ETag persistent cache (shipit-gh-etag--persistent-cache) for storage
;; Stores a list of read activity event IDs per PR

(defvar shipit-gh-etag--persistent-cache)
(declare-function shipit-gh-etag--ensure-cache-loaded "shipit-gh-etag")
(declare-function shipit-gh-etag--save-cache "shipit-gh-etag")
(declare-function shipit-pr--backend-id "shipit-pr-backends")

(defun shipit--get-read-activities (repo pr-number)
  "Get the list of read activity event IDs for REPO and PR-NUMBER.
Returns a list of event IDs or nil if none marked as read."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let ((key (format "activities-read:%s:%s" repo pr-number)))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (gethash key shipit-gh-etag--persistent-cache))))

(defun shipit--is-activity-read-p (repo pr-number event-id)
  "Check if activity EVENT-ID is marked as read for REPO and PR-NUMBER."
  (let ((read-activities (shipit--get-read-activities repo pr-number)))
    (and read-activities (member event-id read-activities))))

(defun shipit--mark-activity-read (repo pr-number event-id)
  "Mark activity EVENT-ID as read for REPO and PR-NUMBER."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let* ((key (format "activities-read:%s:%s" repo pr-number))
         (read-activities (when (boundp 'shipit-gh-etag--persistent-cache)
                            (gethash key shipit-gh-etag--persistent-cache))))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (unless (member event-id read-activities)
        (puthash key (cons event-id read-activities) shipit-gh-etag--persistent-cache)
        (when (fboundp 'shipit-gh-etag--save-cache)
          (shipit-gh-etag--save-cache))))))

;; Inline comment tracking (for detecting new review comments/replies)
(defun shipit--get-seen-inline-comments (repo pr-number)
  "Get the list of seen inline comment IDs for REPO and PR-NUMBER.
Returns a list of comment IDs or nil if none tracked."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let ((key (format "inline-comments-seen:%s:%s" repo pr-number)))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (gethash key shipit-gh-etag--persistent-cache))))

(defun shipit--mark-inline-comments-seen (repo pr-number comment-ids)
  "Mark COMMENT-IDS as seen for REPO and PR-NUMBER.
COMMENT-IDS should be a list of comment ID numbers."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let ((key (format "inline-comments-seen:%s:%s" repo pr-number)))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (puthash key comment-ids shipit-gh-etag--persistent-cache)
      (when (fboundp 'shipit-gh-etag--save-cache)
        (shipit-gh-etag--save-cache)))))

;; General comments baseline tracking (for unread indicators)
;; Similar to inline comments, tracks which general comments existed when first viewed

(defun shipit--get-seen-general-comments (repo pr-number)
  "Get the list of seen general comment IDs for REPO and PR-NUMBER.
Returns a list of comment IDs or nil if none tracked."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let ((key (format "general-comments-seen:%s:%s" repo pr-number)))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (gethash key shipit-gh-etag--persistent-cache))))

(defun shipit--mark-general-comments-seen (repo pr-number comment-ids)
  "Mark COMMENT-IDS as seen general comments for REPO and PR-NUMBER.
COMMENT-IDS should be a list of comment ID numbers."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let ((key (format "general-comments-seen:%s:%s" repo pr-number)))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (puthash key comment-ids shipit-gh-etag--persistent-cache)
      (when (fboundp 'shipit-gh-etag--save-cache)
        (shipit-gh-etag--save-cache)))))

;; Individual comment read tracking (for unread indicators on comment headers)
;; Tracks which individual comments the user has "read" by visiting with cursor

(defun shipit--get-read-comments (repo pr-number)
  "Get the list of read comment IDs for REPO and PR-NUMBER.
Returns a list of comment IDs or nil if none marked as read."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let ((key (format "comments-read:%s:%s" repo pr-number)))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (gethash key shipit-gh-etag--persistent-cache))))

(defun shipit--is-comment-read-p (repo pr-number comment-id)
  "Check if comment COMMENT-ID is marked as read for REPO and PR-NUMBER."
  (let ((read-comments (shipit--get-read-comments repo pr-number)))
    (and read-comments (member comment-id read-comments))))

(defun shipit--mark-comment-read (repo pr-number comment-id)
  "Mark comment COMMENT-ID as read for REPO and PR-NUMBER."
  (when (fboundp 'shipit-gh-etag--ensure-cache-loaded)
    (shipit-gh-etag--ensure-cache-loaded))
  (let* ((key (format "comments-read:%s:%s" repo pr-number))
         (read-comments (when (boundp 'shipit-gh-etag--persistent-cache)
                          (gethash key shipit-gh-etag--persistent-cache))))
    (when (boundp 'shipit-gh-etag--persistent-cache)
      (unless (member comment-id read-comments)
        (puthash key (cons comment-id read-comments) shipit-gh-etag--persistent-cache)
        (when (fboundp 'shipit-gh-etag--save-cache)
          (shipit-gh-etag--save-cache))))))

(defun shipit--is-comment-unread-p (repo pr-number comment-id)
  "Check if COMMENT-ID is unread for REPO and PR-NUMBER.
Works for both inline comments and general comments using their respective baselines.
Returns nil if already marked as read."
  ;; If already marked as read via our comment-read tracking, it's not unread
  (when (not (shipit--is-comment-read-p repo pr-number comment-id))
    (let ((seen-inline (shipit--get-seen-inline-comments repo pr-number))
          (seen-general (shipit--get-seen-general-comments repo pr-number)))
      ;; A comment is unread if:
      ;; 1. It's in a baseline we're tracking AND not in that baseline
      ;; 2. We check BOTH baselines - if comment is in EITHER baseline, it's not new
      (cond
       ;; If comment is in the inline baseline, it's not unread
       ((and seen-inline (member comment-id seen-inline))
        nil)
       ;; If comment is in the general baseline, it's not unread
       ((and seen-general (member comment-id seen-general))
        nil)
       ;; If we have at least one baseline and comment is not in either, it's unread
       ((or seen-inline seen-general)
        t)
       ;; No baselines established yet, don't show as unread
       (t nil)))))

(defun shipit--check-for-new-inline-comments (repo pr-number comments)
  "Check if COMMENTS contains new inline comments not yet seen.
REPO and PR-NUMBER identify the PR.
COMMENTS is the list of inline comment alists from the API.
Returns t if there are new unseen comments, nil otherwise.
Also stores the new comments in `shipit--new-inline-comments' for injection
into the Activity timeline."
  (let* ((seen-ids (shipit--get-seen-inline-comments repo pr-number))
         (current-ids (mapcar (lambda (c) (cdr (assq 'id c))) comments)))
    (if (null seen-ids)
        ;; First time seeing this PR's inline comments - establish baseline
        (progn
          (shipit--mark-inline-comments-seen repo pr-number current-ids)
          (shipit--debug-log "INLINE-COMMENTS: Established baseline with %d comments" (length current-ids))
          (setq shipit--new-inline-comments nil)
          nil)  ; No "new" comments on first view
      ;; Check for comments not in seen list
      (let* ((new-ids (cl-remove-if (lambda (id) (member id seen-ids)) current-ids))
             (new-comment-objects (seq-filter
                                   (lambda (c)
                                     (member (cdr (assq 'id c)) new-ids))
                                   comments)))
        (shipit--debug-log "INLINE-COMMENTS: seen=%d current=%d new=%d"
                           (length seen-ids) (length current-ids) (length new-ids))
        (when new-ids
          (shipit--debug-log "INLINE-COMMENTS: New comment IDs: %S" new-ids))
        ;; Store new comment objects for Activity section injection
        (setq shipit--new-inline-comments new-comment-objects)
        (> (length new-ids) 0)))))

(defun shipit--check-for-new-general-comments (repo pr-number comments)
  "Check if COMMENTS contains new general comments not yet seen.
REPO and PR-NUMBER identify the PR.
COMMENTS is the list of general comment alists from the API.
Returns t if there are new unseen comments, nil otherwise.
Establishes baseline on first view."
  (let* ((seen-ids (shipit--get-seen-general-comments repo pr-number))
         (current-ids (mapcar (lambda (c) (cdr (assq 'id c))) comments)))
    (if (null seen-ids)
        ;; First time seeing this PR's general comments - establish baseline
        (progn
          (shipit--mark-general-comments-seen repo pr-number current-ids)
          (shipit--debug-log "GENERAL-COMMENTS: Established baseline with %d comments" (length current-ids))
          nil)  ; No "new" comments on first view
      ;; Check for comments not in seen list
      (let ((new-ids (cl-remove-if (lambda (id) (member id seen-ids)) current-ids)))
        (shipit--debug-log "GENERAL-COMMENTS: seen=%d current=%d new=%d"
                           (length seen-ids) (length current-ids) (length new-ids))
        (when new-ids
          (shipit--debug-log "GENERAL-COMMENTS: New comment IDs: %S" new-ids))
        ;; NOTE: Don't update baseline here - new comments should stay "unread"
        ;; until user visits them (added to comments-read cache)
        (> (length new-ids) 0)))))

(defun shipit--inline-comment-to-activity-event (comment)
  "Convert an inline COMMENT to an activity event format.
Returns an alist that can be inserted by `shipit--insert-activity-event'."
  (let* ((user (cdr (assq 'user comment)))
         (comment-id (cdr (assq 'id comment)))
         (path (cdr (assq 'path comment)))
         (body (cdr (assq 'body comment)))
         (created-at (cdr (assq 'created_at comment))))
    `((event . "line-commented")
      (id . ,(format "inline-%s" comment-id))
      (shipit-activity-comment-id . ,comment-id)
      (shipit-inline-comment-path . ,path)
      (user . ,user)
      (body . ,body)
      (path . ,path)
      (created_at . ,created-at))))

(defun shipit--activity-event-to-sections (event)
  "Map activity EVENT to corresponding section type(s).
EVENT is the full event alist from GitHub timeline API.
Returns a list of section types that should show unread indicators
when there are unread activities of this type."
  (let ((event-type (cdr (assq 'event event))))
    (pcase event-type
      ("commented"
       ;; Check if this is an inline comment (has path field) or general comment
       (if (cdr (assq 'path event))
           '(pr-files)  ; Inline comment
         '(general-comments)))  ; General comment
      ("reviewed" '(reviewers pr-files))  ; Reviews can have inline comments
      ("committed" '(pr-commits))
      ("labeled" '(labels))
      ("unlabeled" '(labels))
      ("assigned" '(assignees))
      ("unassigned" '(assignees))
      ("review_requested" '(reviewers))
      ("review_request_removed" '(reviewers))
      ("line-commented" '(pr-files))  ; Inline comments on files
      ("review_comment" '(pr-files))  ; Review comments on files
      ;; Events that don't map to specific sections
      (_ nil))))

(defun shipit--get-sections-with-unread-activities (repo pr-number events)
  "Get list of section types that have unread activities.
REPO and PR-NUMBER identify the PR, EVENTS is the list of timeline events.
Returns a list of section type symbols."
  (let ((read-activities (shipit--get-read-activities repo pr-number))
        (sections-with-unread '()))
    (shipit--debug-log "GET-SECTIONS-UNREAD: repo=%s pr=%s events=%d read-activities=%s"
                       repo pr-number (length events)
                       (if read-activities (length read-activities) "nil"))
    (when read-activities  ; Only check if we have baseline
      (dolist (event events)
        ;; Use 'id' for most events, but 'sha' for commit events
        (let ((event-id (or (cdr (assq 'id event))
                            (cdr (assq 'sha event))))
              (event-type (cdr (assq 'event event))))
          (shipit--debug-log "GET-SECTIONS-UNREAD: event-id=%s event-type=%s is-read=%s"
                             event-id event-type
                             (shipit--is-activity-read-p repo pr-number event-id))
          (when (and event-id
                     (not (shipit--is-activity-read-p repo pr-number event-id)))
            ;; This event is unread - add its sections
            (let ((sections (shipit--activity-event-to-sections event)))
              (shipit--debug-log "GET-SECTIONS-UNREAD: Unread event %s (path=%s) maps to sections: %S"
                                 event-type (cdr (assq 'path event)) sections)
              (dolist (section sections)
                (unless (memq section sections-with-unread)
                  (push section sections-with-unread))))))))
    (shipit--debug-log "GET-SECTIONS-UNREAD: Final sections with unread: %S" sections-with-unread)
    sections-with-unread))

(defun shipit--reaction-to-emoji (reaction-type)
  "Convert GitHub REACTION-TYPE to emoji with smart fallback based on environment support.
Pre-rendered emoji characters (e.g. from Jira's internal reactions API)
are returned as-is."
  (let ((support-level (shipit--detect-emoji-support)))
    (cond
     ((and (stringp reaction-type)
           (string-match-p "[^\x00-\x7F]" reaction-type))
      reaction-type)
     ((string= reaction-type "+1")
      (cond ((eq support-level 'full) "👍")
            (t "+1")))
     ((string= reaction-type "-1")
      (cond ((eq support-level 'full) "👎")
            (t "-1")))
     ((string= reaction-type "laugh")
      (cond ((eq support-level 'full) "😄")
            (t ":)")))
     ((string= reaction-type "confused")
      (cond ((eq support-level 'full) "😕")
            (t "?")))
     ((string= reaction-type "heart")
      (cond ((eq support-level 'full) "❤")      ; Simple heart without variation selector!
            ((eq support-level 'basic) "♥")     ; Unicode heart symbol
            (t "<3")))                          ; ASCII heart
     ((string= reaction-type "hooray")
      (cond ((eq support-level 'full) "🎉")
            (t "!")))
     ((string= reaction-type "rocket")
      (cond ((eq support-level 'full) "🚀")
            (t "^")))
     ((string= reaction-type "eyes")
      (cond ((eq support-level 'full) "👀")
            (t "o_o")))
     ;; Unknown reaction type — use raw name as fallback (e.g. GitLab custom emoji)
     (t (format ":%s:" reaction-type)))))

;;;###autoload

(defun shipit--suppress-request-404-errors (orig-fun format-string &rest args)
  "Suppress request 404 error messages for deleted comments.
This is an advice function for `message'."
  (let ((final-msg (if args (apply #'format format-string args) format-string)))
    (unless (and final-msg
                 (stringp final-msg)
                 (or (string-match-p "peculiar error:? 404" final-msg)
                     (string-match-p "API error:.*404" final-msg)
                     (string-match-p "http 404" final-msg)))
      (apply orig-fun format-string args))))

(defun shipit--enable-404-suppression ()
  "Enable suppression of request callback 404 errors."
  (advice-add 'message :around #'shipit--suppress-request-404-errors))

;; Enable 404 suppression when shipit loads
(shipit--enable-404-suppression)

;;; TEXT UTILITIES

(defun shipit--expand-tabs-to-spaces (line &optional indent-width)
  "Expand tabs in LINE to spaces, preserving column alignment.
INDENT-WIDTH is the number of spaces that will be prepended to the line later,
used to calculate correct tab stops (default 0).
Tabs align to multiples of 4 columns (GitHub's default for code blocks)."
  (let* ((starting-col (or indent-width 0))
         (tw 4)  ; tab width - GitHub uses 4 for code blocks
         (result "")
         (col starting-col))  ; Start at indent column
    (dolist (char (string-to-list line))
      (if (eq char ?\t)
          ;; Tab: add spaces to reach next tab stop
          (let ((spaces-needed (- tw (mod col tw))))
            (setq result (concat result (make-string spaces-needed ?\s)))
            (setq col (+ col spaces-needed)))
        ;; Regular char: add it and increment column
        (setq result (concat result (char-to-string char)))
        (setq col (1+ col))))
    result))

(defvar shipit--wrap-text-scratch-buffer nil
  "Module-level scratch buffer reused by `shipit--wrap-text'.
Callers like `shipit--render-comment-body' invoke `shipit--wrap-text'
once per line of a comment (hundreds of times for large comments);
allocating a fresh `with-temp-buffer' each call dominated render time.
Reusing a single hidden buffer eliminates that overhead.  Not
thread-safe, but Emacs is single-threaded for buffer work.")

(defun shipit--wrap-text-buffer ()
  "Return the shared wrap-text scratch buffer, creating it if needed."
  (or (and (buffer-live-p shipit--wrap-text-scratch-buffer)
           shipit--wrap-text-scratch-buffer)
      (setq shipit--wrap-text-scratch-buffer
            (get-buffer-create " *shipit-wrap-text*"))))

(defun shipit--wrap-text (text width &optional code-block-indent)
  "Wrap TEXT to WIDTH, preserving word boundaries and intentional newlines.
Lines inside code blocks (fenced with ```) are preserved exactly without
wrapping.  CODE-BLOCK-INDENT is the indent that will be added to lines
later (used to expand tabs correctly; default 0).

Uses the persistent `shipit--wrap-text-scratch-buffer' so repeated
calls — common when a caller wraps a long comment body one line at a
time to handle blockquotes — avoid the per-call buffer allocation
that would otherwise dominate.  The first call allocates the buffer;
every subsequent call reuses it via `erase-buffer'."
  (let ((in-code-block nil)
        (indent (or code-block-indent 0))
        (result '()))
    (with-current-buffer (shipit--wrap-text-buffer)
      (let ((fill-column width))
        (dolist (line (split-string text "\n"))
          (cond
           ((string-match-p "^\\s-*```" line)
            (setq in-code-block (not in-code-block))
            (push line result))
           (in-code-block
            (push (shipit--expand-tabs-to-spaces line indent) result))
           ((string-empty-p line)
            (push "" result))
           ((string-match-p "^|" line)
            (push line result))
           (t
            (erase-buffer)
            (insert line)
            (fill-region (point-min) (point-max))
            (push (buffer-string) result))))))
    (mapconcat #'identity (nreverse result) "\n")))

(defun shipit--humanize-workflow-name (filename)
  "Convert a workflow filename to a readable name."
  (let ((name (replace-regexp-in-string "[-_]" " " filename)))
    ;; Capitalize each word
    (mapconcat (lambda (word)
                 (concat (upcase (substring word 0 1))
                         (substring word 1)))
               (split-string name " ") " ")))

;;; Star indicator

(declare-function shipit--get-pr-field-icon "shipit-render" (field-type emoji-fallback))

(defun shipit--star-indicator (starred)
  "Return a display string for STARRED status.
Returns icon + Starred when starred, empty string when not."
  (if starred
      (format "  %s Starred" (shipit--get-pr-field-icon "star" "⭐"))
    ""))

;;; Targeted Section Refresh

;; shipit--find-section-by-type is defined in shipit-sections.el (recursive search)
(declare-function shipit--find-section-by-type "shipit-sections" (target-type))

(defun shipit--refresh-section-targeted (section-type heading-pattern item-count insert-content-fn)
  "Refresh a section using targeted update pattern.

SECTION-TYPE is the magit section type symbol (e.g., \\='labels, \\='assignees).
HEADING-PATTERN is a regex to match the count in heading (e.g., \"Labels (\\\\([0-9]+\\\\))\").
ITEM-COUNT is the new count to display in the heading.
INSERT-CONTENT-FN is a function called with the section object to insert new content.

This uses the safe magit section update pattern:
1. Find section by type
2. Extract marker positions as integers (markers become invalid after delete)
3. Clear children list
4. Delete content between content and end markers
5. Call INSERT-CONTENT-FN with magit-insert-section--parent bound
6. Update end and content markers

Returns t on success, nil if section not found."
  (let ((section (shipit--find-section-by-type section-type)))
    (if (not section)
        (progn
          (shipit--debug-log "SECTION-REFRESH: No %s section found, skipping targeted refresh" section-type)
          nil)
      (let ((inhibit-read-only t))
        (save-excursion
          ;; Step 1: Update the heading count (MUST happen before extracting
          ;; integer positions, since replace-match shifts subsequent positions)
          (goto-char (oref section start))
          (when (re-search-forward heading-pattern (line-end-position) t)
            (replace-match (number-to-string item-count) nil nil nil 1))

          ;; Step 2: Extract marker positions as integers AFTER heading update
          ;; (replace-match may have shifted positions)
          (let* ((content-pos (and (oref section content)
                                   (marker-position (oref section content))))
                 (end-pos (and (oref section end)
                               (marker-position (oref section end)))))
            (shipit--debug-log "SECTION-REFRESH: Found %s section, content=%s end=%s, updating with %d items"
                               section-type content-pos end-pos item-count)

            ;; Step 3: Clear old children list before inserting new ones
            (oset section children nil)

            ;; Step 4: Clear any washer function (deferred content insertion)
            (when (slot-boundp section 'washer)
              (oset section washer nil))

            ;; Step 5: Remove any invisible overlays in the content region
            (when (and content-pos end-pos)
              (remove-overlays content-pos end-pos 'invisible t))

            ;; Step 6: Delete body content if any exists, then insert new content
            (when (and content-pos end-pos)
              (when (> end-pos content-pos)
                (shipit--debug-log "SECTION-REFRESH: Deleting region %d to %d" content-pos end-pos)
                (delete-region content-pos end-pos))

              ;; Step 7: Insert new content at content position
              (goto-char content-pos)

              ;; Bind magit-insert-section--parent so children get proper parent
              (let ((magit-insert-section--parent section))
                (funcall insert-content-fn section))

              ;; Only add trailing newline if there are items
              (when (> item-count 0)
                (insert "\n"))

              ;; Step 8: Update section end marker to new position
              (oset section end (point-marker))

              ;; Step 9: Restore content marker
              (oset section content (copy-marker content-pos))

              ;; Step 10: Ensure section is marked as expanded (content is
              ;; now visible). Without this, sections created with HIDE=t
              ;; have hidden=t but visible content, causing the first TAB
              ;; to call magit-section-show (no-op) instead of hide.
              (oset section hidden nil)

              (shipit--debug-log "SECTION-REFRESH: Updated %s section, new end=%s, content at buffer: %S"
                                 section-type (point)
                                 (buffer-substring-no-properties content-pos (min (point) (+ content-pos 100)))))))

        ;; Force display update
        (when (fboundp 'magit-section-update-highlight)
          (magit-section-update-highlight))
        t))))

(provide 'shipit-core)
;;; shipit-core.el ends here
