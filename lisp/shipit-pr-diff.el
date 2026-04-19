;;; shipit-pr-diff.el --- Diff, ediff, and inline comment display -*- lexical-binding: t; -*-

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
;; File diff/ediff viewing, inline comment display in diffs, magit
;; advice hooks, and setup for magit integration.
;; Extracted from shipit-magit.el.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'shipit-lib)
(require 'shipit-core)
(require 'shipit-sections)
(require 'shipit-http)
(require 'shipit-gh-etag)
(require 'shipit-render)
(require 'shipit-comments)
(require 'shipit-pr-backends)
(require 'shipit-rounded-section)

(eval-when-compile
  (require 'magit-section))
(require 'magit-section)

;; Forward declarations — functions
(declare-function shipit--debug-log "shipit-core")
(declare-function shipit--get-comment-type-icon "shipit-render")
(declare-function shipit--get-comment-icon "shipit-render")
(declare-function shipit--render-markdown-content "shipit-render")
(declare-function shipit--clean-text "shipit-lib")
(declare-function shipit--get-repo-from-remote "shipit-core")
(declare-function shipit--ensure-repository "shipit-core")
(declare-function shipit-comment--fetch-reactions-batch "shipit-comments")
(declare-function shipit-comment--fetch-reactions "shipit-comments")
(declare-function shipit--insert-comment "shipit-pr-sections")
(declare-function shipit--insert-file-comment-with-replies "shipit-pr-sections")
(declare-function shipit--add-comment-to-pr "shipit-http")
(declare-function shipit--get-pr-review-details "shipit-http")
(declare-function shipit--group-inline-comments-by-file "shipit-http")
(declare-function shipit-gh-etag-invalidate-endpoint "shipit-gh-etag")
(declare-function shipit--should-use-review-mode-p "shipit-pr-diff")
(declare-function shipit--open-file-in-review-mode "shipit-pr-diff")
(declare-function shipit-refresh "shipit-pr-actions")
(declare-function shipit--in-general-comments-section-p "shipit-core")
(declare-function shipit--get-comment-unread-indicator "shipit-render")
(declare-function shipit--is-comment-unread-p "shipit-core")
(declare-function shipit--insert-status-hierarchical-comment "shipit-pr-sections")
(declare-function shipit--get-current-pr-data "shipit-http")
(declare-function shipit--fetch-inline-comments "shipit-http")
(declare-function shipit--group-comments-by-api-replies "shipit-http")
(declare-function shipit--insert-file-comment "shipit-pr-sections")
(declare-function shipit--format-timestamp "shipit-core")
(declare-function shipit--is-comment-in-resolved-thread "shipit-comments")
(declare-function shipit--insert-comment-body-only "shipit-render")
(declare-function shipit--try-overlay-action-at-point "shipit-render")
(declare-function shipit--format-comment-reactions "shipit-http")
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--expand-code-urls "shipit-core")
(declare-function shipit--wrap-comment-text "shipit-http")
(declare-function shipit--indent-comment-body "shipit-http")
(declare-function shipit--apply-comment-face-selectively "shipit-http")
(declare-function shipit--toggle-reaction-interactive "shipit-pr-actions")
(declare-function shipit--edit-comment-interactive "shipit-commands")
(declare-function shipit--highlight-region-with-pulse "shipit-core")
(declare-function shipit--highlight-modified-region-with-pulse "shipit-core")
(declare-function shipit--simple-comment-dialog "shipit-pr-actions")
(declare-function shipit--mark-commit-as-read "shipit-pr-sections")
(declare-function shipit--in-pr-section-p "shipit-pr-actions")
(declare-function shipit--in-shipit-context-p "shipit-core")
(declare-function shipit--position-cursor-on-diff-line "shipit-diff")
(declare-function shipit--ref-exists-locally "shipit-lib")
(declare-function shipit--range-needs-fetch-p "shipit-lib")
(declare-function shipit--fetch-branch-async "shipit-lib")
(declare-function shipit--fetch-branches-async "shipit-lib")
(declare-function shipit--fetch-pr-ref-async "shipit-lib")
(declare-function shipit-get-pr-files "shipit-http")
(declare-function shipit-get-pull-request "shipit-http")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")
(declare-function shipit-pr--backend-id "shipit-pr-backends")
(declare-function shipit--find-worktree-for-pr "shipit-worktree")
(declare-function shipit--worktree-in-sync-p "shipit-worktree")
(declare-function shipit-dwim "shipit-pr-actions")
(declare-function shipit-review-mode "shipit-review-mode")
(declare-function shipit "shipit")
(declare-function shipit-debug "shipit-debug")
(declare-function shipit-set-repository "shipit-commands")
(declare-function shipit--insert-threaded-file-comment "shipit-pr-sections")
(declare-function shipit--get-files-icon "shipit-render")
(declare-function magit-diff-wash-diffs "magit-diff")
(declare-function magit-diff-mode "magit-diff")
(declare-function magit-diff-range "magit-diff")
(declare-function magit-diff-setup-buffer "magit-diff")
(declare-function magit-diff-show-or-scroll-up "magit-diff")
(declare-function magit-show-commit "magit")
(declare-function magit-visit-thing "magit")
(declare-function magit-find-file "magit")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-get-upstream-branch "magit-git")
(declare-function magit-git "magit-git")
(declare-function magit-status "magit-status")
(declare-function magit-toplevel "magit-git")
(declare-function magit-current-section "magit-section")
(declare-function magit-section-match "magit-section")
(declare-function ediff-buffers "ediff")

;; Forward declarations — variables
(defvar shipit-use-magit-sections-for-diff-comments)
(defvar shipit--pending-commit-for-comments)
(defvar shipit--comment-retry-timer)
(defvar shipit--comment-retry-count)
(defvar shipit--comment-retry-max)
(defvar shipit-commit-keymap)
(defvar shipit-file-keymap)
(defvar shipit-inline-comment-faces)
(defvar shipit-inline-comment-username-color)
(defvar shipit-inline-comment-timestamp-color)
(defvar shipit-inline-comment-background-color)
(defvar shipit-inline-comment-username-face)
(defvar shipit-inline-comment-timestamp-face)
(defvar shipit-inline-comment-background-face)
(defvar shipit-comment-wrap-width)
(defvar shipit-render-wrap-column)
(defvar shipit-buffer-repo)
(defvar shipit-buffer-pr-data)
(defvar shipit-buffer-pr-number)
(defvar shipit--current-displayed-pr)
(defvar shipit--cached-inline-comments)
(defvar shipit--cached-general-comments)
(defvar shipit--inline-comments-fetched)
(defvar shipit--general-comments-fetched)
(defvar shipit--comment-cache)
(defvar shipit-current-repo)
(defvar shipit--refresh-pr-context)
(defvar shipit--refresh-target-file-path)
(defvar shipit--diff-pr-context)
(defvar shipit--target-file-path)
(defvar shipit--ediff-pr-number)
(defvar shipit--ediff-repo)
(defvar shipit--ediff-file-path)
(defvar shipit--ediff-head-sha)
(defvar shipit-render-markdown)
(defvar shipit-expand-code-urls)
(defvar shipit-enable-mouse-navigation)
(defvar shipit-dwim-hook)
(defvar shipit-post-action-hook)
(defvar magit-buffer-revision)
(defvar magit-diff-mode-map)
(defvar magit-revision-mode-map)
(defvar magit-section-mode-map)
(defvar magit-diff-mode-hook)
(defvar magit-revision-mode-hook)
(defvar magit-insert-section--parent)
(defvar magit-diff-sections-hook)
(defvar ediff-quit-hook)
(defvar ediff-temp-indirect-buffer)

;;; -------------------------------------------------------------------------
;;; Block 1: Repo checks + Patch + Diff range + Ediff
;;; -------------------------------------------------------------------------

(defun shipit--repo-available-locally-p (repo)
  "Return non-nil if REPO's git objects are accessible from current directory.
Combines `magit-toplevel' (are we in a git repo?) with
`shipit--local-repo-matches-p' (does it match REPO?)."
  (and (fboundp 'magit-toplevel)
       (magit-toplevel)
       (shipit--local-repo-matches-p repo)))

(defun shipit--local-repo-matches-p (repo)
  "Return non-nil if the local git origin matches REPO.
Compares the origin remote URL against REPO (owner/name format)."
  (let ((local-repo (shipit--get-repo-from-remote)))
    (and local-repo (string= local-repo repo))))

(defun shipit--get-pr-file-patch (_repo pr-number file-path)
  "Get the patch (unified diff) for FILE-PATH in PR-NUMBER.
Dispatches to the active PR backend's :fetch-files via `shipit-get-pr-files'."
  (condition-case _err
      (let ((files-data (shipit-get-pr-files pr-number)))
        (when files-data
          (let ((file-data (cl-find-if (lambda (file)
                                         (equal (cdr (assq 'filename file)) file-path))
                                       files-data)))
            (when file-data
              (cdr (assq 'patch file-data))))))
    (error nil)))

(defun shipit--create-magit-patch-buffer (repo pr-number file-path patch)
  "Create a proper Magit diff buffer with washed sections from PATCH data.
Uses magit-diff-wash-diffs to produce real magit sections (collapsible,
navigable with n/p, proper inline comment positioning)."
  (let ((buffer-name (format "*PR #%s: %s*" pr-number (file-name-nondirectory file-path))))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-diff-mode)
        ;; Build full diff text in --no-prefix format that magit-diff-wash-diffs expects
        (magit-insert-section (diffbuf)
          (magit-insert-heading
            (format "Changes in PR #%s  %s" pr-number file-path))
          (shipit--wash-patch-into-sections file-path patch))
        (setq buffer-read-only t)
        (setq shipit--current-displayed-pr (list pr-number repo))
        (setq-local shipit--diff-pr-context (cons pr-number repo))
        (setq-local shipit--target-file-path file-path)
        (goto-char (point-min))))
    buffer-name))

(defun shipit--wash-patch-into-sections (file-path patch)
  "Insert PATCH for FILE-PATH and wash it into proper magit sections."
  (let ((beg (point)))
    ;; Insert diff in --no-prefix format (what magit expects)
    (insert (format "diff --git %s %s\n" file-path file-path))
    (insert (format "--- %s\n" file-path))
    (insert (format "+++ %s\n" file-path))
    (insert patch)
    (unless (bolp) (insert "\n"))
    ;; Wash the raw text into magit sections
    (save-restriction
      (narrow-to-region beg (point))
      (goto-char beg)
      (magit-diff-wash-diffs nil))))

(defun shipit--show-pr-file-patch (repo pr-number file-path &optional target-line)
  "Show GitHub's patch for FILE-PATH in a proper Magit diff buffer."
  (let ((patch (shipit--get-pr-file-patch repo pr-number file-path)))
    (if patch
        (let ((buffer-name (shipit--create-magit-patch-buffer repo pr-number file-path patch)))
          (switch-to-buffer buffer-name)
          ;; Position cursor on target line if provided
          (when target-line
            (goto-char (point-min))
            (when (re-search-forward (format "^\\+.*\\|^@@.*\\+%d" target-line) nil t)
              (beginning-of-line)))
          ;; Display inline comments (patch buffers don't use magit-diff-sections-hook)
          (when (fboundp 'shipit--display-inline-comments)
            (run-with-timer 0.1 nil
                            (lambda ()
                              (when (buffer-live-p (get-buffer buffer-name))
                                (with-current-buffer buffer-name
                                  (shipit--display-inline-comments t))))))
          (message "Opened patch for %s" file-path))
      (message "No patch available for %s in PR #%s (check debug log for details: M-x shipit-view-debug-log)" file-path pr-number))))

(defun shipit--compute-pr-merge-base-range (_base-ref _head-ref base-sha head-sha repo)
  "Compute GitHub-style three-dot diff range using Compare API.
Uses the backend :fetch-compare to get merge_base_commit.sha.
Returns START..END where START is merge-base and END is head-sha."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (compare-result (funcall (plist-get backend :fetch-compare)
                                  config base-sha head-sha)))
    (if compare-result
        (let ((merge-base-commit (cdr (assq 'merge_base_commit compare-result))))
          (if (and merge-base-commit
                   (cdr (assq 'sha merge-base-commit)))
              (let ((merge-base-sha (cdr (assq 'sha merge-base-commit))))
                (format "%s..%s" merge-base-sha head-sha))
            (format "%s..%s" base-sha head-sha)))
      (format "%s..%s" base-sha head-sha))))


(defun shipit--magit-diff-setup-buffer-with-fetch (range &optional args switches files type branch-to-fetch)
  "Call magit-diff-setup-buffer with automatic fetch retry on revision range errors.
RANGE is the revision range to diff.
ARGS are additional arguments.
SWITCHES are diff switches.
FILES are the files to limit the diff to.
TYPE is the diff type.
BRANCH-TO-FETCH is the PR branch to fetch if revision range error occurs."
  ;; First check if we need to fetch at all by testing if the range is valid
  (if (and branch-to-fetch (shipit--range-needs-fetch-p range))
      (progn
        (shipit--fetch-branch-async branch-to-fetch
                                    (lambda (success)
                                      (if success
                                          (progn
                                            (message "Fetch complete. Opening diff...")
                                            (magit-diff-setup-buffer range args switches files type))
                                        (progn
                                          (message "Fetch failed, trying diff anyway...")
                                          (condition-case err
                                              (magit-diff-setup-buffer range args switches files type)
                                            (error (message "Diff setup failed: %s" (error-message-string err)))))))))
    ;; Range appears valid, try setup directly with fallback
    (condition-case err
        (magit-diff-setup-buffer range args switches files type)
      (error
       ;; Check if this is a revision range error
       (let ((error-message (error-message-string err)))
         (if (and branch-to-fetch (string-match-p "fatal.*revision.*range\\|bad revision" error-message))
             (progn
               (message "Fetching missing commits for branch %s..." branch-to-fetch)
               (shipit--fetch-branch-async branch-to-fetch
                                           (lambda (success)
                                             (if success
                                                 (progn
                                                   (message "Fetch complete. Retrying diff setup...")
                                                   (magit-diff-setup-buffer range args switches files type))
                                               (progn
                                                 (message "Fetch failed. Manual git fetch may be required.")
                                                 (signal (car result) (cdr result)))))))
           ;; Not a revision range error, re-signal
           (signal (car result) (cdr result))))))))

(defun shipit--magit-diff-range-with-fetch (range &optional args files branch-to-fetch base-branch-to-fetch pr-number repo)
  "Call magit-diff-range with automatic fetch retry on revision range errors.
RANGE is the revision range to diff.
ARGS are additional arguments to magit-diff-range.
FILES are the files to limit the diff to.
BRANCH-TO-FETCH is the PR head branch to fetch if revision range error occurs.
BASE-BRANCH-TO-FETCH is the PR base branch to fetch if revision range error occurs.
PR-NUMBER if provided, uses GitHub's refs/pull/N/head ref for fetching (works for forks).
REPO is the GitHub repo (owner/name) to fetch from for cross-repo PRs."
  ;; First check if we need to fetch at all by testing if the range is valid
  (let ((needs-fetch (shipit--range-needs-fetch-p range)))
    (shipit--debug-log "shipit--magit-diff-range-with-fetch: range=%s needs-fetch=%s default-directory=%s"
                       range needs-fetch default-directory)
    (if (and (or branch-to-fetch pr-number) needs-fetch)
      ;; Capture default-directory for async callback
      (let ((repo-dir default-directory))
        ;; Use PR ref fetching if PR number provided (works for fork PRs)
        ;; Otherwise fall back to branch fetching
        (if pr-number
            (shipit--fetch-pr-ref-async
             pr-number
             (lambda (success)
               (let ((default-directory repo-dir))
                 (if success
                     (progn
                       (message "Fetch complete. Opening diff...")
                       (magit-diff-range range args files))
                   (progn
                     (message "PR ref fetch failed, trying branch fetch...")
                     ;; Fall back to branch fetch
                     (shipit--fetch-branches-async
                      (list base-branch-to-fetch branch-to-fetch)
                      (lambda (branch-success)
                        (let ((default-directory repo-dir))
                          (if branch-success
                              (progn
                                (message "Fetch complete. Opening diff...")
                                (magit-diff-range range args files))
                            (progn
                              (message "Fetch failed, trying diff anyway...")
                              (condition-case err
                                  (magit-diff-range range args files)
                                (error (message "Diff failed: %s" (error-message-string err)))))))))))))
             repo branch-to-fetch)
          ;; No PR number - use branch fetching
          (shipit--fetch-branches-async
           (list base-branch-to-fetch branch-to-fetch)
           (lambda (success)
             (let ((default-directory repo-dir))
               (if success
                   (progn
                     (message "Fetch complete. Opening diff...")
                     (magit-diff-range range args files))
                 (progn
                   (message "Fetch failed, trying diff anyway...")
                   (condition-case err
                       (magit-diff-range range args files)
                     (error (message "Diff failed: %s" (error-message-string err)))))))))))
    ;; Range appears valid, try diff directly with fallback
    (let ((result (condition-case err
                      (progn
                        ;; Use magit-diff-setup-buffer directly - it's faster than magit-diff-range
                        (magit-diff-setup-buffer range args nil files nil)
                        ;; Give magit a moment to populate the buffer
                        (sit-for 0.05)
                        ;; Check if the diff buffer contains a fatal error
                        (let ((diff-buf (cl-find-if (lambda (b)
                                                      (with-current-buffer b
                                                        (derived-mode-p 'magit-diff-mode)))
                                                    (buffer-list))))
                          (if (and diff-buf
                                   (with-current-buffer diff-buf
                                     (save-excursion
                                       (goto-char (point-min))
                                       (re-search-forward "fatal.*revision.*range\\|bad revision" nil t))))
                              (error "fatal: Invalid revision range %s" range)
                            'success)))
                    (error err))))
      (when (and (not (eq result 'success)) (consp result))
        ;; Check if this is a revision range error
        (let ((error-message (error-message-string result)))
          (if (and (or branch-to-fetch pr-number)
                   (string-match-p "fatal.*revision.*range\\|bad revision" error-message))
              (let ((repo-dir default-directory))
                (if pr-number
                    (progn
                      (message "Fetching PR #%s head ref..." pr-number)
                      (shipit--fetch-pr-ref-async
                       pr-number
                       (lambda (success)
                         (let ((default-directory repo-dir))
                           (if success
                               (progn
                                 (message "Fetch complete. Retrying diff...")
                                 (magit-diff-range range args files))
                             (progn
                               (message "Fetch failed. Manual git fetch may be required.")
                               (signal (car result) (cdr result))))))
                       repo branch-to-fetch))
                  (progn
                    (message "Fetching missing commits for branch %s..." branch-to-fetch)
                    (shipit--fetch-branch-async
                     branch-to-fetch
                     (lambda (success)
                       (let ((default-directory repo-dir))
                         (if success
                             (progn
                               (message "Fetch complete. Retrying diff...")
                               (magit-diff-range range args files))
                           (progn
                             (message "Fetch failed. Manual git fetch may be required.")
                             (signal (car result) (cdr result))))))))))
            ;; Not a revision range error, re-signal
            (signal (car result) (cdr result)))))))))


(defun shipit--refresh-ediff-comments ()
  "Refresh inline comments in the current ediff buffer."
  (interactive)
  (when (and (boundp 'shipit--ediff-pr-number) shipit--ediff-pr-number
             (boundp 'shipit--ediff-repo) shipit--ediff-repo
             (boundp 'shipit--ediff-file-path) shipit--ediff-file-path)
    (let ((inhibit-read-only t))
      ;; Remove all existing comment lines (marked with shipit-ediff-comment property)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (if (get-text-property (point) 'shipit-ediff-comment)
              ;; Found a comment - delete from here to where property ends
              (let ((comment-end (or (next-single-property-change (point) 'shipit-ediff-comment)
                                     (point-max))))
                (delete-region (point) comment-end))
            ;; No comment here, move to next property change
            (goto-char (or (next-single-property-change (point) 'shipit-ediff-comment)
                           (point-max))))))

      ;; Re-insert fresh comments
      (shipit--insert-inline-comments-in-ediff
       (current-buffer)
       shipit--ediff-repo
       shipit--ediff-pr-number
       shipit--ediff-file-path
       'head))
    (message "Refreshed ediff comments")))

(defun shipit--insert-inline-comments-in-ediff (buffer repo pr-number file-path _side)
  "Insert inline comments into ediff BUFFER for FILE-PATH on SIDE (base or head).
SIDE should be 'base' or 'head' to determine which comments to show."
  (with-current-buffer buffer
    ;; Fetch inline comments using the existing mechanism (populates cache)
    (shipit--fetch-inline-comments repo pr-number)
    ;; Read from the global cache that was populated
    (let* ((all-comments shipit--cached-inline-comments)
           (file-comments (cl-remove-if-not
                           (lambda (comment)
                             (and (string= (cdr (assq 'path comment)) file-path)
                                  (not (cdr (assq 'outdated comment)))))
                           all-comments)))
      (when file-comments
        ;; Sort comments by line number (ascending order)
        (setq file-comments (sort file-comments
                                  (lambda (a b)
                                    (< (or (cdr (assq 'line a))
                                           (cdr (assq 'original_line a)) 0)
                                       (or (cdr (assq 'line b))
                                           (cdr (assq 'original_line b)) 0)))))

        ;; Group comments by line number to handle threads
        (let ((comments-by-line (make-hash-table :test 'equal))
              (line-numbers '()))
          (dolist (comment file-comments)
            (let ((line-num (or (cdr (assq 'line comment))
                                (cdr (assq 'original_line comment)))))
              (when line-num
                (unless (gethash line-num comments-by-line)
                  (push line-num line-numbers))
                (push comment (gethash line-num comments-by-line)))))

          ;; Sort line numbers and process from bottom up to preserve line positions
          (setq line-numbers (sort line-numbers '>))

          ;; Insert comments for each line
          (dolist (line-num line-numbers)
            (let* ((line-comments (reverse (gethash line-num comments-by-line)))
                   ;; Group comments by threading structure
                   (threads (shipit--group-comments-by-api-replies line-comments))
                   ;; Get root comments (those without in_reply_to_id)
                   (root-comments (gethash 'root threads)))
              ;; Go to the line
              (goto-char (point-min))
              (forward-line (1- line-num))
              (end-of-line)

              ;; Insert each root comment with its threaded replies
              ;; Use simple text rendering for ediff (magit-section-mode can't be a minor mode)
              (dolist (comment root-comments)
                (shipit--insert-file-comment-with-replies comment threads repo pr-number)))))))))

(defun shipit--open-file-ediff (base-sha head-sha file-path &optional base-label head-label)
  "Open FILE-PATH in ediff, comparing BASE-SHA and HEAD-SHA versions.
BASE-LABEL and HEAD-LABEL are optional labels for the buffer names."
  (if (not (and base-sha head-sha file-path))
      (user-error "Cannot open ediff: missing base-sha, head-sha, or file-path")
    (let* ((base-label (or base-label (substring base-sha 0 7)))
           (head-label (or head-label (substring head-sha 0 7)))
           (base-buffer-name (format "*%s: %s*" base-label (file-name-nondirectory file-path)))
           (head-buffer-name (format "*%s: %s*" head-label (file-name-nondirectory file-path)))
           (base-buffer (get-buffer-create base-buffer-name))
           (head-buffer (get-buffer-create head-buffer-name)))

      ;; Fill base buffer
      (with-current-buffer base-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((exit-code (call-process "git" nil t nil "show"
                                         (format "%s:%s" base-sha file-path))))
            (when (not (zerop exit-code))
              (erase-buffer)
              (insert (format ";; File did not exist in %s\n" base-sha)))))
        (when (and file-path (not (string-empty-p file-path)))
          (let ((buffer-file-name file-path))
            (set-auto-mode)))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t))

      ;; Fill head buffer
      (with-current-buffer head-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((exit-code (call-process "git" nil t nil "show"
                                         (format "%s:%s" head-sha file-path))))
            (when (not (zerop exit-code))
              (erase-buffer)
              (insert (format ";; File did not exist in %s\n" head-sha)))))
        (when (and file-path (not (string-empty-p file-path)))
          (let ((buffer-file-name file-path))
            (set-auto-mode)))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t))

      ;; Save window configuration and launch ediff
      (let ((window-config (current-window-configuration)))
        (ediff-buffers base-buffer head-buffer)

        ;; Mark buffers as temporary for ediff cleanup
        (with-current-buffer base-buffer
          (setq-local ediff-temp-indirect-buffer t))
        (with-current-buffer head-buffer
          (setq-local ediff-temp-indirect-buffer t))

        ;; Restore window configuration after ediff quits
        (let ((restore-fn nil))
          (setq restore-fn
                (lambda ()
                  (run-with-idle-timer 0.1 nil
                                       (lambda ()
                                         (set-window-configuration window-config)))
                  (remove-hook 'ediff-quit-hook restore-fn)))
          (add-hook 'ediff-quit-hook restore-fn 'append)))

      (message "Ediff: %s (%s vs %s)"
               (file-name-nondirectory file-path)
               base-label head-label))))

(defun shipit--with-local-or-api (repo pr-number local-fn api-fn)
  "Dispatch to LOCAL-FN or API-FN based on repo availability.
Gets PR data, extracts SHAs, and checks local availability.
LOCAL-FN receives (BASE-SHA HEAD-SHA HEAD-REF BASE-REF PR-DATA).
API-FN receives (REPO PR-NUMBER).
Dispatches to LOCAL-FN when in a matching local repo with valid SHAs.
Dispatches to API-FN otherwise."
  (let* ((pr-data (shipit-get-pull-request pr-number repo))
         (base-sha (cdr (assq 'sha (cdr (assq 'base pr-data)))))
         (head-sha (cdr (assq 'sha (cdr (assq 'head pr-data)))))
         (head-ref (cdr (assq 'ref (cdr (assq 'head pr-data)))))
         (base-ref (cdr (assq 'ref (cdr (assq 'base pr-data))))))
    (if (and base-sha head-sha
             (shipit--repo-available-locally-p repo))
        (funcall local-fn base-sha head-sha head-ref base-ref pr-data)
      (funcall api-fn repo pr-number))))

(defun shipit--sha-exists-locally-p (sha)
  "Check if SHA exists in the local git repository."
  (zerop (call-process "git" nil nil nil "cat-file" "-e" sha)))

(defun shipit--ediff-local-fn (repo pr-number file-path base-sha head-sha head-ref _base-ref _pr-data)
  "Open ediff for FILE-PATH using local git objects.
Fetches missing commits synchronously via backend dispatch if needed."
  (let ((shas-local (and (shipit--sha-exists-locally-p base-sha)
                         (shipit--sha-exists-locally-p head-sha))))
    (unless shas-local
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (refspec-fn (plist-get backend :refspec-for-pr))
             (remote-fn (plist-get backend :remote-for-fetch))
             (refspec (if refspec-fn
                          (funcall refspec-fn config pr-number head-ref)
                        (error "Backend %s missing :refspec-for-pr" (shipit-pr--backend-id))))
             (remote (if remote-fn
                         (funcall remote-fn config repo)
                       "origin"))
             (cmd (format "git fetch %s %s 2>&1" (shell-quote-argument remote) refspec)))
        (message "Fetching PR #%s from %s (please wait)..." pr-number (or repo "origin"))
        (let ((output (shell-command-to-string cmd)))
          (unless (and (shipit--sha-exists-locally-p base-sha)
                       (shipit--sha-exists-locally-p head-sha))
            (message "Fetch output: %s" output)
            (user-error "Failed to fetch PR #%s commits" pr-number)))
        (message "Fetch complete")))
    (shipit--open-file-ediff base-sha head-sha file-path
                             (format "PR#%s Base" pr-number)
                             (format "PR#%s Head" pr-number))))

(defun shipit--open-pr-file-ediff (repo pr-number file-path)
  "Open FILE-PATH in ediff, comparing base and head versions from PR-NUMBER in REPO.
Fetches missing commits automatically if not available locally.
Falls back to API-based ediff when the repo is not checked out locally."
  (if (not file-path)
      (user-error "Cannot open ediff: file-path is nil")
    (shipit--with-local-or-api
     repo pr-number
     (lambda (base-sha head-sha head-ref base-ref pr-data)
       (shipit--ediff-local-fn repo pr-number file-path
                               base-sha head-sha head-ref base-ref pr-data))
     (lambda (api-repo api-pr-number)
       (let* ((pr-data (shipit-get-pull-request api-pr-number api-repo))
              (base-sha (cdr (assq 'sha (cdr (assq 'base pr-data)))))
              (head-sha (cdr (assq 'sha (cdr (assq 'head pr-data))))))
         (if (not (and base-sha head-sha))
             (user-error "Cannot open ediff: missing commit information")
           (shipit--open-pr-file-ediff-from-api api-repo file-path base-sha head-sha api-pr-number)))))))

(defun shipit--open-pr-file-ediff-from-api (repo file-path base-sha head-sha pr-number)
  "Open ediff for FILE-PATH using API to fetch BASE-SHA and HEAD-SHA versions.
REPO and PR-NUMBER provide context.  Used when the repo is not checked out locally."
  (shipit--debug-log "Using API-based ediff for %s (base=%s head=%s)" file-path
                     (substring base-sha 0 7) (substring head-sha 0 7))
  (message "Fetching file versions from API...")
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-file-content))
         (base-content (when fetch-fn (funcall fetch-fn config file-path base-sha)))
         (head-content (when fetch-fn (funcall fetch-fn config file-path head-sha))))
    (if (not (and base-content head-content))
        (user-error "Failed to fetch file versions from API")
      (let* ((short-base (substring base-sha 0 (min 7 (length base-sha))))
             (short-head (substring head-sha 0 (min 7 (length head-sha))))
             (base-label (format "PR#%s Base" pr-number))
             (head-label (format "PR#%s Head" pr-number))
             (base-buf (get-buffer-create (format "*%s: %s*" base-label (file-name-nondirectory file-path))))
             (head-buf (get-buffer-create (format "*%s: %s*" head-label (file-name-nondirectory file-path)))))
        ;; Fill base buffer
        (with-current-buffer base-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert base-content))
          (let ((buffer-file-name file-path)) (set-auto-mode))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t))
        ;; Fill head buffer
        (with-current-buffer head-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert head-content))
          (let ((buffer-file-name file-path)) (set-auto-mode))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t))
        ;; Save window configuration and launch ediff
        (let ((window-config (current-window-configuration)))
          (ediff-buffers base-buf head-buf)
          ;; Mark buffers as temporary for ediff cleanup
          (with-current-buffer base-buf
            (setq-local ediff-temp-indirect-buffer t))
          (with-current-buffer head-buf
            (setq-local ediff-temp-indirect-buffer t))
          ;; Restore window configuration after ediff quits
          (let ((restore-fn nil))
            (setq restore-fn
                  (lambda ()
                    (run-with-idle-timer 0.1 nil
                                         (lambda ()
                                           (set-window-configuration window-config)))
                    (remove-hook 'ediff-quit-hook restore-fn)))
            (add-hook 'ediff-quit-hook restore-fn 'append)))
        (message "Ediff: %s (%s vs %s)"
                 (file-name-nondirectory file-path)
                 base-label head-label)))))

;;; -------------------------------------------------------------------------
;;; Block 2: More diff range + Setup + Navigation
;;; -------------------------------------------------------------------------

(defun shipit--open-pr-file-diff (repo pr-number file-path)
  "Open diff for FILE-PATH in the context of PR-NUMBER with proper Magit diff buffer.
Uses git-based approach for full magit formatting and inline comments.
Falls back to API patch when the repo is not checked out locally."
  (let ((repo-dir default-directory))
    (shipit--with-local-or-api
     repo pr-number
     (lambda (base-sha head-sha head-ref base-ref _pr-data)
       (when (fboundp 'magit-diff-range)
         (let ((range (shipit--compute-pr-merge-base-range base-ref head-ref base-sha head-sha repo)))
           (message "Opening PR diff for %s..." file-path)
           (setq shipit--refresh-pr-context (list pr-number repo))
           (let ((default-directory repo-dir))
             (shipit--magit-diff-range-with-fetch range '() (list file-path) head-ref base-ref pr-number repo))
           (shipit--set-diff-buffer-pr-context pr-number repo))))
     (lambda (_api-repo _api-pr-number)
       (shipit--debug-log "Using API patch for D-diff: %s (no local repo)" file-path)
       (shipit--show-pr-file-patch repo pr-number file-path)))))

(defun shipit--set-diff-buffer-pr-context (pr-number repo)
  "Set PR context in the most recent magit-diff-mode buffer for inline comments."
  (run-with-timer 0.05 nil
                  (lambda ()
                    (let ((diff-buffer (cl-find-if (lambda (buf)
                                                     (with-current-buffer buf
                                                       (derived-mode-p 'magit-diff-mode)))
                                                   (buffer-list))))
                      (when diff-buffer
                        (with-current-buffer diff-buffer
                          (setq shipit--current-displayed-pr (list pr-number repo))
                          (setq-local shipit--diff-pr-context (cons pr-number repo))))))))

(defun shipit--open-file-diff (file-path &optional target-line diff-hunk)
  "Open magit-diff buffer for FILE-PATH and optionally position cursor.
TARGET-LINE is the line number to position cursor on.
DIFF-HUNK is the diff hunk context to help locate the correct position."
  (interactive)
  (cond
   ;; First priority: Use GitHub API if PR context is available
   ((and shipit--current-displayed-pr
         (shipit--get-repo-from-remote))
    (let* ((pr-number (car shipit--current-displayed-pr))
           (repo (cadr shipit--current-displayed-pr)))
      (shipit--show-pr-file-patch repo pr-number file-path target-line)))

   ;; Fallback: Try magit-diff-range with proper arguments
   ((and (fboundp 'magit-get-current-branch)
         (fboundp 'magit-diff-range))
    (let* ((current-branch (magit-get-current-branch))
           (repo (shipit--get-repo-from-remote))
           ;; Use displayed PR instead of current branch PR
           (pr-data (cond
                     ;; First try: use the currently displayed PR in the status buffer
                     ((and shipit--current-displayed-pr
                           (equal (cadr shipit--current-displayed-pr) repo))
                      (shipit-get-pull-request (car shipit--current-displayed-pr)))
                     ;; No fallback - error instead of using wrong PR
                     ((and current-branch repo)
                      (user-error "No PR currently displayed. Please view a PR first using 's' or by browsing to a PR"))
                     (t nil)))
           (base-ref (when pr-data
                       (cdr (assq 'ref (cdr (assq 'base pr-data))))))
           (base-sha (when pr-data
                       (cdr (assq 'sha (cdr (assq 'base pr-data))))))
           (head-ref (when pr-data
                       (cdr (assq 'ref (cdr (assq 'head pr-data))))))
           (head-sha (when pr-data
                       (cdr (assq 'sha (cdr (assq 'head pr-data))))))
           (head-branch (or head-ref current-branch)) ; Use PR's head branch, fallback to current branch
           ;; Ensure head branch exists locally, fetch if needed
           (_ (when (and head-ref (not (equal head-ref current-branch)))
                (unless (shipit--ref-exists-locally head-ref)
                  (message "Fetching PR branch %s from remote..." head-ref)
                  (condition-case _err
                      (magit-git "fetch" "origin" (format "%s:%s" head-ref head-ref))
                    (error
                     (message "Warning: Could not fetch PR branch %s" head-ref))))))
           (range (cond
                   ;; First try: use base SHA if it exists locally (with merge-base)
                   ((and base-sha (shipit--sha-exists-locally base-sha))
                    (shipit--compute-pr-merge-base-range base-ref head-ref base-sha head-sha repo))
                   ;; Second try: use base ref if it exists locally (with merge-base)
                   ((and base-ref (shipit--ref-exists-locally base-ref))
                    (shipit--compute-pr-merge-base-range base-ref head-ref base-sha head-sha repo))
                   ;; Fallback: use upstream or HEAD~1
                   (t
                    (if current-branch
                        (or (magit-get-upstream-branch current-branch)
                            "HEAD~1..HEAD")
                      "HEAD~1..HEAD")))))
      (message "Opening diff for %s (range: %s)" file-path range)
      ;; Set global PR context BEFORE calling magit-diff-range
      ;; This ensures it's available when magit-diff-sections-hook runs
      (when pr-data
        (setq shipit--refresh-pr-context
              (list (cdr (assq 'number pr-data)) repo)))

      ;; Set global target file path BEFORE calling magit-diff-setup-buffer
      ;; This ensures file-level comments are filtered correctly when magit-diff-sections-hook runs
      (setq shipit--refresh-target-file-path file-path)

      ;; Use magit-diff-setup-buffer with auto-fetch on error
      (shipit--magit-diff-setup-buffer-with-fetch range nil '() (list file-path) 'committed head-ref)
      ;; Also set displayed PR info in the new diff buffer as backup
      (when pr-data
        (run-with-timer 0.05 nil
                        (lambda ()
                          ;; Find the diff buffer and set PR context
                          (let ((diff-buffer (cl-find-if (lambda (buf)
                                                           (with-current-buffer buf
                                                             (derived-mode-p 'magit-diff-mode)))
                                                         (buffer-list))))
                            (when diff-buffer
                              (with-current-buffer diff-buffer
                                (setq shipit--current-displayed-pr
                                      (list (cdr (assq 'number pr-data)) repo))
                                (setq-local shipit--diff-pr-context
                                            (cons (cdr (assq 'number pr-data)) repo))
                                (setq-local shipit--target-file-path file-path)))))))
      ;; Position cursor on target line if provided and display comments
      (run-with-timer 0.1 nil
                      (lambda ()
                        ;; Set displayed PR info in diff buffer for inline comments
                        (when pr-data
                          (setq shipit--current-displayed-pr
                                (list (cdr (assq 'number pr-data)) repo))
                          (setq-local shipit--diff-pr-context
                                      (cons (cdr (assq 'number pr-data)) repo)))
                        (when (and target-line diff-hunk)
                          (shipit--position-cursor-on-diff-line target-line diff-hunk))
                        ;; Inline comments are inserted via magit-diff-sections-hook
                        ))))
   ;; Try alternative magit functions
   ((and (fboundp 'magit-get-current-branch)
         (fboundp 'magit-diff))
    (let* ((current-branch (magit-get-current-branch))
           (repo (shipit--get-repo-from-remote))
           ;; Use displayed PR instead of current branch PR
           (pr-data (cond
                     ;; First try: use the currently displayed PR in the status buffer
                     ((and shipit--current-displayed-pr
                           (equal (cadr shipit--current-displayed-pr) repo))
                      (shipit-get-pull-request (car shipit--current-displayed-pr)))
                     ;; No fallback - error instead of using wrong PR
                     ((and current-branch repo)
                      (user-error "No PR currently displayed. Please view a PR first using 's' or by browsing to a PR"))
                     (t nil)))
           (base-ref (when pr-data
                       (cdr (assq 'ref (cdr (assq 'base pr-data))))))
           (head-ref (when pr-data
                       (cdr (assq 'ref (cdr (assq 'head pr-data))))))
           (head-branch (or head-ref current-branch))) ; Use PR's head branch, fallback to current branch
      ;; Ensure head branch exists locally, fetch if needed
      (when (and head-ref (not (equal head-ref current-branch)))
        (unless (shipit--ref-exists-locally head-ref)
          (message "Fetching PR branch %s from remote..." head-ref)
          (condition-case _err
              (magit-git "fetch" "origin" (format "%s:%s" head-ref head-ref))
            (error
             (message "Warning: Could not fetch PR branch %s" head-ref)))))
      ;; Set global target file path BEFORE calling magit-diff-setup-buffer
      ;; This ensures file-level comments are filtered correctly when magit-diff-sections-hook runs
      (setq shipit--refresh-target-file-path file-path)

      (if base-ref
          (let ((range (format "%s..%s" base-ref head-branch)))
            (magit-diff-setup-buffer range nil '() (list file-path) 'committed))
        (magit-diff-setup-buffer "HEAD~1..HEAD" nil '() (list file-path) 'committed))
      ;; Position cursor on target line if provided and display comments
      (run-with-timer 0.1 nil
                      (lambda ()
                        ;; Set displayed PR info in diff buffer for inline comments
                        (when pr-data
                          (setq shipit--current-displayed-pr
                                (list (cdr (assq 'number pr-data)) repo))
                          (setq-local shipit--diff-pr-context
                                      (cons (cdr (assq 'number pr-data)) repo))
                          (setq-local shipit--target-file-path file-path))
                        (when (and target-line diff-hunk)
                          (shipit--position-cursor-on-diff-line target-line diff-hunk))
                        ;; Inline comments are inserted via magit-diff-sections-hook
                        ))))
   ;; Fallback to opening the file
   ((fboundp 'find-file)
    (find-file file-path)
    (message "Opened %s (magit-diff not available)" file-path))
   ;; Final fallback
   (t
    (message "Cannot open %s: required functions not available" file-path))))

(defun shipit-setup-magit-integration ()
  "Set up integration with magit buffers."
  (interactive)
  (when (fboundp 'magit-add-section-hook)
    ;; Ensure keybindings are set up in magit mode maps
    (when (boundp 'magit-diff-mode-map)
      (define-key magit-diff-mode-map (kbd "C-c C-s") #'shipit)
      (define-key magit-diff-mode-map (kbd "C-c C-d") #'shipit-debug))
    (when (boundp 'magit-revision-mode-map)
      (define-key magit-revision-mode-map (kbd "C-c C-s") #'shipit)
      (define-key magit-revision-mode-map (kbd "C-c C-d") #'shipit-debug))))

(defun shipit--navigate-to-github-url (owner repo sha file-path start-line end-line)
  "Navigate to OWNER/REPO FILE-PATH at SHA and jump to START-LINE using magit."
  (let* ((local-file (expand-file-name file-path default-directory))
         (target-line (or start-line 1)))
    (condition-case err
        (if (and sha (fboundp 'magit-find-file) (file-directory-p ".git"))
            ;; Use magit to show the file at the specific commit
            (progn
              (magit-find-file sha file-path)
              (goto-char (point-min))
              (forward-line (1- target-line))
              (message "Opened %s@%s:%d via magit" file-path (substring sha 0 7) target-line))
          ;; Fallback to regular file opening
          (if (file-exists-p local-file)
              (progn
                (find-file local-file)
                (goto-char (point-min))
                (forward-line (1- target-line))
                (message "Navigated to %s:%d (current version)" file-path target-line))
            (message "File not found locally: %s" local-file)))
      (error
       (message "Could not navigate to %s: %s" file-path err)))))

(defun shipit--add-line-numbers (lines start-line owner repo sha file-path)
  "Add line numbers to LINES starting from START-LINE with magit-diff-context face and clickable navigation."
  (let* ((end-line (+ start-line (length lines) -1))
         ;; Calculate width needed for the largest line number in the snippet
         (max-line-length (max (length (number-to-string start-line))
                               (length (number-to-string end-line))))
         (current-line start-line))
    (mapconcat
     (lambda (line)
       (let* ((line-num-str (number-to-string current-line))
              (padding-needed (- max-line-length (length line-num-str)))
              (padding-spaces (make-string padding-needed ? ))
              ;; Right-align line number, then exactly 3 spaces, then code
              (formatted (concat padding-spaces line-num-str "   " line))
              ;; Create clickable line that navigates to this specific line
              (clickable-line (propertize formatted

                                          'help-echo (format "Click to open %s at line %d" file-path current-line)
                                          'keymap (let ((map (make-sparse-keymap))
                                                        (target-line current-line))
                                                    (set-keymap-parent map magit-section-mode-map)
                                                    (define-key map [mouse-1]
                                                                `(lambda (event)
                                                                   (interactive "e")
                                                                   (shipit--navigate-to-github-url
                                                                    ,owner ,repo ,sha ,file-path ,target-line nil)))
                                                    (define-key map [return]
                                                                `(lambda ()
                                                                   (interactive)
                                                                   (shipit--navigate-to-github-url
                                                                    ,owner ,repo ,sha ,file-path ,target-line nil)))
                                                    map))))
         (setq current-line (1+ current-line))
         ;; Apply magit face with higher priority and mark as protected from comment face
         (add-face-text-property 0 (length clickable-line) 'magit-diff-context t clickable-line)
         (add-text-properties 0 (length clickable-line) '(shipit-protected-face t) clickable-line)
         clickable-line))
     lines
     "\n")))

;;; -------------------------------------------------------------------------
;;; Block 3: Format + Advice + Highlighting
;;; -------------------------------------------------------------------------

(defun shipit--truncate-comment-context (text max-width)
  "Truncate TEXT for context display, preferring newline breaks.
Truncates at first newline if found within MAX-WIDTH, otherwise truncates at MAX-WIDTH.
Adds ellipsis (...) when truncation occurs."
  (when text
    (let* ((clean-text (shipit--clean-text text))
           (first-newline (string-match-p "\n" clean-text)))
      (cond
       ;; If there's a newline within our limit, truncate there and add ellipsis if more content follows
       ((and first-newline (< first-newline max-width))
        (let ((truncated (substring clean-text 0 first-newline)))
          (if (> (length clean-text) (1+ first-newline))  ; More than just the newline character
              (concat truncated "...")
            truncated)))
       ;; If text is shorter than max-width, return as-is
       ((<= (length clean-text) max-width)
        clean-text)
       ;; Otherwise truncate at max width with ellipsis
       (t
        (concat (truncate-string-to-width clean-text (- max-width 3)) "..."))))))

;; Keybinding setup for magit buffers
(with-eval-after-load 'magit-diff
  (with-eval-after-load 'magit-revision
    (define-key magit-diff-mode-map (kbd "C-c C-s") #'shipit)
    (define-key magit-revision-mode-map (kbd "C-c C-s") #'shipit)))

;; Advice magit-show-commit to set up flag-based comment insertion
(defun shipit--advice-magit-show-commit (orig-fun commit &rest args)
  "Advice for magit-show-commit to set up flag-based comment insertion.
Only activates if we're in a shipit PR context (shipit-buffer-pr-number is set)."
  ;; Only run shipit comment logic if we're in a shipit PR context
  (when (or (bound-and-true-p shipit-buffer-pr-number)
            (bound-and-true-p shipit-buffer-repo))
    ;; Set flag for this commit before opening
    (setq shipit--pending-commit-for-comments commit))

  ;; Call the original function
  (apply orig-fun commit args)

  ;; Only start comment retry system if we're in shipit context
  (when (or (bound-and-true-p shipit-buffer-pr-number)
            (bound-and-true-p shipit-buffer-repo))
    (shipit--start-comment-retry-system commit)))

;; Advice magit-visit-thing to handle our custom shipit sections
(defun shipit--advice-magit-visit-thing (orig-fun &rest args)
  "Advice for magit-visit-thing to handle shipit sections and comments."
  (if (and (fboundp 'magit-current-section) (fboundp 'magit-section-match))
      (let ((section (magit-current-section)))
        (cond
         ;; Handle text properties first (author line, comments, etc.)
         ((get-text-property (point) 'shipit-pr-header)
          (shipit-dwim))
         ;; For inline comments (has file-path), RET triggers DWIM
         ;; For general comments (no file-path), RET uses default behavior (M-; for actions)
         ((and (get-text-property (point) 'shipit-comment)
               (get-text-property (point) 'shipit-file-path))
          (shipit-dwim))
         ((get-text-property (point) 'shipit-pr-description)
          (shipit-dwim))
         ((get-text-property (point) 'shipit-labels)
          (shipit-dwim))
         ((get-text-property (point) 'shipit-reviews)
          (shipit-dwim))
         ((get-text-property (point) 'shipit-assignees)
          (shipit-dwim))
         ;; Handle file comment sections - navigate to diff
         ((and section
               (or (magit-section-match '(file-comments) section)
                   (magit-section-match '(file-comments-outdated) section)))
          (let ((file-path (oref section value)))
            (if file-path
                (progn
                  (message "Opening diff for %s..." (file-name-nondirectory file-path))
                  (shipit--file-at-point))
              (message "No file path found for section"))))
         ;; For inline-comments section, RET triggers DWIM
         ;; For general-comments section, RET falls through to default (M-; for actions)
         ((and section (magit-section-match '(inline-comments) section))
          (shipit-dwim))
         ((and section (magit-section-match '(labels) section))
          (shipit-dwim))
         ((and section (magit-section-match '(reviewers) section))
          (shipit-dwim))
         ((and section (magit-section-match '(shipit-description) section))
          (shipit-dwim))
         ((and section (magit-section-match '(notifications) section))
          (shipit-dwim))
         ((and section (magit-section-match '(shipit-pr) section))
          ;; For PR sections, use normal magit behavior (collapse/expand)
          (apply orig-fun args))
         ;; Handle all check sections
         ((and section (or (magit-section-match '(checks) section)
                           (magit-section-match '(in-progress-checks) section)
                           (magit-section-match '(pending-checks) section)
                           (magit-section-match '(cancelled-checks) section)
                           (magit-section-match '(failing-checks) section)
                           (magit-section-match '(skipped-checks) section)
                           (magit-section-match '(successful-checks) section)))
          (shipit-dwim))
         ;; Handle inline comments (with file-path) - trigger DWIM
         ;; General comments (no file-path) fall through to default behavior
         ((and (get-text-property (point) 'shipit-comment)
               (get-text-property (point) 'shipit-file-path))
          (shipit-dwim))
         ;; Not our section/comment type, call original function
         (t (apply orig-fun args))))
    ;; Fallback if magit functions aren't available
    (apply orig-fun args)))

;; Apply advice when magit is available
(when (locate-library "magit")
  (with-eval-after-load 'magit
    (when (fboundp 'advice-add)
      (advice-add 'magit-visit-thing :around #'shipit--advice-magit-visit-thing)
      (advice-add 'magit-show-commit :around #'shipit--advice-magit-show-commit))))

;; Automatically enable highlighting when global mode is activated
(defun shipit--setup-highlighting ()
  "Set up pre-action and post-action highlighting hooks."
  (add-hook 'shipit-dwim-hook #'shipit--highlight-region-with-pulse)
  (add-hook 'shipit-post-action-hook #'shipit--highlight-modified-region-with-pulse))

(defun shipit--teardown-highlighting ()
  "Remove pre-action and post-action highlighting hooks."
  (remove-hook 'shipit-dwim-hook #'shipit--highlight-region-with-pulse)
  (remove-hook 'shipit-post-action-hook #'shipit--highlight-modified-region-with-pulse))

;; Set up highlighting when shipit is loaded
(shipit--setup-highlighting)

(defun shipit--format-comment-as-text (comment &optional is-inline)
  "Format COMMENT as insertable text with proper styling and interactive elements.
If IS-INLINE is non-nil, treats comment as an inline comment for proper reaction handling."
  (let* ((raw-comment-body (cdr (assq 'body comment)))
         (comment-body (shipit--clean-text raw-comment-body))
         (comment-user (cdr (assq 'login (cdr (assq 'user comment)))))
         (rendered-body (if shipit-render-markdown
                            (shipit--render-markdown comment-body)
                          comment-body))
         (expanded-body (if shipit-expand-code-urls
                            (condition-case err
                                (shipit--expand-code-urls rendered-body)
                              (error
                               (shipit--debug-log "URL expansion failed: %s" err)
                               rendered-body))
                          rendered-body))
         ;; Clean again before wrapping to remove any \r characters that may have been reintroduced
         ;; by markdown rendering or URL expansion
         (cleaned-expanded-body (shipit--clean-text expanded-body))
         (wrapped-body (shipit--wrap-comment-text cleaned-expanded-body (or (and (boundp 'shipit-comment-wrap-width) shipit-comment-wrap-width) 80)))
         (timestamp (cdr (assq 'created_at comment)))
         (formatted-timestamp (shipit--format-timestamp timestamp))
         (indented-body (shipit--indent-comment-body wrapped-body))
         (reactions (shipit--format-comment-reactions comment is-inline))
         (comment-id (cdr (assq 'id comment)))
         (orange-color (if (eq (frame-parameter nil 'background-mode) 'light) "#d2691e" "#ff8c00"))
         (comment-face `(:foreground ,orange-color :extend t))
         (user-face `(:foreground ,orange-color :weight bold :extend t))
         (button-face `(:foreground ,orange-color :extend t)))

    ;; Build the comment text with proper faces and keymaps
    (concat
     (propertize (format "💬 %s %s\n\n" comment-user formatted-timestamp)
                 'font-lock-face user-face
                 'shipit-comment t
                 'shipit-comment-id comment-id
                 'shipit-comment-body raw-comment-body)
     ;; Apply comment face but preserve protected faces (like magit-diff-context)
     (let ((formatted-body (format "%s\n\n" indented-body)))
       (shipit--apply-comment-face-selectively formatted-body comment-face)
       (propertize formatted-body 'shipit-comment t))
     (if (not (string-empty-p reactions))
         (propertize (format "%s " reactions)
                     'font-lock-face comment-face
                     'shipit-comment t
                     'shipit-comment-id comment-id
                     'shipit-comment-body raw-comment-body)
       (propertize "   "
                   'font-lock-face comment-face
                   'shipit-comment t
                   'shipit-comment-id comment-id
                   'shipit-comment-body raw-comment-body))
     (propertize "[+]"
                 'font-lock-face button-face
                 'shipit-comment t
                 'shipit-comment-id comment-id
                 'shipit-comment-body raw-comment-body

                 'help-echo "Click to add a reaction"
                 'keymap (let ((map (make-sparse-keymap)))
                           (set-keymap-parent map magit-section-mode-map)
                           (define-key map [mouse-1]
                                       (lambda (event)
                                         (interactive "e")
                                         (shipit--toggle-reaction-interactive comment-id)))
                           (define-key map [return]
                                       (lambda ()
                                         (interactive)
                                         (shipit--toggle-reaction-interactive comment-id)))
                           map))
     (propertize " [edit]\n\n"
                 'font-lock-face button-face
                 'shipit-comment t
                 'shipit-comment-id comment-id
                 'shipit-comment-body raw-comment-body

                 'help-echo "Click to edit this comment"
                 'keymap (let ((map (make-sparse-keymap)))
                           (set-keymap-parent map magit-section-mode-map)
                           (define-key map [mouse-1]
                                       (lambda (event)
                                         (interactive "e")
                                         (shipit--edit-comment-interactive comment-id raw-comment-body)))
                           (define-key map [return]
                                       (lambda ()
                                         (interactive)
                                         (shipit--edit-comment-interactive comment-id raw-comment-body)))
                           map)))))

;;; -------------------------------------------------------------------------
;;; Block 4: Diff inline comments
;;; -------------------------------------------------------------------------

(defun shipit--commit-has-inline-comments-p ()
  "Check if current magit-revision commit has inline comments without expensive operations."
  (when (and (derived-mode-p 'magit-revision-mode)
             (boundp 'magit-buffer-revision)
             magit-buffer-revision)
    (let* ((repo (shipit--get-repo-from-remote))
           (pr-data (when (car-safe shipit--current-displayed-pr)
                      (let ((displayed-pr-number (car shipit--current-displayed-pr)))
                        (shipit-get-pull-request displayed-pr-number))))
           (pr-number (when pr-data (cdr (assq 'number pr-data)))))
      (when (and repo pr-number)
        ;; Use cached inline comments to avoid async issues
        (let ((inline-comments shipit--cached-inline-comments))
          ;; If no cached comments, fetch them synchronously (this is rare)
          (unless inline-comments
            (setq inline-comments (shipit--fetch-inline-comments repo pr-number nil))
            ;; If still a request object, return nil to avoid errors
            (when (and inline-comments (not (listp inline-comments)))
              (setq inline-comments nil)))
          (when (and inline-comments (listp inline-comments))
            ;; Check if any comment matches this commit
            (cl-some (lambda (comment)
                       (let ((commit-id (cdr (assq 'commit_id comment))))
                         (and commit-id
                              (string-prefix-p magit-buffer-revision commit-id))))
                     inline-comments)))))))

(defun shipit--start-comment-retry-system (commit-sha)
  "Start flag-based retry system for comment insertion."
  ;; Cancel any existing retry timer
  (when shipit--comment-retry-timer
    (cancel-timer shipit--comment-retry-timer)
    (setq shipit--comment-retry-timer nil))

  ;; Reset retry counter
  (setq shipit--comment-retry-count 0)

  ;; Start retry loop with a small delay to let buffer settle
  (setq shipit--comment-retry-timer
        (run-with-timer 0.2 0.2 'shipit--retry-comment-insertion commit-sha)))

(defun shipit--retry-comment-insertion (commit-sha)
  "Retry inserting comments for COMMIT-SHA until success or timeout."
  (setq shipit--comment-retry-count (1+ shipit--comment-retry-count))

  ;; Check if we've exceeded max retries
  (if (> shipit--comment-retry-count shipit--comment-retry-max)
      (progn
        (when shipit--comment-retry-timer
          (cancel-timer shipit--comment-retry-timer)
          (setq shipit--comment-retry-timer nil))
        (setq shipit--pending-commit-for-comments nil))

    ;; Find magit-revision buffer for this commit
    (let ((target-buffer (shipit--find-magit-revision-buffer commit-sha)))
      (if target-buffer
          (with-current-buffer target-buffer
            ;; Check if comments are already inserted
            (let ((has-comments (save-excursion
                                  (goto-char (point-min))
                                  (text-property-any (point-min) (point-max) 'shipit-comment t))))
              (if has-comments
                  ;; Success! Comments are already there
                  (progn
                    (when shipit--comment-retry-timer
                      (cancel-timer shipit--comment-retry-timer)
                      (setq shipit--comment-retry-timer nil))
                    (setq shipit--pending-commit-for-comments nil))

                ;; Try to insert comments
                (shipit--display-inline-comments t)

                ;; Check if insertion worked
                (let ((has-comments-now (save-excursion
                                          (goto-char (point-min))
                                          (text-property-any (point-min) (point-max) 'shipit-comment t))))
                  (if has-comments-now
                      ;; Success! Comments were inserted
                      (progn
                        (when shipit--comment-retry-timer
                          (cancel-timer shipit--comment-retry-timer)
                          (setq shipit--comment-retry-timer nil))
                        (setq shipit--pending-commit-for-comments nil))
                    ;; No comments inserted - check if this commit actually has comments
                    ;; If no comments exist for this commit, stop retrying
                    (unless (shipit--commit-has-inline-comments-p)
                      (when shipit--comment-retry-timer
                        (cancel-timer shipit--comment-retry-timer)
                        (setq shipit--comment-retry-timer nil))
                      (setq shipit--pending-commit-for-comments nil)))))))))))

(defun shipit--find-magit-revision-buffer (commit-sha)
  "Find the magit-revision buffer for COMMIT-SHA."
  (let ((target-buffer nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'magit-revision-mode)
                   (boundp 'magit-buffer-revision)
                   magit-buffer-revision
                   (string-prefix-p commit-sha magit-buffer-revision))
          (setq target-buffer buffer))))
    target-buffer))

(defun shipit--display-inline-comments (&optional force)
  "Display inline comments in diff buffers.
FORCE indicates whether to force display even if already shown."
  ;; Only display in diff/revision buffers
  (when (or (derived-mode-p 'magit-diff-mode 'magit-revision-mode)
            (and (buffer-name) (string-match-p "magit-diff\\|magit-revision" (buffer-name))))

    (let* ((repo (shipit--get-repo-from-remote))
           (branch (when (fboundp 'magit-get-current-branch) (magit-get-current-branch)))
           (current-pr (when (and repo branch) (shipit--get-current-pr-data branch repo))))

      (when (and repo current-pr)
        (let* ((pr-number (cdr (assq 'number current-pr)))
               (comments (shipit--fetch-inline-comments repo pr-number force)))

          (when comments
            ;; Group comments by file for easier processing
            (let ((comments-by-file (shipit--group-inline-comments-by-file comments)))

              ;; Insert comments into diff buffer at appropriate locations
              (save-excursion
                (goto-char (point-min))

                ;; Find diff sections and insert relevant comments
                (while (re-search-forward "^diff --git a/\\(.+\\) b/\\(.+\\)$" nil t)
                  (let* ((file-path (match-string 2))
                         (file-comments (cdr (assoc file-path comments-by-file))))

                    (when file-comments
                      (shipit--insert-file-diff-comments file-comments file-path))))))))))))

(defun shipit--insert-file-diff-comments (comments file-path)
  "Insert COMMENTS for FILE-PATH into the current diff buffer at appropriate line positions."
  (dolist (comment comments)
    (let* ((line (cdr (assq 'line comment)))
           (original-line (cdr (assq 'original_line comment)))
           (diff-hunk (cdr (assq 'diff_hunk comment)))
           (body (cdr (assq 'body comment)))
           (user (cdr (assq 'login (cdr (assq 'user comment)))))
           (is-outdated (cdr (assq 'outdated comment))))

      ;; Try to find the appropriate line in the diff to insert the comment
      (when (shipit--find-comment-line-in-diff line original-line diff-hunk)
        (let ((comment-start (point))
              (review-id (cdr (assq 'pull_request_review_id comment))))
          ;; Insert the comment after the line
          (end-of-line)
          (insert "\n")
          (insert (propertize (format "  💬 %s: %s" user body)
                              'font-lock-face (if is-outdated 'shadow 'font-lock-comment-face)
                              'shipit-inline-comment t
                              'shipit-comment-data comment
                              ;; CRITICAL: Add missing properties needed for DWIM handler to recognize as inline comment
                              'shipit-comment t
                              'shipit-comment-id (cdr (assq 'id comment))
                              'shipit-review-id review-id
                              'shipit-comment-body body
                              'shipit-file-path file-path
                              'shipit-line-number line
                              'shipit-repo (shipit--get-repo-from-remote)
                              'shipit-pr-number (when (and shipit--current-displayed-pr (listp shipit--current-displayed-pr))
                                                  (car shipit--current-displayed-pr))))
          (insert "\n"))))))

(defun shipit--find-comment-line-in-diff (line original-line diff-hunk)
  "Find the location in diff buffer for a comment at LINE or ORIGINAL-LINE.
DIFF-HUNK provides context. Return t if found and point is positioned correctly."
  (save-excursion
    (let ((found nil))
      ;; Look for the specific line number in context
      (when (and line (numberp line))
        ;; Search for +LINE pattern (addition)
        (when (re-search-forward (format "^\\+.*" "") (point-max) t)
          ;; This is simplified - in practice would need to parse hunk headers
          ;; to find exact line numbers and match them properly
          (beginning-of-line)
          (setq found t)))

      ;; Fallback: look for original line number
      (when (and (not found) original-line (numberp original-line))
        (when (re-search-forward (format "^-.*" "") (point-max) t)
          (beginning-of-line)
          (setq found t)))

      found)))

(defun shipit--setup-diff-hooks ()
  "Set up hooks to display inline comments in diff buffers."
  (when (fboundp 'add-hook)
    (add-hook 'magit-diff-mode-hook #'shipit--maybe-display-inline-comments)
    (add-hook 'magit-revision-mode-hook #'shipit--maybe-display-inline-comments)))

(defun shipit--maybe-display-inline-comments ()
  "Maybe display inline comments if we're in a shipit-enabled repository."
  (when (and (shipit--in-shipit-context-p)  ; Only run in shipit context
             (fboundp 'shipit--get-repo-from-remote)
             (shipit--get-repo-from-remote))
    (run-with-idle-timer 0.5 nil #'shipit--display-inline-comments)))

(defun shipit--insert-diff-hierarchical-comment (comment threads depth pr-number)
  "Insert a COMMENT with hierarchical threading at DEPTH level using THREADS hash table for diff buffer."
  (let* ((comment-id (cdr (assq 'id comment)))
         (user (cdr (assq 'login (cdr (assq 'user comment)))))
         (created (cdr (assq 'created_at comment)))
         (formatted-timestamp (shipit--format-timestamp created))
         ;; Check outdated status
         (is-outdated (cdr (assq 'outdated comment)))
         ;; Check resolved status
         (is-resolved (or (cdr (assq 'resolved comment))
                          (and comment-id (shipit--is-comment-in-resolved-thread comment-id))))
         ;; Use the stored reply-depth from threading analysis (includes quote-based detection)
         (actual-depth (or (cdr (assq 'reply-depth comment)) depth))
         ;; Create proper indentation: root comments have no tree indicator, replies at 3 spaces with └─
         (thread-prefix (if (> actual-depth 0)
                            (make-string (+ 3 (* (1- actual-depth) 6)) ?\s)
                          ""))
         (tree-indicator (if (> actual-depth 0) "└─ " ""))
         ;; Use same orange color scheme as old system
         (orange-color (if (eq (frame-parameter nil 'background-mode) 'light)
                           "#d2691e" "#ff8c00"))
         (user-face `(:foreground ,orange-color :weight bold :extend t)))

    ;; Define context-keymap for diff buffer
    (let ((context-keymap (let ((map (make-sparse-keymap)))
                            ;; Allow undefined keys to fall through to parent keybindings
                            (set-keymap-parent map (current-local-map))
                            (define-key map [return] 'shipit--file-at-point)
                            (define-key map (kbd "SPC") 'shipit--file-at-point)
                            (define-key map (kbd "M-;")
                                        (lambda ()
                                          (interactive)
                                          (shipit--simple-comment-dialog pr-number (shipit--get-repo-from-remote))))
                            (when shipit-enable-mouse-navigation
                              (define-key map [mouse-1]
                                          `(lambda (event)
                                             (interactive "e")
                                             (mouse-set-point event)
                                             (shipit--file-at-point))))
                            map)))

      ;; Wrap each comment in a magit-section for hierarchical TAB navigation
      (condition-case err
          (progn
            (magit-insert-section (shipit-comment comment-id)
              ;; Use the same keymap as code context for unified behavior
              (let ((comment-keymap context-keymap))
                (let* ((comment-type (cdr (assq 'shipit-comment-type comment)))
                       (icon (if (and comment-type (string= comment-type "review")) "📋" "👤"))
                       ;; Add nil-safe guards for all string variables
                       (safe-thread-prefix (or thread-prefix ""))
                       (safe-tree-indicator (or tree-indicator ""))
                       (safe-user (or user "Unknown"))
                       (safe-timestamp (or formatted-timestamp ""))
                       (heading-text (format "%s%s%s %s (%s)%s"
                                             safe-thread-prefix safe-tree-indicator icon
                                             safe-user safe-timestamp
                                             (if is-outdated " [OUTDATED]" "")))
                       (heading-with-props (propertize heading-text
                                                       'font-lock-face user-face
                                                       'shipit-icon-color orange-color
                                                       'shipit-comment t
                                                       'shipit-comment-id comment-id
                                                       'shipit-comment-body (shipit--clean-text (or (cdr (assq 'body comment)) ""))
                                                       'shipit-file-path (cdr (assq 'path comment))
                                                       'shipit-line-number (or (cdr (assq 'line comment)) 0)
                                                       'shipit-repo (shipit--get-repo-from-remote)
                                                       'shipit-pr-number pr-number
                                                       'keymap comment-keymap
                                                       'local-map comment-keymap)))
                  (let ((heading-start (point)))
                    ;; Build final heading with [RESOLVED] tag
                    (let ((final-heading-text (if is-resolved
                                                  (concat heading-with-props " [RESOLVED]")
                                                heading-with-props)))
                      (magit-insert-heading final-heading-text))
                    ;; Apply shadow + italic face to [RESOLVED] tag after insertion
                    (when is-resolved
                      (save-excursion
                        (goto-char heading-start)
                        (when (search-forward "[RESOLVED]" (line-end-position) t)
                          (add-face-text-property (match-beginning 0) (match-end 0) 'shadow t)
                          (add-face-text-property (match-beginning 0) (match-end 0) 'italic t))))))

                (magit-insert-section-body
                  ;; Insert comment body with proper indentation
                  ;; Root comments at 3 spaces, replies aligned with their tree indicator
                  (let ((body-indent (if (> actual-depth 0)
                                         (+ 3 (* (1- actual-depth) 6) 6) ; 3 base + depth offset + "└─ " width
                                       3)))
                    (shipit--insert-comment-body-only comment body-indent comment-keymap (cdr (assq 'path comment)) (shipit--get-repo-from-remote) pr-number (or (cdr (assq 'line comment)) 0)))

                  ;; Insert reactions with proper indentation (always shows with placeholder)
                  (let ((reactions (shipit--format-comment-reactions comment t)))
                    (when reactions
                      (insert "\n")  ;; Add blank line separator before reactions
                      (let ((reactions-indent (make-string (if (> actual-depth 0)
                                                               (+ 3 (* (1- actual-depth) 6) 6)
                                                             3) ?\s))
                            (reactions-start (point)))
                        (insert (format "%s%s\n" reactions-indent (shipit--clean-text reactions)))
                        ;; Add properties to reactions (capture comment-id in local var to avoid scope issues)
                        (let ((captured-comment-id comment-id))
                          (add-text-properties reactions-start (point)
                                               `(shipit-reactions t
                                                                  shipit-comment t
                                                                  shipit-comment-id ,captured-comment-id
                                                                  shipit-comment-body ,(shipit--clean-text (or (cdr (assq 'body comment)) ""))
                                                                  shipit-file-path ,(cdr (assq 'path comment))
                                                                  shipit-line-number ,(or (cdr (assq 'line comment)) 0)
                                                                  shipit-repo ,(shipit--get-repo-from-remote)
                                                                  shipit-pr-number ,pr-number
                                                                  keymap ,comment-keymap
                                                                  local-map ,comment-keymap))))))

                  ;; Insert separator newline with comment properties for navigation
                  (let ((newline-start (point)))
                    (insert "\n")
                    (add-text-properties newline-start (point)
                                         `(shipit-comment t
                                                          shipit-comment-id ,comment-id
                                                          shipit-comment-body ,(shipit--clean-text (or (cdr (assq 'body comment)) ""))
                                                          shipit-file-path ,(cdr (assq 'path comment))
                                                          shipit-line-number ,(or (cdr (assq 'line comment)) 0)
                                                          shipit-repo ,(shipit--get-repo-from-remote)
                                                          shipit-pr-number ,pr-number
                                                          keymap ,comment-keymap
                                                          local-map ,comment-keymap))))

                ;; Recursively insert replies as nested child sections
                (let ((replies (when threads (gethash comment-id threads))))
                  (when replies
                    (dolist (reply replies)
                      (shipit--insert-diff-threaded-comment reply threads (1+ depth))))))
              ;; Apply rounded background to comment section
              (when (display-graphic-p)
                (shipit-rounded--apply-to-section
                 magit-insert-section--current orange-color)))))
      (error err
             (when (fboundp 'shipit--debug-log)
               (shipit--debug-log "❌ ERROR in diff-comment section for comment %s: %s" comment-id (error-message-string err))
               (shipit--debug-log "❌ FULL BACKTRACE:")
               (shipit--debug-log "%s" (with-output-to-string (backtrace))))
             ;; Re-signal the error with context instead of suppressing it
             (error "Failed to insert comment %s: %s" comment-id (error-message-string err))))))

(defun shipit--cached-comments-match-pr (pr-number repo _pr-data)
  "Check if cached comments in shipit buffer match the given PR data including commit SHAs."
  (when (and pr-number repo)
    (let ((status-buffer (cl-find-if (lambda (buf)
                                       (with-current-buffer buf
                                         (let ((is-shipit (string-prefix-p "*shipit" (buffer-name buf)))
                                               (has-pr (and (boundp 'shipit-buffer-pr-number) shipit-buffer-pr-number))
                                               (has-comments (and (boundp 'shipit--cached-inline-comments) shipit--cached-inline-comments)))
                                           (and is-shipit has-pr has-comments))))
                                     (buffer-list))))
      (when status-buffer
        (with-current-buffer status-buffer
          (equal shipit-buffer-pr-number pr-number))))))

;; Quick fix: Add working function definition above the problematic area
(defun shipit--cached-general-comments-match-pr (pr-number repo)
  "TEMPORARY: Check if cached general comments match the given PR number and repo."
  (when pr-number
    ;; Return t if we have cached comments, allowing them to be displayed
    ;; This prevents infinite loading by allowing cached comments to be used
    (and (boundp 'shipit--cached-general-comments)
         shipit--cached-general-comments
         (> (length shipit--cached-general-comments) 0))))

(defun shipit--get-cached-comments-from-shipit-buffer ()
  "Try to get cached inline comments from a shipit buffer.
This allows diff buffers to access comments loaded in the shipit buffer."
  (let ((shipit-buffer (cl-find-if (lambda (buf)
                                     (with-current-buffer buf
                                       (and (string-prefix-p "*shipit" (buffer-name buf))
                                            (boundp 'shipit--cached-inline-comments)
                                            shipit--cached-inline-comments)))
                                   (buffer-list))))
    (when shipit-buffer
      (with-current-buffer shipit-buffer
        shipit--cached-inline-comments))))

(eval-and-compile
  (require 'magit-section))

(defun shipit--insert-hierarchical-comments ()
  "Insert comment sections directly into hunk sections during diff construction.
This function is designed to be added to magit-diff-sections-hook."
  ;; Only run in shipit-managed diff buffers (buffer-local or transient context)
  (when (or shipit--diff-pr-context shipit--refresh-pr-context)
    (shipit--debug-log 'file-diff "🔍 shipit--insert-hierarchical-comments: ENTRY with PR context")
    (let ((start-time (current-time)))
      (shipit--debug-log 'file-diff "shipit--insert-hierarchical-comments: ⏱️ Starting hierarchical comment insertion")
      (when (fboundp 'magit-insert-section)
        (let* ((repo (or (cdr shipit--diff-pr-context)
                         (cadr shipit--refresh-pr-context)))
               (branch (when (fboundp 'magit-get-current-branch) (magit-get-current-branch))))
          (let* (;; Priority 1: transient context (consumed on first use during diff creation)
                 ;; Priority 2: buffer-local context (persists for re-renders)
                 (pr-data (cond
                           ((and shipit--refresh-pr-context
                                 (equal (cadr shipit--refresh-pr-context) repo))
                            (let ((displayed-pr-number (car shipit--refresh-pr-context)))
                              (setq shipit--refresh-pr-context nil)
                              (setq shipit--refresh-target-file-path nil)
                              ;; Stamp the buffer-local so re-renders work
                              (setq-local shipit--diff-pr-context
                                          (cons displayed-pr-number repo))
                              (condition-case _err
                                  (shipit-get-pull-request displayed-pr-number)
                                (error nil))))
                           (shipit--diff-pr-context
                            (condition-case _err
                                (shipit-get-pull-request (car shipit--diff-pr-context))
                              (error nil)))
                           (t nil)))
                 (pr-number (when pr-data (cdr (assq 'number pr-data)))))
            ;; Get cached comments - but only use shipit buffer cache if it's for the same PR
            (let* ((cached-comments
                    (or shipit--cached-inline-comments
                        (when (shipit--cached-comments-match-pr pr-number repo pr-data)
                          (shipit--get-cached-comments-from-shipit-buffer)))))

              (when (and pr-number (not cached-comments))
                (shipit--debug-log "BUG: PR #%s has context but no cached comments — loading sequence issue" pr-number))

              ;; Process comments
              (when cached-comments
                (shipit--debug-log 'file-diff "STEP4: Processing %d cached comments for display" (length cached-comments))
                (save-excursion
                  (goto-char (point-min))

                  ;; First, process file-level comments (insert before any hunks)
                  (when cached-comments
                    (let* ((target-file-path (or shipit--refresh-target-file-path
                                                 (and (boundp 'shipit--target-file-path) shipit--target-file-path)))
                           (file-level-comments (when target-file-path
                                                  (cl-remove-if-not
                                                   (lambda (comment)
                                                     (and (equal (cdr (assq 'subject_type comment)) "file")
                                                          ;; Only include file-level comments for the target file
                                                          (string= (cdr (assq 'path comment)) target-file-path)))
                                                   cached-comments))))
                      (when file-level-comments
                        (shipit--debug-log 'file-diff "Found %d file-level comments for target file %s, inserting at top of diff"
                                           (length file-level-comments)
                                           (or target-file-path "unknown"))
                        ;; Insert file-level comments at the very beginning
                        (goto-char (point-min))
                        ;; Skip any diff header lines to find a good insertion point
                        (while (and (not (eobp)) (looking-at "^\\(diff\\|index\\|---\\|\\+\\+\\+\\|@@\\)"))
                          (forward-line 1))
                        ;; Insert file-level comments here
                        (magit-insert-section (file-level-comments nil)
                          ;; Insert each file-level comment using same rendering as Files section
                          (let ((threads (make-hash-table :test 'equal)))
                            (dolist (comment file-level-comments)
                              (shipit--insert-threaded-file-comment comment threads repo pr-number 0)))))))

                  ;; Then find all hunk sections and insert line-specific comments as children
                  (goto-char (point-min))
                  (while (re-search-forward "^@@.*@@" nil t)
                    (let ((hunk-section (magit-current-section)))
                      (when (and hunk-section
                                 (eq (oref hunk-section type) 'hunk))
                        (shipit--debug-log 'file-diff "Found hunk section, checking for comments")
                        ;; Use the old system's precise line-by-line positioning but with hierarchical sections
                        (let* ((hunk-header (buffer-substring-no-properties
                                             (line-beginning-position) (line-end-position)))
                               (hunk-start-line (when (string-match "@@ -[0-9]+,?[0-9]* \\+\\([0-9]+\\)" hunk-header)
                                                  (string-to-number (match-string 1 hunk-header))))
                               (current-file (condition-case nil
                                                 (when hunk-section
                                                   (let ((parent (oref hunk-section parent)))
                                                     (when parent (oref parent value))))
                                               (error nil)))
                               ;; Get comments for this file only
                               (file-comments (when (and cached-comments current-file)
                                                (cl-remove-if-not
                                                 (lambda (comment)
                                                   (let ((comment-path (cdr (assq 'path comment))))
                                                     (and comment-path
                                                          (string= comment-path current-file))))
                                                 cached-comments))))
                          (when (and hunk-start-line file-comments)
                            (shipit--debug-log 'file-diff "Processing hunk starting at line %d with %d file comments"
                                               hunk-start-line (length file-comments))
                            ;; Process each line in the hunk to find where comments belong
                            (forward-line 1)
                            (let ((current-line-number hunk-start-line)
                                  (magit-insert-section--parent hunk-section))  ; Set parent for hierarchical sections
                              (while (and (not (eobp))
                                          (looking-at "^[-+ ]"))
                                (let ((line-start (line-beginning-position))
                                      (line-end (line-end-position)))
                                  ;; Check if any comments belong to this line
                                  (when (looking-at "^[+ ]")  ; Added or context lines (not removed lines)
                                    (let ((line-comments (cl-remove-if-not
                                                          (lambda (comment)
                                                            (= (or (cdr (assq 'line comment)) 0) current-line-number))
                                                          file-comments)))
                                      (when line-comments
                                        (shipit--debug-log 'file-diff "Found %d comments for line %d" (length line-comments) current-line-number)
                                        ;; Insert comments right after this line using hierarchical magit sections
                                        (goto-char line-end)
                                        (forward-line 1)
                                        ;; Use original working magit section implementation
                                        (magit-insert-section (shipit-comments current-line-number)
                                          (progn
                                            ;; Use hierarchical threading but with working section types
                                            (let ((threads (shipit--group-comments-by-api-replies line-comments)))
                                              (shipit--debug-log 'file-diff "DIFF HIERARCHICAL: Found %d threads for %d comments"
                                                                 (hash-table-count threads) (length line-comments))
                                              (dolist (comment line-comments)
                                                (let ((reply-to (cdr (assq 'in_reply_to_id comment))))
                                                  (unless reply-to ; Only process root comments - replies handled recursively
                                                    (shipit--debug-log "DIFF HIERARCHICAL: Processing root comment %s (depth=0)" (cdr (assq 'id comment)))
                                                    (shipit--insert-threaded-file-comment comment threads repo pr-number 0))))))))))
                                  ;; Update line number for added/context lines only
                                  (when (looking-at "^[+ ]")
                                    (setq current-line-number (1+ current-line-number))))
                                (forward-line 1))))))))))))))
      (shipit--debug-log 'file-diff "shipit--insert-hierarchical-comments: ⏱️ Completed in %.2f seconds"
                         (float-time (time-since start-time))))))

(defun shipit-setup-diff-comments ()
  "Set up inline comments in magit diff buffers.
The hook function only runs when PR context is available."
  (add-hook 'magit-diff-sections-hook #'shipit--insert-hierarchical-comments t))

(with-eval-after-load 'magit
  ;; (shipit-setup-magit-integration) ;; Disabled - user must manually enable via shipit-dwim
  (shipit-setup-diff-comments))

;;; -------------------------------------------------------------------------
;;; Block 5: Setup
;;; -------------------------------------------------------------------------

(defun shipit-start-magit-integration ()
  "Start GitHub code review interface via Magit integration.
Ensures Shipit is properly set up and opens magit-status for the current repository."
  (interactive)
  ;; Auto-detect repository if not set
  (unless (shipit--ensure-repository)
    (call-interactively 'shipit-set-repository))

  (if (fboundp 'magit-status)
      (magit-status)
    (message "Shipit setup complete! Repository: %s. Install Magit for full integration."
             shipit-current-repo)))

;;; -------------------------------------------------------------------------
;;; Block 6: File ops + More ediff
;;; -------------------------------------------------------------------------

(defun shipit--commit-at-point ()
  "Open magit-revision for commit at point."
  (interactive)
  (let ((commit-sha (get-text-property (point) 'shipit-commit-sha))
        (in-pr-section (shipit--in-pr-section-p)))
    (if (and commit-sha in-pr-section)
        (shipit--open-commit-revision commit-sha)
      ;; Not in a PR section or no commit SHA, use default magit behavior
      (when (fboundp 'magit-visit-thing)
        (magit-visit-thing)))))

(defun shipit--maybe-commit-at-point ()
  "Open magit-revision for commit or file diff at point, otherwise use default magit behavior."
  (interactive)
  (let ((commit-sha (get-text-property (point) 'shipit-commit-sha))
        (file-path (get-text-property (point) 'shipit-file-path)))
    (cond
     (commit-sha (shipit--open-commit-revision commit-sha))
     (file-path (shipit--file-at-point))
     ;; Fall back to default magit behavior
     (t (when (fboundp 'magit-visit-thing)
          (call-interactively #'magit-visit-thing))))))

(defun shipit--maybe-commit-at-point-or-scroll ()
  "Open magit-revision for commit or file diff at point, otherwise scroll."
  (interactive)
  (let ((commit-sha (get-text-property (point) 'shipit-commit-sha))
        (file-path (get-text-property (point) 'shipit-file-path)))
    (cond
     (commit-sha (shipit--open-commit-revision commit-sha))
     (file-path (shipit--file-at-point))
     ;; Fall back to default magit scroll behavior
     (t (when (fboundp 'magit-diff-show-or-scroll-up)
          (call-interactively #'magit-diff-show-or-scroll-up))))))

(defun shipit--open-commit-revision (sha)
  "Open magit-revision buffer for commit SHA."
  (if (and sha (fboundp 'magit-show-commit))
      (progn
        (shipit--debug-log "Opening magit-revision for commit: %s" sha)
        ;; Mark commit as read and clear unread indicator
        (shipit--mark-commit-as-read sha)
        ;; Set flag for this commit before opening
        (setq shipit--pending-commit-for-comments sha)
        (magit-show-commit sha)
        ;; Start flag-based retry system
        (shipit--start-comment-retry-system sha))
    (message "Cannot open commit: %s" (or sha "unknown SHA"))))

(defun shipit--show-commit-file-diff ()
  "Show diff for the commit file at point.
Uses magit-diff to show the diff of a single file for a specific commit."
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (commit-sha (get-text-property (point) 'shipit-commit-sha)))
    (if (and file-path commit-sha)
        (if (fboundp 'magit-diff-range)
            (progn
              (shipit--debug-log "Showing diff for %s @ %s" file-path commit-sha)
              (magit-diff-range (format "%s^..%s" commit-sha commit-sha)
                                nil (list file-path)))
          (user-error "magit-diff-range not available"))
      (user-error "No commit file at point"))))

(defun shipit--show-diff-at-point ()
  "Show plain diff for file at point (no inline comments).
Works for both PR files and commit files.
Uses raw magit-diff-range without shipit's PR context to avoid comment injection."
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (commit-sha (get-text-property (point) 'shipit-commit-sha))
         (pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo)))
    (cond
     ;; Commit file - show commit diff
     ((and file-path commit-sha)
      (if (fboundp 'magit-diff-range)
          (progn
            (shipit--debug-log "Showing plain commit diff for %s @ %s" file-path commit-sha)
            ;; Clear PR context to prevent comment injection
            (let ((shipit--current-displayed-pr nil))
              (magit-diff-range (format "%s^..%s" commit-sha commit-sha)
                                nil (list file-path))))
        (user-error "magit-diff-range not available")))
     ;; PR file - show PR diff using magit-diff-range with auto-fetch
     ((and file-path pr-number repo)
      (shipit--with-local-or-api
       repo pr-number
       (lambda (base-sha head-sha head-ref base-ref _pr-data)
         (when (fboundp 'magit-diff-range)
           (let ((range (shipit--compute-pr-merge-base-range base-ref head-ref base-sha head-sha repo)))
             (shipit--debug-log "Showing plain PR diff for %s range=%s" file-path range)
             (let ((shipit--current-displayed-pr nil))
               (shipit--magit-diff-range-with-fetch range '() (list file-path) head-ref base-ref pr-number repo)))))
       (lambda (_api-repo _api-pr-number)
         (shipit--debug-log "Using API patch for diff: %s (no local repo match)" file-path)
         (shipit--show-pr-file-patch repo pr-number file-path))))
     (t
      (user-error "No file at point")))))

(defun shipit--show-diff-with-comments-at-point ()
  "Show diff for file at point with inline comments.
For commit files within a PR context, shows PR diff with comments.
For PR files, shows PR diff with comments."
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (commit-sha (get-text-property (point) 'shipit-commit-sha))
         ;; Try text property first, then buffer-local PR context
         (pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (bound-and-true-p shipit-buffer-pr-number)))
         (repo (or (get-text-property (point) 'shipit-repo)
                   (bound-and-true-p shipit-buffer-repo)
                   (shipit--get-repo-from-remote))))
    (cond
     ;; File with PR context available - show PR diff with comments
     ((and file-path pr-number repo)
      (shipit--open-pr-file-diff repo pr-number file-path))
     ;; Commit file without PR context - fall back to plain diff
     ((and file-path commit-sha)
      (message "No PR context, showing plain diff")
      (shipit--show-commit-file-diff))
     (t
      (user-error "No file at point")))))

(defun shipit--open-commit-file-ediff (commit-sha file-path)
  "Open FILE-PATH in ediff, comparing parent and COMMIT-SHA versions."
  (if (not (and commit-sha file-path))
      (user-error "Cannot open ediff: missing commit-sha or file-path")
    (let ((short-sha (substring commit-sha 0 (min 7 (length commit-sha)))))
      (shipit--open-file-ediff (format "%s^" commit-sha) commit-sha file-path
                               (format "%s~" short-sha)
                               short-sha))))

(defun shipit--should-use-review-mode-p (pr-number pr-repo pr-head-sha)
  "Check if we should use review mode for this PR.
Returns t if:
1. A worktree exists for this PR
2. The worktree is in-sync with current PR head"
  (when (and (fboundp 'shipit--find-worktree-for-pr)
             (fboundp 'shipit--worktree-in-sync-p))
    (let ((worktree-path (shipit--find-worktree-for-pr pr-number pr-repo)))
      (and worktree-path
           (shipit--worktree-in-sync-p worktree-path pr-head-sha)))))

(defun shipit--open-file-in-review-mode (file-path worktree-path pr-number pr-repo pr-head-sha pr-data)
  "Open FILE-PATH from WORKTREE-PATH in review mode with PR context.
FILE-PATH is relative to repo root.
WORKTREE-PATH is absolute path to worktree directory.
PR-NUMBER, PR-REPO, PR-HEAD-SHA are PR identifiers.
PR-DATA is the full PR data object."
  (let ((full-file-path (expand-file-name file-path worktree-path)))
    (unless (file-exists-p full-file-path)
      (user-error "File not found in worktree: %s" full-file-path))
    (let ((source-buffer (current-buffer))
          (buf (find-file full-file-path))
          (comments-from-source (and (bound-and-true-p shipit--cached-inline-comments)
                                     shipit--cached-inline-comments)))
      (shipit--debug-log "Opening file in review mode: source-buffer=%s has-comments=%s count=%s"
                        (buffer-name source-buffer)
                        (if comments-from-source t nil)
                        (if comments-from-source (length comments-from-source) 0))
      (with-current-buffer buf
        ;; Store PR context in buffer-local variables for review mode
        (setq-local shipit-review--pr-data pr-data)
        (setq-local shipit-review--head-sha pr-head-sha)
        (setq-local shipit-review--base-sha (cdr (assq 'sha (cdr (assq 'base pr-data)))))
        ;; Copy cached comments from the source buffer (magit status buffer)
        (when comments-from-source
          (setq-local shipit--cached-inline-comments comments-from-source)
          (shipit--debug-log "Copied %d cached comments to review mode buffer" (length comments-from-source)))
        (when (fboundp 'shipit-review-mode)
          (shipit-review-mode 1)
          (shipit--debug-log "Opened file %s in review mode from worktree (base-sha=%s head-sha=%s)"
                             file-path
                             (substring (or shipit-review--base-sha "unknown") 0 7)
                             (substring pr-head-sha 0 7))))
      buf)))

(defun shipit--file-at-point ()
  "Open file from review mode if worktree available, otherwise open diff.
Checks for available worktree first:
1. If worktree exists and in-sync, open file in review mode
2. Otherwise, fall back to magit-diff (current behavior)"
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (repo (get-text-property (point) 'shipit-repo))
         (pr-number (get-text-property (point) 'shipit-pr-number))
         (pr-head-sha (get-text-property (point) 'shipit-pr-head-sha))
         ;; Try to get full PR data from buffer context
         (pr-data (get-text-property (point) 'shipit-pr-data)))
    (if (and file-path repo pr-number pr-head-sha pr-data)
        (if (shipit--should-use-review-mode-p pr-number repo pr-head-sha)
            ;; Use review mode with worktree
            (let ((worktree-path (shipit--find-worktree-for-pr pr-number repo)))
              (shipit--open-file-in-review-mode file-path worktree-path pr-number repo pr-head-sha pr-data))
          ;; Fall back to diff view (current behavior)
          (shipit--open-pr-file-diff repo pr-number file-path))
      ;; No file information - try overlay actions, then fall back to DWIM
      (or (shipit--try-overlay-action-at-point)
          (if (fboundp 'shipit-dwim)
              (shipit-dwim)
            (message "Cannot open file: missing file information"))))))

(defun shipit--ediff-file-at-point ()
  "Open ediff for file at point, comparing base and head versions.
Handles both PR files (comparing base..head) and commit files (comparing parent..commit).
If on Files header (pr-files section), show list of files to choose from.
If on a specific file, open ediff for that file."
  (interactive)
  (shipit--debug-log "shipit--ediff-file-at-point called")
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (commit-sha (get-text-property (point) 'shipit-commit-sha))
         (pr-number (get-text-property (point) 'shipit-pr-number))
         (repo (get-text-property (point) 'shipit-repo)))
    (cond
     ;; Case 1: Commit file - compare parent..commit
     ((and file-path commit-sha)
      (shipit--debug-log "ediff commit file: %s @ %s" file-path commit-sha)
      (shipit--open-commit-file-ediff commit-sha file-path))

     ;; Case 2: PR file - compare base..head
     ((and file-path pr-number repo)
      (shipit--debug-log "ediff PR file: %s in PR #%s" file-path pr-number)
      (shipit--open-pr-file-ediff repo pr-number file-path))

     ;; Case 3: On Files section header - show file selection menu
     ((and (fboundp 'magit-current-section)
           (magit-section-match '(pr-files) (magit-current-section)))
      (let* ((section (magit-current-section))
             (repo (get-text-property (oref section start) 'shipit-repo))
             (pr-number (get-text-property (oref section start) 'shipit-pr-number))
             (file-sections (oref section children))
             (file-choices (mapcar (lambda (file-sec) (oref file-sec value))
                                   file-sections)))
        (shipit--debug-log "Files header: repo=%s pr-number=%s file-count=%s"
                           repo pr-number (length file-sections))
        (when (and (not pr-number) file-sections)
          (let ((first-file (car file-sections)))
            (setq repo (or repo (get-text-property (oref first-file start) 'shipit-repo)))
            (setq pr-number (or pr-number (get-text-property (oref first-file start) 'shipit-pr-number)))))
        (if file-choices
            (let ((selected-file (completing-read "Select file to ediff: " file-choices nil t)))
              (when selected-file
                (shipit--open-pr-file-ediff repo pr-number selected-file)))
          (message "No files in this PR"))))

     ;; Case 4: Nothing actionable
     (t
      (message "No file at point for ediff")))))

(defun shipit--ediff-add-comment ()
  "Add a PR comment at the current line in ediff buffer."
  (interactive)
  (if (and (boundp 'shipit--ediff-pr-number)
           (boundp 'shipit--ediff-repo)
           (boundp 'shipit--ediff-file-path)
           (boundp 'shipit--ediff-head-sha))
      (let ((line-number (line-number-at-pos))
            (pr-number shipit--ediff-pr-number)
            (repo shipit--ediff-repo)
            (file-path shipit--ediff-file-path)
            (head-sha shipit--ediff-head-sha))
        ;; Prompt for comment text
        (let ((comment-text (read-string (format "Add comment at line %d: " line-number))))
          (when (and comment-text (not (string-empty-p comment-text)))
            (message "Adding comment to PR #%s at %s:%d..." pr-number file-path line-number)
            ;; Use the existing comment API (side is RIGHT for changes in head)
            (if (fboundp 'shipit--add-comment-to-pr)
                (shipit--add-comment-to-pr pr-number file-path line-number comment-text "RIGHT")
              (message "shipit--add-comment-to-pr function not available")))))
    (message "Cannot add comment: not in a shipit ediff buffer")))

(provide 'shipit-pr-diff)
;;; shipit-pr-diff.el ends here
