;;; shipit-file-filter.el --- File filter subsystem -*- lexical-binding: t; -*-

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
;; File filtering for PR files sections: text-based filename filtering,
;; comment-only filtering, and CODEOWNERS ownership filtering.
;; Extracted from shipit-magit.el.

;;; Code:
(require 'cl-lib)
(require 'transient)
(require 'shipit-lib)
(require 'shipit-core)

;; Ensure magit-section macros are available at compile time
(eval-when-compile
  (require 'magit-section))
(require 'magit-section)

;; Forward declarations — functions
(declare-function shipit--debug-log "shipit-core")
(declare-function shipit--fetch-codeowners-from-base-branch "shipit-http")
(declare-function shipit--parse-codeowners-patterns "shipit-lib")
(declare-function shipit--get-github-username "shipit-lib")
(declare-function shipit--get-user-teams-in-org "shipit-http")
(declare-function shipit--file-viewed-p "shipit-pr-sections")
(declare-function shipit--user-matches-owner-p "shipit-lib")
(declare-function shipit--codeowners-pattern-to-regexp "shipit-lib")
(declare-function shipit--insert-files-content "shipit-pr-sections")
(declare-function shipit-preview--refresh-files-section-only "shipit-preview")

;; Forward declarations — variables
(defvar shipit-buffer-repo)
(defvar shipit-buffer-pr-data)
(defvar shipit-buffer-pr-number)
(defvar shipit--cached-inline-comments)

;;; Buffer-local filter state

(defvar-local shipit--files-filter-text ""
  "Current filter text for files section (main Files Changed).
Only files matching this text (case-insensitive) will be displayed.")

(defvar-local shipit--files-filter-mode nil
  "Current filter mode for files section (main Files Changed).
nil means no special filter, 'comments means files with comments only,
'owned means files I own based on CODEOWNERS.")

;;; Filter predicates

(defun shipit--files-filter-active-p ()
  "Return non-nil if any file filter is currently active."
  (or shipit--files-filter-mode
      (not (string-empty-p shipit--files-filter-text))))

(defun shipit--get-files-filter-display ()
  "Get the display string for the current files filter.
Returns nil if no filter is active."
  (cond
   ((eq shipit--files-filter-mode 'comments)
    "files with comments")
   ((eq shipit--files-filter-mode 'owned)
    "files I own (CODEOWNERS)")
   ((eq shipit--files-filter-mode 'viewed)
    "viewed files")
   ((eq shipit--files-filter-mode 'unviewed)
    "unviewed files")
   ((not (string-empty-p shipit--files-filter-text))
    shipit--files-filter-text)
   (t nil)))

(defun shipit--in-files-section-p ()
  "Return non-nil if point is within a files section (pr-files or commit-files)."
  (when (fboundp 'magit-current-section)
    (let ((section (magit-current-section)))
      (while (and section
                  (not (memq (oref section type) '(pr-files commit-files))))
        (setq section (oref section parent)))
      (and section (memq (oref section type) '(pr-files commit-files))))))

(defun shipit--file-has-comments-p (filename)
  "Return non-nil if FILENAME has inline comments in the current PR.
Uses the buffer-local `shipit--cached-inline-comments' variable."
  (shipit--debug-log "FILE-FILTER: Checking comments for file '%s', cache has %d comments"
                     filename (if shipit--cached-inline-comments
                                 (length shipit--cached-inline-comments) 0))
  (let ((result (when (and filename shipit--cached-inline-comments)
                  (cl-some (lambda (comment)
                             (let ((comment-path (cdr (assq 'path comment))))
                               (shipit--debug-log "FILE-FILTER:   comment path='%s' match=%s"
                                                  comment-path (string= comment-path filename))
                               (and comment-path (string= comment-path filename))))
                           shipit--cached-inline-comments))))
    (shipit--debug-log "FILE-FILTER: Result for '%s': %s" filename result)
    result))

;;; CODEOWNERS filtering

(defvar-local shipit--codeowners-filter-cache nil
  "Cached CODEOWNERS data for filtering: (patterns github-user user-teams org-name).")

(defvar shipit--codeowners-debug-logged nil
  "Flag to only log CODEOWNERS debug once per filter operation.")

(defun shipit--get-codeowners-filter-data ()
  "Get cached CODEOWNERS patterns, username, and team membership for filtering.
Returns (patterns github-user user-teams org-name) or nil if not available."
  (unless shipit--codeowners-filter-cache
    (when (and shipit-buffer-repo shipit-buffer-pr-data)
      (let* ((base-ref (cdr (assq 'ref (cdr (assq 'base shipit-buffer-pr-data)))))
             (codeowners-text (when base-ref
                               (shipit--fetch-codeowners-from-base-branch
                                shipit-buffer-repo base-ref)))
             (patterns (shipit--parse-codeowners-patterns codeowners-text))
             (github-user (shipit--get-github-username))
             ;; Extract org name from repo (e.g., "org/repo" -> "org")
             (org-name (car (split-string shipit-buffer-repo "/")))
             ;; Fetch user's team memberships (cached by shipit--get-user-teams-in-org)
             (user-teams (when (and github-user org-name)
                          (shipit--get-user-teams-in-org github-user org-name))))
        (shipit--debug-log "CODEOWNERS-FILTER: base-ref=%s, patterns=%d, user=%s, org=%s, teams=%S"
                           base-ref (if patterns (length patterns) 0) github-user org-name user-teams)
        (setq shipit--codeowners-filter-cache (list patterns github-user user-teams org-name)))))
  shipit--codeowners-filter-cache)

(defun shipit--file-owned-by-user-p (filename)
  "Return non-nil if FILENAME is owned by the current user based on CODEOWNERS.
Checks both direct user ownership and team membership.
Uses cached CODEOWNERS data for performance."
  (when filename
    (let* ((cache-data (shipit--get-codeowners-filter-data))
           (patterns (nth 0 cache-data))
           (github-user (nth 1 cache-data))
           (user-teams (nth 2 cache-data))
           (owned nil)
           (matching-pattern nil)
           (matching-owners nil))
      (when (and patterns github-user)
        ;; Later patterns take precedence in CODEOWNERS, so check in reverse
        ;; and return the first (most specific) match
        (catch 'found
          (dolist (entry (reverse patterns))
            (let* ((pattern (car entry))
                   (owners (cdr entry))
                   (regexp (shipit--codeowners-pattern-to-regexp pattern)))
              (when (string-match-p regexp filename)
                (setq matching-pattern pattern)
                (setq matching-owners owners)
                ;; Check if current user is in owners list (direct or via team)
                (dolist (owner owners)
                  (when (shipit--user-matches-owner-p owner github-user user-teams)
                    (setq owned t)
                    (throw 'found t)))
                ;; Found a matching pattern but user not in owners - stop here
                ;; (later patterns take precedence)
                (throw 'found nil))))))
      ;; Debug log first few files to help diagnose
      (unless shipit--codeowners-debug-logged
        (shipit--debug-log "CODEOWNERS-MATCH: file='%s' pattern='%s' owners=%S user='@%s' teams=%S owned=%s"
                           filename matching-pattern matching-owners github-user user-teams owned)
        (when (> (random 100) 95)  ; Log ~5% of files
          (setq shipit--codeowners-debug-logged t)))
      owned)))

(defun shipit--file-matches-filter-p (file filter-text)
  "Return non-nil if FILE matches the current filter criteria.
Checks `shipit--files-filter-mode' first:
- nil: use FILTER-TEXT for filename matching
- `comments': show only files with inline comments
- `owned': show only files the current user owns per CODEOWNERS
FILTER-TEXT is used for case-insensitive filename matching when mode is nil."
  (let ((filename (cdr (assq 'filename file))))
    (pcase shipit--files-filter-mode
      ('comments
       ;; Filter to files with comments only
       (shipit--file-has-comments-p filename))
      ('owned
       ;; Filter to files the user owns
       (shipit--file-owned-by-user-p filename))
      ('viewed
       (shipit--file-viewed-p filename))
      ('unviewed
       (not (shipit--file-viewed-p filename)))
      (_
       ;; Default: text-based filename filter
       (if (or (null filter-text) (string-empty-p filter-text))
           t  ; Empty or nil filter matches everything
         (let ((filter-lower (downcase filter-text)))
           (and filename (string-match-p filter-lower (downcase filename)))))))))

;;; Interactive filter commands

(defun shipit--set-files-filter ()
  "Set filter text for files section with live updates as you type."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-filter shipit--files-filter-text)
         (timer nil))
    (minibuffer-with-setup-hook
        (lambda ()
          ;; Set up a post-command hook to update the filter on each keystroke
          (add-hook 'post-command-hook
                    (lambda ()
                      ;; Cancel any pending timer
                      (when timer
                        (cancel-timer timer))
                      ;; Set a short timer to debounce updates (reduces flickering)
                      (setq timer
                            (run-with-idle-timer 0.1 nil
                                                 (lambda ()
                                                   (condition-case err
                                                       (let ((current-input (minibuffer-contents-no-properties)))
                                                         (when (buffer-live-p original-buffer)
                                                           (with-current-buffer original-buffer
                                                             (setq shipit--files-filter-text current-input)
                                                             (shipit--refresh-files-section-only)
                                                             ;; Force redisplay to show updates while in minibuffer
                                                             (redisplay t))))
                                                     (error
                                                      (message "Filter refresh error: %s" (error-message-string err))))))))
                    nil t))
      (let ((new-filter (read-string "Filter files (filename): "
                                     shipit--files-filter-text)))
        ;; Apply the final filter value
        (setq shipit--files-filter-text new-filter)
        (setq shipit--files-filter-mode nil)  ; Clear special filter mode
        (message "Files filter set to: %s"
                 (if (string-empty-p new-filter) "(none)" new-filter))
        ;; Refresh main Files Changed section (commit files update on re-expand)
        (shipit--refresh-files-section-only)))))

(defun shipit--filter-files-with-comments ()
  "Filter to show only files with inline comments."
  (interactive)
  (setq shipit--files-filter-mode 'comments)
  (setq shipit--files-filter-text "")  ; Clear text filter
  (shipit--refresh-files-section-only)
  (message "Showing files with comments only"))

(defun shipit--filter-files-owned ()
  "Filter to show only files I own based on CODEOWNERS.
Matches both direct user ownership (@username) and team membership (@org/team)."
  (interactive)
  (setq shipit--files-filter-mode 'owned)
  (setq shipit--files-filter-text "")  ; Clear text filter
  (setq shipit--codeowners-filter-cache nil)  ; Clear cache to force refresh
  (setq shipit--codeowners-debug-logged nil)  ; Reset debug flag
  (shipit--refresh-files-section-only)
  (message "Showing files I own (via direct ownership or team membership)"))

(defun shipit--filter-files-viewed ()
  "Filter to show only viewed files."
  (interactive)
  (setq shipit--files-filter-mode 'viewed)
  (setq shipit--files-filter-text "")
  (shipit--refresh-files-section-only)
  (message "Showing viewed files only"))

(defun shipit--filter-files-unviewed ()
  "Filter to show only unviewed files."
  (interactive)
  (setq shipit--files-filter-mode 'unviewed)
  (setq shipit--files-filter-text "")
  (shipit--refresh-files-section-only)
  (message "Showing unviewed files only"))

(defun shipit--clear-files-filter ()
  "Clear all file filters."
  (interactive)
  (setq shipit--files-filter-mode nil)
  (setq shipit--files-filter-text "")
  (setq shipit--codeowners-filter-cache nil)  ; Clear cache
  (shipit--refresh-files-section-only)
  (message "File filters cleared"))

(transient-define-prefix shipit-files-filter-transient ()
  "Filter files in the PR."
  ["Filter Files"
   [("f" "By filename" shipit--set-files-filter)
    ("c" "With comments" shipit--filter-files-with-comments)
    ("o" "I own (CODEOWNERS)" shipit--filter-files-owned)]
   [("v" "Viewed" shipit--filter-files-viewed)
    ("u" "Unviewed" shipit--filter-files-unviewed)
    ("x" "Clear filter" shipit--clear-files-filter)
    ("q" "Quit" transient-quit-one)]])

(defun shipit-files-filter ()
  "Filter files in the PR.
Works from anywhere within the Files Changed section."
  (interactive)
  (if (or (shipit--in-files-section-p)
          (get-text-property (point) 'shipit-pr-file)
          (get-text-property (point) 'shipit-pr-files))
      (shipit-files-filter-transient)
    (user-error "Press 'f' on the Files Changed section to filter")))

;;; Section refresh

(defvar shipit--files-section-refresh-in-progress nil
  "Guard to prevent concurrent files section refreshes.")

(defun shipit--refresh-files-section-only ()
  "Refresh the files section content while preserving section structure.
Updates only the BODY (content between `content` and `end` markers),
preserving the heading and parent/child relationships.
Works from any buffer - refreshes all visible shipit-mode buffers with files sections."
  ;; Guard against concurrent refreshes
  (unless shipit--files-section-refresh-in-progress
    (setq shipit--files-section-refresh-in-progress t)
    (unwind-protect
        (progn
          ;; Refresh files section in all shipit-mode and shipit-preview-mode buffers
          (let ((refreshed-count 0))
            (dolist (buffer (buffer-list))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (when (and (or (derived-mode-p 'shipit-mode)
                                 (derived-mode-p 'shipit-preview-mode))
                             (fboundp 'magit-current-section))
                    (let ((refresh-start (float-time)))
                      ;; For preview mode, do targeted files section refresh
                      (if (derived-mode-p 'shipit-preview-mode)
                          (when (fboundp 'shipit-preview--refresh-files-section-only)
                            (shipit-preview--refresh-files-section-only)
                            (setq refreshed-count (1+ refreshed-count)))
                        ;; For shipit-mode, do targeted section refresh
                        (save-excursion
                          (goto-char (point-min))
                          ;; Find the files section header using text property
                          (let ((header-pos (text-property-any (point-min) (point-max) 'shipit-pr-files t)))
                            (when header-pos
                              (goto-char header-pos)
                              (let* ((section (magit-current-section)))
                                (when (and section
                                           (condition-case nil (oref section start) (error nil)))
                                  (let ((inhibit-read-only t))
                                    (condition-case err
                                        (let* (;; Use magit's content marker - position after heading
                                               (content-pos (condition-case nil (oref section content) (error nil)))
                                               (section-end-pos (condition-case nil (oref section end) (error nil)))
                                               (repo shipit-buffer-repo)
                                               (pr-data shipit-buffer-pr-data)
                                               (pr-number shipit-buffer-pr-number))
                                          (when (and content-pos section-end-pos repo pr-data pr-number)
                                            ;; Clear old children list - they have stale markers after delete
                                            (oset section children nil)
                                            ;; Delete ONLY the content area (preserves heading)
                                            (delete-region content-pos section-end-pos)
                                            ;; Re-insert content at the content position
                                            (goto-char content-pos)
                                            ;; Bind parent so new child sections register correctly
                                            (let ((magit-insert-section--parent section))
                                              (shipit--insert-files-content repo pr-data pr-number))
                                            ;; Update section end marker
                                            (oset section end (copy-marker (point)))
                                            (setq refreshed-count (1+ refreshed-count))))
                                      (error
                                       (shipit--debug-log "TARGET-REFRESH: Section update failed: %s"
                                                          (error-message-string err))
                                       nil)))))))))
                      )))))))
      ;; Always clear the guard flag, even if there was an error
      (setq shipit--files-section-refresh-in-progress nil))))

(provide 'shipit-file-filter)
;;; shipit-file-filter.el ends here
