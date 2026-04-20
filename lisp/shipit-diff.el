;;; shipit-diff.el --- diff module -*- lexical-binding: t; -*-

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
;; diff internals split from monolithic shipit.el

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'shipit-lib)   ; For shipit--defer-refresh
(require 'shipit-pr-backends)
;; Magit section is optional for compilation
(when (locate-library "magit-section")
  (require 'magit-section))

;; Forward declarations
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-current-section "magit-section")
(declare-function shipit--try-overlay-action-at-point "shipit-render")
(declare-function shipit--get-repo-from-remote "shipit-core")
(declare-function shipit--in-shipit-context-p "shipit-core")
(declare-function shipit--get-current-pr-data "shipit-http")
(declare-function shipit-gh-etag-get-json-with-refresh-cache "shipit-gh-etag")
(declare-function shipit-gh-etag-invalidate-endpoint "shipit-gh-etag")
(declare-function shipit-pr--backend-id "shipit-pr-backends")
(declare-function shipit-get-pr-for-branch "shipit-http")
(declare-function shipit--edit-pr-description "shipit-http")
(declare-function shipit--open-file-diff "shipit-pr-diff")
(declare-function shipit--open-commit-revision "shipit-pr-diff")
(declare-function shipit--approval-dwim-actions "shipit-pr-actions")
(declare-function shipit--activity-event-actions "shipit-pr-actions")
(declare-function magit-visit-thing "magit-diff")
(declare-function shipit--clear-branch-cache "shipit-checks")
(declare-function shipit--try-open-check-workflow "shipit-checks")
(declare-function shipit--insert-labels-section "shipit-pr-sections")
(declare-function shipit--format-label "shipit-pr-sections")
(declare-function magit-current-section "magit-section")
(declare-function magit-section-hide "magit-section")
(declare-function shipit--get-repo-from-remote "shipit-http")
(declare-function magit-get-current-branch "magit-git")
(declare-function shipit--get-worktree-status "shipit-worktree")
(declare-function shipit--find-worktree-for-pr "shipit-worktree")
(declare-function shipit--get-repo-root "shipit-worktree")
(declare-function shipit-review-mode "shipit-review-mode")

;; Forward declarations for review mode variables
(defvar shipit-review--pr-data)
(defvar shipit-review--head-sha)
(defvar shipit-review--base-sha)

(defvar-local shipit--inline-comments-fetched nil
  "Flag indicating whether inline comments have been fetched.")

(defun shipit--position-cursor-on-diff-line (target-line diff-hunk)
  "Position cursor on TARGET-LINE in the current diff buffer.
DIFF-HUNK provides context to help locate the correct hunk."
  (when target-line
    (goto-char (point-min))
    ;; Look for the hunk that contains our target line
    (when (and diff-hunk (string-match "@@ -\\([0-9]+\\),?[0-9]* \\+\\([0-9]+\\),?[0-9]* @@" diff-hunk))
      (let ((old-start (string-to-number (match-string 1 diff-hunk)))
            (new-start (string-to-number (match-string 2 diff-hunk))))
        ;; Search for a similar hunk header in the diff buffer
        (when (re-search-forward (format "@@ .* \\+%d,.*@@" new-start) nil t)
          ;; Move to the beginning of the hunk
          (beginning-of-line)
          ;; Try to position on or near the target line
          (let ((hunk-start (point))
                (current-line new-start)
                (found-line nil))
            (forward-line 1)
            ;; Scan through the hunk to find the target line
            (while (and (not found-line)
                        (not (looking-at "^@@\\|^diff\\|^index\\|^\\\\")))
              (cond
               ;; Added line: increment line counter
               ((looking-at "^\\+")
                (when (= current-line target-line)
                  (setq found-line (point)))
                (setq current-line (1+ current-line)))
               ;; Context line: increment line counter
               ((looking-at "^ ")
                (when (= current-line target-line)
                  (setq found-line (point)))
                (setq current-line (1+ current-line)))
               ;; Removed line: don't increment counter
               ((looking-at "^-")
                nil))
              (forward-line 1))
            ;; Position cursor on the found line or approximate position
            (when found-line
              (goto-char found-line)
              (beginning-of-line)
              (message "Positioned cursor on line %d" target-line))))))))

(defun shipit--in-general-comments-section-p ()
  "Check if point is currently in the general comments section using simple text detection."
  (save-excursion
    ;; First check if we're on a General Comments header line
    (beginning-of-line)
    (if (looking-at ".*General Comments")
        t  ; We're on the General Comments header itself
      ;; Look backwards for section headers to determine where we are
      (let ((found-general nil)
            (found-inline nil)
            (search-limit (max (point-min) (- (point) 2000))))
        ;; Search backwards for either "General Comments" or "Inline Comments" headers
        (while (and (> (point) search-limit)
                    (not found-general)
                    (not found-inline))
          (forward-line -1)
          (beginning-of-line)
          (cond
           ((looking-at ".*General Comments")
            (setq found-general t))
           ((looking-at ".*Inline Comments")
            (setq found-inline t))))
        ;; We're in general comments section if we found "General Comments" header first
        found-general))))

(defun shipit--execute-action (mode input pr-number repo comment-info
                                    file-path line-number in-general on-comment
                                    side &optional pr-info on-pr-description
                                    labels-info on-labels pr-header-info on-pr-header
                                    reviewers-info on-reviewers)
  "Execute comment action."
  (pcase mode
    ('add
     (let* ((stored-file-path (get-text-property (point) 'shipit-file-path))
            (stored-line-number (get-text-property (point) 'shipit-line-number))
            (effective-file-path (or stored-file-path file-path))
            (effective-line-number (or stored-line-number line-number)))
       (shipit--debug-log "ADD COMMENT DETECTION: stored-file-path=%s stored-line-number=%s file-path=%s line-number=%s effective-file-path=%s effective-line-number=%s in-general=%s side=%s point=%s"
                          stored-file-path stored-line-number file-path line-number effective-file-path effective-line-number in-general side (point))
       (shipit--debug-log "TEXT PROPERTIES AT POINT: %s" (text-properties-at (point)))
       (cond
        ;; Case 1: In inline section but no file context - prompt for file and line
        ((not in-general)
         (cond
          ;; Sub-case 1a: File header (has file path but no line) - navigate to diff
          ((and effective-file-path (not effective-line-number))
           (shipit--debug-log "FILE HEADER NAVIGATION: file=%s, navigating to diff buffer" effective-file-path)
           (message "Navigate to the diff buffer to add inline comments on specific lines")
           (shipit--open-file-diff effective-file-path)
           nil)
          ;; Sub-case 1b: Normal inline comment (has both file path and line number)
          ((and effective-file-path effective-line-number)
           (shipit--add-comment-to-pr pr-number effective-file-path effective-line-number input (or side "RIGHT"))
           (shipit--refresh-after-comment-operation-with-hook
            t  ; inline comment = t
            'comment-added
            `((pr-number . ,pr-number) (file-path . ,effective-file-path) (line . ,effective-line-number) (body . ,input)))
           (message "Added inline comment")
           t)
          ;; Sub-case 1c: In inline section but no context - prompt for both file and line
          (t
           (let* ((target-file (read-string "File path for inline comment: "))
                  (target-line (read-number "Line number for inline comment: ")))
             (when (and (not (string-empty-p target-file)) target-line (> target-line 0))
               (shipit--add-comment-to-pr pr-number target-file target-line input (or side "RIGHT"))
               (shipit--refresh-after-comment-operation-with-hook
                t  ; inline comment = t
                'comment-added
                `((pr-number . ,pr-number) (file-path . ,target-file) (line . ,target-line) (body . ,input)))
               (message "Added inline comment to %s at line %d" target-file target-line)
               t)))))
        ;; Case 2: General comment (in general section)
        (t
         (progn
           (shipit--add-general-comment-to-pr pr-number input)
           (shipit--refresh-after-comment-operation-with-hook
            nil  ; inline comment = nil
            'general-comment-added
            `((pr-number . ,pr-number) (body . ,input)))
           (message "Added general comment")
           t)))))

    ('edit
     (cond
      ;; Edit PR description
      (on-pr-description
       (let ((pr-number (nth 0 pr-info)))
         (shipit--edit-pr-description pr-number input)
         (message "PR description updated")
         t))
      ;; Edit existing comment
      (comment-info
       (let* ((comment-id (car comment-info))
              ;; Check if this is a review comment from text properties
              (comment-type (get-text-property (point) 'shipit-comment-type))
              ;; Determine comment/review type for proper API endpoint selection
              (is-review (string= comment-type "review"))
              (is-inline-comment (and (not in-general) (not is-review))))
         (shipit--edit-comment comment-id input is-inline-comment is-review)
         (shipit--refresh-after-comment-operation-with-hook
          is-inline-comment
          'comment-edited
          `((comment-id . ,comment-id) (new-body . ,input)))
         (message "Comment updated")
         t))
      (t nil)))

    ('delete
     (when comment-info
       (let* ((comment-id (car comment-info))
              ;; Detect comment type: general comments are in general section, inline are not
              (is-inline-comment (not in-general)))
         (when (yes-or-no-p (format "Delete comment: %s? "
                                    (truncate-string-to-width (or (cdr comment-info) "") 40)))
           (shipit--delete-comment comment-id is-inline-comment)
           (shipit--refresh-after-comment-operation-with-hook
            is-inline-comment
            'comment-deleted
            `((comment-id . ,comment-id)))
           (message "Comment deleted")
           t))))

    ('reply
     (cond
      ;; Reply to PR description - add as general comment
      (on-pr-description
       (shipit--add-general-comment-to-pr pr-number input)
       (shipit--refresh-after-comment-operation nil)  ; inline comment = nil
       (message "Added reply to PR description")
       t)
      ;; Reply to existing comment
      (comment-info
       (let* ((parent-comment-id (car comment-info))
              (parent-comment-body (cdr comment-info))
              ;; Format reply with quoted parent comment
              (formatted-input (if (and parent-comment-body (fboundp 'shipit--format-quoted-reply))
                                   (concat (shipit--format-quoted-reply parent-comment-body) input)
                                 input)))
         (shipit--debug-log "🔍 REPLY DEBUG: in-general=%s, parent-comment-id=%s, comment-info=%S"
                            in-general parent-comment-id comment-info)
         (if (not in-general)
             ;; Reply to inline comment - use proper reply API with in_reply_to
             ;; Pass file-path for targeted refresh (reply function handles refresh internally)
             (progn
               (shipit--debug-log "📝 REPLY: Calling shipit--reply-to-inline-comment pr=%s parent=%s file-path=%s"
                                  pr-number parent-comment-id file-path)
               (shipit--reply-to-inline-comment pr-number parent-comment-id formatted-input file-path)
               ;; No separate refresh needed - reply function handles it
               (message "Added inline reply")
               t)
           ;; Reply to general comment - use reply API with in_reply_to
           (progn
             (shipit--debug-log "📝 REPLY: Calling shipit--reply-to-general-comment pr=%s parent=%s"
                                pr-number parent-comment-id)
             (shipit--reply-to-general-comment pr-number parent-comment-id formatted-input)
             (shipit--refresh-after-comment-operation nil)  ; inline comment = nil
             (message "Added general reply")
             t))))
      (t nil)))

    ('react
     (cond
      ;; React to PR description
      (on-pr-description
       (let ((reaction (shipit--select-reaction)))
         (when reaction
           ;; Check if user already has this reaction (toggle behavior)
           (if (shipit--user-has-pr-reaction pr-number reaction)
               ;; Remove existing reaction
               (progn
                 (shipit--remove-reaction-from-pr pr-number reaction)
                 (message "Removed %s reaction from PR" reaction))
             ;; Add new reaction
             (progn
               (shipit--add-reaction-to-pr pr-number reaction)
               (message "Added %s reaction to PR" reaction)))
           t)))
      ;; React to existing comment
      (comment-info
       (let* ((comment-id (car comment-info))
              (reaction (shipit--select-reaction))
              ;; Detect if this is an inline comment (not in general section)
              (is-inline (not in-general)))
         (when reaction
           ;; Check if user already has this reaction (toggle behavior)
           (shipit--debug-log "DIFF-REACT: About to check if user has reaction: comment-id=%s reaction=%s is-inline=%s" comment-id reaction is-inline)
           (let ((has-reaction (shipit--user-has-reaction comment-id reaction is-inline)))
             (shipit--debug-log "DIFF-REACT: shipit--user-has-reaction returned: %s" has-reaction)
             (if has-reaction
                 ;; Remove existing reaction
                 (progn
                   (shipit--debug-log "DIFF-REACT: Removing existing %s reaction from comment %s" reaction comment-id)
                   (shipit--remove-reaction-from-comment comment-id reaction repo pr-number is-inline)
                   (message "Removed %s reaction" reaction))
               ;; Add new reaction
               (progn
                 (shipit--debug-log "DIFF-REACT: Adding new %s reaction to comment %s" reaction comment-id)
                 (shipit--add-reaction-to-comment comment-id reaction repo pr-number is-inline)
                 (message "Added %s reaction" reaction))))
           ;; Set context for targeted refresh
           (setq shipit--current-operation-comment-id comment-id)
           ;; Refresh to show the updated reactions immediately
           (shipit--refresh-after-comment-operation is-inline)
           ;; Clear context after refresh
           (setq shipit--current-operation-comment-id nil)
           t)))
      (t nil)))

    ('manage-labels
     (when on-labels
       ;; Preserve cursor position during label editing to prevent jumping to top
       (let ((original-point (point))
             (original-window-start (window-start)))
         (shipit--manage-labels-interactive pr-number repo labels-info)
         ;; Restore cursor position after label update
         (goto-char original-point)
         (set-window-start (selected-window) original-window-start)
         (message "Labels updated")
         t)))

    ('review-approve
     (when (or on-pr-header on-reviewers)
       (shipit-post-review pr-number "APPROVE" nil)
       (message "PR approved")
       t))

    ('review-reject
     (when (or on-pr-header on-reviewers)
       ;; REQUEST_CHANGES typically requires a body, but let's try with empty string first
       (shipit-post-review pr-number "REQUEST_CHANGES"
                           (if (and input (not (string-empty-p input)))
                               input
                             "No specific comments"))
       (message "Changes requested")
       t))

    ('review-comment
     (when (or on-pr-header on-reviewers)
       (shipit-post-review pr-number "COMMENT" input)
       (message "Review comment submitted")
       t))

    ('review-dismiss
     (when (or on-pr-header on-reviewers)
       (shipit-dismiss-review pr-number input)
       (message "Review dismissed")
       t))

    ('review-undo-approve
     (when (or on-pr-header on-reviewers)
       (shipit-dismiss-review pr-number (if (and input (not (string-empty-p input))) input "Undoing approval"))
       (message "Approval undone")
       t))

    ('review-undo-reject
     (when (or on-pr-header on-reviewers)
       (shipit-dismiss-review pr-number (if (and input (not (string-empty-p input))) input "Undoing rejection"))
       (message "Rejection undone")
       t))

    (_ (message "Unknown action: %s" mode) nil)))

(defun shipit--open-file-from-worktree-or-repo (file-path pr-data &optional line-number)
  "Open FILE-PATH based on worktree status.
If worktree exists and is in-sync, open from worktree (editable).
If worktree is out-of-sync, ask user what to do.
If no worktree, open the file at PR head revision (read-only).
For cross-repo PRs (different repo than current), always opens read-only.
PR-DATA provides context for worktree lookup and status checking.
If LINE-NUMBER is provided, position cursor on that line."
  (let* ((pr-number (cdr (assq 'number pr-data)))
         (pr-head-sha (cdr (assq 'sha (cdr (assq 'head pr-data)))))
         (pr-repo (cdr (assq 'full_name (cdr (assq 'repo (cdr (assq 'base pr-data)))))))
         ;; For cross-repo PRs, shipit--get-worktree-status will error - treat as 'none
         (status (when (fboundp 'shipit--get-worktree-status)
                   (condition-case nil
                       (shipit--get-worktree-status pr-number pr-head-sha pr-repo)
                     (user-error nil))))  ; Cross-repo PRs have no local worktree
         (worktree-path (when (and (memq status '(in-sync out-of-sync))
                                   (fboundp 'shipit--find-worktree-for-pr))
                          (shipit--find-worktree-for-pr pr-number pr-repo))))
    (pcase status
      ('in-sync
       ;; Worktree is in sync - open editable file from worktree with review mode
       (let ((full-path (expand-file-name file-path worktree-path)))
         (shipit--debug-log "Opening file from in-sync worktree: %s" full-path)
         (if (file-exists-p full-path)
             (progn
               (find-file full-path)
               ;; Enable review mode with PR context
               (setq-local shipit-review--pr-data pr-data)
               (setq-local shipit-review--head-sha pr-head-sha)
               (setq-local shipit-review--base-sha (cdr (assq 'sha (cdr (assq 'base pr-data)))))
               (when (fboundp 'shipit-review-mode)
                 (shipit-review-mode 1))
               (when line-number
                 (goto-char (point-min))
                 (forward-line (1- line-number))))
           (user-error "File not found in worktree: %s" file-path))))

      ('out-of-sync
       ;; Worktree exists but is out of sync - ask user what to do
       (let ((choice (completing-read
                      (format "Worktree is out of sync. Open file: "
                              file-path)
                      '("from worktree (may differ from PR)"
                        "at PR revision (read-only)"
                        "sync worktree first")
                      nil t)))
         (pcase choice
           ("from worktree (may differ from PR)"
            (let ((full-path (expand-file-name file-path worktree-path)))
              (shipit--debug-log "Opening file from out-of-sync worktree: %s" full-path)
              (if (file-exists-p full-path)
                  (progn
                    (find-file full-path)
                    (when line-number
                      (goto-char (point-min))
                      (forward-line (1- line-number))))
                (user-error "File not found in worktree: %s" file-path))))
           ("at PR revision (read-only)"
            (shipit--open-file-at-revision file-path pr-head-sha line-number worktree-path pr-repo))
           ("sync worktree first"
            (when (fboundp 'shipit--sync-worktree)
              (let ((branch (cdr (assq 'ref (cdr (assq 'head pr-data))))))
                (if (shipit--sync-worktree worktree-path branch)
                    (let ((full-path (expand-file-name file-path worktree-path)))
                      (if (file-exists-p full-path)
                          (progn
                            (find-file full-path)
                            (when line-number
                              (goto-char (point-min))
                              (forward-line (1- line-number))))
                        (user-error "File not found in worktree: %s" file-path)))
                  (user-error "Failed to sync worktree"))))))))

      (_
       ;; No worktree in-sync/out-of-sync, but still try to find one for git context
       ;; This allows blob navigation (n/p) to work within the PR's commit history
       ;; For cross-repo PRs, worktree lookup will fail - that's fine
       (let ((wt-path (when (fboundp 'shipit--find-worktree-for-pr)
                        (condition-case nil
                            (shipit--find-worktree-for-pr pr-number pr-repo)
                          (user-error nil)))))
         (shipit--open-file-at-revision file-path pr-head-sha line-number wt-path pr-repo))))))

(defun shipit--get-diff-line-number-at-point ()
  "Get the file line number at point in a diff.
Returns the line number in the target file, or nil if not on a diff line.
Uses the shipit-line-number text property which is set when rendering diffs."
  ;; First try the text property (most reliable, handles inline comments correctly)
  (or (get-text-property (point) 'shipit-line-number)
      ;; Fallback: parse the hunk header if no property
      (save-excursion
        (let ((current-line (line-number-at-pos))
              (hunk-new-start nil))
          ;; Search backward for the hunk header
          ;; Shipit prefixes with 5 spaces, so match both "^@@" and "^     @@"
          (when (re-search-backward "^\\(?:     \\)?@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@" nil t)
            (setq hunk-new-start (string-to-number (match-string 1)))
            ;; Count lines from hunk header to current position
            ;; Only count lines that exist in the new file (context and additions, not deletions)
            (let ((new-line-offset 0))
              (forward-line 1)  ; Move past the @@ line
              (while (< (line-number-at-pos) current-line)
                ;; Skip lines that have shipit-comment property (inline comments)
                (unless (get-text-property (point) 'shipit-comment)
                  ;; Get the diff indicator character (after any leading spaces)
                  ;; Shipit uses 5-space prefix, so check char at position 5
                  (let* ((line-start (line-beginning-position))
                         (char-at-5 (char-after (+ line-start 5)))
                         (char-at-0 (char-after line-start)))
                    ;; Use char at position 5 if line is long enough, otherwise char at 0
                    (let ((diff-char (if (and char-at-5 (> (- (line-end-position) line-start) 5))
                                         char-at-5
                                       char-at-0)))
                      ;; Count context lines (space) and additions (+), skip deletions (-)
                      (when (and diff-char (memq diff-char '(?\s ?+)))
                        (setq new-line-offset (1+ new-line-offset))))))
                (forward-line 1))
              ;; Check if current line is a deletion (shouldn't position there)
              (let* ((line-start (line-beginning-position))
                     (char-at-5 (char-after (+ line-start 5)))
                     (char-at-0 (char-after line-start))
                     (current-diff-char (if (and char-at-5 (> (- (line-end-position) line-start) 5))
                                            char-at-5
                                          char-at-0)))
                (if (and current-diff-char (eq current-diff-char ?-))
                    nil  ; On a deletion line, return nil
                  (+ hunk-new-start new-line-offset)))))))))

(defun shipit--blob-nav-disabled ()
  "Show message that blob navigation is disabled without worktree."
  (interactive)
  (let ((key (where-is-internal 'shipit-dwim shipit-mode-map t)))
    (message "Blob navigation requires a synced worktree. Use %s on Worktree section to create one."
             (if key (key-description key) "shipit-dwim"))))

(defun shipit--disable-blob-navigation ()
  "Disable blob navigation in current buffer by overriding n/p keys."
  (when (boundp 'magit-blob-mode-map)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map magit-blob-mode-map)
      (define-key map (kbd "n") #'shipit--blob-nav-disabled)
      (define-key map (kbd "p") #'shipit--blob-nav-disabled)
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'magit-blob-mode map)
                        minor-mode-overriding-map-alist)))))

(defun shipit--revision-exists-locally-p (revision git-dir)
  "Check if REVISION exists in the git repository at GIT-DIR."
  (let ((default-directory git-dir))
    (zerop (call-process "git" nil nil nil "cat-file" "-e" revision))))

(defun shipit--open-file-at-revision (file-path revision &optional line-number worktree-path repo)
  "Open FILE-PATH at REVISION using magit-find-file.
This opens the file in read-only mode showing the exact content at that revision.
If LINE-NUMBER is provided, position cursor on that line.
If WORKTREE-PATH is provided, use that directory as git context so blob
navigation (n/p) works within the PR's commit history.
If no WORKTREE-PATH, blob navigation is disabled to avoid confusion.
For cross-repo PRs (no local git context), fetches content via GitHub API.
REPO should be 'owner/repo' format for GitHub API fallback."
  (shipit--debug-log "Opening file at revision %s: %s (line %s, worktree %s, repo %s)"
                     revision file-path line-number worktree-path repo)
  (let* ((git-dir (or worktree-path
                      (shipit--get-repo-root)))
         (in-valid-git-repo (and git-dir
                                 (file-directory-p git-dir)
                                 (or (file-directory-p (expand-file-name ".git" git-dir))
                                     (file-exists-p (expand-file-name ".git" git-dir)))))
         ;; Check if the revision actually exists in this repo
         (revision-exists (and in-valid-git-repo revision
                               (shipit--revision-exists-locally-p revision git-dir))))
    (shipit--debug-log "git-dir=%s in-valid-git-repo=%s revision-exists=%s"
                       git-dir in-valid-git-repo revision-exists)
    (cond
     ;; Valid git context and revision exists locally - use magit-find-file
     ((and revision (fboundp 'magit-find-file) revision-exists)
      (shipit--debug-log "Using magit-find-file for %s at %s" file-path revision)
      (let ((default-directory git-dir))
        (condition-case err
            (progn
              (magit-find-file revision file-path)
              (when line-number
                (goto-char (point-min))
                (forward-line (1- line-number)))
              ;; Disable blob navigation if no worktree context
              (unless worktree-path
                (shipit--disable-blob-navigation)))
          (error
           ;; Git object not available locally - try GitHub API if repo available
           (shipit--debug-log "magit-find-file failed: %s" (error-message-string err))
           (if (and repo revision)
               (shipit--display-file-from-api repo file-path revision line-number)
             (message "File content not available locally. Use 'd' to view the diff."))))))

     ;; Revision doesn't exist locally or no valid git context - fetch via API
     ((and repo revision)
      (shipit--debug-log "Revision not available locally, fetching from API: %s" file-path)
      (shipit--display-file-from-api repo file-path revision line-number))

     ;; No repo info - can't fetch from GitHub
     ((not revision-exists)
      (shipit--debug-log "Revision not available locally and no repo info for GitHub API")
      (message "File content not available locally. Use 'd' to view the diff."))

     ;; Fallback if magit-find-file not available or no revision
     (t
      (let ((full-path (expand-file-name file-path
                                         (if (fboundp 'shipit--get-repo-root)
                                             (shipit--get-repo-root)
                                           default-directory))))
        (shipit--debug-log "Fallback: opening file from repo: %s" full-path)
        (find-file full-path)
        (when line-number
          (goto-char (point-min))
          (forward-line (1- line-number))))))))

(defun shipit--display-file-from-api (repo file-path revision &optional line-number)
  "Fetch and display FILE-PATH from REPO at REVISION via backend API.
Uses the active PR backend's :fetch-file-content if available,
falling back to `shipit--fetch-file-content-from-github'.
If LINE-NUMBER is provided, position cursor on that line."
  (shipit--debug-log "shipit--display-file-from-api: repo=%s file=%s rev=%s" repo file-path revision)
  (message "Fetching %s from API..." file-path)
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-file-content))
         (content (if fetch-fn
                      (funcall fetch-fn config file-path revision)
                    (shipit--fetch-file-content-from-github repo file-path revision))))
    (shipit--debug-log "Fetched content: %s bytes" (if content (length content) "nil"))
    (if content
        (let* ((buffer-name (format "*%s @ %s*" file-path (substring revision 0 (min 7 (length revision)))))
               (buf (get-buffer-create buffer-name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert content))
            ;; Set appropriate mode based on file extension.
            ;; `delay-mode-hooks' suppresses user mode-hooks (lsp-mode,
            ;; flyspell, flycheck, company-mode, …) on this read-only
            ;; SHA-blob buffer — font-lock and syntax setup from the
            ;; mode itself still run, but we skip the heavyweight tooling
            ;; that a real file-visit would attach.
            (let ((buffer-file-name file-path))
              (delay-mode-hooks (set-auto-mode)))
            ;; Disable font-lock in the ephemeral blob buffer.  Some
            ;; modes (notably `markdown-mode') have `syntax-propertize'
            ;; patterns that go into regex backtracking on real-world
            ;; content; fontifying them on display can hang hard enough
            ;; that `C-g' can't interrupt the C regex engine.  Users who
            ;; want colouring can `M-x font-lock-mode' locally.
            (font-lock-mode -1)
            (setq buffer-read-only t)
            (goto-char (point-min))
            (when line-number
              (forward-line (1- line-number))))
          (switch-to-buffer buf)
          (message "Fetched %s from %s" file-path repo))
      (user-error "Failed to fetch file content from API"))))

(defun shipit--open-pr-file-at-point ()
  "Open PR file at point based on worktree status.
For files in the Files Changed section:
- If worktree exists and is in-sync, open from worktree (editable).
- If worktree is out-of-sync, ask user what to do.
- If no worktree, open file at PR head revision (read-only).
If point is within a diff hunk, positions cursor on the corresponding line."
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (pr-data (get-text-property (point) 'shipit-pr-data))
         ;; Support both full pr-data and individual properties (from Files section)
         (pr-number (or (cdr (assq 'number pr-data))
                        (get-text-property (point) 'shipit-pr-number)))
         (pr-head-sha (or (cdr (assq 'sha (cdr (assq 'head pr-data))))
                          (get-text-property (point) 'shipit-pr-head-sha)))
         (pr-head-ref (or (cdr (assq 'ref (cdr (assq 'head pr-data))))
                          (get-text-property (point) 'shipit-pr-head-ref)))
         (pr-repo (or (cdr (assq 'full_name (cdr (assq 'repo (cdr (assq 'base pr-data))))))
                      (get-text-property (point) 'shipit-repo)))
         (line-number (shipit--get-diff-line-number-at-point)))
    ;; Mark inline comments as seen when user views a file
    (when (and pr-repo pr-number shipit--cached-inline-comments)
      (let ((current-ids (mapcar (lambda (c) (cdr (assq 'id c))) shipit--cached-inline-comments)))
        (shipit--mark-inline-comments-seen pr-repo pr-number current-ids)
        ;; Update section indicators to remove red dot
        (when (memq 'pr-files shipit--sections-with-unread)
          (setq shipit--sections-with-unread (delq 'pr-files shipit--sections-with-unread))
          (shipit--update-section-unread-indicators))))
    (shipit--debug-log "Opening PR file: path=%s pr-number=%s pr-head-sha=%s pr-repo=%s line=%s"
                       file-path pr-number pr-head-sha pr-repo line-number)
    (if file-path
        (if (and pr-number pr-head-sha pr-repo)
            ;; Construct minimal pr-data for the helper function
            (let ((minimal-pr-data `((number . ,pr-number)
                                     (head . ((sha . ,pr-head-sha)
                                              (ref . ,pr-head-ref)))
                                     (base . ((repo . ((full_name . ,pr-repo))))))))
              (shipit--debug-log "Opening file from worktree or repo: %s" file-path)
              (shipit--open-file-from-worktree-or-repo file-path minimal-pr-data line-number))
          (user-error "Missing PR data to determine worktree context"))
      (user-error "No file at point"))))

(defun shipit--open-commit-file-at-point ()
  "Open commit file at point at the specific commit revision.
For files in a Commit section, always opens at that commit's SHA (read-only).
If point is within a diff hunk, positions cursor on the corresponding line.
Uses worktree git context if available for blob navigation (n/p)."
  (interactive)
  (let* ((file-path (get-text-property (point) 'shipit-file-path))
         (commit-sha (get-text-property (point) 'shipit-commit-sha))
         (line-number (shipit--get-diff-line-number-at-point))
         ;; Get PR context from buffer or text properties for worktree lookup
         (pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (bound-and-true-p shipit-buffer-pr-number)))
         (pr-repo (or (get-text-property (point) 'shipit-repo)
                      (bound-and-true-p shipit-buffer-repo)
                      (shipit--get-repo-from-remote)))
         ;; Find worktree if available for git context
         (worktree-path (when (and pr-number pr-repo
                                   (fboundp 'shipit--find-worktree-for-pr))
                          (shipit--find-worktree-for-pr pr-number pr-repo))))
    (shipit--debug-log "Opening commit file: path=%s commit-sha=%s line=%s worktree=%s repo=%s"
                       file-path commit-sha line-number worktree-path pr-repo)
    (if file-path
        (if commit-sha
            (shipit--open-file-at-revision file-path commit-sha line-number worktree-path pr-repo)
          (user-error "No commit SHA at point"))
      (user-error "No file at point"))))

(defun shipit--open-file-at-point ()
  "Open file at point, dispatching to the appropriate handler.
Detects whether point is on a file (commit file or PR file), a commit header,
approval section, activity event, or other elements and calls the appropriate function.
Overlay actions (URLs, references) take priority over all other dispatch.
This is a DWIM function that handles multiple contexts."
  (interactive)
  (unless (shipit--try-overlay-action-at-point)
  (let ((commit-sha (get-text-property (point) 'shipit-commit-sha))
        (file-path (get-text-property (point) 'shipit-file-path))
        (is-commit-header (get-text-property (point) 'shipit-commit))
        (is-activity-event (get-text-property (point) 'shipit-activity-event))
        (is-approval-section (and (fboundp 'magit-current-section)
                                  (magit-section-match '(approval) (magit-current-section))
                                  (not (get-text-property (point) 'shipit-comment)))))
    (cond
     ;; On a file (has file-path property)
     (file-path
      (if commit-sha
          ;; Commit file - open at commit revision
          (shipit--open-commit-file-at-point)
        ;; PR file - use worktree-aware logic
        (shipit--open-pr-file-at-point)))
     ;; On a commit header (has shipit-commit property but no file-path)
     ((and is-commit-header commit-sha)
      (shipit--open-commit-revision commit-sha))
     ;; On activity event - invoke activity event actions
     (is-activity-event
      (shipit--activity-event-actions))
     ;; On approval section - invoke approval actions
     (is-approval-section
      (shipit--approval-dwim-actions))
     ;; On the linked-issue section - dispatch through DWIM
     ((get-text-property (point) 'shipit-pr-linked-issue)
      (shipit-dwim))
     ;; On a CI section - expand to show steps/logs, or open actions buffer
     ((and (fboundp 'magit-current-section)
           (run-hook-with-args-until-success
            'shipit-buffer-section-expand-functions
            (magit-current-section)))
      nil)
     ;; On a workflow/workflow-run check section heading - open actions buffer
     ((shipit--try-open-check-workflow)
      nil)
     ;; Fall back to magit's default behavior
     (t
      (if (fboundp 'magit-visit-thing)
          (call-interactively #'magit-visit-thing)
        (user-error "Nothing actionable at point")))))))

(defun shipit--select-reaction ()
  "Select a reaction emoji interactively."
  (let* ((reactions '(("👍" . "+1")
                      ("👎" . "-1")
                      ("😄" . "laugh")
                      ("🎉" . "hooray")
                      ("😕" . "confused")
                      ("❤️" . "heart")
                      ("🚀" . "rocket")
                      ("👀" . "eyes")))
         (choices (mapcar (lambda (pair) (format "%s %s" (car pair) (cdr pair))) reactions))
         (selected (completing-read "Select reaction: " choices nil t)))
    (when selected
      (let ((reaction-data (cl-find-if (lambda (pair)
                                         (string-prefix-p (car pair) selected))
                                       reactions)))
        (when reaction-data
          (cdr reaction-data))))))

(defun shipit--manage-labels-interactive (pr-number repo _current-labels)
  "Interactively manage labels for PR using a proper toggle interface."
  ;; Offensive programming - validate all inputs
  (unless pr-number
    (error "pr-number is required but was nil"))
  (unless (and (numberp pr-number) (> pr-number 0))
    (error "pr-number must be a positive number, got: %S" pr-number))
  (unless repo
    (error "repo is required but was nil"))
  (unless (stringp repo)
    (error "repo must be a string, got: %S" repo))
  (unless (string-match-p "^[^/]+/[^/]+$" repo)
    (error "repo must be in format 'owner/name', got: %S" repo))

  (let* ((available-labels (shipit--get-available-labels repo))
         ;; Always fetch fresh current labels instead of relying on cached data
         (fresh-labels-data (shipit--get-labels-only-data pr-number repo))
         (fresh-current-labels (cdr (assq 'labels fresh-labels-data)))
         (_ (shipit--debug-log "LABEL-FETCH: fresh-labels-data type=%s, labels=%S"
                               (type-of fresh-labels-data)
                               fresh-current-labels))
         (current-label-names (mapcar (lambda (label)
                                        (unless (and (listp label) (consp label))
                                          (error "Expected label to be an alist, got: %S" label))
                                        (let ((name (cdr (assq 'name label))))
                                          (unless (stringp name)
                                            (error "Expected label name to be string, got: %S" name))
                                          name))
                                      (or fresh-current-labels '()))))
    (if available-labels
        (let* (;; Create label choices with • for current, space for others
               (all-label-choices (mapcar (lambda (label)
                                            (unless (and (listp label) (consp label))
                                              (error "Each available label must be an alist, got: %S" label))
                                            (let* ((name (cdr (assq 'name label))))
                                              (unless (stringp name)
                                                (error "Label name must be string, got: %S" name))
                                              (let ((is-current (member name current-label-names)))
                                                (cons (format "%s%s"
                                                              (if is-current "• " "  ")
                                                              name)
                                                      name))))
                                          available-labels))
               ;; Sort with current labels (•) first, then others alphabetically
               (label-choices (sort all-label-choices
                                    (lambda (a b)
                                      (let ((choice-a (car a))
                                            (choice-b (car b)))
                                        (cond
                                         ;; Current labels (•) come first
                                         ((and (string-prefix-p "•" choice-a) (not (string-prefix-p "•" choice-b))) t)
                                         ((and (string-prefix-p "•" choice-b) (not (string-prefix-p "•" choice-a))) nil)
                                         ;; Within same group, sort alphabetically
                                         (t (string< choice-a choice-b)))))))
               ;; Use simple list of strings for completion
               (choices-list (mapcar 'car label-choices))
               ;; Let user select labels to toggle using custom completion that preserves order
               (selected-choices (completing-read-multiple
                                  "Toggle labels (•=current, select to toggle on/off): "
                                  ;; Custom completion table that forces our order
                                  (lambda (string pred action)
                                    (cond
                                     ;; Return metadata to disable sorting
                                     ((eq action 'metadata)
                                      '(metadata (display-sort-function . identity)
                                                 (cycle-sort-function . identity)))
                                     ;; Handle completion
                                     (t (complete-with-action action choices-list string pred))))
                                  nil nil ""))
               ;; Extract label names from choices
               (selected-names (mapcar (lambda (choice)
                                         (if (string-match "^\\(?:• \\|  \\)\\(.+\\)$" choice)
                                             (match-string 1 choice)
                                           choice))
                                       selected-choices))
               ;; Calculate final label set by toggling
               (final-labels (let ((result (copy-sequence current-label-names)))
                               (shipit--debug-log "LABEL-TOGGLE: Current labels: %S" current-label-names)
                               (shipit--debug-log "LABEL-TOGGLE: Selected to toggle: %S" selected-names)
                               (dolist (label selected-names)
                                 (if (member label result)
                                     (progn
                                       (shipit--debug-log "LABEL-TOGGLE: Removing label: %s" label)
                                       (setq result (delete label result)))
                                   (progn
                                     (shipit--debug-log "LABEL-TOGGLE: Adding label: %s" label)
                                     (push label result))))
                               (shipit--debug-log "LABEL-TOGGLE: Final labels: %S" result)
                               result)))
          ;; Update labels and refresh
          (shipit--debug-log "LABEL-UPDATE: Sending final labels to API: %S" final-labels)
          (shipit--update-pr-labels pr-number repo final-labels)
          (message "Toggled labels from [%s] to [%s]"
                   (if current-label-names
                       (mapconcat 'identity current-label-names ", ")
                     "none")
                   (if final-labels
                       (mapconcat 'identity final-labels ", ")
                     "none")))
      ;; Note: Targeted refresh is handled by shipit--update-pr-labels
      (message "No labels available for this repository"))))











(defun shipit--get-available-labels (repo)
  "Get available labels for REPO.
Dispatches to the active PR backend's :fetch-available-labels."
  ;; Offensive programming - validate inputs
  (unless repo
    (error "repo parameter is required but was nil"))
  (unless (stringp repo)
    (error "repo must be a string, got: %S" repo))
  (condition-case err
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (fetch-fn (plist-get backend :fetch-available-labels)))
        (if fetch-fn
            (funcall fetch-fn config)
          '()))
    (error
     (message "ERROR fetching labels: %s" (error-message-string err))
     '())))


(defun shipit--update-pr-labels (pr-number repo selected-labels)
  "Update PR labels with SELECTED-LABELS and refresh display asynchronously.
Uses targeted section refresh to update only the labels section."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (set-labels-fn (plist-get backend :set-labels))
         (start-time (float-time))
         (target-buffer (current-buffer)))
    (shipit--debug-log "LABEL-UPDATE: Starting label update for PR %s in buffer %s" pr-number (buffer-name target-buffer))
    (run-at-time 0 nil
                 (lambda ()
                   (let ((response-data (when set-labels-fn
                                          (funcall set-labels-fn config pr-number selected-labels))))
                     (let ((api-time (- (float-time) start-time)))
                       (if response-data
                           (progn
                             (shipit--debug-log "LABEL-UPDATE: API call succeeded in %.3f seconds" api-time)
                             ;; Invalidate backend-specific PR cache so refresh gets fresh data
                             (let ((invalidate-fn (plist-get backend :invalidate-pr-cache)))
                               (when invalidate-fn
                                 (funcall invalidate-fn config pr-number)))
                             (when (buffer-live-p target-buffer)
                               (with-current-buffer target-buffer
                                 ;; Update labels in the cache
                                 (let* ((branch (magit-get-current-branch))
                                        (cache-key (format "%s:%s" repo branch))
                                        (new-labels (if (and (listp response-data)
                                                            (cdr (assq 'labels response-data)))
                                                       (cdr (assq 'labels response-data))
                                                     ;; set-labels API returns array directly
                                                     (append response-data nil))))
                                   (when (and (boundp 'shipit--cached-branch-prs)
                                              (hash-table-p shipit--cached-branch-prs))
                                     (let ((cached-pr (gethash cache-key shipit--cached-branch-prs)))
                                       (when cached-pr
                                         (shipit--debug-log "LABEL-UPDATE: Updating labels in cached PR data")
                                         (setf (cdr (assq 'labels cached-pr)) new-labels)
                                         (puthash cache-key cached-pr shipit--cached-branch-prs))))
                                   ;; Use targeted refresh instead of full buffer refresh
                                   (run-at-time 0 nil
                                                (lambda ()
                                                  (when (buffer-live-p target-buffer)
                                                    (with-current-buffer target-buffer
                                                      (shipit--refresh-labels-section-targeted new-labels pr-number repo))))))))
                             (message "Labels updated"))
                         (progn
                           (shipit--debug-log "LABEL-UPDATE: API call failed after %.3f seconds" api-time)
                           (message "Error updating labels - API call failed")))))))))


(defun shipit--refresh-labels-section-targeted (new-labels pr-number repo)
  "Refresh only the labels section with NEW-LABELS data.
PR-NUMBER and REPO provide context for text properties."
  (shipit--refresh-section-targeted
   'labels
   "Labels (\\([0-9]+\\))"
   (length new-labels)
   (lambda (section)
     (if (= (length new-labels) 0)
         (let ((start (point)))
           (insert "   No labels\n")
           (add-text-properties start (point)
                                `(shipit-labels t
                                  shipit-pr-labels nil
                                  shipit-pr-number ,pr-number
                                  shipit-repo ,repo
                                  magit-section ,section)))
       (dolist (label new-labels)
         ;; Insert each label as a child section
         (magit-insert-section (label label)
           (let ((start (point)))
             (insert "   ")
             (insert (shipit--format-label label))
             (insert "\n")
             (add-text-properties start (1- (point))
                                  `(shipit-labels t
                                    shipit-pr-labels ,new-labels
                                    shipit-pr-number ,pr-number
                                    shipit-repo ,repo
                                    help-echo "RET: manage labels")))))))))

(defun shipit--get-labels-only-data (pr-number repo)
  "Get ONLY labels data for a specific PR number - ultra minimal fetch.
Dispatches to the active PR backend's :fetch-labels."
  (let ((labels-start (float-time)))
    (shipit--debug-log "LABELS-ONLY: Fetching only labels for PR #%s" pr-number)
    (condition-case err
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (fetch-fn (plist-get backend :fetch-labels))
               (result (funcall fetch-fn config pr-number)))
          (let ((labels-time (- (float-time) labels-start)))
            (shipit--debug-log "LABELS-ONLY: Got labels in %.3f seconds" labels-time))
          result)
      (error
       (shipit--debug-log "LABELS-ONLY: Error fetching labels: %s" (error-message-string err))
       nil))))

(defun shipit--check-and-fix-section-boundaries ()
  "Check for section boundary issues and fix them if needed.
This can be called interactively if TAB collapse is not working correctly."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((boundary-issues-found nil)
          (corruption-issues-found nil)
          (all-sections '()))

      ;; First, collect ALL sections to check for overlaps and conflicts
      (goto-char (point-min))
      (while (re-search-forward "^[ ]*\\([📋🏷👥👤💬💭🔧⚪❌🟡✅🔵⚫⚠📝📁🌟]\\)" nil t)
        (let* ((section-start (line-beginning-position))
               (section-type (match-string 1))
               (section-obj (get-text-property section-start 'magit-section))
               (section-end (when section-obj
                             (condition-case nil (oref section-obj end) (error nil))))
               (content-start (save-excursion (forward-line 1) (point)))
               (expected-end (save-excursion
                              (goto-char content-start)
                              (if (re-search-forward "^[ ]*[📋🏷👥👤💬💭🔧⚪❌🟡✅🔵⚫⚠📝📁🌟]" nil t)
                                  (match-beginning 0)
                                (point-max)))))
          (push (list :type section-type
                     :start section-start
                     :content-start content-start
                     :expected-end expected-end
                     :actual-end section-end
                     :section-obj section-obj) all-sections)))

      (setq all-sections (nreverse all-sections))
      (shipit--debug-log "BOUNDARY-CHECK: Found %d sections total" (length all-sections))

      ;; Check for section overlaps and conflicts
      (let ((prev-section nil))
        (dolist (section all-sections)
          (when prev-section
            (let ((prev-end (plist-get prev-section :expected-end))
                  (curr-start (plist-get section :start)))
              (when (> prev-end curr-start)
                (setq corruption-issues-found t)
                (message "shipit: CORRUPTION DETECTED - Section overlap: %s ends at %d, %s starts at %d"
                        (plist-get prev-section :type) prev-end
                        (plist-get section :type) curr-start))))
          (setq prev-section section)))

      ;; Now examine labels sections specifically
      (goto-char (point-min))
      (while (re-search-forward "🏷 Labels" nil t)
        (let ((labels-start (line-beginning-position))
              (labels-content-start (save-excursion (forward-line 1) (point))))

          ;; Check for boundary uncertainty flags
          (when (get-text-property labels-content-start 'shipit-boundary-uncertain)
            (setq boundary-issues-found t)
            (shipit--debug-log "BOUNDARY-CHECK: Found labels section with boundary uncertainty"))

          ;; Check for deeper corruption issues
          (let* ((labels-section (get-text-property labels-start 'magit-section))
                 (content-has-magit-property (get-text-property labels-content-start 'magit-section))
                 (expected-end (save-excursion
                                (goto-char labels-content-start)
                                (if (re-search-forward "^[ ]*[📋👥👤💬💭🔧⚪❌🟡✅🔵⚫⚠📝📁🌟]" nil t)
                                    (match-beginning 0)
                                  (point-max))))
                 (actual-end (when labels-section
                              (condition-case nil (oref labels-section end) (error nil))))
                 (content-region (buffer-substring labels-content-start expected-end)))

            (shipit--debug-log "BOUNDARY-CHECK: Analyzing labels section:")
            (shipit--debug-log "  - Labels start: %d" labels-start)
            (shipit--debug-log "  - Content start: %d" labels-content-start)
            (shipit--debug-log "  - Expected end: %d" expected-end)
            (shipit--debug-log "  - Section object: %s" (if labels-section "present" "MISSING"))
            (shipit--debug-log "  - Actual section end: %s" (if actual-end actual-end "UNKNOWN"))
            (shipit--debug-log "  - Content has magit-section property: %s" (if content-has-magit-property "yes" "NO"))
            (shipit--debug-log "  - Content preview: %s" (replace-regexp-in-string "\n" "\\\\n" (substring content-region 0 (min 50 (length content-region)))))

            ;; Detect various types of corruption
            (cond
             ;; Missing magit-section object entirely
             ((not labels-section)
              (setq corruption-issues-found t)
              (message "shipit: CORRUPTION DETECTED - Missing magit-section object for labels"))

             ;; Content missing magit-section text property
             ((not content-has-magit-property)
              (setq corruption-issues-found t)
              (message "shipit: CORRUPTION DETECTED - Labels content missing magit-section text property"))

             ;; Section end boundary is wrong
             ((and actual-end (/= actual-end expected-end))
              (setq corruption-issues-found t)
              (message "shipit: CORRUPTION DETECTED - Section end boundary mismatch: expected %d, actual %d" expected-end actual-end))

             ;; Section object is corrupted (can't read end property)
             ((not actual-end)
              (setq corruption-issues-found t)
              (message "shipit: CORRUPTION DETECTED - Cannot read section end boundary"))

             (t
              (shipit--debug-log "  - Section appears healthy")))

            ;; Try to fix any corruption found
            (when (or boundary-issues-found corruption-issues-found labels-section)
              (condition-case err
                  (progn
                    ;; Update the section boundary if we have a section object
                    (when labels-section
                      (setf (oref labels-section end) expected-end))
                    ;; Ensure magit-section property covers the entire content
                    (put-text-property labels-content-start expected-end 'magit-section labels-section)
                    ;; Remove any uncertainty flags
                    (remove-text-properties labels-content-start expected-end '(shipit-boundary-uncertain))
                    (shipit--debug-log "BOUNDARY-CHECK: Applied fixes to labels section"))
                (error
                 (shipit--debug-log "BOUNDARY-CHECK: Failed to fix boundaries: %s" (error-message-string err))))))))

      (cond
       ((or boundary-issues-found corruption-issues-found)
        (message "shipit: Found and fixed section corruption issues. TAB collapse should work correctly now.")
        (when (fboundp 'magit-section-update-highlight)
          (magit-section-update-highlight)))
       (t
        (message "shipit: No section boundary issues detected."))))))

(defun shipit--verify-section-integrity ()
  "Lightweight verification of section boundaries after section operations.
This should be called after expanding/collapsing any sections."
  (when (shipit--in-shipit-context-p)  ; Only run in shipit context
    (save-excursion
      (goto-char (point-min))
      (let ((integrity-issues nil))
        ;; Quick check: find labels sections and verify their boundaries
        (while (re-search-forward "🏷 Labels" nil t)
          (let* ((labels-start (line-beginning-position))
                 (labels-content-start (save-excursion (forward-line 1) (point)))
                 (labels-section (get-text-property labels-start 'magit-section))
                 (expected-end (save-excursion
                                 (goto-char labels-content-start)
                                 (if (re-search-forward "^[ ]*[📋👥👤💬💭🔧⚪❌🟡✅🔵⚫⚠📝📁🌟]" nil t)
                                     (match-beginning 0)
                                   (point-max))))
                 (actual-end (when labels-section
                               (condition-case nil (oref labels-section end) (error nil)))))

            (when (and actual-end (/= actual-end expected-end))
              (push (list :start labels-start
                         :expected expected-end
                         :actual actual-end) integrity-issues)
              (shipit--debug-log "INTEGRITY-CHECK: Labels section boundary mismatch detected: expected %d, actual %d" expected-end actual-end)

              ;; Log detailed corruption context to help find root cause
              (shipit--debug-log "CORRUPTION-CONTEXT: Command: %s" (or this-command 'unknown))
              (shipit--debug-log "CORRUPTION-CONTEXT: Last command: %s" (or last-command 'unknown))
              (shipit--debug-log "CORRUPTION-CONTEXT: Stack trace:")
              (let ((stack-trace (with-output-to-string
                                  (backtrace))))
                ;; Log first few lines of stack trace to see what's calling this
                (dolist (line (cl-subseq (split-string stack-trace "\n") 0 8))
                  (when (string-match "\\S-" line)
                    (shipit--debug-log "CORRUPTION-CONTEXT:   %s" line))))

              ;; Auto-fix the boundary
              (condition-case err
                  (progn
                    (let ((old-end (condition-case nil (oref labels-section end) (error nil))))
                      (setf (oref labels-section end) expected-end)
                      (shipit--log-boundary-change labels-section old-end expected-end "INTEGRITY-AUTO-FIX"))
                    (put-text-property labels-content-start expected-end 'magit-section labels-section)
                    (shipit--debug-log "INTEGRITY-CHECK: Auto-fixed labels section boundary"))
                (error
                 (shipit--debug-log "INTEGRITY-CHECK: Failed to auto-fix: %s" (error-message-string err)))))))

        (when integrity-issues
          (shipit--debug-log "INTEGRITY-CHECK: Fixed %d section boundary issues" (length integrity-issues)))))))

(defun shipit--add-section-integrity-hooks ()
  "Add hooks to verify section integrity after magit operations.
This helps prevent section boundary corruption from expanding/collapsing sections."
  (when (featurep 'magit-section)
    ;; Add hook after section visibility changes
    (add-hook 'magit-section-visibility-hook #'shipit--verify-section-integrity)
    ;; Add hook after buffer updates that might affect sections
    (add-hook 'post-command-hook #'shipit--periodic-integrity-check nil t)
    (shipit--debug-log "INTEGRITY: Added section integrity verification hooks")))

(defvar shipit--last-integrity-check-time 0
  "Last time we ran a periodic integrity check.")

(defun shipit--periodic-integrity-check ()
  "Periodic integrity check that runs after commands but with rate limiting."
  (when (and (shipit--in-shipit-context-p)  ; Only run in shipit context
             (> (float-time) (+ shipit--last-integrity-check-time 2.0))) ; Max once per 2 seconds
    (setq shipit--last-integrity-check-time (float-time))
    (condition-case err
        (shipit--verify-section-integrity)
      (error
       (shipit--debug-log "PERIODIC-INTEGRITY: Error during check: %s" (error-message-string err))))))

(defun shipit-enable-aggressive-integrity-monitoring ()
  "Enable aggressive integrity monitoring for debugging frequent corruption.
This checks section boundaries after every command and logs issues."
  (interactive)
  (setq shipit--last-integrity-check-time 0) ; Reset rate limiting
  (remove-hook 'post-command-hook #'shipit--periodic-integrity-check t)
  (add-hook 'post-command-hook
            (lambda ()
              (condition-case err
                  (shipit--verify-section-integrity)
                (error
                 (message "shipit: Integrity check error: %s" (error-message-string err)))))
            nil t)
  (message "shipit: Enabled aggressive integrity monitoring. Check debug log for corruption detection.")
  (shipit--debug-log "AGGRESSIVE-MONITORING: Enabled - will check integrity after every command"))

(defun shipit-disable-aggressive-integrity-monitoring ()
  "Disable aggressive integrity monitoring and return to normal periodic checks."
  (interactive)
  (remove-hook 'post-command-hook #'shipit--periodic-integrity-check t)
  (shipit--add-section-integrity-hooks) ; Re-add normal hooks
  (message "shipit: Disabled aggressive integrity monitoring, returned to normal periodic checks.")
  (shipit--debug-log "AGGRESSIVE-MONITORING: Disabled - returned to normal periodic checks"))

(defvar shipit--section-boundary-log '()
  "Log of section boundary changes to track corruption sources.")

(defun shipit--log-boundary-change (section-obj old-end new-end context)
  "Log when a section boundary changes to help track corruption sources."
  (let ((entry (list :time (float-time)
                    :section section-obj
                    :old-end old-end
                    :new-end new-end
                    :context context
                    :command (or this-command 'unknown)
                    :last-command (or last-command 'unknown))))
    (push entry shipit--section-boundary-log)
    ;; Keep only last 20 entries
    (when (> (length shipit--section-boundary-log) 20)
      (setq shipit--section-boundary-log (cl-subseq shipit--section-boundary-log 0 20)))
    (shipit--debug-log "BOUNDARY-CHANGE: %s changed section end from %d to %d (cmd: %s)"
                      context old-end new-end (or this-command 'unknown))))

(defun shipit-show-boundary-change-log ()
  "Show recent section boundary changes to help debug corruption."
  (interactive)
  (if shipit--section-boundary-log
      (progn
        (with-current-buffer (get-buffer-create "*shipit-boundary-log*")
          (erase-buffer)
          (insert "Recent Section Boundary Changes:\n")
          (insert "=================================\n\n")
          (dolist (entry shipit--section-boundary-log)
            (insert (format "[%s] %s: %d -> %d (cmd: %s, last: %s)\n"
                           (format-time-string "%H:%M:%S" (plist-get entry :time))
                           (plist-get entry :context)
                           (plist-get entry :old-end)
                           (plist-get entry :new-end)
                           (plist-get entry :command)
                           (plist-get entry :last-command))))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
        (message "shipit: Boundary change log displayed"))
    (message "shipit: No boundary changes recorded yet")))

;; Auto-add integrity hooks when shipit-diff is loaded
(add-hook 'magit-mode-hook #'shipit--add-section-integrity-hooks)

(provide 'shipit-diff)
;;; shipit-diff.el ends here
