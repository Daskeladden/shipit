;;; shipit-review-mode.el --- Minor mode for interactive PR review in worktrees -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-render)
(require 'shipit-review-side-frame)
(require 'vc-git)
(require 'transient nil t)

(defgroup shipit-review nil
  "Shipit worktree review mode for interactive PR exploration."
  :group 'shipit)

(defcustom shipit-review-side-frame-position 'right
  "Position of the side-frame showing comments.
Can be \\='right, \\='left, \\='top, or \\='bottom."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'shipit-review)

(defcustom shipit-review-side-frame-width 50
  "Width of side-frame when positioned left or right (in characters)."
  :type 'integer
  :group 'shipit-review)

;; Buffer-local variables for review context
(defvar-local shipit-review--base-sha nil
  "SHA of the base branch for diff comparison in review mode.")

(defvar-local shipit-review--head-sha nil
  "SHA of the PR head for commit navigation in review mode.")

(defvar-local shipit-review--pr-data nil
  "Full PR data object for the current review session.")

(defvar-local shipit-review--comparison-target nil
  "Current comparison target SHA. If nil, defaults to base-sha.")

(defvar-local shipit-review--current-commit-index nil
  "Current commit index when navigating through PR commits (1-indexed).")

(defvar-local shipit-review--total-commits nil
  "Total number of commits in PR.")

(defvar-local shipit-review--changed-lines nil
  "List of line ranges that were changed in the diff. Format: ((added-start . added-end) (removed-start . removed-end) ...)")

(defvar-local shipit-review--mode-line-string ""
  "Current mode line string. Updated dynamically when comparison target changes.")

(defvar-local shipit-review--current-commit-index nil
  "Current commit being viewed (0-based index). nil means showing range.")

(defvar-local shipit-review--range-start-index nil
  "Start of current range (0-based index). nil means showing from base.")

(defvar-local shipit-review--range-end-index nil
  "End of current range (0-based index). nil means showing to HEAD.")

(defvar-local shipit-review--displayed-sha nil
  "SHA of the current commit being displayed in the file buffer.")


(defvar shipit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r c") #'shipit-review-compare-menu)
    map)
  "Keymap for shipit-review-mode.")

;; Transient menu for comparison target
(when (fboundp 'transient-define-prefix)
  (transient-define-prefix shipit-review-compare-menu ()
    "Menu to adjust diff comparison target in review mode."
    [["Comparison Target"
      ("b" "Compare vs Base" shipit-review--compare-vs-base)
      ("h" "Compare vs HEAD" shipit-review--compare-vs-head)
      ("p" "Compare vs Previous" shipit-review--compare-vs-previous)]
     ["Advanced"
      ("c" "Custom SHA..." shipit-review--compare-vs-custom)
      ("r" "Reset to Base" shipit-review--reset-comparison)]]))

;;;###autoload
(define-minor-mode shipit-review-mode
  "Minor mode for interactive PR review in worktrees.

Provides:
- Revision navigation (commit-by-commit through PR history)
- Comment display in side-frame synced to cursor position
- Visual change indicators via diff-hl integration
- Full Emacs integration (LSP, linting, tests, Magit)

When enabled on a file opened from a PR worktree:
- diff-hl shows changed lines
- Side-frame displays comments for current line
- Transient menu provides revision navigation
- Buffer remains editable for exploring changes

\\{shipit-review-mode-map}"
  :lighter (:eval shipit-review--mode-line-string)
  :keymap shipit-review-mode-map
  (if shipit-review-mode
      (shipit-review-mode--setup)
    (shipit-review-mode--teardown)))

(defun shipit-review--mode-line-string ()
  "Generate mode line string showing the current diff range with graphical indicator."
  (if (and shipit-review--base-sha shipit-review--head-sha)
      (let* ((target (or shipit-review--comparison-target shipit-review--base-sha))
             (from-short (if target (substring target 0 7) "unknown"))
             (to-short (if shipit-review--head-sha (substring shipit-review--head-sha 0 7) "HEAD")))
        (format " Review[%s..%s]" from-short to-short))
    " Review"))

(defun shipit-review--update-buffer-mode-line ()
  "Update the buffer's mode line to show the current file SHA."
  (let ((commits (cdr (assq 'commits shipit-review--pr-data)))
        (displayed-sha nil))
    (cond
     ;; Single-commit mode: show that commit's SHA
     (shipit-review--current-commit-index
      (let ((commit (nth shipit-review--current-commit-index commits)))
        (setq displayed-sha (cdr (assq 'sha commit)))))
     ;; Range mode: show the "to" SHA of the range
     ((or shipit-review--range-start-index shipit-review--range-end-index)
      (let ((end-idx (or shipit-review--range-end-index (1- (length commits)))))
        (let ((commit (nth end-idx commits)))
          (setq displayed-sha (cdr (assq 'sha commit))))))
     ;; Default: show head SHA
     (t (setq displayed-sha shipit-review--head-sha)))
    (setq-local shipit-review--displayed-sha displayed-sha)
    (force-mode-line-update)))

(defun shipit-review--set-comparison-target (target-sha description)
  "Change the comparison target to TARGET-SHA and update overlays.
TARGET-SHA should be a git SHA. DESCRIPTION is for logging."
  (setq-local shipit-review--comparison-target target-sha)
  (shipit--debug-log "Changed comparison target to: %s (%s)"
                     (if target-sha (substring target-sha 0 7) "base")
                     description)
  (shipit-review--update-diff-overlays)
  (shipit-review--update-side-frame-header)
  (shipit-review--update-buffer-mode-line)
  (shipit-review--update-mode-line)
  (message "Comparing: %s vs HEAD" description))

(defun shipit-review--compare-vs-base ()
  "Compare against the base branch (default)."
  (interactive)
  (shipit-review--set-comparison-target nil "base"))

(defun shipit-review--compare-vs-head ()
  "Compare against HEAD (shows no changes since you're on HEAD)."
  (interactive)
  (shipit-review--set-comparison-target shipit-review--head-sha "HEAD"))

(defun shipit-review--compare-vs-previous ()
  "Compare against the previous commit in the PR."
  (interactive)
  (if (and shipit-review--current-commit-index
           (> shipit-review--current-commit-index 1)
           shipit-review--pr-data)
      (let* ((commits (when (fboundp 'shipit--build-pr-commit-list)
                        (shipit--build-pr-commit-list shipit-review--pr-data)))
             (prev-index (- shipit-review--current-commit-index 2))
             (prev-commit (and (>= prev-index 0) (nth prev-index commits)))
             (prev-sha (and prev-commit (plist-get prev-commit :sha))))
        (if prev-sha
            (shipit-review--set-comparison-target prev-sha
                                                  (format "commit %d/%d"
                                                          (1+ prev-index)
                                                          shipit-review--total-commits))
          (message "No previous commit available")))
    (message "Not navigating commits. Use 'b' to compare against base.")))

(defun shipit-review--compare-vs-custom ()
  "Prompt user for a custom SHA to compare against."
  (interactive)
  (let ((custom-sha (read-string "Enter commit SHA to compare against: ")))
    (when (and custom-sha (not (string-empty-p custom-sha)))
      (shipit-review--set-comparison-target custom-sha
                                            (substring custom-sha 0 7)))))

(defun shipit-review--reset-comparison ()
  "Reset comparison target to base branch."
  (interactive)
  (shipit-review--compare-vs-base))

(defun shipit-review--make-jump-to-commit-fn (commit-num)
  "Create a function to jump to COMMIT-NUM (1-based)."
  (lambda ()
    (interactive)
    (when shipit-review--pr-data
      (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
             (index (1- commit-num)))
        (when (< index (length commits))
          (let ((commit (nth index commits)))
            (shipit-review--view-single-commit index commit)))))))

(defun shipit-review--view-single-commit (index commit)
  "View diff for a single COMMIT at INDEX (0-based)."
  (setq-local shipit-review--current-commit-index index)
  (setq-local shipit-review--range-start-index nil)
  (setq-local shipit-review--range-end-index nil)
  (let ((sha (cdr (assq 'sha commit))))
    (shipit-review--set-comparison-target sha (format "commit %d" (1+ index)))))

(defun shipit-review--prev-commit ()
  "Go to previous commit."
  (interactive)
  (when shipit-review--pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
           (total (length commits))
           (current-idx (or shipit-review--current-commit-index
                           (or shipit-review--range-end-index (1- total)))))
      (when (> current-idx 0)
        (let ((prev-idx (1- current-idx)))
          (shipit-review--view-single-commit prev-idx (nth prev-idx commits)))))))

(defun shipit-review--next-commit ()
  "Go to next commit."
  (interactive)
  (when shipit-review--pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
           (total (length commits))
           (current-idx (or shipit-review--current-commit-index
                           (or shipit-review--range-end-index (1- total)))))
      (when (< current-idx (1- total))
        (let ((next-idx (1+ current-idx)))
          (shipit-review--view-single-commit next-idx (nth next-idx commits)))))))

(defun shipit-review--move-range-start-prev ()
  "Extend range by moving base to previous (older) commit."
  (interactive)
  (when shipit-review--pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
           ;; If in single-commit mode, initialize range from that commit
           (current-start (if (not (null shipit-review--current-commit-index))
                             shipit-review--current-commit-index
                           (or shipit-review--range-start-index 0)))
           (current-end (if (not (null shipit-review--current-commit-index))
                           shipit-review--current-commit-index
                         (or shipit-review--range-end-index (1- (length commits))))))
      (when (> current-start 0)
        (let ((new-start (1- current-start)))
          (setq-local shipit-review--range-start-index new-start)
          (setq-local shipit-review--range-end-index current-end)
          (setq-local shipit-review--current-commit-index nil)
          (shipit-review--update-diff-overlays)
          (shipit-review--update-side-frame-header)
          (shipit-review--update-buffer-mode-line)
          (message "Range extended (base moved earlier)"))))))

(defun shipit-review--move-range-end-next ()
  "Extend range by moving head to next (newer) commit."
  (interactive)
  (when shipit-review--pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
           (total (length commits))
           ;; If in single-commit mode, initialize range from that commit
           (current-start (if (not (null shipit-review--current-commit-index))
                             shipit-review--current-commit-index
                           (or shipit-review--range-start-index 0)))
           (current-end (if (not (null shipit-review--current-commit-index))
                           shipit-review--current-commit-index
                         (or shipit-review--range-end-index (1- total)))))
      (when (< current-end (1- total))
        (let ((new-end (1+ current-end)))
          (setq-local shipit-review--range-start-index current-start)
          (setq-local shipit-review--range-end-index new-end)
          (setq-local shipit-review--current-commit-index nil)
          (shipit-review--update-diff-overlays)
          (shipit-review--update-side-frame-header)
          (shipit-review--update-buffer-mode-line)
          (message "Range extended (head moved later)"))))))

(defun shipit-review--reset-to-full-range ()
  "Reset to full range (base..HEAD)."
  (interactive)
  (setq-local shipit-review--current-commit-index nil)
  (setq-local shipit-review--range-start-index nil)
  (setq-local shipit-review--range-end-index nil)
  (shipit-review--set-comparison-target nil "full range"))

(defun shipit-review--update-mode-line ()
  "Update the mode line string with current comparison information."
  (setq-local shipit-review--mode-line-string
              (if (and shipit-review--base-sha shipit-review--head-sha)
                  (let* ((target (or shipit-review--comparison-target shipit-review--base-sha))
                         (from-short (if target (substring target 0 7) "unknown"))
                         (to-short (if shipit-review--head-sha (substring shipit-review--head-sha 0 7) "HEAD")))
                    (format " Review[%s..%s]" from-short to-short))
                " Review"))
  (force-mode-line-update))

(defun shipit-review--update-side-frame-header ()
  "Update the side-frame with current review info and graphical bar."
  (shipit--debug-log "SIDE-FRAME: update-side-frame-header called, fboundp=%s pr-data=%s base=%s head=%s"
                     (fboundp 'shipit-review--display-side-frame-header)
                     (if shipit-review--pr-data "present" "nil")
                     (if shipit-review--base-sha (substring shipit-review--base-sha 0 7) "nil")
                     (if shipit-review--head-sha (substring shipit-review--head-sha 0 7) "nil"))
  (when (and (fboundp 'shipit-review--display-side-frame-header)
             (buffer-file-name))  ; Guard against nil file-path
    (let ((main-buffer (current-buffer))
          (commits (cdr (assq 'commits shipit-review--pr-data)))
          (from-sha (or shipit-review--comparison-target shipit-review--base-sha))
          (to-sha shipit-review--head-sha))
      ;; If in range mode, calculate SHAs from indices
      (when (and (not shipit-review--current-commit-index)
                 (or shipit-review--range-start-index shipit-review--range-end-index))
        (when shipit-review--range-start-index
          (setq from-sha (cdr (assq 'sha (nth shipit-review--range-start-index commits)))))
        (when shipit-review--range-end-index
          (setq to-sha (cdr (assq 'sha (nth shipit-review--range-end-index commits))))))
      (shipit-review--display-side-frame-header
       from-sha
       to-sha
       (buffer-file-name)
       shipit-review--pr-data
       main-buffer
       shipit-review--current-commit-index))))

(defun shipit-review-mode--setup ()
  "Initialize shipit review mode for current buffer.
Enables diff-hl and configures it to show PR changes (base..head)."
  (shipit--debug-log "Setting up shipit-review-mode in buffer: %s (base=%s head=%s)"
                     (buffer-name)
                     (if shipit-review--base-sha (substring shipit-review--base-sha 0 7) "unknown")
                     (if shipit-review--head-sha (substring shipit-review--head-sha 0 7) "unknown"))
  ;; Enable diff-hl for this buffer
  (when (and (fboundp 'diff-hl-mode) (not (bound-and-true-p diff-hl-mode)))
    (diff-hl-mode 1))

  ;; Update visual diff overlays to show changed lines
  (shipit-review--update-diff-overlays)

  ;; Display review info in side-frame
  (shipit-review--update-side-frame-header)

  ;; Update mode line with initial range
  (shipit-review--update-mode-line)
  (shipit-review--update-buffer-mode-line)

  (when (bound-and-true-p diff-hl-mode)
    (shipit--debug-log "Review mode setup complete - diff-hl is active")))

(defun shipit-review--parse-diff-hunk-header (header)
  "Parse a unified diff hunk header like '@@ -10,5 +15,7 @@'.
Returns (start-old count-old start-new count-new) or nil if header is invalid."
  (when (string-match "@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?" header)
    (let ((start-old (string-to-number (match-string 1 header)))
          (count-old (string-to-number (or (match-string 2 header) "1")))
          (start-new (string-to-number (match-string 3 header)))
          (count-new (string-to-number (or (match-string 4 header) "1"))))
      (list start-old count-old start-new count-new))))

(defun shipit-review--get-diff-output (from-sha to-sha file-path)
  "Get unified diff output between FROM-SHA and TO-SHA for FILE-PATH.
Returns the diff as a string."
  (condition-case err
      (shell-command-to-string
       (format "cd %s && git diff --unified=0 %s..%s -- %s 2>/dev/null"
               (shell-quote-argument (file-name-directory (or buffer-file-name default-directory)))
               (shell-quote-argument from-sha)
               (shell-quote-argument to-sha)
               (shell-quote-argument (file-name-nondirectory file-path))))
    (error
     (shipit--debug-log "Error getting diff: %s" (error-message-string err))
     "")))

(defun shipit-review--parse-diff-for-changed-lines (diff-output)
  "Parse unified diff output and extract line ranges that were changed.
Returns list of (line-number . type) where type is 'added, 'removed, or 'modified.
NOTE: We track the NEW file line numbers (what's in the worktree)."
  (let ((lines (split-string diff-output "\n"))
        (current-line-new nil)
        changed-lines)
    (shipit--debug-log "Parsing diff: %d lines" (length lines))
    (dolist (line lines)
      (cond
       ;; Hunk header: @@ -10,5 +15,7 @@
       ((string-match "^@@ -\\([0-9]+\\)" line)
        (let ((parsed (shipit-review--parse-diff-hunk-header line)))
          (when parsed
            (let ((start-new (caddr parsed)))
              (setq current-line-new start-new)))))
       ;; Line removed (only affects old file, skip)
       ((and current-line-new (string-match "^-" line))
        nil)
       ;; Line added (in the new file)
       ((and current-line-new (string-match "^\\+" line))
        (push (cons current-line-new 'added) changed-lines)
        (setq current-line-new (1+ current-line-new)))
       ;; Context line (unchanged, in both files)
       ((and current-line-new (or (string-match "^ " line)
                                  (string-match "^\\\\" line)))
        (setq current-line-new (1+ current-line-new)))
       ;; Anything else (diff headers, etc)
       (t
        nil)))
    (reverse changed-lines)))

(defun shipit-review--update-diff-overlays ()
  "Update visual overlays to mark changed lines based on current diff."
  (interactive)
  (when shipit-review--base-sha
    (condition-case err
        (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
               (from-sha (if shipit-review--current-commit-index
                           ;; For single commit view: compare against parent
                           (if (> shipit-review--current-commit-index 0)
                               (let ((parent-commit (nth (1- shipit-review--current-commit-index) commits)))
                                 (cdr (assq 'sha parent-commit)))
                             shipit-review--base-sha)
                         ;; For range view: use range start or base
                         (if shipit-review--range-start-index
                             (if (= shipit-review--range-start-index 0)
                                 ;; If starting at commit 0, compare from base
                                 shipit-review--base-sha
                               ;; Otherwise, compare from the parent commit
                               (let ((parent-commit (nth (1- shipit-review--range-start-index) commits)))
                                 (cdr (assq 'sha parent-commit))))
                           shipit-review--base-sha)))
               (to-sha (if shipit-review--current-commit-index
                          ;; For single commit view: use current commit
                          (let ((current-commit (nth shipit-review--current-commit-index commits)))
                            (cdr (assq 'sha current-commit)))
                        ;; For range view: use range end or HEAD
                        (if shipit-review--range-end-index
                            (let ((end-commit (nth shipit-review--range-end-index commits)))
                              (cdr (assq 'sha end-commit)))
                          shipit-review--head-sha)))
               (diff-output (shipit-review--get-diff-output from-sha to-sha (buffer-file-name)))
               (changed-lines (shipit-review--parse-diff-for-changed-lines diff-output)))
          ;; Store changed lines for filtering comments by range
          (setq-local shipit-review--changed-lines changed-lines)
          ;; Remove existing overlays
          (remove-overlays (point-min) (point-max) 'shipit-review-change t)
          ;; Add new overlays for each changed line
          (dolist (line-info changed-lines)
            (let ((line-num (car line-info))
                  (change-type (cdr line-info)))
              (save-excursion
                (goto-line line-num)
                (when (>= line-num 1)
                  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                    (overlay-put ov 'shipit-review-change t)
                    (overlay-put ov 'face (pcase change-type
                                            ('added 'diff-added)
                                            ('removed 'diff-removed)
                                            ('modified 'diff-changed)
                                            (_ 'default)))
                    (overlay-put ov 'help-echo (format "%s in PR diff" change-type))))))))
      (error
       (shipit--debug-log "Error updating diff overlays: %s" (error-message-string err))))))

(defun shipit-review--get-active-comments-for-file (file-path)
  "Get all active (non-outdated) comments for FILE-PATH.
First tries cached comments, then fetches from magit buffer if needed.
Returns list of comment objects, or empty list if none found."
  (let ((cached-comments (and (bound-and-true-p shipit--cached-inline-comments)
                             shipit--cached-inline-comments)))
    (shipit--debug-log "Getting comments: file-path=%s cached-comments-exist=%s cached-count=%s"
                      file-path
                      (if cached-comments t nil)
                      (if cached-comments (length cached-comments) 0))
    (if cached-comments
        ;; Use cached comments if available
        (let ((matching (seq-filter (lambda (comment)
                                      (let ((comment-path (cdr (assq 'path comment)))
                                            (is-outdated (cdr (assq 'outdated comment))))
                                        (shipit--debug-log "  Comment: path=%s outdated=%s" comment-path is-outdated)
                                        (and (equal comment-path file-path)
                                             (not is-outdated))))
                                    cached-comments)))
          (shipit--debug-log "Matching comments from cache: %s" (length matching))
          matching)
      ;; Fallback: try to get comments from shipit magit buffer
      (shipit--debug-log "No cached comments, attempting to fetch from shipit buffer")
      (shipit-review--get-comments-from-magit-buffer file-path))))

(defun shipit-review--get-comments-from-magit-buffer (file-path)
  "Try to get comments for FILE-PATH from the shipit magit buffer.
FILE-PATH may be absolute (in worktree) or relative.
Comments are stored with relative repo paths, so we normalize file-path.
Returns list of comment objects, or empty list if not found."
  (let ((shipit-buffers (seq-filter (lambda (buf)
                                      (with-current-buffer buf
                                        (and (boundp 'shipit--cached-inline-comments)
                                             shipit--cached-inline-comments)))
                                    (buffer-list)))
        ;; Extract just the repo-relative path from file-path
        ;; e.g., "/path/.worktrees/pr-123-branch/common/python/file.py" -> "common/python/file.py"
        (relative-path (if (string-match ".worktrees/[^/]+/\\(.*\\)$" file-path)
                          (match-string 1 file-path)
                        (file-name-nondirectory file-path))))
    (shipit--debug-log "Looking for comments with relative-path=%s" relative-path)
    (if shipit-buffers
        (with-current-buffer (car shipit-buffers)
          (shipit--debug-log "Searching shipit buffer: %s with %d cached comments"
                            (buffer-name)
                            (length shipit--cached-inline-comments))
          (let ((matching (seq-filter (lambda (comment)
                                        (let ((comment-path (cdr (assq 'path comment)))
                                              (is-outdated (cdr (assq 'outdated comment))))
                                          (and (equal comment-path relative-path)
                                               (not is-outdated))))
                                      shipit--cached-inline-comments)))
            (shipit--debug-log "Found %d matching comments in shipit buffer for file" (length matching))
            matching))
      (progn
        (shipit--debug-log "No shipit buffer found with cached comments")
        nil))))

(defun shipit-review--filter-comments-by-range (comments)
  "Filter COMMENTS to only those on lines in the current diff range.
If no changed lines tracked, return all comments."
  (let ((changed-lines (and (bound-and-true-p shipit-review--changed-lines)
                           shipit-review--changed-lines)))
    (shipit-review--filter-comments-by-range-with-list comments changed-lines)))

(defun shipit-review--filter-comments-by-range-with-list (comments changed-lines)
  "Filter COMMENTS to only those on lines in CHANGED-LINES list.
If CHANGED-LINES is nil, return all comments."
  (shipit--debug-log "Filter: comments=%d changed-lines=%s"
                    (length comments)
                    (if changed-lines (length changed-lines) nil))
  (if (not changed-lines)
      (progn
        (shipit--debug-log "  -> No changed lines, returning all %d comments" (length comments))
        comments)
    (let ((filtered (seq-filter (lambda (c)
                                  (let ((line (cdr (assq 'line c))))
                                    (and line
                                         (seq-some (lambda (cl) (= line (car cl)))
                                                  changed-lines))))
                                comments)))
      (shipit--debug-log "  -> Filtered to %d comments in changed lines" (length filtered))
      filtered)))

(defun shipit-review--group-comments-by-line (comments)
  "Group COMMENTS by line number.
Returns alist: ((line-number . (comment1 comment2 ...)) ...)"
  (let ((grouped (make-hash-table :test 'equal)))
    (dolist (comment comments)
      (let ((line (cdr (assq 'line comment))))
        (when line
          (puthash line
                   (append (gethash line grouped '()) (list comment))
                   grouped))))
    ;; Convert hash table to sorted alist using maphash
    (let ((result '()))
      (maphash (lambda (line comments-list)
                 (push (cons line comments-list) result))
               grouped)
      (sort result (lambda (a b) (< (car a) (car b)))))))

(defun shipit-review--format-comments-for-side-frame (file-path &optional changed-lines)
  "Format active comments for FILE-PATH in current diff range.
CHANGED-LINES is optional list of changed line numbers from the main buffer."
  (let ((all-comments (shipit-review--get-active-comments-for-file file-path)))
    (if (seq-empty-p all-comments)
        "  (no active comments)"
      (let ((filtered (if changed-lines
                         (shipit-review--filter-comments-by-range-with-list all-comments changed-lines)
                       (shipit-review--filter-comments-by-range all-comments))))
        (if (seq-empty-p filtered)
            "  (no comments in current range)"
          (shipit-review--render-comments-list filtered))))))

(defun shipit-review--render-comments-list (comments)
  "Render COMMENTS as formatted text for side-frame."
  (let ((grouped (shipit-review--group-comments-by-line comments))
        (result ""))
    (dolist (group grouped)
      (let ((line-num (car group))
            (line-comments (cdr group)))
        (setq result (concat result (format "\nLine %d:\n" line-num)))
        (dolist (comment line-comments)
          (let ((header (if (fboundp 'shipit--render-comment-header)
                           (shipit--render-comment-header comment 0 'inline)
                         ""))
                (body (if (fboundp 'shipit--render-comment-body)
                         (shipit--render-comment-body comment 2)
                       (or (cdr (assq 'body comment)) ""))))
            (setq result (concat result
                                "  " header "\n"
                                "  " body "\n"))))))
    result))

(defun shipit-review-mode--teardown ()
  "Cleanup shipit review mode when disabled."
  (shipit--debug-log "Tearing down shipit-review-mode from buffer: %s" (buffer-name))
  ;; Remove all review-related overlays
  (remove-overlays (point-min) (point-max) 'shipit-review-change t))

(provide 'shipit-review-mode)
;;; shipit-review-mode.el ends here
