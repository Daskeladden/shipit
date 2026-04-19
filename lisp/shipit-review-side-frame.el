;;; shipit-review-side-frame.el --- Side-frame for comment display in review mode -*- lexical-binding: t; -*-

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

;; Forward declarations
(defvar shipit-review--base-sha)
(defvar shipit-review--head-sha)
(defvar shipit-review--pr-data)
(defvar shipit-review--current-commit-index)
(defvar shipit-review--range-start-index)
(defvar shipit-review--range-end-index)
(defvar shipit--cached-inline-comments)
(declare-function shipit--insert-file-diff "shipit-pr-sections")
(declare-function shipit--debug-log "shipit-core")

(defvar shipit--review-side-frame-buffer nil
  "Buffer for displaying comments in review mode.")

(defvar shipit--review-side-frame-window nil
  "Window displaying the side-frame.")

(defcustom shipit-review-side-frame-position 'right
  "Position of the side-frame: 'right or 'left."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left))
  :group 'shipit-review)

(defcustom shipit-review-side-frame-width 50
  "Width of the side-frame in characters."
  :type 'integer
  :group 'shipit-review)

(defvar-local shipit-review--side-frame-pr-data nil
  "PR data stored in side-frame for interactive operations.")

(defvar-local shipit-review--side-frame-main-buffer nil
  "Reference to main file buffer for updating comparison targets.")

(defvar shipit-review-side-frame-bar-map
  (let ((map (make-sparse-keymap)))
    ;; These will be set after functions are defined
    map)
  "Keymap for interactive bar in side-frame.")

(defvar shipit-review-side-frame-mode-map
  (let ((map (make-sparse-keymap)))
    ;; These will be set after functions are defined
    map)
  "Keymap for side-frame navigation.")

(define-derived-mode shipit-review-side-frame-mode fundamental-mode "ShipitReview"
  "Major mode for shipit review side-frame.
Provides navigation and interaction with PR diffs."
  (setq buffer-read-only nil)
  (setq truncate-lines t))

(defun shipit--create-review-side-frame-buffer (file-path)
  "Create a new buffer for displaying comments in review mode.
FILE-PATH is used to create a unique buffer name."
  (let* ((safe-name (string-replace "/" "-" (string-replace "." "-" file-path)))
         (buffer-name (format "*shipit-review-comments-%s*" safe-name)))
    (get-buffer-create buffer-name)))

(defun shipit--format-side-frame-header (commit-sha is-pr-commit &optional current-index total-count)
  "Format header for side-frame showing commit context.

COMMIT-SHA: Full git SHA
IS-PR-COMMIT: Whether this commit is part of the PR
CURRENT-INDEX: Current position in PR commits (if applicable)
TOTAL-COUNT: Total PR commits (if applicable)

Returns formatted header string."
  (let ((short-sha (substring commit-sha 0 7)))
    (if is-pr-commit
        (if (and current-index total-count)
            (format "Comments for commit %s (PR commit %d/%d)" short-sha (1+ current-index) total-count)
          (format "Comments for commit %s (PR commit)" short-sha))
      (format "Comments for commit %s (before PR)" short-sha))))

(defun shipit--display-side-frame (file-path commit-sha is-pr-commit comments)
  "Display side-frame with COMMENTS for FILE-PATH at COMMIT-SHA.

FILE-PATH: Path to file being reviewed
COMMIT-SHA: Current commit SHA
IS-PR-COMMIT: Whether this is a PR commit
COMMENTS: List of comment objects to display"
  (let ((buffer (shipit--create-review-side-frame-buffer file-path)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (shipit--format-side-frame-header commit-sha is-pr-commit))
      (insert "\n\n")
      (if (seq-empty-p comments)
          (insert "No comments on this line")
        (dolist (comment comments)
          (insert (format "Author: %s\n" (plist-get comment :author)))
          (insert (format "Time: %s\n" (plist-get comment :timestamp)))
          (insert (format "Body: %s\n\n" (plist-get comment :body))))))))

(defun shipit--filter-comments-for-commit (all-comments commit-sha file-path)
  "Filter ALL-COMMENTS to only those relevant for COMMIT-SHA on FILE-PATH.

Returns only comments where the line still exists in this commit version."
  (seq-filter (lambda (comment)
                (and (equal (plist-get comment :file) file-path)
                     (equal (plist-get comment :commit) commit-sha)))
              all-comments))

(defun shipit-review--find-sha-position (sha commits)
  "Find the position (index) of SHA in COMMITS list.
Returns 0-based index, or nil if not found."
  (let ((index 0))
    (catch 'found
      (dolist (commit commits)
        (let ((commit-sha (cdr (assq 'sha commit))))
          (when (and commit-sha (string-prefix-p sha commit-sha))
            (throw 'found index)))
        (setq index (1+ index)))
      nil)))

(defun shipit-review--generate-bar (width from-sha to-sha commits &optional single-commit-index)
  "Generate a graphical bar showing the comparison range.

WIDTH: Bar width in characters
FROM-SHA: Starting SHA (base or comparison target)
TO-SHA: Ending SHA (HEAD)
COMMITS: List of commit objects with 'sha field
SINGLE-COMMIT-INDEX: If set, shows single commit mode (0-based index)

Returns a string like [████████████████████░░░░░░░░░░░░░░░░░░░░]
with fill proportional to the range being compared."
  (if (and commits (> (length commits) 0))
      (let* ((total (length commits)))
        (if single-commit-index
            ;; Single commit mode: show just that commit filled
            (let* ((char-per-commit (/ width total))
                   (fill-start (* single-commit-index char-per-commit))
                   (fill-end (min width (+ fill-start (max 1 char-per-commit))))
                   (empty-before fill-start)
                   (fill-width (- fill-end fill-start))
                   (empty-after (- width fill-end)))
              (concat "["
                      (make-string empty-before ?░)
                      (make-string fill-width ?█)
                      (make-string empty-after ?░)
                      "]"))
          ;; Range mode: show the full range
          (if (and from-sha to-sha)
              (let* ((from-pos (or (shipit-review--find-sha-position from-sha commits) 0))
                     (to-pos (or (shipit-review--find-sha-position to-sha commits) (1- total)))
                     (range-start (min from-pos to-pos))
                     (range-end (max from-pos to-pos))
                     (fill-start (/ (* range-start width) total))
                     (fill-end (/ (* (1+ range-end) width) total))
                     (fill-width (max 1 (- fill-end fill-start)))
                     (empty-before fill-start)
                     (empty-after (- width fill-end)))
                (concat "["
                        (make-string empty-before ?░)
                        (make-string fill-width ?█)
                        (make-string empty-after ?░)
                        "]"))
            ;; Fallback: show full bar
            (concat "["
                    (make-string width ?█)
                    "]"))))
    ;; Fallback: show empty bar if no commits data
    (concat "["
            (make-string width ?░)
            "]")))

(defun shipit-review-side-frame--show-bar-menu ()
  "Show menu for bar interactions."
  (interactive)
  (message "Bar keys: [l] left side, [r] right side, [c] range"))

(defun shipit-review-side-frame--change-left-side ()
  "Change the left side (base) of the comparison range."
  (interactive)
  (when shipit-review--side-frame-pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--side-frame-pr-data)))
           (choice (completing-read "Select base commit: "
                                   (mapcar (lambda (c)
                                             (let ((sha (cdr (assq 'sha c)))
                                                   (msg (cdr (assq 'message c))))
                                               (format "%s %s" (substring sha 0 7) msg)))
                                           commits))))
      (when choice
        (let* ((sha (substring choice 0 7))
               (full-sha (cdr (assq 'sha (seq-find (lambda (c)
                                                     (string-prefix-p sha (cdr (assq 'sha c))))
                                                   commits)))))
          (with-current-buffer shipit-review--side-frame-main-buffer
            (when (fboundp 'shipit-review--set-comparison-target)
              (shipit-review--set-comparison-target full-sha "custom base"))))))))

(defun shipit-review-side-frame--change-right-side ()
  "Change the right side (HEAD) of the comparison range."
  (interactive)
  (when shipit-review--side-frame-pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--side-frame-pr-data)))
           (choice (completing-read "Select HEAD commit: "
                                   (mapcar (lambda (c)
                                             (let ((sha (cdr (assq 'sha c)))
                                                   (msg (cdr (assq 'message c))))
                                               (format "%s %s" (substring sha 0 7) msg)))
                                           commits)
                                   nil t)))
      (when choice
        (let* ((sha (substring choice 0 7)))
          ;; Note: Changing the right side (HEAD) would require updating the main buffer's head-sha
          ;; For now, we'll just update the display
          (message "Selected HEAD: %s (feature coming soon)" sha))))))

(defun shipit-review-side-frame--change-range ()
  "Change the comparison range interactively."
  (interactive)
  (message "Use [l] to change left side (base), [r] to change right side (HEAD)"))

;; Navigation functions that delegate to main buffer
(defun shipit-review-side-frame--make-jump-to-commit-fn (commit-num)
  "Create a function to jump to COMMIT-NUM (1-based)."
  (lambda ()
    (interactive)
    (when shipit-review--side-frame-main-buffer
      (with-current-buffer shipit-review--side-frame-main-buffer
        (when (fboundp 'shipit-review--make-jump-to-commit-fn)
          (funcall (shipit-review--make-jump-to-commit-fn commit-num)))))))

(defun shipit-review-side-frame--prev-commit ()
  "Go to previous commit."
  (interactive)
  (when shipit-review--side-frame-main-buffer
    (with-current-buffer shipit-review--side-frame-main-buffer
      (when (fboundp 'shipit-review--prev-commit)
        (shipit-review--prev-commit)))))

(defun shipit-review-side-frame--next-commit ()
  "Go to next commit."
  (interactive)
  (when shipit-review--side-frame-main-buffer
    (with-current-buffer shipit-review--side-frame-main-buffer
      (when (fboundp 'shipit-review--next-commit)
        (shipit-review--next-commit)))))

(defun shipit-review-side-frame--move-range-start-prev ()
  "Extend range by moving base to previous (older) commit."
  (interactive)
  (when shipit-review--side-frame-main-buffer
    (with-current-buffer shipit-review--side-frame-main-buffer
      (when (fboundp 'shipit-review--move-range-start-prev)
        (shipit-review--move-range-start-prev)))))

(defun shipit-review-side-frame--move-range-end-next ()
  "Move the right end of the range to the next commit."
  (interactive)
  (when shipit-review--side-frame-main-buffer
    (with-current-buffer shipit-review--side-frame-main-buffer
      (when (fboundp 'shipit-review--move-range-end-next)
        (shipit-review--move-range-end-next)))))

(defun shipit-review-side-frame--reset-to-full-range ()
  "Reset to full range (base..HEAD)."
  (interactive)
  (when shipit-review--side-frame-main-buffer
    (with-current-buffer shipit-review--side-frame-main-buffer
      (when (fboundp 'shipit-review--reset-to-full-range)
        (shipit-review--reset-to-full-range)))))

(defun shipit-review--display-side-frame-window (buffer)
  "Display BUFFER in a side-frame window.
Creates or reuses a window based on shipit-review-side-frame-position."
  (let* ((current-window (selected-window))
         (position shipit-review-side-frame-position)
         (width shipit-review-side-frame-width))
    ;; Check if side-frame window already exists
    (if (and shipit--review-side-frame-window
             (window-live-p shipit--review-side-frame-window))
        ;; Reuse existing window
        (set-window-buffer shipit--review-side-frame-window buffer)
      ;; Create new window
      (let ((new-window (if (eq position 'right)
                           (split-window current-window (- width) 'right)
                         (split-window current-window width 'left))))
        (setq shipit--review-side-frame-window new-window)
        (set-window-buffer new-window buffer)
        ;; Set window width
        (set-window-dedicated-p new-window t)))))

(defun shipit-review--display-side-frame-header (from-sha to-sha file-path pr-data main-buffer &optional single-commit-index)
  "Display side-frame with review info header showing comparison range.

FROM-SHA: Starting SHA (base or comparison target)
TO-SHA: Ending SHA (HEAD)
FILE-PATH: Path to the file being reviewed
PR-DATA: Full PR data object containing commits list
MAIN-BUFFER: The main file buffer for interactive operations
SINGLE-COMMIT-INDEX: If set, show single commit mode (0-based index)"
  (shipit--debug-log "SIDE-FRAME: display-side-frame-header called from=%s to=%s file=%s"
                     (if from-sha (substring from-sha 0 (min 7 (length from-sha))) "nil")
                     (if to-sha (substring to-sha 0 (min 7 (length to-sha))) "nil")
                     (if file-path (file-name-nondirectory file-path) "nil"))
  (let* ((commits (cdr (assq 'commits pr-data)))
         (from-short (if single-commit-index
                        (format "#%d" (1+ single-commit-index))
                      (if from-sha (substring from-sha 0 7) "unknown")))
         (to-short (if single-commit-index
                      (format "#%d" (1+ single-commit-index))
                    (if to-sha (substring to-sha 0 7) "HEAD")))
         (bar (shipit-review--generate-bar 40 from-sha to-sha commits single-commit-index)))
    (let ((buffer (shipit--create-review-side-frame-buffer file-path)))
      (with-current-buffer buffer
        ;; Enable side-frame mode
        (shipit-review-side-frame-mode)
        ;; Store PR data and main buffer reference for interactive operations
        (setq-local shipit-review--side-frame-pr-data pr-data)
        (setq-local shipit-review--side-frame-main-buffer main-buffer)

        (erase-buffer)
        (insert (format "ShipitReview: %s..%s\n" from-short to-short))
        ;; Insert bar with interactive properties
        (let ((bar-start (point)))
          (insert (format "%s\n" bar))
          (add-text-properties bar-start (point)
                             `(keymap ,shipit-review-side-frame-bar-map
                                      help-echo "l/r/c to adjust range, RET for menu"
                                      face (:background "#1e1e1e" :foreground "#00ff00"))))
        (insert "\n")
        ;; Show current file SHA
        (let ((file-name (file-name-nondirectory file-path))
              (current-sha (if single-commit-index
                             (let* ((commits (cdr (assq 'commits pr-data)))
                                    (commit (nth single-commit-index commits)))
                               (substring (cdr (assq 'sha commit)) 0 7))
                           (and from-sha (substring from-sha 0 7)))))
          (insert (format "File: %s [%s]\n" file-name (or current-sha "unknown"))))
        (insert "\n")
        (insert "Comments:\n")
        (insert "---------\n")
        (if (fboundp 'shipit-review--format-comments-for-side-frame)
            (let ((changed-lines (and main-buffer
                                      (with-current-buffer main-buffer
                                        (and (bound-and-true-p shipit-review--changed-lines)
                                             shipit-review--changed-lines)))))
              (insert (shipit-review--format-comments-for-side-frame file-path changed-lines)))
          (insert "(Comments will appear here)\n"))
        ;; Position cursor at top so navigation doesn't jump to bottom
        (goto-char (point-min)))
      ;; Display the buffer in a side-frame window
      (shipit-review--display-side-frame-window buffer))))

;; ============================================================================
;; Diff and Ediff Commands
;; ============================================================================

(defvar-local shipit-review-diff--file-buffer nil
  "Reference to the original file buffer for toggling back.")

(defvar-local shipit-review-diff--file-window nil
  "Reference to the window showing the file buffer.")

(defvar shipit-review-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'shipit-review-diff--switch-to-file)
    (define-key map (kbd "d") #'shipit-review-diff--switch-to-file)
    (define-key map (kbd "g") #'shipit-review-side-frame--refresh-diff)
    map)
  "Keymap for shipit review diff buffer.")

(define-derived-mode shipit-review-diff-mode special-mode "ShipitDiff"
  "Major mode for shipit review diff buffer.
Press \\<shipit-review-diff-mode-map>\\[shipit-review-diff--switch-to-file] to switch back to file view.
\\{shipit-review-diff-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun shipit-review-diff--switch-to-file ()
  "Switch back to the file buffer from diff view."
  (interactive)
  (when (and shipit-review-diff--file-buffer
             (buffer-live-p shipit-review-diff--file-buffer))
    (switch-to-buffer shipit-review-diff--file-buffer)))

(defun shipit-review-side-frame--get-commit-range ()
  "Get the current commit range from the main buffer.
Returns (from-sha . to-sha) or nil if not available."
  (when shipit-review--side-frame-main-buffer
    (with-current-buffer shipit-review--side-frame-main-buffer
      (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
             (from-sha nil)
             (to-sha nil))
        (cond
         ;; Single commit mode
         (shipit-review--current-commit-index
          (let ((commit (nth shipit-review--current-commit-index commits)))
            (setq to-sha (cdr (assq 'sha commit)))
            ;; Compare against parent or base
            (if (> shipit-review--current-commit-index 0)
                (let ((parent (nth (1- shipit-review--current-commit-index) commits)))
                  (setq from-sha (cdr (assq 'sha parent))))
              (setq from-sha shipit-review--base-sha))))
         ;; Range mode
         ((or shipit-review--range-start-index shipit-review--range-end-index)
          (let ((start-idx (or shipit-review--range-start-index 0))
                (end-idx (or shipit-review--range-end-index (1- (length commits)))))
            (if (= start-idx 0)
                (setq from-sha shipit-review--base-sha)
              (let ((parent (nth (1- start-idx) commits)))
                (setq from-sha (cdr (assq 'sha parent)))))
            (let ((end-commit (nth end-idx commits)))
              (setq to-sha (cdr (assq 'sha end-commit))))))
         ;; Default: full range
         (t
          (setq from-sha shipit-review--base-sha)
          (setq to-sha shipit-review--head-sha)))
        (when (and from-sha to-sha)
          (cons from-sha to-sha))))))

(defun shipit-review-side-frame--get-diff-output (from-sha to-sha file-path)
  "Get unified diff output between FROM-SHA and TO-SHA for FILE-PATH.
Returns the diff as a string with standard context (3 lines)."
  (let ((default-directory (file-name-directory file-path)))
    (condition-case err
        (shell-command-to-string
         (format "git diff --unified=3 %s..%s -- %s 2>/dev/null"
                 (shell-quote-argument from-sha)
                 (shell-quote-argument to-sha)
                 (shell-quote-argument (file-name-nondirectory file-path))))
      (error
       (when (fboundp 'shipit--debug-log)
         (shipit--debug-log "Error getting diff: %s" (error-message-string err)))
       ""))))

(defun shipit-review-side-frame--filter-comments-by-commit-range (comments from-sha to-sha commits)
  "Filter COMMENTS to only those relevant to the commit range FROM-SHA..TO-SHA.
COMMITS is the list of PR commits for SHA lookup.
Returns comments whose commit_id falls within the range, or all comments if
the commit_id field is not available."
  (when comments
    (let* ((from-idx (shipit-review--find-sha-position from-sha commits))
           (to-idx (shipit-review--find-sha-position to-sha commits))
           ;; Get list of SHAs in range
           (range-shas (when (and from-idx to-idx)
                        (mapcar (lambda (c) (cdr (assq 'sha c)))
                                (seq-subseq commits from-idx (1+ to-idx))))))
      (if range-shas
          ;; Try to filter by commit_id (GitHub REST API field)
          (let ((filtered (seq-filter
                          (lambda (comment)
                            (let ((commit-id (or (cdr (assq 'commit_id comment))
                                                (cdr (assq 'original_commit_id comment)))))
                              (and commit-id
                                   (seq-some (lambda (sha)
                                              (or (string-prefix-p commit-id sha)
                                                  (string-prefix-p sha commit-id)))
                                            range-shas))))
                          comments)))
            ;; If filtering removed all comments, return original list
            ;; (commit_id field might not be present)
            (if (and (null filtered) comments)
                (progn
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "DIFF-POPUP: No commit_id field in comments, showing all"))
                  comments)
              filtered))
        ;; If we can't determine range, return all comments
        comments))))

(defun shipit-review-side-frame--show-diff ()
  "Toggle between file view and unified diff view in the main buffer window.
When in file view, switch to diff. When in diff view, switch back to file."
  (interactive)
  (let* ((main-buffer shipit-review--side-frame-main-buffer)
         (main-window (when main-buffer
                       (get-buffer-window main-buffer)))
         (should-show-diff t))
    (unless main-buffer
      (user-error "No main buffer associated with side-frame"))

    ;; Check if main window is showing a diff buffer (toggle back to file)
    (when main-window
      (let ((current-buf (window-buffer main-window)))
        (when (with-current-buffer current-buf
                (and (derived-mode-p 'shipit-review-diff-mode)
                     shipit-review-diff--file-buffer))
          ;; Currently showing diff - switch back to file
          (with-current-buffer current-buf
            (shipit-review-diff--switch-to-file))
          (setq should-show-diff nil))))

    ;; Show diff view (unless we just toggled back to file)
    (when should-show-diff
      (let* ((range (shipit-review-side-frame--get-commit-range))
             (from-sha (car range))
             (to-sha (cdr range))
             (file-path (with-current-buffer main-buffer (buffer-file-name)))
             (pr-data shipit-review--side-frame-pr-data)
             (commits (cdr (assq 'commits pr-data))))
        (unless (and from-sha to-sha file-path)
          (user-error "Cannot determine diff range or file path"))

        (let* ((filename (file-name-nondirectory file-path))
               ;; Get repo-relative path for comment matching
               (relative-path (if (string-match ".worktrees/[^/]+/\\(.*\\)$" file-path)
                                 (match-string 1 file-path)
                               filename))
               (buffer-name (format "*shipit-diff: %s*" filename))
               (diff-output (shipit-review-side-frame--get-diff-output from-sha to-sha file-path))
               (diff-buffer (get-buffer-create buffer-name)))

          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "DIFF-VIEW: Showing diff for %s (%s..%s)"
                              filename
                              (substring from-sha 0 7)
                              (substring to-sha 0 7)))

          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              ;; Insert header
              (insert (propertize (format "Diff: %s (%s..%s)\n"
                                         filename
                                         (substring from-sha 0 7)
                                         (substring to-sha 0 7))
                                 'font-lock-face 'bold))
              (insert (make-string 40 ?=))
              (insert "\n\n")

              (if (string-empty-p diff-output)
                  (insert (propertize "No changes in this range.\n" 'font-lock-face 'italic))
                ;; Get comments from various sources
                (let* ((main-buffer-comments
                        (with-current-buffer main-buffer
                          (and (boundp 'shipit--cached-inline-comments)
                               shipit--cached-inline-comments)))
                       (magit-buffer-comments
                        (unless main-buffer-comments
                          (let ((found nil))
                            (dolist (buf (buffer-list))
                              (when (and (not found)
                                        (with-current-buffer buf
                                          (and (boundp 'shipit--cached-inline-comments)
                                               shipit--cached-inline-comments)))
                                (setq found (with-current-buffer buf
                                             shipit--cached-inline-comments))))
                            found)))
                       (all-comments (or main-buffer-comments magit-buffer-comments))
                       (file-comments (seq-filter (lambda (c)
                                                   (let ((path (cdr (assq 'path c))))
                                                     (and path (string= path relative-path))))
                                                 all-comments))
                       (range-comments (shipit-review-side-frame--filter-comments-by-commit-range
                                       file-comments from-sha to-sha commits)))

                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "DIFF-VIEW: relative-path=%s all=%d file=%d range=%d"
                                      relative-path
                                      (length all-comments)
                                      (length file-comments)
                                      (length range-comments)))

                  (setq-local shipit--cached-inline-comments range-comments)
                  (shipit--insert-file-diff diff-output relative-path (length range-comments) nil nil))))

            ;; Set up mode and back-reference
            (shipit-review-diff-mode)
            (setq-local shipit-review-diff--file-buffer main-buffer)
            (goto-char (point-min)))

          ;; Switch to diff buffer in the main window
          (when main-window
            (with-selected-window main-window
              (switch-to-buffer diff-buffer))))))))

(defun shipit-review-side-frame--refresh-diff ()
  "Refresh the current diff view."
  (interactive)
  (when (derived-mode-p 'shipit-review-diff-mode)
    (let ((file-buffer shipit-review-diff--file-buffer)
          (pos (point)))
      ;; Switch back to file briefly to re-trigger diff generation
      (when file-buffer
        (switch-to-buffer file-buffer)
        ;; Re-show diff (will regenerate content)
        (shipit-review-side-frame--show-diff)
        ;; Try to restore position
        (goto-char (min pos (point-max)))))))

(defun shipit-review-side-frame--show-ediff ()
  "Open ediff for current file between commit range endpoints.
Side-frame remains visible."
  (interactive)
  (let* ((range (shipit-review-side-frame--get-commit-range))
         (from-sha (car range))
         (to-sha (cdr range))
         (file-path (when shipit-review--side-frame-main-buffer
                     (with-current-buffer shipit-review--side-frame-main-buffer
                       (buffer-file-name)))))
    (unless (and from-sha to-sha file-path)
      (user-error "Cannot determine diff range or file path"))

    (let* ((filename (file-name-nondirectory file-path))
           (default-directory (file-name-directory file-path))
           ;; Get file content at each SHA
           (from-content (shell-command-to-string
                         (format "git show %s:%s 2>/dev/null"
                                 (shell-quote-argument from-sha)
                                 (shell-quote-argument filename))))
           (to-content (shell-command-to-string
                       (format "git show %s:%s 2>/dev/null"
                               (shell-quote-argument to-sha)
                               (shell-quote-argument filename))))
           (from-buffer (generate-new-buffer (format "*%s @ %s*" filename (substring from-sha 0 7))))
           (to-buffer (generate-new-buffer (format "*%s @ %s*" filename (substring to-sha 0 7)))))

      ;; Populate buffers
      (with-current-buffer from-buffer
        (insert from-content)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        ;; Set mode based on filename
        (let ((mode (assoc-default filename auto-mode-alist #'string-match-p)))
          (when mode (funcall mode))))

      (with-current-buffer to-buffer
        (insert to-content)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (let ((mode (assoc-default filename auto-mode-alist #'string-match-p)))
          (when mode (funcall mode))))

      ;; Launch ediff
      (ediff-buffers from-buffer to-buffer))))

;; Initialize keymaps (after functions are defined)
(setq shipit-review-side-frame-bar-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "l") #'shipit-review-side-frame--change-left-side)
        (define-key map (kbd "r") #'shipit-review-side-frame--change-right-side)
        (define-key map (kbd "c") #'shipit-review-side-frame--change-range)
        (define-key map (kbd "RET") #'shipit-review-side-frame--show-bar-menu)
        map))

(setq shipit-review-side-frame-mode-map
      (let ((map (make-sparse-keymap)))
        ;; Navigation keybindings
        (define-key map (kbd "p") #'shipit-review-side-frame--prev-commit)
        (define-key map (kbd "n") #'shipit-review-side-frame--next-commit)
        (define-key map (kbd "M-p") #'shipit-review-side-frame--move-range-start-prev)
        (define-key map (kbd "M-n") #'shipit-review-side-frame--move-range-end-next)
        (define-key map (kbd "=") #'shipit-review-side-frame--reset-to-full-range)
        ;; Diff and ediff
        (define-key map (kbd "d") #'shipit-review-side-frame--show-diff)
        (define-key map (kbd "e") #'shipit-review-side-frame--show-ediff)
        ;; Jump to commit (1-9)
        (dotimes (i 9)
          (define-key map (kbd (format "%d" (1+ i)))
                       (shipit-review-side-frame--make-jump-to-commit-fn (1+ i))))
        map))

(provide 'shipit-review-side-frame)
;;; shipit-review-side-frame.el ends here
