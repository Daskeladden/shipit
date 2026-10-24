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
;; File filtering for PR files sections.  The filter shows only some
;; files: by a text in their name, with comments, owned by the user
;; per CODEOWNERS, and so on.  Hiding takes chosen files out, and
;; combines with the filter.  Extracted from shipit-magit.el.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
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
(declare-function shipit-commit-comments-refresh-files-section "shipit-commit-comments-buffer")

;; Forward declarations — variables
(defvar shipit-buffer-repo)
(defvar shipit-buffer-pr-data)
(defvar shipit-buffer-pr-number)
(defvar shipit--cached-inline-comments)
(defvar shipit-review-queue)
(defvar shipit-review-queue--sections)

;;; Buffer-local filter state

(defvar-local shipit--files-filter-text ""
  "Current filter text for files section (main Files Changed).
Only files matching this text (case-insensitive) will be displayed.")

(defvar-local shipit--files-filter-mode nil
  "Current filter mode for files section (main Files Changed).
Possible values:
- nil: no special filter (use text filter if set)
- `comments': files with inline comments only
- `owned': files I own based on CODEOWNERS
- `viewed' / `unviewed': by viewed status
- `drafts': files with pending review-queue drafts only.
- a mode of `shipit-files-show-only-filters'.")

(defvar shipit-files-show-only-filters nil
  "Filters other packages add to what Files Changed can show only.
Each is (MODE DESCRIPTION PREDICATE).  PREDICATE is called with the
name of a file, and returns non-nil for a file to show.  DESCRIPTION
is what the filter line says.")

(defvar-local shipit--files-hidden nil
  "Names of the files hidden from Files Changed, one by one.")

(defvar-local shipit--files-hidden-texts nil
  "Texts that hide from Files Changed every file whose name contains one.
Matched without regard to case.")

;;; Filter predicates

(defun shipit--files-filter-active-p ()
  "Return non-nil if any file filter is currently active, or files are hidden."
  (or shipit--files-filter-mode
      (not (string-empty-p shipit--files-filter-text))
      shipit--files-hidden
      shipit--files-hidden-texts))

(defun shipit--name-contains-p (name text)
  "Non-nil when NAME contains TEXT, without regard to case.
TEXT is plain text, not a regexp: file names contain dots and brackets."
  (string-search (downcase text) (downcase name)))

(defun shipit--file-hidden-p (filename)
  "Non-nil when FILENAME is hidden from Files Changed."
  (and filename
       (or (member filename shipit--files-hidden)
           (seq-some (lambda (text) (shipit--name-contains-p filename text))
                     shipit--files-hidden-texts))))

(defun shipit--files-hidden-display (files)
  "How many of FILES are hidden, as the filter line says it, or nil."
  (when (or shipit--files-hidden shipit--files-hidden-texts)
    (format "%d hidden"
            (seq-count (lambda (file) (shipit--file-hidden-p (cdr (assq 'filename file))))
                       files))))

(defun shipit--get-files-filter-display (files)
  "The text of the filter line for FILES, or nil when no filter is active.
It says what the filter shows only, and how many of FILES are hidden."
  (let ((parts (delq nil (list (shipit--files-show-only-display)
                               (shipit--files-hidden-display files)))))
    (and parts (string-join parts ", "))))

(defun shipit--files-show-only-display ()
  "What the filter shows only, as the filter line says it, or nil."
  (cond
   ((eq shipit--files-filter-mode 'comments)
    "files with comments")
   ((eq shipit--files-filter-mode 'owned)
    "files I own (CODEOWNERS)")
   ((eq shipit--files-filter-mode 'viewed)
    "viewed files")
   ((eq shipit--files-filter-mode 'unviewed)
    "unviewed files")
   ((eq shipit--files-filter-mode 'drafts)
    "files with draft comments")
   ((assq shipit--files-filter-mode shipit-files-show-only-filters)
    (nth 1 (assq shipit--files-filter-mode shipit-files-show-only-filters)))
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

(defun shipit--file-has-draft-p (filename)
  "Return non-nil if FILENAME has any pending review-queue draft in the current PR.
Checks the buffer-local `shipit-review-queue' for entries whose
`:file' matches FILENAME exactly."
  (and filename
       (boundp 'shipit-review-queue)
       (cl-some (lambda (entry)
                  (let ((entry-file (plist-get entry :file)))
                    (and entry-file (string= entry-file filename))))
                shipit-review-queue)))

(defvar-local shipit--files-with-comments-cache nil
  "Hashtable FILENAME -> t for files that have at least one inline comment.
Built lazily by `shipit--file-has-comments-p' from
`shipit--cached-inline-comments'.  Identity of the source list
is tracked in `shipit--files-with-comments-token' so the cache
rebuilds when comments change.")

(defvar-local shipit--files-with-comments-token nil
  "Identity (eq) of `shipit--cached-inline-comments' the cache was built from.")

(defun shipit--files-with-comments-ensure-cache ()
  "Rebuild `shipit--files-with-comments-cache' if the comments list changed."
  (unless (eq shipit--files-with-comments-token
              (bound-and-true-p shipit--cached-inline-comments))
    (let ((tbl (make-hash-table :test 'equal)))
      (dolist (comment (bound-and-true-p shipit--cached-inline-comments))
        (let ((path (cdr (assq 'path comment))))
          (when path (puthash path t tbl))))
      (setq shipit--files-with-comments-cache tbl)
      (setq shipit--files-with-comments-token
            (bound-and-true-p shipit--cached-inline-comments)))))

(defun shipit--file-has-comments-p (filename)
  "Return non-nil if FILENAME has inline comments in the current PR.
O(1) hashtable lookup; the cache is rebuilt only when
`shipit--cached-inline-comments' identity changes.  The old
linear scan was the bottleneck behind the `with comments'
filter feeling laggy on PRs with many files."
  (shipit--files-with-comments-ensure-cache)
  (and filename
       shipit--files-with-comments-cache
       (gethash filename shipit--files-with-comments-cache)))

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
  "Return non-nil if FILE shows under the current filter, and is not hidden.
FILTER-TEXT is as for `shipit--file-matches-show-only-p'."
  (let ((filename (cdr (assq 'filename file))))
    (and (not (shipit--file-hidden-p filename))
         (shipit--file-matches-show-only-p filename filter-text))))

(defun shipit--filtered-files (files)
  "The FILES that Files Changed shows under the current filter."
  (if (shipit--files-filter-active-p)
      (seq-filter (lambda (file) (shipit--file-matches-filter-p file shipit--files-filter-text))
                  files)
    files))

(defun shipit--insert-files-filter-line (files)
  "Insert the line that says how FILES are filtered, when they are.
The line is marked as part of the files, so a refresh deletes it."
  (let ((display (shipit--get-files-filter-display files)))
    (when display
      (let ((start (point)))
        (insert (propertize (format "   Filter: %s (%d)\n"
                                    (propertize display 'font-lock-face 'font-lock-string-face)
                                    (length (shipit--filtered-files files)))
                            'font-lock-face 'italic))
        (add-text-properties start (point) '(shipit-pr-file t))))))

(defun shipit--file-matches-show-only-p (filename filter-text)
  "Return non-nil if FILENAME matches what the filter shows only.
Checks `shipit--files-filter-mode' first:
- nil: show only names that contain FILTER-TEXT, without regard to case
- `comments': show only files with inline comments
- `owned': show only files the current user owns per CODEOWNERS"
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
    ('drafts
     (shipit--file-has-draft-p filename))
    ((guard (assq shipit--files-filter-mode shipit-files-show-only-filters))
     (funcall (nth 2 (assq shipit--files-filter-mode shipit-files-show-only-filters))
              filename))
    (_
     ;; Default: text-based filename filter
     (if (or (null filter-text) (string-empty-p filter-text))
         t  ; Empty or nil filter matches everything
       (and filename (shipit--name-contains-p filename filter-text))))))

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
      (let ((new-filter (read-string "Show only files whose name contains: "
                                     shipit--files-filter-text)))
        ;; Apply the final filter value
        (setq shipit--files-filter-text new-filter)
        (setq shipit--files-filter-mode nil)  ; Clear special filter mode
        (message "Files filter set to: %s"
                 (if (string-empty-p new-filter) "(none)" new-filter))
        ;; Refresh main Files Changed section (commit files update on re-expand)
        (shipit--refresh-files-section-only)))))

(defun shipit-files-show-only (mode)
  "Show only the files of Files Changed that the filter MODE shows.
MODE is one of `shipit--files-filter-mode', such as `comments', or a
mode of `shipit-files-show-only-filters'.  It replaces the text filter."
  (setq shipit--files-filter-mode mode)
  (setq shipit--files-filter-text "")
  (shipit--refresh-files-section-only)
  (message "Showing %s only" (shipit--files-show-only-display)))

(defun shipit--filter-files-with-comments ()
  "Filter to show only files with inline comments."
  (interactive)
  (shipit-files-show-only 'comments))

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
  (shipit-files-show-only 'viewed))

(defun shipit--filter-files-unviewed ()
  "Filter to show only unviewed files."
  (interactive)
  (shipit-files-show-only 'unviewed))

(defun shipit--filter-files-with-drafts ()
  "Filter to show only files with pending review-queue drafts."
  (interactive)
  (shipit-files-show-only 'drafts))

(defun shipit--files-section ()
  "The Files Changed section of this buffer, or nil."
  (let ((pos (text-property-any (point-min) (point-max) 'shipit-pr-files t)))
    (and pos (save-excursion (goto-char pos) (magit-current-section)))))

(defun shipit--shown-file-names ()
  "The names of the files Files Changed shows now, in order."
  (let ((section (or (shipit--files-section)
                     (user-error "No Files Changed section in this buffer"))))
    (delq nil (mapcar (lambda (child) (and (eq 'pr-file (oref child type)) (oref child value)))
                      (oref section children)))))

(defun shipit--file-to-hide ()
  "The file at point, or else one picked by name from the files shown."
  (or (get-text-property (point) 'shipit-file-path)
      (completing-read "Hide file: "
                       (or (shipit--shown-file-names)
                           (user-error "No file to hide: open Files Changed first"))
                       nil t)))

(defun shipit--hide-file (filename)
  "Hide FILENAME from Files Changed.
Interactively, the file at point, or else one picked by name."
  (interactive (list (shipit--file-to-hide)))
  (cl-pushnew filename shipit--files-hidden :test #'equal)
  (shipit--refresh-files-section-only)
  (message "Hid %s" filename))

(defun shipit--hide-files-matching (text)
  "Hide from Files Changed every file whose name contains TEXT, in any case."
  (interactive (list (read-string "Hide files whose name contains: ")))
  (when (string-empty-p text)
    (user-error "No text to hide files by"))
  (cl-pushnew text shipit--files-hidden-texts :test #'equal)
  (shipit--refresh-files-section-only)
  (message "Hid the files whose name contains \"%s\"" text))

(defun shipit--clear-files-filter ()
  "Clear all file filters, and show the hidden files again."
  (interactive)
  (setq shipit--files-filter-mode nil)
  (setq shipit--files-filter-text "")
  (setq shipit--files-hidden nil)
  (setq shipit--files-hidden-texts nil)
  (setq shipit--codeowners-filter-cache nil)  ; Clear cache
  (shipit--refresh-files-section-only)
  (message "File filters cleared"))

(transient-define-prefix shipit-files-filter-transient ()
  "Show only some files of the PR, or hide some."
  [["Show only"
    ("f" "By name" shipit--set-files-filter)
    ("c" "With comments" shipit--filter-files-with-comments)
    ("o" "I own (CODEOWNERS)" shipit--filter-files-owned)
    ("v" "Viewed" shipit--filter-files-viewed)]
   [""
    ("u" "Unviewed" shipit--filter-files-unviewed)
    ("d" "With drafts" shipit--filter-files-with-drafts)]
   ["Hide"
    ("h" "This file" shipit--hide-file)
    ("H" "By name" shipit--hide-files-matching)]
   [""
    ("x" "Clear filter and hidden" shipit--clear-files-filter)
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

(defvar shipit-files-section-refreshed-hook nil
  "Hook run in a pull request buffer once Files Changed is drawn again.
A redraw, as when the filter changes, drops what other packages put
inside the file sections, and their functions here put it back.")

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
                                 (derived-mode-p 'shipit-preview-mode)
                                 (derived-mode-p 'shipit-commit-comments-mode))
                             (fboundp 'magit-current-section))
                    (let ((refresh-start (float-time)))
                      ;; Preview and commit-comments modes have their own
                      ;; targeted refreshes; plain shipit-mode is handled below
                      (if (or (derived-mode-p 'shipit-preview-mode)
                              (derived-mode-p 'shipit-commit-comments-mode))
                          (cond
                           ((derived-mode-p 'shipit-commit-comments-mode)
                            (when (fboundp 'shipit-commit-comments-refresh-files-section)
                              (shipit-commit-comments-refresh-files-section)
                              (setq refreshed-count (1+ refreshed-count))))
                           ((fboundp 'shipit-preview--refresh-files-section-only)
                            (shipit-preview--refresh-files-section-only)
                            (setq refreshed-count (1+ refreshed-count))))
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
                                            (run-hooks 'shipit-files-section-refreshed-hook)
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
