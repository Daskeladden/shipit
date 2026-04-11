;;; shipit-render.el --- render module -*- lexical-binding: t; -*-

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
;; render internals split from monolithic shipit.el

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'transient)  ; For PR preview menu
(require 'magit-section)  ; For magit-insert-section and related macros
(require 'shipit-core)  ; For utility functions like shipit--wrap-text
(require 'shipit-gh-etag)  ; For ETag-based caching of avatars
(require 'shipit-issue-backends)  ; For backend reference patterns
(require 'shipit-pr-backends)  ; For PR backend dispatch in #NNN routing

;; Forward declarations for variables from shipit-core
(defvar shipit-inline-comment-icon-style)
(defvar shipit-inline-comment-highlight-background)
(defvar shipit-inline-comment-left-border)
(defvar shipit-inline-comment-faces)
(defvar shipit-show-avatars)
(defvar shipit-show-unread-indicators)
(defvar shipit--current-displayed-pr)
(defvar shipit-render-wrap-column)
(defvar shipit-code-block-background)

;; Forward declarations for functions from other modules
(declare-function shipit--get-repo-from-remote "shipit-core")
(declare-function shipit--browse-pr-url "shipit-notifications")
(declare-function shipit--browse-issue-url "shipit-notifications")
(declare-function shipit--is-comment-unread-p "shipit-core")
(declare-function shipit--debug-log "shipit-debug")
(declare-function shipit--generate-avatar-url "shipit-pr-sections")
(declare-function shipit--is-comment-in-resolved-thread "shipit-http")
(declare-function shipit--format-timestamp "shipit-core")
(declare-function shipit--clean-text "shipit-core")
(declare-function shipit--get-repo-root "shipit-worktree")
(declare-function shipit--create-jira-mention-overlays "shipit-issue-jira")
(declare-function shipit--format-comment-reactions "shipit-http")
(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-insert-section-body "magit-section")
(declare-function shipit--search-prs-with-encoded-query "shipit-commands")
(declare-function shipit--select-pr-from-results "shipit-commands")
(declare-function shipit--edit-comment-interactive "shipit-commands")
(declare-function shipit--get-pr-actual-state "shipit-http")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-open-actions-run "shipit-actions")
(declare-function shipit-issues--fetch-issue "shipit-issues")
(declare-function shipit-issue--classify-url "shipit-issue-backends")
(declare-function shipit-open-releases-buffer "shipit-releases-buffer")

(defun shipit--filter-active-comments (comments)
  "Return only non-outdated comments from COMMENTS list.
COMMENTS should be an alist of comment data structures.
A comment is considered outdated if it has (outdated . t)."
  (cl-remove-if (lambda (comment)
                  (eq (cdr (assq 'outdated comment)) t))
                comments))

(defun shipit--filter-outdated-comments (comments)
  "Return only outdated comments from COMMENTS list.
COMMENTS should be an alist of comment data structures."
  (cl-remove-if-not (lambda (comment)
                      (eq (cdr (assq 'outdated comment)) t))
                    comments))

(defun shipit--filter-resolved-comments (comments)
  "Return only resolved comments from COMMENTS list.
COMMENTS should be an alist of comment data structures.
A comment is considered resolved if it's part of a resolved thread.
Note: REST API comments don't have resolved property; must check via hash."
  (cl-remove-if-not (lambda (comment)
                      (let ((comment-id (cdr (assq 'id comment))))
                        (and comment-id (shipit--is-comment-in-resolved-thread comment-id))))
                    comments))

(defun shipit--filter-comments-preserve-mixed-threads (comments)
  "Filter outdated comments but preserve roots with active replies.
COMMENTS is the list of all comments.
Returns filtered list that hides standalone outdated but shows outdated roots with active replies.

This function analyzes the comment thread structure to identify outdated root comments
that have active (non-outdated) replies, and preserves those for display with the [OUTDATED]
indicator while filtering out standalone outdated comments."
  ;; Build threading structure to identify parent-child relationships
  (let ((comment-lookup (make-hash-table :test 'equal))
        (threads (make-hash-table :test 'equal)))
    ;; Create lookup table for quick access
    (dolist (comment comments)
      (let ((comment-id (cdr (assq 'id comment))))
        (puthash comment-id comment comment-lookup)))

    ;; Build threads hash: maps parent-id to list of reply comments
    (dolist (comment comments)
      (let ((reply-to (cdr (assq 'in_reply_to_id comment))))
        (when reply-to
          (let* ((parent-id (if (numberp reply-to) reply-to (string-to-number (format "%s" reply-to))))
                 (existing-replies (gethash parent-id threads)))
            (puthash parent-id (cons comment existing-replies) threads)))))

    ;; Filter: remove outdated comments UNLESS they have active replies
    (cl-remove-if
     (lambda (comment)
       (let* ((is-outdated (eq (cdr (assq 'outdated comment)) t))
              (comment-id (cdr (assq 'id comment)))
              (replies (gethash comment-id threads))
              (has-active-replies (and replies
                                       (cl-some (lambda (reply)
                                                  (not (eq (cdr (assq 'outdated reply)) t)))
                                                replies))))
         ;; Remove if outdated AND (no replies OR all replies outdated)
         (and is-outdated (not has-active-replies))))
     comments)))

(defun shipit--filter-comments-by-commit (comments commit-sha)
  "Filter COMMENTS to those made on COMMIT-SHA.
For threaded comments, include entire thread if root comment matches.
COMMIT-SHA can be a full SHA or abbreviated prefix.
Returns nil if COMMIT-SHA is nil or COMMENTS is empty."
  (when (and comments commit-sha)
    (let ((comment-lookup (make-hash-table :test 'equal))
          (root-ids-in-commit (make-hash-table :test 'equal))
          (reply-to-root (make-hash-table :test 'equal)))

      ;; Build lookup table and identify root comments
      (dolist (comment comments)
        (let* ((comment-id (cdr (assq 'id comment)))
               (c-commit-id (cdr (assq 'commit_id comment)))
               (c-original-commit-id (cdr (assq 'original_commit_id comment)))
               (in-reply-to (cdr (assq 'in_reply_to_id comment))))
          (puthash comment-id comment comment-lookup)

          ;; Track reply relationships
          (when in-reply-to
            (puthash comment-id in-reply-to reply-to-root))

          ;; Check if this root comment matches the commit
          ;; (only check roots - replies are handled via thread membership)
          (unless in-reply-to
            (when (or (and c-commit-id
                          (or (string-prefix-p commit-sha c-commit-id)
                              (string-prefix-p c-commit-id commit-sha)))
                     (and c-original-commit-id
                          (or (string-prefix-p commit-sha c-original-commit-id)
                              (string-prefix-p c-original-commit-id commit-sha))))
              (puthash comment-id t root-ids-in-commit)))))

      ;; Find root for any comment (walks up reply chain)
      (cl-labels ((find-root (comment-id visited)
                    (if (or (gethash comment-id visited)
                            (not (gethash comment-id reply-to-root)))
                        comment-id
                      (puthash comment-id t visited)
                      (find-root (gethash comment-id reply-to-root) visited))))

        ;; Filter: include comment if its root is in the commit
        (cl-remove-if-not
         (lambda (comment)
           (let* ((comment-id (cdr (assq 'id comment)))
                  (root-id (find-root comment-id (make-hash-table :test 'equal))))
             (gethash root-id root-ids-in-commit)))
         comments)))))

(defun shipit--get-mode-for-language (lang)
  "Get the appropriate major mode for programming language LANG.
Uses `shipit--language-mode-alist' for comprehensive language support."
  (or (cdr (assoc (downcase (or lang "")) shipit--language-mode-alist))
      'text-mode))

(defun shipit--extract-lines-from-diff-hunk (diff-hunk start-line end-line)
  "Extract lines START-LINE through END-LINE from DIFF-HUNK.
Returns a list of line contents (without +/-/space prefix) for lines
in the new-file side of the hunk that fall within the given range.
Falls back to the last N lines of the new-file side if exact line
matching fails (common for review comment hunks).
Returns nil if DIFF-HUNK is nil or invalid."
  (when (and diff-hunk (stringp diff-hunk) (> (length diff-hunk) 0))
    (let* ((lines (split-string diff-hunk "\n" t))
           (result '())
           (all-new-lines '())
           (num-lines-wanted (1+ (- end-line start-line))))
      ;; Collect all new-file lines and try exact match
      (when (and (car lines)
                 (string-match "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" (car lines)))
        (let ((current-line (string-to-number (match-string 1 (car lines)))))
          (dolist (hunk-line (cdr lines))
            (when (> (length hunk-line) 0)
              (let ((prefix (aref hunk-line 0)))
                (cond
                 ((eq prefix ?-)
                  nil)
                 ((or (eq prefix ?+) (eq prefix ?\s))
                  (push (substring hunk-line 1) all-new-lines)
                  (when (and (>= current-line start-line)
                             (<= current-line end-line))
                    (push (substring hunk-line 1) result))
                  (setq current-line (1+ current-line)))))))))
      (setq all-new-lines (nreverse all-new-lines))
      ;; Use exact match if found, otherwise take last N lines
      (or (and result (nreverse result))
          (when (>= (length all-new-lines) num-lines-wanted)
            (last all-new-lines num-lines-wanted))))))

(defun shipit--render-suggestion-block (new-code old-lines)
  "Render a suggestion as a diff-style block.
NEW-CODE is the replacement text.  OLD-LINES is a list of original line strings.
Returns a propertized string with diff-removed and diff-added faces."
  (let ((parts (list (propertize "Suggested change:" 'face 'font-lock-comment-face))))
    (when old-lines
      (dolist (old-line old-lines)
        (push (propertize (concat "- " old-line) 'face 'diff-removed) parts)))
    (dolist (new-line (split-string new-code "\n"))
      (push (propertize (concat "+ " new-line) 'face 'diff-added) parts))
    (mapconcat #'identity (nreverse parts) "\n")))

(defun shipit--suggestion-lang-p (lang)
  "Return non-nil if LANG indicates a suggestion code block."
  (and lang (string-prefix-p "suggestion" lang)))

(defun shipit--add-suggestion-text-properties (body-start body-end comment)
  "Add suggestion text properties from BODY-START to BODY-END.
COMMENT is the comment alist.  Detects suggestion code blocks in the
comment body and adds shipit-suggestion properties for the `a' keybind."
  (let* ((raw-body (or (cdr (assq 'body comment)) ""))
         (body (shipit--clean-comment-text raw-body))
         (snippets (shipit--extract-code-snippets body)))
    (when (and (fboundp 'shipit--debug-log)
               (cl-some (lambda (s) (shipit--suggestion-lang-p (plist-get s :lang))) snippets))
      (shipit--debug-log "SUGGESTION add-props: %d snippets, region=%d-%d"
                         (length snippets) body-start body-end))
    (dolist (snippet snippets)
      (when (shipit--suggestion-lang-p (plist-get snippet :lang))
        (let* ((suggestion-code (plist-get snippet :code))
               (file-path (cdr (assq 'path comment)))
               (line-number (shipit--json-number-p (cdr (assq 'line comment))))
               (raw-start (cdr (assq 'start_line comment)))
               (start-line-num (or (and (numberp raw-start) raw-start)
                                   line-number))
               (end-line-num line-number))
          (add-text-properties body-start body-end
                               (list 'shipit-suggestion t
                                     'shipit-suggestion-code suggestion-code
                                     'shipit-suggestion-path file-path
                                     'shipit-suggestion-start-line start-line-num
                                     'shipit-suggestion-end-line end-line-num)))))))

(defun shipit--render-comment-with-code (body &optional comment)
  "Render comment BODY with syntax highlighting for code snippets.
When COMMENT is provided and contains a suggestion block, render it
as a diff-style block showing old and new lines."
  (if (string-empty-p body)
      ""
    (let ((snippets (shipit--extract-code-snippets body))
          (result "")
          (last-pos 0))

      (dolist (snippet snippets)
        (let ((start (plist-get snippet :start))
              (end (plist-get snippet :end))
              (lang (plist-get snippet :lang))
              (code (plist-get snippet :code)))

          ;; Add text before code snippet
          (setq result (concat result (substring body last-pos start)))

          (if (shipit--suggestion-lang-p lang)
              ;; Render suggestion as diff-style block
              (let* ((diff-hunk (when comment (shipit--json-string-p (cdr (assq 'diff_hunk comment)))))
                     (line-num (when comment (shipit--json-number-p (cdr (assq 'line comment)))))
                     (start-line-num (when comment
                                       (or (shipit--json-number-p (cdr (assq 'start_line comment)))
                                           line-num)))
                     (old-lines (when (and diff-hunk line-num start-line-num)
                                  (shipit--extract-lines-from-diff-hunk
                                   diff-hunk start-line-num line-num))))
                (setq result (concat result "\n"
                                     (shipit--render-suggestion-block code old-lines)
                                     "\n")))
            ;; Regular code block — render with syntax highlighting
            (let* ((clean-lang (if (string-empty-p lang) "text" lang))
                   (highlighted-code (shipit--highlight-code-snippet code clean-lang)))
              (setq result (concat result
                                   "\n" (propertize (format "[%s]" clean-lang) 'face 'font-lock-comment-face)
                                   "\n" highlighted-code "\n"))))

          (setq last-pos end)))

      ;; Add remaining text
      (concat result (substring body last-pos)))))

(defun shipit--html-to-gfm (html-content)
  "Convert HTML to GitHub Flavored Markdown using pandoc.
Falls back to original content if pandoc is not available."
  (if (and (stringp html-content) (executable-find "pandoc"))
      (condition-case err
          (let ((result (with-temp-buffer
                          (insert html-content)
                          (shell-command-on-region (point-min) (point-max)
                                                   "pandoc -f html -t gfm" t)
                          ;; Clean up unnecessary escaping in markdown tables
                          ;; Pandoc escapes hyphens as \- but markdown tables don't need this
                          (shipit--cleanup-markdown-escaping (buffer-string)))))
            (if (stringp result)
                result
              (shipit--debug-log "❌ Pandoc returned non-string result: %s" (type-of result))
              html-content))
        (error
         (shipit--debug-log "❌ Pandoc conversion failed: %s" err)
         html-content))
    html-content))

(defun shipit--cleanup-markdown-escaping (markdown-text)
  "Remove unnecessary escaping in markdown tables and strip raw HTML.
Pandoc escapes hyphens as \\- in table cells and sometimes includes raw HTML,
but markdown tables don't need these."
  ;; First remove escaping from hyphens in markdown tables
  (let ((step1 (replace-regexp-in-string "\\\\-" "-" markdown-text)))
    ;; Then remove raw HTML blocks that pandoc might have left
    ;; Look for <table and strip everything from there to </table>
    (with-temp-buffer
      (insert step1)
      (goto-char (point-min))
      (while (search-forward "<table" nil t)
        (let ((table-start (- (point) (length "<table")))
              (table-end (search-forward "</table>" nil t)))
          (if table-end
              (delete-region table-start table-end)
            (delete-region table-start (point-max))
            (goto-char (point-max)))))
      (buffer-string))))

(defun shipit--parse-html-table-aligned (table-html)
  "Parse HTML table using libxml and convert to aligned text table.
Uses space-based alignment without markdown colons."
  (condition-case err
      (with-temp-buffer
        (insert table-html)
        (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
               (table-node (shipit--dom-find-tag dom 'table)))
          (if (not table-node)
              table-html
            ;; Extract headers (handles colspan/rowspan flattening)
            (let ((headers (shipit--flatten-table-headers table-node))
                  (rows '()))
              ;; Extract data rows from tbody
              (let ((tbody (shipit--dom-find-tag table-node 'tbody)))
                (if tbody
                    (dolist (tr (shipit--dom-find-all-tags tbody 'tr))
                      (let ((cells '()))
                        (dolist (cell (shipit--dom-find-all-tags tr 'td))
                          (push (shipit--dom-get-cell-text cell) cells))
                        (when cells
                          (push (reverse cells) rows))))
                  ;; If no tbody, get all rows with td
                  (dolist (tr (shipit--dom-find-all-tags table-node 'tr))
                    (let ((cells '()))
                      (dolist (cell (shipit--dom-find-all-tags tr 'td))
                        (push (shipit--dom-get-cell-text cell) cells))
                      (when cells
                        (push (reverse cells) rows))))))

              ;; Use first row as header if no headers found
              (when (and (not headers) rows)
                (setq headers (list (car rows)))
                (setq rows (cdr rows)))

              ;; Build aligned table if we have data
              (if (and headers (> (length headers) 0))
                  (shipit--build-aligned-text-table headers (reverse rows))
                table-html)))))
    (error
     (shipit--debug-log "❌ libxml table parsing failed: %s" err)
     table-html)))

(defun shipit--convert-html-tables-to-markdown (content)
  "Find and convert HTML tables to aligned markdown (space-based, no colons).
Returns content with tables converted, or original content if no tables found."
  (condition-case err
      (let ((result content)
            (converted-p nil)
            (search-start 0))
        ;; Keep replacing tables until none are found
        (while (setq search-start (string-match "<table\\b" result search-start))
          (shipit--debug-log "🔍 Found table at position %d" search-start)
          (let* ((table-start search-start)
                 (table-end (string-match "</table>" result table-start)))
            (shipit--debug-log "🔍 Searching for closing tag from %d, found at %s" table-start table-end)
            (if table-end
                (let* ((table-end-pos (+ table-end (length "</table>")))
                       (table-html (substring result table-start table-end-pos))
                       (converted-table (shipit--parse-html-table-aligned table-html)))
                  (shipit--debug-log "🔍 Extracted table (last 50 chars): ...%s"
                                    (substring table-html (max 0 (- (length table-html) 50))))
                  (if (stringp converted-table)
                      (progn
                        (shipit--debug-log "🔄 Converting HTML table (input: %d bytes, output: %d bytes)"
                                          (length table-html) (length converted-table))
                        (shipit--debug-log "🔍 Before replacement: result length=%d, table-start=%d, table-end-pos=%d"
                                          (length result) table-start table-end-pos)
                        ;; Replace this table in the result string
                        (let ((before (substring result 0 table-start))
                              (after (substring result table-end-pos)))
                          (shipit--debug-log "🔍 CONCAT: before(%d) + markdown(%d) + after(%d) = %d"
                                            (length before) (length converted-table) (length after)
                                            (+ (length before) (length converted-table) (length after)))
                          (setq result (concat before converted-table after)))
                        (shipit--debug-log "🔍 After replacement: result length=%d" (length result))
                        ;; Continue searching after the inserted markdown
                        (setq search-start (+ table-start (length converted-table)))
                        (setq converted-p t))
                    (shipit--debug-log "❌ Table conversion returned non-string: %s" (type-of converted-table))
                    (setq search-start (1+ table-start))))
              ;; No closing tag found, skip past this opening tag
              (setq search-start (1+ table-start)))))
        (if converted-p
            (shipit--debug-log "✅ Tables converted successfully"))
        (shipit--debug-log "📄 Final content length: %d bytes (original: %d bytes)"
                          (length result) (length content))
        (shipit--debug-log "📄 FINAL RETURN - has <table tag? %s"
                          (if (string-match "<table" result) "YES ❌" "NO ✅"))
        result)
    (error
     (shipit--debug-log "❌ Table conversion error: %s" err)
     content)))

(defun shipit--insert-comment-simple (comment &optional indent-level is-general)
  "Insert a single COMMENT with optional INDENT-LEVEL."
  (let* ((user (cdr (assq 'login (cdr (assq 'user comment)))))
         (body (shipit--clean-text (cdr (assq 'body comment))))
         (created (cdr (assq 'created_at comment)))
         (path (cdr (assq 'path comment)))
         (line (cdr (assq 'line comment)))
         (comment-type (cdr (assq 'shipit-comment-type comment)))
         (is-review-comment (string= comment-type "review"))
         (icon (if is-review-comment "📋" "👤"))  ; Review icon vs regular comment icon
         (indent (make-string (* (or indent-level 0) 2) ?\s))
         (start-pos (point))
         (timestamp-str (if created
                            (format-time-string "%Y-%m-%d %H:%M" (date-to-time created))
                          "Unknown date")))

    ;; Insert header line with icon
    (insert (format "%s%s " indent icon))

    ;; Insert username with face if enabled
    (let ((user-start (point)))
      (insert user)
      (let ((user-end (point)))
        ;; Apply conditional inline comment face if enabled
        (when shipit-inline-comment-faces
          (put-text-property user-start user-end 'face 'shipit-inline-comment-username-face))
        ;; Also apply the general username face
        (put-text-property user-start user-end 'face 'shipit-username-face)))

    ;; Insert separator and timestamp with face
    (insert " • ")
    (let ((timestamp-start (point)))
      (insert timestamp-str)
      (let ((timestamp-end (point)))
        ;; Apply conditional inline comment face if enabled
        (when shipit-inline-comment-faces
          (put-text-property timestamp-start timestamp-end 'face 'shipit-inline-comment-timestamp-face))
        ;; Also apply the general timestamp face
        (put-text-property timestamp-start timestamp-end 'face 'shipit-timestamp-face)))

    ;; Insert file path and line number if not general comment
    (unless is-general
      (when path
        (insert (format " • %s" (file-name-nondirectory path)))
        (when line
          (insert (format ":%d" line)))))
    (insert "\n")

    ;; Render comment body with code highlighting
    (let ((rendered-body (shipit--render-comment-with-code body comment)))
      (let ((body-lines (split-string rendered-body "\n")))
        (dolist (body-line body-lines)
          (let ((line-start (point)))
            (insert (concat indent "  " body-line "\n"))
            ;; Apply body face if enabled
            (when shipit-inline-comment-faces
              (put-text-property line-start (- (point) 1) 'face 'shipit-inline-comment-body-face))))))

    ;; Insert spacing between comments (1 blank line = 1 newline)
    (insert "\n")))

(defun shipit--apply-blockquote-faces (start end)
  "Apply blockquote styling to lines containing > in region START to END.
This ensures blockquoted text in diff buffers has proper markdown styling."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-start (point))
            (line-text (thing-at-point 'line t))
            (line-end (progn (end-of-line) (point))))
        ;; Check if this line contains a blockquote marker (>)
        ;; It might be after thread-prefix and border-indicator
        (when (and line-text (string-match ">+" line-text))
          ;; Use blue color for blockquotes to match general comments
          ;; Use 'prepend to put this face first so it overrides the gray foreground
          (add-face-text-property line-start line-end '(:foreground "#0366d6") 'prepend))
        (forward-line 1)))))

(defun shipit--apply-strikethrough-faces (start end)
  "Apply strikethrough styling to ~text~ patterns in region START to END.
Supports both single tilde ~text~ and double tilde ~~text~~ (GFM syntax)."
  (save-excursion
    (goto-char start)
    ;; Match both ~text~ and ~~text~~ patterns
    ;; Use non-greedy match and avoid matching inside code blocks
    (while (re-search-forward "\\(~~\\([^~\n]+\\)~~\\|~\\([^~\n]+\\)~\\)" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        ;; Apply strikethrough face to the entire match including tildes
        (add-face-text-property match-start match-end '(:strike-through t))))))

(defconst shipit--suggestion-placeholder-prefix "SHIPITSUGGESTBLOCK"
  "Prefix for suggestion placeholders in comment body processing.")

(defun shipit--preprocess-suggestions (body comment)
  "Replace suggestion code blocks in BODY with placeholders.
COMMENT provides context.  Returns (BODY-WITH-PLACEHOLDERS . SUGGESTION-ALIST)
where SUGGESTION-ALIST maps placeholder strings to rendered suggestion strings."
  (let ((snippets (shipit--extract-code-snippets body)))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "SUGGESTION preprocess: %d snippets, langs=%S, body-len=%d, body=%S"
                         (length snippets)
                         (mapcar (lambda (s) (plist-get s :lang)) snippets)
                         (length body)
                         (if (> (length body) 200) (substring body 0 200) body)))
    (if (cl-some (lambda (s) (shipit--suggestion-lang-p (plist-get s :lang))) snippets)
        (let ((result "")
              (last-pos 0)
              (suggestions nil)
              (idx 0))
          (dolist (snippet snippets)
            (let ((start (plist-get snippet :start))
                  (end (plist-get snippet :end))
                  (lang (plist-get snippet :lang))
                  (code (plist-get snippet :code)))
              (setq result (concat result (substring body last-pos start)))
              (if (shipit--suggestion-lang-p lang)
                  (let* ((placeholder (format "%s%dEND" shipit--suggestion-placeholder-prefix idx))
                         (old-lines (shipit--get-suggestion-old-lines comment)))
                    (push (cons placeholder
                                (shipit--render-suggestion-block code old-lines))
                          suggestions)
                    (setq result (concat result "\n" placeholder "\n"))
                    (setq idx (1+ idx)))
                (setq result (concat result (substring body start end))))
              (setq last-pos end)))
          (cons (concat result (substring body last-pos))
                (nreverse suggestions)))
      (cons body nil))))

(defun shipit--json-number-p (value)
  "Return VALUE if it is a number, nil otherwise.
Handles :json-null and :json-false from `json-read'."
  (and (numberp value) value))

(defun shipit--json-string-p (value)
  "Return VALUE if it is a non-empty string, nil otherwise.
Handles :json-null and :json-false from `json-read'."
  (and (stringp value) (not (string-empty-p value)) value))

(defun shipit--hunk-last-new-lines (diff-hunk n)
  "Return the last N lines from the new-file side of DIFF-HUNK."
  (when (and diff-hunk (stringp diff-hunk) (> (length diff-hunk) 0))
    (let ((lines (split-string diff-hunk "\n" t))
          (new-lines '()))
      (dolist (hunk-line (cdr lines)) ; skip header
        (when (> (length hunk-line) 0)
          (let ((prefix (aref hunk-line 0)))
            (when (or (eq prefix ?+) (eq prefix ?\s))
              (push (substring hunk-line 1) new-lines)))))
      (setq new-lines (nreverse new-lines))
      (when (>= (length new-lines) n)
        (last new-lines n)))))

(defun shipit--get-suggestion-old-lines (comment)
  "Get the old lines that a suggestion would replace from COMMENT.
Tries diff hunk with line numbers first, then hunk last-line fallback,
then falls back to reading the local file."
  (let* ((diff-hunk (shipit--json-string-p (cdr (assq 'diff_hunk comment))))
         (line-num (shipit--json-number-p (cdr (assq 'line comment))))
         (start-line-num (or (shipit--json-number-p (cdr (assq 'start_line comment)))
                             line-num))
         (file-path (shipit--json-string-p (cdr (assq 'path comment))))
         (old-lines (when (and diff-hunk line-num start-line-num)
                      (shipit--extract-lines-from-diff-hunk
                       diff-hunk start-line-num line-num))))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "SUGGESTION old-lines: hunk=%s line=%s start=%s path=%s result=%S"
                         (if diff-hunk "present" "nil")
                         line-num start-line-num file-path old-lines))
    (or old-lines
        ;; Fallback: when line numbers are missing but hunk is present,
        ;; the hunk typically ends at the commented line — take the last line
        (when (and diff-hunk (not line-num))
          (shipit--hunk-last-new-lines diff-hunk 1))
        (shipit--read-lines-from-file file-path start-line-num line-num))))

(defun shipit--read-lines-from-file (relative-path start-line end-line)
  "Read lines START-LINE through END-LINE from RELATIVE-PATH in the repo.
Returns a list of line strings, or nil if the file cannot be read."
  (when (and relative-path start-line end-line)
    (let* ((repo-root (when (fboundp 'shipit--get-repo-root)
                        (shipit--get-repo-root)))
           (full-path (when repo-root
                        (expand-file-name relative-path repo-root))))
      (when (and full-path (file-readable-p full-path))
        (with-temp-buffer
          (insert-file-contents full-path)
          (let ((lines (split-string (buffer-string) "\n"))
                (result nil))
            (cl-loop for i from start-line to (min end-line (length lines))
                     do (push (nth (1- i) lines) result))
            (nreverse result)))))))

(defun shipit--replace-suggestion-placeholders (text suggestions)
  "Replace suggestion placeholders in TEXT with rendered SUGGESTIONS.
SUGGESTIONS is an alist of (PLACEHOLDER . RENDERED) pairs.
Replaces the entire line containing the placeholder, preserving the
line's indentation prefix on each rendered line."
  (with-temp-buffer
    (insert text)
    (dolist (pair suggestions)
      (let ((placeholder (car pair))
            (rendered (cdr pair)))
        (goto-char (point-min))
        (while (search-forward placeholder nil t)
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line-text (buffer-substring line-start (match-beginning 0)))
                 (indent-prefix (if (string-match "\\`\\([ \t]*\\)" line-text)
                                    (match-string 1 line-text)
                                  ""))
                 (indented-rendered
                  (mapconcat (lambda (line) (concat indent-prefix line))
                             (split-string rendered "\n")
                             "\n")))
            (delete-region line-start line-end)
            (goto-char line-start)
            (insert indented-rendered)))))
    (buffer-string)))

(defun shipit--render-comment-body (comment indent-level)
  "Render comment body with markdown, wrapping, and blockquote styling.
Returns the fully rendered and indented body text.
Does NOT insert into buffer - caller is responsible for insertion and text properties."
  (let* ((raw-body (or (cdr (assq 'body comment)) ""))
         (body (shipit--clean-comment-text raw-body))
         (_suggestion-trace (when (and (fboundp 'shipit--debug-log)
                                       (string-match-p "```suggestion" (or raw-body "")))
                              (shipit--debug-log "SUGGESTION render-comment-body: has suggestion block, comment keys=%S"
                                                 (mapcar #'car comment))))
         ;; Pre-process suggestion blocks — replace with placeholders
         (preprocess-result (shipit--preprocess-suggestions body comment))
         (body (car preprocess-result))
         (suggestion-replacements (cdr preprocess-result))
         (comment-type (cdr (assq 'shipit-comment-type comment)))
         (review-state (cdr (assq 'review-state comment)))
         (rendered-body (if shipit-render-markdown
                            (shipit--render-markdown body)
                          body))
         (final-body (cond
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "APPROVED")
                            (or (not rendered-body) (string-empty-p rendered-body)))
                       "✅ Approved this pull request")
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "CHANGES_REQUESTED")
                            (or (not rendered-body) (string-empty-p rendered-body)))
                       "❌ Requested changes on this pull request")
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "DISMISSED")
                            (or (not rendered-body) (string-empty-p rendered-body)))
                       "🚫 Dismissed previous review")
                      (t rendered-body)))
         ;; Convert HTML tables BEFORE wrapping to preserve table structure
         (tables-converted (shipit--convert-html-tables-to-markdown (or final-body "")))
         ;; Wrap text but preserve table lines (those starting with |)
         ;; For blockquote lines (those starting with >), wrap only if they exceed the width
         (wrapped-body (let ((lines (split-string tables-converted "\n"))
                            (result '())
                            (target-width (- (or (and (boundp 'shipit-render-wrap-column) shipit-render-wrap-column) 120) indent-level)))
                        (dolist (line lines)
                          ;; Don't wrap table lines (they start with |)
                          (if (string-match-p "^|" line)
                              (push line result)
                            ;; For blockquote lines, only wrap if they're too long
                            ;; Match nested quotes like "> > > text" - captures full prefix including spaces
                            (if (string-match "^\\(\\(?:>[ \t]*\\)+\\)" line)
                                (if (<= (length line) target-width)
                                    ;; Line fits within width, preserve as-is
                                    (push line result)
                                  ;; Line is too long, wrap it while preserving full quote prefix
                                  (let* ((quote-prefix (match-string 1 line))
                                         (content (substring line (match-end 1)))
                                         (prefix-len (length quote-prefix))
                                         (content-width (max 20 (- target-width prefix-len))))
                                    ;; Wrap the content and add prefix back
                                    (let ((wrapped (shipit--wrap-text content content-width)))
                                      (dolist (wrapped-line (split-string wrapped "\n"))
                                        (unless (string-empty-p wrapped-line)
                                          (push (concat quote-prefix wrapped-line) result))))))
                              ;; Wrap regular non-blockquote lines
                              (let ((wrapped (shipit--wrap-text line target-width)))
                                (dolist (wrapped-line (split-string wrapped "\n"))
                                  (push wrapped-line result))))))
                        (mapconcat 'identity (reverse result) "\n")))
         ;; Clean again after wrapping to remove any stray carriage returns
         (cleaned-wrapped (shipit--clean-text wrapped-body))
         (body-indent-str (make-string (+ indent-level 0) ?\s))
         ;; Add indentation to ALL lines including the first line
         ;; This ensures blockquotes and regular text are consistently aligned
         (indented-body (let ((lines (split-string (or cleaned-wrapped "") "\n")))
                         (mapconcat (lambda (line) (concat body-indent-str line))
                                    lines "\n")))
         ;; Replace suggestion placeholders with rendered diff-style blocks
         (indented-body (if suggestion-replacements
                            (shipit--replace-suggestion-placeholders
                             indented-body suggestion-replacements)
                          indented-body)))
    indented-body))

(defun shipit--insert-comment-body-only (comment indent-level &optional custom-keymap file-path repo pr-number line-number skip-reactions)
  "Insert only the comment body without header - for use in magit sections with existing headers.
If SKIP-REACTIONS is non-nil, reactions are not rendered (caller will handle them separately)."
  (let* ((raw-body (shipit--clean-text (or (cdr (assq 'body comment)) "")))
         (comment-type (cdr (assq 'shipit-comment-type comment)))
         (review-state (cdr (assq 'review-state comment)))
         ;; Use the common rendering function
         (indented-body (shipit--render-comment-body comment indent-level))
         (body-indent-str (make-string (+ indent-level 0) ?\s)))

    ;; Insert comment body with text properties for M-; and RET functionality
    (let* ((body-start (point))
           (body-keymap (or (and (keymapp custom-keymap) custom-keymap)
                            (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map (current-local-map))
                              (define-key map [return] 'shipit--file-at-point)
                              (define-key map (kbd "SPC") 'shipit--file-at-point)
                              (define-key map (kbd "M-;") 'shipit-dwim)
                              map)))
           ;; Apply background highlighting for inline comments if configured
           (background-face (when (and file-path shipit-inline-comment-highlight-background)
                             `(:background ,shipit-inline-comment-highlight-background)))
           ;; Check if body contains details blocks
           (has-details (string-match-p "<details>" raw-body)))
      ;; Use details-aware rendering if details blocks present, otherwise use normal rendering
      (if has-details
          ;; For details blocks, insert directly with magit-sections
          ;; (shipit--insert-body-with-details will handle table conversion internally)
          (shipit--insert-body-with-details raw-body indent-level)
        ;; For normal comments without details, insert the indented body directly
        ;; indented-body already includes proper indentation from shipit--render-comment-body
        (insert (concat indented-body "\n")))
      ;; Apply blockquote faces to preserve markdown styling in diff buffers
      (shipit--apply-blockquote-faces body-start (point))
      ;; Apply strikethrough faces for ~text~ patterns
      (shipit--apply-strikethrough-faces body-start (point))
      ;; Apply code block background overlays (must be done after insertion)
      (shipit--apply-code-block-backgrounds-in-region body-start (point))
      ;; Create overlays for PR references, commit SHAs, and user mentions in the comment body
      (when (and repo pr-number)
        (shipit--create-pr-reference-overlays repo pr-number body-start (point))
        (shipit--create-commit-sha-overlays repo body-start (point))
        (shipit--create-user-mention-overlays repo body-start (point)))
      ;; Jira mention overlays (doesn't need repo/pr context)
      (shipit--create-jira-mention-overlays body-start (point))
      ;; Custom URL patterns (doesn't need repo/pr context)
      (shipit--create-custom-url-overlays body-start (point))
      ;; Generic URL overlays (catch-all for any remaining URLs)
      (shipit--create-generic-url-overlays body-start (point))
      ;; Add shipit properties to the entire comment body
      (add-text-properties body-start (point)
                           `(shipit-comment t
                                            shipit-comment-id ,(cdr (assq 'id comment))
                                            shipit-comment-body ,raw-body
                                            shipit-comment-body-text t
                                            shipit-comment-type ,comment-type
                                            ,@(when background-face `(face ,background-face))
                                            ,@(when file-path `(shipit-file-path ,file-path))
                                            ,@(when repo `(shipit-repo ,repo))
                                            ,@(when pr-number `(shipit-pr-number ,pr-number))
                                            ,@(when line-number `(shipit-line-number ,line-number))
                                            keymap ,body-keymap
                                            help-echo ,(if (and file-path repo pr-number)
                                                          "RET/SPC: open file diff, M-; interact with comment"
                                                        "Press M-; or RET to interact with this comment")))
      ;; Add suggestion text properties for the `a' keybind
      (shipit--add-suggestion-text-properties body-start (point) comment))
    ;; Insert reactions for general comments (is-inline = nil) with shipit properties (always shows with placeholder)
    ;; Skip if caller will handle reactions separately
    (unless skip-reactions
      (let ((reactions (shipit--format-comment-reactions comment nil)))
        (when reactions
          (let ((reactions-start (point)))
            (insert (concat "\n" body-indent-str reactions "\n"))
            (add-text-properties reactions-start (point)
                                 `(shipit-reactions t
                                                    shipit-comment-id ,(cdr (assq 'id comment))))))))))

(defun shipit--insert-simple-comment (comment indent-level)
  "Insert a simple comment with minimal properties to avoid magit corruption."
  (let* ((user-obj (cdr (assq 'user comment)))
         (user (or (cdr (assq 'login user-obj)) "Unknown User"))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (raw-body (shipit--clean-text (or (cdr (assq 'body comment)) "")))
         (body (shipit--clean-comment-text raw-body))
         (tables-converted (shipit--convert-html-tables-to-markdown body))
         (created (or (cdr (assq 'created_at comment))
                      (cdr (assq 'submitted_at comment))
                      (cdr (assq 'updated_at comment))))
         (formatted-timestamp (if created (shipit--format-timestamp created) "unknown time"))
         (comment-type (cdr (assq 'shipit-comment-type comment)))
         (review-state (cdr (assq 'review-state comment)))
         (rendered-body (if shipit-render-markdown
                            (shipit--render-markdown tables-converted)
                          tables-converted))
         ;; Provide default message for empty review states (if enabled)
         (final-body (cond
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "APPROVED")
                            (or (not rendered-body) (string-empty-p rendered-body)))
                       "✅ Approved this pull request")
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "CHANGES_REQUESTED")
                            (or (not rendered-body) (string-empty-p rendered-body)))
                       "❌ Requested changes on this pull request")
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "DISMISSED")
                            (or (not rendered-body) (string-empty-p rendered-body)))
                       "🚫 Dismissed previous review")
                      (t rendered-body)))
         (wrapped-body (shipit--wrap-text (or final-body "") (- 72 indent-level)))
         (indent-str (make-string indent-level ?\s))
         (body-indent-str (make-string (+ indent-level 2) ?\s))
         (indented-body (shipit--clean-text
                          (replace-regexp-in-string "\n" (concat "\n" body-indent-str) (or wrapped-body "")))))
    ;; Insert comment header with restored properties (safe now that manual refresh is fixed)
    (let* ((header-icon (cond
                         ((string= comment-type "review") (shipit--get-comment-type-icon "review" "📝"))
                         (t (shipit--get-comment-type-icon "comment" "💬"))))
           (header-prefix (cond
                           ((string= review-state "APPROVED") (concat (shipit--get-approval-status-icon "APPROVED" "✅") " "))
                           ((string= review-state "CHANGES_REQUESTED") (concat (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌") " "))
                           ((string= review-state "DISMISSED") (concat (shipit--get-approval-status-icon "DISMISSED" "🚫") " DISMISSED "))
                           ((string= comment-type "review") "💬 ")
                           (t "")))
           (header-face (cond
                         ((string= review-state "APPROVED") '(:foreground "green" :weight bold))
                         ((string= review-state "CHANGES_REQUESTED") '(:foreground "red" :weight bold))
                         ((string= review-state "DISMISSED") '(:foreground "gray" :weight bold))
                         ((string= comment-type "review") '(:foreground "blue" :weight bold))
                         (t nil)))
           (header-start (point))
           (comment-id (cdr (assq 'id comment)))
           ;; Create keymap for RET binding
           (comment-keymap (let ((map (make-sparse-keymap)))
                             (set-keymap-parent map (current-local-map))
                             (define-key map [return]
                                         (lambda ()
                                           (interactive)
                                           (shipit-dwim)))
                             map)))
      ;; Debug avatar extraction
      (when (and (boundp 'shipit-debug-logging-enabled) shipit-debug-logging-enabled)
        (shipit--debug-log "AVATAR-RENDER: Comment ID %s [%s] user=%s, avatar=%s"
                           (or comment-id "nil") (or comment-type "regular") user
                           (or avatar-url "MISSING")))
      ;; Insert header text with avatar
      (insert (format "%s%s %s%s%s %s\n"
                      indent-str
                      header-icon
                      header-prefix
                      (if shipit-show-avatars
                          (concat (shipit--create-avatar-display user avatar-url 16) " ")
                        "")
                      (propertize user 'face 'shipit-username-face)
                      (propertize formatted-timestamp 'face 'shipit-timestamp-face)))
      ;; Add styling and shipit properties to entire header line
      (add-text-properties header-start (point)
                           `(face ,header-face
                                  shipit-comment t
                                  shipit-comment-id ,comment-id
                                  shipit-comment-body ,raw-body
                                  shipit-comment-type ,comment-type
                                  keymap ,comment-keymap
                                  
                                  help-echo "Press M-; or RET to interact with this comment")))
    ;; Insert comment body with text properties for M-; and RET functionality
    (let ((body-start (point))
          (body-keymap (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map (current-local-map))
                         (define-key map [return]
                                     (lambda ()
                                       (interactive)
                                       (shipit-dwim)))
                         map)))
      (insert (format "%s%s\n" body-indent-str indented-body))
      ;; Apply code block background overlays (must be done after insertion)
      (shipit--apply-code-block-backgrounds-in-region body-start (point))
      ;; Add shipit properties to the entire comment body
      (add-text-properties body-start (point)
                           `(shipit-comment t
                                            shipit-comment-id ,(cdr (assq 'id comment))
                                            shipit-comment-body ,raw-body
                                            shipit-comment-body-text t
                                            shipit-comment-type ,comment-type
                                            ,@(when file-path `(shipit-file-path ,file-path))
                                            ,@(when repo `(shipit-repo ,repo))
                                            ,@(when pr-number `(shipit-pr-number ,pr-number))
                                            ,@(when line-number `(shipit-line-number ,line-number))
                                            keymap ,body-keymap
                                            help-echo ,(if (and file-path repo pr-number)
                                                          "RET/SPC: open file diff, M-; interact with comment"
                                                        "Press M-; or RET to interact with this comment"))))
    ;; Insert reactions for general comments (is-inline = nil) with shipit properties (always shows with placeholder)
    (let ((reactions (shipit--format-comment-reactions comment nil)))
      (when reactions
        (let ((reactions-start (point))
              (reactions-keymap (let ((map (make-sparse-keymap)))
                                  (set-keymap-parent map (current-local-map))
                                  (define-key map [return] 'shipit--file-at-point)
                                  (define-key map (kbd "SPC") 'shipit--file-at-point)
                                  (define-key map (kbd "M-;")
                                    (lambda ()
                                      (interactive)
                                      (shipit--simple-comment-dialog pr-number repo)))
                                  map)))
          (insert (concat body-indent-str (substring reactions 3) "\n")) ; Remove built-in 3 spaces since we're adding our own
          ;; Add shipit properties to reactions area too
          (add-text-properties reactions-start (point)
                               `(shipit-reactions t
                                                shipit-comment t
                                                shipit-comment-id ,(cdr (assq 'id comment))
                                                shipit-comment-body ,raw-body
                                                shipit-comment-type ,comment-type
                                                shipit-file-path ,file-path
                                                shipit-repo ,repo
                                                shipit-pr-number ,pr-number
                                                keymap ,reactions-keymap
                                                local-map ,reactions-keymap
                                                help-echo "RET/SPC: open file diff, M-; interact with comment")))))))


(defun shipit--avatar-needs-refresh-p (cache-file username)
  "Return non-nil if avatar for USERNAME needs a network refresh.
Returns nil when CACHE-FILE exists and the ETag timestamp is less than 1 hour old."
  (if (file-exists-p cache-file)
      (let* ((etag-key (format "avatar:%s" username))
             (entry (gethash etag-key shipit-gh-etag--persistent-cache))
             (timestamp (when entry (plist-get entry :timestamp)))
             (age (when timestamp (- (float-time) timestamp))))
        (or (null age) (> age 3600)))
    t))

(defun shipit--download-and-cache-avatar (avatar-url username &optional make-round)
  "Download and cache user avatar from AVATAR-URL for USERNAME using ETag system.
If MAKE-ROUND is t, create a round version. Returns local file path if successful, nil otherwise."
  (when (and avatar-url username)
    (let* ((cache-dir (expand-file-name "shipit-avatars" user-emacs-directory))
           (file-extension (if (string-match-p "\\.png" avatar-url) ".png" ".jpg"))
           (cache-file (expand-file-name
                        (format "%s%s%s" username
                                (if make-round "-round" "")
                                (if make-round ".png" file-extension))
                        cache-dir))
           (original-cache-file (expand-file-name (format "%s%s" username file-extension) cache-dir)))

      ;; Create cache directory if it doesn't exist
      (unless (file-directory-p cache-dir)
        (make-directory cache-dir t))

      ;; Use ETag-based downloading for efficient caching — skip if recently fetched
      (when (shipit--avatar-needs-refresh-p original-cache-file username)
        (shipit--download-avatar-with-etag avatar-url original-cache-file username))

      ;; If we want round avatar and original exists, create round version
      (when (and make-round (file-exists-p original-cache-file))
        (unless (file-exists-p cache-file)
          (shipit--create-round-avatar original-cache-file cache-file)))

      (when (file-exists-p (if make-round cache-file original-cache-file))
        (if make-round cache-file original-cache-file)))))

(defun shipit--download-avatar-with-etag (avatar-url cache-file username)
  "Download avatar using ETag system for efficient caching.
Downloads AVATAR-URL to CACHE-FILE for USERNAME, using ETags to avoid unnecessary downloads."
  (condition-case err
      (let ((avatar-data (shipit--fetch-binary-with-etag avatar-url username)))
        (when avatar-data
          (with-temp-file cache-file
            (set-buffer-multibyte nil)
            (insert avatar-data))
          (shipit--debug-log "Avatar downloaded for user: %s (size: %d bytes)" username (length avatar-data))))
    (error
     (shipit--debug-log "Failed to download avatar for %s: %s" username err)
     nil)))

(defun shipit--fetch-binary-with-etag (url cache-key)
  "Fetch binary data from URL using ETag system with CACHE-KEY.
Returns binary data as string, or nil if unchanged (304) or error."
  (let* ((headers '(("User-Agent" . "shipit.el")))
         (etag-cache-key (format "avatar:%s" cache-key))
         (cached-entry (gethash etag-cache-key shipit-gh-etag--persistent-cache))
         (cached-etag (when cached-entry (plist-get cached-entry :etag)))
         (request-headers (if cached-etag
                             (cons `("If-None-Match" . ,cached-etag) headers)
                           headers)))

    (shipit--debug-log "Fetching avatar with ETag: %s (cached: %s)" url (or cached-etag "none"))

    (let ((response (shipit--http-request-binary url request-headers)))
      (pcase (plist-get response :status)
        (200
         ;; New or updated content
         (let ((etag (plist-get response :etag))
               (data (plist-get response :data)))
           (when etag
             (puthash etag-cache-key (list :etag etag :timestamp (float-time))
                      shipit-gh-etag--persistent-cache))
           (shipit--debug-log "Avatar fetched: %s (size: %d, ETag: %s)"
                             url (length data) (or etag "none"))
           data))
        (304
         ;; Not modified - update/create timestamp so TTL cache knows we checked recently
         ;; Entry may be nil when url.el handles ETag transparently
         (puthash etag-cache-key
                  (if cached-entry
                      (plist-put cached-entry :timestamp (float-time))
                    (list :etag nil :timestamp (float-time)))
                  shipit-gh-etag--persistent-cache)
         (shipit--debug-log "Avatar not modified: %s" url)
         nil)
        (_
         ;; Error
         (shipit--debug-log "Avatar fetch failed: %s (status: %s)"
                           url (plist-get response :status))
         nil)))))

(defun shipit--http-request-binary (url headers)
  "Make HTTP request for binary data. Returns plist with :status, :data, :etag."
  (let ((url-request-extra-headers headers)
        (url-request-method "GET"))
    (with-current-buffer (url-retrieve-synchronously url t t 30)
      (goto-char (point-min))
      (let ((status-line (buffer-substring-no-properties (point) (line-end-position)))
            (status-code nil)
            (etag nil)
            (data nil))

        ;; Parse HTTP status
        (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
          (setq status-code (string-to-number (match-string 1 status-line))))

        ;; Find headers section
        (when (search-forward "\n\n" nil t)
          ;; Extract ETag from headers
          (save-excursion
            (goto-char (point-min))
            (when (search-forward "\n\n" nil t)
              (goto-char (point-min))
              (while (and (not (looking-at "^\r?$")) (zerop (forward-line 1)))
                (when (looking-at "^[Ee][Tt]ag: *\"?\\([^\"]+\\)\"?")
                  (setq etag (match-string 1))))))

          ;; Extract binary data
          (when (eq status-code 200)
            (setq data (buffer-substring-no-properties (point) (point-max)))))

        (kill-buffer (current-buffer))
        `(:status ,status-code :data ,data :etag ,etag)))))

(defun shipit--imagemagick-available-p ()
  "Check if ImageMagick convert command is available."
  (executable-find "convert"))


;;;###autoload
(defun shipit-check-dependencies ()
  "Check if optional dependencies for shipit features are available."
  (interactive)
  (let ((imagemagick-available (shipit--imagemagick-available-p)))
    (message "=== SHIPIT DEPENDENCIES CHECK ===")
    (message "ImageMagick (convert): %s" (if imagemagick-available "✅ Available" "❌ Not found"))
    (message "Round avatars: %s" (if (and shipit-round-avatars imagemagick-available)
                                     "✅ Enabled and working"
                                   (if shipit-round-avatars
                                       "⚠ Enabled but ImageMagick missing"
                                     "⚪ Disabled (square avatars)")))
    (when (and shipit-round-avatars (not imagemagick-available))
      (message "💡 To enable round avatars: install ImageMagick or run 'apt install imagemagick'"))
    (message "================================")
    imagemagick-available))

(defun shipit--create-round-avatar (input-file output-file)
  "Create a round version of INPUT-FILE and save to OUTPUT-FILE using ImageMagick if available."
  (if (shipit--imagemagick-available-p)
      (condition-case err
          (progn
            ;; Create round avatar with transparent background using mask approach
            (call-process "convert" nil nil nil
                          input-file
                          "-resize" "20x20^"
                          "-gravity" "center"
                          "-crop" "20x20+0+0"
                          "+repage"
                          "(" "-size" "20x20" "xc:black" "-fill" "white" "-draw" "circle 10,10 10,0" ")"
                          "-alpha" "off"
                          "-compose" "CopyOpacity" "-composite"
                          "-background" "none"
                          "-compose" "Over" "-flatten"
                          output-file))
        (error
         (message "shipit: ImageMagick round avatar creation failed: %s" err)
         ;; If ImageMagick fails, just copy the original
         (copy-file input-file output-file t)))
    ;; ImageMagick not available - just copy original
    (copy-file input-file output-file t)
    ;; Only warn once per session
    (when (and shipit-round-avatars (not (get 'shipit-round-avatars 'warning-shown)))
      (message "shipit: ImageMagick not found. Using square avatars. Run M-x shipit-check-dependencies for info.")
      (put 'shipit-round-avatars 'warning-shown t))))


(defun shipit--create-round-avatar-mask (size)
  "Create a round mask for avatar images of SIZE."
  (let* ((mask-data (make-string (* size size) 0))
         (center (/ size 2.0))
         (radius (- center 1)))
    ;; Create circular mask data
    (dotimes (y size)
      (dotimes (x size)
        (let* ((dx (- x center))
               (dy (- y center))
               (distance (sqrt (+ (* dx dx) (* dy dy))))
               (index (+ x (* y size))))
          (when (< distance radius)
            (aset mask-data index 255)))))
    mask-data))

(defun shipit--find-cached-avatar (username)
  "Find existing cached avatar file for USERNAME. Returns file path if found, nil otherwise."
  (when username
    (let* ((cache-dir (expand-file-name "shipit-avatars" user-emacs-directory))
           (round-file (expand-file-name (format "%s-round.png" username) cache-dir))
           (square-file (expand-file-name (format "%s.jpg" username) cache-dir))
           (square-png-file (expand-file-name (format "%s.png" username) cache-dir)))
      (cond
       ((and shipit-round-avatars (file-exists-p round-file)) round-file)
       ((file-exists-p square-file) square-file)
       ((file-exists-p square-png-file) square-png-file)
       (t nil)))))

(defun shipit--create-avatar-display (username avatar-url &optional size)
  "Create a small round avatar image display for USERNAME from AVATAR-URL.
SIZE defaults to 20 pixels."
  (let ((size (or size 20)))
    (if (display-images-p)
        (let ((cached-file (if avatar-url
                               ;; If we have avatar-url, download/cache normally
                               (shipit--download-and-cache-avatar avatar-url username shipit-round-avatars)
                             ;; If no avatar-url, check if we have a cached version
                             (shipit--find-cached-avatar username))))
          (if cached-file
              (condition-case err
                  (let ((image (create-image cached-file nil nil
                                             :width size :height size
                                             :ascent 'center
                                             :relief 0
                                             :margin 0)))
                    (propertize " " 'display image 'rear-nonsticky '(display)))
                (error
                 "👤"))
            "👤"))
      "👤")))

(defun shipit--detect-image-type-from-content (file)
  "Detect image type from FILE content by reading magic bytes.
Returns symbol like `png', `jpeg', `gif', `webp', or nil if unknown."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 16)
        (let ((bytes (buffer-string)))
          (cond
           ;; PNG: 89 50 4E 47 0D 0A 1A 0A
           ((and (>= (length bytes) 8)
                 (equal (substring bytes 0 8) "\x89PNG\r\n\x1a\n"))
            'png)
           ;; JPEG: FF D8 FF
           ((and (>= (length bytes) 3)
                 (equal (substring bytes 0 3) "\xff\xd8\xff"))
            'jpeg)
           ;; GIF: GIF87a or GIF89a
           ((and (>= (length bytes) 6)
                 (or (equal (substring bytes 0 6) "GIF87a")
                     (equal (substring bytes 0 6) "GIF89a")))
            'gif)
           ;; WebP: RIFF....WEBP
           ((and (>= (length bytes) 12)
                 (equal (substring bytes 0 4) "RIFF")
                 (equal (substring bytes 8 12) "WEBP"))
            'webp)
           (t nil))))
    (error nil)))

(defvar shipit--image-base-repo nil
  "Repo name for resolving relative image URLs during rendering.")

(defun shipit--create-image-display (url alt-text)
  "Create an image display for URL with ALT-TEXT."
  ;; Strip markdown backslash escapes (e.g. \_ → _) from URLs
  (setq url (replace-regexp-in-string "\\\\\\([_*()~>#+.!|-]\\)" "\\1" url))
  ;; Resolve relative URLs against repo raw content URL
  (when (and (not (string-match-p "\\`https?://" url))
             (boundp 'shipit--image-base-repo)
             shipit--image-base-repo)
    (setq url (format "https://raw.githubusercontent.com/%s/HEAD/%s"
                      shipit--image-base-repo url)))
  (let ((cached-file (shipit--download-and-cache-image url)))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "IMAGE-DISPLAY: url=%s cached-file=%s display-images-p=%s"
                         url cached-file (display-images-p)))
    (if cached-file
        (condition-case err
            ;; Detect image type from content, not filename
            (let* ((image-type (or (shipit--detect-image-type-from-content cached-file)
                                   (image-type-from-file-name cached-file)))
                   (col-width (when (and (boundp 'shipit--image-base-repo)
                                         shipit--image-base-repo)
                                (* 80 (frame-char-width))))
                   (max-w (or col-width
                              shipit-image-max-width
                              (window-body-width nil t)))
                   (max-h (or shipit-image-max-height
                              (window-body-height nil t)))
                   (image (create-image cached-file image-type nil
                                        :max-width max-w
                                        :max-height max-h)))
              (when (fboundp 'shipit--debug-log)
                (shipit--debug-log "IMAGE-DISPLAY: Created image type=%s" image-type))
              (if (display-images-p)
                  (let ((image-string (propertize " " 'display image 'rear-nonsticky '(display))))
                    (when (fboundp 'shipit--debug-log)
                      (shipit--debug-log "IMAGE-DISPLAY: Returning image with display property"))
                    (concat "\n[Image: " alt-text "]\n" image-string "\n"))
                (progn
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "IMAGE-DISPLAY: display-images-p is nil, showing URL"))
                  (format "\n[Image: %s - %s]\n" alt-text url))))
          (error
           (when (fboundp 'shipit--debug-log)
             (shipit--debug-log "IMAGE-DISPLAY: Error loading image: %S" err))
           (format "\n[Image: %s - Error loading: %s]\n" alt-text (error-message-string err))))
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log "IMAGE-DISPLAY: Download failed for %s" url))
      (format "\n[Image: %s - Download failed]\n" alt-text))))

(defun shipit--strip-inline-html-tags (text)
  "Strip safe inline HTML tags from TEXT, preserving their content.
Only removes <kbd> tags which have no markdown equivalent and add noise
in text buffers.  Other formatting tags (<b>, <em>, <del>, etc.) are
preserved since they carry meaningful emphasis or semantic information."
  (replace-regexp-in-string "</?kbd>" "" text))

(defun shipit--convert-inline-html (text)
  "Convert inline HTML tags in TEXT to markdown equivalents.
Handles <hr>, <em>, <a href>, <strong>, and <br> tags.
Process <a> first so <em>/<strong> can wrap the converted links."
  (let ((result text))
    ;; <hr /> or <hr> -> markdown horizontal rule
    (setq result (replace-regexp-in-string
                  "<hr[[:space:]]*/?>\\s-*" (concat (propertize (make-string 40 ?\x2500) 'face 'shadow) "\n") result))
    ;; <br /> or <br> -> newline
    (setq result (replace-regexp-in-string
                  "<br[[:space:]]*/?>\\s-*" "\n" result))
    ;; <a href> FIRST so <em>/<strong> can wrap the converted links
    (setq result (replace-regexp-in-string
                  "<a[[:space:]]+href=['\"]\\([^'\"]*\\)['\"][^>]*>\\([^<]*\\)</a>"
                  "[\\2](\\1)" result))
    ;; <strong>text</strong> -> **text** (may contain converted links)
    (setq result (replace-regexp-in-string
                  "<strong>\\(\\(?:.\\|\n\\)*?\\)</strong>" "**\\1**" result))
    ;; <em>text</em> -> *text* (may contain converted links)
    (setq result (replace-regexp-in-string
                  "<em>\\(\\(?:.\\|\n\\)*?\\)</em>" "*\\1*" result))
    result))

(defun shipit--escape-underscores-in-table-cells (text)
  "Escape underscores in markdown table cells to prevent emphasis rendering."
  (let ((lines (split-string text "\n"))
        (result '()))
    (dolist (line lines)
      (if (string-match-p "^|.*|$" line)
          ;; This is a table row
          (if (string-match-p "^|[ ]*-" line)
              ;; Separator row - don't escape
              (push line result)
            ;; Data/header row - escape all underscores
            (push (replace-regexp-in-string "_" "\\\\_" line) result))
        ;; Not a table row - leave unchanged
        (push line result)))
    (mapconcat 'identity (nreverse result) "\n")))

(defun shipit--align-markdown-tables-with-pandoc (text)
  "Align markdown tables in TEXT using pandoc if available.
Only invokes pandoc when TEXT contains an actual markdown table
separator line (a line starting with `|' that contains only spaces,
pipes, colons, and at least one dash — e.g. `|---|---|',
`| --- | --- |', `|:---:|---:|').  Plain comments with stray pipes
no longer trigger a pandoc subprocess."
  (if (and (stringp text)
           (string-match-p "^\\s-*|[ \t|:-]*-[ \t|:-]*$" text)
           (executable-find "pandoc"))
      (condition-case err
          (with-temp-buffer
            (insert text)
            (let ((exit-code (shell-command-on-region
                             (point-min) (point-max)
                             "pandoc -f gfm -t gfm --columns=1000"
                             t t)))
              (if (= exit-code 0)
                  (let* ((aligned (buffer-string))
                         ;; Replace \- with "- " (dash + space) to preserve column width
                         (cleaned (replace-regexp-in-string "\\\\-" "- " aligned)))
                    ;; Escape underscores in table cells to prevent italic rendering
                    (shipit--escape-underscores-in-table-cells cleaned))
                text)))
        (error
         (when (fboundp 'shipit--debug-log)
           (shipit--debug-log "Pandoc table alignment failed: %s" err))
         text))
    text))

(defun shipit--escape-mid-word-underscores (text)
  "Escape underscores in the middle of words to prevent italic rendering.
Leaves underscores at word boundaries for emphasis (e.g., _italic_).
Does NOT escape underscores inside backtick-delimited code (e.g., `foo_bar`)."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Find all backtick-delimited regions and mark them
    (let ((protected-regions '()))
      ;; Find inline code spans (single backticks)
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
        (push (cons (match-beginning 0) (match-end 0)) protected-regions))
      ;; Find code blocks (triple backticks)
      (goto-char (point-min))
      (while (re-search-forward "```[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)```" nil t)
        (push (cons (match-beginning 0) (match-end 0)) protected-regions))
      ;; Now escape underscores that are NOT in protected regions
      (goto-char (point-min))
      (while (re-search-forward "\\(\\w\\)_\\(\\w\\)" nil t)
        (let ((underscore-pos (1- (point))))
          ;; Check if this underscore is in a protected region
          (unless (cl-some (lambda (region)
                             (and (>= underscore-pos (car region))
                                  (<= underscore-pos (cdr region))))
                           protected-regions)
            ;; Not protected - escape it
            (replace-match "\\1\\\\_\\2" t nil))))
      (buffer-string))))

(defun shipit--convert-html-lists-to-markdown (text)
  "Convert HTML list elements to markdown format in TEXT.
Handles <ol> with numbered items and <ul>/<li> with bullet points."
  (let ((result text))
    ;; Convert <ol>...</ol> blocks: number each <li> inside
    (while (string-match "<ol>" result)
      (let* ((ol-start (match-beginning 0))
             (ol-end (string-match "</ol>" result ol-start))
             (ol-end (if ol-end (+ ol-end 5) (length result)))
             (before (substring result 0 ol-start))
             (block (substring result (+ ol-start 4) (- ol-end 5)))
             (after (substring result ol-end))
             (counter 0)
             ;; Replace </li> with newline, then number each <li>
             (items (replace-regexp-in-string "</li>" "\n" block))
             (numbered (replace-regexp-in-string
                        "<li>"
                        (lambda (_match)
                          (setq counter (1+ counter))
                          (format "%d. " counter))
                        items)))
        (setq result (concat before numbered after))))
    ;; Convert remaining <ul>...</ul> and standalone <li> to "- "
    (setq result (replace-regexp-in-string "</?ul>" "" result))
    (setq result (replace-regexp-in-string "<li>" "- "
                   (replace-regexp-in-string "</li>" "\n" result)))
    result))

(defun shipit--auto-increment-ordered-lists (text)
  "Auto-increment ordered list numbers in TEXT.
Markdown allows all items to be numbered `1.` and renderers auto-number.
Emacs markdown-mode does not auto-number, so we pre-process the text.
Handles nested lists by tracking indent levels independently."
  (if (or (not text) (string-empty-p text))
      (or text "")
    (let ((lines (split-string text "\n"))
          (result '())
          ;; Stack of (indent-level . counter) for nested lists
          (counters '()))
      (dolist (line lines)
        (if (string-match "\\`\\([ \t]*\\)\\([0-9]+\\)\\. " line)
            (let* ((indent (match-string 1 line))
                   (indent-len (length indent))
                   (rest-of-line (substring line (match-end 0))))
              ;; Pop counters for deeper or equal indent that no longer apply
              (while (and counters (> (caar counters) indent-len))
                (setq counters (cdr counters)))
              ;; Find or create counter for this indent level
              (if (and counters (= (caar counters) indent-len))
                  ;; Increment existing counter
                  (setcdr (car counters) (1+ (cdar counters)))
                ;; New indent level — start at 1
                (push (cons indent-len 1) counters))
              (push (format "%s%d. %s" indent (cdar counters) rest-of-line) result))
          ;; Non-list line — reset counters if blank line (list break)
          (when (string-match-p "\\`[ \t]*\\'" line)
            (setq counters nil))
          (push line result)))
      (mapconcat #'identity (nreverse result) "\n"))))

(defun shipit--process-inline-images (text)
  "Process inline images in TEXT and replace them with image displays.
Handles both HTML <img> tags and markdown ![alt](url) syntax."
  (if (not shipit-render-images)
      text
    (let ((result text))
      ;; Process HTML <img> tags
      ;; Use LITERAL=t to prevent backslash interpretation in replacement
      (setq result
            (replace-regexp-in-string
             "<img[^>]+>"
             (lambda (img-tag)
               (save-match-data
                 (let ((src (when (string-match "src=\"\\([^\"]+\\)\"" img-tag)
                              (match-string 1 img-tag)))
                       (alt (when (string-match "alt=\"\\([^\"]*\\)\"" img-tag)
                              (match-string 1 img-tag))))
                   (if src
                       (shipit--create-image-display src (or alt "image"))
                     img-tag))))
             result nil t))
      ;; Process linked images [![alt](img-url)](link-url) BEFORE plain images
      ;; so the inner ![alt](url) doesn't match first and break the outer link
      (setq result
            (replace-regexp-in-string
             "\\[!\\[\\([^]]*\\)\\](\\([^)]+\\))\\](\\([^)]+\\))"
             (lambda (match)
               (save-match-data
                 (string-match "\\[!\\[\\([^]]*\\)\\](\\([^)]+\\))\\](\\([^)]+\\))" match)
                 (let ((alt (match-string 1 match))
                       (url (match-string 2 match)))
                   (shipit--create-image-display url (or alt "image")))))
             result nil t))
      ;; Process markdown images ![alt](url)
      ;; Use LITERAL=t to prevent backslash interpretation in replacement
      (setq result
            (replace-regexp-in-string
             "!\\[\\([^]]*\\)\\](\\([^)]+\\))"
             (lambda (match)
               (save-match-data
                 (string-match "!\\[\\([^]]*\\)\\](\\([^)]+\\))" match)
                 (let ((alt (match-string 1 match))
                       (url (match-string 2 match)))
                   (shipit--create-image-display url (or alt "image")))))
             result nil t))
      result)))

(defvar shipit--language-mode-alist
  '(;; Lisp family
    ("elisp" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("lisp" . lisp-mode)
    ("clojure" . clojure-mode)
    ("clj" . clojure-mode)
    ("scheme" . scheme-mode)
    ("racket" . racket-mode)
    ;; Scripting languages
    ("python" . python-mode)
    ("py" . python-mode)
    ("ruby" . ruby-mode)
    ("rb" . ruby-mode)
    ("perl" . perl-mode)
    ("pl" . perl-mode)
    ("lua" . lua-mode)
    ("r" . ess-r-mode)
    ("julia" . julia-mode)
    ;; Web languages
    ("javascript" . js-mode)
    ("js" . js-mode)
    ("typescript" . typescript-mode)
    ("ts" . typescript-mode)
    ("tsx" . typescript-mode)
    ("jsx" . js-mode)
    ("json" . js-mode)
    ("jsonc" . js-mode)
    ("html" . html-mode)
    ("css" . css-mode)
    ("scss" . scss-mode)
    ("sass" . sass-mode)
    ("less" . less-css-mode)
    ;; Systems languages
    ("c" . c-mode)
    ("cpp" . c++-mode)
    ("c++" . c++-mode)
    ("cxx" . c++-mode)
    ("h" . c-mode)
    ("hpp" . c++-mode)
    ("rust" . rust-mode)
    ("rs" . rust-mode)
    ("go" . go-mode)
    ("golang" . go-mode)
    ("zig" . zig-mode)
    ;; JVM languages
    ("java" . java-mode)
    ("kotlin" . kotlin-mode)
    ("kt" . kotlin-mode)
    ("scala" . scala-mode)
    ("groovy" . groovy-mode)
    ;; .NET languages
    ("csharp" . csharp-mode)
    ("cs" . csharp-mode)
    ("fsharp" . fsharp-mode)
    ("fs" . fsharp-mode)
    ;; Shell
    ("sh" . sh-mode)
    ("bash" . sh-mode)
    ("shell" . sh-mode)
    ("zsh" . sh-mode)
    ("fish" . fish-mode)
    ("powershell" . powershell-mode)
    ("ps1" . powershell-mode)
    ;; Functional languages
    ("haskell" . haskell-mode)
    ("hs" . haskell-mode)
    ("ocaml" . tuareg-mode)
    ("ml" . tuareg-mode)
    ("erlang" . erlang-mode)
    ("elixir" . elixir-mode)
    ("ex" . elixir-mode)
    ("elm" . elm-mode)
    ;; Data/Config formats
    ("yaml" . yaml-mode)
    ("yml" . yaml-mode)
    ("toml" . toml-mode)
    ("ini" . conf-mode)
    ("conf" . conf-mode)
    ("xml" . xml-mode)
    ("sql" . sql-mode)
    ("graphql" . graphql-mode)
    ("gql" . graphql-mode)
    ;; Documentation
    ("markdown" . markdown-mode)
    ("md" . markdown-mode)
    ("org" . org-mode)
    ("rst" . rst-mode)
    ("tex" . latex-mode)
    ("latex" . latex-mode)
    ;; Other
    ("diff" . diff-mode)
    ("patch" . diff-mode)
    ("dockerfile" . dockerfile-mode)
    ("docker" . dockerfile-mode)
    ("makefile" . makefile-mode)
    ("make" . makefile-mode)
    ("cmake" . cmake-mode)
    ("nginx" . nginx-mode)
    ("terraform" . terraform-mode)
    ("tf" . terraform-mode)
    ("hcl" . hcl-mode)
    ("nix" . nix-mode)
    ("proto" . protobuf-mode)
    ("protobuf" . protobuf-mode)
    ;; Hardware description
    ("vhdl" . vhdl-mode)
    ("verilog" . verilog-mode)
    ("v" . verilog-mode)
    ("systemverilog" . verilog-mode)
    ("sv" . verilog-mode))
  "Alist mapping language identifiers to Emacs major modes.")

(defun shipit--strip-common-indent (text)
  "Strip the common leading whitespace from all lines in TEXT.
Returns a cons of (stripped-text . indent-string)."
  (let* ((lines (split-string text "\n"))
         (non-empty-lines (cl-remove-if (lambda (l) (string-match-p "^[ \t]*$" l)) lines))
         (min-indent (if non-empty-lines
                         (apply #'min
                                (mapcar (lambda (line)
                                          (if (string-match "^\\([ \t]*\\)" line)
                                              (length (match-string 1 line))
                                            0))
                                        non-empty-lines))
                       0))
         (indent-str (make-string min-indent ?\s))
         (stripped-lines (mapcar (lambda (line)
                                   (if (>= (length line) min-indent)
                                       (substring line min-indent)
                                     line))
                                 lines)))
    (cons (mapconcat #'identity stripped-lines "\n") indent-str)))

(defun shipit--restore-indent (text indent-str)
  "Restore INDENT-STR as leading whitespace to all lines in TEXT.
Only adds indent to non-empty lines to preserve original text length."
  (let ((lines (split-string text "\n")))
    (mapconcat (lambda (line)
                 (if (string-empty-p line)
                     line
                   (concat indent-str line)))
               lines "\n")))

(defun shipit--fontify-code-string (code-string language)
  "Fontify CODE-STRING using the major mode for LANGUAGE.
Returns the fontified string with face properties.
Strips common leading indentation before fontifying to ensure
language modes (especially diff-mode) work correctly."
  (let ((mode (cdr (assoc (downcase language) shipit--language-mode-alist))))
    (if (and mode (shipit--mode-available-p mode))
        (let* ((stripped-result (shipit--strip-common-indent code-string))
               (stripped-code (car stripped-result))
               (indent-str (cdr stripped-result)))
          (with-temp-buffer
            (insert stripped-code)
            (delay-mode-hooks (funcall mode))
            (font-lock-ensure)
            ;; Restore indentation to fontified result
            (shipit--restore-indent (buffer-string) indent-str)))
      ;; Return unmodified if mode not found
      code-string)))

(defun shipit--mode-available-p (mode)
  "Check if MODE is available (defined or autoloaded)."
  (or (fboundp mode)
      (and (symbolp mode)
           (autoloadp (indirect-function mode t)))))

(defun shipit--apply-code-block-syntax-highlighting (content-start content-end language)
  "Apply syntax highlighting to code block content between CONTENT-START and CONTENT-END.
LANGUAGE is the language identifier from the code fence."
  (shipit--debug-log "SYNTAX-HL: Called with language=%s start=%d end=%d" language content-start content-end)
  (when (and language (not (string-empty-p language)))
    (let ((mode (cdr (assoc (downcase language) shipit--language-mode-alist))))
      (shipit--debug-log "SYNTAX-HL: language=%s mode=%s available=%s"
                         language mode (and mode (shipit--mode-available-p mode)))
      (when (and mode (shipit--mode-available-p mode))
        (let* ((code-text (buffer-substring-no-properties content-start content-end))
               (fontified (shipit--fontify-code-string code-text language)))
          (shipit--debug-log "SYNTAX-HL: Fontifying %d chars of %s code" (length code-text) language)
          ;; Replace the region with fontified text
          (save-excursion
            (goto-char content-start)
            (delete-region content-start content-end)
            (insert fontified)))))))

(defun shipit--adjust-color-brightness (color amount)
  "Adjust COLOR brightness by AMOUNT (-1.0 to 1.0).
Positive AMOUNT lightens, negative darkens."
  (when color
    (let* ((rgb (color-name-to-rgb color))
           (r (nth 0 rgb))
           (g (nth 1 rgb))
           (b (nth 2 rgb)))
      (if (> amount 0)
          (format "#%02x%02x%02x"
                  (round (* 255 (+ r (* (- 1 r) amount))))
                  (round (* 255 (+ g (* (- 1 g) amount))))
                  (round (* 255 (+ b (* (- 1 b) amount)))))
        (format "#%02x%02x%02x"
                (round (* 255 (* r (+ 1 amount))))
                (round (* 255 (* g (+ 1 amount))))
                (round (* 255 (* b (+ 1 amount)))))))))

(defun shipit--nested-code-block-bg (base-bg)
  "Compute background color for nested code blocks.
Lightens dark backgrounds, darkens light backgrounds."
  (when base-bg
    (let* ((rgb (color-name-to-rgb base-bg))
           (luminance (+ (* 0.2126 (nth 0 rgb))
                         (* 0.7152 (nth 1 rgb))
                         (* 0.0722 (nth 2 rgb)))))
      (if (< luminance 0.5)
          (shipit--adjust-color-brightness base-bg 0.04)
        (shipit--adjust-color-brightness base-bg -0.04)))))

(defun shipit--point-in-code-block-overlay-p (pos)
  "Return t if POS is inside a shipit-code-block overlay."
  (cl-some (lambda (ov) (overlay-get ov 'shipit-code-block))
           (overlays-at pos)))

(defun shipit--apply-code-block-backgrounds-in-region (start end)
  "Apply background color overlays and syntax highlighting to code blocks.
Detects both triple-backtick fenced code blocks and indented code blocks
between START and END. Uses overlays with :extend t to fill full line width.
If a language is specified after ```, applies syntax highlighting.
Must be called AFTER text is inserted into the final buffer."
  ;; Guard against invalid region bounds (can happen during async refresh)
  (when (and start end (< start end) (<= end (point-max)))
    (condition-case err
      (let* ((explicit-bg (and (boundp 'shipit-code-block-background)
                               shipit-code-block-background))
             ;; Get background from markdown-code-face if no explicit setting
             (markdown-bg (and (not explicit-bg)
                               (facep 'markdown-code-face)
                               (face-background 'markdown-code-face nil 'default)))
             ;; Always adjust the background slightly to distinguish from magit-section-highlight
             ;; which may have the same background as markdown-code-face
             ;; Lighten for dark themes, darken for light themes
             (base-bg (or explicit-bg markdown-bg))
             (code-block-bg (when base-bg
                              (let* ((rgb (color-name-to-rgb base-bg))
                                     (luminance (+ (* 0.2126 (nth 0 rgb))
                                                   (* 0.7152 (nth 1 rgb))
                                                   (* 0.0722 (nth 2 rgb)))))
                                (shipit--adjust-color-brightness base-bg
                                                                 (if (< luminance 0.5) 0.04 -0.04)))))
             (nested-block-bg (shipit--nested-code-block-bg code-block-bg))
             (found-fenced 0)
             (found-indented 0)
             (found-org 0))
        (shipit--debug-log "CODE-BLOCK-BG: Searching region %d-%d, explicit-bg=%s, markdown-bg=%s, code-block-bg=%s"
                           start end explicit-bg markdown-bg code-block-bg)
        (save-excursion
          (goto-char start)
          ;; First pass: Find all triple-backtick code blocks (with optional leading whitespace)
          ;; Use [ \t]* instead of \s* because \s* is syntax-class dependent
          ;; Allow optional whitespace between ``` and language (gfm-mode can insert spaces)
          (while (re-search-forward "^\\([ \t]*\\)```[ \t]*\\([a-zA-Z0-9_+-]*\\)" end t)
            (setq found-fenced (1+ found-fenced))
            (let* ((indent (match-string 1))
                   (language (match-string 2))
                   (block-start (line-beginning-position))
                   (content-start (1+ (line-end-position))))  ; Start after the ``` line
              (shipit--debug-log "CODE-BLOCK-BG: Found fenced block, language='%s' indent='%s' block-start=%d"
                                 language indent block-start)
              ;; Check if this is a single-line code block (closing backticks on same line)
              (if (re-search-forward "```" (line-end-position) t)
                  ;; Single-line code block - just apply background
                  (when code-block-bg
                    (let* ((block-end (min (1+ (line-end-position)) end))
                           (ov (make-overlay block-start block-end)))
                      (overlay-put ov 'priority 10)
                      (overlay-put ov 'face `(:background ,code-block-bg :extend t))
                      (overlay-put ov 'shipit-code-block t)))
                ;; Multi-line code block - find closing backticks
                (end-of-line)
                ;; Allow any leading whitespace for closing - the indent captured from opening
                ;; may not match due to markdown-mode processing or comment body indentation
                (let ((close-pattern "^[ \t]*```"))
                  (if (re-search-forward close-pattern end t)
                      (let* ((content-end (line-beginning-position))  ; End before closing ```
                             (block-end (min (1+ (line-end-position)) end)))
                        (shipit--debug-log "CODE-BLOCK-BG: Multi-line block content-start=%d content-end=%d language='%s'"
                                           content-start content-end language)
                        ;; Apply syntax highlighting to code content
                        (when (and language (not (string-empty-p language))
                                   (< content-start content-end))
                          (shipit--apply-code-block-syntax-highlighting
                           content-start content-end language))
                        ;; Apply background overlay
                        (when code-block-bg
                          (let ((ov (make-overlay block-start block-end)))
                            (overlay-put ov 'priority 10)
                            (overlay-put ov 'face `(:background ,code-block-bg :extend t))
                            (overlay-put ov 'shipit-code-block t))))
                    ;; If closing backticks not found, style to end of region
                    (when code-block-bg
                      (let ((ov (make-overlay block-start end)))
                        (overlay-put ov 'priority 10)
                        (overlay-put ov 'face `(:background ,code-block-bg :extend t))
                        (overlay-put ov 'shipit-code-block t))))))))
          ;; Second pass: Find indented code blocks by looking for markdown-pre-face
          ;; These are created by markdown-mode for 4-space indented code
          ;; Skip positions already inside fenced code blocks
          (when code-block-bg
            (goto-char start)
            (let ((pos start)
                  (block-start nil))
              (while (< pos end)
                ;; Skip if already inside a fenced code block overlay
                (if (shipit--point-in-code-block-overlay-p pos)
                    (setq block-start nil)  ; Reset any pending block
                  (let ((face (get-text-property pos 'face)))
                    ;; Check if this position has markdown-pre-face
                    (if (or (eq face 'markdown-pre-face)
                            (and (listp face) (memq 'markdown-pre-face face)))
                        ;; In a code block - record start of line
                        (unless block-start
                          (save-excursion
                            (goto-char pos)
                            (setq block-start (line-beginning-position))))
                      ;; Not in a code block
                      (when block-start
                        ;; End of block - create overlay from block-start to end of last code line
                        (save-excursion
                          (goto-char pos)
                          ;; Go back to end of previous line (last line of code block)
                          (forward-line -1)
                          (let ((block-end (line-end-position)))
                            ;; Only create if non-empty
                            (when (< block-start block-end)
                              (setq found-indented (1+ found-indented))
                              (let ((ov (make-overlay block-start block-end)))
                                (overlay-put ov 'priority 10)
                                (overlay-put ov 'face `(:background ,code-block-bg :extend t))
                                (overlay-put ov 'shipit-code-block t)))))
                        (setq block-start nil)))))
                (setq pos (next-single-property-change pos 'face nil end)))
              ;; Handle block that extends to end of region
              (when (and block-start (< block-start end))
                (setq found-indented (1+ found-indented))
                (let ((ov (make-overlay block-start end)))
                  (overlay-put ov 'priority 10)
                  (overlay-put ov 'face `(:background ,code-block-bg :extend t))
                  (overlay-put ov 'shipit-code-block t)))))
          ;; Third pass: org-mode source blocks (#+begin_src LANG / #+end_src)
          (goto-char start)
          (while (re-search-forward
                  "^[ \t]*#\\+begin_src[ \t]+\\([a-zA-Z0-9_+-]+\\)[ \t]*$" end t)
            (let* ((language (match-string 1))
                   (block-start (line-beginning-position))
                   (content-start (1+ (line-end-position)))
                   (is-nested (shipit--point-in-code-block-overlay-p block-start))
                   (bg-color (if is-nested nested-block-bg code-block-bg)))
              (when (re-search-forward "^[ \t]*#\\+end_src[ \t]*$" end t)
                (let* ((content-end (line-beginning-position))
                       (block-end (1+ (line-end-position))))
                  (setq found-org (1+ found-org))
                  (when (< content-start content-end)
                    (shipit--apply-code-block-syntax-highlighting
                     content-start content-end language))
                  (when bg-color
                    (let ((ov (make-overlay block-start block-end)))
                      (overlay-put ov 'priority 15)
                      (overlay-put ov 'face `(:background ,bg-color :extend t))
                      (overlay-put ov 'shipit-code-block t)
                      (overlay-put ov 'shipit-nested-code-block is-nested)))))))
          (shipit--debug-log "CODE-BLOCK-BG: Found %d fenced, %d indented, %d org blocks"
                             found-fenced found-indented found-org)
          ;; Render mermaid diagrams after applying code block styling
          (shipit--render-mermaid-blocks-in-region start end)))
      (error
       (when (fboundp 'shipit--debug-log)
         (shipit--debug-log "Code block background application failed: %s" err))))))

(defun shipit--apply-code-block-backgrounds ()
  "Apply background color to code blocks in the entire current buffer.
Convenience wrapper around `shipit--apply-code-block-backgrounds-in-region'."
  (shipit--apply-code-block-backgrounds-in-region (point-min) (point-max)))

(defvar shipit--mmdc-available 'unknown
  "Cached mmdc availability.  One of `unknown', t, or nil.
Set to nil after the first failed render attempt so we don't
block the UI with repeated synchronous calls to a broken mmdc.")

(defvar shipit--mmdc-puppeteer-config nil
  "Path to the puppeteer config file used by mmdc.
Created on demand with `--no-sandbox' for Linux.")

(defun shipit--mmdc-puppeteer-config ()
  "Return path to a puppeteer config file for mmdc.
On Linux, Chrome needs `--no-sandbox' due to AppArmor restrictions."
  (unless (and shipit--mmdc-puppeteer-config
               (file-exists-p shipit--mmdc-puppeteer-config))
    (let ((cfg (expand-file-name "shipit-mmdc-puppeteer.json"
                                 temporary-file-directory)))
      (with-temp-file cfg
        (insert "{\"args\":[\"--no-sandbox\"]}"))
      (setq shipit--mmdc-puppeteer-config cfg)))
  shipit--mmdc-puppeteer-config)

(defun shipit--mmdc-call (input-file output-file &rest extra-args)
  "Call mmdc to render INPUT-FILE to OUTPUT-FILE.
Passes puppeteer config for sandbox workaround on Linux.
EXTRA-ARGS are appended to the argument list.
Returns the exit code."
  (let ((args (append (list "-i" input-file "-o" output-file
                            "-p" (shipit--mmdc-puppeteer-config)
                            "--quiet")
                      extra-args)))
    (apply #'call-process "mmdc" nil nil nil args)))

(defun shipit--mmdc-available-p ()
  "Return non-nil if mmdc is installed and can render diagrams.
Caches the result after the first probe.  The probe does a real
test render because `mmdc --version' succeeds even when the
Chrome dependency is missing."
  (when (eq shipit--mmdc-available 'unknown)
    (if (not (executable-find "mmdc"))
        (setq shipit--mmdc-available nil)
      ;; Real probe: render a trivial diagram
      (let* ((tmp-in (make-temp-file "shipit-mmdc-probe-" nil ".mmd"))
             (tmp-out (concat (file-name-sans-extension tmp-in) ".png")))
        (unwind-protect
            (progn
              (with-temp-file tmp-in (insert "graph TD;\n  A-->B\n"))
              (setq shipit--mmdc-available
                    (and (= 0 (shipit--mmdc-call tmp-in tmp-out))
                         (file-exists-p tmp-out))))
          (when (file-exists-p tmp-in) (delete-file tmp-in))
          (when (file-exists-p tmp-out) (delete-file tmp-out)))
        (shipit--debug-log "MERMAID: probe result=%s" shipit--mmdc-available))))
  shipit--mmdc-available)

(defun shipit--render-mermaid-blocks-in-region (start end)
  "Render mermaid code blocks as inline images between START and END.
Requires `mmdc' (mermaid-cli).  Replaces the code block content
with the rendered diagram image, keeping the ``` markers as a heading."
  (when (and shipit-render-mermaid
             (display-graphic-p)
             (shipit--mmdc-available-p))
    (save-excursion
      (goto-char start)
      (while (and shipit--mmdc-available
                  (re-search-forward "^\\([ \t]*\\)```[ \t]*mermaid[ \t]*\n" end t))
        (let* ((content-start (point))
               (indent (match-string 1))
               (close-pattern (concat "^" (regexp-quote indent) "```"))
               (block-end (when (re-search-forward close-pattern end t)
                            (match-end 0)))
               (content-end (when block-end (match-beginning 0)))
               (content (when content-end
                          (buffer-substring-no-properties content-start content-end))))
          (when (and content (not (string-empty-p (string-trim content))))
            (condition-case err
                (let* ((tmp-input (make-temp-file "shipit-mermaid-" nil ".mmd"))
                       (tmp-output (concat (file-name-sans-extension tmp-input) ".png")))
                  (unwind-protect
                      (progn
                        (with-temp-file tmp-input
                          (insert content))
                        (let ((exit-code (shipit--mmdc-call
                                         tmp-input tmp-output
                                         "-t" shipit-mermaid-theme
                                         "-b" shipit-mermaid-background)))
                          (if (and (= exit-code 0) (file-exists-p tmp-output))
                              (let* ((image-data (with-temp-buffer
                                                   (set-buffer-multibyte nil)
                                                   (insert-file-contents-literally tmp-output)
                                                   (buffer-string)))
                                     (image (create-image image-data 'png t
                                                          :max-width shipit-mermaid-max-width)))
                                (when image
                                  (let ((inhibit-read-only t))
                                    (delete-region content-start content-end)
                                    (goto-char content-start)
                                    (insert-image image "[mermaid diagram]")
                                    (insert "\n")
                                    (setq end (+ end (- (point) content-end))))))
                            ;; mmdc failed — stop trying
                            (shipit--debug-log "MERMAID: mmdc exit %d, disabling" exit-code)
                            (setq shipit--mmdc-available nil))))
                    (when (file-exists-p tmp-input)
                      (delete-file tmp-input))
                    (when (file-exists-p tmp-output)
                      (delete-file tmp-output))))
              (error
               (shipit--debug-log "Mermaid render failed: %s" err)
               (setq shipit--mmdc-available nil)))))))))

(defun shipit--create-pr-reference-overlays (repo pr-number &optional body-start body-end)
  "Create overlays for PR references (#\\d+) in comment body.
Styles PR references with blue underline to make them visually distinct.
REPO and PR-NUMBER parameters provided for context (interactive features TODO).
BODY-START and BODY-END define the region to search (defaults to current point to point-max).
Currently provides styling only - interactive keybindings deferred."
  (condition-case err
      (let ((search-start (or body-start (point)))
            (search-limit (or body-end (point-max)))
            (found-count 0)
            (overlay-count 0))
        ;; Debug: log function entry
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "Creating PR reference overlays from point %d to %d (repo=%s pr=%s)"
                             search-start search-limit repo pr-number))
        ;; Search within the specified region
        (save-excursion
          (goto-char search-start)
          (while (re-search-forward "#\\([0-9]+\\)" search-limit t)
            (setq found-count (1+ found-count))
            (let ((ref-num (match-string 1))
                  (start (match-beginning 0))
                  (end (match-end 0))
                  (backtick-count 0))
              ;; Check if in code block by counting backticks on this line
              (save-excursion
                (beginning-of-line)
                (while (< (point) start)
                  (when (= (char-after) ?`)
                    (setq backtick-count (1+ backtick-count)))
                  (forward-char)))
              ;; Create overlay only if not in code
              (if (= (mod backtick-count 2) 1)
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "Skipping #%s (in code block)" ref-num))
                (let* ((ref-num-int (string-to-number ref-num))
                       (repo-copy repo)  ; Capture for closure
                       (ov (make-overlay start end))
                       (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap (current-local-map))
                  (setq overlay-count (1+ overlay-count))
                  ;; Add RET keybinding to show backend-aware action menu
                  (define-key keymap (kbd "RET")
                    (lambda ()
                      (interactive)
                      (shipit--hash-reference-action-menu ref-num-int repo-copy)))
                  ;; Apply styling and interaction properties
                  (overlay-put ov 'face 'markdown-plain-url-face)
                  (overlay-put ov 'keymap keymap)
                  (overlay-put ov 'evaporate t)
                  ;; Generic help-echo (could be issue or PR)
                  (overlay-put ov 'help-echo (format "#%s - RET: actions" ref-num))
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "Created overlay for #%s at %d-%d" ref-num start end)))))))
        ;; Dispatch URL-based overlays to all registered backends
        (shipit-pr--create-reference-overlays
         repo search-start search-limit
         (lambda () (setq found-count (1+ found-count)))
         (lambda () (setq overlay-count (1+ overlay-count))))
        ;; Sixth pass: detect references from non-GitHub registered backends
        (shipit--create-backend-reference-overlays
         repo search-start search-limit
         (lambda () (setq found-count (1+ found-count)))
         (lambda () (setq overlay-count (1+ overlay-count))))
        ;; Debug: log results
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "PR reference overlay creation complete: found %d refs, created %d overlays"
                             found-count overlay-count)))
    (error
     ;; Log error with full backtrace for investigation (don't suppress)
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "ERROR in shipit--create-pr-reference-overlays: %s"
                          err)))))


(defun shipit--create-backend-reference-overlays (repo search-start search-limit inc-found inc-overlay)
  "Create overlays for references from non-GitHub registered backends.
REPO is the repository context.  SEARCH-START and SEARCH-LIMIT bound the region.
INC-FOUND and INC-OVERLAY are thunks called to increment counters."
  (condition-case err
      (let* ((resolved (shipit-issue--resolve-for-repo repo))
             (resolved-config (cdr resolved))
             (backend-plist (car resolved))
             (browse-fn (plist-get backend-plist :browse-url))
             (all-patterns (shipit-issue--all-reference-patterns repo)))
        (shipit--debug-log "Backend ref overlays: repo=%s backend=%s patterns=%d region=%d-%d"
                           repo (plist-get backend-plist :name)
                           (length all-patterns) search-start search-limit)
        (dolist (entry all-patterns)
          (let* ((backend-id (car entry))
                 (regex (nth 1 entry))
                 (group (nth 2 entry))
                 (extractor (nth 3 entry)))
            ;; Skip bare #NNN patterns for all backends — handled by pass 1
            (unless (string-match-p "\\`#" regex)
              (save-excursion
                (goto-char search-start)
                (while (re-search-forward regex search-limit t)
                  (funcall inc-found)
                  (let ((ref-str (match-string group))
                        (start (match-beginning 0))
                        (end (match-end 0))
                        (backtick-count 0))
                    ;; Check if in code block
                    (save-excursion
                      (beginning-of-line)
                      (while (< (point) start)
                        (when (= (char-after) ?`)
                          (setq backtick-count (1+ backtick-count)))
                        (forward-char)))
                    (when (= (mod backtick-count 2) 0)
                      (let* ((ref-id (funcall extractor ref-str))
                             (ov (make-overlay start end))
                             (keymap (make-sparse-keymap)))
                        (set-keymap-parent keymap (current-local-map))
                        (funcall inc-overlay)
                        (let ((id-copy ref-id)
                              (url-copy (funcall browse-fn resolved-config ref-id))
                              (repo-copy repo))
                          (define-key keymap (kbd "RET")
                            (lambda ()
                              (interactive)
                              (shipit--issue-reference-action-menu
                               id-copy repo-copy url-copy))))
                        (overlay-put ov 'face 'markdown-plain-url-face)
                        (overlay-put ov 'keymap keymap)
                        (overlay-put ov 'evaporate t)
                        (overlay-put ov 'help-echo (format "%s - RET: actions" ref-str))
                        (shipit--debug-log "Created backend overlay for %s at %d-%d"
                                           ref-str start end))))))))))
    (error
     (shipit--debug-log "ERROR in shipit--create-backend-reference-overlays: %s" err))))

(defun shipit--issue-reference-action-menu (id repo url)
  "Show action menu for issue reference ID in REPO with browse URL.
Shared by both GitHub and backend-specific reference overlays.
Passes the current buffer's PR backend as the issue backend so that
references in GitLab MR buffers route through GitLab, not GitHub."
  (let* ((display-id (if (integerp id) (format "#%d" id) (format "%s" id)))
         (prompt (format "%s: RET open in shipit, [p]review, [b]rowser, [c]opy URL, [q]uit: " display-id))
         (choice (read-char-choice prompt '(?\r ?p ?b ?c ?q))))
    (pcase choice
      (?\r (shipit-issues-open-buffer id repo
                                      shipit-pr-backend
                                      shipit-pr-backend-config))
      (?p (shipit--show-issue-preview id repo))
      (?b (browse-url url))
      (?c (kill-new url)
          (message "Copied %s to kill ring" url))
      (?q nil))))

(defun shipit--create-commit-sha-overlays (repo &optional body-start body-end)
  "Create overlays for commit SHAs in comment body.
Styles commit SHAs with blue underline and makes them clickable.
REPO is used for constructing GitHub URLs.
BODY-START and BODY-END define the region to search."
  (condition-case err
      (let ((search-start (or body-start (point)))
            (search-limit (or body-end (point-max))))
        (save-excursion
          (goto-char search-start)
          ;; Match 7-40 character hex strings that look like commit SHAs
          ;; Use word boundaries to avoid matching partial hex strings
          ;; Require at least 7 chars to avoid matching short hex like colors
          (while (re-search-forward "\\b\\([0-9a-f]\\{7,40\\}\\)\\b" search-limit t)
            (let* ((sha (match-string 1))
                   (start (match-beginning 1))
                   (end (match-end 1))
                   (short-sha (substring sha 0 (min 7 (length sha))))
                   (backtick-count 0)
                   (in-url nil))
              ;; Check if in code block by counting backticks on this line
              (save-excursion
                (beginning-of-line)
                (while (< (point) start)
                  (when (= (char-after) ?`)
                    (setq backtick-count (1+ backtick-count)))
                  (forward-char)))
              ;; Check if this is part of a URL
              (save-excursion
                (goto-char start)
                (beginning-of-line)
                (while (re-search-forward "https?://[^ \t\n]*" (line-end-position) t)
                  ;; Check if our match position is within the URL
                  (when (and (<= (match-beginning 0) start)
                             (>= (match-end 0) end))
                    (setq in-url t))))
              ;; Create overlay only if not in inline code and not part of URL
              (when (and (= (mod backtick-count 2) 0)
                         (not in-url))
                (let* ((ov (make-overlay start end))
                       (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap (current-local-map))
                  ;; Add RET keybinding to show action menu
                  (define-key keymap (kbd "RET")
                    (let ((sha-copy sha)
                          (repo-copy repo))
                      (lambda ()
                        (interactive)
                        (shipit--commit-sha-action-menu sha-copy repo-copy))))
                  ;; Apply styling (same as PR references - blue with underline)
                  (overlay-put ov 'face 'markdown-plain-url-face)
                  (overlay-put ov 'keymap keymap)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'shipit-commit-sha sha)
                  (overlay-put ov 'help-echo (format "Commit %s - RET: actions" short-sha))))))))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "ERROR in shipit--create-commit-sha-overlays: %s" err)))))

(defun shipit--commit-sha-action-menu (sha repo)
  "Show action menu for commit SHA."
  (let* ((short-sha (substring sha 0 (min 7 (length sha))))
         (in-commits-section (shipit--find-commit-in-commits-section sha))
         (prompt (if in-commits-section
                     (format "Commit %s: [g]o to section, [v]iew in magit, [b]rowser, [c]opy SHA, [q]uit: " short-sha)
                   (format "Commit %s: [v]iew in magit, [b]rowser, [c]opy SHA, [q]uit: " short-sha)))
         (choices (if in-commits-section '(?g ?v ?b ?c ?q) '(?v ?b ?c ?q)))
         (choice (read-char-choice prompt choices)))
    (pcase choice
      (?g (shipit--goto-commit-in-commits-section sha))
      (?v (shipit--view-commit-in-magit sha))
      (?b (shipit--open-commit-in-browser sha repo))
      (?c (kill-new sha)
          (message "Copied %s to kill ring" sha))
      (?q nil))))

(defun shipit--find-commit-in-commits-section (sha)
  "Check if SHA exists in the Commits section of the current buffer.
Returns the position if found, nil otherwise.
Matches by prefix since comments may have short SHAs (7-8 chars)
while the section stores full SHAs (40 chars)."
  (let ((short-sha (substring sha 0 (min 7 (length sha)))))
    (catch 'found
      ;; Walk through magit section tree recursively
      (shipit--find-commit-in-section-tree magit-root-section short-sha)
      nil)))

(defun shipit--find-commit-in-section-tree (section short-sha)
  "Recursively search SECTION and its children for a commit matching SHORT-SHA."
  (when (and section (magit-section-p section))
    ;; Check if this section is a shipit-commit with matching SHA
    (when (eq (oref section type) 'shipit-commit)
      (let ((section-sha (oref section value)))
        (when (and section-sha
                   (stringp section-sha)
                   (string-prefix-p short-sha section-sha))
          (throw 'found (oref section start)))))
    ;; Recursively check children
    (dolist (child (oref section children))
      (shipit--find-commit-in-section-tree child short-sha))))

(defun shipit--goto-commit-in-commits-section (sha)
  "Jump to SHA in the Commits section.
Expands parent sections and positions cursor on the commit header."
  (let ((pos (shipit--find-commit-in-commits-section sha)))
    (if pos
        (progn
          (goto-char pos)
          (let ((section (magit-current-section)))
            ;; Expand all parent sections (e.g., the Commits section itself)
            (when section
              (let ((parent (oref section parent)))
                (while parent
                  (when (oref parent hidden)
                    (magit-section-show parent))
                  (setq parent (oref parent parent))))
              ;; Expand the commit section itself
              (when (oref section hidden)
                (magit-section-show section))
              ;; Go to section start (the header line)
              (goto-char (oref section start))))
          (message "Jumped to commit %s" (substring sha 0 (min 7 (length sha)))))
      (message "Commit %s not found in Commits section" (substring sha 0 (min 7 (length sha)))))))

(defun shipit--view-commit-in-magit (sha)
  "View commit SHA in magit-revision buffer.
If the commit is not available locally, offer to fetch from origin."
  (if (fboundp 'magit-show-commit)
      (if (magit-commit-p sha)
          (magit-show-commit sha)
        ;; Commit not found locally
        (if (y-or-n-p (format "Commit %s not found locally. Fetch from origin? " sha))
            (progn
              (message "Fetching from origin...")
              (magit-run-git "fetch" "origin")
              (if (magit-commit-p sha)
                  (magit-show-commit sha)
                (message "Commit %s still not found. It may be from a fork or unmerged PR." sha)))
          (message "Commit %s not available locally." sha)))
    (message "magit-show-commit not available")))

(defun shipit--open-commit-in-browser (sha repo)
  "Open commit SHA in browser via the active PR backend."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :browse-commit-url)))
    (if fn
        (browse-url (funcall fn config sha))
      (error "Backend has no :browse-commit-url"))))

(defun shipit--reference-is-pr-p (ref-data)
  "Return non-nil if REF-DATA (from issues endpoint) represents a PR."
  (assq 'pull_request ref-data))

(defun shipit--reference-is-pr-by-backend (number repo)
  "Dispatch PR detection for NUMBER in REPO via the active PR backend.
Returns \\='pr, \\='issue, or nil.
Uses the backend's :detect-hash-reference function when registered."
  (let ((detect-fn (plist-get (shipit-pr--get-backend) :detect-hash-reference)))
    (when detect-fn
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (config (cdr resolved)))
        (funcall detect-fn config number repo)))))

(defun shipit--hash-reference-browse-url (repo number)
  "Build browser URL for #NUMBER in REPO via the active PR backend."
  (shipit--browse-issue-url repo number))

(defun shipit--hash-reference-open-issue (number repo url)
  "Open #NUMBER in REPO as an issue, passing PR backend as the issue backend.
Falls back to browsing URL when issues are disabled."
  (if shipit-issues-enabled
      (shipit-issues-open-buffer number repo
                                 shipit-pr-backend
                                 shipit-pr-backend-config)
    (browse-url url)))

(defun shipit--hash-reference-action-menu (number repo)
  "Show action menu for bare #NUMBER reference in REPO.
Routes through the current PR/issue backends instead of hardcoding GitHub.
Resolution: PR detection → issue backend → browser fallback."
  (let* ((url (shipit--hash-reference-browse-url repo number))
         (prompt (format "#%d (%s): [o]pen in shipit, [b]rowser, [c]opy URL, [q]uit: "
                         number repo))
         (choice (read-char-choice prompt '(?o ?b ?c ?q))))
    (pcase choice
      (?o (let ((ref-type (shipit--reference-is-pr-by-backend number repo)))
            (pcase ref-type
              ('pr (shipit--open-pr-same-repo number repo))
              (_ (shipit--hash-reference-open-issue number repo url)))))
      (?b (browse-url url))
      (?c (kill-new url)
          (message "Copied %s to kill ring" url))
      (?q nil))))

(defun shipit--pr-reference-action-menu (pr-number repo &optional type)
  "Show action menu for issue/PR reference.
PR-NUMBER is the issue/PR number, REPO is the repository (owner/name).
TYPE, when non-nil, is `pr' or `issue' to skip the API type-detection fetch.
GitHub uses the same number space for issues and PRs, so preview/open
use DWIM to detect the correct type."
  (let* ((url (shipit--browse-pr-url repo pr-number))
         (prompt (format "#%d (%s): [p]review, [o]pen in shipit, [b]rowser, [c]opy URL, [q]uit: "
                         pr-number repo))
         (choice (read-char-choice prompt '(?p ?o ?b ?c ?q))))
    (pcase choice
      (?p (shipit--reference-preview-dwim pr-number repo type))
      (?o (shipit--reference-open-dwim pr-number repo type))
      (?b (browse-url url))
      (?c (kill-new url)
          (message "Copied %s to kill ring" url))
      (?q nil))))

(defun shipit--reference-detect-type (number repo type)
  "Determine if NUMBER in REPO is a PR or issue.
If TYPE is already `pr' or `issue', return it directly.
Otherwise fetch from GitHub API directly and check for `pull_request' key.
Always uses GitHub API since references come from GitHub URLs, bypassing
the configured issue backend (which may be Jira).

This function is GitHub-specific: it calls `shipit-gh-etag-get-json-with-refresh-cache'
directly.  It is only reachable from GitHub URL overlay keybindings (passes 2-3),
which are now gated behind (eq (shipit-pr--backend-id) \\='github)."
  (or type
      (let* ((endpoint (format "/repos/%s/issues/%s" repo number))
             (result (shipit-gh-etag-get-json-with-refresh-cache endpoint nil shipit-github-token))
             (ref-data (plist-get result :json)))
        (if (shipit--reference-is-pr-p ref-data) 'pr 'issue))))

(defun shipit--reference-open-dwim (number repo &optional type)
  "Open NUMBER in REPO as PR or issue buffer, detecting type automatically.
TYPE, when non-nil, skips the API fetch.
Uses the active PR backend for dispatch."
  (let* ((backend-id (shipit-pr--backend-id))
         (shipit-issue-backend backend-id))
    (pcase (shipit--reference-detect-type number repo type)
      ('pr (shipit--open-pr-same-repo number repo))
      ('issue (if shipit-issues-enabled
                  (shipit-issues-open-buffer number repo backend-id)
                (let* ((resolved (shipit-pr--resolve-for-repo repo))
                       (backend (car resolved))
                       (config (cdr resolved))
                       (browse-fn (plist-get backend :browse-issue-url)))
                  (when browse-fn
                    (browse-url (funcall browse-fn config number)))))))))

(defun shipit--reference-preview-dwim (number repo &optional type)
  "Preview NUMBER in REPO as PR preview or issue preview, detecting type automatically.
TYPE, when non-nil, skips the API fetch.
Uses the active PR backend for dispatch."
  (let* ((backend-id (shipit-pr--backend-id))
         (shipit-issue-backend backend-id))
    (pcase (shipit--reference-detect-type number repo type)
      ('pr (shipit--show-pr-preview number repo))
      ('issue (if shipit-issues-enabled
                  (shipit--show-issue-preview number repo)
                (message "#%d is an issue, not a PR (preview only works for PRs)" number))))))

(defun shipit--create-user-mention-overlays (repo &optional body-start body-end)
  "Create overlays for @username mentions in comment body.
Styles mentions with bold underline and makes them clickable.
REPO is used for constructing GitHub URLs.
BODY-START and BODY-END define the region to search."
  (condition-case err
      (let ((search-start (or body-start (point)))
            (search-limit (or body-end (point-max))))
        (save-excursion
          (goto-char search-start)
          ;; Match @username - GitHub usernames can contain alphanumeric and hyphens
          ;; but cannot start with a hyphen
          (while (re-search-forward "@\\([a-zA-Z0-9][a-zA-Z0-9-]*\\)" search-limit t)
            (let* ((username (match-string 1))
                   (start (match-beginning 0))
                   (end (match-end 0))
                   (backtick-count 0))
              ;; Check if in code block by counting backticks on this line
              (save-excursion
                (beginning-of-line)
                (while (< (point) start)
                  (when (= (char-after) ?`)
                    (setq backtick-count (1+ backtick-count)))
                  (forward-char)))
              ;; Create overlay only if not in inline code
              (when (= (mod backtick-count 2) 0)
                (let* ((ov (make-overlay start end))
                       (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap (current-local-map))
                  ;; Add RET keybinding to show action menu
                  (define-key keymap (kbd "RET")
                    (let ((user-copy username)
                          (repo-copy repo))
                      (lambda ()
                        (interactive)
                        (shipit--user-mention-action-menu user-copy repo-copy))))
                  ;; Apply styling - bold with underline (not blue)
                  (overlay-put ov 'face '(:weight bold :underline t))
                  (overlay-put ov 'keymap keymap)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'shipit-user-mention username)
                  (overlay-put ov 'help-echo (format "@%s - RET: actions" username))))))))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "ERROR in shipit--create-user-mention-overlays: %s" err)))))

(defun shipit--user-mention-action-menu (username repo)
  "Show action menu for USERNAME mention."
  (let ((choice (read-char-choice
                 (format "@%s: [b]rowser (profile), [p]Rs by user, [c]opy username, [q]uit: " username)
                 '(?b ?p ?c ?q))))
    (pcase choice
      (?b (shipit--open-user-profile-in-browser username))
      (?p (shipit--show-user-prs username repo))
      (?c (kill-new username)
          (message "Copied %s to kill ring" username))
      (?q nil))))

(defun shipit--open-user-profile-in-browser (username)
  "Open USERNAME's profile in browser via the active PR backend."
  (let* ((resolved (shipit-pr--resolve-for-repo
                    (or (bound-and-true-p shipit-current-repo)
                        (shipit--get-repo-from-remote))))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :browse-user-url)))
    (if fn
        (browse-url (funcall fn config username))
      (error "Backend has no :browse-user-url"))))

(defun shipit--show-user-prs (username repo)
  "Show PRs by USERNAME in REPO using Emacs completing-read.
Searches for open and closed PRs authored by USERNAME and presents
them in a completing-read interface."
  (message "Searching for PRs by %s..." username)
  (let* ((query-parts (list (format "repo:%s" repo)
                            "is:pr"
                            (format "author:%s" username)))
         (per-page 50)
         (max-results 100)
         (results (shipit--search-prs-with-encoded-query repo query-parts per-page max-results)))
    (if (and results (> (length results) 0))
        (progn
          (message "Found %d PRs by %s" (length results) username)
          (shipit--select-pr-from-results results repo))
      (message "No PRs found by %s in %s" username repo))))

(defun shipit--create-custom-url-overlays (&optional body-start body-end)
  "Create overlays for custom URL patterns in region.
Uses patterns from `shipit-custom-url-patterns'.
BODY-START and BODY-END define the region to search."
  (when (and (boundp 'shipit-custom-url-patterns) shipit-custom-url-patterns)
    (condition-case err
        (let ((search-start (or body-start (point)))
              (search-limit (or body-end (point-max))))
          (dolist (pattern-spec shipit-custom-url-patterns)
            (let ((pattern (car pattern-spec))
                  (url-template (cdr pattern-spec)))
              (save-excursion
                (goto-char search-start)
                (while (re-search-forward pattern search-limit t)
                  (let* ((matched-text (match-string 0))
                         (start (match-beginning 0))
                         (end (match-end 0))
                         (backtick-count 0))
                    ;; Check if in code block by counting backticks on this line
                    (save-excursion
                      (beginning-of-line)
                      (while (< (point) start)
                        (when (= (char-after) ?`)
                          (setq backtick-count (1+ backtick-count)))
                        (forward-char)))
                    ;; Create overlay only if not in inline code and not an issuelink
                    (when (and (= (mod backtick-count 2) 0)
                               (not (get-text-property start 'shipit-issuelink-key)))
                      ;; Build URL by matching pattern and substituting capture groups
                      ;; Use fixedcase=t to preserve case in URL template
                      (let* ((url (progn
                                    (string-match pattern matched-text)
                                    (replace-match url-template t nil matched-text)))
                             (ov (make-overlay start end))
                             (keymap (make-sparse-keymap))
                             (id-copy matched-text)
                             (url-copy url)
                             (repo-copy (shipit--get-repo-from-remote)))
                        (set-keymap-parent keymap (current-local-map))
                        (define-key keymap (kbd "RET")
                          (lambda ()
                            (interactive)
                            (shipit--issue-reference-action-menu
                             id-copy repo-copy url-copy)))
                        (overlay-put ov 'face 'markdown-plain-url-face)
                        (overlay-put ov 'keymap keymap)
                        (overlay-put ov 'evaporate t)
                        (overlay-put ov 'help-echo (format "%s → %s (RET: actions)" matched-text url))))))))))
      (error
       (when (fboundp 'shipit--debug-log)
         (shipit--debug-log "ERROR in shipit--create-custom-url-overlays: %s" err))))))

(defun shipit--try-overlay-action-at-point ()
  "Try to invoke the RET action from the highest-priority overlay at point.
Returns non-nil if an overlay action was found and invoked.
Backend-specific overlays take priority over generic URL overlays via
the overlay `priority' property."
  (let ((best-binding nil)
        (best-priority -1))
    (dolist (ov (overlays-at (point)))
      (when-let* ((km (overlay-get ov 'keymap))
                  (binding (or (lookup-key km [return])
                               (lookup-key km (kbd "RET")))))
        (when (and (commandp binding)
                   (> (or (overlay-get ov 'priority) 0) best-priority))
          (setq best-binding binding
                best-priority (or (overlay-get ov 'priority) 0)))))
    (when best-binding
      (call-interactively best-binding)
      t)))

(defun shipit--classify-url (url)
  "Classify URL as a known shipit resource.
Returns a plist (:type TYPE :repo REPO :number N) or nil.
Tries PR backends first, then issue backends."
  (let ((clean (substring-no-properties url)))
    (or (shipit-pr--classify-url clean)
        (shipit-issue--classify-url clean))))

(defun shipit--open-blob-url (classified)
  "Open a blob URL CLASSIFIED plist in a buffer with appropriate major mode."
  (let* ((repo (plist-get classified :repo))
         (ref (plist-get classified :ref))
         (path (plist-get classified :path))
         (fragment (plist-get classified :fragment))
         (backend-id (or (plist-get classified :backend-id) 'github))
         (filename (file-name-nondirectory path))
         (buf-name (format "*shipit-blob: %s/%s@%s*" repo path ref)))
    (message "Fetching %s..." path)
    (let* ((resolved (let ((shipit-pr-backend backend-id)
                           (shipit-pr-backend-config nil))
                       (shipit-pr--resolve-for-repo repo)))
           (backend (car resolved))
           (config (cdr resolved))
           (fetch-fn (plist-get backend :fetch-file-content))
           (file-content (when fetch-fn
                           (funcall fetch-fn config path ref))))
      (if (not file-content)
          (user-error "Could not fetch %s" path)
        (let ((buf (get-buffer-create buf-name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert file-content)
              (goto-char (point-min))
              ;; Set major mode from filename
              (let ((buffer-file-name filename))
                (set-auto-mode))
              (setq buffer-read-only t)
              ;; Navigate to fragment anchor
              (when (and fragment (not (string-empty-p fragment)))
                (goto-char (point-min))
                ;; Try heading/anchor patterns
                (or (search-forward fragment nil t)
                    (search-forward (replace-regexp-in-string "-" " " fragment) nil t)
                    (goto-char (point-min))))
              (set-buffer-modified-p nil)))
          (switch-to-buffer buf)
          (message "Fetched %s@%s" path ref))))))

(defun shipit--open-classified-url (classified)
  "Open a CLASSIFIED URL plist in the appropriate shipit buffer.
When :backend-id is present, passes it through so the correct forge is used."
  (let ((type (plist-get classified :type))
        (repo (plist-get classified :repo))
        (number (plist-get classified :number))
        (backend-id (plist-get classified :backend-id))
        (backend-config (plist-get classified :backend-config)))
    (pcase type
      ('pr (shipit-open-pr-buffer number repo backend-id))
      ('issue (shipit-issues-open-buffer number repo backend-id backend-config))
      ('repo (shipit-open-repo-buffer repo backend-id backend-config))
      ('discussion
       (shipit-discussions-open-buffer number repo))
      ('blob
       (shipit--open-blob-url classified))
      ('releases
       (shipit-open-releases-buffer repo (plist-get classified :tag)))
      ('tags
       (shipit-open-releases-buffer repo nil 'tags))
      ('actions-run
       (shipit-open-actions-run repo
                                (plist-get classified :run-id)
                                (plist-get classified :job-id))))))

(defun shipit--generic-url-action-menu (url &optional prefix-arg)
  "Show action menu for a generic URL.
With PREFIX-ARG, always show the full action menu."
  (let ((classified (shipit--classify-url url)))
    (if (and classified (not prefix-arg))
        (shipit--open-classified-url classified)
      (let* ((has-open (and classified t))
             (prompt (format "Open %s — %s[b]rowser, [c]opy URL, [q]uit: "
                             (truncate-string-to-width url 60 nil nil t)
                             (if has-open "[o]pen in shipit, " "")))
             (chars (if has-open '(?o ?b ?c ?q) '(?b ?c ?q)))
             (choice (read-char-choice prompt chars)))
        (pcase choice
          (?o (shipit--open-classified-url classified))
          (?b (browse-url url))
          (?c (kill-new url) (message "Copied %s to kill ring" url))
          (?q nil))))))

(defun shipit--generic-url-overlay-range (url-start)
  "Return (OV-START . OV-END) for the visible portion of a URL at URL-START.
For markdown links [text](url), returns the link text range.
For bare URLs, returns the URL range from the current match data."
  (if (and (> url-start 1)
           (eq (char-before url-start) ?\()
           (eq (char-before (1- url-start)) ?\]))
      ;; Markdown link: [text](url) — overlay the visible link text
      (let ((text-end (- url-start 2))
            (text-start (save-excursion
                          (goto-char (- url-start 2))
                          (when (search-backward "[" nil t)
                            (1+ (point))))))
        (if text-start
            (cons text-start text-end)
          (cons (match-beginning 0) (match-end 0))))
    ;; Bare URL — overlay the URL itself
    (cons (match-beginning 0) (match-end 0))))

(defun shipit--create-generic-url-overlays (&optional body-start body-end)
  "Create overlays for any remaining URLs not already handled by specific passes.
For markdown links [text](url), the overlay covers the visible link text.
For bare URLs, the overlay covers the URL itself.
BODY-START and BODY-END define the region to search."
  (let ((search-start (or body-start (point-min)))
        (search-limit (or body-end (point-max))))
    (save-excursion
      (goto-char search-start)
      (while (re-search-forward "https?://[^] \t\n\r)<>\"']+" search-limit t)
        (let* ((url (match-string 0))
               (url-start (match-beginning 0))
               (range (shipit--generic-url-overlay-range url-start))
               (ov-start (car range))
               (ov-end (cdr range))
               (already-handled nil)
               (backtick-count 0))
          ;; Skip if overlay range already has an overlay with a keymap
          (dolist (ov (overlays-at ov-start))
            (when (overlay-get ov 'keymap)
              (setq already-handled t)))
          ;; Check if in code block by counting backticks on this line
          (save-excursion
            (goto-char (line-beginning-position))
            (while (< (point) ov-start)
              (when (= (char-after) ?`)
                (setq backtick-count (1+ backtick-count)))
              (forward-char)))
          (when (and (not already-handled)
                     (= (mod backtick-count 2) 0))
            (let ((ov (make-overlay ov-start ov-end))
                  (keymap (make-sparse-keymap))
                  (url-copy url))
              (set-keymap-parent keymap (current-local-map))
              (define-key keymap [return]
                (lambda ()
                  (interactive)
                  (shipit--generic-url-action-menu url-copy current-prefix-arg)))
              (define-key keymap (kbd "RET")
                (lambda ()
                  (interactive)
                  (shipit--generic-url-action-menu url-copy current-prefix-arg)))
              (overlay-put ov 'face 'markdown-plain-url-face)
              (overlay-put ov 'keymap keymap)
              (overlay-put ov 'priority 100)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'help-echo (format "%s (RET: actions)" url)))))))))

(defun shipit--show-pr-summary-at-cursor ()
  "Show PR summary in echo area if cursor is on a PR reference overlay."
  (let ((overlays (overlays-at (point))))
    (dolist (ov overlays)
      (when-let ((help-echo (overlay-get ov 'help-echo)))
        ;; Check if this is a PR reference overlay by looking for help-echo pattern
        ;; PR reference overlays have help-echo starting with "PR #"
        (when (stringp help-echo)
          (if (string-match-p "^PR #" help-echo)
              (progn
                (message "%s" help-echo)
                (throw 'found t))))))))

(defun shipit--pr-preview-open (pr-number repo)
  "Open PR-NUMBER from REPO in shipit."
  (interactive)
  (shipit--open-pr-same-repo pr-number repo))

(defun shipit--pr-preview-open-browser (pr-number repo)
  "Open PR-NUMBER from REPO in Emacs browser."
  (interactive)
  (when-let ((url (shipit--browse-pr-url repo pr-number)))
    (browse-url url)))

(defun shipit--pr-preview-open-external-browser (pr-number repo)
  "Open PR-NUMBER from REPO in external browser.
Tries system browsers (xdg-open, open, start) then falls back to eww."
  (interactive)
  (when-let ((url (shipit--browse-pr-url repo pr-number)))
    (let ((browser-cmd (cond
                        ((executable-find "xdg-open") "xdg-open")   ; Linux
                        ((executable-find "open") "open")           ; macOS
                        ((executable-find "start") "start")         ; Windows
                        (t nil))))
      (if browser-cmd
          (start-process "shipit-browser" nil browser-cmd url)
        (progn
          (message "No external browser found. Opening in eww browser (use & to open in external browser).")
          (browse-url url))))))

;; Store current PR preview data for transient menu access
(defvar shipit--current-pr-preview-data nil
  "Current PR data being previewed in transient menu.")

;;; Helper function to format PR information
(defun shipit--format-pr-info-string (pr-number pr-data)
  "Format PR data as a display string."
  (let* ((title (string-trim (or (cdr (assq 'title pr-data)) "Unknown")))
         (state (or (shipit--get-pr-actual-state pr-data) "unknown"))
         (author (or (cdr (assq 'login (cdr (assq 'user pr-data)))) "Unknown"))
         (created (or (cdr (assq 'created_at pr-data)) ""))
         (updated (or (cdr (assq 'updated_at pr-data)) ""))
         (timestamp (if (and (fboundp 'shipit--format-timestamp) (not (string-empty-p created)))
                       (shipit--format-timestamp created)
                     ""))
         (updated-timestamp (if (and (fboundp 'shipit--format-timestamp) (not (string-empty-p updated)))
                               (shipit--format-timestamp updated)
                             ""))
         (review-comments (or (cdr (assq 'review_comments pr-data)) 0))
         (comments (or (cdr (assq 'comments pr-data)) 0))
         (body (or (cdr (assq 'body pr-data)) ""))
         (commits (or (cdr (assq 'commits pr-data)) 0)))
    (concat
     (format "PR #%d [%s] ─ %s\n" pr-number (upcase state) title)
     (format "Author: %s (%s)\n" author (if (not (string-empty-p timestamp)) timestamp "unknown"))
     (format "Created: %s | Updated: %s\n"
             (if (not (string-empty-p timestamp)) timestamp "unknown")
             (if (not (string-empty-p updated-timestamp)) updated-timestamp "unknown"))
     (format "Reviews: %d | Comments: %d | Commits: %d" review-comments comments commits)
     (if (> (length body) 0)
         (let ((body-preview (if (> (length body) 100)
                                (concat (substring body 0 97) "...")
                              body)))
           (format "\nDescription: %s" body-preview))
       ""))))

;;; Helper to format PR state with emoji
(defun shipit--get-pr-state-emoji (state)
  "Get emoji icon for PR state."
  (pcase state
    ("open" "🟢")
    ("merged" "🎉")
    ("closed" "❌")
    ("draft" "🚧")
    (_ "⚪")))

;;; Child frame for PR preview
(defun shipit--create-pr-preview-frame (pr-number pr-data)
  "Create a child frame displaying PR information next to the cursor."
  (let* ((original-window (selected-window))
         (buffer (generate-new-buffer " *shipit-pr-preview*"))
         (width 80)
         ;; Define preview actions - each action is one line
         (preview-actions '(("RET" "Open PR in shipit")
                           ("b" "Open in Emacs browser")
                           ("B" "Open in external browser")
                           ("m" "Mark as read")
                           ("q" "Close popup")))
         ;; Calculate height based on description length
         (body (or (cdr (assq 'body pr-data)) ""))
         (body-lines (max 1 (ceiling (/ (float (length body)) (float width)))))
         ;; Fixed overhead:
         ;; - Header: top padding (1) + PR header (1) + author (1) + dates (1) + stats (1) = 5 lines
         ;; - Activity: "Recent:" header (1) + up to 3 activity items = 4 lines (estimated)
         ;; - Description: header (1) when content exists
         ;; - Actions: separator (1) + "Actions:" header (1) + action items (N) + bottom padding (1)
         (num-actions (length preview-actions))
         (header-footer-lines (+ 5 4 1 1 1 num-actions 1))  ; header + activity + desc-header + sep + actions-header + items + padding
         ;; Calculate available vertical space (up to 80% of screen)
         (screen-height (frame-height (selected-frame)))
         (available-height (truncate (* screen-height 0.8)))
         (height (max 15 (min available-height (+ body-lines header-footer-lines))))
         ;; Get cursor position in pixels relative to current window
         (cursor-pos (posn-x-y (posn-at-point)))
         (cursor-x (car cursor-pos))
         (cursor-y (cdr cursor-pos))
         ;; Get window position within the frame (accounts for splits)
         (window-edges (window-pixel-edges))
         (window-left (nth 0 window-edges))
         (window-top (nth 1 window-edges))
         ;; Get frame position on screen
         (frame-pos (frame-position))
         (frame-x (car frame-pos))
         (frame-y (cdr frame-pos))
         ;; Calculate absolute cursor position (frame pos + window pos + cursor pos)
         (abs-cursor-x (+ frame-x window-left cursor-x))
         (abs-cursor-y (+ frame-y window-top cursor-y))
         ;; Get screen dimensions for boundary checking
         (frame-edges-rect (frame-edges (selected-frame)))
         (frame-width (- (nth 2 frame-edges-rect) (nth 0 frame-edges-rect)))
         (frame-height (- (nth 3 frame-edges-rect) (nth 1 frame-edges-rect)))
         ;; Convert popup height from lines to pixels (approximately 20 pixels per line)
         (popup-height-px (* height 20))
         (popup-width-px (* width 8))
         ;; Smart positioning: check vertical space below cursor
         (space-below (- (+ frame-y frame-height) abs-cursor-y 20))
         (space-above (- abs-cursor-y frame-y))
         ;; Position below by default, above if not enough space below
         (popup-y-below (+ abs-cursor-y 20))
         (popup-y-above (max frame-y (- abs-cursor-y popup-height-px 10)))
         (popup-y (if (>= space-below popup-height-px)
                     popup-y-below
                   popup-y-above))
         ;; Smart horizontal positioning: prefer right, but adjust if near right edge
         (popup-x-right (+ abs-cursor-x 20))
         (popup-x-left (max frame-x (- abs-cursor-x popup-width-px 20)))
         (popup-x (if (<= (+ popup-x-right popup-width-px) (+ frame-x frame-width))
                     popup-x-right
                   popup-x-left))
         (frame (make-frame
                 `((parent-frame . ,(selected-frame))
                   (name . "shipit-pr-preview")
                   (width . ,width)
                   (height . ,height)
                   (left . ,popup-x)
                   (top . ,popup-y)
                   (border-width . 1)
                   (internal-border-width . 1)
                   (vertical-scroll-bars . right)
                   (scroll-bar-width . 6)
                   (horizontal-scroll-bars . nil)))))

    ;; Set up the buffer with styled PR info
    (with-current-buffer buffer
      ;; Hide modeline for cleaner popup appearance
      (setq mode-line-format nil)

      ;; Add top padding
      (insert "\n")

      ;; PR header line with state icon
      (let* ((title (string-trim (or (cdr (assq 'title pr-data)) "Unknown")))
             (state (or (shipit--get-pr-actual-state pr-data) "unknown"))
             (state-emoji (shipit--get-pr-state-emoji state)))
        (insert "  ")
        (insert (shipit--get-pr-state-icon state state-emoji))
        (insert (format " PR #%d [%s] ─ "
                        pr-number
                        (upcase state)))
        (insert (propertize (truncate-string-to-width title 40)
                           'face 'magit-section-heading))
        (insert "\n"))

      ;; Author line with styled name and timestamp
      (let* ((author (or (cdr (assq 'login (cdr (assq 'user pr-data)))) "Unknown"))
             (created (or (cdr (assq 'created_at pr-data)) ""))
             (timestamp (if (and (fboundp 'shipit--format-timestamp) (not (string-empty-p created)))
                           (shipit--format-timestamp created)
                         "")))
        (insert "  ")
        (insert (shipit--get-pr-field-icon "author" "👤"))
        (insert " ")
        (insert (propertize author 'face 'shipit-username-face))
        (when (not (string-empty-p timestamp))
          (insert " ")
          (insert (propertize timestamp 'face 'shipit-timestamp-face)))
        (insert "\n"))

      ;; Dates line
      (let* ((created (or (cdr (assq 'created_at pr-data)) ""))
             (updated (or (cdr (assq 'updated_at pr-data)) ""))
             (created-ts (if (and (fboundp 'shipit--format-timestamp) (not (string-empty-p created)))
                            (shipit--format-timestamp created)
                          ""))
             (updated-ts (if (and (fboundp 'shipit--format-timestamp) (not (string-empty-p updated)))
                            (shipit--format-timestamp updated)
                          "")))
        (insert "  ")
        (insert (shipit--get-pr-field-icon "created" "📅"))
        (insert " Created: ")
        (insert (propertize created-ts 'face 'shipit-timestamp-face))
        (insert " | Updated: ")
        (insert (propertize updated-ts 'face 'shipit-timestamp-face))
        (insert "\n"))

      ;; Stats line with emoji
      (let* ((review-comments (or (cdr (assq 'review_comments pr-data)) 0))
             (comments (or (cdr (assq 'comments pr-data)) 0))
             (commits (or (cdr (assq 'commits pr-data)) 0)))
        (insert "  ")
        (insert (shipit--get-comment-type-icon "review" "💬"))
        (insert (format " Reviews: %d | " review-comments))
        (insert (shipit--get-comment-type-icon "comment" "💬"))
        (insert (format " Comments: %d | " comments))
        (insert (shipit--get-pr-field-icon "created" "📝"))
        (insert (format " Commits: %d\n" commits)))

      ;; Recent Activity (customizable number of events)
      ;; Always fetch fresh timeline data for preview popup to show current state
      (when (and (fboundp 'shipit--fetch-timeline-events)
                 (car shipit--current-pr-preview-data)
                 (cadr shipit--current-pr-preview-data))
        (condition-case err
            (let* ((pr-num (car shipit--current-pr-preview-data))
                   (repo (cadr shipit--current-pr-preview-data))
                   (events (shipit--fetch-timeline-events repo pr-num t)) ; force-fresh
                   (activity-count (if (boundp 'shipit-pr-preview-recent-activities)
                                      shipit-pr-preview-recent-activities
                                    5))
                   (recent-events (when events
                                    ;; Get last N events (most recent first)
                                    (take activity-count (reverse events)))))
              (when (and recent-events (> (length recent-events) 0))
                (insert "  ")
                (insert (propertize "Recent:" 'face 'markdown-metadata-key-face))
                (insert "\n")
                (dolist (event recent-events)
                  (let* ((event-type (cdr (assq 'event event)))
                         ;; For commit events, author/committer have login directly
                         (author (cdr (assq 'author event)))
                         (committer (cdr (assq 'committer event)))
                         ;; For non-commit events, user/actor objects
                         (user-obj (or (cdr (assq 'user event))
                                       (cdr (assq 'actor event))))
                         ;; Get actor name - different structure for commits vs other events
                         (actor (cond
                                 ;; Commit events: login is directly in author/committer
                                 ((string= event-type "committed")
                                  (or (when author (cdr (assq 'login author)))
                                      (when committer (cdr (assq 'login committer)))
                                      (when author (cdr (assq 'name author)))
                                      (when committer (cdr (assq 'name committer)))
                                      "Unknown"))
                                 ;; Other events: login is in user/actor object
                                 (t (or (when user-obj (cdr (assq 'login user-obj)))
                                        "Unknown"))))
                         ;; Get backend login for avatar URL construction
                         (backend-login (cond
                                         ((string= event-type "committed")
                                          (or (when author (cdr (assq 'login author)))
                                              (when committer (cdr (assq 'login committer)))))
                                         (t (when user-obj (cdr (assq 'login user-obj))))))
                         ;; Get email for Gravatar fallback
                         (commit-email (when (string= event-type "committed")
                                         (or (when author (cdr (assq 'email author)))
                                             (when committer (cdr (assq 'email committer))))))
                         ;; Avatar: try direct URL first, then construct from GitHub login or email
                         (avatar-url (or (when user-obj (cdr (assq 'avatar_url user-obj)))
                                         (when author (cdr (assq 'avatar_url author)))
                                         (when committer (cdr (assq 'avatar_url committer)))
                                         ;; Construct from backend username if available
                                         (when backend-login
                                           (shipit--generate-avatar-url backend-login))
                                         ;; Fallback: use backend's email-based avatar if available
                                         (when commit-email
                                           (let ((email-avatar-fn (plist-get (shipit-pr--get-backend) :email-avatar-url)))
                                             (when email-avatar-fn
                                               (funcall email-avatar-fn nil commit-email))))))
                         (avatar-display (if (and (fboundp 'shipit--create-avatar-display)
                                                  (boundp 'shipit-show-avatars)
                                                  shipit-show-avatars
                                                  avatar-url)
                                            (shipit--create-avatar-display actor avatar-url 14)
                                          ""))
                         (created-at (cdr (assq 'created_at event)))
                         (timestamp (if (and (fboundp 'shipit--format-timestamp)
                                            (not (string-empty-p created-at)))
                                       (shipit--format-timestamp created-at)
                                     ""))
                         ;; Get review state for review events
                         (review-state (when (string= event-type "reviewed") (cdr (assq 'state event))))
                         ;; Map emoji fallbacks to match main buffer
                         (emoji-fallback (pcase event-type
                                          ("reviewed" (pcase review-state
                                                        ("approved" "✅")
                                                        ("changes_requested" "🔄")
                                                        ("commented" "💬")
                                                        (_ "📝")))
                                          ("commented" "💬")
                                          ("committed" "📝")
                                          ("review_requested" "👀")
                                          ("labeled" "🏷")
                                          ("unlabeled" "🏷")
                                          ("assigned" "👤")
                                          ("unassigned" "👤")
                                          ("closed" "❌")
                                          ("reopened" "🔄")
                                          ("merged" "🎉")
                                          (_ "📌")))
                         ;; Get SVG icon with emoji fallback
                         (icon (if (fboundp 'shipit--get-activity-event-icon)
                                  (shipit--get-activity-event-icon event-type review-state emoji-fallback)
                                emoji-fallback))
                         (action (pcase event-type
                                   ("reviewed" (pcase review-state
                                                 ("approved" "approved")
                                                 ("changes_requested" "requested changes")
                                                 ("commented" "reviewed")))
                                   ("commented" "commented")
                                   ("committed" "committed")
                                   ("review_requested" "requested review")
                                   ("labeled" (format "labeled: %s" (or (cdr (assq 'name (cdr (assq 'label event)))) "")))
                                   ("unlabeled" (format "unlabeled: %s" (or (cdr (assq 'name (cdr (assq 'label event)))) "")))
                                   ("assigned" "assigned")
                                   ("unassigned" "unassigned")
                                   ("closed" "closed")
                                   ("reopened" "reopened")
                                   ("merged" "merged")
                                   (_ event-type))))
                    (insert "    ")
                    (insert icon)
                    (insert " ")
                    ;; Display avatar followed by username
                    (when (not (string-empty-p avatar-display))
                      (insert avatar-display)
                      (insert " "))
                    (insert (propertize actor 'face 'shipit-username-face))
                    (insert " ")
                    (insert (propertize action 'face 'shadow))
                    (insert " ")
                    (insert (propertize timestamp 'face 'shipit-timestamp-face))
                    (insert "\n")))))
          (error (shipit--debug-log "Error fetching activities for popup: %s" err))))

      ;; Description (with full rendering)
      (let* ((raw-body (or (cdr (assq 'body pr-data)) ""))
             (clean-body (if (and raw-body (not (string-empty-p raw-body)))
                            (let ((cleaned (shipit--clean-text raw-body)))
                              (if (string-match-p "\\`[[:space:]]*\\'" cleaned)
                                  nil
                                cleaned))
                          nil))
             (is-empty (not clean-body)))
        (when (not is-empty)
          (insert "  Description:\n")
          (let* ((description-start (point))
                 (rendered-body (if (and (boundp 'shipit-render-markdown) shipit-render-markdown
                                         (fboundp 'shipit--render-markdown))
                                    (shipit--render-markdown clean-body)
                                  clean-body))
                 (wrapped-description (if (fboundp 'shipit--wrap-text)
                                         (shipit--wrap-text rendered-body 70)
                                       rendered-body))
                 (indented-description (replace-regexp-in-string "\n" "\n  " wrapped-description)))
            (insert "  ")
            (insert (propertize indented-description 'face 'default))
            (insert "\n")
            ;; Apply code block background overlays
            (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
              (shipit--apply-code-block-backgrounds-in-region description-start (point))))))

      ;; Actions section
      (insert "  ")
      (insert (propertize "─────────────────────────────" 'face 'shadow))
      (insert "\n")
      (insert "  Actions:\n")
      ;; Render each action from preview-actions list
      (dolist (action preview-actions)
        (insert "    ")
        (insert (propertize (car action) 'face 'font-lock-keyword-face))
        (insert (format " - %s\n" (cadr action))))

      ;; Add bottom padding
      (insert "\n")

      ;; Move to beginning of buffer to ensure it displays from the top
      (goto-char (point-min))

      ;; Set up keybindings with cleanup
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET")
          (lambda ()
            (interactive)
            (delete-frame frame)
            (kill-buffer buffer)
            (select-window original-window)
            (shipit--pr-preview-open pr-number (cadr shipit--current-pr-preview-data))))
        (define-key map (kbd "b")
          (lambda ()
            (interactive)
            (delete-frame frame)
            (kill-buffer buffer)
            (select-window original-window)
            (shipit--pr-preview-open-browser pr-number (cadr shipit--current-pr-preview-data))))
        (define-key map (kbd "B")
          (lambda ()
            (interactive)
            (delete-frame frame)
            (kill-buffer buffer)
            (select-window original-window)
            (shipit--pr-preview-open-external-browser pr-number (cadr shipit--current-pr-preview-data))))
        (define-key map (kbd "m")
          (lambda ()
            (interactive)
            (let ((repo (cadr shipit--current-pr-preview-data)))
              (when (fboundp 'shipit--mark-notification-read)
                (shipit--mark-notification-read pr-number repo)))
            (delete-frame frame)
            (kill-buffer buffer)
            (select-window original-window)))
        (define-key map (kbd "q")
          (lambda ()
            (interactive)
            (delete-frame frame)
            (kill-buffer buffer)
            (select-window original-window)))
        (use-local-map map))

      ;; Set buffer properties
      (setq buffer-read-only t)
      (setq cursor-type nil))

    ;; Show buffer in the frame and give it focus
    (set-frame-parameter frame 'default-minibuffer-frame (selected-frame))
    (set-window-buffer (frame-selected-window frame) buffer)
    (select-frame-set-input-focus frame)
    frame))

(defun shipit--show-pr-preview (pr-number repo)
  "Show PR preview in a child frame with action keybindings."
  (let ((pr-data (shipit-get-pull-request pr-number repo)))
    (if pr-data
        (progn
          ;; Store data for keybinding handlers
          (setq shipit--current-pr-preview-data (list pr-number repo pr-data))
          ;; Create and display child frame
          (shipit--create-pr-preview-frame pr-number pr-data))
      (message "Could not fetch PR #%d from %s" pr-number repo))))

(defvar shipit--current-issue-preview-data nil
  "Stores (issue-number repo issue-data) for the current issue preview.")

(defun shipit--show-issue-preview (issue-number repo)
  "Show issue preview in a child frame with action keybindings."
  (let ((issue-data (shipit-issues--fetch-issue repo issue-number)))
    (if issue-data
        (progn
          (setq shipit--current-issue-preview-data (list issue-number repo issue-data))
          (shipit--create-issue-preview-frame issue-number issue-data))
      (message "Could not fetch issue %s from %s" issue-number repo))))

(defun shipit--create-issue-preview-frame (issue-number issue-data)
  "Create a child frame displaying issue information next to the cursor.
ISSUE-NUMBER is the issue number, ISSUE-DATA is the alist from the API."
  (let* ((original-window (selected-window))
         (buffer (generate-new-buffer " *shipit-issue-preview*"))
         (width 80)
         (preview-actions '(("RET/o" "Open issue in shipit")
                            ("b" "Open in browser")
                            ("q" "Close popup")))
         (body (or (cdr (assq 'body issue-data)) ""))
         (body-lines (max 1 (ceiling (/ (float (length body)) (float width)))))
         ;; Header (4) + labels (1) + desc-header (1) + sep (1) + actions-header (1) + items + padding (1)
         (num-actions (length preview-actions))
         (header-footer-lines (+ 4 1 1 1 1 num-actions 1))
         (screen-height (frame-height (selected-frame)))
         (available-height (truncate (* screen-height 0.8)))
         (height (max 12 (min available-height (+ body-lines header-footer-lines))))
         (cursor-pos (posn-x-y (posn-at-point)))
         (cursor-x (car cursor-pos))
         (cursor-y (cdr cursor-pos))
         (window-edges (window-pixel-edges))
         (window-left (nth 0 window-edges))
         (window-top (nth 1 window-edges))
         (frame-pos (frame-position))
         (frame-x (car frame-pos))
         (frame-y (cdr frame-pos))
         (abs-cursor-x (+ frame-x window-left cursor-x))
         (abs-cursor-y (+ frame-y window-top cursor-y))
         (frame-edges-rect (frame-edges (selected-frame)))
         (frame-width (- (nth 2 frame-edges-rect) (nth 0 frame-edges-rect)))
         (frame-height (- (nth 3 frame-edges-rect) (nth 1 frame-edges-rect)))
         (popup-height-px (* height 20))
         (popup-width-px (* width 8))
         (space-below (- (+ frame-y frame-height) abs-cursor-y 20))
         (popup-y-below (+ abs-cursor-y 20))
         (popup-y-above (max frame-y (- abs-cursor-y popup-height-px 10)))
         (popup-y (if (>= space-below popup-height-px)
                     popup-y-below
                   popup-y-above))
         (popup-x-right (+ abs-cursor-x 20))
         (popup-x-left (max frame-x (- abs-cursor-x popup-width-px 20)))
         (popup-x (if (<= (+ popup-x-right popup-width-px) (+ frame-x frame-width))
                     popup-x-right
                   popup-x-left))
         (frame (make-frame
                 `((parent-frame . ,(selected-frame))
                   (name . "shipit-issue-preview")
                   (width . ,width)
                   (height . ,height)
                   (left . ,popup-x)
                   (top . ,popup-y)
                   (border-width . 1)
                   (internal-border-width . 1)
                   (vertical-scroll-bars . right)
                   (scroll-bar-width . 6)
                   (horizontal-scroll-bars . nil)))))

    (with-current-buffer buffer
      (setq mode-line-format nil)
      (insert "\n")

      ;; Issue header with state
      (let* ((title (string-trim (or (cdr (assq 'title issue-data)) "Unknown")))
             (state (or (cdr (assq 'state issue-data)) "unknown"))
             (field-type (if (string= state "open") "issue-open" "issue-closed"))
             (emoji-fallback (if (string= state "open") "🟢" "🟣")))
        (insert "  ")
        (insert (shipit--get-pr-field-icon field-type emoji-fallback))
        (insert (format " %s [%s] ─ "
                        (if (integerp issue-number)
                            (format "Issue #%d" issue-number)
                          (format "%s" issue-number))
                        (upcase state)))
        (insert (propertize (truncate-string-to-width title 40)
                           'face 'magit-section-heading))
        (insert "\n"))

      ;; Author and dates
      (let* ((author (or (cdr (assq 'login (cdr (assq 'user issue-data)))) "Unknown"))
             (created (or (cdr (assq 'created_at issue-data)) ""))
             (updated (or (cdr (assq 'updated_at issue-data)) ""))
             (created-ts (if (and (fboundp 'shipit--format-timestamp) (not (string-empty-p created)))
                            (shipit--format-timestamp created) ""))
             (updated-ts (if (and (fboundp 'shipit--format-timestamp) (not (string-empty-p updated)))
                            (shipit--format-timestamp updated) "")))
        (insert "  ")
        (insert (shipit--get-pr-field-icon "author" "👤"))
        (insert " ")
        (insert (propertize author 'face 'shipit-username-face))
        (when (not (string-empty-p created-ts))
          (insert " ")
          (insert (propertize created-ts 'face 'shipit-timestamp-face)))
        (insert "\n")
        (insert "  ")
        (insert (shipit--get-pr-field-icon "created" "📅"))
        (insert " Created: ")
        (insert (propertize created-ts 'face 'shipit-timestamp-face))
        (insert " | Updated: ")
        (insert (propertize updated-ts 'face 'shipit-timestamp-face))
        (insert "\n"))

      ;; Labels
      (let ((labels (cdr (assq 'labels issue-data))))
        (when (and labels (> (length labels) 0))
          (insert "  ")
          (insert (shipit--get-pr-field-icon "labels" "🏷"))
          (insert " ")
          (dolist (label labels)
            (insert (propertize (cdr (assq 'name label)) 'face 'shipit-label-face))
            (insert " "))
          (insert "\n")))

      ;; Description
      (let* ((raw-body (or (cdr (assq 'body issue-data)) ""))
             (clean-body (if (and raw-body (not (string-empty-p raw-body)))
                            (let ((cleaned (shipit--clean-text raw-body)))
                              (if (string-match-p "\\`[[:space:]]*\\'" cleaned) nil cleaned))
                          nil)))
        (when clean-body
          (insert "  Description:\n")
          (let* ((description-start (point))
                 (rendered-body (if (and (boundp 'shipit-render-markdown) shipit-render-markdown
                                         (fboundp 'shipit--render-markdown))
                                    (shipit--render-markdown clean-body)
                                  clean-body))
                 (wrapped (if (fboundp 'shipit--wrap-text)
                             (shipit--wrap-text rendered-body 70)
                           rendered-body))
                 (indented (replace-regexp-in-string "\n" "\n  " wrapped)))
            (insert "  ")
            (insert (propertize indented 'face 'default))
            (insert "\n")
            (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
              (shipit--apply-code-block-backgrounds-in-region description-start (point))))))

      ;; Actions
      (insert "  ")
      (insert (propertize "─────────────────────────────" 'face 'shadow))
      (insert "\n")
      (insert "  Actions:\n")
      (dolist (action preview-actions)
        (insert "    ")
        (insert (propertize (car action) 'face 'font-lock-keyword-face))
        (insert (format " - %s\n" (cadr action))))
      (insert "\n")

      (goto-char (point-min))

      ;; Keybindings
      (let* ((map (make-sparse-keymap))
             (open-issue (lambda ()
                           (interactive)
                           (delete-frame frame)
                           (kill-buffer buffer)
                           (select-window original-window)
                           (shipit-issues-open-buffer issue-number (cadr shipit--current-issue-preview-data) (shipit-pr--backend-id)))))
        (define-key map (kbd "RET") open-issue)
        (define-key map (kbd "o") open-issue)
        (define-key map (kbd "b")
          (lambda ()
            (interactive)
            (let* ((repo (cadr shipit--current-issue-preview-data)))
              (delete-frame frame)
              (kill-buffer buffer)
              (select-window original-window)
              (browse-url (shipit--browse-issue-url repo issue-number)))))
        (define-key map (kbd "q")
          (lambda ()
            (interactive)
            (delete-frame frame)
            (kill-buffer buffer)
            (select-window original-window)))
        (use-local-map map))

      (setq buffer-read-only t)
      (setq cursor-type nil))

    (set-window-buffer (frame-selected-window frame) buffer)
    (select-frame-set-input-focus frame)
    frame))

(defun shipit--render-yield ()
  "Refresh the display during a long-running render operation.
Calls `redisplay' so partially-rendered content becomes visible to
the user.  Intended to be called periodically from chunked insertion
loops so the user sees content appearing progressively instead of
waiting for the whole render to finish."
  (redisplay t))

(defcustom shipit-markdown-render-cache-size 500
  "Maximum number of entries in the markdown render cache.
When the cache exceeds this size, it is cleared entirely before
storing the next entry.  Set to 0 to disable caching."
  :type 'integer
  :group 'shipit)

(defvar shipit--markdown-render-cache (make-hash-table :test 'equal :size 256)
  "Cache of rendered markdown keyed by md5 hash of the input text.
Value is the propertized rendered string.  Size capped by
`shipit-markdown-render-cache-size'.")

(defun shipit--markdown-cache-clear ()
  "Clear the markdown render cache."
  (interactive)
  (clrhash shipit--markdown-render-cache))

(defun shipit--render-markdown-uncached (text)
  "Render markdown TEXT without consulting the cache.
This is the actual rendering implementation; callers should use
`shipit--render-markdown' which adds caching."
  (condition-case err
      (with-temp-buffer
        ;; Ensure markdown-mode is available
        (unless (featurep 'markdown-mode)
          (require 'markdown-mode nil t))

        (if (featurep 'markdown-mode)
            (progn
              ;; Step 1: Normalize CRLF
              ;; Step 2: Strip inline HTML tags (e.g., <kbd>) before alignment
              ;; Step 3: Align tables with pandoc
              ;; Step 4: Escape mid-word underscores (e.g., file_name.py)
              ;; Note: Don't process images yet - they'll be done after markdown rendering
              (let* ((normalized (replace-regexp-in-string "\r" "" text))
                     (html-stripped (shipit--strip-inline-html-tags normalized))
                     (html-converted (shipit--convert-inline-html html-stripped))
                     (aligned (shipit--align-markdown-tables-with-pandoc html-converted))
                     (escaped (shipit--escape-mid-word-underscores aligned))
                     (numbered (shipit--auto-increment-ordered-lists escaped)))
                (insert numbered))
              (delay-mode-hooks (markdown-mode))
              (font-lock-ensure)
              ;; Note: Code block backgrounds are applied via overlays AFTER
              ;; text is inserted into the final buffer (see shipit--apply-code-block-backgrounds-in-region)
              ;; because overlays don't transfer with buffer-string
              ;; Step 4: Process inline images AFTER markdown rendering
              ;; This preserves the display properties that would otherwise be lost
              (let ((fontified (buffer-string)))
                (shipit--process-inline-images fontified)))
          ;; Fallback if markdown-mode not available
          text))
    (error
     ;; If anything goes wrong, return original text
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "Markdown rendering failed: %s" err))
     text)))

(defun shipit--render-markdown (text)
  "Apply GitHub markdown rendering to TEXT using markdown-mode's font-lock system.
Caches the result keyed by md5 hash of TEXT to avoid redoing the
expensive font-lock pass for repeated comment bodies.
Cache size is bounded by `shipit-markdown-render-cache-size'."
  (if (not text)
      ""
    (let* ((cache-key (md5 text))
           (cached (gethash cache-key shipit--markdown-render-cache)))
      (or cached
          (let ((result (shipit--render-markdown-uncached text)))
            (when (>= (hash-table-count shipit--markdown-render-cache)
                      shipit-markdown-render-cache-size)
              (clrhash shipit--markdown-render-cache))
            (puthash cache-key result shipit--markdown-render-cache)
            result)))))

(defun shipit--find-matching-details-close (text start-pos)
  "Find the position of the closing </details> that matches the <details> at START-POS.
Returns the position of < in </details>, or nil if not found."
  (let ((depth 1)
        (pos (+ start-pos 9))
        (closing-pos nil))
    (while (and (> depth 0) (< pos (length text)))
      (let ((next-open (string-match "<details>" text pos))
            (next-close (string-match "</details>" text pos)))
        (cond
         ((and next-open next-close (< next-open next-close))
          (setq depth (1+ depth))
          (setq pos (+ next-open 9)))
         ((and next-close (or (not next-open) (< next-close next-open)))
          (setq depth (1- depth))
          (if (= depth 0)
              ;; Found our matching close
              (setq closing-pos next-close))
          (setq pos (+ next-close 10)))
         (t
          (setq pos (length text))
          (setq depth 0)))))
    closing-pos))

(defun shipit--parse-markdown-details (text)
  "Parse markdown details blocks from TEXT.
Returns a list of elements, each either:
  - (details summary body) for a <details> block
  - (text content) for regular text between blocks"
  (let ((result '())
        (pos 0))
    (while (< pos (length text))
      ;; Look for next details block
      (let ((details-start (string-match "<details>" text pos)))
        (if (not details-start)
            ;; No more details blocks, add remaining text
            (progn
              (when (< pos (length text))
                (push (list 'text (substring text pos)) result))
              (setq pos (length text)))
          ;; Found a details block
          ;; First, add any text before this details block
          (when (> details-start pos)
            (push (list 'text (substring text pos details-start)) result))

          ;; Find summary
          (let* ((summary-start (string-match "<summary>" text details-start))
                 (summary-end (string-match "</summary>" text (or summary-start details-start)))
                 (summary (if (and summary-start summary-end)
                             (substring text (+ summary-start 9) summary-end)
                           ""))
                 ;; Find body end - accounts for nested details blocks
                 (details-end (shipit--find-matching-details-close text details-start))
                 ;; Body is everything between summary and closing tag
                 (body-start (if summary-end (+ summary-end 10) (+ details-start 9)))
                 (body (if details-end
                          (substring text body-start details-end)
                        (substring text body-start))))

            ;; Add the details block
            (push (list 'details summary body) result)

            ;; Move position past this details block
            (setq pos (if details-end (+ details-end 10) (length text)))))))

    ;; Reverse to get correct order
    (reverse result)))

(defun shipit--clean-summary-for-heading (summary)
  "Clean SUMMARY text for use as a magit heading.
Removes HTML tags, newlines, and collapses whitespace."
  (let* ((no-tags (replace-regexp-in-string "<[^>]+>" "" summary))
         (no-newlines (replace-regexp-in-string "\n" " " no-tags))
         (collapsed (replace-regexp-in-string "  +" " " no-newlines))
         (trimmed (string-trim collapsed)))
    trimmed))

(defvar shipit--details-section-counter 0)

(defun shipit--insert-body-with-details (body indent-level)
  "Insert BODY content with markdown details blocks as collapsible magit sections.
INDENT-LEVEL is the current indentation level (in spaces).
Recursively handles nested details blocks with proper indentation."
  (let ((parsed (shipit--parse-markdown-details body))
        (indent-str (make-string indent-level ? )))
    ;; Debug: log what was parsed
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "DEBUG INSERT-BODY-WITH-DETAILS: parsed %d blocks, has-details=%s"
                         (length parsed)
                         (seq-some (lambda (b) (eq (car b) 'details)) parsed)))
    (dolist (block parsed)
      (pcase block
        (`(text ,content)
         ;; Regular text - convert limited HTML elements to markdown and render
         (let* (;; Convert HTML tables to markdown first (before other conversions)
                (tables-converted (shipit--convert-html-tables-to-markdown content))
                ;; DEBUG: Log the content after table conversion
                (_ (when (string-match "<table" content)
                     (shipit--debug-log "📊 Content after table conversion (first 500 chars): %s"
                                       (substring tables-converted 0 (min 500 (length tables-converted))))))
                ;; Collapse newlines within list items (HTML treats them as whitespace)
                (li-newlines-collapsed (replace-regexp-in-string
                                        "<li>\\([^<]*\n[^<]*\\)</li>"
                                        (lambda (match)
                                          (concat "<li>"
                                                  (replace-regexp-in-string "\n+" " " (substring match 4 -5))
                                                  "</li>"))
                                        tables-converted))
                ;; Convert HTML list elements to markdown format
                ;; Handles <ol> with numbered items, <ul>/<li> with bullets
                (markdown-converted (shipit--convert-html-lists-to-markdown li-newlines-collapsed))
                ;; Strip blockquotes (just remove the tags)
                (no-blockquotes (replace-regexp-in-string "<blockquote>\\|</blockquote>" "" markdown-converted))
                ;; Add visual marker for horizontal rules (ensure newline before rule)
                ;; Use the same width as the wrap column
                (hr-width (or (and (boundp 'shipit-render-wrap-column) shipit-render-wrap-column) 120))
                (horizontal-rules (replace-regexp-in-string "^---+$"
                                                            (concat "\n" (make-string hr-width ?─))
                                                            no-blockquotes))
                ;; Convert <img> tags to markdown syntax before HTML stripping
                ;; so shipit--render-markdown can process all images after font-lock
                (img-converted (replace-regexp-in-string
                                "<img[^>]*src=\"\\([^\"]+\\)\"[^>]*\\(?:alt=\"\\([^\"]*\\)\"\\)?[^>]*>"
                                (lambda (match)
                                  (save-match-data
                                    (string-match "<img[^>]*src=\"\\([^\"]+\\)\"[^>]*\\(?:alt=\"\\([^\"]*\\)\"\\)?[^>]*>" match)
                                    (let ((src (match-string 1 match))
                                          (alt (or (match-string 2 match) "image")))
                                      (format "![%s](%s)" alt src))))
                                horizontal-rules nil t))
                ;; Strip any remaining HTML tags
                (no-html (replace-regexp-in-string "<[^>]+>" "" img-converted))
                (trimmed (string-trim no-html))
                ;; DEBUG: Log content before and after HTML stripping
                (_ (when (string-match "Status.*Not run" trimmed)
                     (shipit--debug-log "📝 Content with table - before HTML strip (first 800 chars): %s"
                                       (substring horizontal-rules 0 (min 800 (length horizontal-rules))))
                     (shipit--debug-log "📝 Content with table - after HTML strip (first 800 chars): %s"
                                       (substring trimmed 0 (min 800 (length trimmed)))))))
           ;; Only render if there's actual content (not just whitespace)
           (unless (string-empty-p trimmed)
             (let* ((rendered (if shipit-render-markdown
                                (shipit--render-markdown trimmed)
                              trimmed))
                    ;; Wrap long prose lines to fit the configured wrap
                    ;; column minus the current indent so paragraphs do
                    ;; not run off the right edge.
                    (wrap-col (or (and (boundp 'shipit-render-wrap-column)
                                       shipit-render-wrap-column)
                                  120))
                    (wrap-width (max 40 (- wrap-col indent-level)))
                    (wrapped (shipit--wrap-text rendered wrap-width indent-level))
                    ;; Remove blank lines before list items to keep them compact
                    ;; Match blank lines followed by a list item (dash space)
                    ;; Preserves blank lines in other contexts
                    (compact-lists (replace-regexp-in-string "\n\n+\\(- \\)" "\n\\1" wrapped)))
               ;; Add indentation to each line (preserve markdown list nesting)
               (dolist (line (split-string compact-lists "\n"))
                 (let* ((trimmed-line (string-trim-left line))
                        ;; Count leading spaces to preserve markdown nesting for list indentation
                        (leading-spaces (- (length line) (length trimmed-line)))
                        ;; Preserve indentation for nested list items: dash or star, or checkboxes
                        (is-list-item (string-match-p "^[*-][ \t]" trimmed-line))
                        (has-checkbox (string-match-p "\\[[Xx ]\\]" trimmed-line))
                        ;; Keep original leading spaces for nested lists, trim for other content
                        (preserved-line (if (or is-list-item has-checkbox)
                                           line  ; Keep original with indentation for nested lists
                                         trimmed-line)))
                   (if (string-empty-p trimmed-line)
                       ;; Check if original line has display property such as an image
                       (if (text-property-not-all 0 (length line) 'display nil line)
                           (insert indent-str line "\n")
                         ;; Preserve single blank lines for markdown structure
                         (insert "\n"))
                     ;; Indent non-blank lines
                     (insert indent-str preserved-line "\n"))))))))
        (`(details ,summary ,body-content)
         ;; Details block - create as magit-section with unique data
         ;; Recursively render body to handle nested details blocks
         (setq shipit--details-section-counter (1+ shipit--details-section-counter))
         ;; Include indentation as part of the section heading so TAB works correctly
         (magit-insert-section (comment-details shipit--details-section-counter)
           ;; Force details sections to be collapsed by default
           (when magit-insert-section--current
             (oset magit-insert-section--current hidden t))
           (magit-insert-heading (concat indent-str "► " (shipit--clean-summary-for-heading summary)))
           (magit-insert-section-body
             ;; Strip markdown blockquote > prefixes from body so inner
             ;; code blocks and text render correctly
             (let ((clean-body (replace-regexp-in-string
                                "^> ?" "" (string-trim-left body-content)))
                   (body-start (point)))
               (shipit--insert-body-with-details
                clean-body
                (+ indent-level 3))
               ;; Apply syntax highlighting here since magit-insert-section-body
               ;; defers execution for hidden sections — the outer caller's scan
               ;; runs before this content exists in the buffer
               (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
                 (shipit--apply-code-block-backgrounds-in-region body-start (point)))))))))))

(defun shipit--count-comment-lines (comment)
  "Count the number of lines a COMMENT would take when rendered."
  (let* ((raw-body (cdr (assq 'body comment)))
         (body (shipit--clean-comment-text raw-body))
         (rendered-body (if shipit-render-markdown
                            (shipit--render-markdown body)
                          body))
         (expanded-body (if shipit-expand-code-urls
                            (shipit--expand-code-urls rendered-body)
                          rendered-body)))
    ;; Count lines in the expanded content plus header lines
    ;; Images can add extra lines, so we account for them
    (+ 2 ; Header line + timestamp line
       (length (split-string expanded-body "\n"))
       ;; Add extra lines for each image (3 lines per image: label + image + spacing)
       (let ((img-count 0))
         ;; Count HTML img tags
         (with-temp-buffer
           (insert raw-body)
           (goto-char (point-min))
           (while (re-search-forward "<img[^>]+>" nil t)
             (setq img-count (1+ img-count))))
         ;; Count markdown images
         (with-temp-buffer
           (insert raw-body)
           (goto-char (point-min))
           (while (re-search-forward "!\\[[^]]*\\]([^)]+)" nil t)
             (setq img-count (1+ img-count))))
         (* 3 img-count)))))

(defun shipit--insert-single-comment (comment indent-level)
  "Insert a single COMMENT with INDENT-LEVEL spaces of indentation."
  (let* ((user (cdr (assq 'login (cdr (assq 'user comment)))))
         (raw-body (cdr (assq 'body comment)))
         (body (shipit--clean-comment-text raw-body))
         (tables-converted (shipit--convert-html-tables-to-markdown body))
         (created (or (cdr (assq 'created_at comment))
                      (cdr (assq 'submitted_at comment))
                      (cdr (assq 'updated_at comment))))
         (formatted-timestamp (if created (shipit--format-timestamp created) "unknown time"))
         (rendered-body (if shipit-render-markdown
                            (shipit--render-markdown tables-converted)
                          tables-converted))
         (comment-type (cdr (assq 'shipit-comment-type comment)))
         (review-state (cdr (assq 'review-state comment)))
         (expanded-body (if shipit-expand-code-urls
                            (shipit--expand-code-urls rendered-body)
                          rendered-body))
         ;; Provide default message for empty review states (if enabled)
         (final-body (cond
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "APPROVED")
                            (or (not expanded-body) (string-empty-p expanded-body)))
                       "✅ Approved this pull request")
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "CHANGES_REQUESTED")
                            (or (not expanded-body) (string-empty-p expanded-body)))
                       "❌ Requested changes on this pull request")
                      ((and shipit-show-empty-review-messages
                            (string= comment-type "review")
                            (string= review-state "DISMISSED")
                            (or (not expanded-body) (string-empty-p expanded-body)))
                       "🚫 Dismissed previous review")
                      (t expanded-body)))
         (wrapped-body (shipit--wrap-text final-body (- 72 indent-level)))
         (indent-str (make-string indent-level ?\s))
         (body-indent-str (make-string (+ indent-level 2) ?\s))
         (indented-body (shipit--clean-text
                          (replace-regexp-in-string "\n" (concat "\n" body-indent-str) wrapped-body)))
         (comment-id (cdr (assq 'id comment)))
         (orange-color (if (eq (frame-parameter nil 'background-mode) 'light) "#d2691e" "#ff8c00"))
         (user-face `(:foreground ,orange-color :weight bold :extend t))
         (button-face `(:foreground ,orange-color :underline t :extend t))
         (header-icon (cond
                       ((string= comment-type "review") (shipit--get-comment-type-icon "review" "📝"))
                       (t (shipit--get-comment-type-icon "comment" "💬"))))
         (header-prefix (cond
                         ((string= review-state "APPROVED") (concat (shipit--get-approval-status-icon "APPROVED" "✅") " "))
                         ((string= review-state "CHANGES_REQUESTED") (concat (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌") " "))
                         ((string= review-state "DISMISSED") (concat (shipit--get-approval-status-icon "DISMISSED" "🚫") " DISMISSED "))
                         ((string= comment-type "review") (concat (shipit--get-comment-type-icon "review" "💬") " "))
                         (t "")))
         (header-face (cond
                       ((string= review-state "APPROVED") `(:foreground "green" :weight bold :extend t))
                       ((string= review-state "CHANGES_REQUESTED") `(:foreground "red" :weight bold :extend t))
                       ((string= review-state "DISMISSED") `(:foreground "gray" :weight bold :extend t))
                       ((string= comment-type "review") `(:foreground "blue" :weight bold :extend t))
                       (t user-face))))
    ;; Insert comment header with styling and text properties for interaction
    (insert (propertize (format "%s%s %s%s %s\n" indent-str header-icon header-prefix user formatted-timestamp)
                        'face header-face
                        'shipit-comment t
                        'shipit-comment-id comment-id
                        'shipit-comment-body raw-body
                        'shipit-comment-type comment-type))
    ;; Insert comment body with text properties
    (let ((body-start (point)))
      (insert (format "%s%s\n" body-indent-str indented-body))
      ;; Apply text properties to the entire comment body
      (add-text-properties body-start (point)
                           `(shipit-comment t
                                            shipit-comment-id ,comment-id
                                            shipit-comment-body ,raw-body
                                            shipit-comment-body-text t)))
    ;; Insert newline before buttons row (with comment properties so TAB doesn't fail)
    (let ((newline-start (point)))
      (insert "\n")
      (add-text-properties newline-start (point)
                           `(shipit-comment t
                                            shipit-comment-id ,comment-id
                                            shipit-comment-body ,raw-body)))
    ;; Insert reaction and edit buttons with reactions on same line
    (let ((reactions-text (shipit--format-comment-reactions comment nil)))
      (if reactions-text
          ;; If we have reactions, add proper indentation then reactions
          (let ((reactions-start (point)))
            (insert body-indent-str)
            (insert (substring reactions-text 3)) ; Remove built-in 3 spaces since we're adding our own
            (add-text-properties reactions-start (point)
                                 `(shipit-reactions t
                                                    shipit-comment-id ,comment-id)))
        ;; If no reactions, add indentation to align buttons with comment text
        (insert body-indent-str)))
    (insert (propertize "[+]"
                        'face button-face
                        
                        'help-echo "Click to add a reaction"
                        'keymap (let ((map (make-sparse-keymap)))
                                  (set-keymap-parent map (current-local-map))
                                  (define-key map [mouse-1]
                                              (lambda (event)
                                                (interactive "e")
                                                (shipit--toggle-reaction-interactive comment-id)))
                                  (define-key map [return]
                                              (lambda ()
                                                (interactive)
                                                (shipit--toggle-reaction-interactive comment-id)))
                                  map)))
    (insert (propertize " [edit]"
                        'face button-face
                        
                        'help-echo "Click to edit this comment"
                        'keymap (let ((map (make-sparse-keymap)))
                                  (set-keymap-parent map (current-local-map))
                                  (define-key map [mouse-1]
                                              (lambda (event)
                                                (interactive "e")
                                                (shipit--edit-comment-interactive comment-id raw-body)))
                                  (define-key map [return]
                                              (lambda ()
                                                (interactive)
                                                (shipit--edit-comment-interactive comment-id raw-body)))
                                  map)))
    (insert "\n\n")))

;;;###autoload
(defun shipit-test-avatar-system ()
  "Test the avatar system and report what's working."
  (interactive)
  (message "=== SHIPIT AVATAR SYSTEM TEST ===")
  (message "shipit-show-avatars: %s" shipit-show-avatars)
  (message "shipit-round-avatars: %s" shipit-round-avatars)
  (message "display-images-p: %s" (display-images-p))
  (message "ImageMagick available: %s" (if (fboundp 'shipit--imagemagick-available-p)
                                           (shipit--imagemagick-available-p)
                                         "Function not found"))
  (let ((test-url "https://avatars.githubusercontent.com/u/1?v=4")
        (test-username "octocat"))
    (message "Testing download with: %s" test-url)
    (let ((result (shipit--download-and-cache-avatar test-url test-username nil)))
      (message "Download result: %s" result)
      (when result
        (message "File exists: %s" (file-exists-p result))
        (when (file-exists-p result)
          (message "File size: %d bytes" (nth 7 (file-attributes result)))))))
  (message "Testing avatar display...")
  (let ((display-result (shipit--create-avatar-display "octocat" "https://avatars.githubusercontent.com/u/1?v=4")))
    (message "Display result: %s" (if (string= display-result "👤 ") "EMOJI FALLBACK" "IMAGE CREATED")))
  (message "=== END TEST ==="))

;; Final compatibility fallbacks to ensure functions are always available

;;; Unified Comment Renderer

(defun shipit--render-comment-header (comment depth context)
  "Render a comment header string for COMMENT at indentation DEPTH in CONTEXT.
COMMENT is an alist with keys: id, user, created_at, body.
DEPTH is the indentation level (unused in current implementation, reserved for future use).
CONTEXT is a symbol indicating the rendering context (e.g., 'inline, 'general).

Returns a formatted header string containing:
- Icon (based on shipit-inline-comment-icon-style)
- Avatar (if shipit-show-avatars is enabled)
- Username
- Timestamp
- [RESOLVED] suffix (if comment is in resolved thread)
- [OUTDATED] suffix (if comment is outdated)
- Red dot indicator if comment is unread

The returned string does not include a trailing newline."
  (let* ((user-obj (or (cdr (assq 'user comment)) (cdr (assq 'author comment))))
         (user (or (cdr (assq 'login user-obj)) "Unknown User"))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (created (or (cdr (assq 'created_at comment))
                     (cdr (assq 'submitted_at comment))
                     (cdr (assq 'updated_at comment))))
         (formatted-timestamp (if created (shipit--format-timestamp created) "unknown time"))
         (comment-id (cdr (assq 'id comment)))
         (is-resolved (and comment-id (shipit--is-comment-in-resolved-thread comment-id)))
         (is-outdated (cdr (assq 'outdated comment)))
         (icon (shipit--get-comment-icon comment context))
         (avatar-display (if (and shipit-show-avatars avatar-url)
                            (shipit--create-avatar-display user avatar-url 16)
                          ""))
         (avatar-space (if (and shipit-show-avatars avatar-url) " " ""))
         (resolved-suffix (if is-resolved " [RESOLVED]" ""))
         (outdated-suffix (if is-outdated " [OUTDATED]" ""))
         (header-without-suffixes (format "%s%s%s%s (%s)"
                                         (or icon "")
                                         avatar-display
                                         avatar-space
                                         user
                                         formatted-timestamp)))
    ;; NOTE: Unread indicator is NOT included here - it's added separately by callers
    ;; after any face styling is applied (to preserve the red color)
    (concat header-without-suffixes resolved-suffix outdated-suffix)))

(defun shipit--get-comment-unread-indicator (comment-id)
  "Return red dot indicator if COMMENT-ID is unread, empty string otherwise.
This should be called AFTER any face styling is applied to preserve the red color.
Uses buffer-local variables when available (in shipit-mode buffers) for correct
behavior when switching between multiple PR buffers."
  ;; Prefer buffer-local variables over global shipit--current-displayed-pr
  ;; to handle switching between multiple shipit buffers correctly
  (let* ((pr-number (or (and (boundp 'shipit-buffer-pr-number) shipit-buffer-pr-number)
                        (car-safe shipit--current-displayed-pr)))
         (repo (or (and (boundp 'shipit-buffer-repo) shipit-buffer-repo)
                   (cadr shipit--current-displayed-pr)))
         (is-unread (and shipit-show-unread-indicators
                         comment-id pr-number repo
                         (shipit--is-comment-unread-p repo pr-number comment-id))))
    (shipit--debug-log "GET-UNREAD-INDICATOR: comment-id=%s pr=%s repo=%s show=%s is-unread=%s"
                       comment-id pr-number repo shipit-show-unread-indicators is-unread)
    (if is-unread
        (propertize " ●" 'face '(:foreground "red")
                    'shipit-unread-comment t
                    'shipit-unread-comment-id comment-id
                    'shipit-comment-id comment-id)  ; Also add shipit-comment-id for cursor detection
      "")))

(defun shipit--get-comment-icon (comment context)
  "Get the appropriate icon for COMMENT based on CONTEXT and user settings."
  (pcase shipit-inline-comment-icon-style
    ('text
     (if (equal (cdr (assq 'shipit-comment-type comment)) "review") "[R] " "[C] "))
    ('avatar
     (when shipit-show-avatars
       (let* ((user-obj (cdr (assq 'user comment)))
              (user (cdr (assq 'login user-obj)))
              (avatar-url (cdr (assq 'avatar_url user-obj))))
         (concat (shipit--create-avatar-display user avatar-url 16) " "))))
    ('emoji
     (if (equal (cdr (assq 'shipit-comment-type comment)) "review")
         (concat (shipit--get-comment-type-icon "review" "📋") " ")
       (concat (shipit--get-comment-type-icon "comment" "💬") " ")))
    (_ (concat (shipit--get-comment-type-icon "comment" "💬") " "))))

(defun shipit--svg-lib-icon-with-bg (name face-or-nil &rest args)
  "Call svg-lib-icon with NAME, FACE-OR-NIL and ARGS, adding theme-aware background.
Automatically sets :background to the default face background if not specified."
  (let ((bg (face-background 'default nil 'default)))
    (apply #'svg-lib-icon name face-or-nil
           (append args (list :background bg)))))

(defun shipit--get-reactions-placeholder-icon ()
  "Get reactions placeholder icon - octicon smiley from svglib if enabled and available, grey emoji fallback.
Returns either an image (from svglib) or a string (emoji fallback)."
  (cond
    ;; Try svglib octicon-smiley first (if svglib is enabled and available)
    ((and shipit-use-svglib-icons
          (featurep 'svg-lib)
          (fboundp 'svg-lib-icon))
     (condition-case err
         ;; Render octicon-smiley (GitHub's icon for reactions) using the shipit-comment-reactions-placeholder face
         ;; svg-lib-icon returns an image object that needs to be wrapped in a string for concat
         ;; Reduce brightness with lower contrast background, thinner stroke, and subtle border
         (let ((icon (shipit--svg-lib-icon-with-bg "smiley" 'shipit-comment-reactions-placeholder :collection "octicons" :height 1.0 :width 1.0 :padding 0 :radius 20 :stroke 0.5)))
           (if (imagep icon)
               ;; Image object: create a string with the image as a display property
               (propertize " " 'display icon)
             ;; Fallback to icon as-is if it's already a string
             icon))
       (error
        ;; Fall back to emoji if svglib rendering fails
        (shipit--debug-log "Failed to render SVG icon: %s, using emoji fallback" err)
        (propertize "➕" 'face 'shipit-comment-reactions-placeholder))))
    ;; Fallback: Use grey plus sign emoji (clearly indicates "add reaction")
    (t
     (propertize "➕" 'face 'shipit-comment-reactions-placeholder))))

(defun shipit--event-type-to-octicon (event-type)
  "Map activity EVENT-TYPE to appropriate GitHub octicon name.
Returns the octicon name string, or nil if no mapping exists."
  (pcase event-type
    ("reviewed" "comment-discussion")  ; Generic review/approval icon
    ("commented" "comment")              ; Chat bubble for comments
    ("review_requested" "eye")           ; Eye for review requests
    ("labeled" "tag")                    ; Tag for labels
    ("unlabeled" "tag")                  ; Tag for label removal
    ("assigned" "person")                ; Person for assignment
    ("unassigned" "person")              ; Person for unassignment
    ("closed" "x-circle")                ; X circle for closing
    ("reopened" "history")               ; Refresh/history for reopening
    ("merged" "git-merge")               ; Merge icon for merge events
    ("committed" "git-commit")           ; Commit icon for commits
    (_ nil)))

(defun shipit--event-type-to-icon-color (event-type)
  "Map activity EVENT-TYPE to an appropriate color for its icon.
Returns a color name (hex or named color)."
  (pcase event-type
    ("reviewed" "#28a745")              ; Green - approval
    ("commented" "#0366d6")             ; Blue - comment/discussion
    ("review_requested" "#6f42c1")      ; Purple - pending action
    ("labeled" "#d4af37")               ; Gold - label
    ("unlabeled" "#d4af37")             ; Gold - label removal
    ("assigned" "#ff9800")              ; Orange - assignment
    ("unassigned" "#ff9800")            ; Orange - unassignment
    ("closed" "#cb2431")                ; Red - closed/rejected
    ("reopened" "#28a745")              ; Green - reopened
    ("merged" "#6f42c1")                ; Purple - merged
    ("committed" "#808080")             ; Gray - commit
    (_ "#999999")))


(defun shipit--emoji-to-event-type (emoji)
  "Map activity emoji to event type for SVG icon rendering.
Used when emoji is passed directly instead of event type."
  (pcase emoji
    ("💬" "commented")
    ("👀" "reviewed")
    ("🏷" "labeled")
    ("👤" "assigned")
    ("❌" "closed")
    ("🔄" "review_requested")
    ("🎉" "merged")
    ("📝" "committed")
    (_ nil)))

(defun shipit--activity-emoji-to-icon (emoji)
  "Convert activity emoji to SVG icon using event type mapping.
Falls back to emoji if no SVG available."
  (let ((event-type (shipit--emoji-to-event-type emoji)))
    (if event-type
        (shipit--get-activity-event-icon event-type emoji)
      emoji)))

(defun shipit--check-status-to-octicon-color (status conclusion)
  "Map check STATUS and CONCLUSION to octicon name and color.
Returns (octicon . color) or nil if no mapping."
  (let ((icon-color
         (cond
          ((string= conclusion "success") '("check" . "#28a745"))           ; Green - Success (positive)
          ((string= conclusion "failure") '("x" . "#cb2431"))              ; Red - Failure (blocking)
          ((string= conclusion "cancelled") '("circle-slash" . "#999999")) ; Gray - Cancelled (removed)
          ((string= conclusion "skipped") '("circle-slash" . "#999999"))   ; Gray - Skipped (removed)
          ((string= conclusion "neutral") '("circle" . "#999999"))         ; Gray - Neutral (unknown)
          ((string= status "in_progress") '("dot-fill" . "#fd7e14"))       ; Orange - In Progress (pending)
          ((string= status "queued") '("dot-fill" . "#0366d6"))            ; Blue - Queued (informational)
          (t '("question" . "#999999")))))                                 ; Gray - Unknown
    icon-color))

(defun shipit--get-check-status-icon (status conclusion emoji-fallback)
  "Get SVG icon for check STATUS and CONCLUSION, with emoji fallback.
EMOJI-FALLBACK is the emoji string to use if SVG unavailable."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--check-status-to-octicon-color status conclusion)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error
                 (shipit--debug-log "Failed to render check status SVG: %s, using emoji" err)
                 emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--get-checks-status-emoji-svg (checks emoji-fallback)
  "Return SVG icon representing overall status of CHECKS, with emoji fallback."
  (if (not checks)
      emoji-fallback ; No checks - return hourglass emoji
    (let ((has-failing nil)
          (has-in-progress nil)
          (has-pending nil)
          (has-cancelled nil)
          (has-successful nil))
      ;; Categorize all checks
      (dolist (check checks)
        (let* ((status (cdr (assq 'status check)))
               (conclusion (cdr (assq 'conclusion check))))
          (cond
           ((string= conclusion "failure") (setq has-failing t))
           ((string= conclusion "cancelled") (setq has-cancelled t))
           ((string= conclusion "success") (setq has-successful t))
           ((member status '("in_progress" "running")) (setq has-in-progress t))
           ((member status '("queued" "requested" "waiting")) (setq has-pending t))
           ((not (member status '("completed"))) (setq has-pending t)))))
      ;; Get the overall status and render as SVG
      (let ((status (cond
                     (has-failing "failure")
                     (has-in-progress "in_progress")
                     (has-pending "pending")
                     (has-cancelled "cancelled")
                     (has-successful "success")
                     (t "unknown")))
            (conclusion (cond
                         (has-failing "failure")
                         (has-cancelled "cancelled")
                         (has-successful "success")
                         (t nil))))
        (shipit--get-check-status-icon status conclusion emoji-fallback)))))

(defun shipit--approval-status-to-icon-color (approval-status)
  "Map APPROVAL-STATUS to octicon name and color.
Returns (octicon . color) or nil."
  (pcase approval-status
    ("APPROVED" '("check" . "#28a745"))           ; Green checkmark
    ("CHANGES_REQUESTED" '("x" . "#cb2431"))      ; Red X
    ("DISMISSED" '("prohibition" . "#999999"))    ; Gray prohibition
    ("REVIEW_REQUIRED" '("clock" . "#fd7e14"))    ; Orange clock - waiting
    ("UNKNOWN" '("question" . "#999999"))         ; Gray question mark
    (_ nil)))

(defun shipit--get-approval-status-icon (approval-status emoji-fallback)
  "Get SVG icon for APPROVAL-STATUS, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--approval-status-to-icon-color approval-status)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--comment-type-to-icon-color (comment-type)
  "Map COMMENT-TYPE to octicon name and color.
Returns (octicon . color) or nil."
  (pcase comment-type
    ("review" '("comment-discussion" . "#0366d6"))  ; Blue - review comment
    ("comment" '("comment" . "#fd7e14"))            ; Orange - general comment (discussion collection)
    (_ nil)))

(defun shipit--get-comment-type-icon (comment-type emoji-fallback)
  "Get SVG icon for COMMENT-TYPE, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--comment-type-to-icon-color comment-type)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--user-type-to-icon-color (user-type)
  "Map USER-TYPE to octicon name and color for user/reviewer indicators.
Returns (octicon . color) or nil."
  (pcase user-type
    ("single" '("person" . "#fd7e14"))    ; Orange person - Assignees (metadata)
    ("team" '("people" . "#fd7e14"))      ; Orange people - Team assignment (metadata)
    (_ nil)))

(defun shipit--get-user-type-icon (user-type emoji-fallback)
  "Get SVG icon for USER-TYPE, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--user-type-to-icon-color user-type)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--label-icon-color ()
  "Get SVG icon for labels.
Returns (octicon . color) or nil."
  '("tag" . "#999999"))  ; Gray tag - Labels (secondary metadata)

(defun shipit--get-label-icon (emoji-fallback)
  "Get SVG icon for labels, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--label-icon-color)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

;;; PR field icons (for PR header sections)
(defun shipit--pr-field-to-icon-color (field-type)
  "Map PR FIELD-TYPE to octicon name and color.
Returns (octicon . color) or nil."
  (pcase field-type
    ("pull-request" '("git-pull-request" . "#0366d6"))  ; Blue - Informational (primary identifier)
    ("description" '("comment-discussion" . "#fd7e14")) ; Orange - Discussion content
    ("draft" '("git-branch" . "#fd7e14"))               ; Orange - Draft status (metadata)
    ("refs" '("git-branch" . "#0366d6"))                ; Blue - Informational (branches)
    ("created" '("calendar" . "#999999"))               ; Gray - Metadata (timestamp)
    ("author" '("person" . "#0366d6"))                  ; Blue - Informational
    ("committer" '("tools" . "#999999"))                ; Gray - Committer (metadata)
    ("committed" '("calendar" . "#999999"))             ; Gray - Committed date (metadata)
    ("state" '("dot-fill" . "#28a745"))                 ; Green - Open/positive state
    ("approval" '("person-add" . "#cb2431"))            ; Red - Approval (blocking/decision)
    ("checks" '("checklist" . "#0366d6"))               ; Blue - Informational (CI status)
    ("reviewer" '("eye" . "#0366d6"))                   ; Blue - Reviewer (informational)
    ("assignee" '("person" . "#fd7e14"))                ; Orange - Assignee (metadata)
    ("general-comments" '("comment-discussion" . "#fd7e14")) ; Orange - Discussion content
    ("issue-open" '("issue-opened" . "#28a745"))             ; Green - Open issue
    ("issue-closed" '("issue-closed" . "#cb2431"))           ; Red - Closed issue
    ("metadata" '("info" . "#999999"))                       ; Gray - Metadata
    ("updated" '("calendar" . "#999999"))                    ; Gray - Updated timestamp
    ("assignees" '("people" . "#fd7e14"))                    ; Orange - Assignees
    ("labels" '("tag" . "#0366d6"))                          ; Blue - Labels
    ("milestone" '("milestone" . "#0366d6"))                 ; Blue - Milestone
    ("comment" '("comment-discussion" . "#fd7e14"))          ; Orange - Comments
    ("type" '("issue-draft" . "#6f42c1"))                    ; Purple - Issue type
    ("parent" '("north-star" . "#6f42c1"))                   ; Purple - Parent/epic
    ("links" '("link" . "#0366d6"))                          ; Blue - Issue links
    ("children" '("tasklist" . "#6f42c1"))                    ; Purple - Child work items
    ("activity" '("history" . "#fd7e14"))                    ; Orange - Activity/changelog
    ("components" '("package" . "#0366d6"))                  ; Blue - Components
    ("discussion" '("comment-discussion" . "#0366d6"))       ; Blue - Discussion (open)
    ("discussion-answered" '("check-circle" . "#28a745"))    ; Green - Discussion (answered)
    ("category" '("apps" . "#6f42c1"))                       ; Purple - Category
    ("upvotes" '("arrow-up" . "#fd7e14"))                     ; Orange - Upvotes
    ("github" '("mark-github" . "#999999"))                    ; Gray - GitHub backend
    ("gitlab" '("gitlab" "#e24329" "simple"))                  ; Orange - GitLab backend (simple-icons)
    ("repo" '("repo" . "#0366d6"))                               ; Blue - Repository
    ("branch" '("git-branch" . "#28a745"))                       ; Green - Branch
    ("language" '("code" . "#6f42c1"))                           ; Purple - Language
    ("stats" '("graph" . "#0366d6"))                              ; Blue - Statistics
    ("project" '("project" . "#0366d6"))                            ; Blue - Project/board
    ("clock" '("clock" . "#999999"))                                ; Gray - Time/recent
    ("issues" '("issue-opened" . "#28a745"))                        ; Green - Issues list
    ("notification" '("bell" . "#0366d6"))                           ; Blue - Notification/subscription
    ("star" '("star-fill" . "#e3b341"))                              ; Yellow - Starred
    (_ nil)))

(defun shipit--get-pr-field-icon (field-type emoji-fallback)
  "Get SVG icon for PR FIELD-TYPE, with emoji fallback.
Icon data from `shipit--pr-field-to-icon-color' is either
  (ICON-NAME . COLOR) for octicons, or
  (ICON-NAME COLOR COLLECTION) for other collections."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (shipit--debug-log "get-pr-field-icon: field-type=%s emoji=%s use-svglib=%s" field-type emoji-fallback use-svglib)
    (if use-svglib
        (let ((icon-data (shipit--pr-field-to-icon-color field-type)))
          (shipit--debug-log "  icon-data=%s" icon-data)
          (if icon-data
              (let* ((icon-name (car icon-data))
                     (color (if (consp (cdr icon-data)) (cadr icon-data) (cdr icon-data)))
                     (collection (or (and (consp (cdr icon-data)) (caddr icon-data))
                                     "octicons")))
                (condition-case err
                    (let ((icon (shipit--svg-lib-icon-with-bg icon-name nil
                                                              :collection collection
                                                              :height 1.0 :width 1.0 :padding 0.1
                                                              :foreground color :stroke 0)))
                      (shipit--debug-log "  icon created: %s" (if (imagep icon) "YES" "NO"))
                      (if (imagep icon)
                          (propertize " " 'display icon)
                        emoji-fallback))
                  (error (shipit--debug-log "  error creating icon: %s" err) emoji-fallback)))
            (shipit--debug-log "  no icon-data found")
            emoji-fallback))
      (shipit--debug-log "  not using svglib, returning emoji")
      emoji-fallback)))

(defun shipit--pr-state-to-icon-color (state)
  "Map PR STATE to octicon name and color.
Returns (octicon . color) or nil.
STATE should be one of: open, closed, merged, draft."
  (pcase state
    ("open" '("git-pull-request" . "#28a745"))    ; Green - Open PR
    ("closed" '("git-pull-request-closed" . "#cb2431")) ; Red - Closed PR
    ("merged" '("git-merge" . "#8250df"))         ; Purple - Merged PR (GitHub's purple)
    ("draft" '("git-pull-request-draft" . "#6e7781")) ; Gray - Draft PR
    (_ '("git-pull-request" . "#999999"))))       ; Default gray

(defun shipit--get-pr-state-icon (state emoji-fallback)
  "Get SVG icon for PR STATE, with emoji fallback.
STATE should be one of: open, closed, merged, draft."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--pr-state-to-icon-color state)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--files-icon-color ()
  "Get SVG icon for files/changes.
Returns (octicon . color) or nil."
  '("file" . "#6f42c1"))  ; Purple file icon - Distinctive/Special changes

(defun shipit--get-files-icon (emoji-fallback)
  "Get SVG icon for files/changes, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--files-icon-color)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--worktree-status-to-icon-color (status)
  "Map worktree STATUS to octicon name and color.
Returns (octicon . color) or nil.
STATUS should be one of: 'none, 'in-sync, 'out-of-sync, 'different-repo"
  (pcase status
    ('none '("git-branch" . "#999999"))           ; Gray - No worktree
    ('in-sync '("check" . "#28a745"))             ; Green - In sync (positive/success)
    ('out-of-sync '("alert" . "#cb2431"))         ; Red - Out of sync (blocking)
    ('different-repo '("git-branch" . "#999999")))) ; Gray - PR from different repo

(defun shipit--get-worktree-status-icon (status emoji-fallback)
  "Get SVG icon for worktree STATUS, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--worktree-status-to-icon-color status)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--activity-event-type-to-icon-color (event-type state)
  "Map activity EVENT-TYPE and STATE to octicon name and color.
Returns (octicon . color) or nil."
  (pcase event-type
    ("reviewed" (pcase state
                  ("approved" '("check" . "#28a745"))          ; Green - Approved (positive)
                  ("changes_requested" '("x" . "#cb2431"))     ; Red - Changes Requested (blocking)
                  ("commented" '("comment-discussion" . "#0366d6")) ; Blue - Comment
                  (_ '("comment-discussion" . "#0366d6"))))
    ("commented" '("comment-discussion" . "#0366d6"))          ; Blue - Discussion/Comment
    ("review_requested" '("person" . "#0366d6"))               ; Blue - Review Request (informational)
    ("labeled" '("tag" . "#fd7e14"))                           ; Orange - Labels (metadata/organization)
    ("unlabeled" '("tag" . "#999999"))                         ; Gray - Unlabeled (removal)
    ("assigned" '("person-add" . "#fd7e14"))                   ; Orange - Assignment (metadata/organization)
    ("unassigned" '("person" . "#999999"))                     ; Gray - Unassigned (removal)
    ("closed" '("issue-closed" . "#cb2431"))                   ; Red - Closed (blocking/negative)
    ("reopened" '("issue-reopened" . "#28a745"))               ; Green - Reopened (positive action)
    ("merged" '("git-merge" . "#8250df"))                      ; Purple - Merged (matches GitHub)
    ("committed" '("git-commit" . "#6f42c1"))                  ; Purple - Commit (distinctive/special)
    ("head_ref_force_pushed" '("repo-push" . "#6e7781"))       ; Gray - Force push (neutral/informational)
    (_ '("pin" . "#999999"))))

(defun shipit--get-activity-event-icon (event-type &optional state emoji-fallback)
  "Get SVG icon for activity event, with emoji fallback.
EVENT-TYPE: The GitHub activity event type (e.g., 'reviewed', 'commented')
STATE: Optional review state (e.g., 'approved', 'changes_requested') for review events
EMOJI-FALLBACK: Emoji to use if SVG rendering fails
Can be called with 2 args (event-type emoji-fallback) or 3 args (event-type state emoji-fallback)"
  ;; Handle both 2-arg and 3-arg calling conventions
  (when (and (stringp state) (not emoji-fallback))
    ;; Called with (event-type emoji-fallback) - state is actually emoji-fallback
    (setq emoji-fallback state)
    (setq state nil))

  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--activity-event-type-to-icon-color event-type state)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--activity-icon-color ()
  "Get SVG icon for activity timeline.
Returns (octicon . color) or nil."
  '("list-unordered" . "#6f42c1"))  ; Purple list icon - Distinctive/special timeline

(defun shipit--get-activity-icon (emoji-fallback)
  "Get SVG icon for activity timeline, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--activity-icon-color)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--notification-type-icon-color (type)
  "Return (octicon . color) for notification TYPE."
  (pcase type
    ("pr"         '("git-pull-request" . "#28a745"))    ; Green
    ("pr-draft"   '("git-pull-request-draft" . "#8b949e")) ; Grey
    ("pr-merged"  '("git-merge" . "#8250df"))              ; Purple
    ("pr-closed"  '("git-pull-request-closed" . "#da3633")) ; Red
    ("issue"      '("issue-opened" . "#8250df"))         ; Purple
    ("discussion" '("comment-discussion" . "#0366d6"))   ; Blue
    ("rss"        '("rss" . "#ee802f"))                  ; Orange
    (_ nil)))

(defun shipit--get-notification-type-icon (type emoji-fallback)
  "Get SVG icon for notification TYPE, with EMOJI-FALLBACK."
  (let ((use-svglib (and (display-graphic-p)
                         shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--notification-type-icon-color type)))
          (if icon-color
              (condition-case _err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--notification-icon-color ()
  "Get SVG icon for notifications.
Returns (octicon . color) or nil."
  '("bell" . "#0366d6"))  ; Blue bell icon

(defun shipit--get-notification-icon (emoji-fallback)
  "Get SVG icon for notifications, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--notification-icon-color)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--backend-icon-plist (source)
  "Return the backend plist for SOURCE that contains :icon-spec.
Checks PR backends first (primary context for icons), then issue backends."
  (let ((id (or source 'github)))
    (or (cdr (assq id (and (boundp 'shipit-pr-backends) shipit-pr-backends)))
        (cdr (assq id (and (boundp 'shipit-issue-backends) shipit-issue-backends))))))

(defun shipit--notification-source-icon-spec (source)
  "Return (icon-name collection . color) for notification SOURCE backend.
SOURCE is a symbol like \\='github, \\='gitlab, \\='forgejo, or nil."
  (let ((plist (shipit--backend-icon-plist source)))
    (or (plist-get plist :icon-spec)
        '("mark-github" "octicons" . "#888888"))))

(defun shipit--get-notification-source-icon (source)
  "Get icon for notification SOURCE backend.
SOURCE is a symbol like \\='github, \\='gitlab, \\='forgejo, or nil.
Returns an SVG icon string when svg-lib is available, text fallback otherwise."
  (let* ((plist (shipit--backend-icon-plist source))
         (fallback-text (or (plist-get plist :icon-fallback-text) "??"))
         (icon-spec (or (plist-get plist :icon-spec)
                        '("mark-github" "octicons" . "#888888")))
         (color (cddr icon-spec))
         (fallback (propertize fallback-text
                              'face (if color
                                        (list :foreground color)
                                      'font-lock-comment-face)))
         (use-svglib (and (display-graphic-p)
                         shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((spec (shipit--notification-source-icon-spec source)))
          (condition-case _err
              (let ((icon (shipit--svg-lib-icon-with-bg
                           (car spec) nil
                           :collection (cadr spec)
                           :height 1.0 :width 1.0 :padding 0.1
                           :foreground (cddr spec) :stroke 0)))
                (if (imagep icon)
                    (propertize " " 'display icon)
                  fallback))
            (error fallback)))
      fallback)))

(defun shipit--status-icon-color (status-type)
  "Map STATUS-TYPE to octicon name and color.
Returns (octicon . color) or nil."
  (pcase status-type
    ("waiting" '("clock" . "#ffc107"))    ; Yellow clock - waiting
    ("pending" '("dot-fill" . "#0366d6")) ; Blue dot - pending
    (_ nil)))

(defun shipit--get-status-icon (status-type emoji-fallback)
  "Get SVG icon for status, with emoji fallback."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (let ((icon-color (shipit--status-icon-color status-type)))
          (if icon-color
              (condition-case err
                  (let ((icon (shipit--svg-lib-icon-with-bg (car icon-color) nil
                                                            :collection "octicons"
                                                            :height 1.0 :width 1.0 :padding 0.1
                                                            :foreground (cdr icon-color) :stroke 0)))
                    (if (imagep icon)
                        (propertize " " 'display icon)
                      emoji-fallback))
                (error emoji-fallback))
            emoji-fallback))
      emoji-fallback)))

(defun shipit--insert-inline-comment-section (comment threads depth base-indent context)
  "Insert COMMENT as a magit section with proper nesting for parent/child relationships.
COMMENT is an alist containing comment data (id, user, body, etc.).
THREADS is a hash table mapping comment-id to list of reply comments.
DEPTH is the current nesting level (0 for root comments).
BASE-INDENT is the base indentation level in spaces.
CONTEXT is a symbol indicating the rendering context (e.g., 'inline, 'ediff).

This function creates a proper magit section hierarchy where:
- Each comment is a section of type shipit-inline-comment
- The section contains a header with comment metadata
- magit-insert-section-body wraps the body, reactions, and nested replies
- Recursive calls create child sections for replies
- TAB folding works automatically due to parent/child relationships"
  (let* ((comment-id (cdr (assq 'id comment)))
         (indent-str (make-string (+ base-indent (* depth 2)) ?\s))
         (replies (gethash comment-id threads)))
    ;; Create magit section for this comment
    (magit-insert-section (shipit-inline-comment comment-id)
      ;; Insert header using shared rendering function
      (let ((header-start (point)))
        (magit-insert-heading
          (concat indent-str (shipit--render-comment-header comment depth context) "\n"))
        ;; Add unread indicator at end of heading line (before newline)
        (save-excursion
          (goto-char header-start)
          (end-of-line)
          (insert (shipit--get-comment-unread-indicator comment-id))))
      ;; Use magit-insert-section-body to contain body, reactions, and nested replies
      (magit-insert-section-body
        ;; Insert comment body with proper indentation
        (shipit--insert-comment-body-only comment (+ base-indent (* depth 2)) nil nil nil nil nil t)
        ;; Insert reactions (body-only skips them, so we insert them here)
        (let ((reactions (shipit--format-comment-reactions comment t)))
          (when reactions
            (let ((reactions-start (point)))
              (insert (concat indent-str reactions "\n"))
              (add-text-properties reactions-start (point)
                                   `(shipit-reactions t
                                                      shipit-comment-id ,comment-id)))))
        ;; Recursively insert nested replies as child sections
        (when replies
          (dolist (reply (reverse replies))
            (shipit--insert-inline-comment-section reply threads (1+ depth) base-indent context)))))))

;; ============================================================================
;; TABLE ALIGNMENT: Space-Based Manual Alignment (No Colons)
;; ============================================================================

(defun shipit--dom-find-tag (node tag-name)
  "Find first child NODE with TAG-NAME, recursively if needed."
  (let ((children (dom-children node)))
    (catch 'found
      (dolist (child children)
        (when (listp child)
          (when (and (symbolp (car child))
                    (eq (car child) tag-name))
            (throw 'found child))
          (when-let ((result (shipit--dom-find-tag child tag-name)))
            (throw 'found result)))))))

(defun shipit--dom-find-all-tags (node tag-name)
  "Find all child NODEs with TAG-NAME (direct children only)."
  (let ((result '())
        (children (dom-children node)))
    (dolist (child children)
      (when (and (listp child)
                (symbolp (car child))
                (eq (car child) tag-name))
        (push child result)))
    (reverse result)))

(defun shipit--dom-get-cell-text (cell)
  "Extract text content from a table cell NODE.
For images, extracts the alt attribute. For links with images, extracts link text or image alt."
  (if (listp cell)
      (let ((text ""))
        (dolist (child (dom-children cell))
          (cond
           ((stringp child)
            (let ((trimmed (string-trim child)))
              (unless (string-empty-p trimmed)
                (setq text (concat text trimmed)))))
           ((listp child)
            ;; Check if this is an img element - extract alt text
            (if (and (symbolp (car child)) (eq (car child) 'img))
                (let ((alt (cdr (assq 'alt (cadr child)))))
                  (when alt
                    (setq text (concat text alt))))
              ;; Otherwise recursively process
              (setq text (concat text (shipit--dom-get-cell-text child)))))))
        (string-trim text))
    (if (stringp cell) (string-trim cell) "")))

(defun shipit--flatten-table-headers (table-node)
  "Flatten multi-row headers combining parent headers with children.
Handles colspan and rowspan attributes for proper column mapping."
  (let ((thead (shipit--dom-find-tag table-node 'thead))
        (all-trs (shipit--dom-find-all-tags table-node 'tr))
        (header-rows '()))
    (if thead
        (setq header-rows (shipit--dom-find-all-tags thead 'tr))
      (let ((first-row (car all-trs)))
        (when (and first-row (shipit--dom-find-all-tags first-row 'th))
          (setq header-rows (list first-row)))))

    ;; Handle single vs multi-row headers
    (if (<= (length header-rows) 1)
        ;; Single row: expand colspan cells
        (let ((flattened '()))
          (dolist (cell (shipit--dom-find-all-tags (car header-rows) 'th))
            (let ((text (shipit--dom-get-cell-text cell))
                  (colspan-count (string-to-number (or (cdr (assq 'colspan (cadr cell))) "1"))))
              (push text flattened)
              (dotimes (_ (1- colspan-count))
                (push "" flattened))))
          (reverse flattened))
      ;; Multi-row headers: combine parent and child smartly
      (let ((parent-row (car header-rows))
            (child-row (cadr header-rows)))
        ;; Build expanded parent cells and track rowspan columns
        (let ((parent-cells '())
              (rowspan-cols '())
              (current-col 0))
          ;; Expand parent cells with colspan and track rowspan
          (dolist (cell (shipit--dom-find-all-tags parent-row 'th))
            (let ((text (shipit--dom-get-cell-text cell))
                  (colspan-count (string-to-number (or (cdr (assq 'colspan (cadr cell))) "1")))
                  (rowspan-count (string-to-number (or (cdr (assq 'rowspan (cadr cell))) "1"))))
              ;; Add parent text for each column it spans
              (dotimes (_ colspan-count)
                (push text parent-cells)
                ;; Track columns occupied by rowspan > 1
                (when (> rowspan-count 1)
                  (push current-col rowspan-cols))
                (setq current-col (1+ current-col)))))
          (setq parent-cells (reverse parent-cells))
          (setq rowspan-cols (reverse rowspan-cols))

          ;; Extract raw child cells
          (let ((raw-child-cells '()))
            (dolist (cell (shipit--dom-find-all-tags child-row 'th))
              (let ((text (shipit--dom-get-cell-text cell)))
                (push text raw-child-cells)))
            (setq raw-child-cells (reverse raw-child-cells))

            ;; Build expanded child cells with nils at rowspan positions
            (let ((child-cells '())
                  (child-idx 0))
              (dotimes (col (length parent-cells))
                (if (member col rowspan-cols)
                    ;; Column occupied by parent rowspan - no child cell
                    (push nil child-cells)
                  ;; Regular child cell
                  (if (< child-idx (length raw-child-cells))
                      (progn
                        (push (nth child-idx raw-child-cells) child-cells)
                        (setq child-idx (1+ child-idx)))
                    ;; No more child cells
                    (push "" child-cells))))
              (setq child-cells (reverse child-cells))

              ;; Combine: "Parent: Child" intelligently
              (let ((result '()))
                (dotimes (i (length parent-cells))
                  (let ((parent (nth i parent-cells))
                        (child (nth i child-cells)))
                    (cond
                     ;; Both exist, different, and parent is not empty
                     ((and parent child (not (string-empty-p parent))
                           (not (string-empty-p child)) (not (string= parent child)))
                      (push (concat parent ": " child) result))
                     ;; Only parent exists (child is nil/empty due to rowspan)
                     ((and parent (not child))
                      (push parent result))
                     ;; Child exists and parent is empty/nil
                     ((and child (or (not parent) (string-empty-p parent)))
                      (push child result))
                     ;; Both empty/nil
                     (t
                      (push "" result)))))
                (reverse result)))))))))

(defun shipit--build-aligned-text-table (headers rows)
  "Build a text table with manual space-based alignment.
No markdown alignment markers (colons) - uses pure space padding.
HEADERS is a list of column headers.
ROWS is a list of lists where each inner list is a row of cells."
  (if (not headers)
      ""
    (let* ((num-cols (length headers))
           (col-widths (make-vector num-cols 0))
           table-lines)
      ;; Calculate column widths from headers (use string-width for emoji support)
      (dotimes (i num-cols)
        (aset col-widths i (max (aref col-widths i) (string-width (nth i headers)))))

      ;; Calculate column widths from data rows (use string-width for emoji support)
      (dolist (row rows)
        (dotimes (i num-cols)
          (aset col-widths i (max (aref col-widths i) (string-width (or (nth i row) ""))))))

      ;; Build header row with space padding
      (let ((header-cells '()))
        (dotimes (i num-cols)
          (let ((header (nth i headers))
                (width (aref col-widths i)))
            (push (concat header (make-string (- width (string-width header)) ?\s)) header-cells)))
        (push (concat "| " (mapconcat 'identity (reverse header-cells) " | ") " |") table-lines))

      ;; Build separator row with matching dashes - same structure as header
      (let ((separators '()))
        (dotimes (i num-cols)
          (let ((width (aref col-widths i)))
            (push (make-string width ?-) separators)))
        (push (concat "| " (mapconcat 'identity (reverse separators) " | ") " |") table-lines))

      ;; Build data rows with space padding
      (dolist (row rows)
        (let ((row-cells '()))
          (dotimes (i num-cols)
            (let ((cell (or (nth i row) ""))
                  (width (aref col-widths i)))
              (push (concat cell (make-string (- width (string-width cell)) ?\s)) row-cells)))
          (push (concat "| " (mapconcat 'identity (reverse row-cells) " | ") " |") table-lines)))

      ;; Join lines
      (mapconcat 'identity (reverse table-lines) "\n"))))

(provide 'shipit-render)
;;; shipit-render.el ends here
