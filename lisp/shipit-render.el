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
(require 'shipit-issue-titles)  ; For shared eldoc title cache/fetcher
(require 'shipit-pr-backends)  ; For PR backend dispatch in #NNN routing
(require 'shipit-canvas)  ; For canvas-mermaid, which draws mermaid pictures

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
(defvar shipit-code-block-detect-shebang)

;; Forward declarations for functions from other modules
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
(declare-function shipit--get-current-user "shipit-pr-actions")
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
  (let ((parts (list (propertize "Suggested change:" 'font-lock-face 'font-lock-comment-face))))
    (when old-lines
      (dolist (old-line old-lines)
        (push (propertize (concat "- " old-line) 'font-lock-face 'diff-removed) parts)))
    (dolist (new-line (split-string new-code "\n"))
      (push (propertize (concat "+ " new-line) 'font-lock-face 'diff-added) parts))
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
                                   "\n" (propertize (format "[%s]" clean-lang) 'font-lock-face 'font-lock-comment-face)
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
          (put-text-property user-start user-end 'font-lock-face 'shipit-inline-comment-username-face))
        ;; Also apply the general username face
        (put-text-property user-start user-end 'font-lock-face 'shipit-username-face)))

    ;; Insert separator and timestamp with face
    (insert " • ")
    (let ((timestamp-start (point)))
      (insert timestamp-str)
      (let ((timestamp-end (point)))
        ;; Apply conditional inline comment face if enabled
        (when shipit-inline-comment-faces
          (put-text-property timestamp-start timestamp-end 'font-lock-face 'shipit-inline-comment-timestamp-face))
        ;; Also apply the general timestamp face
        (put-text-property timestamp-start timestamp-end 'font-lock-face 'shipit-timestamp-face)))

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
              (put-text-property line-start (- (point) 1) 'font-lock-face 'shipit-inline-comment-body-face))))))

    ;; Insert spacing between comments (1 blank line = 1 newline)
    (insert "\n")))

(defun shipit--apply-blockquote-faces (start end)
  "Apply blockquote styling to lines that start with > in region START to END.
A > later in a line, as in an arrow ->, marks no quote.
Writes the blue color to `font-lock-face' (via
`shipit--prepend-font-lock-face') so magit-section's
`font-lock-mode' doesn't strip it on refontification --
`add-face-text-property' targets `face', which font-lock owns
and clears."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-start (point))
            (line-text (thing-at-point 'line t))
            (line-end (progn (end-of-line) (point))))
        (when (and line-text (string-match-p "\\`[ \t]*>" line-text))
          (shipit--prepend-font-lock-face
           line-start line-end 'shipit-blockquote-face))
        (forward-line 1)))))

(defun shipit--apply-strikethrough-faces (start end)
  "Apply strikethrough styling to ~text~ patterns in region START to END.
Supports both single tilde ~text~ and double tilde ~~text~~ (GFM syntax).
Writes `shipit-strikethrough-face' to `font-lock-face' (via
`shipit--prepend-font-lock-face') so magit-section's
`font-lock-mode' doesn't strip it on refontification --
`add-face-text-property' targets `face', which font-lock owns
and clears.

A ~~...~~ span may cross newlines: the body wrapper breaks long
struck sentences onto indented continuation lines, so requiring
the pair on one line silently drops exactly the spans long enough
to wrap.  A blank line (paragraph break) inside the pair
disqualifies it, matching GFM.  Single-tilde pairs stay
line-bound -- across lines they are far more likely to be two
unrelated tildes (paths, approximations) than markup.
Line-edge whitespace inside the span stays unstruck so wrapped
spans don't draw a line through continuation-line indentation."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\(~~\\([^~]+?\\)~~\\|~\\([^~\n]+\\)~\\)" end t)
      (if (and (match-beginning 2)
               (string-match-p "\n[ \t]*\n" (match-string 2)))
          ;; Paragraph break inside ~~...~~ -- not strikethrough;
          ;; resume just past the opening marker
          (goto-char (+ (match-beginning 0) 2))
        ;; Apply strikethrough face to the entire match including tildes
        (shipit--prepend-font-lock-face-by-line
         (match-beginning 0) (match-end 0) 'shipit-strikethrough-face)))))

(defun shipit--prepend-font-lock-face-by-line (start end face)
  "Prepend FACE to `font-lock-face' between START and END, line by line.
Unlike `shipit--prepend-font-lock-face', newlines and each line's
leading/trailing whitespace are left untouched, so a face spanning
wrapped lines doesn't paint over the continuation lines'
indentation or past the text's edge."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (skip-chars-forward " \t\n" end)
      (when (< (point) end)
        (let* ((seg-start (point))
               (line-end (min end (line-end-position)))
               (seg-end (progn (goto-char line-end)
                               (skip-chars-backward " \t" seg-start)
                               (point))))
          (when (< seg-start seg-end)
            (shipit--prepend-font-lock-face seg-start seg-end face))
          (goto-char line-end)
          (unless (>= (point) end)
            (forward-char 1)))))))

(defun shipit--apply-comment-body-decorations (body-start body-end &optional repo pr-number)
  "Apply markdown body decorations to the region between BODY-START and BODY-END.
Bundles the post-insertion pass shared by every comment-body
renderer: blockquote face, strikethrough face, code-block
backgrounds, PR/commit/user-mention overlays (when REPO is
provided -- PR-NUMBER additionally enables `#NN' references),
Jira mention overlays, custom URL patterns, and generic URL
overlays.  Centralising the calls means new renderers
(activity-event bodies, etc.) get the same styling as inline
comments without copy-pasting the list of post-processing steps."
  (when (and (integer-or-marker-p body-start)
             (integer-or-marker-p body-end)
             (< body-start body-end))
    (shipit--apply-blockquote-faces body-start body-end)
    (shipit--apply-strikethrough-faces body-start body-end)
    (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
      (shipit--apply-code-block-backgrounds-in-region body-start body-end))
    (shipit--create-comment-body-overlays body-start body-end repo pr-number)))

(defun shipit--create-comment-body-overlays (body-start body-end &optional repo pr-number)
  "Make the links of a comment body between BODY-START and BODY-END.
PR/commit/user-mention overlays when REPO is provided (PR-NUMBER
additionally enables `#NN' references), Jira mention overlays when
the Jira backend is loaded, custom URL patterns, and generic URL
overlays.  The Jira backend is loaded only when Jira is configured."
  (when (and repo pr-number (fboundp 'shipit--create-pr-reference-overlays))
    (shipit--create-pr-reference-overlays repo pr-number body-start body-end))
  (when (and repo (fboundp 'shipit--create-commit-sha-overlays))
    (shipit--create-commit-sha-overlays repo body-start body-end))
  (when (and repo (fboundp 'shipit--create-user-mention-overlays))
    (shipit--create-user-mention-overlays repo body-start body-end))
  (when (fboundp 'shipit--create-jira-mention-overlays)
    (shipit--create-jira-mention-overlays body-start body-end))
  (when (fboundp 'shipit--create-custom-url-overlays)
    (shipit--create-custom-url-overlays body-start body-end))
  (when (fboundp 'shipit--create-generic-url-overlays)
    (shipit--create-generic-url-overlays body-start body-end)))

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
                            (shipit--render-body body)
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
         ;; Cap inline images (screenshots, badges, gifs) to the 80-col
         ;; wrap envelope used by the comment text.  Measured here in
         ;; the destination buffer's face — the renderer otherwise
         ;; falls back to its `with-temp-buffer' default which can
         ;; differ from this buffer's.
         (shipit--image-target-width
          (or (and (boundp 'shipit--image-target-width) shipit--image-target-width)
              (if (fboundp 'string-pixel-width)
                  (string-pixel-width (make-string 80 ?M))
                (* 80 (frame-char-width)))))
         ;; Use the common rendering function
         (indented-body (shipit--render-comment-body comment indent-level))
         (body-indent-str (make-string (+ indent-level 0) ?\s)))

    ;; Insert comment body with text properties for M-; and RET functionality
    (let* ((body-start (point))
           (body-keymap (or (and (keymapp custom-keymap) custom-keymap)
                            (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map (current-local-map))
                              (define-key map [return] 'shipit--file-at-point)
                              (define-key map [C-return] 'shipit--open-file-from-worktree-at-point)
                              (define-key map (kbd "C-RET") 'shipit--open-file-from-worktree-at-point)
                              (define-key map (kbd "SPC") #'shipit-page-menu)
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
      (shipit--apply-comment-body-decorations body-start (point) repo pr-number)
      ;; Add shipit properties to the entire comment body
      (add-text-properties body-start (point)
                           `(shipit-comment t
                                            shipit-comment-id ,(cdr (assq 'id comment))
                                            shipit-comment-body ,raw-body
                                            shipit-comment-body-text t
                                            shipit-comment-type ,comment-type
                                            ,@(when background-face `(font-lock-face ,background-face))
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
                            (shipit--render-body tables-converted)
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
                      (propertize user 'font-lock-face 'shipit-username-face)
                      (propertize formatted-timestamp 'font-lock-face 'shipit-timestamp-face)))
      ;; Add styling and shipit properties to entire header line
      (add-text-properties header-start (point)
                           `(font-lock-face ,header-face
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
                                  (define-key map [C-return] 'shipit--open-file-from-worktree-at-point)
                                  (define-key map (kbd "C-RET") 'shipit--open-file-from-worktree-at-point)
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

(defun shipit--prefetch-avatars-async (comments)
  "Prefetch avatars for users in COMMENTS in parallel.
For each comment, fire an async `url-retrieve' for the author's
avatar and, on success, write the response body to the shipit
avatar cache directory.  Subsequent synchronous renders find the
cache file on disk and skip the blocking HTTP round trip — this is
the entire benefit; results are otherwise ignored.

Skips comments whose avatar is already cached fresh (as decided by
`shipit--avatar-needs-refresh-p').  Fire-and-forget: errors are
swallowed because the sync render path will still work for any
avatar the prefetch missed."
  (let* ((cache-dir (expand-file-name "shipit-avatars" user-emacs-directory))
         (seen (make-hash-table :test 'equal)))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))
    (dolist (comment comments)
      (let* ((user-obj (cdr (assq 'user comment)))
             (avatar-url (cdr (assq 'avatar_url user-obj)))
             (username (cdr (assq 'login user-obj))))
        (when (and avatar-url username
                   (not (gethash username seen)))
          (puthash username t seen)
          (let* ((ext (if (string-match-p "\\.png" avatar-url) ".png" ".jpg"))
                 (cache-file (expand-file-name (concat username ext) cache-dir)))
            (when (shipit--avatar-needs-refresh-p cache-file username)
              (shipit--prefetch-avatar-one avatar-url cache-file username))))))))

(defun shipit--prefetch-avatar-one (avatar-url cache-file username)
  "Kick off an async download of AVATAR-URL into CACHE-FILE for USERNAME.
The response body is written to CACHE-FILE and the ETag timestamp is
recorded so `shipit--avatar-needs-refresh-p' will return nil on the
next call within an hour."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("User-Agent" . "shipit.el"))))
    (condition-case err
        (url-retrieve
         avatar-url
         (lambda (status &rest _)
           (condition-case inner
               (unless (plist-get status :error)
                 (goto-char (point-min))
                 (let (etag data-start)
                   (save-excursion
                     (goto-char (point-min))
                     (while (and (not (looking-at "^\r?$"))
                                 (zerop (forward-line 1)))
                       (when (looking-at "^[Ee][Tt]ag: *\"?\\([^\"]+\\)\"?")
                         (setq etag (match-string 1)))))
                   (when (search-forward "\n\n" nil t)
                     (setq data-start (point))
                     (let ((coding-system-for-write 'binary))
                       (write-region data-start (point-max)
                                     cache-file nil 'silent))
                     (when etag
                       (puthash (format "avatar:%s" username)
                                (list :etag etag :timestamp (float-time))
                                shipit-gh-etag--persistent-cache)))))
             (error
              (shipit--debug-log "prefetch-avatar callback failed for %s: %s"
                                 username (error-message-string inner))))
           (when (buffer-live-p (current-buffer))
             (kill-buffer (current-buffer))))
         nil t t)
      (error
       (shipit--debug-log "prefetch-avatar kickoff failed for %s: %s"
                          username (error-message-string err))))))

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

(defvar shipit--image-target-width nil
  "Pixel width caller wants images (especially animated GIFs) sized to.
Bound dynamically by callers that have a live buffer context — e.g.,
the README inserter measures `string-pixel-width' for an 80-char string
in the destination buffer's face and binds this.  Inside the markdown
render path the current buffer is a `with-temp-buffer', so measurements
taken there use the temp buffer's face which may differ from the
destination.  When unbound the renderer falls back to its own
heuristics.")

(defun shipit--display-image (display)
  "Return the image that the display property DISPLAY shows, or nil.
DISPLAY is an image, or a list that has one, as a strip of an image
has.  The strips of one image share it."
  (cond ((eq (car-safe display) 'image) display)
        ((consp display)
         (seq-find (lambda (part) (eq (car-safe part) 'image)) display))))

(defun shipit--animate-images-in-region (start end)
  "Animate any multi-frame images displayed via text properties in START..END.
The render pipeline creates images inside `with-temp-buffer', so calling
`image-animate' there would bind the timer to a buffer that dies as soon
as the temp buffer is unwound.  Callers run this helper after inserting
the rendered text into the destination buffer; the timer then captures
that live buffer and the animation actually plays."
  (save-excursion
    (let ((pos start)
          (started nil))
      (while (and pos (< pos end))
        (let ((image (shipit--display-image (get-text-property pos 'display))))
          (when (and image
                     (not (memq image started))
                     (fboundp 'image-multi-frame-p)
                     (image-multi-frame-p image))
            (push image started)
            (image-animate image 0 t)))
        (setq pos (next-single-property-change pos 'display nil end))))))

(defun shipit--gif-resize-command (file width dest)
  "Return the command that writes GIF FILE WIDTH pixels wide as DEST, or nil.
It runs gifsicle, or else ImageMagick, whichever is on the path.
ImageMagick coalesces the frames first: in an optimized GIF a frame
holds only what changed, at its own place."
  (let ((size (format "%dx" width)))
    (cond
     ((executable-find "gifsicle")
      (list "gifsicle" (format "--resize-width=%d" width) file "-o" dest))
     ((executable-find "magick")
      (list "magick" file "-coalesce" "-resize" size dest))
     ((executable-find "convert")
      (list "convert" file "-coalesce" "-resize" size dest)))))

(defun shipit--prepare-gif-for-display (file target-width)
  "Return a path to a GIF FILE suitable for display, pre-resized if needed.
Animated GIFs scaled at display time often hit Emacs's tardiness limit
(2s cumulative) and the animation aborts, and each frame costs time to
draw.  When FILE's natural width exceeds TARGET-WIDTH, and gifsicle or
ImageMagick is available, this resizes the file once to a sibling cache
path and returns that.  Otherwise returns FILE unchanged.  Failures
fall back to the original file."
  (let* ((dim (shipit--gif-pixel-dimensions file))
         (dest (concat (file-name-sans-extension file)
                       (format "-w%d.gif" target-width)))
         (command (and dim (car dim) (> (car dim) target-width)
                       (shipit--gif-resize-command file target-width dest))))
    (if (not command)
        file
      (unless (file-exists-p dest)
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "GIF-RESIZE: %s w=%d -> %s w=%d with %s"
                             file (car dim) dest target-width (car command)))
        (condition-case err
            (apply #'call-process (car command) nil nil nil (cdr command))
          (error
           (when (fboundp 'shipit--debug-log)
             (shipit--debug-log "GIF-RESIZE failed: %s" err)))))
      (if (file-exists-p dest) dest file))))

(defun shipit--gif-pixel-dimensions (file)
  "Return (WIDTH . HEIGHT) of GIF FILE in pixels by parsing its header.
GIF89a / GIF87a store the canvas size as two little-endian 16-bit
values at byte offsets 6 (width) and 8 (height).  Returns nil if FILE
isn't a GIF or the header can't be read."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 10)
        (let ((bytes (buffer-string)))
          (when (and (>= (length bytes) 10)
                     (or (equal (substring bytes 0 6) "GIF87a")
                         (equal (substring bytes 0 6) "GIF89a")))
            (cons (+ (aref bytes 6) (* 256 (aref bytes 7)))
                  (+ (aref bytes 8) (* 256 (aref bytes 9)))))))
    (error nil)))

(defun shipit--detect-image-type-from-content (file)
  "Detect image type from FILE content by reading magic bytes.
Returns symbol like `png', `jpeg', `gif', `webp', `svg', or nil if unknown."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 256)
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
           ;; SVG: starts with `<?xml' or `<svg' (after optional whitespace/BOM).
           ;; shields.io badges and many README icons are served as SVG with no
           ;; extension hint in the URL, so content sniffing is the only signal.
           ((string-match-p "\\`[\t\n\r ]*\\(<\\?xml\\|<svg\\b\\)" bytes)
            'svg)
           (t nil))))
    (error nil)))

(defvar shipit--image-base-repo nil
  "Repo name for resolving relative image URLs during rendering.")

(defcustom shipit-render-videos t
  "Whether to render inline video links (.mp4 / .webm / .mov / .avi).
When non-nil and ffmpeg is available, video URLs in body text are
replaced with a still thumbnail plus a clickable `▶ Play Video'
button that opens the video via `shipit-video-player-command'.
Without ffmpeg the button is shown without a thumbnail."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-video-player-command '("mpv" "--autofit-larger=960x540")
  "Command used to play video URLs invoked from the buffer.
A list of strings — the URL is appended as the final argument.
Defaults to mpv capped at 960x540 (mpv keeps aspect ratio and only
shrinks down to that size, never up).  Switch to a percentage like
`--autofit=50%' if you prefer screen-relative sizing — be aware it
sums multi-monitor area on some setups.  When the configured program
is not on `exec-path', `browse-url' is used as a fallback."
  :type '(repeat string)
  :group 'shipit)

(defcustom shipit-video-embed-in-frame t
  "When non-nil, embed mpv into an Emacs child frame instead of its own window.
Requires an X11 build of mpv (`--wid' support) and an X11 Emacs frame.
The child frame is centered over the current frame, sized to
`shipit-video-embed-frame-size', and deleted automatically when mpv
exits.  If the window-id can't be obtained the call falls back to the
standalone mpv launch — set to nil to skip the child-frame attempt
entirely (e.g. on Wayland without XWayland)."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-video-embed-frame-size '(960 . 540)
  "(WIDTH . HEIGHT) in pixels for the embedded video child frame."
  :type '(cons integer integer)
  :group 'shipit)

(defcustom shipit-video-inline-function 'canvas-browser-embed
  "Function that plays a video in the buffer, under its Play button, or nil.
It is called with the URL of a page that plays the video, a width and a
height in pixels, and the buffer.  It returns the text to insert, or
nil when it cannot play there, as in a terminal frame; the video then
plays outside the buffer.  canvas-browser gives such a function."
  :type '(choice (const :tag "Outside the buffer" nil) function)
  :group 'shipit)

(defcustom shipit-video-inline-show t
  "Whether a video shows in the buffer as soon as the buffer is ready.
It shows under its Play button, paused on its first frame, and a click
on it plays it.  Nil leaves each video to its Play button."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-video-inline-width 640
  "Width in pixels of a video played in the buffer, at most.
A narrower window gets a narrower video.  The height is 9/16 of it."
  :type 'integer
  :group 'shipit)

(defconst shipit--video-extension-regexp "\\.\\(?:mp4\\|webm\\|mov\\|avi\\)"
  "Regexp matching the extension of a video file name.")

(defconst shipit--video-url-regexp
  (concat "\\bhttps?://[^][ \t\n<>\"'`)]+" shipit--video-extension-regexp "\\b")
  "Regexp matching bare video URLs in body text.")

(defconst shipit--video-link-regexp
  (concat "\\[\\([^]\n]*" shipit--video-extension-regexp "\\)\\](\\([^)\n]+\\))")
  "Regexp matching a markdown link whose text names a video file.
Group 1 is the name and group 2 the URL, which need not name the file:
a tracker serves an attachment from a URL of its own.")

(defun shipit--ffmpeg-thumbnail-for-url (url)
  "Extract a single-frame thumbnail for URL via ffmpeg.
Caches the result alongside images.  Returns the cached file path
or nil when ffmpeg is unavailable, fails, or `display-images-p' is
nil.  Uses an early seek (-ss 1) to skip black intro frames."
  (when (and (display-images-p) (executable-find "ffmpeg"))
    (let* ((cache-dir (expand-file-name "shipit-images"
                                        temporary-file-directory))
           (url-hash (secure-hash 'md5 url))
           (cache-file (expand-file-name (concat url-hash "-thumb.png")
                                         cache-dir)))
      (unless (file-directory-p cache-dir)
        (make-directory cache-dir t))
      (unless (file-exists-p cache-file)
        (condition-case err
            (call-process "ffmpeg" nil nil nil
                          "-y" "-loglevel" "error"
                          "-ss" "1" "-i" url
                          "-frames:v" "1"
                          "-vf" "scale=600:-2"
                          cache-file)
          (error
           (when (fboundp 'shipit--debug-log)
             (shipit--debug-log "VIDEO-THUMB: ffmpeg failed for %s: %s"
                                url err)))))
      (and (file-exists-p cache-file)
           (> (file-attribute-size (file-attributes cache-file)) 0)
           cache-file))))

(defvar shipit--video-active-frame nil
  "Child frame currently hosting an embedded mpv instance, if any.")

(defvar shipit--video-active-process nil
  "mpv process currently rendering into `shipit--video-active-frame'.")

(defun shipit--video-cleanup-active ()
  "Tear down any existing embedded mpv frame and process."
  (when (and shipit--video-active-process
             (process-live-p shipit--video-active-process))
    (set-process-sentinel shipit--video-active-process #'ignore)
    (delete-process shipit--video-active-process))
  (setq shipit--video-active-process nil)
  (when (and shipit--video-active-frame
             (frame-live-p shipit--video-active-frame))
    (delete-frame shipit--video-active-frame))
  (setq shipit--video-active-frame nil))

(defun shipit-video-stop ()
  "Tear down the currently embedded mpv child frame, if any."
  (interactive)
  (shipit--video-cleanup-active))

(defun shipit--play-video-in-child-frame (url)
  "Embed mpv into an X11 child frame and play URL.
Returns t when the embed was launched, nil if the frame couldn't
expose an `outer-window-id' and the caller should fall back to a
standalone window.  Tears down any previously-embedded session
before creating a new one so retries don't accumulate frames."
  (shipit--video-cleanup-active)
  (let* ((parent (selected-frame))
         (size shipit-video-embed-frame-size)
         (target-w (car size))
         (target-h (cdr size))
         (parent-w (frame-pixel-width parent))
         (parent-h (frame-pixel-height parent))
         (child (make-frame
                 `((parent-frame . ,parent)
                   (no-accept-focus . nil)
                   (minibuffer . nil)
                   (undecorated . t)
                   (internal-border-width . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (background-color . "black")
                   (left . ,(max 0 (/ (- parent-w target-w) 2)))
                   (top . ,(max 0 (/ (- parent-h target-h) 2)))))))
    (set-frame-size child target-w target-h t)
    (make-frame-visible child)
    (redisplay t)
    (let ((wid (frame-parameter child 'outer-window-id)))
      (cond
       ((not wid)
        (delete-frame child)
        nil)
       (t
        (let* ((bufname "*shipit-mpv*")
               (buf (get-buffer-create bufname))
               (proc (apply #'start-process "shipit-mpv" buf
                            "mpv" (concat "--wid=" wid) "--vo=x11" "--no-osc"
                            (list url))))
          (with-current-buffer buf
            (let ((inhibit-read-only t)) (erase-buffer)))
          (setq shipit--video-active-frame child)
          (setq shipit--video-active-process proc)
          (set-process-sentinel
           proc
           (lambda (p event)
             (let ((status (process-exit-status p)))
               (when (string-match-p "exited abnormally" event)
                 (message "mpv exited (code %s) — see %s for output"
                          status bufname)))
             (when (frame-live-p child)
               (delete-frame child))
             (setq shipit--video-active-frame nil)
             (setq shipit--video-active-process nil)))
          t))))))

(defun shipit--play-video-url (url &optional place)
  "Play video URL, under its Play button at PLACE when the buffer can.
PLACE is a marker on the button.  A video playing there already stops.
A video the buffer cannot play, or one with no PLACE, plays outside it;
see `shipit--play-video-outside'.  A tracker's attachment plays from the
URL the tracker hands out for it, which needs no login."
  (interactive
   (list (or (get-text-property (point) 'shipit-video-url)
             (read-string "Video URL: "))))
  (if (and place (shipit--inline-video-at place))
      (shipit--press-inline-video place)
    (let ((url (or (shipit--media-url url) url)))
      (unless (and place (shipit--play-video-inline url place t))
        (shipit--play-video-outside url)))))

(defun shipit--inline-video-start (place)
  "Return where the video under the Play button at PLACE starts, or would.
That is the end of the button's line."
  (with-current-buffer (marker-buffer place)
    (save-excursion (goto-char place) (line-end-position))))

(defun shipit--inline-video-at (place)
  "Return non-nil when a video plays under the Play button at PLACE."
  (with-current-buffer (marker-buffer place)
    (get-text-property (shipit--inline-video-start place) 'shipit-video-inline)))

(defun shipit--press-inline-video (place)
  "Press RET on the video under the Play button at PLACE: it plays or pauses.
The video's own text says what RET does there."
  (with-current-buffer (marker-buffer place)
    (let* ((start (shipit--inline-video-start place))
           (end (next-single-property-change start 'shipit-video-inline nil (point-max)))
           (frame (text-property-not-all start end 'keymap nil))
           (command (and frame (lookup-key (get-text-property frame 'keymap) (kbd "RET")))))
      (when (commandp command)
        (save-excursion
          (goto-char frame)
          (call-interactively command))))))

(defun shipit--inline-video-size (buffer)
  "Return (WIDTH . HEIGHT) for a video played in BUFFER, in pixels.
At most `shipit-video-inline-width', less when a window of BUFFER is
narrower, and 16 by 9."
  (let* ((window (get-buffer-window buffer t))
         (room (if window (- (window-body-width window t) 60) shipit-video-inline-width))
         (width (max 400 (min shipit-video-inline-width room))))
    (cons width (round (* width 9) 16))))

(defun shipit--play-video-inline (url place &optional autoplay)
  "Show URL on the line under the Play button at PLACE; nil when it cannot.
With AUTOPLAY it plays at once; without, it waits on its first frame for
a click."
  (when (functionp shipit-video-inline-function)
    (let* ((buffer (marker-buffer place))
           (size (shipit--inline-video-size buffer))
           (text (funcall shipit-video-inline-function
                          (shipit--video-page url autoplay) (car size) (cdr size) buffer)))
      (when text
        (with-current-buffer buffer
          (save-excursion
            (goto-char (shipit--inline-video-start place))
            (let ((inhibit-read-only t))
              (insert (propertize (concat "\n   " text) 'shipit-video-inline t)))))
        t))))

(defun shipit--play-video-outside (url)
  "Play URL outside the buffer: in mpv, or else in the browser.
When `shipit-video-embed-in-frame' is non-nil, embed mpv in a child
frame; otherwise launch `shipit-video-player-command' as its own
window, and without a player play it in the browser.  One that still
needs a login plays in the browser, which may hold that login."
  (let* ((cmd shipit-video-player-command)
         (program (car cmd)))
    (cond
     ;; A player has no login, and one on its command line would be
     ;; readable in the process list; the browser may hold its own.
     ((shipit--login-headers url)
      (shipit--play-video-in-browser url))
     ((and shipit-video-embed-in-frame
           (executable-find "mpv")
           (shipit--play-video-in-child-frame url))
      (message "Playing %s in child frame" url))
     ((and program (executable-find program))
      (apply #'call-process program nil 0 nil
             (append (cdr cmd) (list url)))
      (message "Playing: %s" url))
     (t (shipit--play-video-in-browser url)))))

(defun shipit--video-page (url &optional autoplay)
  "Write a local page whose video element plays URL; return its file URL.
With AUTOPLAY the video starts at once, muted, since a browser may
refuse to start one with its sound.  Without, it loads and shows its
first frame.  The page may hold a short-lived token in URL, so only its
owner can read it."
  (let ((file (with-file-modes #o600 (make-temp-file "shipit-video-" nil ".html"))))
    ;; Written here, silently: with its text `make-temp-file' says "Wrote".
    (write-region (format (concat "<!doctype html><meta charset=\"utf-8\"><title>Video</title>"
                                  "<body style=\"margin:0;overflow:hidden;background:#000\">"
                                  "<video src=\"%s\" controls %s"
                                  " style=\"display:block;width:100%%;height:100vh\"></video>")
                          (url-insert-entities-in-string url)
                          (if autoplay "autoplay muted" "preload=\"auto\""))
                  nil file nil 'silent)
    (concat "file://" file)))

(defun shipit--video-button-places ()
  "Return a marker on each visible Play button of this buffer, in order."
  (let ((places nil)
        (position (point-min)))
    (while (setq position (text-property-any position (point-max) 'shipit-video-button t))
      (unless (invisible-p position)
        (push (copy-marker position) places))
      (setq position (or (next-single-property-change position 'shipit-video-button)
                         (point-max))))
    (nreverse places)))

(defun shipit--show-inline-videos ()
  "Show each video of this buffer under its Play button, paused.
One shown already, or one whose button is hidden, as in a shut section,
is left as it is."
  (when (and shipit-video-inline-show (functionp shipit-video-inline-function))
    (dolist (place (shipit--video-button-places))
      (unless (shipit--inline-video-at place)
        (let ((url (get-text-property place 'shipit-video-url)))
          (shipit--play-video-inline (or (shipit--media-url url) url) place))))))

(defvar-local shipit--videos-timer nil
  "The timer that shows this buffer's videos, while one is pending.")

(defun shipit--show-inline-videos-in (buffer)
  "Show the videos of BUFFER, if it is still there."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shipit--videos-timer nil)
      (shipit--show-inline-videos))))

(defun shipit--show-inline-videos-soon ()
  "Show the videos of this buffer once Emacs is idle, once for many calls.
A tracker's video asks the tracker for its URL first, so the buffer is
shown before that."
  (unless (timerp shipit--videos-timer)
    (setq shipit--videos-timer
          (run-with-idle-timer 0.2 nil #'shipit--show-inline-videos-in (current-buffer)))))

(defun shipit--video-buttons-inserted (beg end _length)
  "Show the videos soon when text inserted from BEG to END has a Play button.
In `after-change-functions' of a buffer that shows videos.  Its text
arrives in parts, and again: the description, then the comments, more
comments, and a section drawn again; each brings its buttons."
  (when (and (< beg end) (text-property-any beg end 'shipit-video-button t))
    (shipit--show-inline-videos-soon)))

(defun shipit--watch-videos ()
  "Show this buffer's videos whenever text with a Play button arrives."
  (add-hook 'after-change-functions #'shipit--video-buttons-inserted nil t))

(defun shipit--play-video-in-browser (url)
  "Play URL in the browser, in a page of its own with a video element.
Opening URL itself would not do: a browser downloads a video served as
an attachment, as a tracker serves one, instead of playing it."
  (browse-url (shipit--video-page url)))

(defun shipit--event-place (event)
  "Return a marker on what EVENT, a click or a key, was pressed on."
  (if (mouse-event-p event)
      (let ((start (event-start event)))
        (set-marker (make-marker) (posn-point start) (window-buffer (posn-window start))))
    (point-marker)))

(defun shipit--video-button-keymap (url)
  "Return a keymap for the `▶ Play Video' button, bound to play URL.
Binds both \\`(kbd \"RET\")' (TTY/control-M) and `[return]' (GUI
function key) — without the function-key form, hitting Enter in a
GUI frame falls through to the major mode's `<return>' binding."
  (let ((play (lambda (event) (interactive (list last-nonmenu-event))
                (shipit--play-video-url url (shipit--event-place event)))))
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") play)
      (define-key map [return] play)
      (define-key map (kbd "<RET>") play)
      (define-key map [mouse-1] play)
      (define-key map [mouse-2] play)
      map)))

(defun shipit--render-video-link (url &optional name)
  "Return a string representation of video URL with a clickable play button.
The label is propertized so that RET / mouse-1 invokes
`shipit--play-video-url' on the URL, and says NAME when given.  When
ffmpeg is available a still thumbnail is included beneath the button,
unless the URL needs a login, which ffmpeg does not have."
  (let* ((thumb (and shipit-render-videos
                     (not (shipit--login-headers url))
                     (shipit--ffmpeg-thumbnail-for-url url)))
         (icon (shipit--get-pr-field-icon "video" ">"))
         (label-text (format "[%s Play %s]" icon (or name "Video")))
         (label (propertize label-text
                            'face 'link
                            'shipit-video-button t
                            'pointer 'hand
                            'mouse-face 'highlight
                            'help-echo (format "RET / mouse-1 to play %s" url)
                            'shipit-video-url url
                            'keymap (shipit--video-button-keymap url)))
         (image-string
          (and thumb
               (condition-case err
                   (let* ((image (create-image thumb 'png nil
                                               :max-width (or shipit-image-max-width
                                                              (window-body-width nil t))
                                               :max-height (or shipit-image-max-height
                                                               (window-body-height nil t))))
                          (s (propertize " "
                                         'display image
                                         'rear-nonsticky '(display)
                                         'shipit-video-url url
                                         'keymap (shipit--video-button-keymap url)
                                         'pointer 'hand
                                         'mouse-face 'highlight
                                         'help-echo (format "RET / mouse-1 to play %s" url))))
                     s)
                 (error
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "VIDEO-DISPLAY: image creation failed: %s" err))
                  nil)))))
    (if image-string
        (concat "\n" label "\n" image-string "\n")
      (concat "\n" label "\n"))))

(defun shipit--process-inline-videos (text)
  "Replace bare video URLs in TEXT with thumbnail/labeled-link displays.
Inverse of `shipit--process-inline-images' for video formats —
matches `shipit--video-url-regexp' and feeds each URL through
`shipit--render-video-link'.  When `shipit-render-videos' is nil
TEXT is returned unchanged.

`save-match-data' wraps the per-match callback because
`shipit--render-video-link' (via image-type detection, ffmpeg
file-name regex, etc.) clobbers global match-data; without this
guard `replace-regexp-in-string' calls `replace-match' afterwards
with the wrong region and only replaces a few characters of the
URL, leaving the surrounding fragments visible in the buffer."
  (if (not shipit-render-videos)
      text
    (replace-regexp-in-string
     shipit--video-url-regexp
     (lambda (url) (save-match-data (shipit--render-video-link url)))
     (shipit--process-lone-video-urls (shipit--process-video-links text)) nil t)))

(defconst shipit--lone-url-regexp "^[ \t]*\\(https?://[^] \t\n<>\"'`)[]+\\)[ \t]*$"
  "Regexp matching a line that holds only a URL.  Group 1 is the URL.")

(defun shipit--process-lone-video-urls (text)
  "Replace each line of TEXT that holds only the URL of a video with a Play button.
A forge shows an uploaded video so.  Its URL names no file, and its
site tells by `shipit-video-url-functions' that it is a video."
  (replace-regexp-in-string
   shipit--lone-url-regexp
   (lambda (line)
     (save-match-data
       (string-match shipit--lone-url-regexp line)
       (let ((url (match-string 1 line)))
         (if (shipit--video-url-p url)
             (shipit--render-video-link url)
           line))))
   text t t))

(defun shipit--unescape-markdown (text)
  "Return TEXT without its markdown backslash escapes, as `\\_' for `_'."
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" text))

(defun shipit--process-video-links (text)
  "Replace each markdown link in TEXT whose text names a video with a Play button.
Done before the bare URLs, and it leaves no URL in the text for them."
  (replace-regexp-in-string
   shipit--video-link-regexp
   (lambda (link)
     (save-match-data
       (string-match shipit--video-link-regexp link)
       (shipit--render-video-link (match-string 2 link)
                                  (shipit--unescape-markdown (match-string 1 link)))))
   text nil t))

(defun shipit--process-inline-media (text)
  "Replace the images and then the videos of rendered TEXT with their displays."
  (shipit--process-inline-videos (shipit--process-inline-images text)))

(defconst shipit--markdown-image-re "!\\[\\([^]\n]*\\)\\](\\([^)\n]+\\))"
  "Regexp matching a markdown image; group 1 is its name, 2 its URL.")

(declare-function shipit--image-cache-file "shipit-http" (url))

(defun shipit--render-cached-images (text)
  "TEXT with each markdown image this machine has in its image cache shown.
Any other image stays as written: this downloads nothing, so it can run
while text is typed, where a half-typed address would be fetched at
every pause."
  (replace-regexp-in-string
   shipit--markdown-image-re
   (lambda (image)
     (save-match-data
       (string-match shipit--markdown-image-re image)
       (let ((name (match-string 1 image))
             (url (match-string 2 image)))
         (if (file-exists-p (shipit--image-cache-file url))
             (shipit--create-image-display url name)
           image))))
   text t t))

(defun shipit--create-image-display (url alt-text &optional explicit-width)
  "Create an image display for URL with ALT-TEXT.
When EXPLICIT-WIDTH is a positive integer (e.g. parsed from an
`<img width=\"N\">' attribute) the image is sized to it exactly,
overriding the wrap-column heuristic.  Without it the precedence
chain (shipit--image-target-width, col-width, shipit-image-max-width,
window-body-width) applies."
  ;; Strip markdown backslash escapes (e.g. \_ → _) from URLs
  (setq url (replace-regexp-in-string "\\\\\\([_*()~>#+.!|-]\\)" "\\1" url))
  ;; GitHub `/blob/' URLs return HTML — rewrite to raw.githubusercontent.com.
  (when (string-match
         "\\`https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)/blob/\\(.*\\)\\'"
         url)
    (setq url (format "https://raw.githubusercontent.com/%s/%s/%s"
                      (match-string 1 url)
                      (match-string 2 url)
                      (match-string 3 url))))
  ;; Resolve relative URLs against repo raw content URL
  (when (and (not (string-match-p "\\`https?://" url))
             (boundp 'shipit--image-base-repo)
             shipit--image-base-repo)
    (setq url (format "https://raw.githubusercontent.com/%s/HEAD/%s"
                      shipit--image-base-repo url)))
  (let ((cached-file (shipit--download-and-cache-image url)))
    (if cached-file
        (condition-case err
            ;; Detect image type from content, not filename
            (let* ((image-type (or (shipit--detect-image-type-from-content cached-file)
                                   (image-type-from-file-name cached-file)))
                   (col-width (when (and (boundp 'shipit--image-base-repo)
                                         shipit--image-base-repo)
                                (* 80 (frame-char-width))))
                   ;; Width precedence:
                   ;; 1. EXPLICIT-WIDTH from the source (e.g. `<img width=N>')
                   ;;    — the author asked for this size, honor it.
                   ;; 2. `shipit--image-target-width' bound by the caller.
                   ;; 3. col-width / shipit-image-max-width / window-body-width.
                   (max-w (or (and (integerp explicit-width)
                                   (> explicit-width 0)
                                   explicit-width)
                              (and (boundp 'shipit--image-target-width)
                                   shipit--image-target-width)
                              col-width
                              shipit-image-max-width
                              (window-body-width nil t)))
                   (max-h (or shipit-image-max-height
                              (window-body-height nil t)))
                   ;; GIFs: pre-resize the file with `gifsicle', or else
                   ;; ImageMagick, so each frame is already at the target
                   ;; size.  Asking Emacs to scale every frame at display
                   ;; time was hitting the 2s tardiness cap and aborting
                   ;; animation on the 414-frame demo gif.  Pre-resized
                   ;; files decode fast enough to keep up.  If neither is
                   ;; installed we fall back to in-Emacs `:scale'.
                   ;; Width resolution order:
                   ;; 1. EXPLICIT-WIDTH from source (`<img width=N>').
                   ;; 2. `shipit--image-target-width' bound by the caller.
                   ;; 3. `string-pixel-width' / `frame-char-width' in the
                   ;;    current buffer, capped by `shipit-animated-gif-max-width'.
                   (gif-target-w
                    (or (and (integerp explicit-width)
                             (> explicit-width 0)
                             explicit-width)
                        (and (boundp 'shipit--image-target-width)
                             shipit--image-target-width)
                        (let ((natural
                               (cond
                                ((fboundp 'string-pixel-width)
                                 (string-pixel-width (make-string 80 ?M)))
                                (t (* 80 (frame-char-width))))))
                          (if shipit-animated-gif-max-width
                              (min natural shipit-animated-gif-max-width)
                            natural))))
                   (gif-file (when (eq image-type 'gif)
                               (shipit--prepare-gif-for-display
                                cached-file gif-target-w)))
                   (gif-dim (when (eq image-type 'gif)
                              (shipit--gif-pixel-dimensions
                               (or gif-file cached-file))))
                   (gif-scale (when (and gif-dim (car gif-dim)
                                         (> (car gif-dim) gif-target-w))
                                (/ (float gif-target-w) (car gif-dim))))
                   (image (cond
                           ((and (eq image-type 'gif) gif-scale)
                            (create-image (or gif-file cached-file)
                                          image-type nil
                                          :scale gif-scale
                                          :transform-smoothing nil))
                           ((eq image-type 'gif)
                            (create-image (or gif-file cached-file)
                                          image-type nil
                                          :transform-smoothing nil))
                           (t
                            (create-image cached-file image-type nil
                                          :max-width max-w
                                          :max-height max-h)))))
              ;; Note: do NOT call `image-animate' here.  This function
              ;; runs inside `with-temp-buffer' (the markdown render path),
              ;; so `(current-buffer)' is a temp buffer that gets killed as
              ;; soon as we return.  `image-animate' captures it as the
              ;; image's `:animate-buffer' and cancels the timer the first
              ;; time it fires (the buffer is dead).  Animation is started
              ;; later by `shipit--animate-images-in-region', which the
              ;; caller invokes after the rendered text is inserted into
              ;; the real destination buffer.
              (if (display-images-p)
                  (let ((image-string (propertize " " 'display image 'rear-nonsticky '(display))))
                    ;; Inline placement: just the propertized space carrying
                    ;; the image.  No surrounding newlines and no `[Image:
                    ;; ALT]' label — the image is visible, and the label was
                    ;; forcing each badge onto its own line.
                    image-string)
                (format "[Image: %s - %s]" alt-text url)))
          (error
           (when (fboundp 'shipit--debug-log)
             (shipit--debug-log "IMAGE-DISPLAY: Error loading image: %S" err))
           (format "[Image: %s - Error loading: %s]" alt-text (error-message-string err))))
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log "IMAGE-DISPLAY: Download failed for %s" url))
      (format "[Image: %s - Download failed]" alt-text))))

(defun shipit--in-markdown-code-p (position)
  "Whether POSITION of this buffer, which holds markdown, is in code.
In a fenced code block, or in a code span of its line."
  (save-excursion
    (goto-char position)
    (let ((fences (how-many "^ \\{0,3\\}\\(?:```\\|~~~\\)" (point-min) (line-beginning-position)))
          (ticks (how-many "`" (line-beginning-position) position)))
      (or (cl-oddp fences) (cl-oddp ticks)))))

(defun shipit--delete-html-comment (start end)
  "Delete the HTML comment from START to END of this buffer.
A comment that fills its lines takes the lines with it, so that no
blank line stands where it was.  One inside a line leaves one space
between the words around it."
  (let ((alone (and (save-excursion (goto-char start) (skip-chars-backward " \t") (bolp))
                    (save-excursion (goto-char end) (skip-chars-forward " \t") (eolp)))))
    (if alone
        (delete-region (save-excursion (goto-char start) (line-beginning-position))
                       (save-excursion (goto-char end) (min (point-max) (1+ (line-end-position)))))
      (delete-region start end)
      (goto-char start)
      (when (and (eq ?\s (char-before)) (eq ?\s (char-after)))
        (delete-char 1)))))

(defun shipit--strip-html-comments (text)
  "Return markdown TEXT without its HTML comments, which a forge does not show.
A pull request template has them as hints under its headings.  A
comment in a fenced code block or in a code span is text, and stays.
One that is never closed hides the rest, as a forge shows it."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "<!--" nil t)
      (let ((start (match-beginning 0)))
        (unless (shipit--in-markdown-code-p start)
          (shipit--delete-html-comment
           start (if (search-forward "-->" nil t) (point) (point-max))))))
    (buffer-string)))

(defun shipit--markdown-face-p (position face)
  "Whether markdown-mode gave POSITION of this buffer FACE."
  (let ((faces (get-text-property position 'face)))
    (memq face (if (listp faces) faces (list faces)))))

(defun shipit--hide-heading-hashes ()
  "Hide the hashes of the headings of this buffer, fontified by markdown-mode.
A forge shows a heading by its look alone.  The hashes in front of a
heading go with the space after them, and those that close one with
the space before them.  The text stays in the buffer, so a copy of a
heading still has its hashes."
  (save-excursion
    (dolist (hashes '("^ \\{0,3\\}\\(#\\{1,6\\}[ \t]+\\)" "\\([ \t]+#+\\)[ \t]*$"))
      (goto-char (point-min))
      (while (re-search-forward hashes nil t)
        (when (shipit--markdown-face-p (save-excursion
                                         (goto-char (match-beginning 1))
                                         (skip-chars-forward " \t")
                                         (point))
                                       'markdown-header-delimiter-face)
          (put-text-property (match-beginning 1) (match-end 1) 'invisible t))))))

(defun shipit--draw-horizontal-rules ()
  "Show the rules of this buffer, fontified by markdown-mode, as lines.
A rule is a line of three or more dashes, stars or underscores.  A
forge draws a horizontal line for it.  The text stays in the buffer,
so a copy of it is still markdown.

Dashes under a line of text are no rule: they make that line a
heading.  markdown-mode knows which is which, and gives only a rule
the face that is asked for here."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ \\{0,3\\}\\(\\([-*_]\\)\\(?:[ \t]*\\2\\)\\{2,\\}\\)[ \t]*$" nil t)
      (when (shipit--markdown-face-p (match-beginning 1) 'markdown-hr-face)
        (put-text-property (match-beginning 1) (match-end 1) 'display
                           (propertize (shipit--horizontal-rule) 'face 'shadow))))))

(defconst shipit--quote-bar "▎"
  "The character that the mark of a quote is shown as.")

(defun shipit--draw-quote-bars-between (start end)
  "Show each `>' from START to END of this buffer as a bar."
  (save-excursion
    (goto-char start)
    (while (search-forward ">" end t)
      (put-text-property (1- (point)) (point) 'display
                         (propertize shipit--quote-bar 'face 'shadow)))))

(defun shipit--draw-quote-bars ()
  "Show the marks of the quotes of this buffer as bars.
The buffer is fontified by markdown-mode.  A line of a quote starts
with `>'.  A forge draws a bar at the left of a quote.  A quote inside
a quote has two marks, and gets two bars.  The text stays in the
buffer, so a copy of a quote still has its marks.

A `>' that starts a line of a code block is no mark of a quote.
markdown-mode knows which is which, and gives only the first mark of a
quote the face that is asked for here."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^ \\{0,3\\}\\(\\(?:>[ \t]?\\)+\\)" nil t)
      (when (shipit--markdown-face-p (match-beginning 1) 'markdown-markup-face)
        (shipit--draw-quote-bars-between (match-beginning 1) (match-end 1))))))

(defun shipit--draw-quoted-headings ()
  "Show each heading in a quote of this buffer as a heading.
The buffer is fontified by markdown-mode, which makes no heading of a
line in a quote, such as `> ## Notes'.  A forge does, so the hashes
and the space after them are hidden, and the text gets the face of a
heading of its level.  The text stays in the buffer, so a copy of the
heading still has its hashes.  Only the first mark of a real quote has
the face of markup, as `shipit--draw-quote-bars' says."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ \\{0,3\\}\\(\\(?:>[ \t]?\\)+\\)\\(\\(#\\{1,6\\}\\)[ \t]+\\)\\(.+\\)$" nil t)
      (when (shipit--markdown-face-p (match-beginning 1) 'markdown-markup-face)
        (put-text-property (match-beginning 2) (match-end 2) 'invisible t)
        (add-face-text-property
         (match-beginning 4) (match-end 4)
         (intern (format "markdown-header-face-%d" (length (match-string 3)))))))))

(defun shipit--strip-inline-html-tags (text)
  "Strip safe inline HTML tags from TEXT, preserving their content.
Only removes <kbd> tags which have no markdown equivalent and add noise
in text buffers.  Other formatting tags (<b>, <em>, <del>, etc.) are
preserved since they carry meaningful emphasis or semantic information."
  (replace-regexp-in-string "</?kbd>" "" text))

(defun shipit--horizontal-rule ()
  "The characters that a horizontal rule is drawn with."
  (make-string 40 ?\x2500))

(defconst shipit--html-tag-regexp
  "<\\(/\\)?\\([a-zA-Z][a-zA-Z0-9]*\\)\\(\\(?:[ \t\n][^<>]*\\)?\\)/?>"
  "Regexp matching an HTML tag.
Group 1 is the slash of a closing tag, 2 the name of the tag, and 3
its attributes.  A tag with no end tag can end with a slash, as <br/>
does.")

(defconst shipit--html-tag-converters
  '(("strong" shipit--html-mark "**")
    ("b" shipit--html-mark "**")
    ("em" shipit--html-mark "*")
    ("i" shipit--html-mark "*")
    ("span" shipit--html-mark "")
    ("sub" shipit--html-mark "")
    ("sup" shipit--html-mark "")
    ("picture" shipit--html-mark "")
    ("source" shipit--html-mark "")
    ("p" shipit--html-block "")
    ("div" shipit--html-block "")
    ("center" shipit--html-block "")
    ("h1" shipit--html-block "# ")
    ("h2" shipit--html-block "## ")
    ("h3" shipit--html-block "### ")
    ("h4" shipit--html-block "#### ")
    ("h5" shipit--html-block "##### ")
    ("h6" shipit--html-block "###### ")
    ("br" shipit--html-line-break)
    ("hr" shipit--html-rule)
    ("a" shipit--html-link)
    ("ul" shipit--html-list)
    ("ol" shipit--html-list)
    ("li" shipit--html-list-item)
    ("blockquote" shipit--html-quote)
    ("code" shipit--html-code)
    ("del" shipit--html-mark "~~")
    ("s" shipit--html-mark "~~")
    ("ins" shipit--html-mark "")
    ("u" shipit--html-mark ""))
  "What each HTML tag in markdown becomes.
An element is (NAME FUNCTION ARGUMENT...).  FUNCTION is called at the
place where a tag NAME stood, after the tag is deleted.  It gets the
name of the tag, whether it is a closing tag, the attributes of the
tag, and each ARGUMENT.  A tag that is not named here stays as text.")

(defvar shipit--html-link-targets nil
  "The addresses of the <a> tags that are open, the last one first.
An <a> tag without an address has nil here.  Bound while the HTML tags
of a text are converted.")

(defun shipit--html-attribute (attributes name)
  "Return the value that the attribute NAME has in ATTRIBUTES, or nil.
ATTRIBUTES is the text of a tag after its name."
  (when (string-match (concat "[ \t\n]" name
                              "[ \t\n]*=[ \t\n]*\\(?:\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\)")
                      attributes)
    (or (match-string 1 attributes) (match-string 2 attributes))))

(defun shipit--html-mark (_name _closing _attributes mark)
  "Put MARK, the markdown of an HTML tag, where the tag stood."
  (insert mark))

(defun shipit--html-delete-white-space-after ()
  "Delete the white space after point, the ends of lines too."
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

(defun shipit--html-skip-white-space-back ()
  "Go back over the white space before point, up to a line that a <br> started."
  (while (and (memq (char-before) '(?\s ?\t ?\n))
              (not (get-text-property (1- (point)) 'shipit-line-break)))
    (backward-char)))

(defun shipit--html-line-break (&rest _)
  "Start a new line where a <br> tag stood.
The white space around the tag goes, as HTML shows none there.  A line
of white space before it would be a blank line to markdown, which ends
a paragraph and a link in it.  The new line is marked with
`shipit-line-break'.  The next <br> keeps it, so two of them leave an
empty line, and so does `shipit--join-paragraph-lines'."
  (delete-region (progn (shipit--html-skip-white-space-back) (point))
                 (progn (skip-chars-forward " \t\n") (point)))
  (insert (propertize "\n" 'shipit-line-break t)))

(defun shipit--html-rule (&rest _)
  "Draw a horizontal line where an <hr> tag stood."
  (shipit--html-delete-white-space-after)
  (insert (propertize (shipit--horizontal-rule) 'font-lock-face 'shadow) "\n"))

(defun shipit--html-link (_name closing attributes)
  "Put a part of a markdown link where an <a> tag stood.
An <a> tag without an address, which only names a place in the text,
leaves nothing.  The white space at the edges of the text of the link
goes, as a forge shows none there."
  (if closing
      (when-let* ((target (pop shipit--html-link-targets)))
        (delete-region (point) (progn (skip-chars-backward " \t\n") (point)))
        (insert "](" target ")"))
    (let ((target (shipit--html-attribute attributes "href")))
      (push target shipit--html-link-targets)
      (when target
        (insert "[")
        (shipit--html-delete-white-space-after)))))

(defun shipit--html-content-end (name)
  "Return the place of the next closing tag NAME after point, or nil."
  (save-excursion
    (when (re-search-forward (format "</%s[ \t\n]*>" name) nil t)
      (match-beginning 0))))

(defun shipit--html-join-lines (start end)
  "Make each run of white space from START to END one space.
A browser shows the text of an element that way.  The text stays as it
is when it has a blank line, because then it is markdown, and when it
has a <pre> element."
  (unless (string-match-p "\n[ \t]*\n\\|<pre\\b" (buffer-substring start end))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward "[ \t\n]+" nil t)
          (replace-match " " t t))))))

(defun shipit--html-white-space-end ()
  "Return the end of the white space after point.
The indent of a later line is no part of it: there it can make the
line a code block."
  (save-excursion
    (skip-chars-forward " \t")
    (when (eolp)
      (skip-chars-forward " \t\n")
      (unless (eobp)
        (skip-chars-backward " \t")))
    (point)))

(defun shipit--html-paragraph-break ()
  "Put one blank line at point, in place of the white space around it.
At the start and at the end of the buffer no line is put."
  (let ((end (shipit--html-white-space-end)))
    (skip-chars-backward " \t\n")
    (delete-region (point) end)
    (unless (or (bobp) (eobp))
      (insert "\n\n"))))

(defconst shipit--center-open "<shipit-center>"
  "The marker that starts centered text in markdown that is made ready.")

(defconst shipit--center-close "</shipit-center>"
  "The marker that ends centered text in markdown that is made ready.")

(defconst shipit--row-marker "<shipit-row>"
  "The marker that starts a table row in markdown that is made ready.")

(defconst shipit--cell-open "<shipit-cell>"
  "The marker that starts a table cell in markdown that is made ready.")

(defconst shipit--cell-close "</shipit-cell>"
  "The marker that ends a table cell in markdown that is made ready.")

(defconst shipit--layout-marker-regexp
  "<\\(/\\)?shipit-\\(center\\|row\\|cell\\)\\(?: \\(top\\|bottom\\)\\)?>"
  "Regexp matching a marker of the layout of markdown that is made ready.
Group 1 is the slash of a marker that ends a part, 2 is the kind of the
part, and 3 where a table cell stands in its row, if not in the middle.")

(defvar shipit--html-open-blocks nil
  "The block elements that are open, the last one first.
An element is (NAME . CENTERED).  Bound while the HTML tags of a text
are converted.")

(defun shipit--html-centered-p (name attributes)
  "Whether the tag NAME with ATTRIBUTES asks for centered text."
  (or (equal name "center")
      (equal "center" (downcase (or (shipit--html-attribute attributes "align") "")))))

(defun shipit--html-insert-paragraph (text)
  "Put TEXT at point as a paragraph of its own."
  (shipit--html-paragraph-break)
  (unless (or (bobp) (bolp))
    (insert "\n\n"))
  (insert text)
  (shipit--html-paragraph-break))

(defun shipit--html-open-block (name attributes lead)
  "Start a paragraph where the opening tag NAME with ATTRIBUTES stood.
LEAD is the markdown that starts the paragraph, such as the hashes of
a heading."
  ;; What: a centered element gets a marker and no text property.
  ;; Why:  pandoc writes the whole text again when it aligns a table,
  ;;       and a text property is lost there.  A marker comes through.
  (when-let* ((end (shipit--html-content-end name)))
    (shipit--html-join-lines (point) end))
  (let ((centered (shipit--html-centered-p name attributes))
        (heading (shipit--html-take-heading-lead)))
    (push (cons name centered) shipit--html-open-blocks)
    (if centered
        (shipit--html-insert-paragraph shipit--center-open)
      (shipit--html-paragraph-break))
    (insert heading lead)))

(defun shipit--html-take-heading-lead ()
  "Delete the marks of a heading that stand alone before point; return them.
An enclosing heading put them there, and a block that opens at the start
of the heading takes them before its own text.  Without them, return
the empty string."
  (if (looking-back "^#\\{1,6\\} " (line-beginning-position))
      (prog1 (match-string 0)
        (delete-region (match-beginning 0) (match-end 0)))
    ""))

(defun shipit--html-close-block (name)
  "End a paragraph where the closing tag NAME stood.
An element that was opened after NAME and not closed ends here too.
Each of them that was centered gets the marker that ends centered text."
  (let* ((entry (assoc name shipit--html-open-blocks))
         (closed (and entry (seq-take shipit--html-open-blocks
                                      (1+ (seq-position shipit--html-open-blocks entry #'eq)))))
         (centered (cl-count-if #'cdr closed)))
    (when entry
      (setq shipit--html-open-blocks (cdr (memq entry shipit--html-open-blocks))))
    (if (= centered 0)
        (shipit--html-paragraph-break)
      (dotimes (_ centered)
        (shipit--html-insert-paragraph shipit--center-close)))))

(defun shipit--html-block (name closing attributes lead)
  "Make the element NAME, a tag of which stood at point, a paragraph.
CLOSING tells which tag it was.  ATTRIBUTES and LEAD are as
`shipit--html-open-block' takes them."
  (if closing
      (shipit--html-close-block name)
    (shipit--html-open-block name attributes lead)))

(defvar shipit--html-open-lists nil
  "The HTML lists that are open, the innermost first.
An element is the number of the last item of a numbered list, or nil
for a list with bullets.  Bound while the HTML tags of a text are
converted.")

(defvar shipit--html-open-quotes nil
  "The starts of the HTML quotes that are open, the innermost first.
Bound while the HTML tags of a text are converted.")

(defun shipit--html-new-line ()
  "Start a new line at point, in place of the white space around it.
A blank line before point stays."
  (delete-region (point) (shipit--html-white-space-end))
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (unless (bolp)
    (insert "\n")))

(defun shipit--html-list (name closing _attributes)
  "Start or end a list where a tag NAME of a <ul> or an <ol> stood.
CLOSING tells which tag it was.  A list in an item of another list
starts on the next line, and any other list stands between blank
lines."
  (let ((outer (if closing (cdr shipit--html-open-lists) shipit--html-open-lists)))
    (if closing
        (pop shipit--html-open-lists)
      (push (and (equal name "ol") 0) shipit--html-open-lists))
    (if outer
        (shipit--html-new-line)
      (shipit--html-paragraph-break))))

(defun shipit--html-list-item-lead ()
  "Return the start of the next item of the innermost open list.
It is the bullet or the number of the item, after the indent that puts
it under the text of the item of each list around it."
  (let ((indent (apply #'+ (mapcar (lambda (number) (if number 3 2))
                                   (cdr shipit--html-open-lists)))))
    (concat (make-string indent ?\s)
            (if (car shipit--html-open-lists)
                (format "%d. " (cl-incf (car shipit--html-open-lists)))
              "- "))))

(defun shipit--html-list-item (_name closing _attributes)
  "Start a list item on a line of its own where an <li> tag stood.
The closing tag, CLOSING, leaves nothing: the next item or the end of
the list starts the next line."
  (unless closing
    (shipit--html-new-line)
    (insert (shipit--html-list-item-lead))
    (shipit--html-delete-white-space-after)))

(defun shipit--html-quote (_name closing _attributes)
  "Start or end a quote where a <blockquote> tag stood.
CLOSING tells which tag it was.  At its end, each line of the quote
gets the mark of a markdown quote, as the text in it is markdown by
then."
  (shipit--html-paragraph-break)
  (if closing
      (when-let* ((start (pop shipit--html-open-quotes)))
        (shipit--html-mark-quote-lines start))
    (push (point-marker) shipit--html-open-quotes)))

(defun shipit--html-mark-quote-lines (start)
  "Put the mark of a markdown quote before each line from START to point.
The blank lines at the ends are no part of the quote.  START can be at
the end of the paragraph before the quote: the paragraph break of the
first element in the quote puts its blank line after START."
  (save-excursion
    (let ((end (copy-marker (progn (skip-chars-backward " \t\n") (point)))))
      (goto-char start)
      (skip-chars-forward " \t\n")
      (beginning-of-line)
      (while (< (point) end)
        (insert (if (looking-at "[ \t]*$") ">" "> "))
        (forward-line 1))
      (set-marker end nil))))

(defconst shipit--html-entity-regexp
  "&\\(?:#[0-9]+\\|#[xX][0-9a-fA-F]+\\|[a-zA-Z][a-zA-Z0-9]*\\);"
  "Regexp matching an HTML entity, such as &lt; or &#39;.")

(defun shipit--html-text (html)
  "Return the text that the HTML shows, as libxml reads it."
  (with-temp-buffer
    (insert html)
    (apply #'concat (seq-filter #'stringp (flatten-tree (libxml-parse-html-region
                                                         (point-min) (point-max)))))))

(defun shipit--html-entity-text (entity)
  "Return the text that the HTML ENTITY, such as \"&lt;\", stands for.
Return nil when HTML does not know ENTITY, or when Emacs has no
libxml to ask.  A no-break space becomes a plain space: Emacs shows a
no-break space with a face of its own, see `nobreak-char-display'."
  (cond
   ((equal entity "&nbsp;") " ")
   ((libxml-available-p)
    (let ((text (shipit--html-text entity)))
      (unless (equal text entity)
        text)))))

(defun shipit--decode-html-entities (start end &optional in-code)
  "Put the text of each HTML entity from START to END in its place.
Unless IN-CODE, an entity in markdown code stays, as a forge shows it
there.  The text of a <code> element is HTML, so there it goes."
  (save-excursion
    (let ((end (copy-marker end t)))
      (goto-char start)
      (while (re-search-forward shipit--html-entity-regexp end t)
        (let ((from (match-beginning 0))
              (entity (match-string 0)))
          (unless (and (not in-code) (shipit--in-markdown-code-p from))
            (when-let* ((text (shipit--html-entity-text entity)))
              (delete-region from (point))
              (insert text)))))
      (set-marker end nil))))

(defun shipit--html-code (_name closing _attributes)
  "Make the text of a <code> element a code span, where its tag stood.
Both of its tags become backticks when the first one is converted, as
a lone backtick makes the rest of the line code, so the tags after
the element would stay.  CLOSING tells which tag it was."
  (unless closing
    (insert "`")
    (when-let* ((end (shipit--html-content-end "code")))
      (save-excursion
        (let ((end (copy-marker end t)))
          (shipit--decode-html-entities (point) end t)
          (goto-char end)
          (when (looking-at "</code[ \t\n]*>")
            (replace-match "`" t t))
          (set-marker end nil))))))

(defvar shipit--layout-open-parts nil
  "The parts of the layout that are open, the last one first.
An element is (KIND START VALIGN), with VALIGN where a table cell
stands in its row.  Bound while layout markers are applied.")

(defvar shipit--layout-next-cell nil
  "The mark that the next table cell gets, (ROW . PLACE).
Bound while layout markers are applied.")

(defun shipit--layout-open-part (kind &optional valign)
  "Take note that a part of the layout of KIND starts at point.
A table cell stands at VALIGN in its row, `top' or `bottom', or in the
middle when it is nil.  A table row has no end of its own.  It starts
the count of its cells."
  (if (equal kind "row")
      (setq shipit--layout-next-cell (cons (1+ (car shipit--layout-next-cell)) 0))
    (push (list kind (point) valign) shipit--layout-open-parts)))

(defun shipit--layout-mark-cell (start end valign)
  "Mark the text from START to END as the next cell of its table row.
VALIGN is where the cell stands in its row, as
`shipit--layout-open-part' takes it."
  (put-text-property start end 'shipit-cell shipit--layout-next-cell)
  (when valign
    (put-text-property start end 'shipit-valign valign))
  (setq shipit--layout-next-cell (cons (car shipit--layout-next-cell)
                                       (1+ (cdr shipit--layout-next-cell)))))

(defun shipit--layout-close-part (kind)
  "Mark the text from the start of the open part of KIND to point.
A part that was opened after it and not closed ends here too.  A part
without text gets no mark."
  (when-let* ((entry (assoc kind shipit--layout-open-parts)))
    (setq shipit--layout-open-parts (cdr (memq entry shipit--layout-open-parts)))
    (let ((start (nth 1 entry))
          (end (save-excursion (skip-chars-backward " \t\n") (point))))
      (cond
       ((>= start end))
       ((equal kind "center") (put-text-property start end 'shipit-align 'center))
       (t (shipit--layout-mark-cell start end (nth 2 entry)))))))

(defun shipit--apply-layout-markers (text)
  "Return TEXT without its markers of the layout.
The text between the two markers of centered text gets the text
property `shipit-align', with the value `center'.  The text between the
two markers of a table cell gets the text property `shipit-cell', with
the value (ROW . PLACE): the number of its table row in TEXT, counted
from 1, and its place in that row, counted from 0."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((shipit--layout-open-parts nil)
          (shipit--layout-next-cell (cons 0 0)))
      (while (re-search-forward shipit--layout-marker-regexp nil t)
        (let ((closing (match-beginning 1))
              (kind (match-string 2))
              (valign (match-string 3)))
          (replace-match "" t t)
          (shipit--html-paragraph-break)
          (if closing
              (shipit--layout-close-part kind)
            (shipit--layout-open-part kind (and valign (intern valign)))))))
    (buffer-string)))

(defun shipit--line-text-start (end)
  "Return the place where the text of the line at point starts, up to END.
The white space in front of the text is the indent of the line.  A
space that shows a picture is text."
  (save-excursion
    (while (and (< (point) end)
                (memq (char-after) '(?\s ?\t))
                (not (get-text-property (point) 'display)))
      (forward-char))
    (point)))

(defun shipit--center-line (start end width)
  "Draw the line from START to END in the middle of a column.
The column starts after the indent of the line and is WIDTH pixels
wide.  A line that fills the column stays where it is."
  (let* ((text-start (save-excursion
                       (goto-char start)
                       (shipit--line-text-start end)))
         (room (- width (string-pixel-width (buffer-substring text-start end)))))
    (when (> room 1)
      (put-text-property start end 'line-prefix
                         `(space :width (,(/ room 2)))))))

(defun shipit--center-aligned-lines-in-region (start end width)
  "Center each line from START to END that is marked as centered.
WIDTH is the width in pixels of the column that the text stands in."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-end (min end (line-end-position))))
        (when (text-property-any (point) line-end 'shipit-align 'center)
          (shipit--center-line (point) line-end width)))
      (forward-line 1))))

(defconst shipit--table-gutter-width 3
  "The columns between two table cells that stand side by side.
They hold a space, a bar and a space.")

(defconst shipit--table-cell-min-width 20
  "The fewest columns that a table cell needs to stand beside another.
`shipit--wrap-line' fills no text to fewer columns.")

(defun shipit--marked-table-cells ()
  "Return the marked table cells of this buffer, the last one first.
A cell is (ROW START . END), with ROW the number of its table row."
  (let ((cells nil) (pos (point-min)))
    (while (< pos (point-max))
      (let ((mark (get-text-property pos 'shipit-cell))
            (next (next-single-property-change pos 'shipit-cell nil (point-max))))
        (when mark
          (push (cons (car mark) (cons pos next)) cells))
        (setq pos next)))
    cells))

(defun shipit--marked-table-rows ()
  "Return the marked table rows of this buffer, the last one first.
A row is the list of its cells in the order of the text, each
\(START . END)."
  (let ((rows nil) (row nil))
    (dolist (cell (shipit--marked-table-cells))
      (if (and rows (eql row (car cell)))
          (push (cdr cell) (car rows))
        (push (list (cdr cell)) rows))
      (setq row (car cell)))
    (nreverse rows)))

(defun shipit--table-cell-text (text)
  "Return TEXT, the text of a table cell, as it stands in the lines of its row."
  ;; What: the hidden hashes that start a heading go.
  ;; Why:  a line of a row that starts with hashes is taken for a
  ;;       heading of the document.
  (replace-regexp-in-string
   "^#\\{1,6\\}[ \t]+"
   (lambda (hashes) (if (get-text-property 0 'invisible hashes) "" hashes))
   text t t))

(defun shipit--table-cell-parts (cell)
  "Return the text of the table CELL of this buffer as parts, in order.
CELL is (START . END).  A fenced code block is a part (LANGUAGE .
LINES), without its fences.  The text between code blocks is a part
that is a string."
  (let ((parts nil) (text nil) (code nil))
    (dolist (line (split-string (shipit--table-cell-text
                                 (buffer-substring (car cell) (cdr cell)))
                                "\n"))
      (cond
       ((and code (string-match-p shipit--code-fence-end-regexp line))
        (push (cons (car code) (nreverse (cdr code))) parts)
        (setq code nil))
       (code
        (push (if (string-search "\t" line) (shipit--expand-tabs-to-spaces line) line)
              (cdr code)))
       ((string-match shipit--code-fence-regexp line)
        (when text
          (push (string-join (nreverse text) "\n") parts))
        (setq text nil
              code (list (match-string 2 line))))
       (t (push line text))))
    (when code
      (push (cons (car code) (nreverse (cdr code))) parts))
    (when text
      (push (string-join (nreverse text) "\n") parts))
    (nreverse parts)))

(defun shipit--table-code-width (parts)
  "Return the columns that the code of PARTS, the parts of a cell, takes.
That is its widest line with a space on each side, or nil when the
cell has no code."
  (let ((codes (seq-remove #'stringp parts)))
    (when codes
      (+ 2 (apply #'max 0 (mapcar #'string-width (apply #'append (mapcar #'cdr codes))))))))

(defun shipit--visible-width (line)
  "Return the columns that LINE takes on the screen.
Hidden text takes none."
  (let ((width 0))
    (dotimes (i (length line))
      (unless (get-text-property i 'invisible line)
        (setq width (+ width (char-width (aref line i))))))
    width))

(defun shipit--table-line-width (line)
  "Return the columns that LINE, a line of a table cell, takes unwrapped.
A picture on a line of its own takes the columns of its width."
  (let ((picture (shipit--line-picture line)))
    (if picture
        (ceiling (car (image-size picture t)) (frame-char-width))
      (shipit--visible-width line))))

(defun shipit--table-natural-width (parts)
  "Return the columns that a table cell of PARTS takes without wrapping."
  (apply #'max 0 (mapcar (lambda (part)
                           (if (stringp part)
                               (apply #'max 0 (mapcar #'shipit--table-line-width
                                                      (split-string part "\n")))
                             (shipit--table-code-width (list part))))
                         parts)))

(defun shipit--table-room-level (wants room)
  "Return the most columns that one of the columns WANTS gets in ROOM.
WANTS has the columns that each one would take.  A column that wants
less than an even share of what is left gets what it wants.  The result
is nil when all of them fit."
  (when (> (apply #'+ wants) room)
    (let ((left room) (count (length wants)) (level nil))
      (dolist (want (sort (copy-sequence wants) #'<) level)
        (unless level
          (if (<= (* want count) left)
              (setq left (- left want)
                    count (1- count))
            (setq level (/ left count))))))))

(defun shipit--table-share-room (wants room)
  "Return the width of each of the columns WANTS when they share ROOM.
Nil when a column that gets less than it wants gets fewer than
`shipit--table-cell-min-width'."
  (let ((level (shipit--table-room-level wants room)))
    (unless (and level (< level shipit--table-cell-min-width))
      (mapcar (lambda (want) (if level (min want level) want)) wants))))

(defun shipit--table-columns (rows)
  "Return the cells of each column of a table of ROWS, a list of cells."
  (mapcar (lambda (index) (delq nil (mapcar (lambda (row) (nth index row)) rows)))
          (number-sequence 0 (1- (apply #'max (mapcar #'length rows))))))

(defun shipit--table-column-widths (rows width)
  "Return the width of each column of a table of ROWS in WIDTH columns.
ROWS has the parts of each cell of each row.  A column of code is as
wide as its widest code.  Each other column is as wide as its widest
cell, and when they do not fit, the widest of them share the room that
the others leave.  The result is nil for a table of one column, and
for one where the code does not fit or a column gets fewer than
`shipit--table-cell-min-width'."
  (let* ((columns (shipit--table-columns rows))
         (fixed (mapcar (lambda (cells)
                          (let ((widths (delq nil (mapcar #'shipit--table-code-width cells))))
                            (and widths (apply #'max widths))))
                        columns))
         (wants (cl-loop for cells in columns
                         for code-width in fixed
                         unless code-width
                         collect (apply #'max 0 (mapcar #'shipit--table-natural-width cells))))
         (room (- width
                  (* shipit--table-gutter-width (1- (length columns)))
                  (apply #'+ (delq nil (copy-sequence fixed)))))
         (shared (and (cdr columns) (>= room 0) (shipit--table-share-room wants room))))
    (when (and (cdr columns) (>= room 0) (or shared (null wants)))
      (mapcar (lambda (code-width) (or code-width (pop shared))) fixed))))

(defun shipit--line-picture (line)
  "Return the picture that LINE shows, when it shows nothing else.
Spaces and hidden text do not count."
  (let ((pictures nil) (other nil))
    (dotimes (i (length line))
      (let ((display (get-text-property i 'display line)))
        (cond ((eq (car-safe display) 'image)
               (cl-pushnew display pictures :test #'eq))
              ((not (or (memq (aref line i) '(?\s ?\t))
                        (get-text-property i 'invisible line)))
               (setq other t)))))
    (and (not other) (null (cdr pictures)) (car pictures))))

(defun shipit--strip-picture (picture width)
  "Return PICTURE, an image, as its strips show it: no wider than WIDTH pixels.
Each strip is centered on its line as the text is.  Then it makes its
line no higher, and no gap shows between two strips."
  (let ((properties (copy-sequence (cdr picture))))
    (when (> (car (image-size picture t)) width)
      (setq properties (plist-put properties :max-width width)))
    (cons 'image (plist-put properties :ascent 'center))))

(defun shipit--picture-strip (picture size top height)
  "Return a space that shows the strip of PICTURE from TOP, HEIGHT pixels high.
SIZE is the size of PICTURE in pixels.  The last strip is lower when
the picture ends inside it.  Centered on its line, it would leave a gap
above it, so it is raised to the top of the line."
  (let ((low (min height (- (cdr size) top))))
    (propertize " " 'display
                (if (< low height)
                    `((raise ,(/ (- height low) 2.0 (frame-char-height)))
                      (slice 0 ,top ,(car size) ,low)
                      ,picture)
                  `((slice 0 ,top ,(car size) ,height) ,picture)))))

(defun shipit--picture-strips (picture width)
  "Return lines that show PICTURE in strips one line high.
PICTURE is made no wider than WIDTH pixels.  Each line is a space that
shows one strip, so that text can stand beside the picture."
  (let* ((fitted (shipit--strip-picture picture width))
         (size (image-size fitted t))
         (height (default-line-height)))
    (mapcar (lambda (top) (shipit--picture-strip fitted size top height))
            (number-sequence 0 (max 0 (1- (cdr size))) height))))

(defun shipit--table-text-lines (text width)
  "Return the lines of TEXT in a table cell WIDTH columns wide.
TEXT is wrapped to WIDTH.  A picture on a line of its own is cut into
strips, one on each line."
  (mapcan (lambda (line)
            (let ((picture (shipit--line-picture line)))
              (if picture
                  (shipit--picture-strips picture (* width (frame-char-width)))
                (list line))))
          (split-string (shipit--wrap-text text width) "\n")))

(defun shipit--table-code-fontified (code)
  "Return the lines of CODE, (LANGUAGE . LINES), in the colors of LANGUAGE."
  (let ((text (string-join (cdr code) "\n")))
    (split-string (if (string-empty-p (car code))
                      text
                    (shipit--fontify-code-string text (car code)))
                  "\n")))

(defun shipit--table-code-lines (code width)
  "Return the lines of CODE, a code block of a table cell WIDTH columns wide.
CODE is (LANGUAGE . LINES).  Each line has a space on each side and the
code background up to the width of the cell.  It is marked as code of
a table, so that no code block of the whole text is drawn over its row."
  (let ((background (shipit--code-block-background)))
    (mapcar (lambda (line)
              (let ((padded (concat " " line
                                    (make-string (max 0 (- width 1 (string-width line)))
                                                 ?\s))))
                (when background
                  (add-face-text-property 0 (length padded)
                                          `(:background ,background) t padded))
                (propertize padded 'shipit-table-code t)))
            (shipit--table-code-fontified code))))

(defun shipit--table-cell-lines (parts width)
  "Return the lines of a table cell WIDTH columns wide, made of PARTS."
  (mapcan (lambda (part)
            (if (stringp part)
                (shipit--table-text-lines part width)
              (shipit--table-code-lines part width)))
          parts))

(defun shipit--table-column-gap (column)
  "Return the text between two table cells in a line of their row.
It is a space, a bar and a space.  The bar must start at COLUMN of the
row.  The first space is marked with that column, for
`shipit--align-table-columns-in-region' to show it as wide as that needs."
  (concat (propertize " " 'shipit-column column)
          (propertize "│" 'face 'shadow)
          " "))

(defun shipit--table-row-line (texts widths)
  "Return one line of a table row: TEXTS, one for each cell, side by side.
WIDTHS has the width of each cell in columns."
  (let ((line (car texts))
        (column (1+ (car widths))))
    (cl-mapc (lambda (text width)
               (setq line (concat line (shipit--table-column-gap column) text))
               (setq column (+ column width shipit--table-gutter-width)))
             (cdr texts) (cdr widths))
    ;; A centered line gets its place from the start of the line, and
    ;; a cell does not start there.  So no text of a cell is centered.
    (propertize line 'shipit-no-wrap t 'shipit-align nil)))

(defun shipit--table-cell-aligned (lines height valign)
  "Return LINES, the lines of a table cell, below the blank lines it needs.
They put the cell at VALIGN in a row HEIGHT lines high: at the `top', at
the `bottom', or else in the middle, as HTML has it.  In the middle it
takes the upper line when the room above and below cannot be the same."
  (let ((room (- height (length lines))))
    (append (make-list (pcase valign ('top 0) ('bottom room) (_ (/ room 2))) "")
            lines)))

(defun shipit--table-row-text (columns widths)
  "Return the text of a table row whose cells have the lines COLUMNS.
COLUMNS has one list of lines for each cell, and WIDTHS the width of
each cell in columns."
  (mapconcat (lambda (index)
               (shipit--table-row-line
                (mapcar (lambda (lines) (or (nth index lines) "")) columns)
                widths))
             (number-sequence 0 (1- (apply #'max (mapcar #'length columns))))
             "\n"))

(defun shipit--marked-tables ()
  "Return the marked tables of this buffer, the last one first.
A table is the list of its rows in the order of the text, each as
`shipit--marked-table-rows' gives it.  Rows with only white space
between them are rows of one table."
  (let ((tables nil) (end nil))
    (dolist (row (reverse (shipit--marked-table-rows)))
      (if (and end (string-blank-p (buffer-substring-no-properties end (caar row))))
          (push row (car tables))
        (push (list row) tables))
      (setq end (cdar (last row))))
    (mapcar #'reverse tables)))

(defun shipit--table-rule (widths)
  "Return the line that parts two rows of a table whose columns are WIDTHS.
It crosses the bar between two cells where that bar stands."
  (propertize (mapconcat (lambda (width) (make-string width ?─)) widths "─┼─")
              'face 'shadow 'shipit-no-wrap t))

(defun shipit--table-row-of (cells parts widths)
  "Return the text of the table row of CELLS, whose parts are PARTS.
Its columns are WIDTHS wide.  Each cell stands where its alignment
says, and a row with fewer cells than columns has empty cells at its
end."
  (let* ((columns (cl-mapcar #'shipit--table-cell-lines parts widths))
         (height (apply #'max (mapcar #'length columns)))
         (aligned (cl-mapcar (lambda (cell lines)
                               (shipit--table-cell-aligned
                                lines height (get-text-property (car cell) 'shipit-valign)))
                             cells columns)))
    (shipit--table-row-text
     (append aligned (make-list (- (length widths) (length aligned)) nil))
     widths)))

(defun shipit--lay-out-table (rows width)
  "Put the cells of the table of ROWS of this buffer side by side.
ROWS is the list of its rows, each a list of cells (START . END).  The
table takes WIDTH columns, each column is as wide in each row, and a
rule parts two rows.  A table that does not fit stays as it is."
  (let* ((parts (mapcar (lambda (row) (mapcar #'shipit--table-cell-parts row)) rows))
         (widths (shipit--table-column-widths parts width)))
    (when widths
      (let ((text (mapconcat (lambda (row)
                               (shipit--table-row-of (car row) (cdr row) widths))
                             (cl-mapcar #'cons rows parts)
                             (concat "\n" (shipit--table-rule widths) "\n"))))
        (goto-char (caar (car rows)))
        (delete-region (point) (cdar (last (car (last rows)))))
        (insert text)))))

(defun shipit--lay-out-table-rows (text width)
  "Return TEXT with the cells of each of its table rows side by side.
The cells are marked with the text property `shipit-cell'.  WIDTH is
the number of columns that a table can take.  A column is as wide as
its widest cell, or as `shipit--table-column-widths' shares the room.
The text of a cell is wrapped to its width.  A line of a row has the
text property `shipit-no-wrap'."
  (with-temp-buffer
    (insert text)
    (dolist (table (shipit--marked-tables))
      (shipit--lay-out-table table width))
    (buffer-string)))

(defconst shipit--no-paragraph-regexp
  (concat "\\`\\(?:#\\{1,6\\}[ \t]"          ; a heading
          "\\||"                              ; a table row
          "\\|```\\|~~~"                      ; a code fence
          "\\|<[a-zA-Z/!]"                    ; an HTML tag
          "\\|\\(?:=+\\|-+\\|\\*+\\|_+\\)[ \t]*\\'\\)") ; a rule, or the line under a heading
  "Regexp matching the text of a line that is no line of a paragraph.
The text is what comes after the indent and the marks of a quote.")

(defconst shipit--list-item-regexp
  "\\`[ \t]*\\(?:>[ \t]?\\)*[ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+"
  "Regexp matching the start of a line that starts a list item.
The item can be in a quote.")

(defun shipit--paragraph-text (line)
  "Return the text of LINE after its indent and its marks of a quote."
  (replace-regexp-in-string "\\`[ \t]*\\(?:>[ \t]?\\)*[ \t]*" "" line t t))

(defun shipit--blank-line-p (line)
  "Whether LINE shows nothing: only white space, and no picture."
  (not (seq-some (lambda (i)
                   (or (not (memq (aref line i) '(?\s ?\t)))
                       (get-text-property i 'display line)))
                 (number-sequence 0 (1- (length line))))))

(defun shipit--paragraph-line-p (line)
  "Whether LINE can be a line of a paragraph.
It shows something, and is no code, heading, table row, HTML tag or
rule."
  (let ((text (shipit--paragraph-text line)))
    (not (or (shipit--blank-line-p line)
             (string-match-p shipit--no-paragraph-regexp text)
             (memq 'markdown-pre-face
                   (ensure-list (get-text-property (- (length line) (length text))
                                                   'face line)))))))

(defun shipit--quote-depth (line)
  "Return how many quotes LINE is in."
  (string-match "\\`[ \t]*\\(?:>[ \t]?\\)*" line)
  (cl-count ?> (match-string 0 line)))

(defun shipit--hard-break-p (line)
  "Whether LINE ends with a hard line break: two spaces or a backslash."
  (string-match-p "\\(?:[^ ]  +\\|\\\\\\)\\'" (substring-no-properties line)))

(defun shipit--paragraph-lines-p (line next)
  "Whether LINE and the line NEXT after it are lines of one paragraph.
NEXT starts no list item, and both are in as many quotes, in one table
cell and in one alignment."
  (and (shipit--paragraph-line-p line)
       (shipit--paragraph-line-p next)
       (not (shipit--hard-break-p line))
       (not (string-match-p shipit--list-item-regexp next))
       (= (shipit--quote-depth line) (shipit--quote-depth next))
       (seq-every-p (lambda (property)
                      (equal (get-text-property 0 property line)
                             (get-text-property 0 property next)))
                    shipit--layout-properties)))

(defun shipit--joins-next-line-p ()
  "Whether the line at point and the next line are lines of one paragraph.
A line break that a <br> tag put stays."
  (let ((newline (line-end-position)))
    (and (< newline (point-max))
         (not (get-text-property newline 'shipit-line-break))
         (shipit--paragraph-lines-p
          (buffer-substring (point) newline)
          (buffer-substring (1+ newline) (save-excursion (forward-line 1)
                                                         (line-end-position)))))))

(defun shipit--skip-lead-forward ()
  "Go past the indent and the marks of a quote after point.
A space that shows a picture is no indent."
  (while (or (eq (char-after) ?>)
             (and (memq (char-after) '(?\s ?\t))
                  (not (get-text-property (point) 'display))))
    (forward-char)))

(defun shipit--join-next-line ()
  "Join the next line to the line at point, a space between them.
The white space at the end of the line goes, and so do the indent and
the marks of a quote of the next.  The space has the layout of the
line."
  (let ((space (shipit--copy-layout (buffer-substring (point) (1+ (point))) " ")))
    (end-of-line)
    (delete-region (progn (while (and (memq (char-before) '(?\s ?\t))
                                      (not (get-text-property (1- (point)) 'display)))
                            (backward-char))
                          (point))
                   (save-excursion (forward-line 1)
                                   (shipit--skip-lead-forward)
                                   (point)))
    (insert space)
    (beginning-of-line)))

(defun shipit--join-paragraph-lines (text)
  "Return TEXT with the lines of each of its paragraphs joined into one.
A forge shows a file so: a line break in a paragraph is a space.  A
hard line break stays: two spaces or a backslash at the end of a line,
and a <br> tag.  So does each line of code, a heading, a list item, a
table row, an HTML tag and a rule."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((in-code nil))
      (while (not (eobp))
        (cond
         ((looking-at "[ \t]*\\(?:>[ \t]?\\)*[ \t]*\\(?:```\\|~~~\\)")
          (setq in-code (not in-code))
          (forward-line 1))
         ((and (not in-code) (shipit--joins-next-line-p))
          (shipit--join-next-line))
         (t (forward-line 1)))))
    (buffer-string)))

(defun shipit--align-table-columns-in-region (start end)
  "Put each bar between two table cells from START to END at its column.
The space before such a bar is marked with the column of the row where
the bar must start."
  ;; What: the space is shown as wide as it takes to reach that column,
  ;;       counted from the indent of the line.
  ;; Why:  the text of a cell can be drawn in a font of another width,
  ;;       as a heading is, and can hold hidden text.  Then a count of
  ;;       its characters does not tell where it ends.
  (save-excursion
    (let ((pos start))
      (while (setq pos (text-property-not-all pos end 'shipit-column nil))
        (goto-char pos)
        (let ((indent (- (text-property-any (line-beginning-position) (1+ pos)
                                            'shipit-no-wrap t)
                         (line-beginning-position))))
          (put-text-property
           pos (1+ pos) 'display
           `(space :align-to ,(+ indent (get-text-property pos 'shipit-column)))))
        (setq pos (1+ pos))))))

(defun shipit--convert-html-tag-at-match ()
  "Convert the HTML tag that `shipit--html-tag-regexp' just matched.
A tag in a fenced code block or in a code span is text, and stays."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (name (downcase (match-string 2)))
         (closing (and (match-beginning 1) t))
         (attributes (match-string 3))
         (converter (cdr (assoc name shipit--html-tag-converters))))
    (when (and converter (not (shipit--in-markdown-code-p start)))
      (delete-region start end)
      (apply (car converter) name closing attributes (cdr converter)))))

(defconst shipit--html-table-tag-regexp
  "<\\(/\\)?\\(table\\|thead\\|tbody\\|tfoot\\|tr\\|td\\|th\\)\\([ \t\n][^<>]*\\)?>"
  "Regexp matching a tag of an HTML table: the table, a row or a cell.
Group 1 is the slash of a closing tag, 2 is the name of the tag, and 3
its attributes.")

(defun shipit--html-table-end ()
  "Return the end of the HTML table that point is in, or nil."
  (save-excursion
    (when (re-search-forward "</table[ \t\n]*>" nil t)
      (point))))

(defun shipit--html-valign (attributes)
  "Return where the tag with ATTRIBUTES puts its table cells in a row.
That is `top', `middle' or `bottom', from its valign or else from the
vertical-align of its style, or nil when it says nothing."
  (save-match-data
    (let* ((style (shipit--html-attribute attributes "style"))
           (value (or (shipit--html-attribute attributes "valign")
                      (and style
                           (string-match "vertical-align[ \t]*:[ \t]*\\([a-z]+\\)" style)
                           (match-string 1 style)))))
      (car (member (and value (intern (downcase value))) '(top middle bottom))))))

(defun shipit--html-cell-open-marker (valign)
  "Return the marker that starts a table cell that stands at VALIGN.
A cell in the middle, where HTML puts it when nothing is said, gets
the plain marker."
  (if (memq valign '(top bottom))
      (format "<shipit-cell %s>" valign)
    shipit--cell-open))

(defun shipit--html-table-marker (name closing valign)
  "Return the layout marker that stands for the table tag NAME, or nil.
CLOSING tells whether it is a closing tag.  A cell stands at VALIGN in
its row."
  (pcase name
    ("tr" (unless closing shipit--row-marker))
    ((or "td" "th") (if closing
                        shipit--cell-close
                      (shipit--html-cell-open-marker valign)))))

(defun shipit--html-unwrap-table (start end)
  "Put a paragraph break for each tag of the HTML table from START to END.
A layout marker also stands where each row started, and where each
cell started and ended.  A cell stands where its tag says, or else
where the tag of its row says."
  ;; The marker moves with the text that is put at it.  The break for
  ;; the last tag is put there, and the search must not end before point.
  (let ((end (copy-marker end t))
        (row-valign nil))
    (goto-char start)
    (while (re-search-forward shipit--html-table-tag-regexp end t)
      (let* ((name (downcase (match-string 2)))
             (closing (match-beginning 1))
             (valign (shipit--html-valign (or (match-string 3) "")))
             (marker (shipit--html-table-marker name closing (or valign row-valign))))
        (when (and (equal name "tr") (not closing))
          (setq row-valign valign))
        (replace-match "")
        (if marker
            (shipit--html-insert-paragraph marker)
          (shipit--html-paragraph-break))))
    (set-marker end nil)))

(defun shipit--html-unwrap-layout-tables ()
  "Take the tags out of each HTML table of this buffer.
A forge shows its cells side by side.  Here the text of each cell
stays, and layout markers tell where each row and each cell is.  A
table in code is text, and stays."
  (goto-char (point-min))
  (while (re-search-forward "<table\\(?:[ \t\n][^<>]*\\)?>" nil t)
    (let ((start (match-beginning 0))
          (end (shipit--html-table-end)))
      (when (and end (not (shipit--in-markdown-code-p start)))
        (shipit--html-unwrap-table start end)))))

(defun shipit--convert-html-tags ()
  "Convert the HTML tags of this buffer as `shipit--html-tag-converters' says."
  (goto-char (point-min))
  (let ((shipit--html-link-targets nil)
        (shipit--html-open-blocks nil)
        (shipit--html-open-lists nil)
        (shipit--html-open-quotes nil))
    (while (re-search-forward shipit--html-tag-regexp nil t)
      (shipit--convert-html-tag-at-match))))

(defun shipit--convert-inline-html (text)
  "Return markdown TEXT with its HTML tags as markdown.
`shipit--html-tag-converters' names the tags and what each becomes.
Each HTML entity becomes the text it stands for, after the tags, so
that an entity such as &lt;b&gt; is no tag."
  (with-temp-buffer
    (insert text)
    (let ((case-fold-search t))
      (shipit--html-unwrap-layout-tables)
      (shipit--convert-html-tags))
    (shipit--decode-html-entities (point-min) (point-max))
    (buffer-string)))

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

(defconst shipit--underscore-keep-regexps
  '("`[^`\n]+`"
    "```[^\n]*\n\\(?:.\\|\n\\)*?```"
    "https?://[^[:space:]<>]+")
  "Regexps of the text whose underscores get no backslash.
These are inline code, code blocks and web addresses.  markdown-mode
hides the backslash before an underscore in plain text, but it shows
it in code and in an address, and in an address it also changes the
page that a click opens.")

(defun shipit--regions-matching (regexps)
  "Return (START . END) of each match of REGEXPS in this buffer."
  (let ((regions nil))
    (dolist (regexp regexps regions)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (match-beginning 0) (match-end 0)) regions)))))

(defun shipit--mid-word-underscores (keep)
  "Return the position of each underscore in the middle of a word.
Leave out an underscore inside one of the regions KEEP, a list of
\(START . END).  The last position comes first."
  (let ((found nil))
    (goto-char (point-min))
    (while (re-search-forward "\\w\\(_\\)\\w" nil t)
      (let ((pos (match-beginning 1)))
        (unless (cl-some (lambda (region)
                           (and (>= pos (car region)) (< pos (cdr region))))
                         keep)
          (push pos found))))
    found))

(defun shipit--escape-mid-word-underscores (text)
  "Escape underscores in the middle of words to prevent italic rendering.
Leaves underscores at word boundaries for emphasis (e.g., _italic_).
Leaves the underscores in the text of `shipit--underscore-keep-regexps'."
  (with-temp-buffer
    (insert text)
    ;; From the last underscore to the first, so that a backslash that
    ;; is put in moves no position that is still to come.
    (dolist (pos (shipit--mid-word-underscores
                  (shipit--regions-matching shipit--underscore-keep-regexps)))
      (goto-char pos)
      (insert "\\"))
    (buffer-string)))

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
Handles nested lists by tracking indent levels independently.  Only
the numbers change, so the rest of TEXT keeps its text properties."
  (if (or (not text) (string-empty-p text))
      (or text "")
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Stack of (indent-level . counter) for nested lists
      (let ((counters '()))
        (while (not (eobp))
          (cond
           ((looking-at "\\([ \t]*\\)\\([0-9]+\\)\\. ")
            (let ((indent-len (length (match-string 1))))
              ;; Pop counters for deeper or equal indent that no longer apply
              (while (and counters (> (caar counters) indent-len))
                (setq counters (cdr counters)))
              ;; Find or create counter for this indent level
              (if (and counters (= (caar counters) indent-len))
                  ;; Increment existing counter
                  (setcdr (car counters) (1+ (cdar counters)))
                ;; New indent level — start at 1
                (push (cons indent-len 1) counters))
              (replace-match (number-to-string (cdar counters)) t t nil 2)))
           ;; Non-list line — reset counters if blank line (list break)
           ((looking-at "[ \t]*$")
            (setq counters nil)))
          (forward-line 1)))
      (buffer-string))))

(defun shipit--rewrite-relative-markdown-links (text)
  "Rewrite relative URLs in markdown `[text](url)' links to absolute paths.
When `shipit--image-base-repo' is bound (READMEs and bodies rendered
with a repo context), relative URLs become
`https://github.com/<repo>/blob/HEAD/<path>' so the existing URL
overlay handler (`shipit--create-generic-url-overlays') makes them
clickable.  Already-absolute URLs, in-document anchors (`#section'),
mailto: links, and image links (`![text](url)') are left untouched."
  (if (not (and (boundp 'shipit--image-base-repo) shipit--image-base-repo))
      text
    (let ((repo shipit--image-base-repo))
      (replace-regexp-in-string
       "\\(\\(?:\\`\\|[^!]\\)\\)\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))"
       (lambda (m)
         (save-match-data
           (string-match
            "\\(\\(?:\\`\\|[^!]\\)\\)\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))" m)
           (let ((prefix (match-string 1 m))
                 (link-text (match-string 2 m))
                 (url (match-string 3 m)))
             (if (string-match-p "\\`\\(?:https?://\\|#\\|mailto:\\)" url)
                 m
               (format "%s[%s](https://github.com/%s/blob/HEAD/%s)"
                       prefix link-text repo url)))))
       text nil t))))

(defun shipit--resolve-reference-style-images (text)
  "Resolve markdown reference-style images in TEXT to inline form.
Reference-style images use a separate definition line and two main
reference forms: alt+ref and shorthand (label doubles as alt).  The
downstream image pipeline expects inline form, so this pass:

  1. Collects every reference definition into a lookup table.
  2. Substitutes alt+ref forms with inline form for each known label.
  3. Substitutes the shorthand form when the immediate context shows
     it isn't already an inline or alt+ref form.
  4. Strips the now-unused definition lines so they don't render as
     orphan URLs in the output.

Unknown labels are left alone (markdown-mode renders them as literal
text)."
  (let ((refs (make-hash-table :test 'equal))
        (result text))
    (let ((pos 0))
      (while (string-match
              "^[ \t]*\\[\\([^]\n]+\\)\\][ \t]*:[ \t]*\\(\\S-+\\)[ \t]*$"
              result pos)
        (puthash (match-string 1 result) (match-string 2 result) refs)
        (setq pos (match-end 0))))
    (when (> (hash-table-count refs) 0)
      (setq result
            (replace-regexp-in-string
             "!\\[\\([^]\n]*\\)\\]\\[\\([^]\n]+\\)\\]"
             (lambda (match)
               (save-match-data
                 (string-match "!\\[\\([^]\n]*\\)\\]\\[\\([^]\n]+\\)\\]" match)
                 (let* ((alt (match-string 1 match))
                        (ref (match-string 2 match))
                        (url (gethash ref refs)))
                   (if url (format "![%s](%s)" alt url) match))))
             result nil t))
      (setq result
            (replace-regexp-in-string
             "!\\[\\([^]\n]+\\)\\]\\([^([]\\|\\'\\)"
             (lambda (match)
               (save-match-data
                 (string-match "!\\[\\([^]\n]+\\)\\]\\([^([]\\|\\'\\)" match)
                 (let* ((ref (match-string 1 match))
                        (trailing (or (match-string 2 match) ""))
                        (url (gethash ref refs)))
                   (if url
                       (format "![%s](%s)%s" ref url trailing)
                     match))))
             result nil t))
      (setq result
            (replace-regexp-in-string
             "^[ \t]*\\[[^]\n]+\\][ \t]*:[ \t]*\\S-+[ \t]*\n?"
             "" result)))
    result))

(defconst shipit--layout-properties '(shipit-align shipit-cell shipit-valign)
  "The text properties that tell where text stands on the page.
They say whether it is centered, which table cell it is in, and where
that cell stands in its row.")

(defun shipit--copy-layout (source text)
  "Return TEXT with the layout properties of the start of the text SOURCE."
  (let ((text (copy-sequence text)))
    (dolist (property shipit--layout-properties text)
      (let ((value (get-text-property 0 property source)))
        (when value
          (put-text-property 0 (length text) property value text))))))

(defmacro shipit--with-layout-of (source &rest body)
  "Return the string that BODY makes, with the layout of the text SOURCE.
BODY runs with the match data saved.  The string takes the place of
SOURCE in a text, and SOURCE can be marked as centered or as a table
cell."
  (declare (indent 1))
  `(shipit--copy-layout ,source (save-match-data ,@body)))

(defconst shipit--img-width-regexp
  "\\bwidth[ \t]*=[ \t]*[\"']?\\([0-9]+\\)\\(?:px\\)?\\(?:[\"'[:space:]/>]\\|\\'\\)"
  "Regexp matching the width of an <img> tag in pixels; group 1 is the number.
The number can have `px' after it, and quotes and spaces around it.  A
share in percent does not match.")

(defun shipit--img-tag-width (tag)
  "Return the width in pixels that the <img> TAG asks for, or nil."
  (when (string-match shipit--img-width-regexp tag)
    (string-to-number (match-string 1 tag))))

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
               (shipit--with-layout-of img-tag
                 (let ((src (shipit--html-attribute img-tag "src"))
                       (alt (shipit--html-attribute img-tag "alt"))
                       ;; GitHub authors set explicit display sizes via
                       ;; `<img width="N">' on issue/PR screenshots.
                       ;; Honor it so shipit doesn't upscale to fill the
                       ;; column when the author asked for a smaller size.
                       (width (shipit--img-tag-width img-tag)))
                   (if src
                       (shipit--create-image-display src (or alt "image") width)
                     img-tag))))
             result nil t))
      ;; Process linked images [![alt](img-url)](link-url) BEFORE plain images
      ;; so the inner ![alt](url) doesn't match first and break the outer link
      (setq result
            (replace-regexp-in-string
             "\\[!\\[\\([^]]*\\)\\](\\([^)]+\\))\\](\\([^)]+\\))"
             (lambda (match)
               (shipit--with-layout-of match
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
               (shipit--with-layout-of match
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

(defvar css-fontify-colors)

(defun shipit--fontify-code-string (code-string language)
  "Fontify CODE-STRING using the major mode for LANGUAGE.
Returns the fontified string with face properties.
Strips common leading indentation before fontifying to ensure
language modes (especially diff-mode) work correctly."
  (let ((mode (cdr (assoc (downcase language) shipit--language-mode-alist)))
        ;; css-mode paints a color name with that color as background
        ;; and a box.  The code block background covers the color, so
        ;; only the box would show.
        (css-fontify-colors nil))
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
           ;; The second argument was NOERROR and is obsolete; not
           ;; signalling is what it does anyway.
           (autoloadp (indirect-function mode)))))

(defun shipit--detect-language-from-shebang (content-start content-end)
  "Detect programming language from shebang line between CONTENT-START and CONTENT-END.
Returns a language string compatible with `shipit--language-mode-alist', or nil."
  (when (< content-start content-end)
    (save-excursion
      (goto-char content-start)
      (when (looking-at "[ \t]*#!.*/\\(?:env[ \t]+\\)?\\([a-zA-Z0-9_+-]+\\)")
        (let ((interpreter (downcase (match-string 1))))
          (cond
           ((member interpreter (list "python" "python3" "python2")) "python")
           ((member interpreter (list "bash" "sh" "zsh" "fish" "dash")) "bash")
           ((member interpreter (list "ruby" "jruby")) "ruby")
           ((string= interpreter "perl") "perl")
           ((string= interpreter "node") "javascript")
           ((string= interpreter "lua") "lua")
           ((assoc interpreter shipit--language-mode-alist) interpreter)))))))

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

(defvar shipit--fontify-hunk-scratch-buffer nil
  "Persistent scratch buffer for PR hunk language fontification.")

(defun shipit--fontify-hunk-scratch-buffer ()
  "Return the scratch buffer used for hunk fontification, creating it if needed."
  (unless (buffer-live-p shipit--fontify-hunk-scratch-buffer)
    (setq shipit--fontify-hunk-scratch-buffer
          (get-buffer-create " *shipit-fontify-hunk-scratch*")))
  shipit--fontify-hunk-scratch-buffer)

(defun shipit--language-for-filename (filename)
  "Return a `shipit--language-mode-alist' key for FILENAME, or nil."
  (let ((ext (and filename (file-name-extension filename))))
    (and ext
         (let ((key (downcase ext)))
           (and (assoc key shipit--language-mode-alist) key)))))

(defun shipit--diff-hunk-mode-for-filename (filename)
  "Return the major-mode symbol to fontify a diff of FILENAME, or nil."
  (let* ((key (shipit--language-for-filename filename))
         (mode (and key (cdr (assoc key shipit--language-mode-alist)))))
    (and mode (shipit--mode-available-p mode) mode)))

(defcustom shipit-pr-fontify-max-bytes 40000
  "Skip diff-hunk language fontification when concat text exceeds this size.
`markdown-mode' and a handful of other font-lock setups have regex paths
that degrade into multi-second hangs on large inputs.  The cap protects
interactive section expansion; raise it if you are happy to wait."
  :type 'integer
  :group 'shipit)

(defun shipit--diff-hunk-fontify (fragments mode main-buf)
  "Fontify FRAGMENTS in MODE, layer syntax foreground onto MAIN-BUF regions.
FRAGMENTS is a list of (BUF-START . TEXT) cells in buffer order, each
TEXT is the content portion of one diff line (no prefix) and BUF-START
is the buffer position of its first character.  Diff lines on the same
side (new or old) are concatenated with newlines so that language modes
can reason about multi-line constructs.

Skipped when the concatenated text exceeds
`shipit-pr-fontify-max-bytes' or when `font-lock-ensure' signals an
error (some modes hit pathological regex paths)."
  (when fragments
    (let* ((concat-text (mapconcat #'cdr fragments "
"))
           (intervals nil))
      (when (<= (length concat-text) shipit-pr-fontify-max-bytes)
        (with-current-buffer (shipit--fontify-hunk-scratch-buffer)
          ;; Defensively disable any globalized minor mode that may have
          ;; attached to the scratch via a derived major mode.  In
          ;; particular `shipit-code-refs-mode' calls `syntax-ppss' in
          ;; its matcher, which under `markdown-mode' can take minutes
          ;; on content with nested fences or many backticks.
          (when (bound-and-true-p shipit-code-refs-mode)
            (shipit-code-refs-mode -1))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer)
            (unless (eq major-mode mode)
              (delay-mode-hooks (funcall mode)))
            (insert concat-text)
            (ignore-errors (font-lock-ensure)))
          ;; Walk fragments, collect (dest-start dest-end face) intervals.
          (let ((scratch-pos (point-min)))
            (dolist (frag fragments)
              (let* ((buf-start (car frag))
                     (text (cdr frag))
                     (frag-end (+ scratch-pos (length text))))
                (let ((pos scratch-pos))
                  (while (< pos frag-end)
                    (let* ((face (or (get-text-property pos 'font-lock-face)
                                     (get-text-property pos 'face)))
                           (next (or (next-property-change pos nil frag-end)
                                     frag-end))
                           (clamped (min next frag-end)))
                      (when face
                        (push (list (+ buf-start (- pos scratch-pos))
                                    (+ buf-start (- clamped scratch-pos))
                                    face)
                              intervals))
                      (setq pos clamped))))
                (setq scratch-pos (+ frag-end 1))))) ; +1 for the inserted newline
          (with-current-buffer main-buf
            (dolist (iv (nreverse intervals))
              ;; Prepend the syntax face onto the existing font-lock-face so
              ;; its foreground wins over the diff face's foreground, while
              ;; the diff face's background continues to apply for attributes
              ;; the syntax face does not set.
              (shipit--prepend-font-lock-face
               (nth 0 iv) (nth 1 iv) (nth 2 iv)))))))))

(defun shipit--prepend-font-lock-face (start end face)
  "Prepend FACE to the `font-lock-face' text property between START and END.
Walks property intervals so any existing `font-lock-face' value is preserved
per-interval and combined into a face list (newer face first)."
  (let ((pos start))
    (while (< pos end)
      (let* ((existing (get-text-property pos 'font-lock-face))
             (change (next-single-property-change pos 'font-lock-face nil end))
             (next (or change end))
             (merged (cond
                      ((null existing) face)
                      ((listp existing) (cons face existing))
                      (t (list face existing)))))
        (put-text-property pos next 'font-lock-face merged)
        (setq pos next))))) 

(defun shipit--fontify-diff-hunk-lines (line-ranges filename)
  "Apply language fontification to LINE-RANGES in the current buffer.
LINE-RANGES is a list of (CONTENT-START CONTENT-END PREFIX-CHAR) tuples
where PREFIX-CHAR is one of ?+, ?-, ?\s, identifying which side the
line belongs to.  Language is derived from FILENAME extension via
`shipit--diff-hunk-mode-for-filename'.  No-op if the language has no
mapped mode."
  (let ((mode (shipit--diff-hunk-mode-for-filename filename)))
    (when mode
      (let ((new-frags nil)
            (old-frags nil)
            (main-buf (current-buffer)))
        (dolist (range line-ranges)
          (let* ((cstart (nth 0 range))
                 (cend (nth 1 range))
                 (prefix (nth 2 range))
                 (text (buffer-substring-no-properties cstart cend)))
            (cond
             ((eq prefix ?+)
              (push (cons cstart text) new-frags))
             ((eq prefix ?-)
              (push (cons cstart text) old-frags))
             ((eq prefix ?\s)
              (push (cons cstart text) new-frags)
              (push (cons cstart text) old-frags)))))
        (shipit--diff-hunk-fontify (nreverse new-frags) mode main-buf)
        (shipit--diff-hunk-fontify (nreverse old-frags) mode main-buf)))))

(defvar shipit--refine-removed-props '((face . diff-refine-removed))
  "Overlay props applied to differing characters on `-\' lines during refinement.")

(defvar shipit--refine-added-props '((face . diff-refine-added))
  "Overlay props applied to differing characters on `+\' lines during refinement.")

(defun shipit--collect-diff-line-block (prefix-char body-end)
  "From point, walk forward over consecutive lines starting with PREFIX-CHAR.
Return (CONTENT-START . CONTENT-END) covering the content portions of
all such lines (newlines included between lines, so smerge/diff can see
line boundaries), or nil if point is not on a matching line.
BODY-END caps the search."
  (let ((re (format "^     %s" (regexp-quote (char-to-string prefix-char)))))
    (when (and (< (point) body-end)
               (looking-at-p re))
      (let ((start (+ (point) 6)))
        (while (and (< (point) body-end)
                    (looking-at-p re))
          (forward-line 1))
        (cons start (1- (point)))))))

(defun shipit--refine-diff-hunk (hunk-body-start hunk-body-end)
  "Add intra-line refinement highlighting to paired -/+ blocks.
Walks the buffer range HUNK-BODY-START..HUNK-BODY-END, pairs consecutive
runs of `-' lines with immediately following `+' lines, and calls
`smerge-refine-regions' on their content portions so differing
characters get `diff-refine-removed' / `diff-refine-added' overlays."
  (require 'smerge-mode)
  (save-excursion
    (goto-char hunk-body-start)
    (while (< (point) hunk-body-end)
      (let ((removed (shipit--collect-diff-line-block ?- hunk-body-end)))
        (if removed
            (let ((added (shipit--collect-diff-line-block ?+ hunk-body-end)))
              (when added
                ;; Skip refine for very large blocks — smerge-refine does
                ;; character-level diff and gets quadratic on big inputs.
                (let ((removed-size (- (cdr removed) (car removed)))
                      (added-size (- (cdr added) (car added))))
                  (when (and (<= removed-size shipit-pr-fontify-max-bytes)
                             (<= added-size shipit-pr-fontify-max-bytes))
                    (condition-case err
                        (smerge-refine-regions
                         (car removed) (cdr removed)
                         (car added) (cdr added)
                         nil nil
                         shipit--refine-removed-props
                         shipit--refine-added-props)
                      (error
                       (shipit--debug-log "REFINE-ERROR: %s" err))))))) 
          (forward-line 1))))))

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

(defun shipit--apply-inline-code-faces-in-region (start end)
  "Apply `markdown-inline-code-face' as an overlay over inline-code spans in START..END.
Walks the region looking for the `markdown-inline-code-face' text
property set by `shipit--render-markdown' (or any other markdown
font-lock pass) and adds `markdown-inline-code-face' via
`add-face-text-property' with the prepend flag so our shipit-side
attributes (background + foreground tint) take precedence over
the inherited / default-looking markdown face.

Without this, `markdown-inline-code-face' often blends with body
text -- its inherited chain is `fixed-pitch' +
`font-lock-constant-face', and most themes leave both close to
default on a monospace buffer.  This pass gives `code' a
distinct background so it actually reads as code at a glance."
  (when (and start end (< start end) (<= end (point-max)))
    (save-excursion
      (let ((pos start))
        (while (< pos end)
          (let* ((face (or (get-text-property pos 'face)
                           (get-text-property pos 'font-lock-face)))
                 (is-inline-code
                  (cond
                   ((eq face 'markdown-inline-code-face) t)
                   ((and (listp face)
                         (memq 'markdown-inline-code-face face))
                    t)
                   (t nil)))
                 (next (or (next-single-property-change pos 'face nil end)
                           (next-single-property-change
                            pos 'font-lock-face nil end)
                           end)))
            (when is-inline-code
              ;; Overlay > text-property in Emacs's display priority,
              ;; and the magit-section-highlight overlay on this
              ;; buffer wins against our text-property face.  Use a
              ;; high-priority overlay so the chip actually shows.
              (let ((ov (make-overlay pos next)))
                (overlay-put ov 'face 'markdown-inline-code-face)
                (overlay-put ov 'priority 100)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'shipit-inline-code t)))
            (setq pos next)))))))

(defun shipit--apply-inline-code-faces-by-regex-in-region (start end)
  "Detect single-backtick `code' spans in START..END via regex.
For regions that haven't been markdown-fontified (e.g., headings
inserted via `magit-insert-heading' directly without going through
`shipit--render-body').  Mirrors what
markdown-mode would do: applies `markdown-inline-code-face' to the
inner text and hides the backticks via `invisible' overlays.

Skips:
  * fenced code blocks (``` triplets) -- handled separately,
  * spans already wrapped with a `shipit-inline-code' overlay
    (idempotent across re-renders)."
  (when (and start end (< start end) (<= end (point-max)))
    (save-excursion
      (goto-char start)
      (let ((case-fold-search nil))
        (while (re-search-forward "`\\([^`\n]+?\\)`" end t)
          (let* ((bt1-start (match-beginning 0))
                 (code-start (match-beginning 1))
                 (code-end (match-end 1))
                 (bt2-end (match-end 0))
                 (skip
                  (or
                   (cl-some
                    (lambda (ov)
                      (overlay-get ov 'shipit-code-block))
                    (overlays-at bt1-start))
                   (cl-some
                    (lambda (ov)
                      (overlay-get ov 'shipit-inline-code))
                    (overlays-at code-start)))))
            (unless skip
              (let ((mk1 (make-overlay bt1-start code-start))
                    (mk2 (make-overlay code-end bt2-end)))
                (overlay-put mk1 'invisible t)
                (overlay-put mk1 'evaporate t)
                (overlay-put mk1 'shipit-inline-code-marker t)
                (overlay-put mk2 'invisible t)
                (overlay-put mk2 'evaporate t)
                (overlay-put mk2 'shipit-inline-code-marker t))
              (let ((ov (make-overlay code-start code-end)))
                (overlay-put ov 'face 'markdown-inline-code-face)
                (overlay-put ov 'priority 100)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'shipit-inline-code t)))))))))

(defconst shipit--code-fence-regexp "^\\([ \t]*\\)```[ \t]*\\([a-zA-Z0-9_+-]*\\)"
  "Regexp matching the fence that opens a code block.
Group 1 is its indent, and group 2 its language, which can be empty.")

(defconst shipit--code-fence-end-regexp "^[ \t]*```"
  "Regexp matching the fence that closes a code block.")

(defun shipit--code-block-background ()
  "Return the background color of a code block, or nil.
It is `shipit-code-block-background', or else the background of
`markdown-code-face'.  Either is made a little lighter on a dark
background and a little darker on a light one, so that it differs from
the highlight of the section at point."
  (let* ((explicit (and (boundp 'shipit-code-block-background)
                        shipit-code-block-background))
         ;; `face-background' can return the symbolic "unspecified-bg"
         ;; string on TTY frames or when the inheritance chain bottoms
         ;; out without a concrete background -- treat that as no
         ;; background to avoid `color-name-to-rgb' failing with
         ;; "Unable to load color 'unspecified-bg'".
         (markdown (and (not explicit)
                        (facep 'markdown-code-face)
                        (face-background 'markdown-code-face nil 'default)))
         (base (or explicit
                   (and markdown
                        (not (member markdown '("unspecified-bg" "unspecified-fg")))
                        markdown))))
    (when base
      (let* ((rgb (color-name-to-rgb base))
             (luminance (+ (* 0.2126 (nth 0 rgb))
                           (* 0.7152 (nth 1 rgb))
                           (* 0.0722 (nth 2 rgb)))))
        (shipit--adjust-color-brightness base (if (< luminance 0.5) 0.04 -0.04))))))

(defun shipit--apply-code-block-backgrounds-in-region (start end)
  "Apply background color overlays and syntax highlighting to code blocks.
Detects both triple-backtick fenced code blocks and indented code blocks
between START and END. Uses overlays with :extend t to fill full line width.
If a language is specified after ```, applies syntax highlighting.
Must be called AFTER text is inserted into the final buffer.

Also runs `shipit--apply-inline-code-faces-in-region' so inline
code (`code`) gets a distinct shipit face on top of whatever
markdown-mode applied -- without it, inline code in a heading
blends with the surrounding prose."
  ;; Guard against invalid region bounds (can happen during async refresh)
  (when (and start end (< start end) (<= end (point-max)))
    (condition-case err
      (let* ((code-block-bg (shipit--code-block-background))
             (nested-block-bg (shipit--nested-code-block-bg code-block-bg))
             (found-fenced 0)
             (found-indented 0)
             (found-org 0))
        (shipit--debug-log "CODE-BLOCK-BG: Searching region %d-%d, code-block-bg=%s"
                           start end code-block-bg)
        (save-excursion
          (goto-char start)
          ;; First pass: Find all triple-backtick code blocks (with optional leading whitespace)
          ;; Use [ \t]* instead of \s* because \s* is syntax-class dependent
          ;; Allow optional whitespace between ``` and language (gfm-mode can insert spaces)
          (while (re-search-forward shipit--code-fence-regexp end t)
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
                (if (re-search-forward shipit--code-fence-end-regexp end t)
                    (let* ((content-end (line-beginning-position))  ; End before closing ```
                           ;; A marker: the colored code that takes the
                           ;; place of the code can be shorter, as an
                           ;; indented blank line in it loses its indent.
                           (block-end (copy-marker (min (1+ (line-end-position)) end))))
                      (shipit--debug-log "CODE-BLOCK-BG: Multi-line block content-start=%d content-end=%d language='%s'"
                                         content-start content-end language)
                      ;; Apply syntax highlighting to code content
                      ;; If no language on fence, try detecting from shebang
                      (let ((lang (if (and language (not (string-empty-p language)))
                                      language
                                    (and shipit-code-block-detect-shebang
                                         (shipit--detect-language-from-shebang
                                          content-start content-end)))))
                        (when (and lang (< content-start content-end))
                          (shipit--apply-code-block-syntax-highlighting
                           content-start content-end lang)))
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
                      (overlay-put ov 'shipit-code-block t)))))))
        ;; Second pass: Find indented code blocks by looking for markdown-pre-face
          ;; These are created by markdown-mode for 4-space indented code
          ;; Skip positions already inside fenced code blocks
          (when code-block-bg
            (goto-char start)
            (let ((pos start)
                  (block-start nil))
              (while (< pos end)
                ;; Skip if already inside a fenced code block overlay,
                ;; or in the code of a table, which has its background
                (if (or (shipit--point-in-code-block-overlay-p pos)
                        (get-text-property pos 'shipit-table-code))
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
                (setq pos (next-single-property-change pos 'font-lock-face nil end)))
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
          ;; Inline-code styling (single backticks).  Runs after the
          ;; fenced-block pass; the fenced overlays already cover any
          ;; text inside their range, so we don't double-style.
          (shipit--apply-inline-code-faces-in-region start end)
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

(declare-function canvas-mermaid-export-text "canvas-mermaid" (text out))

(defun shipit--mermaid-tool ()
  "The tool that draws mermaid pictures here, or nil for none.
`canvas-mermaid' when it loads, else `mmdc' when it works."
  (cond ((shipit-canvas-library-available-p 'canvas-mermaid) 'canvas-mermaid)
        ((shipit--mmdc-available-p) 'mmdc)))

(defun shipit--mermaid-png (content tool)
  "The PNG that TOOL draws of CONTENT, a mermaid diagram, as a unibyte string.
TOOL is `canvas-mermaid' or `mmdc'.  A TOOL that makes no PNG is an
error, with the message of the tool when it has one."
  (let* ((input (make-temp-file "shipit-mermaid-" nil ".mmd"))
         (output (concat (file-name-sans-extension input) ".png")))
    (unwind-protect
        (progn
          (pcase-exhaustive tool
            ('canvas-mermaid (canvas-mermaid-export-text content output))
            ('mmdc
             (with-temp-file input
               (insert content))
             (let ((exit-code (shipit--mmdc-call input output
                                                 "-t" shipit-mermaid-theme
                                                 "-b" shipit-mermaid-background)))
               (unless (= exit-code 0)
                 (error "mmdc exited with %d" exit-code)))))
          (unless (file-exists-p output)
            (error "%s wrote no picture" tool))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally output)
            (buffer-string)))
      (dolist (file (list input output))
        (when (file-exists-p file)
          (delete-file file))))))

(defun shipit--insert-mermaid-message (content-end indent err end)
  "Put the message of ERR on a line of its own at CONTENT-END, where the
closing fence of the block starts.  So the message stays inside the
block, and inside an overlay or a section that ends at the fence, which
the live preview of the editor draws again at each pause.  INDENT is the
indent of the block.  Return END, the end of the region that is drawn,
moved past the inserted text."
  (goto-char content-end)
  (let ((inhibit-read-only t)
        (line (concat indent (propertize (error-message-string err) 'font-lock-face 'error) "\n")))
    (insert line)
    (+ end (length line))))

(defun shipit--render-mermaid-blocks-in-region (start end)
  "Render mermaid code blocks as inline images between START and END.
canvas-mermaid draws them when it loads, else `mmdc' (mermaid-cli).
Replaces the code block content with the rendered diagram image,
keeping the ``` markers as a heading.  A block that canvas-mermaid
cannot draw stays code, with its message on the line before its closing
fence, so the other blocks still get their pictures.  A failure of
`mmdc' turns `mmdc' off for the session, as a broken mmdc would block
the UI on each block."
  (let ((tool (and shipit-render-mermaid
                   (display-graphic-p)
                   (shipit--mermaid-tool))))
    (save-excursion
      (goto-char start)
      (while (and tool
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
                (let ((image (create-image (shipit--mermaid-png content tool) 'png t
                                           :max-width shipit-mermaid-max-width))
                      (inhibit-read-only t))
                  (delete-region content-start content-end)
                  (goto-char content-start)
                  (insert-image image "[mermaid diagram]")
                  (insert "\n")
                  (setq end (+ end (- (point) content-end))))
              (error
               (shipit--debug-log "MERMAID: %s failed: %s" tool (error-message-string err))
               (if (eq tool 'mmdc)
                   (setq shipit--mmdc-available nil
                         tool nil)
                 (setq end (shipit--insert-mermaid-message content-end indent err end)))))))))))

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
                  ;; Metadata for eldoc description lookup
                  (overlay-put ov 'shipit-issue-id ref-num-int)
                  (overlay-put ov 'shipit-issue-repo repo)
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
                        (overlay-put ov 'shipit-issue-id ref-id)
                        (overlay-put ov 'shipit-issue-repo repo)
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

;;; Issue reference eldoc — show description in echo area

(defun shipit--issue-ref-overlay-at-point ()
  "Return the shipit issue-reference overlay at point, or nil."
  (cl-find-if (lambda (ov) (overlay-get ov 'shipit-issue-id))
              (overlays-at (point))))

(defun shipit--issue-ref-eldoc-function (_callback &rest _ignored)
  "Eldoc documentation function for issue references.
When point rests on an issue reference overlay, returns a one-line
description from cache, or kicks off an async fetch and returns
\"Loading…\".  CALLBACK is accepted for eldoc protocol compatibility;
re-display is driven by the fetch completion triggering eldoc."
  (let ((ov (shipit--issue-ref-overlay-at-point)))
    (when ov
      (let* ((id (overlay-get ov 'shipit-issue-id))
             (repo (overlay-get ov 'shipit-issue-repo))
             (buf (current-buffer)))
        (shipit--issue-title-eldoc-doc
         id repo
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (fboundp 'eldoc) (eldoc))))))))))

(defun shipit--enable-issue-ref-eldoc ()
  "Register `shipit--issue-ref-eldoc-function' in the current buffer."
  (add-hook 'eldoc-documentation-functions
            #'shipit--issue-ref-eldoc-function nil t)
  (eldoc-mode 1)
  (shipit--debug-log "Enabled issue-ref eldoc in %s" (buffer-name)))

;;;###autoload
(defun shipit-debug-issue-ref-at-point ()
  "Diagnose the issue-reference overlay at point and invoke eldoc lookup.
Prints overlay properties, cache entry, and raw eldoc output to *Messages*."
  (interactive)
  (let* ((ov (shipit--issue-ref-overlay-at-point))
         (id (and ov (overlay-get ov 'shipit-issue-id)))
         (repo (and ov (overlay-get ov 'shipit-issue-repo)))
         (key (and id repo (shipit--issue-cache-key repo id)))
         (entry (and key (gethash key shipit--issue-title-cache)))
         (doc (shipit--issue-ref-eldoc-function #'ignore))
         (all-at (overlays-at (point)))
         (nearby (overlays-in (max (point-min) (- (point) 20))
                              (min (point-max) (+ (point) 20))))
         (props-at (mapcar (lambda (o) (overlay-properties o)) all-at))
         (tp (text-properties-at (point)))
         (shipit-ovs-in-buffer
          (seq-filter (lambda (o) (overlay-get o 'shipit-issue-id))
                      (overlays-in (point-min) (point-max)))))
    (message "shipit eldoc diag:
  point=%d char=%S
  overlays-at-point=%d (shipit-ov=%S)
  overlays-in-±20=%d
  shipit-id-overlays-in-buffer=%d
  text-props-at-point=%S
  overlay-props=%S
  id=%S repo=%S entry=%S eldoc=%S eldoc-on=%S"
             (point)
             (and (char-after) (char-to-string (char-after)))
             (length all-at) (and ov t)
             (length nearby)
             (length shipit-ovs-in-buffer)
             tp
             props-at
             id repo entry doc
             (bound-and-true-p eldoc-mode))))

(defcustom shipit-commit-cache-directory
  (locate-user-emacs-file "shipit-commit-cache/")
  "Path to the shipit commit cache git repo.
Used by `shipit--view-commit-in-magit' to fetch and display
commits from arbitrary remote repos without needing a local
clone -- the cache is initialised as an empty git repo on
first use, and each clicked SHA is fetched into it
\(`git fetch URL SHA --depth=1') before
`magit-show-commit' takes over.

Wipe this directory if it ever gets too large; new clicks
re-init it."
  :type 'directory
  :group 'shipit)

(defun shipit--commit-cache-init ()
  "Ensure `shipit-commit-cache-directory' is an initialised git repo.
Returns the directory path."
  (let ((dir (file-name-as-directory
              (expand-file-name shipit-commit-cache-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-directory-p (expand-file-name ".git" dir))
      (let ((default-directory dir))
        (call-process "git" nil nil nil "init" "--quiet")
        ;; Suppress "fetch refspec must look like X" warnings on
        ;; bare SHA fetches by allowing them.
        (call-process "git" nil nil nil "config"
                      "uploadpack.allowReachableSHA1InWant" "true")))
    dir))

(defun shipit--repo-clone-url (repo)
  "Return the git clone URL for REPO via the active PR backend.
Uses the backend's `:browse-repo-url' (the HTML URL for
`owner/name') with `.git' appended.  Returns nil when the
backend doesn't expose `:browse-repo-url'."
  (let* ((resolved (ignore-errors (shipit-pr--resolve-for-repo repo)))
         (backend (car-safe resolved))
         (config (cdr-safe resolved))
         (fn (and backend (plist-get backend :browse-repo-url))))
    (when fn
      (concat (funcall fn config) ".git"))))

(defun shipit--expand-commit-sha (sha repo)
  "Expand SHA to its full 40-char form via the GitHub commits API.
Returns the full SHA on success, nil otherwise.  GitHub's git
server only accepts full SHAs in `git fetch URL SHA' -- but
the REST API accepts short SHAs (>=4 chars) and returns the
full one in the `sha' field, which is exactly the round-trip
we need before fetching."
  (when (and (stringp sha) (stringp repo))
    (let ((data (ignore-errors
                  (shipit--api-request
                   (format "/repos/%s/commits/%s" repo sha)))))
      (and data (alist-get 'sha data)))))

(defun shipit--fetch-commit-into-cache (sha repo)
  "Fetch SHA from REPO into `shipit-commit-cache-directory'.
Returns (CACHE . FULL-SHA) on success, nil on failure.  Expands
short SHAs to 40-char form via the GitHub API first since
`git fetch URL SHA' rejects short SHAs.  Uses `--depth=1' so
only the target commit (and its tree) is downloaded, not the
whole history.

Disables git\\='s interactive credential prompts via
`GIT_TERMINAL_PROMPT=0' and `GIT_ASKPASS=true' -- without that,
fetching from a private HTTPS clone URL hangs `call-process'
indefinitely on the password prompt (`call-process' has no
terminal, so the prompt never shows but git waits anyway), and
the user has to C-g out."
  (let* ((full-sha (if (= (length sha) 40)
                       sha
                     (shipit--expand-commit-sha sha repo)))
         (url (shipit--repo-clone-url repo))
         (cache (shipit--commit-cache-init)))
    (when (and full-sha url)
      (let ((default-directory cache)
            (process-environment
             (append '("GIT_TERMINAL_PROMPT=0"
                       "GIT_ASKPASS=true"
                       "SSH_ASKPASS=true")
                     process-environment)))
        (when (zerop (call-process "git" nil nil nil
                                   "fetch" "--depth=1" url full-sha))
          (cons cache full-sha))))))

(defun shipit--view-commit-in-magit (sha &optional repo)
  "View commit SHA in a magit-revision buffer.
Tries hardest-fast-path-first:

1. Look up SHA in the *current* `default-directory' -- when
   reviewing a PR you almost always have the repo cloned, so
   the SHA is right there and no fetch is needed.
2. When REPO (`owner/name') is provided and the local lookup
   missed, fetch into `shipit-commit-cache-directory' with
   interactive prompts disabled (the previous version hung
   on HTTPS auth -- now it fails fast).
3. On any failure, fall back to opening the commit in the
   browser via the active PR backend."
  (cond
   ((not (fboundp 'magit-show-commit))
    (message "magit-show-commit not available"))
   ;; Fast path: SHA is in the local repo already.
   ((magit-commit-p sha)
    (magit-show-commit sha))
   ;; With REPO: try the cache fetch, then browser fallback.
   (repo
    (message "Fetching %s from %s..."
             (substring sha 0 (min 10 (length sha))) repo)
    (let ((result (shipit--fetch-commit-into-cache sha repo)))
      (cond
       (result
        (let ((default-directory (car result))
              (full-sha (cdr result)))
          (if (magit-commit-p full-sha)
              (magit-show-commit full-sha)
            (message "Fetched but commit %s not visible in cache -- browsing"
                     sha)
            (shipit--open-commit-in-browser sha repo))))
       (t
        (message "Could not fetch %s from %s -- opening in browser"
                 sha repo)
        (shipit--open-commit-in-browser sha repo)))))
   ;; No REPO and not local: legacy path that prompts to fetch
   ;; from origin in the local clone.
   ((y-or-n-p (format "Commit %s not found locally. Fetch from origin? " sha))
    (message "Fetching from origin...")
    (magit-run-git "fetch" "origin")
    (if (magit-commit-p sha)
        (magit-show-commit sha)
      (message "Commit %s still not found." sha)))
   (t (message "Commit %s not available locally." sha))))

(defun shipit--open-commit-in-browser (sha repo)
  "Open commit SHA in browser via the active PR backend."
  (browse-url (shipit--commit-url sha repo)))

(defun shipit--commit-url (sha repo)
  "Return the browser URL for commit SHA in REPO via the active backend."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :browse-commit-url)))
    (if fn
        (funcall fn config sha)
      (error "Backend has no :browse-commit-url"))))

(defun shipit--copy-commit-url (sha repo)
  "Kill the commit URL for SHA in REPO and show it in the echo area."
  (let ((url (shipit--commit-url sha repo)))
    (kill-new url)
    (message "Copied %s" url)))

(defun shipit--commit-sha-at-point ()
  "Return (SHA . REPO) for the commit-SHA overlay at point, or nil.
Reads `shipit-commit-sha' / `shipit-commit-repo' overlay
properties planted by `shipit--create-commit-sha-overlays'."
  (let ((sha (get-char-property (point) 'shipit-commit-sha))
        (repo (get-char-property (point) 'shipit-commit-repo)))
    (and sha (cons sha repo))))

(defun shipit-commit-sha-open-magit ()
  "DWIM open action for the commit SHA at point."
  (interactive)
  (let ((info (shipit--commit-sha-at-point)))
    (when info
      (shipit--view-commit-in-magit (car info) (cdr info)))))

(defun shipit-commit-sha-open-browser ()
  "DWIM browse action for the commit SHA at point."
  (interactive)
  (let ((info (shipit--commit-sha-at-point)))
    (when info
      (shipit--open-commit-in-browser (car info) (cdr info)))))

(defun shipit-commit-sha-copy-url ()
  "DWIM copy-URL action for the commit SHA at point."
  (interactive)
  (let ((info (shipit--commit-sha-at-point)))
    (when info
      (shipit--copy-commit-url (car info) (cdr info)))))

(defun shipit-commit-sha-copy-sha ()
  "DWIM copy-SHA action for the commit SHA at point."
  (interactive)
  (let ((info (shipit--commit-sha-at-point)))
    (when info
      (kill-new (car info))
      (message "Copied %s" (car info)))))

(when (fboundp 'transient-define-prefix)
  (transient-define-prefix shipit-commit-sha-menu ()
    "Actions for the commit SHA at point."
    [["Open"
      ("o" "magit-revision"  shipit-commit-sha-open-magit)
      ("b" "browser"         shipit-commit-sha-open-browser)]
     ["Copy"
      ("w" "URL"             shipit-commit-sha-copy-url)
      ("s" "SHA"             shipit-commit-sha-copy-sha)]]))

(defun shipit--hex-run-in-a-name-p (start end)
  "Whether the hex run from START to END is a part of a path or a dotted name.
A / or a . that joins it to a letter or a digit, as in
/kitchen/c0ffee1234.grinder, makes it a part of a longer name.  A . or a /
with a space beyond it, as at the end of a sentence, does not, and
neither do the two dots of a range of commits."
  (save-excursion
    (or (progn (goto-char start)
               (looking-back "[[:alnum:]_-][/.]" (max (point-min) (- start 2))))
        (progn (goto-char end)
               (looking-at "[/.][[:alnum:]_-]")))))

(defun shipit--create-commit-sha-overlays (repo &optional body-start body-end)
  "Highlight 7-40-char hex runs as commit SHAs in REPO.
Applies `magit-hash' face to each match and binds RET / mouse-2
to open the commit in a magit-revision buffer via
`shipit--view-commit-in-magit' (which fetches from origin if
the commit isn't available locally).  Skips runs that are pure
digits (line numbers / dates) since real SHAs contain at least
one [a-f], and runs that are a part of a path or a dotted name."
  (when repo
    (let ((search-start (or body-start (point)))
          (search-limit (or body-end (point-max))))
      (save-excursion
        (goto-char search-start)
        (while (re-search-forward "\\b[0-9a-f]\\{7,40\\}\\b" search-limit t)
          (let* ((sha (match-string-no-properties 0))
                 (start (match-beginning 0))
                 (end (match-end 0)))
            (when (and (string-match-p "[a-f]" sha)
                       (not (shipit--hex-run-in-a-name-p start end)))
              (let ((ov (make-overlay start end))
                    (keymap (make-sparse-keymap)))
                (set-keymap-parent keymap (current-local-map))
                (define-key keymap (kbd "RET")
                  (lambda ()
                    (interactive)
                    (shipit--view-commit-in-magit sha repo)))
                (define-key keymap [mouse-2]
                  (lambda (_)
                    (interactive "e")
                    (shipit--view-commit-in-magit sha repo)))
                (define-key keymap (kbd "b")
                  (lambda ()
                    (interactive)
                    (shipit--open-commit-in-browser sha repo)))
                (define-key keymap (kbd "w")
                  (lambda ()
                    (interactive)
                    (shipit--copy-commit-url sha repo)))
                (overlay-put ov 'face 'magit-hash)
                (overlay-put ov 'mouse-face 'highlight)
                (overlay-put ov 'keymap keymap)
                (overlay-put ov 'evaporate t)
                ;; Stamp the SHA + repo on the overlay so the dwim
                ;; handler (`M-;') can find the commit context from
                ;; arbitrary character properties at point and pop
                ;; the transient action menu.
                (overlay-put ov 'shipit-commit-sha sha)
                (overlay-put ov 'shipit-commit-repo repo)
                (overlay-put ov 'help-echo
                             (format "%s -- RET: magit / b: browse / w: copy URL / M-;: menu"
                                     sha))))))))))

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
Falls back to browsing URL when issues are disabled.  Resolves
the `auto' sentinel via `shipit-pr--backend-id' so the issue
backend lookup gets a concrete symbol (e.g. `github') rather
than `auto', which is not a registered issue backend."
  (if shipit-issues-enabled
      (shipit-issues-open-buffer number repo
                                 (shipit-pr--backend-id)
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
          ;; but cannot start with a hyphen.  An @ after a letter or a
          ;; digit is part of an address, as in barista@cafe, and no
          ;; mention on GitHub.
          (while (re-search-forward "\\(?:^\\|[^[:alnum:]_]\\)@\\([a-zA-Z0-9][a-zA-Z0-9-]*\\)"
                                    search-limit t)
            (let* ((username (match-string 1))
                   (start (1- (match-beginning 1)))
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
                       (keymap (make-sparse-keymap))
                       (me (and (fboundp 'shipit--get-current-user)
                                (shipit--get-current-user)))
                       (self (and me
                                  (string-equal (downcase username)
                                                (downcase me))))
                       (face (if self
                                 'shipit-user-mention-self-face
                               'shipit-user-mention-face)))
                  (set-keymap-parent keymap (current-local-map))
                  ;; Add RET keybinding to show action menu
                  (define-key keymap (kbd "RET")
                    (let ((user-copy username)
                          (repo-copy repo))
                      (lambda ()
                        (interactive)
                        (shipit--user-mention-action-menu user-copy repo-copy))))
                  ;; Apply styling -- distinct face for mentions of self
                  ;; so "the author actually pinged me" stands out.
                  (overlay-put ov 'face face)
                  (overlay-put ov 'keymap keymap)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'shipit-user-mention username)
                  (overlay-put ov 'shipit-user-mention-self self)
                  (overlay-put ov 'help-echo
                               (format "@%s%s - RET: actions"
                                       username
                                       (if self "  (you)" "")))))))))
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
                        (shipit--repo-for-buffer))))
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
                             (repo-copy (shipit--repo-for-buffer)))
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

(defun shipit--region-face-to-font-lock-face (start end)
  "Move `face' text-property to `font-lock-face' from START to END.
The blob viewer's host buffer enables `font-lock-mode' which
strips arbitrary `face' properties on its first pass; copying
to the persistent `font-lock-face' preserves the rendered
styling."
  (save-excursion
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'face nil end))
              (face (get-text-property pos 'face)))
          (when face
            (with-silent-modifications
              (put-text-property pos next 'font-lock-face face)
              (remove-text-properties pos next '(face nil))))
          (setq pos next))))))

(defun shipit--blob-prose-format--from-local-vars (content)
  "Return `org' or `markdown' from a `-*- mode: NAME -*-' header in CONTENT.
Honors the standard Emacs file-local-variables line so files
like magit/forge's `CHANGELOG' (which begins
`# -*- mode: org -*-') are recognised even though the file
itself has no extension and lacks `#+TITLE:'-style org
keywords."
  (when (and (stringp content) (not (string-empty-p content)))
    (let* ((first-newline (string-search "\n" content))
           (first-line (substring content 0 (or first-newline (length content)))))
      (when (string-match
             "-\\*-[ \t]*\\(?:.*[ \t;]+\\)?mode:[ \t]*\\([A-Za-z0-9_-]+\\)\\b"
             first-line)
        (let ((mode (downcase (match-string 1 first-line))))
          (cond
           ((member mode '("org" "org-mode")) 'org)
           ((member mode '("markdown" "markdown-mode" "gfm" "gfm-mode")) 'markdown)))))))

(defun shipit--blob-prose-format (filename content)
  "Return `markdown', `org', or nil for FILENAME with CONTENT.
Returns a non-nil format when FILENAME is a prose file the blob
viewer should render as collapsible magit-sections.  Order:
1. `-*- mode: org -*-' / `-*- mode: markdown -*-' file-local-vars
   line (`# -*- mode: org -*-' on a CHANGELOG counts).
2. Extension (`.md' -> markdown, `.org' -> org).
3. Extension-less GitHub conventions (`CHANGELOG' / `CHANGES' /
   `HISTORY' / `NEWS' / `README') -- format detected from
   CONTENT via `shipit--detect-body-format'."
  (or (shipit--blob-prose-format--from-local-vars content)
      (let ((base (file-name-nondirectory (or filename ""))))
        (cond
         ((string-match-p "\\.\\(md\\|markdown\\|mdown\\)\\'" base) 'markdown)
         ((string-match-p "\\.org\\'" base) 'org)
         ((string-match-p
           "\\`\\(CHANGELOG\\|CHANGES\\|HISTORY\\|NEWS\\|README\\)\\(\\..+\\)?\\'"
           base)
          (and content (shipit--detect-body-format content)))))))

(defun shipit--open-blob-url (classified)
  "Open a blob URL CLASSIFIED plist in a buffer with appropriate major mode.
For markdown-shaped files (`.md', `CHANGELOG', etc.) the body is
rendered as nested magit-sections so the user can navigate +
collapse / expand by heading; other filetypes fall back to
`set-auto-mode' on the raw content."
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
        (let ((buf (get-buffer-create buf-name))
              (prose-format (shipit--blob-prose-format filename file-content)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (cond
               (prose-format
                (require 'shipit-repo-buffer)
                (magit-section-mode)
                ;; Pre-render via `shipit--render-body' so inline
                ;; markup (`~code~', `*bold*', emphasis, links)
                ;; carries `font-lock-face' properties through to
                ;; the section inserter -- the inserter only
                ;; preserves properties, it does not re-render.
                ;; Without this the buffer shows raw `~code~'.
                (let ((rendered (if (fboundp 'shipit--render-body)
                                    (shipit--render-body file-content prose-format)
                                  file-content))
                      (render-start (point)))
                  (magit-insert-section (shipit-blob-root)
                    (cond
                     ((eq prose-format 'org)
                      (shipit-repo-buffer--insert-org-as-sections rendered 0))
                     (t
                      (shipit-repo-buffer--insert-markdown-as-sections rendered 0))))
                  ;; Make `#NNN' issue/PR references, bare URLs, and
                  ;; code blocks visually distinct + clickable.  Same
                  ;; pipeline used for repo READMEs.
                  ;; magit-section-mode enables font-lock-mode and
                  ;; font-lock clears any pre-existing `face' on its
                  ;; first pass.  Migrate the rendered body's `face'
                  ;; to `font-lock-face' (which font-lock leaves
                  ;; alone) so org's `~code~' / markdown emphasis
                  ;; faces survive.
                  (shipit--region-face-to-font-lock-face
                   render-start (point))
                  (when (fboundp 'shipit--create-pr-reference-overlays)
                    (shipit--create-pr-reference-overlays
                     repo nil render-start (point)))
                  (when (fboundp 'shipit--create-commit-sha-overlays)
                    (shipit--create-commit-sha-overlays repo render-start (point)))
                  (when (fboundp 'shipit--create-generic-url-overlays)
                    (shipit--create-generic-url-overlays render-start (point)))
                  (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
                    (shipit--apply-code-block-backgrounds-in-region
                     render-start (point)))
                  (when (fboundp 'shipit--animate-images-in-region)
                    (shipit--animate-images-in-region render-start (point)))))
               (t
                (insert file-content)
                (goto-char (point-min))
                ;; Set major mode from filename for non-prose files.
                (let ((buffer-file-name filename))
                  (set-auto-mode))))
              (setq buffer-read-only t)
              (goto-char (point-min))
              ;; Navigate to fragment anchor (post-render so heading
              ;; sections are present).
              (when (and fragment (not (string-empty-p fragment)))
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
                                (plist-get classified :job-id)
                                (plist-get classified :pr-number))))))

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

(defun shipit--create-link-overlays-by-line (start end configure-fn)
  "Create one overlay per visible line span between START and END.
Skips leading whitespace on each line so an underline face does not
paint across the continuation indent of a wrapped link.  CONFIGURE-FN
is called with each freshly-created overlay to set its properties
(face, keymap, help-echo, etc.)."
  (save-excursion
    (let ((seg-start start))
      (while (< seg-start end)
        (goto-char seg-start)
        (skip-chars-forward " \t" end)
        (let* ((visible-start (point))
               (seg-end (min (line-end-position) end)))
          (when (< visible-start seg-end)
            (let ((ov (make-overlay visible-start seg-end)))
              (funcall configure-fn ov)))
          (setq seg-start (min (1+ seg-end) end)))))))

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
            (let ((keymap (make-sparse-keymap))
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
              (shipit--create-link-overlays-by-line
               ov-start ov-end
               (lambda (ov)
                 (overlay-put ov 'face 'markdown-plain-url-face)
                 (overlay-put ov 'keymap keymap)
                 (overlay-put ov 'priority 100)
                 (overlay-put ov 'evaporate t)
                 (overlay-put ov 'help-echo (format "%s (RET: actions)" url)))))))))))

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

(define-derived-mode shipit-preview-popup-mode special-mode "Shipit-Popup"
  "Major mode of the preview popup of a pull request or an issue.
It is made from `special-mode', so that a package of modal keys, such
as meow, treats the popup as text to read and leaves its keys to it.
Each popup sets its own keys after this mode."
  (setq mode-line-format nil
        cursor-type nil))

;;; Child frame for PR preview
(defun shipit--create-pr-preview-frame (pr-number pr-data &optional anchor no-focus)
  "Create a child frame displaying PR information next to the cursor.
ANCHOR, a (WINDOW . POSITION), puts it next to POSITION of WINDOW
instead, and NO-FOCUS leaves the keyboard where it is, for a preview
that the mouse opens.  Return the frame."
  (let* ((original-window (selected-window))
         (anchor-window (or (car anchor) (selected-window)))
         (anchor-frame (window-frame anchor-window))
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
         (screen-height (frame-height anchor-frame))
         (available-height (truncate (* screen-height 0.8)))
         (height (max 15 (min available-height (+ body-lines header-footer-lines))))
         ;; Get cursor position in pixels relative to current window
         (cursor-pos (posn-x-y (if anchor (posn-at-point (cdr anchor) anchor-window) (posn-at-point))))
         (cursor-x (car cursor-pos))
         (cursor-y (cdr cursor-pos))
         ;; Get window position within the frame (accounts for splits)
         (window-edges (window-pixel-edges anchor-window))
         (window-left (nth 0 window-edges))
         (window-top (nth 1 window-edges))
         ;; Get frame position on screen
         (frame-pos (frame-position anchor-frame))
         (frame-x (car frame-pos))
         (frame-y (cdr frame-pos))
         ;; Calculate absolute cursor position (frame pos + window pos + cursor pos)
         (abs-cursor-x (+ frame-x window-left cursor-x))
         (abs-cursor-y (+ frame-y window-top cursor-y))
         ;; Get screen dimensions for boundary checking
         (frame-edges-rect (frame-edges anchor-frame))
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
                 `((parent-frame . ,anchor-frame)
                   ,@(when no-focus '((no-focus-on-map . t)))
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
                           'font-lock-face 'magit-section-heading))
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
        (insert (propertize author 'font-lock-face 'shipit-username-face))
        (when (not (string-empty-p timestamp))
          (insert " ")
          (insert (propertize timestamp 'font-lock-face 'shipit-timestamp-face)))
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
        (insert (propertize created-ts 'font-lock-face 'shipit-timestamp-face))
        (insert " | Updated: ")
        (insert (propertize updated-ts 'font-lock-face 'shipit-timestamp-face))
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
                (insert (propertize "Recent:" 'font-lock-face 'markdown-metadata-key-face))
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
                    (insert (propertize actor 'font-lock-face 'shipit-username-face))
                    (insert " ")
                    (insert (propertize action 'font-lock-face 'shadow))
                    (insert " ")
                    (insert (propertize timestamp 'font-lock-face 'shipit-timestamp-face))
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
                                         (fboundp 'shipit--render-body))
                                    (shipit--render-body clean-body)
                                  clean-body))
                 (wrapped-description (if (fboundp 'shipit--wrap-text)
                                         (shipit--wrap-text rendered-body 70)
                                       rendered-body))
                 (indented-description (replace-regexp-in-string "\n" "\n  " wrapped-description)))
            (insert "  ")
            (insert (propertize indented-description 'font-lock-face 'default))
            (insert "\n")
            ;; Apply code block background overlays
            (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
              (shipit--apply-code-block-backgrounds-in-region description-start (point))))))

      ;; Actions section
      (insert "  ")
      (insert (propertize "─────────────────────────────" 'font-lock-face 'shadow))
      (insert "\n")
      (insert "  Actions:\n")
      ;; Render each action from preview-actions list
      (dolist (action preview-actions)
        (insert "    ")
        (insert (propertize (car action) 'font-lock-face 'font-lock-keyword-face))
        (insert (format " - %s\n" (cadr action))))

      ;; Add bottom padding
      (insert "\n")

      ;; Move to beginning of buffer to ensure it displays from the top
      (goto-char (point-min))

      ;; The mode resets the keymap of the buffer, so it comes first.
      (shipit-preview-popup-mode)
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
        (use-local-map map)))

    ;; Show buffer in the frame and give it focus
    (set-frame-parameter frame 'default-minibuffer-frame anchor-frame)
    (set-window-buffer (frame-selected-window frame) buffer)
    (unless no-focus
      (select-frame-set-input-focus frame))
    frame))

(defun shipit--show-pr-preview (pr-number repo)
  "Show PR preview in a child frame with action keybindings."
  (let ((pr-data (shipit-get-pull-request pr-number repo)))
    (if pr-data
        (shipit--show-pr-preview-data pr-number repo pr-data)
      (message "Could not fetch PR #%d from %s" pr-number repo))))

(defun shipit--show-pr-preview-data (pr-number repo pr-data &optional anchor no-focus)
  "Show PR-DATA, pull request PR-NUMBER of REPO, in a child frame.
The frame has action keybindings.  ANCHOR and NO-FOCUS are those of
`shipit--create-pr-preview-frame'.  Return the frame."
  ;; Store data for keybinding handlers
  (setq shipit--current-pr-preview-data (list pr-number repo pr-data))
  (shipit--create-pr-preview-frame pr-number pr-data anchor no-focus))

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
                           'font-lock-face 'magit-section-heading))
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
        (insert (propertize author 'font-lock-face 'shipit-username-face))
        (when (not (string-empty-p created-ts))
          (insert " ")
          (insert (propertize created-ts 'font-lock-face 'shipit-timestamp-face)))
        (insert "\n")
        (insert "  ")
        (insert (shipit--get-pr-field-icon "created" "📅"))
        (insert " Created: ")
        (insert (propertize created-ts 'font-lock-face 'shipit-timestamp-face))
        (insert " | Updated: ")
        (insert (propertize updated-ts 'font-lock-face 'shipit-timestamp-face))
        (insert "\n"))

      ;; Labels
      (let ((labels (cdr (assq 'labels issue-data))))
        (when (and labels (> (length labels) 0))
          (insert "  ")
          (insert (shipit--get-pr-field-icon "labels" "🏷"))
          (insert " ")
          (dolist (label labels)
            (insert (propertize (cdr (assq 'name label)) 'font-lock-face 'shipit-label-face))
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
                                         (fboundp 'shipit--render-body))
                                    (shipit--render-body clean-body)
                                  clean-body))
                 (wrapped (if (fboundp 'shipit--wrap-text)
                             (shipit--wrap-text rendered-body 70)
                           rendered-body))
                 (indented (replace-regexp-in-string "\n" "\n  " wrapped)))
            (insert "  ")
            (insert (propertize indented 'font-lock-face 'default))
            (insert "\n")
            (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
              (shipit--apply-code-block-backgrounds-in-region description-start (point))))))

      ;; Actions
      (insert "  ")
      (insert (propertize "─────────────────────────────" 'font-lock-face 'shadow))
      (insert "\n")
      (insert "  Actions:\n")
      (dolist (action preview-actions)
        (insert "    ")
        (insert (propertize (car action) 'font-lock-face 'font-lock-keyword-face))
        (insert (format " - %s\n" (cadr action))))
      (insert "\n")

      (goto-char (point-min))

      ;; The mode resets the keymap of the buffer, so it comes first.
      (shipit-preview-popup-mode)
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
        (use-local-map map)))

    (set-window-buffer (frame-selected-window frame) buffer)
    (select-frame-set-input-focus frame)
    frame))

(defun shipit--render-yield ()
  "Yield to the event loop during a long-running render operation.
Calls `redisplay' so partially-rendered content becomes visible and
`sit-for' so pending input, timers, and async HTTP callbacks get a
chance to run between chunks."
  (redisplay t)
  (sit-for 0))

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

(defun shipit--prepare-markdown (text)
  "Return markdown TEXT as markdown-mode gets it to fontify."
  ;; Step 1: Normalize CRLF, and take out the HTML comments
  ;; Step 2: Convert HTML tags (e.g., <strong>) to markdown, before alignment
  ;; Step 3: Give each link to a file of the repository its whole address.
  ;;         This comes after step 2: a link from an <a> tag needs one too.
  ;; Step 4: Align tables with pandoc
  ;; Step 5: Escape mid-word underscores (e.g., file_name.py)
  ;; Step 6: Mark the text that step 2 put between layout markers: centered
  ;;         text and table cells.  This comes last: step 4 loses a text
  ;;         property.
  (let* ((normalized (replace-regexp-in-string "\r" "" text))
         (uncommented (shipit--strip-html-comments normalized))
         (refs-resolved (shipit--resolve-reference-style-images uncommented))
         (html-stripped (shipit--strip-inline-html-tags refs-resolved))
         (html-converted (shipit--convert-inline-html html-stripped))
         (links-resolved (shipit--rewrite-relative-markdown-links html-converted))
         (aligned (shipit--align-markdown-tables-with-pandoc links-resolved))
         (escaped (shipit--escape-mid-word-underscores aligned))
         (numbered (shipit--auto-increment-ordered-lists escaped)))
    (shipit--apply-layout-markers numbered)))

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
              ;; Note: Don't process images yet - they'll be done after markdown rendering
              (insert (shipit--prepare-markdown text))
              (delay-mode-hooks (markdown-mode))
              (font-lock-ensure)
              (shipit--hide-heading-hashes)
              (shipit--draw-horizontal-rules)
              (shipit--draw-quote-bars)
              (shipit--draw-quoted-headings)
              ;; Note: Code block backgrounds are applied via overlays AFTER
              ;; text is inserted into the final buffer (see shipit--apply-code-block-backgrounds-in-region)
              ;; because overlays don't transfer with buffer-string
              ;; Step 4: Process inline images AFTER markdown rendering
              ;; This preserves the display properties that would otherwise be lost
              (shipit--process-inline-media (buffer-string)))
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

(defvar shipit--org-render-cache (make-hash-table :test 'equal :size 256)
  "Cache of rendered org-mode content keyed by md5 hash of the input text.
Value is the propertized rendered string.  Size capped by
`shipit-markdown-render-cache-size'.")

(defun shipit--face-to-font-lock-face (start end)
  "Move the `face' text property to `font-lock-face' between START and END.
Magit-section buffers apply a `font-lock-face' overlay for the
current section highlight; using `font-lock-face' on inserted text
ensures the foreground and weight set by org/markdown rendering are
not stripped or shadowed by font-lock activity in the destination
buffer."
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'face nil end))
            (face (get-text-property pos 'face)))
        (when face
          (put-text-property pos next 'font-lock-face face)
          (remove-text-properties pos next '(face nil)))
        (setq pos next)))))

(defun shipit--remove-faces-outside-table-code (start end)
  "Take the faces off the text from START to END that is no code of a table.
The code of a table keeps its background up to the edge of its column."
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'shipit-table-code nil end)))
        (unless (get-text-property pos 'shipit-table-code)
          (remove-text-properties pos next '(face nil font-lock-face nil)))
        (setq pos next)))))

(defun shipit--strip-face-on-whitespace (text)
  "Return TEXT with `face'/`font-lock-face' stripped from line-edge whitespace.
Specifically strips:
  - leading whitespace at the start of each line, and
  - trailing whitespace plus the line-terminating newline.

Inter-word whitespace within a line is left alone, so faces with
`:underline' (notably `org-link') stay visually continuous across
words.  Without this, `fill-region' propagates the link face into
the inserted newlines and the underline paints through line breaks
and through the indentation of continuation lines."
  (if (or (null text) (string-empty-p text))
      text
    (with-temp-buffer
      (insert text)
      ;; Trailing whitespace + newline
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*\n" nil t)
        (shipit--remove-faces-outside-table-code (match-beginning 0) (match-end 0)))
      ;; Leading whitespace at start of each line
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" nil t)
        (shipit--remove-faces-outside-table-code (match-beginning 0) (match-end 0)))
      (buffer-string))))

(defun shipit--org-cache-clear ()
  "Clear the org render cache."
  (interactive)
  (clrhash shipit--org-render-cache))

(defun shipit--org-image-links-to-html (text)
  "Convert org image links in TEXT to HTML <img> tags.
Org embeds images via:
  [[LINK-URL][file:IMAGE-URL]]   — wrapped link with badge image
  [[file:IMAGE-URL]]             — bare image link
  [[file:IMAGE-URL][ALT]]        — image link with alt text
Rewriting them to inline `<img>' lets the same inline image processor
used for markdown handle them, so badges render as images instead of
as `file:...' text links."
  (let ((result text))
    (setq result
          (replace-regexp-in-string
           "\\[\\[\\([^]]+\\)\\]\\[file:\\([^]]+\\)\\]\\]"
           (lambda (m)
             (save-match-data
               (string-match
                "\\[\\[\\([^]]+\\)\\]\\[file:\\([^]]+\\)\\]\\]" m)
               (format "<img src=\"%s\" />" (match-string 2 m))))
           result nil t))
    (setq result
          (replace-regexp-in-string
           "\\[\\[file:\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
           (lambda (m)
             (save-match-data
               (string-match
                "\\[\\[file:\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]" m)
               (format "<img src=\"%s\" alt=\"%s\" />"
                       (match-string 1 m)
                       (or (match-string 2 m) ""))))
           result nil t))
    ;; Merge consecutive image-only lines into a single line so badges
    ;; that the org source put on separate lines (one per badge) render
    ;; side-by-side, matching how github.com lays them out.
    (let ((prev nil))
      (while (not (equal prev result))
        (setq prev result)
        (setq result
              (replace-regexp-in-string
               "\\(<img [^>]+/>\\)[ \t]*\n[ \t]*\\(<img \\)"
               "\\1 \\2"
               result t))))
    result))

(defun shipit--render-org-uncached (text)
  "Render org-mode TEXT without consulting the cache.
This is the actual rendering implementation; callers should use
`shipit--render-org' which adds caching.

Mirrors `org-fontify-like-in-org-mode': activates `org-mode' fully
(not via `delay-mode-hooks') so all setup needed for font-lock runs,
then converts the resulting `face' text properties to `font-lock-face'
so they survive insertion into a buffer where `font-lock-mode' is
active (e.g. a magit-section buffer)."
  (condition-case err
      (with-temp-buffer
        (unless (featurep 'org)
          (require 'org nil t))
        (if (featurep 'org)
            (progn
              (insert (shipit--org-image-links-to-html
                       (replace-regexp-in-string "\r" "" text)))
              (defvar org-mode-hook)
              (defvar org-startup-folded)
              (defvar org-startup-indented)
              (let ((org-mode-hook nil)
                    (org-startup-folded nil)
                    (org-startup-indented nil)
                    (inhibit-message t))
                (org-mode))
              (font-lock-ensure)
              (when (and (boundp 'org-link-descriptive)
                         org-link-descriptive
                         (fboundp 'org-link-display-format))
                (let ((collapsed (org-link-display-format (buffer-string))))
                  (erase-buffer)
                  (insert collapsed)))
              (shipit--face-to-font-lock-face (point-min) (point-max))
              (shipit--process-inline-media (buffer-string)))
          text))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "Org rendering failed: %s" err))
     text)))

(defun shipit--render-org (text)
  "Apply org-mode rendering to TEXT using org-mode's font-lock system.
Caches the result keyed by md5 hash of TEXT.  Cache size is bounded
by `shipit-markdown-render-cache-size' (shared with markdown cache)."
  (if (not text)
      ""
    (let* ((cache-key (md5 text))
           (cached (gethash cache-key shipit--org-render-cache)))
      (or cached
          (let ((result (shipit--render-org-uncached text)))
            (when (>= (hash-table-count shipit--org-render-cache)
                      shipit-markdown-render-cache-size)
              (clrhash shipit--org-render-cache))
            (puthash cache-key result shipit--org-render-cache)
            result)))))

(defconst shipit--org-document-keyword-regexp
  "^[ \t]*#\\+\\(?:TITLE\\|AUTHOR\\|OPTIONS\\|DATE\\|STARTUP\\|EMAIL\\|LANGUAGE\\|SETUPFILE\\|FILETAGS\\|CATEGORY\\|PROPERTY\\|TODO\\|TAGS\\|LINK\\|MACRO\\|DESCRIPTION\\|KEYWORDS\\|SUBTITLE\\|LATEX_HEADER\\|HTML_HEAD\\)[ \t]*:"
  "Regexp matching org document-level keywords.
These keywords do not appear in markdown, so a match is a strong
signal that the content is org-mode.  Match is case-insensitive at
the use site.")

(defun shipit--detect-body-format (text)
  "Detect whether TEXT is org-mode or markdown content.
Returns the symbol `org' if strong org-mode signals are present,
otherwise returns `markdown' (the default).

Strong org signals: document-level keywords like #+TITLE: or
#+AUTHOR: matched by `shipit--org-document-keyword-regexp', and
property drawers (lines that look like :PROPERTIES:).  Embedded
#+BEGIN_SRC blocks alone are NOT enough since markdown bodies in
PRs/issues sometimes include them."
  (cond
   ((or (null text) (string-empty-p text)) 'markdown)
   ((let ((case-fold-search t))
      (or (string-match-p shipit--org-document-keyword-regexp text)
          (string-match-p "^[ \t]*:PROPERTIES:[ \t]*$" text)))
    'org)
   (t 'markdown)))

(defun shipit--render-body (text &optional format)
  "Render TEXT according to FORMAT (`org' or `markdown').
If FORMAT is nil, auto-detect via `shipit--detect-body-format'.
Returns the propertized rendered string."
  (let ((fmt (or format (shipit--detect-body-format text))))
    (if (eq fmt 'org)
        (shipit--render-org text)
      (shipit--render-markdown text))))

(defun shipit--wrap-body-width (indent)
  "The width `shipit--wrap-body' wraps text to after INDENT."
  (max 20 (- (max 40 (or fill-column 80)) (length indent))))

(defun shipit--wrap-body (body indent)
  "Render BODY and wrap it, with INDENT before each line.
Each line is INDENT and at most `shipit--wrap-body-width' of text.
The wrap keeps the faces of the rendered markdown, as the one of pull
request comments does."
  (let* ((rendered (if shipit-render-markdown
                       (shipit--render-body (or body ""))
                     (or body "")))
         (wrapped (shipit--wrap-text rendered (shipit--wrap-body-width indent))))
    (with-temp-buffer
      (insert wrapped)
      (goto-char (point-min))
      (insert indent)
      (while (re-search-forward "\n" nil t)
        (unless (eobp)
          (insert indent)))
      (buffer-string))))

(defun shipit--format-for-readme-filename (filename)
  "Return `org' if FILENAME has a `.org' extension, else `markdown'.
Used when fetching repository READMEs to choose the renderer."
  (if (and filename
           (string-match-p "\\.org\\'" filename))
      'org
    'markdown))

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
                                (shipit--render-body trimmed)
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
                            (shipit--render-body body)
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
                            (shipit--render-body tables-converted)
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
                        'font-lock-face header-face
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
                        'font-lock-face button-face
                        
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
                        'font-lock-face button-face
                        
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
         (resolved-suffix
          (if is-resolved
              (concat " "
                      (shipit--get-pr-field-icon "resolved" "✓")
                      (propertize " resolved"
                                  'font-lock-face '(:inherit shadow
                                                             :weight bold)))
            ""))
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
        (propertize " ●" 'font-lock-face '(:foreground "red")
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

(defvar shipit--svg-lib-icon-cache (make-hash-table :test 'equal :size 256)
  "Cache for `shipit--svg-lib-icon-with-bg' results.
Key is `(NAME FACE-OR-NIL BG ARGS HEIGHT)'.  Each call to
`svg-lib-icon' parses XML files and allocates significant memory; with
notifications re-rendering 50+ items per refresh that becomes the
dominant CPU cost in profiles.  Cache hits return the previously-built
image object.")

(defun shipit--svg-lib-icon-cache-clear ()
  "Clear `shipit--svg-lib-icon-cache' (e.g. after theme/font changes)."
  (interactive)
  (clrhash shipit--svg-lib-icon-cache))

(defun shipit--svg-lib-icon-size (face-or-nil)
  "The font height svg-lib will bake into an icon built now.
svg-lib sizes an icon from FACE-OR-NIL\='s height, defaulting to the
default face on the selected frame, so the same name comes out at
different sizes as frames, fonts and themes change."
  (face-attribute (or face-or-nil 'default) :height nil 'default))

(defun shipit--svg-lib-icon-with-bg (name face-or-nil &rest args)
  "Call svg-lib-icon with NAME, FACE-OR-NIL and ARGS, adding theme-aware background.
Automatically sets :background to the default face background if not specified.
Memoized via `shipit--svg-lib-icon-cache' so repeated renders of the
same icon do not re-parse the SVG XML each time.  The font height is
part of the key: an icon built under one font answers at the wrong
size under another, and rows rendered at different times then mix
icon sizes."
  (let* ((bg-raw (face-background 'default nil 'default))
         ;; Drop the symbolic "unspecified-bg" string svg-lib can't parse.
         (bg (and bg-raw
                  (not (member bg-raw '("unspecified-bg" "unspecified-fg")))
                  bg-raw))
         (key (list name face-or-nil bg args
                    (shipit--svg-lib-icon-size face-or-nil)))
         (cached (gethash key shipit--svg-lib-icon-cache)))
    (or cached
        (let ((icon (apply #'svg-lib-icon name face-or-nil
                           (append args (list :background bg)))))
          (puthash key icon shipit--svg-lib-icon-cache)
          icon))))

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
        (propertize "➕" 'font-lock-face 'shipit-comment-reactions-placeholder))))
    ;; Fallback: Use grey plus sign emoji (clearly indicates "add reaction")
    (t
     (propertize "➕" 'font-lock-face 'shipit-comment-reactions-placeholder))))

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

(defun shipit--stack-checks-glyph (state)
  "Return the glyph for a stack sibling\='s check rollup STATE, or nothing.
The checks section\='s own vocabulary: green check, red x, yellow dot
for anything still running.  Nothing for nil, which is a sibling with
no checks at all."
  (pcase state
    ('nil "")
    ("success" (shipit--get-check-status-icon nil "success" "✅"))
    ((or "failure" "error")
     (shipit--get-check-status-icon nil "failure" "❌"))
    (_ (shipit--get-check-status-icon "in_progress" nil "\U0001F7E1"))))

(defun shipit--stack-review-glyph (decision)
  "Return the glyph for a stack sibling\='s review DECISION, or nothing.
The approval section\='s own vocabulary: green check for approved,
red x for changes requested, hourglass while review is still
required.  Nothing for nil, which is a sibling the forge reports no
decision for."
  (pcase decision
    ('nil "")
    ("approved" (shipit--get-approval-status-icon "APPROVED" "✅"))
    ("changes_requested"
     (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌"))
    ("review_required"
     (shipit--get-approval-status-icon "REVIEW_REQUIRED" "⏳"))
    (_ "")))

(defun shipit--stack-status-glyphs (statuses)
  "Return the glyph tail of a stack row\='s STATUSES, leading spaces included.
Check rollup first, review decision behind it; a status the sibling
does not have adds nothing, so a bare state word stays bare.  Shared
by the pull request buffer\='s stack section and the stacked preview,
which must not require one another."
  (mapconcat (lambda (glyph)
               (if (> (length glyph) 0) (concat " " glyph) ""))
             (list (shipit--stack-checks-glyph
                    (cdr (assq 'checks statuses)))
                   (shipit--stack-review-glyph
                    (cdr (assq 'review statuses))))
             ""))

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
    ("stack" '("stack" . "#0366d6"))                    ; Blue - Informational (the chain of layers)
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
    ("priority" '("zap" . "#fd7e14"))                        ; Orange - Priority
    ("parent" '("north-star" . "#6f42c1"))                   ; Purple - Parent/epic
    ("links" '("link" . "#0366d6"))                          ; Blue - Issue links
    ("children" '("tasklist" . "#6f42c1"))                    ; Purple - Child work items
    ("similar" '("light-bulb" . "#d29922"))                   ; Amber - Similar work items
    ("activity" '("history" . "#fd7e14"))                    ; Orange - Activity/changelog
    ("components" '("package" . "#0366d6"))                  ; Blue - Components
    ("discussion" '("comment-discussion" . "#0366d6"))       ; Blue - Discussion (open)
    ("discussion-answered" '("check-circle" . "#28a745"))    ; Green - Discussion (answered)
    ("category" '("apps" . "#6f42c1"))                       ; Purple - Category
    ("upvotes" '("arrow-up" . "#fd7e14"))                     ; Orange - Upvotes
    ("github" '("mark-github" . "#999999"))                    ; Gray - GitHub backend
    ("gitlab" '("gitlab" "#e24329" "simple"))                  ; Orange - GitLab backend (simple-icons)
    ("repo" '("repo" . "#0366d6"))                               ; Blue - Repository
    ("repo-public" '("globe" . "#28a745"))                       ; Green - Repository open to all
    ("repo-private" '("lock" . "#d29922"))                       ; Amber - Repository open to few
    ("repo-internal" '("organization" . "#0366d6"))              ; Blue - Repository of an organization
    ("repo-stars" '("star" . "#e3b341"))                         ; Yellow - Stars of a repository
    ("repo-fork" '("repo-forked" . "#6f42c1"))                   ; Purple - Fork of a repository
    ("repo-release" '("rocket" . "#1a7f37"))                     ; Green - Release of a repository
    ("branch" '("git-branch" . "#28a745"))                       ; Green - Branch
    ("language" '("code" . "#6f42c1"))                           ; Purple - Language
    ("stats" '("graph" . "#0366d6"))                              ; Blue - Statistics
    ("project" '("project" . "#0366d6"))                            ; Blue - Project/board
    ("clock" '("clock" . "#999999"))                                ; Gray - Time/recent
    ("issues" '("issue-opened" . "#28a745"))                        ; Green - Issues list
    ("notification" '("bell" . "#0366d6"))                           ; Blue - Notification/subscription
    ("star" '("star-fill" . "#e3b341"))                              ; Yellow - Starred
    ("pin" '("pin" . "#0366d6"))                                     ; Blue - Pinned comment
    ("linked-issue" '("link" . "#8250df"))                           ; Purple - PR to tracker issue link
    ("video" '("play" . "#0366d6"))                                  ; Blue - Video play button
    ("resolved" '("check-circle-fill" . "#28a745"))                  ; Green - Resolved review thread
    ("commits" '("git-commit" . "#6f42c1"))                          ; Purple - Commits a force push changed
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
    ("description_edited" '("pencil" . "#6e7781"))             ; Gray - Edit (neutral/informational)
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
    ("release"    '("rocket" . "#1a7f37"))               ; Green
    ("deployment" '("rocket" . "#8250df"))               ; Purple (deployment)
    ("check"      '("check-circle" . "#28a745"))         ; Green
    ("commit"     '("git-commit" . "#8250df"))           ; Purple
    ("workflow"   '("workflow" . "#28a745"))             ; Green
    ("alert"      '("shield" . "#da3633"))               ; Red
    ("invitation" '("mail" . "#0366d6"))                 ; Blue
    ("external"   '("inbox" . "#8250df"))                ; Purple
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
                              'font-lock-face (if color
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

;;; Icons in a slot of columns

(defun shipit--image-columns (drawn)
  "The columns DRAWN, an icon on a space, takes on this display."
  (if (display-graphic-p)
      (car (image-size (get-text-property 0 'display drawn)))
    1))

(defun shipit--thin-space (columns)
  "A thin space COLUMNS wide, a part of a column; nothing when COLUMNS is none."
  (if (> columns 0)
      (propertize " " 'display `(space :width ,columns))
    ""))

(defun shipit--icon-in-slot (icon slot)
  "ICON filled out to SLOT columns, so the text after it lines up with spaces.
ICON is text, such as an emoji, or an image drawn on a space.  An
image is not as wide as its columns, so a thin space fills the rest."
  (if (imagep (get-text-property 0 'display icon))
      (concat icon (shipit--thin-space (- slot (shipit--image-columns icon))))
    (concat icon (make-string (max 0 (- slot (string-width icon))) ?\s))))

;;; Word diff of two texts

(defconst shipit--word-diff-context 2
  "The lines kept around each change in a word diff of two texts.")

(defun shipit--word-diff-file (text)
  "A new temporary file that holds TEXT as lines ending in a newline."
  (let ((lines (replace-regexp-in-string "\r" "" text)))
    (make-temp-file "shipit-word-diff-" nil ".txt"
                    (if (string-suffix-p "\n" lines) lines (concat lines "\n")))))

(defun shipit--word-diff (from to)
  "The word diff of FROM and TO, two texts, in the porcelain form of git."
  (let ((old (shipit--word-diff-file from))
        (new (shipit--word-diff-file to))
        (default-directory temporary-file-directory))
    (unwind-protect
        (with-temp-buffer
          (let ((status (call-process "git" nil t nil "diff" "--no-index" "--no-color"
                                      "--word-diff=porcelain"
                                      (format "--unified=%d" shipit--word-diff-context)
                                      old new)))
            ;; git diff --no-index exits with 1 when the files differ.
            (unless (memq status '(0 1))
              (error "shipit: git diff of two texts failed: %s" (buffer-string)))
            (buffer-string)))
      (delete-file old)
      (delete-file new))))

(defun shipit--word-diff-segment (raw)
  "RAW, a line of a porcelain word diff inside a hunk, as text with its face.
Nil for a line that holds no text, such as the note of a missing newline."
  (pcase (and (> (length raw) 0) (aref raw 0))
    (?\s (substring raw 1))
    (?- (propertize (substring raw 1) 'font-lock-face 'shipit-word-diff-removed-face))
    (?+ (propertize (substring raw 1) 'font-lock-face 'shipit-word-diff-added-face))
    (_ nil)))

(defun shipit--word-diff-lines (porcelain)
  "The lines of PORCELAIN, a word diff of git, as text with faces.
Each hunk after the first starts with a dimmed ellipsis line."
  (let ((lines nil)
        (line "")
        (in-hunk nil))
    (dolist (raw (split-string porcelain "\n"))
      (cond ((string-prefix-p "@@" raw)
             (when in-hunk
               (push (propertize "…" 'font-lock-face 'font-lock-comment-face) lines))
             (setq in-hunk t))
            ((not in-hunk) nil)
            ((equal raw "~")
             (push line lines)
             (setq line ""))
            (t (setq line (concat line (or (shipit--word-diff-segment raw) ""))))))
    (nreverse lines)))

(defun shipit--overlay-word-diff-faces (start end)
  "Draw the faces of the word diff between START and END with overlays too.
magit draws the section at point with an overlay that sets the
background, and that wins over the background of the text.  An overlay
of higher priority wins over it, so the words keep their colours."
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'font-lock-face nil end))
            (face (get-text-property pos 'font-lock-face)))
        (when (memq face '(shipit-word-diff-removed-face shipit-word-diff-added-face))
          (let ((overlay (make-overlay pos next)))
            (overlay-put overlay 'face face)
            (overlay-put overlay 'priority 1)
            (overlay-put overlay 'evaporate t)))
        (setq pos next)))))

(defun shipit--insert-word-diff (from to indent)
  "Insert the word diff of FROM and TO, two texts, each line after INDENT.
Only the changed lines show, with a few lines around each, wrapped to
the window.  The faces inherit those of `diff-mode', which this loads."
  (require 'diff-mode)
  (let ((width (max 40 (- (window-width) (length indent) 2)))
        (lines (shipit--word-diff-lines (shipit--word-diff from to)))
        (start (point)))
    (if (null lines)
        (insert indent (propertize "The text did not change." 'font-lock-face
                                   'font-lock-comment-face)
                "\n")
      (dolist (line lines)
        (insert (mapconcat (lambda (part) (concat indent part))
                           (split-string (shipit--wrap-text line width) "\n")
                           "\n")
                "\n")))
    (shipit--overlay-word-diff-faces start (point))
    (insert "\n")))

(provide 'shipit-render)
;;; shipit-render.el ends here
