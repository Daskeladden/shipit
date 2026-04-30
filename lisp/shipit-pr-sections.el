;;; shipit-pr-sections.el --- PR section rendering -*- lexical-binding: t; -*-

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
;; PR section rendering: general comments, inline comments, commits,
;; files, labels, reviewers, assignees, activity, checks, and
;; placeholder/replace sections for incremental loading.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'shipit-lib)      ; Pure utility functions
(require 'shipit-sections) ; Magit section utilities
(require 'shipit-pr-search)   ; PR search and selection
(require 'shipit-file-filter)  ; File filter subsystem
(require 'shipit-pr-actions) ; Interactive PR operations
(require 'shipit-pr-diff)   ; Diff, ediff, inline comment display
(require 'shipit-comments) ; Comment utilities
(require 'shipit-http)
(require 'shipit-gh-etag)
(require 'shipit-core)
(require 'shipit-render)   ; For shipit--get-comment-icon
(require 'shipit-checks)
(require 'shipit-notifications)

;; Ensure magit-section macros are available at compile time
(eval-when-compile
  (require 'magit-section))

;; Also require at runtime to ensure macros and functions are available
(require 'magit-section)

;; Forward declarations
(declare-function shipit-buffer-name "shipit-buffer")
(declare-function shipit-gitlab--api-request "shipit-gitlab-http")
(declare-function shipit--browse-pr-url "shipit-notifications")
(declare-function shipit-comment--fetch-reactions-batch "shipit-comments")
(declare-function shipit-comment--fetch-reactions "shipit-comments")
(declare-function shipit-issue--load-more-all "shipit-issues-buffer")
(declare-function shipit-issue-buffer-refresh "shipit-issues-buffer")
(declare-function shipit--in-shipit-context-p "shipit-core")
(declare-function shipit--open-file-at-point "shipit-diff")
(declare-function shipit-editor-open "shipit-editor")
(declare-function shipit--edit-comment-interactive "shipit-commands")
(declare-function shipit-mark-activities-read "shipit-commands")
(declare-function shipit--is-comment-unread-p "shipit-core")
(declare-function shipit--mark-comment-read "shipit-core")
(declare-function shipit--get-comment-unread-indicator "shipit-render")
(declare-function shipit--add-suggestion-text-properties "shipit-render")
(declare-function shipit--fontify-diff-hunk-lines "shipit-render")
(declare-function shipit--refine-diff-hunk "shipit-render")
(declare-function shipit--create-jira-mention-overlays "shipit-issue-jira")

;; Forward declarations for variables
(defvar shipit-inline-comment-faces)
(defvar shipit-inline-comment-username-color)
(defvar shipit-inline-comment-timestamp-color)
(defvar shipit-inline-comment-background-color)
(defvar shipit-inline-comment-username-face)
(defvar shipit-inline-comment-timestamp-face)
(defvar shipit-inline-comment-background-face)
(defvar shipit-comment-wrap-width)
(defvar shipit-render-wrap-column)
(defvar shipit--pr-completion-table)
(defvar shipit--embark-pr-repo)
(defvar shipit--dwim-handlers)
(defvar shipit--cached-current-user)
(defvar shipit--current-operation-comment-id)

;; Protective wrapper for magit-section-post-command-hook to handle nil point issues
(defun shipit--safe-magit-section-post-command-hook ()
  "Safe wrapper for magit-section-post-command-hook that only runs in magit buffers."
  ;; Only run if we're in a buffer with magit-section-mode or derived modes
  (when (and (fboundp 'magit-section-post-command-hook)
             (condition-case nil (point) (error nil))  ; Check if point is valid
             ;; Check if this buffer has magit sections (proper context)
             (or (bound-and-true-p magit-section-mode)
                 (derived-mode-p 'magit-section-mode)
                 ;; Also check if there's a magit-root-section (indicates magit buffer)
                 (bound-and-true-p magit-root-section)))
    (condition-case err
        (magit-section-post-command-hook)
      (error
       (when (fboundp 'shipit--debug-log)
         (shipit--debug-log "❌ Error in magit-section-post-command-hook: %S" err)))))
  ;; Show PR summary if cursor is on a PR reference
  (when (fboundp 'shipit--show-pr-summary-at-cursor)
    (condition-case nil
        (catch 'found
          (shipit--show-pr-summary-at-cursor))
      (error nil))))

;; Replace magit's hook with our safe version if magit-section is loaded
(when (featurep 'magit-section)
  (remove-hook 'post-command-hook 'magit-section-post-command-hook)
  (add-hook 'post-command-hook 'shipit--safe-magit-section-post-command-hook))

;; Also hook into after-load-functions to catch magit-section loading after shipit
(eval-after-load 'magit-section
  '(progn
     (remove-hook 'post-command-hook 'magit-section-post-command-hook)
     (add-hook 'post-command-hook 'shipit--safe-magit-section-post-command-hook)))

;;; Magit integration

;; Declare cache functions
(declare-function shipit--ensure-cache-initialized "shipit-cache")

;; Ensure this variable is defined (fallback if not loaded from core)
(defvar shipit-use-magit-sections-for-diff-comments t
  "Whether to use magit sections for hierarchical inline comments in diff buffers.")

;; Magit is required for this module
(eval-when-compile
  (when (locate-library "magit")
    (require 'magit)
    (require 'magit-section)))

;; At runtime, load magit if available (graceful degradation for test environments)


(require 'shipit-http)
(require 'shipit-render)
(require 'shipit-diff)

;; Forward declarations
(declare-function magit-section-match "magit-section")
(declare-function magit-current-section "magit-section")
(declare-function shipit--get-pr-review-details "shipit-http")
(declare-function shipit--group-inline-comments-by-file "shipit-http")
(declare-function shipit-gh-etag-invalidate-endpoint "shipit-gh-etag")

;; Keymaps (defined early to avoid forward reference warnings)
;; All keymaps inherit from magit-section-mode-map to support 1/2/3/4 and C-TAB
(defvar shipit-commit-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") 'shipit--commit-at-point)
    (define-key map (kbd "SPC") 'shipit--commit-at-point)
    (define-key map (kbd "M-;") 'shipit-dwim)
    (define-key map (kbd "M-w") 'shipit-copy-pr-url)
    (define-key map (kbd "n") 'magit-section-forward)
    (define-key map (kbd "p") 'magit-section-backward)
    map)
  "Keymap for shipit commit text.")

;; Declare and initialize keymap (functions can be forward-referenced with quoted symbols)
(defvar shipit-file-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") 'shipit--file-at-point)
    (define-key map (kbd "C-RET") 'shipit--open-file-from-worktree-at-point)
    (define-key map (kbd "SPC") 'shipit--file-at-point)
    (define-key map (kbd "e") 'shipit--ediff-file-at-point)
    (define-key map (kbd "o") 'shipit--open-file-at-point)
    ;; Note: 'f' for file filter is bound via section-specific overlays in Files Changed
    (define-key map (kbd "M-;") 'shipit-dwim)
    (define-key map (kbd "n") 'magit-section-forward)
    (define-key map (kbd "p") 'magit-section-backward)
    map)
  "Keymap for shipit PR file text.")

(defvar shipit-pr-header-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "s") 'shipit--select-pr)
    (define-key map (kbd "M-;") 'shipit-dwim)
    (define-key map (kbd "M-w") 'shipit-copy-pr-url)
    map)
  "Keymap for shipit PR header line.")

(defvar shipit--pending-commit-for-comments nil
  "SHA of commit that needs comment insertion via flag-based system.")

(defvar shipit--comment-retry-timer nil
  "Timer for retrying comment insertion.")

(defvar shipit--comment-retry-count 0
  "Number of retry attempts for comment insertion.")

(defvar shipit--comment-retry-max 25
  "Maximum number of retry attempts (5 seconds at 0.2s intervals).")

;; Filter state variables defined in shipit-file-filter.el
(defvar shipit--files-filter-text)
(defvar shipit--files-filter-mode)

(defun shipit--insert-general-comments-section (repo pr-number)
  "Insert general comments section with caching logic."
  (if (and shipit--general-comments-fetched
           (shipit--cached-general-comments-match-pr pr-number repo))
      ;; Use cached comments BUT always fetch fresh reactions
      (progn
        ;; Fetch reactions via backend dispatch before rendering
        ;; Filter out outdated comments - backends may not support reactions on them
        (when shipit--cached-general-comments
          (let ((active-comments (cl-remove-if (lambda (c) (cdr (assq 'outdated c))) shipit--cached-general-comments)))
            (when active-comments
              (shipit-comment--fetch-reactions-batch active-comments repo nil))))
        (let ((comments-section
               (magit-insert-section (general-comments nil t)
                 (let ((header-start (point)))
                   (magit-insert-heading (format "%s General Comments (%d)" (shipit--get-comment-type-icon "comment" "💬") (if (sequencep shipit--cached-general-comments) (length shipit--cached-general-comments) 0)))
                   (add-text-properties header-start (point)
                                        `(shipit-general-comments t
                                                                  shipit-pr-number ,pr-number
                                                                  shipit-repo ,repo)))
                 (magit-insert-section-body
                   (if (or (not shipit--cached-general-comments) (not (sequencep shipit--cached-general-comments)) (= (length shipit--cached-general-comments) 0))
                       (insert "   No general comments\n")
                     ;; Use hierarchical comment rendering system with quote-based threading
                     ;; NOTE: GitHub REST API accepts in_reply_to but doesn't return in_reply_to_id,
                     ;; so we use quote-based threading for general comments
                     ;; First, deduplicate general comments against inline comments to prevent duplicates
                     (let* ((inline-comment-ids (when (and (boundp 'shipit--cached-inline-comments) shipit--cached-inline-comments)
                                                  (mapcar (lambda (c) (cdr (assq 'id c))) shipit--cached-inline-comments)))
                            (deduplicated-general-comments
                             (if inline-comment-ids
                                 (cl-remove-if (lambda (comment)
                                                 (let ((comment-id (cdr (assq 'id comment))))
                                                   (member comment-id inline-comment-ids)))
                                               shipit--cached-general-comments)
                               shipit--cached-general-comments))
                            (threads (shipit--group-comments-by-api-replies deduplicated-general-comments))
                            (root-comments (gethash 'root threads)))
                       (dolist (comment root-comments)
                         ;; Use hierarchical system properly - it handles replies recursively
                         (shipit--insert-status-hierarchical-comment comment threads nil nil 0 nil nil repo pr-number)))
                     (insert "\n"))))))
          ;; Store imenu metadata for children so they appear even when collapsed
          ;; Since section is collapsed, children aren't accessible via slot.
          ;; Store username from cached data - goto function will search for it.
          (when (and comments-section shipit--cached-general-comments (> (length shipit--cached-general-comments) 0))
            (let ((child-names
                   (mapcar (lambda (comment)
                             (let* ((user (cdr (assq 'user comment)))
                                    (login (if user (cdr (assq 'login user)) "unknown")))
                               login))
                           shipit--cached-general-comments)))
              (shipit--imenu-store-children (oref comments-section start) child-names 'general-comments)))))
    ;; Cache rejected or not available - fetch synchronously then render
    (progn
      (shipit--debug-log "GENERAL-CACHE-REJECT: ❌ Cache not available, fetching fresh comments for PR #%s" pr-number)
      ;; Fetch comments synchronously - this populates the cache
      (shipit--fetch-general-comments repo pr-number nil)
      ;; Now insert the section with the freshly cached data
      (let ((comments-section
             (magit-insert-section (general-comments nil t)
               (let ((header-start (point)))
                 (magit-insert-heading (format "%s General Comments (%d)"
                                               (shipit--get-comment-type-icon "comment" "💬")
                                               (if (sequencep shipit--cached-general-comments)
                                                   (length shipit--cached-general-comments)
                                                 0)))
                 (add-text-properties header-start (point)
                                      `(shipit-general-comments t
                                                                shipit-pr-number ,pr-number
                                                                shipit-repo ,repo)))
               (magit-insert-section-body
                 (if (or (not shipit--cached-general-comments)
                         (not (sequencep shipit--cached-general-comments))
                         (= (length shipit--cached-general-comments) 0))
                     (insert "   No general comments\n")
                   ;; Use same rendering logic as cached branch with quote-based threading
                   (let* ((inline-comment-ids (when (and (boundp 'shipit--cached-inline-comments)
                                                         shipit--cached-inline-comments)
                                                (mapcar (lambda (c) (cdr (assq 'id c)))
                                                        shipit--cached-inline-comments)))
                          (deduplicated-general-comments
                           (if inline-comment-ids
                               (cl-remove-if (lambda (comment)
                                               (let ((comment-id (cdr (assq 'id comment))))
                                                 (member comment-id inline-comment-ids)))
                                             shipit--cached-general-comments)
                             shipit--cached-general-comments))
                          (threads (shipit--group-comments-by-api-replies deduplicated-general-comments))
                          (root-comments (gethash 'root threads)))
                     (dolist (comment root-comments)
                       (shipit--insert-status-hierarchical-comment comment threads nil nil 0 nil nil repo pr-number)))
                     (insert "\n"))))))
        ;; Store imenu metadata for children so they appear even when collapsed
        ;; Since section is collapsed, children aren't accessible via slot.
        ;; Store username from cached data - goto function will search for it.
        (when (and comments-section shipit--cached-general-comments (> (length shipit--cached-general-comments) 0))
          (let ((child-names
                 (mapcar (lambda (comment)
                           (let* ((user (cdr (assq 'user comment)))
                                  (login (if user (cdr (assq 'login user)) "unknown")))
                             login))
                         shipit--cached-general-comments)))
            (shipit--imenu-store-children (oref comments-section start) child-names 'general-comments)))))))

(defun shipit--refresh-general-comments-section (_pr-number _repo)
  "Refresh the buffer after general comment change.
Uses shipit-buffer-refresh which is fast because shipit--add-general-comment-to-pr
already updates the cache before calling this function."
  (when (fboundp 'shipit-buffer-refresh)
    (shipit-buffer-refresh)))

(defun shipit--delete-general-comment-section-targeted (comment-id)
  "Delete the general comment section for COMMENT-ID directly from buffer.
Returns t on success, nil if section not found.
This is faster than a full buffer refresh since we just remove the section."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil)
          (pos (point-min)))
      ;; Find the comment by shipit-comment-id text property
      (while (and (not found)
                  (setq pos (text-property-any pos (point-max) 'shipit-comment-id comment-id)))
        (goto-char pos)
        (let ((section (magit-current-section)))
          (when section
            (setq found t)
            (let* ((section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
                   (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m)))
                   (start-pos (if (markerp section-start) (marker-position section-start) section-start))
                   (end-pos (if (markerp section-end) (marker-position section-end) section-end))
                   (parent-section (oref section parent))
                   (inhibit-read-only t))
              (shipit--debug-log "🗑️ DELETE-TARGETED: Removing comment %s section from %d to %d"
                                 comment-id start-pos end-pos)
              ;; Remove section from parent's children list
              (when parent-section
                (oset parent-section children
                      (delq section (oref parent-section children))))
              ;; Delete the section content from buffer
              (delete-region start-pos end-pos)
              ;; Update parent's end marker if needed
              (when (and parent-section
                         (> (marker-position (oref parent-section end)) end-pos))
                ;; Parent end was beyond deleted region, adjust it
                nil)  ; Markers auto-adjust on delete
              ;; Update the General Comments header count
              (shipit--update-general-comments-header-count)))))
      found)))

(defun shipit--insert-reply-as-child (_parent-comment-id _new-comment repo pr-number)
  "Refresh the General Comments section to include the newly added reply.
This uses targeted section refresh which properly maintains magit section boundaries.
REPO and PR-NUMBER provide context for rendering."
  (shipit--refresh-general-comments-section-targeted repo pr-number))

(defun shipit--refresh-general-comments-section-targeted (repo pr-number)
  "Refresh only the General Comments section with current cached data.
REPO and PR-NUMBER provide context for rendering.
Returns t on success, nil if section not found."
  (let ((gc-section (shipit--find-section-by-type 'general-comments)))
    (if (not gc-section)
        (progn
          (shipit--debug-log "GC-REFRESH: No general-comments section found")
          nil)
      (let* ((content-marker (oref gc-section content))
             (end-marker (oref gc-section end))
             (content-pos (and content-marker (marker-position content-marker)))
             (end-pos (and end-marker (marker-position end-marker)))
             (inhibit-read-only t)
             (comment-count (if (and (boundp 'shipit--cached-general-comments)
                                     (sequencep shipit--cached-general-comments))
                                (length shipit--cached-general-comments)
                              0)))
        (shipit--debug-log "GC-REFRESH: Found section, content=%s end=%s, %d comments"
                           content-pos end-pos comment-count)
        (save-excursion
          ;; Step 1: Update the heading count
          (goto-char (oref gc-section start))
          (when (re-search-forward "General Comments (\\([0-9]+\\))" (line-end-position) t)
            (replace-match (number-to-string comment-count) nil nil nil 1))

          ;; Step 2: Clear old children list
          (oset gc-section children nil)

          ;; Step 3: Clear washer function
          (when (slot-boundp gc-section 'washer)
            (oset gc-section washer nil))

          ;; Step 4: Remove invisible overlays
          (when (and content-pos end-pos)
            (remove-overlays content-pos end-pos 'invisible t))

          ;; Step 5: Delete and re-insert content
          (when (and content-pos end-pos)
            (when (> end-pos content-pos)
              (delete-region content-pos end-pos))

            (goto-char content-pos)

            ;; Insert new content with proper parent binding
            (let ((magit-insert-section--parent gc-section))
              (if (or (not shipit--cached-general-comments)
                      (not (sequencep shipit--cached-general-comments))
                      (= (length shipit--cached-general-comments) 0))
                  (insert "   No general comments\n")
                ;; Re-render all comments with threading
                (let* ((inline-comment-ids
                        (when (and (boundp 'shipit--cached-inline-comments)
                                   shipit--cached-inline-comments)
                          (mapcar (lambda (c) (cdr (assq 'id c)))
                                  shipit--cached-inline-comments)))
                       (deduplicated-comments
                        (if inline-comment-ids
                            (cl-remove-if (lambda (comment)
                                            (member (cdr (assq 'id comment))
                                                    inline-comment-ids))
                                          shipit--cached-general-comments)
                          shipit--cached-general-comments))
                       (threads (shipit--group-comments-by-api-replies deduplicated-comments))
                       (root-comments (gethash 'root threads)))
                  (dolist (comment root-comments)
                    (shipit--insert-status-hierarchical-comment
                     comment threads nil nil 0 nil nil repo pr-number)))))

            ;; Step 6: Update section markers
            (oset gc-section end (point-marker))
            (oset gc-section content (copy-marker content-pos))

            (shipit--debug-log "GC-REFRESH: Updated section, new end=%d" (point))))
        t))))

(defun shipit--update-general-comments-header-count ()
  "Update the General Comments section header to reflect current comment count."
  (save-excursion
    (goto-char (point-min))
    (let ((gc-start (text-property-any (point-min) (point-max) 'shipit-general-comments t)))
      (when gc-start
        (goto-char gc-start)
        (let ((section (magit-current-section)))
          (when section
            (let* ((start-pos (oref section start))
                   (content-pos (oref section content))
                   (start-val (if (markerp start-pos) (marker-position start-pos) start-pos))
                   (content-val (if (markerp content-pos) (marker-position content-pos) content-pos))
                   (comment-count (if (and (boundp 'shipit--cached-general-comments)
                                           shipit--cached-general-comments)
                                      (length shipit--cached-general-comments)
                                    0))
                   (inhibit-read-only t))
              ;; Find and update the count in the heading
              (goto-char start-val)
              (when (re-search-forward "(\\([0-9]+\\))" content-val t)
                (replace-match (number-to-string comment-count) nil nil nil 1)
                (shipit--debug-log "🗑️ DELETE-TARGETED: Updated header count to %d" comment-count)))))))))

;; Resolved comment functions now in shipit-comments.el

(defun shipit--insert-comment (comment &key base-indent repo pr-number
                                       include-header include-prefix
                                       prefix-str apply-faces apply-blockquote-styling
                                       section-insert inline-expanded resolved depth)
  "Insert a formatted COMMENT with customizable rendering options.

COMMENT: The comment object to render
BASE-INDENT: Base indentation level in spaces (8 for files, 6 for general comments)
REPO: Repository name (for properties)
PR-NUMBER: PR number (for properties)
INCLUDE-HEADER: Whether to render header (default: t)
INCLUDE-PREFIX: Whether to include prefix like └─ for replies (default: t)
PREFIX-STR: Custom prefix string to use instead of auto-generated
APPLY-FACES: Whether to apply styled faces (default: t)
APPLY-BLOCKQUOTE-STYLING: Whether to apply blue blockquote styling (default: nil)
SECTION-INSERT: If 'body-only, wraps body in magit-insert-section-body; if t, wraps all in magit-insert-section
INLINE-EXPANDED: If t, use inline-expanded context for icons
RESOLVED: If t, mark comment as resolved
DEPTH: Current nesting depth (0 for root comments)

Returns: nil"
  (let* ((comment-id (cdr (assq 'id comment)))
         (user-obj (or (cdr (assq 'user comment)) (cdr (assq 'author comment))))
         (user (if (stringp user-obj) user-obj (cdr (assq 'login user-obj))))
         (raw-body (cdr (assq 'body comment)))
         (created (cdr (assq 'created_at comment)))
         (formatted-timestamp (shipit--format-timestamp created))
         (file-path (cdr (assq 'path comment)))
         (line-number (cdr (assq 'line comment)))
         (actual-depth (or depth 0))
         (actual-base-indent (or base-indent 8))
         (actual-include-header (if (eq include-header nil) nil t))
         (actual-include-prefix (if (eq include-prefix nil) nil t))
         (actual-apply-faces (if (eq apply-faces nil) nil t))
         (actual-apply-blockquote-styling (if apply-blockquote-styling t nil))
         (actual-inline-expanded (if inline-expanded t nil))
         (indent-str (make-string actual-base-indent 32))
         (indent-level actual-base-indent)
         (indented-body (shipit--render-comment-body comment indent-level)))
    (when actual-include-header
      (let* ((icon (shipit--get-comment-icon comment (if actual-inline-expanded 'inline-expanded 'general)))
             (is-resolved (or resolved (shipit--is-comment-in-resolved-thread comment-id)))
             (resolved-suffix (if is-resolved
                                 (propertize " [RESOLVED]" 'font-lock-face 'magit-dimmed 'slant 'italic)
                               ""))
             (header-prefix (if (and actual-include-prefix (> actual-depth 0))
                               (or prefix-str "└─ ")
                             "     "))
             (header-str (concat header-prefix icon user " (" formatted-timestamp ")" resolved-suffix))
             (header-start (point)))
        (insert header-str)
        (insert "\n")
        (add-text-properties header-start (point)
                            (list 'font-lock-face 'bold
                                  'shipit-comment t
                                  'shipit-comment-id comment-id
                                  'shipit-comment-body raw-body
                                  'shipit-file-path file-path
                                  'shipit-line-number line-number))
        (when actual-apply-faces
          (let ((user-pos (string-match user header-str))
                (paren-start-pos (string-match "(" header-str))
                (paren-end-pos (string-match ")" header-str)))
            (when user-pos
              (put-text-property (+ header-start user-pos)
                               (+ header-start user-pos (length user))
                               'font-lock-face 'shipit-username-face))
            (when (and paren-start-pos paren-end-pos)
              (put-text-property (+ header-start paren-start-pos 1)
                               (+ header-start paren-end-pos)
                               'font-lock-face 'shipit-timestamp-face))))))
    (let ((body-start (point)))
      ;; indented-body already includes proper indentation from shipit--render-comment-body
      (insert indented-body)
      (insert "\n")
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
      (let ((props (list 'shipit-comment t
                        'shipit-comment-id comment-id
                        'shipit-comment-body raw-body
                        'shipit-comment-body-text t
                        'shipit-file-path file-path
                        'shipit-line-number line-number
                        'shipit-repo repo
                        'shipit-pr-number pr-number)))
        (add-text-properties body-start (point) props))
      ;; Add suggestion text properties if body contains a suggestion block
      (shipit--add-suggestion-text-properties body-start (point) comment)
      (when actual-apply-blockquote-styling
        (shipit--apply-blockquote-faces body-start (point)))
      ;; Apply strikethrough faces for ~text~ patterns
      (shipit--apply-strikethrough-faces body-start (point))
      ;; Apply code block syntax highlighting and backgrounds
      (shipit--apply-code-block-backgrounds-in-region body-start (point)))
    (let ((reactions (shipit--format-comment-reactions comment t)))
      (when reactions
        (let ((blank-start (point)))
          (insert "\n")
          (add-text-properties blank-start (point)
                               (list 'shipit-comment t
                                     'shipit-comment-id comment-id
                                     'shipit-comment-body raw-body
                                     'shipit-file-path file-path
                                     'shipit-line-number line-number
                                     'shipit-repo repo
                                     'shipit-pr-number pr-number)))
        (let ((reactions-start (point)))
          (insert indent-str)
          (insert reactions)
          (insert "\n")
          (add-text-properties reactions-start (point)
                               (list 'shipit-reactions t
                                     'shipit-comment t
                                     'shipit-comment-id comment-id
                                     'shipit-comment-body raw-body
                                     'shipit-file-path file-path
                                     'shipit-line-number line-number
                                     'shipit-repo repo
                                     'shipit-pr-number pr-number)))))
    (let ((newline-start (point)))
      (insert "\n")
      (add-text-properties newline-start (point)
                           (list 'shipit-comment t
                                 'shipit-comment-id comment-id
                                 'shipit-comment-body raw-body
                                 'shipit-file-path file-path
                                 'shipit-line-number line-number
                                 'shipit-repo repo
                                 'shipit-pr-number pr-number)))
    nil))

(defun shipit--get-inline-comments-for-review (review-id)
  "Get inline comments that belong to REVIEW-ID, including all replies.
Returns a list of inline comments from the cache that have matching
`pull_request_review_id', plus any replies to those comments."
  (when (and review-id
             (boundp 'shipit--cached-inline-comments)
             shipit--cached-inline-comments)
    (let* (;; First, get root comments that belong to this review
           (root-comments (cl-remove-if-not
                           (lambda (c)
                             (equal (cdr (assq 'pull_request_review_id c)) review-id))
                           shipit--cached-inline-comments))
           (root-ids (mapcar (lambda (c) (cdr (assq 'id c))) root-comments))
           ;; Build a set of all comment IDs to include (starting with roots)
           (included-ids (make-hash-table :test 'equal)))
      ;; Add root IDs to the set
      (dolist (id root-ids)
        (puthash id t included-ids))
      ;; Iteratively find replies until no new ones are found
      (let ((changed t))
        (while changed
          (setq changed nil)
          (dolist (comment shipit--cached-inline-comments)
            (let ((id (cdr (assq 'id comment)))
                  (reply-to (cdr (assq 'in_reply_to_id comment))))
              ;; If this comment replies to an included comment, include it too
              (when (and reply-to
                         (gethash reply-to included-ids)
                         (not (gethash id included-ids)))
                (puthash id t included-ids)
                (setq changed t))))))
      ;; Return all comments whose IDs are in the included set
      (cl-remove-if-not
       (lambda (c)
         (gethash (cdr (assq 'id c)) included-ids))
       shipit--cached-inline-comments))))

(defun shipit--insert-review-inline-comments (review-id repo pr-number)
  "Insert inline comments for REVIEW-ID using nested magit-sections.
REPO and PR-NUMBER provide context for rendering.
Uses the same hierarchical threading as general comments."
  (let ((inline-comments (shipit--get-inline-comments-for-review review-id)))
    (when inline-comments
      ;; Group by API replies (in_reply_to_id) for proper threading
      (let* ((threads (shipit--group-comments-by-api-replies inline-comments))
             (root-comments (gethash 'root threads)))
        (dolist (comment root-comments)
          (let ((file-path (cdr (assq 'path comment)))
                (is-outdated (cdr (assq 'outdated comment))))
            ;; Use the hierarchical comment system with show-context=t for diff hunks
            ;; Pass depth-offset=1 to indent as children of the review body
            (shipit--insert-status-hierarchical-comment
             comment threads file-path is-outdated 0 t t repo pr-number nil 1)))))))

(defun shipit--insert-status-hierarchical-comment (comment threads file-path is-outdated depth is-inline &optional show-context repo pr-number is-resolved-thread depth-offset)
  "Insert a COMMENT with hierarchical threading at DEPTH level.
IS-INLINE indicates if this is an inline comment (t) or general comment (nil).
If SHOW-CONTEXT is t, displays diff context for root comments.
DEPTH-OFFSET is added to indentation (used for inline comments under reviews)."
  (let* ((comment-id (cdr (assq 'id comment)))
         (user-obj (cdr (assq 'user comment)))
         (user (or (cdr (assq 'login user-obj)) "Unknown"))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (created (cdr (assq 'created_at comment)))
         (formatted-timestamp (shipit--format-timestamp created))
         ;; Use the stored reply-depth from threading analysis (includes quote-based detection)
         (actual-depth (or (cdr (assq 'reply-depth comment)) depth))
         ;; Add optional offset for extra indentation (e.g., inline comments under reviews)
         (indent-depth (+ actual-depth (or depth-offset 0)))
         ;; Create proper indentation: root comments get no extra indent, replies get progressive indentation
         ;; Formula for replies: 3 spaces base + (depth-1)*6 additional spaces
         ;; This aligns reply headers with the body of the parent comment
         (base-indent (if (> indent-depth 0)
                          (make-string (+ 3 (* (1- indent-depth) 6)) ?\s)
                        ""))
         ;; Always use indentation for threaded replies to show nesting structure
         (thread-prefix base-indent)
         (tree-indicator (if (> indent-depth 0) "└─ " ""))
         ;; Use same orange color scheme as old system
         (orange-color (if (eq (frame-parameter nil 'background-mode) 'light)
                           "#d2691e" "#ff8c00"))
         (user-face `(:foreground ,orange-color :weight bold :extend t)))

    ;; Wrap each comment in a magit-section for TAB navigation and structure
    ;; Use 'general-comment type to match the declared section type for proper n/p navigation
    (magit-insert-section (general-comment comment-id)
      ;; Insert header with threading indicators (keep replies close to root level)
      ;; Use simple indentation: root at 10 spaces, replies just slightly more
      (let ((status-comment-keymap (make-sparse-keymap)))
        ;; Set parent keymap so undefined keys like "1" fall through to magit keybindings
        (set-keymap-parent status-comment-keymap (current-local-map))
        (define-key status-comment-keymap (kbd "TAB") 'magit-section-toggle)
        ;; RET not bound - use M-; for comment actions
        (define-key status-comment-keymap (kbd "M-;") 'shipit-dwim)
        (define-key status-comment-keymap (kbd "n") 'magit-section-forward)
        (define-key status-comment-keymap (kbd "p") 'magit-section-backward)
        (define-key status-comment-keymap (kbd "k") 'shipit-delete-comment-at-point)
        ;; Check if comment is in a resolved thread and if it's outdated
        ;; For replies, inherit both outdated and resolved status from the thread root
        (let* ((thread-is-resolved (or is-resolved-thread
                                       (and comment-id (shipit--is-comment-in-resolved-thread comment-id))))
               (thread-is-outdated (or is-outdated
                                       (cdr (assq 'outdated comment))
                                       (cdr (assq 'is_outdated comment))))
               (outdated-indicator (if thread-is-outdated " [OUTDATED]" ""))
               (resolved-indicator (if thread-is-resolved " [RESOLVED]" ""))
               ;; For general comments, use avatar instead of icon
               (avatar-display (if (and (not is-inline) (boundp 'shipit-show-avatars) shipit-show-avatars avatar-url)
                                  (shipit--create-avatar-display user avatar-url 16)
                                ""))
               (icon-text (if (and (not is-inline) (boundp 'shipit-show-avatars) shipit-show-avatars avatar-url)
                             ""
                           (format "%s" (shipit--get-comment-icon comment (if is-inline 'inline-expanded 'general)))))
               (avatar-space (if (and (not is-inline) (boundp 'shipit-show-avatars) shipit-show-avatars avatar-url) " " ""))
               (icon-len (length icon-text))
               (plain-user (or user "Unknown"))
               (header-str (concat "   " thread-prefix tree-indicator
                                   icon-text avatar-display avatar-space
                                   plain-user
                                   " ("
                                   formatted-timestamp
                                   ")"
                                   outdated-indicator resolved-indicator))
               (header-start (point)))
          (magit-insert-heading
            (propertize header-str
                        'shipit-comment t
                        'shipit-comment-id comment-id
                        'shipit-comment-body (shipit--clean-text (or (cdr (assq 'body comment)) ""))
                        'shipit-resolved thread-is-resolved
                        'keymap status-comment-keymap
                        'local-map status-comment-keymap))
          ;; Apply component faces to username and timestamp after insertion.
          ;; Magit's `magit-insert-heading' sets `font-lock-face' to
          ;; `magit-section-heading' across the whole heading; that wins
          ;; over plain `face' props under font-lock-mode, so we must
          ;; override `font-lock-face' directly on these regions.
          (let* ((header-end (point))
                 (header-content (buffer-substring header-start header-end))
                 (user-pos (string-match (regexp-quote plain-user) header-content)))
            (when user-pos
              (put-text-property (+ header-start user-pos)
                                 (+ header-start user-pos (length plain-user))
                                 'font-lock-face 'shipit-username-face))
            (let ((ts-pos (string-match (regexp-quote formatted-timestamp) header-content)))
              (when ts-pos
                (put-text-property (+ header-start ts-pos)
                                   (+ header-start ts-pos (length formatted-timestamp))
                                   'font-lock-face 'shipit-timestamp-face))))
            ;; Apply dimmed face to [RESOLVED] tag if present
            (when thread-is-resolved
              (let ((resolved-pos (string-match "\\[RESOLVED\\]" header-content)))
                (when resolved-pos
                  (add-face-text-property (+ header-start resolved-pos)
                                          (+ header-start resolved-pos 10)
                                          'shadow t)
                  (add-face-text-property (+ header-start resolved-pos)
                                          (+ header-start resolved-pos 10)
                                          'italic t))))
            ;; Add unread indicator at end of heading (after face styling to preserve red color)
            (save-excursion
              (goto-char header-start)
              (end-of-line)
              (insert (shipit--get-comment-unread-indicator comment-id)))

          (magit-insert-section-body
            ;; Insert diff context for root comments when requested
            (when (and show-context (= actual-depth 0))
              (let* ((context-hunk (cdr (assq 'diff_hunk comment)))
                     (line (or (cdr (assq 'line comment)) 0))
                     (start-line (or (cdr (assq 'start_line comment)) line))
                     (is-multiline (and start-line line (not (= start-line line)))))
                (when (and context-hunk (not (string-empty-p context-hunk)))
                  (insert (format "         ┌─ Code Context%s:\n"
                                  (if is-multiline (format " (Lines %s-%s)" start-line line) "")))
                  (let ((hunk-lines (split-string context-hunk "\n")))
                    (dolist (hunk-line hunk-lines)
                      (when (not (string-empty-p hunk-line))
                        (let ((line-start (point))
                              (prefix (if (> (length hunk-line) 0) (substring hunk-line 0 1) "")))
                          (insert (format "         │ %s\n" hunk-line))
                          ;; Apply diff coloring safely
                          (let ((diff-face (cond
                                            ((string= prefix "+") 'magit-diff-added)
                                            ((string= prefix "-") 'magit-diff-removed)
                                            ((string= prefix "@") 'magit-diff-hunk-heading)
                                            (t 'magit-diff-context))))
                            ;; Apply face and shipit comment properties
                            (let ((properties `(,@(when (facep diff-face) `(font-lock-face ,diff-face))
                                                shipit-comment t
                                                shipit-comment-id ,comment-id
                                                shipit-comment-body ,(shipit--clean-text (or (cdr (assq 'body comment)) ""))
                                                shipit-file-path ,file-path
                                                shipit-line-number ,(or (cdr (assq 'line comment)) 0)
                                                shipit-repo ,repo
                                                shipit-pr-number ,pr-number)))
                              (add-text-properties line-start (point) properties)))))))
                  (insert "         └─\n"))))

            ;; Insert comment body with proper indentation for status section
            ;; Body text aligns with the username in the header
            (let* (;; Use explicit is-inline parameter instead of dynamic detection
                   ;; For general comments at depth 0: 3 spaces + icon (2 chars) = 6 spaces
                   ;; For general comments with depth > 0, use standard calculation
                   ;; For inline comments, use standard calculation
                   (body-indent (if (and (not is-inline) (= actual-depth 0))
                                    6  ; Align with username for top-level general comments (3 spaces + emoji which is 2 chars wide)
                                  (+ (if is-inline 3 3) (length thread-prefix) (length tree-indicator) 3))))
              (shipit--insert-comment-body-only comment body-indent status-comment-keymap file-path repo pr-number (or (cdr (assq 'line comment)) 0) t)
              (insert "\n")
              ;; For review comments, show associated inline comments using nested magit-sections
              (when (equal (cdr (assq 'shipit-comment-type comment)) "review")
                (shipit--insert-review-inline-comments comment-id repo pr-number)))

            ;; Insert reactions with proper indentation (aligned with username, always shows with placeholder)
            (let ((reactions (shipit--format-comment-reactions comment is-inline)))
              (when reactions
                (let ((reactions-indent (make-string (if (and (not is-inline) (= actual-depth 0))
                                                         6  ; Align with username for root general comments
                                                       (+ 3 (length thread-prefix) (length tree-indicator) 3))
                                                     ?\s))
                      (reactions-start (point)))
                  (insert (format "%s%s\n" reactions-indent reactions))
                  (let ((properties `(shipit-reactions t
                                                       shipit-comment t
                                                       shipit-comment-id ,comment-id
                                                       shipit-comment-body ,(shipit--clean-text (or (cdr (assq 'body comment)) ""))
                                                       shipit-file-path ,file-path
                                                       shipit-line-number ,(or (cdr (assq 'line comment)) 0)
                                                       shipit-repo ,repo
                                                       shipit-pr-number ,pr-number)))
                    (add-text-properties reactions-start (point)
                                         `(,@properties
                                           keymap ,status-comment-keymap
                                           local-map ,status-comment-keymap))))))

            ;; Insert separator newlines between root-level comments (1 blank line = 2 newlines)
            ;; Only apply spacing for root level comments (not replies)
            ;; NOTE: DO NOT add text properties to spacing newlines - they must remain as
            ;; structural whitespace to preserve magit section boundaries. Adding shipit-comment
            ;; properties causes magit to think blank lines are part of the comment content,
            ;; which breaks section detection for subsequent comments.
            (let* ((spacing-count (if (and is-inline (= actual-depth 0))
                                     2  ; 1 blank line after comment
                                   1))  ; No blank line for replies
                   (_newline-start (point)))
              (dotimes (_ spacing-count)
                (insert "\n")))

            ;; Recursively insert replies as nested child sections
            (let ((replies (when threads (gethash comment-id threads))))
              (when replies
                (dolist (reply replies)
                  ;; Pass both thread outdated status and resolved status to replies
                  ;; If the thread is resolved, all replies should also show as resolved
                  (shipit--insert-status-hierarchical-comment reply threads file-path thread-is-outdated (1+ depth) is-inline show-context repo pr-number thread-is-resolved depth-offset))))))))
    ;; Apply rounded background to inline comments only
    (when (and is-inline (display-graphic-p)
               (fboundp 'shipit-rounded--apply-to-section))
      (shipit-rounded--apply-to-section (magit-current-section) orange-color))))

(defun shipit--insert-file-comment-with-replies (comment threads repo pr-number)
  "Insert COMMENT and its replies recursively without magit sections.
Used for ediff buffers which aren't in magit-section-mode."
  ;; Ensure we're on a new line before inserting comment
  (unless (bolp)
    (insert "\n"))
  (insert "\n")  ; Blank line before comment header
  (shipit--insert-file-comment comment repo pr-number)
  ;; Insert replies
  (let ((comment-id (cdr (assq 'id comment)))
        (replies (when threads (gethash (cdr (assq 'id comment)) threads))))
    (when replies
      (dolist (reply replies)
        (shipit--insert-file-comment-with-replies reply threads repo pr-number)))))

(defun shipit--find-region-bounds-at-point ()
  "Find the bounds of the shipit region at point.
For comments, finds the specific comment rather than the entire comments section.
Returns (start . end) cons cell or nil if not in a shipit region."
  (cond
   ;; For comments, find the specific comment boundary
   ((get-text-property (point) 'shipit-comment)
    (let ((comment-id (get-text-property (point) 'shipit-comment-id))
          (start (point))
          (end (point)))
      ;; If we have a comment ID, find bounds of this specific comment
      (if comment-id
          (progn
            ;; Find start of this specific comment
            (while (and (> start (point-min))
                        (equal (get-text-property (1- start) 'shipit-comment-id) comment-id))
              (setq start (1- start)))
            ;; Find end of this specific comment
            (while (and (< end (point-max))
                        (equal (get-text-property end 'shipit-comment-id) comment-id))
              (setq end (1+ end)))
            (cons start end))
        ;; Fallback: find any shipit-comment region
        (progn
          (while (and (> start (point-min))
                      (get-text-property (1- start) 'shipit-comment))
            (setq start (1- start)))
          (while (and (< end (point-max))
                      (get-text-property end 'shipit-comment))
            (setq end (1+ end)))
          (cons start end)))))
   ;; For PR headers with specific metadata properties, find individual line bounds
   ((or (get-text-property (point) 'shipit-pr-state)
        (get-text-property (point) 'shipit-pr-draft)
        (get-text-property (point) 'shipit-pr-refs)
        (get-text-property (point) 'shipit-pr-created)
        (get-text-property (point) 'shipit-pr-author))
    ;; Find bounds of just this metadata line
    (let ((start (point))
          (end (point)))
      ;; Find start of current line
      (while (and (> start (point-min))
                  (not (eq (char-before start) ?\n)))
        (setq start (1- start)))
      ;; Find end of current line
      (while (and (< end (point-max))
                  (not (eq (char-after end) ?\n)))
        (setq end (1+ end)))
      (cons start end)))
   ;; For PR headers (fallback - entire header region)
   ((get-text-property (point) 'shipit-pr-header)
    (let ((start (point))
          (end (point)))
      ;; Find start of PR header region
      (while (and (> start (point-min))
                  (get-text-property (1- start) 'shipit-pr-header))
        (setq start (1- start)))
      ;; Find end of PR header region
      (while (and (< end (point-max))
                  (get-text-property end 'shipit-pr-header))
        (setq end (1+ end)))
      (cons start end)))
   ;; For file headers, find the file header region
   ((and (get-text-property (point) 'shipit-file-path)
         (not (get-text-property (point) 'shipit-comment)))
    (let ((start (point))
          (end (point)))
      ;; Find start of file header region (same file-path)
      (while (and (> start (point-min))
                  (equal (get-text-property (1- start) 'shipit-file-path)
                         (get-text-property (point) 'shipit-file-path))
                  (not (get-text-property (1- start) 'shipit-comment)))
        (setq start (1- start)))
      ;; Find end of file header region (same file-path)
      (while (and (< end (point-max))
                  (equal (get-text-property end 'shipit-file-path)
                         (get-text-property (point) 'shipit-file-path))
                  (not (get-text-property end 'shipit-comment)))
        (setq end (1+ end)))
      (cons start end)))
   ;; Not in a shipit region
   (t nil)))

(defun shipit--insert-labels-section (pr-data &optional repo)
  "Insert labels section for PR with expandable content."
  (let* ((labels (cdr (assq 'labels pr-data)))
         (label-count (if labels (length labels) 0))
         (pr-number (cdr (assq 'number pr-data)))
         (repo (or repo (shipit--get-repo-from-remote))))
    (magit-insert-section (labels nil t)
      (let ((heading-start (point)))
        (magit-insert-heading (format "%s Labels (%d)" (shipit--get-label-icon "🏷") label-count))
        (add-text-properties heading-start (point)
                             `(shipit-labels t
                                             shipit-pr-labels ,labels
                                             shipit-pr-number ,pr-number
                                             shipit-repo ,repo)))
      (magit-insert-section-body
        (if (= label-count 0)
            (let ((start (point)))
              (insert "   No labels\n")
              (add-text-properties start (point)
                                   `(shipit-labels t
                                                   shipit-pr-labels nil
                                                   shipit-pr-number ,pr-number
                                                   shipit-repo ,repo)))
          (dolist (label labels)
            ;; Create individual magit section for each label
            (magit-insert-section (label label)
              (let ((start (point)))
                (insert "   ")
                (insert (shipit--format-label label))
                (insert "\n")
                (add-text-properties start (1- (point))
                                     `(shipit-labels t
                                                     shipit-pr-labels ,labels
                                                     shipit-pr-number ,pr-number
                                                     shipit-repo ,repo
                                                     help-echo "Individual label")))))
          (insert "\n"))))))

(defun shipit--get-requested-reviewers (repo pr-number)
  "Get requested reviewers for PR-NUMBER in REPO.
Dispatches to the active PR backend's :fetch-requested-reviewers."
  (if (or (not repo) (not pr-number) (string-empty-p repo))
      nil
    (condition-case err
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (fetch-fn (plist-get backend :fetch-requested-reviewers)))
          (funcall fetch-fn config pr-number))
      (error
       (shipit--debug-log "Error fetching requested reviewers: repo=%s pr-number=%s: %s"
                          repo pr-number (error-message-string err))
       nil))))

(defun shipit--insert-reviewers-section (repo pr pr-number)
  "Insert unified reviewers section showing both requested reviewers and completed reviews."
  (shipit--debug-log "shipit--insert-reviewers-section called: repo=%s pr-number=%s" repo pr-number)
  ;; Early validation to prevent API calls with invalid data
  (if (or (not repo) (not pr-number) (string-empty-p repo))
      (progn
        (shipit--debug-log "Skipping reviewers section - invalid data: repo=%s pr-number=%s" repo pr-number)
        (magit-insert-section (reviewers nil t)
          (let ((heading-start (point)))
            (magit-insert-heading (format "%s Reviewers (0)" (shipit--get-user-type-icon "team" "👥")))
            (add-text-properties heading-start (point)
                                 `(shipit-reviewers t
                                                    shipit-pr-number ,pr-number
                                                    shipit-repo ,repo)))
          (magit-insert-section-body
            (insert "   Error: Repository or PR data not available\n")
            (insert "\n"))))
    ;; Proceed with normal operation
    (let* ((requested-data (shipit--get-requested-reviewers repo pr-number))
           (requested-users (when requested-data (cdr (assq 'users requested-data))))
           (requested-teams (when requested-data (cdr (assq 'teams requested-data))))
           (requested-reviewers (append
                                 (mapcar (lambda (user) (cdr (assq 'login user))) (or requested-users '()))
                                 (mapcar (lambda (team) (format "@%s" (cdr (assq 'slug team)))) (or requested-teams '()))))
           ;; Get completed reviews
           (cache-key (format "reviews-%s-%s" repo pr-number))
           (review-details (or (gethash cache-key shipit--comment-cache)
                               (condition-case err
                                   (progn
                                     (let ((result (shipit--get-pr-review-details repo pr-number)))
                                       (puthash cache-key result shipit--comment-cache)
                                       result))
                                 (error
                                  (message "Error fetching review details: %S" err)
                                  nil))))
           (total-count (+ (length requested-reviewers) (if review-details (length review-details) 0))))
      (magit-insert-section (reviewers nil t)
        (let ((heading-start (point)))
          (magit-insert-heading (format "%s Reviewers (%d)" (shipit--get-user-type-icon "team" "👥") total-count))
          (add-text-properties heading-start (point)
                               `(shipit-reviewers t
                                                  shipit-pr-reviewers ,requested-reviewers
                                                  shipit-pr-number ,pr-number
                                                  shipit-repo ,repo)))
        (magit-insert-section-body
          ;; Show completed reviews first (with status icons)
          (if review-details
              (dolist (review review-details)
                (let* ((user (or (cdr (assq 'user review)) "Unknown"))
                       (avatar-url (cdr (assq 'avatar_url (cdr (assq 'user-obj review)))))
                       (status-icon (or (cdr (assq 'status-icon review)) ""))
                       (status-text (or (cdr (assq 'status-text review)) "unknown"))
                       (body (cdr (assq 'body review)))
                       (start (point))
                       (line-text (if body
                                      (format "   %s %s%s (%s) - \"%s\"\n"
                                              status-icon
                                              (if shipit-show-avatars
                                                  (concat (shipit--create-avatar-display user avatar-url 16) " ")
                                                "")
                                              (propertize user 'font-lock-face 'shipit-username-face) status-text
                                              (truncate-string-to-width (or body "") 60 nil nil "..."))
                                    (format "   %s %s%s (%s)\n"
                                            status-icon
                                            (if shipit-show-avatars
                                                (concat (shipit--create-avatar-display user avatar-url 16) " ")
                                              "")
                                            (propertize user 'font-lock-face 'shipit-username-face) status-text))))
                  (insert line-text)
                  (add-text-properties start (point)
                                       `(shipit-reviewers t
                                                          shipit-pr-number ,pr-number
                                                          shipit-repo ,repo
                                                          shipit-reviewer ,user)))))
          ;; Show requested reviewers (without status icons)
          (if requested-reviewers
              (dolist (reviewer requested-reviewers)
                (let* ((start (point))
                       (reviewer-name (if (stringp reviewer) reviewer (cdr (assq 'login reviewer))))
                       (avatar-url (when (not (stringp reviewer)) (cdr (assq 'avatar_url reviewer)))))
                  (insert (format "   ⏳ %s%s (requested)\n"
                                  (if shipit-show-avatars
                                      (concat (shipit--create-avatar-display reviewer-name avatar-url 16) " ")
                                    "")
                                  (propertize reviewer-name 'font-lock-face 'shipit-username-face)))
                  (add-text-properties start (point)
                                       `(shipit-reviewers t
                                                          shipit-pr-reviewers ,requested-reviewers
                                                          shipit-pr-number ,pr-number
                                                          shipit-repo ,repo
                                                          shipit-reviewer ,reviewer)))))
          ;; Show message if neither completed reviews nor requested reviewers
          (when (= total-count 0)
            (let ((start (point)))
              (insert "   No reviewers or reviews\n")
              (add-text-properties start (point)
                                   `(shipit-reviewers t
                                                      shipit-pr-reviewers nil
                                                      shipit-pr-number ,pr-number
                                                      shipit-repo ,repo))))
          ;; Always add a final newline to ensure proper section structure
          (insert "\n"))))))

;; svg-lib is optional — checked at runtime via (featurep 'svg-lib)

(defun shipit--svg-lib-tag-fixed (text fg-color bg-color)
  "Create an SVG pill tag for TEXT with FG-COLOR on BG-COLOR.
Uses svg-lib for rendering but fixes width and centering."
  (let* ((style (svg-lib-style (svg-lib-style-default--get)
                               :foreground fg-color
                               :background bg-color
                               :stroke 0 :radius 8
                               :padding 1 :height 0.9))
         (font-family (plist-get style :font-family))
         (font-size   (plist-get style :font-size))
         (font-weight (plist-get style :font-weight))
         (height      (plist-get style :height))
         (radius      (plist-get style :radius))
         (txt-char-height (window-font-height))
         (txt-char-height (if line-spacing (+ txt-char-height line-spacing)
                            txt-char-height))
         (font-info   (font-info (format "%s-%d" font-family font-size)))
         (ascent      (aref font-info 8))
         (tag-char-w  (aref font-info 11))
         (h-pad       (round (* tag-char-w 0.8)))
         (tag-width   (+ (* (length text) tag-char-w) (* 2 h-pad)))
         (tag-height  (round (* txt-char-height height)))
         (svg (svg-create tag-width tag-height)))
    (svg-rectangle svg 0 0 tag-width tag-height
                   :fill bg-color :rx radius)
    (svg-text svg text
              :font-family font-family :font-weight font-weight
              :font-size (aref font-info 2)
              :fill fg-color
              :x (/ tag-width 2.0) :y ascent
              :text-anchor "middle")
    (svg-image svg :ascent 'center)))

(defun shipit--format-label (label)
  "Format LABEL with proper styling."
  (let* ((name (string-trim (or (cdr (assq 'name label)) "")))
         (color (cdr (assq 'color label)))
         (bg-color (concat "#" color))
         (contrasting-color (shipit--get-contrasting-color bg-color)))
    (let ((label-face `(:background ,bg-color :foreground ,contrasting-color
                        :box (:line-width (-1 . -1) :color ,bg-color))))
      (if (and (display-graphic-p)
               (featurep 'svg-lib)
               (fboundp 'svg-lib-tag))
          (condition-case nil
              (let ((tag (shipit--svg-lib-tag-fixed
                          name contrasting-color bg-color)))
                (propertize name 'display tag 'shipit-label-face label-face))
            (error (propertize (format " %s " name) 'font-lock-face label-face
                               'shipit-label-face label-face)))
        (propertize (format " %s " name) 'font-lock-face label-face
                    'shipit-label-face label-face)))))


(defun shipit-debug-text-properties-at-point ()
  "Debug function to show all text properties at point."
  (interactive)
  (let ((props (text-properties-at (point))))
    (message "Text properties at point %d: %S" (point) props)
    (with-current-buffer (get-buffer-create "*shipit-debug-props*")
      (erase-buffer)
      (insert (format "Position: %d\n" (point)))
      (insert (format "Properties: %S\n" props))
      (insert (format "shipit-comment: %S\n" (get-text-property (point) 'shipit-comment)))
      (insert (format "shipit-labels: %S\n" (get-text-property (point) 'shipit-labels)))
      (insert (format "shipit-pr-labels: %S\n" (get-text-property (point) 'shipit-pr-labels)))
      (insert (format "shipit-pr-number: %S\n" (get-text-property (point) 'shipit-pr-number)))
      (display-buffer (current-buffer)))))

;; Debug keybinding is now handled by shipit-mode keymap

(defun shipit--insert-reviews-section (repo pr-number)
  "Insert reviews section showing detailed reviewer status."
  (let ((review-details (condition-case err
                            (shipit--get-pr-review-details repo pr-number)
                          (error
                           (message "Error fetching review details: %S" err)
                           nil))))
    (magit-insert-section (shipit-reviews nil t)
      (let ((header-start (point)))
        (magit-insert-heading (format "%s Reviews (%d)" (shipit--get-user-type-icon "team" "👥") (if review-details (length review-details) 0)))
        (add-text-properties header-start (point)
                             `(shipit-reviews t
                                              shipit-pr-number ,pr-number
                                              shipit-repo ,repo)))
      (magit-insert-section-body
        (if (null review-details)
            (let ((start (point)))
              (insert "   No reviews yet\n")
              (add-text-properties start (point)
                                   `(shipit-reviews t
                                                    shipit-pr-number ,pr-number
                                                    shipit-repo ,repo)))
          (dolist (review review-details)
            ;; Create individual magit section for each review
            (magit-insert-section (review review)
              (let* ((user (or (cdr (assq 'user review)) "Unknown"))
                     (status-icon (or (cdr (assq 'status-icon review)) ""))
                     (status-text (or (cdr (assq 'status-text review)) "unknown"))
                     (body (cdr (assq 'body review)))
                     (start (point))
                     (line-text (if body
                                    (format "    %s %s (%s) - \"%s\"\n"
                                            status-icon user status-text
                                            (truncate-string-to-width (or body "") 60 nil nil "..."))
                                  (format "    %s %s (%s)\n"
                                          status-icon user status-text))))
                (insert line-text)
                (add-text-properties start (1- (point))
                                     `(shipit-reviews t
                                                      shipit-pr-number ,pr-number
                                                      shipit-repo ,repo
                                                      shipit-reviewer ,user

                                                      help-echo "Individual review")))))
          (insert "\n"))))))

(defun shipit--insert-assignees-section (repo pr)
  "Insert assignees section showing current assignees with add/remove functionality."
  (let* ((assignees (cdr (assq 'assignees pr)))
         (assignee-count (if assignees (length assignees) 0)))
    (magit-insert-section (assignees nil t)
      (let ((heading-start (point)))
        (magit-insert-heading (format "%s Assignees (%d)" (shipit--get-user-type-icon "single" "👤") assignee-count))
        (add-text-properties heading-start (point)
                             `(shipit-assignees t
                                                shipit-pr-number ,(cdr (assq 'number pr))
                                                shipit-repo ,repo
                                                shipit-assignees-data ,assignees)))
      (magit-insert-section-body
        (if (= assignee-count 0)
            (let ((start (point)))
              (insert "   No assignees\n")
              (add-text-properties start (point)
                                   `(shipit-assignees t
                                                      shipit-pr-number ,(cdr (assq 'number pr))
                                                      shipit-repo ,repo
                                                      shipit-assignees-data nil)))
          (dolist (assignee assignees)
            ;; Create individual magit section for each assignee
            (magit-insert-section (assignee assignee)
              (let* ((user (cdr (assq 'login assignee)))
                     (avatar-url (cdr (assq 'avatar_url assignee)))
                     (start (point)))
                (insert (format "   %s%s\n"
                                (if shipit-show-avatars
                                    (concat (shipit--create-avatar-display user avatar-url 16) " ")
                                  "")
                                (propertize user 'font-lock-face 'shipit-username-face)))
                (add-text-properties start (1- (point))
                                     `(shipit-assignees t
                                                        shipit-pr-number ,(cdr (assq 'number pr))
                                                        shipit-repo ,repo
                                                        shipit-assignees-data ,assignees
                                                        shipit-assignee-user ,user

                                                        help-echo "Individual assignee"))))))
        (insert "\n")))))


;;; Placeholder Sections for Incremental Rendering
;;
;; These functions insert lightweight placeholder sections that display
;; immediately while data is being fetched. They create proper magit sections
;; that support TAB expand/collapse.

(defun shipit--insert-commits-section-placeholder (_repo _pr-number)
  "Insert a placeholder commits section while data loads.
REPO and PR-NUMBER are kept for API compatibility but unused."
  (magit-insert-section (pr-commits nil t)  ; hidden by default
    (magit-insert-heading
      (format "%s Commits (loading...)" (shipit--get-comment-type-icon "review" "📝")))
    (magit-insert-section-body
      (insert "   Fetching commits...\n")
      (insert "\n"))))

(defun shipit--insert-files-section-placeholder (_repo _pr-number)
  "Insert a placeholder files section while data loads.
REPO and PR-NUMBER are kept for API compatibility but unused."
  (magit-insert-section (pr-files nil t)  ; hidden by default
    (magit-insert-heading
      (format "%s Files Changed (loading...)" (shipit--get-files-icon "📄")))
    (magit-insert-section-body
      (insert "   Fetching files...\n")
      (insert "\n"))))

(defun shipit--insert-general-comments-section-placeholder (_repo _pr-number)
  "Insert a placeholder general comments section while data loads.
REPO and PR-NUMBER are kept for API compatibility but unused."
  (magit-insert-section (general-comments nil t)  ; hidden by default
    (magit-insert-heading
      (format "%s General Comments (loading...)" (shipit--get-comment-type-icon "comment" "💬")))
    (magit-insert-section-body
      (insert "   Fetching comments...\n")
      (insert "\n"))))

(defun shipit--insert-activity-section-placeholder (_repo _pr-number)
  "Insert a placeholder activity section while data loads.
REPO and PR-NUMBER are kept for API compatibility but unused."
  (when shipit-show-activity-timeline
    (magit-insert-section (pr-activity nil t)  ; hidden by default
      (magit-insert-heading
        (format "%s Activity (loading...)" (shipit--get-activity-icon "📋")))
      (magit-insert-section-body
        (insert "   Fetching activity...\n")
        (insert "\n")))))


(defun shipit--deduplicate-children (parent-section new-section)
  "Remove duplicate sections of the same type from PARENT-SECTION children.
Keeps NEW-SECTION and removes any other children with the same type.
This prevents stale sections from accumulating during async replacements."
  (when (and parent-section new-section)
    (let ((new-type (oref new-section type)))
      (oset parent-section children
            (seq-filter (lambda (s)
                          (or (eq s new-section)
                              (not (eq (oref s type) new-type))))
                        (oref parent-section children))))))

;; shipit--find-section-by-type and shipit--find-section-in-tree
;; are now in shipit-sections.el

(defun shipit--get-activity-event-id (event)
  "Extract unique identifier from EVENT for unread tracking.
Most events have an `id' field, but commit events use `sha' instead."
  (or (cdr (assq 'id event))
      (cdr (assq 'sha event))))

(defun shipit--establish-activity-baseline (repo pr-number events)
  "Establish baseline by marking all current activities as read if needed.
Marks all activities as read on first view, or when user just performed a mutation."
  (when (and shipit-show-unread-indicators events)
    (let* ((existing-read (shipit--get-read-activities repo pr-number))
           (should-mark-all (or (not existing-read)
                                shipit--user-mutated-pr)))
      (when should-mark-all
        ;; Mark all current activities as read
        (dolist (event events)
          (let ((event-id (shipit--get-activity-event-id event)))
            (when event-id
              (shipit--mark-activity-read repo pr-number event-id))))
        ;; Clear the mutation flag
        (setq shipit--user-mutated-pr nil)))))

(defun shipit--replace-activity-section-with-content (repo pr-data pr-number events)
  "Replace the activity placeholder section with actual content.
REPO is the repository, PR-DATA is the PR data, PR-NUMBER is the PR number,
EVENTS is the list of timeline events.
Deletes the placeholder and re-inserts a complete section with proper
magit section structure for nested activity-event subsections."
  (shipit--debug-log "ASYNC: Replacing activity placeholder with %d events" (length events))
  ;; Establish baseline on first view so only new activities show red dots
  (shipit--establish-activity-baseline repo pr-number events)
  (when shipit-show-activity-timeline
    (let ((section (shipit--find-section-by-type 'pr-activity)))
      (if section
          (let* ((inhibit-read-only t)
                 (section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
                 (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m)))
                 (parent-section (oref section parent))
                 ;; Find index of old section in parent's children for proper ordering
                 (children (and parent-section (oref parent-section children)))
                 (old-index (and children (seq-position children section #'eq))))
            (when (and section-start section-end)
              (save-excursion
                ;; Delete the entire placeholder section
                (delete-region section-start section-end)
                (goto-char section-start)
                ;; Re-insert complete section with proper magit structure
                ;; This ensures nested activity-event subsections have correct parent
                (let* ((magit-insert-section--parent parent-section)
                       new-section
                       ;; Inject synthetic events for new inline comments first
                       (inline-events (when (and (boundp 'shipit--new-inline-comments)
                                                 shipit--new-inline-comments)
                                        (shipit--debug-log "ACTIVITY: Injecting %d new inline comments as synthetic events"
                                                           (length shipit--new-inline-comments))
                                        (mapcar #'shipit--inline-comment-to-activity-event
                                                shipit--new-inline-comments)))
                       (all-events (append events inline-events))
                       (total-count (length all-events))
                       ;; Count unread activities for header indicator (including inline comments)
                       (read-activities (shipit--get-read-activities repo pr-number))
                       (unread-count (when read-activities
                                       (cl-count-if
                                        (lambda (event)
                                          (let ((event-id (shipit--get-activity-event-id event)))
                                            ;; Only count events with valid IDs that are not read
                                            (and event-id
                                                 (not (shipit--is-activity-read-p repo pr-number event-id)))))
                                        all-events)))
                       (unread-indicator (if (and unread-count (> unread-count 0))
                                             (propertize " ●" 'font-lock-face '(:foreground "red"))
                                           "")))
                  (setq new-section
                        (magit-insert-section (pr-activity nil t)  ; hidden by default
                          (magit-insert-heading
                            (format "%s Activity (%d)%s"
                                    (shipit--get-activity-icon "📋")
                                    total-count
                                    unread-indicator))
                          (magit-insert-section-body
                            (if (not all-events)
                                (insert "   No activity events found\n")
                              ;; Build mapping from Git author names to GitHub usernames
                              (let* ((author-mapping (shipit--build-author-username-mapping events pr-data repo pr-number))
                                     ;; Sort events by created_at timestamp
                                     ;; Note: commit events use author.date or committer.date instead of created_at
                                     (sorted-by-time (sort (copy-sequence all-events)
                                                           (lambda (a b)
                                                             (let ((time-a (or (cdr (assq 'created_at a))
                                                                               (cdr (assq 'submitted_at a))
                                                                               (let ((author (cdr (assq 'author a))))
                                                                                 (when author (cdr (assq 'date author))))
                                                                               (let ((committer (cdr (assq 'committer a))))
                                                                                 (when committer (cdr (assq 'date committer))))
                                                                               ""))
                                                                   (time-b (or (cdr (assq 'created_at b))
                                                                               (cdr (assq 'submitted_at b))
                                                                               (let ((author (cdr (assq 'author b))))
                                                                                 (when author (cdr (assq 'date author))))
                                                                               (let ((committer (cdr (assq 'committer b))))
                                                                                 (when committer (cdr (assq 'date committer))))
                                                                               "")))
                                                               (string< time-a time-b)))))
                                     ;; Apply order preference
                                     (sorted-events (if (eq shipit-activity-timeline-order 'reverse-chronological)
                                                        (reverse sorted-by-time)
                                                      sorted-by-time)))
                                (dolist (event sorted-events)
                                  (shipit--insert-activity-event event repo pr-number author-mapping))))
                            (insert "\n"))))
                  ;; Fix parent's children list to maintain correct order
                  ;; magit-insert-section appends to end, but we need to replace at old position
                  (when (and parent-section new-section)
                    (let* ((current-children (oref parent-section children))
                           (cleaned current-children)
                           (with-new (if (memq new-section cleaned) cleaned (cons new-section cleaned)))
                           (sorted (sort (copy-sequence with-new)
                                         (lambda (a b)
                                           (< (oref a start) (oref b start))))))
                      (oset parent-section children sorted)
                      (shipit--deduplicate-children parent-section new-section)
                      (shipit--debug-log "ASYNC: Fixed children order by position")))))
              ;; Calculate which sections have unread activities and merge with existing
              ;; (e.g., inline comments detection may have already added pr-files)
              (let ((activity-sections (shipit--get-sections-with-unread-activities repo pr-number events)))
                (dolist (section activity-sections)
                  (unless (memq section shipit--sections-with-unread)
                    (push section shipit--sections-with-unread))))
              (shipit--debug-log "ASYNC: Sections with unread: %S" shipit--sections-with-unread)
              (shipit--update-section-unread-indicators)
              (shipit--debug-log "ASYNC: Activity section replaced successfully")))
        (shipit--debug-log "ASYNC: Could not find activity section to replace")))))

(defun shipit--replace-general-comments-section-with-content (repo pr-number comments)
  "Replace the general comments placeholder section with actual content.
REPO is the repository, PR-NUMBER is the PR number, COMMENTS is the list of comments."
  (shipit--debug-log "ASYNC: Replacing general comments placeholder with %d comments" (length comments))
  (let ((section (shipit--find-section-by-type 'general-comments)))
    (if section
        (let* ((inhibit-read-only t)
               (section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
               (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m)))
               (parent-section (oref section parent))
               ;; Find index of old section in parent's children for proper ordering
               (children (and parent-section (oref parent-section children)))
               (old-index (and children (seq-position children section #'eq))))
          (when (and section-start section-end)
            (save-excursion
              ;; Delete the entire placeholder section
              (delete-region section-start section-end)
              (goto-char section-start)
              ;; Re-insert complete section with proper magit structure
              (let ((magit-insert-section--parent parent-section)
                    new-section)
                (setq new-section
                      (magit-insert-section (general-comments nil t)  ; hidden by default
                        (let ((header-start (point)))
                          (magit-insert-heading
                            (format "%s General Comments (%d)"
                                    (shipit--get-comment-type-icon "comment" "💬")
                                    (length comments)))
                          (add-text-properties header-start (point)
                                               `(shipit-general-comments t
                                                                         shipit-pr-number ,pr-number
                                                                         shipit-repo ,repo)))
                        (magit-insert-section-body
                          (if (or (not comments) (= (length comments) 0))
                              (insert "   No general comments\n")
                            ;; Use hierarchical comment rendering with quote-based threading
                            (let* ((inline-comment-ids (when (and (boundp 'shipit--cached-inline-comments)
                                                                  shipit--cached-inline-comments)
                                                         (mapcar (lambda (c) (cdr (assq 'id c)))
                                                                 shipit--cached-inline-comments)))
                                   (deduplicated-comments
                                    (if inline-comment-ids
                                        (cl-remove-if (lambda (comment)
                                                        (member (cdr (assq 'id comment)) inline-comment-ids))
                                                      comments)
                                      comments))
                                   (threads (shipit--group-comments-by-api-replies deduplicated-comments))
                                   (root-comments (gethash 'root threads)))
                              ;; Cache comments AFTER threading analysis (includes reply-depth)
                              (setq shipit--cached-general-comments deduplicated-comments)
                              (setq shipit--general-comments-fetched t)
                              (shipit--debug-log "ASYNC-COMMENTS: total=%d deduplicated=%d root=%d cached=%d"
                                                 (length comments)
                                                 (length deduplicated-comments)
                                                 (length root-comments)
                                                 (length shipit--cached-general-comments))
                              (let ((inserted-count 0))
                                (dolist (comment root-comments)
                                  (condition-case err
                                      (progn
                                        (shipit--insert-status-hierarchical-comment
                                         comment threads nil nil 0 nil nil repo pr-number)
                                        (setq inserted-count (1+ inserted-count)))
                                    (error
                                     (shipit--debug-log "ASYNC-COMMENTS-ERROR: Failed on comment %d: %s"
                                                        (cdr (assq 'id comment))
                                                        (error-message-string err)))))
                                (shipit--debug-log "ASYNC-COMMENTS: inserted %d comments" inserted-count)))))))
                ;; Fix parent's children by position-sorting (async-safe)
                (when (and parent-section new-section)
                  (let* ((current-children (oref parent-section children))
                         (cleaned current-children)
                         (with-new (if (memq new-section cleaned) cleaned (cons new-section cleaned)))
                         (sorted (sort (copy-sequence with-new)
                                       (lambda (a b)
                                         (< (oref a start) (oref b start))))))
                    (oset parent-section children sorted)
                    (shipit--deduplicate-children parent-section new-section)
                    (shipit--debug-log "ASYNC: Fixed children order for comments by position")))
                ;; Store imenu metadata for children so they appear even when collapsed
                ;; Since section is collapsed, children aren't accessible via slot.
                ;; Store username from API data - goto function will search for it.
                (when (and new-section comments (> (length comments) 0))
                  (let ((child-names
                         (mapcar (lambda (comment)
                                   (let* ((user (cdr (assq 'user comment)))
                                          (login (if user (cdr (assq 'login user)) "unknown")))
                                     login))
                                 comments)))
                    (shipit--debug-log "IMENU-ASYNC-COMMENTS: storing %d usernames" (length child-names))
                    (shipit--imenu-store-children (oref new-section start) child-names 'general-comments)))
                ;; Re-apply unread indicator based on actual unread comments (from baseline)
                (when new-section
                  (let ((has-unread-comments
                         (cl-some (lambda (comment)
                                    (let ((comment-id (cdr (assq 'id comment))))
                                      (shipit--is-comment-unread-p repo pr-number comment-id)))
                                  comments)))
                    (shipit--debug-log "ASYNC: General comments has-unread=%s" has-unread-comments)
                    (when has-unread-comments
                      (shipit--update-section-header-indicator new-section t))))))
            (shipit--debug-log "ASYNC: General comments section replaced successfully")))
      (shipit--debug-log "ASYNC: Could not find general comments section to replace"))))

(defvar-local shipit--file-viewed-states nil
  "Alist of (filename . state) for file viewed tracking.")

(defun shipit--fetch-file-viewed-states-for-repo (repo pr-number)
  "Fetch file viewed states for PR-NUMBER in REPO via backend dispatch."
  (condition-case nil
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (fn (plist-get backend :fetch-file-viewed-states)))
        (when fn (funcall fn config pr-number)))
    (error nil)))

(defun shipit--fetch-file-viewed-states-async (repo pr-number buffer)
  "Fetch file viewed states asynchronously for PR-NUMBER in REPO.
Stores results in BUFFER's local `shipit--file-viewed-states'.
Backend may use a true async path (callback-based) or fall back to
deferring a sync call via timer when the backend has no async support."
  (let* ((resolved (condition-case nil (shipit-pr--resolve-for-repo repo) (error nil)))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :fetch-file-viewed-states)))
    (when fn
      (shipit--debug-log "ASYNC: Starting viewed-states fetch for PR %s" pr-number)
      (let ((handler (lambda (states)
                       (when (and states (buffer-live-p buffer))
                         (with-current-buffer buffer
                           (setq-local shipit--file-viewed-states states))
                         (shipit--debug-log "ASYNC: Viewed states fetched, %d files" (length states))))))
        (condition-case err
            (funcall fn config pr-number handler)
          (wrong-number-of-arguments
           ;; Backend doesn't support callback yet — fall back to deferred sync.
           (shipit--debug-log "ASYNC: backend lacks callback, deferring sync (%s)" err)
           (run-at-time 0 nil
                        (lambda ()
                          (condition-case nil
                              (funcall handler (funcall fn config pr-number))
                            (error nil))))))))))

(defun shipit--file-viewed-p (filename)
  "Return non-nil if FILENAME is marked as viewed."
  (equal "VIEWED" (cdr (assoc filename shipit--file-viewed-states))))

(defun shipit--insert-pr-file-section (file repo pr-number &optional pr-data pr-head-sha)
  "Insert a single pr-file magit section for FILE.
FILE is an alist with keys: filename, status, additions, deletions, patch.
REPO and PR-NUMBER provide context.
PR-DATA and PR-HEAD-SHA are optional metadata to attach to the section."
  (let* ((filename (cdr (assq 'filename file)))
         (status (cdr (assq 'status file)))
         (additions (cdr (assq 'additions file)))
         (deletions (cdr (assq 'deletions file)))
         (patch (cdr (assq 'patch file)))
         (comment-count (shipit--get-file-comment-count pr-number filename))
         (outdated-count (shipit--get-file-outdated-comment-count pr-number filename))
         (resolved-count (shipit--get-file-resolved-comment-count pr-number filename))
         (comment-indicator (shipit--get-comment-indicator comment-count outdated-count resolved-count))
         (status-char (cond ((equal status "added") "A")
                            ((equal status "deleted") "D")
                            ((equal status "modified") "M")
                            ((equal status "renamed") "R")
                            (t "?")))
         ;; Text properties to apply - defined once, used in heading and body
         (file-props `(shipit-pr-file t
                                      shipit-file-path ,filename
                                      shipit-file-status ,status
                                      shipit-repo ,repo
                                      shipit-pr-number ,pr-number
                                      shipit-pr-head-sha ,pr-head-sha
                                      shipit-pr-data ,pr-data
                                      shipit-comment-count ,comment-count

                                      help-echo "RET/SPC: open, e: ediff, f: filter, TAB: toggle, M-;: dwim")))
    (magit-insert-section (pr-file filename t)
      ;; Apply properties to heading
      (let ((heading-start (point))
            (adds (or additions 0))
            (dels (or deletions 0)))
        (let ((viewed (shipit--file-viewed-p filename))
              (viewed-icon (if (shipit--file-viewed-p filename)
                               (propertize "✓" 'font-lock-face 'success)
                             " ")))
          (magit-insert-heading
            (format " %s %s %s%s (%s %s)"
                    viewed-icon
                    (propertize status-char 'font-lock-face 'magit-diff-file-heading)
                    (propertize filename 'font-lock-face (if viewed 'shadow 'shipit-filename-face))
                    comment-indicator
                    (if (> adds 0)
                        (propertize (format "+%d" adds) 'font-lock-face 'diff-added)
                      "+0")
                    (if (> dels 0)
                        (propertize (format "-%d" dels) 'font-lock-face 'diff-removed)
                      "-0"))))
        (add-text-properties heading-start (point) file-props))
      ;; Body is lazily rendered - apply properties when it's actually inserted
      (magit-insert-section-body
        (let ((body-start (point)))
          (shipit--insert-file-diff patch filename comment-count repo pr-number)
          (add-text-properties body-start (point) file-props))))))

(defun shipit--replace-files-section-with-content (repo pr-data pr-number files &optional truncated)
  "Replace the files placeholder section with actual content.
REPO is the repository, PR-DATA is the PR data, PR-NUMBER is the PR number,
FILES is the list of changed files.  If TRUNCATED is non-nil, indicate
that more files exist beyond the fetched limit."
  (shipit--debug-log "ASYNC: Replacing files placeholder with %d files (truncated=%s)"
                     (length files) truncated)
  (let ((section (shipit--find-section-by-type 'pr-files)))
    (if section
        (let* ((inhibit-read-only t)
               (section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
               (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m)))
               (parent-section (oref section parent))
               (children (and parent-section (oref parent-section children)))
               (old-index (and children (seq-position children section #'eq))))
          (when (and section-start section-end)
            ;; Remove old section from children BEFORE deletion
            ;; to prevent corrupted markers from interfering
            (when parent-section
              (oset parent-section children
                    (seq-remove (lambda (s) (eq s section))
                                (oref parent-section children))))
            (save-excursion
              (delete-region section-start section-end)
              (goto-char section-start)
              (let ((magit-insert-section--parent parent-section)
                    (files-section nil))
                (setq files-section
                      (magit-insert-section (pr-files nil t)
                        (let* ((total-count (length files))
                               (count-display (if truncated
                                                  (format "%d+" total-count)
                                                (number-to-string total-count)))
                               (total-adds (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'additions f)) 0)) files)))
                               (total-dels (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'deletions f)) 0)) files)))
                               (header-start (point)))
                          (magit-insert-heading
                            (format "%s Files Changed (%s, %s %s)"
                                    (shipit--get-files-icon "📄")
                                    count-display
                                    (if (> total-adds 0)
                                        (propertize (format "+%d" total-adds) 'font-lock-face 'diff-added)
                                      "+0")
                                    (if (> total-dels 0)
                                        (propertize (format "-%d" total-dels) 'font-lock-face 'diff-removed)
                                      "-0")))
                          (add-text-properties header-start (point)
                                               `(shipit-pr-files t
                                                                 shipit-pr-number ,pr-number
                                                                 shipit-repo ,repo)))
                        (magit-insert-section-body
                          (if (= (length files) 0)
                              (insert "   No files changed\n")
                            (dolist (file files)
                              (shipit--insert-pr-file-section file repo pr-number pr-data)))
                          (insert "\n"))))
                ;; Fix parent's children by position-sorting (async-safe)
                (when (and parent-section files-section)
                  (let* ((current-children (oref parent-section children))
                         (cleaned current-children)
                         (with-new (if (memq files-section cleaned) cleaned (cons files-section cleaned)))
                         (sorted (sort (copy-sequence with-new)
                                       (lambda (a b)
                                         (< (oref a start) (oref b start))))))
                    (oset parent-section children sorted)))
                    (shipit--deduplicate-children parent-section files-section)
                ;; Store imenu metadata for children so they appear even when collapsed
                (when files-section
                  (let ((child-names
                         (mapcar (lambda (file)
                                   (cdr (assq 'filename file)))
                                 files)))
                    (shipit--imenu-store-children (oref files-section start) child-names 'pr-files)))
                ;; Hide the section by default
                (when files-section
                  (magit-section-hide files-section))))
            (shipit--debug-log "ASYNC: Files section replaced successfully")))
      (shipit--debug-log "ASYNC: Could not find files section to replace"))))

(defun shipit--replace-commits-section-with-content (repo pr-number commits)
  "Replace the commits placeholder section with actual content.
REPO is the repository, PR-NUMBER is the PR number, COMMITS is the list of commits."
  (shipit--debug-log "ASYNC: Replacing commits placeholder with %d commits" (length commits))
  (let ((section (shipit--find-section-by-type 'pr-commits)))
    (if section
        (let* ((inhibit-read-only t)
               (section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
               (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m)))
               (parent-section (oref section parent))
               (children (and parent-section (oref parent-section children)))
               (old-index (and children (seq-position children section #'eq))))
          (when (and section-start section-end)
            ;; Remove old section from children BEFORE deletion
            ;; to prevent corrupted markers from interfering
            (when parent-section
              (oset parent-section children
                    (seq-remove (lambda (s) (eq s section))
                                (oref parent-section children))))
            (save-excursion
              (delete-region section-start section-end)
              (goto-char section-start)
              (let ((magit-insert-section--parent parent-section)
                    (commits-section nil))
                (setq commits-section
                      (magit-insert-section (pr-commits nil t)  ; hidden by default
                        (magit-insert-heading
                          (format "%s Commits (%d)"
                                  (shipit--get-comment-type-icon "review" "📝")
                                  (length commits)))
                        (magit-insert-section-body
                          (if (and (sequencep commits) (> (length commits) 0))
                              (let ((processed-commits
                                     (mapcar (lambda (commit)
                                               (shipit--process-single-commit-data commit))
                                             commits)))
                                (dolist (commit-data (reverse processed-commits))
                                  (shipit--insert-processed-commit-section commit-data repo pr-number)))
                            (insert "   No commits found\n")))))
                ;; Fix parent's children list to maintain correct order
                (when (and parent-section commits-section)
                  (let* ((current-children (oref parent-section children))
                         (cleaned current-children)
                         (with-new (if (memq commits-section cleaned) cleaned (cons commits-section cleaned)))
                         (sorted (sort (copy-sequence with-new)
                                       (lambda (a b)
                                         (< (oref a start) (oref b start))))))
                    (oset parent-section children sorted)))
                    (shipit--deduplicate-children parent-section commits-section)
                ;; Store imenu metadata for children so they appear even when collapsed
                (when commits-section
                  (let ((child-names
                         (mapcar (lambda (commit)
                                   (let* ((sha (cdr (assq 'sha commit)))
                                          (commit-data (cdr (assq 'commit commit)))
                                          (message (cdr (assq 'message commit-data)))
                                          (first-line (if message
                                                          (car (split-string message "\n" t))
                                                        "No message"))
                                          (short-sha (if sha (substring sha 0 7) "unknown")))
                                     (format "%s %s" short-sha first-line)))
                                 commits)))
                    (shipit--imenu-store-children (oref commits-section start) child-names 'pr-commits)))
                ;; Hide the section by default
                (when commits-section
                  (magit-section-hide commits-section))))
            (shipit--debug-log "ASYNC: Commits section replaced successfully")))
      (shipit--debug-log "ASYNC: Could not find commits section to replace"))))

(defun shipit--replace-approval-section-with-content (repo pr-number review-info)
  "Replace the approval placeholder section with actual content.
REPO is the repository, PR-NUMBER is the PR number, REVIEW-INFO is the
review decision data alist from shipit--fetch-review-decision-async."
  (shipit--debug-log "ASYNC: Replacing approval placeholder")
  (condition-case err
      (let ((section (shipit--find-section-by-type 'approval)))
        (if section
            (let* ((inhibit-read-only t)
                   (section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
                   (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m)))
                   (parent-section (oref section parent))
                   (children (and parent-section (oref parent-section children)))
                   (old-index (and children (seq-position children section #'eq))))
              ;; Validate section positions before proceeding
              (when (and section-start section-end
                         (number-or-marker-p section-start)
                         (number-or-marker-p section-end)
                         (<= section-start section-end)
                         (<= section-end (point-max)))
                (save-excursion
                  (delete-region section-start section-end)
                  (goto-char section-start)
                  (let* ((magit-insert-section--parent parent-section)
                         (review-decision-status (cdr (assq 'status-text review-info)))
                         (completed-reviews (cdr (assq 'completed-reviews review-info)))
                         (approved-reviews (when completed-reviews
                                             (seq-filter (lambda (review)
                                                           (string= (cdr (assq 'state review)) "APPROVED"))
                                                         completed-reviews)))
                         (pending-users (cdr (assq 'pending-users review-info)))
                         (pending-teams (cdr (assq 'pending-teams review-info)))
                         (latest-reviews (cdr (assq 'latest-reviews review-info)))
                         (approval-section nil))
                    (setq approval-section
                          (magit-insert-section (approval nil t)
                            (let ((heading-start (point)))
                              (magit-insert-heading
                                (format "%s Approval:  %s"
                                        (shipit--get-pr-field-icon "approval" "❓")
                                        (or review-decision-status "Unknown")))
                              (add-text-properties heading-start (point)
                                                   `(shipit-pr-number ,pr-number
                                                                      shipit-repo ,repo)))
                            (magit-insert-section-body
                              ;; Approved by section with avatars and comments
                              (when (and approved-reviews (> (length approved-reviews) 0))
                                (insert (format "   %s Approved by:\n" (shipit--get-approval-status-icon "APPROVED" "✅")))
                                (dolist (review approved-reviews)
                                  (let* ((user-obj (or (cdr (assq 'author review)) (cdr (assq 'user review))))
                                         (user (if (stringp user-obj) user-obj (cdr (assq 'login user-obj))))
                                         (avatar-url (when (and user-obj (not (stringp user-obj)))
                                                       (cdr (assq 'avatar_url user-obj))))
                                         (raw-body (cdr (assq 'body review)))
                                         (cleaned-body (when raw-body (shipit--clean-text raw-body)))
                                         ;; Collapse multi-line comments to single line for display
                                         (single-line-body (when cleaned-body
                                                             (replace-regexp-in-string "[\n\r]+" " " cleaned-body)))
                                         (comment-text (if (and single-line-body (not (string-empty-p single-line-body)))
                                                           (format " - \"%s\"" (truncate-string-to-width single-line-body 50 nil nil "..."))
                                                         "")))
                                    (insert (format "      %s%s%s\n"
                                                    (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                                             (fboundp 'shipit--create-avatar-display))
                                                        (concat (shipit--create-avatar-display user avatar-url 16) " ")
                                                      (concat (shipit--get-user-type-icon "single" "🧑") " "))
                                                    (propertize user 'font-lock-face 'shipit-username-face)
                                                    comment-text)))))

                              ;; Requested teams section
                              (when (and pending-teams (> (length pending-teams) 0))
                                (insert "   👥 Requested teams:\n")
                                (dolist (team-info pending-teams)
                                  (let ((team-slug (cdr (assq 'slug team-info))))
                                    (insert (format "      @%s\n" team-slug)))))

                              ;; Requested individual reviewers with avatars
                              (when (and pending-users (> (length pending-users) 0))
                                (insert (format "   %s Requested reviewers:\n" (shipit--get-status-icon "waiting" "⏳")))
                                (dolist (user-info pending-users)
                                  (let* ((user (cdr (assq 'login user-info)))
                                         (avatar-url (cdr (assq 'avatar_url user-info))))
                                    (insert (format "      %s%s\n"
                                                    (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                                             (fboundp 'shipit--create-avatar-display))
                                                        (concat (shipit--create-avatar-display user avatar-url 16) " ")
                                                      (concat (shipit--get-user-type-icon "single" "🧑") " "))
                                                    (propertize user 'font-lock-face 'shipit-username-face))))))

                              ;; Show who requested changes with details
                              (when (string-match-p "Changes Requested" (or review-decision-status ""))
                                (let ((changes-requested-reviews
                                       (seq-filter (lambda (review)
                                                     (string= (cdr (assq 'state review)) "CHANGES_REQUESTED"))
                                                   (or latest-reviews '()))))
                                  (if changes-requested-reviews
                                      (progn
                                        (insert (format "   %s Changes requested by:\n" (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌")))
                                        (dolist (review changes-requested-reviews)
                                          (let* ((author-obj (cdr (assq 'author review)))
                                                 (user (if (stringp author-obj) author-obj (cdr (assq 'login author-obj))))
                                                 (raw-body (cdr (assq 'body review)))
                                                 (cleaned-body (when raw-body (shipit--clean-text raw-body)))
                                                 ;; Collapse multi-line comments to single line for display
                                                 (single-line-body (when cleaned-body
                                                                     (replace-regexp-in-string "[\n\r]+" " " cleaned-body))))
                                            (insert (format "      %s%s"
                                                            (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                                                     (fboundp 'shipit--create-avatar-display))
                                                                (concat (shipit--create-avatar-display user nil 16) " ")
                                                              (concat (shipit--get-user-type-icon "single" "🧑") " "))
                                                            (propertize user 'font-lock-face 'shipit-username-face)))
                                            (when (and single-line-body (not (string-empty-p single-line-body)))
                                              (insert (format " - %s"
                                                              (truncate-string-to-width single-line-body 60 nil nil "..."))))
                                            (insert "\n"))))
                                    (insert (concat "   " (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌") " Changes requested (details not available)\n")))))
                              (insert "\n"))))

                    ;; Fix parent's children list to maintain correct order
                    (when (and parent-section approval-section)
                      (let* ((current-children (oref parent-section children))
                             (cleaned current-children)
                             (with-new (if (memq approval-section cleaned) cleaned (cons approval-section cleaned)))
                             (sorted (sort (copy-sequence with-new)
                                           (lambda (a b)
                                             (< (oref a start) (oref b start))))))
                        (oset parent-section children sorted)))
                    (shipit--deduplicate-children parent-section approval-section)
                    ;; Hide section by default (collapsed)
                    (when approval-section
                      (magit-section-hide approval-section))))
                (shipit--debug-log "ASYNC: Approval section replaced successfully")))
          (shipit--debug-log "ASYNC: Could not find approval section to replace")))
    (error
     (shipit--debug-log "ASYNC: Error replacing approval section: %s" (error-message-string err)))))

(defun shipit--get-workflow-names-for-checks (checks)
  "Get sorted workflow names for a list of CHECKS.
Returns a list of workflow name strings."
  (let ((workflow-names (make-hash-table :test 'equal)))
    (dolist (check checks)
      (let ((workflow-name (or (cdr (assq 'workflow_name check))
                               (cdr (assq 'name check))
                               "Unknown")))
        (puthash workflow-name t workflow-names)))
    (let ((names '()))
      (maphash (lambda (k _v) (push k names)) workflow-names)
      (sort names (lambda (a b) (string< (downcase a) (downcase b)))))))

(defun shipit--get-checks-group-names (checks)
  "Get the group names that will be shown in imenu for CHECKS.
Returns a list of group heading strings based on which check categories have items."
  (let ((failing-checks '())
        (in-progress-checks '())
        (pending-checks '())
        (cancelled-checks '())
        (skipped-checks '())
        (successful-checks '())
        (result '()))
    ;; Categorize checks (same logic as shipit--render-checks-section-content)
    (dolist (check checks)
      (let ((status (cdr (assq 'status check)))
            (conclusion (cdr (assq 'conclusion check))))
        (cond
         ((string= conclusion "success") (push check successful-checks))
         ((or (string= conclusion "failure") (string= conclusion "timed_out")) (push check failing-checks))
         ((string= conclusion "cancelled") (push check cancelled-checks))
         ((or (string= conclusion "skipped") (string= conclusion "neutral")) (push check skipped-checks))
         ((string= status "in_progress") (push check in-progress-checks))
         ((or (string= status "queued") (string= status "pending")) (push check pending-checks))
         ((string= status "cancelled") (push check cancelled-checks))
         (t (push check failing-checks)))))
    ;; Build list of group names in display order
    (when in-progress-checks
      (push (format "In Progress (%d)" (length in-progress-checks)) result))
    (when pending-checks
      (push (format "Pending (%d)" (length pending-checks)) result))
    (when cancelled-checks
      (push (format "Cancelled (%d)" (length cancelled-checks)) result))
    (when failing-checks
      (push (format "Failing checks (%d)" (length failing-checks)) result))
    (when skipped-checks
      (push (format "Skipped checks (%d)" (length skipped-checks)) result))
    (when successful-checks
      (push (format "Successful checks (%d)" (length successful-checks)) result))
    (nreverse result)))

(defun shipit--get-checks-group-data (checks)
  "Get categorized check data for imenu storage.
Returns an alist of (group-type . checks-list) for each non-empty category."
  (let ((failing-checks '())
        (in-progress-checks '())
        (pending-checks '())
        (cancelled-checks '())
        (skipped-checks '())
        (successful-checks '()))
    ;; Categorize checks
    (dolist (check checks)
      (let ((status (cdr (assq 'status check)))
            (conclusion (cdr (assq 'conclusion check))))
        (cond
         ((string= conclusion "success") (push check successful-checks))
         ((or (string= conclusion "failure") (string= conclusion "timed_out")) (push check failing-checks))
         ((string= conclusion "cancelled") (push check cancelled-checks))
         ((or (string= conclusion "skipped") (string= conclusion "neutral")) (push check skipped-checks))
         ((string= status "in_progress") (push check in-progress-checks))
         ((or (string= status "queued") (string= status "pending")) (push check pending-checks))
         ((string= status "cancelled") (push check cancelled-checks))
         (t (push check failing-checks)))))
    ;; Return alist of non-empty groups
    (let ((result '()))
      (when in-progress-checks (push (cons 'in-progress-checks in-progress-checks) result))
      (when pending-checks (push (cons 'pending-checks pending-checks) result))
      (when cancelled-checks (push (cons 'cancelled-checks cancelled-checks) result))
      (when failing-checks (push (cons 'failing-checks failing-checks) result))
      (when skipped-checks (push (cons 'skipped-checks skipped-checks) result))
      (when successful-checks (push (cons 'successful-checks successful-checks) result))
      result)))

(defun shipit--replace-checks-section-with-content (repo pr-number checks)
  "Replace the checks placeholder section with actual content.
REPO is the repository, PR-NUMBER is the PR number, CHECKS is the list of check runs
or a symbol like \\='closed for closed PRs or nil for disabled/error."
  (shipit--debug-log "ASYNC: Replacing checks placeholder with %s"
                     (cond ((eq checks 'closed) "closed PR")
                           ((null checks) "disabled/nil")
                           (t (format "%d checks" (length checks)))))
  (condition-case err
      (let ((section (shipit--find-section-by-type 'checks)))
        (if section
            (let* ((inhibit-read-only t)
                   (section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
                   (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m)))
                   (parent-section (oref section parent))
                   (children (and parent-section (oref parent-section children)))
                   (old-index (and children (seq-position children section #'eq))))
              (when (and section-start section-end
                         (number-or-marker-p section-start)
                         (number-or-marker-p section-end)
                         (<= section-start section-end)
                         (<= section-end (point-max)))
                (save-excursion
                  (delete-region section-start section-end)
                  (goto-char section-start)
                  (let ((magit-insert-section--parent parent-section)
                        (checks-section nil))
                    (setq checks-section
                          (cond
                           ;; Checks disabled
                           ((null checks)
                            (magit-insert-section (checks nil t)
                              (magit-insert-heading
                                (format "%s Checks (disabled)" (shipit--get-pr-field-icon "checks" "⚪")))
                              (magit-insert-section-body
                                (insert "   PR checks display is disabled.\n"))))

                           ;; Closed PR
                           ((eq checks 'closed)
                            (magit-insert-section (checks nil t)
                              (magit-insert-heading
                                (format "%s Checks (skipped for closed PR)"
                                        (shipit--get-pr-field-icon "checks" "🧪")))
                              (magit-insert-section-body
                                (insert "\n"))))

                           ;; Empty checks list
                           ((= (length checks) 0)
                            (magit-insert-section (checks nil t)
                              (magit-insert-heading
                                (format "%s Checks (0)" (shipit--get-pr-field-icon "checks" "🧪")))
                              (magit-insert-section-body
                                (insert "   No checks found\n"))))

                           ;; Render actual checks
                           (t
                            (shipit--render-checks-section-content repo pr-number checks))))

                    ;; Fix parent's children list to maintain correct order
                    (when (and parent-section checks-section)
                      (let* ((current-children (oref parent-section children))
                             (cleaned current-children)
                             (with-new (if (memq checks-section cleaned) cleaned (cons checks-section cleaned)))
                             (sorted (sort (copy-sequence with-new)
                                           (lambda (a b)
                                             (< (oref a start) (oref b start))))))
                        (oset parent-section children sorted)))
                        (shipit--deduplicate-children parent-section checks-section)
                    ;; Hide section by default (collapsed)
                    (when checks-section
                      (magit-section-hide checks-section))
                    ;; Store imenu metadata for children so they appear even when collapsed
                    (when (and checks-section (listp checks) (> (length checks) 0))
                      ;; Store status group names for the checks section
                      (let ((child-names (shipit--get-checks-group-names checks)))
                        (when child-names
                          (shipit--imenu-store-children (oref checks-section start) child-names 'checks)))
                      ;; Store workflow names for each status group
                      (let ((group-data (shipit--get-checks-group-data checks)))
                        (dolist (group group-data)
                          (let* ((group-type (car group))
                                 (group-checks (cdr group))
                                 (workflow-names (shipit--get-workflow-names-for-checks group-checks)))
                            (when workflow-names
                              (shipit--imenu-store-children (oref checks-section start) workflow-names group-type))))))))
                (shipit--debug-log "ASYNC: Checks section replaced successfully")))
          (shipit--debug-log "ASYNC: Could not find checks section to replace")))
    (error
     (shipit--debug-log "ASYNC: Error replacing checks section: %s" (error-message-string err)))))

(defun shipit--render-checks-section-content (repo pr-number checks)
  "Render the checks section content with CHECKS grouped by status.
Returns the created magit section."
  (let* ((failing-checks '())
         (in-progress-checks '())
         (pending-checks '())
         (cancelled-checks '())
         (skipped-checks '())
         (successful-checks '())
         (status-emoji (shipit--get-checks-status-emoji checks)))

    ;; Categorize checks
    (dolist (check checks)
      (let ((status (cdr (assq 'status check)))
            (conclusion (cdr (assq 'conclusion check))))
        (cond
         ((string= conclusion "success") (push check successful-checks))
         ((or (string= conclusion "failure") (string= conclusion "timed_out")) (push check failing-checks))
         ((string= conclusion "cancelled") (push check cancelled-checks))
         ((or (string= conclusion "skipped") (string= conclusion "neutral")) (push check skipped-checks))
         ((string= status "in_progress") (push check in-progress-checks))
         ((or (string= status "queued") (string= status "pending")) (push check pending-checks))
         ((string= status "cancelled") (push check cancelled-checks))
         (t (push check failing-checks)))))  ; Unknown goes to failing for visibility

    (magit-insert-section (checks nil t)
      (magit-insert-heading
        (format "%s Checks (%d)" (shipit--get-pr-field-icon "checks" status-emoji) (length checks)))
      (magit-insert-section-body
        ;; In-progress checks first
        (when in-progress-checks
          (magit-insert-section (in-progress-checks)
            (magit-insert-heading
              (format "   %s In Progress (%d)" (shipit--get-check-status-icon "in_progress" nil "🟡") (length in-progress-checks)))
            (shipit--insert-check-list in-progress-checks)))

        ;; Pending checks
        (when pending-checks
          (magit-insert-section (pending-checks)
            (magit-insert-heading
              (format "   %s Pending (%d)" (shipit--get-check-status-icon "queued" nil "🔵") (length pending-checks)))
            (shipit--insert-check-list pending-checks)))

        ;; Cancelled checks
        (when cancelled-checks
          (magit-insert-section (cancelled-checks)
            (magit-insert-heading
              (format "   %s Cancelled (%d)" (shipit--get-check-status-icon nil "cancelled" "⚪") (length cancelled-checks)))
            (shipit--insert-check-list cancelled-checks)))

        ;; Failing checks
        (when failing-checks
          (magit-insert-section (failing-checks)
            (magit-insert-heading
              (format "   %s Failing checks (%d)" (shipit--get-check-status-icon nil "failure" "❌") (length failing-checks)))
            (shipit--insert-check-list failing-checks)))

        ;; Skipped checks
        (when skipped-checks
          (magit-insert-section (skipped-checks)
            (magit-insert-heading
              (format "   %s Skipped checks (%d)" (shipit--get-check-status-icon nil "skipped" "⚪") (length skipped-checks)))
            (shipit--insert-check-list skipped-checks)))

        ;; Successful checks
        (when successful-checks
          (magit-insert-section (successful-checks)
            (magit-insert-heading
              (format "   %s Successful checks (%d)" (shipit--get-check-status-icon nil "success" "✅") (length successful-checks)))
            (shipit--insert-check-list successful-checks)))

        (insert "\n")))))

(defun shipit--render-commits-section (processed-commits &optional repo pr-number store-imenu)
  "Render a commits section with PROCESSED-COMMITS.
PROCESSED-COMMITS is a list of plists (from `shipit--process-single-commit-data').
REPO and PR-NUMBER are optional context for interactive features.
If STORE-IMENU is non-nil, store imenu metadata for collapsed navigation.
Returns the section object."
  (let ((commit-count (length processed-commits))
        (commits-section nil))
    (setq commits-section
          (magit-insert-section (pr-commits nil t)
            (magit-insert-heading
              (format "%s Commits (%d)" (shipit--get-comment-type-icon "review" "📝") commit-count))
            (magit-insert-section-body
              (if (> commit-count 0)
                  (dolist (commit-data (reverse processed-commits))
                    (shipit--insert-processed-commit-section commit-data repo pr-number))
                (insert "   No commits found\n"))
              (insert "\n"))))
    ;; Store imenu metadata if requested
    (when (and store-imenu commits-section (> commit-count 0))
      (let ((child-names
             (mapcar (lambda (commit-data)
                       (format "%s %s"
                               (or (plist-get commit-data :short-sha) "unknown")
                               (or (plist-get commit-data :first-line) "No message")))
                     processed-commits)))
        (shipit--imenu-store-children (oref commits-section start) child-names 'pr-commits)))
    commits-section))

(defun shipit--render-files-section (files &optional repo pr-number pr-data pr-head-sha store-imenu)
  "Render a files section with FILES.
FILES is a list of alists with keys: filename, status, additions, deletions.
REPO, PR-NUMBER, PR-DATA, PR-HEAD-SHA are optional context for interactive features.
If STORE-IMENU is non-nil, store imenu metadata for collapsed navigation.
Applies current filter if `shipit--files-filter-text' is set.
Returns the section object."
  (let* ((total-count (length files))
         (has-filter (shipit--files-filter-active-p))
         (filtered-files (if has-filter
                             (seq-filter (lambda (file)
                                           (shipit--file-matches-filter-p file shipit--files-filter-text))
                                         files)
                           files))
         (display-count (length filtered-files))
         (total-adds (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'additions f)) 0)) files)))
         (total-dels (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'deletions f)) 0)) files)))
         (files-section nil))
    (setq files-section
          (magit-insert-section (pr-files nil t)
            (let ((heading-start (point)))
              (magit-insert-heading
                (if has-filter
                    (format "%s Files Changed (%d/%d, %s %s)"
                            (shipit--get-files-icon "📄") display-count total-count
                            (if (> total-adds 0)
                                (propertize (format "+%d" total-adds) 'font-lock-face 'diff-added)
                              "+0")
                            (if (> total-dels 0)
                                (propertize (format "-%d" total-dels) 'font-lock-face 'diff-removed)
                              "-0"))
                  (format "%s Files Changed (%d, %s %s)"
                          (shipit--get-files-icon "📄") total-count
                          (if (> total-adds 0)
                              (propertize (format "+%d" total-adds) 'font-lock-face 'diff-added)
                            "+0")
                          (if (> total-dels 0)
                              (propertize (format "-%d" total-dels) 'font-lock-face 'diff-removed)
                            "-0"))))
              ;; Add filter display if active
              (let ((filter-display (shipit--get-files-filter-display)))
                (when filter-display
                  (insert (propertize (format "   Filter: %s\n"
                                              (propertize filter-display 'font-lock-face 'font-lock-string-face))
                                      'font-lock-face 'italic))))
              ;; Text properties for other features
              (add-text-properties heading-start (point)
                                   `(shipit-pr-files t

                                     help-echo "f to filter")))
            (magit-insert-section-body
              (if (> display-count 0)
                  (dolist (file filtered-files)
                    (shipit--insert-pr-file-section file repo pr-number pr-data pr-head-sha))
                (if has-filter
                    (insert "   No files match filter\n")
                  (insert "   No files changed\n")))
              (insert "\n"))))
    ;; Store imenu metadata if requested
    (when (and store-imenu files-section (> total-count 0))
      (let ((child-names
             (mapcar (lambda (file)
                       (or (cdr (assq 'filename file)) "unknown"))
                     files)))
        (shipit--imenu-store-children (oref files-section start) child-names 'pr-files)))
    files-section))

(defun shipit--insert-commits-section (repo pr pr-number)
  "Insert a commits section for the PR showing the commit list with expandable details."
  (shipit--debug-log "shipit--insert-commits-section called: repo=%s pr-number=%s" repo pr-number)
  (condition-case err
      (shipit--time-operation "commits section total"
                              (let* ((pr-commits (cdr (assq 'commits pr)))
                                     (commits (if (and pr-commits (sequencep pr-commits) (> (length pr-commits) 0))
                                                  pr-commits
                                                (progn
                                                  (shipit--debug-log "BUG: PR #%s has no commits data — loading sequence issue" pr-number)
                                                  nil))))
                                (shipit--debug-log "COMMITS-DEBUG: pr-commits=%s commits-count=%d"
                                                   (if pr-commits "found-in-pr" "nil")
                                                   (if (sequencep commits) (length commits) -1))
                                ;; Process commits into plist format, then use shared renderer
                                (let ((processed-commits
                                       (when (and (sequencep commits) (> (length commits) 0))
                                         (shipit--time-operation "batch process commit data"
                                                                 (mapcar #'shipit--process-single-commit-data commits)))))
                                  (shipit--render-commits-section processed-commits repo pr-number t))))
    (error
     (shipit--debug-log "Error in shipit--insert-commits-section: %S" err)
     (magit-insert-section (pr-commits nil t)
       (magit-insert-heading (format "%s Commits (error)" (shipit--get-comment-type-icon "review" "📝")))
       (magit-insert-section-body
         (insert "   Error loading commits\n"))))))

(defun shipit--extract-username-from-email (email)
  "Extract username from EMAIL using the active backend's pattern.
Returns nil if email doesn't match any known pattern."
  (when email
    (let* ((backend (shipit-pr--get-backend))
           (fn (plist-get backend :extract-username-from-email))
           (config (or shipit-pr-backend-config (list))))
      (when fn
        (funcall fn config email)))))

(defun shipit--generate-avatar-url (username)
  "Generate avatar URL from USERNAME using the active backend.
Returns nil if username is nil/empty or backend doesn't support it."
  (when (and username (not (string-empty-p username)))
    (let* ((backend (shipit-pr--get-backend))
           (fn (plist-get backend :generate-avatar-url))
           (config (or shipit-pr-backend-config (list))))
      (when fn
        (funcall fn config username)))))

(defun shipit--process-single-commit-data (commit)
  "Extract and process data for a single commit (parallelizable part)."
  (condition-case err
      (let* (;; Basic data extraction
             (sha (cdr (assq 'sha commit)))
             (commit-data (cdr (assq 'commit commit)))
             (raw-message (cdr (assq 'message commit-data)))
             ;; Message cleaning
             (message (when raw-message (shipit--clean-comment-text raw-message)))
             ;; Author/committer info extraction
             (author-info (cdr (assq 'author commit-data)))
             (committer-info (cdr (assq 'committer commit-data)))
             ;; GitHub user info
             (author-user (cdr (assq 'author commit)))
             (committer-user (cdr (assq 'committer commit)))
             (author (cdr (assq 'name author-info)))
             (author-email (cdr (assq 'email author-info)))
             (author-date (cdr (assq 'date author-info)))
             (author-login (when author-user (cdr (assq 'login author-user))))
             (author-avatar-url (when author-user (cdr (assq 'avatar_url author-user))))
             (committer (cdr (assq 'name committer-info)))
             (committer-email (cdr (assq 'email committer-info)))
             (committer-date (cdr (assq 'date committer-info)))
             (committer-login (when committer-user (cdr (assq 'login committer-user))))
             (committer-avatar-url (when committer-user (cdr (assq 'avatar_url committer-user))))
             ;; Fallback: try to extract GitHub username from email if no user object
             (author-github-username (or author-login
                                         (shipit--extract-username-from-email author-email)))
             (committer-github-username (or committer-login
                                            (shipit--extract-username-from-email committer-email)))
             (short-sha (if sha (substring sha 0 7) "unknown"))
             (first-line (if message
                             (let ((lines (split-string message "\n" t)))
                               (if lines (car lines) "Empty message"))
                           "No message"))
             (stats (cdr (assq 'stats commit)))
             (additions (if stats (cdr (assq 'additions stats)) 0))
             (deletions (if stats (cdr (assq 'deletions stats)) 0))
             ;; Extract files from commit object
             (files (cdr (assq 'files commit)))
             ;; File count: prefer actual files array length over stats.total
             ;; (stats.total = additions + deletions, NOT file count)
             (files-changed (if files
                                (length (if (vectorp files) (append files nil) files))
                              0)))

        ;; Return processed data structure
        (list :sha sha
              :message message
              :short-sha short-sha
              :first-line first-line
              :author author
              :author-email author-email
              :author-date author-date
              :author-github-username author-github-username
              :author-avatar-url author-avatar-url
              :committer committer
              :committer-email committer-email
              :committer-date committer-date
              :committer-github-username committer-github-username
              :committer-avatar-url committer-avatar-url
              :additions additions
              :deletions deletions
              :files-changed files-changed
              :files files))
    (error
     (shipit--debug-log "Error processing commit data: %S" err)
     ;; Return minimal data structure on error
     (list :sha (or (cdr (assq 'sha commit)) "unknown")
           :short-sha "error"
           :first-line "Error processing commit"))))

(defun shipit--format-commit-metadata-inline (author author-date)
  "Format commit metadata as columns: Author | Date.
Returns individual parts for layout control.
The :date value uses `shipit--format-timestamp' for toggle support.
The :raw-date contains the ISO timestamp for property storage."
  (let* ((date-str (if author-date
                       (shipit--format-timestamp author-date)
                     "unknown"))
         (author-str (or author "Unknown")))
    ;; Return plist with metadata parts
    (list :author author-str
          :date date-str
          :raw-date author-date)))

(defun shipit--insert-processed-commit-section (commit-data repo pr-number)
  "Insert a processed commit as a magit section (UI part)."
  (condition-case err
      (let* ((sha (plist-get commit-data :sha))
             (message (plist-get commit-data :message))
             (short-sha (plist-get commit-data :short-sha))
             (first-line (plist-get commit-data :first-line))
             (author (plist-get commit-data :author))
             (author-email (plist-get commit-data :author-email))
             (author-date (plist-get commit-data :author-date))
             (author-github-username (plist-get commit-data :author-github-username))
             (author-avatar-url (plist-get commit-data :author-avatar-url))
             (committer (plist-get commit-data :committer))
             (committer-email (plist-get commit-data :committer-email))
             (committer-date (plist-get commit-data :committer-date))
             (committer-github-username (plist-get commit-data :committer-github-username))
             (committer-avatar-url (plist-get commit-data :committer-avatar-url))
             (additions (plist-get commit-data :additions))
             (deletions (plist-get commit-data :deletions))
             (files-changed (plist-get commit-data :files-changed)))

        (magit-insert-section (shipit-commit sha t)
          ;; Debug logging for avatar data
          (shipit--debug-log "Commit avatar debug - SHA: %s" (or sha "nil"))
          (shipit--debug-log "  author-github-username: %S" author-github-username)
          (shipit--debug-log "  author-avatar-url: %S" author-avatar-url)
          (shipit--debug-log "  generated-avatar-url: %S" (shipit--generate-avatar-url author-github-username))

          ;; Insert heading using magit-insert-heading for proper section behavior
          (let* ((header-start (point))
                 ;; Check if this commit is unread (uses same tracking as activity section)
                 (read-activities (when (and shipit-show-unread-indicators repo pr-number)
                                    (shipit--get-read-activities repo pr-number)))
                 (is-unread (and shipit-show-unread-indicators
                                 sha
                                 read-activities  ; Only show unread if PR has been viewed before
                                 (not (shipit--is-activity-read-p repo pr-number sha))))
                 (unread-indicator (if is-unread
                                       (propertize "● " 'font-lock-face '(:foreground "red"))
                                     "  "))
                 (metadata-parts (shipit--format-commit-metadata-inline author author-date))
                 (metadata-author (plist-get metadata-parts :author))
                 (metadata-date (plist-get metadata-parts :date))
                 (raw-date (plist-get metadata-parts :raw-date))
                 ;; Format metadata columns with fixed widths and apply faces
                 (author-col (propertize (format "%11s" metadata-author) 'font-lock-face 'shipit-username-face))
                 ;; Fixed 16-char timestamp field, right-aligned (matches Activity section)
                 (date-text (substring-no-properties metadata-date))
                 (date-width (string-width date-text))
                 (date-pad (max 0 (- 16 date-width)))
                 (date-col (propertize (concat (make-string date-pad ?\s) date-text)
                                       'font-lock-face 'shipit-timestamp-face
                                       'shipit-raw-timestamp raw-date))
                 (metadata-str (format "%s | %s" author-col date-col))
                 ;; Calculate window width for right alignment
                 (window-width (max 80 (or (window-width) 80)))
                 ;; Calculate available width for title
                 ;; Reserve: indent (1) + unread (2) + SHA (8) + space (1) + metadata + space (1) + magit indicator (3)
                 (metadata-width (string-width metadata-str))
                 (magit-indicator-width 3)  ; Space for "..." or similar magit section indicator
                 (reserved-width (+ 1 2 8 1 metadata-width 1 magit-indicator-width))
                 (available-title-width (max 50 (- window-width reserved-width)))
                 (title-str (truncate-string-to-width first-line available-title-width))
                 ;; Build left part with SHA and title
                 (title-with-avatar (format "%s%s"
                                           (if shipit-show-avatars
                                               (concat (shipit--create-avatar-display author-github-username
                                                                                      (or author-avatar-url
                                                                                          (shipit--generate-avatar-url author-github-username)) 16) " ")
                                             "")
                                           (propertize title-str 'font-lock-face 'markdown-metadata-value-face)))
                 (left-part (format "%s %s" (propertize short-sha 'font-lock-face 'magit-hash) title-with-avatar))
                 ;; Calculate dynamic padding to right-align metadata to screen width
                 ;; Leave space for magit section indicator at the end
                 (left-width (string-width left-part))
                 (total-left (+ 1 2 left-width))  ; 1 space indent + 2 unread indicator + left part
                 (padding-needed (max 1 (- window-width total-left metadata-width magit-indicator-width)))
                 (padding (make-string padding-needed ?\s))
                 (heading-line (format " %s%s%s%s" unread-indicator left-part padding metadata-str)))
            (magit-insert-heading heading-line)
            ;; Apply interactive properties to the entire heading including leading whitespace
            ;; Use put-text-property to ensure our keymap takes precedence over magit's properties
            (put-text-property header-start (point) 'keymap shipit-commit-keymap)
            (add-text-properties header-start (point)
                                 `(shipit-commit t
                                                 shipit-commit-sha ,sha
                                                 shipit-commit-message ,message
                                                 shipit-repo ,repo
                                                 shipit-pr-number ,pr-number

                                                 help-echo "RET/SPC: show commit, TAB: toggle details, M-;: shipit-dwim")))

          ;; Expandable commit details
          (magit-insert-section-body
            (let ((body-start (point)))
              ;; Full message as a magit section (expanded by default)
              (when (and message (string-match-p "\n" message))
                (magit-insert-section (commit-full-message nil)  ; nil = expanded by default
                  (magit-insert-heading (format "           %s Full message" (shipit--get-comment-type-icon "review" "📝")))
                  (magit-insert-section-body
                    (let ((rendered-message (if (and (boundp 'shipit-render-markdown) shipit-render-markdown
                                                       (fboundp 'shipit--render-body))
                                                 (shipit--render-body message)
                                               message)))
                      (let ((message-lines (split-string rendered-message "\n")))
                        (dolist (line message-lines)
                          (insert (format "              %s\n" line)))))
                    (insert "\n"))))

              ;; Files subsection (independently collapsible, lazy-loaded)
              (if (and repo pr-number)
                  ;; API mode: lazy-load files from GitHub
                  (magit-insert-section (commit-files sha t)
                    (magit-insert-heading (format "           %s Files Changed (will load on expand)" (shipit--get-files-icon "📄")))
                    ;; Set the washer function to load files when section is first expanded
                    ;; The washer will be called by magit when the section is first shown
                    (when magit-insert-section--current
                      (let ((section magit-insert-section--current))
                        (oset section washer
                              (shipit--create-commit-files-washer repo sha section pr-number)))))
                ;; Preview mode: lazy-load files from local git
                (magit-insert-section (commit-files sha t)
                  (magit-insert-heading (format "           %s Files Changed (will load on expand)" (shipit--get-files-icon "📄")))
                  (when magit-insert-section--current
                    (let ((section magit-insert-section--current))
                      (oset section washer
                            (shipit--create-local-commit-files-washer sha section))))))

              ;; Author information with avatar (moved to heading for compact display)
              ;; Details removed: author/date are now on the heading line

              ;; Committer information (if different from author)
              (when (and committer
                         committer-email
                         (not (and (string= author committer)
                                   (string= author-email committer-email))))
                (insert (format "           %s Committer: %s%s <%s>\n"
                                (shipit--get-pr-field-icon "committer" "🔧")
                                (if shipit-show-avatars
                                    (concat (shipit--create-avatar-display committer-github-username
                                                                           (or committer-avatar-url
                                                                               (shipit--generate-avatar-url committer-github-username)) 16) " ")
                                  "")
                                (propertize committer 'font-lock-face 'shipit-username-face)
                                (propertize committer-email 'font-lock-face 'shipit-username-face)))
                (when committer-date
                  (insert (format "           %s Committed: %s\n"
                                  (shipit--get-pr-field-icon "committed" "📅")
                                  (propertize (format-time-string "%Y-%m-%d %H:%M:%S %Z"
                                                                  (date-to-time committer-date))
                                              'font-lock-face 'shipit-timestamp-face)))))

              ;; Statistics (if available)
              (when (or (> additions 0) (> deletions 0) (> files-changed 0))
                (insert (format "           %s Changes: %s files, %s, %s\n"
                                (shipit--get-pr-field-icon "stats" "📊")
                                (if (> files-changed 0)
                                    (propertize (number-to-string files-changed) 'font-lock-face 'bold)
                                  "0")
                                (if (> additions 0)
                                    (propertize (format "+%d" additions) 'font-lock-face 'diff-added)
                                  "+0")
                                (if (> deletions 0)
                                    (propertize (format "-%d" deletions) 'font-lock-face 'diff-removed)
                                  "-0"))))

              ;; Full SHA moved to heading line for compact display
              (insert "\n")

              ;; Apply keymap and properties to the entire section body
              (shipit--debug-log "DEBUG: About to use shipit-commit-keymap: %s"
                                 (if (keymapp shipit-commit-keymap) "VALID" "INVALID"))
              (when (not (keymapp shipit-commit-keymap))
                (shipit--debug-log "❌ shipit-commit-keymap is not a keymap! Value: %S" shipit-commit-keymap)
                (shipit--debug-log "❌ BACKTRACE for invalid keymap:")
                (shipit--debug-log "%s" (with-output-to-string (backtrace))))
              (condition-case err-keymap
                  (add-text-properties body-start (point)
                                       `(shipit-commit t
                                                       shipit-commit-sha ,sha
                                                       shipit-commit-message ,message
                                                       shipit-repo ,repo
                                                       shipit-pr-number ,pr-number
                                                       keymap ,shipit-commit-keymap

                                                       help-echo "RET/SPC: show commit, TAB: toggle details, M-;: shipit-dwim"))
                (error
                 (when (fboundp 'shipit--debug-log)
                   (shipit--debug-log "❌ BACKTRACE for commit keymap error: %S" err-keymap)
                   (shipit--debug-log "%s" (with-output-to-string (backtrace))))
                 ;; Re-signal the original error
                 (signal (car err-keymap) (cdr err-keymap))))))))
    (error
     (shipit--debug-log "Error in shipit--insert-processed-commit-section: %S" err)
     (insert "   Error processing commit\n"))))

(defun shipit--insert-commit-files-subsection (files repo pr-number)
  "Insert a Files subsection within a commit showing changed files.
Reuses the file rendering logic from the PR Files section.
FILES is a list of file objects from the PR data.
REPO and PR-NUMBER are used for context in file operations."
  (when (and (sequencep files) (> (length files) 0))
    (magit-insert-section (commit-files nil t)
      (let* ((total-count (length files))
             (has-filter (shipit--files-filter-active-p))
             (filtered-count (if has-filter
                                (length (seq-filter (lambda (file)
                                                      (shipit--file-matches-filter-p file shipit--files-filter-text))
                                                    files))
                              total-count))
             (total-adds (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'additions f)) 0)) files)))
             (total-dels (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'deletions f)) 0)) files))))
        ;; Insert heading with file count
        (let ((heading-start (point))
              (heading-end nil))
          (magit-insert-heading (format "%s Files Changed (%d, %s %s)"
                                        (shipit--get-files-icon "📄") total-count
                                        (if (> total-adds 0)
                                            (propertize (format "+%d" total-adds) 'font-lock-face 'diff-added)
                                          "+0")
                                        (if (> total-dels 0)
                                            (propertize (format "-%d" total-dels) 'font-lock-face 'diff-removed)
                                          "-0")))
          (setq heading-end (point))
          ;; Add filter display if active
          (let ((filter-display (shipit--get-files-filter-display)))
            (when filter-display
              (let ((filter-start (point)))
                (insert (propertize (format "   Filter: %s\n"
                                           (propertize filter-display
                                                       'font-lock-face 'font-lock-string-face))
                                   'font-lock-face 'italic))
                (add-text-properties filter-start (point) '(shipit-pr-file t)))))
          ;; Add keymap for 'f' to filter (same filter as main Files Changed section)
          (let ((header-keymap (make-sparse-keymap)))
            (set-keymap-parent header-keymap (current-local-map))
            (define-key header-keymap (kbd "f") #'shipit-files-filter)
            (let ((heading-overlay (make-overlay heading-start heading-end)))
              (overlay-put heading-overlay 'keymap header-keymap)
              (overlay-put heading-overlay 'evaporate t))
            (add-text-properties heading-start (point)
                                '(shipit-commit-files-header t

                                 help-echo "f to filter files"))))
        ;; Insert file content (body)
        (let* ((filtered-files (if has-filter
                                  (seq-filter (lambda (file)
                                               (shipit--file-matches-filter-p file shipit--files-filter-text))
                                             files)
                                files)))
          (when (and (sequencep filtered-files) (> (length filtered-files) 0))
            (dolist (file filtered-files)
              (shipit--insert-pr-file-section file repo pr-number nil pr-head-sha))))))))

(defun shipit--get-file-comment-count (pr-number filename)
  "Get the number of ACTIVE comments for FILENAME in PR-NUMBER using cached data.
Includes both line-level and file-level comments."
  ;; Use cached inline comments if available (much faster than individual API calls)
  (if (and (boundp 'shipit--inline-comments-fetched) shipit--inline-comments-fetched
           (boundp 'shipit--cached-inline-comments))
      ;; Fast path: filter from already-cached comments (count only active, non-resolved comments)
      ;; Include both line-level comments (have line/original_line) and file-level comments (no line)
      (let* ((file-comments (seq-filter (lambda (comment)
                                          (string= (cdr (assq 'path comment)) filename))
                                        shipit--cached-inline-comments))
             ;; Filter out both outdated and resolved comments
             ;; Note: REST API comments don't have resolved property; must check via hash
             (non-outdated (shipit--filter-active-comments file-comments))
             ;; For replies: check if root comment (or self) is resolved
             ;; Walk up in_reply_to chain to find root, then check if that's resolved
             (active-comments (cl-remove-if (lambda (comment)
                                              (condition-case err
                                                  (let* ((comment-id (cdr (assq 'id comment)))
                                                         (in-reply-to (cdr (assq 'in_reply_to_id comment)))
                                                         (root-id (shipit--find-thread-root-id comment-id in-reply-to file-comments))
                                                         (is-resolved (and root-id (shipit--is-comment-in-resolved-thread root-id))))
                                                    is-resolved)
                                                (error
                                                 nil)))
                                            non-outdated))
             (count (length active-comments)))
        count)
    ;; Fallback: Return 0 for now - async fetch will refresh later with correct counts
    (progn
      0)))

(defun shipit--get-file-outdated-comment-count (pr-number filename)
  "Get the number of outdated inline comments for FILENAME in PR-NUMBER using cached data."
  (if (and (boundp 'shipit--inline-comments-fetched) shipit--inline-comments-fetched
           (boundp 'shipit--cached-inline-comments))
      ;; Filter for outdated comments on this file
      (let* ((all-file-comments (seq-filter (lambda (comment)
                                              (and (string= (cdr (assq 'path comment)) filename)
                                                   (or (cdr (assq 'line comment))
                                                       (cdr (assq 'original_line comment)))))
                                            shipit--cached-inline-comments))
             (outdated-comments (shipit--filter-outdated-comments all-file-comments)))
        (length outdated-comments))
    0))

(defun shipit--get-file-resolved-comment-count (pr-number filename)
  "Get the number of resolved inline comments for FILENAME in PR-NUMBER using cached data."
  (if (and (boundp 'shipit--inline-comments-fetched) shipit--inline-comments-fetched
           (boundp 'shipit--cached-inline-comments))
      ;; Filter for resolved comments on this file
      (let* ((all-file-comments (seq-filter (lambda (comment)
                                              (and (string= (cdr (assq 'path comment)) filename)
                                                   (or (cdr (assq 'line comment))
                                                       (cdr (assq 'original_line comment)))))
                                            shipit--cached-inline-comments))
             ;; Include comments that are resolved (including replies in resolved threads)
             (resolved-comments (cl-remove-if-not (lambda (comment)
                                                    (let* ((comment-id (cdr (assq 'id comment)))
                                                           (in-reply-to (cdr (assq 'in_reply_to_id comment)))
                                                           (root-id (shipit--find-thread-root-id comment-id in-reply-to all-file-comments))
                                                           (is-resolved (and root-id (shipit--is-comment-in-resolved-thread root-id))))
                                                      is-resolved))
                                                  all-file-comments)))
        (length resolved-comments))
    0))

(defun shipit--get-comment-indicator (comment-count &optional outdated-count resolved-count)
  "Get comment indicator string based on comment counts.
Shows format like: 💬 (5) 🕐 2 ✅ 1 with indicators for active, outdated, and resolved comments.
COMMENT-COUNT is the number of active (non-outdated, non-resolved) comments.
OUTDATED-COUNT (optional) is the number of outdated comments.
RESOLVED-COUNT (optional) is the number of resolved comments.
The total count shown should be the sum of all comments (active + outdated + resolved)."
  (let ((active-indicator (if (> comment-count 0)
                              (format " %s (%d)" (shipit--get-comment-type-icon "comment" "💬") comment-count)
                            ""))
        (outdated-indicator (if (and outdated-count (> outdated-count 0))
                                (format " 🕐 %d" outdated-count)
                              ""))
        (resolved-indicator (if (and resolved-count (> resolved-count 0))
                                (format " ✅ %d" resolved-count)
                              "")))
    (concat active-indicator outdated-indicator resolved-indicator)))

(defun shipit--parse-hunks-from-patch (file-patch)
  "Parse FILE-PATCH into a list of hunks.
Each hunk is a plist containing:
  :header - the hunk header line (e.g., \"@@ -8,3 +8,5 @@\")
  :start-line - starting line number in new file
  :lines - list of diff lines (without hunk header)"
  (let ((hunks '())
        (current-hunk nil)
        (lines (split-string file-patch "\n")))
    (dolist (line lines)
      (if (string-match "^@@ -[0-9]+,?[0-9]* \\+\\([0-9]+\\)" line)
          (progn
            (when current-hunk
              (setf (plist-get current-hunk :lines) (nreverse (plist-get current-hunk :lines)))
              (push current-hunk hunks))
            (setq current-hunk
                  (list :header line
                        :start-line (string-to-number (match-string 1 line))
                        :lines nil)))
        (when current-hunk
          (push line (plist-get current-hunk :lines)))))
    (when current-hunk
      (setf (plist-get current-hunk :lines) (nreverse (plist-get current-hunk :lines)))
      (push current-hunk hunks))
    (nreverse hunks)))

(defun shipit--insert-file-comment (comment repo pr-number &optional resolved)
  "Insert a single COMMENT in the expanded file diff view.
REPO and PR-NUMBER are needed for comment interaction properties.
If RESOLVED is non-nil, includes [RESOLVED] indicator in header."
  (shipit--insert-comment comment
                         :base-indent 8
                         :repo repo
                         :pr-number pr-number
                         :include-header t
                         :include-prefix nil
                         :apply-faces (and (boundp 'shipit-inline-comment-faces)
                                          shipit-inline-comment-faces)
                         :apply-blockquote-styling nil
                         :inline-expanded t
                         :resolved resolved))

(defun shipit--insert-threaded-file-comment (comment threads repo pr-number depth)
  "Insert a file COMMENT with its replies as nested magit sections.
THREADS is a hash table from shipit--group-comments-by-api-replies.
REPO and PR-NUMBER are for interaction properties.
DEPTH is the current nesting level (0 for root comments).

Replies are inserted INSIDE the parent comment's section body to ensure
proper magit section hierarchy for TAB collapse to work correctly."
  (let* ((comment-id (cdr (assq 'id comment)))
         (raw-body (cdr (assq 'body comment)))
         (file-path (cdr (assq 'path comment)))
         (line-number (cdr (assq 'line comment)))
         (indent-str "        ")
         ;; For replies, increase indent level so wrapped lines align with body
         (indent-level (if (> depth 0) (+ 11 (* 4 (1- depth))) (length indent-str)))
         (indented-body (shipit--render-comment-body comment indent-level))
         (orange-color (if (eq (frame-parameter nil 'background-mode) 'light)
                           "#d2691e" "#ff8c00"))
         ;; Check if this comment's thread is resolved
         (is-resolved (shipit--is-comment-in-resolved-thread comment-id))
         (visibility (if is-resolved t nil)))
    ;; Create the section for THIS comment
    (magit-insert-section (inline-comment comment-id visibility)
      ;; Force comment visibility: expanded for unresolved, collapsed for resolved
      (when magit-insert-section--current
        (oset magit-insert-section--current hidden is-resolved))
      ;; The heading shows the full header with icon, avatar, user, and timestamp
      (let* ((user-obj (or (cdr (assq 'user comment)) (cdr (assq 'author comment))))
             (user (if (stringp user-obj) user-obj (cdr (assq 'login user-obj))))
             ;; Add indentation and tree symbol for nested replies
             ;; Replies should be indented to align with parent comment body (8 spaces base)
             (reply-prefix (if (> depth 0)
                              (concat (make-string (+ 5 (* 4 (1- depth))) ?\ ) "└─ ")
                            "     "))
             ;; Use shared header rendering function
             (header-without-prefix (shipit--render-comment-header comment depth 'inline-expanded))
             ;; Strip [RESOLVED] and [OUTDATED] tags from header to apply separate styling
             (is-resolved (or (cdr (assq 'resolved comment))
                            (and (cdr (assq 'id comment)) (shipit--is-comment-in-resolved-thread (cdr (assq 'id comment))))))
             (is-outdated (cdr (assq 'outdated comment)))
             (header-for-bold (replace-regexp-in-string " \\[\\(RESOLVED\\|OUTDATED\\)\\]" "" header-without-prefix))
             (header-str (concat reply-prefix (propertize header-for-bold
                                                          'font-lock-face 'bold
                                                          'shipit-icon-color orange-color)))
             (header-start (point)))
        (magit-insert-heading header-str)
        ;; Apply individual faces to components if enabled
        (when (and (boundp 'shipit-inline-comment-faces) shipit-inline-comment-faces)
          (let ((prefix-len (length reply-prefix))
                (user-pos (string-match user header-for-bold))
                (paren-start-pos (string-match "(" header-for-bold))
                (paren-end-pos (string-match ")" header-for-bold)))
            ;; Apply username face if found (adjust for reply-prefix)
            (when user-pos
              (put-text-property (+ header-start prefix-len user-pos)
                               (+ header-start prefix-len user-pos (length user))
                               'font-lock-face 'shipit-username-face))
            ;; Apply timestamp face - between opening and closing paren (adjust for reply-prefix)
            (when (and paren-start-pos paren-end-pos)
              (put-text-property (+ header-start prefix-len paren-start-pos 1)
                               (+ header-start prefix-len paren-end-pos)
                               'font-lock-face 'shipit-timestamp-face))))
        ;; Add and style [RESOLVED] tag on the same heading line
        (when is-resolved
          (save-excursion
            ;; Go back to the heading start line and position at the end
            (goto-char header-start)
            (end-of-line)
            ;; Insert [RESOLVED] on the same line
            (insert (propertize " [RESOLVED]" 'font-lock-face '(shadow italic)))))
        ;; Add and style [OUTDATED] tag on the same heading line
        (when is-outdated
          (save-excursion
            (goto-char header-start)
            (end-of-line)
            (insert (propertize " [OUTDATED]" 'font-lock-face '(warning italic)))))
        ;; Add unread indicator at end of heading (after face styling to preserve red color)
        (save-excursion
          (goto-char header-start)
          (end-of-line)
          (insert (shipit--get-comment-unread-indicator comment-id)))
        ;; CRITICAL: Add shipit text properties to the header for reply context detection
        ;; Without these properties, replying from the header line falls back to general comment API
        (add-text-properties header-start (line-end-position)
                             `(shipit-comment t
                               shipit-comment-id ,comment-id
                               shipit-review-id ,(cdr (assq 'pull_request_review_id comment))
                               shipit-comment-body ,raw-body
                               shipit-file-path ,file-path
                               shipit-line-number ,line-number)))
      ;; The body shows only body, reactions, and nested replies
      (magit-insert-section-body
        ;; Body portion - indented-body already includes proper indentation from shipit--render-comment-body
        (let ((body-str (concat indented-body "\n"))
              (body-start (point)))
          (insert body-str)
          (add-text-properties body-start (point)
                              `(shipit-comment t
                                              shipit-comment-id ,comment-id
                                              shipit-review-id ,(cdr (assq 'pull_request_review_id comment))
                                              shipit-comment-body ,raw-body
                                              shipit-comment-body-text t
                                              shipit-file-path ,file-path
                                              shipit-line-number ,line-number))
          ;; Add suggestion text properties for the `a' keybind
          (shipit--add-suggestion-text-properties body-start (point) comment)
          ;; Apply code block syntax highlighting and backgrounds
          (shipit--apply-code-block-backgrounds-in-region body-start (point))
          ;; Apply strikethrough faces for ~text~ patterns
          (shipit--apply-strikethrough-faces body-start (point)))
        ;; Reactions
        (let ((reactions (shipit--format-comment-reactions comment t)))
          (when reactions
            (insert "\n")
            (insert (propertize (format "%s%s\n" (if (> depth 0) (make-string (+ (length indent-str) 3) ?\ ) indent-str) reactions)
                                'shipit-reactions t
                                'shipit-comment t
                                'shipit-comment-id comment-id
                                'shipit-review-id (cdr (assq 'pull_request_review_id comment))
                                'shipit-comment-body raw-body
                                'shipit-file-path file-path
                                'shipit-line-number line-number
                                'shipit-repo repo
                                'shipit-pr-number pr-number))))
        ;; Insert replies as nested sections INSIDE this section's body
        (when threads
          (let ((replies (gethash comment-id threads)))
            (when replies
              (dolist (reply replies)
                ;; Replies are inserted as child sections (within parent's body)
                (shipit--insert-threaded-file-comment reply threads repo pr-number (1+ depth))))))
        ;; Spacing
        (insert "\n")
        ;; Apply rounded background (inside section so current = this section)
        (when (and (display-graphic-p)
                   (fboundp 'shipit-rounded--apply-to-section))
          (shipit-rounded--apply-to-section
           magit-insert-section--current orange-color)
          ;; Count overlays in this section's region for debugging
          (let* ((s (oref magit-insert-section--current start))
                 (e (point))
                 (ovs (cl-count-if (lambda (ov) (overlay-get ov 'shipit-rounded-bg))
                                   (overlays-in s e))))
            (shipit--debug-log "ROUNDED: depth=%d start=%s point=%s overlays=%d"
                               depth s e ovs)))))))

(defun shipit--insert-file-level-comment (comment filename repo pr-number)
  "Insert a file-level COMMENT (not on a specific line) as a collapsible magit section.
FILENAME, REPO, and PR-NUMBER provide context."
  (let* ((user-obj (cdr (assq 'user comment)))
         (user (or (cdr (assq 'login user-obj)) "Unknown"))
         (body (shipit--clean-text (or (cdr (assq 'body comment)) "")))
         (timestamp (cdr (assq 'created_at comment)))
         (formatted-timestamp (shipit--format-timestamp timestamp))
         (comment-id (cdr (assq 'id comment)))
         (comment-body (shipit--clean-text (or (cdr (assq 'body comment)) "")))
         (comment-icon (shipit--get-comment-type-icon "comment" "💬")))
    (magit-insert-section (file-level-comment comment-id)
      ;; Insert heading - this makes the section collapsible with TAB
      (magit-insert-heading
        (format "   %s File comment by %s %s"
                comment-icon
                (propertize user 'font-lock-face 'shipit-username-face)
                (propertize formatted-timestamp 'font-lock-face 'shipit-timestamp-face)))
      ;; Insert body content - this is what gets hidden/shown when collapsing
      (let ((body-start (point)))
        (let ((wrapped-body (shipit--wrap-text body (- (window-width) 8))))
          (dolist (line (split-string wrapped-body "\n"))
            (insert (format "      %s\n" line))))
        ;; Add text properties for dwim to the body
        (add-text-properties body-start (point)
                             `(shipit-comment t
                               shipit-comment-id ,comment-id
                               shipit-review-id ,(cdr (assq 'pull_request_review_id comment))
                               shipit-comment-body ,comment-body
                               shipit-file-path ,filename
                               shipit-repo ,repo
                               shipit-pr-number ,pr-number))
        ;; Apply strikethrough faces for ~text~ patterns
        (shipit--apply-strikethrough-faces body-start (point)))
      ;; Insert reactions placeholder (file comments are review comments, so is-inline=t)
      (let ((reactions (shipit--format-comment-reactions comment t)))
        (when reactions
          (let ((reactions-start (point)))
            (insert (format "      %s\n" reactions))
            (add-text-properties reactions-start (point)
                                 `(shipit-reactions t
                                   shipit-comment-id ,comment-id)))))
      (insert "\n")
      ;; Also add properties to the heading for dwim recognition
      (add-text-properties (oref magit-insert-section--current start) (point)
                           `(shipit-comment t
                             shipit-comment-id ,comment-id
                             shipit-review-id ,(cdr (assq 'pull_request_review_id comment))
                             shipit-comment-body ,comment-body
                             shipit-file-path ,filename
                             shipit-repo ,repo
                             shipit-pr-number ,pr-number))
      ;; Apply rounded background — same visual treatment as line-level inline comments
      (when (and (display-graphic-p)
                 (fboundp 'shipit-rounded--apply-to-section))
        (let ((orange-color (if (eq (frame-parameter nil 'background-mode) 'light)
                                "#d2691e" "#ff8c00")))
          (shipit-rounded--apply-to-section
           magit-insert-section--current orange-color))))))

(defun shipit--insert-outdated-comment-context (diff-hunk)
  "Insert the DIFF-HUNK context with proper diff highlighting."
  (when diff-hunk
    (let ((lines (split-string diff-hunk "\n")))
      (dolist (line lines)
        (let ((start (point)))
          (insert (concat "       " line "\n"))
          (cond
           ((string-prefix-p "+" line)
            (add-text-properties start (point) '(font-lock-face magit-diff-added)))
           ((string-prefix-p "-" line)
            (add-text-properties start (point) '(font-lock-face magit-diff-removed)))
           ((string-prefix-p "@@" line)
            (add-text-properties start (point) '(font-lock-face magit-diff-hunk-heading)))
           (t
            (add-text-properties start (point) '(font-lock-face magit-diff-context)))))))))

(defun shipit--insert-outdated-comments-section (outdated-comments comment-threads repo pr-number filename)
  "Insert a collapsible section for OUTDATED-COMMENTS.
COMMENT-THREADS is the hash table for threading.
REPO, PR-NUMBER, and FILENAME provide context for the comments."
  (magit-insert-section (shipit-outdated-comments filename t)  ; collapsed by default
    (magit-insert-heading
      (propertize (format "     🕐 Outdated Comments (%d)"
                          (length outdated-comments))
                  'font-lock-face 'magit-section-heading))
    (magit-insert-section-body
      (dolist (comment outdated-comments)
        (let ((in-reply-to (cdr (assq 'in_reply_to_id comment)))
              (diff-hunk (cdr (assq 'diff_hunk comment))))
          ;; Only insert root comments; replies handled by threading
          (unless in-reply-to
            ;; Insert the original diff context first
            (when diff-hunk
              (insert "       ─── Original context ───\n")
              (shipit--insert-outdated-comment-context diff-hunk)
              (insert "       ────────────────────────\n"))
            (shipit--insert-threaded-file-comment comment comment-threads repo pr-number 0))))
      (insert "\n"))))

(defun shipit--insert-file-diff (file-patch filename comment-count repo pr-number &optional commit-sha)
  "Insert diff for a file with inline comments.
FILE-PATCH is the patch text from GitHub API.
FILENAME is the file path.
COMMENT-COUNT is the number of comments on this file.
REPO and PR-NUMBER are used to fetch inline comments.
Optional COMMIT-SHA filters comments to those made on that specific commit."
  (if (not file-patch)
      (insert "   [No diff available - binary file or too large]\n")
    ;; Get inline comments for this file
    (shipit--debug-log "DIFF-INSERT: Starting - filename=%s, cache available=%s, cache size=%s, commit-sha=%s"
                       filename
                       (and (boundp 'shipit--cached-inline-comments) (not (null shipit--cached-inline-comments)))
                       (if (and (boundp 'shipit--cached-inline-comments) shipit--cached-inline-comments)
                           (length shipit--cached-inline-comments) 0)
                       commit-sha)
    (let* ((base-comments (when (and (boundp 'shipit--cached-inline-comments)
                                     shipit--cached-inline-comments)
                            (seq-filter (lambda (comment)
                                          (string= (cdr (assq 'path comment)) filename))
                                        shipit--cached-inline-comments)))
           ;; If commit-sha provided, filter to comments on that commit
           (all-file-comments (if commit-sha
                                  (shipit--filter-comments-by-commit base-comments commit-sha)
                                base-comments))
           ;; Separate active and outdated comments
           ;; When viewing a specific commit, show ALL comments for that commit (don't filter outdated)
           (file-comments (if commit-sha
                              all-file-comments  ; Show all when viewing specific commit
                            (shipit--filter-active-comments all-file-comments)))
           (outdated-comments (if commit-sha
                                  nil  ; No separate outdated section for per-commit view
                                (shipit--filter-outdated-comments all-file-comments)))
           (outdated-count (length outdated-comments))
           ;; Group comments by LINE NUMBER (not position!) and SIDE
           ;; RIGHT side comments go on added/context lines (use 'line' field)
           ;; LEFT side comments go on deleted lines (use 'original_line' field)
           (comments-by-line-right (make-hash-table :test 'equal))
           (comments-by-line-left (make-hash-table :test 'equal)))

      (shipit--debug-log "DIFF-INSERT: File %s has %d active comments, %d outdated (total cached: %d)"
                         filename
                         (length file-comments)
                         outdated-count
                         (if (and (boundp 'shipit--cached-inline-comments) shipit--cached-inline-comments)
                             (length shipit--cached-inline-comments)
                           0))

      ;; Debug: trace outdated property on all file comments
      (shipit--debug-log "OUTDATED-TRACE: file=%s checking %d comments for outdated property" filename (length all-file-comments))
      (dolist (c all-file-comments)
        (shipit--debug-log "OUTDATED-TRACE:   id=%s outdated=%S line=%S orig_line=%S"
                           (cdr (assq 'id c))
                           (cdr (assq 'outdated c))
                           (cdr (assq 'line c))
                           (cdr (assq 'original_line c))))

      ;; Render file-level comments first (comments with no line number)
      ;; Check for subject_type = "file" OR missing/null line fields
      (shipit--debug-log "FILE-LEVEL-CHECK: Checking %d file-comments for file-level comments" (length file-comments))
      (dolist (c file-comments)
        (shipit--debug-log "FILE-LEVEL-CHECK: comment id=%s subject_type=%S line=%S original_line=%S"
                           (cdr (assq 'id c))
                           (cdr (assq 'subject_type c))
                           (cdr (assq 'line c))
                           (cdr (assq 'original_line c))))
      (let ((file-level-comments (seq-filter (lambda (c)
                                               (let ((subject-type (cdr (assq 'subject_type c)))
                                                     (line (cdr (assq 'line c)))
                                                     (original-line (cdr (assq 'original_line c))))
                                                 (or (equal subject-type "file")
                                                     (and (or (null line) (eq line :json-null))
                                                          (or (null original-line) (eq original-line :json-null))))))
                                             file-comments)))
        (shipit--debug-log "FILE-LEVEL-CHECK: Found %d file-level comments" (length file-level-comments))
        (when file-level-comments
          (dolist (comment file-level-comments)
            (shipit--insert-file-level-comment comment filename repo pr-number))))

      ;; Group comments by threading relationships (in_reply_to_id)
      ;; Use all-file-comments (not just active) so outdated comment threads work too
      (let ((comment-threads (shipit--group-comments-by-api-replies all-file-comments)))
        (shipit--debug-log "THREADING: comment-threads has %d entries" (hash-table-count comment-threads))
        (maphash (lambda (k v) (shipit--debug-log "THREADING: key=%s has %d comments" k (if (listp v) (length v) 1))) comment-threads)

      ;; Build hash tables: line number -> list of comments, separated by side
      ;; Use 'side' field from GitHub API: "LEFT" for deletions, "RIGHT" for additions
      ;; RIGHT side comments: use 'line' field, stored in comments-by-line-right
      ;; LEFT side comments: use 'original_line' field, stored in comments-by-line-left
      ;; Skip file-level comments - they're already rendered separately above
      (dolist (comment file-comments)
        (let ((line (cdr (assq 'line comment)))
              (original-line (cdr (assq 'original_line comment)))
              (side (cdr (assq 'side comment)))
              (is-outdated (cdr (assq 'outdated comment)))
              (position (cdr (assq 'position comment)))
              (path (cdr (assq 'path comment)))
              (subject-type (cdr (assq 'subject_type comment))))
          (shipit--debug-log "DIFF-INSERT: Comment fields - line:%s original_line:%s side:%s outdated:%s position:%s path:%s subject_type:%s"
                             line original-line side is-outdated position path subject-type)
          ;; Skip file-level comments (already rendered above)
          (unless (or (equal subject-type "file")
                      (and (or (null line) (eq line :json-null))
                           (or (null original-line) (eq original-line :json-null))))
            ;; Determine side: only treat as LEFT side if explicitly marked as such
            ;; LEFT side = comment on deleted lines (use original_line)
            ;; RIGHT side = comment on added/context lines (use line, fall back to original_line for outdated)
            ;; Default to RIGHT side when 'side' is not set - most comments are on added/changed code
            (let* ((is-left-side (equal side "LEFT"))
                   ;; For RIGHT side, prefer 'line' but fall back to 'original_line' for outdated comments
                   ;; For LEFT side, always use 'original_line'
                   (effective-line (if is-left-side
                                       original-line
                                     (let ((l line))
                                       (if (or (null l) (eq l :json-null))
                                           original-line
                                         l))))
                   (target-hash (if is-left-side comments-by-line-left comments-by-line-right)))
              (when (and effective-line
                         (not (eq effective-line :json-null))
                         (integerp effective-line))
                (shipit--debug-log "DIFF-INSERT: Mapping comment to %s side line %S (outdated=%s)"
                                   (if is-left-side "LEFT" "RIGHT")
                                   effective-line
                                   is-outdated)
                (puthash effective-line
                         (cons comment (gethash effective-line target-hash))
                         target-hash))))))

      ;; Log the complete hash tables for debugging
      (shipit--debug-log "DIFF-INSERT: Hash table summary for file %s:" filename)
      (shipit--debug-log "  RIGHT side (added/context lines):")
      (maphash (lambda (line-num comments)
                 (shipit--debug-log "    Line %S: %d comment(s)" line-num (length comments)))
               comments-by-line-right)
      (shipit--debug-log "  LEFT side (deleted lines):")
      (maphash (lambda (line-num comments)
                 (shipit--debug-log "    Line %S: %d comment(s)" line-num (length comments)))
               comments-by-line-left)

      (shipit--debug-log "DIFF-INSERT: Filtered - all-file=%d active=%d outdated=%d"
                         (length all-file-comments) (length file-comments) outdated-count)
      ;; Parse hunks from the patch and insert each as a collapsible section
      (let ((hunks (shipit--parse-hunks-from-patch file-patch)))
        (shipit--debug-log "DIFF-INSERT: Found %d hunks" (length hunks))
        (dolist (hunk hunks)
          (let ((hunk-header (plist-get hunk :header))
                (hunk-lines (plist-get hunk :lines)))
            (magit-insert-section (diff-hunk hunk-header nil)
              ;; Force hunks to always be expanded (override magit's visibility cache)
              (when magit-insert-section--current
                (oset magit-insert-section--current hidden nil))
              ;; Insert the hunk header as the section heading
              (magit-insert-heading (propertize (concat "     " hunk-header)
                                               'font-lock-face 'magit-diff-hunk-heading))
              ;; Insert hunk contents and comments in the section body
              (magit-insert-section-body
                (let* ((current-line-number (string-to-number
                                            (save-match-data
                                              (when (string-match "^@@ -[0-9]+,?[0-9]* \\+\\([0-9]+\\)" hunk-header)
                                                (match-string 1 hunk-header)))))
                       ;; Also track old (deleted) line numbers for LEFT side comments
                       (current-old-line-number (string-to-number
                                                 (save-match-data
                                                   (when (string-match "^@@ -\\([0-9]+\\)" hunk-header)
                                                     (match-string 1 hunk-header)))))
                       (diff-line-ranges nil)
                       (hunk-body-start (point)))
                  (dolist (line hunk-lines)
                    (cond
                     ;; Added line
                     ((string-prefix-p "+" line)
                      (let ((start (point)))
                        (insert (concat "     " line "\n"))
                        (add-text-properties start (point)
                                             `(font-lock-face magit-diff-added
                                                    shipit-file-path ,filename
                                                    shipit-line-number ,current-line-number
                                                    shipit-repo ,repo
                                                    shipit-pr-number ,pr-number))
                        (put-text-property (+ start 5) (+ start 6) 'font-lock-face
                                           '(magit-diff-added-indicator magit-diff-added bold))
                        (push (list (+ start 6) (1- (point)) ?+) diff-line-ranges))
                      ;; Look up RIGHT side comments for added lines
                      (let ((comments-at-line (gethash current-line-number comments-by-line-right)))
                        (when comments-at-line
                          (shipit--debug-log "DIFF-INSERT: Inserting %d comments at added line %d (RIGHT side)" (length comments-at-line) current-line-number)
                          (dolist (comment (reverse comments-at-line))
                            ;; Only insert root comments (those without in_reply_to_id); replies handled recursively
                            (let ((comment-id (cdr (assq 'id comment)))
                                  (in-reply-to (cdr (assq 'in_reply_to_id comment))))
                              (shipit--debug-log "COMMENT-AT-LINE %d: id=%s in_reply_to=%s" current-line-number comment-id in-reply-to)
                              (unless in-reply-to
                                (shipit--insert-threaded-file-comment comment comment-threads repo pr-number 0))))))
                      (setq current-line-number (1+ current-line-number)))
                     ;; Removed line - check for LEFT side comments using old line number
                     ((string-prefix-p "-" line)
                      (let ((start (point)))
                        (insert (concat "     " line "\n"))
                        (add-text-properties start (point)
                                             `(font-lock-face magit-diff-removed
                                                    shipit-file-path ,filename
                                                    shipit-old-line-number ,current-old-line-number
                                                    shipit-repo ,repo
                                                    shipit-pr-number ,pr-number))
                        (put-text-property (+ start 5) (+ start 6) 'font-lock-face
                                           '(magit-diff-removed-indicator magit-diff-removed bold))
                        (push (list (+ start 6) (1- (point)) ?-) diff-line-ranges))
                      ;; Look up LEFT side comments for deleted lines
                      (let ((comments-at-line (gethash current-old-line-number comments-by-line-left)))
                        (when comments-at-line
                          (shipit--debug-log "DIFF-INSERT: Inserting %d comments at deleted line %d (LEFT side)" (length comments-at-line) current-old-line-number)
                          (dolist (comment (reverse comments-at-line))
                            ;; Only insert root comments (those without in_reply_to_id); replies handled recursively
                            (let ((comment-id (cdr (assq 'id comment)))
                                  (in-reply-to (cdr (assq 'in_reply_to_id comment))))
                              (shipit--debug-log "COMMENT-AT-OLD-LINE %d: id=%s in_reply_to=%s" current-old-line-number comment-id in-reply-to)
                              (unless in-reply-to
                                (shipit--insert-threaded-file-comment comment comment-threads repo pr-number 0))))))
                      (setq current-old-line-number (1+ current-old-line-number)))
                     ;; Context line
                     (t
                      (let ((start (point)))
                        (insert (concat "     " line "\n"))
                        (add-text-properties start (point)
                                             `(font-lock-face magit-diff-context
                                                              shipit-file-path ,filename
                                                              shipit-line-number ,current-line-number
                                                              shipit-old-line-number ,current-old-line-number
                                                              shipit-repo ,repo
                                                              shipit-pr-number ,pr-number))
                        (push (list (+ start 6) (1- (point)) ?\s) diff-line-ranges))
                      ;; Look up RIGHT side comments for context lines (they appear on the new/right side)
                      (let ((comments-at-line (gethash current-line-number comments-by-line-right)))
                        (when comments-at-line
                          (shipit--debug-log "DIFF-INSERT: Inserting %d comments at context line %d (RIGHT side)" (length comments-at-line) current-line-number)
                          (dolist (comment (reverse comments-at-line))
                            ;; Only insert root comments (those without in_reply_to_id); replies handled recursively
                            (let ((comment-id (cdr (assq 'id comment)))
                                  (in-reply-to (cdr (assq 'in_reply_to_id comment))))
                              (shipit--debug-log "COMMENT-AT-LINE %d: id=%s in_reply_to=%s" current-line-number comment-id in-reply-to)
                              (unless in-reply-to
                                (shipit--insert-threaded-file-comment comment comment-threads repo pr-number 0))))))
                      (setq current-line-number (1+ current-line-number))
                      (setq current-old-line-number (1+ current-old-line-number)))))
                  (when (and shipit-pr-fontify-hunks diff-line-ranges)
                    (shipit--fontify-diff-hunk-lines
                     (nreverse diff-line-ranges) filename))
                  (when (and shipit-pr-refine-hunks
                             (> (point) hunk-body-start))
                    (shipit--refine-diff-hunk hunk-body-start (point)))))))))
        ;; Insert outdated comments section if there are any
        (shipit--debug-log "OUTDATED-SECTION: file=%s outdated-count=%d" filename (length outdated-comments))
        (when (and outdated-comments (> (length outdated-comments) 0))
          (shipit--debug-log "OUTDATED-SECTION: Inserting section for %s with %d comments" filename (length outdated-comments))
          (shipit--insert-outdated-comments-section outdated-comments comment-threads repo pr-number filename))))))



(defun shipit--insert-files-content (repo pr pr-number)
  "Insert files content only (no header).
Used for targeted refresh - preserves section structure."
  (shipit--debug-log "INSERT-FILES-CONTENT called: repo=%s pr-number=%s pr-is-nil=%s pr-has-head=%s"
                     repo pr-number (if pr "NO" "YES") (if (and pr (assq 'head pr)) "YES" "NO"))
  ;; Add filter line if filter is active
  (let ((filter-display (shipit--get-files-filter-display)))
    (when filter-display
      (let* ((filter-start (point))
             (files (cdr (assq 'files pr)))
             (filtered-count (if (sequencep files)
                                 (length (seq-filter (lambda (file)
                                                       (shipit--file-matches-filter-p file shipit--files-filter-text))
                                                     files))
                               0)))
        (insert (propertize (format "   Filter: %s (%d)\n"
                                    (propertize filter-display
                                                'font-lock-face 'font-lock-string-face)
                                    filtered-count)
                            'font-lock-face 'italic))
        ;; Mark filter line with shipit-pr-file property so it gets deleted on refresh
        (add-text-properties filter-start (point) '(shipit-pr-file t)))))

  (let ((files (progn
                 (shipit--debug-log "FILES-DATA-DEBUG: pr type=%s, has-files=%s"
                                    (type-of pr)
                                    (if (assq 'files pr) "YES" "NO"))
                 (when pr
                   (let ((files-data (cdr (assq 'files pr))))
                     (shipit--debug-log "FILES-DATA-DEBUG: files-data type=%s, sequencep=%s, length=%s"
                                        (type-of files-data)
                                        (sequencep files-data)
                                        (if (sequencep files-data) (length files-data) "N/A"))
                     files-data)))))
    (shipit--debug-log "Files content: files count=%s, sequencep=%s, condition=%s"
                       (if (sequencep files) (length files) "not-sequence")
                       (sequencep files)
                       (and (sequencep files) (> (length files) 0)))
    (if (and (sequencep files) (> (length files) 0))
        (progn
          ;; Apply filter to files
          (let* ((pr-head-sha (cdr (assq 'sha (cdr (assq 'head pr)))))
                 (_debug-pr-head (shipit--debug-log "PR FILES: pr-head-sha extraction - pr has head=%s sha=%s"
                                                   (if (assq 'head pr) "YES" "NO")
                                                   pr-head-sha))
                 (filtered-files (if (shipit--files-filter-active-p)
                                     (seq-filter (lambda (file)
                                                   (shipit--file-matches-filter-p file shipit--files-filter-text))
                                                 files)
                                   files)))
            (if (and (sequencep filtered-files) (> (length filtered-files) 0))
                (progn
                  (dolist (file filtered-files)
                    (shipit--insert-pr-file-section file repo pr-number pr pr-head-sha))
                  t)
              t)))
      ;; No files at all
      (insert "   No files changed\n"))
    ;; Trailing newline to preserve section boundary
    (insert "\n")))

(defun shipit--refresh-file-inline-comment-section (file-path pr-number repo)
  "Refresh inline comments for a single FILE-PATH without full PR buffer refresh.
Follows the labels section targeted refresh pattern with ETag caching.
Uses text properties (shipit-file-path) to locate sections - robust with any icon type.
Returns t on success, nil on failure.

IMPORTANT: This function updates a section that contains child sections (hunks).
Per magit-section best practices, we must:
1. Clear the parent section's children list before deleting content
2. Delete only between content and end (not heading)
3. Update the section's end marker after inserting new content"
  (let ((refresh-start (float-time))
        (section-found nil)
        (refresh-successful nil))
    (save-excursion
      (goto-char (point-min))
      ;; Use text properties to find the file section - works with emoji or SVG icons
      (let ((pos (point-min)))
        (while (and (not section-found)
                    (setq pos (text-property-any pos (point-max) 'shipit-pr-file t)))
          (let ((found-file-path (get-text-property pos 'shipit-file-path)))
            (when (and found-file-path (string= found-file-path file-path))
              (setq section-found t)
              (goto-char pos)
              (let ((section (when (fboundp 'magit-current-section) (magit-current-section))))
                (unless section
                  (error "TARGET-REFRESH: magit-current-section returned nil at point %d" (point)))
                ;; Get content start (after heading) - this is where body begins
                (let* ((content-start (oref section content))
                       (section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m))))
                  (let ((inhibit-read-only t)
                        (inhibit-point-motion-hooks t))
                    (condition-case err
                        ;; Use cached comments instead of fetching fresh from API
                        ;; This preserves in_reply_to_id that we manually add for new replies
                        ;; (GitHub REST API doesn't return in_reply_to_id in responses)
                        (let* ((file-comments (when (boundp 'shipit--cached-inline-comments)
                                                (seq-filter (lambda (c)
                                                              (string= (cdr (assq 'path c)) file-path))
                                                            shipit--cached-inline-comments)))
                               (comment-count (length file-comments))
                               (pr-data shipit-buffer-pr-data)
                               (files (when pr-data (cdr (assq 'files pr-data))))
                               (file-data (when files
                                            (seq-find (lambda (f)
                                                        (string= (cdr (assq 'filename f)) file-path))
                                                      files)))
                               (patch (when file-data (cdr (assq 'patch file-data)))))
                          (unless file-data
                            (error "TARGET-REFRESH: file-data not found for %s" file-path))
                          (shipit--debug-log "TARGET-REFRESH: Using %d cached comments for file %s"
                                             comment-count file-path)

                          ;; CRITICAL: Clear children list before deleting content
                          ;; This prevents stale references to deleted child sections
                          (oset section children nil)

                          ;; Delete ONLY the body content (between content and end)
                          ;; This preserves the heading and section structure
                          (when (and content-start section-end)
                            (delete-region content-start section-end)
                            (goto-char content-start)

                            ;; Insert new content - child sections will be added to children list
                            ;; by magit-insert-section macro automatically
                            (let ((magit-insert-section--parent section))
                              (shipit--insert-file-diff patch file-path comment-count repo pr-number))

                            ;; Update section end marker to new position
                            (let ((new-end (point-marker)))
                              (oset section end new-end)

                              ;; CRITICAL: Recalculate ancestor section end markers
                              ;; When child content changes size, parent end markers may become stale.
                              ;; Each parent's end should be the maximum end of all its children.
                              (let ((parent (oref section parent)))
                                (while parent
                                  (let* ((children (oref parent children))
                                         (max-end (oref parent content))) ; Start with content marker
                                    ;; Find the maximum end position among all children
                                    (dolist (child children)
                                      (let ((child-end (oref child end)))
                                        (when (and child-end
                                                   (marker-position child-end)
                                                   (or (null max-end)
                                                       (> (marker-position child-end)
                                                          (marker-position max-end))))
                                          (setq max-end child-end))))
                                    ;; Update parent end if needed
                                    (when max-end
                                      (let ((old-end (oref parent end)))
                                        (unless (and old-end
                                                     (= (marker-position old-end) (marker-position max-end)))
                                          (oset parent end (copy-marker max-end))))))
                                  (setq parent (oref parent parent)))))

                            (setq refresh-successful t))
                          )
                      (error
                       (shipit--debug-log "TARGET-REFRESH: Error: %s" (error-message-string err))
                       (message "Failed to refresh: %s" (error-message-string err))
                       (setq refresh-successful nil)))))))
            ;; Move to next position with shipit-pr-file property
            (setq pos (1+ pos))))))
    (unless section-found
      (message "File section not visible"))
    refresh-successful))

(defun shipit--insert-files-section (repo pr pr-number)
  "Insert a files section for the PR showing changed files."
  (shipit--time-operation "files-section-total"
                          (shipit--debug-log "shipit--insert-files-section called: repo=%s pr-number=%s" repo pr-number)

                          ;; Ensure inline comments are cached - fetch fresh if cache is empty
                          ;; Use force-fresh=t to bypass ETag cache and get truly fresh data during refresh
                          (unless (and (boundp 'shipit--cached-inline-comments)
                                      shipit--cached-inline-comments)
                            (shipit--debug-log "FILES-SECTION: Cache empty, fetching fresh inline comments for PR #%s" pr-number)
                            (shipit--fetch-inline-comments repo pr-number t))
                          (shipit--debug-log "FILES-SECTION: Using cached inline comments (count: %d)"
                                             (if (and (boundp 'shipit--cached-inline-comments)
                                                      shipit--cached-inline-comments)
                                                 (length shipit--cached-inline-comments)
                                               0))
                          (let ((files-section nil))
                            (setq files-section
                                  (magit-insert-section (pr-files nil t)
                            (let* ((files (shipit--time-operation "extract files from PR data"
                                                                  ;; CRITICAL: No API calls during rendering - only use cached data
                                                                  ;; This matches notifications pattern: render only what's in memory
                                                                  (progn
                                                                    (shipit--debug-log "FILES-DATA-DEBUG: pr type=%s, has-files=%s"
                                                                                       (type-of pr)
                                                                                       (if (assq 'files pr) "YES" "NO"))
                                                                    (when pr
                                                                      (shipit--debug-log "FILES-DATA-DEBUG: pr keys=%s"
                                                                                         (mapcar 'car pr)))
                                                                    (let ((files-val (cdr (assq 'files pr))))
                                                                      (shipit--debug-log "FILES-DATA-DEBUG: files-val type=%s, count=%s"
                                                                                         (type-of files-val)
                                                                                         (if (sequencep files-val) (length files-val) "not-sequence"))
                                                                      files-val))))
                                   (total-count (if (sequencep files) (length files) 0))
                                   (total-adds (if (sequencep files)
                                                   (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'additions f)) 0)) files))
                                                 0))
                                   (total-dels (if (sequencep files)
                                                   (apply #'+ (mapcar (lambda (f) (or (cdr (assq 'deletions f)) 0)) files))
                                                 0))
                                   (has-filter (shipit--files-filter-active-p))
                                   ;; Count filtered files if filter is active
                                   (filtered-count (if (and has-filter (sequencep files))
                                                       (length (seq-filter (lambda (file)
                                                                             (shipit--file-matches-filter-p file shipit--files-filter-text))
                                                                           files))
                                                     total-count)))
                              (shipit--time-operation "insert files heading"
                                                      (let ((header-start (point))
                                                            (heading-start nil)
                                                            (heading-end nil))
                                                        (setq heading-start (point))
                                                        (magit-insert-heading
                                                          (format "%s Files Changed (%d, %s %s)"
                                                                  (shipit--get-files-icon "📄") total-count
                                                                  (if (> total-adds 0)
                                                                      (propertize (format "+%d" total-adds) 'font-lock-face 'diff-added)
                                                                    "+0")
                                                                  (if (> total-dels 0)
                                                                      (propertize (format "-%d" total-dels) 'font-lock-face 'diff-removed)
                                                                    "-0")))
                                                        (setq heading-end (point))
                                                        ;; Add filter field display after heading if filter is active (matches notifications pattern)
                                                        (let ((filter-display (shipit--get-files-filter-display)))
                                                          (when filter-display
                                                            (let ((filter-start (point)))
                                                              (insert (propertize (format "   Filter: %s\n"
                                                                                          (propertize filter-display
                                                                                                      'font-lock-face 'font-lock-string-face))
                                                                                  'font-lock-face 'italic))
                                                              ;; Mark filter line with shipit-pr-file property so it gets deleted during refresh
                                                              (add-text-properties filter-start (point) '(shipit-pr-file t)))))
                                                        ;; Add keymap for 'f' to filter
                                                        (let ((header-keymap (make-sparse-keymap)))
                                                          (set-keymap-parent header-keymap (current-local-map))
                                                          (define-key header-keymap (kbd "f") #'shipit-files-filter)

                                                          ;; Use an overlay for the heading to add our keymap without interfering with Magit's section system
                                                          ;; Overlays have higher priority than text properties, so our keymap will work
                                                          (let ((heading-overlay (make-overlay heading-start heading-end)))
                                                            (overlay-put heading-overlay 'keymap header-keymap)
                                                            (overlay-put heading-overlay 'evaporate t))  ; Auto-delete when text is deleted

                                                          ;; Apply to entire header region (heading + filter line if present) using text properties
                                                          (add-text-properties header-start (point)
                                                                               `(shipit-pr-files t
                                                                                                 shipit-pr-number ,pr-number
                                                                                                 keymap ,header-keymap

                                                                                                 help-echo "f to filter")))))
                              ;; Insert file content in section body for proper TAB collapse/expand
                              (magit-insert-section-body
                                (shipit--debug-log "Files section: files count=%s, sequencep=%s, condition=%s"
                                                   (if (sequencep files) (length files) "not-sequence")
                                                   (sequencep files)
                                                   (and (sequencep files) (> (length files) 0)))
                                (if (and (sequencep files) (> (length files) 0))
                                    (shipit--time-operation "process all files"
                                                            ;; Apply filter to files
                                                            (let* ((filtered-files (if (shipit--files-filter-active-p)
                                                                                       (seq-filter (lambda (file)
                                                                                                     (shipit--file-matches-filter-p file shipit--files-filter-text))
                                                                                                   files)
                                                                                     files)))
                                                              (when (and (sequencep filtered-files) (> (length filtered-files) 0))
                                                                (let ((pr-head-sha (cdr (assq 'sha (cdr (assq 'head pr))))))
                                                                  (shipit--time-operation "insert all processed files"
                                                                    (dolist (file filtered-files)
                                                                      (shipit--insert-pr-file-section file repo pr-number pr pr-head-sha)))))))
                                  ;; ELSE clause: no files at all
                                  (progn
                                    (shipit--debug-log "Files section: ELSE branch - no files")
                                    (shipit--time-operation "insert no files message"
                                                            (insert "   No files changed\n"))))
                                (insert "\n")))))
                            ;; Store imenu metadata for children so they appear even when collapsed
                            (when files-section
                              (let* ((files (cdr (assq 'files pr)))
                                     (child-names
                                      (when (sequencep files)
                                        (mapcar (lambda (file)
                                                  (cdr (assq 'filename file)))
                                                files))))
                                (shipit--imenu-store-children (oref files-section start) child-names 'pr-files)))
                            ;; Explicitly hide the section - HIDE=t doesn't reliably work
                            (when files-section
                              (magit-section-hide files-section)))))

;;;
;;; Activity Timeline Section
;;;

(defun shipit--build-author-username-mapping (events pr-data repo pr-number)
  "Build a mapping from Git author names/emails to GitHub usernames.
Analyzes EVENTS and PR-DATA to find correlations between Git identities and GitHub users.
REPO and PR-NUMBER are used to fetch commits if not available in PR-DATA.
Returns a hash table mapping names/emails to usernames."
  (let ((mapping (make-hash-table :test 'equal))
        (email-to-username (make-hash-table :test 'equal))
        (commit-count 0))
    ;; Phase 1: Extract email->username mappings from non-commit events
    ;; Reviews, comments, etc. have GitHub usernames we can correlate
    ;; NOTE: GitHub API rarely includes email in user objects, so this phase usually finds nothing
    (dolist (event events)
      (let* ((event-type (cdr (assq 'event event)))
             (user-obj (or (cdr (assq 'user event))
                           (cdr (assq 'actor event))
                           (cdr (assq 'submitted_by event))))
             (github-login (when user-obj (cdr (assq 'login user-obj))))
             (user-email (when user-obj (cdr (assq 'email user-obj)))))
        ;; Store email->username correlation if we have both
        (when (and github-login user-email)
          (puthash user-email github-login email-to-username))))

    ;; Also check PR author
    (when pr-data
      (let* ((pr-user (cdr (assq 'user pr-data)))
             (pr-login (when pr-user (cdr (assq 'login pr-user))))
             (pr-email (when pr-user (cdr (assq 'email pr-user)))))
        (when (and pr-login pr-email)
          (puthash pr-email pr-login email-to-username))))

    ;; Phase 1.5: Use PR commits data (fetching directly if needed)
    ;; The /repos/{owner}/{repo}/pulls/{number}/commits endpoint includes GitHub user objects
    ;; Note: Basic PR data has (commits . N) as a count, not a list
    ;; Enhanced PR data has (commits . ((sha ...) ...)) as a list of commit objects
    (let* ((pr-commits-raw (when pr-data (cdr (assq 'commits pr-data))))
           ;; Only use pr-commits if it's actually a list of commits, not a count
           (pr-commits (when (and pr-commits-raw (listp pr-commits-raw)) pr-commits-raw)))
      (let* ((commits (if (and pr-commits (> (length pr-commits) 0))
                          pr-commits
                        ;; Need to fetch - dispatch through active backend
                        (if (and repo pr-number (> pr-number 0))
                            (condition-case err
                                (let* ((resolved (shipit-pr--resolve-for-repo repo))
                                       (backend (car resolved))
                                       (config (cdr resolved))
                                       (fetch-fn (plist-get backend :fetch-commits)))
                                  (when fetch-fn
                                    (funcall fetch-fn config pr-number)))
                              (error
                               (shipit--debug-log "AUTHOR-MAPPING-FALLBACK: Error fetching commits: %s" (error-message-string err))
                               nil))
                          nil))))
        (when commits
          (dolist (commit-wrapper commits)
            (let* ((commit-data (cdr (assq 'commit commit-wrapper)))
                   ;; GitHub user objects (have login)
                   (author-user (cdr (assq 'author commit-wrapper)))
                   (committer-user (cdr (assq 'committer commit-wrapper)))
                   ;; Git identity data (from commit object)
                   (author-git (cdr (assq 'author commit-data)))
                   (committer-git (cdr (assq 'committer commit-data)))
                   ;; Extract fields
                   (author-login (when author-user (cdr (assq 'login author-user))))
                   (author-email (when author-git (cdr (assq 'email author-git))))
                   (committer-login (when committer-user (cdr (assq 'login committer-user))))
                   (committer-email (when committer-git (cdr (assq 'email committer-git)))))
              ;; Store email->username mappings
              (when (and author-login author-email)
                (puthash author-email author-login email-to-username))
              (when (and committer-login committer-email)
                (puthash committer-email committer-login email-to-username)))))))

    ;; Phase 2: For commit events, map Git author info to GitHub login
    (dolist (event events)
      (let ((event-type (cdr (assq 'event event))))
        (when (string= event-type "committed")
          (setq commit-count (1+ commit-count))
          (let* ((author (cdr (assq 'author event)))
                 (committer (cdr (assq 'committer event)))
                 ;; GitHub login is stored directly in author/committer objects
                 (author-login (when author (cdr (assq 'login author))))
                 (committer-login (when committer (cdr (assq 'login committer))))
                 ;; Git identity info
                 (author-name (when author (cdr (assq 'name author))))
                 (author-email (when author (cdr (assq 'email author))))
                 (committer-name (when committer (cdr (assq 'name committer))))
                 (committer-email (when committer (cdr (assq 'email committer))))
                 ;; Try to get GitHub login from:
                 ;; 1. Direct login in commit event
                 ;; 2. Email correlation from other events
                 (github-login (or author-login
                                   committer-login
                                   (when author-email (gethash author-email email-to-username))
                                   (when committer-email (gethash committer-email email-to-username)))))
            ;; Only create mapping if we have a GitHub login
            (when github-login
              ;; Map both name and email to username
              (when author-name
                (puthash author-name github-login mapping))
              (when author-email
                (puthash author-email github-login mapping))
              (when committer-name
                (puthash committer-name github-login mapping))
              (when committer-email
                (puthash committer-email github-login mapping)))))))
    mapping))

(defun shipit--insert-activity-event (event repo pr-number author-mapping &optional indent)
  "Insert a single activity EVENT in the timeline.
EVENT is an alist containing event data from GitHub timeline API.
REPO and PR-NUMBER are used for navigation context.
AUTHOR-MAPPING is a hash table mapping Git author names/emails to GitHub usernames.
INDENT is optional parent section indent in columns (default 0)."
  (let* ((event-type (cdr (assq 'event event)))
         ;; Different event types use different fields for the user
         ;; Try: user, actor, author, submitted_by in that order
         (user-obj (or (cdr (assq 'user event))
                       (cdr (assq 'actor event))
                       (cdr (assq 'author event))
                       (cdr (assq 'submitted_by event))))
         (actor-login (or (when user-obj (cdr (assq 'login user-obj)))
                          "Unknown"))
         ;; Try multiple timestamp fields - different events use different names
         ;; Also check nested objects (e.g., comments have their own created_at)
         (created-at (or (cdr (assq 'created_at event))
                         (cdr (assq 'submitted_at event))
                         (cdr (assq 'updated_at event))
                         ;; Check nested comment object
                         (let ((comment (cdr (assq 'comment event))))
                           (when comment (cdr (assq 'created_at comment))))
                         ;; Check author object for commit date
                         (let ((author (cdr (assq 'author event))))
                           (when author (cdr (assq 'date author))))))
         (timestamp (shipit--format-timestamp created-at)))
    (cond
     ;; Review events
     ((string= event-type "reviewed")
      (let* ((state (cdr (assq 'state event)))
             (body (cdr (assq 'body event)))
             (review-id (cdr (assq 'id event)))
             (icon (pcase state
                     ("approved" "✅")
                     ("changes_requested" "🔄")
                     ("commented" "💬")
                     (_ "📝"))))
        ;; Store review ID for potential navigation
        (when review-id
          (push (cons 'shipit-activity-comment-id review-id) event))
        ;; Event already contains user-obj with avatar_url
        (shipit--insert-activity-item icon actor-login
                                      (format "%s this PR" state)
                                      timestamp body event repo pr-number indent)))

     ;; Comment events (both general and inline comments)
     ((string= event-type "commented")
      (let ((body (cdr (assq 'body event)))
            (comment-id (cdr (assq 'id event))))
        ;; Store comment ID for navigation
        (when comment-id
          (push (cons 'shipit-activity-comment-id comment-id) event))
        ;; Event already contains user-obj with avatar_url
        (shipit--insert-activity-item "💬" actor-login
                                      "commented"
                                      timestamp body event repo pr-number indent)))

     ;; Synthetic inline comment events (from shipit--inline-comment-to-activity-event)
     ((string= event-type "line-commented")
      (let ((body (cdr (assq 'body event)))
            (path (cdr (assq 'path event))))
        ;; Show file path in action text
        (shipit--insert-activity-item "📝" actor-login
                                      (format "commented on %s" (file-name-nondirectory (or path "file")))
                                      timestamp body event repo pr-number indent)))

     ;; Review request events
     ((string= event-type "review_requested")
      (let* ((requested-reviewer (cdr (assq 'requested_reviewer event)))
             (requested-team (cdr (assq 'requested_team event)))
             (reviewer-name (or (when requested-reviewer
                                  (cdr (assq 'login requested-reviewer)))
                                (when requested-team
                                  (cdr (assq 'name requested-team))))))
        (when reviewer-name
          (shipit--insert-activity-item "👀" actor-login
                                        (format "requested review from %s" reviewer-name)
                                        timestamp nil event repo pr-number indent))))

     ;; Label events
     ((or (string= event-type "labeled") (string= event-type "unlabeled"))
      (let* ((label (cdr (assq 'label event)))
             (label-name (when label (cdr (assq 'name label))))
             (action (if (string= event-type "labeled") "added" "removed"))
             (action-text (format "%s label: %s" action label-name)))
        (shipit--insert-activity-item "🏷" actor-login
                                      action-text
                                      timestamp nil event repo pr-number indent)))

     ;; Assignment events
     ((or (string= event-type "assigned") (string= event-type "unassigned"))
      (let* ((assignee (cdr (assq 'assignee event)))
             (assignee-login (when assignee (cdr (assq 'login assignee))))
             (action (if (string= event-type "assigned") "assigned" "unassigned")))
        (shipit--insert-activity-item "👤" actor-login
                                      (format "%s %s" action assignee-login)
                                      timestamp nil event repo pr-number indent)))

     ;; State change events
     ((string= event-type "closed")
      (shipit--insert-activity-item "❌" actor-login
                                    "closed this PR"
                                    timestamp nil event repo pr-number indent))

     ((string= event-type "reopened")
      (shipit--insert-activity-item "🔄" actor-login
                                    "reopened this PR"
                                    timestamp nil event repo pr-number indent))

     ((string= event-type "merged")
      (shipit--insert-activity-item "🎉" actor-login
                                    "merged this PR"
                                    timestamp nil event repo pr-number indent))

     ;; Commit events
     ((string= event-type "committed")
      (let* ((sha (cdr (assq 'sha event)))
             (message (cdr (assq 'message event)))
             (short-sha (when sha (substring sha 0 7)))
             ;; Commit events: prefer GitHub login over Git author name
             (author (cdr (assq 'author event)))
             (committer (cdr (assq 'committer event)))
             ;; Try to get GitHub login (but don't fall back to "Unknown" yet)
             (commit-login (or (when author (cdr (assq 'login author)))
                               (when committer (cdr (assq 'login committer)))))
             (commit-name (or (when author (cdr (assq 'name author)))
                              (when committer (cdr (assq 'name committer)))))
             (commit-email (or (when author (cdr (assq 'email author)))
                               (when committer (cdr (assq 'email committer)))))
             ;; Try to map Git author to GitHub username using the mapping
             (mapped-username (or (when commit-name (gethash commit-name author-mapping))
                                  (when commit-email (gethash commit-email author-mapping))))
             ;; Prefer: direct login, mapped username, Git name, actor-login, "Unknown"
             (commit-user (or commit-login mapped-username commit-name actor-login))
             ;; Get avatar URL for display - try direct URL or construct from username
             (avatar-url (or (when author (cdr (assq 'avatar_url author)))
                             (when committer (cdr (assq 'avatar_url committer)))
                             ;; If we have a mapped username or direct login, construct avatar URL
                             (when (or commit-login mapped-username)
                               (shipit--generate-avatar-url
                                (or commit-login mapped-username))))))
        ;; Store avatar info in event for rendering
        (when avatar-url
          (push (cons 'display-avatar-url avatar-url) event))
        ;; Store SHA for navigation
        (when sha
          (push (cons 'shipit-activity-commit-sha sha) event))
        (shipit--insert-activity-item "📝" commit-user
                                      (format "committed %s" short-sha)
                                      timestamp message event repo pr-number indent)))

     ;; Force push events
     ((string= event-type "head_ref_force_pushed")
      (let ((icon (if (fboundp 'shipit--get-activity-event-icon)
                      (shipit--get-activity-event-icon event-type nil "⬆️")
                    "⬆️")))
        (shipit--insert-activity-item icon actor-login
                                      "force-pushed"
                                      timestamp nil event repo pr-number indent)))

     ;; Cross-referenced events (when another PR/issue references this one)
     ((string= event-type "mentioned")
      (shipit--insert-activity-item "📣" actor-login
                                    "was mentioned"
                                    timestamp nil event repo pr-number indent))

     ((string= event-type "cross-referenced")
      (let* ((source (cdr (assq 'source event)))
             (source-issue (when source (cdr (assq 'issue source))))
             (source-type (when source (cdr (assq 'type source))))
             (source-number (when source-issue (cdr (assq 'number source-issue))))
             (source-title (when source-issue (cdr (assq 'title source-issue))))
             (source-url (when source-issue (cdr (assq 'html_url source-issue))))
             (source-repo-obj (when source-issue (cdr (assq 'repository source-issue))))
             (source-repo (when source-repo-obj (cdr (assq 'full_name source-repo-obj))))
             (source-state (when source-issue (cdr (assq 'state source-issue))))
             (action-text (cond
                          ((and source-repo source-number)
                           (format "cross-referenced from %s#%d" source-repo source-number))
                          (source-number
                           (format "cross-referenced from #%d" source-number))
                          (t "cross-referenced"))))
        ;; Store source info for navigation
        (when source-repo
          (push (cons 'shipit-crossref-repo source-repo) event))
        (when source-number
          (push (cons 'shipit-crossref-number source-number) event))
        (when source-url
          (push (cons 'shipit-crossref-url source-url) event))
        (when source-title
          (push (cons 'shipit-crossref-title source-title) event))
        (shipit--insert-activity-item "🔗" actor-login
                                      action-text
                                      timestamp source-title event repo pr-number indent)))

     ;; Catch-all for any other event types
     (t
      (let ((icon (if (fboundp 'shipit--get-activity-event-icon)
                     (shipit--get-activity-event-icon event-type nil "📌")
                   "📌")))
        (shipit--insert-activity-item icon actor-login
                                      (format "%s" event-type)
                                      timestamp nil event repo pr-number indent))))))

(defun shipit--insert-activity-item (icon actor action timestamp body event repo pr-number &optional indent)
  "Insert a single activity timeline item.
ICON is the emoji/symbol for the event type.
ACTOR is the GitHub username who performed the action.
ACTION is a description of what happened.
TIMESTAMP is when it happened.
BODY is optional additional content (comment text, etc.).
EVENT is the full event data for potential navigation.
REPO and PR-NUMBER provide context.
INDENT is optional parent section indent in columns.
This function adds 3 to create child indent (default 0 → margin 3)."
  (let* ((start (point))
         (has-body (and body (not (string-empty-p body))))
         (event-id (shipit--get-activity-event-id event))
         (event-type (cdr (assq 'event event)))
         ;; Extract comment ID if present (for navigation)
         (comment-id (cdr (assq 'shipit-activity-comment-id event)))
         ;; Extract commit SHA if present (for navigation)
         (commit-sha (cdr (assq 'shipit-activity-commit-sha event)))
         ;; Extract inline comment path if present (for line-commented navigation)
         (inline-path (cdr (assq 'shipit-inline-comment-path event)))
         ;; Extract review state if present (for reviewed events: approved, changes_requested, commented)
         (review-state (cdr (assq 'state event)))
         ;; Extract cross-reference info if present (for cross-referenced events)
         (crossref-repo (cdr (assq 'shipit-crossref-repo event)))
         (crossref-number (cdr (assq 'shipit-crossref-number event)))
         (crossref-url (cdr (assq 'shipit-crossref-url event)))
         (crossref-title (cdr (assq 'shipit-crossref-title event)))
         ;; Get the activity's created_at timestamp for unread detection
         (created-at (or (cdr (assq 'created_at event))
                         (cdr (assq 'submitted_at event))
                         (cdr (assq 'updated_at event))))
         ;; Check if this activity is unread (per-activity tracking)
         (read-activities (when (and shipit-show-unread-indicators repo pr-number)
                            (shipit--get-read-activities repo pr-number)))
         (is-unread (and shipit-show-unread-indicators
                         event-id
                         read-activities  ; Only show unread if PR has been viewed before
                         (not (shipit--is-activity-read-p repo pr-number event-id))))
         ;; Red dot indicator for unread items
         (unread-indicator (if is-unread
                               (propertize "● " 'font-lock-face '(:foreground "red"))
                             "  "))
         ;; Get avatar info - try multiple sources
         (user-obj (or (cdr (assq 'user event))
                       (cdr (assq 'actor event))
                       (cdr (assq 'author event))))
         (avatar-url (or (cdr (assq 'display-avatar-url event))
                         (when user-obj (cdr (assq 'avatar_url user-obj)))))
         (avatar-display (if (and shipit-show-avatars avatar-url
                                  (fboundp 'shipit--create-avatar-display))
                             (concat (shipit--create-avatar-display actor avatar-url 16) " ")
                           "")))
    (magit-insert-section (activity-event event-id t)
      (let* ((activity-margin (+ (or indent 0) 1))  ; parent + 3 - 2 (unread indicator width)
             (margin-str (make-string activity-margin ?\s))
             ;; Layout target: the right edge that notification
             ;; rows land on (`window-body-width' - 3 padding
             ;; cols).  Subtract `activity-margin' so the indent
             ;; doesn't push the timestamp past that edge, plus
             ;; one more column to compensate for a residual
             ;; 1-col offset versus the notification row layout
             ;; (varies with fringe/scrollbar/cursor-column).
             (window-width (- (let* ((raw (max 80 (or (window-body-width) 80)))
                                     (cap (and (boundp 'shipit-notifications-activity-line-width)
                                               shipit-notifications-activity-line-width)))
                                (if (and cap (numberp cap))
                                    (min cap raw)
                                  raw))
                              activity-margin
                              1))
             (magit-indicator-width 3)  ; Space for "..." or fold indicator (matches Commits section)
             ;; Fixed timestamp column width (16 chars handles both formats)
             (timestamp-width 16)
             ;; Fixed-width layout: unread (2) + left-content + space (1) + timestamp (16) + magit (3)
             (timestamp-col (- window-width timestamp-width magit-indicator-width 1))
             (left-max-width (max 20 (- timestamp-col 2)))  ; Subtract unread width
             ;; Build left part: icon + avatar + actor + action
             ;; Use SVG icons if enabled and available, otherwise use emoji
             (emoji-icon (shipit--clean-text icon))
             (icon-str (cond
                       ;; First try event-type based SVG rendering
                       ((and (fboundp 'shipit--get-activity-event-icon) event-type)
                        (shipit--get-activity-event-icon event-type emoji-icon))
                       ;; Fall back to emoji-to-SVG conversion for direct emoji icons
                       ((fboundp 'shipit--activity-emoji-to-icon)
                        (shipit--activity-emoji-to-icon emoji-icon))
                       ;; Final fallback to raw emoji
                       (t emoji-icon)))
             (actor-str (propertize actor 'font-lock-face 'shipit-username-face))
             ;; Format: icon + avatar + actor + action
             (left-content (format "%s %s%s %s" icon-str avatar-display actor-str action))
             ;; Truncate left content to exact fixed width
             (truncated-left (truncate-string-to-width left-content left-max-width))
             ;; Pad to exact width for alignment using string-width (handles emoji correctly)
             (padded-left (let* ((truncated-width (string-width truncated-left))
                                 (pad-needed (max 0 (- left-max-width truncated-width))))
                            (concat truncated-left (make-string pad-needed ?\s))))
             ;; Format timestamp right-aligned in fixed-width field (use string-width for proper alignment)
             ;; Get raw timestamp for property (strip existing properties from formatted timestamp)
             (raw-ts (when timestamp (get-text-property 0 'shipit-raw-timestamp timestamp)))
             (timestamp-str (if (and timestamp (not (string-empty-p timestamp)))
                              (let* ((ts-text (substring-no-properties timestamp))
                                     (ts-width (string-width ts-text))
                                     (pad-needed (max 0 (- 16 ts-width)))
                                     (padded-ts (concat (make-string pad-needed ?\s) ts-text)))
                                ;; Apply both face and raw-timestamp to entire field for proper in-place updates
                                (propertize padded-ts
                                            'font-lock-face 'shipit-timestamp-face
                                            'shipit-raw-timestamp raw-ts))
                            (make-string 16 ?\s))))
        (insert (format "%s%s%s %s" margin-str unread-indicator padded-left timestamp-str)))
      (insert "\n")

      (when has-body
        (magit-insert-section-body
          ;; Insert activity body with support for markdown tables and GFM details blocks
          (shipit--insert-body-with-details body (+ (or indent 0) 3))))

      ;; Add text properties for navigation
      (add-text-properties start (point)
                           `(shipit-activity-event t
                                                   shipit-event-type ,event-type
                                                   shipit-event-id ,event-id
                                                   shipit-activity-timestamp ,created-at
                                                   ,@(when comment-id
                                                       `(shipit-activity-comment-id ,comment-id))
                                                   ,@(when commit-sha
                                                       `(shipit-activity-commit-sha ,commit-sha))
                                                   ,@(when inline-path
                                                       `(shipit-inline-comment-path ,inline-path))
                                                   ,@(when review-state
                                                       `(shipit-review-state ,review-state))
                                                   ,@(when crossref-repo
                                                       `(shipit-crossref-repo ,crossref-repo))
                                                   ,@(when crossref-number
                                                       `(shipit-crossref-number ,crossref-number))
                                                   ,@(when crossref-url
                                                       `(shipit-crossref-url ,crossref-url))
                                                   ,@(when crossref-title
                                                       `(shipit-crossref-title ,crossref-title))
                                                   shipit-repo ,repo
                                                   shipit-pr-number ,pr-number)))))

(defun shipit--pulse-current-line ()
  "Pulse the current line for visual feedback after navigation.
Uses `pulsar-pulse-line' when the pulsar package is available,
otherwise falls back to the built-in `pulse-momentary-highlight-region'."
  (cond
   ((fboundp 'pulsar-pulse-line)
    (with-no-warnings (pulsar-pulse-line)))
   ((fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region
     (line-beginning-position) (line-beginning-position 2)))))

(defun shipit--find-comment-pos (comment-id)
  "Search buffer for comment with COMMENT-ID, return position or nil."
  (let ((found-pos nil)
        (search-pos (point-min)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found-pos) (< search-pos (point-max)))
        (when (equal (get-text-property search-pos 'shipit-comment-id) comment-id)
          (setq found-pos search-pos))
        (setq search-pos (next-single-property-change search-pos 'shipit-comment-id nil (point-max)))))
    found-pos))

(defun shipit--expand-section-by-type (section-type)
  "Find and expand a section of SECTION-TYPE.
Returns the section if found and expanded, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (let ((found-section nil))
      (while (and (not found-section) (not (eobp)))
        (let ((section (magit-current-section)))
          (when (and section (eq (oref section type) section-type))
            (setq found-section section)))
        (forward-line 1))
      (when found-section
        (shipit--debug-log "NAV: Expanding %s section" section-type)
        (magit-section-show found-section)
        found-section))))

(defun shipit--navigate-to-comment-by-id (comment-id &optional no-retry)
  "Navigate to the comment with COMMENT-ID in the buffer.
Searches in both inline comments (files section) and general comments
section.  Automatically expands sections if needed.  In an issue
buffer, on miss, expands pagination via `shipit-issue--load-more-all'
and — if still not found and NO-RETRY is nil — kicks off a buffer
refresh and re-runs navigation once the new fetch settles, so a stale
buffer that's missing a freshly-arrived comment still resolves.
Returns t if found, nil otherwise."
  (shipit--debug-log "NAV: Searching for comment-id: %s (type: %s)" comment-id (type-of comment-id))
  (let ((found-pos (shipit--find-comment-pos comment-id)))
    (unless found-pos
      (shipit--debug-log "NAV: Comment not found, expanding general-comments section")
      (when (shipit--expand-section-by-type 'general-comments)
        (redisplay)
        (setq found-pos (shipit--find-comment-pos comment-id))))
    (unless found-pos
      (shipit--debug-log "NAV: Comment not found in general, expanding pr-files section")
      (when (shipit--expand-pr-files-section)
        (redisplay)
        (setq found-pos (shipit--find-comment-pos comment-id))))
    ;; Issue pagination: rerender without pagination and re-search.
    (unless found-pos
      (when (and (derived-mode-p 'shipit-issue-mode)
                 (fboundp 'shipit-issue--load-more-all))
        (shipit--debug-log "NAV: comment %s not in head/tail, expanding pagination" comment-id)
        (shipit-issue--load-more-all)
        (redisplay)
        (setq found-pos (shipit--find-comment-pos comment-id))))
    (cond
     (found-pos
      (goto-char found-pos)
      (when (fboundp 'magit-section-show)
        (let ((section (magit-current-section)))
          (while section
            (magit-section-show section)
            (setq section (oref section parent)))))
      (recenter)
      (message "Navigated to comment in %s"
               (if (get-text-property found-pos 'shipit-file-path)
                   "files section"
                 "general comments"))
      t)
     ;; Stale issue buffer: the comment exists upstream but isn't in our
     ;; cached state yet.  Trigger a fresh refresh and re-run navigation
     ;; once `shipit-issue-buffer-ready-hook' fires.  NO-RETRY guards
     ;; against an infinite loop when the comment really doesn't exist.
     ((and (not no-retry)
           (derived-mode-p 'shipit-issue-mode)
           (fboundp 'shipit-issue-buffer-refresh))
      (shipit--debug-log "NAV: comment %s missing — refreshing issue buffer and retrying"
                         comment-id)
      (let ((fn-sym (make-symbol "shipit-nav-retry-after-refresh")))
        (fset fn-sym
              (lambda ()
                (remove-hook 'shipit-issue-buffer-ready-hook fn-sym t)
                (shipit--navigate-to-comment-by-id comment-id t)))
        (add-hook 'shipit-issue-buffer-ready-hook fn-sym nil t))
      (shipit-issue-buffer-refresh)
      nil)
     (t
      (message "Comment not found")
      nil))))

(defun shipit--clear-unread-indicator-at-point ()
  "Remove the red dot unread indicator on the current activity line."
  (save-excursion
    (let ((inhibit-read-only t)
          (line-start (line-beginning-position))
          (line-end (line-end-position)))
      (goto-char line-start)
      (when (re-search-forward "● " line-end t)
        (replace-match "  " t t)))))

(defun shipit--mark-commit-as-read (sha)
  "Mark commit SHA as read and clear its unread indicator.
Uses the same activity tracking as the Activity section."
  (let ((pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr)))
    (when (and pr-number repo sha)
      ;; Mark the commit as read in activity tracking
      (shipit--mark-activity-read repo pr-number sha)
      (shipit--debug-log "COMMIT-READ: Marked commit %s as read" sha)
      ;; Clear the visual indicator in the Commits section
      (save-excursion
        (let ((commits-section (shipit--find-section-by-type 'pr-commits)))
          (when commits-section
            (goto-char (oref commits-section start))
            (let ((section-end (oref commits-section end))
                  (inhibit-read-only t))
              ;; Search for the commit line with this SHA
              (while (< (point) section-end)
                (when (equal (get-text-property (point) 'shipit-commit-sha) sha)
                  ;; Found the commit, clear the red dot on this line
                  (shipit--clear-unread-indicator-at-point)
                  (shipit--debug-log "COMMIT-READ: Cleared indicator for commit %s" sha))
                (forward-line 1)))))
        ;; Update the Commits section header indicator
        (shipit--update-commits-header-indicator)))))

(defun shipit--update-commits-header-indicator ()
  "Update the Commits section header's unread indicator.
Removes the red dot if all commits are now read."
  (let ((pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr)))
    (when (and pr-number repo)
      (let ((section (shipit--find-section-by-type 'pr-commits)))
        (when section
          (let* ((read-activities (shipit--get-read-activities repo pr-number))
                 (unread-count 0))
            ;; Count commits that are not read
            (save-excursion
              (goto-char (oref section start))
              (let ((section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m))))
                (while (< (point) section-end)
                  (let ((commit-sha (get-text-property (point) 'shipit-commit-sha)))
                    (when (and commit-sha
                               read-activities
                               (not (shipit--is-activity-read-p repo pr-number commit-sha)))
                      (setq unread-count (1+ unread-count))))
                  (forward-line 1))))
            ;; Update the header indicator
            (shipit--update-section-header-indicator section (> unread-count 0))))))))

(defun shipit--update-activity-header-indicator ()
  "Update the Activity section header's unread indicator.
Removes the red dot if all activities are now read."
  (let ((pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr)))
    (when (and pr-number repo)
      (let ((section (shipit--find-section-by-type 'pr-activity)))
        (when section
          (let* ((read-activities (shipit--get-read-activities repo pr-number))
                 ;; Count unread activities by scanning the section
                 (unread-count 0))
            ;; Count events with shipit-event-id that are not read
            (save-excursion
              (goto-char (oref section start))
              (let ((section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m))))
                (while (< (point) section-end)
                  (let ((event-id (get-text-property (point) 'shipit-event-id)))
                    (when (and event-id
                               read-activities
                               (not (shipit--is-activity-read-p repo pr-number event-id)))
                      (setq unread-count (1+ unread-count))))
                  (goto-char (or (next-single-property-change (point) 'shipit-event-id)
                                 section-end)))))
            ;; Update the header indicator
            (save-excursion
              (let* ((section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
                     (header-end (or (oref section content) (oref section end))))
                ;; Guard against invalid bounds during buffer refresh
                (when (and section-start header-end
                           (< section-start header-end)
                           (<= header-end (point-max)))
                  (goto-char section-start)
                  (let ((inhibit-read-only t))
                    ;; Look for the red dot in the header area
                    (when (re-search-forward " ●" header-end t)
                      (if (zerop unread-count)
                          ;; No more unread - remove the indicator
                          (replace-match "" t t)
                        ;; Still have unread - keep the indicator
                        nil))))))))))))

(defun shipit--update-section-header-indicator (section has-unread)
  "Update the unread indicator on SECTION header.
If HAS-UNREAD is non-nil, add red dot. Otherwise remove it if present."
  (save-excursion
    (let* ((section-start (let ((m (oref section start))) (if (markerp m) (marker-position m) m)))
           (header-end (or (oref section content) (oref section end))))
      ;; Guard against invalid bounds during buffer refresh
      (when (and section-start header-end
                 (< section-start header-end)
                 (<= header-end (point-max)))
        (goto-char section-start)
        (let ((inhibit-read-only t))
          (shipit--debug-log "SECTION-INDICATOR: section-start=%s header-end=%s has-unread=%s"
                             section-start header-end has-unread)
          (if (re-search-forward " ●" header-end t)
              ;; Found existing indicator - remove if not needed
              (unless has-unread
                (shipit--debug-log "SECTION-INDICATOR: Removing existing indicator")
                (delete-region (match-beginning 0) (match-end 0)))
            ;; No indicator - add one if needed
            (when has-unread
              (goto-char section-start)
              ;; Insert at end of first line (before newline)
              (end-of-line)
              (shipit--debug-log "SECTION-INDICATOR: Inserting indicator at pos %s" (point))
              (insert (propertize " ●" 'font-lock-face '(:foreground "red"))))))))))

(defun shipit--update-section-unread-indicators ()
  "Update unread indicators on section headers based on unread activity types.
Adds or removes red dot indicators on sections like General Comments, Files Changed, etc."
  (let ((sections-with-unread shipit--sections-with-unread)
        (inhibit-read-only t))
    (shipit--debug-log "UPDATE-SECTION-INDICATORS: sections-with-unread=%S" sections-with-unread)
    (dolist (section-spec '((general-comments . "General Comments")
                            (pr-files . "Files Changed")
                            (pr-commits . "Commits")
                            (reviewers . "Reviewers")
                            (labels . "Labels")
                            (assignees . "Assignees")))
      (let* ((section-type (car section-spec))
             (section (shipit--find-section-by-type section-type))
             (has-unread (memq section-type sections-with-unread)))
        (shipit--debug-log "UPDATE-SECTION-INDICATORS: checking %s section=%s has-unread=%s"
                           section-type (if section "found" "not-found") has-unread)
        (when section
          (shipit--update-section-header-indicator section has-unread))))))

(defun shipit--update-all-section-indicators ()
  "Update all section unread indicators after marking activities as read.
Recalculates which sections should have indicators based on remaining unread activities."
  (let ((pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr)))
    (when (and pr-number repo)
      ;; Recalculate which sections have unread activities
      (let ((activity-section (shipit--find-section-by-type 'pr-activity)))
        (when activity-section
          (let ((events '())
                (read-activities (shipit--get-read-activities repo pr-number)))
            ;; Collect all events from the activity section
            (save-excursion
              (goto-char (oref activity-section start))
              (let ((section-end (oref activity-section end)))
                (while (< (point) section-end)
                  (let ((event-id (get-text-property (point) 'shipit-event-id))
                        (event-type (get-text-property (point) 'shipit-event-type)))
                    (when (and event-id event-type)
                      (push `((id . ,event-id) (event . ,event-type)) events)))
                  (goto-char (or (next-single-property-change (point) 'shipit-event-id)
                                 section-end)))))
            ;; Calculate sections with unread using the collected events
            (setq shipit--sections-with-unread
                  (shipit--get-sections-with-unread-activities repo pr-number events))
            ;; Update the visual indicators
            (shipit--update-section-unread-indicators)))))))

(defun shipit--find-review-comment-pos (review-id)
  "Search buffer for any comment with REVIEW-ID, return position or nil."
  (let ((found-pos nil)
        (search-pos (point-min)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found-pos) (< search-pos (point-max)))
        (let ((prop-review-id (get-text-property search-pos 'shipit-review-id)))
          (when (equal prop-review-id review-id)
            (setq found-pos search-pos)))
        (setq search-pos (next-single-property-change search-pos 'shipit-review-id nil (point-max)))))
    found-pos))

(defun shipit--expand-pr-files-section ()
  "Find and expand the pr-files (Files Changed) section.
Returns the section if found and expanded, nil otherwise.
Also expands each file's Outdated Comments subsection so that
review-id text properties on outdated comments become searchable."
  (save-excursion
    (goto-char (point-min))
    (let ((found-section nil))
      (while (and (not found-section) (not (eobp)))
        (let ((section (magit-current-section)))
          (when (and section (eq (oref section type) 'pr-files))
            (setq found-section section)))
        (forward-line 1))
      (when found-section
        (shipit--debug-log "NAV-REVIEW: Expanding pr-files section")
        (magit-section-show found-section)
        ;; Expand each file section to run its washer and populate the body
        ;; (inline comments + outdated-comments placeholder).  Then expand the
        ;; outdated-comments grandchild so outdated review comments' text
        ;; properties (shipit-review-id) exist in the buffer and can be found.
        (dolist (child (oref found-section children))
          (when (eq (oref child type) 'pr-file)
            (magit-section-show child)
            (dolist (grandchild (oref child children))
              (when (eq (oref grandchild type) 'shipit-outdated-comments)
                (magit-section-show grandchild)))))
        found-section))))

(defun shipit--find-activity-event-pos (comment-id)
  "Search buffer for activity event with shipit-activity-comment-id = COMMENT-ID.
Returns buffer position or nil."
  (let ((found-pos nil)
        (search-pos (point-min)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found-pos) (< search-pos (point-max)))
        (when (equal (get-text-property search-pos 'shipit-activity-comment-id) comment-id)
          (setq found-pos search-pos))
        (setq search-pos
              (next-single-property-change
               search-pos 'shipit-activity-comment-id nil (point-max)))))
    found-pos))

(defun shipit--navigate-to-activity-event (comment-id)
  "Navigate to activity event with COMMENT-ID in the Activity section.
Expands `pr-activity' so events render, searches the buffer, and
positions point on the matching event line.  Returns t on success."
  (shipit--debug-log "NAV-ACTIVITY-EVENT: Searching for comment-id: %s" comment-id)
  (when (shipit--expand-section-by-type 'pr-activity)
    (redisplay)
    (let ((found-pos (shipit--find-activity-event-pos comment-id)))
      (when found-pos
        (goto-char found-pos)
        (when (fboundp 'magit-section-show)
          (let ((section (magit-current-section)))
            (while section
              (magit-section-show section)
              (setq section (oref section parent)))))
        (recenter)
        t))))

(defun shipit--navigate-to-review-comment (review-id)
  "Navigate to any comment belonging to REVIEW-ID in the buffer.
Review comments have shipit-review-id property linking them to their review.
Automatically expands Files Changed section if needed.  When the review has
no inline comments, falls back to the matching review event in the Activity
section."
  (shipit--debug-log "NAV-REVIEW: Searching for review-id: %s (type: %s)" review-id (type-of review-id))
  (let ((found-pos (shipit--find-review-comment-pos review-id)))
    ;; If not found, try expanding the Files Changed section
    (unless found-pos
      (shipit--debug-log "NAV-REVIEW: Comment not found, expanding Files Changed section")
      (when (shipit--expand-pr-files-section)
        ;; Give buffer a moment to update, then search again
        (redisplay)
        (setq found-pos (shipit--find-review-comment-pos review-id))))
    (cond
     (found-pos
      (goto-char found-pos)
      ;; Expand parent sections to make the comment visible
      (when (fboundp 'magit-section-show)
        (let ((section (magit-current-section)))
          (while section
            (magit-section-show section)
            (setq section (oref section parent)))))
      (recenter)
      (message "Navigated to review comment")
      t)
     ;; Fallback: navigate to the review event in the Activity section
     ;; (e.g., a review body without inline comments).
     ((shipit--navigate-to-activity-event review-id)
      (message "Navigated to review entry in Activity section")
      t)
     (t
      (message "Review not found in buffer")
      nil))))

(defun shipit--navigate-to-approval-section ()
  "Navigate to the Approval section in the buffer."
  (shipit--debug-log "NAV-APPROVAL: Navigating to Approval section")
  (let ((section (shipit--find-section-by-type 'approval)))
    (if section
        (progn
          (goto-char (oref section start))
          (magit-section-show section)
          (recenter)
          (message "Navigated to Approval section")
          t)
      (message "Approval section not found")
      nil)))

(defun shipit--find-commit-pos (sha)
  "Search buffer for commit with SHA, return position or nil."
  (let ((found-pos nil)
        (search-pos (point-min)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found-pos) (< search-pos (point-max)))
        (let ((prop-sha (get-text-property search-pos 'shipit-commit-sha)))
          (when (and prop-sha (string-prefix-p sha prop-sha))
            (setq found-pos search-pos)))
        (setq search-pos (next-single-property-change search-pos 'shipit-commit-sha nil (point-max)))))
    found-pos))

(defun shipit--navigate-to-commit (sha)
  "Navigate to the commit with SHA in the buffer.
Automatically expands Commits section if needed."
  (shipit--debug-log "NAV-COMMIT: Searching for commit SHA: %s" sha)
  (let ((found-pos (shipit--find-commit-pos sha)))
    ;; If not found, try expanding the Commits section
    (unless found-pos
      (shipit--debug-log "NAV-COMMIT: Commit not found, expanding pr-commits section")
      (when (shipit--expand-section-by-type 'pr-commits)
        (redisplay)
        (setq found-pos (shipit--find-commit-pos sha))))
    (if found-pos
        (progn
          (goto-char found-pos)
          ;; Expand parent sections to make the commit visible
          (when (fboundp 'magit-section-show)
            (let ((section (magit-current-section)))
              (while section
                (magit-section-show section)
                (setq section (oref section parent)))))
          (recenter)
          (message "Navigated to commit %s" (substring sha 0 7))
          t)
      (message "Commit not found")
      nil)))

(defun shipit--navigate-to-inline-comment-file (file-path comment-id)
  "Navigate to inline comment in FILE-PATH within current buffer.
Expands Files Changed section, finds the file, and positions cursor at the comment."
  (shipit--debug-log "NAV-INLINE: Navigating to file %s comment %s" file-path comment-id)
  ;; First expand the Files Changed section
  (let ((files-section (shipit--expand-pr-files-section)))
    (if files-section
        (progn
          ;; Find and expand the specific file section
          (let ((file-pos (shipit--find-file-in-pr-files file-path)))
            (if file-pos
                (progn
                  (goto-char file-pos)
                  ;; Expand the file section to load comments
                  (let ((file-section (magit-current-section)))
                    (when file-section
                      (magit-section-show file-section)
                      ;; Give it a moment to render comments, then find the specific comment
                      (redisplay)
                      (let ((comment-pos (shipit--find-inline-comment-in-file file-section comment-id)))
                        (if comment-pos
                            (progn
                              (goto-char comment-pos)
                              (recenter)
                              (message "Navigated to inline comment"))
                          (message "Navigated to file - comment may be collapsed"))))))
              (message "File %s not found in Files Changed" file-path))))
      (message "Files Changed section not found"))))

(defun shipit--find-file-in-pr-files (file-path)
  "Find FILE-PATH in the Files Changed section, return position or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((found-pos nil))
      (while (and (not found-pos) (not (eobp)))
        ;; shipit-pr-file is a boolean marker, actual path is in shipit-file-path
        (let ((prop-file (get-text-property (point) 'shipit-file-path)))
          (when (and prop-file (string= prop-file file-path))
            (setq found-pos (point))))
        (goto-char (or (next-single-property-change (point) 'shipit-file-path)
                       (point-max))))
      found-pos)))

(defun shipit--find-inline-comment-in-file (file-section comment-id)
  "Find inline comment with COMMENT-ID within FILE-SECTION, return position or nil."
  (when file-section
    (let ((section-start (oref file-section start))
          (section-end (oref file-section end))
          (found-pos nil))
      (save-excursion
        (goto-char section-start)
        (while (and (not found-pos) (< (point) section-end))
          (let ((prop-id (get-text-property (point) 'shipit-comment-id)))
            (when (equal prop-id comment-id)
              (setq found-pos (point))))
          (goto-char (or (next-single-property-change (point) 'shipit-comment-id)
                         section-end))))
      found-pos)))

(defun shipit--find-inline-comment-pos (comment-id)
  "Search buffer for inline comment with COMMENT-ID, return position or nil."
  (let ((found-pos nil)
        (search-pos (point-min)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found-pos) (< search-pos (point-max)))
        (let ((prop-id (get-text-property search-pos 'shipit-comment-id)))
          (when (equal prop-id comment-id)
            (setq found-pos search-pos)))
        (setq search-pos (next-single-property-change search-pos 'shipit-comment-id nil (point-max)))))
    found-pos))

;;; Lazy-load commit files

;; Try to register hook for lazy-loading when sections are expanded
;; Note: magit-section-visibility-changed-hook may not exist in all versions
;;;; Lazy-loading for commit files using magit's washer mechanism

(defun shipit--create-commit-files-washer (repo commit-sha section pr-number)
  "Create a washer function for lazy-loading commit files.
REPO is the repository string (owner/repo).
COMMIT-SHA is the commit SHA.
SECTION is the magit section object that will contain the files.
PR-NUMBER is the PR number for looking up comment counts.
Fetches files via backend when the section is first shown."
  (lambda ()
    "Washer function called by magit when the section is first expanded."
    (condition-case err
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (response (funcall (plist-get backend :fetch-commit) config commit-sha))
               (files (when response (cdr (assq 'files response)))))
          (when files
            (shipit--render-commit-files-section-body section files pr-number)))
      (error
       (shipit--debug-log "washer: ERROR: %S" err)))))

(defun shipit--get-local-commit-file-patch (commit-sha filename)
  "Get the diff patch for FILENAME in COMMIT-SHA.
Uses --root to handle the initial commit."
  (shell-command-to-string
   (format "git diff-tree --root -p %s -- %s 2>/dev/null"
           commit-sha (shell-quote-argument filename))))

(defun shipit--get-local-commit-files (commit-sha)
  "Get files changed in COMMIT-SHA from local git.
Returns a list of alists compatible with shipit file rendering.
Uses --root flag to handle the initial commit (root commit with no parent)."
  (let* ((output (shell-command-to-string
                  (format "git diff-tree --no-commit-id --numstat -r --root %s 2>/dev/null" commit-sha)))
         (lines (split-string output "\n" t))
         (files nil))
    (dolist (line lines)
      (let* ((parts (split-string line "\t"))
             (additions (string-to-number (or (nth 0 parts) "0")))
             (deletions (string-to-number (or (nth 1 parts) "0")))
             (filename (nth 2 parts)))
        (when filename
          (push `((filename . ,filename)
                  (status . "modified")
                  (additions . ,additions)
                  (deletions . ,deletions)
                  (patch . ,(shipit--get-local-commit-file-patch commit-sha filename)))
                files))))
    (nreverse files)))

(defun shipit--create-local-commit-files-washer (commit-sha section)
  "Create a washer function for lazy-loading commit files from local git.
COMMIT-SHA is the commit SHA.
SECTION is the magit section object that will contain the files."
  (lambda ()
    "Washer function called by magit when the section is first expanded."
    (condition-case err
        (let ((files (shipit--get-local-commit-files commit-sha)))
          (if files
              (shipit--render-commit-files-section-body section files nil)
            (insert (propertize "              No files in this commit\n"
                                'font-lock-face 'font-lock-comment-face))))
      (error
       (shipit--debug-log "local-washer: ERROR: %S" err)
       (insert (propertize (format "              Error: %s\n" (error-message-string err))
                           'font-lock-face 'error))))))

;; Legacy: Using local-post-command-hook as a fallback to detect section changes
(defvar shipit--last-section-at-point nil
  "Track the last section at point to detect section changes.")

(defun shipit--check-section-expansion ()
  "Check if a commit-files section was just expanded and load files if needed."
  ;; Guard: only run if magit functions are available
  (when (fboundp 'magit-current-section)
    (ignore-errors  ; Defensive: if magit isn't fully loaded, don't crash
      (let ((current-section (magit-current-section)))
        (when current-section
          (let ((stype (magit-section-type current-section)))
            (when (and (eq stype 'commit-files)
                       (not (magit-section-hidden current-section))
                       (not (equal shipit--last-section-at-point current-section)))
              (setq shipit--last-section-at-point current-section)
              (shipit--handle-commit-files-section-visibility-changed current-section t))))))))

(add-hook 'post-command-hook #'shipit--check-section-expansion)

(defun shipit--fetch-commit-files (repo commit-sha)
  "Fetch files changed in a specific commit.
REPO is the repo string (owner/name).
COMMIT-SHA is the commit SHA.
Returns a list of file objects with filename, status, additions, deletions.
Dispatches to the active PR backend's :fetch-commit."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fetch-fn (plist-get backend :fetch-commit))
         (response (funcall fetch-fn config commit-sha)))
    (when response
      (cdr (assq 'files response)))))

(defun shipit--handle-commit-files-section-visibility-changed (section visible)
  "Handle visibility change for commit-files section.
SECTION is the magit section object.
VISIBLE is t when expanded, nil when collapsed."
  (when (and visible (eq (magit-section-type section) 'commit-files))
    ;; Get the commit SHA from the section value
    (let* ((commit-sha (magit-section-value section))
           ;; Get parent (commit) section to access repo and pr-number
           (parent-section (magit-section-parent section))
           (repo (and parent-section
                      (get-text-property (magit-section-start parent-section) 'shipit-repo))))
      (when repo
        (let ((files (shipit--fetch-commit-files repo commit-sha)))
          (when files
            ;; Re-render the section body with the fetched files
            (shipit--render-commit-files-section-body section files)))))))

(defun shipit--insert-commit-file-diff (file-patch filename &optional commit-sha)
  "Insert diff for a commit file with proper indentation for the commit context.
Adds 11 spaces of indentation to align with commit file headings (14 total spaces).
FILE-PATCH is the patch text from GitHub API.
FILENAME is the file path.
Optional COMMIT-SHA filters inline comments to those made on that commit."
  (let ((start-pos (point))
        (repo (shipit--get-repo-from-remote))
        (pr-number (when (and (boundp 'shipit--current-displayed-pr)
                              shipit--current-displayed-pr
                              (listp shipit--current-displayed-pr))
                     (car shipit--current-displayed-pr))))
    ;; Insert the diff using standard function (adds 3 spaces indentation)
    ;; Pass commit-sha to filter comments to this specific commit
    (shipit--insert-file-diff file-patch filename 0 repo pr-number commit-sha)
    ;; Add extra indentation (11 spaces) to each line to reach 14 total spaces
    (let ((end-pos (point)))
      (goto-char start-pos)
      (while (< (point) end-pos)
        ;; Add 11 spaces at the beginning of each line
        (insert "           ")  ; 11 spaces
        ;; Move to next line
        (forward-line)
        ;; Update end-pos since we added characters
        (setq end-pos (+ end-pos 11))))))

(defun shipit--render-commit-files-section-body (section files pr-number)
  "Render the body of a commit-files section with the given files.
SECTION is the magit section object.
FILES is the list of file objects to display.
PR-NUMBER is the PR number for looking up comment counts.

Uses proper magit section safe update pattern:
- Only delete content between 'content' and 'end'
- Insert at 'content' marker
- Update 'end' marker after insertion"
  (condition-case err
      (let ((content-start (oref section content))
            (content-end (oref section end))
            (commit-sha (oref section value)))
        (shipit--debug-log "render-files-body: content-start=%s content-end=%s" content-start content-end)
        (when (and content-start content-end)
          ;; SAFE UPDATE: Only delete the body content area
          (delete-region content-start content-end)
          ;; Position at the start of the deleted content
          (goto-char content-start)
          ;; Insert new file content as individual magit sections
          (dolist (file files)
            (let* ((filename (cdr (assq 'filename file)))
                   (status (cdr (assq 'status file)))
                   (additions (cdr (assq 'additions file)))
                   (deletions (cdr (assq 'deletions file)))
                   (patch (cdr (assq 'patch file)))
                   (comment-count (shipit--get-file-comment-count pr-number filename))
                   (outdated-count (shipit--get-file-outdated-comment-count pr-number filename))
                   (resolved-count (shipit--get-file-resolved-comment-count pr-number filename))
                   (comment-indicator (shipit--get-comment-indicator comment-count outdated-count resolved-count))
                   (status-char (cond ((equal status "added") "A")
                                      ((equal status "deleted") "D")
                                      ((equal status "modified") "M")
                                      ((equal status "renamed") "R")
                                      (t "?"))))
              ;; Create a magit section for each file
              ;; Define properties once, apply to both heading and body
              (let ((file-props `(shipit-commit-file t
                                                     shipit-file-path ,filename
                                                     shipit-commit-sha ,commit-sha
                                                     shipit-file-status ,status
                                                     shipit-comment-count ,comment-count
                                                     shipit-patch ,patch

                                                     help-echo "RET: open at commit, SPC: show diff, e: ediff, TAB: toggle")))
                (magit-insert-section (commit-file filename t)
                  ;; Apply properties to heading
                  (let ((heading-start (point))
                        (adds (or additions 0))
                        (dels (or deletions 0)))
                    (magit-insert-heading (format "              %s %s%s (%s %s)"
                                                  (propertize status-char 'font-lock-face 'magit-diff-file-heading)
                                                  (propertize (or filename "unknown") 'font-lock-face 'shipit-filename-face)
                                                  comment-indicator
                                                  (if (> adds 0)
                                                      (propertize (format "+%d" adds) 'font-lock-face 'diff-added)
                                                    "+0")
                                                  (if (> dels 0)
                                                      (propertize (format "-%d" dels) 'font-lock-face 'diff-removed)
                                                    "-0")))
                    (add-text-properties heading-start (point) file-props))
                  ;; Body is lazily rendered - apply properties when actually inserted
                  (when patch
                    (magit-insert-section-body
                      (let ((body-start (point)))
                        ;; Pass commit-sha to show comments specific to this commit
                        (shipit--insert-commit-file-diff patch filename commit-sha)
                        (add-text-properties body-start (point) file-props))))))))
          ;; UPDATE: Set the section's end marker to current position
          (oset section end (point-marker))))
    (error
     (shipit--debug-log "render-files-body ERROR: %S" err))))

;;; Comment unread indicator tracking
;; These functions handle marking individual comments as read when cursor visits them

(defvar shipit--last-comment-id-at-point nil
  "The comment ID at point during the last post-command-hook run.
Used to avoid redundant marking and visual updates.")

(defun shipit--clear-comment-unread-indicator-at-point ()
  "Remove the red dot unread indicator on the current comment header line.
Searches for the red dot character and replaces it with empty string."
  (save-excursion
    (let ((inhibit-read-only t)
          (comment-id (get-text-property (point) 'shipit-comment-id))
          (current-pos (point)))
      (when comment-id
        ;; Find the start of this comment's text (find the region with this comment-id)
        (let* ((region-start (or (previous-single-property-change current-pos 'shipit-comment-id)
                                 (point-min)))
               (region-end (or (next-single-property-change current-pos 'shipit-comment-id)
                               (point-max))))
          ;; Ensure valid search bounds (region-start must be < region-end for re-search-forward)
          (when (< region-start region-end)
            ;; Search for red dot in this comment region
            (goto-char region-start)
            (when (re-search-forward " ●" region-end t)
              ;; Only remove if it has our unread marker property
              (when (get-text-property (match-beginning 0) 'shipit-unread-comment)
                (delete-region (match-beginning 0) (match-end 0))))))))))

(defun shipit--update-general-comments-header-indicator ()
  "Update the General Comments section header unread indicator.
Removes the red dot if all comments in the section are read."
  (let ((section (shipit--find-section-by-type 'general-comments)))
    (when section
      (let ((pr-number (car-safe shipit--current-displayed-pr))
            (repo (cadr shipit--current-displayed-pr)))
        (when (and pr-number repo)
          (let ((unread-count 0))
            ;; Count unread comments by scanning the section
            (save-excursion
              (goto-char (oref section start))
              (let ((section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m))))
                (while (< (point) section-end)
                  (let ((comment-id (get-text-property (point) 'shipit-comment-id)))
                    (when (and comment-id
                               (shipit--is-comment-unread-p repo pr-number comment-id))
                      (setq unread-count (1+ unread-count))))
                  (goto-char (or (next-single-property-change (point) 'shipit-comment-id)
                                 section-end)))))
            ;; Update the header indicator
            (shipit--update-section-header-indicator section (> unread-count 0))))))))

(defun shipit--update-files-section-comment-indicators ()
  "Update the Files Changed section header based on unread inline comments.
Removes the red dot if all inline comments are read."
  (let ((section (shipit--find-section-by-type 'pr-files)))
    (when section
      (let ((pr-number (car-safe shipit--current-displayed-pr))
            (repo (cadr shipit--current-displayed-pr)))
        (when (and pr-number repo)
          (let ((unread-count 0))
            ;; Count unread comments by scanning the section
            (save-excursion
              (goto-char (oref section start))
              (let ((section-end (let ((m (oref section end))) (if (markerp m) (marker-position m) m))))
                (while (< (point) section-end)
                  (let ((comment-id (get-text-property (point) 'shipit-comment-id)))
                    (when (and comment-id
                               (shipit--is-comment-unread-p repo pr-number comment-id))
                      (setq unread-count (1+ unread-count))))
                  (goto-char (or (next-single-property-change (point) 'shipit-comment-id)
                                 section-end)))))
            ;; Update the header indicator
            (shipit--update-section-header-indicator section (> unread-count 0))))))))

(defun shipit--clear-activity-indicator-for-comment (comment-id)
  "Clear the activity indicator for the activity event linked to COMMENT-ID.
For general comments, COMMENT-ID equals the event-id, so we always mark the
activity as read. Also searches the buffer to clear the visual indicator if
the Activity section is currently expanded."
  (shipit--debug-log "CLEAR-ACTIVITY: Looking for comment-id=%s (type: %s)" comment-id (type-of comment-id))
  (let ((pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr)))
    (when (and pr-number repo)
      ;; For general comments, the comment-id IS the event-id, so always mark
      ;; the activity as read even if the Activity section is collapsed.
      ;; This ensures when the section is later expanded, the activity won't
      ;; show as unread.
      (shipit--mark-activity-read repo pr-number comment-id)
      (shipit--debug-log "CLEAR-ACTIVITY: Marked activity %s as read (direct)" comment-id)
      ;; Now search the buffer to clear the visual indicator if visible
      (save-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          ;; Search entire buffer for matching activity-comment-id
          (while (< (point) (point-max))
            (let ((activity-comment-id (get-text-property (point) 'shipit-activity-comment-id)))
              (when (and activity-comment-id (equal activity-comment-id comment-id))
                (shipit--debug-log "CLEAR-ACTIVITY: Found visible activity for comment %s at pos %s"
                                   comment-id (point))
                ;; Clear the visual indicator on this line
                (let ((line-start (line-beginning-position))
                      (line-end (line-end-position)))
                  (when (< line-start line-end)
                    (save-excursion
                      (goto-char line-start)
                      (when (re-search-forward "● " line-end t)
                        (replace-match "  " t t)))))))
            (let ((next-pos (next-single-property-change (point) 'shipit-activity-comment-id)))
              (goto-char (or next-pos (point-max)))))))
      ;; Update activity header indicator
      (shipit--update-activity-header-indicator))))

(defun shipit--mark-comment-read-at-point ()
  "Mark the comment at point as read and update visual indicators."
  (let ((comment-id (get-text-property (point) 'shipit-comment-id))
        (pr-number (car-safe shipit--current-displayed-pr))
        (repo (cadr shipit--current-displayed-pr)))
    (when comment-id
      (shipit--debug-log "MARK-READ: comment-id=%s pr=%s repo=%s last-id=%s is-unread=%s"
                         comment-id pr-number repo shipit--last-comment-id-at-point
                         (and pr-number repo (shipit--is-comment-unread-p repo pr-number comment-id))))
    (when (and comment-id pr-number repo
               ;; Only process if this is a different comment than last time
               (not (equal comment-id shipit--last-comment-id-at-point))
               ;; Only process if comment is actually unread
               (shipit--is-comment-unread-p repo pr-number comment-id))
      (shipit--debug-log "MARK-READ: Processing comment %s as read" comment-id)
      (setq shipit--last-comment-id-at-point comment-id)
      ;; Mark comment as read in persistent cache
      (shipit--mark-comment-read repo pr-number comment-id)
      ;; Clear the visual indicator on the comment
      (shipit--clear-comment-unread-indicator-at-point)
      ;; Clear the corresponding activity indicator
      (shipit--clear-activity-indicator-for-comment comment-id)
      ;; Update parent section headers
      (shipit--update-general-comments-header-indicator)
      (shipit--update-files-section-comment-indicators))))

(defun shipit--show-reaction-tooltip ()
  "Show reaction authors in minibuffer when cursor is on a reaction emoji."
  (let ((tooltip (get-text-property (point) 'shipit-reaction-tooltip)))
    (when tooltip
      (message "%s" tooltip))))

(defun shipit--comment-read-post-command-hook ()
  "Post-command hook to mark comments as read and show reaction tooltips."
  (when (derived-mode-p 'shipit-mode)
    (condition-case err
        (progn
          (when shipit-show-unread-indicators
            (shipit--mark-comment-read-at-point))
          (shipit--show-reaction-tooltip))
      (error (shipit--debug-log "POST-COMMAND-HOOK ERROR: %s" err)))))

(defun shipit--emojify-help-with-reaction-tooltip (orig-fn window string pos)
  "Advice around `emojify-help-function' to show reaction authors on hover.
When the mouse is over an emojified reaction that has a
`shipit-reaction-tooltip' text property, return that tooltip
instead of the default emoji name."
  (or (get-text-property pos 'shipit-reaction-tooltip)
      (funcall orig-fn window string pos)))

(with-eval-after-load 'emojify
  (advice-add 'emojify-help-function :around #'shipit--emojify-help-with-reaction-tooltip))

;;; Diff-hunk relayer (in-place re-render, no API refetch)

(defun shipit--walk-sections (section fn)
  "Call FN with SECTION and each of its descendants in pre-order."
  (funcall fn section)
  (dolist (child (oref section children))
    (shipit--walk-sections child fn)))

(defun shipit--diff-hunk-filename (section)
  "Return the filename associated with diff-hunk SECTION.
Walks up the section tree to the enclosing pr-file section and
returns its value slot, or nil if not found."
  (let ((parent (oref section parent)))
    (while (and parent (not (eq (oref parent type) 'pr-file)))
      (setq parent (oref parent parent)))
    (and parent (oref parent value))))

(defun shipit--diff-hunk-line-ranges (body-start body-end)
  "Collect (CONTENT-START CONTENT-END PREFIX-CHAR) for each diff line in BODY-START..BODY-END.
Only lines matching shipit's `     +', `     -', or `      ' gutter are
returned, so inline comments interleaved in the hunk body are skipped."
  (let ((ranges nil))
    (save-excursion
      (goto-char body-start)
      (while (< (point) body-end)
        (let ((lstart (line-beginning-position))
              (lend (line-end-position)))
          (when (and (>= (- lend lstart) 6)
                     (string= (buffer-substring-no-properties
                               lstart (+ lstart 5))
                              "     ")
                     (memq (char-after (+ lstart 5)) '(?+ ?- ?\s)))
            (push (list (+ lstart 6) lend (char-after (+ lstart 5)))
                  ranges)))
        (forward-line 1)))
    (nreverse ranges)))

(defun shipit--reset-diff-hunk-content-faces (ranges)
  "Reset `font-lock-face' on each range in RANGES to the base diff face.
Each entry is (CONTENT-START CONTENT-END PREFIX-CHAR).  Used to peel
off any language-syntax face layer before re-applying or clearing."
  (let ((inhibit-read-only t))
    (dolist (r ranges)
      (let* ((start (nth 0 r))
             (end (nth 1 r))
             (prefix (nth 2 r))
             (base (cond ((eq prefix ?+) 'magit-diff-added)
                         ((eq prefix ?-) 'magit-diff-removed)
                         (t 'magit-diff-context))))
        (put-text-property start end 'font-lock-face base)))))

(defun shipit--remove-diff-refine-overlays (body-start body-end)
  "Delete `diff-refine-{added,removed}' overlays between BODY-START and BODY-END."
  (dolist (ov (overlays-in body-start body-end))
    (when (memq (overlay-get ov 'face) '(diff-refine-added diff-refine-removed))
      (delete-overlay ov))))

(defun shipit--relayer-diff-hunk-section (section)
  "Strip existing fontify/refine layers on SECTION and re-apply per current defcustoms."
  (let* ((content-m (oref section content))
         (end-m (oref section end))
         (body-start (and content-m (if (markerp content-m)
                                        (marker-position content-m)
                                      content-m)))
         (body-end (and end-m (if (markerp end-m)
                                  (marker-position end-m)
                                end-m))))
    (when (and body-start body-end (< body-start body-end))
      (shipit--remove-diff-refine-overlays body-start body-end)
      (let ((ranges (shipit--diff-hunk-line-ranges body-start body-end))
            (filename (shipit--diff-hunk-filename section)))
        (shipit--reset-diff-hunk-content-faces ranges)
        (when (and shipit-pr-fontify-hunks ranges filename)
          (shipit--fontify-diff-hunk-lines ranges filename))
        (when shipit-pr-refine-hunks
          (shipit--refine-diff-hunk body-start body-end))))))

(defun shipit--relayer-all-diff-hunks ()
  "Re-apply diff visual layers on every diff-hunk section in the current buffer.
No API calls; walks `magit-root-section' and rewrites font-lock-face /
refine overlays in place per current defcustoms."
  (when (and (boundp 'magit-root-section) magit-root-section)
    (let ((inhibit-read-only t))
      (save-excursion
        (shipit--walk-sections
         magit-root-section
         (lambda (s)
           (when (eq (oref s type) 'diff-hunk)
             (shipit--relayer-diff-hunk-section s))))))))

(defun shipit--pr-apply-diff-toggle ()
  "Re-apply diff layers in the current shipit PR buffer, no refetch."
  (when (derived-mode-p 'shipit-mode)
    (shipit--relayer-all-diff-hunks)))

;;;###autoload
(defun shipit-pr-toggle-fontify-hunks ()
  "Toggle language syntax highlighting in PR diff hunks.
Flips `shipit-pr-fontify-hunks' and relayers the current shipit PR
buffer in place (no API refetch)."
  (interactive)
  (setq shipit-pr-fontify-hunks (not shipit-pr-fontify-hunks))
  (message "PR hunk language fontification %s"
           (if shipit-pr-fontify-hunks "enabled" "disabled"))
  (shipit--pr-apply-diff-toggle))

;;;###autoload
(defun shipit-pr-toggle-refine-hunks ()
  "Toggle intra-line refinement in PR diff hunks.
Flips `shipit-pr-refine-hunks' and relayers the current shipit PR
buffer in place (no API refetch)."
  (interactive)
  (setq shipit-pr-refine-hunks (not shipit-pr-refine-hunks))
  (message "PR hunk refinement %s"
           (if shipit-pr-refine-hunks "enabled" "disabled"))
  (shipit--pr-apply-diff-toggle))

(provide 'shipit-pr-sections)
;;; shipit-pr-sections.el ends here
