;;; shipit-sections.el --- Magit section utilities -*- lexical-binding: t; -*-

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
;; Core magit section utilities for shipit.
;; This module provides section finding, navigation, and manipulation helpers.

;;; Code:
(require 'cl-lib)
(require 'shipit-lib)

;; Require magit-section at compile time for macros
(eval-when-compile
  (require 'magit-section))
(require 'magit-section)

;; Forward declarations
(declare-function shipit--debug-log "shipit-debug")

;;; Section Types
;; Define the magit section types used by shipit.
;; These are symbols used with magit-insert-section.

(defconst shipit-section-types
  '(shipit-pr              ; Main PR section in shipit buffer
    shipit-pr-header       ; PR header with title, state, etc.
    pr-commits             ; Commits section
    pr-files               ; Files changed section
    pr-file                ; Individual file entry
    pr-checks              ; CI/checks section
    pr-activity            ; Activity timeline section
    general-comments       ; General comments section
    shipit-comment         ; Individual comment
    shipit-inline-comment  ; Inline comment on code
    labels                 ; Labels section
    reviewers              ; Reviewers section
    assignees              ; Assignees section
    reviews                ; Reviews section
    commit-files)          ; Files changed in a specific commit
  "List of magit section types used by shipit.")

;;; Section Finding

(defun shipit--find-section-by-type (target-type)
  "Find a magit section by its TYPE in the current buffer.
Returns the section object or nil if not found.
Searches the section tree recursively."
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "FIND-SECTION: Looking for %s, magit-root-section=%s"
                       target-type
                       (if (bound-and-true-p magit-root-section) "bound" "NOT-BOUND")))
  (if (bound-and-true-p magit-root-section)
      (let ((result (shipit--find-section-in-tree magit-root-section target-type)))
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "FIND-SECTION: Result=%s" (if result "FOUND" "NOT-FOUND")))
        result)
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "FIND-SECTION: magit-root-section not bound!"))
    nil))

(defun shipit--find-section-in-tree (section target-type)
  "Recursively search SECTION and its children for TARGET-TYPE.
Returns the matching section or nil."
  (cond
   ;; Found it
   ((eq (oref section type) target-type)
    section)
   ;; Search children
   (t
    (let ((found nil)
          (children (oref section children)))
      (while (and children (not found))
        (setq found (shipit--find-section-in-tree (car children) target-type))
        (setq children (cdr children)))
      found))))

(defun shipit--find-all-sections-by-type (target-type)
  "Find all magit sections of TARGET-TYPE in the current buffer.
Returns a list of section objects."
  (when (bound-and-true-p magit-root-section)
    (shipit--collect-sections-in-tree magit-root-section target-type)))

(defun shipit--collect-sections-in-tree (section target-type)
  "Recursively collect all sections of TARGET-TYPE from SECTION tree.
Returns a list of matching sections."
  (let ((results '()))
    (when (eq (oref section type) target-type)
      (push section results))
    (dolist (child (oref section children))
      (setq results (append results (shipit--collect-sections-in-tree child target-type))))
    results))

(defun shipit--find-parent-section-of-type (section target-type)
  "Find the nearest ancestor of SECTION with TARGET-TYPE.
Returns the matching section or nil."
  (when section
    (let ((parent (oref section parent)))
      (cond
       ((null parent) nil)
       ((eq (oref parent type) target-type) parent)
       (t (shipit--find-parent-section-of-type parent target-type))))))

;;; Section Navigation

(defun shipit--section-at-point ()
  "Return the magit section at point, if any."
  (when (fboundp 'magit-current-section)
    (magit-current-section)))

(defun shipit--section-type-at-point ()
  "Return the type of the magit section at point, if any."
  (when-let* ((section (shipit--section-at-point)))
    (oref section type)))

(defun shipit--in-section-type-p (target-type)
  "Return t if point is within a section of TARGET-TYPE or its descendants."
  (when-let* ((section (shipit--section-at-point)))
    (or (eq (oref section type) target-type)
        (shipit--find-parent-section-of-type section target-type))))

;;; Section Manipulation

(defun shipit--delete-section (section)
  "Delete SECTION from the buffer and remove from parent's children.
Returns the position where the section was."
  (when section
    (let* ((inhibit-read-only t)
           (start (oref section start))
           (end (oref section end))
           (parent (oref section parent)))
      ;; Remove from parent's children list
      (when parent
        (oset parent children
              (seq-remove (lambda (s) (eq s section))
                          (oref parent children))))
      ;; Delete from buffer
      (when (and start end)
        (delete-region start end))
      start)))

(defun shipit--replace-section-content (section insert-fn)
  "Replace content of SECTION by calling INSERT-FN at the content position.
SECTION must have start, content, and end markers.
INSERT-FN is called with no arguments and should insert the new content.
Returns the section."
  (when section
    (let* ((inhibit-read-only t)
           (content-pos (marker-position (oref section content)))
           (end-pos (marker-position (oref section end))))
      ;; Clear existing content (between content and end markers)
      (delete-region content-pos end-pos)
      ;; Insert new content
      (save-excursion
        (goto-char content-pos)
        (let ((magit-insert-section--parent section))
          (funcall insert-fn)))
      ;; Update end marker
      (oset section end (copy-marker (point)))
      section)))

;;; Section State

(defvar-local shipit--section-expansion-state (make-hash-table :test 'equal)
  "Hash table storing expansion state of sections by their identity key.
Keys are strings identifying sections, values are t for expanded, nil for collapsed.")

(defun shipit--section-identity-key (section)
  "Generate a unique identity key for SECTION.
Used for preserving expansion state across refreshes."
  (let ((type (oref section type))
        (value (oref section value)))
    (format "%s:%s" type (or value ""))))

(defun shipit--save-section-expansion-state ()
  "Save the expansion state of all shipit sections in current buffer."
  (clrhash shipit--section-expansion-state)
  (when (bound-and-true-p magit-root-section)
    (shipit--save-section-state-recursive magit-root-section)))

(defun shipit--save-section-state-recursive (section)
  "Recursively save expansion state starting from SECTION."
  (let ((type (oref section type)))
    ;; Only track shipit-related sections
    (when (memq type shipit-section-types)
      (let ((key (shipit--section-identity-key section)))
        (puthash key (not (oref section hidden)) shipit--section-expansion-state))))
  ;; Recurse into children
  (dolist (child (oref section children))
    (shipit--save-section-state-recursive child)))

(defun shipit--restore-section-expansion-state ()
  "Restore the saved expansion state of shipit sections."
  (when (and (bound-and-true-p magit-root-section)
             (> (hash-table-count shipit--section-expansion-state) 0))
    (shipit--restore-section-state-recursive magit-root-section)))

(defun shipit--restore-section-state-recursive (section)
  "Recursively restore expansion state starting from SECTION."
  (let ((type (oref section type)))
    (when (memq type shipit-section-types)
      (let* ((key (shipit--section-identity-key section))
             (was-expanded (gethash key shipit--section-expansion-state)))
        (when was-expanded
          (oset section hidden (not was-expanded))))))
  ;; Recurse into children
  (dolist (child (oref section children))
    (shipit--restore-section-state-recursive child)))

;;; Magit Section Type Definitions
;; These stub defuns + (put ... 'magit-section t) prevent "void function"
;; errors when used with magit-insert-section.

(defun shipit-comment (&rest _args)
  "Magit section type for individual comments. Always visible."
  t)

(defun shipit-comments (&rest _args)
  "Magit section type for comment groups. Always visible."
  t)

(defun shipit-pr (&rest _args)
  "Magit section identifier for PR headers.")
(put 'shipit-pr 'magit-section t)
(defun shipit-description (&rest _args)
  "Magit section identifier for PR descriptions.")
(put 'shipit-description 'magit-section t)
(defun shipit-reviews (&rest _args)
  "Magit section identifier for PR reviews.")
(put 'shipit-reviews 'magit-section t)
(defun shipit-assignees (&rest _args)
  "Magit section identifier for PR assignees.")
(put 'shipit-assignees 'magit-section t)
(defun checks (&rest _args)
  "Magit section identifier for PR checks.")
(put 'checks 'magit-section t)
(defun comment-details (&rest _args)
  "Magit section identifier for collapsible details blocks in comments.")
(put 'comment-details 'magit-section t)

(defun general-comments (&rest _args)
  "Magit section identifier for general comments.")
(put 'general-comments 'magit-section t)
(defun general-comment (&rest _args)
  "Magit section identifier for individual general comments.")
(put 'general-comment 'magit-section t)
(defun comment-thread (&rest _args)
  "Magit section identifier for comment threads.")
(put 'comment-thread 'magit-section t)

(defun inline-comments (&rest _args)
  "Magit section identifier for inline comments.")
(put 'inline-comments 'magit-section t)

(defun file-comments (&rest _args)
  "Magit section identifier for file comments.")
(put 'file-comments 'magit-section t)
(defun notifications (&rest _args)
  "Magit section identifier for notifications.")
(put 'notifications 'magit-section t)

(defun file-comments-outdated (&rest _args)
  "Magit section identifier for outdated file comments.")
(put 'file-comments-outdated 'magit-section t)

(defun labels (&rest _args)
  "Magit section identifier for PR labels.")
(put 'labels 'magit-section t)

(defun reviewers (&rest _args)
  "Magit section identifier for PR reviewers.")
(put 'reviewers 'magit-section t)

(defun pr-commits (&rest _args)
  "Magit section identifier for PR commits.")
(put 'pr-commits 'magit-section t)

(defun pr-files (&rest _args)
  "Magit section identifier for PR files.")
(put 'pr-files 'magit-section t)

(defun pr-file (&rest _args)
  "Magit section identifier for individual PR file.")
(put 'pr-file 'magit-section t)

(defun commit-files (&rest _args)
  "Magit section identifier for commit files.")
(put 'commit-files 'magit-section t)

(defun commit-file (&rest _args)
  "Magit section identifier for individual commit file.")
(put 'commit-file 'magit-section t)

(defun commit-full-message (&rest _args)
  "Magit section identifier for commit full message.")
(put 'commit-full-message 'magit-section t)

(defun diff-hunk (&rest _args)
  "Magit section identifier for diff hunks.")
(put 'diff-hunk 'magit-section t)

(defun inline-comment (&rest _args)
  "Magit section identifier for inline comments.")
(put 'inline-comment 'magit-section t)

(defun shipit--find-files-section ()
  "Return the pr-files magit section without expanding it.
Text-property scans work on collapsed sections too, so a lookup that
walks it leaves Files Changed as it was."
  (when (and (boundp 'magit-root-section) magit-root-section)
    (catch 'found
      (dolist (child (oref magit-root-section children))
        (when (eq (oref child type) 'pr-files)
          (throw 'found child))))))

(defun shipit--find-file-in-files-section (file pr-files-section)
  "Find FILE within PR-FILES-SECTION; return (POS . MATCHED-PATH) or nil.
Search is constrained to the section\\='s range, so it never falls
through into another section whose rows also carry `shipit-file-path'.
Falls back to a basename match when FILE is only a file name."
  (when pr-files-section
    (let* ((start (oref pr-files-section start))
           (end (oref pr-files-section end))
           ;; Candidate basenames to try, in order:
           ;;   1. The raw basename ("smoke script").
           ;;   2. The first whitespace-separated token ("smoke") --
           ;;      catches a name with a descriptor appended, like
           ;;      "auth handler" or "smoke script".
           (raw-basename (file-name-nondirectory (or file "")))
           (first-token (and raw-basename
                             (car (split-string raw-basename "[[:space:]]+" t))))
           (candidates (delete-dups
                        (delq nil (list raw-basename first-token))))
           (exact nil)
           (basename-match nil))
      (save-excursion
        (goto-char start)
        (while (and (not exact) (< (point) end))
          (let* ((prop (get-text-property (point) 'shipit-file-path))
                 (prop-base (and prop (file-name-nondirectory prop))))
            (cond
             ((and prop (string= prop file))
              (setq exact (cons (point) prop)))
             ((and prop-base (not basename-match)
                   (member prop-base candidates))
              (setq basename-match (cons (point) prop)))))
          (goto-char (or (next-single-property-change
                          (point) 'shipit-file-path nil end)
                         end))))
      (or exact basename-match))))

(defun shipit--find-diff-line-in-file-section (file line section)
  "Find diff line for FILE/LINE within file SECTION.
Walks the section\\='s range and returns the first position
whose `shipit-file-path' and `shipit-line-number' match without
carrying `shipit-comment' -- diff lines are inserted before any
inline comment that anchors to them, so the first non-comment
hit is the diff line itself."
  (when section
    (let ((start (oref section start))
          (end (oref section end))
          (found nil))
      (save-excursion
        (goto-char start)
        (while (and (not found) (< (point) end))
          (when (and (equal (get-text-property (point) 'shipit-file-path) file)
                     (equal (get-text-property (point) 'shipit-line-number) line)
                     (not (get-text-property (point) 'shipit-comment)))
            (setq found (point)))
          (goto-char (or (next-single-property-change
                          (point) 'shipit-line-number nil end)
                         end))))
      found)))

(defun shipit--call-with-section-shown (section thunk)
  "Run THUNK with SECTION expanded, restoring its prior collapsed state.
Used by render paths that need to walk a `pr-file' section's
content (for line lookup or buffer-substring) but shouldn't
leave the file expanded.

`unwind-protect' guarantees we collapse back even if THUNK
errors out."
  (let ((was-hidden (and section
                         (slot-boundp section 'hidden)
                         (oref section hidden))))
    (when (and section was-hidden (fboundp 'magit-section-show))
      (magit-section-show section))
    (unwind-protect
        (funcall thunk)
      (when (and section was-hidden
                 (buffer-live-p (current-buffer))
                 (fboundp 'magit-section-hide))
        (ignore-errors (magit-section-hide section))))))

;;; Stack rendering

(defun shipit-stack-layer (&rest _args)
  "Magit section identifier for one layer of a stack.")
(put 'shipit-stack-layer 'magit-section t)

(defun shipit-sections--pad (text width)
  "Return TEXT padded with spaces to at least WIDTH.
`format\=' has no width-from-argument directive, so the column that
keeps a deep stack readable is built here."
  (concat text (make-string (max 0 (- width (length text))) ?\s)))

(defun shipit-sections--stack-row-suffix (row)
  "Return the trailing marker and note for stack ROW."
  (concat (if (cdr (assq 'current row))
              (format "  <- %s" (or (cdr (assq 'marker row)) "this one"))
            "")
          (if (cdr (assq 'note row))
              (concat "  " (cdr (assq 'note row)))
            "")))

(defun shipit-sections--stack-name-width (rows)
  "Return the column width the names of ROWS need."
  (apply #'max 8 (mapcar (lambda (row) (length (cdr (assq 'name row)))) rows)))

(defun shipit-sections--insert-stack-row (row width body-fn)
  "Insert ROW as its own section, names padded to WIDTH.
BODY-FN, when given, is called with ROW to fill the section body,
which is where the commits that layer adds belong."
  (magit-insert-section (shipit-stack-layer (cdr (assq 'name row)) t)
    (let ((start (point)))
      (magit-insert-heading
        (format "   %-3s %s %-6s%s"
                (or (cdr (assq 'label row)) "")
                (shipit-sections--pad (or (cdr (assq 'name row)) "") width)
                (or (cdr (assq 'extra row)) "")
                (shipit-sections--stack-row-suffix row)))
      ;; A row may need to be actionable, which for the stack of pull
      ;; requests means carrying enough to open the one named on it.
      (let ((props (cdr (assq 'props row))))
        (when props (add-text-properties start (point) props))))
    (when body-fn
      (magit-insert-section-body (funcall body-fn row)))))

(defun shipit-sections-insert-stack-rows (rows base &optional body-fn)
  "Insert ROWS as a stack over BASE, top of the stack first.
ROWS arrive bottom first, the order they have to merge in, and are
shown reversed so the picture matches the word stack.  Each row is an
alist of `label\=', `name\=', `extra\=', `current\=' and an optional
`note\=', plus optional `props\=' applied to its line.  Each row
becomes its own section, so it can be expanded and stepped between.
BODY-FN, when given, fills those bodies; magit holds it until the
layer is opened, so a deep stack costs nothing to draw."
  (let ((width (shipit-sections--stack-name-width rows)))
    (dolist (row (reverse rows))
      (shipit-sections--insert-stack-row row width body-fn))
    (insert (format "   %-3s %s\n" "" base))))

(provide 'shipit-sections)
;;; shipit-sections.el ends here
