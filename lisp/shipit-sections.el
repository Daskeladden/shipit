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
(require 'seq)
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

;;; Files Changed

(defun shipit--find-files-section ()
  "Return the Files Changed section of this buffer, without showing it.
Only the top level is searched, where the pull request buffer and the
preview put it.  Text property scans work on a hidden section too, so
a caller that only reads it does not need to open it."
  (and (bound-and-true-p magit-root-section)
       (seq-find (lambda (child) (eq (oref child type) 'pr-files))
                 (oref magit-root-section children))))

(defun shipit--show-files-section ()
  "Show the Files Changed section, without showing each file in it.
Return the section, or nil when the buffer has none.  For navigation
that a user asked for; a scan that only reads the section uses
`shipit--find-files-section', which leaves it as it is."
  (let ((section (shipit--find-files-section)))
    (when section
      (magit-section-show section)
      section)))

(defun shipit--call-with-section-shown (section thunk)
  "Call THUNK with SECTION shown, then hide SECTION again if it was hidden.
A scan of the lines of a file needs them in the buffer, but must not
leave the file open: a freshly opened pull request would otherwise
show every file that a draft or a finding was looked up in."
  (let ((was-hidden (and section
                         (slot-boundp section 'hidden)
                         (oref section hidden))))
    (when was-hidden
      (magit-section-show section))
    (unwind-protect
        (funcall thunk)
      (when (and was-hidden (buffer-live-p (current-buffer)))
        (magit-section-hide section)))))

(defun shipit--find-diff-line-in-file-section (file line section)
  "Return the position of the diff line of FILE at LINE in file SECTION.
Return nil when it is not there.  An inline comment on a line carries
the same file and line, but is inserted after the diff line, so the
first position without `shipit-comment' is the diff line itself."
  (when section
    (let ((end (oref section end))
          (found nil))
      (save-excursion
        (goto-char (oref section start))
        (while (and (not found) (< (point) end))
          (when (and (equal (get-text-property (point) 'shipit-file-path) file)
                     (equal (get-text-property (point) 'shipit-line-number) line)
                     (not (get-text-property (point) 'shipit-comment)))
            (setq found (point)))
          (goto-char (or (next-single-property-change
                          (point) 'shipit-line-number nil end)
                         end))))
      found)))

(defun shipit--find-file-in-files-section (file files-section &optional base-names)
  "Find FILE in FILES-SECTION; return (POSITION . MATCHED-PATH) or nil.
The search stays inside the section, so other rows that carry
`shipit-file-path' do not match.  Without an exact match, the first
file whose base name is one of BASE-NAMES matches."
  (when files-section
    (let ((end (oref files-section end))
          (exact nil)
          (base-name-match nil))
      (save-excursion
        (goto-char (oref files-section start))
        (while (and (not exact) (< (point) end))
          (let ((path (get-text-property (point) 'shipit-file-path)))
            (cond
             ((and path (string= path file))
              (setq exact (cons (point) path)))
             ((and path (not base-name-match)
                   (member (file-name-nondirectory path) base-names))
              (setq base-name-match (cons (point) path)))))
          (goto-char (or (next-single-property-change
                          (point) 'shipit-file-path nil end)
                         end))))
      (or exact base-name-match))))

(defvar shipit-files-section-shown-hook nil
  "Hook run in a buffer each time its Files Changed section is shown.
A mark on a file heading, such as a badge, is painted only while the
section is open: in a closed section, all of them would show on its
one line.  The functions here paint theirs again.")

(defun shipit--run-files-section-shown-hook (section)
  "Run `shipit-files-section-shown-hook' when SECTION is Files Changed.
For `:after' advice on `magit-section-show', since magit has no hook
for a section being shown."
  (when (eq (oref section type) 'pr-files)
    (run-hooks 'shipit-files-section-shown-hook)))

(advice-add 'magit-section-show :after #'shipit--run-files-section-shown-hook)

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
;; A section type is registered with (put ... 'magit-section t).  That
;; is all magit reads; nothing calls the type as a function.
;;
;; Each of these used to carry a stub defun as well, to keep a file
;; that expands `magit-insert-section' without requiring `magit-section'
;; from signalling `void-function'.  That hid the fault rather than
;; fixing it: without the macro the section object is never created, so
;; the stub only moved the failure to a later `oset' on nil, reported as
;; a wrong type argument somewhere unrelated.  The precise error was the
;; useful one.  `test-shipit-magit-requires' fails on the missing
;; require directly, which is where the problem is.

(defun shipit-comments (&rest _args)
  "Magit section type for comment groups. Always visible."
  t)

(put 'shipit-pr 'magit-section t)
(put 'shipit-description 'magit-section t)
(put 'shipit-reviews 'magit-section t)
(put 'shipit-assignees 'magit-section t)
(put 'checks 'magit-section t)
(put 'comment-details 'magit-section t)

(put 'general-comments 'magit-section t)
(put 'general-comment 'magit-section t)
(put 'comment-thread 'magit-section t)

(put 'inline-comments 'magit-section t)

(put 'file-comments 'magit-section t)
(put 'notifications 'magit-section t)

(put 'file-comments-outdated 'magit-section t)

(put 'labels 'magit-section t)

(put 'reviewers 'magit-section t)

(put 'pr-commits 'magit-section t)

(put 'pr-files 'magit-section t)

(put 'pr-file 'magit-section t)

(put 'commit-files 'magit-section t)

(put 'commit-file 'magit-section t)

(put 'commit-full-message 'magit-section t)

(put 'diff-hunk 'magit-section t)

(put 'inline-comment 'magit-section t)

;;; Stack rendering

(put 'shipit-stack-layer 'magit-section t)
(put 'shipit-stack-description 'magit-section t)


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

(defun shipit-sections--stack-label-width (rows)
  "Return the column width the labels of ROWS need.
Computed rather than format-padded so the base row below the stack
can span the label and name columns and still meet the state column."
  (apply #'max 3 (mapcar (lambda (row) (length (or (cdr (assq 'label row)) "")))
                 rows)))

(defun shipit-sections--stack-extra-width (rows)
  "Return the column width the extras of ROWS need.
A fixed width cut \"merged\" loose from the marker column: the widest
extra decides, as it does for names."
  (apply #'max 6 (mapcar (lambda (row) (length (or (cdr (assq 'extra row)) "")))
                 rows)))

(defun shipit-sections--insert-stack-row (row label-width width extra-width
                                               body-fn washer-fn)
  "Insert ROW as its own section, labels and names padded to widths.
LABEL-WIDTH and WIDTH pad the first two columns, EXTRA-WIDTH the
state column.  BODY-FN, when given,
is called with ROW to fill the section body as the row is drawn.
WASHER-FN instead fills it on the row\='s first expand, for bodies
that cost a fetch; give one or the other, not both, since washing
moves the content marker past anything built eagerly."
  (let ((start (point)))
    (magit-insert-section (shipit-stack-layer (cdr (assq 'name row)) t)
      (magit-insert-heading
        (format "   %s %s %s%s"
                (shipit-sections--pad (or (cdr (assq 'label row)) "")
                                      label-width)
                (shipit-sections--pad (or (cdr (assq 'name row)) "") width)
                (shipit-sections--pad (or (cdr (assq 'extra row)) "")
                                      extra-width)
                (shipit-sections--stack-row-suffix row)))
      ;; Inserted now, not deferred.  A deferred body runs as a magit
      ;; washer, and a section created inside one does not nest under
      ;; the layer the way it does here; the tree ended up wrong and
      ;; collapsing reached for the root.  A layer's body is a few
      ;; lines of git output, so building it up front costs nothing
      ;; worth having.  The layer still starts collapsed.
      (when body-fn
        (funcall body-fn row)))
    ;; Closing a section only sets its hidden slot; magit realizes the
    ;; hiding as an invisibility overlay elsewhere, and nothing on this
    ;; render path does, so a row born shut still showed its body and
    ;; TAB had to be pressed twice.  Realize it here.
    (let ((section (get-text-property start 'magit-section)))
      (when (and section washer-fn)
        (oset section washer
              (lambda ()
                ;; The same policy the rows themselves get: what washes
                ;; in starts shut every time, whatever a visibility
                ;; cache remembers from an earlier render.
                (let ((magit-section-set-visibility-hook nil))
                  (funcall washer-fn row)))))
      (when (and section (oref section hidden))
        (magit-section-hide section)))
    ;; A row may need to be actionable, which for the stack of pull
    ;; requests means carrying enough to open the one named on it.
    ;; Applied after the section closes: at close magit lays its own
    ;; keymap over the heading of a section that has a body, and a nil
    ;; one over a section that has none, so anything put on the line
    ;; before that is gone by now.  The row\='s keys go in front of
    ;; whatever magit left, keeping its mouse bindings behind them.
    (when-let* ((props (cdr (assq 'props row))))
      (shipit-sections--add-row-props start props))))

(defun shipit-sections--add-row-props (start props)
  "Apply PROPS to the row heading at START, composing keymaps.
A `keymap\=' in PROPS goes in front of the one already on the line."
  (save-excursion
    (goto-char start)
    (let* ((eol (line-end-position))
           (map (plist-get props 'keymap))
           (existing (and map (get-text-property start 'keymap))))
      (add-text-properties start eol props)
      (when (and map existing)
        (put-text-property start eol 'keymap
                           (make-composed-keymap (list map existing)))))))

(defun shipit-sections-insert-stack-rows (rows base &optional body-fn
                                               washer-fn base-extra)
  "Insert ROWS as a stack over BASE, top of the stack first.
ROWS arrive bottom first, the order they have to merge in, and are
shown reversed so the picture matches the word stack.  Each row is an
alist of `label\=', `name\=', `extra\=', `current\=' and an optional
`note\=', plus optional `props\=' applied to its line.  Each row
becomes its own section, so it can be expanded and stepped between.
BODY-FN, when given, fills those bodies as the rows are drawn, and
WASHER-FN on a row\='s first expand instead; keys beyond the listed
ones ride along untouched for either to read.  BASE-EXTRA, when
given, marks the base\='s row in the state column, the way the rows
above carry their states."
  (let ((label-width (shipit-sections--stack-label-width rows))
        (width (shipit-sections--stack-name-width rows))
        (extra-width (shipit-sections--stack-extra-width rows))
        ;; A layer starts shut every time.  magit decides visibility
        ;; from two things before it reaches the value asked for here:
        ;; a hook that reads what was remembered, and the previous
        ;; render's tree, which a section inherits from.  Both are set
        ;; aside for these rows, so one expanded layer does not stay
        ;; expanded for the rest of the session.  Opening one lasts
        ;; until the buffer is drawn again.
        (magit-section-set-visibility-hook nil)
        (magit-insert-section--oldroot nil))
    (dolist (row (reverse rows))
      (shipit-sections--insert-stack-row row label-width width extra-width
                                         body-fn washer-fn))
    ;; The base is a row like the others: it starts where the labels
    ;; start and, given BASE-EXTRA, is marked in the state column.  An
    ;; icon mark is a space carrying a display property, so the line
    ;; is only trimmed when there is no mark to trim away.
    (let ((line (format "   %s"
                        (shipit-sections--pad base (+ label-width 1 width)))))
      (insert (if (and base-extra (> (length base-extra) 0))
                  (concat line " " base-extra)
                (string-trim-right line))
              "\n"))))

(provide 'shipit-sections)
;;; shipit-sections.el ends here
