;;; shipit-atlassian-kanban.el --- A Jira board drawn on a canvas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

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

;;; Commentary:
;; Draws one Jira Agile board as columns of cards on an Emacs 32
;; canvas, through canvas-diagram.  The text board in
;; `shipit-atlassian-dashboard' stays the way to read a board on any
;; Emacs; this is the extra that Emacs 32 makes possible.
;;
;; Nothing here is required when shipit loads.  The canvas image type
;; arrived in Emacs 32 and the drawing needs the canvas-cairo module
;; that ships with canvas-diagram, so `shipit-canvas-available-p'
;; gates the command and every canvas-diagram function is reached
;; through `declare-function'.  The card data lives in the model rather
;; than in a struct that includes `canvas-diagram-node', because a
;; struct including it would need canvas-diagram at load time.
;;
;; Nothing here writes a canvas-diagram struct slot through its own
;; accessor; see `shipit-canvas-set-slot' for why.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-atlassian-board)
(require 'shipit-atlassian-history)
(require 'shipit-canvas)
(require 'magit-section)

(declare-function canvas-diagram-create "canvas-diagram")
(declare-function canvas-diagram-node-create "canvas-diagram")
(declare-function canvas-diagram-node-label "canvas-diagram")
(declare-function canvas-diagram-model "canvas-diagram")
(declare-function canvas-diagram-nodes "canvas-diagram")
(declare-function canvas-diagram-measure "canvas-diagram")
(declare-function canvas-diagram-size-node "canvas-diagram")
(declare-function canvas-diagram-box-size "canvas-diagram")
(declare-function canvas-diagram-node-h "canvas-diagram")
(declare-function canvas-diagram-node-y "canvas-diagram")
(declare-function canvas-diagram-font "canvas-diagram")
(declare-function canvas-diagram-color "canvas-diagram")
(declare-function canvas-diagram-palette-rgb "canvas-diagram")
(declare-function canvas-diagram-rgb "canvas-diagram")
(declare-function canvas-diagram-icon-data "canvas-diagram" (name))
(declare-function canvas-diagram-set-rgb "canvas-diagram")
(declare-function canvas-diagram-rounded-rect "canvas-diagram")
(declare-function canvas-diagram-export "canvas-diagram")
(declare-function canvas-diagram-show "canvas-diagram")
(declare-function canvas-diagram-rebuild "canvas-diagram")
(declare-function canvas-diagram-redraw "canvas-diagram")
(declare-function canvas-diagram-selected "canvas-diagram")
(declare-function canvas-diagram-go "canvas-diagram")
(declare-function canvas-diagram-set-selected "canvas-diagram")
(declare-function canvas-diagram-current-model "canvas-diagram")
(declare-function canvas-diagram-current-zoom "canvas-diagram")
(declare-function canvas-diagram-zoom-to "canvas-diagram")
(declare-function canvas-diagram-nodes-shown "canvas-diagram")
(declare-function canvas-diagram-mode "canvas-diagram")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-issue--insert-description-section "shipit-issues-buffer")
(declare-function shipit-issue--insert-activity-section "shipit-issues-buffer")
(declare-function shipit-issues--fetch-issue "shipit-issues")
(declare-function shipit-issue-jira--fetch-myself "shipit-issue-jira")
(declare-function consult--read "consult")
(declare-function consult--prompt "consult")
(defvar canvas-diagram-mode-map)
(declare-function canvas-cairo-text "canvas-cairo")
(declare-function canvas-cairo-text-size "canvas-cairo")
(declare-function canvas-cairo-svg "canvas-cairo")
(declare-function canvas-cairo-fill "canvas-cairo")
(declare-function canvas-cairo-stroke "canvas-cairo")
(declare-function canvas-cairo-new-path "canvas-cairo")
(declare-function canvas-cairo-move-to "canvas-cairo")
(declare-function canvas-cairo-line-to "canvas-cairo")
(declare-function canvas-cairo-curve-to "canvas-cairo")
(declare-function canvas-cairo-close-path "canvas-cairo")
(declare-function canvas-cairo-set-line-width "canvas-cairo")
(declare-function canvas-cairo-set-dash "canvas-cairo")
(declare-function canvas-diagram-ink-on "canvas-diagram")
(declare-function canvas-diagram-node-x "canvas-diagram")
(declare-function canvas-diagram-node-w "canvas-diagram")
(defvar canvas-diagram-extra-kinds)

;;; Availability

(defun shipit-atlassian-kanban--require-canvas ()
  "Signal unless this Emacs can draw a board on a canvas."
  (shipit-canvas-require "The canvas kanban" "shipit-atlassian-dashboard"))

;;; The spec
;;
;; canvas-diagram keeps one spec per buffer and compares it with
;; `equal' to decide whether the drawing changed.  The staged moves are
;; part of what the drawing shows, so they travel in the spec rather
;; than being read from a buffer-local behind the callback's back.

(defun shipit-atlassian-kanban--spec (board pending &optional collapsed me filter timeline
                                           tracker)
  "Return the drawing spec for BOARD.
PENDING is the staged moves, COLLAPSED the names of the columns that
are shut, ME the account id of the current user, and FILTER what the
board is narrowed to.  TIMELINE is nil when BOARD is the board now;
see `shipit-atlassian-kanban--timeline-spec' for what it holds
otherwise.  TRACKER is the plist of the issue backend, whose
`:priority-color', `:issue-type-color' and `:issue-type-octicon' the
cards are drawn with, or nil.  A plist, so this can grow another thing
the drawing shows without every caller changing."
  (list :board board :pending pending :collapsed collapsed
        :me me :filter filter :timeline timeline :tracker tracker))

(defun shipit-atlassian-kanban--spec-board (spec)
  "Return the board alist of SPEC."
  (plist-get spec :board))

(defun shipit-atlassian-kanban--spec-pending (spec)
  "Return the staged moves of SPEC, an alist of issue key to status name."
  (plist-get spec :pending))

(defun shipit-atlassian-kanban--spec-collapsed (spec)
  "Return the names of the columns SPEC shows collapsed."
  (plist-get spec :collapsed))

(defun shipit-atlassian-kanban--spec-me (spec)
  "Return the account id SPEC counts as the current user."
  (plist-get spec :me))

(defun shipit-atlassian-kanban--spec-filter (spec)
  "Return what SPEC narrows the board to."
  (plist-get spec :filter))

(defun shipit-atlassian-kanban--spec-timeline (spec)
  "Return the timeline SPEC shows, or nil when it shows the board now."
  (plist-get spec :timeline))

(defun shipit-atlassian-kanban--spec-tracker (spec)
  "Return the plist of the issue backend SPEC draws the cards with, or nil."
  (plist-get spec :tracker))

;;; Narrowing the board
;;
;; A filter is a plist: `:mine' shows only what the current user is
;; assigned, `:type' only one issue type, `:priority' only one
;; priority, and `:text' only the cards whose key or summary carries
;; it.  `:moved' shows only the cards that changed since the time the
;; timeline shows: moved, or made.  They combine, so a card has to pass
;; every part that is set.

(defun shipit-atlassian-kanban--changed-since-p (issue)
  "Return non-nil when ISSUE moved or was made since the time shown."
  (or (assq 'timeline-moves issue)
      (assq 'timeline-new issue)))

(defun shipit-atlassian-kanban--mine-p (issue me)
  "Return non-nil when ISSUE is assigned to the account id ME."
  (and me
       (cl-some (lambda (assignee) (equal me (cdr (assq 'id assignee))))
                (cdr (assq 'assignees issue)))
       t))

(defun shipit-atlassian-kanban--matches-text-p (issue text)
  "Return non-nil when TEXT is in ISSUE\='s key or summary, whatever the case."
  (let ((needle (downcase text)))
    (or (string-search needle (downcase (or (shipit-atlassian-board--issue-key issue) "")))
        (string-search needle (downcase (shipit-atlassian-kanban--summary issue)))
        nil)))

(defun shipit-atlassian-kanban--passes-p (issue filter me)
  "Return non-nil when ISSUE passes every part of FILTER that is set."
  (and (or (not (plist-get filter :mine))
           (shipit-atlassian-kanban--mine-p issue me))
       (or (null (plist-get filter :type))
           (equal (plist-get filter :type)
                  (shipit-atlassian-kanban--issue-type issue)))
       (or (null (plist-get filter :priority))
           (equal (plist-get filter :priority)
                  (shipit-atlassian-kanban--priority issue)))
       (or (null (plist-get filter :text))
           (let ((hit (shipit-atlassian-kanban--matches-text-p
                       issue (plist-get filter :text))))
             (if (plist-get filter :text-invert) (not hit) hit)))
       (or (not (plist-get filter :moved))
           (shipit-atlassian-kanban--changed-since-p issue))
       t))

(defun shipit-atlassian-kanban--filtered-p (filter)
  "Return non-nil when FILTER narrows anything."
  (and (or (plist-get filter :mine)
           (plist-get filter :type)
           (plist-get filter :priority)
           (plist-get filter :text))
       t))

;;; The model

(cl-defstruct (shipit-atlassian-kanban-board
               (:constructor shipit-atlassian-kanban--board-create))
  "A board ready to draw: its NAME and its COLUMNS, left to right.
FRAMES is what the layout worked out for each column frame, as
(X . WIDTH) in drawing coordinates, so the drawing does not work it
out a second time.
INDEX maps an issue key to a plist saying where its card sits and what
draws it: `:issue', `:column', `:column-index', `:row', and, once the
layout has run, `:text' and `:node'.  The nodes are plain
`canvas-diagram-node' structs, so a callback comes back here to find
the issue a node draws.

HEADERS is the same for the column headers, which are nodes as well:
the column name maps to a plist of `:column', `:column-index' and
`:node'.  A header is a node so the keyboard can reach it and TAB can
shut the column from there.

ME is the account id of the current user, which marks their own cards.
TIMELINE is the spec\\='s timeline, which the header line shows.
GHOST-PLAN says which column shows a ghost of which moved card: an
alist of column name to issue keys.  GHOSTS is where the layout put
them, a list of plists of `:key', `:column', `:x', `:y', `:w' and `:h'.
A ghost is not a node: the keyboard cannot reach it.
TRACKER is the plist of the issue backend, as in the spec, or nil."
  name columns index frames headers me timeline ghost-plan ghosts tracker)

(cl-defstruct (shipit-atlassian-kanban-column
               (:constructor shipit-atlassian-kanban--column-create))
  "One column: its NAME, the STATUSES it accepts and its CARDS.
TOTAL is how many cards it holds before the filter, so the header can
say how much is hidden.  COLLAPSED is non-nil while the column shows
only its header."
  name statuses cards total collapsed)

(defun shipit-atlassian-kanban--card-column (raw-columns card home pending)
  "Return the name of the column CARD belongs in.
HOME is the column it came from.  A staged move in PENDING routes it
through the target column\='s status list instead, and a target no
column accepts leaves it at HOME."
  (let ((target (cdr (assoc (shipit-atlassian-board--issue-key card) pending))))
    (or (and target
             (shipit-atlassian-board--column-name-for-status target raw-columns))
        home)))

(defun shipit-atlassian-kanban--place-cards (raw-columns pending)
  "Return a hash of column name to its cards, in reading order.
RAW-COLUMNS is the board configuration\='s column list and PENDING the
staged moves."
  (let ((placed (make-hash-table :test #'equal)))
    (dolist (col raw-columns)
      (let ((home (cdr (assq 'name col))))
        (dolist (card (append (cdr (assq 'issues col)) nil))
          (push card
                (gethash (shipit-atlassian-kanban--card-column
                          raw-columns card home pending)
                         placed)))))
    (maphash (lambda (name cards) (puthash name (nreverse cards) placed)) placed)
    placed))

(defun shipit-atlassian-kanban--columns (raw-columns pending collapsed filter me)
  "Return the column structs for RAW-COLUMNS.
PENDING moves are applied, a column whose name is in COLLAPSED is
marked shut, and a card that fails FILTER is left out while still
counting towards the column\='s total.  ME is the current user\='s
account id, which `:mine\=' needs."
  (let ((placed (shipit-atlassian-kanban--place-cards raw-columns pending)))
    (mapcar (lambda (col)
              (let* ((name (cdr (assq 'name col)))
                     (cards (gethash name placed)))
                (shipit-atlassian-kanban--column-create
                 :name name
                 :statuses (append (cdr (assq 'statuses col)) nil)
                 :cards (seq-filter
                         (lambda (card)
                           (shipit-atlassian-kanban--passes-p card filter me))
                         cards)
                 :total (length cards)
                 :collapsed (and (member name collapsed) t))))
            raw-columns)))

(defun shipit-atlassian-kanban--index (columns)
  "Return a hash of issue key to where its card sits among COLUMNS."
  (let ((index (make-hash-table :test #'equal)))
    (cl-loop for column in columns
             for ci from 0
             do (cl-loop for card in (shipit-atlassian-kanban-column-cards column)
                         for row from 0
                         do (puthash (shipit-atlassian-board--issue-key card)
                                     (list :issue card :column column
                                           :column-index ci :row row)
                                     index)))
    index))

(defun shipit-atlassian-kanban--other-column (card raw-columns)
  "Return the column of RAW-COLUMNS CARD sits in on the other side of the
timeline, or nil when it did not move or no column takes its status."
  (when-let* ((other (cdr (assq 'timeline-other card))))
    (shipit-atlassian-board--column-name-for-status other raw-columns)))

(defun shipit-atlassian-kanban--ghost-plan (columns raw-columns)
  "Return which of COLUMNS shows a ghost of which card, as the model keeps it.
A card that sits in another column on the other side of the timeline
gets a ghost there.  Neither column may be shut: a card in a shut
column is not drawn, and a shut column has no room for a ghost.
RAW-COLUMNS route a status to its column."
  (let ((open (mapcar #'shipit-atlassian-kanban-column-name
                      (seq-remove #'shipit-atlassian-kanban-column-collapsed columns)))
        (plan nil))
    (dolist (column columns)
      (let ((name (shipit-atlassian-kanban-column-name column)))
        (dolist (card (and (member name open)
                           (shipit-atlassian-kanban-column-cards column)))
          (let ((other (shipit-atlassian-kanban--other-column card raw-columns)))
            (when (and (member other open) (not (equal other name)))
              (push (shipit-atlassian-board--issue-key card)
                    (alist-get other plan nil nil #'equal)))))))
    (mapcar (lambda (entry) (cons (car entry) (reverse (cdr entry)))) plan)))

(defun shipit-atlassian-kanban--build (_diagram spec)
  "Build the model of SPEC: its columns, with the staged moves applied."
  (let* ((board (shipit-atlassian-kanban--spec-board spec))
         (raw-columns (append (shipit-atlassian-board--columns board) nil))
         (timeline (shipit-atlassian-kanban--spec-timeline spec))
         (columns (shipit-atlassian-kanban--columns
                   raw-columns
                   (shipit-atlassian-kanban--spec-pending spec)
                   (shipit-atlassian-kanban--spec-collapsed spec)
                   (shipit-atlassian-kanban--spec-filter spec)
                   (shipit-atlassian-kanban--spec-me spec))))
    (let ((model (shipit-atlassian-kanban--board-create
                  :me (shipit-atlassian-kanban--spec-me spec)
                  :timeline timeline
                  :tracker (shipit-atlassian-kanban--spec-tracker spec)
                  :ghost-plan (and (plist-get timeline :arrows)
                                   (shipit-atlassian-kanban--ghost-plan columns raw-columns))
                  :name (or (cdr (assq 'name board)) "Board")
                  :columns columns
                  :index (shipit-atlassian-kanban--index columns)
                  :headers (make-hash-table :test #'equal))))
      (shipit-atlassian-kanban--mark-staged
       model (shipit-atlassian-kanban--spec-pending spec))
      model)))

(defun shipit-atlassian-kanban--mark-staged (model pending)
  "Record in MODEL which cards carry a staged move, and where to.
A staged card draws in a different colour and says so on its card, so
a move is visible before it reaches Jira."
  (pcase-dolist (`(,key . ,target) pending)
    (when (shipit-atlassian-kanban--card model key)
      (shipit-atlassian-kanban--remember model key :staged target))))

(defun shipit-atlassian-kanban--card (model key)
  "Return the plist recorded for issue KEY in MODEL, or nil."
  (gethash key (shipit-atlassian-kanban-board-index model)))

(defun shipit-atlassian-kanban--card-of (model node)
  "Return the plist MODEL records for the issue NODE draws, or nil.
A column header carries a name rather than an issue key, so the node
recorded for that name has to be NODE itself before this answers."
  (let ((entry (shipit-atlassian-kanban--card
                model (canvas-diagram-node-label node))))
    (when (eq node (plist-get entry :node)) entry)))

(defun shipit-atlassian-kanban--header-of (model node)
  "Return the plist MODEL records for the column header NODE draws, or nil."
  (let ((entry (gethash (canvas-diagram-node-label node)
                        (shipit-atlassian-kanban-board-headers model))))
    (when (eq node (plist-get entry :node)) entry)))

(defun shipit-atlassian-kanban--column-of (model node)
  "Return the column NODE belongs to, whether it draws a card or a header."
  (let ((entry (or (shipit-atlassian-kanban--card-of model node)
                   (shipit-atlassian-kanban--header-of model node))))
    (plist-get entry :column)))

(defun shipit-atlassian-kanban--column-index-of (model node)
  "Return the index of the column NODE belongs to."
  (let ((entry (or (shipit-atlassian-kanban--card-of model node)
                   (shipit-atlassian-kanban--header-of model node))))
    (plist-get entry :column-index)))

(defun shipit-atlassian-kanban--remember (model key prop value)
  "Record PROP as VALUE for issue KEY in MODEL."
  (puthash key
           (plist-put (shipit-atlassian-kanban--card model key) prop value)
           (shipit-atlassian-kanban-board-index model)))

(defun shipit-atlassian-kanban--column-at (model index)
  "Return column number INDEX of MODEL, or nil when there is none."
  (when (>= index 0)
    (nth index (shipit-atlassian-kanban-board-columns model))))

;;; Wrapping a card's summary
;;
;; canvas-diagram measures and draws a node's text without a wrap
;; width, so a card that wants more than one line has to carry its own
;; newlines.  Pango lays those out as separate lines, and measuring the
;; same string gives the block back, so what is measured is what is
;; drawn.

(defconst shipit-atlassian-kanban--break-after "-_/.:"
  "Characters a word too wide for a card may be broken after.
A Jira summary carries test names and identifiers, and breaking one
after its own hyphen reads far better than breaking it mid-word.")

(defun shipit-atlassian-kanban--break-points (word)
  "Return the positions in WORD a break may follow, shortest first."
  (let ((wanted (append shipit-atlassian-kanban--break-after nil))
        (points nil))
    (dotimes (i (1- (length word)))
      (when (memq (aref word i) wanted)
        (push (1+ i) points)))
    (nreverse points)))

(defun shipit-atlassian-kanban--longest-prefix (word measure width points)
  "Return the last position in POINTS whose prefix of WORD fits WIDTH, or nil.
MEASURE gives a string\='s pixel width."
  (let ((best nil))
    (dolist (point points)
      (when (<= (funcall measure (substring word 0 point)) width)
        (setq best point)))
    best))

(defun shipit-atlassian-kanban--split-point (word measure width)
  "Return where to break WORD so its first piece fits WIDTH.
Prefers a break after one of `shipit-atlassian-kanban--break-after\='.
Falls back to the character where the word stops fitting, and to one
character, so a break always makes progress."
  (or (shipit-atlassian-kanban--longest-prefix
       word measure width (shipit-atlassian-kanban--break-points word))
      (shipit-atlassian-kanban--longest-prefix
       word measure width (number-sequence 1 (length word)))
      1))

(defun shipit-atlassian-kanban--split-word (word measure width)
  "Split WORD into pieces, none wider than WIDTH; return them in order."
  (let ((rest word)
        (pieces nil))
    (while (> (funcall measure rest) width)
      (let ((at (shipit-atlassian-kanban--split-point rest measure width)))
        (push (substring rest 0 at) pieces)
        (setq rest (substring rest at))))
    (nreverse (if (string-empty-p rest) pieces (cons rest pieces)))))

(defun shipit-atlassian-kanban--fill-line (words measure width)
  "Take words off WORDS while they fit WIDTH; return (LINE . REST).
MEASURE gives a string\='s pixel width.  The first word is taken
whatever its width; `shipit-atlassian-kanban--wrap\=' has already split
any word too wide to stand on a line of its own."
  (let ((line (car words))
        (rest (cdr words)))
    (while (and rest (<= (funcall measure (concat line " " (car rest))) width))
      (setq line (concat line " " (car rest))
            rest (cdr rest)))
    (cons line rest)))

(defun shipit-atlassian-kanban--wrap (text measure width)
  "Return TEXT with newlines so that no line is wider than WIDTH.
MEASURE gives a string\='s pixel width.  A word too wide to stand on a
line of its own is split, and its pieces take lines of their own."
  (let ((words (split-string (or text "") nil t))
        (lines nil))
    (while words
      (if (> (funcall measure (car words)) width)
          (progn
            (dolist (piece (shipit-atlassian-kanban--split-word
                            (car words) measure width))
              (push piece lines))
            (setq words (cdr words)))
        (let ((filled (shipit-atlassian-kanban--fill-line words measure width)))
          (push (car filled) lines)
          (setq words (cdr filled)))))
    (string-join (nreverse lines) "\n")))

;;; Issue types as kinds
;;
;; canvas-diagram outlines a node by its kind and lists the kinds in
;; the legend, so the Jira issue type becomes the kind and the board
;; gets a legend of its types without drawing code here.

(defcustom shipit-atlassian-kanban-type-colors
  '(("Bug" . "red") ("Story" . "sea green") ("Task" . "steel blue")
    ("Sub-task" . "slate gray") ("Epic" . "medium purple")
    ("Improvement" . "dark cyan") ("Spike" . "gold"))
  "Outline colour for each Jira issue type.
A type not listed here is outlined in the edge colour, like any kind
canvas-diagram does not know."
  :type '(alist :key-type string :value-type string)
  :group 'shipit-atlassian)

(defcustom shipit-atlassian-kanban-mine-mark "●"
  "What marks a card the current user is assigned.
It goes after the issue key.  The fill already says whether a move is
staged for a card or it changed on the timeline, and the outline
already says its type, so the mark is text."
  :type 'string
  :group 'shipit-atlassian)

(defcustom shipit-atlassian-kanban-new-mark "new"
  "What marks a card made since the time the timeline shows.
It goes after the issue key.  The board of then shows such a card
where it is now, and the mark says it was not there then."
  :type 'string
  :group 'shipit-atlassian)

(defcustom shipit-atlassian-kanban-staged-color "dark orange"
  "Colour a card is drawn in while a move is staged for it.
A staged card is not where Jira has it, and this says so at a glance.
It is deliberately not the selection colour: that already rings the
card the keyboard is on, and one colour cannot mean two things."
  :type 'string
  :group 'shipit-atlassian)

(defcustom shipit-atlassian-kanban-staged-colors
  '("dark orange" "gold" "medium purple" "deep pink" "cyan")
  "The colours the menu cycles `shipit-atlassian-kanban-staged-color' through."
  :type '(repeat string)
  :group 'shipit-atlassian)

(defcustom shipit-atlassian-kanban-timeline-color "orchid"
  "Colour a card is drawn in on the timeline when it moved since then.
Those cards are the ones not where they are now, so the colour shows
what changed between the time shown and now."
  :type 'string
  :group 'shipit-atlassian)

(defun shipit-atlassian-kanban--issue-type (issue)
  "Return the Jira issue type of ISSUE, or nil."
  (cdr (assq 'issue-type issue)))

(defun shipit-atlassian-kanban--priority (issue)
  "Return the name of the priority of ISSUE, or nil."
  (cdr (assq 'priority issue)))

(defun shipit-atlassian-kanban--values-in (columns fn)
  "Return what FN gives for the cards of COLUMNS, each once, in order.
Nil answers are left out."
  (let (values)
    (dolist (column columns)
      (dolist (card (shipit-atlassian-kanban-column-cards column))
        (let ((value (funcall fn card)))
          (when (and value (not (member value values)))
            (push value values)))))
    (nreverse values)))

(defun shipit-atlassian-kanban--types-in (columns)
  "Return the issue types used across COLUMNS, in order of first appearance."
  (shipit-atlassian-kanban--values-in columns #'shipit-atlassian-kanban--issue-type))

(defun shipit-atlassian-kanban--register-kinds (columns)
  "Tell canvas-diagram the colour of each issue type in COLUMNS.
Adds to the kinds other packages registered rather than replacing them.

A type carries no icon.  canvas-diagram sizes a node's icon to the
node's height, which is right for a box of one line and wrong for a
card of four: the icon grows with the summary and crowds out the text
it belongs to.  The outline colour marks the type instead, and the
legend still lists the types in use."
  (dolist (type (shipit-atlassian-kanban--types-in columns))
    (let ((color (cdr (assoc type shipit-atlassian-kanban-type-colors))))
      (when (and color (not (assoc type canvas-diagram-extra-kinds)))
        (push (cons type color) canvas-diagram-extra-kinds)))))

;;; What a card says

(defun shipit-atlassian-kanban--summary (issue)
  "Return the summary line of ISSUE."
  (or (cdr (assq 'summary issue)) (cdr (assq 'title issue)) ""))

(defun shipit-atlassian-kanban--assignee (issue)
  "Return the display name of ISSUE\='s assignee, or nil."
  (cdr (assq 'login (car (cdr (assq 'assignees issue))))))

(defun shipit-atlassian-kanban--fields (issue)
  "Return the lines of ISSUE\='s detail: its type, priority, assignee, status."
  (delq nil
        (list (when-let* ((v (shipit-atlassian-kanban--issue-type issue)))
                (format "Type: %s" v))
              (when-let* ((v (cdr (assq 'priority issue))))
                (format "Priority: %s" v))
              (format "Assignee: %s"
                      (or (shipit-atlassian-kanban--assignee issue) "nobody"))
              (when-let* ((v (cdr (assq 'state issue))))
                (format "Status: %s" v)))))

(defun shipit-atlassian-kanban--move-time (move)
  "Return when MOVE, a status change, was made."
  (format-time-string "%Y-%m-%d %H:%M" (plist-get move :time)))

(defun shipit-atlassian-kanban--move-author (move)
  "Return who made MOVE."
  (or (plist-get move :by) "someone"))

(defun shipit-atlassian-kanban--move-statuses (move)
  "Return the statuses MOVE went between."
  (format "%s → %s" (plist-get move :from) (plist-get move :to)))

(defun shipit-atlassian-kanban--made-since (issue)
  "Return the line telling when ISSUE was made, if after the timeline\\='s time."
  (when-let* ((made (cdr (assq 'timeline-new issue))))
    (list (format "Made since then: %s" (format-time-string "%Y-%m-%d %H:%M" made)))))

(defun shipit-atlassian-kanban--moves-since (issue)
  "Return the lines telling the moves of ISSUE since the timeline\\='s time."
  (when-let* ((moves (cdr (assq 'timeline-moves issue))))
    (cons "Moved since then:"
          (mapcar (lambda (move)
                    (format "  %s  %s: %s"
                            (shipit-atlassian-kanban--move-time move)
                            (shipit-atlassian-kanban--move-author move)
                            (shipit-atlassian-kanban--move-statuses move)))
                  moves))))

(defun shipit-atlassian-kanban--note (model key)
  "Return the hover note for issue KEY in MODEL."
  (let* ((card (shipit-atlassian-kanban--card model key))
         (issue (plist-get card :issue))
         (staged (plist-get card :staged)))
    (string-join
     (append (shipit-atlassian-kanban--fields issue)
             (when staged (list (format "Staged for: %s" staged)))
             (shipit-atlassian-kanban--made-since issue)
             (shipit-atlassian-kanban--moves-since issue))
     "\n")))

;;; The layout

(defcustom shipit-atlassian-kanban-card-width 240
  "Width of a card on the canvas, in pixels at natural zoom.
A summary wraps to fit, and a word too wide to fit is broken, so this
is what a column is wide unless a card cannot be made to fit at all."
  :type 'integer
  :group 'shipit-atlassian)

(defconst shipit-atlassian-kanban--frame-pad 10
  "Pixels between a column frame and the cards inside it.")

(defconst shipit-atlassian-kanban--card-gap 10
  "Pixels between two cards in a column.")

(defconst shipit-atlassian-kanban--column-gap 16
  "Pixels between two column frames.")

(defun shipit-atlassian-kanban--text-measure (ctx)
  "Return a function giving a text\='s pixel width on CTX."
  (let ((font (canvas-diagram-font)))
    (lambda (text) (car (canvas-cairo-text-size ctx text font)))))

(defun shipit-atlassian-kanban--line-height (ctx)
  "Height of one line of text on CTX, with the box padding around it."
  (cdr (funcall (canvas-diagram-measure ctx) "Ag")))

(defun shipit-atlassian-kanban--text-budget (ctx)
  "Pixels a card\='s text may take on CTX.
The card is `shipit-atlassian-kanban-card-width' wide and the box
padding sits inside that, so the budget is the width less the padding.
Measuring the empty string gives the padding without naming it, which
keeps this exact rather than nearly right."
  (max 40 (- shipit-atlassian-kanban-card-width
             (car (funcall (canvas-diagram-measure ctx) "")))))

(defun shipit-atlassian-kanban--frame-width (card-width)
  "Width of a column frame around cards CARD-WIDTH wide."
  (+ card-width (* 2 shipit-atlassian-kanban--frame-pad)))

(defun shipit-atlassian-kanban--key-line (model key issue)
  "Return the first line of a card: its KEY, marked when ISSUE is mine,
and when ISSUE was made since the time the timeline shows."
  (string-join
   (delq nil (list key
                   (and (shipit-atlassian-kanban--mine-p
                         issue (shipit-atlassian-kanban-board-me model))
                        shipit-atlassian-kanban-mine-mark)
                   (and (assq 'timeline-new issue)
                        shipit-atlassian-kanban-new-mark)))
   " "))

(defun shipit-atlassian-kanban--card-text (model key ctx)
  "Return what the card for KEY says: its key, then its wrapped summary.
The key leaves room before it for the icon of the issue type, when the
type has one."
  (let ((issue (plist-get (shipit-atlassian-kanban--card model key) :issue)))
    (concat (if (shipit-atlassian-kanban--type-icon model issue)
                (shipit-atlassian-kanban--icon-room ctx)
              "")
            (shipit-atlassian-kanban--key-line model key issue) "\n"
            (shipit-atlassian-kanban--wrap
             (shipit-atlassian-kanban--summary issue)
             (shipit-atlassian-kanban--text-measure ctx)
             (shipit-atlassian-kanban--text-budget ctx)))))

(defun shipit-atlassian-kanban--make-node (diagram model key ctx measure)
  "Return the node drawing issue KEY, sized for its text.
Records the text and the node in MODEL, so the callbacks can find
them again from the node alone."
  (let* ((card (shipit-atlassian-kanban--card model key))
         (issue (plist-get card :issue))
         (node (canvas-diagram-node-create
                :label key
                :kind (shipit-atlassian-kanban--issue-type issue))))
    (shipit-atlassian-kanban--remember
     model key :text (shipit-atlassian-kanban--card-text model key ctx))
    (shipit-atlassian-kanban--remember model key :node node)
    (shipit-canvas-set-slot
     'canvas-diagram-node 'note node (shipit-atlassian-kanban--note model key))
    (canvas-diagram-size-node diagram node measure)
    node))

(defun shipit-atlassian-kanban--header-count (column)
  "Return COLUMN\='s count: how many it shows, over how many it holds."
  (let ((shown (length (shipit-atlassian-kanban-column-cards column)))
        (total (shipit-atlassian-kanban-column-total column)))
    (if (and total (/= shown total))
        (format "%d/%d" shown total)
      (format "%d" shown))))

(defun shipit-atlassian-kanban--header-text (column)
  "Return what COLUMN\='s header says: its name, its count, and whether it
is shut."
  (format "%s (%s)%s"
          (shipit-atlassian-kanban-column-name column)
          (shipit-atlassian-kanban--header-count column)
          (if (shipit-atlassian-kanban-column-collapsed column) " …" "")))

(defun shipit-atlassian-kanban--make-header (diagram model column index measure)
  "Return the header node of COLUMN at INDEX, sized for its text.
Records it in MODEL, so a callback can tell a header from a card.  The
header carries no kind, so it neither takes an outline nor reaches the
legend of issue types."
  (let ((node (canvas-diagram-node-create
               :label (shipit-atlassian-kanban-column-name column))))
    (puthash (shipit-atlassian-kanban-column-name column)
             (list :column column :column-index index :node node
                   :text (shipit-atlassian-kanban--header-text column))
             (shipit-atlassian-kanban-board-headers model))
    (canvas-diagram-size-node diagram node measure)
    node))

(defun shipit-atlassian-kanban--column-nodes (diagram model column ctx measure)
  "Return the card nodes of COLUMN, sized for their text and not yet placed.
A collapsed column has none: its cards are neither drawn nor walked."
  (unless (shipit-atlassian-kanban-column-collapsed column)
    (mapcar (lambda (card)
              (shipit-atlassian-kanban--make-node
               diagram model (shipit-atlassian-board--issue-key card) ctx measure))
            (shipit-atlassian-kanban-column-cards column))))

(defun shipit-atlassian-kanban--needed-width (diagram node measure)
  "Width NODE needs for everything canvas-diagram draws inside it."
  (car (canvas-diagram-box-size
        node (shipit-atlassian-kanban--node-text diagram node) measure)))

(defun shipit-atlassian-kanban--card-width-for (diagram nodes measure)
  "Width every card in a column of NODES gets.
Normally `shipit-atlassian-kanban-card-width', because a summary is
wrapped to fit and a word too wide to fit is split.  The maximum is
the guarantee behind that: whatever a card turns out to need, it gets,
rather than drawing its text past its own edge."
  (apply #'max shipit-atlassian-kanban-card-width
         (mapcar (lambda (node)
                   (shipit-atlassian-kanban--needed-width diagram node measure))
                 nodes)))

(defun shipit-atlassian-kanban--width-for (diagram column header nodes measure)
  "Width the cards of COLUMN take, HEADER and card NODES already sized.
A collapsed column takes the width of its own header, which is what
makes the columns after it move left."
  (if (shipit-atlassian-kanban-column-collapsed column)
      (shipit-atlassian-kanban--needed-width diagram header measure)
    (shipit-atlassian-kanban--card-width-for diagram nodes measure)))

(defun shipit-atlassian-kanban--place-column (nodes x top width)
  "Place NODES down a column at X, starting at TOP, each WIDTH wide."
  (let ((y top))
    (dolist (node nodes)
      (shipit-canvas-set-slot 'canvas-diagram-node 'x node x)
      (shipit-canvas-set-slot 'canvas-diagram-node 'y node y)
      (shipit-canvas-set-slot 'canvas-diagram-node 'w node width)
      (setq y (+ y (canvas-diagram-node-h node)
                 shipit-atlassian-kanban--card-gap)))))

(defun shipit-atlassian-kanban--place-ghosts (model column x top width ctx)
  "Place the ghosts MODEL plans for COLUMN at X from TOP, each WIDTH wide.
Records each in MODEL and returns the y below the last of them."
  (let ((name (shipit-atlassian-kanban-column-name column))
        (height (shipit-atlassian-kanban--line-height ctx))
        (y top))
    (dolist (key (cdr (assoc name (shipit-atlassian-kanban-board-ghost-plan model))))
      (push (list :key key :column name :x x :y y :w width :h height)
            (shipit-atlassian-kanban-board-ghosts model))
      (setq y (+ y height shipit-atlassian-kanban--card-gap)))
    y))

(defun shipit-atlassian-kanban--lay-out-column (diagram model column index ctx measure x)
  "Lay COLUMN out at INDEX with its frame starting at X; return its nodes.
The header comes first, then the cards, so reading order runs down a
column before it starts the next.  The ghosts of cards that moved into
or out of the column on the timeline go between the two."
  (let* ((header (shipit-atlassian-kanban--make-header
                  diagram model column index measure))
         (cards (shipit-atlassian-kanban--column-nodes
                 diagram model column ctx measure))
         (width (shipit-atlassian-kanban--width-for
                 diagram column header cards measure))
         (inner (+ x shipit-atlassian-kanban--frame-pad)))
    (shipit-atlassian-kanban--place-column (list header) inner
                                           shipit-atlassian-kanban--frame-pad width)
    (shipit-atlassian-kanban--place-column
     cards inner
     (shipit-atlassian-kanban--place-ghosts
      model column inner
      (+ (canvas-diagram-node-y header) (canvas-diagram-node-h header)
         shipit-atlassian-kanban--card-gap)
      width ctx)
     width)
    (cons (cons header cards) width)))

(defun shipit-atlassian-kanban--lay-out (diagram ctx)
  "Place every card of DIAGRAM on CTX; return the nodes column by column.
Records each column frame in the model for the drawing to use."
  (let* ((model (canvas-diagram-model diagram))
         (measure (canvas-diagram-measure ctx))
         (x 0)
         (index 0)
         (frames nil)
         (all nil))
    ;; Here and not in `:build\=', which stays plain data: the kinds live
    ;; in canvas-diagram, which is loaded only where a board is drawn.
    (shipit-atlassian-kanban--register-kinds
     (shipit-atlassian-kanban-board-columns model))
    ;; A zoom or a resize lays the same model out again.
    (setf (shipit-atlassian-kanban-board-ghosts model) nil)
    (dolist (column (shipit-atlassian-kanban-board-columns model))
      (pcase-let* ((`(,nodes . ,width)
                    (shipit-atlassian-kanban--lay-out-column
                     diagram model column index ctx measure x))
                   (frame (shipit-atlassian-kanban--frame-width width)))
        (push (cons x frame) frames)
        (setq x (+ x frame shipit-atlassian-kanban--column-gap)
              index (1+ index)
              all (append all nodes))))
    (setf (shipit-atlassian-kanban-board-frames model) (nreverse frames))
    (setf (shipit-atlassian-kanban-board-ghosts model)
          (nreverse (shipit-atlassian-kanban-board-ghosts model)))
    (shipit-atlassian-kanban--make-room-for-ghosts diagram all)
    all))

(defun shipit-atlassian-kanban--box-bottom (box)
  "Return the bottom edge of BOX, a ghost plist."
  (+ (plist-get box :y) (plist-get box :h)))

(defun shipit-atlassian-kanban--node-bottom (node)
  "Return the bottom edge of NODE."
  (+ (canvas-diagram-node-y node) (canvas-diagram-node-h node)))

(defun shipit-atlassian-kanban--make-room-for-ghosts (diagram nodes)
  "Let the drawing of DIAGRAM reach below NODES to its lowest ghost.
canvas-diagram sizes a drawing by its nodes and its slack, and a ghost
is no node, so a ghost in a column without cards could fall off the
bottom.  The slack grows to take it, and shrinks back without it."
  (let* ((pad shipit-atlassian-kanban--frame-pad)
         (below (- (apply #'max 0 (mapcar #'shipit-atlassian-kanban--box-bottom
                                          (shipit-atlassian-kanban-board-ghosts
                                           (canvas-diagram-model diagram))))
                   (apply #'max 0 (mapcar #'shipit-atlassian-kanban--node-bottom nodes)))))
    (shipit-canvas-set-slot 'canvas-diagram 'slack diagram
                                       (cons pad (+ pad (max 0 below))))))

;;; Drawing the columns
;;
;; canvas-diagram calls `:draw-edges' before it draws the boxes, so the
;; frames and headers drawn here sit behind the cards.  A column takes
;; the colour the palette gives its place: its header is filled with it
;; and its frame faintly tinted, so the cards can stay plain.

(defun shipit-atlassian-kanban--frame-height (nodes ghosts ctx)
  "Height every column frame takes, given the laid-out NODES and GHOSTS on CTX."
  (+ (apply #'max
            (shipit-atlassian-kanban--line-height ctx)
            (append (mapcar #'shipit-atlassian-kanban--node-bottom nodes)
                    (mapcar #'shipit-atlassian-kanban--box-bottom ghosts)))
     shipit-atlassian-kanban--frame-pad))

(defun shipit-atlassian-kanban--draw-frame (ctx frame height rgb)
  "Fill one column FRAME, an (X . WIDTH) pair, HEIGHT tall, faintly in RGB."
  (canvas-diagram-set-rgb ctx rgb 0.12)
  (canvas-diagram-rounded-rect ctx (car frame) 0 (cdr frame) height 8)
  (canvas-cairo-fill ctx))

(defun shipit-atlassian-kanban--draw-columns (diagram ctx)
  "Draw the frame behind every column of DIAGRAM on CTX, and the ghosts.
The header is a node, so canvas-diagram draws it with the cards."
  (let* ((model (canvas-diagram-model diagram))
         (height (shipit-atlassian-kanban--frame-height
                  (canvas-diagram-nodes diagram)
                  (shipit-atlassian-kanban-board-ghosts model) ctx)))
    (seq-do-indexed (lambda (frame index)
                      (shipit-atlassian-kanban--draw-frame
                       ctx frame height (canvas-diagram-palette-rgb index)))
                    (shipit-atlassian-kanban-board-frames model))
    (shipit-atlassian-kanban--draw-ghosts diagram ctx)))

;;; Drawing the priority of a card
;;
;; A card says its priority in a pill at the right of its first line,
;; the line of its key, which has room to spare.  The pill is drawn over
;; the card rather than put in its text: a canvas-diagram badge takes
;; the height of all the lines of a card and pushes them all to the
;; right.  The colour is the issue backend's, so a tracker without
;; priorities gives no card a pill, and one without colours a plain one.
;;
;; The key line starts with the icon of the card's issue type, drawn
;; over the card in the colour the tracker gives that type, in room the
;; text leaves for it.  A canvas-diagram icon is as tall as all the
;; lines of a card, which is why this one is drawn here too.

(defconst shipit-atlassian-kanban--pill-pad 4
  "Room left and right of the text of a priority pill, in pixels.")

(defconst shipit-atlassian-kanban--icon-gap 4
  "Pixels between the type icon of a card and its key.")

(defun shipit-atlassian-kanban--tracker-rgb (model key name)
  "Return the colour the tracker of MODEL gives NAME through its KEY.
KEY is `:priority-color' or `:issue-type-color'.  The colour of the
edges when the tracker gives none."
  (or (when-let* ((fn (plist-get (shipit-atlassian-kanban-board-tracker model) key))
                  (color (funcall fn name)))
        (canvas-diagram-rgb color))
      (canvas-diagram-color :edge)))

(defun shipit-atlassian-kanban--type-icon (model issue)
  "Return the icon of the issue type of ISSUE on the board MODEL draws, or nil.
A cons of the icon\\='s SVG text and its colour.  Nil when the tracker
names no octicon for the type, or the icon cannot be had."
  (when-let* ((type (shipit-atlassian-kanban--issue-type issue))
              (fn (plist-get (shipit-atlassian-kanban-board-tracker model)
                             :issue-type-octicon))
              (octicon (funcall fn type))
              (svg (canvas-diagram-icon-data (concat "octicons:" octicon))))
    (cons svg (shipit-atlassian-kanban--tracker-rgb model :issue-type-color type))))

(defun shipit-atlassian-kanban--icon-side (ctx)
  "Return the side of a type icon on CTX: the height of a line of text."
  (cdr (canvas-cairo-text-size ctx "Ag" (canvas-diagram-font))))

(defun shipit-atlassian-kanban--icon-room (ctx)
  "Return the spaces that leave room for a type icon and its gap on CTX."
  (let ((space (car (canvas-cairo-text-size ctx " " (canvas-diagram-font)))))
    (make-string (ceiling (+ (shipit-atlassian-kanban--icon-side ctx)
                             shipit-atlassian-kanban--icon-gap)
                          (max 1 space))
                 ?\s)))

(defun shipit-atlassian-kanban--type-mark (model node ctx)
  "Return the type icon of the card NODE draws in MODEL, or nil.
A plist of `:svg' and `:rgb', and the square at the start of the key
line: `:x', `:y' and `:side'.  Nil for a card whose type has no icon,
and for a column header."
  (when-let* ((card (shipit-atlassian-kanban--card-of model node))
              (icon (shipit-atlassian-kanban--type-icon model (plist-get card :issue))))
    (let ((pad (shipit-atlassian-kanban--box-pad ctx)))
      (list :svg (car icon) :rgb (cdr icon)
            :x (+ (canvas-diagram-node-x node) pad)
            :y (+ (canvas-diagram-node-y node) pad)
            :side (shipit-atlassian-kanban--icon-side ctx)))))

(defun shipit-atlassian-kanban--draw-type-mark (ctx mark)
  "Draw on CTX MARK, a type icon, in its colour."
  (let ((side (plist-get mark :side)))
    (canvas-diagram-set-rgb ctx (plist-get mark :rgb))
    (canvas-cairo-svg ctx (plist-get mark :svg) (plist-get mark :x) (plist-get mark :y)
                      side side)))

(defun shipit-atlassian-kanban--box-pad (ctx)
  "Return the padding of a box on CTX, on one side."
  (/ (car (funcall (canvas-diagram-measure ctx) "")) 2.0))

(defun shipit-atlassian-kanban--pill-or-dot (name rgb node ctx key-width)
  "Return the mark of priority NAME in RGB on NODE: a pill, or a dot.
The pill sits at the right of the first line, and becomes a dot when
KEY-WIDTH, the width of the key line, leaves no room for it."
  (let* ((font (canvas-diagram-font))
         (pad (shipit-atlassian-kanban--box-pad ctx))
         (size (canvas-cairo-text-size ctx name font))
         (w (+ (car size) (* 2 shipit-atlassian-kanban--pill-pad)))
         (h (cdr size))
         (left (+ (canvas-diagram-node-x node) pad))
         (right (- (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node)) pad))
         (top (+ (canvas-diagram-node-y node) pad)))
    (if (<= (+ left key-width shipit-atlassian-kanban--pill-pad w) right)
        (list :text name :rgb rgb :x (- right w) :y top :w w :h h)
      (let ((d (/ h 2.0)))
        (list :text name :rgb rgb :dot t
              :x (- right d) :y (+ top (/ (- h d) 2.0)) :w d :h d)))))

(defun shipit-atlassian-kanban--priority-mark (model node ctx)
  "Return the mark of the priority of the card NODE draws in MODEL, or nil.
A plist of `:text' and `:rgb', the box `:x', `:y', `:w' and `:h', and
`:dot' when the key leaves no room for the name.  Nil for a card of no
priority, and for a column header."
  (when-let* ((card (shipit-atlassian-kanban--card-of model node))
              (name (shipit-atlassian-kanban--priority (plist-get card :issue))))
    (shipit-atlassian-kanban--pill-or-dot
     name (shipit-atlassian-kanban--tracker-rgb model :priority-color name) node ctx
     (car (canvas-cairo-text-size
           ctx (car (split-string (plist-get card :text) "\n"))
           (canvas-diagram-font))))))

(defun shipit-atlassian-kanban--draw-mark (ctx mark)
  "Draw on CTX MARK, a priority mark: a pill saying its text, or a dot."
  (let ((x (plist-get mark :x))
        (y (plist-get mark :y))
        (h (plist-get mark :h))
        (rgb (plist-get mark :rgb)))
    (canvas-diagram-rounded-rect ctx x y (plist-get mark :w) h (/ h 2.0))
    (canvas-diagram-set-rgb ctx rgb)
    (canvas-cairo-fill ctx)
    (unless (plist-get mark :dot)
      (canvas-diagram-set-rgb ctx (canvas-diagram-ink-on rgb))
      (canvas-cairo-text ctx (+ x shipit-atlassian-kanban--pill-pad) y
                         (plist-get mark :text) (canvas-diagram-font)))))

(defun shipit-atlassian-kanban--draw-card-marks (diagram ctx)
  "Draw on CTX the type icon and the priority mark of every card DIAGRAM draws."
  (let ((model (canvas-diagram-model diagram)))
    (maphash (lambda (_key card)
               (when-let* ((node (plist-get card :node)))
                 (when-let* ((mark (shipit-atlassian-kanban--type-mark model node ctx)))
                   (shipit-atlassian-kanban--draw-type-mark ctx mark))
                 (when-let* ((mark (shipit-atlassian-kanban--priority-mark model node ctx)))
                   (shipit-atlassian-kanban--draw-mark ctx mark))))
             (shipit-atlassian-kanban-board-index model))))

(defun shipit-atlassian-kanban--draw-over (diagram ctx)
  "Draw on CTX what goes over the cards of DIAGRAM.
The `:draw-over' callback: the type icons and priority marks, then the
arrows of the timeline, which may cross a mark."
  (shipit-atlassian-kanban--draw-card-marks diagram ctx)
  (shipit-atlassian-kanban--draw-arrows diagram ctx))

;;; Drawing where the moved cards went
;;
;; On the timeline a card that sits in another column on the other side
;; gets a ghost there, a dashed box, and an arrow joins the two: from
;; the card to where it is now on the board of then, and from where it
;; was to the card on the board of now.  The ghosts are drawn with the
;; column frames, under the cards, which they never cover.  The arrows
;; are drawn over every card, with a halo, so an arrow that crosses a
;; card stays one line rather than seeming to start from that card.

(defun shipit-atlassian-kanban--timeline-rgb ()
  "Return the colour of the timeline: moved cards, ghosts and arrows."
  (or (canvas-diagram-rgb shipit-atlassian-kanban-timeline-color)
      (canvas-diagram-color :selection)))

(defun shipit-atlassian-kanban--ghost-node (model ghost)
  "Return the node of the card GHOST stands for in MODEL, or nil."
  (plist-get (shipit-atlassian-kanban--card model (plist-get ghost :key)) :node))

(defun shipit-atlassian-kanban--each-ghost (diagram fn)
  "Call FN with each ghost of DIAGRAM whose card is drawn, and that card\\='s node."
  (let ((model (canvas-diagram-model diagram)))
    (dolist (ghost (shipit-atlassian-kanban-board-ghosts model))
      (when-let* ((node (shipit-atlassian-kanban--ghost-node model ghost)))
        (funcall fn ghost node)))))

(defun shipit-atlassian-kanban--now-side-p (diagram)
  "Return non-nil when DIAGRAM shows the board of now beside the board of then."
  (plist-get (shipit-atlassian-kanban-board-timeline (canvas-diagram-model diagram)) :now))

(defun shipit-atlassian-kanban--node-box (node)
  "Return the box of NODE as (X Y W H)."
  (list (canvas-diagram-node-x node) (canvas-diagram-node-y node)
        (canvas-diagram-node-w node) (canvas-diagram-node-h node)))

(defun shipit-atlassian-kanban--ghost-box (ghost)
  "Return the box of GHOST as (X Y W H)."
  (list (plist-get ghost :x) (plist-get ghost :y)
        (plist-get ghost :w) (plist-get ghost :h)))

(defun shipit-atlassian-kanban--side-middle (box right)
  "Return (X . Y), the middle of the RIGHT side of BOX, else of its left."
  (pcase-let ((`(,x ,y ,w ,h) box))
    (cons (if right (+ x w) x) (+ y (/ h 2.0)))))

(defconst shipit-atlassian-kanban--arrow-head 10
  "Length of an arrow head, in pixels at natural zoom.")

(defun shipit-atlassian-kanban--arrow-path (ctx start end right)
  "Make on CTX the path of an arrow from START to END, each (X . Y).
The line bends into END level and the head points RIGHT, else left."
  (let* ((head shipit-atlassian-kanban--arrow-head)
         (base (- (car end) (if right head (- head))))
         (bend (/ (- base (car start)) 2.0)))
    (canvas-cairo-new-path ctx)
    (canvas-cairo-move-to ctx (car start) (cdr start))
    (canvas-cairo-curve-to ctx (+ (car start) bend) (cdr start)
                           (- base bend) (cdr end) base (cdr end))
    (canvas-cairo-line-to ctx base (- (cdr end) (/ head 2.0)))
    (canvas-cairo-line-to ctx (car end) (cdr end))
    (canvas-cairo-line-to ctx base (+ (cdr end) (/ head 2.0)))
    (canvas-cairo-line-to ctx base (cdr end))))

(defun shipit-atlassian-kanban--draw-arrow (ctx from to)
  "Draw on CTX an arrow from the box FROM to the box TO, in other columns.
It leaves FROM from the side facing TO.  A wide stroke in the
background colour goes first, so the arrow reads over a card."
  (let* ((right (> (car to) (car from)))
         (start (shipit-atlassian-kanban--side-middle from right))
         (end (shipit-atlassian-kanban--side-middle to (not right))))
    (shipit-atlassian-kanban--arrow-path ctx start end right)
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :background) 0.85)
    (canvas-cairo-set-line-width ctx 6)
    (canvas-cairo-stroke ctx)
    (shipit-atlassian-kanban--arrow-path ctx start end right)
    (canvas-diagram-set-rgb ctx (shipit-atlassian-kanban--timeline-rgb) 0.95)
    (canvas-cairo-set-line-width ctx 2)
    (canvas-cairo-stroke ctx)))

(defun shipit-atlassian-kanban--draw-ghost (ctx ghost label)
  "Draw GHOST on CTX: a dashed box in the timeline colour, saying LABEL."
  (pcase-let* ((`(,x ,y ,w ,h) (shipit-atlassian-kanban--ghost-box ghost))
               (rgb (shipit-atlassian-kanban--timeline-rgb))
               (font (canvas-diagram-font))
               (size (canvas-cairo-text-size ctx label font)))
    (canvas-diagram-rounded-rect ctx x y w h 6)
    (canvas-diagram-set-rgb ctx rgb 0.15)
    (canvas-cairo-fill ctx t)
    (canvas-diagram-set-rgb ctx rgb 0.9)
    (canvas-cairo-set-line-width ctx 1.5)
    (canvas-cairo-set-dash ctx [6 4])
    (canvas-cairo-stroke ctx)
    (canvas-cairo-set-dash ctx [])
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :text) 0.8)
    (canvas-cairo-text ctx (+ x 8) (+ y (/ (- h (cdr size)) 2.0)) label font)))

(defun shipit-atlassian-kanban--draw-ghosts (diagram ctx)
  "Draw on CTX the ghost of every moved card of DIAGRAM."
  (let ((now (shipit-atlassian-kanban--now-side-p diagram)))
    (shipit-atlassian-kanban--each-ghost
     diagram
     (lambda (ghost _node)
       (shipit-atlassian-kanban--draw-ghost
        ctx ghost (format (if now "%s (then)" "%s (now)") (plist-get ghost :key)))))))

(defun shipit-atlassian-kanban--draw-arrows (diagram ctx)
  "Draw on CTX the arrow between every moved card of DIAGRAM and its ghost.
Drawn after every card."
  (let ((now (shipit-atlassian-kanban--now-side-p diagram)))
    (shipit-atlassian-kanban--each-ghost
     diagram
     (lambda (ghost node)
       (let ((card (shipit-atlassian-kanban--node-box node))
             (place (shipit-atlassian-kanban--ghost-box ghost)))
         (if now
             (shipit-atlassian-kanban--draw-arrow ctx place card)
           (shipit-atlassian-kanban--draw-arrow ctx card place)))))))

;;; Moving between the cards

(defun shipit-atlassian-kanban--header-node (model column-index)
  "Return the header node of column COLUMN-INDEX in MODEL, or nil."
  (when-let* ((column (shipit-atlassian-kanban--column-at model column-index)))
    (plist-get (gethash (shipit-atlassian-kanban-column-name column)
                        (shipit-atlassian-kanban-board-headers model))
               :node)))

(defun shipit-atlassian-kanban--card-node (model column-index row)
  "Return the node at ROW of column COLUMN-INDEX in MODEL, or nil.
A collapsed column shows no cards, so it answers nil at every row."
  (let* ((column (shipit-atlassian-kanban--column-at model column-index))
         (cards (and column
                     (not (shipit-atlassian-kanban-column-collapsed column))
                     (shipit-atlassian-kanban-column-cards column))))
    (when (and cards (>= row 0) (< row (length cards)))
      (plist-get (shipit-atlassian-kanban--card
                  model (shipit-atlassian-board--issue-key (nth row cards)))
                 :node))))

(defun shipit-atlassian-kanban--node-in (model column-index row)
  "Return what the keyboard lands on at ROW of column COLUMN-INDEX.
ROW -1 is the header.  A row past the end of the column gives its last
card, and a column with no card to land on gives its header, so a step
sideways always arrives somewhere."
  (when (shipit-atlassian-kanban--column-at model column-index)
    (if (< row 0)
        (shipit-atlassian-kanban--header-node model column-index)
      (or (shipit-atlassian-kanban--card-node model column-index row)
          (shipit-atlassian-kanban--last-card model column-index)
          (shipit-atlassian-kanban--header-node model column-index)))))

(defun shipit-atlassian-kanban--last-card (model column-index)
  "Return the last card node of column COLUMN-INDEX in MODEL, or nil."
  (when-let* ((column (shipit-atlassian-kanban--column-at model column-index))
              (cards (and (not (shipit-atlassian-kanban-column-collapsed column))
                          (shipit-atlassian-kanban-column-cards column))))
    (shipit-atlassian-kanban--card-node model column-index (1- (length cards)))))

(defun shipit-atlassian-kanban--row-of (model node)
  "Return the row NODE sits on: -1 for a column header, else its card row."
  (if (shipit-atlassian-kanban--header-of model node)
      -1
    (plist-get (shipit-atlassian-kanban--card-of model node) :row)))

(defun shipit-atlassian-kanban--last-column (model)
  "Index of MODEL\='s last column."
  (1- (length (shipit-atlassian-kanban-board-columns model))))

(defun shipit-atlassian-kanban--edge-node (model direction)
  "Return the first or last node of MODEL, by DIRECTION."
  (let ((last (shipit-atlassian-kanban--last-column model)))
    (if (eq direction 'first)
        (shipit-atlassian-kanban--header-node model 0)
      (or (shipit-atlassian-kanban--last-card model last)
          (shipit-atlassian-kanban--header-node model last)))))

(defun shipit-atlassian-kanban--step (model column-index row)
  "Return the node at ROW of column COLUMN-INDEX, or nil at either end.
Used by the moves that must stop rather than hold: up off the header
and down off the last card."
  (if (< row -1)
      nil
    (if (< row 0)
        (shipit-atlassian-kanban--header-node model column-index)
      (shipit-atlassian-kanban--card-node model column-index row))))

(defun shipit-atlassian-kanban--move (diagram node direction)
  "Return the node DIRECTION from NODE in DIAGRAM, or nil to hold.
Left and right walk the columns, holding the row; up and down walk a
column\='s header and its cards and stop at either end."
  (let* ((model (canvas-diagram-model diagram))
         (ci (shipit-atlassian-kanban--column-index-of model node))
         (row (shipit-atlassian-kanban--row-of model node)))
    (pcase direction
      ((or 'in 'next-at-depth) (shipit-atlassian-kanban--node-in model (1+ ci) row))
      ((or 'out 'previous-at-depth) (shipit-atlassian-kanban--node-in model (1- ci) row))
      ((or 'next 'next-sibling) (shipit-atlassian-kanban--step model ci (1+ row)))
      ((or 'previous 'previous-sibling) (shipit-atlassian-kanban--step model ci (1- row)))
      ('next-branch (shipit-atlassian-kanban--header-node model (1+ ci)))
      ('branch (shipit-atlassian-kanban--header-node model (1- ci)))
      ('first (shipit-atlassian-kanban--edge-node model 'first))
      ('last (shipit-atlassian-kanban--edge-node model 'last)))))

;;; The menu
;;
;; `canvas-diagram-define-menu' and `canvas-diagram-define-setting' are
;; macros, and they exist only once canvas-diagram is loaded.  This file
;; has to load without it, so the definitions are evaluated when
;; canvas-diagram arrives rather than expanded here.  canvas-diagram
;; adds the shared zoom, box, colour and icon groups after this one, so
;; the keys below stay clear of theirs.

(with-eval-after-load 'canvas-diagram
  (eval
   '(progn
      (canvas-diagram-define-setting shipit-atlassian-kanban-cycle-card-width
        shipit-atlassian-kanban-card-width '(200 240 300 360)
        "Cycle how wide a card is drawn, and lay the board out again.")

      (canvas-diagram-define-setting shipit-atlassian-kanban-cycle-staged-color
        shipit-atlassian-kanban-staged-color shipit-atlassian-kanban-staged-colors
        "Cycle the colour a card takes while a move is staged for it.")

      (canvas-diagram-define-menu shipit-atlassian-kanban-menu
        "Move a card, send the moves to Jira, and change how the board is drawn.
The keys of the board itself still work while this is up, so a move can
be staged and its effect seen without closing the menu."
        ["Move the card"
         (">" shipit-atlassian-kanban-stage-forward :transient t
          :description "stage one column on")
         ("<" shipit-atlassian-kanban-stage-backward :transient t
          :description "stage one column back")]
        ["Staged moves"
         ("x" shipit-atlassian-kanban-commit :transient t
          :description (lambda ()
                         (format "%-10s %d staged"
                                 "send to Jira"
                                 (length shipit-atlassian-kanban--pending))))
         ("u" shipit-atlassian-kanban-discard :transient t
          :description "discard every staged move")
         ("S" shipit-atlassian-kanban-cycle-staged-color :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "colour" 'shipit-atlassian-kanban-staged-color)))]
        ["Find and narrow"
         ("f" shipit-atlassian-kanban-find-card
          :description "find a card by key or summary")
         ("n" shipit-atlassian-kanban-search
          :description "narrow as you type, then go to a card")
         ("a" shipit-atlassian-kanban-filter-mine :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "only mine"
                          'shipit-atlassian-kanban--filter-mine-p)))
         ("t" shipit-atlassian-kanban-filter-type :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "only type" 'shipit-atlassian-kanban--filter-type "all")))
         ("p" shipit-atlassian-kanban-filter-priority :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "only priority" 'shipit-atlassian-kanban--filter-priority "all")))
         ("/" shipit-atlassian-kanban-focus :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "matching" 'shipit-atlassian-kanban--filter-text "all")))]
        ["The board"
         ("TAB" shipit-atlassian-kanban-toggle-column :transient t
          :description "collapse this column, or open it")
         ("r" shipit-atlassian-kanban-refresh :transient t
          :description "read the board again")
         ("o" shipit-atlassian-kanban-visit-issue
          :description "open this issue in shipit")
         ("c" shipit-atlassian-kanban-cycle-card-width :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "card width" 'shipit-atlassian-kanban-card-width)))]
        ["Timeline"
         ("M-p" shipit-atlassian-kanban-timeline-earlier :transient t
          :description "one step back in time")
         ("M-n" shipit-atlassian-kanban-timeline-later :transient t
          :description "one step later")
         ("T" shipit-atlassian-kanban-timeline-cycle-step :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "step" 'shipit-atlassian-kanban--timeline-step)))
         ("d" shipit-atlassian-kanban-timeline-changes
          :description "the moves since then")
         ("D" shipit-atlassian-kanban-timeline-side-by-side
          :description "now beside then")
         ("H" shipit-atlassian-kanban-timeline-toggle-unchanged :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "only moved" 'shipit-atlassian-kanban--only-moved)))
         ("A" shipit-atlassian-kanban-timeline-toggle-arrows :transient t
          :description (lambda ()
                         (canvas-diagram-setting
                          "arrows" 'shipit-atlassian-kanban--arrows)))]))
   t))

;;; How the board plugs into canvas-diagram

(defun shipit-atlassian-kanban--node-text (diagram node)
  "Return what NODE says in DIAGRAM, whether it is a card or a header."
  (let ((model (canvas-diagram-model diagram)))
    (plist-get (or (shipit-atlassian-kanban--card-of model node)
                   (shipit-atlassian-kanban--header-of model node))
               :text)))

(defun shipit-atlassian-kanban--node-rgb (diagram node)
  "Return the fill of NODE in DIAGRAM, a card or a column header.
A header takes the palette colour of its column.  A staged card takes
the staged colour, so a move shows without opening anything.  On the
timeline a card that moved or was made since then takes the timeline
colour.  Any other card takes the colour of the page: the header and
the frame show the column, so a colour of the column would only
compete with the colours that mean something."
  (let* ((model (canvas-diagram-model diagram))
         (card (shipit-atlassian-kanban--card-of model node)))
    (cond
     ((null card) (canvas-diagram-palette-rgb
                   (shipit-atlassian-kanban--column-index-of model node)))
     ((plist-get card :staged)
      (or (canvas-diagram-rgb shipit-atlassian-kanban-staged-color)
          (canvas-diagram-color :selection)))
     ((shipit-atlassian-kanban--changed-since-p (plist-get card :issue))
      (shipit-atlassian-kanban--timeline-rgb))
     (t (canvas-diagram-color :background)))))

(defun shipit-atlassian-kanban--header (diagram node)
  "Return the header line of NODE in DIAGRAM: its board and its column.
On the timeline a banner comes first, which says so and how to leave."
  (let* ((model (canvas-diagram-model diagram))
         (place (format "%s › %s"
                        (shipit-atlassian-kanban-board-name model)
                        (shipit-atlassian-kanban-column-name
                         (shipit-atlassian-kanban--column-of model node))))
         (timeline (shipit-atlassian-kanban-board-timeline model)))
    (if timeline
        (concat (shipit-atlassian-kanban--timeline-banner timeline) "   " place)
      place)))

(defun shipit-atlassian-kanban--column-popup (model column)
  "Return (TITLE PATH BODY) for COLUMN\='s header."
  (list (shipit-atlassian-kanban-column-name column)
        (shipit-atlassian-kanban-board-name model)
        (string-join
         (list (format "Cards: %d"
                       (length (shipit-atlassian-kanban-column-cards column)))
               (format "Statuses: %s"
                       (string-join (shipit-atlassian-kanban-column-statuses column)
                                    ", ")))
         "\n")))

(defun shipit-atlassian-kanban--card-popup (diagram node)
  "Return (TITLE PATH BODY) for NODE in DIAGRAM, a card or a column header."
  (let* ((model (canvas-diagram-model diagram))
         (key (canvas-diagram-node-label node)))
    (if (shipit-atlassian-kanban--header-of model node)
        (shipit-atlassian-kanban--column-popup
         model (shipit-atlassian-kanban--column-of model node))
      (let ((issue (plist-get (shipit-atlassian-kanban--card model key) :issue)))
        (list (format "%s  %s" key (shipit-atlassian-kanban--summary issue))
              (shipit-atlassian-kanban--header diagram node)
              (shipit-atlassian-kanban--note model key))))))

(defun shipit-atlassian-kanban--content (diagram node)
  "Return (HEADER BODY) that copying NODE in DIAGRAM copies."
  (let* ((model (canvas-diagram-model diagram))
         (key (canvas-diagram-node-label node)))
    (if (shipit-atlassian-kanban--header-of model node)
        (pcase-let ((`(,title ,_path ,body)
                     (shipit-atlassian-kanban--card-popup diagram node)))
          (list title body))
      (let ((issue (plist-get (shipit-atlassian-kanban--card model key) :issue)))
        (list (format "%s %s" key (shipit-atlassian-kanban--summary issue))
              (shipit-atlassian-kanban--note model key))))))

(defun shipit-atlassian-kanban--node-key (diagram node)
  "Return the key NODE is found by again once DIAGRAM is rebuilt.
A card goes by its issue key, which is also what holds a mark on it.
A header goes by `(column . NAME)\=': a column name is no issue key, and
looking it up among the cards found nothing, so the keyboard left it."
  (if (shipit-atlassian-kanban--header-of (canvas-diagram-model diagram) node)
      (cons 'column (canvas-diagram-node-label node))
    (canvas-diagram-node-label node)))

(defun shipit-atlassian-kanban--header-named (model name)
  "Return the header node of the column called NAME in MODEL, or nil."
  (plist-get (gethash name (shipit-atlassian-kanban-board-headers model))
             :node))

(defun shipit-atlassian-kanban--restore (diagram key)
  "Return the node of the rebuilt DIAGRAM that KEY names, or nil.
A card that is no longer drawn because its column is shut gives that
column\='s header.  Without it canvas-diagram falls back to the first
node, and folding a column threw the keyboard to the leftmost one."
  (let ((model (canvas-diagram-model diagram)))
    (if (consp key)
        (shipit-atlassian-kanban--header-named model (cdr key))
      (let ((card (shipit-atlassian-kanban--card model key)))
        (or (plist-get card :node)
            (and card
                 (shipit-atlassian-kanban--header-named
                  model (shipit-atlassian-kanban-column-name
                         (plist-get card :column)))))))))

(defconst shipit-atlassian-kanban--callbacks
  (list :build #'shipit-atlassian-kanban--build
        :layout #'shipit-atlassian-kanban--lay-out
        :draw-edges #'shipit-atlassian-kanban--draw-columns
        :node-text #'shipit-atlassian-kanban--node-text
        :node-rgb #'shipit-atlassian-kanban--node-rgb
        :header #'shipit-atlassian-kanban--header
        :card #'shipit-atlassian-kanban--card-popup
        :content #'shipit-atlassian-kanban--content
        :move #'shipit-atlassian-kanban--move
        :node-key #'shipit-atlassian-kanban--node-key
        :restore #'shipit-atlassian-kanban--restore
        :go #'shipit-atlassian-kanban--follow
        :zoom #'shipit-atlassian-kanban--zoom-along
        :draw-over #'shipit-atlassian-kanban--draw-over
        :menu 'shipit-atlassian-kanban-menu)
  "How the board plugs into canvas-diagram.")

(defun shipit-atlassian-kanban--diagram ()
  "Return a fresh diagram of a board.
The slack is the column frame, which reaches past the cards by the
frame padding on the right and below."
  (shipit-atlassian-kanban--require-canvas)
  (canvas-diagram-create
   :callbacks shipit-atlassian-kanban--callbacks
   :slack (cons shipit-atlassian-kanban--frame-pad
                shipit-atlassian-kanban--frame-pad)))

;;; Export

(defun shipit-atlassian-kanban-export (board file)
  "Draw BOARD into FILE, a PNG, an SVG or a PDF by its name; return FILE.
No frame or buffer is needed, so this works in batch."
  (shipit-atlassian-kanban--require-canvas)
  (canvas-diagram-export (shipit-atlassian-kanban--diagram)
                         (shipit-atlassian-kanban--spec board nil)
                         file))

;;; The buffer

(defvar-local shipit-atlassian-kanban--repo nil
  "Repository this board was opened for.")

(defvar-local shipit-atlassian-kanban--config nil
  "Jira backend config plist for this buffer.")

(defvar-local shipit-atlassian-kanban--board-id nil
  "Identifier of the board this buffer draws.")

(defvar-local shipit-atlassian-kanban--board nil
  "The board alist this buffer draws, as last fetched.")

(defvar-local shipit-atlassian-kanban--pending nil
  "Staged moves, an alist of issue key to target status name.
Nothing here has reached Jira yet.  `shipit-atlassian-kanban-commit'
sends them and `shipit-atlassian-kanban-discard' drops them.")

(defvar-local shipit-atlassian-kanban--collapsed nil
  "Names of the columns shown collapsed in this buffer.
Kept here rather than in the board data, so it outlives a refetch.")

(defvar-local shipit-atlassian-kanban--me nil
  "Account id of the current user on this Jira instance, or nil.
Read once when the board opens; it marks the cards they are assigned
and answers the `:mine' part of the filter.")

(defvar-local shipit-atlassian-kanban--filter nil
  "What this buffer narrows the board to, a plist, or nil for all of it.
See `shipit-atlassian-kanban--passes-p'.")

(defvar-local shipit-atlassian-kanban--timeline nil
  "How far back in time the board is shown, in seconds, or nil for now.
While it is set the board is read-only, since an old board cannot be
moved.  Kept as a distance rather than a time, so that stepping later
as often as earlier comes back to now.")

(defvar-local shipit-atlassian-kanban--timeline-step 'day
  "How far one step of the timeline goes.
A key of `shipit-atlassian-kanban--timeline-steps'.")

(defvar-local shipit-atlassian-kanban--histories nil
  "The history of every issue on the board, or `reading\\=' while Jira has
not answered, or nil before it is asked.  The history is an alist of
issue key to history; see shipit-atlassian-history.el.")

(defvar-local shipit-atlassian-kanban--only-moved nil
  "Non-nil when the timeline shows only the cards that moved since then.")

(defvar-local shipit-atlassian-kanban--arrows t
  "Non-nil when the timeline draws where each moved card went.")

(defvar-local shipit-atlassian-kanban--changes nil
  "The buffer listing the moves since the time the timeline shows, or nil.")

(defvar-local shipit-atlassian-kanban--beside nil
  "The buffer showing the board now beside this board of then, or nil.")

(defvar-local shipit-atlassian-kanban--compared-with nil
  "The board of then this buffer shows the board now beside, or nil.
Such a buffer is only there to compare, so it cannot change the board.")

(defvar-local shipit-atlassian-kanban--board-filter nil
  "The JQL of the board\\='s filter, `none\\=' when it has none, or nil
before it is read.")

(defvar-local shipit-atlassian-kanban--left nil
  "The cards the board had at a time on the timeline and no longer has.
An alist of the timeline\\='s distance back to an alist of issue key to
history, each carrying its issue: the board does not have them, so
they come from a search, one for each time, as the filter matched
differently then.")

(defvar-local shipit-atlassian-kanban--left-reading nil
  "The distance back whose lost cards are being read, or nil.")

(defvar-local shipit-atlassian-kanban--tracker nil
  "The plist of the issue backend whose issues this board shows.")

(defvar-local shipit-atlassian-kanban--drawing nil
  "The `canvas-diagram' this buffer draws, kept so its spec can be replaced.")

(defun shipit-atlassian-kanban--buffer-name (repo board &optional side)
  "Return the buffer name for BOARD of REPO.
SIDE names the second buffer of a comparison."
  (format "*shipit-kanban: %s %s%s*" repo (or (cdr (assq 'name board)) "board")
          (if side (format " (%s)" side) "")))

(defun shipit-atlassian-kanban--update-mode-line ()
  "Show how many moves are staged, or nothing when none are."
  (setq mode-line-process
        (string-join
         (delq nil
               (list (when shipit-atlassian-kanban--timeline
                       (propertize " TIMELINE" 'face 'shipit-atlassian-kanban-timeline))
                     (when (buffer-live-p shipit-atlassian-kanban--compared-with)
                       (propertize " NOW" 'face 'shipit-atlassian-kanban-timeline))
                     (when shipit-atlassian-kanban--pending
                       (format " staged:%d"
                               (length shipit-atlassian-kanban--pending)))
                     (when (shipit-atlassian-kanban--filtered-p
                            shipit-atlassian-kanban--filter)
                       " filtered")))))
  (when (equal "" mode-line-process)
    (setq mode-line-process nil))
  (force-mode-line-update))

(defun shipit-atlassian-kanban--redraw ()
  "Draw the board again from the current board data and staged moves.
The keyboard stays on its card, which canvas-diagram restores by the
issue key.  On the timeline the board is the one of then, and the
staged moves are left out: they belong to the board now.  The board of
now beside it follows."
  (shipit-canvas-set-slot
   'canvas-diagram 'spec shipit-atlassian-kanban--drawing
   (shipit-atlassian-kanban--spec (shipit-atlassian-kanban--shown-board)
                                  (unless (shipit-atlassian-kanban--read-only-p)
                                    shipit-atlassian-kanban--pending)
                                  shipit-atlassian-kanban--collapsed
                                  shipit-atlassian-kanban--me
                                  (shipit-atlassian-kanban--shown-filter)
                                  (shipit-atlassian-kanban--timeline-spec)
                                  shipit-atlassian-kanban--tracker))
  (canvas-diagram-rebuild)
  (canvas-diagram-redraw)
  (shipit-atlassian-kanban--update-mode-line)
  (when (buffer-live-p shipit-atlassian-kanban--beside)
    (with-current-buffer shipit-atlassian-kanban--beside
      (shipit-atlassian-kanban--redraw))))

;;; Staging a move

(defun shipit-atlassian-kanban--selected-node ()
  "Return the node the keyboard is on."
  (or (canvas-diagram-selected) (user-error "Nothing here")))

(defun shipit-atlassian-kanban--selected-key ()
  "Return the issue key of the card the keyboard is on.
A column header is not a card, so it raises rather than staging
something odd."
  (let ((node (shipit-atlassian-kanban--selected-node)))
    (when (shipit-atlassian-kanban--header-of (canvas-diagram-current-model) node)
      (user-error "No card here: this is the %s column"
                  (canvas-diagram-node-label node)))
    (canvas-diagram-node-label node)))

(defun shipit-atlassian-kanban--issue (key)
  "Return the issue alist for KEY in this buffer\='s model."
  (plist-get (shipit-atlassian-kanban--card (canvas-diagram-current-model) key)
             :issue))

(defun shipit-atlassian-kanban--actual-status (key)
  "Return the status KEY really has in Jira, whatever is staged for it."
  (cdr (assq 'state (shipit-atlassian-kanban--issue key))))

(defun shipit-atlassian-kanban--effective-status (key)
  "Return the status KEY counts as now: its staged target, else its real one.
Staging from the effective status is what makes two presses skip two
columns."
  (or (cdr (assoc key shipit-atlassian-kanban--pending))
      (shipit-atlassian-kanban--actual-status key)))

(defun shipit-atlassian-kanban--stage (direction)
  "Stage a move one column DIRECTION for the card the keyboard is on."
  (shipit-atlassian-kanban--refuse-on-the-timeline)
  (let* ((key (shipit-atlassian-kanban--selected-key))
         (boards (list shipit-atlassian-kanban--board))
         (effective (shipit-atlassian-kanban--effective-status key))
         (target (shipit-atlassian-board--target-status boards effective direction)))
    (unless target
      (user-error "No %s column from %s on this board"
                  direction (or effective "an unknown status")))
    (setq shipit-atlassian-kanban--pending
          (shipit-atlassian-board--stage-move
           shipit-atlassian-kanban--pending key
           (shipit-atlassian-kanban--actual-status key) target))
    (shipit-atlassian-kanban--redraw)))

(defun shipit-atlassian-kanban-stage-forward ()
  "Stage a move of this card to the next column.
Nothing reaches Jira until \\[shipit-atlassian-kanban-commit]."
  (interactive)
  (shipit-atlassian-kanban--stage 'forward))

(defun shipit-atlassian-kanban-stage-backward ()
  "Stage a move of this card to the previous column."
  (interactive)
  (shipit-atlassian-kanban--stage 'backward))

(defun shipit-atlassian-kanban-discard ()
  "Drop every staged move and draw each card in its own column again."
  (interactive)
  (shipit-atlassian-kanban--refuse-on-the-timeline)
  (let ((n (length shipit-atlassian-kanban--pending)))
    (when (zerop n)
      (user-error "No staged moves to discard"))
    (when (or (= n 1)
              (yes-or-no-p (format "Discard %d staged move%s? "
                                   n (if (= n 1) "" "s"))))
      (setq shipit-atlassian-kanban--pending nil)
      (shipit-atlassian-kanban--redraw)
      (message "Discarded %d staged move%s" n (if (= n 1) "" "s")))))

(defun shipit-atlassian-kanban-commit ()
  "Send every staged move to Jira, then read the board again.
A move Jira refuses stays staged, so it can be retried or discarded.
This blocks while the transitions run."
  (interactive)
  (shipit-atlassian-kanban--refuse-on-the-timeline)
  (unless shipit-atlassian-kanban--pending
    (user-error "No staged moves to commit"))
  (pcase-let ((`(,committed ,remaining ,failures)
               (shipit-atlassian-board--commit-pending
                (list shipit-atlassian-kanban--board)
                shipit-atlassian-kanban--repo
                shipit-atlassian-kanban--pending)))
    (setq shipit-atlassian-kanban--pending remaining)
    (cond
     (failures
      (shipit-atlassian-kanban--redraw)
      (message "Moved %d, failed %d: %s" committed (length failures)
               (mapconcat #'identity failures "; ")))
     (t
      (message "Moved %d card%s" committed (if (= committed 1) "" "s"))
      (shipit-atlassian-kanban-refresh)))))

;;; Narrowing what the board shows

(defvar-local shipit-atlassian-kanban--filter-mine-p nil
  "Whether the filter is narrowed to the current user\='s cards.
The menu reads a variable to describe a setting, so each part of the
filter has one alongside `shipit-atlassian-kanban--filter\='.")

(defvar-local shipit-atlassian-kanban--filter-type nil
  "The issue type the filter is narrowed to, for the menu to show.")

(defvar-local shipit-atlassian-kanban--filter-priority nil
  "The priority the filter is narrowed to, for the menu to show.")

(defvar-local shipit-atlassian-kanban--filter-text nil
  "The text the filter is narrowed to, for the menu to show.")

(defun shipit-atlassian-kanban--narrow (&rest changes)
  "Apply CHANGES, a plist, to this buffer\='s filter and draw again."
  (let ((filter (copy-sequence shipit-atlassian-kanban--filter)))
    (while changes
      (setq filter (plist-put filter (pop changes) (pop changes))))
    (setq shipit-atlassian-kanban--filter filter))
  (setq shipit-atlassian-kanban--filter-mine-p
        (plist-get shipit-atlassian-kanban--filter :mine)
        shipit-atlassian-kanban--filter-type
        (plist-get shipit-atlassian-kanban--filter :type)
        shipit-atlassian-kanban--filter-priority
        (plist-get shipit-atlassian-kanban--filter :priority)
        shipit-atlassian-kanban--filter-text
        (plist-get shipit-atlassian-kanban--filter :text))
  (shipit-atlassian-kanban--redraw))

(defun shipit-atlassian-kanban-filter-mine ()
  "Show only the cards assigned to you, or show them all again."
  (interactive)
  (unless shipit-atlassian-kanban--me
    (user-error "Jira did not say who you are, so `mine\=' has nothing to match"))
  (shipit-atlassian-kanban--narrow
   :mine (not (plist-get shipit-atlassian-kanban--filter :mine))))

(defun shipit-atlassian-kanban--types-on-the-board ()
  "Return the issue types the board holds, in order of first appearance."
  (shipit-atlassian-kanban--types-in
   (shipit-atlassian-kanban-board-columns (canvas-diagram-current-model))))

(defun shipit-atlassian-kanban-filter-type (type)
  "Show only the cards of issue TYPE.  An empty answer shows them all."
  (interactive
   (list (completing-read "Type (empty for all): "
                          (shipit-atlassian-kanban--types-on-the-board)
                          nil nil nil nil "")))
  (shipit-atlassian-kanban--narrow
   :type (unless (string-empty-p type) type)))

(defun shipit-atlassian-kanban--priorities-on-the-board ()
  "Return the priorities the board holds, in order of first appearance."
  (shipit-atlassian-kanban--values-in
   (shipit-atlassian-kanban-board-columns (canvas-diagram-current-model))
   #'shipit-atlassian-kanban--priority))

(defun shipit-atlassian-kanban-filter-priority (priority)
  "Show only the cards of PRIORITY.  An empty answer shows them all."
  (interactive
   (list (completing-read "Priority (empty for all): "
                          (shipit-atlassian-kanban--priorities-on-the-board)
                          nil nil nil nil "")))
  (shipit-atlassian-kanban--narrow
   :priority (unless (string-empty-p priority) priority)))

(defun shipit-atlassian-kanban-filter-text (text)
  "Show only the cards whose key or summary carries TEXT.
An empty answer shows them all again."
  (interactive
   (list (read-string "Cards matching (empty for all): "
                      (plist-get shipit-atlassian-kanban--filter :text))))
  (shipit-atlassian-kanban--narrow
   :text (unless (string-empty-p text) text)))

(defconst shipit-atlassian-kanban--invert-prefix "! "
  "What at the front of a filter reverses it.
The same thing reverses consult\='s line filters, and this is bound to
their keys.")

(defun shipit-atlassian-kanban--parse-filter-text (input)
  "Return (TEXT . INVERT) for INPUT, a filter somebody typed."
  (if (string-prefix-p shipit-atlassian-kanban--invert-prefix input)
      (cons (substring input (length shipit-atlassian-kanban--invert-prefix)) t)
    (cons input nil)))

(defun shipit-atlassian-kanban--filter-input ()
  "Return what the reader should start from: the text filter as typed."
  (let ((text (plist-get shipit-atlassian-kanban--filter :text)))
    (cond ((null text) "")
          ((plist-get shipit-atlassian-kanban--filter :text-invert)
           (concat shipit-atlassian-kanban--invert-prefix text))
          (t text))))

(defun shipit-atlassian-kanban--narrow-text (input)
  "Narrow this buffer to INPUT, a filter as typed, and draw again."
  (pcase-let ((`(,text . ,invert)
               (shipit-atlassian-kanban--parse-filter-text (or input ""))))
    (let ((wanted (unless (string-empty-p text) text)))
      (shipit-atlassian-kanban--narrow
       :text wanted :text-invert (and wanted invert)))))

(defvar shipit-atlassian-kanban--focus-history nil
  "What has been typed into the focus reader before.")

(defcustom shipit-atlassian-kanban-focus-debounce 0.2
  "Seconds of quiet before the focus reader narrows the board.
Laying a board of a hundred and seventy cards out again costs some
sixty milliseconds, so narrowing on every keystroke would drag behind
the typing.  The board narrows once the typing pauses instead."
  :type 'number
  :group 'shipit-atlassian)

(defun shipit-atlassian-kanban--focus-live ()
  "Read a filter with consult, narrowing the board on every keystroke.
Leaving without an answer puts the filter back the way it was."
  (let ((before shipit-atlassian-kanban--filter)
        (buffer (current-buffer))
        (answered nil))
    (unwind-protect
        (progn
          (shipit-atlassian-kanban--narrow-text
           (consult--prompt
            :prompt "Focus on: "
            :initial (shipit-atlassian-kanban--filter-input)
            :history 'shipit-atlassian-kanban--focus-history
            :preview-key `(:debounce ,shipit-atlassian-kanban-focus-debounce any)
            :state (lambda (action input)
                     (when (and (eq action 'preview) (buffer-live-p buffer))
                       (with-current-buffer buffer
                         (shipit-atlassian-kanban--narrow-text input))))))
          (setq answered t))
      (unless answered
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq shipit-atlassian-kanban--filter before)
            (shipit-atlassian-kanban--redraw)))))))

(defun shipit-atlassian-kanban-focus (&optional show)
  "Narrow the board to the cards matching what you type, as you type.
With SHOW, a prefix argument, show every card again.  A leading `! \='
reverses the match.  Both work the way they do in
`consult-focus-lines\=', whose keys reach this.

Without consult there is no preview: the text is read first and the
board narrows once."
  (interactive "P")
  (cond
   (show (shipit-atlassian-kanban--narrow :text nil :text-invert nil))
   ((shipit-atlassian-kanban--consult-available-p)
    (shipit-atlassian-kanban--focus-live))
   (t (call-interactively #'shipit-atlassian-kanban-filter-text))))

;;; Finding a card

(defun shipit-atlassian-kanban--find-candidates ()
  "Return an alist of what each drawn card says to the issue key it draws.
A key rather than a node: narrowing the board builds every node again,
and a node caught before that is no longer one of the drawn ones."
  (let ((model (canvas-diagram-current-model)))
    (delq nil
          (mapcar
           (lambda (node)
             (unless (shipit-atlassian-kanban--header-of model node)
               (let* ((key (canvas-diagram-node-label node))
                      (issue (plist-get (shipit-atlassian-kanban--card model key)
                                        :issue)))
                 (cons (format "%s  %s" key
                               (shipit-atlassian-kanban--summary issue))
                       key))))
           (canvas-diagram-nodes-shown)))))

(defun shipit-atlassian-kanban--card-group (candidates)
  "Return a function putting each of CANDIDATES under its column, for consult.
A board is columns before it is a list, so the cards to choose from
read better under the headings they sit in.

The columns are worked out here, while the board is the buffer.  The
completion framework calls the function it gets back from the
minibuffer, which holds no diagram to ask."
  (let ((model (canvas-diagram-current-model))
        (columns (make-hash-table :test #'equal)))
    (pcase-dolist (`(,candidate . ,key) candidates)
      (when-let* ((card (shipit-atlassian-kanban--card model key)))
        (puthash candidate
                 (shipit-atlassian-kanban-column-name (plist-get card :column))
                 columns)))
    (lambda (candidate transform)
      (if transform candidate (gethash candidate columns)))))

(defun shipit-atlassian-kanban--go-to (node)
  "Put the keyboard on NODE, bring it into view and draw again.
Through canvas-diagram\='s own move, which scrolls to the node; setting
the selection alone left a card far down a long column off the screen."
  (canvas-diagram-go node))

(defun shipit-atlassian-kanban--go-to-key (key)
  "Put the keyboard on the card drawing issue KEY, if it is drawn."
  (when-let* ((node (plist-get (shipit-atlassian-kanban--card
                                (canvas-diagram-current-model) key)
                               :node)))
    (shipit-atlassian-kanban--go-to node)
    t))

(defun shipit-atlassian-kanban--preview (candidates)
  "Return a function previewing a choice among CANDIDATES, for consult.
Moving the keyboard is the preview, and leaving without choosing puts
it back where it was.

The board is named here and made current there: the function it gives
back is called while the minibuffer is in use."
  (let ((buffer (current-buffer))
        (start (and (canvas-diagram-selected)
                    (canvas-diagram-node-label (canvas-diagram-selected)))))
    (lambda (action candidate)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (pcase action
            ('preview (when-let* ((key (cdr (assoc candidate candidates))))
                        (shipit-atlassian-kanban--go-to-key key)))
            ('return (unless candidate
                       (when start
                         (shipit-atlassian-kanban--go-to-key start))))))))))

(defun shipit-atlassian-kanban--consult-available-p ()
  "Return non-nil when consult is there to read a card with a preview."
  (and (require 'consult nil t)
       (fboundp 'consult--read)
       t))

(defun shipit-atlassian-kanban--read-card (candidates)
  "Ask which of CANDIDATES to go to; return its issue key, or nil.
Uses consult when it is there, for the preview as the answer narrows,
and `completing-read\=' when it is not."
  (let ((choice
         (if (shipit-atlassian-kanban--consult-available-p)
             (consult--read (mapcar #'car candidates)
                            :prompt "Card: "
                            :category 'shipit-atlassian-kanban-card
                            :require-match t
                            :sort nil
                            :group (shipit-atlassian-kanban--card-group candidates)
                            :state (shipit-atlassian-kanban--preview candidates))
           (completing-read "Card: " candidates nil t))))
    (cdr (assoc choice candidates))))

(defun shipit-atlassian-kanban-find-card ()
  "Find a card by its key or its summary and put the keyboard on it.
Bound to whatever key searches, because a canvas has no text for
`isearch\=' to walk.  The board is left as it is; see
`shipit-atlassian-kanban-search\=' for the one that narrows it."
  (interactive)
  (let ((candidates (shipit-atlassian-kanban--find-candidates)))
    (unless candidates
      (user-error "No cards to find"))
    (when-let* ((key (shipit-atlassian-kanban--read-card candidates)))
      (shipit-atlassian-kanban--go-to-key key))))

;;; Searching the board

(defun shipit-atlassian-kanban--minibuffer-input ()
  "Return what is typed in the minibuffer right now, or nil."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (minibuffer-contents-no-properties))))

(defun shipit-atlassian-kanban--search-state (candidates buffer before start)
  "Return the state function of the search, for consult.
It narrows BUFFER to what is typed and puts the keyboard on the card
being looked at.  CANDIDATES maps what a card says to its issue key.
Leaving without an answer puts the filter BEFORE and the card START
back."
  (lambda (action candidate)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (pcase action
          ('preview
           (shipit-atlassian-kanban--narrow-text
            (shipit-atlassian-kanban--minibuffer-input))
           (when-let* ((key (cdr (assoc candidate candidates))))
             (shipit-atlassian-kanban--go-to-key key)))
          ('return
           (unless candidate
             (setq shipit-atlassian-kanban--filter before)
             (shipit-atlassian-kanban--redraw)
             (when start (shipit-atlassian-kanban--go-to-key start)))))))))

(defun shipit-atlassian-kanban-search ()
  "Narrow the board to what you type, and go to the card you pick.
The cards offered are the ones still drawn, so the list and the board
say the same thing.  Leaving without picking one puts the board back.

A card can be picked that the board has since hidden, if consult
matched it more loosely than the board does; the filter is dropped so
the card is there to land on.  Without consult this finds a card
without narrowing anything."
  (interactive)
  (if (not (shipit-atlassian-kanban--consult-available-p))
      (shipit-atlassian-kanban-find-card)
    (let* ((candidates (shipit-atlassian-kanban--find-candidates))
           (buffer (current-buffer))
           (before shipit-atlassian-kanban--filter)
           (start (and (canvas-diagram-selected)
                       (canvas-diagram-node-label (canvas-diagram-selected))))
           (chosen nil))
      (unless candidates
        (user-error "No cards to search"))
      (unwind-protect
          (let ((choice
                 (consult--read
                  (mapcar #'car candidates)
                  :prompt "Card on the board: "
                  :category 'shipit-atlassian-kanban-card
                  :require-match t
                  :sort nil
                  :group (shipit-atlassian-kanban--card-group candidates)
                  :preview-key `(:debounce ,shipit-atlassian-kanban-focus-debounce any)
                  :state (shipit-atlassian-kanban--search-state
                          candidates buffer before start))))
            (setq chosen (cdr (assoc choice candidates))))
        (unless chosen
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq shipit-atlassian-kanban--filter before)
              (shipit-atlassian-kanban--redraw)
              (when start (shipit-atlassian-kanban--go-to-key start))))))
      (when chosen
        (unless (shipit-atlassian-kanban--go-to-key chosen)
          (setq shipit-atlassian-kanban--filter before)
          (shipit-atlassian-kanban--redraw)
          (shipit-atlassian-kanban--go-to-key chosen))))))

;;; Jumping to either end of a column

(defun shipit-atlassian-kanban--column-top-node (model node)
  "Return the top of NODE\'s column in MODEL, seen from NODE.
The first card, or the header when NODE is that card already or the
column shows no card, so a second jump reaches the header.  From the
header there is nowhere higher, so it stays."
  (let* ((index (shipit-atlassian-kanban--column-index-of model node))
         (header (shipit-atlassian-kanban--header-node model index))
         (first (shipit-atlassian-kanban--card-node model index 0)))
    (cond ((eq node header) header)
          ((or (null first) (eq node first)) header)
          (t first))))

(defun shipit-atlassian-kanban--column-bottom-node (model node)
  "Return the bottom of NODE\'s column in MODEL: its last card, or its
header when it shows none."
  (let ((index (shipit-atlassian-kanban--column-index-of model node)))
    (or (shipit-atlassian-kanban--last-card model index)
        (shipit-atlassian-kanban--header-node model index))))

(defun shipit-atlassian-kanban--jump-in-column (where)
  "Put the keyboard where WHERE says, a function of the model and the node."
  (let* ((node (shipit-atlassian-kanban--selected-node))
         (target (funcall where (canvas-diagram-current-model) node)))
    (unless (eq target node)
      (shipit-atlassian-kanban--go-to target))))

(defun shipit-atlassian-kanban-column-top ()
  "Go to the first card of this column, or to its header from there."
  (interactive)
  (shipit-atlassian-kanban--jump-in-column
   #'shipit-atlassian-kanban--column-top-node))

(defun shipit-atlassian-kanban-column-bottom ()
  "Go to the last card of this column."
  (interactive)
  (shipit-atlassian-kanban--jump-in-column
   #'shipit-atlassian-kanban--column-bottom-node))

;;; Collapsing a column

(defun shipit-atlassian-kanban-toggle-column ()
  "Collapse the column the keyboard is on, or open it again.
Works on a column header and on any card in the column, so there is no
need to go to the header first.  A collapsed column shows only its
header and takes only its width, which is what makes room for the
columns after it."
  (interactive)
  (let* ((model (canvas-diagram-current-model))
         (column (shipit-atlassian-kanban--column-of
                  model (shipit-atlassian-kanban--selected-node)))
         (name (and column (shipit-atlassian-kanban-column-name column))))
    (unless name
      (user-error "No column here"))
    (setq shipit-atlassian-kanban--collapsed
          (if (member name shipit-atlassian-kanban--collapsed)
              (delete name (copy-sequence shipit-atlassian-kanban--collapsed))
            (cons name shipit-atlassian-kanban--collapsed)))
    (shipit-atlassian-kanban--redraw)))

;;; Reading the board again

(defun shipit-atlassian-kanban-refresh ()
  "Fetch this board again and draw it, keeping the staged moves.
On the timeline its history is read again as well.  Beside the board
of then, the board of then is read, which this one follows."
  (interactive)
  (shipit-atlassian-kanban--on-then #'shipit-atlassian-kanban--read-again))

(defun shipit-atlassian-kanban--read-again ()
  "Fetch this board and, on the timeline, its history again; then draw it."
  (let ((buffer (current-buffer)))
    (setq shipit-atlassian-kanban--histories nil
          shipit-atlassian-kanban--left nil)
    (when shipit-atlassian-kanban--timeline
      (shipit-atlassian-kanban--read-history)
      (shipit-atlassian-kanban--read-left))
    (message "Reading the board...")
    (shipit-atlassian-board--fetch-all-async
     shipit-atlassian-kanban--config
     (list shipit-atlassian-kanban--board-id)
     (lambda (boards)
       (when (and (buffer-live-p buffer) (car boards))
         (with-current-buffer buffer
           (setq shipit-atlassian-kanban--board (car boards))
           (shipit-atlassian-kanban--redraw)
           (message "Board read")))))))

(defun shipit-atlassian-kanban--open-issue (key)
  "Open issue KEY of this board\\='s repository in a shipit buffer."
  (shipit-issues-open-buffer key shipit-atlassian-kanban--repo))

(defun shipit-atlassian-kanban-visit-issue ()
  "Open the issue of the card the keyboard is on in a shipit buffer."
  (interactive)
  (shipit-atlassian-kanban--open-issue (shipit-atlassian-kanban--selected-key)))

;;; The timeline
;;
;; M-p shows the board as it was one step earlier, and M-n one step
;; later, until it is back at now.  The board of then is worked out
;; from the history of its issues, which is read from Jira the first
;; time.  Nothing can be moved on it: the moves would be made on the
;; board of now, which is not the one shown.  The header line says so
;; and names the keys, and the rest of the board keys work as before.

(defface shipit-atlassian-kanban-timeline
  '((t :inherit warning :inverse-video t :weight bold))
  "Face of the banner that says the board is shown as it was."
  :group 'shipit-atlassian)

(defconst shipit-atlassian-kanban--timeline-steps
  '((hour . 3600) (day . 86400) (week . 604800))
  "The steps of the timeline, in the order they cycle, and their seconds.")

(defun shipit-atlassian-kanban--step-seconds ()
  "Return how many seconds one step of the timeline goes."
  (or (alist-get shipit-atlassian-kanban--timeline-step
                 shipit-atlassian-kanban--timeline-steps)
      (error "Unknown timeline step: %S" shipit-atlassian-kanban--timeline-step)))

(defun shipit-atlassian-kanban--count (n unit)
  "Return N UNITs, with UNIT made plural unless N is one."
  (format "%d %s%s" n unit (if (= n 1) "" "s")))

(defun shipit-atlassian-kanban--distance (seconds)
  "Return SECONDS as the two largest of weeks, days, hours and minutes."
  (let ((parts nil)
        (left (round seconds)))
    (pcase-dolist (`(,unit . ,size) '(("week" . 604800) ("day" . 86400)
                                      ("hour" . 3600) ("minute" . 60)))
      (when (>= left size)
        (push (shipit-atlassian-kanban--count (/ left size) unit) parts)
        (setq left (% left size))))
    (string-join (seq-take (nreverse parts) 2) " ")))

(defun shipit-atlassian-kanban--then-buffer ()
  "Return the buffer whose timeline this buffer shows.
That is the board of then beside which this buffer shows now, or else
this buffer itself."
  (if (buffer-live-p shipit-atlassian-kanban--compared-with)
      shipit-atlassian-kanban--compared-with
    (current-buffer)))

(defun shipit-atlassian-kanban--on-then (fn)
  "Call FN in the buffer whose timeline this buffer shows, and return its value.
So the timeline keys work on either side of a comparison."
  (with-current-buffer (shipit-atlassian-kanban--then-buffer)
    (funcall fn)))

(defun shipit-atlassian-kanban--read-only-p ()
  "Return non-nil when this buffer shows a board that cannot be changed.
That is the board of then on the timeline, and the board of now shown
beside it."
  (or shipit-atlassian-kanban--timeline
      (buffer-live-p shipit-atlassian-kanban--compared-with)))

(defun shipit-atlassian-kanban--history-ready-p ()
  "Return non-nil when the timeline is on and its history has arrived."
  (and shipit-atlassian-kanban--timeline
       (consp shipit-atlassian-kanban--histories)))

(defun shipit-atlassian-kanban--timeline-time ()
  "Return the time the timeline shows, in seconds since the epoch."
  (- (float-time) shipit-atlassian-kanban--timeline))

(defun shipit-atlassian-kanban--own-timeline-spec ()
  "Return what the drawing shows of this buffer\\='s timeline, or nil at now."
  (when shipit-atlassian-kanban--timeline
    (list :time (shipit-atlassian-kanban--timeline-time)
          :back shipit-atlassian-kanban--timeline
          :step shipit-atlassian-kanban--timeline-step
          :reading (or (not (consp shipit-atlassian-kanban--histories))
                       shipit-atlassian-kanban--left-reading)
          :only-moved shipit-atlassian-kanban--only-moved
          :arrows shipit-atlassian-kanban--arrows)))

(defun shipit-atlassian-kanban--timeline-spec ()
  "Return what the drawing shows of the timeline, or nil at now.
A plist: `:time\\=' is the time shown, `:back\\=' how long before now it
is, `:step\\=' the step, and `:reading\\=' is non-nil while the history
is still being read.  `:only-moved\\=' and `:arrows\\=' are the two
toggles.  Beside the board of then, the board of now shows the same
timeline with `:now\\=' set."
  (if (buffer-live-p shipit-atlassian-kanban--compared-with)
      (append (list :now t)
              (shipit-atlassian-kanban--on-then
               #'shipit-atlassian-kanban--own-timeline-spec))
    (shipit-atlassian-kanban--own-timeline-spec)))

(defun shipit-atlassian-kanban--all-histories ()
  "Return the history of every card: those on the board, and those it had
at the time shown and has lost since."
  (append shipit-atlassian-kanban--histories
          (cdr (assoc shipit-atlassian-kanban--timeline shipit-atlassian-kanban--left))))

(defun shipit-atlassian-kanban--board-then ()
  "Return the board as it was at the time the timeline shows."
  (shipit-atlassian-history--board-at shipit-atlassian-kanban--board
                                      (shipit-atlassian-kanban--all-histories)
                                      (shipit-atlassian-kanban--timeline-time)))

(defun shipit-atlassian-kanban--board-now-marked ()
  "Return the board now, with the moves since the time the timeline shows."
  (if (shipit-atlassian-kanban--history-ready-p)
      (shipit-atlassian-history--mark-moves shipit-atlassian-kanban--board
                                            (shipit-atlassian-kanban--all-histories)
                                            (shipit-atlassian-kanban--timeline-time))
    shipit-atlassian-kanban--board))

(defun shipit-atlassian-kanban--shown-board ()
  "Return the board to draw.
On the timeline that is the board of then, and beside it the board of
now with the moves since then.  Until the history arrives it is the
board of now."
  (cond
   ((buffer-live-p shipit-atlassian-kanban--compared-with)
    (shipit-atlassian-kanban--on-then #'shipit-atlassian-kanban--board-now-marked))
   ((shipit-atlassian-kanban--history-ready-p)
    (shipit-atlassian-kanban--board-then))
   (t shipit-atlassian-kanban--board)))

(defun shipit-atlassian-kanban--shown-filter ()
  "Return the filter to draw with.
That is this buffer\\='s own, and on the timeline with only the moved
cards asked for, that as well."
  (if (plist-get (shipit-atlassian-kanban--timeline-spec) :only-moved)
      (append '(:moved t) shipit-atlassian-kanban--filter)
    shipit-atlassian-kanban--filter))

(defun shipit-atlassian-kanban--timeline-keys (timeline)
  "Return the keys of TIMELINE, a timeline spec, for its banner.
The key that leaves comes first, so it shows in a narrow window too."
  (let ((now (plist-get timeline :now)))
    (substitute-command-keys
     (string-join
      (delq nil
            (list (if now
                      "\\[shipit-atlassian-kanban-quit] closes"
                    "\\[shipit-atlassian-kanban-quit] back to now")
                  "\\[shipit-atlassian-kanban-timeline-earlier] earlier"
                  "\\[shipit-atlassian-kanban-timeline-later] later"
                  (format "\\[shipit-atlassian-kanban-timeline-cycle-step] step: %s"
                          (plist-get timeline :step))
                  "\\[shipit-atlassian-kanban-timeline-changes] changes"
                  (unless now
                    "\\[shipit-atlassian-kanban-timeline-side-by-side] side by side")
                  "\\[shipit-atlassian-kanban-timeline-toggle-unchanged] only moved"
                  "\\[shipit-atlassian-kanban-timeline-toggle-arrows] arrows"))
      " · "))))

(defun shipit-atlassian-kanban--timeline-when (timeline)
  "Return when TIMELINE, a timeline spec, shows the board."
  (if (plist-get timeline :reading)
      "reading the history…"
    (format "%s (%s ago)"
            (format-time-string "%a %Y-%m-%d %H:%M" (plist-get timeline :time))
            (shipit-atlassian-kanban--distance (plist-get timeline :back)))))

(defun shipit-atlassian-kanban--timeline-banner (timeline)
  "Return the header line banner for TIMELINE, a timeline spec."
  (let ((now (plist-get timeline :now)))
    (concat (propertize (if now " NOW " " TIMELINE ")
                        'face 'shipit-atlassian-kanban-timeline)
            " "
            (propertize (concat (if now "beside " "")
                                (shipit-atlassian-kanban--timeline-when timeline))
                        'face 'bold)
            "   "
            (shipit-atlassian-kanban--timeline-keys timeline))))

(defun shipit-atlassian-kanban--refuse-on-the-timeline ()
  "Signal while this buffer shows a board that cannot be changed.
The board of then cannot be changed, and the board of now beside it
is there to compare, not to move cards on."
  (when (shipit-atlassian-kanban--read-only-p)
    (user-error "%s" (substitute-command-keys
                      (if shipit-atlassian-kanban--timeline
                          "The timeline is read-only: \\[shipit-atlassian-kanban-quit] goes back to now"
                        "This side is only to compare: \\[shipit-atlassian-kanban-quit] closes it")))))

(defun shipit-atlassian-kanban--history-read (histories)
  "Keep HISTORIES, the board\\='s history, and draw the board again.
Nothing back means the history could not be read.  The timeline then
ends, rather than show the board of now under an old date."
  (setq shipit-atlassian-kanban--histories histories)
  (unless histories
    (shipit-atlassian-kanban--end-timeline))
  (shipit-atlassian-kanban--redraw)
  (shipit-atlassian-kanban--follow-changes)
  (message (if histories "History read" "Could not read the history of the board")))

(defun shipit-atlassian-kanban--read-history ()
  "Read the history of the board, unless it is read or being read."
  (unless shipit-atlassian-kanban--histories
    (let ((buffer (current-buffer)))
      (setq shipit-atlassian-kanban--histories 'reading)
      (message "Reading the history of the board...")
      (shipit-atlassian-history--fetch-async
       shipit-atlassian-kanban--config shipit-atlassian-kanban--board-id
       (lambda (histories)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (shipit-atlassian-kanban--history-read histories))))))))

(defun shipit-atlassian-kanban--with-board-filter (fn)
  "Call FN with the JQL of the board\\='s filter, read first when not known.
FN gets nil when the board has no filter."
  (if shipit-atlassian-kanban--board-filter
      (funcall fn (and (stringp shipit-atlassian-kanban--board-filter)
                       shipit-atlassian-kanban--board-filter))
    (let ((buffer (current-buffer)))
      (shipit-atlassian-history--board-filter-async
       shipit-atlassian-kanban--config shipit-atlassian-kanban--board-id
       (lambda (filter)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq shipit-atlassian-kanban--board-filter (or filter 'none))
             (funcall fn filter))))))))

(defun shipit-atlassian-kanban--left-read (back left)
  "Keep LEFT, the cards lost since the time BACK seconds ago; draw again.
A step taken meanwhile may be to a time not read yet, so that is read
next."
  (push (cons back left) shipit-atlassian-kanban--left)
  (setq shipit-atlassian-kanban--left-reading nil)
  (shipit-atlassian-kanban--redraw)
  (shipit-atlassian-kanban--follow-changes)
  (shipit-atlassian-kanban--read-left))

(defun shipit-atlassian-kanban--left-to-read-p ()
  "Return non-nil when the cards lost since the time shown are not read yet."
  (and shipit-atlassian-kanban--timeline
       (not shipit-atlassian-kanban--left-reading)
       (not (eq shipit-atlassian-kanban--board-filter 'none))
       (not (assoc shipit-atlassian-kanban--timeline shipit-atlassian-kanban--left))))

(defun shipit-atlassian-kanban--board-keys ()
  "Return the keys of the issues the board has now."
  (mapcar #'shipit-atlassian-board--issue-key
          (shipit-atlassian-history--board-issues shipit-atlassian-kanban--board)))

(defun shipit-atlassian-kanban--read-left ()
  "Read the cards the board had at the time shown and has lost since.
Once for each time: the search is the board\\='s filter as it matched
then, less the cards the board has now."
  (when (shipit-atlassian-kanban--left-to-read-p)
    (let ((buffer (current-buffer))
          (back shipit-atlassian-kanban--timeline))
      (setq shipit-atlassian-kanban--left-reading back)
      (shipit-atlassian-kanban--with-board-filter
       (lambda (filter)
         (if (null filter)
             (setq shipit-atlassian-kanban--left-reading nil)
           (shipit-atlassian-history--fetch-left-async
            shipit-atlassian-kanban--config filter (- (float-time) back)
            (shipit-atlassian-kanban--board-keys)
            (lambda (left)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (shipit-atlassian-kanban--left-read back left)))))))))))

(defun shipit-atlassian-kanban--end-timeline ()
  "End the timeline, and close the board of now and the moves beside it."
  (setq shipit-atlassian-kanban--timeline nil)
  (shipit-atlassian-kanban--close-companions))

(defun shipit-atlassian-kanban--show-back (seconds)
  "Show the board as it was SECONDS ago, or as it is now when nil."
  (if seconds
      (progn (setq shipit-atlassian-kanban--timeline seconds)
             (shipit-atlassian-kanban--read-history)
             (shipit-atlassian-kanban--read-left))
    (shipit-atlassian-kanban--end-timeline))
  (shipit-atlassian-kanban--redraw)
  (shipit-atlassian-kanban--follow-changes))

(defun shipit-atlassian-kanban-timeline-earlier ()
  "Show the board as it was one step earlier.
At now this starts the timeline.  The board is read-only there, and
\\[shipit-atlassian-kanban-quit] goes back to now."
  (interactive)
  (shipit-atlassian-kanban--on-then
   (lambda ()
     (shipit-atlassian-kanban--show-back
      (+ (or shipit-atlassian-kanban--timeline 0)
         (shipit-atlassian-kanban--step-seconds))))))

(defun shipit-atlassian-kanban--step-later ()
  "Show the board one step later; a step past now ends the timeline."
  (unless shipit-atlassian-kanban--timeline
    (user-error "%s" (substitute-command-keys
                      "The board is at now: \\[shipit-atlassian-kanban-timeline-earlier] goes back in time")))
  (let ((back (- shipit-atlassian-kanban--timeline
                 (shipit-atlassian-kanban--step-seconds))))
    (shipit-atlassian-kanban--show-back (and (> back 0) back))
    (unless shipit-atlassian-kanban--timeline
      (message "Back to now"))))

(defun shipit-atlassian-kanban-timeline-later ()
  "Show the board as it was one step later; a step past now ends the timeline."
  (interactive)
  (shipit-atlassian-kanban--on-then #'shipit-atlassian-kanban--step-later))

(defun shipit-atlassian-kanban--next-step ()
  "Make one step of the timeline the next of an hour, a day and a week."
  (let ((steps (mapcar #'car shipit-atlassian-kanban--timeline-steps)))
    (setq shipit-atlassian-kanban--timeline-step
          (or (cadr (memq shipit-atlassian-kanban--timeline-step steps))
              (car steps))))
  (when shipit-atlassian-kanban--timeline
    (shipit-atlassian-kanban--redraw))
  (message "One step of the timeline is now a %s"
           shipit-atlassian-kanban--timeline-step))

(defun shipit-atlassian-kanban-timeline-cycle-step ()
  "Make one step of the timeline the next of an hour, a day and a week."
  (interactive)
  (shipit-atlassian-kanban--on-then #'shipit-atlassian-kanban--next-step))

(defun shipit-atlassian-kanban-quit ()
  "Leave the timeline and show the board as it is now.
Beside the board of then, close the board of now.  At now, quit the
window as usual."
  (interactive)
  (cond
   ((buffer-live-p shipit-atlassian-kanban--compared-with)
    (shipit-atlassian-kanban--on-then #'shipit-atlassian-kanban--close-beside))
   (shipit-atlassian-kanban--timeline
    (shipit-atlassian-kanban--show-back nil)
    (message "Back to now"))
   (t (quit-window))))

(defun shipit-atlassian-kanban--toggle (variable what)
  "Flip VARIABLE, a toggle of the timeline, and draw again; WHAT names it."
  (unless shipit-atlassian-kanban--timeline
    (user-error "%s" (substitute-command-keys
                      "Go back in time first with \\[shipit-atlassian-kanban-timeline-earlier]: at now nothing moved")))
  (set variable (not (symbol-value variable)))
  (shipit-atlassian-kanban--redraw)
  (message "%s: %s" what (if (symbol-value variable) "on" "off")))

(defun shipit-atlassian-kanban-timeline-toggle-unchanged ()
  "Hide the cards that did not move since the time the timeline shows.
Again, show them again.  The column headers count the hidden cards."
  (interactive)
  (shipit-atlassian-kanban--on-then
   (lambda ()
     (shipit-atlassian-kanban--toggle 'shipit-atlassian-kanban--only-moved
                                      "Only the moved cards"))))

(defun shipit-atlassian-kanban-timeline-toggle-arrows ()
  "Turn off, or on again, the arrows to where each moved card went.
A moved card gets a dashed ghost in the column it sits in on the other
side of the timeline, and an arrow joins the two."
  (interactive)
  (shipit-atlassian-kanban--on-then
   (lambda ()
     (shipit-atlassian-kanban--toggle 'shipit-atlassian-kanban--arrows
                                      "Arrows to where the cards went"))))

;;; Then and now side by side
;;
;; The board of now opens in a second buffer beside the board of then.
;; It reads the board, the history and the time from the buffer of
;; then, so it follows every step, and the cards that moved in between
;; take the timeline colour on both sides.  The keyboard goes to the
;; same card on the other side, wherever that card sits there, and a
;; zoom on one side zooms the other.

(defvar shipit-atlassian-kanban--following nil
  "Non-nil while one side of a comparison moves or zooms the other.
It stops the other side from moving or zooming this one back in turn.")

(defun shipit-atlassian-kanban--other-side ()
  "Return the buffer on the other side of a comparison, or nil."
  (seq-find #'buffer-live-p (list shipit-atlassian-kanban--beside
                                  shipit-atlassian-kanban--compared-with)))

(defun shipit-atlassian-kanban--in-board (buffer fn)
  "Call FN in the board BUFFER, in the window showing it when there is one.
So it is that view that scrolls or zooms.  Return what FN returns."
  (let ((window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (if window
          (with-selected-window window (funcall fn))
        (funcall fn)))))

(defun shipit-atlassian-kanban--go-to-in (buffer key)
  "Put the keyboard of BUFFER on the node KEY names there, if it is drawn.
Return non-nil when the node is drawn."
  (shipit-atlassian-kanban--in-board
   buffer
   (lambda ()
     (when-let* ((node (shipit-atlassian-kanban--restore
                        shipit-atlassian-kanban--drawing key)))
       (canvas-diagram-go node)
       t))))

(defun shipit-atlassian-kanban--zoom-in-board (buffer zoom)
  "Draw the board BUFFER at ZOOM, without it zooming the other side back."
  (let ((shipit-atlassian-kanban--following t))
    (shipit-atlassian-kanban--in-board
     buffer (lambda () (canvas-diagram-zoom-to zoom)))))

(defun shipit-atlassian-kanban--zoom-along (_diagram zoom)
  "Draw the other side of a comparison at ZOOM as well.
The `:zoom\\=' callback of canvas-diagram, which runs on every zoom."
  (when-let* ((other (and (not shipit-atlassian-kanban--following)
                          (shipit-atlassian-kanban--other-side))))
    (shipit-atlassian-kanban--zoom-in-board other zoom)))

(defun shipit-atlassian-kanban--follow (diagram node)
  "Put the keyboard of the other side on the node NODE of DIAGRAM draws there.
The `:go\\=' callback of canvas-diagram, which runs on every move."
  (when-let* ((other (and (not shipit-atlassian-kanban--following)
                          (shipit-atlassian-kanban--other-side))))
    (let ((shipit-atlassian-kanban--following t))
      (shipit-atlassian-kanban--go-to-in
       other (shipit-atlassian-kanban--node-key diagram node)))))

(defun shipit-atlassian-kanban--close-buffer (buffer)
  "Close BUFFER, if it is live, and the windows opened for it."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (quit-restore-window window 'kill))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun shipit-atlassian-kanban--close-beside ()
  "Close the board of now shown beside this board, if there is one."
  (let ((now shipit-atlassian-kanban--beside))
    (setq shipit-atlassian-kanban--beside nil)
    (shipit-atlassian-kanban--close-buffer now)))

(defun shipit-atlassian-kanban--close-companions ()
  "Close what shows this board\\='s timeline beside it: now, and the moves."
  (shipit-atlassian-kanban--close-beside)
  (let ((changes shipit-atlassian-kanban--changes))
    (setq shipit-atlassian-kanban--changes nil)
    (shipit-atlassian-kanban--close-buffer changes)))

(defun shipit-atlassian-kanban--open-beside ()
  "Open the board of now in a window beside this one, and return its buffer.
The keyboard stays on the board of then.  Drawing the new board makes
it the current buffer, so what belongs to the board of then is taken
first or done in it by name."
  (let* ((then (current-buffer))
         (window (selected-window))
         (collapsed shipit-atlassian-kanban--collapsed)
         (filter shipit-atlassian-kanban--filter)
         (me shipit-atlassian-kanban--me)
         (zoom (canvas-diagram-current-zoom))
         (now (let ((display-buffer-overriding-action
                     '(display-buffer-in-direction (direction . right))))
                (shipit-atlassian-kanban--draw-in
                 (shipit-atlassian-kanban--buffer-name
                  shipit-atlassian-kanban--repo shipit-atlassian-kanban--board "now")
                 shipit-atlassian-kanban--board shipit-atlassian-kanban--repo
                 shipit-atlassian-kanban--config shipit-atlassian-kanban--board-id
                 shipit-atlassian-kanban--tracker))))
    (with-current-buffer now
      (setq shipit-atlassian-kanban--compared-with then
            shipit-atlassian-kanban--collapsed collapsed
            shipit-atlassian-kanban--filter filter
            shipit-atlassian-kanban--me me)
      (shipit-atlassian-kanban--redraw))
    (shipit-atlassian-kanban--zoom-in-board now zoom)
    (with-current-buffer then
      (setq shipit-atlassian-kanban--beside now)
      (add-hook 'kill-buffer-hook #'shipit-atlassian-kanban--close-companions nil t))
    (select-window window)
    now))

(defun shipit-atlassian-kanban--side-by-side ()
  "Show the board of now beside this board of then, and return its buffer."
  (unless shipit-atlassian-kanban--timeline
    (user-error "%s" (substitute-command-keys
                      "Go back in time first with \\[shipit-atlassian-kanban-timeline-earlier]: now goes beside the board of then")))
  (let ((now (if (buffer-live-p shipit-atlassian-kanban--beside)
                 shipit-atlassian-kanban--beside
               (shipit-atlassian-kanban--open-beside))))
    (display-buffer now)
    now))

(defun shipit-atlassian-kanban-timeline-side-by-side ()
  "Show the board as it is now beside the board of then.
Cards that moved in between take the timeline colour on both sides,
and the keyboard goes to the same card on the other side.  The timeline
keys work on either side, and \\[shipit-atlassian-kanban-quit] on the side of now closes it."
  (interactive)
  (shipit-atlassian-kanban--on-then #'shipit-atlassian-kanban--side-by-side))

;;; The changes since the time the timeline shows

(defvar-local shipit-atlassian-kanban--changes-board nil
  "The board buffer whose changes this buffer lists.")

(defun shipit-atlassian-kanban--changes-buffer-name ()
  "Return the name of the buffer listing the changes of this board."
  (format "*shipit-kanban-changes: %s %s*" shipit-atlassian-kanban--repo
          (or (cdr (assq 'name shipit-atlassian-kanban--board)) "board")))

(defun shipit-atlassian-kanban--styled (text face)
  "Return TEXT in FACE, whether font lock runs in its buffer or not."
  (propertize text 'face face 'font-lock-face face))

(defun shipit-atlassian-kanban--change-line (move)
  "Return the line listing MOVE, with the summary of the card it moved.
The time, the key and the person look as they do elsewhere in shipit."
  (let ((key (plist-get move :key)))
    (concat (shipit-atlassian-kanban--styled
              (shipit-atlassian-kanban--move-time move) 'shipit-timestamp-face)
             "  "
             (shipit-atlassian-kanban--styled (format "%-12s" key) 'magit-hash)
             " "
             (shipit-atlassian-kanban--styled
              (format "%-20s" (shipit-atlassian-kanban--move-author move))
              'shipit-username-face)
             " "
             (shipit-atlassian-kanban--styled (plist-get move :from) 'shadow)
             " → "
             (plist-get move :to)
             "  "
             (shipit-atlassian-kanban--summary
              (shipit-atlassian-board--issue-in-boards
               (list shipit-atlassian-kanban--board) key))
             "\n")))

(put 'shipit-kanban-changes 'magit-section t)
(put 'shipit-kanban-change 'magit-section t)

(defvar-local shipit-atlassian-kanban--issues-read nil
  "The issues this list of moves read, a hash of key to issue.
So a move that stays open while the timeline steps is not read again.")

(defun shipit-atlassian-kanban--read-issue (repo key)
  "Return issue KEY of REPO, read once for this list of moves."
  (unless shipit-atlassian-kanban--issues-read
    (setq shipit-atlassian-kanban--issues-read (make-hash-table :test #'equal)))
  (or (gethash key shipit-atlassian-kanban--issues-read)
      (progn
        (message "Reading %s..." key)
        (prog1 (puthash key (shipit-issues--fetch-issue repo key)
                        shipit-atlassian-kanban--issues-read)
          (message nil)))))

(defun shipit-atlassian-kanban--insert-issue-details (repo key)
  "Insert the description and the activity of issue KEY of REPO.
Read from the issue backend when the move is first expanded, and drawn
by the issue buffer\\='s own sections, loaded then.

The details start with a blank line.  Shutting the move hides them from
their first character on, and Emacs still draws an overlay string and a
display property found right at the start of hidden text.  A section
starting there put its magit mark and its icon on the next move\\='s line."
  (require 'shipit-issues)
  (require 'shipit-issues-buffer)
  (shipit-issue--use-backend-of repo)
  (insert "\n")
  (let ((issue (shipit-atlassian-kanban--read-issue repo key)))
    (if (null issue)
        (insert (propertize (format "   Could not read %s\n" key)
                            'font-lock-face 'error))
      (shipit-issue--insert-description-section repo issue key)
      (shipit-issue--insert-activity-section repo issue))))

(defun shipit-atlassian-kanban--insert-change (move line repo open)
  "Insert MOVE as a section headed by LINE, its issue of REPO inside.
The section starts shut unless OPEN, and nothing is read until it is
opened.  The section\\='s value is MOVE, which tells one move of an issue
from another."
  (magit-insert-section (shipit-kanban-change move (not open))
    (magit-insert-heading line)
    (magit-insert-section-body
      (shipit-atlassian-kanban--insert-issue-details repo (plist-get move :key)))))

(defun shipit-atlassian-kanban--open-moves ()
  "Return the moves this list shows open."
  (and magit-root-section
       (mapcar (lambda (section) (oref section value))
               (seq-remove (lambda (section) (oref section hidden))
                           (oref magit-root-section children)))))

(defun shipit-atlassian-kanban--go-to-move (move)
  "Put point on the line of MOVE, or on the first move when it is gone."
  (goto-char (point-min))
  (forward-line 2)
  (when-let* ((section (and move
                            (seq-find (lambda (section) (equal move (oref section value)))
                                      (oref magit-root-section children)))))
    (goto-char (oref section start))))

(defun shipit-atlassian-kanban--insert-changes (board moves since)
  "Insert MOVES, made on BOARD since the time SINCE, in the changes buffer.
A move open before stays open, and point stays on its move."
  (let ((inhibit-read-only t)
        (repo (buffer-local-value 'shipit-atlassian-kanban--repo board))
        (open (shipit-atlassian-kanban--open-moves))
        (here (when-let* ((section (shipit-atlassian-kanban--move-section-at-point)))
                (oref section value)))
        (lines (with-current-buffer board
                 (mapcar #'shipit-atlassian-kanban--change-line moves))))
    (erase-buffer)
    (magit-insert-section (shipit-kanban-changes)
      (insert (shipit-atlassian-kanban--styled
               (format "%d move%s since %s" (length moves)
                       (if (= 1 (length moves)) "" "s")
                       (format-time-string "%a %Y-%m-%d %H:%M" since))
               'magit-section-heading)
              "\n\n")
      (cl-mapc (lambda (move line)
                 (shipit-atlassian-kanban--insert-change
                  move line repo (member move open)))
               moves lines))
    ;; The root is not shown by inserting it, and showing it is what
    ;; gives every move magit's mark, shut or open.
    (magit-section-show magit-root-section)
    (shipit-atlassian-kanban--go-to-move here)))

(defun shipit-atlassian-kanban--fit-changes (window)
  "Make WINDOW as tall as the list of changes in it, at most a third of its frame."
  (fit-window-to-buffer
   window (max window-min-height (/ (frame-height (window-frame window)) 3))))

(defun shipit-atlassian-kanban--show-changes (buffer)
  "Show BUFFER, a list of changes, at the bottom across the frame, and select it.
Across the frame, since the board of then and the board of now may
share the width above it."
  (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-at-bottom)
                          (window-height . shipit-atlassian-kanban--fit-changes))))

(defun shipit-atlassian-kanban-timeline-changes ()
  "List the moves made on the board since the time the timeline shows.
Newest first, each with who made it.
\\<shipit-atlassian-kanban-changes-mode-map>\\[magit-section-toggle] on one opens it on its issue, \\[shipit-atlassian-kanban-changes-visit] opens the issue in shipit, and
\\[shipit-atlassian-kanban-changes-show-card] shows its card on the board."
  (interactive)
  (shipit-atlassian-kanban--on-then #'shipit-atlassian-kanban--list-changes))

(defun shipit-atlassian-kanban--list-changes ()
  "List the moves since the time the timeline shows; return the list\\='s buffer."
  (unless shipit-atlassian-kanban--timeline
    (user-error "%s" (substitute-command-keys
                      "Go back in time first with \\[shipit-atlassian-kanban-timeline-earlier]: the changes are those since then")))
  (unless (consp shipit-atlassian-kanban--histories)
    (user-error "The history of the board is still being read"))
  (let ((board (current-buffer))
        (list (get-buffer-create (shipit-atlassian-kanban--changes-buffer-name))))
    (setq shipit-atlassian-kanban--changes list)
    (add-hook 'kill-buffer-hook #'shipit-atlassian-kanban--close-companions nil t)
    (with-current-buffer list
      (unless (derived-mode-p 'shipit-atlassian-kanban-changes-mode)
        (shipit-atlassian-kanban-changes-mode))
      (setq shipit-atlassian-kanban--changes-board board))
    (shipit-atlassian-kanban--fill-changes)
    (shipit-atlassian-kanban--show-changes list)
    list))

(defun shipit-atlassian-kanban--fill-changes ()
  "Fill this board\\='s list with the moves since the time the timeline shows."
  (let ((board (current-buffer))
        (since (shipit-atlassian-kanban--timeline-time)))
    (let ((moves (shipit-atlassian-history--moves-between
                  (shipit-atlassian-kanban--all-histories) since (float-time))))
      (with-current-buffer shipit-atlassian-kanban--changes
        (shipit-atlassian-kanban--insert-changes board moves since)))))

(defun shipit-atlassian-kanban--follow-changes ()
  "Fill this board\\='s list of moves again, when it is open and can be.
It follows the timeline as it steps, and the history as it is read
again.  The list closes when the timeline ends, elsewhere."
  (when (and (buffer-live-p shipit-atlassian-kanban--changes)
             (shipit-atlassian-kanban--history-ready-p))
    (let* ((list shipit-atlassian-kanban--changes)
           (window (get-buffer-window list)))
      ;; The list is seldom the selected window while the board steps,
      ;; and its window keeps a point of its own.
      (when window
        (with-current-buffer list (goto-char (window-point window))))
      (shipit-atlassian-kanban--fill-changes)
      (when window
        (set-window-point window (with-current-buffer list (point)))
        (shipit-atlassian-kanban--fit-changes window)))))

(defun shipit-atlassian-kanban--move-section-at-point ()
  "Return the section of the move point is in, its line or its details, or nil."
  (cl-loop for section = (magit-current-section) then (oref section parent)
           while section
           when (eq (oref section type) 'shipit-kanban-change)
           return section))

(defun shipit-atlassian-kanban--change-key ()
  "Return the key of the card whose move point is in, its line or its details."
  (plist-get (oref (or (shipit-atlassian-kanban--move-section-at-point)
                       (user-error "No move here"))
                   value)
             :key))

(defun shipit-atlassian-kanban--changes-board-buffer ()
  "Return the board this list of changes belongs to, while it is open."
  (if (buffer-live-p shipit-atlassian-kanban--changes-board)
      shipit-atlassian-kanban--changes-board
    (user-error "The board of these changes is closed")))

(defun shipit-atlassian-kanban--show-move (board key)
  "Put the keyboard of BOARD, and of the board of now beside it, on card KEY.
Each board goes on its own: a card made since the time shown is only on
the board of now, and one gone from the board since only on the board
of then.  Return the boards that show the card."
  (let ((shipit-atlassian-kanban--following t))
    (seq-filter (lambda (buffer) (shipit-atlassian-kanban--go-to-in buffer key))
                (seq-filter #'buffer-live-p
                            (list board (buffer-local-value
                                         'shipit-atlassian-kanban--beside board))))))

(defun shipit-atlassian-kanban--why-not-shown (key)
  "Return why no board beside the list of changes shows the card KEY.
A card made since the time shown is on the board of then too, and one
in a shut column is reached through the column\\='s header, so a card
missing from both is filtered out, or in a status no column takes."
  (format "%s is not on the board shown: filtered out, or in a status no column takes"
          key))

(defvar-local shipit-atlassian-kanban--move-followed nil
  "The move whose card the board went to last, so moving within it does nothing.")

(defun shipit-atlassian-kanban--follow-point ()
  "Put the keyboard of the board on the card of the move point is in.
Run after every command in the list, so the board follows point, and
the board of now beside it follows the board.  A card the board does
not show is left alone.  An error is shown rather than raised, since a
raised one would take this off the hook for good."
  (with-demoted-errors "Following the list on the board: %S"
    (let ((move (when-let* ((section (shipit-atlassian-kanban--move-section-at-point)))
                  (oref section value))))
      (when (and move
                 (not (equal move shipit-atlassian-kanban--move-followed))
                 (buffer-live-p shipit-atlassian-kanban--changes-board))
        (setq shipit-atlassian-kanban--move-followed move)
        (let ((board shipit-atlassian-kanban--changes-board)
              (key (plist-get move :key)))
          (unless (shipit-atlassian-kanban--show-move board key)
            (message "%s" (shipit-atlassian-kanban--why-not-shown key))))))))

(defun shipit-atlassian-kanban-changes-show-card ()
  "Put the keyboard of the boards on the card moved on this line.
The list keeps the focus, so the next change can be shown in turn."
  (interactive)
  (let ((key (shipit-atlassian-kanban--change-key))
        (board (shipit-atlassian-kanban--changes-board-buffer)))
    (unless (shipit-atlassian-kanban--show-move board key)
      (user-error "%s" (shipit-atlassian-kanban--why-not-shown key)))))

(defun shipit-atlassian-kanban-changes-visit ()
  "Open in shipit the issue moved on this line.
It opens in the window of the board rather than in the short list."
  (interactive)
  (let ((key (shipit-atlassian-kanban--change-key))
        (board (shipit-atlassian-kanban--changes-board-buffer)))
    (when-let* ((window (get-buffer-window board)))
      (select-window window))
    (with-current-buffer board
      (shipit-atlassian-kanban--open-issue key))))

(defun shipit-atlassian-kanban--change-url ()
  "Return the web address of the issue whose move point is in."
  (shipit-issue--url-for
   (buffer-local-value 'shipit-atlassian-kanban--repo
                       (shipit-atlassian-kanban--changes-board-buffer))
   (shipit-atlassian-kanban--change-key)))

(defun shipit-atlassian-kanban--copy (text)
  "Put TEXT on the kill ring, and say so."
  (kill-new text)
  (message "Copied: %s" text))

(defun shipit-atlassian-kanban-changes-browse ()
  "Open in the browser the issue moved on this line."
  (interactive)
  (browse-url (shipit-atlassian-kanban--change-url)))

(defun shipit-atlassian-kanban-changes-copy-url ()
  "Copy the web address of the issue moved on this line."
  (interactive)
  (shipit-atlassian-kanban--copy (shipit-atlassian-kanban--change-url)))

(defun shipit-atlassian-kanban-changes-copy-key ()
  "Copy the key of the issue moved on this line."
  (interactive)
  (shipit-atlassian-kanban--copy (shipit-atlassian-kanban--change-key)))

;;; Embark on the line of a move

(defun shipit-atlassian-kanban-changes-embark-target ()
  "Embark target finder: the issue of the move whose line point is on.
The kind of the target is `shipit-kanban-change' and its value is the
key of the issue.  The details of an opened move are no part of it, so
a link there keeps its own target."
  (when-let* (((derived-mode-p 'shipit-atlassian-kanban-changes-mode))
              (section (magit-current-section))
              ((eq (oref section type) 'shipit-kanban-change))
              ((< (point) (oref section content))))
    `(shipit-kanban-change ,(plist-get (oref section value) :key)
                           ,(line-beginning-position) . ,(line-end-position))))

(defvar shipit-atlassian-kanban-changes-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-atlassian-kanban-changes-visit)
    (define-key map (kbd "o") #'shipit-atlassian-kanban-changes-visit)
    (define-key map (kbd "b") #'shipit-atlassian-kanban-changes-browse)
    (define-key map (kbd "u") #'shipit-atlassian-kanban-changes-copy-url)
    (define-key map (kbd "w") #'shipit-atlassian-kanban-changes-copy-key)
    map)
  "Embark actions on the line of a move in the list of changes.
The letters are those of the actions on an issue key in code.")

(defvar embark-target-finders)
(defvar embark-keymap-alist)

(defun shipit-atlassian-kanban--embark-register ()
  "Make embark offer `shipit-atlassian-kanban-changes-embark-map' on a move."
  (add-to-list 'embark-target-finders #'shipit-atlassian-kanban-changes-embark-target)
  (add-to-list 'embark-keymap-alist
               '(shipit-kanban-change . shipit-atlassian-kanban-changes-embark-map)))

(with-eval-after-load 'embark
  (shipit-atlassian-kanban--embark-register))

(defvar shipit-atlassian-kanban-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-atlassian-kanban-changes-visit)
    (define-key map (kbd "SPC") #'shipit-atlassian-kanban-changes-show-card)
    map)
  "Keys of the list of changes to a board.")

(define-derived-mode shipit-atlassian-kanban-changes-mode magit-section-mode "Kanban changes"
  "Major mode listing the moves made on a board since a time.
Each move is a section: \\[magit-section-toggle] opens it on the description and the
activity of its issue.  \\[shipit-atlassian-kanban-changes-visit] opens the issue in shipit, and
\\[shipit-atlassian-kanban-changes-show-card] shows its card on the board.
The board follows point as well: moving onto a move puts the keyboard
of the board on its card, and of the board of now beside it.
With embark, the line of a move is a target, with the actions of
`shipit-atlassian-kanban-changes-embark-map'.

\\{shipit-atlassian-kanban-changes-mode-map}"
  (add-hook 'post-command-hook #'shipit-atlassian-kanban--follow-point nil t))

;;; The mode
;;
;; The keymap gets its parent in the mode body rather than at load
;; time: `canvas-diagram-mode-map' does not exist without
;; canvas-diagram, and this file has to load anyway.

(defvar shipit-atlassian-kanban-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<right>") #'shipit-atlassian-kanban-stage-forward)
    (define-key map (kbd "M-<left>") #'shipit-atlassian-kanban-stage-backward)
    (define-key map (kbd "C-c C-c") #'shipit-atlassian-kanban-commit)
    (define-key map (kbd "C-c C-k") #'shipit-atlassian-kanban-discard)
    (define-key map (kbd "M-p") #'shipit-atlassian-kanban-timeline-earlier)
    (define-key map (kbd "M-n") #'shipit-atlassian-kanban-timeline-later)
    (define-key map (kbd "T") #'shipit-atlassian-kanban-timeline-cycle-step)
    (define-key map (kbd "d") #'shipit-atlassian-kanban-timeline-changes)
    (define-key map (kbd "D") #'shipit-atlassian-kanban-timeline-side-by-side)
    (define-key map (kbd "H") #'shipit-atlassian-kanban-timeline-toggle-unchanged)
    (define-key map (kbd "A") #'shipit-atlassian-kanban-timeline-toggle-arrows)
    (define-key map (kbd "q") #'shipit-atlassian-kanban-quit)
    (define-key map (kbd "g") #'shipit-atlassian-kanban-refresh)
    (define-key map (kbd "C-<return>") #'shipit-atlassian-kanban-visit-issue)
    (define-key map (kbd "TAB") #'shipit-atlassian-kanban-toggle-column)
    (define-key map (kbd "<tab>") #'shipit-atlassian-kanban-toggle-column)
    ;; The literal keys, not a remap of `beginning-of-buffer\': that
    ;; command also runs on M-< and M->, which keep going to the ends of
    ;; the whole board through canvas-diagram\'s own remap.
    (define-key map (kbd "C-<home>") #'shipit-atlassian-kanban-column-top)
    (define-key map (kbd "C-<end>") #'shipit-atlassian-kanban-column-bottom)
    (define-key map [remap isearch-forward] #'shipit-atlassian-kanban-find-card)
    (define-key map [remap isearch-backward] #'shipit-atlassian-kanban-find-card)
    (define-key map [remap consult-line] #'shipit-atlassian-kanban-search)
    (define-key map [remap consult-focus-lines] #'shipit-atlassian-kanban-focus)
    (define-key map [remap consult-keep-lines] #'shipit-atlassian-kanban-focus)
    map)
  "Keys a board has over those of every canvas diagram.")

(with-eval-after-load 'meow
  (shipit-canvas-keep-other-packages-away 'shipit-atlassian-kanban-mode))

(with-eval-after-load 'undo-tree
  (shipit-canvas-keep-other-packages-away 'shipit-atlassian-kanban-mode))

(define-derived-mode shipit-atlassian-kanban-mode canvas-diagram-mode "Kanban"
  "Major mode of a buffer drawing a Jira board as cards on a canvas.

The keyboard is on one card, ringed.  The keys that move point move
it: left and right walk the columns, up and down the cards of one
column.  \\[canvas-diagram-toggle-card] opens the card,
\\[shipit-atlassian-kanban-visit-issue] opens the issue in shipit,
and \\[canvas-diagram-copy-node] copies the card.

\\[shipit-atlassian-kanban-stage-forward] and
\\[shipit-atlassian-kanban-stage-backward] stage a move to the next
and the previous column.  Nothing reaches Jira until
\\[shipit-atlassian-kanban-commit]; \\[shipit-atlassian-kanban-discard]
drops every staged move, and the mode line counts them.
\\[shipit-atlassian-kanban-column-top] and \\[shipit-atlassian-kanban-column-bottom] go to the top and the
bottom of the column; a second jump to the top reaches the header.
\\[shipit-atlassian-kanban-toggle-column] collapses the column the
keyboard is on and opens it again, from the header or from any card in
it.  \\[shipit-atlassian-kanban-refresh] reads the board again.

\\[shipit-atlassian-kanban-timeline-earlier] shows the board as it was one step earlier, and
\\[shipit-atlassian-kanban-timeline-later] one step later, from the history of its cards.  The
board is read-only there, and \\[shipit-atlassian-kanban-quit] goes back to now.
\\[shipit-atlassian-kanban-timeline-cycle-step] makes a step an hour, a day or a week, and
\\[shipit-atlassian-kanban-timeline-changes] lists the moves since the time shown, and
\\[shipit-atlassian-kanban-timeline-side-by-side] shows the board now beside it.
\\[shipit-atlassian-kanban-timeline-toggle-unchanged] hides the cards that did not move, and
\\[shipit-atlassian-kanban-timeline-toggle-arrows] turns off the arrows to where each moved card went.

\\{shipit-atlassian-kanban-mode-map}"
  (set-keymap-parent shipit-atlassian-kanban-mode-map canvas-diagram-mode-map)
  (shipit-canvas-keep-other-packages-away 'shipit-atlassian-kanban-mode))

;;; Showing a board

(defun shipit-atlassian-kanban--whoami (config)
  "Return the current user\='s Jira account id, or nil.
Cached per instance for the session by the backend, so opening a
second board costs nothing."
  (condition-case err
      (shipit-issue-jira--fetch-myself config)
    (error
     (shipit--debug-log "kanban: could not tell who you are: %S" err)
     nil)))

(defun shipit-atlassian-kanban--draw-in (name board repo config board-id tracker)
  "Draw BOARD of REPO on a canvas in the buffer NAME and return the buffer.
CONFIG is the Jira backend config plist and BOARD-ID the board's
identifier, both kept for reading the board again.  TRACKER is the
plist of the issue backend of REPO."
  (shipit-atlassian-kanban--require-canvas)
  (let* ((drawing (shipit-atlassian-kanban--diagram))
         (buffer (canvas-diagram-show name #'shipit-atlassian-kanban-mode drawing
                                      (shipit-atlassian-kanban--spec
                                       board nil nil nil nil nil tracker))))
    (with-current-buffer buffer
      (setq shipit-atlassian-kanban--repo repo
            shipit-atlassian-kanban--config config
            shipit-atlassian-kanban--board board
            shipit-atlassian-kanban--board-id board-id
            shipit-atlassian-kanban--tracker tracker
            shipit-atlassian-kanban--drawing drawing))
    buffer))

(defun shipit-atlassian-kanban--tracker-of (repo)
  "Return the plist of the issue backend of REPO."
  (car (shipit-issue--resolve-for-repo repo)))

(defun shipit-atlassian-kanban--show (board repo config board-id)
  "Draw BOARD of REPO on a canvas and return the buffer.
CONFIG is the Jira backend config plist and BOARD-ID the board\='s
identifier, both kept for reading the board again."
  (let ((buffer (shipit-atlassian-kanban--draw-in
                 (shipit-atlassian-kanban--buffer-name repo board)
                 board repo config board-id
                 (shipit-atlassian-kanban--tracker-of repo))))
    (with-current-buffer buffer
      (setq shipit-atlassian-kanban--me (shipit-atlassian-kanban--whoami config))
      (when shipit-atlassian-kanban--me
        (shipit-atlassian-kanban--redraw))
      (shipit-atlassian-kanban--update-mode-line))
    buffer))

;;; Entry point

(defun shipit-atlassian-kanban--read-board (names)
  "Ask which board to open; NAMES is an alist of identifier to name."
  (let* ((candidates (mapcar (lambda (entry)
                               (cons (format "%s (%d)" (cdr entry) (car entry))
                                     (car entry)))
                             names))
         (choice (completing-read "Board: " candidates nil t)))
    (cdr (assoc choice candidates))))

(defun shipit-atlassian-kanban--pick-board (config callback)
  "Call CALLBACK with the board identifier to open.
One configured board is opened without asking.  With several, their
names are fetched first so the prompt can name them."
  (if (null (cdr shipit-atlassian-board-ids))
      (funcall callback (car shipit-atlassian-board-ids))
    (shipit-atlassian-board--fetch-names-async
     config shipit-atlassian-board-ids
     (lambda (names)
       (funcall callback (shipit-atlassian-kanban--read-board names))))))

(defun shipit-atlassian-kanban--open (repo config board-id)
  "Fetch BOARD-ID of REPO through CONFIG and draw it."
  (message "Reading board %d..." board-id)
  (shipit-atlassian-board--fetch-all-async
   config (list board-id)
   (lambda (boards)
     (if (car boards)
         (shipit-atlassian-kanban--show (car boards) repo config board-id)
       (message "Board %d has nothing to draw" board-id)))))

;;;###autoload
(defun shipit-atlassian-kanban (&optional board-id)
  "Open a Jira board as cards on a canvas.
With BOARD-ID, open that board.  Otherwise open the one configured
board, or ask which of several to open.

Needs Emacs 32 and canvas-diagram.  Without them
`shipit-atlassian-dashboard' shows the same board as text."
  (interactive)
  (shipit-atlassian-kanban--require-canvas)
  (unless (or board-id shipit-atlassian-board-ids)
    (user-error "No Jira boards configured; set `shipit-atlassian-board-ids'"))
  (let* ((repo (or (shipit--repo-for-buffer)
                   (user-error "Could not detect repository from git remote")))
         (config (cdr (shipit-issue--resolve-for-repo repo))))
    (if board-id
        (shipit-atlassian-kanban--open repo config board-id)
      (shipit-atlassian-kanban--pick-board
       config
       (lambda (chosen)
         (when chosen
           (shipit-atlassian-kanban--open repo config chosen)))))))

(provide 'shipit-atlassian-kanban)
;;; shipit-atlassian-kanban.el ends here
