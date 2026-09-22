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
(require 'shipit-atlassian-board)
(require 'shipit-canvas)

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
(declare-function canvas-diagram-nodes-shown "canvas-diagram")
(declare-function canvas-diagram-mode "canvas-diagram")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit--get-repo-from-remote "shipit-core")
(declare-function shipit-issue-jira--fetch-myself "shipit-issue-jira")
(declare-function consult--read "consult")
(declare-function consult--prompt "consult")
(defvar canvas-diagram-mode-map)
(declare-function canvas-cairo-text "canvas-cairo")
(declare-function canvas-cairo-text-size "canvas-cairo")
(declare-function canvas-cairo-svg "canvas-cairo")
(declare-function canvas-cairo-fill "canvas-cairo")
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

(defun shipit-atlassian-kanban--spec (board pending &optional collapsed me filter tracker)
  "Return the drawing spec for BOARD.
PENDING is the staged moves, COLLAPSED the names of the columns that
are shut, ME the account id of the current user, and FILTER what the
board is narrowed to.  TRACKER is the plist of the issue backend, whose
`:priority-color', `:issue-type-color' and `:issue-type-octicon' the
cards are drawn with, or nil.  A plist, so this can grow another thing
the drawing shows without every caller changing."
  (list :board board :pending pending :collapsed collapsed
        :me me :filter filter :tracker tracker))

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

(defun shipit-atlassian-kanban--spec-tracker (spec)
  "Return the plist of the issue backend SPEC draws the cards with, or nil."
  (plist-get spec :tracker))

;;; Narrowing the board
;;
;; A filter is a plist: `:mine' shows only what the current user is
;; assigned, `:type' only one issue type, `:priority' only one
;; priority, and `:text' only the cards whose key or summary carries
;; it.  They combine, so a card has to pass every part that is set.

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
TRACKER is the plist of the issue backend, as in the spec, or nil."
  name columns index frames headers me tracker)

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

(defun shipit-atlassian-kanban--build (_diagram spec)
  "Build the model of SPEC: its columns, with the staged moves applied."
  (let* ((board (shipit-atlassian-kanban--spec-board spec))
         (columns (shipit-atlassian-kanban--columns
                   (append (shipit-atlassian-board--columns board) nil)
                   (shipit-atlassian-kanban--spec-pending spec)
                   (shipit-atlassian-kanban--spec-collapsed spec)
                   (shipit-atlassian-kanban--spec-filter spec)
                   (shipit-atlassian-kanban--spec-me spec))))
    (let ((model (shipit-atlassian-kanban--board-create
                  :me (shipit-atlassian-kanban--spec-me spec)
                  :tracker (shipit-atlassian-kanban--spec-tracker spec)
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
staged for a card, and the outline already says its type, so the mark
is text."
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

(defun shipit-atlassian-kanban--note (model key)
  "Return the hover note for issue KEY in MODEL."
  (let* ((card (shipit-atlassian-kanban--card model key))
         (issue (plist-get card :issue))
         (staged (plist-get card :staged)))
    (string-join
     (append (shipit-atlassian-kanban--fields issue)
             (when staged (list (format "Staged for: %s" staged))))
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
  "Return the first line of a card: its KEY, marked when ISSUE is mine."
  (if (shipit-atlassian-kanban--mine-p
       issue (shipit-atlassian-kanban-board-me model))
      (concat key " " shipit-atlassian-kanban-mine-mark)
    key))

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

(defun shipit-atlassian-kanban--lay-out-column (diagram model column index ctx measure x)
  "Lay COLUMN out at INDEX with its frame starting at X; return its nodes.
The header comes first, then the cards, so reading order runs down a
column before it starts the next."
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
     (+ (canvas-diagram-node-y header) (canvas-diagram-node-h header)
        shipit-atlassian-kanban--card-gap)
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
    all))

;;; Drawing the columns
;;
;; canvas-diagram calls `:draw-edges' before it draws the boxes, so the
;; frames and headers drawn here sit behind the cards.

(defun shipit-atlassian-kanban--frame-height (nodes ctx)
  "Height every column frame takes, given the laid-out NODES on CTX."
  (+ (apply #'max
            (shipit-atlassian-kanban--line-height ctx)
            (mapcar (lambda (n) (+ (canvas-diagram-node-y n)
                                   (canvas-diagram-node-h n)))
                    nodes))
     shipit-atlassian-kanban--frame-pad))

(defun shipit-atlassian-kanban--draw-frame (ctx frame height)
  "Fill one column FRAME, an (X . WIDTH) pair, HEIGHT tall."
  (canvas-diagram-set-rgb ctx (canvas-diagram-color :edge) 0.12)
  (canvas-diagram-rounded-rect ctx (car frame) 0 (cdr frame) height 8)
  (canvas-cairo-fill ctx))

(defun shipit-atlassian-kanban--draw-columns (diagram ctx)
  "Draw the frame behind every column of DIAGRAM on CTX.
The header is a node, so canvas-diagram draws it with the cards."
  (let* ((model (canvas-diagram-model diagram))
         (height (shipit-atlassian-kanban--frame-height
                  (canvas-diagram-nodes diagram) ctx)))
    (dolist (frame (shipit-atlassian-kanban-board-frames model))
      (shipit-atlassian-kanban--draw-frame ctx frame height))))

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
The `:draw-over' callback: the type icons and the priority marks."
  (shipit-atlassian-kanban--draw-card-marks diagram ctx))

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
                          "card width" 'shipit-atlassian-kanban-card-width)))]))
   t))

;;; How the board plugs into canvas-diagram

(defun shipit-atlassian-kanban--node-text (diagram node)
  "Return what NODE says in DIAGRAM, whether it is a card or a header."
  (let ((model (canvas-diagram-model diagram)))
    (plist-get (or (shipit-atlassian-kanban--card-of model node)
                   (shipit-atlassian-kanban--header-of model node))
               :text)))

(defun shipit-atlassian-kanban--node-rgb (diagram node)
  "Return the fill of NODE\\='s card in DIAGRAM.
A staged card takes the staged colour, so a move shows without
opening anything.  Any other card takes the colour of the page: the
column frame shows the column, so a colour of the column would only
compete with the colours that mean something."
  (let* ((model (canvas-diagram-model diagram))
         (card (shipit-atlassian-kanban--card-of model node)))
    (cond
     ((null card) (canvas-diagram-color :node))
     ((plist-get card :staged)
      (or (canvas-diagram-rgb shipit-atlassian-kanban-staged-color)
          (canvas-diagram-color :selection)))
     (t (canvas-diagram-color :background)))))

(defun shipit-atlassian-kanban--header (diagram node)
  "Return the header line of NODE in DIAGRAM: its board and its column."
  (let ((model (canvas-diagram-model diagram)))
    (format "%s › %s"
            (shipit-atlassian-kanban-board-name model)
            (shipit-atlassian-kanban-column-name
             (shipit-atlassian-kanban--column-of model node)))))

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

(defvar-local shipit-atlassian-kanban--tracker nil
  "The plist of the issue backend whose issues this board shows.")

(defvar-local shipit-atlassian-kanban--drawing nil
  "The `canvas-diagram' this buffer draws, kept so its spec can be replaced.")

(defun shipit-atlassian-kanban--buffer-name (repo board)
  "Return the buffer name for BOARD of REPO."
  (format "*shipit-kanban: %s %s*" repo (or (cdr (assq 'name board)) "board")))

(defun shipit-atlassian-kanban--update-mode-line ()
  "Show how many moves are staged, or nothing when none are."
  (setq mode-line-process
        (string-join
         (delq nil
               (list (when shipit-atlassian-kanban--pending
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
issue key."
  (shipit-canvas-set-slot
   'canvas-diagram 'spec shipit-atlassian-kanban--drawing
   (shipit-atlassian-kanban--spec shipit-atlassian-kanban--board
                                  shipit-atlassian-kanban--pending
                                  shipit-atlassian-kanban--collapsed
                                  shipit-atlassian-kanban--me
                                  shipit-atlassian-kanban--filter
                                  shipit-atlassian-kanban--tracker))
  (canvas-diagram-rebuild)
  (canvas-diagram-redraw)
  (shipit-atlassian-kanban--update-mode-line))

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
  "Fetch this board again and draw it, keeping the staged moves."
  (interactive)
  (let ((buffer (current-buffer)))
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

(defun shipit-atlassian-kanban-visit-issue ()
  "Open the issue of the card the keyboard is on in a shipit buffer."
  (interactive)
  (shipit-issues-open-buffer (shipit-atlassian-kanban--selected-key)
                             shipit-atlassian-kanban--repo))

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

(defun shipit-atlassian-kanban--tracker-of (repo)
  "Return the plist of the issue backend of REPO."
  (car (shipit-issue--resolve-for-repo repo)))

(defun shipit-atlassian-kanban--show (board repo config board-id)
  "Draw BOARD of REPO on a canvas and return the buffer.
CONFIG is the Jira backend config plist and BOARD-ID the board\='s
identifier, both kept for reading the board again."
  (shipit-atlassian-kanban--require-canvas)
  (let* ((drawing (shipit-atlassian-kanban--diagram))
         (tracker (shipit-atlassian-kanban--tracker-of repo))
         (buffer (canvas-diagram-show
                  (shipit-atlassian-kanban--buffer-name repo board)
                  #'shipit-atlassian-kanban-mode
                  drawing
                  (shipit-atlassian-kanban--spec board nil nil nil nil tracker))))
    (with-current-buffer buffer
      (setq shipit-atlassian-kanban--repo repo
            shipit-atlassian-kanban--config config
            shipit-atlassian-kanban--board board
            shipit-atlassian-kanban--board-id board-id
            shipit-atlassian-kanban--tracker tracker
            shipit-atlassian-kanban--drawing drawing
            shipit-atlassian-kanban--me (shipit-atlassian-kanban--whoami config))
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
  (let* ((repo (or (shipit--get-repo-from-remote)
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
