;;; shipit-stack-canvas.el --- A stack of pull requests drawn on a canvas -*- lexical-binding: t; -*-

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
;; Draws the stack a stacked preview holds as a graph on an Emacs 32
;; canvas, through canvas-graph.  A box stands for each layer and one
;; for the base, the top layer at the top, with an arrow from each layer
;; to the one below it.  A box is filled by how far its layer has got.
;;
;; The canvas holds no state of its own.  It follows the preview buffer:
;; canvas-diagram reads a followed buffer again after it changes, and the
;; preview draws itself by erasing its text and inserting it again, so
;; every redraw there draws the canvas again.  The keys run the
;; preview's own commands, in the preview.
;;
;; Nothing here loads until the preview's key asks for it, once
;; `shipit-canvas-available-p' says canvas-graph loads.  Every canvas
;; function is reached through `declare-function', so this file loads
;; and compiles on an Emacs without the canvas.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-canvas)

;; The stacked preview is a user interface file too, so it is not
;; required.  Its key is the way here, so it is always loaded first.
(declare-function shipit-stacked-preview-mode "shipit-stacked-preview")
(declare-function shipit-stacked-preview--current-layers "shipit-stacked-preview")
(declare-function shipit-stacked-preview--display-commits "shipit-stacked-preview"
                  (commits))
(declare-function shipit-stacked-preview--layer-position "shipit-stacked-preview"
                  (branch))
(declare-function shipit-stacked-preview--layer-named "shipit-stacked-preview"
                  (branch))
(declare-function shipit-stacked-preview--base-name "shipit-stacked-preview" (ref))
(declare-function shipit-stacked-preview--visit "shipit-stacked-preview" (layer))
(declare-function shipit-stacked-preview-push "shipit-stacked-preview"
                  (buffer branch))
(declare-function shipit-stacked-preview-refresh "shipit-stacked-preview")
(defvar shipit-stacked-preview--base)
(defvar shipit-stacked-preview--states-shown)

(declare-function canvas-graph-diagram "canvas-graph" (&optional callbacks))
(declare-function canvas-graph-build "canvas-graph" (spec &optional style))
(declare-function canvas-graph-header "canvas-graph" (graph node &optional place))
(declare-function canvas-graph-card "canvas-graph" (graph node))
(declare-function canvas-graph-node-id "canvas-graph" (node))
(declare-function canvas-graph-export "canvas-graph" (spec file &optional diagram))
(declare-function canvas-graph-mode "canvas-graph")
(declare-function canvas-diagram-show "canvas-diagram"
                  (name mode diagram spec &optional source))
(declare-function canvas-diagram-spec "canvas-diagram" (diagram))
(declare-function canvas-diagram-model "canvas-diagram" (diagram))
(declare-function canvas-diagram-selected "canvas-diagram")
(declare-function canvas-diagram-refresh "canvas-diagram" (&rest _))
(declare-function canvas-diagram-color "canvas-diagram" (key))
(declare-function canvas-diagram-rgb "canvas-diagram" (name))
(declare-function canvas-diagram-blend "canvas-diagram" (a b fraction))
(defvar canvas-graph-mode-map)

(defgroup shipit-stack-canvas nil
  "A stack of pull requests drawn on a canvas."
  :group 'shipit)

(defcustom shipit-stack-canvas-row-width 50
  "Characters a commit subject keeps on its layer's box.
A box is as wide as its widest row, so a longer subject is cut and ends
in an ellipsis.  The card shows it whole."
  :type 'natnum
  :group 'shipit-stack-canvas)

(defcustom shipit-stack-canvas-fill-strength 0.35
  "How far a box's fill goes from the background to its class's colour.
At 0 a box is the colour of the background.  At 1 it is the colour
itself, which the text may not read on."
  :type 'number
  :group 'shipit-stack-canvas)

(defcustom shipit-stack-canvas-state-faces
  '((sending . warning)
    (no-branch . shadow)
    (not-pushed . font-lock-comment-face)
    (pushed . success)
    (drifted . font-lock-constant-face)
    (pull-request . link)
    (checks-pending . warning)
    (checks-failing . error))
  "The face whose foreground colours a layer's box, by the layer's class.
A class is how far the layer has got, with a pull request told apart by
its checks: `sending', `no-branch', `not-pushed', `pushed', `drifted',
`pull-request', `checks-pending' and `checks-failing'."
  :type '(alist :key-type symbol :value-type face)
  :group 'shipit-stack-canvas)

(defconst shipit-stack-canvas--class-names
  '((sending . "sending")
    (no-branch . "no branch yet")
    (not-pushed . "not pushed")
    (pushed . "pushed")
    (drifted . "pushed, changed since")
    (pull-request . "pull request")
    (checks-pending . "checks running")
    (checks-failing . "checks failing"))
  "What the legend calls each class, in the order it lists them.")

(defconst shipit-stack-canvas--style '(:start "top")
  "How a stack speaks on the graph: its start node is the top layer.")

(defvar-local shipit-stack-canvas--preview nil
  "The stacked preview buffer this canvas follows.")

;;; Reading the preview

(defun shipit-stack-canvas--row (subject)
  "Return SUBJECT cut to `shipit-stack-canvas-row-width' for a box row."
  (truncate-string-to-width subject shipit-stack-canvas-row-width nil nil "…"))

(defun shipit-stack-canvas--seen-layer (layer states)
  "Return what the canvas draws of LAYER, as a plist.
`:branch' names it, `:subjects' holds its commit subjects newest first,
`:state' is its state and `:pos' is where its row starts in the
preview.  STATES are the layer states the preview drew, by branch."
  (let ((branch (cdr (assq 'branch layer))))
    (list :branch branch
          :subjects (mapcar (lambda (commit) (or (plist-get commit :first-line) ""))
                            (shipit-stacked-preview--display-commits
                             (cdr (assq 'commits layer))))
          :state (or (cdr (assoc branch states))
                     (error "shipit-stack-canvas: the preview drew no state for %s"
                            branch))
          :pos (shipit-stacked-preview--layer-position branch))))

(defun shipit-stack-canvas--node (seen)
  "Return the canvas-graph node of the layer SEEN."
  (list (plist-get seen :branch)
        :rows (mapcar #'shipit-stack-canvas--row (plist-get seen :subjects))
        :pos (plist-get seen :pos)))

(defun shipit-stack-canvas--spec (layers base)
  "Return the canvas-graph spec of LAYERS, bottom first, sitting on BASE.
Each of LAYERS is what `shipit-stack-canvas--seen-layer' returns.  The
graph starts at the top layer, so the layered layout puts it at the
top.  An edge goes from each layer to the one below it, and from the
bottom layer to BASE.  `:data' holds each layer's state and subjects
by branch, for the colours, the header and the card."
  (let* ((top-first (reverse layers))
         (chain (append (mapcar (lambda (seen) (plist-get seen :branch)) top-first)
                        (list base))))
    (list :name (car chain)
          :start (car chain)
          :nodes (append (mapcar #'shipit-stack-canvas--node top-first)
                         (list (list base)))
          :edges (cl-mapcar #'list chain (cdr chain))
          :data (mapcar (lambda (seen)
                          (list (plist-get seen :branch)
                                :state (plist-get seen :state)
                                :subjects (plist-get seen :subjects)))
                        layers))))

(defun shipit-stack-canvas--read (buffer)
  "Return the canvas-graph spec of the stack BUFFER shows.
Nil when BUFFER is no stacked preview, which leaves the last drawing
standing.  The states are the ones the preview drew with: this runs
after every redraw there, and working out a state asks git and the
forge."
  (with-current-buffer buffer
    (when (derived-mode-p 'shipit-stacked-preview-mode)
      (shipit-stack-canvas--spec
       (mapcar (lambda (layer)
                 (shipit-stack-canvas--seen-layer
                  layer shipit-stacked-preview--states-shown))
               (shipit-stacked-preview--current-layers))
       (shipit-stacked-preview--base-name shipit-stacked-preview--base)))))

;;; Colours, header and card

(defun shipit-stack-canvas--class (state)
  "Return the colour class of a layer in STATE.
A pull request is told apart by its checks, the way its row glyph is."
  (let ((stage (plist-get state :state)))
    (if (eq stage 'pull-request)
        (pcase (plist-get state :checks)
          ((or 'nil "success") 'pull-request)
          ((or "failure" "error") 'checks-failing)
          (_ 'checks-pending))
      stage)))

(defun shipit-stack-canvas--face-rgb (face)
  "Return FACE's foreground as (R G B), or the text colour without one."
  (or (canvas-diagram-rgb (face-attribute face :foreground nil t))
      (canvas-diagram-color :text)))

(defun shipit-stack-canvas--fill (class)
  "Return the fill of a box in CLASS.
The background moves `shipit-stack-canvas-fill-strength' of the way to
the colour of the class's face, so the text reads in any theme."
  (canvas-diagram-blend
   (canvas-diagram-color :background)
   (shipit-stack-canvas--face-rgb
    (or (alist-get class shipit-stack-canvas-state-faces)
        (error "shipit-stack-canvas: no face for the class %S" class)))
   shipit-stack-canvas-fill-strength))

(defun shipit-stack-canvas--layer-of (diagram node)
  "Return what DIAGRAM's spec holds of NODE's layer, or nil for the base."
  (cdr (assoc (canvas-graph-node-id node)
              (plist-get (canvas-diagram-spec diagram) :data))))

(defun shipit-stack-canvas--node-rgb (diagram node)
  "Return the fill of NODE's box in DIAGRAM, or nil for the base.
The base then takes the plain box colour."
  (when-let* ((layer (shipit-stack-canvas--layer-of diagram node)))
    (shipit-stack-canvas--fill
     (shipit-stack-canvas--class (plist-get layer :state)))))

(defun shipit-stack-canvas--legend (diagram)
  "Return the legend rows of DIAGRAM: a filled swatch per class in use."
  (let ((in-use (mapcar (lambda (entry)
                          (shipit-stack-canvas--class (plist-get (cdr entry) :state)))
                        (plist-get (canvas-diagram-spec diagram) :data))))
    (cl-loop for (class . name) in shipit-stack-canvas--class-names
             when (memq class in-use)
             collect (list name (shipit-stack-canvas--fill class) 'fill))))

(defun shipit-stack-canvas--stage-words (state)
  "Return the words for how far a layer in STATE has got."
  (pcase (plist-get state :state)
    ('pull-request (format "PR #%s" (plist-get state :number)))
    ('drifted "pushed")
    (stage (or (alist-get stage shipit-stack-canvas--class-names)
               (error "shipit-stack-canvas: no words for the state %S" stage)))))

(defun shipit-stack-canvas--state-words (state)
  "Return a layer's STATE in words.
The stage comes first, then the checks, the review and the drift."
  (string-join
   (delq nil (list (shipit-stack-canvas--stage-words state)
                   (when-let* ((checks (plist-get state :checks)))
                     (concat "checks " checks))
                   (when-let* ((review (plist-get state :review)))
                     (string-replace "_" " " review))
                   (plist-get state :drift)))
   " · "))

(defun shipit-stack-canvas--header (diagram node)
  "Return the header line of DIAGRAM on NODE: the graph's, and the state."
  (concat (canvas-graph-header (canvas-diagram-model diagram) node)
          " · "
          (if-let* ((layer (shipit-stack-canvas--layer-of diagram node)))
              (shipit-stack-canvas--state-words (plist-get layer :state))
            "base")))

(defun shipit-stack-canvas--card (diagram node)
  "Return NODE's card in DIAGRAM.
A layer's card holds its state in words and its commit subjects in
full, above the edges canvas-graph lists."
  (pcase-let ((`(,title ,path ,body)
               (canvas-graph-card (canvas-diagram-model diagram) node)))
    (if-let* ((layer (shipit-stack-canvas--layer-of diagram node)))
        (list title path
              (concat (shipit-stack-canvas--state-words (plist-get layer :state))
                      "\n\n" (string-join (plist-get layer :subjects) "\n")
                      "\n\n" body))
      (list title path body))))

(defun shipit-stack-canvas--build (_diagram spec)
  "Return the graph of the stack SPEC describes."
  (canvas-graph-build spec shipit-stack-canvas--style))

(defun shipit-stack-canvas--diagram ()
  "Return a fresh diagram of a stack."
  (canvas-graph-diagram
   (list :build #'shipit-stack-canvas--build
         :node-rgb #'shipit-stack-canvas--node-rgb
         :legend #'shipit-stack-canvas--legend
         :header #'shipit-stack-canvas--header
         :card #'shipit-stack-canvas--card
         :read-source #'shipit-stack-canvas--read)))

;;; Commands

(defun shipit-stack-canvas--live-preview ()
  "Return the stacked preview this canvas follows, or signal it is gone."
  (if (buffer-live-p shipit-stack-canvas--preview)
      shipit-stack-canvas--preview
    (user-error "The stacked preview this stack was drawn from is gone")))

(defun shipit-stack-canvas--in-layer (fn)
  "Call FN in the preview with the layer the keyboard is on.
Signals on the base, the branch the stack sits on, which is no layer."
  (let ((preview (shipit-stack-canvas--live-preview))
        (branch (canvas-graph-node-id
                 (or (canvas-diagram-selected)
                     (user-error "The keyboard is on no box")))))
    (with-current-buffer preview
      (when (equal branch (shipit-stacked-preview--base-name
                           shipit-stacked-preview--base))
        (user-error "%s is the base the stack sits on, not a layer" branch))
      (funcall fn (shipit-stacked-preview--layer-named branch)))))

(defun shipit-stack-canvas-visit-layer ()
  "Open the layer the keyboard is on, as RET does in the stacked preview.
A layer with a pull request opens it, and the preview describes one
without."
  (interactive)
  (shipit-stack-canvas--in-layer #'shipit-stacked-preview--visit))

(defun shipit-stack-canvas-push ()
  "Open the stacked preview's push menu for the layer the keyboard is on."
  (interactive)
  (shipit-stack-canvas--in-layer
   (lambda (layer)
     (shipit-stacked-preview-push (current-buffer) (cdr (assq 'branch layer))))))

(defun shipit-stack-canvas-refresh ()
  "Read the stack again in the stacked preview, and draw it at once."
  (interactive)
  (with-current-buffer (shipit-stack-canvas--live-preview)
    (shipit-stacked-preview-refresh))
  (canvas-diagram-refresh))

;;; The mode

(defvar shipit-stack-canvas-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-stack-canvas-visit-layer)
    (define-key map (kbd "g") #'shipit-stack-canvas-refresh)
    (define-key map (kbd "P") #'shipit-stack-canvas-push)
    map)
  "Keys a stack has over those of every graph on a canvas.
RET, g and P do what they do in the stacked preview.  P is the key
canvas-diagram cycles the palette with, which the stack's own fills
leave nothing to do; the menu still cycles it.")

(with-eval-after-load 'meow
  (shipit-canvas-keep-other-packages-away 'shipit-stack-canvas-mode))

(with-eval-after-load 'undo-tree
  (shipit-canvas-keep-other-packages-away 'shipit-stack-canvas-mode))

(define-derived-mode shipit-stack-canvas-mode canvas-graph-mode "Stack"
  "Major mode of a buffer drawing a stack of pull requests on a canvas.

The keyboard is on one box, ringed: a layer, or the base the stack sits
on.  The keys that move point move it.
\\[shipit-stack-canvas-visit-layer] opens the layer's pull request, or
hands a layer without one to the preview to describe.
\\[shipit-stack-canvas-push] opens the push menu for the layer, and
\\[shipit-stack-canvas-refresh] reads the stack again.  A click opens
the box's card.  The drawing follows the stacked preview it came from,
and draws again whenever the preview does.

\\{shipit-stack-canvas-mode-map}"
  (set-keymap-parent shipit-stack-canvas-mode-map canvas-graph-mode-map)
  (shipit-canvas-keep-other-packages-away 'shipit-stack-canvas-mode))

;;; Showing and exporting

(defun shipit-stack-canvas--require-canvas ()
  "Signal unless this Emacs can draw a graph on a canvas."
  (shipit-canvas-require "The stack canvas" "shipit-stacked-preview" 'canvas-graph))

(defun shipit-stack-canvas--buffer-name (spec)
  "Return the name of the canvas buffer drawing SPEC, after its top layer."
  (format "*shipit-stack: %s*" (plist-get spec :name)))

(defun shipit-stack-canvas--spec-of (preview)
  "Return the spec of the stack PREVIEW shows, or signal it shows none."
  (or (shipit-stack-canvas--read preview)
      (user-error "%s is no stacked preview" (buffer-name preview))))

(defun shipit-stack-canvas-show (preview)
  "Draw the stack PREVIEW shows on a canvas that follows it.
Return the canvas buffer.  Drawing the same stack again brings its
buffer back, drawn afresh."
  (shipit-stack-canvas--require-canvas)
  (let* ((spec (shipit-stack-canvas--spec-of preview))
         (buffer (canvas-diagram-show (shipit-stack-canvas--buffer-name spec)
                                      #'shipit-stack-canvas-mode
                                      (shipit-stack-canvas--diagram)
                                      spec preview)))
    (with-current-buffer buffer
      (setq shipit-stack-canvas--preview preview))
    buffer))

(defun shipit-stack-canvas-export (preview file)
  "Draw the stack PREVIEW shows into FILE and return FILE.
FILE is a PNG, an SVG or a PDF by its name.  No frame is needed."
  (shipit-stack-canvas--require-canvas)
  (canvas-graph-export (shipit-stack-canvas--spec-of preview) file
                       (shipit-stack-canvas--diagram)))
(provide 'shipit-stack-canvas)
;;; shipit-stack-canvas.el ends here
