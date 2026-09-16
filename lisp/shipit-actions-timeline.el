;;; shipit-actions-timeline.el --- An Actions run drawn on a time axis -*- lexical-binding: t; -*-

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
;; Draws the jobs of one GitHub Actions run on an Emacs 32 canvas,
;; through canvas-diagram.  Each job is a box on a row of its own,
;; placed where it started on a time axis the whole run shares, and as
;; wide as it ran.  The jobs that ran side by side and the time between
;; them then show, which the duration bars of the Actions list cannot:
;; every bar there starts at the same column.
;;
;; Nothing here loads until the Actions list's key asks for it, once
;; `shipit-canvas-available-p' says the canvas works.  Every canvas
;; function is reached through `declare-function', so this file loads
;; and compiles on an Emacs without the canvas.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-canvas)

;; The run viewer is a user interface file too, so it is not required.
;; The Actions list opens this and loads the run viewer, so it is there.
(declare-function shipit-actions--status-color "shipit-actions" (status))
(declare-function shipit-actions--fetch-jobs "shipit-actions" (repo run-id callback))
(declare-function shipit-open-actions-run "shipit-actions"
                  (repo run-id &optional job-id pr-number))

(declare-function canvas-diagram-create "canvas-diagram")
(declare-function canvas-diagram-node-create "canvas-diagram")
(declare-function canvas-diagram-node-label "canvas-diagram" (node))
(declare-function canvas-diagram-node-y "canvas-diagram" (node))
(declare-function canvas-diagram-node-w "canvas-diagram" (node))
(declare-function canvas-diagram-model "canvas-diagram" (diagram))
(declare-function canvas-diagram-nodes "canvas-diagram" (diagram))
(declare-function canvas-diagram-measure "canvas-diagram" (ctx))
(declare-function canvas-diagram-font "canvas-diagram")
(declare-function canvas-diagram-color "canvas-diagram" (key))
(declare-function canvas-diagram-rgb "canvas-diagram" (name))
(declare-function canvas-diagram-set-rgb "canvas-diagram" (ctx rgb &optional alpha))
(declare-function canvas-diagram-neighbour "canvas-diagram" (nodes node step))
(declare-function canvas-diagram-show "canvas-diagram"
                  (name mode diagram spec &optional source))
(declare-function canvas-diagram-export "canvas-diagram" (diagram spec file))
(declare-function canvas-diagram-selected "canvas-diagram")
(declare-function canvas-diagram-go "canvas-diagram" (node))
(declare-function canvas-diagram-rebuild "canvas-diagram")
(declare-function canvas-diagram-redraw "canvas-diagram")
(declare-function canvas-diagram-mode "canvas-diagram")
(declare-function canvas-cairo-text "canvas-cairo")
(declare-function canvas-cairo-text-size "canvas-cairo")
(declare-function canvas-cairo-new-path "canvas-cairo")
(declare-function canvas-cairo-move-to "canvas-cairo")
(declare-function canvas-cairo-line-to "canvas-cairo")
(declare-function canvas-cairo-stroke "canvas-cairo")
(declare-function canvas-cairo-set-line-width "canvas-cairo")
(defvar canvas-diagram-mode-map)

(defgroup shipit-actions-timeline nil
  "An Actions run drawn on a time axis."
  :group 'shipit)

(defcustom shipit-actions-timeline-width 900
  "Pixels the whole run spans on the time axis, at natural zoom.
A job too short to see at that scale is still drawn a few pixels
wide, so it can be seen and chosen."
  :type 'natnum
  :group 'shipit-actions-timeline)

(defconst shipit-actions-timeline--least-width 4
  "Pixels a box is wide at least, however short its job ran.")

(defconst shipit-actions-timeline--row-gap 6
  "Pixels between two rows.")

(defconst shipit-actions-timeline--name-gap 16
  "Pixels between the column of names and the start of the time axis.")

(defconst shipit-actions-timeline--tick-room 80
  "Pixels two ticks of the time axis are apart at least, for their labels.")

(defconst shipit-actions-timeline--tick-length 6
  "Pixels a tick reaches above the first row.")

(defconst shipit-actions-timeline--intervals
  '(1 2 5 10 15 30 60 120 300 600 900 1800 3600 7200 14400)
  "The round times, in seconds, that two ticks may be apart.")

;;; Placing jobs in time

(defun shipit-actions-timeline--seconds (time)
  "Return TIME, an ISO 8601 string, as seconds since the epoch, or nil.
The API gives a time it does not have as nil or as `:null'."
  (and (stringp time) (float-time (date-to-time time))))

(defun shipit-actions-timeline--status (item)
  "Return what ITEM, a job or a step, is drawn as.
Its conclusion once it has one, else its status."
  (or (cdr (assq 'conclusion item)) (cdr (assq 'status item)) "unknown"))

(defun shipit-actions-timeline--span (item origin now)
  "Return (START . END) of ITEM, a job or a step, in seconds from ORIGIN.
An item still running ends at NOW, and one that never started has no
span."
  (when-let* ((start (shipit-actions-timeline--seconds (cdr (assq 'started_at item)))))
    (cons (- start origin)
          (- (or (shipit-actions-timeline--seconds (cdr (assq 'completed_at item))) now)
             origin))))

(defun shipit-actions-timeline--origin (jobs)
  "Return when the first of JOBS started, in seconds since the epoch, or nil."
  (when-let* ((starts (delq nil (mapcar (lambda (job)
                                          (shipit-actions-timeline--seconds
                                           (cdr (assq 'started_at job))))
                                        jobs))))
    (apply #'min starts)))

(defun shipit-actions-timeline--placed (item key origin now)
  "Return ITEM placed in time as a plist under KEY, or nil when it never started.
KEY is `:job' or `:step'.  ORIGIN and NOW are as
`shipit-actions-timeline--span' takes them."
  (when-let* ((span (shipit-actions-timeline--span item origin now)))
    (list key item :start (car span) :end (cdr span)
          :status (shipit-actions-timeline--status item))))

(defun shipit-actions-timeline--steps (job origin now)
  "Return the steps of JOB that started, placed in time."
  (delq nil (mapcar (lambda (step)
                      (shipit-actions-timeline--placed step :step origin now))
                    (append (cdr (assq 'steps job)) nil))))

(defun shipit-actions-timeline--rows (jobs now)
  "Return a row for each of JOBS that started, in the order given.
A row is a plist: `:job', `:start' and `:end' in seconds from the first
start, `:status', and `:steps' placed the same way.  The layout fills
in `:node' and `:text'.  NOW ends what is still running."
  (when-let* ((origin (shipit-actions-timeline--origin jobs)))
    (delq nil (mapcar (lambda (job)
                        (when-let* ((row (shipit-actions-timeline--placed job :job origin now)))
                          (append row
                                  (list :steps (shipit-actions-timeline--steps job origin now)
                                        :node nil :text nil))))
                      jobs))))

(defun shipit-actions-timeline--span-of (rows)
  "Return how far ROWS reach, in seconds, and at least one."
  (apply #'max 1.0 (mapcar (lambda (row) (plist-get row :end)) rows)))

;;; Writing times

(defun shipit-actions-timeline--tick-interval (scale)
  "Return the seconds between two ticks at SCALE pixels a second.
It is the shortest round time that keeps two ticks
`shipit-actions-timeline--tick-room' pixels apart."
  (or (seq-find (lambda (seconds)
                  (>= (* seconds scale) shipit-actions-timeline--tick-room))
                shipit-actions-timeline--intervals)
      (car (last shipit-actions-timeline--intervals))))

(defun shipit-actions-timeline--clock (seconds)
  "Return SECONDS in words: 45s, 1m 30s, 1h 5m.
A whole minute or hour leaves its zero part out, so a tick reads 2m."
  (let* ((total (round seconds))
         (hours (/ total 3600))
         (minutes (/ (% total 3600) 60))
         (secs (% total 60)))
    (cond ((> hours 0) (if (zerop minutes)
                           (format "%dh" hours)
                         (format "%dh %dm" hours minutes)))
          ((> minutes 0) (if (zerop secs)
                             (format "%dm" minutes)
                           (format "%dm %ds" minutes secs)))
          (t (format "%ds" secs)))))

(defun shipit-actions-timeline--took (item)
  "Return how long ITEM, a placed job or step, ran, in words."
  (shipit-actions-timeline--clock (- (plist-get item :end) (plist-get item :start))))

;;; The spec and the model

(defun shipit-actions-timeline--spec (run jobs &optional now)
  "Return the drawing spec of RUN with its JOBS, as they stood at NOW.
NOW is seconds since the epoch, the current time unless given, and it
ends the jobs still running."
  (list :run run :jobs jobs :now (or now (float-time))))

(defun shipit-actions-timeline--build (_diagram spec)
  "Return the model of SPEC: its run and its rows.
`:geometry' is left for the layout to fill in."
  (list :run (plist-get spec :run)
        :rows (shipit-actions-timeline--rows (plist-get spec :jobs) (plist-get spec :now))
        :geometry nil))

(defun shipit-actions-timeline--job-name (row)
  "Return the name of ROW's job."
  (or (cdr (assq 'name (plist-get row :job))) ""))

(defun shipit-actions-timeline--row-of (diagram node)
  "Return the row of DIAGRAM whose box is NODE."
  (or (cl-find node (plist-get (canvas-diagram-model diagram) :rows)
               :key (lambda (row) (plist-get row :node)) :test #'eq)
      (error "shipit-actions-timeline: no row has the box %s"
             (canvas-diagram-node-label node))))

;;; The layout

(defun shipit-actions-timeline--geometry (rows ctx)
  "Return where ROWS are drawn on CTX, as a plist.
`:left' is where the time axis starts, right of the column of names.
`:top' is where the first row starts, below the tick labels.  `:row-h'
is the height of a row, `:pad' the room above a name in it, and
`:scale' the pixels a second."
  (let* ((measure (canvas-diagram-measure ctx))
         (line (funcall measure "Ag"))
         (text-h (cdr (canvas-cairo-text-size ctx "Ag" (canvas-diagram-font)))))
    (list :left (+ (apply #'max 0 (mapcar (lambda (row)
                                            (car (funcall measure (shipit-actions-timeline--job-name row))))
                                          rows))
                   shipit-actions-timeline--name-gap)
          :top (+ (cdr line) shipit-actions-timeline--tick-length)
          :row-h (cdr line)
          :pad (/ (- (cdr line) text-h) 2.0)
          :scale (/ (float shipit-actions-timeline-width)
                    (shipit-actions-timeline--span-of rows)))))

(defun shipit-actions-timeline--node (row index geometry)
  "Return the box of ROW, the INDEXth row, placed by GEOMETRY."
  (let ((scale (plist-get geometry :scale)))
    (canvas-diagram-node-create
     :label (shipit-actions-timeline--job-name row)
     :x (+ (plist-get geometry :left) (* scale (plist-get row :start)))
     :y (+ (plist-get geometry :top)
           (* index (+ (plist-get geometry :row-h) shipit-actions-timeline--row-gap)))
     :w (max shipit-actions-timeline--least-width
             (* scale (- (plist-get row :end) (plist-get row :start))))
     :h (plist-get geometry :row-h))))

(defun shipit-actions-timeline--box-text (row node measure)
  "Return what ROW's box NODE says: how long it ran, when that fits.
MEASURE says how wide a text is."
  (let ((text (shipit-actions-timeline--took row)))
    (if (<= (car (funcall measure text)) (canvas-diagram-node-w node)) text "")))

(defun shipit-actions-timeline--lay-out (diagram ctx)
  "Place a box for each row of DIAGRAM on CTX; return the boxes in order."
  (let* ((model (canvas-diagram-model diagram))
         (rows (plist-get model :rows))
         (geometry (shipit-actions-timeline--geometry rows ctx))
         (measure (canvas-diagram-measure ctx))
         (index -1))
    (plist-put model :geometry geometry)
    (mapcar (lambda (row)
              (let ((node (shipit-actions-timeline--node row (cl-incf index) geometry)))
                (plist-put row :node node)
                (plist-put row :text (shipit-actions-timeline--box-text row node measure))
                node))
            rows)))

;;; Drawing the axis and the names
;;
;; canvas-diagram calls `:draw-edges' before it draws the boxes, so the
;; ticks drawn here run behind them.

(defun shipit-actions-timeline--bottom (rows geometry)
  "Return where the last of ROWS ends, laid out by GEOMETRY."
  (+ (plist-get geometry :top)
     (* (length rows) (+ (plist-get geometry :row-h) shipit-actions-timeline--row-gap))))

(defun shipit-actions-timeline--draw-tick (ctx x top bottom label font)
  "Draw on CTX a tick at X from above TOP down to BOTTOM, with LABEL above it."
  (canvas-diagram-set-rgb ctx (canvas-diagram-color :edge) 0.35)
  (canvas-cairo-new-path ctx)
  (canvas-cairo-move-to ctx x (- top shipit-actions-timeline--tick-length))
  (canvas-cairo-line-to ctx x bottom)
  (canvas-cairo-stroke ctx)
  (canvas-diagram-set-rgb ctx (canvas-diagram-color :edge))
  (canvas-cairo-text ctx (+ x 2) 0 label font))

(defun shipit-actions-timeline--draw-ticks (ctx rows geometry font)
  "Draw on CTX a tick at each round time across the span of ROWS.
GEOMETRY places them, and FONT writes their times."
  (let* ((scale (plist-get geometry :scale))
         (left (plist-get geometry :left))
         (top (plist-get geometry :top))
         (bottom (shipit-actions-timeline--bottom rows geometry)))
    (canvas-cairo-set-line-width ctx 1)
    (cl-loop for seconds from 0 to (shipit-actions-timeline--span-of rows)
             by (shipit-actions-timeline--tick-interval scale)
             do (shipit-actions-timeline--draw-tick
                 ctx (+ left (* scale seconds)) top bottom
                 (shipit-actions-timeline--clock seconds) font))))

(defun shipit-actions-timeline--draw-names (ctx rows geometry font)
  "Write on CTX the name of each of ROWS' jobs, level with its box.
GEOMETRY places them in the column left of the time axis."
  (canvas-diagram-set-rgb ctx (canvas-diagram-color :text))
  (dolist (row rows)
    (canvas-cairo-text ctx 0 (+ (canvas-diagram-node-y (plist-get row :node))
                                (plist-get geometry :pad))
                       (shipit-actions-timeline--job-name row) font)))

(defun shipit-actions-timeline--draw (diagram ctx)
  "Draw the time axis and the column of names of DIAGRAM on CTX."
  (let* ((model (canvas-diagram-model diagram))
         (rows (plist-get model :rows))
         (geometry (plist-get model :geometry))
         (font (canvas-diagram-font)))
    (shipit-actions-timeline--draw-ticks ctx rows geometry font)
    (shipit-actions-timeline--draw-names ctx rows geometry font)))

;;; Colours, header and card

(defun shipit-actions-timeline--status-rgb (status)
  "Return the colour of STATUS as (R G B), as the duration bars draw it."
  (canvas-diagram-rgb (shipit-actions--status-color status)))

(defun shipit-actions-timeline--node-rgb (diagram node)
  "Return the fill of NODE's box in DIAGRAM: the colour of its job's result."
  (shipit-actions-timeline--status-rgb
   (plist-get (shipit-actions-timeline--row-of diagram node) :status)))

(defun shipit-actions-timeline--legend (diagram)
  "Return the legend rows of DIAGRAM: a filled swatch per result in use."
  (mapcar (lambda (status)
            (list status (shipit-actions-timeline--status-rgb status) 'fill))
          (delete-dups (mapcar (lambda (row) (plist-get row :status))
                               (plist-get (canvas-diagram-model diagram) :rows)))))

(defun shipit-actions-timeline--node-text (diagram node)
  "Return what NODE's box in DIAGRAM says."
  (plist-get (shipit-actions-timeline--row-of diagram node) :text))

(defun shipit-actions-timeline--run-name (run)
  "Return the name of RUN: its workflow and its number."
  (format "%s #%s" (cdr (assq 'name run)) (cdr (assq 'run_number run))))

(defun shipit-actions-timeline--header (diagram node)
  "Return the header line of DIAGRAM on NODE: the run, the job and its times."
  (let ((row (shipit-actions-timeline--row-of diagram node)))
    (format "%s › %s · from %s · took %s · %s"
            (shipit-actions-timeline--run-name (plist-get (canvas-diagram-model diagram) :run))
            (shipit-actions-timeline--job-name row)
            (shipit-actions-timeline--clock (plist-get row :start))
            (shipit-actions-timeline--took row)
            (plist-get row :status))))

(defun shipit-actions-timeline--step-line (step row)
  "Return the card's line for STEP of ROW's job.
A step that never started has no time to give."
  (let ((placed (cl-find step (plist-get row :steps)
                         :key (lambda (item) (plist-get item :step)) :test #'eq)))
    (format "%-8s %s  %s"
            (if placed (shipit-actions-timeline--took placed) "")
            (cdr (assq 'name step))
            (shipit-actions-timeline--status step))))

(defun shipit-actions-timeline--card (diagram node)
  "Return NODE's card in DIAGRAM: when its job ran, and every step of it."
  (let ((row (shipit-actions-timeline--row-of diagram node)))
    (list (shipit-actions-timeline--job-name row)
          (shipit-actions-timeline--run-name (plist-get (canvas-diagram-model diagram) :run))
          (string-join
           (cons (format "Started %s into the run, took %s, %s"
                         (shipit-actions-timeline--clock (plist-get row :start))
                         (shipit-actions-timeline--took row)
                         (plist-get row :status))
                 (when-let* ((steps (append (cdr (assq 'steps (plist-get row :job))) nil)))
                   (cons "" (mapcar (lambda (step)
                                      (shipit-actions-timeline--step-line step row))
                                    steps))))
           "\n"))))

;;; Moving and keys

(defun shipit-actions-timeline--move (diagram node direction)
  "Return the box DIRECTION leads to from NODE in DIAGRAM, or nil to hold.
Up and down walk the rows.  A row has nothing beside it."
  (let ((nodes (canvas-diagram-nodes diagram)))
    (pcase direction
      ((or 'next 'next-sibling 'next-at-depth) (canvas-diagram-neighbour nodes node 1))
      ((or 'previous 'previous-sibling 'previous-at-depth)
       (canvas-diagram-neighbour nodes node -1))
      ('first (car nodes))
      ('last (car (last nodes))))))

(defun shipit-actions-timeline--job-id (row)
  "Return the id of ROW's job."
  (cdr (assq 'id (plist-get row :job))))

(defun shipit-actions-timeline--node-key (diagram node)
  "Return what names NODE across rebuilds of DIAGRAM: its job's id."
  (shipit-actions-timeline--job-id (shipit-actions-timeline--row-of diagram node)))

(defun shipit-actions-timeline--restore (diagram key)
  "Return the box of DIAGRAM for the job whose id is KEY, or nil."
  (plist-get (cl-find key (plist-get (canvas-diagram-model diagram) :rows)
                      :key #'shipit-actions-timeline--job-id :test #'equal)
             :node))

(defconst shipit-actions-timeline--callbacks
  (list :build #'shipit-actions-timeline--build
        :layout #'shipit-actions-timeline--lay-out
        :draw-edges #'shipit-actions-timeline--draw
        :node-text #'shipit-actions-timeline--node-text
        :node-rgb #'shipit-actions-timeline--node-rgb
        :legend #'shipit-actions-timeline--legend
        :header #'shipit-actions-timeline--header
        :card #'shipit-actions-timeline--card
        :move #'shipit-actions-timeline--move
        :node-key #'shipit-actions-timeline--node-key
        :restore #'shipit-actions-timeline--restore)
  "How the run timeline plugs into canvas-diagram.")

(defun shipit-actions-timeline--require-canvas ()
  "Signal unless this Emacs can draw on a canvas."
  (shipit-canvas-require "The run timeline" "shipit-open-actions-list"))

(defun shipit-actions-timeline--diagram ()
  "Return a fresh diagram of a run."
  (shipit-actions-timeline--require-canvas)
  (canvas-diagram-create :callbacks shipit-actions-timeline--callbacks))

(defvar-local shipit-actions-timeline--repo nil
  "Repository of the run this buffer draws.")

(defvar-local shipit-actions-timeline--run nil
  "The run this buffer draws.")

(defvar-local shipit-actions-timeline--drawing nil
  "The diagram this buffer shows, kept to give it a new spec.")

(defun shipit-actions-timeline-visit-job ()
  "Open the run viewer on this run, with the job the keyboard is on expanded."
  (interactive)
  (let ((node (or (canvas-diagram-selected) (user-error "The keyboard is on no job"))))
    (shipit-open-actions-run shipit-actions-timeline--repo
                             (cdr (assq 'id shipit-actions-timeline--run))
                             (canvas-diagram-node-label node))))

(defun shipit-actions-timeline--redraw (jobs)
  "Draw this buffer's run again with JOBS, the keyboard staying on its job."
  (shipit-canvas-set-slot 'canvas-diagram 'spec shipit-actions-timeline--drawing
                          (shipit-actions-timeline--spec shipit-actions-timeline--run jobs))
  (canvas-diagram-rebuild)
  (canvas-diagram-redraw))

(defun shipit-actions-timeline-refresh ()
  "Fetch the run's jobs again and draw them as they now stand."
  (interactive)
  (let ((buffer (current-buffer)))
    (shipit-actions--fetch-jobs
     shipit-actions-timeline--repo (cdr (assq 'id shipit-actions-timeline--run))
     (lambda (jobs)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (shipit-actions-timeline--redraw jobs)))))))

(defvar shipit-actions-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-actions-timeline-visit-job)
    (define-key map (kbd "g") #'shipit-actions-timeline-refresh)
    map)
  "Keys a run timeline has over those of every canvas diagram.")

(with-eval-after-load 'meow
  (shipit-canvas-keep-other-packages-away 'shipit-actions-timeline-mode))

(with-eval-after-load 'undo-tree
  (shipit-canvas-keep-other-packages-away 'shipit-actions-timeline-mode))

(define-derived-mode shipit-actions-timeline-mode canvas-diagram-mode "Timeline"
  "Major mode of a buffer drawing an Actions run on a time axis.

Each job is a box on its row, placed where it started and as wide as it
ran.  The keys that move point walk the rows.
\\[shipit-actions-timeline-visit-job] opens the job in the run viewer,
\\[canvas-diagram-toggle-card] opens its card with the time of each
step, and \\[shipit-actions-timeline-refresh] fetches the jobs again.

\\{shipit-actions-timeline-mode-map}"
  (set-keymap-parent shipit-actions-timeline-mode-map canvas-diagram-mode-map)
  (shipit-canvas-keep-other-packages-away 'shipit-actions-timeline-mode))

;;; Showing and exporting

(defun shipit-actions-timeline--buffer-name (run)
  "Return the name of the buffer drawing RUN."
  (format "*shipit-timeline: %s*" (shipit-actions-timeline--run-name run)))

(defun shipit-actions-timeline--select-job (job-id)
  "Put the keyboard on the box of the job JOB-ID, when the run holds it."
  (when-let* ((node (shipit-actions-timeline--restore shipit-actions-timeline--drawing job-id)))
    (canvas-diagram-go node)))

(defun shipit-actions-timeline-show (repo run jobs &optional job-id)
  "Draw RUN of REPO with its JOBS on a time axis, and return the buffer.
JOB-ID is the job the keyboard starts on, the first one unless given."
  (shipit-actions-timeline--require-canvas)
  (let* ((drawing (shipit-actions-timeline--diagram))
         (buffer (canvas-diagram-show (shipit-actions-timeline--buffer-name run)
                                      #'shipit-actions-timeline-mode drawing
                                      (shipit-actions-timeline--spec run jobs))))
    (with-current-buffer buffer
      (setq shipit-actions-timeline--repo repo
            shipit-actions-timeline--run run
            shipit-actions-timeline--drawing drawing)
      (when job-id
        (shipit-actions-timeline--select-job job-id)))
    buffer))

(defun shipit-actions-timeline-export (run jobs file)
  "Draw RUN with its JOBS into FILE and return FILE.
FILE is a PNG, an SVG or a PDF by its name.  No frame is needed."
  (shipit-actions-timeline--require-canvas)
  (canvas-diagram-export (shipit-actions-timeline--diagram)
                         (shipit-actions-timeline--spec run jobs) file))

(provide 'shipit-actions-timeline)
;;; shipit-actions-timeline.el ends here
