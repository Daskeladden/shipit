;;; test-shipit-stack-canvas.el --- A stack drawn on a canvas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The stack canvas draws the layers of a stacked preview as a graph.
;; The reader, the colour classes and the words are data work and run
;; on any Emacs.  The drawing, the keys and the export need the Emacs 32
;; canvas and canvas-graph, and those tests skip themselves without
;; them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'shipit-canvas)
(require 'shipit-stacked-preview)
(require 'shipit-stack-canvas)
;; A stack drawn in a real preview buffer, with only git and the forge
;; stood in for.
(require 'test-shipit-stacked-preview)

;; Load canvas-graph now, when this Emacs can: a helper below sets
;; canvas-diagram struct slots, and `load' expands `setf' as it reads
;; the form, which needs the struct known by then.
(shipit-canvas-available-p 'canvas-graph)

(defface test-stack-canvas--red '((t :foreground "#ff0000"))
  "A face whose foreground is pure red, for measuring a fill.")

(defface test-stack-canvas--green '((t :foreground "#00ff00"))
  "A face whose foreground is pure green, for measuring a fill.")

(defun test-stack-canvas--layer (branch &rest subjects)
  "Return a layer on BRANCH holding commits with SUBJECTS, oldest first."
  (list (cons 'branch branch)
        (cons 'commits (seq-map-indexed
                        (lambda (subject i)
                          (list :sha (format "%s-%d" branch i)
                                :first-line subject))
                        subjects))))

(defun test-stack-canvas--two-layers ()
  "Return a stack of two layers, bottom first."
  (list (test-stack-canvas--layer "one" "fpga: conventions"
                                  "fpga: point tests at the skill")
        (test-stack-canvas--layer "two" "uoe: abort path")))

(defun test-stack-canvas--seen (branch state &rest subjects)
  "Return what the canvas reads of a layer on BRANCH in STATE with SUBJECTS."
  (list :branch branch :subjects subjects :state state))

(defun test-stack-canvas--built (spec)
  "Return a stack diagram built from SPEC, not yet laid out."
  (let ((diagram (shipit-stack-canvas--diagram)))
    (setf (canvas-diagram-spec diagram) spec
          (canvas-diagram-model diagram)
          (canvas-diagram--call diagram :build diagram spec))
    diagram))

(defun test-stack-canvas--node (diagram name)
  "Return the box of DIAGRAM called NAME."
  (canvas-graph-node-named (canvas-diagram-model diagram) name))

(defmacro test-stack-canvas--with-canvas (canvas preview layers prs statuses &rest body)
  "Draw LAYERS in PREVIEW, open its canvas as CANVAS, and run BODY there.
PRS and STATUSES are what `test-stacked--with-drawn-stack' takes.
Skips when this Emacs cannot draw a graph on a canvas."
  (declare (indent 5))
  `(progn
     (skip-unless (shipit-canvas-available-p 'canvas-graph))
     (test-stacked--with-drawn-stack ,preview ,layers ,prs ,statuses
       (let ((,canvas (shipit-stack-canvas-show ,preview)))
         (unwind-protect
             (with-current-buffer ,canvas ,@body)
           (kill-buffer ,canvas))))))

(defun test-stack-canvas--select (name)
  "Put the keyboard on the box called NAME."
  (canvas-diagram-set-selected
   (canvas-graph-node-named (canvas-diagram-current-model) name)))

(defun test-stack-canvas--press (keys)
  "Run the command KEYS is bound to in the current buffer.
Not `execute-kbd-macro': its command loop looks keys up in the buffer
of the selected window, which need not be this one in a batch run."
  (call-interactively (key-binding keys)))

;;;; Reading the preview

(ert-deftest test-stack-canvas-reads-a-box-for-each-layer-and-the-base ()
  "GIVEN a stack of two layers drawn in the preview
WHEN the canvas reads it
THEN it starts at the top layer and names the graph after it, holds a
box for each layer and one for the base, top first, and chains them
down to the base.  A layer's rows are its commit subjects, newest
first, as the preview lists them."
  (let ((layers (test-stack-canvas--two-layers)))
    (test-stacked--with-drawn-stack preview layers nil nil
      (let* ((spec (shipit-stack-canvas--read preview))
             (nodes (plist-get spec :nodes)))
        (should (equal "two" (plist-get spec :name)))
        (should (equal "two" (plist-get spec :start)))
        (should (equal '("two" "one" "master") (mapcar #'car nodes)))
        (should (equal '("fpga: point tests at the skill" "fpga: conventions")
                       (plist-get (cdr (assoc "one" nodes)) :rows)))
        (should-not (plist-get (cdr (assoc "master" nodes)) :rows))
        (should (equal '(("two" "one") ("one" "master"))
                       (plist-get spec :edges)))))))

(ert-deftest test-stack-canvas-reads-where-each-layer-is-in-the-preview ()
  "GIVEN a stack drawn in the preview
WHEN the canvas reads it
THEN each layer's box knows where its row starts, so visiting the
source from a box lands on that layer's row."
  (let ((layers (test-stack-canvas--two-layers)))
    (test-stacked--with-drawn-stack preview layers nil nil
      (let* ((nodes (plist-get (shipit-stack-canvas--read preview) :nodes))
             (pos (plist-get (cdr (assoc "one" nodes)) :pos)))
        (should (integerp pos))
        (should (equal "one" (oref (magit-section-at pos) value)))))))

(ert-deftest test-stack-canvas-reads-each-layer-state ()
  "GIVEN a stack whose bottom layer has a pull request with failing
checks and whose top layer is only pushed
WHEN the canvas reads it
THEN each layer's state travels with the graph, by branch."
  (let ((layers (test-stack-canvas--two-layers)))
    (test-stacked--with-drawn-stack preview layers '(("one" . 101))
        '((101 . ((checks . "failure"))))
      (let* ((data (plist-get (shipit-stack-canvas--read preview) :data))
             (one (plist-get (cdr (assoc "one" data)) :state))
             (two (plist-get (cdr (assoc "two" data)) :state)))
        (should (equal 'pull-request (plist-get one :state)))
        (should (equal 101 (plist-get one :number)))
        (should (equal "failure" (plist-get one :checks)))
        (should (equal 'pushed (plist-get two :state)))))))

(ert-deftest test-stack-canvas-reads-the-statuses-the-rows-drew ()
  "GIVEN a stack drawn with pull request statuses from the forge
WHEN the canvas reads it
THEN the forge is not asked again.  The canvas reads the preview every
time the preview redraws, and each ask is a request to the forge."
  (let ((layers (test-stack-canvas--two-layers))
        (asked 0))
    (test-stacked--with-drawn-stack preview layers '(("one" . 101))
        (progn (cl-incf asked) '((101 . ((checks . "pending")))))
      (should (equal 1 asked))
      (let ((data (plist-get (shipit-stack-canvas--read preview) :data)))
        (should (equal "pending" (plist-get (plist-get (cdr (assoc "one" data)) :state)
                                            :checks))))
      (should (equal 1 asked)))))

(ert-deftest test-stack-canvas-reads-nothing-from-another-buffer ()
  "GIVEN a buffer that is no stacked preview
WHEN the canvas reads it
THEN it reads nothing, which leaves the last drawing standing."
  (with-temp-buffer
    (should-not (shipit-stack-canvas--read (current-buffer)))))

(ert-deftest test-stack-canvas-cuts-a-long-subject-on-its-box ()
  "GIVEN a commit subject longer than a box row
WHEN it becomes a row
THEN it is cut to the row width and ends in an ellipsis.  A box is as
wide as its widest row."
  (let* ((shipit-stack-canvas-row-width 50)
         (row (shipit-stack-canvas--row (concat "fpga: " (make-string 66 ?x)))))
    (should (equal 50 (string-width row)))
    (should (string-suffix-p "…" row)))
  (let ((shipit-stack-canvas-row-width 50))
    (should (equal "uoe: abort path" (shipit-stack-canvas--row "uoe: abort path")))))

;;;; Colours and words

(ert-deftest test-stack-canvas-checks-decide-a-pull-request-colour ()
  "GIVEN pull requests whose checks pass, run or fail
WHEN their colour class is worked out
THEN a failing or running one is told apart from the rest, and a
state without a pull request is its own class."
  (should (eq 'checks-failing
              (shipit-stack-canvas--class '(:state pull-request :checks "failure"))))
  (should (eq 'checks-pending
              (shipit-stack-canvas--class '(:state pull-request :checks "pending"))))
  (should (eq 'pull-request
              (shipit-stack-canvas--class '(:state pull-request :checks "success"))))
  (should (eq 'pull-request (shipit-stack-canvas--class '(:state pull-request))))
  (should (eq 'drifted (shipit-stack-canvas--class '(:state drifted)))))

(ert-deftest test-stack-canvas-every-class-has-a-face-and-a-name ()
  "GIVEN the colour classes a layer can be in
WHEN a box is filled and the legend names it
THEN each class has a face to take its colour from and a name."
  (dolist (class '(sending no-branch not-pushed pushed drifted
                           pull-request checks-pending checks-failing))
    (should (facep (alist-get class shipit-stack-canvas-state-faces)))
    (should (alist-get class shipit-stack-canvas--class-names))))

(ert-deftest test-stack-canvas-says-a-state-in-words ()
  "GIVEN layers in different states
WHEN their state is put in words for the header and the card
THEN the words say the stage, the pull request with its checks and
review, and how far the remote copy has drifted."
  (should (equal "PR #12 · checks failure · changes requested · 1 ahead"
                 (shipit-stack-canvas--state-words
                  '(:state pull-request :number 12 :checks "failure"
                           :review "changes_requested" :drift "1 ahead"))))
  (should (equal "pushed · 2 ahead, 1 behind"
                 (shipit-stack-canvas--state-words
                  '(:state drifted :drift "2 ahead, 1 behind"))))
  (should (equal "no branch yet"
                 (shipit-stack-canvas--state-words '(:state no-branch)))))

(ert-deftest test-stack-canvas-fills-a-box-towards-its-state-colour ()
  "GIVEN a white background, a pushed layer and a layer whose checks fail
WHEN their boxes are filled
THEN each fill goes the configured part of the way from the background
to its state's colour, the two differ, and the base keeps the plain box
colour."
  (skip-unless (shipit-canvas-available-p 'canvas-graph))
  (let* ((canvas-diagram-colors '(:background "#ffffff"))
         (shipit-stack-canvas-fill-strength 0.5)
         (shipit-stack-canvas-state-faces '((pushed . test-stack-canvas--green)
                                            (checks-failing . test-stack-canvas--red)))
         (diagram (test-stack-canvas--built
                   (shipit-stack-canvas--spec
                    (list (test-stack-canvas--seen
                           "one" '(:state pull-request :number 1 :checks "failure") "a")
                          (test-stack-canvas--seen "two" '(:state pushed) "b"))
                    "master"))))
    (should (equal '(1.0 0.5 0.5)
                   (shipit-stack-canvas--node-rgb
                    diagram (test-stack-canvas--node diagram "one"))))
    (should (equal '(0.5 1.0 0.5)
                   (shipit-stack-canvas--node-rgb
                    diagram (test-stack-canvas--node diagram "two"))))
    (should-not (shipit-stack-canvas--node-rgb
                 diagram (test-stack-canvas--node diagram "master")))))

(ert-deftest test-stack-canvas-legend-names-the-classes-in-use ()
  "GIVEN a stack with a pushed layer and a layer with a pull request
WHEN the legend is drawn
THEN it names those two classes and no others, with filled swatches."
  (skip-unless (shipit-canvas-available-p 'canvas-graph))
  (let* ((diagram (test-stack-canvas--built
                   (shipit-stack-canvas--spec
                    (list (test-stack-canvas--seen "one" '(:state pushed) "a")
                          (test-stack-canvas--seen
                           "two" '(:state pull-request :number 3) "b"))
                    "master")))
         (legend (shipit-stack-canvas--legend diagram)))
    (should (equal '("pushed" "pull request") (mapcar #'car legend)))
    (should (equal '(fill fill) (mapcar #'caddr legend)))))

(ert-deftest test-stack-canvas-header-and-card-say-the-state ()
  "GIVEN a layer with a pull request and a commit subject longer than a row
WHEN its header and its card are shown
THEN both say the state in words, and the card holds the subject whole."
  (skip-unless (shipit-canvas-available-p 'canvas-graph))
  (let* ((subject (concat "fpga: " (make-string 66 ?x)))
         (diagram (test-stack-canvas--built
                   (shipit-stack-canvas--spec
                    (list (test-stack-canvas--seen
                           "one" '(:state pull-request :number 3) subject))
                    "master")))
         (node (test-stack-canvas--node diagram "one")))
    (should (string-match-p "PR #3" (shipit-stack-canvas--header diagram node)))
    (should-not (member subject (canvas-diagram-node-rows node)))
    (let ((body (nth 2 (shipit-stack-canvas--card diagram node))))
      (should (string-match-p "PR #3" body))
      (should (string-match-p (regexp-quote subject) body)))))

;;;; The buffer and its keys

(ert-deftest test-stack-canvas-follows-its-preview ()
  "GIVEN a stack drawn in the preview
WHEN its canvas is opened
THEN the canvas is named after the top layer and draws a box for every
layer and the base."
  (let ((layers (test-stack-canvas--two-layers)))
    (test-stack-canvas--with-canvas canvas preview layers nil nil
      (should (equal "*shipit-stack: two*" (buffer-name canvas)))
      (should (derived-mode-p 'shipit-stack-canvas-mode))
      (should (equal '("two" "one" "master")
                     (mapcar #'canvas-diagram-node-label (canvas-diagram-nodes-shown)))))))

(ert-deftest test-stack-canvas-draws-again-when-the-preview-redraws ()
  "GIVEN a canvas following a preview of one layer
WHEN the preview is divided into two layers and redraws
THEN the canvas reads it again and draws both.  The canvas reads the
preview once Emacs is idle, which a batch run never is, so the test
runs that read itself."
  (let ((layers (list (test-stack-canvas--layer "one" "a" "b"))))
    (test-stack-canvas--with-canvas canvas preview layers nil nil
      (with-current-buffer preview
        (setq layers (list (test-stack-canvas--layer "one" "a")
                           (test-stack-canvas--layer "two" "b")))
        (shipit-stacked-preview--render))
      (should canvas-diagram--source-timer)
      (canvas-diagram--refresh-from-source canvas)
      (should (canvas-graph-node-named (canvas-diagram-current-model) "two")))))

(ert-deftest test-stack-canvas-ret-visits-the-layer ()
  "GIVEN the keyboard on a layer with a pull request
WHEN RET is pressed
THEN the preview's visit runs in the preview for that layer, and opens
its pull request."
  (let ((layers (test-stack-canvas--two-layers))
        (visited nil))
    (test-stack-canvas--with-canvas canvas preview layers '(("one" . 101)) nil
      (cl-letf (((symbol-function 'shipit-open-pr-buffer)
                 (lambda (number repo &rest _)
                   (setq visited (list number repo (current-buffer))))))
        (test-stack-canvas--select "one")
        (test-stack-canvas--press (kbd "RET"))
        (should (equal (list 101 "owner/repo" preview) visited))))))

(ert-deftest test-stack-canvas-g-reads-the-stack-again ()
  "GIVEN a canvas on a preview
WHEN g is pressed after the commits changed
THEN the preview reads its commits again and the canvas draws what the
preview then holds, at once."
  (let ((layers (test-stack-canvas--two-layers))
        (read-commits nil))
    (test-stack-canvas--with-canvas canvas preview layers nil nil
      (cl-letf (((symbol-function 'shipit-stack--base-ref) (lambda () "origin/master"))
                ((symbol-function 'shipit--commits-in-range)
                 (lambda (&rest _) (setq read-commits (current-buffer)) nil)))
        (setq layers (append layers (list (test-stack-canvas--layer "three" "c"))))
        (test-stack-canvas--press (kbd "g"))
        (should (eq preview read-commits))
        (should (canvas-graph-node-named (canvas-diagram-current-model) "three"))))))

(ert-deftest test-stack-canvas-p-opens-the-push-menu-on-the-layer ()
  "GIVEN the keyboard on the bottom layer
WHEN P is pressed
THEN the preview's push menu opens for that preview and that layer, so
its suffixes push the layer wherever the menu was opened from."
  (let ((layers (test-stack-canvas--two-layers))
        (scope nil))
    (test-stack-canvas--with-canvas canvas preview layers nil nil
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (&rest args) (setq scope (plist-get (nthcdr 3 args) :scope)))))
        (test-stack-canvas--select "one")
        (test-stack-canvas--press (kbd "P"))
        (should (equal (cons preview "one") scope))))))

(ert-deftest test-stack-canvas-keys-on-the-base-say-it-is-no-layer ()
  "GIVEN the keyboard on the base
WHEN a key that acts on a layer is pressed
THEN it says the base is no layer, rather than acting on a branch the
stack only sits on."
  (let ((layers (test-stack-canvas--two-layers)))
    (test-stack-canvas--with-canvas canvas preview layers nil nil
      (test-stack-canvas--select "master")
      (dolist (command '(shipit-stack-canvas-visit-layer shipit-stack-canvas-push))
        (let ((err (should-error (funcall command) :type 'user-error)))
          (should (string-match-p "base" (cadr err))))))))

(ert-deftest test-stack-canvas-keys-without-the-preview-say-so ()
  "GIVEN a canvas whose preview was killed
WHEN a key that needs the preview is pressed
THEN it says the preview is gone."
  (let ((layers (test-stack-canvas--two-layers)))
    (test-stack-canvas--with-canvas canvas preview layers nil nil
      (kill-buffer preview)
      (test-stack-canvas--select "one")
      (dolist (command '(shipit-stack-canvas-visit-layer shipit-stack-canvas-push
                                                         shipit-stack-canvas-refresh))
        (let ((err (should-error (funcall command) :type 'user-error)))
          (should (string-match-p "preview" (cadr err))))))))

(ert-deftest test-stack-canvas-v-in-the-preview-opens-the-canvas ()
  "GIVEN a stack drawn in the preview on an Emacs that has the canvas
WHEN v is pressed there
THEN the canvas opens on that stack."
  (skip-unless (shipit-canvas-available-p 'canvas-graph))
  (let ((layers (test-stack-canvas--two-layers)))
    (test-stacked--with-drawn-stack preview layers nil nil
      (unwind-protect
          (progn
            (test-stack-canvas--press (kbd "v"))
            (should (get-buffer "*shipit-stack: two*"))
            (with-current-buffer "*shipit-stack: two*"
              (should (derived-mode-p 'shipit-stack-canvas-mode))))
        (when (get-buffer "*shipit-stack: two*")
          (kill-buffer "*shipit-stack: two*"))))))

;;;; Export

(ert-deftest test-stack-canvas-exports-a-picture ()
  "GIVEN a stack drawn in the preview
WHEN it is exported
THEN a PNG is written with pixels in it, without a frame."
  (skip-unless (shipit-canvas-available-p 'canvas-graph))
  (let ((layers (test-stack-canvas--two-layers))
        (file (make-temp-file "test-stack-canvas-" nil ".png")))
    (unwind-protect
        (test-stacked--with-drawn-stack preview layers '(("one" . 101)) nil
          (shipit-stack-canvas-export preview file)
          (should (> (file-attribute-size (file-attributes file)) 0)))
      (delete-file file))))

(provide 'test-shipit-stack-canvas)
;;; test-shipit-stack-canvas.el ends here
