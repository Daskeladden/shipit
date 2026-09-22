;;; test-shipit-atlassian-kanban.el --- The board drawn on a canvas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The canvas kanban draws a Jira board as columns of cards.  The model
;; and the text wrapping are plain data work and run anywhere.  The
;; layout and the export need the Emacs 32 canvas and the canvas-cairo
;; module, so those tests skip themselves when either is missing, and
;; the suite still passes on an older Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-atlassian-kanban)
(require 'shipit-canvas)

;; Load canvas-diagram now, when this Emacs can: the layout macros below
;; set its struct slots, and `load' expands a macro as it reads the form,
;; which needs the struct known by then.  The kanban no longer loads it
;; as it loads, since that broke compiling shipit on an older Emacs.
(shipit-canvas-available-p)

(defvar test-kanban--board-override nil
  "A board for one test to use in place of the usual fixture.")

(defun test-kanban--board ()
  "A board whose column names differ from the statuses they accept."
  (or test-kanban--board-override
      '((id . 1)
    (name . "Alpha")
    (columns . (((name . "Open")
                 (statuses . ("Open" "Reopened"))
                 (issues . (((key . "PROJ-1") (title . "Fix the flaky login test")
                             (state . "Open") (issue-type . "Bug")
                             (priority . "High") (status-category . "new"))
                            ((key . "PROJ-2") (title . "Rename the widget")
                             (state . "Open") (issue-type . "Task")))))
                ((name . "Code Review")
                 (statuses . ("In Review"))
                 (issues . (((key . "PROJ-3") (title . "Ship the parser")
                             (state . "In Review") (issue-type . "Story")))))
                ((name . "Done")
                 (statuses . ("Done" "Closed"))
                 (issues . ())))))))

(defun test-kanban--assigned-board ()
  "A board whose issues are assigned to two different people."
  '((id . 1)
    (name . "Assigned")
    (columns . (((name . "Open")
                 (statuses . ("Open"))
                 (issues . (((key . "MINE-1") (title . "Fix the USB reset")
                             (state . "Open") (issue-type . "Bug")
                             (assignees . (((login . "Ronny") (id . "me-1")))))
                            ((key . "THEIRS-1") (title . "Rename the widget")
                             (state . "Open") (issue-type . "Task")
                             (assignees . (((login . "Ada") (id . "other")))))
                            ((key . "NOBODY-1") (title . "USB cable audit")
                             (state . "Open") (issue-type . "Task")))))))))

(defun test-kanban--wordy-board ()
  "A board whose summaries are long enough to fill a card and then some."
  '((id . 1)
    (name . "Wordy")
    (columns . (((name . "Open")
                 (statuses . ("Open"))
                 (issues . (((key . "WORD-1")
                             (title . "Recalibrate the stereo projector immediately after every firmware flash, including downgrades")
                             (state . "Open") (issue-type . "Bug"))
                            ((key . "WORD-2")
                             (title . "shipit--atlassian-dashboard-refresh-data misreports pagination")
                             (state . "Open") (issue-type . "Task")))))))))

(defun test-kanban--column-named (model name)
  "Return the column called NAME in MODEL."
  (cl-find name (shipit-atlassian-kanban-board-columns model)
           :key #'shipit-atlassian-kanban-column-name :test #'equal))

(defun test-kanban--keys-in (model name)
  "Return the issue keys of the column called NAME in MODEL."
  (mapcar #'shipit-atlassian-board--issue-key
          (shipit-atlassian-kanban-column-cards
           (test-kanban--column-named model name))))

;;;; The model

(ert-deftest test-kanban-build-groups-issues-by-column ()
  "GIVEN a board whose columns carry issues
WHEN the model is built
THEN each issue sits in the column it came from, in order."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec (test-kanban--board) nil))))
    (should (equal '("PROJ-1" "PROJ-2") (test-kanban--keys-in model "Open")))
    (should (equal '("PROJ-3") (test-kanban--keys-in model "Code Review")))
    (should (equal '() (test-kanban--keys-in model "Done")))))

(ert-deftest test-kanban-build-keeps-an-empty-column ()
  "GIVEN a board column with no issues
WHEN the model is built
THEN the column is still there, so it can be drawn and moved into."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec (test-kanban--board) nil))))
    (should (equal '("Open" "Code Review" "Done")
                   (mapcar #'shipit-atlassian-kanban-column-name
                           (shipit-atlassian-kanban-board-columns model))))))

(ert-deftest test-kanban-build-draws-a-staged-card-in-its-target ()
  "GIVEN a move staged for one card
WHEN the model is built
THEN that card sits in the target column and has left its own.

The staged target is a status name, and the column accepting it is
named differently, so the routing goes through the column's status
list rather than by matching names."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec
                     (test-kanban--board) '(("PROJ-1" . "In Review"))))))
    (should (equal '("PROJ-2") (test-kanban--keys-in model "Open")))
    (should (equal '("PROJ-1" "PROJ-3") (test-kanban--keys-in model "Code Review")))))

(ert-deftest test-kanban-build-marks-a-staged-card ()
  "GIVEN a move staged for one card
WHEN the model is built
THEN that card is marked with its target and the others are not,
     so the drawing can colour it and its card can say so."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec
                     (test-kanban--board) '(("PROJ-1" . "In Review"))))))
    (should (equal "In Review"
                   (plist-get (shipit-atlassian-kanban--card model "PROJ-1") :staged)))
    (should (null (plist-get (shipit-atlassian-kanban--card model "PROJ-2") :staged)))
    (should (string-match-p "Staged for: In Review"
                            (shipit-atlassian-kanban--note model "PROJ-1")))))

(ert-deftest test-kanban-build-leaves-an-unroutable-target-at-home ()
  "GIVEN a staged target no column on this board accepts
WHEN the model is built
THEN the card stays where it is, rather than vanishing from the board."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec
                     (test-kanban--board) '(("PROJ-1" . "Mythical"))))))
    (should (equal '("PROJ-1" "PROJ-2") (test-kanban--keys-in model "Open")))))

(ert-deftest test-kanban-build-carries-the-board-name ()
  "GIVEN a board with a name
WHEN the model is built
THEN the model carries it, for the buffer name and the card path."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec (test-kanban--board) nil))))
    (should (equal "Alpha" (shipit-atlassian-kanban-board-name model)))))

;;;; Wrapping a summary

(defun test-kanban--measure (text)
  "A stand-in for pango: seven pixels per character, on the widest line."
  (* 7 (apply #'max (mapcar #'length (split-string text "\n")))))

(ert-deftest test-kanban-wrap-breaks-between-words ()
  "GIVEN a summary wider than the card
WHEN it is wrapped
THEN it breaks between words, and no line is wider than the card."
  (let ((wrapped (shipit-atlassian-kanban--wrap
                  "Fix the flaky login test" #'test-kanban--measure 80)))
    (should (string-match-p "\n" wrapped))
    (should (equal "Fix the flaky login test"
                   (string-join (split-string wrapped "\n") " ")))
    (dolist (line (split-string wrapped "\n"))
      (should (<= (test-kanban--measure line) 80)))))

(ert-deftest test-kanban-wrap-leaves-a-short-summary-alone ()
  "GIVEN a summary that fits the card
WHEN it is wrapped
THEN it comes back unchanged, on one line."
  (should (equal "Ship it"
                 (shipit-atlassian-kanban--wrap
                  "Ship it" #'test-kanban--measure 80))))

(ert-deftest test-kanban-wrap-breaks-a-long-word-at-its-separators ()
  "GIVEN a word wider than the card, with hyphens in it
WHEN it is wrapped
THEN it breaks after a hyphen rather than mid-word, and every piece fits.

A Jira summary often carries a test name or an identifier.  Keeping
such a word whole would widen the column it sits in, and a column of a
hundred cards is not worth widening for one of them."
  (let* ((word "test-upgrade-carbon-camera-while-cutting-its-power")
         (lines (split-string
                 (shipit-atlassian-kanban--wrap word #'test-kanban--measure 140)
                 "\n")))
    (should (< 1 (length lines)))
    (dolist (line lines)
      (should (<= (test-kanban--measure line) 140)))
    (dolist (line (butlast lines))
      (should (string-suffix-p "-" line)))
    (should (equal word (string-join lines "")))))

(ert-deftest test-kanban-wrap-splits-a-word-with-nothing-to-break-at ()
  "GIVEN a long word with no separator in it
WHEN it is wrapped
THEN it is split where it stops fitting, rather than reaching past the card."
  (let ((lines (split-string (shipit-atlassian-kanban--wrap
                              (make-string 40 ?x) #'test-kanban--measure 70)
                             "\n")))
    (should (< 1 (length lines)))
    (should (equal (make-string 40 ?x) (string-join lines "")))
    (dolist (line lines)
      (should (<= (test-kanban--measure line) 70)))))

;;;; Availability

(ert-deftest test-kanban-unavailable-without-the-canvas ()
  "GIVEN an Emacs without the canvas image type
WHEN the kanban is asked whether it can draw
THEN it says no, so the command can send the reader to the dashboard."
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil)))
    (should-not (shipit-canvas-available-p))))

(ert-deftest test-kanban-command-refuses-without-the-canvas ()
  "GIVEN an Emacs without the canvas image type
WHEN the kanban command runs
THEN it names Emacs 32 and the dashboard instead of failing obscurely."
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil)))
    (let ((raised (condition-case err
                      (shipit-atlassian-kanban)
                    (error (error-message-string err)))))
      (should (string-match-p "Emacs 32" raised))
      (should (string-match-p "shipit-atlassian-dashboard" raised)))))

;;;; The layout
;;
;; These need the Emacs 32 canvas and the canvas-cairo module, so they
;; skip themselves without one.  Each runs with a staged move and
;; without, because a staged move changes which column a card lands in.

(defun test-kanban--canvas ()
  "A canvas spec to measure and draw on."
  (list 'image :type 'canvas :id (make-symbol "test-kanban")
        :data-width 1200 :data-height 800))

(defmacro test-kanban--with-spec-layout (spec nodes model &rest body)
  "Lay the board of SPEC out; run BODY.
NODES is bound to the laid-out nodes in reading order and MODEL to the
board model, and BODY sees the `diagram' and the `ctx' it was laid out
on.  Skips when this Emacs cannot draw a canvas."
  (declare (indent 3))
  `(progn
     (skip-unless (shipit-canvas-available-p))
     (let* ((diagram (shipit-atlassian-kanban--diagram))
            (spec ,spec)
            (ctx (canvas-cairo-context (test-kanban--canvas))))
       (unwind-protect
           (let* ((,model (shipit-atlassian-kanban--build diagram spec))
                  (,nodes (progn
                            (setf (canvas-diagram-spec diagram) spec
                                  (canvas-diagram-model diagram) ,model)
                            (shipit-atlassian-kanban--lay-out diagram ctx))))
             (setf (canvas-diagram-nodes diagram) ,nodes)
             ,@body)
         (canvas-cairo-destroy ctx)))))

(defmacro test-kanban--with-layout (nodes model pending collapsed &rest body)
  "Lay the fixture board out with PENDING staged and COLLAPSED shut; run BODY.
NODES, MODEL, `diagram' and `ctx' are as in `test-kanban--with-spec-layout'."
  (declare (indent 4))
  `(test-kanban--with-spec-layout
       (shipit-atlassian-kanban--spec (test-kanban--board) ,pending ,collapsed)
       ,nodes ,model
     ,@body))

(defun test-kanban--node (nodes key)
  "The node in NODES drawn for issue KEY or for the column called KEY."
  (cl-find key nodes :key #'canvas-diagram-node-label :test #'equal))

(defun test-kanban--cards (model nodes)
  "The nodes in NODES that draw a card, not a column header."
  (seq-remove (lambda (n) (shipit-atlassian-kanban--header-of model n)) nodes))

(defun test-kanban--headers (model nodes)
  "The nodes in NODES that draw a column header."
  (seq-filter (lambda (n) (shipit-atlassian-kanban--header-of model n)) nodes))

(defun test-kanban--left-edges (nodes keys)
  "The left edge of each node in NODES named by KEYS."
  (mapcar (lambda (k) (canvas-diagram-node-x (test-kanban--node nodes k))) keys))

(ert-deftest test-kanban-layout-puts-columns-left-to-right ()
  "GIVEN a board with three columns
WHEN it is laid out
THEN each column sits to the right of the one before it, in configured order."
  (test-kanban--with-layout nodes _model nil nil
    (pcase-let ((`(,open ,review) (test-kanban--left-edges nodes '("PROJ-1" "PROJ-3"))))
      (should (< open review)))))

(ert-deftest test-kanban-layout-shares-one-width-in-a-column ()
  "GIVEN two cards in the same column with summaries of different length
WHEN the board is laid out
THEN both cards are the same width, so the column reads as a column."
  (test-kanban--with-layout nodes _model nil nil
    (should (equal (canvas-diagram-node-w (test-kanban--node nodes "PROJ-1"))
                   (canvas-diagram-node-w (test-kanban--node nodes "PROJ-2"))))))

(ert-deftest test-kanban-layout-stacks-cards-without-overlap ()
  "GIVEN two cards in the same column
WHEN the board is laid out
THEN the second starts below the bottom of the first."
  (test-kanban--with-layout nodes _model nil nil
    (let ((first (test-kanban--node nodes "PROJ-1"))
          (second (test-kanban--node nodes "PROJ-2")))
      (should (>= (canvas-diagram-node-y second)
                  (+ (canvas-diagram-node-y first)
                     (canvas-diagram-node-h first)))))))

(ert-deftest test-kanban-layout-leaves-room-for-the-header ()
  "GIVEN a column header drawn above the cards
WHEN the board is laid out
THEN every card starts below the bottom of its column header."
  (test-kanban--with-layout nodes model nil nil
    (let ((header (car (test-kanban--headers model nodes))))
      (dolist (node (test-kanban--cards model nodes))
        (should (>= (canvas-diagram-node-y node)
                    (+ (canvas-diagram-node-y header)
                       (canvas-diagram-node-h header))))))))

(ert-deftest test-kanban-layout-draws-a-staged-card-in-its-target-column ()
  "GIVEN a move staged from the first column to the second
WHEN the board is laid out
THEN the staged card shares the left edge of the target column's card."
  (test-kanban--with-layout nodes _model '(("PROJ-1" . "In Review")) nil
    (pcase-let ((`(,staged ,native) (test-kanban--left-edges nodes '("PROJ-1" "PROJ-3"))))
      (should (equal staged native)))))

(ert-deftest test-kanban-layout-orders-nodes-column-by-column ()
  "GIVEN the laid-out nodes
WHEN they are read in order
THEN each column's header comes first, then its cards, then the next
     column."
  (test-kanban--with-layout nodes _model nil nil
    (should (equal '("Open" "PROJ-1" "PROJ-2" "Code Review" "PROJ-3" "Done")
                   (mapcar #'canvas-diagram-node-label nodes)))))

(ert-deftest test-kanban-layout-keeps-the-text-inside-the-card ()
  "GIVEN summaries longer than a card is wide
WHEN the board is laid out
THEN what canvas-diagram will draw fits inside the card it draws it in.

This is the invariant the card width exists for.  It broke once
because an icon is sized to the node's height, so a card of three
lines reserved three lines of width for its icon and pushed the text
out of the box.

Every icon lookup answers here, the way it does on a machine whose
svg-lib has the icons cached.  In batch none are on disk, so without
this the card that overflowed measured as if it fit."
  (let ((test-kanban--board-override (test-kanban--wordy-board)))
    (cl-letf (((symbol-function 'canvas-diagram-icon-data)
               (lambda (_name) "<svg/>")))
      (test-kanban--with-layout nodes model nil nil
      (let ((measure (canvas-diagram-measure ctx)))
        (dolist (node nodes)
            (should (<= (car (canvas-diagram-box-size
                              node
                              (shipit-atlassian-kanban--node-text diagram node)
                              measure))
                        (canvas-diagram-node-w node)))))))))

(ert-deftest test-kanban-layout-holds-its-width-for-a-long-word ()
  "GIVEN a summary carrying an identifier wider than the card
WHEN the board is laid out
THEN the column keeps the configured width, because the word breaks
     instead of the column growing.

One such summary among a hundred used to widen the whole column."
  (let ((test-kanban--board-override (test-kanban--wordy-board)))
    (test-kanban--with-layout nodes model nil nil
      (dolist (node (test-kanban--cards model nodes))
        (should (equal shipit-atlassian-kanban-card-width
                       (canvas-diagram-node-w node)))))))

(ert-deftest test-kanban-layout-keeps-the-default-width-when-it-fits ()
  "GIVEN summaries that fit the card
WHEN the board is laid out
THEN the column stays at the configured width, so columns line up."
  (test-kanban--with-layout nodes model nil nil
    (dolist (node (test-kanban--cards model nodes))
      (should (equal shipit-atlassian-kanban-card-width
                     (canvas-diagram-node-w node))))))

;;;; Moving between the cards

(defun test-kanban--move (nodes model key direction)
  "The issue key one DIRECTION from KEY, or nil."
  (let* ((diagram (canvas-diagram-create
                   :callbacks shipit-atlassian-kanban--callbacks
                   :model model :nodes nodes))
         (node (shipit-atlassian-kanban--move
                diagram (test-kanban--node nodes key) direction)))
    (and node (canvas-diagram-node-label node))))

(ert-deftest test-kanban-move-steps-across-columns ()
  "GIVEN a card in the first column
WHEN the keyboard moves in and out
THEN it lands in the next column and comes back."
  (test-kanban--with-layout nodes model nil nil
    (should (equal "PROJ-3" (test-kanban--move nodes model "PROJ-1" 'in)))
    (should (equal "PROJ-1" (test-kanban--move nodes model "PROJ-3" 'out)))))

(ert-deftest test-kanban-move-holds-at-the-outer-columns ()
  "GIVEN a card in the first column and the header of the last
WHEN the keyboard moves past either end
THEN it holds, rather than wrapping around."
  (test-kanban--with-layout nodes model nil nil
    (should (null (test-kanban--move nodes model "PROJ-1" 'out)))
    (should (null (test-kanban--move nodes model "Done" 'in)))))

(ert-deftest test-kanban-move-steps-within-a-column ()
  "GIVEN two cards in one column
WHEN the keyboard moves next and previous
THEN it walks them, reaches the header above them, and holds below."
  (test-kanban--with-layout nodes model nil nil
    (should (equal "PROJ-2" (test-kanban--move nodes model "PROJ-1" 'next)))
    (should (equal "PROJ-1" (test-kanban--move nodes model "PROJ-2" 'previous)))
    (should (equal "Open" (test-kanban--move nodes model "PROJ-1" 'previous)))
    (should (equal "PROJ-1" (test-kanban--move nodes model "Open" 'next)))
    (should (null (test-kanban--move nodes model "PROJ-2" 'next)))
    (should (null (test-kanban--move nodes model "Open" 'previous)))))

(ert-deftest test-kanban-move-into-a-shorter-column-holds-the-row ()
  "GIVEN a second row in the first column and one row in the next
WHEN the keyboard moves in from the second row
THEN it lands on the last card of that column rather than nowhere."
  (test-kanban--with-layout nodes model nil nil
    (should (equal "PROJ-3" (test-kanban--move nodes model "PROJ-2" 'in)))))

(ert-deftest test-kanban-move-into-an-empty-column-lands-on-its-header ()
  "GIVEN a last column with no cards
WHEN the keyboard moves in from the column before it
THEN it lands on that column's header, which is the only thing there."
  (test-kanban--with-layout nodes model nil nil
    (should (equal "Done" (test-kanban--move nodes model "PROJ-3" 'in)))))

;;;; Export

(ert-deftest test-kanban-exports-a-picture ()
  "GIVEN a board
WHEN it is exported
THEN a PNG is written with pixels in it, without a frame or a buffer."
  (skip-unless (shipit-canvas-available-p))
  (let ((file (make-temp-file "test-kanban-" nil ".png")))
    (unwind-protect
        (progn
          (shipit-atlassian-kanban-export (test-kanban--board) file)
          (should (file-exists-p file))
          (should (> (file-attribute-size (file-attributes file)) 0)))
      (delete-file file))))


;;;; Loading without canvas-diagram
;;
;; The promise this file makes is that shipit loads on any Emacs.  An
;; `:include' of `canvas-diagram-node', or a keymap parented on
;; `canvas-diagram-mode-map' at load time, would quietly break it, and
;; only a fresh Emacs without canvas-diagram on its load path notices.

(defconst test-kanban--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(defun test-kanban--load-in-a-bare-emacs ()
  "Load the kanban in a subprocess that cannot see canvas-diagram.
Returns what that Emacs printed, which is empty when the load worked."
  (with-temp-buffer
    (call-process
     (expand-file-name invocation-name invocation-directory)
     nil t nil
     "-batch" "-Q"
     "-L" (expand-file-name "lisp" test-kanban--root)
     "--eval" (prin1-to-string
               `(setq package-user-dir
                      ,(expand-file-name ".packages" test-kanban--root)))
     "--eval" "(package-initialize)"
     "--eval" "(require 'shipit-atlassian-kanban)"
     "--eval" "(princ (if (featurep 'canvas-diagram) \"canvas-diagram leaked in\" \"\"))")
    (buffer-string)))

(ert-deftest test-kanban-loads-without-canvas-diagram ()
  "GIVEN an Emacs that cannot see canvas-diagram
WHEN the kanban file is loaded
THEN it loads, so requiring shipit never depends on the canvas."
  (should (equal "" (test-kanban--load-in-a-bare-emacs))))

;;;; The buffer and its keys

(defmacro test-kanban--with-buffer (buffer pending &rest body)
  "Open the fixture board in a kanban buffer bound to BUFFER; run BODY.
PENDING is staged first.  The repository's issues are on Jira, as those
of a board are.  Skips when this Emacs cannot draw a canvas."
  (declare (indent 2))
  `(progn
     (skip-unless (shipit-canvas-available-p))
     (let ((,buffer (let ((shipit-issue-repo-backends
                           '(("owner/repo" :backend jira :base-url "https://x"))))
                      (shipit-atlassian-kanban--show
                       (test-kanban--board) "owner/repo" '(:base-url "https://x") 1))))
       (unwind-protect
           (with-current-buffer ,buffer
             (when ,pending
               (setq shipit-atlassian-kanban--pending ,pending)
               (shipit-atlassian-kanban--redraw))
             ,@body)
         (kill-buffer ,buffer)))))

(defun test-kanban--select (key)
  "Put the keyboard on the card for KEY."
  (canvas-diagram-set-selected
   (test-kanban--node (canvas-diagram-nodes-shown) key)))

(ert-deftest test-kanban-buffer-names-the-repo-and-the-board ()
  "GIVEN a board opened for a repository
WHEN the buffer is named
THEN it carries both, so two boards can be open at once."
  (should (equal "*shipit-kanban: owner/repo Alpha*"
                 (shipit-atlassian-kanban--buffer-name
                  "owner/repo" (test-kanban--board)))))

(ert-deftest test-kanban-stage-forward-moves-the-card ()
  "GIVEN the keyboard on a card in the first column
WHEN a move to the next column is staged
THEN the card is staged for that column's status and draws there."
  (test-kanban--with-buffer buffer nil
    (test-kanban--select "PROJ-1")
    (shipit-atlassian-kanban-stage-forward)
    (should (equal '(("PROJ-1" . "In Review")) shipit-atlassian-kanban--pending))
    (should (equal '("PROJ-1" "PROJ-3")
                   (test-kanban--keys-in (canvas-diagram-current-model) "Code Review")))))

(ert-deftest test-kanban-stage-twice-skips-two-columns ()
  "GIVEN a card already staged one column on
WHEN a further move is staged
THEN the staged target moves one more column, rather than standing still."
  (test-kanban--with-buffer buffer nil
    (test-kanban--select "PROJ-1")
    (shipit-atlassian-kanban-stage-forward)
    (test-kanban--select "PROJ-1")
    (shipit-atlassian-kanban-stage-forward)
    (should (equal '(("PROJ-1" . "Done")) shipit-atlassian-kanban--pending))))

(ert-deftest test-kanban-stage-back-to-the-start-cancels ()
  "GIVEN a card staged one column on
WHEN it is staged back to where it really is
THEN nothing is staged, because the moves cancelled out."
  (test-kanban--with-buffer buffer nil
    (test-kanban--select "PROJ-1")
    (shipit-atlassian-kanban-stage-forward)
    (test-kanban--select "PROJ-1")
    (shipit-atlassian-kanban-stage-backward)
    (should (null shipit-atlassian-kanban--pending))))

(ert-deftest test-kanban-stage-refuses-past-the-last-column ()
  "GIVEN the keyboard on a card in the last column that holds cards
WHEN a move past the end is staged
THEN it says there is no such column instead of staging nothing quietly."
  (test-kanban--with-buffer buffer '(("PROJ-3" . "Done"))
    (test-kanban--select "PROJ-3")
    (should-error (shipit-atlassian-kanban-stage-forward) :type 'user-error)))

(ert-deftest test-kanban-discard-puts-every-card-back ()
  "GIVEN two staged moves
WHEN they are discarded
THEN nothing is staged and each card draws in its own column again."
  (test-kanban--with-buffer buffer '(("PROJ-1" . "In Review") ("PROJ-2" . "Done"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (shipit-atlassian-kanban-discard))
    (should (null shipit-atlassian-kanban--pending))
    (should (equal '("PROJ-1" "PROJ-2")
                   (test-kanban--keys-in (canvas-diagram-current-model) "Open")))))

(ert-deftest test-kanban-commit-sends-each-move-and-refetches ()
  "GIVEN one staged move
WHEN it is committed
THEN the board layer walks it and the board is read again."
  (test-kanban--with-buffer buffer '(("PROJ-1" . "In Review"))
    (let ((walked nil)
          (refetched nil))
      (cl-letf (((symbol-function 'shipit-atlassian-board--commit-one)
                 (lambda (_boards _repo key target) (push (cons key target) walked) t))
                ((symbol-function 'shipit-atlassian-kanban-refresh)
                 (lambda () (setq refetched t))))
        (shipit-atlassian-kanban-commit))
      (should (equal '(("PROJ-1" . "In Review")) walked))
      (should refetched)
      (should (null shipit-atlassian-kanban--pending)))))

(ert-deftest test-kanban-commit-keeps-a-failed-move-staged ()
  "GIVEN a staged move Jira refuses
WHEN it is committed
THEN it stays staged, so it can be retried or discarded."
  (test-kanban--with-buffer buffer '(("PROJ-1" . "In Review"))
    (cl-letf (((symbol-function 'shipit-atlassian-board--commit-one)
               (lambda (&rest _) (error "No transition"))))
      (shipit-atlassian-kanban-commit))
    (should (equal '(("PROJ-1" . "In Review")) shipit-atlassian-kanban--pending))))

(ert-deftest test-kanban-mode-line-counts-the-staged-moves ()
  "GIVEN moves staged and then discarded
WHEN the mode line is read
THEN it counts them while they are there and says nothing when they are not.

Read straight off `mode-line-process\=': `format-mode-line\=' returns an
empty string in batch, where there is no window to format against."
  (test-kanban--with-buffer buffer '(("PROJ-1" . "In Review"))
    (should (string-match-p "1" mode-line-process))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (shipit-atlassian-kanban-discard))
    (should (null mode-line-process))))

;;;; Choosing a board

(ert-deftest test-kanban-refuses-without-a-configured-board ()
  "GIVEN no board identifiers configured
WHEN the kanban command runs
THEN it names the variable to set, before it goes near git or Jira."
  (skip-unless (shipit-canvas-available-p))
  (let ((shipit-atlassian-board-ids nil))
    (let ((raised (condition-case err
                      (shipit-atlassian-kanban)
                    (error (error-message-string err)))))
      (should (string-match-p "shipit-atlassian-board-ids" raised)))))

(ert-deftest test-kanban-opens-a-single-board-without-asking ()
  "GIVEN exactly one configured board
WHEN a board is picked
THEN it is opened straight away, with no prompt and no extra fetch."
  (let ((shipit-atlassian-board-ids '(7))
        (asked nil)
        (chosen nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (setq asked t) ""))
              ((symbol-function 'shipit-atlassian-board--fetch-names-async)
               (lambda (&rest _) (setq asked t))))
      (shipit-atlassian-kanban--pick-board nil (lambda (id) (setq chosen id))))
    (should (equal 7 chosen))
    (should-not asked)))

(ert-deftest test-kanban-asks-which-of-several-boards ()
  "GIVEN several configured boards
WHEN a board is picked
THEN their names are fetched and the prompt offers each by name."
  (let ((shipit-atlassian-board-ids '(7 9))
        (offered nil)
        (chosen nil))
    (cl-letf (((symbol-function 'shipit-atlassian-board--fetch-names-async)
               (lambda (_config _ids callback)
                 (funcall callback '((7 . "Alpha") (9 . "Beta")))))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (setq offered (mapcar #'car candidates))
                 "Beta (9)")))
      (shipit-atlassian-kanban--pick-board nil (lambda (id) (setq chosen id))))
    (should (equal '("Alpha (7)" "Beta (9)") offered))
    (should (equal 9 chosen))))

;;;; The priority of a card

(defun test-kanban--high-is-red (name)
  "A tracker's priority colour: red for High, and nothing for the rest."
  (and (equal name "High") "#cb2431"))

(defun test-kanban--red-tracker ()
  "A tracker that colours the High priority red, and nothing else."
  (list :priority-color #'test-kanban--high-is-red))

(defmacro test-kanban--with-marks (board mark &rest body)
  "Lay BOARD out with priorities coloured by `test-kanban--red-tracker'.
Run BODY with MARK bound to a function of an issue key that gives the
priority mark of its card, or nil."
  (declare (indent 2))
  (let ((nodes (make-symbol "nodes"))
        (model (make-symbol "model")))
    `(test-kanban--with-spec-layout
         (shipit-atlassian-kanban--spec ,board nil nil nil nil (test-kanban--red-tracker))
         ,nodes ,model
       (let ((,mark (lambda (key)
                      (shipit-atlassian-kanban--priority-mark
                       ,model (test-kanban--node ,nodes key) ctx))))
         ,@body))))

(defun test-kanban--node-of (board key)
  "Return the laid-out node KEY of BOARD, for a geometry check."
  (let* ((diagram (shipit-atlassian-kanban--diagram))
         (spec (shipit-atlassian-kanban--spec board nil))
         (ctx (canvas-cairo-context (test-kanban--canvas))))
    (unwind-protect
        (progn
          (setf (canvas-diagram-spec diagram) spec
                (canvas-diagram-model diagram) (shipit-atlassian-kanban--build diagram spec))
          (test-kanban--node (shipit-atlassian-kanban--lay-out diagram ctx) key))
      (canvas-cairo-destroy ctx))))

(ert-deftest test-kanban-card-shows-its-priority-at-the-top-right ()
  "GIVEN a card of High priority, and a tracker that colours High red
WHEN its priority mark is asked for
THEN it is a pill saying High, in red, at the right of the card's first
line."
  (test-kanban--with-marks (test-kanban--board) mark
    (let ((pill (funcall mark "PROJ-1"))
          (node (test-kanban--node-of (test-kanban--board) "PROJ-1")))
      (should (equal "High" (plist-get pill :text)))
      (should-not (plist-get pill :dot))
      (should (equal (canvas-diagram-rgb "#cb2431") (plist-get pill :rgb)))
      (should (<= (+ (plist-get pill :x) (plist-get pill :w))
                  (+ (canvas-diagram-node-x node) (canvas-diagram-node-w node))))
      (should (> (plist-get pill :x)
                 (+ (canvas-diagram-node-x node) (/ (canvas-diagram-node-w node) 2))))
      (should (< (plist-get pill :y)
                 (+ (canvas-diagram-node-y node) (/ (canvas-diagram-node-h node) 2)))))))

(ert-deftest test-kanban-card-without-priority-has-no-mark ()
  "GIVEN a card with no priority
WHEN its priority mark is asked for
THEN there is none."
  (test-kanban--with-marks (test-kanban--board) mark
    (should-not (funcall mark "PROJ-2"))))

(ert-deftest test-kanban-card-priority-the-tracker-cannot-colour-is-plain ()
  "GIVEN a card of a priority the tracker has no colour for
WHEN its priority mark is asked for
THEN it still says the priority, in the colour of the edges."
  (let ((test-kanban--board-override (copy-tree (test-kanban--board))))
    (setf (alist-get 'priority (car (cdr (assq 'issues (car (cdr (assq 'columns test-kanban--board-override)))))))
          "Blocker")
    (test-kanban--with-marks (test-kanban--board) mark
      (let ((pill (funcall mark "PROJ-1")))
        (should (equal "Blocker" (plist-get pill :text)))
        (should (equal (canvas-diagram-color :edge) (plist-get pill :rgb)))))))

(ert-deftest test-kanban-card-priority-is-a-dot-when-the-key-fills-the-line ()
  "GIVEN a card whose key takes most of its first line
WHEN its priority mark is asked for
THEN it is a dot in the priority's colour, so it covers no text."
  (let ((test-kanban--board-override
         '((id . 1) (name . "Long")
           (columns . (((name . "Open") (statuses . ("Open"))
                        (issues . (((key . "AVERYLONGPROJECTKEYINDEED-123456")
                                    (title . "Long key") (state . "Open")
                                    (priority . "High"))))))))))
    (test-kanban--with-marks (test-kanban--board) mark
      (let ((dot (funcall mark "AVERYLONGPROJECTKEYINDEED-123456")))
        (should (plist-get dot :dot))
        (should (equal (canvas-diagram-rgb "#cb2431") (plist-get dot :rgb)))))))

(ert-deftest test-kanban-board-takes-its-priority-colours-from-the-tracker ()
  "GIVEN a board of a repository whose issues are on Jira
WHEN its High card is drawn
THEN the colour of its priority is the one the Jira backend gives High."
  (test-kanban--with-buffer buffer nil
    (let ((pill (shipit-atlassian-kanban--priority-mark
                 (canvas-diagram-current-model)
                 (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-1")
                 (canvas-cairo-context (test-kanban--canvas)))))
      (should (equal (canvas-diagram-rgb
                      (funcall (plist-get (cdr (assq 'jira shipit-issue-backends)) :priority-color)
                               "High"))
                     (plist-get pill :rgb))))))

(ert-deftest test-kanban-draws-priority-marks ()
  "GIVEN a board with a card of High priority
WHEN it is drawn into a file
THEN the drawing completes, its priority mark included."
  (skip-unless (shipit-canvas-available-p))
  (let ((file (make-temp-file "kanban-priority-" nil ".png")))
    (unwind-protect
        (progn
          (canvas-diagram-export
           (shipit-atlassian-kanban--diagram)
           (shipit-atlassian-kanban--spec (test-kanban--board) nil nil nil nil
                                          (test-kanban--red-tracker))
           file)
          (should (> (file-attribute-size (file-attributes file)) 0)))
      (delete-file file))))

;;;; The type icon of a card
;;
;; A card starts its key line with the icon of its issue type, in the
;; colour the tracker gives that type.

(defvar canvas-diagram-icon-directory)
(defvar canvas-diagram--icons)

(defconst test-kanban--icon-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\"><rect width=\"16\" height=\"16\"/></svg>"
  "An icon: a square that fills its box.")

(defmacro test-kanban--with-type-icons (&rest body)
  "Run BODY where the icons Jira names for Bug, Task and Story are on disk.
The icons canvas-diagram knows already are set aside, so each lookup
reads the disk, and the session's are back afterwards."
  (declare (indent 0))
  `(let ((canvas-diagram-icon-directory (make-temp-file "kanban-icons" t))
         (canvas-diagram--icons (make-hash-table :test 'equal)))
     (dolist (name '("bug" "tasklist" "bookmark"))
       (with-temp-file (expand-file-name (format "octicons_%s.svg" name)
                                         canvas-diagram-icon-directory)
         (insert test-kanban--icon-svg)))
     (unwind-protect (progn ,@body)
       (delete-directory canvas-diagram-icon-directory t))))

(defun test-kanban--jira-tracker ()
  "The tracker of a board whose issues are on Jira: the Jira backend."
  (cdr (assq 'jira shipit-issue-backends)))

(defun test-kanban--key-indent (ctx text)
  "Pixels on CTX before the key, on the first line of the card TEXT."
  (let ((line (car (split-string text "\n")))
        (font (canvas-diagram-font)))
    (- (car (canvas-cairo-text-size ctx line font))
       (car (canvas-cairo-text-size ctx (string-trim-left line) font)))))

(ert-deftest test-kanban-card-shows-its-type-icon-before-its-key ()
  "GIVEN a board of a repository whose issues are on Jira, and the icon
Jira names for a Bug on disk
WHEN its Bug card is drawn
THEN the card's type icon is that icon in the colour Jira gives a Bug,
at the start of the key line, AND the key is written after it."
  (test-kanban--with-type-icons
    (test-kanban--with-buffer buffer nil
      (let* ((node (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-1"))
             (ctx (canvas-cairo-context (test-kanban--canvas)))
             (mark (shipit-atlassian-kanban--type-mark
                    (canvas-diagram-current-model) node ctx))
             (left (+ (canvas-diagram-node-x node) (shipit-atlassian-kanban--box-pad ctx))))
        (unwind-protect
            (progn
              (should (equal test-kanban--icon-svg (plist-get mark :svg)))
              (should (equal (canvas-diagram-rgb
                              (funcall (plist-get (test-kanban--jira-tracker) :issue-type-color)
                                       "Bug"))
                             (plist-get mark :rgb)))
              (should (= left (plist-get mark :x)))
              (should (<= (+ (plist-get mark :x) (plist-get mark :side))
                          (+ left (test-kanban--key-indent
                                   ctx (shipit-atlassian-kanban--node-text
                                        shipit-atlassian-kanban--drawing node))))))
          (canvas-cairo-destroy ctx))))))

(ert-deftest test-kanban-card-has-no-type-icon-the-tracker-does-not-name ()
  "GIVEN a board whose tracker names no icon for an issue type
WHEN it is laid out
THEN no card has a type icon, AND no key line leaves room for one."
  (test-kanban--with-type-icons
    (test-kanban--with-layout nodes model nil nil
      (dolist (node (test-kanban--cards model nodes))
        (should-not (shipit-atlassian-kanban--type-mark model node ctx))
        (should (= 0 (test-kanban--key-indent
                      ctx (shipit-atlassian-kanban--node-text diagram node))))))))

(ert-deftest test-kanban-draws-type-icons-in-their-colours ()
  "GIVEN a board whose issues are on Jira, with the icons of its types on disk
WHEN what goes over the cards is drawn
THEN the middle of the Bug card's type icon has the colour Jira gives a Bug."
  (test-kanban--with-type-icons
    (test-kanban--with-spec-layout
        (shipit-atlassian-kanban--spec (test-kanban--board) nil nil nil nil
                                       (test-kanban--jira-tracker))
        nodes model
      (let* ((mark (shipit-atlassian-kanban--type-mark
                    model (test-kanban--node nodes "PROJ-1") ctx))
             (half (/ (plist-get mark :side) 2)))
        (shipit-atlassian-kanban--draw-over diagram ctx)
        (should (= (logior #xff000000
                           (string-to-number
                            (substring (funcall (plist-get (test-kanban--jira-tracker)
                                                           :issue-type-color)
                                                "Bug")
                                       1)
                            16))
                   (canvas-cairo-pixel ctx
                                       (round (+ (plist-get mark :x) half))
                                       (round (+ (plist-get mark :y) half)))))))))

;;;; The menu

(ert-deftest test-kanban-has-a-menu ()
  "GIVEN a board on a canvas
WHEN the menu key is pressed
THEN there is a menu to open, rather than canvas-diagram saying there is none."
  (skip-unless (shipit-canvas-available-p))
  (should (eq 'shipit-atlassian-kanban-menu
              (plist-get shipit-atlassian-kanban--callbacks :menu)))
  (should (commandp 'shipit-atlassian-kanban-menu)))

(ert-deftest test-kanban-menu-keys-miss-the-shared-ones ()
  "GIVEN canvas-diagram's own groups, which come after the board's
WHEN a key is looked up in the menu
THEN each of canvas-diagram's keys still reaches a command of its own,
and none reaches one of the board's.

Read from the menu rather than from a list written by hand: the list
left out `W', and the board shadowed the key that writes the drawing
to a file for a while without anything noticing.  The commands are not
named here.  canvas-diagram moved several of them into canvas-keys, and
a list naming them failed although no key was shadowed."
  (skip-unless (shipit-canvas-available-p))
  (dolist (key '("W" "C" "q" "SPC" "z" "B" "s" "F" "P" "k" "l" "w" "i"))
    (let* ((suffix (transient-get-suffix 'shipit-atlassian-kanban-menu key))
           (command (plist-get (cdr suffix) :command)))
      (should suffix)
      (should (commandp command))
      (should-not (string-prefix-p "shipit-" (symbol-name command))))))

(ert-deftest test-kanban-menu-cycles-the-card-width ()
  "GIVEN the card width setting
WHEN it is cycled
THEN the width moves on to the next value in the list."
  (skip-unless (shipit-canvas-available-p))
  (should (commandp 'shipit-atlassian-kanban-cycle-card-width))
  (let ((shipit-atlassian-kanban-card-width 240))
    (cl-letf (((symbol-function 'canvas-diagram-relayout) #'ignore)
              ((symbol-function 'canvas-diagram-redraw) #'ignore))
      (ignore-errors (shipit-atlassian-kanban-cycle-card-width)))
    (should-not (equal 240 shipit-atlassian-kanban-card-width))))

(ert-deftest test-kanban-plain-cards-have-the-page-colour ()
  "GIVEN cards in two columns, neither staged nor moved
WHEN each is asked for its fill
THEN both take the colour of the page, and not a colour of their column,
     which its frame already shows."
  (test-kanban--with-layout nodes _model nil nil
    (dolist (key '("PROJ-1" "PROJ-3"))
      (should (equal (canvas-diagram-color :background)
                     (shipit-atlassian-kanban--node-rgb
                      diagram (test-kanban--node nodes key)))))))

(ert-deftest test-kanban-staged-card-has-its-own-colour ()
  "GIVEN a staged card and a card that is where Jira has it
WHEN each is asked for its fill
THEN the staged one takes the staged colour, and not the selection
     colour, which already rings the card the keyboard is on."
  (test-kanban--with-layout nodes _model '(("PROJ-1" . "In Review")) nil
    (let ((staged (shipit-atlassian-kanban--node-rgb
                   diagram (test-kanban--node nodes "PROJ-1")))
          (plain (shipit-atlassian-kanban--node-rgb
                  diagram (test-kanban--node nodes "PROJ-3"))))
      (should (equal (canvas-diagram-rgb shipit-atlassian-kanban-staged-color)
                     staged))
      (should-not (equal staged plain))
      (should-not (equal staged (canvas-diagram-color :selection))))))

(ert-deftest test-kanban-menu-cycles-the-staged-colour ()
  "GIVEN the staged colour setting
WHEN the menu cycles it
THEN the colour moves on to the next one offered."
  (skip-unless (shipit-canvas-available-p))
  (should (commandp 'shipit-atlassian-kanban-cycle-staged-color))
  (let ((shipit-atlassian-kanban-staged-color
         (car shipit-atlassian-kanban-staged-colors)))
    (cl-letf (((symbol-function 'canvas-diagram-relayout) #'ignore)
              ((symbol-function 'canvas-diagram-redraw) #'ignore))
      (ignore-errors (shipit-atlassian-kanban-cycle-staged-color)))
    (should (equal (cadr shipit-atlassian-kanban-staged-colors)
                   shipit-atlassian-kanban-staged-color))))

;;;; Narrowing the board

(defun test-kanban--filtered-keys (filter &optional me)
  "The keys the Open column shows under FILTER, as ME."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec
                     (test-kanban--assigned-board) nil nil (or me "me-1") filter))))
    (test-kanban--keys-in model "Open")))

(ert-deftest test-kanban-filter-mine-keeps-only-my-cards ()
  "GIVEN cards assigned to me, to somebody else and to nobody
WHEN the board is narrowed to mine
THEN only mine is left, matched on the account id rather than a name."
  (should (equal '("MINE-1") (test-kanban--filtered-keys '(:mine t))))
  (should (equal '() (test-kanban--filtered-keys '(:mine t) "someone-else"))))

(ert-deftest test-kanban-filter-by-type ()
  "GIVEN cards of two issue types
WHEN the board is narrowed to one type
THEN only the cards of that type are left."
  (should (equal '("MINE-1") (test-kanban--filtered-keys '(:type "Bug"))))
  (should (equal '("THEIRS-1" "NOBODY-1")
                 (test-kanban--filtered-keys '(:type "Task")))))

(ert-deftest test-kanban-filter-by-text-reads-key-and-summary ()
  "GIVEN a text to narrow by
WHEN the board is narrowed
THEN a card matches on its key or on its summary, whatever the case."
  (should (equal '("MINE-1" "NOBODY-1") (test-kanban--filtered-keys '(:text "usb"))))
  (should (equal '("THEIRS-1") (test-kanban--filtered-keys '(:text "theirs"))))
  (should (equal '() (test-kanban--filtered-keys '(:text "mythical")))))

(ert-deftest test-kanban-filters-combine ()
  "GIVEN more than one part of the filter set
WHEN the board is narrowed
THEN a card has to pass every part."
  (should (equal '("MINE-1") (test-kanban--filtered-keys '(:mine t :text "usb"))))
  (should (equal '() (test-kanban--filtered-keys '(:mine t :type "Task")))))

(ert-deftest test-kanban-header-says-how-much-is-hidden ()
  "GIVEN a narrowed board
WHEN a column header is read
THEN it says how many it shows over how many it holds, and says one
     number when nothing is hidden."
  (let ((narrowed (shipit-atlassian-kanban--build
                   nil (shipit-atlassian-kanban--spec
                        (test-kanban--assigned-board) nil nil "me-1" '(:mine t))))
        (whole (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec
                     (test-kanban--assigned-board) nil nil "me-1" nil))))
    (should (equal "1/3" (shipit-atlassian-kanban--header-count
                          (test-kanban--column-named narrowed "Open"))))
    (should (equal "3" (shipit-atlassian-kanban--header-count
                        (test-kanban--column-named whole "Open"))))))

(ert-deftest test-kanban-marks-my-own-card ()
  "GIVEN a card assigned to me and one assigned to somebody else
WHEN the card text is built
THEN mine carries the mark after its key and the other does not."
  (skip-unless (shipit-canvas-available-p))
  (let* ((model (shipit-atlassian-kanban--build
                 nil (shipit-atlassian-kanban--spec
                      (test-kanban--assigned-board) nil nil "me-1" nil)))
         (mine (plist-get (shipit-atlassian-kanban--card model "MINE-1") :issue))
         (theirs (plist-get (shipit-atlassian-kanban--card model "THEIRS-1") :issue)))
    (should (equal (concat "MINE-1 " shipit-atlassian-kanban-mine-mark)
                   (shipit-atlassian-kanban--key-line model "MINE-1" mine)))
    (should (equal "THEIRS-1"
                   (shipit-atlassian-kanban--key-line model "THEIRS-1" theirs)))))

(ert-deftest test-kanban-marks-nothing-without-knowing-who-i-am ()
  "GIVEN no account id for the current user
WHEN the card text is built
THEN no card is marked, rather than every card or none at random."
  (let* ((model (shipit-atlassian-kanban--build
                 nil (shipit-atlassian-kanban--spec
                      (test-kanban--assigned-board) nil nil nil nil)))
         (mine (plist-get (shipit-atlassian-kanban--card model "MINE-1") :issue)))
    (should (equal "MINE-1" (shipit-atlassian-kanban--key-line model "MINE-1" mine)))))

(ert-deftest test-kanban-search-keys-do-what-they-say ()
  "GIVEN two kinds of search key
WHEN each is pressed in a board
THEN the plain one finds a card and leaves the board alone, and
     consult-line's narrows the board as you type.

consult-line would otherwise search a canvas holding one character."
  (test-kanban--with-buffer buffer nil
    (should (eq 'shipit-atlassian-kanban-find-card
                (lookup-key shipit-atlassian-kanban-mode-map
                            (vector 'remap 'isearch-forward))))
    (should (eq 'shipit-atlassian-kanban-search
                (lookup-key shipit-atlassian-kanban-mode-map
                            (vector 'remap 'consult-line))))))

(ert-deftest test-kanban-find-groups-the-cards-by-column ()
  "GIVEN the cards to choose from
WHEN consult asks which group each belongs to
THEN it is the column the card sits in, and transforming gives the
     card back unchanged."
  (test-kanban--with-buffer buffer nil
    (let* ((candidates (shipit-atlassian-kanban--find-candidates))
           (group (shipit-atlassian-kanban--card-group candidates))
           (first (car (car candidates))))
      (should (equal "Open" (funcall group first nil)))
      (should (equal first (funcall group first t)))
      (should (equal "Code Review"
                     (funcall group (car (nth 2 candidates)) nil))))))

(ert-deftest test-kanban-search-narrows-as-you-type-and-lands-on-a-card ()
  "GIVEN the search reading a card
WHEN each keystroke is previewed
THEN the board narrows to what is typed, and picking a card lands on it."
  (test-kanban--with-buffer buffer nil
    (let ((drawn nil))
      (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
                 (lambda () t))
                ((symbol-function 'shipit-atlassian-kanban--minibuffer-input)
                 (lambda () "parser"))
                ((symbol-function 'consult--read)
                 (lambda (candidates &rest args)
                   (funcall (plist-get args :state) 'preview (car candidates))
                   (setq drawn (length (test-kanban--cards
                                        (canvas-diagram-current-model)
                                        (canvas-diagram-nodes-shown))))
                   "PROJ-3  Ship the parser")))
        (shipit-atlassian-kanban-search))
      (should (equal 1 drawn))
      (should (equal "parser" (plist-get shipit-atlassian-kanban--filter :text)))
      (should (equal "PROJ-3"
                     (canvas-diagram-node-label (canvas-diagram-selected)))))))

(ert-deftest test-kanban-search-puts-the-board-back-when-it-is-abandoned ()
  "GIVEN the search narrowed the board
WHEN it is left without a card
THEN the board and the keyboard are as they were."
  (test-kanban--with-buffer buffer nil
    (canvas-diagram-set-selected
     (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-2"))
    (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
               (lambda () t))
              ((symbol-function 'shipit-atlassian-kanban--minibuffer-input)
               (lambda () "parser"))
              ((symbol-function 'consult--read)
               (lambda (candidates &rest args)
                 (funcall (plist-get args :state) 'preview (car candidates))
                 (keyboard-quit))))
      (ignore-errors (shipit-atlassian-kanban-search)))
    (should (null (plist-get shipit-atlassian-kanban--filter :text)))
    (should (equal 3 (length (test-kanban--cards
                              (canvas-diagram-current-model)
                              (canvas-diagram-nodes-shown)))))
    (should (equal "PROJ-2"
                   (canvas-diagram-node-label (canvas-diagram-selected))))))

(ert-deftest test-kanban-search-drops-the-filter-to-reach-a-hidden-card ()
  "GIVEN consult matched a card more loosely than the board does
WHEN that card is picked
THEN the filter is dropped so the card is there to land on, rather than
     the pick doing nothing."
  (test-kanban--with-buffer buffer nil
    (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
               (lambda () t))
              ((symbol-function 'shipit-atlassian-kanban--minibuffer-input)
               (lambda () "parser"))
              ((symbol-function 'consult--read)
               (lambda (candidates &rest args)
                 (funcall (plist-get args :state) 'preview (car candidates))
                 "PROJ-1  Fix the flaky login test")))
      (shipit-atlassian-kanban-search))
    (should (equal "PROJ-1"
                   (canvas-diagram-node-label (canvas-diagram-selected))))
    (should (null (plist-get shipit-atlassian-kanban--filter :text)))))

(ert-deftest test-kanban-candidates-survive-the-board-being-drawn-again ()
  "GIVEN a card chosen from a list built before the board narrowed
WHEN the keyboard is sent to it
THEN it lands on the card drawn now, not on the node caught earlier.

Narrowing builds every node again, so a node caught before that is no
longer one of the drawn ones."
  (test-kanban--with-buffer buffer nil
    (let ((before (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-3")))
      (shipit-atlassian-kanban-filter-text "parser")
      (should (shipit-atlassian-kanban--go-to-key "PROJ-3"))
      (should-not (eq before (canvas-diagram-selected)))
      (should (memq (canvas-diagram-selected) (canvas-diagram-nodes-shown))))))

(ert-deftest test-kanban-grouping-works-away-from-the-board ()
  "GIVEN the function that says which column a card is in
WHEN it is called with another buffer current
THEN it still answers.

The completion framework calls it from the minibuffer, which holds no
diagram, and reading the model there signalled a wrong type."
  (test-kanban--with-buffer buffer nil
    (let ((group (shipit-atlassian-kanban--card-group
                  (shipit-atlassian-kanban--find-candidates)))
          (candidate "PROJ-1  Fix the flaky login test"))
      (with-temp-buffer
        (should (equal "Open" (funcall group candidate nil)))
        (should (equal candidate (funcall group candidate t)))))))

(ert-deftest test-kanban-preview-works-away-from-the-board ()
  "GIVEN the preview, which the completion framework may call anywhere
WHEN it runs with another buffer current
THEN it still moves the keyboard on the board."
  (test-kanban--with-buffer buffer nil
    (let ((preview (shipit-atlassian-kanban--preview
                    (shipit-atlassian-kanban--find-candidates))))
      (with-temp-buffer
        (funcall preview 'preview "PROJ-3  Ship the parser"))
      (should (equal "PROJ-3"
                     (canvas-diagram-node-label (canvas-diagram-selected)))))))

;;;; Focusing the board as you type

(ert-deftest test-kanban-focus-text-is-read-with-an-invert-prefix ()
  "GIVEN what somebody typed into the focus reader
WHEN it is read
THEN a leading bang reverses the match and the rest is the text."
  (should (equal '("usb" . nil)
                 (shipit-atlassian-kanban--parse-filter-text "usb")))
  (should (equal '("usb" . t)
                 (shipit-atlassian-kanban--parse-filter-text "! usb")))
  (should (equal '("" . nil)
                 (shipit-atlassian-kanban--parse-filter-text ""))))

(ert-deftest test-kanban-filter-can-be-inverted ()
  "GIVEN a reversed text filter
WHEN the board is narrowed
THEN the cards that do not match are the ones left."
  (should (equal '("THEIRS-1")
                 (test-kanban--filtered-keys '(:text "usb" :text-invert t))))
  (should (equal '("MINE-1" "NOBODY-1")
                 (test-kanban--filtered-keys '(:text "usb")))))

(ert-deftest test-kanban-focus-keys-are-the-consult-ones ()
  "GIVEN consult's line filters, which a canvas has no lines for
WHEN their keys are pressed in a board
THEN both narrow the board instead.

Keeping deletes what does not match, and nothing here is deletable, so
both keys do the one thing that has a meaning on a canvas."
  (test-kanban--with-buffer buffer nil
    (dolist (command '(consult-focus-lines consult-keep-lines))
      (should (eq 'shipit-atlassian-kanban-focus
                  (lookup-key shipit-atlassian-kanban-mode-map
                              (vector 'remap command)))))))

(ert-deftest test-kanban-focus-narrows-as-you-type ()
  "GIVEN consult reading the filter
WHEN each keystroke is previewed
THEN the board narrows on every one, and the last one stays."
  (test-kanban--with-buffer buffer nil
    (let ((seen nil))
      (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
                 (lambda () t))
                ((symbol-function 'consult--prompt)
                 (lambda (&rest args)
                   (let ((state (plist-get args :state)))
                     (dolist (input '("p" "pa" "parser"))
                       (funcall state 'preview input)
                       (push (length (test-kanban--cards
                                      (canvas-diagram-current-model)
                                      (canvas-diagram-nodes-shown)))
                             seen)))
                   "parser")))
        (shipit-atlassian-kanban-focus))
      (should (equal '(3 1 1) (nreverse seen)))
      (should (equal "parser" (plist-get shipit-atlassian-kanban--filter :text))))))

(ert-deftest test-kanban-focus-waits-for-the-typing-to-pause ()
  "GIVEN a board too big to lay out on every keystroke
WHEN the focus reader opens
THEN it is told to wait for a pause before it narrows.

Laying a large board out again costs tens of milliseconds, which would
drag behind the typing."
  (test-kanban--with-buffer buffer nil
    (let ((asked nil))
      (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
                 (lambda () t))
                ((symbol-function 'consult--prompt)
                 (lambda (&rest args) (setq asked args) "")))
        (shipit-atlassian-kanban-focus))
      (should (equal `(:debounce ,shipit-atlassian-kanban-focus-debounce any)
                     (plist-get asked :preview-key))))))

(ert-deftest test-kanban-focus-put-back-when-it-is-abandoned ()
  "GIVEN a filter already set and a preview that changed it
WHEN the reader is left without an answer
THEN the filter is the one it was before."
  (test-kanban--with-buffer buffer nil
    (shipit-atlassian-kanban-filter-text "parser")
    (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
               (lambda () t))
              ((symbol-function 'consult--prompt)
               (lambda (&rest args)
                 (funcall (plist-get args :state) 'preview "widget")
                 (keyboard-quit))))
      (ignore-errors (shipit-atlassian-kanban-focus)))
    (should (equal "parser" (plist-get shipit-atlassian-kanban--filter :text)))
    (should (equal '("PROJ-3")
                   (mapcar #'canvas-diagram-node-label
                           (test-kanban--cards (canvas-diagram-current-model)
                                               (canvas-diagram-nodes-shown)))))))

(ert-deftest test-kanban-focus-with-a-prefix-shows-every-card ()
  "GIVEN a narrowed board
WHEN focus runs with a prefix argument
THEN every card is back, as `consult-focus-lines' does."
  (test-kanban--with-buffer buffer nil
    (shipit-atlassian-kanban-filter-text "parser")
    (shipit-atlassian-kanban-focus t)
    (should (null (plist-get shipit-atlassian-kanban--filter :text)))
    (should (equal 3 (length (test-kanban--cards
                              (canvas-diagram-current-model)
                              (canvas-diagram-nodes-shown)))))))

(ert-deftest test-kanban-focus-without-consult-reads-the-text-plainly ()
  "GIVEN no consult
WHEN the board is focused
THEN the text is read once and the board narrows, without a preview."
  (test-kanban--with-buffer buffer nil
    (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
               (lambda () nil))
              ((symbol-function 'read-string) (lambda (&rest _) "parser")))
      (shipit-atlassian-kanban-focus))
    (should (equal "parser" (plist-get shipit-atlassian-kanban--filter :text)))))

(ert-deftest test-kanban-focus-starts-from-what-is-set ()
  "GIVEN a filter already set, reversed
WHEN the reader opens
THEN it starts from what is set, bang and all, so it can be edited."
  (test-kanban--with-buffer buffer nil
    (shipit-atlassian-kanban--narrow :text "usb" :text-invert t)
    (should (equal "! usb" (shipit-atlassian-kanban--filter-input)))
    (shipit-atlassian-kanban--narrow :text "usb" :text-invert nil)
    (should (equal "usb" (shipit-atlassian-kanban--filter-input)))))

;;;; Collapsing a column

(ert-deftest test-kanban-build-marks-a-collapsed-column ()
  "GIVEN a column named as collapsed
WHEN the model is built
THEN that column is marked and the others are not."
  (let ((model (shipit-atlassian-kanban--build
                nil (shipit-atlassian-kanban--spec
                     (test-kanban--board) nil '("Open")))))
    (should (shipit-atlassian-kanban-column-collapsed
             (test-kanban--column-named model "Open")))
    (should-not (shipit-atlassian-kanban-column-collapsed
                 (test-kanban--column-named model "Done")))))

(ert-deftest test-kanban-layout-gives-every-column-a-header-node ()
  "GIVEN a board of three columns
WHEN it is laid out
THEN each column has a header node carrying its name, above its cards."
  (test-kanban--with-layout nodes model nil nil
    (should (equal '("Open" "Code Review" "Done")
                   (mapcar #'canvas-diagram-node-label
                           (test-kanban--headers model nodes))))))

(ert-deftest test-kanban-layout-hides-the-cards-of-a-collapsed-column ()
  "GIVEN a collapsed column
WHEN the board is laid out
THEN only its header is there, so its cards are neither drawn nor walked."
  (test-kanban--with-layout nodes model nil '("Open")
    (should (null (test-kanban--node nodes "PROJ-1")))
    (should (null (test-kanban--node nodes "PROJ-2")))
    (should (test-kanban--node nodes "Open"))
    (should (test-kanban--node nodes "PROJ-3"))))

(ert-deftest test-kanban-layout-narrows-a-collapsed-column ()
  "GIVEN a collapsed column
WHEN the board is laid out
THEN it takes only the width of its own header, so the columns after it
     move left and more of the board fits on screen."
  (let ((open (test-kanban--with-layout nodes model nil nil
                (canvas-diagram-node-x (test-kanban--node nodes "Code Review"))))
        (shut (test-kanban--with-layout nodes model nil '("Open")
                (canvas-diagram-node-x (test-kanban--node nodes "Code Review")))))
    (should (< shut open))))

(ert-deftest test-kanban-move-into-a-collapsed-column-lands-on-its-header ()
  "GIVEN a collapsed column
WHEN the keyboard moves into it
THEN it lands on the header, the only thing that column shows."
  (test-kanban--with-layout nodes model nil '("Code Review")
    (should (equal "Code Review" (test-kanban--move nodes model "PROJ-1" 'in)))
    (should (equal "Open" (test-kanban--move nodes model "Code Review" 'out)))))

(ert-deftest test-kanban-tab-on-a-header-collapses-and-expands ()
  "GIVEN the keyboard on a column header
WHEN the column is toggled
THEN its cards go, and toggling again brings them back."
  (test-kanban--with-buffer buffer nil
    (canvas-diagram-set-selected
     (test-kanban--node (canvas-diagram-nodes-shown) "Open"))
    (shipit-atlassian-kanban-toggle-column)
    (should (equal '("Open") shipit-atlassian-kanban--collapsed))
    (should (null (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-1")))
    (shipit-atlassian-kanban-toggle-column)
    (should (null shipit-atlassian-kanban--collapsed))
    (should (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-1"))))

(ert-deftest test-kanban-tab-on-a-card-collapses-its-own-column ()
  "GIVEN the keyboard on a card
WHEN the column is toggled
THEN the card's own column collapses, so there is no need to go to the
     header first."
  (test-kanban--with-buffer buffer nil
    (canvas-diagram-set-selected
     (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-1"))
    (shipit-atlassian-kanban-toggle-column)
    (should (equal '("Open") shipit-atlassian-kanban--collapsed))))

(ert-deftest test-kanban-collapse-outlives-a-refetch ()
  "GIVEN a collapsed column
WHEN the board is fetched again
THEN it stays collapsed, because the state belongs to the buffer."
  (test-kanban--with-buffer buffer nil
    (setq shipit-atlassian-kanban--collapsed '("Open"))
    (setq shipit-atlassian-kanban--board (test-kanban--board))
    (shipit-atlassian-kanban--redraw)
    (should (null (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-1")))
    (should (test-kanban--node (canvas-diagram-nodes-shown) "Open"))))

(ert-deftest test-kanban-staging-on-a-header-says-it-is-not-a-card ()
  "GIVEN the keyboard on a column header
WHEN a move is staged
THEN it says there is no card here rather than staging something odd."
  (test-kanban--with-buffer buffer nil
    (canvas-diagram-set-selected
     (test-kanban--node (canvas-diagram-nodes-shown) "Open"))
    (should-error (shipit-atlassian-kanban-stage-forward) :type 'user-error)))

;;;; Finding and narrowing from the buffer

(ert-deftest test-kanban-search-key-finds-a-card ()
  "GIVEN a canvas, which holds no text for `isearch' to walk
WHEN the key that searches is pressed
THEN it finds a card instead."
  (test-kanban--with-buffer buffer nil
    (should (eq 'shipit-atlassian-kanban-find-card (key-binding (kbd "C-s"))))))

(ert-deftest test-kanban-find-offers-what-is-drawn ()
  "GIVEN a board with a collapsed column
WHEN the cards to find are gathered
THEN every drawn card is offered by key and summary, and the hidden
     ones are not, because there is nothing to land on."
  (test-kanban--with-buffer buffer nil
    (setq shipit-atlassian-kanban--collapsed '("Open"))
    (shipit-atlassian-kanban--redraw)
    (let ((offered (mapcar #'car (shipit-atlassian-kanban--find-candidates))))
      (should (equal '("PROJ-3  Ship the parser") offered)))))

(ert-deftest test-kanban-find-moves-the-keyboard-to-the-card ()
  "GIVEN a card chosen from the finder
WHEN the choice comes back
THEN the keyboard is on that card."
  (test-kanban--with-buffer buffer nil
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "PROJ-3  Ship the parser")))
      (shipit-atlassian-kanban-find-card))
    (should (equal "PROJ-3"
                   (canvas-diagram-node-label (canvas-diagram-selected))))))

(ert-deftest test-kanban-find-uses-consult-when-it-is-there ()
  "GIVEN consult is loaded
WHEN a card is read
THEN consult reads it, with a preview that moves the keyboard."
  (test-kanban--with-buffer buffer nil
    (let ((asked nil))
      (cl-letf (((symbol-function 'shipit-atlassian-kanban--consult-available-p)
                 (lambda () t))
                ((symbol-function 'consult--read)
                 (lambda (candidates &rest args)
                   (setq asked args)
                   (funcall (plist-get args :state) 'preview (car candidates))
                   (car candidates))))
        (shipit-atlassian-kanban-find-card))
      (should (equal 'shipit-atlassian-kanban-card (plist-get asked :category)))
      (should (equal "PROJ-1"
                     (canvas-diagram-node-label (canvas-diagram-selected)))))))

(ert-deftest test-kanban-find-puts-the-keyboard-back-when-nothing-is-chosen ()
  "GIVEN a preview has moved the keyboard
WHEN the reader comes back with nothing
THEN the keyboard is where it started."
  (test-kanban--with-buffer buffer nil
    (canvas-diagram-set-selected
     (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-2"))
    (let* ((candidates (shipit-atlassian-kanban--find-candidates))
           (preview (shipit-atlassian-kanban--preview candidates)))
      (funcall preview 'preview (car (car candidates)))
      (should (equal "PROJ-1"
                     (canvas-diagram-node-label (canvas-diagram-selected))))
      (funcall preview 'return nil)
      (should (equal "PROJ-2"
                     (canvas-diagram-node-label (canvas-diagram-selected)))))))

(ert-deftest test-kanban-filter-mine-needs-to-know-who-i-am ()
  "GIVEN Jira did not say who the current user is
WHEN the board is narrowed to mine
THEN it says so rather than hiding every card."
  (test-kanban--with-buffer buffer nil
    (setq shipit-atlassian-kanban--me nil)
    (should-error (shipit-atlassian-kanban-filter-mine) :type 'user-error)))

(ert-deftest test-kanban-filter-text-narrows-and-clears ()
  "GIVEN a text to narrow by
WHEN it is set and then cleared
THEN the board narrows and comes back, and the mode line says which."
  (test-kanban--with-buffer buffer nil
    (shipit-atlassian-kanban-filter-text "parser")
    (should (equal '("PROJ-3") (mapcar #'canvas-diagram-node-label
                                       (test-kanban--cards
                                        (canvas-diagram-current-model)
                                        (canvas-diagram-nodes-shown)))))
    (should (string-match-p "filtered" mode-line-process))
    (shipit-atlassian-kanban-filter-text "")
    (should (equal 3 (length (test-kanban--cards
                              (canvas-diagram-current-model)
                              (canvas-diagram-nodes-shown)))))
    (should (null mode-line-process))))

(ert-deftest test-kanban-filter-type-narrows-to-one-type ()
  "GIVEN cards of several types
WHEN the board is narrowed to one
THEN only that type is drawn."
  (test-kanban--with-buffer buffer nil
    (shipit-atlassian-kanban-filter-type "Bug")
    (should (equal '("PROJ-1") (mapcar #'canvas-diagram-node-label
                                       (test-kanban--cards
                                        (canvas-diagram-current-model)
                                        (canvas-diagram-nodes-shown)))))))

(defun test-kanban--prioritised-board ()
  "The fixture board with a priority on each card of Open."
  (let ((board (copy-tree (test-kanban--board))))
    (setf (alist-get 'priority (cadr (cdr (assq 'issues (car (cdr (assq 'columns board)))))))
          "Low")
    board))

(ert-deftest test-kanban-filter-priority-narrows-to-one-priority ()
  "GIVEN cards of several priorities
WHEN the board is narrowed to one, and then to none
THEN only the cards of that priority are drawn and the mode line says
the board is narrowed, and then every card is back."
  (test-kanban--with-buffer buffer nil
    (shipit-atlassian-kanban-filter-priority "High")
    (should (equal '("PROJ-1") (mapcar #'canvas-diagram-node-label
                                       (test-kanban--cards
                                        (canvas-diagram-current-model)
                                        (canvas-diagram-nodes-shown)))))
    (should (string-match-p "filtered" mode-line-process))
    (shipit-atlassian-kanban-filter-priority "")
    (should (equal 3 (length (test-kanban--cards (canvas-diagram-current-model)
                                                 (canvas-diagram-nodes-shown)))))))

(ert-deftest test-kanban-filter-priority-offers-the-priorities-on-the-board ()
  "GIVEN a board with a High card and a Low card
WHEN the priorities to narrow to are asked for
THEN they are the two, in the order the board shows them."
  (let ((test-kanban--board-override (test-kanban--prioritised-board)))
    (test-kanban--with-buffer buffer nil
      (should (equal '("High" "Low")
                     (shipit-atlassian-kanban--priorities-on-the-board))))))

(ert-deftest test-kanban-menu-narrows-by-priority ()
  "GIVEN the board's menu
THEN p narrows the board to a priority."
  (skip-unless (shipit-canvas-available-p))
  (should (eq 'shipit-atlassian-kanban-filter-priority
              (plist-get (cdr (transient-get-suffix 'shipit-atlassian-kanban-menu "p"))
                         :command))))

;;;; Jumping to either end of a column

(defun test-kanban--on (key)
  "The label of the node the keyboard is on after putting it on KEY."
  (canvas-diagram-set-selected (test-kanban--node (canvas-diagram-nodes-shown) key)))

(defun test-kanban--selected-label ()
  "The label of the node the keyboard is on."
  (canvas-diagram-node-label (canvas-diagram-selected)))

(ert-deftest test-kanban-column-ends-are-on-control-home-and-end ()
  "GIVEN a board buffer
WHEN the keys are read
THEN C-<home> and C-<end> jump within the column, while the other keys
     that run `beginning-of-buffer' still reach the ends of the board."
  (test-kanban--with-buffer buffer nil
    (should (eq 'shipit-atlassian-kanban-column-top (key-binding (kbd "C-<home>"))))
    (should (eq 'shipit-atlassian-kanban-column-bottom (key-binding (kbd "C-<end>"))))
    (should (eq 'canvas-diagram-move-first (key-binding (kbd "M-<"))))
    (should (eq 'canvas-diagram-move-last (key-binding (kbd "M->"))))))

(ert-deftest test-kanban-column-bottom-goes-to-the-last-card ()
  "GIVEN the keyboard on the first card of a column
WHEN it jumps to the bottom of the column
THEN it is on that column's last card, not on the board's last."
  (test-kanban--with-buffer buffer nil
    (test-kanban--on "PROJ-1")
    (shipit-atlassian-kanban-column-bottom)
    (should (equal "PROJ-2" (test-kanban--selected-label)))))

(ert-deftest test-kanban-column-top-goes-to-the-first-card-then-the-header ()
  "GIVEN the keyboard on the last card of a column
WHEN it jumps to the top of the column twice, and once more
THEN it lands on the first card, then on the header, and stays there."
  (test-kanban--with-buffer buffer nil
    (test-kanban--on "PROJ-2")
    (shipit-atlassian-kanban-column-top)
    (should (equal "PROJ-1" (test-kanban--selected-label)))
    (shipit-atlassian-kanban-column-top)
    (should (equal "Open" (test-kanban--selected-label)))
    (shipit-atlassian-kanban-column-top)
    (should (equal "Open" (test-kanban--selected-label)))))

(ert-deftest test-kanban-column-ends-of-an-empty-column-are-its-header ()
  "GIVEN the keyboard on the header of a column with no cards
WHEN it jumps to either end of the column
THEN it stays on the header, the only thing there."
  (test-kanban--with-buffer buffer nil
    (test-kanban--on "Done")
    (shipit-atlassian-kanban-column-bottom)
    (should (equal "Done" (test-kanban--selected-label)))
    (shipit-atlassian-kanban-column-top)
    (should (equal "Done" (test-kanban--selected-label)))))

(ert-deftest test-kanban-going-to-a-card-brings-it-into-view ()
  "GIVEN a card the keyboard is sent to
WHEN it goes there
THEN it goes through canvas-diagram's own move, which scrolls the view
     to it.

Setting the selection and drawing again left a card far down a long
column off the screen with the ring on it."
  (test-kanban--with-buffer buffer nil
    (let ((went nil))
      (cl-letf (((symbol-function 'canvas-diagram-go)
                 (lambda (node) (setq went node))))
        (test-kanban--on "PROJ-1")
        (shipit-atlassian-kanban-column-bottom))
      (should (equal "PROJ-2" (canvas-diagram-node-label went))))))

;;;; Where the keyboard is after a column folds

(ert-deftest test-kanban-folding-a-card-s-column-lands-on-its-header ()
  "GIVEN the keyboard on a card in a column that is not the first
WHEN that column is collapsed
THEN the keyboard is on that column's header, because the card is gone,
     and not on the header of the leftmost column."
  (test-kanban--with-buffer buffer nil
    (test-kanban--on "PROJ-3")
    (shipit-atlassian-kanban-toggle-column)
    (should (equal "Code Review" (test-kanban--selected-label)))))

(ert-deftest test-kanban-folding-from-a-header-stays-on-it ()
  "GIVEN the keyboard on the header of a column that is not the first
WHEN the column is collapsed and then opened again
THEN the keyboard stays on that header both times."
  (test-kanban--with-buffer buffer nil
    (test-kanban--on "Code Review")
    (shipit-atlassian-kanban-toggle-column)
    (should (equal "Code Review" (test-kanban--selected-label)))
    (shipit-atlassian-kanban-toggle-column)
    (should (equal "Code Review" (test-kanban--selected-label)))))

(ert-deftest test-kanban-drawing-again-keeps-the-keyboard-on-a-header ()
  "GIVEN the keyboard on a column header
WHEN the board is drawn again
THEN it is still on that header, as it would be on a card."
  (test-kanban--with-buffer buffer nil
    (test-kanban--on "Done")
    (shipit-atlassian-kanban--redraw)
    (should (equal "Done" (test-kanban--selected-label)))))

(ert-deftest test-kanban-a-mark-on-a-card-outlives-folding ()
  "GIVEN a marked card
WHEN its column is collapsed and opened again
THEN the card is still marked, since a mark is held by the card's key."
  (test-kanban--with-buffer buffer nil
    (test-kanban--on "PROJ-1")
    (canvas-diagram-toggle-mark)
    (shipit-atlassian-kanban-toggle-column)
    (test-kanban--on "Open")
    (shipit-atlassian-kanban-toggle-column)
    (should (canvas-diagram-marked-p
             (test-kanban--node (canvas-diagram-nodes-shown) "PROJ-1")))))

(provide 'test-shipit-atlassian-kanban)
;;; test-shipit-atlassian-kanban.el ends here
