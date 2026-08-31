;;; test-shipit-stacked-preview.el --- Dividing commits into layers -*- lexical-binding: t; -*-

;;; Commentary:
;; Where the divisions go, and what layers they make.  This is the part
;; that decides what becomes a pull request, so it is worked out from
;; data and tested without a buffer.

;;; Code:

(require 'ert)
(require 'shipit-stacked-preview)
(require 'shipit-pr-github)
;; Loaded here so the require inside `shipit-stacked-preview-visit-layer'
;; is a no-op and cannot replace the stub this file puts in its place.
(require 'shipit-preview)

(defconst test-stacked-preview--commits
  '((:sha "a1" :first-line "fpga: conventions")
    (:sha "a2" :first-line "fpga: point tests at the skill")
    (:sha "b1" :first-line "uoe: abort path")
    (:sha "b2" :first-line "uoe: tx mux")
    (:sha "c1" :first-line "zhit: formal"))
  "Five commits across three areas, oldest first.")

(ert-deftest test-shipit-stacked-preview-proposes-a-division-where-the-area-changes ()
  "GIVEN commits whose subjects name the area they touch
WHEN divisions are proposed
THEN one falls after the last commit of each run, and never after the
newest: that one ends the top layer, and the top of a pile is not a
division."
  (should (equal '("a2" "b2")
                 (shipit-stacked-preview--proposed-boundaries
                  test-stacked-preview--commits))))

(ert-deftest test-shipit-stacked-preview-proposes-nothing-without-prefixes ()
  "GIVEN subjects with no areas named
WHEN divisions are proposed
THEN there are none, leaving one layer and the dividing to be done by
hand.  Guessing at boundaries would be inventing them."
  (should-not (shipit-stacked-preview--proposed-boundaries
               '((:sha "a" :first-line "first thing")
                 (:sha "b" :first-line "second thing")))))

(ert-deftest test-shipit-stacked-preview-divides-into-layers-bottom-first ()
  "GIVEN divisions
WHEN the layers are worked out
THEN each holds the commits up to its division, oldest first, and the
last keeps the branch already checked out, whose work is there
already."
  (let ((layers (shipit-stacked-preview--layers
                 test-stacked-preview--commits '("a2" "b2") "work")))
    (should (equal 3 (length layers)))
    (should (equal '(2 2 1)
                   (mapcar (lambda (l) (length (cdr (assq 'commits l)))) layers)))
    (should (equal '("work-fpga" "work-uoe" "work")
                   (mapcar (lambda (l) (cdr (assq 'branch l))) layers)))
    (should (equal '("a2" "b2" "c1")
                   (mapcar (lambda (l) (cdr (assq 'tip l))) layers)))
    (should (equal '(nil nil t)
                   (mapcar (lambda (l) (cdr (assq 'existing l))) layers)))))

(ert-deftest test-shipit-stacked-preview-with-no-divisions-is-one-layer ()
  "GIVEN no divisions
WHEN the layers are worked out
THEN there is one, holding everything, and it is the branch already
checked out.  That is an ordinary pull request, which is what this
was before anyone divided it."
  (let ((layers (shipit-stacked-preview--layers test-stacked-preview--commits nil "work")))
    (should (equal 1 (length layers)))
    (should (equal "work" (cdr (assq 'branch (car layers)))))
    (should (cdr (assq 'existing (car layers))))))

(ert-deftest test-shipit-stacked-preview-names-a-layer-after-its-area ()
  "GIVEN a layer whose commits share an area
WHEN it is named
THEN the name says which, so the branch and the pull request it
becomes are recognisable."
  (should (equal "work-uoe"
                 (shipit-stacked-preview--layer-name
                  "work" '((:sha "b1" :first-line "uoe: abort path"))))))

(ert-deftest test-shipit-stacked-preview-keeps-the-names-branches-already-have ()
  "GIVEN layers already marked by branches
WHEN the layers are worked out
THEN each keeps the name its branch gave it, rather than one derived
from its commit subjects.  A stack prepared elsewhere, or divided
here before, should open as it stands."
  (let ((layers (shipit-stacked-preview--layers
                 test-stacked-preview--commits '("a2" "b2") "work"
                 '(("a2" . "PRJ-101-parser-cleanup")
                   ("b2" . "PRJ-102-writer-rework")))))
    (should (equal '("PRJ-101-parser-cleanup"
                     "PRJ-102-writer-rework"
                     "work")
                   (mapcar (lambda (l) (cdr (assq 'branch l))) layers)))))

(ert-deftest test-shipit-stacked-preview-derives-a-name-only-where-there-is-none ()
  "GIVEN one layer marked by a branch and one not
WHEN the layers are worked out
THEN the marked one keeps its name and the other is named from its
commits, so dividing further does not rename what is already there."
  (let ((layers (shipit-stacked-preview--layers
                 test-stacked-preview--commits '("a2" "b2") "work"
                 '(("a2" . "already-here")))))
    (should (equal '("already-here" "work-uoe" "work")
                   (mapcar (lambda (l) (cdr (assq 'branch l))) layers)))))

(ert-deftest test-shipit-stacked-preview-commits-line-up-under-their-layer ()
  "GIVEN a layer and the commits inside it
WHEN both are drawn
THEN a commit's hash begins in the same column as a branch name.

A layer row spends its first columns on the HEAD mark and the layer
number, which a commit has neither of, so without an indent the
commits sit to the left of the layer holding them."
  (let* ((layer-row (format "  %-5s %2d  %s" "HEAD" 6 "branch-name"))
         (branch-column (string-match "branch-name" layer-row))
         ;; the renderer spends one column, then two on the unread mark,
         ;; before the hash
         (hash-column (+ shipit-stacked-preview--commit-indent 1 2)))
    (should (equal branch-column hash-column))))

(ert-deftest test-shipit-stacked-preview-counts-line-up-past-two-digits ()
  "GIVEN layers whose commit counts differ in width
WHEN the column width is worked out
THEN it fits the widest.  A branch left far behind its base holds
hundreds, and a fixed field pushes the word beside it out of line the
moment one does."
  (should (equal 3 (shipit-stacked-preview--count-width
                    (list (list (cons 'commits (make-list 507 nil)))
                          (list (cons 'commits (make-list 7 nil)))))))
  (should (equal 1 (shipit-stacked-preview--count-width
                    (list (list (cons 'commits (make-list 7 nil))))))))

(defun test-stacked--layers (&rest branches)
  "Return layers named BRANCHES, bottom first, each holding one commit."
  (mapcar (lambda (branch)
            (list (cons 'branch branch)
                  (cons 'commits (list (list :sha "abc1234" :first-line "x")))))
          branches))

(ert-deftest test-shipit-stacked-preview-layer-targets-the-one-below ()
  "GIVEN a stack of three layers sitting on master
WHEN each layer is asked what its pull request should be raised against
THEN it names the layer below it, and the bottom layer names the branch
the whole stack sits on, read from the remote."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "origin/master")
    (let ((layers (test-stacked--layers "one" "two" "three")))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stack--rev) (lambda (_) "abc1234")))
        (should (equal "origin/master"
                       (shipit-stacked-preview--base-of (nth 0 layers))))
        (should (equal "one" (shipit-stacked-preview--base-of (nth 1 layers))))
        (should (equal "two" (shipit-stacked-preview--base-of (nth 2 layers))))))))

(ert-deftest test-shipit-stacked-preview-refuses-a-layer-without-a-branch ()
  "GIVEN a layer no branch has been put on yet
WHEN it is acted on
THEN it says so rather than acting on a name that resolves to nothing."
  (let ((layer (car (test-stacked--layers "not-made-yet"))))
    (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) nil)))
      (should-error (shipit-stacked-preview--require-branch layer)
                    :type 'user-error))))

(ert-deftest test-shipit-stacked-preview-stops-at-the-branch-that-failed ()
  "GIVEN three layers where the middle one cannot be sent
WHEN every layer is sent
THEN the ones below it were tried, the ones above it were left alone,
and the failure is raised.  A branch whose parent never arrived cannot
be the base of a pull request."
  (let ((layers (test-stacked--layers "one" "two" "three"))
        (tried nil))
    (cl-letf (((symbol-function 'shipit-stacked-preview--refuse-when-broken)
               (lambda (_layer) nil))
              ((symbol-function 'shipit-stacked-preview--current-layers)
               (lambda () layers))
              ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
              ((symbol-function 'shipit--push-branch)
               (lambda (branch _args)
                 (setq tried (append tried (list branch)))
                 (not (equal branch "two")))))
      (should-error (shipit-stacked-preview--push-through (nth 2 layers) nil)
                    :type 'user-error)
      (should (equal '("one" "two") tried)))))

(ert-deftest test-shipit-stacked-preview-dry-run-asks-git-for-one ()
  "GIVEN the dry run argument
WHEN every layer is sent
THEN git is asked with it, and no layer is left saying it is on its
way."
  (let ((layers (test-stacked--layers "one" "two"))
        (seen nil)
        (redrawn nil))
    (cl-letf (((symbol-function 'shipit-stacked-preview--refuse-when-broken)
               (lambda (_layer) nil))
              ((symbol-function 'shipit-stacked-preview--current-layers)
               (lambda () layers))
              ((symbol-function 'shipit-stacked-preview--render)
               (lambda () (setq redrawn t)))
              ((symbol-function 'shipit--push-branch)
               (lambda (_branch args) (setq seen args) t)))
      (shipit-stacked-preview--push-through (nth 1 layers) '("--dry-run"))
      (should (equal '("--dry-run") seen))
      (should redrawn)
      (should-not shipit-stacked-preview--sending))))

(ert-deftest test-shipit-stacked-preview-hands-a-layer-to-the-preview ()
  "GIVEN a stack whose middle layer is visited
WHEN it is handed over
THEN the preview is asked for that branch, against the layer below,
ending at that branch rather than at HEAD, and with a way back."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "master"
          shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two" "three"))
          (asked nil))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--layer-at-point)
                 (lambda () (nth 1 layers)))
                ((symbol-function 'shipit-stack--rev) (lambda (_) "abc1234"))
                ((symbol-function 'shipit-stacked-preview--pr-number)
                 (lambda (_branch) nil))
                ((symbol-function 'shipit-stacked-preview--refuse-when-broken)
                 (lambda (_layer) nil))
                ((symbol-function 'shipit-preview-branch)
                 (lambda (&rest args) (setq asked args))))
        (shipit-stacked-preview-visit-layer)
        (should (equal '("owner/repo" "two" "one") (seq-take asked 3)))
        (let ((keys (nthcdr 3 asked)))
          (should (equal "two" (plist-get keys :tip)))
          (should (plist-get keys :stacked))
          (should (functionp (plist-get keys :on-done))))))))

(ert-deftest test-shipit-stacked-preview-comes-back-brought-up-to-date ()
  "GIVEN a preview opened from a stack
WHEN it is finished with
THEN the stack it came from is redrawn, so the layer just submitted
shows what it became."
  (let ((redrawn nil))
    (with-temp-buffer
      (setq shipit-stacked-preview--repo "owner/repo")
      (cl-letf (((symbol-function 'shipit-stacked-preview--render)
                 (lambda () (setq redrawn t)))
                ((symbol-function 'shipit-cache-forget-branch-pr)
                 (lambda (&rest _) nil))
                ((symbol-function 'shipit-stacked-preview--group-submitted)
                 (lambda () nil))
                ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
        (funcall (shipit-stacked-preview--return-to (current-buffer) "two") 42)
        (should redrawn)))))

(ert-deftest test-shipit-stacked-preview-will-not-send-a-broken-stack ()
  "GIVEN layers whose branches are not built on one another
WHEN they are sent
THEN nothing is sent and the reason is given.  Branches that reach the
remote out of line make pull requests carrying each other's commits,
which is easier to prevent than to unpick."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "master")
    (let ((layers (test-stacked--layers "one" "two"))
          (sent nil))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stack--rev) (lambda (_) "abc1234"))
                ((symbol-function 'shipit-stack-problems)
                 (lambda (_branches _base) '("two is not built on one")))
                ((symbol-function 'shipit--push-branch)
                 (lambda (&rest _) (setq sent t) t)))
        (should-error (shipit-stacked-preview--push-through (nth 1 layers) nil)
                      :type 'user-error)
        (should-not sent)))))

(ert-deftest test-shipit-stacked-preview-checks-the-layers-below-as-well ()
  "GIVEN the middle layer of a stack
WHEN it is sent on its own
THEN the layers below it are checked too, each being the base of the
next: a layer standing on a broken one cannot be submitted either."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "master")
    (let ((layers (test-stacked--layers "one" "two" "three"))
          (asked nil))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stack--rev) (lambda (_) "abc1234"))
                ((symbol-function 'shipit-stack-problems)
                 (lambda (branches _base) (setq asked branches) nil))
                ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
                ((symbol-function 'shipit--push-branch) (lambda (&rest _) t)))
        (shipit-stacked-preview--push-through (nth 1 layers) nil)
        (should (equal '("one" "two") asked))))))

(ert-deftest test-shipit-stacked-preview-key-cells-are-one-width ()
  "GIVEN a column that runs out of keys before its neighbours do
WHEN a row is laid out
THEN its empty cell is as wide as a full one, so the columns to the
right of it stay in line."
  (let ((full (shipit-stacked-preview--key-cell '("RET" . "describe") 3 8))
        (empty (shipit-stacked-preview--key-cell nil 3 8)))
    (should (equal 13 (length full)))
    (should (equal (length full) (length empty)))))

(ert-deftest test-shipit-stacked-preview-every-key-shown-is-bound ()
  "GIVEN the keys this buffer advertises
WHEN each is looked up in the keymap
THEN it runs something.  A key written down that does nothing is worse
than one not written down at all."
  (dolist (entry (apply #'append shipit-stacked-preview--key-columns))
    (should (equal (list (car entry) t)
                   (list (car entry)
                         (commandp (lookup-key shipit-stacked-preview-mode-map
                                               (kbd (car entry)))))))))

(ert-deftest test-shipit-stacked-preview-a-layer-takes-the-ones-below-it ()
  "GIVEN the middle layer of a stack
WHEN it is sent on its own
THEN the layer below it is sent first and the one above is left.  A
pull request is raised against a branch the remote must already have,
so a layer sent by itself would arrive unsubmittable."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "origin/master")
    (let ((layers (test-stacked--layers "one" "two" "three"))
          (sent nil))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--refuse-when-broken)
                 (lambda (_layer) nil))
                ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
                ((symbol-function 'shipit--push-branch)
                 (lambda (branch _args) (setq sent (append sent (list branch))) t)))
        (shipit-stacked-preview--push-through (nth 1 layers) nil)
        (should (equal '("one" "two") sent))))))

(ert-deftest test-shipit-stacked-preview-will-not-submit-onto-an-absent-base ()
  "GIVEN a layer whose lower layer has not reached the remote
WHEN it is handed to the preview
THEN it is refused here, naming what to send.  Left to the remote it
would be refused at creation, after the description had been written."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "origin/master"
          shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two"))
          (handed-over nil))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--layer-at-point)
                 (lambda () (nth 1 layers)))
                ((symbol-function 'shipit-stacked-preview--refuse-when-broken)
                 (lambda (_layer) nil))
                ;; the branches exist here, but nothing is on the remote
                ((symbol-function 'shipit-stack--rev)
                 (lambda (ref) (unless (string-prefix-p "origin/" ref) "abc1234")))
                ((symbol-function 'shipit-preview-branch)
                 (lambda (&rest _) (setq handed-over t))))
        (should-error (shipit-stacked-preview-visit-layer) :type 'user-error)
        (should-not handed-over)))))

(ert-deftest test-shipit-stacked-preview-bottom-layer-needs-nothing-below ()
  "GIVEN the bottom layer, with nothing under it but the base
WHEN it is handed to the preview
THEN nothing stops it: the branch it is raised against is the one the
whole stack sits on, which is already there."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "origin/master"
          shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two"))
          (handed-over nil))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--layer-at-point)
                 (lambda () (nth 0 layers)))
                ((symbol-function 'shipit-stacked-preview--refuse-when-broken)
                 (lambda (_layer) nil))
                ((symbol-function 'shipit-stack--rev)
                 (lambda (ref) (unless (string-prefix-p "origin/" ref) "abc1234")))
                ((symbol-function 'shipit-preview-branch)
                 (lambda (&rest _) (setq handed-over t))))
        (shipit-stacked-preview-visit-layer)
        (should handed-over)))))

(defun test-stacked--status-face (layer)
  "Return the face the status of LAYER is written in."
  (get-text-property 0 'font-lock-face
                     (shipit-stacked-preview--layer-status layer)))

(ert-deftest test-shipit-stacked-preview-status-is-coloured-by-how-far-it-got ()
  "GIVEN layers at each stage
WHEN their status is asked for
THEN each is written in a face saying how far along it is, so the state
of the stack can be read off the colours."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((layer (car (test-stacked--layers "one"))))
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) nil)))
        (should (equal 'font-lock-comment-face (test-stacked--status-face layer))))
      (cl-letf (((symbol-function 'shipit-stack--rev)
                 (lambda (ref) (unless (string-prefix-p "origin/" ref) "abc"))))
        (should (equal 'font-lock-comment-face (test-stacked--status-face layer))))
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
                ((symbol-function 'shipit-stack-plan--pr-for-branch)
                 (lambda (&rest _) nil)))
        (should (equal 'success (test-stacked--status-face layer))))
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
                ((symbol-function 'shipit-stack-plan--pr-for-branch)
                 (lambda (&rest _) 4471)))
        (should (equal 'magit-branch-remote (test-stacked--status-face layer)))))))

(ert-deftest test-shipit-stacked-preview-the-branch-on-its-way-says-so ()
  "GIVEN a branch being sent
WHEN the rows are drawn
THEN that one says so, and is not reported by where it stood when the
push began.  Each push blocks, so this is the only sign anything is
happening."
  (with-temp-buffer
    (let ((layer (car (test-stacked--layers "one"))))
      (setq shipit-stacked-preview--sending "one")
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) nil)))
        (should (equal "sending…" (shipit-stacked-preview--layer-status layer)))
        (should (equal 'warning (test-stacked--status-face layer)))))))

(ert-deftest test-shipit-stacked-preview-a-failed-push-leaves-no-mark ()
  "GIVEN a push that fails partway
WHEN it gives up
THEN no layer is left saying it is on its way, which would be a lie
that survived until the next redraw."
  (with-temp-buffer
    (setq shipit-stacked-preview--base "origin/master")
    (let ((layers (test-stacked--layers "one" "two")))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--refuse-when-broken)
                 (lambda (_layer) nil))
                ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
                ((symbol-function 'redisplay) (lambda (&rest _) nil))
                ((symbol-function 'shipit--push-branch) (lambda (&rest _) nil)))
        (should-error (shipit-stacked-preview--push-through (nth 1 layers) nil)
                      :type 'user-error)
        (should-not shipit-stacked-preview--sending)))))

(ert-deftest test-shipit-stacked-preview-counts-an-unbroken-run-only ()
  "GIVEN layers where the middle one has no pull request
WHEN the submitted run is worked out
THEN it stops there.  A stack is a chain, so the layers above a gap are
not part of it and grouping them would claim a chain that is not."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two" "three")))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--pr-number)
                 (lambda (branch) (pcase branch ("one" 1) ("three" 3)))))
        (should (equal '(1) (shipit-stacked-preview--submitted-numbers)))))))

(ert-deftest test-shipit-stacked-preview-groups-what-has-been-submitted ()
  "GIVEN two submitted layers that are in no stack
WHEN they are grouped
THEN one stack is asked for, holding both in order.  The remote checks
the chain but will not group them unasked."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((grouped nil))
      (cl-letf (((symbol-function 'shipit-pr--fetch-stack) (lambda (&rest _) nil))
                ((symbol-function 'shipit-stacked-preview--backend-call)
                 (lambda (operation &rest args)
                   (setq grouped (cons operation args)))))
        (shipit-stacked-preview--group '(1 2))
        (should (equal '(:create-stack (1 2)) grouped))))))

(ert-deftest test-shipit-stacked-preview-extends-a-stack-it-already-has ()
  "GIVEN a stack already holding the lower layers
WHEN a further layer is submitted
THEN only that one is added, and to the stack that exists.  Grouping
them afresh would leave a second stack beside the first."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((calls nil))
      (cl-letf (((symbol-function 'shipit-pr--fetch-stack)
                 (lambda (&rest _)
                   '((number . 7) (entries . (((number . 1)) ((number . 2)))))))
                ((symbol-function 'shipit-stacked-preview--backend-call)
                 (lambda (operation &rest args)
                   (setq calls (append calls (list (cons operation args)))))))
        (shipit-stacked-preview--group '(1 2 3))
        (should (equal '((:add-to-stack 7 3)) calls))))))

(ert-deftest test-shipit-stacked-preview-will-not-group-one-pull-request ()
  "GIVEN a single submitted layer
WHEN grouping is asked for
THEN it says so rather than asking the remote for a stack of one."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two")))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--pr-number)
                 (lambda (branch) (when (equal branch "one") 1)))
                ((symbol-function 'shipit-stacked-preview--group)
                 (lambda (_numbers) (error "should not be reached"))))
        (should-error (shipit-stacked-preview-register) :type 'user-error)))))

(ert-deftest test-shipit-stacked-preview-forgets-what-it-knew-of-the-branch ()
  "GIVEN a layer that has just been submitted
WHEN the stack buffer takes over again
THEN what was remembered about that branch is dropped.  That it had no
pull request was cached, misses included, and it has just stopped being
true."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((forgotten nil))
      (cl-letf (((symbol-function 'shipit-cache-forget-branch-pr)
                 (lambda (repo branch) (setq forgotten (list repo branch))))
                ((symbol-function 'shipit-stacked-preview--group-submitted)
                 (lambda () nil))
                ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
                ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
        (funcall (shipit-stacked-preview--return-to (current-buffer) "two") 42)
        (should (equal '("owner/repo" "two") forgotten))))))

(ert-deftest test-shipit-stacked-preview-a-submitted-layer-opens-its-pr ()
  "GIVEN a layer that has already been submitted
WHEN it is opened
THEN its pull request is shown rather than a preview of one.  Offering
to describe a layer that has a pull request is offering to make a
second."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/demo")
    (let ((layers (test-stacked--layers "one"))
          (opened nil)
          (previewed nil))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--layer-at-point)
                 (lambda () (car layers)))
                ((symbol-function 'shipit-stack--rev) (lambda (_) "abc1234"))
                ((symbol-function 'shipit-stacked-preview--pr-number)
                 (lambda (_branch) 100))
                ((symbol-function 'shipit-open-pr-buffer)
                 (lambda (number repo) (setq opened (list number repo))))
                ((symbol-function 'shipit-preview-branch)
                 (lambda (&rest _) (setq previewed t))))
        (shipit-stacked-preview-visit-layer)
        (should (equal '(100 "owner/demo") opened))
        (should-not previewed)))))

(ert-deftest test-shipit-stacked-preview-commits-show-newest-first ()
  "GIVEN a layer's commits, oldest first as they merge
WHEN they are ordered for display
THEN the newest comes first, matching the direction the stack is
drawn: the base sits at the bottom and history grows upward."
  (should (equal '(3 2 1)
                 (mapcar (lambda (c) (plist-get c :n))
                         (shipit-stacked-preview--display-commits
                          '((:n 1) (:n 2) (:n 3)))))))

(ert-deftest test-shipit-stacked-preview-a-layer-starts-open ()
  "GIVEN a freshly drawn preview layer
THEN it is shown, commits and all: the preview exists to look at the
division, and a layer born shut made every reader press TAB first."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
    (magit-insert-section (shipit-pr-buffer)
      (magit-insert-section (shipit-stack-layer "work")
        (magit-insert-heading "  HEAD 1 work")
        (magit-insert-section-body (insert "    c1\n"))))
    (let ((layer (car (oref magit-root-section children))))
      (should-not (oref layer hidden)))))

(ert-deftest test-shipit-stacked-preview-refs-name-head-upstream-and-base ()
  "GIVEN a branch whose upstream is not the base the stack is over
WHEN the refs are inserted
THEN the buffer names the branch, its upstream with how far ahead,
and the base in its resolved form, so the branch stood on is not
mistaken for the base stacked over."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-refs-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (shipit-sandbox--git
           "push" "-q" "origin"
           (concat (car (shipit-stack--git-lines "rev-parse" "work~3"))
                   ":refs/heads/elsewhere"))
          (shipit-sandbox--git "branch" "-q" "-u" "origin/elsewhere")
          (with-temp-buffer
            (setq default-directory (file-name-as-directory
                                     (expand-file-name "work" root)))
            (setq-local shipit-stacked-preview--base "origin/main")
            (shipit-stacked-preview--insert-refs)
            (let ((text (buffer-string)))
              (should (string-match-p "Head:.*work" text))
              (should (string-match-p "Upstream:.*origin/elsewhere.*3 ahead" text))
              (should (string-match-p "Base:.*origin/main" text)))))
      (delete-directory root t))))

(defmacro test-stacked-preview--with-pr-backend (branch-with-pr &rest body)
  "Run BODY with a backend that has an open PR for BRANCH-WITH-PR only."
  (declare (indent 1))
  `(let ((shipit-pr-backend 'test-preview)
         (shipit-pr-backends
          (list (cons 'test-preview
                      (list :name "Preview"
                            :fetch-pr-for-branch
                            (lambda (_config branch)
                              (and (equal branch ,branch-with-pr) 7)))))))
     ,@body))

(ert-deftest test-shipit-stacked-preview-base-follows-an-open-pr ()
  "GIVEN a commit below HEAD that is the tip of a remote branch
AND that branch has an open pull request
WHEN the base is detected
THEN it is that branch: the commit is already on its way to the
default branch, and stacking it again would raise it twice."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-base-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (shipit-sandbox--git
           "push" "-q" "origin"
           (concat (car (shipit-stack--git-lines "rev-parse" "work~3"))
                   ":refs/heads/part-one"))
          (test-stacked-preview--with-pr-backend "part-one"
            (should (equal "origin/part-one"
                           (shipit-stacked-preview--detect-base
                            "o/r" "origin/main")))))
      (delete-directory root t))))

(ert-deftest test-shipit-stacked-preview-base-stays-without-an-open-pr ()
  "GIVEN a commit below HEAD marked by a remote branch with no pull
request
WHEN the base is detected
THEN the default stands: a branch alone promises nothing about
reaching the default branch."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-base-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (shipit-sandbox--git
           "push" "-q" "origin"
           (concat (car (shipit-stack--git-lines "rev-parse" "work~3"))
                   ":refs/heads/part-one"))
          (test-stacked-preview--with-pr-backend "something-else"
            (should (equal "origin/main"
                           (shipit-stacked-preview--detect-base
                            "o/r" "origin/main")))))
      (delete-directory root t))))

(ert-deftest test-shipit-stacked-preview-base-never-eats-the-whole-stack ()
  "GIVEN the newest commit itself is the tip of a remote branch with
an open pull request
WHEN the base is detected
THEN the default stands: a base at HEAD would leave a stack of
nothing."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-base-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (shipit-sandbox--git "push" "-q" "origin" "work:refs/heads/part-two")
          (test-stacked-preview--with-pr-backend "part-two"
            (should (equal "origin/main"
                           (shipit-stacked-preview--detect-base
                            "o/r" "origin/main")))))
      (delete-directory root t))))

(ert-deftest test-shipit-stacked-preview-rename-layer-takes-the-new-name ()
  "GIVEN a divided stack in a repository
WHEN a layer with only a derived name is renamed
THEN the new name shows on the row and is what `c' will create
WHEN a layer whose branch exists is renamed
THEN the git branch moves with it."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-rename-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (with-temp-buffer
            (setq default-directory (file-name-as-directory
                                     (expand-file-name "work" root)))
            (setq-local shipit-stacked-preview--branch "work")
            (setq-local shipit-stacked-preview--commits
                        (shipit--commits-in-range "origin/main..HEAD"))
            (setq-local shipit-stacked-preview--boundaries
                        (list (plist-get (nth 1 shipit-stacked-preview--commits)
                                         :sha)))
            (setq-local shipit-stacked-preview--names nil)
            (shipit-stacked-preview--rename
             (car (shipit-stacked-preview--current-layers)) "part-one")
            (should (equal "part-one"
                           (cdr (assq 'branch
                                      (car (shipit-stacked-preview--current-layers))))))
            (shipit-stack--git-lines
             "branch" "part-one"
             (plist-get (nth 1 shipit-stacked-preview--commits) :sha))
            (shipit-stacked-preview--rename
             (car (shipit-stacked-preview--current-layers)) "part-1")
            (should (shipit-stack--rev "part-1"))
            (should-not (shipit-stack--rev "part-one"))
            ;; the move is a mutation, so the process buffer has it
            (should (with-current-buffer shipit-process-buffer-name
                      (string-match-p "branch -m part-one part-1"
                                      (buffer-string))))
            (should (equal "part-1"
                           (cdr (assq 'branch
                                      (car (shipit-stacked-preview--current-layers))))))))
      (delete-directory root t))))

(defmacro test-stacked--with-auth-layer (&rest body)
  "Run BODY in a stacked preview of one layer of two auth commits.
The rename prompt takes the text it is offered."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local shipit-stacked-preview--branch "work")
     (setq-local shipit-stacked-preview--commits
                 '((:sha "a1" :first-line "auth: add flow"
                    :message "auth: add flow\n\nWhy the flow exists.")
                   (:sha "b2" :first-line "auth: verify token"
                    :message "auth: verify token")))
     (setq-local shipit-stacked-preview--boundaries nil)
     (setq-local shipit-stacked-preview--names nil)
     (cl-letf (((symbol-function 'read-string)
                (lambda (_prompt &optional initial &rest _) initial)))
       ,@body)))

(ert-deftest test-shipit-stacked-preview-suggester-names-a-layer ()
  "GIVEN a suggester of branch names
WHEN a name is asked for a layer
THEN the suggester gets the commits' full messages, its answer prefills
the rename prompt, and the layer takes the confirmed name."
  (let* ((descriptions nil)
         (shipit-branch-name-suggester
          (lambda (description callback)
            (push description descriptions)
            (funcall callback "Add-Auth-Flow"))))
    (test-stacked--with-auth-layer
      (shipit-stacked-preview--suggest-name-for
       (car (shipit-stacked-preview--current-layers)) (current-buffer))
      (should (string-match-p "Why the flow exists." (car descriptions)))
      (should (string-match-p "verify token" (car descriptions)))
      (should (equal "add-auth-flow"
                     (cdr (assq 'branch
                                (car (shipit-stacked-preview--current-layers)))))))))

(ert-deftest test-shipit-stacked-preview-without-a-suggester-says-so ()
  "GIVEN no suggester of branch names
WHEN a name is asked for a layer
THEN that is refused, and the layer keeps its name."
  (let ((shipit-branch-name-suggester nil))
    (test-stacked--with-auth-layer
      (should-error (shipit-stacked-preview--suggest-name-for
                     (car (shipit-stacked-preview--current-layers)) (current-buffer))
                    :type 'user-error))))

(provide 'test-shipit-stacked-preview)
;;; test-shipit-stacked-preview.el ends here
