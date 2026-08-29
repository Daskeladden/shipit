;;; test-shipit-stacked-preview.el --- Dividing commits into layers -*- lexical-binding: t; -*-

;;; Commentary:
;; Where the divisions go, and what layers they make.  This is the part
;; that decides what becomes a pull request, so it is worked out from
;; data and tested without a buffer.

;;; Code:

(require 'ert)
(require 'shipit-stacked-preview)
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
      (cl-letf (((symbol-function 'shipit-stacked-preview--render)
                 (lambda () (setq redrawn t)))
                ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil)))
        (funcall (shipit-stacked-preview--return-to (current-buffer)) 42)
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

(provide 'test-shipit-stacked-preview)
;;; test-shipit-stacked-preview.el ends here
