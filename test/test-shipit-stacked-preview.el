;;; test-shipit-stacked-preview.el --- Dividing commits into layers -*- lexical-binding: t; -*-

;;; Commentary:
;; Where the divisions go, and what layers they make.  This is the part
;; that decides what becomes a pull request, so it is worked out from
;; data and tested without a buffer.

;;; Code:

(require 'ert)
(require 'shipit-stacked-preview)
(require 'shipit-pr-github)
(require 'shipit-test-http)
;; Loaded here so the require inside `shipit-stacked-preview-visit-layer'
;; is a no-op and cannot replace the stub this file puts in its place.
(require 'shipit-preview)

(defconst test-stacked-preview--commits
  '((:sha "a1" :first-line "docs: conventions")
    (:sha "a2" :first-line "docs: point tests at the skill")
    (:sha "b1" :first-line "net: abort path")
    (:sha "b2" :first-line "net: tx mux")
    (:sha "c1" :first-line "lint: format"))
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
    (should (equal '("work-docs" "work-net" "work")
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
  (should (equal "work-net"
                 (shipit-stacked-preview--layer-name
                  "work" '((:sha "b1" :first-line "net: abort path"))))))

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
    (should (equal '("already-here" "work-net" "work")
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

(defconst test-stacked--no-pull-requests
  '(("/repos/owner/repo/pulls\\?" 200 []))
  "GitHub's answer when no branch of owner/repo has a pull request.")

(defun test-stacked--layers (&rest branches)
  "Return layers named BRANCHES, bottom first, each holding one commit."
  (mapcar (lambda (branch)
            (list (cons 'branch branch)
                  (cons 'tip "abc1234")
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

(ert-deftest test-shipit-stacked-preview-sends-every-layer-in-one-push ()
  "GIVEN three layers
WHEN every layer is sent
THEN git is asked once for all three, bottom first.  One push is one
lease check for the whole stack, and what git refuses it refuses whole."
  (let ((layers (test-stacked--layers "one" "two" "three"))
        (calls nil))
    (cl-letf (((symbol-function 'shipit-stacked-preview--refuse-when-broken)
               (lambda (_layer) nil))
              ((symbol-function 'shipit-stacked-preview--current-layers)
               (lambda () layers))
              ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
              ((symbol-function 'redisplay) (lambda (&rest _) nil))
              ((symbol-function 'shipit--push-branches)
               (lambda (branches _args) (push branches calls) t)))
      (shipit-stacked-preview--push-through (nth 2 layers) nil)
      (should (equal '(("one" "two" "three")) calls)))))

(ert-deftest test-shipit-stacked-preview-a-refused-push-says-where-to-look ()
  "GIVEN a stack git refuses to take
WHEN every layer is sent
THEN the failure says each push was atomic and points at the process
buffer, which says what landed when the stack went in chunks."
  (let ((layers (test-stacked--layers "one" "two")))
    (cl-letf (((symbol-function 'shipit-stacked-preview--refuse-when-broken)
               (lambda (_layer) nil))
              ((symbol-function 'shipit-stacked-preview--current-layers)
               (lambda () layers))
              ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
              ((symbol-function 'redisplay) (lambda (&rest _) nil))
              ((symbol-function 'shipit--push-branches) (lambda (&rest _) nil)))
      (let ((err (should-error (shipit-stacked-preview--push-through (nth 1 layers) nil)
                               :type 'user-error)))
        (should (string-match-p "atomic" (cadr err)))))))

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
              ((symbol-function 'redisplay) (lambda (&rest _) nil))
              ((symbol-function 'shipit--push-branches)
               (lambda (_branches args) (setq seen args) t)))
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
                ((symbol-function 'shipit--push-branches)
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
                ((symbol-function 'redisplay) (lambda (&rest _) nil))
                ((symbol-function 'shipit--push-branches) (lambda (&rest _) t)))
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
                ((symbol-function 'redisplay) (lambda (&rest _) nil))
                ((symbol-function 'shipit--push-branches)
                 (lambda (branches _args) (setq sent branches) t)))
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
        (should-error (shipit-test-http-answering test-stacked--no-pull-requests
                        (shipit-stacked-preview-visit-layer))
                      :type 'user-error)
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
        (shipit-test-http-answering test-stacked--no-pull-requests
          (shipit-stacked-preview-visit-layer))
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
                ((symbol-function 'shipit--push-branches) (lambda (&rest _) nil)))
        (should-error (shipit-stacked-preview--push-through (nth 1 layers) nil)
                      :type 'user-error)
        (should-not shipit-stacked-preview--sending)))))

(ert-deftest test-shipit-stacked-preview-the-branches-on-their-way-say-so ()
  "GIVEN a whole stack being sent in one push
WHEN the rows are drawn
THEN every layer in it says so, since one push carries them all."
  (with-temp-buffer
    (let ((layers (test-stacked--layers "one" "two")))
      (setq shipit-stacked-preview--sending '("one" "two"))
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) nil)))
        (dolist (layer layers)
          (should (equal "sending…" (shipit-stacked-preview--layer-status layer))))))))

(ert-deftest test-shipit-stacked-preview-a-refused-push-moves-no-layer ()
  "GIVEN two layers on the remote, the upper one moved there since the
last fetch, and both rewritten here
WHEN the stack is sent with a lease
THEN it is refused and neither branch on the remote moves.  Sent one
at a time the lower layer would have gone through and left the stack
half-moved."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-atomic-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (shipit-sandbox--git "branch" "-q" "one" "work~2")
          (shipit-sandbox--git "branch" "-q" "two" "work")
          (should (shipit--push-branches '("one" "two")))
          (let ((one-before (shipit--remote-branch-rev "one"))
                (elsewhere (shipit--branch-rev "main"))
                (layers (test-stacked--layers "one" "two")))
            (shipit-sandbox--git "--git-dir" (expand-file-name "origin.git" root)
                                 "update-ref" "refs/heads/two" elsewhere)
            (shipit-sandbox--git "checkout" "-q" "one")
            (shipit-sandbox--commit "more on one")
            (shipit-sandbox--git "checkout" "-q" "two")
            (shipit-sandbox--git "commit" "-q" "--amend" "--allow-empty" "-m" "two again")
            (with-temp-buffer
              (setq default-directory (file-name-as-directory
                                       (expand-file-name "work" root)))
              (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                         (lambda () layers))
                        ((symbol-function 'shipit-stacked-preview--refuse-when-broken)
                         (lambda (_layer) nil))
                        ((symbol-function 'shipit-stacked-preview--render) (lambda () nil))
                        ((symbol-function 'redisplay) (lambda (&rest _) nil)))
                (should-error (shipit-stacked-preview--push-through
                               (nth 1 layers) '("--force-with-lease"))
                              :type 'user-error)
                (should-not shipit-stacked-preview--sending)))
            (should (equal one-before (shipit--remote-branch-rev "one")))
            (should (equal elsewhere (shipit--remote-branch-rev "two")))))
      (delete-directory root t))))

(ert-deftest test-shipit-stacked-preview-status-says-how-far-a-branch-drifted ()
  "GIVEN a layer whose branch is on the remote
WHEN the branch is rewritten here after the last fetch
THEN its status says how far the two have drifted apart, so a stack
whose remote copies are stale can be read off the rows instead of
looking pushed."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-drift-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (shipit-sandbox--git "branch" "-q" "one" "work")
          (shipit-sandbox--git "push" "-q" "-u" "origin" "one")
          (with-temp-buffer
            (setq default-directory (file-name-as-directory
                                     (expand-file-name "work" root)))
            (let ((layer (car (test-stacked--layers "one"))))
              (should (equal "pushed" (shipit-stacked-preview--layer-status layer)))
              (shipit-sandbox--git "checkout" "-q" "one")
              (shipit-sandbox--git "commit" "-q" "--amend" "--allow-empty" "-m" "one again")
              (should (string-match-p "pushed.*1 ahead, 1 behind"
                                      (shipit-stacked-preview--layer-status layer))))))
      (delete-directory root t))))

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

(defmacro test-stacked--with-github-answering (prs &rest body)
  "Run BODY against the GitHub backend, the network answering PRS.
PRS maps a branch to the number of its open pull request.  Only the
network is stood in for, so the branch lookup and both of the caches
in front of it run for real."
  (declare (indent 1))
  `(let ((shipit-pr-backend 'github)
         (shipit--cached-branch-prs (make-hash-table :test 'equal))
         (shipit-gh-etag--refresh-cache (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'shipit--github-token) (lambda () "token"))
               ((symbol-function 'shipit-gh-etag-get-json)
                (lambda (_endpoint params &rest _)
                  (let* ((head (cdr (assq 'head params)))
                         (branch (car (last (split-string head ":"))))
                         (number (cdr (assoc branch ,prs))))
                    ;; A list, as the ETag layer parses arrays.
                    (list :json (and number
                                     (list `((number . ,number)
                                             (head . ((ref . ,branch)))
                                             (base . ((ref . "main"))))))))))
               ((symbol-function 'shipit--fetch-commits-and-files-parallel-sync)
                (lambda (_repo _number pr) pr)))
       ,@body)))

(ert-deftest test-shipit-stacked-preview-register-asks-again ()
  "GIVEN two layers looked up before either had a pull request
AND both have one now, made outside this buffer
WHEN the stack is registered
THEN it asks again and groups both.  It used to believe the
remembered answer and refuse with \"0 has been made\"."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two"))
          (grouped nil))
      (test-stacked--with-github-answering '(("one" . 11) ("two" . 12))
        (dolist (branch '("one" "two"))
          (puthash (shipit-cache-branch-pr-key "owner/repo" branch)
                   nil shipit--cached-branch-prs))
        (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                   (lambda () layers))
                  ((symbol-function 'shipit-stacked-preview--group)
                   (lambda (numbers) (setq grouped numbers)))
                  ((symbol-function 'shipit-stacked-preview--render) #'ignore))
          (shipit-stacked-preview-register)))
      (should (equal '(11 12) grouped)))))

(ert-deftest test-shipit-stacked-preview-register-sees-past-a-stale-request ()
  "GIVEN a branch whose lookup was answered empty before its pull
request existed, and that answer still held by the per-refresh request
cache, which a PR buffer refresh opens and nothing closes
WHEN the stack is registered
THEN the pull request is found.  Dropping the branch's own answer was
not enough: the request underneath it was served from that cache."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two"))
          (grouped nil))
      (test-stacked--with-github-answering '(("one" . 11) ("two" . 12))
        (dolist (branch '("one" "two"))
          (dolist (head (list (concat "owner:" branch) branch))
            (puthash (concat "owner/repo/repos/owner/repo/pulls"
                             (shipit-gh-etag--qstr
                              `((state . ,(if shipit-magit-show-closed-prs "all" "open"))
                                (head . ,head))))
                     '(:status 200 :json nil)
                     shipit-gh-etag--refresh-cache)))
        (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                   (lambda () layers))
                  ((symbol-function 'shipit-stacked-preview--group)
                   (lambda (numbers) (setq grouped numbers)))
                  ((symbol-function 'shipit-stacked-preview--render) #'ignore))
          (shipit-stacked-preview-register)))
      (should (equal '(11 12) grouped)))))

(ert-deftest test-shipit-stacked-preview-counts-the-pr-it-sits-on ()
  "GIVEN a stack raised over a branch whose pull request is open
WHEN the submitted run is worked out
THEN that pull request comes first.  Measuring from it leaves it out
of the layers, but it is the bottom of the stack GitHub groups: two
pull requests made one on the other otherwise never formed a stack."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo"
          shipit-stacked-preview--base "origin/one")
    (let ((layers (test-stacked--layers "two")))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stack--base-ref) (lambda () "origin/main"))
                ((symbol-function 'shipit-stacked-preview--pr-number)
                 (lambda (branch) (pcase branch ("one" 11) ("two" 12)))))
        (should (equal '(11 12) (shipit-stacked-preview--submitted-numbers)))))))

(ert-deftest test-shipit-stacked-preview-default-branch-is-no-pr-to-sit-on ()
  "GIVEN a stack raised over the default branch
AND some pull request with the default branch as its head, as one from
master into a release branch has
WHEN the submitted run is worked out, and the base line drawn
THEN that pull request is no part of it.  The base line used to show
it, as if the stack sat on it."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo"
          shipit-stacked-preview--base "origin/main")
    (let ((layers (test-stacked--layers "one" "two")))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stack--base-ref) (lambda () "origin/main"))
                ((symbol-function 'shipit-stack--current-branch) (lambda () "two"))
                ((symbol-function 'shipit-stack--git-lines) (lambda (&rest _) nil))
                ((symbol-function 'shipit-stacked-preview--pr-number)
                 (lambda (branch) (pcase branch ("main" 99) ("one" 11) ("two" 12)))))
        (should (equal '(11 12) (shipit-stacked-preview--submitted-numbers)))
        (shipit-stacked-preview--insert-refs)
        (should-not (string-match-p "#99" (buffer-string)))))))

(ert-deftest test-shipit-stacked-preview-register-groups-with-the-pr-below ()
  "GIVEN two open pull requests, the second based on the first's branch,
so the stack view sits on the first and holds the second as its layer
WHEN the stack is registered
THEN both are grouped, the one it sits on first.  This is the report:
the view counted one and refused with \"1 has been made\"."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo"
          shipit-stacked-preview--base "origin/one")
    (let ((layers (test-stacked--layers "two"))
          (grouped nil))
      (test-stacked--with-github-answering '(("one" . 11) ("two" . 12))
        (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                   (lambda () layers))
                  ((symbol-function 'shipit-stack--base-ref) (lambda () "origin/main"))
                  ((symbol-function 'shipit-stacked-preview--group)
                   (lambda (numbers) (setq grouped numbers)))
                  ((symbol-function 'shipit-stacked-preview--render) #'ignore))
          (shipit-stacked-preview-register)))
      (should (equal '(11 12) grouped)))))

(ert-deftest test-shipit-stacked-preview-refresh-asks-again ()
  "GIVEN layers whose branches were looked up before they had pull
requests
WHEN the buffer is refreshed
THEN what was remembered about those branches is dropped, so a pull
request made anywhere, on the web page included, shows up."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo"
          shipit-stacked-preview--base "origin/main")
    (let ((layers (test-stacked--layers "one" "two"))
          (shipit--cached-branch-prs (make-hash-table :test 'equal)))
      (dolist (branch '("one" "two" "unrelated"))
        (puthash (shipit-cache-branch-pr-key "owner/repo" branch)
                 nil shipit--cached-branch-prs))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit--commits-in-range) (lambda (_) nil))
                ((symbol-function 'shipit-stacked-preview--fetch) #'ignore)
                ((symbol-function 'shipit-stacked-preview--render) #'ignore))
        (shipit-stacked-preview-refresh))
      (should (eq 'absent (gethash (shipit-cache-branch-pr-key "owner/repo" "one")
                                   shipit--cached-branch-prs 'absent)))
      (should (eq 'absent (gethash (shipit-cache-branch-pr-key "owner/repo" "two")
                                   shipit--cached-branch-prs 'absent)))
      (should-not (eq 'absent (gethash (shipit-cache-branch-pr-key "owner/repo" "unrelated")
                                       shipit--cached-branch-prs 'absent))))))

(ert-deftest test-shipit-stacked-preview-register-says-none-properly ()
  "GIVEN a stack with no pull requests at all
WHEN it is registered
THEN the refusal reads as English."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((layers (test-stacked--layers "one" "two")))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--forget-layer-prs) #'ignore)
                ((symbol-function 'shipit-stacked-preview--pr-number) (lambda (_) nil)))
        (let ((raised (condition-case err (shipit-stacked-preview-register)
                        (user-error (error-message-string err)))))
          (should (string-match-p "0 have been made" raised)))))))

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

(ert-deftest test-shipit-stacked-preview-base-skips-the-stacks-own-layers ()
  "GIVEN a layer of this stack: a local branch below HEAD, pushed, with
an open pull request
WHEN the base is detected with that branch among the stack's own
THEN the default stands: a stack opened again after its layers were
submitted still holds every layer."
  (require 'shipit-sandbox)
  (let ((root (make-temp-file "shipit-base-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory
                                  (shipit-sandbox-build root))))
          (shipit-sandbox--git "branch" "part-one" "work~3")
          (shipit-sandbox--git "push" "-q" "origin" "part-one")
          (test-stacked-preview--with-pr-backend "part-one"
            (should (equal "origin/main"
                           (shipit-stacked-preview--detect-base
                            "o/r" "origin/main" '("part-one"))))))
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
  "Run BODY in a stacked preview of one layer of two auth commits."
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

(ert-deftest test-shipit-stacked-preview-statuses-are-fetched-once ()
  "GIVEN layers where two branches have pull requests and one has none
WHEN their statuses are asked for
THEN the backend is asked once with only the numbers that exist, and
its answer comes back as it stands, so one query serves every row."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let* ((layers (test-stacked--layers "one" "two" "three"))
           (asked nil)
           (shipit-pr-backend 'test-preview)
           (shipit-pr-backends
            (list (cons 'test-preview
                        (list :name "Preview"
                              :fetch-stack-statuses
                              (lambda (_config numbers)
                                (push numbers asked)
                                '((101 . ((review . "approved"))))))))))
      (cl-letf (((symbol-function 'shipit-stack-plan--pr-for-branch)
                 (lambda (_repo branch)
                   (cdr (assoc branch '(("one" . 101) ("three" . 103)))))))
        (should (equal '((101 . ((review . "approved"))))
                       (shipit-stacked-preview--statuses layers)))
        (should (equal '((101 103)) asked))))))

(ert-deftest test-shipit-stacked-preview-status-carries-the-pr-statuses ()
  "GIVEN a layer whose branch has a pull request with checks and a
review decision
WHEN its status is drawn with SVG icons off
THEN the glyphs ride behind the pull request number, so a red or
waiting layer is seen from the preview without opening it."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((shipit-use-svglib-icons nil)
          (layer (car (test-stacked--layers "one"))))
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
                ((symbol-function 'shipit-stack-plan--pr-for-branch)
                 (lambda (&rest _) 4471)))
        (let ((status (shipit-stacked-preview--layer-status
                       layer '((4471 . ((checks . "success")
                                        (review . "changes_requested")))))))
          (should (string-match-p "PR #4471" status))
          (should (string-match-p "✅" status))
          (should (string-match-p "❌" status)))))))

(ert-deftest test-shipit-stacked-preview-rows-carry-the-pr-statuses ()
  "GIVEN a stack of two layers with pull requests and a backend
answering their statuses
WHEN the buffer is drawn with SVG icons off
THEN each row wears its own glyphs: the approved layer the green
check, the one still waiting the hourglass."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
    (setq shipit-stacked-preview--repo "owner/repo"
          shipit-stacked-preview--base "origin/master")
    (let ((shipit-use-svglib-icons nil)
          (layers (test-stacked--layers "one" "two"))
          (shipit-pr-backend 'test-preview)
          (shipit-pr-backends
           (list (cons 'test-preview
                       (list :name "Preview"
                             :fetch-stack-statuses
                             (lambda (&rest _)
                               '((101 . ((review . "approved")))
                                 (102 . ((review . "review_required"))))))))))
      (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                 (lambda () layers))
                ((symbol-function 'shipit-stacked-preview--insert-header)
                 (lambda () nil))
                ((symbol-function 'shipit-stacked-preview--insert-problems)
                 (lambda () nil))
                ((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
                ((symbol-function 'shipit-stack-plan--pr-for-branch)
                 (lambda (_repo branch)
                   (cdr (assoc branch '(("one" . 101) ("two" . 102))))))
                ((symbol-function 'shipit--insert-processed-commit-section)
                 (lambda (&rest _) nil)))
        (shipit-stacked-preview--render)
        (let* ((lines (split-string (buffer-string) "\n"))
               (row-one (cl-find-if
                         (lambda (l) (string-match-p "PR #101" l)) lines))
               (row-two (cl-find-if
                         (lambda (l) (string-match-p "PR #102" l)) lines)))
          (should row-one)
          (should row-two)
          (should (string-match-p "✅" row-one))
          (should (string-match-p "⏳" row-two)))))))

(defmacro test-stacked--with-drawn-stack (buffer layers prs statuses &rest body)
  "Draw LAYERS in a stacked preview bound to BUFFER, and run BODY there.
LAYERS is read again each time the preview asks for its layers, so
BODY can set it and redraw.  PRS maps a branch to its pull request,
and STATUSES is what the forge answers when asked for their statuses.
Every branch stands on one commit here and on the remote.  Only git
and the forge are stood in for, besides the header and the commit rows,
which ask git for more."
  (declare (indent 4))
  `(let ((,buffer (generate-new-buffer " *test-stacked-preview*"))
         (shipit-use-svglib-icons nil)
         (shipit-pr-backend 'test-preview)
         (shipit-pr-backends
          (list (cons 'test-preview
                      (list :name "Preview"
                            :fetch-stack-statuses (lambda (&rest _) ,statuses))))))
     (unwind-protect
         (cl-letf (((symbol-function 'shipit-stacked-preview--current-layers)
                    (lambda () ,layers))
                   ((symbol-function 'shipit-stacked-preview--insert-header) #'ignore)
                   ((symbol-function 'shipit-stacked-preview--insert-problems) #'ignore)
                   ((symbol-function 'shipit--insert-processed-commit-section) #'ignore)
                   ((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
                   ((symbol-function 'shipit-stack--git-lines) (lambda (&rest _) nil))
                   ((symbol-function 'shipit-stack-plan--pr-for-branch)
                    (lambda (_repo branch) (cdr (assoc branch ,prs)))))
           (with-current-buffer ,buffer
             (shipit-stacked-preview-mode)
             (setq shipit-stacked-preview--repo "owner/repo"
                   shipit-stacked-preview--base "origin/master")
             (shipit-stacked-preview--render)
             ,@body))
       (kill-buffer ,buffer))))

;;;; The state of a layer, as data

(ert-deftest test-shipit-stacked-preview-state-names-how-far-a-layer-got ()
  "GIVEN a layer with no branch, then a branch only here, then a branch
on the remote at the same commit
WHEN its state is asked for
THEN it names the stage as data, which the row and the stack canvas
both draw from."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (let ((layer (car (test-stacked--layers "one"))))
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) nil)))
        (should (equal '(:state no-branch)
                       (shipit-stacked-preview--layer-state layer))))
      (cl-letf (((symbol-function 'shipit-stack--rev)
                 (lambda (ref) (unless (string-prefix-p "origin/" ref) "abc"))))
        (should (equal '(:state not-pushed)
                       (shipit-stacked-preview--layer-state layer))))
      (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
                ((symbol-function 'shipit-stack--git-lines) (lambda (&rest _) nil))
                ((symbol-function 'shipit-stack-plan--pr-for-branch)
                 (lambda (&rest _) nil)))
        (should (equal '(:state pushed :drift nil :moved nil)
                       (shipit-stacked-preview--layer-state layer)))))))

(ert-deftest test-shipit-stacked-preview-state-of-a-branch-on-its-way ()
  "GIVEN a layer whose branch is being sent
WHEN its state is asked for
THEN it is sending, whatever the branch was before the push began."
  (with-temp-buffer
    (setq shipit-stacked-preview--sending "one")
    (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) nil)))
      (should (equal '(:state sending)
                     (shipit-stacked-preview--layer-state
                      (car (test-stacked--layers "one"))))))))

(ert-deftest test-shipit-stacked-preview-state-of-a-branch-changed-since-its-push ()
  "GIVEN a layer on the remote whose branch was rewritten here
WHEN its state is asked for
THEN it is drifted, and says how far the two copies came apart."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
              ((symbol-function 'shipit-stack--git-lines) (lambda (&rest _) '("1 2")))
              ((symbol-function 'shipit-stack-plan--pr-for-branch)
               (lambda (&rest _) nil)))
      (should (equal '(:state drifted :drift "2 ahead, 1 behind" :moved nil)
                     (shipit-stacked-preview--layer-state
                      (car (test-stacked--layers "one"))))))))

(ert-deftest test-shipit-stacked-preview-state-of-a-layer-with-a-pull-request ()
  "GIVEN a layer whose branch has a pull request with failing checks
and an approval
WHEN its state is asked for with the stack's statuses
THEN it carries the number, the checks and the review."
  (with-temp-buffer
    (setq shipit-stacked-preview--repo "owner/repo")
    (cl-letf (((symbol-function 'shipit-stack--rev) (lambda (_) "abc"))
              ((symbol-function 'shipit-stack--git-lines) (lambda (&rest _) nil))
              ((symbol-function 'shipit-stack-plan--pr-for-branch)
               (lambda (&rest _) 4471)))
      (should (equal '(:state pull-request :number 4471 :checks "failure"
                              :review "approved" :drift nil :moved nil)
                     (shipit-stacked-preview--layer-state
                      (car (test-stacked--layers "one"))
                      '((4471 . ((checks . "failure") (review . "approved"))))))))))

;;;; The push menu, from here or from the stack canvas

(ert-deftest test-shipit-stacked-preview-push-menu-opens-on-the-layer-at-point ()
  "GIVEN point on a layer's row, and then below every layer
WHEN the push menu is opened
THEN it records this buffer with that layer, and then with no layer."
  (let ((layers (test-stacked--layers "one" "two"))
        (scope 'unset))
    (test-stacked--with-drawn-stack preview layers nil nil
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (&rest args) (setq scope (plist-get (nthcdr 3 args) :scope)))))
        (goto-char (shipit-stacked-preview--layer-position "one"))
        (call-interactively #'shipit-stacked-preview-push)
        (should (equal (cons preview "one") scope))
        (goto-char (point-max))
        (call-interactively #'shipit-stacked-preview-push)
        (should (equal (cons preview nil) scope))))))

(ert-deftest test-shipit-stacked-preview-push-menu-pushes-the-layer-it-opened-on ()
  "GIVEN the push menu opened for a layer of a preview
WHEN its layer suffix runs while another buffer is current
THEN it pushes that layer from that preview.  A suffix runs in the
buffer of the selected window, and the stack canvas opens the menu from
its own buffer."
  (let ((layers (test-stacked--layers "one" "two"))
        (preview (generate-new-buffer " *test-push-scope*"))
        (pushed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda (&rest _) (cons preview "one")))
                  ((symbol-function 'shipit-stacked-preview--current-layers)
                   (lambda ()
                     (unless (eq (current-buffer) preview)
                       (error "Asked for the layers outside the preview"))
                     layers))
                  ((symbol-function 'shipit-stacked-preview--refuse-when-broken) #'ignore)
                  ((symbol-function 'shipit-stacked-preview--render) #'ignore)
                  ((symbol-function 'redisplay) #'ignore)
                  ((symbol-function 'shipit--push-branches)
                   (lambda (branches _args) (setq pushed branches) t)))
          (with-temp-buffer
            (shipit-stacked-preview-push-layer nil))
          (should (equal '("one") pushed)))
      (kill-buffer preview))))

;;;; The canvas key

(ert-deftest test-shipit-stacked-preview-canvas-key-is-offered ()
  "GIVEN the keys this buffer advertises
WHEN they are read
THEN v, which draws the stack on a canvas, is among them and bound."
  (should (assoc "v" (apply #'append shipit-stacked-preview--key-columns)))
  (should (eq 'shipit-stacked-preview-canvas
              (lookup-key shipit-stacked-preview-mode-map (kbd "v")))))

(ert-deftest test-shipit-stacked-preview-canvas-key-without-a-canvas-says-so ()
  "GIVEN an Emacs without the canvas
WHEN v is pressed in the preview
THEN it says the view needs Emacs 32, and signals nothing."
  (with-temp-buffer
    (let ((said nil))
      (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format-message fmt args)))))
        (shipit-stacked-preview-canvas)
        (should (string-match-p "Emacs 32" said))))))

;;;; What the remote did since the last fetch

(require 'shipit-sandbox)

(defun test-stacked--other-clone (root)
  "Clone the origin under ROOT to a second working directory and return it.
Where a colleague, or a bot on the remote, does its work: a push from
there moves the remote and leaves what `work\=' knows of it stale, the
way a push from this directory would not."
  (let ((other (expand-file-name "other" root)))
    (let ((default-directory (file-name-as-directory root)))
      (shipit-sandbox--git "clone" "-q" (expand-file-name "origin.git" root) other))
    (let ((default-directory (file-name-as-directory other)))
      (shipit-sandbox--git "config" "user.email" "other@example.invalid")
      (shipit-sandbox--git "config" "user.name" "Other"))
    (file-name-as-directory other)))

(defun test-stacked--open-pushed-stack ()
  "Open the sandbox stack, give its layers branches and push them.
Returns the preview buffer."
  (shipit-stacked-preview)
  (with-current-buffer (format "*shipit-stacked-preview: %s*" "work")
    (shipit-stacked-preview-create-branches)
    (shipit-stacked-preview--push-through
     (car (last (shipit-stacked-preview--current-layers))) nil)
    (current-buffer)))

(defun test-stacked--rebase-the-stack-on-the-remote (root)
  "Advance main and rebase the pushed stack onto it, from a second clone.
What a bot does on the remote when the pull request under a stack
merges.  Returns the commit the stack stands on there afterwards."
  (let ((default-directory (test-stacked--other-clone root)))
    (shipit-sandbox--git "checkout" "-q" "main")
    (shipit-sandbox--commit "merged below the stack")
    (shipit-sandbox--git "push" "-q" "origin" "main")
    (shipit-sandbox--git "checkout" "-q" "work-parser")
    (shipit-sandbox--git "checkout" "-q" "work")
    (shipit-sandbox--git "rebase" "-q" "--update-refs" "main")
    (shipit-sandbox--git "push" "-q" "--force" "origin" "work-parser" "work")
    (car (shipit-stack--git-lines "rev-parse" "main"))))

(ert-deftest test-shipit-stacked-preview-refresh-fetches-what-the-remote-holds ()
  "GIVEN a pushed stack, and a commit pushed to its top layer from elsewhere
WHEN the preview is refreshed
THEN the top row says the remote has a commit this branch has not, so a
push from elsewhere is seen without a fetch by hand.  Before the refresh
the row still reads as pushed: the refs here are what the last fetch
left."
  (shipit-sandbox-with-repo
    (let ((buffer (test-stacked--open-pushed-stack)))
      (let ((default-directory (test-stacked--other-clone root)))
        (shipit-sandbox--git "checkout" "-q" "work")
        (shipit-sandbox--commit "from a reviewer")
        (shipit-sandbox--git "push" "-q" "origin" "work"))
      (with-current-buffer buffer
        (let ((top (shipit-stacked-preview--layer-named "work")))
          (should (equal "pushed" (shipit-stacked-preview--layer-status top)))
          (shipit-stacked-preview-refresh)
          (should (string-match-p "1 behind, new commits on the remote"
                                  (shipit-stacked-preview--layer-status top))))))))

(ert-deftest test-shipit-stacked-preview-says-when-the-remote-copies-were-rebased ()
  "GIVEN a pushed stack whose copies on the remote were rebased onto a newer base
WHEN the preview is refreshed
THEN each row says so beside how far the copies drifted: the remote did
not merely gain commits, it moved the stack, and the answer is to move
this one the same way rather than to pull."
  (shipit-sandbox-with-repo
    (let ((buffer (test-stacked--open-pushed-stack)))
      (test-stacked--rebase-the-stack-on-the-remote root)
      (with-current-buffer buffer
        (shipit-stacked-preview-refresh)
        (dolist (branch '("work-parser" "work"))
          (should (string-match-p "behind, rebased on the remote"
                                  (shipit-stacked-preview--layer-status
                                   (shipit-stacked-preview--layer-named branch)))))))))

(ert-deftest test-shipit-stacked-preview-rebases-onto-where-the-remote-copies-stand ()
  "GIVEN a stack whose copies on the remote were rebased onto a newer base
WHEN b is pressed
THEN magit is asked to rebase the commits over the stack\='s base onto the
commit the copies stand on, with every layer\='s branch following and
uncommitted work stashed around it, so the stack lands where the remote
put it.  A conflict is magit\='s to show, and this buffer redraws once
magit has refreshed after the rebase, and only that once."
  (shipit-sandbox-with-repo
    (let* ((buffer (test-stacked--open-pushed-stack))
           (new-base (test-stacked--rebase-the-stack-on-the-remote root))
           (hooks-before (length magit-post-refresh-hook))
           (ran nil)
           (redrawn 0))
      (with-current-buffer buffer
        (shipit-stacked-preview-refresh)
        (cl-letf (((symbol-function 'magit-run-git-sequencer)
                   (lambda (&rest args) (setq ran (flatten-tree args)))))
          (shipit-stacked-preview-rebase-onto-remote-base))
        (should (equal (list "rebase" "--onto" new-base "origin/main"
                             "--update-refs" "--autostash")
                       ran))
        (cl-letf (((symbol-function 'shipit-stacked-preview-refresh)
                   (lambda () (setq redrawn (1+ redrawn)))))
          (run-hooks 'magit-post-refresh-hook)
          (run-hooks 'magit-post-refresh-hook))
        (should (equal 1 redrawn))
        (should (equal hooks-before (length magit-post-refresh-hook)))))))

(ert-deftest test-shipit-stacked-preview-keeps-its-layers-through-the-rebase ()
  "GIVEN a stack whose copies on the remote were rebased onto a newer base
WHEN the stack is rebased onto that base with every layer\='s branch
following, as b has magit do, and the preview is refreshed
THEN the preview still shows both layers, divided where the branches now
stand, although every commit has a new id."
  (shipit-sandbox-with-repo
    (let* ((buffer (test-stacked--open-pushed-stack))
           (new-base (test-stacked--rebase-the-stack-on-the-remote root)))
      (with-current-buffer buffer
        (shipit-stacked-preview-refresh)
        (shipit-sandbox--git "rebase" "-q" "--onto" new-base "origin/main"
                             "--update-refs" "--autostash")
        (shipit-stacked-preview-refresh)
        (should (equal '("work-parser" "work")
                       (mapcar (lambda (layer) (cdr (assq 'branch layer)))
                               (shipit-stacked-preview--current-layers))))))))

(ert-deftest test-shipit-stacked-preview-refuses-to-rebase-with-nowhere-to-go ()
  "GIVEN a stack whose copies on the remote stand where it does
WHEN b is pressed
THEN it says there is nothing to rebase onto and runs no rebase."
  (shipit-sandbox-with-repo
    (with-current-buffer (test-stacked--open-pushed-stack)
      (cl-letf (((symbol-function 'magit-run-git-sequencer)
                 (lambda (&rest _) (error "A rebase ran"))))
        (should-error (shipit-stacked-preview-rebase-onto-remote-base)
                      :type 'user-error)))))

(ert-deftest test-shipit-stacked-preview-says-when-a-layer-merged ()
  "GIVEN a pushed stack whose bottom layer was merged by rebasing it onto the
base, its branch deleted on the remote with the merge
WHEN the preview is refreshed
THEN the bottom row says merged, not \"not pushed\": its commits are on
the base under other ids, which is what a rebase merge leaves, and the
layer is done rather than waiting to be sent."
  (shipit-sandbox-with-repo
    (let ((buffer (test-stacked--open-pushed-stack)))
      (let ((default-directory (test-stacked--other-clone root)))
        (shipit-sandbox--git "checkout" "-q" "main")
        (shipit-sandbox--git "cherry-pick" "main..origin/work-parser")
        (shipit-sandbox--git "push" "-q" "origin" "main" ":work-parser"))
      (with-current-buffer buffer
        (shipit-stacked-preview-refresh)
        (should (string-match-p "merged into main"
                                (shipit-stacked-preview--layer-status
                                 (shipit-stacked-preview--layer-named "work-parser"))))
        (should (equal "pushed"
                       (shipit-stacked-preview--layer-status
                        (shipit-stacked-preview--layer-named "work"))))))))

(ert-deftest test-shipit-stacked-preview-says-when-the-fetch-fails ()
  "GIVEN a remote that cannot be reached
WHEN the preview is refreshed
THEN the buffer says the fetch failed and still draws the stack from the
refs it has, so working away from the remote is no worse than before,
only said."
  (shipit-sandbox-with-repo
    (let ((buffer (test-stacked--open-pushed-stack)))
      (shipit-sandbox--git "remote" "set-url" "origin" (expand-file-name "gone.git" root))
      (with-current-buffer buffer
        (shipit-stacked-preview-refresh)
        (should (string-match-p "Fetch from origin failed" (buffer-string)))
        (should (shipit-stacked-preview--layer-named "work"))))))

(provide 'test-shipit-stacked-preview)
;;; test-shipit-stacked-preview.el ends here
