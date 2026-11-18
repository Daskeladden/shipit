;;; shipit-stacked-preview.el --- Preview a stack before creating it -*- lexical-binding: t; -*-

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
;; Where a line of commits is divided into layers, before any of them
;; becomes a pull request.
;;
;; A layer is an ordinary pull request whose base is the layer below,
;; so this buffer does not open them.  It decides where the divisions
;; go, puts a branch on each, and hands each layer to the preview,
;; which is where a title and description are written and where the
;; pull request is submitted.  Coming back, it says which layers exist
;; and which are still to do.
;;
;; Dividing costs nothing to undo: a layer is a branch pointing at a
;; commit that is already there.  Nothing is rebased and no commit
;; moves.

;;; Code:

(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-canvas)
(require 'shipit-cache)
(require 'shipit-stack)
(require 'shipit-sections)
(require 'shipit-pr-sections)
(require 'shipit-stack-plan)
(require 'magit-section)
(require 'transient)

;; The preview is the other half of this flow: a layer is described and
;; submitted there.  It is reached through its autoload rather than a
;; require, so the two buffers stay independent of one another.
(declare-function shipit-preview-branch "shipit-preview"
                  (repo branch base-ref &rest keys))
(declare-function shipit-open-pr-buffer "shipit-buffer"
                  (pr-number &optional repo backend-id backend-config))
;; The stack canvas draws this buffer on an Emacs 32 canvas.  It is
;; loaded by the key that opens it, once the canvas is known to work.
(declare-function shipit-stack-canvas-show "shipit-stack-canvas" (preview))

(declare-function shipit--insert-processed-commit-section "shipit-pr-sections")
(declare-function shipit--stack-status-glyphs "shipit-render" (statuses))
(declare-function shipit-pr--stack-statuses-quietly "shipit-pr-backends"
                  (repo numbers))

(defvar-local shipit-stacked-preview--repo nil
  "Repository these commits belong to.")

(defvar-local shipit-stacked-preview--sending nil
  "Branch on its way to the remote, or nil.
Read by the layer rows, so the one being sent says so while it is
happening rather than afterwards.")

(defvar-local shipit-stacked-preview--base nil
  "Ref the lowest layer is measured from.")

(defvar-local shipit-stacked-preview--commits nil
  "The commits being divided, oldest first.")

(defvar-local shipit-stacked-preview--boundaries nil
  "SHAs of the commits a layer ends at, excluding the last.
The final layer always ends at the newest commit, so it needs no
entry; a boundary is a division, and the top of the pile is not one.")

(defvar-local shipit-stacked-preview--branch nil
  "Branch these commits are on, which stays the name of the top layer.")

(defvar-local shipit-stacked-preview--names nil
  "Commit SHA to the branch already marking that layer.
A stack divided before keeps the names it was given.")

(defvar-local shipit-stacked-preview--merged-prs nil
  "Branch to its merged pull request as the forge answered, or to nil.
Kept until the next refresh.")

(defvar-local shipit-stacked-preview--fetch-problem nil
  "Why the last fetch from the remote failed, or nil when it did not.")

(defvar-local shipit-stacked-preview--states-shown nil
  "The state of each layer the rows were last drawn with, by branch.
Working out a state asks git and the forge.  The stack canvas reads
this buffer again after every redraw, so it reads these instead of
asking again.")

(defvar-local shipit-stacked-preview--layers-shown nil
  "The layers the buffer was last drawn with.
Naming a new layer asks git, so a drawing for a new window width
reads these.")

(defvar-local shipit-stacked-preview--refs-shown nil
  "Where HEAD, its upstream and the base pointed when last looked up.
A plist, as `shipit-stacked-preview--refs' returns it.")

(defvar-local shipit-stacked-preview--problems-shown nil
  "What stopped the stack being submitted when last looked up.")

(defvar-local shipit-stacked-preview--known-statuses nil
  "Pull request number to its checks and review, as the forge told them.
A number the forge told nothing about maps to nil.  Kept until the
next refresh: the checks and reviews change on the forge, not with a
division or an undo here.")

(defvar-local shipit-stacked-preview--drawn-width nil
  "The width that the commit rows were last drawn for.")

(defvar-local shipit-stacked-preview--fit-timer nil
  "The timer that draws the buffer for a new window width, or nil.")

(defconst shipit-stacked-preview--fit-delay 0.2
  "Seconds without a change of width before the buffer fits its window.
A drag of a window border changes the width many times a second, and
the buffer is drawn once, after the drag.")

(defvar-local shipit-stacked-preview--stack-to-rebuild nil
  "Non-nil when shipit could not build the stack in the order of the layers.
Set when a step of the rebuild after a split failed, and cleared when
the stack is built.  Kept in the buffer, so a preview opened again
does not know it.")

;;;; Working out the layers

(defun shipit-stacked-preview--layer-name (branch commits)
  "Return a branch name for a layer of COMMITS under BRANCH.
Named after what its commits have in common, when they say."
  (let ((prefix (shipit-stack--commit-prefix
                 (or (plist-get (car (last commits)) :first-line) ""))))
    (if prefix
        (format "%s-%s" branch (shipit-stack--slug prefix))
      (format "%s-%s" branch
              (shipit-stack--slug
               (or (plist-get (car (last commits)) :first-line) "layer"))))))

(defun shipit-stacked-preview--commit-groups (commits boundaries)
  "Divide COMMITS, oldest first, into lists that end at BOUNDARIES."
  (let (groups current)
    (dolist (commit commits)
      (push commit current)
      (when (member (plist-get commit :sha) boundaries)
        (push (nreverse current) groups)
        (setq current nil)))
    (when current (push (nreverse current) groups))
    (nreverse groups)))

(defun shipit-stacked-preview--name-taken-p (name tip taken)
  "Return non-nil when NAME cannot be the branch of the layer that ends at TIP.
TAKEN are the names of the other layers.  A branch of that name at
another commit is taken too: a layer split off below a layer with a
branch would otherwise get the name of that branch.  A branch at TIP
is the layer\='s own, made by `b c\='."
  (or (member name taken)
      (let ((rev (shipit-stack--rev (concat "refs/heads/" name))))
        (and rev (not (equal rev tip))))))

(defun shipit-stacked-preview--free-name (name tip taken)
  "Return NAME, or NAME with -2, -3 and on, whichever is free first.
TIP and TAKEN are as for `shipit-stacked-preview--name-taken-p'."
  (let ((candidate name)
        (count 1))
    (while (shipit-stacked-preview--name-taken-p candidate tip taken)
      (setq count (1+ count)
            candidate (format "%s-%d" name count)))
    candidate))

(defun shipit-stacked-preview--group-branch (group tip branch names taken)
  "Return the branch of the layer of GROUP, which ends at TIP.
BRANCH is the branch of the stack, NAMES maps a tip to the branch
already marking it, and TAKEN are the names of the other layers."
  (or (cdr (assoc tip names))
      (shipit-stacked-preview--free-name
       (shipit-stacked-preview--layer-name branch group) tip taken)))

(defun shipit-stacked-preview--layers (commits boundaries branch &optional names)
  "Divide COMMITS into layers ending at BOUNDARIES.
COMMITS are oldest first.  The last layer keeps BRANCH, since the work
is already on it and it needs no new pointer.  NAMES maps a layer\='s
last commit to the branch already marking it, so a stack that has been
divided before keeps the names it was given rather than being renamed
from its commit subjects."
  (let* ((groups (shipit-stacked-preview--commit-groups commits boundaries))
         (last (car (last groups)))
         (taken (cons branch (mapcar #'cdr names)))
         (layers nil))
    (dolist (group groups (nreverse layers))
      (let* ((top (eq group last))
             (tip (plist-get (car (last group)) :sha))
             (name (if top
                       branch
                     (shipit-stacked-preview--group-branch
                      group tip branch names taken))))
        (push name taken)
        (push `((commits . ,group) (tip . ,tip) (existing . ,top)
                (branch . ,name))
              layers)))))

(defun shipit-stacked-preview--current-layers ()
  "Return the layers this buffer currently describes."
  (shipit-stacked-preview--layers shipit-stacked-preview--commits
                                  shipit-stacked-preview--boundaries
                                  shipit-stacked-preview--branch
                                  shipit-stacked-preview--names))

(defun shipit-stacked-preview--from-existing-stack ()
  "Return (BOUNDARIES . NAMES) from the branches already dividing this
branch, or nil when none do.
Reuses the detection the rest of shipit uses, so a stack prepared
elsewhere, or divided here earlier, opens as it stands rather than
being proposed again from the commit subjects."
  (let ((stack (shipit-stack-detect)))
    (when stack
      (let* ((layers (cdr (assq 'layers stack)))
             (below (butlast layers)))
        (cons (mapcar (lambda (layer) (cdr (assq 'tip layer))) below)
              (mapcar (lambda (layer)
                        (cons (cdr (assq 'tip layer)) (cdr (assq 'branch layer))))
                      layers))))))

(defun shipit-stacked-preview--proposed-boundaries (commits)
  "Return the SHAs to divide COMMITS at, from their subject prefixes.
A division goes after the last commit of a run sharing one prefix.
Nil when the subjects give nothing to go on, which leaves one layer
and the divisions to be made by hand.  The newest commit is never one:
it ends the top layer, and the top of a pile is not a division."
  (let (boundaries previous-prefix previous-sha)
    (dolist (commit commits)
      (let ((prefix (shipit-stack--commit-prefix
                     (or (plist-get commit :first-line) ""))))
        (when (and previous-prefix prefix previous-sha
                   (not (equal prefix previous-prefix)))
          (push previous-sha boundaries))
        (when prefix (setq previous-prefix prefix))
        (setq previous-sha (plist-get commit :sha))))
    (nreverse boundaries)))

;;;; Rendering

(defun shipit-stacked-preview--sending-p (branch)
  "Return non-nil when BRANCH is among the ones on their way."
  (let ((sending shipit-stacked-preview--sending))
    (member branch (if (listp sending) sending (list sending)))))

(defun shipit-stacked-preview--drift (branch)
  "Return how far BRANCH and its copy on the remote have come apart.
Nil when they stand on the same commit, or when the remote has none."
  (and (shipit-stack--rev (concat "origin/" branch))
       (shipit-stacked-preview--ahead-behind (concat "origin/" branch) branch)))

(defun shipit-stacked-preview--base-under (ref)
  "Return the commit REF stands on over the stack\='s base, or nil."
  (and shipit-stacked-preview--base
       (car (shipit-stack--git-lines "merge-base" ref shipit-stacked-preview--base))))

(defun shipit-stacked-preview--remote-base-moved (branch)
  "Return where the copy of BRANCH on the remote stands over the stack\='s
base, when that is beyond where BRANCH stands; nil otherwise.
A bot that rebases the stack after the pull request under it merges
moves the copies this way, and a copy rebased there is followed with a
rebase here, not pulled."
  (let ((here (shipit-stacked-preview--base-under branch))
        (there (shipit-stacked-preview--base-under (concat "origin/" branch))))
    (and here there (not (equal here there))
         (shipit-stack--ancestor-p here there)
         there)))

(defun shipit-stacked-preview--remote-movement (branch)
  "Return what the copy of BRANCH on the remote did that this one has not.
`rebased\=' when it stands on a newer base, `new-commits\=' when it only
gained commits, nil when it did nothing or when which side moved cannot
be told: two sides that both gained commits say nothing about who
rewrote what."
  (cond ((shipit-stacked-preview--remote-base-moved branch) 'rebased)
        ((pcase-let ((`(,ahead . ,behind)
                      (shipit-stacked-preview--ahead-behind-counts
                       (concat "origin/" branch) branch)))
           (and behind (> behind 0) (zerop ahead)))
         'new-commits)))

(defun shipit-stacked-preview--merged-by-git-p (tip)
  "Return non-nil when a merge of TIP into the stack\='s base changes nothing.
Then the base holds everything up to TIP, whichever way it got there:
a squash merge or a rebase merge puts the commits on the base under new
ids, and a layer merged that way is still listed over the base with its
branch gone from the remote, looking like one never sent.  It misses a
layer whose lines the base changed again after the merge, which is why
the forge is asked first."
  (and shipit-stacked-preview--base
       (let ((merged (car (shipit-stack--git-lines
                           "merge-tree" "--write-tree"
                           shipit-stacked-preview--base tip)))
             (base (car (shipit-stack--git-lines
                         "rev-parse" (concat shipit-stacked-preview--base "^{tree}")))))
         (and merged (equal merged base)))))

(defun shipit-stacked-preview--ask-forge-merged (branch)
  "Return the merged pull request of BRANCH as the forge tells it, or nil.
A forge that cannot answer counts as no answer, and git decides."
  (and shipit-stacked-preview--repo
       (condition-case err
           (shipit-pr--fetch-merged-pr-for-branch shipit-stacked-preview--repo branch)
         (error
          (shipit--debug-log "stacked preview: merged pull request of %s not known: %s"
                             branch (error-message-string err))
          nil))))

(defun shipit-stacked-preview--merged-pr (branch)
  "Return the merged pull request of BRANCH, or nil, asking the forge once.
The answer is kept until the next refresh, since every redraw asks for
the state of every layer."
  (let ((known (assoc branch shipit-stacked-preview--merged-prs)))
    (if known
        (cdr known)
      (let ((merged (shipit-stacked-preview--ask-forge-merged branch)))
        (push (cons branch merged) shipit-stacked-preview--merged-prs)
        merged))))

(defun shipit-stacked-preview--merged-pr-of (layer)
  "Return the merged pull request that holds every commit of LAYER, or nil.
A branch name can be used again after its pull request merged, so a
merged pull request counts only when the layer ends at or below the
commit that it merged."
  (let ((merged (shipit-stacked-preview--merged-pr (cdr (assq 'branch layer))))
        (tip (cdr (assq 'tip layer))))
    (and merged
         (cdr (assq 'sha merged))
         (shipit-stack--ancestor-p tip (cdr (assq 'sha merged)))
         merged)))

(defun shipit-stacked-preview--merged-state (layer)
  "Return the state of LAYER when it merged into the stack\='s base, or nil.
A pull request that the forge says merged the layer gives its number."
  (let ((merged (shipit-stacked-preview--merged-pr-of layer)))
    (cond (merged (list :state 'merged :number (cdr (assq 'number merged))))
          ((shipit-stacked-preview--merged-by-git-p (cdr (assq 'tip layer)))
           (list :state 'merged)))))

(defun shipit-stacked-preview--movement-words (moved)
  "Return the words for MOVED, what the copy on the remote did, or nil."
  (pcase moved
    ('rebased "rebased on the remote")
    ('new-commits "new commits on the remote")))

(defun shipit-stacked-preview--drift-words (state)
  "Return the words for how far the branch of STATE left the remote, or nil.
A branch rewritten since it was pushed still has its remote copy, so
without them it would read as pushed while the remote holds the old
version of the stack.  The `:moved' of STATE says what the copy on the
remote did when the drift is its doing, since the counts alone do not
say which side moved, and the answer differs: a copy rebased onto a
newer base is followed with a rebase here, one that gained commits is
pulled."
  (when-let* ((words (delq nil (list (plist-get state :drift)
                                     (shipit-stacked-preview--movement-words
                                      (plist-get state :moved))))))
    (propertize (string-join words ", ") 'font-lock-face 'font-lock-comment-face)))

(defun shipit-stacked-preview--remote-state (branch statuses)
  "Return the state of a layer whose BRANCH is on the remote.
STATUSES maps pull request numbers to their check rollup and review
decision."
  (let ((number (shipit-stacked-preview--pr-number branch))
        (drift (shipit-stacked-preview--drift branch))
        (moved (shipit-stacked-preview--remote-movement branch)))
    (if number
        (let ((status (cdr (assq number statuses))))
          (list :state 'pull-request :number number
                :checks (cdr (assq 'checks status))
                :review (cdr (assq 'review status))
                :drift drift :moved moved))
      (list :state (if drift 'drifted 'pushed) :drift drift :moved moved))))

(defun shipit-stacked-preview--layer-state (layer &optional statuses)
  "Return where LAYER has got to, as a plist.
`:state' is `sending', `merged', `no-branch', `not-pushed', `pushed',
`drifted' or `pull-request'.  A layer on the remote also carries
`:drift', the words for how far its copy there came apart from this
one, or nil, and `:moved', what that copy did to come apart.  A pull
request carries `:number', and `:checks' and `:review' from STATUSES,
which maps pull request numbers to their check rollup and review
decision.  The row and the stack canvas both draw from this, so the
two cannot disagree about a layer."
  (let ((branch (cdr (assq 'branch layer))))
    (cond
     ((shipit-stacked-preview--sending-p branch) (list :state 'sending))
     ((shipit-stacked-preview--merged-state layer))
     ((not (or (cdr (assq 'existing layer)) (shipit-stack--rev branch)))
      (list :state 'no-branch))
     ((not (shipit-stack--rev (concat "origin/" branch)))
      (list :state 'not-pushed))
     (t (shipit-stacked-preview--remote-state branch statuses)))))

(defun shipit-stacked-preview--remote-status (state)
  "Return the status text of a layer on the remote in STATE, drift aside."
  (if-let* ((number (plist-get state :number)))
      (concat (propertize (format "PR #%s" number)
                          'font-lock-face 'magit-branch-remote)
              (shipit--stack-status-glyphs
               (list (cons 'checks (plist-get state :checks))
                     (cons 'review (plist-get state :review)))))
    (propertize "pushed" 'font-lock-face 'success)))

(defun shipit-stacked-preview--merged-text (state)
  "Return the words for a layer in the merged STATE."
  (let ((into (format "merged into %s" (shipit-stacked-preview--base-name
                                        shipit-stacked-preview--base))))
    (if-let* ((number (plist-get state :number)))
        (format "#%s %s" number into)
      into)))

(defun shipit-stacked-preview--status-text (state)
  "Return the status of a layer in STATE without its drift words.
Dim while the layer is still only local, orange while it is on its way,
green once it is on the remote, and the branch colour once it has a
pull request of its own; green with the name of the base once it is
merged into it.  A layer with a pull request wears its glyphs behind
the number."
  (pcase (plist-get state :state)
    ('sending (propertize "sending…" 'font-lock-face 'warning))
    ('merged (propertize (shipit-stacked-preview--merged-text state)
                         'font-lock-face 'success))
    ('no-branch (propertize "no branch yet" 'font-lock-face 'font-lock-comment-face))
    ('not-pushed (propertize "not pushed" 'font-lock-face 'font-lock-comment-face))
    (_ (shipit-stacked-preview--remote-status state))))

(defun shipit-stacked-preview--state-text (state)
  "Return the status text of a layer in STATE, coloured by how far it got.
A layer on the remote also says how far its copy there came apart from
this one and what that copy did."
  (let ((status (shipit-stacked-preview--status-text state)))
    (if-let* ((drift (shipit-stacked-preview--drift-words state)))
        (concat status "  " drift)
      status)))

(defun shipit-stacked-preview--layer-status (layer &optional statuses)
  "Return the status text of LAYER, as its row draws it.
STATUSES maps pull request numbers to their check rollup and review
decision."
  (shipit-stacked-preview--state-text
   (shipit-stacked-preview--layer-state layer statuses)))

(defun shipit-stacked-preview--learn-statuses (numbers)
  "Ask the forge for the checks and reviews of NUMBERS, and keep the answer.
One quiet fetch answers for all of them, and a forge that cannot
answer right now leaves their rows bare until the next refresh."
  (let ((answer (shipit-pr--stack-statuses-quietly shipit-stacked-preview--repo
                                                   numbers)))
    (dolist (number numbers)
      (push (cons number (cdr (assq number answer)))
            shipit-stacked-preview--known-statuses))))

(defun shipit-stacked-preview--statuses (layers)
  "Return the statuses of LAYERS\=' pull requests, keyed by number.
The forge is asked only for a pull request that it was not asked about
since the last refresh, and a layer without a pull request asks for
nothing."
  (let* ((numbers (delq nil (mapcar (lambda (layer)
                                      (shipit-stacked-preview--pr-number
                                       (cdr (assq 'branch layer))))
                                    layers)))
         (unknown (seq-remove (lambda (number)
                                (assq number shipit-stacked-preview--known-statuses))
                              numbers)))
    (when unknown
      (shipit-stacked-preview--learn-statuses unknown))
    (seq-filter (lambda (entry) (and (cdr entry) (memq (car entry) numbers)))
                shipit-stacked-preview--known-statuses)))

(defun shipit-stacked-preview--pr-number (branch)
  "Return the number of the open pull request whose head is BRANCH.
Only asked for a branch that has reached the remote, since one that has
not cannot have a pull request.  The answer is cached by the http layer,
misses included, so redrawing the list does not ask again."
  (and shipit-stacked-preview--repo
       (shipit-stack-plan--pr-for-branch shipit-stacked-preview--repo branch)))

(defun shipit-stacked-preview--read-base (&optional default)
  "Ask which ref the stack sits on, offering DEFAULT or the default branch."
  (completing-read "Stack sits on: " (magit-list-refnames) nil t nil nil
                   (or default (shipit-stack--base-ref))))

(defun shipit-stacked-preview--commits-over (base)
  "Return the commits from BASE to HEAD, oldest first.
Signal when there are none, since a stack of nothing has no layers."
  (or (shipit--commits-in-range (format "%s..HEAD" base))
      (user-error "No commits between %s and HEAD" base)))

(defun shipit-stacked-preview--divisions-among (boundaries commits)
  "Return the BOUNDARIES that are commits of COMMITS.
A branch at the base, or below it, divides nothing above the base."
  (let ((shas (mapcar (lambda (commit) (plist-get commit :sha)) commits)))
    (seq-filter (lambda (sha) (member sha shas)) boundaries)))

(defun shipit-stacked-preview--set-stack (base commits existing)
  "Make this buffer describe COMMITS over BASE.
EXISTING is what `shipit-stacked-preview--from-existing-stack' found.
Its branches divide the commits, and without them the subjects do."
  (setq shipit-stacked-preview--base base
        shipit-stacked-preview--commits commits
        shipit-stacked-preview--boundaries
        (or (shipit-stacked-preview--divisions-among (car existing) commits)
            (shipit-stacked-preview--proposed-boundaries commits))
        shipit-stacked-preview--names (cdr existing)))

(defun shipit-stacked-preview-set-base (base)
  "Measure the stack from BASE, and divide its commits again.
The commits are divided as when the preview opens, so a division made
by hand is proposed again, and undo brings it back.  When BASE leaves
no commits, the stack stays as it was."
  (interactive (list (shipit-stacked-preview--read-base
                      shipit-stacked-preview--base)))
  (let ((commits (shipit-stacked-preview--commits-over base)))
    (shipit-stacked-preview--record-step)
    (shipit-stacked-preview--set-stack
     base commits (shipit-stacked-preview--from-existing-stack))
    (shipit-stacked-preview--render)))

(defun shipit-stacked-preview--remote-tips ()
  "Alist of commit sha to the remote branch names standing there."
  (let (tips)
    (dolist (line (shipit-stack--git-lines
                   "for-each-ref" "refs/remotes/origin"
                   "--format=%(objectname) %(refname:short)"))
      (pcase-let ((`(,sha ,name) (split-string line " ")))
        (unless (equal name "origin/HEAD")
          (push name (alist-get sha tips nil nil #'equal)))))
    tips))

(defun shipit-stacked-preview--detect-base (repo default &optional own)
  "Return the ref the stack sits on: DEFAULT, unless a pull request says else.
A commit below HEAD that is the tip of a remote branch with an open
pull request is already on its way to DEFAULT, and stacking it again
would raise it twice.  The nearest such branch becomes the base, so
the buffer holds only work no pull request carries yet.  The newest
commit never does: a base at HEAD would leave a stack of nothing.
A branch named in OWN is a layer of this stack, and its pull request
is one the stack made, so it does not move the base either: a stack
opened again after its layers were submitted still holds all of them.
With REPO nil, or no such branch, DEFAULT stands."
  (or (when repo
        (let ((tips (shipit-stacked-preview--remote-tips))
              (current (shipit-stack--current-branch)))
          (seq-some
           (lambda (sha)
             (seq-some
              (lambda (ref)
                (let ((name (shipit-stacked-preview--base-name ref)))
                  (and (not (equal name current))
                       (not (member name own))
                       (not (equal ref default))
                       (shipit-stack-plan--pr-for-branch repo name)
                       ref)))
              (alist-get sha tips nil nil #'equal)))
           (cdr (shipit-stack--git-lines
                 "log" "--format=%H" (format "%s..HEAD" default))))))
      default))

(defun shipit-stacked-preview--branch-at-point ()
  "Return the branch of the layer whose section holds point, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'shipit-stack-layer)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun shipit-stacked-preview--layer-named (branch)
  "Return the layer of this stack whose branch is BRANCH.
Signals when there is none: every action here works on one layer, and
acting on the wrong one is worse than refusing to act."
  (or (seq-find (lambda (layer) (equal branch (cdr (assq 'branch layer))))
                (shipit-stacked-preview--current-layers))
      (user-error "No layer named %s" branch)))

(defun shipit-stacked-preview--layer-at-point ()
  "Return the layer whose section holds point, or signal on none."
  (shipit-stacked-preview--layer-named
   (or (shipit-stacked-preview--branch-at-point)
       (user-error "Point is not on a layer"))))

(defun shipit-stacked-preview--layer-position (branch)
  "Return where the row of the layer BRANCH starts, or nil when not drawn.
The section is found in the section tree by its type and its branch."
  (when-let* ((section (and magit-root-section
                            (magit-get-section
                             `((shipit-stack-layer . ,branch) (shipit-stack-root))))))
    (let ((start (oref section start)))
      ;; A section starts at a marker, or at a plain position when
      ;; `magit-section-inhibit-markers' is on.
      (if (markerp start) (marker-position start) start))))

(defun shipit-stacked-preview--base-of (layer)
  "Return the ref the pull request for LAYER should be raised against.
The layer below it, or -- for the bottom layer -- the branch the whole
stack sits on.  A layer below is a local branch that has usually not
reached the remote, so it is named unqualified; the branch under the
stack is read from the remote, the way the preview names it when it
works one out itself."
  (let* ((layers (shipit-stacked-preview--current-layers))
         (position (seq-position layers layer)))
    (unless position (user-error "That layer is no longer in this stack"))
    (if (> position 0)
        (cdr (assq 'branch (nth (1- position) layers)))
      shipit-stacked-preview--base)))

(defun shipit-stacked-preview--require-branch (layer)
  "Return LAYER's branch name, or signal when it does not exist yet."
  (let ((branch (cdr (assq 'branch layer))))
    (unless (shipit-stack--rev branch)
      (user-error "Branch %s does not exist yet -- press b c to create it" branch))
    branch))

(defconst shipit-stacked-preview--commit-indent 9
  "Columns to indent a commit by, so its hash starts where a branch
name does.  A layer row spends its first twelve columns on the HEAD
mark and the layer number; a commit has neither, and without this its
hash sits to the left of the layer holding it.")

(defconst shipit-stacked-preview--text-column 12
  "The column where the text of a row starts.
The branch name of a layer starts there, after the HEAD mark and the
layer number, and so do the words after a label such as Next:.  A line
that goes on below a row starts there too.")

(defun shipit-stacked-preview--fits-p (columns)
  "Return non-nil when a line of COLUMNS fits the width the buffer is drawn for.
The last column of a window holds the mark of a cut line, so the line
must leave it free.  Without a width, as in a drawing for no window,
each line fits."
  (or (null shipit-stacked-preview--drawn-width)
      (< columns shipit-stacked-preview--drawn-width)))

(defun shipit-stacked-preview--below (text)
  "Return TEXT as a line that goes on below a row, at the text column."
  (concat "\n" (make-string shipit-stacked-preview--text-column ?\s) text))

(defun shipit-stacked-preview--layer-heading (start state)
  "Return the heading of a layer: START, then the status text of STATE.
When the heading is too wide for the window, the drift words of STATE
go to a line of their own below it."
  (let ((full (concat start (shipit-stacked-preview--state-text state)))
        (drift (shipit-stacked-preview--drift-words state)))
    (if (or (shipit-stacked-preview--fits-p (string-width full)) (null drift))
        full
      (concat start (shipit-stacked-preview--status-text state)
              (shipit-stacked-preview--below drift)))))

(defun shipit-stacked-preview--name-width (layers)
  "Return the column width the branch names of LAYERS need."
  (apply #'max 12 (mapcar (lambda (l) (length (cdr (assq 'branch l)))) layers)))

(defun shipit-stacked-preview--count-width (layers)
  "Return the column width the commit counts of LAYERS need.
Measured rather than assumed: a branch that has run well ahead of the
base can hold hundreds, and a fixed field pushes the word beside it
out of line the moment one does."
  (apply #'max 1 (mapcar (lambda (l) (length (number-to-string
                                              (length (cdr (assq 'commits l))))))
                         layers)))

(defun shipit-stacked-preview--insert-layer
    (layer index width count-width top-p state)
  "Insert LAYER, numbered INDEX from the bottom.
Names are padded to WIDTH and commit counts to COUNT-WIDTH.  TOP-P
marks the newest end of the line of commits.  STATE is where the layer
has got to, as `shipit-stacked-preview--layer-state' returns it."
  (let ((commits (cdr (assq 'commits layer))))
    (magit-insert-section (shipit-stack-layer (cdr (assq 'branch layer)))
      (magit-insert-heading
        (shipit-stacked-preview--layer-heading
         (format "  %-5s %2d  %s  %s commit%s  "
                 (if top-p
                     ;; The colour of the current branch without its box.
                     ;; A box adds pixels and no characters, so the row
                     ;; carrying it sits a shade right of the rest while
                     ;; being aligned to the column.
                     (propertize "HEAD" 'font-lock-face
                                 '(:inherit magit-branch-current :box nil))
                   "")
                 index
                 (shipit-sections--pad
                  (propertize (cdr (assq 'branch layer))
                              'font-lock-face 'magit-branch-local)
                  width)
                 (let ((n (number-to-string (length commits))))
                   (concat (make-string (- count-width (length n)) ?\s) n))
                 (if (cdr commits) "s" " "))
         state))
      (magit-insert-section-body
        ;; Each commit through the renderer the pull request buffer
        ;; uses, so a commit here opens onto its message and the files
        ;; it changed exactly as it does there.  The section around
        ;; them is skipped: it would put a second "Commits" heading
        ;; inside a layer that already says how many it holds.
        (dolist (commit (shipit-stacked-preview--display-commits commits))
          (shipit--insert-processed-commit-section
           commit shipit-stacked-preview--repo nil
           shipit-stacked-preview--commit-indent))))))

(defun shipit-stacked-preview--display-commits (commits)
  "Return COMMITS ordered for display, newest first.
Layers hold them oldest first, the order they merge in, but the
buffer draws the stack top first with the base at the bottom, so
within a layer history grows upward too."
  (reverse commits))

(defun shipit-stacked-preview--insert-base-line (text)
  "Insert TEXT, a line that names the base, so that RET on it sets the base."
  (insert (propertize text 'shipit-stack-base t)))

(defun shipit-stacked-preview--insert-base (width)
  "Insert the line naming what the lowest layer sits on, padded to WIDTH."
  (shipit-stacked-preview--insert-base-line
   (format "  %-5s %2s  %s\n"
           (propertize "base" 'font-lock-face 'font-lock-comment-face)
           ""
           (shipit-sections--pad
            (propertize (shipit-stacked-preview--base-name
                         shipit-stacked-preview--base)
                        'font-lock-face 'magit-branch-remote)
            width))))

(defconst shipit-stacked-preview--key-columns
  '((("RET" . "open layer, set base") ("s" . "divide or join"))
    (("b" . "branch") ("r" . "rebase") ("P" . "push"))
    (("R" . "register stack") ("v" . "draw on canvas"))
    (("g" . "refresh") ("q" . "quit") ("$" . "process log")))
  "The keys this buffer offers, a list per column.
Down each column rather than across: dividing the stack, making it
exist on the remote, then leaving.")

(defun shipit-stacked-preview--key-cell (entry key-width label-width)
  "Return ENTRY as a cell of a fixed width, or blank when there is none.
Every cell is the same width whether or not it holds anything, so the
columns line up on rows where one has run out."
  (if entry
      (concat (shipit-sections--pad
               (propertize (car entry) 'font-lock-face 'transient-key)
               key-width)
              "  "
              (shipit-sections--pad
               (propertize (cdr entry) 'font-lock-face 'font-lock-comment-face)
               label-width))
    (make-string (+ key-width 2 label-width) ?\s)))

(defun shipit-stacked-preview--key-row (columns row key-width label-widths)
  "Return line ROW of the key COLUMNS, padded to LABEL-WIDTHS."
  (string-trim-right
   (mapconcat #'identity
              (seq-mapn (lambda (column label-width)
                          (shipit-stacked-preview--key-cell
                           (nth row column) key-width label-width))
                        columns label-widths)
              "   ")))

(defun shipit-stacked-preview--key-width ()
  "Return the width of the widest key that this buffer offers."
  (apply #'max (mapcar (lambda (entry) (length (car entry)))
                       (apply #'append shipit-stacked-preview--key-columns))))

(defun shipit-stacked-preview--label-width (column)
  "Return the width of the widest label of COLUMN, 0 when there is none."
  (apply #'max 0 (mapcar (lambda (entry) (length (cdr entry))) column)))

(defun shipit-stacked-preview--key-blocks (count)
  "Return the key columns in blocks of COUNT columns, one block below the other."
  (seq-partition shipit-stacked-preview--key-columns count))

(defun shipit-stacked-preview--block-label-widths (blocks)
  "Return the label width of each place of a column in BLOCKS.
The columns in the same place of each block share a width, so the
blocks line up."
  (mapcar (lambda (place)
            (apply #'max (mapcar (lambda (block)
                                   (shipit-stacked-preview--label-width (nth place block)))
                                 blocks)))
          (number-sequence 0 (1- (length (car blocks))))))

(defun shipit-stacked-preview--blocks-width (blocks)
  "Return the width of the widest line that the key BLOCKS can have."
  (let ((cells (mapcar (lambda (label-width)
                         (+ (shipit-stacked-preview--key-width) 2 label-width))
                       (shipit-stacked-preview--block-label-widths blocks))))
    (+ 3 (apply #'+ cells) (* 3 (1- (length cells))))))

(defun shipit-stacked-preview--fitting-key-blocks ()
  "Return the key columns in as few blocks as fit, one below the other.
The columns are spread evenly over the blocks, so four columns that do
not fit side by side become two blocks of two, not three and one.  A
window too narrow for even two gets one column in each block."
  (let* ((columns (length shipit-stacked-preview--key-columns))
         (counts (mapcar (lambda (blocks) (ceiling columns blocks))
                         (number-sequence 1 columns))))
    (shipit-stacked-preview--key-blocks
     (or (seq-find (lambda (count)
                     (shipit-stacked-preview--fits-p
                      (shipit-stacked-preview--blocks-width
                       (shipit-stacked-preview--key-blocks count))))
                   counts)
         1))))

(defun shipit-stacked-preview--insert-key-block (block key-width label-widths)
  "Insert the key columns of BLOCK side by side, padded to LABEL-WIDTHS."
  (dotimes (row (apply #'max (mapcar #'length block)))
    (insert "   "
            (shipit-stacked-preview--key-row block row key-width label-widths)
            "\n")))

(defun shipit-stacked-preview--insert-keys ()
  "Insert the keys this buffer offers, in aligned columns.
A window too narrow for all columns side by side gets them in blocks,
one below the other."
  (let* ((blocks (shipit-stacked-preview--fitting-key-blocks))
         (key-width (shipit-stacked-preview--key-width))
         (label-widths (shipit-stacked-preview--block-label-widths blocks)))
    (dolist (block blocks)
      (shipit-stacked-preview--insert-key-block block key-width label-widths))
    (insert "\n")))

(defun shipit-stacked-preview--ahead-behind-counts (upstream &optional local)
  "Return (AHEAD . BEHIND): the commits LOCAL has over UPSTREAM, and the reverse.
LOCAL defaults to HEAD.  Nil when git cannot compare the two."
  (when-let* ((counts (car (shipit-stack--git-lines
                            "rev-list" "--left-right" "--count"
                            (format "%s...%s" upstream (or local "HEAD")))))
              (parts (split-string counts)))
    (cons (string-to-number (cadr parts)) (string-to-number (car parts)))))

(defun shipit-stacked-preview--ahead-behind (upstream &optional local)
  "Return \"N ahead, M behind\" of LOCAL against UPSTREAM, or nil.
LOCAL defaults to HEAD.  A count of zero is left out; nil when the two
are level."
  (pcase-let ((`(,ahead . ,behind)
               (shipit-stacked-preview--ahead-behind-counts upstream local)))
    (let ((words (delq nil
                       (list (and ahead (> ahead 0) (format "%d ahead" ahead))
                             (and behind (> behind 0)
                                  (format "%d behind" behind))))))
      (and words (string-join words ", ")))))

(defun shipit-stacked-preview--refs ()
  "Return where HEAD, its upstream and the base point now, as a plist.
`:branch' and `:subject' are those of HEAD, `:upstream' is its
upstream, `:counts' says how far HEAD and the upstream are apart, and
`:base-pr' is the pull request the stack sits on."
  (let ((upstream (car (shipit-stack--git-lines
                        "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}"))))
    (list :branch (or (shipit-stack--current-branch) "detached HEAD")
          :subject (car (shipit-stack--git-lines "log" "--format=%s" "-1"))
          :upstream upstream
          :counts (and upstream (shipit-stacked-preview--ahead-behind upstream))
          :base-pr (shipit-stacked-preview--base-pr-number))))

(defun shipit-stacked-preview--labelled-line (label text)
  "Return a line with LABEL, then TEXT, that fits the window.
When the line is too wide, TEXT is wrapped, and its lines after the
first start under its first word, at the text column."
  (let* ((head (format "  %-9s " label))
         (line (concat head text)))
    (concat
     (if (shipit-stacked-preview--fits-p (string-width line))
         line
       (concat head
               (replace-regexp-in-string
                "\n" (concat "\n" (make-string shipit-stacked-preview--text-column ?\s))
                (shipit--wrap-text text (- shipit-stacked-preview--drawn-width
                                           shipit-stacked-preview--text-column 1))
                t t)))
     "\n")))

(defun shipit-stacked-preview--insert-refs (&optional refs)
  "Say where HEAD, its upstream and the base actually point.
REFS is a plist as `shipit-stacked-preview--refs' returns it; without
it, git and the forge are asked now.  The stack is measured over the
base, not over the branch\='s upstream, and when the two differ a
reader can believe they stand on one branch while standing on
another."
  (let* ((refs (or refs (shipit-stacked-preview--refs)))
         (upstream (plist-get refs :upstream))
         (counts (plist-get refs :counts)))
    (insert (shipit-stacked-preview--labelled-line
             "Head:"
             (concat (propertize (plist-get refs :branch)
                                 'font-lock-face 'magit-branch-local)
                     "  " (or (plist-get refs :subject) ""))))
    (when upstream
      (insert (shipit-stacked-preview--labelled-line
               "Upstream:"
               (concat (propertize upstream
                                   'font-lock-face 'magit-branch-remote)
                       (if counts
                           (propertize (concat "  " counts) 'font-lock-face
                                       'font-lock-comment-face)
                         "")))))
    (shipit-stacked-preview--insert-base-line
     (shipit-stacked-preview--labelled-line
      "Base:"
      (concat (propertize (or shipit-stacked-preview--base "")
                          'font-lock-face 'magit-branch-remote)
              (if-let* ((pr (plist-get refs :base-pr)))
                  (propertize (format "  PR #%s" pr)
                              'font-lock-face 'font-lock-comment-face)
                ""))))))

(defconst shipit-stacked-preview--done-states '(pull-request merged)
  "The states of a layer that needs no further step to become a pull request.")

(defun shipit-stacked-preview--key (key)
  "Return KEY in the face that the key hints use."
  (propertize key 'font-lock-face 'transient-key))

(defun shipit-stacked-preview--some-state-p (states wanted)
  "Return non-nil when a layer in STATES is in one of the states WANTED."
  (seq-some (lambda (entry) (memq (plist-get (cdr entry) :state) wanted))
            states))

(defun shipit-stacked-preview--open-layers (states)
  "Return the entries of STATES whose layer still needs a step, bottom first."
  (seq-remove (lambda (entry)
                (memq (plist-get (cdr entry) :state)
                      shipit-stacked-preview--done-states))
              states))

(defun shipit-stacked-preview--branch-words (branch)
  "Return BRANCH in the face of a local branch."
  (propertize branch 'font-lock-face 'magit-branch-local))

(defun shipit-stacked-preview--number-above (states entry)
  "Return the pull request of the layer right above ENTRY in STATES, or nil.
A layer without a pull request under one that has it was split off
from that one."
  (let ((above (cadr (member entry states))))
    (and above
         (eq (plist-get (cdr above) :state) 'pull-request)
         (plist-get (cdr above) :number))))

(defun shipit-stacked-preview--split-off-layer (states)
  "Return the entry of STATES of a layer split off below a pull request, or nil."
  (seq-find (lambda (entry)
              (and (eq (plist-get (cdr entry) :state) 'no-branch)
                   (shipit-stacked-preview--number-above states entry)))
            states))

(defun shipit-stacked-preview--branch-step (states)
  "Return the words for giving the layers in STATES their branches."
  (if-let* ((split (shipit-stacked-preview--split-off-layer states)))
      (format "press %s to give the new layer %s a branch"
              (shipit-stacked-preview--key "b c")
              (shipit-stacked-preview--branch-words (car split)))
    (format "check the base and the divisions, then press %s to give each layer a branch"
            (shipit-stacked-preview--key "b c"))))

(defun shipit-stacked-preview--describe-step (states entry)
  "Return the words for opening the pull request of the layer of ENTRY.
STATES are the states of all layers.  For a layer split off below a
pull request, the words also say what shipit does after the submit."
  (let ((number (shipit-stacked-preview--number-above states entry)))
    (concat
     (format "press %s on %s to describe it and open its pull request"
             (shipit-stacked-preview--key "RET")
             (shipit-stacked-preview--branch-words (car entry)))
     (when number
       (format "; shipit then makes it the base of #%s%s" number
               (if (shipit-pr--stack-supported-p shipit-stacked-preview--repo)
                   " and builds the stack again"
                 ""))))))

(defun shipit-stacked-preview--next-step (states)
  "Return the words for the next step to make the stack, or nil if none.
STATES maps the branch of each layer to its state, bottom first.  The
steps go in the order a stack is made: the branches, the push, then a
pull request for each layer from the bottom up.  A layer whose copy on
the remote differs from this one needs the push too, or its pull
request would show the commits of the old copy.  When every layer has
its pull request, a stack that shipit could not build in the order of
the layers is the last step."
  (let ((open (shipit-stacked-preview--open-layers states)))
    (cond
     ((null open)
      (when shipit-stacked-preview--stack-to-rebuild
        (format "press %s to build the stack again in the order of the layers"
                (shipit-stacked-preview--key "R"))))
     ((shipit-stacked-preview--some-state-p states '(no-branch))
      (shipit-stacked-preview--branch-step states))
     ((shipit-stacked-preview--some-state-p states '(not-pushed drifted))
      (format "press %s to push the layers" (shipit-stacked-preview--key "P")))
     (t (shipit-stacked-preview--describe-step states (car open))))))

(defun shipit-stacked-preview--insert-next-step (states)
  "Insert the line that names the next step to make the stack, if one is left.
STATES maps the branch of each layer to its state, bottom first."
  (when-let* ((words (shipit-stacked-preview--next-step states)))
    (insert (shipit-stacked-preview--labelled-line "Next:" words))))

(defun shipit-stacked-preview--insert-header (states)
  "Insert the banner saying what this buffer is.
STATES maps the branch of each layer to its state, bottom first."
  (insert (propertize
           (format "STACKED PULL REQUESTS — %d commit%s over %s\n"
                   (length shipit-stacked-preview--commits)
                   (if (cdr shipit-stacked-preview--commits) "s" "")
                   (shipit-stacked-preview--base-name
                    shipit-stacked-preview--base))
           'font-lock-face '(:foreground "orange" :weight bold)))
  (shipit-stacked-preview--insert-refs shipit-stacked-preview--refs-shown)
  (shipit-stacked-preview--insert-next-step states)
  (shipit-stacked-preview--insert-keys))

(defun shipit-stacked-preview--layer-states (layers statuses)
  "Return the state of each of LAYERS, by branch.
STATUSES are the pull request statuses of the stack."
  (mapcar (lambda (layer)
            (cons (cdr (assq 'branch layer))
                  (shipit-stacked-preview--layer-state layer statuses)))
          layers))

(defun shipit-stacked-preview--look-up ()
  "Ask git and the forge for what the buffer shows, and keep the answers."
  (let ((layers (shipit-stacked-preview--current-layers)))
    (setq shipit-stacked-preview--layers-shown layers
          shipit-stacked-preview--states-shown
          (shipit-stacked-preview--layer-states
           layers (shipit-stacked-preview--statuses layers))
          shipit-stacked-preview--refs-shown (shipit-stacked-preview--refs)
          shipit-stacked-preview--problems-shown
          (shipit-stacked-preview--problems layers))))

(defun shipit-stacked-preview--draw ()
  "Draw the buffer from what the last look-up found, and ask nothing."
  (let ((inhibit-read-only t)
        (layers shipit-stacked-preview--layers-shown)
        (states shipit-stacked-preview--states-shown))
    (setq shipit-stacked-preview--drawn-width (shipit--commit-row-width))
    (erase-buffer)
    (magit-insert-section (shipit-stack-root)
      (shipit-stacked-preview--insert-header states)
      (shipit-stacked-preview--insert-fetch-problem)
      (shipit-stacked-preview--insert-problems)
      ;; Newest at the top and the base at the bottom, which is the way
      ;; a stack is drawn and the way the commits actually sit.
      (let ((width (shipit-stacked-preview--name-width layers))
            (count-width (shipit-stacked-preview--count-width layers))
            (index (length layers)))
        (dolist (layer (reverse layers))
          (shipit-stacked-preview--insert-layer
           layer index width count-width (= index (length layers))
           (cdr (assoc (cdr (assq 'branch layer)) states)))
          (setq index (1- index)))
        (shipit-stacked-preview--insert-base width)))))

(defun shipit-stacked-preview--render ()
  "Look up the stack again, and draw the buffer from it."
  (let ((position (point)))
    (shipit-stacked-preview--look-up)
    (shipit-stacked-preview--draw)
    (goto-char (min position (point-max)))))

;;;; Fitting the window

(defun shipit-stacked-preview--width-changed-p ()
  "Return non-nil when the commit rows were drawn for another width."
  (not (eql shipit-stacked-preview--drawn-width (shipit--commit-row-width))))

(defun shipit-stacked-preview--window-resized (window)
  "Fit the buffer of WINDOW to its window once the resize stops.
Called from `window-size-change-functions' after a window that shows
the buffer changed its size.  A change of height alone draws nothing."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (when (shipit-stacked-preview--width-changed-p)
        (when (timerp shipit-stacked-preview--fit-timer)
          (cancel-timer shipit-stacked-preview--fit-timer))
        (setq shipit-stacked-preview--fit-timer
              (run-at-time shipit-stacked-preview--fit-delay nil
                           #'shipit-stacked-preview--fit buffer))))))

(defun shipit-stacked-preview--line-start (line)
  "Return the position where LINE of the buffer starts."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun shipit-stacked-preview--window-lines ()
  "Return each window on the buffer with the line of its cursor and its top."
  (mapcar (lambda (window)
            (list window
                  (line-number-at-pos (window-point window))
                  (line-number-at-pos (window-start window))))
          (get-buffer-window-list nil nil t)))

(defun shipit-stacked-preview--draw-in-place ()
  "Draw the buffer, and keep the cursor and the top of each window on their line.
A new width changes no line, only the columns of the commit rows.
Without this the cursor of a window that is not selected goes to the
start of the buffer."
  (let ((windows (shipit-stacked-preview--window-lines))
        (line (line-number-at-pos)))
    (shipit-stacked-preview--draw)
    (goto-char (shipit-stacked-preview--line-start line))
    (pcase-dolist (`(,window ,cursor ,top) windows)
      (set-window-start window (shipit-stacked-preview--line-start top) t)
      (set-window-point window (shipit-stacked-preview--line-start cursor)))))

(defun shipit-stacked-preview--fit (buffer)
  "Draw BUFFER for the width of its window, when that width is new.
The drawing reuses what the last look-up found, so it asks neither git
nor the forge."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shipit-stacked-preview--fit-timer nil)
      (when (and (get-buffer-window buffer t)
                 (shipit-stacked-preview--width-changed-p))
        (shipit-stacked-preview--draw-in-place)))))

;;;; Commands

(defun shipit-stacked-preview--sha-at-point ()
  "Return the SHA of the commit at point, or nil."
  (or (get-text-property (point) 'shipit-commit-sha)
      (let ((section (magit-current-section)))
        (when (and section (eq (oref section type) 'shipit-commit))
          (oref section value)))))

(defun shipit-stacked-preview--toggled-boundaries (sha)
  "Return the divisions without the one at SHA, or with it when it is absent."
  (cond ((member sha shipit-stacked-preview--boundaries)
         (remove sha shipit-stacked-preview--boundaries))
        ((equal sha (plist-get (car (last shipit-stacked-preview--commits)) :sha))
         (user-error "The newest commit already ends the top layer"))
        (t (cons sha shipit-stacked-preview--boundaries))))

(defun shipit-stacked-preview-toggle-division ()
  "End a layer at the commit at point, or join the two layers that meet there.
Each press is one step of the undo history."
  (interactive)
  (let ((boundaries (shipit-stacked-preview--toggled-boundaries
                     (or (shipit-stacked-preview--sha-at-point)
                         (user-error "No commit at point")))))
    (shipit-stacked-preview--record-step)
    (setq shipit-stacked-preview--boundaries boundaries)
    (shipit-stacked-preview--render)))

;;;; The undo history
;;
;; Each change of the base or of the divisions is one step of the undo
;; history of the buffer, so the undo keys and the undo trees take it
;; back.  A step is an `apply' entry that puts the earlier base and
;; divisions back and draws the buffer again.  The text never enters
;; the history: an undone change of text would cut into a newer drawing.
;;
;; Every undo command and undo tree refuses a read-only buffer.  So the
;; buffer is writable, and its text carries the `read-only' property
;; instead, as a customize buffer does.

(defun shipit-stacked-preview--plan ()
  "Return the base and the divisions, which one step of undo puts back."
  (list shipit-stacked-preview--base shipit-stacked-preview--boundaries))

(defun shipit-stacked-preview--record-step ()
  "Add a step to the undo history that puts back the base and divisions of now.
Called before a change of either, so undo takes that change back."
  (push (list 'apply #'shipit-stacked-preview--restore-plan
              (shipit-stacked-preview--plan))
        buffer-undo-list))

(defun shipit-stacked-preview--restore-plan (plan)
  "Put back PLAN, a base and its divisions from the undo history, and redraw.
The base and the divisions it replaces go to the history in turn, so
redo brings them back.  A base other than the one shown reads its
commits again, since git may have moved them meanwhile, and does so
first: a base without commits stops the undo before anything changes."
  (pcase-let* ((`(,base ,boundaries) plan)
               (commits (unless (equal base shipit-stacked-preview--base)
                          (shipit-stacked-preview--commits-over base))))
    (shipit-stacked-preview--record-step)
    (when commits
      (shipit-stacked-preview--set-stack
       base commits (shipit-stacked-preview--from-existing-stack)))
    (setq shipit-stacked-preview--boundaries boundaries))
  (shipit-stacked-preview--render))

(defun shipit-stacked-preview--step-p (entry)
  "Return non-nil when ENTRY of an undo list is a step of this buffer."
  (and (eq (car-safe entry) 'apply)
       (eq (car-safe (cdr entry)) #'shipit-stacked-preview--restore-plan)))

(defun shipit-stacked-preview--text-change-p (entry)
  "Return non-nil when ENTRY of an undo list records a change of the text.
The node Undo of the Emacs Lisp manual lists the kinds of entries.  A
boundary is nil, and undo-tree marks the end of the list with a symbol.
Every other entry but a step of this buffer is about the text: Emacs
records how deleted text moved the markers in it as `apply' entries
too."
  (and entry
       (not (symbolp entry))
       (not (shipit-stacked-preview--step-p entry))))

(defun shipit-stacked-preview--drop-text-changes ()
  "Take each change of the text out of the undo history of this buffer.
The entries that stay keep their cells, since `undo' finds the steps it
undid before by those cells."
  (when (consp buffer-undo-list)
    (while (shipit-stacked-preview--text-change-p (car buffer-undo-list))
      (setq buffer-undo-list (cdr buffer-undo-list)))
    (let ((cell buffer-undo-list))
      (while (cdr cell)
        (if (shipit-stacked-preview--text-change-p (cadr cell))
            (setcdr cell (cddr cell))
          (setq cell (cdr cell)))))))

(defun shipit-stacked-preview--after-change (beg end _length)
  "Make the text from BEG to END read-only, and keep it out of the undo history.
Text comes in with each drawing, and with each commit opened later.
The text is sticky at both ends, so nothing can be typed before the
first character either."
  (let ((inhibit-read-only t))
    (add-text-properties beg end '(read-only t front-sticky (read-only))))
  (shipit-stacked-preview--drop-text-changes))

(defun shipit-stacked-preview--rename (layer name)
  "Give LAYER the branch NAME.
A layer with only a derived name takes it for `c\=' to create; one
whose branch exists is renamed in git, checked out or not."
  (let ((old (cdr (assq 'branch layer)))
        (tip (cdr (assq 'tip layer))))
    (when (equal old name)
      (user-error "The layer is already called %s" name))
    (when (shipit-stack--rev name)
      (user-error "A branch called %s already exists" name))
    (when (shipit-stack--rev old)
      (shipit-stack--git-do "branch" "-m" old name))
    (when (cdr (assq 'existing layer))
      (setq shipit-stacked-preview--branch name))
    (setf (alist-get tip shipit-stacked-preview--names nil nil #'equal)
          name)))

(defun shipit-stacked-preview-rename-layer ()
  "Rename the layer at point.
The names the buffer proposes come from commit subjects; this is for
when the subjects name the work badly."
  (interactive)
  (let* ((layer (shipit-stacked-preview--layer-at-point))
         (name (read-string "Rename layer to: "
                            (cdr (assq 'branch layer)))))
    (when (> (length name) 0)
      (shipit-stacked-preview--rename layer name)
      (shipit-stacked-preview--render))))

(defun shipit-stacked-preview--layer-description (layer)
  "The work of LAYER, for a suggestion of its name: its commit messages."
  (mapconcat (lambda (commit)
               (or (plist-get commit :message)
                   (plist-get commit :first-line)
                   ""))
             (cdr (assq 'commits layer))
             "\n---\n"))

(defun shipit-stacked-preview--suggest-name-for (layer buffer)
  "Have `shipit-branch-name-suggester' name LAYER, confirm in BUFFER, apply.
The answer prefills the rename prompt rather than being applied as
it is: the suggester proposes, the reader decides."
  (unless shipit-branch-name-suggester
    (user-error "shipit: nothing suggests branch names here"))
  (with-current-buffer buffer
    (funcall
     shipit-branch-name-suggester
     (shipit-stacked-preview--layer-description layer)
     (lambda (answer)
       (when (and answer (buffer-live-p buffer))
         (with-current-buffer buffer
           (let ((name (read-string "Rename layer to: "
                                    (shipit-stack--slug answer))))
             (when (> (length name) 0)
               (shipit-stacked-preview--rename layer name)
               (when (derived-mode-p 'shipit-stacked-preview-mode)
                 (shipit-stacked-preview--render))))))))))

(defun shipit-stacked-preview-suggest-name ()
  "Have `shipit-branch-name-suggester' suggest a name for the layer at point.
Its commit messages go to the suggester; the answer prefills the
rename prompt, to confirm or edit."
  (interactive)
  (shipit-stacked-preview--suggest-name-for
   (shipit-stacked-preview--layer-at-point) (current-buffer)))

(defun shipit-stacked-preview-create-branches ()
  "Put a branch on each layer that has not got one.
Creating a branch on a commit that is already there moves nothing, so
this can be run again after dividing differently."
  (interactive)
  (let ((made 0))
    (dolist (layer (shipit-stacked-preview--current-layers))
      (let ((branch (cdr (assq 'branch layer))))
        (unless (or (cdr (assq 'existing layer)) (shipit-stack--rev branch))
          (shipit-stack--git-do "branch" branch (cdr (assq 'tip layer)))
          (setq made (1+ made)))))
    (shipit-stacked-preview--render)
    (message "shipit: %d branch%s created" made (if (= made 1) "" "es"))))

(defun shipit-stacked-preview-visit-layer ()
  "Open the layer at point.
A layer that has a pull request opens it.  The preview describes one
that does not have it yet, and offering it for a layer already
submitted would be offering to make a second.

Describing hands the layer to the ordinary preview -- a layer is a pull
request like any other, one whose base is the layer below -- and comes
back here once it has been submitted or abandoned, so the list shows
what it became.  The layer does not have to be checked out for this."
  (interactive)
  (shipit-stacked-preview--visit (shipit-stacked-preview--layer-at-point)))

(defun shipit-stacked-preview-visit ()
  "Open the layer at point, or ask for a new base when point is on the base."
  (interactive)
  (if (get-text-property (point) 'shipit-stack-base)
      (call-interactively #'shipit-stacked-preview-set-base)
    (shipit-stacked-preview-visit-layer)))

(defun shipit-stacked-preview--visit (layer)
  "Open LAYER, as `shipit-stacked-preview-visit-layer' describes.
The stack canvas opens a layer through this, with this buffer current."
  (let* ((branch (shipit-stacked-preview--require-branch layer))
         (number (shipit-stacked-preview--pr-number branch)))
    (if number
        (shipit-open-pr-buffer number shipit-stacked-preview--repo)
      (require 'shipit-preview)
      (shipit-stacked-preview--refuse-when-broken layer)
      (shipit-stacked-preview--refuse-when-base-is-absent layer)
      (shipit-preview-branch shipit-stacked-preview--repo branch
                             (shipit-stacked-preview--base-of layer)
                             :tip branch
                             :stacked t
                             :on-done (shipit-stacked-preview--return-to
                                       (current-buffer) branch)))))

(defun shipit-stacked-preview--return-to (buffer _branch)
  "Return a function that brings BUFFER up to date and shows it again.
Called once a layer\='s preview is finished with, so submitting one layer
leaves the list ready for the next.  The preview has already dropped
the remembered answer that the branch had no pull request."
  (lambda (number)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when number
          (shipit-stacked-preview--group-submitted))
        (shipit-stacked-preview--render))
      (pop-to-buffer buffer))))

(defun shipit-stacked-preview--forget-layer-prs ()
  "Drop what is remembered about which pull request each layer has.
A pull request made outside this buffer, on the web page or from
another preview, is otherwise hidden behind a miss remembered from
before it existed.  A merge on the forge is asked again too, and so
are the checks and reviews."
  (setq shipit-stacked-preview--merged-prs nil
        shipit-stacked-preview--known-statuses nil)
  (dolist (branch (cons (shipit-stacked-preview--base-branch)
                        (shipit-stacked-preview--branch-names
                         (shipit-stacked-preview--current-layers))))
    (when branch
      (shipit-cache-forget-branch-pr shipit-stacked-preview--repo branch))))

(defun shipit-stacked-preview--show-sending (branches)
  "Mark BRANCHES as on their way, and put that on screen now.
The push blocks until it finishes, so without asking for the screen to
be brought up to date the buffer would sit unchanged until it was done."
  (setq shipit-stacked-preview--sending branches)
  (shipit-stacked-preview--render)
  (redisplay))

(defun shipit-stacked-preview--push-through (layer args)
  "Push the branches from the bottom layer up to LAYER, in one push.
The layers below go too, asked for or not.  A pull request cannot be
raised against a base the remote does not have, so a layer sent on its
own would arrive unsubmittable: its commits are all there, but the
branch its pull request is based on is not.

One atomic push carries them all.  Git checks every branch\='s lease
before it moves any, so a refusal leaves the whole stack where it was
on the remote rather than half-moved.  A remote that caps the refs one
push may update gets the layers bottom-up in atomic chunks instead, and
a refusal then leaves a stack complete from the bottom up to the chunk
that was refused; the process buffer under $ says which."
  (shipit-stacked-preview--refuse-when-broken layer)
  (let ((branches (shipit-stacked-preview--branches-through layer)))
    (unwind-protect
        (progn
          (shipit-stacked-preview--show-sending branches)
          (unless (shipit--push-branches branches args)
            (user-error "Pushing %s refused -- every push is atomic, $ shows what landed"
                        (string-join branches ", "))))
      ;; However this ends, no layer may be left saying it is on its
      ;; way: after a failure that would be a lie standing until the
      ;; next redraw.
      (setq shipit-stacked-preview--sending nil)
      (shipit-stacked-preview--render))
    (message "Pushed %d branch%s%s" (length branches)
             (if (cdr branches) "es" "")
             (if (member "--dry-run" args)
                 " (dry run -- $ shows what it tried)"
               ""))))

(defun shipit-stacked-preview--push-arguments ()
  "Return the git arguments chosen in the push transient."
  (transient-args 'shipit-stacked-preview-push))

(defun shipit-stacked-preview--push-scope ()
  "Return (BUFFER . BRANCH), the preview and layer the push menu opened on.
BRANCH is nil when the menu opened away from every layer.  A suffix
runs in the buffer of the selected window, and the stack canvas opens
the menu from its own buffer, so the menu carries the preview along.
Without the menu, this buffer and the layer at point."
  (or (transient-scope 'shipit-stacked-preview-push)
      (cons (current-buffer) (shipit-stacked-preview--branch-at-point))))

(defun shipit-stacked-preview--push-in-scope (layer-of args)
  "Push with ARGS, in the preview the push menu opened on.
LAYER-OF is called there with the branch the menu opened on, or nil,
and returns the layer to push up to."
  (pcase-let ((`(,buffer . ,branch) (shipit-stacked-preview--push-scope)))
    (with-current-buffer buffer
      (shipit-stacked-preview--push-through (funcall layer-of branch) args))))

(transient-define-suffix shipit-stacked-preview-push-layer (args)
  "Push the layer the push menu opened on, and the layers below it."
  (interactive (list (shipit-stacked-preview--push-arguments)))
  (shipit-stacked-preview--push-in-scope
   (lambda (branch)
     (shipit-stacked-preview--layer-named
      (or branch (user-error "Point is not on a layer"))))
   args))

(transient-define-suffix shipit-stacked-preview-push-all (args)
  "Push every layer of the stack, from the base upwards."
  (interactive (list (shipit-stacked-preview--push-arguments)))
  (shipit-stacked-preview--push-in-scope
   (lambda (_branch) (car (last (shipit-stacked-preview--current-layers))))
   args))

(transient-define-prefix shipit-stacked-preview-push (buffer branch)
  "Push the branches of the stack in BUFFER to origin.
BRANCH is the layer the menu opened on, or nil away from every layer."
  ["Arguments"
   ;; Rewriting a lower layer and rebasing the ones above it is the
   ;; ordinary way a stack moves, and every layer above the rewrite
   ;; then needs the forced form.
   ("-f" "Force with lease" "--force-with-lease")
   ("-n" "Dry run" "--dry-run")
   ("-h" "Skip hooks" "--no-verify")]
  ["Push"
   ("p" "this layer and the ones below" shipit-stacked-preview-push-layer)
   ("P" "every layer" shipit-stacked-preview-push-all)]
  (interactive (list (current-buffer) (shipit-stacked-preview--branch-at-point)))
  (transient-setup 'shipit-stacked-preview-push nil nil
                   :scope (cons buffer branch)))

;;;; The branch menu

(transient-define-prefix shipit-stacked-preview-branch ()
  "Make and name the branches of the layers.
The keys follow magit\='s branch menu where it has the same action."
  ["Branch"
   ("c" "create the layer branches" shipit-stacked-preview-create-branches)
   ("m" "rename layer" shipit-stacked-preview-rename-layer)
   ("a" "suggest a name" shipit-stacked-preview-suggest-name)])

(defun shipit-stacked-preview--backend-call (operation &rest args)
  "Call OPERATION on this repository\='s backend with ARGS.
Dispatched rather than branched on.  A backend that has no stacks says
so by not carrying the operation, and is told about here rather than
part way through a sequence of writes."
  (let* ((resolved (shipit-pr--resolve-for-repo shipit-stacked-preview--repo))
         (fn (plist-get (car resolved) operation)))
    (unless fn
      (user-error "Stacks are not supported for %s" shipit-stacked-preview--repo))
    (apply fn (cdr resolved) args)))

(defun shipit-stacked-preview--base-branch ()
  "Return the branch this stack sits on, or nil when it is the default.
The default branch is never part of a stack, although a pull request
can have it as its head: one from master into a release branch is no
part of a feature stack built on master."
  (let ((base (and shipit-stacked-preview--base
                   (shipit-stacked-preview--base-name shipit-stacked-preview--base))))
    (and base
         (not (equal base (shipit-stacked-preview--base-name (shipit-stack--base-ref))))
         base)))

(defun shipit-stacked-preview--base-pr-number ()
  "Return the open pull request this stack sits on, or nil.
A stack raised over a pull request that is already open is measured
from that pull request\='s branch, which leaves it out of the layers.
It is still the bottom of the stack GitHub groups."
  (let ((branch (shipit-stacked-preview--base-branch)))
    (and branch (shipit-stacked-preview--pr-number branch))))

(defun shipit-stacked-preview--submitted-numbers ()
  "Return the pull requests made from this stack, bottom first.
The pull request the stack sits on comes first, when there is one.
Stops at the first layer without one.  A stack is a chain, so only an
unbroken run from the bottom can be registered as one; the layers above
a gap are not part of it yet."
  (let ((numbers (let ((base (shipit-stacked-preview--base-pr-number)))
                   (and base (list base)))))
    (catch 'gap
      (dolist (branch (shipit-stacked-preview--branch-names
                       (shipit-stacked-preview--current-layers)))
        (let ((number (shipit-stacked-preview--pr-number branch)))
          (unless number (throw 'gap nil))
          (push number numbers))))
    (nreverse numbers)))

(defun shipit-stacked-preview--stack-of (number)
  "Return the stack NUMBER is in as (STACK . MEMBERS), or nil."
  (let ((stack (shipit-pr--fetch-stack shipit-stacked-preview--repo number)))
    (when (cdr (assq 'number stack))
      (cons (cdr (assq 'number stack))
            (mapcar (lambda (entry) (cdr (assq 'number entry)))
                    (cdr (assq 'entries stack)))))))

(defun shipit-stacked-preview--add-missing (existing numbers)
  "Add each of NUMBERS that EXISTING does not already hold, bottom first.
Counts what the remote accepted, not what was attempted: a refused
add reported as added hid a wrong payload for as long as this
existed.  Return non-nil when the remote accepted each."
  (let ((stack (car existing))
        (added 0)
        (failed nil))
    (dolist (number numbers)
      (unless (member number (cdr existing))
        (if (shipit-stacked-preview--backend-call :add-to-stack stack number)
            (setq added (1+ added))
          (push number failed))))
    (if failed
        (message "Stack %s: %d added, refused %s -- the error is above"
                 stack added
                 (mapconcat #'number-to-string (nreverse failed) ", "))
      (message "Added %d pull request%s to stack %s"
               added (if (= added 1) "" "s") stack))
    (null failed)))

(defun shipit-stacked-preview--create-stack (numbers)
  "Make NUMBERS, bottom first, a stack on the remote; non-nil when it took."
  (when (shipit-stacked-preview--backend-call :create-stack numbers)
    (message "Grouped %d pull requests into a stack" (length numbers))
    t))

(defun shipit-stacked-preview--group (numbers existing)
  "Put NUMBERS, bottom first, into one stack on the remote.
EXISTING is the stack that holds the lower of them already, as (STACK
. MEMBERS), or nil.  The ones it does not hold are added to it, so
submitting a further layer extends that stack rather than making a
second one beside it.  Return non-nil when the remote took all."
  (if existing
      (shipit-stacked-preview--add-missing existing numbers)
    (shipit-stacked-preview--create-stack numbers)))

(defun shipit-stacked-preview--wanted-bases ()
  "Return (NUMBER . BASE) for each layer with a pull request, bottom first.
BASE is the branch the pull request must be raised against: the branch
of the layer below, or the branch the stack sits on.  Stops at the
first layer without a pull request, as the stack does."
  (let ((below (shipit-stacked-preview--base-name shipit-stacked-preview--base))
        (pairs nil))
    (catch 'gap
      (dolist (branch (shipit-stacked-preview--branch-names
                       (shipit-stacked-preview--current-layers)))
        (let ((number (shipit-stacked-preview--pr-number branch)))
          (unless number (throw 'gap nil))
          (push (cons number below) pairs)
          (setq below branch))))
    (nreverse pairs)))

(defun shipit-stacked-preview--pr-base (number)
  "Return the branch that pull request NUMBER is raised against on the remote."
  (let ((pr (shipit-stacked-preview--backend-call :fetch-pr number)))
    (cdr (assq 'ref (cdr (assq 'base pr))))))

(defun shipit-stacked-preview--wrong-bases ()
  "Return the (NUMBER . BASE) pairs whose pull request has another base.
After a split, the pull request above the new layer is still raised
against the layer that was below it."
  (seq-remove (lambda (pair)
                (equal (cdr pair) (shipit-stacked-preview--pr-base (car pair))))
              (shipit-stacked-preview--wanted-bases)))

(defun shipit-stacked-preview--set-base (pair)
  "Make the cdr of PAIR the base of pull request (car PAIR).
Return non-nil when the remote took it, and say so when it did not."
  (or (shipit-stacked-preview--backend-call
       :update-pr (car pair) `((base . ,(cdr pair))))
      (progn (message "shipit: could not make %s the base of #%s"
                      (cdr pair) (car pair))
             nil)))

(defun shipit-stacked-preview--set-bases (pairs)
  "Give each pull request in PAIRS its base, and stop at the first refusal.
Return non-nil when the remote took all."
  (seq-every-p #'shipit-stacked-preview--set-base pairs))

(defun shipit-stacked-preview--stack-holding (numbers)
  "Return the stack on the remote that holds one of NUMBERS, or nil.
The answer is (STACK . MEMBERS)."
  (seq-some #'shipit-stacked-preview--stack-of numbers))

(defun shipit-stacked-preview--extends-p (members numbers)
  "Return non-nil when NUMBERS are MEMBERS with more pull requests on top."
  (equal members (seq-take numbers (length members))))

(defun shipit-stacked-preview--members-above (members numbers)
  "Return the MEMBERS of a stack above the highest of NUMBERS in it.
They are the pull requests of layers that this buffer does not show,
as when it was opened on a branch lower down the stack."
  (let ((top (seq-find (lambda (member) (memq member numbers))
                       (reverse members))))
    (and top (cdr (memq top members)))))

(defun shipit-stacked-preview--rebuild (existing numbers wrong)
  "Dissolve the stack EXISTING, set the bases in WRONG, and stack NUMBERS.
GitHub refuses to change the base of a pull request in a stack, and it
adds pull requests only on top, so a stack that must change in the
middle goes first.  When a base is refused, the old stack is made
again, since its bases still form a chain.  Return non-nil when
NUMBERS are a stack in the end."
  (cond
   ((not (shipit-stacked-preview--backend-call :dissolve-stack (car existing)))
    (message "shipit: could not dissolve stack %s -- press R to try again"
             (car existing))
    nil)
   ((not (shipit-stacked-preview--set-bases wrong))
    (if (shipit-stacked-preview--create-stack (cdr existing))
        (message "shipit: the stack is as it was -- press R to try again")
      (message "shipit: the pull requests have no stack now -- press R to try again"))
    nil)
   ((shipit-stacked-preview--create-stack numbers) t)
   (t (message "shipit: could not build the stack again -- press R to try again")
      nil)))

(defun shipit-stacked-preview--restack-on-remote (numbers wrong)
  "Make NUMBERS one stack on the remote, after setting the bases in WRONG.
The pull requests of the stack there that sit above NUMBERS stay on
top of them.  Return non-nil when that worked."
  (let* ((existing (shipit-stacked-preview--stack-holding numbers))
         (numbers (append numbers (shipit-stacked-preview--members-above
                                   (cdr existing) numbers))))
    (if (and existing
             (or wrong (not (shipit-stacked-preview--extends-p
                             (cdr existing) numbers))))
        (shipit-stacked-preview--rebuild existing numbers wrong)
      (and (shipit-stacked-preview--set-bases wrong)
           (shipit-stacked-preview--group numbers existing)))))

(defun shipit-stacked-preview--restack (numbers)
  "Make the pull requests NUMBERS, bottom first, follow the layers.
Each pull request gets the branch of the layer below as its base, and
on a forge with stacks NUMBERS become one stack in this order.  When a
step fails, the next step of the preview says to press R."
  (let ((wrong (shipit-stacked-preview--wrong-bases)))
    (setq shipit-stacked-preview--stack-to-rebuild
          (not (if (shipit-pr--stack-supported-p shipit-stacked-preview--repo)
                   (shipit-stacked-preview--restack-on-remote numbers wrong)
                 (shipit-stacked-preview--set-bases wrong))))))

(defun shipit-stacked-preview--group-submitted ()
  "Group the pull requests submitted so far, once there are two.
Pull requests based on one another are not a stack: the remote checks
the chain but will not group them, so without this they stay unrelated
to anyone reading them."
  (let ((numbers (shipit-stacked-preview--submitted-numbers)))
    (when (cdr numbers)
      (shipit-stacked-preview--restack numbers))))

(defun shipit-stacked-preview-register ()
  "Group the pull requests made from this stack into a stack on the remote.
This happens as each layer is submitted; the key is for saying it again
after a failure, or for a stack whose pull requests were made
elsewhere.  A stack in another order, as after a split, is built again
in the order of the layers."
  (interactive)
  (shipit-stacked-preview--forget-layer-prs)
  (let ((numbers (shipit-stacked-preview--submitted-numbers)))
    (unless (cdr numbers)
      (user-error "A stack needs two pull requests, and %d %s been made"
                  (length numbers) (if (= (length numbers) 1) "has" "have")))
    (shipit-stacked-preview--restack numbers)
    (shipit-stacked-preview--render)))

(defun shipit-stacked-preview--fetch ()
  "Bring the remote-tracking refs up to date, pruned; return nil or why not.
The rows say where each layer\='s copy on the remote stands, and a bot
that rebases a stack after the pull request under it merges moves those
copies without anyone here hearing of it.  A branch deleted there, as
after a merge, goes too.  Every branch rather than the stack\='s own: a
fetch of a branch the remote no longer has fails outright, and the
names a stack will take are not known until it is divided.  A failure
is returned rather than signalled: the refs still here draw a stack
that is only as stale as it was, and the buffer says so."
  (pcase-let ((`(,exit . ,output)
               (shipit--run-git "fetch" "--prune" "--quiet" "origin")))
    (unless (zerop exit)
      (format "Fetch from origin failed: %s"
              (or (car (split-string output "\n" t)) "no reason given")))))

(defun shipit-stacked-preview--insert-fetch-problem ()
  "Say that the last fetch failed, when it did."
  (when shipit-stacked-preview--fetch-problem
    (insert (propertize (format "   %s\n\n" shipit-stacked-preview--fetch-problem)
                        'font-lock-face 'font-lock-warning-face))))

(defun shipit-stacked-preview--new-base ()
  "Return the commit the stack should move onto, or nil.
Where the copy on the remote of the lowest layer whose copy was rebased
stands over the stack\='s base.  The lowest, since the copies above it
were rebased with it, and the layers below it have nothing to follow."
  (seq-some (lambda (layer)
              (shipit-stacked-preview--remote-base-moved
               (cdr (assq 'branch layer))))
            (shipit-stacked-preview--current-layers)))

(defun shipit-stacked-preview--redraw-after-magit (buffer)
  "Redraw BUFFER the next time magit refreshes, and only then.
Magit refreshes when the rebase it runs ends, finished or stopped on a
conflict, and either is worth seeing here."
  (letrec ((redraw (lambda ()
                     (remove-hook 'magit-post-refresh-hook redraw)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (shipit-stacked-preview-refresh))))))
    (add-hook 'magit-post-refresh-hook redraw)))

(defun shipit-stacked-preview--rebase-arguments ()
  "Return the arguments of the rebase menu, with `--update-refs\=' first.
Without it the branches of the layers above stay on the old commits
and the stack splits in two, so it is no choice of the menu."
  (cons "--update-refs" (transient-args 'shipit-stacked-preview-rebase)))

(defun shipit-stacked-preview--refuse-unless-checked-out ()
  "Signal unless the top layer of the stack is checked out."
  (unless (equal (shipit-stack--current-branch) shipit-stacked-preview--branch)
    (user-error "Check out %s first; the rebase moves the checked-out branch"
                shipit-stacked-preview--branch)))

(defun shipit-stacked-preview--rebase-subset (newbase start args)
  "Have magit rebase the commits from START to HEAD onto NEWBASE with ARGS.
Magit runs the rebase, so a conflict lands in its usual flow, and this
buffer redraws once magit has."
  (shipit-stacked-preview--refuse-unless-checked-out)
  (require 'magit-sequence)
  (shipit-stacked-preview--redraw-after-magit (current-buffer))
  (magit-rebase-subset newbase start args))

(defun shipit-stacked-preview-rebase-onto-remote-base (&optional args)
  "Move the stack onto the base its copies on the remote stand on.
After the pull request under a stack merges, a bot rebases the copies
on the remote onto the base that now holds it, and a stack rewritten
here in the meantime has to go the same way before it can be pushed.
The commits over the stack\='s base are rebased onto the commit the
lowest rebased copy stands on, with every layer\='s branch following.
Commits the base already holds, as a merged layer\='s do, are dropped
by git as it goes.  ARGS are those of the rebase menu."
  (interactive (list (shipit-stacked-preview--rebase-arguments)))
  (let ((newbase (shipit-stacked-preview--new-base)))
    (unless newbase
      (user-error "No layer's copy on the remote stands on a newer base -- nothing to rebase onto"))
    (shipit-stacked-preview--rebase-subset
     newbase shipit-stacked-preview--base
     (or args (shipit-stacked-preview--rebase-arguments)))))

(defun shipit-stacked-preview--merged-at-bottom ()
  "Return the merged layers at the bottom of the stack, bottom first.
Only those: a layer can merge only after the layers below it did."
  (seq-take-while #'shipit-stacked-preview--merged-state
                  (shipit-stacked-preview--current-layers)))

(defun shipit-stacked-preview--rebase-start ()
  "Return the commit after which the rebase onto the base takes commits.
The tip of the highest merged layer at the bottom, so its commits are
left out, or the base when none merged."
  (if-let* ((merged (car (last (shipit-stacked-preview--merged-at-bottom)))))
      (cdr (assq 'tip merged))
    shipit-stacked-preview--base))

(defun shipit-stacked-preview--onto-base-p ()
  "Return non-nil when a rebase onto the base would move the stack.
That is when a layer at the bottom merged, or when the base moved past
where the stack stands.  Not when every layer merged: then nothing is
left to move."
  (let ((merged (length (shipit-stacked-preview--merged-at-bottom))))
    (and (< merged (length (shipit-stacked-preview--current-layers)))
         (or (> merged 0)
             (not (shipit-stack--ancestor-p shipit-stacked-preview--base "HEAD"))))))

(defun shipit-stacked-preview-rebase-onto-base (args)
  "Rebase the stack onto the newest base, and leave the merged layers out.
ARGS are those of the rebase menu."
  (interactive (list (shipit-stacked-preview--rebase-arguments)))
  (shipit-stacked-preview--rebase-subset
   shipit-stacked-preview--base (shipit-stacked-preview--rebase-start) args))

(defun shipit-stacked-preview--fixup-p (commit)
  "Return non-nil when COMMIT is a fixup, a squash or an amend of another."
  (string-match-p "\\`\\(fixup\\|squash\\|amend\\)! "
                  (or (plist-get commit :first-line) "")))

(defun shipit-stacked-preview--fixups-p ()
  "Return non-nil when the stack holds a commit to fold into another."
  (seq-some #'shipit-stacked-preview--fixup-p shipit-stacked-preview--commits))

(defun shipit-stacked-preview-rebase-fixups (args)
  "Fold each fixup commit of the stack into the commit it fixes.
The stack stays on the commit of the base it stands on, and every
layer\='s branch follows its commits.  Git needs an interactive rebase
for this; the list of commits opens only with `--interactive\=' among
ARGS, the arguments of the rebase menu."
  (interactive (list (shipit-stacked-preview--rebase-arguments)))
  (shipit-stacked-preview--refuse-unless-checked-out)
  (require 'magit-sequence)
  (shipit-stacked-preview--redraw-after-magit (current-buffer))
  (let ((process-environment process-environment))
    (unless (member "--interactive" args)
      (push "GIT_SEQUENCE_EDITOR=true" process-environment))
    (magit-run-git-sequencer "rebase" "-i" "--autosquash" "--keep-base"
                             (remove "--interactive" args)
                             shipit-stacked-preview--base)))

(defun shipit-stacked-preview--continue-with-magit (command)
  "Run the magit COMMAND that goes on with a stopped rebase, and redraw after."
  (require 'magit-sequence)
  (shipit-stacked-preview--redraw-after-magit (current-buffer))
  (call-interactively command))

(defun shipit-stacked-preview-rebase-continue ()
  "Continue the stopped rebase, as magit does."
  (interactive)
  (shipit-stacked-preview--continue-with-magit #'magit-rebase-continue))

(defun shipit-stacked-preview-rebase-skip ()
  "Skip the commit the rebase stopped at, as magit does."
  (interactive)
  (shipit-stacked-preview--continue-with-magit #'magit-rebase-skip))

(defun shipit-stacked-preview-rebase-abort ()
  "Abort the stopped rebase, as magit does."
  (interactive)
  (shipit-stacked-preview--continue-with-magit #'magit-rebase-abort))

(defun shipit-stacked-preview--rebase-in-progress-p ()
  "Return non-nil when a rebase is stopped in this repository."
  (require 'magit-sequence)
  (magit-rebase-in-progress-p))

(defun shipit-stacked-preview--plan-words (merged moves)
  "Return what a rebase onto the base does to a layer.
MERGED is the merged state of the layer, or nil.  MOVES is non-nil
when the rebase moves the stack at all."
  (let ((base (shipit-stacked-preview--base-name shipit-stacked-preview--base)))
    (cond ((plist-get merged :number)
           (format "merged in #%s, left out" (plist-get merged :number)))
          (merged "merged, left out")
          (moves (format "moves onto %s" base))
          (t (format "already on %s" base)))))

(defun shipit-stacked-preview--plan-text ()
  "Return the plan of a rebase onto the base, a line for each layer, top first.
Transient puts one space before the text, so each further line starts
with one too."
  (let* ((layers (shipit-stacked-preview--current-layers))
         (merged (shipit-stacked-preview--merged-at-bottom))
         (moves (shipit-stacked-preview--onto-base-p))
         (width (shipit-stacked-preview--name-width layers)))
    (mapconcat
     (lambda (layer)
       (format "%s  %s"
                (shipit-sections--pad
                 (propertize (cdr (assq 'branch layer))
                             'face 'magit-branch-local)
                 width)
                (shipit-stacked-preview--plan-words
                 (and (member layer merged)
                      (shipit-stacked-preview--merged-state layer))
                 moves)))
     (reverse layers) "\n ")))

(defun shipit-stacked-preview--onto-base-label ()
  "Return the label of the rebase onto the base."
  (format "onto %s, leaving the merged layers out"
          (shipit-stacked-preview--base-name shipit-stacked-preview--base)))

(defun shipit-stacked-preview--in-menu (fn)
  "Call FN in the preview that the open menu belongs to, and return its value.
Transient draws a menu again after each option, in a buffer of its own
where the stack of the preview is not known."
  (with-current-buffer (or (transient-scope) (current-buffer))
    (funcall fn)))

(defun shipit-stacked-preview--menu-stopped-p ()
  "Return non-nil when a rebase is stopped, for the rebase menu."
  (shipit-stacked-preview--in-menu #'shipit-stacked-preview--rebase-in-progress-p))

(defun shipit-stacked-preview--menu-plan ()
  "Return the plan of a rebase onto the base, for the rebase menu."
  (shipit-stacked-preview--in-menu #'shipit-stacked-preview--plan-text))

(defun shipit-stacked-preview--menu-onto-base-p ()
  "Return non-nil when the rebase menu offers the rebase onto the base."
  (shipit-stacked-preview--in-menu #'shipit-stacked-preview--onto-base-p))

(defun shipit-stacked-preview--menu-onto-base-label ()
  "Return the label of the rebase onto the base, for the rebase menu."
  (shipit-stacked-preview--in-menu #'shipit-stacked-preview--onto-base-label))

(defun shipit-stacked-preview--menu-remote-moved-p ()
  "Return non-nil when the rebase menu offers to follow the remote copies."
  (shipit-stacked-preview--in-menu #'shipit-stacked-preview--new-base))

(defun shipit-stacked-preview--menu-fixups-p ()
  "Return non-nil when the rebase menu offers to apply the fixups."
  (shipit-stacked-preview--in-menu #'shipit-stacked-preview--fixups-p))

(transient-define-prefix shipit-stacked-preview-rebase ()
  "Rebase the stack: shipit works out each rebase, and magit runs it.
The keys follow magit\='s rebase menu where it has the same action."
  :value '("--autostash")
  ["Plan"
   :if-not shipit-stacked-preview--menu-stopped-p
   (:info* #'shipit-stacked-preview--menu-plan)]
  ["Arguments"
   :if-not shipit-stacked-preview--menu-stopped-p
   ("-A" "Autostash" "--autostash")
   ("-i" "Interactive" "--interactive")]
  ["Rebase"
   :if-not shipit-stacked-preview--menu-stopped-p
   ("b" shipit-stacked-preview-rebase-onto-base
    :description shipit-stacked-preview--menu-onto-base-label
    :if shipit-stacked-preview--menu-onto-base-p)
   ("o" "onto the remote copies, which a bot rebased"
    shipit-stacked-preview-rebase-onto-remote-base
    :if shipit-stacked-preview--menu-remote-moved-p)
   ("f" "apply the fixups to their layers" shipit-stacked-preview-rebase-fixups
    :if shipit-stacked-preview--menu-fixups-p)]
  ["Rebase in progress"
   :if shipit-stacked-preview--menu-stopped-p
   ("r" "Continue" shipit-stacked-preview-rebase-continue)
   ("s" "Skip" shipit-stacked-preview-rebase-skip)
   ("a" "Abort" shipit-stacked-preview-rebase-abort)]
  (interactive)
  (transient-setup 'shipit-stacked-preview-rebase nil nil
                   :scope (current-buffer)))

(defun shipit-stacked-preview--divisions-stale-p ()
  "Return non-nil when a division no longer names one of the commits.
A rebase gives every commit a new id, and the divisions are ids."
  (let ((shas (mapcar (lambda (commit) (plist-get commit :sha))
                      shipit-stacked-preview--commits)))
    (seq-some (lambda (sha) (not (member sha shas)))
              shipit-stacked-preview--boundaries)))

(defun shipit-stacked-preview--follow-the-rebase ()
  "Divide the commits again when a rebase gave them new ids.
The layer branches moved with the rebase, so they say where the
divisions are; without branches the subjects propose them again.
Not while a rebase is stopped: HEAD then holds only the commits done
so far, under new ids, and an abort brings back the commits that the
divisions name."
  (when (and (not (shipit-stacked-preview--rebase-in-progress-p))
             (shipit-stacked-preview--divisions-stale-p))
    (let ((existing (shipit-stacked-preview--from-existing-stack)))
      (setq shipit-stacked-preview--boundaries
            (or (car existing)
                (shipit-stacked-preview--proposed-boundaries
                 shipit-stacked-preview--commits))
            shipit-stacked-preview--names (cdr existing)))))

(defun shipit-stacked-preview-refresh ()
  "Fetch, read the commits and each layer\='s pull request again, and redraw."
  (interactive)
  (setq shipit-stacked-preview--fetch-problem (shipit-stacked-preview--fetch))
  (shipit-stacked-preview--forget-layer-prs)
  (setq shipit-stacked-preview--commits
        (shipit--commits-in-range
         (format "%s..HEAD" shipit-stacked-preview--base)))
  (shipit-stacked-preview--follow-the-rebase)
  (shipit-stacked-preview--render))

(defun shipit-stacked-preview-canvas ()
  "Draw this stack as a graph on a canvas that follows this buffer.
It needs Emacs 32 with canvas-graph.  Without them this says so and
leaves the buffer as it is; the canvas file is not loaded until then."
  (interactive)
  (if (shipit-canvas-available-p 'canvas-graph)
      (progn
        (require 'shipit-stack-canvas)
        (shipit-stack-canvas-show (current-buffer)))
    (message "The stack canvas needs Emacs 32 with canvas-diagram and canvas-graph")))

(defvar shipit-stacked-preview-mode-map (make-sparse-keymap)
  "Keymap for `shipit-stacked-preview-mode'.")

;; The keys are set at top level, not inside the `defvar': a reload of
;; this file keeps a keymap that already exists, but runs these again.
(set-keymap-parent shipit-stacked-preview-mode-map magit-section-mode-map)
(define-key shipit-stacked-preview-mode-map (kbd "s") #'shipit-stacked-preview-toggle-division)
(define-key shipit-stacked-preview-mode-map (kbd "RET") #'shipit-stacked-preview-visit)
(define-key shipit-stacked-preview-mode-map (kbd "P") #'shipit-stacked-preview-push)
(define-key shipit-stacked-preview-mode-map (kbd "R") #'shipit-stacked-preview-register)
(define-key shipit-stacked-preview-mode-map (kbd "b") #'shipit-stacked-preview-branch)
(define-key shipit-stacked-preview-mode-map (kbd "r") #'shipit-stacked-preview-rebase)
(define-key shipit-stacked-preview-mode-map (kbd "v") #'shipit-stacked-preview-canvas)
(define-key shipit-stacked-preview-mode-map (kbd "g") #'shipit-stacked-preview-refresh)
(define-key shipit-stacked-preview-mode-map (kbd "$") #'shipit-process-buffer)
(define-key shipit-stacked-preview-mode-map (kbd "q") #'quit-window)

(define-derived-mode shipit-stacked-preview-mode magit-section-mode "Shipit Stack"
  "Divide a line of commits into stacked pull requests."
  ;; The same section defaults every other shipit buffer applies, which
  ;; among other things drops magit's left-margin expansion markers.
  ;; Without this the commits here carry markers the ones in the pull
  ;; request buffer do not, and the two stop looking alike.
  (shipit--apply-section-defaults)
  (setq-local revert-buffer-function
              (lambda (&rest _) (shipit-stacked-preview-refresh)))
  ;; Writable with read-only text, and undo on: see "The undo history".
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (add-hook 'after-change-functions #'shipit-stacked-preview--after-change nil t)
  (add-hook 'window-size-change-functions #'shipit-stacked-preview--window-resized nil t)
  ;; The preview often stands beside the canvas of a stack or an undo
  ;; tree, so its commit rows follow a window narrower than the 80
  ;; columns of a pull request buffer.
  (setq-local shipit--commit-row-min-width 40))

;;;###autoload
(defun shipit-stacked-preview (&optional choose-base)
  "Preview the stack the commits on this branch would become.
What `shipit-preview\=' is to one pull request, this is to several: the
commits with divisions proposed from what their subjects have in
common.  Move the divisions, put a branch on each layer, then hand
each layer to `shipit-preview\=' to describe and submit.

With a prefix argument CHOOSE-BASE, ask which branch the stack sits on
instead of measuring from the default branch, for a stack raised
against a release branch.  RET on the base in the buffer asks the same."
  (interactive "P")
  (let* ((repo (shipit--get-repo-from-remote))
         ;; The buffer runs git where it was opened.  One opened before
         ;; from another directory, for a branch of the same name, would
         ;; otherwise keep running it there.
         (directory default-directory)
         (fetch-problem (shipit-stacked-preview--fetch))
         ;; Read before the base: the branches already dividing this
         ;; branch are the stack's own layers, and their pull requests
         ;; must not pull the base up to the layer below the top.
         (existing (shipit-stacked-preview--from-existing-stack))
         (base (if choose-base
                   (shipit-stacked-preview--read-base)
                 (shipit-stacked-preview--detect-base
                  repo (shipit-stack--base-ref)
                  (mapcar #'cdr (cdr existing)))))
         (branch (or (shipit-stack--current-branch)
                     (user-error "Not on a branch")))
         (commits (shipit-stacked-preview--commits-over base)))
    (let ((buffer (get-buffer-create
                   (format "*shipit-stacked-preview: %s*" branch))))
      (with-current-buffer buffer
        (shipit-stacked-preview-mode)
        (setq default-directory directory
              shipit-stacked-preview--repo repo
              shipit-stacked-preview--fetch-problem fetch-problem
              shipit-stacked-preview--branch branch)
        (shipit-stacked-preview--set-stack base commits existing)
        (shipit-stacked-preview--render))
      (switch-to-buffer buffer))))

(defun shipit-stacked-preview--base-name (ref)
  "Return REF as a branch name, without the remote it was read from.
The remote copy is what the commits are measured from and what the
bottom layer is raised against, but the name on its own is what reads
in the buffer and what the remote is asked for."
  (replace-regexp-in-string "\\`origin/" "" ref))

(defun shipit-stacked-preview--branch-names (layers)
  "Return the branch name of each of LAYERS."
  (mapcar (lambda (layer) (cdr (assq 'branch layer))) layers))

(defun shipit-stacked-preview--branches-through (layer)
  "Return the branch names from the bottom layer up to LAYER."
  (let* ((layers (shipit-stacked-preview--current-layers))
         (position (seq-position layers layer)))
    (unless position (user-error "That layer is no longer in this stack"))
    (shipit-stacked-preview--branch-names (seq-take layers (1+ position)))))

(defun shipit-stacked-preview--refuse-when-broken (layer)
  "Signal unless the layers up to LAYER form a stack, saying what is wrong.
Checked before anything leaves this machine.  A break in the line of
branches makes pull requests that carry each other\'s commits, and the
layers below the one being submitted are as much a part of that as the
layer itself: each is the base of the next."
  (let ((problems (shipit-stack-problems
                   (shipit-stacked-preview--branches-through layer)
                   shipit-stacked-preview--base)))
    (when problems
      (user-error "Not a stack yet: %s" (string-join problems "; ")))))

(defun shipit-stacked-preview--push-stack-base (name)
  "Offer to push NAME, the branch the stack sits on, and signal unless it lands.
NAME must be a branch here.  The push is the one the layers get."
  (unless (y-or-n-p (format "The base %s is not on the remote.  Push it? " name))
    (user-error "The base %s is not on the remote yet -- push it, or press RET on the base to pick another"
                name))
  (unless (shipit--push-branch name)
    (user-error "Pushing the base %s failed -- $ shows what git said" name)))

(defun shipit-stacked-preview--ensure-stack-base-on-remote ()
  "Make sure that the branch the stack sits on is on the remote, or signal.
The bottom layer is raised against it.  A base picked with RET or a
prefix argument can be a branch made here and never pushed, and `P'
pushes only the layers, so the push is offered here.  A tag or a
single commit as the base cannot become a branch on the remote by a
push of its name, so it is refused without a question."
  (let ((name (shipit-stacked-preview--base-name shipit-stacked-preview--base)))
    (unless (shipit-stack--rev (concat "origin/" name))
      (unless (shipit-stack--rev (concat "refs/heads/" name))
        (user-error "The base %s is not a branch on the remote -- press RET on the base to pick a branch"
                    name))
      (shipit-stacked-preview--push-stack-base name))))

(defun shipit-stacked-preview--refuse-when-base-is-absent (layer)
  "Signal when the base or a layer below LAYER has not reached the remote.
A pull request is raised against a branch the remote already has, and
LAYER is raised against the layer below it.  Caught here rather than
left to the remote, which would refuse the created pull request with a
message about a base ref, well after the description had been written."
  (shipit-stacked-preview--ensure-stack-base-on-remote)
  (let* ((below (butlast (shipit-stacked-preview--branches-through layer)))
         (absent (seq-remove (lambda (branch)
                               (shipit-stack--rev (concat "origin/" branch)))
                             below)))
    (when absent
      (user-error "%s not on the remote yet -- press P to send %s"
                  (string-join absent ", ")
                  (if (cdr absent) "them" "it")))))

(defun shipit-stacked-preview--problems (layers)
  "Return what stops LAYERS being submitted, or nil when nothing does.
Only once every layer has a branch: before that the layer rows already
say which ones are still to be made, and a branch not yet made is not a
fault."
  (let ((branches (shipit-stacked-preview--branch-names layers)))
    (when (seq-every-p #'shipit-stack--rev branches)
      (shipit-stack-problems branches shipit-stacked-preview--base))))

(defun shipit-stacked-preview--insert-problems ()
  "Say what stops this stack being submitted, as the last look-up found it."
  (let ((problems shipit-stacked-preview--problems-shown))
    (dolist (problem problems)
      (insert (propertize (format "   %s\n" problem)
                          'font-lock-face 'font-lock-warning-face)))
    (when problems (insert "\n"))))

(provide 'shipit-stacked-preview)
;;; shipit-stacked-preview.el ends here
