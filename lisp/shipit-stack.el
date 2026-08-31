;;; shipit-stack.el --- Detect a stack of branches from local git -*- lexical-binding: t; -*-

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
;; Works out whether the checked-out branch sits on top of other
;; branches that have not reached the default branch yet, which is what
;; a stack of pull requests looks like before any of it is pushed.
;;
;; Nothing local records that one branch targets another.  Git only
;; knows where a branch was cut from, so the chain has to be recovered
;; from ancestry.  The test that does the work is containment in HEAD
;; but NOT in the default branch: an old branch merged long ago is an
;; ancestor of everything and would otherwise swamp the result.

;;; Code:

(require 'shipit-core)

(defun shipit-stack--git-lines (&rest args)
  "Run git with ARGS and return stdout as a list of non-empty lines."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil args)))
      (when (zerop exit)
        (seq-remove #'string-empty-p
                    (mapcar #'string-trim
                            (split-string (buffer-string) "\n")))))))

(defun shipit-stack--git-do (&rest args)
  "Run git with ARGS for its effect, logged, erroring on failure.
The reads stay off the log: `shipit-stack--git-lines\=' runs
constantly and would bury the commands worth seeing under $."
  (pcase-let ((`(,exit . ,output) (apply #'shipit--run-git args)))
    (unless (zerop exit)
      (error "git %s failed: %s" (string-join args " ") output))
    output))

(defun shipit-stack--rev (ref)
  "Return the commit REF resolves to, or nil when it does not exist."
  (car (shipit-stack--git-lines "rev-parse" "--verify" "-q" ref)))

(defun shipit-stack--current-branch ()
  "Return the checked-out branch name, or nil when detached."
  (let ((branch (car (shipit-stack--git-lines
                      "rev-parse" "--abbrev-ref" "HEAD"))))
    (unless (equal branch "HEAD") branch)))

(defun shipit-stack--base-ref ()
  "Return the ref a stack is measured from.
Prefers the remote-tracking default branch, falling back to the local
one so detection still works where origin has never been fetched."
  (let ((default (shipit--upstream-default-branch)))
    (if (shipit-stack--rev (concat "origin/" default))
        (concat "origin/" default)
      default)))

(defun shipit-stack--contained-in (ref)
  "Return the local branches whose tip is an ancestor of REF."
  (shipit-stack--git-lines "branch" "--merged" ref
                           "--format=%(refname:short)"))

(defun shipit-stack--ancestor-p (a b)
  "Return non-nil when commit A is an ancestor of commit B."
  (zerop (call-process "git" nil nil nil
                       "merge-base" "--is-ancestor" a b)))

(defun shipit-stack--depth (base ref)
  "Return how many commits REF carries beyond BASE."
  (string-to-number
   (or (car (shipit-stack--git-lines
             "rev-list" "--count" (format "%s..%s" base ref)))
       "0")))

(defun shipit-stack--candidate-branches (base)
  "Return branches contained in HEAD but not in BASE.
A branch already merged into BASE is an ancestor of HEAD only because
it got there first, which makes it no part of the stack."
  (let ((below (shipit-stack--contained-in base)))
    (seq-remove (lambda (branch) (member branch below))
                (shipit-stack--contained-in "HEAD"))))

(defun shipit-stack--layer (tip branches base current-branch)
  "Return the layer at TIP named by BRANCHES, measured from BASE.
Several names can sit on one commit.  They are one layer, so the
extra names are recorded as aliases rather than counted as depth.
CURRENT-BRANCH wins the naming when it is among them."
  (let* ((sorted (sort (copy-sequence branches) #'string<))
         (checked-out (member current-branch sorted))
         (name (if checked-out current-branch (car sorted)))
         (aliases (delete name sorted)))
    `((branch . ,name)
      (aliases . ,aliases)
      ;; Several names on one commit and no way to tell which is the
      ;; layer: one of them may be a backup kept while the branches
      ;; were rearranged.  Being checked out settles it, nothing else
      ;; does, so say so rather than pick and target the wrong branch.
      (ambiguous . ,(and aliases (not checked-out) t))
      (tip . ,tip)
      (depth . ,(shipit-stack--depth base tip))
      (current . ,(and (equal name current-branch) t)))))

(defun shipit-stack--group-by-tip (branches)
  "Group BRANCHES into an alist of (TIP . NAMES)."
  (let (groups)
    (dolist (branch branches)
      (let* ((tip (shipit-stack--rev branch))
             (cell (assoc tip groups)))
        (if cell
            (setcdr cell (cons branch (cdr cell)))
          (push (cons tip (list branch)) groups))))
    groups))

(defun shipit-stack--with-lineage (base layers)
  "Return LAYERS with what each adds over the one below it.
Depth is measured from BASE and so counts everything underneath; a
reviewer is handed one layer, so each also records the commits it adds
and the commit it adds them to."
  (let ((previous-depth 0)
        (previous-tip base))
    (mapcar (lambda (layer)
              (let* ((depth (cdr (assq 'depth layer)))
                     (enriched (append `((commits . ,(- depth previous-depth))
                                         (parent-tip . ,previous-tip))
                                       layer)))
                (setq previous-depth depth
                      previous-tip (cdr (assq 'tip layer)))
                enriched))
            layers)))

(defun shipit-stack--layers (base branches current-branch)
  "Return the layers BRANCHES form over BASE, bottom first.
CURRENT-BRANCH marks the layer being worked on."
  (shipit-stack--with-lineage
   base
   (sort (mapcar (lambda (group)
                   (shipit-stack--layer (car group) (cdr group)
                                        base current-branch))
                 (shipit-stack--group-by-tip branches))
         (lambda (a b) (< (cdr (assq 'depth a)) (cdr (assq 'depth b)))))))

(defun shipit-stack--linear-p (layers)
  "Return non-nil when each layer of LAYERS is an ancestor of the next.
A set of branches that fork apart has no single order to merge in, and
picking one would target the wrong branch."
  (let ((ok t)
        (rest layers))
    (while (and ok (cdr rest))
      (unless (shipit-stack--ancestor-p (cdr (assq 'tip (car rest)))
                                        (cdr (assq 'tip (cadr rest))))
        (setq ok nil))
      (setq rest (cdr rest)))
    ok))

(defun shipit-stack--position (layers)
  "Return the 1-based index of the current layer within LAYERS."
  (let ((n 0) (found nil))
    (dolist (layer layers)
      (setq n (1+ n))
      (when (cdr (assq 'current layer)) (setq found n)))
    found))

(defun shipit-stack-detect ()
  "Return the stack the checked-out branch belongs to, or nil.
The result is (base . NAME) and (layers . LIST), bottom of the stack
first, which is the order the layers have to merge in.  Answers nil
when the branch was cut straight from the default branch, and when the
branches below it do not form a single chain."
  (let ((current (shipit-stack--current-branch)))
    (when current
      (let* ((base-ref (shipit-stack--base-ref))
             (candidates (shipit-stack--candidate-branches base-ref)))
        (when (cdr candidates)
          (let ((layers (shipit-stack--layers base-ref candidates current)))
            (when (shipit-stack--linear-p layers)
              `((base . ,(replace-regexp-in-string
                          "\\`origin/" "" base-ref))
                (size . ,(length layers))
                (position . ,(shipit-stack--position layers))
                (layers . ,layers)))))))))

(defun shipit-stack-parent-of (stack)
  "Return the branch a pull request for the current branch must target.
That is the layer of STACK directly below, or nil when there is none.
A branch sharing the current commit is skipped: it names the same
layer, and a pull request cannot be based on its own head."
  (let* ((layers (cdr (assq 'layers stack)))
         (head (shipit-stack--rev "HEAD"))
         (below (seq-remove (lambda (layer)
                              (equal (cdr (assq 'tip layer)) head))
                            layers)))
    (when below
      (cdr (assq 'branch (car (last below)))))))

(defun shipit-stack-parent-branch ()
  "Return the branch a pull request for the current one must target.
Detects the stack first; see `shipit-stack-parent-of\='."
  (shipit-stack-parent-of (shipit-stack-detect)))

(defun shipit-stack-layer-commits (layer)
  "Return the commits LAYER adds over the layer below it.
Each is a string of abbreviated hash and subject."
  (shipit-stack--git-lines
   "log" "--format=%h %s" "--no-merges"
   (format "%s..%s" (cdr (assq 'parent-tip layer)) (cdr (assq 'tip layer)))))

;;; Splitting a branch into layers

;; A branch of many commits already is a stack: the commits are in a
;; line.  Turning it into one is putting a branch pointer on the last
;; commit of each layer.  Nothing is rebased and no commit moves, so
;; the split is reversible by deleting the branches it made.

(defun shipit-stack--commit-prefix (subject)
  "Return the `area\=' of a SUBJECT written as \"area: summary\", or nil.
The prefix is how the author already grouped the work, which is a
better boundary than anything this could infer on its own."
  (when (string-match "\\`\\([^:\n]+\\): " subject)
    (match-string 1 subject)))

(defun shipit-stack--slug (text)
  "Return TEXT as a branch-name fragment: lower case, words hyphenated."
  (let ((slug (downcase (replace-regexp-in-string "[^A-Za-z0-9]+" "-" text))))
    (replace-regexp-in-string "\\`-\\|-\\'" "" slug)))

(defun shipit-stack--range-commits (base)
  "Return the commits from BASE to HEAD, oldest first.
Each is an alist of `sha\=' and `subject\='.  Nil when the range
contains a merge, which has no single line to put layers on."
  (unless (shipit-stack--git-lines "rev-list" "--merges" (format "%s..HEAD" base))
    (mapcar (lambda (line)
              (let ((space (string-match " " line)))
                `((sha . ,(substring line 0 space))
                  (subject . ,(substring line (1+ space))))))
            (shipit-stack--git-lines
             "log" "--reverse" "--format=%H %s" (format "%s..HEAD" base)))))

(defun shipit-stack--group-by-prefix (commits)
  "Group COMMITS into runs sharing a subject prefix, in order.
Returns nil unless every commit has one: a run of unprefixed commits
has no boundary to find, and inventing one would be guessing."
  (let (groups)
    (catch 'ungrouped
      (dolist (commit commits)
        (let ((prefix (shipit-stack--commit-prefix (cdr (assq 'subject commit)))))
          (unless prefix (throw 'ungrouped nil))
          (if (and groups (equal prefix (car (car groups))))
              (setcdr (car groups) (cons commit (cdr (car groups))))
            (push (cons prefix (list commit)) groups))))
      (mapcar (lambda (group) (cons (car group) (nreverse (cdr group))))
              (nreverse groups)))))

(defun shipit-stack--split-layer (group branch-base top-p)
  "Return the layer GROUP describes, named under BRANCH-BASE.
TOP-P marks the layer that keeps the branch already checked out: the
work is there already, so it needs no new pointer."
  (let ((commits (cdr group)))
    `((prefix . ,(car group))
      (branch . ,(if top-p
                     branch-base
                   (format "%s-%s" branch-base (shipit-stack--slug (car group)))))
      (tip . ,(cdr (assq 'sha (car (last commits)))))
      (existing . ,top-p)
      (commits . ,commits))))

(defun shipit-stack-split-proposal (&optional base)
  "Return the layers the checked-out branch could be split into.
Nil when it is not worth proposing: one group, no groups, or a range
with a merge in it.  BASE defaults to the ref a stack is measured
from."
  (let* ((base (or base (shipit-stack--base-ref)))
         (branch (shipit-stack--current-branch))
         (commits (and branch (shipit-stack--range-commits base)))
         (groups (and commits (shipit-stack--group-by-prefix commits))))
    (when (cdr groups)
      (let ((last-group (car (last groups))))
        `((base . ,(replace-regexp-in-string "\\`origin/" "" base))
          (layers . ,(mapcar (lambda (group)
                               (shipit-stack--split-layer
                                group branch (eq group last-group)))
                             groups)))))))

(defun shipit-stack-split-apply (proposal)
  "Create the branches PROPOSAL describes, and return the layers made.
A layer whose branch is already there is left alone, so applying the
same proposal twice does nothing the second time."
  (delq nil
        (mapcar
         (lambda (layer)
           (let ((branch (cdr (assq 'branch layer))))
             (unless (or (cdr (assq 'existing layer))
                         (shipit-stack--rev branch))
               (shipit-stack--git-lines "branch" branch (cdr (assq 'tip layer)))
               layer)))
         (cdr (assq 'layers proposal)))))

(defun shipit-stack--missing-branches (branches)
  "Return a sentence for each of BRANCHES that does not exist."
  (mapcar (lambda (branch) (format "%s does not exist" branch))
          (seq-remove #'shipit-stack--rev branches)))

(defun shipit-stack--repeated-branches (branches)
  "Return a sentence for each name used by two of BRANCHES."
  (let ((seen nil) (repeated nil))
    (dolist (branch branches)
      (when (and (member branch seen) (not (member branch repeated)))
        (push branch repeated))
      (push branch seen))
    (mapcar (lambda (branch) (format "%s names two layers" branch))
            (nreverse repeated))))

(defun shipit-stack--chain-problems (branches base)
  "Return a sentence for each break in the line from BASE through BRANCHES.
A layer must contain the layer below it, or its pull request carries
that layer's commits as well as its own.

The branch under the stack is held to less.  A pull request is compared
from where its two sides last met, so the branch under the stack may
move on without the stack being rebuilt on it, and usually has.  All
that is asked of the bottom layer is that it adds something."
  (let ((problems nil) (previous base) (above-the-base nil))
    (dolist (branch branches)
      (cond
       ((and above-the-base (not (shipit-stack--ancestor-p previous branch)))
        (push (format "%s is not built on %s" branch previous) problems))
       ((zerop (shipit-stack--depth previous branch))
        (push (format "%s adds nothing to %s" branch previous) problems)))
      (setq previous branch
            above-the-base t))
    (nreverse problems)))

(defun shipit-stack-problems (branches base)
  "Return why BRANCHES, bottom first, do not form a stack over BASE.
Each reason is a sentence.  An empty list means every layer is built on
the one below it and adds work of its own, so each pull request will
show only that layer.

The branches are asked rather than the division a buffer proposes.  The
remote sees where the branch pointers actually are, and one moved since
the division was made is the case worth catching.

Each check makes the later ones unanswerable, so the first to find
anything is the last to run: git cannot say what a missing ref is built
on, and a name used twice puts the same commit on both sides of a
comparison."
  (or (shipit-stack--missing-branches branches)
      (shipit-stack--repeated-branches branches)
      (shipit-stack--chain-problems branches base)))

(provide 'shipit-stack)
;;; shipit-stack.el ends here
