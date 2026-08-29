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

(defun shipit-stack--layers (base branches current-branch)
  "Return the layers BRANCHES form over BASE, bottom first.
CURRENT-BRANCH marks the layer being worked on."
  (sort (mapcar (lambda (group)
                  (shipit-stack--layer (car group) (cdr group)
                                       base current-branch))
                (shipit-stack--group-by-tip branches))
        (lambda (a b) (< (cdr (assq 'depth a)) (cdr (assq 'depth b))))))

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

(defun shipit-stack-parent-branch ()
  "Return the branch a pull request for the current one must target.
That is the layer directly below, or nil when there is none.  A branch
sharing the current commit is skipped: it names the same layer, and a
pull request cannot be based on its own head."
  (let* ((stack (shipit-stack-detect))
         (layers (cdr (assq 'layers stack)))
         (head (shipit-stack--rev "HEAD"))
         (below (seq-remove (lambda (layer)
                              (equal (cdr (assq 'tip layer)) head))
                            layers)))
    (when below
      (cdr (assq 'branch (car (last below)))))))

(provide 'shipit-stack)
;;; shipit-stack.el ends here
