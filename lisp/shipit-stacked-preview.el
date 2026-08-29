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

(declare-function shipit--insert-processed-commit-section "shipit-pr-sections")
(declare-function magit-section-type "magit-section")
(declare-function magit-section-value "magit-section")

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

(defun shipit-stacked-preview--layers (commits boundaries branch &optional names)
  "Divide COMMITS into layers ending at BOUNDARIES.
COMMITS are oldest first.  The last layer keeps BRANCH, since the work
is already on it and it needs no new pointer.  NAMES maps a layer\='s
last commit to the branch already marking it, so a stack that has been
divided before keeps the names it was given rather than being renamed
from its commit subjects."
  (let (layers current)
    (dolist (commit commits)
      (push commit current)
      (when (member (plist-get commit :sha) boundaries)
        (push (nreverse current) layers)
        (setq current nil)))
    (when current (push (nreverse current) layers))
    (setq layers (nreverse layers))
    (let ((last (car (last layers))))
      (mapcar (lambda (group)
                (let ((top (eq group last)))
                  `((commits . ,group)
                    (tip . ,(plist-get (car (last group)) :sha))
                    (existing . ,top)
                    (branch . ,(cond
                                (top branch)
                                ((cdr (assoc (plist-get (car (last group)) :sha) names)))
                                (t (shipit-stacked-preview--layer-name branch group)))))))
              layers))))

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

(defun shipit-stacked-preview--layer-status (layer)
  "Return where LAYER has got to, coloured by how far that is.
Dim while the layer is still only local, orange while it is on its way,
green once it is on the remote, and the branch colour once it has a
pull request of its own."
  (let ((branch (cdr (assq 'branch layer))))
    (cond
     ((equal branch shipit-stacked-preview--sending)
      (propertize "sending…" 'font-lock-face 'warning))
     ((not (or (cdr (assq 'existing layer)) (shipit-stack--rev branch)))
      (propertize "no branch yet" 'font-lock-face 'font-lock-comment-face))
     ((not (shipit-stack--rev (concat "origin/" branch)))
      (propertize "not pushed" 'font-lock-face 'font-lock-comment-face))
     (t (let ((number (shipit-stacked-preview--pr-number branch)))
          (if number
              (propertize (format "PR #%s" number)
                          'font-lock-face 'magit-branch-remote)
            (propertize "pushed" 'font-lock-face 'success)))))))

(defun shipit-stacked-preview--pr-number (branch)
  "Return the number of the open pull request whose head is BRANCH.
Only asked for a branch that has reached the remote, since one that has
not cannot have a pull request.  The answer is cached by the http layer,
misses included, so redrawing the list does not ask again."
  (and shipit-stacked-preview--repo
       (shipit-stack-plan--pr-for-branch shipit-stacked-preview--repo branch)))

(defun shipit-stacked-preview--read-base ()
  "Ask which ref the stack sits on."
  (completing-read "Stack sits on: " (magit-list-refnames) nil t nil nil
                   (shipit-stack--base-ref)))

(defun shipit-stacked-preview--layer-at-point ()
  "Return the layer whose section holds point.
Signals when point is on none: every action here works on one layer,
and acting on the wrong one is worse than refusing to act."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'shipit-stack-layer)))
      (setq section (oref section parent)))
    (unless section (user-error "Point is not on a layer"))
    (let* ((branch (oref section value))
           (layer (seq-find (lambda (l) (equal branch (cdr (assq 'branch l))))
                            (shipit-stacked-preview--current-layers))))
      (unless layer (user-error "No layer named %s" branch))
      layer)))

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
      (user-error "Branch %s does not exist yet -- press c to create it" branch))
    branch))

(defconst shipit-stacked-preview--commit-indent 9
  "Columns to indent a commit by, so its hash starts where a branch
name does.  A layer row spends its first twelve columns on the HEAD
mark and the layer number; a commit has neither, and without this its
hash sits to the left of the layer holding it.")

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

(defun shipit-stacked-preview--insert-layer (layer index width count-width top-p)
  "Insert LAYER, numbered INDEX from the bottom.
Names are padded to WIDTH and commit counts to COUNT-WIDTH.  TOP-P
marks the newest end of the line of commits."
  (let ((commits (cdr (assq 'commits layer))))
    (magit-insert-section (shipit-stack-layer (cdr (assq 'branch layer)))
      (magit-insert-heading
        (format "  %-5s %2d  %s  %s commit%s  %s"
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
                (if (cdr commits) "s" " ")
                (shipit-stacked-preview--layer-status layer)))
      (magit-insert-section-body
        ;; Each commit through the renderer the pull request buffer
        ;; uses, so a commit here opens onto its message and the files
        ;; it changed exactly as it does there.  The section around
        ;; them is skipped: it would put a second "Commits" heading
        ;; inside a layer that already says how many it holds.
        (dolist (commit commits)
          (shipit--insert-processed-commit-section
           commit shipit-stacked-preview--repo nil
           shipit-stacked-preview--commit-indent))))))

(defun shipit-stacked-preview--insert-base (width)
  "Insert the line naming what the lowest layer sits on, padded to WIDTH."
  (insert (format "  %-5s %2s  %s\n"
                  (propertize "base" 'font-lock-face 'font-lock-comment-face)
                  ""
                  (shipit-sections--pad
                   (propertize (shipit-stacked-preview--base-name
                                shipit-stacked-preview--base)
                               'font-lock-face 'magit-branch-remote)
                   width))))

(defconst shipit-stacked-preview--key-columns
  '((("RET" . "describe & submit") ("s" . "divide here") ("u" . "undo"))
    (("c" . "create branches") ("P" . "push"))
    (("g" . "refresh") ("q" . "quit")))
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

(defun shipit-stacked-preview--key-row (row key-width label-widths)
  "Return line ROW of the key columns, padded to LABEL-WIDTHS."
  (string-trim-right
   (mapconcat #'identity
              (seq-mapn (lambda (column label-width)
                          (shipit-stacked-preview--key-cell
                           (nth row column) key-width label-width))
                        shipit-stacked-preview--key-columns label-widths)
              "   ")))

(defun shipit-stacked-preview--insert-keys ()
  "Insert the keys this buffer offers, in aligned columns."
  (let* ((entries (apply #'append shipit-stacked-preview--key-columns))
         (key-width (apply #'max (mapcar (lambda (e) (length (car e))) entries)))
         (label-widths (mapcar (lambda (column)
                                 (apply #'max (mapcar (lambda (e) (length (cdr e)))
                                                      column)))
                               shipit-stacked-preview--key-columns))
         (rows (apply #'max (mapcar #'length
                                    shipit-stacked-preview--key-columns))))
    (dotimes (row rows)
      (insert "   "
              (shipit-stacked-preview--key-row row key-width label-widths)
              "\n"))
    (insert "\n")))

(defun shipit-stacked-preview--insert-header ()
  "Insert the banner saying what this buffer is."
  (insert (propertize
           (format "STACKED PULL REQUESTS — %d commit%s over %s\n"
                   (length shipit-stacked-preview--commits)
                   (if (cdr shipit-stacked-preview--commits) "s" "")
                   (shipit-stacked-preview--base-name
                    shipit-stacked-preview--base))
           'font-lock-face '(:foreground "orange" :weight bold)))
  (shipit-stacked-preview--insert-keys))

(defun shipit-stacked-preview--render ()
  "Draw the buffer from the state it holds."
  (let ((inhibit-read-only t)
        (position (point)))
    (erase-buffer)
    (magit-insert-section (shipit-stack-root)
      (shipit-stacked-preview--insert-header)
      (shipit-stacked-preview--insert-problems)
      ;; Newest at the top and the base at the bottom, which is the way
      ;; a stack is drawn and the way the commits actually sit.
      (let* ((layers (shipit-stacked-preview--current-layers))
             (width (shipit-stacked-preview--name-width layers))
             (count-width (shipit-stacked-preview--count-width layers))
             (index (length layers)))
        (dolist (layer (reverse layers))
          (shipit-stacked-preview--insert-layer
           layer index width count-width (= index (length layers)))
          (setq index (1- index)))
        (shipit-stacked-preview--insert-base width)))
    (goto-char (min position (point-max)))))

;;;; Commands

(defun shipit-stacked-preview--sha-at-point ()
  "Return the SHA of the commit at point, or nil."
  (or (get-text-property (point) 'shipit-commit-sha)
      (let ((section (magit-current-section)))
        (when (and section (eq (magit-section-type section) 'shipit-commit))
          (magit-section-value section)))))

(defun shipit-stacked-preview-divide ()
  "Make the commit at point the end of a layer."
  (interactive)
  (let ((sha (shipit-stacked-preview--sha-at-point)))
    (unless sha (user-error "No commit at point"))
    (when (equal sha (plist-get (car (last shipit-stacked-preview--commits)) :sha))
      (user-error "The newest commit already ends the top layer"))
    (cl-pushnew sha shipit-stacked-preview--boundaries :test #'equal)
    (shipit-stacked-preview--render)))

(defun shipit-stacked-preview-undivide ()
  "Remove the division at the commit at point."
  (interactive)
  (let ((sha (shipit-stacked-preview--sha-at-point)))
    (unless sha (user-error "No commit at point"))
    (setq shipit-stacked-preview--boundaries
          (delete sha shipit-stacked-preview--boundaries))
    (shipit-stacked-preview--render)))

(defun shipit-stacked-preview-create-branches ()
  "Put a branch on each layer that has not got one.
Creating a branch on a commit that is already there moves nothing, so
this can be run again after dividing differently."
  (interactive)
  (let ((made 0))
    (dolist (layer (shipit-stacked-preview--current-layers))
      (let ((branch (cdr (assq 'branch layer))))
        (unless (or (cdr (assq 'existing layer)) (shipit-stack--rev branch))
          (shipit-stack--git-lines "branch" branch (cdr (assq 'tip layer)))
          (setq made (1+ made)))))
    (shipit-stacked-preview--render)
    (message "shipit: %d branch%s created" made (if (= made 1) "" "es"))))

(defun shipit-stacked-preview-visit-layer ()
  "Describe and submit the layer at point as a pull request.
Hands it to the ordinary preview -- a layer is a pull request like any
other, one whose base is the layer below -- and comes back here once it
has been submitted or abandoned, so the list shows what it became.  The
layer does not have to be checked out for this."
  (interactive)
  (require 'shipit-preview)
  (let* ((layer (shipit-stacked-preview--layer-at-point))
         (branch (shipit-stacked-preview--require-branch layer)))
    (shipit-stacked-preview--refuse-when-broken layer)
    (shipit-stacked-preview--refuse-when-base-is-absent layer)
    (shipit-preview-branch shipit-stacked-preview--repo branch
                           (shipit-stacked-preview--base-of layer)
                           :tip branch
                           :stacked t
                           :on-done (shipit-stacked-preview--return-to
                                     (current-buffer)))))

(defun shipit-stacked-preview--return-to (buffer)
  "Return a function that brings BUFFER up to date and shows it again.
Called once a layer's preview is finished with, so submitting one layer
leaves the list ready for the next."
  (lambda (_number)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (shipit-stacked-preview--render))
      (pop-to-buffer buffer))))

(defun shipit-stacked-preview--show-sending (branch)
  "Mark BRANCH as the one on its way, and put that on screen now.
Each push blocks until it finishes, so without asking for the screen to
be brought up to date the buffer would sit unchanged until every layer
had gone and then change all at once."
  (setq shipit-stacked-preview--sending branch)
  (shipit-stacked-preview--render)
  (redisplay))

(defun shipit-stacked-preview--push-through (layer args)
  "Push the branches from the bottom layer up to LAYER, oldest first.
The layers below go too, asked for or not.  A pull request cannot be
raised against a base the remote does not have, so a layer sent on its
own would arrive unsubmittable: its commits are all there, but the
branch its pull request is based on is not.

Stops at the first branch that fails and leaves the rest alone, for the
same reason: what stands on it could not be submitted anyway."
  (shipit-stacked-preview--refuse-when-broken layer)
  (let ((branches (shipit-stacked-preview--branches-through layer)))
    (unwind-protect
        (dolist (branch branches)
          (shipit-stacked-preview--show-sending branch)
          (unless (shipit--push-branch branch args)
            (user-error "Pushing %s failed -- the layers above it are left alone"
                        branch)))
      ;; However this ends, no layer may be left saying it is on its
      ;; way: after a failure that would be a lie standing until the
      ;; next redraw.
      (setq shipit-stacked-preview--sending nil)
      (shipit-stacked-preview--render))
    (message "Pushed %d branch%s%s" (length branches)
             (if (cdr branches) "es" "")
             (if (member "--dry-run" args) " (dry run)" ""))))

(defun shipit-stacked-preview--push-arguments ()
  "Return the git arguments chosen in the push transient."
  (transient-args 'shipit-stacked-preview-push))

(transient-define-suffix shipit-stacked-preview-push-layer (args)
  "Push the layer at point, and the layers below it."
  (interactive (list (shipit-stacked-preview--push-arguments)))
  (shipit-stacked-preview--push-through
   (shipit-stacked-preview--layer-at-point) args))

(transient-define-suffix shipit-stacked-preview-push-all (args)
  "Push every layer of the stack, from the base upwards."
  (interactive (list (shipit-stacked-preview--push-arguments)))
  (shipit-stacked-preview--push-through
   (car (last (shipit-stacked-preview--current-layers))) args))

(transient-define-prefix shipit-stacked-preview-push ()
  "Push the branches of this stack to origin."
  ["Arguments"
   ;; Rewriting a lower layer and rebasing the ones above it is the
   ;; ordinary way a stack moves, and every layer above the rewrite
   ;; then needs the forced form.
   ("-f" "Force with lease" "--force-with-lease")
   ("-n" "Dry run" "--dry-run")
   ("-h" "Skip hooks" "--no-verify")]
  ["Push"
   ("p" "this layer and the ones below" shipit-stacked-preview-push-layer)
   ("P" "every layer" shipit-stacked-preview-push-all)])

(defun shipit-stacked-preview-refresh ()
  "Read the commits again and redraw."
  (interactive)
  (setq shipit-stacked-preview--commits
        (shipit--commits-in-range
         (format "%s..HEAD" shipit-stacked-preview--base)))
  (shipit-stacked-preview--render))

(defvar shipit-stacked-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "s") #'shipit-stacked-preview-divide)
    (define-key map (kbd "u") #'shipit-stacked-preview-undivide)
    (define-key map (kbd "c") #'shipit-stacked-preview-create-branches)
    (define-key map (kbd "RET") #'shipit-stacked-preview-visit-layer)
    (define-key map (kbd "P") #'shipit-stacked-preview-push)
    (define-key map (kbd "g") #'shipit-stacked-preview-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `shipit-stacked-preview-mode'.")

(define-derived-mode shipit-stacked-preview-mode magit-section-mode "Shipit Stack"
  "Divide a line of commits into stacked pull requests."
  ;; The same section defaults every other shipit buffer applies, which
  ;; among other things drops magit's left-margin expansion markers.
  ;; Without this the commits here carry markers the ones in the pull
  ;; request buffer do not, and the two stop looking alike.
  (shipit--apply-section-defaults)
  (setq-local revert-buffer-function
              (lambda (&rest _) (shipit-stacked-preview-refresh))))

;;;###autoload
(defun shipit-stacked-preview (&optional choose-base)
  "Preview the stack the commits on this branch would become.
What `shipit-preview\=' is to one pull request, this is to several: the
commits with divisions proposed from what their subjects have in
common.  Move the divisions, put a branch on each layer, then hand
each layer to `shipit-preview\=' to describe and submit.

With a prefix argument CHOOSE-BASE, ask which branch the stack sits on
instead of measuring from the default branch, for a stack raised
against a release branch."
  (interactive "P")
  (let* ((base (if choose-base
                   (shipit-stacked-preview--read-base)
                 (shipit-stack--base-ref)))
         (branch (shipit-stack--current-branch))
         (commits (shipit--commits-in-range (format "%s..HEAD" base))))
    (unless branch (user-error "Not on a branch"))
    (unless commits (user-error "No commits between %s and HEAD" base))
    (let ((buffer (get-buffer-create
                   (format "*shipit-stacked-preview: %s*" branch))))
      (with-current-buffer buffer
        (shipit-stacked-preview-mode)
        (setq shipit-stacked-preview--repo (shipit--get-repo-from-remote)
              shipit-stacked-preview--base base
              shipit-stacked-preview--branch branch
              shipit-stacked-preview--commits commits)
        (let ((existing (shipit-stacked-preview--from-existing-stack)))
          (setq shipit-stacked-preview--boundaries
                (or (car existing)
                    (shipit-stacked-preview--proposed-boundaries commits))
                shipit-stacked-preview--names (cdr existing)))
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

(defun shipit-stacked-preview--refuse-when-base-is-absent (layer)
  "Signal when a layer below LAYER has not reached the remote.
A pull request is raised against a branch the remote already has, and
LAYER is raised against the layer below it.  Caught here rather than
left to the remote, which would refuse the created pull request with a
message about a base ref, well after the description had been written."
  (let* ((below (butlast (shipit-stacked-preview--branches-through layer)))
         (absent (seq-remove (lambda (branch)
                               (shipit-stack--rev (concat "origin/" branch)))
                             below)))
    (when absent
      (user-error "%s not on the remote yet -- press P to send %s"
                  (string-join absent ", ")
                  (if (cdr absent) "them" "it")))))

(defun shipit-stacked-preview--insert-problems ()
  "Say what stops this stack being submitted, when anything does.
Only once every layer has a branch: before that the layer rows already
say which ones are still to be made, and a branch not yet made is not a
fault."
  (let ((branches (shipit-stacked-preview--branch-names
                   (shipit-stacked-preview--current-layers))))
    (when (seq-every-p #'shipit-stack--rev branches)
      (let ((problems (shipit-stack-problems
                       branches shipit-stacked-preview--base)))
        (dolist (problem problems)
          (insert (propertize (format "   %s\n" problem)
                              'font-lock-face 'font-lock-warning-face)))
        (when problems (insert "\n"))))))

(provide 'shipit-stacked-preview)
;;; shipit-stacked-preview.el ends here
