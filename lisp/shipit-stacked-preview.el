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
(require 'magit-section)

(declare-function shipit--insert-processed-commit-section "shipit-pr-sections")
(declare-function magit-section-type "magit-section")
(declare-function magit-section-value "magit-section")

(defvar-local shipit-stacked-preview--repo nil
  "Repository these commits belong to.")

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
  "Return a short word for where LAYER has got to."
  (let ((branch (cdr (assq 'branch layer))))
    (cond
     ((not (or (cdr (assq 'existing layer)) (shipit-stack--rev branch)))
      "no branch yet")
     ((not (shipit-stack--rev (concat "origin/" branch))) "not pushed")
     (t "pushed"))))

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
                (propertize (shipit-stacked-preview--layer-status layer)
                            'font-lock-face 'font-lock-comment-face)))
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
                   (propertize shipit-stacked-preview--base
                               'font-lock-face 'magit-branch-remote)
                   width))))

(defun shipit-stacked-preview--insert-header ()
  "Insert the banner saying what this buffer is."
  (insert (propertize
           (format "STACKED PULL REQUESTS — %d commit%s over %s\n"
                   (length shipit-stacked-preview--commits)
                   (if (cdr shipit-stacked-preview--commits) "s" "")
                   shipit-stacked-preview--base)
           'font-lock-face '(:foreground "orange" :weight bold)))
  (insert (propertize
           "   s divide here   u undo   c create branches   g refresh   q quit\n\n"
           'font-lock-face 'font-lock-comment-face)))

(defun shipit-stacked-preview--render ()
  "Draw the buffer from the state it holds."
  (let ((inhibit-read-only t)
        (position (point)))
    (erase-buffer)
    (magit-insert-section (shipit-stack-root)
      (shipit-stacked-preview--insert-header)
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
(defun shipit-stacked-preview ()
  "Preview the stack the commits on this branch would become.
What `shipit-preview\=' is to one pull request, this is to several: the
commits with divisions proposed from what their subjects have in
common.  Move the divisions, put a branch on each layer, then hand
each layer to `shipit-preview\=' to describe and submit."
  (interactive)
  (let* ((base (shipit-stack--base-ref))
         (branch (shipit-stack--current-branch))
         (commits (shipit--commits-in-range (format "%s..HEAD" base))))
    (unless branch (user-error "Not on a branch"))
    (unless commits (user-error "No commits between %s and HEAD" base))
    (let ((buffer (get-buffer-create
                   (format "*shipit-stacked-preview: %s*" branch))))
      (with-current-buffer buffer
        (shipit-stacked-preview-mode)
        (setq shipit-stacked-preview--repo (shipit--get-repo-from-remote)
              shipit-stacked-preview--base (replace-regexp-in-string
                                         "\\`origin/" "" base)
              shipit-stacked-preview--branch branch
              shipit-stacked-preview--commits commits)
        (let ((existing (shipit-stacked-preview--from-existing-stack)))
          (setq shipit-stacked-preview--boundaries
                (or (car existing)
                    (shipit-stacked-preview--proposed-boundaries commits))
                shipit-stacked-preview--names (cdr existing)))
        (shipit-stacked-preview--render))
      (switch-to-buffer buffer))))

(provide 'shipit-stacked-preview)
;;; shipit-stacked-preview.el ends here
