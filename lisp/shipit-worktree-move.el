;;; shipit-worktree-move.el --- Move the work of a checkout to a worktree -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Work often starts in the main checkout, on the default branch or on
;; a branch of its own, and only later earns a worktree.  This moves it
;; there: the branch, with its commits and the edits not yet committed,
;; goes to a worktree under `shipit-worktree-directory', and the
;; checkout goes back to the default branch.  Work begun on the default
;; branch itself is first spun off onto a new branch, as magit's spinoff
;; does, and the default branch goes back to its upstream.
;;
;; A branch then has one worktree.  Other packages act on a move
;; through `shipit-worktree-move-functions'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-stack)

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))

;;;; Reading the repository

(defun shipit-worktree-move--root ()
  "The top directory of the checkout here, a directory name."
  (file-name-as-directory (string-trim (shipit--git-output "rev-parse" "--show-toplevel"))))

(defun shipit-worktree-move--current-branch ()
  "The branch checked out here, or nil on no branch."
  (let ((name (string-trim (shipit--git-output "rev-parse" "--abbrev-ref" "HEAD"))))
    (unless (equal name "HEAD")
      name)))

(defun shipit-worktree-move--worktrees ()
  "The worktrees of the repository as (DIRECTORY . BRANCH), the main one first.
BRANCH is nil for a worktree on no branch."
  (mapcar (lambda (worktree)
            (cons (plist-get worktree :path) (plist-get worktree :branch)))
          (shipit--git-worktrees)))

(defun shipit-worktree-move-worktree-of (branch)
  "The directory of the worktree that has BRANCH checked out, or nil.
The main checkout does not count: work there shares its session."
  (car (seq-find (lambda (worktree) (equal branch (cdr worktree)))
                 (cdr (shipit-worktree-move--worktrees)))))

(defun shipit-worktree-move--checked-out-in (branch)
  "The worktree BRANCH is checked out in, the main one included, or nil."
  (car (rassoc branch (shipit-worktree-move--worktrees))))

(defun shipit-worktree-move--path (branch root)
  "Where the worktree of BRANCH goes, for the checkout ROOT."
  (file-name-as-directory
   (expand-file-name (string-replace "/" "-" branch)
                     (expand-file-name shipit-worktree-directory root))))

(defun shipit-worktree-move--dirty-p ()
  "Non-nil when the checkout here has changes not committed, new files included."
  (not (string-empty-p (shipit--git-output "status" "--porcelain"))))

(defun shipit-worktree-move--lines (&rest args)
  "The lines git ARGS prints."
  (split-string (apply #'shipit--git-output args) "\n" t))

(defun shipit-worktree-move--changed-files (base)
  "The files the work here changed since BASE, committed or not."
  (delete-dups
   (append (shipit-worktree-move--lines "diff" "--name-only" (concat base "...HEAD"))
           (shipit-worktree-move--lines "diff" "--name-only" "HEAD")
           (shipit-worktree-move--lines "ls-files" "--others" "--exclude-standard"))))

(defun shipit-worktree-move--upstream (branch)
  "The upstream of BRANCH, as git names it."
  (with-temp-buffer
    (unless (zerop (process-file "git" nil '(t nil) nil "rev-parse" "--abbrev-ref"
                                 (concat branch "@{upstream}")))
      (user-error "%s has no upstream, so which of its commits are the work is not known"
                  branch))
    (string-trim (buffer-string))))

(defun shipit-worktree-move--ahead-p (upstream)
  "Non-nil when HEAD has commits that UPSTREAM has not."
  (not (equal "0" (string-trim (shipit--git-output "rev-list" "--count"
                                                   (concat upstream "..HEAD"))))))

;;;; Checks before anything moves

(defun shipit-worktree-move--check-free (dir)
  "Refuse when the worktree directory DIR is taken."
  (when (file-exists-p dir)
    (user-error "%s is there already" dir)))

(defun shipit-worktree-move--check-saved (root)
  "Refuse while a buffer of a file under ROOT has edits not saved.
They would stay behind, in a checkout on another branch."
  (let ((unsaved (seq-filter (lambda (buffer)
                               (let ((file (buffer-file-name buffer)))
                                 (and file (buffer-modified-p buffer)
                                      (file-in-directory-p file root))))
                             (buffer-list))))
    (when unsaved
      (user-error "Save or revert %s first: its edits would stay behind"
                  (mapconcat #'buffer-name unsaved ", ")))))

(defun shipit-worktree-move--check-default-free (default)
  "Refuse when DEFAULT, the branch the checkout goes back to, is elsewhere."
  (let ((elsewhere (shipit-worktree-move--checked-out-in default)))
    (when elsewhere
      (user-error "%s is checked out in %s, so the checkout cannot go back to it"
                  default elsewhere))))

(defun shipit-worktree-move--check-new-branch (name)
  "Refuse when NAME is no branch name, or a branch of that name exists."
  (unless (zerop (process-file "git" nil nil nil "check-ref-format" "--branch" name))
    (user-error "%s is no branch name" name))
  (unless (string-empty-p (shipit--git-output "branch" "--list" name))
    (user-error "A branch %s exists already" name)))

;;;; Moving

(defun shipit-worktree-move--add (dir branch root)
  "Make the worktree DIR of the checkout ROOT, with BRANCH checked out."
  (let ((default-directory root))
    (shipit--ignore-worktree-directory root)
    (make-directory (file-name-directory (directory-file-name dir)) t)
    (shipit--git-change "worktree" "add" dir branch)))

(defun shipit-worktree-move--stash ()
  "Stash the changes here, new files included; non-nil when there were any."
  (when (shipit-worktree-move--dirty-p)
    (shipit--git-change "stash" "push" "--include-untracked"
                        "-m" "shipit: moving the work to a worktree")
    t))

(defun shipit-worktree-move--unstash (dir)
  "Put the changes stashed last back, in the worktree DIR."
  (let ((default-directory dir))
    (shipit--git-change "stash" "pop")))

(defun shipit-worktree-move--buffers-of (files root)
  "The buffers that visit FILES, which are named relative to ROOT."
  (delq nil (mapcar (lambda (file) (get-file-buffer (expand-file-name file root)))
                    files)))

(defun shipit-worktree-move--reopen (buffers root dir)
  "Visit the files of BUFFERS under DIR instead of ROOT, at the same place.
A window that showed one shows the new one, and BUFFERS are killed."
  (dolist (buffer buffers)
    (let ((file (expand-file-name (file-relative-name (buffer-file-name buffer) root) dir))
          (place (with-current-buffer buffer (point))))
      (when (file-exists-p file)
        (let ((new (find-file-noselect file)))
          (with-current-buffer new (goto-char place))
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-buffer window new))
          (kill-buffer buffer))))))

(defvar shipit-worktree-move-functions nil
  "Functions called after the work of a checkout moved to a worktree.
Each is called with the checkout and the worktree, as directory names.")

(defun shipit-worktree-move--carry (base root dir move)
  "Carry the work since BASE from the checkout ROOT into the worktree DIR.
MOVE is called with no argument once the changes not committed are
stashed, to take the work out of the checkout and make DIR."
  (let ((buffers (shipit-worktree-move--buffers-of
                  (shipit-worktree-move--changed-files base) root))
        (stashed (shipit-worktree-move--stash)))
    (funcall move)
    (when stashed
      (shipit-worktree-move--unstash dir))
    (shipit-worktree-move--reopen buffers root dir)
    (run-hook-with-args 'shipit-worktree-move-functions root dir)))

(defun shipit-worktree-move--move-current (branch root dir)
  "Move BRANCH, checked out in the checkout ROOT, to the worktree DIR."
  (let ((default (shipit--upstream-default-branch)))
    (when (equal branch default)
      (user-error "%s is the default branch: spin its work off instead" branch))
    (shipit-worktree-move--check-saved root)
    (shipit-worktree-move--check-default-free default)
    (shipit-worktree-move--carry
     default root dir
     (lambda ()
       (shipit--git-change "switch" "-q" default)
       (shipit-worktree-move--add dir branch root)))))

(defun shipit-worktree-move-branch (branch)
  "Move BRANCH to a worktree of its own, and return its directory.
When BRANCH is checked out here, its changes not committed go along,
the checkout goes back to the default branch, the files of the work
that are open are visited in the worktree instead, and
`shipit-worktree-move-functions' are called."
  (let* ((root (shipit-worktree-move--root))
         (dir (shipit-worktree-move--path branch root))
         (existing (shipit-worktree-move-worktree-of branch)))
    (when existing
      (user-error "%s has a worktree already: %s" branch existing))
    (shipit-worktree-move--check-free dir)
    (if (equal branch (shipit-worktree-move--current-branch))
        (shipit-worktree-move--move-current branch root dir)
      (shipit-worktree-move--add dir branch root))
    dir))

(defun shipit-worktree-move-spin-off (name)
  "Spin the work on the branch here off to a new branch NAME, in a worktree.
The commits the branch has over its upstream go to NAME, along with the
changes not committed, and the branch goes back to its upstream.  Returns
the worktree's directory."
  (let* ((root (shipit-worktree-move--root))
         (branch (or (shipit-worktree-move--current-branch)
                     (user-error "This checkout is on no branch")))
         (upstream (shipit-worktree-move--upstream branch))
         (dir (shipit-worktree-move--path name root)))
    (shipit-worktree-move--check-new-branch name)
    (shipit-worktree-move--check-free dir)
    (shipit-worktree-move--check-saved root)
    (unless (or (shipit-worktree-move--dirty-p) (shipit-worktree-move--ahead-p upstream))
      (user-error "%s has no work to move: no commit over %s and no change" branch upstream))
    (shipit-worktree-move--carry
     upstream root dir
     (lambda ()
       (shipit--git-change "branch" name)
       (shipit--git-change "reset" "-q" "--hard" upstream)
       (shipit-worktree-move--add dir name root)))
    dir))

;;;; The command

(defun shipit-worktree-move--work-description ()
  "The work on the branch here: its commit messages and changed files."
  (let ((upstream (shipit-worktree-move--upstream (shipit-worktree-move--current-branch))))
    (concat (shipit--git-output "log" "--format=%B" (concat upstream "..HEAD"))
            "\n"
            (shipit--git-output "status" "--short"))))

(defun shipit-worktree-move--suggest-name (callback)
  "Have `shipit-branch-name-suggester' name the work here.
CALLBACK gets the name, or nil when there is no suggester."
  (if shipit-branch-name-suggester
      (funcall shipit-branch-name-suggester
               (shipit-worktree-move--work-description) callback)
    (funcall callback nil)))

(defun shipit-worktree-move--show (dir)
  "Show the worktree DIR the work moved to."
  (when (fboundp 'magit-status-setup-buffer)
    (magit-status-setup-buffer dir))
  (message "The work is in %s now" dir))

(defun shipit-worktree-move--name-and-spin-off ()
  "Spin the work here off, to a branch named as suggested.
The suggestion is offered to edit, the answer names the branch."
  (let ((dir default-directory))
    (shipit-worktree-move--suggest-name
     (lambda (suggestion)
       (let* ((default-directory dir)
              (name (read-string "Spin the work off to the branch: "
                                 (and suggestion (shipit-stack--slug suggestion)))))
         (when (string-empty-p name)
           (user-error "No branch name"))
         (shipit-worktree-move--show (shipit-worktree-move-spin-off name)))))))

;;;###autoload
(defun shipit-worktree-move-work ()
  "Move the work in this checkout to a worktree of its own.
On a branch, the branch moves.  On the default branch, the work is spun
off to a new branch, with the name `shipit-branch-name-suggester'
suggests offered to edit.  Magit shows the worktree after."
  (interactive)
  (let ((branch (or (shipit-worktree-move--current-branch)
                    (user-error "This checkout is on no branch; check out the work's branch"))))
    (if (equal branch (shipit--upstream-default-branch))
        (shipit-worktree-move--name-and-spin-off)
      (shipit-worktree-move--show (shipit-worktree-move-branch branch)))))

(with-eval-after-load 'magit-worktree
  (transient-append-suffix 'magit-worktree "c"
    '("s" "Move this work to a worktree" shipit-worktree-move-work)))

(provide 'shipit-worktree-move)
;;; shipit-worktree-move.el ends here
