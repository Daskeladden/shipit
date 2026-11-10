;;; test-shipit-pr-worktree-found-cases.el --- More cases of the worktrees of a pull request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The cases of the worktrees of a pull request that the scenarios of
;; test-shipit-pr-worktree-found.el leave out: a worktree of mine on no
;; branch, a branch that tracks the fork of the pull request, a worktree
;; whose folder was deleted by hand, a clone on the branch of the pull
;; request, shipit's own worktree behind the head while mine is at it,
;; RET on a file with only my worktree, the worktree menu with only my
;; worktree, the Worktree section with only shipit's own, and the list
;; of worktrees that git gives.
;;
;; And a page whose directory is a clone of another repository, which
;; has worktrees of its own.  A pull request has no worktree there.
;; It has one again when a second remote of the clone names its
;; repository.
;;
;; They use the kitchen, the page and the stand-ins of
;; test-shipit-worktree-offer.el and test-shipit-pr-worktree-found.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'magit-repos)
(require 'shipit-core)
(require 'test-shipit-pr-worktree-found)

;;; What each scenario is given, before the page opens

(defun test-pr-worktree-found-cases--my-worktree-on-no-branch-at-the-pr-head ()
  "Make my worktree kitchen-work on no branch, at the head of PR 8."
  (test-worktree-offer--fetch-the-pr)
  (test-worktree-offer--git "clone" "worktree" "add" "-q"
                            (test-pr-worktree-found--my-worktree)
                            (test-worktree-offer--sha "coffee")))

(defun test-pr-worktree-found-cases--my-branch-tracks-the-fork-by-its-remote ()
  "Make my own branch coffee in kitchen-work track the remote guest of the fork.
The clone gets the remote guest for guest/r, which it never fetches."
  (test-pr-worktree-found--my-own-branch-coffee)
  (test-worktree-offer--git "clone" "remote" "add" "guest" "https://github.com/guest/r.git")
  (test-worktree-offer--git "clone" "config" "branch.coffee.remote" "guest"))

(defun test-pr-worktree-found-cases--my-branch-tracks-the-fork-by-its-address ()
  "Make my own branch coffee in kitchen-work track the address of the fork guest/r."
  (test-pr-worktree-found--my-own-branch-coffee)
  (test-worktree-offer--git "clone" "config" "branch.coffee.remote"
                            "https://github.com/guest/r.git"))

(defun test-pr-worktree-found-cases--my-worktree-deleted-by-hand ()
  "Make my worktree kitchen-work at the head of PR 8, and delete its folder by hand.
Git still lists the worktree."
  (test-pr-worktree-found--my-worktree-at-the-pr-head)
  (delete-directory (test-pr-worktree-found--my-worktree) t))

(defun test-pr-worktree-found-cases--own-worktree-deleted-by-hand ()
  "Give PR 8 shipit's own worktree at its head, and delete its folder by hand.
Git still lists the worktree."
  (test-worktree-offer--worktree-at-the-pr-head)
  (delete-directory (test-worktree-offer--worktree) t))

(defun test-pr-worktree-found-cases--clone-on-the-pr-branch-behind-its-head ()
  "Check the clone out on a branch coffee at main, one commit behind the head of PR 8."
  (test-worktree-offer--git "clone" "checkout" "-q" "-b" "coffee"))

(defun test-pr-worktree-found-cases--check-out-main-in-the-clone ()
  "Check the clone out on main again, while the page is open."
  (test-worktree-offer--git "clone" "checkout" "-q" "main"))

(defun test-pr-worktree-found-cases--own-behind-and-mine-at-the-pr-head ()
  "Make my worktree kitchen-work at the head of PR 8, and shipit's own behind it."
  (test-pr-worktree-found--my-worktree-at-the-pr-head)
  (test-worktree-offer--worktree-behind-the-pr-head))

;;; A clone of another repository

(defconst test-pr-worktree-found-cases--other-address "https://github.com/acme/other.git"
  "The address of acme/other at GitHub, which git rewrites to its forge on disk.")

(defun test-pr-worktree-found-cases--build-other-forge ()
  "Make the forge of acme/other on disk, with the branches main and coffee of the seed."
  (test-worktree-offer--git "." "init" "-q" "--bare" "-b" "main" "other.git")
  (test-worktree-offer--git "other.git" "fetch" "-q" (test-worktree-offer--path "seed")
                            "main:refs/heads/main" "coffee:refs/heads/coffee"))

(defun test-pr-worktree-found-cases--origin-names-other ()
  "Make the remote origin of the clone name acme/other, whose forge is on disk.
So a fetch from origin works, and gets the commits of acme/other."
  (test-pr-worktree-found-cases--build-other-forge)
  (test-worktree-offer--git "clone" "config" "remote.origin.url"
                            test-pr-worktree-found-cases--other-address)
  (test-worktree-offer--git "clone" "config"
                            (format "url.%s.insteadOf" (test-worktree-offer--path "other.git"))
                            test-pr-worktree-found-cases--other-address))

(defun test-pr-worktree-found-cases--clone-of-other-with-its-worktrees ()
  "Make the clone a clone of acme/other, with two worktrees of its own.
One is in the folder .worktrees/pr-8-coffee, at main.  The other is
kitchen-work, on a branch coffee at main."
  (test-pr-worktree-found-cases--origin-names-other)
  (test-worktree-offer--worktree-behind-the-pr-head)
  (test-pr-worktree-found--add-my-worktree "main"))

(defun test-pr-worktree-found-cases--clone-of-other-and-o-r-with-my-worktree ()
  "Make the clone name acme/other as origin, and o/r as its remote upstream.
And make my worktree kitchen-work on a branch coffee at the head of PR 8."
  (test-pr-worktree-found-cases--origin-names-other)
  (test-worktree-offer--git "clone" "remote" "add" "upstream"
                            test-worktree-offer--github-address)
  (test-pr-worktree-found--my-worktree-at-the-pr-head))

(defmacro test-pr-worktree-found-cases--on-the-page-in-the-clone (given &rest body)
  "Run BODY on the page of pull request 8, in a kitchen that GIVEN prepared.
Magit and project.el know no repository, so the page runs git in the
clone of the kitchen, whatever repository that is a clone of."
  (declare (indent 1))
  `(let ((project--list nil)
         (magit-repository-directories nil))
     (test-pr-worktree-found--on-the-page-of-pr-8 ,given ,@body)))

(defun test-pr-worktree-found-cases--no-clone-text ()
  "The words that say that the clone is no clone of o/r."
  (format "%s is no clone of o/r"
          (abbreviate-file-name
           (file-name-as-directory (file-truename (test-worktree-offer--path "clone/"))))))

(defun test-pr-worktree-found-cases--no-clone-said ()
  "What the worktree menu says on the page in the clone, which is no clone of o/r."
  (format "%s, so this pull request has no worktree here"
          (test-pr-worktree-found-cases--no-clone-text)))

(defun test-pr-worktree-found-cases--head-of-pr-8-coffee ()
  "The commit that the worktree .worktrees/pr-8-coffee of the clone is at."
  (test-worktree-offer--git "clone/.worktrees/pr-8-coffee" "rev-parse" "HEAD"))

(defun test-pr-worktree-found-cases--worktrees-of-other-are-left ()
  "Check that git still lists pr-8-coffee and kitchen-work, and their folders exist."
  (dolist (folder '("pr-8-coffee" "kitchen-work"))
    (should (test-pr-worktree-found--listed-by-git-p folder)))
  (should (file-directory-p (test-worktree-offer--worktree)))
  (should (file-directory-p (test-pr-worktree-found--my-worktree))))

;;; What I do

(defvar test-pr-worktree-found-cases--offered nil
  "The choices that the question of RET offered, or nil when it asked none.")

(defun test-pr-worktree-found-cases--choose-from-the-worktree (_prompt collection &rest _)
  "Stand-in for `completing-read': note the choices of COLLECTION.
Choose to open the file from the worktree."
  (setq test-pr-worktree-found-cases--offered (all-completions "" collection))
  "from worktree (may differ from PR)")

(defun test-pr-worktree-found-cases--press-ret-on-file ()
  "Press RET on brew.sh among the changed files, shown in my window.
When RET asks how to open the file, choose to open it from the worktree."
  (setq test-pr-worktree-found-cases--offered nil)
  (switch-to-buffer test-worktree-offer--page)
  (goto-char (test-worktree-offer--file-line "brew.sh"))
  (let ((command (key-binding (kbd "RET") t)))
    (should (commandp command))
    (cl-letf (((symbol-function 'completing-read)
               #'test-pr-worktree-found-cases--choose-from-the-worktree))
      (call-interactively command))))

(defun test-pr-worktree-found-cases--directory-in-dired ()
  "The directory that Dired shows in my window, or nil when it shows no Dired."
  (with-current-buffer (window-buffer (selected-window))
    (and (derived-mode-p 'dired-mode)
         (file-name-as-directory (file-truename default-directory)))))

(defun test-pr-worktree-found-cases--open-worktree-directory ()
  "Run \"Open worktree directory\" of the worktree menu.
Return the directory that Dired then shows in my window, or nil.  Kill
each buffer of Dired that the command opened."
  (let ((before (buffer-list)))
    (test-pr-worktree-found--run-from-the-worktree-menu #'shipit--open-pr-worktree-directory)
    (prog1 (test-pr-worktree-found-cases--directory-in-dired)
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (with-current-buffer buffer (derived-mode-p 'dired-mode))
            (kill-buffer buffer)))))))

;;; What I see

(defun test-pr-worktree-found-cases--my-worktree-p (directory)
  "Whether DIRECTORY is my worktree kitchen-work."
  (equal (file-truename (test-pr-worktree-found--my-worktree)) directory))

(defun test-pr-worktree-found-cases--plain-file-shown-p (file)
  "Whether FILE is shown, and not in review mode."
  (not (test-worktree-offer--in-review-mode-p file)))

(defun test-pr-worktree-found-cases--asked-to-make-one (times)
  "Check that the question whether to make a worktree came TIMES times, and no other."
  (should (equal (list :choices nil
                       :yes-or-no (make-list times test-worktree-offer--question))
                 (test-pr-worktree-found--questions))))

;;; The list of worktrees that git gives

(defun test-pr-worktree-found-cases--worktrees-of-the-clone ()
  "The worktrees that shipit finds in the clone, each with its real path."
  (let ((default-directory (test-worktree-offer--path "clone/")))
    (mapcar (lambda (worktree)
              (plist-put (copy-sequence worktree) :path
                         (file-name-as-directory
                          (file-truename (plist-get worktree :path)))))
            (shipit--git-worktrees))))

(defun test-pr-worktree-found-cases--worktrees-outside-git ()
  "The worktrees that shipit finds in the kitchen, which is no git repository.
Git looks for no repository above the kitchen."
  (let ((default-directory test-worktree-offer--kitchen)
        (process-environment
         (cons (concat "GIT_CEILING_DIRECTORIES="
                       (directory-file-name
                        (file-name-directory (directory-file-name test-worktree-offer--kitchen))))
               process-environment)))
    (shipit--git-worktrees)))

(defun test-pr-worktree-found-cases--worktree (path head branch)
  "A worktree as shipit gives it: at PATH of the kitchen, with HEAD and BRANCH."
  (list :path (file-name-as-directory (file-truename (test-worktree-offer--path path)))
        :head head
        :branch branch))

;;; The scenarios

(ert-deftest test-pr-worktree-found-cases-my-worktree-on-no-branch-at-the-pr-head ()
  "My worktree on no branch, at the head of the pull request, opens in review mode."
  ;; GIVEN the page of pull request 8
  ;; AND a worktree kitchen-work that I made by hand on no branch, at
  ;;   the PR head
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of kitchen-work is shown in review mode
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found-cases--my-worktree-on-no-branch-at-the-pr-head
    (test-worktree-offer--press-c-ret-on-file)
    (test-pr-worktree-found--asked-nothing)
    (should (test-worktree-offer--in-review-mode-p
             (test-pr-worktree-found--in-my-worktree "brew.sh")))))

(ert-deftest test-pr-worktree-found-cases-my-branch-that-tracks-the-fork-by-its-remote ()
  "My branch coffee counts for a pull request from a fork when it tracks a remote of the fork."
  ;; GIVEN the page of pull request 8, which comes from the fork guest/r
  ;; AND my worktree kitchen-work with a branch coffee of my own, not at
  ;;   the PR head, that tracks the remote guest of that fork
  ;; WHEN I press C-RET on brew.sh and choose your worktree
  ;; THEN it asks whether to open the file from "your worktree" or "a
  ;;   review worktree"
  ;; AND brew.sh of kitchen-work is shown as a plain file
  (test-pr-worktree-found--when-the-pr-comes-from-the-fork
   (lambda ()
     (test-pr-worktree-found--on-the-page-of-pr-8
         #'test-pr-worktree-found-cases--my-branch-tracks-the-fork-by-its-remote
       (test-pr-worktree-found--press-c-ret-and-choose "your worktree")
       (test-pr-worktree-found--asked-which-worktree)
       (should (test-pr-worktree-found-cases--plain-file-shown-p
                (test-pr-worktree-found--in-my-worktree "brew.sh")))))))

(ert-deftest test-pr-worktree-found-cases-my-branch-that-tracks-the-fork-by-its-address ()
  "My branch coffee counts for a pull request from a fork when it tracks the address of the fork."
  ;; GIVEN the page of pull request 8, which comes from the fork guest/r
  ;; AND my worktree kitchen-work with a branch coffee of my own, not at
  ;;   the PR head, that tracks the address of that fork
  ;; THEN the Worktree section says "kitchen-work (your worktree)"
  (test-pr-worktree-found--when-the-pr-comes-from-the-fork
   (lambda ()
     (test-pr-worktree-found--on-the-page-of-pr-8
         #'test-pr-worktree-found-cases--my-branch-tracks-the-fork-by-its-address
       (test-pr-worktree-found--worktree-section-says "kitchen-work (your worktree)")))))

(ert-deftest test-pr-worktree-found-cases-a-worktree-whose-folder-was-deleted-is-none ()
  "A worktree that git still lists, but whose folder I deleted, does not count."
  ;; GIVEN the page of pull request 8, in a clone on main
  ;; AND my worktree kitchen-work at the PR head, whose folder I deleted
  ;;   by hand, so that git still lists it
  ;; WHEN I press C-RET on brew.sh and answer no
  ;; THEN the question "No worktree for PR #8. Create one for review
  ;;   mode?" comes
  ;; AND brew.sh of the clone is shown as a plain file
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found-cases--my-worktree-deleted-by-hand
    (should (test-pr-worktree-found--listed-by-git-p "kitchen-work"))
    (test-worktree-offer--press-c-ret-on-file 'no)
    (test-pr-worktree-found--asked-to-make-one)
    (should (test-pr-worktree-found-cases--plain-file-shown-p
             (test-worktree-offer--in-clone "brew.sh")))))

(ert-deftest test-pr-worktree-found-cases-a-clone-on-the-pr-branch-is-asked-about-once ()
  "A clone on the branch of the pull request, behind its head, is asked about once."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on the PR branch coffee, behind the PR head
  ;; WHEN I press C-RET on brew.sh and answer no
  ;; THEN the question "No worktree for PR #8. Create one for review
  ;;   mode?" comes
  ;; AND brew.sh of the clone is shown as a plain file
  ;; WHEN I press C-RET on brew.sh again
  ;; THEN no more question comes, and brew.sh of the clone is shown
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found-cases--clone-on-the-pr-branch-behind-its-head
    (test-worktree-offer--press-c-ret-on-file 'no)
    (test-pr-worktree-found--asked-to-make-one)
    (let ((brew (test-worktree-offer--in-clone "brew.sh")))
      (should (test-pr-worktree-found-cases--plain-file-shown-p brew))
      (test-worktree-offer--press-c-ret-on-file)
      (test-pr-worktree-found--asked-to-make-one)
      (should (test-pr-worktree-found-cases--plain-file-shown-p brew)))))

(ert-deftest test-pr-worktree-found-cases-a-clone-that-left-the-pr-branch-is-asked-again ()
  "A no given on the branch of the pull request no longer holds when the clone left that branch."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on the PR branch coffee, behind the PR head
  ;; AND I pressed C-RET on brew.sh and answered no
  ;; WHEN I check out main in the clone
  ;; AND press C-RET on brew.sh again
  ;; THEN the question "No worktree for PR #8. Create one for review
  ;;   mode?" comes again
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found-cases--clone-on-the-pr-branch-behind-its-head
    (test-worktree-offer--press-c-ret-on-file 'no)
    (test-pr-worktree-found-cases--asked-to-make-one 1)
    (test-pr-worktree-found-cases--check-out-main-in-the-clone)
    (test-worktree-offer--press-c-ret-on-file 'no)
    (test-pr-worktree-found-cases--asked-to-make-one 2)))

(ert-deftest test-pr-worktree-found-cases-own-worktree-whose-folder-was-deleted-is-made-again ()
  "Shipit's own worktree whose folder I deleted by hand is made again on yes."
  ;; GIVEN the page of pull request 8, in a clone on main
  ;; AND shipit's own worktree pr-8-coffee, whose folder I deleted by
  ;;   hand, so that git still lists it
  ;; WHEN I press C-RET on brew.sh and answer yes to "No worktree for
  ;;   PR #8. Create one for review mode?"
  ;; THEN the worktree is made again
  ;; AND brew.sh of it is shown in review mode
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found-cases--own-worktree-deleted-by-hand
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-pr-worktree-found--asked-to-make-one)
    (test-pr-worktree-found--wait-until-ready)
    (should (file-directory-p (test-worktree-offer--worktree)))
    (should (test-worktree-offer--in-review-mode-p
             (test-worktree-offer--in-worktree "brew.sh")))))

(ert-deftest test-pr-worktree-found-cases-shipits-own-behind-the-head-comes-first ()
  "Shipit's own worktree behind the head opens with a warning, also when mine is at the head."
  ;; GIVEN the page of pull request 8
  ;; AND shipit's own worktree pr-8-coffee, behind the PR head
  ;; AND my worktree kitchen-work at the PR head
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of pr-8-coffee is shown as a plain file
  ;; AND the warning says that it differs from the PR head, and does not
  ;;   name kitchen-work
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found-cases--own-behind-and-mine-at-the-pr-head
    (test-worktree-offer--press-c-ret-on-file)
    (test-pr-worktree-found--asked-nothing)
    (should (test-pr-worktree-found-cases--plain-file-shown-p
             (test-worktree-offer--in-worktree "brew.sh")))
    (should (test-worktree-offer--warned-p))
    (should-not (test-pr-worktree-found--warned-about-my-worktree-p))))

(ert-deftest test-pr-worktree-found-cases-ret-offers-no-sync-of-my-worktree ()
  "RET on a file with only my worktree, off the head, does not offer to sync it."
  ;; GIVEN the page of pull request 8
  ;; AND my worktree kitchen-work with the branch coffee and one commit
  ;;   that I did not push, and no worktree made by shipit
  ;; WHEN I press RET on brew.sh among the changed files
  ;; THEN it asks how to open the file, and offers to open it from the
  ;;   worktree or at the PR revision, but not to sync the worktree first
  ;; WHEN I choose to open it from the worktree
  ;; THEN brew.sh of kitchen-work is shown
  ;; AND kitchen-work still has my commit
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--my-worktree-with-my-commit
    (test-pr-worktree-found-cases--press-ret-on-file)
    (should (equal '("from worktree (may differ from PR)" "at PR revision (read-only)")
                   test-pr-worktree-found-cases--offered))
    (should (test-worktree-offer--shown (test-pr-worktree-found--in-my-worktree "brew.sh")))
    (test-pr-worktree-found--my-worktree-is-as-i-left-it)))

(ert-deftest test-pr-worktree-found-cases-open-worktree-directory-opens-mine ()
  "Open worktree directory opens my worktree when the pull request has no other."
  ;; GIVEN the page of pull request 8
  ;; AND my worktree kitchen-work with the branch coffee and one commit
  ;;   that I did not push, and no worktree made by shipit
  ;; WHEN I run "Open worktree directory" of the worktree menu
  ;; THEN my window shows kitchen-work in Dired
  ;; AND no question comes
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--my-worktree-with-my-commit
    (should (test-pr-worktree-found-cases--my-worktree-p
             (test-pr-worktree-found-cases--open-worktree-directory)))
    (test-pr-worktree-found--asked-nothing)))

(ert-deftest test-pr-worktree-found-cases-the-section-names-shipits-own-worktree ()
  "The Worktree section says that a worktree was made by shipit, when it is the only one."
  ;; GIVEN the page of pull request 8, whose worktree made by shipit is at
  ;;   the PR head, and no worktree of mine
  ;; THEN the Worktree section says "Path: .worktrees/pr-8-coffee (made
  ;;   by shipit)" on a line of its own
  (test-pr-worktree-found--on-the-page-of-pr-8 #'test-worktree-offer--worktree-at-the-pr-head
    (test-pr-worktree-found--worktree-section-says
     "Path: .worktrees/pr-8-coffee (made by shipit)\n")))

(ert-deftest test-pr-worktree-found-cases-git-lists-each-worktree ()
  "Shipit lists the worktrees of a clone as git does, the main one first."
  ;; GIVEN a clone on main
  ;; AND my worktree kitchen-work on the branch coffee, at the PR head
  ;; AND shipit's own worktree pr-8-coffee on no branch, at main
  ;; WHEN shipit lists the worktrees of the clone
  ;; THEN the clone comes first, on main
  ;; AND kitchen-work and pr-8-coffee follow, each with its commit and
  ;;   its branch, or no branch
  ;; WHEN shipit lists the worktrees of a folder that is in no repository
  ;; THEN it lists none
  (test-worktree-offer--in-a-new-kitchen
   (lambda ()
     (test-worktree-offer--build-kitchen)
     (test-pr-worktree-found--my-worktree-at-the-pr-head)
     (test-worktree-offer--worktree-behind-the-pr-head)
     (let ((main (test-worktree-offer--sha "main"))
           (worktrees (test-pr-worktree-found-cases--worktrees-of-the-clone)))
       (should (equal (test-pr-worktree-found-cases--worktree "clone" main "main")
                      (car worktrees)))
       (should (equal (list (test-pr-worktree-found-cases--worktree
                             "clone/.worktrees/pr-8-coffee" main nil)
                            (test-pr-worktree-found-cases--worktree
                             "kitchen-work" (test-worktree-offer--sha "coffee") "coffee"))
                      (seq-sort-by (lambda (worktree) (plist-get worktree :path)) #'string<
                                   (cdr worktrees)))))
     (should-not (test-pr-worktree-found-cases--worktrees-outside-git)))))

(ert-deftest test-pr-worktree-found-cases-a-clone-of-another-repo-shows-no-worktree ()
  "The Worktree section shows no worktree of another repository."
  ;; GIVEN the page of pull request 8 of o/r, in a clone of acme/other
  ;; AND that clone has a worktree .worktrees/pr-8-coffee, and a
  ;;   worktree kitchen-work on a branch coffee, the name of the PR branch
  ;; THEN the Worktree section says that the clone is no clone of o/r
  ;; AND it names neither pr-8-coffee nor kitchen-work
  (test-pr-worktree-found-cases--on-the-page-in-the-clone
      #'test-pr-worktree-found-cases--clone-of-other-with-its-worktrees
    (test-pr-worktree-found--worktree-section-says
     (format "Status: %s\n" (test-pr-worktree-found-cases--no-clone-text)))
    (let ((section (test-worktree-offer--worktree-section)))
      (should-not (string-search "pr-8-coffee" section))
      (should-not (string-search "kitchen-work" section)))))

(ert-deftest test-pr-worktree-found-cases-delete-in-a-clone-of-another-repo-is-refused ()
  "Delete worktree refuses to delete a worktree of another repository."
  ;; GIVEN the page of pull request 8 of o/r, in a clone of acme/other
  ;; AND that clone has a worktree .worktrees/pr-8-coffee, and a
  ;;   worktree kitchen-work on a branch coffee, the name of the PR branch
  ;; WHEN I run "Delete worktree" of the worktree menu, and would answer
  ;;   yes if asked
  ;; THEN it says that the clone is no clone of o/r, so this pull request
  ;;   has no worktree here
  ;; AND no question comes
  ;; AND git still lists both worktrees, and their folders exist
  (test-pr-worktree-found-cases--on-the-page-in-the-clone
      #'test-pr-worktree-found-cases--clone-of-other-with-its-worktrees
    (setq test-worktree-offer--answers '(yes))
    (should (equal (test-pr-worktree-found-cases--no-clone-said)
                   (test-pr-worktree-found--run-from-the-worktree-menu
                    #'shipit--delete-pr-worktree)))
    (test-pr-worktree-found--asked-nothing)
    (test-pr-worktree-found-cases--worktrees-of-other-are-left)))

(ert-deftest test-pr-worktree-found-cases-sync-in-a-clone-of-another-repo-is-refused ()
  "Sync worktree refuses to reset a worktree of another repository."
  ;; GIVEN the page of pull request 8 of o/r, in a clone of acme/other,
  ;;   whose forge has a branch coffee, the name of the PR branch
  ;; AND that clone has a worktree .worktrees/pr-8-coffee at main
  ;; WHEN I run "Sync worktree" of the worktree menu
  ;; THEN it says that the clone is no clone of o/r, so this pull request
  ;;   has no worktree here
  ;; AND pr-8-coffee is still at main
  (test-pr-worktree-found-cases--on-the-page-in-the-clone
      #'test-pr-worktree-found-cases--clone-of-other-with-its-worktrees
    (let ((head (test-pr-worktree-found-cases--head-of-pr-8-coffee)))
      (should (equal (test-pr-worktree-found-cases--no-clone-said)
                     (test-pr-worktree-found--run-from-the-worktree-menu
                      #'shipit--sync-pr-worktree)))
      (should (equal head (test-pr-worktree-found-cases--head-of-pr-8-coffee))))))

(ert-deftest test-pr-worktree-found-cases-checkout-in-a-clone-of-another-repo-is-refused ()
  "Checkout in worktree refuses to run git in another repository."
  ;; GIVEN the page of pull request 8 of o/r, in a clone of acme/other
  ;; AND that clone has a worktree .worktrees/pr-8-coffee, and a
  ;;   worktree kitchen-work on a branch coffee, the name of the PR branch
  ;; WHEN I run "Checkout in worktree" of the worktree menu
  ;; THEN it says that the clone is no clone of o/r, so this pull request
  ;;   has no worktree here
  ;; AND a moment later git lists the same worktrees as before
  (test-pr-worktree-found-cases--on-the-page-in-the-clone
      #'test-pr-worktree-found-cases--clone-of-other-with-its-worktrees
    (let ((listed (test-worktree-offer--git "clone" "worktree" "list" "--porcelain")))
      (should (equal (test-pr-worktree-found-cases--no-clone-said)
                     (test-pr-worktree-found--run-from-the-worktree-menu
                      #'shipit--checkout-pr-worktree)))
      (test-worktree-offer--let-a-moment-pass)
      (should (equal listed
                     (test-worktree-offer--git "clone" "worktree" "list" "--porcelain"))))))

(ert-deftest test-pr-worktree-found-cases-c-ret-in-a-clone-of-another-repo-opens-no-worktree ()
  "C-RET opens no file of a worktree of another repository."
  ;; GIVEN the page of pull request 8 of o/r, in a clone of acme/other
  ;; AND that clone has a worktree .worktrees/pr-8-coffee, and a
  ;;   worktree kitchen-work on a branch coffee, the name of the PR branch
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND no buffer visits a file in pr-8-coffee or in kitchen-work
  (test-pr-worktree-found-cases--on-the-page-in-the-clone
      #'test-pr-worktree-found-cases--clone-of-other-with-its-worktrees
    (test-worktree-offer--press-c-ret-on-file)
    (test-pr-worktree-found--asked-nothing)
    (should-not (test-worktree-offer--visiting (test-worktree-offer--worktree)))
    (should-not (test-worktree-offer--visiting (test-pr-worktree-found--my-worktree)))))

(ert-deftest test-pr-worktree-found-cases-a-second-remote-of-the-pr-repo-makes-a-clone ()
  "A clone whose second remote names the repository of the pull request has its worktrees."
  ;; GIVEN the page of pull request 8 of o/r, in a clone whose origin
  ;;   names acme/other and whose remote upstream names o/r
  ;; AND my worktree kitchen-work on a branch coffee, at the PR head
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of kitchen-work is shown in review mode
  (test-pr-worktree-found-cases--on-the-page-in-the-clone
      #'test-pr-worktree-found-cases--clone-of-other-and-o-r-with-my-worktree
    (test-worktree-offer--press-c-ret-on-file)
    (test-pr-worktree-found--asked-nothing)
    (should (test-worktree-offer--in-review-mode-p
             (test-pr-worktree-found--in-my-worktree "brew.sh")))))

(provide 'test-shipit-pr-worktree-found-cases)
;;; test-shipit-pr-worktree-found-cases.el ends here
