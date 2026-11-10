;;; test-shipit-fork-clone.el --- A clone of a fork has the worktrees of a pull request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A directory is a clone of the repository of a pull request when one
;; of its remotes names that repository.  It is one too when the forge
;; says that the repository of one of its remotes is a fork of it.  So
;; in a clone of my fork me/r, a pull request of o/r has worktrees: the
;; Worktree section shows them, C-RET offers to make one, and the
;; worktree menu makes and syncs one.  Each of them fetches the pull
;; request from o/r, not from my fork.
;;
;; Shipit asks the forge about a repository only when no remote names
;; the repository of the pull request.  It asks once, and asks again
;; when the forge gave no answer.  Of the repositories that magit and
;; project.el know, it asks only about one with the same name as the
;; repository of the pull request.
;;
;; They use the kitchen, the page and the stand-ins of
;; test-shipit-worktree-offer.el, test-shipit-pr-worktree-found.el and
;; test-shipit-pr-worktree-found-cases.el.  My fork has a forge of its
;; own on disk, fork.git, with main and nothing of the pull request.
;; Git still rewrites the address of o/r to the forge of the kitchen,
;; so a fetch from o/r works although no remote names it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'magit-repos)
(require 'shipit-commands)
(require 'shipit-test-http)
(require 'test-shipit-page-views)
(require 'test-shipit-worktree-offer)
(require 'test-shipit-pr-worktree-found)
(require 'test-shipit-pr-worktree-found-cases)

;;; My fork me/r, and clones of it

(defconst test-fork-clone--fork-address "https://github.com/me/r.git"
  "The address of my fork me/r at GitHub, which git rewrites to its forge on disk.")

(defun test-fork-clone--build-fork-forge ()
  "Make the forge of my fork me/r on disk, with main and nothing of pull request 8."
  (test-worktree-offer--git "." "init" "-q" "--bare" "-b" "main" "fork.git")
  (test-worktree-offer--git "fork.git" "fetch" "-q" (test-worktree-offer--path "seed")
                            "main:refs/heads/main"))

(defun test-fork-clone--clone-of-my-fork ()
  "Make the clone a clone of my fork me/r: its only remote, origin, names me/r.
Git still rewrites the address of o/r to the forge of the kitchen."
  (test-fork-clone--build-fork-forge)
  (test-worktree-offer--git "clone" "config" "remote.origin.url"
                            test-fork-clone--fork-address)
  (test-worktree-offer--git "clone" "config"
                            (format "url.%s.insteadOf" (test-worktree-offer--path "fork.git"))
                            test-fork-clone--fork-address))

(defun test-fork-clone--clone-of-my-fork-and-o-r ()
  "Make the clone name my fork me/r as origin, and o/r as its remote upstream."
  (test-fork-clone--clone-of-my-fork)
  (test-worktree-offer--git "clone" "remote" "add" "upstream"
                            test-worktree-offer--github-address))

(defconst test-fork-clone--namesake-address "https://github.com/acme/r.git"
  "The address of acme/r at GitHub, which git rewrites to a forge of its own on disk.")

(defun test-fork-clone--clone-of-a-namesake-with-a-worktree ()
  "Make the clone a clone of acme/r, with a worktree of its own.
acme/r has the name of o/r, and is another repository.  The worktree
is .worktrees/pr-8-coffee, at main."
  (test-pr-worktree-found-cases--build-other-forge)
  (test-worktree-offer--git "clone" "config" "remote.origin.url"
                            test-fork-clone--namesake-address)
  (test-worktree-offer--git "clone" "config"
                            (format "url.%s.insteadOf" (test-worktree-offer--path "other.git"))
                            test-fork-clone--namesake-address)
  (test-worktree-offer--worktree-behind-the-pr-head))

;;; Known projects, and a directory in no repository

(defun test-fork-clone--street ()
  "A directory of the kitchen that is in no repository."
  (test-worktree-offer--path "street/"))

(defun test-fork-clone--cups ()
  "The directory of the clone of acme/cups in the kitchen."
  (test-worktree-offer--path "cups/"))

(defun test-fork-clone--build-clone-of-cups ()
  "Make a clone of acme/cups in the kitchen: its remote origin names acme/cups."
  (test-worktree-offer--git "." "init" "-q" "-b" "main" "cups")
  (test-worktree-offer--git "cups" "remote" "add" "origin"
                            "https://github.com/acme/cups.git"))

(defun test-fork-clone--known-clones-of-my-fork-and-cups ()
  "Make the clone of my fork and a clone of acme/cups known to project.el.
The clone of acme/cups comes first, so that shipit sees it first.  And
make the street, which is in no repository.  The scenario binds
`project--list', so this sets only that binding."
  (test-fork-clone--clone-of-my-fork)
  (test-fork-clone--build-clone-of-cups)
  (make-directory (test-fork-clone--street))
  (setq project--list (list (list (test-fork-clone--cups))
                            (list (test-worktree-offer--path "clone/")))))

(defun test-fork-clone--git-stops-at-the-kitchen ()
  "The environment of processes, with git kept from repositories above the kitchen.
So the street is in no repository, wherever the kitchen is."
  (cons (concat "GIT_CEILING_DIRECTORIES="
                (directory-file-name
                 (file-name-directory (directory-file-name test-worktree-offer--kitchen))))
        process-environment))

(defun test-fork-clone--when-the-page-opens-from-the-street (scenario)
  "Run SCENARIO, a function, while the page of PR 8 opens from the street.
The kitchen opens the page with `test-worktree-offer--open-page', from
the clone.  Here it opens from the street, which is in no repository."
  (cl-letf (((symbol-function 'test-worktree-offer--open-page)
             (lambda ()
               (let ((default-directory (test-fork-clone--street))
                     (process-environment (test-fork-clone--git-stops-at-the-kitchen)))
                 (setq test-worktree-offer--page (test-page-views--open-pr 8))))))
    (funcall scenario)))

;;; GitHub's answers about a repository

(defun test-fork-clone--fork-of (repo parent)
  "GitHub's answer about REPO, as owner/name, which is a fork of PARENT."
  `((full_name . ,repo) (fork . t)
    (parent (full_name . ,parent)) (source (full_name . ,parent))))

(defun test-fork-clone--about (repo)
  "The pattern of GitHub's address of REPO itself, as owner/name.
It does not match the address of a part of REPO, such as its pull requests."
  (concat "/repos/" (regexp-quote repo) "\\'"))

(defun test-fork-clone--my-fork-is-of-o-r ()
  "GitHub's answers that say that me/r is a fork of o/r."
  (list (list (test-fork-clone--about "me/r") 200 (test-fork-clone--fork-of "me/r" "o/r"))))

(defun test-fork-clone--namesake-is-of-pots ()
  "GitHub's answers that say that acme/r is a fork of pots/r."
  (list (list (test-fork-clone--about "acme/r") 200
              (test-fork-clone--fork-of "acme/r" "pots/r"))))

(defvar test-fork-clone--github-answers nil
  "Whether GitHub answers about me/r now.  When nil, it gives an error.")

(defun test-fork-clone--status-about-my-fork ()
  "The status of GitHub's answer about me/r: an error, until it answers again."
  (if test-fork-clone--github-answers 200 500))

(defun test-fork-clone--body-about-my-fork (_sent)
  "The body of GitHub's answer about me/r: an error, until it answers again."
  (if test-fork-clone--github-answers
      (test-fork-clone--fork-of "me/r" "o/r")
    '((message . "Server Error"))))

(defun test-fork-clone--my-fork-is-of-o-r-after-an-error ()
  "GitHub's answers about me/r: an error, and a fork of o/r once it answers again."
  (list (list (test-fork-clone--about "me/r")
              #'test-fork-clone--status-about-my-fork
              #'test-fork-clone--body-about-my-fork)))

(defun test-fork-clone--github-answers-again ()
  "Make GitHub answer about me/r again."
  (setq test-fork-clone--github-answers t))

(defun test-fork-clone--when-github-also-answers (answers scenario)
  "Run SCENARIO, a function, while GitHub gives ANSWERS too.
The kitchen asks `test-worktree-offer--github' for GitHub's answers, and
the first answer whose pattern matches wins.  So ANSWERS go before the
answers of the kitchen."
  (let ((answers-of-the-kitchen (symbol-function 'test-worktree-offer--github)))
    (cl-letf (((symbol-function 'test-worktree-offer--github)
               (lambda () (append answers (funcall answers-of-the-kitchen)))))
      (funcall scenario))))

;;; A scenario

(defvar shipit--forked-from-cache)

(defun test-fork-clone--on-the-page (answers given scenario)
  "Run SCENARIO on the page of PR 8 of o/r, in a kitchen that GIVEN prepared.
GIVEN and SCENARIO are functions.  GitHub gives ANSWERS too, before
those of the kitchen.  Magit and project.el know no repository, unless
GIVEN makes some known.  Shipit remembers no answer of GitHub about a
fork from an earlier scenario."
  (let ((shipit--forked-from-cache (make-hash-table :test 'equal)))
    (test-fork-clone--when-github-also-answers
     answers
     (lambda ()
       (test-pr-worktree-found-cases--on-the-page-in-the-clone given
         (funcall scenario))))))

(defmacro test-fork-clone--on-the-page-of-pr-8 (answers given &rest body)
  "Run BODY on the page of pull request 8 of o/r, in a kitchen that GIVEN prepared.
GIVEN is a function that prepares the clone.  GitHub gives ANSWERS too."
  (declare (indent 2))
  `(test-fork-clone--on-the-page ,answers ,given (lambda () ,@body)))

;;; What I do

(defun test-fork-clone--run-unrefused (command)
  "Run COMMAND of the worktree menu, and check that it does not refuse."
  (should-not (test-pr-worktree-found--run-from-the-worktree-menu command)))

(defun test-fork-clone--press-g ()
  "Press g on the page, shown in my window, to draw it again."
  (switch-to-buffer test-worktree-offer--page)
  (goto-char (point-min))
  (test-page-views--press "g"))

(defun test-fork-clone--new-commit-on-the-pr ()
  "Give pull request 8 a new commit.
Commit to coffee in the seed, and make that commit the head of PR 8 on
the forge."
  (test-worktree-offer--commit "fill the pot\nboil two cups\nstir\npour\n" "Stir")
  (test-worktree-offer--git "forge.git" "fetch" "-q" (test-worktree-offer--path "seed")
                            "+coffee:refs/pull/8/head"))

;;; Waiting

(defun test-fork-clone--ready-or-failed-p ()
  "Whether the worktree of PR 8 is ready, or failed.
That is when the echo area said that it is ready, or the Worktree
section says that it failed."
  (or (test-worktree-offer--echo-area-said-p "Worktree ready at")
      (string-search "Failed" (test-worktree-offer--worktree-section))))

(defun test-fork-clone--wait-until-ready-or-failed ()
  "Wait until the worktree of PR 8 is ready, or failed; ten seconds at most."
  (test-worktree-offer--wait-for "the worktree to be ready, or to fail"
                                 #'test-fork-clone--ready-or-failed-p))

;;; What I see

(defun test-fork-clone--head-of-the-worktree ()
  "The commit that the worktree pr-8-coffee of the clone is at, or nil.
Nil when git lists no such worktree, or its folder does not exist."
  (and (test-pr-worktree-found--listed-by-git-p "pr-8-coffee")
       (file-directory-p (test-worktree-offer--worktree))
       (test-pr-worktree-found-cases--head-of-pr-8-coffee)))

(defun test-fork-clone--worktree-is-at-the-pr-head ()
  "Check that the worktree pr-8-coffee exists, at the head of pull request 8 now."
  (should (equal (test-worktree-offer--sha "coffee")
                 (test-fork-clone--head-of-the-worktree))))

(defun test-fork-clone--section-says-no-worktree ()
  "Check that the Worktree section says that no worktree is checked out.
It must not say that the clone is no clone of o/r."
  (test-pr-worktree-found--worktree-section-says "No worktree checked out")
  (should-not (string-search "is no clone of o/r" (test-worktree-offer--worktree-section))))

(defun test-fork-clone--section-says-no-clone ()
  "Check that the Worktree section says that the clone is no clone of o/r."
  (test-pr-worktree-found--worktree-section-says
   (format "Status: %s\n" (test-pr-worktree-found-cases--no-clone-text))))

(defun test-fork-clone--page-shown-p ()
  "Whether the page of pull request 8 is open and a window shows it."
  (and (buffer-live-p test-worktree-offer--page)
       (get-buffer-window test-worktree-offer--page)
       t))

(defun test-fork-clone--directory-of-the-page ()
  "The directory that the page runs git in, as its real path."
  (file-name-as-directory
   (file-truename (buffer-local-value 'default-directory test-worktree-offer--page))))

(defun test-fork-clone--directory-of-the-clone ()
  "The directory of the clone of the kitchen, as its real path."
  (file-name-as-directory (file-truename (test-worktree-offer--path "clone/"))))

(defun test-fork-clone--times-asked-about (repo)
  "How often GitHub was asked about REPO itself, as owner/name.
A request about a part of it, such as its pull requests, does not count."
  (seq-count (lambda (request) (string-match-p (test-fork-clone--about repo) (cadr request)))
             shipit-test-http-requests))

;;; The scenarios

(ert-deftest test-fork-clone-c-ret-makes-a-worktree-in-a-clone-of-my-fork ()
  "C-RET offers and makes the worktree of a pull request of o/r in a clone of my fork."
  ;; GIVEN a clone of my fork me/r, whose only remote is origin for me/r
  ;; AND GitHub says that me/r is a fork of o/r
  ;; AND the page of PR 8 of o/r, opened from that clone
  ;; THEN the Worktree section says "No worktree checked out", and not
  ;;   "is no clone of o/r"
  (test-fork-clone--on-the-page-of-pr-8 (test-fork-clone--my-fork-is-of-o-r)
      #'test-fork-clone--clone-of-my-fork
    (test-fork-clone--section-says-no-worktree)
    ;; WHEN I press C-RET on brew.sh and answer yes to "No worktree for
    ;;   PR #8. Create one for review mode?"
    ;; THEN the worktree pr-8-coffee is made at the head of the PR
    ;; AND brew.sh of it is shown in review mode
    ;; AND GitHub was asked about me/r once
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-pr-worktree-found--asked-to-make-one)
    (test-fork-clone--wait-until-ready-or-failed)
    (test-fork-clone--worktree-is-at-the-pr-head)
    (should (test-worktree-offer--in-review-mode-p
             (test-worktree-offer--in-worktree "brew.sh")))
    (should (= 1 (test-fork-clone--times-asked-about "me/r")))))

(ert-deftest test-fork-clone-checkout-and-sync-in-a-clone-of-my-fork ()
  "Checkout in worktree and Sync worktree get the pull request of o/r in a clone of my fork."
  ;; GIVEN a clone of my fork me/r, whose only remote is origin for me/r
  ;; AND GitHub says that me/r is a fork of o/r
  ;; AND the page of PR 8 of o/r, opened from that clone
  (test-fork-clone--on-the-page-of-pr-8 (test-fork-clone--my-fork-is-of-o-r)
      #'test-fork-clone--clone-of-my-fork
    ;; WHEN I run "Checkout in worktree" of the worktree menu
    ;; THEN it is not refused
    ;; AND the worktree pr-8-coffee is made at the head of the PR
    (test-fork-clone--run-unrefused #'shipit--checkout-pr-worktree)
    (test-fork-clone--wait-until-ready-or-failed)
    (test-fork-clone--worktree-is-at-the-pr-head)
    ;; WHEN the PR gets a new commit
    ;; AND I run "Sync worktree"
    ;; THEN the worktree is at the new head of the PR
    (test-fork-clone--new-commit-on-the-pr)
    (test-fork-clone--run-unrefused #'shipit--sync-pr-worktree)
    (test-fork-clone--worktree-is-at-the-pr-head)))

(ert-deftest test-fork-clone-c-ret-with-a-remote-of-o-r-asks-github-nothing ()
  "C-RET makes the worktree from o/r in a clone of my fork whose second remote names o/r."
  ;; GIVEN a clone whose origin is my fork me/r and whose remote upstream
  ;;   is o/r
  ;; AND the page of PR 8 of o/r, opened from that clone
  ;; WHEN I press C-RET on brew.sh and answer yes to the question
  ;; THEN the worktree pr-8-coffee is made at the head of the PR
  ;; AND brew.sh of it is shown in review mode
  ;; AND GitHub was not asked about me/r
  (test-fork-clone--on-the-page-of-pr-8 nil #'test-fork-clone--clone-of-my-fork-and-o-r
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-pr-worktree-found--asked-to-make-one)
    (test-fork-clone--wait-until-ready-or-failed)
    (test-fork-clone--worktree-is-at-the-pr-head)
    (should (test-worktree-offer--in-review-mode-p
             (test-worktree-offer--in-worktree "brew.sh")))
    (should (zerop (test-fork-clone--times-asked-about "me/r")))))

(ert-deftest test-fork-clone-a-fork-of-another-repo-is-no-clone ()
  "A clone of a fork of another repository is no clone of o/r, and its worktree is left alone."
  ;; GIVEN a clone of acme/r, which has a worktree pr-8-coffee of its own
  ;; AND GitHub says that acme/r is a fork of pots/r, not of o/r
  ;; AND the page of PR 8 of o/r, opened from that clone
  ;; THEN the Worktree section says that the clone is no clone of o/r
  (test-fork-clone--on-the-page-of-pr-8 (test-fork-clone--namesake-is-of-pots)
      #'test-fork-clone--clone-of-a-namesake-with-a-worktree
    (test-fork-clone--section-says-no-clone)
    ;; WHEN I run "Sync worktree"
    ;; THEN it says that the clone is no clone of o/r, so this pull
    ;;   request has no worktree here
    ;; AND the worktree of acme/r is as it was, at main
    (should (equal (test-pr-worktree-found-cases--no-clone-said)
                   (test-pr-worktree-found--run-from-the-worktree-menu
                    #'shipit--sync-pr-worktree)))
    (should (equal (test-worktree-offer--sha "main")
                   (test-fork-clone--head-of-the-worktree)))))

(ert-deftest test-fork-clone-github-is-asked-again-after-an-error ()
  "A clone of my fork counts once GitHub answers again about my fork."
  ;; GIVEN the clone of my fork me/r
  ;; AND GitHub does not answer about me/r: it gives an error
  ;; WHEN I open the page of PR 8 of o/r from that clone
  ;; THEN the page is shown
  ;; AND the Worktree section says that the clone is no clone of o/r
  (let ((test-fork-clone--github-answers nil))
    (test-fork-clone--on-the-page-of-pr-8 (test-fork-clone--my-fork-is-of-o-r-after-an-error)
        #'test-fork-clone--clone-of-my-fork
      (should (test-fork-clone--page-shown-p))
      (test-fork-clone--section-says-no-clone)
      ;; WHEN GitHub answers again that me/r is a fork of o/r
      ;; AND I draw the page again with g
      ;; THEN the Worktree section says "No worktree checked out"
      (test-fork-clone--github-answers-again)
      (test-fork-clone--press-g)
      (test-fork-clone--section-says-no-worktree))))

(ert-deftest test-fork-clone-a-known-clone-of-my-fork-is-found ()
  "A page opened outside every repository runs in a known clone of my fork."
  ;; GIVEN the clone of my fork me/r is a project that project.el knows
  ;; AND a clone of acme/cups is a known project too
  ;; AND Emacs is in a directory that is in no repository
  ;; AND GitHub says that me/r is a fork of o/r
  ;; WHEN I open the page of PR 8 of o/r
  ;; THEN the page runs in the clone of my fork
  ;; AND the Worktree section says "No worktree checked out"
  ;; AND GitHub was not asked about acme/cups
  (test-fork-clone--when-the-page-opens-from-the-street
   (lambda ()
     (test-fork-clone--on-the-page-of-pr-8 (test-fork-clone--my-fork-is-of-o-r)
         #'test-fork-clone--known-clones-of-my-fork-and-cups
       (should (equal (test-fork-clone--directory-of-the-clone)
                      (test-fork-clone--directory-of-the-page)))
       (test-fork-clone--section-says-no-worktree)
       (should (zerop (test-fork-clone--times-asked-about "acme/cups")))))))

(ert-deftest test-fork-clone-sync-gets-a-pr-from-a-fork-in-a-clone-of-o-r ()
  "Sync worktree gets the new head of a pull request from a fork, in a clone of o/r."
  ;; GIVEN a clone of o/r itself
  ;; AND PR 8 comes from the fork guest/r
  ;; AND the worktree pr-8-coffee that shipit made is at the head of the PR
  ;; WHEN the PR gets a new commit
  ;; AND I run "Sync worktree"
  ;; THEN the worktree is at the new head of the PR
  (test-pr-worktree-found--when-the-pr-comes-from-the-fork
   (lambda ()
     (test-fork-clone--on-the-page-of-pr-8 nil #'test-worktree-offer--worktree-at-the-pr-head
       (test-fork-clone--new-commit-on-the-pr)
       (test-fork-clone--run-unrefused #'shipit--sync-pr-worktree)
       (test-fork-clone--worktree-is-at-the-pr-head)))))

(provide 'test-shipit-fork-clone)
;;; test-shipit-fork-clone.el ends here
