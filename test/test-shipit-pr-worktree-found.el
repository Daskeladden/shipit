;;; test-shipit-pr-worktree-found.el --- Shipit finds each worktree of a pull request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; Shipit finds the worktree of a pull request in the list of worktrees
;; that git keeps.  The worktree that shipit made, under .worktrees/,
;; counts.  So does a worktree that I made by hand, anywhere and with
;; any name, when it has the branch of the pull request checked out or
;; when its HEAD is the head of the pull request.  For a pull request
;; from a fork, the branch counts only when it tracks the fork.  A
;; folder that git does not list is no worktree.
;;
;; Shipit's own worktree comes first.  C-RET opens the file from my
;; worktree in review mode when it is at the head of the pull request.
;; When it is not, C-RET asks once for each pull request whether to
;; open the file from my worktree or from a review worktree that shipit
;; makes.  The worktree menu syncs and deletes only shipit's own
;; worktree.
;;
;; They use the kitchen, the page and the stand-ins of
;; test-shipit-worktree-offer.el.  My worktree kitchen-work lies in the
;; kitchen, next to the clone.  The question with two choices is stood
;; in for too.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'shipit-commands)
(require 'test-shipit-worktree-offer)

;; C-RET remembers here which worktree I chose for each pull request.
;; Each scenario starts with none.
(defvar shipit--pr-own-worktree-chosen)

;;; My worktree, made by hand next to the clone

(defvar test-pr-worktree-found--my-commit nil
  "The commit that I made in my worktree and did not push.")

(defun test-pr-worktree-found--my-worktree ()
  "The directory of my worktree kitchen-work."
  (test-worktree-offer--path "kitchen-work/"))

(defun test-pr-worktree-found--in-my-worktree (file)
  "FILE in my worktree kitchen-work."
  (expand-file-name file (test-pr-worktree-found--my-worktree)))

(defun test-pr-worktree-found--add-my-worktree (start)
  "Make my worktree kitchen-work from the clone, on a new branch coffee at START."
  (test-worktree-offer--git "clone" "worktree" "add" "-q" "-b" "coffee"
                            (test-pr-worktree-found--my-worktree) start))

(defun test-pr-worktree-found--commit-in-my-worktree (text message)
  "Commit TEXT as brew.sh in my worktree, with MESSAGE, and note the commit.
The clone has no name and e-mail of a committer, so the commit gives them."
  (with-temp-file (test-pr-worktree-found--in-my-worktree "brew.sh")
    (insert text))
  (test-worktree-offer--git "kitchen-work" "-c" "user.name=Cook"
                            "-c" "user.email=cook@example.com"
                            "commit" "-q" "-a" "-m" message)
  (setq test-pr-worktree-found--my-commit
        (test-worktree-offer--git "kitchen-work" "rev-parse" "HEAD")))

;;; What each scenario is given, before the page opens

(defun test-pr-worktree-found--my-worktree-at-the-pr-head ()
  "Make my worktree kitchen-work, on a branch coffee at the head of PR 8."
  (test-worktree-offer--fetch-the-pr)
  (test-pr-worktree-found--add-my-worktree "FETCH_HEAD"))

(defun test-pr-worktree-found--my-worktree-with-my-commit ()
  "Make my worktree kitchen-work at the head of PR 8, and a commit of mine.
I did not push the commit."
  (test-pr-worktree-found--my-worktree-at-the-pr-head)
  (test-pr-worktree-found--commit-in-my-worktree
   "fill the pot\nboil two cups\nadd sugar\npour\n" "Add sugar"))

(defun test-pr-worktree-found--a-folder-that-git-does-not-list ()
  "Make the empty folder .worktrees/pr-8-coffee in the clone, which is no worktree."
  (make-directory (test-worktree-offer--worktree) t))

(defun test-pr-worktree-found--my-own-branch-coffee ()
  "Make my worktree kitchen-work with a branch coffee of my own, from main.
The branch has a commit of mine and tracks nothing, and the clone knows
no remote but origin."
  (test-pr-worktree-found--add-my-worktree "main")
  (test-pr-worktree-found--commit-in-my-worktree
   "fill the pot\nboil one cup\n" "Boil one cup"))

;;; Pull request 8 from a fork, as GitHub gives it

(defun test-pr-worktree-found--pull-request-from-the-fork ()
  "Pull request 8 as GitHub gives it, with its head in the fork guest/r."
  (let ((pull-request (test-worktree-offer--pull-request)))
    (cons `(head (sha . ,(test-worktree-offer--sha "coffee")) (ref . "coffee")
                 (repo (full_name . "guest/r") (fork . t)
                       (clone_url . "https://github.com/guest/r.git")))
          (seq-remove (lambda (field) (eq (car field) 'head)) pull-request))))

(defun test-pr-worktree-found--when-the-pr-comes-from-the-fork (scenario)
  "Run SCENARIO, a function, while pull request 8 comes from the fork guest/r.
The kitchen asks `test-worktree-offer--github' for GitHub's answers, and
the first answer whose pattern matches wins.  So the pull request from
the fork goes before the answers of the kitchen."
  (let ((answers-of-the-kitchen (symbol-function 'test-worktree-offer--github)))
    (cl-letf (((symbol-function 'test-worktree-offer--github)
               (lambda ()
                 (cons (list "/repos/o/r/pulls/8\\'" 200
                             (test-pr-worktree-found--pull-request-from-the-fork))
                       (funcall answers-of-the-kitchen)))))
      (funcall scenario))))

;;; The question with two choices

(defconst test-pr-worktree-found--which-worktree
  '("Your worktree kitchen-work is not at the head of PR #8. Open the file from"
    (?w "your worktree") (?r "a review worktree"))
  "What C-RET asks when my worktree is not at the head of pull request 8.
The prompt, then the key and the name of each choice.")

(defvar test-pr-worktree-found--choices nil
  "The choices still to make, by their names, one for each question.")

(defvar test-pr-worktree-found--choice-questions nil
  "The questions with choices asked so far, oldest first.
Each is the prompt, then the key and the name of each choice.")

(defun test-pr-worktree-found--choose (prompt choices &rest _)
  "Stand-in for `read-multiple-choice': note PROMPT and CHOICES, and choose.
Return the element of CHOICES whose name is the next choice, or nil
when no choice is left to make."
  (setq test-pr-worktree-found--choice-questions
        (append test-pr-worktree-found--choice-questions
                (list (cons (string-trim prompt)
                            (mapcar (lambda (choice) (list (car choice) (cadr choice)))
                                    choices)))))
  (let ((name (pop test-pr-worktree-found--choices)))
    (seq-find (lambda (choice) (equal (cadr choice) name)) choices)))

;;; A scenario

(defun test-pr-worktree-found--to-the-end-of-the-warning (scenario)
  "Run SCENARIO, a function, and then let the warning of a file it opened come.
The warning that a file may differ from the PR head comes 0.2 seconds
after the file opens.  It must come in the scenario that opened the
file, not in a later one that checks that no warning came."
  (unwind-protect
      (funcall scenario)
    (test-worktree-offer--let-a-moment-pass)))

(defun test-pr-worktree-found--on-the-page (given scenario)
  "Run SCENARIO on the page of pull request 8, in a kitchen that GIVEN prepared.
GIVEN and SCENARIO are functions.  The question with two choices is
stood in for, and no choice of an earlier scenario is remembered."
  (let ((shipit--pr-own-worktree-chosen (make-hash-table :test 'equal))
        (test-pr-worktree-found--choices nil)
        (test-pr-worktree-found--choice-questions nil)
        (test-pr-worktree-found--my-commit nil))
    (cl-letf (((symbol-function 'read-multiple-choice) #'test-pr-worktree-found--choose))
      (test-worktree-offer--on-the-page
       given
       (lambda () (test-pr-worktree-found--to-the-end-of-the-warning scenario))))))

(defmacro test-pr-worktree-found--on-the-page-of-pr-8 (given &rest body)
  "Run BODY on the page of pull request 8, in a kitchen that GIVEN prepared.
GIVEN is a function that prepares the forge or the clone."
  (declare (indent 1))
  `(test-pr-worktree-found--on-the-page ,given (lambda () ,@body)))

;;; What I do

(defun test-pr-worktree-found--press-c-ret-and-choose (choice)
  "Press C-RET on brew.sh among the changed files; make CHOICE, a name, if asked."
  (setq test-pr-worktree-found--choices (list choice))
  (test-worktree-offer--press-c-ret-on-file))

(defun test-pr-worktree-found--run-from-the-worktree-menu (command)
  "Run COMMAND of the worktree menu on the Worktree section of the page.
The page is shown in my window.  Return the text of the `user-error'
that COMMAND signals, or nil when it signals none."
  (switch-to-buffer test-worktree-offer--page)
  (goto-char (test-worktree-offer--position-with 'shipit-worktree t))
  (condition-case err
      (progn (call-interactively command) nil)
    (user-error (error-message-string err))))

(defun test-pr-worktree-found--wait-until-ready ()
  "Wait until the echo area says that a worktree is ready; ten seconds at most."
  (test-worktree-offer--wait-for
   "the echo area to say that the worktree is ready"
   (lambda () (test-worktree-offer--echo-area-said-p "Worktree ready at"))))

;;; What I see

(defun test-pr-worktree-found--questions ()
  "The questions asked so far: those with choices, and those of yes or no."
  (list :choices test-pr-worktree-found--choice-questions
        :yes-or-no test-worktree-offer--questions))

(defun test-pr-worktree-found--asked-nothing ()
  "Check that no question came."
  (should (equal '(:choices nil :yes-or-no nil) (test-pr-worktree-found--questions))))

(defun test-pr-worktree-found--asked-which-worktree ()
  "Check that the question which worktree to open came once, and no other."
  (should (equal (list :choices (list test-pr-worktree-found--which-worktree)
                       :yes-or-no nil)
                 (test-pr-worktree-found--questions))))

(defun test-pr-worktree-found--asked-to-make-one ()
  "Check that the question whether to make a worktree came once, and no other."
  (should (equal (list :choices nil :yes-or-no (list test-worktree-offer--question))
                 (test-pr-worktree-found--questions))))

(defun test-pr-worktree-found--warned-about-my-worktree-p ()
  "Whether the echo area warned that kitchen-work differs from the PR head.
The warning must name it as my worktree.  It comes with a delay, so
this lets a moment pass first."
  (test-worktree-offer--let-a-moment-pass)
  (seq-some (lambda (said)
              (and (string-search "your worktree kitchen-work" said)
                   (string-search test-worktree-offer--warning said)))
            test-worktree-offer--echoed))

(defun test-pr-worktree-found--mode-line-text (construct)
  "The text that the mode line CONSTRUCT shows, without its %-constructs.
`format-mode-line' gives an empty string in batch Emacs, so this reads
the construct itself: strings, variables, conditionals, widths, :eval
and :propertize, as the Elisp manual describes them in \"Mode Line Data\"."
  (cond
   ((stringp construct) construct)
   ((and construct (symbolp construct) (boundp construct))
    (test-pr-worktree-found--mode-line-text (symbol-value construct)))
   ((not (consp construct)) "")
   ((eq (car construct) :eval)
    (test-pr-worktree-found--mode-line-text (eval (cadr construct) t)))
   ((eq (car construct) :propertize)
    (test-pr-worktree-found--mode-line-text (cadr construct)))
   ((integerp (car construct))
    (test-pr-worktree-found--mode-line-text (cdr construct)))
   ((and (car construct) (symbolp (car construct)))
    (test-pr-worktree-found--mode-line-text
     (if (and (boundp (car construct)) (symbol-value (car construct)))
         (cadr construct)
       (caddr construct))))
   (t (mapconcat #'test-pr-worktree-found--mode-line-text construct ""))))

(defun test-pr-worktree-found--header-line-of (file)
  "The text of the header line of FILE, once a window shows it."
  (with-current-buffer (window-buffer (test-worktree-offer--shown file))
    (test-pr-worktree-found--mode-line-text header-line-format)))

(defun test-pr-worktree-found--worktree-section-says (&rest texts)
  "Check that the Worktree section of the page says each of TEXTS.
The section is unfolded first, as TAB does.  It can take a moment to
show a worktree that was just made, so this waits ten seconds at most."
  (test-worktree-offer--unfold-at (test-worktree-offer--position-with 'shipit-worktree t))
  (test-worktree-offer--run-until
   (lambda ()
     (let ((section (test-worktree-offer--worktree-section)))
       (seq-every-p (lambda (text) (string-search text section)) texts)))
   10)
  (let ((section (test-worktree-offer--worktree-section)))
    (dolist (text texts)
      (should (string-search text section)))))

(defun test-pr-worktree-found--listed-by-git-p (folder)
  "Whether git lists a worktree of the clone in a folder named FOLDER."
  (string-match-p (format "^worktree .*/%s$" (regexp-quote folder))
                  (test-worktree-offer--git "clone" "worktree" "list" "--porcelain")))

(defun test-pr-worktree-found--my-worktree-is-as-i-left-it ()
  "Check that my worktree kitchen-work still exists and still has my commit."
  (should (file-directory-p (test-pr-worktree-found--my-worktree)))
  (should (test-pr-worktree-found--listed-by-git-p "kitchen-work"))
  (should (equal test-pr-worktree-found--my-commit
                 (test-worktree-offer--git "kitchen-work" "rev-parse" "HEAD"))))

(defconst test-pr-worktree-found--left-as-it-is
  '("PR #8 has no worktree made by shipit" "your worktree kitchen-work is left as it is")
  "What the worktree menu says when pull request 8 has only my worktree.")

(defun test-pr-worktree-found--says-my-worktree-is-left-as-it-is (said)
  "Check that SAID tells that kitchen-work is left as it is.
SAID must tell too that PR #8 has no worktree made by shipit."
  (dolist (text test-pr-worktree-found--left-as-it-is)
    (should (string-search text (or said "")))))

;;; The scenarios

(ert-deftest test-pr-worktree-found-my-worktree-at-the-pr-head-opens-in-review-mode ()
  "My worktree at the head of the pull request opens in review mode, with no question."
  ;; GIVEN the page of pull request 8
  ;; AND a worktree kitchen-work that I made by hand, with the branch
  ;;   coffee checked out at the PR head
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of kitchen-work is shown in review mode
  ;; AND its header line names kitchen-work
  ;; AND the Worktree section says "kitchen-work (your worktree)"
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--my-worktree-at-the-pr-head
    (test-worktree-offer--press-c-ret-on-file)
    (test-pr-worktree-found--asked-nothing)
    (let ((brew (test-pr-worktree-found--in-my-worktree "brew.sh")))
      (should (test-worktree-offer--in-review-mode-p brew))
      (should (string-search "kitchen-work" (test-pr-worktree-found--header-line-of brew))))
    (test-pr-worktree-found--worktree-section-says "kitchen-work (your worktree)")))

(ert-deftest test-pr-worktree-found-my-worktree-off-the-pr-head-asks-once-and-opens-it ()
  "My worktree off the head of the pull request is asked about once, and then opened."
  ;; GIVEN the page of pull request 8
  ;; AND my worktree kitchen-work with the branch coffee and one commit
  ;;   that I did not push
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--my-worktree-with-my-commit
    ;; WHEN I press C-RET on brew.sh
    ;; THEN it asks whether to open the file from "your worktree" or "a
    ;;   review worktree", and the question names kitchen-work and PR #8
    ;; WHEN I choose your worktree
    (test-pr-worktree-found--press-c-ret-and-choose "your worktree")
    (test-pr-worktree-found--asked-which-worktree)
    ;; THEN brew.sh of kitchen-work opens as a plain file
    ;; AND the warning names kitchen-work and says that it differs from
    ;;   the PR head
    (let ((brew (test-pr-worktree-found--in-my-worktree "brew.sh")))
      (should-not (test-worktree-offer--in-review-mode-p brew))
      (should (test-pr-worktree-found--warned-about-my-worktree-p))
      ;; WHEN I press C-RET on brew.sh again
      ;; THEN no question comes, and brew.sh of kitchen-work is shown
      (test-worktree-offer--press-c-ret-on-file)
      (test-pr-worktree-found--asked-which-worktree)
      (should (test-worktree-offer--shown brew)))))

(ert-deftest test-pr-worktree-found-a-review-worktree-leaves-my-worktree-alone ()
  "Choosing a review worktree makes shipit's own and leaves my worktree as it is."
  ;; GIVEN the page of pull request 8
  ;; AND my worktree kitchen-work with the branch coffee and one commit
  ;;   that I did not push
  ;; WHEN I press C-RET on brew.sh and choose a review worktree
  ;; THEN when it is ready, brew.sh of shipit's own worktree pr-8-coffee
  ;;   is shown in review mode
  ;; AND kitchen-work still has my commit
  ;; AND the Worktree section says "pr-8-coffee (made by shipit, in use)"
  ;;   and "kitchen-work (your worktree)"
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--my-worktree-with-my-commit
    (test-pr-worktree-found--press-c-ret-and-choose "a review worktree")
    (test-pr-worktree-found--asked-which-worktree)
    (should (test-worktree-offer--in-review-mode-p
             (test-worktree-offer--in-worktree "brew.sh")))
    (test-pr-worktree-found--my-worktree-is-as-i-left-it)
    (test-pr-worktree-found--worktree-section-says
     "pr-8-coffee (made by shipit, in use)" "kitchen-work (your worktree)")))

(ert-deftest test-pr-worktree-found-checkout-makes-shipits-own-beside-mine ()
  "Checkout in worktree makes shipit's own worktree also when mine exists, and C-RET uses it."
  ;; GIVEN the page of pull request 8
  ;; AND my worktree kitchen-work at the PR head
  ;; WHEN I run "Checkout in worktree" of the worktree menu
  ;; AND press C-RET on brew.sh once shipit's own worktree is ready
  ;; THEN brew.sh of pr-8-coffee is shown in review mode
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--my-worktree-at-the-pr-head
    (test-pr-worktree-found--run-from-the-worktree-menu #'shipit--checkout-pr-worktree)
    (test-pr-worktree-found--wait-until-ready)
    (test-worktree-offer--press-c-ret-on-file)
    (should (test-worktree-offer--in-review-mode-p
             (test-worktree-offer--in-worktree "brew.sh")))))

(ert-deftest test-pr-worktree-found-sync-and-delete-leave-my-worktree-alone ()
  "Sync worktree and Delete worktree say that they leave my worktree as it is, and do so."
  ;; GIVEN the page of pull request 8
  ;; AND my worktree kitchen-work with one commit that I did not push,
  ;;   and no worktree made by shipit
  ;; WHEN I run "Sync worktree" of the worktree menu
  ;; AND then "Delete worktree"
  ;; THEN each says that PR #8 has no worktree made by shipit and that
  ;;   kitchen-work is left as it is
  ;; AND no question comes
  ;; AND kitchen-work still exists and still has my commit
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--my-worktree-with-my-commit
    (let ((sync-said (test-pr-worktree-found--run-from-the-worktree-menu
                      #'shipit--sync-pr-worktree))
          (delete-said (test-pr-worktree-found--run-from-the-worktree-menu
                        #'shipit--delete-pr-worktree)))
      (test-pr-worktree-found--says-my-worktree-is-left-as-it-is sync-said)
      (test-pr-worktree-found--says-my-worktree-is-left-as-it-is delete-said))
    (test-pr-worktree-found--asked-nothing)
    (test-pr-worktree-found--my-worktree-is-as-i-left-it)))

(ert-deftest test-pr-worktree-found-a-folder-that-git-does-not-list-is-no-worktree ()
  "A folder under .worktrees that git does not list is no worktree, so C-RET offers to make one."
  ;; GIVEN the page of pull request 8, in a clone on main
  ;; AND an empty folder .worktrees/pr-8-coffee that git does not list
  ;;   as a worktree
  ;; WHEN I press C-RET on brew.sh and answer yes to "No worktree for
  ;;   PR #8. Create one for review mode?"
  ;; THEN the worktree is made
  ;; AND brew.sh of it is shown in review mode
  (test-pr-worktree-found--on-the-page-of-pr-8
      #'test-pr-worktree-found--a-folder-that-git-does-not-list
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-pr-worktree-found--asked-to-make-one)
    (test-pr-worktree-found--wait-until-ready)
    (should (test-pr-worktree-found--listed-by-git-p "pr-8-coffee"))
    (should (test-worktree-offer--in-review-mode-p
             (test-worktree-offer--in-worktree "brew.sh")))))

(ert-deftest test-pr-worktree-found-a-branch-of-the-same-name-is-not-the-forks ()
  "My branch coffee does not count for a pull request from a fork when it does not track it."
  ;; GIVEN the page of pull request 8, which comes from the fork guest/r
  ;; AND my worktree kitchen-work with a branch coffee of my own that
  ;;   does not track that fork
  ;; WHEN I press C-RET on brew.sh
  ;; THEN kitchen-work does not count: the question "No worktree for
  ;;   PR #8. Create one for review mode?" comes
  (test-pr-worktree-found--when-the-pr-comes-from-the-fork
   (lambda ()
     (test-pr-worktree-found--on-the-page-of-pr-8
         #'test-pr-worktree-found--my-own-branch-coffee
       (test-worktree-offer--press-c-ret-on-file)
       (test-pr-worktree-found--asked-to-make-one)))))

(provide 'test-shipit-pr-worktree-found)
;;; test-shipit-pr-worktree-found.el ends here
