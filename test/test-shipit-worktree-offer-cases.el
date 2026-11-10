;;; test-shipit-worktree-offer-cases.el --- More cases of the C-RET worktree offer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The cases of the C-RET offer that the scenarios of
;; test-shipit-worktree-offer.el leave out: a second C-RET while the
;; worktree is being made, a clone of another repository, and a pull
;; request whose head does not have the file.  One more pins that RET
;; still opens a file of a worktree at the PR head in review mode.
;;
;; They use the kitchen, the page and the stand-ins of that file.

;;; Code:

(require 'ert)
(require 'seq)
(require 'test-shipit-worktree-offer)

;;; What each scenario is given, before the page opens

(defun test-worktree-offer-cases--clone-of-another-repository ()
  "Make the clone a clone of o/kitchen, not of o/r."
  (test-worktree-offer--git "clone" "config" "remote.origin.url"
                            "https://github.com/o/kitchen.git"))

(defun test-worktree-offer-cases--pr-head-without-brew-sh ()
  "Give pull request 8 one more commit, which removes brew.sh.
The commit goes to the coffee branch of the seed and to the forge, so
GitHub gives it as the head of the pull request."
  (test-worktree-offer--git "seed" "rm" "-q" "brew.sh")
  (test-worktree-offer--git "seed" "commit" "-q" "-m" "Drink the coffee")
  (test-worktree-offer--git "forge.git" "fetch" "-q" (test-worktree-offer--path "seed")
                            "+coffee:refs/pull/8/head"))

;;; What I do on the page

(defun test-worktree-offer-cases--view-file ()
  "Run `shipit--file-at-point' on brew.sh among the changed files, as M-x does.
On the page no key runs it on that line: RET there runs
`shipit--open-file-at-point'.  <return> runs it on a comment of a diff
and in a diff buffer."
  (switch-to-buffer test-worktree-offer--page)
  (goto-char (test-worktree-offer--file-line "brew.sh"))
  (call-interactively #'shipit--file-at-point))

;;; What I see

(defun test-worktree-offer-cases--said-not-found-p (file)
  "Whether the echo area said that FILE was not found in the worktree.
Only a whole text of the echo area counts, so a line of the debug log
that quotes the error does not."
  (let ((not-found (format "\\`File not found in worktree: .*/%s\\'" (regexp-quote file))))
    (seq-some (lambda (said) (string-match-p not-found said))
              test-worktree-offer--echoed)))

;;; The scenarios

(ert-deftest test-worktree-offer-cases-c-ret-while-the-worktree-is-made-says-so ()
  "C-RET while the worktree is being made asks nothing, and the first file still opens."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on main
  ;; WHEN I press C-RET on brew.sh and answer yes
  ;; AND press C-RET on brew.sh again before the worktree is ready
  ;; THEN the question came only once
  ;; AND the echo area says "The worktree for PR #8 is still being made"
  ;; AND when the worktree is ready, brew.sh of the worktree is shown in
  ;;   review mode
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--clone-on-main
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-worktree-offer--press-c-ret-on-file)
    (test-worktree-offer--asked 1)
    (should (test-worktree-offer--echo-area-said-p
             "The worktree for PR #8 is still being made"))
    (should (test-worktree-offer--in-review-mode-p
             (test-worktree-offer--in-worktree "brew.sh")))))

(ert-deftest test-worktree-offer-cases-a-clone-of-another-repository-asks-nothing ()
  "C-RET in a clone of another repository opens the file of the clone, with no question."
  ;; GIVEN the page of pull request 8 of o/r, which has no worktree, in a
  ;;   clone of another repository
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of the clone opens as a plain file
  (test-worktree-offer--on-the-page-of-pr-8
      #'test-worktree-offer-cases--clone-of-another-repository
    (test-worktree-offer--press-c-ret-on-file)
    (test-worktree-offer--asked 0)
    (should-not (test-worktree-offer--in-review-mode-p
                 (test-worktree-offer--in-clone "brew.sh")))))

(ert-deftest test-worktree-offer-cases-a-file-missing-from-the-pr-head-opens-nothing ()
  "A file that the head of the pull request does not have opens nothing, and the echo area says so."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on main
  ;; AND a pull request whose last commit removes brew.sh
  ;; WHEN I press C-RET on brew.sh and answer yes
  ;; THEN when the worktree is ready, the echo area says that brew.sh was
  ;;   not found in the worktree
  ;; AND no file opens, and the page stays in my window
  (test-worktree-offer--on-the-page-of-pr-8
      #'test-worktree-offer-cases--pr-head-without-brew-sh
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-worktree-offer--asked 1)
    (test-worktree-offer--wait-for
     "the echo area to say that brew.sh was not found"
     (lambda () (test-worktree-offer-cases--said-not-found-p "brew.sh")))
    (should-not (test-worktree-offer--visiting test-worktree-offer--kitchen))
    (should (eq test-worktree-offer--page (window-buffer (selected-window))))))

(ert-deftest test-worktree-offer-cases-view-file-opens-a-worktree-at-the-pr-head-in-review-mode ()
  "Viewing a file of a pull request whose worktree is at its head opens it in review mode."
  ;; GIVEN the page of pull request 8, whose worktree exists and is at
  ;;   the PR head
  ;; WHEN I view brew.sh among the changed files with
  ;;   `shipit--file-at-point'
  ;; THEN no question comes
  ;; AND brew.sh of the worktree is shown in review mode, on its first line
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--worktree-at-the-pr-head
    (test-worktree-offer-cases--view-file)
    (test-worktree-offer--asked 0)
    (let ((brew (test-worktree-offer--in-worktree "brew.sh")))
      (should (test-worktree-offer--in-review-mode-p brew))
      (should (= 1 (test-worktree-offer--line-at-point brew))))))

(provide 'test-shipit-worktree-offer-cases)
;;; test-shipit-worktree-offer-cases.el ends here
