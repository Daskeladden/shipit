;;; test-shipit-stack-menus.el --- Acceptance tests of the stack menus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The branch menu on `b' and the rebase menu on `r' of the stacked
;; preview, RET on its base, the line that names the next step, and the
;; offer to push a base that the remote lacks, as a user meets them:
;; keys pressed in the preview of a real repository, whose remote is a
;; bare repository beside it.  Only the forge stands in.  The design of
;; the menus is in
;; docs/superpowers/specs/2026-11-14-stack-rebase-menu-design.md.

;;; Code:

(require 'ert)
(require 'magit)
(require 'transient)
;; The sandbox stack and the second clone that acts for the remote.
(require 'test-shipit-stacked-preview)

;;;; Helpers

(defun test-stack-menus--press (buffer keys)
  "Press KEYS in BUFFER, the preview, the way a user does.
No key in these scenarios asks a question, so a prompt fails the test
rather than waiting for an answer that never comes."
  (set-window-buffer (selected-window) buffer)
  (with-current-buffer buffer
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (prompt &rest _) (error "Asked for input: %s" prompt))))
      (execute-kbd-macro (kbd keys)))))

(defun test-stack-menus--menu-text (buffer key)
  "Open the menu on KEY in BUFFER, close it again, and return what it showed."
  (when (get-buffer " *transient*") (kill-buffer " *transient*"))
  (test-stack-menus--press buffer key)
  (let ((menu (get-buffer " *transient*")))
    (unwind-protect
        (if menu
            (with-current-buffer menu (substring-no-properties (buffer-string)))
          "")
      (condition-case nil (execute-kbd-macro (kbd "C-g")) (quit nil)))))

(defun test-stack-menus--wait-for-git ()
  "Wait until the git that magit runs has finished, ten seconds at most."
  (let ((deadline (+ (float-time) 10)))
    (while (and magit-this-process (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (when magit-this-process (error "Git still runs after ten seconds"))))

(defun test-stack-menus--row (buffer branch)
  "Return the row of the layer BRANCH as BUFFER shows it."
  (with-current-buffer buffer
    (let ((start (or (shipit-stacked-preview--layer-position branch)
                     (error "No row for %s" branch))))
      (save-excursion
        (goto-char start)
        (buffer-substring-no-properties start (line-end-position))))))

(defun test-stack-menus--rebase-stopped-p ()
  "Return non-nil when a rebase is stopped in the working directory."
  (or (file-directory-p (expand-file-name ".git/rebase-merge"))
      (file-directory-p (expand-file-name ".git/rebase-apply"))))

(defun test-stack-menus--squash-merge-bottom (root number &optional then-edit)
  "Squash-merge the bottom layer into main on the remote, from another clone.
ROOT holds the sandbox.  NUMBER is the pull request that merged it, as
the forge tells it, or nil when the forge knows of none.  With
THEN-EDIT, main later changes a line that the layer added, so git alone
can no longer tell that the layer merged."
  (let ((default-directory (test-stacked--other-clone root))
        (header (shipit-sandbox--file-of "parser: read the header")))
    (shipit-sandbox--git "checkout" "-q" "main")
    (shipit-sandbox--git "merge" "-q" "--squash" "origin/work-parser")
    (shipit-sandbox--git "commit" "-q" "-m" "parser: read the file")
    (when then-edit
      (with-temp-file header (insert "parser: read the header, renamed"))
      (shipit-sandbox--git "commit" "-q" "-a" "-m" "parser: rename the header"))
    (shipit-sandbox--git "push" "-q" "origin" "main" ":work-parser"))
  (when number
    (push (cons "work-parser" number) shipit-sandbox-merged)))

(defun test-stack-menus--commit-a-fixup-for-the-bottom-layer ()
  "Change the newest commit of the bottom layer with a fixup on top of the stack."
  (with-temp-file (shipit-sandbox--file-of "parser: read the body")
    (insert "parser: read the body, fixed"))
  (shipit-sandbox--git "commit" "-q" "-a" "--fixup=work-parser"))

;;;; Which layers merged

(ert-deftest test-shipit-stack-menus-squash-merged-layer-reads-as-merged ()
  ;; GIVEN a pushed stack of two layers, whose bottom layer was
  ;; squash-merged into main through pull request #101
  ;; AND main later changed a line of that layer
  ;; WHEN the preview is refreshed
  ;; THEN the bottom row says that #101 merged into main
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--squash-merge-bottom root 101 'then-edit)
      (test-stack-menus--press preview "g")
      (should (string-match-p "#101 merged into main"
                              (test-stack-menus--row preview "work-parser"))))))

(ert-deftest test-shipit-stack-menus-git-tells-a-merge-the-forge-does-not ()
  ;; GIVEN a pushed stack whose bottom layer was squash-merged into main
  ;; AND the forge knows of no pull request that merged it
  ;; WHEN the preview is refreshed
  ;; THEN the bottom row says merged into main
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--squash-merge-bottom root nil)
      (test-stack-menus--press preview "g")
      (should (string-match-p "merged into main"
                              (test-stack-menus--row preview "work-parser"))))))

;;;; The rebase menu

(ert-deftest test-shipit-stack-menus-rebase-menu-shows-the-plan ()
  ;; GIVEN a pushed stack whose bottom layer merged through #101, and
  ;; main changed since
  ;; WHEN r is pressed
  ;; THEN the menu shows the plan, top layer first: work moves onto main,
  ;; and work-parser is left out, merged in #101
  ;; AND of the rebase actions it offers only the rebase onto main
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--squash-merge-bottom root 101 'then-edit)
      (test-stack-menus--press preview "g")
      (let* ((menu (test-stack-menus--menu-text preview "r"))
             (top (string-match "work +moves onto main" menu))
             (bottom (string-match "work-parser +merged in #101, left out" menu)))
        (should top)
        (should bottom)
        (should (< top bottom))
        (should (string-match-p "onto main" menu))
        (should-not (string-match-p "remote copies" menu))
        (should-not (string-match-p "fixup" menu))))))

(ert-deftest test-shipit-stack-menus-rebase-leaves-the-merged-layer-out ()
  ;; GIVEN a pushed stack whose bottom layer merged through #101, and
  ;; main changed a line of that layer since
  ;; WHEN r and then b are pressed
  ;; THEN the rebase finishes with no conflict
  ;; AND the top layer sits on the newest main with only its own two
  ;; commits
  ;; AND the preview, drawn again, shows the top layer alone
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--squash-merge-bottom root 101 'then-edit)
      (test-stack-menus--press preview "g")
      (test-stack-menus--press preview "r b")
      (test-stack-menus--wait-for-git)
      (should-not (test-stack-menus--rebase-stopped-p))
      (should (shipit-stack--ancestor-p "origin/main" "work"))
      (should (equal "2" (shipit-sandbox--git "rev-list" "--count" "origin/main..work")))
      (with-current-buffer preview
        (should (shipit-stacked-preview--layer-position "work"))
        (should-not (shipit-stacked-preview--layer-position "work-parser"))))))

(ert-deftest test-shipit-stack-menus-fixups-land-in-their-layers ()
  ;; GIVEN a pushed stack of two layers
  ;; AND a fix for the newest commit of the bottom layer, committed as a
  ;; fixup on top of the stack
  ;; WHEN r and then f are pressed
  ;; THEN the fix is in the bottom layer and no fixup commit is left
  ;; AND both branches followed: the bottom branch still ends at its own
  ;; commit, under the top branch
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--commit-a-fixup-for-the-bottom-layer)
      (test-stack-menus--press preview "g")
      (test-stack-menus--press preview "r f")
      (test-stack-menus--wait-for-git)
      (should-not (test-stack-menus--rebase-stopped-p))
      (should (equal "parser: read the body, fixed"
                     (shipit-sandbox--git "show" (concat "work-parser:"
                                                         (shipit-sandbox--file-of
                                                          "parser: read the body")))))
      (should-not (string-match-p "fixup!" (shipit-sandbox--git "log" "--format=%s"
                                                               "origin/main..work")))
      (should (equal "parser: read the body"
                     (shipit-sandbox--git "log" "-1" "--format=%s" "work-parser")))
      (should (shipit-stack--ancestor-p "work-parser" "work")))))

(ert-deftest test-shipit-stack-menus-stopped-rebase-offers-continue-skip-abort ()
  ;; GIVEN a pushed stack whose bottom layer merged through #101, and
  ;; main changed a line of that layer since
  ;; AND a rebase of the whole stack onto main, started by hand, that
  ;; stopped on a conflict
  ;; WHEN r is pressed
  ;; THEN the menu offers continue, skip and abort
  ;; AND it offers no new rebase
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--squash-merge-bottom root 101 'then-edit)
      (test-stack-menus--press preview "g")
      (call-process "git" nil nil nil "rebase" "--update-refs" "origin/main")
      (should (test-stack-menus--rebase-stopped-p))
      (let ((menu (test-stack-menus--menu-text preview "r")))
        (should (string-match-p "[Cc]ontinue" menu))
        (should (string-match-p "[Ss]kip" menu))
        (should (string-match-p "[Aa]bort" menu))
        (should-not (string-match-p "onto main" menu))))))

;;;; The branch menu

(ert-deftest test-shipit-stack-menus-branch-menu-creates-the-layer-branches ()
  ;; GIVEN the preview of a stack whose bottom layer has no branch yet
  ;; WHEN b and then c are pressed
  ;; THEN the bottom layer has its branch, at the newest of its commits
  (shipit-sandbox-with-repo
    (shipit-stacked-preview)
    (let ((preview (get-buffer "*shipit-stacked-preview: work*")))
      (should-not (shipit-stack--rev "work-parser"))
      (test-stack-menus--press preview "b c")
      (should (equal (shipit-stack--rev "work~2")
                     (shipit-stack--rev "work-parser"))))))

;;;; Faults found while building the menus

(defun test-stack-menus--menu-after-a-key-elsewhere (buffer keys)
  "Open the rebase menu in BUFFER, press KEYS while another buffer is current.
Return the text of the open menu, or nil when no menu is open any
more.  The menu is closed again afterwards."
  (when (get-buffer " *transient*") (kill-buffer " *transient*"))
  (test-stack-menus--press buffer "r")
  (with-temp-buffer (execute-kbd-macro (kbd keys)))
  (unwind-protect
      (when-let* ((menu (get-buffer " *transient*")))
        (with-current-buffer menu (buffer-string)))
    (condition-case nil (execute-kbd-macro (kbd "C-g")) (quit nil))))

(defun test-stack-menus--switched-on-p (menu argument)
  "Return non-nil when the text MENU shows ARGUMENT as on."
  (let ((at (string-search (format "(%s)" argument) menu)))
    (and at (memq 'transient-argument
                  (ensure-list (get-text-property (1+ at) 'face menu))))))

(ert-deftest test-shipit-stack-menus-menu-keeps-to-its-preview ()
  ;; GIVEN a pushed stack whose bottom layer merged through #101, and
  ;;   main changed since
  ;; AND the rebase menu opened from its preview
  ;; WHEN -i is pressed while another buffer is current, as when the
  ;;   menu is used from a window that does not show the preview
  ;; THEN the menu is still open and shows the option as on
  ;; AND it still shows the plan of that preview
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--squash-merge-bottom root 101 'then-edit)
      (test-stack-menus--press preview "g")
      (let ((menu (test-stack-menus--menu-after-a-key-elsewhere preview "-i")))
        (should menu)
        (should (test-stack-menus--switched-on-p menu "--interactive"))
        (should (string-match-p "work-parser +merged in #101, left out" menu))))))

;;;; More cases

(ert-deftest test-shipit-stack-menus-nothing-to-move-offers-no-rebase ()
  ;; GIVEN a pushed stack that stands on the newest main, with no layer
  ;;   merged
  ;; WHEN r is pressed
  ;; THEN the plan says that each layer is already on main
  ;; AND the menu offers no rebase onto main
  (shipit-sandbox-with-repo
    (let* ((preview (test-stacked--open-pushed-stack))
           (menu (test-stack-menus--menu-text preview "r")))
      (should (string-match-p "work +already on main" menu))
      (should (string-match-p "work-parser +already on main" menu))
      (should-not (string-match-p "onto main" menu)))))

;;;; Changing the base

(defun test-stack-menus--open-with-a-release-branch ()
  "Open the preview of the sandbox stack, with a branch release on the remote.
The release branch already holds the first commit of the stack, so a
stack raised against it is one commit shorter than one raised against
main.  Returns the preview buffer."
  (shipit-sandbox--git "push" "-q" "origin" "work~3:refs/heads/release")
  (shipit-stacked-preview)
  (get-buffer "*shipit-stacked-preview: work*"))

(defun test-stack-menus--press-on-line (buffer line keys)
  "Put point on the line of BUFFER that matches LINE, then press KEYS.
LINE is a regexp for the start of the line.  KEYS can answer a prompt,
the way a user types the answer."
  (set-window-buffer (selected-window) buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (re-search-forward line))
    (goto-char (match-beginning 0))
    (execute-kbd-macro (kbd keys))))

(defun test-stack-menus--header (buffer)
  "Return the first line of BUFFER, the one that names the base."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun test-stack-menus--base-row (buffer)
  "Return the row at the bottom of BUFFER that names the base."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (re-search-forward "^  base .*$"))
      (substring-no-properties (match-string 0)))))

(defconst test-stack-menus--base-line "^  Base:"
  "The line in the header of the preview that names the base.")

(defconst test-stack-menus--base-row-line "^  base "
  "The row at the bottom of the preview that names the base.")

(ert-deftest test-shipit-stack-menus-ret-on-the-base-line-changes-the-base ()
  ;; GIVEN the preview of a stack of four commits over main
  ;; AND a branch release on the remote that already holds the first
  ;;   commit of the stack
  ;; WHEN RET is pressed on the Base: line and origin/release is picked
  ;; THEN the header says 3 commits over release
  ;; AND the bottom layer work-parser holds 1 commit and work 2
  ;; AND the base row at the bottom says release
  (shipit-sandbox-with-repo
    (let ((preview (test-stack-menus--open-with-a-release-branch)))
      (should (string-match-p "4 commits over main"
                              (test-stack-menus--header preview)))
      (test-stack-menus--press-on-line preview test-stack-menus--base-line
                                       "RET origin/release RET")
      (should (string-match-p "3 commits over release"
                              (test-stack-menus--header preview)))
      (should (string-match-p "work-parser +1 commit\\b"
                              (test-stack-menus--row preview "work-parser")))
      (should (string-match-p "work +2 commits"
                              (test-stack-menus--row preview "work")))
      (should (string-match-p "base +release"
                              (test-stack-menus--base-row preview))))))

(ert-deftest test-shipit-stack-menus-ret-on-the-base-row-changes-the-base ()
  ;; GIVEN the preview of a stack of four commits over main
  ;; AND a branch release on the remote that already holds the first
  ;;   commit of the stack
  ;; WHEN RET is pressed on the base row at the bottom and
  ;;   origin/release is picked
  ;; THEN the header says 3 commits over release
  (shipit-sandbox-with-repo
    (let ((preview (test-stack-menus--open-with-a-release-branch)))
      (test-stack-menus--press-on-line preview test-stack-menus--base-row-line
                                       "RET origin/release RET")
      (should (string-match-p "3 commits over release"
                              (test-stack-menus--header preview))))))

(ert-deftest test-shipit-stack-menus-new-base-proposes-the-divisions-again ()
  ;; GIVEN the preview of a stack of two layers over main, parser and
  ;;   writer, as the subjects propose
  ;; AND a division made by hand with s after "writer: emit the header",
  ;;   so the top layer work holds 1 commit
  ;; AND a branch release on the remote that holds the first commit
  ;; WHEN the base is changed to origin/release with RET on the Base: line
  ;; THEN the divisions are the ones the subjects propose: work-parser
  ;;   holds 1 commit and work holds 2 again
  (shipit-sandbox-with-repo
    (let ((preview (test-stack-menus--open-with-a-release-branch)))
      (test-stack-menus--press-on-line preview "writer: emit the header" "s")
      (should (string-match-p "work +1 commit\\b"
                              (test-stack-menus--row preview "work")))
      (test-stack-menus--press-on-line preview test-stack-menus--base-line
                                       "RET origin/release RET")
      (should (string-match-p "work-parser +1 commit\\b"
                              (test-stack-menus--row preview "work-parser")))
      (should (string-match-p "work +2 commits"
                              (test-stack-menus--row preview "work"))))))

(ert-deftest test-shipit-stack-menus-base-with-no-commits-keeps-the-old-base ()
  ;; GIVEN the preview of a stack of four commits over main
  ;; WHEN RET is pressed on the Base: line and work, the branch itself,
  ;;   is picked
  ;; THEN the preview says that there are no commits between work and HEAD
  ;; AND the header still says 4 commits over main
  (shipit-sandbox-with-repo
    (let* ((preview (test-stack-menus--open-with-a-release-branch))
           (refusal (should-error
                     (test-stack-menus--press-on-line
                      preview test-stack-menus--base-line "RET work RET")
                     :type 'user-error)))
      (should (string-match-p "No commits between work and HEAD"
                              (cadr refusal)))
      (should (string-match-p "4 commits over main"
                              (test-stack-menus--header preview))))))

(ert-deftest test-shipit-stack-menus-refresh-keeps-the-new-base ()
  ;; GIVEN the preview of a stack whose base was changed from main to
  ;;   origin/release with RET on the Base: line
  ;; WHEN g is pressed
  ;; THEN the header still says 3 commits over release
  (shipit-sandbox-with-repo
    (let ((preview (test-stack-menus--open-with-a-release-branch)))
      (test-stack-menus--press-on-line preview test-stack-menus--base-line
                                       "RET origin/release RET")
      (test-stack-menus--press preview "g")
      (should (string-match-p "3 commits over release"
                              (test-stack-menus--header preview))))))

(ert-deftest test-shipit-stack-menus-key-hints-name-the-base ()
  ;; GIVEN the preview of a stack
  ;; THEN the key hints say that RET opens a layer or sets the base
  (shipit-sandbox-with-repo
    (let ((preview (test-stack-menus--open-with-a-release-branch)))
      (with-current-buffer preview
        (should (string-match-p "RET +open layer, set base"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))))

;;;; The next step

(defun test-stack-menus--line-under-base (buffer)
  "Return the line of BUFFER right under the Base: line."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (re-search-forward test-stack-menus--base-line))
      (forward-line 1)
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun test-stack-menus--next-step-shown-p (buffer)
  "Return non-nil when BUFFER has a line that names the next step."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (re-search-forward "^  Next:" nil t)))))

(defun test-stack-menus--give-pull-requests (&rest pairs)
  "Tell the forge that each branch in PAIRS has the pull request after it.
PAIRS alternate a branch name and a number."
  (while pairs
    (push (cons (pop pairs) (pop pairs)) shipit-sandbox-prs)))

(ert-deftest test-shipit-stack-menus-next-step-without-branches ()
  ;; GIVEN the preview of a stack whose lower layer has no branch yet
  ;; THEN the line under Base: says to check the base and the divisions,
  ;;   then press b c to give each layer a branch
  (shipit-sandbox-with-repo
    (shipit-stacked-preview)
    (let ((line (test-stack-menus--line-under-base
                 (get-buffer "*shipit-stacked-preview: work*"))))
      (should (string-match-p
               "^  Next: +check the base and the divisions, then press b c to give each layer a branch"
               line)))))

(ert-deftest test-shipit-stack-menus-next-step-after-the-branches ()
  ;; GIVEN the preview of a stack whose layers have no branches yet
  ;; WHEN b and then c are pressed
  ;; THEN the line under Base: says to press P to push the layers
  (shipit-sandbox-with-repo
    (shipit-stacked-preview)
    (let ((preview (get-buffer "*shipit-stacked-preview: work*")))
      (test-stack-menus--press preview "b c")
      (should (string-match-p "^  Next: +press P to push the layers"
                              (test-stack-menus--line-under-base preview))))))

(ert-deftest test-shipit-stack-menus-next-step-after-the-push ()
  ;; GIVEN the preview of a stack whose layers have branches and are
  ;;   pushed, and no layer has a pull request
  ;; THEN the line under Base: says to press RET on work-parser to
  ;;   describe it and open its pull request
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (should (string-match-p
               "^  Next: +press RET on work-parser to describe it and open its pull request"
               (test-stack-menus--line-under-base preview))))))

(ert-deftest test-shipit-stack-menus-next-step-names-the-lowest-layer-left ()
  ;; GIVEN the preview of a pushed stack whose bottom layer work-parser
  ;;   has pull request #101 and whose top layer work has none
  ;; WHEN the preview is refreshed
  ;; THEN the line under Base: says to press RET on work
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (test-stack-menus--give-pull-requests "work-parser" 101)
      (test-stack-menus--press preview "g")
      (should (string-match-p "^  Next: +press RET on work to describe it"
                              (test-stack-menus--line-under-base preview))))))

(ert-deftest test-shipit-stack-menus-no-next-step-when-every-layer-has-a-pr ()
  ;; GIVEN the preview of a pushed stack with no pull request yet
  ;; THEN it shows a line that names the next step
  (shipit-sandbox-with-repo
    (let ((preview (test-stacked--open-pushed-stack)))
      (should (test-stack-menus--next-step-shown-p preview))
      ;; WHEN both layers get a pull request, #101 and #102, and the
      ;;   preview is refreshed
      (test-stack-menus--give-pull-requests "work-parser" 101 "work" 102)
      (test-stack-menus--press preview "g")
      ;; THEN the preview shows no line that names a next step
      (should-not (test-stack-menus--next-step-shown-p preview)))))

;;;; A base that the remote lacks

(defun test-stack-menus--open-on-a-local-base (base)
  "Open the pushed sandbox stack, then make BASE its base with RET on Base:.
BASE is a ref made here at main and never pushed.  Returns the preview."
  (let ((preview (test-stacked--open-pushed-stack)))
    (test-stack-menus--press-on-line preview test-stack-menus--base-line
                                     (format "RET %s RET" base))
    preview))

(defun test-stack-menus--draft-buffer (branch)
  "Return the buffer of the pull request description for BRANCH, or nil."
  (get-buffer (format "*shipit-preview: %s @ %s*" shipit-sandbox-repo branch)))

(defun test-stack-menus--open-bottom-layer-answering (preview answer)
  "Press RET on the bottom layer of PREVIEW and answer each question ANSWER.
Return (QUESTIONS . REFUSAL): the questions asked, oldest first, and
the words of the refusal, or nil when there was none."
  (let ((draft (test-stack-menus--draft-buffer "work-parser"))
        asked refusal)
    (when draft (kill-buffer draft))
    (set-window-buffer (selected-window) preview)
    (with-current-buffer preview
      (goto-char (shipit-stacked-preview--layer-position "work-parser"))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (push prompt asked) answer)))
        (condition-case err
            (execute-kbd-macro (kbd "RET"))
          (user-error (setq refusal (cadr err))))))
    (cons (nreverse asked) refusal)))

(defun test-stack-menus--remote-branch (name)
  "Return the commit of the branch NAME on the remote, or nil."
  (car (split-string (shipit-sandbox--git "ls-remote" "--heads" "origin" name))))

(ert-deftest test-shipit-stack-menus-yes-pushes-the-missing-base ()
  ;; GIVEN a pushed stack whose base release is a branch made here at
  ;;   main and never pushed
  ;; WHEN RET is pressed on the bottom layer and the question is
  ;;   answered with y
  ;; THEN shipit asked "The base release is not on the remote.  Push it?"
  ;; AND the remote has release at the same commit as here
  ;; AND the process log shows the push
  ;; AND the description of work-parser opens
  (shipit-sandbox-with-repo
    (shipit-sandbox--git "branch" "release" "main")
    (let* ((preview (test-stack-menus--open-on-a-local-base "release"))
           (answer (test-stack-menus--open-bottom-layer-answering preview t)))
      (unwind-protect
          (progn
            (should (string-match-p "The base release is not on the remote\\.  Push it\\?"
                                    (or (car (car answer)) "")))
            (should-not (cdr answer))
            (should (equal (shipit-sandbox--git "rev-parse" "release")
                           (test-stack-menus--remote-branch "release")))
            (should (with-current-buffer shipit-process-buffer-name
                      (string-match-p "git push .*origin release"
                                      (buffer-string))))
            (should (test-stack-menus--draft-buffer "work-parser")))
        (when-let* ((draft (test-stack-menus--draft-buffer "work-parser")))
          (kill-buffer draft))))))

(ert-deftest test-shipit-stack-menus-no-keeps-the-base-off-the-remote ()
  ;; GIVEN a pushed stack whose base release is a branch made here at
  ;;   main and never pushed
  ;; WHEN RET is pressed on the bottom layer and the question is
  ;;   answered with n
  ;; THEN shipit asked whether to push the base
  ;; AND it stops with "The base release is not on the remote yet"
  ;; AND the remote still has no branch release
  ;; AND no description opens
  (shipit-sandbox-with-repo
    (shipit-sandbox--git "branch" "release" "main")
    (let* ((preview (test-stack-menus--open-on-a-local-base "release"))
           (answer (test-stack-menus--open-bottom-layer-answering preview nil)))
      (should (string-match-p "The base release is not on the remote\\.  Push it\\?"
                              (or (car (car answer)) "")))
      (should (string-match-p "The base release is not on the remote yet"
                              (or (cdr answer) "")))
      (should-not (test-stack-menus--remote-branch "release"))
      (should-not (test-stack-menus--draft-buffer "work-parser")))))

(ert-deftest test-shipit-stack-menus-tag-as-base-is-refused-without-a-question ()
  ;; GIVEN a pushed stack whose base is the tag v1, made here at main
  ;; WHEN RET is pressed on the bottom layer
  ;; THEN shipit asks nothing
  ;; AND it stops with "The base v1 is not a branch on the remote"
  ;; AND no description opens
  (shipit-sandbox-with-repo
    (shipit-sandbox--git "tag" "v1" "main")
    (let* ((preview (test-stack-menus--open-on-a-local-base "v1"))
           (answer (test-stack-menus--open-bottom-layer-answering preview t)))
      (should-not (car answer))
      (should (string-match-p "The base v1 is not a branch on the remote"
                              (or (cdr answer) "")))
      (should-not (test-stack-menus--draft-buffer "work-parser")))))

(provide 'test-shipit-stack-menus)
;;; test-shipit-stack-menus.el ends here
