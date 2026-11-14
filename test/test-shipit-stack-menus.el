;;; test-shipit-stack-menus.el --- Acceptance tests of the stack menus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The branch menu on `b' and the rebase menu on `r' of the stacked
;; preview, as a user meets them: keys pressed in the preview of a real
;; repository, whose remote is a bare repository beside it.  Only the
;; forge stands in.  The design is in
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

(provide 'test-shipit-stack-menus)
;;; test-shipit-stack-menus.el ends here
