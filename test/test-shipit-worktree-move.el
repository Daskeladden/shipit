;;; test-shipit-worktree-move.el --- Moving the work in a checkout to a worktree -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; Work often starts in the main checkout and later earns a worktree of
;; its own.  These tests move it in the sandbox repository, the one the
;; stacked flow is tried in, and read the result from git.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit-sandbox)
(require 'shipit-worktree-move)

(defun test-move--git (&rest args)
  "The output of git ARGS here, trimmed."
  (string-trim (apply #'shipit--git-output args)))

(defun test-move--in (dir &rest args)
  "The output of git ARGS in DIR, trimmed."
  (let ((default-directory dir))
    (apply #'test-move--git args)))

(defun test-move--write (file text)
  "Write TEXT to FILE."
  (with-temp-file file (insert text)))

(defun test-move--text (file)
  "The text of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun test-move--commit (file text message)
  "Write TEXT to FILE and commit it with MESSAGE."
  (test-move--write file text)
  (test-move--git "add" file)
  (test-move--git "commit" "-q" "-m" message))

(defmacro test-move--on-main (&rest body)
  "Run BODY in the sandbox with `main' checked out."
  (declare (indent 0))
  `(shipit-sandbox-with-repo
     (test-move--git "checkout" "-q" "main")
     ,@body))

(defmacro test-move--visiting (buffer file &rest body)
  "Run BODY with BUFFER visiting FILE; kill every buffer of the sandbox after."
  (declare (indent 2))
  `(let ((,buffer (find-file-noselect ,file)))
     (unwind-protect
         (progn ,@body)
       (test-move--kill-sandbox-buffers))))

(defun test-move--kill-sandbox-buffers ()
  "Kill the buffers that visit files of the sandbox, without saving them."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (and file (string-prefix-p temporary-file-directory file))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

;;; Moving a branch

(ert-deftest test-move-a-branch-to-a-worktree ()
  "GIVEN the branch `work' checked out, with an edit and a new file not
committed
WHEN it is moved to a worktree
THEN the worktree has the branch, the edit and the new file, the
checkout is on `main' with nothing changed, and nothing is left stashed."
  (shipit-sandbox-with-repo
    (test-move--write "base.txt" "an edit")
    (test-move--write "new.txt" "new")
    (let ((dir (shipit-worktree-move-branch "work")))
      (should (equal "work" (test-move--in dir "rev-parse" "--abbrev-ref" "HEAD")))
      (should (equal "an edit" (test-move--text (expand-file-name "base.txt" dir))))
      (should (equal "new" (test-move--text (expand-file-name "new.txt" dir))))
      (should (equal "main" (test-move--git "rev-parse" "--abbrev-ref" "HEAD")))
      (should (string-empty-p (test-move--git "status" "--porcelain")))
      (should (string-empty-p (test-move--git "stash" "list"))))))

(ert-deftest test-move-a-branch-not-checked-out ()
  "GIVEN a branch that is not checked out anywhere
WHEN it is moved to a worktree
THEN the worktree has it, and the checkout stays as it was."
  (shipit-sandbox-with-repo
    (test-move--git "branch" "other" "work~1")
    (let ((dir (shipit-worktree-move-branch "other")))
      (should (equal "other" (test-move--in dir "rev-parse" "--abbrev-ref" "HEAD")))
      (should (equal "work" (test-move--git "rev-parse" "--abbrev-ref" "HEAD"))))))

(ert-deftest test-move-finds-the-worktree-of-a-branch ()
  "GIVEN the branch `work', first in the checkout and then moved
WHEN its worktree is asked for
THEN there is none before the move, and the moved one after."
  (shipit-sandbox-with-repo
    (should-not (shipit-worktree-move-worktree-of "work"))
    (let ((dir (shipit-worktree-move-branch "work")))
      (should (equal dir (shipit-worktree-move-worktree-of "work"))))))

(ert-deftest test-move-leaves-the-checkout-status-clean ()
  "GIVEN a checkout that ignores nothing
WHEN a branch is moved to a worktree inside it
THEN the checkout does not list the worktree directory as a new file."
  (shipit-sandbox-with-repo
    (shipit-worktree-move-branch "work")
    (should (string-empty-p (test-move--git "status" "--porcelain")))))

(ert-deftest test-move-waits-for-unsaved-edits ()
  "GIVEN a buffer of the checkout with edits not saved
WHEN the branch is moved
THEN that is refused, and nothing moves: the edits would be left behind."
  (shipit-sandbox-with-repo
    (test-move--visiting buffer "base.txt"
      (with-current-buffer buffer (insert "unsaved"))
      (should-error (shipit-worktree-move-branch "work") :type 'user-error)
      (should (equal "work" (test-move--git "rev-parse" "--abbrev-ref" "HEAD")))
      (should-not (shipit-worktree-move-worktree-of "work")))))

(ert-deftest test-move-opens-the-files-of-the-work-in-the-worktree ()
  "GIVEN a buffer on a file the branch changed, and one on a file it did not
WHEN the branch is moved
THEN the first file is visited in the worktree instead, at the same
place, and the other stays in the checkout."
  (shipit-sandbox-with-repo
    (let ((changed (expand-file-name "writer--emit-the-body.txt"))
          (unchanged (expand-file-name "base.txt")))
      (test-move--visiting buffer changed
        (with-current-buffer buffer (goto-char 4))
        (find-file-noselect unchanged)
        (let* ((dir (shipit-worktree-move-branch "work"))
               (moved (get-file-buffer (expand-file-name "writer--emit-the-body.txt" dir))))
          (should moved)
          (should (= 4 (with-current-buffer moved (point))))
          (should-not (buffer-live-p buffer))
          (should (get-file-buffer unchanged)))))))

;;; Spinning off from the default branch

(ert-deftest test-move-spins-off-work-begun-on-main ()
  "GIVEN `main' checked out, with two commits not pushed and an edit
WHEN its work is spun off to a worktree as `feat/x'
THEN `feat/x' has the two commits and the worktree the edit, and `main'
is back at its upstream with nothing changed in the checkout."
  (test-move--on-main
    (test-move--commit "a.txt" "a" "first")
    (test-move--commit "b.txt" "b" "second")
    (test-move--write "a.txt" "an edit")
    (let ((dir (shipit-worktree-move-spin-off "feat/x")))
      (should (equal "feat/x" (test-move--in dir "rev-parse" "--abbrev-ref" "HEAD")))
      (should (equal "2" (test-move--git "rev-list" "--count" "origin/main..feat/x")))
      (should (equal "an edit" (test-move--text (expand-file-name "a.txt" dir))))
      (should (equal (test-move--git "rev-parse" "origin/main") (test-move--git "rev-parse" "main")))
      (should (equal "main" (test-move--git "rev-parse" "--abbrev-ref" "HEAD")))
      (should (string-empty-p (test-move--git "status" "--porcelain"))))))

(ert-deftest test-move-spin-off-without-work-is-refused ()
  "GIVEN `main' checked out at its upstream, with nothing changed
WHEN its work is spun off
THEN that is refused: there is no work to move."
  (test-move--on-main
    (should-error (shipit-worktree-move-spin-off "feat/x") :type 'user-error)
    (should (string-empty-p (test-move--git "branch" "--list" "feat/x")))))

;;; The command

(ert-deftest test-move-command-names-the-spin-off-as-suggested ()
  "GIVEN `main' checked out, with a commit not pushed, and a suggester
of branch names
WHEN the work is moved with the command
THEN the suggester reads the commit message, and the work is spun off
to a branch named as it suggests."
  (test-move--on-main
    (test-move--commit "a.txt" "a" "Read the header once")
    (let* ((work nil)
           (shipit-branch-name-suggester
            (lambda (description callback)
              (setq work description)
              (funcall callback "Read The Header"))))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial &rest _) initial)))
        (shipit-worktree-move-work))
      (should (string-match-p "Read the header once" work))
      (should (shipit-worktree-move-worktree-of "read-the-header")))))

(ert-deftest test-move-command-moves-a-feature-branch ()
  "GIVEN the branch `work' checked out
WHEN the work is moved with the command
THEN the branch moves, and no name is asked for."
  (shipit-sandbox-with-repo
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (error "No name should be asked for"))))
      (shipit-worktree-move-work))
    (should (shipit-worktree-move-worktree-of "work"))))

(ert-deftest test-move-is-in-the-worktree-menu-of-magit ()
  "GIVEN magit's worktree menu
WHEN it is looked at
THEN it offers to move the work to a worktree."
  (require 'magit-worktree)
  (should (string-match-p "shipit-worktree-move-work"
                          (format "%S" (transient-get-suffix 'magit-worktree "s")))))

(provide 'test-shipit-worktree-move)
;;; test-shipit-worktree-move.el ends here
