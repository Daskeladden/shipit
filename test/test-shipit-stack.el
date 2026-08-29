;;; test-shipit-stack.el --- Tests for local stack detection -*- lexical-binding: t; -*-

;;; Commentary:
;; Detecting a stack of branches from local git topology alone, for the
;; case where the branches were prepared outside shipit and nothing has
;; been pushed yet.
;;
;; These build a real repository rather than mocking git.  The whole
;; point of the code under test is what git reports about ancestry, so
;; a mock would only assert that the mock was written to match.

;;; Code:

(require 'ert)
(require 'shipit-stack)

;;;; Harness

(defun test-stack--git (&rest args)
  "Run git with ARGS in `default-directory', erroring on failure."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil args)))
      (unless (zerop exit)
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun test-stack--commit (message)
  "Add one file and commit it with MESSAGE."
  (let ((file (format "%s.txt" (replace-regexp-in-string "[^a-z0-9]" "-" message))))
    (with-temp-file (expand-file-name file default-directory) (insert message))
    (test-stack--git "add" file)
    (test-stack--git "commit" "-q" "-m" message)))

(defmacro test-stack--with-repo (&rest body)
  "Run BODY in a fresh git repository whose default branch is main."
  (declare (indent 0))
  `(let ((dir (make-temp-file "shipit-stack-test-" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory dir)))
           (test-stack--git "-c" "init.defaultBranch=main" "init" "-q")
           (test-stack--git "config" "user.email" "test@example.invalid")
           (test-stack--git "config" "user.name" "Test")
           (test-stack--commit "base")
           ,@body)
       (delete-directory dir t))))

;;;; No stack

(ert-deftest test-shipit-stack-branch-off-the-default-is-not-a-stack ()
  "GIVEN a branch cut straight from the default branch
WHEN the topology is inspected
THEN no stack is reported and there is no parent to base a pull
request on, so an ordinary branch keeps behaving as it always did."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "solo")
    (test-stack--commit "work")
    (should-not (shipit-stack-detect))
    (should-not (shipit-stack-parent-branch))))

(ert-deftest test-shipit-stack-merged-branches-are-not-parents ()
  "GIVEN an old branch already merged into the default branch
WHEN the topology is inspected
THEN it is not offered as a parent, because being an ancestor of the
current branch only means it reached the default branch first."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "old-work")
    (test-stack--commit "old")
    (test-stack--git "checkout" "-q" "main")
    (test-stack--git "merge" "-q" "--no-ff" "-m" "merge old" "old-work")
    (test-stack--git "checkout" "-q" "-b" "solo")
    (test-stack--commit "work")
    (should-not (shipit-stack-parent-branch))))

;;;; A chain

(ert-deftest test-shipit-stack-reports-the-chain-bottom-first ()
  "GIVEN three branches each cut from the one before
WHEN the topology is inspected
THEN the layers come back bottom-first with their depth from the
default branch, and the branch being worked on is marked."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two-a")
    (test-stack--commit "two-b")
    (test-stack--git "checkout" "-q" "-b" "layer-three")
    (test-stack--commit "three")
    (let* ((stack (shipit-stack-detect))
           (layers (cdr (assq 'layers stack))))
      (should (equal "main" (cdr (assq 'base stack))))
      (should (equal '("layer-one" "layer-two" "layer-three")
                     (mapcar (lambda (l) (cdr (assq 'branch l))) layers)))
      (should (equal '(1 3 4)
                     (mapcar (lambda (l) (cdr (assq 'depth l))) layers)))
      (should (equal '(nil nil t)
                     (mapcar (lambda (l) (cdr (assq 'current l))) layers))))))

(ert-deftest test-shipit-stack-parent-is-the-layer-directly-below ()
  "GIVEN a chain of three branches
WHEN the parent of the top one is asked for
THEN it is the layer directly below, which is the branch a pull
request for the top one must target."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two")
    (should (equal "layer-one" (shipit-stack-parent-branch)))))

;;;; Edge cases seen in a real prepared stack

(ert-deftest test-shipit-stack-ignores-branches-on-the-current-commit ()
  "GIVEN a second branch name pointing at the commit HEAD is on
WHEN the parent is asked for
THEN that alias is skipped and the layer below is chosen, because
basing a pull request on its own head is not a stack."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two")
    (test-stack--git "branch" "layer-two-alias")
    (should (equal "layer-one" (shipit-stack-parent-branch)))))

(ert-deftest test-shipit-stack-collapses-two-names-for-one-layer ()
  "GIVEN two branch names on the same commit partway down the stack
WHEN the topology is inspected
THEN they are one layer carrying both names, not two layers, so the
depth of the stack is not overstated."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "branch" "layer-one-also")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two")
    (let ((layers (cdr (assq 'layers (shipit-stack-detect)))))
      (should (equal 2 (length layers)))
      (should (equal '("layer-one-also")
                     (cdr (assq 'aliases (car layers))))))))

(ert-deftest test-shipit-stack-refuses-a-fork ()
  "GIVEN two branches that both descend from a lower one but not from
each other
WHEN the topology is inspected
THEN no stack is reported, because a fork has no single order to
merge in and guessing one would target the wrong branch."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "checkout" "-q" "-b" "sibling")
    (test-stack--commit "sibling work")
    (test-stack--git "checkout" "-q" "layer-one")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two")
    ;; `sibling' is not an ancestor of HEAD, so it is not a candidate at
    ;; all; the chain below HEAD stays linear and is still a stack.
    (should (shipit-stack-detect))
    (should (equal "layer-one" (shipit-stack-parent-branch)))))

;;;; Backup branches left beside a layer

(ert-deftest test-shipit-stack-checked-out-name-wins-over-an-alias ()
  "GIVEN a backup branch sharing the commit of the branch checked out,
whose name sorts first
WHEN the layers are built
THEN the checked-out name is the layer and the backup is an alias, and
the layer is not reported ambiguous, because being checked out settles
which name is the real one."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two")
    (test-stack--git "branch" "aaa-backup-of-layer-two")
    (let* ((layers (cdr (assq 'layers (shipit-stack-detect))))
           (top (car (last layers))))
      (should (equal "layer-two" (cdr (assq 'branch top))))
      (should (equal '("aaa-backup-of-layer-two") (cdr (assq 'aliases top))))
      (should-not (cdr (assq 'ambiguous top))))))

(ert-deftest test-shipit-stack-marks-an-undecidable-layer-ambiguous ()
  "GIVEN two names on one commit partway down the stack, neither of
them checked out
WHEN the layers are built
THEN the layer is marked ambiguous so the caller can ask which name is
the layer.  Nothing local distinguishes a backup from the branch it
was copied from, and guessing targets the wrong branch."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "branch" "layer-one-backup")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two")
    (let ((bottom (car (cdr (assq 'layers (shipit-stack-detect))))))
      (should (cdr (assq 'ambiguous bottom)))
      (should (equal '("layer-one" "layer-one-backup")
                     (sort (cons (cdr (assq 'branch bottom))
                                 (copy-sequence (cdr (assq 'aliases bottom))))
                           #'string<))))))

;;;; Per-layer size

(ert-deftest test-shipit-stack-reports-what-each-layer-adds ()
  "GIVEN layers of one, two and one commits
WHEN the stack is detected
THEN each layer reports the commits it adds over the one below, not
its distance from the default branch, because what a reviewer is
handed is the layer, not everything under it."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "layer-one")
    (test-stack--commit "one")
    (test-stack--git "checkout" "-q" "-b" "layer-two")
    (test-stack--commit "two-a")
    (test-stack--commit "two-b")
    (test-stack--git "checkout" "-q" "-b" "layer-three")
    (test-stack--commit "three")
    (let ((layers (cdr (assq 'layers (shipit-stack-detect)))))
      (should (equal '(1 2 1)
                     (mapcar (lambda (l) (cdr (assq 'commits l))) layers))))))

(provide 'test-shipit-stack)
;;; test-shipit-stack.el ends here
