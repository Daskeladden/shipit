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

;;;; Splitting a branch into layers

(ert-deftest test-shipit-stack-split-groups-commits-by-subject-prefix ()
  "GIVEN commits whose subjects carry `area: ' prefixes
WHEN a split is proposed
THEN each run of one prefix becomes a layer, in order, because that is
how the work was already grouped by whoever wrote the messages."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "work")
    (dolist (s '("fpga: conventions" "fpga: point tests"
                 "uoe: abort path" "uoe: tx mux" "uoe: reset"))
      (test-stack--commit s))
    (let ((layers (cdr (assq 'layers (shipit-stack-split-proposal)))))
      (should (equal '("fpga" "uoe")
                     (mapcar (lambda (l) (cdr (assq 'prefix l))) layers)))
      (should (equal '(2 3)
                     (mapcar (lambda (l) (length (cdr (assq 'commits l)))) layers))))))

(ert-deftest test-shipit-stack-split-names-layers-from-their-prefix ()
  "GIVEN a proposed split
WHEN the layers are named
THEN each name is derived from its prefix and is usable as a branch
name, since it becomes the head of a pull request."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "work")
    (test-stack--commit "FPGA Core: conventions")
    (test-stack--commit "uoe: abort path")
    (test-stack--commit "zhit: formal")
    (let ((names (mapcar (lambda (l) (cdr (assq 'branch l)))
                         (cdr (assq 'layers (shipit-stack-split-proposal))))))
      ;; The top layer keeps the branch already checked out; the work is
      ;; there and it needs no new pointer.
      (should (equal '("work-fpga-core" "work-uoe" "work") names)))))

(ert-deftest test-shipit-stack-split-declines-when-nothing-groups ()
  "GIVEN commits with no prefixes to group by
WHEN a split is proposed
THEN there is nothing to propose.  One layer is not a stack, and
guessing boundaries from unprefixed subjects would be inventing them."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "work")
    (test-stack--commit "first thing")
    (test-stack--commit "second thing")
    (should-not (shipit-stack-split-proposal))))

(ert-deftest test-shipit-stack-split-refuses-a-range-with-a-merge ()
  "GIVEN a merge commit in the range
WHEN a split is proposed
THEN there is none.  Layers are branch pointers on a line of commits,
and a merge means there is no single line to put them on."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "side")
    (test-stack--commit "side: work")
    (test-stack--git "checkout" "-q" "main")
    (test-stack--commit "main: work")
    (test-stack--git "checkout" "-q" "-b" "work")
    (test-stack--git "merge" "-q" "--no-ff" "-m" "merge side" "side")
    (should-not (shipit-stack-split-proposal))))

(ert-deftest test-shipit-stack-split-creates-a-branch-at-each-boundary ()
  "GIVEN a proposed split
WHEN it is applied
THEN a branch points at the last commit of each layer below the top,
the checked-out branch is untouched and still the top, and no commit
has moved: splitting is naming, not rewriting."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "work")
    (dolist (s '("fpga: one" "fpga: two" "uoe: three"))
      (test-stack--commit s))
    (let* ((head-before (test-stack--git "rev-parse" "HEAD"))
           (proposal (shipit-stack-split-proposal)))
      (shipit-stack-split-apply proposal)
      (should (equal head-before (test-stack--git "rev-parse" "HEAD")))
      (should (equal "work" (test-stack--git "rev-parse" "--abbrev-ref" "HEAD")))
      ;; the lower layer now has a branch of its own, at its last commit
      (should (equal (test-stack--git "rev-parse" "HEAD~1")
                     (test-stack--git "rev-parse" "work-fpga"))))))

(ert-deftest test-shipit-stack-split-is-detected-as-a-stack-afterwards ()
  "GIVEN a branch that has been split
WHEN the topology is read back
THEN it is a stack, which is the whole point: the split feeds the same
detection everything else already uses."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "work")
    (dolist (s '("fpga: one" "uoe: two" "zhit: three"))
      (test-stack--commit s))
    (shipit-stack-split-apply (shipit-stack-split-proposal))
    (let ((stack (shipit-stack-detect)))
      (should stack)
      (should (equal 3 (cdr (assq 'size stack))))
      (should (equal '("work-fpga" "work-uoe" "work")
                     (mapcar (lambda (l) (cdr (assq 'branch l)))
                             (cdr (assq 'layers stack))))))))

(ert-deftest test-shipit-stack-split-applied-twice-changes-nothing ()
  "GIVEN a split that has already been applied
WHEN it is applied again
THEN the branches are where they were.  Every step of this feature is
meant to be safe to repeat, so that a failure part way can be resumed
rather than unwound."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "work")
    (dolist (s '("fpga: one" "uoe: two")) (test-stack--commit s))
    (let ((proposal (shipit-stack-split-proposal)))
      (shipit-stack-split-apply proposal)
      (let ((first (test-stack--git "rev-parse" "work-fpga")))
        (shipit-stack-split-apply proposal)
        (should (equal first (test-stack--git "rev-parse" "work-fpga")))))))

;;;; Whether branches form a stack at all

(ert-deftest test-shipit-stack-a-line-of-branches-has-nothing-wrong ()
  "GIVEN each branch cut from the one below it
WHEN they are checked as a stack
THEN nothing is reported, so submitting may go ahead."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "one")
    (test-stack--commit "first")
    (test-stack--git "checkout" "-q" "-b" "two")
    (test-stack--commit "second")
    (should (equal '() (shipit-stack-problems '("one" "two") "main")))))

(ert-deftest test-shipit-stack-a-branch-off-the-line-is-reported ()
  "GIVEN a second branch cut from the default branch rather than from
the layer below it
WHEN they are checked as a stack
THEN the break is named.  Its pull request would carry the commits of
the layer below as well as its own."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "one")
    (test-stack--commit "first")
    (test-stack--git "checkout" "-q" "main")
    (test-stack--git "checkout" "-q" "-b" "two")
    (test-stack--commit "second")
    (should (equal '("two is not built on one")
                   (shipit-stack-problems '("one" "two") "main")))))

(ert-deftest test-shipit-stack-a-layer-adding-nothing-is-reported ()
  "GIVEN two branches on the same commit
WHEN they are checked as a stack
THEN it is reported.  A pull request between them has no diff, and the
remote refuses to open one."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "one")
    (test-stack--commit "first")
    (test-stack--git "branch" "two" "one")
    (should (equal '("two adds nothing to one")
                   (shipit-stack-problems '("one" "two") "main")))))

(ert-deftest test-shipit-stack-a-branch-that-is-not-there-stops-the-check ()
  "GIVEN a layer whose branch has not been made
WHEN the stack is checked
THEN that is all that is reported: git cannot say what a ref that does
not exist is built on, so the later answers would be noise."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "one")
    (test-stack--commit "first")
    (should (equal '("ghost does not exist")
                   (shipit-stack-problems '("one" "ghost") "main")))))

(ert-deftest test-shipit-stack-one-name-on-two-layers-is-reported ()
  "GIVEN the same branch named as two layers
WHEN the stack is checked
THEN it is reported rather than compared with itself."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "one")
    (test-stack--commit "first")
    (should (equal '("one names two layers")
                   (shipit-stack-problems '("one" "one") "main")))))

(ert-deftest test-shipit-stack-a-rebased-lower-layer-breaks-the-line ()
  "GIVEN a stack whose bottom layer is rewritten, leaving the layer
above it on the commits it replaced
WHEN the stack is checked
THEN the break is named.  This is the ordinary way a stack falls out of
line: a lower layer is amended and the ones above are not moved onto
it."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "one")
    (test-stack--commit "first")
    (test-stack--git "checkout" "-q" "-b" "two")
    (test-stack--commit "second")
    (test-stack--git "checkout" "-q" "one")
    (test-stack--git "commit" "-q" "--amend" "-m" "first, again")
    (should (equal '("two is not built on one")
                   (shipit-stack-problems '("one" "two") "main")))))

(ert-deftest test-shipit-stack-a-base-that-has-moved-on-is-not-a-fault ()
  "GIVEN a stack cut from the base before the base gained more commits
WHEN the stack is checked
THEN nothing is reported.  A pull request is compared from where its
two sides last met, so the branch under a stack may move on without the
stack being rebuilt on it, and usually has."
  (test-stack--with-repo
    (test-stack--git "checkout" "-q" "-b" "one")
    (test-stack--commit "first")
    (test-stack--git "checkout" "-q" "-b" "two")
    (test-stack--commit "second")
    (test-stack--git "checkout" "-q" "main")
    (test-stack--commit "someone else's work")
    (should (equal '() (shipit-stack-problems '("one" "two") "main")))))

(provide 'test-shipit-stack)
;;; test-shipit-stack.el ends here
