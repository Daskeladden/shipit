;;; test-shipit-stack-plan.el --- What a submit would do -*- lexical-binding: t; -*-

;;; Commentary:
;; Publishing a stack is a list of steps computed before anything runs.
;; The dry run prints that list and the submit executes it, so both see
;; the same plan; a dry run derived separately would drift from what
;; actually happens and still be believed.
;;
;; The steps are data, so this asks nothing of the network.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-stack-plan)

(defconst test-plan--stack
  '((base . "master") (size . 3) (position . 3)
    (layers . (((branch . "l1") (tip . "aaa") (commits . 2) (current . nil))
               ((branch . "l2") (tip . "bbb") (commits . 3) (current . nil))
               ((branch . "l3") (tip . "ccc") (commits . 1) (current . t)))))
  "Three layers over master, bottom first.")

(defmacro test-plan--with (pushed prs &rest body)
  "Run BODY with PUSHED branches and PRS, an alist of branch to number."
  (declare (indent 2))
  ;; The three facts the plan needs from outside: whether a branch is on
  ;; the remote, whether a pull request has it as head, and whether
  ;; those pull requests are already a stack.  Tests that care about the
  ;; third rebind it again.
  `(cl-letf (((symbol-function 'shipit-stack-plan--pushed-p)
              (lambda (branch) (member branch ,pushed)))
             ((symbol-function 'shipit-stack-plan--pr-for-branch)
              (lambda (_repo branch) (cdr (assoc branch ,prs))))
             ((symbol-function 'shipit-stack-plan--existing-stack)
              (lambda (&rest _) nil)))
     ,@body))

;;;; A stack that has never been published

(ert-deftest test-shipit-stack-plan-publishes-every-layer-bottom-first ()
  "GIVEN a stack where nothing is pushed and no pull request exists
WHEN the plan is computed
THEN every layer is pushed and opened, bottom first, each based on the
layer below, because GitHub rejects a base ref that is not yet on the
remote."
  (test-plan--with '() '()
    (let ((steps (cdr (assq 'steps (shipit-stack-plan test-plan--stack "o/r")))))
      (should (equal '("l1" "l2" "l3")
                     (mapcar (lambda (s) (cdr (assq 'branch s))) steps)))
      (should (equal '("master" "l1" "l2")
                     (mapcar (lambda (s) (cdr (assq 'base s))) steps)))
      (should (seq-every-p (lambda (s) (eq 'needed (cdr (assq 'push s)))) steps))
      (should (seq-every-p (lambda (s) (eq 'none (cdr (assq 'pr s)))) steps)))))

(ert-deftest test-shipit-stack-plan-registers-the-stack-at-the-end ()
  "GIVEN pull requests that do not exist yet
WHEN the plan is computed
THEN it ends by creating the stack.  Chained bases alone are not a
stack: GitHub validates the chain but will not infer one, so without
this step the web UI shows three unrelated pull requests."
  (test-plan--with '() '()
    (let ((reg (cdr (assq 'registration (shipit-stack-plan test-plan--stack "o/r")))))
      (should (eq 'create (cdr (assq 'action reg))))
      (should (equal 3 (cdr (assq 'count reg)))))))

;;;; Adding one layer to a stack already published

(ert-deftest test-shipit-stack-plan-adds-only-what-is-new ()
  "GIVEN a stack whose lower layers are already pushed and open
WHEN the plan is computed
THEN the published layers are left alone and only the new one is
pushed and opened.  This is the ordinary case once a stack exists, and
it is the same code path as the first publish."
  (test-plan--with '("l1" "l2") '(("l1" . 401) ("l2" . 402))
    (let ((steps (cdr (assq 'steps (shipit-stack-plan test-plan--stack "o/r")))))
      (should (equal '(done done needed)
                     (mapcar (lambda (s) (cdr (assq 'push s))) steps)))
      (should (equal '(401 402 none)
                     (mapcar (lambda (s) (cdr (assq 'pr s))) steps))))))

(ert-deftest test-shipit-stack-plan-appends-to-an-existing-stack ()
  "GIVEN lower layers that are already a stack on GitHub
WHEN one layer is new
THEN the plan appends to that stack rather than creating another."
  (test-plan--with '("l1" "l2") '(("l1" . 401) ("l2" . 402))
    (cl-letf (((symbol-function 'shipit-stack-plan--existing-stack)
               (lambda (&rest _) 7)))
      (let ((reg (cdr (assq 'registration (shipit-stack-plan test-plan--stack "o/r")))))
        (should (eq 'add (cdr (assq 'action reg))))
        (should (equal 7 (cdr (assq 'stack reg))))
        (should (equal 1 (cdr (assq 'count reg))))))))

(ert-deftest test-shipit-stack-plan-has-nothing-to-do-when-all-published ()
  "GIVEN a stack that is entirely published and registered
WHEN the plan is computed
THEN there is nothing left, which is what makes re-running a submit
after a failure safe: the work already done drops out of the plan."
  (test-plan--with '("l1" "l2" "l3") '(("l1" . 401) ("l2" . 402) ("l3" . 403))
    (cl-letf (((symbol-function 'shipit-stack-plan--existing-stack)
               (lambda (&rest _) 7)))
      (let ((plan (shipit-stack-plan test-plan--stack "o/r")))
        (should-not (shipit-stack-plan-work-p plan))
        (should (eq 'none (cdr (assq 'action (cdr (assq 'registration plan))))))))))

;;;; Publishing only part of a stack

(ert-deftest test-shipit-stack-plan-cut-stops-at-a-layer ()
  "GIVEN a cut at layer 2
WHEN the plan is computed
THEN it covers the bottom two layers only.  A cut is a stopping point
rather than a selection: a layer's base is the layer below, so the
ones above it cannot be published without it."
  (test-plan--with '() '()
    (let ((plan (shipit-stack-plan test-plan--stack "o/r" 2)))
      (should (equal '("l1" "l2")
                     (mapcar (lambda (s) (cdr (assq 'branch s)))
                             (cdr (assq 'steps plan)))))
      (should (equal 2 (cdr (assq 'count (cdr (assq 'registration plan)))))))))

;;;; A layer that cannot be published

(ert-deftest test-shipit-stack-plan-blocks-on-an-undecided-layer ()
  "GIVEN a layer with more than one branch name and none chosen
WHEN the plan is computed
THEN that layer is blocked and the layers above it wait on it, rather
than publishing against a branch that may be a backup nobody will
touch again."
  (let ((stack (copy-tree test-plan--stack)))
    (setf (cdr (assq 'layers stack))
          (list (nth 0 (cdr (assq 'layers stack)))
                (append '((ambiguous . t) (aliases . ("l2-backup")))
                        (nth 1 (cdr (assq 'layers stack))))
                (nth 2 (cdr (assq 'layers stack)))))
    (test-plan--with '() '()
      (let ((steps (cdr (assq 'steps (shipit-stack-plan stack "o/r")))))
        (should-not (cdr (assq 'blocked (nth 0 steps))))
        (should (cdr (assq 'blocked (nth 1 steps))))
        (should (cdr (assq 'blocked (nth 2 steps))))))))

;;;; Reading the plan

(ert-deftest test-shipit-stack-plan-renders-what-it-would-do ()
  "GIVEN a plan
WHEN it is rendered for the dry run
THEN each branch, what would happen to it and the base it would target
are all in the text, and it says plainly that nothing has been done."
  (test-plan--with '("l1") '(("l1" . 401))
    (let ((text (shipit-stack-plan-render (shipit-stack-plan test-plan--stack "o/r"))))
      (dolist (needle '("l1" "l2" "l3" "401" "master" "create" "push"))
        (should (string-match-p (regexp-quote needle) text)))
      (should (string-match-p "Nothing has been" text)))))

(provide 'test-shipit-stack-plan)
;;; test-shipit-stack-plan.el ends here
