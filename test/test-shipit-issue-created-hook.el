;;; test-shipit-issue-created-hook.el --- The hook of a new issue -*- lexical-binding: t; -*-

;;; Commentary:
;; `shipit-issue-created-hook' runs after shipit made an issue.  A
;; function of another package can fail there.  The issue is made by
;; then, so the steps after it must still happen.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'shipit-issue-github)
(require 'shipit-issue-create)
(require 'shipit-test-http)
(require 'test-shipit-worktree-offer)

(defun test-issue-created-hook--github ()
  "GitHub's answers: a new issue of o/r gets the number 9, and its page shows."
  (append (list (list "/repos/o/r/issues\\'" 201 test-page-views--issue))
          test-page-views--issue-answers
          test-page-views--repository-answers
          (test-worktree-offer--github)))

(defun test-issue-created-hook--press (key)
  "Press KEY at point, as the user does."
  (call-interactively (key-binding (kbd key) t)))

(defun test-issue-created-hook--create (title)
  "Run M-x shipit-create-issue here, give the issue TITLE and press C-c C-c.
The answer to the question whether to create it is yes."
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
            ((symbol-function 'read-string) (lambda (&rest _) title)))
    (call-interactively #'shipit-create-issue)
    (with-current-buffer (shipit-issue-create--buffer-name "o/r")
      (goto-char (text-property-not-all (point-min) (point-max)
                                        'shipit-issue-create-title nil))
      (test-issue-created-hook--press "RET")
      (test-issue-created-hook--press "C-c C-c"))
    (test-page-views--settle)))

(defmacro test-issue-created-hook--on-the-page (&rest body)
  "Run BODY on the page of pull request 8 of o/r, with GitHub taking a new issue.
Kill the buffers that BODY leaves."
  (declare (indent 0))
  `(test-worktree-offer--on-the-page-of-pr-8 #'ignore
     (let ((shipit-issue-backend 'github)
           (shipit-issue-backend-config nil)
           (shipit-issue-repo-backends nil)
           (shipit-issue-create--drafts (make-hash-table :test 'equal))
           (buffers-before (buffer-list)))
       (unwind-protect
           (shipit-test-http-answering (test-issue-created-hook--github)
             ,@body)
         (dolist (buffer (buffer-list))
           (unless (memq buffer buffers-before)
             (kill-buffer buffer)))))))

(defun test-issue-created-hook--break-a-cup (&rest _)
  "Fail, as a function of another package on the hook can."
  (error "The cup is broken"))

(defun test-issue-created-hook--page-of-issue-9 ()
  "The buffer that shows issue 9 of o/r, or nil."
  (seq-find (lambda (buffer) (string-match-p "o/r#9" (buffer-name buffer)))
            (buffer-list)))

(ert-deftest test-issue-created-hook-a-function-that-fails-stops-nothing ()
  "A function that fails on the hook of a new issue stops no step after it."
  ;; GIVEN the page of pull request 8 of o/r
  ;; AND two functions on the hook of a new issue: the first one fails,
  ;;   and the second one notes what it gets
  (let* ((got nil)
         (shipit-issue-created-hook
          (list #'test-issue-created-hook--break-a-cup
                (lambda (number repo) (setq got (list number repo))))))
    (test-issue-created-hook--on-the-page
      ;; WHEN I create an issue there with C-c C-c, and GitHub makes issue 9
      (test-issue-created-hook--create "Order more cups")
      ;; THEN the page of issue 9 opens, and the buffer of the new issue
      ;;   is gone, so a second C-c C-c cannot make the issue twice
      (should (test-issue-created-hook--page-of-issue-9))
      (should-not (get-buffer (shipit-issue-create--buffer-name "o/r")))
      ;; AND the second function got the number and the repository
      (should (equal '(9 "o/r") got))
      ;; AND the echo area names the function that failed and its error,
      ;;   and does not say that the creation failed
      (should (test-worktree-offer--echo-area-said-p
               "test-issue-created-hook--break-a-cup failed: The cup is broken"))
      (should-not (test-worktree-offer--echo-area-said-p "Issue creation failed")))))

(ert-deftest test-issue-created-hook-a-function-gets-the-number-and-the-repository ()
  "A function on the hook of a new issue gets its number and its repository."
  ;; GIVEN the page of pull request 8 of o/r
  ;; AND a function on the hook of a new issue that notes what it gets
  (let* ((got nil)
         (shipit-issue-created-hook
          (list (lambda (number repo) (setq got (list number repo))))))
    (test-issue-created-hook--on-the-page
      ;; WHEN I create an issue there with C-c C-c, and GitHub makes issue 9
      (test-issue-created-hook--create "Order more cups")
      ;; THEN the function got 9 and o/r, and the page of issue 9 opens
      (should (equal '(9 "o/r") got))
      (should (test-issue-created-hook--page-of-issue-9)))))

(provide 'test-shipit-issue-created-hook)
;;; test-shipit-issue-created-hook.el ends here
