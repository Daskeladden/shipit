;;; test-shipit-preview.el --- Tests for shipit-preview.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for shipit-preview.el covering PR preview functionality.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load test stubs if not already loaded
(unless (fboundp 'shipit--api-request)
  (load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name)))))

(require 'shipit-preview)
(require 'shipit-pr-backends)

;;; Auto-fill description tests

(ert-deftest test-shipit-preview--auto-fill-description-single-commit ()
  "GIVEN a preview with exactly 1 commit and empty description
WHEN the preview is initialized
THEN the description should be auto-filled with the commit message."
  (let ((commits (list (list :sha "abc123"
                             :message "Add user authentication\n\nThis implements OAuth2 login."
                             :first-line "Add user authentication"))))
    ;; WHEN we call the auto-fill function with empty description
    (let ((result (shipit-preview--maybe-auto-fill-description nil commits)))
      ;; THEN the full commit message is returned
      (should (string= result "Add user authentication\n\nThis implements OAuth2 login.")))))

(ert-deftest test-shipit-preview--auto-fill-description-no-commits ()
  "GIVEN a preview with no commits
WHEN the preview is initialized
THEN the description should remain nil."
  (let ((commits nil))
    ;; WHEN we call the auto-fill function
    (let ((result (shipit-preview--maybe-auto-fill-description nil commits)))
      ;; THEN nil is returned (no change)
      (should (null result)))))

(ert-deftest test-shipit-preview--auto-fill-description-multiple-commits ()
  "GIVEN a preview with multiple commits
WHEN the preview is initialized
THEN the description should remain nil (not auto-filled)."
  (let ((commits (list (list :sha "abc123" :message "First commit")
                       (list :sha "def456" :message "Second commit"))))
    ;; WHEN we call the auto-fill function with empty description
    (let ((result (shipit-preview--maybe-auto-fill-description nil commits)))
      ;; THEN nil is returned (no auto-fill for multiple commits)
      (should (null result)))))

(ert-deftest test-shipit-preview--auto-fill-description-already-set ()
  "GIVEN a preview with 1 commit but description already set
WHEN the preview is initialized
THEN the existing description should be preserved."
  (let ((commits (list (list :sha "abc123" :message "Commit message")))
        (existing-desc "User wrote this description"))
    ;; WHEN we call the auto-fill function with existing description
    (let ((result (shipit-preview--maybe-auto-fill-description existing-desc commits)))
      ;; THEN the existing description is returned unchanged
      (should (string= result "User wrote this description")))))

(ert-deftest test-shipit-preview--auto-fill-description-empty-string ()
  "GIVEN a preview with 1 commit and empty string description
WHEN the preview is initialized
THEN the description should be auto-filled."
  (let ((commits (list (list :sha "abc123" :message "Commit message"))))
    ;; WHEN we call the auto-fill function with empty string
    (let ((result (shipit-preview--maybe-auto-fill-description "" commits)))
      ;; THEN the commit message is returned
      (should (string= result "Commit message")))))

(ert-deftest test-shipit-preview--auto-fill-after-restore-with-empty-description ()
  "GIVEN state was restored but description was empty
WHEN auto-fill runs after restore
THEN the description should be filled from commit message.
This tests the scenario where user previously opened preview, quit,
and reopened - the restored empty description should still auto-fill."
  (let ((commits (list (list :sha "abc123" :message "Commit message from git")))
        ;; Simulate restored state with nil description
        (restored-description nil))
    ;; WHEN we call auto-fill (as happens after restore)
    (let ((result (shipit-preview--maybe-auto-fill-description restored-description commits)))
      ;; THEN the commit message is used
      (should (string= result "Commit message from git")))))

;;; Draft forwarding tests

(defun test-shipit-preview--create-pr-capturing (draft-value)
  "Drive `shipit-preview--create-pr' with DRAFT-VALUE and return backend args.
Mocks every side-effecting boundary so only the argument wiring is
exercised, then returns the list of arguments the backend's
`:create-pr' function received."
  (let ((captured-args nil)
        (shipit-preview--repo "owner/repo")
        (shipit-preview--branch "feature")
        (shipit-preview--base-ref "origin/main")
        (shipit-preview--title "My title")
        (shipit-preview--description "Body")
        (shipit-preview--draft draft-value)
        (shipit-preview--labels nil)
        (shipit-preview--reviewers nil)
        (shipit-preview--assignees nil)
        (shipit-preview--comments nil)
        (shipit-preview--general-comments nil))
    (cl-letf (((symbol-function 'shipit-preview--check-existing-pr)
               (lambda () nil))
              ((symbol-function 'shipit-preview--ensure-branch-pushed)
               (lambda () t))
              ((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :create-pr
                             (lambda (&rest args)
                               (setq captured-args args)
                               '((number . 7) (html_url . "https://example/7"))))
                       '(:repo "owner/repo"))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'shipit-preview--clear-saved-state)
               (lambda () nil)))
      (shipit-preview--create-pr)
      captured-args)))

(ert-deftest test-shipit-preview--create-pr-forwards-draft-true ()
  "GIVEN a preview buffer with draft enabled
WHEN the PR is created
THEN the backend :create-pr receives the draft flag as its 6th argument."
  ;; WHEN/THEN
  (should (equal (test-shipit-preview--create-pr-capturing t)
                 (list '(:repo "owner/repo") "My title" "Body" "main" "feature" t))))

(ert-deftest test-shipit-preview--create-pr-forwards-draft-nil ()
  "GIVEN a preview buffer with draft disabled
WHEN the PR is created
THEN the backend :create-pr receives nil as its draft argument."
  ;; WHEN/THEN
  (should (equal (test-shipit-preview--create-pr-capturing nil)
                 (list '(:repo "owner/repo") "My title" "Body" "main" "feature" nil))))

;;; Branch push tests

(ert-deftest test-shipit-preview--push-branch-succeeds-on-zero-exit ()
  "GIVEN git push succeeds (exit 0) but its output happens to contain
the words \"error:\"/\"fatal:\" (e.g. from a pre-push hook or a remote
server message)
WHEN shipit-preview--push-branch runs
THEN it returns t -- success is decided by the process exit code, not by
grepping the output."
  ;; GIVEN both the process boundaries are stubbed: a stray \"error:\" in
  ;; the output must NOT be treated as a failure when git exits 0.
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (&rest _)
               "remote: hook: 0 error: blockers\n * [new branch] feature -> feature\n"))
            ((symbol-function 'call-process)
             (lambda (_program &optional _infile _dest _display &rest _args) 0)))
    ;; WHEN / THEN
    (should (shipit-preview--push-branch "feature"))))

(ert-deftest test-shipit-preview--push-branch-fails-on-nonzero-exit ()
  "GIVEN git push fails (non-zero exit)
WHEN shipit-preview--push-branch runs
THEN it returns nil."
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (&rest _) "fatal: repository not found\n"))
            ((symbol-function 'call-process)
             (lambda (_program &optional _infile _dest _display &rest _args) 128)))
    ;; WHEN / THEN
    (should-not (shipit-preview--push-branch "feature"))))

(provide 'test-shipit-preview)
;;; test-shipit-preview.el ends here
