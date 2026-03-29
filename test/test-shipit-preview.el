;;; test-shipit-preview.el --- Tests for shipit-preview.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for shipit-preview.el covering PR preview functionality.

;;; Code:

(require 'ert)

;; Load test stubs if not already loaded
(unless (fboundp 'shipit--api-request)
  (load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name)))))

(require 'shipit-preview)

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

(provide 'test-shipit-preview)
;;; test-shipit-preview.el ends here
