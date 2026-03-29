;;; test-shipit-review-revision.el --- Tests for revision navigation -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'shipit-review-revision)

(ert-deftest shipit--build-pr-commit-list-test ()
  "Test building list of PR commits from PR data."
  (let* ((pr-data '((commits . [
                      ((oid . "abc1234") (message . "First commit"))
                      ((oid . "def5678") (message . "Second commit"))
                      ((oid . "ghi9012") (message . "Third commit"))])))
         (commits (shipit--build-pr-commit-list pr-data)))
    (should (= (length commits) 3))
    (should (equal (nth 0 commits) '(:sha "abc1234" :message "First commit" :pr-member t)))
    (should (equal (nth 2 commits) '(:sha "ghi9012" :message "Third commit" :pr-member t)))))

(ert-deftest shipit--is-pr-commit-test ()
  "Test identifying if a commit is part of the PR."
  (let ((pr-commits '((:sha "abc1234") (:sha "def5678")))
        (test-sha "abc1234")
        (non-pr-sha "xyz9999"))
    (should (shipit--is-pr-commit-p test-sha pr-commits))
    (should-not (shipit--is-pr-commit-p non-pr-sha pr-commits))))

(ert-deftest shipit--get-file-at-commit-test ()
  "Test retrieving file content at specific commit."
  ;; This test will be mocked in test environment
  (let ((default-directory "/tmp/test-repo"))
    (should-error (shipit--get-file-at-commit "nonexistent.txt" "abc1234")
                  :type 'user-error)))

(ert-deftest shipit--format-commit-indicator-test ()
  "Test formatting commit indicator for mode-line."
  (let ((test-sha "abc1234567890"))
    ;; Test PR commit
    (should (equal (shipit--format-commit-indicator test-sha t)
                   "[Commit abc1234 (PR)]"))
    ;; Test non-PR commit
    (should (equal (shipit--format-commit-indicator test-sha nil)
                   "[Commit abc1234]"))))

(provide 'test-shipit-review-revision)
;;; test-shipit-review-revision.el ends here
