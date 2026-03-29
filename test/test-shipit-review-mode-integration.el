;;; test-shipit-review-mode-integration.el --- Integration tests for review mode -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

;; Load external dependencies for integration testing
(require 'magit)

;; Load shipit modules
(require 'shipit-core)
(require 'shipit-pr-sections)

(ert-deftest shipit--should-use-review-mode-test ()
  "Test logic for deciding whether to use review mode."
  ;; Worktree exists and in-sync
  (let ((pr-number 42)
        (pr-repo "org/repo")
        (pr-head-sha "abc1234"))
    ;; Mock shipit--find-worktree-for-pr
    (cl-letf (((symbol-function 'shipit--find-worktree-for-pr)
               (lambda (&rest _) "/path/to/worktree"))
              ((symbol-function 'shipit--worktree-in-sync-p)
               (lambda (&rest _) t)))
      (should (shipit--should-use-review-mode-p pr-number pr-repo pr-head-sha))))

  ;; Worktree doesn't exist
  (cl-letf (((symbol-function 'shipit--find-worktree-for-pr)
             (lambda (&rest _) nil)))
    (should-not (shipit--should-use-review-mode-p 42 "org/repo" "abc1234"))))

(provide 'test-shipit-review-mode-integration)
;;; test-shipit-review-mode-integration.el ends here
