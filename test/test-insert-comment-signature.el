;;; test-insert-comment-signature.el --- Test shipit--insert-comment function signature  -*- lexical-binding: t; -*-

;;; Commentary:
;; This test verifies that shipit--insert-comment accepts the correct keyword arguments.
;; Previously, stale .elc files had the wrong signature causing wrong-number-of-arguments errors.

;;; Code:

(require 'ert)
(require 'shipit-pr-sections)

(ert-deftest test-insert-comment-signature-correct ()
  "Test that shipit--insert-comment accepts keyword arguments correctly."
  (let ((comment '((id . 12345)
                   (user . ((login . "testuser")))
                   (body . "test comment")
                   (created_at . "2025-11-19T12:00:00Z")
                   (path . "test.txt")
                   (line . 10))))
    ;; This should not raise wrong-number-of-arguments error
    (with-temp-buffer
      (should-not
       (condition-case err
           (progn
             ;; Call with keyword arguments as done in shipit--insert-file-comment
             (shipit--insert-comment comment
                                    :base-indent 8
                                    :repo "test/repo"
                                    :pr-number 1
                                    :include-header t
                                    :include-prefix nil
                                    :apply-faces nil
                                    :apply-blockquote-styling nil
                                    :inline-expanded t
                                    :resolved nil)
             nil)
         (wrong-number-of-arguments err err))))))

(ert-deftest test-insert-comment-renders-content ()
  "Test that shipit--insert-comment actually renders comment content."
  (let ((comment '((id . 12345)
                   (user . ((login . "testuser")))
                   (body . "test comment body")
                   (created_at . "2025-11-19T12:00:00Z")
                   (path . "test.txt")
                   (line . 10))))
    (with-temp-buffer
      (shipit--insert-comment comment
                             :base-indent 0
                             :repo "test/repo"
                             :pr-number 1
                             :include-header nil
                             :include-prefix nil
                             :apply-faces nil
                             :apply-blockquote-styling nil
                             :inline-expanded nil
                             :resolved nil)
      ;; Check that something was inserted
      (should (> (point-max) 1)))))

(provide 'test-insert-comment-signature)
;;; test-insert-comment-signature.el ends here
