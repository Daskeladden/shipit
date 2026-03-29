;;; test-shipit-review-side-frame.el --- Tests for review side-frame -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'shipit-review-side-frame)

(ert-deftest shipit--create-review-side-frame-buffer-test ()
  "Test creating a side-frame buffer for comments."
  (let* ((file-path "src/test.js")
         (buffer (shipit--create-review-side-frame-buffer file-path)))
    (should (bufferp buffer))
    (should (string-match "shipit-review-comments" (buffer-name buffer)))
    (kill-buffer buffer)))

(ert-deftest shipit--side-frame-header-format-test ()
  "Test formatting header for side-frame."
  (let* ((commit-sha "abc1234def5678")
         (is-pr-commit t)
         (total-pr-commits 5)
         (current-pr-index 2)
         (header (shipit--format-side-frame-header commit-sha is-pr-commit current-pr-index total-pr-commits)))
    (should (string-match "abc1234" header))
    (should (string-match "PR commit" header))))

(provide 'test-shipit-review-side-frame)
;;; test-shipit-review-side-frame.el ends here
