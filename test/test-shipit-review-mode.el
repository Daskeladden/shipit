;;; test-shipit-review-mode.el --- Tests for shipit review mode -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'shipit-review-mode)

(ert-deftest shipit-review-mode-activation-test ()
  "Test that shipit-review-mode can be activated on a buffer."
  (with-temp-buffer
    (shipit-review-mode 1)
    (should (bound-and-true-p shipit-review-mode))))

(ert-deftest shipit-review-mode-deactivation-test ()
  "Test that shipit-review-mode can be deactivated."
  (with-temp-buffer
    (shipit-review-mode 1)
    (shipit-review-mode -1)
    (should-not (bound-and-true-p shipit-review-mode))))

(provide 'test-shipit-review-mode)
;;; test-shipit-review-mode.el ends here
