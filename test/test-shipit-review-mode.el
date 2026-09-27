;;; test-shipit-review-mode.el --- Tests for shipit review mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Turning review mode on and off.
;;
;; These check the buffer they name rather than whatever buffer is
;; current afterwards.  Enabling the mode runs a setup that displays
;; the actions legend, and that leaves a different buffer current, so
;; reading the plain variable after the call reports on the wrong
;; buffer and the mode looks as though it never came on.

;;; Code:

(require 'ert)
(require 'shipit)
(require 'cl-lib)
(require 'shipit-review-mode)

(defmacro test-review-mode--with-buffer (var &rest body)
  "Bind VAR to a fresh named buffer, run BODY, then kill it.
A buffer named for the test, rather than a temporary one, because
review mode is meant for a file being reviewed."
  (declare (indent 1))
  `(let ((,var (get-buffer-create "*test-shipit-review-mode*")))
     (unwind-protect (progn ,@body)
       (kill-buffer ,var))))

(ert-deftest shipit-review-mode-activation-test ()
  "GIVEN a buffer
WHEN review mode is switched on in it
THEN the mode is on in that buffer."
  (test-review-mode--with-buffer buffer
    (with-current-buffer buffer (shipit-review-mode 1))
    (should (buffer-local-value 'shipit-review-mode buffer))))

(ert-deftest shipit-review-mode-deactivation-test ()
  "GIVEN a buffer with review mode on
WHEN it is switched off
THEN the mode is off in that buffer."
  (test-review-mode--with-buffer buffer
    (with-current-buffer buffer (shipit-review-mode 1))
    (with-current-buffer buffer (shipit-review-mode -1))
    (should-not (buffer-local-value 'shipit-review-mode buffer))))

(ert-deftest shipit-review-mode-activation-leaves-the-callers-buffer-current ()
  "GIVEN a buffer
WHEN review mode is switched on in it
THEN that buffer is still the current one afterwards.

Entering the mode pops the actions legend and selects its window,
which is deliberate: the legend is a control panel and takes focus the
way ediff\='s does.  Selecting a window also changes the current
buffer, though, and that happens from a mode hook, so anything that
switches the mode on and carries on working would carry on in the
legend buffer."
  (test-review-mode--with-buffer buffer
    (with-current-buffer buffer
      (shipit-review-mode 1)
      (should (eq buffer (current-buffer))))))

(ert-deftest test-shipit-review-legend-shows-a-section-added-to-it ()
  "GIVEN a section another package adds to the legend
WHEN the full legend is built
THEN the section is in it."
  (let ((shipit-review-actions-legend-extra-sections '(("Extra" ("x" "do the extra")))))
    (should (string-match-p "do the extra" (shipit-review--actions-legend-full)))))

(provide 'test-shipit-review-mode)
;;; test-shipit-review-mode.el ends here
