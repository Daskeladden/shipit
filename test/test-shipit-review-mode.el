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

(ert-deftest test-shipit-review-legends-name-the-key-that-picks-the-side ()
  "GIVEN the one-line legend and the full legend of a review session
WHEN each is read
THEN it names `s', the key that picks the side that M-n and M-p walk,
next to `|', the key that opens the A side."
  (should (string-match-p "| prev side +s walk A or B"
                          (shipit-review--actions-legend-minimal)))
  (let ((navigate (cdr (assoc "Navigate" shipit-review--actions-legend-sections)))
        (view (cdr (assoc "View" shipit-review--actions-legend-sections))))
    (should (equal '("|" "s") (last (mapcar #'car navigate) 2)))
    (should (equal "walk A or B" (cadr (assoc "s" navigate))))
    (should-not (assoc "s" view))))

(ert-deftest test-shipit-review-menus-show-outside-the-row-of-the-legend ()
  "GIVEN a side window at the bottom, as the legend of a review session is
WHEN the review menu or the compare menu shows its popup
THEN the popup is no side window.  Emacs gives the windows on one side
of the frame one height, so in the row of a one-line legend a menu was
cut off."
  (dolist (command '(shipit-review-menu shipit-review-compare-menu))
    (let ((legend (get-buffer-create " *test legend*"))
          (popup (get-buffer-create " *test popup*"))
          (menu (get command 'transient--prefix)))
      (unwind-protect
          (save-window-excursion
            (display-buffer-in-side-window legend '((side . bottom) (window-height . 2)))
            (let ((window (display-buffer popup (or (oref menu display-action)
                                                    transient-display-buffer-action))))
              (should (window-live-p window))
              (should-not (window-parameter window 'window-side))))
        (kill-buffer legend)
        (kill-buffer popup)))))

(ert-deftest test-shipit-review-mode-leaves-c-c-letters-to-the-user ()
  "GIVEN the keymap of review mode
WHEN each key of C-c and a letter is looked up
THEN none is bound, because Emacs keeps those keys for the user."
  (dolist (letter (string-to-list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    ;; A number means that the map has no such prefix at all.
    (let ((binding (lookup-key shipit-review-mode-map (kbd (format "C-c %c" letter)))))
      (should (or (null binding) (numberp binding))))))

(provide 'test-shipit-review-mode)
;;; test-shipit-review-mode.el ends here
