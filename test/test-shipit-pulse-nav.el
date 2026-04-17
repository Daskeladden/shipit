;;; test-shipit-pulse-nav.el --- Tests for navigation pulse helper -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for shipit--pulse-current-line and its integration with the activity
;; dispatcher.  The helper pulses the current line after navigation, using
;; pulsar when available and falling back to the built-in pulse.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-sections)
(require 'shipit-pr-actions)

(ert-deftest test-pulse-current-line-uses-pulsar-when-available ()
  "GIVEN pulsar-pulse-line is fboundp
WHEN shipit--pulse-current-line is called
THEN pulsar-pulse-line is invoked."
  (let ((pulsar-called 0))
    (cl-letf (((symbol-function 'pulsar-pulse-line)
               (lambda () (cl-incf pulsar-called))))
      (with-temp-buffer
        (insert "hello world\n")
        (shipit--pulse-current-line)
        (should (= pulsar-called 1))))))

(ert-deftest test-pulse-current-line-falls-back-to-builtin ()
  "GIVEN pulsar is not available but pulse-momentary-highlight-region is
WHEN shipit--pulse-current-line is called
THEN the built-in pulse highlights the current line region."
  (let ((builtin-range nil))
    ;; Force the pulsar path closed regardless of the test env by rebinding
    ;; `fboundp' to lie about `pulsar-pulse-line', so we never touch the
    ;; real symbol's function slot.
    (cl-letf* ((orig-fboundp (symbol-function 'fboundp))
               ((symbol-function 'fboundp)
                (lambda (sym)
                  (if (eq sym 'pulsar-pulse-line)
                      nil
                    (funcall orig-fboundp sym))))
               ((symbol-function 'pulse-momentary-highlight-region)
                (lambda (beg end &rest _)
                  (setq builtin-range (cons beg end)))))
      (with-temp-buffer
        (insert "hello world\n")
        (goto-char (point-min))
        (shipit--pulse-current-line)
        (should builtin-range)
        (should (= (car builtin-range) (line-beginning-position)))))))

(ert-deftest test-dispatch-pulses-after-successful-navigation ()
  "GIVEN a navigation helper that moves point
WHEN shipit-pr--dispatch-activity-navigation is invoked
THEN shipit--pulse-current-line runs."
  (let ((pulsed 0))
    (cl-letf (((symbol-function 'shipit--navigate-to-comment-by-id)
               (lambda (_id)
                 (goto-char (1+ (point)))  ; simulate point move
                 t))
              ((symbol-function 'shipit--pulse-current-line)
               (lambda () (cl-incf pulsed))))
      (with-temp-buffer
        (insert "aaaaa\n")
        (goto-char (point-min))
        (shipit-pr--dispatch-activity-navigation
         '(:event-type "commented" :comment-id 1))
        (should (= pulsed 1))))))

(ert-deftest test-dispatch-does-not-pulse-when-point-unchanged ()
  "GIVEN a navigation helper that does not move point (e.g., target not found)
WHEN shipit-pr--dispatch-activity-navigation is invoked
THEN shipit--pulse-current-line is not called."
  (let ((pulsed 0))
    (cl-letf (((symbol-function 'shipit--navigate-to-comment-by-id)
               (lambda (_id) nil))  ; no point move, no success
              ((symbol-function 'shipit--pulse-current-line)
               (lambda () (cl-incf pulsed)))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (with-temp-buffer
        (insert "aaaaa\n")
        (goto-char (point-min))
        (shipit-pr--dispatch-activity-navigation
         '(:event-type "commented" :comment-id 1))
        (should (= pulsed 0))))))

(provide 'test-shipit-pulse-nav)
;;; test-shipit-pulse-nav.el ends here
