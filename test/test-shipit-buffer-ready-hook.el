;;; test-shipit-buffer-ready-hook.el --- Tests for buffer-ready hook -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for shipit-buffer-ready-hook and pending-async-sections tracking.
;; The hook fires once per refresh cycle when all expected async sections
;; have reported back. A safety timeout guarantees the hook fires even if
;; a section never marks itself ready.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-buffer)

(defun test-buffer-ready--with-fresh-buffer (body)
  "Run BODY in a fresh buffer with pending-sections tracking reset."
  (with-temp-buffer
    (setq-local shipit-buffer--pending-async-sections nil)
    (setq-local shipit-buffer--ready-hook-fired nil)
    (setq-local shipit-buffer--ready-timeout-timer nil)
    (funcall body)))

(ert-deftest test-buffer-ready-hook-fires-when-all-sections-marked ()
  "GIVEN three pending sections
WHEN each one is marked ready
THEN shipit-buffer-ready-hook fires exactly once after the last mark."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((fire-count 0))
       ;; GIVEN
       (shipit-buffer--reset-pending-sections '(general-comments activity files))
       (add-hook 'shipit-buffer-ready-hook
                 (lambda () (cl-incf fire-count))
                 nil t)
       ;; WHEN
       (shipit-buffer--mark-section-ready 'general-comments)
       (should (= fire-count 0))
       (shipit-buffer--mark-section-ready 'activity)
       (should (= fire-count 0))
       (shipit-buffer--mark-section-ready 'files)
       ;; THEN
       (should (= fire-count 1))))))

(ert-deftest test-buffer-ready-hook-fires-only-once ()
  "GIVEN all sections marked ready (hook fired)
WHEN the same section is marked ready again
THEN the hook does not re-fire."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((fire-count 0))
       ;; GIVEN
       (shipit-buffer--reset-pending-sections '(a b))
       (add-hook 'shipit-buffer-ready-hook
                 (lambda () (cl-incf fire-count))
                 nil t)
       (shipit-buffer--mark-section-ready 'a)
       (shipit-buffer--mark-section-ready 'b)
       (should (= fire-count 1))
       ;; WHEN
       (shipit-buffer--mark-section-ready 'a)
       (shipit-buffer--mark-section-ready 'b)
       ;; THEN
       (should (= fire-count 1))))))

(ert-deftest test-buffer-ready-hook-ignores-unknown-section ()
  "GIVEN pending sections are (a b)
WHEN an unknown section name is marked ready
THEN the pending set does not change and the hook does not fire."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((fire-count 0))
       ;; GIVEN
       (shipit-buffer--reset-pending-sections '(a b))
       (add-hook 'shipit-buffer-ready-hook
                 (lambda () (cl-incf fire-count))
                 nil t)
       ;; WHEN
       (shipit-buffer--mark-section-ready 'zzz)
       ;; THEN
       (should (= fire-count 0))
       (should (equal (sort (copy-sequence shipit-buffer--pending-async-sections) #'string<)
                      '(a b)))))))

(ert-deftest test-buffer-ready-hook-reset-clears-fired-flag ()
  "GIVEN hook has already fired for a prior refresh
WHEN reset is called again for a new refresh
THEN the hook can fire again once all new sections mark ready."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((fire-count 0))
       (add-hook 'shipit-buffer-ready-hook
                 (lambda () (cl-incf fire-count))
                 nil t)
       ;; First cycle
       (shipit-buffer--reset-pending-sections '(a))
       (shipit-buffer--mark-section-ready 'a)
       (should (= fire-count 1))
       ;; Second cycle
       (shipit-buffer--reset-pending-sections '(b c))
       (shipit-buffer--mark-section-ready 'b)
       (shipit-buffer--mark-section-ready 'c)
       (should (= fire-count 2))))))

(ert-deftest test-buffer-ready-hook-timeout-fires-hook ()
  "GIVEN pending sections and a short safety timeout
WHEN sections never mark ready and the timeout elapses
THEN shipit-buffer-ready-hook fires anyway."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((fire-count 0)
           (shipit-buffer-ready-timeout 0.05))
       ;; GIVEN
       (add-hook 'shipit-buffer-ready-hook
                 (lambda () (cl-incf fire-count))
                 nil t)
       (shipit-buffer--reset-pending-sections '(a b))
       ;; WHEN — wait past timeout
       (sleep-for 0.15)
       ;; Process pending timers
       (while (accept-process-output nil 0.01))
       ;; THEN
       (should (>= fire-count 1))))))

(ert-deftest test-buffer-ready-hook-timeout-cancelled-on-completion ()
  "GIVEN a long safety timeout
WHEN all sections mark ready before timeout
THEN the timer is cancelled and hook fires exactly once."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((fire-count 0)
           (shipit-buffer-ready-timeout 10.0))
       ;; GIVEN
       (add-hook 'shipit-buffer-ready-hook
                 (lambda () (cl-incf fire-count))
                 nil t)
       (shipit-buffer--reset-pending-sections '(a))
       ;; WHEN
       (shipit-buffer--mark-section-ready 'a)
       ;; THEN
       (should (= fire-count 1))
       (should (null shipit-buffer--ready-timeout-timer))))))

(ert-deftest test-buffer-ready-reset-clears-existing-timer ()
  "GIVEN a previous refresh left a pending timer
WHEN reset is called for a new refresh
THEN the old timer is cancelled and a new one is started."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((shipit-buffer-ready-timeout 10.0))
       (shipit-buffer--reset-pending-sections '(a))
       (let ((first-timer shipit-buffer--ready-timeout-timer))
         (should first-timer)
         (shipit-buffer--reset-pending-sections '(b))
         (let ((second-timer shipit-buffer--ready-timeout-timer))
           (should second-timer)
           (should (not (eq first-timer second-timer)))))))))

(ert-deftest test-buffer-ready-mark-empty-set-does-nothing ()
  "GIVEN no pending sections (empty set)
WHEN mark-section-ready is called
THEN the hook does not fire (nothing was pending)."
  (test-buffer-ready--with-fresh-buffer
   (lambda ()
     (let ((fire-count 0))
       (add-hook 'shipit-buffer-ready-hook
                 (lambda () (cl-incf fire-count))
                 nil t)
       (shipit-buffer--mark-section-ready 'anything)
       (should (= fire-count 0))))))

(provide 'test-shipit-buffer-ready-hook)
;;; test-shipit-buffer-ready-hook.el ends here
