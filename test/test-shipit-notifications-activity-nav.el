;;; test-shipit-notifications-activity-nav.el --- Tests for notification activity RET navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the notifications-buffer activity RET handler: when cursor is on
;; an activity line (with shipit-event-type text property), pressing RET opens
;; the PR buffer and navigates to that activity once the buffer is ready.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-buffer)
(require 'shipit-pr-actions)
(require 'shipit-notifications-buffer)

(ert-deftest test-activity-props-at-point-nil-without-event-type ()
  "GIVEN point has no shipit-event-type text property
WHEN reading activity props at point
THEN returns nil."
  (with-temp-buffer
    (insert "plain text\n")
    (goto-char (point-min))
    (should (null (shipit-notifications-buffer--activity-props-at-point)))))

(ert-deftest test-activity-props-at-point-captures-all-fields ()
  "GIVEN point has activity text properties
WHEN reading activity props at point
THEN returns a plist with all nine fields from the properties."
  (with-temp-buffer
    (insert "activity line\n")
    (add-text-properties
     (point-min) (1- (point-max))
     '(shipit-event-type "commented"
                         shipit-activity-comment-id 12345
                         shipit-activity-commit-sha "abc123"
                         shipit-review-state "approved"
                         shipit-crossref-repo "owner/repo"
                         shipit-crossref-number 99
                         shipit-crossref-url "https://example.com/99"
                         shipit-crossref-title "Fix X"
                         shipit-inline-comment-path "src/foo.el"))
    (goto-char (point-min))
    (let ((props (shipit-notifications-buffer--activity-props-at-point)))
      (should (equal (plist-get props :event-type) "commented"))
      (should (equal (plist-get props :comment-id) 12345))
      (should (equal (plist-get props :commit-sha) "abc123"))
      (should (equal (plist-get props :review-state) "approved"))
      (should (equal (plist-get props :crossref-repo) "owner/repo"))
      (should (equal (plist-get props :crossref-number) 99))
      (should (equal (plist-get props :crossref-url) "https://example.com/99"))
      (should (equal (plist-get props :crossref-title) "Fix X"))
      (should (equal (plist-get props :inline-comment-path) "src/foo.el")))))

(ert-deftest test-schedule-activity-nav-dispatches-when-buffer-ready ()
  "GIVEN a target buffer with no pending async sections
WHEN schedule-activity-nav is called
THEN the dispatcher runs immediately with the given props."
  (let ((captured nil))
    (cl-letf (((symbol-function 'shipit-pr--dispatch-activity-navigation)
               (lambda (p) (setq captured p))))
      (with-temp-buffer
        (setq-local shipit-buffer--pending-async-sections nil)
        (shipit-notifications-buffer--schedule-activity-nav
         (current-buffer) '(:event-type "commented" :comment-id 42))
        (should (equal captured '(:event-type "commented" :comment-id 42)))))))

(ert-deftest test-schedule-activity-nav-registers-hook-when-loading ()
  "GIVEN a target buffer with pending async sections
WHEN schedule-activity-nav is called
THEN a one-shot handler is registered on shipit-buffer-ready-hook."
  (let ((dispatch-calls 0))
    (cl-letf (((symbol-function 'shipit-pr--dispatch-activity-navigation)
               (lambda (_p) (cl-incf dispatch-calls))))
      (with-temp-buffer
        (setq-local shipit-buffer--pending-async-sections '(general-comments))
        (shipit-notifications-buffer--schedule-activity-nav
         (current-buffer) '(:event-type "commented" :comment-id 42))
        ;; Dispatcher should not have fired yet
        (should (= dispatch-calls 0))
        ;; Hook should be populated
        (should (bound-and-true-p shipit-buffer-ready-hook))
        ;; Fire the hook manually to simulate buffer settling
        (run-hooks 'shipit-buffer-ready-hook)
        (should (= dispatch-calls 1))
        ;; Firing again should NOT double-dispatch (one-shot semantics)
        (run-hooks 'shipit-buffer-ready-hook)
        (should (= dispatch-calls 1))))))

(ert-deftest test-schedule-activity-nav-handles-dead-buffer ()
  "GIVEN the target buffer has been killed
WHEN schedule-activity-nav is called
THEN it returns without error and does not dispatch."
  (let ((dispatched nil))
    (cl-letf (((symbol-function 'shipit-pr--dispatch-activity-navigation)
               (lambda (_p) (setq dispatched t))))
      (let ((buf (generate-new-buffer " *dead*")))
        (kill-buffer buf)
        (shipit-notifications-buffer--schedule-activity-nav
         buf '(:event-type "commented"))
        (should (null dispatched))))))

(provide 'test-shipit-notifications-activity-nav)
;;; test-shipit-notifications-activity-nav.el ends here
