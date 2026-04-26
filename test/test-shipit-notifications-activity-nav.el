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

(ert-deftest test-schedule-activity-nav-waits-for-required-section ()
  "GIVEN a target buffer where the section holding the activity is still pending
WHEN schedule-activity-nav is called
THEN dispatch waits for that specific section to be marked ready, not the
whole buffer-ready hook."
  (let ((dispatch-calls 0))
    (cl-letf (((symbol-function 'shipit-pr--dispatch-activity-navigation)
               (lambda (_p) (cl-incf dispatch-calls))))
      (with-temp-buffer
        ;; "commented" needs general-comments rendered.  files is irrelevant.
        (setq-local shipit-buffer--pending-async-sections '(general-comments files))
        (shipit-notifications-buffer--schedule-activity-nav
         (current-buffer) '(:event-type "commented" :comment-id 42))
        ;; Dispatcher should not have fired yet — general-comments still pending
        (should (= dispatch-calls 0))
        ;; Marking an unrelated section ready does NOT trigger dispatch
        (shipit-buffer--mark-section-ready 'files)
        (should (= dispatch-calls 0))
        ;; Marking the required section ready fires the dispatch
        (shipit-buffer--mark-section-ready 'general-comments)
        (should (= dispatch-calls 1))
        ;; Subsequent mark-ready calls do not double-dispatch
        (shipit-buffer--mark-section-ready 'general-comments)
        (should (= dispatch-calls 1))))))

(ert-deftest test-schedule-activity-nav-dispatches-when-required-section-ready ()
  "GIVEN the section holding the activity has already rendered
WHEN schedule-activity-nav is called
THEN dispatch runs immediately, even if other sections are still pending."
  (let ((dispatch-calls 0))
    (cl-letf (((symbol-function 'shipit-pr--dispatch-activity-navigation)
               (lambda (_p) (cl-incf dispatch-calls))))
      (with-temp-buffer
        ;; general-comments NOT in pending → already rendered.  Other sections
        ;; still pending — but irrelevant to a "commented" event.
        (setq-local shipit-buffer--pending-async-sections '(checks files))
        (shipit-notifications-buffer--schedule-activity-nav
         (current-buffer) '(:event-type "commented" :comment-id 42))
        (should (= dispatch-calls 1))))))

(ert-deftest test-schedule-activity-nav-safety-timeout-drains-callbacks ()
  "GIVEN a callback queued for a section that never reports
WHEN the safety timeout forces fire-ready-hook
THEN the queued callback runs anyway (so navigation isn't stranded)."
  (let ((dispatch-calls 0))
    (cl-letf (((symbol-function 'shipit-pr--dispatch-activity-navigation)
               (lambda (_p) (cl-incf dispatch-calls))))
      (with-temp-buffer
        (setq-local shipit-buffer--pending-async-sections '(general-comments))
        (shipit-notifications-buffer--schedule-activity-nav
         (current-buffer) '(:event-type "commented" :comment-id 42))
        (should (= dispatch-calls 0))
        ;; Simulate safety timeout firing without any mark-ready
        (shipit-buffer--fire-ready-hook)
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
