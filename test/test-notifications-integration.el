;;; test-notifications-integration.el --- Integration tests -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Daskeladden
;; Keywords: test, github, notifications

;;; Commentary:

;; Integration tests for the notifications buffer system.

;;; Code:

(require 'ert)
(require 'shipit-notifications)
(require 'shipit-notifications-buffer)

(ert-deftest test-notifications-full-workflow ()
  "Test the complete notifications workflow."
  ;; Stub the network fetch + total probe so the merge step doesn't wipe
  ;; our test fixture and the buffer renders only what we put in.
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _) nil))
            ((symbol-function 'shipit--fetch-notifications-total-count-async)
             (lambda (&rest _) nil))
            ((symbol-function 'shipit--poll-backend-notifications)
             (lambda (&rest _) nil)))
    ;; Setup mock data — `backend-id' tag prevents
    ;; `shipit--merge-github-notifications' from removing it on refresh.
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
      (puthash "test/repo:pr:42"
               '((repo . "test/repo")
                 (number . 42)
                 (type . "pr")
                 (subject . "Test PR")
                 (reason . "mention")
                 (updated-at . "2025-01-29T10:00:00Z")
                 (backend-id . test))
               shipit--notification-pr-activities)

      (unwind-protect
          (progn
            ;; View notifications
            (shipit--view-notifications)
            (should (string= (buffer-name) "*shipit-notifications*"))

            ;; Check content rendered
            (should (string-match-p "test/repo" (buffer-string)))
            (should (string-match-p "#42" (buffer-string)))

            ;; Test filter
            (let ((shipit-notifications-buffer--filter-text "nonexistent"))
              (shipit-notifications-buffer-refresh)
              (should (string-match-p "No notifications" (buffer-string))))

            ;; Clear filter
            (let ((shipit-notifications-buffer--filter-text ""))
              (shipit-notifications-buffer-refresh)
              (should (string-match-p "test/repo" (buffer-string)))))

        ;; Cleanup
        (when (get-buffer "*shipit-notifications*")
          (kill-buffer "*shipit-notifications*"))))))

;;; Alert Tests

(ert-deftest test-shipit--should-alert-p-enabled ()
  "Test that should-alert-p returns t when reason matches and backend enabled."
  (let ((shipit-notification-alert-backend 'dbus)
        (shipit-notification-alert-reasons '(mention review_requested))
        (shipit--alerted-notification-ids (make-hash-table :test 'equal))
        (notification '((reason . "mention")
                        (repo . "test/repo")
                        (number . 123)
                        (notification . ((id . "notif-123"))))))
    (should (shipit--should-alert-p notification))))

(ert-deftest test-shipit--should-alert-p-wrong-reason ()
  "Test that should-alert-p returns nil when reason doesn't match."
  (let ((shipit-notification-alert-backend 'dbus)
        (shipit-notification-alert-reasons '(mention))
        (shipit--alerted-notification-ids (make-hash-table :test 'equal))
        (notification '((reason . "comment")
                        (repo . "test/repo")
                        (number . 123)
                        (notification . ((id . "notif-123"))))))
    (should-not (shipit--should-alert-p notification))))

(ert-deftest test-shipit--should-alert-p-backend-disabled ()
  "Test that should-alert-p returns nil when backend is disabled."
  (let ((shipit-notification-alert-backend nil)
        (shipit-notification-alert-reasons '(mention))
        (shipit--alerted-notification-ids (make-hash-table :test 'equal))
        (notification '((reason . "mention")
                        (repo . "test/repo")
                        (number . 123)
                        (notification . ((id . "notif-123"))))))
    (should-not (shipit--should-alert-p notification))))

(ert-deftest test-shipit--should-alert-p-already-alerted ()
  "Test that should-alert-p returns nil for already-alerted notifications."
  (let ((shipit-notification-alert-backend 'dbus)
        (shipit-notification-alert-reasons '(mention))
        (shipit--alerted-notification-ids (make-hash-table :test 'equal))
        (notification '((reason . "mention")
                        (repo . "test/repo")
                        (number . 123)
                        (notification . ((id . "notif-123"))))))
    ;; Mark as already alerted
    (puthash "notif-123" t shipit--alerted-notification-ids)
    (should-not (shipit--should-alert-p notification))))

(ert-deftest test-shipit--mark-alerted ()
  "Test that mark-alerted adds notification ID to hash table."
  (let ((shipit--alerted-notification-ids (make-hash-table :test 'equal))
        (notification '((reason . "mention")
                        (repo . "test/repo")
                        (number . 123)
                        (notification . ((id . "notif-456"))))))
    (shipit--mark-alerted notification)
    (should (gethash "notif-456" shipit--alerted-notification-ids))))

(ert-deftest test-shipit--format-reason-for-alert ()
  "Test that reason formatting works correctly."
  (should (equal "Mention" (shipit--format-reason-for-alert "mention")))
  (should (equal "Review requested" (shipit--format-reason-for-alert "review_requested")))
  (should (equal "Assigned" (shipit--format-reason-for-alert "assign")))
  (should (equal "Comment" (shipit--format-reason-for-alert "comment")))
  (should (equal "Team mention" (shipit--format-reason-for-alert "team_mention")))
  (should (equal "Activity on your PR" (shipit--format-reason-for-alert "author")))
  (should (equal "Subscribed" (shipit--format-reason-for-alert "subscribed"))))

(ert-deftest test-shipit--alert-toggle-desc ()
  "Test alert toggle description formatting."
  (let ((shipit-notification-alert-reasons '(mention)))
    (should (equal "[x] Mention alerts" (shipit--alert-toggle-desc "Mention" 'mention)))
    (should (equal "[ ] Comment alerts" (shipit--alert-toggle-desc "Comment" 'comment)))))

(ert-deftest test-shipit--toggle-alert-reason ()
  "Test toggling alert reasons on and off."
  (let ((shipit-notification-alert-reasons '()))
    ;; Toggle on
    (shipit--toggle-alert-reason 'mention)
    (should (memq 'mention shipit-notification-alert-reasons))
    ;; Toggle off
    (shipit--toggle-alert-reason 'mention)
    (should-not (memq 'mention shipit-notification-alert-reasons))))

(ert-deftest test-shipit--queue-alert-batching ()
  "Test that queue-alert batches alerts correctly."
  (let ((shipit--pending-alerts '())
        (shipit--alert-timer nil)
        (shipit--alerted-notification-ids (make-hash-table :test 'equal))
        (shipit-notification-alert-batch-seconds 5)
        (notification1 '((reason . "mention")
                         (repo . "test/repo")
                         (number . 123)
                         (notification . ((id . "notif-1")))))
        (notification2 '((reason . "review_requested")
                         (repo . "test/repo")
                         (number . 456)
                         (notification . ((id . "notif-2"))))))
    (unwind-protect
        (progn
          (shipit--queue-alert notification1)
          (should (= 1 (length shipit--pending-alerts)))
          (should shipit--alert-timer)
          (shipit--queue-alert notification2)
          (should (= 2 (length shipit--pending-alerts))))
      ;; Cleanup
      (when shipit--alert-timer
        (cancel-timer shipit--alert-timer)))))

(ert-deftest test-shipit-mark-notification-read-removes-from-buffer ()
  "GIVEN a GitHub notification (has `notification' key with thread id)
WHEN calling shipit--mark-notification-read
THEN the PR backend's mark-notification-read is called and the notification is removed."
  (require 'shipit-pr-github)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (mark-read-called nil))
    ;; Add a test notification (GitHub notification — has `notification' key)
    (puthash "test/repo:pr:42"
             '((repo . "test/repo")
               (number . 42)
               (type . "pr")
               (subject . "Test PR")
               (reason . "mention")
               (updated-at . "2025-01-29T10:00:00Z")
               (notification . ((id . "notif-123"))))
             shipit--notification-pr-activities)
    ;; Add to mention tracking
    (push '((repo . "test/repo") (number . 42)) shipit--mention-prs)
    (setq shipit--mention-count 1)
    (setq shipit--last-notification-count 1)

    (unwind-protect
        (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
                   (lambda (_config id) (setq mark-read-called id)))
                  ((symbol-function 'shipit--update-modeline-indicator) #'ignore)
                  ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
          ;; Mark the notification as read
          (shipit--mark-notification-read 42 "test/repo" t)

          ;; Verify it was removed from the hash table
          (should-not (gethash "test/repo:pr:42" shipit--notification-pr-activities))
          ;; Verify the count was updated
          (should (= 0 (hash-table-count shipit--notification-pr-activities)))
          ;; Verify mention was removed
          (should (= 0 shipit--mention-count))
          ;; Verify the PR backend's mark-notification-read was called with notification id
          (should (equal "notif-123" mark-read-called)))

      ;; Cleanup
      (when (get-buffer "*shipit-notifications*")
        (kill-buffer "*shipit-notifications*")))))

(ert-deftest test-shipit-mark-notification-read-github-no-source ()
  "GIVEN a GitHub notification without a source field and the GitHub PR backend registered
WHEN calling shipit--mark-notification-read
THEN the PR backend's :mark-notification-read is called
     AND the notification-id is stored in locally-marked-read for filtering."
  (require 'shipit-pr-github)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 1)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (github-api-called-with nil))
    ;; Add a notification without source field
    (puthash "owner/repo:pr:99"
             '((repo . "owner/repo")
               (number . 99)
               (type . "pr")
               (subject . "Fix widget")
               (reason . "review_requested")
               (notification . ((id . "22923047192"))))
             shipit--notification-pr-activities)

    (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
               (lambda (_config id) (setq github-api-called-with id)))
              ((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      (shipit--mark-notification-read 99 "owner/repo" t "pr")

      ;; THEN the PR backend's mark-notification-read was called with the notification ID
      (should (equal "22923047192" github-api-called-with))
      ;; THEN notification-id is tracked for local filtering
      (should (gethash "22923047192" shipit--locally-marked-read-notifications))
      ;; THEN activity was removed from the hash table
      (should-not (gethash "owner/repo:pr:99" shipit--notification-pr-activities)))))

(ert-deftest test-shipit-mark-notification-read-github-with-jira-issue-backend ()
  "GIVEN a GitHub notification (has `notification' key) for a repo whose issue backend is Jira
WHEN calling shipit--mark-notification-read
THEN the PR backend's :mark-notification-read is called (not routed through the issue backend)."
  (require 'shipit-pr-github)
  (require 'shipit-issue-jira)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 1)
        ;; Repo configured with Jira as the issue backend
        (shipit-issue-repo-backends
         '(("acme/acme-project" :backend jira
            :base-url "https://test.atlassian.net" :project "PROJ")))
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (github-api-called-with nil))
    ;; GIVEN: a GitHub notification with a `notification' key containing the thread id
    ;; (the `notification' key is the natural indicator of a GitHub notification —
    ;; it comes from the GitHub Notifications API, NOT the issue backend)
    (puthash "acme/acme-project:pr:123"
             '((repo . "acme/acme-project")
               (number . 123)
               (type . "pr")
               (subject . "Add feature")
               (reason . "review_requested")
               (notification . ((id . "22959180290"))))
             shipit--notification-pr-activities)

    (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
               (lambda (_config id) (setq github-api-called-with id)))
              ((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      (shipit--mark-notification-read 123 "acme/acme-project" t "pr")

      ;; THEN the PR backend's mark-notification-read was called (not routed through Jira)
      (should (equal "22959180290" github-api-called-with))
      ;; THEN notification-id is tracked for local filtering
      (should (gethash "22959180290" shipit--locally-marked-read-notifications)))))

(ert-deftest test-shipit-notifications-buffer-mark-read-keybinding ()
  "GIVEN a notification in the buffer with the GitHub PR backend registered
WHEN pressing 'm' on the notification line
THEN the notification is removed and mention count updated."
  (require 'shipit-pr-github)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    ;; Add a test notification
    (puthash "test/repo:pr:42"
             '((repo . "test/repo")
               (number . 42)
               (type . "pr")
               (subject . "Test PR")
               (reason . "mention")
               (updated-at . "2025-01-29T10:00:00Z")
               (notification . ((id . "notif-123"))))
             shipit--notification-pr-activities)
    (push '((repo . "test/repo") (number . 42)) shipit--mention-prs)
    (setq shipit--mention-count 1)
    (setq shipit--last-notification-count 1)

    (unwind-protect
        (progn
          ;; Create and render the buffer
          (let ((buf (shipit-notifications-buffer-create)))
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              ;; Move to the notification line
              (goto-char (point-min))
              (forward-line 2)  ;; Skip header lines

              ;; Verify notification section is present before marking read
              (should (eq 'notification-entry (oref (magit-current-section) type)))
              (should (= 1 (hash-table-count shipit--notification-pr-activities)))
              (should (= 1 shipit--mention-count))

              ;; Mock the PR backend's mark-notification-read
              (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
                         (lambda (_config _id) nil)))
                ;; Call the mark-read function (simulates pressing 'm')
                (shipit-notifications-buffer-mark-read)

                ;; Verify notification was removed from hash table
                (should (= 0 (hash-table-count shipit--notification-pr-activities)))
                ;; Verify mention count was updated
                (should (= 0 shipit--mention-count))))))

      ;; Cleanup
      (when (get-buffer "*shipit-notifications*")
        (kill-buffer "*shipit-notifications*")))))

(ert-deftest test-shipit-notifications-buffer-mark-read-region ()
  "GIVEN a notifications buffer with 3 notifications
WHEN region is active covering only the first 2
THEN only those 2 are marked as read and the 3rd remains."
  (require 'shipit-pr-github)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    ;; GIVEN: 3 notifications
    (puthash "repo/a:pr:1"
             '((repo . "repo/a") (number . 1) (type . "pr")
               (subject . "First PR") (reason . "mention")
               (updated-at . "2025-01-29T10:00:00Z")
               (notification . ((id . "notif-1"))))
             shipit--notification-pr-activities)
    (puthash "repo/a:pr:2"
             '((repo . "repo/a") (number . 2) (type . "pr")
               (subject . "Second PR") (reason . "comment")
               (updated-at . "2025-01-29T11:00:00Z")
               (notification . ((id . "notif-2"))))
             shipit--notification-pr-activities)
    (puthash "repo/b:pr:3"
             '((repo . "repo/b") (number . 3) (type . "pr")
               (subject . "Third PR") (reason . "review_requested")
               (updated-at . "2025-01-29T12:00:00Z")
               (notification . ((id . "notif-3"))))
             shipit--notification-pr-activities)
    (setq shipit--mention-count 1)
    (setq shipit--last-notification-count 3)

    (unwind-protect
        (progn
          (let ((buf (shipit-notifications-buffer-create)))
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              ;; Find all notification sections
              (let ((notif-starts '()))
                (dolist (child (oref magit-root-section children))
                  (when (eq (oref child type) 'notification-entry)
                    (push (oref child start) notif-starts)))
                (setq notif-starts (nreverse notif-starts))
                (should (= 3 (length notif-starts)))

                ;; WHEN: set region covering first 2 notification sections
                (goto-char (nth 0 notif-starts))
                (set-mark (nth 0 notif-starts))
                (goto-char (nth 1 notif-starts))
                (end-of-line)
                (activate-mark)

                (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
                           (lambda (_config _id) nil)))
                  (shipit-notifications-buffer-mark-read)

                  ;; THEN: only 1 notification remains
                  ;; (notifications render newest-first, so first 2 lines
                  ;; are pr:3 and pr:2; pr:1 survives)
                  (should (= 1 (hash-table-count shipit--notification-pr-activities)))
                  (should (gethash "repo/a:pr:1" shipit--notification-pr-activities)))))))

      ;; Cleanup
      (when (get-buffer "*shipit-notifications*")
        (kill-buffer "*shipit-notifications*")))))

(ert-deftest test-process-pr-notifications-empty-list-clears-count ()
  "GIVEN existing notifications with count 2
WHEN shipit--process-pr-notifications is called with empty list
THEN notification count resets to 0 and modeline clears."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 2)
        (shipit--notification-count 2)
        (shipit--modeline-string "mock-indicator")
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil))
    ;; GIVEN: pre-existing notifications in the hash table
    (puthash "repo/a:pr:1" '((repo . "repo/a") (number . 1) (type . "pr") (reason . "mention")) shipit--notification-pr-activities)
    (puthash "repo/a:pr:2" '((repo . "repo/a") (number . 2) (type . "pr") (reason . "comment")) shipit--notification-pr-activities)

    ;; WHEN: process empty notification list (GitHub returned 0 unread)
    (shipit--process-pr-notifications nil)

    ;; THEN: with the 1-poll grace period the first empty response keeps
    ;; the entries (now tagged missed-polls=1) so a single flaky GitHub
    ;; reply doesn't silently wipe legitimate notifications.
    (should (= 2 (hash-table-count shipit--notification-pr-activities)))
    ;; A second consecutive empty response evicts them.
    (shipit--process-pr-notifications nil)
    (should (= 0 (hash-table-count shipit--notification-pr-activities)))
    (should (= 0 shipit--notification-count))
    (should (null shipit--modeline-string))))

(ert-deftest test-async-callback-processes-empty-notifications ()
  "GIVEN a polling callback with prior notification state
WHEN GitHub returns empty notification list (all read)
THEN the callback should still process and clear the count."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 2)
        (shipit--notification-count 2)
        (shipit--modeline-string "mock-indicator")
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil)
        (shipit-issue-backends nil)
        (shipit-issue-repo-backends nil)
        (process-called nil))
    ;; GIVEN: pre-existing notification state
    (puthash "repo/a:pr:1" '((repo . "repo/a") (number . 1) (type . "pr") (reason . "mention")) shipit--notification-pr-activities)
    (puthash "repo/a:pr:2" '((repo . "repo/a") (number . 2) (type . "pr") (reason . "comment")) shipit--notification-pr-activities)

    ;; WHEN: simulate the async callback with nil (empty JSON array)
    ;; twice (grace period: first empty response keeps stale entries,
    ;; second evicts them).
    (cl-letf (((symbol-function 'shipit--fetch-all-notifications-async)
               (lambda (_params _since _force callback)
                 (funcall callback nil))))
      (shipit--check-notifications-background-async t)
      (shipit--check-notifications-background-async t))

    ;; THEN: notifications should be cleared
    (should (= 0 (hash-table-count shipit--notification-pr-activities)))
    (should (= 0 shipit--notification-count))
    (should (null shipit--modeline-string))))

(ert-deftest test-async-callback-handles-error-response ()
  "GIVEN an async notification fetch
WHEN GitHub returns a 404 error response {\"message\": \"Not Found\"}
THEN the error is ignored and existing notifications are preserved."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 1)
        (shipit--notification-count 1)
        (shipit--modeline-string "mock-indicator")
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil)
        (shipit-issue-backends nil)
        (shipit-issue-repo-backends nil)
        (callback-received nil))
    ;; GIVEN: one existing notification
    (puthash "repo/a:pr:1"
             '((repo . "repo/a") (number . 1) (type . "pr") (reason . "mention"))
             shipit--notification-pr-activities)

    ;; WHEN: simulate the async page fetcher returning a 404 error response
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (_endpoint _params _token callback &optional _force)
                 ;; Simulate GitHub 404: {"message": "Not Found"}
                 (funcall callback (list :json '((message . "Not Found")))))))
      ;; Call the page fetcher directly
      (shipit--fetch-notifications-page-async
       1 nil nil nil nil
       (lambda (notifications)
         (setq callback-received notifications))))

    ;; THEN: callback received nil (error filtered out)
    (should (null callback-received))))

(ert-deftest test-github-mark-read-errors-without-notification-key ()
  "GIVEN a GitHub backend activity WITHOUT a 'notification' key
WHEN shipit-issue-github--mark-notification-read is called
THEN it signals an error instead of silently skipping."
  (require 'shipit-issue-github)
  ;; GIVEN: activity missing the 'notification' key (e.g., from backend polling)
  (let ((activity '((repo . "owner/repo")
                    (number . 42)
                    (type . "pr")
                    (reason . "comment"))))
    ;; WHEN/THEN: should signal error, not silently skip
    (should-error (shipit-issue-github--mark-notification-read nil activity)
                  :type 'error)))

(ert-deftest test-github-mark-read-errors-without-notification-id ()
  "GIVEN a GitHub backend activity with notification missing 'id'
WHEN shipit-issue-github--mark-notification-read is called
THEN it signals an error instead of silently skipping."
  (require 'shipit-issue-github)
  ;; GIVEN: activity with notification but no id field
  (let ((activity '((repo . "owner/repo")
                    (number . 42)
                    (type . "pr")
                    (notification . ((url . "https://example.com"))))))
    ;; WHEN/THEN: should signal error
    (should-error (shipit-issue-github--mark-notification-read nil activity)
                  :type 'error)))

(ert-deftest test-mark-notification-read-removes-locally-without-backend-support ()
  "GIVEN a backend notification (no `notification' key) whose backend lacks :mark-notification-read
WHEN shipit--mark-notification-read is called
THEN the activity is still removed locally (graceful degradation)."
  (require 'shipit-issue-backends)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 1)
        ;; Register a minimal backend without :mark-notification-read
        (shipit-issue-backends `((test-backend . (:name "Test"
                                                  :fetch-issue ignore :fetch-comments ignore
                                                  :fetch-comments-async ignore :search ignore
                                                  :create-issue ignore :reference-patterns ignore
                                                  :browse-url ignore :id-to-string ignore
                                                  :string-to-id ignore))))
        (shipit-issue-backend 'test-backend)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    ;; GIVEN: a backend notification (has backend-id but no `notification' key)
    (puthash "test/repo:pr:1"
             '((repo . "test/repo")
               (number . 1)
               (type . "pr")
               (reason . "comment")
               (backend-id . test-backend))
             shipit--notification-pr-activities)
    (cl-letf (((symbol-function 'shipit--update-modeline-indicator) #'ignore))
      ;; WHEN: marking as read
      (shipit--mark-notification-read 1 "test/repo" t "pr")
      ;; THEN: activity removed locally even though backend has no mark-read
      (should (= 0 (hash-table-count shipit--notification-pr-activities))))))

(ert-deftest test-mark-notification-read-uses-activity-backend-id ()
  "GIVEN a GitLab notification with backend-id in a repo where default is GitHub
WHEN shipit--mark-notification-read is called
THEN the GitLab backend's mark-read is used, not GitHub's."
  (require 'shipit-issue-backends)
  (require 'shipit-issue-github)
  (require 'shipit-issue-gitlab)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 1)
        ;; Default backend is GitHub (but the activity has backend-id gitlab)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil)
        (gitlab-mark-called nil)
        (github-mark-called nil))
    ;; GIVEN: a GitLab notification with backend-id
    (puthash "mygroup/myproject:pr:10"
             '((repo . "mygroup/myproject")
               (number . 10)
               (type . "pr")
               (reason . "assigned")
               (source . gitlab)
               (backend-id . gitlab)
               (gitlab-todo-id . 12345)
               (backend-config . (:api-url "https://gitlab.com" :project-path "mygroup/myproject")))
             shipit--notification-pr-activities)
    ;; Mock both backends' mark-read functions
    (cl-letf (((symbol-function 'shipit-issue-gitlab--mark-notification-read)
               (lambda (_config _activity) (setq gitlab-mark-called t)))
              ((symbol-function 'shipit-pr-github--mark-notification-read)
               (lambda (_config _id) (setq github-mark-called t)))
              ((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      ;; WHEN: marking as read
      (shipit--mark-notification-read 10 "mygroup/myproject" t "pr")
      ;; THEN: GitLab backend was used (not GitHub)
      (should gitlab-mark-called)
      (should-not github-mark-called)
      ;; AND activity was removed
      (should (= 0 (hash-table-count shipit--notification-pr-activities))))))

;;; Phase 5 — Backend dispatch tests

(ert-deftest test-mark-notification-read-dispatches-through-pr-backend ()
  "GIVEN a GitHub notification (has `notification' key with thread id)
   AND the GitHub PR backend is registered with :mark-notification-read
WHEN calling shipit--mark-notification-read
THEN the PR backend's :mark-notification-read function is called
     AND the notification is removed from local cache."
  (require 'shipit-pr-github)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 1)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (pr-backend-mark-called-with nil))
    ;; GIVEN: a GitHub notification with notification key containing thread id
    (puthash "owner/repo:pr:55"
             '((repo . "owner/repo")
               (number . 55)
               (type . "pr")
               (subject . "Fix widget")
               (reason . "review_requested")
               (notification . ((id . "thread-99887"))))
             shipit--notification-pr-activities)

    (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
               (lambda (config notification-id)
                 (setq pr-backend-mark-called-with
                       (list config notification-id))))
              ((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      ;; WHEN: marking the notification as read
      (shipit--mark-notification-read 55 "owner/repo" t "pr")

      ;; THEN: PR backend's mark-notification-read was called
      (should pr-backend-mark-called-with)
      ;; THEN: notification-id was passed correctly
      (should (equal "thread-99887" (cadr pr-backend-mark-called-with)))
      ;; THEN: config has :repo set
      (should (equal "owner/repo"
                     (plist-get (car pr-backend-mark-called-with) :repo)))
      ;; THEN: notification was removed from the hash table
      (should-not (gethash "owner/repo:pr:55" shipit--notification-pr-activities))
      ;; THEN: notification-id tracked in locally-marked-read
      (should (gethash "thread-99887" shipit--locally-marked-read-notifications)))))

(ert-deftest test-mark-notification-read-falls-back-without-pr-backend-fn ()
  "GIVEN a GitHub notification
   AND the PR backend has no :mark-notification-read function
WHEN calling shipit--mark-notification-read
THEN no error is raised
     AND the notification is still removed locally."
  (require 'shipit-pr-backends)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 1)
        ;; Register a minimal PR backend without :mark-notification-read
        (shipit-pr-backends `((test-pr . (:name "Test PR"
                                           :fetch-pr ignore :search ignore
                                           :create-pr ignore :merge-pr ignore
                                           :update-pr ignore :fetch-reviews ignore
                                           :submit-review ignore :fetch-review-decision ignore
                                           :fetch-files ignore :fetch-commits ignore
                                           :fetch-checks ignore :browse-url ignore))))
        (shipit-pr-backend 'test-pr)
        (shipit-pr-backend-config nil))
    ;; GIVEN: a GitHub notification
    (puthash "test/repo:pr:10"
             '((repo . "test/repo")
               (number . 10)
               (type . "pr")
               (subject . "Test")
               (reason . "mention")
               (notification . ((id . "thread-555"))))
             shipit--notification-pr-activities)

    (cl-letf (((symbol-function 'shipit--update-modeline-indicator) #'ignore))
      ;; WHEN: marking as read (should not error)
      (shipit--mark-notification-read 10 "test/repo" t "pr")

      ;; THEN: notification was still removed locally
      (should (= 0 (hash-table-count shipit--notification-pr-activities))))))

(ert-deftest test-discussion-url-dispatches-through-pr-backend ()
  "GIVEN the GitHub PR backend with :browse-discussion-url registered
WHEN opening a discussion via notification actions
THEN the URL is constructed through the PR backend."
  (require 'shipit-pr-github)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (browsed-url nil))
    ;; GIVEN: repo and discussion number
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq browsed-url url))))
      ;; WHEN: constructing discussion URL through PR backend dispatch
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (browse-fn (plist-get backend :browse-discussion-url)))
        ;; THEN: the function is available
        (should browse-fn)
        ;; THEN: it returns the correct URL
        (let ((url (funcall browse-fn config 42)))
          (should (equal "https://github.com/owner/repo/discussions/42" url)))))))

(ert-deftest test-fetch-all-notifications-dispatches-through-pr-backend ()
  "GIVEN the GitHub PR backend with :fetch-notifications registered
WHEN shipit--fetch-all-notifications is called
THEN it dispatches through the backend's :fetch-notifications function."
  (require 'shipit-pr-github)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (fetch-called-with nil))
    ;; GIVEN: mock the backend fetch function
    (cl-letf (((symbol-function 'shipit-pr-github--fetch-notifications)
               (lambda (config params &optional force-fresh)
                 (setq fetch-called-with (list config params force-fresh))
                 ;; Return empty list (no notifications)
                 nil))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      ;; WHEN: fetching notifications
      (let ((result (shipit--fetch-all-notifications '((per_page . 100)) nil nil)))
        ;; THEN: the backend function was called
        (should fetch-called-with)
        ;; THEN: config has :repo set
        (should (equal "owner/repo"
                       (plist-get (car fetch-called-with) :repo)))
        ;; THEN: params include page number
        (should (assq 'page (cadr fetch-called-with)))))))

(ert-deftest test-mark-all-notifications-read-dispatches-through-pr-backend ()
  "GIVEN a GitHub notification in the activities hash
   AND the GitHub PR backend is registered with :mark-notification-read
WHEN shipit--mark-all-notifications-read is called
THEN the PR backend's :mark-notification-read is called for GitHub notifications."
  (require 'shipit-pr-github)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (pr-backend-mark-called-ids nil))
    ;; GIVEN: a GitHub notification
    (puthash "owner/repo:pr:1"
             '((repo . "owner/repo")
               (number . 1)
               (type . "pr")
               (subject . "Test PR")
               (reason . "mention")
               (notification . ((id . "thread-111"))))
             shipit--notification-pr-activities)

    (cl-letf (((symbol-function 'shipit-pr-github--mark-notification-read)
               (lambda (_config notification-id)
                 (push notification-id pr-backend-mark-called-ids)))
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore)
              ((symbol-function 'shipit--clear-modeline-indicator) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'magit-refresh) #'ignore))
      ;; WHEN: marking all as read
      (shipit--mark-all-notifications-read)

      ;; THEN: PR backend's mark-notification-read was called
      (should (member "thread-111" pr-backend-mark-called-ids)))))

(ert-deftest test-shipit--merge-github-notifications-grace-period ()
  "GIVEN a GitHub entry from a previous poll
WHEN the next poll's response is missing it once
THEN the entry survives with `missed-polls' incremented.
WHEN the poll after that is also missing it
THEN the entry is evicted."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (let ((first (make-hash-table :test 'equal)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr"))
               first)
      (shipit--merge-github-notifications first))
    (should (gethash "owner/foo:pr:1" shipit--notification-pr-activities))
    ;; Empty response: entry stays, missed-polls bumped to 1.
    (shipit--merge-github-notifications (make-hash-table :test 'equal))
    (let ((entry (gethash "owner/foo:pr:1" shipit--notification-pr-activities)))
      (should entry)
      (should (= 1 (cdr (assq 'missed-polls entry)))))
    ;; Second consecutive empty: evicted.
    (shipit--merge-github-notifications (make-hash-table :test 'equal))
    (should-not (gethash "owner/foo:pr:1" shipit--notification-pr-activities))))

(ert-deftest test-shipit--merge-github-notifications-resets-on-reappearance ()
  "An entry that reappears after one missing poll has missed-polls reset to 0."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (let ((first (make-hash-table :test 'equal)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr"))
               first)
      (shipit--merge-github-notifications first))
    ;; Missing once.
    (shipit--merge-github-notifications (make-hash-table :test 'equal))
    (let ((entry (gethash "owner/foo:pr:1" shipit--notification-pr-activities)))
      (should (= 1 (cdr (assq 'missed-polls entry)))))
    ;; Reappears.
    (let ((second (make-hash-table :test 'equal)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr"))
               second)
      (shipit--merge-github-notifications second))
    (let ((entry (gethash "owner/foo:pr:1" shipit--notification-pr-activities)))
      (should entry)
      (should (= 0 (cdr (assq 'missed-polls entry)))))))

(ert-deftest test-shipit--count-unread-activities-subtracts-snooze-and-locally-read ()
  "GIVEN three GitHub activities in the global hash, all with `unread=t',
WHEN one is snoozed and another is on the locally-marked-read tracker,
THEN `shipit--count-unread-activities' returns 1.
This is the regression for the modeline bell counting items the user
already dismissed -- previously the background poll's inline counter
forgot to subtract snoozes, so the bell drifted out of sync with the
buffer header on the manual-refresh path."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit-notifications--snoozes nil)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal)))
    (dolist (n '((1 . "id-1") (2 . "id-2") (3 . "id-3")))
      (let ((num (car n))
            (id (cdr n)))
        (puthash (format "owner/foo:pr:%d" num)
                 `((repo . "owner/foo")
                   (number . ,num)
                   (type . "pr")
                   (notification . ((id . ,id) (unread . t))))
                 shipit--notification-pr-activities)))
    (push (cons "owner/foo:pr:1" (+ (float-time) 3600))
          shipit-notifications--snoozes)
    (puthash "id-2" t shipit--locally-marked-read-notifications)
    (should (= 1 (shipit--count-unread-activities)))))

(ert-deftest test-shipit--count-unread-activities-counts-backend-and-permanent-snooze ()
  "GIVEN a backend activity (no `notification' alist) and a github
activity flagged with a `:permanent' snooze,
WHEN counting,
THEN the backend activity is counted, the permanent-snoozed one is not."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit-notifications--snoozes nil)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal)))
    (puthash "owner/foo:issue:42"
             '((repo . "owner/foo") (number . 42) (type . "issue"))
             shipit--notification-pr-activities)
    (puthash "owner/foo:pr:7"
             '((repo . "owner/foo") (number . 7) (type . "pr")
               (notification . ((id . "id-7") (unread . t))))
             shipit--notification-pr-activities)
    (push (cons "owner/foo:pr:7" :permanent) shipit-notifications--snoozes)
    (should (= 1 (shipit--count-unread-activities)))))

(ert-deftest test-shipit-notifications-buffer--unread-scope-hides-read-items ()
  "GIVEN the activity hash holds two GitHub items: one unread, one read
\(left over from a previous `all' scope fetch),
WHEN the buffer is in `unread' scope,
THEN only the unread item passes the repo-filtered-activities chain.
WHEN scope is flipped to `all',
THEN both items pass."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit-notifications--snoozes nil)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal)))
    (puthash "owner/foo:pr:1"
             '((repo . "owner/foo") (number . 1) (type . "pr")
               (notification . ((id . "id-1") (unread . t))))
             shipit--notification-pr-activities)
    (puthash "owner/foo:pr:2"
             '((repo . "owner/foo") (number . 2) (type . "pr")
               (notification . ((id . "id-2") (unread . :json-false))))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (let ((shipit-notifications-buffer--render-pool nil))
              (setq shipit-notifications-buffer--display-scope 'unread)
              (should (= 1 (length (shipit-notifications-buffer--repo-filtered-activities))))
              (setq shipit-notifications-buffer--display-scope 'all)
              (should (= 2 (length (shipit-notifications-buffer--repo-filtered-activities))))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer--unread-scope-passes-backend-activities ()
  "GIVEN a backend activity in the hash (no `notification' field),
WHEN the buffer is in `unread' scope,
THEN the backend activity passes the filter regardless of unread state
\(backends remove activities from the hash on mark)."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit-notifications--snoozes nil)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal)))
    (puthash "owner/foo:issue:42"
             '((repo . "owner/foo") (number . 42) (type . "issue"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (let ((shipit-notifications-buffer--render-pool nil))
              (setq shipit-notifications-buffer--display-scope 'unread)
              (should (= 1 (length (shipit-notifications-buffer--repo-filtered-activities))))))
        (kill-buffer buf)))))

(provide 'test-notifications-integration)
;;; test-notifications-integration.el ends here
