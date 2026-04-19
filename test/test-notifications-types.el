;;; test-notifications-types.el --- Tests for type-aware notifications -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for notification type handling (PR vs Issue), key format,
;; extraction helpers, and backend notification support.

;;; Code:

(require 'ert)
(require 'shipit-notifications)

;;; Key Helper Tests

(ert-deftest test-shipit--notification-activity-key-pr ()
  "GIVEN a PR notification
WHEN building activity key
THEN format is repo:pr:number."
  (should (equal "owner/repo:pr:42"
                 (shipit--notification-activity-key "owner/repo" "pr" 42))))

(ert-deftest test-shipit--notification-activity-key-issue ()
  "GIVEN an Issue notification
WHEN building activity key
THEN format is repo:issue:number."
  (should (equal "owner/repo:issue:42"
                 (shipit--notification-activity-key "owner/repo" "issue" 42))))

(ert-deftest test-shipit--notification-activity-key-distinct ()
  "GIVEN same repo and number but different types
WHEN building activity keys
THEN keys are distinct (no collision)."
  (let ((pr-key (shipit--notification-activity-key "owner/repo" "pr" 42))
        (issue-key (shipit--notification-activity-key "owner/repo" "issue" 42)))
    (should-not (equal pr-key issue-key))))

;;; Extraction Helper Tests

(ert-deftest test-shipit--extract-notification-number-pr ()
  "GIVEN a PullRequest subject URL
WHEN extracting number
THEN returns the PR number."
  (should (= 123 (shipit--extract-notification-number
                   "https://api.github.com/repos/owner/repo/pulls/123"
                   "PullRequest"))))

(ert-deftest test-shipit--extract-notification-number-issue ()
  "GIVEN an Issue subject URL
WHEN extracting number
THEN returns the issue number."
  (should (= 456 (shipit--extract-notification-number
                   "https://api.github.com/repos/owner/repo/issues/456"
                   "Issue"))))

(ert-deftest test-shipit--extract-notification-number-invalid ()
  "GIVEN an invalid subject URL
WHEN extracting number
THEN returns nil."
  (should-not (shipit--extract-notification-number
               "https://api.github.com/repos/owner/repo/commits/abc"
               "Commit")))

;;; Type Mapping Tests

(ert-deftest test-shipit--notification-type-from-subject-pr ()
  "GIVEN PullRequest subject type
WHEN mapping to internal type
THEN returns \"pr\"."
  (should (equal "pr" (shipit--notification-type-from-subject "PullRequest"))))

(ert-deftest test-shipit--notification-type-from-subject-issue ()
  "GIVEN Issue subject type
WHEN mapping to internal type
THEN returns \"issue\"."
  (should (equal "issue" (shipit--notification-type-from-subject "Issue"))))

(ert-deftest test-shipit--notification-type-from-subject-unknown ()
  "GIVEN unknown subject type
WHEN mapping to internal type
THEN returns nil."
  (should-not (shipit--notification-type-from-subject "Release")))

;;; Mixed PR + Issue Processing Tests

(ert-deftest test-process-notifications-mixed-types ()
  "GIVEN notifications with both PR and Issue of same number
WHEN processing notifications
THEN both are stored with distinct keys."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--mention-prs '())
        (shipit--mention-count 0)
        (shipit--last-notification-count 0)
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit-notification-alert-backend nil)
        (shipit-notifications-enabled t)
        (shipit-notifications-visual-indicator 'modeline)
        (notifications
         (list
          ;; PR notification
          `((id . "notif-1")
            (subject . ((type . "PullRequest")
                        (title . "PR Title")
                        (url . "https://api.github.com/repos/owner/repo/pulls/42")))
            (reason . "review_requested")
            (unread . t)
            (repository . ((full_name . "owner/repo")))
            (updated_at . "2025-01-29T10:00:00Z"))
          ;; Issue notification with same number
          `((id . "notif-2")
            (subject . ((type . "Issue")
                        (title . "Issue Title")
                        (url . "https://api.github.com/repos/owner/repo/issues/42")))
            (reason . "mention")
            (unread . t)
            (repository . ((full_name . "owner/repo")))
            (updated_at . "2025-01-29T11:00:00Z")))))

    (shipit--process-notifications notifications)

    ;; THEN both entries exist with distinct keys
    (should (= 2 (hash-table-count shipit--notification-pr-activities)))
    (should (gethash "owner/repo:pr:42" shipit--notification-pr-activities))
    (should (gethash "owner/repo:issue:42" shipit--notification-pr-activities))

    ;; Verify types are correctly set
    (let ((pr-activity (gethash "owner/repo:pr:42" shipit--notification-pr-activities))
          (issue-activity (gethash "owner/repo:issue:42" shipit--notification-pr-activities)))
      (should (equal "pr" (cdr (assq 'type pr-activity))))
      (should (equal "issue" (cdr (assq 'type issue-activity))))
      (should (equal "PR Title" (cdr (assq 'subject pr-activity))))
      (should (equal "Issue Title" (cdr (assq 'subject issue-activity)))))))

;;; Buffer Type Property Tests

(ert-deftest test-notifications-buffer-renders-type-property ()
  "GIVEN a notification with type field
WHEN rendered in the buffer
THEN section value contains the correct type."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:issue:99"
             '((repo . "owner/repo")
               (number . 99)
               (type . "issue")
               (subject . "Bug report")
               (reason . "mention")
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "#99")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (equal "issue" (cdr (assq 'type activity)))))))
        (kill-buffer buf)))))

;;; Action Menu Type Tests

(ert-deftest test-notification-actions-pr-type ()
  "GIVEN a PR notification at point
WHEN getting section data
THEN type is \"pr\"."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:pr:42"
             '((repo . "owner/repo")
               (number . 42)
               (type . "pr")
               (subject . "PR Title")
               (reason . "review_requested")
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "#42")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (equal "pr" (cdr (assq 'type activity)))))))
        (kill-buffer buf)))))

(ert-deftest test-notification-actions-issue-type ()
  "GIVEN an Issue notification at point
WHEN getting section data
THEN type is \"issue\"."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:issue:42"
             '((repo . "owner/repo")
               (number . 42)
               (type . "issue")
               (subject . "Issue Title")
               (reason . "mention")
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "#42")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (equal "issue" (cdr (assq 'type activity)))))))
        (kill-buffer buf)))))

(ert-deftest test-notification-buffer-stores-backend-text-properties ()
  "GIVEN a Jira notification with backend-id and backend-config
WHEN rendered in the buffer
THEN text has shipit-notification-backend-id and backend-config properties."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (config '(:base-url "https://jira.example.com" :project-keys ("PRJ"))))
    (puthash "MyJira:issue:PRJ-42"
             `((repo . "MyJira")
               (number . "PRJ-42")
               (type . "issue")
               (subject . "Bug report")
               (reason . "updated")
               (source . jira)
               (backend-id . jira)
               (backend-config . ,config)
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "PRJ-42")
              (let* ((section (magit-current-section))
                     (activity (oref section value)))
                (should (eq 'jira (cdr (assq 'backend-id activity))))
                (should (equal config (cdr (assq 'backend-config activity)))))))
        (kill-buffer buf)))))

;;; Backend Notification Support Tests

(ert-deftest test-shipit-issue--backend-has-notifications-p-true ()
  "GIVEN a backend plist with :notifications key
WHEN checking for notification support
THEN returns non-nil."
  (require 'shipit-issue-backends)
  (let ((plist '(:name "Test" :notifications #'ignore)))
    (should (shipit-issue--backend-has-notifications-p plist))))

(ert-deftest test-shipit-issue--backend-has-notifications-p-false ()
  "GIVEN a backend plist without :notifications key
WHEN checking for notification support
THEN returns nil."
  (require 'shipit-issue-backends)
  (let ((plist '(:name "Test")))
    (should-not (shipit-issue--backend-has-notifications-p plist))))

;;; Jira Notification JQL Tests

(ert-deftest test-jira-build-notifications-jql ()
  "GIVEN Jira config with project keys
WHEN building notifications JQL
THEN includes project filter and updated clause."
  (require 'shipit-issue-jira)
  (let ((config '(:base-url "https://jira.example.com" :project-keys ("PRJ" "TEAM")))
        (since-time "2025-01-29 10:00"))
    (let ((jql (shipit-issue-jira--build-notifications-jql config since-time)))
      (should (string-match-p "project in" jql))
      (should (string-match-p "PRJ" jql))
      (should (string-match-p "TEAM" jql))
      (should (string-match-p "updated >=" jql))
      (should (string-match-p "2025-01-29 10:00" jql)))))

(ert-deftest test-jira-issue-to-activity ()
  "GIVEN a raw Jira issue response
WHEN converting to activity alist
THEN has correct fields."
  (require 'shipit-issue-jira)
  (let* ((config '(:project-keys ("PRJ") :display-name "MyJira"))
         (issue '((key . "PRJ-42")
                  (fields . ((summary . "Fix the bug")
                             (updated . "2025-01-29T12:00:00.000+0000")))))
         (activity (shipit-issue-jira--issue-to-activity config issue)))
    (should (equal "MyJira" (cdr (assq 'repo activity))))
    (should (equal "PRJ-42" (cdr (assq 'number activity))))
    (should (equal "issue" (cdr (assq 'type activity))))
    (should (equal "Fix the bug" (cdr (assq 'subject activity))))
    (should (equal "updated" (cdr (assq 'reason activity))))))

(ert-deftest test-jira-issue-to-activity-includes-backend-info ()
  "GIVEN a raw Jira issue response and config
WHEN converting to activity alist
THEN includes backend-id and backend-config for buffer routing."
  (require 'shipit-issue-jira)
  (let* ((config '(:project-keys ("PRJ") :display-name "MyJira"
                   :base-url "https://jira.example.com"))
         (issue '((key . "PRJ-42")
                  (fields . ((summary . "Fix the bug")
                             (updated . "2025-01-29T12:00:00.000+0000")))))
         (activity (shipit-issue-jira--issue-to-activity config issue)))
    (should (eq 'jira (cdr (assq 'backend-id activity))))
    (should (equal config (cdr (assq 'backend-config activity))))))

(ert-deftest test-jira-notifications-since-time ()
  "GIVEN a since timestamp
WHEN computing notifications since time
THEN subtracts 5 minute overlap margin."
  (require 'shipit-issue-jira)
  ;; With nil since, should return roughly 1 hour ago
  (let ((result (shipit-issue-jira--notifications-since-time nil)))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}$" result))))

;;; Modeline Icon Preservation Tests

(ert-deftest test-modeline-indicator-uses-emoji-bell ()
  "GIVEN notifications enabled with no mentions
WHEN shipit--update-modeline-indicator builds the modeline string
THEN the string contains the bell emoji and count."
  (let ((shipit-notifications-enabled t)
        (shipit--mention-count 0)
        (shipit--modeline-string nil)
        (shipit--notification-count 0))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (shipit--update-modeline-indicator 5)
      (should shipit--modeline-string)
      (should (string-match-p "🔔" shipit--modeline-string))
      (should (string-match-p "5" shipit--modeline-string)))))

(ert-deftest test-modeline-indicator-with-mentions-uses-emoji-bell ()
  "GIVEN non-zero mention count
WHEN shipit--update-modeline-indicator builds the modeline string
THEN the string contains @ mention count, bell emoji, and total count."
  (let ((shipit-notifications-enabled t)
        (shipit--mention-count 3)
        (shipit--modeline-string nil)
        (shipit--notification-count 0))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      (shipit--update-modeline-indicator 10)
      (should shipit--modeline-string)
      (should (string-match-p "@3" shipit--modeline-string))
      (should (string-match-p "🔔" shipit--modeline-string))
      (should (string-match-p "10" shipit--modeline-string)))))

(ert-deftest test-modeline-indicator-with-emoji-fallback ()
  "GIVEN an emoji fallback icon (no display property)
WHEN shipit--update-modeline-indicator builds the modeline string
THEN the emoji text is present in the result."
  (let ((shipit-notifications-enabled t)
        (shipit--mention-count 0)
        (shipit--modeline-string nil)
        (shipit--notification-count 0))
    (cl-letf (((symbol-function 'shipit--get-notification-icon)
               (lambda (emoji) emoji))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (shipit--update-modeline-indicator 7)
      ;; THEN modeline string contains the count
      (should shipit--modeline-string)
      (should (string-match-p "7" shipit--modeline-string)))))

;;; Activity Section Rendering Tests

(ert-deftest test-issue-activity-section-renders-changelog ()
  "GIVEN issue data with changelog entries
WHEN rendering the activity section
THEN changelog events appear with author and change description."
  (require 'shipit-issues-buffer)
  (let ((issue-data
         `((id . "PRJ-42")
           (number . "PRJ-42")
           (title . "Test issue")
           (state . "In Progress")
           (body . "test body")
           (user . ((login . "Alice") (avatar_url . nil)))
           (labels . ())
           (created_at . "2024-01-15T10:30:00.000+0000")
           (updated_at . "2024-01-20T14:00:00.000+0000")
           (changelog . (((id . "100")
                           (created_at . "2024-01-16T10:00:00.000+0000")
                           (user . ((login . "Alice") (avatar_url . nil)))
                           (items . (((field . "status")
                                      (from . "To Do")
                                      (to . "In Progress")))))
                          ((id . "101")
                           (created_at . "2024-01-17T11:00:00.000+0000")
                           (user . ((login . "Bob") (avatar_url . nil)))
                           (items . (((field . "assignee")
                                      (from . "")
                                      (to . "Alice"))))))))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (_root 'root)
          (shipit-issue--insert-activity-section "test/repo" issue-data)))
      ;; THEN buffer contains Activity heading with count
      (goto-char (point-min))
      (should (search-forward "Activity" nil t))
      ;; THEN assignee change is shown (newest first)
      (should (search-forward "Assignee" nil t))
      ;; THEN status change is shown
      (should (search-forward "Status" nil t)))))

(ert-deftest test-issue-activity-section-empty-state-without-changelog ()
  "GIVEN issue data without changelog key and loading-p=nil
WHEN rendering the activity section
THEN Activity (0) heading is shown with empty state message.
Callers that will fetch asynchronously pass loading-p=t and get a
loading placeholder instead; see
`test-issue-activity-section-loading-state-when-async'."
  (require 'shipit-issues-buffer)
  (let ((issue-data
         '((id . "PRJ-42")
           (number . "PRJ-42")
           (title . "Test issue")
           (state . "Open")
           (body . "test")
           (user . ((login . "Alice") (avatar_url . nil)))
           (labels . ())
           (created_at . "2024-01-15T10:30:00.000+0000")
           (updated_at . "2024-01-15T10:30:00.000+0000"))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (_root 'root)
          (shipit-issue--insert-activity-section "test/repo" issue-data nil))
        (goto-char (point-min))
        (should (search-forward "Activity (0)" nil t))
        (should (search-forward "No activity yet" nil t))))))

(ert-deftest test-issue-activity-section-loading-state-when-async ()
  "GIVEN issue data without changelog key and loading-p=t
WHEN rendering the activity section
THEN the heading shows a loading indicator and the body says
\"Loading activity…\" — the async events fetch will replace this
placeholder once it completes."
  (require 'shipit-issues-buffer)
  (let ((issue-data
         '((id . "42")
           (number . 42)
           (title . "Popular issue")
           (state . "Open")
           (body . "test"))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (magit-insert-section (_root 'root)
          (shipit-issue--insert-activity-section "owner/repo" issue-data t))
        (goto-char (point-min))
        (should (search-forward "Activity (…)" nil t))
        (goto-char (point-min))
        (should (search-forward "Loading activity" nil t))
        (goto-char (point-min))
        (should-not (search-forward "No activity yet" nil t))))))

(ert-deftest test-issue-format-changelog-items-status-change ()
  "GIVEN a changelog item for status change
WHEN formatting
THEN shows 'changed Status: X -> Y'."
  (require 'shipit-issues-buffer)
  (let ((items '(((field . "status") (from . "To Do") (to . "In Progress")))))
    (let ((result (shipit-issue--format-changelog-items items)))
      (should (string-match-p "Status" result))
      (should (string-match-p "To Do" result))
      (should (string-match-p "In Progress" result)))))

(ert-deftest test-issue-format-changelog-items-description-change ()
  "GIVEN a changelog item for description change
WHEN formatting
THEN shows 'updated Description' without from/to."
  (require 'shipit-issues-buffer)
  (let ((items '(((field . "description") (from . "old text") (to . "new text")))))
    (let ((result (shipit-issue--format-changelog-items items)))
      (should (string-match-p "Description" result))
      (should-not (string-match-p "old text" result)))))

;;; Source Icon Tests

(ert-deftest test-shipit-notification-source-icon-gitlab ()
  "GIVEN source is gitlab (non-graphic mode)
WHEN getting source icon
THEN returns \"GL\" with GitLab orange color."
  (require 'shipit-render)
  (let ((icon (shipit--get-notification-source-icon 'gitlab)))
    (should (string-match-p "GL" icon))))

(ert-deftest test-shipit-notification-source-icon-jira ()
  "GIVEN source is jira (non-graphic mode)
WHEN getting source icon
THEN returns \"JR\" with Jira blue color."
  (require 'shipit-render)
  (let ((icon (shipit--get-notification-source-icon 'jira)))
    (should (string-match-p "JR" icon))))

(ert-deftest test-shipit-notification-source-icon-github ()
  "GIVEN source is nil (GitHub, the default)
WHEN getting source icon
THEN returns \"GH\"."
  (require 'shipit-render)
  (let ((icon (shipit--get-notification-source-icon nil)))
    (should (string-match-p "GH" icon))))

(ert-deftest test-shipit-notification-buffer-shows-source-icon ()
  "GIVEN a GitLab notification with source=gitlab
WHEN rendered in the buffer
THEN the line contains \"GL\" source indicator."
  (require 'shipit-notifications-buffer)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "g/p:issue:5"
             '((repo . "g/p")
               (number . 5)
               (type . "issue")
               (subject . "GitLab issue")
               (reason . "assign")
               (source . gitlab)
               (backend-id . gitlab)
               (updated-at . "2026-02-15T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer--rerender)
            (with-current-buffer buf
              (goto-char (point-min))
              ;; Should contain GL source indicator
              (should (search-forward "GL" nil t))))
        (kill-buffer buf)))))

(ert-deftest test-notification-activity-section-type ()
  "GIVEN the notification-activity section type
   WHEN checking its magit-section property
   THEN it is registered as a magit section."
  (should (get 'notification-activity 'magit-section)))

;;; Activity Timeline Filter Tests

(ert-deftest test-filter-events-since-timestamp ()
  "GIVEN a list of timeline events and a cutoff timestamp
   WHEN shipit-notifications-buffer--filter-events-since is called
   THEN only events at or after the cutoff are returned."
  (require 'shipit-notifications-buffer)
  (let ((events '(((event . "commented") (created_at . "2026-03-20T10:00:00Z"))
                  ((event . "reviewed") (created_at . "2026-03-21T14:00:00Z"))
                  ((event . "commented") (created_at . "2026-03-22T08:00:00Z"))))
        (cutoff "2026-03-21T12:00:00Z"))
    (let ((filtered (shipit-notifications-buffer--filter-events-since events cutoff)))
      (should (= (length filtered) 2))
      (should (equal "2026-03-21T14:00:00Z"
                     (cdr (assq 'created_at (car filtered))))))))

(ert-deftest test-filter-events-since-uses-submitted-at ()
  "GIVEN events with submitted_at instead of created_at
   WHEN filtering since a cutoff
   THEN submitted_at is used for comparison."
  (require 'shipit-notifications-buffer)
  (let ((events '(((event . "reviewed") (submitted_at . "2026-03-22T08:00:00Z"))))
        (cutoff "2026-03-21T00:00:00Z"))
    (let ((filtered (shipit-notifications-buffer--filter-events-since events cutoff)))
      (should (= (length filtered) 1)))))

(ert-deftest test-filter-events-since-empty ()
  "GIVEN events all before the cutoff
   WHEN filtering
   THEN empty list is returned."
  (require 'shipit-notifications-buffer)
  (let ((events '(((event . "commented") (created_at . "2026-03-20T10:00:00Z"))))
        (cutoff "2026-03-21T00:00:00Z"))
    (let ((filtered (shipit-notifications-buffer--filter-events-since events cutoff)))
      (should (= (length filtered) 0)))))

;;; Activity Sub-Section Insertion Tests

(ert-deftest test-insert-notification-activity-section ()
  "GIVEN timeline events and a cutoff timestamp
   WHEN shipit-notifications-buffer--insert-activity-section is called
   THEN a notification-activity magit section is created with events."
  (require 'shipit-notifications-buffer)
  (require 'shipit-pr-sections)
  (let ((events '(((event . "commented")
                   (created_at . "2026-03-22T08:00:00Z")
                   (user . ((login . "alice")))
                   (body . "looks good"))
                  ((event . "reviewed")
                   (state . "approved")
                   (submitted_at . "2026-03-22T09:00:00Z")
                   (user . ((login . "bob"))))))
        (cutoff "2026-03-22T00:00:00Z"))
    (with-temp-buffer
      (magit-insert-section (notifications-root)
        (magit-insert-section (notification-entry nil)
          (magit-insert-heading "Test notification")
          (let ((magit-insert-section--parent
                 magit-insert-section--current))
            (shipit-notifications-buffer--insert-activity-section
             events cutoff "owner/repo" 42))))
      (goto-char (point-min))
      ;; Find the notification-activity section
      (let ((found nil))
        (dolist (child (oref (car (oref magit-root-section children)) children))
          (when (eq (oref child type) 'notification-activity)
            (setq found t)))
        (should found)))))

(provide 'test-notifications-types)
;;; test-notifications-types.el ends here
