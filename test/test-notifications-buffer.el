;;; test-notifications-buffer.el --- Tests for notifications buffer -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'shipit-notifications-buffer)

(ert-deftest test-shipit-notifications-buffer-creation ()
  "Test that notifications buffer is created with correct mode."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (eq major-mode 'shipit-notifications-buffer-mode))
          (should (string= (buffer-name) "*shipit-notifications*")))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-singleton ()
  "Test that buffer is a singleton - same buffer returned each time."
  (let ((buf1 (shipit-notifications-buffer-create))
        (buf2 (shipit-notifications-buffer-create)))
    (unwind-protect
        (should (eq buf1 buf2))
      (kill-buffer buf1))))

(ert-deftest test-shipit-notifications-buffer-renders-notifications ()
  "Test that buffer renders notifications from cache."
  ;; Setup mock notification data
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:pr:123"
             '((repo . "owner/repo")
               (number . 123)
               (type . "pr")
               (subject . "Fix the bug")
               (reason . "review_requested")
               (updated-at . "2025-01-29T10:00:00Z"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer-refresh)
            (with-current-buffer buf
              (should (string-match-p "owner/repo" (buffer-string)))
              (should (string-match-p "#123" (buffer-string)))
              (should (string-match-p "Fix the bug" (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-get-notification-at-point ()
  "Test getting notification data at point."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "owner/repo:pr:123"
             '((repo . "owner/repo")
               (number . 123)
               (type . "pr")
               (subject . "Test PR"))
             shipit--notification-pr-activities)
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (progn
            (shipit-notifications-buffer-refresh)
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "#123")
              (let ((data (shipit-notifications-buffer--get-notification-at-point)))
                (should data)
                (should (equal (plist-get data :repo) "owner/repo"))
                (should (equal (plist-get data :pr-number) 123)))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-view-notifications-opens-buffer ()
  "Test that view command opens the notifications buffer."
  (unwind-protect
      (progn
        (shipit--view-notifications)
        (should (string= (buffer-name) "*shipit-notifications*"))
        (should (eq major-mode 'shipit-notifications-buffer-mode)))
    (when (get-buffer "*shipit-notifications*")
      (kill-buffer "*shipit-notifications*"))))

(ert-deftest test-shipit-notifications-buffer-f-binds-filter-menu ()
  "Test that `f' opens the filter transient, not the text-filter minibuffer.
GIVEN a notifications buffer
WHEN we look up the binding of `f'
THEN it should resolve to `shipit-notifications-buffer-filter-menu',
     since the text filter is now one entry inside the transient."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (eq (lookup-key shipit-notifications-buffer-mode-map "f")
                      'shipit-notifications-buffer-filter-menu)))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-filter-menu-defined ()
  "Test that the filter transient is defined and interactive.
GIVEN the notifications-buffer module is loaded
THEN `shipit-notifications-buffer-filter-menu' should be a command."
  (should (fboundp 'shipit-notifications-buffer-filter-menu))
  (should (commandp 'shipit-notifications-buffer-filter-menu)))

(ert-deftest test-shipit-notifications-buffer-display-scope-default-unread ()
  "Test that the buffer-local display scope defaults to `unread'.
GIVEN a fresh notifications buffer
THEN `shipit-notifications-buffer--display-scope' should be `unread'."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (eq shipit-notifications-buffer--display-scope 'unread)))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-page-limit-default-1 ()
  "Test that the buffer-local page limit defaults to 1.
GIVEN a fresh notifications buffer
THEN `shipit-notifications-buffer--current-page' should be 1."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (= shipit-notifications-buffer--current-page 1)))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-toggle-scope-flips ()
  "Test that toggle-scope flips between `unread' and `all' and resets pages.
GIVEN a buffer in `unread' scope with page-limit > 1
WHEN toggle-scope is called
THEN scope flips to `all' and page-limit resets to 1.
WHEN called again
THEN scope flips back to `unread'."
  (cl-letf (((symbol-function 'shipit-notifications-buffer-refresh)
             (lambda (&rest _args) nil)))
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--display-scope 'unread
                  shipit-notifications-buffer--current-page 3)
            (shipit-notifications-buffer-toggle-scope)
            (should (eq shipit-notifications-buffer--display-scope 'all))
            (should (= shipit-notifications-buffer--current-page 1))
            (shipit-notifications-buffer-toggle-scope)
            (should (eq shipit-notifications-buffer--display-scope 'unread))
            (should (= shipit-notifications-buffer--current-page 1)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-load-more-increments ()
  "Test that load-more bumps page-limit when scope is `all'.
GIVEN a buffer in `all' scope with page-limit=1
WHEN load-more is called
THEN page-limit becomes 2."
  (cl-letf (((symbol-function 'shipit-notifications-buffer-refresh)
             (lambda (&rest _args) nil)))
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--display-scope 'all
                  shipit-notifications-buffer--current-page 1)
            (shipit-notifications-buffer-load-more)
            (should (= shipit-notifications-buffer--current-page 2))
            (shipit-notifications-buffer-load-more)
            (should (= shipit-notifications-buffer--current-page 3)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-load-more-noop-in-unread ()
  "Test that load-more does nothing when scope is `unread'.
GIVEN a buffer in `unread' scope
WHEN load-more is called
THEN page-limit stays at 1 and user-error is signalled."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq shipit-notifications-buffer--display-scope 'unread
                shipit-notifications-buffer--current-page 1)
          (should-error (shipit-notifications-buffer-load-more)
                        :type 'user-error)
          (should (= shipit-notifications-buffer--current-page 1)))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-total-count-default-nil ()
  "GIVEN a fresh notifications buffer
THEN the buffer-local total-count is nil (unknown until probed)."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (null shipit-notifications-buffer--total-count)))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-header-shows-page-of-total-pages ()
  "GIVEN total-count=250 in all scope at page 2
WHEN the buffer is rendered
THEN the header shows `page: 2/3' (250/100 per-page = 3 pages total)."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--display-scope 'all
                  shipit-notifications-buffer--current-page 2
                  shipit-notifications-buffer--total-count 250)
            (shipit-notifications-buffer--rerender)
            (let ((header (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) 200))))
              (should (string-match-p "page: 2/3" header))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-header-page-uses-constant-face ()
  "GIVEN buffer in `all' scope at page 2 of 3
WHEN the header is rendered
THEN the `2/3' page number is propertized with
`font-lock-constant-face' (and not `font-lock-comment-face')."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--display-scope 'all
                  shipit-notifications-buffer--current-page 2
                  shipit-notifications-buffer--total-count 250)
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (should (search-forward "2/3" nil t))
            (should (eq (get-text-property (1- (point)) 'font-lock-face)
                        'font-lock-constant-face)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-header-before-uses-timestamp-face ()
  "GIVEN buffer with a `before' timestamp filter set
WHEN the header is rendered
THEN the timestamp value is propertized with `shipit-timestamp-face'."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--before-filter
                  "2026-01-15T00:00:00Z")
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (should (search-forward "2026-01-15T00:00:00Z" nil t))
            (should (eq (get-text-property (1- (point)) 'font-lock-face)
                        'shipit-timestamp-face)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-header-since-uses-timestamp-face ()
  "GIVEN buffer with a `since' timestamp filter set
WHEN the header is rendered
THEN the timestamp value is propertized with `shipit-timestamp-face'."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--since-filter
                  "2025-12-01T00:00:00Z")
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (should (search-forward "2025-12-01T00:00:00Z" nil t))
            (should (eq (get-text-property (1- (point)) 'font-lock-face)
                        'shipit-timestamp-face)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-header-shows-shown-over-total ()
  "GIVEN a buffer with total-count=666 and 2 activities in the hash
WHEN the buffer is rendered
THEN the header shows `2/666' (shown / total)."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/repo:pr:1"
               (quote ((repo . "owner/repo") (number . 1) (type . "pr")
                       (subject . "A") (reason . "mention")
                       (updated-at . "2026-01-01T00:00:00Z")))
               shipit--notification-pr-activities)
      (puthash "owner/repo:pr:2"
               (quote ((repo . "owner/repo") (number . 2) (type . "pr")
                       (subject . "B") (reason . "mention")
                       (updated-at . "2026-01-02T00:00:00Z")))
               shipit--notification-pr-activities)
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--display-scope 'all
                  shipit-notifications-buffer--current-page 1
                  shipit-notifications-buffer--total-count 666)
            (shipit-notifications-buffer--rerender)
            (let ((header (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) 200))))
              (should (string-match-p "2/666" header))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-header-filter-changes-denominator ()
  "Test that the denominator follows the filter.
GIVEN a buffer with total-count=666, 2 activities in the hash, and
a filter that matches only 1 of them
WHEN the buffer is rendered
THEN the header shows `1/2' — matches over loaded — not
`1/666', because the filter can only see locally-loaded items."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/match-repo:pr:1"
               (quote ((repo . "owner/match-repo") (number . 1) (type . "pr")
                       (subject . "A") (reason . "mention")
                       (updated-at . "2026-01-01T00:00:00Z")))
               shipit--notification-pr-activities)
      (puthash "owner/other-repo:pr:2"
               (quote ((repo . "owner/other-repo") (number . 2) (type . "pr")
                       (subject . "B") (reason . "mention")
                       (updated-at . "2026-01-02T00:00:00Z")))
               shipit--notification-pr-activities)
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--display-scope 'all
                  shipit-notifications-buffer--current-page 1
                  shipit-notifications-buffer--total-count 666
                  shipit-notifications-buffer--filter-text "match-repo")
            (shipit-notifications-buffer--rerender)
            (let ((header (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) 200))))
              (should (string-match-p "1/2 (of 666)" header))
              (should-not (string-match-p "1/666" header))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-repo-filter-default-nil ()
  "GIVEN a fresh notifications buffer
THEN `shipit-notifications-buffer--repo-filter' is nil (no filter)."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (null shipit-notifications-buffer--repo-filter)))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-repo-filter-in-header ()
  "GIVEN a buffer with `shipit-notifications-buffer--repo-filter' set
WHEN the buffer is rendered
THEN the header includes `[repo: owner/foo]'."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--repo-filter "owner/foo")
            (shipit-notifications-buffer--rerender)
            (let ((header (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) 200))))
              (should (string-match-p "\[repo: owner/foo\]" header))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-clear-repo-filter ()
  "GIVEN a buffer with a repo filter set
WHEN `shipit-notifications-buffer-clear-repo-filter' is invoked
THEN the filter is cleared to nil."
  (cl-letf (((symbol-function 'shipit-notifications-buffer-refresh)
             (lambda (&rest _args) nil)))
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--repo-filter "owner/foo")
            (shipit-notifications-buffer-clear-repo-filter)
            (should (null shipit-notifications-buffer--repo-filter)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-clear-all-filters ()
  "GIVEN a buffer with text, repo, type, before, and since filters set
WHEN `shipit-notifications-buffer-clear-all-filters' is invoked
THEN every filter is cleared, current-page resets to 1, and a refresh
is triggered."
  (let ((refresh-calls 0))
    (cl-letf (((symbol-function 'shipit-notifications-buffer-refresh)
               (lambda (&rest _args) (cl-incf refresh-calls))))
      (let ((buf (shipit-notifications-buffer-create)))
        (unwind-protect
            (with-current-buffer buf
              (setq shipit-notifications-buffer--filter-text "needle")
              (setq shipit-notifications-buffer--repo-filter "owner/foo")
              (setq shipit-notifications-buffer--type-filter "pr")
              (setq shipit-notifications-buffer--before-filter
                    "2026-01-01T00:00:00Z")
              (setq shipit-notifications-buffer--since-filter
                    "2025-12-01T00:00:00Z")
              (setq shipit-notifications-buffer--current-page 5)
              (shipit-notifications-buffer-clear-all-filters)
              (should (string-empty-p
                       shipit-notifications-buffer--filter-text))
              (should (null shipit-notifications-buffer--repo-filter))
              (should (null shipit-notifications-buffer--type-filter))
              (should (null shipit-notifications-buffer--before-filter))
              (should (null shipit-notifications-buffer--since-filter))
              (should (= shipit-notifications-buffer--current-page 1))
              (should (= refresh-calls 1)))
          (kill-buffer buf))))))

(ert-deftest test-shipit-notifications-buffer-state-filter-predicate-open ()
  "GIVEN state filter set to `open'
WHEN the predicate runs against PR activities of various states
THEN open-non-draft passes; merged/closed/draft are excluded;
non-PR activities (issues, workflows) are also excluded so the
filter does not silently let stateless rows through."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq shipit-notifications-buffer--state-filter "open")
          (should (shipit-notifications-buffer--matches-state-filter-p
                   '((type . "pr") (pr-state . "open"))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "pr") (pr-state . "open") (draft . t))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "pr") (pr-state . "merged"))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "pr") (pr-state . "closed"))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "issue") (number . 1))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "workflow")))))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-state-filter-predicate-draft ()
  "GIVEN state filter set to `draft'
WHEN the predicate runs
THEN only PRs with `draft' = t and `pr-state' = `open' pass."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq shipit-notifications-buffer--state-filter "draft")
          (should (shipit-notifications-buffer--matches-state-filter-p
                   '((type . "pr") (pr-state . "open") (draft . t))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "pr") (pr-state . "open"))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "pr") (pr-state . "merged") (draft . t)))))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-state-filter-predicate-merged-and-closed ()
  "GIVEN state filter set to `merged' (or `closed')
WHEN the predicate runs
THEN only PRs whose `pr-state' equals the filter pass."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq shipit-notifications-buffer--state-filter "merged")
          (should (shipit-notifications-buffer--matches-state-filter-p
                   '((type . "pr") (pr-state . "merged"))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "pr") (pr-state . "closed"))))
          (setq shipit-notifications-buffer--state-filter "closed")
          (should (shipit-notifications-buffer--matches-state-filter-p
                   '((type . "pr") (pr-state . "closed"))))
          (should-not (shipit-notifications-buffer--matches-state-filter-p
                       '((type . "pr") (pr-state . "merged")))))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-state-filter-nil-passes-everything ()
  "GIVEN no state filter set
WHEN the predicate runs against any activity
THEN it always returns non-nil."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (setq shipit-notifications-buffer--state-filter nil)
          (should (shipit-notifications-buffer--matches-state-filter-p
                   '((type . "pr") (pr-state . "merged"))))
          (should (shipit-notifications-buffer--matches-state-filter-p
                   '((type . "issue")))))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-clear-state-filter ()
  "GIVEN buffer with a state filter set
WHEN `shipit-notifications-buffer-clear-state-filter' is invoked
THEN the filter is cleared to nil."
  (cl-letf (((symbol-function 'shipit-notifications-buffer-refresh)
             (lambda (&rest _args) nil)))
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--state-filter "merged")
            (shipit-notifications-buffer-clear-state-filter)
            (should (null shipit-notifications-buffer--state-filter)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-state-filter-in-clear-all ()
  "GIVEN clear-all-filters invoked with state filter set
WHEN it returns
THEN the state filter is also reset to nil."
  (cl-letf (((symbol-function 'shipit-notifications-buffer-refresh)
             (lambda (&rest _args) nil)))
    (let ((buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--state-filter "merged")
            (shipit-notifications-buffer-clear-all-filters)
            (should (null shipit-notifications-buffer--state-filter)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-header-state-uses-keyword-face ()
  "GIVEN buffer with a state filter set
WHEN the header is rendered
THEN the state value uses `font-lock-keyword-face'."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--state-filter "merged")
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (should (search-forward "merged" nil t))
            (should (eq (get-text-property (1- (point)) 'font-lock-face)
                        'font-lock-keyword-face)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-mark-resolved-read-marks-merged-and-closed ()
  "GIVEN 1 open PR, 1 merged PR, 1 closed PR, 1 issue
WHEN `shipit-notifications-buffer-mark-resolved-read' is invoked
and the user confirms
THEN `shipit--mark-notification-read' is called for the merged and
closed PRs only — open PR and issue are left untouched."
  (let ((marked '())
        (refresh-calls 0))
    (cl-letf (((symbol-function 'shipit--mark-notification-read)
               (lambda (number repo &optional _no-refresh type)
                 (push (list number repo type) marked)))
              ((symbol-function 'shipit-notifications-buffer-refresh)
               (lambda (&rest _args) (cl-incf refresh-calls)))
              ((symbol-function 'yes-or-no-p) (lambda (_p) t)))
      (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
            (buf (shipit-notifications-buffer-create)))
        (puthash "owner/foo:pr:1"
                 '((repo . "owner/foo") (number . 1) (type . "pr")
                   (subject . "Open PR") (pr-state . "open"))
                 shipit--notification-pr-activities)
        (puthash "owner/foo:pr:2"
                 '((repo . "owner/foo") (number . 2) (type . "pr")
                   (subject . "Merged PR") (pr-state . "merged"))
                 shipit--notification-pr-activities)
        (puthash "owner/foo:pr:3"
                 '((repo . "owner/foo") (number . 3) (type . "pr")
                   (subject . "Closed PR") (pr-state . "closed"))
                 shipit--notification-pr-activities)
        (puthash "owner/foo:issue:4"
                 '((repo . "owner/foo") (number . 4) (type . "issue")
                   (subject . "An issue"))
                 shipit--notification-pr-activities)
        (unwind-protect
            (with-current-buffer buf
              (shipit-notifications-buffer-mark-resolved-read)
              (should (= (length marked) 2))
              (should (member '(2 "owner/foo" "pr") marked))
              (should (member '(3 "owner/foo" "pr") marked))
              (should-not (member '(1 "owner/foo" "pr") marked))
              (should (= refresh-calls 1)))
          (kill-buffer buf))))))

(ert-deftest test-shipit-notifications-buffer-mark-resolved-read-no-resolved-no-prompt ()
  "GIVEN no merged or closed PRs in the hash
WHEN `shipit-notifications-buffer-mark-resolved-read' is invoked
THEN no prompt is shown, no marks are performed, and a message
informs the user nothing was resolved."
  (let ((prompted nil)
        (marked '()))
    (cl-letf (((symbol-function 'shipit--mark-notification-read)
               (lambda (&rest args) (push args marked)))
              ((symbol-function 'yes-or-no-p)
               (lambda (_p) (setq prompted t) t)))
      (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
            (buf (shipit-notifications-buffer-create)))
        (puthash "owner/foo:pr:1"
                 '((repo . "owner/foo") (number . 1) (type . "pr")
                   (pr-state . "open"))
                 shipit--notification-pr-activities)
        (unwind-protect
            (with-current-buffer buf
              (shipit-notifications-buffer-mark-resolved-read)
              (should-not prompted)
              (should (null marked)))
          (kill-buffer buf))))))

(ert-deftest test-shipit-notifications-buffer-state-filter-drops-non-matching ()
  "GIVEN 1 open PR, 1 merged PR, 1 issue and state-filter `merged'
WHEN the buffer is rendered
THEN only the merged PR appears.  The open PR and the issue are
both hidden — picking a state means only PRs in that state, so
stateless rows do not silently bypass the filter."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr")
                 (subject . "Open PR") (reason . "mention")
                 (pr-state . "open") (updated-at . "2026-01-01T00:00:00Z"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:pr:2"
               '((repo . "owner/foo") (number . 2) (type . "pr")
                 (subject . "Merged PR") (reason . "mention")
                 (pr-state . "merged") (updated-at . "2026-01-02T00:00:00Z"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:issue:3"
               '((repo . "owner/foo") (number . 3) (type . "issue")
                 (subject . "Some issue") (reason . "mention")
                 (updated-at . "2026-01-03T00:00:00Z"))
               shipit--notification-pr-activities)
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--state-filter "merged")
            (shipit-notifications-buffer--rerender)
            (let ((body (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "Merged PR" body))
              (should-not (string-match-p "Open PR" body))
              (should-not (string-match-p "Some issue" body))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-repo-filter-drops-other-repos ()
  "GIVEN 2 GitHub activities in `owner/foo' and 3 Jira activities in `BAR'
and `shipit-notifications-buffer--repo-filter' set to `owner/foo'
WHEN the buffer is rendered
THEN both the header count and the rendered listing show 2/2 —
the 3 off-repo activities are excluded from both."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/foo:pr:1"
               (quote ((repo . "owner/foo") (number . 1) (type . "pr")
                       (subject . "A") (reason . "mention")
                       (updated-at . "2026-01-01T00:00:00Z")))
               shipit--notification-pr-activities)
      (puthash "owner/foo:pr:2"
               (quote ((repo . "owner/foo") (number . 2) (type . "pr")
                       (subject . "B") (reason . "mention")
                       (updated-at . "2026-01-02T00:00:00Z")))
               shipit--notification-pr-activities)
      (dotimes (i 3)
        (puthash (format "BAR:issue:%d" i)
                 (list (cons 'repo "BAR") (cons 'number i)
                       (cons 'type "issue") (cons 'subject (format "J%d" i))
                       (cons 'reason "mention")
                       (cons 'updated-at "2026-01-01T00:00:00Z")
                       (cons 'backend-id 'jira))
                 shipit--notification-pr-activities))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--repo-filter "owner/foo")
            (should (= 2 (shipit-notifications-buffer--loaded-count)))
            (should (= 2 (shipit-notifications-buffer--shown-count))))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-candidate-repos-from-hash ()
  "GIVEN activities in the hash from several repos and no watched-repos
WHEN gathering picker candidates
THEN distinct repos from the hash appear, sorted."
  (cl-letf (((symbol-function 'shipit-notifications-buffer--gather-watched-repos)
             (lambda () nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
      (puthash "owner/z:pr:1"
               (quote ((repo . "owner/z") (number . 1) (type . "pr")))
               shipit--notification-pr-activities)
      (puthash "owner/a:pr:2"
               (quote ((repo . "owner/a") (number . 2) (type . "pr")))
               shipit--notification-pr-activities)
      (puthash "owner/a:pr:3"
               (quote ((repo . "owner/a") (number . 3) (type . "pr")))
               shipit--notification-pr-activities)
      (let ((candidates (shipit-notifications-buffer--candidate-repos)))
        (should (equal '("owner/a" "owner/z") candidates))))))

(ert-deftest test-shipit-notifications-buffer-header-shows-scope ()
  "Test that the header reflects scope and page limit.
GIVEN a buffer in `all' scope with page-limit=2
WHEN the buffer is rendered
THEN the header mentions `all' and `page 2'."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (unwind-protect
          (with-current-buffer buf
            (setq shipit-notifications-buffer--display-scope 'all
                  shipit-notifications-buffer--current-page 2)
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (let ((header (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) 200))))
              (should (string-match-p "all" header))
              (should (string-match-p "page 2\\|pages 2\\|2 pages\\|page: 2" header))))
        (kill-buffer buf)))))

;;; Auto-mark rules editor

(ert-deftest test-shipit-notifications--save-auto-mark-rules-persists-and-sets ()
  "GIVEN a list of new rules
WHEN `shipit-notifications--save-auto-mark-rules' is invoked
THEN it sets the variable AND calls `customize-save-variable' so
the change persists across sessions."
  (let ((saved nil)
        (shipit-notifications-auto-mark-read-rules nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (sym val &rest _)
                 (setq saved (list sym val))
                 (set sym val))))
      (shipit-notifications--save-auto-mark-rules
       '((:state merged) (:title "x")))
      (should (equal saved
                     '(shipit-notifications-auto-mark-read-rules
                       ((:state merged) (:title "x")))))
      (should (equal shipit-notifications-auto-mark-read-rules
                     '((:state merged) (:title "x")))))))

(ert-deftest test-shipit-notifications-buffer--candidate-reasons-sorted-distinct ()
  "GIVEN activities with various reasons in the global hash
WHEN `--candidate-reasons' runs
THEN distinct reason strings are returned sorted ascending."
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (puthash "k1" '((reason . "subscribed")) shipit--notification-pr-activities)
    (puthash "k2" '((reason . "mention"))    shipit--notification-pr-activities)
    (puthash "k3" '((reason . "subscribed")) shipit--notification-pr-activities)
    (puthash "k4" '((reason . "team_mention")) shipit--notification-pr-activities)
    (should (equal (shipit-notifications-buffer--candidate-reasons)
                   '("mention" "subscribed" "team_mention")))))

(ert-deftest test-shipit-notifications-buffer--apply-auto-mark-preview-highlights-matches ()
  "GIVEN 3 PR notifications in the buffer (one whose subject matches REGEX)
WHEN `--apply-auto-mark-preview' is called with that regex
THEN exactly one preview overlay is installed, on the matching row."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr")
                 (subject . "feat: real feature") (reason . "mention")
                 (pr-state . "open") (updated-at . "2026-01-01T00:00:00Z"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:pr:2"
               '((repo . "owner/foo") (number . 2) (type . "pr")
                 (subject . "chore: bump dep from 1 to 2")
                 (reason . "mention") (pr-state . "open")
                 (updated-at . "2026-01-02T00:00:00Z"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:pr:3"
               '((repo . "owner/foo") (number . 3) (type . "pr")
                 (subject . "fix: critical bug") (reason . "mention")
                 (pr-state . "open") (updated-at . "2026-01-03T00:00:00Z"))
               shipit--notification-pr-activities)
      (unwind-protect
          (with-current-buffer buf
            (shipit-notifications-buffer--rerender)
            (shipit-notifications-buffer--apply-auto-mark-preview
             "^chore: bump")
            (should (= 1 (length
                          shipit-notifications-buffer--auto-mark-preview-overlays)))
            (let ((ov (car shipit-notifications-buffer--auto-mark-preview-overlays)))
              (should (eq (overlay-get ov 'face)
                          'shipit-auto-mark-preview-face))))
        (shipit-notifications-buffer--clear-auto-mark-preview)
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer--apply-auto-mark-preview-clears-prior ()
  "GIVEN preview overlays are already installed
WHEN `--apply-auto-mark-preview' is called with a new regex
THEN the previous overlays are removed before the new ones go down.
Each matching row contributes one row overlay (strike-through) and
one or more match-character overlays — clearing must remove both."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr")
                 (subject . "alpha") (reason . "mention")
                 (pr-state . "open") (updated-at . "2026-01-01T00:00:00Z"))
               shipit--notification-pr-activities)
      (puthash "owner/foo:pr:2"
               '((repo . "owner/foo") (number . 2) (type . "pr")
                 (subject . "beta") (reason . "mention")
                 (pr-state . "open") (updated-at . "2026-01-02T00:00:00Z"))
               shipit--notification-pr-activities)
      (unwind-protect
          (with-current-buffer buf
            (shipit-notifications-buffer--rerender)
            (shipit-notifications-buffer--apply-auto-mark-preview "alpha")
            (let ((after-alpha
                   (length shipit-notifications-buffer--auto-mark-preview-overlays)))
              (should (>= after-alpha 1))
              (shipit-notifications-buffer--apply-auto-mark-preview "beta")
              (let ((after-beta
                     (length shipit-notifications-buffer--auto-mark-preview-overlays)))
                ;; Switching regex must replace, not append.
                (should (= after-beta after-alpha))))
            (shipit-notifications-buffer--apply-auto-mark-preview "")
            (should (null
                     shipit-notifications-buffer--auto-mark-preview-overlays)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer--apply-auto-mark-preview-highlights-matched-chars ()
  "GIVEN a notification with subject `chore: bump foo from 1 to 2'
WHEN preview runs with regex `bump'
THEN at least two overlays are installed for that row: the
strike-through row overlay AND a sub-match overlay covering
the four `bump' characters with the match-highlight face."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr")
                 (subject . "chore: bump foo from 1 to 2")
                 (reason . "mention") (pr-state . "open")
                 (updated-at . "2026-01-01T00:00:00Z"))
               shipit--notification-pr-activities)
      (unwind-protect
          (with-current-buffer buf
            (shipit-notifications-buffer--rerender)
            (shipit-notifications-buffer--apply-auto-mark-preview "bump")
            (let* ((ovs shipit-notifications-buffer--auto-mark-preview-overlays)
                   (row-ovs (seq-filter
                             (lambda (o) (eq (overlay-get o 'face)
                                             'shipit-auto-mark-preview-face))
                             ovs))
                   (match-ovs (seq-filter
                               (lambda (o) (eq (overlay-get o 'face)
                                               'shipit-auto-mark-preview-match-face))
                               ovs)))
              (should (= 1 (length row-ovs)))
              (should (>= (length match-ovs) 1))
              ;; Each match overlay covers exactly the regex match (4 chars).
              (dolist (mov match-ovs)
                (should (= 4 (- (overlay-end mov)
                                (overlay-start mov))))
                (should (string= "bump"
                                 (buffer-substring-no-properties
                                  (overlay-start mov)
                                  (overlay-end mov)))))))
        (shipit-notifications-buffer--clear-auto-mark-preview)
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer--apply-auto-mark-preview-tolerates-bad-regex ()
  "GIVEN a malformed regex (e.g. unbalanced backslash)
WHEN `--apply-auto-mark-preview' is called
THEN the function does not raise and no overlays are installed —
this matters because the regex is being typed character by character."
  (cl-letf (((symbol-function 'shipit--check-notifications-background)
             (lambda (&rest _args) nil))
            ((symbol-function 'shipit--check-notifications-background-async)
             (lambda (&rest _args) nil)))
    (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
          (buf (shipit-notifications-buffer-create)))
      (puthash "owner/foo:pr:1"
               '((repo . "owner/foo") (number . 1) (type . "pr")
                 (subject . "alpha") (reason . "mention")
                 (pr-state . "open") (updated-at . "2026-01-01T00:00:00Z"))
               shipit--notification-pr-activities)
      (unwind-protect
          (with-current-buffer buf
            (shipit-notifications-buffer--rerender)
            (shipit-notifications-buffer--apply-auto-mark-preview "[")
            (should (null
                     shipit-notifications-buffer--auto-mark-preview-overlays)))
        (kill-buffer buf)))))

(ert-deftest test-shipit-notifications-buffer-remove-auto-mark-rule-by-index ()
  "GIVEN a rules list with 3 entries
WHEN `--remove-auto-mark-rule-at' is invoked with index 1 (second rule)
THEN the resulting list keeps the first and third rules in order."
  (let ((shipit-notifications-auto-mark-read-rules
         '((:state merged) (:title "x") (:type "workflow")))
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (sym val &rest _)
                 (set sym val)
                 (setq saved val))))
      (shipit-notifications-buffer--remove-auto-mark-rule-at 1)
      (should (equal saved '((:state merged) (:type "workflow")))))))

(ert-deftest test-shipit-notifications-buffer-clear-auto-mark-rules-confirms ()
  "GIVEN a non-empty rules list
WHEN clear is invoked and the user confirms
THEN the variable is set to nil and customize-save-variable was called."
  (let ((shipit-notifications-auto-mark-read-rules
         '((:state merged) (:state closed)))
        (saved-val 'untouched))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) t))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val &rest _) (set sym val) (setq saved-val val))))
      (shipit-notifications-buffer-clear-auto-mark-rules)
      (should (null shipit-notifications-auto-mark-read-rules))
      (should (null saved-val)))))

(provide 'test-notifications-buffer)
