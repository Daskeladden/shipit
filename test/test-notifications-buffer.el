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
