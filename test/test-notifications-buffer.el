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
THEN `shipit-notifications-buffer--page-limit' should be 1."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (= shipit-notifications-buffer--page-limit 1)))
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
                  shipit-notifications-buffer--page-limit 3)
            (shipit-notifications-buffer-toggle-scope)
            (should (eq shipit-notifications-buffer--display-scope 'all))
            (should (= shipit-notifications-buffer--page-limit 1))
            (shipit-notifications-buffer-toggle-scope)
            (should (eq shipit-notifications-buffer--display-scope 'unread))
            (should (= shipit-notifications-buffer--page-limit 1)))
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
                  shipit-notifications-buffer--page-limit 1)
            (shipit-notifications-buffer-load-more)
            (should (= shipit-notifications-buffer--page-limit 2))
            (shipit-notifications-buffer-load-more)
            (should (= shipit-notifications-buffer--page-limit 3)))
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
                shipit-notifications-buffer--page-limit 1)
          (should-error (shipit-notifications-buffer-load-more)
                        :type 'user-error)
          (should (= shipit-notifications-buffer--page-limit 1)))
      (kill-buffer buf))))

(ert-deftest test-shipit-notifications-buffer-total-count-default-nil ()
  "GIVEN a fresh notifications buffer
THEN the buffer-local total-count is nil (unknown until probed)."
  (let ((buf (shipit-notifications-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (null shipit-notifications-buffer--total-count)))
      (kill-buffer buf))))

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
                  shipit-notifications-buffer--page-limit 1
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
                  shipit-notifications-buffer--page-limit 1
                  shipit-notifications-buffer--total-count 666
                  shipit-notifications-buffer--filter-text "match-repo")
            (shipit-notifications-buffer--rerender)
            (let ((header (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) 200))))
              (should (string-match-p "1/2 (of 666)" header))
              (should-not (string-match-p "1/666" header))))
        (kill-buffer buf)))))

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
                  shipit-notifications-buffer--page-limit 2)
            (shipit-notifications-buffer--rerender)
            (goto-char (point-min))
            (let ((header (buffer-substring-no-properties
                           (point-min)
                           (min (point-max) 200))))
              (should (string-match-p "all" header))
              (should (string-match-p "page 2\\|pages 2\\|2 pages\\|pages: 2" header))))
        (kill-buffer buf)))))
