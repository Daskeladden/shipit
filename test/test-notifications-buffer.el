;;; test-notifications-buffer.el --- Tests for notifications buffer -*- lexical-binding: t -*-

(require 'ert)
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
