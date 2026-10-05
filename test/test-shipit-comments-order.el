;;; test-shipit-comments-order.el --- Tests for the order of comments -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the order of the comments of a page, for the key that
;; toggles the order of the section at point, and for the general
;; comments of a pull request buffer.  The comments of an issue buffer
;; are tested on the Jira sandbox, in test-shipit-jira-sandbox.el.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-core)
(require 'shipit-buffer)
(require 'shipit-issues-buffer)
(require 'shipit-pr-sections)
(require 'shipit-http)

;;; The order

(ert-deftest test-comments-order-keeps-or-turns-the-order-of-time ()
  "GIVEN three comments in the order of time
WHEN the setting asks for the oldest first
THEN they stay as they are
WHEN it asks for the newest first
THEN they come the other way round, AND the list itself is not changed."
  (let ((comments (list 1 2 3)))
    (let ((shipit-comments-order 'chronological))
      (should (equal '(1 2 3) (shipit--comments-in-order comments))))
    (let ((shipit-comments-order 'reverse-chronological))
      (should (equal '(3 2 1) (shipit--comments-in-order comments))))
    (should (equal '(1 2 3) comments))))

(ert-deftest test-comments-order-refuses-an-order-it-does-not-know ()
  "GIVEN a setting that names no order of comments
THEN to put comments in order is an error that names the setting."
  (let ((shipit-comments-order 'sideways))
    (should (string-match-p "sideways"
                            (cadr (should-error (shipit--comments-in-order '(1 2))))))))

;;; The key

(ert-deftest test-comments-order-o-toggles-the-order-of-the-section-at-point ()
  "GIVEN a pull request buffer and an issue buffer
THEN O is the key that toggles the order of the section at point in both."
  (dolist (mode '(shipit-mode shipit-issue-mode))
    (with-temp-buffer
      (funcall mode)
      (should (eq (key-binding (kbd "O")) #'shipit-toggle-section-order)))))

(ert-deftest test-comments-order-the-activity-is-toggled-from-inside-it ()
  "GIVEN a buffer with an activity section, and an event inside it
WHEN the order of the section is toggled with point on the event
THEN the order of the activity changes."
  (let ((shipit-activity-timeline-order 'reverse-chronological))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (magit-insert-section (pr-activity)
            (magit-insert-heading "Activity")
            (magit-insert-section (activity-event 1)
              (insert "an event\n")))))
      (goto-char (point-min))
      (search-forward "an event")
      (shipit-toggle-section-order)
      (should (eq shipit-activity-timeline-order 'chronological)))))

(ert-deftest test-comments-order-a-section-without-an-order-says-so ()
  "GIVEN a section that has no order
WHEN the order of the section at point is toggled
THEN that is a user error, AND no order changes."
  (let ((shipit-comments-order 'chronological))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (insert "a line\n")))
      (goto-char (point-min))
      (should-error (shipit-toggle-section-order) :type 'user-error)
      (should (eq shipit-comments-order 'chronological)))))

;;; The general comments of a pull request

(defun test-comments-order--comment (id body minute)
  "A general comment with ID and BODY, written at MINUTE past ten."
  `((id . ,id) (body . ,body) (user (login . "someone"))
    (shipit-comment-type . "issue")
    (created_at . ,(format "2026-01-01T10:%02d:00Z" minute))))

(defconst test-comments-order--bodies '("First said" "Second said" "Third said")
  "The bodies of three general comments, in the order they are written in.")

(defun test-comments-order--as-shown ()
  "The bodies of the general comments, in the order this buffer shows them."
  (sort (copy-sequence test-comments-order--bodies)
        (lambda (a b)
          (< (save-excursion (goto-char (point-min)) (search-forward a))
             (save-excursion (goto-char (point-min)) (search-forward b))))))

(defmacro test-comments-order--with-general-comments (&rest body)
  "Run BODY in a pull request buffer that shows three general comments.
The section is unfolded.  No avatar and no reaction is asked for."
  (declare (indent 0))
  `(with-temp-buffer
     (shipit-mode)
     (let ((inhibit-read-only t)
           (shipit-show-avatars nil)
           (shipit--reaction-cache (make-hash-table :test 'equal))
           (shipit-comments-order shipit-comments-order))
       (setq shipit-buffer-repo "o/r"
             shipit-buffer-pr-number 7
             shipit-current-repo "o/r")
       (magit-insert-section (root)
         (shipit--insert-general-comments-section-placeholder "o/r" 7))
       (shipit--replace-general-comments-section-with-content
        "o/r" 7 (cl-loop for text in test-comments-order--bodies
                         for id from 1
                         collect (test-comments-order--comment id text id)))
       (magit-section-show (shipit--find-section-by-type 'general-comments))
       ,@body)))

(ert-deftest test-comments-order-general-comments-show-the-oldest-first ()
  "GIVEN a pull request with three general comments
WHEN its general comments are drawn
THEN they stand in the order they were written in."
  (test-comments-order--with-general-comments
    (should (equal test-comments-order--bodies (test-comments-order--as-shown)))))

(ert-deftest test-comments-order-general-comments-show-the-newest-first-when-asked ()
  "GIVEN a pull request with three general comments, AND the setting that
asks for the newest comment first
WHEN its general comments are drawn
THEN they stand with the newest first."
  (let ((shipit-comments-order 'reverse-chronological))
    (test-comments-order--with-general-comments
      (should (equal (reverse test-comments-order--bodies)
                     (test-comments-order--as-shown))))))

(ert-deftest test-comments-order-general-comments-are-toggled-in-the-buffer ()
  "GIVEN the general comments of a pull request, the oldest first
WHEN the order of the section is toggled with point on a comment
THEN they stand with the newest first
WHEN it is toggled again
THEN the oldest is first again."
  (test-comments-order--with-general-comments
    (dolist (expected (list (reverse test-comments-order--bodies)
                            test-comments-order--bodies))
      (goto-char (point-min))
      (search-forward "Second said")
      (shipit-toggle-section-order)
      (magit-section-show (shipit--find-section-by-type 'general-comments))
      (should (equal expected (test-comments-order--as-shown))))))

(provide 'test-shipit-comments-order)
;;; test-shipit-comments-order.el ends here
