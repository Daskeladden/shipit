;;; test-shipit-review-line-comments.el --- Line comments of a review in its activity row -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; A review that only comments on lines of the diff comes in the
;; timeline with an empty body.  Its activity row opens to those
;; comments, each below the file and line it is on.  HTTP is the only
;; mocked boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-comment-github)
(require 'shipit-pr-sections)
(require 'shipit-notifications-buffer)

;;; Fixtures

(defconst test-srlc--pr-comments
  '(((id . 1) (pull_request_review_id . 100) (path . "lisp/a.el")
     (line . 12) (original_line . 12) (body . "First remark")
     (user . ((login . "kim"))) (created_at . "2026-09-09T12:00:00Z"))
    ((id . 2) (pull_request_review_id . 200) (path . "lisp/a.el")
     (line . 40) (original_line . 40) (body . "Remark of another review")
     (user . ((login . "lee"))) (created_at . "2026-09-09T12:01:00Z"))
    ((id . 3) (pull_request_review_id . 100) (path . "lisp/b.el")
     (line . nil) (original_line . 3) (body . "Second remark")
     (user . ((login . "kim"))) (created_at . "2026-09-09T12:02:00Z")))
  "The comments on lines of a pull request, as GitHub lists them.
Review 100 made two of them, the second on a line that is outdated.")

(defmacro test-srlc--with-comments (comments &rest body)
  "Run BODY with the GitHub backend, and its HTTP answering COMMENTS.
COMMENTS is the list of all line comments of the pull request."
  (declare (indent 1))
  `(let ((shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-show-avatars nil))
     (cl-letf (((symbol-function 'shipit--api-request-paginated)
                (lambda (endpoint &rest _)
                  (should (equal "/repos/o/r/pulls/7/comments" endpoint))
                  ,comments)))
       ,@body)))

(defun test-srlc--review (body)
  "A review event 100 by kim, that says BODY."
  `((event . "reviewed") (id . 100) (state . "commented") (body . ,body)
    (submitted_at . "2026-09-09T12:03:00Z")
    (user . ((login . "kim")))))

(defun test-srlc--render-row (event)
  "Insert EVENT as an activity row, and return its finished section."
  (magit-insert-section (root)
    (shipit--insert-activity-event event "o/r" 7 nil))
  (car (last (oref magit-root-section children))))

(defun test-srlc--opened-text (event)
  "Return the text of the activity row of EVENT once it is opened."
  (let ((row (test-srlc--render-row event)))
    (should (oref row hidden))
    (magit-section-show row)
    (buffer-substring (oref row start) (oref row end))))

;;; Backend

(ert-deftest test-srlc-a-review-gets-only-its-own-line-comments ()
  "GIVEN a pull request with line comments of two reviews
WHEN the line comments of review 100 are fetched through the backend
THEN they are the two comments that review 100 made, in their order."
  (test-srlc--with-comments test-srlc--pr-comments
    (let* ((resolved (shipit-comment--resolve-for-repo "o/r"))
           (fetch (plist-get (car resolved) :fetch-review-comments)))
      (should (equal '(1 3)
                     (mapcar (lambda (comment) (alist-get 'id comment))
                             (funcall fetch (cdr resolved) 7 100)))))))

;;; The activity row

(ert-deftest test-srlc-a-review-without-text-opens-to-its-line-comments ()
  "GIVEN a review with an empty body, that commented on two lines
WHEN its activity row is opened
THEN it shows each comment below its file and line, the outdated one
at the line it was made on, and no comment of another review."
  (with-temp-buffer
    (test-srlc--with-comments test-srlc--pr-comments
      (let ((text (test-srlc--opened-text (test-srlc--review ""))))
        (should (string-match-p "lisp/a\\.el:12\n.*First remark" text))
        (should (string-match-p "lisp/b\\.el:3\n.*Second remark" text))
        (should (eq 'shipit-filename-face
                    (get-text-property (string-match "lisp/a\\.el" text)
                                       'font-lock-face text)))
        (should-not (string-match-p "another review" text))))))

(ert-deftest test-srlc-a-review-with-text-shows-it-before-its-line-comments ()
  "GIVEN a review with a body, that also commented on lines
WHEN its activity row is opened
THEN the body comes first, and the line comments follow."
  (with-temp-buffer
    (test-srlc--with-comments test-srlc--pr-comments
      (let ((text (test-srlc--opened-text (test-srlc--review "Overall fine"))))
        (should (< (string-match "Overall fine" text)
                   (string-match "lisp/a\\.el:12" text)))))))

(ert-deftest test-srlc-a-review-with-nothing-to-show-says-so ()
  "GIVEN a review with an empty body, and no comments on lines
WHEN its activity row is opened
THEN it says that the review has no comments."
  (with-temp-buffer
    (test-srlc--with-comments nil
      (should (string-match-p "No comments"
                              (test-srlc--opened-text (test-srlc--review "")))))))

(ert-deftest test-srlc-a-failed-fetch-shows-in-the-row ()
  "GIVEN a review whose line comments cannot be fetched
WHEN its activity row is opened
THEN the row is open and tells why, and the row after it stays."
  (with-temp-buffer
    (let ((shipit-pr-backend 'github)
          (shipit-pr-backend-config nil))
      (cl-letf (((symbol-function 'shipit--api-request-paginated)
                 (lambda (&rest _) (error "GitHub API error: 502"))))
        (let ((text (test-srlc--opened-text (test-srlc--review "Overall fine"))))
          (should (string-match-p "Overall fine" text))
          (should (string-match-p "GitHub API error: 502" text)))))))

;;; In the notifications buffer

(defun test-srlc--visible-text ()
  "Return the text of this buffer that is not hidden."
  (let ((out "") (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-char-property-change pos)))
        (unless (invisible-p pos)
          (setq out (concat out (buffer-substring-no-properties pos next))))
        (setq pos next)))
    out))

(ert-deftest test-srlc-a-review-under-the-activity-of-a-notification-opens ()
  "GIVEN a notification of a pull request whose Activity holds a review
with an empty body, that commented on lines
WHEN TAB is pressed on the review row
THEN its line comments show below it."
  (with-temp-buffer
    (shipit-notifications-buffer-mode)
    (test-srlc--with-comments test-srlc--pr-comments
      (let ((inhibit-read-only t))
        (magit-insert-section (notifications-root)
          (magit-insert-section (notification-entry nil)
            (magit-insert-heading "A notification")
            (let ((magit-insert-section--parent magit-insert-section--current))
              (shipit-notifications-buffer--insert-activity-section
               (list (test-srlc--review "")) "2026-09-09T00:00:00Z" "o/r" 7)))))
      (goto-char (point-min))
      (re-search-forward "kim commented this PR")
      (should-not (string-match-p "First remark" (test-srlc--visible-text)))
      (shipit-notifications-buffer-toggle-section)
      (should (string-match-p "lisp/a\\.el:12\n +First remark"
                              (test-srlc--visible-text))))))

(provide 'test-shipit-review-line-comments)
;;; test-shipit-review-line-comments.el ends here
