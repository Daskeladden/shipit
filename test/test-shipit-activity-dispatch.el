;;; test-shipit-activity-dispatch.el --- Tests for activity navigation dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for shipit-pr--dispatch-activity-navigation, the activity-to-buffer
;; navigation dispatcher extracted from shipit--activity-event-actions so
;; both in-buffer RET and notifications-buffer RET can reuse it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-sections)
(require 'shipit-pr-actions)

(defmacro test-dispatch--capture (&rest body)
  "Run BODY with navigation helpers stubbed; return captured calls plist."
  `(let ((calls nil))
     (cl-letf (((symbol-function 'shipit--navigate-to-comment-by-id)
                (lambda (id) (push (cons :comment id) calls)))
               ((symbol-function 'shipit--navigate-to-approval-section)
                (lambda () (push (cons :approval t) calls)))
               ((symbol-function 'shipit--navigate-to-review-comment)
                (lambda (id) (push (cons :review id) calls)))
               ((symbol-function 'shipit--navigate-to-commit)
                (lambda (sha) (push (cons :commit sha) calls)))
               ((symbol-function 'shipit--navigate-to-inline-comment-file)
                (lambda (path id) (push (list :inline path id) calls)))
               ((symbol-function 'shipit--crossref-actions)
                (lambda (r n u t) (push (list :crossref r n u t) calls)))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body
       (nreverse calls))))

(ert-deftest test-activity-dispatch-commented-navigates-to-comment ()
  "GIVEN an activity with event-type=commented and a comment-id
WHEN dispatching the navigation
THEN shipit--navigate-to-comment-by-id is called with the id."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "commented" :comment-id 12345)))))
    (should (equal calls '((:comment . 12345))))))

(ert-deftest test-activity-dispatch-reviewed-approved-goes-to-approval ()
  "GIVEN a reviewed event with review-state=approved
WHEN dispatching
THEN the approval section navigation is invoked."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "reviewed" :review-state "approved")))))
    (should (equal calls '((:approval . t))))))

(ert-deftest test-activity-dispatch-reviewed-other-goes-to-review-comment ()
  "GIVEN a reviewed event with non-approved state
WHEN dispatching
THEN review-comment navigation is invoked with the id."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "reviewed"
                   :review-state "commented"
                   :comment-id 777)))))
    (should (equal calls '((:review . 777))))))

(ert-deftest test-activity-dispatch-committed-goes-to-commit ()
  "GIVEN a committed event with a commit-sha
WHEN dispatching
THEN the commit-navigation function is invoked with the sha."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "committed" :commit-sha "abc123")))))
    (should (equal calls '((:commit . "abc123"))))))

(ert-deftest test-activity-dispatch-line-commented-with-path ()
  "GIVEN a line-commented event with an inline-comment-path and comment-id
WHEN dispatching
THEN the inline-comment-file navigation is invoked with path and id."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "line-commented"
                   :inline-comment-path "src/foo.el"
                   :comment-id 42)))))
    (should (equal calls '((:inline "src/foo.el" 42))))))

(ert-deftest test-activity-dispatch-line-commented-without-path ()
  "GIVEN a line-commented event with no inline-comment-path
WHEN dispatching
THEN no navigation is invoked (message is emitted but not tracked)."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "line-commented" :comment-id 42)))))
    (should (null calls))))

(ert-deftest test-activity-dispatch-cross-referenced ()
  "GIVEN a cross-referenced event with crossref-number and repo
WHEN dispatching
THEN shipit--crossref-actions is invoked with repo, number, url, title."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "cross-referenced"
                   :crossref-repo "owner/repo"
                   :crossref-number 99
                   :crossref-url "https://example.com/99"
                   :crossref-title "Fix X")))))
    (should (equal calls
                   '((:crossref "owner/repo" 99 "https://example.com/99" "Fix X"))))))

(ert-deftest test-activity-dispatch-unknown-event-type ()
  "GIVEN an unknown event-type
WHEN dispatching
THEN no navigation function is invoked."
  (let ((calls (test-dispatch--capture
                (shipit-pr--dispatch-activity-navigation
                 '(:event-type "some-unknown-thing")))))
    (should (null calls))))

(provide 'test-shipit-activity-dispatch)
;;; test-shipit-activity-dispatch.el ends here
