;;; test-pr-approval-endpoint.el --- Tests for PR approval endpoint -*- lexical-binding: t; -*-

;; Tests to ensure PR approval uses the correct GitHub API endpoint:
;; POST /repos/{owner}/{repo}/pulls/{pull_number}/reviews
;; with event="APPROVE"
;;
;; This is critical because using the wrong endpoint or event would fail to
;; actually approve the PR on GitHub.

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load shipit modules
(require 'shipit-http)

;;; Code:

(defvar test-approval-captured-url nil
  "Captured URL from the last API call.")

(defvar test-approval-captured-method nil
  "Captured HTTP method from the last API call.")

(defvar test-approval-captured-payload nil
  "Captured payload from the last API call.")

(defmacro test-with-approval-tracking (&rest body)
  "Execute BODY with mocked API functions that track approval requests."
  `(let ((test-approval-captured-url nil)
         (test-approval-captured-method nil)
         (test-approval-captured-payload nil)
         (shipit-current-repo "test-owner/test-repo")
         (shipit-api-url "https://api.github.com")
         (shipit-github-token "test-token")
         ;; Mock approval status cache
         (shipit--cached-approval-status (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                (lambda () "test-owner/test-repo"))

               ;; Mock shipit--ensure-repository to return t
               ((symbol-function 'shipit--ensure-repository)
                (lambda () t))

               ;; Mock shipit-get-pull-request to return PR data with different author
               ((symbol-function 'shipit-get-pull-request)
                (lambda (pr-number &optional repo)
                  '((user . ((login . "other-user"))))))

               ;; Mock shipit--get-current-user to return test user
               ((symbol-function 'shipit--get-current-user)
                (lambda () "test-user"))

               ;; Mock shipit--get-auth-header
               ((symbol-function 'shipit--get-auth-header)
                (lambda () '("Authorization" . "token test-token")))

               ;; Mock the async URL retrieve to capture the request
               ((symbol-function 'shipit--url-retrieve-async)
                (lambda (url method headers payload success-callback error-callback)
                  (setq test-approval-captured-url url)
                  (setq test-approval-captured-method method)
                  (setq test-approval-captured-payload payload)
                  ;; Call success callback with success response
                  (when success-callback
                    (funcall success-callback '((id . 12345))))))

               ;; Mock cache clearing functions
               ((symbol-function 'shipit-clear-etag-cache-for-endpoint)
                (lambda (endpoint) nil)))
       ,@body)))

;;;; Behavior Tests for PR Approval

(ert-deftest test-approving-pr-sends-to-reviews-endpoint ()
  "Approving a PR should POST to /repos/{owner}/{repo}/pulls/{pr}/reviews."
  (test-with-approval-tracking
   (shipit-post-review 123 "APPROVE" nil)

   ;; Verify the correct endpoint was called
   (should test-approval-captured-url)
   (should (string-match-p "/repos/test-owner/test-repo/pulls/123/reviews"
                           test-approval-captured-url))

   ;; Verify it's a POST request
   (should (string= test-approval-captured-method "POST"))))

(ert-deftest test-approving-pr-sends-approve-event ()
  "Approving a PR should send event='APPROVE' in the payload."
  (test-with-approval-tracking
   (shipit-post-review 123 "APPROVE" nil)

   ;; Verify the payload contains the APPROVE event
   (should test-approval-captured-payload)
   (let ((decoded-payload (json-read-from-string test-approval-captured-payload)))
     (should (string= (cdr (assq 'event decoded-payload)) "APPROVE")))))

(ert-deftest test-approving-pr-with-body-includes-body ()
  "Approving a PR with a comment should include the body in the payload."
  (test-with-approval-tracking
   (shipit-post-review 123 "APPROVE" "LGTM!")

   ;; Verify the payload contains both event and body
   (should test-approval-captured-payload)
   (let ((decoded-payload (json-read-from-string test-approval-captured-payload)))
     (should (string= (cdr (assq 'event decoded-payload)) "APPROVE"))
     (should (string= (cdr (assq 'body decoded-payload)) "LGTM!")))))

(ert-deftest test-requesting-changes-sends-correct-event ()
  "Requesting changes should send event='REQUEST_CHANGES' in the payload."
  (test-with-approval-tracking
   (shipit-post-review 123 "REQUEST_CHANGES" "Please fix the tests")

   ;; Verify the correct endpoint was called
   (should test-approval-captured-url)
   (should (string-match-p "/repos/test-owner/test-repo/pulls/123/reviews"
                           test-approval-captured-url))

   ;; Verify the payload contains the REQUEST_CHANGES event
   (should test-approval-captured-payload)
   (let ((decoded-payload (json-read-from-string test-approval-captured-payload)))
     (should (string= (cdr (assq 'event decoded-payload)) "REQUEST_CHANGES"))
     (should (string= (cdr (assq 'body decoded-payload)) "Please fix the tests")))))

(ert-deftest test-commenting-review-sends-correct-event ()
  "A comment-only review should send event='COMMENT' in the payload."
  (test-with-approval-tracking
   (shipit-post-review 123 "COMMENT" "Just some thoughts")

   ;; Verify the correct endpoint was called
   (should test-approval-captured-url)
   (should (string-match-p "/repos/test-owner/test-repo/pulls/123/reviews"
                           test-approval-captured-url))

   ;; Verify the payload contains the COMMENT event
   (should test-approval-captured-payload)
   (let ((decoded-payload (json-read-from-string test-approval-captured-payload)))
     (should (string= (cdr (assq 'event decoded-payload)) "COMMENT")))))

;;;; Negative Tests

(ert-deftest test-approving-own-pr-is-rejected ()
  "Users should not be able to approve their own PRs."
  (let ((shipit-current-repo "test-owner/test-repo")
        (shipit-api-url "https://api.github.com")
        (shipit-github-token "test-token")
        (shipit--cached-approval-status (make-hash-table :test 'equal))
        (api-called nil))
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () t))
              ;; PR author is same as current user
              ((symbol-function 'shipit-get-pull-request)
               (lambda (pr-number &optional repo)
                 '((user . ((login . "test-user"))))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "test-user"))
              ((symbol-function 'shipit--url-retrieve-async)
               (lambda (&rest args)
                 (setq api-called t))))

      ;; Should error when trying to approve own PR
      (should-error (shipit-post-review 123 "APPROVE" nil))

      ;; Verify API was never called
      (should-not api-called))))

(ert-deftest test-approval-does-not-use-issues-endpoint ()
  "PR approval must NOT use the /issues/ endpoint (that's for comments)."
  (test-with-approval-tracking
   (shipit-post-review 123 "APPROVE" nil)

   ;; Verify /issues/ is NOT in the URL
   (should test-approval-captured-url)
   (should-not (string-match-p "/issues/" test-approval-captured-url))))

(ert-deftest test-approval-does-not-use-comments-endpoint ()
  "PR approval must NOT use the /comments endpoint (that's for PR comments)."
  (test-with-approval-tracking
   (shipit-post-review 123 "APPROVE" nil)

   ;; Verify /comments is NOT in the URL
   (should test-approval-captured-url)
   (should-not (string-match-p "/comments" test-approval-captured-url))))

;;;; Repository Context Tests

(ert-deftest test-approval-uses-explicit-repo-when-provided ()
  "When an explicit repo is passed, it should be used in the endpoint."
  (test-with-approval-tracking
   ;; Pass a different repo explicitly - this should override shipit-current-repo
   (shipit-post-review 123 "APPROVE" nil nil "different-owner/different-repo")

   ;; Verify the explicit repo was used, not the default
   (should test-approval-captured-url)
   (should (string-match-p "/repos/different-owner/different-repo/pulls/123/reviews"
                           test-approval-captured-url))
   ;; Make sure the default repo was NOT used
   (should-not (string-match-p "test-owner/test-repo" test-approval-captured-url))))

(ert-deftest test-approval-uses-buffer-local-repo-when-no-explicit-repo ()
  "When no explicit repo is passed, buffer-local repo should be preferred."
  (let ((shipit-current-repo "global-owner/global-repo")
        (shipit-api-url "https://api.github.com")
        (shipit-github-token "test-token")
        (shipit--cached-approval-status (make-hash-table :test 'equal))
        (test-approval-captured-url nil))
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () t))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (pr-number &optional repo)
                 '((user . ((login . "other-user"))))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "test-user"))
              ((symbol-function 'shipit--get-auth-header)
               (lambda () '("Authorization" . "token test-token")))
              ((symbol-function 'shipit--url-retrieve-async)
               (lambda (url method headers payload success-callback error-callback)
                 (setq test-approval-captured-url url)
                 (when success-callback
                   (funcall success-callback '((id . 12345))))))
              ((symbol-function 'shipit-clear-etag-cache-for-endpoint)
               (lambda (endpoint) nil)))

      ;; Use a temp buffer and set buffer-local repo properly
      (with-temp-buffer
        (setq-local shipit-buffer-repo "buffer-owner/buffer-repo")
        (shipit-post-review 123 "APPROVE" nil))

      ;; Verify the buffer-local repo was used
      (should test-approval-captured-url)
      (should (string-match-p "/repos/buffer-owner/buffer-repo/pulls/123/reviews"
                              test-approval-captured-url)))))

(ert-deftest test-approval-falls-back-to-global-repo ()
  "When no explicit or buffer-local repo, fall back to global shipit-current-repo."
  (let ((shipit-current-repo "global-owner/global-repo")
        (shipit-api-url "https://api.github.com")
        (shipit-github-token "test-token")
        (shipit--cached-approval-status (make-hash-table :test 'equal))
        (shipit-buffer-repo nil)  ; Explicitly nil
        (test-approval-captured-url nil))
    (cl-letf (((symbol-function 'shipit--ensure-repository)
               (lambda () t))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (pr-number &optional repo)
                 '((user . ((login . "other-user"))))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "test-user"))
              ((symbol-function 'shipit--get-auth-header)
               (lambda () '("Authorization" . "token test-token")))
              ((symbol-function 'shipit--url-retrieve-async)
               (lambda (url method headers payload success-callback error-callback)
                 (setq test-approval-captured-url url)
                 (when success-callback
                   (funcall success-callback '((id . 12345))))))
              ((symbol-function 'shipit-clear-etag-cache-for-endpoint)
               (lambda (endpoint) nil)))

      (shipit-post-review 123 "APPROVE" nil)

      ;; Verify the global repo was used
      (should test-approval-captured-url)
      (should (string-match-p "/repos/global-owner/global-repo/pulls/123/reviews"
                              test-approval-captured-url)))))

(provide 'test-pr-approval-endpoint)
;;; test-pr-approval-endpoint.el ends here
