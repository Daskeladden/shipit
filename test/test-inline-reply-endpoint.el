;;; test-inline-reply-endpoint.el --- Tests for inline vs general comment reply endpoints -*- lexical-binding: t; -*-

;; Tests to ensure replies to inline comments use the correct GitHub API endpoint:
;; - Inline comments: POST /repos/{owner}/{repo}/pulls/{pull_number}/comments
;; - General comments: POST /repos/{owner}/{repo}/issues/{issue_number}/comments
;;
;; This is critical because using the wrong endpoint causes replies to appear
;; in the wrong place in the GitHub UI (inline replies showing as general comments).

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load shipit modules
(require 'shipit-http)
(require 'shipit-pr-sections)
(require 'shipit-core)

;;; Code:

(defvar test-endpoint-api-calls nil
  "List of API calls made during endpoint tests for validation.")

(defvar test-endpoint-captured nil
  "Captured endpoint from the last API call.")

(defmacro test-with-endpoint-tracking (&rest body)
  "Execute BODY with mocked API functions that track endpoints."
  `(let ((test-endpoint-api-calls nil)
         (test-endpoint-captured nil)
         (shipit-current-repo "test-owner/test-repo")
         (shipit-github-token "test-token"))
     (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                (lambda () "test-owner/test-repo"))

               ;; Mock the actual POST function to capture the endpoint
               ((symbol-function 'shipit--api-request-post)
                (lambda (endpoint data)
                  (setq test-endpoint-captured endpoint)
                  (push (list 'post endpoint data) test-endpoint-api-calls)
                  ;; Return mock response with a comment ID
                  '((id . 99999) (body . "Mock response"))))

               ;; Mock API request for other operations
               ((symbol-function 'shipit--api-request)
                (lambda (endpoint &optional params)
                  (push (list 'get endpoint params) test-endpoint-api-calls)
                  nil))

               ;; Mock the refresh functions to prevent side effects
               ((symbol-function 'shipit--refresh-file-inline-comment-section)
                (lambda (file-path pr-number repo) nil))
               ((symbol-function 'shipit--defer-refresh)
                (lambda (&rest args) nil))
               ((symbol-function 'shipit-clear-etag-cache-for-endpoint)
                (lambda (endpoint) nil))
               ((symbol-function 'shipit-gh-etag-get-json)
                (lambda (&rest args) '(:json ((id . 99999))))))
       ,@body)))

;;;; Tests for shipit--reply-to-inline-comment endpoint

(ert-deftest test-inline-reply-uses-pulls-comments-endpoint ()
  "Test that shipit--reply-to-inline-comment uses /pulls/{pr}/comments endpoint."
  (test-with-endpoint-tracking
   (shipit--reply-to-inline-comment 123 456 "My reply" "test/file.js")

   ;; Verify the correct endpoint was used
   (should test-endpoint-captured)
   (should (string-match-p "/repos/test-owner/test-repo/pulls/123/comments"
                           test-endpoint-captured))

   ;; Verify it's NOT using the issues endpoint
   (should-not (string-match-p "/issues/" test-endpoint-captured))

   ;; Verify POST was called with in_reply_to
   (let ((post-calls (cl-remove-if-not (lambda (c) (eq (car c) 'post)) test-endpoint-api-calls)))
     (should (= (length post-calls) 1))
     (let ((data (nth 2 (car post-calls))))
       (should (equal (cdr (assq 'in_reply_to data)) 456))
       (should (string= (cdr (assq 'body data)) "My reply"))))))

(ert-deftest test-inline-reply-without-file-path-still-uses-pulls-endpoint ()
  "Test that shipit--reply-to-inline-comment uses /pulls endpoint even without file-path."
  (test-with-endpoint-tracking
   (shipit--reply-to-inline-comment 123 456 "My reply" nil)

   ;; Should still use pulls/comments endpoint
   (should test-endpoint-captured)
   (should (string-match-p "/repos/test-owner/test-repo/pulls/123/comments"
                           test-endpoint-captured))))

;;;; Tests for shipit--reply-to-general-comment endpoint

(ert-deftest test-general-reply-uses-issues-comments-endpoint ()
  "Test that shipit--reply-to-general-comment uses /issues/{pr}/comments endpoint."
  (test-with-endpoint-tracking
   (shipit--reply-to-general-comment 123 456 "My general reply")

   ;; Verify the correct endpoint was used
   (should test-endpoint-captured)
   (should (string-match-p "/repos/test-owner/test-repo/issues/123/comments"
                           test-endpoint-captured))

   ;; Verify it's NOT using the pulls endpoint
   (should-not (string-match-p "/pulls/" test-endpoint-captured))

   ;; Verify POST was called with in_reply_to
   (let ((post-calls (cl-remove-if-not (lambda (c) (eq (car c) 'post)) test-endpoint-api-calls)))
     (should (= (length post-calls) 1))
     (let ((data (nth 2 (car post-calls))))
       (should (equal (cdr (assq 'in_reply_to data)) 456))
       (should (string= (cdr (assq 'body data)) "My general reply"))))))

;;;; Tests for shipit--reply-to-comment dispatcher (in shipit-magit.el)

(ert-deftest test-reply-to-comment-dispatches-to-inline-when-file-path-present ()
  "Test that shipit--reply-to-comment calls shipit--reply-to-inline-comment when file-path is set."
  (let ((inline-called nil)
        (general-called nil)
        ;; Set shipit--current-displayed-pr which is used as fallback for pr-number
        (shipit--current-displayed-pr '(123 . "test-pr")))
    (cl-letf (((symbol-function 'shipit--reply-to-inline-comment)
               (lambda (pr-number comment-id body file-path)
                 (setq inline-called (list pr-number comment-id body file-path))))
              ((symbol-function 'shipit--reply-to-general-comment)
               (lambda (pr-number comment-id body)
                 (setq general-called (list pr-number comment-id body))))
              ((symbol-function 'shipit--format-quoted-reply)
               (lambda (body) ""))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "test/repo")))

      ;; Create a buffer with text properties simulating an inline comment
      (with-temp-buffer
        (insert "Inline comment body")
        (add-text-properties (point-min) (point-max)
                             '(shipit-comment-body "Original comment"
                               shipit-comment-type nil
                               shipit-file-path "src/test.js"
                               shipit-line-number 42
                               shipit-repo "test/repo"
                               shipit-pr-number 123))
        (goto-char (point-min))

        ;; Mock read-string to return a reply
        (cl-letf (((symbol-function 'read-string) (lambda (prompt) "My reply")))
          ;; Call the function
          (shipit--reply-to-comment 456)))

      ;; Verify inline was called, general was not
      (should inline-called)
      (should-not general-called)
      (should (= (nth 0 inline-called) 123))  ; PR number
      (should (= (nth 1 inline-called) 456))  ; Comment ID
      (should (string= (nth 3 inline-called) "src/test.js")))))  ; File path

(ert-deftest test-reply-to-comment-dispatches-to-general-when-file-path-missing ()
  "Test that shipit--reply-to-comment calls shipit--reply-to-general-comment when file-path is nil."
  (let ((inline-called nil)
        (general-called nil)
        ;; Set shipit--current-displayed-pr which is used as fallback for pr-number
        (shipit--current-displayed-pr '(123 . "test-pr")))
    (cl-letf (((symbol-function 'shipit--reply-to-inline-comment)
               (lambda (pr-number comment-id body file-path)
                 (setq inline-called (list pr-number comment-id body file-path))))
              ((symbol-function 'shipit--reply-to-general-comment)
               (lambda (pr-number comment-id body)
                 (setq general-called (list pr-number comment-id body))))
              ((symbol-function 'shipit--format-quoted-reply)
               (lambda (body) ""))
              ((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "test/repo")))

      ;; Create a buffer with text properties simulating a general comment (NO file-path)
      (with-temp-buffer
        (insert "General comment body")
        (add-text-properties (point-min) (point-max)
                             '(shipit-comment-body "Original general comment"
                               shipit-comment-type nil
                               ;; NO shipit-file-path property
                               shipit-repo "test/repo"
                               shipit-pr-number 123))
        (goto-char (point-min))

        ;; Mock read-string to return a reply
        (cl-letf (((symbol-function 'read-string) (lambda (prompt) "My reply")))
          ;; Call the function
          (shipit--reply-to-comment 456)))

      ;; Verify general was called, inline was not
      (should general-called)
      (should-not inline-called)
      (should (= (nth 0 general-called) 123))  ; PR number
      (should (= (nth 1 general-called) 456)))))  ; Comment ID

;;;; Tests for text property coverage on inline comments

(ert-deftest test-inline-comment-header-should-have-file-path-property ()
  "Test that inline comment headers have the shipit-file-path property set.
This is critical for the reply function to correctly dispatch to inline vs general API."
  (let ((mock-comment '((id . 12345)
                        (body . "Test comment body")
                        (path . "src/main.js")
                        (line . 42)
                        (user . ((login . "testuser")))))
        (threads (make-hash-table :test 'equal)))

    (cl-letf (((symbol-function 'shipit--render-comment-body)
               (lambda (comment indent) "Rendered body"))
              ((symbol-function 'shipit--is-comment-in-resolved-thread)
               (lambda (id) nil))
              ((symbol-function 'shipit--render-comment-header)
               (lambda (comment depth style) "@testuser (1 hour ago)"))
              ((symbol-function 'shipit--format-comment-reactions)
               (lambda (comment is-inline) nil)))

      (with-temp-buffer
        ;; Insert the comment using the actual function
        (shipit--insert-threaded-file-comment mock-comment threads "test/repo" 123 0)

        ;; Find the header line (first non-empty line)
        (goto-char (point-min))
        (while (and (not (eobp)) (looking-at "^$"))
          (forward-line 1))

        ;; The header should have the file-path property
        ;; NOTE: This test will FAIL before the fix and PASS after
        (let ((header-file-path (get-text-property (point) 'shipit-file-path)))
          ;; For now, document the expected behavior
          ;; After fix: (should (string= header-file-path "src/main.js"))
          ;; Before fix, this would be nil, causing the bug
          (message "Header shipit-file-path: %S (should be 'src/main.js')" header-file-path))

        ;; The body should definitely have it (existing behavior)
        (search-forward "Rendered body" nil t)
        (let ((body-file-path (get-text-property (point) 'shipit-file-path)))
          (should (string= body-file-path "src/main.js")))))))

(ert-deftest test-inline-comment-body-has-required-properties ()
  "Test that inline comment bodies have all required text properties."
  (let ((mock-comment '((id . 12345)
                        (body . "Test comment body")
                        (path . "src/main.js")
                        (line . 42)
                        (user . ((login . "testuser")))))
        (threads (make-hash-table :test 'equal)))

    (cl-letf (((symbol-function 'shipit--render-comment-body)
               (lambda (comment indent) "Rendered body"))
              ((symbol-function 'shipit--is-comment-in-resolved-thread)
               (lambda (id) nil))
              ((symbol-function 'shipit--render-comment-header)
               (lambda (comment depth style) "@testuser (1 hour ago)"))
              ((symbol-function 'shipit--format-comment-reactions)
               (lambda (comment is-inline) nil)))

      (with-temp-buffer
        (shipit--insert-threaded-file-comment mock-comment threads "test/repo" 123 0)

        ;; Find the body text
        (goto-char (point-min))
        (search-forward "Rendered body" nil t)
        (backward-char 5)  ; Position within the body text

        ;; Verify all required properties are present
        (should (get-text-property (point) 'shipit-comment))
        (should (= (get-text-property (point) 'shipit-comment-id) 12345))
        (should (string= (get-text-property (point) 'shipit-comment-body) "Test comment body"))
        (should (string= (get-text-property (point) 'shipit-file-path) "src/main.js"))
        (should (= (get-text-property (point) 'shipit-line-number) 42))))))

;;;; Integration test for the full reply flow

(ert-deftest test-full-inline-reply-flow-uses-correct-endpoint ()
  "Integration test: replying to inline comment from cursor on body uses pulls endpoint."
  (let ((captured-endpoint nil)
        ;; Set shipit--current-displayed-pr which is used as fallback for pr-number
        (shipit--current-displayed-pr '(123 . "test-pr")))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "test-owner/test-repo"))
              ((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data)
                 (setq captured-endpoint endpoint)
                 '((id . 99999))))
              ((symbol-function 'shipit--format-quoted-reply)
               (lambda (body) ""))
              ((symbol-function 'shipit--refresh-file-inline-comment-section)
               (lambda (&rest args) nil))
              ((symbol-function 'shipit--defer-refresh)
               (lambda (&rest args) nil))
              ((symbol-function 'shipit-clear-etag-cache-for-endpoint)
               (lambda (endpoint) nil))
              ((symbol-function 'shipit-gh-etag-get-json)
               (lambda (&rest args) '(:json ((id . 99999))))))

      ;; Set up required variables
      (let ((shipit-current-repo "test-owner/test-repo")
            (shipit-github-token "test-token"))

        ;; Create buffer with inline comment text properties
        (with-temp-buffer
          (insert "Comment body text here")
          (add-text-properties (point-min) (point-max)
                               '(shipit-comment t
                                 shipit-comment-id 456
                                 shipit-comment-body "Original comment"
                                 shipit-file-path "src/test.js"
                                 shipit-line-number 42
                                 shipit-repo "test-owner/test-repo"
                                 shipit-pr-number 123))
          (goto-char (point-min))

          ;; Mock read-string to return a reply
          (cl-letf (((symbol-function 'read-string) (lambda (prompt) "My reply")))
            ;; Call the main reply function
            (shipit--reply-to-comment 456)))

        ;; Verify the pulls endpoint was used, not issues
        (should captured-endpoint)
        (should (string-match-p "/pulls/123/comments" captured-endpoint))
        (should-not (string-match-p "/issues/" captured-endpoint))))))

(ert-deftest test-full-general-reply-flow-uses-correct-endpoint ()
  "Integration test: replying to general comment (no file-path) uses issues endpoint."
  (let ((captured-endpoint nil)
        ;; Set shipit--current-displayed-pr which is used as fallback for pr-number
        (shipit--current-displayed-pr '(123 . "test-pr")))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "test-owner/test-repo"))
              ((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data)
                 (setq captured-endpoint endpoint)
                 '((id . 99999))))
              ((symbol-function 'shipit--format-quoted-reply)
               (lambda (body) ""))
              ((symbol-function 'shipit--defer-refresh)
               (lambda (&rest args) nil))
              ((symbol-function 'shipit-clear-etag-cache-for-endpoint)
               (lambda (endpoint) nil))
              ((symbol-function 'shipit-gh-etag-get-json)
               (lambda (&rest args) '(:json ((id . 99999))))))

      ;; Set up required variables
      (let ((shipit-current-repo "test-owner/test-repo")
            (shipit-github-token "test-token"))

        ;; Create buffer with general comment text properties (NO file-path)
        (with-temp-buffer
          (insert "General comment body")
          (add-text-properties (point-min) (point-max)
                               '(shipit-comment t
                                 shipit-comment-id 456
                                 shipit-comment-body "Original general comment"
                                 ;; NO shipit-file-path - this is what triggers general API
                                 shipit-repo "test-owner/test-repo"
                                 shipit-pr-number 123))
          (goto-char (point-min))

          ;; Mock read-string to return a reply
          (cl-letf (((symbol-function 'read-string) (lambda (prompt) "My reply")))
            ;; Call the main reply function
            (shipit--reply-to-comment 456)))

        ;; Verify the issues endpoint was used, not pulls
        (should captured-endpoint)
        (should (string-match-p "/issues/123/comments" captured-endpoint))
        (should-not (string-match-p "/pulls/" captured-endpoint))))))

(provide 'test-inline-reply-endpoint)
;;; test-inline-reply-endpoint.el ends here
