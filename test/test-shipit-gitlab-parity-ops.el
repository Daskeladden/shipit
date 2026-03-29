;;; test-shipit-gitlab-parity-ops.el --- Tests for missing GitLab parity operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests (mocked API) for all GitLab operations that are missing
;; relative to GitHub — the operations in each *--parity-operations list.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-backends)
(require 'shipit-pr-gitlab)
(require 'shipit-comment-backends)
(require 'shipit-comment-gitlab)
(require 'shipit-issue-backends)
(require 'shipit-issue-gitlab)

;;; Test config

(defvar test-gl-parity-config
  '(:repo "group/project" :project-path "group/project"
    :api-url "https://gitlab.example.com")
  "Minimal GitLab config for parity operation tests.")

;;; Canned GitLab API responses

(defconst test-gl-parity--label
  '((id . 1) (name . "bug") (color . "#d73a4a") (description . "Something isn't working"))
  "Canned GitLab label response.")

(defconst test-gl-parity--label-2
  '((id . 2) (name . "enhancement") (color . "#a2eeef") (description . "New feature"))
  "Canned GitLab label response (second).")

(defconst test-gl-parity--member
  '((id . 10) (username . "alice") (name . "Alice") (avatar_url . "https://gl.example.com/a.png")
    (state . "active"))
  "Canned GitLab project member response.")

(defconst test-gl-parity--member-2
  '((id . 11) (username . "bob") (name . "Bob") (avatar_url . "https://gl.example.com/b.png")
    (state . "active"))
  "Canned GitLab project member response (second).")

(defconst test-gl-parity--mr-with-labels
  '((iid . 5)
    (title . "Add feature")
    (description . "")
    (state . "opened")
    (author . ((username . "alice") (avatar_url . "https://gl.example.com/a.png")))
    (source_branch . "feature")
    (target_branch . "main")
    (sha . "abc123")
    (diff_refs . ((start_sha . "aaa") (head_sha . "abc123") (base_sha . "bbb")))
    (web_url . "https://gitlab.example.com/group/project/-/merge_requests/5")
    (created_at . "2026-01-01T00:00:00Z")
    (labels . ["bug" "enhancement"])
    (assignees . [])
    (draft . :json-false))
  "Canned GitLab MR response with labels.")

(defconst test-gl-parity--mr-with-assignees
  '((iid . 5)
    (title . "Add feature")
    (description . "")
    (state . "opened")
    (author . ((username . "alice") (avatar_url . "https://gl.example.com/a.png")))
    (source_branch . "feature")
    (target_branch . "main")
    (sha . "abc123")
    (diff_refs . ((start_sha . "aaa") (head_sha . "abc123") (base_sha . "bbb")))
    (web_url . "https://gitlab.example.com/group/project/-/merge_requests/5")
    (created_at . "2026-01-01T00:00:00Z")
    (labels . [])
    (assignees . [((username . "alice") (avatar_url . "https://gl.example.com/a.png"))])
    (draft . :json-false))
  "Canned GitLab MR response with assignees.")

(defconst test-gl-parity--approval-state
  '((rules . [((id . 1) (name . "All Members")
               (eligible_approvers . [((username . "alice")) ((username . "bob"))])
               (approved_by . [((username . "alice"))]))])
    (approved_by . [((username . "alice"))]))
  "Canned GitLab MR approval state response.")

(defconst test-gl-parity--commit
  '((id . "abc123def456789")
    (message . "Fix widget rendering")
    (author_name . "GL User")
    (author_email . "gl@example.com")
    (created_at . "2026-01-01T00:00:00Z")
    (web_url . "https://gitlab.example.com/group/project/-/commit/abc123def456789")
    (stats . ((additions . 10) (deletions . 3) (total . 13)))
    (last_pipeline . nil))
  "Canned GitLab commit detail response.")

(defconst test-gl-parity--commit-diff
  '((old_path . "src/widget.el")
    (new_path . "src/widget.el")
    (new_file . :json-false)
    (deleted_file . :json-false)
    (renamed_file . :json-false)
    (diff . "@@ -1,3 +1,10 @@\n+new line"))
  "Canned GitLab commit diff entry.")

(defconst test-gl-parity--discussion
  '((id . "disc-abc")
    (notes . [((id . 100)
               (body . "Looks good")
               (author . ((username . "bob") (avatar_url . "https://gl.example.com/b.png")))
               (system . :json-false)
               (type . "DiffNote")
               (created_at . "2026-01-01T12:00:00Z")
               (updated_at . "2026-01-01T12:00:00Z")
               (position . ((new_path . "src/main.el")
                            (new_line . 10)
                            (old_line . nil)
                            (base_sha . "aaa")
                            (head_sha . "bbb")
                            (start_sha . "ccc"))))]))
  "Canned GitLab inline discussion for reply-to-inline test.")

(defconst test-gl-parity--note-reply
  '((id . 200)
    (body . "Thanks for the review!")
    (author . ((username . "alice") (avatar_url . "https://gl.example.com/a.png")))
    (system . :json-false)
    (type . "DiffNote")
    (created_at . "2026-01-02T00:00:00Z")
    (updated_at . "2026-01-02T00:00:00Z")
    (position . ((new_path . "src/main.el")
                 (new_line . 10)
                 (old_line . nil)
                 (base_sha . "aaa")
                 (head_sha . "bbb")
                 (start_sha . "ccc"))))
  "Canned GitLab note reply (returned by reply-to-inline).")

(defconst test-gl-parity--issue-note
  '((id . 300)
    (body . "Updated comment text")
    (author . ((username . "alice") (avatar_url . "https://gl.example.com/a.png")))
    (system . :json-false)
    (created_at . "2026-01-01T00:00:00Z")
    (updated_at . "2026-01-02T00:00:00Z"))
  "Canned GitLab issue note (for edit-comment test).")

(defconst test-gl-parity--award-emoji
  '((id . 50)
    (name . "thumbsup")
    (user . ((username . "alice"))))
  "Canned GitLab award_emoji response.")

(defconst test-gl-parity--issue-raw
  '((iid . 7)
    (title . "Extended issue")
    (description . "Created with labels")
    (state . "opened")
    (author . ((username . "alice") (avatar_url . "https://gl.example.com/a.png")))
    (web_url . "https://gitlab.example.com/group/project/-/issues/7")
    (created_at . "2026-01-01T00:00:00Z")
    (updated_at . "2026-01-01T00:00:00Z")
    (labels . ["bug"])
    (assignees . [((username . "alice"))]))
  "Canned GitLab issue response for create-issue-extended.")


;;;; ============================================================
;;;; PR backend — Labels (5 operations)
;;;; ============================================================

(ert-deftest test/gl-parity-add-label ()
  ;; GIVEN a GitLab MR #5 and a label "bug"
  ;; WHEN adding the label via the backend
  ;; THEN the API is called with PUT and label data, and MR is returned normalized
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-gl-parity--mr-with-labels))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--add-label test-gl-parity-config 5 "bug")))
      (should result)
      (should (equal (cdr (assq 'number result)) 5))
      (should (assq 'labels result)))))

(ert-deftest test/gl-parity-remove-label ()
  ;; GIVEN a GitLab MR #5 with label "bug"
  ;; WHEN removing the label
  ;; THEN the API is called with PUT and returns normalized MR
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-gl-parity--mr-with-labels))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--remove-label test-gl-parity-config 5 "bug")))
      (should result)
      (should (equal (cdr (assq 'number result)) 5)))))

(ert-deftest test/gl-parity-fetch-labels ()
  ;; GIVEN a GitLab MR #5
  ;; WHEN fetching its labels
  ;; THEN returns minimal PR data with labels list
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--fetch-labels test-gl-parity-config 5)))
      (should result)
      (should (equal (cdr (assq 'number result)) 5))
      (should (cdr (assq 'labels result))))))

(ert-deftest test/gl-parity-set-labels ()
  ;; GIVEN a GitLab MR #5
  ;; WHEN setting labels to ("bug" "enhancement")
  ;; THEN the API is called with PUT and label list, returns normalized MR
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--set-labels
                   test-gl-parity-config 5 '("bug" "enhancement"))))
      (should result)
      (should (equal (cdr (assq 'number result)) 5)))))

(ert-deftest test/gl-parity-fetch-available-labels ()
  ;; GIVEN a GitLab project
  ;; WHEN fetching available labels
  ;; THEN returns list of label alists with name key
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config _path)
               (list test-gl-parity--label test-gl-parity--label-2))))
    (let ((result (shipit-pr-gitlab--fetch-available-labels test-gl-parity-config)))
      (should (= (length result) 2))
      (should (equal (cdr (assq 'name (car result))) "bug")))))

;;;; ============================================================
;;;; PR backend — Reviewers (4 operations)
;;;; ============================================================

(ert-deftest test/gl-parity-add-reviewer ()
  ;; GIVEN a GitLab MR #5
  ;; WHEN adding reviewer "alice"
  ;; THEN the API is called and returns result
  (cl-letf (((symbol-function 'shipit-pr-gitlab--resolve-user-id)
             (lambda (_config _username) 10))
            ((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-gl-parity--mr-with-labels))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--add-reviewer test-gl-parity-config 5 "alice")))
      (should result))))

(ert-deftest test/gl-parity-remove-reviewer ()
  ;; GIVEN a GitLab MR #5 with reviewer "alice"
  ;; WHEN removing reviewer "alice"
  ;; THEN the API is called and returns result
  (cl-letf (((symbol-function 'shipit-pr-gitlab--resolve-user-id)
             (lambda (_config _username) 10))
            ((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-gl-parity--mr-with-labels))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--remove-reviewer test-gl-parity-config 5 "alice")))
      (should result))))

(ert-deftest test/gl-parity-add-reviewers-batch ()
  ;; GIVEN a GitLab MR #5
  ;; WHEN adding reviewers ("alice" "bob") in batch
  ;; THEN the API is called with the full user ID list
  (cl-letf (((symbol-function 'shipit-pr-gitlab--resolve-user-id)
             (lambda (_config _username) 10))
            ((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-gl-parity--mr-with-labels))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--add-reviewers-batch
                   test-gl-parity-config 5 '("alice" "bob"))))
      (should result))))

(ert-deftest test/gl-parity-fetch-requested-reviewers ()
  ;; GIVEN a GitLab MR #5 with approval state
  ;; WHEN fetching requested reviewers
  ;; THEN returns reviewer data from approval rules
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               test-gl-parity--approval-state)))
    (let ((result (shipit-pr-gitlab--fetch-requested-reviewers
                   test-gl-parity-config 5)))
      (should result)
      ;; Should return a structure with users list
      (should (cdr (assq 'users result))))))

;;;; ============================================================
;;;; PR backend — Assignees (5 operations)
;;;; ============================================================

(ert-deftest test/gl-parity-add-assignee ()
  ;; GIVEN a GitLab MR #5
  ;; WHEN adding assignee "alice"
  ;; THEN returns normalized MR with assignees
  (cl-letf (((symbol-function 'shipit-pr-gitlab--resolve-user-id)
             (lambda (_config _username) 10))
            ((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-gl-parity--mr-with-labels))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-assignees)))
    (let ((result (shipit-pr-gitlab--add-assignee test-gl-parity-config 5 "alice")))
      (should result))))

(ert-deftest test/gl-parity-remove-assignee ()
  ;; GIVEN a GitLab MR #5 with assignee "alice"
  ;; WHEN removing assignee "alice"
  ;; THEN returns normalized MR
  (cl-letf (((symbol-function 'shipit-pr-gitlab--resolve-user-id)
             (lambda (_config _username) 10))
            ((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               test-gl-parity--mr-with-assignees))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-labels)))
    (let ((result (shipit-pr-gitlab--remove-assignee test-gl-parity-config 5 "alice")))
      (should result))))

(ert-deftest test/gl-parity-add-assignees-batch ()
  ;; GIVEN a GitLab MR #5
  ;; WHEN adding assignees ("alice" "bob") in batch
  ;; THEN the API is called with the user ID list
  (cl-letf (((symbol-function 'shipit-pr-gitlab--resolve-user-id)
             (lambda (_config _username) 10))
            ((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) test-gl-parity--mr-with-labels))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--mr-with-assignees)))
    (let ((result (shipit-pr-gitlab--add-assignees-batch
                   test-gl-parity-config 5 '("alice" "bob"))))
      (should result))))

(ert-deftest test/gl-parity-fetch-pr-assignees ()
  ;; GIVEN a GitLab MR #5 with assignees
  ;; WHEN fetching PR assignees
  ;; THEN returns normalized assignee list
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               test-gl-parity--mr-with-assignees)))
    (let ((result (shipit-pr-gitlab--fetch-pr-assignees test-gl-parity-config 5)))
      (should (listp result))
      (should (cdr (assq 'login (car result)))))))

(ert-deftest test/gl-parity-fetch-available-assignees ()
  ;; GIVEN a GitLab project with members
  ;; WHEN fetching available assignees
  ;; THEN returns list of member alists
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config _path)
               (list test-gl-parity--member test-gl-parity--member-2))))
    (let ((result (shipit-pr-gitlab--fetch-available-assignees test-gl-parity-config)))
      (should (= (length result) 2))
      (should (cdr (assq 'login (car result)))))))

;;;; ============================================================
;;;; PR backend — Reviews & Content (2 operations)
;;;; ============================================================

(ert-deftest test/gl-parity-dismiss-review ()
  ;; GIVEN a GitLab MR #5 that was previously approved
  ;; WHEN dismissing the review with a message
  ;; THEN unapproves the MR via POST to unapprove endpoint
  (let ((called-method nil)
        (called-path nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path _data method)
                 (setq called-path path called-method method)
                 t)))
      (let ((result (shipit-pr-gitlab--dismiss-review
                     test-gl-parity-config 5 "Needs changes")))
        (should result)
        (should (string= called-method "POST"))
        (should (string-match "unapprove" called-path))))))

(ert-deftest test/gl-parity-fetch-commit ()
  ;; GIVEN a commit SHA in the GitLab project
  ;; WHEN fetching commit details
  ;; THEN returns commit data with files list
  (let ((call-paths nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (push path call-paths)
                 (if (string-match "/diff" path)
                     (vector test-gl-parity--commit-diff)
                   test-gl-parity--commit))))
      (let ((result (shipit-pr-gitlab--fetch-commit
                     test-gl-parity-config "abc123def456789")))
        (should result)
        (should (cdr (assq 'sha result)))
        (should (cdr (assq 'files result)))))))

;;;; ============================================================
;;;; Comment backend — reply-to-inline (1 operation)
;;;; ============================================================

(ert-deftest test/gl-parity-reply-to-inline ()
  ;; GIVEN a GitLab MR #5 with an inline discussion disc-abc
  ;; WHEN replying to that inline discussion with file context
  ;; THEN posts to the discussion notes endpoint and returns normalized note
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               ;; Return discussions list when fetching
               (vector test-gl-parity--discussion)))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--note-reply)))
    (let ((result (shipit-comment-gitlab--reply-to-inline
                   test-gl-parity-config 5 "disc-abc"
                   "Thanks for the review!" "src/main.el")))
      (should result)
      (should (cdr (assq 'id result)))
      (should (cdr (assq 'body result))))))

;;;; ============================================================
;;;; Issue backend — Comment & Reaction operations (3 operations)
;;;; ============================================================

(ert-deftest test/gl-parity-issue-edit-comment ()
  ;; GIVEN a GitLab issue #7 with comment #300 in an active issue buffer
  ;; WHEN editing the comment body
  ;; THEN calls PUT on the note endpoint and returns normalized comment
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--issue-note)))
    (let ((shipit-issue-buffer-number 7))
      (let ((result (shipit-issue-gitlab--edit-comment
                     test-gl-parity-config 300 "Updated comment text")))
        (should result)
        (should (equal (cdr (assq 'id result)) 300))
        (should (equal (cdr (assq 'body result)) "Updated comment text"))))))

(ert-deftest test/gl-parity-issue-toggle-reaction ()
  ;; GIVEN a GitLab issue #7 with comment #300 in an active issue buffer
  ;; WHEN toggling a "+1" reaction
  ;; THEN calls POST to award_emoji endpoint and returns normalized reaction
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--award-emoji)))
    (let ((shipit-issue-buffer-number 7))
      (let ((result (shipit-issue-gitlab--toggle-reaction
                     test-gl-parity-config 300 "+1")))
        (should result)))))

(ert-deftest test/gl-parity-issue-update-description ()
  ;; GIVEN a GitLab issue #7
  ;; WHEN updating its description
  ;; THEN calls PUT on the issue endpoint and returns normalized issue
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-gl-parity--issue-raw)))
    (let ((result (shipit-issue-gitlab--update-description
                   test-gl-parity-config 7 "New description")))
      (should result)
      (should (cdr (assq 'title result))))))

;;;; ============================================================
;;;; Issue backend — Creation fields (2 operations)
;;;; ============================================================

(ert-deftest test/gl-parity-issue-creation-fields ()
  ;; GIVEN a GitLab backend config
  ;; WHEN requesting creation field descriptors
  ;; THEN returns list with title, body, labels, assignees fields
  (let ((fields (shipit-issue-gitlab--creation-fields test-gl-parity-config)))
    (should (>= (length fields) 4))
    (let ((names (mapcar (lambda (f) (plist-get f :name)) fields)))
      (should (memq 'title names))
      (should (memq 'body names))
      (should (memq 'labels names))
      (should (memq 'assignees names)))))

(ert-deftest test/gl-parity-issue-create-extended ()
  ;; GIVEN a GitLab project and extended fields (title, body, labels, assignees)
  ;; WHEN creating an issue with extended fields
  ;; THEN calls POST with all fields and returns normalized issue
  (let ((posted-data nil))
    (cl-letf (((symbol-function 'shipit-issue-gitlab--resolve-user-id)
               (lambda (_config _username) 10))
              ((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config _path data _method)
                 (setq posted-data data)
                 test-gl-parity--issue-raw)))
      (let ((result (shipit-issue-gitlab--create-issue-extended
                     test-gl-parity-config
                     '((title . "Extended issue")
                       (body . "Created with labels")
                       (labels . ("bug"))
                       (assignees . ("alice"))))))
        (should result)
        (should (cdr (assq 'title result)))
        ;; Verify correct GitLab field names were sent
        (should (cdr (assq 'description posted-data)))
        (should (cdr (assq 'labels posted-data)))))))

(provide 'test-shipit-gitlab-parity-ops)
;;; test-shipit-gitlab-parity-ops.el ends here
