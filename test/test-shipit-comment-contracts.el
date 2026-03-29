;;; test-shipit-comment-contracts.el --- Contract tests for comment backends -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies that GitHub and GitLab comment backends return data
;; satisfying the output contracts defined in shipit-comment-backends.el.
;; Each test mocks the HTTP layer with canned JSON, calls the real
;; backend function, then runs the contract verifier.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-comment-backends)
(require 'shipit-comment-github)
(require 'shipit-comment-gitlab)

;;; Test helpers

(defvar test-comment-config '(:repo "owner/repo")
  "Minimal GitHub config for comment contract tests.")

(defvar test-comment-gitlab-config
  '(:repo "group/project" :project-path "group/project"
    :api-url "https://gitlab.example.com")
  "Minimal GitLab config for comment contract tests.")

;;; Canned API responses

(defconst test-comment--github-issue-comment
  '((id . 1001)
    (body . "A general comment")
    (user . ((login . "octocat") (avatar_url . "https://example.com/avatar.png")))
    (created_at . "2025-01-01T00:00:00Z")
    (updated_at . "2025-01-01T00:00:00Z"))
  "Canned GitHub issue comment.")

(defconst test-comment--github-review-comment
  '((id . 2001)
    (body . "A review body")
    (user . ((login . "reviewer") (avatar_url . "https://example.com/avatar2.png")))
    (created_at . "2025-01-01T01:00:00Z")
    (state . "COMMENTED")
    (shipit-comment-type . "review"))
  "Canned GitHub review comment.")

(defconst test-comment--github-inline-comment
  '((id . 3001)
    (body . "Inline comment")
    (user . ((login . "devuser")))
    (created_at . "2025-01-02T00:00:00Z")
    (path . "src/main.el")
    (line . 42)
    (original_line . 42)
    (side . "RIGHT")
    (diff_hunk . "@@ -40,6 +40,8 @@"))
  "Canned GitHub inline comment.")

(defconst test-comment--github-reaction
  '((id . 5001)
    (content . "+1")
    (user . ((login . "reactor"))))
  "Canned GitHub reaction.")

(defconst test-comment--gitlab-discussion-general
  `((id . "disc-001")
    (notes . [((id . 4001)
               (body . "General note")
               (author . ((username . "gluser") (avatar_url . "https://gl.example.com/a.png")))
               (created_at . "2025-02-01T00:00:00Z")
               (updated_at . "2025-02-01T00:00:00Z")
               (system . :json-false))]))
  "Canned GitLab general discussion.")

(defconst test-comment--gitlab-discussion-inline
  `((id . "disc-002")
    (notes . [((id . 4002)
               (body . "Diff note")
               (author . ((username . "gldev") (avatar_url . "https://gl.example.com/b.png")))
               (created_at . "2025-02-02T00:00:00Z")
               (updated_at . "2025-02-02T00:00:00Z")
               (system . :json-false)
               (position . ((new_line . 10)
                            (old_line . nil)
                            (new_path . "lib/foo.el")
                            (old_path . "lib/foo.el"))))]))
  "Canned GitLab inline discussion.")

(defconst test-comment--gitlab-note-response
  '((id . 4010)
    (body . "Created note")
    (author . ((username . "gluser") (avatar_url . "https://gl.example.com/a.png")))
    (created_at . "2025-02-03T00:00:00Z")
    (updated_at . "2025-02-03T00:00:00Z")
    (system . :json-false))
  "Canned GitLab note response from POST.")

(defconst test-comment--gitlab-discussion-response
  `((id . "disc-003")
    (notes . [((id . 4011)
               (body . "Inline note")
               (author . ((username . "gldev") (avatar_url . "https://gl.example.com/b.png")))
               (created_at . "2025-02-04T00:00:00Z")
               (updated_at . "2025-02-04T00:00:00Z")
               (system . :json-false)
               (position . ((new_line . 5)
                            (old_line . nil)
                            (new_path . "lib/bar.el")
                            (old_path . "lib/bar.el"))))]))
  "Canned GitLab discussion response from POST (inline).")

(defconst test-comment--gitlab-award-emoji
  '((id . 6001)
    (name . "thumbsup")
    (user . ((username . "glreactor"))))
  "Canned GitLab award_emoji response.")

;;; GitHub contract tests

(ert-deftest test/comment-github-fetch-general-comments-contract ()
  ;; GIVEN canned issue + review comments from GitHub API
  ;; WHEN fetching general comments via the GitHub backend
  ;; THEN result satisfies the :fetch-general-comments contract
  (cl-letf (((symbol-function 'shipit--get-issue-comments)
             (lambda (_repo _number)
               (list test-comment--github-issue-comment)))
            ((symbol-function 'shipit--get-pr-reviews-for-comments)
             (lambda (_repo _number)
               (list test-comment--github-review-comment))))
    (let ((result (shipit-comment-github--fetch-general-comments
                   test-comment-config 1)))
      (shipit-comment--verify-contract :fetch-general-comments result))))

(ert-deftest test/comment-github-fetch-inline-comments-contract ()
  ;; GIVEN canned inline comments from GitHub API
  ;; WHEN fetching inline comments via the GitHub backend
  ;; THEN result satisfies the :fetch-inline-comments contract
  (cl-letf (((symbol-function 'shipit--api-request-paginated)
             (lambda (_endpoint)
               (list test-comment--github-inline-comment))))
    (let ((result (shipit-comment-github--fetch-inline-comments
                   test-comment-config 1)))
      (shipit-comment--verify-contract :fetch-inline-comments result))))

(ert-deftest test/comment-github-add-general-comment-contract ()
  ;; GIVEN a mock POST that returns a comment
  ;; WHEN adding a general comment via the GitHub backend
  ;; THEN result satisfies the :add-general-comment contract
  (cl-letf (((symbol-function 'shipit--api-request-post)
             (lambda (_endpoint _data &optional _method)
               test-comment--github-issue-comment)))
    (let ((result (shipit-comment-github--add-general-comment
                   test-comment-config 1 "test body")))
      (shipit-comment--verify-contract :add-general-comment result))))

(ert-deftest test/comment-github-add-inline-comment-contract ()
  ;; GIVEN a mock POST that returns an inline comment
  ;; WHEN adding an inline comment via the GitHub backend
  ;; THEN result satisfies the :add-inline-comment contract
  (cl-letf (((symbol-function 'shipit--api-request-post)
             (lambda (_endpoint _data &optional _method)
               test-comment--github-inline-comment))
            ((symbol-function 'shipit--get-pr-head-sha)
             (lambda (_number) "abc123")))
    (let ((result (shipit-comment-github--add-inline-comment
                   test-comment-config 1 "src/main.el" 42 "fix this" "RIGHT")))
      (shipit-comment--verify-contract :add-inline-comment result))))

(ert-deftest test/comment-github-reply-to-comment-contract ()
  ;; GIVEN a mock POST that returns a comment reply
  ;; WHEN replying to a comment via the GitHub backend
  ;; THEN result satisfies the :reply-to-comment contract
  (cl-letf (((symbol-function 'shipit--api-request-post)
             (lambda (_endpoint _data &optional _method)
               test-comment--github-issue-comment)))
    (let ((result (shipit-comment-github--reply-to-comment
                   test-comment-config 1 2001 "reply body")))
      (shipit-comment--verify-contract :reply-to-comment result))))

(ert-deftest test/comment-github-edit-comment-contract ()
  ;; GIVEN a mock PATCH that returns an edited comment
  ;; WHEN editing a comment via the GitHub backend
  ;; THEN result satisfies the :edit-comment contract
  (cl-letf (((symbol-function 'shipit--api-request-post)
             (lambda (_endpoint _data &optional _method)
               '((id . 1001) (body . "edited body")))))
    (let ((result (shipit-comment-github--edit-comment
                   test-comment-config 1001 "edited body")))
      (shipit-comment--verify-contract :edit-comment result))))

(ert-deftest test/comment-github-delete-comment-contract ()
  ;; GIVEN a mock DELETE that returns 204
  ;; WHEN deleting a comment via the GitHub backend
  ;; THEN result satisfies the :delete-comment contract (truthy)
  (cl-letf (((symbol-function 'shipit--url-retrieve-sync)
             (lambda (_url _method _headers &optional _data _status-only) 204)))
    (let ((result (shipit-comment-github--delete-comment
                   test-comment-config 1001)))
      (shipit-comment--verify-contract :delete-comment result))))

(ert-deftest test/comment-github-fetch-reactions-contract ()
  ;; GIVEN canned reactions from GitHub API
  ;; WHEN fetching reactions via the GitHub backend
  ;; THEN result satisfies the :fetch-reactions contract
  (cl-letf (((symbol-function 'shipit--api-request-paginated)
             (lambda (_endpoint)
               (list test-comment--github-reaction))))
    (let ((result (shipit-comment-github--fetch-reactions
                   test-comment-config 1001)))
      (shipit-comment--verify-contract :fetch-reactions result))))

(ert-deftest test/comment-github-toggle-reaction-contract ()
  ;; GIVEN a mock POST that returns a reaction
  ;; WHEN toggling a reaction via the GitHub backend
  ;; THEN result satisfies the :toggle-reaction contract (truthy)
  (cl-letf (((symbol-function 'shipit--api-request-post)
             (lambda (_endpoint _data &optional _method)
               test-comment--github-reaction)))
    (let ((result (shipit-comment-github--toggle-reaction
                   test-comment-config 1001 "+1")))
      (shipit-comment--verify-contract :toggle-reaction result))))

;;; GitLab contract tests

(ert-deftest test/comment-gitlab-fetch-general-comments-contract ()
  ;; GIVEN canned general discussions from GitLab API
  ;; WHEN fetching general comments via the GitLab backend
  ;; THEN result satisfies the :fetch-general-comments contract
  (cl-letf (((symbol-function 'shipit-comment-gitlab--fetch-discussions)
             (lambda (_config _number)
               (list test-comment--gitlab-discussion-general))))
    (let ((result (shipit-comment-gitlab--fetch-general-comments
                   test-comment-gitlab-config 1)))
      (shipit-comment--verify-contract :fetch-general-comments result))))

(ert-deftest test/comment-gitlab-fetch-inline-comments-contract ()
  ;; GIVEN canned inline discussions from GitLab API
  ;; WHEN fetching inline comments via the GitLab backend
  ;; THEN result satisfies the :fetch-inline-comments contract
  (cl-letf (((symbol-function 'shipit-comment-gitlab--fetch-discussions)
             (lambda (_config _number)
               (list test-comment--gitlab-discussion-inline))))
    (let ((result (shipit-comment-gitlab--fetch-inline-comments
                   test-comment-gitlab-config 1)))
      (shipit-comment--verify-contract :fetch-inline-comments result))))

(ert-deftest test/comment-gitlab-add-general-comment-contract ()
  ;; GIVEN a mock POST that returns a note
  ;; WHEN adding a general comment via the GitLab backend
  ;; THEN result satisfies the :add-general-comment contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-comment--gitlab-note-response)))
    (let ((result (shipit-comment-gitlab--add-general-comment
                   test-comment-gitlab-config 1 "test body")))
      (shipit-comment--verify-contract :add-general-comment result))))

(ert-deftest test/comment-gitlab-add-inline-comment-contract ()
  ;; GIVEN mock diff_refs and a discussion response
  ;; WHEN adding an inline comment via the GitLab backend
  ;; THEN result satisfies the :add-inline-comment contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               ;; diff_refs response
               '((diff_refs . ((base_sha . "aaa")
                               (head_sha . "bbb")
                               (start_sha . "ccc"))))))
            ((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-comment--gitlab-discussion-response)))
    (let ((result (shipit-comment-gitlab--add-inline-comment
                   test-comment-gitlab-config 1 "lib/bar.el" 5 "fix" "RIGHT")))
      (shipit-comment--verify-contract :add-inline-comment result))))

(ert-deftest test/comment-gitlab-reply-to-comment-contract ()
  ;; GIVEN a mock POST that returns a note
  ;; WHEN replying to a comment via the GitLab backend
  ;; THEN result satisfies the :reply-to-comment contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-comment--gitlab-note-response)))
    (let ((result (shipit-comment-gitlab--reply-to-comment
                   test-comment-gitlab-config 1 "disc-001" "reply body")))
      (shipit-comment--verify-contract :reply-to-comment result))))

(ert-deftest test/comment-gitlab-edit-comment-contract ()
  ;; GIVEN a mock PUT that returns an edited note
  ;; WHEN editing a comment via the GitLab backend
  ;; THEN result satisfies the :edit-comment contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               '((id . 4001)
                 (body . "edited note")
                 (author . ((username . "gluser")))
                 (created_at . "2025-02-01T00:00:00Z")))))
    (let ((result (shipit-comment-gitlab--edit-comment
                   test-comment-gitlab-config 4001 "edited note" nil 1)))
      (shipit-comment--verify-contract :edit-comment result))))

(ert-deftest test/comment-gitlab-delete-comment-contract ()
  ;; GIVEN a mock DELETE that succeeds
  ;; WHEN deleting a comment via the GitLab backend
  ;; THEN result satisfies the :delete-comment contract (truthy)
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method) nil))
            ((symbol-function 'shipit-comment-gitlab--current-mr-number)
             (lambda () 1)))
    (let ((result (shipit-comment-gitlab--delete-comment
                   test-comment-gitlab-config 4001)))
      (shipit-comment--verify-contract :delete-comment result))))

(ert-deftest test/comment-gitlab-fetch-reactions-contract ()
  ;; GIVEN canned award_emoji from GitLab API
  ;; WHEN fetching reactions via the GitLab backend
  ;; THEN result satisfies the :fetch-reactions contract
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               (list test-comment--gitlab-award-emoji)))
            ((symbol-function 'shipit-comment-gitlab--current-mr-number)
             (lambda () 1)))
    (let ((result (shipit-comment-gitlab--fetch-reactions
                   test-comment-gitlab-config 4001)))
      (shipit-comment--verify-contract :fetch-reactions result))))

(ert-deftest test/comment-gitlab-toggle-reaction-contract ()
  ;; GIVEN a mock POST that returns an award_emoji
  ;; WHEN toggling a reaction via the GitLab backend
  ;; THEN result satisfies the :toggle-reaction contract (truthy)
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
             (lambda (_config _path _data _method)
               test-comment--gitlab-award-emoji))
            ((symbol-function 'shipit-comment-gitlab--current-mr-number)
             (lambda () 1)))
    (let ((result (shipit-comment-gitlab--toggle-reaction
                   test-comment-gitlab-config 4001 "thumbsup")))
      (shipit-comment--verify-contract :toggle-reaction result))))

(provide 'test-shipit-comment-contracts)
;;; test-shipit-comment-contracts.el ends here
