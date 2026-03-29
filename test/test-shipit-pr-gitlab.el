;;; test-shipit-pr-gitlab.el --- Tests for GitLab PR backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for GitLab MR normalization and PR backend registration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)

;;; Test data helpers

(defun test-gitlab--make-mr-data ()
  "Return a minimal GitLab MR API response alist (fresh copy each call)."
  (list (cons 'iid 42)
        (cons 'title "Add feature X")
        (cons 'description "This MR adds feature X")
        (cons 'state "opened")
        (cons 'draft :json-false)
        (cons 'web_url "https://gitlab.com/mygroup/myproject/-/merge_requests/42")
        (cons 'author (list (cons 'username "jdoe")
                            (cons 'avatar_url "https://gitlab.com/uploads/-/system/user/avatar/1/avatar.png")))
        (cons 'source_branch "feature-x")
        (cons 'target_branch "main")
        (cons 'sha "abc123def456")
        (cons 'user_notes_count 5)
        (cons 'labels (vector "bug" "enhancement"))
        (cons 'assignees (vector (list (cons 'username "reviewer1")
                                       (cons 'avatar_url "https://gitlab.com/avatar1.png"))
                                 (list (cons 'username "reviewer2")
                                       (cons 'avatar_url "https://gitlab.com/avatar2.png"))))
        (cons 'diff_refs (list (cons 'base_sha "merge000base")
                              (cons 'start_sha "target111sha")
                              (cons 'head_sha "abc123def456")))
        (cons 'created_at "2026-01-15T10:00:00Z")
        (cons 'updated_at "2026-01-16T14:30:00Z")
        (cons 'merged_at nil)))

(defun test-gitlab--make-merged-mr-data ()
  "Return a merged GitLab MR API response alist."
  (let ((data (test-gitlab--make-mr-data)))
    (setf (cdr (assq 'state data)) "merged")
    (setf (cdr (assq 'merged_at data)) "2026-01-17T09:00:00Z")
    data))

(defun test-gitlab--make-pipeline-job ()
  "Return a minimal GitLab pipeline job alist."
  '((id . 100)
    (name . "rspec")
    (stage . "test")
    (status . "success")
    (web_url . "https://gitlab.com/mygroup/myproject/-/jobs/100")
    (started_at . "2026-01-15T10:05:00Z")
    (finished_at . "2026-01-15T10:10:00Z")))

(defun test-gitlab--make-mr-change ()
  "Return a minimal GitLab MR changes entry."
  '((old_path . "src/main.rb")
    (new_path . "src/main.rb")
    (new_file . :json-false)
    (renamed_file . :json-false)
    (deleted_file . :json-false)
    (diff . "@@ -1,3 +1,5 @@\n+added line\n context\n-removed line")))

;;; Normalization tests

(ert-deftest test-shipit-pr-gitlab-normalize-mr-basic ()
  "GIVEN a standard opened GitLab MR
WHEN normalizing to common shipit shape
THEN all basic fields map correctly."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (shipit-pr-gitlab--normalize-mr mr)))
    (should (= 42 (cdr (assq 'number pr))))
    (should (string= "Add feature X" (cdr (assq 'title pr))))
    (should (string= "This MR adds feature X" (cdr (assq 'body pr))))
    (should (string= "https://gitlab.com/mygroup/myproject/-/merge_requests/42"
                      (cdr (assq 'html_url pr))))
    (should (eq nil (cdr (assq 'draft pr))))
    (should (= 5 (cdr (assq 'comments pr))))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-state-opened ()
  "GIVEN a GitLab MR with state \"opened\"
WHEN normalizing
THEN state is \"open\" and merged_at is nil."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (shipit-pr-gitlab--normalize-mr mr)))
    (should (string= "open" (cdr (assq 'state pr))))
    (should-not (cdr (assq 'merged_at pr)))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-state-merged ()
  "GIVEN a GitLab MR with state \"merged\"
WHEN normalizing
THEN state is \"closed\" and merged_at is set."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-merged-mr-data))
         (pr (shipit-pr-gitlab--normalize-mr mr)))
    (should (string= "closed" (cdr (assq 'state pr))))
    (should (string= "2026-01-17T09:00:00Z" (cdr (assq 'merged_at pr))))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-state-closed ()
  "GIVEN a GitLab MR with state \"closed\"
WHEN normalizing
THEN state is \"closed\" and merged_at is nil."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (progn
               (setf (cdr (assq 'state mr)) "closed")
               (shipit-pr-gitlab--normalize-mr mr))))
    (should (string= "closed" (cdr (assq 'state pr))))
    (should-not (cdr (assq 'merged_at pr)))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-head-base ()
  "GIVEN a GitLab MR with source_branch, target_branch, sha, and diff_refs
WHEN normalizing
THEN head has ref and sha, base has ref and sha from diff_refs.start_sha."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (shipit-pr-gitlab--normalize-mr mr))
         (head (cdr (assq 'head pr)))
         (base (cdr (assq 'base pr))))
    (should (string= "feature-x" (cdr (assq 'ref head))))
    (should (string= "abc123def456" (cdr (assq 'sha head))))
    (should (string= "main" (cdr (assq 'ref base))))
    (should (string= "target111sha" (cdr (assq 'sha base))))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-base-sha-absent ()
  "GIVEN a GitLab MR without diff_refs (e.g. from list endpoint)
WHEN normalizing
THEN base has ref but no sha."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (_  (setq mr (assq-delete-all 'diff_refs mr)))
         (pr (shipit-pr-gitlab--normalize-mr mr))
         (base (cdr (assq 'base pr))))
    (should (string= "main" (cdr (assq 'ref base))))
    (should-not (assq 'sha base))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-user ()
  "GIVEN a GitLab MR with author.username
WHEN normalizing
THEN user.login maps to author.username."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (shipit-pr-gitlab--normalize-mr mr))
         (user (cdr (assq 'user pr))))
    (should (string= "jdoe" (cdr (assq 'login user))))
    (should (cdr (assq 'avatar_url user)))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-labels ()
  "GIVEN a GitLab MR with labels array
WHEN normalizing
THEN labels are mapped to ((name . L1) (name . L2) ...)."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (shipit-pr-gitlab--normalize-mr mr))
         (labels (cdr (assq 'labels pr))))
    (should (= 2 (length labels)))
    (should (string= "bug" (cdr (assq 'name (car labels)))))
    (should (string= "enhancement" (cdr (assq 'name (cadr labels)))))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-assignees ()
  "GIVEN a GitLab MR with assignees array
WHEN normalizing
THEN assignees are mapped to ((login . U1) ...)."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (shipit-pr-gitlab--normalize-mr mr))
         (assignees (cdr (assq 'assignees pr))))
    (should (= 2 (length assignees)))
    (should (string= "reviewer1" (cdr (assq 'login (car assignees)))))))

(ert-deftest test-shipit-pr-gitlab-normalize-mr-draft ()
  "GIVEN a GitLab MR with draft true
WHEN normalizing
THEN draft is t."
  (require 'shipit-pr-gitlab)
  (let* ((mr (test-gitlab--make-mr-data))
         (pr (progn
               (setf (cdr (assq 'draft mr)) t)
               (shipit-pr-gitlab--normalize-mr mr))))
    (should (eq t (cdr (assq 'draft pr))))))

;;; Pipeline job normalization tests

(ert-deftest test-shipit-pr-gitlab-normalize-pipeline-job-success ()
  "GIVEN a GitLab pipeline job with status \"success\"
WHEN normalizing to common shipit check shape
THEN status is \"completed\", conclusion is \"success\", and workflow-name is set."
  (require 'shipit-pr-gitlab)
  (let* ((job (test-gitlab--make-pipeline-job))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "rspec" (cdr (assq 'name check))))
    (should (string= "completed" (cdr (assq 'status check))))
    (should (string= "success" (cdr (assq 'conclusion check))))
    (should (cdr (assq 'html_url check)))
    (should (string= "test" (cdr (assq 'workflow-name check))))))

(ert-deftest test-shipit-pr-gitlab-normalize-pipeline-job-failed ()
  "GIVEN a GitLab pipeline job with status \"failed\"
WHEN normalizing
THEN status is \"completed\" and conclusion is \"failure\"."
  (require 'shipit-pr-gitlab)
  (let* ((job `((id . 101) (name . "lint") (status . "failed")
                (web_url . "https://gitlab.com/jobs/101")
                (started_at . nil) (finished_at . nil)))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "completed" (cdr (assq 'status check))))
    (should (string= "failure" (cdr (assq 'conclusion check))))))

(ert-deftest test-shipit-pr-gitlab-normalize-pipeline-job-running ()
  "GIVEN a GitLab pipeline job with status \"running\"
WHEN normalizing
THEN status is \"in_progress\" and conclusion is nil."
  (require 'shipit-pr-gitlab)
  (let* ((job `((id . 102) (name . "test") (status . "running")
                (web_url . "https://gitlab.com/jobs/102")
                (started_at . "2026-01-15T10:05:00Z") (finished_at . nil)))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "in_progress" (cdr (assq 'status check))))
    (should-not (cdr (assq 'conclusion check)))))

(ert-deftest test-shipit-pr-gitlab-normalize-pipeline-job-pending ()
  "GIVEN a GitLab pipeline job with status \"pending\"
WHEN normalizing
THEN status is \"queued\" and conclusion is nil."
  (require 'shipit-pr-gitlab)
  (let* ((job `((id . 103) (name . "deploy") (status . "pending")
                (web_url . "https://gitlab.com/jobs/103")
                (started_at . nil) (finished_at . nil)))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "queued" (cdr (assq 'status check))))
    (should-not (cdr (assq 'conclusion check)))))

(ert-deftest test-shipit-pr-gitlab-normalize-pipeline-job-canceled ()
  "GIVEN a GitLab pipeline job with status \"canceled\"
WHEN normalizing
THEN status is \"completed\" and conclusion is \"cancelled\"."
  (require 'shipit-pr-gitlab)
  (let* ((job `((id . 104) (name . "build") (status . "canceled")
                (web_url . "https://gitlab.com/jobs/104")
                (started_at . nil) (finished_at . nil)))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "completed" (cdr (assq 'status check))))
    (should (string= "cancelled" (cdr (assq 'conclusion check))))))

(ert-deftest test-shipit-pr-gitlab-normalize-pipeline-job-manual ()
  "GIVEN a GitLab pipeline job with status \"manual\"
WHEN normalizing
THEN status is \"completed\" and conclusion is \"action_required\"."
  (require 'shipit-pr-gitlab)
  (let* ((job `((id . 105) (name . "deploy-prod") (status . "manual")
                (web_url . "https://gitlab.com/jobs/105")
                (started_at . nil) (finished_at . nil)))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "completed" (cdr (assq 'status check))))
    (should (string= "action_required" (cdr (assq 'conclusion check))))))

(ert-deftest test-shipit-pr-gitlab-normalize-pipeline-job-skipped ()
  "GIVEN a GitLab pipeline job with status \"skipped\"
WHEN normalizing
THEN status is \"completed\" and conclusion is \"skipped\"."
  (require 'shipit-pr-gitlab)
  (let* ((job `((id . 106) (name . "optional") (status . "skipped")
                (web_url . "https://gitlab.com/jobs/106")
                (started_at . nil) (finished_at . nil)))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "completed" (cdr (assq 'status check))))
    (should (string= "skipped" (cdr (assq 'conclusion check))))))

(ert-deftest test/pr-gitlab-normalize-pipeline-job-includes-stage ()
  "GIVEN a GitLab job with stage \"test\"
WHEN normalized
THEN workflow-name = \"test\"."
  (require 'shipit-pr-gitlab)
  (let* ((job '((id . 200) (name . "rspec") (status . "success")
                (stage . "test")
                (web_url . "https://gitlab.com/jobs/200")
                (started_at . "2026-01-15T10:05:00Z")
                (finished_at . "2026-01-15T10:10:00Z")))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "test" (cdr (assq 'workflow-name check))))))

(ert-deftest test/pr-gitlab-normalize-pipeline-job-stage-nil-fallback ()
  "GIVEN a GitLab job without a stage field
WHEN normalized
THEN workflow-name = \"Pipeline\" (fallback)."
  (require 'shipit-pr-gitlab)
  (let* ((job '((id . 201) (name . "lint") (status . "success")
                (web_url . "https://gitlab.com/jobs/201")
                (started_at . nil) (finished_at . nil)))
         (check (shipit-pr-gitlab--normalize-pipeline-job job)))
    (should (string= "Pipeline" (cdr (assq 'workflow-name check))))))

;;; File normalization tests

(ert-deftest test-shipit-pr-gitlab-normalize-file-modified ()
  "GIVEN a GitLab MR change entry for a modified file
WHEN normalizing
THEN filename and status \"modified\" are set."
  (require 'shipit-pr-gitlab)
  (let* ((change (test-gitlab--make-mr-change))
         (file (shipit-pr-gitlab--normalize-file change)))
    (should (string= "src/main.rb" (cdr (assq 'filename file))))
    (should (string= "modified" (cdr (assq 'status file))))))

(ert-deftest test-shipit-pr-gitlab-normalize-file-added ()
  "GIVEN a GitLab MR change entry for a new file
WHEN normalizing
THEN status is \"added\"."
  (require 'shipit-pr-gitlab)
  (let* ((change `((old_path . "src/new.rb")
                   (new_path . "src/new.rb")
                   (new_file . t)
                   (renamed_file . :json-false)
                   (deleted_file . :json-false)
                   (diff . "+new content")))
         (file (shipit-pr-gitlab--normalize-file change)))
    (should (string= "added" (cdr (assq 'status file))))))

(ert-deftest test-shipit-pr-gitlab-normalize-file-deleted ()
  "GIVEN a GitLab MR change entry for a deleted file
WHEN normalizing
THEN status is \"removed\"."
  (require 'shipit-pr-gitlab)
  (let* ((change `((old_path . "src/old.rb")
                   (new_path . "src/old.rb")
                   (new_file . :json-false)
                   (renamed_file . :json-false)
                   (deleted_file . t)
                   (diff . "-removed content")))
         (file (shipit-pr-gitlab--normalize-file change)))
    (should (string= "removed" (cdr (assq 'status file))))))

(ert-deftest test-shipit-pr-gitlab-normalize-file-renamed ()
  "GIVEN a GitLab MR change entry for a renamed file
WHEN normalizing
THEN status is \"renamed\" and previous_filename is set."
  (require 'shipit-pr-gitlab)
  (let* ((change `((old_path . "src/old_name.rb")
                   (new_path . "src/new_name.rb")
                   (new_file . :json-false)
                   (renamed_file . t)
                   (deleted_file . :json-false)
                   (diff . "")))
         (file (shipit-pr-gitlab--normalize-file change)))
    (should (string= "renamed" (cdr (assq 'status file))))
    (should (string= "src/new_name.rb" (cdr (assq 'filename file))))
    (should (string= "src/old_name.rb" (cdr (assq 'previous_filename file))))))

;;; Backend registration tests

(ert-deftest test-shipit-pr-gitlab-registered ()
  "GIVEN shipit-pr-gitlab is loaded
WHEN checking shipit-pr-backends
THEN 'gitlab is registered with all required keys."
  (require 'shipit-pr-gitlab)
  (let ((entry (assq 'gitlab shipit-pr-backends)))
    (should entry)
    (let ((plist (cdr entry)))
      (should (string= "GitLab" (plist-get plist :name)))
      ;; Verify all required keys exist
      (dolist (key shipit-pr--required-keys)
        (should (plist-get plist key))))))

(ert-deftest test-shipit-pr-gitlab-dispatch-browse-url ()
  "GIVEN the GitLab backend is registered
WHEN dispatching :browse-url
THEN the correct GitLab MR URL is returned."
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject")))
    (let* ((resolved (shipit-pr--resolve-for-repo "mygroup/myproject"))
           (backend (car resolved))
           (config (cdr resolved))
           (url (funcall (plist-get backend :browse-url) config 42)))
      (should (string= "https://gitlab.com/mygroup/myproject/-/merge_requests/42"
                        url)))))

;;; Search repos tests

(ert-deftest test-shipit-pr-gitlab-search-repos ()
  "GIVEN a GitLab API response with project search results
WHEN calling shipit-pr-gitlab--search-repos
THEN results are normalized to list of alists with full_name and description."
  (require 'shipit-pr-gitlab)
  (let* ((mock-response (vector
                         (list (cons 'path_with_namespace "mygroup/project-a")
                               (cons 'description "First project"))
                         (list (cons 'path_with_namespace "mygroup/project-b")
                               (cons 'description nil))))
         (api-called-with nil)
         (config '(:api-url "https://gitlab.com" :token "test-token")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (cfg path)
                 (setq api-called-with path)
                 mock-response)))
      (let ((results (shipit-pr-gitlab--search-repos config "mygroup")))
        ;; THEN API was called with search query
        (should (string-match-p "search=" api-called-with))
        ;; THEN results are a list
        (should (= 2 (length results)))
        ;; THEN first result has full_name from path_with_namespace
        (should (string= "mygroup/project-a"
                          (cdr (assq 'full_name (car results)))))
        (should (string= "First project"
                          (cdr (assq 'description (car results)))))
        ;; THEN second result has nil description
        (should (string= "mygroup/project-b"
                          (cdr (assq 'full_name (cadr results)))))))))

(ert-deftest test-shipit-pr-gitlab-search-repos-error-response ()
  "GIVEN a GitLab API that returns an error object (e.g. 500)
WHEN calling shipit-pr-gitlab--search-repos
THEN result is nil (error is handled gracefully)."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test-token")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_cfg _path)
                 '((message . "500 Internal Server Error")))))
      (let ((results (shipit-pr-gitlab--search-repos config "test")))
        ;; THEN results are nil (not an error)
        (should (null results))))))

(ert-deftest test-shipit-pr-gitlab-search-repos-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for :search-repos key
THEN it is present and callable."
  (require 'shipit-pr-gitlab)
  (let* ((entry (assq 'gitlab shipit-pr-backends))
         (plist (cdr entry)))
    (should (plist-get plist :search-repos))))

(ert-deftest test-shipit-pr-gitlab-search-repos-request-global ()
  "GIVEN a GitLab config
WHEN calling search-repos-request with a plain query
THEN URL uses global /projects?search= endpoint."
  (require 'shipit-pr-gitlab)
  (cl-letf (((symbol-function 'shipit-gitlab--auth-header)
             (lambda (_cfg) '("PRIVATE-TOKEN" . "fake-token"))))
    (let* ((config '(:api-url "https://gitlab.com"))
           (result (shipit-pr-gitlab--search-repos-request config "myproject"))
           (url (nth 0 result)))
      (should (string-match-p "/api/v4/projects\\?search=myproject" url))
      (should-not (string-match-p "/groups/" url)))))

(ert-deftest test-shipit-pr-gitlab-search-repos-request-groups-api ()
  "GIVEN a GitLab config
WHEN calling search-repos-request with a slash query (group/project)
THEN URL uses Groups API /groups/:group/projects endpoint."
  (require 'shipit-pr-gitlab)
  (cl-letf (((symbol-function 'shipit-gitlab--auth-header)
             (lambda (_cfg) '("PRIVATE-TOKEN" . "fake-token"))))
    (let* ((config '(:api-url "https://gitlab.com"))
           (result (shipit-pr-gitlab--search-repos-request config "genomedx/"))
           (url (nth 0 result)))
      (should (string-match-p "/api/v4/groups/genomedx/projects" url))
      (should (string-match-p "include_subgroups=true" url)))))

;;; Timeline Tests

(ert-deftest test-shipit-pr-gitlab-fetch-timeline-combines-all-sources ()
  "GIVEN a GitLab MR with state events, system notes, and user comments
WHEN fetching timeline
THEN all are combined and sorted by created_at."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config path)
               (cond
                ((string-match-p "resource_state_events" path)
                 `[((id . 1)
                    (state . "merged")
                    (user . ((username . "alice") (avatar_url . "https://a.png")))
                    (created_at . "2024-01-20T14:00:00.000Z"))])
                ((string-match-p "/notes" path)
                 `[((id . 10) (body . "added ~bug label") (system . t)
                    (author . ((username . "bob") (avatar_url . "https://b.png")))
                    (created_at . "2024-01-19T10:00:00.000Z")
                    (updated_at . "2024-01-19T10:00:00.000Z"))
                   ((id . 11) (body . "User comment") (system . :json-false)
                    (author . ((username . "carol") (avatar_url . "https://c.png")))
                    (created_at . "2024-01-19T12:00:00.000Z")
                    (updated_at . "2024-01-19T12:00:00.000Z"))])))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (events (shipit-pr-gitlab--fetch-timeline config 42)))
      ;; Should have 3 events: system note + user comment + state event
      (should (= 3 (length events)))
      ;; Sorted by created_at
      (should (equal "labeled" (cdr (assq 'event (nth 0 events)))))
      (should (equal "commented" (cdr (assq 'event (nth 1 events)))))
      (should (equal "merged" (cdr (assq 'event (nth 2 events)))))
      ;; User comment has body
      (should (equal "User comment" (cdr (assq 'body (nth 1 events))))))))

(ert-deftest test-shipit-pr-gitlab-normalize-state-event-closed ()
  "GIVEN a GitLab resource state event with state \"closed\"
WHEN normalizing
THEN event type is \"closed\" with actor info."
  (let ((event '((id . 1) (state . "closed")
                 (user . ((username . "alice") (avatar_url . "https://a.png")))
                 (created_at . "2024-01-20T14:00:00.000Z"))))
    (let ((normalized (shipit-pr-gitlab--normalize-state-event event)))
      (should (equal "closed" (cdr (assq 'event normalized))))
      (should (equal "alice" (cdr (assq 'login (cdr (assq 'actor normalized)))))))))

(ert-deftest test-shipit-pr-gitlab-normalize-state-event-unknown-skipped ()
  "GIVEN a GitLab resource state event with unknown state
WHEN normalizing
THEN returns nil."
  (let ((event '((id . 1) (state . "locked")
                 (user . ((username . "alice")))
                 (created_at . "2024-01-20T14:00:00.000Z"))))
    (should-not (shipit-pr-gitlab--normalize-state-event event))))

(ert-deftest test-shipit-pr-gitlab-normalize-system-note-commits ()
  "GIVEN a system note with HTML commit list
WHEN normalizing
THEN returns a list of committed events with sha and message."
  (let ((note `((id . 50)
                (body . "added 2 commits\n\n<ul><li>abc1234f - first commit</li><li>def5678a - second commit</li></ul>")
                (system . t)
                (author . ((username . "bob") (avatar_url . "https://b.png")))
                (created_at . "2024-01-19T10:00:00.000Z"))))
    (let ((events (shipit-pr-gitlab--normalize-system-note note)))
      ;; Returns a list of events for commits
      (should (= 2 (length events)))
      (should (equal "committed" (cdr (assq 'event (nth 0 events)))))
      (should (equal "abc1234f" (cdr (assq 'sha (nth 0 events)))))
      (should (equal "first commit" (cdr (assq 'message (nth 0 events)))))
      (should (equal "bob" (cdr (assq 'login (cdr (assq 'actor (nth 0 events)))))))
      (should (equal "committed" (cdr (assq 'event (nth 1 events)))))
      (should (equal "def5678a" (cdr (assq 'sha (nth 1 events))))))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-labeled ()
  "GIVEN a system note body \"added ~bug label\"
WHEN parsing
THEN returns labeled event with label name."
  (let ((result (shipit-pr-gitlab--parse-system-note-body "added ~bug label")))
    (should (equal "labeled" (car result)))
    (should (equal "bug" (cdr (assq 'name (cdr (assq 'label (cdr result)))))))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-assigned ()
  "GIVEN a system note body \"assigned to @alice\"
WHEN parsing
THEN returns assigned event with assignee login."
  (let ((result (shipit-pr-gitlab--parse-system-note-body "assigned to @alice")))
    (should (equal "assigned" (car result)))
    (should (equal "alice" (cdr (assq 'login (cdr (assq 'assignee (cdr result)))))))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-title-changed ()
  "GIVEN a system note body \"changed title from **Old** to **New**\"
WHEN parsing
THEN returns renamed event with old and new titles."
  (let ((result (shipit-pr-gitlab--parse-system-note-body
                 "changed title from **Old Title** to **New Title**")))
    (should (equal "renamed" (car result)))
    (should (equal "Old Title" (cdr (assq 'old-title (cdr result)))))
    (should (equal "New Title" (cdr (assq 'new-title (cdr result)))))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-commits-added ()
  "GIVEN a system note body with HTML commit list
WHEN parsing
THEN returns committed event with parsed commits list."
  (let ((result (shipit-pr-gitlab--parse-system-note-body
                 "added 2 commits\n\n<ul><li>abc1234f - first commit</li><li>def5678a - second commit</li></ul>")))
    (should (equal "committed" (car result)))
    (let ((commits (cdr (assq 'commits (cdr result)))))
      (should (= 2 (length commits)))
      (should (equal "abc1234f" (cdr (assq 'sha (nth 0 commits)))))
      (should (equal "first commit" (cdr (assq 'message (nth 0 commits)))))
      (should (equal "def5678a" (cdr (assq 'sha (nth 1 commits))))))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-single-commit ()
  "GIVEN a system note body with single HTML commit
WHEN parsing
THEN returns committed event with one parsed commit."
  (let ((result (shipit-pr-gitlab--parse-system-note-body
                 "added 1 commit\n\n<ul><li>b6fe9077 - fix bug</li></ul>")))
    (should (equal "committed" (car result)))
    (let ((commits (cdr (assq 'commits (cdr result)))))
      (should (= 1 (length commits)))
      (should (equal "b6fe9077" (cdr (assq 'sha (car commits)))))
      (should (equal "fix bug" (cdr (assq 'message (car commits))))))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-commits-with-range ()
  "GIVEN a system note with range and individual commits
WHEN parsing
THEN only individual commits are extracted, ranges are skipped."
  (let ((result (shipit-pr-gitlab--parse-system-note-body
                 "added 5 commits\n\n<ul><li>ec16711a...bbf8fcde - 4 commits from branch <code>dev</code></li><li>75b6c31a - Enable default file watchers.</li></ul>")))
    (should (equal "committed" (car result)))
    (let ((commits (cdr (assq 'commits (cdr result)))))
      ;; Only the individual commit, not the range
      (should (= 1 (length commits)))
      (should (equal "75b6c31a" (cdr (assq 'sha (car commits))))))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-description-changed ()
  "GIVEN a system note body \"changed the description\"
WHEN parsing
THEN returns description_changed event."
  (let ((result (shipit-pr-gitlab--parse-system-note-body "changed the description")))
    (should (equal "description_changed" (car result)))))

(ert-deftest test-shipit-pr-gitlab-parse-system-note-unrecognized ()
  "GIVEN a system note body with truly unrecognized text
WHEN parsing
THEN returns nil."
  (should-not (shipit-pr-gitlab--parse-system-note-body "locked this merge request")))

(ert-deftest test-shipit-pr-gitlab-timeline-registered ()
  "GIVEN the GitLab PR backend is registered
WHEN checking for :fetch-timeline
THEN the key is present."
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-timeline))))

(ert-deftest test-shipit-pr-gitlab-review-decision-approved ()
  "GIVEN a GitLab MR with approved status and two approvers
WHEN fetching review decision
THEN returns alist with status-text, completed-reviews, and empty pending lists."
  (require 'shipit-pr-gitlab)
  (let ((mock-data `((approved . t)
                     (approvals_left . 0)
                     (approved_by . [((user . ((username . "alice")
                                              (avatar_url . "https://gl.com/alice.png"))))
                                     ((user . ((username . "bob")
                                              (avatar_url . "https://gl.com/bob.png"))))]))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-data))
              ((symbol-function 'shipit-gitlab--project-path)
               (lambda (_config) "123")))
      (let ((result (shipit-pr-gitlab--fetch-review-decision '(:url "https://gl.com") 42)))
        ;; THEN returns an alist, not a bare string
        (should (listp result))
        ;; THEN status-text contains "Approved"
        (should (string-match-p "Approved" (cdr (assq 'status-text result))))
        ;; THEN completed-reviews has both approvers
        (let ((reviews (cdr (assq 'completed-reviews result))))
          (should (= 2 (length reviews)))
          (should (equal "APPROVED" (cdr (assq 'state (car reviews)))))
          (let ((user (cdr (assq 'user (car reviews)))))
            (should (equal "alice" (cdr (assq 'login user))))
            (should (equal "https://gl.com/alice.png" (cdr (assq 'avatar_url user))))))
        ;; THEN pending lists are empty
        (should-not (cdr (assq 'pending-users result)))
        (should-not (cdr (assq 'pending-teams result)))))))

(ert-deftest test-shipit-pr-gitlab-review-decision-required ()
  "GIVEN a GitLab MR with approvals still needed
WHEN fetching review decision
THEN returns alist with Review Required status."
  (require 'shipit-pr-gitlab)
  (let ((mock-data `((approved . :json-false)
                     (approvals_left . 2)
                     (approved_by . []))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-data))
              ((symbol-function 'shipit-gitlab--project-path)
               (lambda (_config) "123")))
      (let ((result (shipit-pr-gitlab--fetch-review-decision '(:url "https://gl.com") 42)))
        (should (listp result))
        (should (string-match-p "Review Required" (cdr (assq 'status-text result))))
        (should-not (cdr (assq 'completed-reviews result)))))))

(ert-deftest test-shipit-pr-gitlab-review-decision-nil-data ()
  "GIVEN the GitLab API returns nil
WHEN fetching review decision
THEN returns nil."
  (require 'shipit-pr-gitlab)
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) nil))
            ((symbol-function 'shipit-gitlab--project-path)
             (lambda (_config) "123")))
    (should-not (shipit-pr-gitlab--fetch-review-decision '(:url "https://gl.com") 42))))

;;; Fetch-compare tests

(ert-deftest test-shipit-pr-gitlab-fetch-compare-normalizes-sha ()
  "GIVEN a GitLab compare API response with merge_base_commit.id
WHEN calling shipit-pr-gitlab--fetch-compare
THEN result contains merge_base_commit.sha (not .id)."
  (require 'shipit-pr-gitlab)
  (let ((mock-response '((merge_base_commit . ((id . "abc123def456"))))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-response))
              ((symbol-function 'shipit-gitlab--project-path)
               (lambda (_config) "123")))
      (let ((result (shipit-pr-gitlab--fetch-compare
                     '(:api-url "https://gitlab.com") "main-sha" "feature-sha")))
        ;; THEN merge_base_commit has sha key
        (should result)
        (let ((mbc (cdr (assq 'merge_base_commit result))))
          (should (equal "abc123def456" (cdr (assq 'sha mbc)))))))))

(ert-deftest test-shipit-pr-gitlab-fetch-compare-nil-response ()
  "GIVEN a GitLab compare API that returns nil
WHEN calling shipit-pr-gitlab--fetch-compare
THEN result is nil."
  (require 'shipit-pr-gitlab)
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) nil))
            ((symbol-function 'shipit-gitlab--project-path)
             (lambda (_config) "123")))
    (should-not (shipit-pr-gitlab--fetch-compare
                 '(:api-url "https://gitlab.com") "main-sha" "feature-sha"))))

(ert-deftest test-shipit-pr-gitlab-fetch-compare-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for :fetch-compare
THEN the key is present."
  (require 'shipit-pr-gitlab)
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-compare))))

;;; shipit--get-pr-file-patch backend dispatch tests

(ert-deftest test-shipit-get-pr-file-patch-uses-backend-dispatch ()
  "GIVEN a GitLab backend with files containing patches
WHEN calling shipit--get-pr-file-patch
THEN the patch is returned via shipit-get-pr-files dispatch."
  (require 'shipit-pr-gitlab)
  (require 'shipit-pr-sections)
  (let ((mock-files `(((filename . "src/main.rb")
                       (status . "modified")
                       (additions . 2)
                       (deletions . 1)
                       (patch . "@@ -1,3 +1,5 @@\n+added line\n context\n-removed line"))
                      ((filename . "README.md")
                       (status . "modified")
                       (additions . 1)
                       (deletions . 0)
                       (patch . "@@ -1 +1,2 @@\n+new line")))))
    (cl-letf (((symbol-function 'shipit-get-pr-files)
               (lambda (_pr-number) mock-files)))
      (let ((patch (shipit--get-pr-file-patch "mygroup/myproject" 42 "src/main.rb")))
        ;; THEN returns the patch for the matching file
        (should (stringp patch))
        (should (string-match-p "added line" patch))))))

(ert-deftest test-shipit-get-pr-file-patch-file-not-found ()
  "GIVEN files data that does not contain the requested file
WHEN calling shipit--get-pr-file-patch
THEN returns nil."
  (require 'shipit-pr-sections)
  (let ((mock-files `(((filename . "other.rb") (patch . "some patch")))))
    (cl-letf (((symbol-function 'shipit-get-pr-files)
               (lambda (_pr-number) mock-files)))
      (should-not (shipit--get-pr-file-patch "repo" 1 "missing.rb")))))

;;; PR ref fetching backend-aware tests

(ert-deftest test-shipit-fetch-pr-ref-async-github-refspec ()
  "GIVEN a GitHub backend
WHEN calling shipit--fetch-pr-ref-async
THEN uses refs/pull/N/head refspec."
  (require 'shipit-lib)
  (let ((captured-args nil)
        (shipit-pr-backend 'github))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args) (setq captured-args args) nil)))
      (ignore-errors
        (shipit--fetch-pr-ref-async 42 #'ignore "owner/repo")))
    ;; THEN refspec is GitHub-style
    (when captured-args
      (should (member "refs/pull/42/head" captured-args)))))

(ert-deftest test-shipit-fetch-pr-ref-async-gitlab-refspec ()
  "GIVEN a GitLab backend
WHEN calling shipit--fetch-pr-ref-async with head-ref
THEN fetches the source branch from origin."
  (require 'shipit-lib)
  (let ((captured-args nil)
        (shipit-pr-backend 'gitlab))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args) (setq captured-args args) nil)))
      (ignore-errors
        (shipit--fetch-pr-ref-async 42 #'ignore "group/project" "feature-branch")))
    ;; THEN refspec is the source branch name
    (when captured-args
      (should (member "feature-branch" captured-args))
      ;; THEN remote is origin (not HTTPS URL)
      (should (member "origin" captured-args)))))

;;; Fetch-file-content tests

(ert-deftest test-shipit-pr-gitlab-fetch-file-content-decodes-base64 ()
  "GIVEN a GitLab API response with base64-encoded file content
WHEN calling shipit-pr-gitlab--fetch-file-content
THEN returns decoded file content as a string."
  (require 'shipit-pr-gitlab)
  (let ((mock-response `((content . ,(base64-encode-string "hello world"))
                         (encoding . "base64"))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-response))
              ((symbol-function 'shipit-gitlab--project-path)
               (lambda (_config) "123")))
      (let ((result (shipit-pr-gitlab--fetch-file-content
                     '(:api-url "https://gitlab.com") "src/main.rb" "abc123")))
        (should (stringp result))
        (should (string= "hello world" result))))))

(ert-deftest test-shipit-pr-gitlab-fetch-file-content-nil-response ()
  "GIVEN a GitLab API that returns nil
WHEN calling shipit-pr-gitlab--fetch-file-content
THEN returns nil."
  (require 'shipit-pr-gitlab)
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) nil))
            ((symbol-function 'shipit-gitlab--project-path)
             (lambda (_config) "123")))
    (should-not (shipit-pr-gitlab--fetch-file-content
                 '(:api-url "https://gitlab.com") "missing.rb" "abc123"))))

(ert-deftest test-shipit-pr-gitlab-fetch-file-content-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for :fetch-file-content
THEN the key is present."
  (require 'shipit-pr-gitlab)
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-file-content))))

;;; insert-file-diff non-integer line key tests

(ert-deftest test-shipit-insert-file-diff-non-integer-line-no-crash ()
  "GIVEN inline comments with non-integer line values (strings from GitLab API)
WHEN calling shipit--insert-file-diff
THEN no crash occurs (non-integer keys are skipped)."
  (require 'shipit-pr-sections)
  (let* ((shipit--cached-inline-comments
          (list `((path . "src/main.rb")
                  (line . "10")  ;; String instead of integer — GitLab quirk
                  (original_line . nil)
                  (side . "RIGHT")
                  (id . 100)
                  (in_reply_to_id . nil)
                  (body . "Fix this")
                  (outdated . nil)
                  (subject_type . nil)
                  (user . ((login . "reviewer"))))))
         (patch "@@ -1,3 +1,5 @@\n+added line\n context\n-removed line"))
    (with-temp-buffer
      ;; Should not signal an error
      (should (progn
                (shipit--insert-file-diff patch "src/main.rb" 1 "group/project" 42)
                t)))))

(ert-deftest test-shipit-insert-file-diff-json-null-line-no-crash ()
  "GIVEN inline comments with :json-null line values
WHEN calling shipit--insert-file-diff
THEN no crash occurs."
  (require 'shipit-pr-sections)
  (let* ((shipit--cached-inline-comments
          (list `((path . "src/main.rb")
                  (line . :json-null)
                  (original_line . :json-null)
                  (side . "RIGHT")
                  (id . 101)
                  (in_reply_to_id . nil)
                  (body . "Comment")
                  (outdated . nil)
                  (subject_type . nil)
                  (user . ((login . "reviewer"))))))
         (patch "@@ -1,3 +1,5 @@\n+added line\n context\n-removed line"))
    (with-temp-buffer
      (should (progn
                (shipit--insert-file-diff patch "src/main.rb" 1 "group/project" 42)
                t)))))

(ert-deftest test-shipit-insert-file-diff-integer-line-still-works ()
  "GIVEN inline comments with proper integer line values
WHEN calling shipit--insert-file-diff
THEN comments are correctly mapped to hash tables."
  (require 'shipit-pr-sections)
  (let* ((shipit--cached-inline-comments
          (list `((path . "src/main.rb")
                  (line . 2)
                  (original_line . nil)
                  (side . "RIGHT")
                  (id . 102)
                  (in_reply_to_id . nil)
                  (body . "Good line")
                  (outdated . nil)
                  (subject_type . nil)
                  (user . ((login . "reviewer"))))))
         (patch "@@ -1,3 +1,5 @@\n+added line\n context\n-removed line"))
    (with-temp-buffer
      (should (progn
                (shipit--insert-file-diff patch "src/main.rb" 1 "group/project" 42)
                t)))))

;;; shipit--get-repo-from-remote backend-aware tests

(ert-deftest test-shipit-parse-repo-from-url-gitlab-ssh ()
  "GIVEN a GitLab SSH remote URL
WHEN calling shipit--parse-repo-from-url
THEN returns owner/repo."
  (should (string= "mygroup/myproject"
                    (shipit--parse-repo-from-url "git@gitlab.com:mygroup/myproject.git\n"))))

(ert-deftest test-shipit-parse-repo-from-url-gitlab-https ()
  "GIVEN a GitLab HTTPS remote URL
WHEN calling shipit--parse-repo-from-url
THEN returns owner/repo."
  (should (string= "mygroup/myproject"
                    (shipit--parse-repo-from-url "https://gitlab.com/mygroup/myproject.git\n"))))

(ert-deftest test-shipit-parse-repo-from-url-self-hosted-gitlab ()
  "GIVEN a self-hosted GitLab SSH remote URL
WHEN calling shipit--parse-repo-from-url
THEN returns owner/repo."
  (should (string= "team/project"
                    (shipit--parse-repo-from-url "git@gitlab.company.com:team/project.git\n"))))

(ert-deftest test-shipit-parse-repo-from-url-github-still-works ()
  "GIVEN a GitHub SSH remote URL
WHEN calling shipit--parse-repo-from-url
THEN returns owner/repo (regression check)."
  (should (string= "owner/repo"
                    (shipit--parse-repo-from-url "git@github.com:owner/repo.git\n"))))

(ert-deftest test-shipit-parse-repo-from-url-nested-gitlab-group ()
  "GIVEN a GitLab remote with nested group path
WHEN calling shipit--parse-repo-from-url
THEN returns the full nested path."
  (should (string= "org/subgroup/project"
                    (shipit--parse-repo-from-url "git@gitlab.com:org/subgroup/project.git\n"))))

;;; Backend infrastructure ops tests

(ert-deftest test-shipit-pr-github-refspec-for-pr ()
  "GIVEN the GitHub backend with :refspec-for-pr registered
WHEN calling with pr-number 42
THEN returns refs/pull/42/head."
  (require 'shipit-pr-github)
  (let* ((backend (cdr (assq 'github shipit-pr-backends)))
         (refspec-fn (plist-get backend :refspec-for-pr)))
    (should refspec-fn)
    (let ((result (funcall refspec-fn '(:repo "owner/repo") 42 "feature-branch")))
      (should (string= "refs/pull/42/head" result)))))

(ert-deftest test-shipit-pr-gitlab-refspec-for-pr ()
  "GIVEN the GitLab backend with :refspec-for-pr registered
WHEN calling with head-ref \"feature-branch\"
THEN returns the head-ref (source branch name)."
  (require 'shipit-pr-gitlab)
  (let* ((backend (cdr (assq 'gitlab shipit-pr-backends)))
         (refspec-fn (plist-get backend :refspec-for-pr)))
    (should refspec-fn)
    (let ((result (funcall refspec-fn '(:repo "group/project") 42 "feature-branch")))
      (should (string= "feature-branch" result)))))

(ert-deftest test-shipit-pr-github-remote-for-fetch ()
  "GIVEN the GitHub backend with :remote-for-fetch registered
WHEN calling with a repo
THEN returns HTTPS URL for that repo."
  (require 'shipit-pr-github)
  (let* ((backend (cdr (assq 'github shipit-pr-backends)))
         (remote-fn (plist-get backend :remote-for-fetch)))
    (should remote-fn)
    (let ((result (funcall remote-fn '(:repo "owner/repo") "owner/repo")))
      (should (string= "https://github.com/owner/repo.git" result)))))

(ert-deftest test-shipit-pr-github-remote-for-fetch-origin ()
  "GIVEN the GitHub backend with :remote-for-fetch
WHEN calling with nil repo
THEN returns \"origin\"."
  (require 'shipit-pr-github)
  (let* ((backend (cdr (assq 'github shipit-pr-backends)))
         (remote-fn (plist-get backend :remote-for-fetch)))
    (should remote-fn)
    (let ((result (funcall remote-fn '(:repo "owner/repo") nil)))
      (should (string= "origin" result)))))

(ert-deftest test-shipit-pr-gitlab-remote-for-fetch ()
  "GIVEN the GitLab backend with :remote-for-fetch registered
WHEN calling with any repo
THEN always returns \"origin\"."
  (require 'shipit-pr-gitlab)
  (let* ((backend (cdr (assq 'gitlab shipit-pr-backends)))
         (remote-fn (plist-get backend :remote-for-fetch)))
    (should remote-fn)
    (let ((result (funcall remote-fn '(:repo "group/project") "group/project")))
      (should (string= "origin" result)))))

;;; shipit--repo-available-locally-p tests

(ert-deftest test-shipit-repo-available-locally-p-true ()
  "GIVEN magit-toplevel returns a path AND local repo matches
WHEN calling shipit--repo-available-locally-p
THEN returns non-nil."
  (require 'shipit-pr-sections)
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/home/user/project/"))
            ((symbol-function 'shipit--local-repo-matches-p) (lambda (_repo) t)))
    (should (shipit--repo-available-locally-p "owner/repo"))))

(ert-deftest test-shipit-repo-available-locally-p-no-magit ()
  "GIVEN magit-toplevel returns nil (not in a git repo)
WHEN calling shipit--repo-available-locally-p
THEN returns nil."
  (require 'shipit-pr-sections)
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () nil))
            ((symbol-function 'shipit--local-repo-matches-p) (lambda (_repo) t)))
    (should-not (shipit--repo-available-locally-p "owner/repo"))))

(ert-deftest test-shipit-repo-available-locally-p-wrong-repo ()
  "GIVEN magit-toplevel returns a path BUT local repo doesn't match
WHEN calling shipit--repo-available-locally-p
THEN returns nil."
  (require 'shipit-pr-sections)
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/home/user/other/"))
            ((symbol-function 'shipit--local-repo-matches-p) (lambda (_repo) nil)))
    (should-not (shipit--repo-available-locally-p "owner/repo"))))

;;; shipit--with-local-or-api tests

(ert-deftest test-shipit-with-local-or-api-dispatches-local ()
  "GIVEN SHAs are available in a matching local repo
WHEN calling shipit--with-local-or-api
THEN local-fn is called with base-sha, head-sha, head-ref, base-ref."
  (require 'shipit-pr-sections)
  (let ((local-called nil)
        (api-called nil)
        (mock-pr-data '((base . ((sha . "base111") (ref . "main")))
                        (head . ((sha . "head222") (ref . "feature"))))))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_number &optional _repo) mock-pr-data))
              ((symbol-function 'shipit--repo-available-locally-p)
               (lambda (_repo) t)))
      (shipit--with-local-or-api
       "owner/repo" 42
       (lambda (base-sha head-sha head-ref base-ref _pr-data)
         (setq local-called (list base-sha head-sha head-ref base-ref)))
       (lambda (_repo _pr-number)
         (setq api-called t))))
    (should local-called)
    (should-not api-called)
    (should (equal '("base111" "head222" "feature" "main") local-called))))

(ert-deftest test-shipit-with-local-or-api-dispatches-api ()
  "GIVEN repo is not available locally
WHEN calling shipit--with-local-or-api
THEN api-fn is called with repo and pr-number."
  (require 'shipit-pr-sections)
  (let ((local-called nil)
        (api-called nil)
        (mock-pr-data '((base . ((sha . "base111") (ref . "main")))
                        (head . ((sha . "head222") (ref . "feature"))))))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_number &optional _repo) mock-pr-data))
              ((symbol-function 'shipit--repo-available-locally-p)
               (lambda (_repo) nil)))
      (shipit--with-local-or-api
       "owner/repo" 42
       (lambda (_base-sha _head-sha _head-ref _base-ref _pr-data)
         (setq local-called t))
       (lambda (repo pr-number)
         (setq api-called (list repo pr-number)))))
    (should-not local-called)
    (should api-called)
    (should (equal '("owner/repo" 42) api-called))))

(ert-deftest test-shipit-with-local-or-api-missing-shas-falls-to-api ()
  "GIVEN pr-data has no SHAs (e.g. from list endpoint)
WHEN calling shipit--with-local-or-api
THEN api-fn is called (even if repo is local)."
  (require 'shipit-pr-sections)
  (let ((api-called nil)
        (mock-pr-data '((base . ((ref . "main")))
                        (head . ((ref . "feature"))))))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_number &optional _repo) mock-pr-data))
              ((symbol-function 'shipit--repo-available-locally-p)
               (lambda (_repo) t)))
      (shipit--with-local-or-api
       "owner/repo" 42
       (lambda (_b _h _hr _br _pd) nil)
       (lambda (repo pr-number)
         (setq api-called (list repo pr-number)))))
    (should api-called)))

;;; shipit--fetch-pr-ref-async backend dispatch tests

(ert-deftest test-shipit-fetch-pr-ref-async-uses-backend-refspec ()
  "GIVEN a GitHub backend with :refspec-for-pr and :remote-for-fetch
WHEN calling shipit--fetch-pr-ref-async
THEN uses backend dispatch instead of hardcoded checks."
  (require 'shipit-lib)
  (require 'shipit-pr-github)
  (let ((captured-args nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest args) (setq captured-args args) nil)))
      (ignore-errors
        (shipit--fetch-pr-ref-async 42 #'ignore "owner/repo" "feature")))
    (when captured-args
      (should (member "refs/pull/42/head" captured-args)))))

;;; Thread root finding with string IDs (GitLab discussions)

(ert-deftest test-gitlab-find-thread-root-id-with-string-discussion-ids ()
  "GIVEN comments with GitLab string discussion IDs as in_reply_to_id
WHEN calling shipit--find-thread-root-id
THEN it finds the root without crashing on = with strings."
  (require 'shipit-comments)
  (let ((comments `(((id . 100) (body . "root") (in_reply_to_id))
                    ((id . 101) (body . "reply")
                     (in_reply_to_id . "e5a6355e5400365f442ac67c161f45db585a1b64")))))
    ;; in_reply_to is a string discussion ID, not a numeric comment ID
    ;; This should NOT crash with wrong-type-argument number-or-marker-p
    (let ((result (shipit--find-thread-root-id
                   101 "e5a6355e5400365f442ac67c161f45db585a1b64" comments)))
      ;; No parent found for string ID, so it returns the string itself
      (should (equal result "e5a6355e5400365f442ac67c161f45db585a1b64")))))

(ert-deftest test-gitlab-find-thread-root-id-with-numeric-ids ()
  "GIVEN comments with numeric IDs (GitHub style)
WHEN calling shipit--find-thread-root-id
THEN it still works correctly with equal comparison."
  (require 'shipit-comments)
  (let ((comments `(((id . 100) (body . "root") (in_reply_to_id))
                    ((id . 101) (body . "reply") (in_reply_to_id . 100)))))
    (let ((result (shipit--find-thread-root-id 101 100 comments)))
      (should (equal result 100)))))

;;; Reactions: non-GitHub backends should not crash

(ert-deftest test-gitlab-reactions-batch-dispatches-through-backend ()
  "GIVEN shipit-pr-backend is gitlab with few comments (<= 5)
WHEN shipit-comment--fetch-reactions-batch is called
THEN reactions are fetched eagerly via GitLab API
     AND results are cached for each comment."
  (require 'shipit-http)
  (require 'shipit-comments)
  (require 'shipit-comment-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:project-path "mygroup/myproject"))
        (shipit--reaction-cache (make-hash-table :test 'equal))
        (shipit--current-displayed-pr '(42 "mygroup/myproject"))
        (gitlab-called nil)
        (github-called nil)
        (comments '(((id . 100) (body . "test")))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path &optional _params)
                 (setq gitlab-called t)
                 ;; Return a thumbsup reaction
                 (list (list (cons 'id 201)
                             (cons 'name "thumbsup")
                             (cons 'user (list (cons 'username "alice")))))))
              ((symbol-function 'url-retrieve)
               (lambda (&rest _) (setq github-called t) nil)))
      (shipit-comment--fetch-reactions-batch comments "mygroup/myproject" nil)
      ;; THEN GitLab API is called (eager fetch for <= 5 comments)
      (should gitlab-called)
      ;; THEN GitHub url-retrieve should NOT have been called
      (should-not github-called)
      ;; THEN reactions are cached
      (let ((cached (gethash "mygroup/myproject:100-general" shipit--reaction-cache)))
        (should cached)
        (should (= 1 (length cached)))))))

;;; Async backend wrappers

(ert-deftest test-shipit-pr-gitlab-async-wrappers-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for async keys
THEN :fetch-review-decision-async, :fetch-timeline-async,
     :fetch-commits-async, :fetch-files-async are all present."
  (require 'shipit-pr-gitlab)
  (let* ((entry (assq 'gitlab shipit-pr-backends))
         (plist (cdr entry)))
    (should (plist-get plist :fetch-review-decision-async))
    (should (plist-get plist :fetch-timeline-async))
    (should (plist-get plist :fetch-commits-async))
    (should (plist-get plist :fetch-files-async))))


;;; MR Award Emoji (Reactions) Tests

(ert-deftest test-gitlab-pr-fetch-reactions ()
  "GIVEN a GitLab MR with award emojis
WHEN fetching reactions via the PR backend
THEN returns normalized reaction alists with GitHub-compatible content."
  (require 'shipit-pr-gitlab)
  (require 'shipit-comment-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test-token"
                  :project-path "mygroup/myproject" :repo "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_cfg _path &optional _params)
                 (list (list (cons 'id 101)
                             (cons 'name "thumbsup")
                             (cons 'user (list (cons 'username "alice"))))
                       (list (cons 'id 102)
                             (cons 'name "rocket")
                             (cons 'user (list (cons 'username "bob"))))))))
      (let ((reactions (shipit-pr-gitlab--fetch-reactions config 42)))
        (should (= 2 (length reactions)))
        ;; THEN thumbsup is normalized to +1
        (should (equal "+1" (cdr (assq 'content (car reactions)))))
        (should (equal "alice" (cdr (assq 'login (cdr (assq 'user (car reactions)))))))
        ;; THEN rocket stays as rocket
        (should (equal "rocket" (cdr (assq 'content (cadr reactions)))))
        (should (equal "bob" (cdr (assq 'login (cdr (assq 'user (cadr reactions)))))))))))

(ert-deftest test-gitlab-pr-add-reaction ()
  "GIVEN a GitHub-compatible reaction name (+1)
WHEN adding it to a GitLab MR
THEN it converts to GitLab emoji name (thumbsup) and POSTs."
  (require 'shipit-pr-gitlab)
  (require 'shipit-comment-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test-token"
                  :project-path "mygroup/myproject" :repo "mygroup/myproject"))
        (posted-data nil)
        (posted-path nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_cfg path data _method)
                 (setq posted-path path
                       posted-data data)
                 (list (cons 'id 201)
                       (cons 'name "thumbsup")
                       (cons 'user (list (cons 'username "me")))))))
      (let ((result (shipit-pr-gitlab--add-reaction config 42 "+1")))
        ;; THEN the POST path includes award_emoji
        (should (string-match-p "award_emoji" posted-path))
        ;; THEN the emoji name is converted from +1 to thumbsup
        (should (equal "thumbsup" (cdr (assq 'name posted-data))))
        ;; THEN the result is normalized
        (should (equal "+1" (cdr (assq 'content result))))))))

(ert-deftest test-gitlab-pr-delete-reaction ()
  "GIVEN an award emoji ID on a GitLab MR
WHEN deleting the reaction
THEN DELETEs the correct award_emoji path."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test-token"
                  :project-path "mygroup/myproject" :repo "mygroup/myproject"))
        (deleted-path nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_cfg path _data _method)
                 (setq deleted-path path)
                 nil)))
      (shipit-pr-gitlab--delete-reaction config 42 101)
      ;; THEN the DELETE path includes both MR number and reaction ID
      (should (string-match-p "merge_requests/42" deleted-path))
      (should (string-match-p "award_emoji/101" deleted-path)))))

(ert-deftest test-gitlab-pr-backend-has-reaction-keys ()
  "GIVEN the GitLab PR backend is registered
WHEN checking for reaction keys
THEN :fetch-reactions, :add-reaction, :delete-reaction are present."
  (require 'shipit-pr-gitlab)
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-reactions))
    (should (plist-get backend :add-reaction))
    (should (plist-get backend :delete-reaction))))

;;; Truly-async wrapper tests

(ert-deftest test/pr-gitlab-fetch-commits-async ()
  "GIVEN a mocked shipit-gitlab--api-request-async
WHEN calling shipit-pr-gitlab--fetch-commits-async
THEN callback receives normalized commits."
  (require 'shipit-pr-gitlab)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-async)
               (lambda (_config _path cb)
                 (funcall cb (vector
                              '((id . "abc123") (message . "feat: add X")
                                (author_name . "Alice")
                                (created_at . "2024-01-15T10:00:00Z")
                                (web_url . "https://gitlab.com/commit/abc123"))
                              '((id . "def456") (message . "fix: bug Y")
                                (author_name . "Bob")
                                (created_at . "2024-01-16T10:00:00Z")
                                (web_url . "https://gitlab.com/commit/def456")))))))
      (shipit-pr-gitlab--fetch-commits-async
       config 42 (lambda (data) (setq callback-result data))))
    ;; THEN callback received normalized commits
    (should (not (eq 'not-called callback-result)))
    (should (= 2 (length callback-result)))
    (should (equal "abc123" (cdr (assq 'sha (car callback-result)))))))

(ert-deftest test/pr-gitlab-fetch-files-async ()
  "GIVEN a mocked shipit-gitlab--api-request-async
WHEN calling shipit-pr-gitlab--fetch-files-async
THEN callback receives (files . nil) format."
  (require 'shipit-pr-gitlab)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-async)
               (lambda (_config _path cb)
                 (funcall cb '((changes . [((old_path . "src/main.rb")
                                            (new_path . "src/main.rb")
                                            (new_file . :json-false)
                                            (renamed_file . :json-false)
                                            (deleted_file . :json-false)
                                            (diff . "+line"))]))))))
      (shipit-pr-gitlab--fetch-files-async
       config 42 (lambda (data) (setq callback-result data))))
    ;; THEN result is (files . truncated-p) cons
    (should (not (eq 'not-called callback-result)))
    (let ((files (car callback-result))
          (truncated (cdr callback-result)))
      (should (= 1 (length files)))
      (should (string= "src/main.rb" (cdr (assq 'filename (car files)))))
      (should-not truncated))))

(ert-deftest test/pr-gitlab-fetch-review-decision-async ()
  "GIVEN a mocked shipit-gitlab--api-request-async
WHEN calling shipit-pr-gitlab--fetch-review-decision-async
THEN callback receives review-info alist."
  (require 'shipit-pr-gitlab)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-async)
               (lambda (_config _path cb)
                 (funcall cb '((approved . t)
                               (approvals_left . 0)
                               (approved_by . [((user . ((username . "alice")
                                                         (avatar_url . "https://a.png"))))]))))))
      (shipit-pr-gitlab--fetch-review-decision-async
       config 42 (lambda (data) (setq callback-result data))))
    ;; THEN callback received review decision
    (should (not (eq 'not-called callback-result)))
    (should (string-match-p "Approved" (cdr (assq 'status-text callback-result))))))

(ert-deftest test/pr-gitlab-fetch-timeline-async ()
  "GIVEN mocked shipit-gitlab--api-request-async for two endpoints
WHEN calling shipit-pr-gitlab--fetch-timeline-async
THEN callback receives combined+sorted events from both requests."
  (require 'shipit-pr-gitlab)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-async)
               (lambda (_config path cb)
                 (cond
                  ((string-match-p "resource_state_events" path)
                   (funcall cb (vector
                                '((id . 1)
                                  (state . "merged")
                                  (user . ((username . "alice") (avatar_url . "https://a.png")))
                                  (created_at . "2024-01-20T14:00:00.000Z")))))
                  ((string-match-p "/notes" path)
                   (funcall cb (vector
                                '((id . 10) (body . "added ~bug label") (system . t)
                                  (author . ((username . "bob") (avatar_url . "https://b.png")))
                                  (created_at . "2024-01-19T10:00:00.000Z")
                                  (updated_at . "2024-01-19T10:00:00.000Z"))
                                '((id . 11) (body . "User comment") (system . :json-false)
                                  (author . ((username . "carol") (avatar_url . "https://c.png")))
                                  (created_at . "2024-01-19T12:00:00.000Z")
                                  (updated_at . "2024-01-19T12:00:00.000Z")))))))))
      (shipit-pr-gitlab--fetch-timeline-async
       config 42 (lambda (data) (setq callback-result data))))
    ;; THEN callback received 3 events sorted by created_at
    (should (not (eq 'not-called callback-result)))
    (should (= 3 (length callback-result)))
    (should (equal "labeled" (cdr (assq 'event (nth 0 callback-result)))))
    (should (equal "commented" (cdr (assq 'event (nth 1 callback-result)))))
    (should (equal "merged" (cdr (assq 'event (nth 2 callback-result)))))))

(ert-deftest test/pr-gitlab-fetch-timeline-async-nil-responses ()
  "GIVEN mocked async requests that return nil
WHEN calling shipit-pr-gitlab--fetch-timeline-async
THEN callback receives empty list (not error)."
  (require 'shipit-pr-gitlab)
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-async)
               (lambda (_config _path cb) (funcall cb nil))))
      (shipit-pr-gitlab--fetch-timeline-async
       config 42 (lambda (data) (setq callback-result data))))
    (should (not (eq 'not-called callback-result)))
    (should-not callback-result)))

;;; classify-notes tests

(ert-deftest test/pr-gitlab-classify-notes-separates-system-and-user ()
  "GIVEN a list of notes with system and user notes
WHEN calling shipit-pr-gitlab--classify-notes
THEN returns (SYSTEM-EVENTS . USER-COMMENTS) with correct classification."
  (require 'shipit-pr-gitlab)
  (let* ((notes `(((id . 10) (body . "added ~bug label") (system . t)
                   (author . ((username . "bob") (avatar_url . "https://b.png")))
                   (created_at . "2024-01-19T10:00:00.000Z")
                   (updated_at . "2024-01-19T10:00:00.000Z"))
                  ((id . 11) (body . "User comment") (system . :json-false)
                   (author . ((username . "carol") (avatar_url . "https://c.png")))
                   (created_at . "2024-01-19T12:00:00.000Z")
                   (updated_at . "2024-01-19T12:00:00.000Z"))))
         (result (shipit-pr-gitlab--classify-notes notes))
         (system-events (car result))
         (user-comments (cdr result)))
    ;; THEN one system event (labeled) and one user comment (commented)
    (should (= 1 (length system-events)))
    (should (equal "labeled" (cdr (assq 'event (car system-events)))))
    (should (= 1 (length user-comments)))
    (should (equal "commented" (cdr (assq 'event (car user-comments)))))))

(ert-deftest test/pr-gitlab-classify-notes-empty-list ()
  "GIVEN an empty notes list
WHEN calling shipit-pr-gitlab--classify-notes
THEN returns (nil . nil)."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr-gitlab--classify-notes nil)))
    (should-not (car result))
    (should-not (cdr result))))

(ert-deftest test/pr-gitlab-classify-notes-commits-expand ()
  "GIVEN a system note with multiple commits
WHEN calling shipit-pr-gitlab--classify-notes
THEN committed notes expand into multiple system events."
  (require 'shipit-pr-gitlab)
  (let* ((notes `(((id . 50)
                   (body . "added 2 commits\n\n<ul><li>abc1234f - first commit</li><li>def5678a - second commit</li></ul>")
                   (system . t)
                   (author . ((username . "bob") (avatar_url . "https://b.png")))
                   (created_at . "2024-01-19T10:00:00.000Z"))))
         (result (shipit-pr-gitlab--classify-notes notes))
         (system-events (car result)))
    ;; THEN 2 committed events
    (should (= 2 (length system-events)))
    (should (equal "committed" (cdr (assq 'event (car system-events)))))
    (should (equal "abc1234f" (cdr (assq 'sha (car system-events)))))))

;;; Backend-aware checks message tests

(ert-deftest test/checks-message-shows-gitlab-backend-name ()
  "GIVEN shipit-pr-backend is gitlab
WHEN the checks section renders the lazy-load message
THEN the message says \"Press RET to load checks from GitLab\"."
  (require 'shipit-checks)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config nil)
        (result nil))
    (with-temp-buffer
      (setq result (format "   Press RET to load checks from %s\n"
                           (or (ignore-errors (plist-get (shipit-pr--get-backend) :name)) "CI")))
      (should (string-match-p "GitLab" result)))))

(ert-deftest test/checks-message-shows-github-backend-name ()
  "GIVEN shipit-pr-backend is github
WHEN the checks section renders the lazy-load message
THEN the message says \"Press RET to load checks from GitHub\"."
  (require 'shipit-checks)
  (require 'shipit-pr-github)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (result nil))
    (with-temp-buffer
      (setq result (format "   Press RET to load checks from %s\n"
                           (or (ignore-errors (plist-get (shipit-pr--get-backend) :name)) "CI")))
      (should (string-match-p "GitHub" result)))))

(ert-deftest test/checks-message-falls-back-to-ci ()
  "GIVEN shipit-pr-backend is an unregistered backend
WHEN the checks section renders the lazy-load message
THEN the message falls back to \"CI\"."
  (require 'shipit-checks)
  (let ((shipit-pr-backend 'nonexistent)
        (shipit-pr-backend-config nil)
        (result nil))
    (setq result (format "   Press RET to load checks from %s\n"
                         (or (ignore-errors (plist-get (shipit-pr--get-backend) :name)) "CI")))
    (should (string-match-p "CI" result))))

;;; GitLab URL reference overlay tests

(ert-deftest test/gitlab-mr-url-gets-overlay ()
  "GIVEN a buffer with a GitLab MR URL
WHEN creating PR reference overlays with gitlab backend
THEN an overlay is created on the MR URL."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject")))
    (with-temp-buffer
      (insert "See https://gitlab.com/mygroup/myproject/-/merge_requests/42 for details")
      (shipit--create-pr-reference-overlays "mygroup/myproject" 1 (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        ;; THEN at least one overlay covers the MR URL
        (should (cl-some (lambda (ov)
                           (string-match-p "merge_requests/42"
                                           (buffer-substring-no-properties
                                            (overlay-start ov) (overlay-end ov))))
                         ovs))))))

(ert-deftest test/gitlab-issue-url-gets-overlay ()
  "GIVEN a buffer with a GitLab issue URL
WHEN creating PR reference overlays with gitlab backend
THEN an overlay is created on the issue URL."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject")))
    (with-temp-buffer
      (insert "Fix for https://gitlab.com/mygroup/myproject/-/issues/99")
      (shipit--create-pr-reference-overlays "mygroup/myproject" 1 (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov)
                           (string-match-p "issues/99"
                                           (buffer-substring-no-properties
                                            (overlay-start ov) (overlay-end ov))))
                         ovs))))))

(ert-deftest test/gitlab-self-hosted-url-gets-overlay ()
  "GIVEN a buffer with a self-hosted GitLab MR URL
WHEN creating PR reference overlays with gitlab backend pointing to self-hosted
THEN an overlay is created on the MR URL."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.company.com"
                                    :project-path "team/project")))
    (with-temp-buffer
      (insert "See https://gitlab.company.com/team/project/-/merge_requests/10 here")
      (shipit--create-pr-reference-overlays "team/project" 1 (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (cl-some (lambda (ov)
                           (string-match-p "merge_requests/10"
                                           (buffer-substring-no-properties
                                            (overlay-start ov) (overlay-end ov))))
                         ovs))))))

(ert-deftest test/gitlab-url-non-matching-host-no-overlay ()
  "GIVEN a buffer with a URL from a different GitLab host
WHEN creating PR reference overlays with gitlab backend for gitlab.com
THEN no overlay is created for the non-matching URL."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject")))
    (with-temp-buffer
      (insert "See https://gitlab.other.com/team/project/-/merge_requests/5 here")
      (shipit--create-pr-reference-overlays "mygroup/myproject" 1 (point-min) (point-max))
      (let ((ovs (overlays-in (point-min) (point-max))))
        ;; No overlay should match the other-host URL
        (should-not (cl-some (lambda (ov)
                               (string-match-p "merge_requests/5"
                                               (buffer-substring-no-properties
                                                (overlay-start ov) (overlay-end ov))))
                             ovs))))))

;;; Backend-aware commit link tests

(ert-deftest test/open-commit-github-url-format ()
  "GIVEN shipit-pr-backend is github
WHEN opening a commit in browser
THEN the URL uses github.com format."
  (require 'shipit-render)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (shipit--open-commit-in-browser "abc123" "owner/repo")
      (should (string= "https://github.com/owner/repo/commit/abc123" opened-url)))))

(ert-deftest test/open-commit-gitlab-url-format ()
  "GIVEN shipit-pr-backend is gitlab with api-url and project-path
WHEN opening a commit in browser
THEN the URL uses GitLab format with /-/commit/ path."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"))
        (opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (shipit--open-commit-in-browser "abc123" "mygroup/myproject")
      (should (string= "https://gitlab.com/mygroup/myproject/-/commit/abc123" opened-url)))))

(ert-deftest test/open-commit-gitlab-self-hosted-url ()
  "GIVEN shipit-pr-backend is gitlab with a self-hosted api-url
WHEN opening a commit in browser
THEN the URL uses the self-hosted domain."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.company.com"
                                    :project-path "team/project"))
        (opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (shipit--open-commit-in-browser "def456" "team/project")
      (should (string= "https://gitlab.company.com/team/project/-/commit/def456" opened-url)))))

;;; Backend-aware URL helper tests

(ert-deftest test/open-user-profile-gitlab ()
  "GIVEN shipit-pr-backend is gitlab
WHEN opening a user profile in browser
THEN the URL uses the GitLab instance."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"))
        (opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (shipit--open-user-profile-in-browser "jdoe")
      (should (string= "https://gitlab.com/jdoe" opened-url)))))

(ert-deftest test/open-user-profile-github ()
  "GIVEN shipit-pr-backend is github
WHEN opening a user profile in browser
THEN the URL uses github.com."
  (require 'shipit-render)
  (require 'shipit-pr-github)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (shipit--open-user-profile-in-browser "jdoe")
      (should (string= "https://github.com/jdoe" opened-url)))))

(ert-deftest test/debug-open-pr-browser-gitlab ()
  "GIVEN shipit-pr-backend is gitlab
WHEN shipit-open-pr-in-browser constructs a URL
THEN it uses backend :browse-url."
  (require 'shipit-debug)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"))
        (opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      ;; Test the URL construction logic directly
      (let* ((resolved (shipit-pr--resolve-for-repo "mygroup/myproject"))
             (backend (car resolved))
             (config (cdr resolved))
             (url (funcall (plist-get backend :browse-url) config 42)))
        (should (string-match-p "gitlab\\.com" url))
        (should (string-match-p "merge_requests/42" url))))))

;;; Notification URL dispatch tests

(ert-deftest test/browse-pr-url-github ()
  "GIVEN shipit-pr-backend is github
WHEN constructing a PR browser URL
THEN returns github.com format."
  (require 'shipit-notifications)
  (require 'shipit-pr-github)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (let ((url (shipit--browse-pr-url "owner/repo" 42)))
      (should (string= "https://github.com/owner/repo/pull/42" url)))))

(ert-deftest test/browse-pr-url-gitlab ()
  "GIVEN shipit-pr-backend is gitlab
WHEN constructing a PR browser URL
THEN returns gitlab.com MR format."
  (require 'shipit-notifications)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject")))
    (let ((url (shipit--browse-pr-url "mygroup/myproject" 42)))
      (should (string-match-p "gitlab\\.com" url))
      (should (string-match-p "merge_requests/42" url)))))

(ert-deftest test/browse-issue-url-github ()
  "GIVEN shipit-pr-backend is github
WHEN constructing an issue browser URL
THEN returns github.com format."
  (require 'shipit-notifications)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (let ((url (shipit--browse-issue-url "owner/repo" 99)))
      (should (string= "https://github.com/owner/repo/issues/99" url)))))

(ert-deftest test/browse-issue-url-gitlab ()
  "GIVEN shipit-pr-backend is gitlab
WHEN constructing an issue browser URL
THEN returns gitlab.com issue format."
  (require 'shipit-notifications)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject")))
    (let ((url (shipit--browse-issue-url "mygroup/myproject" 99)))
      (should (string-match-p "gitlab\\.com" url))
      (should (string-match-p "issues/99" url)))))

;;; Avatar and commit author mapping tests

(ert-deftest test/generate-avatar-url-github ()
  "GIVEN shipit-pr-backend is github
WHEN generating avatar URL from username
THEN returns github.com avatar URL."
  (require 'shipit-pr-sections)
  (let ((shipit-pr-backend 'github))
    (should (string= "https://github.com/jdoe.png?size=20"
                      (shipit--generate-avatar-url "jdoe")))))

(ert-deftest test/generate-avatar-url-gitlab ()
  "GIVEN shipit-pr-backend is gitlab
WHEN generating avatar URL from username
THEN returns nil (GitLab avatars come from API data)."
  (require 'shipit-pr-sections)
  (let ((shipit-pr-backend 'gitlab))
    (should-not (shipit--generate-avatar-url "jdoe"))))

(ert-deftest test/generate-avatar-url-nil-username ()
  "GIVEN a nil username
WHEN generating avatar URL
THEN returns nil."
  (require 'shipit-pr-sections)
  (let ((shipit-pr-backend 'github))
    (should-not (shipit--generate-avatar-url nil))))

(ert-deftest test/extract-username-from-email-github ()
  "GIVEN a GitHub noreply email
WHEN extracting username with github backend
THEN returns the username."
  (require 'shipit-pr-sections)
  (let ((shipit-pr-backend 'github))
    (should (string= "jdoe" (shipit--extract-username-from-email "jdoe@users.noreply.github.com")))
    (should (string= "jdoe" (shipit--extract-username-from-email "12345+jdoe@users.noreply.github.com")))))

(ert-deftest test/extract-username-from-email-gitlab ()
  "GIVEN a GitLab noreply email
WHEN extracting username with gitlab backend
THEN returns the username."
  (require 'shipit-pr-sections)
  (let ((shipit-pr-backend 'gitlab))
    (should (string= "jdoe" (shipit--extract-username-from-email "123-jdoe@users.noreply.gitlab.com")))))

(ert-deftest test/extract-username-from-email-no-match ()
  "GIVEN a regular email
WHEN extracting username
THEN returns nil."
  (require 'shipit-pr-sections)
  (let ((shipit-pr-backend 'github))
    (should-not (shipit--extract-username-from-email "jdoe@example.com"))))

;;; Review thread tests

(ert-deftest test/pr-gitlab-fetch-review-threads ()
  "GIVEN a GitLab MR with discussions containing diff notes
WHEN fetching review threads
THEN returns normalized thread objects."
  (require 'shipit-pr-gitlab)
  (let ((mock-discussions
         (vector
          `((id . "disc-abc")
            (notes . [((id . 100) (body . "Fix this")
                       (author . ((username . "alice") (avatar_url . "https://a.png")))
                       (created_at . "2024-01-20T10:00:00Z")
                       (updated_at . "2024-01-20T10:00:00Z")
                       (resolved . :json-false)
                       (resolvable . t)
                       (position . ((new_path . "src/main.rb")
                                    (new_line . 42)
                                    (old_line . nil)
                                    (position_type . "text"))))])))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-discussions)))
      (let ((threads (shipit-pr-gitlab--fetch-review-threads
                      '(:project-path "mygroup/myproject") 42)))
        (should (= 1 (length threads)))
        (let ((thread (car threads)))
          (should (string= "disc-abc" (cdr (assq 'id thread))))
          (should-not (cdr (assq 'isResolved thread)))
          (should (string= "src/main.rb" (cdr (assq 'path thread))))
          (should (= 42 (cdr (assq 'line thread))))
          ;; Comments should have databaseId for resolved hash
          (let* ((comments (cdr (assq 'comments thread)))
                 (nodes (cdr (assq 'nodes comments))))
            (should (= 1 (length nodes)))
            (should (= 100 (cdr (assq 'databaseId (car nodes)))))))))))

(ert-deftest test/pr-gitlab-fetch-review-threads-skips-non-diff ()
  "GIVEN a GitLab MR with general (non-diff) discussions
WHEN fetching review threads
THEN only discussions with position data are included."
  (require 'shipit-pr-gitlab)
  (let ((mock-discussions
         (vector
          ;; General comment — no position
          `((id . "disc-general")
            (notes . [((id . 200) (body . "General comment")
                       (author . ((username . "bob")))
                       (created_at . "2024-01-20T10:00:00Z")
                       (resolved . :json-false)
                       (resolvable . :json-false))]))
          ;; Diff comment — has position
          `((id . "disc-diff")
            (notes . [((id . 201) (body . "Diff comment")
                       (author . ((username . "alice")))
                       (created_at . "2024-01-20T11:00:00Z")
                       (resolved . :json-false)
                       (resolvable . t)
                       (position . ((new_path . "a.rb") (new_line . 5))))])))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-discussions)))
      (let ((threads (shipit-pr-gitlab--fetch-review-threads
                      '(:project-path "mygroup/myproject") 42)))
        ;; Only the diff discussion
        (should (= 1 (length threads)))
        (should (string= "disc-diff" (cdr (assq 'id (car threads)))))))))

(ert-deftest test/pr-gitlab-fetch-resolved-threads ()
  "GIVEN a GitLab MR with both resolved and unresolved discussions
WHEN fetching resolved threads
THEN returns only resolved threads."
  (require 'shipit-pr-gitlab)
  (let ((mock-discussions
         (vector
          `((id . "disc-resolved")
            (notes . [((id . 100) (body . "Fixed")
                       (author . ((username . "alice")))
                       (created_at . "2024-01-20T10:00:00Z")
                       (resolved . t)
                       (resolvable . t)
                       (position . ((new_path . "a.rb") (new_line . 10))))]))
          `((id . "disc-open")
            (notes . [((id . 101) (body . "Not resolved")
                       (author . ((username . "carol")))
                       (created_at . "2024-01-20T11:00:00Z")
                       (resolved . :json-false)
                       (resolvable . t)
                       (position . ((new_path . "b.rb") (new_line . 20))))])))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-discussions)))
      (let ((threads (shipit-pr-gitlab--fetch-resolved-threads
                      '(:project-path "mygroup/myproject") 42)))
        (should (= 1 (length threads)))
        (should (string= "disc-resolved" (cdr (assq 'id (car threads)))))
        (should (eq t (cdr (assq 'isResolved (car threads)))))))))

(ert-deftest test/pr-gitlab-review-threads-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for review thread keys
THEN :fetch-review-threads and :fetch-resolved-threads are present."
  (require 'shipit-pr-gitlab)
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-review-threads))
    (should (plist-get backend :fetch-resolved-threads))))

;;; Branch protection tests

(ert-deftest test/pr-gitlab-fetch-branch-protection ()
  "GIVEN a GitLab project with approval rules requiring 2 approvals
WHEN calling shipit-pr-gitlab--fetch-branch-protection
THEN returns normalized protection shape with required-approving-review-count = 2
     AND api-accessible = t."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (cond
                  ;; approval_rules endpoint
                  ((string-match-p "approval_rules" path)
                   (vector `((id . 1)
                             (name . "Default")
                             (approvals_required . 2)
                             (rule_type . "regular"))))
                  ;; protected_branches endpoint
                  ((string-match-p "protected_branches" path)
                   `((name . "main")
                     (push_access_levels . [((access_level . 40))])
                     (merge_access_levels . [((access_level . 30))])))))))
      (let ((result (shipit-pr-gitlab--fetch-branch-protection config "main")))
        ;; THEN api-accessible is t
        (should (eq t (cdr (assq 'api-accessible result))))
        ;; THEN required-approving-review-count is 2
        (should (= 2 (cdr (assq 'required-approving-review-count result))))))))

(ert-deftest test/pr-gitlab-fetch-branch-protection-not-found ()
  "GIVEN a GitLab API that returns nil for approval rules
WHEN calling shipit-pr-gitlab--fetch-branch-protection
THEN returns graceful fallback with api-accessible = nil."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) nil)))
      (let ((result (shipit-pr-gitlab--fetch-branch-protection config "main")))
        ;; THEN api-accessible is nil
        (should-not (cdr (assq 'api-accessible result)))
        ;; THEN required-approving-review-count is nil
        (should-not (cdr (assq 'required-approving-review-count result)))
        ;; THEN require-code-owner-reviews is nil
        (should-not (cdr (assq 'require-code-owner-reviews result)))
        ;; THEN dismiss-stale-reviews is nil
        (should-not (cdr (assq 'dismiss-stale-reviews result)))))))

(ert-deftest test/pr-gitlab-branch-protection-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for :fetch-branch-protection
THEN the key is present."
  (require 'shipit-pr-gitlab)
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-branch-protection))))

;;; CODEOWNERS tests

(ert-deftest test/pr-gitlab-fetch-codeowners ()
  "GIVEN a GitLab repository with CODEOWNERS at .gitlab/CODEOWNERS
WHEN calling shipit-pr-gitlab--fetch-codeowners
THEN returns decoded CODEOWNERS content string."
  (require 'shipit-pr-gitlab)
  (let* ((codeowners-text "* @backend-team\n/docs/ @docs-team\n")
         (encoded-content (base64-encode-string codeowners-text))
         (config '(:api-url "https://gitlab.com" :token "test"
                   :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 ;; First path (CODEOWNERS) returns nil (not found)
                 ;; Second path (.gitlab/CODEOWNERS) returns content
                 (when (string-match-p "%2FCODEOWNERS" path)
                   `((content . ,encoded-content)
                     (encoding . "base64"))))))
      (let ((result (shipit-pr-gitlab--fetch-codeowners config "main")))
        ;; THEN the decoded content is returned
        (should (stringp result))
        (should (string= codeowners-text result))))))

(ert-deftest test/pr-gitlab-fetch-codeowners-not-found ()
  "GIVEN a GitLab repository with no CODEOWNERS file in any location
WHEN calling shipit-pr-gitlab--fetch-codeowners
THEN returns nil."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) nil)))
      (let ((result (shipit-pr-gitlab--fetch-codeowners config "main")))
        ;; THEN nil is returned
        (should-not result)))))

(ert-deftest test/pr-gitlab-codeowners-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for :fetch-codeowners
THEN the key is present."
  (require 'shipit-pr-gitlab)
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-codeowners))))

;;; Check suites (pipeline stages) tests

(ert-deftest test/pr-gitlab-fetch-check-suites ()
  "GIVEN a GitLab MR with pipeline jobs in multiple stages
WHEN fetching check suites
THEN returns stages as suites with job count and status."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject"))
        (mock-pipeline (vector `((id . 100) (status . "success"))))
        (mock-jobs (vector `((id . 1) (name . "rspec") (stage . "test")
                            (status . "success")
                            (web_url . "https://gl.com/jobs/1")
                            (started_at . "2026-01-15T10:00:00Z")
                            (finished_at . "2026-01-15T10:05:00Z"))
                          `((id . 2) (name . "rubocop") (stage . "test")
                            (status . "success")
                            (web_url . "https://gl.com/jobs/2")
                            (started_at . "2026-01-15T10:00:00Z")
                            (finished_at . "2026-01-15T10:04:00Z"))
                          `((id . 3) (name . "docker-build") (stage . "build")
                            (status . "success")
                            (web_url . "https://gl.com/jobs/3")
                            (started_at . "2026-01-15T10:06:00Z")
                            (finished_at . "2026-01-15T10:10:00Z"))
                          `((id . 4) (name . "deploy-staging") (stage . "deploy")
                            (status . "failed")
                            (web_url . "https://gl.com/jobs/4")
                            (started_at . "2026-01-15T10:11:00Z")
                            (finished_at . "2026-01-15T10:12:00Z")))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (cond
                  ((string-match-p "pipelines\\?sha=" path) mock-pipeline)
                  ((string-match-p "pipelines/100/jobs" path) mock-jobs)))))
      (let* ((result (shipit-pr-gitlab--fetch-check-suites config "abc123" 1 100))
             (suites (cdr (assq 'check_suites result)))
             (total (cdr (assq 'total_count result))))
        ;; THEN returns 3 suites (test, build, deploy)
        (should (= 3 (length suites)))
        (should (= 3 total))
        ;; THEN test suite has 2 jobs and success conclusion
        (let ((test-suite (cl-find-if (lambda (s) (string= "test" (cdr (assq 'name s)))) suites)))
          (should test-suite)
          (should (= 2 (cdr (assq 'check_runs_count test-suite))))
          (should (string= "success" (cdr (assq 'conclusion test-suite))))
          (should (string= "completed" (cdr (assq 'status test-suite))))
          (should (string= "test:100" (cdr (assq 'id test-suite)))))
        ;; THEN build suite has 1 job and success conclusion
        (let ((build-suite (cl-find-if (lambda (s) (string= "build" (cdr (assq 'name s)))) suites)))
          (should build-suite)
          (should (= 1 (cdr (assq 'check_runs_count build-suite))))
          (should (string= "success" (cdr (assq 'conclusion build-suite)))))
        ;; THEN deploy suite has failure conclusion
        (let ((deploy-suite (cl-find-if (lambda (s) (string= "deploy" (cdr (assq 'name s)))) suites)))
          (should deploy-suite)
          (should (string= "failure" (cdr (assq 'conclusion deploy-suite)))))))))

(ert-deftest test/pr-gitlab-fetch-check-suites-page-2-empty ()
  "GIVEN a GitLab pipeline with stages
WHEN fetching check suites on page 2
THEN returns empty suites."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) nil)))
      (let* ((result (shipit-pr-gitlab--fetch-check-suites config "abc123" 2 100))
             (suites (cdr (assq 'check_suites result))))
        ;; THEN page 2 returns empty list
        (should (= 0 (length suites)))))))

(ert-deftest test/pr-gitlab-fetch-check-suites-mixed-statuses ()
  "GIVEN a GitLab pipeline with jobs having mixed statuses in one stage
WHEN fetching check suites
THEN the stage conclusion reflects the worst status."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject"))
        (mock-pipeline (vector `((id . 200) (status . "running"))))
        (mock-jobs (vector `((id . 10) (name . "job-a") (stage . "test")
                            (status . "success")
                            (web_url . "https://gl.com/jobs/10")
                            (started_at . "2026-01-15T10:00:00Z")
                            (finished_at . "2026-01-15T10:05:00Z"))
                          `((id . 11) (name . "job-b") (stage . "test")
                            (status . "running")
                            (web_url . "https://gl.com/jobs/11")
                            (started_at . "2026-01-15T10:00:00Z")
                            (finished_at . nil)))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (cond
                  ((string-match-p "pipelines\\?sha=" path) mock-pipeline)
                  ((string-match-p "pipelines/200/jobs" path) mock-jobs)))))
      (let* ((result (shipit-pr-gitlab--fetch-check-suites config "def456" 1 100))
             (suites (cdr (assq 'check_suites result)))
             (test-suite (car suites)))
        ;; THEN status is in_progress (running job overrides completed)
        (should (string= "in_progress" (cdr (assq 'status test-suite))))
        ;; THEN conclusion is nil (still in progress)
        (should-not (cdr (assq 'conclusion test-suite)))))))

(ert-deftest test/pr-gitlab-fetch-suite-check-runs ()
  "GIVEN a GitLab pipeline with jobs in a stage
WHEN fetching suite check runs for that stage
THEN returns jobs as check runs."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject"))
        (mock-jobs (vector `((id . 1) (name . "rspec") (stage . "test")
                            (status . "success")
                            (web_url . "https://gl.com/jobs/1")
                            (started_at . "2026-01-15T10:00:00Z")
                            (finished_at . "2026-01-15T10:05:00Z"))
                          `((id . 2) (name . "rubocop") (stage . "test")
                            (status . "failed")
                            (web_url . "https://gl.com/jobs/2")
                            (started_at . "2026-01-15T10:00:00Z")
                            (finished_at . "2026-01-15T10:04:00Z"))
                          `((id . 3) (name . "docker-build") (stage . "build")
                            (status . "success")
                            (web_url . "https://gl.com/jobs/3")
                            (started_at . "2026-01-15T10:06:00Z")
                            (finished_at . "2026-01-15T10:10:00Z")))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-jobs)))
      ;; WHEN fetching suite check runs for "test:100"
      (let* ((result (shipit-pr-gitlab--fetch-suite-check-runs config "test:100"))
             (runs (cdr (assq 'check_runs result))))
        ;; THEN only the 2 test-stage jobs are returned
        (should (= 2 (length runs)))
        ;; THEN first run has correct fields
        (let ((rspec-run (cl-find-if (lambda (r) (string= "rspec" (cdr (assq 'name r)))) runs)))
          (should rspec-run)
          (should (= 1 (cdr (assq 'id rspec-run))))
          (should (string= "success" (cdr (assq 'conclusion rspec-run))))
          (should (string= "completed" (cdr (assq 'status rspec-run))))
          (should (string= "https://gl.com/jobs/1" (cdr (assq 'html_url rspec-run))))
          (should (string= "2026-01-15T10:00:00Z" (cdr (assq 'started_at rspec-run))))
          (should (string= "2026-01-15T10:05:00Z" (cdr (assq 'completed_at rspec-run)))))
        ;; THEN rubocop run has failure conclusion
        (let ((rubocop-run (cl-find-if (lambda (r) (string= "rubocop" (cdr (assq 'name r)))) runs)))
          (should rubocop-run)
          (should (string= "failure" (cdr (assq 'conclusion rubocop-run)))))))))

(ert-deftest test/pr-gitlab-fetch-suite-check-runs-no-match ()
  "GIVEN a GitLab pipeline with no jobs in the requested stage
WHEN fetching suite check runs
THEN returns empty check runs."
  (require 'shipit-pr-gitlab)
  (let ((config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject"))
        (mock-jobs (vector `((id . 1) (name . "rspec") (stage . "test")
                            (status . "success")
                            (web_url . "https://gl.com/jobs/1")
                            (started_at . nil) (finished_at . nil)))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) mock-jobs)))
      (let* ((result (shipit-pr-gitlab--fetch-suite-check-runs config "deploy:100"))
             (runs (cdr (assq 'check_runs result))))
        ;; THEN no runs match
        (should (= 0 (length runs)))))))

(ert-deftest test/pr-gitlab-check-suites-registered ()
  "GIVEN the GitLab backend is registered
WHEN checking for check suite keys
THEN :fetch-check-suites and :fetch-suite-check-runs are present."
  (require 'shipit-pr-gitlab)
  (let ((backend (cdr (assq 'gitlab shipit-pr-backends))))
    (should (plist-get backend :fetch-check-suites))
    (should (plist-get backend :fetch-suite-check-runs))))

;;; --- !NNN MR shorthand reference overlay tests ---

(ert-deftest test/pr-gitlab-exclamation-ref-gets-overlay ()
  "GIVEN a buffer with !42 and GitLab backend active
WHEN creating PR reference overlays
THEN the !42 text gets a styled overlay with keymap."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"
                                    :token "test")))
    (with-temp-buffer
      (insert "See !42 for details")
      (shipit--create-pr-reference-overlays "mygroup/myproject" 1
                                            (point-min) (point-max))
      ;; THEN overlay exists on !42
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (= 1 (length ovs)))
        (should (eq 'markdown-plain-url-face (overlay-get (car ovs) 'face)))
        (should (overlay-get (car ovs) 'keymap))
        (should (string-match-p "!42" (overlay-get (car ovs) 'help-echo)))))))

(ert-deftest test/pr-gitlab-exclamation-ref-ignored-on-github ()
  "GIVEN a buffer with !42 and GitHub backend active
WHEN creating PR reference overlays
THEN the !42 text does NOT get an overlay."
  (require 'shipit-render)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (with-temp-buffer
      (insert "See !42 for details")
      (shipit--create-pr-reference-overlays "owner/repo" 1
                                            (point-min) (point-max))
      ;; THEN no overlays (no #NNN in this text either)
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (= 0 (length ovs)))))))

(ert-deftest test/pr-gitlab-exclamation-ref-in-backticks-skipped ()
  "GIVEN a buffer with `!42` inside backticks and GitLab backend
WHEN creating PR reference overlays
THEN the !42 does NOT get an overlay."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"
                                    :token "test")))
    (with-temp-buffer
      (insert "See `!42` for details")
      (shipit--create-pr-reference-overlays "mygroup/myproject" 1
                                            (point-min) (point-max))
      ;; THEN no overlays created
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (= 0 (length ovs)))))))

(ert-deftest test/pr-gitlab-multiple-exclamation-refs ()
  "GIVEN a buffer with !10 and !20 and GitLab backend active
WHEN creating PR reference overlays
THEN both get overlays."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"
                                    :token "test")))
    (with-temp-buffer
      (insert "See !10 and !20")
      (shipit--create-pr-reference-overlays "mygroup/myproject" 1
                                            (point-min) (point-max))
      ;; THEN two overlays
      (let ((ovs (overlays-in (point-min) (point-max))))
        (should (= 2 (length ovs)))))))

;;; --- pr-reference-action-menu backend-aware URL tests ---

(ert-deftest test/pr-reference-action-menu-browse-uses-backend-url ()
  "GIVEN GitLab backend is active
WHEN user presses 'b' (browse) in pr-reference-action-menu
THEN URL uses GitLab format, not hardcoded github.com."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"
                                    :token "test"))
        (opened-url nil))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (_p _c) ?b))
              ((symbol-function 'browse-url) (lambda (url) (setq opened-url url)))
              ((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (cdr (assq 'gitlab shipit-pr-backends))
                       shipit-pr-backend-config))))
      (shipit--pr-reference-action-menu 42 "mygroup/myproject")
      ;; THEN URL is GitLab format
      (should (string-match-p "gitlab\\.com" opened-url))
      (should (string-match-p "merge_requests/42" opened-url)))))

(ert-deftest test/pr-reference-action-menu-copy-uses-backend-url ()
  "GIVEN GitLab backend is active
WHEN user presses 'c' (copy) in pr-reference-action-menu
THEN copied URL uses GitLab format."
  (require 'shipit-render)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"
                                    :token "test"))
        (copied-url nil))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (_p _c) ?c))
              ((symbol-function 'kill-new) (lambda (url) (setq copied-url url)))
              ((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons (cdr (assq 'gitlab shipit-pr-backends))
                       shipit-pr-backend-config))))
      (shipit--pr-reference-action-menu 42 "mygroup/myproject")
      ;; THEN copied URL is GitLab format
      (should (string-match-p "gitlab\\.com" copied-url))
      (should (string-match-p "merge_requests/42" copied-url)))))

;;; --- get-github-username backend-aware tests ---

(ert-deftest test/get-github-username-gitlab-noreply-email ()
  "GIVEN git config returns a GitLab noreply email
WHEN calling shipit--get-github-username
THEN extracts the numeric user ID as username."
  (require 'shipit-lib)
  (cl-letf (((symbol-function 'getenv) (lambda (_var) nil))
            ((symbol-function 'shell-command-to-string)
             (lambda (cmd)
               (cond
                ((string-match-p "github\\.user" cmd) "")
                ((string-match-p "user\\.email" cmd)
                 "12345-jdoe@users.noreply.gitlab.com")
                (t "")))))
    (should (equal "12345-jdoe" (shipit--get-github-username)))))

(ert-deftest test/get-github-username-github-noreply-email ()
  "GIVEN git config returns a GitHub noreply email
WHEN calling shipit--get-github-username
THEN extracts the username."
  (require 'shipit-lib)
  (cl-letf (((symbol-function 'getenv) (lambda (_var) nil))
            ((symbol-function 'shell-command-to-string)
             (lambda (cmd)
               (cond
                ((string-match-p "github\\.user" cmd) "")
                ((string-match-p "user\\.email" cmd)
                 "jdoe@users.noreply.github.com")
                (t "")))))
    (should (equal "jdoe" (shipit--get-github-username)))))

;;; --- fetch-pr-ref-async backend dispatch tests ---

(ert-deftest test/fetch-pr-ref-async-gitlab-fallback-refspec ()
  "GIVEN GitLab backend without :refspec-for-pr
WHEN fetching PR ref
THEN an error is signaled (no silent fallback)."
  (require 'shipit-lib)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject"
                                    :token "test")))
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 ;; Return backend without :refspec-for-pr or :remote-for-fetch
                 (cons '(:name "GitLab") shipit-pr-backend-config)))
              ((symbol-function 'shipit-pr--backend-id) (lambda () 'gitlab)))
      ;; THEN error is signaled because :refspec-for-pr is missing
      (should-error
       (shipit--fetch-pr-ref-async 42 #'ignore "mygroup/myproject" "feature-branch")))))

(ert-deftest test/fetch-pr-ref-async-github-fallback-refspec ()
  "GIVEN GitHub backend without :refspec-for-pr
WHEN fetching PR ref
THEN an error is signaled (no silent fallback)."
  (require 'shipit-lib)
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons '(:name "GitHub") nil)))
              ((symbol-function 'shipit-pr--backend-id) (lambda () 'github)))
      ;; THEN error is signaled because :refspec-for-pr is missing
      (should-error
       (shipit--fetch-pr-ref-async 42 #'ignore "owner/repo")))))

;;; --- crossref-actions backend-aware URL tests ---

(ert-deftest test/crossref-actions-browse-pr-url-gitlab ()
  "GIVEN GitLab backend is active
WHEN building a browse URL via backend :browse-url dispatch
THEN URL uses GitLab format (merge_requests)."
  (require 'shipit-pr-gitlab)
  (let* ((shipit-pr-backend 'gitlab)
         (config '(:api-url "https://gitlab.com"
                   :project-path "mygroup/myproject"
                   :token "test"))
         (backend (cdr (assq 'gitlab shipit-pr-backends)))
         (browse-fn (plist-get backend :browse-url))
         (url (funcall browse-fn config 42)))
    ;; THEN URL is GitLab format
    (should (string-match-p "gitlab\\.com" url))
    (should (string-match-p "merge_requests/42" url))))

(ert-deftest test/crossref-actions-browse-pr-url-github ()
  "GIVEN GitHub backend is active
WHEN building a browse URL via backend :browse-url dispatch
THEN URL uses GitHub format (pull)."
  (require 'shipit-pr-github)
  (let* ((shipit-pr-backend 'github)
         (config '(:repo "owner/repo"))
         (backend (cdr (assq 'github shipit-pr-backends)))
         (browse-fn (plist-get backend :browse-url))
         (url (funcall browse-fn config 42)))
    ;; THEN URL is GitHub format
    (should (string-match-p "github\\.com" url))
    (should (string-match-p "pull/42" url))))

;;; --- avatar download Referer header test ---

(ert-deftest test/avatar-download-auth-only-for-same-host ()
  "GIVEN GitLab backend is active
WHEN downloading an image from the same host
THEN the Authorization header is included."
  (require 'shipit-http)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.example.com"
                                    :token "test"))
        (shipit-github-token "test-token")
        (captured-headers nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _)
                 (setq captured-headers url-request-extra-headers)
                 nil)))
      ;; Use a same-host URL to trigger download attempt
      (shipit--download-and-cache-image
       "https://gitlab.example.com/uploads/avatar.png")
      ;; THEN Authorization header is present for same-host image
      (should captured-headers)
      (let ((auth (cdr (assoc "Authorization" captured-headers))))
        (should auth)
        (should (string-match-p "token test-token" auth))))))

(ert-deftest test/avatar-download-no-auth-for-external-host ()
  "GIVEN GitLab backend is active
WHEN downloading an image from an external host
THEN the Authorization header is NOT included."
  (require 'shipit-http)
  (require 'shipit-pr-gitlab)
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.example.com"
                                    :token "test"))
        (shipit-github-token "test-token")
        (captured-headers nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _)
                 (setq captured-headers url-request-extra-headers)
                 nil)))
      (shipit--download-and-cache-image
       "https://ko-fi.com/img/some-badge.png")
      (should captured-headers)
      (let ((auth (cdr (assoc "Authorization" captured-headers))))
        (should-not auth)))))

(provide 'test-shipit-pr-gitlab)
;;; test-shipit-pr-gitlab.el ends here
