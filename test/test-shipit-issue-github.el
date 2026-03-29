;;; test-shipit-issue-github.el --- Tests for GitHub issue backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the GitHub issue backend.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)

;;; Registration Tests

(ert-deftest test-shipit-issue-github-registered ()
  "GIVEN shipit-issue-github is loaded
WHEN checking the backend registry
THEN 'github is registered with name \"GitHub\"."
  (should (assq 'github shipit-issue-backends))
  (should (string= "GitHub" (plist-get (cdr (assq 'github shipit-issue-backends)) :name))))

;;; Reference Pattern Tests

(ert-deftest test-shipit-issue-github-reference-patterns ()
  "GIVEN the github backend
WHEN calling :reference-patterns
THEN it returns a pattern matching #NNN."
  (let* ((backend (cdr (assq 'github shipit-issue-backends)))
         (patterns (funcall (plist-get backend :reference-patterns) nil)))
    (should (= 1 (length patterns)))
    (let ((pattern (car patterns)))
      ;; Pattern should match #42
      (should (string-match (car pattern) "#42"))
      ;; Extractor should convert "42" to number
      (should (= 42 (funcall (nth 2 pattern) (match-string (nth 1 pattern) "#42")))))))

;;; ID Conversion Tests

(ert-deftest test-shipit-issue-github-id-to-string ()
  "GIVEN the github backend
WHEN converting issue id 42 to string
THEN it returns \"#42\"."
  (let* ((backend (cdr (assq 'github shipit-issue-backends)))
         (id-to-string (plist-get backend :id-to-string)))
    (should (string= "#42" (funcall id-to-string 42)))))

(ert-deftest test-shipit-issue-github-string-to-id ()
  "GIVEN the github backend
WHEN converting string \"#42\" to id
THEN it returns 42."
  (let* ((backend (cdr (assq 'github shipit-issue-backends)))
         (string-to-id (plist-get backend :string-to-id)))
    (should (= 42 (funcall string-to-id "#42")))))

;;; Browse URL Tests

(ert-deftest test-shipit-issue-github-browse-url ()
  "GIVEN a config with :repo \"owner/repo\"
WHEN calling :browse-url with id 42
THEN it returns the GitHub issue URL."
  (let* ((backend (cdr (assq 'github shipit-issue-backends)))
         (browse-url-fn (plist-get backend :browse-url))
         (config '(:repo "owner/repo")))
    (should (string= "https://github.com/owner/repo/issues/42"
                      (funcall browse-url-fn config 42)))))

;;; Fetch Delegation Tests

(ert-deftest test-shipit-issue-github-fetch-issue-calls-api ()
  "GIVEN the github backend
WHEN calling :fetch-issue
THEN it calls shipit-gh-etag-get-json-with-refresh-cache with correct endpoint."
  (let ((called-endpoint nil))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint &rest _args)
                 (setq called-endpoint endpoint)
                 (list :json '((number . 42) (title . "Test"))))))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (fetch-fn (plist-get backend :fetch-issue))
             (config '(:repo "owner/repo")))
        (funcall fetch-fn config 42)
        (should (string= "/repos/owner/repo/issues/42" called-endpoint))))))

(ert-deftest test-shipit-issue-github-fetch-comments-calls-api ()
  "GIVEN the github backend
WHEN calling :fetch-comments
THEN it calls shipit--api-request-paginated with correct endpoint."
  (let ((called-endpoint nil))
    (cl-letf (((symbol-function 'shipit--api-request-paginated)
               (lambda (endpoint &rest _args)
                 (setq called-endpoint endpoint)
                 nil)))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (fetch-fn (plist-get backend :fetch-comments))
             (config '(:repo "owner/repo")))
        (funcall fetch-fn config 42)
        (should (string= "/repos/owner/repo/issues/42/comments" called-endpoint))))))

(ert-deftest test-shipit-issue-github-search-calls-api ()
  "GIVEN the github backend with transient args
WHEN calling :search
THEN it builds query parts and calls shipit--search-prs-with-encoded-query."
  (let ((called-args nil))
    (cl-letf (((symbol-function 'shipit--search-prs-with-encoded-query)
               (lambda (repo query-parts per-page &rest _args)
                 (setq called-args (list repo query-parts per-page))
                 nil)))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (search-fn (plist-get backend :search))
             (config '(:repo "owner/repo")))
        (funcall search-fn config '("--state=open" "--limit=50" "--sort=created"))
        (should called-args)
        (should (string= "owner/repo" (nth 0 called-args)))
        ;; Query parts should include is:issue and state:open
        (let ((qparts (nth 1 called-args)))
          (should (member "is:issue" qparts))
          (should (member "state:open" qparts)))))))

;;; Timeline Normalizer Tests

(ert-deftest test-shipit-issue-github-timeline-closed ()
  "GIVEN a 'closed' timeline event
WHEN normalizing the event
THEN it returns field=status, from=open, to=closed."
  (let* ((event `((event . "closed")
                  (created_at . "2025-01-15T10:00:00Z")
                  (actor . ((login . "alice") (avatar_url . "https://example.com/alice.png")))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (should result)
    (should (string= "2025-01-15T10:00:00Z" (cdr (assq 'created_at result))))
    (should (string= "alice" (cdr (assq 'login (cdr (assq 'user result))))))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "status" (cdr (assq 'field item))))
      (should (string= "open" (cdr (assq 'from item))))
      (should (string= "closed" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-closed-not-planned ()
  "GIVEN a 'closed' event with state_reason=not_planned
WHEN normalizing the event
THEN to=closed (not_planned)."
  (let* ((event `((event . "closed")
                  (created_at . "2025-01-15T10:00:00Z")
                  (state_reason . "not_planned")
                  (actor . ((login . "alice") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "closed (not_planned)" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-reopened ()
  "GIVEN a 'reopened' timeline event
WHEN normalizing the event
THEN it returns field=status, from=closed, to=open."
  (let* ((event `((event . "reopened")
                  (created_at . "2025-01-16T10:00:00Z")
                  (actor . ((login . "bob") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "status" (cdr (assq 'field item))))
      (should (string= "closed" (cdr (assq 'from item))))
      (should (string= "open" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-labeled ()
  "GIVEN a 'labeled' timeline event
WHEN normalizing the event
THEN it returns field=labels, from=\"\", to=label name."
  (let* ((event `((event . "labeled")
                  (created_at . "2025-01-15T10:00:00Z")
                  (label . ((name . "bug")))
                  (actor . ((login . "alice") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "labels" (cdr (assq 'field item))))
      (should (string= "" (cdr (assq 'from item))))
      (should (string= "bug" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-unlabeled ()
  "GIVEN an 'unlabeled' timeline event
WHEN normalizing the event
THEN it returns field=labels, from=label name, to=\"\"."
  (let* ((event `((event . "unlabeled")
                  (created_at . "2025-01-15T10:00:00Z")
                  (label . ((name . "wontfix")))
                  (actor . ((login . "alice") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "labels" (cdr (assq 'field item))))
      (should (string= "wontfix" (cdr (assq 'from item))))
      (should (string= "" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-assigned ()
  "GIVEN an 'assigned' timeline event
WHEN normalizing the event
THEN it returns field=assignee, from=\"\", to=login."
  (let* ((event `((event . "assigned")
                  (created_at . "2025-01-15T10:00:00Z")
                  (assignee . ((login . "charlie")))
                  (actor . ((login . "alice") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "assignee" (cdr (assq 'field item))))
      (should (string= "" (cdr (assq 'from item))))
      (should (string= "charlie" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-renamed ()
  "GIVEN a 'renamed' timeline event
WHEN normalizing the event
THEN it returns field=title with old and new titles."
  (let* ((event `((event . "renamed")
                  (created_at . "2025-01-15T10:00:00Z")
                  (rename . ((from . "Old title") (to . "New title")))
                  (actor . ((login . "alice") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "title" (cdr (assq 'field item))))
      (should (string= "Old title" (cdr (assq 'from item))))
      (should (string= "New title" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-milestoned ()
  "GIVEN a 'milestoned' timeline event
WHEN normalizing the event
THEN it returns field=milestone, from=\"\", to=title."
  (let* ((event `((event . "milestoned")
                  (created_at . "2025-01-15T10:00:00Z")
                  (milestone . ((title . "v2.0")))
                  (actor . ((login . "alice") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (let ((item (car (cdr (assq 'items result)))))
      (should (string= "milestone" (cdr (assq 'field item))))
      (should (string= "" (cdr (assq 'from item))))
      (should (string= "v2.0" (cdr (assq 'to item)))))))

(ert-deftest test-shipit-issue-github-timeline-skips-commented ()
  "GIVEN a 'commented' timeline event
WHEN normalizing the event
THEN it returns nil (skipped)."
  (let* ((event `((event . "commented")
                  (created_at . "2025-01-15T10:00:00Z")
                  (actor . ((login . "alice") (avatar_url . nil)))))
         (result (shipit-issue-github--normalize-timeline-event event)))
    (should-not result)))

(ert-deftest test-shipit-issue-github-timeline-empty ()
  "GIVEN an empty timeline
WHEN normalizing it
THEN it returns nil."
  (should-not (shipit-issue-github--normalize-timeline nil))
  (should-not (shipit-issue-github--normalize-timeline [])))

(ert-deftest test-shipit-issue-github-fetch-issue-includes-changelog ()
  "GIVEN a mocked API that returns issue data and timeline events
WHEN calling fetch-issue
THEN the result includes a changelog key with normalized events."
  (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
             (lambda (_endpoint &rest _args)
               (list :json '((number . 42) (title . "Test issue")))))
            ((symbol-function 'shipit--api-request-paginated)
             (lambda (endpoint &rest _args)
               (if (string-match "timeline" endpoint)
                   `[((event . "closed")
                      (created_at . "2025-01-15T10:00:00Z")
                      (actor . ((login . "alice") (avatar_url . nil))))
                     ((event . "commented")
                      (created_at . "2025-01-15T11:00:00Z")
                      (actor . ((login . "bob") (avatar_url . nil))))]
                 nil))))
    (let* ((config '(:repo "owner/repo"))
           (result (shipit-issue-github--fetch-issue config 42))
           (changelog (cdr (assq 'changelog result))))
      (should (equal 42 (cdr (assq 'number result))))
      (should changelog)
      ;; Only 1 event (commented is skipped)
      (should (= 1 (length changelog)))
      (let ((entry (car changelog)))
        (should (string= "alice" (cdr (assq 'login (cdr (assq 'user entry))))))))))

;;; Reaction Function Tests

(ert-deftest test-shipit-issue-github-fetch-reactions ()
  "GIVEN a GitHub config with :repo
WHEN calling fetch-reactions for issue 42
THEN it calls GET /repos/{repo}/issues/42/reactions and returns reactions."
  (let ((called-endpoint nil))
    (cl-letf (((symbol-function 'shipit--api-request-paginated)
               (lambda (endpoint &rest _args)
                 (setq called-endpoint endpoint)
                 '(((id . 1) (content . "+1") (user . ((login . "alice"))))))))
      (let* ((config '(:repo "owner/repo"))
             (result (shipit-issue-github--fetch-reactions config 42)))
        (should (string= "/repos/owner/repo/issues/42/reactions" called-endpoint))
        (should (= 1 (length result)))
        (should (equal "+1" (cdr (assq 'content (car result)))))))))

(ert-deftest test-shipit-issue-github-add-reaction ()
  "GIVEN a GitHub config with :repo
WHEN calling add-reaction for issue 42 with \"+1\"
THEN it calls POST /repos/{repo}/issues/42/reactions with correct data."
  (let ((called-endpoint nil)
        (called-data nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &rest _args)
                 (setq called-endpoint endpoint
                       called-data data)
                 '((id . 99) (content . "+1")))))
      (let* ((config '(:repo "owner/repo"))
             (result (shipit-issue-github--add-reaction config 42 "+1")))
        (should (string= "/repos/owner/repo/issues/42/reactions" called-endpoint))
        (should (equal "+1" (cdr (assq 'content called-data))))
        (should (equal 99 (cdr (assq 'id result))))))))

(ert-deftest test-shipit-issue-github-remove-reaction ()
  "GIVEN a GitHub config with :repo
WHEN calling remove-reaction for issue 42 with reaction-id 99
THEN it calls DELETE /repos/{repo}/issues/42/reactions/99."
  (let ((called-url nil)
        (called-method nil))
    (cl-letf (((symbol-function 'shipit--url-retrieve-sync)
               (lambda (url method _headers _data &optional _skip-json)
                 (setq called-url url
                       called-method method)
                 204)))
      (let ((config '(:repo "owner/repo")))
        (shipit-issue-github--remove-reaction config 42 99)
        (should (string-match-p "/repos/owner/repo/issues/42/reactions/99" called-url))
        (should (string= "DELETE" called-method))))))

(ert-deftest test-shipit-issue-github-registered-with-reactions ()
  "GIVEN shipit-issue-github is loaded
WHEN checking the backend registry
THEN :fetch-reactions, :add-reaction, :remove-reaction are present."
  (let ((backend (cdr (assq 'github shipit-issue-backends))))
    (should (plist-get backend :fetch-reactions))
    (should (plist-get backend :add-reaction))
    (should (plist-get backend :remove-reaction))))

(provide 'test-shipit-issue-github)
;;; test-shipit-issue-github.el ends here
