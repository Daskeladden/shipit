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

(ert-deftest test-shipit-issue-github-fetch-issue-does-not-fetch-events ()
  "GIVEN a mocked API for issue GET
WHEN calling fetch-issue
THEN only the issue GET is called and no changelog is attached.
Events are fetched separately via `:fetch-timeline-async' so the
buffer renders without waiting for paginated events on popular issues."
  (let ((issue-get-called nil)
        (paginated-called nil))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-with-refresh-cache)
               (lambda (endpoint &rest _args)
                 (setq issue-get-called endpoint)
                 (list :json '((number . 42) (title . "Test issue")))))
              ((symbol-function 'shipit--api-request-paginated)
               (lambda (endpoint &rest _args)
                 (setq paginated-called endpoint)
                 nil)))
      (let* ((config '(:repo "owner/repo"))
             (result (shipit-issue-github--fetch-issue config 42)))
        (should (equal 42 (cdr (assq 'number result))))
        (should (string= "/repos/owner/repo/issues/42" issue-get-called))
        (should-not paginated-called)
        (should-not (assq 'changelog result))))))

(ert-deftest test-shipit-issue-github-fetch-timeline-async-uses-events-endpoint ()
  "GIVEN a mocked paginated async fetcher
WHEN calling fetch-timeline-async
THEN the events endpoint is used, pagination is capped, and the
callback receives normalized changelog entries (subscribed/mentioned
noise filtered out)."
  (let (captured-endpoint captured-max-pages received-changelog)
    (cl-letf (((symbol-function 'shipit--api-request-paginated-async)
               (lambda (endpoint callback &optional _per-page max-pages)
                 (setq captured-endpoint endpoint
                       captured-max-pages max-pages)
                 (funcall callback
                          `[((event . "closed")
                             (created_at . "2025-01-15T10:00:00Z")
                             (actor . ((login . "alice") (avatar_url . nil))))
                            ((event . "subscribed")
                             (created_at . "2025-01-15T11:00:00Z")
                             (actor . ((login . "bob") (avatar_url . nil))))]))))
      (shipit-issue-github--fetch-timeline-async
       '(:repo "owner/repo") 42
       (lambda (changelog) (setq received-changelog changelog)))
      (should (string= "/repos/owner/repo/issues/42/events" captured-endpoint))
      (should (equal shipit-issue-github--timeline-max-pages captured-max-pages))
      (should received-changelog)
      (should (= 1 (length received-changelog)))
      (should (string= "alice"
                       (cdr (assq 'login
                                  (cdr (assq 'user
                                             (car received-changelog))))))))))

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

(ert-deftest test-shipit-issue-github-fetch-reactions-async-caps-at-page-1 ()
  "GIVEN a GitHub config with :repo
WHEN calling fetch-reactions-async for an issue
THEN the paginated async fetch is capped at max-pages=1.
This prevents popular issues (thousands of reactions) from triggering
a long pagination storm; accurate counts come from the summary cache
populated at buffer render time."
  (let (captured-endpoint captured-per-page captured-max-pages)
    (cl-letf (((symbol-function 'shipit--api-request-paginated-async)
               (lambda (endpoint _callback &optional per-page max-pages)
                 (setq captured-endpoint endpoint
                       captured-per-page per-page
                       captured-max-pages max-pages))))
      (shipit-issue-github--fetch-reactions-async
       '(:repo "owner/repo") 42 (lambda (_r) nil))
      (should (string= "/repos/owner/repo/issues/42/reactions"
                       captured-endpoint))
      (should (equal 1 captured-max-pages))
      (should (equal 100 captured-per-page)))))

;;; Reaction Summary Tests

(require 'shipit-http)

(ert-deftest test-shipit-extract-reaction-summary-handles-plus-one-minus-one ()
  "GIVEN a GitHub reactions object parsed from JSON (keys `+1', `-1'
arrive as interned symbols whose *names* are those strings, not the
integers 1/-1)
WHEN extracting the summary alist
THEN `+1' and `-1' counts survive.  Source-level `+1' and `-1' read as
integers in Elisp, so a naive `assq' against them misses the parsed
keys — this test pins down the correct string-based lookup."
  (let* ((json (json-read-from-string
                "{\"total_count\": 10, \"+1\": 7, \"-1\": 2, \"heart\": 1, \"laugh\": 0}"))
         (summary (shipit--extract-reaction-summary json)))
    (should (equal 7 (cdr (assoc "+1" summary))))
    (should (equal 2 (cdr (assoc "-1" summary))))
    (should (equal 1 (cdr (assoc "heart" summary))))
    (should-not (assoc "laugh" summary))))

(ert-deftest test-shipit-extract-reaction-summary-nil-on-empty ()
  "Nil input or all-zero counts yield nil."
  (should-not (shipit--extract-reaction-summary nil))
  (should-not (shipit--extract-reaction-summary
               (json-read-from-string "{\"+1\": 0, \"heart\": 0}"))))

(ert-deftest test-shipit-populate-reaction-summary-stores-counts ()
  "GIVEN a real GitHub reactions object parsed from JSON
WHEN populating the summary cache
THEN each emoji type is stored with its count; zero-count types are omitted.
Input is constructed via `json-read-from-string' rather than a quoted
literal because source-level `+1'/`-1' read as integers, not as the
symbols GitHub's JSON parser actually produces."
  (clrhash shipit--reaction-summary-cache)
  (shipit--populate-reaction-summary
   42
   (json-read-from-string
    "{\"url\": \"...\", \"total_count\": 2550,
      \"+1\": 2500, \"-1\": 0, \"laugh\": 0,
      \"hooray\": 0, \"confused\": 0, \"heart\": 50,
      \"rocket\": 0, \"eyes\": 0}"))
  (let ((summary (gethash "pr-42" shipit--reaction-summary-cache)))
    (should summary)
    (should (equal 2500 (cdr (assoc "+1" summary))))
    (should (equal 50 (cdr (assoc "heart" summary))))
    (should-not (assoc "-1" summary))
    (should-not (assoc "rocket" summary))))

(ert-deftest test-shipit-populate-reaction-summary-clears-on-nil ()
  "GIVEN an existing summary in the cache
WHEN populating with nil reactions
THEN the entry is removed."
  (clrhash shipit--reaction-summary-cache)
  (puthash "pr-42" '(("+1" . 5)) shipit--reaction-summary-cache)
  (shipit--populate-reaction-summary 42 nil)
  (should-not (gethash "pr-42" shipit--reaction-summary-cache)))

(ert-deftest test-shipit-format-pr-reactions-uses-summary-counts ()
  "GIVEN a summary cache with 2500 thumbs-up and a page-1 sample of 100 users
WHEN formatting the reactions line
THEN the displayed count is 2500 (from summary, not sample length)."
  (clrhash shipit--reaction-summary-cache)
  (clrhash shipit--reaction-cache)
  (puthash "pr-42" '(("+1" . 2500)) shipit--reaction-summary-cache)
  (let ((sample (cl-loop for i from 1 to 100
                         collect `((content . "+1")
                                   (user . ((login . ,(format "user%d" i))))))))
    (puthash "pr-42" sample shipit--reaction-cache))
  (cl-letf (((symbol-function 'shipit--get-reactions-placeholder-icon)
             (lambda () "[r]"))
            ((symbol-function 'shipit--reaction-to-emoji)
             (lambda (c) (when (string= c "+1") "👍"))))
    (let ((formatted (shipit--format-pr-reactions 42)))
      (should (string-match-p "👍 2500" formatted))
      (should-not (string-match-p "👍 100" formatted)))))

(ert-deftest test-shipit-format-pr-reactions-tooltip-shows-first-n-plus-more ()
  "GIVEN a summary count of 2500 and a sample with 100 reactor logins
WHEN formatting the reactions line
THEN the tooltip lists the first 5 reactors with a \"+ N more\" suffix
reflecting the true summary count."
  (clrhash shipit--reaction-summary-cache)
  (clrhash shipit--reaction-cache)
  (puthash "pr-42" '(("+1" . 2500)) shipit--reaction-summary-cache)
  (let ((sample (cl-loop for i from 1 to 100
                         collect `((content . "+1")
                                   (user . ((login . ,(format "u%d" i))))))))
    (puthash "pr-42" sample shipit--reaction-cache))
  (cl-letf (((symbol-function 'shipit--get-reactions-placeholder-icon)
             (lambda () "[r]"))
            ((symbol-function 'shipit--reaction-to-emoji)
             (lambda (c) (when (string= c "+1") "👍"))))
    (let* ((formatted (shipit--format-pr-reactions 42))
           (pos (next-single-property-change 0 'shipit-reaction-tooltip formatted))
           (tooltip (and pos (get-text-property pos 'shipit-reaction-tooltip formatted))))
      (should tooltip)
      (should (string-match-p "u1" tooltip))
      (should (string-match-p "u5" tooltip))
      (should-not (string-match-p "u6" tooltip))
      (should (string-match-p "\\+ 2495 more" tooltip)))))

(provide 'test-shipit-issue-github)
;;; test-shipit-issue-github.el ends here
