;;; test-shipit-issue-jira.el --- Tests for Jira issue backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the Jira issue backend normalization and registration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-jira)

;;; Registration Tests

(ert-deftest test-shipit-issue-jira-registered ()
  "GIVEN shipit-issue-jira is loaded
WHEN checking the backend registry
THEN 'jira is registered with name \"Jira\"."
  (should (assq 'jira shipit-issue-backends))
  (should (string= "Jira" (plist-get (cdr (assq 'jira shipit-issue-backends)) :name))))

;;; Normalization Tests

(ert-deftest test-shipit-issue-jira-normalize-basic ()
  "GIVEN a Jira API issue response
WHEN normalizing to shipit format
THEN key fields are mapped correctly."
  (let ((jira-data '((key . "PRJ-42")
                     (fields . ((summary . "Fix the login bug")
                                (status . ((name . "In Progress")))
                                (description . "Steps to reproduce...")
                                (reporter . ((displayName . "Alice")
                                             (name . "alice")))
                                (assignee . ((displayName . "Bob")
                                             (name . "bob")))
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-20T14:00:00.000+0000")
                                (labels . ("bug" "critical"))
                                (comment . ((comments . ()))))))))
    (let ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should (equal "PRJ-42" (cdr (assq 'id normalized))))
      (should (equal "PRJ-42" (cdr (assq 'number normalized))))
      (should (equal "Fix the login bug" (cdr (assq 'title normalized))))
      (should (equal "In Progress" (cdr (assq 'state normalized))))
      (should (equal "Steps to reproduce..." (cdr (assq 'body normalized))))
      (should (equal "Alice" (cdr (assq 'login (cdr (assq 'user normalized)))))))))

(ert-deftest test-shipit-issue-jira-normalize-adf-description ()
  "GIVEN a Jira v3 issue with ADF description
WHEN normalizing to shipit format
THEN description is converted to plain text."
  (let ((jira-data `((key . "PRJ-99")
                     (fields . ((summary . "ADF test")
                                (status . ((name . "Open")))
                                (description . ((type . "doc")
                                                (version . 1)
                                                (content . [((type . "paragraph")
                                                             (content . [((type . "text")
                                                                          (text . "Hello world"))]))
                                                            ((type . "paragraph")
                                                             (content . [((type . "text")
                                                                          (text . "Second paragraph"))]))])))
                                (reporter . ((displayName . "Alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (labels . ())
                                (comment . ((comments . ()))))))))
    (let ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should (string-match-p "Hello world" (cdr (assq 'body normalized))))
      (should (string-match-p "Second paragraph" (cdr (assq 'body normalized)))))))

(ert-deftest test-shipit-issue-jira-normalize-nil-assignee ()
  "GIVEN a Jira issue with no assignee
WHEN normalizing to shipit format
THEN assignee is nil without error."
  (let ((jira-data '((key . "PRJ-10")
                     (fields . ((summary . "Unassigned task")
                                (status . ((name . "Open")))
                                (description . nil)
                                (reporter . ((displayName . "Alice")
                                             (name . "alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-20T14:00:00.000+0000")
                                (labels . ())
                                (comment . ((comments . ()))))))))
    (let ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should (equal "PRJ-10" (cdr (assq 'id normalized))))
      (should-not (cdr (assq 'assignees normalized))))))

(ert-deftest test-shipit-issue-jira-normalize-labels ()
  "GIVEN a Jira issue with labels
WHEN normalizing
THEN labels are converted to the expected alist format."
  (let ((jira-data '((key . "PRJ-5")
                     (fields . ((summary . "Test")
                                (status . ((name . "Done")))
                                (description . nil)
                                (reporter . ((displayName . "Alice")
                                             (name . "alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (labels . ("enhancement" "feature"))
                                (comment . ((comments . ()))))))))
    (let* ((normalized (shipit-issue-jira--normalize-issue jira-data))
           (labels (cdr (assq 'labels normalized))))
      (should (= 2 (length labels)))
      (should (equal "enhancement" (cdr (assq 'name (car labels))))))))

(ert-deftest test-shipit-issue-jira-normalize-components ()
  "GIVEN a Jira issue with components
WHEN normalizing
THEN components are surfaced as a list of (name . X) alists, just like labels."
  (let ((jira-data '((key . "PRJ-7")
                     (fields . ((summary . "Test")
                                (status . ((name . "In Progress")))
                                (description . nil)
                                (reporter . ((displayName . "Alice")
                                             (name . "alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (components . (((name . "SDK"))
                                               ((name . "Studio"))))
                                (comment . ((comments . ()))))))))
    (let* ((normalized (shipit-issue-jira--normalize-issue jira-data))
           (components (cdr (assq 'components normalized))))
      (should (= 2 (length components)))
      (should (equal "SDK" (cdr (assq 'name (car components)))))
      (should (equal "Studio" (cdr (assq 'name (cadr components))))))))

(ert-deftest test-shipit-issue-jira-fetch-myself-caches-per-base-url ()
  "GIVEN `shipit-issue-jira--fetch-myself' called twice for the same config
WHEN the underlying API request is mocked
THEN the API is hit exactly once -- accountId is stable so the cache
is canonical for the Emacs session."
  (let ((api-calls 0)
        (shipit-issue-jira--myself-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config _path)
                 (cl-incf api-calls)
                 '((accountId . "abc-123") (displayName . "Me")))))
      (let ((config '(:base-url "https://jira.example.com")))
        (should (equal "abc-123" (shipit-issue-jira--fetch-myself config)))
        (should (equal "abc-123" (shipit-issue-jira--fetch-myself config)))
        (should (= 1 api-calls))))))

(ert-deftest test-shipit-issue-jira-fetch-myself-caches-failure ()
  "GIVEN the /myself endpoint signals an error
WHEN `shipit-issue-jira--fetch-myself' is called twice
THEN the failure is cached as nil -- we don't retry on every poll."
  (let ((api-calls 0)
        (shipit-issue-jira--myself-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config _path)
                 (cl-incf api-calls)
                 (error "boom"))))
      (let ((config '(:base-url "https://jira.example.com")))
        (should (null (shipit-issue-jira--fetch-myself config)))
        (should (null (shipit-issue-jira--fetch-myself config)))
        (should (= 1 api-calls))))))

(ert-deftest test-shipit-issue-jira-adf-collect-mentions ()
  "GIVEN an ADF tree with two mention nodes nested in paragraphs
WHEN collecting mention account-ids
THEN both ids are returned in document order."
  (let ((adf '((type . "doc")
               (content . [((type . "paragraph")
                            (content . [((type . "text") (text . "hi "))
                                        ((type . "mention")
                                         (attrs . ((id . "u1") (text . "@Alice"))))]))
                           ((type . "paragraph")
                            (content . [((type . "mention")
                                         (attrs . ((id . "u2") (text . "@Bob"))))]))]))))
    (should (equal '("u1" "u2")
                   (shipit-issue-jira--adf-collect-mentions adf)))))

(ert-deftest test-shipit-issue-jira-compute-reason-mention-in-description ()
  "GIVEN an issue whose description ADF mentions me
WHEN computing the involvement reason
THEN the reason is \"mention\"."
  (cl-letf (((symbol-function 'shipit-issue-jira--fetch-myself)
             (lambda (_config) "me-id")))
    (let* ((issue '((fields . ((description . ((type . "doc")
                                                (content . [((type . "paragraph")
                                                             (content . [((type . "mention")
                                                                          (attrs . ((id . "me-id"))))]))])))
                               (assignee . nil)
                               (comment . ((comments . ())))))))
           (reason (shipit-issue-jira--compute-involvement-reason
                    '(:base-url "x") issue)))
      (should (equal "mention" reason)))))

(ert-deftest test-shipit-issue-jira-compute-reason-i-commented ()
  "GIVEN an issue with a comment authored by me but no mention
WHEN computing the involvement reason
THEN the reason is \"comment\"."
  (cl-letf (((symbol-function 'shipit-issue-jira--fetch-myself)
             (lambda (_config) "me-id")))
    (let* ((issue '((fields . ((description . nil)
                               (assignee . nil)
                               (comment . ((comments . (((author . ((accountId . "me-id")))
                                                         (body . "noted"))))))))))
           (reason (shipit-issue-jira--compute-involvement-reason
                    '(:base-url "x") issue)))
      (should (equal "comment" reason)))))

(ert-deftest test-shipit-issue-jira-compute-reason-assigned ()
  "GIVEN an issue assigned to me with no mention or comment from me
WHEN computing the involvement reason
THEN the reason is \"assigned\"."
  (cl-letf (((symbol-function 'shipit-issue-jira--fetch-myself)
             (lambda (_config) "me-id")))
    (let* ((issue '((fields . ((description . nil)
                               (assignee . ((accountId . "me-id")
                                            (displayName . "Me")))
                               (comment . ((comments . ())))))))
           (reason (shipit-issue-jira--compute-involvement-reason
                    '(:base-url "x") issue)))
      (should (equal "assigned" reason)))))

(ert-deftest test-shipit-issue-jira-compute-reason-default-updated ()
  "GIVEN an issue with no involvement signals
WHEN computing the involvement reason
THEN the reason is \"updated\"."
  (cl-letf (((symbol-function 'shipit-issue-jira--fetch-myself)
             (lambda (_config) "me-id")))
    (let* ((issue '((fields . ((description . nil)
                               (assignee . ((accountId . "someone-else")))
                               (comment . ((comments . (((author . ((accountId . "stranger")))
                                                         (body . "not me"))))))))))
           (reason (shipit-issue-jira--compute-involvement-reason
                    '(:base-url "x") issue)))
      (should (equal "updated" reason)))))

(ert-deftest test-shipit-issue-jira-compute-reason-mention-beats-comment ()
  "GIVEN an issue where I am mentioned AND I have commented
WHEN computing the involvement reason
THEN \"mention\" wins (highest priority signal)."
  (cl-letf (((symbol-function 'shipit-issue-jira--fetch-myself)
             (lambda (_config) "me-id")))
    (let* ((issue '((fields . ((description . ((type . "doc")
                                                (content . [((type . "mention")
                                                             (attrs . ((id . "me-id"))))])))
                               (assignee . nil)
                               (comment . ((comments . (((author . ((accountId . "me-id")))
                                                         (body . "noted"))))))))))
           (reason (shipit-issue-jira--compute-involvement-reason
                    '(:base-url "x") issue)))
      (should (equal "mention" reason)))))

(ert-deftest test-shipit-issue-jira-normalize-no-components ()
  "GIVEN a Jira issue without components
WHEN normalizing
THEN the components key exists with an empty list (so callers can
    safely use `(cdr (assq 'components …))' without nil-checking)."
  (let ((jira-data '((key . "PRJ-8")
                     (fields . ((summary . "Test")
                                (status . ((name . "Done")))
                                (description . nil)
                                (reporter . ((displayName . "Alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (comment . ((comments . ()))))))))
    (let* ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should (assq 'components normalized))
      (should (null (cdr (assq 'components normalized)))))))

;;; Reference Pattern Tests

(ert-deftest test-shipit-issue-jira-reference-patterns-single-key ()
  "GIVEN a Jira config with one project key and base-url
WHEN calling :reference-patterns
THEN it returns patterns for both bare keys and browse URLs."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (patterns (funcall (plist-get backend :reference-patterns)
                            '(:base-url "https://jira.example.com"
                              :project-keys ("PRJ")))))
    (should (= 2 (length patterns)))
    ;; Bare key pattern
    (let ((pattern (nth 0 patterns)))
      (should (string-match (car pattern) "See PRJ-42 for details"))
      (should (equal "PRJ-42" (funcall (nth 2 pattern)
                                        (match-string (nth 1 pattern) "See PRJ-42 for details")))))
    ;; URL pattern
    (let ((pattern (nth 1 patterns)))
      (should (string-match (car pattern) "Duplicate of https://jira.example.com/browse/PRJ-42"))
      (should (equal "PRJ-42" (funcall (nth 2 pattern)
                                        (match-string (nth 1 pattern)
                                                      "Duplicate of https://jira.example.com/browse/PRJ-42")))))))

(ert-deftest test-shipit-issue-jira-reference-patterns-multiple-keys ()
  "GIVEN a Jira config with two project keys
WHEN calling :reference-patterns
THEN it returns bare key patterns for each plus one URL pattern."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (patterns (funcall (plist-get backend :reference-patterns)
                            '(:base-url "https://jira.example.com"
                              :project-keys ("PRJ" "TEAM")))))
    (should (= 3 (length patterns)))
    ;; First pattern matches PRJ-NNN
    (should (string-match (car (nth 0 patterns)) "PRJ-42"))
    ;; Second pattern matches TEAM-NNN
    (should (string-match (car (nth 1 patterns)) "TEAM-7"))
    ;; Third pattern matches browse URLs
    (should (string-match (car (nth 2 patterns)) "https://jira.example.com/browse/PRJ-42"))))

(ert-deftest test-shipit-issue-jira-reference-patterns-no-keys ()
  "GIVEN a Jira config without project-keys
WHEN calling :reference-patterns
THEN it returns empty list."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (patterns (funcall (plist-get backend :reference-patterns) '())))
    (should (= 0 (length patterns)))))

;;; Browse URL Tests

(ert-deftest test-shipit-issue-jira-browse-url ()
  "GIVEN a Jira config with base-url
WHEN calling :browse-url with id PRJ-42
THEN it returns the Jira browse URL."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (browse-url-fn (plist-get backend :browse-url))
         (config '(:base-url "https://jira.example.com")))
    (should (string= "https://jira.example.com/browse/PRJ-42"
                      (funcall browse-url-fn config "PRJ-42")))))

;;; ID Conversion Tests

(ert-deftest test-shipit-issue-jira-id-to-string ()
  "GIVEN the jira backend
WHEN converting issue id to string
THEN it returns the id as-is (already a string like PRJ-42)."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (id-to-string (plist-get backend :id-to-string)))
    (should (string= "PRJ-42" (funcall id-to-string "PRJ-42")))))

(ert-deftest test-shipit-issue-jira-string-to-id ()
  "GIVEN the jira backend
WHEN converting string to id
THEN it returns the string as-is."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (string-to-id (plist-get backend :string-to-id)))
    (should (string= "PRJ-42" (funcall string-to-id "PRJ-42")))))

;;; Normalize Comments Tests

(ert-deftest test-shipit-issue-jira-normalize-comments ()
  "GIVEN Jira API comments
WHEN normalizing
THEN they match the expected alist format."
  (let ((jira-comments '(((id . "10001")
                           (body . "First comment")
                           (author . ((displayName . "Alice")
                                      (name . "alice")))
                           (created . "2024-01-16T10:00:00.000+0000")
                           (updated . "2024-01-16T10:00:00.000+0000"))
                          ((id . "10002")
                           (body . "Second comment")
                           (author . ((displayName . "Bob")
                                      (name . "bob")))
                           (created . "2024-01-17T12:00:00.000+0000")
                           (updated . "2024-01-17T12:00:00.000+0000")))))
    (let ((normalized (mapcar #'shipit-issue-jira--normalize-comment jira-comments)))
      (should (= 2 (length normalized)))
      (should (equal "10001" (cdr (assq 'id (car normalized)))))
      (should (equal "First comment" (cdr (assq 'body (car normalized)))))
      (should (equal "Alice"
                      (cdr (assq 'login (cdr (assq 'user (car normalized))))))))))

(ert-deftest test-shipit-issue-jira-normalize-comment-adf-body ()
  "GIVEN a Jira v3 comment with ADF body
WHEN normalizing
THEN body is converted to plain text."
  (let ((jira-comment `((id . "10099")
                        (body . ((type . "doc")
                                 (version . 1)
                                 (content . [((type . "paragraph")
                                              (content . [((type . "text")
                                                           (text . "Review looks good"))]))])))
                        (author . ((displayName . "Alice")))
                        (created . "2024-01-16T10:00:00.000+0000")
                        (updated . "2024-01-16T10:00:00.000+0000"))))
    (let ((normalized (shipit-issue-jira--normalize-comment jira-comment)))
      (should (string-match-p "Review looks good" (cdr (assq 'body normalized)))))))

;;; Search / JQL Tests

(ert-deftest test-shipit-issue-jira-build-jql-state-open ()
  "GIVEN transient args with --state=open
WHEN building JQL
THEN JQL uses statusCategory != \"Done\" to match all non-closed statuses."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--state=open"))))
    (should (string-match-p "project in" jql))
    (should (string-match-p "statusCategory != \"Done\"" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-state-closed ()
  "GIVEN transient args with --state=closed
WHEN building JQL
THEN JQL uses statusCategory = \"Done\"."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--state=closed"))))
    (should (string-match-p "statusCategory = \"Done\"" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-author ()
  "GIVEN transient args with --author=alice
WHEN building JQL
THEN JQL contains reporter = \"alice\"."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--author=alice"))))
    (should (string-match-p "reporter = \"alice\"" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-title-search ()
  "GIVEN transient args with --title=login bug
WHEN building JQL
THEN JQL contains summary ~ \"login bug\"."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--title=login bug"))))
    (should (string-match-p "summary ~ \"login bug\"" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-multiple-filters ()
  "GIVEN transient args with multiple filters
WHEN building JQL
THEN all clauses are combined with AND."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--state=open" "--assignee=bob" "--label=bug"))))
    (should (string-match-p "project in" jql))
    (should (string-match-p "statusCategory != \"Done\"" jql))
    (should (string-match-p "assignee = \"bob\"" jql))
    (should (string-match-p "labels = \"bug\"" jql))
    (should (string-match-p " AND " jql))))

;;; Auth Tests

(ert-deftest test-shipit-issue-jira-auth-header-from-auth-source ()
  "GIVEN auth-source has credentials for the Jira host
WHEN building the auth header
THEN it returns a Basic auth header with base64(email:token)."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _args)
               (list (list :user "alice@example.com"
                           :secret (lambda () "my-api-token"))))))
    (let ((header (shipit-issue-jira--auth-header
                   '(:base-url "https://jira.example.com"))))
      (should header)
      (should (string-prefix-p "Basic " header))
      (should (string= (base64-encode-string "alice@example.com:my-api-token" t)
                        (substring header 6))))))

(ert-deftest test-shipit-issue-jira-auth-header-error-when-no-creds ()
  "GIVEN auth-source has no credentials for the host
WHEN building the auth header
THEN a user-error is signaled with the hostname."
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _args) nil)))
    (should-error (shipit-issue-jira--auth-header
                   '(:base-url "https://jira.example.com"))
                  :type 'user-error)))

(ert-deftest test-shipit-issue-jira-api-request-uses-basic-auth ()
  "GIVEN auth-source credentials exist
WHEN making a Jira API request
THEN the Authorization header uses Basic auth."
  (let ((captured-headers nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args)
                 (list (list :user "alice@example.com"
                             :secret (lambda () "my-token")))))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (setq captured-headers url-request-extra-headers)
                 (let ((buf (generate-new-buffer " *test*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\r\n\r\n{}")
                     (setq-local url-http-end-of-headers
                                 (save-excursion
                                   (goto-char (point-min))
                                   (re-search-forward "\r?\n\r?\n" nil t)
                                   (match-beginning 0)))
                     buf)))))
      (shipit-issue-jira--api-request
       '(:base-url "https://jira.example.com") "/rest/api/2/issue/PRJ-1")
      (let ((auth (cdr (assoc "Authorization" captured-headers))))
        (should auth)
        (should (string-prefix-p "Basic " auth))))))

;;; ADF Heading Tests

(ert-deftest test-shipit-issue-jira-adf-heading ()
  "GIVEN an ADF heading with level 3
WHEN converting to text
THEN it renders as '### Heading text' with trailing blank line."
  (let ((adf `((type . "doc")
               (version . 1)
               (content . [((type . "heading")
                            (attrs . ((level . 3)))
                            (content . [((type . "text")
                                         (text . "Necessary parameters:"))]))]))))
    (let ((result (shipit-issue-jira--adf-to-text adf)))
      (should (equal "\n### Necessary parameters:\n\n" result)))))

(ert-deftest test-shipit-issue-jira-adf-heading-level-1 ()
  "GIVEN an ADF heading with level 1
WHEN converting to text
THEN it renders as '# Heading text'."
  (let ((adf `((type . "doc")
               (version . 1)
               (content . [((type . "heading")
                            (attrs . ((level . 1)))
                            (content . [((type . "text")
                                         (text . "Title"))]))]))))
    (let ((result (shipit-issue-jira--adf-to-text adf)))
      (should (equal "\n# Title\n\n" result)))))

;;; ADF List Rendering Tests

(ert-deftest test-shipit-issue-jira-adf-flat-bullet-list ()
  "GIVEN an ADF bulletList with 2 items
WHEN converting to text
THEN each item is prefixed with '- '."
  (let ((adf `((type . "doc")
               (version . 1)
               (content . [((type . "bulletList")
                            (content . [((type . "listItem")
                                         (content . [((type . "paragraph")
                                                      (content . [((type . "text")
                                                                   (text . "Item 1"))]))]))
                                        ((type . "listItem")
                                         (content . [((type . "paragraph")
                                                      (content . [((type . "text")
                                                                   (text . "Item 2"))]))]))]))]))))
    (let ((result (shipit-issue-jira--adf-to-text adf)))
      (should (equal "\n- Item 1\n- Item 2\n\n" result)))))

(ert-deftest test-shipit-issue-jira-adf-nested-bullet-list ()
  "GIVEN an ADF bulletList with a nested sub-bulletList
WHEN converting to text
THEN nested items are indented with 2 spaces."
  (let ((adf `((type . "doc")
               (version . 1)
               (content . [((type . "bulletList")
                            (content . [((type . "listItem")
                                         (content . [((type . "paragraph")
                                                      (content . [((type . "text")
                                                                   (text . "Parent"))]))
                                                     ((type . "bulletList")
                                                      (content . [((type . "listItem")
                                                                   (content . [((type . "paragraph")
                                                                                (content . [((type . "text")
                                                                                             (text . "Child"))]))]))]))]))]))]))))
    (let ((result (shipit-issue-jira--adf-to-text adf)))
      (should (equal "\n- Parent\n  - Child\n\n" result)))))

(ert-deftest test-shipit-issue-jira-adf-ordered-list ()
  "GIVEN an ADF orderedList with 3 items
WHEN converting to text
THEN each item is numbered sequentially."
  (let ((adf `((type . "doc")
               (version . 1)
               (content . [((type . "orderedList")
                            (content . [((type . "listItem")
                                         (content . [((type . "paragraph")
                                                      (content . [((type . "text")
                                                                   (text . "First"))]))]))
                                        ((type . "listItem")
                                         (content . [((type . "paragraph")
                                                      (content . [((type . "text")
                                                                   (text . "Second"))]))]))
                                        ((type . "listItem")
                                         (content . [((type . "paragraph")
                                                      (content . [((type . "text")
                                                                   (text . "Third"))]))]))]))]))))
    (let ((result (shipit-issue-jira--adf-to-text adf)))
      (should (equal "\n1. First\n2. Second\n3. Third\n\n" result)))))

;;; JQL @me Placeholder Tests

(ert-deftest test-shipit-issue-jira-build-jql-assignee-at-me ()
  "GIVEN transient args with --assignee=@me
WHEN building JQL
THEN JQL contains assignee = currentUser()."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--assignee=@me"))))
    (should (string-match-p "assignee = currentUser()" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-author-at-me ()
  "GIVEN transient args with --author=@me
WHEN building JQL
THEN JQL contains reporter = currentUser()."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--author=@me"))))
    (should (string-match-p "reporter = currentUser()" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-assignee-named ()
  "GIVEN transient args with --assignee=bob (not @me)
WHEN building JQL
THEN JQL contains assignee = \"bob\" with quotes."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--assignee=bob"))))
    (should (string-match-p "assignee = \"bob\"" jql))))

;;; JQL --number= Tests

(ert-deftest test-shipit-issue-jira-build-jql-number ()
  "GIVEN transient args with --number=PRJ-42
WHEN building JQL
THEN JQL contains key = \"PRJ-42\"."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--number=PRJ-42"))))
    (should (string-match-p "key = \"PRJ-42\"" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-author-id ()
  "GIVEN transient args with --author-id=712020:abc
WHEN building JQL
THEN JQL contains reporter = \"712020:abc\" (account ID, not display name)."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--author-id=712020:abc"))))
    (should (string-match-p "reporter = \"712020:abc\"" jql))))

(ert-deftest test-shipit-issue-jira-build-jql-assignee-id ()
  "GIVEN transient args with --assignee-id=712020:def
WHEN building JQL
THEN JQL contains assignee = \"712020:def\" (account ID, not display name)."
  (let ((jql (shipit-issue-jira--build-jql-from-args
              '(:project-keys ("PRJ"))
              '("--assignee-id=712020:def"))))
    (should (string-match-p "assignee = \"712020:def\"" jql))))

;;; Changelog Normalization Tests

(ert-deftest test-shipit-issue-jira-normalize-changelog ()
  "GIVEN a Jira issue response with changelog.histories
WHEN normalizing the changelog
THEN each history entry is mapped to normalized format."
  (let ((raw-changelog
         '((histories . [((id . "12345")
                          (created . "2025-02-10T10:30:00.000+0000")
                          (author . ((displayName . "Alice")))
                          (items . [((field . "status")
                                     (fromString . "To Do")
                                     (toString . "In Progress"))
                                    ((field . "assignee")
                                     (fromString . "Bob")
                                     (toString . "Alice"))]))]))))
    (let ((normalized (shipit-issue-jira--normalize-changelog raw-changelog)))
      (should (= 1 (length normalized)))
      (let ((entry (car normalized)))
        (should (equal "12345" (cdr (assq 'id entry))))
        (should (equal "2025-02-10T10:30:00.000+0000" (cdr (assq 'created_at entry))))
        (should (equal "Alice" (cdr (assq 'login (cdr (assq 'user entry))))))
        (let ((items (cdr (assq 'items entry))))
          (should (= 2 (length items)))
          (should (equal "status" (cdr (assq 'field (car items)))))
          (should (equal "To Do" (cdr (assq 'from (car items)))))
          (should (equal "In Progress" (cdr (assq 'to (car items))))))))))

(ert-deftest test-shipit-issue-jira-normalize-changelog-empty ()
  "GIVEN a Jira issue with no changelog histories
WHEN normalizing the changelog
THEN returns empty list."
  (let ((raw-changelog '((histories . []))))
    (should (null (shipit-issue-jira--normalize-changelog raw-changelog)))))

(ert-deftest test-shipit-issue-jira-normalize-changelog-nil ()
  "GIVEN nil changelog data
WHEN normalizing
THEN returns empty list."
  (should (null (shipit-issue-jira--normalize-changelog nil))))

(ert-deftest test-shipit-issue-jira-normalize-issue-includes-changelog ()
  "GIVEN a Jira issue response with changelog
WHEN normalizing the full issue
THEN the changelog key is present in the result."
  (let ((jira-data `((key . "PRJ-42")
                     (fields . ((summary . "Test")
                                (status . ((name . "Open")))
                                (description . nil)
                                (reporter . ((displayName . "Alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (labels . ())
                                (comment . ((comments . ())))))
                     (changelog . ((histories . [((id . "100")
                                                  (created . "2024-01-16T10:00:00.000+0000")
                                                  (author . ((displayName . "Bob")))
                                                  (items . [((field . "status")
                                                              (fromString . "Open")
                                                              (toString . "In Progress"))]))]))))))
    (let ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should (assq 'changelog normalized))
      (should (= 1 (length (cdr (assq 'changelog normalized))))))))

;;; Issue Links Normalization Tests

(ert-deftest test-shipit-issue-jira-normalize-issuelinks ()
  "GIVEN a Jira issue with issuelinks
WHEN normalizing the links
THEN each link is mapped with type, direction, key, summary, and status."
  (let ((raw-links
         `[((type . ((name . "Blocks")
                     (outward . "blocks")
                     (inward . "is blocked by")))
            (outwardIssue . ((key . "PRJ-43")
                             (fields . ((summary . "Related task")
                                        (status . ((name . "Open"))))))))
           ((type . ((name . "Blocks")
                     (outward . "blocks")
                     (inward . "is blocked by")))
            (inwardIssue . ((key . "PRJ-40")
                            (fields . ((summary . "Dependency")
                                       (status . ((name . "Done"))))))))]))
    (let ((normalized (shipit-issue-jira--normalize-issuelinks raw-links)))
      (should (= 2 (length normalized)))
      ;; First link: outward "blocks"
      (let ((first (car normalized)))
        (should (equal "blocks" (cdr (assq 'type first))))
        (should (equal "outward" (cdr (assq 'direction first))))
        (should (equal "PRJ-43" (cdr (assq 'key first))))
        (should (equal "Related task" (cdr (assq 'summary first))))
        (should (equal "Open" (cdr (assq 'status first)))))
      ;; Second link: inward "is blocked by"
      (let ((second (cadr normalized)))
        (should (equal "is blocked by" (cdr (assq 'type second))))
        (should (equal "inward" (cdr (assq 'direction second))))
        (should (equal "PRJ-40" (cdr (assq 'key second))))
        (should (equal "Dependency" (cdr (assq 'summary second))))
        (should (equal "Done" (cdr (assq 'status second))))))))

(ert-deftest test-shipit-issue-jira-normalize-issuelinks-includes-assignee ()
  "GIVEN a Jira issuelink with assignee in fields
WHEN normalizing the link
THEN the normalized link includes assignee display name."
  (let* ((raw-links
          `[((type . ((name . "Blocks")
                      (outward . "blocks")
                      (inward . "is blocked by")))
             (outwardIssue . ((key . "PRJ-43")
                              (fields . ((summary . "Related task")
                                         (status . ((name . "Open")))
                                         (assignee . ((displayName . "Bob"))))))))])
         (normalized (shipit-issue-jira--normalize-issuelinks raw-links)))
    (should (= 1 (length normalized)))
    (should (equal "Bob" (cdr (assq 'assignee (car normalized)))))))

(ert-deftest test-shipit-issue-jira-normalize-issuelinks-nil-assignee ()
  "GIVEN a Jira issuelink with no assignee
WHEN normalizing the link
THEN assignee is nil."
  (let* ((raw-links
          `[((type . ((name . "Relates")
                      (outward . "relates to")
                      (inward . "relates to")))
             (outwardIssue . ((key . "PRJ-44")
                              (fields . ((summary . "Other task")
                                         (status . ((name . "Open")))
                                         (assignee . nil))))))])
         (normalized (shipit-issue-jira--normalize-issuelinks raw-links)))
    (should (= 1 (length normalized)))
    (should (null (cdr (assq 'assignee (car normalized)))))))

(ert-deftest test-shipit-issue-jira-normalize-issuelinks-empty ()
  "GIVEN no issue links
WHEN normalizing
THEN returns empty list."
  (should (null (shipit-issue-jira--normalize-issuelinks [])))
  (should (null (shipit-issue-jira--normalize-issuelinks nil))))

(ert-deftest test-shipit-issue-jira-normalize-issue-includes-issuelinks ()
  "GIVEN a Jira issue response with issuelinks field
WHEN normalizing the full issue
THEN the issuelinks key is present in the result."
  (let ((jira-data `((key . "PRJ-42")
                     (fields . ((summary . "Test")
                                (status . ((name . "Open")))
                                (description . nil)
                                (reporter . ((displayName . "Alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (labels . ())
                                (issuelinks . [((type . ((name . "Relates")
                                                         (outward . "relates to")
                                                         (inward . "is related to")))
                                                (outwardIssue . ((key . "PRJ-50")
                                                                  (fields . ((summary . "Other")
                                                                             (status . ((name . "Open"))))))))])
                                (comment . ((comments . ()))))))))
    (let ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should (assq 'issuelinks normalized))
      (should (= 1 (length (cdr (assq 'issuelinks normalized))))))))

;;; API v2 Path Tests

(ert-deftest test-shipit-issue-jira-create-issue-uses-api-v2 ()
  "GIVEN the jira backend
WHEN calling create-issue
THEN it POSTs to /rest/api/2/issue (not v3)."
  (let ((post-path nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config path _data)
                 (setq post-path path)
                 '((key . "PRJ-1") (id . "10001"))))
              ((symbol-function 'shipit-issue-jira--fetch-issue)
               (lambda (_config _id)
                 '((id . "PRJ-1") (number . "PRJ-1") (title . "T")))))
      (shipit-issue-jira--create-issue
       '(:base-url "https://jira.example.com" :project-keys ("PRJ"))
       "Title" "Body")
      (should (string= "/rest/api/2/issue" post-path)))))

(ert-deftest test-shipit-issue-jira-create-issue-extended-uses-api-v2 ()
  "GIVEN the jira backend
WHEN calling create-issue-extended
THEN it POSTs to /rest/api/2/issue (not v3)."
  (let ((post-path nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config path _data)
                 (setq post-path path)
                 '((key . "PRJ-2") (id . "10002"))))
              ((symbol-function 'shipit-issue-jira--fetch-issue)
               (lambda (_config _id)
                 '((id . "PRJ-2") (number . "PRJ-2") (title . "T")))))
      (shipit-issue-jira--create-issue-extended
       '(:base-url "https://jira.example.com" :project-keys ("PRJ"))
       '((title . "Title") (body . "Body")))
      (should (string= "/rest/api/2/issue" post-path)))))

;;; POST Error Checking Tests

(ert-deftest test-shipit-issue-jira-post-signals-error-on-400 ()
  "GIVEN a POST request that returns HTTP 400 with error JSON
WHEN the response is processed
THEN an error is signaled with the error message."
  (cl-letf (((symbol-function 'shipit-issue-jira--auth-header)
             (lambda (_config) nil))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-400*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 400 Bad Request\r\n\r\n")
                   (insert "{\"errorMessages\":[\"Field 'description' cannot be set\"],\"errors\":{}}")
                   (setq-local url-http-end-of-headers
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward "\r?\n\r?\n" nil t)
                                 (match-beginning 0)))
                   (setq-local url-http-response-status 400))
                 buf))))
    (should-error
     (shipit-issue-jira--api-request-post
      '(:base-url "https://jira.example.com")
      "/rest/api/2/issue"
      '((fields . ((summary . "Test"))))))))

(ert-deftest test-shipit-issue-jira-post-returns-json-on-201 ()
  "GIVEN a POST request that returns HTTP 201 with valid JSON
WHEN the response is processed
THEN the parsed JSON is returned."
  (cl-letf (((symbol-function 'shipit-issue-jira--auth-header)
             (lambda (_config) nil))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (let ((buf (generate-new-buffer " *test-201*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 201 Created\r\n\r\n")
                   (insert "{\"id\":\"10001\",\"key\":\"PRJ-42\",\"self\":\"https://jira.example.com/rest/api/2/issue/10001\"}")
                   (setq-local url-http-end-of-headers
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward "\r?\n\r?\n" nil t)
                                 (match-beginning 0)))
                   (setq-local url-http-response-status 201))
                 buf))))
    (let ((result (shipit-issue-jira--api-request-post
                   '(:base-url "https://jira.example.com")
                   "/rest/api/2/issue"
                   '((fields . ((summary . "Test")))))))
      (should result)
      (should (equal "PRJ-42" (cdr (assq 'key result)))))))

(ert-deftest test-shipit-issue-jira-search-page-uses-jql-endpoint ()
  "GIVEN a Jira search-page request with no page token
WHEN building the API path
THEN it uses /rest/api/3/search/jql with jql and maxResults params."
  (require 'shipit-issue-jira)
  (let ((captured-path nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (setq captured-path path)
                 '((issues . []) (isLast . t)))))
      (shipit-issue-jira--search-page
       '(:base-url "https://jira.example.com")
       "project = PROJ" 100 "key,summary" nil)
      (should (string-match-p "/rest/api/3/search/jql\\?" captured-path))
      (should (string-match-p "maxResults=100" captured-path))
      (should-not (string-match-p "startAt=" captured-path)))))

(ert-deftest test-shipit-issue-jira-search-page-passes-next-page-token ()
  "GIVEN a Jira search-page request with a nextPageToken
WHEN building the API path
THEN it includes nextPageToken query param."
  (require 'shipit-issue-jira)
  (let ((captured-path nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (setq captured-path path)
                 '((issues . []) (isLast . t)))))
      (shipit-issue-jira--search-page
       '(:base-url "https://jira.example.com")
       "project = PROJ" 100 "key,summary" "abc123token")
      (should (string-match-p "nextPageToken=abc123token" captured-path)))))

(ert-deftest test-shipit-issue-jira-fetch-issue-asks-for-the-priority ()
  "GIVEN a Jira issue with a priority
WHEN it is fetched for its page
THEN the request asks for the priority, and the issue carries its name."
  (require 'shipit-issue-jira)
  (let ((path nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config p)
                 (setq path p)
                 '((key . "PRJ-1")
                   (fields . ((summary . "Prioritised")
                              (status . ((name . "Open")))
                              (priority . ((name . "High")))))))))
      (let ((issue (shipit-issue-jira--fetch-issue
                    '(:base-url "https://jira.example.com") "PRJ-1")))
        (should (string-match-p "fields=[^&]*priority" path))
        (should (equal "High" (cdr (assq 'priority issue))))))))

(ert-deftest test-shipit-issue-jira-search-paginates-with-next-page-token ()
  "GIVEN Jira v3 search/jql returns nextPageToken for cursor pagination
WHEN searching with limit=150
THEN pagination follows nextPageToken chain until isLast is true."
  (require 'shipit-issue-jira)
  (let* ((page-calls 0)
         (page1 (mapcar (lambda (i)
                          `((key . ,(format "PROJ-%d" i))
                            (fields . ((summary . ,(format "Issue %d" i))
                                       (status . ((name . "Open")))
                                       (issuetype . ((name . "Task")))))))
                        (number-sequence 1 100)))
         (page2 (mapcar (lambda (i)
                          `((key . ,(format "PROJ-%d" i))
                            (fields . ((summary . ,(format "Issue %d" i))
                                       (status . ((name . "Open")))
                                       (issuetype . ((name . "Task")))))))
                        (number-sequence 101 130))))
    (cl-letf (((symbol-function 'shipit-issue-jira--search-page)
               (lambda (_config _jql _page-size _fields token)
                 (setq page-calls (1+ page-calls))
                 (cond
                  ((null token)
                   `((issues . ,(vconcat page1))
                     (nextPageToken . "token-page2")
                     (isLast . :json-false)))
                  ((equal token "token-page2")
                   `((issues . ,(vconcat page2))
                     (isLast . t)))
                  (t `((issues . []) (isLast . t))))))
              ((symbol-function 'shipit-issue-jira--resolve-mention)
               (lambda (_config _aid _dn) nil)))
      (let* ((config '(:base-url "https://jira.example.com"
                        :project-keys ("PROJ")))
             (result (shipit-issue-jira--search
                      config '("--state=open" "--limit=150"))))
        (should (= page-calls 2))
        (should (= (length result) 130))))))

;;; Fetch Children Tests

(ert-deftest test-shipit-issue-jira-fetch-children-builds-jql ()
  "GIVEN an epic key PRJ-10
WHEN fetching children
THEN JQL is 'parent = PRJ-10 ORDER BY status ASC, key ASC'."
  (let ((captured-jql nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--search-page)
               (lambda (_config jql _page-size _fields _token)
                 (setq captured-jql jql)
                 '((issues . []) (isLast . t)))))
      (shipit-issue-jira--fetch-children
       '(:base-url "https://jira.example.com") "PRJ-10")
      (should (string-match-p "parent = PRJ-10" captured-jql))
      (should (string-match-p "ORDER BY" captured-jql)))))

(ert-deftest test-shipit-issue-jira-fetch-children-normalizes ()
  "GIVEN Jira returns 2 child issues
WHEN fetching children
THEN results are normalized with key, summary, status, assignee, issue-type."
  (cl-letf (((symbol-function 'shipit-issue-jira--search-page)
             (lambda (_config _jql _page-size _fields _token)
               `((issues . [((key . "PRJ-11")
                              (fields . ((summary . "Child 1")
                                         (status . ((name . "Open")))
                                         (assignee . ((displayName . "Alice")))
                                         (issuetype . ((name . "Story"))))))
                             ((key . "PRJ-12")
                              (fields . ((summary . "Child 2")
                                         (status . ((name . "Done")))
                                         (assignee . nil)
                                         (issuetype . ((name . "Bug"))))))])
                 (isLast . t)))))
    (let ((result (shipit-issue-jira--fetch-children
                   '(:base-url "https://jira.example.com") "PRJ-10")))
      (should (= 2 (length result)))
      (let ((first (car result)))
        (should (equal "PRJ-11" (cdr (assq 'key first))))
        (should (equal "Child 1" (cdr (assq 'summary first))))
        (should (equal "Open" (cdr (assq 'status first))))
        (should (equal "Alice" (cdr (assq 'assignee first))))
        (should (equal "Story" (cdr (assq 'issue-type first)))))
      (let ((second (cadr result)))
        (should (null (cdr (assq 'assignee second))))))))

;;; Fetch Labels Tests

(ert-deftest test-shipit-issue-jira-fetch-labels-flattens-values ()
  "GIVEN the Jira label endpoint returns a single page of label strings
WHEN fetching labels
THEN a flat list of those strings is returned."
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config _path)
               '((maxResults . 1000) (startAt . 0) (total . 2)
                 (isLast . t) (values . ["bug" "critical"])))))
    (let ((result (shipit-issue-jira--fetch-labels
                   '(:base-url "https://jira.example.com"))))
      (should (equal result '("bug" "critical"))))))

(ert-deftest test-shipit-issue-jira-fetch-labels-follows-pagination ()
  "GIVEN the Jira label endpoint paginates across two pages
WHEN fetching labels
THEN every page is requested by advancing startAt and all label
strings are concatenated in order."
  (let ((calls 0))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (setq calls (1+ calls))
                 (cond
                  ((string-match-p "startAt=0" path)
                   '((maxResults . 2) (startAt . 0) (total . 3)
                     (isLast . :json-false) (values . ["alpha" "beta"])))
                  ((string-match-p "startAt=2" path)
                   '((maxResults . 2) (startAt . 2) (total . 3)
                     (isLast . t) (values . ["gamma"])))
                  (t '((maxResults . 2) (isLast . t) (values . [])))))))
      (let ((result (shipit-issue-jira--fetch-labels
                     '(:base-url "https://jira.example.com"))))
        (should (= calls 2))
        (should (equal result '("alpha" "beta" "gamma")))))))

(ert-deftest test-shipit-issue-jira-update-labels-puts-vector ()
  "GIVEN a list of label strings
WHEN updating an issue's labels
THEN a PUT is sent to the issue endpoint with labels as a JSON array."
  (let ((captured-path nil) (captured-data nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-put)
               (lambda (_config path data)
                 (setq captured-path path captured-data data)
                 nil)))
      (shipit-issue-jira--update-labels
       '(:base-url "https://jira.example.com") "PRJ-7" '("backend" "urgent"))
      (should (equal captured-path "/rest/api/3/issue/PRJ-7"))
      (let ((labels (cdr (assq 'labels (cdr (assq 'fields captured-data))))))
        (should (vectorp labels))
        (should (equal labels ["backend" "urgent"]))))))

(ert-deftest test-shipit-issue-jira-update-labels-clears-with-empty-vector ()
  "GIVEN an empty label list
WHEN updating an issue's labels
THEN labels are sent as an empty JSON array (clearing them), not null or {}."
  (let ((captured-data nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-put)
               (lambda (_config _path data) (setq captured-data data) nil)))
      (shipit-issue-jira--update-labels
       '(:base-url "https://jira.example.com") "PRJ-7" nil)
      (let ((labels (cdr (assq 'labels (cdr (assq 'fields captured-data))))))
        (should (vectorp labels))
        (should (equal labels []))
        (should (equal (json-encode labels) "[]"))))))

(ert-deftest test-shipit-issue-jira-registers-label-ops ()
  "GIVEN the jira backend
WHEN inspecting the registry
THEN it exposes :fetch-labels and :update-labels operations."
  (let ((backend (cdr (assq 'jira shipit-issue-backends))))
    (should (functionp (plist-get backend :fetch-labels)))
    (should (functionp (plist-get backend :update-labels)))))

;;; Fetch Assignable Users Tests

(ert-deftest test-shipit-issue-jira-fetch-assignable-users-follows-pagination ()
  "GIVEN a project with more assignable users than fit on one page
WHEN fetching assignable users
THEN paging continues past the first page and every user is returned.
A single page silently truncates large sites, dropping whoever sorts
last -- the reason a name can be missing from the assignee prompt."
  (let* ((size shipit-issue-jira--assignable-page-size)
         (full-page
          (vconcat (mapcar (lambda (i)
                             `((displayName . ,(format "User %03d" i))
                               (accountId . ,(format "id-%d" i))))
                           (number-sequence 1 size))))
         (paths nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (push path paths)
                 (if (string-match-p "startAt=0" path)
                     full-page
                   [((displayName . "Zoe Zulu") (accountId . "id-zoe"))]))))
      (let ((result (shipit-issue-jira--fetch-assignable-users
                     '(:base-url "https://jira.example.com"
                       :project-keys ("PRJ")))))
        (should (= (length paths) 2))
        (should (string-match-p (format "startAt=%d" size) (car paths)))
        (should (member "Zoe Zulu" result))
        (should (= (length result) (1+ size)))))))

(ert-deftest test-shipit-issue-jira-fetch-assignable-users-covers-every-project ()
  "GIVEN a config naming two projects
WHEN fetching assignable users
THEN both are queried and their users merge into one sorted list.
Only asking the first project hides anyone assignable solely in the
others."
  (let ((paths nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (push path paths)
                 (cond
                  ((string-match-p "project=ONE" path)
                   [((displayName . "Ann") (accountId . "id-ann"))
                    ((displayName . "Bob") (accountId . "id-bob"))])
                  ((string-match-p "project=TWO" path)
                   [((displayName . "Bob") (accountId . "id-bob"))
                    ((displayName . "Cid") (accountId . "id-cid"))])
                  (t [])))))
      (let ((result (shipit-issue-jira--fetch-assignable-users
                     '(:base-url "https://jira.example.com"
                       :project-keys ("ONE" "TWO")))))
        (should (= (length paths) 2))
        (should (equal result '("Ann" "Bob" "Cid")))))))

(ert-deftest test-shipit-issue-jira-fetch-assignable-users-caches-account-ids ()
  "GIVEN users gathered from more than one project
WHEN fetching assignable users
THEN each returned name maps to its account ID in the cache."
  (let ((shipit-issue-jira--assignable-users-cache nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (if (string-match-p "project=ONE" path)
                     [((displayName . "Ann") (accountId . "id-ann"))]
                   [((displayName . "Cid") (accountId . "id-cid"))]))))
      (shipit-issue-jira--fetch-assignable-users
       '(:base-url "https://jira.example.com" :project-keys ("ONE" "TWO")))
      (should (equal "id-ann"
                     (cdr (assoc "Ann" shipit-issue-jira--assignable-users-cache))))
      (should (equal "id-cid"
                     (cdr (assoc "Cid" shipit-issue-jira--assignable-users-cache)))))))

(ert-deftest test-shipit-issue-jira-update-assignee-rejects-unknown-name ()
  "GIVEN a display name the assignable-user cache does not know
WHEN updating an issue's assignee
THEN it errors rather than sending the PUT that would clear the field."
  (let ((shipit-issue-jira--assignable-users-cache '(("Ann" . "id-ann")))
        (called nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-put)
               (lambda (&rest _) (setq called t) nil)))
      (should-error (shipit-issue-jira--update-assignee
                     '(:base-url "https://jira.example.com") "PRJ-7" "Nobody"))
      (should-not called))))

(ert-deftest test-shipit-issue-jira-update-assignee-unassigns-with-nil ()
  "GIVEN no assignee name
WHEN updating an issue's assignee
THEN the assignee field is sent as JSON null, clearing it."
  (let ((captured-data nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-put)
               (lambda (_config _path data) (setq captured-data data) nil)))
      (shipit-issue-jira--update-assignee
       '(:base-url "https://jira.example.com") "PRJ-7" nil)
      (should (eq :null (cdr (assq 'assignee
                                   (cdr (assq 'fields captured-data)))))))))

;;; Issue Link Creation Tests

(defconst test-shipit-issue-jira--link-types
  '((issueLinkTypes . [((id . "10000") (name . "Blocks")
                        (inward . "is blocked by") (outward . "blocks"))
                       ((id . "10003") (name . "Relates")
                        (inward . "relates to") (outward . "relates to"))]))
  "A GET /issueLinkType answer with one asymmetric and one symmetric type.")

(ert-deftest test-shipit-issue-jira-link-relations-offer-both-phrases ()
  "GIVEN Jira's link types
WHEN listing the relations a user may pick
THEN each direction of an asymmetric type is its own phrase, while a
symmetric type like Relates appears once."
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config _path) test-shipit-issue-jira--link-types)))
    (let ((relations (shipit-issue-jira--fetch-link-relations
                      '(:base-url "https://jira.example.com"))))
      (should (equal (mapcar #'car relations)
                     '("blocks" "is blocked by" "relates to"))))))

(ert-deftest test-shipit-issue-jira-link-issues-outward-phrase ()
  "GIVEN the phrase \"blocks\" chosen on PRJ-1 towards PRJ-2
WHEN creating the link
THEN PRJ-1 is the inward issue and PRJ-2 the outward one, which is how
Jira spells \"PRJ-1 blocks PRJ-2\"."
  (let ((captured nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config _path) test-shipit-issue-jira--link-types))
              ((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config path data)
                 (setq captured (cons path data)) nil)))
      (let* ((config '(:base-url "https://jira.example.com"))
             (relation (cdr (assoc "blocks"
                                   (shipit-issue-jira--fetch-link-relations config)))))
        (shipit-issue-jira--link-issues config "PRJ-1" relation "PRJ-2")
        (should (equal "/rest/api/3/issueLink" (car captured)))
        (let ((body (cdr captured)))
          (should (equal "Blocks" (cdr (assq 'name (cdr (assq 'type body))))))
          (should (equal "PRJ-1" (cdr (assq 'key (cdr (assq 'inwardIssue body))))))
          (should (equal "PRJ-2" (cdr (assq 'key (cdr (assq 'outwardIssue body)))))))))))

(ert-deftest test-shipit-issue-jira-link-issues-inward-phrase ()
  "GIVEN the phrase \"is blocked by\" chosen on PRJ-1 towards PRJ-2
WHEN creating the link
THEN the issues swap: PRJ-2 blocks PRJ-1, so PRJ-2 is inward."
  (let ((captured nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config _path) test-shipit-issue-jira--link-types))
              ((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config _path data) (setq captured data) nil)))
      (let* ((config '(:base-url "https://jira.example.com"))
             (relation (cdr (assoc "is blocked by"
                                   (shipit-issue-jira--fetch-link-relations config)))))
        (shipit-issue-jira--link-issues config "PRJ-1" relation "PRJ-2")
        (should (equal "PRJ-2" (cdr (assq 'key (cdr (assq 'inwardIssue captured))))))
        (should (equal "PRJ-1" (cdr (assq 'key (cdr (assq 'outwardIssue captured))))))))))

(ert-deftest test-shipit-issue-jira-registers-link-ops ()
  "GIVEN the jira backend
WHEN inspecting the registry
THEN it exposes the two operations behind Link work item."
  (let ((backend (cdr (assq 'jira shipit-issue-backends))))
    (should (functionp (plist-get backend :fetch-link-relations)))
    (should (functionp (plist-get backend :link-issues)))))

;;; Remote Link Tests

(ert-deftest test-shipit-issue-jira-link-remote-url-posts-idempotently ()
  "GIVEN a pull request's URL and title
WHEN attaching it to an issue as a remote link
THEN the POST goes to the issue's remotelink endpoint with the URL as
globalId, so attaching the same URL twice updates rather than
duplicates."
  (let ((captured nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config path data) (setq captured (cons path data)) nil)))
      (shipit-issue-jira--link-remote-url
       '(:base-url "https://jira.example.com") "PRJ-7"
       "https://github.com/o/r/pull/12" "#12: Fix sensor")
      (should (equal "/rest/api/3/issue/PRJ-7/remotelink" (car captured)))
      (let ((body (cdr captured)))
        (should (equal "https://github.com/o/r/pull/12" (cdr (assq 'globalId body))))
        (should (equal "https://github.com/o/r/pull/12"
                       (cdr (assq 'url (cdr (assq 'object body))))))
        (should (equal "#12: Fix sensor"
                       (cdr (assq 'title (cdr (assq 'object body))))))))))

(ert-deftest test-shipit-issue-jira-fetch-remote-urls-returns-url-and-title ()
  "GIVEN an issue with remote links
WHEN fetching them
THEN each comes back as (URL . TITLE)."
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config path)
               (should (equal "/rest/api/3/issue/PRJ-7/remotelink" path))
               [((id . 1) (globalId . "g1")
                 (object . ((url . "https://github.com/o/r/pull/12")
                            (title . "#12: Fix sensor"))))
                ((id . 2)
                 (object . ((url . "https://wiki.example.com/page")
                            (title . "Design page"))))])))
    (should (equal (shipit-issue-jira--fetch-remote-urls
                    '(:base-url "https://jira.example.com") "PRJ-7")
                   '(("https://github.com/o/r/pull/12" . "#12: Fix sensor")
                     ("https://wiki.example.com/page" . "Design page"))))))

(ert-deftest test-shipit-issue-jira-registers-remote-link-ops ()
  "GIVEN the jira backend
WHEN inspecting the registry
THEN it exposes the remote link operations."
  (let ((backend (cdr (assq 'jira shipit-issue-backends))))
    (should (functionp (plist-get backend :link-remote-url)))
    (should (functionp (plist-get backend :fetch-remote-urls)))))

;;; Request Method Tests

(defun test-shipit-issue-jira--respond (status body)
  "Return a `url-retrieve-synchronously' stand-in answering STATUS with BODY.
Records the request method and body it saw in the buffer-local
`test-method' and `test-body' of the response buffer."
  (lambda (_url &rest _args)
    (let ((buf (generate-new-buffer " *test-response*"))
          (method url-request-method)
          (data url-request-data))
      (with-current-buffer buf
        (insert (format "HTTP/1.1 %d X\r\n\r\n" status))
        (insert body)
        (setq-local url-http-end-of-headers
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "\r?\n\r?\n" nil t)
                      (match-beginning 0)))
        (setq-local url-http-response-status status)
        (setq-local test-method method)
        (setq-local test-body data))
      (setq test-shipit-issue-jira--last-request (cons method data))
      buf)))

(defvar test-shipit-issue-jira--last-request nil
  "The (METHOD . BODY) of the last request a test stand-in answered.")

(ert-deftest test-shipit-issue-jira-put-sends-put-with-body ()
  "GIVEN data to update
WHEN sending it with the PUT helper
THEN the request goes out as PUT with the data as its JSON body."
  (cl-letf (((symbol-function 'shipit-issue-jira--auth-header) (lambda (_c) nil))
            ((symbol-function 'url-retrieve-synchronously)
             (test-shipit-issue-jira--respond 204 "")))
    (shipit-issue-jira--api-request-put
     '(:base-url "https://jira.example.com") "/rest/api/3/issue/PRJ-1"
     '((fields . ((summary . "New")))))
    (should (equal "PUT" (car test-shipit-issue-jira--last-request)))
    (should (string-match-p "\"summary\":\"New\""
                            (cdr test-shipit-issue-jira--last-request)))))

(ert-deftest test-shipit-issue-jira-delete-sends-delete-without-body ()
  "GIVEN a resource to remove
WHEN sending the DELETE helper
THEN the request goes out as DELETE with no body, and an empty 204
answer returns nil without error."
  (cl-letf (((symbol-function 'shipit-issue-jira--auth-header) (lambda (_c) nil))
            ((symbol-function 'url-retrieve-synchronously)
             (test-shipit-issue-jira--respond 204 "")))
    (should-not (shipit-issue-jira--api-request-delete
                 '(:base-url "https://jira.example.com")
                 "/rest/api/3/issue/PRJ-1/remotelink?globalId=x"))
    (should (equal "DELETE" (car test-shipit-issue-jira--last-request)))
    (should-not (cdr test-shipit-issue-jira--last-request))))

(ert-deftest test-shipit-issue-jira-delete-signals-error-on-404 ()
  "GIVEN a DELETE answered with 404
WHEN the response is processed
THEN an error is signaled, as for the other methods."
  (cl-letf (((symbol-function 'shipit-issue-jira--auth-header) (lambda (_c) nil))
            ((symbol-function 'url-retrieve-synchronously)
             (test-shipit-issue-jira--respond
              404 "{\"errorMessages\":[\"No link\"],\"errors\":{}}")))
    (should-error (shipit-issue-jira--api-request-delete
                   '(:base-url "https://jira.example.com")
                   "/rest/api/3/issue/PRJ-1/remotelink?globalId=x"))))

(ert-deftest test-shipit-issue-jira-unlink-remote-url-deletes-by-global-id ()
  "GIVEN a URL attached to an issue
WHEN detaching it
THEN the DELETE names the issue's remotelink endpoint and the URL as
globalId, escaped for the query string."
  (let ((captured nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-delete)
               (lambda (_config path) (setq captured path) nil)))
      (shipit-issue-jira--unlink-remote-url
       '(:base-url "https://jira.example.com") "PRJ-7"
       "https://github.com/o/r/pull/12")
      (should (equal (concat "/rest/api/3/issue/PRJ-7/remotelink?globalId="
                             (url-hexify-string "https://github.com/o/r/pull/12"))
                     captured)))))

(ert-deftest test-shipit-issue-jira-registers-unlink-remote-url ()
  "GIVEN the jira backend
WHEN inspecting the registry
THEN it exposes `:unlink-remote-url'."
  (should (functionp (plist-get (cdr (assq 'jira shipit-issue-backends))
                                :unlink-remote-url))))

(ert-deftest test-shipit-issue-jira-update-title-puts-summary ()
  "GIVEN a new title
WHEN updating an issue's title
THEN a PUT sets fields.summary on the issue endpoint."
  (let ((captured-path nil) (captured-data nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-put)
               (lambda (_config path data)
                 (setq captured-path path captured-data data)
                 nil)))
      (shipit-issue-jira--update-title
       '(:base-url "https://jira.example.com") "PRJ-7" "Improved summary")
      (should (equal captured-path "/rest/api/3/issue/PRJ-7"))
      (should (equal (cdr (assq 'summary (cdr (assq 'fields captured-data))))
                     "Improved summary")))))

(ert-deftest test-shipit-issue-jira-registers-update-title ()
  "GIVEN the jira backend
WHEN inspecting the registry
THEN it exposes an :update-title operation."
  (should (functionp (plist-get (cdr (assq 'jira shipit-issue-backends))
                                :update-title))))

(ert-deftest test-shipit-issue-jira-normalize-priority-and-status-category ()
  "GIVEN a Jira issue with priority and status containing statusCategory
WHEN normalizing to shipit format
THEN priority name and status-category key are extracted."
  (let ((jira-data '((key . "PRJ-50")
                     (fields . ((summary . "Test priority")
                                (status . ((name . "In Progress")
                                           (id . "3")
                                           (statusCategory . ((key . "indeterminate")
                                                              (name . "In Progress")))))
                                (priority . ((name . "High")
                                             (id . "2")))
                                (issuetype . ((name . "Bug")))
                                (description . nil)
                                (reporter . ((displayName . "Alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (labels . ())
                                (comment . ((comments . ()))))))))
    (let ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should (equal "High" (cdr (assq 'priority normalized))))
      (should (equal "indeterminate" (cdr (assq 'status-category normalized)))))))

(ert-deftest test-shipit-issue-jira-normalize-nil-priority ()
  "GIVEN a Jira issue with no priority
WHEN normalizing
THEN priority is nil."
  (let ((jira-data '((key . "PRJ-51")
                     (fields . ((summary . "No priority")
                                (status . ((name . "Open")
                                           (statusCategory . ((key . "new")))))
                                (description . nil)
                                (reporter . ((displayName . "Alice")))
                                (assignee . nil)
                                (created . "2024-01-15T10:30:00.000+0000")
                                (updated . "2024-01-15T10:30:00.000+0000")
                                (labels . ())
                                (comment . ((comments . ()))))))))
    (let ((normalized (shipit-issue-jira--normalize-issue jira-data)))
      (should-not (cdr (assq 'priority normalized)))
      (should (equal "new" (cdr (assq 'status-category normalized)))))))

;;; Icon and Face Mapping Tests

(ert-deftest test-shipit-issue-jira-issue-type-icon-bug ()
  "GIVEN issue type 'Bug'
WHEN getting the icon mapping
THEN returns bug octicon with red color."
  (let ((result (shipit-issue-jira--issue-type-icon "Bug")))
    (should (equal "bug" (car result)))
    (should (equal "#cb2431" (cdr result)))))

(ert-deftest test-shipit-issue-jira-issue-type-icon-story ()
  "GIVEN issue type 'Story'
WHEN getting the icon mapping
THEN returns bookmark octicon with green color."
  (let ((result (shipit-issue-jira--issue-type-icon "Story")))
    (should (equal "bookmark" (car result)))
    (should (equal "#28a745" (cdr result)))))

(ert-deftest test-shipit-issue-jira-issue-type-icon-task ()
  "GIVEN issue type 'Task'
WHEN getting the icon mapping
THEN returns tasklist octicon with blue color."
  (let ((result (shipit-issue-jira--issue-type-icon "Task")))
    (should (equal "tasklist" (car result)))
    (should (equal "#0366d6" (cdr result)))))

(ert-deftest test-shipit-issue-jira-issue-type-icon-subtask ()
  "GIVEN issue type 'Sub-task'
WHEN getting the icon mapping
THEN returns tasklist octicon with purple color."
  (let ((result (shipit-issue-jira--issue-type-icon "Sub-task")))
    (should (equal "tasklist" (car result)))
    (should (equal "#6f42c1" (cdr result)))))

(ert-deftest test-shipit-issue-jira-issue-type-icon-epic ()
  "GIVEN issue type 'Epic'
WHEN getting the icon mapping
THEN returns flame octicon with orange color."
  (let ((result (shipit-issue-jira--issue-type-icon "Epic")))
    (should (equal "flame" (car result)))
    (should (equal "#fd7e14" (cdr result)))))

(ert-deftest test-shipit-issue-jira-issue-type-icon-unknown ()
  "GIVEN an unknown issue type
WHEN getting the icon mapping
THEN returns issue-opened octicon with gray color."
  (let ((result (shipit-issue-jira--issue-type-icon "Whatever")))
    (should (equal "issue-opened" (car result)))
    (should (equal "#999999" (cdr result)))))

(ert-deftest test-shipit-issue-jira-issue-type-icon-nil ()
  "GIVEN nil issue type
WHEN getting the icon mapping
THEN returns fallback icon without error."
  (let ((result (shipit-issue-jira--issue-type-icon nil)))
    (should (equal "issue-opened" (car result)))))

(ert-deftest test-shipit-issue-jira-priority-icon-highest ()
  "GIVEN priority 'Highest'
WHEN getting the priority icon
THEN returns arrow-up with red color."
  (let ((result (shipit-issue-jira--priority-icon "Highest")))
    (should (equal "arrow-up" (car result)))
    (should (equal "#cb2431" (cdr result)))))

(ert-deftest test-shipit-issue-jira-priority-icon-critical ()
  "GIVEN priority 'Critical'
WHEN getting the priority icon
THEN returns arrow-up with red color."
  (let ((result (shipit-issue-jira--priority-icon "Critical")))
    (should (equal "arrow-up" (car result)))
    (should (equal "#cb2431" (cdr result)))))

(ert-deftest test-shipit-issue-jira-priority-icon-high ()
  "GIVEN priority 'High'
WHEN getting the priority icon
THEN returns chevron-up with orange color."
  (let ((result (shipit-issue-jira--priority-icon "High")))
    (should (equal "chevron-up" (car result)))
    (should (equal "#fd7e14" (cdr result)))))

(ert-deftest test-shipit-issue-jira-priority-icon-medium ()
  "GIVEN priority 'Medium'
WHEN getting the priority icon
THEN returns dash with orange color."
  (let ((result (shipit-issue-jira--priority-icon "Medium")))
    (should (equal "dash" (car result)))
    (should (equal "#fd7e14" (cdr result)))))

(ert-deftest test-shipit-issue-jira-priority-icon-low ()
  "GIVEN priority 'Low'
WHEN getting the priority icon
THEN returns chevron-down with blue color."
  (let ((result (shipit-issue-jira--priority-icon "Low")))
    (should (equal "chevron-down" (car result)))
    (should (equal "#0366d6" (cdr result)))))

(ert-deftest test-shipit-issue-jira-priority-icon-lowest ()
  "GIVEN priority 'Lowest'
WHEN getting the priority icon
THEN returns arrow-down with gray color."
  (let ((result (shipit-issue-jira--priority-icon "Lowest")))
    (should (equal "arrow-down" (car result)))
    (should (equal "#999999" (cdr result)))))

(ert-deftest test-shipit-issue-jira-priority-icon-unknown ()
  "GIVEN an unknown priority
WHEN getting the priority icon
THEN returns nil."
  (should-not (shipit-issue-jira--priority-icon "Whatever")))

(ert-deftest test-shipit-issue-jira-priority-icon-nil ()
  "GIVEN nil priority
WHEN getting the priority icon
THEN returns nil without error."
  (should-not (shipit-issue-jira--priority-icon nil)))

(ert-deftest test-shipit-issue-jira-status-category-face-new ()
  "GIVEN status category 'new'
WHEN getting the status face
THEN returns magit-dimmed."
  (should (eq 'magit-dimmed
              (shipit-issue-jira--status-category-face "new"))))

(ert-deftest test-shipit-issue-jira-status-category-face-indeterminate ()
  "GIVEN status category 'indeterminate'
WHEN getting the status face
THEN returns magit-branch-remote."
  (should (eq 'magit-branch-remote
              (shipit-issue-jira--status-category-face "indeterminate"))))

(ert-deftest test-shipit-issue-jira-status-category-face-done ()
  "GIVEN status category 'done'
WHEN getting the status face
THEN returns success."
  (should (eq 'success
              (shipit-issue-jira--status-category-face "done"))))

(ert-deftest test-shipit-issue-jira-status-category-face-unknown ()
  "GIVEN an unknown status category
WHEN getting the status face
THEN returns default."
  (should (eq 'default
              (shipit-issue-jira--status-category-face "something"))))

(ert-deftest test-shipit-issue-jira-render-icon-no-svglib ()
  "GIVEN svglib icons disabled
WHEN rendering an icon
THEN returns empty string."
  (let ((shipit-use-svglib-icons nil))
    (should (equal "" (shipit-issue-jira--render-icon '("bug" . "#cb2431"))))))

(ert-deftest test-shipit-issue-jira-render-icon-nil-data ()
  "GIVEN nil icon data
WHEN rendering an icon
THEN returns empty string."
  (let ((shipit-use-svglib-icons nil))
    (should (equal "" (shipit-issue-jira--render-icon nil)))))

(ert-deftest test-shipit-issue-jira-render-issue-type-icon ()
  "GIVEN a type name and svglib disabled
WHEN calling the convenience render wrapper
THEN returns empty string (no svg)."
  (let ((shipit-use-svglib-icons nil))
    (should (equal "" (shipit-issue-jira--render-issue-type-icon "Bug")))))

(ert-deftest test-shipit-issue-jira-render-priority-icon ()
  "GIVEN a priority name and svglib disabled
WHEN calling the convenience render wrapper
THEN returns empty string for known priority, nil for unknown."
  (let ((shipit-use-svglib-icons nil))
    (should (equal "" (shipit-issue-jira--render-priority-icon "High")))
    (should-not (shipit-issue-jira--render-priority-icon "Whatever"))))

(ert-deftest test-shipit-issue-jira-dashboard-columns-default ()
  "GIVEN default configuration
WHEN reading dashboard columns defcustom
THEN returns issue-type-icon, priority-icon, work, status."
  (should (equal '(issue-type-icon priority-icon work status)
                 shipit-jira-dashboard-columns)))

(ert-deftest test-shipit-issue-jira-backend-has-icon-keys ()
  "GIVEN the Jira backend is registered
WHEN checking the backend plist
THEN icon render and face keys are present."
  (let ((backend (cdr (assq 'jira shipit-issue-backends))))
    (should (plist-get backend :issue-type-icon-render))
    (should (plist-get backend :priority-icon-render))
    (should (plist-get backend :status-category-face))
    (should (plist-get backend :dashboard-columns))))

(defun test-shipit-issue-jira--make-comment (author created body)
  "Build a Jira comment alist for tests."
  (list (cons 'author (list (cons 'displayName author)))
        (cons 'created created)
        (cons 'body body)))

(defun test-shipit-issue-jira--make-history (author created items)
  "Build a Jira changelog history alist for tests."
  (list (cons 'author (list (cons 'displayName author)))
        (cons 'created created)
        (cons 'items items)))

(defun test-shipit-issue-jira--make-issue (comments histories)
  "Wrap COMMENTS and HISTORIES into a Jira-style raw issue alist."
  (list (cons 'fields (list (cons 'comment (list (cons 'comments comments)))))
        (cons 'changelog (list (cons 'histories histories)))))

(ert-deftest test-shipit-issue-jira-extract-last-activity-prefers-newer-comment ()
  "GIVEN an issue with both a comment and a changelog entry
WHEN the comment is newer
THEN the comment wins."
  (let* ((comments (list (test-shipit-issue-jira--make-comment
                          "Bob" "2026-04-27T15:00:00.000+0000" "looks good")))
         (histories (list (test-shipit-issue-jira--make-history
                           "Alice" "2026-04-27T14:00:00.000+0000"
                           (list (list (cons 'field "status")
                                       (cons 'fromString "In Progress")
                                       (cons 'toString "Done"))))))
         (raw (test-shipit-issue-jira--make-issue comments histories))
         (last (shipit-issue-jira--extract-last-activity raw)))
    (should (eq 'comment (plist-get last :type)))
    (should (equal "Bob" (plist-get last :author)))
    (should (equal "looks good" (plist-get last :summary)))))

(ert-deftest test-shipit-issue-jira-extract-last-activity-prefers-newer-history ()
  "GIVEN an issue with both a comment and a changelog entry
WHEN the changelog entry is newer
THEN the changelog entry wins and the summary is field: from -> to."
  (let* ((comments (list (test-shipit-issue-jira--make-comment
                          "Bob" "2026-04-27T14:00:00.000+0000" "earlier comment")))
         (histories (list (test-shipit-issue-jira--make-history
                           "Alice" "2026-04-27T15:00:00.000+0000"
                           (list (list (cons 'field "status")
                                       (cons 'fromString "In Progress")
                                       (cons 'toString "Done"))))))
         (raw (test-shipit-issue-jira--make-issue comments histories))
         (last (shipit-issue-jira--extract-last-activity raw)))
    (should (eq 'change (plist-get last :type)))
    (should (equal "Alice" (plist-get last :author)))
    (should (string-match-p "status: In Progress -> Done"
                            (plist-get last :summary)))))

(ert-deftest test-shipit-issue-jira-extract-last-activity-empty ()
  "GIVEN an issue with no comments and no changelog
WHEN extracting last activity
THEN it returns nil."
  (let ((raw (test-shipit-issue-jira--make-issue nil nil)))
    (should (null (shipit-issue-jira--extract-last-activity raw)))))

(ert-deftest test-shipit-issue-jira-summarize-comment-body-truncates ()
  "GIVEN a long comment body
WHEN summarizing
THEN whitespace collapses and the result fits in 80 chars (with ...)."
  (let* ((body (make-string 200 ?x))
         (out (shipit-issue-jira--summarize-comment-body body)))
    (should (<= (length out) 80))
    (should (string-suffix-p "..." out))))

(ert-deftest test-shipit-issue-jira-summarize-history-items-joins ()
  "GIVEN multiple history items
WHEN summarizing
THEN they are comma-joined as field: from -> to."
  (let ((items '(((field . "status")
                  (fromString . "Open")
                  (toString . "Done"))
                 ((field . "assignee")
                  (fromString . "")
                  (toString . "Bob")))))
    (let ((out (shipit-issue-jira--summarize-history-items items)))
      (should (string-match-p "status: Open -> Done" out))
      (should (string-match-p "assignee: <empty> -> Bob" out))
      (should (string-match-p ", " out)))))

;;; Async API Request Tests

(defvar test-shipit-jira--response-buffer nil
  "Response buffer handed to the callback by the stubbed `url-retrieve'.")

(defconst test-shipit-jira--config '(:base-url "https://example.atlassian.net")
  "Minimal Jira backend config for the async request tests.")

(defun test-shipit-jira--stub-url-retrieve (status body)
  "Return a `url-retrieve' stub delivering BODY under STATUS.
Mimics url.el's response buffer: headers, a blank line, then BODY,
with `url-http-end-of-headers' on the blank line's newline."
  (lambda (_url callback &rest _)
    (let ((buffer (generate-new-buffer " *test-jira-response*")))
      (setq test-shipit-jira--response-buffer buffer)
      (with-current-buffer buffer
        (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
        (setq-local url-http-end-of-headers (1- (point)))
        (insert body)
        (funcall callback status))
      buffer)))

(defmacro test-shipit-jira--with-response (status body &rest form)
  "Evaluate FORM with Jira answering BODY under STATUS.
Stubs the HTTP boundary and auth-source lookup only."
  (declare (indent 2))
  `(progn
     (setq test-shipit-jira--response-buffer nil)
     (cl-letf (((symbol-function 'url-retrieve)
                (test-shipit-jira--stub-url-retrieve ,status ,body))
               ((symbol-function 'shipit-issue-jira--auth-header)
                (lambda (_config) "Basic dGVzdDp0ZXN0")))
       ,@form)))

(ert-deftest test-shipit-issue-jira-async-delivers-parsed-body ()
  "GIVEN Jira answers a request with a JSON body
WHEN the async request completes
THEN the callback receives the parsed payload
AND the response buffer is released."
  (let ((received 'nothing))
    (test-shipit-jira--with-response nil "{\"key\": \"PRJ-42\"}"
      (shipit-issue-jira--api-request-async
       test-shipit-jira--config "/rest/api/3/issue/PRJ-42"
       (lambda (data) (setq received data))))
    (should (equal "PRJ-42" (cdr (assq 'key received))))
    (should-not (buffer-live-p test-shipit-jira--response-buffer))))

(ert-deftest test-shipit-issue-jira-async-releases-buffer-when-request-fails ()
  "GIVEN Jira fails the request
WHEN the async request completes
THEN the callback is told nothing came back
AND the response buffer is released rather than leaked."
  (let ((received 'nothing))
    (test-shipit-jira--with-response '(:error (error http 500))
        "{\"errorMessages\": [\"boom\"]}"
      (shipit-issue-jira--api-request-async
       test-shipit-jira--config "/rest/api/3/issue/PRJ-42"
       (lambda (data) (setq received data))))
    (should (null received))
    (should-not (buffer-live-p test-shipit-jira--response-buffer))))

(ert-deftest test-shipit-issue-jira-async-tolerates-absent-callback ()
  "GIVEN a caller that fires a request without wanting the result
WHEN the request fails
THEN nothing is signalled
AND the response buffer is still released."
  (test-shipit-jira--with-response '(:error (error http 500)) "{}"
    (shipit-issue-jira--api-request-async
     test-shipit-jira--config "/rest/api/3/myself" nil))
  (should-not (buffer-live-p test-shipit-jira--response-buffer)))

(ert-deftest test-shipit-issue-jira-async-parses-utf8-body ()
  "GIVEN a Jira response whose body carries raw UTF-8 bytes
WHEN the async request completes
THEN the callback receives correctly decoded text."
  (let ((received 'nothing))
    (test-shipit-jira--with-response nil
        (encode-coding-string "{\"displayName\": \"Øystein\"}" 'utf-8)
      (shipit-issue-jira--api-request-async
       test-shipit-jira--config "/rest/api/3/myself"
       (lambda (data) (setq received data))))
    (should (equal "Øystein" (cdr (assq 'displayName received))))))

(ert-deftest test-shipit-issue-jira-async-inhibits-cookies ()
  "GIVEN Jira sets cookies on its API responses
WHEN an async request is made
THEN url.el is asked to inhibit them, keeping the global cookie jar clean."
  (let ((call-args nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest args) (setq call-args args) nil))
              ((symbol-function 'shipit-issue-jira--auth-header)
               (lambda (_config) nil)))
      (shipit-issue-jira--api-request-async
       test-shipit-jira--config "/rest/api/3/myself" #'ignore))
    ;; url-retrieve is (URL CALLBACK &optional CBARGS SILENT INHIBIT-COOKIES)
    (should (nth 4 call-args))))

(ert-deftest test-jira-normalize-keeps-the-assignee-account-id ()
  "GIVEN a Jira issue with an assignee
WHEN it is normalized
THEN the assignee keeps its account id next to its display name, so a
     caller can tell whose issue it is without matching on a name."
  (let ((issue (shipit-issue-jira--normalize-issue
                '((key . "PROJ-1")
                  (fields . ((summary . "A task")
                             (status . ((name . "Open") (id . "1")))
                             (assignee . ((displayName . "Ronny Randen")
                                          (accountId . "5f2a")))))))))
    (let ((assignee (car (cdr (assq 'assignees issue)))))
      (should (equal "Ronny Randen" (cdr (assq 'login assignee))))
      (should (equal "5f2a" (cdr (assq 'id assignee)))))))

(ert-deftest test-jira-normalize-without-an-assignee ()
  "GIVEN a Jira issue nobody is assigned
WHEN it is normalized
THEN it carries no assignee at all, rather than an empty one."
  (let ((issue (shipit-issue-jira--normalize-issue
                '((key . "PROJ-1")
                  (fields . ((summary . "A task")
                             (status . ((name . "Open") (id . "1")))))))))
    (should (null (cdr (assq 'assignees issue))))))

;;; Images in descriptions and comments

(defconst test-shipit-issue-jira--attachments
  '(((id . "10001") (filename . "image-1.png") (mimeType . "image/png")
     (content . "https://jira.example.com/rest/api/3/attachment/content/10001"))
    ((id . "10002") (filename . "report.pdf") (mimeType . "application/pdf")
     (content . "https://jira.example.com/rest/api/3/attachment/content/10002")))
  "Two attachments of an issue: an image and a document.")

(defconst test-shipit-issue-jira--image-markdown
  "![image-1.png](https://jira.example.com/rest/api/3/attachment/content/10001?redirect=false)"
  "The markdown the attached image becomes.")

(defun test-shipit-issue-jira--media-doc (type attrs)
  "Return an ADF document holding one media node of TYPE with ATTRS.
A block image sits in a mediaSingle, as Jira writes one.  An inline
file, a mediaInline, sits in a paragraph."
  (if (equal type "mediaInline")
      `((type . "doc") (version . 1)
        (content . [((type . "paragraph")
                     (content . [((type . "mediaInline") (attrs . ,attrs))]))]))
    `((type . "doc") (version . 1)
      (content . [((type . "mediaSingle") (attrs . ((layout . "center")))
                   (content . [((type . "media") (attrs . ,attrs))]))]))))

(defun test-shipit-issue-jira--attached-image-doc ()
  "Return an ADF document holding the attached image, as Jira stores it."
  (test-shipit-issue-jira--media-doc
   "media" '((id . "a1b2c3") (type . "file") (collection . "jira-1") (alt . "image-1.png"))))

(ert-deftest test-shipit-issue-jira-adf-image-attached-to-the-issue ()
  "GIVEN a description holding an image that names an attachment of the issue
WHEN it is converted to text
THEN it is a markdown image of that attachment.  The URL asks Jira not
to redirect, so the login header goes to Jira and not on to its media
host."
  (let ((shipit-issue-jira--attachments test-shipit-issue-jira--attachments))
    (should (equal (concat test-shipit-issue-jira--image-markdown "\n")
                   (shipit-issue-jira--adf-to-text (test-shipit-issue-jira--attached-image-doc))))))

(ert-deftest test-shipit-issue-jira-adf-image-hosted-elsewhere ()
  "GIVEN an image that the text links to on another site
WHEN it is converted to text
THEN it keeps its own URL."
  (should (equal "![diagram](https://example.com/diagram.png)\n"
                 (shipit-issue-jira--adf-to-text
                  (test-shipit-issue-jira--media-doc
                   "media" '((type . "external") (url . "https://example.com/diagram.png")
                             (alt . "diagram")))))))

(ert-deftest test-shipit-issue-jira-adf-attached-file-is-a-link ()
  "GIVEN a paragraph holding an attached file that is no image
WHEN it is converted to text
THEN the file is a link, not an image the renderer cannot draw."
  (let ((shipit-issue-jira--attachments test-shipit-issue-jira--attachments))
    (should (equal "[report.pdf](https://jira.example.com/rest/api/3/attachment/content/10002?redirect=false)\n"
                   (shipit-issue-jira--adf-to-text
                    (test-shipit-issue-jira--media-doc
                     "mediaInline" '((id . "d4e5") (type . "file") (alt . "report.pdf"))))))))

(ert-deftest test-shipit-issue-jira-adf-image-without-its-attachment ()
  "GIVEN an image naming a file the issue has no attachment for
WHEN it is converted to text
THEN its name stands in its place, where the image vanished before."
  (let ((shipit-issue-jira--attachments test-shipit-issue-jira--attachments))
    (should (equal "[image: gone.png]\n"
                   (shipit-issue-jira--adf-to-text
                    (test-shipit-issue-jira--media-doc
                     "media" '((type . "file") (alt . "gone.png"))))))))

(ert-deftest test-shipit-issue-jira-fetch-issue-shows-attached-images ()
  "GIVEN an issue whose description holds an image of one of its attachments
WHEN the issue is fetched
THEN its attachments are asked for with it, and the description holds
the image."
  (let ((asked nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (push path asked)
                 `((key . "PRJ-7")
                   (fields . ((summary . "With a screenshot")
                              (status . ((name . "Open")))
                              (description . ,(test-shipit-issue-jira--attached-image-doc))
                              (attachment . ,(vconcat test-shipit-issue-jira--attachments))))))))
      (let ((issue (shipit-issue-jira--fetch-issue '(:base-url "https://jira.example.com") "PRJ-7")))
        (should (string-match-p "attachment" (car (last asked))))
        (should (equal test-shipit-issue-jira--image-markdown (cdr (assq 'body issue))))))))

(defun test-shipit-issue-jira--answer (path)
  "Return what the Jira API answers for PATH.
The issue's attachments when PATH asks for them, else its one comment,
which holds the attached image."
  (if (string-match-p "fields=attachment" path)
      `((fields . ((attachment . ,(vconcat test-shipit-issue-jira--attachments)))))
    `((comments . [((id . "501")
                    (author . ((displayName . "Alice")))
                    (body . ,(test-shipit-issue-jira--attached-image-doc))
                    (created . "2026-09-16T10:00:00.000+0000")
                    (updated . "2026-09-16T10:00:00.000+0000"))]))))

(ert-deftest test-shipit-issue-jira-fetch-comments-shows-attached-images ()
  "GIVEN a comment holding an image attached to its issue
WHEN the comments are fetched
THEN the issue's attachments are asked for too, since the comments
endpoint gives none, and the comment holds the image."
  (let ((asked nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (push path asked)
                 (test-shipit-issue-jira--answer path))))
      (let ((comments (shipit-issue-jira--fetch-comments
                       '(:base-url "https://jira.example.com") "PRJ-7")))
        (should (cl-some (lambda (path) (string-match-p "fields=attachment" path)) asked))
        (should (equal test-shipit-issue-jira--image-markdown
                       (cdr (assq 'body (car comments)))))))))

(ert-deftest test-shipit-issue-jira-fetch-comments-async-shows-attached-images ()
  "GIVEN a comment holding an image attached to its issue
WHEN the comments are fetched without blocking
THEN the comment handed on holds the image."
  (let ((got nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-async)
               (lambda (_config path callback)
                 (funcall callback (test-shipit-issue-jira--answer path)))))
      (shipit-issue-jira--fetch-comments-async
       '(:base-url "https://jira.example.com") "PRJ-7"
       (lambda (comments) (setq got comments)))
      (should (equal test-shipit-issue-jira--image-markdown
                     (cdr (assq 'body (car got))))))))

(defmacro test-shipit-issue-jira--with-site (&rest body)
  "Run BODY with one Jira site configured, and a login for it."
  `(let ((shipit-issue-repo-backends
          '(("owner/repo" :backend jira :base-url "https://jira.example.com"))))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest _) (list (list :user "me@example.com" :secret "api-token")))))
       ,@body)))

(ert-deftest test-shipit-issue-jira-image-login-only-for-its-own-site ()
  "GIVEN a configured Jira site
WHEN the login headers of an image download are asked for
THEN an image on that site gets Jira's login, and an image anywhere
else gets none."
  (test-shipit-issue-jira--with-site
   (should (equal `(("Authorization" . ,(concat "Basic " (base64-encode-string
                                                         "me@example.com:api-token" t))))
                  (shipit-issue-jira--image-auth-headers
                   "https://jira.example.com/rest/api/3/attachment/content/10001?redirect=false")))
   (should-not (shipit-issue-jira--image-auth-headers "https://example.com/diagram.png"))))

(ert-deftest test-shipit-issue-jira-image-download-logs-in-to-jira ()
  "GIVEN a configured Jira site
WHEN an image attached there is downloaded
THEN the download sends Jira's login header."
  (require 'shipit-http)
  (let ((shipit-pr-backend-config nil)
        (shipit-github-token "gh-token")
        (captured nil))
    (test-shipit-issue-jira--with-site
     (cl-letf (((symbol-function 'url-retrieve-synchronously)
                (lambda (&rest _) (setq captured url-request-extra-headers) nil)))
       (shipit--download-and-cache-image
        (format "https://jira.example.com/rest/api/3/attachment/content/%d?redirect=false"
                (random 1000000000)))
       (should (string-prefix-p "Basic " (or (cdr (assoc "Authorization" captured)) "")))))))

(ert-deftest test-shipit-issue-jira-media-url-follows-the-attachment-redirect ()
  "GIVEN a configured Jira site, and a video attached on it
WHEN the URL a player can fetch it from is asked for
THEN the attachment is asked for with Jira's login and without following
its redirect, and the media URL it redirects to comes back.  A URL on
another site gets nothing, and Jira is not asked for it."
  (test-shipit-issue-jira--with-site
   (let ((asked nil))
     (cl-letf (((symbol-function 'url-retrieve-synchronously)
                (lambda (url &rest _)
                  (push (list url url-max-redirections
                              (assoc "Authorization" url-request-extra-headers))
                        asked)
                  (let ((buffer (generate-new-buffer " *jira-redirect*")))
                    (with-current-buffer buffer
                      (insert "HTTP/1.1 303 See Other\r\n"
                              "Location: https://api.media.atlassian.com/file/f1/binary?token=t&dl=true\r\n"
                              "\r\n"))
                    buffer))))
       (should (equal "https://api.media.atlassian.com/file/f1/binary?token=t&dl=true"
                      (shipit-issue-jira--media-url
                       "https://jira.example.com/rest/api/3/attachment/content/31733?redirect=false")))
       (should (equal "https://jira.example.com/rest/api/3/attachment/content/31733"
                      (car (car asked))))
       (should (equal 0 (nth 1 (car asked))))
       (should (nth 2 (car asked)))
       (should-not (shipit-issue-jira--media-url
                    "https://example.com/rest/api/3/attachment/content/1"))
       (should (= 1 (length asked)))))))

(ert-deftest test-shipit-issue-jira-hands-out-media-urls ()
  "GIVEN the Jira backend loaded
THEN it tells shipit how to turn its attachment URLs into media URLs."
  (should (memq #'shipit-issue-jira--media-url shipit-media-url-functions)))

(provide 'test-shipit-issue-jira)
;;; test-shipit-issue-jira.el ends here
