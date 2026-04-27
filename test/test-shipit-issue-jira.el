;;; test-shipit-issue-jira.el --- Tests for Jira issue backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the Jira issue backend normalization and registration.

;;; Code:

(require 'ert)
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

(provide 'test-shipit-issue-jira)
;;; test-shipit-issue-jira.el ends here
