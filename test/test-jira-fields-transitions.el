;;; test-jira-fields-transitions.el --- Tests for Jira fields, ADF, and transitions -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for:
;; - ADF taskList/taskItem/hardBreak/codeBlock conversion
;; - Normalizer: issue-type field, assignees (plural) format
;; - Status face helper: various Jira states → correct faces
;; - Transition normalization

;;; Code:

(require 'ert)
(require 'shipit-issue-jira)
(require 'shipit-issue-backends)
(require 'shipit-issues-buffer)

;;; ADF taskList / taskItem Tests

(ert-deftest test-adf-tasklist-todo-items ()
  "GIVEN an ADF taskList with TODO taskItems
WHEN converting to text
THEN outputs unchecked checkboxes."
  (let ((node '((type . "taskList")
                (content . [((type . "taskItem")
                             (attrs . ((state . "TODO")))
                             (content . [((type . "text") (text . "Buy milk"))]))]))))
    (should (string-match-p "- \\[ \\] Buy milk"
                            (shipit-issue-jira--adf-to-text node)))))

(ert-deftest test-adf-tasklist-done-items ()
  "GIVEN an ADF taskList with DONE taskItems
WHEN converting to text
THEN outputs checked checkboxes."
  (let ((node '((type . "taskList")
                (content . [((type . "taskItem")
                             (attrs . ((state . "DONE")))
                             (content . [((type . "text") (text . "Write tests"))]))]))))
    (should (string-match-p "- \\[x\\] Write tests"
                            (shipit-issue-jira--adf-to-text node)))))

(ert-deftest test-adf-tasklist-mixed-items ()
  "GIVEN an ADF taskList with both TODO and DONE items
WHEN converting to text
THEN outputs correct checkboxes for each."
  (let ((node '((type . "taskList")
                (content . [((type . "taskItem")
                             (attrs . ((state . "DONE")))
                             (content . [((type . "text") (text . "Done task"))]))
                            ((type . "taskItem")
                             (attrs . ((state . "TODO")))
                             (content . [((type . "text") (text . "Pending task"))]))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "- \\[x\\] Done task" result))
      (should (string-match-p "- \\[ \\] Pending task" result)))))

(ert-deftest test-adf-hardbreak ()
  "GIVEN an ADF node with hardBreak
WHEN converting to text
THEN outputs a newline."
  (let ((node '((type . "paragraph")
                (content . [((type . "text") (text . "Line one"))
                            ((type . "hardBreak"))
                            ((type . "text") (text . "Line two"))]))))
    (should (string-match-p "Line one\nLine two"
                            (shipit-issue-jira--adf-to-text node)))))

(ert-deftest test-adf-codeblock ()
  "GIVEN an ADF codeBlock node
WHEN converting to text
THEN wraps content in triple-backtick fences."
  (let ((node '((type . "codeBlock")
                (content . [((type . "text") (text . "def foo():\n    pass"))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "^```" result))
      (should (string-match-p "def foo" result))
      (should (string-match-p "```$" result)))))

;;; ADF Link Tests

(ert-deftest test-adf-inline-card-link ()
  "GIVEN an ADF paragraph with an inlineCard node
WHEN converting to text
THEN outputs the URL."
  (let ((node '((type . "paragraph")
                (content . [((type . "text") (text . "Check out "))
                            ((type . "inlineCard")
                             (attrs . ((url . "https://example.com/docs"))))
                            ((type . "text") (text . " for details"))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "https://example.com/docs" result)))))

(ert-deftest test-adf-text-with-link-mark ()
  "GIVEN an ADF text node with a link mark
WHEN converting to text
THEN outputs the text with URL in markdown link format."
  (let ((node '((type . "paragraph")
                (content . [((type . "text")
                             (text . "click here")
                             (marks . [((type . "link")
                                        (attrs . ((href . "https://example.com"))))]))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "click here" result))
      (should (string-match-p "https://example.com" result)))))

(ert-deftest test-adf-text-without-marks ()
  "GIVEN an ADF text node without marks
WHEN converting to text
THEN outputs plain text unchanged."
  (let ((node '((type . "text") (text . "plain text"))))
    (should (equal "plain text" (shipit-issue-jira--adf-to-text node)))))

(ert-deftest test-adf-mention-renders-username ()
  "GIVEN an ADF paragraph containing a mention node
WHEN converting to text
THEN the display name appears without @ prefix with Jira mention property."
  (let ((node '((type . "paragraph")
                (content . [((type . "mention")
                             (attrs . ((id . "abc123")
                                       (text . "@Alice"))))
                            ((type . "text")
                             (text . "'s idea: do something"))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "Alice's idea" result))
      (should-not (string-match-p "@Alice" result))
      (should (text-property-any 0 (length result) 'shipit-jira-mention t result)))))

;;; Normalizer Tests

(ert-deftest test-jira-normalize-issue-type ()
  "GIVEN a Jira issue with issuetype field
WHEN normalizing
THEN result contains issue-type key."
  (let ((jira-data '((key . "PRJ-42")
                     (fields . ((summary . "Test issue")
                                (status . ((name . "Open")))
                                (issuetype . ((name . "Bug")))
                                (reporter . ((displayName . "Alice")))
                                (labels . [])
                                (created . "2025-01-29T10:00:00Z"))))))
    (let ((result (shipit-issue-jira--normalize-issue jira-data)))
      (should (equal "Bug" (cdr (assq 'issue-type result)))))))

(ert-deftest test-jira-normalize-issue-type-nil ()
  "GIVEN a Jira issue without issuetype field
WHEN normalizing
THEN issue-type is nil."
  (let ((jira-data '((key . "PRJ-42")
                     (fields . ((summary . "Test issue")
                                (status . ((name . "Open")))
                                (reporter . ((displayName . "Alice")))
                                (labels . [])
                                (created . "2025-01-29T10:00:00Z"))))))
    (let ((result (shipit-issue-jira--normalize-issue jira-data)))
      (should (null (cdr (assq 'issue-type result)))))))

(ert-deftest test-jira-normalize-assignees-plural ()
  "GIVEN a Jira issue with an assignee
WHEN normalizing
THEN result has assignees (plural) as a list of alists."
  (let ((jira-data '((key . "PRJ-42")
                     (fields . ((summary . "Test issue")
                                (status . ((name . "Open")))
                                (reporter . ((displayName . "Alice")))
                                (assignee . ((displayName . "Bob")))
                                (labels . [])
                                (created . "2025-01-29T10:00:00Z"))))))
    (let ((result (shipit-issue-jira--normalize-issue jira-data)))
      ;; Should have assignees (plural), not assignee (singular)
      (should (cdr (assq 'assignees result)))
      (let ((assignees (cdr (assq 'assignees result))))
        (should (listp assignees))
        (should (= 1 (length assignees)))
        (should (equal "Bob" (cdr (assq 'login (car assignees)))))))))

(ert-deftest test-jira-normalize-assignees-nil ()
  "GIVEN a Jira issue with no assignee
WHEN normalizing
THEN assignees is nil."
  (let ((jira-data '((key . "PRJ-42")
                     (fields . ((summary . "Test issue")
                                (status . ((name . "Open")))
                                (reporter . ((displayName . "Alice")))
                                (labels . [])
                                (created . "2025-01-29T10:00:00Z"))))))
    (let ((result (shipit-issue-jira--normalize-issue jira-data)))
      (should (null (cdr (assq 'assignees result)))))))

;;; Status Face Tests

(ert-deftest test-state-face-open-states ()
  "GIVEN various 'open' states
WHEN getting state face
THEN returns success face."
  (dolist (state '("open" "Open" "to do" "To Do" "new" "backlog" "reopened"))
    (should (eq 'success (shipit-issue--state-face state)))))

(ert-deftest test-state-face-in-progress-states ()
  "GIVEN various 'in progress' states
WHEN getting state face
THEN returns warning face."
  (dolist (state '("in progress" "In Progress" "in review" "testing"))
    (should (eq 'warning (shipit-issue--state-face state)))))

(ert-deftest test-state-face-closed-states ()
  "GIVEN various 'closed' states
WHEN getting state face
THEN returns error face."
  (dolist (state '("closed" "Closed" "done" "Done" "resolved" "won't do" "declined"))
    (should (eq 'error (shipit-issue--state-face state)))))

(ert-deftest test-state-face-unknown-state ()
  "GIVEN an unknown state
WHEN getting state face
THEN returns comment face."
  (should (eq 'font-lock-comment-face (shipit-issue--state-face "something weird"))))

(ert-deftest test-state-face-nil-state ()
  "GIVEN nil state
WHEN getting state face
THEN returns comment face (no error)."
  (should (eq 'font-lock-comment-face (shipit-issue--state-face nil))))

;;; Transition Normalization Tests

(ert-deftest test-jira-normalize-transitions ()
  "GIVEN a raw Jira transitions API response
WHEN normalizing transitions
THEN returns list of (id . name) alists."
  (let ((raw '((transitions . [((id . "11") (name . "To Do"))
                                ((id . "21") (name . "In Progress"))
                                ((id . "31") (name . "Done"))]))))
    (let ((result (shipit-issue-jira--normalize-transitions raw)))
      (should (= 3 (length result)))
      (should (equal "11" (cdr (assq 'id (nth 0 result)))))
      (should (equal "To Do" (cdr (assq 'name (nth 0 result)))))
      (should (equal "21" (cdr (assq 'id (nth 1 result)))))
      (should (equal "In Progress" (cdr (assq 'name (nth 1 result)))))
      (should (equal "31" (cdr (assq 'id (nth 2 result)))))
      (should (equal "Done" (cdr (assq 'name (nth 2 result))))))))

(ert-deftest test-jira-normalize-transitions-empty ()
  "GIVEN a Jira transitions response with no transitions
WHEN normalizing
THEN returns empty list."
  (let ((raw '((transitions . []))))
    (should (null (shipit-issue-jira--normalize-transitions raw)))))

;;; Backend Transition Support Tests

(ert-deftest test-backend-has-transitions-p-true ()
  "GIVEN a backend plist with :get-transitions key
WHEN checking for transition support
THEN returns non-nil."
  (let ((plist '(:name "Test" :get-transitions #'ignore :transition-status #'ignore)))
    (should (shipit-issue--backend-has-transitions-p plist))))

(ert-deftest test-backend-has-transitions-p-false ()
  "GIVEN a backend plist without :get-transitions key
WHEN checking for transition support
THEN returns nil."
  (let ((plist '(:name "Test")))
    (should-not (shipit-issue--backend-has-transitions-p plist))))

;;; Jira fields constant includes issuetype

(ert-deftest test-jira-fields-include-issuetype ()
  "GIVEN the Jira fields constant
WHEN checking its value
THEN includes issuetype."
  (should (string-match-p "issuetype" shipit-issue-jira--fields)))

;;; Jira Notification Mark-as-Read Tests

(ert-deftest test-jira-mark-notification-read-removes-activity ()
  "GIVEN a Jira notification activity with backend-id in the hash table
WHEN marking the notification as read
THEN the activity is removed from the hash table and count is updated
     (Jira backend has no :mark-notification-read, so only local removal)."
  (require 'shipit-notifications)
  (require 'shipit-issue-jira)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--last-notification-count 1)
        (shipit--mention-prs nil)
        (shipit--mention-count 0))
    ;; GIVEN a Jira activity with backend-id (as produced by real Jira notifications)
    (puthash "myorg/myrepo:issue:42"
             '((source . jira)
               (backend-id . jira)
               (reason . "assigned")
               (backend-config . (:base-url "https://jira.example.com" :project "PROJ")))
             shipit--notification-pr-activities)
    ;; Stub out modeline update and cache clear
    (cl-letf (((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      ;; WHEN marking the notification as read
      (shipit--mark-notification-read 42 "myorg/myrepo" t "issue")
      ;; THEN the activity is removed locally
      (should (= 0 (hash-table-count shipit--notification-pr-activities)))
      ;; AND the notification count is updated
      (should (= 0 shipit--last-notification-count)))))

(ert-deftest test-jira-mark-notification-read-clears-mention ()
  "GIVEN a Jira notification activity with reason 'mention' and backend-id
WHEN marking the notification as read
THEN the mention is removed from mention tracking."
  (require 'shipit-notifications)
  (require 'shipit-issue-jira)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--last-notification-count 1)
        (shipit--mention-prs '(((repo . "myorg/myrepo") (number . 42))))
        (shipit--mention-count 1))
    ;; GIVEN a Jira mention activity with backend-id
    (puthash "myorg/myrepo:issue:42"
             '((source . jira)
               (backend-id . jira)
               (reason . "mention")
               (backend-config . (:base-url "https://jira.example.com" :project "PROJ")))
             shipit--notification-pr-activities)
    (cl-letf (((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      ;; WHEN marking the notification as read
      (shipit--mark-notification-read 42 "myorg/myrepo" t "issue")
      ;; THEN the mention is removed
      (should (= 0 shipit--mention-count))
      (should (null shipit--mention-prs)))))

(ert-deftest test-jira-mark-notification-read-survives-next-poll ()
  "GIVEN a Jira notification that was marked as read
WHEN the next backend poll returns the same notification
THEN it is NOT re-inserted into the activity hash table."
  (require 'shipit-notifications)
  (require 'shipit-issue-jira)
  (let ((shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--last-notification-count 1)
        (shipit--mention-prs nil)
        (shipit--mention-count 0))
    ;; GIVEN a Jira activity that gets marked as read
    (puthash "myorg/myrepo:issue:42"
             '((repo . "myorg/myrepo")
               (number . 42)
               (type . "issue")
               (source . jira)
               (backend-id . jira)
               (reason . "assigned")
               (backend-config . (:base-url "https://jira.example.com" :project "PROJ")))
             shipit--notification-pr-activities)
    (cl-letf (((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      (shipit--mark-notification-read 42 "myorg/myrepo" t "issue")
      ;; Activity is removed
      (should (= 0 (hash-table-count shipit--notification-pr-activities)))
      ;; WHEN the next poll returns the same notification
      (shipit--merge-backend-notifications
       '(((repo . "myorg/myrepo")
          (number . 42)
          (type . "issue")
          (source . jira)
          (backend-id . jira)
          (reason . "assigned"))))
      ;; THEN it is NOT re-inserted (the local mark-as-read survives)
      (should (= 0 (hash-table-count shipit--notification-pr-activities))))))

;;; UTF-8 Encoding Tests

(ert-deftest test-jira-api-request-decodes-utf8 ()
  "GIVEN a Jira API response buffer with raw UTF-8 bytes stored as multibyte chars
WHEN decoded with set-buffer-multibyte nil then t (reinterpret as UTF-8)
THEN non-ASCII characters like Ø are correctly parsed by json-read."
  (let* ((utf8-name "Øystein")
         (json-string (format "{\"displayName\": \"%s\"}" utf8-name))
         (raw-bytes (encode-coding-string json-string 'utf-8)))
    ;; Simulate url-retrieve-synchronously: multibyte buffer with raw bytes
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
      (let ((body-start (point)))
        (insert raw-bytes)
        ;; Apply the same decode pattern used in shipit-issue-jira--api-request:
        ;; unibyte→multibyte reinterprets raw bytes as Emacs internal encoding (UTF-8)
        (set-buffer-multibyte nil)
        (set-buffer-multibyte t)
        (goto-char body-start)
        (let ((result (json-read)))
          ;; THEN the displayName should be properly decoded UTF-8
          (should (equal utf8-name (cdr (assq 'displayName result)))))))))

;;; Jira Mention Resolution Tests

(ert-deftest test-adf-mention-with-resolver-returning-github-username ()
  "GIVEN an ADF mention node with a resolver that returns @octocat
WHEN converting to text
THEN output contains @octocat."
  (let ((shipit-issue-jira--mention-resolver
         (lambda (_account-id _display-name) "@octocat"))
        (node '((type . "paragraph")
                (content . [((type . "mention")
                             (attrs . ((id . "abc123")
                                       (text . "@Alice"))))
                            ((type . "text")
                             (text . " filed this"))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "@octocat" result))
      (should (string-match-p "filed this" result)))))

(ert-deftest test-adf-mention-with-resolver-returning-nil ()
  "GIVEN an ADF mention node with a resolver that returns nil
WHEN converting to text
THEN output contains display name with shipit-jira-mention text property."
  (let ((shipit-issue-jira--mention-resolver
         (lambda (_account-id _display-name) nil))
        (node '((type . "paragraph")
                (content . [((type . "mention")
                             (attrs . ((id . "abc123")
                                       (text . "@Alice"))))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "Alice" result))
      (should-not (string-match-p "@Alice" result))
      ;; Should have Jira mention text property for overlay creation
      (should (text-property-any 0 (length result) 'shipit-jira-mention t result)))))

(ert-deftest test-adf-mention-with-nil-resolver-backward-compat ()
  "GIVEN an ADF mention node with no resolver bound (nil, default)
WHEN converting to text
THEN output contains display name with shipit-jira-mention property."
  (let ((shipit-issue-jira--mention-resolver nil)
        (node '((type . "paragraph")
                (content . [((type . "mention")
                             (attrs . ((id . "abc123")
                                       (text . "@Bob"))))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "Bob" result))
      (should-not (string-match-p "@Bob" result))
      ;; Should have Jira mention text property
      (should (text-property-any 0 (length result) 'shipit-jira-mention t result)))))

(ert-deftest test-jira-resolve-mention-cache-hit ()
  "GIVEN a cached jira-user mapping for an account ID
WHEN resolving a mention
THEN returns cached GitHub username without making API calls."
  (require 'shipit-gh-etag)
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (api-called nil)
        (config '(:base-url "https://jira.example.com")))
    ;; GIVEN cached mapping
    (puthash "jira-user:abc123" "octocat" shipit-gh-etag--persistent-cache)
    ;; Mock API to detect if called
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (&rest _) (setq api-called t) nil)))
      ;; WHEN resolving
      (let ((result (shipit-issue-jira--resolve-mention config "abc123" "Alice")))
        ;; THEN returns cached username
        (should (equal "@octocat" result))
        ;; AND no API call was made
        (should-not api-called)))))

(ert-deftest test-jira-resolve-mention-negative-cache-hit ()
  "GIVEN a negative cache entry (nil) for an account ID
WHEN resolving a mention
THEN returns nil without making API calls."
  (require 'shipit-gh-etag)
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (api-called nil)
        (config '(:base-url "https://jira.example.com")))
    ;; GIVEN negative cache entry (use :negative to distinguish from missing)
    (puthash "jira-user:abc123" :negative shipit-gh-etag--persistent-cache)
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (&rest _) (setq api-called t) nil)))
      ;; WHEN resolving
      (let ((result (shipit-issue-jira--resolve-mention config "abc123" "Alice")))
        ;; THEN returns nil
        (should-not result)
        ;; AND no API call was made
        (should-not api-called)))))

(ert-deftest test-jira-resolve-mention-noreply-email ()
  "GIVEN a Jira user with a GitHub noreply email
WHEN resolving a mention
THEN extracts username from email without GitHub search API."
  (require 'shipit-gh-etag)
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (shipit-pr-backend 'github)
        (github-search-called nil)
        (config '(:base-url "https://jira.example.com")))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 ;; Return user with noreply email
                 (when (string-match-p "/rest/api/3/user" path)
                   '((emailAddress . "12345+octocat@users.noreply.github.com")))))
              ((symbol-function 'shipit--api-request)
               (lambda (&rest _) (setq github-search-called t) nil))
              ((symbol-function 'shipit-gh-etag--save-cache) #'ignore))
      ;; WHEN resolving
      (let ((result (shipit-issue-jira--resolve-mention config "abc123" "Alice")))
        ;; THEN returns username from noreply email
        (should (equal "@octocat" result))
        ;; AND no GitHub search API was called
        (should-not github-search-called)
        ;; AND result is cached
        (should (equal "octocat"
                       (gethash "jira-user:abc123"
                                shipit-gh-etag--persistent-cache)))))))

(ert-deftest test-jira-resolve-mention-github-search-exact-match ()
  "GIVEN a Jira user with a regular email that matches exactly one GitHub user
WHEN resolving a mention
THEN returns the GitHub username."
  (require 'shipit-gh-etag)
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal))
        (config '(:base-url "https://jira.example.com")))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (when (string-match-p "/rest/api/3/user" path)
                   '((emailAddress . "alice@example.com")))))
              ((symbol-function 'shipit--api-request)
               (lambda (endpoint &optional params _callback)
                 (when (string-match-p "/search/users" endpoint)
                   '((total_count . 1)
                     (items . [((login . "alice-gh"))])))))
              ((symbol-function 'shipit-gh-etag--save-cache) #'ignore))
      ;; WHEN resolving
      (let ((result (shipit-issue-jira--resolve-mention config "abc123" "Alice")))
        ;; THEN returns matched username
        (should (equal "@alice-gh" result))
        ;; AND result is cached
        (should (equal "alice-gh"
                       (gethash "jira-user:abc123"
                                shipit-gh-etag--persistent-cache)))))))

(ert-deftest test-adf-mention-preserves-account-id-property ()
  "GIVEN an ADF mention node with account ID and no resolver
WHEN converting to text
THEN output has shipit-jira-account-id text property with the account ID."
  (let ((shipit-issue-jira--mention-resolver nil)
        (node '((type . "paragraph")
                (content . [((type . "mention")
                             (attrs . ((id . "712020:abc-def")
                                       (text . "@Kristian"))))]))))
    (let ((result (shipit-issue-jira--adf-to-text node)))
      (should (string-match-p "Kristian" result))
      (let ((pos (text-property-any 0 (length result) 'shipit-jira-mention t result)))
        (should pos)
        (should (equal "712020:abc-def"
                       (get-text-property pos 'shipit-jira-account-id result)))))))

(ert-deftest test-jira-mention-search-uses-account-id ()
  "GIVEN a Jira mention with account-id
WHEN searching for reported issues
THEN passes --author-id=<account-id> instead of --author=<display-name>."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "myorg/myrepo"))
              ((symbol-function 'shipit-issues--display-search-results)
               (lambda (_repo args) (setq captured-args args))))
      (shipit--jira-mention-search-issues "Kristian Borgen" "reporter"
                                          "712020:abc-def")
      (should captured-args)
      (should (member "--author-id=712020:abc-def" captured-args)))))

(ert-deftest test-jira-mention-search-falls-back-to-display-name ()
  "GIVEN a Jira mention without account-id
WHEN searching for assigned issues
THEN passes --assignee=<display-name>."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "myorg/myrepo"))
              ((symbol-function 'shipit-issues--display-search-results)
               (lambda (_repo args) (setq captured-args args))))
      (shipit--jira-mention-search-issues "Kristian Borgen" "assignee" nil)
      (should captured-args)
      (should (member "--assignee=Kristian Borgen" captured-args)))))

(ert-deftest test-jira-user-prefix-survives-cache-clear ()
  "GIVEN a jira-user: entry in persistent cache
WHEN clearing API cache only
THEN the jira-user: entry survives."
  (require 'shipit-gh-etag)
  (let ((shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)))
    ;; GIVEN cached jira-user entry and a regular API entry
    (puthash "jira-user:abc123" "octocat" shipit-gh-etag--persistent-cache)
    (puthash "myorg/myrepo:/repos/myorg/myrepo/pulls" '(:etag "abc" :json nil)
             shipit-gh-etag--persistent-cache)
    ;; WHEN clearing API cache
    (cl-letf (((symbol-function 'shipit-gh-etag--save-cache) #'ignore))
      (shipit-gh-etag--clear-api-cache-only))
    ;; THEN jira-user entry survives
    (should (equal "octocat"
                   (gethash "jira-user:abc123" shipit-gh-etag--persistent-cache)))
    ;; AND API entry is removed
    (should-not (gethash "myorg/myrepo:/repos/myorg/myrepo/pulls"
                         shipit-gh-etag--persistent-cache))))

;;; Parent (Epic) Normalization Tests

(ert-deftest test-jira-normalize-parent-present ()
  "GIVEN a Jira issue with a parent issue (epic)
WHEN normalizing
THEN result contains parent with key, summary, issue-type, and status."
  (let ((jira-data '((key . "PRJ-42")
                     (fields . ((summary . "Child issue")
                                (status . ((name . "Open")))
                                (issuetype . ((name . "Task")))
                                (reporter . ((displayName . "Alice")))
                                (labels . [])
                                (created . "2025-01-29T10:00:00Z")
                                (parent . ((key . "PRJ-10")
                                           (fields . ((summary . "Epic: big feature")
                                                      (issuetype . ((name . "Epic")))
                                                      (status . ((name . "In Progress"))))))))))))
    (let* ((result (shipit-issue-jira--normalize-issue jira-data))
           (parent (cdr (assq 'parent result))))
      (should parent)
      (should (equal "PRJ-10" (cdr (assq 'key parent))))
      (should (equal "Epic: big feature" (cdr (assq 'summary parent))))
      (should (equal "Epic" (cdr (assq 'issue-type parent))))
      (should (equal "In Progress" (cdr (assq 'status parent)))))))

(ert-deftest test-jira-normalize-parent-absent ()
  "GIVEN a Jira issue without a parent
WHEN normalizing
THEN no parent key in output."
  (let ((jira-data '((key . "PRJ-42")
                     (fields . ((summary . "Standalone issue")
                                (status . ((name . "Open")))
                                (reporter . ((displayName . "Alice")))
                                (labels . [])
                                (created . "2025-01-29T10:00:00Z"))))))
    (let ((result (shipit-issue-jira--normalize-issue jira-data)))
      (should-not (assq 'parent result)))))

(ert-deftest test-jira-fields-include-parent ()
  "GIVEN the Jira fields constant
WHEN checking its value
THEN includes parent."
  (should (string-match-p "parent" shipit-issue-jira--fields)))

(provide 'test-jira-fields-transitions)
;;; test-jira-fields-transitions.el ends here
