;;; test-shipit-issue-create.el --- Tests for issue creation -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for issue creation via the pluggable backend system.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-jira)
(require 'shipit-issues)
(require 'shipit-editor nil t)

;;; GitHub Create Issue Tests

(ert-deftest test-shipit-issue-github-create-issue-calls-api ()
  "GIVEN the github backend
WHEN calling :create-issue with title and body
THEN it POSTs to /repos/{repo}/issues with correct payload."
  (let ((called-endpoint nil)
        (called-data nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &optional _method)
                 (setq called-endpoint endpoint
                       called-data data)
                 '((number . 99) (title . "New Issue") (state . "open")))))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (create-fn (plist-get backend :create-issue))
             (config '(:repo "owner/repo"))
             (result (funcall create-fn config "New Issue" "Issue body")))
        (should (string= "/repos/owner/repo/issues" called-endpoint))
        (should (equal "New Issue" (cdr (assq 'title called-data))))
        (should (equal "Issue body" (cdr (assq 'body called-data))))
        (should (= 99 (cdr (assq 'number result))))))))

(ert-deftest test-shipit-issue-github-create-issue-returns-normalized ()
  "GIVEN the github backend
WHEN :create-issue returns a response
THEN the response is already in normalized format (GitHub is canonical)."
  (cl-letf (((symbol-function 'shipit--api-request-post)
             (lambda (_endpoint _data &optional _method)
               '((number . 42) (title . "Test") (state . "open")
                 (body . "desc") (user . ((login . "me")))))))
    (let* ((backend (cdr (assq 'github shipit-issue-backends)))
           (create-fn (plist-get backend :create-issue))
           (result (funcall create-fn '(:repo "o/r") "Test" "desc")))
      (should (= 42 (cdr (assq 'number result))))
      (should (string= "Test" (cdr (assq 'title result)))))))

;;; Jira Create Issue Tests

(ert-deftest test-shipit-issue-jira-create-issue-calls-api ()
  "GIVEN the jira backend with project-keys (\"PRJ\")
WHEN calling :create-issue with title and body
THEN it POSTs to /rest/api/2/issue with correct Jira payload
     AND re-fetches the created issue for full normalization."
  (let ((post-called nil)
        (fetch-called nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config _path data)
                 (setq post-called data)
                 '((key . "PRJ-123") (id . "10001"))))
              ((symbol-function 'shipit-issue-jira--fetch-issue)
               (lambda (_config id)
                 (setq fetch-called id)
                 '((id . "PRJ-123") (number . "PRJ-123")
                   (title . "New Bug") (state . "To Do")))))
      (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
             (create-fn (plist-get backend :create-issue))
             (config '(:base-url "https://jira.example.com"
                       :project-keys ("PRJ")))
             (result (funcall create-fn config "New Bug" "Description")))
        ;; Verify POST payload structure
        (let* ((fields (cdr (assq 'fields post-called)))
               (project (cdr (assq 'project fields)))
               (issuetype (cdr (assq 'issuetype fields))))
          (should (string= "PRJ" (cdr (assq 'key project))))
          (should (string= "New Bug" (cdr (assq 'summary fields))))
          (should (string= "Description" (cdr (assq 'description fields))))
          (should (string= "Task" (cdr (assq 'name issuetype)))))
        ;; Verify re-fetch
        (should (string= "PRJ-123" fetch-called))
        ;; Verify normalized result
        (should (string= "PRJ-123" (cdr (assq 'id result))))))))

(ert-deftest test-shipit-issue-jira-create-issue-uses-default-issue-type ()
  "GIVEN a jira config with :default-issue-type \"Bug\"
WHEN calling :create-issue
THEN the issuetype in the payload is \"Bug\"."
  (let ((post-called nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config _path data)
                 (setq post-called data)
                 '((key . "PRJ-1") (id . "10001"))))
              ((symbol-function 'shipit-issue-jira--fetch-issue)
               (lambda (_config _id)
                 '((id . "PRJ-1") (number . "PRJ-1") (title . "T")))))
      (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
             (create-fn (plist-get backend :create-issue))
             (config '(:base-url "https://jira.example.com"
                       :project-keys ("PRJ")
                       :default-issue-type "Bug")))
        (funcall create-fn config "Title" "Body")
        (let* ((fields (cdr (assq 'fields post-called)))
               (issuetype (cdr (assq 'issuetype fields))))
          (should (string= "Bug" (cdr (assq 'name issuetype)))))))))

(ert-deftest test-shipit-issue-jira-create-issue-errors-without-project-keys ()
  "GIVEN a jira config with no :project-keys
WHEN calling :create-issue
THEN an error is signaled."
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
             (lambda (_config _path _data) nil)))
    (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
           (create-fn (plist-get backend :create-issue))
           (config '(:base-url "https://jira.example.com")))
      (should-error (funcall create-fn config "Title" "Body")))))

;;; Wrapper Dispatch Tests

(ert-deftest test-shipit-issues-create-issue-dispatches-to-backend ()
  "GIVEN a repo resolving to the github backend
WHEN calling shipit-issues--create-issue
THEN it delegates to the backend's :create-issue function."
  (let ((create-called nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit-issue-github--create-issue)
               (lambda (_config title body)
                 (setq create-called (list title body))
                 '((number . 1) (title . "T")))))
      (let ((result (shipit-issues--create-issue "owner/repo" "T" "B")))
        (should (equal '("T" "B") create-called))
        (should (= 1 (cdr (assq 'number result))))))))

;;; Editor Save Tests

(ert-deftest test-shipit-editor-save-create-issue-extracts-title-body ()
  "GIVEN an editor buffer with type 'create-issue containing title and body
WHEN saving
THEN title is first line, body is everything after first blank line."
  (skip-unless (fboundp 'shipit-editor-save))
  (let ((created-title nil)
        (created-body nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit-issues--create-issue)
               (lambda (_repo title body)
                 (setq created-title title
                       created-body body)
                 '((number . 55) (title . "My Issue"))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (_number _repo) nil))
              ((symbol-function 'shipit-editor--close)
               (lambda (&optional _restore) nil)))
      (with-temp-buffer
        (setq-local shipit-editor--type 'create-issue)
        (setq-local shipit-editor--repo "owner/repo")
        (setq-local shipit-editor--source-buffer nil)
        (setq-local shipit-editor--pr-number nil)
        (setq-local shipit-editor--file-path nil)
        (setq-local shipit-editor--line-number nil)
        (setq-local shipit-editor--comment-id nil)
        (setq-local shipit-editor--parent-comment-id nil)
        (setq-local shipit-editor--review-event nil)
        (setq-local shipit-editor--on-save nil)
        (insert "My Issue Title\n\nThis is the body\nwith multiple lines")
        (shipit-editor-save)
        (should (string= "My Issue Title" created-title))
        (should (string= "This is the body\nwith multiple lines" created-body))))))

(ert-deftest test-shipit-editor-save-create-issue-title-only ()
  "GIVEN an editor buffer with only a title (no blank line separator)
WHEN saving
THEN body is empty string."
  (skip-unless (fboundp 'shipit-editor-save))
  (let ((created-body nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit-issues--create-issue)
               (lambda (_repo _title body)
                 (setq created-body body)
                 '((number . 1) (title . "T"))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (_number _repo) nil))
              ((symbol-function 'shipit-editor--close)
               (lambda (&optional _restore) nil)))
      (with-temp-buffer
        (setq-local shipit-editor--type 'create-issue)
        (setq-local shipit-editor--repo "owner/repo")
        (setq-local shipit-editor--source-buffer nil)
        (setq-local shipit-editor--pr-number nil)
        (setq-local shipit-editor--file-path nil)
        (setq-local shipit-editor--line-number nil)
        (setq-local shipit-editor--comment-id nil)
        (setq-local shipit-editor--parent-comment-id nil)
        (setq-local shipit-editor--review-event nil)
        (setq-local shipit-editor--on-save nil)
        (insert "Just a title")
        (shipit-editor-save)
        (should (string= "" created-body))))))

;;; Creation Fields Tests

(ert-deftest test-shipit-issue-github-creation-fields-returns-4-fields ()
  "GIVEN the github backend
WHEN calling :creation-fields
THEN it returns 4 field descriptors (title, body, labels, assignees)."
  (let* ((backend (cdr (assq 'github shipit-issue-backends)))
         (fields-fn (plist-get backend :creation-fields))
         (fields (funcall fields-fn '(:repo "owner/repo"))))
    (should (= 4 (length fields)))
    (should (eq 'title (plist-get (nth 0 fields) :name)))
    (should (eq 'body (plist-get (nth 1 fields) :name)))
    (should (eq 'labels (plist-get (nth 2 fields) :name)))
    (should (eq 'assignees (plist-get (nth 3 fields) :name)))))

(ert-deftest test-shipit-issue-github-creation-fields-types ()
  "GIVEN the github backend
WHEN calling :creation-fields
THEN title is string, body is text, labels/assignees are multi-select."
  (let* ((backend (cdr (assq 'github shipit-issue-backends)))
         (fields-fn (plist-get backend :creation-fields))
         (fields (funcall fields-fn '(:repo "owner/repo"))))
    (should (eq 'string (plist-get (nth 0 fields) :type)))
    (should (eq 'text (plist-get (nth 1 fields) :type)))
    (should (eq 'multi-select (plist-get (nth 2 fields) :type)))
    (should (eq 'multi-select (plist-get (nth 3 fields) :type)))))

(ert-deftest test-shipit-issue-jira-creation-fields-returns-6-fields ()
  "GIVEN the jira backend
WHEN calling :creation-fields
THEN it returns 6 field descriptors (title, body, issue-type, labels, assignees, components)."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (fields-fn (plist-get backend :creation-fields))
         (config '(:base-url "https://jira.example.com"
                   :project-keys ("PRJ")))
         (fields (funcall fields-fn config)))
    (should (= 6 (length fields)))
    (should (eq 'title (plist-get (nth 0 fields) :name)))
    (should (eq 'body (plist-get (nth 1 fields) :name)))
    (should (eq 'issue-type (plist-get (nth 2 fields) :name)))
    (should (eq 'labels (plist-get (nth 3 fields) :name)))
    (should (eq 'assignees (plist-get (nth 4 fields) :name)))
    (should (eq 'components (plist-get (nth 5 fields) :name)))))

(ert-deftest test-shipit-issue-jira-creation-fields-types ()
  "GIVEN the jira backend
WHEN calling :creation-fields
THEN issue-type is select, components is multi-select."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (fields-fn (plist-get backend :creation-fields))
         (config '(:base-url "https://jira.example.com"
                   :project-keys ("PRJ")))
         (fields (funcall fields-fn config)))
    (should (eq 'select (plist-get (nth 2 fields) :type)))
    (should (eq 'multi-select (plist-get (nth 5 fields) :type)))))

;;; Extended Create Tests

(ert-deftest test-shipit-issues-create-issue-extended-dispatches ()
  "GIVEN a backend with :create-issue-extended
WHEN calling shipit-issues--create-issue-extended
THEN it delegates to the extended function."
  (let ((extended-called nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit-issue-github--create-issue-extended)
               (lambda (_config fields)
                 (setq extended-called fields)
                 '((number . 42) (title . "T")))))
      (let ((fields '((title . "T") (body . "B") (labels . ("bug")))))
        (shipit-issues--create-issue-extended "owner/repo" fields)
        (should extended-called)
        (should (equal "T" (cdr (assq 'title extended-called))))))))

(ert-deftest test-shipit-issues-create-issue-extended-falls-back ()
  "GIVEN a backend WITHOUT :create-issue-extended
WHEN calling shipit-issues--create-issue-extended
THEN it falls back to :create-issue with just title+body."
  (let ((simple-called nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    ;; Temporarily remove the extended function
    (let* ((backend-entry (assq 'github shipit-issue-backends))
           (backend-plist (cdr backend-entry))
           (orig-extended (plist-get backend-plist :create-issue-extended)))
      (plist-put backend-plist :create-issue-extended nil)
      (unwind-protect
          (cl-letf (((symbol-function 'shipit-issue-github--create-issue)
                     (lambda (_config title body)
                       (setq simple-called (list title body))
                       '((number . 1) (title . "T")))))
            (shipit-issues--create-issue-extended
             "owner/repo" '((title . "T") (body . "B") (labels . ("bug"))))
            (should simple-called)
            (should (equal '("T" "B") simple-called)))
        (plist-put backend-plist :create-issue-extended orig-extended)))))

(ert-deftest test-shipit-issue-github-extended-create-includes-labels-assignees ()
  "GIVEN the github backend
WHEN calling :create-issue-extended with labels and assignees
THEN the POST payload includes labels and assignees arrays."
  (let ((called-data nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (_endpoint data &optional _method)
                 (setq called-data data)
                 '((number . 99) (title . "T") (state . "open")))))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (create-fn (plist-get backend :create-issue-extended))
             (config '(:repo "owner/repo"))
             (fields '((title . "New Issue") (body . "Body")
                       (labels . ("bug" "urgent"))
                       (assignees . ("alice" "bob")))))
        (funcall create-fn config fields)
        (should (equal '("bug" "urgent")
                       (cdr (assq 'labels called-data))))
        (should (equal '("alice" "bob")
                       (cdr (assq 'assignees called-data))))))))

(ert-deftest test-shipit-issue-jira-extended-create-includes-fields ()
  "GIVEN the jira backend
WHEN calling :create-issue-extended with issue-type, labels, components
THEN the POST payload includes all fields."
  (let ((post-called nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-post)
               (lambda (_config _path data)
                 (setq post-called data)
                 '((key . "PRJ-99") (id . "10099"))))
              ((symbol-function 'shipit-issue-jira--fetch-issue)
               (lambda (_config _id)
                 '((id . "PRJ-99") (number . "PRJ-99") (title . "T")))))
      (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
             (create-fn (plist-get backend :create-issue-extended))
             (config '(:base-url "https://jira.example.com"
                       :project-keys ("PRJ")))
             (fields '((title . "New Bug") (body . "Description")
                       (issue-type . "Bug")
                       (labels . ("backend" "critical"))
                       (components . ("API")))))
        (funcall create-fn config fields)
        (let* ((jira-fields (cdr (assq 'fields post-called)))
               (issuetype (cdr (assq 'issuetype jira-fields))))
          (should (string= "Bug" (cdr (assq 'name issuetype))))
          (should (equal '("backend" "critical")
                         (cdr (assq 'labels jira-fields))))
          (should (equal `(((name . "API")))
                         (cdr (assq 'components jira-fields)))))))))

;;; Backend Accessor Tests

(ert-deftest test-shipit-issue-creation-fields-accessor ()
  "GIVEN a backend with :creation-fields
WHEN calling shipit-issue--creation-fields
THEN it returns the field descriptors."
  (let* ((backend (cdr (assq 'github shipit-issue-backends)))
         (config '(:repo "owner/repo"))
         (fields (shipit-issue--creation-fields backend config)))
    (should fields)
    (should (= 4 (length fields)))))

(ert-deftest test-shipit-issue-creation-fields-returns-nil-when-missing ()
  "GIVEN a backend without :creation-fields
WHEN calling shipit-issue--creation-fields
THEN it returns nil."
  (let* ((backend '(:name "Dummy" :creation-fields nil))
         (fields (shipit-issue--creation-fields backend '(:repo "x"))))
    (should (null fields))))

;;; Interactive Command Tests

(ert-deftest test-shipit-create-issue-opens-rich-buffer ()
  "GIVEN a backend with :creation-fields
WHEN calling shipit-create-issue
THEN it opens the rich creation buffer."
  (let ((rich-buffer-opened nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-issue-create-buffer)
               (lambda () (setq rich-buffer-opened t))))
      (let ((shipit-issue-backend 'github)
            (shipit-issue-backend-config nil)
            (shipit-issue-repo-backends nil))
        (shipit-create-issue)
        (should rich-buffer-opened)))))

(ert-deftest test-shipit-create-issue-falls-back-to-editor ()
  "GIVEN a backend WITHOUT :creation-fields
WHEN calling shipit-create-issue
THEN it falls back to the plain-text editor."
  (let ((editor-context nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-editor-open)
               (lambda (context) (setq editor-context context))))
      ;; Temporarily remove creation-fields from github backend
      (let* ((shipit-issue-backend 'github)
             (shipit-issue-backend-config nil)
             (shipit-issue-repo-backends nil)
             (backend-entry (assq 'github shipit-issue-backends))
             (backend-plist (cdr backend-entry))
             (orig-fields (plist-get backend-plist :creation-fields)))
        (plist-put backend-plist :creation-fields nil)
        (unwind-protect
            (progn
              (shipit-create-issue)
              (should editor-context)
              (should (eq 'create-issue (plist-get editor-context :type))))
          (plist-put backend-plist :creation-fields orig-fields))))))

(provide 'test-shipit-issue-create)
;;; test-shipit-issue-create.el ends here
