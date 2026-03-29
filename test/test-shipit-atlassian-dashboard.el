;;; test-shipit-atlassian-dashboard.el --- Tests for Atlassian dashboard -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-atlassian-dashboard)
(require 'shipit-atlassian-board)
(require 'shipit-issues-buffer)
(require 'shipit-issue-jira)

(ert-deftest test-atlassian-dashboard-creates-buffer ()
  "GIVEN a Jira config
WHEN opening the dashboard
THEN a buffer in shipit-atlassian-dashboard-mode is created."
  (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
             (lambda (_repo) (cons nil '(:base-url "https://test.atlassian.net"))))
            ((symbol-function 'shipit-atlassian-dashboard--refresh-data)
             (lambda () nil)))
    (let ((buf (shipit-atlassian-dashboard--open "test/repo")))
      (unwind-protect
          (with-current-buffer buf
            (should (eq major-mode 'shipit-atlassian-dashboard-mode))
            (should (equal shipit-atlassian-dashboard--repo "test/repo")))
        (kill-buffer buf)))))

(ert-deftest test-atlassian-dashboard-my-issues-jql ()
  "GIVEN a Jira config with project-keys
WHEN building my-issues JQL
THEN JQL filters by assignee, unresolved, and project."
  (let ((config '(:base-url "https://test.atlassian.net"
                  :project-keys ("PROJ1" "PROJ2"))))
    (let ((jql (shipit-atlassian-dashboard--my-issues-jql config)))
      (should (string-match-p "assignee=currentUser()" jql))
      (should (string-match-p "resolution=Unresolved" jql))
      (should (string-match-p "project in (PROJ1,PROJ2)" jql)))))

(ert-deftest test-atlassian-dashboard-my-issues-jql-no-projects ()
  "GIVEN a Jira config without project-keys
WHEN building my-issues JQL
THEN JQL filters by assignee and unresolved without project clause."
  (let ((config '(:base-url "https://test.atlassian.net")))
    (let ((jql (shipit-atlassian-dashboard--my-issues-jql config)))
      (should (string-match-p "assignee=currentUser()" jql))
      (should (string-match-p "resolution=Unresolved" jql))
      (should-not (string-match-p "project in" jql)))))

(ert-deftest test-atlassian-dashboard-project-key ()
  "GIVEN issue IDs with various formats
WHEN extracting the project key
THEN the uppercase prefix before the dash is returned."
  (should (equal "PROJ" (shipit-atlassian-dashboard--project-key "PROJ-123")))
  (should (equal "AB2" (shipit-atlassian-dashboard--project-key "AB2-1")))
  (should (equal "OTHER" (shipit-atlassian-dashboard--project-key "invalid"))))

(ert-deftest test-atlassian-dashboard-group-by-project ()
  "GIVEN a list of normalized issues with different project prefixes
WHEN grouping by project
THEN issues are grouped into (project-key . items) alists."
  (let ((issues '(((id . "PROJ1-1") (title . "Bug A"))
                  ((id . "PROJ2-5") (title . "Task B"))
                  ((id . "PROJ1-3") (title . "Bug C")))))
    (let ((grouped (shipit-atlassian-dashboard--group-by-project issues)))
      (should (= 2 (length grouped)))
      (should (equal "PROJ1" (car (nth 0 grouped))))
      (should (= 2 (length (cdr (nth 0 grouped)))))
      (should (equal "PROJ2" (car (nth 1 grouped))))
      (should (= 1 (length (cdr (nth 1 grouped))))))))

(ert-deftest test-atlassian-dashboard-insert-issue-line ()
  "GIVEN a normalized issue alist
WHEN inserting an issue line with columns and widths
THEN the buffer contains key, title, state with text property."
  (with-temp-buffer
    (let* ((issue '((id . "PROJ-42") (title . "Fix login") (state . "Open")))
           (columns '(work status))
           (widths '((key . 12) (summary . 40) (status . 10))))
      (shipit-atlassian-dashboard--insert-issue-line issue columns widths)
      (let ((text (buffer-string)))
        (should (string-match-p "PROJ-42" text))
        (should (string-match-p "Fix login" text))
        (should (string-match-p "Open" text)))
      (goto-char (point-min))
      (should (equal "PROJ-42"
                     (get-text-property (point) 'shipit-issuelink-key))))))

(ert-deftest test-atlassian-dashboard-render-my-issues-section ()
  "GIVEN cached my-issues data
WHEN rendering the dashboard
THEN My Open Issues section appears with grouped items."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--backend-plist
           (cdr (assq 'jira shipit-issue-backends)))
          (shipit-atlassian-dashboard--my-issues
           '(((id . "PROJ-1") (title . "Fix login") (state . "Open")
              (issue-type . "Bug") (assignees . (((login . "me"))))))))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-my-issues-section))
      (let ((text (buffer-string)))
        (should (string-match-p "My Open Issues" text))
        (should (string-match-p "PROJ-1" text))
        (should (string-match-p "Fix login" text))))))

(ert-deftest test-atlassian-dashboard-render-my-issues-section-empty ()
  "GIVEN no cached my-issues data
WHEN rendering the my-issues section
THEN section shows 'No open issues' message."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--my-issues nil))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-my-issues-section))
      (let ((text (buffer-string)))
        (should (string-match-p "My Open Issues (0)" text))
        (should (string-match-p "No open issues" text))))))

(ert-deftest test-atlassian-dashboard-fetch-my-issues ()
  "GIVEN a Jira config and mock search response
WHEN fetching my issues
THEN normalized issues are returned."
  (let ((shipit-atlassian-dashboard--config
         '(:base-url "https://test.atlassian.net")))
    (cl-letf (((symbol-function 'shipit-issue-jira--search-page)
               (lambda (_config _jql _size _fields _token)
                 '((issues . [((key . "PROJ-1")
                               (fields . ((summary . "Bug fix")
                                          (status . ((name . "Open")))
                                          (priority . ((name . "High")))
                                          (issuetype . ((name . "Bug")))
                                          (assignee . ((displayName . "Me")
                                                       (accountId . "abc")))
                                          (updated . "2026-01-01T00:00:00.000+0000"))))]))))
              ((symbol-function 'shipit-issue-jira--normalize-issue)
               (lambda (data)
                 `((id . ,(cdr (assq 'key data)))
                   (title . ,(cdr (assq 'summary (cdr (assq 'fields data)))))))))
      (let ((result (shipit-atlassian-dashboard--fetch-my-issues)))
        (should (= 1 (length result)))
        (should (equal "PROJ-1" (cdr (assq 'id (car result)))))
        (should (equal "Bug fix" (cdr (assq 'title (car result)))))))))

(ert-deftest test-atlassian-dashboard-active-sprint-path ()
  "GIVEN a board ID
WHEN building the active sprint URL
THEN the path includes the board ID and state=active."
  (should (equal "/rest/agile/1.0/board/42/sprint?state=active"
                 (shipit-atlassian-dashboard--active-sprint-path 42))))

(ert-deftest test-atlassian-dashboard-sprint-issues-path ()
  "GIVEN a sprint ID
WHEN building the sprint issues URL
THEN the path includes the sprint ID and JQL for assignee/status."
  (let ((path (shipit-atlassian-dashboard--sprint-issues-path 99)))
    (should (string-match-p "/rest/agile/1.0/sprint/99/issue" path))
    (should (string-match-p "jql=" path))
    (should (string-match-p "fields=" path))))

(ert-deftest test-atlassian-dashboard-normalize-sprint-issues ()
  "GIVEN raw agile API sprint issue response
WHEN normalizing
THEN issues are extracted and normalized."
  (let ((raw '((issues . [((key . "PROJ-1")
                            (fields . ((summary . "Task A")
                                       (status . ((name . "To Do")))
                                       (priority . ((name . "High")))
                                       (assignee . ((displayName . "Alice")))
                                       (issuetype . ((name . "Story"))))))]))))
    (cl-letf (((symbol-function 'shipit-issue-jira--normalize-issue)
               (lambda (data)
                 `((id . ,(cdr (assq 'key data)))
                   (title . ,(cdr (assq 'summary (cdr (assq 'fields data)))))))))
      (let ((result (shipit-atlassian-dashboard--normalize-agile-issues raw)))
        (should (= 1 (length result)))
        (should (equal "PROJ-1" (cdr (assq 'id (car result)))))
        (should (equal "Task A" (cdr (assq 'title (car result)))))))))

(ert-deftest test-atlassian-dashboard-render-whats-next-section ()
  "GIVEN cached what's next items
WHEN rendering
THEN What's Next section appears with items."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--backend-plist
           (cdr (assq 'jira shipit-issue-backends)))
          (shipit-atlassian-dashboard--whats-next
           '(((id . "PROJ-10") (title . "Implement login") (state . "To Do"))))
          (shipit-atlassian-board-id 42))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-whats-next-section))
      (let ((text (buffer-string)))
        (should (string-match-p "What's Next" text))
        (should (string-match-p "PROJ-10" text))))))

(ert-deftest test-atlassian-dashboard-whats-next-no-board ()
  "GIVEN no board ID configured
WHEN rendering what's next section
THEN a configuration message is shown."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-board-id nil)
          (shipit-atlassian-dashboard--whats-next nil))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-whats-next-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Press RET to configure" text))))))

(ert-deftest test-atlassian-dashboard-whats-next-empty-sprint ()
  "GIVEN a board ID but no items in the sprint
WHEN rendering what's next section
THEN 'No items in current sprint' message is shown."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-board-id 42)
          (shipit-atlassian-dashboard--whats-next nil))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-whats-next-section))
      (let ((text (buffer-string)))
        (should (string-match-p "No items in current sprint" text))))))

(ert-deftest test-atlassian-dashboard-fetch-whats-next ()
  "GIVEN a configured board ID and mock API responses
WHEN fetching what's next items
THEN active sprint is resolved and issues returned."
  (let ((shipit-atlassian-dashboard--config
         '(:base-url "https://test.atlassian.net"))
        (shipit-atlassian-board-id 42))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (cond
                  ((string-match-p "board/42/sprint" path)
                   '((values . [((id . 100) (name . "Sprint 5") (state . "active"))])))
                  ((string-match-p "sprint/100/issue" path)
                   '((issues . [((key . "PROJ-1")
                                 (fields . ((summary . "Task A")
                                            (status . ((name . "To Do")))
                                            (priority . ((name . "High")))
                                            (assignee . ((displayName . "Alice")))
                                            (issuetype . ((name . "Story"))))))])))
                  (t nil))))
              ((symbol-function 'shipit-issue-jira--normalize-issue)
               (lambda (data)
                 `((id . ,(cdr (assq 'key data)))
                   (title . ,(cdr (assq 'summary (cdr (assq 'fields data)))))))))
      (let ((result (shipit-atlassian-dashboard--fetch-whats-next)))
        (should (= 1 (length result)))
        (should (equal "PROJ-1" (cdr (assq 'id (car result)))))))))

(ert-deftest test-atlassian-dashboard-fetch-whats-next-no-board ()
  "GIVEN no board ID configured
WHEN fetching what's next items
THEN nil is returned without any API calls."
  (let ((shipit-atlassian-board-id nil)
        (api-called nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config _path) (setq api-called t) nil)))
      (should (null (shipit-atlassian-dashboard--fetch-whats-next)))
      (should (null api-called)))))

;;; Board tests

(ert-deftest test-atlassian-board-config-path ()
  "GIVEN a board ID
WHEN building the board configuration path
THEN the path includes the board ID."
  (should (equal "/rest/agile/1.0/board/42/configuration"
                 (shipit-atlassian-board--config-path 42))))

(ert-deftest test-atlassian-board-issues-path ()
  "GIVEN a board ID
WHEN building the board issues path
THEN the path includes the board ID and fields."
  (let ((path (shipit-atlassian-board--issues-path 42 0)))
    (should (string-match-p "/rest/agile/1.0/board/42/issue" path))
    (should (string-match-p "maxResults=50" path))
    (should (string-match-p "startAt=0" path))
    (should (string-match-p "fields=" path))))

(ert-deftest test-atlassian-board-group-issues-by-column ()
  "GIVEN board column config and issues
WHEN grouping issues by column
THEN each issue lands in the correct column."
  (let ((columns '(((name . "To Do")
                    (statuses . [((name . "Open") (id . "1"))
                                 ((name . "To Do") (id . "2"))]))
                   ((name . "In Progress")
                    (statuses . [((name . "In Progress") (id . "3"))]))
                   ((name . "Done")
                    (statuses . [((name . "Done") (id . "4"))]))))
        (issues '(((id . "P-1") (state . "Open") (state-id . "1"))
                  ((id . "P-2") (state . "In Progress") (state-id . "3"))
                  ((id . "P-3") (state . "Done") (state-id . "4"))
                  ((id . "P-4") (state . "To Do") (state-id . "2")))))
    (let ((grouped (shipit-atlassian-board--group-by-column columns issues)))
      (should (= 3 (length grouped)))
      (should (equal "To Do" (cdr (assq 'name (nth 0 grouped)))))
      (should (= 2 (length (cdr (assq 'issues (nth 0 grouped))))))
      (should (equal "In Progress" (cdr (assq 'name (nth 1 grouped)))))
      (should (= 1 (length (cdr (assq 'issues (nth 1 grouped))))))
      (should (equal "Done" (cdr (assq 'name (nth 2 grouped)))))
      (should (= 1 (length (cdr (assq 'issues (nth 2 grouped)))))))))

(ert-deftest test-atlassian-board-group-empty-columns ()
  "GIVEN columns where some have no matching issues
WHEN grouping issues by column
THEN empty columns appear with zero issues."
  (let ((columns '(((name . "To Do")
                    (statuses . [((name . "Open") (id . "1"))]))
                   ((name . "In Progress")
                    (statuses . [((name . "In Progress") (id . "2"))]))
                   ((name . "Done")
                    (statuses . [((name . "Done") (id . "3"))]))))
        (issues '(((id . "P-1") (state . "Open") (state-id . "1")))))
    (let ((grouped (shipit-atlassian-board--group-by-column columns issues)))
      (should (= 3 (length grouped)))
      (should (= 1 (length (cdr (assq 'issues (nth 0 grouped))))))
      (should (= 0 (length (cdr (assq 'issues (nth 1 grouped))))))
      (should (= 0 (length (cdr (assq 'issues (nth 2 grouped)))))))))

(ert-deftest test-atlassian-board-fetch ()
  "GIVEN mock board configuration and issues API responses
WHEN fetching board data
THEN issues are grouped into columns."
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config path)
               (cond
                ((string-match-p "configuration" path)
                 '((columnConfig . ((columns . [((name . "To Do")
                                                  (statuses . [((id . "1"))]))
                                                ((name . "Done")
                                                  (statuses . [((id . "4"))]))])))))
                ((string-match-p "issue" path)
                 '((total . 1)
                   (issues . [((key . "P-1")
                               (fields . ((summary . "Task A")
                                          (status . ((name . "Open") (id . "1")))
                                          (priority . ((name . "High")))
                                          (issuetype . ((name . "Task")))
                                          (assignee . nil))))])))
                (t nil))))
            ((symbol-function 'shipit-issue-jira--normalize-issue)
             (lambda (data)
               (let ((fields (cdr (assq 'fields data))))
                 `((id . ,(cdr (assq 'key data)))
                   (title . ,(cdr (assq 'summary fields)))
                   (state . ,(cdr (assq 'name (cdr (assq 'status fields)))))
                   (state-id . ,(cdr (assq 'id (cdr (assq 'status fields))))))))))
    (let* ((config '(:base-url "https://test.atlassian.net"))
           (result (shipit-atlassian-board--fetch config 42)))
      (should (= 2 (length result)))
      (should (equal "To Do" (cdr (assq 'name (nth 0 result)))))
      (should (= 1 (length (cdr (assq 'issues (nth 0 result))))))
      (should (equal "P-1" (cdr (assq 'id (car (cdr (assq 'issues (nth 0 result))))))))
      (should (equal "Done" (cdr (assq 'name (nth 1 result)))))
      (should (= 0 (length (cdr (assq 'issues (nth 1 result)))))))))

(ert-deftest test-atlassian-board-render-columns ()
  "GIVEN grouped board data
WHEN rendering the board section
THEN column headers and issue keys appear."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--backend-plist
           (cdr (assq 'jira shipit-issue-backends)))
          (shipit-atlassian-dashboard--board-data
           '(((name . "To Do")
              (issues . (((id . "P-1") (title . "Fix bug") (state . "To Do")))))
             ((name . "Done")
              (issues . (((id . "P-2") (title . "Add tests") (state . "Done")))))))
          (shipit-atlassian-board-id 42))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-board-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Board" text))
        (should (string-match-p "To Do" text))
        (should (string-match-p "P-1" text))
        (should (string-match-p "Done" text))
        (should (string-match-p "P-2" text))))))

(ert-deftest test-atlassian-board-render-no-board-id ()
  "GIVEN no board ID configured
WHEN rendering the board section
THEN a configuration message is shown."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-board-id nil)
          (shipit-atlassian-dashboard--board-data nil))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-board-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Board" text))
        (should (string-match-p "Press RET to configure" text))
        ;; THEN the configure line has the text property for RET handling
        (goto-char (point-min))
        (search-forward "Press RET")
        (should (eq 'board-id (get-text-property (point) 'shipit-atlassian-configure)))))))

(ert-deftest test-atlassian-board-render-no-data ()
  "GIVEN a board ID but no board data
WHEN rendering the board section
THEN 'No board data' message is shown."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-board-id 42)
          (shipit-atlassian-dashboard--board-data nil))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-board-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Board" text))
        (should (string-match-p "No board data" text))))))

;;; Frequently Visited tests

(ert-deftest test-atlassian-dashboard-record-visit ()
  "GIVEN an empty visit history
WHEN recording a visit
THEN the entry is added with timestamp."
  (let ((shipit-atlassian-dashboard--recent-visits nil)
        (shipit-atlassian-dashboard--recent-file (make-temp-file "shipit-test-recent")))
    (unwind-protect
        (progn
          (shipit-atlassian-dashboard--record-visit "PROJ-1" "Fix login bug")
          (should (= 1 (length shipit-atlassian-dashboard--recent-visits)))
          (should (equal "PROJ-1"
                         (cdr (assq 'key (car shipit-atlassian-dashboard--recent-visits))))))
      (delete-file shipit-atlassian-dashboard--recent-file))))

(ert-deftest test-atlassian-dashboard-record-visit-deduplicates ()
  "GIVEN an existing visit for PROJ-1
WHEN recording another visit for PROJ-1
THEN only one entry exists and it's moved to front."
  (let ((shipit-atlassian-dashboard--recent-visits
         (list `((key . "PROJ-2") (title . "Other") (time . 100))
               `((key . "PROJ-1") (title . "Old title") (time . 50))))
        (shipit-atlassian-dashboard--recent-file (make-temp-file "shipit-test-recent")))
    (unwind-protect
        (progn
          (shipit-atlassian-dashboard--record-visit "PROJ-1" "New title")
          (should (= 2 (length shipit-atlassian-dashboard--recent-visits)))
          (should (equal "PROJ-1"
                         (cdr (assq 'key (car shipit-atlassian-dashboard--recent-visits))))))
      (delete-file shipit-atlassian-dashboard--recent-file))))

(ert-deftest test-atlassian-dashboard-record-visit-max-size ()
  "GIVEN a full visit history
WHEN recording a new visit
THEN the oldest entry is dropped."
  (let ((shipit-atlassian-frequently-visited-max 3)
        (shipit-atlassian-dashboard--recent-visits
         (list `((key . "A") (title . "A") (time . 30))
               `((key . "B") (title . "B") (time . 20))
               `((key . "C") (title . "C") (time . 10))))
        (shipit-atlassian-dashboard--recent-file (make-temp-file "shipit-test-recent")))
    (unwind-protect
        (progn
          (shipit-atlassian-dashboard--record-visit "D" "New item")
          (should (= 3 (length shipit-atlassian-dashboard--recent-visits)))
          (should (equal "D" (cdr (assq 'key (car shipit-atlassian-dashboard--recent-visits))))))
      (delete-file shipit-atlassian-dashboard--recent-file))))

(ert-deftest test-atlassian-dashboard-render-frequently-visited ()
  "GIVEN recent visits
WHEN rendering
THEN Frequently Visited section shows items with relative times."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--backend-plist
           (cdr (assq 'jira shipit-issue-backends)))
          (shipit-atlassian-dashboard--recent-visits
           (list `((key . "PROJ-5") (title . "Fix thing") (time . ,(float-time))))))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-frequently-visited-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Frequently Visited" text))
        (should (string-match-p "PROJ-5" text))))))

(ert-deftest test-atlassian-dashboard-render-frequently-visited-empty ()
  "GIVEN no recent visits
WHEN rendering
THEN Frequently Visited section shows empty message."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--recent-visits nil)
          (shipit-atlassian-dashboard--recent-file "/tmp/shipit-nonexistent-file"))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-frequently-visited-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Frequently Visited (0)" text))
        (should (string-match-p "No recently visited issues" text))))))


(ert-deftest test-atlassian-dashboard-persistence-round-trip ()
  "GIVEN recorded visits saved to file
WHEN loading from file with nil visits
THEN visits are restored from disk."
  (let* ((shipit-atlassian-dashboard--recent-file (make-temp-file "shipit-test-recent"))
         (shipit-atlassian-dashboard--recent-visits nil))
    (unwind-protect
        (progn
          (shipit-atlassian-dashboard--record-visit "PROJ-1" "Bug fix")
          (shipit-atlassian-dashboard--record-visit "PROJ-2" "Feature")
          (let ((saved shipit-atlassian-dashboard--recent-visits))
            ;; Clear in-memory and reload from disk
            (setq shipit-atlassian-dashboard--recent-visits nil)
            (shipit-atlassian-dashboard--load-recent)
            (should (= 2 (length shipit-atlassian-dashboard--recent-visits)))
            (should (equal "PROJ-2"
                           (cdr (assq 'key (car shipit-atlassian-dashboard--recent-visits)))))))
      (delete-file shipit-atlassian-dashboard--recent-file))))

(ert-deftest test-atlassian-dashboard-full-render ()
  "GIVEN cached data for all sections
WHEN rendering the dashboard
THEN all sections are rendered."
  (with-temp-buffer
    (shipit-atlassian-dashboard-mode)
    (setq shipit-atlassian-dashboard--repo "test/repo")
    (setq shipit-atlassian-dashboard--config '(:base-url "https://test.atlassian.net"))
    (setq shipit-atlassian-dashboard--backend-plist
          (cdr (assq 'jira shipit-issue-backends)))
    (setq shipit-atlassian-dashboard--my-issues
          '(((id . "P-1") (title . "Bug") (state . "Open"))))
    (setq shipit-atlassian-dashboard--whats-next
          '(((id . "P-2") (title . "Task") (state . "To Do"))))
    (setq shipit-atlassian-dashboard--board-data
          '(((name . "To Do") (issues . (((id . "P-3") (title . "X") (state . "To Do")))))))
    (setq shipit-atlassian-board-id 42)
    (shipit-atlassian-dashboard--render)
    (let ((text (buffer-string)))
      (should (string-match-p "Atlassian Dashboard" text))
      (should (string-match-p "What's Next" text))
      (should (string-match-p "My Open Issues" text))
      (should (string-match-p "Board" text))
      (should (string-match-p "Frequently Visited" text)))))

(ert-deftest test-atlassian-dashboard-refresh-data-fetches-and-renders ()
  "GIVEN mock fetch functions
WHEN refreshing dashboard data
THEN all data is fetched and render is called."
  (with-temp-buffer
    (shipit-atlassian-dashboard-mode)
    (setq shipit-atlassian-dashboard--repo "test/repo")
    (setq shipit-atlassian-dashboard--config '(:base-url "https://test.atlassian.net"))
    (setq shipit-atlassian-board-id 42)
    (let ((render-called nil))
      (cl-letf (((symbol-function 'shipit-atlassian-dashboard--fetch-my-issues)
                 (lambda () '(((id . "P-1") (title . "Bug") (state . "Open")))))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-whats-next)
                 (lambda () '(((id . "P-2") (title . "Task") (state . "To Do")))))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-board)
                 (lambda () '(((name . "Col") (issues . nil)))))
                ((symbol-function 'shipit-atlassian-dashboard--render)
                 (lambda () (setq render-called t))))
        (shipit-atlassian-dashboard--refresh-data)
        (should render-called)
        (should (equal '(((id . "P-1") (title . "Bug") (state . "Open")))
                       shipit-atlassian-dashboard--my-issues))
        (should (equal '(((id . "P-2") (title . "Task") (state . "To Do")))
                       shipit-atlassian-dashboard--whats-next))
        (should (equal '(((name . "Col") (issues . nil)))
                       shipit-atlassian-dashboard--board-data))))))

(ert-deftest test-atlassian-dashboard-refresh-data-handles-fetch-error ()
  "GIVEN a fetch function that errors
WHEN refreshing dashboard data
THEN partial data is kept and render is still called."
  (with-temp-buffer
    (shipit-atlassian-dashboard-mode)
    (setq shipit-atlassian-dashboard--repo "test/repo")
    (setq shipit-atlassian-dashboard--config '(:base-url "https://test.atlassian.net"))
    (setq shipit-atlassian-board-id nil)
    (let ((render-called nil))
      (cl-letf (((symbol-function 'shipit-atlassian-dashboard--fetch-my-issues)
                 (lambda () (error "API timeout")))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-whats-next)
                 (lambda () nil))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-board)
                 (lambda () nil))
                ((symbol-function 'shipit-atlassian-dashboard--render)
                 (lambda () (setq render-called t))))
        (shipit-atlassian-dashboard--refresh-data)
        (should render-called)))))

(ert-deftest test-atlassian-dashboard-advice-records-visit ()
  "GIVEN a mock shipit-issues-open-buffer
WHEN the advice wraps it
THEN a visit is recorded for the opened issue."
  (let ((shipit-atlassian-dashboard--recent-visits nil)
        (shipit-atlassian-dashboard--recent-file (make-temp-file "shipit-test-recent"))
        (visit-recorded nil))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit-atlassian-dashboard--record-visit)
                   (lambda (key title)
                     (setq visit-recorded (cons key title)))))
          (let* ((buf-name (shipit-issue-buffer-name "test/repo" "PROJ-1"))
                 (buf (get-buffer-create buf-name)))
            (unwind-protect
                (progn
                  (with-current-buffer buf
                    (setq shipit-issue-buffer-data '((title . "Fix login bug"))))
                  (shipit-atlassian-dashboard--advice-record-visit
                   (lambda (_number &rest _args) nil)
                   "PROJ-1" "test/repo")
                  (should visit-recorded)
                  (should (equal "PROJ-1" (car visit-recorded))))
              (kill-buffer buf))))
      (delete-file shipit-atlassian-dashboard--recent-file))))

(ert-deftest test-atlassian-dashboard-advice-skips-numeric-issues ()
  "GIVEN a numeric issue number (GitHub-style)
WHEN the advice fires
THEN no visit is recorded."
  (let ((shipit-atlassian-dashboard--recent-visits nil)
        (shipit-atlassian-dashboard--recent-file (make-temp-file "shipit-test-recent"))
        (visit-recorded nil))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit-atlassian-dashboard--record-visit)
                   (lambda (key title)
                     (setq visit-recorded (cons key title)))))
          ;; Numeric issue number (GitHub)
          (shipit-atlassian-dashboard--advice-record-visit
           (lambda (_number &rest _args) nil)
           42 "test/repo")
          (should-not visit-recorded)
          ;; String number without project prefix
          (shipit-atlassian-dashboard--advice-record-visit
           (lambda (_number &rest _args) nil)
           "99" "test/repo")
          (should-not visit-recorded))
      (delete-file shipit-atlassian-dashboard--recent-file))))

(ert-deftest test-atlassian-dashboard-parse-board-input ()
  "GIVEN various board ID inputs
WHEN parsing
THEN numeric board ID is extracted."
  ;; Plain number
  (should (= 42 (shipit-atlassian-dashboard--parse-board-input "42")))
  ;; Board URL
  (should (= 7 (shipit-atlassian-dashboard--parse-board-input
                 "https://foo.atlassian.net/jira/software/projects/X/boards/7")))
  ;; Alternate URL with /board/ (singular)
  (should (= 99 (shipit-atlassian-dashboard--parse-board-input
                  "https://foo.atlassian.net/jira/software/projects/X/board/99")))
  ;; Invalid input
  (should-not (shipit-atlassian-dashboard--parse-board-input "hello"))
  (should-not (shipit-atlassian-dashboard--parse-board-input "")))

(ert-deftest test-atlassian-dashboard-section-visibility ()
  "GIVEN all dashboard section types
WHEN checking initial visibility
THEN all are hidden by default, unknown types return nil."
  ;; Sub-sections
  (let ((col-section (make-instance 'magit-section :type 'atlassian-board-column))
        (proj-section (make-instance 'magit-section :type 'atlassian-my-issues-project)))
    (should (eq 'hide (shipit-atlassian-dashboard--section-visibility col-section)))
    (should (eq 'hide (shipit-atlassian-dashboard--section-visibility proj-section))))
  ;; Top-level sections also hidden
  (dolist (type '(atlassian-dashboard atlassian-my-issues
                  atlassian-whats-next atlassian-board
                  atlassian-frequently-visited))
    (let ((section (make-instance 'magit-section :type type)))
      (should (eq 'hide (shipit-atlassian-dashboard--section-visibility section)))))
  ;; Unknown section types return nil
  (let ((other-section (make-instance 'magit-section :type 'unknown)))
    (should-not (shipit-atlassian-dashboard--section-visibility other-section))))

(ert-deftest test-atlassian-dashboard-insert-issue-line-with-icons ()
  "GIVEN a normalized issue with type, priority, status-category
WHEN inserting an issue line with column renderer
THEN the buffer contains key, title, state and text property."
  (with-temp-buffer
    (let* ((shipit-use-svglib-icons nil)
           (issue '((id . "PROJ-42") (title . "Fix login") (state . "In Progress")
                   (issue-type . "Bug") (priority . "High")
                   (status-category . "indeterminate")))
           (columns '(work status))
           (widths `((key . 12) (summary . 40) (status . 20)
                     (status-category-face
                      . ,#'shipit-issue-jira--status-category-face))))
      (shipit-atlassian-dashboard--insert-issue-line issue columns widths)
      (let ((text (buffer-string)))
        (should (string-match-p "PROJ-42" text))
        (should (string-match-p "Fix login" text))
        (should (string-match-p "In Progress" text)))
      (goto-char (point-min))
      (should (equal "PROJ-42"
                     (get-text-property (point) 'shipit-issuelink-key))))))

(ert-deftest test-atlassian-dashboard-insert-issue-line-status-has-face ()
  "GIVEN an issue with status-category 'done'
WHEN inserting an issue line with status-category-face callback
THEN the status text has the success face applied."
  (with-temp-buffer
    (let* ((issue '((id . "PROJ-1") (title . "Finish login") (state . "Closed")
                    (status-category . "done")))
           (columns '(work status))
           (widths `((key . 12) (summary . 40) (status . 10)
                     (status-category-face
                      . ,#'shipit-issue-jira--status-category-face))))
      (shipit-atlassian-dashboard--insert-issue-line issue columns widths)
      (goto-char (point-min))
      (let ((pos (string-match "Closed" (buffer-string))))
        (should pos)
        (should (eq 'success (get-text-property (+ (point-min) pos) 'face)))))))

(provide 'test-shipit-atlassian-dashboard)
;;; test-shipit-atlassian-dashboard.el ends here
