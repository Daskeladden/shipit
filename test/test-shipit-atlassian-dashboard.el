;;; test-shipit-atlassian-dashboard.el --- Tests for Atlassian dashboard -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-atlassian-dashboard)
(require 'shipit-atlassian-board)
(require 'shipit-issues-buffer)
(require 'shipit-issue-jira)

(ert-deftest test-normalize-date-offset-passes-valid-through ()
  "GIVEN JQL-valid relative offsets or non-offset expressions
WHEN normalising
THEN the input is returned unchanged."
  (dolist (input '("-7d" "-2w" "-1h" "-30m"
                   "2026-01-15" "startOfWeek()" "endOfMonth()" "now()"
                   nil ""))
    (should (equal input
                   (shipit-atlassian-dashboard--normalize-date-offset
                    input)))))

(ert-deftest test-normalize-date-offset-converts-months-and-years ()
  "GIVEN month/year offsets not supported by JQL
WHEN normalising
THEN the value is rewritten into an equivalent number of days."
  (should (equal "-30d"
                 (shipit-atlassian-dashboard--normalize-date-offset "-1M")))
  (should (equal "-90d"
                 (shipit-atlassian-dashboard--normalize-date-offset "-3M")))
  (should (equal "-180d"
                 (shipit-atlassian-dashboard--normalize-date-offset "-6M")))
  (should (equal "-365d"
                 (shipit-atlassian-dashboard--normalize-date-offset "-1y")))
  (should (equal "30d"
                 (shipit-atlassian-dashboard--normalize-date-offset "1M"))))

(ert-deftest test-atlassian-dashboard-creates-buffer ()
  "GIVEN a Jira config
WHEN opening the dashboard
THEN a buffer in shipit-atlassian-dashboard-mode is created."
  (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
             (lambda (_repo)
               (cons '(:name "Jira"
                       :fetch-project-statuses ignore)
                     '(:base-url "https://test.atlassian.net"))))
            ((symbol-function 'shipit-atlassian-dashboard--refresh-data)
             (lambda () nil)))
    (let ((buf (shipit-atlassian-dashboard--open "test/repo")))
      (unwind-protect
          (with-current-buffer buf
            (should (eq major-mode 'shipit-atlassian-dashboard-mode))
            (should (equal shipit-atlassian-dashboard--repo "test/repo")))
        (kill-buffer buf)))))

(ert-deftest test-atlassian-dashboard-refuses-non-jira-repo ()
  "GIVEN a repo whose backend is not Jira-capable
WHEN opening the dashboard
THEN a user-error signals with a helpful message."
  (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
             (lambda (_repo)
               ;; Simulates the GitHub issue backend falling through:
               ;; no :fetch-project-statuses, no :base-url in config.
               (cons '(:name "GitHub") '(:repo "owner/name")))))
    (should-error (shipit-atlassian-dashboard--open "owner/name")
                  :type 'user-error)))

(ert-deftest test-atlassian-dashboard-my-issues-jql ()
  "GIVEN a Jira config with project-keys
WHEN building my-issues JQL
THEN JQL filters by assignee, unresolved, and project."
  (let ((config '(:base-url "https://test.atlassian.net"
                  :project-keys ("PROJ1" "PROJ2"))))
    (let ((jql (shipit-atlassian-dashboard--my-issues-jql config)))
      (should (string-match-p "assignee=currentUser()" jql))
      (should (string-match-p "resolution=Unresolved" jql))
      (should (string-match-p "project in (\"PROJ1\",\"PROJ2\")" jql)))))

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

(ert-deftest test-atlassian-dashboard-issue-is-magit-section ()
  "GIVEN an inserted issue line inside a dashboard root
WHEN searching for a magit-section of type `atlassian-issue'
THEN a section is found with the issue key as its value."
  (with-temp-buffer
    (let* ((magit-insert-section--parent nil)
           (issue '((id . "PROJ-42") (title . "Fix login") (state . "Open")))
           (columns '(work status))
           (widths '((key . 12) (summary . 40) (status . 10))))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-issue-line issue columns widths))
      (let ((found nil)
            (pos (point-min)))
        (while (and (< pos (point-max)) (not found))
          (let ((s (get-text-property pos 'magit-section)))
            (when (and s (eq (oref s type) 'atlassian-issue))
              (setq found s)))
          (setq pos (1+ pos)))
        (should found)
        (should (equal "PROJ-42" (oref found value)))))))

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
          (shipit-atlassian-board-ids '(42)))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-whats-next-section))
      (let ((text (buffer-string)))
        (should (string-match-p "What's Next" text))
        (should (string-match-p "PROJ-10" text))))))

(ert-deftest test-atlassian-dashboard-whats-next-no-board ()
  "GIVEN no board IDs configured
WHEN rendering what's next section
THEN a configuration message is shown."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-board-ids nil)
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
          (shipit-atlassian-board-ids '(42))
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
        (shipit-atlassian-board-ids '(42)))
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
  "GIVEN no board IDs configured
WHEN fetching what's next items
THEN nil is returned without any API calls."
  (let ((shipit-atlassian-board-ids nil)
        (api-called nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config _path) (setq api-called t) nil)))
      (should (null (shipit-atlassian-dashboard--fetch-whats-next)))
      (should (null api-called)))))

(ert-deftest test-atlassian-dashboard-fetch-whats-next-multiple-boards ()
  "GIVEN two configured boards with different active sprints
  AND one issue appearing in both sprints
WHEN fetching what's next items
THEN items from both sprints are aggregated in board order
     AND the duplicate issue appears only once."
  (let ((shipit-atlassian-dashboard--config
         '(:base-url "https://test.atlassian.net"))
        (shipit-atlassian-board-ids '(42 57)))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
               (lambda (_config path)
                 (cond
                  ((string-match-p "board/42/sprint" path)
                   '((values . [((id . 100) (name . "Sprint A") (state . "active"))])))
                  ((string-match-p "board/57/sprint" path)
                   '((values . [((id . 200) (name . "Sprint B") (state . "active"))])))
                  ((string-match-p "sprint/100/issue" path)
                   '((issues . [((key . "PROJ-1")) ((key . "PROJ-2"))])))
                  ((string-match-p "sprint/200/issue" path)
                   '((issues . [((key . "PROJ-2")) ((key . "PROJ-3"))])))
                  (t nil))))
              ((symbol-function 'shipit-issue-jira--normalize-issue)
               (lambda (data)
                 `((id . ,(cdr (assq 'key data)))))))
      (let ((result (shipit-atlassian-dashboard--fetch-whats-next)))
        (should (equal '("PROJ-1" "PROJ-2" "PROJ-3")
                       (mapcar (lambda (i) (cdr (assq 'id i))) result)))))))

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
    (should (string-match-p "maxResults=100" path))
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

(ert-deftest test-atlassian-board-group-resolves-empty-column-via-extra-map ()
  "GIVEN production-shape board columns where status objects expose only `{id, self}'
  AND one column (`Review & Test') has no visible issue, so the issue-derived
  id->name map can\\='t supply its status name
WHEN `--group-by-column' runs with EXTRA-STATUS-NAMES from
     `--fetch-status-names' (the instance status catalogue)
THEN the empty column still advertises a usable status name --
     so `--board-target-status' can stage `In Progress' -> `Review & Test'
     forward without falling back to `No forward column'."
  (let ((columns '(((name . "To Do")
                    (statuses . [((self . "x") (id . "1"))]))
                   ((name . "In Progress")
                    (statuses . [((self . "x") (id . "3"))]))
                   ((name . "Review & Test")
                    (statuses . [((self . "x") (id . "5"))]))
                   ((name . "Done")
                    (statuses . [((self . "x") (id . "6"))]))))
        (issues '(((id . "P-2") (state . "In Progress") (state-id . "3"))))
        (extra '(("1" . "To Do")
                 ("3" . "In Progress")
                 ("5" . "Review & Test")
                 ("6" . "Done"))))
    (let* ((grouped (shipit-atlassian-board--group-by-column columns issues extra))
           (review (cl-find-if (lambda (c)
                                 (equal (cdr (assq 'name c)) "Review & Test"))
                               grouped))
           (todo (cl-find-if (lambda (c)
                               (equal (cdr (assq 'name c)) "To Do"))
                             grouped)))
      (should review)
      (should (equal '("Review & Test") (cdr (assq 'statuses review))))
      (should todo)
      (should (equal '("To Do") (cdr (assq 'statuses todo)))))))

(ert-deftest test-atlassian-board-group-issue-name-wins-over-extra ()
  "GIVEN production-shape columns AND an issue carrying status `In Progress'
  AND EXTRA-STATUS-NAMES naming the same id `Different Name'
WHEN `--group-by-column' merges the two maps
THEN the issue-derived name wins (live state is canonical) so the
     existing `--column-for-status' lookup keeps matching against
     the same string the issue currently displays."
  (let ((columns '(((name . "In Progress")
                    (statuses . [((self . "x") (id . "3"))]))))
        (issues '(((id . "P-2") (state . "In Progress") (state-id . "3"))))
        (extra '(("3" . "Different Name"))))
    (let* ((grouped (shipit-atlassian-board--group-by-column columns issues extra)))
      (should (equal '("In Progress")
                     (cdr (assq 'statuses (car grouped))))))))

(ert-deftest test-atlassian-board-fetch-all ()
  "GIVEN mock board configuration and issues API responses
WHEN fetching all configured boards
THEN a board alist with id, name, and grouped columns is returned."
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config path)
               (cond
                ((string-match-p "configuration" path)
                 '((name . "Team Board")
                   (columnConfig . ((columns . [((name . "To Do")
                                                 (statuses . [((id . "1"))]))
                                                ((name . "Done")
                                                 (statuses . [((id . "4"))]))])))))
                ((string-match-p "/rest/api/2/status" path)
                 [((id . "1") (name . "Open"))
                  ((id . "4") (name . "Done"))])
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
           (boards (shipit-atlassian-board--fetch-all config '(42)))
           (board (car boards))
           (cols (cdr (assq 'columns board))))
      (should (= 1 (length boards)))
      (should (= 42 (cdr (assq 'id board))))
      (should (equal "Team Board" (cdr (assq 'name board))))
      (should (= 2 (length cols)))
      (should (equal "To Do" (cdr (assq 'name (nth 0 cols)))))
      (should (= 1 (length (cdr (assq 'issues (nth 0 cols))))))
      (should (equal "P-1" (cdr (assq 'id (car (cdr (assq 'issues (nth 0 cols))))))))
      (should (equal "Done" (cdr (assq 'name (nth 1 cols)))))
      (should (= 0 (length (cdr (assq 'issues (nth 1 cols)))))))))

(ert-deftest test-atlassian-board-fetch-all-multiple-boards ()
  "GIVEN two configured board IDs with distinct configurations
WHEN fetching all boards
THEN both boards are returned in configured order with their own
     names and columns, falling back to a generated name when the
     configuration response carries no name."
  (cl-letf (((symbol-function 'shipit-issue-jira--api-request)
             (lambda (_config path)
               (cond
                ((string-match-p "board/42/configuration" path)
                 '((name . "Alpha")
                   (columnConfig . ((columns . [((name . "Open")
                                                 (statuses . [((id . "1"))]))])))))
                ((string-match-p "board/57/configuration" path)
                 '((columnConfig . ((columns . [((name . "Triage")
                                                 (statuses . [((id . "2"))]))])))))
                ((string-match-p "/rest/api/2/status" path)
                 [((id . "1") (name . "Open"))
                  ((id . "2") (name . "Triage"))])
                ((string-match-p "board/42/issue" path)
                 '((total . 1)
                   (issues . [((key . "A-1")
                               (fields . ((summary . "A task")
                                          (status . ((name . "Open") (id . "1"))))))])))
                ((string-match-p "board/57/issue" path)
                 '((total . 0) (issues . [])))
                (t nil))))
            ((symbol-function 'shipit-issue-jira--normalize-issue)
             (lambda (data)
               (let ((fields (cdr (assq 'fields data))))
                 `((id . ,(cdr (assq 'key data)))
                   (state . ,(cdr (assq 'name (cdr (assq 'status fields)))))
                   (state-id . ,(cdr (assq 'id (cdr (assq 'status fields))))))))))
    (let* ((config '(:base-url "https://test.atlassian.net"))
           (boards (shipit-atlassian-board--fetch-all config '(42 57))))
      (should (= 2 (length boards)))
      (should (= 42 (cdr (assq 'id (nth 0 boards)))))
      (should (equal "Alpha" (cdr (assq 'name (nth 0 boards)))))
      (should (= 57 (cdr (assq 'id (nth 1 boards)))))
      (should (equal "Board 57" (cdr (assq 'name (nth 1 boards)))))
      (should (= 1 (length (cdr (assq 'issues
                                      (car (cdr (assq 'columns (nth 0 boards))))))))))))

(ert-deftest test-atlassian-boards-render-columns ()
  "GIVEN grouped data for one board
WHEN rendering the Boards section
THEN the Boards heading, board name, column headers, and issue keys appear."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--backend-plist
           (cdr (assq 'jira shipit-issue-backends)))
          (shipit-atlassian-dashboard--board-data
           '(((id . 42)
              (name . "Team Board")
              (columns . (((name . "To Do")
                           (issues . (((id . "P-1") (title . "Fix bug") (state . "To Do")))))
                          ((name . "Done")
                           (issues . (((id . "P-2") (title . "Add tests") (state . "Done"))))))))))
          (shipit-atlassian-board-ids '(42)))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-boards-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Boards" text))
        (should (string-match-p "Team Board" text))
        (should (string-match-p "To Do" text))
        (should (string-match-p "P-1" text))
        (should (string-match-p "Done" text))
        (should (string-match-p "P-2" text))))))

(ert-deftest test-atlassian-boards-render-multiple-boards ()
  "GIVEN grouped data for two boards
WHEN rendering the Boards section
THEN each board renders as its own atlassian-board section carrying
     the board ID as section value, with its own columns."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-dashboard--backend-plist
           (cdr (assq 'jira shipit-issue-backends)))
          (shipit-atlassian-dashboard--board-data
           '(((id . 42)
              (name . "Alpha")
              (columns . (((name . "Open")
                           (issues . (((id . "A-1") (title . "A task") (state . "Open"))))))))
             ((id . 57)
              (name . "Beta")
              (columns . (((name . "Triage")
                           (issues . (((id . "B-1") (title . "B task") (state . "Triage"))))))))))
          (shipit-atlassian-board-ids '(42 57)))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-boards-section))
      (let ((text (buffer-string))
            (board-values nil)
            (pos (point-min)))
        (should (string-match-p "Alpha" text))
        (should (string-match-p "A-1" text))
        (should (string-match-p "Beta" text))
        (should (string-match-p "B-1" text))
        (while (< pos (point-max))
          (let ((s (get-text-property pos 'magit-section)))
            (when (and s (eq (oref s type) 'atlassian-board)
                       (not (memq (oref s value) board-values)))
              (push (oref s value) board-values)))
          (setq pos (1+ pos)))
        (should (equal '(42 57) (nreverse board-values)))))))

(ert-deftest test-atlassian-boards-render-no-board-ids ()
  "GIVEN no board IDs configured
WHEN rendering the Boards section
THEN a configuration message is shown."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-board-ids nil)
          (shipit-atlassian-dashboard--board-data nil))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-boards-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Boards" text))
        (should (string-match-p "Press RET to configure" text))
        ;; THEN the configure line has the text property for RET handling
        (goto-char (point-min))
        (search-forward "Press RET")
        (should (eq 'board-ids (get-text-property (point) 'shipit-atlassian-configure)))))))

(ert-deftest test-atlassian-boards-render-no-data ()
  "GIVEN board IDs but no board data
WHEN rendering the Boards section
THEN 'No board data' message is shown."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-atlassian-board-ids '(42))
          (shipit-atlassian-dashboard--board-data nil))
      (magit-insert-section (atlassian-dashboard)
        (shipit-atlassian-dashboard--insert-boards-section))
      (let ((text (buffer-string)))
        (should (string-match-p "Boards" text))
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
          '(((id . 42) (name . "Team Board")
             (columns . (((name . "To Do")
                          (issues . (((id . "P-3") (title . "X") (state . "To Do"))))))))))
    (setq shipit-atlassian-board-ids '(42))
    (shipit-atlassian-dashboard--render)
    (let ((text (buffer-string)))
      (should (string-match-p "Atlassian Dashboard" text))
      (should (string-match-p "What's Next" text))
      (should (string-match-p "My Open Issues" text))
      (should (string-match-p "Boards" text))
      (should (string-match-p "Team Board" text))
      (should (string-match-p "Frequently Visited" text)))))

(ert-deftest test-atlassian-dashboard-refresh-data-fetches-and-renders ()
  "GIVEN mock fetch functions
WHEN refreshing dashboard data
THEN all data is fetched and render is called."
  (with-temp-buffer
    (shipit-atlassian-dashboard-mode)
    (setq shipit-atlassian-dashboard--repo "test/repo")
    (setq shipit-atlassian-dashboard--config '(:base-url "https://test.atlassian.net"))
    (setq shipit-atlassian-board-ids '(42))
    (let ((render-called nil)
          (shipit-atlassian-dashboard-defer-fetches nil))
      (cl-letf (((symbol-function 'shipit-atlassian-dashboard--fetch-my-issues)
                 (lambda () '(((id . "P-1") (title . "Bug") (state . "Open")))))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-whats-next)
                 (lambda () '(((id . "P-2") (title . "Task") (state . "To Do")))))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-boards)
                 (lambda () '(((id . 42) (name . "B")
                               (columns . (((name . "Col") (issues . nil))))))))
                ((symbol-function 'shipit-atlassian-dashboard--render)
                 (lambda () (setq render-called t))))
        (shipit-atlassian-dashboard--refresh-data)
        (should render-called)
        (should (equal '(((id . "P-1") (title . "Bug") (state . "Open")))
                       shipit-atlassian-dashboard--my-issues))
        (should (equal '(((id . "P-2") (title . "Task") (state . "To Do")))
                       shipit-atlassian-dashboard--whats-next))
        (should (equal '(((id . 42) (name . "B")
                          (columns . (((name . "Col") (issues . nil))))))
                       shipit-atlassian-dashboard--board-data))))))

(ert-deftest test-atlassian-dashboard-refresh-data-handles-fetch-error ()
  "GIVEN a fetch function that errors
WHEN refreshing dashboard data
THEN partial data is kept and render is still called."
  (with-temp-buffer
    (shipit-atlassian-dashboard-mode)
    (setq shipit-atlassian-dashboard--repo "test/repo")
    (setq shipit-atlassian-dashboard--config '(:base-url "https://test.atlassian.net"))
    (setq shipit-atlassian-board-ids nil)
    (let ((render-called nil)
          (shipit-atlassian-dashboard-defer-fetches nil))
      (cl-letf (((symbol-function 'shipit-atlassian-dashboard--fetch-my-issues)
                 (lambda () (error "API timeout")))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-whats-next)
                 (lambda () nil))
                ((symbol-function 'shipit-atlassian-dashboard--fetch-boards)
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

(ert-deftest test-atlassian-dashboard-parse-board-ids-input ()
  "GIVEN comma- or space-separated board IDs and URLs
WHEN parsing the multi-board input
THEN all numeric board IDs are extracted in order; unparsable
     tokens are dropped and all-garbage input yields nil."
  (should (equal '(42 57)
                 (shipit-atlassian-dashboard--parse-board-ids-input "42, 57")))
  (should (equal '(42 7)
                 (shipit-atlassian-dashboard--parse-board-ids-input
                  "42 https://foo.atlassian.net/jira/software/projects/X/boards/7")))
  (should (equal '(99)
                 (shipit-atlassian-dashboard--parse-board-ids-input "hello,99")))
  (should-not (shipit-atlassian-dashboard--parse-board-ids-input "hello"))
  (should-not (shipit-atlassian-dashboard--parse-board-ids-input "")))

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
                  atlassian-whats-next atlassian-boards atlassian-board
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
        (should (eq 'success (get-text-property (+ (point-min) pos) 'font-lock-face)))))))

;;; Issues section — JQL builder

(ert-deftest test-atlassian-issues-jql-project-only ()
  "GIVEN an empty filter plist and a config with project-keys
WHEN building Issues JQL
THEN only the project scope + default ORDER BY are present."
  (let* ((config '(:project-keys ("PROJ1" "PROJ2")))
         (jql (shipit-atlassian-dashboard--build-issues-jql config nil)))
    (should (string-match-p "project in (\"PROJ1\",\"PROJ2\")" jql))
    (should (string-match-p "ORDER BY key DESC" jql))
    (should-not (string-match-p "assignee" jql))
    (should-not (string-match-p "resolution" jql))))

(ert-deftest test-atlassian-issues-jql-custom-sort ()
  "GIVEN a filter with :sort
WHEN building JQL
THEN ORDER BY uses the custom sort expression and the default is suppressed."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:sort "created ASC"))))
    (should (string-match-p "ORDER BY created ASC" jql))
    (should-not (string-match-p "key DESC" jql))))

(ert-deftest test-atlassian-issues-jql-assignee-me ()
  "GIVEN a filter with :assignee 'me
WHEN building JQL
THEN the clause \"assignee=currentUser()\" is included."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql config '(:assignee me))))
    (should (string-match-p "assignee=currentUser()" jql))))

(ert-deftest test-atlassian-issues-jql-assignee-unassigned ()
  "GIVEN a filter with :assignee 'unassigned
WHEN building JQL
THEN the clause \"assignee is EMPTY\" is included."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:assignee unassigned))))
    (should (string-match-p "assignee is EMPTY" jql))))

(ert-deftest test-atlassian-issues-jql-assignee-literal ()
  "GIVEN a filter with :assignee set to a free-text username
WHEN building JQL
THEN assignee is compared by literal quoted value."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:assignee "alice"))))
    (should (string-match-p "assignee=\"alice\"" jql))))

(ert-deftest test-atlassian-issues-jql-reporter-type-priority ()
  "GIVEN :reporter, :type, :priority filters
WHEN building JQL
THEN each appears as its own clause."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:reporter me :type "Bug" :priority "High"))))
    (should (string-match-p "reporter=currentUser()" jql))
    (should (string-match-p "issuetype=\"Bug\"" jql))
    (should (string-match-p "priority=\"High\"" jql))))

(ert-deftest test-atlassian-issues-jql-text-search ()
  "GIVEN :text filter
WHEN building JQL
THEN a text clause with quoted value is included."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:text "flaky tests"))))
    (should (string-match-p "text ~ \"flaky tests\"" jql))))

(ert-deftest test-atlassian-issues-jql-comment-text ()
  "GIVEN :comment-text filter
WHEN building JQL
THEN the comment ~ \"value\" clause is included."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:comment-text "regression"))))
    (should (string-match-p "comment ~ \"regression\"" jql))))

(ert-deftest test-atlassian-issues-jql-has-comments ()
  "GIVEN :has-comments t filter
WHEN building JQL
THEN the OR-pair workaround for any comment is included."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:has-comments t))))
    (should (string-match-p "comment ~ \"anything\\*\"" jql))
    (should (string-match-p "comment !~ \"anything\\*\"" jql))))

(ert-deftest test-atlassian-issues-jql-status-and-resolution ()
  "GIVEN status and resolution filters
WHEN building JQL
THEN both appear as quoted clauses."
  (let* ((config '(:project-keys ("P")))
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config '(:status "In Progress" :resolution "Unresolved"))))
    (should (string-match-p "status=\"In Progress\"" jql))
    (should (string-match-p "resolution=\"Unresolved\"" jql))))

;;; Issues section — dispatcher

(ert-deftest test-atlassian-dispatch-section-type-finds-issues ()
  "GIVEN point is inside a child of atlassian-issues
WHEN shipit-atlassian-dashboard--top-section-type is called
THEN it returns 'atlassian-issues."
  (cl-letf* ((fake-issues-child
              (list :type 'atlassian-issues-project-group
                    :parent (list :type 'atlassian-issues
                                  :parent (list :type 'atlassian-dashboard
                                                :parent nil))))
             ((symbol-function 'magit-current-section)
              (lambda () fake-issues-child))
             ((symbol-function 'eieio-oref)
              (lambda (obj slot)
                (pcase slot
                  ('type (plist-get obj :type))
                  ('parent (plist-get obj :parent))))))
    (should (eq 'atlassian-issues
                (shipit-atlassian-dashboard--top-section-type)))))

(ert-deftest test-atlassian-dispatch-section-type-root-is-nil ()
  "GIVEN point is on the root atlassian-dashboard section itself
WHEN shipit-atlassian-dashboard--top-section-type is called
THEN it returns nil (no top-level section applies)."
  (cl-letf* ((root (list :type 'atlassian-dashboard :parent nil))
             ((symbol-function 'magit-current-section)
              (lambda () root))
             ((symbol-function 'eieio-oref)
              (lambda (obj slot)
                (pcase slot
                  ('type (plist-get obj :type))
                  ('parent (plist-get obj :parent))))))
    (should (null (shipit-atlassian-dashboard--top-section-type)))))

(ert-deftest test-atlassian-dispatch-filter-routes-to-issues-menu ()
  "GIVEN point is inside atlassian-issues
WHEN shipit-atlassian-dashboard-filter-dispatch runs
THEN shipit-atlassian-dashboard-issues-filter-menu is invoked."
  (let ((called nil))
    (cl-letf (((symbol-function 'shipit-atlassian-dashboard--top-section-type)
               (lambda () 'atlassian-issues))
              ((symbol-function 'shipit-atlassian-dashboard-issues-filter-menu)
               (lambda () (interactive) (setq called t))))
      (call-interactively #'shipit-atlassian-dashboard-filter-dispatch)
      (should called))))

(ert-deftest test-atlassian-dispatch-filter-reports-unsupported-section ()
  "GIVEN point is inside a section with no filter handler
WHEN shipit-atlassian-dashboard-filter-dispatch runs
THEN a message is emitted and no transient is launched."
  (let ((msg nil))
    (cl-letf (((symbol-function 'shipit-atlassian-dashboard--top-section-type)
               (lambda () 'atlassian-my-issues))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (call-interactively #'shipit-atlassian-dashboard-filter-dispatch)
      (should msg)
      (should (string-match-p "No filter" msg)))))

;;; Issues section — filter summary and transient round-trip

(ert-deftest test-atlassian-filter-summary-empty ()
  "GIVEN no filter
WHEN rendering the summary
THEN the result is an empty string."
  (should (equal "" (shipit-atlassian-dashboard--filter-summary nil))))

(ert-deftest test-atlassian-filter-summary-multi ()
  "GIVEN a filter with assignee=me and type=Bug
WHEN rendering the summary
THEN the result lists both conditions inside brackets."
  (let ((s (shipit-atlassian-dashboard--filter-summary
            '(:assignee me :type "Bug"))))
    (should (string-match-p "\\[" s))
    (should (string-match-p "assignee=me" s))
    (should (string-match-p "type=Bug" s))))

(ert-deftest test-atlassian-filter-to-transient-args-roundtrip ()
  "GIVEN a filter plist
WHEN converting to transient args and back
THEN the filter round-trips (except key order)."
  (let* ((original '(:assignee me :type "Bug" :text "flaky"
                     :comment-text "regression" :sort "created DESC"))
         (args (shipit-atlassian-dashboard--filter-to-transient-args original))
         (back (shipit-atlassian-dashboard--filter-from-args args)))
    (should (equal 'me (plist-get back :assignee)))
    (should (equal "Bug" (plist-get back :type)))
    (should (equal "flaky" (plist-get back :text)))
    (should (equal "regression" (plist-get back :comment-text)))
    (should (equal "created DESC" (plist-get back :sort)))))

(ert-deftest test-atlassian-filter-to-transient-args-unassigned ()
  "GIVEN a filter with assignee 'unassigned
WHEN converting to transient args
THEN the arg uses the literal \"unassigned\"."
  (let ((args (shipit-atlassian-dashboard--filter-to-transient-args
               '(:assignee unassigned))))
    (should (member "--assignee=unassigned" args))))

(ert-deftest test-atlassian-dashboard-pending-arrivals-into-column ()
  "GIVEN pending transitions targeting various status columns
WHEN `shipit-atlassian-dashboard--pending-arrivals-into-column\=' is
     queried for a column of the board the issues live on
THEN issues whose target matches the column (case-insensitive)
     are returned with their issue alists."
  (let* ((board '((id . 1) (name . "Alpha")
                  (columns . (((name . "In Progress")
                               (issues . (((key . "PROJ-1") (status . "In Progress"))
                                          ((key . "PROJ-3") (status . "In Progress")))))
                              ((name . "Done")
                               (issues . (((key . "PROJ-2") (status . "Done")))))))))
         (shipit-atlassian-dashboard--pending-transitions
          '(("PROJ-1" . "In Review")
            ("PROJ-2" . "Done")
            ("PROJ-3" . "In Review")))
         (shipit-atlassian-dashboard--board-data (list board)))
    (let ((arrivals (shipit-atlassian-dashboard--pending-arrivals-into-column
                     board "in review")))
      (should (= (length arrivals) 2))
      (should (cl-some (lambda (a) (equal "PROJ-1" (cdr (assq 'key a)))) arrivals))
      (should (cl-some (lambda (a) (equal "PROJ-3" (cdr (assq 'key a)))) arrivals)))
    ;; A column with no pending arrivals returns nil.
    (should (null (shipit-atlassian-dashboard--pending-arrivals-into-column
                   board "Backlog")))))

(ert-deftest test-atlassian-dashboard-pending-arrivals-scoped-to-board ()
  "GIVEN two boards with identically named columns
  AND a pending transition for an issue that lives only on the first board
WHEN querying arrivals for the same-named column on each board
THEN only the board that displays the issue reports the arrival."
  (let* ((board-a '((id . 1) (name . "Alpha")
                    (columns . (((name . "Open")
                                 (statuses . ("Open"))
                                 (issues . (((key . "PROJ-1") (status . "Open")))))
                                ((name . "Code Review")
                                 (statuses . ("In Review"))
                                 (issues . ()))))))
         (board-b '((id . 2) (name . "Beta")
                    (columns . (((name . "Code Review")
                                 (statuses . ("In Review"))
                                 (issues . ()))))))
         (shipit-atlassian-dashboard--pending-transitions
          '(("PROJ-1" . "In Review")))
         (shipit-atlassian-dashboard--board-data (list board-a board-b)))
    (should (= 1 (length (shipit-atlassian-dashboard--pending-arrivals-into-column
                          board-a "Code Review"))))
    (should (null (shipit-atlassian-dashboard--pending-arrivals-into-column
                   board-b "Code Review")))))

(ert-deftest test-atlassian-dashboard-pending-arrivals-routes-via-status-mapping ()
  "GIVEN a pending transition whose target name doesn\='t equal any column name
WHEN `shipit-atlassian-dashboard--pending-arrivals-into-column\=' is queried
     for the column whose `statuses\=' include the target
THEN the issue is included as an arrival -- routing happens
     through the column\='s status list, not by literal
     name match."
  (let* ((board '((id . 1) (name . "Alpha")
                  (columns . (((name . "Open")
                               (statuses . ("Open"))
                               (issues . (((id . "PROJ-1") (number . "PROJ-1") (state . "Open")))))
                              ((name . "Code Review")
                               (statuses . ("In Review"))
                               (issues . ()))))))
         (shipit-atlassian-dashboard--pending-transitions
          '(("PROJ-1" . "In Review")))
         (shipit-atlassian-dashboard--board-data (list board)))
    (let ((arrivals (shipit-atlassian-dashboard--pending-arrivals-into-column
                     board "Code Review")))
      (should (= (length arrivals) 1))
      (should (equal (cdr (assq 'number (car arrivals))) "PROJ-1")))))

(ert-deftest test-atlassian-dashboard-dedup-issues-by-key ()
  "GIVEN issues where the same key appears twice (different case)
WHEN deduplicating by work-item key
THEN the first occurrence wins and order is preserved."
  (let ((issues '(((id . "PROJ-1") (title . "First"))
                  ((id . "PROJ-2") (title . "Second"))
                  ((id . "proj-1") (title . "Duplicate")))))
    (let ((result (shipit-atlassian-dashboard--dedup-issues-by-key issues)))
      (should (= 2 (length result)))
      (should (equal "First" (cdr (assq 'title (nth 0 result)))))
      (should (equal "Second" (cdr (assq 'title (nth 1 result))))))))

(ert-deftest test-atlassian-dashboard-draw-board-refuses-without-the-canvas ()
  "GIVEN an Emacs that cannot draw a canvas
WHEN the dashboard is asked to draw the board at point
THEN it says so and the board stays as text, rather than failing obscurely."
  (cl-letf (((symbol-function 'shipit-canvas-available-p) (lambda () nil)))
    (let ((raised (condition-case err
                      (shipit-atlassian-dashboard-draw-board)
                    (error (error-message-string err)))))
      (should (string-match-p "Emacs 32" raised))
      (should (string-match-p "stays as text" raised)))))

(defmacro test-atlassian--in-a-rendered-board (&rest body)
  "Render one board section in a temp buffer and run BODY with point on its
heading."
  `(with-temp-buffer
     (let ((magit-insert-section--parent nil)
           (shipit-atlassian-dashboard--backend-plist
            (cdr (assq 'jira shipit-issue-backends)))
           (shipit-atlassian-dashboard--board-data
            '(((id . 42) (name . "Firmware")
               (columns . (((name . "Backlog")
                            (issues . (((id . "F-1") (title . "A task") (state . "Open"))))))))))
           (shipit-atlassian-board-ids '(42)))
       (magit-insert-section (atlassian-dashboard)
         (shipit-atlassian-dashboard--insert-boards-section))
       (goto-char (or (text-property-any (point-min) (point-max) 'shipit-atlassian-board-heading t)
                      (progn (goto-char (point-min))
                             (search-forward "Firmware")
                             (line-beginning-position))))
       ,@body)))

(ert-deftest test-atlassian-dashboard-ret-on-a-board-draws-it ()
  "GIVEN point on a board heading and an Emacs that can draw a canvas
WHEN RET is pressed
THEN the board opens on a canvas, because TAB already folds the section."
  (let ((drawn nil))
    (cl-letf (((symbol-function 'shipit-atlassian-dashboard--canvas-available-p)
               (lambda () t))
              ((symbol-function 'shipit-atlassian-kanban)
               (lambda (board-id) (setq drawn board-id))))
      (test-atlassian--in-a-rendered-board
       (shipit-atlassian-dashboard--ret-action)
       (should (equal 42 drawn))))))

(ert-deftest test-atlassian-dashboard-ret-on-a-board-folds-without-a-canvas ()
  "GIVEN an Emacs that cannot draw a canvas
WHEN RET is pressed on a board heading
THEN it folds the section as it always did, rather than raising."
  (cl-letf (((symbol-function 'shipit-atlassian-dashboard--canvas-available-p)
             (lambda () nil)))
    (test-atlassian--in-a-rendered-board
     (let ((before (magit-section-hidden (magit-current-section))))
       (shipit-atlassian-dashboard--ret-action)
       (should-not (eq before (magit-section-hidden (magit-current-section))))))))

(ert-deftest test-atlassian-dashboard-ret-inside-a-board-still-folds ()
  "GIVEN point on a column inside a board, not on the board heading
WHEN RET is pressed
THEN the column folds; only the board heading itself draws a canvas."
  (let ((drawn nil))
    (cl-letf (((symbol-function 'shipit-atlassian-dashboard--canvas-available-p)
               (lambda () t))
              ((symbol-function 'shipit-atlassian-kanban)
               (lambda (board-id) (setq drawn board-id))))
      (test-atlassian--in-a-rendered-board
       (goto-char (point-min))
       (search-forward "Backlog")
       (goto-char (line-beginning-position))
       (shipit-atlassian-dashboard--ret-action)
       (should-not drawn)))))

(provide 'test-shipit-atlassian-dashboard)
;;; test-shipit-atlassian-dashboard.el ends here
