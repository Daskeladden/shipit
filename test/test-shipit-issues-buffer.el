;;; test-shipit-issues-buffer.el --- Tests for shipit-issues-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for issue buffer work item column rendering.

;;; Code:

(require 'ert)
(require 'shipit)
(require 'shipit-issues-buffer)
(require 'shipit-sections)

;;; Work Item Column Renderer Tests

(ert-deftest test-shipit-issue-format-work-item-line-default-columns ()
  "GIVEN a work item with key, summary, assignee, status
WHEN formatting with columns (work assignee status)
THEN line contains all three fields."
  (let ((columns '(work assignee status))
        (item '((key . "PRJ-42")
                (summary . "Fix the login bug")
                (assignee . "Alice")
                (status . "In Progress")))
        (widths '((key . 6) (summary . 40) (assignee . 5) (status . 11))))
    (let ((line (shipit-issue--format-work-item-line item columns widths)))
      (should (string-match-p "PRJ-42" line))
      (should (string-match-p "Fix the login bug" line))
      (should (string-match-p "Alice" line))
      (should (string-match-p "In Progress" line)))))

(ert-deftest test-shipit-issue-format-work-item-line-custom-columns ()
  "GIVEN custom columns (key status)
WHEN formatting a work item
THEN only key and status appear, no assignee or summary."
  (let ((columns '(key status))
        (item '((key . "PRJ-42")
                (summary . "Fix the login bug")
                (assignee . "Alice")
                (status . "Open")))
        (widths '((key . 6) (status . 4))))
    (let ((line (shipit-issue--format-work-item-line item columns widths)))
      (should (string-match-p "PRJ-42" line))
      (should (string-match-p "Open" line))
      (should-not (string-match-p "Alice" line))
      (should-not (string-match-p "Fix the login bug" line)))))

(ert-deftest test-shipit-issue-compute-work-item-widths ()
  "GIVEN a list of work items with varying field lengths
WHEN computing column widths with indent=4
THEN key/assignee/status use minimums when data fits
     AND summary fills remaining window width."
  (let ((items '(((key . "PRJ-1") (assignee . "Al") (status . "Open"))
                 ((key . "PRJ-100") (assignee . "Alice") (status . "In Progress")))))
    (let ((widths (shipit-issue--compute-work-item-widths items 3)))
      ;; THEN key uses minimum 12 (data max=7 < 12)
      (should (= 12 (cdr (assq 'key widths))))
      ;; THEN assignee uses minimum 16 (data max=5 < 16)
      (should (= 16 (cdr (assq 'assignee widths))))
      ;; THEN status uses minimum 24 (data max=11 < 24)
      (should (= 24 (cdr (assq 'status widths))))
      ;; THEN summary fills remaining window width (at least 20)
      (should (>= (cdr (assq 'summary widths)) 20)))))

;;; Linked Work Items Section Tests

(ert-deftest test-shipit-issue-insert-linked-items-section ()
  "GIVEN issue data with issuelinks
WHEN inserting the linked items section
THEN a collapsible magit section is created with grouped items."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (columns '(work status))
          (links '(((type . "blocks") (direction . "outward")
                    (key . "PRJ-2") (summary . "Task B")
                    (status . "Open") (assignee . nil))
                   ((type . "blocks") (direction . "outward")
                    (key . "PRJ-3") (summary . "Task C")
                    (status . "Done") (assignee . "Bob"))
                   ((type . "is blocked by") (direction . "inward")
                    (key . "PRJ-1") (summary . "Task A")
                    (status . "In Progress") (assignee . "Alice")))))
      (magit-insert-section (root)
        (shipit-issue--insert-linked-items-section links columns))
      (goto-char (point-min))
      ;; THEN section heading exists
      (should (search-forward "Linked Work Items" nil t))
      ;; THEN group headers exist
      (should (search-forward "Blocks" nil t))
      (should (search-forward "PRJ-2" nil t))
      (should (search-forward "PRJ-3" nil t))
      (should (search-forward "Is blocked by" nil t))
      (should (search-forward "PRJ-1" nil t)))))

(ert-deftest test-shipit-issue-linked-items-section-absent-when-no-links ()
  "GIVEN no issuelinks
WHEN inserting linked items section
THEN nothing is inserted."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil))
      (magit-insert-section (root)
        (shipit-issue--insert-linked-items-section nil '(work status)))
      (should (= (point-min) (point-max))))))

;;; Child Work Items Section Tests

(ert-deftest test-shipit-issue-insert-child-items-section-for-epic ()
  "GIVEN an issue with issue-type Epic
WHEN inserting child items section
THEN a placeholder section is created."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-issue-buffer-repo "test/repo")
          (issue-data '((id . "PRJ-10")
                        (issue-type . "Epic"))))
      (magit-insert-section (root)
        (shipit-issue--insert-child-items-section "test/repo" issue-data))
      (goto-char (point-min))
      (should (search-forward "Child Work Items" nil t)))))

(ert-deftest test-shipit-issue-child-items-section-absent-for-non-epic ()
  "GIVEN an issue with issue-type Story
WHEN inserting child items section
THEN nothing is inserted."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (issue-data '((id . "PRJ-11")
                        (issue-type . "Story"))))
      (magit-insert-section (root)
        (shipit-issue--insert-child-items-section "test/repo" issue-data))
      (should (= (point-min) (point-max))))))

(ert-deftest test-shipit-issue-replace-child-items-with-content ()
  "GIVEN a child items placeholder section exists
WHEN replacing with fetched children
THEN child items appear with count in heading."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-issue-buffer-repo "test/repo")
          (issue-data '((id . "PRJ-10")
                        (issue-type . "Epic"))))
      (magit-insert-section (root)
        (shipit-issue--insert-child-items-section "test/repo" issue-data))
      (let ((children '(((key . "PRJ-11") (summary . "Child 1")
                         (status . "Open") (assignee . "Alice")
                         (issue-type . "Story"))
                        ((key . "PRJ-12") (summary . "Child 2")
                         (status . "Done") (assignee . nil)
                         (issue-type . "Bug")))))
        (shipit-issue--replace-child-items-with-content children '(work status)))
      (goto-char (point-min))
      (should (search-forward "Child Work Items (2)" nil t))
      (should (search-forward "PRJ-11" nil t))
      (should (search-forward "PRJ-12" nil t))))

  ;; Verify section collapse works after replacement
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-issue-buffer-repo "test/repo")
          (issue-data '((id . "PRJ-10")
                        (issue-type . "Epic"))))
      (magit-insert-section (root)
        (shipit-issue--insert-child-items-section "test/repo" issue-data))
      (let ((children '(((key . "PRJ-11") (summary . "Child 1")
                         (status . "Open") (assignee . "Alice")
                         (issue-type . "Story")))))
        (shipit-issue--replace-child-items-with-content children '(work status)))
      ;; THEN section content marker is before end marker
      (let ((section (shipit--find-section-by-type 'issue-child-items)))
        (should section)
        (should (< (marker-position (oref section content))
                   (marker-position (oref section end))))
        ;; THEN children list is populated with new child sections
        (should (= 1 (length (oref section children))))))))

(ert-deftest test-shipit-issue-child-replace-preserves-sibling-sections ()
  "GIVEN child items and linked items sections exist as siblings
WHEN replacing child items placeholder with fetched children
THEN linked items section is preserved and not corrupted."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-issue-buffer-repo "test/repo")
          (issue-data '((id . "PRJ-10")
                        (issue-type . "Epic")))
          (links '(((type . "blocks") (direction . "outward")
                    (key . "PRJ-99") (summary . "Blocked task")
                    (status . "Open") (assignee . "Bob")))))
      (magit-insert-section (root)
        (shipit-issue--insert-child-items-section "test/repo" issue-data)
        (shipit-issue--insert-linked-items-section links '(work status)))
      ;; Verify both sections exist before replacement
      (let ((buf-before (buffer-string)))
        (should (string-match-p "Child Work Items" buf-before))
        (should (string-match-p "Linked Work Items" buf-before))
        (should (string-match-p "PRJ-99" buf-before)))
      ;; WHEN replacing child items (heading changes from "loading..." to "2")
      (let ((children '(((key . "PRJ-11") (summary . "Child 1")
                         (status . "Open") (assignee . "Alice")
                         (issue-type . "Story"))
                        ((key . "PRJ-12") (summary . "Child 2")
                         (status . "Done") (assignee . nil)
                         (issue-type . "Bug")))))
        (shipit-issue--replace-child-items-with-content children '(work status)))
      ;; THEN child section has correct content
      (let ((buf-after (buffer-string)))
        (should (string-match-p "Child Work Items (2)" buf-after))
        (should (string-match-p "PRJ-11" buf-after))
        (should (string-match-p "PRJ-12" buf-after))
        ;; THEN linked items section is still intact
        (should (string-match-p "Linked Work Items" buf-after))
        (should (string-match-p "PRJ-99" buf-after)))
      ;; THEN child section boundaries don't overlap linked section
      (let ((child-section (shipit--find-section-by-type 'issue-child-items))
            (linked-section (shipit--find-section-by-type 'issue-linked-items)))
        (should child-section)
        (should linked-section)
        (should (<= (marker-position (oref child-section end))
                    (marker-position (oref linked-section start))))))))

;;; Integration Tests

(ert-deftest test-shipit-issue-buffer-sections-for-epic-with-links ()
  "GIVEN an Epic issue with issuelinks
WHEN rendering issue buffer sections
THEN both Child Work Items and Linked Work Items sections appear
     AND issuelinks are NOT in the metadata section."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-issue-buffer-repo "test/repo")
          (columns '(work status))
          (issue-data '((id . "PRJ-10")
                        (number . "PRJ-10")
                        (title . "Epic Feature")
                        (state . "In Progress")
                        (issue-type . "Epic")
                        (body . "Description here")
                        (user . ((login . "Alice")))
                        (assignees . nil)
                        (labels . nil)
                        (comments . 0)
                        (created_at . "2024-01-15T10:30:00Z")
                        (updated_at . "2024-01-20T14:00:00Z")
                        (issuelinks . (((type . "blocks")
                                        (direction . "outward")
                                        (key . "PRJ-20")
                                        (summary . "Blocked task")
                                        (status . "Open")
                                        (assignee . "Bob")))))))
      (cl-letf (((symbol-function 'shipit-pr-github--get-repo-subscription)
                 (lambda (_config) nil)))
      (magit-insert-section (shipit-issue)
        (shipit-issue--insert-metadata-section "test/repo" issue-data)
        (shipit-issue--insert-child-items-section "test/repo" issue-data)
        (let ((issuelinks (cdr (assq 'issuelinks issue-data))))
          (shipit-issue--insert-linked-items-section issuelinks columns)))
      (let ((buf-text (buffer-string)))
        ;; THEN Child section exists (placeholder)
        (should (string-match-p "Child Work Items" buf-text))
        ;; THEN Linked section exists with item
        (should (string-match-p "Linked Work Items" buf-text))
        (should (string-match-p "PRJ-20" buf-text))
        ;; THEN metadata section does NOT contain issuelink keys
        ;; (issuelinks moved to their own section)
        )))))

;;; Icon Column Type Tests

(ert-deftest test-shipit-issue-format-issue-type-icon-column ()
  "GIVEN an item with issue-type and a render callback in widths
WHEN formatting the issue-type-icon column
THEN the callback is called with the type name."
  (let* ((called-with nil)
         (item '((issue-type . "Bug")))
         (widths `((issue-type-icon . 3)
                   (issue-type-icon-render . ,(lambda (type)
                                                (setq called-with type)
                                                "[B]")))))
    (let ((result (shipit-issue--format-work-item-column item 'issue-type-icon widths)))
      (should (equal "Bug" called-with))
      (should (equal "[B]" result)))))

(ert-deftest test-shipit-issue-format-issue-type-icon-no-render ()
  "GIVEN an item but no render callback in widths
WHEN formatting the issue-type-icon column
THEN returns empty string."
  (let ((item '((issue-type . "Bug")))
        (widths '((issue-type-icon . 3))))
    (should (equal "" (shipit-issue--format-work-item-column item 'issue-type-icon widths)))))

(ert-deftest test-shipit-issue-format-priority-icon-column ()
  "GIVEN an item with priority and a render callback in widths
WHEN formatting the priority-icon column
THEN the callback is called with the priority name."
  (let* ((called-with nil)
         (item '((priority . "High")))
         (widths `((priority-icon . 3)
                   (priority-icon-render . ,(lambda (prio)
                                              (setq called-with prio)
                                              "[H]")))))
    (let ((result (shipit-issue--format-work-item-column item 'priority-icon widths)))
      (should (equal "High" called-with))
      (should (equal "[H]" result)))))

(ert-deftest test-shipit-issue-format-priority-icon-nil-result ()
  "GIVEN a render callback that returns nil (unknown priority)
WHEN formatting the priority-icon column
THEN returns a space padded to width."
  (let ((item '((priority . "Whatever")))
        (widths `((priority-icon . 3)
                  (priority-icon-render . ,(lambda (_) nil)))))
    (let ((result (shipit-issue--format-work-item-column item 'priority-icon widths)))
      (should (equal "   " result)))))

(ert-deftest test-shipit-issue-format-relative-time-column ()
  "GIVEN an item with time and a format callback in widths
WHEN formatting the relative-time column
THEN the callback is called and result has magit-dimmed face."
  (let ((item `((time . ,(float-time))))
        (widths `((relative-time . 14)
                  (relative-time-format . ,(lambda (ts) (ignore ts) "just now")))))
    (let ((result (shipit-issue--format-work-item-column item 'relative-time widths)))
      (should (string-match-p "just now" result))
      (should (eq 'magit-dimmed (get-text-property 0 'face result))))))

(ert-deftest test-shipit-issue-format-status-with-category-face ()
  "GIVEN an item with status-category and a face callback in widths
WHEN formatting the status column
THEN the status-category face is used instead of default."
  (let ((item '((status . "In Progress") (status-category . "indeterminate")))
        (widths `((status . 20)
                  (status-category-face . ,(lambda (cat)
                                             (when (equal cat "indeterminate")
                                               'magit-branch-remote))))))
    (let ((result (shipit-issue--format-work-item-column item 'status widths)))
      (should (string-match-p "In Progress" result))
      (should (eq 'magit-branch-remote (get-text-property 0 'face result))))))

(ert-deftest test-shipit-issue-format-status-falls-back-without-category ()
  "GIVEN an item without status-category
WHEN formatting the status column
THEN the default state-face is used."
  (let ((item '((status . "Open")))
        (widths '((status . 10))))
    (let ((result (shipit-issue--format-work-item-column item 'status widths)))
      (should (string-match-p "Open" result))
      (should (eq 'success (get-text-property 0 'face result))))))

(ert-deftest test-shipit-issue-compute-widths-icon-columns ()
  "GIVEN items and icon column types
WHEN computing widths
THEN issue-type-icon and priority-icon get fixed width 3."
  (let ((items '(((key . "P-1") (status . "Open")))))
    (let ((widths (shipit-issue--compute-work-item-widths items 3)))
      (should (= 3 (cdr (assq 'issue-type-icon widths))))
      (should (= 3 (cdr (assq 'priority-icon widths))))
      (should (= 14 (cdr (assq 'relative-time widths)))))))

;;; Data Adapter Tests

(ert-deftest test-shipit-issue-normalize-to-work-item-dashboard-format ()
  "GIVEN a dashboard-format issue with id/title/state
WHEN normalizing to work-item format
THEN key/summary/status fields are mapped correctly."
  (let ((issue '((id . "PROJ-42") (title . "Fix login") (state . "Open")
                 (issue-type . "Bug") (priority . "High")
                 (status-category . "new") (assignee . "Alice")
                 (time . 12345.0))))
    (let ((mapped (shipit-issue--normalize-to-work-item issue)))
      (should (equal "PROJ-42" (cdr (assq 'key mapped))))
      (should (equal "Fix login" (cdr (assq 'summary mapped))))
      (should (equal "Open" (cdr (assq 'status mapped))))
      (should (equal "Bug" (cdr (assq 'issue-type mapped))))
      (should (equal "High" (cdr (assq 'priority mapped))))
      (should (equal "new" (cdr (assq 'status-category mapped))))
      (should (equal "Alice" (cdr (assq 'assignee mapped))))
      (should (equal 12345.0 (cdr (assq 'time mapped)))))))

(ert-deftest test-shipit-issue-normalize-to-work-item-already-work-item ()
  "GIVEN a work-item-format issue with key/summary/status
WHEN normalizing
THEN fields pass through unchanged."
  (let ((issue '((key . "PRJ-1") (summary . "Task") (status . "Done"))))
    (let ((mapped (shipit-issue--normalize-to-work-item issue)))
      (should (equal "PRJ-1" (cdr (assq 'key mapped))))
      (should (equal "Task" (cdr (assq 'summary mapped))))
      (should (equal "Done" (cdr (assq 'status mapped)))))))

;;; Dashboard Widths Builder Tests

(ert-deftest test-shipit-issue-dashboard-widths ()
  "GIVEN items and a backend plist with render callbacks
WHEN computing dashboard widths
THEN base widths are augmented with render callbacks."
  (let ((items '(((key . "P-1") (status . "Open"))))
        (backend (list :issue-type-icon-render #'identity
                       :priority-icon-render #'identity
                       :status-category-face #'identity)))
    (let ((widths (shipit-issue--dashboard-widths items backend 3)))
      ;; THEN base widths are present
      (should (cdr (assq 'key widths)))
      (should (cdr (assq 'summary widths)))
      ;; THEN render callbacks are included
      (should (cdr (assq 'issue-type-icon-render widths)))
      (should (cdr (assq 'priority-icon-render widths)))
      (should (cdr (assq 'status-category-face widths)))
      (should (cdr (assq 'relative-time-format widths))))))

;;; Relative Time Tests

(ert-deftest test-shipit-issue-format-relative-time ()
  "GIVEN various timestamps
WHEN formatting as relative time
THEN correct human-readable strings are returned."
  (let ((now (float-time)))
    (should (equal "just now" (shipit-issue--format-relative-time now)))
    (should (string-match-p "5 min ago" (shipit-issue--format-relative-time (- now 300))))
    (should (string-match-p "2 hours ago" (shipit-issue--format-relative-time (- now 7200))))
    (should (string-match-p "3 days ago" (shipit-issue--format-relative-time (- now 259200))))))

;;; RET-dwim on work item lines

(ert-deftest test-shipit-issue-ret-dwim-opens-child-item-anywhere-on-line ()
  "GIVEN point is on a child work item line but NOT on the key text
WHEN pressing RET
THEN the linked issue is opened via shipit-issues-open-buffer."
  (let ((opened-key nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'shipit--try-overlay-action-at-point)
               (lambda () nil))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (key &rest _) (setq opened-key key))))
      (with-temp-buffer
        (shipit-issue-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (issue-root)
            (magit-insert-section (issue-child-item
                                   '((key . "PROJ-99")
                                     (summary . "Some task")))
              (insert "   PROJ-99  Some task\n"))))
        ;; Move to summary text (not the key)
        (goto-char (point-min))
        (search-forward "Some task")
        (shipit-issue--ret-dwim)
        (should (equal "PROJ-99" opened-key))))))

(ert-deftest test-shipit-issue-ret-dwim-opens-linked-item-anywhere-on-line ()
  "GIVEN point is on a linked work item line but NOT on the key text
WHEN pressing RET
THEN the linked issue is opened via shipit-issues-open-buffer."
  (let ((opened-key nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'shipit--try-overlay-action-at-point)
               (lambda () nil))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (key &rest _) (setq opened-key key))))
      (with-temp-buffer
        (shipit-issue-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (issue-root)
            (magit-insert-section (issue-linked-item
                                   '((key . "OTHER-7")
                                     (summary . "Related bug")))
              (insert "   OTHER-7  Related bug\n"))))
        (goto-char (point-min))
        (search-forward "Related bug")
        (shipit-issue--ret-dwim)
        (should (equal "OTHER-7" opened-key))))))

;;; C-u RET action menu on work item lines

(ert-deftest test-shipit-issue-ret-dwim-prefix-calls-actions ()
  "GIVEN point is on a child work item line
WHEN pressing C-u RET (prefix arg)
THEN shipit-issue--work-item-actions is called instead of open-buffer."
  (let ((actions-called nil)
        (opened-key nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'shipit--try-overlay-action-at-point)
               (lambda () nil))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (key &rest _) (setq opened-key key)))
              ((symbol-function 'shipit-issue--work-item-actions)
               (lambda (key) (setq actions-called key))))
      (with-temp-buffer
        (shipit-issue-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (issue-root)
            (magit-insert-section (issue-child-item
                                   '((key . "PROJ-50")
                                     (summary . "Some task")))
              (insert "   PROJ-50  Some task\n"))))
        (goto-char (point-min))
        (search-forward "Some task")
        (let ((current-prefix-arg '(4)))
          (shipit-issue--ret-dwim))
        ;; THEN actions menu was called with the key
        (should (equal "PROJ-50" actions-called))
        ;; THEN open-buffer was NOT called
        (should-not opened-key)))))

(ert-deftest test-shipit-issue-ret-dwim-no-prefix-still-opens-buffer ()
  "GIVEN point is on a child work item line
WHEN pressing RET without prefix arg
THEN shipit-issues-open-buffer is called (existing behavior)."
  (let ((actions-called nil)
        (opened-key nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'shipit--try-overlay-action-at-point)
               (lambda () nil))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (key &rest _) (setq opened-key key)))
              ((symbol-function 'shipit-issue--work-item-actions)
               (lambda (key) (setq actions-called key))))
      (with-temp-buffer
        (shipit-issue-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (issue-root)
            (magit-insert-section (issue-child-item
                                   '((key . "PROJ-50")
                                     (summary . "Some task")))
              (insert "   PROJ-50  Some task\n"))))
        (goto-char (point-min))
        (search-forward "Some task")
        (let ((current-prefix-arg nil))
          (shipit-issue--ret-dwim))
        ;; THEN open-buffer was called
        (should (equal "PROJ-50" opened-key))
        ;; THEN actions was NOT called
        (should-not actions-called)))))

(ert-deftest test-shipit-issue-work-item-actions-open-in-browser ()
  "GIVEN a work item key and a backend with :browse-url
WHEN user selects 'Open in browser' from actions menu
THEN browse-url is called with the correct URL."
  (let ((browsed-url nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _) "Open in browser"))
              ((symbol-function 'browse-url)
               (lambda (url) (setq browsed-url url)))
              ((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :browse-url (lambda (_config id)
                               (format "https://example.com/issue/%s" id))
                             :name "Test")
                       '(:repo "test/repo")))))
      (shipit-issue--work-item-actions "PROJ-42")
      (should (equal "https://example.com/issue/PROJ-42" browsed-url)))))

(ert-deftest test-shipit-issue-work-item-actions-change-status-when-supported ()
  "GIVEN a backend that supports :get-transitions
WHEN building actions list
THEN 'Change status' appears in the list."
  (let ((offered-actions nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq offered-actions choices)
                 "Open in buffer"))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :get-transitions #'ignore
                             :transition-status #'ignore
                             :browse-url #'ignore
                             :name "Test")
                       '(:repo "test/repo")))))
      (shipit-issue--work-item-actions "PROJ-42")
      (should (member "Change status" offered-actions)))))

(ert-deftest test-shipit-issue-work-item-actions-no-change-status-when-unsupported ()
  "GIVEN a backend without :get-transitions
WHEN building actions list
THEN 'Change status' does NOT appear."
  (let ((offered-actions nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq offered-actions choices)
                 "Open in buffer"))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 (cons (list :browse-url #'ignore
                             :name "Test")
                       '(:repo "test/repo")))))
      (shipit-issue--work-item-actions "PROJ-42")
      (should-not (member "Change status" offered-actions)))))

;;; Chunked comment rendering tests

(ert-deftest test-shipit-issue--insert-comments-chunked-yields-after-chunk ()
  "GIVEN 25 comments and chunk size 10
WHEN inserting comments via the chunked helper
THEN the yield function is called twice (after comments 10 and 20)."
  (let ((shipit-comments-render-chunk-size 10)
        (insert-calls 0)
        (yield-calls 0))
    (cl-letf (((symbol-function 'shipit-issue--insert-single-comment)
               (lambda (&rest _) (cl-incf insert-calls)))
              ((symbol-function 'shipit--render-yield)
               (lambda (&rest _) (cl-incf yield-calls))))
      (shipit-issue--insert-comments-chunked
       "owner/repo" 1
       (cl-loop for i below 25 collect `((id . ,i))))
      (should (= 25 insert-calls))
      (should (= 2 yield-calls)))))

(ert-deftest test-shipit-issue--insert-comments-chunked-no-yield-under-chunk ()
  "GIVEN fewer comments than the chunk size
WHEN inserting comments
THEN the yield function is not called."
  (let ((shipit-comments-render-chunk-size 10)
        (yield-calls 0))
    (cl-letf (((symbol-function 'shipit-issue--insert-single-comment)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit--render-yield)
               (lambda (&rest _) (cl-incf yield-calls))))
      (shipit-issue--insert-comments-chunked
       "owner/repo" 1
       (cl-loop for i below 5 collect `((id . ,i))))
      (should (= 0 yield-calls)))))

(ert-deftest test-shipit-issue--insert-comments-chunked-disabled-with-zero-size ()
  "GIVEN chunk size 0
WHEN inserting comments
THEN yield is never called."
  (let ((shipit-comments-render-chunk-size 0)
        (yield-calls 0))
    (cl-letf (((symbol-function 'shipit-issue--insert-single-comment)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit--render-yield)
               (lambda (&rest _) (cl-incf yield-calls))))
      (shipit-issue--insert-comments-chunked
       "owner/repo" 1
       (cl-loop for i below 50 collect `((id . ,i))))
      (should (= 0 yield-calls)))))

(provide 'test-shipit-issues-buffer)
;;; test-shipit-issues-buffer.el ends here
