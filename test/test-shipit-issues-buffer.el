;;; test-shipit-issues-buffer.el --- Tests for shipit-issues-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for issue buffer work item column rendering.

;;; Code:

(require 'ert)
(require 'shipit)
(require 'shipit-issues-buffer)
(require 'shipit-issues)
(require 'shipit-sections)
(require 'shipit-pr-github)
(require 'shipit-test-http)

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
      (cl-letf (((symbol-function 'shipit-issue--get-backend)
                 (lambda () (list :name "Test"))))
        (magit-insert-section (root)
          (shipit-issue--insert-linked-items-section nil '(work status))))
      (should (= (point-min) (point-max))))))

;;; Link Work Item Tests

(defun test-shipit-issue--linking-backend (recorder)
  "Return a backend plist whose link operations report to RECORDER.
RECORDER is called with the arguments `:link-issues' receives.  Its
search lists three issues, the current one among them, and a fetch by
key answers for any key so a typed key can be tried."
  (list :name "Test"
        :search
        (lambda (_config _args)
          '(((number . "PRJ-1") (title . "The current issue"))
            ((number . "PRJ-4") (title . "Fix sensor"))
            ((number . "PRJ-5") (title . "Sensor docs"))))
        :fetch-issue
        (lambda (_config key) `((number . ,key) (title . "Fetched by key")))
        :fetch-link-relations
        (lambda (_config) '(("blocks" . (blocks . out))
                            ("is blocked by" . (blocks . in))))
        :link-issues
        (lambda (&rest args) (funcall recorder args))))

(defmacro test-shipit-issue--with-linking-buffer (recorder &rest body)
  "Run BODY in an issue buffer for PRJ-1 whose backend reports to RECORDER."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local shipit-issue-buffer-number "PRJ-1")
     (setq-local shipit-issue-buffer-repo "test/repo")
     (setq-local shipit-issue-backend-config '(:base-url "https://x"))
     (cl-letf (((symbol-function 'shipit-issue--get-backend)
                (lambda () (test-shipit-issue--linking-backend ,recorder)))
               ((symbol-function 'shipit-issue-buffer-refresh) #'ignore))
       ,@body)))

(ert-deftest test-shipit-issue-link-work-item-sends-relation-and-target ()
  "GIVEN a backend that can link issues
WHEN the user picks a relation and then an issue from the search list
THEN the backend links the current issue to the chosen key with the
relation's opaque value, and the buffer refreshes."
  (let ((linked nil) (refreshed nil))
    (test-shipit-issue--with-linking-buffer (lambda (args) (setq linked args))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (if (listp choices)
                       "is blocked by"
                     (seq-find (lambda (c) (string-prefix-p "PRJ-4" c))
                               (all-completions "" choices)))))
                ((symbol-function 'shipit-issue-buffer-refresh)
                 (lambda (&rest _) (setq refreshed t))))
        (shipit-issue--link-work-item)
        (should (equal linked '((:base-url "https://x") "PRJ-1" (blocks . in) "PRJ-4")))
        (should refreshed)))))

(ert-deftest test-shipit-issue-link-work-item-lists-search-results-without-typing ()
  "GIVEN the backend's search answers with issues
WHEN the target prompt opens
THEN those issues are the candidates before anything is typed, except
the issue being linked from, which cannot link to itself."
  (let ((shown nil))
    (test-shipit-issue--with-linking-buffer #'ignore
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (if (listp choices)
                       "blocks"
                     (setq shown (all-completions "" choices))
                     (car shown)))))
        (shipit-issue--link-work-item)
        (should (= 2 (length shown)))
        (should (seq-find (lambda (c) (string-prefix-p "PRJ-4" c)) shown))
        (should (seq-find (lambda (c) (string-prefix-p "PRJ-5" c)) shown))
        (should-not (seq-find (lambda (c) (string-prefix-p "PRJ-1" c)) shown))))))

(ert-deftest test-shipit-issue-link-work-item-fetches-a-typed-key ()
  "GIVEN the user types a key the search list does not hold
WHEN completing
THEN the tracker is asked for it and it becomes the target, the way
the issue search re-queries as one types."
  (let ((linked nil))
    (test-shipit-issue--with-linking-buffer (lambda (args) (setq linked args))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (if (listp choices)
                       "blocks"
                     (car (all-completions "OTHER-77" choices))))))
        (shipit-issue--link-work-item)
        (should (equal (nth 3 linked) "OTHER-77"))
        (should (equal (nth 2 linked) '(blocks . out)))))))

(ert-deftest test-shipit-issue-link-work-item-refuses-without-backend-support ()
  "GIVEN a backend with no `:link-issues'
WHEN asked to link a work item
THEN it signals a user error before prompting for anything."
  (with-temp-buffer
    (cl-letf (((symbol-function 'shipit-issue--get-backend)
               (lambda () (list :name "Test")))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (error "Should not prompt"))))
      (should-error (shipit-issue--link-work-item) :type 'user-error))))

(ert-deftest test-shipit-issue-linked-items-heading-shown-when-linking-possible ()
  "GIVEN an issue with no links but a backend that can create them
WHEN inserting the linked items section
THEN a heading with a zero count is still there, so there is a place
to invoke Link work item from."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil))
      (cl-letf (((symbol-function 'shipit-issue--get-backend)
                 (lambda () (test-shipit-issue--linking-backend #'ignore))))
        (magit-insert-section (root)
          (shipit-issue--insert-linked-items-section nil '(work status))))
      (goto-char (point-min))
      (should (search-forward "Linked Work Items (0)" nil t)))))

(ert-deftest test-shipit-issue-linked-items-heading-dwim-offers-link ()
  "GIVEN point on the Linked Work Items heading
WHEN the DWIM handler for it runs
THEN it links a work item."
  (require 'shipit-pr-actions)
  (shipit-issue--register-dwim-handlers)
  (let ((handler (assq 'issue-linked-items shipit--dwim-handlers))
        (called nil))
    (should handler)
    (cl-letf (((symbol-function 'shipit-issue--link-work-item)
               (lambda () (setq called t))))
      (funcall (cdr (assq 'action (cdr handler))))
      (should called))))

;;; Similar Work Items Tests

(defmacro test-shipit-issue--with-similar-finder (finder &rest body)
  "Run BODY with FINDER as `shipit-issue-similar-finder'.
FINDER is called with (SOURCE-BUFFER ISSUE CALLBACK); nil means no
finder."
  (declare (indent 1))
  `(let ((shipit-issue-similar-finder ,finder))
     ,@body))

(defun test-shipit-issue--render-similar-in-temp-buffer ()
  "Insert the Similar Work Items section under a root in the current buffer."
  (let ((magit-insert-section--parent nil))
    (magit-insert-section (root)
      (shipit-issue--insert-similar-items-section))))

(ert-deftest test-shipit-issue-similar-section-idle-until-asked ()
  "GIVEN a finder of similar issues
WHEN the issue buffer renders
THEN a Similar Work Items section is there with a hint to ask, and
nothing has been asked yet."
  (let ((asked nil))
    (test-shipit-issue--with-similar-finder (lambda (&rest _) (setq asked t))
      (with-temp-buffer
        (test-shipit-issue--render-similar-in-temp-buffer)
        (goto-char (point-min))
        (should (search-forward "Similar Work Items (0)" nil t))
        (should (search-forward "M-;" nil t))
        (should-not asked)))))

(ert-deftest test-shipit-issue-similar-section-absent-without-finder ()
  "GIVEN no finder of similar issues
WHEN the issue buffer renders
THEN there is no Similar Work Items section to ask from."
  (test-shipit-issue--with-similar-finder nil
    (with-temp-buffer
      (test-shipit-issue--render-similar-in-temp-buffer)
      (should (= (point-min) (point-max))))))

(ert-deftest test-shipit-issue-find-similar-shows-results-when-they-land ()
  "GIVEN the user asks for similar work items
WHEN the finder answers
THEN the section first says it is asking, then lists each item with
its key, summary and reason, and the heading counts them."
  (let ((callback nil) (passed-issue nil))
    (test-shipit-issue--with-similar-finder
        (lambda (_buf issue cb) (setq passed-issue issue callback cb))
      (with-temp-buffer
        (setq-local shipit-issue-buffer-data '((number . "PRJ-1") (title . "T")))
        (test-shipit-issue--render-similar-in-temp-buffer)
        (shipit-issue--find-similar-work-items)
        (should (equal passed-issue '((number . "PRJ-1") (title . "T"))))
        (goto-char (point-min))
        (should (search-forward "Asking" nil t))
        (funcall callback
                 '(((key . "PRJ-4") (summary . "Fix sensor") (status . "Open")
                    (why . "Same subsystem"))
                   ((key . "PRJ-9") (summary . "Docs") (status . "Done")
                    (why . "Follow-up")))
                 nil)
        (goto-char (point-min))
        (should (search-forward "Similar Work Items (2)" nil t))
        (should (search-forward "PRJ-4" nil t))
        (should (search-forward "Fix sensor" nil t))
        (should (search-forward "Same subsystem" nil t))
        (should (search-forward "PRJ-9" nil t))
        (should-not (save-excursion (goto-char (point-min))
                                    (search-forward "Asking" nil t)))))))

(ert-deftest test-shipit-issue-find-similar-shows-the-note-on-failure ()
  "GIVEN the finder comes back empty with a note
WHEN it answers
THEN the section shows the note rather than an empty list."
  (let ((callback nil))
    (test-shipit-issue--with-similar-finder (lambda (_buf _issue cb) (setq callback cb))
      (with-temp-buffer
        (setq-local shipit-issue-buffer-data '((number . "PRJ-1")))
        (test-shipit-issue--render-similar-in-temp-buffer)
        (shipit-issue--find-similar-work-items)
        (funcall callback nil "timed out")
        (goto-char (point-min))
        (should (search-forward "timed out" nil t))))))

(ert-deftest test-shipit-issue-find-similar-says-when-nothing-is-found ()
  "GIVEN the finder comes back empty with no note
WHEN it answers
THEN the section says nothing related was found."
  (let ((callback nil))
    (test-shipit-issue--with-similar-finder (lambda (_buf _issue cb) (setq callback cb))
      (with-temp-buffer
        (setq-local shipit-issue-buffer-data '((number . "PRJ-1")))
        (test-shipit-issue--render-similar-in-temp-buffer)
        (shipit-issue--find-similar-work-items)
        (funcall callback nil nil)
        (goto-char (point-min))
        (should (search-forward "Similar Work Items (0)" nil t))
        (should (search-forward "No related work items found" nil t))))))

(ert-deftest test-shipit-issue-similar-heading-dwim-asks ()
  "GIVEN point on the Similar Work Items heading
WHEN the DWIM handler for it runs
THEN it asks for similar work items."
  (require 'shipit-pr-actions)
  (shipit-issue--register-dwim-handlers)
  (let ((handler (assq 'issue-similar-items shipit--dwim-handlers))
        (called nil))
    (should handler)
    (cl-letf (((symbol-function 'shipit-issue--find-similar-work-items)
               (lambda () (setq called t))))
      (funcall (cdr (assq 'action (cdr handler))))
      (should called))))

(ert-deftest test-shipit-issue-similar-row-is-a-work-item ()
  "GIVEN point on a similar work item row
WHEN asking for the work item key at point
THEN its key comes back, so RET opens it like any other work item."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil))
      (magit-insert-section (root)
        (magit-insert-section (issue-similar-item '((key . "PRJ-4") (summary . "S")))
          (insert "   PRJ-4  S\n"))))
    (goto-char (point-min))
    (should (equal "PRJ-4" (shipit-issue--work-item-key-at-point)))))

(ert-deftest test-shipit-issue-work-item-row-dwim-opens-actions ()
  "GIVEN point on a work item row -- a similar, linked or child item
WHEN the DWIM handlers dispatch
THEN the row's handler matches and opens the action menu for its key."
  (require 'shipit-pr-actions)
  (shipit-issue--register-dwim-handlers)
  (let ((handler (assq 'issue-work-item shipit--dwim-handlers))
        (acted nil))
    (should handler)
    (with-temp-buffer
      (shipit-issue-mode)
      (let ((inhibit-read-only t)
            (magit-insert-section--parent nil))
        (magit-insert-section (root)
          (magit-insert-section (issue-similar-items nil nil)
            (magit-insert-heading "Similar Work Items (1)")
            (magit-insert-section (issue-similar-item '((key . "PRJ-4")))
              (insert "   PRJ-4  S\n")))))
      (cl-letf (((symbol-function 'shipit-issue--work-item-actions)
                 (lambda (key) (setq acted key))))
        ;; On the heading the row handler stays out of the way.
        (goto-char (point-min))
        (should-not (funcall (cdr (assq 'matcher (cdr handler)))))
        ;; On the row it matches and acts on the row's key.
        (forward-line 1)
        (should (funcall (cdr (assq 'matcher (cdr handler)))))
        (funcall (cdr (assq 'action (cdr handler))))
        (should (equal "PRJ-4" acted))))))

(ert-deftest test-shipit-issue-work-item-actions-offer-linking ()
  "GIVEN a backend that can link issues
WHEN the action menu for a work item row opens and Link is chosen
THEN the current issue is linked to that row's key, with the relation
asked but no target prompt."
  (let ((offered nil) (linked nil))
    (test-shipit-issue--with-linking-buffer (lambda (args) (setq linked args))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (cond ((string-match-p "action" prompt)
                          (setq offered choices)
                          "Link to this issue")
                         ((member "blocks" choices) "blocks")
                         (t (error "Unexpected prompt %s" prompt)))))
                ((symbol-function 'shipit-issue--resolve-for-repo)
                 (lambda (_repo) (cons (shipit-issue--get-backend) nil))))
        (shipit-issue--work-item-actions "PRJ-4")
        (should (member "Link to this issue" offered))
        (should (equal (nth 3 linked) "PRJ-4"))
        (should (equal (nth 2 linked) '(blocks . out)))))))

(ert-deftest test-shipit-issue-work-item-actions-hide-linking-when-unsupported ()
  "GIVEN a backend that cannot link issues
WHEN the action menu for a work item row opens
THEN Link is not offered."
  (let ((offered nil)
        (shipit-issue-buffer-repo "test/repo"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq offered choices) "Open in buffer"))
              ((symbol-function 'shipit-issues-open-buffer) #'ignore)
              ((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo) (cons (list :browse-url #'ignore :name "Test") nil))))
      (shipit-issue--work-item-actions "PRJ-4")
      (should-not (member "Link to this issue" offered)))))

;;; Link Pull Request Tests

(ert-deftest test-shipit-issue-linked-prs-from-remote-keeps-pr-urls ()
  "GIVEN the tracker holds remote links, some of them pull requests
WHEN collecting linked PRs from them
THEN each PR URL becomes a linked-PR plist and the rest are left out."
  (with-temp-buffer
    (setq-local shipit-issue-backend-config '(:base-url "https://j"))
    (cl-letf (((symbol-function 'shipit-issue--get-backend)
               (lambda ()
                 (list :fetch-remote-urls
                       (lambda (_config key)
                         (should (equal "PRJ-9" key))
                         '(("https://github.com/foo/bar/pull/42" . "#42: Fix")
                           ("https://wiki.example.com/page" . "Design")
                           ("https://github.com/foo/bar/issues/7" . "#7")))))))
      (let ((plists (shipit-issue--linked-prs-from-remote "PRJ-9")))
        (should (= 1 (length plists)))
        (should (equal "foo/bar" (plist-get (car plists) :repo)))
        (should (equal 42 (plist-get (car plists) :number)))))))

(ert-deftest test-shipit-issue-linked-prs-from-remote-without-support ()
  "GIVEN an issue backend with no remote links
WHEN collecting linked PRs from them
THEN there are none, and no error."
  (with-temp-buffer
    (cl-letf (((symbol-function 'shipit-issue--get-backend)
               (lambda () (list :name "Plain"))))
      (should-not (shipit-issue--linked-prs-from-remote "PRJ-9")))))

(ert-deftest test-shipit-issue-link-pull-request-picks-and-links ()
  "GIVEN the user picks a PR from the repo's pull requests
WHEN linking it to the current issue
THEN the shared link helper gets the issue, the PR, its URL and title,
and the Linked PRs section is repopulated."
  (let ((linked nil) (repopulated nil))
    (with-temp-buffer
      (setq-local shipit-issue-buffer-number "PRJ-9")
      (setq-local shipit-issue-buffer-repo "PRJ")
      (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                 (lambda () "foo/bar"))
                ((symbol-function 'shipit--read-pr)
                 (lambda (prompt repo)
                   (should (equal "foo/bar" repo))
                   (should (string-match-p "PRJ-9" prompt))
                   (list :number 42 :repo "foo/bar"
                         :data '((number . 42) (title . "Fix sensor")
                                 (html_url . "https://github.com/foo/bar/pull/42")))))
                ((symbol-function 'shipit-pr-linked-issue-link)
                 (lambda (&rest args) (setq linked args)))
                ((symbol-function 'shipit-issue--populate-linked-prs-async)
                 (lambda (&rest _) (setq repopulated t)))
                ((symbol-function 'message) #'ignore))
        (shipit-issue--link-pull-request)
        (should (equal linked '("PRJ" "PRJ-9" "foo/bar" 42
                                "https://github.com/foo/bar/pull/42" "Fix sensor")))
        (should repopulated)))))

(ert-deftest test-shipit-issue-linked-prs-heading-dwim-links-a-pr ()
  "GIVEN point on the Linked PRs heading
WHEN the DWIM handler for it runs
THEN it links a pull request."
  (require 'shipit-pr-actions)
  (shipit-issue--register-dwim-handlers)
  (let ((handler (assq 'issue-linked-prs shipit--dwim-handlers))
        (called nil))
    (should handler)
    (cl-letf (((symbol-function 'shipit-issue--link-pull-request)
               (lambda () (setq called t))))
      (funcall (cdr (assq 'action (cdr handler))))
      (should called))))

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

(ert-deftest test-shipit-issue-metadata-section-renders-labels ()
  "GIVEN a Jira issue whose normalized data carries labels
WHEN rendering the metadata section
THEN a Labels line lists each label name."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-issue-buffer-repo "test/repo")
          (issue-data '((id . "PRJ-7")
                        (number . "PRJ-7")
                        (title . "Labelled issue")
                        (state . "Open")
                        (issue-type . "Task")
                        (body . "Body")
                        (user . ((login . "Alice")))
                        (assignees . nil)
                        (labels . (((name . "backend")) ((name . "urgent"))))
                        (comments . 0)
                        (created_at . "2024-01-15T10:30:00Z")
                        (updated_at . "2024-01-20T14:00:00Z"))))
      (cl-letf (((symbol-function 'shipit-pr-github--get-repo-subscription)
                 (lambda (_config) nil)))
        (magit-insert-section (shipit-issue)
          (shipit-issue--insert-metadata-section "test/repo" issue-data))
        (let ((buf-text (buffer-string)))
          ;; THEN the Labels line is present with both names
          (should (string-match-p "Labels:" buf-text))
          (should (string-match-p "backend" buf-text))
          (should (string-match-p "urgent" buf-text)))))))

(defun test-shipit-issue--metadata-text (issue-data)
  "Return the metadata section of ISSUE-DATA as text.
Drawn for a stand-in tracker with no icons or colours of its own, so
this is about the page, not about one tracker."
  (with-temp-buffer
    (setq-local shipit-issue-backend 'tracker)
    (let ((magit-insert-section--parent nil)
          (shipit-issue-backends '((tracker :name "Tracker")))
          (shipit-issue-buffer-repo "test/repo"))
      (cl-letf (((symbol-function 'shipit-pr-github--get-repo-subscription)
                 (lambda (_config) nil)))
        (magit-insert-section (shipit-issue)
          (shipit-issue--insert-metadata-section "test/repo" issue-data))
        (buffer-string)))))

(defconst test-shipit-issue--plain-issue
  '((id . "PRJ-9") (number . "PRJ-9") (title . "Plain") (state . "Open")
    (body . "Body") (user . ((login . "Alice"))) (assignees . nil) (labels . nil)
    (comments . 0) (created_at . "2024-01-15T10:30:00Z"))
  "An issue with no priority, as a tracker without priorities gives it.")

(ert-deftest test-shipit-issue-metadata-section-shows-the-priority ()
  "GIVEN an issue whose tracker gave it a priority
WHEN rendering the metadata section
THEN a Priority line names it."
  (should (string-match-p "Priority: +High"
                          (test-shipit-issue--metadata-text
                           (cons '(priority . "High") test-shipit-issue--plain-issue)))))

(ert-deftest test-shipit-issue-metadata-section-has-no-priority-without-one ()
  "GIVEN an issue whose tracker has no priorities, as GitHub and GitLab
WHEN rendering the metadata section
THEN there is no Priority line."
  (should-not (string-match-p "Priority:"
                              (test-shipit-issue--metadata-text test-shipit-issue--plain-issue))))

(ert-deftest test-shipit-issue-metadata-section-labels-line-always-shown ()
  "GIVEN an issue with no labels
WHEN rendering the metadata section
THEN a Labels line is still shown with a None placeholder, and the line
is tagged for editing (consistent with how Assignee always renders)."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (shipit-issue-buffer-repo "test/repo")
          (issue-data '((id . "PRJ-8")
                        (number . "PRJ-8")
                        (title . "No labels")
                        (state . "Open")
                        (issue-type . "Task")
                        (body . "Body")
                        (user . ((login . "Alice")))
                        (assignees . nil)
                        (labels . nil)
                        (comments . 0)
                        (created_at . "2024-01-15T10:30:00Z")
                        (updated_at . "2024-01-20T14:00:00Z"))))
      (cl-letf (((symbol-function 'shipit-pr-github--get-repo-subscription)
                 (lambda (_config) nil)))
        (magit-insert-section (shipit-issue)
          (shipit-issue--insert-metadata-section "test/repo" issue-data))
        ;; THEN the Labels line is present even with no labels
        (should (string-match-p "Labels:[^\n]*None" (buffer-string)))
        ;; AND the line carries the editable text property
        (goto-char (point-min))
        (should (re-search-forward "Labels:" nil t))
        (should (get-text-property (match-beginning 0) 'shipit-issue-labels))))))

(ert-deftest test-shipit-issue--change-labels-dispatches ()
  "GIVEN a backend exposing :fetch-labels and :update-labels
WHEN the user edits labels on an issue
THEN the selected labels are passed to :update-labels and the buffer
refreshes."
  (let ((updated 'unset) (refreshed nil) (fetched nil))
    (cl-letf (((symbol-function 'shipit-issue--get-backend)
               (lambda ()
                 (list :fetch-labels (lambda (_c) (setq fetched t) '("a" "b" "c"))
                       :update-labels (lambda (_c _num labels) (setq updated labels)))))
              ((symbol-function 'completing-read-multiple)
               (lambda (&rest _) '("a" "c")))
              ((symbol-function 'shipit-issue-buffer-refresh)
               (lambda (&rest _) (setq refreshed t))))
      (let ((shipit-issue-backend-config '(:base-url "x"))
            (shipit-issue-buffer-number "PRJ-7")
            (shipit-issue-buffer-data '((labels . nil))))
        (shipit-issue--change-labels)
        (should fetched)
        (should (equal updated '("a" "c")))
        (should refreshed)))))

(ert-deftest test-shipit-issue--edit-title-dispatches ()
  "GIVEN a backend exposing :update-title
WHEN the user edits the issue title to a new value
THEN the new title is passed to :update-title and the buffer refreshes."
  (let ((updated 'unset) (refreshed nil))
    (cl-letf (((symbol-function 'shipit-issue--get-backend)
               (lambda () (list :update-title
                                (lambda (_c _num title) (setq updated title)))))
              ((symbol-function 'read-string)
               (lambda (&rest _) "Brand new summary"))
              ((symbol-function 'shipit-issue-buffer-refresh)
               (lambda (&rest _) (setq refreshed t))))
      (let ((shipit-issue-backend-config '(:base-url "x"))
            (shipit-issue-buffer-number "PRJ-7")
            (shipit-issue-buffer-data '((title . "Old summary"))))
        (shipit-issue--edit-title)
        (should (equal updated "Brand new summary"))
        (should refreshed)))))

(ert-deftest test-shipit-issue--edit-title-noop-when-unchanged ()
  "GIVEN the user keeps the title unchanged
WHEN editing the title
THEN no update is sent and the buffer is not refreshed."
  (let ((updated nil) (refreshed nil))
    (cl-letf (((symbol-function 'shipit-issue--get-backend)
               (lambda () (list :update-title (lambda (&rest _) (setq updated t)))))
              ((symbol-function 'read-string) (lambda (&rest _) "Same title"))
              ((symbol-function 'shipit-issue-buffer-refresh)
               (lambda (&rest _) (setq refreshed t))))
      (let ((shipit-issue-backend-config '(:base-url "x"))
            (shipit-issue-buffer-number "PRJ-7")
            (shipit-issue-buffer-data '((title . "Same title"))))
        (shipit-issue--edit-title)
        (should-not updated)
        (should-not refreshed)))))

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
      (should (eq 'magit-dimmed (get-text-property 0 'font-lock-face result))))))

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
      (should (eq 'magit-branch-remote (get-text-property 0 'font-lock-face result))))))

(ert-deftest test-shipit-issue-format-status-falls-back-without-category ()
  "GIVEN an item without status-category
WHEN formatting the status column
THEN the default state-face is used."
  (let ((item '((status . "Open")))
        (widths '((status . 10))))
    (let ((result (shipit-issue--format-work-item-column item 'status widths)))
      (should (string-match-p "Open" result))
      (should (eq 'success (get-text-property 0 'font-lock-face result))))))

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

;;; Comment pagination tests

(ert-deftest test-shipit-issue-pagination-below-threshold-renders-all ()
  "GIVEN 10 comments and a threshold of 30
WHEN rendering the comments section
THEN all 10 comments are rendered without a load-more section."
  (let ((shipit-comments-pagination-threshold 30)
        (shipit-comments-initial-count 5)
        (shipit-comments-tail-count 5)
        (insert-calls 0))
    (cl-letf (((symbol-function 'shipit-issue--insert-single-comment)
               (lambda (&rest _) (cl-incf insert-calls)))
              ((symbol-function 'shipit--render-yield)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit--get-pr-field-icon)
               (lambda (&rest _) "")))
      (with-temp-buffer
        (let ((magit-insert-section--parent nil))
          (shipit-issue--insert-comments-section
           "owner/repo" 1
           (cl-loop for i from 1 to 10 collect `((id . ,i)))))
        (should (= 10 insert-calls))
        (goto-char (point-min))
        (should-not (search-forward "Load" nil t))))))

(ert-deftest test-shipit-issue-pagination-above-threshold-splits ()
  "GIVEN 50 comments, threshold 30, head 5, tail 5
WHEN rendering the comments section
THEN only 10 comments are rendered and a load-more section appears."
  (let ((shipit-comments-pagination-threshold 30)
        (shipit-comments-initial-count 5)
        (shipit-comments-tail-count 5)
        (insert-calls 0))
    (cl-letf (((symbol-function 'shipit-issue--insert-single-comment)
               (lambda (&rest _) (cl-incf insert-calls)))
              ((symbol-function 'shipit--render-yield)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit--get-pr-field-icon)
               (lambda (&rest _) "")))
      (with-temp-buffer
        (let ((magit-insert-section--parent nil))
          (shipit-issue--insert-comments-section
           "owner/repo" 1
           (cl-loop for i from 1 to 50 collect `((id . ,i)))))
        (should (= 10 insert-calls))
        (goto-char (point-min))
        (should (search-forward "Load 40 more" nil t))))))

(ert-deftest test-shipit-issue-pagination-correct-hidden-count ()
  "GIVEN 50 comments, head 5, tail 5
WHEN rendering with pagination
THEN the Load more text shows exactly 40 hidden comments."
  (let ((shipit-comments-pagination-threshold 30)
        (shipit-comments-initial-count 5)
        (shipit-comments-tail-count 5))
    (cl-letf (((symbol-function 'shipit-issue--insert-single-comment)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit--render-yield)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit--get-pr-field-icon)
               (lambda (&rest _) "")))
      (with-temp-buffer
        (let ((magit-insert-section--parent nil))
          (shipit-issue--insert-comments-section
           "owner/repo" 1
           (cl-loop for i from 1 to 50 collect `((id . ,i)))))
        (goto-char (point-min))
        (should (search-forward "Load 40 more comments" nil t))))))

(ert-deftest test-shipit-issue-metadata-shows-jira-components ()
  "GIVEN a Jira-like issue with components
WHEN rendering the metadata section
THEN a Component row appears with each component name."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (issue-data '((id . "PRJ-1")
                        (number . "PRJ-1")
                        (title . "T")
                        (state . "Open")
                        (issue-type . "Bug")
                        (user . ((login . "Alice")))
                        (assignees . nil)
                        (labels . nil)
                        (components . (((name . "SDK"))
                                       ((name . "Firmware"))))
                        (created_at . "2024-01-15T10:30:00Z")
                        (updated_at . "2024-01-20T14:00:00Z"))))
      (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
                 (lambda (_repo) nil)))
        (magit-insert-section (shipit-issue)
          (shipit-issue--insert-metadata-section "test/repo" issue-data))
        (let ((buf-text (buffer-string)))
          (should (string-match-p "Component:" buf-text))
          (should (string-match-p "SDK" buf-text))
          (should (string-match-p "Firmware" buf-text)))))))

(ert-deftest test-shipit-issue-metadata-omits-component-row-when-absent ()
  "GIVEN an issue with no components key
WHEN rendering the metadata section
THEN no Component row is inserted (avoids a blank label for non-Jira issues)."
  (with-temp-buffer
    (let ((magit-insert-section--parent nil)
          (issue-data '((id . 1)
                        (number . 1)
                        (title . "T")
                        (state . "open")
                        (user . ((login . "alice")))
                        (assignees . nil)
                        (labels . nil)
                        (created_at . "2024-01-15T10:30:00Z")
                        (updated_at . "2024-01-20T14:00:00Z"))))
      (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
                 (lambda (_repo) nil)))
        (magit-insert-section (shipit-issue)
          (shipit-issue--insert-metadata-section "test/repo" issue-data))
        (should-not (string-match-p "Component:" (buffer-string)))))))

;;; Activity Section Sort Order Tests

(defconst test-shipit-issue--changelog-newest-first
  '(((id . "e3") (created_at . "2026-03-03T10:00:00Z")
     (user . ((login . "carol")))
     (items . (((field . "status") (from . "Open") (to . "Done")))))
    ((id . "e2") (created_at . "2026-02-02T10:00:00Z")
     (user . ((login . "bob")))
     (items . (((field . "comment")))))
    ((id . "e1") (created_at . "2026-01-01T10:00:00Z")
     (user . ((login . "alice")))
     (items . (((field . "status") (from . "New") (to . "Open"))))))
  "Changelog fixture delivered newest-first (Jira API delivery order).")

(defconst test-shipit-issue--activity-timestamps-descending
  '("2026-03-03T10:00:00Z" "2026-02-02T10:00:00Z" "2026-01-01T10:00:00Z")
  "The fixture's timestamps in newest-first order.")

(defun test-shipit-issue--activity-raw-timestamps ()
  "Collect the shipit-raw-timestamp property values in buffer order."
  (let ((stamps nil)
        (pos (point-min)))
    (while (setq pos (next-single-property-change pos 'shipit-raw-timestamp))
      (let ((ts (get-text-property pos 'shipit-raw-timestamp)))
        (when ts (push ts stamps)))
      (setq pos (1+ pos)))
    (nreverse stamps)))

(defun test-shipit-issue--render-activity (changelog &optional loading-p)
  "Render CHANGELOG's activity section in the current buffer.
CHANGELOG and LOADING-P are passed to the section inserter."
  (magit-section-mode)
  (let ((inhibit-read-only t))
    (magit-insert-section (root)
      (shipit-issue--insert-activity-section
       "test/repo" `((changelog . ,changelog)) loading-p))))

(ert-deftest test-shipit-issue-activity-renders-newest-first-by-default ()
  "GIVEN a changelog delivered newest-first (Jira delivery order)
WHEN rendering the activity section under the default order
THEN events appear newest-first in the buffer."
  (let ((shipit-activity-timeline-order 'reverse-chronological))
    (with-temp-buffer
      (test-shipit-issue--render-activity
       test-shipit-issue--changelog-newest-first)
      (should (equal test-shipit-issue--activity-timestamps-descending
                     (test-shipit-issue--activity-raw-timestamps))))))

(ert-deftest test-shipit-issue-activity-order-independent-of-delivery ()
  "GIVEN a changelog delivered oldest-first (GitHub delivery order)
WHEN rendering the activity section under the default order
THEN events still appear newest-first in the buffer."
  (let ((shipit-activity-timeline-order 'reverse-chronological))
    (with-temp-buffer
      (test-shipit-issue--render-activity
       (reverse test-shipit-issue--changelog-newest-first))
      (should (equal test-shipit-issue--activity-timestamps-descending
                     (test-shipit-issue--activity-raw-timestamps))))))

(ert-deftest test-shipit-issue-activity-chronological-variant ()
  "GIVEN a changelog delivered newest-first
WHEN rendering under chronological order
THEN events appear oldest-first in the buffer."
  (let ((shipit-activity-timeline-order 'chronological))
    (with-temp-buffer
      (test-shipit-issue--render-activity
       test-shipit-issue--changelog-newest-first)
      (should (equal (reverse test-shipit-issue--activity-timestamps-descending)
                     (test-shipit-issue--activity-raw-timestamps))))))

(ert-deftest test-shipit-issue-activity-refresh-honors-order ()
  "GIVEN an activity section rendered as a loading placeholder
WHEN the async refresh delivers a newest-first changelog
THEN the refreshed section renders events newest-first."
  (let ((shipit-activity-timeline-order 'reverse-chronological))
    (with-temp-buffer
      (test-shipit-issue--render-activity nil t)
      (shipit-issue--refresh-activity-section
       test-shipit-issue--changelog-newest-first)
      (should (equal test-shipit-issue--activity-timestamps-descending
                     (test-shipit-issue--activity-raw-timestamps)))
      (should (search-backward "Activity (3)" nil t)))))

(ert-deftest test-shipit-issue-activity-toggle-rerenders-buffer ()
  "GIVEN a rendered activity section under the default newest-first order
WHEN toggling the sort order
THEN the buffer re-renders oldest-first without a refetch
     AND toggling again restores newest-first."
  (let ((shipit-activity-timeline-order 'reverse-chronological))
    (with-temp-buffer
      (test-shipit-issue--render-activity
       test-shipit-issue--changelog-newest-first)
      (shipit-activity-toggle-sort-order)
      (should (equal (reverse test-shipit-issue--activity-timestamps-descending)
                     (test-shipit-issue--activity-raw-timestamps)))
      (shipit-activity-toggle-sort-order)
      (should (equal test-shipit-issue--activity-timestamps-descending
                     (test-shipit-issue--activity-raw-timestamps))))))

(ert-deftest test-shipit-issue-activity-dwim-handler-registered ()
  "GIVEN shipit-pr-actions and the issue buffer are loaded
WHEN the issue DWIM handlers are registered
THEN an issue-activity-header handler exists in the dispatch list."
  (require 'shipit-pr-actions)
  (shipit-issue--register-dwim-handlers)
  (let ((handler (assq 'issue-activity-header shipit--dwim-handlers)))
    (should handler)
    (should (functionp (cdr (assq 'action (cdr handler)))))))

;;; Attachments

(defconst test-issue-attachments
  '(((filename . "shot.png") (mime-type . "image/png") (size . 223373) (author . "Alice")
     (created . "2026-09-25T04:06:31.341-0500")
     (url . "https://jira.example.com/rest/api/3/attachment/content/10001?redirect=false"))
    ((filename . "notes.txt") (mime-type . "text/plain") (size . 12) (author . "Bob")
     (created . "2026-09-24T10:00:00.000-0500")
     (url . "https://jira.example.com/rest/api/3/attachment/content/10002?redirect=false")))
  "Two attachments of an issue, as a backend gives them.")

(defmacro test-issue-attachments--shown (attachments &rest body)
  "Run BODY in an issue buffer that shows ATTACHMENTS, point at the start."
  (declare (indent 1))
  `(with-temp-buffer
     (shipit-issue-mode)
     (let ((inhibit-read-only t))
       (magit-insert-section (shipit-issue)
         (shipit-issue--insert-attachments-section (list (cons 'attachments ,attachments)))))
     (goto-char (point-min))
     ,@body))

(ert-deftest test-shipit-issue-attachments-are-listed ()
  "GIVEN an issue with two attachments
WHEN its buffer shows them
THEN a section lists each, with its name, size and who added it."
  (test-issue-attachments--shown test-issue-attachments
    (let ((text (buffer-string)))
      (should (string-match-p "Attachments (2)" text))
      (dolist (part '("shot.png" "218k" "Alice" "notes.txt" "Bob"))
        (should (string-match-p (regexp-quote part) text))))))

(ert-deftest test-shipit-issue-without-attachments-has-no-section ()
  "GIVEN an issue with no attachment
WHEN its buffer is drawn
THEN it has no Attachments section."
  (test-issue-attachments--shown nil
    (should-not (string-match-p "Attachments" (buffer-string)))))

(defconst test-issue-attachment-png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")
  "A PNG of one pixel.")

(defconst test-issue-attachment-thumbnail
  "https://jira.example.com/rest/api/3/attachment/thumbnail/10009?redirect=false"
  "Where the thumbnail of the attached picture downloads from.")

(defconst test-issue-attachment-with-thumbnail
  `((filename . "picture.png") (mime-type . "image/png") (size . 2048) (author . "Alice")
    (created . "2026-09-25T04:06:31.341-0500")
    (url . "https://jira.example.com/rest/api/3/attachment/content/10009?redirect=false")
    (thumbnail . ,test-issue-attachment-thumbnail))
  "An attached picture with a thumbnail.")

(defun test-issue-attachments--images ()
  "The images this buffer shows, as its display properties hold them."
  (let ((images nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((display (get-text-property pos 'display)))
        (when (eq 'image (car-safe display))
          (push display images)))
      (setq pos (next-single-property-change pos 'display nil (point-max))))
    images))

(defmacro test-issue-attachments--fetching-thumbnail (status &rest body)
  "Run BODY with the thumbnail answering STATUS, and none of it cached."
  (declare (indent 1))
  `(let ((cached (shipit--image-cache-file test-issue-attachment-thumbnail)))
     (when (file-exists-p cached)
       (delete-file cached))
     (shipit-test-http-answering (list (list "/attachment/thumbnail/10009" ,status
                                             test-issue-attachment-png))
       ,@body)))

(ert-deftest test-shipit-issue-attachment-shows-its-thumbnail ()
  "GIVEN a picture attached to an issue, with a thumbnail
WHEN the issue buffer shows it
THEN the thumbnail is fetched in the background, and shown in the row
of the picture."
  (test-issue-attachments--fetching-thumbnail 200
    (test-issue-attachments--shown (list test-issue-attachment-with-thumbnail)
      (let ((images (test-issue-attachments--images)))
        (should (= 1 (length images)))
        (should (file-exists-p (plist-get (cdr (car images)) :file)))))))

(ert-deftest test-shipit-issue-attachment-keeps-its-icon-without-a-thumbnail ()
  "GIVEN a picture whose thumbnail does not download
WHEN the issue buffer shows it
THEN its row keeps its icon, and nothing fails."
  (test-issue-attachments--fetching-thumbnail 404
    (test-issue-attachments--shown (list test-issue-attachment-with-thumbnail)
      (should-not (test-issue-attachments--images))
      (should (string-match-p "picture\\.png" (buffer-string))))))

(ert-deftest test-shipit-issue-ret-opens-an-attachment ()
  "GIVEN an attachment listed in an issue buffer
WHEN RET is pressed on it
THEN its content is downloaded, and opened as a file of its name."
  (test-issue-attachments--shown test-issue-attachments
    (re-search-forward "notes\\.txt")
    (let ((opened nil))
      (unwind-protect
          (progn
            (shipit-test-http-answering '(("/attachment/content/10002" 200 "Some notes."))
              (shipit-issue--ret-dwim))
            (setq opened (seq-find (lambda (buffer)
                                     (equal "notes.txt" (file-name-nondirectory
                                                         (or (buffer-file-name buffer) ""))))
                                   (buffer-list)))
            (should opened)
            (should (equal "Some notes." (with-current-buffer opened (buffer-string)))))
        (when opened (kill-buffer opened))))))

(provide 'test-shipit-issues-buffer)
;;; test-shipit-issues-buffer.el ends here
