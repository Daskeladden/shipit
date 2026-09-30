;;; test-shipit-pr-actions.el --- Tests for PR action menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for PR header action menu and related operations.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-actions)
(require 'shipit-render)
(require 'shipit-pr-sections)  ; shipit--get-requested-reviewers

;;; Test helpers

(defun test-pr-actions--make-minimal-pr-plist ()
  "Return a minimal valid PR backend plist with all required keys."
  (list :name "Test"
        :fetch-pr #'ignore
        :search #'ignore
        :create-pr #'ignore
        :merge-pr #'ignore
        :update-pr #'ignore
        :fetch-reviews #'ignore
        :submit-review #'ignore
        :fetch-review-decision #'ignore
        :fetch-files #'ignore
        :fetch-commits #'ignore
        :fetch-checks #'ignore
        :browse-url #'ignore))

(defmacro test-pr-actions--with-mock-backend (backend-ops &rest body)
  "Run BODY with a mock PR backend registered.
BACKEND-OPS is a plist of operation overrides merged onto the minimal backend."
  (declare (indent 1))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'mock)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo")
         (shipit--current-displayed-pr '(42 "owner/repo")))
     (let ((plist (test-pr-actions--make-minimal-pr-plist)))
       (plist-put plist :name "Mock")
       ,@(cl-loop for (key val) on backend-ops by #'cddr
                  collect `(plist-put plist ,key ,val))
       (shipit-pr-register-backend 'mock plist))
     ,@body))

;;; Tests — shipit--edit-pr-title dispatch

(ert-deftest test-shipit-edit-pr-title-dispatches-to-backend ()
  "GIVEN a mock PR backend with :update-pr
WHEN calling shipit--edit-pr-title
THEN backend :update-pr is called with title data."
  (let ((captured-args nil))
    (test-pr-actions--with-mock-backend
        (:update-pr
         (lambda (config number data)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :data data))
           '((title . "New Title"))))
      (cl-letf (((symbol-function 'shipit--clear-branch-cache) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (shipit--edit-pr-title 42 "New Title" "owner/repo")
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (equal "New Title"
                       (cdr (assq 'title (plist-get captured-args :data)))))))))

;;; Tests — action menu contents

(ert-deftest test-shipit-pr-header-actions-includes-edit-title-for-author ()
  "GIVEN the current user is the PR author
WHEN building the PR header action menu
THEN 'Edit title' is included in the actions list."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'get-text-property)
               (lambda (_pos prop)
                 (pcase prop
                   ('shipit-pr-number 42)
                   ('shipit-repo "owner/repo")
                   (_ nil))))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_num _repo)
                 '((user (login . "testuser"))
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (title . "Old Title"))))
              ((symbol-function 'shipit--get-pr-actual-state)
               (lambda (_data) "open"))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (shipit--pr-header-actions)
      (should (member "Edit title" captured-actions)))))

(ert-deftest test-shipit-pr-header-actions-excludes-edit-title-for-non-author ()
  "GIVEN the current user is NOT the PR author
WHEN building the PR header action menu
THEN 'Edit title' is NOT included in the actions list."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'get-text-property)
               (lambda (_pos prop)
                 (pcase prop
                   ('shipit-pr-number 42)
                   ('shipit-repo "owner/repo")
                   (_ nil))))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_num _repo)
                 '((user (login . "otheruser"))
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (title . "Old Title"))))
              ((symbol-function 'shipit--get-pr-actual-state)
               (lambda (_data) "open"))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (shipit--pr-header-actions)
      (should-not (member "Edit title" captured-actions)))))

;;; Tests — state action label gating

(ert-deftest test-shipit-pr-state-action-labels-open-author ()
  "GIVEN an open non-draft PR where the current user is the author
WHEN building the state action labels
THEN close, close-with-comment, convert-to-draft and merge are offered."
  (should (equal '("Close PR" "Close PR with comment" "Convert to draft" "Merge")
                 (shipit--pr-state-action-labels "open" t))))

(ert-deftest test-shipit-pr-state-action-labels-open-draft-author ()
  "GIVEN an open draft PR where the current user is the author
WHEN building the state action labels
THEN mark-ready-for-review leads and merge is NOT offered
     (GitHub refuses to merge drafts)."
  (should (equal '("Mark ready for review" "Close PR" "Close PR with comment")
                 (shipit--pr-state-action-labels "open" t t))))

(ert-deftest test-shipit-pr-state-action-labels-open-draft-non-author ()
  "GIVEN an open draft PR where the current user is NOT the author
WHEN building the state action labels
THEN no state actions are offered (drafts cannot be merged and only
     the author may mark them ready)."
  (should (null (shipit--pr-state-action-labels "open" nil t))))

(ert-deftest test-shipit-pr-state-action-labels-open-non-author ()
  "GIVEN an open PR where the current user is NOT the author
WHEN building the state action labels
THEN only merge is offered."
  (should (equal '("Merge")
                 (shipit--pr-state-action-labels "open" nil))))

(ert-deftest test-shipit-pr-state-action-labels-closed-author ()
  "GIVEN a closed (not merged) PR where the current user is the author
WHEN building the state action labels
THEN reopen is offered."
  (should (equal '("Reopen PR")
                 (shipit--pr-state-action-labels "closed" t))))

(ert-deftest test-shipit-pr-state-action-labels-closed-non-author ()
  "GIVEN a closed PR where the current user is NOT the author
WHEN building the state action labels
THEN no state actions are offered."
  (should (null (shipit--pr-state-action-labels "closed" nil))))

(ert-deftest test-shipit-pr-state-action-labels-merged ()
  "GIVEN a merged PR, even for the author
WHEN building the state action labels
THEN no state actions are offered (merged PRs cannot change state)."
  (should (null (shipit--pr-state-action-labels "merged" t))))

;;; Tests — reopen from the PR header menu

(ert-deftest test-shipit-pr-header-actions-includes-reopen-for-closed-pr-author ()
  "GIVEN a closed (not merged) PR authored by the current user
WHEN building the PR header action menu
THEN 'Reopen PR' is offered and 'Close PR'/'Merge' are not."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'get-text-property)
               (lambda (_pos prop)
                 (pcase prop
                   ('shipit-pr-number 42)
                   ('shipit-repo "owner/repo")
                   (_ nil))))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_num _repo)
                 '((state . "closed")
                   (user (login . "testuser"))
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (title . "Old Title"))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (shipit--pr-header-actions)
      (should (member "Reopen PR" captured-actions))
      (should-not (member "Close PR" captured-actions))
      (should-not (member "Merge" captured-actions)))))

(ert-deftest test-shipit-pr-header-actions-excludes-reopen-for-merged-pr ()
  "GIVEN a merged PR authored by the current user
WHEN building the PR header action menu
THEN 'Reopen PR' is NOT offered."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'get-text-property)
               (lambda (_pos prop)
                 (pcase prop
                   ('shipit-pr-number 42)
                   ('shipit-repo "owner/repo")
                   (_ nil))))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_num _repo)
                 '((state . "closed")
                   (merged_at . "2026-07-01T10:00:00Z")
                   (user (login . "testuser"))
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (title . "Old Title"))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (shipit--pr-header-actions)
      (should-not (member "Reopen PR" captured-actions)))))

(ert-deftest test-shipit-pr-header-actions-reopen-choice-reopens-pr ()
  "GIVEN a closed PR authored by the current user
WHEN choosing 'Reopen PR' from the header menu and confirming
THEN backend :update-pr is called with state=open."
  (let ((captured-args nil))
    (test-pr-actions--with-mock-backend
        (:update-pr
         (lambda (config number data)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :data data))
           '((state . "open"))))
      (cl-letf (((symbol-function 'get-text-property)
                 (lambda (_pos prop)
                   (pcase prop
                     ('shipit-pr-number 42)
                     ('shipit-repo "owner/repo")
                     (_ nil))))
                ((symbol-function 'shipit-get-pull-request)
                 (lambda (_num _repo)
                   '((state . "closed")
                     (user (login . "testuser"))
                     (html_url . "https://github.com/owner/repo/pull/42")
                     (title . "Old Title"))))
                ((symbol-function 'shipit--get-current-user)
                 (lambda () "testuser"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _collection &rest _args) "Reopen PR"))
                ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-clear-etag-cache-for-repo) #'ignore)
                ((symbol-function 'shipit-clear-stale-cache-files) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (shipit--pr-header-actions)
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (equal '((state . "open")) (plist-get captured-args :data)))))))

;;; Tests — state line dwim actions

(ert-deftest test-shipit-pr-state-actions-reopens-closed-pr ()
  "GIVEN a closed PR authored by the current user and point on the state line
WHEN running the state dwim action and choosing 'Reopen PR'
THEN backend :update-pr is called with state=open."
  (let ((captured-args nil))
    (test-pr-actions--with-mock-backend
        (:update-pr
         (lambda (config number data)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :data data))
           '((state . "open"))))
      (cl-letf (((symbol-function 'shipit-get-pull-request)
                 (lambda (_num &optional _repo)
                   '((state . "closed")
                     (user (login . "testuser")))))
                ((symbol-function 'shipit--get-current-user)
                 (lambda () "testuser"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _collection &rest _args) "Reopen PR"))
                ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-clear-etag-cache-for-repo) #'ignore)
                ((symbol-function 'shipit-clear-stale-cache-files) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (with-temp-buffer
          (shipit--pr-state-actions))
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (equal '((state . "open")) (plist-get captured-args :data)))))))

(ert-deftest test-shipit-pr-state-actions-rejects-merged-pr ()
  "GIVEN a merged PR and point on the state line
WHEN running the state dwim action
THEN a user-error explains the state cannot be changed."
  (test-pr-actions--with-mock-backend ()
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_num &optional _repo)
                 '((state . "closed")
                   (merged_at . "2026-07-01T10:00:00Z")
                   (user (login . "testuser")))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser")))
      (with-temp-buffer
        (should-error (shipit--pr-state-actions) :type 'user-error)))))

(ert-deftest test-shipit-pr-state-actions-rejects-non-author-on-closed-pr ()
  "GIVEN a closed PR NOT authored by the current user
WHEN running the state dwim action
THEN a user-error explains only the author can reopen."
  (test-pr-actions--with-mock-backend ()
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_num &optional _repo)
                 '((state . "closed")
                   (user (login . "otheruser")))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser")))
      (with-temp-buffer
        (should-error (shipit--pr-state-actions) :type 'user-error)))))

;;; Tests — draft state toggling

(ert-deftest test-shipit-pr-state-actions-marks-draft-ready ()
  "GIVEN an open draft PR authored by the current user
WHEN running the state dwim action and choosing 'Mark ready for review'
THEN backend :set-pr-draft is called with draft-p nil."
  (let ((captured nil))
    (test-pr-actions--with-mock-backend
        (:set-pr-draft
         (lambda (config number draft-p)
           (setq captured (list :repo (plist-get config :repo)
                                :number number :draft draft-p))
           t))
      (cl-letf (((symbol-function 'shipit-get-pull-request)
                 (lambda (_num &optional _repo)
                   '((state . "open")
                     (draft . t)
                     (user (login . "testuser")))))
                ((symbol-function 'shipit--get-current-user)
                 (lambda () "testuser"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (should (member "Mark ready for review" collection))
                   (should-not (member "Merge" collection))
                   "Mark ready for review"))
                ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-clear-etag-cache-for-repo) #'ignore)
                ((symbol-function 'shipit-clear-stale-cache-files) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (with-temp-buffer
          (shipit--pr-state-actions))
        (should (string= "owner/repo" (plist-get captured :repo)))
        (should (= 42 (plist-get captured :number)))
        (should (plist-member captured :draft))
        (should-not (plist-get captured :draft))))))

(ert-deftest test-shipit-pr-state-actions-converts-open-pr-to-draft ()
  "GIVEN an open non-draft PR authored by the current user
WHEN running the state dwim action and choosing 'Convert to draft'
THEN backend :set-pr-draft is called with draft-p t."
  (let ((captured nil))
    (test-pr-actions--with-mock-backend
        (:set-pr-draft
         (lambda (_config number draft-p)
           (setq captured (list :number number :draft draft-p))
           t))
      (cl-letf (((symbol-function 'shipit-get-pull-request)
                 (lambda (_num &optional _repo)
                   '((state . "open")
                     (user (login . "testuser")))))
                ((symbol-function 'shipit--get-current-user)
                 (lambda () "testuser"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (should (member "Convert to draft" collection))
                   (should (member "Merge" collection))
                   "Convert to draft"))
                ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-clear-etag-cache-for-repo) #'ignore)
                ((symbol-function 'shipit-clear-stale-cache-files) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (with-temp-buffer
          (shipit--pr-state-actions))
        (should (= 42 (plist-get captured :number)))
        (should (eq t (plist-get captured :draft)))))))

(ert-deftest test-shipit-pr-state-actions-errors-when-backend-lacks-draft-support ()
  "GIVEN an open draft PR on a backend without :set-pr-draft
WHEN running the state dwim action and choosing 'Mark ready for review'
THEN a clear user-error is signaled instead of failing silently."
  (test-pr-actions--with-mock-backend ()
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_num &optional _repo)
                 '((state . "open")
                   (draft . t)
                   (user (login . "testuser")))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection &rest _args)
                 "Mark ready for review"))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (with-temp-buffer
        (should-error (shipit--pr-state-actions) :type 'user-error)))))

(ert-deftest test-shipit-pr-header-actions-includes-mark-ready-for-draft ()
  "GIVEN an open draft PR authored by the current user
WHEN building the PR header action menu
THEN 'Mark ready for review' is offered while 'Merge' and
     'Convert to draft' are not."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'get-text-property)
               (lambda (_pos prop)
                 (pcase prop
                   ('shipit-pr-number 42)
                   ('shipit-repo "owner/repo")
                   (_ nil))))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_num _repo)
                 '((state . "open")
                   (draft . t)
                   (user (login . "testuser"))
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (title . "My draft"))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (shipit--pr-header-actions)
      (should (member "Mark ready for review" captured-actions))
      (should-not (member "Merge" captured-actions))
      (should-not (member "Convert to draft" captured-actions)))))

;;; Tests — bot reviewers (Copilot code review)

(ert-deftest test-shipit-reviewer-add-choices-and-mapping ()
  "GIVEN a backend exposing Copilot as a requestable bot reviewer
WHEN assembling reviewer choices and mapping picker choices
THEN bots lead the list, requested logins are filtered on both kinds,
     and display names translate to logins and back."
  (test-pr-actions--with-mock-backend
      (:reviewer-bot-candidates
       (lambda (_config)
         '(("Copilot" . "copilot-pull-request-reviewer[bot]"))))
    (should (equal '("Copilot" "alice")
                   (shipit-pr--reviewer-add-choices
                    "owner/repo" '("bob") '("alice" "bob"))))
    (should (equal '("alice")
                   (shipit-pr--reviewer-add-choices
                    "owner/repo"
                    '("copilot-pull-request-reviewer[bot]")
                    '("alice"))))
    (should (equal "copilot-pull-request-reviewer[bot]"
                   (shipit-pr--reviewer-choice-to-login "owner/repo" "Copilot")))
    (should (equal "alice"
                   (shipit-pr--reviewer-choice-to-login "owner/repo" "alice")))
    (should (equal "Copilot"
                   (shipit-pr--reviewer-login-to-display
                    "owner/repo" "copilot-pull-request-reviewer[bot]")))
    (should (equal "alice"
                   (shipit-pr--reviewer-login-to-display "owner/repo" "alice")))))

(ert-deftest test-shipit-manage-reviewers-offers-and-requests-copilot ()
  "GIVEN a backend exposing Copilot as a requestable bot reviewer
WHEN adding a reviewer via manage-reviewers and choosing Copilot
THEN the backend :add-reviewer receives the bot login, not the label."
  (let ((captured nil))
    (test-pr-actions--with-mock-backend
        (:reviewer-bot-candidates
         (lambda (_config)
           '(("Copilot" . "copilot-pull-request-reviewer[bot]")))
         :fetch-requested-reviewers
         (lambda (_config _number) '((users . ()) (teams . ())))
         :fetch-available-assignees
         (lambda (_config) '(((login . "alice"))))
         :add-reviewer
         (lambda (_config number username)
           (setq captured (list number username))
           '((ok . t))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _args)
                   (if (string-match-p "action" prompt)
                       "Add reviewer"
                     (progn
                       (should (member "Copilot" collection))
                       (should (member "alice" collection))
                       "Copilot"))))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (with-temp-buffer
          (shipit--manage-reviewers 42 "owner/repo"))
        (should (equal '(42 "copilot-pull-request-reviewer[bot]") captured))))))

(ert-deftest test-shipit-manage-reviewers-removes-copilot-by-login ()
  "GIVEN Copilot is already a requested reviewer
WHEN removing a reviewer via manage-reviewers
THEN it is offered under its display name and removed by bot login."
  (let ((captured nil))
    (test-pr-actions--with-mock-backend
        (:reviewer-bot-candidates
         (lambda (_config)
           '(("Copilot" . "copilot-pull-request-reviewer[bot]")))
         :fetch-requested-reviewers
         (lambda (_config _number)
           '((users . (((login . "copilot-pull-request-reviewer[bot]"))
                       ((login . "alice"))))))
         :remove-reviewer
         (lambda (_config number username)
           (setq captured (list number username))
           '((ok . t))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _args)
                   (if (string-match-p "action" prompt)
                       "Remove reviewer"
                     (progn
                       (should (member "Copilot" collection))
                       (should (member "alice" collection))
                       "Copilot"))))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (with-temp-buffer
          (shipit--manage-reviewers 42 "owner/repo"))
        (should (equal '(42 "copilot-pull-request-reviewer[bot]") captured))))))

(ert-deftest test-shipit-manage-reviewers-without-bot-op ()
  "GIVEN a backend without :reviewer-bot-candidates
WHEN adding a reviewer
THEN only collaborators are offered and the flow is unchanged."
  (let (offered)
    (test-pr-actions--with-mock-backend
        (:fetch-requested-reviewers
         (lambda (_config _number) '((users . ())))
         :fetch-available-assignees
         (lambda (_config) '(((login . "alice"))))
         :add-reviewer
         (lambda (&rest _) '((ok . t))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _args)
                   (if (string-match-p "action" prompt)
                       "Add reviewer"
                     (progn (setq offered collection) "alice"))))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (with-temp-buffer
          (shipit--manage-reviewers 42 "owner/repo"))
        (should (equal '("alice") offered))))))

;;; Tests — generic URL overlays

(ert-deftest test-shipit-generic-url-overlay-created-for-plain-url ()
  "GIVEN a buffer containing a plain https URL
WHEN shipit--create-generic-url-overlays is called
THEN an overlay with keymap and face is created on the URL."
  (with-temp-buffer
    (insert "Check out https://example.com/some/page for details.\n")
    (shipit--create-generic-url-overlays (point-min) (point-max))
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (= 1 (length overlays)))
      (let ((ov (car overlays)))
        (should (eq 'markdown-plain-url-face (overlay-get ov 'face)))
        (should (overlay-get ov 'keymap))
        (should (overlay-get ov 'evaporate))
        ;; Verify overlay covers exactly the URL
        (should (string= "https://example.com/some/page"
                         (buffer-substring-no-properties
                          (overlay-start ov) (overlay-end ov))))
        ;; Verify both [return] and RET are bound (GUI vs terminal)
        (let ((km (overlay-get ov 'keymap)))
          (should (lookup-key km [return]))
          (should (lookup-key km (kbd "RET"))))))))

(ert-deftest test-shipit-generic-url-overlay-covers-link-text-for-markdown-link ()
  "GIVEN a buffer containing a markdown link [text](url)
WHEN shipit--create-generic-url-overlays is called
THEN the overlay covers the link text, not the URL."
  (with-temp-buffer
    (insert "See [my-project](https://github.com/owner/my-project) for info.\n")
    (shipit--create-generic-url-overlays (point-min) (point-max))
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (= 1 (length overlays)))
      (let ((ov (car overlays)))
        ;; Overlay should cover "my-project" (the link text), not the URL
        (should (string= "my-project"
                         (buffer-substring-no-properties
                          (overlay-start ov) (overlay-end ov))))
        (should (overlay-get ov 'keymap))
        (should (eq 'markdown-plain-url-face (overlay-get ov 'face)))))))

(ert-deftest test-shipit-generic-url-overlay-skips-existing-keymap-overlay ()
  "GIVEN a buffer where a URL already has an overlay with a keymap
WHEN shipit--create-generic-url-overlays is called
THEN no additional overlay is created for that URL."
  (with-temp-buffer
    (insert "See https://github.com/owner/repo/pull/42 for the PR.\n")
    ;; Simulate an existing overlay (from PR reference pass)
    (let ((existing-ov (make-overlay 5 46)))
      (overlay-put existing-ov 'keymap (make-sparse-keymap))
      (shipit--create-generic-url-overlays (point-min) (point-max))
      ;; Should still be just the one pre-existing overlay
      (should (= 1 (length (overlays-in (point-min) (point-max))))))))

(ert-deftest test-shipit-generic-url-overlay-skips-backtick-code ()
  "GIVEN a buffer with a URL inside backticks
WHEN shipit--create-generic-url-overlays is called
THEN no overlay is created for the URL inside code."
  (with-temp-buffer
    (insert "Use `https://example.com/api` for the endpoint.\n")
    (shipit--create-generic-url-overlays (point-min) (point-max))
    (should (= 0 (length (overlays-in (point-min) (point-max)))))))

;;; Tests — overlay action dispatch

(ert-deftest test-shipit-try-overlay-action-dispatches-highest-priority ()
  "GIVEN two overlays at point with different priorities
WHEN shipit--try-overlay-action-at-point is called
THEN the highest-priority overlay's RET binding is invoked."
  (with-temp-buffer
    (insert "some text here\n")
    (let ((called nil)
          (ov-low (make-overlay 1 10))
          (ov-high (make-overlay 1 10))
          (km-low (make-sparse-keymap))
          (km-high (make-sparse-keymap)))
      (define-key km-low (kbd "RET") (lambda () (interactive) (setq called 'low)))
      (define-key km-high (kbd "RET") (lambda () (interactive) (setq called 'high)))
      (overlay-put ov-low 'keymap km-low)
      (overlay-put ov-low 'priority 10)
      (overlay-put ov-high 'keymap km-high)
      (overlay-put ov-high 'priority 100)
      (goto-char 5)
      (should (shipit--try-overlay-action-at-point))
      (should (eq 'high called)))))

(ert-deftest test-shipit-try-overlay-action-returns-nil-without-overlay ()
  "GIVEN no overlays at point
WHEN shipit--try-overlay-action-at-point is called
THEN it returns nil."
  (with-temp-buffer
    (insert "some text here\n")
    (goto-char 5)
    (should-not (shipit--try-overlay-action-at-point))))

;;; Activity Heading Menu Tests

(ert-deftest test-pr-actions-activity-menu-is-transient ()
  "GIVEN shipit-pr-actions is loaded
THEN the Activity heading menu is a transient prefix
     AND it binds the sort-order toggle and mark-all-read."
  (should (fboundp 'shipit-activity-menu))
  (should (get 'shipit-activity-menu 'transient--prefix))
  (let ((toggle (transient-get-suffix 'shipit-activity-menu "o"))
        (mark-read (transient-get-suffix 'shipit-activity-menu "m")))
    (should (eq (plist-get (cdr toggle) :command)
                'shipit-activity-toggle-sort-order))
    (should (eq (plist-get (cdr mark-read) :command)
                'shipit-mark-activities-read))))

(ert-deftest test-pr-actions-activity-header-opens-menu ()
  "GIVEN point is on the Activity section heading
WHEN the header DWIM action runs
THEN the Activity transient menu opens."
  (let ((opened nil))
    (cl-letf (((symbol-function 'shipit-activity-menu)
               (lambda () (interactive) (setq opened t))))
      (shipit--activity-section-header-actions)
      (should opened))))

;;; The menu of a diff line

(defmacro test-pr-actions--on-diff-line (&rest body)
  "Run BODY with point on line 2 of src/parser.el in a diff, with no comment."
  (declare (indent 0))
  `(with-temp-buffer
     (insert (propertize "+  (read-header)\n"
                         'shipit-file-path "src/parser.el" 'shipit-line-number 2))
     (goto-char (point-min))
     ,@body))

(ert-deftest test-pr-actions-a-diff-line-offers-the-actions-added-to-it ()
  "GIVEN an action added to the menu of a diff line, and no pull request
to comment on
WHEN the menu of a diff line is opened, and the action chosen
THEN it is the one action offered, and it runs on the line."
  (test-pr-actions--on-diff-line
    (let* ((offered nil)
           (ran-on nil)
           (explain (lambda () (interactive)
                      (setq ran-on (get-text-property (point) 'shipit-line-number))))
           (shipit-diff-line-actions (list (cons "Explain" explain))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered collection)
                   "Explain")))
        (shipit--diff-line-actions))
      (should (equal '("Explain") offered))
      (should (equal 2 ran-on)))))

(ert-deftest test-pr-actions-a-diff-line-with-no-action-says-so ()
  "GIVEN no action added to the menu of a diff line, and no pull request
to comment on
WHEN the menu of a diff line is opened
THEN that is refused: there is nothing to offer."
  (test-pr-actions--on-diff-line
    (let ((shipit-diff-line-actions nil))
      (should-error (shipit--diff-line-actions) :type 'user-error))))

(ert-deftest test-shipit-dwim-a-handler-of-a-higher-priority-goes-first ()
  "GIVEN a handler of priority 10, registered before one of the default
priority, both matching
WHEN `shipit-dwim' runs
THEN the handler of priority 10 runs, whatever the order of registration."
  (let ((shipit--dwim-handlers nil)
        (ran nil))
    (shipit-register-dwim-handler 'test-high (lambda () t) (lambda () (push 'high ran)) 10)
    (shipit-register-dwim-handler 'test-low (lambda () t) (lambda () (push 'low ran)))
    (shipit-dwim)
    (should (equal '(high) ran))))

(ert-deftest test-shipit-dwim-handlers-of-one-priority-go-newest-first ()
  "GIVEN two handlers of the default priority, both matching
WHEN `shipit-dwim' runs
THEN the one registered last runs."
  (let ((shipit--dwim-handlers nil)
        (ran nil))
    (shipit-register-dwim-handler 'test-old (lambda () t) (lambda () (push 'old ran)))
    (shipit-register-dwim-handler 'test-new (lambda () t) (lambda () (push 'new ran)))
    (shipit-dwim)
    (should (equal '(new) ran))))

(provide 'test-shipit-pr-actions)
;;; test-shipit-pr-actions.el ends here
