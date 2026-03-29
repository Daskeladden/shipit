;;; test-shipit-issue-comment-actions.el --- Tests for issue comment actions -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for issue comment interactions: reply, react, edit.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-jira)
(require 'shipit-issues)

;;; GitHub Backend — add-comment

(ert-deftest test-shipit-issue-github-add-comment ()
  "GIVEN the github backend
WHEN calling :add-comment with repo, issue-id, and body
THEN it POSTs to /repos/{repo}/issues/{id}/comments."
  (let ((called-endpoint nil)
        (called-data nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &rest _)
                 (setq called-endpoint endpoint
                       called-data data)
                 '((id . 999) (body . "test reply")))))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (fn (plist-get backend :add-comment))
             (config '(:repo "owner/repo")))
        (funcall fn config 42 "test reply")
        (should (string= "/repos/owner/repo/issues/42/comments" called-endpoint))
        (should (equal "test reply" (cdr (assq 'body called-data))))))))

;;; GitHub Backend — edit-comment

(ert-deftest test-shipit-issue-github-edit-comment ()
  "GIVEN the github backend
WHEN calling :edit-comment with comment-id and body
THEN it PATCHes to /repos/{repo}/issues/comments/{id}."
  (let ((called-endpoint nil)
        (called-data nil)
        (called-method nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &optional method)
                 (setq called-endpoint endpoint
                       called-data data
                       called-method method)
                 '((id . 100) (body . "updated")))))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (fn (plist-get backend :edit-comment))
             (config '(:repo "owner/repo")))
        (funcall fn config 100 "updated")
        (should (string= "/repos/owner/repo/issues/comments/100" called-endpoint))
        (should (equal "updated" (cdr (assq 'body called-data))))
        (should (string= "PATCH" called-method))))))

;;; GitHub Backend — toggle-reaction

(ert-deftest test-shipit-issue-github-toggle-reaction ()
  "GIVEN the github backend
WHEN calling :toggle-reaction with comment-id and reaction
THEN it POSTs to /repos/{repo}/issues/comments/{id}/reactions."
  (let ((called-endpoint nil)
        (called-data nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &rest _)
                 (setq called-endpoint endpoint
                       called-data data)
                 '((id . 1) (content . "+1")))))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (fn (plist-get backend :toggle-reaction))
             (config '(:repo "owner/repo")))
        (funcall fn config 100 "+1")
        (should (string= "/repos/owner/repo/issues/comments/100/reactions" called-endpoint))
        (should (equal "+1" (cdr (assq 'content called-data))))))))

;;; Thin Wrappers — add-comment dispatches to backend

(ert-deftest test-shipit-issues-add-comment-dispatches ()
  "GIVEN a github backend resolved for the repo
WHEN calling shipit-issues--add-comment
THEN it dispatches to the backend's :add-comment function."
  (let ((called nil))
    (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 (cons (cdr (assq 'github shipit-issue-backends))
                       '(:repo "owner/repo"))))
              ((symbol-function 'shipit-issue-github--add-comment)
               (lambda (_config issue-id body)
                 (setq called (list issue-id body))
                 '((id . 1)))))
      (shipit-issues--add-comment "owner/repo" 42 "hello")
      (should (equal '(42 "hello") called)))))

;;; Thin Wrappers — edit-comment dispatches to backend

(ert-deftest test-shipit-issues-edit-comment-dispatches ()
  "GIVEN a github backend resolved for the repo
WHEN calling shipit-issues--edit-comment
THEN it dispatches to the backend's :edit-comment function."
  (let ((called nil))
    (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 (cons (cdr (assq 'github shipit-issue-backends))
                       '(:repo "owner/repo"))))
              ((symbol-function 'shipit-issue-github--edit-comment)
               (lambda (_config comment-id body)
                 (setq called (list comment-id body))
                 '((id . 100)))))
      (shipit-issues--edit-comment "owner/repo" 100 "updated")
      (should (equal '(100 "updated") called)))))

;;; Thin Wrappers — toggle-reaction dispatches to backend

(ert-deftest test-shipit-issues-toggle-reaction-dispatches ()
  "GIVEN a github backend resolved for the repo
WHEN calling shipit-issues--toggle-reaction
THEN it dispatches to the backend's :toggle-reaction function."
  (let ((called nil))
    (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 (cons (cdr (assq 'github shipit-issue-backends))
                       '(:repo "owner/repo"))))
              ((symbol-function 'shipit-issue-github--toggle-reaction)
               (lambda (_config comment-id reaction)
                 (setq called (list comment-id reaction))
                 '((id . 1)))))
      (shipit-issues--toggle-reaction "owner/repo" 100 "+1")
      (should (equal '(100 "+1") called)))))

;;; DWIM Handler — matcher recognizes issue-mode with comment-id

(ert-deftest test-shipit-issue-dwim-handler-matches-in-issue-mode ()
  "GIVEN point is in shipit-issue-mode with shipit-comment-id text property
WHEN the issue-comment DWIM matcher runs
THEN it returns non-nil."
  (require 'shipit-pr-sections)
  (require 'shipit-issues-buffer)
  (with-temp-buffer
    (shipit-issue-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "comment text" 'shipit-comment-id 123)))
    (goto-char (point-min))
    (let* ((handler (assq 'issue-comment shipit--dwim-handlers))
           (matcher (cdr (assq 'matcher (cdr handler)))))
      (should handler)
      (should (funcall matcher)))))

;;; DWIM Handler — matcher does NOT match in non-issue modes

(ert-deftest test-shipit-issue-dwim-handler-skips-non-issue-mode ()
  "GIVEN point is NOT in shipit-issue-mode
WHEN the issue-comment DWIM matcher runs
THEN it returns nil."
  (require 'shipit-pr-sections)
  (require 'shipit-issues-buffer)
  (with-temp-buffer
    (insert (propertize "comment text" 'shipit-comment-id 123))
    (goto-char (point-min))
    (let* ((handler (assq 'issue-comment shipit--dwim-handlers))
           (matcher (cdr (assq 'matcher (cdr handler)))))
      (should handler)
      (should-not (funcall matcher)))))

;;; Editor save — issue-comment type calls shipit-issues--add-comment

(ert-deftest test-shipit-editor-save-issue-comment ()
  "GIVEN the editor type is issue-comment
WHEN saving
THEN it calls shipit-issues--add-comment with repo, pr-number, content."
  (require 'shipit-editor)
  (let ((called nil))
    (cl-letf (((symbol-function 'shipit-issues--add-comment)
               (lambda (repo issue-number body)
                 (setq called (list repo issue-number body))
                 '((id . 1))))
              ((symbol-function 'shipit-editor--close)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-issue-buffer-refresh)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq shipit-editor--type 'issue-comment
              shipit-editor--repo "owner/repo"
              shipit-editor--pr-number 42
              shipit-editor--source-buffer nil)
        (insert "my comment")
        (shipit-editor-save)
        (should (equal '("owner/repo" 42 "my comment") called))))))

;;; Editor save — issue-comment-edit type calls shipit-issues--edit-comment

(ert-deftest test-shipit-editor-save-issue-comment-edit ()
  "GIVEN the editor type is issue-comment-edit
WHEN saving
THEN it calls shipit-issues--edit-comment with repo, comment-id, content."
  (require 'shipit-editor)
  (let ((called nil))
    (cl-letf (((symbol-function 'shipit-issues--edit-comment)
               (lambda (repo comment-id body)
                 (setq called (list repo comment-id body))
                 '((id . 100))))
              ((symbol-function 'shipit-editor--close)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-issue-buffer-refresh)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq shipit-editor--type 'issue-comment-edit
              shipit-editor--repo "owner/repo"
              shipit-editor--comment-id 100
              shipit-editor--source-buffer nil)
        (insert "updated comment")
        (shipit-editor-save)
        (should (equal '("owner/repo" 100 "updated comment") called))))))

;;; Jira Backend — toggle-reaction is a no-op

(ert-deftest test-shipit-issue-jira-toggle-reaction-noop ()
  "GIVEN the jira backend
WHEN calling :toggle-reaction
THEN it returns nil (no-op)."
  (let* ((backend (cdr (assq 'jira shipit-issue-backends)))
         (fn (plist-get backend :toggle-reaction))
         (config '(:base-url "https://jira.example.com")))
    (should (null (funcall fn config "12345" "+1")))))

;;; Draft issue comment section creation

(ert-deftest test-shipit-editor-draft-issue-comment-section ()
  "GIVEN an issue buffer with an issue-comments magit section
WHEN calling shipit-editor--create-draft-issue-comment-section
THEN it creates a draft issue-comment section inside the container."
  (require 'shipit-editor)
  (require 'shipit-issues-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-insert-section--parent nil))
      (magit-insert-section (shipit-issue)
        (magit-insert-section (issue-comments nil t)
          (magit-insert-heading "Comments (0)")
          (magit-insert-section-body
            (insert "   No comments\n")))))
    ;; Now call the function under test
    (let ((result (shipit-editor--create-draft-issue-comment-section)))
      ;; THEN it returns a plist with overlay info
      (should result)
      (should (plist-get result :overlay))
      (should (plist-get result :start))
      (should (plist-get result :end))
      ;; The overlay should be a live overlay
      (let ((ov (plist-get result :overlay)))
        (should (overlayp ov))
        (should (overlay-buffer ov))
        ;; Should be marked as new comment placeholder
        (should (overlay-get ov 'shipit-new-comment-placeholder))))))

;;; Edit existing issue comment finds section by id

(ert-deftest test-shipit-editor-issue-comment-preview-edit ()
  "GIVEN an issue buffer with a comment section having shipit-comment-id
WHEN calling shipit-editor--setup-issue-comment-preview with that id
THEN it finds the section and creates an overlay."
  (require 'shipit-editor)
  (require 'shipit-issues-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-insert-section--parent nil))
      (magit-insert-section (shipit-issue)
        (magit-insert-section (issue-comment 42)
          (let ((start (point)))
            (magit-insert-heading
              (propertize "   user  2h ago" 'shipit-comment-id 42))
            (add-text-properties start (point) '(shipit-comment-id 42)))
          (magit-insert-section-body
            (let ((body-start (point)))
              (insert "      Comment body here\n")
              (add-text-properties body-start (point) '(shipit-comment-id 42)))))))
    ;; Call the function under test
    (let ((result (shipit-editor--setup-issue-comment-preview 42)))
      ;; THEN it returns overlay info
      (should result)
      (should (plist-get result :overlay))
      (should (plist-get result :original-content)))))

;;; Issue description preview

(ert-deftest test-shipit-issue-description-preview ()
  "GIVEN an issue buffer with a shipit-issue-description section
WHEN calling shipit-editor--setup-issue-description-preview
THEN it creates an overlay on the description body."
  (require 'shipit-editor)
  (require 'shipit-issues-buffer)
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-insert-section--parent nil))
      (magit-insert-section (shipit-issue)
        (magit-insert-section (shipit-issue-description nil nil)
          (magit-insert-heading "Description:")
          (magit-insert-section-body
            (insert "   This is the issue body\n")
            (insert "   with multiple lines\n")))))
    ;; Call the function under test
    (let ((result (shipit-editor--setup-issue-description-preview)))
      ;; THEN it returns overlay info
      (should result)
      (should (plist-get result :overlay))
      (should (plist-get result :start))
      (should (plist-get result :end))
      (should (plist-get result :original-content))
      ;; The overlay should cover the body content
      (let ((ov (plist-get result :overlay)))
        (should (overlayp ov))
        (should (overlay-buffer ov))))))

;;; Render preview content uses 6-space indent for issue-comment

(ert-deftest test-shipit-render-preview-content-issue-comment-indent ()
  "GIVEN content for an issue-comment preview
WHEN rendering with shipit-editor--render-preview-content
THEN it uses 6-space indentation (same as general-comment)."
  (require 'shipit-editor)
  (let ((rendered (shipit-editor--render-preview-content "hello" 'issue-comment)))
    ;; THEN the output starts with 6 spaces
    (should (string-prefix-p "      " rendered))
    ;; Also check issue-comment-edit type
    (let ((rendered2 (shipit-editor--render-preview-content "hello" 'issue-comment-edit)))
      (should (string-prefix-p "      " rendered2)))))

;;; GitHub Backend — update-description

(ert-deftest test-shipit-issue-github-update-description ()
  "GIVEN the github backend
WHEN calling :update-description with repo, issue-number, and body
THEN it PATCHes to /repos/{repo}/issues/{number}."
  (let ((called-endpoint nil)
        (called-data nil)
        (called-method nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &optional method)
                 (setq called-endpoint endpoint
                       called-data data
                       called-method method)
                 '((id . 1) (body . "new desc")))))
      (let* ((backend (cdr (assq 'github shipit-issue-backends)))
             (fn (plist-get backend :update-description))
             (config '(:repo "owner/repo")))
        (funcall fn config 42 "new desc")
        (should (string= "/repos/owner/repo/issues/42" called-endpoint))
        (should (equal "new desc" (cdr (assq 'body called-data))))
        (should (string= "PATCH" called-method))))))

;;; Editor save — issue-description dispatches to update-description

(ert-deftest test-shipit-editor-save-issue-description ()
  "GIVEN the editor type is issue-description
WHEN saving
THEN it calls shipit-issues--update-description with repo, pr-number, content."
  (require 'shipit-editor)
  (let ((called nil))
    (cl-letf (((symbol-function 'shipit-issues--update-description)
               (lambda (repo issue-number body)
                 (setq called (list repo issue-number body))))
              ((symbol-function 'shipit-editor--close)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-issue-buffer-refresh)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq shipit-editor--type 'issue-description
              shipit-editor--repo "owner/repo"
              shipit-editor--pr-number 42
              shipit-editor--source-buffer nil)
        (insert "new description")
        (shipit-editor-save)
        (should (equal '("owner/repo" 42 "new description") called))))))

;;; Issue Description Reactions

(ert-deftest test-shipit-reaction-choices-shared ()
  "GIVEN the shared shipit-reaction-choices constant
WHEN accessed
THEN it contains 8 reaction types matching expected values."
  (require 'shipit-core)
  ;; THEN it should be an alist with 8 entries
  (should (listp shipit-reaction-choices))
  (should (= 8 (length shipit-reaction-choices)))
  ;; THEN each expected reaction type is present
  (let ((types (mapcar #'cdr shipit-reaction-choices)))
    (should (member "+1" types))
    (should (member "-1" types))
    (should (member "laugh" types))
    (should (member "confused" types))
    (should (member "heart" types))
    (should (member "hooray" types))
    (should (member "rocket" types))
    (should (member "eyes" types))))

(ert-deftest test-shipit-issue-description-reactions-render ()
  "GIVEN an issue buffer with cached reactions for the issue
WHEN the description section is rendered
THEN a reactions line is present with shipit-reactions text property."
  (require 'shipit-issues-buffer)
  (let ((shipit-current-repo "owner/repo")
        (shipit--reaction-cache (make-hash-table :test 'equal))
        (issue-data '((body . "Issue body text")
                      (title . "Test issue"))))
    ;; GIVEN cached reactions for issue #10
    (puthash "pr-10" '(((content . "+1") (user . ((login . "alice"))))) shipit--reaction-cache)
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t)
            (magit-insert-section--parent nil))
        (magit-insert-section (shipit-issue)
          ;; WHEN description section renders
          (shipit-issue--insert-description-section "owner/repo" issue-data 10)))
      ;; THEN a line with shipit-reactions property exists
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (< (point) (point-max)))
          (when (get-text-property (point) 'shipit-reactions)
            (setq found t))
          (forward-char 1))
        (should found)))))

(ert-deftest test-shipit-issue-description-reactions-display-update ()
  "GIVEN an issue buffer with a reactions line
WHEN shipit-issue--update-description-reactions-display is called
THEN the reactions line content is replaced."
  (require 'shipit-issues-buffer)
  (let ((shipit-current-repo "owner/repo")
        (shipit--reaction-cache (make-hash-table :test 'equal)))
    ;; GIVEN a buffer with issue mode and reactions line
    (with-temp-buffer
      (shipit-issue-mode)
      (setq shipit-issue-buffer-number 10
            shipit-issue-buffer-repo "owner/repo")
      (let ((inhibit-read-only t))
        (insert (propertize "   old-reactions\n"
                            'shipit-issue-description t
                            'shipit-issue-number 10
                            'shipit-repo "owner/repo"
                            'shipit-reactions t)))
      ;; GIVEN updated cached reactions
      (puthash "pr-10" '(((content . "heart") (user . ((login . "bob"))))) shipit--reaction-cache)
      ;; WHEN targeted update is called
      (shipit-issue--update-description-reactions-display 10)
      ;; THEN the line was replaced (no longer contains "old-reactions")
      (goto-char (point-min))
      (should-not (search-forward "old-reactions" nil t)))))

(ert-deftest test-shipit-issue-react-to-description-add ()
  "GIVEN no existing reaction on an issue description
WHEN shipit-issue--react-to-description is called
THEN shipit-issues--add-reaction is invoked with correct args."
  (require 'shipit-issues-buffer)
  (require 'shipit-issue-backends)
  (let ((added-args nil)
        (shipit--reaction-cache (make-hash-table :test 'equal)))
    ;; GIVEN empty reactions cache for issue #10
    (puthash "pr-10" '() shipit--reaction-cache)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "👍 +1"))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'shipit-issue--get-backend)
               (lambda ()
                 (list :name "Mock" :fetch-reactions #'ignore)))
              ((symbol-function 'shipit-issues--fetch-reactions)
               (lambda (_repo _num) '()))
              ((symbol-function 'shipit-issues--add-reaction)
               (lambda (repo issue-number reaction)
                 (setq added-args (list repo issue-number reaction))))
              ((symbol-function 'shipit-issues--remove-reaction)
               (lambda (&rest _) nil))
              ((symbol-function 'shipit-issue--update-description-reactions-display)
               (lambda (_n) nil)))
      (with-temp-buffer
        (shipit-issue-mode)
        (setq shipit-issue-buffer-number 10
              shipit-issue-buffer-repo "owner/repo")
        ;; WHEN react to description is called
        (shipit-issue--react-to-description)))
    ;; THEN add-reaction was called with repo, issue number and reaction
    (should (equal '("owner/repo" 10 "+1") added-args))))

(provide 'test-shipit-issue-comment-actions)
;;; test-shipit-issue-comment-actions.el ends here
