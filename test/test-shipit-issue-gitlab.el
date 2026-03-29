;;; test-shipit-issue-gitlab.el --- Tests for GitLab issue backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the GitLab issue backend normalization and registration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-gitlab)
(require 'shipit-gitlab-http)

;;; Registration Tests

(ert-deftest test-shipit-issue-gitlab-registered ()
  "GIVEN shipit-issue-gitlab is loaded
WHEN checking the backend registry
THEN 'gitlab is registered with name \"GitLab\"."
  (should (assq 'gitlab shipit-issue-backends))
  (should (string= "GitLab" (plist-get (cdr (assq 'gitlab shipit-issue-backends)) :name))))

(ert-deftest test-shipit-issue-gitlab-has-all-required-keys ()
  "GIVEN shipit-issue-gitlab is loaded
WHEN checking the backend plist
THEN all required keys from shipit-issue--required-keys are present."
  (let ((backend (cdr (assq 'gitlab shipit-issue-backends))))
    (dolist (key shipit-issue--required-keys)
      (should (plist-get backend key)))))

;;; Normalization Tests — Issues

(ert-deftest test-shipit-issue-gitlab-normalize-issue ()
  "GIVEN a GitLab API issue response
WHEN normalizing to shipit format
THEN key fields are mapped correctly."
  (let ((gitlab-data '((iid . 42)
                       (title . "Fix the login bug")
                       (state . "opened")
                       (description . "Steps to reproduce...")
                       (author . ((username . "alice")
                                  (avatar_url . "https://gitlab.com/alice.png")))
                       (assignees . [((username . "bob"))])
                       (labels . ("bug" "critical"))
                       (created_at . "2024-01-15T10:30:00.000Z")
                       (updated_at . "2024-01-20T14:00:00.000Z")
                       (web_url . "https://gitlab.com/group/project/-/issues/42")
                       (user_notes_count . 5))))
    (let ((normalized (shipit-issue-gitlab--normalize-issue gitlab-data)))
      (should (equal 42 (cdr (assq 'number normalized))))
      (should (equal 42 (cdr (assq 'id normalized))))
      (should (equal "Fix the login bug" (cdr (assq 'title normalized))))
      (should (equal "open" (cdr (assq 'state normalized))))
      (should (equal "Steps to reproduce..." (cdr (assq 'body normalized))))
      (should (equal "alice" (cdr (assq 'login (cdr (assq 'user normalized))))))
      (should (equal "https://gitlab.com/alice.png"
                     (cdr (assq 'avatar_url (cdr (assq 'user normalized))))))
      (should (equal "https://gitlab.com/group/project/-/issues/42"
                     (cdr (assq 'html_url normalized)))))))

(ert-deftest test-shipit-issue-gitlab-normalize-issue-state-opened ()
  "GIVEN a GitLab issue with state \"opened\"
WHEN normalizing
THEN state becomes \"open\" (GitHub convention)."
  (let* ((gitlab-data '((iid . 1) (title . "t") (state . "opened")
                        (description . "") (author . ((username . "a")))
                        (labels . ()) (created_at . "") (updated_at . "")
                        (web_url . "") (user_notes_count . 0)))
         (normalized (shipit-issue-gitlab--normalize-issue gitlab-data)))
    (should (equal "open" (cdr (assq 'state normalized))))))

(ert-deftest test-shipit-issue-gitlab-normalize-issue-state-closed ()
  "GIVEN a GitLab issue with state \"closed\"
WHEN normalizing
THEN state stays \"closed\"."
  (let* ((gitlab-data '((iid . 1) (title . "t") (state . "closed")
                        (description . "") (author . ((username . "a")))
                        (labels . ()) (created_at . "") (updated_at . "")
                        (web_url . "") (user_notes_count . 0)))
         (normalized (shipit-issue-gitlab--normalize-issue gitlab-data)))
    (should (equal "closed" (cdr (assq 'state normalized))))))

(ert-deftest test-shipit-issue-gitlab-normalize-issue-labels ()
  "GIVEN a GitLab issue with string labels
WHEN normalizing
THEN labels are converted to ((name . \"x\")) alist format."
  (let* ((gitlab-data '((iid . 1) (title . "t") (state . "opened")
                        (description . "") (author . ((username . "a")))
                        (labels . ("enhancement" "feature"))
                        (created_at . "") (updated_at . "")
                        (web_url . "") (user_notes_count . 0)))
         (normalized (shipit-issue-gitlab--normalize-issue gitlab-data))
         (labels (cdr (assq 'labels normalized))))
    (should (= 2 (length labels)))
    (should (equal "enhancement" (cdr (assq 'name (car labels)))))
    (should (equal "feature" (cdr (assq 'name (cadr labels)))))))

(ert-deftest test-shipit-issue-gitlab-normalize-issue-assignees ()
  "GIVEN a GitLab issue with assignees array
WHEN normalizing
THEN assignees are mapped to ((login . username)) format."
  (let* ((gitlab-data '((iid . 1) (title . "t") (state . "opened")
                        (description . "") (author . ((username . "a")))
                        (assignees . [((username . "bob") (avatar_url . "https://bob.png"))
                                      ((username . "carol"))])
                        (labels . ()) (created_at . "") (updated_at . "")
                        (web_url . "") (user_notes_count . 0)))
         (normalized (shipit-issue-gitlab--normalize-issue gitlab-data))
         (assignees (cdr (assq 'assignees normalized))))
    (should (= 2 (length assignees)))
    (should (equal "bob" (cdr (assq 'login (car assignees)))))))

(ert-deftest test-shipit-issue-gitlab-normalize-issue-nil-description ()
  "GIVEN a GitLab issue with nil description
WHEN normalizing
THEN body is empty string."
  (let* ((gitlab-data '((iid . 1) (title . "t") (state . "opened")
                        (description . nil) (author . ((username . "a")))
                        (labels . ()) (created_at . "") (updated_at . "")
                        (web_url . "") (user_notes_count . 0)))
         (normalized (shipit-issue-gitlab--normalize-issue gitlab-data)))
    (should (equal "" (cdr (assq 'body normalized))))))

;;; Normalization Tests — Comments

(ert-deftest test-shipit-issue-gitlab-normalize-comment ()
  "GIVEN a GitLab note (comment) response
WHEN normalizing
THEN fields are mapped to shipit format."
  (let* ((note '((id . 101)
                 (body . "Looks good to me")
                 (author . ((username . "alice")
                            (avatar_url . "https://alice.png")))
                 (created_at . "2024-01-16T10:00:00.000Z")
                 (updated_at . "2024-01-16T10:00:00.000Z")
                 (system . :json-false)))
         (normalized (shipit-issue-gitlab--normalize-comment note)))
    (should (equal 101 (cdr (assq 'id normalized))))
    (should (equal "Looks good to me" (cdr (assq 'body normalized))))
    (should (equal "alice" (cdr (assq 'login (cdr (assq 'user normalized))))))
    (should (equal "2024-01-16T10:00:00.000Z" (cdr (assq 'created_at normalized))))))

(ert-deftest test-shipit-issue-gitlab-filter-system-notes ()
  "GIVEN a list of GitLab notes including system notes
WHEN filtering
THEN system notes (system=t) are excluded."
  (let ((notes `(((id . 1) (body . "User comment") (system . :json-false)
                  (author . ((username . "alice")))
                  (created_at . "2024-01-16T10:00:00.000Z")
                  (updated_at . "2024-01-16T10:00:00.000Z"))
                 ((id . 2) (body . "changed the description") (system . t)
                  (author . ((username . "alice")))
                  (created_at . "2024-01-16T11:00:00.000Z")
                  (updated_at . "2024-01-16T11:00:00.000Z"))
                 ((id . 3) (body . "Another comment") (system . :json-false)
                  (author . ((username . "bob")))
                  (created_at . "2024-01-16T12:00:00.000Z")
                  (updated_at . "2024-01-16T12:00:00.000Z")))))
    (let ((filtered (shipit-issue-gitlab--filter-user-notes notes)))
      (should (= 2 (length filtered)))
      (should (equal 1 (cdr (assq 'id (car filtered)))))
      (should (equal 3 (cdr (assq 'id (cadr filtered))))))))

;;; Browse URL Tests

(ert-deftest test-shipit-issue-gitlab-browse-url ()
  "GIVEN a GitLab config with api-url and project-path
WHEN calling browse-url with id 42
THEN it returns the GitLab issue URL with /-/ separator."
  (let* ((backend (cdr (assq 'gitlab shipit-issue-backends)))
         (browse-url-fn (plist-get backend :browse-url))
         (config '(:api-url "https://gitlab.com"
                   :project-path "mygroup/myproject")))
    (should (string= "https://gitlab.com/mygroup/myproject/-/issues/42"
                     (funcall browse-url-fn config 42)))))

(ert-deftest test-shipit-issue-gitlab-browse-url-custom-host ()
  "GIVEN a GitLab config with a self-hosted URL
WHEN calling browse-url
THEN it uses the custom host."
  (let* ((backend (cdr (assq 'gitlab shipit-issue-backends)))
         (browse-url-fn (plist-get backend :browse-url))
         (config '(:api-url "https://gitlab.mycorp.com"
                   :project-path "team/repo")))
    (should (string= "https://gitlab.mycorp.com/team/repo/-/issues/7"
                     (funcall browse-url-fn config 7)))))

;;; Reference Pattern Tests

(ert-deftest test-shipit-issue-gitlab-reference-patterns-hash ()
  "GIVEN a GitLab config
WHEN getting reference patterns
THEN #N numeric pattern matches issue references."
  (let* ((backend (cdr (assq 'gitlab shipit-issue-backends)))
         (patterns-fn (plist-get backend :reference-patterns))
         (config '(:api-url "https://gitlab.com"
                   :project-path "mygroup/myproject"))
         (patterns (funcall patterns-fn config)))
    ;; Should have at least 2 patterns: #N and URL
    (should (>= (length patterns) 2))
    ;; First pattern: #N
    (let ((hash-pattern (nth 0 patterns)))
      (should (string-match (car hash-pattern) "See #42 for details"))
      (should (= 42 (funcall (nth 2 hash-pattern)
                              (match-string (nth 1 hash-pattern)
                                            "See #42 for details")))))))

(ert-deftest test-shipit-issue-gitlab-reference-patterns-url ()
  "GIVEN a GitLab config
WHEN getting reference patterns
THEN /-/issues/N URL pattern matches."
  (let* ((backend (cdr (assq 'gitlab shipit-issue-backends)))
         (patterns-fn (plist-get backend :reference-patterns))
         (config '(:api-url "https://gitlab.com"
                   :project-path "mygroup/myproject"))
         (patterns (funcall patterns-fn config))
         (url-pattern (nth 1 patterns))
         (test-url "https://gitlab.com/mygroup/myproject/-/issues/99"))
    (should (string-match (car url-pattern) test-url))
    (should (= 99 (funcall (nth 2 url-pattern)
                            (match-string (nth 1 url-pattern)
                                          test-url))))))

;;; ID Conversion Tests

(ert-deftest test-shipit-issue-gitlab-id-to-string ()
  "GIVEN the gitlab backend
WHEN converting issue id to string
THEN it returns \"#42\" format."
  (let* ((backend (cdr (assq 'gitlab shipit-issue-backends)))
         (id-to-string (plist-get backend :id-to-string)))
    (should (string= "#42" (funcall id-to-string 42)))))

(ert-deftest test-shipit-issue-gitlab-string-to-id ()
  "GIVEN the gitlab backend
WHEN converting \"#42\" to id
THEN it returns numeric 42."
  (let* ((backend (cdr (assq 'gitlab shipit-issue-backends)))
         (string-to-id (plist-get backend :string-to-id)))
    (should (= 42 (funcall string-to-id "#42")))))

;;; API Integration Tests (mocked)

(ert-deftest test-shipit-issue-gitlab-fetch-issue-calls-api ()
  "GIVEN a GitLab config
WHEN fetching an issue
THEN it calls the correct API endpoint and normalizes the result."
  (let ((captured-path nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (setq captured-path path)
                 '((iid . 42)
                   (title . "Test issue")
                   (state . "opened")
                   (description . "body")
                   (author . ((username . "alice")))
                   (labels . ())
                   (created_at . "2024-01-15T10:30:00.000Z")
                   (updated_at . "2024-01-15T10:30:00.000Z")
                   (web_url . "https://gitlab.com/g/p/-/issues/42")
                   (user_notes_count . 0)))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-issue-gitlab--fetch-issue config 42)))
        (should (string-match-p "/projects/.*/issues/42" captured-path))
        (should (equal 42 (cdr (assq 'number result))))
        (should (equal "open" (cdr (assq 'state result))))))))

(ert-deftest test-shipit-issue-gitlab-fetch-comments-filters-system ()
  "GIVEN a GitLab issue with system and user notes
WHEN fetching comments
THEN system notes are filtered out."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path)
               `[((id . 1) (body . "User note") (system . :json-false)
                  (author . ((username . "alice")))
                  (created_at . "2024-01-16T10:00:00.000Z")
                  (updated_at . "2024-01-16T10:00:00.000Z"))
                 ((id . 2) (body . "system note") (system . t)
                  (author . ((username . "alice")))
                  (created_at . "2024-01-16T11:00:00.000Z")
                  (updated_at . "2024-01-16T11:00:00.000Z"))])))
    (let* ((config '(:project-path "mygroup/myproject"))
           (comments (shipit-issue-gitlab--fetch-comments config 42)))
      (should (= 1 (length comments)))
      (should (equal "User note" (cdr (assq 'body (car comments))))))))

(ert-deftest test-shipit-issue-gitlab-add-comment-calls-api ()
  "GIVEN a GitLab config
WHEN adding a comment
THEN it calls POST on the correct endpoint."
  (let ((captured-path nil)
        (captured-data nil)
        (captured-method nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data method)
                 (setq captured-path path
                       captured-data data
                       captured-method method)
                 '((id . 201)
                   (body . "My comment")
                   (system . :json-false)
                   (author . ((username . "alice")))
                   (created_at . "2024-01-17T10:00:00.000Z")
                   (updated_at . "2024-01-17T10:00:00.000Z")))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-issue-gitlab--add-comment config 42 "My comment")))
        (should (string-match-p "/projects/.*/issues/42/notes" captured-path))
        (should (equal "POST" captured-method))
        (should (equal "My comment" (cdr (assq 'body captured-data))))
        (should (equal 201 (cdr (assq 'id result))))))))

(ert-deftest test-shipit-issue-gitlab-create-issue-calls-api ()
  "GIVEN a GitLab config
WHEN creating an issue
THEN it calls POST on the correct endpoint and normalizes result."
  (let ((captured-path nil)
        (captured-data nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data _method)
                 (setq captured-path path
                       captured-data data)
                 '((iid . 99)
                   (title . "New issue")
                   (state . "opened")
                   (description . "Issue body")
                   (author . ((username . "alice")))
                   (labels . ())
                   (created_at . "2024-01-18T10:00:00.000Z")
                   (updated_at . "2024-01-18T10:00:00.000Z")
                   (web_url . "https://gitlab.com/g/p/-/issues/99")
                   (user_notes_count . 0)))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-issue-gitlab--create-issue config "New issue" "Issue body")))
        (should (string-match-p "/projects/.*/issues$" captured-path))
        (should (equal "New issue" (cdr (assq 'title captured-data))))
        (should (equal "Issue body" (cdr (assq 'description captured-data))))
        (should (equal 99 (cdr (assq 'number result))))))))

;;; Backend-aware #NNN Reference Routing Tests

(require 'shipit-render)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-pr-gitlab)

(ert-deftest test-shipit-hash-reference-browse-url-github ()
  "GIVEN PR backend is github with repo \"owner/repo\"
WHEN building browse URL for #42
THEN returns https://github.com/owner/repo/issues/42."
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (should (string= "https://github.com/owner/repo/issues/42"
                     (shipit--hash-reference-browse-url "owner/repo" 42)))))

(ert-deftest test-shipit-hash-reference-browse-url-gitlab ()
  "GIVEN PR backend is gitlab with project-path config
WHEN building browse URL for #7
THEN returns GitLab issue URL with /-/issues/ path."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "mygroup/myproject")))
    (should (string= "https://gitlab.com/mygroup/myproject/-/issues/7"
                     (shipit--hash-reference-browse-url "mygroup/myproject" 7)))))

(ert-deftest test-shipit-hash-reference-browse-url-gitlab-self-hosted ()
  "GIVEN PR backend is gitlab with self-hosted URL
WHEN building browse URL for #99
THEN uses the custom host in the URL."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.mycorp.com"
                                    :project-path "team/repo")))
    (should (string= "https://gitlab.mycorp.com/team/repo/-/issues/99"
                     (shipit--hash-reference-browse-url "team/repo" 99)))))

(ert-deftest test-shipit-reference-is-pr-by-backend-github-detects-pr ()
  "GIVEN PR backend is github and API returns pull_request key
WHEN checking if #42 is a PR
THEN returns 'pr."
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (cl-letf (((symbol-function 'shipit--reference-detect-type)
               (lambda (_number _repo _type) 'pr)))
      (should (eq 'pr (shipit--reference-is-pr-by-backend 42 "owner/repo"))))))

(ert-deftest test-shipit-reference-is-pr-by-backend-github-detects-issue ()
  "GIVEN PR backend is github and API returns no pull_request key
WHEN checking if #42 is a PR
THEN returns 'issue."
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (cl-letf (((symbol-function 'shipit--reference-detect-type)
               (lambda (_number _repo _type) 'issue)))
      (should (eq 'issue (shipit--reference-is-pr-by-backend 42 "owner/repo"))))))

(ert-deftest test-shipit-reference-is-pr-by-backend-gitlab-returns-nil ()
  "GIVEN PR backend is gitlab
WHEN checking if #42 is a PR
THEN returns nil (GitLab uses !NNN for MRs, not #NNN)."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "g/p")))
    (should-not (shipit--reference-is-pr-by-backend 42 "g/p"))))

(ert-deftest test-shipit-reference-is-pr-by-backend-unknown-returns-nil ()
  "GIVEN PR backend without :detect-hash-reference
WHEN checking if #42 is a PR
THEN returns nil."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'bitbucket)
        (shipit-pr-backend-config nil))
    (shipit-pr-register-backend
     'bitbucket
     (list :name "Bitbucket"
           :fetch-pr #'ignore :search #'ignore :create-pr #'ignore
           :merge-pr #'ignore :update-pr #'ignore :fetch-reviews #'ignore
           :submit-review #'ignore :fetch-review-decision #'ignore
           :fetch-files #'ignore :fetch-commits #'ignore
           :fetch-checks #'ignore :browse-url #'ignore))
    (should-not (shipit--reference-is-pr-by-backend 42 "owner/repo"))))

(ert-deftest test-shipit-pass1-overlay-uses-hash-reference-action-menu ()
  "GIVEN text with #42 reference and gitlab PR backend
WHEN creating PR reference overlays
THEN the overlay keymap calls shipit--hash-reference-action-menu."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "g/p"))
        (shipit-issue-backend 'gitlab)
        (shipit-issue-backend-config '(:api-url "https://gitlab.com"
                                       :project-path "g/p"))
        (called-fn nil)
        (called-args nil))
    (cl-letf (((symbol-function 'shipit--hash-reference-action-menu)
               (lambda (&rest args)
                 (setq called-fn 'shipit--hash-reference-action-menu
                       called-args args)))
              ((symbol-function 'shipit--create-backend-reference-overlays)
               (lambda (&rest _args) nil)))
      (with-temp-buffer
        (insert "See #42 for details")
        (shipit--create-pr-reference-overlays "g/p" 1 (point-min) (point-max))
        ;; Find the overlay and invoke its RET keymap
        (let* ((ovs (overlays-in (point-min) (point-max)))
               (ov (car ovs))
               (km (overlay-get ov 'keymap))
               (ret-binding (lookup-key km (kbd "RET"))))
          (should ov)
          (should ret-binding)
          (funcall ret-binding)
          (should (eq 'shipit--hash-reference-action-menu called-fn))
          (should (= 42 (car called-args))))))))

(ert-deftest test-shipit-hash-reference-open-passes-pr-backend-to-issues ()
  "GIVEN PR backend is gitlab in a GitLab MR buffer
WHEN opening #468 via shipit--hash-reference-open-issue
THEN shipit-issues-open-buffer receives gitlab as backend-id
AND the GitLab backend-config is passed through."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "g/p"))
        (shipit-issues-enabled t)
        (captured-backend-id nil)
        (captured-backend-config nil))
    (cl-letf (((symbol-function 'shipit-issues-open-buffer)
               (lambda (_number _repo backend-id backend-config)
                 (setq captured-backend-id backend-id
                       captured-backend-config backend-config))))
      (shipit--hash-reference-open-issue 468 "g/p" "https://gitlab.com/g/p/-/issues/468")
      (should (eq 'gitlab captured-backend-id))
      (should (equal '(:api-url "https://gitlab.com" :project-path "g/p")
                     captured-backend-config)))))

(ert-deftest test-shipit-issue-reference-action-passes-pr-backend ()
  "GIVEN PR backend is gitlab in a GitLab MR buffer
WHEN shipit--issue-reference-action-menu opens an issue via RET
THEN shipit-issues-open-buffer receives gitlab as backend-id."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.com"
                                    :project-path "g/p"))
        (shipit-issues-enabled t)
        (captured-backend-id nil))
    (cl-letf (((symbol-function 'shipit-issues-open-buffer)
               (lambda (_id _repo backend-id &rest _)
                 (setq captured-backend-id backend-id)))
              ((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?\r)))
      (shipit--issue-reference-action-menu 99 "g/p" "https://gitlab.com/g/p/-/issues/99")
      (should (eq 'gitlab captured-backend-id)))))

(ert-deftest test-shipit-pass5-skips-hash-patterns-for-all-backends ()
  "GIVEN GitLab issue backend with #NNN reference pattern
WHEN creating backend reference overlays (pass 5)
THEN #NNN patterns are skipped (handled by pass 1)
AND non-#NNN patterns like URL patterns still create overlays."
  (let* ((shipit-issue-backend 'gitlab)
         (shipit-issue-backend-config '(:api-url "https://gitlab.com"
                                        :project-path "mygroup/myproject"))
         (found-count 0)
         (overlay-count 0))
    (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
               (lambda (_repo)
                 (let ((plist (cdr (assq 'gitlab shipit-issue-backends))))
                   (cons plist (list :api-url "https://gitlab.com"
                                     :project-path "mygroup/myproject"
                                     :backend 'gitlab
                                     :repo "mygroup/myproject"))))))
      (with-temp-buffer
        (insert "See #42 for details and https://gitlab.com/mygroup/myproject/-/issues/99 too")
        (shipit--create-backend-reference-overlays
         "mygroup/myproject" (point-min) (point-max)
         (lambda () (setq found-count (1+ found-count)))
         (lambda () (setq overlay-count (1+ overlay-count))))
        ;; #42 should be skipped (it's a #\d+ pattern, handled by pass 1)
        ;; The URL pattern should still create an overlay
        (should (= 1 overlay-count))
        ;; Verify the overlay is for the URL, not for #42
        (let ((ovs (overlays-in (point-min) (point-max))))
          (should (= 1 (length ovs)))
          (should (string-match-p "issues/99"
                                  (buffer-substring
                                   (overlay-start (car ovs))
                                   (overlay-end (car ovs))))))))))

;;; curl transport tests

(ert-deftest test-shipit-gitlab-curl-request-parses-json ()
  "GIVEN a curl call that returns valid JSON
WHEN calling shipit-gitlab--curl-request
THEN returns parsed alist."
  (require 'shipit-gitlab-http)
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer _display &rest _args)
               (with-current-buffer buffer
                 (insert "{\"id\": 42, \"title\": \"test\"}"))
               0)))
    (let* ((config '(:api-url "https://gitlab.com" :project-path "g/p" :token "tok"))
           (result (shipit-gitlab--curl-request config "/test" "GET")))
      ;; THEN result has parsed id and title
      (should (= 42 (cdr (assq 'id result))))
      (should (string= "test" (cdr (assq 'title result)))))))

;;; Changelog / Timeline Tests

(ert-deftest test-shipit-issue-gitlab-fetch-issue-includes-changelog ()
  "GIVEN a GitLab issue with system notes for activity
WHEN fetching the issue
THEN the result includes a changelog key with normalized entries."
  (let ((api-call-count 0))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (setq api-call-count (1+ api-call-count))
                 (cond
                  ;; Issue fetch
                  ((string-match-p "/issues/42$" path)
                   '((iid . 42)
                     (title . "Test issue")
                     (state . "opened")
                     (description . "body")
                     (author . ((username . "alice")))
                     (labels . ())
                     (created_at . "2024-01-15T10:30:00.000Z")
                     (updated_at . "2024-01-15T10:30:00.000Z")
                     (web_url . "https://gitlab.com/g/p/-/issues/42")
                     (user_notes_count . 0)))
                  ;; Notes fetch for timeline
                  ((string-match-p "/notes" path)
                   `[((id . 100) (body . "added ~bug label") (system . t)
                      (author . ((username . "bob")))
                      (created_at . "2024-01-16T10:00:00.000Z")
                      (updated_at . "2024-01-16T10:00:00.000Z"))
                     ((id . 101) (body . "User comment") (system . :json-false)
                      (author . ((username . "carol")))
                      (created_at . "2024-01-16T11:00:00.000Z")
                      (updated_at . "2024-01-16T11:00:00.000Z"))])
                  ;; State events
                  ((string-match-p "resource_state_events" path)
                   `[((id . 200) (state . "closed")
                      (user . ((username . "alice")))
                      (created_at . "2024-01-17T10:00:00.000Z"))])))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-issue-gitlab--fetch-issue config 42)))
        ;; Should have changelog key
        (should (assq 'changelog result))
        (let ((changelog (cdr (assq 'changelog result))))
          ;; 1 system note (labeled) + 1 state event (closed) + 1 user comment = 3 entries
          (should (= 3 (length changelog))))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-closed ()
  "GIVEN a system note body starting with \"closed\"
WHEN parsing
THEN returns status change item."
  (let ((result (shipit-issue-gitlab--parse-system-note "closed")))
    (should result)
    (should (equal "status" (cdr (assq 'field result))))
    (should (equal "closed" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-labeled ()
  "GIVEN a system note body \"added ~enhancement label\"
WHEN parsing
THEN returns labels change item."
  (let ((result (shipit-issue-gitlab--parse-system-note "added ~enhancement label")))
    (should result)
    (should (equal "labels" (cdr (assq 'field result))))
    (should (equal "enhancement" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-assigned ()
  "GIVEN a system note body \"assigned to @alice\"
WHEN parsing
THEN returns assignee change item."
  (let ((result (shipit-issue-gitlab--parse-system-note "assigned to @alice")))
    (should result)
    (should (equal "assignee" (cdr (assq 'field result))))
    (should (equal "alice" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-title-changed ()
  "GIVEN a system note body \"changed title from **old** to **new**\"
WHEN parsing
THEN returns title change item with from/to."
  (let ((result (shipit-issue-gitlab--parse-system-note
                 "changed title from **Old Title** to **New Title**")))
    (should result)
    (should (equal "title" (cdr (assq 'field result))))
    (should (equal "Old Title" (cdr (assq 'from result))))
    (should (equal "New Title" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-description-changed ()
  "GIVEN a system note body \"changed the description\"
WHEN parsing
THEN returns description change item."
  (let ((result (shipit-issue-gitlab--parse-system-note "changed the description")))
    (should result)
    (should (equal "description" (cdr (assq 'field result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-milestone-changed ()
  "GIVEN a system note body \"changed milestone to %v1.0\"
WHEN parsing
THEN returns milestone change item."
  (let ((result (shipit-issue-gitlab--parse-system-note "changed milestone to %v1.0")))
    (should result)
    (should (equal "milestone" (cdr (assq 'field result))))
    (should (equal "v1.0" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-milestone-removed ()
  "GIVEN a system note body \"removed milestone\"
WHEN parsing
THEN returns milestone removal item."
  (let ((result (shipit-issue-gitlab--parse-system-note "removed milestone")))
    (should result)
    (should (equal "milestone" (cdr (assq 'field result))))
    (should (string-empty-p (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-set-status ()
  "GIVEN a system note body \"set status to **Draft**\"
WHEN parsing
THEN returns status change item with bold markers stripped."
  (let ((result (shipit-issue-gitlab--parse-system-note "set status to **Draft**")))
    (should result)
    (should (equal "status" (cdr (assq 'field result))))
    (should (equal "Draft" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-mentioned-in-mr ()
  "GIVEN a system note body \"mentioned in merge request !537\"
WHEN parsing
THEN returns mentioned-in-mr item with MR number."
  (let ((result (shipit-issue-gitlab--parse-system-note "mentioned in merge request !537")))
    (should result)
    (should (equal "mentioned" (cdr (assq 'field result))))
    (should (equal "!537" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-created-branch ()
  "GIVEN a system note body with markdown link format for branch
WHEN parsing
THEN extracts just the branch name from the markdown link."
  (let ((result (shipit-issue-gitlab--parse-system-note
                 "created branch [`352-evolve-triple-instance`](/DPIPE/repo/-/compare/dev...352-evolve-triple-instance) to address this issue")))
    (should result)
    (should (equal "branch" (cdr (assq 'field result))))
    (should (equal "352-evolve-triple-instance" (cdr (assq 'to result))))))

(ert-deftest test-shipit-issue-gitlab-parse-system-note-unknown ()
  "GIVEN a system note body with unrecognized text
WHEN parsing
THEN returns nil."
  (should-not (shipit-issue-gitlab--parse-system-note "some random system note")))

(ert-deftest test-shipit-issue-gitlab-timeline-includes-user-comments ()
  "GIVEN a GitLab issue with user notes
WHEN fetching timeline
THEN user comments appear as 'commented' activity entries."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config path)
               (cond
                ((string-match-p "/notes" path)
                 `[((id . 100) (body . "assigned to @alice") (system . t)
                    (author . ((username . "bob")))
                    (created_at . "2024-01-16T10:00:00.000Z"))
                   ((id . 101) (body . "Great work on this") (system . :json-false)
                    (author . ((username . "carol")))
                    (created_at . "2024-01-16T11:00:00.000Z"))])
                ((string-match-p "resource_state_events" path)
                 '())))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (result (shipit-issue-gitlab--fetch-timeline config 42)))
      ;; THEN 1 system note + 1 user comment = 2 entries
      (should (= 2 (length result)))
      ;; The user comment entry should have field=comment
      (let* ((comment-entry (seq-find (lambda (e)
                                        (let ((items (cdr (assq 'items e))))
                                          (equal "comment" (cdr (assq 'field (car items))))))
                                      result)))
        (should comment-entry)
        (should (equal "carol" (cdr (assq 'login (cdr (assq 'user comment-entry))))))))))

(ert-deftest test-shipit-issue-gitlab-normalize-timeline-event ()
  "GIVEN a GitLab system note for a label add
WHEN normalizing to timeline event
THEN returns properly structured changelog entry."
  (let* ((note '((id . 100)
                 (body . "added ~bug label")
                 (system . t)
                 (author . ((username . "bob") (avatar_url . "https://b.png")))
                 (created_at . "2024-01-16T10:00:00.000Z")
                 (updated_at . "2024-01-16T10:00:00.000Z")))
         (entry (shipit-issue-gitlab--normalize-timeline-event note)))
    (should entry)
    (should (equal "system-100" (cdr (assq 'id entry))))
    (should (equal "bob" (cdr (assq 'login (cdr (assq 'user entry))))))
    (let ((items (cdr (assq 'items entry))))
      (should (= 1 (length items)))
      (should (equal "labels" (cdr (assq 'field (car items)))))
      (should (equal "bug" (cdr (assq 'to (car items))))))))

;;; Format changelog items tests (activity rendering for new field types)

(require 'shipit-issues-buffer)

(ert-deftest test-shipit-issue-format-changelog-comment ()
  "GIVEN a changelog item with field=comment
WHEN formatting
THEN shows 'commented' text."
  (let ((items '(((field . "comment")))))
    (should (string-match-p "commented"
                            (shipit-issue--format-changelog-items items)))))

(ert-deftest test-shipit-issue-format-changelog-mentioned ()
  "GIVEN a changelog item with field=mentioned and to=!537
WHEN formatting
THEN shows 'mentioned in !537'."
  (let ((items '(((field . "mentioned") (from . "") (to . "!537")))))
    (should (string-match-p "mentioned in.*!537"
                            (shipit-issue--format-changelog-items items)))))

(ert-deftest test-shipit-issue-format-changelog-branch ()
  "GIVEN a changelog item with field=branch
WHEN formatting
THEN shows 'created branch ...'."
  (let ((items '(((field . "branch") (from . "") (to . "feature-branch")))))
    (should (string-match-p "created branch.*feature-branch"
                            (shipit-issue--format-changelog-items items)))))

(ert-deftest test-shipit-issue-format-changelog-title ()
  "GIVEN a changelog item with field=title and from/to values
WHEN formatting
THEN shows 'changed Title: old → new'."
  (let ((items '(((field . "title") (from . "Old") (to . "New")))))
    (should (string-match-p "changed.*Title.*Old.*New"
                            (shipit-issue--format-changelog-items items)))))

;;; Reaction Function Tests

(ert-deftest test-shipit-issue-gitlab-fetch-reactions ()
  "GIVEN a GitLab config
WHEN fetching reactions for issue 42
THEN it calls the award_emoji endpoint and normalizes results."
  (let ((captured-path nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (setq captured-path path)
                 `[((id . 1) (name . "thumbsup")
                    (user . ((username . "alice"))))
                   ((id . 2) (name . "thumbsdown")
                    (user . ((username . "bob"))))])))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-issue-gitlab--fetch-reactions config 42)))
        (should (string-match-p "/projects/.*/issues/42/award_emoji" captured-path))
        (should (= 2 (length result)))
        ;; Verify normalization: thumbsup → +1
        (should (equal "+1" (cdr (assq 'content (car result)))))
        (should (equal "alice" (cdr (assq 'login (cdr (assq 'user (car result)))))))))))

(ert-deftest test-shipit-issue-gitlab-add-reaction ()
  "GIVEN a GitLab config
WHEN adding a reaction to issue 42
THEN it POSTs to the award_emoji endpoint with name field."
  (let ((captured-path nil)
        (captured-data nil)
        (captured-method nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data method)
                 (setq captured-path path
                       captured-data data
                       captured-method method)
                 '((id . 99) (name . "thumbsup")
                   (user . ((username . "alice")))))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-issue-gitlab--add-reaction config 42 "+1")))
        (should (string-match-p "/projects/.*/issues/42/award_emoji" captured-path))
        (should (equal "POST" captured-method))
        ;; GitLab uses name, not content
        (should (equal "thumbsup" (cdr (assq 'name captured-data))))
        ;; Normalized result
        (should (equal "+1" (cdr (assq 'content result))))))))

(ert-deftest test-shipit-issue-gitlab-remove-reaction ()
  "GIVEN a GitLab config
WHEN removing reaction 99 from issue 42
THEN it calls DELETE on the award_emoji endpoint."
  (let ((captured-path nil)
        (captured-method nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path _data method)
                 (setq captured-path path
                       captured-method method)
                 nil)))
      (let ((config '(:project-path "mygroup/myproject")))
        (shipit-issue-gitlab--remove-reaction config 42 99)
        (should (string-match-p "/projects/.*/issues/42/award_emoji/99" captured-path))
        (should (equal "DELETE" captured-method))))))

(ert-deftest test-shipit-issue-gitlab-registered-with-reactions ()
  "GIVEN shipit-issue-gitlab is loaded
WHEN checking the backend registry
THEN :fetch-reactions, :add-reaction, :remove-reaction are present."
  (let ((backend (cdr (assq 'gitlab shipit-issue-backends))))
    (should (plist-get backend :fetch-reactions))
    (should (plist-get backend :add-reaction))
    (should (plist-get backend :remove-reaction))))

;;; Truly-async issue comment tests

(ert-deftest test/issue-gitlab-fetch-comments-async ()
  "GIVEN a mocked shipit-gitlab--api-request-async
WHEN calling shipit-issue-gitlab--fetch-comments-async
THEN callback receives filtered and normalized comments."
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-async)
               (lambda (_config _path cb)
                 (funcall cb (vector
                              '((id . 1) (body . "User note") (system . :json-false)
                                (author . ((username . "alice")
                                           (avatar_url . "https://alice.png")))
                                (created_at . "2024-01-16T10:00:00.000Z")
                                (updated_at . "2024-01-16T10:00:00.000Z"))
                              '((id . 2) (body . "system note") (system . t)
                                (author . ((username . "bot")))
                                (created_at . "2024-01-16T11:00:00.000Z")
                                (updated_at . "2024-01-16T11:00:00.000Z"))
                              '((id . 3) (body . "Another comment") (system . :json-false)
                                (author . ((username . "bob")
                                           (avatar_url . "https://bob.png")))
                                (created_at . "2024-01-16T12:00:00.000Z")
                                (updated_at . "2024-01-16T12:00:00.000Z")))))))
      (shipit-issue-gitlab--fetch-comments-async
       config 42 (lambda (data) (setq callback-result data))))
    ;; THEN system notes filtered out, 2 user comments remain
    (should (not (eq 'not-called callback-result)))
    (should (= 2 (length callback-result)))
    (should (equal 1 (cdr (assq 'id (car callback-result)))))
    (should (equal "User note" (cdr (assq 'body (car callback-result)))))
    (should (equal 3 (cdr (assq 'id (cadr callback-result)))))))

(ert-deftest test/issue-gitlab-fetch-comments-async-nil ()
  "GIVEN a mocked async request that returns nil
WHEN calling shipit-issue-gitlab--fetch-comments-async
THEN callback receives nil."
  (let ((callback-result 'not-called)
        (config '(:api-url "https://gitlab.com" :token "test"
                  :project-path "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-async)
               (lambda (_config _path cb) (funcall cb nil))))
      (shipit-issue-gitlab--fetch-comments-async
       config 42 (lambda (data) (setq callback-result data))))
    (should (not (eq 'not-called callback-result)))
    (should-not callback-result)))

;;; Notification (Todos API) Tests

(ert-deftest test-shipit-issue-gitlab-todo-to-activity-mr ()
  "GIVEN a GitLab todo with target_type MergeRequest
WHEN converting to activity
THEN type is \"pr\" and all fields are mapped correctly."
  (let* ((todo '((target . ((iid . 123)
                             (title . "Add feature X")
                             (web_url . "https://gitlab.com/g/p/-/merge_requests/123")))
                 (target_type . "MergeRequest")
                 (action_name . "assigned")
                 (project . ((path_with_namespace . "mygroup/myproject")))
                 (updated_at . "2026-02-15T10:00:00.000Z")))
         (activity (shipit-issue-gitlab--todo-to-activity todo)))
    (should (equal 123 (cdr (assq 'number activity))))
    (should (equal "pr" (cdr (assq 'type activity))))
    (should (equal "Add feature X" (cdr (assq 'subject activity))))
    (should (equal "assign" (cdr (assq 'reason activity))))
    (should (equal "mygroup/myproject" (cdr (assq 'repo activity))))
    (should (equal "https://gitlab.com/g/p/-/merge_requests/123"
                   (cdr (assq 'browse-url activity))))
    (should (equal "2026-02-15T10:00:00.000Z" (cdr (assq 'updated-at activity))))
    (should (eq 'gitlab (cdr (assq 'source activity))))
    (should (eq 'gitlab (cdr (assq 'backend-id activity))))
    ;; backend-config carries project-path for issue buffer opening
    (let ((cfg (cdr (assq 'backend-config activity))))
      (should (equal "https://gitlab.com" (plist-get cfg :api-url)))
      (should (equal "mygroup/myproject" (plist-get cfg :project-path))))))

(ert-deftest test-shipit-issue-gitlab-todo-to-activity-issue ()
  "GIVEN a GitLab todo with target_type Issue
WHEN converting to activity
THEN type is \"issue\" and backend-config has project-path."
  (let* ((todo '((target . ((iid . 42)
                             (title . "Bug report")
                             (web_url . "https://gitlab.com/g/p/-/issues/42")))
                 (target_type . "Issue")
                 (action_name . "mentioned")
                 (project . ((path_with_namespace . "mygroup/myproject")))
                 (updated_at . "2026-02-15T11:00:00.000Z")))
         (activity (shipit-issue-gitlab--todo-to-activity todo)))
    (should (equal "issue" (cdr (assq 'type activity))))
    (should (equal 42 (cdr (assq 'number activity))))
    (should (equal "mention" (cdr (assq 'reason activity))))
    (let ((cfg (cdr (assq 'backend-config activity))))
      (should (equal "mygroup/myproject" (plist-get cfg :project-path))))))

(ert-deftest test-shipit-issue-gitlab-map-todo-action ()
  "GIVEN various GitLab todo action_name values
WHEN mapping to notification reasons
THEN each maps to the expected string."
  (should (equal "assign" (shipit-issue-gitlab--map-todo-action "assigned")))
  (should (equal "mention" (shipit-issue-gitlab--map-todo-action "mentioned")))
  (should (equal "mention" (shipit-issue-gitlab--map-todo-action "directly_addressed")))
  (should (equal "review_requested" (shipit-issue-gitlab--map-todo-action "approval_required")))
  (should (equal "review_requested" (shipit-issue-gitlab--map-todo-action "review_requested")))
  (should (equal "ci_activity" (shipit-issue-gitlab--map-todo-action "build_failed")))
  (should (equal "updated" (shipit-issue-gitlab--map-todo-action "unmergeable")))
  (should (equal "updated" (shipit-issue-gitlab--map-todo-action "marked")))
  (should (equal "updated" (shipit-issue-gitlab--map-todo-action "merge_train_removed")))
  (should (equal "updated" (shipit-issue-gitlab--map-todo-action "some_unknown_action"))))

(ert-deftest test-shipit-issue-gitlab-fetch-notifications ()
  "GIVEN a mocked GitLab API returning two todos and events disabled
WHEN calling fetch-notifications
THEN returns a list of two activity alists from Todos only."
  (let ((shipit-gitlab-notifications-include-events nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (when (string-match-p "/todos" path)
                   (vector
                    '((target . ((iid . 10)
                                 (title . "MR Ten")
                                 (web_url . "https://gitlab.com/g/p/-/merge_requests/10")))
                      (target_type . "MergeRequest")
                      (action_name . "review_requested")
                      (project . ((path_with_namespace . "g/p")))
                      (updated_at . "2026-02-15T12:00:00.000Z"))
                    '((target . ((iid . 5)
                                 (title . "Issue Five")
                                 (web_url . "https://gitlab.com/g/p/-/issues/5")))
                      (target_type . "Issue")
                      (action_name . "assigned")
                      (project . ((path_with_namespace . "g/p")))
                      (updated_at . "2026-02-15T12:01:00.000Z")))))))
      (let* ((config '(:api-url "https://gitlab.com" :project-path "g/p"))
             (result (shipit-issue-gitlab--fetch-notifications config nil)))
        (should (= 2 (length result)))
        (should (equal "pr" (cdr (assq 'type (car result)))))
        (should (equal "issue" (cdr (assq 'type (cadr result)))))))))

(ert-deftest test-shipit-issue-gitlab-notifications-registered ()
  "GIVEN shipit-issue-gitlab is loaded
WHEN checking the backend plist
THEN :notifications key is present."
  (let ((backend (cdr (assq 'gitlab shipit-issue-backends))))
    (should (plist-get backend :notifications))))

;;; Auto-discover Notification Backend Tests

(require 'shipit-notifications)

(ert-deftest test-shipit-autodiscover-finds-gitlab-with-token ()
  "GIVEN auth-source returns a token for gitlab.com
WHEN calling shipit--autodiscover-notification-backends
THEN returns a (\"gitlab.com\" :backend gitlab :api-url ...) entry."
  (let ((shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit-gitlab--auth-header)
               (lambda (_config) '("PRIVATE-TOKEN" . "glpat-fake-token"))))
      (let ((result (shipit--autodiscover-notification-backends)))
        (should (= 1 (length result)))
        (should (equal "gitlab.com" (car (car result))))
        (should (eq 'gitlab (plist-get (cdr (car result)) :backend)))
        (should (equal "https://gitlab.com" (plist-get (cdr (car result)) :api-url)))))))

(ert-deftest test-shipit-autodiscover-skips-when-no-token ()
  "GIVEN auth-source returns nil for gitlab.com
WHEN calling shipit--autodiscover-notification-backends
THEN returns empty list."
  (let ((shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit-gitlab--auth-header)
               (lambda (_config) nil)))
      (let ((result (shipit--autodiscover-notification-backends)))
        (should (= 0 (length result)))))))

(ert-deftest test-shipit-autodiscover-skips-when-already-in-repo-backends ()
  "GIVEN shipit-issue-repo-backends already has a gitlab entry
WHEN calling shipit--autodiscover-notification-backends
THEN returns empty list (no duplicate)."
  (let ((shipit-issue-repo-backends
         '(("mygroup/myproject" :backend gitlab
            :api-url "https://gitlab.com"
            :project-path "mygroup/myproject"))))
    ;; Should never even call auth-header
    (cl-letf (((symbol-function 'shipit-gitlab--auth-header)
               (lambda (_config) (error "Should not be called"))))
      (let ((result (shipit--autodiscover-notification-backends)))
        (should (= 0 (length result)))))))

(ert-deftest test-shipit-poll-backend-notifications-includes-autodiscovered ()
  "GIVEN shipit-issue-repo-backends is empty
AND auto-discover returns a gitlab entry with a token
WHEN shipit--poll-backend-notifications runs
THEN shipit-issue-gitlab--fetch-notifications is called."
  (let ((shipit-issue-repo-backends nil)
        (shipit--backend-last-poll-time nil)
        (shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--last-notification-count 0)
        (fetch-called nil))
    (cl-letf (((symbol-function 'shipit-gitlab--auth-header)
               (lambda (_config) '("PRIVATE-TOKEN" . "glpat-fake-token")))
              ((symbol-function 'shipit-issue-gitlab--fetch-notifications)
               (lambda (_config _since)
                 (setq fetch-called t)
                 nil)))
      (shipit--poll-backend-notifications)
      (should fetch-called))))

;;; Events API Integration Tests

(ert-deftest test-shipit-issue-gitlab-fetch-current-username ()
  "GIVEN a mocked GET /user returning username \"me\"
WHEN calling shipit-issue-gitlab--fetch-current-username twice
THEN returns \"me\" both times and only fetches once."
  (let ((shipit-issue-gitlab--user-cache nil)
        (fetch-count 0))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (when (string-match-p "^/user$" path)
                   (setq fetch-count (1+ fetch-count))
                   '((username . "me"))))))
      (let ((config '(:api-url "https://gitlab.com" :project-path "g/p")))
        ;; WHEN first call
        (should (equal "me" (shipit-issue-gitlab--fetch-current-username config)))
        ;; WHEN second call
        (should (equal "me" (shipit-issue-gitlab--fetch-current-username config)))
        ;; THEN only one API call
        (should (= 1 fetch-count))))))

(ert-deftest test-shipit-issue-gitlab-event-to-activity-comment ()
  "GIVEN a comment event on MergeRequest !42 in project_id 1
WHEN converting to activity with project cache {1 → \"group/project\"}
THEN activity has type=pr, number=42, reason=comment, repo=group/project."
  (let ((shipit-issue-gitlab--project-cache (make-hash-table :test 'equal)))
    (puthash 1 "group/project" shipit-issue-gitlab--project-cache)
    (let* ((event '((action_name . "commented on")
                    (target_type . "MergeRequest")
                    (target_iid . 42)
                    (target_title . "Fix bug")
                    (project_id . 1)
                    (created_at . "2026-02-15T10:00:00.000Z")
                    (author . ((username . "alice")))))
           (activity (shipit-issue-gitlab--event-to-activity event)))
      (should (equal "pr" (cdr (assq 'type activity))))
      (should (equal 42 (cdr (assq 'number activity))))
      (should (equal "comment" (cdr (assq 'reason activity))))
      (should (equal "group/project" (cdr (assq 'repo activity))))
      (should (equal "Fix bug" (cdr (assq 'subject activity))))
      (should (eq 'gitlab (cdr (assq 'source activity)))))))

(ert-deftest test-shipit-issue-gitlab-event-to-activity-merged ()
  "GIVEN a merged event on MergeRequest !55
WHEN converting to activity
THEN reason is \"merged\"."
  (let ((shipit-issue-gitlab--project-cache (make-hash-table :test 'equal)))
    (puthash 2 "team/repo" shipit-issue-gitlab--project-cache)
    (let* ((event '((action_name . "merged")
                    (target_type . "MergeRequest")
                    (target_iid . 55)
                    (target_title . "Feature Y")
                    (project_id . 2)
                    (created_at . "2026-02-15T11:00:00.000Z")
                    (author . ((username . "bob")))))
           (activity (shipit-issue-gitlab--event-to-activity event)))
      (should (equal "merged" (cdr (assq 'reason activity))))
      (should (equal "pr" (cdr (assq 'type activity))))
      (should (equal 55 (cdr (assq 'number activity)))))))

(ert-deftest test-shipit-issue-gitlab-event-noteable-type-direct ()
  "GIVEN an event with target_type MergeRequest (direct)
WHEN calling event-noteable-type
THEN returns MergeRequest."
  (let ((event '((target_type . "MergeRequest") (target_iid . 42))))
    (should (equal "MergeRequest"
                   (shipit-issue-gitlab--event-noteable-type event)))))

(ert-deftest test-shipit-issue-gitlab-event-noteable-type-diffnote ()
  "GIVEN an event with target_type DiffNote and note.noteable_type MergeRequest
WHEN calling event-noteable-type
THEN returns MergeRequest from the nested note."
  (let ((event '((target_type . "DiffNote")
                 (note . ((noteable_type . "MergeRequest")
                          (noteable_iid . 1))))))
    (should (equal "MergeRequest"
                   (shipit-issue-gitlab--event-noteable-type event)))))

(ert-deftest test-shipit-issue-gitlab-event-noteable-iid-direct ()
  "GIVEN an event with target_type MergeRequest and target_iid 42
WHEN calling event-noteable-iid
THEN returns 42."
  (let ((event '((target_type . "MergeRequest") (target_iid . 42))))
    (should (equal 42 (shipit-issue-gitlab--event-noteable-iid event)))))

(ert-deftest test-shipit-issue-gitlab-event-noteable-iid-note ()
  "GIVEN an event with target_type Note and note.noteable_iid 7
WHEN calling event-noteable-iid
THEN returns 7 from the nested note."
  (let ((event '((target_type . "Note")
                 (note . ((noteable_type . "Issue")
                          (noteable_iid . 7))))))
    (should (equal 7 (shipit-issue-gitlab--event-noteable-iid event)))))

(ert-deftest test-shipit-issue-gitlab-event-relevant-p-diffnote-mr ()
  "GIVEN a DiffNote event whose note.noteable_type is MergeRequest
WHEN calling event-relevant-p
THEN returns non-nil."
  (let ((event '((target_type . "DiffNote")
                 (note . ((noteable_type . "MergeRequest")
                          (noteable_iid . 1))))))
    (should (shipit-issue-gitlab--event-relevant-p event))))

(ert-deftest test-shipit-issue-gitlab-event-relevant-p-note-no-iid ()
  "GIVEN a Note event with no noteable_iid
WHEN calling event-relevant-p
THEN returns nil."
  (let ((event '((target_type . "Note")
                 (note . ((noteable_type . "Snippet"))))))
    (should-not (shipit-issue-gitlab--event-relevant-p event))))

(ert-deftest test-shipit-issue-gitlab-event-to-activity-diffnote ()
  "GIVEN a DiffNote event on MergeRequest !1 in project_id 1
WHEN converting to activity with project cache
THEN activity has type=pr, number=1, reason=comment."
  (let ((shipit-issue-gitlab--project-cache (make-hash-table :test 'equal)))
    (puthash 1 "group/project" shipit-issue-gitlab--project-cache)
    (let* ((event '((action_name . "commented on")
                    (target_type . "DiffNote")
                    (target_title . "Fix bug")
                    (project_id . 1)
                    (created_at . "2026-02-15T10:00:00.000Z")
                    (author . ((username . "alice")))
                    (note . ((noteable_type . "MergeRequest")
                             (noteable_iid . 1)))))
           (activity (shipit-issue-gitlab--event-to-activity event)))
      (should (equal "pr" (cdr (assq 'type activity))))
      (should (equal 1 (cdr (assq 'number activity))))
      (should (equal "comment" (cdr (assq 'reason activity))))
      (should (equal "group/project" (cdr (assq 'repo activity)))))))

(ert-deftest test-shipit-issue-gitlab-events-filter-irrelevant-types ()
  "GIVEN events with target_type \"Project\" and \"MergeRequest\"
WHEN fetching events
THEN only MergeRequest and Issue events are kept."
  (let ((shipit-issue-gitlab--project-cache (make-hash-table :test 'equal))
        (shipit-issue-gitlab--project-ids '(1)))
    (puthash 1 "g/p" shipit-issue-gitlab--project-cache)
    (cl-letf (((symbol-function 'shipit-issue-gitlab--ensure-project-cache)
               (lambda (_config) nil))
              ((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (when (string-match-p "/events" path)
                   (vector
                    '((action_name . "pushed to")
                      (target_type . "Project")
                      (project_id . 1)
                      (created_at . "2026-02-15T09:00:00.000Z")
                      (author . ((username . "alice"))))
                    '((action_name . "commented on")
                      (target_type . "MergeRequest")
                      (target_iid . 42)
                      (target_title . "Fix it")
                      (project_id . 1)
                      (created_at . "2026-02-15T10:00:00.000Z")
                      (author . ((username . "alice")))))))))
      (let* ((config '(:api-url "https://gitlab.com" :project-path "g/p"))
             (result (shipit-issue-gitlab--fetch-events config)))
        (should (= 1 (length result)))
        (should (equal 42 (cdr (assq 'number (car result)))))))))

(ert-deftest test-shipit-issue-gitlab-notifications-combines-todos-and-events ()
  "GIVEN todos with MR !42 and events with comment on MR !42 and MR !99
WHEN fetching notifications with events enabled
THEN combined result has 2 activities: MR !42 from Todo (priority), MR !99 from Event."
  (let ((shipit-gitlab-notifications-include-events t)
        (shipit-issue-gitlab--project-cache (make-hash-table :test 'equal))
        (shipit-issue-gitlab--project-ids '(1)))
    (puthash 1 "g/p" shipit-issue-gitlab--project-cache)
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (cond
                  ((string-match-p "/todos" path)
                   (vector
                    '((target . ((iid . 42)
                                 (title . "MR Forty-Two")
                                 (web_url . "https://gitlab.com/g/p/-/merge_requests/42")))
                      (target_type . "MergeRequest")
                      (action_name . "review_requested")
                      (project . ((path_with_namespace . "g/p")))
                      (updated_at . "2026-02-15T12:00:00.000Z"))))
                  ((string-match-p "/projects/.*/events" path)
                   (vector
                    ;; Duplicate: also about MR !42
                    '((action_name . "commented on")
                      (target_type . "MergeRequest")
                      (target_iid . 42)
                      (target_title . "MR Forty-Two")
                      (project_id . 1)
                      (created_at . "2026-02-15T11:00:00.000Z")
                      (author . ((username . "alice"))))
                    ;; Unique: MR !99
                    '((action_name . "commented on")
                      (target_type . "MergeRequest")
                      (target_iid . 99)
                      (target_title . "MR Ninety-Nine")
                      (project_id . 1)
                      (created_at . "2026-02-15T11:30:00.000Z")
                      (author . ((username . "bob")))))))))
              ((symbol-function 'shipit-issue-gitlab--ensure-project-cache)
               (lambda (_config) nil)))
      (let* ((config '(:api-url "https://gitlab.com" :project-path "g/p"))
             (result (shipit-issue-gitlab--fetch-notifications config nil)))
        ;; THEN 2 activities total
        (should (= 2 (length result)))
        ;; MR !42 should come from Todo (has review_requested reason)
        (let ((mr42 (seq-find (lambda (a) (equal 42 (cdr (assq 'number a)))) result)))
          (should mr42)
          (should (equal "review_requested" (cdr (assq 'reason mr42)))))
        ;; MR !99 should come from Event
        (let ((mr99 (seq-find (lambda (a) (equal 99 (cdr (assq 'number a)))) result)))
          (should mr99)
          (should (equal "comment" (cdr (assq 'reason mr99)))))))))

(ert-deftest test-shipit-issue-gitlab-todo-to-activity-includes-todo-id ()
  "GIVEN a GitLab todo with id 777
WHEN converting to activity
THEN the activity includes gitlab-todo-id = 777."
  (let* ((todo '((id . 777)
                 (target . ((iid . 42)
                             (title . "Test MR")
                             (web_url . "https://gitlab.com/g/p/-/merge_requests/42")))
                 (target_type . "MergeRequest")
                 (action_name . "assigned")
                 (project . ((path_with_namespace . "g/p")))
                 (updated_at . "2026-02-16T00:00:00.000Z")))
         (activity (shipit-issue-gitlab--todo-to-activity todo)))
    (should (equal 777 (cdr (assq 'gitlab-todo-id activity))))))

(ert-deftest test-shipit-issue-gitlab-mark-todo-done ()
  "GIVEN a GitLab config and todo ID
WHEN calling mark-todo-done
THEN it POSTs to /todos/:id/mark_as_done."
  (let ((captured-path nil)
        (captured-method nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path _data method)
                 (setq captured-path path
                       captured-method method)
                 nil)))
      (shipit-issue-gitlab--mark-todo-done '(:api-url "https://gitlab.com") 777)
      (should (equal "/todos/777/mark_as_done" captured-path))
      (should (equal "POST" captured-method)))))

(ert-deftest test-shipit-issue-gitlab-ensure-project-cache-repopulates-when-ids-nil ()
  "GIVEN project cache hash has entries but project-ids is nil
WHEN calling ensure-project-cache
THEN it repopulates both hash and ordered list."
  (let ((shipit-issue-gitlab--project-cache (make-hash-table :test 'equal))
        (shipit-issue-gitlab--project-ids nil))
    ;; Pre-populate hash to simulate stale state after reload
    (puthash 99 "stale/project" shipit-issue-gitlab--project-cache)
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (when (string-match-p "/projects" path)
                   (vector
                    '((id . 1) (path_with_namespace . "group/project"))
                    '((id . 2) (path_with_namespace . "team/repo")))))))
      (shipit-issue-gitlab--ensure-project-cache
       '(:api-url "https://gitlab.com"))
      ;; THEN hash is repopulated (stale entry gone)
      (should (= 2 (hash-table-count shipit-issue-gitlab--project-cache)))
      (should (equal "group/project" (gethash 1 shipit-issue-gitlab--project-cache)))
      ;; THEN project-ids is populated in order
      (should (equal '(1 2) shipit-issue-gitlab--project-ids)))))

(ert-deftest test-shipit-issue-gitlab-events-disabled ()
  "GIVEN shipit-gitlab-notifications-include-events is nil
WHEN fetching notifications
THEN only todos are returned, events are not fetched."
  (let ((shipit-gitlab-notifications-include-events nil)
        (events-fetched nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (cond
                  ((string-match-p "/todos" path)
                   (vector
                    '((target . ((iid . 10)
                                 (title . "MR Ten")
                                 (web_url . "https://gitlab.com/g/p/-/merge_requests/10")))
                      (target_type . "MergeRequest")
                      (action_name . "assigned")
                      (project . ((path_with_namespace . "g/p")))
                      (updated_at . "2026-02-15T12:00:00.000Z"))))
                  ((string-match-p "/events" path)
                   (setq events-fetched t)
                   (vector))))))
      (let* ((config '(:api-url "https://gitlab.com" :project-path "g/p"))
             (result (shipit-issue-gitlab--fetch-notifications config nil)))
        ;; THEN only 1 todo activity
        (should (= 1 (length result)))
        (should (equal 10 (cdr (assq 'number (car result)))))
        ;; THEN events endpoint was NOT called
        (should-not events-fetched)))))

;;; Notification PR Opening — Backend Awareness Tests

(ert-deftest test-shipit-issue-gitlab-open-notification-pr-uses-backend ()
  "GIVEN a notification line with backend-id=gitlab and backend-config
WHEN opening the PR notification (no prefix arg)
THEN shipit-open-pr-buffer is called with gitlab backend info."
  (let ((captured-pr nil)
        (captured-repo nil)
        (captured-backend-id nil)
        (captured-backend-config nil)
        (current-prefix-arg nil))
    (cl-letf (((symbol-function 'shipit-open-pr-buffer)
               (lambda (pr-number repo backend-id backend-config)
                 (setq captured-pr pr-number
                       captured-repo repo
                       captured-backend-id backend-id
                       captured-backend-config backend-config))))
      (with-temp-buffer
        (insert "  GL PR Daskeladden/test1  #1   Fix bug  comment\n")
        (add-text-properties
         (point-min) (point-max)
         '(shipit-repo "Daskeladden/test1"
           shipit-pr-number 1
           shipit-notification-type "pr"
           shipit-notification-source gitlab
           shipit-notification-backend-id gitlab
           shipit-notification-backend-config (:api-url "https://gitlab.com"
                                               :project-path "Daskeladden/test1")
           shipit-notification-browse-url "https://gitlab.com/Daskeladden/test1/-/merge_requests/1"))
        (goto-char (point-min))
        (shipit--open-notification-pr 1 "Daskeladden/test1")))
    ;; THEN called with GitLab backend
    (should (equal 1 captured-pr))
    (should (equal "Daskeladden/test1" captured-repo))
    (should (eq 'gitlab captured-backend-id))
    (should (equal "https://gitlab.com"
                   (plist-get captured-backend-config :api-url)))))

(ert-deftest test-shipit-issue-gitlab-open-notification-pr-browser-uses-stored-url ()
  "GIVEN a notification line with browse-url for GitLab
WHEN opening the PR notification with C-u prefix
THEN browse-url receives the stored GitLab URL, not a GitHub URL."
  (let ((captured-url nil)
        (current-prefix-arg '(4))
        (shipit--notification-pr-activities (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq captured-url url))))
      (with-temp-buffer
        (insert "  GL PR Daskeladden/test1  #1   Fix bug  comment\n")
        (add-text-properties
         (point-min) (point-max)
         '(shipit-repo "Daskeladden/test1"
           shipit-pr-number 1
           shipit-notification-type "pr"
           shipit-notification-source gitlab
           shipit-notification-browse-url "https://gitlab.com/Daskeladden/test1/-/merge_requests/1"))
        (goto-char (point-min))
        (shipit--open-notification-pr 1 "Daskeladden/test1")))
    ;; THEN browse-url received the GitLab URL
    (should (string-match-p "gitlab\\.com" captured-url))
    (should (string-match-p "merge_requests/1" captured-url))))

(provide 'test-shipit-issue-gitlab)
;;; test-shipit-issue-gitlab.el ends here
