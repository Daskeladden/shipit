;;; test-shipit-pr-backends.el --- Tests for PR backend registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for pluggable PR backend system.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-comment-backends)
(require 'shipit-http)
(require 'shipit-pr-sections)
(require 'shipit-diff)
(require 'shipit-pr-gitlab)

;;; Test helpers

(defun test-shipit-pr--make-minimal-comment-plist ()
  "Return a minimal valid comment backend plist with all required keys."
  (list :name "Test"
        :fetch-general-comments #'ignore
        :fetch-inline-comments #'ignore
        :add-general-comment #'ignore
        :add-inline-comment #'ignore
        :reply-to-comment #'ignore
        :edit-comment #'ignore
        :delete-comment #'ignore
        :toggle-reaction #'ignore
        :fetch-reactions #'ignore))

(defun test-shipit-pr--make-minimal-plist ()
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

;;; Registration Tests

(ert-deftest test-shipit-pr-register-backend-basic ()
  "GIVEN a valid backend plist with all required keys
WHEN registering the backend
THEN it is stored in shipit-pr-backends."
  (let ((shipit-pr-backends nil))
    (shipit-pr-register-backend 'test-backend (test-shipit-pr--make-minimal-plist))
    (should (assq 'test-backend shipit-pr-backends))))

(ert-deftest test-shipit-pr-register-backend-missing-key ()
  "GIVEN a backend plist missing required key :fetch-pr
WHEN registering the backend
THEN an error is signaled."
  (let ((shipit-pr-backends nil))
    (should-error
     (shipit-pr-register-backend
      'bad-backend
      (list :name "Bad"
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
            :browse-url #'ignore)))))

(ert-deftest test-shipit-pr-register-backend-replaces-existing ()
  "GIVEN a backend already registered under id 'my-be
WHEN registering a new backend with the same id
THEN the old backend is replaced."
  (let ((shipit-pr-backends nil))
    (let ((plist1 (test-shipit-pr--make-minimal-plist))
          (plist2 (test-shipit-pr--make-minimal-plist)))
      (plist-put plist1 :name "V1")
      (plist-put plist2 :name "V2")
      (shipit-pr-register-backend 'my-be plist1)
      (shipit-pr-register-backend 'my-be plist2)
      (should (string= "V2" (plist-get (cdr (assq 'my-be shipit-pr-backends)) :name)))
      (should (= 1 (length shipit-pr-backends))))))

;;; Lookup Tests

(ert-deftest test-shipit-pr-get-backend-returns-active ()
  "GIVEN 'github backend is registered and shipit-pr-backend is 'github
WHEN calling shipit-pr--get-backend
THEN the github backend plist is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "GitHub")
      (shipit-pr-register-backend 'github plist))
    (let ((backend (shipit-pr--get-backend)))
      (should backend)
      (should (string= "GitHub" (plist-get backend :name))))))

(ert-deftest test-shipit-pr-get-backend-errors-on-unknown ()
  "GIVEN no backends registered
WHEN calling shipit-pr--get-backend with backend set to 'unknown
THEN an error is signaled."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'unknown))
    (should-error (shipit-pr--get-backend))))

;;; Resolve-for-repo Tests

(ert-deftest test-shipit-pr-resolve-for-repo-returns-backend-and-config ()
  "GIVEN a github PR backend registered
WHEN resolving for a repo
THEN returns (BACKEND-PLIST . CONFIG) with :repo injected."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "GitHub")
      (shipit-pr-register-backend 'github plist))
    (let ((result (shipit-pr--resolve-for-repo "owner/repo")))
      (should (consp result))
      (should (string= "GitHub" (plist-get (car result) :name)))
      (should (string= "owner/repo" (plist-get (cdr result) :repo))))))

(ert-deftest test-shipit-pr-resolve-for-repo-includes-user-config ()
  "GIVEN shipit-pr-backend-config has :api-url set
WHEN resolving for a repo
THEN the config includes both :repo and :api-url."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config '(:api-url "https://custom.api.com")))
    (shipit-pr-register-backend 'github (test-shipit-pr--make-minimal-plist))
    (let* ((result (shipit-pr--resolve-for-repo "owner/repo"))
           (config (cdr result)))
      (should (string= "owner/repo" (plist-get config :repo)))
      (should (string= "https://custom.api.com" (plist-get config :api-url))))))

;;; Dispatch Tests

(ert-deftest test-shipit-pr-dispatch-fetch-pr ()
  "GIVEN a mock backend that returns PR data from :fetch-pr
WHEN dispatching :fetch-pr through the backend
THEN the mock function is called with config and returns data."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "Mock")
      (plist-put plist :fetch-pr
                 (lambda (config number)
                   `((number . ,number)
                     (repo . ,(plist-get config :repo)))))
      (shipit-pr-register-backend 'mock plist))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :fetch-pr) config 42)))
      (should (= 42 (cdr (assq 'number result))))
      (should (string= "owner/repo" (cdr (assq 'repo result)))))))

(ert-deftest test-shipit-pr-dispatch-browse-url ()
  "GIVEN a mock backend with :browse-url
WHEN dispatching :browse-url
THEN the URL is correctly constructed."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "Mock")
      (plist-put plist :browse-url
                 (lambda (config number)
                   (format "https://example.com/%s/pull/%s"
                           (plist-get config :repo) number)))
      (shipit-pr-register-backend 'mock plist))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (url (funcall (plist-get backend :browse-url) config 99)))
      (should (string= "https://example.com/owner/repo/pull/99" url)))))

(ert-deftest test-shipit-pr-dispatch-optional-key ()
  "GIVEN a backend registered with optional :fetch-review-threads
WHEN checking the optional key
THEN it is accessible."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "Mock")
      (plist-put plist :fetch-review-threads
                 (lambda (_config _number) '((thread-id . 1))))
      (shipit-pr-register-backend 'mock plist))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (fn (plist-get backend :fetch-review-threads)))
      (should fn)
      (should (equal '((thread-id . 1)) (funcall fn config 1))))))

(ert-deftest test-shipit-pr-dispatch-missing-optional-key ()
  "GIVEN a backend without optional :fetch-review-threads
WHEN checking the optional key
THEN nil is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (shipit-pr-register-backend 'mock (test-shipit-pr--make-minimal-plist))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved)))
      (should-not (plist-get backend :fetch-review-threads)))))

;;; Orchestrator Dispatch Tests — Labels

(defmacro test-shipit-pr--with-mock-backend (backend-ops &rest body)
  "Run BODY with a mock PR backend registered.
BACKEND-OPS is a plist of operation overrides merged onto the minimal backend."
  (declare (indent 1))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'mock)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo")
         (shipit--current-displayed-pr '(42 "owner/repo")))
     (let ((plist (test-shipit-pr--make-minimal-plist)))
       (plist-put plist :name "Mock")
       ,@(cl-loop for (key val) on backend-ops by #'cddr
                  collect `(plist-put plist ,key ,val))
       (shipit-pr-register-backend 'mock plist))
     ,@body))

(ert-deftest test-shipit-add-label-dispatches-to-backend ()
  "GIVEN a mock PR backend with :add-label
WHEN calling shipit--add-label-to-pr
THEN backend :add-label is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:add-label
         (lambda (config number label-name)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :label label-name))
           '(((name . "bug")))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t)))
        (let ((result (shipit--add-label-to-pr 42 "bug")))
          (should result)
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number)))
          (should (string= "bug" (plist-get captured-args :label))))))))

(ert-deftest test-shipit-remove-label-dispatches-to-backend ()
  "GIVEN a mock PR backend with :remove-label
WHEN calling shipit--remove-label-from-pr
THEN backend :remove-label is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:remove-label
         (lambda (config number label-name)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :label label-name))
           t))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t)))
        (let ((result (shipit--remove-label-from-pr 42 "bug")))
          (should result)
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number)))
          (should (string= "bug" (plist-get captured-args :label))))))))

;;; Orchestrator Dispatch Tests — Read Operations

(ert-deftest test-shipit-get-pull-request-dispatches-to-backend ()
  "GIVEN a mock PR backend with :fetch-pr
WHEN calling shipit-get-pull-request
THEN backend :fetch-pr is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-pr
         (lambda (config number)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number))
           '((number . 42) (title . "Test PR"))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t)))
        (let ((result (shipit-get-pull-request 42 "owner/repo")))
          (should result)
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number)))
          (should (equal "Test PR" (cdr (assq 'title result)))))))))

(ert-deftest test-shipit-get-pr-files-dispatches-to-backend ()
  "GIVEN a mock PR backend with :fetch-files
WHEN calling shipit-get-pr-files
THEN backend :fetch-files is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-files
         (lambda (config number)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number))
           '(((filename . "file.el") (status . "modified")))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t)))
        (let ((result (shipit-get-pr-files 42)))
          (should result)
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number)))
          (should (equal "file.el" (cdr (assq 'filename (car result))))))))))

(ert-deftest test-shipit-get-pr-comments-dispatches-to-backend ()
  "GIVEN a mock comment backend with :fetch-inline-comments
WHEN calling shipit-get-pr-comments
THEN backend :fetch-inline-comments is called with correct args."
  (let ((captured-args nil)
        (shipit-comment-backends nil))
    (test-shipit-pr--with-mock-backend ()
      ;; Register mock comment backend
      (let ((cplist (test-shipit-pr--make-minimal-comment-plist)))
        (plist-put cplist :name "Mock")
        (plist-put cplist :fetch-inline-comments
                   (lambda (config number)
                     (setq captured-args (list :repo (plist-get config :repo)
                                               :number number))
                     '(((id . 1) (body . "comment")))))
        (shipit-comment-register-backend 'mock cplist))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t)))
        (let ((result (shipit-get-pr-comments 42)))
          (should result)
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number))))))))

(ert-deftest test-shipit-fetch-inline-comments-dispatches-to-backend ()
  "GIVEN a mock comment backend with :fetch-inline-comments
WHEN calling shipit--fetch-inline-comments
THEN backend :fetch-inline-comments is called for the API fetch."
  (let ((captured-args nil)
        (shipit-comment-backends nil)
        (shipit--cached-inline-comments nil)
        (shipit--inline-comments-fetched nil)
        (shipit--resolved-comments-hash nil)
        (shipit--sections-with-unread nil)
        (shipit--reaction-cache (make-hash-table :test 'equal)))
    (test-shipit-pr--with-mock-backend
        (:fetch-pr
         (lambda (_config _number)
           '((head . ((sha . "abc123"))))))
      ;; Register mock comment backend
      (let ((cplist (test-shipit-pr--make-minimal-comment-plist)))
        (plist-put cplist :name "Mock")
        (plist-put cplist :fetch-inline-comments
                   (lambda (config number)
                     (setq captured-args (list :repo (plist-get config :repo)
                                               :number number))
                     '(((id . 1) (body . "inline") (path . "f.el")
                        (line . 10) (side . "RIGHT")))))
        (shipit-comment-register-backend 'mock cplist))
      (cl-letf (((symbol-function 'shipit--enhance-rest-comments-with-outdated)
                 (lambda (data _repo _sha) data))
                ((symbol-function 'shipit--check-for-new-inline-comments)
                 (lambda (_repo _pr _comments) nil))
                ((symbol-function 'shipit--fetch-reactions-parallel-sync) #'ignore))
        (shipit--fetch-inline-comments "owner/repo" 42)
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))))))

(ert-deftest test-shipit-get-pr-comments-for-file-dispatches-to-backend ()
  "GIVEN a mock comment backend with :fetch-inline-comments
WHEN calling shipit-get-pr-comments-for-file
THEN backend is used to fetch comments and results are filtered by file."
  (let ((captured-args nil)
        (shipit-comment-backends nil)
        (shipit--comment-cache (make-hash-table :test 'equal))
        (shipit--comment-type-cache (make-hash-table :test 'equal)))
    (test-shipit-pr--with-mock-backend ()
      ;; Register mock comment backend
      (let ((cplist (test-shipit-pr--make-minimal-comment-plist)))
        (plist-put cplist :name "Mock")
        (plist-put cplist :fetch-inline-comments
                   (lambda (config number)
                     (setq captured-args (list :repo (plist-get config :repo)
                                               :number number))
                     '(((id . 1) (body . "match") (path . "src/main.el"))
                       ((id . 2) (body . "other") (path . "src/other.el")))))
        (shipit-comment-register-backend 'mock cplist))
      (cl-letf (((symbol-function 'shipit--get-repo-from-remote) (lambda () "owner/repo")))
        (let ((result (shipit-get-pr-comments-for-file 42 "src/main.el")))
          (should result)
          (should (= 1 (length result)))
          (should (equal "match" (cdr (assq 'body (car result))))))))))

(ert-deftest test-shipit-fetch-pr-reactions-sync-dispatches-to-backend ()
  "GIVEN a mock PR backend with :fetch-reactions
WHEN calling shipit--fetch-pr-reactions-sync
THEN backend :fetch-reactions is called with correct args
     AND result is cached."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-reactions
         (lambda (config number)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number))
           '(((id . 1) (content . "+1") (user . ((login . "user1")))))))
      (let ((shipit--reaction-cache (make-hash-table :test 'equal)))
        (let ((result (shipit--fetch-pr-reactions-sync "owner/repo" 42)))
          (should result)
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number)))
          ;; Verify result is cached
          (should (gethash "pr-42" shipit--reaction-cache)))))))

;;; Orchestrator Dispatch Tests — PR Reactions

(ert-deftest test-shipit-add-pr-reaction-dispatches-to-backend ()
  "GIVEN a mock PR backend with :add-reaction
WHEN calling shipit--add-reaction-to-pr
THEN backend :add-reaction is called with correct args
     AND reaction cache is updated."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:add-reaction
         (lambda (config number reaction)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :reaction reaction))
           '((id . 1) (content . "+1"))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit--update-pr-reactions-display) #'ignore))
        (let ((shipit--reaction-cache (make-hash-table :test 'equal)))
          (let ((result (shipit--add-reaction-to-pr 42 "+1")))
            (should result)
            (should (string= "owner/repo" (plist-get captured-args :repo)))
            (should (= 42 (plist-get captured-args :number)))
            (should (string= "+1" (plist-get captured-args :reaction)))))))))

(ert-deftest test-shipit-remove-pr-reaction-dispatches-to-backend ()
  "GIVEN a mock PR backend with :fetch-reactions and :delete-reaction
WHEN calling shipit--remove-reaction-from-pr
THEN backend :fetch-reactions is called to find the user's reaction
     AND backend :delete-reaction is called with the reaction ID."
  (let ((captured-delete-args nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-reactions
         (lambda (_config _number)
           '(((id . 99) (content . "+1") (user . ((login . "testuser"))))))
         :delete-reaction
         (lambda (config number reaction-id)
           (setq captured-delete-args (list :number number :reaction-id reaction-id))
           t))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit--get-current-user) (lambda () "testuser"))
                ((symbol-function 'shipit--update-pr-reactions-display) #'ignore))
        (let ((shipit--reaction-cache (make-hash-table :test 'equal)))
          (let ((result (shipit--remove-reaction-from-pr 42 "+1")))
            (should result)
            (should (= 42 (plist-get captured-delete-args :number)))
            (should (= 99 (plist-get captured-delete-args :reaction-id)))))))))

;;; Orchestrator Dispatch Tests — Review Operations

(ert-deftest test-shipit-dismiss-review-dispatches-to-backend ()
  "GIVEN a mock PR backend with :dismiss-review
WHEN calling shipit-dismiss-review
THEN backend :dismiss-review is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:dismiss-review
         (lambda (config number message)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :message message))
           '((id . 1))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit-clear-etag-cache-for-endpoint) #'ignore)
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (let ((shipit--cached-approval-status (make-hash-table :test 'equal)))
          (shipit-dismiss-review 42 "No longer needed" "owner/repo")
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number)))
          (should (string= "No longer needed" (plist-get captured-args :message))))))))

(ert-deftest test-shipit-post-review-dispatches-to-backend ()
  "GIVEN a mock PR backend with :submit-review and :fetch-pr
WHEN calling shipit-post-review
THEN backend :submit-review is called with correct args
     AND caches are cleared."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:submit-review
         (lambda (config number event body comments)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :event event
                                     :body body :comments comments))
           '((id . 1)))
         :fetch-pr
         (lambda (_config _number)
           '((number . 42) (user . ((login . "other-user"))))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit--get-current-user) (lambda () "testuser"))
                ((symbol-function 'shipit-clear-etag-cache-for-endpoint) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (let ((shipit--cached-approval-status (make-hash-table :test 'equal)))
          (shipit-post-review 42 "APPROVE" "Looks good" nil "owner/repo")
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          (should (= 42 (plist-get captured-args :number)))
          (should (string= "APPROVE" (plist-get captured-args :event)))
          (should (string= "Looks good" (plist-get captured-args :body))))))))

;;; Orchestrator Dispatch Tests — Reviewer Operations

(ert-deftest test-shipit-add-reviewer-dispatches-to-backend ()
  "GIVEN a mock PR backend with :add-reviewer
WHEN calling shipit--add-reviewer-to-pr
THEN backend :add-reviewer is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:add-reviewer
         (lambda (config number username)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :username username))
           '((requested_reviewers . [((login . "reviewer1"))]))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (shipit--add-reviewer-to-pr 42 "owner/repo" "reviewer1")
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (string= "reviewer1" (plist-get captured-args :username)))))))

(ert-deftest test-shipit-remove-reviewer-dispatches-to-backend ()
  "GIVEN a mock PR backend with :remove-reviewer
WHEN calling shipit--remove-reviewer-from-pr
THEN backend :remove-reviewer is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:remove-reviewer
         (lambda (config number username)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :username username))
           '((requested_reviewers . []))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (shipit--remove-reviewer-from-pr 42 "owner/repo" "reviewer1")
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (string= "reviewer1" (plist-get captured-args :username)))))))

(ert-deftest test-shipit-get-requested-reviewers-dispatches-to-backend ()
  "GIVEN a mock PR backend with :fetch-requested-reviewers
WHEN calling shipit--get-requested-reviewers
THEN backend :fetch-requested-reviewers is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-requested-reviewers
         (lambda (config number)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number))
           '((users . [((login . "reviewer1"))]))))
      (let ((result (shipit--get-requested-reviewers "owner/repo" 42)))
        (should result)
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))))))

;;; Orchestrator Dispatch Tests — Assignee Operations

(ert-deftest test-shipit-add-assignee-dispatches-to-backend ()
  "GIVEN a mock PR backend with :add-assignee
WHEN calling shipit--add-assignee
THEN backend :add-assignee is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:add-assignee
         (lambda (config number username)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :username username))
           '((assignees . [((login . "user1"))]))))
      (shipit--add-assignee 42 "user1" "owner/repo")
      (should (string= "owner/repo" (plist-get captured-args :repo)))
      (should (= 42 (plist-get captured-args :number)))
      (should (string= "user1" (plist-get captured-args :username))))))

(ert-deftest test-shipit-remove-assignee-dispatches-to-backend ()
  "GIVEN a mock PR backend with :remove-assignee
WHEN calling shipit--remove-assignee
THEN backend :remove-assignee is called with correct args."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:remove-assignee
         (lambda (config number username)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :username username))
           '((assignees . []))))
      (shipit--remove-assignee 42 "user1" "owner/repo")
      (should (string= "owner/repo" (plist-get captured-args :repo)))
      (should (= 42 (plist-get captured-args :number)))
      (should (string= "user1" (plist-get captured-args :username))))))

;;; Orchestrator Dispatch Tests — PR Close/Update

(ert-deftest test-shipit-close-pr-dispatches-to-backend ()
  "GIVEN a mock PR backend with :update-pr
WHEN calling shipit--close-pr
THEN backend :update-pr is called with state=closed."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:update-pr
         (lambda (config number data)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :data data))
           '((state . "closed"))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-clear-etag-cache-for-repo) #'ignore)
                ((symbol-function 'shipit-clear-stale-cache-files) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (shipit--close-pr 42 "owner/repo")
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (equal '((state . "closed")) (plist-get captured-args :data)))))))

(ert-deftest test-shipit-update-pr-description-dispatches-to-backend ()
  "GIVEN a mock PR backend with :update-pr
WHEN calling shipit--update-pr-description
THEN backend :update-pr is called with body data."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:update-pr
         (lambda (config number data)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :data data))
           '((body . "new description"))))
      (shipit--update-pr-description 42 "owner/repo" "new description")
      (should (string= "owner/repo" (plist-get captured-args :repo)))
      (should (= 42 (plist-get captured-args :number)))
      (should (equal "new description" (cdr (assq 'body (plist-get captured-args :data))))))))

;;; Orchestrator Dispatch Tests — Commit Operations

(ert-deftest test-shipit-fetch-commit-files-dispatches-to-backend ()
  "GIVEN a mock PR backend with :fetch-commit
WHEN calling shipit--fetch-commit-files
THEN backend :fetch-commit is called with correct args
     AND files are extracted from result."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-commit
         (lambda (config sha)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :sha sha))
           `((sha . ,sha)
             (files . (((filename . "file.el") (status . "modified")))))))
      (let ((result (shipit--fetch-commit-files "owner/repo" "abc123")))
        (should result)
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (string= "abc123" (plist-get captured-args :sha)))
        (should (equal "file.el" (cdr (assq 'filename (car result)))))))))

;;; Orchestrator Dispatch Tests — Check Suites

(ert-deftest test-shipit-pr-dispatch-fetch-check-suites ()
  "GIVEN a mock backend with :fetch-check-suites
WHEN dispatching :fetch-check-suites
THEN data is returned for the given ref and page."
  (test-shipit-pr--with-mock-backend
      (:fetch-check-suites
       (lambda (config ref page per-page)
         `((total_count . 1)
           (check_suites . [((id . 100)
                             (app . ((slug . "github-actions"))))]))))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :fetch-check-suites) config "abc123" 1 30)))
      (should result)
      (should (= 1 (cdr (assq 'total_count result)))))))

(ert-deftest test-shipit-pr-dispatch-fetch-suite-check-runs ()
  "GIVEN a mock backend with :fetch-suite-check-runs
WHEN dispatching :fetch-suite-check-runs
THEN check runs are returned for the suite."
  (test-shipit-pr--with-mock-backend
      (:fetch-suite-check-runs
       (lambda (config suite-id)
         `((total_count . 2)
           (check_runs . [((id . 1) (name . "test"))
                          ((id . 2) (name . "lint"))]))))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :fetch-suite-check-runs) config 100)))
      (should result)
      (should (= 2 (cdr (assq 'total_count result)))))))

(ert-deftest test-shipit-pr-dispatch-fetch-workflow-info ()
  "GIVEN a mock backend with :fetch-workflow-info
WHEN dispatching :fetch-workflow-info
THEN workflow name is returned."
  (test-shipit-pr--with-mock-backend
      (:fetch-workflow-info
       (lambda (config workflow-id)
         `((id . ,workflow-id) (name . "CI Pipeline"))))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :fetch-workflow-info) config 456)))
      (should result)
      (should (string= "CI Pipeline" (cdr (assq 'name result)))))))

(ert-deftest test-shipit-pr-dispatch-fetch-action-run-info ()
  "GIVEN a mock backend with :fetch-action-run-info
WHEN dispatching :fetch-action-run-info
THEN run data with name is returned."
  (test-shipit-pr--with-mock-backend
      (:fetch-action-run-info
       (lambda (config run-id)
         `((id . ,run-id) (name . "Build & Test") (path . ".github/workflows/ci.yml"))))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :fetch-action-run-info) config "789")))
      (should result)
      (should (string= "Build & Test" (cdr (assq 'name result)))))))

(ert-deftest test-shipit-pr-dispatch-fetch-commit-check-runs ()
  "GIVEN a mock backend with :fetch-commit-check-runs
WHEN dispatching :fetch-commit-check-runs
THEN check runs for the commit are returned."
  (test-shipit-pr--with-mock-backend
      (:fetch-commit-check-runs
       (lambda (config ref page per-page)
         `((total_count . 1)
           (check_runs . [((id . 1) (name . "test") (status . "completed"))]))))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :fetch-commit-check-runs) config "abc123" 1 100)))
      (should result)
      (should (= 1 (cdr (assq 'total_count result)))))))

;;; Orchestrator Dispatch Tests — Compare & Search

(ert-deftest test-shipit-pr-dispatch-fetch-compare ()
  "GIVEN a mock backend with :fetch-compare
WHEN dispatching :fetch-compare
THEN compare data with merge_base_commit is returned."
  (test-shipit-pr--with-mock-backend
      (:fetch-compare
       (lambda (config base head)
         `((merge_base_commit . ((sha . "base123")))
           (ahead_by . 3))))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :fetch-compare) config "main" "feature")))
      (should result)
      (should (string= "base123" (cdr (assq 'sha (cdr (assq 'merge_base_commit result)))))))))

(ert-deftest test-shipit-pr-dispatch-search-raw ()
  "GIVEN a mock backend with :search-raw
WHEN dispatching :search-raw
THEN search results are returned."
  (test-shipit-pr--with-mock-backend
      (:search-raw
       (lambda (config query page per-page)
         `((total_count . 1)
           (items . [((number . 42) (title . "Test PR"))]))))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :search-raw) config "is:pr" 1 30)))
      (should result)
      (should (= 1 (cdr (assq 'total_count result)))))))

;;; Orchestrator Dispatch Tests — Batch Operations

(ert-deftest test-shipit-pr-dispatch-set-labels ()
  "GIVEN a mock backend with :set-labels
WHEN dispatching :set-labels
THEN labels are set on the PR."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:set-labels
         (lambda (config number label-names)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :labels label-names))
           '(((name . "bug")) ((name . "enhancement")))))
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (result (funcall (plist-get backend :set-labels) config 42 '("bug" "enhancement"))))
        (should result)
        (should (= 42 (plist-get captured-args :number)))
        (should (equal '("bug" "enhancement") (plist-get captured-args :labels)))))))

(ert-deftest test-shipit-pr-dispatch-add-reviewers-batch ()
  "GIVEN a mock backend with :add-reviewers-batch
WHEN dispatching :add-reviewers-batch
THEN reviewers are added in batch."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:add-reviewers-batch
         (lambda (config number usernames)
           (setq captured-args (list :number number :usernames usernames))
           '((requested_reviewers . [((login . "user1")) ((login . "user2"))]))))
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (result (funcall (plist-get backend :add-reviewers-batch) config 42 '("user1" "user2"))))
        (should result)
        (should (= 42 (plist-get captured-args :number)))
        (should (equal '("user1" "user2") (plist-get captured-args :usernames)))))))

(ert-deftest test-shipit-pr-dispatch-add-assignees-batch ()
  "GIVEN a mock backend with :add-assignees-batch
WHEN dispatching :add-assignees-batch
THEN assignees are added in batch."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:add-assignees-batch
         (lambda (config number usernames)
           (setq captured-args (list :number number :usernames usernames))
           '((assignees . [((login . "user1"))]))))
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (result (funcall (plist-get backend :add-assignees-batch) config 42 '("user1"))))
        (should result)
        (should (= 42 (plist-get captured-args :number)))
        (should (equal '("user1") (plist-get captured-args :usernames)))))))

;;; Orchestrator Dispatch Tests — Execute Search with Repo Override

(ert-deftest test-shipit-execute-search-with-repo-override ()
  "GIVEN transient args including --repo=other/repo
WHEN calling shipit--execute-advanced-search
THEN the search uses the overridden repo instead of auto-detected one."
  (require 'shipit-commands)
  (let ((captured-repo nil)
        (captured-backend-id nil)
        (captured-args nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "auto-detected/repo"))
              ((symbol-function 'shipit--execute-backend-search)
               (lambda (backend-id repo args)
                 (setq captured-backend-id backend-id)
                 (setq captured-repo repo)
                 (setq captured-args args)))
              ((symbol-function 'transient-args)
               (lambda (_) '("--state=open" "--repo=other/repo"))))
      (shipit--execute-advanced-search '("--state=open" "--repo=other/repo"))
      ;; THEN the overridden repo is used
      (should (string= "other/repo" captured-repo))
      ;; THEN --repo= is NOT passed through to backend search
      (should-not (cl-some (lambda (a) (string-match-p "--repo=" a))
                           captured-args)))))

(ert-deftest test-shipit-execute-search-without-repo-override ()
  "GIVEN transient args without --repo=
WHEN calling shipit--execute-advanced-search
THEN the search uses the auto-detected repo."
  (require 'shipit-commands)
  (let ((captured-repo nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "auto-detected/repo"))
              ((symbol-function 'shipit--execute-backend-search)
               (lambda (_backend-id repo _args)
                 (setq captured-repo repo)))
              ((symbol-function 'transient-args)
               (lambda (_) '("--state=open"))))
      (shipit--execute-advanced-search '("--state=open"))
      ;; THEN the auto-detected repo is used
      (should (string= "auto-detected/repo" captured-repo)))))

;;; Backend-Aware Buffer Tests

(ert-deftest test-shipit-open-pr-buffer-with-backend-override ()
  "GIVEN a gitlab backend registered
WHEN calling shipit-open-pr-buffer with backend-id=gitlab
THEN the buffer has buffer-local shipit-pr-backend set to gitlab."
  (require 'shipit-buffer)
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (buf nil))
    ;; Register both backends
    (shipit-pr-register-backend 'github (test-shipit-pr--make-minimal-plist))
    (let ((gl-plist (test-shipit-pr--make-minimal-plist)))
      (plist-put gl-plist :name "GitLab")
      (shipit-pr-register-backend 'gitlab gl-plist))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit--get-repo-from-remote) (lambda () nil))
                  ((symbol-function 'shipit--get-repo-root) (lambda () "/tmp"))
                  ((symbol-function 'shipit-buffer-refresh) #'ignore)
                  ((symbol-function 'shipit-buffer-display) #'ignore))
          (setq buf (shipit-open-pr-buffer
                     734 "genomedx/labautomation/steps"
                     'gitlab '(:project-path "genomedx/labautomation/steps")))
          (with-current-buffer buf
            ;; THEN buffer-local backend is gitlab
            (should (eq shipit-pr-backend 'gitlab))
            ;; THEN buffer-local config has :project-path
            (should (string= "genomedx/labautomation/steps"
                             (plist-get shipit-pr-backend-config :project-path)))))
      (when (and buf (buffer-live-p buf))
        (kill-buffer buf)))))

(ert-deftest test-shipit-open-pr-buffer-without-backend-uses-global ()
  "GIVEN shipit-pr-backend is github (global default)
WHEN calling shipit-open-pr-buffer without backend-id
THEN the buffer uses the global backend."
  (require 'shipit-buffer)
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (buf nil))
    (shipit-pr-register-backend 'github (test-shipit-pr--make-minimal-plist))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit--get-repo-from-remote) (lambda () "owner/repo"))
                  ((symbol-function 'shipit--get-repo-root) (lambda () "/tmp"))
                  ((symbol-function 'shipit-buffer-refresh) #'ignore)
                  ((symbol-function 'shipit-buffer-display) #'ignore))
          (setq buf (shipit-open-pr-buffer 42 "owner/repo"))
          (with-current-buffer buf
            ;; THEN buffer-local backend is NOT set (uses global github)
            (should (eq shipit-pr-backend 'github))))
      (when (and buf (buffer-live-p buf))
        (kill-buffer buf)))))

(ert-deftest test-shipit-resolve-for-repo-uses-buffer-local-backend ()
  "GIVEN a buffer with buffer-local shipit-pr-backend=gitlab
WHEN calling shipit-pr--resolve-for-repo inside that buffer
THEN the gitlab backend is used, not the global github."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (captured-backend-name nil))
    ;; Register both backends
    (let ((gh-plist (test-shipit-pr--make-minimal-plist)))
      (plist-put gh-plist :name "GitHub")
      (shipit-pr-register-backend 'github gh-plist))
    (let ((gl-plist (test-shipit-pr--make-minimal-plist)))
      (plist-put gl-plist :name "GitLab")
      (shipit-pr-register-backend 'gitlab gl-plist))
    ;; Create a temp buffer simulating a shipit buffer with gitlab override
    (with-temp-buffer
      (setq-local shipit-pr-backend 'gitlab)
      (setq-local shipit-pr-backend-config '(:project-path "group/project"))
      (let* ((resolved (shipit-pr--resolve-for-repo "group/project"))
             (backend (car resolved))
             (config (cdr resolved)))
        ;; THEN gitlab backend is resolved
        (should (string= "GitLab" (plist-get backend :name)))
        ;; THEN config includes :project-path from buffer-local config
        (should (string= "group/project" (plist-get config :project-path)))
        ;; THEN config also has :repo injected
        (should (string= "group/project" (plist-get config :repo)))))))

(ert-deftest test-shipit-execute-backend-search-passes-backend-to-display ()
  "GIVEN a gitlab backend search returns results
WHEN calling shipit--execute-backend-search
THEN shipit--select-pr-from-results receives the backend-id and config."
  (require 'shipit-commands)
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (captured-backend-id nil)
        (captured-backend-config nil))
    (let ((gl-plist (test-shipit-pr--make-minimal-plist)))
      (plist-put gl-plist :name "GitLab")
      (plist-put gl-plist :search
                 (lambda (_config _args)
                   '(((number . 1) (title . "Test MR")))))
      (shipit-pr-register-backend 'gitlab gl-plist))
    (cl-letf (((symbol-function 'shipit--select-pr-from-results)
               (lambda (results repo &optional backend-id backend-config)
                 (setq captured-backend-id backend-id)
                 (setq captured-backend-config backend-config))))
      (shipit--execute-backend-search 'gitlab "group/project" '("--state=open"))
      ;; THEN backend-id is passed through
      (should (eq 'gitlab captured-backend-id))
      ;; THEN backend-config is passed through
      (should captured-backend-config))))

;;; GitLab :search-raw Tests

(ert-deftest test-shipit-pr-gitlab--strip-github-query-syntax ()
  "GIVEN a GitHub-flavored search query with repo:, is:pr, in:title prefixes
WHEN stripping GitHub-specific syntax
THEN only the plain search term remains."
  (should (string= "bugfix"
                    (shipit-pr-gitlab--strip-github-query-syntax
                     "repo%3Aowner%2Frepo+is%3Apr+in%3Atitle+bugfix")))
  (should (string= "my search term"
                    (shipit-pr-gitlab--strip-github-query-syntax
                     "repo%3Afoo%2Fbar+is%3Apr+in%3Atitle+my+search+term")))
  ;; Plain query without GitHub syntax passes through
  (should (string= "plain query"
                    (shipit-pr-gitlab--strip-github-query-syntax
                     "plain+query"))))

(ert-deftest test-shipit-pr-gitlab--search-raw-returns-normalized-items ()
  "GIVEN a GitLab API that returns MR data
WHEN calling search-raw
THEN result has total_count and items with normalized PR shape."
  (let ((api-response
         (vector
          `((iid . 42)
            (title . "Fix widget")
            (description . "Fixes the widget")
            (state . "opened")
            (merged_at)
            (draft . :json-false)
            (web_url . "https://gitlab.com/g/p/-/merge_requests/42")
            (author . ((username . "alice")
                       (avatar_url . "https://gitlab.com/alice.png")))
            (source_branch . "fix-widget")
            (target_branch . "main")
            (sha . "abc123")
            (labels . ,(vector "bug"))
            (assignees . ,(vector))
            (user_notes_count . 3)
            (created_at . "2026-01-01T00:00:00Z")
            (updated_at . "2026-01-02T00:00:00Z")))))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path) api-response))
              ((symbol-function 'shipit-gitlab--project-path)
               (lambda (_config) "123")))
      (let* ((config '(:project-id 123 :url "https://gitlab.com"))
             (result (shipit-pr-gitlab--search-raw
                      config "repo%3Aowner%2Frepo+is%3Apr+in%3Atitle+widget" 1 50))
             (items (cdr (assq 'items result)))
             (first-item (car items)))
        ;; THEN total_count is present
        (should (assq 'total_count result))
        ;; THEN items are normalized
        (should (= 1 (length items)))
        (should (= 42 (cdr (assq 'number first-item))))
        (should (string= "Fix widget" (cdr (assq 'title first-item))))
        (should (string= "open" (cdr (assq 'state first-item))))))))

(ert-deftest test-shipit-pr-gitlab--search-raw-empty-results ()
  "GIVEN a GitLab API that returns no MRs
WHEN calling search-raw
THEN result has total_count 999 and empty items list."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request)
             (lambda (_config _path) (vector)))
            ((symbol-function 'shipit-gitlab--project-path)
             (lambda (_config) "123")))
    (let* ((config '(:project-id 123 :url "https://gitlab.com"))
           (result (shipit-pr-gitlab--search-raw config "test" 1 50))
           (items (cdr (assq 'items result))))
      (should (= 999 (cdr (assq 'total_count result))))
      (should (= 0 (length items))))))

;;; Backend property dispatch tests

(ert-deftest test-shipit-pr-type-label-dispatches-through-backend ()
  "GIVEN a PR backend with :pr-type-label set to a custom value
WHEN calling shipit--pr-type-label
THEN the backend's label is returned (not hardcoded)."
  (require 'shipit-buffer)
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "GitHub")
      (plist-put plist :pr-type-label "Change Request")
      (shipit-pr-register-backend 'github plist))
    (should (string= "Change Request" (shipit--pr-type-label)))))

(ert-deftest test-shipit-pr-type-label-defaults-to-pull-request ()
  "GIVEN a PR backend without :pr-type-label property
WHEN calling shipit--pr-type-label
THEN \"Pull Request\" is returned as default."
  (require 'shipit-buffer)
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github))
    (shipit-pr-register-backend 'github (test-shipit-pr--make-minimal-plist))
    (should (string= "Pull Request" (shipit--pr-type-label)))))

(ert-deftest test-shipit-cache-clear-dispatches-through-comment-backend ()
  "GIVEN a comment backend with :clear-discussions-cache
WHEN clearing general comments cache
THEN the backend's cache clear function is called."
  (require 'shipit-cache)
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'gitlab)
        (clear-called nil)
        (shipit--cached-general-comments '(some data))
        (shipit--general-comments-fetched t))
    (shipit-comment-register-backend
     'gitlab
     (let ((plist (test-shipit-pr--make-minimal-comment-plist)))
       (plist-put plist :name "GitLab")
       (plist-put plist :clear-discussions-cache
                  (lambda () (setq clear-called t)))
       plist))
    (shipit--clear-general-comments-cache)
    (should clear-called)
    (should-not shipit--cached-general-comments)))

;;; Dispatch Tests — URL Builders

(ert-deftest test-shipit-browse-issue-url-dispatches-through-backend ()
  "GIVEN a mock backend with :browse-issue-url
WHEN calling the dispatch function
THEN backend :browse-issue-url is called with config and number."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:browse-issue-url
         (lambda (config number)
           (setq captured-args (list (plist-get config :repo) number))
           "https://example.com/issues/42"))
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (fn (plist-get backend :browse-issue-url)))
        (should fn)
        (should (string= "https://example.com/issues/42" (funcall fn config 42)))
        (should (equal '("owner/repo" 42) captured-args))))))

(ert-deftest test-shipit-browse-commit-url-dispatches-through-backend ()
  "GIVEN a mock backend with :browse-commit-url
WHEN calling the dispatch function
THEN backend :browse-commit-url is called with config and sha."
  (test-shipit-pr--with-mock-backend
      (:browse-commit-url
       (lambda (config sha)
         (format "https://example.com/%s/commit/%s" (plist-get config :repo) sha)))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (url (funcall (plist-get backend :browse-commit-url) config "abc123")))
      (should (string= "https://example.com/owner/repo/commit/abc123" url)))))

(ert-deftest test-shipit-browse-user-url-dispatches-through-backend ()
  "GIVEN a mock backend with :browse-user-url
WHEN calling the dispatch function
THEN backend :browse-user-url is called with config and username."
  (test-shipit-pr--with-mock-backend
      (:browse-user-url
       (lambda (_config username)
         (format "https://example.com/%s" username)))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (url (funcall (plist-get backend :browse-user-url) config "testuser")))
      (should (string= "https://example.com/testuser" url)))))

;;; Dispatch Tests — User/Avatar

(ert-deftest test-shipit-extract-username-from-email-dispatches-through-backend ()
  "GIVEN a mock backend with :extract-username-from-email
WHEN calling shipit--extract-username-from-email
THEN backend function is called and returns username."
  (test-shipit-pr--with-mock-backend
      (:extract-username-from-email
       (lambda (_config email)
         (when (string-match "^\\([^@]+\\)@" email)
           (match-string 1 email))))
    (should (string= "testuser" (shipit--extract-username-from-email "testuser@example.com")))))

(ert-deftest test-shipit-extract-username-from-email-returns-nil-without-backend-fn ()
  "GIVEN a backend without :extract-username-from-email
WHEN calling shipit--extract-username-from-email
THEN nil is returned."
  (test-shipit-pr--with-mock-backend ()
    (should-not (shipit--extract-username-from-email "test@example.com"))))

(ert-deftest test-shipit-generate-avatar-url-dispatches-through-backend ()
  "GIVEN a mock backend with :generate-avatar-url
WHEN calling shipit--generate-avatar-url
THEN backend function is called and returns URL."
  (test-shipit-pr--with-mock-backend
      (:generate-avatar-url
       (lambda (_config username)
         (format "https://example.com/%s.png" username)))
    (should (string= "https://example.com/testuser.png"
                      (shipit--generate-avatar-url "testuser")))))

(ert-deftest test-shipit-generate-avatar-url-returns-nil-without-backend-fn ()
  "GIVEN a backend without :generate-avatar-url
WHEN calling shipit--generate-avatar-url
THEN nil is returned."
  (test-shipit-pr--with-mock-backend ()
    (should-not (shipit--generate-avatar-url "testuser"))))

(ert-deftest test-shipit-generate-avatar-url-returns-nil-for-empty-username ()
  "GIVEN a backend with :generate-avatar-url
WHEN calling shipit--generate-avatar-url with empty string
THEN nil is returned (early guard)."
  (test-shipit-pr--with-mock-backend
      (:generate-avatar-url
       (lambda (_config username) (format "https://x.com/%s.png" username)))
    (should-not (shipit--generate-avatar-url ""))
    (should-not (shipit--generate-avatar-url nil))))

;;; Dispatch Tests — parse-repo-arg

(ert-deftest test-shipit-parse-repo-arg-uses-active-backend ()
  "GIVEN shipit-pr-backend is 'mock
WHEN parsing plain 'owner/repo' without backend prefix
THEN the active backend id is used as default."
  (test-shipit-pr--with-mock-backend ()
    (let ((result (shipit--parse-repo-arg "owner/repo")))
      (should (eq 'mock (car result)))
      (should (string= "owner/repo" (cdr result))))))

(ert-deftest test-shipit-parse-repo-arg-explicit-prefix-overrides ()
  "GIVEN shipit-pr-backend is 'mock
WHEN parsing 'gitlab:group/project' with explicit backend prefix
THEN the prefix backend is used, not the active one."
  (test-shipit-pr--with-mock-backend ()
    (let ((result (shipit--parse-repo-arg "gitlab:group/project")))
      (should (eq 'gitlab (car result)))
      (should (string= "group/project" (cdr result))))))

;;; Dispatch Tests — Notification mark-as-read

(ert-deftest test-shipit-mark-notification-read-dispatches-to-issue-backend ()
  "GIVEN a backend notification (has backend-id, no `notification' key) with :mark-notification-read
WHEN calling shipit--mark-notification-read
THEN the backend's mark-notification-read is called via the dispatcher."
  (let ((mark-called nil)
        (shipit-issue-backends nil)
        (shipit-issue-backend 'mock-backend)
        (shipit-issue-backend-config nil)
        (shipit--notification-pr-activities (make-hash-table :test 'equal))
        (shipit--locally-marked-read-notifications (make-hash-table :test 'equal))
        (shipit--last-notification-count 1)
        (shipit--mention-prs nil)
        (shipit--mention-count 0))
    ;; Register a mock issue backend
    (push (cons 'mock-backend
                (list :name "Mock"
                      :fetch-issue #'ignore
                      :fetch-comments #'ignore
                      :fetch-comments-async #'ignore
                      :search #'ignore
                      :create-issue #'ignore
                      :reference-patterns (lambda (_c) nil)
                      :browse-url #'ignore
                      :id-to-string #'ignore
                      :string-to-id #'ignore
                      :mark-notification-read
                      (lambda (_config activity)
                        (setq mark-called activity))))
          shipit-issue-backends)
    ;; Store a backend notification activity (no `notification' key — uses backend-id dispatch)
    (let ((activity `((reason . "review_requested")
                      (backend-id . mock-backend))))
      (puthash "owner/repo:pr:99" activity shipit--notification-pr-activities))
    ;; Mark as read
    (cl-letf (((symbol-function 'shipit--update-modeline-indicator) #'ignore)
              ((symbol-function 'shipit--clear-notifications-cache) #'ignore))
      (shipit--mark-notification-read 99 "owner/repo" t "pr"))
    ;; The backend dispatch should have been called with the activity
    (should mark-called)
    (should (equal "review_requested" (cdr (assq 'reason mark-called))))))

;;; Dispatch Tests — Labels via :set-labels in shipit--update-pr-labels

(ert-deftest test-shipit-update-pr-labels-dispatches-through-set-labels ()
  "GIVEN a mock PR backend with :set-labels
WHEN calling shipit--update-pr-labels
THEN backend :set-labels is called with correct config, number, and labels."
  (let ((captured-args nil))
    (test-shipit-pr--with-mock-backend
        (:set-labels
         (lambda (config number label-names)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :labels label-names))
           '(((name . "bug")) ((name . "enhancement")))))
      (cl-letf (((symbol-function 'shipit-gh-etag-invalidate-endpoint) #'ignore)
                ((symbol-function 'shipit-pr--backend-id) (lambda () 'mock))
                ((symbol-function 'magit-get-current-branch) (lambda () "feature"))
                ((symbol-function 'shipit--refresh-labels-section-targeted) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat func &rest args) (apply func args))))
        (let ((shipit--cached-branch-prs (make-hash-table :test 'equal)))
          (shipit--update-pr-labels 42 "owner/repo" '("bug" "enhancement"))
          ;; THEN :set-labels was called
          (should captured-args)
          ;; THEN repo is correct
          (should (string= "owner/repo" (plist-get captured-args :repo)))
          ;; THEN PR number is correct
          (should (= 42 (plist-get captured-args :number)))
          ;; THEN labels list is correct
          (should (equal '("bug" "enhancement") (plist-get captured-args :labels))))))))

;;; Dispatch Tests — Gate shipit--expand-code-urls behind backend check

(ert-deftest test-shipit-expand-code-urls-returns-unchanged-for-non-github ()
  "GIVEN the active backend is not github
WHEN calling shipit--expand-code-urls with text containing a GitHub code URL
THEN the text is returned unchanged."
  (cl-letf (((symbol-function 'shipit-pr--backend-id) (lambda () 'gitlab)))
    (let ((text "See https://github.com/owner/repo/blob/main/file.el#L10-L20 for details"))
      ;; THEN text passes through unmodified
      (should (string= text (shipit--expand-code-urls text))))))

(ert-deftest test-shipit-expand-code-urls-expands-for-github ()
  "GIVEN the active backend is github
WHEN calling shipit--expand-code-urls with text containing a GitHub code URL
THEN the URL is expanded (not returned as-is)."
  (cl-letf (((symbol-function 'shipit-pr--backend-id) (lambda () 'github))
            ((symbol-function 'shipit--fetch-code-snippet-with-navigation)
             (lambda (_owner _repo _sha _file _start _end)
               "[code snippet]")))
    (let ((text "See https://github.com/owner/repo/blob/main/file.el#L10-L20 for details"))
      ;; THEN the URL is replaced with the snippet
      (should-not (string= text (shipit--expand-code-urls text)))
      (should (string-match-p "\\[code snippet\\]" (shipit--expand-code-urls text))))))

;;; Dispatch Tests — Route shipit--get-user-teams through backend

(ert-deftest test-shipit-get-user-teams-dispatches-through-backend ()
  "GIVEN a mock PR backend with :fetch-user-teams
WHEN calling shipit--get-user-teams
THEN the backend's :fetch-user-teams is called and results are formatted."
  (require 'shipit-commands)
  (let ((captured-config nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-user-teams
         (lambda (config)
           (setq captured-config config)
           '(((slug . "core-team")
              (organization . ((login . "myorg"))))
             ((slug . "backend")
              (organization . ((login . "myorg")))))))
      (cl-letf (((symbol-function 'shipit--get-repo-from-remote) (lambda () "owner/repo")))
        (let ((shipit--cached-user-teams nil))
          (let ((result (shipit--get-user-teams)))
            ;; THEN backend was called with config
            (should captured-config)
            (should (string= "owner/repo" (plist-get captured-config :repo)))
            ;; THEN results are formatted as org/slug
            (should (equal '("myorg/core-team" "myorg/backend") result))))))))

(ert-deftest test-shipit-get-user-teams-returns-nil-without-fetch-fn ()
  "GIVEN a mock PR backend without :fetch-user-teams
WHEN calling shipit--get-user-teams
THEN nil is returned."
  (require 'shipit-commands)
  (test-shipit-pr--with-mock-backend ()
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote) (lambda () "owner/repo")))
      (let ((shipit--cached-user-teams nil))
        ;; THEN nil is returned
        (should-not (shipit--get-user-teams))))))

;;; Orchestrator Dispatch Tests — Checks module (Phase 2)

(ert-deftest test-shipit-checks-async-dispatches-through-fetch-check-suites-async ()
  "GIVEN a mock PR backend with :fetch-check-suites-async
WHEN calling shipit--fetch-checks-async-with-pr-data
THEN :fetch-check-suites-async is dispatched with correct args."
  (require 'shipit-checks)
  (let ((captured-ref nil)
        (captured-page nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-check-suites-async
         (lambda (config ref page per-page callback)
           (setq captured-ref ref)
           (setq captured-page page)
           ;; Return empty suites to end the chain
           (funcall callback '((total_count . 0) (check_suites)))))
      (let ((pr-data '((head . ((sha . "abc123") (ref . "feature-branch"))))))
        (shipit--fetch-checks-async-with-pr-data
         "owner/repo" pr-data
         (lambda (result) nil))
        ;; THEN backend received the correct ref
        (should (string= "feature-branch" captured-ref))
        ;; THEN first page was requested
        (should (= 1 captured-page))))))

(ert-deftest test-shipit-checks-async-falls-back-to-fetch-checks-without-suites ()
  "GIVEN a mock PR backend WITHOUT :fetch-check-suites-async
WHEN calling shipit--fetch-checks-async-with-pr-data
THEN :fetch-checks is dispatched as fallback."
  (require 'shipit-checks)
  (let ((fetch-checks-called nil)
        (callback-result nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-checks
         (lambda (config ref)
           (setq fetch-checks-called t)
           '(((name . "test") (status . "completed") (conclusion . "success") (html_url . "http://ci")))))
      (let ((pr-data '((head . ((sha . "abc123") (ref . "feature-branch"))))))
        (shipit--fetch-checks-async-with-pr-data
         "owner/repo" pr-data
         (lambda (result) (setq callback-result result)))
        ;; THEN :fetch-checks was called
        (should fetch-checks-called)
        ;; THEN callback received results
        (should callback-result)))))

(ert-deftest test-shipit-checks-runs-dispatch-through-fetch-suite-check-runs-async ()
  "GIVEN a mock backend with :fetch-suite-check-runs-async
WHEN shipit--fetch-check-runs-with-names-async is called
THEN :fetch-suite-check-runs-async is dispatched for each suite."
  (require 'shipit-checks)
  (let ((captured-suite-ids nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-suite-check-runs-async
         (lambda (config suite-id callback)
           (push suite-id captured-suite-ids)
           (funcall callback '((check_runs . (((name . "test")
                                               (status . "completed")
                                               (conclusion . "success")
                                               (html_url . "http://ci"))))))))
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (suite-info-list (list (list :suite-id 100 :app-name "GitHub Actions"
                                         :workflow-id nil :workflow-run-name nil)
                                   (list :suite-id 200 :app-name "Codecov"
                                         :workflow-id nil :workflow-run-name nil)))
             (callback-result nil)
             (shipit--workflow-name-cache (make-hash-table :test 'equal)))
        (shipit--fetch-check-runs-with-names-async
         backend config suite-info-list
         (lambda (result) (setq callback-result result)))
        ;; THEN both suite IDs were dispatched
        (should (= 2 (length captured-suite-ids)))
        (should (memq 100 captured-suite-ids))
        (should (memq 200 captured-suite-ids))))))

(ert-deftest test-shipit-checks-workflow-names-gated-behind-backend-support ()
  "GIVEN a mock backend WITHOUT :fetch-action-run-info-async
WHEN shipit--fetch-workflow-names-for-runs-async is called
THEN workflow name fetching is skipped and finalization proceeds."
  (require 'shipit-checks)
  (let ((finalized nil))
    (test-shipit-pr--with-mock-backend ()
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (shipit--workflow-run-cache (make-hash-table :test 'equal))
             (shipit--cached-pr-checks nil)
             (shipit--checks-fetched nil)
             (shipit--checks-loading t))
        ;; Backend does NOT have :fetch-action-run-info-async
        (should-not (plist-get backend :fetch-action-run-info-async))
        (shipit--fetch-workflow-names-for-runs-async
         backend config '("run-1" "run-2")
         '(((name . "test") (status . "completed") (conclusion . "success")
            (html_url . "http://ci")))
         (lambda (result) (setq finalized t)))
        ;; THEN finalization was called (workflow fetching skipped)
        (should finalized)))))

(ert-deftest test-shipit-checks-workflow-names-fetched-when-backend-supports ()
  "GIVEN a mock backend WITH :fetch-action-run-info-async
WHEN shipit--fetch-workflow-names-for-runs-async is called
THEN workflow names are fetched via backend dispatch."
  (require 'shipit-checks)
  (let ((captured-run-ids nil)
        (finalized nil))
    (test-shipit-pr--with-mock-backend
        (:fetch-action-run-info-async
         (lambda (config run-id callback)
           (push run-id captured-run-ids)
           (funcall callback `((name . ,(format "Workflow for %s" run-id))))))
      (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
             (backend (car resolved))
             (config (cdr resolved))
             (shipit--workflow-run-cache (make-hash-table :test 'equal))
             (shipit--cached-pr-checks nil)
             (shipit--checks-fetched nil)
             (shipit--checks-loading t))
        (shipit--fetch-workflow-names-for-runs-async
         backend config '("run-1" "run-2")
         '(((name . "test") (status . "completed") (conclusion . "success")
            (html_url . "http://ci")))
         (lambda (result) (setq finalized t)))
        ;; THEN both run IDs were dispatched
        (should (= 2 (length captured-run-ids)))
        ;; THEN workflow names are cached
        (should (gethash "run-1" shipit--workflow-run-cache))
        (should (gethash "run-2" shipit--workflow-run-cache))
        ;; THEN finalization was called
        (should finalized)))))

;;; PR URL section tests

(ert-deftest test-shipit-insert-pr-url-section ()
  "GIVEN pr-data with a repo html_url under base.repo
WHEN inserting the URL section
THEN a clickable repo homepage URL line is inserted with an overlay
AND RET calls shipit-open-repo-buffer."
  (require 'shipit-buffer)
  (let ((pr-data '((base . ((repo . ((html_url . "https://github.com/owner/repo")
                                      (full_name . "owner/repo")))))))
        (opened-repo nil))
    (with-temp-buffer
      (let ((magit-insert-section--parent (magit-insert-section (root))))
        (shipit--insert-pr-url-section pr-data)
        ;; THEN the buffer contains the repo homepage URL
        (should (string-match-p "https://github.com/owner/repo"
                                (buffer-string)))
        ;; THEN the buffer contains "URL:" label
        (should (string-match-p "URL:" (buffer-string)))
        ;; THEN an overlay with keymap and help-echo exists on the URL
        (goto-char (point-min))
        (search-forward "https://github.com/owner/repo")
        (let ((ovs (overlays-at (1- (point)))))
          (should ovs)
          (should (overlay-get (car ovs) 'keymap))
          (should (string= "RET: open repo buffer"
                           (overlay-get (car ovs) 'help-echo)))
          (should (eq 'highlight (overlay-get (car ovs) 'mouse-face))))
        ;; THEN the URL text has 'link face
        (goto-char (point-min))
        (search-forward "https://")
        (should (eq 'link (get-text-property (1- (point)) 'face)))
        ;; THEN RET calls shipit-open-repo-buffer with the repo name
        (goto-char (point-min))
        (search-forward "https://github.com/owner/repo")
        (let ((ovs (overlays-at (1- (point)))))
          (cl-letf (((symbol-function 'shipit-open-repo-buffer)
                     (lambda (repo) (setq opened-repo repo))))
            (funcall (lookup-key (overlay-get (car ovs) 'keymap) (kbd "RET")))))
        (should (string= "owner/repo" opened-repo))))))

(ert-deftest test-shipit-insert-pr-url-section-fallback-to-full-name ()
  "GIVEN pr-data with full_name but no html_url on repo
WHEN inserting the URL section with a backend providing :browse-repo-url
THEN a URL is constructed via backend dispatch."
  (require 'shipit-buffer)
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (pr-data '((base . ((repo . ((full_name . "owner/repo"))))))))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :browse-repo-url
                 (lambda (config)
                   (format "https://github.com/%s" (plist-get config :repo))))
      (shipit-pr-register-backend 'mock plist))
    (with-temp-buffer
      (let ((magit-insert-section--parent (magit-insert-section (root))))
        (shipit--insert-pr-url-section pr-data)
        ;; THEN the buffer contains the constructed URL
        (should (string-match-p "https://github.com/owner/repo"
                                (buffer-string)))))))

(ert-deftest test-shipit-insert-pr-url-section-no-repo ()
  "GIVEN pr-data without base.repo
WHEN inserting the URL section
THEN nothing is inserted."
  (require 'shipit-buffer)
  (let ((pr-data '((title . "Some PR"))))
    (with-temp-buffer
      (let ((magit-insert-section--parent (magit-insert-section (root))))
        (shipit--insert-pr-url-section pr-data)
        ;; THEN the buffer is empty
        (should (string= "" (buffer-string)))))))

;;; Backend Key Dispatch Tests — :inject-project-path

(ert-deftest test-pr-backend-inject-project-path-gitlab ()
  "GIVEN a GitLab PR backend with :inject-project-path t
WHEN resolving for a repo without :project-path in config
THEN :project-path is injected from repo slug."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "GitLab")
      (plist-put plist :inject-project-path t)
      (shipit-pr-register-backend 'gitlab plist))
    (let* ((result (shipit-pr--resolve-for-repo "group/project"))
           (config (cdr result)))
      (should (string= "group/project" (plist-get config :project-path))))))

(ert-deftest test-pr-backend-inject-project-path-github ()
  "GIVEN a GitHub PR backend without :inject-project-path
WHEN resolving for a repo
THEN :project-path is NOT injected."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (shipit-pr-register-backend 'github (test-shipit-pr--make-minimal-plist))
    (let* ((result (shipit-pr--resolve-for-repo "owner/repo"))
           (config (cdr result)))
      (should-not (plist-get config :project-path)))))

(ert-deftest test-pr-backend-inject-project-path-skips-when-already-set ()
  "GIVEN a backend with :inject-project-path and config already has :project-path
WHEN resolving for a repo
THEN the existing :project-path is preserved."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:project-path "custom/path")))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :name "GitLab")
      (plist-put plist :inject-project-path t)
      (shipit-pr-register-backend 'gitlab plist))
    (let* ((result (shipit-pr--resolve-for-repo "group/project"))
           (config (cdr result)))
      (should (string= "custom/path" (plist-get config :project-path))))))

;;; Backend Key Dispatch Tests — :pr-type-short-label

(ert-deftest test-pr-backend-pr-type-short-label-github ()
  "GIVEN a GitHub backend with :pr-type-short-label \"PR\"
WHEN reading :pr-type-short-label from the backend
THEN \"PR\" is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :pr-type-short-label "PR")
      (shipit-pr-register-backend 'github plist))
    (should (string= "PR" (plist-get (shipit-pr--get-backend) :pr-type-short-label)))))

(ert-deftest test-pr-backend-pr-type-short-label-gitlab ()
  "GIVEN a GitLab backend with :pr-type-short-label \"MR\"
WHEN reading :pr-type-short-label from the backend
THEN \"MR\" is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'gitlab))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :pr-type-short-label "MR")
      (shipit-pr-register-backend 'gitlab plist))
    (should (string= "MR" (plist-get (shipit-pr--get-backend) :pr-type-short-label)))))

;;; Backend Key Dispatch Tests — :detect-hash-reference

(ert-deftest test-pr-backend-detect-hash-reference-github ()
  "GIVEN a GitHub backend with :detect-hash-reference
WHEN reading :detect-hash-reference from the backend
THEN a function is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :detect-hash-reference (lambda (_c _n _r) 'pr))
      (shipit-pr-register-backend 'github plist))
    (should (plist-get (shipit-pr--get-backend) :detect-hash-reference))))

(ert-deftest test-pr-backend-detect-hash-reference-nil-without-key ()
  "GIVEN a backend without :detect-hash-reference
WHEN reading :detect-hash-reference from the backend
THEN nil is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock))
    (shipit-pr-register-backend 'mock (test-shipit-pr--make-minimal-plist))
    (should-not (plist-get (shipit-pr--get-backend) :detect-hash-reference))))

;;; Backend Key Dispatch Tests — :email-avatar-url

(ert-deftest test-pr-backend-email-avatar-url-github ()
  "GIVEN a GitHub backend with :email-avatar-url
WHEN calling the function with an email
THEN a URL string is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :email-avatar-url
                 (lambda (_config email)
                   (format "https://avatars.example.com/u/e?email=%s" email)))
      (shipit-pr-register-backend 'github plist))
    (let ((fn (plist-get (shipit-pr--get-backend) :email-avatar-url)))
      (should fn)
      (should (string= "https://avatars.example.com/u/e?email=test@example.com"
                        (funcall fn nil "test@example.com"))))))

(ert-deftest test-pr-backend-email-avatar-url-nil-without-key ()
  "GIVEN a backend without :email-avatar-url
WHEN reading :email-avatar-url from the backend
THEN nil is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock))
    (shipit-pr-register-backend 'mock (test-shipit-pr--make-minimal-plist))
    (should-not (plist-get (shipit-pr--get-backend) :email-avatar-url))))

;;; Backend Key Dispatch Tests — :invalidate-pr-cache

(ert-deftest test-pr-backend-invalidate-pr-cache-github ()
  "GIVEN a GitHub backend with :invalidate-pr-cache
WHEN calling the function
THEN the cache invalidation side-effect occurs."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'github)
        (invalidated-endpoint nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :invalidate-pr-cache
                 (lambda (config pr-number)
                   (setq invalidated-endpoint
                         (format "/repos/%s/pulls/%s" (plist-get config :repo) pr-number))))
      (shipit-pr-register-backend 'github plist))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (fn (plist-get backend :invalidate-pr-cache)))
      (funcall fn config 42)
      (should (string= "/repos/owner/repo/pulls/42" invalidated-endpoint)))))

(ert-deftest test-pr-backend-invalidate-pr-cache-nil-without-key ()
  "GIVEN a backend without :invalidate-pr-cache
WHEN reading :invalidate-pr-cache from the backend
THEN nil is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock))
    (shipit-pr-register-backend 'mock (test-shipit-pr--make-minimal-plist))
    (should-not (plist-get (shipit-pr--get-backend) :invalidate-pr-cache))))

(ert-deftest test-pr-backend-build-search-query-parts-dispatches ()
  "GIVEN a backend with :build-search-query-parts
WHEN dispatching through the backend plist
THEN the function is called with args and repo."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :build-search-query-parts
                 (lambda (args repo) (list :args args :repo repo)))
      (shipit-pr-register-backend 'mock plist))
    (let* ((backend (shipit-pr--get-backend))
           (fn (plist-get backend :build-search-query-parts)))
      (should fn)
      (should (equal '(:args ("--state=open") :repo "owner/repo")
                     (funcall fn '("--state=open") "owner/repo"))))))

(ert-deftest test-pr-backend-build-search-query-parts-nil-without-key ()
  "GIVEN a backend without :build-search-query-parts
WHEN reading the key from the backend
THEN nil is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock))
    (shipit-pr-register-backend 'mock (test-shipit-pr--make-minimal-plist))
    (should-not (plist-get (shipit-pr--get-backend) :build-search-query-parts))))

(ert-deftest test-pr-backend-hash-insert-reference-fn-dispatches ()
  "GIVEN a backend with :hash-insert-reference-fn
WHEN reading the key from the backend
THEN a function is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :hash-insert-reference-fn #'ignore)
      (shipit-pr-register-backend 'mock plist))
    (should (plist-get (shipit-pr--get-backend) :hash-insert-reference-fn))))

(ert-deftest test-pr-backend-hash-insert-reference-fn-defaults-nil ()
  "GIVEN a backend without :hash-insert-reference-fn
WHEN reading the key
THEN nil is returned (caller uses default)."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock))
    (shipit-pr-register-backend 'mock (test-shipit-pr--make-minimal-plist))
    (should-not (plist-get (shipit-pr--get-backend) :hash-insert-reference-fn))))

(ert-deftest test-pr-backend-emoji-fallback-dispatches ()
  "GIVEN a backend with :emoji-fallback
WHEN reading the key
THEN the registered emoji is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :emoji-fallback "🔧")
      (shipit-pr-register-backend 'mock plist))
    (should (string= "🔧" (plist-get (shipit-pr--get-backend) :emoji-fallback)))))

(ert-deftest test-autodiscover-dispatches-through-issue-backends ()
  "GIVEN a mock issue backend with :autodiscover and :notifications
WHEN calling shipit--autodiscover-notification-backends
THEN the backend's :autodiscover function is called."
  (require 'shipit-notifications)
  (let ((shipit-issue-backends nil)
        (shipit-issue-repo-backends nil)
        (discover-called nil))
    (push (cons 'mock-be
                (list :name "Mock"
                      :fetch-issue #'ignore :fetch-comments #'ignore
                      :fetch-comments-async #'ignore :search #'ignore
                      :create-issue #'ignore :reference-patterns (lambda (_c) nil)
                      :browse-url #'ignore :id-to-string #'ignore
                      :string-to-id #'ignore
                      :notifications #'ignore
                      :autodiscover (lambda ()
                                      (setq discover-called t)
                                      (cons "mock.com" (list :backend 'mock-be)))))
          shipit-issue-backends)
    (let ((result (shipit--autodiscover-notification-backends)))
      (should discover-called)
      (should (= 1 (length result)))
      (should (string= "mock.com" (car (car result)))))))

(ert-deftest test-issue-backend-icon-spec-dispatches ()
  "GIVEN an issue backend with :icon-spec and :icon-fallback-text
WHEN looking up the icon spec from the registry
THEN the registered values are returned."
  (let ((shipit-issue-backends nil))
    (push (cons 'mock
                (list :name "Mock"
                      :fetch-issue #'ignore :fetch-comments #'ignore
                      :fetch-comments-async #'ignore :search #'ignore
                      :create-issue #'ignore :reference-patterns (lambda (_c) nil)
                      :browse-url #'ignore :id-to-string #'ignore
                      :string-to-id #'ignore
                      :icon-spec '("mock-icon" "simple" . "#FF0000")
                      :icon-fallback-text "MK"))
          shipit-issue-backends)
    (let ((plist (cdr (assq 'mock shipit-issue-backends))))
      (should (equal '("mock-icon" "simple" . "#FF0000")
                     (plist-get plist :icon-spec)))
      (should (string= "MK" (plist-get plist :icon-fallback-text))))))

(ert-deftest test-pr-backend-browse-repo-url-dispatches ()
  "GIVEN a backend with :browse-repo-url
WHEN calling the function
THEN a URL is returned."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-pr--make-minimal-plist)))
      (plist-put plist :browse-repo-url
                 (lambda (config) (format "https://example.com/%s" (plist-get config :repo))))
      (shipit-pr-register-backend 'mock plist))
    (let* ((resolved (shipit-pr--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (fn (plist-get backend :browse-repo-url)))
      (should (string= "https://example.com/owner/repo"
                       (funcall fn config))))))

;;; Check item expansion tests

(ert-deftest test-shipit-checks-finalize-preserves-id ()
  "GIVEN check runs with id fields
   WHEN finalize-async-checks processes them
   THEN the id is preserved in the finalized data."
  (require 'shipit-checks)
  (let ((check-runs '(((id . 12345)
                        (name . "build")
                        (status . "completed")
                        (conclusion . "success")
                        (html_url . "https://github.com/owner/repo/actions/runs/100/job/12345")
                        (workflow-name . "CI")
                        (workflow-run-name . "CI"))))
        (result nil))
    (shipit--finalize-async-checks
     check-runs
     (lambda (checks) (setq result checks)))
    (should (= 1 (length result)))
    (should (= 12345 (cdr (assq 'id (car result)))))))

(ert-deftest test-shipit-checks-insert-check-items-creates-sections ()
  "GIVEN a list of checks
   WHEN insert-check-items is called
   THEN check-item magit sections are created with correct text."
  (require 'shipit-checks)
  (let ((buf (generate-new-buffer "*test-check-items*")))
    (unwind-protect
        (with-current-buffer buf
          (magit-section-mode)
          (let ((inhibit-read-only t))
            (magit-insert-section (root)
              (shipit--insert-check-items
               '(((name . "build") (id . 100)
                  (status . "completed") (conclusion . "success")
                  (html_url . "https://example.com"))
                 ((name . "test") (id . 101)
                  (status . "completed") (conclusion . "failure")
                  (html_url . "https://example.com")))
               "  ")))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "build" text))
            (should (string-match-p "test" text)))
          ;; Verify check-item sections via root's children
          (should (oref magit-root-section children))
          (let ((child (car (oref magit-root-section children))))
            (should (eq (oref child type) 'check-item))
            (should (= 100 (cdr (assq 'id (oref child value)))))))
      (kill-buffer buf))))

(provide 'test-shipit-pr-backends)
;;; test-shipit-pr-backends.el ends here
