;;; test-shipit-discussion-backends.el --- Tests for discussion backend registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for pluggable discussion backend system.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-discussion-backends)

;;; Test helpers

(defun test-shipit-discussion--make-minimal-plist ()
  "Return a minimal valid discussion backend plist with all required keys."
  (list :name "Test"
        :browse-url (lambda (_config _number) "https://example.com/discussions/1")
        :browse-repo-url (lambda (_config) "https://example.com")))

;;; Registration Tests

(ert-deftest test-discussion-backend-registration ()
  "GIVEN a valid discussion backend plist with required keys
WHEN registering the backend
THEN it is stored in shipit-discussion-backends."
  (let ((shipit-discussion-backends nil))
    (shipit-discussion-register-backend
     'github (test-shipit-discussion--make-minimal-plist))
    (should (assq 'github shipit-discussion-backends))
    (should (string= "Test"
                      (plist-get (cdr (assq 'github shipit-discussion-backends))
                                 :name)))))

(ert-deftest test-discussion-backend-registration-missing-key ()
  "GIVEN a discussion backend plist missing required key :browse-url
WHEN registering the backend
THEN an error is signaled."
  (let ((shipit-discussion-backends nil))
    (should-error
     (shipit-discussion-register-backend
      'bad-be
      (list :name "Bad"
            :browse-repo-url (lambda (_config) "https://example.com"))))))

(ert-deftest test-discussion-backend-registration-replaces-existing ()
  "GIVEN a backend already registered under id 'my-be
WHEN registering a new backend with the same id
THEN the old backend is replaced."
  (let ((shipit-discussion-backends nil))
    (let ((plist1 (test-shipit-discussion--make-minimal-plist))
          (plist2 (test-shipit-discussion--make-minimal-plist)))
      (plist-put plist1 :name "V1")
      (plist-put plist2 :name "V2")
      (shipit-discussion-register-backend 'my-be plist1)
      (shipit-discussion-register-backend 'my-be plist2)
      (should (string= "V2"
                        (plist-get (cdr (assq 'my-be shipit-discussion-backends))
                                   :name)))
      (should (= 1 (length shipit-discussion-backends))))))

;;; Browse URL Tests

(ert-deftest test-discussion-backend-browse-url ()
  "GIVEN a registered GitHub discussion backend
WHEN calling :browse-url with a config and number
THEN it returns the correct discussion URL."
  (let ((shipit-discussion-backends nil))
    (shipit-discussion-register-backend
     'github
     (list :name "GitHub"
           :browse-url (lambda (config number)
                         (format "https://github.com/%s/discussions/%s"
                                 (plist-get config :repo) number))
           :browse-repo-url (lambda (config)
                              (format "https://github.com/%s"
                                      (plist-get config :repo)))))
    (let* ((backend (cdr (assq 'github shipit-discussion-backends)))
           (url (funcall (plist-get backend :browse-url)
                         '(:repo "owner/repo") 42)))
      (should (string= "https://github.com/owner/repo/discussions/42" url)))))

(ert-deftest test-discussion-backend-browse-repo-url ()
  "GIVEN a registered GitHub discussion backend
WHEN calling :browse-repo-url with a config
THEN it returns the correct repo URL."
  (let ((shipit-discussion-backends nil))
    (shipit-discussion-register-backend
     'github
     (list :name "GitHub"
           :browse-url (lambda (config number)
                         (format "https://github.com/%s/discussions/%s"
                                 (plist-get config :repo) number))
           :browse-repo-url (lambda (config)
                              (format "https://github.com/%s"
                                      (plist-get config :repo)))))
    (let* ((backend (cdr (assq 'github shipit-discussion-backends)))
           (url (funcall (plist-get backend :browse-repo-url)
                         '(:repo "owner/repo"))))
      (should (string= "https://github.com/owner/repo" url)))))

;;; Resolve for Repo Tests

(ert-deftest test-discussion-backend-resolve-for-repo ()
  "GIVEN a registered GitHub discussion backend and shipit-pr-backend set to 'github
WHEN calling shipit-discussion--resolve-for-repo
THEN it returns (backend . config) with :repo injected."
  (let ((shipit-discussion-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (shipit-discussion-register-backend
     'github (test-shipit-discussion--make-minimal-plist))
    (let ((resolved (shipit-discussion--resolve-for-repo "owner/repo")))
      (should (consp resolved))
      ;; Car is the backend plist
      (should (string= "Test" (plist-get (car resolved) :name)))
      ;; Cdr is the config plist with :repo injected
      (should (string= "owner/repo" (plist-get (cdr resolved) :repo))))))

(ert-deftest test-discussion-backend-resolve-for-repo-with-base-config ()
  "GIVEN shipit-pr-backend-config has an existing key :token
WHEN calling shipit-discussion--resolve-for-repo
THEN the returned config contains both :repo and :token."
  (let ((shipit-discussion-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config '(:token "abc123")))
    (shipit-discussion-register-backend
     'github (test-shipit-discussion--make-minimal-plist))
    (let* ((resolved (shipit-discussion--resolve-for-repo "owner/repo"))
           (config (cdr resolved)))
      (should (string= "owner/repo" (plist-get config :repo)))
      (should (string= "abc123" (plist-get config :token))))))

(ert-deftest test-discussion-backend-resolve-for-repo-injects-project-path ()
  "GIVEN a discussion backend that asks for project-path injection
WHEN resolving it for a repository without one
THEN :project-path is filled in from the repository slug.

Only GitHub registers a discussion backend today, so nothing exercises
this branch in practice.  The test names a forge that asks for the
injection rather than one that exists, because what is under test is
the resolver, which any forge needing a project path would rely on."
  (let ((shipit-discussion-backends nil)
        (shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config nil))
    (shipit-discussion-register-backend
     'gitlab
     (list :name "GitLab"
           :inject-project-path t
           :browse-url (lambda (_config _number) "")
           :browse-repo-url (lambda (_config) "")))
    (let* ((resolved (shipit-discussion--resolve-for-repo "group/project"))
           (config (cdr resolved)))
      (should (string= "group/project" (plist-get config :project-path))))))

;;; Get Backend Tests

(ert-deftest test-discussion-backend-get-backend-errors-when-not-registered ()
  "GIVEN no discussion backend is registered for 'github
WHEN calling shipit-discussion--get-backend with shipit-pr-backend set to 'github
THEN an error is signaled."
  (let ((shipit-discussion-backends nil)
        (shipit-pr-backend 'github))
    (should-error (shipit-discussion--get-backend))))

;;; GitHub Backend Registration (via discussions-graphql) Tests

(ert-deftest test-github-discussion-backend-is-registered ()
  "GIVEN the GitHub discussions module
WHEN the registry is read
THEN a `github\=' backend is registered, with the keys the registry
requires of one.

The registry is read as it stands rather than bound to nil first.
Registration happens once, when the module loads, so emptying the
registry and requiring the module again proves nothing: the require
is a no-op whenever anything else has already loaded it, and the test
then passes or fails on the order the suite happened to run in."
  (require 'shipit-discussions-graphql)
  (should (assq 'github shipit-discussion-backends))
  (let ((backend (cdr (assq 'github shipit-discussion-backends))))
    (should (plist-get backend :name))
    (should (plist-get backend :browse-url))
    (should (plist-get backend :browse-repo-url))))

(provide 'test-shipit-discussion-backends)
;;; test-shipit-discussion-backends.el ends here
