;;; test-shipit-issue-backends.el --- Tests for issue backend registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for pluggable issue backend system.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-issue-backends)

;;; Registration Tests

(ert-deftest test-shipit-issue-register-backend-basic ()
  "GIVEN a valid backend plist with all required keys
WHEN registering the backend
THEN it is stored in shipit-issue-backends."
  (let ((shipit-issue-backends nil))
    (shipit-issue-register-backend
     'test-backend
     (list :name "Test"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns #'ignore
           :browse-url #'ignore
           :id-to-string #'ignore
           :string-to-id #'ignore))
    (should (assq 'test-backend shipit-issue-backends))))

(ert-deftest test-shipit-issue-register-backend-missing-key ()
  "GIVEN a backend plist missing required key :fetch-issue
WHEN registering the backend
THEN an error is signaled."
  (let ((shipit-issue-backends nil))
    (should-error
     (shipit-issue-register-backend
      'bad-backend
      (list :name "Bad"
            :fetch-comments #'ignore
            :fetch-comments-async #'ignore
            :search #'ignore
            :reference-patterns #'ignore
            :browse-url #'ignore
            :id-to-string #'ignore
            :string-to-id #'ignore)))))

(ert-deftest test-shipit-issue-register-backend-replaces-existing ()
  "GIVEN a backend already registered under id 'my-be
WHEN registering a new backend with the same id
THEN the old backend is replaced."
  (let ((shipit-issue-backends nil))
    (let ((plist1 (list :name "V1"
                        :fetch-issue #'ignore
                        :fetch-comments #'ignore
                        :fetch-comments-async #'ignore
                        :search #'ignore
                        :create-issue #'ignore
                        :reference-patterns #'ignore
                        :browse-url #'ignore
                        :id-to-string #'ignore
                        :string-to-id #'ignore))
          (plist2 (list :name "V2"
                        :fetch-issue #'ignore
                        :fetch-comments #'ignore
                        :fetch-comments-async #'ignore
                        :search #'ignore
                        :create-issue #'ignore
                        :reference-patterns #'ignore
                        :browse-url #'ignore
                        :id-to-string #'ignore
                        :string-to-id #'ignore)))
      (shipit-issue-register-backend 'my-be plist1)
      (shipit-issue-register-backend 'my-be plist2)
      (should (string= "V2" (plist-get (cdr (assq 'my-be shipit-issue-backends)) :name)))
      (should (= 1 (length shipit-issue-backends))))))

;;; Lookup Tests

(ert-deftest test-shipit-issue-get-backend-returns-active ()
  "GIVEN 'github backend is registered and shipit-issue-backend is 'github
WHEN calling shipit-issue--get-backend
THEN the github backend plist is returned."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github))
    (shipit-issue-register-backend
     'github
     (list :name "GitHub"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns #'ignore
           :browse-url #'ignore
           :id-to-string #'ignore
           :string-to-id #'ignore))
    (let ((backend (shipit-issue--get-backend)))
      (should backend)
      (should (string= "GitHub" (plist-get backend :name))))))

(ert-deftest test-shipit-issue-get-backend-errors-on-unknown ()
  "GIVEN no backends registered
WHEN calling shipit-issue--get-backend with backend set to 'unknown
THEN an error is signaled."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'unknown))
    (should-error (shipit-issue--get-backend))))

(ert-deftest test-shipit-issue-get-config-returns-config ()
  "GIVEN shipit-issue-backend-config is set
WHEN calling shipit-issue--get-config
THEN the config plist is returned."
  (let ((shipit-issue-backend-config '(:base-url "https://example.com")))
    (should (equal '(:base-url "https://example.com")
                   (shipit-issue--get-config)))))

(ert-deftest test-shipit-issue-get-config-returns-nil-when-unset ()
  "GIVEN shipit-issue-backend-config is nil
WHEN calling shipit-issue--get-config
THEN nil is returned."
  (let ((shipit-issue-backend-config nil))
    (should-not (shipit-issue--get-config))))

;;; Backend Iteration Tests

(ert-deftest test-shipit-issue-all-reference-patterns-for-repo ()
  "GIVEN a repo configured with a Jira backend
WHEN calling shipit-issue--all-reference-patterns
THEN patterns from the resolved backend are returned."
  (let ((shipit-issue-backends nil)
        (shipit-issue-repo-backends
         '(("myorg/proj" :backend jira :project-keys ("PRJ")))))
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns (lambda (config)
                                 (mapcar (lambda (k)
                                           (list (format "\\b\\(%s-[0-9]+\\)" k) 1 #'identity))
                                         (plist-get config :project-keys)))
           :browse-url #'ignore
           :id-to-string #'ignore
           :string-to-id #'ignore))
    (let ((all-patterns (shipit-issue--all-reference-patterns "myorg/proj")))
      (should (= 1 (length all-patterns)))
      (should (eq 'jira (car (car all-patterns)))))))

;;; Per-Repo Backend Resolution Tests

(ert-deftest test-shipit-issue-resolve-for-repo-exact-match ()
  "GIVEN shipit-issue-repo-backends has an entry for \"myorg/jira-proj\"
WHEN resolving for that repo
THEN the Jira backend and its config are returned."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends
         '(("myorg/jira-proj" :backend jira
            :base-url "https://jira.example.com"
            :project-keys ("PRJ")))))
    (shipit-issue-register-backend
     'github
     (list :name "GitHub"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore :reference-patterns #'ignore :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore :reference-patterns #'ignore :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (let ((result (shipit-issue--resolve-for-repo "myorg/jira-proj")))
      (should (string= "Jira" (plist-get (car result) :name)))
      (should (string= "https://jira.example.com"
                        (plist-get (cdr result) :base-url))))))

(ert-deftest test-shipit-issue-resolve-for-repo-regex-match ()
  "GIVEN shipit-issue-repo-backends has a regex pattern
WHEN resolving for a matching repo
THEN the correct backend is returned."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends
         '(("myorg/.*" :backend jira
            :base-url "https://jira.example.com"
            :project-keys ("PRJ")))))
    (shipit-issue-register-backend
     'github
     (list :name "GitHub"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore :reference-patterns #'ignore :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore :reference-patterns #'ignore :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (let ((result (shipit-issue--resolve-for-repo "myorg/some-project")))
      (should (string= "Jira" (plist-get (car result) :name))))))

(ert-deftest test-shipit-issue-resolve-for-repo-falls-back-to-global ()
  "GIVEN shipit-issue-repo-backends has no match for \"other/repo\"
WHEN resolving for that repo
THEN the global shipit-issue-backend setting is used."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends
         '(("myorg/jira-proj" :backend jira
            :base-url "https://jira.example.com"
            :project-keys ("PRJ")))))
    (shipit-issue-register-backend
     'github
     (list :name "GitHub"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore :reference-patterns #'ignore :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore :reference-patterns #'ignore :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (let ((result (shipit-issue--resolve-for-repo "other/repo")))
      (should (string= "GitHub" (plist-get (car result) :name))))))

(ert-deftest test-shipit-issue-resolve-config-includes-repo ()
  "GIVEN a per-repo backend config
WHEN resolving for a matched repo
THEN the returned config includes :repo."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends
         '(("myorg/proj" :backend jira
            :base-url "https://jira.example.com"
            :project-keys ("PRJ")))))
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore :reference-patterns #'ignore :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (let ((result (shipit-issue--resolve-for-repo "myorg/proj")))
      (should (string= "myorg/proj" (plist-get (cdr result) :repo))))))

(ert-deftest test-shipit-backend-reference-overlay-created-for-jira-key ()
  "GIVEN a buffer containing PROJ-11494 and a Jira backend configured for acme/.*
WHEN creating backend reference overlays
THEN an overlay is created on the PROJ-11494 text."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends
         '(("acme/.*" :backend jira
            :base-url "https://test.atlassian.net"
            :project-keys ("PROJ")))))
    (shipit-issue-register-backend
     'github
     (list :name "GitHub"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore
           :reference-patterns (lambda (_config) '(("#\\([0-9]+\\)" 1 identity)))
           :browse-url #'ignore
           :id-to-string #'ignore :string-to-id #'ignore))
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore :fetch-comments #'ignore
           :fetch-comments-async #'ignore :search #'ignore
           :create-issue #'ignore
           :reference-patterns #'shipit-issue-jira--reference-patterns
           :browse-url (lambda (_config id) (format "https://test/%s" id))
           :id-to-string #'ignore :string-to-id #'ignore))
    (with-temp-buffer
      (insert "   Part of PROJ-11494\n")
      (let ((overlay-count 0))
        (shipit--create-backend-reference-overlays
         "acme/acme-python" 1 (point-max)
         #'ignore (lambda () (setq overlay-count (1+ overlay-count))))
        ;; THEN overlay was created
        (should (= 1 overlay-count))
        ;; AND overlay covers the right text
        (let ((ovs (overlays-in 1 (point-max))))
          (should (>= (length ovs) 1))
          (let* ((ov (car ovs))
                 (text (buffer-substring-no-properties
                        (overlay-start ov) (overlay-end ov))))
            (should (string= "PROJ-11494" text))))))))

;;; Reactions Capability Tests

(ert-deftest test-shipit-issue-backend-has-reactions-p-true ()
  "GIVEN a backend plist with :fetch-reactions key
WHEN checking has-reactions-p
THEN returns non-nil."
  (let ((plist (list :name "Test" :fetch-reactions #'ignore)))
    (should (shipit-issue--backend-has-reactions-p plist))))

(ert-deftest test-shipit-issue-backend-has-reactions-p-false ()
  "GIVEN a backend plist without :fetch-reactions key
WHEN checking has-reactions-p
THEN returns nil."
  (let ((plist (list :name "Test")))
    (should-not (shipit-issue--backend-has-reactions-p plist))))

(provide 'test-shipit-issue-backends)
;;; test-shipit-issue-backends.el ends here
