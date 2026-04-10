;;; test-thread-subscription.el --- Tests for thread subscription -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for per-thread subscription management.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-http)
(require 'shipit-pr-github)
(require 'shipit-repo-buffer)

;;; Test helpers

(defun test-thread-sub--make-minimal-plist ()
  "Return a minimal valid PR backend plist."
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

(defmacro test-thread-sub--with-mock-backend (plist-extras &rest body)
  "Run BODY with a test backend that has PLIST-EXTRAS merged in."
  (declare (indent 1))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'test-backend)
         (shipit-pr-backend-config nil)
         (shipit-current-repo "owner/repo"))
     (shipit-pr-register-backend
      'test-backend
      (append (test-thread-sub--make-minimal-plist) ,plist-extras))
     ,@body))

;;; Tests -- dispatch wrappers

(ert-deftest test-thread-sub-get-dispatches-through-registry ()
  ;; GIVEN a backend with :get-thread-subscription registered
  ;; WHEN calling shipit--get-thread-subscription
  ;; THEN the backend function is called with config, repo, type, number.
  (let ((called-with nil))
    (test-thread-sub--with-mock-backend
        (list :get-thread-subscription
              (lambda (config repo type number)
                (setq called-with (list config repo type number))
                "subscribed"))
      (let ((result (shipit--get-thread-subscription "owner/repo" "pr" 42)))
        (should (equal result "subscribed"))
        (should (equal (nth 1 called-with) "owner/repo"))
        (should (equal (nth 2 called-with) "pr"))
        (should (equal (nth 3 called-with) 42))))))

(ert-deftest test-thread-sub-get-returns-nil-when-unsupported ()
  ;; GIVEN a backend without :get-thread-subscription
  ;; WHEN calling shipit--get-thread-subscription
  ;; THEN returns nil.
  (test-thread-sub--with-mock-backend nil
    (should-not (shipit--get-thread-subscription "owner/repo" "pr" 42))))

(ert-deftest test-thread-sub-set-dispatches-through-registry ()
  ;; GIVEN a backend with :set-thread-subscription registered
  ;; WHEN calling shipit--set-thread-subscription
  ;; THEN the backend function is called with config, repo, type, number, subscribed.
  (let ((called-with nil))
    (test-thread-sub--with-mock-backend
        (list :set-thread-subscription
              (lambda (config repo type number subscribed)
                (setq called-with (list config repo type number subscribed))
                t))
      (shipit--set-thread-subscription "owner/repo" "pr" 42 t)
      (should (equal (nth 1 called-with) "owner/repo"))
      (should (equal (nth 2 called-with) "pr"))
      (should (equal (nth 3 called-with) 42))
      (should (equal (nth 4 called-with) t)))))

(ert-deftest test-thread-sub-set-returns-nil-when-unsupported ()
  ;; GIVEN a backend without :set-thread-subscription
  ;; WHEN calling shipit--set-thread-subscription
  ;; THEN returns nil.
  (test-thread-sub--with-mock-backend nil
    (should-not (shipit--set-thread-subscription "owner/repo" "pr" 42 t))))

;;; Tests -- GitHub PR/Issue thread subscription

(defmacro test-thread-sub--with-mock-github (&rest body)
  "Run BODY with GitHub backend and standard test bindings."
  (declare (indent 0))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo"))
     ,@body))

(ert-deftest test-thread-sub-github-get-pr-subscribed ()
  ;; GIVEN a PR where the user is subscribed
  ;; WHEN getting thread subscription
  ;; THEN returns "subscribed".
  (test-thread-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _)
                 '((subscribed . t) (ignored . :json-false)))))
      (let ((result (shipit-pr-github--get-thread-subscription
                     '(:repo "owner/repo") "owner/repo" "pr" 42)))
        (should (equal result "subscribed"))))))

(ert-deftest test-thread-sub-github-get-issue-subscribed ()
  ;; GIVEN an issue where the user is subscribed
  ;; WHEN getting thread subscription
  ;; THEN returns "subscribed" and uses the issues endpoint.
  (test-thread-sub--with-mock-github
    (let ((requested-endpoint nil))
      (cl-letf (((symbol-function 'shipit--api-request)
                 (lambda (endpoint &rest _)
                   (setq requested-endpoint endpoint)
                   '((subscribed . t) (ignored . :json-false)))))
        (shipit-pr-github--get-thread-subscription
         '(:repo "owner/repo") "owner/repo" "issue" 8)
        (should (string-match-p "/repos/owner/repo/issues/8/subscription"
                                requested-endpoint))))))

(ert-deftest test-thread-sub-github-get-unsubscribed ()
  ;; GIVEN a PR where the user is not subscribed
  ;; WHEN getting thread subscription
  ;; THEN returns "unsubscribed".
  (test-thread-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _) nil)))
      (let ((result (shipit-pr-github--get-thread-subscription
                     '(:repo "owner/repo") "owner/repo" "pr" 42)))
        (should (equal result "unsubscribed"))))))

(ert-deftest test-thread-sub-github-get-ignored ()
  ;; GIVEN a PR where the user has ignored the thread
  ;; WHEN getting thread subscription
  ;; THEN returns "ignored".
  (test-thread-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _)
                 '((subscribed . :json-false) (ignored . t)))))
      (let ((result (shipit-pr-github--get-thread-subscription
                     '(:repo "owner/repo") "owner/repo" "pr" 42)))
        (should (equal result "ignored"))))))

(ert-deftest test-thread-sub-github-subscribe-pr ()
  ;; GIVEN a PR
  ;; WHEN subscribing to the thread
  ;; THEN PUT is sent with subscribed=true.
  (test-thread-sub--with-mock-github
    (let ((called-with nil))
      (cl-letf (((symbol-function 'shipit--api-request-post)
                 (lambda (endpoint data &optional method)
                   (setq called-with (list endpoint data method))
                   '((subscribed . t)))))
        (shipit-pr-github--set-thread-subscription
         '(:repo "owner/repo") "owner/repo" "pr" 42 t)
        (should (string-match-p "/repos/owner/repo/issues/42/subscription"
                                (nth 0 called-with)))
        (should (eq t (cdr (assq 'subscribed (nth 1 called-with)))))
        (should (equal "PUT" (nth 2 called-with)))))))

(ert-deftest test-thread-sub-github-unsubscribe-pr ()
  ;; GIVEN a PR the user is subscribed to
  ;; WHEN unsubscribing from the thread
  ;; THEN DELETE is sent to the subscription endpoint.
  (test-thread-sub--with-mock-github
    (let ((called-with nil))
      (cl-letf (((symbol-function 'shipit--api-request-post)
                 (lambda (endpoint data &optional method)
                   (setq called-with (list endpoint data method))
                   nil)))
        (shipit-pr-github--set-thread-subscription
         '(:repo "owner/repo") "owner/repo" "pr" 42 nil)
        (should (string-match-p "/repos/owner/repo/issues/42/subscription"
                                (nth 0 called-with)))
        (should (equal "DELETE" (nth 2 called-with)))))))

;;; Tests -- GitHub Discussion thread subscription

(ert-deftest test-thread-sub-github-get-discussion-subscribed ()
  ;; GIVEN a discussion where the user is subscribed
  ;; WHEN getting thread subscription
  ;; THEN returns "subscribed" via GraphQL.
  (test-thread-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query _vars)
                 '((repository
                    (discussion
                     (viewerSubscription . "SUBSCRIBED")
                     (id . "D_abc123")))))))
      (let ((result (shipit-pr-github--get-thread-subscription
                     '(:repo "owner/repo") "owner/repo" "discussion" 10)))
        (should (equal result "subscribed"))))))

(ert-deftest test-thread-sub-github-get-discussion-unsubscribed ()
  ;; GIVEN a discussion where the user is not subscribed
  ;; WHEN getting thread subscription
  ;; THEN returns "unsubscribed".
  (test-thread-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--graphql-query)
               (lambda (_query _vars)
                 '((repository
                    (discussion
                     (viewerSubscription . "UNSUBSCRIBED")
                     (id . "D_abc123")))))))
      (let ((result (shipit-pr-github--get-thread-subscription
                     '(:repo "owner/repo") "owner/repo" "discussion" 10)))
        (should (equal result "unsubscribed"))))))

(ert-deftest test-thread-sub-github-subscribe-discussion ()
  ;; GIVEN a discussion whose id-query resolves to "D_abc123"
  ;; WHEN subscribing to it
  ;; THEN the updateSubscription mutation is called with the node ID
  ;;      propagated from the id-query and state variable exactly "SUBSCRIBED".
  (test-thread-sub--with-mock-github
    (let ((mutation-vars nil)
          (mutation-called nil))
      (cl-letf (((symbol-function 'shipit--graphql-query)
                 (lambda (query vars)
                   (cond
                    ((string-match-p "updateSubscription" query)
                     (setq mutation-called t)
                     (setq mutation-vars vars)
                     '((updateSubscription
                        (subscribable
                         (viewerSubscription . "SUBSCRIBED")))))
                    (t
                     '((repository
                        (discussion
                         (viewerSubscription . "UNSUBSCRIBED")
                         (id . "D_abc123")))))))))
        (shipit-pr-github--set-thread-subscription
         '(:repo "owner/repo") "owner/repo" "discussion" 10 t)
        (should mutation-called)
        (should (equal (cdr (assq 'id mutation-vars)) "D_abc123"))
        (should (equal (cdr (assq 'state mutation-vars)) "SUBSCRIBED"))))))

(ert-deftest test-thread-sub-github-unsubscribe-discussion ()
  ;; GIVEN a discussion the user is subscribed to (id-query returns "D_abc123")
  ;; WHEN unsubscribing
  ;; THEN the updateSubscription mutation is called with the node ID
  ;;      propagated and state variable exactly "UNSUBSCRIBED".
  (test-thread-sub--with-mock-github
    (let ((mutation-vars nil)
          (mutation-called nil))
      (cl-letf (((symbol-function 'shipit--graphql-query)
                 (lambda (query vars)
                   (cond
                    ((string-match-p "updateSubscription" query)
                     (setq mutation-called t)
                     (setq mutation-vars vars)
                     '((updateSubscription
                        (subscribable
                         (viewerSubscription . "UNSUBSCRIBED")))))
                    (t
                     '((repository
                        (discussion
                         (viewerSubscription . "SUBSCRIBED")
                         (id . "D_abc123")))))))))
        (shipit-pr-github--set-thread-subscription
         '(:repo "owner/repo") "owner/repo" "discussion" 10 nil)
        (should mutation-called)
        (should (equal (cdr (assq 'id mutation-vars)) "D_abc123"))
        (should (equal (cdr (assq 'state mutation-vars)) "UNSUBSCRIBED"))))))

;;; Tests -- end-to-end dispatch via GitHub backend

(ert-deftest test-thread-sub-github-dispatch-get ()
  ;; GIVEN the GitHub backend is active and globally registered
  ;; WHEN calling the dispatch wrapper shipit--get-thread-subscription
  ;; THEN it resolves to the GitHub implementation.
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo"))
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _)
                 '((subscribed . t) (ignored . :json-false)))))
      (let ((result (shipit--get-thread-subscription "owner/repo" "pr" 42)))
        (should (equal result "subscribed"))))))

(ert-deftest test-thread-sub-github-dispatch-set ()
  ;; GIVEN the GitHub backend is active and globally registered
  ;; WHEN calling the dispatch wrapper shipit--set-thread-subscription
  ;; THEN it resolves to the GitHub implementation.
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo")
        (called nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (_endpoint _data &optional _method)
                 (setq called t)
                 '((subscribed . t)))))
      (shipit--set-thread-subscription "owner/repo" "pr" 42 t)
      (should called))))

;;; Tests -- transient thread group

(ert-deftest test-thread-sub-transient-pr-context-detected ()
  ;; GIVEN a PR buffer with shipit-buffer-pr-number and shipit-buffer-repo
  ;; WHEN shipit--current-buffer-thread-context is called
  ;; THEN returns plist with repo, type, number, subject.
  (with-temp-buffer
    (setq-local shipit-buffer-pr-number 42)
    (setq-local shipit-buffer-repo "owner/repo")
    (setq-local shipit-buffer-pr-data '((title . "Add feature X")))
    (let ((ctx (shipit--current-buffer-thread-context)))
      (should ctx)
      (should (equal (plist-get ctx :repo) "owner/repo"))
      (should (equal (plist-get ctx :type) "pr"))
      (should (equal (plist-get ctx :number) 42))
      (should (equal (plist-get ctx :subject) "Add feature X")))))

(ert-deftest test-thread-sub-transient-issue-context-detected ()
  ;; GIVEN an issue buffer with shipit-issue-buffer-number and repo
  ;; WHEN shipit--current-buffer-thread-context is called
  ;; THEN returns issue context.
  (with-temp-buffer
    (setq-local shipit-issue-buffer-number 8)
    (setq-local shipit-issue-buffer-repo "owner/repo")
    (setq-local shipit-issue-buffer-data '((title . "Bug report")))
    (let ((ctx (shipit--current-buffer-thread-context)))
      (should ctx)
      (should (equal (plist-get ctx :type) "issue"))
      (should (equal (plist-get ctx :number) 8)))))

(ert-deftest test-thread-sub-transient-discussion-context-detected ()
  ;; GIVEN a discussion buffer with shipit-discussion-buffer-number
  ;; WHEN shipit--current-buffer-thread-context is called
  ;; THEN returns discussion context.
  (with-temp-buffer
    (setq-local shipit-discussion-buffer-number 10)
    (setq-local shipit-discussion-buffer-repo "owner/repo")
    (setq-local shipit-discussion-buffer-data '((title . "RFC: new API")))
    (let ((ctx (shipit--current-buffer-thread-context)))
      (should ctx)
      (should (equal (plist-get ctx :type) "discussion"))
      (should (equal (plist-get ctx :number) 10)))))

(ert-deftest test-thread-sub-transient-no-context-in-repo-buffer ()
  ;; GIVEN a repo buffer with no thread-specific locals
  ;; WHEN shipit--current-buffer-thread-context is called
  ;; THEN returns nil.
  (with-temp-buffer
    (setq-local shipit-repo-buffer-repo "owner/repo")
    (should-not (shipit--current-buffer-thread-context))))

(ert-deftest test-thread-sub-transient-unsupported-backend ()
  ;; GIVEN a PR buffer with thread context
  ;; BUT the backend doesn't support :get-thread-subscription
  ;; WHEN shipit--thread-subscription-supported-p is called
  ;; THEN returns nil.
  (with-temp-buffer
    (setq-local shipit-buffer-pr-number 42)
    (setq-local shipit-buffer-repo "owner/repo")
    (let ((shipit-pr-backends nil)
          (shipit-pr-backend 'mock)
          (shipit-pr-backend-config nil)
          (shipit-current-repo "owner/repo"))
      (shipit-pr-register-backend
       'mock (test-thread-sub--make-minimal-plist))
      (should-not
       (shipit--thread-subscription-supported-p "owner/repo")))))

(ert-deftest test-thread-sub-populate-state-supported ()
  ;; GIVEN a PR buffer and a backend supporting thread subscriptions
  ;; WHEN populating thread transient state
  ;; THEN both ctx and state defvars are set.
  (with-temp-buffer
    (setq-local shipit-buffer-pr-number 42)
    (setq-local shipit-buffer-repo "owner/repo")
    (setq-local shipit-buffer-pr-data '((title . "Add feature X")))
    (let ((shipit--subscription-transient-thread-ctx nil)
          (shipit--subscription-transient-thread-state nil))
      (test-thread-sub--with-mock-backend
          (list :get-thread-subscription
                (lambda (_config _repo _type _number) "subscribed"))
        (shipit--populate-thread-transient-state "owner/repo")
        (should shipit--subscription-transient-thread-ctx)
        (should (equal shipit--subscription-transient-thread-state
                       "subscribed"))))))

(ert-deftest test-thread-sub-populate-state-unsupported ()
  ;; GIVEN a PR buffer but backend without thread subscription support
  ;; WHEN populating thread transient state
  ;; THEN both defvars are nil (thread group will be omitted).
  (with-temp-buffer
    (setq-local shipit-buffer-pr-number 42)
    (setq-local shipit-buffer-repo "owner/repo")
    (setq-local shipit-buffer-pr-data '((title . "Add feature X")))
    (let ((shipit--subscription-transient-thread-ctx 'old-value)
          (shipit--subscription-transient-thread-state 'old-value))
      (test-thread-sub--with-mock-backend nil
        (shipit--populate-thread-transient-state "owner/repo"))
      (should-not shipit--subscription-transient-thread-ctx)
      (should-not shipit--subscription-transient-thread-state))))

(provide 'test-thread-subscription)
;;; test-thread-subscription.el ends here
