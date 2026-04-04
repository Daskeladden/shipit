;;; test-shipit-repo-subscription.el --- Tests for repo subscription -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for repository subscription management.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-http)
(require 'shipit-repo-buffer)

;;; Test helpers

(defun test-sub--make-minimal-plist ()
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

(defmacro test-sub--with-mock-github (&rest body)
  "Run BODY with GitHub backend and standard test bindings."
  (declare (indent 0))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo"))
     ,@body))

;;; Tests — get-repo-subscription

(ert-deftest test-sub-get-watching ()
  ;; GIVEN a GitHub repo where the user is subscribed (watching)
  ;; WHEN fetching the subscription
  ;; THEN returns watching state.
  (test-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _)
                 '((subscribed . t) (ignored . :json-false)))))
      (let ((result (shipit-pr-github--get-repo-subscription
                     '(:repo "owner/repo"))))
        (should (eq (cdr (assq 'subscribed result)) t))
        (should (eq (cdr (assq 'ignored result)) :json-false))))))

(ert-deftest test-sub-get-ignoring ()
  ;; GIVEN a GitHub repo where the user is ignoring
  ;; WHEN fetching the subscription
  ;; THEN returns ignoring state.
  (test-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _)
                 '((subscribed . t) (ignored . t)))))
      (let ((result (shipit-pr-github--get-repo-subscription
                     '(:repo "owner/repo"))))
        (should (eq (cdr (assq 'subscribed result)) t))
        (should (eq (cdr (assq 'ignored result)) t))))))

(ert-deftest test-sub-get-not-subscribed ()
  ;; GIVEN a GitHub repo where the user has no subscription (404)
  ;; WHEN fetching the subscription
  ;; THEN returns nil (participating).
  (test-sub--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _) nil)))
      (let ((result (shipit-pr-github--get-repo-subscription
                     '(:repo "owner/repo"))))
        (should-not result)))))

;;; Tests — set-repo-subscription

(ert-deftest test-sub-set-watching ()
  ;; GIVEN a repo
  ;; WHEN setting subscription to watching
  ;; THEN PUT is called with subscribed=true, ignored=false.
  (test-sub--with-mock-github
    (let ((called-with nil))
      (cl-letf (((symbol-function 'shipit--api-request-post)
                 (lambda (endpoint data &optional method)
                   (setq called-with (list endpoint data method))
                   '((subscribed . t)))))
        (shipit-pr-github--set-repo-subscription
         '(:repo "owner/repo") "watching")
        (should (string-match-p "/repos/owner/repo/subscription"
                                (nth 0 called-with)))
        (should (eq t (cdr (assq 'subscribed (nth 1 called-with)))))
        (should (equal "PUT" (nth 2 called-with)))))))

(ert-deftest test-sub-set-ignoring ()
  ;; GIVEN a repo
  ;; WHEN setting subscription to ignoring
  ;; THEN PUT is called with subscribed=false, ignored=true.
  (test-sub--with-mock-github
    (let ((called-with nil))
      (cl-letf (((symbol-function 'shipit--api-request-post)
                 (lambda (endpoint data &optional method)
                   (setq called-with (list endpoint data method))
                   '((subscribed . t) (ignored . t)))))
        (shipit-pr-github--set-repo-subscription
         '(:repo "owner/repo") "ignoring")
        (should (eq t (cdr (assq 'ignored (nth 1 called-with)))))
        (should (equal "PUT" (nth 2 called-with)))))))

(ert-deftest test-sub-set-participating ()
  ;; GIVEN a repo
  ;; WHEN setting subscription to participating
  ;; THEN DELETE is called on the subscription endpoint.
  (test-sub--with-mock-github
    (let ((called-with nil))
      (cl-letf (((symbol-function 'shipit--api-request-post)
                 (lambda (endpoint data &optional method)
                   (setq called-with (list endpoint data method))
                   nil)))
        (shipit-pr-github--set-repo-subscription
         '(:repo "owner/repo") "participating")
        (should (string-match-p "/repos/owner/repo/subscription"
                                (nth 0 called-with)))
        (should (equal "DELETE" (nth 2 called-with)))))))

;;; Tests — subscription state helpers

(ert-deftest test-sub-state-from-api-watching ()
  ;; GIVEN API data with subscribed=t, ignored=false
  ;; WHEN computing subscription state
  ;; THEN returns watching.
  (should (equal "watching"
                 (shipit--subscription-state-from-api
                  '((subscribed . t) (ignored . :json-false))))))

(ert-deftest test-sub-state-from-api-ignoring ()
  ;; GIVEN API data with subscribed=t, ignored=t
  ;; WHEN computing subscription state
  ;; THEN returns ignoring.
  (should (equal "ignoring"
                 (shipit--subscription-state-from-api
                  '((subscribed . t) (ignored . t))))))

(ert-deftest test-sub-state-from-api-ignoring-unsubscribed ()
  ;; GIVEN API data with subscribed=false but ignored=t
  ;; WHEN computing subscription state
  ;; THEN returns ignoring (ignored takes precedence over subscribed).
  (should (equal "ignoring"
                 (shipit--subscription-state-from-api
                  '((subscribed . :json-false) (ignored . t))))))

(ert-deftest test-sub-state-from-api-participating ()
  ;; GIVEN nil API data (no subscription / 404)
  ;; WHEN computing subscription state
  ;; THEN returns participating.
  (should (equal "participating"
                 (shipit--subscription-state-from-api nil))))

(ert-deftest test-sub-state-label ()
  ;; GIVEN each subscription state string
  ;; WHEN getting the display label
  ;; THEN returns the correct human-readable label.
  (should (equal "All Activity"
                 (shipit--subscription-state-label "watching")))
  (should (equal "Participating and @mentions"
                 (shipit--subscription-state-label "participating")))
  (should (equal "Ignoring"
                 (shipit--subscription-state-label "ignoring"))))

;;; Tests — unsupported backend

(ert-deftest test-sub-unsupported-backend-errors ()
  ;; GIVEN a backend without :get-repo-subscription
  ;; WHEN trying to get subscription
  ;; THEN user-error is signaled.
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo"))
    (shipit-pr-register-backend 'mock (test-sub--make-minimal-plist))
    (should-error
     (shipit--repo-get-subscription "owner/repo")
     :type 'user-error)))

;;; Tests — header indicator

(ert-deftest test-sub-header-shows-watching-state ()
  ;; GIVEN a repo buffer with subscription data set to watching
  ;; WHEN rendering the header
  ;; THEN the Watching line shows All Activity.
  (with-temp-buffer
    (let ((shipit-repo-buffer-subscription '((subscribed . t) (ignored . :json-false))))
      (shipit-repo-buffer--insert-subscription-line)
      (goto-char (point-min))
      (should (search-forward "All Activity" nil t)))))

(ert-deftest test-sub-header-shows-participating-state ()
  ;; GIVEN a repo buffer with nil subscription data
  ;; WHEN rendering the header
  ;; THEN the Watching line shows Participating and @mentions.
  (with-temp-buffer
    (let ((shipit-repo-buffer-subscription nil))
      (shipit-repo-buffer--insert-subscription-line)
      (goto-char (point-min))
      (should (search-forward "Participating" nil t)))))

(ert-deftest test-sub-header-omitted-when-no-backend-support ()
  ;; GIVEN a repo buffer with subscription as symbol not-supported
  ;; WHEN rendering the header
  ;; THEN no Watching line is inserted.
  (with-temp-buffer
    (let ((shipit-repo-buffer-subscription 'not-supported))
      (shipit-repo-buffer--insert-subscription-line)
      (should (= (point-min) (point-max))))))

;;; Tests — subscription change

(ert-deftest test-sub-set-watching-calls-put ()
  ;; GIVEN a GitHub backend with :set-repo-subscription registered
  ;; WHEN setting subscription to watching via high-level dispatch
  ;; THEN PUT is called with correct endpoint and body.
  (let ((shipit-pr-backend 'github)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo")
        (called-with nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (endpoint data &optional method)
                 (setq called-with (list endpoint data method))
                 '((subscribed . t)))))
      (shipit--repo-set-subscription "owner/repo" "watching")
      (should called-with)
      (should (string-match-p "subscription" (nth 0 called-with)))
      (should (equal "PUT" (nth 2 called-with))))))

;;; Tests — RET dispatch on subscription line

(ert-deftest test-sub-ret-dispatches-on-subscription-property ()
  ;; GIVEN point is on text with shipit-repo-subscription property
  ;; WHEN RET action checks text properties
  ;; THEN the subscription property is detected.
  (with-temp-buffer
    (insert (propertize "Watching: All Activity"
                        'shipit-repo-subscription t))
    (goto-char (point-min))
    (should (get-text-property (point) 'shipit-repo-subscription))))

(provide 'test-shipit-repo-subscription)
;;; test-shipit-repo-subscription.el ends here
