;;; test-thread-subscription.el --- Tests for thread subscription -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for per-thread subscription management.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-http)

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

(provide 'test-thread-subscription)
;;; test-thread-subscription.el ends here
