;;; test-shipit-merge.el --- Tests for PR merge menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for merge action menu.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-http)
(require 'shipit-pr-actions)

;;; Test helpers

(defun test-merge--make-minimal-plist ()
  "Return a minimal valid PR backend plist with all required keys set to `ignore'."
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

(defmacro test-merge--with-mock-github (&rest body)
  "Run BODY with a mock GitHub-like environment."
  (declare (indent 0))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo")
         (shipit--merge-methods-cache (make-hash-table :test 'equal)))
     ,@body))

;;; Tests — shipit-pr-github--fetch-merge-methods

(ert-deftest test-merge-fetch-methods-all-enabled ()
  "GIVEN GitHub API returns all three merge method booleans as true
WHEN calling shipit-pr-github--fetch-merge-methods
THEN all three method strings are returned."
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint)
                 '((allow_merge_commit . t)
                   (allow_squash_merge . t)
                   (allow_rebase_merge . t)))))
      (let* ((config (list :repo "owner/repo"))
             (methods (shipit-pr-github--fetch-merge-methods config)))
        (should (member "merge" methods))
        (should (member "squash" methods))
        (should (member "rebase" methods))
        (should (= 3 (length methods)))))))

(ert-deftest test-merge-fetch-methods-squash-only ()
  "GIVEN GitHub API returns only squash enabled (others are :json-false)
WHEN calling shipit-pr-github--fetch-merge-methods
THEN only \"squash\" is returned."
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint)
                 '((allow_merge_commit . :json-false)
                   (allow_squash_merge . t)
                   (allow_rebase_merge . :json-false)))))
      (let* ((config (list :repo "owner/repo"))
             (methods (shipit-pr-github--fetch-merge-methods config)))
        (should (equal '("squash") methods))))))

(ert-deftest test-merge-fetch-methods-cached ()
  "GIVEN the cache already has methods for the repo
WHEN calling shipit-pr-github--fetch-merge-methods
THEN no API call is made and the cached value is returned."
  (test-merge--with-mock-github
    (puthash "owner/repo" '("merge") shipit--merge-methods-cache)
    (let ((api-called nil))
      (cl-letf (((symbol-function 'shipit--api-request)
                 (lambda (_endpoint)
                   (setq api-called t)
                   '((allow_merge_commit . t)))))
        (let* ((config (list :repo "owner/repo"))
               (methods (shipit-pr-github--fetch-merge-methods config)))
          (should (equal '("merge") methods))
          (should (not api-called)))))))

(ert-deftest test-merge-fetch-methods-nil-field-treated-as-enabled ()
  "GIVEN GitHub API returns nil (missing) for all method fields
WHEN calling shipit-pr-github--fetch-merge-methods
THEN all three methods are returned (nil is not :json-false)"
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint)
                 '())))
      (let* ((config (list :repo "owner/repo"))
             (methods (shipit-pr-github--fetch-merge-methods config)))
        (should (member "merge" methods))
        (should (member "squash" methods))
        (should (member "rebase" methods))
        (should (= 3 (length methods)))))))

;;; Tests — shipit--merge-guard

(ert-deftest test-merge-guard-rejects-merged-pr ()
  "GIVEN a PR with state=closed and merged_at set (merged PR)
WHEN calling shipit--merge-guard
THEN a user-error is signaled indicating the PR is already merged."
  (let ((pr-data '((state . "closed")
                   (merged_at . "2025-01-01T00:00:00Z")
                   (draft . :json-false))))
    (should-error (shipit--merge-guard pr-data) :type 'user-error)))

(ert-deftest test-merge-guard-rejects-closed-pr ()
  "GIVEN a PR with state=closed and no merged_at (closed without merge)
WHEN calling shipit--merge-guard
THEN a user-error is signaled indicating the PR is closed."
  (let ((pr-data '((state . "closed")
                   (draft . :json-false))))
    (should-error (shipit--merge-guard pr-data) :type 'user-error)))

(ert-deftest test-merge-guard-rejects-draft-pr ()
  "GIVEN a PR with state=open and draft=t (GitHub returns open for drafts)
WHEN calling shipit--merge-guard
THEN a user-error is signaled indicating the PR is a draft."
  (let ((pr-data '((state . "open")
                   (draft . t))))
    (should-error (shipit--merge-guard pr-data) :type 'user-error)))

(ert-deftest test-merge-guard-accepts-open-pr ()
  "GIVEN a PR with state=open and draft=:json-false (ready for review)
WHEN calling shipit--merge-guard
THEN no error is signaled and the function returns nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false))))
    (should (eq nil (shipit--merge-guard pr-data)))))

;;; Tests — shipit--merge-pr-execute and shipit--merge-get-methods

(ert-deftest test-merge-dispatch-calls-backend ()
  "GIVEN a backend registered with :merge-pr that records its args
WHEN calling shipit--merge-pr-execute with repo, number, and method
THEN the backend :merge-pr is called with the correct args
     AND shipit-buffer-refresh is called."
  (let* ((recorded-args nil)
         (refresh-called nil)
         (mock-backend (append (list :merge-pr
                                     (lambda (config number method)
                                       (setq recorded-args
                                             (list config number method))))
                               (test-merge--make-minimal-plist)))
         (mock-config (list :repo "owner/repo")))
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons mock-backend mock-config)))
              ((symbol-function 'shipit-buffer-refresh)
               (lambda ()
                 (setq refresh-called t))))
      (shipit--merge-pr-execute "owner/repo" 42 "squash")
      (should (equal (nth 0 recorded-args) mock-config))
      (should (equal (nth 1 recorded-args) 42))
      (should (equal (nth 2 recorded-args) "squash"))
      (should refresh-called))))

(ert-deftest test-merge-unsupported-backend-errors ()
  "GIVEN a backend registered without :fetch-merge-methods
WHEN calling shipit--merge-get-methods
THEN a user-error is signaled about unsupported backend."
  (let* ((mock-backend (test-merge--make-minimal-plist))
         (mock-config (list :repo "owner/repo")))
    ;; Remove :fetch-merge-methods key to simulate unsupported backend
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons mock-backend mock-config))))
      (should-error (shipit--merge-get-methods "owner/repo")
                    :type 'user-error))))

;;; Tests — DWIM integration

(ert-deftest test-merge-dwim-includes-merge-for-open-pr ()
  "GIVEN an open non-draft PR in the header DWIM context
WHEN building the action list
THEN Merge is among the actions."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo"))
    (shipit-pr-register-backend 'mock (test-merge--make-minimal-plist))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_number &optional _repo)
                 '((number . 42)
                   (state . "open")
                   (draft . :json-false)
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (user . ((login . "other-user"))))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "test-user"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "Merge" collection))
                 "Merge"))
              ((symbol-function 'shipit-merge) #'ignore))
      (with-temp-buffer
        (insert (propertize "PR #42" 'shipit-pr-number 42 'shipit-repo "owner/repo"))
        (goto-char (point-min))
        (shipit--pr-header-actions)))))

(ert-deftest test-merge-dwim-excludes-merge-for-closed-pr ()
  "GIVEN a closed PR in the header DWIM context
WHEN building the action list
THEN Merge is not among the actions."
  (let ((shipit-pr-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (shipit-github-token "test-token")
        (shipit-current-repo "owner/repo"))
    (shipit-pr-register-backend 'mock (test-merge--make-minimal-plist))
    (cl-letf (((symbol-function 'shipit-get-pull-request)
               (lambda (_number &optional _repo)
                 '((number . 42)
                   (state . "closed")
                   (merged_at . "2026-01-01T00:00:00Z")
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (user . ((login . "other-user"))))))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "test-user"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should-not (member "Merge" collection))
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (with-temp-buffer
        (insert (propertize "PR #42" 'shipit-pr-number 42 'shipit-repo "owner/repo"))
        (goto-char (point-min))
        (shipit--pr-header-actions)))))

;;; Tests — merge readiness gating

(ert-deftest test-merge-suffixes-hidden-when-blocked ()
  "GIVEN a PR with mergeable_state=blocked
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable_state . "blocked")
                   (mergeable . :json-false))))
    (should-not (shipit--merge-ready-p pr-data))))

(ert-deftest test-merge-suffixes-shown-when-clean ()
  "GIVEN a PR with mergeable_state=clean
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns non-nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable_state . "clean")
                   (mergeable . t))))
    (should (shipit--merge-ready-p pr-data))))

(ert-deftest test-merge-suffixes-shown-when-mergeable-fallback ()
  "GIVEN a PR with no mergeable_state but mergeable=t
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns non-nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable . t))))
    (should (shipit--merge-ready-p pr-data))))

(ert-deftest test-merge-ready-but-no-methods ()
  "GIVEN a PR with mergeable_state=clean but all merge methods disabled
WHEN checking merge readiness
THEN shipit--merge-ready-p returns non-nil but fetch-merge-methods returns empty."
  (test-merge--with-mock-github
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint &rest _)
                 '((allow_merge_commit . :json-false)
                   (allow_squash_merge . :json-false)
                   (allow_rebase_merge . :json-false)))))
      (let ((pr-data '((state . "open")
                       (draft . :json-false)
                       (mergeable_state . "clean")
                       (mergeable . t))))
        (should (shipit--merge-ready-p pr-data))
        (should-not (shipit-pr-github--fetch-merge-methods
                     '(:repo "owner/repo")))))))

(ert-deftest test-merge-suffixes-hidden-when-dirty ()
  "GIVEN a PR with mergeable_state=dirty (merge conflict)
WHEN checking if merge is ready
THEN shipit--merge-ready-p returns nil."
  (let ((pr-data '((state . "open")
                   (draft . :json-false)
                   (mergeable_state . "dirty")
                   (mergeable . :json-false))))
    (should-not (shipit--merge-ready-p pr-data))))

;;; Tests — cache invalidation

(ert-deftest test-merge-hard-refresh-clears-cache ()
  "GIVEN cached merge methods for a repo
WHEN shipit-buffer-refresh-hard runs
THEN the cache entry for the buffer's repo is removed."
  (require 'shipit-buffer)
  (let ((shipit--merge-methods-cache (make-hash-table :test 'equal))
        (shipit-buffer-repo "owner/repo")
        (shipit-buffer-pr-number 42)
        (shipit-buffer-pr-data '((number . 42))))
    (puthash "owner/repo" '("merge" "squash") shipit--merge-methods-cache)
    (cl-letf (((symbol-function 'shipit-clear-all-caches) #'ignore)
              ((symbol-function 'shipit-gh-etag-clear-all) #'ignore)
              ((symbol-function 'shipit-buffer-refresh) #'ignore)
              ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
      (shipit-buffer-refresh-hard))
    (should-not (gethash "owner/repo" shipit--merge-methods-cache))))

(provide 'test-shipit-merge)
;;; test-shipit-merge.el ends here
