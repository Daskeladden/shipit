;;; test-shipit-pr-github.el --- Tests for GitHub PR backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the GitHub PR backend create-pr payload, with focus on
;; the draft flag being threaded into the REST API request body.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)

(defun test-github--create-pr-payload (draft)
  "Call create-pr with DRAFT and return the captured POST data alist."
  (let ((captured nil))
    (cl-letf (((symbol-function 'shipit--api-request-post)
               (lambda (_endpoint data &optional _method)
                 (setq captured data)
                 '((number . 1) (html_url . "https://example/1")))))
      (shipit-pr-github--create-pr '(:repo "owner/repo")
                                   "Add feature" "Body" "main" "feature" draft)
      captured)))

(ert-deftest test-shipit-pr-github--create-pr-draft-true ()
  "GIVEN draft is non-nil
WHEN creating a GitHub PR
THEN the POST payload sets the draft field to t (true)."
  ;; WHEN
  (let ((data (test-github--create-pr-payload t)))
    ;; THEN
    (should (eq t (cdr (assq 'draft data))))))

(ert-deftest test-shipit-pr-github--create-pr-draft-false ()
  "GIVEN draft is nil
WHEN creating a GitHub PR
THEN the POST payload sets the draft field to :json-false (ready for review)."
  ;; WHEN
  (let ((data (test-github--create-pr-payload nil)))
    ;; THEN
    (should (eq :json-false (cdr (assq 'draft data))))))

(ert-deftest test-shipit-pr-github--create-pr-keeps-core-fields ()
  "GIVEN a draft PR request
WHEN creating a GitHub PR
THEN title, body, base and head are still present in the payload."
  ;; WHEN
  (let ((data (test-github--create-pr-payload t)))
    ;; THEN
    (should (equal "Add feature" (cdr (assq 'title data))))
    (should (equal "Body" (cdr (assq 'body data))))
    (should (equal "main" (cdr (assq 'base data))))
    (should (equal "feature" (cdr (assq 'head data))))))

;;; Draft state toggling (GraphQL-only on GitHub)

(ert-deftest test-shipit-pr-github-set-pr-draft-ready-uses-graphql-mutation ()
  "GIVEN a draft PR whose GraphQL node id resolves
WHEN marking it ready for review through the backend op
THEN the markPullRequestReadyForReview mutation is sent with that id."
  (let (captured-query captured-vars)
    (cl-letf (((symbol-function 'shipit--get-pr-id)
               (lambda (_repo _number) "PR_node123"))
              ((symbol-function 'shipit--graphql-request)
               (lambda (query &optional variables _callback)
                 (setq captured-query query
                       captured-vars variables)
                 '((data (markPullRequestReadyForReview
                          (pullRequest (isDraft . :json-false))))))))
      (should (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 nil))
      (should (string-match-p "markPullRequestReadyForReview" captured-query))
      (should (equal '((id . "PR_node123")) captured-vars)))))

(ert-deftest test-shipit-pr-github-set-pr-draft-convert-uses-graphql-mutation ()
  "GIVEN an open non-draft PR whose GraphQL node id resolves
WHEN converting it to draft through the backend op
THEN the convertPullRequestToDraft mutation is sent with that id."
  (let (captured-query)
    (cl-letf (((symbol-function 'shipit--get-pr-id)
               (lambda (_repo _number) "PR_node123"))
              ((symbol-function 'shipit--graphql-request)
               (lambda (query &optional _variables _callback)
                 (setq captured-query query)
                 '((data (convertPullRequestToDraft
                          (pullRequest (isDraft . t))))))))
      (should (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 t))
      (should (string-match-p "convertPullRequestToDraft" captured-query)))))

(ert-deftest test-shipit-pr-github-set-pr-draft-fails-on-graphql-errors ()
  "GIVEN the GraphQL mutation answers with an errors array
WHEN toggling draft state through the backend op
THEN the op returns nil so callers can report the failure."
  (cl-letf (((symbol-function 'shipit--get-pr-id)
             (lambda (_repo _number) "PR_node123"))
            ((symbol-function 'shipit--graphql-request)
             (lambda (&rest _)
               '((data . nil)
                 (errors . (((message . "boom")))))))
            ((symbol-function 'message) #'ignore))
    (should-not (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 nil))))

(ert-deftest test-shipit-pr-github-set-pr-draft-fails-without-node-id ()
  "GIVEN the PR node id cannot be resolved
WHEN toggling draft state through the backend op
THEN the op returns nil and no mutation is attempted."
  (let (mutation-called)
    (cl-letf (((symbol-function 'shipit--get-pr-id)
               (lambda (_repo _number) nil))
              ((symbol-function 'shipit--graphql-request)
               (lambda (&rest _) (setq mutation-called t) nil)))
      (should-not (shipit-pr-github--set-pr-draft '(:repo "owner/repo") 42 nil))
      (should-not mutation-called))))

(ert-deftest test-shipit-pr-github-registers-set-pr-draft ()
  "GIVEN the GitHub backend is loaded
WHEN inspecting its registration
THEN the :set-pr-draft operation is present."
  (should (functionp (plist-get (cdr (assq 'github shipit-pr-backends))
                                :set-pr-draft))))

(provide 'test-shipit-pr-github)
;;; test-shipit-pr-github.el ends here
