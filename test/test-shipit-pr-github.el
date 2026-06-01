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

(provide 'test-shipit-pr-github)
;;; test-shipit-pr-github.el ends here
