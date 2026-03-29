;;; test-shipit-vendor-icons.el --- Tests for vendor icons in buffer headers -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for forge vendor icons in PR and issue buffer headers.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-buffer)
(require 'shipit-issues-buffer)
(require 'shipit-issue-create)
(require 'shipit-preview)
(require 'shipit-render)

;;; shipit--pr-type-label tests

(ert-deftest test-shipit-pr-type-label-github ()
  "GIVEN shipit-pr-backend is github
WHEN calling shipit--pr-type-label
THEN it returns \"Pull Request\"."
  (let ((shipit-pr-backend 'github))
    (should (string= "Pull Request" (shipit--pr-type-label)))))

(ert-deftest test-shipit-pr-type-label-gitlab ()
  "GIVEN shipit-pr-backend is gitlab
WHEN calling shipit--pr-type-label
THEN it returns \"Merge Request\"."
  (let ((shipit-pr-backend 'gitlab))
    (should (string= "Merge Request" (shipit--pr-type-label)))))

(ert-deftest test-shipit-pr-type-label-auto-defaults ()
  "GIVEN shipit-pr-backend is auto (default)
WHEN calling shipit--pr-type-label and auto resolves to github
THEN it returns \"Pull Request\"."
  (cl-letf (((symbol-function 'shipit--detect-backend-from-remote)
             (lambda () 'github)))
    (let ((shipit-pr-backend 'auto))
      (should (string= "Pull Request" (shipit--pr-type-label))))))

;;; PR header vendor icon tests

(ert-deftest test-shipit-pr-header-github-vendor-icon ()
  "GIVEN shipit-pr-backend is github
WHEN inserting a PR title section
THEN the heading contains \"GH\" text fallback and \"Pull Request\"."
  (let ((shipit-pr-backend 'github)
        (shipit-use-svglib-icons nil)
        (pr-data '((title . "Test PR"))))
    (with-temp-buffer
      (magit-insert-section (magit-status-mode)
        (shipit--insert-pr-title-section "owner/repo" pr-data 42))
      (goto-char (point-min))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "GH" text))
        (should (string-match-p "Pull Request" text))
        (should (string-match-p "#42" text))))))

(ert-deftest test-shipit-pr-header-gitlab-vendor-icon ()
  "GIVEN shipit-pr-backend is gitlab
WHEN inserting a PR title section
THEN the heading contains \"GL\" text fallback and \"Merge Request\"."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-use-svglib-icons nil)
        (pr-data '((title . "Test MR"))))
    (with-temp-buffer
      (magit-insert-section (magit-status-mode)
        (shipit--insert-pr-title-section "owner/repo" pr-data 99))
      (goto-char (point-min))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "GL" text))
        (should (string-match-p "Merge Request" text))
        (should (string-match-p "#99" text))))))

;;; Issue header vendor icon tests

(ert-deftest test-shipit-issue-header-gitlab-vendor-icon ()
  "GIVEN shipit-issue-backend is gitlab
WHEN inserting an issue header section
THEN the heading contains \"GL\" text fallback."
  (let ((shipit-issue-backend 'gitlab)
        (shipit-use-svglib-icons nil)
        (issue-data '((title . "Test Issue") (state . "open"))))
    (with-temp-buffer
      (magit-insert-section (magit-status-mode)
        (shipit-issue--insert-header-section "owner/repo" issue-data 7))
      (goto-char (point-min))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "GL" text))
        (should (string-match-p "#7" text))))))

(ert-deftest test-shipit-issue-header-github-vendor-icon ()
  "GIVEN shipit-issue-backend is github
WHEN inserting an issue header section
THEN the heading contains \"GH\" text fallback."
  (let ((shipit-issue-backend 'github)
        (shipit-use-svglib-icons nil)
        (issue-data '((title . "Bug report") (state . "closed"))))
    (with-temp-buffer
      (magit-insert-section (magit-status-mode)
        (shipit-issue--insert-header-section "owner/repo" issue-data 15))
      (goto-char (point-min))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "GH" text))
        (should (string-match-p "#15" text))))))

;;; Create PR (preview) header vendor icon tests

(ert-deftest test-shipit-preview-header-github-vendor-icon ()
  "GIVEN shipit-pr-backend is github
WHEN inserting the preview header banner
THEN the heading contains \"GH\" text fallback."
  (let ((shipit-pr-backend 'github)
        (shipit-use-svglib-icons nil)
        (shipit-preview--repo "owner/repo"))
    (with-temp-buffer
      (magit-insert-section (magit-status-mode)
        (shipit-preview--insert-header))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "GH" text))))))

(ert-deftest test-shipit-preview-header-gitlab-vendor-icon ()
  "GIVEN shipit-pr-backend is gitlab
WHEN inserting the preview header banner
THEN the heading contains \"GL\" text fallback."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-use-svglib-icons nil)
        (shipit-preview--repo "group/project"))
    (cl-letf (((symbol-function 'shipit-pr--get-backend)
               (lambda () '(:name "GitLab" :pr-type-short-label "MR"))))
      (with-temp-buffer
        (magit-insert-section (magit-status-mode)
          (shipit-preview--insert-header))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "GL" text))
          (should (string-match-p "MR" text)))))))

(ert-deftest test-shipit-preview-title-github-vendor-icon ()
  "GIVEN shipit-pr-backend is github
WHEN inserting the preview title section
THEN the heading contains \"GH\" text fallback and \"Pull Request\"."
  (let ((shipit-pr-backend 'github)
        (shipit-use-svglib-icons nil)
        (shipit-preview--title "My PR")
        (shipit-preview--branch "feature"))
    (cl-letf (((symbol-function 'shipit-pr--get-backend)
               (lambda () '(:name "GitHub"))))
      (with-temp-buffer
        (magit-insert-section (magit-status-mode)
          (shipit-preview--insert-title-section))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "GH" text))
          (should (string-match-p "Pull Request" text)))))))

(ert-deftest test-shipit-preview-title-gitlab-vendor-icon ()
  "GIVEN shipit-pr-backend is gitlab
WHEN inserting the preview title section
THEN the heading contains \"GL\" text fallback and \"Merge Request\"."
  (let ((shipit-pr-backend 'gitlab)
        (shipit-use-svglib-icons nil)
        (shipit-preview--title "My MR")
        (shipit-preview--branch "feature"))
    (cl-letf (((symbol-function 'shipit-pr--get-backend)
               (lambda () '(:name "GitLab" :pr-type-label "Merge Request"))))
      (with-temp-buffer
        (magit-insert-section (magit-status-mode)
          (shipit-preview--insert-title-section))
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "GL" text))
          (should (string-match-p "Merge Request" text)))))))

;;; Create issue header vendor icon tests

(ert-deftest test-shipit-issue-create-header-github-vendor-icon ()
  "GIVEN shipit-issue-create--backend-id is github
WHEN inserting the create issue header
THEN the heading contains \"GH\" text fallback and repo name."
  (let ((shipit-issue-create--backend-id 'github)
        (shipit-issue-create--repo "owner/repo")
        (shipit-use-svglib-icons nil))
    (with-temp-buffer
      (magit-insert-section (magit-status-mode)
        (shipit-issue-create--insert-header))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "GH" text))
        (should (string-match-p "NEW ISSUE" text))
        (should (string-match-p "owner/repo" text))))))

(ert-deftest test-shipit-issue-create-header-gitlab-vendor-icon ()
  "GIVEN shipit-issue-create--backend-id is gitlab
WHEN inserting the create issue header
THEN the heading contains \"GL\" text fallback and repo name."
  (let ((shipit-issue-create--backend-id 'gitlab)
        (shipit-issue-create--repo "group/project")
        (shipit-use-svglib-icons nil))
    (with-temp-buffer
      (magit-insert-section (magit-status-mode)
        (shipit-issue-create--insert-header))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "GL" text))
        (should (string-match-p "NEW ISSUE" text))
        (should (string-match-p "group/project" text))))))

(provide 'test-shipit-vendor-icons)
;;; test-shipit-vendor-icons.el ends here
