;;; test-shipit-pr-linked-issue.el --- Tests for PR → issue linking -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for auto-detection of linked issues from PR branch names
;; and for local-override persistence.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-jira)
(require 'shipit-pr-linked-issue)

;;; Branch-name extraction

(ert-deftest test-shipit-pr-linked-issue-extract-jira-key-from-branch ()
  "GIVEN a Jira backend with project key ZIVID
WHEN scanning a branch named ZIVID-12624-fix-bug
THEN the extracted key is ZIVID-12624."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("ZIVID"))))
    (should (equal "ZIVID-12624"
                   (shipit-pr-linked-issue--extract-from-text
                    "ZIVID-12624-fix-bug" "foo/bar")))))

(ert-deftest test-shipit-pr-linked-issue-extract-jira-key-with-prefix ()
  "GIVEN a Jira backend
WHEN scanning a branch named feature/ZIVID-999-xy
THEN the key is found despite the directory-style prefix."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("ZIVID"))))
    (should (equal "ZIVID-999"
                   (shipit-pr-linked-issue--extract-from-text
                    "feature/ZIVID-999-xy" "foo/bar")))))

(ert-deftest test-shipit-pr-linked-issue-extract-no-match ()
  "GIVEN a Jira backend
WHEN scanning a branch with no matching key
THEN returns nil."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("ZIVID"))))
    (should (null (shipit-pr-linked-issue--extract-from-text
                   "main" "foo/bar")))))

(ert-deftest test-shipit-pr-linked-issue-extract-wrong-project ()
  "GIVEN a Jira backend that knows only ZIVID
WHEN scanning a branch that references a different project
THEN returns nil."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("ZIVID"))))
    (should (null (shipit-pr-linked-issue--extract-from-text
                   "OTHER-123-foo" "foo/bar")))))

;;; Override persistence

(ert-deftest test-shipit-pr-linked-issue-override-roundtrip ()
  "GIVEN a clean override store
WHEN setting then reading an override
THEN the same key is returned."
  (let* ((tmp (make-temp-file "shipit-link-" nil ".el"))
         (shipit-pr-linked-issue-file tmp)
         (shipit-pr-linked-issue--overrides nil)
         (shipit-pr-linked-issue--loaded nil))
    (unwind-protect
        (progn
          (shipit-pr-linked-issue--set-override "foo/bar" 42 "ZIVID-999")
          ;; Force reload from disk
          (setq shipit-pr-linked-issue--loaded nil
                shipit-pr-linked-issue--overrides nil)
          (should (equal "ZIVID-999"
                         (shipit-pr-linked-issue--get-override "foo/bar" 42))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest test-shipit-pr-linked-issue-override-clear ()
  "GIVEN an existing override
WHEN clearing it with a nil value
THEN get-override returns nil."
  (let* ((tmp (make-temp-file "shipit-link-" nil ".el"))
         (shipit-pr-linked-issue-file tmp)
         (shipit-pr-linked-issue--overrides nil)
         (shipit-pr-linked-issue--loaded nil))
    (unwind-protect
        (progn
          (shipit-pr-linked-issue--set-override "foo/bar" 42 "ZIVID-999")
          (shipit-pr-linked-issue--set-override "foo/bar" 42 nil)
          (setq shipit-pr-linked-issue--loaded nil
                shipit-pr-linked-issue--overrides nil)
          (should (null (shipit-pr-linked-issue--get-override "foo/bar" 42))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest test-shipit-pr-linked-issue-override-beats-branch ()
  "GIVEN a manual override that differs from the branch-derived key
WHEN detecting
THEN the override wins."
  (let* ((tmp (make-temp-file "shipit-link-" nil ".el"))
         (shipit-pr-linked-issue-file tmp)
         (shipit-pr-linked-issue--overrides nil)
         (shipit-pr-linked-issue--loaded nil)
         (shipit-issue-backend 'jira)
         (shipit-issue-backend-config
          '(:base-url "https://x.atlassian.net" :project-keys ("ZIVID")))
         (pr-data '((head . ((ref . "ZIVID-1-auto"))))))
    (unwind-protect
        (progn
          (shipit-pr-linked-issue--set-override "foo/bar" 42 "ZIVID-999")
          (should (equal "ZIVID-999"
                         (shipit-pr-linked-issue--detect
                          "foo/bar" 42 pr-data))))
      (ignore-errors (delete-file tmp)))))

(provide 'test-shipit-pr-linked-issue)
;;; test-shipit-pr-linked-issue.el ends here
