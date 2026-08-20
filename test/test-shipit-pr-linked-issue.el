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
  "GIVEN a Jira backend with project key PRJ
WHEN scanning a branch named PRJ-12624-fix-bug
THEN the extracted key is PRJ-12624."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("PRJ"))))
    (should (equal "PRJ-12624"
                   (shipit-pr-linked-issue--extract-from-text
                    "PRJ-12624-fix-bug" "foo/bar")))))

(ert-deftest test-shipit-pr-linked-issue-extract-jira-key-with-prefix ()
  "GIVEN a Jira backend
WHEN scanning a branch named feature/PRJ-999-xy
THEN the key is found despite the directory-style prefix."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("PRJ"))))
    (should (equal "PRJ-999"
                   (shipit-pr-linked-issue--extract-from-text
                    "feature/PRJ-999-xy" "foo/bar")))))

(ert-deftest test-shipit-pr-linked-issue-extract-no-match ()
  "GIVEN a Jira backend
WHEN scanning a branch with no matching key
THEN returns nil."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("PRJ"))))
    (should (null (shipit-pr-linked-issue--extract-from-text
                   "main" "foo/bar")))))

(ert-deftest test-shipit-pr-linked-issue-extract-wrong-project ()
  "GIVEN a Jira backend that knows only PRJ
WHEN scanning a branch that references a different project
THEN returns nil."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config
         '(:base-url "https://x.atlassian.net" :project-keys ("PRJ"))))
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
          (shipit-pr-linked-issue--set-override "foo/bar" 42 "PRJ-999")
          ;; Force reload from disk
          (setq shipit-pr-linked-issue--loaded nil
                shipit-pr-linked-issue--overrides nil)
          (should (equal "PRJ-999"
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
          (shipit-pr-linked-issue--set-override "foo/bar" 42 "PRJ-999")
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
          '(:base-url "https://x.atlassian.net" :project-keys ("PRJ")))
         (pr-data '((head . ((ref . "PRJ-1-auto"))))))
    (unwind-protect
        (progn
          (shipit-pr-linked-issue--set-override "foo/bar" 42 "PRJ-999")
          (should (equal "PRJ-999"
                         (shipit-pr-linked-issue--detect
                          "foo/bar" 42 pr-data))))
      (ignore-errors (delete-file tmp)))))

;;; Linking a PR in the tracker

(ert-deftest test-shipit-pr-linked-issue-link-records-locally-and-in-tracker ()
  "GIVEN an issue backend that can attach remote links
WHEN linking a PR to an issue
THEN the local override is set and the tracker gets the PR's URL and
a title naming the PR."
  (let* ((tmp (make-temp-file "shipit-linked-"))
         (shipit-pr-linked-issue-file tmp)
         (shipit-pr-linked-issue--overrides nil)
         (shipit-pr-linked-issue--loaded nil)
         (remote nil))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
                   (lambda (_repo)
                     (cons (list :link-remote-url
                                 (lambda (_config key url title)
                                   (setq remote (list key url title))))
                           '(:base-url "https://j")))))
          (shipit-pr-linked-issue-link
           "foo/bar" "PRJ-9" "foo/bar" 42
           "https://github.com/foo/bar/pull/42" "Fix sensor")
          (should (equal "PRJ-9" (shipit-pr-linked-issue--get-override "foo/bar" 42)))
          (should (equal remote '("PRJ-9" "https://github.com/foo/bar/pull/42"
                                  "#42: Fix sensor"))))
      (delete-file tmp))))

(ert-deftest test-shipit-pr-linked-issue-link-without-tracker-support ()
  "GIVEN an issue backend with no remote link op
WHEN linking a PR to an issue
THEN the local override is still set and nothing errors."
  (let* ((tmp (make-temp-file "shipit-linked-"))
         (shipit-pr-linked-issue-file tmp)
         (shipit-pr-linked-issue--overrides nil)
         (shipit-pr-linked-issue--loaded nil))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
                   (lambda (_repo) (cons (list :name "Plain") nil))))
          (shipit-pr-linked-issue-link "foo/bar" "PRJ-9" "foo/bar" 42
                                       "https://github.com/foo/bar/pull/42" nil)
          (should (equal "PRJ-9" (shipit-pr-linked-issue--get-override "foo/bar" 42))))
      (delete-file tmp))))

(ert-deftest test-shipit-pr-linked-issue-search-links-in-tracker-too ()
  "GIVEN the PR buffer's Linked Issue search picks an issue
WHEN it links
THEN it goes through the shared link helper with the PR's URL, so the
tracker learns of it as well as the local file."
  (let ((linked nil))
    (cl-letf (((symbol-function 'shipit-pr-linked-issue--context)
               (lambda () '(:repo "foo/bar" :pr-number 42)))
              ((symbol-function 'shipit-pr-linked-issue--read-issue-key)
               (lambda (_repo) "PRJ-9"))
              ((symbol-function 'shipit-pr-linked-issue--pr-url)
               (lambda (_repo _n) "https://github.com/foo/bar/pull/42"))
              ((symbol-function 'shipit-pr-linked-issue-link)
               (lambda (&rest args) (setq linked args)))
              ((symbol-function 'shipit-pr-linked-issue--invalidate-cache) #'ignore)
              ((symbol-function 'shipit-pr-linked-issue--refresh-buffer) #'ignore)
              ((symbol-function 'message) #'ignore))
      (shipit-pr-linked-issue-search)
      (should (equal (seq-take linked 5)
                     '("foo/bar" "PRJ-9" "foo/bar" 42
                       "https://github.com/foo/bar/pull/42"))))))

(ert-deftest test-shipit-pr-linked-issue-unlink-clears-locally-and-in-tracker ()
  "GIVEN a PR linked to an issue, locally and in the tracker
WHEN unlinking it
THEN the local override goes and the tracker is told to detach the
PR's URL."
  (let* ((tmp (make-temp-file "shipit-linked-"))
         (shipit-pr-linked-issue-file tmp)
         (shipit-pr-linked-issue--overrides nil)
         (shipit-pr-linked-issue--loaded nil)
         (detached nil))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
                   (lambda (_repo)
                     (cons (list :unlink-remote-url
                                 (lambda (_config key url) (setq detached (list key url))))
                           '(:base-url "https://j")))))
          (shipit-pr-linked-issue--set-override "foo/bar" 42 "PRJ-9")
          (shipit-pr-linked-issue-unlink "foo/bar" "PRJ-9" "foo/bar" 42
                                         "https://github.com/foo/bar/pull/42")
          (should-not (shipit-pr-linked-issue--get-override "foo/bar" 42))
          (should (equal detached '("PRJ-9" "https://github.com/foo/bar/pull/42"))))
      (delete-file tmp))))

(ert-deftest test-shipit-pr-linked-issue-clear-unlinks-in-tracker-too ()
  "GIVEN the PR buffer's Linked Issue has a manual link
WHEN clearing it
THEN the shared unlink helper runs with the linked key and the PR's URL."
  (let ((unlinked nil))
    (cl-letf (((symbol-function 'shipit-pr-linked-issue--context)
               (lambda () '(:repo "foo/bar" :pr-number 42)))
              ((symbol-function 'shipit-pr-linked-issue--get-override)
               (lambda (_repo _n) "PRJ-9"))
              ((symbol-function 'shipit-pr-linked-issue--pr-url)
               (lambda (_repo _n) "https://github.com/foo/bar/pull/42"))
              ((symbol-function 'shipit-pr-linked-issue-unlink)
               (lambda (&rest args) (setq unlinked args)))
              ((symbol-function 'shipit-pr-linked-issue--refresh-buffer) #'ignore)
              ((symbol-function 'message) #'ignore))
      (shipit-pr-linked-issue-clear)
      (should (equal unlinked '("foo/bar" "PRJ-9" "foo/bar" 42
                                "https://github.com/foo/bar/pull/42"))))))

(provide 'test-shipit-pr-linked-issue)
;;; test-shipit-pr-linked-issue.el ends here
