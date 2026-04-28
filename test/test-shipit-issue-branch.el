;;; test-shipit-issue-branch.el --- Tests for shipit-issue-branch -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for issue-buffer branch creation feature.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-issue-branch)

;;; Slugifier

(ert-deftest shipit-issue-branch-test-slugify-basic ()
  "GIVEN a simple title with words
WHEN slugified
THEN words are lowercased and dash-joined."
  ;; GIVEN/WHEN/THEN
  (should (string= "fix-login-bug"
                   (shipit-issue-branch--slugify "Fix login bug"))))

(ert-deftest shipit-issue-branch-test-slugify-special-chars ()
  "GIVEN a title with punctuation
WHEN slugified
THEN special characters collapse to single dashes."
  (should (string= "fix-pr-42-urgent"
                   (shipit-issue-branch--slugify "Fix PR #42 (urgent)!"))))

(ert-deftest shipit-issue-branch-test-slugify-apostrophes ()
  "GIVEN a title containing apostrophes
WHEN slugified
THEN apostrophes are stripped, not dashed."
  (should (string= "dont-break-the-build"
                   (shipit-issue-branch--slugify "Don't break the build"))))

(ert-deftest shipit-issue-branch-test-slugify-collapse-and-trim ()
  "GIVEN a title with leading/trailing/multiple separators
WHEN slugified
THEN result has no leading/trailing dashes and no doubles."
  (should (string= "alpha-beta"
                   (shipit-issue-branch--slugify "  Alpha    -- beta  "))))

(ert-deftest shipit-issue-branch-test-slugify-empty ()
  "GIVEN nil or an empty title
WHEN slugified
THEN result is the empty string."
  (should (string= "" (shipit-issue-branch--slugify nil)))
  (should (string= "" (shipit-issue-branch--slugify ""))))

(ert-deftest shipit-issue-branch-test-slugify-non-ascii ()
  "GIVEN a title with non-ascii letters
WHEN slugified
THEN they are replaced with dashes (safe default)."
  (should (string= "naive-cafe"
                   (shipit-issue-branch--slugify "Naïve café"))))

;;; Template formatting

(ert-deftest shipit-issue-branch-test-format-default ()
  "GIVEN default template '%k-%t' with key and title
WHEN formatted
THEN branch name is KEY-slug."
  (should (string= "PRJ-42-fix-login-bug"
                   (shipit-issue-branch--format "%k-%t" "PRJ-42"
                                                "Fix login bug"))))

(ert-deftest shipit-issue-branch-test-format-numeric-key ()
  "GIVEN numeric issue key
WHEN formatted with default template
THEN number is rendered as string."
  (should (string= "123-add-tests"
                   (shipit-issue-branch--format "%k-%t" 123 "Add tests"))))

(ert-deftest shipit-issue-branch-test-format-custom-template ()
  "GIVEN custom template with prefix and suffix
WHEN formatted
THEN literal text is preserved alongside placeholders."
  (should (string= "feature/PRJ-42/fix-login-bug"
                   (shipit-issue-branch--format "feature/%k/%t" "PRJ-42"
                                                "Fix login bug"))))

(ert-deftest shipit-issue-branch-test-format-lowercased-key ()
  "GIVEN template using %K (lowercased key)
WHEN formatted
THEN key portion is lowercased."
  (should (string= "prj-42-fix-bug"
                   (shipit-issue-branch--format "%K-%t" "PRJ-42" "Fix bug"))))

(ert-deftest shipit-issue-branch-test-format-no-placeholders ()
  "GIVEN template with no placeholders
WHEN formatted
THEN template is returned verbatim."
  (should (string= "static-name"
                   (shipit-issue-branch--format "static-name" "PRJ-42" "x"))))

;;; Truncation

(ert-deftest shipit-issue-branch-test-truncate-short ()
  "GIVEN a short branch name and a generous limit
WHEN truncated
THEN name is unchanged."
  (should (string= "PRJ-42-fix"
                   (shipit-issue-branch--truncate "PRJ-42-fix" 80))))

(ert-deftest shipit-issue-branch-test-truncate-long ()
  "GIVEN a long branch name
WHEN truncated to a limit
THEN name is shortened and trailing dashes removed."
  (should (string= "PRJ-42-this-is-a-very"
                   (shipit-issue-branch--truncate
                    "PRJ-42-this-is-a-very-long-title-that-should-be-cut" 21))))

(ert-deftest shipit-issue-branch-test-truncate-disabled ()
  "GIVEN a limit of 0
WHEN truncated
THEN long names pass through unchanged."
  (let ((long "PRJ-42-this-is-a-very-long-title-that-should-be-cut"))
    (should (string= long (shipit-issue-branch--truncate long 0)))))

;;; Public name builder

(ert-deftest shipit-issue-branch-test-name-applies-template-and-limit ()
  "GIVEN a template and max-length
WHEN building the branch name
THEN template is applied AND result is trimmed to limit."
  (let ((shipit-issue-branch-template "%k-%t")
        (shipit-issue-branch-max-length 14))
    (should (string= "PRJ-42-fix-log"
                     (shipit-issue-branch-name "PRJ-42"
                                               "Fix login bug")))))

(ert-deftest shipit-issue-branch-test-name-respects-buffer-template-override ()
  "GIVEN a buffer-local template override
WHEN building the branch name
THEN the local template is used instead of the global one."
  (let ((shipit-issue-branch-template "%k-%t")
        (shipit-issue-branch-max-length 0))
    (with-temp-buffer
      (setq-local shipit-issue-branch--template-override "feature/%K/%t")
      (should (string= "feature/prj-42/fix-bug"
                       (shipit-issue-branch-name "PRJ-42" "Fix bug"))))))

;;; Copy

(ert-deftest shipit-issue-branch-test-copy-puts-name-on-kill-ring ()
  "GIVEN issue context (key, title) and a default template
WHEN copying the branch name
THEN it lands on the kill ring AND a confirmation is messaged."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (messaged nil)
        (shipit-issue-branch-template "%k-%t")
        (shipit-issue-branch-max-length 0))
    (cl-letf (((symbol-function 'shipit-issue-branch--current-context)
               (lambda () (list :key "PRJ-42" :title "Fix login bug"
                                :repo-dir "/tmp/repo" :base nil)))
              ((symbol-function 'message)
               (lambda (&rest args) (setq messaged (apply #'format args)))))
      (shipit-issue-branch-copy-name)
      (should (string= "PRJ-42-fix-login-bug" (car kill-ring)))
      (should (string-match-p "PRJ-42-fix-login-bug" messaged)))))

;;; Create

(ert-deftest shipit-issue-branch-test-create-runs-git-in-target-dir ()
  "GIVEN issue context with a target repo dir and no base
WHEN creating the branch
THEN git branch NAME is invoked in the target dir, without checkout."
  (let* ((calls nil)
         (target "/tmp/foo-repo/")
         (shipit-issue-branch-template "%k-%t")
         (shipit-issue-branch-max-length 0))
    (cl-letf (((symbol-function 'shipit-issue-branch--current-context)
               (lambda () (list :key "PRJ-42" :title "Fix login bug"
                                :repo-dir target :base nil)))
              ((symbol-function 'shipit-issue-branch--git)
               (lambda (dir &rest args)
                 (push (cons dir args) calls)
                 0)))
      (shipit-issue-branch-create)
      (should (= 1 (length calls)))
      (let ((call (car calls)))
        (should (string= target (car call)))
        (should (equal '("branch" "PRJ-42-fix-login-bug")
                       (cdr call)))))))

(ert-deftest shipit-issue-branch-test-create-with-base ()
  "GIVEN issue context with an explicit base ref
WHEN creating the branch
THEN base ref is appended as the start point of git branch."
  (let* ((calls nil)
         (shipit-issue-branch-template "%k-%t")
         (shipit-issue-branch-max-length 0))
    (cl-letf (((symbol-function 'shipit-issue-branch--current-context)
               (lambda () (list :key "PRJ-42" :title "Fix login bug"
                                :repo-dir "/tmp/r/" :base "origin/main")))
              ((symbol-function 'shipit-issue-branch--git)
               (lambda (dir &rest args)
                 (push (cons dir args) calls)
                 0)))
      (shipit-issue-branch-create)
      (should (equal '("branch" "PRJ-42-fix-login-bug" "origin/main")
                     (cdr (car calls)))))))

(ert-deftest shipit-issue-branch-test-create-errors-when-git-fails ()
  "GIVEN git returns a non-zero exit code
WHEN creating the branch
THEN a user-error is signalled."
  (let ((shipit-issue-branch-template "%k-%t")
        (shipit-issue-branch-max-length 0))
    (cl-letf (((symbol-function 'shipit-issue-branch--current-context)
               (lambda () (list :key "PRJ-42" :title "Fix bug"
                                :repo-dir "/tmp/r")))
              ((symbol-function 'shipit-issue-branch--git)
               (lambda (&rest _) 128)))
      (should-error (shipit-issue-branch-create) :type 'user-error))))

(provide 'test-shipit-issue-branch)
;;; test-shipit-issue-branch.el ends here
