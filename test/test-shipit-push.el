;;; test-shipit-push.el --- Pushing a branch to a remote -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; These push for real, to a bare repository standing in for origin.
;; No network is involved and nothing outside the temporary directory
;; is touched.
;;
;; The case worth having: git reporting a failure for a push that in
;; fact moved the branch.  Reporting that as a failure sends the user
;; looking for a push that happened, and in a stack it stops every
;; layer above.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)

(defun test-push--git (&rest args)
  "Run git with ARGS in `default-directory', erroring on failure."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil args)))
      (unless (zerop exit)
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun test-push--commit (message)
  "Add one file and commit it with MESSAGE."
  (let ((file (format "%s.txt" (replace-regexp-in-string "[^a-z0-9]" "-" message))))
    (with-temp-file (expand-file-name file default-directory) (insert message))
    (test-push--git "add" file)
    (test-push--git "commit" "-q" "-m" message)))

(defmacro test-push--with-remote (&rest body)
  "Run BODY in a repository whose origin is a bare repository on disk."
  (declare (indent 0))
  `(let* ((root (make-temp-file "shipit-push-test-" t))
          (remote (expand-file-name "origin.git" root))
          (work (expand-file-name "work" root)))
     (unwind-protect
         (progn
           (make-directory work)
           (let ((default-directory (file-name-as-directory root)))
             (test-push--git "-c" "init.defaultBranch=main" "init" "-q" "--bare"
                             remote))
           (let ((default-directory (file-name-as-directory work)))
             (test-push--git "-c" "init.defaultBranch=main" "init" "-q")
             (test-push--git "config" "user.email" "test@example.invalid")
             (test-push--git "config" "user.name" "Test")
             (test-push--git "remote" "add" "origin" remote)
             (test-push--commit "base")
             ,@body))
       (delete-directory root t))))

(defun test-push--remote-has (branch)
  "Return the commit origin holds for BRANCH."
  (shipit--remote-branch-rev branch))

(ert-deftest test-shipit-push-a-new-branch-arrives ()
  "GIVEN a branch the remote has never seen
WHEN it is pushed
THEN success is reported and the remote holds the same commit."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "feature")
    (test-push--commit "work")
    (should (shipit--push-branch "feature"))
    (should (equal (shipit--branch-rev "feature")
                   (test-push--remote-has "feature")))))

(ert-deftest test-shipit-push-an-unchanged-branch-is-not-a-failure ()
  "GIVEN a branch the remote already holds at the same commit
WHEN it is pushed again
THEN success is reported.  Pushing what is already there is how every
layer below the one being sent behaves, so it must not read as a
failure."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "feature")
    (test-push--commit "work")
    (should (shipit--push-branch "feature"))
    (should (shipit--push-branch "feature"))))

(ert-deftest test-shipit-push-a-rejected-push-is-a-failure ()
  "GIVEN a branch that has moved apart from the remote copy
WHEN it is pushed without force
THEN failure is reported and the remote is left where it was.  This is
the case the check against the remote must not paper over."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "feature")
    (test-push--commit "work")
    (should (shipit--push-branch "feature"))
    (let ((arrived (test-push--remote-has "feature")))
      (test-push--git "reset" "-q" "--hard" "HEAD~1")
      (test-push--commit "different work")
      (should-not (shipit--push-branch "feature"))
      (should (equal arrived (test-push--remote-has "feature"))))))

(ert-deftest test-shipit-push-git-failing-after-the-branch-arrived-is-success ()
  "GIVEN git reporting a failure for a push that moved the branch
WHEN the result is worked out
THEN it is reported as success, because the remote holds the commit.
This is the false negative: a push that setting the upstream, or a
hook's exit status, failed after.  Reported as a failure it sends the
user looking for a push that happened, and stops every layer above it
in a stack."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "feature")
    (test-push--commit "work")
    (test-push--git "push" "-q" "origin" "feature")
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (if (and (equal program "git") (equal (nth 3 args) "push"))
                     1
                   (apply #'test-push--call-process program args)))))
      (should (shipit--push-branch "feature")))))

(defvar test-push--real-call-process (symbol-function 'call-process)
  "The real `call-process', kept so a stub can still reach git.")

(defun test-push--call-process (program &rest args)
  "Call PROGRAM with ARGS through the real `call-process'."
  (apply test-push--real-call-process program args))

(ert-deftest test-shipit-push-a-failed-dry-run-stays-failed ()
  "GIVEN a dry run that git reports as failing
WHEN the branch is already on the remote at that commit
THEN failure is still reported.  A dry run changes nothing, so the
remote holding the right commit says nothing about whether the run
worked."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "feature")
    (test-push--commit "work")
    (test-push--git "push" "-q" "origin" "feature")
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (if (and (equal program "git") (equal (nth 3 args) "push"))
                     1
                   (apply #'test-push--call-process program args)))))
      (should-not (shipit--push-branch "feature" '("--dry-run"))))))

(ert-deftest test-shipit-push-the-process-buffer-remembers-commands ()
  "GIVEN two logged commands from different directories
WHEN the process buffer is read
THEN both commands, their output and their exit status are there, in
order, with the directory noted where it changed."
  (when (get-buffer shipit-process-buffer-name)
    (kill-buffer shipit-process-buffer-name))
  (shipit-process-log "/tmp/a/" '("git" "push" "origin" "x") 0 "to origin")
  (shipit-process-log "/tmp/b/" '("git" "branch" "-m" "x" "y") 128 "fatal: no")
  (with-current-buffer shipit-process-buffer-name
    (let ((text (buffer-string)))
      (should (string-match "git push origin x" text))
      (should (string-match "to origin" text))
      (should (string-match "git branch -m x y" text))
      (should (string-match "fatal: no" text))
      (should (< (cl-search "git push" text) (cl-search "git branch" text)))
      (should (string-match-p "/tmp/a/" text))
      (should (string-match-p "/tmp/b/" text))
      (should (string-match-p "128" text)))))

(ert-deftest test-shipit-push-a-dry-run-shows-what-it-tried ()
  "GIVEN a dry run of a new branch
WHEN it is pushed
THEN the remote is left untouched AND the process buffer holds the
command and git's answer, which is the only place a dry run's answer
can be read."
  (test-push--with-remote
    (when (get-buffer shipit-process-buffer-name)
      (kill-buffer shipit-process-buffer-name))
    (test-push--git "checkout" "-q" "-b" "feature")
    (test-push--commit "more work")
    (should (shipit--push-branch "feature" '("--dry-run")))
    (should-not (test-push--remote-has "feature"))
    (with-current-buffer shipit-process-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "--dry-run" text))
        (should (string-match-p "feature" text))))))

(ert-deftest test-shipit-push-the-process-buffer-is-reachable-everywhere ()
  "GIVEN any of shipit's browsing buffers
THEN $ opens the process buffer, as it does in magit, so what shipit
ran is one key away wherever the reader happens to stand."
  (dolist (entry '((shipit-buffer . shipit-mode-map)
                   (shipit-stacked-preview . shipit-stacked-preview-mode-map)
                   (shipit-preview . shipit-preview-mode-map)
                   (shipit-actions . shipit-actions-mode-map)
                   (shipit-actions-list . shipit-actions-list-mode-map)
                   (shipit-discussions-buffer . shipit-discussion-mode-map)
                   (shipit-issues-buffer . shipit-issue-mode-map)
                   (shipit-issue-create . shipit-issue-create-mode-map)
                   (shipit-review-history . shipit-review-history-mode-map)
                   (shipit-commands . shipit-doctor-mode-map)
                   (shipit-atlassian-dashboard
                    . shipit-atlassian-dashboard-mode-map)
                   (shipit-review-mode . shipit-review-actions-mode-map)
                   (shipit-commit-comments-buffer
                    . shipit-commit-comments-mode-map)
                   (shipit-releases-buffer . shipit-releases-mode-map)
                   (shipit-repo-buffer . shipit-repo-mode-map)
                   (shipit-subscriptions-buffer . shipit-subscriptions-mode-map)
                   (shipit-notifications-buffer
                    . shipit-notifications-buffer-mode-map)))
    (require (car entry))
    (should (eq #'shipit-process-buffer
                (lookup-key (symbol-value (cdr entry)) (kbd "$"))))))

(ert-deftest test-shipit-push-debug-lines-reach-the-process-buffer-on-request ()
  "GIVEN a debug line on its way to the log file
WHEN the process-buffer mirror is opted into
THEN the line lands there too AND without the opt-in it does not.
Tested at the mirror decision itself: the suite stubs the debug
logger wholesale for other files, so going through it here would
test the stub."
  (when (get-buffer shipit-process-buffer-name)
    (kill-buffer shipit-process-buffer-name))
  (let ((shipit-process-log-debug nil))
    (shipit-process--maybe-mirror-debug "[ts] quiet mouse\n"))
  (should-not (and (get-buffer shipit-process-buffer-name)
                   (with-current-buffer shipit-process-buffer-name
                     (string-match-p "quiet mouse" (buffer-string)))))
  (let ((shipit-process-log-debug t))
    (shipit-process--maybe-mirror-debug "[ts] loud lion\n"))
  (should (with-current-buffer shipit-process-buffer-name
            (string-match-p "loud lion" (buffer-string)))))

(provide 'test-shipit-push)
;;; test-shipit-push.el ends here
