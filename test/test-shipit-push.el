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

(ert-deftest test-shipit-push-a-stack-whose-top-was-never-pushed-arrives-whole ()
  "GIVEN a stack whose lower branch origin holds at an older commit and
whose top branch origin has never seen
WHEN both are pushed together, forced with a lease
THEN both arrive, and the new branch tracks its copy on origin."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "one")
    (test-push--commit "one before")
    (test-push--git "push" "-q" "-u" "origin" "one")
    (test-push--git "commit" "-q" "--amend" "-m" "one after")
    (test-push--git "checkout" "-q" "-b" "two")
    (test-push--commit "two")
    (should (shipit--push-branches '("one" "two") '("--force-with-lease")))
    (should (equal (shipit--branch-rev "one") (test-push--remote-has "one")))
    (should (equal (shipit--branch-rev "two") (test-push--remote-has "two")))
    (should (equal "origin/two"
                   (test-push--git "rev-parse" "--abbrev-ref" "two@{upstream}")))))

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
(defun test-push--recorded-pushes (recorder body)
  "Run BODY with every git push argv handed to RECORDER, and git still run."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &rest args)
               (when (and (equal program "git") (equal (nth 3 args) "push"))
                 (funcall recorder (nthcdr 3 args)))
               (apply #'test-push--call-process program args))))
    (funcall body)))

(ert-deftest test-shipit-push-several-branches-go-in-one-atomic-push ()
  "GIVEN two branches to send
WHEN they are pushed together
THEN git is asked once, atomically, for both, and both arrive.  One
push is one lease check for the whole stack; six pushes are six
chances to leave the stack half-moved on the remote."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "one")
    (test-push--commit "one")
    (test-push--git "checkout" "-q" "-b" "two")
    (test-push--commit "two")
    (let ((pushes nil))
      (test-push--recorded-pushes
       (lambda (argv) (push argv pushes))
       (lambda () (should (shipit--push-branches '("one" "two")))))
      (should (equal 1 (length pushes)))
      (should (member "--atomic" (car pushes)))
      (should (member "one" (car pushes)))
      (should (member "two" (car pushes)))
      (should (equal (shipit--branch-rev "one") (test-push--remote-has "one")))
      (should (equal (shipit--branch-rev "two") (test-push--remote-has "two"))))))

(ert-deftest test-shipit-push-a-refused-lease-on-one-branch-moves-none ()
  "GIVEN two branches already on the remote, the upper one moved there
by someone else since the last fetch, and both rewritten here
WHEN they are pushed together with a lease
THEN the push is refused and neither branch moves: the lower one was a
plain fast-forward git would otherwise have taken, and the stack would
have arrived in two halves."
  (test-push--with-remote
    (test-push--git "checkout" "-q" "-b" "one")
    (test-push--commit "one")
    (test-push--git "checkout" "-q" "-b" "two")
    (test-push--commit "two")
    (should (shipit--push-branches '("one" "two")))
    (let ((one-before (test-push--remote-has "one"))
          (elsewhere (shipit--branch-rev "main")))
      (test-push--git "--git-dir" remote "update-ref" "refs/heads/two" elsewhere)
      (test-push--git "checkout" "-q" "one")
      (test-push--commit "more on one")
      (test-push--git "checkout" "-q" "two")
      (test-push--git "commit" "-q" "--amend" "--allow-empty" "-m" "two, rewritten")
      (should-not (shipit--push-branches '("one" "two") '("--force-with-lease")))
      (should (equal one-before (test-push--remote-has "one")))
      (should (equal elsewhere (test-push--remote-has "two"))))))

(defvar test-push--calls nil
  "Every git command the fake remote below was asked to run, newest first.")

(defun test-push--fake-git (limit &optional refused)
  "A stand-in for `shipit--run-git' behind a remote taking at most LIMIT refs per push.
A push naming REFUSED among its branches is turned away with a stale
lease.  Every call is recorded in `test-push--calls'."
  (lambda (&rest args)
    (push args test-push--calls)
    (let ((branches (cdr (member "origin" args))))
      (cond
       ((> (length branches) limit)
        (cons 1 (format "remote: error: GH013: Repository rule violations found for refs/heads/%s.
remote: - Pushes can not update more than %d branches or tags." (car branches) limit)))
       ((member refused branches)
        (cons 1 (format " ! [rejected]  %s -> %s (stale info)" refused refused)))
       (t (cons 0 ""))))))

(defun test-push--branches-sent ()
  "Return the branch lists of the recorded pushes, oldest first."
  (mapcar (lambda (call) (cdr (member "origin" call)))
          (reverse test-push--calls)))

(ert-deftest test-shipit-push-a-limit-on-refs-per-push-splits-the-stack ()
  "GIVEN a remote that refuses any push updating more than two refs, and
says so in its refusal
WHEN five branches are pushed together with a lease
THEN the whole set is tried once, then goes bottom-up in chunks of two,
each chunk with the same arguments, and success is reported."
  (setq test-push--calls nil)
  (cl-letf (((symbol-function 'shipit--run-git) (test-push--fake-git 2))
            ((symbol-function 'shipit--branches-arrived-p) (lambda (&rest _) nil)))
    (should (shipit--push-branches '("a" "b" "c" "d" "e") '("--force-with-lease")))
    (should (equal '(("a" "b" "c" "d" "e") ("a" "b") ("c" "d") ("e"))
                   (test-push--branches-sent)))
    (dolist (call test-push--calls)
      (should (member "--atomic" call))
      (should (member "--force-with-lease" call)))))

(ert-deftest test-shipit-push-a-refused-chunk-stops-the-ones-above ()
  "GIVEN the same remote, with a stale lease on a branch in the second chunk
WHEN five branches are pushed
THEN the first chunk lands, the refusal is reported as a failure, the
chunks above it are never sent, and the report names what was left."
  (setq test-push--calls nil)
  (let (said)
    (cl-letf (((symbol-function 'shipit--run-git) (test-push--fake-git 2 "d"))
              ((symbol-function 'shipit--branches-arrived-p) (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) said) nil)))
      (should-not (shipit--push-branches '("a" "b" "c" "d" "e") '("--force-with-lease")))
      (should (equal '(("a" "b" "c" "d" "e") ("a" "b") ("c" "d"))
                     (test-push--branches-sent)))
      (should (string-match-p "e" (car said)))
      (should (string-match-p "not sent" (car said))))))

(ert-deftest test-shipit-push-a-known-limit-skips-the-refused-attempt ()
  "GIVEN the limit is already known, two refs per push
WHEN five branches are pushed
THEN no push carries more than two, starting from the bottom."
  (setq test-push--calls nil)
  (let ((shipit-push-max-refs 2))
    (cl-letf (((symbol-function 'shipit--run-git) (test-push--fake-git 2))
              ((symbol-function 'shipit--branches-arrived-p) (lambda (&rest _) nil)))
      (should (shipit--push-branches '("a" "b" "c" "d" "e")))
      (should (equal '(("a" "b") ("c" "d") ("e")) (test-push--branches-sent))))))

;;; test-shipit-push.el ends here
