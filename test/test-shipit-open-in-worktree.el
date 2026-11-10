;;; test-shipit-open-in-worktree.el --- C-RET opens the PR's own file -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; C-RET on a file or diff line opens that file for editing.  It used
;; to always open it from the current clone's root, which only shows
;; the pull request's version when the clone happens to be checked out
;; on the PR branch.  When the PR has a worktree, that worktree sits on
;; the PR head, so the file there is the one the diff describes.
;;
;; These run against a real repository with a real linked worktree:
;; the worktree lookup and the file opening are the behaviour under
;; test, so neither is stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-diff)
(require 'shipit-diff)   ; the diff line under point, as in a session
(require 'shipit-worktree)

(defun test-soiw--git (dir &rest args)
  "Run git with ARGS in DIR, signalling when it fails."
  (let ((default-directory dir))
    (unless (zerop (apply #'call-process "git" nil nil nil args))
      (error "git %s failed in %s" (string-join args " ") dir))))

(defun test-soiw--write (file text)
  "Write TEXT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun test-soiw--kill-buffers-under (dir)
  "Kill every buffer visiting a file under DIR."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (and file (string-prefix-p (file-truename dir) (file-truename file)))
        (kill-buffer buffer)))))

(defmacro test-soiw--with-repo (&rest body)
  "Run BODY with a clone of o/r whose PR 7 has a linked worktree.
`root' is the clone, where src/a.c reads \"main\"; `worktree' is the
PR's worktree, where it reads \"pr\".  `default-directory' is the clone,
as it is in a PR buffer."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "shipit-oiw" t)))
          (worktree (file-name-as-directory
                     (expand-file-name ".worktrees/pr-7-feature" root))))
     (unwind-protect
         (let ((default-directory root))
           (test-soiw--git root "init" "-q" "-b" "main")
           (test-soiw--git root "remote" "add" "origin" "https://github.com/o/r.git")
           (test-soiw--git root "config" "user.email" "t@example.com")
           (test-soiw--git root "config" "user.name" "t")
           (test-soiw--write (expand-file-name "src/a.c" root) "main\n")
           (test-soiw--git root "add" ".")
           (test-soiw--git root "commit" "-q" "-m" "init")
           (test-soiw--git root "worktree" "add" "-q" "-b" "feature" worktree)
           (test-soiw--write (expand-file-name "src/a.c" worktree) "pr\n")
           ,@body)
       (test-soiw--kill-buffers-under root)
       (delete-directory root t))))

(defun test-soiw--press-c-ret (properties)
  "Press C-RET on a line carrying PROPERTIES; return the buffer it opens."
  (with-temp-buffer
    (insert (apply #'propertize "     +x\n" properties))
    (goto-char (point-min))
    (shipit--open-file-from-worktree-at-point)
    (current-buffer)))

(ert-deftest test-soiw-c-ret-opens-from-the-pr-worktree ()
  "GIVEN a PR that has a worktree
WHEN C-RET is pressed on one of its files
THEN the file opens from that worktree, which sits on the PR head,
and not from the clone the PR buffer runs in."
  (test-soiw--with-repo
    (let ((buffer (test-soiw--press-c-ret
                   '(shipit-file-path "src/a.c" shipit-pr-number 7 shipit-repo "o/r"))))
      (should (string-prefix-p (file-truename worktree)
                               (file-truename (buffer-file-name buffer))))
      (should (equal (with-current-buffer buffer (buffer-string)) "pr\n")))))

(ert-deftest test-soiw-c-ret-without-a-worktree-opens-from-the-clone ()
  "GIVEN a PR that has no worktree
WHEN C-RET is pressed on one of its files
THEN the file opens from the clone, as before."
  (test-soiw--with-repo
    (let ((buffer (test-soiw--press-c-ret
                   '(shipit-file-path "src/a.c" shipit-pr-number 8 shipit-repo "o/r"))))
      (should (equal (file-truename (buffer-file-name buffer))
                     (file-truename (expand-file-name "src/a.c" root))))
      (should (equal (with-current-buffer buffer (buffer-string)) "main\n")))))

(ert-deftest test-soiw-c-ret-names-the-worktree-when-the-file-is-missing ()
  "GIVEN a PR worktree that does not hold the file at point
WHEN C-RET is pressed on it
THEN the error names the worktree path, so it is clear where it looked."
  (test-soiw--with-repo
    (let ((raised (condition-case err
                      (test-soiw--press-c-ret
                       '(shipit-file-path "src/only-on-main.c"
                                          shipit-pr-number 7 shipit-repo "o/r"))
                    (user-error (error-message-string err)))))
      (should (string-match-p (regexp-quote "pr-7-feature") raised)))))

(provide 'test-shipit-open-in-worktree)
;;; test-shipit-open-in-worktree.el ends here
