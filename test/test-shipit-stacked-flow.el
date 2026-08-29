;;; test-shipit-stacked-flow.el --- The whole stacked flow -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; Commits to a registered stack, in one go, in a temporary repository
;; whose origin is a bare repository on disk.  The git half is real:
;; the branches are made, the pushes happen, the refs are checked on
;; the remote afterwards.  Only the remote's own API stands in, through
;; the backend registry every operation already goes through.
;;
;; This is the flow a user performs by hand: divide, create branches,
;; push, then submit each layer in turn.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-stacked-preview)
(require 'shipit-preview)

(defvar test-flow--prs nil "Branch to pull request number, as the stand-in sees it.")
(defvar test-flow--created nil "Pull requests created, newest first, as (HEAD BASE TITLE).")
(defvar test-flow--stack nil "The stack the stand-in holds, or nil.")

(defun test-flow--git (&rest args)
  "Run git with ARGS in `default-directory', erroring on failure."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil args)))
      (unless (zerop exit)
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun test-flow--commit (message)
  "Add one file and commit it with MESSAGE."
  (let ((file (format "%s.txt" (replace-regexp-in-string "[^a-z0-9]" "-" message))))
    (with-temp-file (expand-file-name file default-directory) (insert message))
    (test-flow--git "add" file)
    (test-flow--git "commit" "-q" "-m" message)))

(defun test-flow--entries (numbers)
  "Return NUMBERS as the entries of a stack."
  (mapcar (lambda (number) (list (cons 'number number))) numbers))

(defun test-flow--backend ()
  "Return a stand-in for the remote, recording what it is asked to do."
  (list
   :fetch-pr-for-branch (lambda (_config branch) (cdr (assoc branch test-flow--prs)))
   :create-pr
   (lambda (_config title _body base head _draft)
     (let ((number (+ 100 (length test-flow--created))))
       (push (list head base title) test-flow--created)
       (push (cons head number) test-flow--prs)
       (list (cons 'number number) (cons 'html_url "https://example.invalid/pr"))))
   :fetch-stack
   (lambda (_config number &optional _pr-data)
     (when (and test-flow--stack
                (member number (mapcar (lambda (e) (cdr (assq 'number e)))
                                       (cdr (assq 'entries test-flow--stack)))))
       test-flow--stack))
   :create-stack
   (lambda (_config numbers)
     (setq test-flow--stack
           (list (cons 'number 7) (cons 'entries (test-flow--entries numbers)))))
   :add-to-stack
   (lambda (_config _stack number)
     (setq test-flow--stack
           (list (cons 'number 7)
                 (cons 'entries (append (cdr (assq 'entries test-flow--stack))
                                        (test-flow--entries (list number)))))))))

(defmacro test-flow--with-repo (&rest body)
  "Run BODY in a repository with commits to divide and a bare origin."
  (declare (indent 0))
  `(let* ((root (make-temp-file "shipit-flow-test-" t))
          (remote (expand-file-name "origin.git" root))
          (work (expand-file-name "work" root))
          (test-flow--prs nil)
          (test-flow--created nil)
          (test-flow--stack nil))
     (unwind-protect
         (progn
           (make-directory work)
           (let ((default-directory (file-name-as-directory root)))
             (test-flow--git "-c" "init.defaultBranch=main" "init" "-q" "--bare" remote))
           (let ((default-directory (file-name-as-directory work)))
             (test-flow--git "-c" "init.defaultBranch=main" "init" "-q")
             (test-flow--git "config" "user.email" "test@example.invalid")
             (test-flow--git "config" "user.name" "Test")
             (test-flow--git "remote" "add" "origin" remote)
             (test-flow--commit "base")
             (test-flow--git "push" "-q" "-u" "origin" "main")
             (test-flow--git "checkout" "-q" "-b" "work")
             (test-flow--commit "parser: read the header")
             (test-flow--commit "parser: read the body")
             (test-flow--commit "writer: emit the header")
             (test-flow--commit "writer: emit the body")
             (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
                        (lambda (&rest _) "owner/demo"))
                       ((symbol-function 'shipit-pr--resolve-for-repo)
                        (lambda (repo) (cons (test-flow--backend) (list :repo repo))))
                       ((symbol-function 'shipit-pr--get-backend)
                        (lambda (&rest _) (test-flow--backend)))
                       ((symbol-function 'shipit--upstream-default-branch)
                        (lambda (&rest _) "main"))
                       ((symbol-function 'redisplay) (lambda (&rest _) t))
                       ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                       ((symbol-function 'switch-to-buffer) (lambda (&rest _) nil))
                       ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                       ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'shipit-preview--get-default-assignees)
                        (lambda (&rest _) nil))
                       ((symbol-function 'shipit-preview--find-pr-for-branch)
                        (lambda (&rest _) nil))
                       ((symbol-function 'shipit-preview--check-existing-pr)
                        (lambda (&rest _) nil)))
               ,@body)))
       (delete-directory root t))))

(defun test-flow--goto-layer (branch)
  "Put point on the row of BRANCH."
  (goto-char (point-min))
  (unless (search-forward branch nil t)
    (error "No row for %s in %s" branch (buffer-string)))
  (goto-char (match-beginning 0)))

(defun test-flow--submit (branch)
  "Submit the layer of BRANCH the way pressing RET on it does."
  (test-flow--goto-layer branch)
  (shipit-stacked-preview-visit-layer)
  (with-current-buffer (shipit-preview--buffer-name "owner/demo" branch)
    (setq shipit-preview--title (format "%s title" branch)
          shipit-preview--description "why this layer exists")
    (shipit-preview--create-pr)))

(ert-deftest test-shipit-stacked-flow-from-commits-to-a-registered-stack ()
  "GIVEN four commits on a branch, in two pieces of work
WHEN they are divided, given branches, pushed and submitted in turn
THEN two pull requests are raised, the second based on the first, and
the two are registered as one stack.  This is the whole flow, with the
git half real and only the remote's API standing in."
  (test-flow--with-repo
    (shipit-stacked-preview)
    (with-current-buffer (format "*shipit-stacked-preview: %s*" "work")
      ;; divided by what the commit subjects have in common
      (should (equal 2 (length (shipit-stacked-preview--current-layers))))
      (let ((branches (shipit-stacked-preview--branch-names
                       (shipit-stacked-preview--current-layers))))
        (shipit-stacked-preview-create-branches)
        (should (seq-every-p #'shipit--branch-rev branches))

        ;; every layer to the remote, for real
        (shipit-stacked-preview--push-through
         (car (last (shipit-stacked-preview--current-layers))) nil)
        (should (seq-every-p #'shipit--remote-branch-rev branches))

        ;; and then one at a time
        (test-flow--submit (nth 0 branches))
        ;; named plainly: the remote is asked for a branch, not for a
        ;; ref of ours that happens to name one
        (should (equal (list (nth 0 branches) "main"
                             (format "%s title" (nth 0 branches)))
                       (car test-flow--created)))
        (should-not test-flow--stack)

        (test-flow--submit (nth 1 branches))
        (should (equal (list (nth 1 branches) (nth 0 branches)
                             (format "%s title" (nth 1 branches)))
                       (car test-flow--created)))
        (should (equal '(100 101)
                       (mapcar (lambda (e) (cdr (assq 'number e)))
                               (cdr (assq 'entries test-flow--stack)))))

        ;; and the rows say so, the remembered answer that these
        ;; branches had no pull request having been dropped
        (shipit-stacked-preview--render)
        (should (equal '("PR #100" "PR #101")
                       (mapcar #'shipit-stacked-preview--layer-status
                               (shipit-stacked-preview--current-layers))))))))

(provide 'test-shipit-stacked-flow)
;;; test-shipit-stacked-flow.el ends here
