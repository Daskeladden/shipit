;;; test-shipit-issue-gitbug.el --- The git-bug issue backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; git-bug keeps bugs in the git repository itself.  These tests run the
;; real git-bug in a scratch repository, and skip themselves where it is
;; not installed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-issue-backends)
(require 'shipit-issue-gitbug)
(require 'shipit-issue-github)

(defun test-gitbug--call (program &rest args)
  "Run PROGRAM with ARGS here, and fail when it fails."
  (with-temp-buffer
    (unless (zerop (apply #'call-process program nil t nil args))
      (error "%s %s failed: %s" program (string-join args " ") (buffer-string)))))

(defmacro test-gitbug--in-repo (config &rest body)
  "Run BODY in a scratch checkout of o/r with a git-bug identity.
CONFIG is bound to the backend's config of it."
  (declare (indent 1))
  `(progn
     (skip-unless (executable-find shipit-issue-gitbug-executable))
     (let* ((dir (file-name-as-directory (make-temp-file "shipit-gitbug-" t)))
            (,config (list :backend 'git-bug :dir dir :repo "o/r")))
       (unwind-protect
           (let ((default-directory dir))
             (test-gitbug--call "git" "init" "-q")
             (test-gitbug--call "git" "config" "user.name" "Tester")
             (test-gitbug--call "git" "config" "user.email" "tester@example.invalid")
             (test-gitbug--call shipit-issue-gitbug-executable "user" "new" "--name" "Tester"
                                "--email" "tester@example.invalid" "--non-interactive")
             ,@body)
         (delete-directory dir t)))))

(defun test-gitbug--new (config title &optional body)
  "Create a bug of TITLE and BODY through the backend of CONFIG; its number."
  (alist-get 'number (shipit-issue-gitbug--create-issue config title (or body "About it."))))

(ert-deftest test-gitbug-a-created-issue-reads-back ()
  "GIVEN a repository with git-bug
WHEN a bug is created through the backend, and fetched again
THEN it has its title and description, is open, names its author, and
is known by its short id."
  (test-gitbug--in-repo config
    (let* ((number (test-gitbug--new config "The parser drops the header" "It reads the body only."))
           (issue (shipit-issue-gitbug--fetch-issue config number)))
      (should (string-match-p "\\`[0-9a-f]\\{7\\}\\'" number))
      (should (equal "The parser drops the header" (alist-get 'title issue)))
      (should (equal "It reads the body only." (alist-get 'body issue)))
      (should (equal "open" (alist-get 'state issue)))
      (should (equal "Tester" (alist-get 'login (alist-get 'user issue))))
      (should (equal 0 (alist-get 'comments issue))))))

(ert-deftest test-gitbug-comments-leave-out-the-description ()
  "GIVEN a bug with one comment
WHEN its comments are fetched
THEN there is that one, with its author: the first comment of a bug is
its description."
  (test-gitbug--in-repo config
    (let ((number (test-gitbug--new config "Title")))
      (shipit-issue-gitbug--add-comment config number "Seen on main too.")
      (let ((comments (shipit-issue-gitbug--fetch-comments config number)))
        (should (= 1 (length comments)))
        (should (equal "Seen on main too." (alist-get 'body (car comments))))
        (should (equal "Tester" (alist-get 'login (alist-get 'user (car comments)))))))))

(ert-deftest test-gitbug-a-comment-is-edited ()
  "GIVEN a bug with a comment
WHEN the comment is edited
THEN it says the new text."
  (test-gitbug--in-repo config
    (let* ((number (test-gitbug--new config "Title"))
           (comment (shipit-issue-gitbug--add-comment config number "Before")))
      (shipit-issue-gitbug--edit-comment config (alist-get 'id comment) "After" number)
      (should (equal "After" (alist-get 'body (car (shipit-issue-gitbug--fetch-comments
                                                    config number))))))))

(ert-deftest test-gitbug-the-description-is-updated ()
  "GIVEN a bug
WHEN its description is updated
THEN it says the new text, and it has still no comment."
  (test-gitbug--in-repo config
    (let ((number (test-gitbug--new config "Title" "Before")))
      (shipit-issue-gitbug--update-description config number "After")
      (let ((issue (shipit-issue-gitbug--fetch-issue config number)))
        (should (equal "After" (alist-get 'body issue)))
        (should (equal 0 (alist-get 'comments issue)))))))

(ert-deftest test-gitbug-transitions-close-and-reopen ()
  "GIVEN an open bug
WHEN it is moved by the transition it offers, and then by the next one
THEN it is closed and then open again: an open bug offers to close, and
a closed one to reopen."
  (test-gitbug--in-repo config
    (let ((number (test-gitbug--new config "Title")))
      (let ((close (car (shipit-issue-gitbug--get-transitions config number))))
        (should (equal "Close" (alist-get 'name close)))
        (shipit-issue-gitbug--transition-status config number (alist-get 'id close)))
      (should (equal "closed" (alist-get 'state (shipit-issue-gitbug--fetch-issue config number))))
      (let ((reopen (car (shipit-issue-gitbug--get-transitions config number))))
        (should (equal "Reopen" (alist-get 'name reopen)))
        (shipit-issue-gitbug--transition-status config number (alist-get 'id reopen)))
      (should (equal "open" (alist-get 'state (shipit-issue-gitbug--fetch-issue config number)))))))

(ert-deftest test-gitbug-search-by-state-and-title ()
  "GIVEN an open bug and a closed one
WHEN bugs are searched by state, and by a word of the title
THEN each search finds the bugs that fit it."
  (test-gitbug--in-repo config
    (let ((open (test-gitbug--new config "The parser drops the header"))
          (closed (test-gitbug--new config "The writer drops the body")))
      (shipit-issue-gitbug--transition-status config closed "close")
      (cl-flet ((found (args) (sort (mapcar (lambda (issue) (alist-get 'number issue))
                                            (shipit-issue-gitbug--search config args))
                                    #'string<)))
        (should (equal (list open) (found '("--state=open"))))
        (should (equal (list closed) (found '("--state=closed"))))
        (should (equal (sort (list open closed) #'string<) (found '("--state=all"))))
        (should (equal (list open) (found '("--state=all" "--title=parser"))))))))

(ert-deftest test-gitbug-a-search-leaves-no-filter-behind ()
  "GIVEN two open bugs
WHEN they are searched by state and a word of one title, and then by
state alone
THEN the second search finds both: the first left no filter behind."
  (test-gitbug--in-repo config
    (test-gitbug--new config "The parser drops the header")
    (test-gitbug--new config "The writer drops the body")
    (should (= 1 (length (shipit-issue-gitbug--search config '("--state=open" "--title=parser")))))
    (should (= 2 (length (shipit-issue-gitbug--search config '("--state=open")))))))

(ert-deftest test-gitbug-creation-asks-for-title-description-and-labels ()
  "GIVEN a repository whose bugs use the label parser
WHEN the fields of a new bug are asked for
THEN they are the title, the description and the labels, and the labels
offer the ones in use."
  (test-gitbug--in-repo config
    (test-gitbug--call shipit-issue-gitbug-executable "bug" "label" "new"
                       (test-gitbug--new config "Title") "parser")
    (let ((fields (shipit-issue-gitbug--creation-fields config)))
      (should (equal '(title body labels) (mapcar (lambda (field) (plist-get field :name)) fields)))
      (should (equal '("parser") (funcall (plist-get (nth 2 fields) :fetch-options) config))))))

(ert-deftest test-gitbug-a-bug-is-created-with-labels ()
  "GIVEN the fields of a new bug, with two labels
WHEN it is created
THEN it has its title and the two labels."
  (test-gitbug--in-repo config
    (let ((issue (shipit-issue-gitbug--create-issue-extended
                  config '((title . "Title") (body . "About it.") (labels "bug" "parser")))))
      (should (equal "Title" (alist-get 'title issue)))
      (should (equal '(((name . "bug")) ((name . "parser"))) (alist-get 'labels issue))))))

(ert-deftest test-gitbug-labels-are-listed ()
  "GIVEN a bug with a label
WHEN it is fetched
THEN it lists the label."
  (test-gitbug--in-repo config
    (let ((number (test-gitbug--new config "Title")))
      (test-gitbug--call shipit-issue-gitbug-executable "bug" "label" "new" number "parser")
      (should (equal '(((name . "parser")))
                     (alist-get 'labels (shipit-issue-gitbug--fetch-issue config number)))))))

(ert-deftest test-gitbug-comments-arrive-in-the-background ()
  "GIVEN a bug with a comment
WHEN its comments are fetched in the background
THEN the callback gets them."
  (test-gitbug--in-repo config
    (let ((number (test-gitbug--new config "Title"))
          (got 'waiting))
      (shipit-issue-gitbug--add-comment config number "Later")
      (shipit-issue-gitbug--fetch-comments-async config number (lambda (comments) (setq got comments)))
      (let ((deadline (+ (float-time) 10)))
        (while (and (eq got 'waiting) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (equal '("Later") (mapcar (lambda (comment) (alist-get 'body comment)) got))))))

(ert-deftest test-gitbug-the-repository-here-is-the-default ()
  "GIVEN a config that names no directory
WHEN a bug is created and fetched
THEN git-bug runs in the repository of the buffer."
  (test-gitbug--in-repo _config
    (let* ((config (list :backend 'git-bug :repo "o/r"))
           (number (test-gitbug--new config "Here")))
      (should (equal "Here" (alist-get 'title (shipit-issue-gitbug--fetch-issue config number)))))))

(ert-deftest test-gitbug-a-failing-command-says-what-git-bug-said ()
  "GIVEN a repository with no such bug
WHEN it is fetched
THEN that fails with what git-bug said, rather than an empty issue."
  (test-gitbug--in-repo config
    (should (string-match-p "git-bug" (cadr (should-error (shipit-issue-gitbug--fetch-issue
                                                           config "0000000")))))))

(ert-deftest test-gitbug-is-a-backend-of-its-own ()
  "GIVEN the backends shipit knows
WHEN a repository is set to use git-bug
THEN it resolves to the git-bug backend, which shipit can load by its
id, and whose bugs have no web page."
  (let ((shipit-issue-repo-backends '(("o/r" :backend git-bug))))
    (should (equal "git-bug" (plist-get (car (shipit-issue--resolve-for-repo "o/r")) :name))))
  (should (eq 'shipit-issue-gitbug (cdr (assq 'git-bug shipit-issue-backend-features))))
  (should-not (shipit-issue-gitbug--browse-url nil "5ad82e0"))
  (should (equal "5ad82e0" (shipit-issue-gitbug--id-to-string "5ad82e0"))))

(provide 'test-shipit-issue-gitbug)
;;; test-shipit-issue-gitbug.el ends here
