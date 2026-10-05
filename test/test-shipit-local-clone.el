;;; test-shipit-local-clone.el --- Tests for finding the clone of a repository -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; A pull request buffer runs git for the repository of its pull
;; request.  These tests are for finding the local clone of that
;; repository, and for refusing to fetch its commits into another one.
;; They make real repositories in temporary directories.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'shipit-core)
(require 'shipit-buffer)
(require 'shipit-pr-diff)
(require 'shipit-pr-github)

(defun test-local-clone--git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY; an error when it fails."
  (let ((default-directory (file-name-as-directory directory)))
    (unless (zerop (apply #'call-process "git" nil nil nil arguments))
      (error "git %s failed in %s" (string-join arguments " ") directory))))

(defun test-local-clone--repository (parent name &rest remotes)
  "Make a repository NAME under PARENT with REMOTES, a list of (NAME URL)."
  (let ((directory (expand-file-name name parent)))
    (make-directory directory t)
    (test-local-clone--git directory "init" "-q")
    (dolist (remote remotes)
      (test-local-clone--git directory "remote" "add" (car remote) (cadr remote)))
    (file-name-as-directory directory)))

(defmacro test-local-clone--with-repositories (&rest body)
  "Run BODY with two repositories: `widget', a clone of acme/widget, and `other'.
Their directories are bound to those names.  project.el knows no
repository, magit knows none, and git may use no network."
  (declare (indent 0))
  `(let* ((parent (file-truename (make-temp-file "shipit-clone-test-" t)))
          (widget (test-local-clone--repository
                   parent "widget" '("origin" "git@github.com:acme/widget.git")))
          (other (test-local-clone--repository
                  parent "other" '("origin" "https://github.com/acme/other.git")))
          (project--list nil)
          (magit-repository-directories nil)
          (shipit--repo-root-cache (make-hash-table :test 'equal))
          (process-environment (cons "GIT_ALLOW_PROTOCOL=file" process-environment)))
     (ignore widget other)
     (unwind-protect
         (progn ,@body)
       (delete-directory parent t))))

(ert-deftest test-local-clone-a-repository-is-a-clone-of-what-its-remotes-name ()
  "GIVEN a repository whose remote origin is acme/widget, over ssh
THEN it is a clone of acme/widget, however the name is cased, AND it is
no clone of acme/other
GIVEN a repository with a second remote for acme/widget, over https
THEN it is a clone of acme/widget too."
  (test-local-clone--with-repositories
    (should (shipit--clone-of-p widget "acme/widget"))
    (should (shipit--clone-of-p widget "Acme/Widget"))
    (should-not (shipit--clone-of-p widget "acme/other"))
    (test-local-clone--git other "remote" "add" "upstream" "https://github.com/acme/widget")
    (should (shipit--clone-of-p other "acme/widget"))))

(ert-deftest test-local-clone-a-directory-that-is-no-repository-is-no-clone ()
  "GIVEN a directory that is no repository
THEN it is a clone of nothing, AND that is no error."
  (let ((directory (make-temp-file "shipit-clone-test-" t)))
    (unwind-protect
        (should-not (shipit--clone-of-p directory "acme/widget"))
      (delete-directory directory t))))

(ert-deftest test-local-clone-the-repository-here-is-found-first ()
  "GIVEN a directory inside a clone of acme/widget
WHEN the local clone of acme/widget is asked for there
THEN it is the root of that clone."
  (test-local-clone--with-repositories
    (let ((inside (expand-file-name "src/" widget)))
      (make-directory inside)
      (let ((default-directory inside))
        (should (equal widget (shipit--local-clone-of "acme/widget")))))))

(ert-deftest test-local-clone-a-known-project-is-found-from-another-repository ()
  "GIVEN the current directory in another repository, AND project.el
knows the clone of acme/widget
WHEN the local clone of acme/widget is asked for
THEN it is that clone
GIVEN project.el knows no clone of it
THEN there is none."
  (test-local-clone--with-repositories
    (let ((default-directory other))
      (should-not (shipit--local-clone-of "acme/widget"))
      (let ((project--list (list (list other) (list widget))))
        (should (equal widget (shipit--local-clone-of "acme/widget")))))))

(ert-deftest test-local-clone-a-pull-request-buffer-works-in-the-clone-of-its-repository ()
  "GIVEN the current directory in another repository, AND project.el
knows the clone of acme/widget
WHEN the directory of a pull request buffer of acme/widget is chosen
THEN it is the clone, where its git commands belong
GIVEN no clone is known
THEN it is the repository here, as before."
  (test-local-clone--with-repositories
    (let ((default-directory other))
      (should (equal other (file-name-as-directory
                            (shipit-buffer--directory-for "acme/widget"))))
      (let ((project--list (list (list widget))))
        (should (equal widget (shipit-buffer--directory-for "acme/widget")))))))

(ert-deftest test-local-clone-commits-are-not-fetched-into-another-repository ()
  "GIVEN a pull request buffer of acme/widget whose directory is in
another repository, AND a commit that is not there
WHEN the commit is needed for a comparison
THEN a user error says that this is no clone of acme/widget.  A fetch
there would bring most of the history of acme/widget into a repository
that it does not belong to."
  (test-local-clone--with-repositories
    (let* ((default-directory other)
           (shipit-pr-backend 'github)
           (missing "0123456789abcdef0123456789abcdef01234567")
           (said (cadr (should-error
                        (shipit--ensure-diff-revisions (cons missing missing) "acme/widget")
                        :type 'user-error))))
      (should (string-match-p "no clone of acme/widget" said)))))

(provide 'test-shipit-local-clone)
;;; test-shipit-local-clone.el ends here
