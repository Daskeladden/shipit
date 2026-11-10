;;; test-shipit-fork-clone-cases.el --- More cases of a clone of a fork -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The cases of a clone of a fork that the scenarios of
;; test-shipit-fork-clone.el leave out: what shipit remembers of the
;; answers of GitHub, a clone of a repository with another name, the
;; remote that names a repository, which known clone wins, the file that
;; a new worktree keeps about its pull request, the commits of a
;; comparison, and the page of a merge request in a clone of a fork of a
;; GitLab project.
;;
;; They use the kitchen, the page and the stand-ins of
;; test-shipit-fork-clone.el and test-shipit-worktree-gitlab.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'magit-repos)
(require 'shipit-cache)
(require 'shipit-pr-diff)
(require 'test-shipit-fork-clone)
(require 'test-shipit-worktree-gitlab)

;;; Asking GitHub, without a page

(defmacro test-fork-clone-cases--with-github (answers &rest body)
  "Run BODY as me of GitHub, while GitHub gives ANSWERS.
Shipit remembers no answer about a fork from an earlier test."
  (declare (indent 1))
  `(test-page-views--as-me
     (let ((shipit--forked-from-cache (make-hash-table :test 'equal)))
       (shipit-test-http-answering ,answers
         ,@body))))

(defun test-fork-clone-cases--my-fork-is-no-fork ()
  "GitHub's answers that say that me/r is no fork."
  (list (list (test-fork-clone--about "me/r") 200 '((full_name . "me/r") (fork . :json-false)))))

(defun test-fork-clone-cases--my-fork-gives-an-error ()
  "GitHub's answers about me/r: each is an error."
  (list (list (test-fork-clone--about "me/r") 500 '((message . "Server Error")))))

;;; Repositories of the kitchen with no commit, known to project.el

(defun test-fork-clone-cases--repository (name repo)
  "Make the repository NAME in the kitchen, whose remote origin names REPO at GitHub.
Return its directory."
  (test-worktree-offer--git "." "init" "-q" "-b" "main" name)
  (test-worktree-offer--git name "remote" "add" "origin"
                            (format "https://github.com/%s.git" repo))
  (file-name-as-directory (test-worktree-offer--path name)))

(defun test-fork-clone-cases--in-the-street (function)
  "Call FUNCTION in a directory of the kitchen that is in no repository."
  (make-directory (test-fork-clone--street))
  (let ((default-directory (test-fork-clone--street))
        (process-environment (test-fork-clone--git-stops-at-the-kitchen)))
    (funcall function)))

(defun test-fork-clone-cases--same-directory-p (one other)
  "Whether the directories ONE and OTHER are the same, by their real paths."
  (equal (file-name-as-directory (file-truename one))
         (file-name-as-directory (file-truename other))))

;;; The clone, outside a page

(defun test-fork-clone-cases--in-the-clone (given scenario)
  "Run SCENARIO in the clone of a new kitchen that GIVEN prepared, as me of GitHub.
GIVEN and SCENARIO are functions.  GitHub says that me/r is a fork of
o/r, and shipit remembers no answer about a fork from an earlier test."
  (test-worktree-offer--in-a-new-kitchen
   (lambda ()
     (test-worktree-offer--build-kitchen)
     (funcall given)
     (test-fork-clone-cases--with-github (test-fork-clone--my-fork-is-of-o-r)
       (let ((default-directory (test-worktree-offer--path "clone/")))
         (funcall scenario))))))

(defun test-fork-clone-cases--clone-has-p (sha)
  "Whether the clone has the commit SHA."
  (let ((default-directory (test-worktree-offer--path "clone/")))
    (zerop (process-file "git" nil nil nil "cat-file" "-e" (concat sha "^{commit}")))))

;;; The worktree of the page

(defun test-fork-clone-cases--repo-of-the-worktree ()
  "The repository that the file of shipit in the worktree pr-8-coffee names."
  (cdr (assq 'repo (json-read-file
                    (test-worktree-offer--in-worktree ".shipit-pr-info.json")))))

;;; A clone of a fork of a GitLab project

(defconst test-fork-clone-cases--gitlab-fork-address
  "https://gitlab.example.com/me/cups.git"
  "The address of my fork me/cups of the project kitchen/menu/cups at GitLab.")

(defun test-fork-clone-cases--clone-of-my-gitlab-fork ()
  "Make the clone a clone of my fork me/cups: its only remote, origin, names it."
  (test-worktree-offer--git "clone" "config" "remote.origin.url"
                            test-fork-clone-cases--gitlab-fork-address))

(defun test-fork-clone-cases--my-gitlab-fork-is-of-the-project ()
  "GitLab's answer about the project me/cups: a fork of kitchen/menu/cups."
  `((,(concat (regexp-quote "gitlab.example.com/api/v4/projects/me%2Fcups") "\\'") 200
     ((id . 9) (path_with_namespace . "me/cups") (namespace (name . "me"))
      (forked_from_project (id . 5) (path_with_namespace . "kitchen/menu/cups"))))))

(defun test-fork-clone-cases--when-gitlab-also-answers (answers scenario)
  "Run SCENARIO, a function, while GitLab gives ANSWERS too, before its others."
  (let ((answers-of-the-kitchen (symbol-function 'test-worktree-gitlab--answers)))
    (cl-letf (((symbol-function 'test-worktree-gitlab--answers)
               (lambda () (append answers (funcall answers-of-the-kitchen)))))
      (funcall scenario))))

;;; The scenarios

(ert-deftest test-fork-clone-cases-a-second-question-asks-github-nothing ()
  "Shipit asks GitHub once whether my fork is a fork of o/r, and remembers the answer."
  ;; GIVEN GitHub says that me/r is a fork of o/r
  ;; WHEN shipit asks twice whether me/r is a fork of o/r
  ;; THEN it is, both times
  ;; AND GitHub was asked about me/r once
  (test-fork-clone-cases--with-github (test-fork-clone--my-fork-is-of-o-r)
    (should (shipit--fork-of-p "me/r" "o/r"))
    (should (shipit--fork-of-p "me/r" "o/r"))
    (should (= 1 (test-fork-clone--times-asked-about "me/r")))))

(ert-deftest test-fork-clone-cases-no-fork-is-remembered ()
  "Shipit remembers that my repository is no fork, and asks GitHub no more about it."
  ;; GIVEN GitHub says that me/r is no fork
  ;; WHEN shipit asks twice whether me/r is a fork of o/r
  ;; THEN it is not, both times
  ;; AND GitHub was asked about me/r once
  (test-fork-clone-cases--with-github (test-fork-clone-cases--my-fork-is-no-fork)
    (should-not (shipit--fork-of-p "me/r" "o/r"))
    (should-not (shipit--fork-of-p "me/r" "o/r"))
    (should (= 1 (test-fork-clone--times-asked-about "me/r")))))

(ert-deftest test-fork-clone-cases-an-error-is-not-remembered ()
  "Shipit asks GitHub again about my fork after GitHub gave an error."
  ;; GIVEN GitHub gives an error for each question about me/r
  ;; WHEN shipit asks twice whether me/r is a fork of o/r
  ;; THEN it is not, both times, and no error comes out
  ;; AND GitHub was asked about me/r twice
  (test-fork-clone-cases--with-github (test-fork-clone-cases--my-fork-gives-an-error)
    (should-not (shipit--fork-of-p "me/r" "o/r"))
    (should-not (shipit--fork-of-p "me/r" "o/r"))
    (should (= 2 (test-fork-clone--times-asked-about "me/r")))))

(ert-deftest test-fork-clone-cases-clearing-the-caches-forgets-the-answers ()
  "After `shipit-clear-all-caches', shipit asks GitHub again about my fork."
  ;; GIVEN GitHub said that me/r is a fork of o/r, and shipit remembers it
  ;; WHEN I clear all caches of shipit
  ;; AND shipit asks again whether me/r is a fork of o/r
  ;; THEN it is
  ;; AND GitHub was asked about me/r twice
  (test-fork-clone-cases--with-github (test-fork-clone--my-fork-is-of-o-r)
    (should (shipit--fork-of-p "me/r" "o/r"))
    (cl-letf (((symbol-function 'message) #'ignore))
      (shipit-clear-all-caches))
    (should (shipit--fork-of-p "me/r" "o/r"))
    (should (= 2 (test-fork-clone--times-asked-about "me/r")))))

(ert-deftest test-fork-clone-cases-a-clone-of-another-name-asks-github-nothing ()
  "A clone of a repository with another name than o/r is no clone of o/r, and GitHub is not asked."
  ;; GIVEN a clone of acme/other, whose name is not that of o/r
  ;; AND the page of PR 8 of o/r, opened from that clone
  ;; THEN the Worktree section says that the clone is no clone of o/r
  ;; AND GitHub was not asked about acme/other
  (test-fork-clone--on-the-page-of-pr-8 nil #'test-pr-worktree-found-cases--origin-names-other
    (test-fork-clone--section-says-no-clone)
    (should (zerop (test-fork-clone--times-asked-about "acme/other")))))

(ert-deftest test-fork-clone-cases-the-remote-that-names-a-repo ()
  "Shipit finds the remote of the clone that names a repository, and none for another one."
  ;; GIVEN a clone whose origin is my fork me/r and whose remote upstream
  ;;   is o/r
  ;; THEN the remote that names o/r is upstream
  ;; AND the remote that names me/r, in another case, is origin
  ;; AND no remote names acme/other
  (test-fork-clone-cases--in-the-clone #'test-fork-clone--clone-of-my-fork-and-o-r
    (lambda ()
      (should (equal "upstream" (shipit--remote-naming "o/r")))
      (should (equal "origin" (shipit--remote-naming "Me/R")))
      (should-not (shipit--remote-naming "acme/other")))))

(ert-deftest test-fork-clone-cases-a-known-clone-of-o-r-wins-over-an-earlier-fork ()
  "A known clone of o/r wins over a clone of my fork that project.el knows before it."
  ;; GIVEN project.el knows a clone of my fork me/r, and after it a clone
  ;;   of o/r
  ;; AND GitHub says that me/r is a fork of o/r
  ;; AND Emacs is in a directory that is in no repository
  ;; WHEN shipit looks for the local clone of o/r
  ;; THEN it is the clone of o/r
  ;; AND GitHub was not asked about me/r
  (test-worktree-offer--in-a-new-kitchen
   (lambda ()
     (let* ((fork (test-fork-clone-cases--repository "fork" "me/r"))
            (upstream (test-fork-clone-cases--repository "upstream" "o/r"))
            (project--list (list (list fork) (list upstream)))
            (magit-repository-directories nil))
       (test-fork-clone-cases--with-github (test-fork-clone--my-fork-is-of-o-r)
         (test-fork-clone-cases--in-the-street
          (lambda ()
            (should (test-fork-clone-cases--same-directory-p
                     upstream (shipit--local-clone-of "o/r")))
            (should (zerop (test-fork-clone--times-asked-about "me/r"))))))))))

(ert-deftest test-fork-clone-cases-a-new-worktree-names-the-repo-of-the-pr ()
  "The worktree that Checkout makes in a clone of my fork names o/r as the repository of its PR."
  ;; GIVEN a clone of my fork me/r, whose only remote is origin for me/r
  ;; AND GitHub says that me/r is a fork of o/r
  ;; AND the page of PR 8 of o/r, opened from that clone
  ;; WHEN I run "Checkout in worktree" of the worktree menu
  ;; THEN the worktree pr-8-coffee is made at the head of the PR
  ;; AND the file that shipit keeps in it names o/r as the repository of
  ;;   the PR, not me/r
  (test-fork-clone--on-the-page-of-pr-8 (test-fork-clone--my-fork-is-of-o-r)
      #'test-fork-clone--clone-of-my-fork
    (test-fork-clone--run-unrefused #'shipit--checkout-pr-worktree)
    (test-fork-clone--wait-until-ready-or-failed)
    (test-fork-clone--worktree-is-at-the-pr-head)
    (should (equal "o/r" (test-fork-clone-cases--repo-of-the-worktree)))))

(ert-deftest test-fork-clone-cases-a-comparison-gets-its-commits-in-a-clone-of-my-fork ()
  "The commits of a comparison of a PR of o/r are fetched into a clone of my fork."
  ;; GIVEN a clone of my fork me/r, which GitHub says is a fork of o/r
  ;; AND the head of PR 8, which the clone does not have
  ;; WHEN the comparison of main with the head of PR 8 of o/r is made
  ;;   ready in the clone
  ;; THEN it is not refused
  ;; AND the clone has the head of PR 8
  (test-fork-clone-cases--in-the-clone #'test-fork-clone--clone-of-my-fork
    (lambda ()
      (let ((revisions (cons (test-worktree-offer--sha "main")
                             (test-worktree-offer--sha "coffee"))))
        (should-not (test-fork-clone-cases--clone-has-p (cdr revisions)))
        (cl-letf (((symbol-function 'message) #'ignore))
          (should (equal revisions (shipit--ensure-diff-revisions revisions "o/r"))))
        (should (test-fork-clone-cases--clone-has-p (cdr revisions)))))))

(ert-deftest test-fork-clone-cases-a-clone-of-a-gitlab-fork-has-the-worktrees ()
  "A clone of a fork of a GitLab project is a clone of that project, on the page of its merge request."
  ;; GIVEN a clone of my fork me/cups, whose only remote is origin for
  ;;   me/cups at GitLab
  ;; AND GitLab says that me/cups is a fork of kitchen/menu/cups
  ;; AND the page of merge request 8 of kitchen/menu/cups, opened from
  ;;   that clone
  ;; THEN the Worktree section says "No worktree checked out"
  ;; AND it does not say that the clone is no clone of kitchen/menu/cups
  (let ((shipit--forked-from-cache (make-hash-table :test 'equal)))
    (test-fork-clone-cases--when-gitlab-also-answers
     (test-fork-clone-cases--my-gitlab-fork-is-of-the-project)
     (lambda ()
       (test-worktree-gitlab--on-the-page-of-mr-8 #'test-fork-clone-cases--clone-of-my-gitlab-fork
         (test-pr-worktree-found--worktree-section-says "Status: No worktree checked out\n")
         (should-not (string-search "is no clone of"
                                    (test-worktree-offer--worktree-section))))))))

(provide 'test-shipit-fork-clone-cases)
;;; test-shipit-fork-clone-cases.el ends here
