;;; test-shipit-buffer-repo.el --- Tests for the repository a buffer is about -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; A shipit page can show a repository that has nothing to do with the
;; directory Emacs was in when the page opened: a repository page opened
;; from Explore or from the subscriptions, for instance.  A command that
;; is asked from such a page is about the repository of the page.
;; These tests ask from pages of acme/widget while the directory is in a
;; real clone of acme/other.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-commands)
(require 'shipit-issues)
(require 'shipit-discussions)
(require 'shipit-issue-create)
(require 'shipit-actions-list)
(require 'shipit-buffer)
(require 'shipit-issues-buffer)
(require 'shipit-discussions-buffer)
(require 'shipit-pr-github)
(require 'shipit-issue-github)
(require 'shipit-comment-github)
(require 'shipit-http)
(require 'shipit-test-http)

(defconst test-buffer-repo--lisp
  (expand-file-name "../lisp/" (file-name-directory (or load-file-name buffer-file-name)))
  "The directory of the shipit sources.")

(defconst test-buffer-repo--not-pages
  '(shipit-notifications-buffer--selected-repo ; a filter of a list of many repositories
    shipit-notifications-buffer--group-by-repo ; a switch, not a repository
    shipit--last-known-repo)                   ; what a cache was filled for
  "Buffer variables named like a repository that say nothing of the page.")

(defun test-buffer-repo--clone-of-other ()
  "Make a repository whose origin is acme/other and return its directory."
  (let ((default-directory (file-name-as-directory (make-temp-file "shipit-buffer-repo-" t))))
    (dolist (arguments '(("init" "-q")
                         ("remote" "add" "origin" "https://github.com/acme/other.git")))
      (unless (zerop (apply #'call-process "git" nil nil nil arguments))
        (error "git %s failed" (string-join arguments " "))))
    default-directory))

(defmacro test-buffer-repo--on-page (variable &rest body)
  "Run BODY on a page that VARIABLE says is about acme/widget.
The directory is in a clone of acme/other.  GitHub is the backend, and
no repository is remembered from an earlier command."
  (declare (indent 1))
  `(let ((directory (test-buffer-repo--clone-of-other))
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit-issue-backend 'github)
         (shipit-issue-repo-backends nil)
         (shipit-github-token "test-token")
         (shipit-current-repo nil))
     (unwind-protect
         (with-temp-buffer
           (setq default-directory directory)
           (set (make-local-variable ,variable) "acme/widget")
           ,@body)
       (delete-directory directory t))))

(defun test-buffer-repo--local-variables ()
  "Every buffer variable of the sources whose name ends in repo."
  (let ((found nil))
    (dolist (file (directory-files test-buffer-repo--lisp t "\\`shipit.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "^(defvar-local \\(shipit[^ \n]*-repo\\)[ \n]" nil t)
          (push (intern (match-string 1)) found))))
    found))

(defconst test-buffer-repo--search-answers
  '(("/search/issues.*acme.*widget" 200 ((total_count . 0) (items . []))))
  "A search of acme/widget that finds nothing.
A search of any other repository gets no answer, which fails the test.")

;;; Which repository a page is about

(ert-deftest test-buffer-repo-each-kind-of-page-answers-for-itself ()
  "GIVEN a page of acme/widget, of each kind in turn, in a directory
that is a clone of acme/other
WHEN the repository of the buffer is asked for
THEN it is acme/widget."
  (dolist (variable shipit--buffer-repo-variables)
    (test-buffer-repo--on-page variable
      (ert-info ((symbol-name variable) :prefix "page variable: ")
        (should (equal "acme/widget" (shipit--repo-for-buffer)))))))

(ert-deftest test-buffer-repo-a-buffer-that-is-no-page-asks-the-directory ()
  "GIVEN a buffer that is about no repository, in a clone of acme/other
WHEN the repository of the buffer is asked for
THEN it is acme/other."
  (test-buffer-repo--on-page 'test-buffer-repo--unrelated
    (should (equal "acme/other" (shipit--repo-for-buffer)))))

(ert-deftest test-buffer-repo-every-page-variable-is-known ()
  "GIVEN every buffer variable of the sources whose name ends in repo
THEN each is known as the repository of a page, or is listed here as
not being one.  A new kind of page that is left out would send its
commands to the repository of the directory.
AND each known one is a buffer variable of the sources."
  (let ((defined (test-buffer-repo--local-variables)))
    (should-not (seq-difference defined
                                (append shipit--buffer-repo-variables
                                        test-buffer-repo--not-pages)))
    (should-not (seq-difference shipit--buffer-repo-variables defined))))

(ert-deftest test-buffer-repo-a-diff-of-a-pull-request-answers-for-itself ()
  "GIVEN a diff buffer that shipit made for a pull request of acme/widget,
in a clone of acme/other
WHEN the repository of the buffer is asked for
THEN it is acme/widget."
  (test-buffer-repo--on-page 'test-buffer-repo--unrelated
    (setq-local test-buffer-repo--unrelated nil)
    (setq-local shipit--diff-pr-context (cons 7 "acme/widget"))
    (should (equal "acme/widget" (shipit--repo-for-buffer)))))

;;; Who may ask the directory

(defconst test-buffer-repo--directory-readers
  '(;; The common code itself.
    shipit--repos-from-remotes shipit--repo-for-buffer shipit--ensure-repository
    ;; Asks whether the directory is a checkout of some repository.
    shipit-issue-gitbug--detect shipit-open-repo-buffer
    shipit--local-repo-matches-p shipit--open-file-diff
    shipit--maybe-display-inline-comments
    shipit--open-pr-cross-repo shipit--open-pr-same-repo
    ;; Works on the branch that is checked out in the directory.
    shipit-open-pr-for-current-branch shipit-pr
    shipit-get-pr-for-branch shipit-get-pr-for-branch-async
    shipit-preview-pr shipit-stacked-preview
    shipit-branch-comments shipit--clear-branch-cache
    ;; Works on the worktrees of the repository in the directory.
    shipit--lfs-pull-all-worktree shipit--lfs-pull-prompted-worktree
    shipit--lfs-pull-pr-worktree
    shipit--create-worktree-pr shipit--create-worktree-pr-async
    shipit--write-pr-info-to-worktree
    shipit-magit-worktrees-insert-section shipit-magit-worktrees-browse-pr
    shipit-magit-worktrees-copy-pr-url shipit-magit-worktrees-remove-merged
    ;; The issue tracker of an issue page is not where its pull requests are.
    shipit-issue--search-linked-prs shipit-issue--link-pull-request
    ;; Runs for no page: polling, the feed, the cache of requests.
    shipit--fetch-all-notifications shipit-explore--fetch-op
    shipit-gh-etag-invalidate
    ;; Diagnostics, which print what the directory says.
    shipit-debug-corruption shipit-debug-workflow-analysis
    shipit-debug-system-state)
  "The functions that read the repository from the git remote of the directory.
Every other one asks `shipit--repo-for-buffer', which knows the page.")

(defun test-buffer-repo--functions-reading-the-directory ()
  "The functions of the sources that call `shipit--get-repo-from-remote'."
  (let ((found nil))
    (dolist (file (directory-files test-buffer-repo--lisp t "\\`shipit.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (while (search-forward "(shipit--get-repo-from-remote)" nil t)
          (save-excursion
            (when (re-search-backward "^(\\(?:cl-\\)?def[a-z-]* \\([^ \n]+\\)" nil t)
              (cl-pushnew (intern (match-string 1)) found))))))
    found))

(ert-deftest test-buffer-repo-only-directory-code-asks-the-directory ()
  "GIVEN every function of the sources that reads the repository from
the git remote of the directory
THEN each is listed here with the reason why the directory is what it
means.  Any other function is about what the user looks at, AND must ask
`shipit--repo-for-buffer', or it goes wrong on a page whose directory
is another repository.
AND the list names no function that reads the directory no more."
  (let ((readers (test-buffer-repo--functions-reading-the-directory)))
    (should-not (seq-difference readers test-buffer-repo--directory-readers))
    (should-not (seq-difference test-buffer-repo--directory-readers readers))))

;;; Commands asked from a repository page

(ert-deftest test-buffer-repo-pull-request-picker-asks-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN the pull request picker asks which repository it is for
THEN it is acme/widget."
  (test-buffer-repo--on-page 'shipit-repo-buffer-repo
    (should (equal "acme/widget" (shipit--ensure-repository)))))

(ert-deftest test-buffer-repo-pull-request-search-asks-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN pull requests are searched for with no repository named
THEN acme/widget is searched."
  (test-buffer-repo--on-page 'shipit-repo-buffer-repo
    (shipit-test-http-answering test-buffer-repo--search-answers
      (shipit--execute-advanced-search nil)
      (should shipit-test-http-requests))))

(ert-deftest test-buffer-repo-issue-searches-ask-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN issues are searched for, quickly or with the advanced search and
no repository named
THEN acme/widget is searched."
  (dolist (search (list #'shipit-issues--quick-search
                        (lambda () (shipit-issues--execute-advanced-search '("--state=open")))))
    (test-buffer-repo--on-page 'shipit-repo-buffer-repo
      (shipit-test-http-answering test-buffer-repo--search-answers
        (funcall search)
        (should shipit-test-http-requests)))))

(ert-deftest test-buffer-repo-issue-search-offers-the-page-first ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN the advanced issue search asks for a repository
THEN acme/widget is what it offers to start with."
  (test-buffer-repo--on-page 'shipit-repo-buffer-repo
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _collection &optional _predicate _require initial &rest _)
                 initial)))
      (shipit-test-http-answering '(("/user/subscriptions" 200 []))
        (should (equal "acme/widget" (shipit-issues--read-repo nil nil nil)))))))

(ert-deftest test-buffer-repo-new-issue-is-for-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN an issue is created
THEN the form that opens is for an issue of acme/widget."
  (test-buffer-repo--on-page 'shipit-repo-buffer-repo
    (let ((form nil))
      (unwind-protect
          (save-window-excursion
            (shipit-test-http-answering nil
              (shipit-create-issue))
            (setq form (current-buffer))
            (should (derived-mode-p 'shipit-issue-create-mode))
            (should (equal "acme/widget" shipit-issue-create--repo)))
        (when (buffer-live-p form) (kill-buffer form))))))

(defun test-buffer-repo--graphql-bodies (thunk)
  "Call THUNK and return the body of each GraphQL request it made.
Each request is answered as a query that finds nothing."
  (let ((bodies nil))
    (shipit-test-http-answering (list shipit-test-http-graphql-empty)
      (let ((answer (symbol-function 'url-retrieve-synchronously)))
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (&rest arguments)
                     (push url-request-data bodies)
                     (apply answer arguments))))
          (funcall thunk))))
    bodies))

(ert-deftest test-buffer-repo-discussion-searches-ask-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN discussions are searched for, quickly or with the advanced search
and no repository named
THEN acme/widget is searched, AND acme/other is not."
  (dolist (search (list #'shipit-discussions--quick-search
                        (lambda () (shipit-discussions--execute-search '("--limit=50")))))
    (test-buffer-repo--on-page 'shipit-repo-buffer-repo
      (let ((bodies (test-buffer-repo--graphql-bodies search)))
        (should bodies)
        (should (seq-every-p (lambda (body) (string-match-p "widget" body)) bodies))
        (should-not (seq-some (lambda (body) (string-match-p "other" body)) bodies))))))

(ert-deftest test-buffer-repo-new-discussion-is-for-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN a discussion is created
THEN the editor that opens is for a discussion of acme/widget."
  (test-buffer-repo--on-page 'shipit-repo-buffer-repo
    (let ((opened nil))
      (cl-letf (((symbol-function 'shipit-editor-open)
                 (lambda (context) (setq opened context))))
        (shipit-create-discussion))
      (should (equal "acme/widget" (plist-get opened :repo))))))

(ert-deftest test-buffer-repo-opening-by-number-asks-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN a pull request, an issue or a discussion is opened by its number
THEN the command reads acme/widget as its repository."
  (dolist (command '(shipit-open-pr-buffer
                     shipit-issues-open-buffer
                     shipit-discussions-open-buffer))
    (test-buffer-repo--on-page 'shipit-repo-buffer-repo
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 7)))
        (ert-info ((symbol-name command) :prefix "command: ")
          (should (equal '(7 "acme/widget")
                         (advice-eval-interactive-spec
                          (cadr (interactive-form command))))))))))

(ert-deftest test-buffer-repo-workflows-are-those-of-the-page ()
  "GIVEN the repository page of acme/widget in a clone of acme/other
WHEN the workflows command reads its repository
THEN it is acme/widget."
  (test-buffer-repo--on-page 'shipit-repo-buffer-repo
    (should (equal '("acme/widget")
                   (advice-eval-interactive-spec
                    (cadr (interactive-form 'shipit-open-actions-list)))))))

;;; Actions inside a pull request page

(ert-deftest test-buffer-repo-file-comments-are-read-from-the-page ()
  "GIVEN the page of pull request 7 of acme/widget in a clone of acme/other
WHEN the comments on one of its files are read
THEN acme/widget is asked for them."
  (test-buffer-repo--on-page 'shipit-buffer-repo
    (let ((shipit--comment-cache (make-hash-table :test 'equal))
          (shipit--cached-inline-comments nil))
      (shipit-test-http-answering '(("/repos/acme/widget/pulls/7/comments" 200 []))
        (shipit-get-pr-comments-for-file 7 "a.c")
        (should shipit-test-http-requests)))))

(provide 'test-shipit-buffer-repo)
;;; test-shipit-buffer-repo.el ends here
