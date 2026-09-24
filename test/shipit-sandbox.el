;;; shipit-sandbox.el --- A repository to try the stacked flow on -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A throwaway repository with commits worth dividing, whose origin is
;; a bare repository beside it.  The git half is real: branches are
;; made, pushes happen, refs can be read back.  The remote's API stands
;; in, so pull requests are made up.
;;
;; `shipit-sandbox-open' builds one and shows the stacked buffer for
;; it.  The stand-in only answers for the sandbox: anywhere else, shipit
;; behaves as it always does, so this is safe to load in the Emacs you
;; work in.  `shipit-sandbox-close' puts everything back.
;;
;; The flow test builds its repository the same way, so what is tried
;; by hand here and what runs in the suite are the same thing.

;;; Code:

(require 'cl-lib)
(require 'shipit-core)
;; Beside this file, which is loaded by hand from outside the load path.
;; Loaded rather than required: `shipit-reload' unbinds every shipit
;; function but loads only the sources again, and a `require' would then
;; find the helper loaded already and skip it.
(load (expand-file-name "shipit-quiet-backend"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar shipit-sandbox-repo "sandbox/demo"
  "Repository name the sandbox answers to.")

(defvar shipit-sandbox-root nil
  "Directory the current sandbox lives in, or nil.")

(defvar shipit-sandbox-prs nil
  "Branch to pull request number, as the stand-in has been told.")

(defvar shipit-sandbox-created nil
  "Pull requests made, newest first, as (HEAD BASE TITLE).")

(defvar shipit-sandbox-stack nil
  "The stack the stand-in holds, or nil.")

;;;; Building one

(defun shipit-sandbox--git (&rest args)
  "Run git with ARGS in `default-directory', erroring on failure."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil args)))
      (unless (zerop exit)
        (error "git %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun shipit-sandbox--commit (message)
  "Add one file and commit it with MESSAGE."
  (let ((file (format "%s.txt" (replace-regexp-in-string "[^a-z0-9]" "-" message))))
    (with-temp-file (expand-file-name file default-directory) (insert message))
    (shipit-sandbox--git "add" file)
    (shipit-sandbox--git "commit" "-q" "-m" message)))

(defconst shipit-sandbox-commits
  '("parser: read the header"
    "parser: read the body"
    "writer: emit the header"
    "writer: emit the body")
  "Commits the sandbox is built with.
Two pieces of work, so the subjects propose a division of their own.")

(defun shipit-sandbox-build (root)
  "Build a repository under ROOT whose origin is a bare one beside it.
Returns the working directory."
  (let ((remote (expand-file-name "origin.git" root))
        (work (expand-file-name "work" root)))
    (make-directory work t)
    (let ((default-directory (file-name-as-directory root)))
      (shipit-sandbox--git "-c" "init.defaultBranch=main" "init" "-q" "--bare" remote))
    (let ((default-directory (file-name-as-directory work)))
      (shipit-sandbox--git "-c" "init.defaultBranch=main" "init" "-q")
      (shipit-sandbox--git "config" "user.email" "sandbox@example.invalid")
      (shipit-sandbox--git "config" "user.name" "Sandbox")
      (shipit-sandbox--git "remote" "add" "origin" remote)
      (shipit-sandbox--commit "base")
      (shipit-sandbox--git "push" "-q" "-u" "origin" "main")
      (shipit-sandbox--git "checkout" "-q" "-b" "work")
      (mapc #'shipit-sandbox--commit shipit-sandbox-commits))
    work))

;;;; Standing in for the remote

(defun shipit-sandbox--entries (numbers)
  "Return NUMBERS as the entries of a stack."
  (mapcar (lambda (number) (list (cons 'number number))) numbers))

(defun shipit-sandbox-reset ()
  "Forget every pull request the stand-in has been told about."
  (setq shipit-sandbox-prs nil
        shipit-sandbox-created nil
        shipit-sandbox-stack nil))

(defun shipit-sandbox--pull-request (number branch)
  "Return a pull request NUMBER for BRANCH, as the stand-in tells it."
  (list (cons 'number number)
        (cons 'title (format "%s title" branch))
        (cons 'body "why this layer exists")
        (cons 'state "open")
        (cons 'draft t)
        (cons 'html_url (format "https://example.invalid/pull/%d" number))
        (cons 'user (list (cons 'login "sandbox")))
        ;; real commits, so anything that reads the diff of this pull
        ;; request reads the diff that is actually there
        (cons 'head (list (cons 'ref branch)
                          (cons 'sha (or (shipit--branch-rev branch)
                                         (shipit--branch-rev "HEAD")))
                          (cons 'repo (list (cons 'full_name shipit-sandbox-repo)))))
        (cons 'base (list (cons 'ref "main")
                          (cons 'sha (shipit--branch-rev "main"))
                          (cons 'repo (list (cons 'full_name shipit-sandbox-repo)))))
        (cons 'created_at "2026-08-29T20:00:00Z")
        (cons 'updated_at "2026-08-29T20:00:00Z")))

(defun shipit-sandbox-backend ()
  "Return a stand-in for the remote, recording what it is asked to do.
What it does not answer for itself it answers with nothing, so a buffer
that asks for more than this has to offer draws an empty one rather
than failing."
  (append
   (shipit-sandbox--operations)
   (shipit-quiet-backend)))

(defun shipit-sandbox--operations ()
  "Return the operations the stand-in answers for itself."
  (list
   :fetch-pr-for-branch
   (lambda (_config branch) (cdr (assoc branch shipit-sandbox-prs)))
   :fetch-pr
   (lambda (_config number &rest _)
     (let ((branch (car (rassoc number shipit-sandbox-prs))))
       (when branch (shipit-sandbox--pull-request number branch))))
   :create-pr
   (lambda (_config title _body base head _draft)
     (let ((number (+ 100 (length shipit-sandbox-created))))
       (push (list head base title) shipit-sandbox-created)
       (push (cons head number) shipit-sandbox-prs)
       (list (cons 'number number)
             (cons 'html_url (format "https://example.invalid/pull/%d" number)))))
   :fetch-stack
   (lambda (_config number &optional _pr-data)
     (when (and shipit-sandbox-stack
                (member number (mapcar (lambda (entry) (cdr (assq 'number entry)))
                                       (cdr (assq 'entries shipit-sandbox-stack)))))
       shipit-sandbox-stack))
   :create-stack
   (lambda (_config numbers)
     (setq shipit-sandbox-stack
           (list (cons 'number 7)
                 (cons 'entries (shipit-sandbox--entries numbers)))))
   :add-to-stack
   (lambda (_config _stack number)
     (setq shipit-sandbox-stack
           (list (cons 'number 7)
                 (cons 'entries (append (cdr (assq 'entries shipit-sandbox-stack))
                                        (shipit-sandbox--entries (list number)))))))))

;;;; Answering for the sandbox only

(defconst shipit-sandbox--repo-variables
  '(shipit-stacked-preview--repo shipit-preview--repo shipit-buffer-repo
    shipit-current-repo shipit-actions--repo)
  "The buffer-local variables by which a buffer says what it is about.")

(defun shipit-sandbox--buffer-repo ()
  "Return the repository this buffer says it is about, or nil."
  (seq-some (lambda (variable)
              (and (boundp variable) (symbol-value variable)))
            shipit-sandbox--repo-variables))

(defun shipit-sandbox-inside-p ()
  "Return non-nil when what is being asked is about the sandbox.

A buffer that says which repository it is about is believed, whatever
directory it happens to sit in.  Only when none says so does the
directory decide, and then not for shipit\='s own buffers: those outlive
the directory they were opened from, and one listing every repository
is about no particular repository.  Judging by directory alone gave a
notifications buffer opened from the sandbox a token that is not one
and refused everything it asked for."
  (and shipit-sandbox-root
       (let ((repo (shipit-sandbox--buffer-repo)))
         (cond
          (repo (equal repo shipit-sandbox-repo))
          ((string-prefix-p "*shipit" (buffer-name)) nil)
          (t (string-prefix-p (expand-file-name shipit-sandbox-root)
                              (expand-file-name default-directory)))))))

(define-advice shipit--get-repo-from-remote
    (:around (original &rest args) shipit-sandbox)
  "Name the sandbox repository, and leave every other one alone."
  (if (shipit-sandbox-inside-p) shipit-sandbox-repo (apply original args)))

(define-advice shipit--upstream-default-branch
    (:around (original &rest args) shipit-sandbox)
  "Answer for the sandbox without asking a remote that is a directory."
  (if (shipit-sandbox-inside-p) "main" (apply original args)))

(define-advice shipit-pr--resolve-for-repo
    (:around (original repo &rest args) shipit-sandbox)
  "Stand in for the remote of the sandbox repository only."
  (if (equal repo shipit-sandbox-repo)
      (cons (shipit-sandbox-backend) (list :repo repo))
    (apply original repo args)))

(define-advice shipit-pr--get-backend (:around (original &rest args) shipit-sandbox)
  "Stand in for the backend while inside the sandbox."
  (if (shipit-sandbox-inside-p) (shipit-sandbox-backend) (apply original args)))

(define-advice shipit-preview--get-default-assignees
    (:around (original &rest args) shipit-sandbox)
  "Ask nobody about assignees for the sandbox."
  (if (shipit-sandbox-inside-p) nil (apply original args)))

(define-advice shipit-preview--find-pr-for-branch
    (:around (original branch repo &rest args) shipit-sandbox)
  "Answer from the stand-in rather than over the network."
  (if (equal repo shipit-sandbox-repo)
      (let ((number (cdr (assoc branch shipit-sandbox-prs))))
        (when number (list (cons 'number number))))
    (apply original branch repo args)))

(define-advice shipit-preview--check-existing-pr
    (:around (original &rest args) shipit-sandbox)
  "Answer from the stand-in rather than over the network."
  (if (shipit-sandbox-inside-p) nil (apply original args)))

(defun shipit-sandbox--ours-p (endpoint)
  "Return non-nil when ENDPOINT is about the sandbox repository."
  (or (shipit-sandbox-inside-p)
      (and (stringp endpoint) (string-match-p shipit-sandbox-repo endpoint))))

(define-advice shipit--github-token (:around (original &rest args) shipit-sandbox)
  "Answer with a token that is not one, and never read the real store.
Reading it can ask for a passphrase, and a sandbox has no business
doing that."
  (if (shipit-sandbox-inside-p) "sandbox-token" (apply original args)))

(define-advice shipit--api-request
    (:around (original endpoint &rest args) shipit-sandbox)
  "Answer nothing for the sandbox rather than asking a real remote.
The stand-in covers what a backend is asked for, but a buffer that
reaches past the backend would otherwise send the sandbox\='s made-up
repository name to github.com."
  (if (shipit-sandbox--ours-p endpoint) nil (apply original endpoint args)))

(define-advice shipit--api-request-post
    (:around (original endpoint &rest args) shipit-sandbox)
  "Answer nothing for the sandbox rather than writing to a real remote."
  (if (shipit-sandbox--ours-p endpoint) nil (apply original endpoint args)))

(defun shipit-sandbox--about-us-p (url)
  "Return non-nil when URL, or whatever is asking for it, is the sandbox\='s."
  (let ((name (regexp-quote shipit-sandbox-repo)))
    (or (shipit-sandbox-inside-p)
        (string-match-p name (format "%s" url))
        (string-match-p name (buffer-name)))))

(define-advice url-retrieve (:around (original url &rest args) shipit-sandbox)
  "Refuse a request the sandbox caused, and let every other one through.
Answering at the backend is not enough: the pull request buffer asks
for things no operation covers -- reactions, a graphql query -- and
those go out under a repository name that does not exist.  The buffer
asking is looked at as well as the address, since a graphql query names
the repository only in its body."
  (if (and shipit-sandbox-root (shipit-sandbox--about-us-p url))
      nil
    (apply original url args)))

(define-advice url-retrieve-synchronously
    (:around (original url &rest args) shipit-sandbox)
  "Refuse a request the sandbox caused, and let every other one through."
  (if (and shipit-sandbox-root (shipit-sandbox--about-us-p url))
      nil
    (apply original url args)))

;;;; Opening and closing

;;;###autoload
(defun shipit-sandbox--work-directory ()
  "Return the working directory of the sandbox, or nil when there is none."
  (let ((work (and shipit-sandbox-root
                   (expand-file-name "work" shipit-sandbox-root))))
    (and work (file-directory-p work) work)))

;;;###autoload
(defun shipit-sandbox-open (&optional fresh)
  "Show the stacked buffer for the sandbox repository.
The sandbox that is already there is opened again, branches, pull
requests and all; with a prefix argument FRESH, or when there is none,
one is built.  Building every time throws away the branches made in the
last one, which is rarely what someone reopening it wants.

Nothing outside the sandbox is affected: the stand-in answers only for
that directory.  `shipit-sandbox-close\=' removes it."
  (interactive "P")
  (require 'shipit-stacked-preview)
  (when (or fresh (not (shipit-sandbox--work-directory)))
    (when fresh (shipit-sandbox-close))
    (shipit-sandbox-reset)
    (setq shipit-sandbox-root (make-temp-file "shipit-sandbox-" t))
    (shipit-sandbox-build shipit-sandbox-root))
  (let ((default-directory (file-name-as-directory
                            (shipit-sandbox--work-directory))))
    (message "Sandbox at %s" shipit-sandbox-root)
    (call-interactively #'shipit-stacked-preview)))

(defun shipit-sandbox--buffers ()
  "Return the buffers sitting in the sandbox."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer (shipit-sandbox-inside-p)))
              (buffer-list)))

(defun shipit-sandbox-close ()
  "Delete the sandbox repository and forget it.
The buffers in it go first.  A buffer whose directory has been deleted
under it fails at the next thing it is asked to do, with a message
about a directory rather than about the sandbox.

The advice stays, and answers for nothing once the root is gone."
  (interactive)
  (when shipit-sandbox-root
    (mapc #'kill-buffer (shipit-sandbox--buffers))
    (delete-directory shipit-sandbox-root t)
    (message "Sandbox at %s removed" shipit-sandbox-root)
    (setq shipit-sandbox-root nil))
  (shipit-sandbox-reset))

(provide 'shipit-sandbox)
;;; shipit-sandbox.el ends here
