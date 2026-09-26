;;; shipit-issue-gitbug.el --- git-bug issue backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; git-bug keeps bugs as git objects in the repository itself, and
;; syncs them with `git bug pull' and `git bug push'.  This backend runs
;; its command line in the repository: the directory the config names
;; as `:dir', or else the repository of the buffer.
;;
;; A bug is known by its short id, such as 5ad82e0.  Its first comment
;; is its description, and it has no web page.
;;
;; A repository whose checkout keeps bugs of git-bug uses this backend
;; without any setting.  A setting names another backend for it, or a
;; checkout elsewhere:
;;
;;   (setq shipit-issue-repo-backends
;;         '(("owner/repo" :backend git-bug :dir "~/src/repo")))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-issue-backends)

(defcustom shipit-issue-gitbug-executable "git-bug"
  "The git-bug program."
  :type 'string
  :group 'shipit)

;;;; Running git-bug

(defun shipit-issue-gitbug--dir (config)
  "The repository git-bug runs in for CONFIG, a directory name."
  (file-name-as-directory
   (expand-file-name
    (or (plist-get config :dir)
        (string-trim (shipit--git-output "rev-parse" "--show-toplevel"))))))

(defun shipit-issue-gitbug--run (config &rest args)
  "The output of git-bug ARGS in the repository of CONFIG.
An error when git-bug fails, with what it said."
  (let ((default-directory (shipit-issue-gitbug--dir config)))
    (with-temp-buffer
      (let ((exit (apply #'process-file shipit-issue-gitbug-executable nil t nil args)))
        (unless (zerop exit)
          (error "shipit: git-bug %s failed: %s"
                 (string-join args " ") (string-trim (buffer-string))))
        (buffer-string)))))

(defun shipit-issue-gitbug--parse (output)
  "OUTPUT of git-bug in its JSON format, as alists and lists."
  (json-parse-string output :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun shipit-issue-gitbug--json (config &rest args)
  "The JSON git-bug ARGS print in the repository of CONFIG, parsed."
  (shipit-issue-gitbug--parse
   (apply #'shipit-issue-gitbug--run config (append args '("--format" "json")))))

(defun shipit-issue-gitbug--run-async (config args callback)
  "Run git-bug ARGS in the background in the repository of CONFIG.
CALLBACK gets what it printed, or nil when it failed.  What it says on
its error output is kept apart, so JSON it prints stays whole."
  (let ((default-directory (shipit-issue-gitbug--dir config))
        (output (generate-new-buffer " *shipit-git-bug*"))
        (errors (generate-new-buffer " *shipit-git-bug-errors*")))
    (make-process
     :name "shipit-git-bug" :buffer output :stderr errors :noquery t
     :connection-type 'pipe
     :command (cons shipit-issue-gitbug-executable args)
     :sentinel (lambda (process _event)
                 (unless (process-live-p process)
                   (let ((text (and (zerop (process-exit-status process))
                                    (with-current-buffer output (buffer-string)))))
                     (kill-buffer output)
                     (when (buffer-live-p errors) (kill-buffer errors))
                     (funcall callback text)))))))

(defun shipit-issue-gitbug--with-message (message function)
  "Call FUNCTION with the name of a file that holds MESSAGE.
git-bug reads a message from a file, so a message that begins with a
dash is not read as a flag."
  (let ((file (make-temp-file "shipit-git-bug-message-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert (or message "")))
          (funcall function file))
      (delete-file file))))

;;;; Normalizing

(defun shipit-issue-gitbug--user (person)
  "PERSON of git-bug, as shipit names the author of an issue or comment."
  `((login . ,(alist-get 'name person)) (avatar_url . nil)))

(defun shipit-issue-gitbug--labels (labels)
  "LABELS of a bug, as shipit lists labels."
  (mapcar (lambda (label) `((name . ,label))) labels))

(defun shipit-issue-gitbug--comment-count (comments)
  "How many comments COMMENTS stand for, the description left out.
A bug shown in full has its comments as a list, and a bug of a list as
their number."
  (max 0 (1- (if (listp comments) (length comments) (or comments 0)))))

(defun shipit-issue-gitbug--normalize-issue (bug)
  "BUG, as git-bug prints it, as shipit shows an issue."
  (let ((comments (alist-get 'comments bug)))
    `((id . ,(alist-get 'human_id bug))
      (number . ,(alist-get 'human_id bug))
      (title . ,(alist-get 'title bug))
      (state . ,(alist-get 'status bug))
      (body . ,(or (and (listp comments) (alist-get 'message (car comments))) ""))
      (user . ,(shipit-issue-gitbug--user (alist-get 'author bug)))
      (labels . ,(shipit-issue-gitbug--labels (alist-get 'labels bug)))
      (comments . ,(shipit-issue-gitbug--comment-count comments))
      (created_at . ,(alist-get 'time (alist-get 'create_time bug)))
      (updated_at . ,(alist-get 'time (alist-get 'edit_time bug))))))

(defun shipit-issue-gitbug--normalize-comment (comment)
  "COMMENT of a bug, as shipit shows a comment.  git-bug gives it no time."
  `((id . ,(alist-get 'id comment))
    (body . ,(alist-get 'message comment))
    (user . ,(shipit-issue-gitbug--user (alist-get 'author comment)))
    (created_at . nil)
    (updated_at . nil)))

;;;; Reading bugs

(defun shipit-issue-gitbug--show (config id)
  "Bug ID of the repository of CONFIG, as git-bug prints it."
  (shipit-issue-gitbug--json config "bug" "show" (format "%s" id)))

(defun shipit-issue-gitbug--fetch-issue (config id)
  "Fetch bug ID of the repository of CONFIG."
  (shipit-issue-gitbug--normalize-issue (shipit-issue-gitbug--show config id)))

(defun shipit-issue-gitbug--comments-of (bug)
  "The comments of BUG, the description left out, normalized."
  (mapcar #'shipit-issue-gitbug--normalize-comment (cdr (alist-get 'comments bug))))

(defun shipit-issue-gitbug--fetch-comments (config id)
  "Fetch the comments of bug ID of the repository of CONFIG."
  (shipit-issue-gitbug--comments-of (shipit-issue-gitbug--show config id)))

(defun shipit-issue-gitbug--fetch-comments-async (config id callback)
  "Fetch the comments of bug ID of CONFIG in the background; CALLBACK gets them."
  (shipit-issue-gitbug--run-async
   config (list "bug" "show" (format "%s" id) "--format" "json")
   (lambda (output)
     (funcall callback (and output (shipit-issue-gitbug--comments-of
                                    (shipit-issue-gitbug--parse output)))))))

(defun shipit-issue-gitbug--search-flag (arg)
  "The git-bug flags for ARG of an issue search, or nil for one it has none for."
  (pcase arg
    ("--state=open" '("--status" "open"))
    ("--state=closed" '("--status" "closed"))
    ((rx "--" (let name (or "author" "label" "title")) "=" (let value (+ anything)))
     (list (concat "--" name) value))))

(defun shipit-issue-gitbug--search-limit (args)
  "The most results ARGS of an issue search ask for, or nil."
  (seq-some (lambda (arg)
              (and (string-match "\\`--limit=\\([0-9]+\\)\\'" arg)
                   (string-to-number (match-string 1 arg))))
            args))

(defun shipit-issue-gitbug--search (config args)
  "Search the bugs of the repository of CONFIG with the transient ARGS."
  (let ((bugs (apply #'shipit-issue-gitbug--json config "bug"
                     ;; `append', not `mapcan': the flags are constants,
                     ;; which joining in place would change for good.
                     (apply #'append (mapcar #'shipit-issue-gitbug--search-flag args))))
        (limit (shipit-issue-gitbug--search-limit args)))
    (mapcar #'shipit-issue-gitbug--normalize-issue
            (if limit (seq-take bugs limit) bugs))))

;;;; Writing bugs

(defun shipit-issue-gitbug--create-issue (config title body)
  "Create a bug of TITLE and BODY in the repository of CONFIG, and fetch it.
Both go as arguments: from a file git-bug takes the title too, from its
first line.  With `=' in each, a text that begins with a dash is not
read as a flag.  git-bug says the short id of the bug it made first."
  (let ((output (shipit-issue-gitbug--run config "bug" "new" (concat "--title=" title)
                                          (concat "--message=" (or body ""))
                                          "--non-interactive")))
    (shipit-issue-gitbug--fetch-issue config (car (split-string output)))))

(defun shipit-issue-gitbug--add-comment (config issue-id body)
  "Add a comment of BODY to bug ISSUE-ID of CONFIG; the comment, normalized."
  (shipit-issue-gitbug--with-message
   body (lambda (file)
          (shipit-issue-gitbug--run config "bug" "comment" "new" (format "%s" issue-id)
                                    "--file" file "--non-interactive")))
  (car (last (shipit-issue-gitbug--fetch-comments config issue-id))))

(defun shipit-issue-gitbug--edit-comment (config comment-id body &optional _issue-id)
  "Make comment COMMENT-ID of CONFIG say BODY.  git-bug finds it by its id alone."
  (shipit-issue-gitbug--with-message
   body (lambda (file)
          (shipit-issue-gitbug--run config "bug" "comment" "edit" comment-id
                                    "--file" file "--non-interactive")))
  `((id . ,comment-id) (body . ,body)))

(defun shipit-issue-gitbug--update-description (config issue-id body)
  "Make the description of bug ISSUE-ID of CONFIG say BODY.
The description is the first comment of the bug."
  (let ((first (car (alist-get 'comments (shipit-issue-gitbug--show config issue-id)))))
    (shipit-issue-gitbug--edit-comment config (alist-get 'id first) body)))

;;;; Creating a bug with its fields

(defun shipit-issue-gitbug--labels-in-use (config)
  "The labels the bugs of the repository of CONFIG use."
  (split-string (shipit-issue-gitbug--run config "label") "\n" t))

(defun shipit-issue-gitbug--creation-fields (_config)
  "The fields of a new bug: its title, its description and its labels."
  (list (list :name 'title :label "Title" :type 'string :required t :fetch-options nil)
        (list :name 'body :label "Description" :type 'text :required nil :fetch-options nil)
        (list :name 'labels :label "Labels" :type 'multi-select :required nil
              :fetch-options #'shipit-issue-gitbug--labels-in-use)))

(defun shipit-issue-gitbug--add-labels (config issue-id labels)
  "Give bug ISSUE-ID of CONFIG the LABELS."
  (when labels
    (apply #'shipit-issue-gitbug--run config "bug" "label" "new" (format "%s" issue-id) labels)))

(defun shipit-issue-gitbug--create-issue-extended (config fields)
  "Create a bug of FIELDS, an alist of `title', `body' and `labels', and fetch it."
  (let ((number (alist-get 'number (shipit-issue-gitbug--create-issue
                                    config (alist-get 'title fields) (alist-get 'body fields)))))
    (shipit-issue-gitbug--add-labels config number (alist-get 'labels fields))
    (shipit-issue-gitbug--fetch-issue config number)))

;;;; Open and closed

(defconst shipit-issue-gitbug--transitions
  '(("open" . (((id . "close") (name . "Close") (to . ((name . "closed"))))))
    ("closed" . (((id . "open") (name . "Reopen") (to . ((name . "open")))))))
  "The transition a bug of each status offers.")

(defun shipit-issue-gitbug--get-transitions (config issue-id)
  "The transitions bug ISSUE-ID of CONFIG offers: to close, or to reopen."
  (cdr (assoc (alist-get 'status (shipit-issue-gitbug--show config issue-id))
              shipit-issue-gitbug--transitions)))

(defun shipit-issue-gitbug--transition-status (config issue-id transition-id)
  "Move bug ISSUE-ID of CONFIG by TRANSITION-ID, \"open\" or \"close\"."
  (unless (member transition-id '("open" "close"))
    (error "shipit: git-bug has no transition %S" transition-id))
  (shipit-issue-gitbug--run config "bug" "status" transition-id (format "%s" issue-id)))

;;;; Finding a repository that keeps its bugs in git-bug

(defvar shipit-issue-gitbug--checkouts (make-hash-table :test 'equal)
  "For each directory, the top of its checkout when that keeps bugs, else nil.")

(defun shipit-issue-gitbug--find-checkout-with-bugs (dir)
  "The top directory of the checkout DIR is in when it keeps bugs, else nil.
git-bug keeps the bugs of a repository under refs/bugs."
  (let ((default-directory dir))
    (with-temp-buffer
      (and (zerop (process-file "git" nil t nil "for-each-ref" "--count=1" "refs/bugs/"))
           (not (string-empty-p (string-trim (buffer-string))))
           (file-name-as-directory
            (string-trim (shipit--git-output "rev-parse" "--show-toplevel")))))))

(defun shipit-issue-gitbug--checkout-with-bugs (dir)
  "As `shipit-issue-gitbug--find-checkout-with-bugs' for DIR, remembered.
Backends are resolved often, and each check runs git."
  (let ((known (gethash dir shipit-issue-gitbug--checkouts 'unknown)))
    (if (eq known 'unknown)
        (puthash dir (shipit-issue-gitbug--find-checkout-with-bugs dir)
                 shipit-issue-gitbug--checkouts)
      known)))

(defun shipit-issue-gitbug--detect (repo)
  "The config of git-bug for REPO, when the checkout here is REPO's and keeps bugs."
  (let ((dir (and (equal repo (shipit--get-repo-from-remote))
                  (shipit-issue-gitbug--checkout-with-bugs default-directory))))
    (and dir (list :backend 'git-bug :dir dir))))

;;;; Naming bugs

(defun shipit-issue-gitbug--reference-patterns (_config)
  "No text refers to a bug: a short id looks like any other hash."
  nil)

(defun shipit-issue-gitbug--browse-url (_config _id)
  "Nil: a bug of git-bug has no web page."
  nil)

(defun shipit-issue-gitbug--id-to-string (id)
  "ID, the short id of a bug, as it is shown."
  (format "%s" id))

(defun shipit-issue-gitbug--string-to-id (string)
  "STRING, a short id as shown, as the id of its bug."
  string)

(shipit-issue-register-backend
 'git-bug
 (list :name "git-bug"
       :fetch-issue #'shipit-issue-gitbug--fetch-issue
       :fetch-comments #'shipit-issue-gitbug--fetch-comments
       :fetch-comments-async #'shipit-issue-gitbug--fetch-comments-async
       :search #'shipit-issue-gitbug--search
       :create-issue #'shipit-issue-gitbug--create-issue
       :reference-patterns #'shipit-issue-gitbug--reference-patterns
       :browse-url #'shipit-issue-gitbug--browse-url
       :id-to-string #'shipit-issue-gitbug--id-to-string
       :string-to-id #'shipit-issue-gitbug--string-to-id
       :add-comment #'shipit-issue-gitbug--add-comment
       :edit-comment #'shipit-issue-gitbug--edit-comment
       :update-description #'shipit-issue-gitbug--update-description
       :get-transitions #'shipit-issue-gitbug--get-transitions
       :transition-status #'shipit-issue-gitbug--transition-status
       :creation-fields #'shipit-issue-gitbug--creation-fields
       :create-issue-extended #'shipit-issue-gitbug--create-issue-extended
       :lacks '(:toggle-reaction)
       :detect #'shipit-issue-gitbug--detect
       :icon-spec '("bug" "octicons" . "#888888")
       :icon-fallback-text "GB"))

(provide 'shipit-issue-gitbug)
;;; shipit-issue-gitbug.el ends here
