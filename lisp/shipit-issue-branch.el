;;; shipit-issue-branch.el --- Branch creation from issue buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Provides a transient menu for creating a git branch named after the
;; current issue (key + slugified title), copying the suggested name,
;; and overriding either the template or the target working tree.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'ucs-normalize)
(require 'shipit-core)

(defvar shipit-issue-buffer-number)
(defvar shipit-issue-buffer-data)

(defcustom shipit-issue-branch-template "%k-%t"
  "Template for branch names built from an issue.
Recognized placeholders:
  %k  Issue key/number as displayed (e.g. PRJ-42, 123).
  %K  Issue key lowercased.
  %t  Slugified title (lowercased, special chars become dashes).
  %T  Raw title — usually unsuitable for branch names."
  :type 'string
  :group 'shipit)

(defcustom shipit-issue-branch-max-length 80
  "Maximum length of a generated branch name.
Use 0 to disable truncation."
  :type 'integer
  :group 'shipit)

(defcustom shipit-issue-branch-default-base nil
  "Default base ref for new branches, or nil to use current HEAD.
Examples: \"main\", \"origin/main\", \"develop\"."
  :type '(choice (const :tag "Current HEAD" nil) (string :tag "Ref"))
  :group 'shipit)

(defvar-local shipit-issue-branch--template-override nil
  "Buffer-local override for `shipit-issue-branch-template'.")

(defvar-local shipit-issue-branch--repo-dir-override nil
  "Buffer-local override for the target git working tree.
When nil, the issue buffer's `default-directory' is used.")

(defvar-local shipit-issue-branch--base-override nil
  "Buffer-local override for `shipit-issue-branch-default-base'.")

(defun shipit-issue-branch--strip-diacritics (text)
  "Return TEXT with combining diacritical marks removed."
  (let ((decomposed (ucs-normalize-NFD-string text)))
    (replace-regexp-in-string "[̀-ͯ]" "" decomposed)))

(defun shipit-issue-branch--slugify (text)
  "Slugify TEXT for use inside a git branch name.
Strips diacritics, lowercases, drops quotes/apostrophes, replaces
every other run of non-alphanumeric characters with a single dash,
then trims leading/trailing separators."
  (let ((s (or text "")))
    (setq s (shipit-issue-branch--strip-diacritics s))
    (setq s (downcase s))
    (setq s (replace-regexp-in-string "[‘’“”'\"`]" "" s))
    (setq s (replace-regexp-in-string "[^a-z0-9_/-]+" "-" s))
    (setq s (replace-regexp-in-string "-+" "-" s))
    (setq s (replace-regexp-in-string "\\`[-./]+" "" s))
    (setq s (replace-regexp-in-string "[-./]+\\'" "" s))
    s))

(defun shipit-issue-branch--format (template key title)
  "Apply TEMPLATE substituting %k/%K/%t/%T with KEY and TITLE."
  (let* ((kstr (format "%s" (or key "")))
         (kstr-low (downcase kstr))
         (slug (shipit-issue-branch--slugify title))
         (raw (or title ""))
         (result template))
    (setq result (string-replace "%K" kstr-low result))
    (setq result (string-replace "%k" kstr result))
    (setq result (string-replace "%T" raw result))
    (setq result (string-replace "%t" slug result))
    result))

(defun shipit-issue-branch--truncate (name limit)
  "Truncate NAME to LIMIT characters, removing trailing separators.
A LIMIT of 0 disables truncation."
  (if (and (> limit 0) (> (length name) limit))
      (replace-regexp-in-string "[-./]+\\'" ""
                                (substring name 0 limit))
    name))

(defun shipit-issue-branch--effective-template ()
  "Return the active branch-name template for this buffer."
  (or shipit-issue-branch--template-override
      shipit-issue-branch-template))

(defun shipit-issue-branch--effective-base ()
  "Return the active base ref for new branches, or nil for HEAD."
  (or shipit-issue-branch--base-override
      shipit-issue-branch-default-base))

(defun shipit-issue-branch-name (key title)
  "Return the branch name derived from KEY and TITLE.
Honors the buffer-local template override and the configured
`shipit-issue-branch-max-length'."
  (shipit-issue-branch--truncate
   (shipit-issue-branch--format (shipit-issue-branch--effective-template)
                                key title)
   shipit-issue-branch-max-length))

(defun shipit-issue-branch--current-context ()
  "Return a plist (:key :title :repo-dir :base) describing the current issue.
Signals a `user-error' when called outside of an issue buffer."
  (unless (and (boundp 'shipit-issue-buffer-number) shipit-issue-buffer-number)
    (user-error "Not in a shipit issue buffer"))
  (let* ((key shipit-issue-buffer-number)
         (data (and (boundp 'shipit-issue-buffer-data) shipit-issue-buffer-data))
         (title (or (cdr (assq 'title data)) ""))
         (repo-dir (or shipit-issue-branch--repo-dir-override
                       default-directory))
         (base (shipit-issue-branch--effective-base)))
    (list :key key :title title :repo-dir repo-dir :base base)))

(defun shipit-issue-branch--list-refs (dir)
  "Return a list of ref names available in DIR for completion.
Includes local branches, remote branches and tags."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil "for-each-ref"
                                 "--format=%(refname:short)"
                                 "refs/heads" "refs/remotes" "refs/tags"))
        (split-string (buffer-string) "\n" t)))))

(defun shipit-issue-branch--git (dir &rest args)
  "Run git in DIR with ARGS, returning the exit code.
Output is collected into the *shipit-issue-branch-git* buffer."
  (let ((default-directory (file-name-as-directory (expand-file-name dir)))
        (out (get-buffer-create "*shipit-issue-branch-git*")))
    (with-current-buffer out
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "$ git %s\n" (mapconcat #'identity args " ")))))
    (apply #'process-file "git" nil out nil args)))

;;;###autoload
(defun shipit-issue-branch-copy-name ()
  "Copy the suggested branch name for the current issue to the kill ring."
  (interactive)
  (let* ((ctx (shipit-issue-branch--current-context))
         (name (shipit-issue-branch-name (plist-get ctx :key)
                                         (plist-get ctx :title))))
    (kill-new name)
    (message "Copied branch name: %s" name)
    name))

;;;###autoload
(defun shipit-issue-branch-create ()
  "Create a git branch for the current issue in the configured repo dir.
The new branch is created from the configured base ref (or HEAD when
no base is set) but the working tree is NOT switched — run
`git checkout NAME' yourself when you are ready."
  (interactive)
  (let* ((ctx (shipit-issue-branch--current-context))
         (name (shipit-issue-branch-name (plist-get ctx :key)
                                         (plist-get ctx :title)))
         (dir (plist-get ctx :repo-dir))
         (base (plist-get ctx :base))
         (args (if base
                   (list "branch" name base)
                 (list "branch" name)))
         (exit (apply #'shipit-issue-branch--git dir args)))
    (unless (zerop exit)
      (user-error "git %s failed (exit %d) — see *shipit-issue-branch-git*"
                  (mapconcat #'identity args " ") exit))
    (message "Created branch %s%s in %s"
             name
             (if base (format " from %s" base) "")
             (abbreviate-file-name dir))
    name))

;;;###autoload
(defun shipit-issue-branch-set-template (template)
  "Override the branch name template for this buffer to TEMPLATE.
An empty TEMPLATE clears the override."
  (interactive
   (list (read-string "Branch name template: "
                      (shipit-issue-branch--effective-template))))
  (setq-local shipit-issue-branch--template-override
              (and template (not (string-empty-p template)) template))
  (let* ((ctx (shipit-issue-branch--current-context))
         (preview (shipit-issue-branch-name (plist-get ctx :key)
                                            (plist-get ctx :title))))
    (message "Branch name preview: %s" preview)))

;;;###autoload
(defun shipit-issue-branch-set-repo-dir (dir)
  "Set the target git working tree for this buffer to DIR."
  (interactive
   (list (read-directory-name
          "Target repo: "
          (or shipit-issue-branch--repo-dir-override default-directory)
          nil t)))
  (setq-local shipit-issue-branch--repo-dir-override
              (file-name-as-directory dir))
  (message "Target repo: %s"
           (abbreviate-file-name shipit-issue-branch--repo-dir-override)))

;;;###autoload
(defun shipit-issue-branch-set-base (base)
  "Set the base ref for new branches in this buffer to BASE.
Empty input clears the override (back to current HEAD)."
  (interactive
   (let* ((ctx (ignore-errors (shipit-issue-branch--current-context)))
          (dir (or (and ctx (plist-get ctx :repo-dir)) default-directory))
          (refs (and dir (shipit-issue-branch--list-refs dir)))
          (current (shipit-issue-branch--effective-base)))
     (list (completing-read
            (format "Base ref%s: "
                    (if current (format " (current: %s)" current) ""))
            refs nil nil nil nil current))))
  (setq-local shipit-issue-branch--base-override
              (and base (not (string-empty-p base)) base))
  (message "Base ref: %s"
           (or shipit-issue-branch--base-override "(HEAD)")))

;;;###autoload (autoload 'shipit-issue-branch-menu "shipit-issue-branch" nil t)
(transient-define-prefix shipit-issue-branch-menu ()
  "Branch actions for the current issue."
  [:description
   (lambda ()
     (let* ((ctx (ignore-errors (shipit-issue-branch--current-context)))
            (name (when ctx
                    (shipit-issue-branch-name (plist-get ctx :key)
                                              (plist-get ctx :title))))
            (dir (when ctx (plist-get ctx :repo-dir)))
            (base (when ctx (plist-get ctx :base))))
       (format
        "Branch from issue\n  Name:     %s\n  Repo:     %s\n  Base:     %s\n  Template: %s"
        (or name "(no issue)")
        (if dir (abbreviate-file-name dir) "(none)")
        (or base "(HEAD)")
        (shipit-issue-branch--effective-template))))
   ("c" "Create branch" shipit-issue-branch-create)
   ("w" "Copy branch name" shipit-issue-branch-copy-name)
   ("b" "Set base ref" shipit-issue-branch-set-base :transient t)
   ("t" "Set template" shipit-issue-branch-set-template :transient t)
   ("d" "Set target repo" shipit-issue-branch-set-repo-dir :transient t)
   ("q" "Quit" transient-quit-one)])

(provide 'shipit-issue-branch)
;;; shipit-issue-branch.el ends here
