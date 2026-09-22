;;; shipit-jira-sandbox.el --- A fake Jira to try the Jira backend on -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; test/fake-jira.py stands in for Jira Cloud on this machine: one
;; project, PRJ, whose first issue holds an attached picture.  It keeps
;; what shipit writes until it stops, so comments, descriptions and
;; uploads can be tried without touching a real site.
;;
;; `shipit-jira-sandbox-open' starts it, points the repository
;; `jira-sandbox/demo' at it and opens its issue PRJ-1.
;; `shipit-jira-sandbox-close' stops it and takes everything back.  The
;; login is a throwaway one in a netrc file of its own, first in
;; `auth-sources' while the sandbox runs, so none of yours reaches it.
;; This is safe to load in the Emacs you work in.
;;
;; The tests in test-shipit-jira-sandbox.el start the same server, so
;; what is tried by hand here and what runs in the suite are the same.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'shipit-core)

(defvar shipit-issue-repo-backends)
(declare-function shipit-issues-open-buffer "shipit-issues-buffer"
                  (issue-number &optional repo backend-id backend-config))

(defconst shipit-jira-sandbox-script
  (expand-file-name "fake-jira.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The fake Jira, a Python script beside this file.")

(defconst shipit-jira-sandbox-repo "jira-sandbox/demo"
  "The repository whose issues the sandbox holds.")

(defconst shipit-jira-sandbox-user "Sandbox User"
  "The display name of the user the fake Jira says is logged in.")

(defconst shipit-jira-sandbox--host "127.0.0.1"
  "The host the fake Jira listens on.
Not \"localhost\": a login of yours for that name would be sent to it.")

(defvar shipit-jira-sandbox--process nil
  "The process of the fake Jira, or nil.")

(defvar shipit-jira-sandbox--netrc nil
  "The netrc file of the sandbox's login, or nil.")

(defvar shipit-jira-sandbox--config nil
  "shipit's config of the fake Jira while it runs, or nil.")

(defun shipit-jira-sandbox--port (process)
  "The port PROCESS, the fake Jira, listens on, once it says so.
An error with what it printed when it stops or says nothing in time."
  (let ((deadline (+ (float-time) 10))
        (said (format "listening on http://%s:\\([0-9]+\\)"
                      (regexp-quote shipit-jira-sandbox--host))))
    (with-current-buffer (process-buffer process)
      (while (and (progn (goto-char (point-min)) (not (re-search-forward said nil t)))
                  (process-live-p process)
                  (< (float-time) deadline))
        (accept-process-output process 0.1))
      (goto-char (point-min))
      (if (re-search-forward said nil t)
          (string-to-number (match-string 1))
        (error "shipit-jira-sandbox: the fake Jira did not start: %s" (buffer-string))))))

(defun shipit-jira-sandbox--login-file ()
  "Write a netrc file that logs in to the fake Jira; its name."
  (let ((file (make-temp-file "shipit-jira-sandbox-" nil ".netrc")))
    (with-temp-file file
      (insert (format "machine %s login sandbox@example.com password sandbox-token\n"
                      shipit-jira-sandbox--host)))
    file))

(defun shipit-jira-sandbox-start ()
  "Start the fake Jira and point `shipit-jira-sandbox-repo' at it.
A sandbox that runs is stopped first.  Return shipit's config of it."
  (shipit-jira-sandbox-stop)
  (let* ((process (make-process :name "shipit-jira-sandbox"
                                :buffer (generate-new-buffer " *shipit-jira-sandbox*")
                                :command (list "python3" shipit-jira-sandbox-script "--port" "0")
                                :connection-type 'pipe
                                :noquery t))
         (config (list :backend 'jira
                       :base-url (format "http://%s:%d" shipit-jira-sandbox--host
                                         (shipit-jira-sandbox--port process))
                       :project-keys '("PRJ"))))
    (setq shipit-jira-sandbox--process process
          shipit-jira-sandbox--netrc (shipit-jira-sandbox--login-file)
          shipit-jira-sandbox--config config)
    (push shipit-jira-sandbox--netrc auth-sources)
    (auth-source-forget-all-cached)
    (push (cons (concat (regexp-quote shipit-jira-sandbox-repo) "\\'") config)
          shipit-issue-repo-backends)
    config))

(defun shipit-jira-sandbox-stop ()
  "Stop the fake Jira, and take back its login and its repository."
  (when shipit-jira-sandbox--process
    (when (process-live-p shipit-jira-sandbox--process)
      (delete-process shipit-jira-sandbox--process))
    (kill-buffer (process-buffer shipit-jira-sandbox--process))
    (setq shipit-jira-sandbox--process nil))
  (when shipit-jira-sandbox--netrc
    (setq auth-sources (delete shipit-jira-sandbox--netrc auth-sources))
    (delete-file shipit-jira-sandbox--netrc)
    (auth-source-forget-all-cached)
    (setq shipit-jira-sandbox--netrc nil))
  (when shipit-jira-sandbox--config
    (setq shipit-issue-repo-backends
          (cl-remove shipit-jira-sandbox--config shipit-issue-repo-backends
                     :key #'cdr :test #'eq))
    (setq shipit-jira-sandbox--config nil)))

;;;###autoload
(defun shipit-jira-sandbox-open ()
  "Start the fake Jira and open its issue PRJ-1 in shipit.
A sandbox that runs already is started afresh, with its issues as they
were at first.  `shipit-jira-sandbox-close' stops it."
  (interactive)
  (require 'shipit-issue-jira)
  (require 'shipit-issues)
  (require 'shipit-issues-buffer)
  (shipit-jira-sandbox-start)
  (shipit-issues-open-buffer "PRJ-1" shipit-jira-sandbox-repo)
  (message "Jira sandbox at %s" (plist-get shipit-jira-sandbox--config :base-url)))

(defun shipit-jira-sandbox-close ()
  "Stop the fake Jira, and take back what the sandbox changed."
  (interactive)
  (let ((base-url (plist-get shipit-jira-sandbox--config :base-url)))
    (shipit-jira-sandbox-stop)
    (when base-url
      (message "Jira sandbox at %s stopped" base-url))))

(provide 'shipit-jira-sandbox)
;;; shipit-jira-sandbox.el ends here
