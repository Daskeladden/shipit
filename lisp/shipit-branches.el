;;; shipit-branches.el --- The branches of a repository -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The branches of a repository, as the backend gives them, in the
;; groups of the branches page of GitHub.  A repository can be watched:
;; each time shipit polls for notifications, it asks for the names of
;; the branches of each watched repository, and a new branch that
;; someone else pushed gives a notification.  A branch with an open pull
;; request gives none, because the pull request gives one of its own.
;; The notifications that were not read are kept in a file, so a restart
;; does not lose them.
;;
;;   (setq shipit-branches-watched-repos '("owner/repo"))
;;
;; `shipit-branches', in shipit-branches-buffer.el, shows the branches,
;; and `w' there watches the repository.

;;; Code:
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-issue-backends)
(require 'shipit-notifications)

(declare-function shipit-commit-comments-open-branch "shipit-commit-comments-buffer")

(defcustom shipit-branches-watched-repos nil
  "The repositories whose new branches give a notification, as owner/name.
Each time shipit polls for notifications, it asks for the names of the
branches of each of them.  A new branch whose last commit someone else
made gives a notification.  `w' in the branches of a repository adds it
here or takes it away, and saves the list with Customize."
  :type '(repeat string)
  :group 'shipit)

(defcustom shipit-branches-watch-file
  (expand-file-name "shipit-branches-watch.el" user-emacs-directory)
  "The file that keeps what the watch of branches knows, between two sessions.
It keeps the branches that were seen, and the notifications of new
branches that were not read.  With nil they are kept for the session
only."
  :type '(choice file (const :tag "Keep them in the session only" nil))
  :group 'shipit)

(defconst shipit-branches--stale-days 90
  "After how many days without a commit a branch is stale, as on GitHub.")

(defconst shipit-branches-group-order '(default yours active stale)
  "The groups of branches, in the order they are shown.")

(defvar shipit-branches--state nil
  "What the watch of branches knows, a plist, or nil until the file is read.
:seen is an alist of (REPO . NAMES), the names of the branches that
were seen.  :unread is the list of the notifications of new branches
that were not read.")

(defvar shipit-branches--etags nil
  "The ETag that came with the names of the branches of each repository.
It is an alist of (REPO . ETAG), kept for the session only.")

;;; The branches

(defun shipit-branches--call (repo operation &rest args)
  "Call OPERATION of the backend of REPO with its config and ARGS.
A backend without branches is a user error."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (function (plist-get (car resolved) operation)))
    (unless function
      (user-error "This backend lists no branches"))
    (apply function (cdr resolved) args)))

(defun shipit-branches-fetch-async (repo callback)
  "Call CALLBACK with the branches of REPO, the newest last commit first.
CALLBACK gets nil when nothing is known of them."
  (shipit-branches--call repo :fetch-branches-async callback))

(defun shipit-branches--fetch-names-async (repo etag callback)
  "Call CALLBACK with the names of all branches of REPO.
CALLBACK gets `unchanged' when they did not change since ETAG came with
them, else (ETAG . NAMES), or nil when nothing is known."
  (shipit-branches--call repo :fetch-branch-names-async etag callback))

(defun shipit-branches--fetch-named-async (repo names callback)
  "Call CALLBACK with the branches of REPO named NAMES that are there.
CALLBACK gets nil when nothing is known of them."
  (shipit-branches--call repo :fetch-named-branches-async names callback))

(defun shipit-branches--stale-time ()
  "The time before which a last commit makes a branch stale.
It is written as GitHub writes a time, so that two times compare as
strings."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract nil (days-to-time shipit-branches--stale-days))
                      t))

(defun shipit-branches--group (branch stale-time)
  "The group of BRANCH: default, yours, active or stale.
A last commit before STALE-TIME makes a branch of another stale."
  (let-alist branch
    (cond (.default 'default)
          (.own 'yours)
          ((string< .time stale-time) 'stale)
          (t 'active))))

(defun shipit-branches-groups (branches)
  "BRANCHES in groups, as an alist of (GROUP . BRANCHES).
GROUP is default, yours, active or stale, in that order, and a group
without branches is left out.  Each group keeps the order of BRANCHES."
  (let ((stale-time (shipit-branches--stale-time)))
    (cl-loop for group in shipit-branches-group-order
             for members = (seq-filter
                            (lambda (branch)
                              (eq (shipit-branches--group branch stale-time) group))
                            branches)
             when members collect (cons group members))))

;;; What the watch knows

(defun shipit-branches--read-state ()
  "What `shipit-branches-watch-file' keeps, or nil."
  (let ((file shipit-branches-watch-file))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        ;; A file that holds nothing yet says nothing.
        (and (> (buffer-size) 0)
             (read (current-buffer)))))))

(defun shipit-branches--state ()
  "What the watch of branches knows, read from the file at first."
  (or shipit-branches--state
      (setq shipit-branches--state
            (or (shipit-branches--read-state) (list :seen nil :unread nil)))))

(defun shipit-branches--set (key value)
  "Make VALUE what the watch knows as KEY, and write all it knows to the file."
  (setq shipit-branches--state (plist-put (shipit-branches--state) key value))
  (when shipit-branches-watch-file
    (with-temp-file shipit-branches-watch-file
      ;; A limit on printing would cut a long list of names short.
      (let ((print-length nil)
            (print-level nil))
        (prin1 shipit-branches--state (current-buffer))))))

(defun shipit-branches--remember (repo names)
  "Keep NAMES as the names of the branches of REPO that were seen.
A branch that went away is forgotten, so that it is new when it comes
back."
  (shipit-branches--set
   :seen (cons (cons repo names)
               (assoc-delete-all repo (copy-sequence
                                       (plist-get (shipit-branches--state) :seen))))))

(defun shipit-branches--new-names (repo names)
  "Those of NAMES that were not seen in REPO; none when REPO was never seen.
A big repository has thousands of branches, so the names that were seen
are looked up in a table."
  (when-let* ((seen (assoc repo (plist-get (shipit-branches--state) :seen))))
    (let ((table (make-hash-table :test 'equal :size (length (cdr seen)))))
      (dolist (name (cdr seen))
        (puthash name t table))
      (seq-remove (lambda (name) (gethash name table)) names))))

(defun shipit-branches--unread ()
  "The notifications of new branches that were not read."
  (plist-get (shipit-branches--state) :unread))

;;; What a branch opens

(defun shipit-branches--operation (repo operation)
  "OPERATION of the backend of REPO, or nil when it has none."
  (plist-get (car (shipit-pr--resolve-for-repo repo)) operation))

(defun shipit-branches-compare-url (repo base name)
  "The address of the page that shows how branch NAME of REPO differs from BASE."
  (funcall (or (shipit-branches--operation repo :browse-compare-url)
               (user-error "This backend has no page that compares branches"))
           (cdr (shipit-pr--resolve-for-repo repo)) base name))

(defun shipit-branches-open-branch (repo name base)
  "Show in shipit how branch NAME of REPO differs from the branch BASE.
The view of a branch reads the comments of its commits, so a backend
without them shows the page of the comparison in the browser."
  (if (not (shipit-branches--operation repo :fetch-commit-comments))
      (browse-url (shipit-branches-compare-url repo base name))
    (require 'shipit-commit-comments-buffer)
    (message "Loading branch view for '%s'..." name)
    (shipit-commit-comments-open-branch repo name base)))

;;; The notification of a new branch

(defun shipit-branches--activity (repo branch)
  "The notification of BRANCH, new in REPO."
  (let-alist branch
    `((repo . ,repo)
      ;; A branch that comes back with another commit is news again.
      (number . ,(format "%s@%s" .name .sha))
      (type . "branch")
      (subject . ,(format "New branch %s" .name))
      (reason . "branch")
      (source . branch)
      (backend-id . branch)
      (branch . ,.name)
      (base . ,.base)
      (browse-url . ,.url)
      (description . ,(format "%s: %s" .author .subject))
      (updated-at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))

(defun shipit-branches--open-pr-p (branch)
  "Whether BRANCH has an open pull request, also a draft."
  (member (alist-get 'state (alist-get 'pr branch)) '("open" "draft")))

(defun shipit-branches--keep-new (repo branches)
  "Keep a notification of each of BRANCHES, new in REPO, that another pushed.
A branch with an open pull request gives none: the pull request gives
one of its own."
  (shipit-branches--set
   :unread (append (mapcar (apply-partially #'shipit-branches--activity repo)
                           (seq-remove (lambda (branch)
                                         (or (alist-get 'own branch)
                                             (shipit-branches--open-pr-p branch)))
                                       branches))
                   (shipit-branches--unread))))

(defun shipit-branches--unread-names (repo)
  "The names of the branches of REPO that an unread notification tells of."
  (cl-loop for activity in (shipit-branches--unread)
           when (equal (alist-get 'repo activity) repo)
           collect (alist-get 'branch activity)))

(defun shipit-branches--forget-with-pr (repo branches)
  "Drop the unread notifications of BRANCHES of REPO with an open pull request.
The pull request gives a notification of its own."
  (let* ((with-pr (mapcar (apply-partially #'alist-get 'name)
                          (seq-filter #'shipit-branches--open-pr-p branches)))
         (dropped (seq-filter (lambda (activity)
                                (and (equal (alist-get 'repo activity) repo)
                                     (member (alist-get 'branch activity) with-pr)))
                              (shipit-branches--unread))))
    (dolist (activity dropped)
      (shipit-branches--mark-read nil activity)
      (let-alist activity
        (shipit--mark-notification-read .number .repo t .type)))
    (when dropped
      (shipit--rerender-notifications-buffer-if-visible))))

(defun shipit-branches--learn (repo answer)
  "Take ANSWER, (ETAG . NAMES), as what was seen of REPO.
The answer `unchanged' teaches nothing."
  (when (consp answer)
    (shipit-branches--remember repo (cdr answer))
    (setf (alist-get repo shipit-branches--etags nil nil #'equal) (car answer))))

(defun shipit-branches--take-names (repo answer done)
  "Keep a notification of each branch of ANSWER that is new in REPO.
ANSWER is (ETAG . NAMES), or `unchanged' when no branch changed.  The
new branches are asked for, and the branches of unread notifications
again, since a pull request is mostly opened after its branch is pushed.
When the backend refuses, nothing is learned, so the next poll asks
again.  DONE is called at the end."
  (let* ((new (and (consp answer) (shipit-branches--new-names repo (cdr answer))))
         (asked (seq-union new (shipit-branches--unread-names repo))))
    (if (null asked)
        (progn (shipit-branches--learn repo answer)
               (funcall done))
      (shipit-branches--fetch-named-async
       repo asked
       (lambda (branches)
         (when branches
           (shipit-branches--forget-with-pr repo branches)
           (shipit-branches--keep-new
            repo (seq-filter (lambda (branch) (member (alist-get 'name branch) new))
                             branches))
           (shipit-branches--learn repo answer))
         (funcall done))))))

(defun shipit-branches--check (repo done)
  "Keep a notification of each new branch of REPO, then call DONE.
The names of the branches are asked for with the ETag that came with
them before, so an answer that they did not change costs little."
  (shipit-branches--fetch-names-async
   repo (alist-get repo shipit-branches--etags nil nil #'equal)
   (lambda (answer)
     (if answer
         (shipit-branches--take-names repo answer done)
       (funcall done)))))

(defun shipit-branches--check-all (done)
  "Check each watched repository for new branches, then call DONE once.
A watched repository whose backend lists no branches is logged and
left out."
  (let ((waiting (length shipit-branches-watched-repos)))
    (cl-flet ((one-done () (when (zerop (cl-decf waiting)) (funcall done))))
      (if (zerop waiting)
          (funcall done)
        (dolist (repo shipit-branches-watched-repos)
          (condition-case err
              (shipit-branches--check repo #'one-done)
            (user-error
             (shipit--debug-log "Branches of %s: %s" repo (error-message-string err))
             (one-done))))))))

;;; The notification source of branches

(defun shipit-branches--notifications-async (_config _since callback)
  "Check the watched repositories for new branches, then call CALLBACK.
CALLBACK gets the notifications of new branches that were not read,
also those of a session before."
  (shipit-branches--check-all
   (lambda () (funcall callback (shipit-branches--unread)))))

(defun shipit-branches--same-p (activity other)
  "Whether ACTIVITY and OTHER are the notification of one branch."
  (and (equal (alist-get 'repo activity) (alist-get 'repo other))
       (equal (alist-get 'number activity) (alist-get 'number other))))

(defun shipit-branches--mark-read (_config activity)
  "Keep ACTIVITY, the notification of a new branch, as unread no longer."
  (shipit-branches--set
   :unread (seq-remove (apply-partially #'shipit-branches--same-p activity)
                       (shipit-branches--unread))))

(defun shipit-branches--open-notification (_config activity)
  "Show how the branch of ACTIVITY differs from the default branch.
To open the notification is to deal with it, so it is marked as read."
  (let-alist activity
    (shipit-branches-open-branch .repo .branch .base)
    (shipit--mark-notification-read .number .repo t .type)
    (shipit--rerender-notifications-buffer-if-visible)))

(defun shipit-branches--autodiscover ()
  "The branches as a backend of notifications, while they can give any.
They can while a repository is watched, or a notification of a new
branch was not read."
  (when (or shipit-branches-watched-repos (shipit-branches--unread))
    (cons "branches" (list :backend 'branch))))

(shipit-issue-register-notification-source
 'branch
 (list :name "Branches"
       :notifications-async #'shipit-branches--notifications-async
       :open-notification #'shipit-branches--open-notification
       :mark-notification-read #'shipit-branches--mark-read
       :autodiscover #'shipit-branches--autodiscover
       :icon-spec '("eye" "octicons" . "#28a745")
       :icon-fallback-text "BR"))

;;; Watching a repository

(defun shipit-branches-watched-p (repo)
  "Whether new branches of REPO give a notification."
  (and (member repo shipit-branches-watched-repos) t))

(defun shipit-branches-toggle-watch (repo branches)
  "Watch REPO when it is not watched, and stop watching it when it is.
The list of watched repositories is saved with Customize.  BRANCHES,
those of REPO that the user sees now, count as seen, so that only a
branch that comes after them gives a notification."
  (customize-save-variable
   'shipit-branches-watched-repos
   (if (shipit-branches-watched-p repo)
       (remove repo shipit-branches-watched-repos)
     (when branches
       (shipit-branches--remember
        repo (mapcar (lambda (branch) (alist-get 'name branch)) branches)))
     (append shipit-branches-watched-repos (list repo)))))

(provide 'shipit-branches)
;;; shipit-branches.el ends here
