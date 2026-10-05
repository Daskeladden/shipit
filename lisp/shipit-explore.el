;;; shipit-explore.el --- The feed of what the people you follow do -*- lexical-binding: t; -*-

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
;; A forge keeps a feed for each user of what the people they follow and
;; the repositories they watch do: someone starred, forked or created a
;; repository, a release came out, a repository went public.  This file
;; asks the backend for that feed, knows which of its entries the user
;; has seen, and can tell of the new ones with one notification:
;;
;;   (setq shipit-explore-notify t)
;;
;; `shipit-explore', in shipit-explore-buffer.el, shows the feed.  An
;; entry is no notification of its own: nothing waits for the user
;; there, and a forge keeps no read state for it.

;;; Code:
(require 'cl-lib)
(require 'format-spec)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-notifications)
(require 'shipit-notifications-external)

(declare-function shipit-explore "shipit-explore-buffer")

(defcustom shipit-explore-kinds '("star" "fork" "create" "release" "public")
  "The kinds of entries of the feed that are shown.
A star, a fork, a new repository, a release, and a repository that was
made public."
  :type '(repeat string)
  :group 'shipit)

(defcustom shipit-explore-about 'all
  "Whose repositories the entries of the feed that are shown are about.
With `mine' they are about the repositories of the user: a star on one
of them, a fork of it.  With `others' they are about the repositories
of others: what the people that the user follows do there.  With `all'
they are about both."
  :type '(choice (const :tag "All repositories" all)
                 (const :tag "The repositories of the user" mine)
                 (const :tag "The repositories of others" others))
  :group 'shipit)

(defcustom shipit-explore-notify nil
  "Whether shipit tells of new entries of the feed with a notification.
The feed is then asked for each time shipit polls for notifications.
There is one notification, which says how many entries are new and
opens Explore."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-explore-seen-file
  (expand-file-name "shipit-explore-seen.el" user-emacs-directory)
  "The file that keeps how far the feed was seen, between two sessions.
With nil it is kept for the session only."
  :type '(choice file (const :tag "Keep it in the session only" nil))
  :group 'shipit)

(defvar shipit-explore--seen nil
  "The time of the newest entry that was seen in Explore, or nil.
It is a time as the backend writes it, and nil until the file is read.")

(defvar shipit-explore--notified nil
  "The time of the newest entry that a notification told of, or nil.")

;;; The feed

(defun shipit-explore--fetch-op ()
  "How to fetch the feed: (FUNCTION . CONFIG) of the backend.
A backend without a feed is a user error."
  (let* ((resolved (shipit-pr--resolve-for-repo (or (shipit--get-repo-from-remote) "")))
         (fetch (plist-get (car resolved) :fetch-feed-async)))
    (unless fetch
      (user-error "This backend has no feed to explore"))
    (cons fetch (cdr resolved))))

(defun shipit-explore--about-p (entry)
  "Whether ENTRY is about a repository that `shipit-explore-about' asks for."
  (pcase shipit-explore-about
    ('all t)
    ('mine (cdr (assq 'own entry)))
    ('others (not (cdr (assq 'own entry))))
    (other (error "%S says nothing of whose repositories to show" other))))

(defun shipit-explore--shown-p (entry)
  "Whether ENTRY is one that the settings ask for.
`shipit-explore-kinds' names its kind, and `shipit-explore-about' whose
repository it is about."
  (and (member (cdr (assq 'kind entry)) shipit-explore-kinds)
       (shipit-explore--about-p entry)))

(defun shipit-explore-fetch-async (callback)
  "Call CALLBACK with the entries of the feed that are shown, the newest first."
  (pcase-let ((`(,fetch . ,config) (shipit-explore--fetch-op)))
    (funcall fetch config
             (lambda (entries)
               (funcall callback (seq-filter #'shipit-explore--shown-p entries))))))

(defconst shipit-explore--sentences
  '(("star" . "%a starred %r")
    ("fork" . "%a forked %r to %d")
    ("create" . "%a created %r")
    ("release" . "%a released %d of %r")
    ("public" . "%a made %r public"))
  "What shipit says of an entry of each kind.
%a is who did it, %r the repository, and %d the detail of the entry.")

(defun shipit-explore-sentence (entry)
  "ENTRY of the feed in words: who did what."
  (format-spec (or (cdr (assoc (cdr (assq 'kind entry)) shipit-explore--sentences))
                   (error "No words for an entry of the kind %S" (cdr (assq 'kind entry))))
               `((?a . ,(cdr (assq 'actor entry)))
                 (?r . ,(cdr (assq 'repo entry)))
                 (?d . ,(or (cdr (assq 'detail entry)) "")))))

;;; What was seen

(defun shipit-explore--read-seen ()
  "The time that `shipit-explore-seen-file' keeps, or nil."
  (let ((file shipit-explore-seen-file))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        ;; A file that holds nothing yet says nothing.
        (and (> (buffer-size) 0)
             (read (current-buffer)))))))

(defun shipit-explore--write-seen ()
  "Write how far the feed was seen to `shipit-explore-seen-file'."
  (when shipit-explore-seen-file
    (with-temp-file shipit-explore-seen-file
      (prin1 shipit-explore--seen (current-buffer)))))

(defun shipit-explore--seen ()
  "The time of the newest entry that was seen, read from the file at first."
  (or shipit-explore--seen
      (setq shipit-explore--seen (shipit-explore--read-seen))))

(defun shipit-explore--newest (entries)
  "The time of the newest of ENTRIES, or nil for none."
  (car (sort (mapcar (lambda (entry) (cdr (assq 'time entry))) entries) #'string>)))

(defun shipit-explore--newer (entries time)
  "Those of ENTRIES that are newer than TIME; all of them when TIME is nil."
  (if time
      (seq-filter (lambda (entry) (string> (cdr (assq 'time entry)) time)) entries)
    entries))

(defun shipit-explore-unseen (entries)
  "Those of ENTRIES that the user has not seen in Explore."
  (shipit-explore--newer entries (shipit-explore--seen)))

(defun shipit-explore-mark-seen (entries)
  "Count ENTRIES, and all that is older, as seen in Explore.
The notification of new entries has done its work then, and goes."
  (let ((newest (shipit-explore--newest entries))
        (seen (shipit-explore--seen)))
    (when (and newest (or (null seen) (string> newest seen)))
      (setq shipit-explore--seen newest)
      (shipit-explore--write-seen)))
  (shipit-explore--drop-notification))

;;; The notification of what is new

(defconst shipit-explore--source "Explore"
  "Where the notification of new entries says that it comes from.")

(defun shipit-explore--notifications ()
  "The notifications of new entries that shipit holds."
  (seq-filter (lambda (activity)
                (and (eq (cdr (assq 'backend-id activity)) 'external)
                     (equal (cdr (assq 'repo activity)) shipit-explore--source)))
              (hash-table-values shipit--notification-pr-activities)))

(defun shipit-explore--drop-notification ()
  "Take the notification of new entries off the notifications of shipit."
  (when-let* ((activities (and shipit--notification-pr-activities
                               (shipit-explore--notifications))))
    (dolist (activity activities)
      (shipit--mark-notification-read (cdr (assq 'number activity))
                                      shipit-explore--source t "external"))
    (shipit--rerender-notifications-buffer-if-visible)))

(defun shipit-explore--open ()
  "Open Explore, as the notification of new entries does."
  (require 'shipit-explore-buffer)
  (shipit-explore))

(defun shipit-explore--notify (entries)
  "Tell of ENTRIES, which are new, with one notification.
It takes the place of one that told of fewer."
  (shipit-explore--drop-notification)
  (shipit-notifications-push
   :source shipit-explore--source
   :title (format "%d new in the feed" (length entries))
   :body (mapconcat #'shipit-explore-sentence entries "\n")
   :open #'shipit-explore--open))

(defun shipit-explore--tell-of-new (entries)
  "Tell of what is new among ENTRIES of the feed.
Nothing is told when no entry came after the last one that was told
of: a notification that was marked as read stays away."
  (let ((unseen (shipit-explore-unseen entries)))
    (when (shipit-explore--newer unseen shipit-explore--notified)
      (setq shipit-explore--notified (shipit-explore--newest unseen))
      (shipit-explore--notify unseen))))

(defun shipit-explore--check ()
  "Ask for the feed and tell of what is new, when `shipit-explore-notify' is on.
This runs each time shipit polls for notifications."
  (when shipit-explore-notify
    (shipit-explore-fetch-async #'shipit-explore--tell-of-new)))

(add-hook 'shipit-notifications-poll-hook #'shipit-explore--check)

(provide 'shipit-explore)
;;; shipit-explore.el ends here
