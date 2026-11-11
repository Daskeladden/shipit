;;; shipit-notifications-external.el --- Notifications that another package gives -*- lexical-binding: t; -*-

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
;; Another package hands a notification to shipit with
;; `shipit-notifications-push'.  It then stands among the notifications
;; of shipit: in the notifications buffer, and in the count of the mode
;; line.  shipit asks no server for it, and knows nothing of the package
;; that gave it.
;;
;;   (shipit-notifications-push
;;    :source "mail.example"
;;    :title "New mail"
;;    :body "From alice"
;;    :open (lambda () (my-show-the-mail)))

;;; Code:
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-notifications)

(defcustom shipit-external-read-limit 50
  "How many pushed notifications that were read shipit keeps.
No server holds a pushed notification, so shipit keeps the newest ones
that were read itself, for the session.  The notifications buffer shows
them when it shows all notifications, and not only the unread."
  :type 'integer
  :group 'shipit)

(defvar shipit-external--read nil
  "The pushed notifications that were read, the one read last first.")

(defvar shipit-external--pushed 0
  "How many notifications were pushed in this session.")

(defun shipit-external--id ()
  "An id for a notification that is pushed now.
No two are the same, so each push is a notification of its own, also
after one with the same title was marked as read."
  (format "%s-%d"
          (format-time-string "%Y%m%dT%H%M%S" nil t)
          (cl-incf shipit-external--pushed)))

(defun shipit-external--activity (notification)
  "NOTIFICATION, a plist of `shipit-notifications-push', as an activity."
  `((repo . ,(plist-get notification :source))
    (number . ,(shipit-external--id))
    (type . "external")
    (subject . ,(plist-get notification :title))
    (reason . ,(or (plist-get notification :reason) "new"))
    (source . external)
    (backend-id . external)
    (browse-url . ,(plist-get notification :url))
    (description . ,(or (plist-get notification :body) ""))
    (open . ,(plist-get notification :open))
    (updated-at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

(defun shipit-external--check (notification)
  "Signal when NOTIFICATION, a plist of `shipit-notifications-push', is not sound."
  (dolist (key '(:source :title))
    (unless (stringp (plist-get notification key))
      (error "shipit-notifications-push: %s must be a string" key)))
  (let ((reason (plist-get notification :reason)))
    (unless (or (null reason) (stringp reason))
      (error "shipit-notifications-push: :reason must be a string"))))

;;;###autoload
(defun shipit-notifications-push (&rest notification)
  "Put NOTIFICATION, from another package, among the notifications of shipit.
NOTIFICATION is a plist:

  :source  where it comes from, a string.  It stands where the
           repository of a notification stands.
  :title   its title, a string.
  :body    its text, shown when the notification is unfolded.
  :open    a function of no arguments that opens it.
  :url     an address that opens it in a browser, when it has no :open.
  :reason  why it came, a string, as GitHub says of its own.  With
           \"mention\" it is counted as a mention.  Without one it
           is \"new\".

The source and the title are needed, the rest may be left out."
  (shipit-external--check notification)
  (shipit--merge-backend-notifications (list (shipit-external--activity notification)))
  (shipit--rerender-notifications-buffer-if-visible))

(defun shipit-external--other-p (activity)
  "A function that says whether a pushed notification is another than ACTIVITY."
  (let ((number (cdr (assq 'number activity))))
    (lambda (other) (not (equal number (cdr (assq 'number other)))))))

(defun shipit-external--keep-read (_config activity)
  "Keep ACTIVITY, a pushed notification that is marked as read.
It is the newest of those that `shipit-external-read-limit' keeps."
  (setq shipit-external--read
        (seq-take (cons activity
                        (seq-filter (shipit-external--other-p activity)
                                    shipit-external--read))
                  shipit-external-read-limit)))

(defun shipit-external--read-notifications ()
  "The pushed notifications that were read, for the view of all notifications."
  shipit-external--read)

(defun shipit-notifications-forget (activity)
  "Keep ACTIVITY, a pushed notification, as read no longer.
The package that pushed it calls this for a notification that it took
back itself, as when a newer one took its place."
  (setq shipit-external--read
        (seq-filter (shipit-external--other-p activity) shipit-external--read)))

(defun shipit-external--open (_config activity)
  "Open ACTIVITY, a pushed notification, and mark it as read.
Its own function opens it, or else a browser at its address.  No server
holds a pushed notification as unread, so to open it is to deal with
it.  One that cannot be opened stays."
  (let ((open (cdr (assq 'open activity)))
        (url (cdr (assq 'browse-url activity))))
    (cond (open (funcall open))
          (url (browse-url url))
          (t (user-error "This notification has nothing to open")))
    (shipit--mark-activity-read-and-rerender activity)))

(shipit-issue-register-notification-source
 'external
 (list :name "External"
       :open-notification #'shipit-external--open
       :mark-notification-read #'shipit-external--keep-read
       :read-notifications #'shipit-external--read-notifications
       :icon-spec '("plug" "octicons" . "#8250df")
       :icon-fallback-text "EX"))

(provide 'shipit-notifications-external)
;;; shipit-notifications-external.el ends here
