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
    (reason . "new")
    (source . external)
    (backend-id . external)
    (browse-url . ,(plist-get notification :url))
    (description . ,(or (plist-get notification :body) ""))
    (open . ,(plist-get notification :open))
    (updated-at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

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

The source and the title are needed, the rest may be left out."
  (dolist (key '(:source :title))
    (unless (stringp (plist-get notification key))
      (error "shipit-notifications-push: %s must be a string" key)))
  (shipit--merge-backend-notifications (list (shipit-external--activity notification)))
  (shipit--rerender-notifications-buffer-if-visible))

(defun shipit-external--mark-read (activity)
  "Take ACTIVITY, a pushed notification, off the notifications of shipit."
  (shipit--mark-notification-read
   (cdr (assq 'number activity)) (cdr (assq 'repo activity)) t "external")
  (shipit--rerender-notifications-buffer-if-visible))

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
    (shipit-external--mark-read activity)))

(shipit-issue-register-notification-source
 'external
 (list :name "External"
       :open-notification #'shipit-external--open
       :icon-spec '("plug" "octicons" . "#8250df")
       :icon-fallback-text "EX"))

(provide 'shipit-notifications-external)
;;; shipit-notifications-external.el ends here
