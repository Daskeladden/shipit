;;; shipit-notifications-rss.el --- RSS feed notification source -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

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
;; RSS/Atom feed notification source for shipit-notifications.
;; Polls configured feeds and surfaces new entries as notifications
;; in the unified notification buffer.
;;
;; Configuration:
;;   (setq shipit-rss-feeds
;;         '((:url "https://example.com/feed.xml" :name "Example Blog")
;;           (:url "https://other.com/atom.xml" :name "Other Feed")))
;;
;; Then add to shipit-issue-repo-backends:
;;   (add-to-list 'shipit-issue-repo-backends '("rss" :backend rss))

;;; Code:
(require 'cl-lib)
(require 'url)
(require 'xml)
(require 'shipit-core)
(require 'shipit-issue-backends)

(defcustom shipit-rss-feeds nil
  "List of RSS/Atom feeds to poll for notifications.
Each entry is a plist with keys:
  :url  — feed URL (required)
  :name — display name (optional, derived from feed title if omitted)"
  :type '(repeat (plist :key-type keyword :value-type string))
  :group 'shipit)

(defvar shipit-rss--seen-guids (make-hash-table :test 'equal)
  "Hash table of seen feed entry GUIDs to suppress duplicates.")

(defvar shipit-rss--feed-titles (make-hash-table :test 'equal)
  "Hash table mapping feed URLs to their titles (from feed metadata).")

;;; Feed parsing

(defun shipit-rss--fetch-feed (url)
  "Fetch and parse RSS/Atom feed at URL synchronously.
Returns the parsed XML tree, or nil on error."
  (condition-case err
      (let ((url-request-method "GET")
            (url-show-status nil))
        (with-current-buffer (url-retrieve-synchronously url t nil 15)
          (goto-char (point-min))
          (when (search-forward "\n\n" nil t)
            (let ((xml (libxml-parse-xml-region (point) (point-max))))
              (kill-buffer (current-buffer))
              xml))))
    (error
     (shipit--debug-log "RSS fetch error for %s: %s" url (error-message-string err))
     nil)))

(defun shipit-rss--parse-entries (xml url)
  "Extract feed entries from parsed XML tree for feed at URL.
Returns a list of alists with keys: title, link, guid, published, summary."
  (let ((root-tag (car xml)))
    (cond
     ;; Atom feed
     ((eq root-tag 'feed)
      (shipit-rss--cache-feed-title url (shipit-rss--xml-text xml 'title))
      (mapcar #'shipit-rss--parse-atom-entry
              (shipit-rss--xml-children xml 'entry)))
     ;; RSS 2.0 feed
     ((eq root-tag 'rss)
      (let ((channel (shipit-rss--xml-child xml 'channel)))
        (shipit-rss--cache-feed-title url (shipit-rss--xml-text channel 'title))
        (mapcar #'shipit-rss--parse-rss-item
                (shipit-rss--xml-children channel 'item))))
     (t
      (shipit--debug-log "RSS: unknown feed format for %s: %s" url root-tag)
      nil))))

(defun shipit-rss--parse-atom-entry (entry)
  "Parse an Atom entry element into an alist."
  (let ((link-el (cl-find-if (lambda (child)
                               (and (consp child) (eq (car child) 'link)
                                    (equal (cdr (assq 'rel (cadr child))) "alternate")))
                             (cddr entry))))
    `((title . ,(or (shipit-rss--xml-text entry 'title) ""))
      (link . ,(or (when link-el (cdr (assq 'href (cadr link-el))))
                   (shipit-rss--xml-text entry 'link)
                   ""))
      (guid . ,(or (shipit-rss--xml-text entry 'id) ""))
      (published . ,(or (shipit-rss--xml-text entry 'updated)
                        (shipit-rss--xml-text entry 'published)
                        ""))
      (summary . ,(or (shipit-rss--xml-text entry 'summary) "")))))

(defun shipit-rss--parse-rss-item (item)
  "Parse an RSS 2.0 item element into an alist."
  `((title . ,(or (shipit-rss--xml-text item 'title) ""))
    (link . ,(or (shipit-rss--xml-text item 'link) ""))
    (guid . ,(or (shipit-rss--xml-text item 'guid)
                 (shipit-rss--xml-text item 'link)
                 ""))
    (published . ,(or (shipit-rss--xml-text item 'pubDate)
                      (shipit-rss--xml-text item 'date)
                      ""))
    (summary . ,(or (shipit-rss--xml-text item 'description) ""))))

;;; XML helpers

(defun shipit-rss--xml-child (node tag)
  "Return the first child of NODE with TAG."
  (cl-find-if (lambda (child)
                (and (consp child) (eq (car child) tag)))
              (cddr node)))

(defun shipit-rss--xml-children (node tag)
  "Return all children of NODE with TAG."
  (cl-remove-if-not (lambda (child)
                      (and (consp child) (eq (car child) tag)))
                    (cddr node)))

(defun shipit-rss--xml-text (node tag)
  "Return the text content of the first TAG child of NODE."
  (let ((child (shipit-rss--xml-child node tag)))
    (when child
      (let ((text (car (cddr child))))
        (when (stringp text)
          (string-trim text))))))

(defun shipit-rss--cache-feed-title (url title)
  "Cache TITLE for feed URL."
  (when (and title (not (string-empty-p title)))
    (puthash url title shipit-rss--feed-titles)))

;;; Notification conversion

(defun shipit-rss--entry-to-activity (feed-config entry)
  "Convert feed ENTRY to a notification activity alist.
FEED-CONFIG is the feed plist from `shipit-rss-feeds'."
  (let* ((url (plist-get feed-config :url))
         (feed-name (or (plist-get feed-config :name)
                        (gethash url shipit-rss--feed-titles)
                        "RSS"))
         (title (cdr (assq 'title entry)))
         (link (cdr (assq 'link entry)))
         (published (cdr (assq 'published entry))))
    `((repo . ,feed-name)
      (number . ,(cdr (assq 'guid entry)))
      (type . "rss")
      (subject . ,title)
      (reason . "new")
      (source . rss)
      (backend-id . rss)
      (backend-config . ,feed-config)
      (browse-url . ,link)
      (description . ,(cdr (assq 'summary entry)))
      (updated-at . ,(shipit-rss--normalize-date published)))))

(defun shipit-rss--short-id (title published)
  "Generate a short display ID from TITLE and PUBLISHED date."
  (if (and published (not (string-empty-p published)))
      (replace-regexp-in-string
       "T.*" ""
       (shipit-rss--normalize-date published))
    (truncate-string-to-width (or title "") 10 nil nil "...")))

(defun shipit-rss--normalize-date (date-str)
  "Normalize DATE-STR to ISO 8601 format, or return current time."
  (condition-case nil
      (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                          (date-to-time date-str) t)
    (error (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

;;; Notification source interface

(defun shipit-rss--fetch-notifications (_config &optional since)
  "Fetch RSS feed notifications from CONFIG.
SINCE is an ISO 8601 timestamp; entries older than this are skipped.
On first poll (SINCE is nil), defaults to 1 hour ago."
  (let* ((cutoff (float-time
                  (if since
                      (date-to-time since)
                    (time-subtract nil (* 60 60)))))
         (all-activities nil))
    (dolist (feed shipit-rss-feeds)
      (let* ((url (plist-get feed :url))
             (xml (shipit-rss--fetch-feed url))
             (entries (when xml (shipit-rss--parse-entries xml url))))
        (shipit--debug-log "RSS: %s returned %d entries (seen-guids: %d)"
                           url (length entries)
                           (hash-table-count shipit-rss--seen-guids))
        (dolist (entry entries)
          (let* ((guid (cdr (assq 'guid entry)))
                 (published (cdr (assq 'published entry)))
                 (entry-time (condition-case nil
                                 (float-time (date-to-time published))
                               (error 0))))
            (when (>= entry-time cutoff)
              (unless (gethash guid shipit-rss--seen-guids)
                (puthash guid t shipit-rss--seen-guids))
              (push (shipit-rss--entry-to-activity feed entry) all-activities))))))
    (shipit--debug-log "RSS: returning %d new activities (cutoff: %s)"
                       (length all-activities)
                       (or since "1h ago"))
    (nreverse all-activities)))

(defun shipit-rss--mark-read (_config _activity)
  "Mark RSS notification ACTIVITY as read (local tracking only)."
  ;; No remote API to mark read — handled by the global locally-marked-read mechanism
  t)

;;; Backend registration

(defun shipit-rss--not-supported (&rest _args)
  "Stub for unsupported issue backend operations."
  (error "RSS backend is notification-only"))

(shipit-issue-register-backend
 'rss
 (list :name "RSS"
       :notification-only t
       :fetch-issue #'shipit-rss--not-supported
       :fetch-comments #'shipit-rss--not-supported
       :fetch-comments-async #'shipit-rss--not-supported
       :search #'shipit-rss--not-supported
       :create-issue #'shipit-rss--not-supported
       :reference-patterns (lambda () nil)
       :browse-url #'shipit-rss--not-supported
       :id-to-string #'identity
       :string-to-id #'identity
       :notifications #'shipit-rss--fetch-notifications
       :mark-notification-read #'shipit-rss--mark-read
       :icon-spec '("rss" "simple" . "#ee802f")
       :icon-fallback-text "RS"))

;; Use autodiscover so the backend gets polled when feeds are configured.
;; Returns a single ("rss" . config) entry when shipit-rss-feeds is set.
(let ((backend (cdr (assq 'rss shipit-issue-backends))))
  (when backend
    (plist-put backend :autodiscover
               (lambda ()
                 (when shipit-rss-feeds
                   (cons "rss" (list :backend 'rss)))))))

(provide 'shipit-notifications-rss)
;;; shipit-notifications-rss.el ends here
