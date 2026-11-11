;;; shipit-notifications-confluence.el --- Notifications from Confluence Cloud -*- lexical-binding: t; -*-

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
;; A page or a blog post of Confluence Cloud that mentions the user, or
;; that the user watches, is a notification of shipit when it changes.
;; Each poll asks the search of Confluence for the pages that changed
;; since the poll before.
;;
;;   (add-to-list 'shipit-issue-repo-backends
;;                '("confluence" :backend confluence
;;                  :base-url "https://example.atlassian.net"))
;;
;; The login is the entry of auth-source for the host of `:base-url',
;; the same one that Jira uses there.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'parse-time)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-notifications)
;; Confluence Cloud is on the site of Jira, under /wiki, and takes the
;; same login.  So the requests of the Jira backend serve it as they are.
(require 'shipit-issue-jira)

(defconst shipit-confluence--search-path "/wiki/rest/api/search"
  "The path of the search of Confluence.")

(defconst shipit-confluence--user-path "/wiki/rest/api/user/current"
  "The path at which Confluence says who the user is.")

(defconst shipit-confluence--result-limit 50
  "How many pages one search asks for.")

(defconst shipit-confluence--first-look-back 60
  "How many minutes the first poll of a session looks back.")

(defconst shipit-confluence--look-back-margin 5
  "How many minutes a poll looks back beyond the poll before.
So a change made close to the poll before is not missed.")

(defconst shipit-confluence--mentions
  '("mention = currentUser()" . "mention")
  "The pages that mention the user: the condition of their search in CQL,
and the reason of their notifications.")

(defconst shipit-confluence--watched
  '("watcher = currentUser()" . "subscribed")
  "The pages that the user watches: the condition of their search in CQL,
and the reason of their notifications.")

(defvar shipit-confluence--account-ids (make-hash-table :test 'equal)
  "The account id of the user at each Confluence site, by its address.")

(defun shipit-confluence-reset ()
  "Forget what the source of Confluence learned in this session.
That is the account id of the user at each site, which the next poll
asks for again."
  (clrhash shipit-confluence--account-ids))

;;; The site

(defun shipit-confluence--site (config)
  "The address of the Confluence site of CONFIG, without a slash at its end."
  (string-trim-right (plist-get config :base-url) "/+"))

(defun shipit-confluence--check-config (config)
  "Signal when CONFIG names no Confluence site."
  (unless (stringp (plist-get config :base-url))
    (user-error "The Confluence backend needs :base-url, the address of the site")))

;;; Requests

(defun shipit-confluence--send (config path callback)
  "Send a GET of PATH to the site of CONFIG, and call CALLBACK with the answer.
CALLBACK gets the JSON of the answer, or nil when the request failed.
A request that cannot be sent, as one without a login, calls CALLBACK
with nil at once.  This signals nothing."
  (let ((answered nil))
    (condition-case err
        (shipit-issue-jira--api-request-async
         config path (lambda (json)
                       (setq answered t)
                       (funcall callback json)))
      (error
       (shipit--debug-log "Confluence: GET %s failed: %s" path (error-message-string err))
       ;; When the answer came before `url-retrieve' returned, the error
       ;; came from CALLBACK, which must not run twice.
       (unless answered
         (funcall callback nil))))))

(defun shipit-confluence--read-safely (read answer path)
  "What READ makes of ANSWER, the JSON of the answer to PATH, or nil.
The answers come outside the poll, where an error would end the poll
before it gives its notifications.  So an answer that READ cannot read
counts as no answer."
  (condition-case err
      (funcall read answer)
    (error
     (shipit--debug-log "Confluence: cannot read the answer to %s: %s"
                        path (error-message-string err))
     nil)))

(defun shipit-confluence--ask (config path read callback)
  "Ask the site of CONFIG for PATH, and call CALLBACK with what READ makes of it.
READ gets the JSON of the answer, or nil when the request failed.
CALLBACK gets nil when the request failed or READ signalled."
  (shipit-confluence--send
   config path
   (lambda (answer)
     (funcall callback (shipit-confluence--read-safely read answer path)))))

;;; Who the user is

(defun shipit-confluence--account-id-of (answer)
  "The account id in ANSWER, the JSON of the user, or nil."
  (let ((id (and (consp answer) (alist-get 'accountId answer))))
    (and (stringp id) id)))

(defun shipit-confluence--with-account-id (config callback)
  "Call CALLBACK with the account id of the user at the site of CONFIG.
The site is asked once in a session.  CALLBACK gets nil when the site
did not say, and the next poll asks again."
  (let* ((site (shipit-confluence--site config))
         (known (gethash site shipit-confluence--account-ids)))
    (if known
        (funcall callback known)
      (shipit-confluence--ask
       config shipit-confluence--user-path #'shipit-confluence--account-id-of
       (lambda (id)
         (when id
           (puthash site id shipit-confluence--account-ids))
         (funcall callback id))))))

;;; The searches

(defun shipit-confluence--minutes-back (since)
  "How many minutes a poll looks back, when the poll before was at SINCE.
SINCE is an ISO 8601 time, or nil when there was no poll before in this
session."
  (if since
      (let ((seconds (float-time (time-subtract nil (parse-iso8601-time-string since)))))
        (+ (max 0 (ceiling seconds 60)) shipit-confluence--look-back-margin))
    shipit-confluence--first-look-back))

(defun shipit-confluence--cql (condition minutes)
  "The CQL of the pages and blog posts with CONDITION, changed in MINUTES.
The time is relative to now on purpose: Confluence reads an absolute
time in the time zone of the profile of the user."
  (format "%s and type in (page, blogpost) and lastmodified >= now(\"-%dm\")"
          condition minutes))

(defun shipit-confluence--search-address (cql)
  "The path and query of a search for CQL."
  (format "%s?cql=%s&limit=%d&expand=content.version,content.space"
          shipit-confluence--search-path (url-hexify-string cql)
          shipit-confluence--result-limit))

(defun shipit-confluence--log-if-cut (answer)
  "Write to the debug log when ANSWER of a search lacks pages that it found.
No later request asks for the rest."
  (let ((total (alist-get 'totalSize answer))
        (given (length (alist-get 'results answer))))
    (when (and (numberp total) (> total given))
      (shipit--debug-log "Confluence: the search found %d pages and gave %d" total given))))

;;; Notifications

(defun shipit-confluence--base (config answer)
  "The address that the paths of the pages in ANSWER start from.
Confluence says it in the answer.  Without it, it is the wiki of the
site of CONFIG."
  (or (let-alist answer ._links.base)
      (concat (shipit-confluence--site config) "/wiki")))

(defun shipit-confluence--without-highlight (text)
  "TEXT without the marks with which the search shows the words it found."
  (replace-regexp-in-string "@@@\\(?:end\\)?hl@@@" "" text t t))

(defun shipit-confluence--description (message excerpt)
  "The text of a notification: the MESSAGE of the change, then EXCERPT.
An empty line is between them.  Either can be missing."
  (string-join (seq-filter (lambda (text) (and (stringp text) (not (string-empty-p text))))
                           (list message
                                 (and excerpt (shipit-confluence--without-highlight excerpt))))
               "\n\n"))

(defun shipit-confluence--reason (result account-id reason)
  "The reason of the notification of RESULT, a page that a search found.
It is \"your_activity\" when ACCOUNT-ID, the user, made the last change
of the page, and REASON, why the search found it, otherwise."
  (if (and account-id
           (equal account-id (let-alist result .content.version.by.accountId)))
      "your_activity"
    reason))

(defun shipit-confluence--activity (config base result reason)
  "The notification of RESULT, a page at the site of CONFIG, for REASON.
BASE is the address that the path of the page starts from."
  (let-alist result
    `((repo . ,(or .content.space.name .resultGlobalContainer.title))
      (number . ,.content.id)
      (type . "confluence")
      (subject . ,.content.title)
      (reason . ,reason)
      (source . confluence)
      (backend-id . confluence)
      (backend-config . ,config)
      (author . ,.content.version.by.displayName)
      (browse-url . ,(concat base .content._links.webui))
      (description . ,(shipit-confluence--description .content.version.message .excerpt))
      (revision . ,.content.version.number)
      (updated-at . ,.content.version.when))))

(defun shipit-confluence--complete-p (activity)
  "Whether ACTIVITY has what a row of the notifications buffer shows.
One without is left out, and the debug log says so."
  (or (cl-every (lambda (key) (stringp (alist-get key activity)))
                '(repo number subject browse-url))
      (progn
        (shipit--debug-log "Confluence: leaving out a page without a space, id or title: %S"
                           activity)
        nil)))

(defun shipit-confluence--activities (config account-id reason answer)
  "The notifications of the pages in ANSWER, a search at the site of CONFIG.
REASON is why the search found them.  ACCOUNT-ID is the user, or nil
when the site did not say who the user is."
  (shipit-confluence--log-if-cut answer)
  (let ((base (shipit-confluence--base config answer)))
    (seq-filter
     #'shipit-confluence--complete-p
     (mapcar (lambda (result)
               (shipit-confluence--activity
                config base result (shipit-confluence--reason result account-id reason)))
             (alist-get 'results answer)))))

(defun shipit-confluence--search (config account-id minutes search callback)
  "Search the site of CONFIG for the pages of SEARCH changed in MINUTES.
SEARCH is `shipit-confluence--mentions' or `shipit-confluence--watched'.
CALLBACK gets their notifications, none when the search failed.
ACCOUNT-ID is the user, or nil."
  (shipit-confluence--ask
   config (shipit-confluence--search-address
           (shipit-confluence--cql (car search) minutes))
   (lambda (answer)
     (shipit-confluence--activities config account-id (cdr search) answer))
   callback))

(defun shipit-confluence--combine (mentioned watched)
  "The notifications MENTIONED, then each of WATCHED that is not among them.
A page that mentions the user and that the user watches is one
notification, a mention."
  (let ((ids (mapcar (lambda (activity) (alist-get 'number activity)) mentioned)))
    (append mentioned
            (seq-remove (lambda (activity) (member (alist-get 'number activity) ids))
                        watched))))

(defun shipit-confluence--search-both (config account-id minutes callback)
  "Search the site of CONFIG for the pages that changed in MINUTES.
First for the pages that mention the user, then for those that the user
watches.  CALLBACK gets the notifications of both.  ACCOUNT-ID is the
user, or nil."
  (shipit-confluence--search
   config account-id minutes shipit-confluence--mentions
   (lambda (mentioned)
     (shipit-confluence--search
      config account-id minutes shipit-confluence--watched
      (lambda (watched)
        (funcall callback (shipit-confluence--combine mentioned watched)))))))

(defun shipit-confluence--notifications-async (config since callback)
  "Ask the Confluence site of CONFIG for the pages that changed since SINCE.
SINCE is the time of the poll before, in ISO 8601, or nil for the first
poll of the session.  CALLBACK gets the notifications once, when the
site has answered.  A request that fails gives no notification, and
nothing signals once the first request is sent."
  (shipit-confluence--check-config config)
  (let ((minutes (shipit-confluence--minutes-back since)))
    (shipit-confluence--with-account-id
     config
     (lambda (account-id)
       (shipit-confluence--search-both config account-id minutes callback)))))

;;; Opening

(defun shipit-confluence--open (_config activity)
  "Open the page of ACTIVITY in the browser, and mark the notification as read.
Confluence keeps no notification as unread, so to open a page is to
deal with it."
  (browse-url (alist-get 'browse-url activity))
  (shipit--mark-activity-read-and-rerender activity))

;;; Backend registration

(shipit-issue-register-notification-source
 'confluence
 (list :name "Confluence"
       :notifications-async #'shipit-confluence--notifications-async
       :open-notification #'shipit-confluence--open
       ;; Confluence keeps no read state, so there is nothing to tell it.
       :mark-notification-read #'ignore
       :icon-spec '("confluence" "simple" . "#172B4D")
       :icon-fallback-text "CF"))

(provide 'shipit-notifications-confluence)
;;; shipit-notifications-confluence.el ends here
