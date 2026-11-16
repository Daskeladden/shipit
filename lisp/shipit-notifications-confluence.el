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
;;
;; The command `shipit-confluence-search' asks the same search for the
;; pages that hold some words, with `shipit-confluence--find-pages'.

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
  "How many pages one search of the notifications asks for.")

(defconst shipit-confluence--content-types "type in (page, blogpost)"
  "The CQL condition that keeps the pages and the blog posts.
So the search gives no comment and no attachment.")

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

(defun shipit-confluence--configs ()
  "The config of each Confluence site in `shipit-issue-repo-backends'."
  (cl-loop for (_pattern . config) in shipit-issue-repo-backends
           when (eq (plist-get config :backend) 'confluence)
           collect config))

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

;; url.el sets it in the buffer of each response.
(defvar url-http-response-status)

(defun shipit-confluence--answer-in (buffer)
  "The answer in BUFFER, a response of a Confluence site, as (STATUS . JSON).
JSON is nil when the body is no JSON."
  (with-current-buffer buffer
    (let ((status (and (boundp 'url-http-response-status) url-http-response-status)))
      (unless (integerp status)
        (error "shipit: the answer of Confluence has no HTTP status"))
      (shipit-issue-jira--goto-response-body)
      (shipit-issue-jira--decode-response-body)
      (cons status (shipit-issue-jira--read-json "Confluence")))))

(defun shipit-confluence--get (config path)
  "Send a GET of PATH to the site of CONFIG, and wait for the answer.
Returns the answer as (STATUS . JSON), or nil when no answer came.  A
site without a login signals the error of the Jira backend."
  (shipit-issue-jira--request "GET" config path
                              '(("Content-Type" . "application/json")) nil
                              #'shipit-confluence--answer-in))

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
  (format "%s and %s and lastmodified >= now(\"-%dm\")"
          condition shipit-confluence--content-types minutes))

(defun shipit-confluence--search-address (cql &optional limit)
  "The path and query of a search for CQL that asks for LIMIT pages.
LIMIT is `shipit-confluence--result-limit' when it is nil."
  (format "%s?cql=%s&limit=%d&expand=content.version,content.space"
          shipit-confluence--search-path (url-hexify-string cql)
          (or limit shipit-confluence--result-limit)))

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

(defun shipit-confluence--complete-p (page keys)
  "Whether PAGE, an alist, has text at each of KEYS.
When it has not, the debug log names the keys that it lacks."
  (let ((missing (seq-remove (lambda (key) (stringp (alist-get key page))) keys)))
    (when missing
      (shipit--debug-log "Confluence: leaving out a page without %S: %S" missing page))
    (null missing)))

(defun shipit-confluence--activities (config account-id reason answer)
  "The notifications of the pages in ANSWER, a search at the site of CONFIG.
REASON is why the search found them.  ACCOUNT-ID is the user, or nil
when the site did not say who the user is.  A page without the space,
the id, the title or the address that its row shows is left out."
  (shipit-confluence--log-if-cut answer)
  (let ((base (shipit-confluence--base config answer)))
    (seq-filter
     (lambda (activity)
       (shipit-confluence--complete-p activity '(repo number subject browse-url)))
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

;;; The pages that hold some words

(defun shipit-confluence--cql-text (text)
  "TEXT as a string of CQL, in double quotes.
Each backslash and each double quote of TEXT gets a backslash before it."
  (concat "\""
          (replace-regexp-in-string "[\\\"]" (lambda (char) (concat "\\" char)) text t t)
          "\""))

(defun shipit-confluence--words-cql (words)
  "The CQL of the pages and blog posts whose title or text holds WORDS.
This is the search of the site, which gives the best match first.  The
search reads WORDS with its own syntax, so a phrase in double quotes
stays a phrase."
  (format "siteSearch ~ %s and %s"
          (shipit-confluence--cql-text words) shipit-confluence--content-types))

(defun shipit-confluence--page (base result)
  "The page of RESULT, a result of a search, as an alist.
BASE is the address that the path of the page starts from.  The title
is the one of the content, because the title of the result itself holds
the marks with which the search shows the words that it found.  The
`editor' is the name of the user who made the last change, and the
`excerpt' is the text around the words found, without those marks."
  (let-alist result
    `((id . ,.content.id)
      (title . ,.content.title)
      (space . ,(or .content.space.name .resultGlobalContainer.title))
      (url . ,(and .content._links.webui (concat base .content._links.webui)))
      (changed . ,.lastModified)
      (editor . ,.content.version.by.displayName)
      (excerpt . ,(and .excerpt (shipit-confluence--without-highlight .excerpt))))))

(defun shipit-confluence--pages (config answer)
  "The pages in ANSWER, the JSON of a search at the site of CONFIG.
A page without an id, a title or an address is left out."
  (let ((base (shipit-confluence--base config answer)))
    (seq-filter (lambda (page) (shipit-confluence--complete-p page '(id title url)))
                (mapcar (lambda (result) (shipit-confluence--page base result))
                        (alist-get 'results answer)))))

(defun shipit-confluence--check-search (answer)
  "Signal unless ANSWER, the (STATUS . JSON) of a search, can give pages.
ANSWER is nil when the site did not answer."
  (unless answer
    (user-error "Confluence did not answer the search"))
  (when (>= (car answer) 400)
    (user-error "Confluence refused the search (%d)" (car answer)))
  (unless (consp (cdr answer))
    (user-error "Confluence answered the search with no JSON")))

(defun shipit-confluence--find-pages (config words limit)
  "The pages and blog posts of the site of CONFIG that hold WORDS.
WORDS can be in the title or in the text.  The pages are at most LIMIT,
in the order of Confluence, the best match first.  Each is an alist of
its `id', `title', `space', `url', and `changed', the time of its last
change.  A search that fails signals a `user-error'."
  (let ((answer (shipit-confluence--get
                 config (shipit-confluence--search-address
                         (shipit-confluence--words-cql words) limit))))
    (shipit-confluence--check-search answer)
    (shipit-confluence--pages config (cdr answer))))

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
