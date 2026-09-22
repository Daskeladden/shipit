;;; shipit-issue-jira.el --- Jira issue backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

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
;; Jira issue backend for the pluggable issue system.
;; Transforms Jira REST API v3 responses into the normalized alist format
;; expected by shipit UI code.
;;
;; Config plist keys:
;;   :base-url      — Jira instance URL (e.g. "https://jira.example.com")
;;   :project-keys  — list of project key prefixes (e.g. ("PRJ" "TEAM"))
;;
;; Authentication:
;;   Credentials are looked up from auth-source using the :base-url hostname.
;;   Add an entry to ~/.authinfo.gpg:
;;     machine jira.example.com login you@example.com password your-api-token
;;   Atlassian Cloud uses Basic auth: base64(email:api-token).

;;; Code:

(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'transient)
(require 'auth-source)
(require 'url)
(require 'json)

;; Forward declarations
(declare-function shipit--svg-lib-icon-with-bg "shipit-render")

;;; Customization

(defcustom shipit-jira-work-item-columns '(work assignee status)
  "Columns to display in Jira child/linked work item sections.
Each element is a symbol: `work' (key + summary), `assignee',
`status', `issue-type', or `key' (key only, no summary)."
  :type '(repeat (choice (const :tag "Work (key + summary)" work)
                         (const :tag "Assignee" assignee)
                         (const :tag "Status" status)
                         (const :tag "Issue Type" issue-type)
                         (const :tag "Key only" key)))
  :group 'shipit)

(defcustom shipit-jira-dashboard-columns '(issue-type-icon priority-icon work status)
  "Columns to display in Jira dashboard issue lines.
Each element is a symbol: `issue-type-icon' (SVG icon),
`priority-icon' (SVG icon), `work' (key + summary),
`status', `assignee', `relative-time'."
  :type '(repeat (choice (const :tag "Issue Type Icon" issue-type-icon)
                         (const :tag "Priority Icon" priority-icon)
                         (const :tag "Work (key + summary)" work)
                         (const :tag "Status" status)
                         (const :tag "Assignee" assignee)
                         (const :tag "Relative Time" relative-time)))
  :group 'shipit)

;;; Column configuration transient

(defun shipit-jira--toggle-column (col)
  "Toggle COL in `shipit-jira-work-item-columns'."
  (if (memq col shipit-jira-work-item-columns)
      (setq shipit-jira-work-item-columns
            (delq col shipit-jira-work-item-columns))
    (setq shipit-jira-work-item-columns
          (append shipit-jira-work-item-columns (list col))))
  (customize-save-variable 'shipit-jira-work-item-columns
                           shipit-jira-work-item-columns)
  (message "Jira columns: %s" shipit-jira-work-item-columns))

(defun shipit-jira--toggle-column-work ()
  "Toggle work column."
  (interactive)
  (shipit-jira--toggle-column 'work))

(defun shipit-jira--toggle-column-assignee ()
  "Toggle assignee column."
  (interactive)
  (shipit-jira--toggle-column 'assignee))

(defun shipit-jira--toggle-column-status ()
  "Toggle status column."
  (interactive)
  (shipit-jira--toggle-column 'status))

(defun shipit-jira--toggle-column-type ()
  "Toggle issue-type column."
  (interactive)
  (shipit-jira--toggle-column 'issue-type))

(defun shipit-jira--toggle-column-key ()
  "Toggle key column."
  (interactive)
  (shipit-jira--toggle-column 'key))

(transient-define-prefix shipit-jira-columns-transient ()
  "Configure Jira work item columns."
  ["Columns (toggle on/off)"
   ("w" shipit-jira--toggle-column-work
    :description (lambda ()
                   (format "Work (key + summary): %s"
                           (if (memq 'work shipit-jira-work-item-columns) "on" "off")))
    :transient t)
   ("a" shipit-jira--toggle-column-assignee
    :description (lambda ()
                   (format "Assignee: %s"
                           (if (memq 'assignee shipit-jira-work-item-columns) "on" "off")))
    :transient t)
   ("s" shipit-jira--toggle-column-status
    :description (lambda ()
                   (format "Status: %s"
                           (if (memq 'status shipit-jira-work-item-columns) "on" "off")))
    :transient t)
   ("t" shipit-jira--toggle-column-type
    :description (lambda ()
                   (format "Issue type: %s"
                           (if (memq 'issue-type shipit-jira-work-item-columns) "on" "off")))
    :transient t)
   ("k" shipit-jira--toggle-column-key
    :description (lambda ()
                   (format "Key only: %s"
                           (if (memq 'key shipit-jira-work-item-columns) "on" "off")))
    :transient t)]
  ["Exit"
   ("q" "Quit" transient-quit-one)])

;;; Mention resolution — Jira @mentions → GitHub usernames

(defvar shipit-issue-jira--mention-resolver nil
  "When non-nil, called as (funcall resolver account-id display-name).
Should return \"@githubUsername\" or nil.  Dynamically bound by callers
that have Jira config in scope.")

(defun shipit-issue-jira--resolve-mention (config account-id display-name)
  "Resolve Jira ACCOUNT-ID to a GitHub @username using CONFIG.
DISPLAY-NAME is the Jira display name (used as fallback context).
Returns \"@githubUsername\" or nil.

Resolution order:
1. Persistent cache hit → return immediately
2. Jira API: GET /rest/api/3/user → extract emailAddress
3. Try noreply email pattern (no GitHub API call needed)
4. GitHub search API: /search/users with email
5. Cache result (including nil for negative cache)"
  (require 'shipit-gh-etag)
  (shipit-gh-etag--ensure-cache-loaded)
  (let ((cache-key (concat "jira-user:" account-id)))
    ;; Check cache first
    (let ((cached (gethash cache-key shipit-gh-etag--persistent-cache 'miss)))
      (cond
       ((eq cached 'miss)
        (shipit-issue-jira--resolve-mention-via-api config account-id cache-key))
       ((eq cached :negative)
        nil)
       (t
        (concat "@" cached))))))

(defun shipit-issue-jira--resolve-mention-via-api (config account-id cache-key)
  "Resolve ACCOUNT-ID via Jira + GitHub APIs, cache under CACHE-KEY.
CONFIG is the Jira backend config plist."
  (let* ((path (format "/rest/api/3/user?accountId=%s"
                       (url-hexify-string account-id)))
         (user-data (shipit-issue-jira--api-request config path))
         (email (cdr (assq 'emailAddress user-data)))
         (username (when email
                     (or (shipit--extract-username-from-email email)
                         (shipit-issue-jira--search-github-user-by-email email)))))
    ;; Cache result (positive or negative)
    (puthash cache-key (or username :negative) shipit-gh-etag--persistent-cache)
    (shipit-gh-etag--save-cache)
    (shipit--debug-log "Jira mention: %s → %s (email=%s)"
                       account-id (or username "nil") (or email "nil"))
    (when username
      (concat "@" username))))

(defun shipit-issue-jira--search-github-user-by-email (email)
  "Search GitHub for a user with EMAIL.  Return username if exactly one match."
  (let ((result (shipit--api-request "/search/users"
                                     `(("q" ,(concat email " in:email"))))))
    (when (and result
               (= 1 (cdr (assq 'total_count result))))
      (let ((items (append (cdr (assq 'items result)) nil)))
        (cdr (assq 'login (car items)))))))

;;; ADF (Atlassian Document Format) → plain text

(defvar shipit-issue-jira--attachments nil
  "Attachments of the issue whose text is being converted.
Bound while an issue or its comments are normalized.  An image in the
text names its file only by filename, which is the one link the REST
API gives from an image to the attachment holding it.")

(defun shipit-issue-jira--attachment-named (filename)
  "Return the attachment called FILENAME, or nil.
It is looked for in `shipit-issue-jira--attachments'.  Two attachments
can share a filename, and then the first one is taken."
  (and filename
       (cl-find filename shipit-issue-jira--attachments
                :key (lambda (attachment) (cdr (assq 'filename attachment)))
                :test #'equal)))

(defun shipit-issue-jira--attachment-url (attachment)
  "Return the URL that downloads the content of ATTACHMENT.
Jira answers the content URL with a redirect to its media host unless
asked not to, and the login header would follow the redirect there."
  (concat (cdr (assq 'content attachment)) "?redirect=false"))

(defun shipit-issue-jira--adf-media (attrs)
  "Return an ADF media node with ATTRS as markdown.
An image hosted elsewhere keeps its own URL.  A file attached to the
issue is found by its filename: an image becomes a markdown image and
any other file a link.  A file with no such attachment shows its name."
  (let* ((name (or (cdr (assq 'alt attrs)) "image"))
         (external (and (equal (cdr (assq 'type attrs)) "external")
                        (cdr (assq 'url attrs))))
         (attachment (and (not external)
                          (shipit-issue-jira--attachment-named (cdr (assq 'alt attrs))))))
    (cond
     (external (format "![%s](%s)" name external))
     ((null attachment) (format "[image: %s]" name))
     ((string-prefix-p "image/" (or (cdr (assq 'mimeType attachment)) ""))
      (format "![%s](%s)" name (shipit-issue-jira--attachment-url attachment)))
     (t (format "[%s](%s)" name (shipit-issue-jira--attachment-url attachment))))))

(defun shipit-issue-jira--adf-apply-marks (text node)
  "Apply ADF marks from NODE to TEXT.
Handles link marks by formatting as [text](url)."
  (let* ((marks (append (cdr (assq 'marks node)) nil))
         (link-mark (cl-find-if (lambda (m) (equal (cdr (assq 'type m)) "link"))
                                marks)))
    (if link-mark
        (let ((href (cdr (assq 'href (cdr (assq 'attrs link-mark))))))
          (if href
              (format "[%s](%s)" text href)
            text))
      text)))

(defun shipit-issue-jira--adf-to-text (node &optional depth)
  "Convert an Atlassian Document Format NODE to plain text.
NODE is a parsed JSON alist with `type' and optional `content' keys.
DEPTH tracks nesting level for list indentation (default 0)."
  (let ((depth (or depth 0)))
    (cond
     ((stringp node) node)
     ((not (listp node)) "")
     (t
      (let ((type (cdr (assq 'type node)))
            (text (cdr (assq 'text node)))
            (content (cdr (assq 'content node)))
            (attrs (cdr (assq 'attrs node))))
        (cond
         ((equal type "hardBreak") "\n")
         ((equal type "inlineCard")
          (or (cdr (assq 'url attrs)) ""))
         ((member type '("media" "mediaInline"))
          (shipit-issue-jira--adf-media attrs))
         ((equal type "mention")
          (let* ((mention-text (or (cdr (assq 'text attrs)) ""))
                 (account-id (cdr (assq 'id attrs)))
                 (display-name (string-remove-prefix "@" mention-text)))
            (or (when (and shipit-issue-jira--mention-resolver account-id)
                  (funcall shipit-issue-jira--mention-resolver
                           account-id display-name))
                (propertize display-name
                            'shipit-jira-mention t
                            'shipit-jira-display-name display-name
                            'shipit-jira-account-id account-id))))
         (text (shipit-issue-jira--adf-apply-marks text node))
         ((equal type "bulletList")
          (shipit-issue-jira--adf-bullet-list content depth))
         ((equal type "orderedList")
          (shipit-issue-jira--adf-ordered-list content depth))
         ((equal type "listItem")
          (shipit-issue-jira--adf-list-item content depth))
         ((equal type "heading")
          (let* ((level (or (cdr (assq 'level attrs)) 1))
                 (prefix (make-string level ?#))
                 (children (mapconcat
                            (lambda (c) (shipit-issue-jira--adf-to-text c depth))
                            (append content nil) "")))
            (concat "\n" prefix " " children "\n\n")))
         ((equal type "taskItem")
          (let* ((state (cdr (assq 'state attrs)))
                 (checkbox (if (equal state "DONE") "- [x] " "- [ ] "))
                 (children (mapconcat
                            (lambda (c) (shipit-issue-jira--adf-to-text c depth))
                            (append content nil) "")))
            (concat checkbox children "\n")))
         ((equal type "codeBlock")
          (let ((children (mapconcat
                           (lambda (c) (shipit-issue-jira--adf-to-text c depth))
                           (append content nil) "")))
            (concat "```\n" children "\n```\n")))
         (content
          (let ((children (mapconcat
                           (lambda (c) (shipit-issue-jira--adf-to-text c depth))
                           (append content nil) "")))
            (if (member type '("paragraph" "blockquote" "taskList" "mediaSingle" "mediaGroup"))
                (concat children "\n")
              children)))
         (t "")))))))

(defun shipit-issue-jira--adf-bullet-list (content depth)
  "Render ADF bulletList CONTENT at nesting DEPTH.
Top-level lists (depth 0) get blank lines before and after."
  (let ((body (mapconcat
               (lambda (item)
                 (concat (make-string (* depth 2) ?\s)
                         "- "
                         (shipit-issue-jira--adf-to-text item (1+ depth))))
               (append content nil) "")))
    (if (= depth 0)
        (concat "\n" body "\n")
      body)))

(defun shipit-issue-jira--adf-ordered-list (content depth)
  "Render ADF orderedList CONTENT at nesting DEPTH.
Top-level lists (depth 0) get blank lines before and after."
  (let* ((idx 0)
         (body (mapconcat
                (lambda (item)
                  (setq idx (1+ idx))
                  (concat (make-string (* depth 2) ?\s)
                          (format "%d. " idx)
                          (shipit-issue-jira--adf-to-text item (1+ depth))))
                (append content nil) "")))
    (if (= depth 0)
        (concat "\n" body "\n")
      body)))

(defun shipit-issue-jira--adf-list-item (content depth)
  "Render ADF listItem CONTENT at nesting DEPTH.
Concatenates child paragraphs and nested lists."
  (let ((parts (append content nil))
        (result ""))
    (dolist (child parts)
      (let ((child-type (cdr (assq 'type child))))
        (if (member child-type '("bulletList" "orderedList"))
            ;; Nested list: already has its own newlines and prefixes
            (setq result (concat result
                                 (shipit-issue-jira--adf-to-text child depth)))
          ;; Paragraph text: render and ensure it ends with newline
          (setq result (concat result
                               (string-trim-right
                                (shipit-issue-jira--adf-to-text child depth)
                                "\n")
                               "\n")))))
    result))

;;; Normalization — Jira → shipit alist format

(defun shipit-issue-jira--normalize-issue (jira-data)
  "Normalize Jira issue JIRA-DATA to shipit alist format.
Maps Jira fields to the keys expected by shipit UI:
  key → id, number
  fields.summary → title
  fields.status.name → state
  fields.description → body
  fields.reporter → user
  fields.assignee → assignee
  fields.labels → labels (as list of (name . X) alists)
  fields.created → created_at
  fields.updated → updated_at
  fields.comment.comments → comments (count)"
  (let* ((key (cdr (assq 'key jira-data)))
         (fields (cdr (assq 'fields jira-data)))
         (reporter (cdr (assq 'reporter fields)))
         (assignee (cdr (assq 'assignee fields)))
         (issue-type-obj (cdr (assq 'issuetype fields)))
         (parent-obj (cdr (assq 'parent fields)))
         (labels (append (cdr (assq 'labels fields)) nil))
         (components-raw (append (cdr (assq 'components fields)) nil))
         (components (delq nil (mapcar (lambda (c) (cdr (assq 'name c)))
                                       components-raw)))
         (comments-data (cdr (assq 'comment fields)))
         (comments-list (append (cdr (assq 'comments comments-data)) nil))
         (raw-desc (cdr (assq 'description fields)))
         (description (cond
                       ((stringp raw-desc) raw-desc)
                       ((and raw-desc (listp raw-desc))
                        (string-trim (shipit-issue-jira--adf-to-text raw-desc)))
                       (t ""))))
    (let ((changelog-data (cdr (assq 'changelog jira-data)))
          (raw-issuelinks (cdr (assq 'issuelinks fields))))

      `((id . ,key)
        (number . ,key)
        (title . ,(cdr (assq 'summary fields)))
        (state . ,(cdr (assq 'name (cdr (assq 'status fields)))))
        (state-id . ,(cdr (assq 'id (cdr (assq 'status fields)))))
        (body . ,description)
        (user . ((login . ,(cdr (assq 'displayName reporter)))
                 (avatar_url . nil)))
        (issue-type . ,(cdr (assq 'name issue-type-obj)))
        (priority . ,(cdr (assq 'name (cdr (assq 'priority fields)))))
        (status-category . ,(cdr (assq 'key (cdr (assq 'statusCategory
                                                        (cdr (assq 'status fields)))))))
        (assignees . ,(when assignee
                        ;; The account id travels with the display name so a
                        ;; caller can tell whether an issue is the current
                        ;; user's without matching on a name.
                        (list `((login . ,(cdr (assq 'displayName assignee)))
                                (id . ,(cdr (assq 'accountId assignee)))))))
        (labels . ,(mapcar (lambda (l) `((name . ,l))) labels))
        (components . ,(mapcar (lambda (c) `((name . ,c))) components))
        (comments . ,(if comments-list (length comments-list) 0))
        (created_at . ,(cdr (assq 'created fields)))
        (updated_at . ,(cdr (assq 'updated fields)))
        ,@(when changelog-data
            `((changelog . ,(shipit-issue-jira--normalize-changelog changelog-data))))
        ,@(when raw-issuelinks
            `((issuelinks . ,(shipit-issue-jira--normalize-issuelinks raw-issuelinks))))
        ,@(when parent-obj
            (let ((parent-fields (cdr (assq 'fields parent-obj))))
              `((parent . ((key . ,(cdr (assq 'key parent-obj)))
                           (summary . ,(cdr (assq 'summary parent-fields)))
                           (issue-type . ,(cdr (assq 'name (cdr (assq 'issuetype parent-fields)))))
                           (status . ,(cdr (assq 'name (cdr (assq 'status parent-fields))))))))))))))

(defun shipit-issue-jira--normalize-changelog (changelog-data)
  "Normalize Jira CHANGELOG-DATA to list of activity entries.
CHANGELOG-DATA is the `changelog' alist from a Jira issue response.
Each history entry is mapped to:
  ((id . STRING) (created_at . STRING) (user . ((login . STRING) ...))
   (items . (((field . STRING) (from . STRING) (to . STRING)) ...)))"
  (let ((histories (append (cdr (assq 'histories changelog-data)) nil)))
    (mapcar (lambda (history)
              (let ((author (cdr (assq 'author history)))
                    (raw-items (append (cdr (assq 'items history)) nil)))
                `((id . ,(cdr (assq 'id history)))
                  (created_at . ,(cdr (assq 'created history)))
                  (user . ((login . ,(cdr (assq 'displayName author)))
                           (avatar_url . nil)))
                  (items . ,(mapcar (lambda (item)
                                      `((field . ,(cdr (assq 'field item)))
                                        (from . ,(or (cdr (assq 'fromString item)) ""))
                                        (to . ,(or (cdr (assq 'toString item)) ""))))
                                    raw-items)))))
            histories)))

(defun shipit-issue-jira--normalize-issuelinks (raw-links)
  "Normalize Jira RAW-LINKS to list of link alists.
Each link has: type, direction, key, summary, status, assignee."
  (let ((links (append raw-links nil)))
    (delq nil
          (mapcar (lambda (link)
                    (let* ((type-obj (cdr (assq 'type link)))
                           (outward-issue (cdr (assq 'outwardIssue link)))
                           (inward-issue (cdr (assq 'inwardIssue link))))
                      (cond
                       (outward-issue
                        (let ((fields (cdr (assq 'fields outward-issue))))
                          `((type . ,(cdr (assq 'outward type-obj)))
                            (direction . "outward")
                            (key . ,(cdr (assq 'key outward-issue)))
                            (summary . ,(cdr (assq 'summary fields)))
                            (status . ,(cdr (assq 'name (cdr (assq 'status fields)))))
                            (assignee . ,(cdr (assq 'displayName (cdr (assq 'assignee fields))))))))
                       (inward-issue
                        (let ((fields (cdr (assq 'fields inward-issue))))
                          `((type . ,(cdr (assq 'inward type-obj)))
                            (direction . "inward")
                            (key . ,(cdr (assq 'key inward-issue)))
                            (summary . ,(cdr (assq 'summary fields)))
                            (status . ,(cdr (assq 'name (cdr (assq 'status fields)))))
                            (assignee . ,(cdr (assq 'displayName (cdr (assq 'assignee fields)))))))))))
                  links))))

(defun shipit-issue-jira--normalize-comment (jira-comment)
  "Normalize a single Jira JIRA-COMMENT to shipit alist format."
  (let* ((author (cdr (assq 'author jira-comment)))
         (raw-body (cdr (assq 'body jira-comment)))
         (body (cond
                ((stringp raw-body) raw-body)
                ((and raw-body (listp raw-body))
                 (string-trim (shipit-issue-jira--adf-to-text raw-body)))
                (t ""))))
    `((id . ,(cdr (assq 'id jira-comment)))
      (body . ,body)
      (user . ((login . ,(cdr (assq 'displayName author)))
               (avatar_url . nil)))
      (created_at . ,(cdr (assq 'created jira-comment)))
      (updated_at . ,(cdr (assq 'updated jira-comment))))))

;;; Authentication

(defun shipit-issue-jira--auth-header (config)
  "Build Basic auth header from auth-source for CONFIG's :base-url.
Looks up credentials using the hostname from :base-url.
Returns \"Basic <base64>\" string, or nil if no credentials found."
  (let* ((base-url (plist-get config :base-url))
         (host (url-host (url-generic-parse-url base-url)))
         (found (car (auth-source-search :host host :max 1))))
    (shipit--debug-log "Jira auth: host=%s found=%s" host (if found "yes" "no"))
    (if (not found)
        (user-error "No Jira credentials found for %s in auth-source.\n\
Add to ~/.authinfo.gpg: machine %s login <email> password <api-token>" host host)
      (let ((user (plist-get found :user))
            (secret (plist-get found :secret)))
        (shipit--debug-log "Jira auth: user=%s secret=%s" user (if secret "present" "nil"))
        (when (and user secret)
          (let ((token (if (functionp secret) (funcall secret) secret)))
            (concat "Basic "
                    (base64-encode-string
                     (concat user ":" token) t))))))))

(defun shipit-issue-jira--configs ()
  "Return the config of every Jira site shipit is set up for."
  (append (cl-loop for (_pattern . config) in shipit-issue-repo-backends
                   when (eq (plist-get config :backend) 'jira)
                   collect config)
          (when (and (eq shipit-issue-backend 'jira) shipit-issue-backend-config)
            (list shipit-issue-backend-config))))

(defun shipit-issue-jira--config-for-host (host)
  "Return the config of the Jira site at HOST, or nil."
  (cl-find host (shipit-issue-jira--configs)
           :key (lambda (config)
                  (url-host (url-generic-parse-url (or (plist-get config :base-url) ""))))
           :test #'equal))

(defun shipit-issue-jira--image-auth-headers (url)
  "Return the headers that log an image download at URL in to Jira.
Nil unless URL is on a Jira site shipit is set up for, so that no other
site sees the login."
  (when-let* ((config (shipit-issue-jira--config-for-host
                       (url-host (url-generic-parse-url url))))
              (auth (shipit-issue-jira--auth-header config)))
    `(("Authorization" . ,auth))))

(add-hook 'shipit-image-auth-functions #'shipit-issue-jira--image-auth-headers)

(defconst shipit-issue-jira--attachment-content-re
  "\\`/rest/api/[23]/attachment/content/\\([0-9]+\\)"
  "Regexp matching the path of an attachment's content; group 1 is its id.")

(defun shipit-issue-jira--redirect-location (config path)
  "Return where Jira at CONFIG redirects a GET of PATH to, or nil."
  (let* ((url-request-method "GET")
         (url-max-redirections 0)
         (url-request-extra-headers
          `(("Authorization" . ,(shipit-issue-jira--auth-header config))))
         (buffer (url-retrieve-synchronously
                  (shipit-issue-jira--build-url config path) t t 15)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (let ((case-fold-search t)
                  (end (save-excursion (or (search-forward "\n\n" nil t) (point-max)))))
              (when (re-search-forward "^location: *\\([^\r\n]+\\)" end t)
                (match-string 1))))
        (kill-buffer buffer)))))

(defun shipit-issue-jira--media-url (url)
  "Return the media URL Jira hands out for the attachment at URL, or nil.
Jira answers for an attachment's content with a redirect to its media
service, whose URL holds a short-lived token for that one file, so a
player needs no login to fetch it.  Nil unless URL is an attachment on
a Jira site shipit is set up for."
  (let* ((parsed (url-generic-parse-url url))
         (config (shipit-issue-jira--config-for-host (url-host parsed)))
         (path (url-filename parsed)))
    (when (and config (string-match shipit-issue-jira--attachment-content-re path))
      (shipit-issue-jira--redirect-location
       config (format "/rest/api/3/attachment/content/%s" (match-string 1 path))))))

(add-hook 'shipit-media-url-functions #'shipit-issue-jira--media-url)

;;; API Functions

(defun shipit-issue-jira--build-url (config path)
  "Build full Jira REST API URL from CONFIG and PATH."
  (let ((base-url (plist-get config :base-url)))
    (unless base-url
      (error "Jira backend requires :base-url in config"))
    (concat (string-trim-right base-url "/") path)))

(defun shipit-issue-jira--goto-response-body ()
  "Move point to the first byte of the HTTP body in the current buffer.
Prefers `url-http-end-of-headers', which url.el sets on the newline
closing the header block, and falls back to the blank line itself."
  (goto-char (point-min))
  (if (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
      (goto-char (1+ url-http-end-of-headers))
    (re-search-forward "\r?\n\r?\n" nil t)))

(defun shipit-issue-jira--decode-response-body ()
  "Reinterpret the body starting at point as UTF-8, leaving point on it.
The round trip through unibyte re-reads the raw bytes url.el delivered
under Emacs\=' internal encoding, so non-ASCII text survives."
  (let ((body-start (point)))
    (set-buffer-multibyte nil)
    (set-buffer-multibyte t)
    (goto-char body-start)))

(defun shipit-issue-jira--read-json (context)
  "Read the JSON value at point, returning nil when it does not parse.
CONTEXT labels the failure in the debug log."
  (condition-case err
      (json-read)
    (error
     (shipit--debug-log "%s: JSON parse error: %s" context err)
     nil)))

(defun shipit-issue-jira--parse-async-response (status url)
  "Return the JSON body of the Jira response in the current buffer.
STATUS is the plist `url-retrieve' handed the callback and URL labels
the debug log.  Returns nil when the request errored or the body was
not JSON."
  (if (plist-get status :error)
      (progn
        (shipit-issue-jira--goto-response-body)
        (shipit--debug-log
         "Jira API async error (%s): %s body=%s" url (plist-get status :error)
         (buffer-substring (point) (min (point-max) (+ (point) 400))))
        nil)
    (shipit-issue-jira--goto-response-body)
    (shipit-issue-jira--decode-response-body)
    (shipit-issue-jira--read-json "Jira API async")))

(defun shipit-issue-jira--api-request-async (config path callback)
  "Async GET to the Jira REST API using CONFIG at PATH.
CALLBACK receives the parsed JSON, or nil when the request failed.
It runs once the response buffer has already been killed, so callers
must tunnel writes to buffer-local state through `with-current-buffer'.
Uses `url-retrieve' so the request does not block the UI even when
Jira is slow."
  (let* ((url (shipit-issue-jira--build-url config path))
         (auth (shipit-issue-jira--auth-header config))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when auth `(("Authorization" . ,auth))))))
    (shipit--debug-log "Jira API async: GET %s" url)
    (url-retrieve
     url
     (lambda (status)
       ;; Release the response buffer whatever happens -- a failed
       ;; request used to leave its buffer behind on every poll.
       (let ((result (unwind-protect
                         (shipit-issue-jira--parse-async-response status url)
                       (kill-buffer (current-buffer)))))
         (when callback (funcall callback result))))
     nil t t)))

(defun shipit-issue-jira--api-request (config path)
  "Make a GET request to Jira REST API using CONFIG at PATH.
Authenticates via auth-source (Basic auth with email:api-token).
Returns parsed JSON as alist."
  (let* ((t0 (float-time))
         (url (shipit-issue-jira--build-url config path))
         (auth (progn
                 (shipit--debug-log "Jira API: resolving auth...")
                 (let ((a (shipit-issue-jira--auth-header config)))
                   (shipit--debug-log "Jira API: auth resolved in %.3fs"
                                      (- (float-time) t0))
                   a)))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when auth
                `(("Authorization" . ,auth)))))
         (t1 (float-time))
         (buffer (url-retrieve-synchronously url t)))
    (shipit--debug-log "Jira API: %s %s (http=%.3fs, total=%.3fs)"
                       url-request-method url
                       (- (float-time) t1) (- (float-time) t0))
    (if (not buffer)
        (progn
          (shipit--debug-log "Jira API: no response buffer for %s" url)
          nil)
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (let ((status-line (buffer-substring (point) (line-end-position))))
              (shipit--debug-log "Jira API: response status: %s" status-line))
            (shipit-issue-jira--goto-response-body)
            (shipit--debug-log "Jira API: response body preview: %s"
                               (buffer-substring
                                (point) (min (+ (point) 200) (point-max))))
            (shipit-issue-jira--decode-response-body)
            (shipit-issue-jira--read-json "Jira API"))
        (kill-buffer buffer)))))

(defun shipit-issue-jira--read-response (buffer method path)
  "Parse the JSON body of the response in BUFFER to a METHOD on PATH.
Returns the parsed alist, or nil for an empty body.  Signals an error
carrying Jira\\='s messages when the status is 400 or above."
  (with-current-buffer buffer
    (goto-char (point-min))
    (shipit--debug-log "Jira API: %s response status: %s" method
                       (buffer-substring (point) (line-end-position)))
    (shipit-issue-jira--goto-response-body)
    (shipit-issue-jira--decode-response-body)
    (let ((parsed (shipit-issue-jira--read-json
                   (format "Jira API %s" method))))
      (when (and (boundp 'url-http-response-status)
                 url-http-response-status
                 (>= url-http-response-status 400))
        (error "Jira API %s %s failed (%d): %s"
               method path url-http-response-status
               (or (cdr (assq 'errorMessages parsed))
                   (cdr (assq 'errors parsed)))))
      parsed)))

(defun shipit-issue-jira--api-request-send (method config path &optional data)
  "Send a METHOD request to the Jira REST API at PATH using CONFIG.
DATA, when given, is an alist sent JSON-encoded as the body.  Returns
the parsed JSON response as an alist, nil for an empty body or no
response, and signals an error on a 4xx/5xx status."
  (let* ((url (shipit-issue-jira--build-url config path))
         (auth (shipit-issue-jira--auth-header config))
         (url-request-method method)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when auth
                `(("Authorization" . ,auth)))))
         (url-request-data
          (when data (encode-coding-string (json-encode data) 'utf-8)))
         (buffer (url-retrieve-synchronously url t)))
    (shipit--debug-log "Jira API: %s %s" method url)
    (if (not buffer)
        (progn
          (shipit--debug-log "Jira API: no response buffer for %s %s" method url)
          nil)
      (unwind-protect
          (shipit-issue-jira--read-response buffer method path)
        (kill-buffer buffer)))))

(defun shipit-issue-jira--api-request-post (config path data)
  "Make a POST request to Jira REST API using CONFIG at PATH with DATA.
DATA is an alist that will be JSON-encoded as the request body.
Returns parsed JSON response as alist."
  (shipit-issue-jira--api-request-send "POST" config path data))

(defun shipit-issue-jira--api-request-put (config path data)
  "Make a PUT request to Jira REST API using CONFIG at PATH with DATA.
DATA is an alist that will be JSON-encoded as the request body.
Returns parsed JSON response as alist."
  (shipit-issue-jira--api-request-send "PUT" config path data))

(defun shipit-issue-jira--api-request-delete (config path)
  "Make a DELETE request to Jira REST API using CONFIG at PATH.
Returns the parsed JSON response, or nil for the usual empty 204."
  (shipit-issue-jira--api-request-send "DELETE" config path))

(defun shipit-issue-jira--add-comment (config issue-id body)
  "Add a comment to Jira issue ISSUE-ID using CONFIG.
BODY is the comment text.  Returns the normalized comment alist."
  (let* ((path (format "/rest/api/2/issue/%s/comment" issue-id))
         (data `((body . ,body)))
         (raw (shipit-issue-jira--api-request-post config path data)))
    (shipit--debug-log "Jira backend: added comment to %s" issue-id)
    (when raw
      (let ((shipit-issue-jira--mention-resolver
             (lambda (aid dn) (shipit-issue-jira--resolve-mention config aid dn))))
        (shipit-issue-jira--normalize-comment raw)))))

(defun shipit-issue-jira--edit-comment (config comment-id body)
  "Edit Jira comment COMMENT-ID using CONFIG.
COMMENT-ID should be \"ISSUE-KEY:COMMENT-ID\" (e.g. \"PRJ-42:10001\").
BODY is the new comment text.  Returns the normalized comment alist."
  (let* ((parts (split-string (format "%s" comment-id) ":"))
         (issue-key (car parts))
         (jira-comment-id (cadr parts))
         (path (format "/rest/api/2/issue/%s/comment/%s" issue-key jira-comment-id))
         (data `((body . ,body)))
         (raw (shipit-issue-jira--api-request-put config path data)))
    (shipit--debug-log "Jira backend: edited comment %s on %s" jira-comment-id issue-key)
    (when raw
      (let ((shipit-issue-jira--mention-resolver
             (lambda (aid dn) (shipit-issue-jira--resolve-mention config aid dn))))
        (shipit-issue-jira--normalize-comment raw)))))

(defun shipit-issue-jira--update-description (config issue-key body)
  "Update the description of Jira issue ISSUE-KEY using CONFIG.
BODY is the new description text.
Returns the API response."
  (let* ((path (format "/rest/api/2/issue/%s" issue-key))
         (data `((fields . ((description . ,body))))))
    (shipit--debug-log "Jira backend: updating description for %s" issue-key)
    (shipit-issue-jira--api-request-put config path data)))

(defcustom shipit-issue-jira-reactions-endpoint
  "/rest/internal/2/reactions/view"
  "Internal Jira endpoint for batch-fetching comment reactions.
Works on Jira Data Center/Server.  Atlassian Cloud uses
`/gateway/api/reactions/reactions' behind session-cookie auth, which
is not reachable with API tokens — so on Cloud this will 404 and
comments render with the placeholder icon only.  See JRACLOUD-78153.
Override this if your instance exposes a different path."
  :type 'string
  :group 'shipit)

(defvar shipit-issue-jira--reactions-unavailable nil
  "When non-nil, skip further attempts to fetch Jira comment reactions.
Set automatically after a 404 on the configured endpoint to avoid
logging on every issue open.")

(defun shipit-issue-jira--emoji-id-to-char (emoji-id)
  "Convert Jira EMOJI-ID (hex Unicode codepoint) to a character string.
Falls back to EMOJI-ID itself when the hex does not parse to a
positive codepoint."
  (let* ((code (string-to-number emoji-id 16))
         (ch (and (> code 0) (decode-char 'unicode code))))
    (if ch (string ch) emoji-id)))

(defun shipit-issue-jira--fetch-comment-reactions-batch (config repo comments is-inline)
  "Populate `shipit--reaction-cache' with Jira reactions for COMMENTS.
CONFIG is the Jira backend config.  REPO is the repository string used
for cache keys.  IS-INLINE is ignored — Jira only has general comment
reactions.

On Atlassian Cloud the endpoint 404s (no token-auth equivalent exists).
After the first 404, this function short-circuits for the rest of the
session via `shipit-issue-jira--reactions-unavailable'."
  (unless (or is-inline shipit-issue-jira--reactions-unavailable)
    (let ((numeric-ids
           (delq nil
                 (mapcar (lambda (c)
                           (let ((id (cdr (assq 'id c))))
                             (when id
                               (string-to-number (format "%s" id)))))
                         comments))))
      (when numeric-ids
        (let* ((raw (condition-case err
                        (shipit-issue-jira--api-request-post
                         config
                         shipit-issue-jira-reactions-endpoint
                         `((commentIds . ,(vconcat numeric-ids))))
                      (error
                       ;; Only silence 404 (expected on Cloud, where the
                       ;; internal endpoint does not exist).  Everything
                       ;; else — auth, 5xx, network — re-signals so the
                       ;; failure surfaces with a backtrace instead of
                       ;; rendering as "no reactions".
                       (let ((msg (error-message-string err)))
                         (if (string-match-p "404" msg)
                             (progn
                               (setq shipit-issue-jira--reactions-unavailable t)
                               (shipit--debug-log
                                "Jira reactions: endpoint 404 (likely Cloud); giving up for session")
                               nil)
                           (signal (car err) (cdr err)))))))
               (entries (append raw nil))
               (by-id (make-hash-table :test 'equal)))
          (dolist (comment comments)
            (puthash (format "%s" (cdr (assq 'id comment))) '() by-id))
          (dolist (entry entries)
            (let* ((cid (format "%s" (cdr (assq 'commentId entry))))
                   (emoji-id (cdr (assq 'emojiId entry)))
                   (content (shipit-issue-jira--emoji-id-to-char emoji-id))
                   (users (append (cdr (assq 'users entry)) nil))
                   (existing (gethash cid by-id '())))
              (dolist (user users)
                (push `((content . ,content)
                        (user . ((login . ,user))))
                      existing))
              (puthash cid existing by-id)))
          (maphash (lambda (cid reactions)
                     (let ((cache-key (format "%s:%s-general" repo cid)))
                       (puthash cache-key reactions shipit--reaction-cache)))
                   by-id)
          (shipit--debug-log "Jira reactions: cached %d comments (%d entries)"
                             (hash-table-count by-id) (length entries)))))))

(defun shipit-issue-jira--toggle-reaction (_config _comment-id _reaction)
  "Toggle reaction on Jira comment (no-op).
Jira REST API v2 does not support emoji reactions."
  (message "Reactions are not supported by the Jira backend")
  nil)

(defun shipit-issue-jira--create-issue (config title body)
  "Create a new Jira issue using CONFIG with TITLE and BODY.
Uses the first key from :project-keys as the project.
Issue type defaults to \"Task\" unless :default-issue-type is set.
Returns normalized issue alist after re-fetching the created issue."
  (let ((project-keys (plist-get config :project-keys)))
    (unless project-keys
      (error "Jira backend requires :project-keys in config to create issues"))
    (let* ((project-key (car project-keys))
           (issue-type (or (plist-get config :default-issue-type) "Task"))
           (data `((fields . ((project . ((key . ,project-key)))
                              (summary . ,title)
                              (description . ,body)
                              (issuetype . ((name . ,issue-type)))))))
           (response (shipit-issue-jira--api-request-post
                      config "/rest/api/2/issue" data))
           (created-key (cdr (assq 'key response))))
      (shipit--debug-log "Jira backend: created issue %s" created-key)
      (shipit-issue-jira--fetch-issue config created-key))))

(defconst shipit-issue-jira--fields
  "key,summary,status,description,reporter,assignee,labels,components,created,updated,comment,issuetype,issuelinks,parent,attachment,priority"
  "Jira issue fields to request.  Limits response size for faster API calls.")

(defun shipit-issue-jira--enrich-link-assignees (config issue-data)
  "Enrich issuelinks in ISSUE-DATA with assignees fetched via CONFIG.
Jira's issuelinks embedded data lacks the assignee field, so we
do a single batch JQL query to fill them in."
  (let* ((links (cdr (assq 'issuelinks issue-data)))
         (keys (delq nil (mapcar (lambda (l) (cdr (assq 'key l))) links))))
    (if (null keys)
        issue-data
      (let* ((jql (format "key in (%s)" (mapconcat #'identity keys ",")))
             (raw (shipit-issue-jira--search-page config jql 100 "key,assignee" nil))
             (issues (append (cdr (assq 'issues raw)) nil))
             (assignee-map (make-hash-table :test 'equal)))
        (dolist (issue issues)
          (let* ((key (cdr (assq 'key issue)))
                 (fields (cdr (assq 'fields issue)))
                 (name (cdr (assq 'displayName (cdr (assq 'assignee fields))))))
            (when (and key name)
              (puthash key name assignee-map))))
        (let ((enriched-links
               (mapcar (lambda (link)
                         (let* ((key (cdr (assq 'key link)))
                                (existing (cdr (assq 'assignee link)))
                                (fetched (gethash key assignee-map)))
                           (if (and (not existing) fetched)
                               (cons (cons 'assignee fetched)
                                     (assq-delete-all 'assignee (copy-alist link)))
                             link)))
                       links)))
          (setf (alist-get 'issuelinks issue-data) enriched-links)
          issue-data)))))

(defun shipit-issue-jira--fetch-issue (config id)
  "Fetch Jira issue ID using CONFIG and normalize to shipit format.
Expands changelog to include activity history."
  (let* ((path (format "/rest/api/3/issue/%s?fields=%s&expand=changelog"
                       id shipit-issue-jira--fields))
         (raw (shipit-issue-jira--api-request config path)))
    (shipit--debug-log "Jira backend: fetched issue %s" id)
    (when raw
      (let ((shipit-issue-jira--mention-resolver
             (lambda (aid dn) (shipit-issue-jira--resolve-mention config aid dn)))
            (shipit-issue-jira--attachments (shipit-issue-jira--raw-attachments raw)))
        (shipit-issue-jira--enrich-link-assignees
         config
         (shipit-issue-jira--normalize-issue raw))))))

(defun shipit-issue-jira--raw-attachments (raw)
  "Return the attachments of RAW, an issue as the API gives it."
  (append (cdr (assq 'attachment (cdr (assq 'fields raw)))) nil))

(defun shipit-issue-jira--attachments-path (id)
  "Return the API path that asks for the attachments of issue ID alone.
The comments endpoint gives none, and an image in a comment is an
attachment of its issue."
  (format "/rest/api/3/issue/%s?fields=attachment" id))

(defun shipit-issue-jira--normalize-comments (config raw attachments)
  "Return the comments of RAW, a comments response, normalized.
CONFIG resolves mentions, and ATTACHMENTS are the issue\='s, for the
images the comments hold."
  (let ((shipit-issue-jira--mention-resolver
         (lambda (aid dn) (shipit-issue-jira--resolve-mention config aid dn)))
        (shipit-issue-jira--attachments attachments))
    (mapcar #'shipit-issue-jira--normalize-comment
            (append (cdr (assq 'comments raw)) nil))))

(defun shipit-issue-jira--fetch-comments (config id)
  "Fetch comments for Jira issue ID using CONFIG."
  (let* ((raw (shipit-issue-jira--api-request
               config (format "/rest/api/3/issue/%s/comment" id)))
         (attachments (shipit-issue-jira--raw-attachments
                       (shipit-issue-jira--api-request
                        config (shipit-issue-jira--attachments-path id))))
         (comments (shipit-issue-jira--normalize-comments config raw attachments)))
    (shipit--debug-log "Jira backend: fetched %d comments for %s"
                       (length comments) id)
    comments))

(defun shipit-issue-jira--fetch-comments-async (config id callback)
  "Fetch comments for Jira issue ID using CONFIG asynchronously.
Calls CALLBACK with the normalized comment list.  The issue\='s
attachments are fetched first, for the images the comments hold."
  (shipit-issue-jira--api-request-async
   config (shipit-issue-jira--attachments-path id)
   (lambda (raw-attachments)
     (shipit-issue-jira--api-request-async
      config (format "/rest/api/3/issue/%s/comment" id)
      (lambda (raw)
        (let ((comments (shipit-issue-jira--normalize-comments
                         config raw
                         (shipit-issue-jira--raw-attachments raw-attachments))))
          (shipit--debug-log "Jira backend async: fetched %d comments for %s"
                             (length comments) id)
          (funcall callback comments)))))))

(defun shipit-issue-jira--summarize-history-items (items)
  "Render Jira changelog ITEMS to a one-line summary string.
ITEMS is the raw `items' array from a single `histories' entry.
Each item becomes `field: from -> to'; multiple items are
comma-joined."
  (mapconcat
   (lambda (item)
     (let* ((field (or (cdr (assq 'field item)) "field"))
            (from (cdr (assq 'fromString item)))
            (to (cdr (assq 'toString item)))
            (from-s (if (and from (not (string-empty-p from))) from "<empty>"))
            (to-s (if (and to (not (string-empty-p to))) to "<empty>")))
       (format "%s: %s -> %s" field from-s to-s)))
   (append items nil) ", "))

(defun shipit-issue-jira--summarize-comment-body (body)
  "Render Jira comment BODY (string or ADF) to a short single line.
ADF nodes are flattened via `--adf-to-text', whitespace is
collapsed, and the result is truncated to 80 chars."
  (let ((text (cond
               ((stringp body) body)
               ((and body (listp body))
                (string-trim (shipit-issue-jira--adf-to-text body)))
               (t ""))))
    (setq text (replace-regexp-in-string "[\n\r\t ]+" " " text))
    (if (> (length text) 80)
        (concat (substring text 0 77) "...")
      text)))

(defun shipit-issue-jira--extract-last-activity (raw)
  "Pick the most recent activity from a RAW issue response with
`fields=comment&expand=changelog'.  Returns nil when neither
stream has any entries.  The result is a plist:
  (:type comment|change :author NAME :timestamp ISO :summary STR)."
  (let* ((key (cdr (assq 'key raw)))
         (fields (cdr (assq 'fields raw)))
         (comments (append (cdr (assq 'comments
                                      (cdr (assq 'comment fields))))
                           nil))
         (changelog (cdr (assq 'changelog raw)))
         (histories (append (cdr (assq 'histories changelog)) nil))
         ;; Don\='t trust the order Jira returns these in -- the
         ;; agile/search endpoints serve histories newest-first while
         ;; the issue endpoint serves them oldest-first.  Pick by
         ;; `created' timestamp explicitly (string-> works on Jira\='s
         ;; ISO8601 timestamps).
         (newer-by-created (lambda (a b)
                             (string> (or (cdr (assq 'created a)) "")
                                      (or (cdr (assq 'created b)) ""))))
         (last-comment (car (sort (copy-sequence comments) newer-by-created)))
         (last-history (car (sort (copy-sequence histories) newer-by-created)))
         (c-time (and last-comment (cdr (assq 'created last-comment))))
         (h-time (and last-history (cdr (assq 'created last-history)))))
    (shipit--debug-log
     "Jira last-activity[%s]: comments=%d histories=%d c-time=%s h-time=%s last-history-author=%S"
     key (length comments) (length histories)
     c-time h-time
     (and last-history (cdr (assq 'displayName
                                   (cdr (assq 'author last-history))))))
    (cond
     ((and c-time h-time (string> c-time h-time))
      (list :type 'comment
            :author (cdr (assq 'displayName
                               (cdr (assq 'author last-comment))))
            :author-id (cdr (assq 'accountId
                                  (cdr (assq 'author last-comment))))
            :timestamp c-time
            :summary (shipit-issue-jira--summarize-comment-body
                      (cdr (assq 'body last-comment)))))
     (last-history
      (list :type 'change
            :author (cdr (assq 'displayName
                               (cdr (assq 'author last-history))))
            :author-id (cdr (assq 'accountId
                                  (cdr (assq 'author last-history))))
            :timestamp h-time
            :summary (shipit-issue-jira--summarize-history-items
                      (cdr (assq 'items last-history)))))
     (last-comment
      (list :type 'comment
            :author (cdr (assq 'displayName
                               (cdr (assq 'author last-comment))))
            :author-id (cdr (assq 'accountId
                                  (cdr (assq 'author last-comment))))
            :timestamp c-time
            :summary (shipit-issue-jira--summarize-comment-body
                      (cdr (assq 'body last-comment)))))
     (t nil))))

(defun shipit-issue-jira--fetch-last-activity-async (config issue-key callback)
  "Fetch ISSUE-KEY's most recent activity (comment or changelog entry).
Calls CALLBACK with a plist or nil when the issue has no activity
at all.  ISSUE-KEY is a Jira issue key string."
  (let ((path (format "/rest/api/3/issue/%s?fields=comment&expand=changelog"
                      issue-key)))
    (shipit-issue-jira--api-request-async
     config path
     (lambda (raw)
       (funcall callback
                (and raw (shipit-issue-jira--extract-last-activity raw)))))))

(defun shipit-issue-jira--search-page (config jql page-size fields next-page-token)
  "Fetch one page of Jira search results.
Uses /rest/api/3/search/jql with cursor-based pagination.
NEXT-PAGE-TOKEN is nil for the first page, or a token from the previous response.
Returns the raw parsed JSON response."
  (let ((path (format "/rest/api/3/search/jql?jql=%s&maxResults=%d&fields=%s%s"
                      (url-hexify-string jql) page-size fields
                      (if next-page-token
                          (format "&nextPageToken=%s" (url-hexify-string next-page-token))
                        ""))))
    (shipit-issue-jira--api-request config path)))

(defun shipit-issue-jira--is-last-page-p (raw)
  "Return non-nil if RAW Jira response indicates the last page.
Checks the isLast field; treats :json-false as false."
  (let ((is-last (cdr (assq 'isLast raw))))
    (and is-last (not (eq is-last :json-false)))))

(defun shipit-issue-jira--search (config args)
  "Search Jira issues using CONFIG with transient ARGS.
Builds JQL from ARGS and queries the Jira search API.
Uses cursor-based pagination via nextPageToken."
  (let* ((jql (shipit-issue-jira--build-jql-from-args config args))
         (limit (shipit-issue-jira--extract-limit args))
         (fields shipit-issue-jira--fields)
         (page-size (min 100 limit))
         (all-issues nil)
         (next-token nil)
         (keep-going t))
    (while keep-going
      (let* ((raw (shipit-issue-jira--search-page config jql page-size fields next-token))
             (page-issues (append (cdr (assq 'issues raw)) nil))
             (page-token (cdr (assq 'nextPageToken raw))))
        (shipit--debug-log "Jira search: got=%d nextToken=%s isLast=%s"
                           (length page-issues) page-token
                           (cdr (assq 'isLast raw)))
        (setq all-issues (nconc all-issues page-issues))
        (setq next-token page-token)
        (setq keep-going (and (> (length page-issues) 0)
                              (< (length all-issues) limit)
                              (not (shipit-issue-jira--is-last-page-p raw))
                              next-token))))
    (shipit--debug-log "Jira backend: search JQL=%s returned %d results"
                       jql (length all-issues))
    (let ((shipit-issue-jira--mention-resolver
           (lambda (aid dn) (shipit-issue-jira--resolve-mention config aid dn))))
      (mapcar #'shipit-issue-jira--normalize-issue
              (seq-take all-issues limit)))))

(defun shipit-issue-jira--extract-limit (args)
  "Extract limit from transient ARGS, defaulting to 50."
  (let ((limit 50))
    (dolist (arg args)
      (when (string-prefix-p "--limit=" arg)
        (setq limit (string-to-number (substring arg 8)))))
    limit))

(defun shipit-issue-jira--build-jql-from-args (config args)
  "Build JQL query from CONFIG and transient ARGS.
Translates shipit transient args to Jira Query Language."
  (let ((project-keys (plist-get config :project-keys))
        (clauses '()))
    (when project-keys
      (push (format "project in (%s)"
                    (mapconcat (lambda (k) (format "\"%s\"" k))
                               project-keys ", "))
            clauses))
    (dolist (arg args)
      (cond
       ((string-prefix-p "--state=" arg)
        (let ((state (substring arg 8)))
          (cond
           ((string= state "all") nil)
           ((string= state "open")
            (push "statusCategory != \"Done\"" clauses))
           ((string= state "closed")
            (push "statusCategory = \"Done\"" clauses))
           (t (push (format "status = \"%s\"" state) clauses)))))
       ((string-prefix-p "--author-id=" arg)
        (let ((id (substring arg 12)))
          (when (> (length id) 0)
            (push (format "reporter = \"%s\"" id) clauses))))
       ((string-prefix-p "--assignee-id=" arg)
        (let ((id (substring arg 14)))
          (when (> (length id) 0)
            (push (format "assignee = \"%s\"" id) clauses))))
       ((string-prefix-p "--author=" arg)
        (let ((author (substring arg 9)))
          (when (> (length author) 0)
            (push (if (string= author "@me")
                      "reporter = currentUser()"
                    (format "reporter = \"%s\"" author))
                  clauses))))
       ((string-prefix-p "--assignee=" arg)
        (let ((assignee (substring arg 11)))
          (when (> (length assignee) 0)
            (push (if (string= assignee "@me")
                      "assignee = currentUser()"
                    (format "assignee = \"%s\"" assignee))
                  clauses))))
       ((string-prefix-p "--label=" arg)
        (let ((label (substring arg 8)))
          (when (> (length label) 0)
            (push (format "labels = \"%s\"" label) clauses))))
       ((string-prefix-p "--title=" arg)
        (let ((title (substring arg 8)))
          (when (> (length title) 0)
            (push (format "summary ~ \"%s\"" title) clauses))))
       ((string-prefix-p "--body=" arg)
        (let ((body (substring arg 7)))
          (when (> (length body) 0)
            (push (format "description ~ \"%s\"" body) clauses))))
       ((string-prefix-p "--number=" arg)
        (let ((number (substring arg 9)))
          (when (> (length number) 0)
            (push (format "key = \"%s\"" number) clauses))))
       ((string-prefix-p "--created-after=" arg)
        (let ((date (substring arg 16)))
          (when (> (length date) 0)
            (push (format "created >= \"%s\"" date) clauses))))
       ((string-prefix-p "--created-before=" arg)
        (let ((date (substring arg 17)))
          (when (> (length date) 0)
            (push (format "created <= \"%s\"" date) clauses))))
       ((string-prefix-p "--updated-after=" arg)
        (let ((date (substring arg 16)))
          (when (> (length date) 0)
            (push (format "updated >= \"%s\"" date) clauses))))
       ((string-prefix-p "--updated-before=" arg)
        (let ((date (substring arg 17)))
          (when (> (length date) 0)
            (push (format "updated <= \"%s\"" date) clauses))))))
    (if clauses
        (string-join (nreverse clauses) " AND ")
      "order by created DESC")))

;;; Status Transitions

(defun shipit-issue-jira--normalize-transitions (raw)
  "Normalize RAW Jira transitions response to list of alists.
Each alist has (id . STRING), (name . STRING), and
(to . ((name . STRING) (id . STRING) (category . STRING))) --
`category\=' is the `statusCategory.key\=' Jira returns on the
target status (typical values: \"new\", \"indeterminate\",
\"done\").  Callers that route moves by destination status use
`name\=' / `id\=', and the dashboard\='s commit walker uses
`category\=' to know when to also set `resolution\=' on a
done-category transition (otherwise Jira boards filter the
issue out by missing `resolutiondate\=')."
  (let ((transitions (append (cdr (assq 'transitions raw)) nil)))
    (mapcar (lambda (tr)
              (let* ((to (cdr (assq 'to tr)))
                     (category (cdr (assq 'key
                                          (cdr (assq 'statusCategory to))))))
                `((id . ,(cdr (assq 'id tr)))
                  (name . ,(cdr (assq 'name tr)))
                  (to . ((name . ,(cdr (assq 'name to)))
                         (id . ,(cdr (assq 'id to)))
                         (category . ,category))))))
            transitions)))

(defun shipit-issue-jira--get-transitions (config issue-id)
  "Fetch available transitions for Jira issue ISSUE-ID using CONFIG.
Returns a normalized list of ((id . ID) (name . NAME)) alists."
  (let* ((path (format "/rest/api/2/issue/%s/transitions" issue-id))
         (raw (shipit-issue-jira--api-request config path)))
    (shipit--debug-log "Jira backend: fetched transitions for %s" issue-id)
    (shipit-issue-jira--normalize-transitions raw)))

(defcustom shipit-issue-jira-set-resolution-on-done t
  "When non-nil, set `resolution\=' on transitions to Done-category statuses.
Jira boards typically include a `resolutiondate >= -Nd\=' clause
in their sub-filter so completed work auto-disappears after N
days.  A bare transition through the REST API leaves the
`resolution\=' field unset, so `resolutiondate\=' is never
populated and the card silently drops off the board the moment
it transitions to Done -- matching neither \"open\" nor
\"recently closed\" predicates.

When this defcustom is non-nil (the default) shipit attaches
`{\"resolution\": {\"name\": \"Done\"}}\=' to the transition
body whenever the destination status\='s `statusCategory.key\='
is `\"done\"\='.  Disable if your workflows expose a screen
that prompts for a specific resolution and rejects the
default."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-issue-jira-default-resolution "Done"
  "Resolution name used by `shipit-issue-jira-set-resolution-on-done\='.
Most Jira instances ship with `Done\=' / `Won\='t Do\=' / `Cannot
Reproduce\=' / etc.; the default value matches the Jira Cloud
default.  Customise if your instance renames it (some teams
use `Fixed\=', `Resolved\=', etc.)."
  :type 'string
  :group 'shipit)

(defun shipit-issue-jira--transition-status (config issue-id transition-id)
  "Transition Jira issue ISSUE-ID to status via TRANSITION-ID using CONFIG.
Posts to the transitions endpoint.  When the destination
status is in the `done\=' category and
`shipit-issue-jira-set-resolution-on-done\=' is non-nil, also
sets `resolution\=' to `shipit-issue-jira-default-resolution\='
in the same call so Jira populates `resolutiondate\=' and
boards keep the card visible per their `resolutiondate >= -Nd\='
sub-filter clause."
  (let* ((category (when shipit-issue-jira-set-resolution-on-done
                     (let* ((transitions (ignore-errors
                                           (shipit-issue-jira--get-transitions
                                            config issue-id)))
                            (matching (cl-find-if
                                       (lambda (tr)
                                         (string-equal
                                          (or (cdr (assq 'id tr)) "")
                                          transition-id))
                                       transitions)))
                       (and matching
                            (cdr (assq 'category
                                       (cdr (assq 'to matching))))))))
         (set-resolution (and shipit-issue-jira-set-resolution-on-done
                              category
                              (string-equal-ignore-case category "done")))
         (path (format "/rest/api/2/issue/%s/transitions" issue-id))
         (data (if set-resolution
                   `((transition . ((id . ,transition-id)))
                     (fields . ((resolution . ((name . ,shipit-issue-jira-default-resolution))))))
                 `((transition . ((id . ,transition-id)))))))
    (shipit--debug-log
     "Jira backend: transitioning %s with %s (target-category=%S set-resolution=%s)"
     issue-id transition-id category set-resolution)
    (shipit-issue-jira--api-request-post config path data)))

(defun shipit-issue-jira-set-resolution (key &optional resolution)
  "Set the `Resolution\=' field on Jira issue KEY (e.g. \"PRJ-12776\").
RESOLUTION is the resolution name (default
`shipit-issue-jira-default-resolution\=', usually \"Done\").
Useful for cleaning up issues that were transitioned to a
Done-category status before the auto-resolution change landed
\(those land in Jira with a Done status but no resolution, so
boards filter them out via `resolutiondate >= -Nd\=' clauses).

When called interactively from a shipit issue buffer, defaults
the key to the buffer\='s issue number; otherwise prompts."
  (interactive
   (let* ((default-key (and (boundp 'shipit-issue-buffer-number)
                            (let ((n shipit-issue-buffer-number))
                              (and n (if (numberp n) (number-to-string n) n)))))
          (key (read-string
                (if default-key
                    (format "Issue key (default %s): " default-key)
                  "Issue key: ")
                nil nil default-key))
          (res (read-string
                (format "Resolution (default %s): "
                        shipit-issue-jira-default-resolution)
                nil nil shipit-issue-jira-default-resolution)))
     (list key res)))
  (let* ((repo (or (and (boundp 'shipit-issue-buffer-repo)
                        shipit-issue-buffer-repo)
                   (when (string-match "\\`\\([A-Z][A-Z0-9_]+\\)-" key)
                     (match-string 1 key))))
         (resolved (and repo (shipit-issue--resolve-for-repo repo)))
         (config (or (cdr-safe resolved)
                     (user-error "Cannot resolve Jira config for %s" key)))
         (resolution-name (or resolution
                              shipit-issue-jira-default-resolution))
         (path (format "/rest/api/2/issue/%s" key))
         (data `((fields . ((resolution . ((name . ,resolution-name))))))))
    (shipit--debug-log
     "Jira backend: setting resolution=%s on %s" resolution-name key)
    (shipit-issue-jira--api-request-put config path data)
    (message "%s: resolution set to %s" key resolution-name)))

;;; Reference Patterns

(defun shipit-issue-jira--reference-patterns (config)
  "Return reference patterns for Jira issues based on CONFIG.
CONFIG should contain :project-keys listing project prefixes.
Returns patterns for both bare keys (PRJ-123) and full browse URLs."
  (let ((project-keys (plist-get config :project-keys))
        (base-url (plist-get config :base-url))
        (patterns nil))
    ;; Bare key patterns: PRJ-123
    (dolist (key project-keys)
      (push (list (format "\\b\\(%s-[0-9]+\\)" (regexp-quote key))
                  1
                  #'identity)
            patterns))
    ;; URL pattern: https://jira.example.com/browse/PRJ-123
    (when base-url
      (let ((escaped-url (regexp-quote (string-trim-right base-url "/"))))
        (push (list (format "%s/browse/\\([A-Z][A-Z0-9]+-[0-9]+\\)" escaped-url)
                    1
                    #'identity)
              patterns)))
    (nreverse patterns)))

;;; URL and Display

(defun shipit-issue-jira--browse-url (config id)
  "Return browser URL for Jira issue ID using CONFIG."
  (format "%s/browse/%s"
          (string-trim-right (plist-get config :base-url) "/")
          id))

(defun shipit-issue-jira--classify-url (config url)
  "Classify URL as a Jira issue if it matches CONFIG's :base-url.
Returns plist (:type issue :number KEY) or nil."
  (let* ((base-url (string-trim-right (plist-get config :base-url) "/"))
         (base-host (url-host (url-generic-parse-url base-url))))
    (when (and base-host
               (string-match
                (format "\\`https?://%s/browse/\\([A-Z][A-Z0-9_]+-[0-9]+\\)/?\\'"
                        (regexp-quote base-host))
                url))
      (list :type 'issue
            :number (match-string 1 url)))))

(defun shipit-issue-jira--id-to-string (id)
  "Convert Jira ID to display string (identity — already like \"PRJ-42\")."
  (format "%s" id))

(defun shipit-issue-jira--string-to-id (str)
  "Convert display string to Jira ID (identity — already like \"PRJ-42\")."
  str)

;;; Notification Polling

(defconst shipit-issue-jira--notifications-fields
  "key,summary,status,updated,components,labels,assignee,reporter,comment,description"
  "JQL `fields=' value used by notification polls.
`components' / `labels' enable component- / label-based
filtering; `assignee', `comment' and `description' feed
involvement detection so each activity gets a real reason
(mention/comment/assigned/updated); `reporter' surfaces the
author in the expanded body.")

(defvar shipit-issue-jira--myself-cache (make-hash-table :test 'equal)
  "Cache mapping Jira base-url to the current user's account-id.
Populated on first poll per session — the value is stable so we
never re-fetch.  Use `shipit-issue-jira-clear-myself-cache' to
force a refresh after switching accounts.")

(defun shipit-issue-jira-clear-myself-cache ()
  "Forget cached Jira account-ids — re-fetched on next poll."
  (interactive)
  (clrhash shipit-issue-jira--myself-cache))

(defun shipit-issue-jira--fetch-myself (config)
  "Return the current user's accountId for the Jira instance in CONFIG.
Cached per base-url for the Emacs session; first call hits
`/rest/api/3/myself', subsequent calls return the cached value.
Returns nil and caches nil when the API call fails (missing
base-url, auth issues, …) so we never re-hit a broken endpoint
and involvement detection just falls through to the default
\"updated\" reason."
  (let* ((base-url (plist-get config :base-url))
         (cached (gethash base-url shipit-issue-jira--myself-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((account-id
             (condition-case err
                 (cdr (assq 'accountId
                            (shipit-issue-jira--api-request
                             config "/rest/api/3/myself")))
               (error
                (shipit--debug-log "Jira myself: fetch failed: %S" err)
                nil))))
        (puthash base-url account-id shipit-issue-jira--myself-cache)
        (shipit--debug-log "Jira myself: cached account-id=%s for %s"
                           account-id base-url)
        account-id))))

(defun shipit-issue-jira--adf-collect-mentions (node)
  "Return a list of mention account-ids in NODE (an ADF tree).
Walks `content' recursively; ignores text-only leaves."
  (let ((acc '()))
    (cl-labels ((walk (n)
                  (when (and n (listp n) (consp (car-safe n)))
                    (let ((type (cdr (assq 'type n)))
                          (attrs (cdr (assq 'attrs n)))
                          (content (cdr (assq 'content n))))
                      (when (equal type "mention")
                        (let ((id (cdr (assq 'id attrs))))
                          (when (stringp id) (push id acc))))
                      (when content
                        (mapc #'walk (append content nil)))))))
      (walk node)
      (nreverse acc))))

(defun shipit-issue-jira--compute-involvement-reason (config issue)
  "Return notification reason for ISSUE based on the user's involvement.
Priority: \"mention\" > \"comment\" > \"assigned\" > \"updated\".
Falls back to \"updated\" when account-id is unavailable."
  (let* ((my-id (shipit-issue-jira--fetch-myself config))
         (fields (cdr (assq 'fields issue)))
         (description (cdr (assq 'description fields)))
         (assignee-id (cdr (assq 'accountId (cdr (assq 'assignee fields)))))
         (comments-data (cdr (assq 'comment fields)))
         (comments (append (cdr (assq 'comments comments-data)) nil))
         (mentioned-in-desc
          (and my-id description
               (member my-id
                       (shipit-issue-jira--adf-collect-mentions description))))
         (mentioned-in-comments
          (and my-id
               (cl-some (lambda (c)
                          (let ((body (cdr (assq 'body c))))
                            (and body
                                 (member my-id
                                         (shipit-issue-jira--adf-collect-mentions
                                          body)))))
                        comments)))
         (i-commented
          (and my-id
               (cl-some (lambda (c)
                          (equal my-id
                                 (cdr (assq 'accountId
                                            (cdr (assq 'author c))))))
                        comments))))
    (cond
     ((or mentioned-in-desc mentioned-in-comments) "mention")
     (i-commented "comment")
     ((and my-id assignee-id (equal my-id assignee-id)) "assigned")
     (t "updated"))))

(defun shipit-issue-jira--fetch-notifications (config &optional since)
  "Fetch recently updated Jira issues as notifications.
Uses JQL: updated >= (SINCE - 5min overlap margin).
Defaults to 1 hour ago if SINCE is nil.
CONFIG is the backend config plist."
  (let* ((since-time (shipit-issue-jira--notifications-since-time since))
         (jql (shipit-issue-jira--build-notifications-jql config since-time))
         (fields shipit-issue-jira--notifications-fields)
         (path (format "/rest/api/3/search/jql?jql=%s&maxResults=50&fields=%s&expand=changelog"
                       (url-hexify-string jql) fields))
         (raw (shipit-issue-jira--api-request config path))
         (issues (append (cdr (assq 'issues raw)) nil)))
    (shipit--debug-log "Jira notifications: JQL=%s returned %d issues" jql (length issues))
    (mapcar (lambda (issue)
              (shipit-issue-jira--issue-to-activity config issue))
            issues)))

(defun shipit-issue-jira--fetch-notifications-async (config &optional since callback)
  "Async variant of `shipit-issue-jira--fetch-notifications'.
Calls CALLBACK with the list of activity alists when the JQL
search returns.  CONFIG and SINCE behave identically to the sync
version.  CALLBACK is called with nil on error."
  (let* ((since-time (shipit-issue-jira--notifications-since-time since))
         (jql (shipit-issue-jira--build-notifications-jql config since-time))
         (fields shipit-issue-jira--notifications-fields)
         (path (format "/rest/api/3/search/jql?jql=%s&maxResults=50&fields=%s&expand=changelog"
                       (url-hexify-string jql) fields)))
    (shipit-issue-jira--api-request-async
     config path
     (lambda (raw)
       (let ((issues (when raw (append (cdr (assq 'issues raw)) nil))))
         (shipit--debug-log "Jira notifications (async): JQL=%s returned %d issues"
                            jql (length (or issues '())))
         (when callback
           (funcall callback
                    (mapcar (lambda (issue)
                              (shipit-issue-jira--issue-to-activity config issue))
                            issues))))))))

(defun shipit-issue-jira--notifications-since-time (since)
  "Compute the since-time for notification polling.
SINCE is an ISO8601 string or nil.  Returns a formatted Jira datetime string.
Subtracts 5 minutes for overlap margin."
  (let* ((base-time (if since
                        (float-time (date-to-time since))
                      (- (float-time) 3600)))  ; Default: 1 hour ago
         (with-margin (- base-time 300)))       ; 5 min overlap
    (format-time-string "%Y-%m-%d %H:%M" with-margin t)))

(defun shipit-issue-jira--build-notifications-jql (config since-time)
  "Build JQL for notification polling.
CONFIG provides :project-keys, SINCE-TIME is a formatted datetime string."
  (let ((project-keys (plist-get config :project-keys))
        (clauses '()))
    (when project-keys
      (push (format "project in (%s)"
                    (mapconcat (lambda (k) (format "\"%s\"" k))
                               project-keys ", "))
            clauses))
    (push (format "updated >= \"%s\"" since-time) clauses)
    (mapconcat #'identity (nreverse clauses) " AND ")))

(defun shipit-issue-jira--issue-to-activity (config issue)
  "Convert normalized Jira ISSUE to notification activity alist.
CONFIG provides context for display name."
  (let* ((key (cdr (assq 'key issue)))
         (fields (cdr (assq 'fields issue)))
         (summary (cdr (assq 'summary fields)))
         (updated (cdr (assq 'updated fields)))
         (components-raw (append (cdr (assq 'components fields)) nil))
         (components (delq nil (mapcar (lambda (c) (cdr (assq 'name c)))
                                       components-raw)))
         (labels (append (cdr (assq 'labels fields)) nil))
         (latest-activity (shipit-issue-jira--extract-last-activity issue))
         (author (or (plist-get latest-activity :author)
                     (cdr (assq 'displayName (cdr (assq 'reporter fields))))))
         (myself-id (ignore-errors (shipit-issue-jira--fetch-myself config)))
         (latest-author-id (plist-get latest-activity :author-id))
         (own-activity (and myself-id latest-author-id
                            (string-equal myself-id latest-author-id)))
         (reason
          (cond
           ;; Activities the viewer triggered themselves: tag them so
           ;; default auto-mark rules can hide them; the user already
           ;; saw the action when they fired it.
           (own-activity "your_activity")
           (t (condition-case err
                  (shipit-issue-jira--compute-involvement-reason config issue)
                (error
                 (shipit--debug-log
                  "Jira involvement detection failed for %s: %S" key err)
                 "updated")))))
         (status-obj (cdr (assq 'status fields)))
         (status-name (cdr (assq 'name status-obj)))
         (status-category (cdr (assq 'key
                                     (cdr (assq 'statusCategory status-obj)))))
         (base-url (plist-get config :base-url))
         (display-name (or (plist-get config :display-name)
                           (car (plist-get config :project-keys))
                           "jira")))
    (shipit--debug-log "Jira activity built: key=%s reason=%s components=%S"
                       key reason components)
    `((repo . ,display-name)
      (number . ,key)
      (type . "issue")
      (subject . ,(or summary ""))
      (reason . ,reason)
      (source . jira)
      (backend-id . jira)
      (backend-config . ,config)
      (jira-components . ,components)
      (labels . ,labels)
      (author . ,author)
      (state . ,status-name)
      (status-category . ,status-category)
      (browse-url . ,(when base-url
                       (format "%s/browse/%s"
                               (string-trim-right base-url "/") key)))
      (updated-at . ,updated))))

;;; Creation field metadata

(defun shipit-issue-jira--creation-fields (config)
  "Return field descriptors for Jira issue creation.
CONFIG must contain :project-keys and :base-url."
  (list
   (list :name 'title :label "Summary" :type 'string :required t
         :fetch-options nil)
   (list :name 'body :label "Description" :type 'text :required nil
         :fetch-options nil)
   (list :name 'issue-type :label "Issue Type" :type 'select :required t
         :fetch-options (lambda (cfg) (shipit-issue-jira--fetch-issue-types cfg)))
   (list :name 'labels :label "Labels" :type 'multi-select :required nil
         :allow-new t
         :fetch-options (lambda (cfg) (shipit-issue-jira--fetch-labels cfg)))
   (list :name 'assignees :label "Assignee" :type 'multi-select :required nil
         :fetch-options (lambda (cfg) (shipit-issue-jira--fetch-assignable-users cfg)))
   (list :name 'components :label "Components" :type 'multi-select :required nil
         :fetch-options (lambda (cfg) (shipit-issue-jira--fetch-components cfg)))))

(defun shipit-issue-jira--fetch-issue-types (config)
  "Fetch available issue types for the project in CONFIG."
  (let* ((project-key (car (plist-get config :project-keys)))
         (path (format "/rest/api/3/project/%s" project-key))
         (raw (shipit-issue-jira--api-request config path))
         (issue-types (append (cdr (assq 'issueTypes raw)) nil)))
    (shipit--debug-log "Jira backend: fetched %d issue types for %s"
                       (length issue-types) project-key)
    (mapcar (lambda (it) (cdr (assq 'name it))) issue-types)))

(defvar shipit-issue-jira--assignable-users-cache nil
  "Cache mapping display names to account IDs for Jira assignees.")

(defconst shipit-issue-jira--assignable-page-size 200
  "Users asked for per page of the assignable-user search.")

(defconst shipit-issue-jira--assignable-user-limit 2000
  "Ceiling on users collected per project.
Paging stops here even if Jira keeps answering with full pages, so a
server that ignores startAt cannot spin the loop forever.")

(defun shipit-issue-jira--assignable-page-path (project-key start-at)
  "Return the assignable-user search path for PROJECT-KEY from START-AT."
  (format "/rest/api/3/user/assignable/search?project=%s&startAt=%d&maxResults=%d"
          (url-hexify-string project-key)
          start-at
          shipit-issue-jira--assignable-page-size))

(defun shipit-issue-jira--fetch-assignable-in-project (config project-key)
  "Return every user assignable in PROJECT-KEY, following pagination.
The endpoint answers a page at a time and reports no total, so a short
page is what marks the end."
  (let ((users nil) (start-at 0) (more t))
    (while more
      (let* ((path (shipit-issue-jira--assignable-page-path project-key start-at))
             (page (append (shipit-issue-jira--api-request config path) nil)))
        (setq users (nconc users page))
        (setq start-at (+ start-at (length page)))
        (setq more (and (= (length page) shipit-issue-jira--assignable-page-size)
                        (< start-at shipit-issue-jira--assignable-user-limit)))))
    (shipit--debug-log "Jira backend: fetched %d assignable users for %s"
                       (length users) project-key)
    users))

(defun shipit-issue-jira--merge-assignable-users (config)
  "Return users assignable in any project CONFIG names, once each.
Someone assignable only in the second project belongs in the prompt
just as much as one from the first, so the projects union rather than
the first winning."
  (let ((seen (make-hash-table :test #'equal)) (users nil))
    (dolist (project-key (plist-get config :project-keys))
      (dolist (user (shipit-issue-jira--fetch-assignable-in-project
                     config project-key))
        (let ((account-id (cdr (assq 'accountId user))))
          (unless (gethash account-id seen)
            (puthash account-id t seen)
            (push user users)))))
    (nreverse users)))

(defun shipit-issue-jira--fetch-assignable-users (config)
  "Fetch users assignable to issues in CONFIG's projects.
Caches the name-to-accountId mapping for use during issue creation and
returns the display names, sorted so the prompt reads the same twice."
  (let ((users (shipit-issue-jira--merge-assignable-users config)))
    (setq shipit-issue-jira--assignable-users-cache
          (mapcar (lambda (u)
                    (cons (cdr (assq 'displayName u))
                          (cdr (assq 'accountId u))))
                  users))
    (sort (mapcar (lambda (u) (cdr (assq 'displayName u))) users) #'string<)))

(defun shipit-issue-jira--fetch-project-statuses (config)
  "Return unique status names across all issue types in the project.
Uses /rest/api/3/project/KEY/statuses which groups statuses per issue
type; this helper flattens and de-duplicates the names."
  (let* ((project-key (car (plist-get config :project-keys)))
         (path (format "/rest/api/3/project/%s/statuses" project-key))
         (raw (shipit-issue-jira--api-request config path))
         (type-entries (append raw nil))
         (names nil))
    (dolist (entry type-entries)
      (dolist (status (append (cdr (assq 'statuses entry)) nil))
        (let ((name (cdr (assq 'name status))))
          (when (and name (not (member name names)))
            (push name names)))))
    (shipit--debug-log "Jira backend: fetched %d unique statuses for %s"
                       (length names) project-key)
    (nreverse names)))

(defun shipit-issue-jira--account-id-for (assignee-name)
  "Return the account ID cached for ASSIGNEE-NAME.
Errors when the name is unknown: Jira addresses assignees by account
ID, and sending none would quietly unassign the issue instead."
  (or (cdr (assoc assignee-name shipit-issue-jira--assignable-users-cache))
      (error "No Jira account ID known for assignee %S" assignee-name)))

(defun shipit-issue-jira--update-assignee (config issue-key assignee-name)
  "Update the assignee of ISSUE-KEY to ASSIGNEE-NAME via CONFIG.
ASSIGNEE-NAME is a display name or nil to unassign."
  (let* ((account-id (when assignee-name
                       (shipit-issue-jira--account-id-for assignee-name)))
         (data `((fields . ((assignee . ,(if account-id
                                             `((accountId . ,account-id))
                                           :null)))))))
    (shipit-issue-jira--api-request-put
     config (format "/rest/api/3/issue/%s" issue-key) data)
    (shipit--debug-log "Jira: updated assignee of %s to %s" issue-key
                       (or assignee-name "unassigned"))))

;;; Issue Links -- creating

(defun shipit-issue-jira--link-type-relations (link-type)
  "Return the relations one LINK-TYPE offers, as (PHRASE . RELATION).
A RELATION is (TYPE-NAME . DIRECTION), DIRECTION being `outward' or
`inward' -- which of the type\\='s two phrases the user picked.  A
symmetric type such as Relates words both directions alike, and is
offered once."
  (let ((name (cdr (assq 'name link-type)))
        (outward (cdr (assq 'outward link-type)))
        (inward (cdr (assq 'inward link-type))))
    (if (equal outward inward)
        (list (cons outward (cons name 'outward)))
      (list (cons outward (cons name 'outward))
            (cons inward (cons name 'inward))))))

(defun shipit-issue-jira--fetch-link-relations (config)
  "Return the relations an issue may have to another, from CONFIG's Jira.
An alist of (PHRASE . RELATION), PHRASE being what Jira shows the user
-- \"blocks\", \"is blocked by\" -- and RELATION what
`shipit-issue-jira--link-issues' needs to create it."
  (let* ((raw (shipit-issue-jira--api-request config "/rest/api/3/issueLinkType"))
         (types (append (cdr (assq 'issueLinkTypes raw)) nil)))
    (shipit--debug-log "Jira backend: fetched %d issue link types" (length types))
    (mapcan #'shipit-issue-jira--link-type-relations types)))

(defun shipit-issue-jira--link-body (issue-key relation target-key)
  "Build the POST body linking ISSUE-KEY to TARGET-KEY by RELATION.
Jira names the ends inward and outward such that the inward issue
\"<outward phrase>\" the outward one: for Blocks, the inward issue
blocks the outward.  So picking the outward phrase on ISSUE-KEY makes
it the inward end, and picking the inward phrase swaps the two."
  (pcase-let* ((`(,type-name . ,direction) relation)
               (`(,inward . ,outward)
                (if (eq direction 'outward)
                    (cons issue-key target-key)
                  (cons target-key issue-key))))
    `((type . ((name . ,type-name)))
      (inwardIssue . ((key . ,inward)))
      (outwardIssue . ((key . ,outward))))))

(defun shipit-issue-jira--link-issues (config issue-key relation target-key)
  "Link ISSUE-KEY to TARGET-KEY by RELATION via CONFIG.
RELATION is a value from `shipit-issue-jira--fetch-link-relations'."
  (shipit-issue-jira--api-request-post
   config "/rest/api/3/issueLink"
   (shipit-issue-jira--link-body issue-key relation target-key))
  (shipit--debug-log "Jira: linked %s %s %s" issue-key (car relation) target-key))

;;; Remote links -- web links on an issue, such as pull requests

(defun shipit-issue-jira--remote-link-path (issue-key)
  "Return the remote link endpoint for ISSUE-KEY."
  (format "/rest/api/3/issue/%s/remotelink" issue-key))

(defun shipit-issue-jira--link-remote-url (config issue-key url title)
  "Attach URL to ISSUE-KEY as a remote link titled TITLE, via CONFIG.
The URL doubles as the link\\='s globalId, so attaching the same URL
again updates the existing link instead of adding a second."
  (shipit-issue-jira--api-request-post
   config (shipit-issue-jira--remote-link-path issue-key)
   `((globalId . ,url)
     (object . ((url . ,url) (title . ,title)))))
  (shipit--debug-log "Jira: attached %s to %s" url issue-key))

(defun shipit-issue-jira--unlink-remote-url (config issue-key url)
  "Detach URL from ISSUE-KEY via CONFIG.
URL is the link\='s globalId, as `shipit-issue-jira--link-remote-url'
set it."
  (shipit-issue-jira--api-request-delete
   config (format "%s?globalId=%s"
                  (shipit-issue-jira--remote-link-path issue-key)
                  (url-hexify-string url)))
  (shipit--debug-log "Jira: detached %s from %s" url issue-key))

(defun shipit-issue-jira--fetch-remote-urls (config issue-key)
  "Return ISSUE-KEY\\='s remote links via CONFIG, as (URL . TITLE) pairs."
  (let ((raw (shipit-issue-jira--api-request
              config (shipit-issue-jira--remote-link-path issue-key))))
    (mapcar (lambda (link)
              (let ((object (cdr (assq 'object link))))
                (cons (cdr (assq 'url object)) (cdr (assq 'title object)))))
            (append raw nil))))

(defun shipit-issue-jira--update-labels (config issue-key labels)
  "Set ISSUE-KEY's labels to LABELS (a list of strings) via CONFIG.
LABELS is encoded as a JSON array; an empty list clears the labels by
sending [] rather than null."
  (let ((data `((fields . ((labels . ,(vconcat labels)))))))
    (shipit-issue-jira--api-request-put
     config (format "/rest/api/3/issue/%s" issue-key) data)
    (shipit--debug-log "Jira: updated labels of %s to %S" issue-key labels)))

(defun shipit-issue-jira--update-title (config issue-key title)
  "Set ISSUE-KEY's summary (title) to TITLE via CONFIG."
  (let ((data `((fields . ((summary . ,title))))))
    (shipit-issue-jira--api-request-put
     config (format "/rest/api/3/issue/%s" issue-key) data)
    (shipit--debug-log "Jira: updated summary of %s" issue-key)))

(defun shipit-issue-jira--fetch-components (config)
  "Fetch available components for the project in CONFIG."
  (let* ((project-key (car (plist-get config :project-keys)))
         (path (format "/rest/api/3/project/%s/components" project-key))
         (raw (shipit-issue-jira--api-request config path))
         (components (append raw nil)))
    (shipit--debug-log "Jira backend: fetched %d components for %s"
                       (length components) project-key)
    (mapcar (lambda (c) (cdr (assq 'name c))) components)))

(defun shipit-issue-jira--fetch-labels (config)
  "Fetch all labels available in the Jira instance for CONFIG.
Jira labels are instance-wide (not project-scoped).  Follows the
startAt/maxResults pagination of GET /rest/api/3/label until the last
page and returns a flat list of label strings."
  (let ((labels nil) (start-at 0) (more t))
    (while more
      (let* ((path (format "/rest/api/3/label?startAt=%d&maxResults=1000" start-at))
             (raw (shipit-issue-jira--api-request config path))
             (values (append (cdr (assq 'values raw)) nil))
             (page-max (or (cdr (assq 'maxResults raw)) (length values))))
        (setq labels (nconc labels values))
        (setq start-at (+ start-at (length values)))
        (setq more (and (> (length values) 0)
                        (>= (length values) page-max)
                        (not (shipit-issue-jira--is-last-page-p raw))))))
    (shipit--debug-log "Jira backend: fetched %d labels" (length labels))
    labels))

(defun shipit-issue-jira--create-issue-extended (config fields)
  "Create a Jira issue with extended FIELDS.
FIELDS is an alist with keys: title, body, issue-type, labels, components.
Returns normalized issue alist after re-fetching."
  (let ((project-keys (plist-get config :project-keys)))
    (unless project-keys
      (error "Jira backend requires :project-keys in config to create issues"))
    (let* ((project-key (car project-keys))
           (issue-type (or (cdr (assq 'issue-type fields))
                           (plist-get config :default-issue-type)
                           "Task"))
           (jira-fields `((project . ((key . ,project-key)))
                          (summary . ,(cdr (assq 'title fields)))
                          (description . ,(or (cdr (assq 'body fields)) ""))
                          (issuetype . ((name . ,issue-type))))))
      (let ((labels (cdr (assq 'labels fields))))
        (when labels
          (push `(labels . ,labels) jira-fields)))
      (let ((components (cdr (assq 'components fields))))
        (when components
          (push `(components . ,(mapcar (lambda (c) `((name . ,c))) components))
                jira-fields)))
      (let ((assignees (cdr (assq 'assignees fields))))
        (when (and assignees (car assignees))
          ;; Jira supports single assignee — use the first selected
          (let ((account-id (cdr (assoc (car assignees)
                                        shipit-issue-jira--assignable-users-cache))))
            (when account-id
              (push `(assignee . ((accountId . ,account-id))) jira-fields)))))
      (let* ((data `((fields . ,jira-fields)))
             (response (shipit-issue-jira--api-request-post
                        config "/rest/api/2/issue" data))
             (created-key (cdr (assq 'key response))))
        (shipit--debug-log "Jira backend: created issue %s (extended)" created-key)
        (shipit-issue-jira--fetch-issue config created-key)))))

;;; Child Work Items (Epic Children)

(defun shipit-issue-jira--normalize-child-item (raw)
  "Normalize a RAW Jira issue to a lightweight child item alist."
  (let ((fields (cdr (assq 'fields raw))))
    `((key . ,(cdr (assq 'key raw)))
      (summary . ,(cdr (assq 'summary fields)))
      (status . ,(cdr (assq 'name (cdr (assq 'status fields)))))
      (assignee . ,(cdr (assq 'displayName (cdr (assq 'assignee fields)))))
      (issue-type . ,(cdr (assq 'name (cdr (assq 'issuetype fields))))))))

(defun shipit-issue-jira--fetch-children (config parent-key)
  "Fetch child work items for PARENT-KEY using CONFIG.
Returns a list of lightweight normalized child item alists."
  (let* ((jql (format "parent = %s ORDER BY status ASC, key ASC" parent-key))
         (fields "key,summary,status,assignee,issuetype")
         (page-size 100)
         (all-items nil)
         (next-token nil)
         (keep-going t))
    (while keep-going
      (let* ((raw (shipit-issue-jira--search-page config jql page-size fields next-token))
             (page-issues (append (cdr (assq 'issues raw)) nil))
             (page-token (cdr (assq 'nextPageToken raw))))
        (setq all-items (nconc all-items page-issues))
        (setq next-token page-token)
        (setq keep-going (and (> (length page-issues) 0)
                              (not (shipit-issue-jira--is-last-page-p raw))
                              next-token))))
    (shipit--debug-log "Jira: fetched %d children for %s" (length all-items) parent-key)
    (mapcar #'shipit-issue-jira--normalize-child-item all-items)))

(defun shipit-issue-jira--work-item-columns ()
  "Return the configured work item columns for Jira."
  shipit-jira-work-item-columns)

;;; Icon and face mappings for dashboard rendering

(defun shipit-issue-jira--issue-type-icon (type-name)
  "Return (octicon . color) for Jira issue TYPE-NAME."
  (pcase (downcase (or type-name ""))
    ("bug" '("bug" . "#cb2431"))
    ("story" '("bookmark" . "#28a745"))
    ("task" '("tasklist" . "#0366d6"))
    ("sub-task" '("tasklist" . "#6f42c1"))
    ("epic" '("flame" . "#fd7e14"))
    (_ '("issue-opened" . "#999999"))))

(defun shipit-issue-jira--priority-icon (priority-name)
  "Return (octicon . color) for Jira PRIORITY-NAME, or nil."
  (pcase (downcase (or priority-name ""))
    ((or "highest" "critical") '("arrow-up" . "#cb2431"))
    ("high" '("chevron-up" . "#fd7e14"))
    ("medium" '("dash" . "#fd7e14"))
    ("low" '("chevron-down" . "#0366d6"))
    ("lowest" '("arrow-down" . "#999999"))
    (_ nil)))

(defun shipit-issue-jira--status-category-face (status-category)
  "Return face for Jira STATUS-CATEGORY key."
  (pcase status-category
    ("new" 'magit-dimmed)
    ("indeterminate" 'magit-branch-remote)
    ("done" 'success)
    (_ 'default)))

(defun shipit-issue-jira--render-icon (icon-data)
  "Render ICON-DATA (octicon . color) as propertized string.
Returns empty string when svglib is unavailable or ICON-DATA is nil."
  (let ((use-svglib (and shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if (and use-svglib icon-data)
        (condition-case nil
            (let ((icon (shipit--svg-lib-icon-with-bg
                         (car icon-data) nil
                         :collection "octicons"
                         :height 0.9 :width 0.9 :padding 0.1
                         :foreground (cdr icon-data) :stroke 0)))
              (if (imagep icon)
                  (propertize " " 'display icon)
                ""))
          (error ""))
      "")))

(defvar shipit-issue-jira--icon-memo (make-hash-table :test 'equal)
  "Session-wide cache from (KIND . NAME) to the propertized icon string.
Rendering the Jira dashboard re-renders every issue row, and each row
asks for an issue-type icon and a priority icon.  Without memoisation
we hit `svg-lib-icon' hundreds of times per refresh (expensive even on
its internal cache due to arg hashing + image allocation); with this
cache we pay the SVG cost once per unique (kind, name) pair.

Cache keys survive across renders — the icon bits never change at
runtime, and clearing is only needed if the theme background changes.
Call `shipit-issue-jira--clear-icon-memo' after a theme switch.")

(defun shipit-issue-jira--clear-icon-memo ()
  "Drop the memoised icon cache (e.g. after a theme change)."
  (interactive)
  (clrhash shipit-issue-jira--icon-memo))

(defun shipit-issue-jira--render-issue-type-icon (type-name)
  "Render SVG icon for Jira issue TYPE-NAME, memoised."
  (let ((key (cons 'type type-name)))
    (or (gethash key shipit-issue-jira--icon-memo)
        (puthash key
                 (shipit-issue-jira--render-icon
                  (shipit-issue-jira--issue-type-icon type-name))
                 shipit-issue-jira--icon-memo))))

(defun shipit-issue-jira--render-priority-icon (priority-name)
  "Render SVG icon for Jira PRIORITY-NAME, memoised.  Nil when unknown."
  (let* ((key (cons 'priority priority-name))
         (cached (gethash key shipit-issue-jira--icon-memo 'missing)))
    (if (eq cached 'missing)
        (let ((val (when-let* ((mapping (shipit-issue-jira--priority-icon
                                         priority-name)))
                     (shipit-issue-jira--render-icon mapping))))
          (puthash key val shipit-issue-jira--icon-memo)
          val)
      cached)))

;; Register the Jira backend
(shipit-issue-register-backend
 'jira
 (list :name "Jira"
       :fetch-issue #'shipit-issue-jira--fetch-issue
       :fetch-comments #'shipit-issue-jira--fetch-comments
       :fetch-comments-async #'shipit-issue-jira--fetch-comments-async
       :search #'shipit-issue-jira--search
       :create-issue #'shipit-issue-jira--create-issue
       :reference-patterns #'shipit-issue-jira--reference-patterns
       :browse-url #'shipit-issue-jira--browse-url
       :id-to-string #'shipit-issue-jira--id-to-string
       :string-to-id #'shipit-issue-jira--string-to-id
       :add-comment #'shipit-issue-jira--add-comment
       :edit-comment #'shipit-issue-jira--edit-comment
       :toggle-reaction #'shipit-issue-jira--toggle-reaction
       :fetch-comment-reactions-batch #'shipit-issue-jira--fetch-comment-reactions-batch
       :update-description #'shipit-issue-jira--update-description
       :get-transitions #'shipit-issue-jira--get-transitions
       :transition-status #'shipit-issue-jira--transition-status
       :notifications #'shipit-issue-jira--fetch-notifications
       :notifications-async #'shipit-issue-jira--fetch-notifications-async
       :creation-fields #'shipit-issue-jira--creation-fields
       :create-issue-extended #'shipit-issue-jira--create-issue-extended
       :fetch-children #'shipit-issue-jira--fetch-children
       :work-item-columns #'shipit-issue-jira--work-item-columns
       :dashboard-columns 'shipit-jira-dashboard-columns
       :issue-type-icon-render #'shipit-issue-jira--render-issue-type-icon
       :priority-icon-render #'shipit-issue-jira--render-priority-icon
       :issue-type-color
       (lambda (name) (cdr (shipit-issue-jira--issue-type-icon name)))
       :issue-type-octicon
       (lambda (name) (car (shipit-issue-jira--issue-type-icon name)))
       :priority-color
       (lambda (name) (cdr (shipit-issue-jira--priority-icon name)))
       :status-category-face #'shipit-issue-jira--status-category-face
       :classify-url #'shipit-issue-jira--classify-url
       :fetch-assignable-users #'shipit-issue-jira--fetch-assignable-users
       :fetch-project-statuses #'shipit-issue-jira--fetch-project-statuses
       :fetch-project-components #'shipit-issue-jira--fetch-components
       :update-assignee #'shipit-issue-jira--update-assignee
       :fetch-labels #'shipit-issue-jira--fetch-labels
       :update-labels #'shipit-issue-jira--update-labels
       :fetch-link-relations #'shipit-issue-jira--fetch-link-relations
       :link-issues #'shipit-issue-jira--link-issues
       :link-remote-url #'shipit-issue-jira--link-remote-url
       :unlink-remote-url #'shipit-issue-jira--unlink-remote-url
       :fetch-remote-urls #'shipit-issue-jira--fetch-remote-urls
       :update-title #'shipit-issue-jira--update-title
       :icon-spec '("jira" "simple" . "#0052CC")
       :icon-fallback-text "JR"))

;;; Jira Mention Overlays

(declare-function shipit-issues--display-search-results "shipit-issues")

(defun shipit--create-jira-mention-overlays (&optional body-start body-end)
  "Create overlays for Jira @mentions that couldn't resolve to GitHub usernames.
Scans for text with `shipit-jira-mention' property and creates clickable
overlays with Jira issue-search actions.
BODY-START and BODY-END define the region to search."
  (condition-case err
      (let ((pos (or body-start (point-min)))
            (limit (or body-end (point-max))))
        (while (< pos limit)
          (let ((next (next-single-property-change pos 'shipit-jira-mention nil limit)))
            (if (and next (get-text-property pos 'shipit-jira-mention))
                (let* ((end (next-single-property-change pos 'shipit-jira-mention nil limit))
                       (display-name (get-text-property pos 'shipit-jira-display-name))
                       (ov (make-overlay pos end))
                       (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap (current-local-map))
                  (let ((account-id (get-text-property pos 'shipit-jira-account-id)))
                    (define-key keymap (kbd "RET")
                      (let ((name display-name)
                            (aid account-id))
                        (lambda ()
                          (interactive)
                          (shipit--jira-mention-action-menu name aid)))))
                  (overlay-put ov 'face '(:weight bold :underline t))
                  (overlay-put ov 'keymap keymap)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'shipit-jira-mention display-name)
                  (overlay-put ov 'help-echo (format "%s - RET: issue actions" display-name))
                  (setq pos end))
              (setq pos (or next limit))))))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "ERROR in shipit--create-jira-mention-overlays: %s" err)))))

(defun shipit--jira-mention-action-menu (display-name &optional account-id)
  "Show action menu for Jira DISPLAY-NAME mention.
ACCOUNT-ID, when non-nil, is used for exact Jira user matching in searches."
  (let ((choice (read-char-choice
                 (format "%s: [a]ssigned issues, [r]eported issues, [c]opy name, [q]uit: "
                         display-name)
                 '(?a ?r ?c ?q))))
    (pcase choice
      (?a (shipit--jira-mention-search-issues display-name "assignee" account-id))
      (?r (shipit--jira-mention-search-issues display-name "reporter" account-id))
      (?c (kill-new display-name)
          (message "Copied %s to kill ring" display-name))
      (?q nil))))

(defun shipit--jira-mention-search-issues (display-name role &optional account-id)
  "Search for issues where DISPLAY-NAME is ROLE (assignee or reporter).
When ACCOUNT-ID is non-nil, use it for exact Jira user matching."
  (let ((repo (shipit--get-repo-from-remote))
        (role-arg (if account-id
                      (format "--%s-id=%s"
                              (if (string= role "reporter") "author" role)
                              account-id)
                    (format "--%s=%s"
                            (if (string= role "reporter") "author" role)
                            display-name))))
    (if repo
        (shipit-issues--display-search-results
         repo (list role-arg "--limit=50" "--sort=created"))
      (message "Could not determine repository from remote"))))

(provide 'shipit-issue-jira)
;;; shipit-issue-jira.el ends here
