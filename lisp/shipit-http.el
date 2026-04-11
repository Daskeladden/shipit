;;; shipit-http.el --- HTTP and GitHub API functionality -*- lexical-binding: t; -*-

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
;; This module handles all HTTP requests and GitHub API interactions.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url)
(require 'url-http)
(require 'shipit-lib)     ; For shipit--defer-refresh
(require 'shipit-core)
(require 'shipit-cache)
(require 'shipit-gh-etag)
(require 'shipit-render)  ; For shipit--get-comment-icon

;; Require magit-section for macros used in this file
(eval-when-compile
  (require 'magit-section))
(require 'magit-section)

;; Backend registries for pluggable dispatch
(require 'shipit-pr-backends)
(require 'shipit-comment-backends)

;; Forward declarations for buffer-local variables
(defvar shipit--inline-comments-fetched)  ; from shipit-diff.el
(defvar shipit--general-comments-fetched) ; from shipit-core.el
(defvar shipit-buffer--refresh-in-progress) ; from shipit-buffer.el

;; Forward declarations for customization variables
(defvar shipit-comment-wrap-width)

;; Forward declarations for functions
(declare-function shipit-gh-etag-invalidate "shipit-gh-etag")
(declare-function shipit-gh-etag--invalidate-refresh-cache "shipit-gh-etag")
(declare-function shipit-comment--fetch-reactions-batch "shipit-comments")
(declare-function shipit-comment--fetch-reactions "shipit-comments")

;; URL.el helper functions to replace request library

(defun shipit--url-retrieve-sync (url method headers data &optional skip-json)
  "Synchronous HTTP request using url.el.
Returns (data . status-code) or signals error.
If SKIP-JSON is non-nil, returns just status-code without parsing JSON."
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "🔧 shipit--url-retrieve-sync called:")
    (shipit--debug-log "   url: %S (type: %s)" url (type-of url))
    (shipit--debug-log "   method: %S (type: %s)" method (type-of method))
    (shipit--debug-log "   headers: %S" headers)
    (shipit--debug-log "   data: %S (type: %s)" data (type-of data)))
  ;; Bind system-time-locale to "C" to ensure HTTP date headers use ASCII
  ;; (avoids "Multibyte text in HTTP request" with non-English locales)
  (let* ((system-time-locale "C")
         (url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data (when data (encode-coding-string data 'utf-8)))
         (buffer (condition-case err
                     (url-retrieve-synchronously url t t 10) ; 10 second timeout, inhibit cookies
                   (error
                    (when (fboundp 'shipit--debug-log)
                      (shipit--debug-log "❌ url-retrieve-synchronously failed: %s" (error-message-string err)))
                    (signal (car err) (cdr err))))))
    (if buffer
        (with-current-buffer buffer
          ;; Enable multibyte mode to properly interpret UTF-8 bytes from HTTP response
          ;; This ensures non-ASCII text (Korean, emoji, etc.) displays correctly
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (let* ((status-line (buffer-substring (point-min) (line-end-position)))
                 (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                               (string-to-number (match-string 1 status-line))))
                 (headers-end (search-forward "\n\n" nil t))
                 (json-data (when (and headers-end (not skip-json))
                             ;; On 304, the response body is empty — read from URL cache instead
                             (if (>= (point) (point-max))
                                 (if (eq status-code 304)
                                     (let ((cache-buf (url-cache-extract (url-cache-create-filename url))))
                                       (when cache-buf
                                         (with-current-buffer cache-buf
                                           (set-buffer-multibyte t)
                                           (goto-char (1+ (or url-http-end-of-headers (point-min))))
                                           (unwind-protect
                                               (let ((json-array-type 'list)
                                                     (json-object-type 'alist)
                                                     (json-key-type 'symbol))
                                                 (json-read))
                                             (kill-buffer cache-buf)))))
                                   (when (fboundp 'shipit--debug-log)
                                     (shipit--debug-log "❌ No JSON content to parse - empty response body"))
                                   nil)
                               (let ((json-array-type 'list)
                                     (json-object-type 'alist)
                                     (json-key-type 'symbol))
                                 (condition-case err
                                     (progn
                                       ;; Clean up emoji variation selectors if configured
                                       (when shipit-strip-emoji-variation-selectors
                                         (shipit--strip-emoji-variation-selectors (point) (point-max)))
                                       (json-read))
                                   (error
                                    (when (fboundp 'shipit--debug-log)
                                      (shipit--debug-log "❌ JSON parsing error: %s" (error-message-string err))
                                      (shipit--debug-log "❌ Response content (first 500 chars): %s"
                                                         (substring (buffer-substring-no-properties (point) (point-max)) 0 (min 500 (- (point-max) (point))))))
                                    (signal (car err) (cdr err)))))))))
            (kill-buffer buffer)
            (if skip-json
                status-code  ; Return just status code
              (cons json-data status-code))))
      (error "Failed to retrieve URL: %s" url))))

(defun shipit--url-retrieve-with-headers (url method headers data)
  "HTTP request that returns both JSON data and response headers.
Returns plist with :json, :status, :headers for GitHub protocol compliance."
  ;; Bind system-time-locale to "C" to ensure HTTP date headers use ASCII
  ;; (avoids "Multibyte text in HTTP request" with non-English locales)
  (let* ((system-time-locale "C")
         (url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data (when data (encode-coding-string data 'utf-8)))
         (buffer (condition-case err
                     (url-retrieve-synchronously url t t 10)
                   (error
                    (when (fboundp 'shipit--debug-log)
                      (shipit--debug-log "❌ url-retrieve-synchronously failed: %s" (error-message-string err)))
                    (signal (car err) (cdr err))))))
    (if buffer
        (with-current-buffer buffer
          ;; Enable multibyte mode to properly interpret UTF-8 bytes from HTTP response
          ;; This ensures non-ASCII text (Korean, emoji, etc.) displays correctly
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (let* ((status-line (buffer-substring (point-min) (line-end-position)))
                 (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                               (string-to-number (match-string 1 status-line))))
                 ;; Parse response headers
                 (response-headers '())
                 (headers-end (search-forward "\n\n" nil t)))

            ;; Extract response headers
            (goto-char (point-min))
            (forward-line 1) ; Skip status line
            (while (and (< (point) (or headers-end (point-max)))
                       (looking-at "\\([^:]+\\): \\(.+\\)"))
              (push (cons (downcase (match-string 1)) (match-string 2)) response-headers)
              (forward-line 1))

            (let ((json-data nil))
              ;; Parse JSON only if status indicates success and body exists
              (when (and headers-end
                        status-code
                        (>= status-code 200)
                        (< status-code 300)
                        (/= status-code 304)) ; Don't parse JSON for 304 Not Modified
                (goto-char headers-end)
                (when (< (point) (point-max))
                  (set-buffer-multibyte t)
                  (decode-coding-region (point) (point-max) 'utf-8)
                  (when shipit-strip-emoji-variation-selectors
                    (shipit--strip-emoji-variation-selectors (point) (point-max)))
                  (let ((json-array-type 'list)
                        (json-object-type 'alist)
                        (json-key-type 'symbol))
                    (condition-case err
                        (setq json-data (json-read))
                      (error
                       (when (fboundp 'shipit--debug-log)
                         (shipit--debug-log "❌ JSON parsing error: %s" (error-message-string err))))))))

              (kill-buffer buffer)
              `(:json ,json-data :status ,status-code :headers ,(nreverse response-headers)))))
      (error "Failed to retrieve URL: %s" url))))

(defun shipit--url-retrieve-async (url method headers data success-callback error-callback)
  "Asynchronous HTTP request using url.el."
  ;; Set system-time-locale globally for async requests (runs outside let scope)
  ;; Ensures HTTP date headers use ASCII instead of locale-specific characters
  (setq system-time-locale "C")
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data (when data (encode-coding-string data 'utf-8))))
    (url-retrieve url
                  (lambda (status)
                        (let* ((error-status (plist-get status :error)))
                          ;; Only treat :error as failure.  :redirect is informational —
                          ;; url.el follows redirects automatically, so the buffer already
                          ;; contains the final response.
                          (if error-status
                              (funcall error-callback error-status)
                            (goto-char (point-min))
                            (let* ((status-line (buffer-substring (point-min) (line-end-position)))
                                   (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                                                 (string-to-number (match-string 1 status-line))))
                                   (headers-end (search-forward "\n\n" nil t))
                                   (json-data (when (and headers-end (< (point) (point-max)))
                                               ;; Ensure response is decoded as UTF-8 before JSON parsing
                                               (set-buffer-multibyte t)
                                               (decode-coding-region (point) (point-max) 'utf-8)
                                               ;; Clean up emoji variation selectors if configured
                                               (when shipit-strip-emoji-variation-selectors
                                                 (shipit--strip-emoji-variation-selectors (point) (point-max)))
                                               (let ((json-array-type 'list)
                                                     (json-object-type 'alist)
                                                     (json-key-type 'symbol))
                                                 (condition-case err
                                                     (json-read)
                                                   (error
                                                    (when (fboundp 'shipit--debug-log)
                                                      (shipit--debug-log "❌ JSON parsing error (async): %s" (error-message-string err))
                                                      (shipit--debug-log "❌ Response content (first 500 chars): %s"
                                                                         (substring (buffer-substring-no-properties (point) (point-max)) 0 (min 500 (- (point-max) (point))))))
                                                    (signal (car err) (cdr err))))))))
                              (if (and status-code (>= status-code 200) (< status-code 300))
                                  (funcall success-callback json-data)
                                (funcall error-callback (format "HTTP %d" status-code)))))
                          (kill-buffer (current-buffer)))))))

(defun shipit--url-retrieve-async-with-headers (url method headers data success-callback error-callback)
  "Like `shipit--url-retrieve-async' but also expose response headers.
SUCCESS-CALLBACK is called with two arguments: the parsed JSON body and
an alist of response headers (name and value both strings).  Used by
callers that need HTTP response metadata such as the Link header for
pagination."
  (setq system-time-locale "C")
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data (when data (encode-coding-string data 'utf-8))))
    (url-retrieve
     url
     (lambda (status)
       (let ((error-status (plist-get status :error)))
         (if error-status
             (funcall error-callback error-status)
           (goto-char (point-min))
           (let* ((status-line (buffer-substring (point-min) (line-end-position)))
                  (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                                 (string-to-number (match-string 1 status-line))))
                  (response-headers nil))
             ;; Parse response headers into an alist until the blank line
             (forward-line 1)
             (while (and (< (point) (point-max))
                         (not (looking-at "\r?\n")))
               (let ((line (buffer-substring (point) (line-end-position))))
                 (when (string-match "^\\([^:]+\\):\\s-*\\(.*\\)$" line)
                   (push (cons (match-string 1 line)
                               (string-trim-right (match-string 2 line) "\r"))
                         response-headers))
                 (forward-line 1)))
             (let* ((headers-end (search-forward "\n\n" nil t))
                    (json-data (when (and headers-end (< (point) (point-max)))
                                 (set-buffer-multibyte t)
                                 (decode-coding-region (point) (point-max) 'utf-8)
                                 (when shipit-strip-emoji-variation-selectors
                                   (shipit--strip-emoji-variation-selectors (point) (point-max)))
                                 (let ((json-array-type 'list)
                                       (json-object-type 'alist)
                                       (json-key-type 'symbol))
                                   (condition-case err
                                       (json-read)
                                     (error
                                      (signal (car err) (cdr err))))))))
               (if (and status-code (>= status-code 200) (< status-code 300))
                   (funcall success-callback json-data (nreverse response-headers))
                 (funcall error-callback (format "HTTP %d" status-code)))))
           (kill-buffer (current-buffer))))))))

(defun shipit--parse-link-header-last-page (headers)
  "Return the last-page number from HEADERS, an alist of HTTP headers.
HEADERS is the response-headers alist produced by
`shipit--url-retrieve-async-with-headers'.  Looks for a Link header
entry containing a `rel=\"last\"' segment and extracts the page query
parameter.  Returns an integer or nil if not found."
  (let ((link (cdr (assoc "Link" headers))))
    (when (and link
               (string-match "<[^>]*[?&]page=\\([0-9]+\\)[^>]*>;\\s-*rel=\"last\"" link))
      (string-to-number (match-string 1 link)))))

;; Helper functions for cache keys
(defun shipit--reaction-cache-key (repo comment-id is-inline)
  "Generate a repository-specific cache key for reactions.
Uses format: 'repo:comment-id-type' where type is 'inline' or 'general'."
  (format "%s:%s-%s" repo comment-id (if is-inline "inline" "general")))

;; Variables
(defvar shipit-current-pr nil
  "Current pull request number.")

;; shipit-github-token and shipit-api-url are defined in shipit-core.el as defcustom

(defun shipit--strip-emoji-variation-selectors (start end)
  "Remove emoji variation selectors between START and END to avoid display issues.
Variation selector U+FE0F (️) can cause emojis to display as square boxes
in terminals or with incomplete font support."
  (save-excursion
    (goto-char start)
    (while (search-forward "\uFE0F" end t) ; U+FE0F = Variation Selector-16
      (replace-match "" nil t)
      (setq end (- end 1))) ; Adjust end position after deletion
    ;; Also remove U+FE0E (text variation selector) if present
    (goto-char start)
    (while (search-forward "\uFE0E" end t) ; U+FE0E = Variation Selector-15
      (replace-match "" nil t)
      (setq end (- end 1)))))

;; Core HTTP functions
(defun shipit--get-auth-header ()
  "Get the authorization header for GitHub API requests."
  (let ((token (shipit--github-token)))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "  github-token: %s" (if token "present" "nil")))
    (when token
      `("Authorization" . ,(format "token %s" token)))))

(defun shipit--graphql-query (query variables)
  "Execute a GraphQL QUERY with VARIABLES.
Returns the data portion of the GraphQL response."
  (let ((token (shipit--github-token)))
    (unless token
      (error "shipit: GitHub token required for GraphQL queries.  Set `shipit-github-token' or add github.com to auth-source"))
    (let* ((url (concat (or shipit-api-url "https://api.github.com") "/graphql"))
           (payload (json-encode `((query . ,query)
                                  (variables . ,variables))))
           (headers `(("Authorization" . ,(format "Bearer %s" token))
                     ("Accept" . "application/vnd.github.v3+json")
                     ("Content-Type" . "application/json"))))
      (shipit--debug-log "🔮 GRAPHQL: Making query to %s" url)
      (shipit--debug-log "🔮 GRAPHQL: Query: %s" query)
      (shipit--debug-log "🔮 GRAPHQL: Variables: %S" variables)
      (condition-case err
          (let* ((response (shipit--url-retrieve-sync url "POST" headers payload))
                 (data (car response))
                 (status (cdr response)))
            (shipit--debug-log "🔮 GRAPHQL: Response status: %s" status)
            (when-let ((errors (cdr (assq 'errors data))))
              (shipit--debug-log "🔮 GRAPHQL: ❌ Errors: %S" errors)
              (error "GraphQL error: %s" (cdr (assq 'message (aref errors 0)))))
            (let ((result (cdr (assq 'data data))))
              (shipit--debug-log "🔮 GRAPHQL: ✅ Success, data: %S" result)
              result))
        (error
         (shipit--debug-log "🔮 GRAPHQL: ❌ Failed: %s" (error-message-string err))
         (signal (car err) (cdr err)))))))

(defun shipit--graphql-reaction-to-rest (graphql-content)
  "Convert GraphQL reaction content to REST API format.
GraphQL uses THUMBS_UP, REST uses +1, etc."
  (pcase graphql-content
    ("THUMBS_UP" "+1")
    ("THUMBS_DOWN" "-1")
    ("LAUGH" "laugh")
    ("CONFUSED" "confused")
    ("HEART" "heart")
    ("HOORAY" "hooray")
    ("ROCKET" "rocket")
    ("EYES" "eyes")
    (_ (downcase graphql-content))))

(defun shipit--fetch-review-reactions-graphql (node-id)
  "Fetch reactions for a PR review using GraphQL.
NODE-ID is the global GitHub node ID for the review.
Returns a list of reactions in the same format as REST API."
  (let* ((query "query($nodeId: ID!) {
  node(id: $nodeId) {
    ... on PullRequestReview {
      reactions(first: 100) {
        nodes {
          id
          content
          user {
            login
            avatarUrl
          }
          createdAt
        }
      }
    }
  }
}")
         (variables `((nodeId . ,node-id)))
         (result (shipit--graphql-query query variables))
         (node (cdr (assq 'node result)))
         (reactions (cdr (assq 'reactions node)))
         (nodes (cdr (assq 'nodes reactions))))

    (shipit--debug-log "🔮 REVIEW-REACTIONS: Fetched %d reactions for review %s"
                       (length nodes) node-id)

    ;; Convert GraphQL format to REST API format for compatibility
    (mapcar (lambda (reaction)
              (let ((content (cdr (assq 'content reaction)))
                    (user (cdr (assq 'user reaction)))
                    (created-at (cdr (assq 'createdAt reaction))))
                `((id . ,(cdr (assq 'id reaction)))
                  (content . ,(shipit--graphql-reaction-to-rest content))
                  (user . ((login . ,(cdr (assq 'login user)))
                          (avatar_url . ,(cdr (assq 'avatarUrl user)))))
                  (created_at . ,created-at))))
            nodes)))

(defun shipit--add-review-reaction-graphql (node-id reaction-content)
  "Add a reaction to a PR review using GraphQL.
NODE-ID is the global GitHub node ID for the review.
REACTION-CONTENT is the emoji type (e.g., '+1', 'heart', 'laugh', etc.).
Returns the mutation result or nil on error."
  (let* ((mutation "mutation($nodeId: ID!, $content: ReactionContent!) {
  addReaction(input: {subjectId: $nodeId, content: $content}) {
    reaction {
      id
      content
      user {
        login
      }
    }
  }
}")
         ;; Convert REST API reaction format to GraphQL format
         (graphql-content (cond
                          ((string= reaction-content "+1") "THUMBS_UP")
                          ((string= reaction-content "-1") "THUMBS_DOWN")
                          ((string= reaction-content "laugh") "LAUGH")
                          ((string= reaction-content "hooray") "HOORAY")
                          ((string= reaction-content "confused") "CONFUSED")
                          ((string= reaction-content "heart") "HEART")
                          ((string= reaction-content "rocket") "ROCKET")
                          ((string= reaction-content "eyes") "EYES")
                          (t reaction-content)))
         (variables `((nodeId . ,node-id)
                     (content . ,graphql-content)))
         (result (shipit--graphql-query mutation variables)))
    (shipit--debug-log "🎯 ADD-REVIEW-REACTION-GRAPHQL: Adding %s to review %s" reaction-content node-id)
    (if result
        (progn
          (shipit--debug-log "✅ ADD-REVIEW-REACTION-GRAPHQL: Success - %S" result)
          result)
      (progn
        (shipit--debug-log "❌ ADD-REVIEW-REACTION-GRAPHQL: Failed - no result")
        nil))))

(defun shipit--remove-review-reaction-graphql (node-id reaction-content)
  "Remove a reaction from a PR review using GraphQL.
NODE-ID is the global GitHub node ID for the review.
REACTION-CONTENT is the emoji type (e.g., '+1', 'heart', 'laugh', etc.).
Returns the mutation result or nil on error."
  (let* ((mutation "mutation($nodeId: ID!, $content: ReactionContent!) {
  removeReaction(input: {subjectId: $nodeId, content: $content}) {
    reaction {
      id
      content
    }
  }
}")
         ;; Convert REST API reaction format to GraphQL format
         (graphql-content (cond
                          ((string= reaction-content "+1") "THUMBS_UP")
                          ((string= reaction-content "-1") "THUMBS_DOWN")
                          ((string= reaction-content "laugh") "LAUGH")
                          ((string= reaction-content "hooray") "HOORAY")
                          ((string= reaction-content "confused") "CONFUSED")
                          ((string= reaction-content "heart") "HEART")
                          ((string= reaction-content "rocket") "ROCKET")
                          ((string= reaction-content "eyes") "EYES")
                          (t reaction-content)))
         (variables `((nodeId . ,node-id)
                     (content . ,graphql-content)))
         (result (shipit--graphql-query mutation variables)))
    (shipit--debug-log "🎯 REMOVE-REVIEW-REACTION-GRAPHQL: Removing %s from review %s" reaction-content node-id)
    (if result
        (progn
          (shipit--debug-log "✅ REMOVE-REVIEW-REACTION-GRAPHQL: Success - %S" result)
          result)
      (progn
        (shipit--debug-log "❌ REMOVE-REVIEW-REACTION-GRAPHQL: Failed - no result")
        nil))))

(defun shipit-debug-api (endpoint)
  "Debug GitHub API call to ENDPOINT."
  (interactive "sAPI endpoint: ")
  (shipit--ensure-repository)
  (let* ((url (concat shipit-api-url
                      (if (string-prefix-p "/" endpoint)
                          endpoint
                        (format "/repos/%s/%s" shipit-current-repo endpoint))))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (message "Making request to: %s" url)
    (message "Headers: %S" headers)
    (shipit--url-retrieve-async
     url "GET" headers nil
     (lambda (data)
       (message "Success! HTTP 200")
       (message "Data type: %S" (type-of data))
       (message "Data length: %S" (if (listp data) (length data) "N/A"))
       (with-current-buffer (get-buffer-create "*GitHub Debug*")
         (erase-buffer)
         (insert (format "URL: %s\n" url))
         (insert (format "Status: %s\n" "200"))
         (insert (format "Data type: %S\n" (type-of data)))
         (insert (format "Data: %S\n" data))
         (display-buffer (current-buffer))))
     (lambda (error-msg)
       (message "Error: %s" error-msg)))))

(defun shipit--api-request (endpoint &optional params callback)
  "Make a GitHub API request to ENDPOINT with optional PARAMS.
If CALLBACK is provided, make an async request."
  (unless (shipit--github-token)
    (error "GitHub token not found.  Set `shipit-github-token' or add github.com to auth-source"))
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "🔍 API request debug:")
    (shipit--debug-log "  endpoint: %S (type: %s)" endpoint (type-of endpoint))
    (shipit--debug-log "  params: %S (type: %s)" params (type-of params))
    (shipit--debug-log "  shipit-api-url: %S (type: %s)" shipit-api-url (type-of shipit-api-url)))
  (let* ((url (if params
                  (concat (or shipit-api-url "https://api.github.com")
                         (or endpoint "")
                         "?"
                         (url-build-query-string params))
                (concat (or shipit-api-url "https://api.github.com")
                       (or endpoint ""))))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "API REQUEST: %s" url)
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "  headers: %S" headers))
    (if callback
        (shipit--url-retrieve-async
         url "GET" headers nil
         callback
         (lambda (error-msg)
           ;; Handle both string error messages and structured error objects like (error http 404)
           (let* ((error-string (cond
                                 ((stringp error-msg) error-msg)
                                 ((listp error-msg) (format "%s" error-msg))
                                 (t (format "%s" error-msg))))
                  (is-404 (string-match-p "404" error-string))
                  (is-reactions (string-match-p "/reactions" url)))
             (shipit--debug-log "API ERROR: %s - %s" url error-string)
             ;; Don't show user message for 404 errors on reactions endpoints (deleted comments)
             (unless (and is-404 is-reactions)
               (message "API error: %s" error-string))
             ;; Call callback with nil to prevent hanging parallel sync
             (funcall callback nil))))
      (condition-case err
          (let* ((result (shipit--url-retrieve-sync url "GET" headers nil))
                 (data (car result))
                 (status-code (cdr result)))
            (cond
             ;; Success: 2xx responses
             ((and status-code (>= status-code 200) (< status-code 300))
              data)
             ;; 304 Not Modified: ETag cache hit - data is in cache
             ((eq status-code 304)
              (shipit--debug-log "✓ Cache hit (304): %s" url)
              data)  ; url.el should have populated data from cache
             ;; Error: 4xx/5xx responses
             (t
              (let* ((is-404 (eq status-code 404))
                     (is-reactions (string-match-p "/reactions" url)))
                (shipit--debug-log "API ERROR: %s - HTTP %s" url status-code)
                (unless (and is-404 is-reactions)
                  (message "API error: HTTP %s" status-code))
                nil))))
        (error
         ;; Log the error for debugging but then re-throw it
         (when (fboundp 'shipit--debug-log)
           (shipit--debug-log "❌ API ERROR: %s - %s" url (error-message-string err)))
         ;; Re-throw the error instead of silently returning nil
         (signal (car err) (cdr err)))))))

(defun shipit--api-request-post-async (endpoint data callback &optional method)
  "Make async GitHub API POST request to ENDPOINT with DATA payload.
CALLBACK is called with the response data when request completes.
METHOD defaults to POST but can be PATCH, PUT, etc."
  (unless (shipit--github-token)
    (error "GitHub token not found.  Set `shipit-github-token' or add github.com to auth-source"))
  (let* ((url (concat shipit-api-url endpoint))
         (method (or method "POST"))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")
                        '("Content-Type" . "application/json")))
         (headers (remove nil headers))
         (json-data (json-encode data)))
    (shipit--debug-log "API REQUEST (ASYNC POST): %s" url)
    (shipit--url-retrieve-async
     url method headers json-data
     callback
     (lambda (error-msg)
       (message "API error: %s" error-msg)
       (shipit--debug-log "API ERROR: %s" error-msg)
       (funcall callback nil)))))

(defun shipit--api-request-post (endpoint data &optional method)
  "Make a GitHub API POST request to ENDPOINT with DATA payload.
METHOD defaults to POST but can be PATCH, PUT, etc."
  (unless (shipit--github-token)
    (error "GitHub token not found.  Set `shipit-github-token' or add github.com to auth-source"))
  (let* ((url (concat shipit-api-url endpoint))
         (method (or method "POST"))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")
                        '("Content-Type" . "application/json")))
         (headers (remove nil headers))
         (json-data (json-encode data)))
    (shipit--debug-log "API REQUEST (POST): %s" url)
    (condition-case err
        (let* ((result (shipit--url-retrieve-sync url method headers json-data))
               (response-data (car result))
               (status-code (cdr result)))
          (if (and status-code (>= status-code 200) (< status-code 300))
              response-data
            (progn
              (message "API error: HTTP %s" status-code)
              (shipit--debug-log "API ERROR: %s - HTTP %s" url status-code)
              nil)))
      (error
       (message "API error: %s" (error-message-string err))
       (shipit--debug-log "API ERROR: %s - %s" url (error-message-string err))
       nil))))

;; GraphQL API functions
(defun shipit--graphql-request (query &optional variables callback)
  "Execute a GraphQL QUERY with optional VARIABLES.
If CALLBACK is provided, execute asynchronously. Otherwise, execute synchronously."
  (let* ((url (concat (or shipit-api-url "https://api.github.com") "/graphql"))
         (headers (list (shipit--get-auth-header)
                       '("Content-Type" . "application/json")))
         (headers (remove nil headers))
         (payload (list (cons 'query query)))
         (payload (if variables
                     (cons (cons 'variables variables) payload)
                   payload))
         (json-data (json-encode payload)))
    (shipit--debug-log "GraphQL REQUEST: %s" query)
    (when variables
      (shipit--debug-log "GraphQL VARIABLES: %s" variables))
    (if callback
        ;; Async
        (shipit--url-retrieve-async
         url "POST" headers json-data
         callback
         (lambda (error-msg)
           (shipit--debug-log "GraphQL error: %s" error-msg)
           (message "GraphQL API error: %s" error-msg)
           (when callback (funcall callback nil))))
      ;; Sync
      (condition-case err
          (let* ((result (shipit--url-retrieve-sync url "POST" headers json-data))
                 (data (car result))
                 (status-code (cdr result)))
            (if (and status-code (>= status-code 200) (< status-code 300))
                data
              (progn
                (shipit--debug-log "GraphQL error: HTTP %s" status-code)
                (message "GraphQL API error: HTTP %s" status-code)
                nil)))
        (error
         (shipit--debug-log "GraphQL error: %s" (error-message-string err))
         (message "GraphQL API error: %s" (error-message-string err))
         nil)))))

(defun shipit--get-pr-id (repo pr-number)
  "Get the GraphQL node ID for PR-NUMBER in REPO."
  (let* ((query "query GetPullRequestId($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      id
    }
  }
}")
         (repo-parts (split-string repo "/"))
         (owner (car repo-parts))
         (repo-name (cadr repo-parts))
         (variables `((owner . ,owner)
                     (repo . ,repo-name)
                     (number . ,pr-number)))
         (response (shipit--graphql-request query variables)))
    (cdr (assq 'id (cdr (assq 'pullRequest (cdr (assq 'repository (cdr (assq 'data response))))))))))

(defun shipit--fetch-review-threads (repo pr-number &optional callback)
  "Fetch review threads with resolved status for PR-NUMBER in REPO.
If CALLBACK is provided, execute asynchronously."
  (let* ((query "query($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      reviewThreads(first: 50) {
        edges {
          node {
            id
            isResolved
            isOutdated
            isCollapsed
            line
            originalLine
            path
            subjectType
            comments(first: 20) {
              totalCount
              edges {
                node {
                  id
                  body
                  createdAt
                  updatedAt
                  author {
                    login
                    avatarUrl
                  }
                  originalPosition
                  position
                  diffHunk
                  path
                  line
                  originalLine
                  startLine
                  originalStartLine
                  reactions(first: 10) {
                    totalCount
                    edges {
                      node {
                        content
                        user {
                          login
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}")
         (repo-parts (split-string repo "/"))
         (owner (car repo-parts))
         (repo-name (cadr repo-parts))
         (variables `((owner . ,owner)
                     (repo . ,repo-name)
                     (number . ,pr-number))))
    (shipit--debug-log "Fetching review threads for PR %s in %s" pr-number repo)
    (if callback
        (shipit--graphql-request query variables callback)
      (shipit--graphql-request query variables))))

(defun shipit--parse-review-threads (response)
  "Parse GraphQL review threads response into resolved and unresolved lists."
  (let* ((data (cdr (assq 'data response)))
         (repository (cdr (assq 'repository data)))
         (pull-request (cdr (assq 'pullRequest repository)))
         (review-threads (cdr (assq 'reviewThreads pull-request)))
         (edges (cdr (assq 'edges review-threads)))
         (resolved-threads '())
         (unresolved-threads '()))

    (dolist (edge edges)
      (let* ((node (cdr (assq 'node edge)))
             (is-resolved (eq (cdr (assq 'isResolved node)) t))
             (is-outdated (eq (cdr (assq 'isOutdated node)) t))
             (thread-data (shipit--convert-graphql-thread-to-comment node)))
        (if is-resolved
            (push thread-data resolved-threads)
          (push thread-data unresolved-threads))))

    (list (cons 'resolved (nreverse resolved-threads))
          (cons 'unresolved (nreverse unresolved-threads)))))

(defun shipit--extract-comments-from-review-threads (threads)
  "Extract individual comments from review threads and mark them with resolved status."
  (let ((comments '()))
    (dolist (thread threads)
      (let* ((thread-resolved (cdr (assq 'is_resolved thread)))
             (thread-comments (cdr (assq 'comments thread))))
        (dolist (comment thread-comments)
          ;; Add resolved status to each comment
          (push (append comment `((resolved . ,thread-resolved))) comments))))
    (nreverse comments)))

(defun shipit--convert-graphql-thread-to-comment (thread-node)
  "Convert a GraphQL review thread node to shipit comment format."
  (let* ((thread-id (cdr (assq 'id thread-node)))
         (is-resolved (eq (cdr (assq 'isResolved thread-node)) t))
         (is-outdated (eq (cdr (assq 'isOutdated thread-node)) t))
         (is-collapsed (eq (cdr (assq 'isCollapsed thread-node)) t))
         (path (cdr (assq 'path thread-node)))
         (line (cdr (assq 'line thread-node)))
         (original-line (cdr (assq 'originalLine thread-node)))
         (start-line (cdr (assq 'startLine thread-node)))
         (comments-data (cdr (assq 'comments thread-node)))
         (comments-edges (cdr (assq 'edges comments-data)))
         (comments '()))

    ;; Convert each comment in the thread
    (dolist (comment-edge comments-edges)
      (let* ((comment-node (cdr (assq 'node comment-edge)))
             (comment-id (cdr (assq 'id comment-node)))
             (body (cdr (assq 'body comment-node)))
             (created-at (cdr (assq 'createdAt comment-node)))
             (updated-at (cdr (assq 'updatedAt comment-node)))
             (author-data (cdr (assq 'author comment-node)))
             (author-login (cdr (assq 'login author-data)))
             (avatar-url (cdr (assq 'avatarUrl author-data)))
             (position (cdr (assq 'position comment-node)))
             (original-position (cdr (assq 'originalPosition comment-node)))
             (diff-hunk (cdr (assq 'diffHunk comment-node)))
             (comment-line (cdr (assq 'line comment-node)))
             (comment-start-line (cdr (assq 'startLine comment-node)))
             (side (cdr (assq 'side comment-node)))
             (reactions-data (cdr (assq 'reactions comment-node)))
             (reactions-edges (cdr (assq 'edges reactions-data)))
             (reactions '()))

        ;; Convert reactions
        (dolist (reaction-edge reactions-edges)
          (let* ((reaction-node (cdr (assq 'node reaction-edge)))
                 (content (cdr (assq 'content reaction-node)))
                 (user-data (cdr (assq 'user reaction-node)))
                 (user-login (cdr (assq 'login user-data))))
            (push (list (cons 'content content)
                       (cons 'user (list (cons 'login user-login))))
                  reactions)))

        ;; Create comment in shipit format
        (push (list (cons 'id comment-id)
                   (cons 'body body)
                   (cons 'created_at created-at)
                   (cons 'updated_at updated-at)
                   (cons 'user (list (cons 'login author-login)
                                   (cons 'avatar_url avatar-url)))
                   (cons 'path path)
                   (cons 'line comment-line)
                   (cons 'start_line comment-start-line)
                   (cons 'original_line original-line)
                   (cons 'position position)
                   (cons 'original_position original-position)
                   (cons 'diff_hunk diff-hunk)
                   (cons 'side side)
                   (cons 'reactions reactions)
                   ;; Add thread-specific metadata
                   (cons 'thread_id thread-id)
                   (cons 'is_resolved is-resolved)
                   (cons 'is_outdated is-outdated)
                   (cons 'is_collapsed is-collapsed))
              comments)))

    ;; Return thread with its comments
    (list (cons 'thread_id thread-id)
          (cons 'is_resolved is-resolved)
          (cons 'is_outdated is-outdated)
          (cons 'is_collapsed is-collapsed)
          (cons 'path path)
          (cons 'line line)
          (cons 'start_line start-line)
          (cons 'original_line original-line)
          (cons 'comments (nreverse comments)))))

;; PR-related functions
(defun shipit--get-current-pr-data (branch repo)
  "Get PR data for current branch, with proper branch-aware caching."
  ;; Use the existing branch-aware cache from shipit-get-pr-for-branch
  (shipit-get-pr-for-branch branch repo))

(defun shipit--get-pr-approval-status (repo pr-number)
  "Get approval status for PR-NUMBER in REPO."
  (shipit--ensure-cache-initialized)  ; Ensure cache is properly initialized
  (let ((cache-key (format "%s:%s" repo pr-number)))
    ;; Return cached status if available
    (or (gethash cache-key shipit--cached-approval-status)
        ;; Fetch and cache status
    (let* ((reviews-endpoint (format "/repos/%s/pulls/%s/reviews" repo pr-number))
           (reviews (condition-case nil
                        (let* ((result (shipit-gh-etag-get-json-with-refresh-cache reviews-endpoint '((per_page . 100)) shipit-github-token)))
                          (plist-get result :json))
                      (error nil))))
      (if (null reviews)
          ;; Cache "Unknown" status too
          (progn
            (puthash cache-key "Unknown" shipit--cached-approval-status)
            "Unknown")
        (let ((latest-reviews (make-hash-table :test 'equal))
              (approval-count 0)
              (changes-requested 0)
              (pending-count 0))
          ;; Get the latest review from each reviewer
          (dolist (review reviews)
            (let* ((user (cdr (assq 'login (cdr (assq 'user review)))))
                   (state (cdr (assq 'state review)))
                   (submitted-at (cdr (assq 'submitted_at review))))
              (when (and user state submitted-at)
                ;; Keep only the latest review from each user
                (let ((existing (gethash user latest-reviews)))
                  (when (or (null existing)
                            (string< (cdr (assq 'submitted_at existing)) submitted-at))
                    (puthash user review latest-reviews))))))

          ;; Count approval states
          (maphash (lambda (user review)
                     (let ((state (cdr (assq 'state review))))
                       (cond
                        ((string= state "APPROVED") (setq approval-count (1+ approval-count)))
                        ((string= state "CHANGES_REQUESTED") (setq changes-requested (1+ changes-requested)))
                        ((string= state "PENDING") (setq pending-count (1+ pending-count))))))
                   latest-reviews)

          ;; Get requested reviewer details for better status
          (let* ((requested-endpoint (format "/repos/%s/pulls/%s/requested_reviewers" repo pr-number))
                 (requested-data (condition-case nil
                                     (shipit--api-request requested-endpoint)
                                   (error nil)))
                 (requested-users (when requested-data (length (cdr (assq 'users requested-data)))))
                 (requested-teams (when requested-data (length (cdr (assq 'teams requested-data)))))
                 (total-requested (+ (or requested-users 0) (or requested-teams 0))))

            ;; Return enhanced status summary and cache it
            (let ((status (cond
                           ((> changes-requested 0)
                            (format "❌ Changes Requested (%d)" changes-requested))
                           ((and (> approval-count 0) (= total-requested 0))
                            (format "✅ Approved (%d)" approval-count))
                           ((and (> approval-count 0) (> total-requested 0))
                            (format "🔄 Partially Approved (%d, %d pending)" approval-count total-requested))
                           ((> total-requested 0)
                            (format "%s Review Requested (%d pending)" (shipit--get-status-icon "waiting" "⏳") total-requested))
                           ((> pending-count 0)
                            (format "%s Pending (%d)" (shipit--get-status-icon "waiting" "⏳") pending-count))
                           (t "📝 No Reviews"))))
              (puthash cache-key status shipit--cached-approval-status)
              status))))))))

(defun shipit--parse-required-owners-from-codeowners (codeowners-text changed-files)
  "Parse CODEOWNERS file and return list of required owners for CHANGED-FILES.
Returns list of owners (users and teams) that must approve based on file patterns.
Implements GitHub's 'last matching rule wins' precedence."
  (shipit--debug-log "CODEOWNERS function called: changed-files=%S" changed-files)

  (let ((file-paths (if (and (consp (car changed-files)) (assq 'path (car changed-files)))
                        ;; GraphQL format: ((path . "file.py") ...)
                        (mapcar (lambda (f) (cdr (assq 'path f))) changed-files)
                      ;; Already strings: ("file.py" ...)
                      changed-files))
        (file-owners-map (make-hash-table :test 'equal))
        (all-required-owners '()))

    (shipit--debug-log "CODEOWNERS parsing: codeowners-length=%s file-paths=%S"
                       (if codeowners-text (length codeowners-text) 0) file-paths)

    (when (and codeowners-text file-paths)
      ;; Parse all CODEOWNERS rules and apply "last matching rule wins" per file
      (with-temp-buffer
        (insert codeowners-text)
        (goto-char (point-min))

        ;; Parse each line of CODEOWNERS
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
            ;; Skip comments and empty lines
            (unless (or (string-empty-p line) (string-prefix-p "#" line))
              (let* ((parts (split-string line))
                     (pattern (car parts))
                     (owners (cdr parts)))
                (when (and pattern owners)
                  (shipit--debug-log "Testing pattern: '%s' -> owners: %S" pattern owners)
                  ;; Check each changed file against this pattern
                  (dolist (file-path file-paths)
                    (let ((matches (shipit--codeowners-pattern-matches-p pattern file-path)))
                      (when matches
                        (shipit--debug-log "  ✅ Pattern '%s' matches file '%s'" pattern file-path)
                        ;; Last matching rule wins - overwrite previous owners for this file
                        (puthash file-path owners file-owners-map))
                      (unless matches
                        (shipit--debug-log "  ❌ Pattern '%s' does not match file '%s'" pattern file-path)))))))
            (forward-line 1)))

      ;; Collect all unique owners from the final file->owner mappings
      (maphash (lambda (file owners)
                 (shipit--debug-log "Final owners for '%s': %S" file owners)
                 (dolist (owner owners)
                   ;; Ensure owner starts with @ (GitHub format)
                   (let ((clean-owner (if (string-prefix-p "@" owner) owner (concat "@" owner))))
                     (unless (member clean-owner all-required-owners)
                       (push clean-owner all-required-owners)))))
               file-owners-map))

      (shipit--debug-log "Final required owners: %S" all-required-owners)
      all-required-owners)))

(defun shipit--codeowners-pattern-matches-p (pattern file-path)
  "Check if CODEOWNERS PATTERN matches FILE-PATH following GitHub's format.
Supports:
- Root-relative paths (starting with /)
- Wildcards: * (single level), ** (recursive), ? (single char)
- Directory patterns (ending with /)
- Negation patterns (starting with !)
- Exact matches and prefix matches"
  (let ((is-negation (string-prefix-p "!" pattern))
        (clean-pattern (if (string-prefix-p "!" pattern)
                          (substring pattern 1)
                        pattern)))

    ;; Handle root-relative paths (remove leading /)
    (when (string-prefix-p "/" clean-pattern)
      (setq clean-pattern (substring clean-pattern 1)))

    (let ((matches
           (cond
            ;; Exact match
            ((string= clean-pattern file-path) t)
            ;; Directory pattern (ends with /)
            ((string-suffix-p "/" clean-pattern)
             (string-prefix-p clean-pattern file-path))
            ;; Has wildcards - convert to regex
            ((string-match-p "[*?]" clean-pattern)
             (let ((regex clean-pattern))
               ;; Handle special wildcard combinations first
               ;; Replace .* with DOT_STAR placeholder (any characters including /)
               (setq regex (replace-regexp-in-string "\\.\\*" "DOT_STAR" regex))
               ;; Replace ** with DOUBLE_STAR placeholder (recursive directories)
               (setq regex (replace-regexp-in-string "\\*\\*" "DOUBLE_STAR" regex))
               ;; Replace single * with SINGLE_STAR placeholder (single level)
               (setq regex (replace-regexp-in-string "\\*" "SINGLE_STAR" regex))
               ;; Replace ? with QUESTION placeholder (single character)
               (setq regex (replace-regexp-in-string "\\?" "QUESTION" regex))
               ;; Now escape all regex special chars
               (setq regex (regexp-quote regex))
               ;; Convert placeholders to their regex equivalents
               (setq regex (replace-regexp-in-string "DOT_STAR" ".*" regex))
               (setq regex (replace-regexp-in-string "DOUBLE_STAR" ".*" regex))
               (setq regex (replace-regexp-in-string "SINGLE_STAR" "[^/]*" regex))
               (setq regex (replace-regexp-in-string "QUESTION" "." regex))
               (let* ((final-regex (concat "^" regex "$"))
                      (match-result (string-match-p final-regex file-path)))
                 (not (null match-result)))))
            ;; Default: prefix match
            (t (string-prefix-p clean-pattern file-path)))))

      ;; Apply negation if needed
      (if is-negation (not matches) matches))))

(defun shipit--process-latest-reviews (latest-reviews)
  "Process latest reviews to create lookup table of user -> state."
  (let ((review-states (make-hash-table :test 'equal)))
    (dolist (review latest-reviews)
      (let ((author-login (cdr (assq 'login (cdr (assq 'author review)))))
            (state (cdr (assq 'state review)))
            (submitted-at (cdr (assq 'submittedAt review)))
            (body (cdr (assq 'body review))))
        (when author-login
          (puthash author-login
                   (list (cons 'state state)
                         (cons 'submitted-at submitted-at)
                         (cons 'body body))
                   review-states))))
    review-states))

(defun shipit--get-approved-users (review-states-hash)
  "Extract list of users who have APPROVED from review states hash."
  (let ((approved-users '()))
    (maphash (lambda (user review-info)
               (when (string= (cdr (assq 'state review-info)) "APPROVED")
                 (push user approved-users)))
             review-states-hash)
    approved-users))

(defun shipit--compute-blocking-owners (required-owners approved-users org-name)
  "Compute which required owners are still blocking (haven't approved).
ORG-NAME is used for team membership checks (when available)."
  (let ((blocking-owners '()))
    (dolist (owner required-owners)
      (cond
       ;; User owner - check if they approved
       ((and (not (string-prefix-p "@" owner)) (not (string-match-p "/" owner)))
        (unless (member owner approved-users)
          (push owner blocking-owners)))
       ;; Team owner (@org/team or org/team)
       ((or (string-prefix-p "@" owner) (string-match-p "/" owner))
        (let ((team-satisfied (shipit--team-requirement-satisfied-p
                               owner approved-users org-name)))
          (unless team-satisfied
            (push owner blocking-owners))))
       ;; Unknown format - treat as user
       (t
        (unless (member owner approved-users)
          (push owner blocking-owners)))))

    (nreverse blocking-owners)))

(defun shipit--team-requirement-satisfied-p (team-owner approved-users org-name)
  "Check if team requirement is satisfied by any approved user.
PERFORMANCE OPTIMIZED: Uses batch team membership lookup with ETag caching."
  (shipit--debug-log "Checking if team %s is satisfied by any of %d approved users"
                     team-owner (length approved-users))

  (when (and org-name approved-users)
    (let ((satisfied-by nil)
          (clean-team (replace-regexp-in-string "^@\\(?:[^/]+/\\)?" "" team-owner)))

      ;; OPTIMIZATION: Check each user until we find a match, with early termination
      (dolist (user approved-users)
        (when (and (not satisfied-by) user)  ; Stop at first match
          (let* ((user-login (if (stringp user) user (cdr (assq 'login user))))
                 (user-teams (when user-login
                              (shipit--get-user-teams-in-org user-login org-name))))
            (when (member clean-team user-teams)
              (setq satisfied-by user-login)
              (shipit--debug-log "✅ Team %s satisfied by user: %s" team-owner user-login)))))

      (unless satisfied-by
        (shipit--debug-log "❌ Team %s not satisfied by any approved user" team-owner))

      satisfied-by)))

(defun shipit--get-branch-protection-requirements (repo base-ref-name)
  "Get branch protection requirements for BASE-REF-NAME in REPO.
Returns alist with required approval count and code owner requirements."
  (let* ((endpoint (format "/repos/%s/branches/%s/protection/required_pull_request_reviews" repo base-ref-name))
         (protection-result (condition-case err
                               (shipit-gh-etag-get-json-with-refresh-cache endpoint nil shipit-github-token)
                             (error
                              (shipit--debug-log "Branch protection API error: %S" err)
                              nil)))
         (protection-info (when protection-result (plist-get protection-result :json))))

    (shipit--debug-log "Branch protection API endpoint: %s" endpoint)
    (shipit--debug-log "Branch protection response: %S" protection-info)

    (if protection-info
        (let ((required-count (cdr (assq 'required_approving_review_count protection-info)))
              (require-codeowners (cdr (assq 'require_code_owner_reviews protection-info)))
              (dismiss-stale (cdr (assq 'dismiss_stale_reviews protection-info))))

          (shipit--debug-log "Branch protection rules: required=%s codeowners=%s dismiss-stale=%s"
                             required-count require-codeowners dismiss-stale)

          (list (cons 'required-approving-review-count (or required-count 0))
                (cons 'require-code-owner-reviews (eq require-codeowners t))
                (cons 'dismiss-stale-reviews (eq dismiss-stale t))
                (cons 'api-accessible t)))

      (shipit--debug-log "Branch protection API inaccessible (404/403) - likely no protection rules or insufficient permissions")
      (list (cons 'required-approving-review-count nil)  ; nil = unknown
            (cons 'require-code-owner-reviews nil)
            (cons 'dismiss-stale-reviews nil)
            (cons 'api-accessible nil)))))

(defun shipit--fetch-codeowners-from-base-branch (repo base-ref-name)
  "Fetch CODEOWNERS file from the base branch REF in REPO using REST API with ETag caching.
Tries common locations: root, .github/, and docs/. Much faster than GraphQL version.
Results are cached to avoid repeated API calls for the same repo/branch combination."
  (let* ((cache-key (format "%s:%s" repo base-ref-name))
         (cached-value (gethash cache-key shipit--codeowners-cache 'not-found)))

    ;; Return cached value immediately if available (including nil from previous lookup)
    (if (not (eq cached-value 'not-found))
        (progn
          (shipit--debug-log "CODEOWNERS cache HIT for %s" cache-key)
          cached-value)

      ;; Cache miss - fetch from API
      (let* ((codeowners-locations '("CODEOWNERS" ".github/CODEOWNERS" "docs/CODEOWNERS"))
             (codeowners-text nil))

        ;; Try each location with REST API + ETag caching until we find one
        (catch 'found-codeowners
          (dolist (location codeowners-locations)
            (let* ((endpoint (format "/repos/%s/contents/%s" repo location))
                   ;; Add ref parameter to get from specific branch
                   (params `((ref . ,base-ref-name)))
                   (result (condition-case err
                              (shipit-gh-etag-get-json-with-refresh-cache endpoint params shipit-github-token)
                            (error
                             (shipit--debug-log "CODEOWNERS not found at %s: %s" location (error-message-string err))
                             nil)))
                   (file-data (when result (plist-get result :json)))
                   (content (when file-data (cdr (assq 'content file-data))))
                   (encoding (when file-data (cdr (assq 'encoding file-data)))))

              (when (and content (string= encoding "base64"))
                (setq codeowners-text (base64-decode-string content))
                (shipit--debug-log "CODEOWNERS found at %s in branch '%s' (length: %d)"
                                   location base-ref-name (length codeowners-text))
                (throw 'found-codeowners codeowners-text)))))

        (shipit--debug-log "CODEOWNERS fetch completed for branch '%s': found=%s length=%s"
                           base-ref-name (not (null codeowners-text))
                           (if codeowners-text (length codeowners-text) 0))

        ;; Cache the result (including nil for "not found") to avoid repeated API calls
        (puthash cache-key codeowners-text shipit--codeowners-cache)
        (shipit--debug-log "CODEOWNERS cache STORE for %s (value: %s)"
                           cache-key (if codeowners-text "content found" "nil"))

        codeowners-text))))

(defun shipit--review-state-priority (state)
  "Get the priority of a review state for determining which review to keep per user.
Higher numbers indicate higher priority states that should override lower priority ones."
  (cond
   ((string= state "CHANGES_REQUESTED") 3)  ; Highest priority - blocks merge
   ((string= state "APPROVED") 2)           ; High priority - enables merge
   ((string= state "COMMENTED") 1)          ; Lowest priority - informational only
   (t 0)))                                  ; Unknown states get lowest priority

(defun shipit--get-latest-reviews-per-user (reviews-data)
  "Get the latest meaningful review from each user from REVIEWS-DATA.
REST API returns all reviews chronologically. We keep the most recent APPROVED,
CHANGES_REQUESTED, or DISMISSED review per user, as COMMENTED reviews don't
change approval status.  DISMISSED reviews are meaningful because they revoke
previous approvals.  This matches GitHub's behavior."
  (let ((user-reviews (make-hash-table :test 'equal))
        (latest-reviews '()))

    ;; Group reviews by user, keeping the most recent meaningful review
    ;; A meaningful review is APPROVED, CHANGES_REQUESTED, or DISMISSED
    ;; COMMENTED reviews only count if there's no meaningful review yet
    (dolist (review reviews-data)
      (let* ((author-login (cdr (assq 'login (cdr (assq 'user review)))))
             (submitted-at (cdr (assq 'submitted_at review)))
             (review-state (cdr (assq 'state review)))
             (existing-review (gethash author-login user-reviews))
             (is-meaningful (or (string= review-state "APPROVED")
                                (string= review-state "CHANGES_REQUESTED")
                                (string= review-state "DISMISSED")))
             (existing-state (when existing-review
                               (cdr (assq 'state existing-review))))
             (existing-is-meaningful (and existing-state
                                          (or (string= existing-state "APPROVED")
                                              (string= existing-state "CHANGES_REQUESTED")
                                              (string= existing-state "DISMISSED")))))
        (when author-login
          (cond
           ;; No existing review, add this one
           ((null existing-review)
            (puthash author-login review user-reviews))

           ;; New review is meaningful (APPROVED/CHANGES_REQUESTED)
           (is-meaningful
            (let ((existing-time (cdr (assq 'submitted_at existing-review))))
              (cond
               ;; Existing is also meaningful - compare timestamps
               ((and existing-is-meaningful (string< existing-time submitted-at))
                (shipit--debug-log "🔄 REVIEW-LATEST: %s - replacing with newer meaningful review (state: %s)"
                                   author-login review-state)
                (puthash author-login review user-reviews))
               ;; Existing is NOT meaningful - meaningful always wins
               ((not existing-is-meaningful)
                (shipit--debug-log "🔄 REVIEW-LATEST: %s - replacing COMMENTED with meaningful review (state: %s)"
                                   author-login review-state)
                (puthash author-login review user-reviews)))))

           ;; New review is COMMENTED, only update if existing is also COMMENTED and older
           ((not existing-is-meaningful)
            (let ((existing-time (cdr (assq 'submitted_at existing-review))))
              (when (string< existing-time submitted-at)
                (shipit--debug-log "🔄 REVIEW-LATEST: %s - replacing COMMENTED with newer COMMENTED"
                                   author-login)
                (puthash author-login review user-reviews))))))))

    ;; Convert hash table back to list with GraphQL-compatible format
    (maphash (lambda (login review)
               (push `((author . ((login . ,login)))
                       (state . ,(cdr (assq 'state review)))
                       (submittedAt . ,(cdr (assq 'submitted_at review)))
                       (body . ,(cdr (assq 'body review))))
                     latest-reviews))
             user-reviews)

    (shipit--debug-log "Processed %d total reviews into %d latest reviews per user"
                       (length reviews-data) (length latest-reviews))
    latest-reviews))

(defun shipit--get-codeowners-based-decision (latest-reviews changed-files repo base-ref-name org-name)
  "Get review decision based on CODEOWNERS file requirements for changed files.
This is the primary logic that determines which teams actually need to approve based on file ownership.
Returns 'APPROVED', 'REVIEW_REQUIRED', or nil (fallback to basic logic)."
  (shipit--debug-log "🎯 CODEOWNERS-PRIMARY: Checking file-based ownership requirements")

  (when (and latest-reviews changed-files)
    (condition-case err
        (let* ((codeowners-text (shipit--fetch-codeowners-from-base-branch repo base-ref-name))
               (approved-reviewers (cl-remove-if-not
                                   (lambda (review) (string= (cdr (assq 'state review)) "APPROVED"))
                                   latest-reviews))
               (approved-usernames (mapcar (lambda (review)
                                           (cdr (assq 'login (cdr (assq 'user review)))))
                                         approved-reviewers)))

          (if (null codeowners-text)
              (progn
                (shipit--debug-log "🎯 CODEOWNERS-PRIMARY: No CODEOWNERS file found, falling back to basic logic")
                nil) ; Fall back to basic logic

            ;; CODEOWNERS exists - this is the authoritative logic
            (let* ((file-ownership-map (shipit--get-file-ownership-requirements codeowners-text changed-files))
                   (all-satisfied (shipit--are-file-ownership-requirements-satisfied
                                  file-ownership-map approved-usernames org-name)))

              (shipit--debug-log "🎯 CODEOWNERS-PRIMARY: %d files have ownership requirements, approved=%S satisfied=%s"
                                 (hash-table-count file-ownership-map) approved-usernames all-satisfied)

              ;; Return definitive decision based on file ownership
              (if all-satisfied
                  (progn
                    (shipit--debug-log "🎯 CODEOWNERS-PRIMARY: All file ownership requirements satisfied → APPROVED")
                    "APPROVED")
                (progn
                  (shipit--debug-log "🎯 CODEOWNERS-PRIMARY: File ownership requirements NOT satisfied → REVIEW_REQUIRED")
                  "REVIEW_REQUIRED")))))
      (error
       (shipit--debug-log "🎯 CODEOWNERS-PRIMARY: Error in file-based analysis: %S, falling back" err)
       nil))))

(defun shipit--parse-required-owners-from-codeowners-fast (codeowners-text changed-files)
  "Fast version of CODEOWNERS parsing with minimal logging.
Only logs summary information to avoid performance issues."
  (let ((file-paths (if (and (consp (car changed-files)) (assq 'path (car changed-files)))
                        (mapcar (lambda (f) (cdr (assq 'path f))) changed-files)
                      changed-files))
        (file-owners-map (make-hash-table :test 'equal))
        (all-required-owners '()))

    (when (and codeowners-text file-paths)
      (with-temp-buffer
        (insert codeowners-text)
        (goto-char (point-min))

        ;; Parse each line of CODEOWNERS
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
            ;; Skip comments and empty lines
            (unless (or (string-empty-p line) (string-prefix-p "#" line))
              (let* ((parts (split-string line))
                     (pattern (car parts))
                     (owners (cdr parts)))
                (when (and pattern owners)
                  ;; Check each changed file against this pattern (no logging per check)
                  (dolist (file-path file-paths)
                    (when (shipit--codeowners-pattern-matches-p pattern file-path)
                      ;; Last matching rule wins - overwrite previous owners for this file
                      (puthash file-path owners file-owners-map))))))
            (forward-line 1)))

        ;; Collect all unique required owners
        (maphash (lambda (file owners)
                   (dolist (owner owners)
                     (unless (member owner all-required-owners)
                       (push owner all-required-owners))))
                 file-owners-map))

      (shipit--debug-log "CODEOWNERS fast parse: %d files -> %d required owners"
                         (length file-paths) (length all-required-owners)))

    all-required-owners))

(defun shipit--get-file-ownership-requirements (codeowners-text changed-files)
  "Parse CODEOWNERS and return file -> required-owners mapping.
Returns hash-table where keys are file paths and values are lists of required owners.
This enables file-centric ownership checking instead of team-centric."
  (let ((file-paths (if (and (consp (car changed-files)) (assq 'path (car changed-files)))
                        (mapcar (lambda (f) (cdr (assq 'path f))) changed-files)
                      changed-files))
        (file-owners-map (make-hash-table :test 'equal)))

    (when (and codeowners-text file-paths)
      (with-temp-buffer
        (insert codeowners-text)
        (goto-char (point-min))

        ;; Parse each line of CODEOWNERS
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
            ;; Skip comments and empty lines
            (unless (or (string-empty-p line) (string-prefix-p "#" line))
              (let* ((parts (split-string line))
                     (pattern (car parts))
                     (owners (cdr parts)))
                (when (and pattern owners)
                  ;; Check each changed file against this pattern
                  (dolist (file-path file-paths)
                    (when (shipit--codeowners-pattern-matches-p pattern file-path)
                      ;; Last matching rule wins - overwrite previous owners for this file
                      (puthash file-path owners file-owners-map))))))
            (forward-line 1))))

    (shipit--debug-log "File ownership map: %d files have ownership requirements"
                       (hash-table-count file-owners-map))
    file-owners-map))

(defun shipit--are-file-ownership-requirements-satisfied (file-ownership-map approved-usernames org-name)
  "Check if all files' ownership requirements are satisfied by approved reviewers.
For each file, at least one of its required owners must be in approved-usernames.
This correctly handles overlapping team ownership.
PERFORMANCE: Caches team memberships upfront to avoid repeated API calls."
  (if (= (hash-table-count file-ownership-map) 0)
      t  ;; No ownership requirements = satisfied

    (let ((all-satisfied t)
          (files-checked 0)
          (files-satisfied 0)
          ;; OPTIMIZATION: Cache team memberships for all approved users upfront
          (user-teams-cache (make-hash-table :test 'equal)))

      ;; Pre-fetch all approved users' team memberships once
      (dolist (username approved-usernames)
        (puthash username
                 (shipit--get-user-teams-in-org username org-name)
                 user-teams-cache))

      (maphash (lambda (file-path required-owners)
         (setq files-checked (1+ files-checked))
         (let ((file-satisfied nil))

           ;; Check if any required owner for this file is in approved reviewers
           (dolist (required-owner required-owners)
             (cond
              ;; Direct user approval
              ((and (string-prefix-p "@" required-owner)
                    (not (string-match-p "/" required-owner)))
               (let ((username (substring required-owner 1)))
                 (when (member username approved-usernames)
                   (setq file-satisfied t)
                   (shipit--debug-log "  ✅ File '%s' satisfied by direct user: %s" file-path username))))

              ;; Team approval - check if any approved user is team member
              ((string-prefix-p "@" required-owner)
               (let* ((clean-team (replace-regexp-in-string "^@\\(?:[^/]+/\\)?" "" required-owner))
                      (team-satisfied nil))
                 ;; Use cached team memberships instead of fetching for each username
                 (dolist (username approved-usernames)
                   (let ((user-teams (gethash username user-teams-cache)))
                     (when (member clean-team user-teams)
                       (setq team-satisfied t)
                       (setq file-satisfied t)
                       (shipit--debug-log "  ✅ File '%s' satisfied by team member: %s -> %s"
                                          file-path username clean-team))))
                 (unless team-satisfied
                   (shipit--debug-log "  ❌ File '%s' NOT satisfied, team '%s' has no approved members"
                                      file-path clean-team)))))))

           (if file-satisfied
               (setq files-satisfied (1+ files-satisfied))
             (setq all-satisfied nil)
             (shipit--debug-log "  ❌ File '%s' ownership NOT satisfied: %S" file-path required-owners))))
               file-ownership-map)

      (shipit--debug-log "File ownership summary: %d/%d files satisfied, overall: %s"
                         files-satisfied files-checked all-satisfied)
      all-satisfied)))

(defun shipit--compute-review-decision-from-reviews (latest-reviews)
  "Compute the overall review decision from LATEST-REVIEWS.
GitHub's GraphQL provides reviewDecision, but REST API requires computing it."
  (if (null latest-reviews)
      nil
    (let ((has-changes-requested nil)
          (has-approved nil))

      ;; Check review states to determine overall decision
      (dolist (review latest-reviews)
        (let ((state (cdr (assq 'state review))))
          (cond
           ((string= state "CHANGES_REQUESTED")
            (setq has-changes-requested t))
           ((string= state "APPROVED")
            (setq has-approved t)))))

      (cond
       (has-changes-requested "CHANGES_REQUESTED")
       (has-approved "APPROVED")
       (t "REVIEW_REQUIRED")))))

(defun shipit--compute-requested-team-approvals (requested-teams approved-users org-name)
  "Compute approval status for each REQUESTED-TEAMS based on APPROVED-USERS.
Returns alist of (team-slug . approval-info) where approval-info contains:
- approved: boolean
- approved-by: username who approved on behalf of team (if any)
Follows GitHub's logic: any team member approval satisfies the team requirement."
  (shipit--debug-log "Computing team approvals for %d teams, %d approved users"
                     (length requested-teams) (length approved-users))

  (let ((team-status '()))
    (dolist (team requested-teams)
      (let* ((team-slug (cdr (assq 'slug team)))
             (team-name (cdr (assq 'name team)))
             (approved-by nil)
             (team-satisfied nil))

        ;; Check if any approved user is a member of this team
        (dolist (approved-user approved-users)
          (when (and (not team-satisfied) approved-user org-name)
            (let* ((user-login (if (stringp approved-user) approved-user (cdr (assq 'login approved-user))))
                   (user-teams (when user-login (shipit--get-user-teams-in-org user-login org-name))))
              (when (member team-slug user-teams)
                (setq team-satisfied t)
                (setq approved-by user-login)
                (shipit--debug-log "✅ Team @%s approved by %s" team-slug user-login)))))

        (unless team-satisfied
          (shipit--debug-log "⏳ Team @%s still pending approval" team-slug))

        ;; Add team status to result
        (push `(,team-slug . ((approved . ,team-satisfied)
                             (approved-by . ,approved-by)
                             (team-name . ,team-name)))
              team-status)))

    team-status))

(defun shipit--parallel-requests (requests-spec on-complete)
  "Execute multiple async requests in parallel, call ON-COMPLETE when all finish.
REQUESTS-SPEC: list of (name endpoint params) tuples
ON-COMPLETE: callback (lambda (results-alist) ...) where results-alist has (name . plist-result)
Note: For endpoints returning multiple pages (e.g., /files), uses pagination automatically."
  (let ((total-requests (length requests-spec))
        (completed-requests 0)
        (results-alist '()))
    (dolist (request-spec requests-spec)
      (let ((name (car request-spec))
            (endpoint (cadr request-spec))
            (params (caddr request-spec)))
        (shipit--debug-log "🚀 PARALLEL: Starting async request for %s" name)

        ;; Use paginated async for files endpoint to handle PRs with >100 files
        (if (string-match "/files" endpoint)
            (shipit-gh-etag-get-json-paginated-async endpoint params shipit-github-token
              (lambda (plist-result)
                (shipit--debug-log "✅ PARALLEL: Completed %s (paginated, %d items)"
                                  name (length (plist-get plist-result :json)))
                (push (cons name plist-result) results-alist)
                (setq completed-requests (1+ completed-requests))
                (when (>= completed-requests total-requests)
                  (shipit--debug-log "✅ PARALLEL: All %d requests completed" total-requests)
                  (funcall on-complete results-alist))))
          ;; Use basic async for other endpoints
          (shipit-gh-etag-get-json-async endpoint params shipit-github-token
            (lambda (plist-result)
              (shipit--debug-log "✅ PARALLEL: Completed %s" name)
              (push (cons name plist-result) results-alist)
              (setq completed-requests (1+ completed-requests))
              (when (>= completed-requests total-requests)
                (shipit--debug-log "✅ PARALLEL: All %d requests completed" total-requests)
                (funcall on-complete results-alist)))))))))

(defun shipit--get-pr-review-decision-detailed (repo pr-number)
  "Get detailed GitHub review decision info using REST API with ETag caching.
Returns alist with reviewDecision, pending requests, and completed reviews."
  (shipit--debug-log "Using REST API for PR review decision data - PR #%s in %s" pr-number repo)

  ;; Check cache first
  (if shipit--cached-review-decision
      (progn
        (shipit--debug-log "✅ Using cached review decision data for PR #%s" pr-number)
        shipit--cached-review-decision)

    ;; Cache miss, fetch fresh data using parallel requests
    (shipit--debug-log "🔄 Cache miss, fetching review decision data for PR #%s using PARALLEL requests" pr-number)
    (let* ((profiling-timer (shipit--profiling-start "fetch PR review decision data (parallel)"))
           (repo-parts (split-string repo "/"))
           (owner (car repo-parts))
           (repo-name (cadr repo-parts))
           ;; Flags to track completion
           (pr-done nil)
           (files-done nil)
           (reviews-done nil)
           (requested-reviewers-done nil)
           ;; Storage for results
           (pr-data nil)
           (changed-files nil)
           (reviews-data nil)
           (requested-reviewers-data nil))

      ;; Start all 4 API requests in parallel
      (shipit--parallel-requests
       `((pr ,(format "/repos/%s/pulls/%s" repo pr-number) nil)
         (files ,(format "/repos/%s/pulls/%s/files" repo pr-number) ((per_page . 100)))
         (reviews ,(format "/repos/%s/pulls/%s/reviews" repo pr-number) ((per_page . 100)))
         (requested-reviewers ,(format "/repos/%s/pulls/%s/requested_reviewers" repo pr-number) ((per_page . 100))))
       (lambda (results-alist)
         (shipit--debug-log "✅ PARALLEL: All PR review decision requests completed")
         ;; Extract results from alist (now plists with :json key)
         (let ((pr-plist (cdr (assoc 'pr results-alist))))
           (setq pr-data (plist-get pr-plist :json)
                 pr-done t)
           (when (plist-get pr-plist :error)
             (shipit--debug-log "⚠️ PARALLEL: PR fetch error: %s" (plist-get pr-plist :error))))

         (let* ((files-plist (cdr (assoc 'files results-alist)))
                (files-raw (plist-get files-plist :json)))
           (when (plist-get files-plist :error)
             (shipit--debug-log "⚠️ PARALLEL: Files fetch error: %s" (plist-get files-plist :error)))
           (when (plist-get files-plist :partial)
             (shipit--debug-log "⚠️ PARALLEL: Files result is partial (pagination error)"))
           (setq changed-files
                 (when files-raw
                   (mapcar (lambda (file)
                             `((path . ,(cdr (assq 'filename file)))))
                           files-raw))
                 files-done t))

         (let ((reviews-plist (cdr (assoc 'reviews results-alist))))
           (setq reviews-data (plist-get reviews-plist :json)
                 reviews-done t)
           (when (plist-get reviews-plist :error)
             (shipit--debug-log "⚠️ PARALLEL: Reviews fetch error: %s" (plist-get reviews-plist :error))))

         (let ((reviewers-plist (cdr (assoc 'requested-reviewers results-alist))))
           (setq requested-reviewers-data (plist-get reviewers-plist :json)
                 requested-reviewers-done t)
           (when (plist-get reviewers-plist :error)
             (shipit--debug-log "⚠️ PARALLEL: Requested reviewers fetch error: %s" (plist-get reviewers-plist :error))))))

      ;; Wait for all parallel requests to complete
      (let ((start-time (current-time))
            (timeout-seconds 30))
        (while (and (not (and pr-done files-done reviews-done requested-reviewers-done))
                    (< (float-time (time-subtract (current-time) start-time)) timeout-seconds))
          (sleep-for 0.1)
          (accept-process-output nil 0.01))

        ;; Check if all requests completed
        (unless (and pr-done files-done reviews-done requested-reviewers-done)
          (error "Parallel PR review decision fetch timeout - pr=%s files=%s reviews=%s requested-reviewers=%s"
                 pr-done files-done reviews-done requested-reviewers-done)))

      ;; Now process the fetched data (same logic as before)
      (let* ((base-ref-name (when pr-data (cdr (assq 'ref (cdr (assq 'base pr-data))))))
             ;; Process reviews to get latest reviews per user (REST API gives all reviews, not just latest)
             (latest-reviews (when reviews-data
                              (shipit--get-latest-reviews-per-user reviews-data)))

             ;; Compute review decision from latest reviews (REST API doesn't provide reviewDecision field)
             (basic-review-decision (shipit--compute-review-decision-from-reviews latest-reviews))

             ;; Get CODEOWNERS-based decision (primary logic)
             (codeowners-decision (shipit--get-codeowners-based-decision
                                  latest-reviews changed-files repo base-ref-name owner))
             (review-decision (or codeowners-decision basic-review-decision))

             ;; Skip CODEOWNERS and branch protection - focus on requested reviewer approval status
             (protection-requirements nil))

        ;; Debug logging
        (shipit--debug-log "REST API PR review overview for %s PR #%s" repo pr-number)
        (shipit--debug-log "PR data fetched: %s" (not (null pr-data)))
        (shipit--debug-log "Files fetched: %s" (length (or changed-files '())))
        (shipit--debug-log "Reviews fetched: %s" (length (or reviews-data '())))
        (shipit--debug-log "Review decision: %s" review-decision)
        (shipit--debug-log "Base ref: %s" base-ref-name)
        (shipit--debug-log "Changed files: %S" (mapcar (lambda (f) (cdr (assq 'path f))) changed-files))
        (shipit--debug-log "Latest reviews: %S" latest-reviews)
        (shipit--debug-log "CODEOWNERS will be fetched from base branch: %s" base-ref-name)

        ;; Process requested reviewers for GitHub-style team approval tracking
        (shipit--debug-log "Processing requested reviewers: users=%s teams=%s"
                           (length (or (cdr (assq 'users requested-reviewers-data)) '()))
                           (length (or (cdr (assq 'teams requested-reviewers-data)) '())))

        (let* ((requested-users (cdr (assq 'users requested-reviewers-data)))
               (requested-teams (cdr (assq 'teams requested-reviewers-data)))
               (latest-review-states (shipit--process-latest-reviews latest-reviews))
               (approved-users (shipit--get-approved-users latest-review-states))
               ;; Compute team approval status - which requested teams have been satisfied by approved users
               (team-approval-status (shipit--compute-requested-team-approvals
                                      requested-teams approved-users owner)))

          (shipit--debug-log "Requested teams: %S" (mapcar (lambda (team) (cdr (assq 'slug team))) requested-teams))
          (shipit--debug-log "Approved users: %S" approved-users)
          (shipit--debug-log "Team approval status: %S" team-approval-status)

          ;; Create status display based on requested reviewer analysis
          (let* ((approved-count (length approved-users))
                 (total-requested-teams (length requested-teams))
                 (approved-teams (seq-count (lambda (team-status)
                                             (cdr (assq 'approved (cdr team-status))))
                                           team-approval-status))
                 (pending-teams-count (- total-requested-teams approved-teams))
                 (total-requested-users (length requested-users))
                 (pending-requested-users-count (seq-count (lambda (user)
                                                             (let ((user-login (cdr (assq 'login user))))
                                                               (not (member user-login approved-users))))
                                                           requested-users))
                 ;; Create actual lists (not counts) for magit section rendering
                 (pending-teams-list (seq-filter (lambda (team)
                                                   (let ((team-slug (cdr (assq 'slug team))))
                                                     (not (cdr (assq 'approved (cdr (assoc team-slug team-approval-status)))))))
                                                 requested-teams))
                 (pending-users-list (seq-filter (lambda (user)
                                                   (let ((user-login (cdr (assq 'login user))))
                                                     (not (member user-login approved-users))))
                                                 requested-users))
                 (has-pending-teams (> pending-teams-count 0))
                 (has-pending-users (> pending-requested-users-count 0))
                 ;; Use CODEOWNERS-based decision as primary, with requested reviewer fallback for display
                 (final-review-decision review-decision)
                 ;; Only show requested reviewer pending status if CODEOWNERS didn't provide a decision
                 (show-requested-pending (and (null codeowners-decision)
                                             (string= final-review-decision "APPROVED")
                                             (or has-pending-teams has-pending-users)))
                 (display-review-decision (if show-requested-pending "REVIEW_REQUIRED" final-review-decision))
                 (status-text (cond
                               ((string= display-review-decision "CHANGES_REQUESTED")
                                (format "❌ Changes Requested (%d approved)" approved-count))
                               ((string= display-review-decision "APPROVED")
                                (format "✅ Approved (%d)" approved-count))
                               ;; Only show pending status if not approved (CODEOWNERS requirements not satisfied)
                               ((or has-pending-teams has-pending-users)
                                (cond
                                 ((and has-pending-teams has-pending-users)
                                  (format "%s Review Required (%d approved) %s %d team%s, %d user%s pending"
                                         (shipit--get-status-icon "waiting" "⏳")
                                         approved-count
                                         (shipit--get-status-icon "pending" "🔍")
                                         pending-teams-count (if (= pending-teams-count 1) "" "s")
                                         pending-requested-users-count (if (= pending-requested-users-count 1) "" "s")))
                                 (has-pending-teams
                                  (format "%s Review Required (%d approved) %s %d team%s pending"
                                         (shipit--get-status-icon "waiting" "⏳")
                                         approved-count
                                         (shipit--get-status-icon "pending" "🔍")
                                         pending-teams-count (if (= pending-teams-count 1) "" "s")))
                                 (has-pending-users
                                  (format "%s Review Required (%d approved) %s %d user%s pending"
                                         (shipit--get-status-icon "waiting" "⏳")
                                         approved-count
                                         (shipit--get-status-icon "pending" "🔍")
                                         pending-requested-users-count (if (= pending-requested-users-count 1) "" "s")))))
                               ;; Fallback for other states
                               ((null display-review-decision)
                                (format "📝 No Review Decision (%d)" approved-count))
                               (t
                                (format "❓ Unknown Decision (%s) (%d)" display-review-decision approved-count)))))

            ;; Debug the final status calculation
            (shipit--debug-log "Final status calculation:")
            (shipit--debug-log "  basic-review-decision: %s" basic-review-decision)
            (shipit--debug-log "  codeowners-decision: %s" codeowners-decision)
            (shipit--debug-log "  final-review-decision: %s" final-review-decision)
            (shipit--debug-log "  display-review-decision: %s" display-review-decision)
            (shipit--debug-log "  pending-teams: %s" pending-teams-count)
            (shipit--debug-log "  pending-users: %s" pending-requested-users-count)
            (shipit--debug-log "  status-text: %s" status-text)

            ;; Build result and cache it
            (let ((result (list (cons 'review-decision display-review-decision)
                                (cons 'status-text status-text)
                                (cons 'approved-count approved-count)
                                (cons 'approved-users approved-users)
                                (cons 'requested-teams requested-teams)
                                (cons 'requested-users requested-users)
                                (cons 'team-approval-status team-approval-status)
                                (cons 'pending-teams pending-teams-list)
                                (cons 'pending-users pending-users-list)
                                (cons 'pending-teams-count pending-teams-count)
                                (cons 'pending-users-count pending-requested-users-count)
                                (cons 'latest-reviews latest-review-states)
                                (cons 'completed-reviews latest-reviews))))
              (shipit--debug-log "💾 Caching review decision data for PR #%s" pr-number)
              (setq shipit--cached-review-decision result)
              (shipit--profiling-end profiling-timer)
              result)))))))


(defun shipit--get-pr-review-decision (repo pr-number)
  "Get GitHub's official review decision using GraphQL (simple version).
For backward compatibility - returns just the status text."
  (let ((detailed-info (shipit--get-pr-review-decision-detailed repo pr-number)))
    (cdr (assq 'status-text detailed-info))))

(defun shipit--get-pr-merge-readiness (repo pr-number)
  "Get overall merge readiness status for PR-NUMBER in REPO.
Returns status based on GitHub's mergeable_state and other criteria."
  (let* ((pr-data (shipit-get-pull-request pr-number))
         (mergeable-state (cdr (assq 'mergeable_state pr-data)))
         (mergeable (cdr (assq 'mergeable pr-data)))
         (draft (cdr (assq 'draft pr-data)))
         (state (shipit--get-pr-actual-state pr-data)))

    (cond
     ;; PR is merged
     ((string= state "merged")
      "🎉 Merged")

     ;; PR is closed
     ((string= state "closed")
      "❌ Closed")

     ;; PR is draft
     ((and draft (not (eq draft :json-false)))
      "🚧 Draft")

     ;; Check mergeable_state for detailed status
     ((string= mergeable-state "clean")
      "✅ Ready to Merge")

     ((string= mergeable-state "blocked")
      "🚫 Blocked")

     ((string= mergeable-state "dirty")
      "⚠ Merge Conflict")

     ((string= mergeable-state "unstable")
      "🔄 Checks Running")

     ((string= mergeable-state "behind")
      "⏪ Behind Base")

     ;; Fallback to simple mergeable check
     ((eq mergeable :json-false)
      "⚠ Cannot Merge")

     ((eq mergeable t)
      "✅ Can Merge")

     ;; Unknown state
     (t
      (format "❓ Unknown (%s)" (or mergeable-state "unknown"))))))

(defun shipit--get-pr-review-details (repo pr-number)
  "Get detailed review information for PR-NUMBER in REPO."
  (let* ((reviews-endpoint (format "/repos/%s/pulls/%s/reviews" repo pr-number))
         (requested-reviewers-endpoint (format "/repos/%s/pulls/%s/requested_reviewers" repo pr-number))
         (reviews (progn
                    (shipit--debug-log "Fetching reviews from: %s" reviews-endpoint)
                    (let* ((result (shipit-gh-etag-get-json-with-refresh-cache reviews-endpoint '((per_page . 100)) shipit-github-token)))
                      (plist-get result :json))))
         (requested-reviewers (progn
                                (shipit--debug-log "Fetching requested reviewers from: %s" requested-reviewers-endpoint)
                                (let* ((result (shipit-gh-etag-get-json-with-refresh-cache requested-reviewers-endpoint '((per_page . 100)) shipit-github-token)))
                                  (plist-get result :json)))))
    (let ((latest-reviews (make-hash-table :test 'equal))
          (review-details '()))
      ;; Process existing reviews if any
      (when reviews
        ;; Get the latest review from each reviewer
        (dolist (review reviews)
          (let* ((user (cdr (assq 'login (cdr (assq 'user review)))))
                 (state (cdr (assq 'state review)))
                 (submitted-at (cdr (assq 'submitted_at review)))
                 (body (cdr (assq 'body review))))
            (when (and user state submitted-at)
              ;; Keep only the latest review from each user
              (let ((existing (gethash user latest-reviews)))
                (when (or (null existing)
                          (string< (cdr (assq 'submitted_at existing)) submitted-at))
                  (puthash user review latest-reviews))))))

        ;; Convert hash table to list with status indicators
        (maphash (lambda (user review)
                   (let* ((state (cdr (assq 'state review)))
                          (body (cdr (assq 'body review)))
                          (clean-body (and body (not (string-empty-p (string-trim body))) body))
                          (status-icon (cond
                                        ((string= state "APPROVED") "✅")
                                        ((string= state "CHANGES_REQUESTED") "🔴")
                                        ((string= state "PENDING") "⏳")
                                        (t "❓")))
                          (status-text (cond
                                        ((string= state "APPROVED") "approved")
                                        ((string= state "CHANGES_REQUESTED") "changes requested")
                                        ((string= state "PENDING") "pending")
                                        (t "commented"))))
                     (push `((user . ,user)
                             (state . ,state)
                             (status-icon . ,status-icon)
                             (status-text . ,status-text)
                             (body . ,clean-body))
                           review-details)))
                 latest-reviews))

      ;; Add requested reviewers who haven't reviewed yet
      (when requested-reviewers
        ;; Add individual users
        (let ((users (cdr (assq 'users requested-reviewers))))
          (dolist (user-info users)
            (let ((user (cdr (assq 'login user-info))))
              (unless (gethash user latest-reviews)
                (push `((user . ,user)
                        (state . "REQUESTED")
                        (status-icon . "⏳")
                        (status-text . "review requested")
                        (body . nil))
                      review-details)))))
        ;; Add requested teams
        (let ((teams (cdr (assq 'teams requested-reviewers))))
          (dolist (team-info teams)
            (let ((team-slug (cdr (assq 'slug team-info)))
                  (team-name (cdr (assq 'name team-info))))
              (push `((user . ,(format "@%s" team-slug))
                      (team . t)
                      (team-slug . ,team-slug)
                      (state . "TEAM_REQUESTED")
                      (status-icon . "👥")
                      (status-text . "team review requested")
                      (body . nil))
                    review-details)))))

      ;; Sort by status (approved first, then changes requested, then pending)
      (sort review-details
            (lambda (a b)
              (let ((state-a (cdr (assq 'state a)))
                    (state-b (cdr (assq 'state b))))
                (cond
                 ((and (string= state-a "APPROVED") (not (string= state-b "APPROVED"))) t)
                 ((and (string= state-b "APPROVED") (not (string= state-a "APPROVED"))) nil)
                 ((and (string= state-a "CHANGES_REQUESTED") (not (string= state-b "CHANGES_REQUESTED"))) t)
                 (t (string< (cdr (assq 'user a)) (cdr (assq 'user b)))))))))))

(defun shipit-get-pull-request (pr-number &optional repo)
  "Get detailed information for pull request PR-NUMBER.
Optional REPO parameter specifies which repository to use.
If not provided, uses the currently set repository.
Dispatches to the active PR backend's :fetch-pr."
  (let* ((target-repo (or repo shipit-current-repo))
         (validated-repo (or target-repo
                            (progn
                              (unless (shipit--ensure-repository)
                                (error "No repository set"))
                              shipit-current-repo)))
         (resolved (shipit-pr--resolve-for-repo validated-repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :fetch-pr) config pr-number)))

(defun shipit-get-pr-files (pr-number)
  "Get files changed in pull request PR-NUMBER.
Dispatches to the active PR backend's :fetch-files."
  (unless (shipit--ensure-repository)
    (error "No repository set"))
  (let* ((repo shipit-current-repo)
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :fetch-files) config pr-number)))

(defun shipit--api-request-paginated (endpoint &optional per-page)
  "Make paginated API requests to ENDPOINT, fetching all pages.
PER-PAGE defaults to 100 (GitHub's maximum per page)."
  (let ((per-page (or per-page 100))
        (page 1)
        (all-results '())
        (continue t))
    (condition-case err
        (while continue
          (let* ((paginated-endpoint (format "%s%sper_page=%d&page=%d"
                                             endpoint
                                             (if (string-match-p "\\?" endpoint) "&" "?")
                                             per-page page)))
            (shipit--debug-log "Fetching page %d from: %s" page paginated-endpoint)
            (let ((page-results (shipit--api-request paginated-endpoint)))
              (if (and page-results (> (length page-results) 0))
                  (progn
                    (setq all-results (append all-results page-results))
                    (setq page (1+ page))
                    ;; Continue if we got a full page (might be more)
                    (when (< (length page-results) per-page)
                      (setq continue nil)))
                ;; Empty page means we're done
                (setq continue nil)))))
      (error
       (shipit--debug-log "Error in paginated API request: %S" err)
       (error "Paginated API request failed: %S" err)))
    (shipit--debug-log "Paginated request complete: fetched %d total items across %d pages"
                       (length all-results) (1- page))
    all-results))

(defvar shipit--async-fetch-state (make-hash-table :test 'equal)
  "Hash table to track async fetch operations by PR number.")

(defun shipit--api-request-paginated-async (endpoint callback &optional per-page max-pages)
  "Make paginated API requests to ENDPOINT asynchronously with ETag caching, calling CALLBACK when complete.
PER-PAGE defaults to 100. MAX-PAGES limits total pages (default unlimited).
Uses ETag caching on each page to handle 304 Not Modified responses."
  (let ((per-page (or per-page 100))
        (max-pages (or max-pages 999))
        (page 1)
        (all-results '())
        (token (shipit--github-token)))

    (cl-labels ((fetch-next-page ()
                  (when (<= page max-pages)
                    (let* ((page-params `((per_page . ,per-page) (page . ,page))))
                      (shipit--debug-log "Async fetching page %d from: %s" page endpoint)
                      ;; Use ETag-aware async to properly handle 304 Not Modified responses
                      (if token
                          (shipit-gh-etag-get-json-async
                           endpoint
                           page-params
                           token
                           (lambda (result)
                             (let ((status (when result (plist-get result :status)))
                                   (data (when result (plist-get result :json))))
                               (shipit--debug-log "Paginated async page %d result: status=%s, data type=%s, data length=%s"
                                                page
                                                status
                                                (type-of data)
                                                (if (listp data) (length data) "not-list"))
                               (handle-page-data data))))
                        ;; Fallback if no token
                        (shipit--api-request
                         (format "%s%sper_page=%d&page=%d"
                                endpoint
                                (if (string-match-p "\\?" endpoint) "&" "?")
                                per-page page)
                         nil
                         (lambda (data)
                           (handle-page-data data)))))))

                (handle-page-data (data)
                  (cond
                   ((null data)
                    (shipit--debug-log "No data returned for async paginated request page %d" page)
                    (funcall callback all-results))  ; Return what we have so far
                   ((and data (> (length data) 0))
                    (setq all-results (append all-results data))
                    (setq page (1+ page))
                    ;; Continue if we got a full page and haven't hit limits
                    (if (< (length data) per-page)
                        (progn
                          (shipit--debug-log "Async paginated request complete: fetched %d total items" (length all-results))
                          (funcall callback all-results))  ; Last page, we're done
                      (fetch-next-page)))  ; More pages available
                   (t
                    ;; Empty page means we're done
                    (shipit--debug-log "Async paginated request complete: fetched %d total items" (length all-results))
                    (funcall callback all-results)))))
      (fetch-next-page))))

(defun shipit--fetch-commits-and-files-parallel (repo pr-number pr callback)
  "Fetch commits, files, comments, and checks for PR in parallel, then call CALLBACK with enhanced PR data."
  ;; Defensive programming: ensure repo is a string, not a list
  (let ((repo (if (listp repo) (format "%s" repo) repo)))
    (unless (stringp repo)
      (error "shipit: repo must be a string, got %s: %S" (type-of repo) repo))
    (let* ((fetch-key (format "%s:%s" repo pr-number))
           (commits-endpoint (format "/repos/%s/pulls/%s/commits" repo pr-number))
           (files-endpoint (format "/repos/%s/pulls/%s/files" repo pr-number))
           (comments-endpoint (format "/repos/%s/pulls/%s/comments" repo pr-number))
         (state (make-hash-table :test 'equal)))

    ;; Track this fetch operation
    (puthash fetch-key state shipit--async-fetch-state)
    (puthash 'commits-done nil state)
    (puthash 'files-done nil state)
    (puthash 'comments-done nil state)
    (puthash 'checks-done nil state)
    (puthash 'pr pr state)

    (shipit--debug-log "Starting parallel fetch for PR #%s: commits, files, comments, and checks" pr-number)

    ;; Define completion checker
    (cl-labels ((check-completion ()
                  (when (and (gethash 'commits-done state)
                             (gethash 'files-done state)
                             (gethash 'comments-done state)
                             (gethash 'checks-done state))
                    ;; All fetches complete - enhance PR and callback
                    (let ((enhanced-pr (gethash 'pr state)))
                      (shipit--debug-log "Parallel fetch complete for PR #%s" pr-number)
                      (remhash fetch-key shipit--async-fetch-state)  ; Clean up
                      (funcall callback enhanced-pr)))))

      ;; Fetch commits asynchronously
      (shipit--api-request-paginated-async
       commits-endpoint
       (lambda (commits)
         (shipit--debug-log "Async commits fetch complete: %d commits" (length commits))
         (let ((enhanced-pr (gethash 'pr state)))
           ;; Remove existing commits key and add new data
           (setq enhanced-pr (assq-delete-all 'commits enhanced-pr))
           (setq enhanced-pr (append enhanced-pr `((commits . ,commits))))
           (puthash 'pr enhanced-pr state)
           (puthash 'commits-done t state)
           (check-completion)))
       100 5)  ; Max 5 pages (500 commits)

      ;; Fetch files asynchronously in parallel
      (shipit--api-request-paginated-async
       files-endpoint
       (lambda (files)
         (shipit--debug-log "Async files fetch complete: %d files" (length files))
         (let ((enhanced-pr (gethash 'pr state)))
           ;; Remove existing files key and add new data
           (setq enhanced-pr (assq-delete-all 'files enhanced-pr))
           (setq enhanced-pr (append enhanced-pr `((files . ,files))))
           (puthash 'pr enhanced-pr state)
           (puthash 'files-done t state)
           (check-completion)))
       100 3)  ; Max 3 pages (300 files)

      ;; Fetch comments asynchronously with pagination and ETag caching
      ;; Large PRs can have 100+ inline comments, so pagination is critical
      (shipit--api-request-paginated-async
       comments-endpoint
       (lambda (comments)
         (shipit--debug-log "Async paginated comments fetch complete: %d comments" (length comments))
         ;; Cache comments globally for use by files section and inline comments section
         ;; Enhance REST API comments with outdated property and stale detection
         (let* ((pr-data (gethash 'pr state))
                (head-obj (cdr (assq 'head pr-data)))
                (head-sha (cdr (assq 'sha head-obj)))
                (enhanced-comments (shipit--enhance-rest-comments-with-outdated comments repo head-sha)))
           (setq shipit--cached-inline-comments enhanced-comments)
           ;; Check for new inline comments and update section indicators
           (when (shipit--check-for-new-inline-comments repo pr-number enhanced-comments)
             (shipit--debug-log "INLINE-COMMENTS: New comments detected, adding pr-files to unread sections")
             (unless (memq 'pr-files shipit--sections-with-unread)
               (push 'pr-files shipit--sections-with-unread))
             (shipit--update-section-unread-indicators)))
         (setq shipit--inline-comments-fetched t)
         (shipit--debug-log "PARALLEL-ASYNC-DEBUG: Set inline-comments flag to %s in buffer %s"
                            shipit--inline-comments-fetched (buffer-name))
         (puthash 'comments-done t state)
         (check-completion))
       100 5)))))  ; Max 5 pages (500 comments) - large PRs can have many inline comments

(defun shipit--fetch-commits-and-files-parallel-sync (repo pr-number pr)
  "Fetch commits, files, comments, and checks for PR in parallel synchronously, returning enhanced PR data.
Uses parallel async requests but waits for all to complete before returning."
  ;; Defensive programming: ensure repo is a string, not a list
  (let ((repo (if (listp repo) (format "%s" repo) repo))
        (target-buffer (current-buffer))) ; Capture current buffer for buffer-local variables
    (shipit--debug-log 'reactions "🔍 PARALLEL-SYNC ENTRY: repo=%s pr-number=%s buffer=%s" repo pr-number (buffer-name))
    (unless (and repo (stringp repo))
      (throw 'debug-repo-nil
             (list :repo repo
                   :type (type-of repo)
                   :pr-number pr-number
                   :pr pr
                   :buffer (buffer-name)
                   :backtrace (backtrace-frames))))
    (let ((commits-done nil)
          (files-done nil)
          (comments-done nil)
          (general-comments-done nil)
          (reviews-done nil)
          (resolved-threads-done nil)
          (reactions-done nil)
          (commits-data nil)
          (files-data nil)
          (comments-data nil)
          (general-comments-data nil)
          (reviews-data nil)
          (resolved-threads-data nil)
          (reactions-data nil)
          (commits-endpoint (format "/repos/%s/pulls/%s/commits" repo pr-number))
          (files-endpoint (format "/repos/%s/pulls/%s/files" repo pr-number))
          (comments-endpoint (format "/repos/%s/pulls/%s/comments" repo pr-number))
          (enhanced-pr pr))

      ;; Start parallel async requests for commits/files/inline-comments/checks, with ETag-cached general comments
      (shipit--debug-log "Starting parallel async fetch for PR #%s: commits, files, inline comments, checks (+ ETag-cached general comments)" pr-number)

      ;; Start both requests in parallel
      ;; Use paginated async function for commits to support 304 Not Modified responses and >100 commits
      (shipit--api-request-paginated-async
       commits-endpoint
       (lambda (commits)
         (shipit--debug-log "Parallel sync: commits fetch complete (%d commits)"
                            (if (sequencep commits) (length commits) 0))
         (setq commits-data commits
               commits-done t))
       100 5)  ; Max 5 pages (500 commits)

      ;; Use paginated async function for files to support 304 Not Modified responses and >100 files
      (shipit--api-request-paginated-async
       files-endpoint
       (lambda (files)
         (shipit--debug-log "PARALLEL-SYNC-FILES-FETCH: files fetch complete - type=%s, sequencep=%s, length=%s"
                            (type-of files)
                            (sequencep files)
                            (if (sequencep files) (length files) 0))
         (setq files-data files
               files-done t))
       100 3)  ; Max 3 pages (300 files)

      ;; Fetch inline comments asynchronously with pagination - GitHub's default is ~30 items
      ;; Use same pagination as files/commits to ensure all comments are fetched (100 per page, 5 pages max = 500 comments)
      (shipit--api-request-paginated-async
       comments-endpoint
       (lambda (comments)
         (shipit--debug-log "PARALLEL-SYNC-DEBUG: Comments fetch complete - type: %s, length: %s"
                            (type-of comments)
                            (if (sequencep comments) (length comments) "not-sequence"))
         (setq comments-data comments
               comments-done t)
         ;; Cache comments globally (use empty list instead of nil for 0 comments)
         ;; Set buffer-local variables in the correct buffer (not the HTTP request buffer)
         (with-current-buffer target-buffer
           ;; Enhance REST API comments with outdated property and stale detection
           (let* ((head-obj (cdr (assq 'head pr)))
                  (head-sha (cdr (assq 'sha head-obj)))
                  (enhanced-comments (shipit--enhance-rest-comments-with-outdated (or comments '()) repo head-sha)))
             (setq shipit--cached-inline-comments enhanced-comments)
             ;; Check for new inline comments and update section indicators
             (when (shipit--check-for-new-inline-comments repo pr-number enhanced-comments)
               (shipit--debug-log "INLINE-COMMENTS: New comments detected, adding pr-files to unread sections")
               (unless (memq 'pr-files shipit--sections-with-unread)
                 (push 'pr-files shipit--sections-with-unread))
               (shipit--update-section-unread-indicators)))
           (setq shipit--inline-comments-fetched t)
           ;; Fetch reactions for inline comments via backend dispatch
           (shipit-comment--fetch-reactions-batch (or comments '()) repo t)
           (shipit--debug-log "Reactions batch completed for %d inline comments in buffer %s"
                              (if (sequencep comments) (length comments) 0)
                              (buffer-name))
           ))
       100 5)  ; Max 5 pages (500 comments) - matches commits/files pagination

      ;; Mark checks as done immediately - preserve lazy loading UX
      ;; Checks will be fetched on-demand when user expands the section
      (setq checks-done t)

      ;; Fetch general comments and reviews asynchronously (parallelize with commits/files/inline)
      (shipit--debug-log "PARALLEL-SYNC: Starting async general comments + reviews fetch")
      (shipit--fetch-general-comments-async-github
       repo pr-number
       (lambda (all-general-comments)
         (shipit--debug-log "PARALLEL-ASYNC: General comments callback with %d comments"
                            (length all-general-comments))
         ;; Set the data and completion flags
         (setq general-comments-data all-general-comments
               general-comments-done t
               reviews-data nil
               reviews-done t)
         ;; Cache general comments in the correct buffer
         (when (buffer-live-p target-buffer)
           (with-current-buffer target-buffer
             (shipit--cache-general-comments-with-dedup all-general-comments)))))

      ;; Fetch resolved threads using GraphQL
      (let* ((owner-repo (split-string repo "/"))
             (owner (car owner-repo))
             (repo-name (cadr owner-repo)))
        (shipit--fetch-resolved-threads-graphql
         owner repo-name pr-number
         (lambda (resolved-threads)
           (shipit--debug-log "PARALLEL-SYNC-DEBUG: Resolved threads fetch complete - type: %s, length: %s"
                              (type-of resolved-threads)
                              (if (sequencep resolved-threads) (length resolved-threads) "not-sequence"))
           (setq resolved-threads-data resolved-threads
                 resolved-threads-done t)
           ;; Cache resolved threads in the correct buffer
           (with-current-buffer target-buffer
             (let* ((old-threads shipit--cached-resolved-threads)
                    (new-threads (or resolved-threads '()))
                    ;; Compare thread count as simple change detection
                    (threads-changed (or (not old-threads)
                                        (not (= (length old-threads) (length new-threads))))))
               (setq shipit--cached-resolved-threads new-threads)
               ;; Only invalidate resolved comments hash when threads actually changed
               (when threads-changed
                 (shipit--debug-log "Resolved threads changed (%d -> %d) - clearing resolved comments hash for rebuilding"
                                    (if old-threads (length old-threads) 0) (length new-threads))
                 (setq shipit--resolved-comments-hash nil)))
             (shipit--debug-log "PARALLEL-SYNC-DEBUG: Set resolved threads cache to %d threads in buffer %s"
                                (if (sequencep resolved-threads) (length resolved-threads) 0)
                                (buffer-name))))))

      ;; Skip reactions on initial load for performance - users don't need emojis on first display
      ;; Reactions can be fetched on-demand later if needed
      (shipit--debug-log 'reactions "⏭️  SKIPPING reactions fetch on buffer load for performance (can be fetched on-demand)")
      (setq reactions-data (make-hash-table :test 'equal)
            reactions-done t)

      ;; Wait for all requests to complete (busy wait with timeout)
      (let ((start-time (current-time))
            (timeout-seconds 30))
        (while (and (not (and commits-done files-done comments-done general-comments-done reviews-done resolved-threads-done reactions-done))
                    (< (float-time (time-subtract (current-time) start-time)) timeout-seconds))
          (sleep-for 0.1)  ; Wait 100ms before checking again
          (accept-process-output nil 0.01))  ; Allow async callbacks to run

        ;; OFFENSIVE PROGRAMMING: If any request failed, crash with detailed context
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          (unless (and commits-done files-done comments-done general-comments-done reviews-done resolved-threads-done reactions-done)
            (error "Shipit parallel async failed - elapsed=%.2fs commits=%s files=%s comments=%s general=%s reviews=%s resolved=%s reactions=%s repo=%s pr=%s buffer=%s"
                   elapsed commits-done files-done comments-done general-comments-done reviews-done resolved-threads-done reactions-done
                   repo pr-number (buffer-name))))

        ;; All requests completed successfully
        (shipit--debug-log "Parallel sync complete: %d commits, %d files, %d inline comments, %d general comments, %d reviews, %d resolved threads, %d reactions"
                           (if (sequencep commits-data) (length commits-data) 0)
                           (if (sequencep files-data) (length files-data) 0)
                           (if (sequencep comments-data) (length comments-data) 0)
                           (if (sequencep general-comments-data) (length general-comments-data) 0)
                           (if (sequencep reviews-data) (length reviews-data) 0)
                           (if (sequencep resolved-threads-data) (length resolved-threads-data) 0)
                           (if reactions-data (hash-table-count reactions-data) 0))
        (message "PR data loaded successfully")

        ;; Enhance PR with all datasets (only add valid sequences)
        (shipit--debug-log "PARALLEL-SYNC-ENHANCE: About to add commits - type: %s, sequence: %s, length: %s"
                          (type-of commits-data)
                          (sequencep commits-data)
                          (if (sequencep commits-data) (length commits-data) "n/a"))
        (when (sequencep commits-data)
          (setq enhanced-pr (assq-delete-all 'commits enhanced-pr))
          (setq enhanced-pr (append enhanced-pr `((commits . ,commits-data))))
          (shipit--debug-log "PARALLEL-SYNC-ENHANCE: Added commits to enhanced-pr, checking: %s"
                            (if (assq 'commits enhanced-pr) "present" "MISSING")))
        (shipit--debug-log "PARALLEL-SYNC-FILES: files-data type=%s, sequencep=%s, length=%s"
                          (type-of files-data)
                          (sequencep files-data)
                          (if (sequencep files-data) (length files-data) "n/a"))
        (when (sequencep files-data)
          (shipit--debug-log "PARALLEL-SYNC-FILES: Adding %d files to enhanced-pr" (length files-data))
          (setq enhanced-pr (assq-delete-all 'files enhanced-pr))
          (setq enhanced-pr (append enhanced-pr `((files . ,files-data)))))
        (unless (sequencep files-data)
          (shipit--debug-log "PARALLEL-SYNC-FILES: WARNING - files-data is NOT a sequence, not adding to PR"))

        ;; Always add comments to enhanced PR, use empty list for nil/non-sequence
        (let ((comments-to-add (if (sequencep comments-data) comments-data '())))
          (setq enhanced-pr (assq-delete-all 'comments enhanced-pr))
          (setq enhanced-pr (append enhanced-pr `((comments . ,comments-to-add))))
          (shipit--debug-log "PARALLEL-SYNC-DEBUG: Added %d inline comments to enhanced PR object (original type: %s)"
                             (length comments-to-add) (type-of comments-data)))

        ;; Combine general comments with converted reviews
        (let* ((issue-comments (if (sequencep general-comments-data) general-comments-data '()))
               (raw-reviews (if (sequencep reviews-data) reviews-data '()))
               ;; Check if general-comments-data already contains converted reviews
               (already-has-reviews (and issue-comments
                                         (cl-some (lambda (c) (cdr (assq 'shipit-comment-type c))) issue-comments)))
               (combined-comments
                (if already-has-reviews
                    ;; Already combined - don't add reviews again
                    (progn
                      (shipit--debug-log "SYNC-FIX: General comments already contain converted reviews, skipping duplicate conversion")
                      issue-comments)
                  ;; Not yet combined - add converted reviews
                  (let ((converted-reviews (shipit--convert-reviews-to-comments raw-reviews)))
                    (shipit--debug-log "SYNC-FIX: Converting %d reviews and combining with %d issue comments"
                                       (length raw-reviews) (length issue-comments))
                    (append issue-comments converted-reviews))))
               ;; ROOT CAUSE FIX: Deduplicate comments by ID to prevent duplicate reviews
               (combined-general-comments (cl-remove-duplicates combined-comments
                                                              :key (lambda (c) (cdr (assq 'id c)))
                                                              :test 'equal)))
          ;; Log deduplication results
          (when (< (length combined-general-comments) (length combined-comments))
            (shipit--debug-log "PARALLEL-SYNC-ROOT-CAUSE-FIX: ✅ Removed %d duplicate comments by ID (reviews appearing multiple times)"
                               (- (length combined-comments) (length combined-general-comments))))
          (setq enhanced-pr (assq-delete-all 'general-comments enhanced-pr))
          (setq enhanced-pr (append enhanced-pr `((general-comments . ,combined-general-comments))))
          ;; Update cache with combined comments
          (setq shipit--cached-general-comments combined-general-comments)
          (shipit--debug-log "PARALLEL-SYNC-DEBUG: Final combined general comments: %d total (after dedup and sync fix)"
                             (length combined-general-comments))
          ;; Reactions will be fetched in parallel by the main parallel system
          )

        ;; Checks data not added - preserving lazy loading UX
        ;; Checks will be loaded on-demand when user expands the section

        enhanced-pr))))

(defun shipit-get-pr-comments (pr-number)
  "Get review comments for pull request PR-NUMBER.
Dispatches to the active comment backend's :fetch-inline-comments."
  (unless (shipit--ensure-repository)
    (error "No repository set"))
  (let* ((repo shipit-current-repo)
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :fetch-inline-comments) config pr-number)))

(defun shipit-post-review (pr-number event &optional body comments repo)
  "Post a review for pull request PR-NUMBER.
EVENT can be \\='APPROVE\\=', \\='REQUEST_CHANGES\\=', or \\='COMMENT\\='.
BODY is the overall review comment.
COMMENTS is a list of line-specific comments.
REPO is the repository (owner/name) to post to.  If nil, uses
`shipit-buffer-repo' or `shipit-current-repo'.
Dispatches to the active PR backend's :submit-review."
  ;; Determine the target repo FIRST, before ensure-repository can overwrite it
  (let ((target-repo (or repo
                         (bound-and-true-p shipit-buffer-repo)
                         shipit-current-repo)))
    ;; Only call ensure-repository if we don't already have a target repo
    (unless target-repo
      (unless (shipit--ensure-repository)
        (error "No repository set"))
      (setq target-repo shipit-current-repo))
    (unless target-repo
      (error "No repository context available"))
    ;; Check if user is trying to review their own PR
    (let* ((pr-data (shipit-get-pull-request pr-number target-repo))
           (pr-author (when pr-data (cdr (assq 'login (cdr (assq 'user pr-data))))))
           (current-user (shipit--get-current-user)))
      (when (and pr-author current-user (string= pr-author current-user))
        (message "Cannot review your own PR - authors cannot approve/request changes on their own PRs")
        (error "Cannot review your own PR"))
      ;; Dispatch to backend
      (let* ((resolved (shipit-pr--resolve-for-repo target-repo))
             (backend (car resolved))
             (config (cdr resolved)))
        (funcall (plist-get backend :submit-review) config pr-number event body comments))
      ;; Clear caches after successful submission
      (let ((cache-key (format "%s:%s" target-repo pr-number)))
        (remhash cache-key shipit--cached-approval-status))
      (let ((reviews-endpoint (format "/repos/%s/pulls/%s/reviews" target-repo pr-number)))
        (shipit-clear-etag-cache-for-endpoint reviews-endpoint))
      (when (boundp 'shipit--cached-general-comments)
        (setq shipit--cached-general-comments nil))
      ;; Refresh the shipit buffer
      (let ((buf-name (format "*shipit: %s#%s*" target-repo pr-number)))
        (when (get-buffer buf-name)
          (with-current-buffer buf-name
            (when (boundp 'shipit--cached-review-decision)
              (setq shipit--cached-review-decision nil))
            (setq shipit--user-mutated-pr t)
            (shipit-buffer-refresh))))
      (message "Review posted successfully"))))
(defun shipit-dismiss-review (pr-number &optional message repo)
  "Dismiss your own review for pull request PR-NUMBER with optional MESSAGE.
REPO is the repository (owner/name).  If nil, uses `shipit-buffer-repo'
or `shipit-current-repo'.
Dispatches to the active PR backend's :dismiss-review."
  ;; Determine the target repo FIRST, before ensure-repository can overwrite it
  (let ((target-repo (or repo
                         (bound-and-true-p shipit-buffer-repo)
                         shipit-current-repo)))
    (unless target-repo
      (unless (shipit--ensure-repository)
        (error "No repository set"))
      (setq target-repo shipit-current-repo))
    (unless target-repo
      (error "No repository context available"))
    ;; Dispatch to backend
    (let* ((resolved (shipit-pr--resolve-for-repo target-repo))
           (backend (car resolved))
           (config (cdr resolved))
           (dismiss-fn (plist-get backend :dismiss-review)))
      (if (not dismiss-fn)
          (message "Backend does not support review dismissal")
        (let ((result (funcall dismiss-fn config pr-number message)))
          (if result
              (progn
                ;; Clear caches after successful dismissal
                (let ((cache-key (format "%s:%s" target-repo pr-number)))
                  (remhash cache-key shipit--cached-approval-status))
                (let ((reviews-endpoint (format "/repos/%s/pulls/%s/reviews" target-repo pr-number)))
                  (shipit-clear-etag-cache-for-endpoint reviews-endpoint))
                (shipit-gh-etag-invalidate-endpoint (format "/repos/%s/pulls/%s" target-repo pr-number))
                (when (boundp 'shipit--cached-general-comments)
                  (setq shipit--cached-general-comments nil))
                ;; Refresh the shipit buffer
                (let ((buf-name (format "*shipit: %s#%s*" target-repo pr-number)))
                  (when (get-buffer buf-name)
                    (with-current-buffer buf-name
                      (when (boundp 'shipit--cached-review-decision)
                        (setq shipit--cached-review-decision nil))
                      (setq shipit--user-mutated-pr t)
                      (shipit-buffer-refresh))))
                (message "Review dismissed successfully"))
            (message "No review found to dismiss")))))))

(defun shipit--refresh-approval-section-targeted (pr-number repo)
  "Refresh only the approval section with fresh data from GitHub.
PR-NUMBER and REPO provide context for the API call.
This is much faster than a full buffer refresh."
  (shipit--debug-log "APPROVAL-REFRESH: Starting targeted refresh for PR #%s in %s" pr-number repo)
  (let ((start-time (float-time)))
    ;; Clear cache first to ensure fresh data
    (when (boundp 'shipit--cached-review-decision)
      (setq shipit--cached-review-decision nil))
    ;; Clear the per-PR cache too
    (when (boundp 'shipit--cached-approval-status)
      (let ((cache-key (format "%s:%s" repo pr-number)))
        (when (gethash cache-key shipit--cached-approval-status)
          (remhash cache-key shipit--cached-approval-status))))
    ;; Fetch fresh review decision data
    (let ((review-info (shipit--get-pr-review-decision-detailed repo pr-number)))
      (shipit--debug-log "APPROVAL-REFRESH: Got review info in %.3fs" (- (float-time) start-time))
      (when review-info
        (let* ((review-decision-status (cdr (assq 'status-text review-info)))
               (pending-teams (cdr (assq 'pending-teams review-info)))
               (pending-users (cdr (assq 'pending-users review-info)))
               (completed-reviews (cdr (assq 'completed-reviews review-info)))
               (approved-reviews (seq-filter
                                  (lambda (review)
                                    (string= (cdr (assq 'state review)) "APPROVED"))
                                  completed-reviews))
               (section (shipit--find-section-by-type 'approval)))
          (if (not section)
              (shipit--debug-log "APPROVAL-REFRESH: No approval section found, skipping")
            (let* ((content-marker (oref section content))
                   (end-marker (oref section end))
                   (content-pos (and content-marker (marker-position content-marker)))
                   (end-pos (and end-marker (marker-position end-marker)))
                   (inhibit-read-only t))
              (save-excursion
                ;; Step 1: Update heading text (replace status after "Approval:  ")
                (goto-char (oref section start))
                (when (re-search-forward "Approval:  \\(.*\\)" (line-end-position) t)
                  (replace-match (or review-decision-status "Unknown") nil nil nil 1))

                ;; Step 2: Clear old children list
                (oset section children nil)

                ;; Step 3: Clear washer function
                (when (slot-boundp section 'washer)
                  (oset section washer nil))

                ;; Step 4: Remove invisible overlays
                (when (and content-pos end-pos)
                  (remove-overlays content-pos end-pos 'invisible t))

                ;; Step 5: Delete body content and insert new
                (when (and content-pos end-pos)
                  (when (> end-pos content-pos)
                    (delete-region content-pos end-pos))
                  (goto-char content-pos)
                  ;; Bind magit-insert-section--parent for child sections
                  (let ((magit-insert-section--parent section))
                    (shipit--insert-approval-body
                     approved-reviews pending-teams pending-users
                     review-decision-status review-info pr-number repo))
                  ;; Add trailing newline if we inserted content
                  (when (or approved-reviews pending-teams pending-users
                            (string-match-p "Changes Requested" (or review-decision-status "")))
                    (insert "\n"))
                  ;; Update markers
                  (oset section end (point-marker))
                  (oset section content (copy-marker content-pos))))
              ;; Force display update
              (when (fboundp 'magit-section-update-highlight)
                (magit-section-update-highlight))))
          (shipit--debug-log "APPROVAL-REFRESH: Completed in %.3fs" (- (float-time) start-time))
          t)))))

(defun shipit--insert-approval-body (approved-reviews pending-teams pending-users
                                                       review-decision-status review-info
                                                       pr-number repo)
  "Insert approval section body content.
APPROVED-REVIEWS is list of approved review objects.
PENDING-TEAMS and PENDING-USERS are lists of pending reviewers.
REVIEW-DECISION-STATUS is the status text.
REVIEW-INFO contains the full review decision data.
PR-NUMBER and REPO provide context for text properties."
  ;; Approved by section with avatars and comments
  (when (and approved-reviews (> (length approved-reviews) 0))
    (insert (format "   %s Approved by:\n" (shipit--get-approval-status-icon "APPROVED" "✅")))
    (dolist (review approved-reviews)
      (let* ((user-obj (or (cdr (assq 'author review)) (cdr (assq 'user review))))
             (user (if (stringp user-obj) user-obj (cdr (assq 'login user-obj))))
             (avatar-url (when (and user-obj (not (stringp user-obj)))
                           (cdr (assq 'avatar_url user-obj))))
             (body (cdr (assq 'body review)))
             (comment-text (if (and body (not (string-empty-p body)))
                               (format " - \"%s\"" (truncate-string-to-width body 50 nil nil "..."))
                             "")))
        (insert (format "      %s%s%s\n"
                        (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                 (fboundp 'shipit--create-avatar-display))
                            (concat (shipit--create-avatar-display user avatar-url 16) " ")
                          (concat (shipit--get-user-type-icon "single" "🧑") " "))
                        user comment-text)))))

  ;; Requested teams section
  (when (and pending-teams (> (length pending-teams) 0))
    (insert "   👥 Requested teams:\n")
    (dolist (team-info pending-teams)
      (let ((team-slug (cdr (assq 'slug team-info))))
        (insert (format "      @%s\n" team-slug)))))

  ;; Requested individual reviewers with avatars
  (when (and pending-users (> (length pending-users) 0))
    (insert (format "   %s Requested reviewers:\n" (shipit--get-status-icon "waiting" "⏳")))
    (dolist (user-info pending-users)
      (let* ((user (cdr (assq 'login user-info)))
             (avatar-url (cdr (assq 'avatar_url user-info))))
        (insert (format "      %s%s\n"
                        (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                 (fboundp 'shipit--create-avatar-display))
                            (concat (shipit--create-avatar-display user avatar-url 16) " ")
                          (concat (shipit--get-user-type-icon "single" "🧑") " "))
                        user)))))

  ;; Show who requested changes with details
  (when (string-match-p "Changes Requested" (or review-decision-status ""))
    (if (and review-info (cdr (assq 'latest-reviews review-info)))
        (let ((latest-reviews (cdr (assq 'latest-reviews review-info))))
          (insert (format "   %s Changes requested by:\n" (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌")))
          (when (and latest-reviews (hash-table-p latest-reviews))
            (maphash (lambda (user review)
                       (when (string= (cdr (assq 'state review)) "CHANGES_REQUESTED")
                         (let ((body (cdr (assq 'body review))))
                           (insert (format "      %s%s"
                                           (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                                    (fboundp 'shipit--create-avatar-display))
                                               (concat (shipit--create-avatar-display user nil 16) " ")
                                             (concat (shipit--get-user-type-icon "single" "🧑") " "))
                                           user))
                           (when (and body (not (string-empty-p body)))
                             (insert (format " - %s"
                                             (truncate-string-to-width body 60 nil nil "..."))))
                           (insert "\n"))))
                     latest-reviews)))
      (insert (concat "   " (shipit--get-approval-status-icon "CHANGES_REQUESTED" "❌") " Changes requested (details not available)\n")))))

(defun shipit--get-current-user-review-state (pr-number)
  "Get the current user's review state for PR-NUMBER.
Returns a plist with :state and :body, where state is 'approved, 'changes_requested, 'commented, or nil."
  (let* ((reviews-endpoint (format "/repos/%s/pulls/%s/reviews" shipit-current-repo pr-number))
         (reviews-data (let* ((result (shipit-gh-etag-get-json-with-refresh-cache reviews-endpoint nil shipit-github-token)))
                         (plist-get result :json)))
         (current-user (shipit--get-current-user)))
    (when (and current-user reviews-data)
      ;; Find the most recent review by the current user
      (let ((user-reviews (cl-remove-if-not
                           (lambda (review)
                             (string= (cdr (assq 'login (cdr (assq 'user review))))
                                      current-user))
                           reviews-data)))
        (when user-reviews
          ;; Sort by id (most recent last) and get the latest
          (let* ((latest-review (car (last (sort user-reviews
                                                 (lambda (a b)
                                                   (< (cdr (assq 'id a))
                                                      (cdr (assq 'id b))))))))
                 (state (cdr (assq 'state latest-review)))
                 (body (cdr (assq 'body latest-review))))
            (list :state (cond
                          ((string= state "APPROVED") 'approved)
                          ((string= state "CHANGES_REQUESTED") 'changes_requested)
                          ((string= state "COMMENTED") 'commented)
                          (t nil))
                  :body body)))))))

(defun shipit-get-pr-for-branch (branch &optional repo)
  "Get pull request for BRANCH in REPO (or current repo).
Finds PRs where BRANCH is the head (source) branch."
  (when (and branch (shipit--github-token))
    (shipit--ensure-cache-initialized)  ; Ensure cache is properly initialized
    (let* ((repo (or repo (shipit--get-repo-from-remote)))
           (cache-key (format "%s:%s" repo branch)))
      (when repo
        ;; Check cache first - try both branch-based and PR-specific keys
        (shipit--debug-log "Checking cache for branch-key: %s" cache-key)
        (or (let ((cached-pr (gethash cache-key shipit--cached-branch-prs)))
              (when cached-pr
                (shipit--debug-log "Found cached PR #%s for branch %s (commits type: %s)"
                                   (cdr (assq 'number cached-pr))
                                   branch
                                   (type-of (cdr (assq 'commits cached-pr)))))
              cached-pr)
            ;; DON'T return displayed PR if it doesn't belong to the requested branch
            ;; This was causing cache corruption by returning wrong PRs for branch lookups
            ;; If not in cache, fetch from API
            (let ((endpoint (format "/repos/%s/pulls" repo))
                  (owner (car (split-string repo "/"))))
              (shipit--debug-log "Looking for PR for branch %s in repo %s" branch repo)
              (let* ((state (if shipit-magit-show-closed-prs "all" "open"))
                     ;; Try multiple head formats for robustness
                     (head-formats (list
                                    (format "%s:%s" owner branch)     ; owner:branch
                                    (format "%s" branch)))            ; just branch name
                     (pr nil))
                (condition-case api-error
                    (let ((pr nil))
                      ;; Try each head format until we find a matching PR
                      (catch 'found-pr
                        (dolist (head-format head-formats)
                          (shipit--debug-log "Trying head format: %s" head-format)
                          (condition-case etag-error
                              (let* ((result (shipit-gh-etag-get-json-with-refresh-cache endpoint `((state . ,state) (head . ,head-format)) shipit-github-token))
                                     (prs (plist-get result :json)))
                                (when (and prs (> (length prs) 0))
                                  (let* ((found-pr (car prs))
                                         (pr-number (cdr (assq 'number found-pr)))
                                         (pr-head (cdr (assq 'ref (cdr (assq 'head found-pr)))))
                                         (pr-base (cdr (assq 'ref (cdr (assq 'base found-pr))))))
                                    (shipit--debug-log "Found PR #%s: %s -> %s (searched for head: %s)" pr-number pr-head pr-base head-format)
                                    ;; Verify this PR actually has our branch as the head
                                    (if (string= pr-head branch)
                                        (progn
                                          (setq pr found-pr)
                                          (throw 'found-pr pr))
                                      (shipit--debug-log "PR #%s head branch '%s' doesn't match current branch '%s', continuing search..." pr-number pr-head branch)))))
                            (error
                             (shipit--debug-log "ETag error with head format %s: %s, trying next..." head-format (error-message-string etag-error))))))
                      ;; If we found a PR, enhance it with commits and files data using parallel sync approach
                      (when pr
                        (let ((pr-number (cdr (assq 'number pr))))
                          (shipit--debug-log "Fetching commits and files in parallel for PR #%s" pr-number)
                          ;; Use new parallel sync function
                          (setq pr (shipit--fetch-commits-and-files-parallel-sync repo pr-number pr))))

                      ;; Cache the result (even if nil) to avoid repeated API calls
                      (shipit--debug-log "CACHE-DEBUG: About to cache PR #%s for branch %s with key %s"
                                         (if pr (cdr (assq 'number pr)) "nil") branch cache-key)
                      (puthash cache-key pr shipit--cached-branch-prs)

                      ;; Log the result
                      (if pr
                          (progn
                            (shipit--debug-log "Successfully found matching PR for branch %s with commits and files" branch)
                            (message "Found PR for branch %s" branch))
                        (shipit--debug-log "No PR found with %s as source branch" branch))
                      pr)
                  (error
                   (message "API error fetching PR for branch %s: %s" branch (error-message-string api-error))
                   ;; Don't cache nil results immediately - allow retries
                   nil)))))))))

(defun shipit-get-pr-for-branch-async (branch callback &optional repo)
  "Get pull request for BRANCH asynchronously, calling CALLBACK with result.
CALLBACK is called with (pr-data) when successful, or (nil) if not found."
  (shipit--debug-log "shipit-get-pr-for-branch-async called with branch=%s" branch)
  (shipit--debug-log "Callback type: %s, functionp: %s" (type-of callback) (functionp callback))
  (when (and branch (shipit--github-token) callback)
    (shipit--ensure-cache-initialized)
    (let* ((repo (or repo (shipit--get-repo-from-remote)))
           (cache-key (format "%s:%s" repo branch))
           (original-callback callback))  ; Store callback to avoid shadowing
      (when repo
        ;; Check cache first - return immediately if found
        (let ((cached-pr (gethash cache-key shipit--cached-branch-prs)))
          (if cached-pr
              (progn
                (shipit--debug-log "Found cached PR #%s for branch %s (async)"
                                   (cdr (assq 'number cached-pr)) branch)
                (funcall original-callback cached-pr))
            ;; Not in cache, fetch asynchronously
            (let ((endpoint (format "/repos/%s/pulls" repo))
                  (owner (car (split-string repo "/"))))
              (shipit--debug-log "Async: Looking for PR for branch %s in repo %s" branch repo)
              (let* ((state (if shipit-magit-show-closed-prs "all" "open"))
                     (head-format (format "%s:%s" owner branch)))
                ;; Make async API call
                (shipit--api-request
                 endpoint
                 `(("state" ,state) ("head" ,head-format))
                 (lambda (prs)
                   (if (and prs (> (length prs) 0))
                       (let* ((found-pr (car prs))
                              (pr-number (cdr (assq 'number found-pr)))
                              (pr-head (cdr (assq 'ref (cdr (assq 'head found-pr))))))
                         (shipit--debug-log "Async: Found PR #%s for branch %s" pr-number branch)
                         ;; Verify this PR actually has our branch as the head
                         (if (string= pr-head branch)
                             (progn
                               ;; Cache the result
                               (puthash cache-key found-pr shipit--cached-branch-prs)
                               (shipit--debug-log "Async: Cached PR #%s for branch %s" pr-number branch)
                               (shipit--debug-log "Async: About to call callback with PR data")
                               (shipit--debug-log "Async: Calling callback with funcall, callback=%s" original-callback)
                               (condition-case err
                                   (progn
                                     (funcall original-callback found-pr)
                                     (shipit--debug-log "Async: Callback funcall succeeded"))
                                 (error
                                  (shipit--debug-log "Async: Callback failed with error: %s" err)))
                               (shipit--debug-log "Async: Callback execution completed"))
                           (shipit--debug-log "Async: PR #%s head '%s' doesn't match branch '%s'" pr-number pr-head branch)
                           (funcall original-callback nil)))
                     (shipit--debug-log "Async: No PR found for branch %s" branch)
                     (funcall original-callback nil))))))))))))

(defun shipit--fetch-resolved-threads-graphql (owner repo pr-number &optional callback)
  "Fetch resolved review threads using GraphQL API."
  (shipit--debug-log "GRAPHQL: Fetching resolved threads for PR %s in %s/%s" pr-number owner repo)
  (let* ((query (format "query {
  repository(owner: \"%s\", name: \"%s\") {
    pullRequest(number: %s) {
      reviewThreads(first: 100) {
        nodes {
          id
          isResolved
          resolvedBy { login }
          comments(first: 1) {
            nodes { id databaseId path line originalLine author { login } }
          }
        }
      }
    }
  }
}" owner repo pr-number)))
    (shipit--graphql-request query
                             nil ; no variables
                             (when callback
                               (lambda (response)
                                 (shipit--debug-log "GRAPHQL: Received response for resolved threads")
                                 (shipit--debug-log "GRAPHQL: Full response: %s" response)
                                 (let* ((data (cdr (assq 'data response)))
                                        (repository (cdr (assq 'repository data)))
                                        (pull-request (cdr (assq 'pullRequest repository)))
                                        (review-threads (cdr (assq 'reviewThreads pull-request)))
                                        (threads (cdr (assq 'nodes review-threads))))
                                   (shipit--debug-log "GRAPHQL: Parsed %d review threads" (length threads))
                                   ;; Debug: Show sample thread data
                                   (when (> (length threads) 0)
                                     (shipit--debug-log "GRAPHQL: Sample thread 1: %s" (nth 0 threads))
                                     (when (> (length threads) 1)
                                       (shipit--debug-log "GRAPHQL: Sample thread 2: %s" (nth 1 threads))))
                                   (funcall callback threads)))))))

(defun shipit--extract-commented-line-from-diff-hunk (diff-hunk &optional target-line)
  "Extract the commented line from DIFF-HUNK.
When TARGET-LINE (the original_line number) is provided, use the hunk header
to find the exact line.  Otherwise fall back to the last added line.
Returns the line content with the +/- prefix stripped, or nil if invalid."
  (when (and diff-hunk (stringp diff-hunk) (> (length diff-hunk) 0))
    (let* ((lines (split-string diff-hunk "\n" t))
           (result nil))
      ;; Try to find the exact line using hunk header and target-line
      (when (and target-line (car lines)
                 (string-match "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" (car lines)))
        (let* ((new-start (string-to-number (match-string 2 (car lines))))
               (current-line new-start))
          (dolist (hunk-line (cdr lines))
            (when (and (> (length hunk-line) 0)
                       (not result))
              (let ((prefix (aref hunk-line 0)))
                (cond
                 ((eq prefix ?-)
                  ;; Delete line — doesn't count in new file
                  nil)
                 ((or (eq prefix ?+) (eq prefix ?\s))
                  ;; Add or context line — counts in new file
                  (when (= current-line target-line)
                    (setq result (substring hunk-line 1)))
                  (setq current-line (1+ current-line)))))))))
      ;; Fall back: find last added (+) line in the hunk
      (unless result
        (dolist (hunk-line (reverse lines))
          (when (and (not result)
                     (> (length hunk-line) 0)
                     (eq (aref hunk-line 0) ?+))
            (setq result (substring hunk-line 1)))))
      result)))

(defvar shipit--file-content-cache (make-hash-table :test 'equal)
  "Cache for file content fetched via backend.
Keys are \"repo:ref:path\", values are file content strings or :not-found.")

(defun shipit--get-file-line-content (repo ref path line-number)
  "Get the content of LINE-NUMBER from file PATH at REF in REPO.
Dispatches to the active PR backend's optional :get-file-line-content method.
Caches full file content (including negative results) so each unique
repo:ref:path is fetched at most once per session.
Returns the line content or nil if unavailable."
  (when (and repo ref path line-number (> line-number 0))
    (let* ((backend (shipit-pr--get-backend))
           (fetch-fn (plist-get backend :get-file-line-content)))
      (when fetch-fn
        (let* ((cache-key (format "%s:%s:%s" repo ref path))
               (cached (gethash cache-key shipit--file-content-cache)))
          (unless cached
            (condition-case err
                (let ((content (funcall fetch-fn repo ref path)))
                  (if content
                      (progn
                        (setq cached content)
                        (puthash cache-key content shipit--file-content-cache)
                        (shipit--debug-log "STALE-CHECK: Cached file content for %s (length=%d)"
                                           cache-key (length content)))
                    (setq cached :not-found)
                    (puthash cache-key :not-found shipit--file-content-cache)
                    (shipit--debug-log "STALE-CHECK: Cached :not-found for %s" cache-key)))
              (error
               (setq cached :not-found)
               (puthash cache-key :not-found shipit--file-content-cache)
               (shipit--debug-log "STALE-CHECK: Error fetching file %s: %s (cached :not-found)"
                                  path (error-message-string err)))))
          (when (and cached (not (eq cached :not-found)))
            (let ((lines (split-string cached "\n")))
              (when (<= line-number (length lines))
                (nth (1- line-number) lines)))))))))

(defun shipit--clear-file-content-cache ()
  "Clear the file content cache."
  (clrhash shipit--file-content-cache))

(defun shipit--normalize-for-comparison (str)
  "Normalize STR for comparison by trimming whitespace."
  (when str
    (string-trim str)))

(defun shipit--enhance-rest-comments-with-outdated (comments &optional repo head-sha)
  "Enhance REST API comments with 'outdated property based on line presence.
REST API comments that have 'original_line but no current 'line are outdated.
If REPO and HEAD-SHA are provided, also detect stale comments where the
diff_hunk content no longer matches the current file content at the line."
  (shipit--debug-log "ENHANCE-OUTDATED: Called with %d comments, repo=%s, head-sha=%s"
                     (length comments) repo (if head-sha (substring head-sha 0 (min 7 (length head-sha))) "nil"))
  (let ((outdated-count 0)
        (stale-count 0))
    ;; Don't clear file content cache here - cache keys include the commit SHA
    ;; (repo:ref:path), so entries are immutable and always valid.  Clearing
    ;; forces a re-fetch that may fail due to url.el's broken 304 cache
    ;; extraction, causing stale detection to silently lose data.
    (when (and repo head-sha)
      (shipit--debug-log "ENHANCE-OUTDATED: Stale detection ENABLED (repo and head-sha provided)"))
    (prog1
        (mapcar (lambda (comment)
                  (let* ((line (cdr (assq 'line comment)))
                         (original-line (cdr (assq 'original_line comment)))
                         (comment-id (cdr (assq 'id comment)))
                         (path (cdr (assq 'path comment)))
                         (diff-hunk (cdr (assq 'diff_hunk comment)))
                         (side (cdr (assq 'side comment)))
                         ;; Traditional outdated check: has original_line but no line
                         (is-outdated (and original-line (not line)))
                         ;; Stale check: line exists but content doesn't match diff_hunk
                         (is-stale nil))
                    ;; Perform stale detection for RIGHT side comments with valid line
                    (when (and repo head-sha
                               line
                               (not is-outdated)
                               (equal side "RIGHT")
                               diff-hunk
                               path)
                      (let* ((expected-content (shipit--extract-commented-line-from-diff-hunk diff-hunk original-line))
                             (actual-content (shipit--get-file-line-content repo head-sha path line)))
                        (when (and expected-content actual-content)
                          (let ((expected-normalized (shipit--normalize-for-comparison expected-content))
                                (actual-normalized (shipit--normalize-for-comparison actual-content)))
                            (unless (string= expected-normalized actual-normalized)
                              (setq is-stale t)
                              (setq stale-count (1+ stale-count))
                              (shipit--debug-log "STALE-CHECK: Comment %s is stale - expected '%s' but found '%s' at line %d"
                                                 comment-id
                                                 (truncate-string-to-width expected-normalized 50)
                                                 (truncate-string-to-width actual-normalized 50)
                                                 line))))))
                    (when is-outdated
                      (setq outdated-count (1+ outdated-count))
                      (shipit--debug-log "OUTDATED-FIX: Enhanced comment %s with outdated=t (original_line=%s, line=%s)"
                                         comment-id original-line line))
                    ;; Apply both outdated and stale markers
                    (let ((enhanced comment))
                      (when is-outdated
                        (setq enhanced (append enhanced '((outdated . t)))))
                      (when is-stale
                        (setq enhanced (append enhanced '((outdated . t)))))
                      enhanced)))
                comments)
      (shipit--debug-log "OUTDATED-FIX: Enhanced %d/%d comments with outdated property, %d stale"
                         outdated-count (length comments) stale-count))))

(defun shipit--fetch-inline-comments (repo pr-number &optional _force-fresh head-sha)
  "Fetch inline comments for PR and refresh buffer.
Dispatches the API call to the active comment backend's :fetch-inline-comments.
Orchestrator logic (enhancement, caching, reactions) stays here.
Optional HEAD-SHA avoids a redundant PR fetch when the caller already has it."
  ;; Offensive programming: validate required parameters
  (unless (and repo (stringp repo))
    (error "shipit: repo must be a non-empty string, got %s: %S" (type-of repo) repo))
  (unless (and pr-number (numberp pr-number))
    (error "shipit: pr-number must be a number, got %s: %S" (type-of pr-number) pr-number))
  (let* ((captured-repo repo)
         ;; Get PR head SHA for stale comment detection (use provided or fetch)
         (head-sha (or head-sha
                       (cdr (assq 'sha (cdr (assq 'head
                                                   (shipit-get-pull-request pr-number)))))))
         ;; Dispatch to comment backend for the API call
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (data (funcall (plist-get backend :fetch-inline-comments) config pr-number)))
    (shipit--debug-log "📥 INLINE-FETCH: Backend returned %d comments" (if data (length data) 0))
    (when data
      ;; Offensive programming: validate inline comments data
      (unless (listp data)
        (error "shipit: Expected list but got %s for inline comments data: %S" (type-of data) data))
      ;; Clear resolved comments hash for fresh data
      (shipit--debug-log "Fresh inline comments data received, clearing resolved comments hash for rebuilding")
      (setq shipit--resolved-comments-hash nil)
      ;; Enhance REST API comments with outdated property and stale detection
      (let ((enhanced-comments (shipit--enhance-rest-comments-with-outdated data repo head-sha)))
        (shipit--debug-log "📥 INLINE-FETCH: After enhance, %d comments" (length enhanced-comments))
        (setq shipit--cached-inline-comments enhanced-comments)
        ;; Check for new inline comments and update section indicators
        (when (shipit--check-for-new-inline-comments repo pr-number enhanced-comments)
          (shipit--debug-log "INLINE-COMMENTS: New comments detected in fetch, adding pr-files to unread sections")
          (unless (memq 'pr-files shipit--sections-with-unread)
            (push 'pr-files shipit--sections-with-unread))
          (shipit--update-section-unread-indicators)))
      (setq shipit--inline-comments-fetched t)
      ;; Fetch reactions for all inline comments via backend dispatch
      (shipit--debug-log "About to fetch reactions for %d inline comments" (length data))
      (shipit-comment--fetch-reactions-batch data captured-repo t))))

(defun shipit--get-file-inline-comments (pr-number repo file-path)
  "Fetch inline comments for a specific FILE-PATH in PR-NUMBER with ETag caching.
This is used for targeted section refresh - fetches all comments then filters to file.
Returns list of comments for the specified file only.
Uses ETag caching for efficiency:
- First call: Full fetch from API
- Subsequent calls: 304 Not Modified if no changes upstream (instant)
- Changed upstream: Fresh data automatically fetched"
  (shipit--debug-log "TARGET-REFRESH: Fetching inline comments for file %s in PR #%s" file-path pr-number)

  (let* ((endpoint (format "/repos/%s/pulls/%s/comments" repo pr-number))
         (start-time (float-time))
         ;; Use ETag-cached API - same as shipit--fetch-inline-comments but without side effects
         (result (shipit-gh-etag-get-json-with-refresh-cache endpoint '((per_page . 100)) shipit-github-token))
         (all-comments (when result (plist-get result :json)))
         (from-cache (when result (plist-get result :from-cache)))
         (fetch-time (- (float-time) start-time)))

    (shipit--debug-log "TARGET-REFRESH: Fetched %d total comments in %.3fs (from-cache=%s)"
                       (if all-comments (length all-comments) 0)
                       fetch-time
                       from-cache)

    (when all-comments
      ;; Get head SHA for stale detection
      (let* ((pr-data (shipit-get-pull-request pr-number))
             (head-obj (cdr (assq 'head pr-data)))
             (head-sha (cdr (assq 'sha head-obj)))
             ;; Filter to only comments for this file
             (file-comments (seq-filter (lambda (comment)
                                          (string= (cdr (assq 'path comment)) file-path))
                                        all-comments))
             ;; Enhance with outdated property and stale detection (matches main fetch)
             (enhanced-comments (shipit--enhance-rest-comments-with-outdated file-comments repo head-sha)))

        (shipit--debug-log "TARGET-REFRESH: Filtered to %d comments for file %s"
                           (length enhanced-comments) file-path)

        ;; Fetch reactions for targeted refresh via backend dispatch
        (shipit-comment--fetch-reactions-batch enhanced-comments repo t)

        enhanced-comments))))

(defun shipit--fetch-code-snippet-with-navigation (owner repo sha file-path start-line end-line)
  "Fetch code snippet from GitHub API with clickable navigation header."
  (let ((fallback-url (format "https://github.com/%s/%s/blob/%s/%s#L%d%s"
                              owner repo sha file-path start-line
                              (if end-line (format "-L%d" end-line) ""))))
    (condition-case nil
        (let* ((endpoint (format "/repos/%s/%s/contents/%s" owner repo file-path))
               (params `((ref . ,sha)))
               (response (shipit--api-request endpoint params))
               (content (when response
                          (base64-decode-string (cdr (assq 'content response)))))
               (lines (when content
                        (split-string content "\n")))
               (snippet-lines (when lines
                                (let ((start (max 0 (1- start-line)))
                                      (end (min (length lines)
                                                (if end-line end-line start-line))))
                                  (seq-subseq lines start end)))))
          (if snippet-lines
              (let* ((numbered-lines (shipit--add-line-numbers snippet-lines start-line owner repo sha file-path))
                     (clickable-header (propertize (format "[%s#L%d%s]"
                                                           file-path start-line
                                                           (if end-line (format "-L%d" end-line) ""))
                                                   'face 'markdown-plain-url-face
                                                   
                                                   'help-echo (format "Click to open %s at line %d" file-path start-line)
                                                   'shipit-protected-face t
                                                   'keymap (let ((map (make-sparse-keymap)))
                                                             (define-key map [mouse-1]
                                                                         `(lambda (event)
                                                                            (interactive "e")
                                                                            (shipit--navigate-to-github-url
                                                                             ,owner ,repo ,sha ,file-path ,start-line ,end-line)))
                                                             (define-key map [return]
                                                                         `(lambda ()
                                                                            (interactive)
                                                                            (shipit--navigate-to-github-url
                                                                             ,owner ,repo ,sha ,file-path ,start-line ,end-line)))
                                                             map)))
                     (result (format "\n%s\n%s\n%s\n%s"
                                     clickable-header
                                     "────────────────────────────────────────────────────────────────────────────────"
                                     numbered-lines
                                     "────────────────────────────────────────────────────────────────────────────────")))
                result)
            (progn
              (propertize (format "[%s#L%d%s]"
                                  file-path start-line
                                  (if end-line (format "-L%d" end-line) ""))
                          'face 'markdown-plain-url-face
                          
                          'help-echo (format "Click to open %s" fallback-url)
                          'shipit-protected-face t
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-1]
                                                `(lambda (event)
                                                   (interactive "e")
                                                   (browse-url ,fallback-url)))
                                    (define-key map [return]
                                                `(lambda ()
                                                   (interactive)
                                                   (browse-url ,fallback-url)))
                                    map)))))
      (error
       (propertize (format "[%s#L%d%s]"
                           file-path start-line
                           (if end-line (format "-L%d" end-line) ""))
                   'face 'markdown-plain-url-face
                   
                   'help-echo (format "Click to open %s" fallback-url)
                   'shipit-protected-face t
                   'keymap (let ((map (make-sparse-keymap)))
                             (define-key map [mouse-1]
                                         `(lambda (event)
                                            (interactive "e")
                                            (browse-url ,fallback-url)))
                             (define-key map [return]
                                         `(lambda ()
                                            (interactive)
                                            (browse-url ,fallback-url)))
                             map))))))

(defun shipit--download-and-cache-image (url)
  "Download image from URL and return cached file path."
  (let* ((cache-dir (expand-file-name "shipit-images" temporary-file-directory))
         (url-hash (secure-hash 'md5 url))
         (file-ext (if (string-match "\\.[a-zA-Z0-9]+$" url)
                       (match-string 0 url)
                     ".png"))
         (cache-file (expand-file-name (concat url-hash file-ext) cache-dir)))
    ;; Create cache directory if it doesn't exist
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))
    ;; Download if not cached
    (unless (file-exists-p cache-file)
      (condition-case err
          ;; Bind system-time-locale to "C" for ASCII HTTP date headers
          (let* ((system-time-locale "C")
                 (backend-host (url-host (url-generic-parse-url
                                          (or (plist-get shipit-pr-backend-config :api-url)
                                              "https://github.com"))))
                 (image-host (url-host (url-generic-parse-url url)))
                 (same-host-p (string-suffix-p backend-host image-host))
                 (url-request-extra-headers
                  `(,@(let ((token (shipit--github-token)))
                        (when (and same-host-p token)
                          `(("Authorization" . ,(concat "token " token)))))
                    ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36")
                    ("Accept" . "image/*,*/*")))
                 (buffer (url-retrieve-synchronously url t nil 10)))
            (when buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                (let* ((status-line (buffer-substring (point-min) (line-end-position)))
                       (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                                     (string-to-number (match-string 1 status-line))))
                       (headers-end (search-forward "\n\n" nil t)))
                  (when (and headers-end (= status-code 200))
                    (with-temp-file cache-file
                      (set-buffer-multibyte nil)
                      (insert-buffer-substring buffer headers-end)))
                  (kill-buffer buffer)))))
        (error nil)))
    (if (file-exists-p cache-file) cache-file nil)))

(defun shipit--get-issue-comments (repo pr-number &optional force-fresh)
  "Get issue comments for PR-NUMBER in REPO using ETag caching.
If FORCE-FRESH is non-nil, bypass ETag cache and fetch fresh data."
  ;; Offensive programming: validate required parameters
  (unless (and repo (stringp repo))
    (error "shipit: repo must be a non-empty string, got %s: %S" (type-of repo) repo))
  (unless (and pr-number (numberp pr-number))
    (error "shipit: pr-number must be a number, got %s: %S" (type-of pr-number) pr-number))

  (let* ((endpoint (format "/repos/%s/issues/%s/comments" repo pr-number))
         (result (shipit-gh-etag-get-json-with-refresh-cache endpoint '((per_page . 100)) shipit-github-token force-fresh))
         (data (plist-get result :json)))
    (shipit--debug-log "Fetched issue comments for PR #%s: %d comments" pr-number (length data))
    ;; Add comment type marker to issue comments
    (mapcar (lambda (comment)
              ;; Offensive programming: validate each comment
              (unless (and comment (listp comment))
                (error "shipit: Expected comment list but got %s: %S" (type-of comment) comment))
              (append comment '((shipit-comment-type . "issue"))))
            data)))

(defun shipit--get-pr-reviews-for-comments (repo pr-number &optional force-fresh)
  "Get PR reviews for PR-NUMBER in REPO using ETag caching, formatted as comments.
If FORCE-FRESH is non-nil, bypass ETag cache and fetch fresh data."
  ;; Offensive programming: validate required parameters
  (unless (and repo (stringp repo))
    (error "shipit: repo must be a non-empty string, got %s: %S" (type-of repo) repo))
  (unless (and pr-number (numberp pr-number))
    (error "shipit: pr-number must be a number, got %s: %S" (type-of pr-number) pr-number))

  (let* ((endpoint (format "/repos/%s/pulls/%s/reviews" repo pr-number))
         (result (shipit-gh-etag-get-json-with-refresh-cache endpoint '((per_page . 100)) shipit-github-token force-fresh))
         (data (plist-get result :json)))
    (shipit--debug-log "Fetched PR reviews for PR #%s: %d reviews" pr-number (length data))
    ;; Convert PR reviews to comment-like format, including PR number for reaction endpoints
    (shipit--convert-reviews-to-comments data pr-number)))

(defun shipit--fetch-general-comments (repo pr-number &optional force-fresh)
  "Fetch general PR comments (including reviews) using ETag caching and refresh magit buffer.
If FORCE-FRESH is non-nil, bypass ETag cache and fetch fresh data."
  ;; Offensive programming: validate required parameters
  (unless (and repo (stringp repo))
    (error "shipit: repo must be a non-empty string, got %s: %S" (type-of repo) repo))
  (unless (and pr-number (numberp pr-number))
    (error "shipit: pr-number must be a number, got %s: %S" (type-of pr-number) pr-number))

  (shipit--debug-log "Fetching general comments for PR #%s" pr-number)
  ;; Use new ETag-cached functions to fetch data synchronously
  (let* ((issue-comments (shipit--get-issue-comments repo pr-number force-fresh))
         (review-comments (shipit--get-pr-reviews-for-comments repo pr-number force-fresh))
         (combined-comments (append issue-comments review-comments))
         ;; ROOT CAUSE FIX: Deduplicate comments by ID to prevent duplicate reviews
         (all-comments (cl-remove-duplicates combined-comments
                                           :key (lambda (c) (cdr (assq 'id c)))
                                           :test 'equal)))
    (when (< (length all-comments) (length combined-comments))
      (shipit--debug-log "ROOT-CAUSE-FIX: ✅ Removed %d duplicate comments by ID (reviews appearing multiple times)"
                         (- (length combined-comments) (length all-comments))))

    ;; Update cache and flags with deduplication
    ;; Deduplicate: Remove any general comments that also exist in inline comments cache
    (let* ((inline-comment-ids (when (boundp 'shipit--cached-inline-comments)
                                  (mapcar (lambda (c) (cdr (assq 'id c))) shipit--cached-inline-comments)))
           (deduplicated-comments
            (if inline-comment-ids
                (cl-remove-if (lambda (comment)
                                (let ((comment-id (cdr (assq 'id comment))))
                                  (member comment-id inline-comment-ids)))
                              all-comments)
              all-comments)))
      (setq shipit--cached-general-comments deduplicated-comments)
      (when (< (length deduplicated-comments) (length all-comments))
        (shipit--debug-log "DEDUPLICATION: Removed %d duplicate comments from general comments (inline comment IDs: %s)"
                           (- (length all-comments) (length deduplicated-comments))
                           inline-comment-ids)))
    (setq shipit--general-comments-fetched t)

    ;; Debug: Log the comment IDs we're caching
    (shipit--debug-log "FETCH-COMPLETE: Set general-comments-fetched=t, cached-count=%d"
                       (length shipit--cached-general-comments))
    (shipit--debug-log "Cached general comments with IDs: %s"
                       (mapcar (lambda (c) (cdr (assq 'id c))) shipit--cached-general-comments))

    ;; Fetch reactions for general comments via backend dispatch
    ;; Filter out outdated comments - backends may not support reactions on them
    (let ((active-comments (cl-remove-if (lambda (c) (cdr (assq 'outdated c))) shipit--cached-general-comments)))
      (shipit--debug-log "About to fetch reactions for %d general comments (filtered from %d total, outdated=%d)"
                         (length active-comments) (length shipit--cached-general-comments)
                         (- (length shipit--cached-general-comments) (length active-comments)))
      (when active-comments
        (shipit-comment--fetch-reactions-batch active-comments repo nil)))

    ;; Note: Do NOT call shipit--refresh-general-comments-section here
    ;; because it would create an infinite loop:
    ;; shipit--refresh-general-comments-section -> shipit--fetch-general-comments -> shipit--refresh-general-comments-section
    ;; The caller is responsible for refreshing the UI after fetching

    shipit--cached-general-comments))

(defun shipit--convert-reviews-to-comments (reviews &optional pr-number)
  "Convert PR reviews to comment-like format for threading.
PR-NUMBER is included in review objects for reaction endpoint construction."
  ;; Offensive programming: validate input
  (unless (listp reviews)
    (error "shipit: Expected list of reviews but got %s: %S" (type-of reviews) reviews))
  (let ((result (mapcar (lambda (review)
                          ;; Offensive programming: validate each review
                          (unless (and review (listp review))
                            (error "shipit: Expected review list but got %s: %S" (type-of review) review))
                          (let* ((body (cdr (assq 'body review)))
                                 (state (cdr (assq 'state review)))
                                 (user (cdr (assq 'login (cdr (assq 'user review)))))
                                 (review-id (cdr (assq 'id review)))
                                 (submitted-at (cdr (assq 'submitted_at review)))
                                 (created-at (cdr (assq 'created_at review)))
                                 (should-include (or (and body (not (string-empty-p body)))
                                                     (and shipit-show-empty-review-messages
                                                          (member state '("APPROVED" "CHANGES_REQUESTED" "DISMISSED"))))))
                            ;; Debug: Log what we're processing
                            (let ((user-obj (cdr (assq 'user review)))
                                  (avatar-url (cdr (assq 'avatar_url (cdr (assq 'user review))))))
                              (shipit--debug-log "REVIEW-CONVERT: Processing review ID %s by %s, state=%s, body-length=%s, submitted_at=%s, created_at=%s, user-obj=%s, avatar=%s"
                                                 review-id user state
                                                 (if body (length body) "nil")
                                                 (or submitted-at "nil")
                                                 (or created-at "nil")
                                                 (if user-obj "present" "missing")
                                                 (or avatar-url "nil")))
                            (shipit--debug-log "REVIEW-CONVERT: Review ID %s should-include=%s (has-body=%s, significant-state=%s)"
                                               review-id should-include
                                               (and body (not (string-empty-p body)))
                                               (member state '("APPROVED" "CHANGES_REQUESTED" "DISMISSED")))
                            (when should-include
                              ;; Create a comment-like structure from the review
                              ;; Ensure proper timestamp field (reviews use submitted_at, comments use created_at)
                              (append review `((shipit-comment-type . "review")
                                               (review-state . ,state)
                                               ,@(when pr-number `((shipit-pr-number . ,pr-number)))
                                               ;; Ensure created_at is available for timestamp formatting
                                               (created_at . ,(or created-at submitted-at)))))))
                        reviews)))
    (let ((filtered-result (remove nil result)))
      (shipit--debug-log "REVIEW-CONVERT: Filtered %d reviews from original %d, final result has %d items"
                         (length result) (length reviews) (length filtered-result))
      (dolist (review filtered-result)
        (let ((review-id (cdr (assq 'id review)))
              (state (cdr (assq 'review-state review)))
              (comment-type (cdr (assq 'shipit-comment-type review))))
          (shipit--debug-log "REVIEW-CONVERT: Final review ID %s, type=%s, state=%s"
                             review-id comment-type state)))
      filtered-result)))

(defun shipit--add-reaction-to-comment (comment-id reaction repo pr-number &optional is-inline comment)
  "Add REACTION to comment with COMMENT-ID in REPO PR-NUMBER.
If COMMENT is provided, checks its type to determine endpoint.
If IS-INLINE is non-nil, uses pull request review comment endpoint,
otherwise uses issue comment endpoint or GraphQL for PR reviews."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  ;; Offensive programming: validate inputs
  (unless (and comment-id (or (stringp comment-id) (numberp comment-id)))
    (error "shipit: comment-id must be a string or number, got %s: %S" (type-of comment-id) comment-id))
  (unless (and reaction (stringp reaction) (not (string-empty-p reaction)))
    (error "shipit: reaction must be a non-empty string, got %s: %S" (type-of reaction) reaction))

  ;; Determine if this is a review and if we have the node_id for GraphQL
  ;; Also correct is-inline based on comment object if available (but only if not issue type)
  (let* ((is-review (and comment (equal (cdr (assq 'shipit-comment-type comment)) "review")))
         (node-id (when is-review (cdr (assq 'node_id comment))))
         (use-graphql (and is-review node-id))
         ;; If we have a comment object with a comment-type, use that to determine is-inline
         ;; Issue comments (shipit-comment-type="issue") are always general, never inline
         ;; Everything else (including reviews which we handle separately) should use the passed is-inline
         (corrected-is-inline (cond
                              ;; If it's an issue comment (non-review general comment), it's never inline
                              ((and comment (equal (cdr (assq 'shipit-comment-type comment)) "issue"))
                               nil)
                              ;; Otherwise trust the passed-in is-inline value
                              (t is-inline))))

    (shipit--debug-log "🎯 ADD-REACTION: Starting - repo=%s, comment-id=%s, is-inline=%s, corrected-is-inline=%s, is-review=%s, use-graphql=%s"
                       repo comment-id is-inline corrected-is-inline is-review use-graphql)

    (let* ((resolved (shipit-comment--resolve-for-repo repo))
           (backend (car resolved))
           (config (cdr resolved)))
      (condition-case err
          (let ((result (if use-graphql
                           (funcall (plist-get backend :add-review-reaction) config node-id reaction)
                         (funcall (plist-get backend :toggle-reaction) config comment-id reaction corrected-is-inline))))

            ;; Offensive programming: validate result
            (unless result
              (error "shipit: No result received from POST request"))

            ;; Fetch fresh reactions for cache via backend dispatch
            (when (boundp 'shipit--reaction-cache)
              (condition-case err
                  (if use-graphql
                      (let ((cache-key (shipit--reaction-cache-key repo comment-id corrected-is-inline))
                            (fresh-reactions (funcall (plist-get backend :fetch-review-reactions) config node-id)))
                        (puthash cache-key (or fresh-reactions '()) shipit--reaction-cache))
                    (shipit-comment--fetch-reactions repo comment-id corrected-is-inline))
                (error
                 (remhash (shipit--reaction-cache-key repo comment-id corrected-is-inline)
                          shipit--reaction-cache))))

            ;; Update display
            (shipit--update-comment-reactions-display comment-id corrected-is-inline repo)

            ;; Refresh ediff buffers
            (dolist (buffer (buffer-list))
              (with-current-buffer buffer
                (when (and (boundp 'shipit--ediff-pr-number) shipit--ediff-pr-number)
                  (shipit--refresh-ediff-comments))))

            (message "Added %s reaction" reaction)
            result)
        (error
         (message "Failed to add reaction: %s" (error-message-string err))
         nil)))))

(defvar shipit--reactions-refresh-timer nil
  "Timer for debounced reactions refresh.")

(defun shipit--debounced-reactions-refresh ()
  "Debounced refresh to avoid multiple rapid refreshes when reactions are fetched."
  ;; Cancel any existing timer
  (when shipit--reactions-refresh-timer
    (cancel-timer shipit--reactions-refresh-timer))
  ;; Set new timer for 0.5 seconds from now
  (setq shipit--reactions-refresh-timer
        (run-at-time 0.5 nil
                     (lambda ()
                       (setq shipit--reactions-refresh-timer nil)
                       (shipit--defer-refresh)))))

(defun shipit--add-reaction-to-pr (pr-number reaction)
  "Add REACTION to PR with PR-NUMBER using GitHub issues API (since PRs are issues)."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  ;; Offensive programming: validate inputs
  (unless (and pr-number (or (stringp pr-number) (numberp pr-number)))
    (error "shipit: pr-number must be a string or number, got %s: %S" (type-of pr-number) pr-number))
  (unless (and reaction (stringp reaction) (not (string-empty-p reaction)))
    (error "shipit: reaction must be a non-empty string, got %s: %S" (type-of reaction) reaction))

  (let* ((repo shipit-current-repo)
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (endpoint (format "/repos/%s/issues/%s/reactions" repo pr-number)))
    (shipit--debug-log "🎯 ADD-PR-REACTION: Starting - repo=%s, pr-number=%s" repo pr-number)
    (condition-case err
        (let ((result (funcall (plist-get backend :add-reaction) config pr-number reaction)))
          ;; Offensive programming: validate result
          (unless result
            (error "shipit: No result received from POST request"))

          (shipit--debug-log "✅ ADD-PR-REACTION: POST successful, now clearing caches")

          ;; Clear ETag cache for reactions endpoint to force fresh fetch
          (shipit-clear-etag-cache-for-endpoint endpoint)

          ;; Clear refresh cache to force fresh fetch even during same refresh cycle
          (when (and (boundp 'shipit-gh-etag--refresh-cache)
                    shipit-gh-etag--refresh-cache)
            (let* ((qstr nil)
                   (repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                                 (match-string 1 endpoint)))
                   (refresh-cache-key (if repo-match
                                         (concat repo-match endpoint (or qstr ""))
                                       (concat endpoint (or qstr "")))))
              (when (gethash refresh-cache-key shipit-gh-etag--refresh-cache)
                (remhash refresh-cache-key shipit-gh-etag--refresh-cache))))

          ;; Immediate fresh fetch (pre-populate cache for next refresh)
          (when (boundp 'shipit--reaction-cache)
            (let ((cache-key (format "pr-%s" pr-number)))
              (condition-case err
                  (let* ((etag-result (shipit-gh-etag-get-json endpoint nil shipit-github-token t))
                         (fresh-reactions (when etag-result (plist-get etag-result :json))))
                    (puthash cache-key (or fresh-reactions '()) shipit--reaction-cache))
                (error
                 (remhash cache-key shipit--reaction-cache)))))

          ;; Targeted display update
          (shipit--update-pr-reactions-display pr-number)

          (message "Added %s reaction" reaction)
          result)
      (error
       (message "Failed to add reaction to PR: %s" (error-message-string err))
       nil))))

(defun shipit--remove-reaction-from-comment (comment-id reaction repo pr-number &optional is-inline comment)
  "Remove REACTION from comment with COMMENT-ID in REPO PR-NUMBER.
If COMMENT is provided, checks its type to determine endpoint.
If IS-INLINE is non-nil, uses pull request review comment endpoint,
otherwise uses issue comment endpoint or GraphQL for PR reviews."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  (unless (and comment-id (or (stringp comment-id) (numberp comment-id)))
    (error "shipit: comment-id must be a string or number, got %s: %S" (type-of comment-id) comment-id))
  (unless (and reaction (stringp reaction) (not (string-empty-p reaction)))
    (error "shipit: reaction must be a non-empty string, got %s: %S" (type-of reaction) reaction))

  (let* ((is-review (and comment (equal (cdr (assq 'shipit-comment-type comment)) "review")))
         (node-id (when is-review (cdr (assq 'node_id comment))))
         (use-graphql (and is-review node-id))
         ;; If we have a comment object with a comment-type, use that to determine is-inline
         ;; Issue comments (shipit-comment-type="issue") are always general, never inline
         ;; Everything else (including reviews which we handle separately) should use the passed is-inline
         (corrected-is-inline (cond
                              ;; If it's an issue comment (non-review general comment), it's never inline
                              ((and comment (equal (cdr (assq 'shipit-comment-type comment)) "issue"))
                               nil)
                              ;; Otherwise trust the passed-in is-inline value
                              (t is-inline))))

    (shipit--debug-log "REMOVE-REACTION: repo=%s comment-id=%s is-review=%s use-graphql=%s corrected-is-inline=%s"
                       repo comment-id is-review use-graphql corrected-is-inline)

    (let* ((resolved (shipit-comment--resolve-for-repo repo))
           (backend (car resolved))
           (config (cdr resolved)))
      (if use-graphql
          (condition-case err
              (let ((result (funcall (plist-get backend :remove-review-reaction) config node-id reaction)))
                (when result
                  (when (boundp 'shipit--reaction-cache)
                    (let ((cache-key (shipit--reaction-cache-key repo comment-id corrected-is-inline)))
                      (condition-case err
                          (let ((fresh-reactions (funcall (plist-get backend :fetch-review-reactions) config node-id)))
                            (puthash cache-key (or fresh-reactions '()) shipit--reaction-cache))
                        (error
                         (remhash cache-key shipit--reaction-cache)))))
                  (shipit--update-comment-reactions-display comment-id corrected-is-inline repo)
                  (dolist (buffer (buffer-list))
                    (with-current-buffer buffer
                      (when (and (boundp 'shipit--ediff-pr-number) shipit--ediff-pr-number)
                        (shipit--refresh-ediff-comments))))
                  (message "Removed %s reaction" reaction)
                  t)
                (progn
                  (message "Failed to remove reaction")
                  nil))
            (error
             (message "Failed to remove reaction: %s" (error-message-string err))
             nil))

        ;; REST path: fetch reactions, find user's, delete by ID
        (let* ((reactions (funcall (plist-get backend :fetch-reactions) config comment-id corrected-is-inline))
               (current-user (shipit--get-current-user))
               (reaction-to-delete (cl-find-if (lambda (r)
                                                 (and (string= (cdr (assq 'content r)) reaction)
                                                      (string= (cdr (assq 'login (cdr (assq 'user r)))) current-user)))
                                               reactions)))
          (unless (listp reactions)
            (error "shipit: Expected list of reactions but got %s: %S" (type-of reactions) reactions))
          (unless (and current-user (stringp current-user) (not (string-empty-p current-user)))
            (error "shipit: Expected non-empty current user string but got %s: %S" (type-of current-user) current-user))

          (if reaction-to-delete
              (let ((reaction-id (cdr (assq 'id reaction-to-delete))))
                (unless (and reaction-id (numberp reaction-id))
                  (error "shipit: Expected numeric reaction ID but got %s: %S" (type-of reaction-id) reaction-id))

                (condition-case err
                    (progn
                      (funcall (plist-get backend :delete-reaction) config comment-id reaction-id corrected-is-inline)

                      ;; Cache refresh via backend dispatch
                      (when (boundp 'shipit--reaction-cache)
                        (condition-case err
                            (shipit-comment--fetch-reactions repo comment-id corrected-is-inline)
                          (error
                           (remhash (shipit--reaction-cache-key repo comment-id corrected-is-inline)
                                    shipit--reaction-cache))))

                      ;; Display update
                      (shipit--update-comment-reactions-display comment-id corrected-is-inline repo)
                      (dolist (buffer (buffer-list))
                        (with-current-buffer buffer
                          (when (and (boundp 'shipit--ediff-pr-number) shipit--ediff-pr-number)
                            (shipit--refresh-ediff-comments))))
                      (message "Removed %s reaction" reaction)
                      t)
                  (error
                   (message "Failed to remove reaction: %s" (error-message-string err))
                   nil)))
            (progn
              (message "You don't have a %s reaction to remove" reaction)
              nil)))))))


(defun shipit--remove-reaction-from-pr (pr-number reaction)
  "Remove REACTION from PR with PR-NUMBER."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  ;; Offensive programming: validate inputs
  (unless (and pr-number (or (stringp pr-number) (numberp pr-number)))
    (error "shipit: pr-number must be a string or number, got %s: %S" (type-of pr-number) pr-number))
  (unless (and reaction (stringp reaction) (not (string-empty-p reaction)))
    (error "shipit: reaction must be a non-empty string, got %s: %S" (type-of reaction) reaction))

  (let* ((repo shipit-current-repo)
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (reactions (funcall (plist-get backend :fetch-reactions) config pr-number))
         (current-user (shipit--get-current-user))
         (reaction-to-delete (cl-find-if (lambda (r)
                                           (and (string= (cdr (assq 'content r)) reaction)
                                                (string= (cdr (assq 'login (cdr (assq 'user r)))) current-user)))
                                         reactions))
         (endpoint (format "/repos/%s/issues/%s/reactions" repo pr-number)))
    ;; Offensive programming: validate API responses
    (unless (listp reactions)
      (error "shipit: Expected list of reactions but got %s: %S" (type-of reactions) reactions))
    (unless (and current-user (stringp current-user) (not (string-empty-p current-user)))
      (error "shipit: Expected non-empty current user string but got %s: %S" (type-of current-user) current-user))

    (if reaction-to-delete
        (let ((reaction-id (cdr (assq 'id reaction-to-delete))))
          ;; Offensive programming: validate reaction data
          (unless (and reaction-id (numberp reaction-id))
            (error "shipit: Expected numeric reaction ID but got %s: %S" (type-of reaction-id) reaction-id))

          (shipit--debug-log "🎯 REMOVE-PR-REACTION: Starting - pr-number=%s, reaction-id=%s" pr-number reaction-id)
          (condition-case err
              (progn
                (funcall (plist-get backend :delete-reaction) config pr-number reaction-id)

                ;; Clear ETag cache for reactions endpoint
                (shipit-clear-etag-cache-for-endpoint endpoint)

                ;; Clear refresh cache
                (when (and (boundp 'shipit-gh-etag--refresh-cache)
                          shipit-gh-etag--refresh-cache)
                  (let* ((qstr nil)
                         (repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                                       (match-string 1 endpoint)))
                         (refresh-cache-key (if repo-match
                                               (concat repo-match endpoint (or qstr ""))
                                             (concat endpoint (or qstr "")))))
                    (when (gethash refresh-cache-key shipit-gh-etag--refresh-cache)
                      (remhash refresh-cache-key shipit-gh-etag--refresh-cache))))

                ;; Immediate fresh fetch
                (when (boundp 'shipit--reaction-cache)
                  (let ((cache-key (format "pr-%s" pr-number)))
                    (condition-case err
                        (let* ((etag-result (shipit-gh-etag-get-json endpoint nil shipit-github-token t))
                               (fresh-reactions (when etag-result (plist-get etag-result :json))))
                          (puthash cache-key (or fresh-reactions '()) shipit--reaction-cache))
                      (error
                       (remhash cache-key shipit--reaction-cache)))))

                ;; Targeted display update
                (shipit--update-pr-reactions-display pr-number)

                (message "Removed %s reaction" reaction)
                t)
            (error
             (message "Failed to remove reaction from PR: %s" (error-message-string err))
             nil)))
      (progn
        (message "You don't have a %s reaction to remove from PR" reaction)
        nil))))

(defun shipit--update-reactions-line (formatted-reactions indent extra-props)
  "Replace current line with FORMATTED-REACTIONS using INDENT.
EXTRA-PROPS is a plist of text properties to apply.
Point should be on the reactions line.  Returns t on success."
  (beginning-of-line)
  (let ((line-start (point)))
    (delete-region line-start (progn (end-of-line) (if (< (point) (point-max)) (1+ (point)) (point))))
    (when (and formatted-reactions (not (string-empty-p formatted-reactions)))
      ;; Extract just the reactions after placeholder (everything after first space)
      ;; Format is: "[placeholder] emoji1 count1 emoji2 count2 ..."
      (let ((reactions-only (if (string-match " \\(.+\\)" formatted-reactions)
                                (match-string 1 formatted-reactions)
                              "")))
        (insert indent reactions-only "\n")
        (add-text-properties line-start (point)
                             (append '(shipit-reactions t) extra-props))
        t))))

(defun shipit--insert-description-reactions (number repo extra-props)
  "Insert a reactions block for description NUMBER in REPO with EXTRA-PROPS.
Renders: blank line, reactions line, blank line.
Shared by PR and issue description rendering.
EXTRA-PROPS is a plist of text properties to apply alongside `shipit-reactions'."
  (let* ((shipit-current-repo repo)
         (reactions (shipit--format-pr-reactions number)))
    ;; Blank line before reactions
    (let ((start (point)))
      (insert "\n")
      (add-text-properties start (point) extra-props))
    ;; Reactions line
    (let ((reaction-start (point)))
      (insert (format "   %s\n" reactions))
      (add-text-properties reaction-start (point)
                           (append '(shipit-reactions t) extra-props)))
    ;; Blank line after reactions
    (let ((start (point)))
      (insert "\n")
      (add-text-properties start (point) extra-props))))

(defun shipit--update-description-reactions (number description-prop buffer-finder extra-props-fn)
  "Update a description reactions line for NUMBER.
DESCRIPTION-PROP is the text property identifying the description (e.g.
`shipit-pr-description' or `shipit-issue-description').
BUFFER-FINDER is a function returning the buffer to update.
EXTRA-PROPS-FN is called at point on the reactions line and returns extra props plist."
  (shipit--debug-log "🔄 UPDATE-REACTIONS: Starting for #%s (prop=%s)" number description-prop)
  (let ((buf (funcall buffer-finder)))
    (if buf
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-min))
              (while (and (< (point) (point-max))
                          (not (and (get-text-property (point) description-prop)
                                    (get-text-property (point) 'shipit-reactions))))
                (goto-char (or (next-single-property-change (point) 'shipit-reactions)
                               (point-max))))
              (when (and (< (point) (point-max))
                         (get-text-property (point) description-prop)
                         (get-text-property (point) 'shipit-reactions))
                (let* ((old-text (buffer-substring (line-beginning-position) (line-end-position)))
                       (indent (or (when (string-match "^\\([ \t]*\\)" old-text)
                                     (match-string 1 old-text))
                                   "   "))
                       (reactions (shipit--format-pr-reactions number)))
                  (shipit--update-reactions-line
                   reactions indent (funcall extra-props-fn)))))))
      (shipit--debug-log "⚠️ UPDATE-REACTIONS: No buffer found for #%s" number))))

(defun shipit--update-pr-reactions-display (pr-number)
  "Update the PR description reactions display for PR-NUMBER in shipit buffer."
  (shipit--update-description-reactions
   pr-number 'shipit-pr-description
   (lambda ()
     (cl-find-if (lambda (b)
                   (with-current-buffer b
                     (and (derived-mode-p 'shipit-mode)
                          (boundp 'shipit-buffer-pr-number)
                          (equal shipit-buffer-pr-number pr-number))))
                 (buffer-list)))
   (lambda ()
     (let ((pr-body (get-text-property (point) 'shipit-pr-body))
           (repo (get-text-property (point) 'shipit-repo)))
       `(shipit-pr-description t
         shipit-pr-number ,pr-number
         shipit-pr-body ,pr-body
         shipit-repo ,repo)))))

(defun shipit--get-repository-labels ()
  "Get all available labels for the current repository."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  (let* ((endpoint (format "/repos/%s/labels" shipit-current-repo))
         (url (concat shipit-api-url endpoint)))
    (condition-case err
        (let* ((result (shipit-gh-etag-get-json-with-refresh-cache endpoint nil shipit-github-token)))
          (plist-get result :json))
      (error
       (message "Failed to fetch repository labels: %s" (error-message-string err))
       nil))))

(defun shipit--add-label-to-pr (pr-number label-name)
  "Add LABEL-NAME to PR with PR-NUMBER."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  ;; Offensive programming: validate inputs
  (unless (and pr-number (or (stringp pr-number) (numberp pr-number)))
    (error "shipit: pr-number must be a string or number, got %s: %S" (type-of pr-number) pr-number))
  (unless (and label-name (stringp label-name) (not (string-empty-p label-name)))
    (error "shipit: label-name must be a non-empty string, got %s: %S" (type-of label-name) label-name))

  (let* ((repo shipit-current-repo)
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (condition-case err
        (let ((result (funcall (plist-get backend :add-label) config pr-number label-name)))
          ;; Offensive programming: validate result
          (unless result
            (error "shipit: No result received from POST request"))
          (message "Added label '%s' to PR %s" label-name pr-number)
          result)
      (error
       (message "Failed to add label '%s' to PR: %s" label-name (error-message-string err))
       nil))))

(defun shipit--remove-label-from-pr (pr-number label-name)
  "Remove LABEL-NAME from PR with PR-NUMBER."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  ;; Offensive programming: validate inputs
  (unless (and pr-number (or (stringp pr-number) (numberp pr-number)))
    (error "shipit: pr-number must be a string or number, got %s: %S" (type-of pr-number) pr-number))
  (unless (and label-name (stringp label-name) (not (string-empty-p label-name)))
    (error "shipit: label-name must be a non-empty string, got %s: %S" (type-of label-name) label-name))

  (let* ((repo shipit-current-repo)
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (condition-case err
        (progn
          (funcall (plist-get backend :remove-label) config pr-number label-name)
          (message "Removed label '%s' from PR %s" label-name pr-number)
          t)
      (error
       (message "Failed to remove label '%s' from PR: %s" label-name (error-message-string err))
       nil))))

(defun shipit--clear-comment-operation-caches (operation-name repo comment-id endpoint &optional is-inline)
  "Clear all relevant caches after a comment operation.
Standardized cache clearing for consistent UI updates after comment operations."
  (shipit--debug-log "🧹 %s: SUCCESS! Aggressively clearing ALL caches - repo=%s comment-id=%s is-inline=%s"
                     operation-name repo comment-id is-inline)

  ;; Clear reaction cache (specific to the comment) - both inline and general variants
  (when (boundp 'shipit--reaction-cache)
    ;; Clear the specific cache variant (inline or general)
    (let ((cache-key (shipit--reaction-cache-key repo comment-id is-inline)))
      (remhash cache-key shipit--reaction-cache)
      (shipit--debug-log "🧹 %s: Cleared reaction cache entry: %s" operation-name cache-key))

    ;; CRITICAL: Also clear the opposite variant since reactions are checked both ways
    (let ((opposite-cache-key (shipit--reaction-cache-key repo comment-id (not is-inline))))
      (remhash opposite-cache-key shipit--reaction-cache)
      (shipit--debug-log "🧹 %s: Cleared opposite reaction cache entry: %s" operation-name opposite-cache-key)))

  ;; Clear ETag cache (forces fresh API data on next request)
  (when (fboundp 'shipit-clear-etag-cache-for-endpoint)
    (shipit-clear-etag-cache-for-endpoint endpoint)
    (shipit--debug-log "🧹 %s: Cleared ETag cache for endpoint: %s" operation-name endpoint)

    ;; CRITICAL: Also clear the comments LIST endpoint that gets fetched during refresh
    ;; Try to get PR number from text properties, with fallback detection
    (let* ((pr-number (or (get-text-property (point) 'shipit-pr-number)
                          (when (boundp 'shipit-current-pr) shipit-current-pr)
                          1))  ; fallback to 1 if we can't detect
           (list-endpoint (if is-inline
                              (format "/repos/%s/pulls/%s/comments" repo pr-number)
                            (format "/repos/%s/issues/%s/comments" repo pr-number))))
      (shipit-clear-etag-cache-for-endpoint list-endpoint)
      (shipit--debug-log "🧹 %s: Cleared ETag cache for LIST endpoint: %s (PR #%s)" operation-name list-endpoint pr-number)))

  ;; Clear comment caches (forces UI refresh)
  (when (boundp 'shipit--cached-general-comments)
    (let ((before-count (if shipit--cached-general-comments (length shipit--cached-general-comments) 0)))
      (setq shipit--cached-general-comments nil)
      (shipit--debug-log "🧹 %s: Cleared general comments cache (had %d comments)" operation-name before-count)))
  (when (boundp 'shipit--cached-inline-comments)
    (let ((before-count (if shipit--cached-inline-comments (length shipit--cached-inline-comments) 0)))
      (setq shipit--cached-inline-comments nil)
      (shipit--debug-log "🧹 %s: Cleared inline comments cache (had %d comments)" operation-name before-count)))

  ;; Clear per-file comment cache for all files in this repo
  (when (boundp 'shipit--comment-cache)
    (let ((keys-to-remove '()))
      (maphash (lambda (key value)
                 (when (string-prefix-p (format "%s:" repo) key)
                   (push key keys-to-remove)))
               shipit--comment-cache)
      (dolist (key keys-to-remove)
        (remhash key shipit--comment-cache))
      (shipit--debug-log "🧹 %s: Cleared %d per-file comment cache entries" operation-name (length keys-to-remove)))))

(defun shipit--delete-comment (comment-id &optional is-inline-comment file-path)
  "Delete comment with COMMENT-ID.
If IS-INLINE-COMMENT is non-nil, delete as an inline comment (review comment).
Otherwise, delete as a general comment (issue comment).
FILE-PATH is used for targeted section refresh when deleting inline/file-level comments."
  ;; Offensive programming: validate inputs
  (unless (and comment-id (or (stringp comment-id) (numberp comment-id)))
    (error "shipit: comment-id must be a string or number, got %s: %S" (type-of comment-id) comment-id))

  (let* ((repo (or (bound-and-true-p shipit-buffer-repo)
                   (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (unless repo
      (error "shipit: Repository must be configured"))

    (shipit--debug-log "🔄 DELETE-COMMENT: Starting delete operation - comment-id=%s is-inline=%s file-path=%s"
                       comment-id is-inline-comment file-path)

    (condition-case err
        (progn
          ;; Backend signals error on failure
          (funcall (plist-get backend :delete-comment) config comment-id is-inline-comment)
          (shipit--debug-log "🔍 DELETE-COMMENT: Backend delete succeeded")

          ;; Clear reaction caches
          (shipit--debug-log "🧹 DELETE-COMMENT: Clearing reaction caches for comment-id=%s" comment-id)
          (when (boundp 'shipit--reaction-cache)
            (let ((cache-key (shipit--reaction-cache-key repo comment-id is-inline-comment))
                  (opposite-cache-key (shipit--reaction-cache-key repo comment-id (not is-inline-comment))))
              (remhash cache-key shipit--reaction-cache)
              (remhash opposite-cache-key shipit--reaction-cache)
              (shipit--debug-log "🧹 DELETE-COMMENT: Cleared reaction caches")))

          ;; Dispatch post-delete operations to backend (ETag/cache invalidation)
          (when-let ((post-fn (plist-get backend :post-delete-comment)))
            (let ((pr-number (or (car shipit--current-displayed-pr)
                                 (bound-and-true-p shipit-buffer-pr-number)
                                 (get-text-property (point) 'shipit-pr-number)
                                 (when (boundp 'shipit-current-pr) shipit-current-pr))))
              (funcall post-fn config comment-id is-inline-comment repo pr-number)))

          ;; Remove deleted comment from cache (don't clear entire cache)
          (if is-inline-comment
              (when (and (boundp 'shipit--cached-inline-comments)
                         shipit--cached-inline-comments)
                (setq shipit--cached-inline-comments
                      (cl-remove-if (lambda (c) (equal (cdr (assq 'id c)) comment-id))
                                    shipit--cached-inline-comments))
                (shipit--debug-log "🔄 DELETE-COMMENT: Removed deleted comment from inline cache, now has %d comments"
                                   (length shipit--cached-inline-comments)))
            (when (and (boundp 'shipit--cached-general-comments)
                       shipit--cached-general-comments)
              (setq shipit--cached-general-comments
                    (cl-remove-if (lambda (c) (equal (cdr (assq 'id c)) comment-id))
                                  shipit--cached-general-comments))
              (shipit--debug-log "🔄 DELETE-COMMENT: Removed deleted comment from general cache, now has %d comments"
                                 (length shipit--cached-general-comments))))

          ;; Track deleted comment for consistency handling
          (when (boundp 'shipit--deleted-comment-ids)
            (puthash comment-id t shipit--deleted-comment-ids)
            (shipit--debug-log "🗑️ DELETE-COMMENT: Marked comment %s as deleted for consistency filtering" comment-id))

          ;; Refresh ediff buffers if any are open
          (when (fboundp 'shipit--refresh-ediff-comments)
            (dolist (buffer (buffer-list))
              (with-current-buffer buffer
                (when (and (boundp 'shipit--ediff-pr-number) shipit--ediff-pr-number)
                  (shipit--refresh-ediff-comments)))))

          ;; Trigger targeted or full refresh
          (let ((pr-number (or (car shipit--current-displayed-pr)
                               (bound-and-true-p shipit-buffer-pr-number)
                               (get-text-property (point) 'shipit-pr-number)
                               (when (boundp 'shipit-current-pr) shipit-current-pr)))
                (captured-comment-id comment-id))
            (when pr-number
              (run-at-time 0 nil
                           (lambda (file-path repo pr-number is-inline cid)
                             (let ((buf-name (format "*shipit: %s#%s*" repo pr-number)))
                               (when (get-buffer buf-name)
                                 (with-current-buffer buf-name
                                   (if (and file-path is-inline
                                            (fboundp 'shipit--refresh-file-inline-comment-section))
                                       ;; Try targeted refresh for inline/file-level comments
                                       (progn
                                         (shipit--debug-log "🔄 DELETE-COMMENT: Attempting targeted refresh for file %s" file-path)
                                         (unless (shipit--refresh-file-inline-comment-section file-path pr-number repo)
                                           ;; Fall back to full refresh if targeted refresh failed
                                           (shipit--debug-log "🔄 DELETE-COMMENT: Targeted refresh failed, falling back to full refresh")
                                           (when (fboundp 'shipit-buffer-refresh)
                                             (shipit-buffer-refresh))))
                                     ;; Try targeted delete for general comments
                                     (if (and (fboundp 'shipit--delete-general-comment-section-targeted)
                                              (shipit--delete-general-comment-section-targeted cid))
                                         (shipit--debug-log "🔄 DELETE-COMMENT: Targeted section delete succeeded for comment %s" cid)
                                       ;; Fall back to full refresh if targeted delete failed
                                       (shipit--debug-log "🔄 DELETE-COMMENT: Targeted delete failed, using full buffer refresh")
                                       (when (fboundp 'shipit-buffer-refresh)
                                         (shipit-buffer-refresh))))))))
                           file-path repo pr-number is-inline-comment captured-comment-id)))

          ;; Mark activities as read since user is actively engaging
          (setq shipit--user-mutated-pr t)
          (message "Comment deleted")
          t)
      (error
       (shipit--debug-log "❌ DELETE-COMMENT: Exception caught: %s" (error-message-string err))
       (message "Failed to delete comment: %s" (error-message-string err))
       nil))))

(defun shipit--get-pr-head-sha (pr-number)
  "Get the HEAD SHA for PR-NUMBER."
  (let* ((displayed-pr-number (car-safe shipit--current-displayed-pr))
         (displayed-repo (cadr shipit--current-displayed-pr))
         (repo (or (bound-and-true-p shipit-buffer-repo)
                   displayed-repo (shipit--get-repo-from-remote)))
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (pr-data (when displayed-pr-number
                    (funcall (plist-get backend :fetch-pr) config displayed-pr-number))))
    (when pr-data
      (let ((head-info (cdr (assq 'head pr-data))))
        (when head-info
          (cdr (assq 'sha head-info)))))))

(defun shipit--reply-to-inline-comment (pr-number parent-comment-id body &optional file-path)
  "Reply to an existing inline comment on PR-NUMBER.
If FILE-PATH is provided, uses targeted section refresh for better performance.
Otherwise, falls back to full PR buffer refresh."
  (let* ((repo (or (bound-and-true-p shipit-buffer-repo)
                   (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (unless repo
      (error "shipit: Repository must be configured"))
    (let ((reply-id (shipit--lookup-discussion-id parent-comment-id)))
      (shipit--debug-log "🔧 INLINE-REPLY API: parent-comment-id=%s, reply-id=%s, body-length=%d, file-path=%s"
                         parent-comment-id reply-id (length body) file-path)
      (condition-case err
          (let ((new-comment (funcall (plist-get backend :reply-to-comment)
                                      config pr-number reply-id body t)))
            (shipit--debug-log "✅ INLINE-REPLY SUCCESS: response=%S" new-comment)

            ;; Add in_reply_to_id to the comment since GitHub API doesn't return it
            ;; This is critical for threading to work correctly
            (when new-comment
              (setcdr new-comment (cons `(in_reply_to_id . ,parent-comment-id)
                                        (cdr new-comment)))
              (shipit--debug-log 'comments "🔄 REPLY-INLINE-COMMENT: Set in_reply_to_id=%s on new comment"
                                 parent-comment-id))

            ;; Copy parent's position fields to reply for cache consistency
            ;; (GitLab reply response lacks path/line/side; GitHub includes them)
            (when (and new-comment (not (cdr (assq 'path new-comment))))
              (when-let ((parent (cl-find-if
                                  (lambda (c) (equal (cdr (assq 'id c)) parent-comment-id))
                                  (and (boundp 'shipit--cached-inline-comments)
                                       shipit--cached-inline-comments))))
                (dolist (field '(path line original_line side diff_hunk))
                  (when-let ((val (cdr (assq field parent))))
                    (setcdr new-comment (cons (cons field val) (cdr new-comment)))))))

            ;; Add the new comment to cached inline comments
            (when (and new-comment (boundp 'shipit--cached-inline-comments))
              (if shipit--cached-inline-comments
                  (push new-comment shipit--cached-inline-comments)
                (setq shipit--cached-inline-comments (list new-comment)))
              (shipit--debug-log 'comments "🔄 REPLY-INLINE-COMMENT: Added reply to cache, total: %d"
                               (length shipit--cached-inline-comments)))

            ;; Dispatch post-reply operations to backend (cache invalidation, reaction pre-fetch)
            (when-let ((post-fn (plist-get backend :post-add-comment)))
              (funcall post-fn config (cdr (assq 'id new-comment)) t repo pr-number))

            ;; Use targeted refresh if file-path is available, otherwise full refresh
            (if (and file-path (fboundp 'shipit--refresh-file-inline-comment-section))
                (progn
                  (shipit--debug-log "TARGET-REFRESH: Using targeted refresh for file %s" file-path)
                  (shipit--refresh-file-inline-comment-section file-path pr-number repo))
              (progn
                (shipit--debug-log "TARGET-REFRESH: file-path not available, using full refresh")
                (when (fboundp 'shipit--refresh-after-comment-operation)
                  (shipit--refresh-after-comment-operation t))))

            ;; Mark activities as read since user is actively engaging
            (setq shipit--user-mutated-pr t)
            (message "Reply added successfully")
            t)
        (error
         (shipit--debug-log "❌ INLINE-REPLY ERROR: %s" (error-message-string err))
         (message "Failed to add reply: %s" (error-message-string err))
         nil)))))

(defun shipit--add-general-comment-to-pr (pr-number body &optional skip-refresh)
  "Add a general comment to PR-NUMBER.
If SKIP-REFRESH is non-nil, don't trigger buffer refresh (for editor with live preview)."
  (let* ((repo (or (bound-and-true-p shipit-buffer-repo)
                   (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (unless repo
      (error "shipit: Repository must be configured"))
    (when repo
      (condition-case err
          (let ((new-comment (funcall (plist-get backend :add-general-comment)
                                      config pr-number body)))
            ;; Add the new comment to cached general comments
            (when (and new-comment (boundp 'shipit--cached-general-comments))
              (if shipit--cached-general-comments
                  (push new-comment shipit--cached-general-comments)
                (setq shipit--cached-general-comments (list new-comment)))
              (shipit--debug-log "🔄 ADD-GENERAL-COMMENT: Added new comment to cache, total: %d"
                               (length shipit--cached-general-comments)))

            ;; Dispatch post-add operations to backend (cache invalidation, reaction pre-fetch)
            (when-let ((post-fn (plist-get backend :post-add-comment)))
              (funcall post-fn config (cdr (assq 'id new-comment)) nil repo pr-number))

            ;; Trigger fast refresh using cached data (unless skip-refresh)
            (unless skip-refresh
              (run-at-time 0 nil (lambda ()
                                   (let ((buf-name (format "*shipit: %s#%s*" repo pr-number)))
                                     (when (get-buffer buf-name)
                                       (with-current-buffer buf-name
                                         (when (fboundp 'shipit-buffer-refresh)
                                           (shipit-buffer-refresh))))))))

            ;; Mark activities as read since user is actively engaging
            (setq shipit--user-mutated-pr t)
            (message "Comment added")
            t)
        (error
         (message "Failed to add general comment: %s" (error-message-string err))
         nil)))))

(defun shipit--lookup-discussion-id (comment-id)
  "Look up discussion_id for COMMENT-ID from cached comments.
Searches both general and inline comment caches.
Returns discussion_id if found, otherwise returns COMMENT-ID unchanged.
GitLab replies require a discussion ID; GitHub uses comment ID directly."
  (or (catch 'found
        (dolist (cache '(shipit--cached-general-comments
                         shipit--cached-inline-comments))
          (when (and (boundp cache) (symbol-value cache))
            (dolist (c (symbol-value cache))
              (when (equal (cdr (assq 'id c)) comment-id)
                (let ((disc-id (cdr (assq 'discussion_id c))))
                  (when disc-id
                    (throw 'found disc-id))))))))
      comment-id))

(defun shipit--reply-to-general-comment (pr-number parent-comment-id body &optional skip-refresh)
  "Reply to a general comment PARENT-COMMENT-ID on PR-NUMBER with BODY.
Uses discussion_id from cache for GitLab, or comment ID for GitHub.
If SKIP-REFRESH is non-nil, don't trigger buffer refresh (for editor with live preview)."
  (let* ((repo (or (bound-and-true-p shipit-buffer-repo)
                   (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (unless repo
      (error "shipit: Repository must be configured"))
    (condition-case err
        (let* ((reply-id (shipit--lookup-discussion-id parent-comment-id))
               (new-comment (funcall (plist-get backend :reply-to-comment)
                                     config pr-number reply-id body)))
          ;; Add the new comment to cached general comments with reply-depth
          (when (and new-comment (boundp 'shipit--cached-general-comments))
            ;; Calculate reply-depth based on parent's depth
            (let* ((parent-depth (shipit--get-comment-depth parent-comment-id))
                   (reply-depth (1+ parent-depth)))
              ;; Add reply-depth to comment before caching
              (setcdr new-comment (cons `(reply-depth . ,reply-depth)
                                        (cdr new-comment)))
              (shipit--debug-log 'comments "🔄 REPLY-GENERAL-COMMENT: Setting reply-depth=%d (parent-depth=%d)"
                                 reply-depth parent-depth))
            (if shipit--cached-general-comments
                (push new-comment shipit--cached-general-comments)
              (setq shipit--cached-general-comments (list new-comment)))
            (shipit--debug-log 'comments "🔄 REPLY-GENERAL-COMMENT: Added reply to cache, total: %d"
                             (length shipit--cached-general-comments))
            ;; Debug: Check if GitHub returned in_reply_to_id
            (let ((reply-id (cdr (assq 'id new-comment)))
                  (in-reply-to-id (cdr (assq 'in_reply_to_id new-comment))))
              (shipit--debug-log 'comments "🔍 REPLY DEBUG: new comment id=%s, in_reply_to_id=%S (we sent parent-comment-id=%s)"
                                 reply-id in-reply-to-id parent-comment-id)))

          ;; Dispatch post-reply operations to backend (cache invalidation, reaction pre-fetch)
          (when-let ((post-fn (plist-get backend :post-add-comment)))
            (funcall post-fn config (cdr (assq 'id new-comment)) nil repo pr-number))

          ;; Trigger fast refresh using cached data (unless skip-refresh)
          (unless skip-refresh
            (run-at-time 0 nil (lambda ()
                                 (let ((buf-name (format "*shipit: %s#%s*" repo pr-number)))
                                   (when (get-buffer buf-name)
                                     (with-current-buffer buf-name
                                       (when (fboundp 'shipit-buffer-refresh)
                                         (shipit-buffer-refresh))))))))

          ;; Mark activities as read since user is actively engaging
          (setq shipit--user-mutated-pr t)
          (message "Reply sent")
          new-comment)  ; Return the new comment for caller to use
      (error
       (message "Failed to reply to comment: %s" (error-message-string err))
       nil))))

(defun shipit--add-comment-to-pr (pr-number file-path line-number body side &optional repo old-line-number)
  "Add a new comment to PR-NUMBER at LINE-NUMBER in FILE-PATH with BODY text.
SIDE should be \"RIGHT\" for added lines, \"LEFT\" for deleted lines,
or \"CONTEXT\" for unchanged lines.
If LINE-NUMBER is nil, creates a file-level comment (comment on the file itself).
REPO is optional; if not provided, uses the repo from `shipit--current-displayed-pr'
or falls back to the current git remote.
OLD-LINE-NUMBER is the line in the old version (for context/deleted lines on GitLab)."
  (let* ((repo (or repo
                   (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (unless repo
      (error "shipit: Repository must be configured"))
    (when repo
      (condition-case err
          (progn
            (shipit--debug-log "ADD-COMMENT: Creating inline comment - file:%s line:%s side:%s old-line:%s"
                               file-path line-number side old-line-number)
            (let ((response (funcall (plist-get backend :add-inline-comment)
                                     config pr-number file-path line-number body side old-line-number)))
              ;; Add the new comment to cached inline comments
              (when (and response (boundp 'shipit--cached-inline-comments))
                (if shipit--cached-inline-comments
                    (push response shipit--cached-inline-comments)
                  (setq shipit--cached-inline-comments (list response)))
                (shipit--debug-log "🔄 ADD-COMMENT: Added new comment to cache, total: %d"
                                 (length shipit--cached-inline-comments)))

              ;; Clear the per-file comment cache for this specific file
              (let ((file-cache-key (format "%s:%s:%s" repo pr-number file-path)))
                (when (boundp 'shipit--comment-cache)
                  (remhash file-cache-key shipit--comment-cache)))

              ;; Dispatch post-add operations to backend (cache invalidation, reaction pre-fetch)
              (when-let ((post-fn (plist-get backend :post-add-comment)))
                (funcall post-fn config (cdr (assq 'id response)) t repo pr-number))

              ;; Refresh ediff buffers if any are open
              (when (fboundp 'shipit--refresh-ediff-comments)
                (dolist (buffer (buffer-list))
                  (with-current-buffer buffer
                    (when (and (boundp 'shipit--ediff-pr-number) shipit--ediff-pr-number)
                      (shipit--refresh-ediff-comments)))))

              ;; Trigger targeted file section refresh in shipit buffer
              (if (and file-path (fboundp 'shipit--refresh-file-inline-comment-section))
                  (progn
                    (shipit--debug-log "🔄 ADD-COMMENT: Triggering targeted refresh for file %s" file-path)
                    (shipit--refresh-file-inline-comment-section file-path pr-number repo)))
              ;; Mark activities as read since user is actively engaging
              (setq shipit--user-mutated-pr t)
              (message "Comment added")
              t))
        (error
         (message "Failed to add comment: %s" err)
         nil)))))

(defun shipit--edit-comment (comment-id new-body &optional is-inline-comment is-review)
  "Edit comment with COMMENT-ID to have NEW-BODY text.
If IS-INLINE-COMMENT is non-nil, use pull request review comments endpoint.
If IS-REVIEW is non-nil, use pull request review endpoint.
Otherwise, use issues comments endpoint for general comments."
  ;; Offensive programming: validate inputs
  (unless (and comment-id (or (stringp comment-id) (numberp comment-id)))
    (error "shipit: comment-id must be a string or number, got %s: %S" (type-of comment-id) comment-id))
  (unless (and new-body (stringp new-body) (not (string-empty-p new-body)))
    (error "shipit: new-body must be a non-empty string, got %s: %S" (type-of new-body) new-body))

  (let* ((repo (or (bound-and-true-p shipit-buffer-repo)
                   (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         ;; Resolve PR number (needed by GitLab for all edits, GitHub for reviews)
         (pr-num (or (car shipit--current-displayed-pr)
                     (bound-and-true-p shipit-buffer-pr-number)
                     (bound-and-true-p shipit-current-pr)
                     (when (and (fboundp 'magit-get-current-branch) repo)
                       (let* ((branch (magit-get-current-branch))
                              (pr-data (when branch (shipit--get-current-pr-data branch repo))))
                         (when pr-data (cdr (assq 'number pr-data))))))))
    (unless repo
      (error "shipit: Repository must be configured"))

    (shipit--debug-log "🔄 EDIT-COMMENT: Starting edit operation - comment-id=%s is-inline=%s is-review=%s"
                       comment-id is-inline-comment is-review)

    (condition-case err
        (let ((response-data (funcall (plist-get backend :edit-comment)
                                      config comment-id new-body is-inline-comment
                                      (when is-review pr-num))))
          (if response-data
              (progn
                ;; Update edited comment body in caches (don't clear — targeted refresh reads from cache)
                (dolist (cache '(shipit--cached-general-comments
                                 shipit--cached-inline-comments))
                  (when (and (boundp cache) (symbol-value cache))
                    (dolist (c (symbol-value cache))
                      (when (equal (cdr (assq 'id c)) comment-id)
                        (setcdr (assq 'body c) new-body)))))
                ;; Dispatch post-edit operations to backend (cache invalidation)
                (when-let ((post-fn (plist-get backend :post-edit-comment)))
                  (let ((pr-number (or (car shipit--current-displayed-pr)
                                       (bound-and-true-p shipit-buffer-pr-number))))
                    (funcall post-fn config comment-id is-inline-comment repo pr-number)))

                ;; Update the comment body display immediately in the buffer
                (shipit--update-comment-body-display comment-id new-body repo)

                ;; Refresh ediff buffers if any are open
                (when (fboundp 'shipit--refresh-ediff-comments)
                  (dolist (buffer (buffer-list))
                    (with-current-buffer buffer
                      (when (and (boundp 'shipit--ediff-pr-number) shipit--ediff-pr-number)
                        (shipit--refresh-ediff-comments)))))

                ;; Mark activities as read since user is actively engaging
                (setq shipit--user-mutated-pr t)
                (message "Comment updated")
                t)
            (progn
              (message "Failed to edit comment - no response data")
              nil)))
      (error
       (message "Failed to edit comment %s: %s" comment-id (error-message-string err))
       nil))))

(defun shipit--edit-pr-description (pr-number new-body)
  "Edit PR description for PR-NUMBER to have NEW-BODY text."
  (let* ((repo (or (bound-and-true-p shipit-buffer-repo)
                   (cadr shipit--current-displayed-pr)
                   (shipit--get-repo-from-remote)))
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (when (and repo (shipit--github-token))
      (condition-case err
          (progn
            (funcall (plist-get backend :update-pr) config pr-number `((body . ,new-body)))
            (message "PR description updated successfully")
            ;; Clear cache to force refresh of PR data
            (shipit--clear-branch-cache)
            ;; Mark activities as read since user is actively engaging
            (setq shipit--user-mutated-pr t)
            t)
        (error
         (message "Failed to edit PR description for #%s: %s" pr-number err))))))

;; Functions merged from shipit-gh.el

(declare-function magit-get-current-branch "magit-git")  ;; optional; avoids byte-compile warnings

(defun shipit-pr (&optional branch-or-repo repo)
  "Return the current PR alist.

Usage:
  (shipit-pr)                 ; auto-detect BRANCH and REPO
  (shipit-pr REPO)            ; auto-detect BRANCH
  (shipit-pr BRANCH REPO)     ; explicit

REPO should be like \"owner/name\"."
  (let* ((branch (cond
                  ((and branch-or-repo repo) branch-or-repo)
                  (t (when (fboundp 'magit-get-current-branch)
                       (magit-get-current-branch)))))
         (repo   (cond
                  ((and branch-or-repo repo) repo)
                  ((and branch-or-repo (string-match-p "/" branch-or-repo)) branch-or-repo)
                  (t (shipit--get-repo-from-remote)))))
    (when (and branch repo)
      (shipit--get-current-pr-data branch repo))))

(defun shipit--get-pr-actual-state (pr-data)
  "Determine actual PR state from PR-DATA, distinguishing merged from closed."
  (let* ((raw-state (cdr (assq 'state pr-data)))
         (merged-at (cdr (assq 'merged_at pr-data))))
    (if (and (string= raw-state "closed") merged-at (not (eq merged-at :json-false)))
        "merged"
      raw-state)))

(defun shipit--get-comprehensive-pr-status (repo pr-number draft state)
  "Get comprehensive PR status combining state, draft, and merge readiness."
  (let* ((pr-data (shipit-get-pull-request pr-number repo))
         (mergeable-state (cdr (assq 'mergeable_state pr-data)))
         (mergeable (cdr (assq 'mergeable pr-data)))
         (is-draft (and draft (not (eq draft :json-false)) (not (null draft)))))

    (cond
     ;; Merged takes highest precedence
     ((string= state "merged")
      (propertize "🎉 merged" 'face '(:foreground "purple" :weight bold)))

     ;; Closed (without merge) - show if draft was closed
     ((string= state "closed")
      (if is-draft
          (propertize "❌ closed (draft)" 'face '(:foreground "red" :weight bold))
        (propertize "❌ closed" 'face '(:foreground "red" :weight bold))))

     ;; Draft (only if still open)
     (is-draft
      (propertize "🚧 draft" 'face '(:foreground "orange" :weight bold)))

     ;; Open - check merge readiness
     ((string= state "open")
      (cond
       ((string= mergeable-state "clean")
        (propertize "✅ ready to merge" 'face '(:foreground "green" :weight bold)))
       ((string= mergeable-state "blocked")
        (propertize "🚫 blocked" 'face '(:foreground "red" :weight bold)))
       ((or (string= mergeable-state "unstable") (string= mergeable-state "checking"))
        (propertize "⏳ checking" 'face '(:foreground "yellow" :weight bold)))
       (t
        (propertize "open" 'face '(:foreground "green" :weight bold)))))

     ;; Fallback
     (t (propertize state 'face '(:weight bold))))))

(defun shipit--colorize-pr-state (state)
  "Apply appropriate color and styling to PR STATE string."
  (cond
   ((string= state "open")
    (propertize state 'face '(:foreground "green" :weight bold)))
   ((string= state "closed")
    (propertize state 'face '(:foreground "red" :weight bold)))
   ((string= state "merged")
    (propertize state 'face '(:foreground "purple" :weight bold)))
   ((string= state "draft")
    (propertize state 'face '(:foreground "orange" :weight bold)))
   (t
    (propertize state 'face '(:weight bold)))))

(defun shipit--group-comments-by-thread (review-comments)
  "Group review comments by file and line to create threads."
  (let ((threads (make-hash-table :test 'equal)))
    (dolist (comment review-comments)
      (let* ((path (cdr (assq 'path comment)))
             (line (cdr (assq 'line comment)))
             (original-line (cdr (assq 'original_line comment)))
             (thread-key (format "%s:%s" path (or original-line line))))
        (push comment (gethash thread-key threads))))

    ;; Sort comments in each thread by creation date
    (maphash (lambda (key comments)
               (puthash key
                        (sort comments
                              (lambda (a b)
                                (string< (cdr (assq 'created_at a))
                                         (cdr (assq 'created_at b)))))
                        threads))
             threads)
    threads))

(defun shipit--extract-code-snippets (body)
  "Extract code snippets from comment BODY.
Handles multi-line code blocks by finding matching ``` delimiters."
  (let ((snippets '())
        (start 0))
    ;; Match opening ``` with optional language, then find closing ```
    (while (string-match "```\\([^\n]*\\)\n" body start)
      (let* ((lang (match-string 1 body))
             (match-start (match-beginning 0))
             (code-start (match-end 0))
             ;; Find closing ``` - check for empty block first (``` immediately
             ;; at code-start), then look for \n``` further in the body
             (empty-block-p (and (>= (length body) (+ code-start 3))
                                 (string= "```" (substring body code-start (+ code-start 3)))))
             (close-pos (if empty-block-p
                            code-start
                          (string-match "\n```" body code-start))))
        (if close-pos
            (let* ((code (substring body code-start close-pos))
                   (match-end (+ close-pos (if empty-block-p 3 4)))) ; 3 for ```, 4 for \n```
              (push (list :lang lang :code code :start match-start :end match-end) snippets)
              (setq start match-end))
          ;; No closing found, skip this opening
          (setq start code-start))))
    (nreverse snippets)))

(defun shipit--group-inline-comments-by-file (comments)
  "Group COMMENTS by file path, returning a hash table."
  (let ((grouped (make-hash-table :test 'equal)))
    (dolist (comment comments)
      (let ((file-path (or (cdr (assq 'path comment)) "Unknown file")))
        (push comment (gethash file-path grouped))))
    ;; Sort comments within each file by line number and creation time
    (maphash (lambda (file-path comments)
               (puthash file-path
                        (sort comments (lambda (a b)
                                         (let ((line-a (cdr (assq 'line a)))
                                               (line-b (cdr (assq 'line b)))
                                               (orig-a (cdr (assq 'original_line a)))
                                               (orig-b (cdr (assq 'original_line b))))
                                           ;; First sort by line number (or original_line for outdated)
                                           (let ((effective-a (or line-a orig-a 0))
                                                 (effective-b (or line-b orig-b 0)))
                                             (if (= effective-a effective-b)
                                                 ;; Same line, sort by creation time
                                                 (string< (cdr (assq 'created_at a))
                                                          (cdr (assq 'created_at b)))
                                               (< effective-a effective-b))))))
                        grouped))
             grouped)
    grouped))

(defun shipit--build-comment-properties (base-face help-text comment-keymap)
  "Build text properties for comment sections with optional mouse navigation.
BASE-FACE is the primary face, HELP-TEXT is the tooltip, COMMENT-KEYMAP is the keymap."
  (let ((valid-keymap (and (keymapp comment-keymap) comment-keymap)))
    (if shipit-enable-mouse-navigation
        (if base-face
            (if valid-keymap
                `(face ,base-face
                       
                       help-echo ,help-text
                       keymap ,comment-keymap)
              `(face ,base-face
                     
                     help-echo ,help-text))
          (if valid-keymap
              `(
                           help-echo ,help-text
                           keymap ,comment-keymap)
            `(
                         help-echo ,help-text)))
      (if base-face
          (if valid-keymap
              `(face ,base-face
                     help-echo ,help-text
                     keymap ,comment-keymap)
            `(face ,base-face
                   help-echo ,help-text))
        (if valid-keymap
            `(help-echo ,help-text
                        keymap ,comment-keymap)
          `(help-echo ,help-text))))))


(defface shipit-comment-face
  '((t :background "#f8f9fa" :foreground "black" :extend t)
    (((class color) (background dark)) :background "#2d3748" :foreground "white"))
  "Face for highlighting lines with GitHub comments."
  :group 'shipit)

(defface shipit-comment-indicator-face
  '((t :foreground "#856404" :weight bold))
  "Face for GitHub comment indicators."
  :group 'shipit)

(defface shipit-inline-comment-face
  '((t :foreground "#ff8c00" :extend t)
    (((class color) (background light)) :foreground "#d2691e")
    (((class color) (background dark)) :foreground "#ff8c00"))
  "Face for inline GitHub comment text with orange foreground."
  :group 'shipit)

;; Dynamic face functions for username and timestamp colors
(defun shipit--get-username-color ()
  "Get the foreground color for username based on shipit-inline-comment-username-color setting."
  (let ((color (or (and (boundp 'shipit-inline-comment-username-color) shipit-inline-comment-username-color) "blue")))
    (pcase color
      ("blue" (if (eq (frame-parameter nil 'background-mode) 'light) "#0366d6" "#58a6ff"))
      ("red" (if (eq (frame-parameter nil 'background-mode) 'light) "#cb2431" "#f85149"))
      ("green" (if (eq (frame-parameter nil 'background-mode) 'light) "#28a745" "#3fb950"))
      ("purple" (if (eq (frame-parameter nil 'background-mode) 'light) "#6f42c1" "#a371f7"))
      ("orange" (if (eq (frame-parameter nil 'background-mode) 'light) "#d2691e" "#ff8c00"))
      ("cyan" (if (eq (frame-parameter nil 'background-mode) 'light) "#0098cc" "#79c0ff"))
      (_ "#0366d6"))))

(defun shipit--get-timestamp-color ()
  "Get the foreground color for timestamp based on shipit-inline-comment-timestamp-color setting."
  (let ((color (or (and (boundp 'shipit-inline-comment-timestamp-color) shipit-inline-comment-timestamp-color) "gray")))
    (pcase color
      ("gray" (if (eq (frame-parameter nil 'background-mode) 'light) "#6a737d" "#8b949e"))
      ("blue" (if (eq (frame-parameter nil 'background-mode) 'light) "#0366d6" "#58a6ff"))
      ("red" (if (eq (frame-parameter nil 'background-mode) 'light) "#cb2431" "#f85149"))
      ("green" (if (eq (frame-parameter nil 'background-mode) 'light) "#28a745" "#3fb950"))
      ("purple" (if (eq (frame-parameter nil 'background-mode) 'light) "#6f42c1" "#a371f7"))
      ("orange" (if (eq (frame-parameter nil 'background-mode) 'light) "#d2691e" "#ff8c00"))
      (_ "#6a737d"))))

(defface shipit-inline-comment-username-face
  '((t :foreground "#0366d6" :weight bold))
  "Face for inline comment username highlighting (color is set dynamically)."
  :group 'shipit)

(defface shipit-inline-comment-timestamp-face
  '((t :foreground "#6a737d" :slant italic))
  "Face for inline comment timestamp highlighting (color is set dynamically)."
  :group 'shipit)

(defun shipit--update-comment-face-colors ()
  "Update inline comment face colors based on customization settings."
  (set-face-attribute 'shipit-inline-comment-username-face nil
                      :foreground (shipit--get-username-color)
                      :weight 'bold)
  (set-face-attribute 'shipit-inline-comment-timestamp-face nil
                      :foreground (shipit--get-timestamp-color)
                      :slant 'italic))

(defun shipit--update-general-face-colors ()
  "Update general username, timestamp, and filename face colors based on customization settings."
  (let ((username-color (and (boundp 'shipit-username-foreground) shipit-username-foreground))
        (timestamp-color (and (boundp 'shipit-timestamp-foreground) shipit-timestamp-foreground))
        (filename-color (and (boundp 'shipit-filename-foreground) shipit-filename-foreground)))
    (when username-color
      (set-face-attribute 'shipit-username-face nil
                          :foreground username-color
                          :weight 'bold))
    (when timestamp-color
      (set-face-attribute 'shipit-timestamp-face nil
                          :foreground timestamp-color
                          :slant 'italic))
    (when filename-color
      (set-face-attribute 'shipit-filename-face nil
                          :foreground filename-color))))

(defface shipit-inline-comment-body-face
  '((t :foreground "black")
    (((class color) (background light)) :foreground "black")
    (((class color) (background dark)) :foreground "white"))
  "Face for inline comment body text."
  :group 'shipit)

(defface shipit-comment-reactions-placeholder
  '((t :foreground "#999999"
       :weight normal))
  "Face for dimmed reactions placeholder smiley when no reactions exist."
  :group 'shipit)

(defface shipit-username-face
  '((t :foreground "#feacd0" :weight bold))
  "Face for username highlighting throughout shipit (color is customizable)."
  :group 'shipit)

(defface shipit-timestamp-face
  '((t :foreground "#00d3d0" :slant italic))
  "Face for timestamp highlighting throughout shipit (color is customizable)."
  :group 'shipit)

(defface shipit-filename-face
  '((t :foreground "#6ae4b9"))
  "Face for filename highlighting throughout shipit (color is customizable)."
  :group 'shipit)

(defun shipit--apply-comment-face-selectively (text comment-face)
  "Apply COMMENT-FACE to TEXT but skip portions with protected faces."
  (let ((pos 0)
        (len (length text)))
    (while (< pos len)
      (let* ((next-change (or (next-property-change pos text) len))
             (protected (get-text-property pos 'shipit-protected-face text)))
        (unless protected
          (add-face-text-property pos next-change comment-face 'append text))
        (setq pos next-change)))))

(defun shipit--wrap-comment-line (line width prefix)
  "Wrap a single LINE to WIDTH characters, adding PREFIX to continuation lines.
Returns a list of wrapped lines."
  (let ((words (split-string line))
        (wrapped '())
        (current-line "")
        (line-length 0))
    (dolist (word words)
      (let ((word-length (length word)))
        (if (= line-length 0)
            ;; First word on the line
            (progn
              (setq current-line word)
              (setq line-length word-length))
          ;; Check if adding this word would exceed width
          (if (<= (+ line-length 1 word-length) width)
              ;; Add word to current line
              (progn
                (setq current-line (concat current-line " " word))
                (setq line-length (+ line-length 1 word-length)))
            ;; Start new line with this word
            (progn
              (push current-line wrapped)
              (setq current-line word)
              (setq line-length word-length))))))
    ;; Add the last line if it has content
    (unless (string-empty-p current-line)
      (push current-line wrapped))
    ;; Add prefix to all wrapped lines (except first if no prefix needed initially)
    (if prefix
        (mapcar (lambda (wrapped-line) (concat prefix wrapped-line)) (reverse wrapped))
      (reverse wrapped))))

(defun shipit--wrap-comment-text (text width)
  "Wrap TEXT to WIDTH characters, preserving line breaks, blockquotes, and indentation.
GitHub blockquotes can span multiple lines: lines with > and continuation lines with spaces."
  (let ((lines (split-string text "\n"))
        (wrapped-lines '())
        (in-blockquote nil))
    (dolist (line lines)
      (cond
        ;; Preserve empty lines and reset blockquote state
        ((string-empty-p line)
         (push "" wrapped-lines)
         (setq in-blockquote nil))
        ;; Preserve code lines (line numbers followed by exactly 3 spaces)
        ((string-match "^[0-9]+   " line)
         (push line wrapped-lines))
        ;; Handle blockquotes (lines starting with >)
        ;; GitHub already formats blockquotes nicely, so preserve them as-is
        ((string-match "^>+" line)
         (push line wrapped-lines)
         (setq in-blockquote t))
        ;; Handle continuation lines of blockquotes (indented lines after blockquote)
        ;; These are part of GitHub's blockquote formatting, preserve as-is
        ((and in-blockquote (string-match "^[[:space:]]+" line))
         (push line wrapped-lines))
        ;; Wrap normal lines
        (t
         (dolist (wrapped-line (shipit--wrap-comment-line line width ""))
           (push wrapped-line wrapped-lines))
         (setq in-blockquote nil))))
    ;; Reverse to get correct order and join with newlines
    (mapconcat 'identity (reverse wrapped-lines) "\n")))

(defun shipit--remove-deleted-comment-from-caches (comment-id)
  "Remove COMMENT-ID from all relevant caches when it's detected as deleted."
  (let ((comment-id-num (if (stringp comment-id) (string-to-number comment-id) comment-id)))
    ;; Remove from comment type cache
    (when (boundp 'shipit--comment-type-cache)
      (remhash comment-id-num shipit--comment-type-cache))

    ;; Remove from general comments cache
    (when (boundp 'shipit--cached-general-comments)
      (setq shipit--cached-general-comments
            (cl-remove-if (lambda (comment)
                            (eq (cdr (assq 'id comment)) comment-id-num))
                          shipit--cached-general-comments)))

    ;; Remove from inline comments cache
    (when (boundp 'shipit--cached-inline-comments)
      (setq shipit--cached-inline-comments
            (cl-remove-if (lambda (comment)
                            (eq (cdr (assq 'id comment)) comment-id-num))
                          shipit--cached-inline-comments)))

    ;; Remove from per-file comment cache
    (when (boundp 'shipit--comment-cache)
      (maphash (lambda (key value)
                 (when (listp value)
                   (let ((filtered-comments (cl-remove-if (lambda (comment)
                                                            (eq (cdr (assq 'id comment)) comment-id-num))
                                                          value)))
                     (puthash key filtered-comments shipit--comment-cache))))
               shipit--comment-cache))

    (shipit--debug-log "Removed deleted comment %s from all caches" comment-id)))

(defun shipit--clean-comment-text (text)
  "Clean up TEXT by removing carriage returns and normalizing line endings.
Unicode handling is now done at the HTTP response level."
  (when text
    (shipit--clean-text text)))

(defun shipit--extract-quoted-text (comment-body)
  "Extract quoted text from COMMENT-BODY (lines starting with >)."
  (let ((quoted-lines '()))
    (dolist (line (split-string comment-body "\n"))
      (when (string-match "^>\\s-*\\(.+\\)" line)
        (push (match-string 1 line) quoted-lines)))
    (nreverse quoted-lines)))

(defun shipit--find-comment-containing-text (text comments)
  "Find the most recent comment in COMMENTS that contains TEXT.
Searches from newest to oldest to prefer more recent matches, which is
important when the quoted text appears as a substring in older comments."
  (catch 'found
    ;; Search in reverse (newest first) to find the most recent match
    (dolist (comment (reverse comments))
      (let* ((raw-body (cdr (assq 'body comment)))
             (body (shipit--clean-comment-text raw-body)))
        (when (and body (string-match-p (regexp-quote text) body))
          (throw 'found (cdr (assq 'id comment))))))
    nil))

(defun shipit--group-comments-by-api-replies (comments)
  "Group comments by GitHub API in_reply_to relationships, building proper nested thread chains with depth tracking.
When a comment contains quoted text, attempts to match the quote to a previous comment
and uses that as the parent instead of in_reply_to_id (which may be incorrect for quoted replies).
Optimized version with reduced debug logging for performance."
  (let ((threads (make-hash-table :test 'equal))
        (comment-to-depth (make-hash-table :test 'equal)) ; Maps comment-id to reply depth
        (comment-lookup (make-hash-table :test 'equal)) ; Maps comment-id to comment object
        (all-processed-comments '()) ; All comments processed so far (for quote matching)
        (root-comments '())
        (enable-per-comment-debug nil)) ; Disable expensive per-comment logging

    ;; Initialize root comments list
    (puthash 'root '() threads)

    ;; Create lookup table for quick comment access
    (dolist (comment comments)
      (let ((comment-id (cdr (assq 'id comment))))
        (puthash comment-id comment comment-lookup)))

    ;; Sort comments by creation time
    (setq comments (sort comments
                         (lambda (a b)
                           (string< (cdr (assq 'created_at a))
                                    (cdr (assq 'created_at b))))))

    ;; Process each comment using API reply relationships with depth tracking
    (dolist (comment comments)
      (let* ((comment-id (cdr (assq 'id comment)))
             (in-reply-to (cdr (assq 'in_reply_to_id comment))) ; GitHub API native field
             (parent-id-from-api (when in-reply-to (if (numberp in-reply-to) in-reply-to (string-to-number (format "%s" in-reply-to)))))
             (parent-id parent-id-from-api) ; Start with API parent, may be overridden by quote matching
             (parent-depth (when parent-id (gethash parent-id comment-to-depth)))
             (comment-depth (if parent-depth (1+ parent-depth) 0)))

        ;; Try to find parent by matching quoted text (overrides in_reply_to_id if found)
        (let ((raw-body (cdr (assq 'body comment)))
              (body (shipit--clean-comment-text (cdr (assq 'body comment)))))
          (when body
            (let ((quoted-lines (shipit--extract-quoted-text body)))
              (when quoted-lines
                (dolist (quoted-text quoted-lines)
                  (when (and (not parent-id) (> (length quoted-text) 2)) ; Only match if no API parent and quote > 2 chars
                    (let ((quote-based-parent-id (shipit--find-comment-containing-text quoted-text all-processed-comments)))
                      (when quote-based-parent-id
                        (when enable-per-comment-debug
                          (shipit--debug-log "Quote Threading: Comment %s matched quote '%s' to parent %s (no in_reply_to_id)"
                                             comment-id
                                             (substring quoted-text 0 (min 30 (length quoted-text)))
                                             quote-based-parent-id))
                        ;; Set parent-id from quote match
                        (setq parent-id quote-based-parent-id)
                        ;; Recalculate depth based on new parent
                        (setq parent-depth (gethash parent-id comment-to-depth))
                        (setq comment-depth (if parent-depth (1+ parent-depth) 0))))))
                ;; Also check if we should override API parent when quote points to different parent
                (dolist (quoted-text quoted-lines)
                  (when (and parent-id-from-api (> (length quoted-text) 2))
                    (let ((quote-based-parent-id (shipit--find-comment-containing-text quoted-text all-processed-comments)))
                      (when (and quote-based-parent-id
                                 (not (equal quote-based-parent-id parent-id-from-api)))
                        (when enable-per-comment-debug
                          (shipit--debug-log "Quote Threading: Comment %s matched quote '%s' to parent %s (overriding in_reply_to_id: %s)"
                                             comment-id
                                             (substring quoted-text 0 (min 30 (length quoted-text)))
                                             quote-based-parent-id
                                             parent-id-from-api))
                        ;; Override parent-id with the quote-matched parent
                        (setq parent-id quote-based-parent-id)
                        ;; Recalculate depth based on new parent
                        (setq parent-depth (gethash parent-id comment-to-depth))
                        (setq comment-depth (if parent-depth (1+ parent-depth) 0))))))))))

        ;; Store the comment with its depth
        (puthash comment-id comment-depth comment-to-depth)

        ;; Check for quoted replies to increase depth and establish proper parent relationships
        (let ((body (cdr (assq 'body comment)))
              (actual-depth comment-depth)
              (quote-parent-id nil))
          (when (and body (string-match "^>" body))
            ;; Count the number of > characters to determine quote depth
            (let* ((first-line (car (split-string body "\n")))
                   (quote-depth 0))
              ;; Count all > characters in the first line
              (let ((i 0))
                (while (< i (length first-line))
                  (when (eq (aref first-line i) ?>)
                    (setq quote-depth (1+ quote-depth)))
                  (setq i (1+ i))))
              ;; Disable expensive per-comment debug logging for performance
              ;; (shipit--debug-log "Quote depth analysis: comment %s, first-line: %s, quote-depth: %d"
              ;;                    comment-id first-line quote-depth)
              (when (> quote-depth 1)
                ;; This is a nested reply (> > means reply to reply)
                ;; Set depth based on quote marks: >> = depth 2, >>> = depth 3, etc.
                (setq actual-depth (max actual-depth quote-depth))
                (when enable-per-comment-debug
                  (shipit--debug-log "Quote-based depth: Comment %s depth %d -> %d (based on %d quote marks)"
                                     comment-id comment-depth actual-depth quote-depth)))))

          ;; Note: GitHub API parent relationships are preserved
          ;; We only override the depth for proper visual nesting

          ;; Update the depth and store in comment
          (puthash comment-id actual-depth comment-to-depth)
          (setcdr comment (cons `(reply-depth . ,actual-depth) (assq-delete-all 'reply-depth (cdr comment)))))

        ;; Debug: Log what we find for threading (use actual depth after quote processing)
        (when enable-per-comment-debug
          (let ((final-depth (gethash comment-id comment-to-depth)))
            (shipit--debug-log "Threading: Comment %s has in_reply_to_id: %s (final parent-id: %s) depth: %d"
                               comment-id in-reply-to parent-id final-depth)))

        (if (and parent-id (gethash parent-id comment-lookup))
            ;; This is a reply - add it as a direct child of its parent
            (progn
              ;; Add to parent's direct children (proper tree structure)
              (let ((current-replies (copy-sequence (gethash parent-id threads))))
                (puthash parent-id (append current-replies (list comment)) threads))
              (when enable-per-comment-debug
                (shipit--debug-log "Threading: Comment %s is replying to %s at depth %d" comment-id parent-id comment-depth)))
          ;; This is a root comment (no in_reply_to or parent not found)
          (progn
            (push comment root-comments)
            (when enable-per-comment-debug
              (shipit--debug-log "Threading: Comment %s is a root comment at depth %d" comment-id comment-depth))))

        ;; Add this comment to the processed list for future quote matching
        (push comment all-processed-comments)))

    ;; Store root comments in reverse chronological order (newest first)
    (puthash 'root (nreverse root-comments) threads)

    (shipit--debug-log "Threading complete: %d root comments, %d total threads (per-comment debug: %s)"
                       (length (gethash 'root threads))
                       (hash-table-count threads)
                       (if enable-per-comment-debug "enabled" "DISABLED for performance"))

    ;; Debug: Log the final threading structure
    (maphash (lambda (key value)
               (if (eq key 'root)
                   (shipit--debug-log "Root comments: %s"
                                      (mapcar (lambda (c) (cdr (assq 'id c))) value))
                 (shipit--debug-log "Thread %s has %d replies: %s"
                                    key (length value)
                                    (mapcar (lambda (c) (cdr (assq 'id c))) value))))
             threads)

    threads))

;; DEPRECATED: Keep the old quote-based function as fallback for compatibility
(defun shipit--group-comments-by-quotes (comments)
  "Group comments by quoted replies, building proper thread chains.
DEPRECATED: Use shipit--group-comments-by-api-replies for better accuracy with GitHub's native in_reply_to field."
  (let ((threads (make-hash-table :test 'equal))
        (comment-to-root (make-hash-table :test 'equal)) ; Maps comment-id to root-comment-id
        (comment-to-parent (make-hash-table :test 'equal)) ; Maps comment-id to direct parent-id
        (all-processed-comments '()) ; All comments processed so far
        (root-comments '()))

    ;; Initialize root comments list
    (puthash 'root '() threads)

    ;; Sort comments by creation time
    (setq comments (sort comments
                         (lambda (a b)
                           (string< (cdr (assq 'created_at a))
                                    (cdr (assq 'created_at b))))))

    ;; Process each comment
    (dolist (comment comments)
      (let* ((comment-id (cdr (assq 'id comment)))
             (raw-body (cdr (assq 'body comment)))
             (body (shipit--clean-comment-text raw-body))
             ;; First check for embedded reply-to marker (most reliable)
             (embedded-parent-id (shipit--extract-reply-to-id raw-body))
             (quoted-lines (shipit--extract-quoted-text body))
             (parent-id nil)
             (root-thread-id nil))

        ;; Use embedded parent ID if available (reliable), otherwise fall back to quote matching
        (if embedded-parent-id
            (progn
              (setq parent-id embedded-parent-id)
              (shipit--debug-log "Threading: Comment %s has embedded reply-to marker -> parent %s"
                                 comment-id parent-id))
          ;; Fall back to quote-based matching for comments without marker
          (when quoted-lines
            (shipit--debug-log "Quote Threading: Comment %s has %d quoted lines: %s"
                               comment-id (length quoted-lines)
                               (if quoted-lines (substring (mapconcat 'identity quoted-lines " | ") 0 (min 100 (length (mapconcat 'identity quoted-lines " | ")))) "none"))
            (dolist (quoted-text quoted-lines)
              (when (and (not parent-id) (> (length quoted-text) 2)) ; Match quotes longer than 2 chars to avoid single-char noise
                (shipit--debug-log "Quote Threading: Trying to match quote '%s' against %d previous comments"
                                   (substring quoted-text 0 (min 50 (length quoted-text)))
                                   (length all-processed-comments))
                (setq parent-id (shipit--find-comment-containing-text quoted-text all-processed-comments))
                (if parent-id
                    (shipit--debug-log "Quote Threading: Found match! Quote '%s' matches comment %s"
                                       (substring quoted-text 0 (min 30 (length quoted-text))) parent-id)
                  (shipit--debug-log "Quote Threading: No match found for quote '%s'"
                                     (substring quoted-text 0 (min 30 (length quoted-text)))))))))

        (if parent-id
            ;; This is a reply - find the root of the thread
            (progn
              ;; Record the direct parent relationship for depth calculation
              (puthash comment-id parent-id comment-to-parent)
              (setq root-thread-id (gethash parent-id comment-to-root))
              (unless root-thread-id
                ;; If parent is itself a root comment, use parent-id as root
                (setq root-thread-id parent-id))
              ;; Add this comment to its direct parent (not to root) for proper nesting
              (push comment (gethash parent-id threads))
              ;; Record that this comment belongs to the root thread
              (puthash comment-id root-thread-id comment-to-root))
          ;; This is a root comment
          (progn
            (push comment root-comments)
            ;; Record that this comment is its own root
            (puthash comment-id comment-id comment-to-root)))

        ;; Add to processed comments list
        (push comment all-processed-comments)))

    ;; Store root comments
    (puthash 'root (nreverse root-comments) threads)
    ;; Reverse all thread lists to maintain chronological order
    (maphash (lambda (key value)
               (unless (eq key 'root)
                 (puthash key (nreverse value) threads)))
             threads)

    ;; Calculate reply depths based on parent relationships established during threading
    (let ((comment-to-depth (make-hash-table :test 'equal)))

      ;; First pass: set root comments to depth 0
      (dolist (root-comment (gethash 'root threads))
        (let ((root-id (cdr (assq 'id root-comment))))
          (puthash root-id 0 comment-to-depth)
          ;; Remove existing reply-depth and set to 0
          (setcdr root-comment (cons `(reply-depth . 0) (assq-delete-all 'reply-depth (cdr root-comment))))))

      ;; Second pass: calculate depths and update all comment instances
      (dolist (comment comments)
        (let* ((comment-id (cdr (assq 'id comment)))
               (parent-id (gethash comment-id comment-to-parent)))
          (when parent-id ; This comment has a parent (is a reply)
            ;; Recursively calculate depth
            (let ((depth (shipit--calculate-comment-depth comment-id comment-to-parent comment-to-depth)))
              (puthash comment-id depth comment-to-depth)
              ;; Replace existing reply-depth property
              (setcdr comment (cons `(reply-depth . ,depth) (assq-delete-all 'reply-depth (cdr comment))))
              (shipit--debug-log "Quote depth: Comment %s set to depth %d (parent %s)"
                                 comment-id depth parent-id)))))

      ;; Third pass: ensure all comment instances in threads hash table have correct depth
      (maphash (lambda (thread-id thread-comments)
                 (unless (eq thread-id 'root)
                   (dolist (thread-comment thread-comments)
                     (let* ((thread-comment-id (cdr (assq 'id thread-comment)))
                            (calculated-depth (gethash thread-comment-id comment-to-depth)))
                       (when calculated-depth
                         ;; Replace existing reply-depth property
                         (setcdr thread-comment (cons `(reply-depth . ,calculated-depth) (assq-delete-all 'reply-depth (cdr thread-comment))))
                         (shipit--debug-log "Updated thread comment %s to depth %d" thread-comment-id calculated-depth))))))
               threads))

    ;; Debug: Log quote-based threading results
    (shipit--debug-log "Quote Threading complete: %d root comments, %d total threads"
                       (length (gethash 'root threads))
                       (hash-table-count threads))
    (maphash (lambda (key value)
               (if (eq key 'root)
                   (shipit--debug-log "Quote Root comments: %s"
                                      (mapcar (lambda (c) (cdr (assq 'id c))) value))
                 (shipit--debug-log "Quote Thread %s has %d replies: %s"
                                    key (length value)
                                    (mapcar (lambda (c) (cdr (assq 'id c))) value))))
             threads)

    threads))

(defun shipit--calculate-comment-depth (comment-id comment-to-parent comment-to-depth)
  "Recursively calculate the depth of COMMENT-ID based on its parent chain."
  (let ((parent-id (gethash comment-id comment-to-parent)))
    (if parent-id
        (let ((parent-depth (or (gethash parent-id comment-to-depth)
                                (shipit--calculate-comment-depth parent-id comment-to-parent comment-to-depth))))
          (1+ parent-depth))
      0))) ; If no parent, it's a root comment at depth 0

(defun shipit--indent-comment-body (body)
  "Indent BODY text to align with the user name."
  (let ((indent-string "   "))  ; 3 spaces to align with the emoji and space
    (mapconcat (lambda (line)
                 (if (string-empty-p line)
                     ""
                   (concat indent-string line)))
               (split-string body "\n")
               "\n")))

(defun shipit--get-comment-reactions (comment-id &optional is-inline)
  "Get reactions for COMMENT-ID from GitHub API.
If IS-INLINE is non-nil, uses pull request review comment endpoint,
otherwise uses issue comment endpoint."
  (unless (shipit--ensure-repository)
    (error "No repository set"))

  ;; Use cached reactions only - reactions should already be fetched synchronously during refresh
  (let* ((cache-key (shipit--reaction-cache-key shipit-current-repo comment-id is-inline))
         (cached-result (gethash cache-key shipit--reaction-cache)))
    (shipit--debug-log 'reactions "GET-COMMENT-REACTIONS: comment %s (%s): cache-key='%s', shipit-current-repo='%s', cached-result=%s"
                       comment-id (if is-inline "inline" "general")
                       cache-key
                       shipit-current-repo
                       (if cached-result (format "found %d reactions" (length cached-result)) "NOT FOUND"))
    (when cached-result
      (shipit--debug-log 'reactions "GET-COMMENT-REACTIONS: Returning %d reactions: %S" (length cached-result) cached-result))
    cached-result))

(defun shipit--format-comment-reactions (comment &optional is-inline)
  "Format reactions for COMMENT as a string with emoji, counts, and hover tooltips.
If IS-INLINE is non-nil, treats comment as an inline comment."
  (let* ((comment-id (cdr (assq 'id comment)))
         (reactions (shipit--get-comment-reactions comment-id is-inline))
         (reaction-groups (make-hash-table :test 'equal)))

    (shipit--debug-log 'reactions "🎭 REACTIONS DEBUG: comment-id=%s, is-inline=%s, reactions=%s"
                       comment-id is-inline
                       (if reactions (format "found %d" (length reactions)) "nil"))

    ;; Group reactions by type and collect user info
    ;; Convert vector to list if needed (GitHub API may return [] for empty reactions)
    (when (and reactions (vectorp reactions))
      (setq reactions (append reactions nil)))
    (when (and reactions (listp reactions) (> (length reactions) 0))
      (shipit--debug-log "FORMAT: Grouping %d reactions for comment %s" (length reactions) comment-id)
      (dolist (reaction reactions)
        (let* ((content (cdr (assq 'content reaction)))
               (user (cdr (assq 'login (cdr (assq 'user reaction)))))
               (existing (gethash content reaction-groups)))
          (shipit--debug-log "🎭 FORMAT: Processing reaction type='%s' user='%s'" content user)
          (if existing
              (puthash content (cons user existing) reaction-groups)
            (puthash content (list user) reaction-groups)))))

    ;; Format as emoji with counts and hover tooltips
    (shipit--debug-log "🎭 FORMAT: Reaction groups hash has %d entries" (hash-table-count reaction-groups))
    ;; Always include placeholder icon, then append any reactions
    (let ((placeholder (shipit--get-reactions-placeholder-icon))
          (formatted-reactions '()))
      (maphash (lambda (reaction-type users)
                 (let ((emoji (shipit--reaction-to-emoji reaction-type))
                       (count (length users))
                       (user-list (mapconcat 'identity (reverse users) ", ")))
                   (shipit--debug-log "🎭 FORMAT: reaction-type='%s' emoji='%s' count=%d users=%s"
                                      reaction-type emoji count user-list)
                   (when emoji
                     (let ((reaction-text (format "%s %d" emoji count)))
                       (shipit--debug-log "🎭 FORMAT: Created reaction-text='%s'" reaction-text)
                       ;; Force emojify processing on the emoji character
                       (when (and (fboundp 'emojify-string) (string-match-p "[^\x00-\x7F]" emoji))
                         (setq reaction-text (replace-regexp-in-string
                                              (regexp-quote emoji)
                                              (emojify-string emoji)
                                              reaction-text)))
                       (let ((tooltip (format "%s: %s" reaction-type user-list)))
                         (push (propertize reaction-text
                                           'help-echo tooltip
                                           'shipit-reaction-tooltip tooltip)
                               formatted-reactions))))))
               reaction-groups)
      ;; Combine placeholder + reactions with space separator if reactions exist
      (if (> (length formatted-reactions) 0)
          (concat placeholder " " (mapconcat 'identity formatted-reactions " "))
        (progn
          (shipit--debug-log "🎭 FORMAT: No reactions, returning placeholder only")
          (concat placeholder " "))))))

(defun shipit--update-comment-reactions-display (comment-id is-inline repo)
  "Update the reactions display for COMMENT-ID in the shipit buffer.
Finds reaction line, updates it using the unified update function."
  (shipit--debug-log "🔄 UPDATE: Starting for comment %s in repo %s" comment-id repo)
  ;; Find the shipit buffer - case-insensitive match for repo name
  (let ((buf (cl-find-if (lambda (b)
                           (and (string-match-p "^\\*shipit: " (buffer-name b))
                                (let ((case-fold-search t))
                                  (string-match-p (regexp-quote repo) (buffer-name b)))))
                         (buffer-list))))
    (when buf
      (with-current-buffer buf
        ;; Set shipit-current-repo to buffer's repo for cache lookups
        (let ((inhibit-read-only t)
              (shipit-current-repo (or (when (boundp 'shipit-buffer-repo) shipit-buffer-repo) repo)))
          (save-excursion
            (goto-char (point-min))
            ;; Find the reactions line with matching comment-id
            (while (and (< (point) (point-max))
                        (not (and (get-text-property (point) 'shipit-reactions)
                                  (equal (get-text-property (point) 'shipit-comment-id) comment-id))))
              (goto-char (or (next-single-property-change (point) 'shipit-reactions)
                             (point-max))))

            (when (and (< (point) (point-max))
                       (get-text-property (point) 'shipit-reactions)
                       (equal (get-text-property (point) 'shipit-comment-id) comment-id))
              (let* ((old-text (buffer-substring (line-beginning-position) (line-end-position)))
                     (indent (or (when (string-match "^\\([ \t]*\\)" old-text)
                                   (match-string 1 old-text))
                                 "   "))
                     (magit-section-obj (get-text-property (point) 'magit-section))
                     (comment-alist `((id . ,comment-id)))
                     (formatted-reactions (shipit--format-comment-reactions comment-alist is-inline)))
                (shipit--debug-log "🔄 UPDATE: Found reactions, indent=%d" (length indent))
                (shipit--update-reactions-line
                 formatted-reactions indent
                 `(shipit-comment t
                   shipit-comment-id ,comment-id
                   ,@(when magit-section-obj
                       `(magit-section ,magit-section-obj))))
                (shipit--debug-log "✅ UPDATE: Complete")))))))))

(defun shipit--update-comment-body-display (comment-id new-body repo)
  "Update the comment body display for COMMENT-ID in the shipit buffer with NEW-BODY.
Uses the same targeted update approach as reactions - find, replace, let Emacs adjust markers."
  (shipit--debug-log "🔄 UPDATE-BODY: Starting for comment %s" comment-id)
  ;; Find the shipit buffer
  (let ((buf (cl-find-if (lambda (b)
                           (and (string-match-p "^\\*shipit: " (buffer-name b))
                                (string-match-p (regexp-quote repo) (buffer-name b))))
                         (buffer-list))))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            ;; Find the comment body TEXT (not header) region with matching comment-id
            (while (and (< (point) (point-max))
                        (not (and (get-text-property (point) 'shipit-comment-body-text)
                                  (equal (get-text-property (point) 'shipit-comment-id) comment-id))))
              (goto-char (or (next-single-property-change (point) 'shipit-comment-body-text)
                             (point-max))))

            (when (and (< (point) (point-max))
                       (get-text-property (point) 'shipit-comment-body-text)
                       (equal (get-text-property (point) 'shipit-comment-id) comment-id))
              (let* ((body-start (point))
                     (body-end (or (next-single-property-change (point) 'shipit-comment-body-text)
                                   (point-max)))
                     (old-text (buffer-substring body-start body-end))
                     ;; Extract indent from the old text
                     (indent (save-excursion
                               (goto-char body-start)
                               (beginning-of-line)
                               (skip-chars-forward " \t")
                               (buffer-substring (line-beginning-position) (point))))
                     (indent-length (length indent))
                     (magit-section-obj (get-text-property body-start 'magit-section))
                     ;; Determine if this is a file comment (has file-path property)
                     (is-file-comment (get-text-property body-start 'shipit-file-path))
                     ;; Clean carriage returns from new body
                     (cleaned-body (shipit--clean-text new-body))
                     ;; Format new body with proper indentation
                     ;; File comments use 63 chars (72 - 9 spaces), general comments use configurable width (default 80)
                     (wrap-width (if is-file-comment 63 (or (and (boundp 'shipit-comment-wrap-width) shipit-comment-wrap-width) 80)))
                     (formatted-body (if (fboundp 'shipit--wrap-text)
                                         (shipit--wrap-text cleaned-body wrap-width)
                                       cleaned-body))
                     ;; Add indent AFTER newlines (not before), matching original rendering logic
                     (indented-body (replace-regexp-in-string "\n" (concat "\n" indent) formatted-body)))

                (shipit--debug-log "🔄 UPDATE-BODY: Found body at %d-%d, old length=%d"
                                   body-start body-end (- body-end body-start))
                (shipit--debug-log "🔄 UPDATE-BODY: Old text: %S" old-text)
                (shipit--debug-log "🔄 UPDATE-BODY: Extracted indent: %S (length=%d)" indent indent-length)
                (shipit--debug-log "🔄 UPDATE-BODY: Is file comment: %s, wrap-width: %d" is-file-comment wrap-width)
                (shipit--debug-log "🔄 UPDATE-BODY: New indented body: %S" indented-body)

                ;; Delete old body and insert new (with trailing newline to match original format)
                (delete-region body-start body-end)
                (goto-char body-start)
                ;; Insert with leading indent, matching original rendering
                (insert indent indented-body "\n")

                ;; Re-apply properties
                (add-text-properties body-start (point)
                                     `(shipit-comment t
                                       shipit-comment-id ,comment-id
                                       shipit-comment-body ,new-body
                                       shipit-comment-body-text t
                                       ,@(when magit-section-obj
                                           `(magit-section ,magit-section-obj))))

                (shipit--debug-log "✅ UPDATE-BODY: Complete")))))))))

(defun shipit--insert-new-comment-in-buffer (comment-data repo &optional is-inline)
  "Insert a new comment into the shipit buffer at the end of the general-comments section.
COMMENT-DATA is the comment object returned from the API."
  (shipit--debug-log "🆕 INSERT: Starting for comment %s" (cdr (assq 'id comment-data)))
  (let ((buf (cl-find-if (lambda (b)
                           (and (string-match-p "^\\*shipit: " (buffer-name b))
                                (string-match-p (regexp-quote repo) (buffer-name b))))
                         (buffer-list))))
    (if buf
        (progn
          (shipit--debug-log "🆕 INSERT: Found buffer %s" (buffer-name buf))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (point-min))
                (shipit--debug-log "🆕 INSERT: Searching for general-comments section from point %d to %d" (point-min) (point-max))
                ;; Find general-comments section by magit section type
                (let ((section nil))
                  (while (and (not section) (< (point) (point-max)))
                    (let ((sec (get-text-property (point) 'magit-section)))
                      (when (and sec (eq (oref sec type) 'general-comments))
                        (setq section sec)))
                    (goto-char (or (next-single-property-change (point) 'magit-section nil (point-max))
                                   (point-max))))

                  (if section
                      (progn
                        ;; Go to section end minus 1 line (before closing marker)
                        (goto-char (oref section end))
                        (forward-line -1)
                        ;; Insert the new comment using the same function as general comment rendering
                        (when (fboundp 'shipit--insert-status-hierarchical-comment)
                          (let ((pr-number (get-text-property (point-min) 'shipit-pr-number)))
                            (shipit--insert-status-hierarchical-comment comment-data nil nil nil 0 nil nil repo pr-number))))
                    (shipit--debug-log "⚠️ INSERT: Section not found - falling back to refresh")))))))
      (shipit--debug-log "⚠️ INSERT: Buffer not found for repo %s" repo))))

(defun shipit--remove-comment-from-buffer (comment-id repo &optional is-inline)
  "Remove comment with COMMENT-ID from the shipit buffer.
REPO is the repository name, IS-INLINE indicates if it's an inline comment."
  (shipit--debug-log "🗑️ REMOVE: Starting for comment %s (inline: %s)" comment-id is-inline)
  (let ((buf (cl-find-if (lambda (b)
                           (and (string-match-p "^\\*shipit: " (buffer-name b))
                                (string-match-p (regexp-quote repo) (buffer-name b))))
                         (buffer-list))))
    (if buf
        (progn
          (shipit--debug-log "🗑️ REMOVE: Found buffer %s" (buffer-name buf))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (point-min))
                ;; Find the magit section containing this comment-id
                (let ((found nil))
                  (while (and (not found) (< (point) (point-max)))
                    (let ((sec (get-text-property (point) 'magit-section)))
                      (when (and sec
                                 (eq (oref sec type) 'general-comment)
                                 (equal (oref sec value) comment-id))
                        (setq found t)
                        (let ((section-start (oref sec start))
                              (section-end (oref sec end)))
                          (shipit--debug-log "🗑️ REMOVE: Found magit section for comment %s from %s to %s"
                                             comment-id section-start section-end)
                          (delete-region section-start section-end)
                          (shipit--debug-log "✅ REMOVE: Comment section removed"))))
                    (goto-char (or (next-single-property-change (point) 'magit-section nil (point-max))
                                   (point-max))))
                  (unless found
                    (shipit--debug-log "⚠️ REMOVE: Comment section not found in buffer")))))))
      (shipit--debug-log "⚠️ REMOVE: Buffer not found for repo %s" repo))))

(defun shipit--get-current-file-line ()
  "Get the current file path and line number from a magit diff buffer.
Returns (file-path line-number diff_hunk side) where side is 'LEFT' or 'RIGHT'."
  (save-excursion
    (let ((current-pos (point))
          file-path line-number target-pos)

      ;; If we're on an inserted comment line, find the actual diff line it belongs to
      (if (get-text-property (point) 'shipit-comment)
          (progn
            ;; Move backward to find the actual diff line this comment is attached to
            (while (and (> (point) (point-min))
                        (get-text-property (point) 'shipit-comment))
              (forward-line -1))
            ;; Now we should be on the actual diff line
            (setq target-pos (point)))
        (setq target-pos current-pos))

      ;; Find the file header
      (when (re-search-backward "^\\(?:diff --git a/\\([^[:space:]]+\\) b/\\([^[:space:]]+\\)\\|\\(?:modified\\|new file\\|deleted\\)[[:space:]]+\\(.+\\)$\\)" nil t)
        (setq file-path (or (match-string 2) (match-string 1) (match-string 3)))
        ;; Find the hunk header that contains our target position
        (goto-char target-pos)
        (when (re-search-backward "^@@.*@@" nil t)
          ;; Check if this hunk contains our target position
          (let* ((hunk-header-start (point))
                 (hunk-header (match-string 0))
                 (hunk-content-start (match-end 0))
                 (hunk-end (save-excursion
                             (forward-line 1)
                             (if (re-search-forward "^\\(?:@@\\|diff\\)" nil t)
                                 (match-beginning 0)
                               (point-max))))
                 (new-start (when (string-match "@@[[:space:]]*-[0-9]+\\(?:,[0-9]+\\)?[[:space:]]*\\+\\([0-9]+\\)" hunk-header)
                              (string-to-number (match-string 1 hunk-header))))
                 (current-new-line (or new-start 1)))
            (message "DEBUG: Hunk header: %s, new-start: %s" hunk-header new-start)
            (message "DEBUG: Hunk bounds: header=%s content=%s end=%s target=%s"
                     hunk-header-start hunk-content-start hunk-end target-pos)

            ;; Only proceed if target is actually within this hunk
            (when (and (> target-pos hunk-content-start) (< target-pos hunk-end))
              ;; Use line content matching instead of position-based counting
              (let ((hunk-start-pos hunk-content-start)  ; Use the correct hunk position!
                    (target-line-content nil)
                    (lines-counted 0)
                    (found-match nil))

                ;; Get the content of the line we're on (without the +/- prefix)
                (save-excursion
                  (goto-char target-pos)
                  (beginning-of-line)
                  (when (looking-at "^\\([-+ ]\\)\\(.*\\)$")
                    (setq target-line-content (match-string 2))))

                ;; Count from hunk start until we reach target position
                (goto-char hunk-content-start)
                (forward-line 1) ; Move to first line after hunk header

                (while (and (< (point) target-pos)
                            (not (looking-at "^\\(?:@@\\|diff\\)")) ; Stop at next hunk/diff
                            (not (eobp)))
                  (when (looking-at "^\\([-+ ]\\)")
                    (let ((line-type (match-string 1)))
                      ;; Count lines that contribute to new file
                      (when (or (string= line-type " ") (string= line-type "+"))
                        (setq lines-counted (1+ lines-counted)))))
                  (forward-line 1))

                ;; The final line number is hunk start + lines counted (without the target line)
                (setq current-new-line (+ new-start lines-counted))

                (setq line-number current-new-line))))))

      ;; Extract diff hunk and determine side if we have the other info
      (when (and file-path line-number)
        (let (diff-hunk side)
          (save-excursion
            (goto-char target-pos)
            ;; Determine which side we're on by looking at the target diff line
            (when (looking-at "^\\([-+ ]\\)")
              (let ((line-type (match-string 1)))
                (setq side (cond
                            ((string= line-type "+") "RIGHT")  ; added line
                            ((string= line-type "-") "LEFT")   ; deleted line
                            (t "RIGHT")))))                    ; context line, default to RIGHT
            ;; Extract the diff hunk
            (when (re-search-backward "^@@.*@@" nil t)
              (let ((hunk-start (point)))
                ;; Find end of hunk (next @@ or diff or end of buffer)
                (let ((hunk-end (save-excursion
                                  (if (re-search-forward "^\\(?:@@\\|diff\\)" nil t)
                                      (match-beginning 0)
                                    (point-max)))))
                  (setq diff-hunk (buffer-substring-no-properties hunk-start hunk-end))))))
          (list file-path line-number diff-hunk side))))))

(defun shipit--get-comment-at-point ()
  "Get comment ID and body for the comment at point.
Returns (comment-id . comment-body) or nil if not found."
  (let (comment-id comment-body)
    ;; First, try to get properties at current point
    (setq comment-id (get-text-property (point) 'shipit-comment-id))
    (setq comment-body (get-text-property (point) 'shipit-comment-body))

    ;; If not found at current point, search within the current comment block
    (when (and (not comment-id) (get-text-property (point) 'shipit-comment))
      (save-excursion
        ;; Find the boundaries of the current comment block
        (let ((start-pos (point))
              comment-start comment-end)
          ;; Find start of comment block
          (while (and (not (bobp)) (get-text-property (1- (point)) 'shipit-comment))
            (goto-char (1- (point))))
          (setq comment-start (point))

          ;; Find end of comment block
          (goto-char start-pos)
          (while (and (not (eobp)) (get-text-property (point) 'shipit-comment))
            (forward-char 1))
          (setq comment-end (point))

          ;; Now search backward from cursor position to find the comment we're actually in
          (goto-char start-pos)
          (while (and (>= (point) comment-start) (not comment-id))
            (setq comment-id (get-text-property (point) 'shipit-comment-id))
            (setq comment-body (get-text-property (point) 'shipit-comment-body))
            (unless comment-id
              (backward-char 1))))))

    (when (and comment-id comment-body)
      (cons comment-id comment-body))))

(defun shipit--format-quoted-reply (original-comment &optional parent-comment-id)
  "Format ORIGINAL-COMMENT as a quoted reply for GitHub.
If PARENT-COMMENT-ID is provided, embeds a hidden reference for reliable threading."
  (let* ((lines (split-string original-comment "\n"))
         (quoted-lines (mapcar (lambda (line) (concat "> " line)) lines))
         (quoted-text (string-join quoted-lines "\n"))
         ;; Add hidden parent reference for reliable threading
         (parent-ref (if parent-comment-id
                         (format "<!-- shipit-reply-to:%s -->\n" parent-comment-id)
                       "")))
    (concat parent-ref quoted-text "\n\n")))

(defun shipit--extract-reply-to-id (body)
  "Extract the parent comment ID from BODY if it contains a shipit-reply-to marker.
Returns the parent comment ID as a number, or nil if not found."
  (when (and body (string-match "<!-- shipit-reply-to:\\([0-9]+\\) -->" body))
    (string-to-number (match-string 1 body))))

(defun shipit--clear-inserted-comments ()
  "Clear all inserted GitHub comments from current buffer.
This function has been disabled to prevent buffer corruption -
comments are now cleared automatically during magit refresh."
  (shipit--debug-log "shipit--clear-inserted-comments disabled - manual buffer manipulation causes corruption")
  ;; Instead of dangerous manual buffer manipulation,
  ;; comments will be cleared automatically during magit refresh
  nil)

(defun shipit--insert-comments-for-file (comments file-path file-start-pos)
  "Insert all COMMENTS for FILE-PATH starting from FILE-START-POS directly into buffer."
  (save-excursion
    (goto-char file-start-pos)
    ;; Find the hunk header for this file
    (when (re-search-forward "^@@.*@@" nil t)
      (let* ((hunk-header (match-string 0))
             (old-start (when (string-match "@@[[:space:]]*-\\([0-9]+\\)" hunk-header)
                          (string-to-number (match-string 1 hunk-header))))
             (new-start (when (string-match "@@[[:space:]]*-[0-9]+\\(?:,[0-9]+\\)?[[:space:]]*\\+\\([0-9]+\\)" hunk-header)
                          (string-to-number (match-string 1 hunk-header))))
             (current-old-line (or old-start 1))
             (current-new-line (or new-start 1))
             (comment-positions (make-hash-table :test 'equal))
             (placed-comments (make-hash-table :test 'equal)))

        ;; Create a hash table of target lines to comments
        (dolist (comment comments)
          (let* ((line-number (cdr (assq 'line comment)))
                 (original-line (cdr (assq 'original_line comment)))
                 (target-line (or line-number original-line)))
            (when target-line
              (push comment (gethash target-line comment-positions)))))

        ;; Walk through the diff lines and insert comments
        (let ((line-counter 0)
              (inhibit-read-only t))
          (catch 'done-with-hunk
            (while (re-search-forward "^\\([-+ ]\\)" nil t)
              (let ((line-type (match-string 1))
                    (line-end (line-end-position)))
                ;; Check if we've gone past the current hunk
                (when (>= (point) (save-excursion
                                    (or (re-search-forward "^\\(?:diff\\|@@\\|modified\\|new file\\|deleted\\)" nil t)
                                        (point-max))))
                  (throw 'done-with-hunk nil))
                (setq line-counter (1+ line-counter))
                (cond
                 ((string= line-type " ")
                  ;; Context line - check both old and new line numbers, but avoid duplicates
                  (let ((old-comments (gethash current-old-line comment-positions))
                        (new-comments (gethash current-new-line comment-positions))
                        (comments-to-insert '()))
                    ;; Collect all unique comments for this line
                    (dolist (comment (append old-comments new-comments))
                      (let ((comment-id (cdr (assq 'id comment))))
                        (unless (gethash comment-id placed-comments)
                          (push comment comments-to-insert))))
                    ;; Insert comments if we have any
                    (when comments-to-insert
                      (let* ((sorted-comments (sort (reverse comments-to-insert)
                                                    (lambda (a b)
                                                      (string< (cdr (assq 'created_at a))
                                                               (cdr (assq 'created_at b))))))
                             (active-comments (shipit--filter-comments-preserve-mixed-threads sorted-comments)))
                        (when active-comments
                          (shipit--insert-inline-comment-thread active-comments line-end)))
                      (dolist (comment comments-to-insert)
                        (puthash (cdr (assq 'id comment)) t placed-comments))))
                  (setq current-old-line (1+ current-old-line))
                  (setq current-new-line (1+ current-new-line)))
                 ((string= line-type "-")
                  ;; Deleted line - check old line number
                  (let ((comments-here (gethash current-old-line comment-positions))
                        (comments-to-insert '()))
                    ;; Filter out already placed comments
                    (dolist (comment comments-here)
                      (let ((comment-id (cdr (assq 'id comment))))
                        (unless (gethash comment-id placed-comments)
                          (push comment comments-to-insert))))
                    ;; Insert comments if we have any
                    (when comments-to-insert
                      (let* ((sorted-comments (sort (reverse comments-to-insert)
                                                    (lambda (a b)
                                                      (string< (cdr (assq 'created_at a))
                                                               (cdr (assq 'created_at b))))))
                             (active-comments (shipit--filter-comments-preserve-mixed-threads sorted-comments)))
                        (when active-comments
                          (shipit--insert-inline-comment-thread active-comments line-end)))
                      (dolist (comment comments-to-insert)
                        (puthash (cdr (assq 'id comment)) t placed-comments))))
                  (setq current-old-line (1+ current-old-line)))
                 ((string= line-type "+")
                  ;; Added line - check new line number
                  (let ((comments-here (gethash current-new-line comment-positions))
                        (comments-to-insert '()))
                    ;; Filter out already placed comments
                    (dolist (comment comments-here)
                      (let ((comment-id (cdr (assq 'id comment))))
                        (unless (gethash comment-id placed-comments)
                          (push comment comments-to-insert))))
                    ;; Insert comments if we have any
                    (when comments-to-insert
                      (let* ((sorted-comments (sort (reverse comments-to-insert)
                                                    (lambda (a b)
                                                      (string< (cdr (assq 'created_at a))
                                                               (cdr (assq 'created_at b))))))
                             (active-comments (shipit--filter-comments-preserve-mixed-threads sorted-comments)))
                        (when active-comments
                          (shipit--insert-inline-comment-thread active-comments line-end)))
                      (dolist (comment comments-to-insert)
                        (puthash (cdr (assq 'id comment)) t placed-comments))))
                  (setq current-new-line (1+ current-new-line))))))))))))

(defun shipit--handle-boundary-comments (comment-positions placed-comments current-new-line)
  "Handle comments that are just outside the visible diff hunk."
  (let ((boundary-comments '())
        (inhibit-read-only t))
    ;; Check lines 1-3 after the hunk ends
    (dolist (offset '(1 2 3))
      (let ((target-line (+ current-new-line offset)))
        (let ((line-comments (gethash target-line comment-positions)))
          (when line-comments
            (dolist (comment line-comments)
              (let ((comment-id (cdr (assq 'id comment))))
                (unless (gethash comment-id placed-comments)
                  (push (cons target-line comment) boundary-comments))))))))

    (when boundary-comments
      (let ((grouped (make-hash-table :test 'equal)))
        (dolist (line-comment boundary-comments)
          (let ((line (car line-comment))
                (comment (cdr line-comment)))
            (push comment (gethash line grouped))))

        (goto-char (point-max))
        (maphash (lambda (line-num comments)
                   (let* ((sorted-comments (sort comments
                                                 (lambda (a b)
                                                   (string< (cdr (assq 'created_at a))
                                                            (cdr (assq 'created_at b))))))
                          (active-comments (shipit--filter-active-comments sorted-comments)))
                     (when active-comments
                       (insert (propertize (format "\n--- Comments on line %d (outside visible diff) ---\n" line-num)
                                           'face 'shipit-inline-comment-face
                                           'shipit-comment t))
                       (dolist (comment active-comments)
                         (let ((comment-text (shipit--format-comment-as-text comment t)))
                           (insert comment-text))
                         (let ((comment-id (cdr (assq 'id comment))))
                           (puthash comment-id t placed-comments))))))
                 grouped)))))

(defun shipit--insert-diff-threaded-comment (comment threads depth)
  "Insert a COMMENT with hierarchical threading at DEPTH level using THREADS hash table."
  (let* ((comment-id (cdr (assq 'id comment)))
         (review-id (cdr (assq 'pull_request_review_id comment)))
         (user-obj (cdr (assq 'user comment)))
         (user (cdr (assq 'login user-obj)))
         (avatar-url (cdr (assq 'avatar_url user-obj)))
         (created (cdr (assq 'created_at comment)))
         (formatted-timestamp (shipit--format-timestamp created))
         (body (cdr (assq 'body comment)))
         ;; Use the stored reply-depth from threading analysis (includes quote-based detection)
         (actual-depth (or (cdr (assq 'reply-depth comment)) depth))
         ;; Create proper indentation: root comments have no tree indicator, replies at 3 spaces with └─
         (thread-prefix (if (> actual-depth 0)
                            (make-string (+ 3 (* (1- actual-depth) 6)) ?\s)
                          ""))
         (tree-indicator (if (> actual-depth 0) "└─ " "")))

    (shipit--debug-log "DIFF-COMMENT: id=%s review-id=%s user=%s" comment-id review-id user)
    (shipit--debug-log "DIFF THREADING: Inserting comment %s at depth %d (actual: %d) (user: %s)"
                       comment-id depth actual-depth user)

    ;; Insert the comment with proper indentation and enhanced features
    (magit-insert-section (shipit-comment comment-id)
      (let* ((icon (shipit--get-comment-icon comment 'inline-diff))
             ;; Check outdated/resolved status
             (is-outdated (cdr (assq 'outdated comment)))
             (is-resolved (or (cdr (assq 'resolved comment))
                              (and comment-id (shipit--is-comment-in-resolved-thread comment-id))))
             ;; Create heading with all features
             (heading-text (format "%s%s%s%s (%s)%s%s"
                                   thread-prefix tree-indicator icon
                                   (or user "Unknown")
                                   (or formatted-timestamp "")
                                   (if is-outdated " [OUTDATED]" "")
                                   (if is-resolved " [RESOLVED]" "")))
             (heading-start (point)))
        ;; Add blank line before comment for spacing
        (insert "\n")
        (magit-insert-heading (propertize heading-text 'face 'bold
                                         'shipit-comment t
                                         'shipit-comment-id comment-id
                                         'shipit-review-id (cdr (assq 'pull_request_review_id comment))))
        ;; Apply individual faces to components like file comments
        (when (and (boundp 'shipit-inline-comment-faces) shipit-inline-comment-faces)
          (let ((user-pos (string-match (regexp-quote user) heading-text))
                (paren-start-pos (string-match "(" heading-text))
                (paren-end-pos (string-match ")" heading-text))
                ;; Adjust heading-start by 1 to account for newline inserted above
                (adjusted-heading-start (1+ heading-start)))
            ;; Apply username face if found
            (when user-pos
              (put-text-property (+ adjusted-heading-start user-pos)
                               (+ adjusted-heading-start user-pos (length user))
                               'face 'shipit-inline-comment-username-face))
            ;; Apply timestamp face - between opening and closing paren
            (when (and paren-start-pos paren-end-pos)
              (put-text-property (+ adjusted-heading-start paren-start-pos 1)
                               (+ adjusted-heading-start paren-end-pos)
                               'face 'shipit-inline-comment-timestamp-face)))))
      (progn
        ;; Use common rendering function - pass 0 for indent-level so we apply our own indentation
        (let* ((rendered-body (shipit--render-comment-body comment 0))
               ;; For replies, indent body to align with username in the heading
               ;; Heading format: thread-prefix + tree-indicator + icon + user
               ;; tree-indicator "└─ " is 3 chars, emoji icon "💬 " is wide character
               ;; Emojis display wider than ASCII, accounting for rendering width ~2-3 cols
               ;; So need thread-prefix + 6 spaces to properly align with username start
               (body-indent (concat thread-prefix (if (> actual-depth 0) "      " "   ")))
               (body-with-prefix (replace-regexp-in-string "\n" (concat "\n" body-indent) rendered-body))
               (body-start (point))
               (body-text (concat body-indent body-with-prefix "\n")))
          ;; Insert rendered body with thread-prefix prepended to each line
          (insert (propertize body-text
                              'shipit-comment t
                              'shipit-comment-id comment-id
                              'shipit-review-id (cdr (assq 'pull_request_review_id comment))
                              'shipit-comment-body (shipit--clean-text (or body ""))
                              'shipit-file-path (cdr (assq 'path comment))
                              'shipit-line-number (cdr (assq 'line comment))
                              'shipit-repo (shipit--get-repo-from-remote)
                              'shipit-pr-number (when (and shipit--current-displayed-pr (listp shipit--current-displayed-pr))
                                                  (car shipit--current-displayed-pr))))
          ;; Apply blockquote faces to preserve markdown styling
          (shipit--apply-blockquote-faces body-start (point))
          ;; Apply strikethrough faces for ~text~ patterns
          (shipit--apply-strikethrough-faces body-start (point)))
        ;; Add reactions if present (with blank line separator)
        (let ((reactions (shipit--format-comment-reactions comment t)))
          (when reactions
            (insert "\n")  ;; Add blank line separator before reactions
            (insert (propertize (concat thread-prefix "    " (shipit--clean-text reactions) "\n")
                                'face '(:foreground "#666666")
                                ;; CRITICAL: Add same properties as comment body for DWIM handler
                                'shipit-reactions t
                                'shipit-comment t
                                'shipit-comment-id comment-id
                                'shipit-comment-body (shipit--clean-text (or body ""))
                                'shipit-file-path (cdr (assq 'path comment))
                                'shipit-line-number (cdr (assq 'line comment))
                                'shipit-repo (shipit--get-repo-from-remote)
                                'shipit-pr-number (when (and shipit--current-displayed-pr (listp shipit--current-displayed-pr))
                                                    (car shipit--current-displayed-pr))))))))

    ;; Recursively insert replies
    (let ((replies (gethash comment-id threads)))
      (when replies
        (shipit--debug-log "DIFF THREADING: Found %d replies to comment %s"
                           (length replies) comment-id)
        (dolist (reply replies)
          (shipit--insert-diff-threaded-comment reply threads (1+ actual-depth)))))))

(defun shipit--insert-inline-comment-thread (comments line-end)
  "Insert a thread of COMMENTS after LINE-END position."
  (shipit--debug-log "shipit--insert-inline-comment-thread: Inserting %d comments at line-end %d in %s mode"
                     (length comments) line-end major-mode)
  (shipit--debug-log "shipit--insert-inline-comment-thread: line-end %d corresponds to line number %d"
                     line-end (line-number-at-pos line-end))
  (shipit--debug-log "shipit--insert-inline-comment-thread: use-magit-sections=%s" shipit-use-magit-sections-for-diff-comments)
  (let ((inhibit-read-only t))
    (goto-char line-end)
    (forward-line)

    ;; Check if comments are already inserted at this location
    (if (get-text-property (point) 'shipit-comment)
        (progn
          (shipit--debug-log "shipit--insert-inline-comment-thread: Comments already exist at line-end %d, skipping" line-end)
          (shipit--debug-log "shipit--insert-inline-comment-thread: Text at position: '%s'"
                             (buffer-substring (max (point-min) (- (point) 50))
                                               (min (point-max) (+ (point) 50)))))
      ;; Choose insertion method based on configuration
      (if (and shipit-use-magit-sections-for-diff-comments (fboundp 'magit-insert-section))
          (progn
            (shipit--debug-log "Using magit-sections for diff comments")
            ;; Check parent section for hierarchical integration
            (let ((parent-section (magit-current-section)))
              (shipit--debug-log "Parent section type: %s" (when parent-section (oref parent-section type)))
              (if (and parent-section (eq (oref parent-section type) 'hunk))
                  (progn
                    (shipit--debug-log "HIERARCHICAL: Attempting true parent-child integration")
                    ;; Try to insert within the hunk section's context
                    (let* ((hunk-section parent-section)
                           (comment-section nil))
                      ;; Create the comment section
                      (setq comment-section
                            (magit-insert-section (hunk-comments line-end)
                              (magit-insert-heading (format "💭 Comments (%d)" (length comments)))
                              (progn
                                ;; Insert comments with hierarchical threading
                                (let ((threads (shipit--group-comments-by-api-replies comments)))
                                  (shipit--debug-log "DIFF BUFFER THREADING: Found %d threads for %d comments"
                                                     (hash-table-count threads) (length comments))
                                  (dolist (comment comments)
                                    (let ((reply-to (cdr (assq 'in_reply_to_id comment))))
                                      (unless reply-to ; Only process root comments - replies handled recursively
                                        (shipit--debug-log "DIFF BUFFER: Processing root comment %s" (cdr (assq 'id comment)))
                                        (shipit--insert-diff-threaded-comment comment threads 0))))))))
                      ;; Attempt to manually establish parent-child relationship
                      (when (and comment-section hunk-section)
                        (shipit--debug-log "HIERARCHICAL: Manually setting parent-child relationship")
                        (condition-case err
                            (progn
                              ;; Remove comment section from current parent's children
                              (when (oref comment-section parent)
                                (oset (oref comment-section parent) children
                                      (delq comment-section (oref (oref comment-section parent) children))))
                              ;; Set hunk as parent
                              (oset comment-section parent hunk-section)
                              ;; Add to hunk's children
                              (oset hunk-section children
                                    (nconc (oref hunk-section children) (list comment-section)))
                              (shipit--debug-log "HIERARCHICAL: Successfully established parent-child relationship")
                              ;; Try to force magit to recognize the new hierarchy by refreshing section state
                              (when (fboundp 'magit-section-update-highlight)
                                (magit-section-update-highlight)))
                          (error
                           (shipit--debug-log "HIERARCHICAL: Failed to set parent-child relationship: %s" err))))))
                (progn
                  (shipit--debug-log "STANDALONE: Inserting comments as standalone section")
                  ;; Insert as standalone section (not inside hunk)
                  (magit-insert-section (hunk-comments line-end)
                    (magit-insert-heading (format "💭 Comments (%d)" (length comments)))
                    (progn
                      ;; Insert comments with hierarchical threading
                      (let ((threads (shipit--group-comments-by-api-replies comments)))
                        (shipit--debug-log "STANDALONE THREADING: Found %d threads for %d comments"
                                           (hash-table-count threads) (length comments))
                        (dolist (comment comments)
                          (let ((reply-to (cdr (assq 'in_reply_to_id comment))))
                            (unless reply-to ; Only process root comments - replies handled recursively
                              (shipit--debug-log "STANDALONE: Processing root comment %s" (cdr (assq 'id comment)))
                              (shipit--insert-diff-threaded-comment comment threads 0))))))))))
            ;; Mark as having comments to prevent duplicates
            (put-text-property (point-min) (point-max) 'shipit-comment t))
        (progn
          (shipit--debug-log "Using text-based comment insertion")
          ;; Insert padding before comment section
          (insert (propertize "\n"
                              'face 'shipit-inline-comment-face
                              'shipit-comment t))

          ;; Insert each comment (comments should already be in chronological order)
          (shipit--debug-log "shipit--insert-inline-comment-thread: About to insert %d comments at buffer position %d"
                             (length comments) (point))
          ;; Insert each comment using same approach as magit-diff buffers
          (dolist (comment comments)
            (let ((comment-text (shipit--format-comment-as-text comment t)))
              ;; Add keymap property to comment text to override TAB behavior
              (put-text-property 0 (length comment-text) 'keymap
                                 (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "TAB") 'ignore)
                                   map) comment-text)
              (shipit--debug-log "shipit--insert-inline-comment-thread: Inserting comment: %s"
                                 (substring comment-text 0 (min 50 (length comment-text))))
              (insert comment-text)))

          ;; Insert padding after comment section
          (insert (propertize "\n"
                              'face 'shipit-inline-comment-face
                              'shipit-comment t))))
      (shipit--debug-log "shipit--insert-inline-comment-thread: Finished inserting comments")
      (shipit--debug-log "shipit--insert-inline-comment-thread: Buffer content around insertion point: '%s'"
                         (buffer-substring (max (point-min) (- line-end 100))
                                           (min (point-max) (+ (point) 200))))
      ))) ; End function

(defun shipit--process-file-hunks (file-path comments start-pos)
  "Process all hunks for FILE-PATH and insert filtered comments for each hunk."
  (save-excursion
    (goto-char start-pos)
    (let ((hunk-count 0))
      ;; Find all hunk headers for this file until we hit the next file or end of buffer
      (while (and (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
                  ;; Stop if we encounter another file diff
                  (not (save-excursion
                         (beginning-of-line)
                         (or (looking-at "^diff --git")
                             (looking-at "^\\(?:modified\\|new file\\|deleted\\)")))))
        (setq hunk-count (1+ hunk-count))
        (let* ((old-start (string-to-number (match-string 1)))
               (old-count (string-to-number (match-string 2)))
               (new-start (string-to-number (match-string 3)))
               (new-count (string-to-number (match-string 4)))
               (hunk-pos (line-beginning-position))
               (hunk-info (list :old-start old-start
                                :old-count old-count
                                :new-start new-start
                                :new-count new-count
                                :old-end (+ old-start old-count -1)
                                :new-end (+ new-start new-count -1)
                                :hunk-pos hunk-pos)))
          (let ((filtered-comments (shipit--filter-comments-for-hunk comments hunk-info)))
            (when filtered-comments
              (save-excursion
                (goto-char hunk-pos)
                (shipit--insert-comments-for-file filtered-comments file-path hunk-pos)))))))))

(defun shipit--filter-comments-for-hunk (comments hunk-info)
  "Filter COMMENTS to only include those within HUNK-INFO range."
  (when hunk-info
    (let ((new-start (plist-get hunk-info :new-start))
          (new-end (plist-get hunk-info :new-end)))
      (cl-remove-if-not
       (lambda (comment)
         (let* ((line-number (cdr (assq 'line comment)))
                (is-outdated (null line-number)))
           ;; Only include current (non-outdated) comments within hunk range
           (and line-number
                (not is-outdated)
                (>= line-number new-start)
                (<= line-number new-end))))
       comments))))

(defun shipit--format-pr-reactions (pr-number)
  "Format reactions for PR with PR-NUMBER as a string with emoji, counts, and hover tooltips."
  (let* ((reactions (shipit--get-pr-reactions pr-number))
         (reaction-groups (make-hash-table :test 'equal)))

    ;; Group reactions by type and collect user info
    (when reactions
      (dolist (reaction reactions)
        (let* ((content (cdr (assq 'content reaction)))
               (user (cdr (assq 'login (cdr (assq 'user reaction)))))
               (existing (gethash content reaction-groups)))
          (if existing
              (puthash content (cons user existing) reaction-groups)
            (puthash content (list user) reaction-groups)))))

    ;; Format as emoji with counts and hover tooltips
    ;; Always include placeholder icon, then append any reactions
    (let ((placeholder (shipit--get-reactions-placeholder-icon))
          (formatted-reactions '()))
      (maphash (lambda (reaction-type users)
                 (let ((emoji (shipit--reaction-to-emoji reaction-type))
                       (count (length users))
                       (user-list (mapconcat 'identity (reverse users) ", ")))
                   (when emoji
                     (let ((reaction-text (format "%s %d" emoji count)))
                       (let ((tooltip (format "%s: %s" reaction-type user-list)))
                         (push (propertize reaction-text
                                           'help-echo tooltip
                                           'shipit-reaction-tooltip tooltip)
                               formatted-reactions))))))
               reaction-groups)
      ;; Combine placeholder + reactions with space separator if reactions exist
      ;; NOTE: No leading indent - caller handles indentation
      (if (> (length formatted-reactions) 0)
          (concat placeholder " " (mapconcat 'identity formatted-reactions " "))
        (concat placeholder " ")))))

(defun shipit--get-pr-reactions (pr-number)
  "Get reactions for PR or issue with PR-NUMBER.
Checks the shared reaction cache first (works for any backend),
then falls back to async PR fetch if needed (requires API token)."
  (let* ((cache-key (format "pr-%s" pr-number))
         (not-found (make-symbol "not-found"))
         (cached-reactions (gethash cache-key shipit--reaction-cache not-found)))
    ;; Return cached reactions if found (works for both PR and issue backends)
    (if (not (eq cached-reactions not-found))
        cached-reactions
      ;; Not cached — try async fetch via PR backend (requires GitHub infrastructure)
      (when (and (shipit--ensure-repository) (shipit--github-token) shipit-current-repo)
        (let ((fetch-in-progress (gethash cache-key shipit--reaction-fetch-in-progress)))
          (cond
           (fetch-in-progress nil)
           ((plist-get (car (shipit-pr--resolve-for-repo shipit-current-repo))
                       :fetch-reactions)
            (shipit--fetch-pr-reactions-async pr-number)
            nil)
           (t nil)))))))

(defun shipit--fetch-pr-reactions-async (pr-number)
  "Fetch reactions for PR with PR-NUMBER asynchronously and cache the result."
  (let* ((endpoint (format "/repos/%s/issues/%s/reactions" shipit-current-repo pr-number))
         (cache-key (format "pr-%s" pr-number)))
    ;; Mark as fetch in progress
    (puthash cache-key t shipit--reaction-fetch-in-progress)
    (shipit--api-request
     endpoint
     nil
     (cl-function
      (lambda (reactions)
        ;; Cache the result and clear fetch-in-progress flag
        (puthash cache-key reactions shipit--reaction-cache)
        (remhash cache-key shipit--reaction-fetch-in-progress)
        ;; Reactions will appear on the next natural refresh (no forced refresh)
        )))))

(defun shipit--fetch-pr-reactions-sync (repo pr-number)
  "Fetch reactions for PR with PR-NUMBER synchronously and cache the result.
REPO is the repository name (owner/repo format).
Dispatches to the active PR backend's :fetch-reactions.
Returns the fetched reactions list."
  (when (and repo pr-number)
    (let* ((cache-key (format "pr-%s" pr-number))
           (resolved (shipit-pr--resolve-for-repo repo))
           (backend (car resolved))
           (config (cdr resolved))
           (fetch-fn (plist-get backend :fetch-reactions))
           (reactions (when fetch-fn (funcall fetch-fn config pr-number))))
      (shipit--debug-log "Fetched %d PR reactions for PR #%s"
                         (if reactions (length reactions) 0) pr-number)
      ;; Cache the result
      (puthash cache-key (or reactions '()) shipit--reaction-cache)
      (or reactions '()))))

(defun shipit--user-has-reaction (comment-id reaction-type is-inline)
  "Check if the current user has already reacted with REACTION-TYPE to comment COMMENT-ID."
  (let* ((cache-key (shipit--reaction-cache-key shipit-current-repo comment-id is-inline))
         (reactions (gethash cache-key shipit--reaction-cache))
         (current-user (shipit--get-current-user)))
    (shipit--debug-log "USER-HAS-REACTION DEBUG: comment-id=%s reaction-type=%s is-inline=%s"
                       comment-id reaction-type is-inline)
    (shipit--debug-log "USER-HAS-REACTION DEBUG: cache-key=%s" cache-key)
    (shipit--debug-log "USER-HAS-REACTION DEBUG: reactions-count=%s current-user=%s"
                       (if reactions (length reactions) 0) current-user)
    (when reactions
      (shipit--debug-log "USER-HAS-REACTION DEBUG: sample reaction users=%s"
                         (mapcar (lambda (r) (cdr (assq 'login (cdr (assq 'user r)))))
                                (seq-take reactions 3))))
    (shipit--debug-log "USER-HAS-REACTION DEBUG: Looking for reaction-type=%s by user=%s" reaction-type current-user)
    (if (and reactions current-user)
        (progn
          (shipit--debug-log "USER-HAS-REACTION DEBUG: Starting reaction comparison loop through %d reactions" (length reactions))
          (cl-some (lambda (reaction)
                     (let* ((reaction-content (cdr (assq 'content reaction)))
                            (reaction-user (cdr (assq 'login (cdr (assq 'user reaction)))))
                            (content-match (string= reaction-content reaction-type))
                            (user-match (string= reaction-user current-user)))
                       (shipit--debug-log "USER-HAS-REACTION DEBUG: checking reaction content='%s' user='%s' content-match=%s user-match=%s"
                                          reaction-content reaction-user content-match user-match)
                       (when (and content-match user-match)
                         (shipit--debug-log "USER-HAS-REACTION DEBUG: ✅ FOUND MATCHING REACTION! User %s has %s reaction" current-user reaction-type))
                       (and content-match user-match)))
                   reactions))
      (progn
        (shipit--debug-log "USER-HAS-REACTION DEBUG: ❌ Cannot check reactions - reactions=%s current-user=%s"
                           (if reactions "present" "nil") (if current-user current-user "nil"))
        nil))))

(defun shipit--user-has-pr-reaction (pr-number reaction-type)
  "Check if the current user has already reacted with REACTION-TYPE to PR PR-NUMBER."
  (let* ((cache-key (format "pr-%s" pr-number))
         (reactions (gethash cache-key shipit--reaction-cache))
         (current-user (shipit--get-current-user)))
    (when (and reactions current-user)
      (cl-some (lambda (reaction)
                 (and (string= (cdr (assq 'content reaction)) reaction-type)
                      (string= (cdr (assq 'login (cdr (assq 'user reaction)))) current-user)))
               reactions))))

(defun shipit-get-pr-comments-for-file (pr-number file-path)
  "Get inline comments for FILE-PATH in pull request PR-NUMBER with caching.
Dispatches fetch to the active comment backend's :fetch-inline-comments."
  (let* ((cache-key (format "%s:%s" pr-number file-path))
         (repo (shipit--get-repo-from-remote))
         (cached-comments (gethash cache-key shipit--comment-cache)))

    ;; Return cached comments if available
    (if cached-comments
        cached-comments
      ;; Check buffer-local inline cache before calling backend API
      (let ((file-comments
             (when shipit--cached-inline-comments
               (seq-filter (lambda (comment)
                             (string= (cdr (assq 'path comment)) file-path))
                           shipit--cached-inline-comments))))
        (if file-comments
            (progn
              (dolist (comment file-comments)
                (let ((comment-id (cdr (assq 'id comment))))
                  (when comment-id
                    (puthash comment-id t shipit--comment-type-cache))))
              (puthash cache-key file-comments shipit--comment-cache)
              file-comments)
          ;; Fetch via backend and filter by file
          (when (and repo (shipit--github-token))
            (let* ((resolved (shipit-comment--resolve-for-repo repo))
                   (backend (car resolved))
                   (config (cdr resolved))
                   (comments (condition-case nil
                                 (funcall (plist-get backend :fetch-inline-comments) config pr-number)
                               (error nil)))
                   (fetched-file-comments
                    (when comments
                      (seq-filter (lambda (comment)
                                    (string= (cdr (assq 'path comment)) file-path))
                                  comments))))
              ;; Cache inline comment type info for each comment
              (when fetched-file-comments
                (dolist (comment fetched-file-comments)
                  (let ((comment-id (cdr (assq 'id comment))))
                    (when comment-id
                      (puthash comment-id t shipit--comment-type-cache)))))
              (puthash cache-key fetched-file-comments shipit--comment-cache)
              fetched-file-comments)))))))

(defun shipit--get-assignees (repo)
  "Get list of possible assignees for REPO.
Dispatches to the active PR backend's :fetch-available-assignees."
  ;; Offensive programming - validate inputs
  (unless repo
    (error "repo parameter is required but was nil"))
  (unless (stringp repo)
    (error "repo must be a string, got: %S" repo))
  (condition-case err
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (fetch-fn (plist-get backend :fetch-available-assignees)))
        (if fetch-fn
            (funcall fetch-fn config)
          '()))
    (error
     (message "Failed to fetch assignees: %s" err)
     nil)))

(defun shipit--get-user-teams-in-org (username org-name)
  "Get list of teams that USERNAME belongs to in ORG-NAME organization.
Results are cached in-memory to avoid redundant API calls for the same user/org."
  (when (and username org-name (shipit--github-token))
    (let ((cache-key (format "%s:%s" org-name username)))
      ;; Check in-memory cache first
      (if-let ((cached (gethash cache-key shipit--team-membership-cache)))
          (progn
            (shipit--debug-log "⚡ TEAM-MEMBERSHIP: Cache HIT for %s in %s" username org-name)
            (cdr (assq 'teams cached)))
        ;; Cache miss - fetch from API
        (shipit--debug-log "⚡ TEAM-MEMBERSHIP: Cache MISS for %s in %s - fetching" username org-name)
        (condition-case err
            (let* ((endpoint (format "/user/teams"))
                   (teams-result (shipit-gh-etag-get-json-with-refresh-cache endpoint '((per_page . 100)) shipit-github-token))
                   (teams-data (when teams-result (plist-get teams-result :json)))
                   (org-teams '()))

              ;; Filter to only teams in the target organization
              ;; Also include parent teams (for nested team hierarchies)
              (dolist (team teams-data)
                (let ((org-login (cdr (assq 'login (cdr (assq 'organization team))))))
                  (when (and org-login (string= org-login org-name))
                    (let ((team-slug (cdr (assq 'slug team)))
                          (parent (cdr (assq 'parent team))))
                      ;; Add the team itself
                      (push team-slug org-teams)
                      ;; Also add parent team if it exists (nested team membership)
                      ;; Being a member of a child team implicitly means membership in parent
                      (when parent
                        (let ((parent-slug (cdr (assq 'slug parent))))
                          (when parent-slug
                            (shipit--debug-log "⚡ TEAM-MEMBERSHIP: %s is child of %s" team-slug parent-slug)
                            (unless (member parent-slug org-teams)
                              (push parent-slug org-teams)))))))))

              ;; Cache result and return
              (puthash cache-key (list (cons 'teams org-teams) (cons 'timestamp (float-time)))
                       shipit--team-membership-cache)
              (shipit--debug-log "⚡ TEAM-MEMBERSHIP: Cached %d teams for %s in %s: %S" (length org-teams) username org-name org-teams)
              org-teams)
          (error
           (shipit--debug-log "❌ TEAM-MEMBERSHIP: Error getting teams for %s: %s" username (error-message-string err))
           '()))))))

(defun shipit--check-if-reviewer-satisfies-teams (reviewer-login required-teams org-name)
  "Check if REVIEWER-LOGIN is a member of any of the REQUIRED-TEAMS in ORG-NAME.
Returns list of teams that the reviewer satisfies, or nil if none."
  (shipit--debug-log "Checking if %s satisfies any of: %S" reviewer-login required-teams)

  (when (and reviewer-login required-teams org-name)
    (let* ((user-teams (shipit--get-user-teams-in-org reviewer-login org-name))
           (satisfied-teams '()))
      (dolist (required-team required-teams)
        ;; Clean up team format (@org/team or @team -> team)
        (let ((clean-team (replace-regexp-in-string "^@\\(?:[^/]+/\\)?" "" required-team)))
          (when (member clean-team user-teams)
            (push required-team satisfied-teams)
            (shipit--debug-log "✅ %s is member of team: %s" reviewer-login required-team))))
      satisfied-teams)))

(defun shipit--resolve-team-requirements-with-reviewers (blocking-owners approved-reviewers org-name)
  "Resolve which BLOCKING-OWNERS (CODEOWNERS teams) are satisfied by APPROVED-REVIEWERS.
Returns alist with resolved teams removed and remaining teams.
PERFORMANCE OPTIMIZED: Fetches team memberships once per reviewer, not per team."
  (shipit--debug-log "Resolving team requirements: %d teams, %d reviewers"
                     (length blocking-owners) (length approved-reviewers))

  (let ((remaining-teams (copy-sequence blocking-owners))
        (satisfied-teams '())
        (reviewer-teams-cache (make-hash-table :test 'equal)))

    ;; OPTIMIZATION: Fetch all reviewer team memberships once upfront
    (dolist (reviewer approved-reviewers)
      (let ((reviewer-login (if (stringp reviewer) reviewer (cdr (assq 'login reviewer)))))
        (when reviewer-login
          (puthash reviewer-login
                   (shipit--get-user-teams-in-org reviewer-login org-name)
                   reviewer-teams-cache))))

    ;; Now check teams efficiently using cached data
    (dolist (reviewer approved-reviewers)
      (let* ((reviewer-login (if (stringp reviewer) reviewer (cdr (assq 'login reviewer))))
             (reviewer-teams (gethash reviewer-login reviewer-teams-cache))
             (reviewer-satisfied-teams '()))

        ;; Check which required teams this reviewer satisfies
        (dolist (required-team remaining-teams)
          (let ((clean-team (replace-regexp-in-string "^@\\(?:[^/]+/\\)?" "" required-team)))
            (when (member clean-team reviewer-teams)
              (push required-team reviewer-satisfied-teams)
              (shipit--debug-log "✅ %s satisfies team: %s" reviewer-login required-team))))

        (when reviewer-satisfied-teams
          (setq satisfied-teams (append satisfied-teams reviewer-satisfied-teams))
          ;; Remove satisfied teams from remaining
          (dolist (satisfied-team reviewer-satisfied-teams)
            (setq remaining-teams (remove satisfied-team remaining-teams))))))

    (shipit--debug-log "Team resolution complete: %d satisfied, %d remaining"
                     (length satisfied-teams) (length remaining-teams))

    `((satisfied-teams . ,satisfied-teams)
      (remaining-teams . ,remaining-teams))))

;;;
;;; Timeline/Activity Functions
;;;

(defun shipit--fetch-timeline-events (repo pr-number &optional force-fresh)
  "Fetch timeline events for PR-NUMBER in REPO using ETag caching.
If FORCE-FRESH is non-nil, bypass cache and fetch fresh data.
Returns a list of timeline events including reviews, comments, labels, etc.
Fetches multiple pages to ensure recent events are included for large PRs."
  (shipit--debug-log "Fetching timeline events for PR %s in %s (force-fresh: %s)"
                     pr-number repo (if force-fresh "YES" "NO"))
  (let* ((endpoint (format "/repos/%s/issues/%s/timeline" repo pr-number))
         (per-page 100)
         (max-pages 10)  ; Limit to avoid excessive API calls
         (page 1)
         (all-events '())
         (continue t))
    ;; Fetch multiple pages to get all events (timeline returns oldest first)
    (while (and continue (<= page max-pages))
      (let* ((params `((per_page . ,per-page) (page . ,page)))
             (etag-result (condition-case err
                             (shipit-gh-etag-get-json-with-refresh-cache endpoint params shipit-github-token force-fresh)
                           (error
                            (shipit--debug-log "❌ Timeline fetch failed on page %d: %s" page (error-message-string err))
                            nil)))
             (events (when etag-result (plist-get etag-result :json))))
        (shipit--debug-log "Timeline page %d: fetched %d events" page (length events))
        (if (and events (> (length events) 0))
            (progn
              (setq all-events (append all-events events))
              (setq page (1+ page))
              ;; Stop if we got less than a full page (no more results)
              (when (< (length events) per-page)
                (setq continue nil)))
          (setq continue nil))))
    (shipit--debug-log "Fetched %d total timeline events across %d pages" (length all-events) (1- page))
    all-events))

(defun shipit--fetch-recent-timeline-events-async (repo pr-number callback)
  "Fetch the most recent timeline events for PR-NUMBER in REPO.
Calls CALLBACK with a list of events.  Unlike
`shipit--fetch-timeline-events-async' which walks pages from the
beginning, this variant uses the Link header from the first response
to jump directly to the last page, which is where recent events live
since GitHub returns timeline events in chronological order oldest
first.  For the common case of a busy issue with many timeline pages
this turns 8-10 sequential HTTP calls into 2."
  (shipit--debug-log "ASYNC: Starting recent-timeline fetch for %s#%s" repo pr-number)
  (let* ((endpoint (format "/repos/%s/issues/%s/timeline" repo pr-number))
         (per-page 100)
         (base-url (concat (or shipit-api-url "https://api.github.com") endpoint))
         (url1 (format "%s?per_page=%d&page=1" base-url per-page))
         (headers `(("Accept" . "application/vnd.github+json")
                    ("Authorization" . ,(format "Bearer %s" (shipit--github-token)))
                    ("X-GitHub-Api-Version" . "2022-11-28"))))
    (shipit--url-retrieve-async-with-headers
     url1 "GET" headers nil
     (lambda (page1-events response-headers)
       (let ((last-page (shipit--parse-link-header-last-page response-headers)))
         (shipit--debug-log "ASYNC: Recent-timeline page 1 got %d events, last-page=%s"
                            (length (or page1-events '())) last-page)
         (if (or (null last-page) (<= last-page 1))
             (funcall callback (or page1-events '()))
           (let ((url-last (format "%s?per_page=%d&page=%d"
                                   base-url per-page last-page)))
             (shipit--debug-log "ASYNC: Recent-timeline jumping to last page %d" last-page)
             (shipit--url-retrieve-async
              url-last "GET" headers nil
              (lambda (last-events)
                (shipit--debug-log "ASYNC: Recent-timeline last page got %d events"
                                   (length (or last-events '())))
                (funcall callback (or last-events '())))
              (lambda (err)
                (shipit--debug-log "ASYNC: Recent-timeline last-page error: %s" err)
                (funcall callback (or page1-events '()))))))))
     (lambda (err)
       (shipit--debug-log "ASYNC: Recent-timeline page 1 error: %s" err)
       (funcall callback nil)))))

(defun shipit--fetch-timeline-events-async (repo pr-number callback)
  "Fetch timeline events asynchronously for PR-NUMBER in REPO.
Calls CALLBACK with the list of events when complete.
Fetches all pages before calling callback."
  (shipit--debug-log "ASYNC: Starting async timeline fetch for PR %s in %s" pr-number repo)
  (let* ((endpoint (format "/repos/%s/issues/%s/timeline" repo pr-number))
         (per-page 100)
         (max-pages 10)
         (all-events '())
         (headers `(("Accept" . "application/vnd.github+json")
                    ("Authorization" . ,(format "Bearer %s" (shipit--github-token)))
                    ("X-GitHub-Api-Version" . "2022-11-28"))))
    ;; Start recursive page fetching
    (shipit--fetch-timeline-page-async
     repo pr-number endpoint headers per-page max-pages 1 all-events callback)))

(defun shipit--fetch-timeline-page-async (repo pr-number endpoint headers per-page max-pages page all-events callback)
  "Fetch a single page of timeline events asynchronously.
Recursively fetches more pages if needed, then calls CALLBACK with all events."
  (let ((url (format "%s%s?per_page=%d&page=%d" (or shipit-api-url "https://api.github.com") endpoint per-page page)))
    (shipit--debug-log "ASYNC: Fetching timeline page %d: %s" page url)
    (shipit--url-retrieve-async
     url "GET" headers nil
     ;; Success callback
     (lambda (events)
       (shipit--debug-log "ASYNC: Timeline page %d returned %d events" page (length events))
       (let ((updated-events (append all-events events)))
         (if (and events
                  (>= (length events) per-page)
                  (< page max-pages))
             ;; More pages to fetch
             (shipit--fetch-timeline-page-async
              repo pr-number endpoint headers per-page max-pages
              (1+ page) updated-events callback)
           ;; Done fetching all pages
           (progn
             (shipit--debug-log "ASYNC: Timeline fetch complete, total %d events" (length updated-events))
             (funcall callback updated-events)))))
     ;; Error callback
     (lambda (err)
       (shipit--debug-log "ASYNC: Timeline fetch error on page %d: %s" page err)
       ;; Return whatever we have so far
       (funcall callback all-events)))))

(defun shipit--fetch-general-comments-async (repo pr-number callback)
  "Fetch general comments asynchronously for PR-NUMBER in REPO.
Dispatches through the comment backend.  Uses :fetch-general-comments-async
if available, otherwise falls back to sync :fetch-general-comments + callback."
  (shipit--debug-log "ASYNC: Starting async general comments fetch for PR %s in %s" pr-number repo)
  (let* ((resolved (shipit-comment--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (async-fn (plist-get backend :fetch-general-comments-async)))
    (if async-fn
        (funcall async-fn config pr-number callback)
      ;; Fallback: sync fetch + callback
      (let ((fetch-fn (plist-get backend :fetch-general-comments)))
        (if fetch-fn
            (let ((comments (funcall fetch-fn config pr-number)))
              (funcall callback comments))
          (funcall callback nil))))))

(defun shipit--fetch-general-comments-async-github (repo pr-number callback)
  "Fetch general comments for GitHub PR-NUMBER in REPO asynchronously.
Uses async pagination for issue comments and reviews, then combines them."
  (let* ((headers `(("Accept" . "application/vnd.github+json")
                    ("Authorization" . ,(format "Bearer %s" (shipit--github-token)))
                    ("X-GitHub-Api-Version" . "2022-11-28")))
         ;; Track completion of both requests
         (results (list :issue-comments nil :reviews nil :issue-done nil :reviews-done nil)))

    ;; Fetch issue comments with pagination
    (shipit--fetch-issue-comments-page-async
     repo pr-number headers 1 '()
     (lambda (all-comments)
       (shipit--debug-log "ASYNC: Issue comments fetch complete with %d comments" (length all-comments))
       ;; Add comment type marker
       (plist-put results :issue-comments
                  (mapcar (lambda (c) (append c '((shipit-comment-type . "issue")))) all-comments))
       (plist-put results :issue-done t)
       (shipit--maybe-complete-general-comments-async results pr-number callback)))

    ;; Fetch PR reviews with pagination
    (shipit--fetch-reviews-page-async
     repo pr-number headers 1 '()
     (lambda (all-reviews)
       (shipit--debug-log "ASYNC: Reviews fetch complete with %d reviews" (length all-reviews))
       ;; Convert reviews to comment format
       (plist-put results :reviews (shipit--convert-reviews-to-comments all-reviews pr-number))
       (plist-put results :reviews-done t)
       (shipit--maybe-complete-general-comments-async results pr-number callback)))))

(defun shipit--fetch-issue-comments-page-async (repo pr-number headers page accumulated callback)
  "Fetch a single page of issue comments asynchronously.
Recursively fetches more pages if needed, then calls CALLBACK with all comments."
  (let ((url (format "%s/repos/%s/issues/%s/comments?per_page=100&page=%d"
                     (or shipit-api-url "https://api.github.com") repo pr-number page)))
    (shipit--debug-log "ASYNC: Fetching issue comments page %d" page)
    (shipit--url-retrieve-async
     url "GET" headers nil
     (lambda (data)
       (shipit--debug-log "ASYNC: Issue comments page %d returned %d comments" page (length data))
       (let ((updated (append accumulated data)))
         (if (and data (>= (length data) 100) (< page 10))
             ;; More pages to fetch
             (shipit--fetch-issue-comments-page-async
              repo pr-number headers (1+ page) updated callback)
           ;; Done
           (funcall callback updated))))
     (lambda (err)
       (shipit--debug-log "ASYNC: Issue comments page %d error: %s" page err)
       (funcall callback accumulated)))))

(defun shipit--fetch-reviews-page-async (repo pr-number headers page accumulated callback)
  "Fetch a single page of PR reviews asynchronously.
Recursively fetches more pages if needed, then calls CALLBACK with all reviews."
  (let ((url (format "%s/repos/%s/pulls/%s/reviews?per_page=100&page=%d"
                     (or shipit-api-url "https://api.github.com") repo pr-number page)))
    (shipit--debug-log "ASYNC: Fetching reviews page %d" page)
    (shipit--url-retrieve-async
     url "GET" headers nil
     (lambda (data)
       (shipit--debug-log "ASYNC: Reviews page %d returned %d reviews" page (length data))
       (let ((updated (append accumulated data)))
         (if (and data (>= (length data) 100) (< page 10))
             ;; More pages to fetch
             (shipit--fetch-reviews-page-async
              repo pr-number headers (1+ page) updated callback)
           ;; Done
           (funcall callback updated))))
     (lambda (err)
       (shipit--debug-log "ASYNC: Reviews page %d error: %s" page err)
       (funcall callback accumulated)))))

(defun shipit--maybe-complete-general-comments-async (results pr-number callback)
  "Check if both async fetches are done and call CALLBACK if so.
RESULTS is a plist tracking completion state, PR-NUMBER is for logging."
  (when (and (plist-get results :issue-done)
             (plist-get results :reviews-done))
    (let* ((issue-comments (or (plist-get results :issue-comments) '()))
           (reviews (or (plist-get results :reviews) '()))
           (combined (append issue-comments reviews))
           ;; Deduplicate by ID
           (all-comments (cl-remove-duplicates combined
                                               :key (lambda (c) (cdr (assq 'id c)))
                                               :test 'equal)))
      (shipit--debug-log "ASYNC: General comments complete - %d issue + %d reviews = %d total (after dedup: %d)"
                         (length issue-comments) (length reviews)
                         (length combined) (length all-comments))
      (funcall callback all-comments))))

(defun shipit--cache-general-comments-with-dedup (all-general-comments)
  "Cache ALL-GENERAL-COMMENTS in current buffer, deduplicating against inline comments.
Must be called with current-buffer set to the shipit buffer."
  (let* ((inline-comment-ids (when (and (boundp 'shipit--cached-inline-comments) shipit--cached-inline-comments)
                               (mapcar (lambda (c) (cdr (assq 'id c))) shipit--cached-inline-comments)))
         (deduplicated-general-comments
          (if inline-comment-ids
              (cl-remove-if (lambda (comment)
                              (member (cdr (assq 'id comment)) inline-comment-ids))
                            all-general-comments)
            all-general-comments)))
    (shipit--debug-log "DEDUPLICATION: general=%d inline=%d after-dedup=%d"
                       (length all-general-comments)
                       (if inline-comment-ids (length inline-comment-ids) 0)
                       (length deduplicated-general-comments))
    (setq shipit--cached-general-comments
          (if (vectorp deduplicated-general-comments)
              (append deduplicated-general-comments nil)
            deduplicated-general-comments))
    (setq shipit--general-comments-fetched t)))

(defun shipit--fetch-files-async (repo pr-number callback)
  "Fetch PR files asynchronously for PR-NUMBER in REPO.
Fetches all pages (up to 10) and calls CALLBACK with (files . truncated-p).
Uses ETag caching to properly handle 304 Not Modified responses."
  (shipit--debug-log "ASYNC: Starting async files fetch for PR %s in %s" pr-number repo)
  (shipit--fetch-files-page-async repo pr-number 1 '() callback))

(defun shipit--fetch-files-page-async (repo pr-number page accumulated callback)
  "Fetch a single page of PR files asynchronously with ETag caching.
Recursively fetches more pages if needed, then calls CALLBACK with (files . truncated-p)."
  (let ((endpoint (format "/repos/%s/pulls/%s/files" repo pr-number))
        (params `((per_page . 100) (page . ,page))))
    (shipit--debug-log "ASYNC: Fetching files page %d" page)
    (shipit-gh-etag-get-json-async
     endpoint params shipit-github-token
     (lambda (result)
       (let ((data (plist-get result :json)))
         (shipit--debug-log "ASYNC: Files page %d returned %d files (status: %s, from-cache: %s)"
                            page (length data)
                            (plist-get result :status)
                            (plist-get result :from-cache))
         (let ((updated (append accumulated data)))
           (if (and data (>= (length data) 100) (< page 10))
               ;; More pages to fetch
               (shipit--fetch-files-page-async
                repo pr-number (1+ page) updated callback)
             ;; Done - check if truncated (hit page limit with full page)
             (let ((truncated (and (>= page 10) data (>= (length data) 100))))
               (shipit--debug-log "ASYNC: Files fetch complete, total %d files, truncated=%s"
                                  (length updated) truncated)
               (funcall callback (cons updated truncated))))))))))

(defun shipit--fetch-more-files-async (repo pr-number start-page callback)
  "Fetch additional files starting from START-PAGE for PR-NUMBER in REPO.
Fetches up to 10 more pages and calls CALLBACK with (files truncated pages-fetched)."
  (shipit--debug-log "ASYNC: Fetching more files starting from page %d" start-page)
  (shipit--fetch-more-files-page-async repo pr-number start-page '() 0 callback))

(defun shipit--fetch-more-files-page-async (repo pr-number page accumulated pages-fetched callback)
  "Fetch additional pages of PR files starting from PAGE.
ACCUMULATED is the list of files fetched so far, PAGES-FETCHED tracks how many pages loaded.
Fetches up to 10 pages then calls CALLBACK with (files truncated pages-fetched)."
  (let ((endpoint (format "/repos/%s/pulls/%s/files" repo pr-number))
        (params `((per_page . 100) (page . ,page)))
        (max-additional-pages 10))
    (shipit--debug-log "ASYNC: Fetching more files page %d (pages fetched so far: %d)" page pages-fetched)
    (shipit-gh-etag-get-json-async
     endpoint params shipit-github-token
     (lambda (result)
       (let ((data (plist-get result :json))
             (new-pages-fetched (1+ pages-fetched)))
         (shipit--debug-log "ASYNC: More files page %d returned %d files" page (length data))
         (let ((updated (append accumulated data)))
           (if (and data (>= (length data) 100) (< new-pages-fetched max-additional-pages))
               ;; More pages to fetch
               (shipit--fetch-more-files-page-async
                repo pr-number (1+ page) updated new-pages-fetched callback)
             ;; Done - check if truncated (full page but hit our limit)
             (let ((truncated (and data (>= (length data) 100))))
               (shipit--debug-log "ASYNC: More files fetch complete, got %d new files, truncated=%s"
                                  (length updated) truncated)
               (funcall callback (list updated truncated new-pages-fetched))))))))))

(defun shipit--fetch-commits-async (repo pr-number callback)
  "Fetch PR commits asynchronously for PR-NUMBER in REPO.
Fetches all pages (up to 10) and calls CALLBACK with the list of commits."
  (shipit--debug-log "ASYNC: Starting async commits fetch for PR %s in %s" pr-number repo)
  (shipit--fetch-commits-page-async repo pr-number 1 '() callback))

(defun shipit--fetch-commits-page-async (repo pr-number page accumulated callback)
  "Fetch a single page of PR commits asynchronously with ETag caching.
Recursively fetches more pages if needed, then calls CALLBACK with all commits."
  (let ((endpoint (format "/repos/%s/pulls/%s/commits" repo pr-number))
        (params `((per_page . 100) (page . ,page))))
    (shipit--debug-log "ASYNC: Fetching commits page %d" page)
    (shipit-gh-etag-get-json-async
     endpoint params shipit-github-token
     (lambda (result)
       (let ((data (plist-get result :json)))
         (shipit--debug-log "ASYNC: Commits page %d returned %d commits (status: %s, from-cache: %s)"
                            page (length data)
                            (plist-get result :status)
                            (plist-get result :from-cache))
         (let ((updated (append accumulated data)))
           (if (and data (>= (length data) 100) (< page 10))
               (shipit--fetch-commits-page-async
                repo pr-number (1+ page) updated callback)
             (shipit--debug-log "ASYNC: Commits fetch complete, total %d commits"
                                (length updated))
             (funcall callback updated))))))))

(defun shipit--fetch-review-decision-async (repo pr-number callback)
  "Fetch PR review decision asynchronously for PR-NUMBER in REPO.
Calls CALLBACK with the review-info alist when complete.
This fetches reviews and requested reviewers in parallel."
  (shipit--debug-log "ASYNC: Starting async review decision fetch for PR %s in %s" pr-number repo)

  ;; Check cache first
  (if shipit--cached-review-decision
      (progn
        (shipit--debug-log "ASYNC: Using cached review decision data for PR #%s" pr-number)
        (funcall callback shipit--cached-review-decision))

    ;; Fetch reviews and requested reviewers in parallel
    (let* ((headers `(("Accept" . "application/vnd.github+json")
                      ("Authorization" . ,(format "Bearer %s" (shipit--github-token)))
                      ("X-GitHub-Api-Version" . "2022-11-28")))
           ;; Storage for parallel results
           (reviews-data nil)
           (reviews-done nil)
           (requested-reviewers-data nil)
           (requested-reviewers-done nil))

      ;; Fetch reviews
      (shipit--url-retrieve-async
       (format "%s/repos/%s/pulls/%s/reviews?per_page=100" (or shipit-api-url "https://api.github.com") repo pr-number)
       "GET" headers nil
       (lambda (data)
         (setq reviews-data data
               reviews-done t)
         (shipit--debug-log "ASYNC: Reviews fetch returned %d reviews" (length data))
         (shipit--check-review-decision-complete
          repo pr-number reviews-data reviews-done requested-reviewers-data requested-reviewers-done callback))
       (lambda (err)
         (shipit--debug-log "ASYNC: Reviews fetch error: %s" err)
         (setq reviews-done t)
         (shipit--check-review-decision-complete
          repo pr-number reviews-data reviews-done requested-reviewers-data requested-reviewers-done callback)))

      ;; Fetch requested reviewers
      (shipit--url-retrieve-async
       (format "%s/repos/%s/pulls/%s/requested_reviewers?per_page=100" (or shipit-api-url "https://api.github.com") repo pr-number)
       "GET" headers nil
       (lambda (data)
         (setq requested-reviewers-data data
               requested-reviewers-done t)
         (shipit--debug-log "ASYNC: Requested reviewers fetch returned")
         (shipit--check-review-decision-complete
          repo pr-number reviews-data reviews-done requested-reviewers-data requested-reviewers-done callback))
       (lambda (err)
         (shipit--debug-log "ASYNC: Requested reviewers fetch error: %s" err)
         (setq requested-reviewers-done t)
         (shipit--check-review-decision-complete
          repo pr-number reviews-data reviews-done requested-reviewers-data requested-reviewers-done callback))))))

(defun shipit--check-review-decision-complete (repo pr-number reviews-data reviews-done requested-reviewers-data requested-reviewers-done callback)
  "Check if all review decision data has been fetched and call CALLBACK if complete."
  (when (and reviews-done requested-reviewers-done)
    (shipit--debug-log "ASYNC: All review decision requests completed")
    (let* (;; Process reviews to get latest reviews per user
           (latest-reviews (when reviews-data
                             (shipit--get-latest-reviews-per-user reviews-data)))
           ;; Compute review decision from latest reviews
           (basic-review-decision (shipit--compute-review-decision-from-reviews latest-reviews))
           ;; Process requested reviewers
           (requested-users (cdr (assq 'users requested-reviewers-data)))
           (requested-teams (cdr (assq 'teams requested-reviewers-data)))
           ;; Build completed reviews list (latest-reviews is already a list)
           (completed-reviews latest-reviews)
           ;; Build status text
           (status-text (shipit--get-status-text-for-decision basic-review-decision))
           ;; Build final result alist
           (review-info `((status-text . ,status-text)
                          (review-decision . ,basic-review-decision)
                          (completed-reviews . ,completed-reviews)
                          (pending-users . ,requested-users)
                          (pending-teams . ,requested-teams)
                          (latest-reviews . ,latest-reviews))))

      ;; Cache the result
      (setq shipit--cached-review-decision review-info)
      (shipit--debug-log "ASYNC: Review decision processed - status=%s" status-text)
      (funcall callback review-info))))

(defun shipit--get-status-text-for-decision (decision)
  "Get human-readable status text with SVG icon for review DECISION."
  (let* ((status-key (cond
                      ((string= decision "APPROVED") "APPROVED")
                      ((string= decision "CHANGES_REQUESTED") "CHANGES_REQUESTED")
                      ((string= decision "REVIEW_REQUIRED") "REVIEW_REQUIRED")
                      (t "UNKNOWN")))
         (emoji-fallback (cond
                          ((string= decision "APPROVED") "✅")
                          ((string= decision "CHANGES_REQUESTED") "❌")
                          ((string= decision "REVIEW_REQUIRED") "⏳")
                          (t "❓")))
         (text (cond
                ((string= decision "APPROVED") "Approved")
                ((string= decision "CHANGES_REQUESTED") "Changes Requested")
                ((string= decision "REVIEW_REQUIRED") "Review Required")
                (t "Unknown")))
         (icon (shipit--get-approval-status-icon status-key emoji-fallback)))
    (concat icon " " text)))

(defun shipit--fetch-file-content-from-github (repo file-path ref)
  "Fetch FILE-PATH content from REPO at REF via GitHub API.
Returns the file content as a string, or nil on error.
REPO should be 'owner/repo' format."
  (let* ((encoded-path (url-hexify-string file-path))
         (endpoint (format "/repos/%s/contents/%s" repo encoded-path))
         (url (concat shipit-api-url endpoint "?ref=" ref))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "Fetching file content: %s at %s from %s" file-path ref repo)
    (condition-case err
        (let* ((result (shipit--url-retrieve-sync url "GET" headers nil))
               (response-data (car result))
               (status (cdr result)))
          (cond
           ;; Success - parse content
           ((and response-data (= status 200))
            (let ((content (cdr (assq 'content response-data)))
                  (encoding (cdr (assq 'encoding response-data))))
              (if (and content (string= encoding "base64"))
                  (decode-coding-string
                   (base64-decode-string (replace-regexp-in-string "\n" "" content))
                   'utf-8)
                (shipit--debug-log "Unexpected encoding or missing content: %s" encoding)
                nil)))
           ;; 304 with no data means corrupted cache - clear and retry
           ((and (= status 304) (null response-data))
            (shipit--debug-log "Got 304 with corrupted cache, clearing and retrying")
            ;; Delete the corrupted cache file
            (let ((cache-file (url-cache-create-filename url)))
              (when (and cache-file (file-exists-p cache-file))
                (delete-file cache-file)
                (shipit--debug-log "Deleted corrupted cache file: %s" cache-file)))
            ;; Retry - cache file is now gone so url.el won't send If-Modified-Since
            (let* ((retry-result (shipit--url-retrieve-sync url "GET" headers nil))
                   (retry-data (car retry-result))
                   (retry-status (cdr retry-result)))
              (if (and retry-data (= retry-status 200))
                  (let ((content (cdr (assq 'content retry-data)))
                        (encoding (cdr (assq 'encoding retry-data))))
                    (if (and content (string= encoding "base64"))
                        (decode-coding-string
                         (base64-decode-string (replace-regexp-in-string "\n" "" content))
                         'utf-8)
                      nil))
                (shipit--debug-log "Retry also failed: status=%s" retry-status)
                nil)))
           (t
            (shipit--debug-log "Failed to fetch file: status=%s" status)
            nil)))
      (error
       (shipit--debug-log "Error fetching file content: %s" (error-message-string err))
       nil))))

(provide 'shipit-http)
;;; shipit-http.el ends here
