;;; shipit-gitlab-http.el --- GitLab HTTP plumbing -*- lexical-binding: t; -*-

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
;; GitLab REST API HTTP plumbing.
;; Pattern follows shipit-issue-jira.el (build-url, api-request, auth-header).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'auth-source)
(require 'shipit-core)

(defun shipit-gitlab--project-path (config)
  "Return URL-encoded project identifier from CONFIG.
CONFIG may contain :project-id (numeric) or :project-path (\"group/project\").
When :project-path is used, slashes are percent-encoded for API URLs."
  (let ((project-id (plist-get config :project-id))
        (project-path (plist-get config :project-path)))
    (cond
     (project-id (number-to-string project-id))
     (project-path (url-hexify-string project-path))
     (t (error "GitLab config requires :project-id or :project-path")))))

(defun shipit-gitlab--build-url (config path)
  "Build full GitLab REST API URL from CONFIG and PATH.
Uses :api-url from CONFIG, defaulting to \"https://gitlab.com\"."
  (let ((api-url (or (plist-get config :api-url) "https://gitlab.com")))
    (concat (string-trim-right api-url "/") "/api/v4" path)))

(defun shipit-gitlab--auth-header (config)
  "Build PRIVATE-TOKEN auth header from CONFIG.
First checks :token in CONFIG, then falls back to auth-source lookup
using the hostname from :api-url.  Tries both the bare host and
api.HOST to match common authinfo conventions.
Returns (\"PRIVATE-TOKEN\" . TOKEN) or nil if no credentials found."
  (let ((token (plist-get config :token)))
    (if token
        (cons "PRIVATE-TOKEN" token)
      ;; Fall back to auth-source — try bare host, then api.host
      (let* ((api-url (or (plist-get config :api-url) "https://gitlab.com"))
             (host (url-host (url-generic-parse-url api-url)))
             (api-host (concat "api." host))
             (found (or (car (auth-source-search :host host :max 1))
                        (car (auth-source-search :host api-host :max 1)))))
        (when found
          (let ((secret (plist-get found :secret)))
            (when secret
              (let ((resolved (if (functionp secret) (funcall secret) secret)))
                (when resolved
                  (cons "PRIVATE-TOKEN" resolved))))))))))

(defun shipit-gitlab--curl-args (url auth method data)
  "Build curl argument list for URL with AUTH, METHOD, and DATA."
  (let ((args (list "-s" "-S" "--max-time" "30" "-X" method)))
    (when auth
      (setq args (append args (list "-H" (format "%s: %s" (car auth) (cdr auth))))))
    (setq args (append args (list "-H" "Content-Type: application/json")))
    (when data
      (setq args (append args (list "-d" (encode-coding-string (json-encode data) 'utf-8)))))
    (append args (list url))))

(defun shipit-gitlab--curl-request (config path method &optional data)
  "Make a METHOD request to GitLab API using CONFIG at PATH.
Optional DATA is an alist JSON-encoded as the request body.
Returns parsed JSON, or nil on failure."
  (let* ((url (shipit-gitlab--build-url config path))
         (auth (shipit-gitlab--auth-header config))
         (args (shipit-gitlab--curl-args url auth method data))
         (buf (generate-new-buffer " *shipit-gitlab-curl*")))
    (shipit--debug-log "GitLab API: %s %s (auth=%s)" method url (if auth "yes" "NO"))
    (unwind-protect
        (let ((exit-code (apply #'call-process "curl" nil buf nil args)))
          (if (not (eq exit-code 0))
              (progn
                (shipit--debug-log "GitLab API: curl exit %d for %s %s" exit-code method url)
                nil)
            (with-current-buffer buf
              (goto-char (point-min))
              (condition-case err
                  (let ((result (json-read)))
                    ;; Detect API error responses (e.g. 401 Unauthorized)
                    (if (and (consp result)
                             (not (vectorp result))
                             (assq 'message result)
                             (null (assq 'id result)))
                        (progn
                          (shipit--debug-log "GitLab API: error response: %s"
                                             (cdr (assq 'message result)))
                          nil)
                      result))
                (error
                 (shipit--debug-log "GitLab API: JSON parse error: %s (body: %s)"
                                    err (buffer-substring-no-properties
                                         (point-min) (min (point-max) (+ (point-min) 200))))
                 nil)))))
      (kill-buffer buf))))

(defun shipit-gitlab--api-request (config path)
  "Make a GET request to GitLab REST API using CONFIG at PATH.
Returns parsed JSON as alist, or nil on failure."
  (shipit-gitlab--curl-request config path "GET"))

(defun shipit-gitlab--api-request-method (config path data method)
  "Make a METHOD request to GitLab REST API using CONFIG at PATH with DATA.
DATA is an alist that will be JSON-encoded as the request body.
METHOD is \"POST\", \"PUT\", or \"DELETE\".
Returns parsed JSON response as alist, or nil on failure."
  (shipit-gitlab--curl-request config path method data))

(defun shipit-gitlab--curl-request-with-headers (config path method &optional data)
  "Make a METHOD request returning (HEADERS-ALIST . PARSED-JSON).
Like `shipit-gitlab--curl-request' but uses -D to capture response headers.
HEADERS-ALIST maps lowercase header names to values."
  (let* ((url (shipit-gitlab--build-url config path))
         (auth (shipit-gitlab--auth-header config))
         (header-file (make-temp-file "shipit-gl-hdr"))
         (args (list "-s" "-S" "--max-time" "30" "-X" method
                     "-D" header-file
                     "-H" "Content-Type: application/json")))
    (when auth
      (setq args (append args (list "-H" (format "%s: %s" (car auth) (cdr auth))))))
    (when data
      (setq args (append args (list "-d" (encode-coding-string (json-encode data) 'utf-8)))))
    (setq args (append args (list url)))
    (shipit--debug-log "GitLab API (paginated): %s %s" method url)
    (let ((buf (generate-new-buffer " *shipit-gitlab-curl*")))
      (unwind-protect
          (let ((exit-code (apply #'call-process "curl" nil buf nil args)))
            (if (not (eq exit-code 0))
                (progn
                  (shipit--debug-log "GitLab API: curl exit %d for %s %s" exit-code method url)
                  (cons nil nil))
              ;; Parse headers
              (let ((headers nil))
                (when (file-exists-p header-file)
                  (with-temp-buffer
                    (insert-file-contents header-file)
                    (goto-char (point-min))
                    (while (re-search-forward "^\\([^:]+\\): \\(.*\\)\r?$" nil t)
                      (push (cons (downcase (match-string 1))
                                  (string-trim (match-string 2)))
                            headers))))
                ;; Parse JSON body
                (with-current-buffer buf
                  (goto-char (point-min))
                  (condition-case err
                      (let ((result (json-read)))
                        (if (and (consp result)
                                 (not (vectorp result))
                                 (assq 'message result)
                                 (null (assq 'id result)))
                            (progn
                              (shipit--debug-log "GitLab API: error response: %s"
                                                 (cdr (assq 'message result)))
                              (cons headers nil))
                          (cons headers result)))
                    (error
                     (shipit--debug-log "GitLab API: JSON parse error: %s" err)
                     (cons headers nil)))))))
        (kill-buffer buf)
        (when (file-exists-p header-file)
          (delete-file header-file))))))

(defun shipit-gitlab--api-request-paginated (config path)
  "Make a paginated GET request to GitLab REST API.
Follows x-next-page headers to collect all pages.
PATH should include ?per_page=100 for efficiency.
Returns combined list of items from all pages, or nil on failure."
  (let ((all-items '())
        (current-path path)
        (max-pages 10)
        (page-count 0))
    (while (and current-path (< page-count max-pages))
      (setq page-count (1+ page-count))
      (let* ((response (shipit-gitlab--curl-request-with-headers config current-path "GET"))
             (headers (car response))
             (body (cdr response)))
        (if (not body)
            (setq current-path nil)
          ;; Collect items (body is a vector)
          (setq all-items (append all-items (append body nil)))
          ;; Check for next page
          (let ((next-page (cdr (assoc "x-next-page" headers))))
            (if (and next-page (not (string-empty-p next-page)))
                ;; Build URL for next page
                (setq current-path
                      (if (string-match "[?&]page=" current-path)
                          (replace-regexp-in-string
                           "\\([?&]\\)page=[0-9]+"
                           (format "\\1page=%s" next-page)
                           current-path)
                        (concat current-path
                                (if (string-match-p "\\?" current-path) "&" "?")
                                "page=" next-page)))
              (setq current-path nil))))))
    (shipit--debug-log "GitLab API paginated: collected %d items over %d pages"
                       (length all-items) page-count)
    (if (> (length all-items) 0) all-items nil)))

;;; Async HTTP via url-retrieve

(defun shipit-gitlab--handle-async-response (status callback)
  "Handle url-retrieve response in current buffer.
STATUS is the status plist from `url-retrieve'.
Call CALLBACK with parsed JSON on success, nil on error.
Always kills the response buffer."
  (let ((buf (current-buffer)))
    (unwind-protect
        (cond
         ((plist-get status :error)
          (shipit--debug-log "GitLab API async: network error: %s"
                             (plist-get status :error))
          (funcall callback nil))
         (t
          (goto-char (point-min))
          (let* ((status-line (buffer-substring (point-min) (line-end-position)))
                 (status-code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                                (string-to-number (match-string 1 status-line))))
                 (headers-end (search-forward "\n\n" nil t)))
            (cond
             ((or (not status-code) (< status-code 200) (>= status-code 300))
              (shipit--debug-log "GitLab API async: HTTP %s" (or status-code "unknown"))
              (funcall callback nil))
             ((not headers-end)
              (shipit--debug-log "GitLab API async: no body in response")
              (funcall callback nil))
             (t
              (set-buffer-multibyte t)
              (condition-case err
                  (let ((result (json-read)))
                    (if (and (consp result)
                             (not (vectorp result))
                             (assq 'message result)
                             (null (assq 'id result)))
                        (progn
                          (shipit--debug-log "GitLab API async: error response: %s"
                                             (cdr (assq 'message result)))
                          (funcall callback nil))
                      (funcall callback result)))
                (error
                 (shipit--debug-log "GitLab API async: JSON parse error: %s" err)
                 (funcall callback nil))))))))
      (kill-buffer buf))))

(defun shipit-gitlab--api-request-async (config path callback)
  "Make async GET request to GitLab REST API at PATH using CONFIG.
Calls CALLBACK with parsed JSON alist or nil on failure."
  (let* ((url (shipit-gitlab--build-url config path))
         (auth (shipit-gitlab--auth-header config))
         (url-request-method "GET")
         (url-request-extra-headers
          (append (when auth (list auth))
                  '(("Content-Type" . "application/json")))))
    (shipit--debug-log "GitLab API async: GET %s (auth=%s)" url (if auth "yes" "NO"))
    (url-retrieve url
                  (lambda (status)
                    (shipit-gitlab--handle-async-response status callback))
                  nil t)))

(provide 'shipit-gitlab-http)
;;; shipit-gitlab-http.el ends here
