;;; shipit-gh-etag.el --- GitHub REST with persistent repo-specific cache  -*- lexical-binding: t; -*-

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

;; Persistent cache implementation that survives refresh and provides performance benefits

(require 'url)
(require 'json)
(require 'shipit-core)

(declare-function shipit--debug-log "shipit-debug")

;; Fix for url-cache-extract header boundary detection
;; From: https://emacs.stackexchange.com/questions/32950/why-isnt-url-http-end-of-headers-set-correctly-when-using-url-automatic-caching
(defun url-cache-extract (fnam)
  "Extract FNAM from the cache."
  (let* ((coding-system-for-read 'binary)
         ;; Handle both URL and direct file path cases
         (cache-file (if (file-exists-p fnam)
                         fnam  ; Direct file path
                       (url-cache-create-filename fnam))))  ; URL -> file path
    (when (file-exists-p cache-file)
      (condition-case err
          (let ((buf (generate-new-buffer " *url-cache*")))
            (with-current-buffer buf
              (set-buffer-multibyte nil)
              (insert-file-contents-literally cache-file)
              (when (zerop (buffer-size))
                (kill-buffer buf)
                (error "Empty cache file %s" cache-file))

              ;; More robust header boundary detection
              (goto-char (point-min))
              (let ((end-of-headers
                     (or
                      ;; Method 1: Look for double CRLF
                      (when (search-forward "\r\n\r\n" nil t)
                        (- (point) 2))
                      ;; Method 2: Look for double LF
                      (progn (goto-char (point-min))
                             (when (search-forward "\n\n" nil t)
                               (- (point) 1)))
                      ;; Method 3: Look for empty line pattern
                      (progn (goto-char (point-min))
                             (when (re-search-forward "^$" nil t)
                               (point)))
                      ;; Fallback: assume no headers if nothing found
                      (point-min))))

                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "🔧 url-cache-extract: found headers end at position %d" end-of-headers))

                ;; Set the boundary marker that url.el expects
                (set (make-local-variable 'url-http-end-of-headers) end-of-headers)

                ;; Decompress gzip body if needed
                (when (and (fboundp 'zlib-decompress-region)
                           (< (1+ end-of-headers) (point-max))
                           (= (char-after (1+ end-of-headers)) ?\x1f))
                  (goto-char (1+ end-of-headers))
                  (zlib-decompress-region (point) (point-max)))

                ;; Switch to multibyte for text processing
                (set-buffer-multibyte t)

                ;; Return buffer
                (current-buffer))))
        (error
         (when (fboundp 'shipit--debug-log)
           (shipit--debug-log "⚠️ url-cache-extract error for %s: %s" cache-file (error-message-string err)))
         ;; Return nil on error instead of propagating
         nil)))))

(defvar shipit-gh-etag--persistent-cache (make-hash-table :test 'equal)
  "Persistent ETag cache: REPO/ENDPOINT -> (:etag STRING :json DATA :timestamp NUMBER).
This cache survives shipit-refresh and only gets cleared explicitly.")

(defconst shipit-gh-etag--user-state-prefixes
  '("activities-read:" "comments-seen:" "inline-seen:" "general-seen:" "last-viewed:" "jira-user:")
  "Key prefixes for user state entries in the persistent cache.
These entries track read/seen state and must survive cache clears.")

(defun shipit-gh-etag--user-state-key-p (key)
  "Return non-nil if KEY is a user state entry (not an API cache entry)."
  (cl-some (lambda (prefix) (string-prefix-p prefix key))
           shipit-gh-etag--user-state-prefixes))

(defun shipit-gh-etag--clear-api-cache-only ()
  "Clear API/ETag entries from persistent cache, preserving user state.
User state entries (activities-read:, comments-seen:, etc.) are kept."
  (let ((keys-to-remove nil))
    (maphash (lambda (key _value)
               (unless (shipit-gh-etag--user-state-key-p key)
                 (push key keys-to-remove)))
             shipit-gh-etag--persistent-cache)
    (dolist (key keys-to-remove)
      (remhash key shipit-gh-etag--persistent-cache))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🧹 Cleared %d API cache entries, preserved %d user state entries"
                         (length keys-to-remove)
                         (hash-table-count shipit-gh-etag--persistent-cache)))))

(defvar shipit-gh-etag--refresh-cache nil
  "Temporary cache for current refresh cycle to avoid duplicate requests.
Hash table: CACHE-KEY -> (:status :json :etag :from-cache ...).
This cache is reset at the start of each refresh to ensure that multiple sections
requesting the same endpoint during a single refresh get the same result without
additional network requests.")

(defvar shipit-gh-etag--cache-file (expand-file-name "shipit-etag-cache.el" user-emacs-directory)
  "File path for persistent ETag cache storage.")

(defun shipit-gh-etag--save-cache ()
  "Save the ETag cache to disk."
  (condition-case err
      (let ((cache-data '())
            (temp-file (concat shipit-gh-etag--cache-file ".tmp")))
        ;; Convert hash table to list for serialization
        (maphash (lambda (key value)
                   (push (cons key value) cache-data))
                 shipit-gh-etag--persistent-cache)
        ;; Write to temporary file first (atomic operation)
        (with-temp-file temp-file
          (insert ";; Shipit ETag cache - generated automatically\n")
          (insert ";; Format: ((key . value) (key . value) ...)\n")
          (insert (format ";; Generated: %s\n" (current-time-string)))
          (insert ";; Entries: ")
          (prin1 (length cache-data) (current-buffer))
          (insert "\n\n")
          (prin1 cache-data (current-buffer)))
        ;; Atomically replace cache file
        (rename-file temp-file shipit-gh-etag--cache-file t)
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "💾 Saved ETag cache: %d entries to %s" 
                            (length cache-data) shipit-gh-etag--cache-file)))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "❌ Failed to save ETag cache: %s" (error-message-string err))))))

(defun shipit-gh-etag--load-cache ()
  "Load the ETag cache from disk."
  (condition-case err
      (when (file-exists-p shipit-gh-etag--cache-file)
        (let ((cache-data nil)
              (loaded-count 0))
          ;; Read cache data from file
          (with-temp-buffer
            (insert-file-contents shipit-gh-etag--cache-file)
            (goto-char (point-min))
            ;; Skip comment lines to find the data
            (while (looking-at ";")
              (forward-line 1))
            (setq cache-data (read (current-buffer))))
          ;; Clear existing cache and load data
          (clrhash shipit-gh-etag--persistent-cache)
          (dolist (entry cache-data)
            (puthash (car entry) (cdr entry) shipit-gh-etag--persistent-cache)
            (setq loaded-count (1+ loaded-count)))
          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "💾 Loaded ETag cache: %d entries from %s" 
                              loaded-count shipit-gh-etag--cache-file))
          loaded-count))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "❌ Failed to load ETag cache: %s" (error-message-string err)))
     ;; On load error, start with empty cache
     (clrhash shipit-gh-etag--persistent-cache)
     0)))

(defun shipit-gh-etag--ensure-cache-loaded ()
  "Ensure the cache is loaded from disk if not already loaded."
  (when (zerop (hash-table-count shipit-gh-etag--persistent-cache))
    (shipit-gh-etag--load-cache)))

(defun shipit-gh-etag-invalidate (endpoint &optional params repo)
  "Invalidate the ETag cache entry for ENDPOINT with optional PARAMS.
REPO can be provided explicitly; if not, uses `shipit--current-displayed-pr' or git remote.
This should be called after mutating operations (POST, PATCH, DELETE)
to ensure the next GET fetches fresh data."
  (shipit-gh-etag--ensure-cache-loaded)
  (let* ((qstr (shipit-gh-etag--qstr params))
         (full-endpoint (concat endpoint qstr))
         (effective-repo (or repo
                             (cadr shipit--current-displayed-pr)
                             (when (fboundp 'shipit--get-repo-from-remote)
                               (shipit--get-repo-from-remote))))
         (cache-key (concat effective-repo full-endpoint)))
    (shipit--debug-log "🗑️ ETAG-INVALIDATE: Attempting to invalidate cache-key=%s (repo=%s, endpoint=%s)"
                       cache-key effective-repo full-endpoint)
    (if (gethash cache-key shipit-gh-etag--persistent-cache)
        (progn
          (remhash cache-key shipit-gh-etag--persistent-cache)
          (shipit--debug-log "🗑️ ETAG-INVALIDATE: ✅ Removed cache entry for %s" cache-key)
          (shipit-gh-etag--save-cache))
      (shipit--debug-log "🗑️ ETAG-INVALIDATE: ⚠️ No cache entry found for %s" cache-key))))

(defun shipit-gh-etag--qstr (params)
  "Turn alist PARAMS into a URL-encoded query string."
  (when params
    (concat "?"
            (mapconcat
             (lambda (kv)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car kv)))
                       (url-hexify-string (format "%s" (cdr kv)))))
             params "&"))))

(defun shipit-gh-etag--extract (headers name)
  "Get header NAME's value from raw HEADERS string."
  (with-temp-buffer
    (insert headers)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward (format "^%s:\\\\s-*\\\\(.*\\\\)$" (regexp-quote name)) nil t)
        (string-trim (match-string 1))))))

(defun shipit-gh-etag--status (headers)
  "Extract HTTP status code from HEADERS string."
  (when (string-match "^HTTP/[12]\\(?:\\.[01]\\)? \\([0-9]+\\)" headers)
    (string-to-number (match-string 1 headers))))

(defun shipit-gh-etag--normalize-etag (etag)
  "Normalize ETAG by removing W/ prefix for comparison.
GitHub sometimes returns ETags with/without W/ prefix inconsistently."
  (when (and etag (stringp etag))
    (if (string-prefix-p "W/" etag)
        (substring etag 2)  ; Remove W/ prefix
      etag)))

(defun shipit-gh-etag--extract-charset-from-headers ()
  "Extract charset from Content-Type header in current buffer.
Returns the charset as a symbol suitable for decode-coding-string, or 'utf-8 as default.
Expected to be called with point in HTTP response buffer after headers."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (charset 'utf-8))
      ;; Look for Content-Type header with charset parameter
      (when (re-search-forward "^Content-Type:\\s-*.*charset=\\([^ \r\n;]+\\)" nil t)
        (let* ((charset-str (match-string 1))
               (charset-sym (intern (downcase charset-str))))
          ;; Validate that this is a known coding system
          (if (coding-system-p charset-sym)
              (setq charset charset-sym)
            ;; Fall back to utf-8 if unknown charset
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "⚠️ Unknown charset '%s', using utf-8 instead" charset-str)))))
      charset)))

(defun shipit-gh-etag--proactive-cache-cleanup (endpoint params)
  "Proactively clean up any problematic cache files that might cause errors.
This prevents the url-cache-extract error by removing invalid cache files before requests."
  (condition-case err
      (let* ((qstr (shipit-gh-etag--qstr params))
             (url (concat "https://api.github.com"
                         (or endpoint "")
                         (or qstr "")))
             (cache-file (when (fboundp 'url-cache-create-filename)
                           (url-cache-create-filename url))))
        
        (when cache-file
          ;; Check if cache file exists and is problematic
          (cond
           ;; Case 1: Cache file doesn't exist - clear stored ETag to avoid 304 with missing cache
           ((not (file-exists-p cache-file))
            ;; Clear any stored ETag for this endpoint since cache file is gone
            ;; Use same cache key format as main code (lines 239-243)
            (let* ((repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                                 (match-string 1 endpoint)))
                   (cache-key (if repo-match
                                  (concat repo-match endpoint (or qstr ""))
                                (concat endpoint (or qstr "")))))
              (when (and (boundp 'shipit-gh-etag--persistent-cache)
                        (hash-table-p shipit-gh-etag--persistent-cache))
                (remhash cache-key shipit-gh-etag--persistent-cache)
                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "🧹 PROACTIVE: Cleared ETag for missing cache file: %s (key: %s)" cache-file cache-key)))))

           ;; Case 2: Cache file exists but is empty or corrupted
           ((condition-case nil
                (with-temp-buffer
                  (insert-file-contents cache-file)
                  (or (zerop (buffer-size))
                      (not (save-excursion
                             (goto-char (point-min))
                             (re-search-forward "^HTTP/" nil t)))))
              (error t))
            ;; File is corrupted or empty - delete it and clear ETag
            (delete-file cache-file)
            ;; Clear ETag using same key format as above
            (let* ((repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                                 (match-string 1 endpoint)))
                   (cache-key (if repo-match
                                  (concat repo-match endpoint (or qstr ""))
                                (concat endpoint (or qstr "")))))
              (when (and (boundp 'shipit-gh-etag--persistent-cache)
                        (hash-table-p shipit-gh-etag--persistent-cache))
                (remhash cache-key shipit-gh-etag--persistent-cache)
                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "🧹 PROACTIVE: Removed corrupted cache file and cleared ETag: %s (key: %s)" cache-file cache-key)))))
           
           ;; Case 3: Cache file looks valid
           (t
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "🧹 PROACTIVE: Cache file looks valid: %s" cache-file))))))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "⚠️ PROACTIVE cleanup failed: %s" (error-message-string err))))))

(defun shipit-gh-etag-get-json (endpoint &optional params token force-fresh)
  "GET GitHub REST JSON at ENDPOINT with persistent repo-specific caching.
If FORCE-FRESH is non-nil, bypass cache and fetch fresh data from API.
Returns plist with :status, :json, :etag, :from-cache, :rate-remaining, :rate-reset."
  ;; Ensure persistent cache is loaded from disk
  (shipit-gh-etag--ensure-cache-loaded)

  ;; Enable url.el caching for ETag functionality
  (setq url-automatic-caching t)
  (setq url-cache-directory (expand-file-name "url/cache" user-emacs-directory))
  
  ;; CRITICAL FIX: Proactively clean up any problematic cache files before starting request
  (shipit-gh-etag--proactive-cache-cleanup endpoint params)

  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "🔍 ETag request debug:")
    (shipit--debug-log "  endpoint: %S (type: %s)" endpoint (type-of endpoint))
    (shipit--debug-log "  params: %S (type: %s)" params (type-of params))
    (shipit--debug-log "  token: %S (type: %s)" (if token "PRESENT" "NIL") (type-of token))
    (shipit--debug-log "🕵️ CALLER INFO: Called from somewhere in the code"))

  (let* ((qstr (shipit-gh-etag--qstr params))
         (url (concat "https://api.github.com"
                     (or endpoint "")
                     (or qstr "")))
         ;; Extract repo from endpoint for cache key
         (repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                       (match-string 1 endpoint)))
         (cache-key (if repo-match
                        (concat repo-match endpoint (or qstr ""))
                        (concat endpoint (or qstr ""))))
         (cached-entry (gethash cache-key shipit-gh-etag--persistent-cache))
         (stored-etag (when cached-entry
                        (let ((etag (plist-get cached-entry :etag)))
                          ;; Only use ETags that are not empty or just quotes
                          (when (and etag
                                   (stringp etag)
                                   (> (length etag) 4)  ; More than just W/""
                                   (not (string= etag "W/\"\"")))
                            etag)))))

    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "  qstr: %S (type: %s)" qstr (type-of qstr))
      (shipit--debug-log "  final URL: %S" url))

    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🚀 ETAG-LOOKUP: endpoint=%s" endpoint)
      (shipit--debug-log "🚀 ETAG-LOOKUP: cache-key=%s" cache-key)
      (shipit--debug-log "🚀 ETAG-LOOKUP: has-cached=%s, force-fresh=%s"
                         (if cached-entry "YES" "NO") (if force-fresh "YES" "NO"))
      (when cached-entry
        (shipit--debug-log "🔍 ETAG-LOOKUP: CACHE ENTRY FOUND - ETag=%S, timestamp=%.2f, valid=%s"
                           (plist-get cached-entry :etag)
                           (or (plist-get cached-entry :timestamp) 0)
                           (if stored-etag "YES" "NO (empty/invalid)")))
      (when (not cached-entry)
        (shipit--debug-log "🔍 ETAG-LOOKUP: NO CACHE ENTRY - will make fresh request")))

    ;; Choose request type based on cache state
    (if (and cached-entry stored-etag (not force-fresh))
        ;; We have cached data - send conditional request with If-None-Match
        (progn
          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "🔄 Conditional request with ETag: %s" stored-etag))
          (let* ((url-request-method "GET")
                 (url-request-extra-headers
                  (delq nil
                        (list '("User-Agent" . "emacs-url")
                              '("Accept" . "application/vnd.github+json")
                              ;; CRITICAL: Manually add If-None-Match header (url.el automatic caching doesn't work)
                              `("If-None-Match" . ,stored-etag)
                              (let ((resolved (or token (shipit--github-token))))
                                (when resolved `("Authorization" . ,(format "Bearer %s" resolved)))))))
                 ;; Disable url.el automatic caching - we handle ETag headers manually
                 ;; Automatic caching adds If-modified-since with locale-specific dates that may contain non-ASCII chars
                 (url-automatic-caching nil)
                 (url-cache-directory (expand-file-name "url/cache" user-emacs-directory)))

            ;; Debug: Log what headers url.el will actually send and enable url-debug
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "🔍 URL.EL DEBUG: url-request-method=%s" url-request-method)
              (shipit--debug-log "🔍 URL.EL DEBUG: url-request-extra-headers=%S" url-request-extra-headers)
              (shipit--debug-log "🔍 URL.EL DEBUG: url-automatic-caching=%s" url-automatic-caching)
              (shipit--debug-log "🔍 URL.EL DEBUG: Final URL=%s" url)
              (shipit--debug-log "🔍 URL.EL DEBUG: Stored ETag value: %S (length=%d)" stored-etag (length stored-etag)))

            ;; Enable url.el debug logging and let url.el handle caching
            ;; Bind system-time-locale to "C" to ensure HTTP date headers use ASCII
            ;; (avoids "Multibyte text in HTTP request" with non-English locales)
            (let ((url-debug t)
                  (system-time-locale "C")
                  (buf (url-retrieve-synchronously url t t 60)))

              (if (not buf)
                  ;; No buffer returned - use cached data instead of erroring
                  (progn
                    (when (fboundp 'shipit--debug-log)
                      (shipit--debug-log "❌ Conditional request returned no buffer - using cached data"))
                    (if cached-entry
                        (list :status 304
                              :json (plist-get cached-entry :json)
                              :etag (plist-get cached-entry :etag)
                              :from-cache t
                              :rate-remaining nil
                              :rate-reset nil)
                      (error "No response buffer and no cached data available")))

                ;; Process conditional request response
                (condition-case err
                    (with-current-buffer buf
                  ;; Enable multibyte mode to properly interpret UTF-8 bytes
                  (set-buffer-multibyte t)
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "🔍 Conditional response buffer size: %d chars" (buffer-size)))

                  ;; Manual HTTP response parsing (more reliable than url-http-parse-response)
                  (goto-char (point-min))
                  (let* ((status (when (re-search-forward "^HTTP/[12]\\(?:\\.[01]\\)? \\([0-9]+\\)" nil t)
                                   (string-to-number (match-string 1))))
                         (etag (progn
                                 (goto-char (point-min))
                                 (when (re-search-forward "^ETag: \\(.*\\)$" nil t)
                                   (match-string 1))))
                         (remain (progn
                                   (goto-char (point-min))
                                   (when (re-search-forward "^X-RateLimit-Remaining: \\(.*\\)$" nil t)
                                     (match-string 1))))
                         (reset (progn
                                  (goto-char (point-min))
                                  (when (re-search-forward "^X-RateLimit-Reset: \\(.*\\)$" nil t)
                                    (match-string 1)))))

                    (when (fboundp 'shipit--debug-log)
                      (let ((normalized-stored (shipit-gh-etag--normalize-etag stored-etag))
                            (normalized-received (shipit-gh-etag--normalize-etag etag)))
                        (shipit--debug-log "🔍 CONDITIONAL RESPONSE:")
                        (shipit--debug-log "  Status: %s" status)
                        (shipit--debug-log "  Sent ETag:     %S" stored-etag)
                        (shipit--debug-log "  Received ETag: %S" (or etag "none"))
                        (shipit--debug-log "  Normalized Sent: %S" normalized-stored)
                        (shipit--debug-log "  Normalized Recv: %S" normalized-received)
                        (shipit--debug-log "  ETags match: %s" (if (equal normalized-stored normalized-received) "YES" "NO"))
                        (shipit--debug-log "  Response size: %d chars (304 should be ~0)" (buffer-size))
                        (shipit--debug-log "  Time since cache: %.2f seconds"
                                           (- (float-time) (or (plist-get cached-entry :timestamp) (float-time))))))

                    (cond
                     ;; 304 Not Modified - use cached data
                     ((eq status 304)
                      (when (fboundp 'shipit--debug-log)
                        (shipit--debug-log "✅ 304 Not Modified - using cached data"))
                      (list :status 304
                            :json (plist-get cached-entry :json)
                            :etag (plist-get cached-entry :etag)
                            :from-cache t
                            :rate-remaining (and remain (string-to-number remain))
                            :rate-reset reset))

                     ;; 200 OK - content changed, parse new JSON
                     ((eq status 200)
                      ;; Find JSON start (after headers) and let url.el handle decompression
                      (unless (re-search-forward "^$" nil t)
                        (error "Could not find end of headers"))
                      ;; Check if there's content to parse before calling json-read
                      (if (>= (point) (point-max))
                          (progn
                            (when (fboundp 'shipit--debug-log)
                              (shipit--debug-log "❌ No JSON content to parse - empty response body"))
                            (error "No JSON content in response"))
                        ;; CRITICAL FIX: Decode UTF-8 from HTTP response before parsing JSON
                        ;; The buffer contains raw UTF-8 bytes interpreted as unibyte/Latin-1 characters
                        ;; We need to convert them to proper Unicode strings
                        (let* ((body-start (point))
                               (body-end (point-max))
                               (charset (shipit-gh-etag--extract-charset-from-headers))
                               (json-data nil))

                          ;; Log the charset we detected
                          (when (fboundp 'shipit--debug-log)
                            (shipit--debug-log "🔤 Detected charset from Content-Type: %s" charset))

                          ;; Decode the response body from the detected charset
                          ;; This converts the unibyte UTF-8 bytes to proper Unicode characters
                          (condition-case err
                              (progn
                                ;; Apply charset decoding to the body region
                                ;; This transforms unibyte UTF-8 bytes to proper multibyte Unicode
                                (decode-coding-region body-start body-end charset)

                                (when (fboundp 'shipit--debug-log)
                                  (shipit--debug-log "✅ Successfully decoded HTTP response body using %s" charset))

                                ;; Now parse the properly-decoded JSON
                                (let* ((json-object-type 'alist)
                                       (json-array-type 'list)
                                       (json-null nil)
                                       (json-false nil))
                                  (goto-char body-start)
                                  (setq json-data (condition-case err
                                                   (json-read)
                                                 (error
                                                  (when (fboundp 'shipit--debug-log)
                                                    (shipit--debug-log "❌ JSON parsing error after UTF-8 decoding: %s" (error-message-string err))
                                                    (shipit--debug-log "❌ Buffer position: %d, buffer size: %d" (point) (buffer-size))
                                                    (shipit--debug-log "❌ Content at point (first 200 chars): %S"
                                                                        (buffer-substring (point) (min (+ (point) 200) (point-max)))))
                                                  (signal (car err) (cdr err)))))))
                            (error
                             (when (fboundp 'shipit--debug-log)
                               (shipit--debug-log "❌ UTF-8 decoding failed: %s" (error-message-string err)))
                             (signal (car err) (cdr err))))

                          (when (fboundp 'shipit--debug-log)
                            (shipit--debug-log "✅ Conditional request got fresh data, updating cache"))

                        ;; Update cache with new data
                        (when (and etag
                                   (stringp etag)
                                   (> (length etag) 4)
                                   (not (string= etag "W/\"\"")))
                          (when (fboundp 'shipit--debug-log)
                            (shipit--debug-log "🔄 CACHE UPDATE: Storing new ETag %S for key %s" etag cache-key))
                          (puthash cache-key
                                   (list :etag etag
                                         :json json-data
                                         :timestamp (float-time))
                                   shipit-gh-etag--persistent-cache)
                          ;; Save cache to disk after update
                          (shipit-gh-etag--save-cache)
                          (when (fboundp 'shipit--debug-log)
                            (let ((updated-entry (gethash cache-key shipit-gh-etag--persistent-cache)))
                              (shipit--debug-log "🔄 CACHE VERIFY: Stored ETag is now %S" (plist-get updated-entry :etag)))))

                        (list :status 200 :json json-data :etag etag :from-cache nil
                              :rate-remaining (and remain (string-to-number remain))
                              :rate-reset reset))))

                     ;; 429 Too Many Requests - return cached data if available
                     ((eq status 429)
                      (when (fboundp 'shipit--debug-log)
                        (shipit--debug-log "⚠️ Rate limited (429) on conditional request, returning cached data for: %s" endpoint))
                      (list :status 304
                            :json (plist-get cached-entry :json)
                            :etag (plist-get cached-entry :etag)
                            :from-cache t))

                     (t
                      (error "%s" (shipit-gh-etag--format-http-error status endpoint))))))
                  (error
                   (when (fboundp 'shipit--debug-log)
                     (shipit--debug-log "❌ Conditional response processing failed: %s" (error-message-string err)))
                   (kill-buffer buf)
                   (error "Conditional request failed: %s" (error-message-string err))))))))

      ;; No cached data or force-fresh - make normal request
      (progn
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "%s - making fresh request"
                             (if force-fresh "🔄 Force fresh requested" "🆕 No cache - first request")))
        ;; Delete url.el cache file when force-fresh to prevent stale If-Modified-Since
        (when force-fresh
          (let ((cache-file (url-cache-create-filename url)))
            (when (file-exists-p cache-file)
              (delete-file cache-file)
              (when (fboundp 'shipit--debug-log)
                (shipit--debug-log "🧹 FORCE-FRESH: Deleted url.el cache file: %s" cache-file)))))
        (let* ((url-request-method "GET")
             (url-request-extra-headers
              (delq nil
                    (list '("User-Agent" . "emacs-url")
                          '("Accept" . "application/vnd.github+json")
                          (let ((resolved (or token (shipit--github-token))))
                            (when resolved `("Authorization" . ,(format "Bearer %s" resolved)))))))
             ;; Disable url.el automatic caching - we handle it manually with ETags
             ;; Automatic caching adds If-modified-since with locale-specific dates that may contain non-ASCII chars
             (url-automatic-caching nil)
             (url-cache-directory (expand-file-name "url/cache" user-emacs-directory)))

        ;; Debug: Log what headers url.el will actually send for fresh request
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "🔍 URL.EL FRESH DEBUG: url-request-method=%s" url-request-method)
          (shipit--debug-log "🔍 URL.EL FRESH DEBUG: url-request-extra-headers=%S" url-request-extra-headers)
          (shipit--debug-log "🔍 URL.EL FRESH DEBUG: url-automatic-caching=%s" url-automatic-caching)
          (shipit--debug-log "🔍 URL.EL FRESH DEBUG: Final URL=%s" url))

        ;; Enable url.el debug logging and let url.el handle caching
        ;; Bind system-time-locale to "C" to ensure HTTP date headers use ASCII
        (let ((url-debug t)
              (system-time-locale "C")
              (buf (url-retrieve-synchronously url t t 60)))

          (unless buf
          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "❌ Fresh request returned no buffer"))
          (error "No response buffer for fresh request"))

        (condition-case err
            (with-current-buffer buf
              ;; Enable multibyte mode to properly interpret UTF-8 bytes
              (set-buffer-multibyte t)
              (when (fboundp 'shipit--debug-log)
                (shipit--debug-log "🔍 Response buffer size: %d chars" (buffer-size)))

              ;; Manual HTTP response parsing (more reliable than url-http-parse-response)
              (goto-char (point-min))
              (let* ((status (when (re-search-forward "^HTTP/[12]\\(?:\\.[01]\\)? \\([0-9]+\\)" nil t)
                               (string-to-number (match-string 1))))
                     (etag (progn
                             (goto-char (point-min))
                             (when (re-search-forward "^ETag: \\(.*\\)$" nil t)
                               (match-string 1))))
                     (remain (progn
                               (goto-char (point-min))
                               (when (re-search-forward "^X-RateLimit-Remaining: \\(.*\\)$" nil t)
                                 (match-string 1))))
                     (reset (progn
                              (goto-char (point-min))
                              (when (re-search-forward "^X-RateLimit-Reset: \\(.*\\)$" nil t)
                                (match-string 1)))))

                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "Response: status=%s, etag=%s" status (or etag "none")))

                (cond
                 ;; 200 OK - parse JSON and cache it
                 ((eq status 200)
                  ;; Find JSON start (after headers) and let url.el handle decompression  
                  (unless (re-search-forward "^$" nil t)
                    (error "Could not find end of headers"))
                  ;; Check if there's content to parse before calling json-read
                  (if (>= (point) (point-max))
                      (progn
                        (when (fboundp 'shipit--debug-log)
                          (shipit--debug-log "❌ No JSON content to parse - empty response body"))
                        (error "No JSON content in response"))
                    ;; CRITICAL FIX: Decode UTF-8 from HTTP response before parsing JSON
                    ;; The buffer contains raw UTF-8 bytes interpreted as unibyte/Latin-1 characters
                    ;; We need to convert them to proper Unicode strings
                    (let* ((body-start (point))
                           (body-end (point-max))
                           (charset (shipit-gh-etag--extract-charset-from-headers))
                           (json-data nil))

                      ;; Log the charset we detected
                      (when (fboundp 'shipit--debug-log)
                        (shipit--debug-log "🔤 Detected charset from Content-Type: %s" charset))

                      ;; Decode the response body from the detected charset
                      ;; This converts the unibyte UTF-8 bytes to proper Unicode characters
                      (condition-case err
                          (progn
                            ;; Apply charset decoding to the body region
                            ;; This transforms unibyte UTF-8 bytes to proper multibyte Unicode
                            (decode-coding-region body-start body-end charset)

                            (when (fboundp 'shipit--debug-log)
                              (shipit--debug-log "✅ Successfully decoded HTTP response body using %s" charset))

                            ;; Now parse the properly-decoded JSON
                            (let* ((json-object-type 'alist)
                                   (json-array-type 'list)
                                   (json-null nil)
                                   (json-false nil))
                              (goto-char body-start)
                              (setq json-data (condition-case err
                                               (json-read)
                                             (error
                                              (when (fboundp 'shipit--debug-log)
                                                (shipit--debug-log "❌ JSON parsing error after UTF-8 decoding: %s" (error-message-string err))
                                                (shipit--debug-log "❌ Buffer position: %d, buffer size: %d" (point) (buffer-size))
                                                (shipit--debug-log "❌ Content at point (first 200 chars): %S"
                                                                    (buffer-substring (point) (min (+ (point) 200) (point-max)))))
                                              (signal (car err) (cdr err)))))))
                        (error
                         (when (fboundp 'shipit--debug-log)
                           (shipit--debug-log "❌ UTF-8 decoding failed: %s" (error-message-string err)))
                         (signal (car err) (cdr err))))

                      (when (fboundp 'shipit--debug-log)
                        (shipit--debug-log "✅ JSON parsed, caching data"))

                    ;; Cache the data (only if ETag is valid)
                    (when (and etag
                               (stringp etag)
                               (> (length etag) 4)
                               (not (string= etag "W/\"\"")))
                      (puthash cache-key
                               (list :etag etag
                                     :json json-data
                                     :timestamp (float-time))
                               shipit-gh-etag--persistent-cache)
                      ;; Save cache to disk after update
                      (shipit-gh-etag--save-cache))

                    (list :status 200 :json json-data :etag etag :from-cache nil
                          :rate-remaining (and remain (string-to-number remain))
                          :rate-reset reset))))

                 ;; 304 Not Modified - url.el found cache data we weren't aware of
                 ((eq status 304)
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "🔄 Fresh request got 304 - cache mismatch detected")
                    (shipit--debug-log "   url.el has cached data, but our ETag cache is empty")
                    (shipit--debug-log "   Clearing url.el cache and retrying to get fresh data"))
                  
                  ;; Clear url.el cache for this URL and retry
                  (let ((cache-file (url-cache-create-filename url)))
                    (when (file-exists-p cache-file)
                      (delete-file cache-file)
                      (when (fboundp 'shipit--debug-log)
                        (shipit--debug-log "🧹 Deleted url.el cache file: %s" cache-file))))
                  
                  ;; Retry the request with clean cache
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "🔄 Retrying request after cache clear"))
                  (kill-buffer buf)
                  (shipit-gh-etag-get-json endpoint params token force-fresh))

                 ;; 429 Too Many Requests - return cached data or nil
                 ((eq status 429)
                  (when (fboundp 'shipit--debug-log)
                    (shipit--debug-log "⚠️ Rate limited (429) on fresh request for: %s" endpoint))
                  (let ((cached-entry (gethash cache-key shipit-gh-etag--persistent-cache)))
                    (if cached-entry
                        (progn
                          (when (fboundp 'shipit--debug-log)
                            (shipit--debug-log "⚠️ Returning cached data for rate-limited endpoint"))
                          (list :status 304
                                :json (plist-get cached-entry :json)
                                :etag (plist-get cached-entry :etag)
                                :from-cache t))
                      (when (fboundp 'shipit--debug-log)
                        (shipit--debug-log "⚠️ No cached data available for rate-limited endpoint"))
                      nil)))

                 (t
                  (error "%s" (shipit-gh-etag--format-http-error status endpoint))))))
          (error
           (when (fboundp 'shipit--debug-log)
             (shipit--debug-log "❌ Response processing failed: %s" (error-message-string err)))
           (error "Request failed: %s" (error-message-string err))))))))))

;;;###autoload
(defun shipit-clear-etag-cache ()
  "Clear the persistent ETag cache and url.el's GitHub cache files.
Preserves user read-state entries (activities-read:, comments-seen:, etc.)."
  (interactive)
  (shipit-gh-etag--clear-api-cache-only)
  ;; Also clear url.el's file cache which stores Last-Modified dates
  (shipit-clear-stale-cache-files)
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "✅ Cleared persistent ETag cache and url.el cache"))
  (message "Cleared shipit ETag cache and url.el cache"))

;;;###autoload
(defun shipit-clear-url-cache ()
  "Clear url.el cache that might interfere with ETag management."
  (interactive)
  (let ((cache-dir (expand-file-name "shipit-cache" temporary-file-directory)))
    (when (file-directory-p cache-dir)
      (delete-directory cache-dir t)
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log "✅ Cleared url.el cache directory: %s" cache-dir))
      (message "Cleared url.el cache directory"))))

;;;###autoload
(defun shipit-clear-etag-cache-for-repo (repo)
  "Clear ETag cache entries for a specific REPO (format: 'owner/repo')."
  (interactive "sRepo (owner/repo): ")
  (let ((cleared-count 0))
    (maphash (lambda (cache-key _value)
               (when (string-prefix-p repo cache-key)
                 (remhash cache-key shipit-gh-etag--persistent-cache)
                 (setq cleared-count (1+ cleared-count))))
             shipit-gh-etag--persistent-cache)
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "✅ Cleared %d ETag cache entries for repo: %s" cleared-count repo))
    (message "Cleared %d ETag cache entries for repo: %s" cleared-count repo)))

;;;###autoload
(defun shipit-clean-invalid-etag-cache ()
  "Remove invalid/empty ETag cache entries that might cause hanging."
  (interactive)
  (let ((cleaned-count 0))
    (maphash (lambda (cache-key value)
               (let ((etag (plist-get value :etag)))
                 (when (or (not etag)
                           (not (stringp etag))
                           (<= (length etag) 4)
                           (string= etag "W/\"\""))
                   (remhash cache-key shipit-gh-etag--persistent-cache)
                   (setq cleaned-count (1+ cleaned-count)))))
             shipit-gh-etag--persistent-cache)
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "✅ Cleaned %d invalid ETag cache entries" cleaned-count))
    (message "Cleaned %d invalid ETag cache entries" cleaned-count)))

;;;###autoload
(defun shipit-clear-etag-cache-for-endpoint (endpoint)
  "Clear ETag cache entry for a specific ENDPOINT."
  ;; CRITICAL: Use same cache-key format as shipit-gh-etag-get-json
  ;; Cache key is: repo + endpoint (if repo found in endpoint), otherwise just endpoint
  (let* ((repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                       (match-string 1 endpoint)))
         (cache-key (if repo-match
                        (concat repo-match endpoint)
                      endpoint)))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🧹 CLEAR-ETAG: Attempting to clear cache for endpoint: %s" endpoint)
      (shipit--debug-log "🧹 CLEAR-ETAG: Extracted repo: %s" (or repo-match "NONE"))
      (shipit--debug-log "🧹 CLEAR-ETAG: Cache-key lookup: %s" cache-key)
      (shipit--debug-log "🧹 CLEAR-ETAG: Cache contains entry: %s" (if (gethash cache-key shipit-gh-etag--persistent-cache) "YES" "NO")))

    (when (gethash cache-key shipit-gh-etag--persistent-cache)
      (remhash cache-key shipit-gh-etag--persistent-cache)
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log "✅ CLEAR-ETAG: Removed persistent cache entry for: %s" endpoint)))

    ;; Also clear from refresh cache if it exists
    (when (and shipit-gh-etag--refresh-cache
               (gethash cache-key shipit-gh-etag--refresh-cache))
      (remhash cache-key shipit-gh-etag--refresh-cache)
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log "✅ CLEAR-ETAG: Removed refresh cache entry for: %s" endpoint)))

    ;; Also clear url.el cache to prevent cache file errors
    (condition-case err
        (let ((full-url (concat "https://api.github.com" endpoint)))
          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "🧹 CLEAR-ETAG: Clearing url.el cache for: %s" full-url))
          (when (fboundp 'url-cache-create-filename)
            (let ((cache-file (url-cache-create-filename full-url)))
              (when (fboundp 'shipit--debug-log)
                (shipit--debug-log "🧹 CLEAR-ETAG: url.el cache file: %s (exists: %s)" cache-file (if (file-exists-p cache-file) "YES" "NO")))
              (when (file-exists-p cache-file)
                (delete-file cache-file)
                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "🗑️ CLEAR-ETAG: Deleted url.el cache file: %s" cache-file))))))
      (error
       (when (fboundp 'shipit--debug-log)
         (shipit--debug-log "⚠️ CLEAR-ETAG: Failed to clear url.el cache file: %s" (error-message-string err)))))))

;;;###autoload
(defun shipit-gh-etag-clear-all ()
  "Clear ALL ETag-related caches (persistent cache and refresh cache).
Used by hard refresh to ensure completely fresh data from GitHub API.
Preserves user read-state entries (activities-read:, comments-seen:, etc.)."
  (interactive)
  ;; Clear the persistent ETag cache (preserving user state)
  (shipit-gh-etag--clear-api-cache-only)
  ;; Clear the temporary refresh cycle cache
  (shipit-gh-etag--clear-refresh-cache)
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "✅ Cleared ALL ETag caches (persistent + refresh)")))

;;;###autoload
(defun shipit-gh-etag-invalidate-endpoint (endpoint)
  "Invalidate a specific ENDPOINT from all ETag caches.
ENDPOINT should be the API path like \"/repos/owner/repo/pulls/123\".
Cache keys have format \"owner/repo/repos/owner/repo/pulls/123\" so we
search for keys containing the endpoint."
  (let ((removed-persistent 0)
        (removed-refresh 0))
    ;; Remove from persistent cache - check all keys that CONTAIN this endpoint
    ;; (cache keys are prefixed with repo, e.g., "owner/repo/repos/owner/repo/pulls/123")
    (maphash (lambda (key _value)
               (when (string-match-p (regexp-quote endpoint) key)
                 (remhash key shipit-gh-etag--persistent-cache)
                 (setq removed-persistent (1+ removed-persistent))))
             shipit-gh-etag--persistent-cache)
    ;; Remove from refresh cache if it exists
    (when shipit-gh-etag--refresh-cache
      (maphash (lambda (key _value)
                 (when (string-match-p (regexp-quote endpoint) key)
                   (remhash key shipit-gh-etag--refresh-cache)
                   (setq removed-refresh (1+ removed-refresh))))
               shipit-gh-etag--refresh-cache))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🗑️ Invalidated endpoint %s: %d persistent, %d refresh entries"
                         endpoint removed-persistent removed-refresh))))

;;;###autoload
(defun shipit-clear-stale-cache-files ()
  "Clean up any stale url.el cache files that might cause errors.
This clears GitHub API cache files that may contain locale-specific dates
which cause 'Multibyte text in HTTP request' errors."
  (interactive)
  (condition-case err
      (let ((cache-dir (expand-file-name "url/cache" user-emacs-directory))
            (cleaned 0))
        (when (file-directory-p cache-dir)
          ;; Look for GitHub cache files - match path containing "github"
          (dolist (file (directory-files-recursively cache-dir "." t))
            (when (and (file-regular-p file)
                       (string-match-p "github" file))
              (condition-case file-err
                  (progn
                    (delete-file file)
                    (setq cleaned (1+ cleaned)))
                (error
                 (when (fboundp 'shipit--debug-log)
                   (shipit--debug-log "⚠️ Failed to delete cache file %s: %s" file (error-message-string file-err)))))))
          (when (> cleaned 0)
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "🗑️ Cleaned %d stale GitHub API cache files" cleaned))
            (message "Cleaned %d stale cache files" cleaned))))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "⚠️ Failed to clean cache files: %s" (error-message-string err)))
     (message "Failed to clean cache files: %s" (error-message-string err)))))

;;;###autoload 
(defun shipit-fix-cache-file-errors ()
  "Fix persistent cache file errors by comprehensive cleanup and reset.
Use this when encountering 'Opening input file: No such file or directory' errors."
  (interactive)
  (let ((total-cleared 0))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🔧 CACHE FIX: Starting comprehensive cache cleanup"))
    
    ;; Step 1: Clear all ETag caches
    (clrhash shipit-gh-etag--persistent-cache)
    (setq shipit-gh-etag--refresh-cache nil)
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🔧 CACHE FIX: Cleared ETag caches"))
    
    ;; Step 2: Remove all GitHub API cache files from url.el
    (condition-case err
        (let ((cache-dir (expand-file-name "url/cache" user-emacs-directory)))
          (when (file-directory-p cache-dir)
            (dolist (file (directory-files-recursively cache-dir ".*" t))
              (when (and (file-regular-p file)
                         (or (string-match-p "github" file)
                             (string-match-p "api" file)))
                (condition-case del-err
                    (progn
                      (delete-file file)
                      (setq total-cleared (1+ total-cleared)))
                  (error
                   (when (fboundp 'shipit--debug-log)
                     (shipit--debug-log "⚠️ CACHE FIX: Failed to delete %s: %s" 
                                        file (error-message-string del-err)))))))))
      (error
       (when (fboundp 'shipit--debug-log)
         (shipit--debug-log "⚠️ CACHE FIX: Directory cleanup failed: %s" (error-message-string err)))))
    
    ;; Step 3: Re-enable url.el automatic caching and ensure locale is set for ASCII dates
    (setq url-automatic-caching t)
    (setq system-time-locale "C")
    
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🔧 CACHE FIX: Cleaned %d files, reset url.el caching" total-cleared))
    
    (message "Cache fix complete: cleaned %d files and reset caching system" total-cleared)))

;;;###autoload
(defun shipit-show-etag-cache-stats ()
  "Show statistics about the persistent ETag cache."
  (interactive)
  (let ((total-entries (hash-table-count shipit-gh-etag--persistent-cache))
        (repos (make-hash-table :test 'equal)))
    (maphash (lambda (cache-key _value)
               (when (string-match "^\\([^/]+/[^/]+\\)" cache-key)
                 (let ((repo (match-string 1 cache-key)))
                   (puthash repo (1+ (gethash repo repos 0)) repos))))
             shipit-gh-etag--persistent-cache)

    (message "ETag cache: %d total entries across %d repos"
             total-entries (hash-table-count repos))

    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "ETag cache stats: %d entries, %d repos"
                         total-entries (hash-table-count repos)))

    (when (> (hash-table-count repos) 0)
      (maphash (lambda (repo count)
                 (message "  %s: %d cached endpoints" repo count))
               repos))))

;; Auto-save ETag cache on Emacs exit
(add-hook 'kill-emacs-hook #'shipit-gh-etag--save-cache)

;;;###autoload
(defun shipit-save-etag-cache ()
  "Manually save the ETag cache to disk."
  (interactive)
  (shipit-gh-etag--save-cache)
  (message "ETag cache saved to %s" shipit-gh-etag--cache-file))

;;;###autoload
(defun shipit-load-etag-cache ()
  "Manually load the ETag cache from disk."
  (interactive)
  (let ((count (shipit-gh-etag--load-cache)))
    (message "ETag cache loaded: %d entries from %s" 
             (or count 0) shipit-gh-etag--cache-file)))

;;;###autoload
(defun shipit-gh-etag-get-json-with-refresh-cache (endpoint &optional params token force-fresh)
  "GET GitHub REST JSON with refresh-level deduplication.
This wrapper around `shipit-gh-etag-get-json` adds request deduplication
during a single refresh cycle to avoid multiple network requests for the same
endpoint within the same refresh."
  (let* ((qstr (shipit-gh-etag--qstr params))
         (repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                       (match-string 1 endpoint)))
         (cache-key (if repo-match
                        (concat repo-match endpoint (or qstr ""))
                        (concat endpoint (or qstr ""))))
         (refresh-cached (when shipit-gh-etag--refresh-cache
                           (gethash cache-key shipit-gh-etag--refresh-cache))))
    
    ;; Check refresh cache first - return immediately if already requested this refresh
    (if (and refresh-cached (not force-fresh))
        (progn
          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "⚡ REFRESH CACHE HIT: %s (skipped network request)" cache-key))
          refresh-cached)
      
      ;; Not in refresh cache - make request and cache result
      (let ((result (shipit-gh-etag-get-json endpoint params token force-fresh)))
        ;; Store result in refresh cache if cache is active
        (when shipit-gh-etag--refresh-cache
          (puthash cache-key result shipit-gh-etag--refresh-cache)
          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "⚡ REFRESH CACHE STORE: %s" cache-key)))
        result))))

;;;###autoload
(defun shipit-gh-etag--init-refresh-cache ()
  "Initialize the refresh-level request deduplication cache."
  (setq shipit-gh-etag--refresh-cache (make-hash-table :test 'equal))
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "⚡ REFRESH CACHE: Initialized for new refresh cycle")))

;;;###autoload
(defun shipit-gh-etag--clear-refresh-cache ()
  "Clear the refresh-level request deduplication cache."
  (when shipit-gh-etag--refresh-cache
    (let ((count (hash-table-count shipit-gh-etag--refresh-cache)))
      (setq shipit-gh-etag--refresh-cache nil)
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log "⚡ REFRESH CACHE: Cleared (%d entries)" count)))))

;;;###autoload
(defun shipit-gh-etag--invalidate-refresh-cache (endpoint &optional params)
  "Invalidate a specific ENDPOINT from the refresh cache.
PARAMS are the query parameters used in the request.
This should be called after mutating operations to ensure subsequent
reads in the same refresh cycle get fresh data."
  (when shipit-gh-etag--refresh-cache
    (let* ((qstr (shipit-gh-etag--qstr params))
           (repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                         (match-string 1 endpoint)))
           (cache-key (if repo-match
                          (concat repo-match endpoint (or qstr ""))
                        (concat endpoint (or qstr "")))))
      (when (gethash cache-key shipit-gh-etag--refresh-cache)
        (remhash cache-key shipit-gh-etag--refresh-cache)
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "⚡ REFRESH CACHE: Invalidated %s" cache-key))))))

;;; Parallel request batching for performance optimization

;;;###autoload
(defun shipit-gh-etag-get-json-batch-sync (requests)
  "Fetch multiple GitHub REST JSON endpoints synchronously with ETag caching.

REQUESTS should be a list of request specs:
  ((endpoint1 params1 token1) (endpoint2 params2 token2) ...)

Returns an alist: ((endpoint1 . result1) (endpoint2 . result2) ...)

Each result follows the same format as shipit-gh-etag-get-json-with-refresh-cache.
The benefit is eliminating redundant requests via refresh-level caching and providing 
a clean batch interface."
  (let ((batch-id (format "batch-%d" (random 100000)))
        (results '()))
    
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🚀 SYNC BATCH START: %s (%d requests)" batch-id (length requests)))
    
    ;; Execute all requests sequentially and collect results
    (dolist (request requests)
      (let* ((endpoint (nth 0 request))
             (params (nth 1 request))
             (token (nth 2 request)))
        
        (condition-case err
            (let ((result (shipit-gh-etag-get-json-with-refresh-cache endpoint params token)))
              (push (cons endpoint result) results)
              (when (fboundp 'shipit--debug-log)
                (shipit--debug-log "✅ SYNC BATCH: %s completed endpoint %s" batch-id endpoint)))
          (error 
           (when (fboundp 'shipit--debug-log)
             (shipit--debug-log "❌ SYNC BATCH ERROR: %s endpoint %s: %S" batch-id endpoint err))
           (push (cons endpoint `(:error . ,err)) results)))))
    
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🏁 SYNC BATCH COMPLETE: %s (%d requests)" batch-id (length requests)))
    
    ;; Return results in original order (reverse because we used push)
    (reverse results)))

;;; Asynchronous API requests for background operations

;;;###autoload
(defun shipit-gh-etag-get-json-async (endpoint params token callback &optional force-fresh)
  "Fetch GitHub REST JSON asynchronously with ETag caching.
CALLBACK will be called with the result when the request completes: (callback result)
Result format is the same as shipit-gh-etag-get-json-with-refresh-cache."
  ;; Ensure persistent cache is loaded from disk
  (shipit-gh-etag--ensure-cache-loaded)

  ;; CRITICAL: Set system-time-locale to "C" GLOBALLY for async requests
  ;; Must be set globally because url-retrieve is async and runs outside let scope
  ;; This ensures HTTP date headers (If-Modified-Since) use ASCII characters
  ;; instead of locale-specific names (e.g., "sø." for Sunday in Norwegian)
  ;; which cause "Multibyte text in HTTP request" errors
  (setq system-time-locale "C")

  ;; CRITICAL FIX: Proactively clean up any problematic cache files before starting async request
  (shipit-gh-etag--proactive-cache-cleanup endpoint params)

  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log "🔍 ASYNC ETag request: endpoint=%S force-fresh=%s" endpoint force-fresh))
  
  (let* ((qstr (shipit-gh-etag--qstr params))
         (repo-match (when (string-match "/repos/\\([^/]+/[^/]+\\)" endpoint)
                       (match-string 1 endpoint)))
         (cache-key (if repo-match
                        (concat repo-match endpoint (or qstr ""))
                      (concat endpoint (or qstr ""))))
         (url (concat shipit-api-url endpoint (or qstr "")))
         (cached-entry (when (not force-fresh)
                         (gethash cache-key shipit-gh-etag--persistent-cache)))
         (stored-etag (when cached-entry
                        (let ((etag (plist-get cached-entry :etag)))
                          ;; Only use ETags that are not empty or just quotes (same as sync)
                          (when (and etag
                                   (stringp etag)
                                   (> (length etag) 4)  ; More than just W/""
                                   (not (string= etag "W/\"\"")))
                            etag))))
         (url-request-method "GET")
         (url-request-extra-headers
          (append (list '("User-Agent" . "emacs-url")
                        '("Accept" . "application/vnd.github+json"))
                  (when stored-etag
                    (list `("If-None-Match" . ,stored-etag)))
                  (let ((resolved (or token (shipit--github-token))))
                    (when resolved
                      (list `("Authorization" . ,(format "Bearer %s" resolved)))))))
         ;; Enable url.el caching for ETag functionality
         (url-automatic-caching t)
         (url-cache-directory (expand-file-name "url/cache" user-emacs-directory)))

    ;; When force-fresh=t, delete url.el cache file to ensure fresh data (not 304)
    (when force-fresh
      (let ((cache-file (url-cache-create-filename url)))
        (when (file-exists-p cache-file)
          (delete-file cache-file)
          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "🧹 FORCE-FRESH: Deleted url.el cache file: %s" cache-file)))))

    ;; Make asynchronous request
    (url-retrieve url
                  (lambda (status)
                    (shipit-gh-etag--handle-async-response 
                     status endpoint params token callback cache-key stored-etag force-fresh))
                  nil t t)))

(defun shipit-gh-etag--handle-async-response (status endpoint params token callback cache-key stored-etag force-fresh)
  "Handle asynchronous response from GitHub API.
Uses the same response parsing logic as the synchronous version."
  ;; Enable multibyte mode to properly interpret UTF-8 bytes from HTTP response
  (set-buffer-multibyte t)

  (condition-case err
      (if (plist-get status :error)
          ;; Handle error
          (progn
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "❌ ASYNC ERROR: %S" (plist-get status :error)))
            (funcall callback `(:error . ,(plist-get status :error))))
        
        ;; Use the same parsing logic as sync version - we're already in the response buffer
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "🔍 ASYNC Response buffer size: %d chars" (buffer-size)))

        ;; Manual HTTP response parsing (same as sync version)
        (goto-char (point-min))
        (let* ((status-code (when (re-search-forward "^HTTP/[12]\\(?:\\.[01]\\)? \\([0-9]+\\)" nil t)
                              (string-to-number (match-string 1))))
               (etag (progn
                       (goto-char (point-min))
                       (when (re-search-forward "^ETag: \\(.*\\)$" nil t)
                         (match-string 1))))
               (remain (progn
                         (goto-char (point-min))
                         (when (re-search-forward "^X-RateLimit-Remaining: \\(.*\\)$" nil t)
                           (match-string 1))))
               (reset (progn
                        (goto-char (point-min))
                        (when (re-search-forward "^X-RateLimit-Reset: \\(.*\\)$" nil t)
                          (match-string 1)))))

          (when (fboundp 'shipit--debug-log)
            (shipit--debug-log "🔍 ASYNC RESPONSE:")
            (shipit--debug-log "  Status: %s" status-code)
            (shipit--debug-log "  ETag: %S" (or etag "none"))
            (shipit--debug-log "  Response size: %d chars" (buffer-size)))

          (cond
           ;; 304 Not Modified - use cached data (same as sync)
           ((eq status-code 304)
            (let ((cached-entry (gethash cache-key shipit-gh-etag--persistent-cache))
                  (cached-json (when (gethash cache-key shipit-gh-etag--persistent-cache)
                                 (plist-get (gethash cache-key shipit-gh-etag--persistent-cache) :json))))
              (if cached-json
                  (progn
                    (when (fboundp 'shipit--debug-log)
                      (shipit--debug-log "✅ ASYNC 304 Not Modified - using cached data"))
                    (funcall callback (list :status 304
                                            :json cached-json
                                            :etag (plist-get cached-entry :etag)
                                            :from-cache t
                                            :rate-remaining (and remain (string-to-number remain))
                                            :rate-reset reset)))
                  ;; Cache entry missing JSON - make fresh request instead
                  (progn
                    (when (fboundp 'shipit--debug-log)
                      (shipit--debug-log "⚠️ ASYNC 304 but cache entry missing JSON - making fresh request"))
                    (kill-buffer)
                    ;; Recursively call with force-fresh to bypass cache and get fresh data
                    (shipit-gh-etag-get-json-async endpoint params token callback t)))))

           ;; 200 OK - content changed, parse new JSON (same as sync)
           ((eq status-code 200)
            ;; Find JSON start (after headers) and let url.el handle decompression
            (goto-char (point-min))
            (unless (re-search-forward "^$" nil t)
              (error "Could not find end of headers"))

            (when (> (point-max) (point))
              ;; CRITICAL FIX: Decode UTF-8 from HTTP response before parsing JSON
              ;; The buffer contains raw UTF-8 bytes interpreted as unibyte/Latin-1 characters
              ;; We need to convert them to proper Unicode strings
              (let* ((body-start (point))
                     (body-end (point-max))
                     (charset (shipit-gh-etag--extract-charset-from-headers)))

                ;; Apply charset decoding to the body region
                ;; This transforms unibyte UTF-8 bytes to proper multibyte Unicode
                (condition-case err
                    (decode-coding-region body-start body-end charset)
                  (error
                   (when (fboundp 'shipit--debug-log)
                     (shipit--debug-log "❌ ASYNC UTF-8 decoding failed: %s" (error-message-string err)))
                   (signal (car err) (cdr err))))

                (let* ((json-data (let ((json-object-type 'alist)
                                        (json-array-type 'list)
                                        (json-null nil)
                                        (json-false nil))
                                    (goto-char body-start)
                                    (json-read)))
                       (normalized-etag (shipit-gh-etag--normalize-etag etag)))
                ;; Cache the new data (same as sync)
                (when json-data
                  (puthash cache-key 
                           (list :etag normalized-etag
                                 :json json-data
                                 :timestamp (float-time))
                           shipit-gh-etag--persistent-cache))
                
                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "✅ ASYNC 200: Cached new data with ETag %S" normalized-etag))
                
                  (funcall callback (list :status 200
                                          :json json-data
                                          :etag normalized-etag
                                          :from-cache nil
                                          :rate-remaining (and remain (string-to-number remain))
                                          :rate-reset reset))))))

           (t
            ;; Other status codes
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "⚠️ ASYNC: Unexpected status %d" status-code))
            (funcall callback `(:status ,status-code :error ,(format "HTTP %d" status-code)))))))
    
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "❌ ASYNC ERROR: %S" err))
     (funcall callback `(:error . ,err)))))

(defun shipit-gh-etag--parse-link-header (link-header)
  "Parse Link header and return the 'next' URL or nil if not present.
GitHub Link headers look like: <url>; rel=\"next\", <url>; rel=\"last\""
  (when link-header
    (let ((parts (split-string link-header ", ")))
      (cl-find-if (lambda (part)
                    (string-match "rel=\"next\"" part))
                  parts
                  :key (lambda (part)
                         ;; Extract the URL from <url>; rel="next"
                         (when (string-match "^<\\([^>]+\\)>;" part)
                           part))))))

(defun shipit-gh-etag--get-next-url (link-header)
  "Extract the 'next' page URL from a Link header."
  (when link-header
    (let ((next-link (shipit-gh-etag--parse-link-header link-header)))
      (when next-link
        (when (string-match "^<\\([^>]+\\)>" next-link)
          (match-string 1 next-link))))))

;;;###autoload
(defun shipit-gh-etag-get-json-paginated (endpoint &optional params token)
  "GET GitHub REST JSON at ENDPOINT with automatic pagination.
Accumulates all pages into a single JSON array, using ETag caching for each page.
This handles the case where GitHub returns >100 items per page."
  (let ((all-results '())
        (current-url endpoint)
        (current-params params)
        (page-count 0))

    (catch 'pagination-complete
      (while current-url
        (setq page-count (+ page-count 1))
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "📄 PAGINATION: Fetching page %d from %s" page-count current-url))

        ;; Fetch current page
        (let* ((result (shipit-gh-etag-get-json-with-refresh-cache
                        current-url current-params token))
               (json-data (plist-get result :json))
               (status (plist-get result :status)))

          (unless (member status '(200 304))
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "❌ PAGINATION: Unexpected status %s on page %d" status page-count))
            (throw 'pagination-complete all-results))

          ;; Accumulate results
          (when (and json-data (listp json-data))
            (setq all-results (append all-results json-data))
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "📄 PAGINATION: Page %d added %d items (total: %d)"
                                page-count (length json-data) (length all-results))))

          ;; Check for next page
          ;; GitHub returns up to 100 items per page, so if we got 100, try next page
          (if (and json-data (>= (length json-data) 100))
              ;; Likely more pages - prepare params for next page
              (let ((next-page (+ page-count 1)))
                (setq current-url endpoint)
                ;; Update params with page number
                (let ((new-params (copy-alist (or current-params '()))))
                  (setf (alist-get 'page new-params) next-page)
                  (setf (alist-get 'per_page new-params) 100)
                  (setq current-params new-params)))
            ;; Less than 100 items - we've reached the last page
            (setq current-url nil)))))

    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "✅ PAGINATION: Complete - fetched %d pages, %d total items"
                        page-count (length all-results)))

    ;; Return in same format as shipit-gh-etag-get-json for consistency
    (list :status 200
          :json all-results
          :from-pagination t
          :page-count page-count
          :total-items (length all-results))))

;;;###autoload
(defun shipit-gh-etag-get-json-paginated-async (endpoint params token callback &optional force-fresh)
  "Asynchronously fetch all pages from GitHub REST API endpoint.
Accumulates all pages into a single JSON array and calls CALLBACK with the result.
This handles PRs with >100 changed files by making multiple async requests.

ENDPOINT is the GitHub API endpoint.
PARAMS are the query parameters (page/per_page will be added automatically).
TOKEN is the GitHub API token.
CALLBACK is called with a plist result when all pages are fetched.
Result format: (:status STATUS :json DATA :page-count N :total-items N)
FORCE-FRESH bypasses ETag cache to force fresh data."
  (let ((all-results '())
        (current-page 1)
        (per-page 100)
        (page-count 0))

    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "🔄 ASYNC PAGINATION: Starting for %s" endpoint))

    ;; Define recursive function using letrec for self-reference
    (letrec ((fetch-next-page
              (lambda ()
                (setq page-count (1+ page-count))
                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "📄 ASYNC PAGINATION: Fetching page %d" current-page))

                ;; Prepare params with page number
                (let ((page-params (append params
                                          `((page . ,current-page)
                                            (per_page . ,per-page)))))

                  ;; Fetch current page asynchronously
                  (shipit-gh-etag-get-json-async
                   endpoint
                   page-params
                   token
                   (lambda (result)
                     ;; Check for error response
                     (let ((error-val (plist-get result :error))
                           (status-code (plist-get result :status)))
                       (cond
                        ;; Error response from async call
                        (error-val
                         (when (fboundp 'shipit--debug-log)
                           (shipit--debug-log "❌ ASYNC PAGINATION: Error on page %d: %S"
                                             current-page error-val))
                         (message "Pagination error on page %d: %s" current-page error-val)
                         (funcall callback (list :status 500
                                                 :json all-results
                                                 :error error-val
                                                 :page-count (- current-page 1)
                                                 :partial t)))
                        ;; HTTP error status
                        ((and status-code (>= status-code 400))
                         (when (fboundp 'shipit--debug-log)
                           (shipit--debug-log "❌ ASYNC PAGINATION: HTTP %d on page %d"
                                             status-code current-page))
                         (message "Pagination HTTP error on page %d: %d" current-page status-code)
                         (funcall callback (list :status status-code
                                                 :json all-results
                                                 :page-count (- current-page 1)
                                                 :partial t)))
                        ;; Success - continue normal processing
                        (t
                         ;; Extract JSON data from result plist
                         (let* ((page-json (plist-get result :json))
                                (page-item-count (length page-json)))

                           (when (fboundp 'shipit--debug-log)
                             (shipit--debug-log "📄 ASYNC PAGINATION: Page %d fetched %d items"
                                               current-page page-item-count))

                           ;; Accumulate results - use append to avoid circular references
                           (setq all-results (append all-results page-json))

                           ;; Check if more pages exist
                           (if (>= page-item-count per-page)
                               ;; More pages likely exist - fetch next page
                               (progn
                                 (setq current-page (1+ current-page))
                                 (funcall fetch-next-page))
                             ;; Last page reached - call final callback with plist format
                             (progn
                               (when (fboundp 'shipit--debug-log)
                                 (shipit--debug-log "✅ ASYNC PAGINATION: Complete - %d pages, %d total items"
                                                   page-count (length all-results)))
                               (funcall callback (list :status 200
                                                       :json all-results
                                                       :from-pagination t
                                                       :page-count page-count
                                                       :total-items (length all-results))))))))))
                   force-fresh)))))

      ;; Start fetching from page 1
      (funcall fetch-next-page))))

(defun shipit-gh-etag--format-http-error (status endpoint)
  "Format an HTTP error message for STATUS and ENDPOINT.
When STATUS is 404 and no GitHub token is configured, hints that
authentication may be needed (GitHub returns 404 for private repos
when unauthenticated)."
  (if (and (eq status 404)
           (not (or shipit-github-token
                    (shipit--github-token))))
      (format "HTTP 404 for %s — no GitHub token configured.  Set `shipit-github-token' or add github.com to auth-source"
              endpoint)
    (format "HTTP %s for endpoint: %s" status endpoint)))

(provide 'shipit-gh-etag)
