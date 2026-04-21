;;; shipit-issue-titles.el --- Shared issue title cache for eldoc previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Low-level helpers shared by eldoc integrations that display issue
;; summaries in the echo area: a per-session title cache keyed by
;; (REPO . ID-STRING), a deferred backend fetcher, and a one-line
;; summary formatter.
;;
;; Two callers currently use this:
;;   - `shipit-render' overlays in PR/issue/discussion buffers
;;   - `shipit-code-refs' text properties in source-code comments

;;; Code:

(require 'shipit-core)
(require 'shipit-issue-backends)

(defvar shipit--issue-title-cache (make-hash-table :test 'equal)
  "Session cache of issue titles keyed by (REPO . ID-STRING).
Values are plists: (:status STATUS :title STR :state STR :assignee STR).
STATUS is one of `loading', `fresh', or `failed'.")

(defun shipit--issue-cache-key (repo id)
  "Build cache key from REPO and ID (integer or string)."
  (cons repo (if (integerp id) (number-to-string id) id)))

(defun shipit--format-issue-eldoc (id entry)
  "Render a one-line eldoc summary for ID from cache ENTRY."
  (let* ((display-id (if (integerp id) (format "#%d" id) (format "%s" id)))
         (state (plist-get entry :state))
         (title (or (plist-get entry :title) ""))
         (assignee (plist-get entry :assignee))
         (state-part (if (and state (> (length state) 0))
                         (format " [%s]" state) ""))
         (assignee-part (if (and assignee (> (length assignee) 0))
                            (format " — @%s" assignee) "")))
    (format "%s%s %s%s" display-id state-part title assignee-part)))

(defun shipit--schedule-issue-title-fetch (repo id refresh-cb)
  "Fetch title for ID in REPO asynchronously; call REFRESH-CB when done.
Dispatches to the backend's `:fetch-issue' via
`shipit-issue--resolve-for-repo', populates `shipit--issue-title-cache',
and defers the (currently synchronous) HTTP call via `run-at-time' so
it runs off the idle timer that invoked eldoc."
  (run-at-time
   0 nil
   (lambda ()
     (let ((key (shipit--issue-cache-key repo id)))
       (condition-case err
           (let* ((resolved (shipit-issue--resolve-for-repo repo))
                  (backend (car resolved))
                  (config (cdr resolved))
                  (fetch-fn (plist-get backend :fetch-issue))
                  (data (funcall fetch-fn config id))
                  (title (cdr (assq 'title data)))
                  (state (cdr (assq 'state data)))
                  (assignee (cdr (assq 'login (cdr (assq 'user data))))))
             (puthash key (list :status 'fresh
                                :title title
                                :state state
                                :assignee assignee)
                      shipit--issue-title-cache))
         (error
          (shipit--debug-log "Issue title fetch failed for %s %s: %s" repo id err)
          (puthash key (list :status 'failed) shipit--issue-title-cache))))
     (when refresh-cb (funcall refresh-cb)))))

(defun shipit--issue-title-eldoc-doc (id repo refresh-cb)
  "Return an eldoc doc-string for ID in REPO.
Returns cached one-liner if fresh, \"Loading…\" placeholder while the
async fetch is in flight (kicked off on first visit), or nil when the
fetch failed.  REFRESH-CB is invoked after an async fetch completes so
the caller can re-trigger eldoc."
  (when (and id repo)
    (let* ((key (shipit--issue-cache-key repo id))
           (entry (gethash key shipit--issue-title-cache)))
      (pcase (plist-get entry :status)
        ('fresh (shipit--format-issue-eldoc id entry))
        ('loading (format "Loading %s…"
                          (if (integerp id) (format "#%d" id) id)))
        ('failed nil)
        (_
         (puthash key '(:status loading) shipit--issue-title-cache)
         (shipit--schedule-issue-title-fetch repo id refresh-cb)
         (format "Loading %s…"
                 (if (integerp id) (format "#%d" id) id)))))))

(provide 'shipit-issue-titles)
;;; shipit-issue-titles.el ends here
