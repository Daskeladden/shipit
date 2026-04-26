;;; shipit-cache.el --- cache module -*- lexical-binding: t; -*-

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

;;; This file is part of shipit — code review integration for Magit.

;;; Commentary:
;; cache internals split from monolithic shipit.el

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(defvar shipit--reaction-cache)
(defvar shipit--reaction-fetch-in-progress)
(defvar shipit--general-comments-fetched)
(defvar shipit--inline-comments-fetched)
(defvar shipit--pr-displayed-in-buffer)
(defvar shipit--codeowners-cache)

;; Forward declarations for rendering config variables from shipit-core
(defvar shipit-inline-comment-highlight-background)
(defvar shipit-inline-comment-icon-style)
(defvar shipit-show-avatars)
(defvar shipit-render-markdown)
(defvar shipit-inline-comment-left-border)

(declare-function shipit--debug-log "shipit-debug")
(declare-function shipit--get-repo-from-remote "shipit-core")
(declare-function shipit-clear-etag-cache-for-repo "shipit-gh-etag")
(declare-function shipit-show-etag-cache-stats "shipit-gh-etag")
(declare-function shipit-clear-url-cache "shipit-gh-etag")
(declare-function shipit--clear-branch-cache "shipit-checks")
(declare-function shipit-buffer-refresh "shipit-buffer")
(declare-function magit-refresh "magit-mode")
(declare-function shipit-comment--get-backend "shipit-comment-backends")

(defun shipit--clear-backend-discussions-cache ()
  "Dispatch discussions cache clearing to the active comment backend.
Calls the backend's :clear-discussions-cache if available."
  (ignore-errors
    (let* ((backend (shipit-comment--get-backend))
           (clear-fn (plist-get backend :clear-discussions-cache)))
      (when clear-fn
        (funcall clear-fn)))))

(defvar-local shipit--cached-general-comments nil
  "Cached general comments for the current PR buffer.")

(defvar-local shipit--cached-pr-reactions nil
  "Cached PR reactions for the current PR buffer.")

(defvar-local shipit--cached-pr-checks nil
  "Cached PR checks for the current PR buffer.")

(defvar-local shipit--cached-inline-comments nil
  "Cached inline comments for the current PR buffer.")

(defvar-local shipit--cached-resolved-threads nil
  "Cached resolved threads for the current PR buffer.")

(defvar-local shipit--cached-review-decision nil
  "Cached review decision data for the current PR buffer.")


(defvar shipit--cached-branch-prs (make-hash-table :test 'equal)
  "Hash table caching PR data for different branches.
Global cache with repo-specific keys.")

(defvar shipit--workflow-name-cache (make-hash-table :test 'equal)
  "Cache mapping workflow IDs to workflow names. Global cache.")

(defvar shipit--cached-approval-status (make-hash-table :test 'equal)
  "Hash table caching PR approval status. Global cache with repo-specific keys.
Key format: \='repo:pr-number\=' → Value: approval status string")

(defun shipit--ensure-cache-initialized ()
  "Ensure all cache hash tables are properly initialized.
This function should be called before accessing any cache to prevent nil errors."
  ;; These caches remain as simple buffer-local lists (no conversion needed)
  ;; shipit--cached-general-comments - remains nil or list
  ;; shipit--cached-pr-reactions - remains nil or list  
  ;; shipit--cached-pr-checks - remains nil or list
  ;; shipit--cached-inline-comments - remains nil or list
  
  ;; Hash table caches are now global and initialized at load time
  ;; No need to initialize them here, but keep defensive checks
  (unless (hash-table-p shipit--cached-approval-status)
    (setq shipit--cached-approval-status (make-hash-table :test 'equal)))
  (unless (hash-table-p shipit--cached-branch-prs)
    (setq shipit--cached-branch-prs (make-hash-table :test 'equal)))
  (unless (hash-table-p shipit--workflow-name-cache)
    (setq shipit--workflow-name-cache (make-hash-table :test 'equal))))

(defun shipit--clear-repo-reaction-cache (repo)
  "Clear all reaction cache entries for a specific repository REPO."
  (when (and repo (boundp 'shipit--reaction-cache))
    (let ((repo-prefix (format "%s:" repo))
          (cleared-count 0))
      (maphash (lambda (cache-key _value)
                 (when (string-prefix-p repo-prefix cache-key)
                   (remhash cache-key shipit--reaction-cache)
                   (setq cleared-count (1+ cleared-count))))
               shipit--reaction-cache)
      (shipit--debug-log "Cleared %d reaction cache entries for repo %s" cleared-count repo)))
  ;; DON'T clear fetch-in-progress flags - this breaks async reaction fetching
  ;; The async system manages its own fetch-in-progress state
  (shipit--debug-log "Skipping reaction fetch-in-progress clearing for repo %s to preserve async fetch state" repo))

(defun shipit--clear-general-comments-cache ()
  "Clear general comments cache and their associated reactions cache."
  (interactive)
  ;; Clear list cache
  (setq shipit--cached-general-comments nil
        shipit--general-comments-fetched nil)
  ;; Clear backend-specific discussions cache via dispatch
  (shipit--clear-backend-discussions-cache)
  ;; Clear reaction cache for current repository only
  (when (boundp 'shipit-current-repo)
    (shipit--clear-repo-reaction-cache shipit-current-repo))
  (message "General comments and reactions cache cleared"))

(defun shipit--clear-inline-comments-cache ()
  "Clear inline comments cache and their associated reactions cache."
  (interactive)
  ;; Clear list cache
  (setq shipit--cached-inline-comments nil
        shipit--inline-comments-fetched nil)
  ;; Clear backend-specific discussions cache via dispatch
  (shipit--clear-backend-discussions-cache)
  ;; Clear reaction cache for current repository only
  (when (boundp 'shipit-current-repo)
    (shipit--clear-repo-reaction-cache shipit-current-repo))
  (message "Inline comments and reactions cache cleared"))


(defun shipit-debug-corruption ()
  "Debug function to diagnose cross-repository corruption issues."
  (interactive)
  (let ((current-repo (shipit--get-repo-from-remote))
        (current-branch (when (fboundp 'magit-get-current-branch) (magit-get-current-branch))))
    (shipit--debug-log "=== SHIPIT CORRUPTION DEBUG ===")
    (shipit--debug-log "Current repo: %s" current-repo)
    (shipit--debug-log "Current branch: %s" current-branch) 
    (shipit--debug-log "Buffer: %s" (buffer-name))
    (shipit--debug-log "shipit-current-repo (global): %s" (bound-and-true-p shipit-current-repo))
    (shipit--debug-log "shipit-magit-integration (buffer-local): %s" (bound-and-true-p shipit-magit-integration))
    
    (shipit--debug-log "=== CACHE CONTENTS ===")
    (when (boundp 'shipit--cached-branch-prs)
      (let ((count 0))
        (maphash (lambda (key value) 
                   (shipit--debug-log "Branch PR cache: %s -> PR #%s" key (cdr (assq 'number value)))
                   (setq count (1+ count)))
                 shipit--cached-branch-prs)
        (shipit--debug-log "Total branch PR entries: %d" count)))
    
    (when (boundp 'shipit--cached-approval-status)
      (let ((count 0))
        (maphash (lambda (key value)
                   (shipit--debug-log "Approval cache: %s -> %s" key value)
                   (setq count (1+ count)))
                 shipit--cached-approval-status) 
        (shipit--debug-log "Total approval entries: %d" count)))
    
    (shipit--debug-log "=== BUFFER LOCAL VARS ===")
    (shipit--debug-log "shipit--cached-general-comments: %s" (if shipit--cached-general-comments "HAS DATA" "nil"))
    (shipit--debug-log "shipit--cached-pr-checks: %s" (if shipit--cached-pr-checks "HAS DATA" "nil"))
    (shipit--debug-log "shipit--pr-displayed-in-buffer: %s" shipit--pr-displayed-in-buffer)
    (shipit--debug-log "=== END CORRUPTION DEBUG ===")
    (message "Debug info logged to shipit debug log. Use M-x shipit-view-debug-log to view.")))

(defun shipit-clear-all-caches ()
  "Clear ALL shipit caches globally - use when upgrading cache format."
  (interactive)
  (when (boundp 'shipit--reaction-cache)
    (clrhash shipit--reaction-cache)
    (message "Cleared global reaction cache"))
  (when (boundp 'shipit--reaction-fetch-in-progress)  
    (clrhash shipit--reaction-fetch-in-progress)
    (message "Cleared global reaction fetch cache"))
  (when (boundp 'shipit--cached-approval-status)
    (clrhash shipit--cached-approval-status)
    (message "Cleared global approval status cache"))
  (when (boundp 'shipit--cached-branch-prs)
    (clrhash shipit--cached-branch-prs)
    (message "Cleared global branch PRs cache"))
  (when (boundp 'shipit--comment-cache)
    (clrhash shipit--comment-cache)
    (message "Cleared global comment cache"))
  (when (boundp 'shipit--comment-type-cache)
    (clrhash shipit--comment-type-cache)
    (message "Cleared global comment type cache"))
  (when (boundp 'shipit--workflow-run-cache)
    (clrhash shipit--workflow-run-cache)
    (message "Cleared global workflow run cache"))
  (when (boundp 'shipit--workflow-name-cache)
    (clrhash shipit--workflow-name-cache)
    (message "Cleared global workflow name cache"))
  (when (boundp 'shipit--codeowners-cache)
    (clrhash shipit--codeowners-cache)
    (message "Cleared global CODEOWNERS cache"))
  ;; Clear inline comments cache
  (when (boundp 'shipit--cached-inline-comments)
    (setq shipit--cached-inline-comments nil)
    (message "Cleared inline comments cache"))
  (when (boundp 'shipit--inline-comments-fetched)
    (setq shipit--inline-comments-fetched nil))
  ;; Clear general comments cache
  (when (boundp 'shipit--cached-general-comments)
    (setq shipit--cached-general-comments nil)
    (message "Cleared general comments cache"))
  ;; Clear backend-specific discussions cache via dispatch
  (shipit--clear-backend-discussions-cache)
  ;; Clear ETag persistent cache (preserving user read-state)
  (when (and (boundp 'shipit-gh-etag--persistent-cache)
             (fboundp 'shipit-gh-etag--clear-api-cache-only))
    (shipit-gh-etag--clear-api-cache-only)
    (message "Cleared ETag cache (preserved user state)"))
  ;; Clear ETag refresh cache (per-refresh deduplication)
  (when (boundp 'shipit-gh-etag--refresh-cache)
    (setq shipit-gh-etag--refresh-cache nil)
    (message "Cleared ETag refresh cache"))
  ;; Clear last displayed PR to prevent auto-restoration on base branches
  (when (boundp 'shipit--current-displayed-pr)
    (setq shipit--current-displayed-pr nil)
    (message "Cleared last displayed PR"))
  ;; Clear global repository state to force fresh detection
  (when (boundp 'shipit-current-repo)
    (setq shipit-current-repo nil)
    (message "Cleared global repository state"))
  ;; Clear PR completion cache (stores formatted PR titles)
  (when (boundp 'shipit--completion-cache)
    (setq shipit--completion-cache nil)
    (message "Cleared PR completion cache"))
  (when (fboundp 'shipit--markdown-cache-clear)
    (shipit--markdown-cache-clear)
    (message "Cleared markdown render cache"))
  (when (fboundp 'shipit--org-cache-clear)
    (shipit--org-cache-clear)
    (message "Cleared org render cache"))
  (when (fboundp 'shipit--svg-lib-icon-cache-clear)
    (shipit--svg-lib-icon-cache-clear))
  (message "All shipit caches cleared globally"))

(defun shipit--debug-reaction-cache ()
  "Debug function to inspect the reaction cache contents."
  (interactive)
  (if (boundp 'shipit--reaction-cache)
      (let ((cache-contents '()))
        (maphash (lambda (key value)
                   (push (cons key (length value)) cache-contents))
                 shipit--reaction-cache)
        (if cache-contents
            (message "Reaction cache has %d entries: %s"
                     (length cache-contents)
                     (mapconcat (lambda (entry)
                                  (format "%s (%d reactions)" (car entry) (cdr entry)))
                                cache-contents ", "))
          (message "Reaction cache is empty")))
    (message "Reaction cache not initialized")))

(defvar shipit--workflow-run-cache (make-hash-table :test 'equal)
  "Cache mapping workflow run IDs to workflow names.")

(defvar shipit--comment-cache (make-hash-table :test 'equal)
  "Cache for PR/MR comments by file path.")

(defvar shipit--comment-type-cache (make-hash-table :test 'equal)
  "Cache to track whether comments are inline (t) or general (nil).")

(defvar shipit--deleted-comment-ids (make-hash-table :test 'equal)
  "Set of comment IDs that have been detected as deleted (return 404 errors).")

(defvar shipit--team-membership-cache (make-hash-table :test 'equal)
  "Cache for team membership data with TTL-based expiration.
  Key: 'org:user' Value: '(teams . timestamp)' where timestamp is from float-time.")

(defun shipit-clear-team-membership-cache ()
  "Clear the team membership cache."
  (interactive)
  (clrhash shipit--team-membership-cache)
  (message "Team membership cache cleared (expired TTL will force fresh requests)"))

;;;###autoload  
(defun shipit-cache ()
  "Open shipit cache management menu."
  (interactive)
  (shipit-cache-menu))

(provide 'shipit-cache)
;;; shipit-cache.el ends here
