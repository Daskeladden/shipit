;;; shipit-comment-backends.el --- Pluggable comment backend registry -*- lexical-binding: t; -*-

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
;; Pluggable backend registry for PR comment operations (GitHub, GitLab, etc.).
;; Covers general and inline PR comments.  Issue comments stay in
;; `shipit-issue-backends.el', discussion comments in `shipit-discussions-graphql.el'.
;;
;; Uses the same `shipit-pr-backend' / `shipit-pr-backend-config' defcustoms
;; as PR backends — comments always come from the same vendor as PRs.
;;
;; Required plist keys:
;;   :name                   — human-readable name
;;   :fetch-general-comments — (config number) → list of comment alists
;;   :fetch-inline-comments  — (config number) → list of comment alists
;;   :add-general-comment    — (config number body) → comment alist
;;   :add-inline-comment     — (config number file line body side) → comment alist
;;   :reply-to-comment       — (config number parent-id body &optional is-inline) → comment alist
;;   :edit-comment           — (config comment-id body &optional is-inline pr-number) → comment alist
;;   :delete-comment         — (config comment-id &optional is-inline) → result
;;   :toggle-reaction        — (config comment-id reaction &optional is-inline) → reaction alist
;;   :fetch-reactions        — (config comment-id &optional is-inline) → list of reaction alists
;;
;; Optional plist keys:
;;   :fetch-general-comments-async — (config number callback) → calls callback with comments
;;   :fetch-reactions-batch   — (config comments is-inline) → populates shipit--reaction-cache
;;   :reply-to-inline         — (config number parent-id body file-path) → comment alist
;;   :delete-reaction         — (config comment-id reaction-id &optional is-inline) → t
;;   :add-review-reaction     — (config node-id reaction) → result (GraphQL)
;;   :remove-review-reaction  — (config node-id reaction) → result (GraphQL)
;;   :fetch-review-reactions  — (config node-id) → reactions (GraphQL)
;;   :clear-discussions-cache — () → clears backend-specific discussion cache

;;; Code:

(require 'shipit-core)

(defvar shipit-comment-backends nil
  "Alist of (ID . PLIST) for registered comment backends.")

(defconst shipit-comment--required-keys
  '(:name :fetch-general-comments :fetch-inline-comments
    :add-general-comment :add-inline-comment :reply-to-comment
    :edit-comment :delete-comment :toggle-reaction :fetch-reactions)
  "Required keys in a comment backend plist.")

(defun shipit-comment-register-backend (id plist)
  "Register comment backend ID with PLIST.
PLIST must contain all keys in `shipit-comment--required-keys'."
  (dolist (key shipit-comment--required-keys)
    (unless (plist-get plist key)
      (error "Comment backend %s missing required key %s" id key)))
  (setq shipit-comment-backends (assq-delete-all id shipit-comment-backends))
  (push (cons id plist) shipit-comment-backends)
  (shipit--debug-log "Registered comment backend: %s (%s)" id (plist-get plist :name)))

(defun shipit-comment--get-backend ()
  "Return the active comment backend plist.
Uses `shipit-pr-backend' — comments come from the same vendor as PRs.
When set to `auto', detects from the git remote URL."
  (let ((id (if (eq shipit-pr-backend 'auto)
                (shipit--detect-backend-from-remote)
              shipit-pr-backend)))
    (or (cdr (assq id shipit-comment-backends))
        (error "No comment backend registered for `%s'" id))))

(defun shipit-comment--resolve-for-repo (repo)
  "Resolve comment backend and config for REPO.
Returns (BACKEND-PLIST . CONFIG-PLIST) with :repo injected into config."
  (let* ((backend-plist (shipit-comment--get-backend))
         (base-config shipit-pr-backend-config)
         (config (if base-config
                     (plist-put (copy-sequence base-config) :repo repo)
                   (list :repo repo))))
    ;; Auto-populate :project-path from repo slug when backend requests it
    (when (and (plist-get backend-plist :inject-project-path)
               (not (plist-get config :project-path))
               (not (plist-get config :project-id)))
      (setq config (plist-put config :project-path repo)))
    (cons backend-plist config)))

;;; Output contracts — define required fields per operation

(defconst shipit-comment--output-contracts
  '((:fetch-general-comments
     . ((list-of . t)
        (required . (id body user created_at))
        (user-required . (login))))
    (:fetch-inline-comments
     . ((list-of . t)
        (required . (id body user created_at path line original_line side))
        (user-required . (login))))
    (:add-general-comment
     . ((required . (id body user created_at))
        (user-required . (login))))
    (:add-inline-comment
     . ((required . (id body user created_at path line original_line side diff_hunk))
        (user-required . (login))))
    (:reply-to-comment
     . ((required . (id body user created_at))
        (user-required . (login))))
    (:edit-comment
     . ((required . (id body))))
    (:delete-comment
     . ((truthy . t)))
    (:fetch-reactions
     . ((list-of . t)
        (required . (content user))
        (user-required . (login))))
    (:toggle-reaction
     . ((truthy . t))))
  "Output contracts for comment backend operations.")

(defun shipit-comment--verify-contract (operation result)
  "Verify RESULT satisfies the output contract for OPERATION.
Signals error listing missing fields.  For list-returning operations,
verifies every element."
  (let* ((spec (cdr (assq operation shipit-comment--output-contracts)))
         (required (cdr (assq 'required spec)))
         (user-required (cdr (assq 'user-required spec)))
         (nested (cdr (assq 'nested spec))))
    (unless spec (error "No contract for %s" operation))
    (if (cdr (assq 'truthy spec))
        (unless result
          (error "Contract %s violated — expected truthy value" operation))
      (let ((items (if (cdr (assq 'list-of spec)) result (list result))))
        (dolist (item items)
          (let ((missing nil))
            (dolist (field required)
              (unless (cdr (assq field item))
                (push field missing)))
            (when user-required
              (let ((user (cdr (assq 'user item))))
                (dolist (f user-required)
                  (unless (cdr (assq f user))
                    (push (intern (format "user.%s" f)) missing)))))
            (dolist (nest nested)
              (let ((sub (cdr (assq (car nest) item))))
                (dolist (f (cdr nest))
                  (unless (and sub (cdr (assq f sub)))
                    (push (intern (format "%s.%s" (car nest) f)) missing)))))
            (when missing
              (error "Contract %s violated — missing: %s" operation missing))))))))

(defconst shipit-comment--parity-operations
  '(:reply-to-inline :delete-reaction)
  "Operations all comment backends should support for feature parity.")

(provide 'shipit-comment-backends)
;;; shipit-comment-backends.el ends here
