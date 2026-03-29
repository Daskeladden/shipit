;;; shipit-discussion-backends.el --- Pluggable discussion backend registry -*- lexical-binding: t; -*-

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
;; Pluggable backend registry for discussion operations (GitHub, etc.).
;; Reuses the same `shipit-pr-backend' / `shipit-pr-backend-config' defcustoms
;; as PR and comment backends — discussions come from the same vendor.
;;
;; Required plist keys:
;;   :name           — human-readable name
;;   :browse-url     — (config number) → URL string
;;   :browse-repo-url — (config) → repo URL string

;;; Code:

(require 'shipit-core)

(defvar shipit-discussion-backends nil
  "Alist of (ID . PLIST) for registered discussion backends.")

(defconst shipit-discussion--required-keys
  '(:name :browse-url :browse-repo-url)
  "Required keys in a discussion backend plist.")

(defun shipit-discussion-register-backend (id plist)
  "Register discussion backend ID with PLIST.
PLIST must contain all keys in `shipit-discussion--required-keys'."
  (dolist (key shipit-discussion--required-keys)
    (unless (plist-get plist key)
      (error "Discussion backend %s missing required key %s" id key)))
  (setq shipit-discussion-backends (assq-delete-all id shipit-discussion-backends))
  (push (cons id plist) shipit-discussion-backends)
  (shipit--debug-log "Registered discussion backend: %s (%s)" id (plist-get plist :name)))

(defun shipit-discussion--get-backend ()
  "Return the active discussion backend plist.
Uses `shipit-pr-backend' — discussions come from the same vendor as PRs.
When set to `auto', detects from the git remote URL."
  (let ((id (if (eq shipit-pr-backend 'auto)
                (shipit--detect-backend-from-remote)
              shipit-pr-backend)))
    (or (cdr (assq id shipit-discussion-backends))
        (error "No discussion backend registered for `%s'" id))))

(defun shipit-discussion--resolve-for-repo (repo)
  "Resolve discussion backend and config for REPO.
Returns (BACKEND-PLIST . CONFIG-PLIST) with :repo injected into config."
  (let* ((backend-plist (shipit-discussion--get-backend))
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

(provide 'shipit-discussion-backends)
;;; shipit-discussion-backends.el ends here
