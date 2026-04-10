;;; shipit-pr-backends.el --- Pluggable PR backend registry -*- lexical-binding: t; -*-

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
;; Pluggable backend registry for PR operations (GitHub, GitLab, etc.).
;; A backend is a plist registered under a symbol ID.
;;
;; Required plist keys:
;;   :name                 — human-readable name
;;   :fetch-pr             — (config number) → PR alist
;;   :search               — (config args) → list of PR alists
;;   :create-pr            — (config title body base head) → PR alist
;;   :merge-pr             — (config number method) → result
;;   :update-pr            — (config number data) → PR alist
;;   :fetch-reviews        — (config number) → list of review alists
;;   :submit-review        — (config number event body comments) → review alist
;;   :fetch-review-decision — (config number) → decision data
;;   :fetch-files          — (config number) → list of file alists
;;   :fetch-commits        — (config number) → list of commit alists
;;   :fetch-checks         — (config ref) → list of check alists
;;   :browse-url           — (config number) → URL string
;;
;; Optional plist keys:
;;   :fetch-review-threads    — (config number) → threaded review data
;;   :fetch-resolved-threads  — (config number) → resolved thread info
;;   :fetch-timeline          — (config number) → timeline events
;;   :fetch-branch-protection — (config base-ref) → protection rules
;;   :fetch-codeowners        — (config base-ref) → CODEOWNERS text
;;   :add-label               — (config number label-name) → list of label alists
;;   :remove-label            — (config number label-name) → t on success
;;   :dismiss-review           — (config number message) → result or nil
;;   :add-reaction            — (config number reaction) → reaction alist
;;   :fetch-reactions         — (config number) → list of reaction alists
;;   :delete-reaction         — (config number reaction-id) → t on success
;;   :add-reviewer            — (config number username) → result
;;   :remove-reviewer         — (config number username) → result
;;   :fetch-requested-reviewers — (config number) → reviewer data
;;   :add-assignee            — (config number username) → result
;;   :remove-assignee         — (config number username) → result
;;   :fetch-pr-assignees      — (config number) → list of assignee alists
;;   :fetch-available-assignees — (config) → list of username strings
;;   :fetch-available-labels  — (config) → list of label alists
;;   :fetch-labels            — (config number) → minimal PR data with labels
;;   :fetch-commit            — (config sha) → commit data with files
;;   :add-reviewers-batch     — (config number usernames) → result
;;   :add-assignees-batch     — (config number usernames) → result
;;   :set-labels              — (config number label-names) → result
;;   :fetch-check-suites      — (config ref page per-page) → check suites response
;;   :fetch-suite-check-runs  — (config suite-id) → check runs response
;;   :fetch-workflow-info     — (config workflow-id) → workflow data
;;   :fetch-action-run-info   — (config run-id) → action run data
;;   :fetch-commit-check-runs — (config ref page per-page) → check runs response
;;   :fetch-compare           — (config base head) → compare data with merge_base_commit
;;   :search-repos            — (config query) → list of repo alists ((full_name . "owner/repo") ...)
;;   :search-repos-request    — (config query) → (url headers normalizer) for curl-based live search
;;   :search-raw              — (config query page per-page) → search results
;;   :refspec-for-pr          — (config pr-number head-ref) → refspec string
;;   :remote-for-fetch        — (config repo) → remote name or URL
;;   :pr-type-label           — string, e.g. "Pull Request" or "Merge Request"
;;   :editor-extra-keys       — alist of (key-string . command) for editor keybindings
;;   :editor-reference-hints  — string for editor header line, e.g. "#: PR" or "#: Issue, !: MR"
;;   :browse-issue-url        — (config number) → URL string for issue
;;   :browse-commit-url       — (config sha) → URL string for commit
;;   :browse-user-url         — (config username) → URL string for user profile
;;   :get-current-username    — (config) → authenticated username string
;;   :extract-username-from-email — (config email) → username or nil
;;   :generate-avatar-url     — (config username) → avatar URL or nil
;;   :fetch-rate-limit        — (config) → rate limit data alist
;;   :fetch-current-user      — (config) → user data alist with login, name
;;
;; Async check operations (used by shipit-checks.el for non-blocking CI fetch):
;;   :fetch-check-suites-async     — (config ref page per-page callback)
;;   :fetch-suite-check-runs-async — (config suite-id callback)
;;   :fetch-workflow-info-async    — (config workflow-id callback) [GitHub-only]
;;   :fetch-action-run-info-async  — (config run-id callback) [GitHub-only]
;;   :fetch-commit-check-runs-async — (config ref page per-page callback)
;;
;; Notification operations:
;;   :mark-notification-read  — (config notification-id) → t on success
;;   :fetch-notifications     — (config params &optional force-fresh) → list
;;   :browse-discussion-url   — (config number) → URL string
;;   :fetch-user-teams        — (config) → list of team data alists
;;   :fetch-merge-methods     — (config) → list of allowed merge method strings
;;   :get-repo-subscription   — (config) → subscription alist or nil
;;   :set-repo-subscription   — (config state) → result
;;   :get-repo-starred        — (config) → t or nil
;;   :set-repo-starred        — (config starred) → result
;;   :fetch-watched-repos     — (config) → list of repo alists
;;   :get-thread-subscription — (config repo type number) → state string or nil
;;   :set-thread-subscription — (config repo type number subscribed) → t on success

;;; Code:

(require 'shipit-core)

(defvar shipit-pr-backends nil
  "Alist of (ID . PLIST) for registered PR backends.")

(defconst shipit-pr--required-keys
  '(:name :fetch-pr :search :create-pr :merge-pr :update-pr
    :fetch-reviews :submit-review :fetch-review-decision
    :fetch-files :fetch-commits :fetch-checks :browse-url)
  "Required keys in a PR backend plist.")

(defun shipit-pr-register-backend (id plist)
  "Register PR backend ID with PLIST.
PLIST must contain all keys in `shipit-pr--required-keys'."
  (dolist (key shipit-pr--required-keys)
    (unless (plist-get plist key)
      (error "PR backend %s missing required key %s" id key)))
  (setq shipit-pr-backends (assq-delete-all id shipit-pr-backends))
  (push (cons id plist) shipit-pr-backends)
  (shipit--debug-log "Registered PR backend: %s (%s)" id (plist-get plist :name)))

(defun shipit-pr--backend-id ()
  "Return the resolved PR backend symbol.
Resolves `auto' to a concrete backend via git remote detection."
  (if (eq shipit-pr-backend 'auto)
      (shipit--detect-backend-from-remote)
    shipit-pr-backend))

(defun shipit-pr--get-backend ()
  "Return the active PR backend plist.
Uses `shipit-pr-backend' defcustom.  When set to `auto', detects
from the git remote URL."
  (let ((id (shipit-pr--backend-id)))
    (or (cdr (assq id shipit-pr-backends))
        (error "No PR backend registered for `%s'" id))))

(defun shipit-pr--resolve-for-repo (repo)
  "Resolve PR backend and config for REPO.
Returns (BACKEND-PLIST . CONFIG-PLIST) with :repo injected into config.
When backend has :inject-project-path, also populates :project-path from REPO."
  (let* ((backend-plist (shipit-pr--get-backend))
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

;; Common shipit check shape — the canonical representation for CI/CD data.
;; Both GitHub and GitLab backends normalize to this shape.
;;
;; Required fields (all backends must provide):
;;   name        — Check/job display name (string)
;;   status      — "queued" | "in_progress" | "completed"
;;   conclusion  — nil | "success" | "failure" | "cancelled" | "skipped" | "action_required"
;;   html_url    — Link to check details page (string)
;;
;; Optional fields:
;;   started_at      — ISO-8601 timestamp when the check started
;;   completed_at    — ISO-8601 timestamp when the check finished
;;   workflow-name   — Grouping: top-level category
;;                     (GitHub: workflow name, GitLab: pipeline stage)
;;   workflow-run-name — Grouping: run instance within category

(defconst shipit-pr--output-contracts
  '((:fetch-pr
     . ((required . (number title body state user head base html_url created_at))
        (user-required . (login))
        (nested . ((head . (ref sha)) (base . (ref))))))
    (:search
     . ((list-of . t)
        (required . (number title state user))
        (user-required . (login))))
    (:fetch-files
     . ((list-of . t)
        (required . (filename status additions deletions))))
    (:fetch-commits
     . ((list-of . t)
        (required . (sha commit))
        (nested . ((commit . (message))))))
    (:fetch-reviews
     . ((list-of . t)
        (required . (user state))
        (user-required . (login))))
    (:fetch-checks
     . ((list-of . t)
        (required . (name status conclusion html_url)))))
  "Output contracts for PR backend operations.
Covers data-returning read operations only (not browse-url, merge, etc.).")

(defun shipit-pr--verify-contract (operation result)
  "Verify RESULT satisfies the output contract for OPERATION.
Signals error listing missing fields.  For list-returning operations,
verifies every element."
  (let* ((spec (cdr (assq operation shipit-pr--output-contracts)))
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

(defconst shipit-pr--parity-operations
  '(:add-label :remove-label :fetch-labels :set-labels :fetch-available-labels
    :add-reviewer :remove-reviewer :add-reviewers-batch :fetch-requested-reviewers
    :add-assignee :remove-assignee :add-assignees-batch
    :fetch-pr-assignees :fetch-available-assignees
    :dismiss-review :fetch-commit)
  "Operations all PR backends should support for feature parity.")

(defun shipit-pr--classify-url (url)
  "Classify URL by trying each registered backend's :classify-url.
Returns plist (:type TYPE :repo REPO :backend-id ID ...) or nil.
The :backend-id key identifies which backend matched."
  (cl-some (lambda (entry)
             (let* ((backend-id (car entry))
                    (fn (plist-get (cdr entry) :classify-url))
                    (result (when fn (funcall fn url))))
               (when result
                 (plist-put result :backend-id backend-id))))
           shipit-pr-backends))

(defun shipit-pr--create-reference-overlays (repo search-start search-limit inc-found inc-overlay)
  "Create reference overlays by calling each backend's :create-reference-overlays.
Iterates ALL registered backends so cross-forge URLs in comments get overlays."
  (dolist (entry shipit-pr-backends)
    (let ((fn (plist-get (cdr entry) :create-reference-overlays)))
      (when fn (funcall fn repo search-start search-limit inc-found inc-overlay)))))

(defun shipit--get-thread-subscription (repo type number)
  "Get thread subscription state for TYPE NUMBER in REPO via backend.
Returns \"subscribed\", \"unsubscribed\", \"ignored\", or nil if unsupported."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :get-thread-subscription)))
    (when fn (funcall fn config repo type number))))

(defun shipit--set-thread-subscription (repo type number subscribed)
  "Set thread subscription for TYPE NUMBER in REPO via backend.
SUBSCRIBED is t to subscribe, nil to unsubscribe.
Returns t on success, nil if unsupported."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :set-thread-subscription)))
    (when fn (funcall fn config repo type number subscribed))))

(provide 'shipit-pr-backends)
;;; shipit-pr-backends.el ends here
