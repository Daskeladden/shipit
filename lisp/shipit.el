;;; shipit.el --- Code review integration for Magit -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

;; Author: Daskeladden
;; Version: 1.4.0
;; Package-Requires: ((emacs "28.1") (magit "4.5.0") (magit-section "4.5.0") (transient "0.3.0") (dash "2.19.0"))
;; Keywords: vc tools git
;; URL: https://github.com/Daskeladden/shipit

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

;; Shipit brings code review into Emacs.  Review pull requests, post
;; comments, approve changes, and track CI status — all without leaving
;; your editor.  Integrates deeply with Magit for a native experience.

;;; Code:

;; CORE MODULES - Always loaded immediately (lightweight, essential)
(require 'cl-lib)
(require 'subr-x)
(require 'shipit-core)       ; Configuration, utilities, token validation
(require 'shipit-http)       ; API functions, GitHub integration
(require 'shipit-cache)      ; Cache utilities

;; HEAVY MODULES - Autoloaded on demand
;; shipit-pr-sections - Complex UI rendering, magit integration
;; shipit-checks   - CI API calls, external process management
;; shipit-render   - Image processing, avatar handling
;; shipit-debug    - Development utilities, debugging tools
;; shipit-diff     - Diff buffer integration
;; shipit-commands - Interactive command implementations

;; Load autoloads for deferred features; fall back to eager loading
;; when autoloads haven't been generated (e.g. running from source).
(unless (load "shipit-autoloads" t t)
  (require 'shipit-pr-sections)
  (require 'shipit-buffer)
  (require 'shipit-commands)
  (require 'shipit-checks)
  (require 'shipit-debug)
  (require 'shipit-notifications)
  (require 'shipit-preview)
  (require 'shipit-diff)
  (require 'shipit-render)
  (require 'shipit-editor))

;; MODERN INITIALIZATION API
;;;###autoload
(defun shipit-init ()
  "Modern shipit initialization - call once in your config.
Replaces shipit-enable and shipit-setup with simpler, more reliable loading."
  (interactive)
  ;; Load ALL modules eagerly to avoid lazy loading issues
  (require 'shipit-core)
  (require 'shipit-cache)
  (require 'shipit-http)
  (require 'shipit-gh-etag)
  (require 'shipit-render)
  (require 'shipit-buffer)  ; Load dedicated buffer mode
  (require 'shipit-diff)    ; Load diff integration
  (require 'magit-section)  ; CRITICAL: Load before shipit-pr-sections for macro expansion
  (require 'shipit-pr-sections)   ; Load UI integration
  (require 'shipit-commands) ; Load interactive commands
  (require 'shipit-checks)   ; Load CI checks
  (require 'shipit-notifications)
  ;; Load review mode modules
  (require 'shipit-review-mode)
  (require 'shipit-review-revision)
  (require 'shipit-review-side-frame)
  (require 'shipit-debug)
  (require 'shipit-editor)  ; Comment/description editor with live preview
  ;; Pluggable backend registries + GitHub implementations
  (require 'shipit-pr-backends)
  (require 'shipit-pr-github)
  (require 'shipit-comment-backends)
  (require 'shipit-comment-github)
  ;; GitLab backend (always loaded for cross-backend features like repo search)
  (require 'shipit-pr-gitlab)
  (require 'shipit-comment-gitlab)
  (when shipit-issues-enabled
    (require 'shipit-issue-backends)   ; Pluggable backend registry
    (require 'shipit-issue-github)     ; GitHub backend (always available)
    ;; Load non-default issue backends on demand
    (let ((needed-ids (delete-dups
                       (delq nil
                             (cons (unless (eq shipit-issue-backend 'github)
                                     shipit-issue-backend)
                                   (mapcar (lambda (e) (plist-get (cdr e) :backend))
                                           shipit-issue-repo-backends))))))
      (dolist (id needed-ids)
        (let ((feature (cdr (assq id shipit-issue-backend-features))))
          (when feature (require feature)))))
    (require 'shipit-issues)           ; Issues search
    (require 'shipit-issues-buffer)    ; Issues buffer view
    (require 'shipit-issue-branch)     ; Branch creation transient
    (require 'shipit-issue-create))    ; Rich issue creation buffer
  (when shipit-discussions-enabled
    (require 'shipit-discussions-graphql)  ; GraphQL queries + normalization
    (require 'shipit-discussions)          ; Discussion search + transient
    (require 'shipit-discussions-buffer))  ; Discussion view buffer

  ;; Set up diff buffer integration for inline comments
  (when (fboundp 'shipit--setup-diff-hooks)
    (shipit--setup-diff-hooks))

  ;; Warm up auth-source cache (decrypts .authinfo.gpg once) before async work
  (shipit--warm-up-auth-source)

  ;; Initialize notifications system
  (shipit-notifications-init)

  ;; Issue-key refs in code comments (opt-out via
  ;; `shipit-code-refs-auto-enable').
  (require 'shipit-code-refs)
  (when shipit-code-refs-auto-enable
    (global-shipit-code-refs-mode 1))

  (message "Shipit initialized successfully"))

;; SELECTIVE AUTOLOADS - Only for heavy features that benefit from deferred loading

;; UI Integration - Heavy magit section rendering
;;;###autoload (autoload 'shipit--select-pr "shipit-pr-search" nil t)
;;;###autoload (autoload 'shipit-refresh "shipit-pr-actions" nil t)

;; Buffer Management (shipit-buffer) - Dedicated PR buffers
;;;###autoload (autoload 'shipit-buffer-refresh-hard "shipit-buffer" nil t)

;; Interactive Commands (shipit-commands) - Complex user interactions
;;;###autoload (autoload 'shipit-open-pr-for-current-branch "shipit-commands" nil t)
;;;###autoload (autoload 'shipit-advanced-pr-search "shipit-commands" nil t)
;;;###autoload (autoload 'shipit-open-url "shipit-commands" nil t)

;; Debug & Development Tools (shipit-debug) - Development utilities
;;;###autoload (autoload 'shipit--open-pr-in-browser "shipit-debug" nil t)
;;;###autoload (autoload 'shipit-debug-menu "shipit-debug" nil t)
;;;###autoload (autoload 'shipit--reload-wrapper "shipit-debug" nil t)

;; Notification System (shipit-notifications) - Background polling
;; Note: shipit-mode is defined in shipit-magit.el, not notifications
;;;###autoload (autoload 'shipit--mention-actions "shipit-notifications" nil t)
;;;###autoload (autoload 'shipit-notifications-menu "shipit-notifications" nil t)

;; Preview PR (shipit-preview) - Preview buffer before PR creation
;;;###autoload (autoload 'shipit-preview-pr "shipit-preview" nil t)

;; Issues (shipit-issues) - GitHub Issues search and viewing
;;;###autoload (autoload 'shipit-issues--quick-search "shipit-issues" nil t)
;;;###autoload (autoload 'shipit-advanced-issue-search "shipit-issues" nil t)

;; Issue Creation (shipit-issue-create) - Rich issue creation buffer
;;;###autoload (autoload 'shipit-issue-create-buffer "shipit-issue-create" nil t)

;; Discussions (shipit-discussions) - GitHub Discussions search and viewing
;;;###autoload (autoload 'shipit-discussions--quick-search "shipit-discussions" nil t)
;;;###autoload (autoload 'shipit-advanced-discussion-search "shipit-discussions" nil t)
;;;###autoload (autoload 'shipit-create-discussion "shipit-discussions" nil t)

;; Actions (shipit-actions-list) - GitHub Actions workflows listing
;;;###autoload (autoload 'shipit-open-actions-list "shipit-actions-list" nil t)

;; Releases (shipit-releases-buffer) - GitHub Releases and tags
;;;###autoload (autoload 'shipit-open-releases-buffer "shipit-releases-buffer" nil t)

;; Functions that remain immediately available (moved to core modules or are lightweight)
(declare-function shipit-cache-menu "shipit-cache")
(declare-function shipit-config "shipit-commands")
(declare-function shipit-subscriptions "shipit-subscriptions-buffer")
(declare-function shipit-help "shipit-core")
(declare-function shipit-notifications-menu "shipit-notifications")
(declare-function shipit-issues--quick-search "shipit-issues")
(declare-function shipit-advanced-issue-search "shipit-issues")
(declare-function shipit-discussions--quick-search "shipit-discussions")
(declare-function shipit-advanced-discussion-search "shipit-discussions")
(declare-function shipit-create-discussion "shipit-discussions")

(transient-define-prefix shipit-menu ()
  "Main shipit menu for GitHub PR integration."
  :man-page "shipit"
  ["Pull Request"
   [("s" "Select PR" shipit--select-pr)
    ("r" "Refresh PR data" shipit-refresh)
    ("p" "Preview PR (before creation)" shipit-preview-pr)]
   [("b" "Open PR for current branch" shipit-open-pr-for-current-branch)
    ("o" "Open PR in browser" shipit--open-pr-in-browser)
    ("a" "Advanced PR search" shipit-advanced-pr-search)]]
  ["Navigate"
   ("u" "Open URL" shipit-open-url)
   ("W" "Workflows" shipit-open-actions-list)
   ("S" "Subscriptions" shipit-subscriptions)
   ("n" "Notifications buffer" shipit--view-notifications)
   ("A" "Atlassian dashboard" shipit-atlassian-dashboard
    :if shipit-atlassian-dashboard-available-p)]
  ["Issues" :if (lambda () shipit-issues-enabled)
   ("i" "Search issues" shipit-issues--quick-search)
   ("I" "Advanced issue search" shipit-advanced-issue-search)
   ("c" "Create issue" shipit-create-issue)]
  ["Discussions" :if (lambda () shipit-discussions-enabled)
   ("d" "Search discussions" shipit-discussions--quick-search)
   ("G" "Advanced discussion search" shipit-advanced-discussion-search)
   ("C-d" "Create discussion" shipit-create-discussion)]
  ["System"
   [("D" "Debug tools" shipit-debug-menu)
    ("K" "Cache management" shipit-cache-menu)
    ("N" "Notifications" shipit-notifications-menu)
    ("C" "Configuration" shipit-config)]
   [("R" "Reload shipit" shipit--reload-wrapper)
    ("h" "Help" shipit-help)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun shipit ()
  "Open main shipit menu."
  (interactive)
  (shipit-menu))

(provide 'shipit)
;;; shipit.el ends here
