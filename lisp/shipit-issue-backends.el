;;; shipit-issue-backends.el --- Pluggable issue backend registry -*- lexical-binding: t; -*-

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
;; Pluggable backend registry for issue trackers (GitHub, Jira, etc.).
;; A backend is a plist registered under a symbol ID.
;;
;; Required plist keys:
;;   :name              — human-readable name
;;   :fetch-issue       — (config id) → normalized alist
;;   :fetch-comments    — (config id) → list of alists
;;   :fetch-comments-async — (config id callback)
;;   :search            — (config query) → list of alists
;;   :create-issue      — (config title body) → normalized alist
;;   :reference-patterns — (config) → list of (REGEX GROUP EXTRACTOR)
;;   :browse-url        — (config id) → URL string
;;   :id-to-string      — (id) → display string (e.g. "#42" or "PRJ-42")
;;   :string-to-id      — (string) → id
;;
;; Optional plist keys (for comment interactions):
;;   :add-comment       — (config issue-id body) → comment alist
;;   :edit-comment      — (config comment-id body) → updated comment alist
;;   :toggle-reaction   — (config comment-id reaction) → reaction alist or nil
;;
;; Optional plist keys (for description reactions):
;;   :fetch-reactions   — (config issue-number) → list of reaction alists
;;   :add-reaction      — (config issue-number reaction) → reaction alist
;;   :remove-reaction   — (config issue-number reaction-id) → t
;;
;; Optional plist keys (for notifications):
;;   :notifications     — (config since) → list of notification activities
;;   :mark-notification-read — (config activity) → marks notification as read

;;; Code:

(require 'shipit-core)

(defvar shipit-issue-backends nil
  "Alist of (ID . PLIST) for registered issue backends.")

(defconst shipit-issue--required-keys
  '(:name :fetch-issue :fetch-comments :fetch-comments-async
    :search :create-issue :reference-patterns :browse-url :id-to-string :string-to-id)
  "Required keys in an issue backend plist.")

(defun shipit-issue-register-backend (id plist)
  "Register issue backend ID with PLIST.
PLIST must contain all keys in `shipit-issue--required-keys'."
  (dolist (key shipit-issue--required-keys)
    (unless (plist-get plist key)
      (error "Issue backend %s missing required key %s" id key)))
  ;; Remove existing entry if present, then add
  (setq shipit-issue-backends (assq-delete-all id shipit-issue-backends))
  (push (cons id plist) shipit-issue-backends)
  (shipit--debug-log "Registered issue backend: %s (%s)" id (plist-get plist :name)))

(defun shipit-issue--get-backend ()
  "Return the active backend plist.
Uses buffer-local `shipit-issue-backend' with global fallback."
  (let ((id shipit-issue-backend))
    (or (cdr (assq id shipit-issue-backends))
        (error "No issue backend registered for `%s'" id))))

(defun shipit-issue--get-config ()
  "Return the active backend config plist."
  shipit-issue-backend-config)

(defun shipit-issue--resolve-for-repo (repo)
  "Resolve backend and config for REPO.
Checks `shipit-issue-repo-backends' first, then falls back to
`shipit-issue-backend' / `shipit-issue-backend-config'.
Returns (BACKEND-PLIST . CONFIG-PLIST) with :repo injected into config."
  (let ((match (cl-find-if
                (lambda (entry)
                  (string-match-p (concat "\\`" (car entry) "\\'") repo))
                shipit-issue-repo-backends)))
    (if match
        (let* ((config-plist (cdr match))
               (backend-id (plist-get config-plist :backend))
               (backend-plist (or (cdr (assq backend-id shipit-issue-backends))
                                  (error "No issue backend registered for `%s'" backend-id)))
               (config (plist-put (copy-sequence config-plist) :repo repo)))
          (cons backend-plist config))
      (let* ((backend-plist (shipit-issue--get-backend))
             (base-config (shipit-issue--get-config))
             (config (if base-config
                         (plist-put (copy-sequence base-config) :repo repo)
                       (list :repo repo))))
        (cons backend-plist config)))))

(defun shipit-issue--all-reference-patterns (repo)
  "Return reference patterns for REPO from the resolved backend.
Each element is (BACKEND-ID REGEX GROUP EXTRACTOR)."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend-plist (car resolved))
         (config (cdr resolved))
         (backend-id (plist-get config :backend))
         (patterns-fn (plist-get backend-plist :reference-patterns))
         (patterns (funcall patterns-fn config))
         (result nil))
    (dolist (pattern patterns)
      (push (cons (or backend-id shipit-issue-backend) pattern) result))
    (nreverse result)))

;;; Optional creation field metadata

(defun shipit-issue--creation-fields (backend-plist config)
  "Return creation field descriptors from BACKEND-PLIST, or nil.
Each field is a plist: (:name SYMBOL :label STRING :type TYPE
:required BOOL :fetch-options FN-or-nil).
CONFIG is passed to the backend's :creation-fields function."
  (let ((fn (plist-get backend-plist :creation-fields)))
    (when fn (funcall fn config))))

;;; Optional status transitions

(defun shipit-issue--backend-has-transitions-p (backend-plist)
  "Return non-nil if BACKEND-PLIST supports status transitions."
  (not (null (plist-get backend-plist :get-transitions))))

(defun shipit-issue--get-transitions (backend-plist config issue-id)
  "Fetch available transitions for ISSUE-ID via BACKEND-PLIST and CONFIG."
  (let ((fn (plist-get backend-plist :get-transitions)))
    (when fn
      (funcall fn config issue-id))))

(defun shipit-issue--transition-status (backend-plist config issue-id transition-id)
  "Transition ISSUE-ID to TRANSITION-ID via BACKEND-PLIST and CONFIG."
  (let ((fn (plist-get backend-plist :transition-status)))
    (unless fn
      (error "Backend does not support :transition-status"))
    (funcall fn config issue-id transition-id)))

;;; Optional notification polling

(defun shipit-issue--backend-has-notifications-p (backend-plist)
  "Return non-nil if BACKEND-PLIST supports the :notifications operation."
  (not (null (plist-get backend-plist :notifications))))

(defun shipit-issue--fetch-notifications (backend-plist config &optional since)
  "Fetch notifications from BACKEND-PLIST using CONFIG.
SINCE is an ISO8601 timestamp; only return items updated after it."
  (let ((fn (plist-get backend-plist :notifications)))
    (when fn
      (funcall fn config since))))

;;; Reactions capability check

(defun shipit-issue--backend-has-reactions-p (backend-plist)
  "Return non-nil if BACKEND-PLIST supports description reactions."
  (not (null (plist-get backend-plist :fetch-reactions))))

;;; Output contracts — define required fields per operation

(defconst shipit-issue--output-contracts
  '((:fetch-issue
     . ((required . (title state user created_at))
        (user-required . (login))))
    (:fetch-comments
     . ((list-of . t)
        (required . (id body user created_at))
        (user-required . (login))))
    (:search
     . ((list-of . t)
        (required . (title state user))
        (user-required . (login))))
    (:create-issue
     . ((required . (title state user))
        (user-required . (login)))))
  "Output contracts for issue backend operations.
Covers data-returning operations (not browse-url, id-to-string, etc.).")

(defun shipit-issue--verify-contract (operation result)
  "Verify RESULT satisfies the output contract for OPERATION.
Signals error listing missing fields.  For list-returning operations,
verifies every element."
  (let* ((spec (cdr (assq operation shipit-issue--output-contracts)))
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

(defconst shipit-issue--parity-operations
  '(:edit-comment :toggle-reaction :update-description
    :creation-fields :create-issue-extended)
  "Operations all issue backends should support for feature parity.")

;;; Shared issue utilities — used by both issues-buffer and dashboard

;; Forward declaration: defined in shipit-issues-buffer.el, used when
;; rendering clickable issue keys.
(defvar shipit-issue--issuelink-keymap)

(defun shipit-issue--state-face (state)
  "Return the face for issue STATE.
Maps common Jira and GitHub statuses to appropriate faces."
  (let ((s (downcase (or state ""))))
    (cond
     ((member s '("open" "to do" "new" "backlog" "reopened")) 'success)
     ((member s '("in progress" "in review" "testing")) 'warning)
     ((member s '("closed" "done" "resolved" "won't do" "declined")) 'error)
     (t 'font-lock-comment-face))))

(defun shipit-issue--compute-work-item-widths (items &optional indent)
  "Compute column widths for ITEMS, fitting to window width.
INDENT is the leading spaces before each item (default 4).
Status and assignee get fixed right-aligned widths; the work
column summary fills remaining space."
  (let* ((indent (or indent 4))
         (win (or (get-buffer-window (current-buffer)) (selected-window)))
         (win-width (max 80 (window-width win)))
         (key-width 12)
         (assignee-width 16)
         (status-width 24)
         (col-sep 2)
         (work-sep 2))
    ;; Expand fixed columns if data exceeds minimums
    (dolist (item items)
      (let ((k (length (or (cdr (assq 'key item)) "")))
            (a (length (or (cdr (assq 'assignee item)) "Unassigned")))
            (s (length (or (cdr (assq 'status item)) ""))))
        (when (> k key-width) (setq key-width k))
        (when (> a assignee-width) (setq assignee-width a))
        (when (> s status-width) (setq status-width s))))
    (let ((summary-width (max 20
                              (- win-width indent
                                 key-width work-sep
                                 assignee-width
                                 status-width
                                 (* col-sep 2)))))
      `((key . ,key-width)
        (summary . ,summary-width)
        (assignee . ,assignee-width)
        (status . ,status-width)
        (issue-type . 10)
        (issue-type-icon . 3)
        (priority-icon . 3)
        (relative-time . 14)))))

(defun shipit-issue--format-work-item-column (item field widths)
  "Format one column FIELD from ITEM using WIDTHS for alignment."
  (let ((width (or (cdr (assq field widths)) 0)))
    (pcase field
      ('work
       (let* ((key (or (cdr (assq 'key item)) ""))
              (summary (or (cdr (assq 'summary item)) ""))
              (key-width (or (cdr (assq 'key widths)) 0))
              (summary-width (or (cdr (assq 'summary widths)) 0))
              (truncated (if (> (length summary) summary-width)
                             (concat (substring summary 0 (- summary-width 1)) "…")
                           summary)))
         (concat (propertize (format (format "%%-%ds" key-width) key)
                             'face '(:inherit link :underline nil)
                             'shipit-issuelink-key key
                             'keymap shipit-issue--issuelink-keymap
                             'help-echo (format "RET: open %s" key))
                 "  "
                 (format (format "%%-%ds" summary-width) truncated))))
      ('key
       (let ((key (or (cdr (assq 'key item)) "")))
         (propertize (format (format "%%-%ds" width) key)
                     'face '(:inherit link :underline nil)
                     'shipit-issuelink-key key
                     'keymap shipit-issue--issuelink-keymap
                     'help-echo (format "RET: open %s" key))))
      ('status
       (let* ((status (or (cdr (assq 'status item)) ""))
              (status-cat (cdr (assq 'status-category item)))
              (face-fn (cdr (assq 'status-category-face widths))))
         (propertize (format (format "%%-%ds" width) status)
                     'face (if (and face-fn status-cat)
                               (funcall face-fn status-cat)
                             (shipit-issue--state-face status)))))
      ('issue-type-icon
       (let* ((type-name (or (cdr (assq 'issue-type item)) ""))
              (render-fn (cdr (assq 'issue-type-icon-render widths))))
         (if render-fn
             (funcall render-fn type-name)
           "")))
      ('priority-icon
       (let* ((prio-name (or (cdr (assq 'priority item)) ""))
              (render-fn (cdr (assq 'priority-icon-render widths))))
         (if render-fn
             (or (funcall render-fn prio-name)
                 (format (format "%%-%ds" width) ""))
           (format (format "%%-%ds" width) ""))))
      ('relative-time
       (let* ((ts (cdr (assq 'time item)))
              (fmt-fn (cdr (assq 'relative-time-format widths)))
              (text (if (and fmt-fn ts) (funcall fmt-fn ts) "")))
         (propertize (format (format "%%-%ds" width) text)
                     'face 'magit-dimmed)))
      ('assignee
       (let ((assignee (or (cdr (assq 'assignee item)) "Unassigned")))
         (format (format "%%-%ds" width) assignee)))
      ('issue-type
       (let ((itype (or (cdr (assq 'issue-type item)) "")))
         (format (format "%%-%ds" width) itype)))
      (_ ""))))

(defun shipit-issue--format-work-item-line (item columns widths)
  "Format ITEM as a single line using COLUMNS for field selection.
COLUMNS is a list of symbols (e.g., (work assignee status)).
WIDTHS is an alist from `shipit-issue--compute-work-item-widths'."
  (mapconcat
   (lambda (col)
     (shipit-issue--format-work-item-column item col widths))
   columns
   "  "))

(defun shipit-issue--format-relative-time (timestamp)
  "Format TIMESTAMP as relative time string."
  (let ((delta (- (float-time) timestamp)))
    (cond
     ((< delta 60) "just now")
     ((< delta 3600) (format "%d min ago" (/ delta 60)))
     ((< delta 86400) (format "%d hours ago" (/ delta 3600)))
     (t (format "%d days ago" (/ delta 86400))))))

(defun shipit-issue--normalize-to-work-item (issue)
  "Map ISSUE alist to work-item field names, handling both formats.
Dashboard issues use `id'/`title'/`state'; work-items use
`key'/`summary'/`status'.  This normalizes to the work-item convention."
  `((key . ,(or (cdr (assq 'key issue)) (cdr (assq 'id issue))))
    (summary . ,(or (cdr (assq 'summary issue)) (cdr (assq 'title issue))))
    (status . ,(or (cdr (assq 'status issue)) (cdr (assq 'state issue))))
    (assignee . ,(cdr (assq 'assignee issue)))
    (issue-type . ,(cdr (assq 'issue-type issue)))
    (priority . ,(cdr (assq 'priority issue)))
    (status-category . ,(cdr (assq 'status-category issue)))
    (time . ,(cdr (assq 'time issue)))))

(defun shipit-issue--dashboard-widths (items backend-plist &optional indent)
  "Compute widths for ITEMS augmented with rendering callbacks from BACKEND-PLIST.
INDENT is the leading spaces (default 3)."
  (let ((base (shipit-issue--compute-work-item-widths items (or indent 3))))
    (append base
            `((issue-type-icon-render . ,(plist-get backend-plist :issue-type-icon-render))
              (priority-icon-render . ,(plist-get backend-plist :priority-icon-render))
              (status-category-face . ,(plist-get backend-plist :status-category-face))
              (relative-time-format . ,#'shipit-issue--format-relative-time)))))

;;; URL classification

(defun shipit-issue--classify-url (url)
  "Classify URL by trying issue backends from `shipit-issue-repo-backends'.
Returns plist (:type TYPE :number ID :backend-id ID :repo REPO
:backend-config CONFIG) or nil."
  (cl-some
   (lambda (entry)
     (let* ((repo (car entry))
            (config (cdr entry))
            (backend-id (plist-get config :backend))
            (backend (cdr (assq backend-id shipit-issue-backends)))
            (fn (plist-get backend :classify-url)))
       (when fn
         (let ((result (funcall fn config url)))
           (when result
             (plist-put result :backend-id backend-id)
             (plist-put result :repo repo)
             (plist-put result :backend-config config)
             result)))))
   shipit-issue-repo-backends))

(provide 'shipit-issue-backends)
;;; shipit-issue-backends.el ends here
