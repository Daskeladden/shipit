;;; shipit-notifications-buffer.el --- Dedicated notifications buffer -*- lexical-binding: t -*-

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

;;; Commentary:
;; Standalone buffer for viewing notifications using magit sections.

;;; Code:

(require 'cl-lib)
(require 'shipit-core)
(require 'magit-section)

;; Forward declarations
(declare-function shipit--mark-notification-read "shipit-notifications")
(declare-function shipit--notification-pr-actions "shipit-notifications")
(declare-function shipit--notification-actions "shipit-notifications")
(declare-function shipit--notification-activity-key "shipit-notifications")
(declare-function shipit--check-notifications-background "shipit-notifications")
(declare-function shipit--update-modeline-indicator "shipit-notifications")
(declare-function shipit--format-time-ago "shipit-notifications")
(declare-function shipit--get-notification-type-icon "shipit-render")
(declare-function shipit--get-notification-source-icon "shipit-render")
(declare-function shipit--debug-log "shipit-core")
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--api-request "shipit-http")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")
(declare-function shipit--insert-activity-event "shipit-pr-sections")
(declare-function shipit--fetch-timeline-events-async "shipit-http")
(declare-function shipit--fetch-recent-timeline-events-async "shipit-http")
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit-toggle-timestamp-format "shipit-commands")
(declare-function shipit--format-timestamp "shipit-core")
(declare-function shipit--browse-pr-url "shipit-notifications")
(declare-function shipit--browse-issue-url "shipit-notifications")
(declare-function shipit--open-notification-pr "shipit-notifications")
(declare-function shipit-pr--dispatch-activity-navigation "shipit-pr-actions")
(declare-function shipit--open-notification-issue "shipit-notifications")
(declare-function shipit--open-notification-discussion "shipit-notifications")

(defconst shipit-notifications-buffer-name "*shipit-notifications*"
  "Name of the notifications buffer.")

(defvar-local shipit-notifications-buffer--filter-text ""
  "Current filter text for the notifications buffer.")

;; Section types
(defun notification-entry (&rest _args)
  "Magit section identifier for notification entries.")
(put 'notification-entry 'magit-section t)

(defun notification-activity (&rest _args)
  "Magit section identifier for notification activity timeline.")
(put 'notification-activity 'magit-section t)

(defun notification-description (&rest _args)
  "Magit section identifier for notification description.")
(put 'notification-description 'magit-section t)

;;; Keymap

(defvar shipit-notifications-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'shipit-notifications-buffer-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "f") #'shipit-notifications-buffer-set-filter)
    (define-key map (kbd "RET") #'shipit-notifications-buffer-open)
    (define-key map (kbd "M-;") #'shipit-notifications-buffer-action)
    (define-key map (kbd "TAB") #'shipit-notifications-buffer-toggle-section)
    (define-key map [tab] #'shipit-notifications-buffer-toggle-section)
    (define-key map (kbd "m") #'shipit-notifications-buffer-mark-read)
    (define-key map (kbd "M") #'shipit-notifications-buffer-mark-all-read)
    (define-key map (kbd "n") #'magit-section-forward)
    (define-key map (kbd "p") #'magit-section-backward)
    (define-key map (kbd "L") #'shipit-toggle-timestamp-format)
    (define-key map (kbd "M-w") #'shipit-notifications-buffer-copy-url)
    (define-key map (kbd "w") #'shipit-notifications-buffer-watch)
    map)
  "Keymap for `shipit-notifications-buffer-mode'.")

;;; Mode

(define-derived-mode shipit-notifications-buffer-mode magit-section-mode "Shipit-Notifications"
  "Major mode for viewing notifications.

\\{shipit-notifications-buffer-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-notifications-buffer-refresh)
  (setq-local truncate-lines t)
  (setq-local tab-width 8)
  (shipit--apply-section-defaults))

;;; Buffer management

(defun shipit-notifications-buffer-create ()
  "Create or return the singleton notifications buffer."
  (let ((buf (get-buffer shipit-notifications-buffer-name)))
    (unless buf
      (setq buf (get-buffer-create shipit-notifications-buffer-name))
      (with-current-buffer buf
        (shipit-notifications-buffer-mode)))
    buf))

(defun shipit-notifications-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the notifications buffer content.
Fetches fresh data from the API before re-rendering.
Arguments IGNORE-AUTO and NOCONFIRM are for compatibility with `revert-buffer'."
  (interactive)
  (message "Fetching notifications...")
  (when (fboundp 'shipit--check-notifications-background)
    (shipit--check-notifications-background t))
  (shipit-notifications-buffer--rerender)
  (message "Notifications refreshed"))

(defun shipit-notifications-buffer--rerender ()
  "Re-render the notifications buffer without fetching new data."
  (let ((buf (shipit-notifications-buffer-create)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (shipit-notifications-buffer--render)
        (goto-char (min pos (point-max)))))))

;;; Rendering

(defun shipit-notifications-buffer--render ()
  "Render the notifications buffer content."
  (magit-insert-section (notifications-root)
    (shipit-notifications-buffer--insert-header)
    (shipit-notifications-buffer--insert-notifications)))

(defun shipit-notifications-buffer--insert-header ()
  "Insert the buffer header."
  (insert (propertize "Notifications" 'face 'bold))
  (unless (string-empty-p shipit-notifications-buffer--filter-text)
    (insert (propertize (format "  [filter: %s]" shipit-notifications-buffer--filter-text)
                        'face 'font-lock-comment-face)))
  (insert "\n\n"))

(defun shipit-notifications-buffer--insert-notifications ()
  "Insert all notification entries as magit sections."
  (require 'shipit-notifications)
  (let ((activities '()))
    (when (and (boundp 'shipit--notification-pr-activities)
               shipit--notification-pr-activities)
      (maphash (lambda (_key activity)
                 (push activity activities))
               shipit--notification-pr-activities))
    (setq activities (sort activities
                           (lambda (a b)
                             (string> (or (cdr (assq 'updated-at a)) "")
                                      (or (cdr (assq 'updated-at b)) "")))))
    (when (not (string-empty-p shipit-notifications-buffer--filter-text))
      (setq activities (seq-filter
                        #'shipit-notifications-buffer--matches-filter-p
                        activities)))
    (if activities
        (dolist (activity activities)
          (shipit-notifications-buffer--insert-notification activity))
      (insert (propertize "  No notifications\n" 'face 'font-lock-comment-face)))))

(defun shipit-notifications-buffer--matches-filter-p (activity)
  "Check if ACTIVITY matches the current filter."
  (let ((filter (downcase shipit-notifications-buffer--filter-text))
        (repo (downcase (or (cdr (assq 'repo activity)) "")))
        (subject (downcase (or (cdr (assq 'subject activity)) "")))
        (reason (downcase (or (cdr (assq 'reason activity)) ""))))
    (or (string-match-p filter repo)
        (string-match-p filter subject)
        (string-match-p filter reason))))

(defun shipit-notifications-buffer--insert-notification (activity)
  "Insert a single notification ACTIVITY as a magit section."
  (let* ((heading (shipit-notifications-buffer--format-heading activity))
         (start (point))
         (sect (magit-insert-section (notification-entry activity)
                 (magit-insert-heading heading))))
    ;; Add text properties on the heading for backward compat with
    ;; shipit--notification-actions which reads data via get-text-property
    (let ((repo (cdr (assq 'repo activity)))
          (number (or (cdr (assq 'number activity))
                      (cdr (assq 'pr-number activity))))
          (type (or (cdr (assq 'type activity)) "pr"))
          (notification (cdr (assq 'notification activity)))
          (source (cdr (assq 'source activity)))
          (browse-url (cdr (assq 'browse-url activity)))
          (backend-id (cdr (assq 'backend-id activity)))
          (backend-config (cdr (assq 'backend-config activity)))
          (heading-end (oref sect content)))
      (add-text-properties start (or heading-end (point))
                           `(shipit-repo ,repo
                             shipit-pr-number ,number
                             shipit-notification-type ,type
                             shipit-notification-source ,source
                             shipit-notification-browse-url ,browse-url
                             shipit-notification-backend-id ,backend-id
                             shipit-notification-backend-config ,backend-config
                             shipit-notification ,notification)))
    (magit-section-hide sect)))

(defun shipit-notifications-buffer--format-heading (activity)
  "Format the heading line for notification ACTIVITY."
  (let* ((repo (cdr (assq 'repo activity)))
         (number (or (cdr (assq 'number activity))
                     (cdr (assq 'pr-number activity))))
         (type (or (cdr (assq 'type activity)) "pr"))
         (subject (string-trim (or (cdr (assq 'subject activity)) "")))
         (reason (or (cdr (assq 'reason activity)) "unknown"))
         (updated-at (cdr (assq 'updated-at activity)))
         (time-ago (shipit--format-timestamp updated-at))
         (source (cdr (assq 'source activity)))
         (source-icon (shipit--get-notification-source-icon source))
         (is-draft (cdr (assq 'draft activity)))
         (pr-state (cdr (assq 'pr-state activity)))
         (icon-type (cond
                     ((and (string= type "pr") is-draft) "pr-draft")
                     ((and (string= type "pr") (equal pr-state "merged")) "pr-merged")
                     ((and (string= type "pr") (equal pr-state "closed")) "pr-closed")
                     (t type)))
         (type-icon (shipit--get-notification-type-icon
                     icon-type (pcase icon-type
                                 ("pr" "PR")
                                 ("pr-draft" "DR")
                                 ("pr-merged" "MG")
                                 ("pr-closed" "CL")
                                 ("issue" "IS")
                                 ("discussion" "💬")
                                 ("rss" "RS")
                                 (_ "??"))))
         (repo-width (or (cdr (assq 'repo shipit-notifications-column-widths)) 30))
         (pr-width (or (cdr (assq 'pr shipit-notifications-column-widths)) 5))
         (title-width-cfg (or (cdr (assq 'title shipit-notifications-column-widths)) 45))
         (reason-width (or (cdr (assq 'reason shipit-notifications-column-widths)) 12))
         (spacing (or (bound-and-true-p shipit-notifications-column-spacing) 2))
         (spacer (make-string spacing ?\s))
         (number-str (cond
                      ((string= type "rss") "")
                      ((integerp number) (format "#%d" number))
                      (t (format "%s" number))))
         (pr-str (truncate-string-to-width number-str (1+ pr-width) nil ?\s))
         (title-width (max 10 (- title-width-cfg (1+ (string-width source-icon)))))
         (left-part (format "%s %s %s %s%s%s%s%s"
                            source-icon
                            type-icon
                            (propertize (truncate-string-to-width repo repo-width nil ?\s)
                                        'face 'font-lock-constant-face)
                            pr-str
                            spacer
                            (truncate-string-to-width subject title-width nil ?\s)
                            spacer
                            (propertize (truncate-string-to-width reason reason-width nil ?\s)
                                        'face 'font-lock-keyword-face)))
         (icon-cols 4)
         (fixed-left-width (+ icon-cols 1 repo-width 1 (1+ pr-width)
                              spacing title-width spacing reason-width))
         (window-width (or (window-body-width) 120))
         (time-field-width 16)  ; fixed width to survive format toggles
         (time-text (let* ((ts-width (string-width time-ago))
                           (pad (max 0 (- time-field-width ts-width))))
                      (concat (make-string pad ?\s) time-ago)))
         (padding (max 2 (- window-width fixed-left-width time-field-width 3))))
    (concat left-part
            (make-string padding ?\s)
            (propertize time-text
                        'face 'shipit-timestamp-face
                        'shipit-raw-timestamp (get-text-property 0 'shipit-raw-timestamp time-ago)))))

(defun shipit-notifications-buffer--insert-activity-details (activity)
  "Insert expandable details for a PR/Issue/Discussion ACTIVITY."
  (let* ((repo (or (cdr (assq 'repo activity)) ""))
         (number (or (cdr (assq 'number activity))
                     (cdr (assq 'pr-number activity))))
         (type (or (cdr (assq 'type activity)) "pr"))
         (subject (or (cdr (assq 'subject activity)) ""))
         (reason (or (cdr (assq 'reason activity)) ""))
         (updated (cdr (assq 'updated-at activity)))
         (url (cdr (assq 'browse-url activity)))
         (type-label (pcase type
                       ("pr" "Pull Request")
                       ("issue" "Issue")
                       ("discussion" "Discussion")
                       (_ type)))
         (number-str (if (integerp number) (format "#%d" number) (format "%s" number))))
    (insert "    " (propertize type-label 'face 'bold)
            " " number-str " in " repo "\n")
    (insert "    " subject "\n")
    (when (and reason (not (string-empty-p reason)))
      (insert "    Reason: " (propertize reason 'face 'font-lock-keyword-face) "\n"))
    (when updated
      (insert "    Updated: " (propertize (shipit--format-timestamp updated)
                                          'face 'shipit-timestamp-face) "\n"))
    (when url
      (insert "    " (propertize url 'face 'link) "\n"))))

(defun shipit-notifications-buffer--insert-description (description)
  "Insert DESCRIPTION as expandable body content.
Strips HTML tags and renders as plain text."
  (let ((text (shipit-notifications-buffer--html-to-text description)))
    (dolist (line (split-string text "\n" t))
      (insert "    " (string-trim line) "\n"))))

(defun shipit-notifications-buffer--html-to-text (html)
  "Convert HTML string to plain text."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    ;; <br> and <br/> → newline
    (while (re-search-forward "<br\\s-*/?>" nil t)
      (replace-match "\n"))
    ;; <p>...</p> → content + blank line
    (goto-char (point-min))
    (while (re-search-forward "</p>" nil t)
      (replace-match "\n"))
    ;; <strong>...</strong> → bold markers
    (goto-char (point-min))
    (while (re-search-forward "<strong>\\(.*?\\)</strong>" nil t)
      (replace-match (propertize (match-string 1) 'face 'bold)))
    ;; Strip remaining tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match ""))
    ;; Decode HTML entities
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t) (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">"))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t) (replace-match "\""))
    (string-trim (buffer-string))))

;;; Section toggle with lazy loading

(defun shipit-notifications-buffer-toggle-section ()
  "Toggle notification section, lazy-loading details on first expand."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ;; Child sub-sections — just toggle
     ((and section (memq (oref section type)
                         '(notification-activity notification-description)))
      (magit-section-toggle section))
     ;; Notification entry — lazy-load on first expand
     ((and section (eq (oref section type) 'notification-entry))
      (if (shipit-notifications-buffer--section-has-content-p section)
          (magit-section-toggle section)
        (let ((activity (oref section value)))
          (shipit-notifications-buffer--load-section-content section activity))))
     ;; Other sections — default toggle (but not root)
     (t (when (and section (oref section parent))
          (magit-section-toggle section))))))

(defun shipit-notifications-buffer--section-has-content-p (section)
  "Return non-nil if SECTION has body content (children or text)."
  (let ((content-pos (oref section content))
        (end-pos (oref section end)))
    (and content-pos end-pos
         (> (marker-position end-pos) (marker-position content-pos)))))

(defun shipit-notifications-buffer--load-section-content (section activity)
  "Load and insert content for notification SECTION from ACTIVITY."
  (let* ((type (or (cdr (assq 'type activity)) "pr"))
         (description (cdr (assq 'description activity))))
    (if (and (string= type "rss")
             (stringp description)
             (not (string-empty-p description)))
        ;; RSS — insert description synchronously (already have it)
        (shipit-notifications-buffer--insert-content-into-section
         section
         (lambda () (shipit-notifications-buffer--insert-description description)))
      ;; PR/Issue/Discussion — fetch details async then render
      (shipit-notifications-buffer--fetch-and-insert-details section activity))))

(defun shipit-notifications-buffer--insert-content-into-section (section insert-fn)
  "Insert content into SECTION by calling INSERT-FN, then show it."
  (let* ((inhibit-read-only t)
         (saved-pos (point))
         (content-pos (oref section content))
         (end-pos (oref section end)))
    (when (and content-pos end-pos)
      (save-excursion
        (goto-char content-pos)
        (let ((magit-insert-section--parent section))
          (funcall insert-fn))
        (oset section end (point-marker)))
      (oset section hidden nil)
      (goto-char saved-pos))))

(defun shipit-notifications-buffer--fetch-and-insert-details (section activity)
  "Fetch PR/Issue details for ACTIVITY and insert into SECTION."
  (let* ((repo (cdr (assq 'repo activity)))
         (number (or (cdr (assq 'number activity))
                     (cdr (assq 'pr-number activity))))
         (type (or (cdr (assq 'type activity)) "pr"))
         (updated-at (cdr (assq 'updated-at activity))))
    ;; Show basic details immediately
    (shipit-notifications-buffer--insert-content-into-section
     section
     (lambda () (shipit-notifications-buffer--insert-activity-details activity)))
    ;; PR-only: fetch rich PR metadata (labels, branch, author, draft state)
    (when (and (string= type "pr") repo (integerp number))
      (condition-case err
          (let* ((resolved (shipit-pr--resolve-for-repo repo))
                 (backend (car resolved))
                 (config (cdr resolved))
                 (fetch-fn (plist-get backend :fetch-pr)))
            (when fetch-fn
              (message "Fetching PR details...")
              (let ((pr-data (funcall fetch-fn config number)))
                (when pr-data
                  (shipit-notifications-buffer--update-pr-details section pr-data)))
              (message nil)))
        (error
         (shipit--debug-log "Failed to fetch PR details: %s" (error-message-string err)))))
    ;; PR and Issue: fetch activity timeline async and insert events
    ;; since the notification was triggered.
    (when (and (or (string= type "pr") (string= type "issue"))
               repo (integerp number) updated-at)
      (condition-case err
          (let ((buf (current-buffer))
                (sect section))
            (shipit--fetch-recent-timeline-events-async
             repo number
             (lambda (events)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((inhibit-read-only t))
                     (save-excursion
                       (goto-char (marker-position (oref sect end)))
                       (let ((magit-insert-section--parent sect))
                         (shipit-notifications-buffer--insert-activity-section
                          events updated-at repo number))
                       (oset sect end (point-marker)))))))))
        (error
         (shipit--debug-log "Failed to fetch timeline events: %s"
                            (error-message-string err)))))))

(defun shipit-notifications-buffer--update-pr-details (section pr-data)
  "Update SECTION with richer PR-DATA."
  (let* ((inhibit-read-only t)
         (content-pos (oref section content))
         (end-pos (oref section end))
         (state (cdr (assq 'state pr-data)))
         (merged (cdr (assq 'merged pr-data)))
         (labels (cdr (assq 'labels pr-data)))
         (head (cdr (assq 'head pr-data)))
         (branch (when head (cdr (assq 'ref head))))
         (user (cdr (assq 'user pr-data)))
         (author (when user (cdr (assq 'login user))))
         (body-text (cdr (assq 'body pr-data)))
         (draft (let ((d (cdr (assq 'draft pr-data))))
                  (and d (not (eq d :json-false))))))
    (when (and content-pos end-pos)
      (save-excursion
        (goto-char (marker-position end-pos))
        (let ((magit-insert-section--parent section))
          (when author
            (let* ((avatar-url (when user (cdr (assq 'avatar_url user))))
                   (avatar (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                    avatar-url (fboundp 'shipit--create-avatar-display))
                               (concat (shipit--create-avatar-display author avatar-url 16) " ")
                             "")))
              (insert "    Author: " avatar
                      (propertize author 'face 'shipit-username-face)
                      "\n")))
          (when (and body-text (stringp body-text) (not (string-empty-p body-text)))
            (let ((desc-sect (magit-insert-section (notification-description body-text)
                               (magit-insert-heading "    Description")
                               (let ((body-start (point))
                                     (rendered (shipit--render-markdown body-text)))
                                 (insert rendered)
                                 (let ((fill-column (- (or (window-body-width) 80) 6)))
                                   (fill-region body-start (point)))
                                 (indent-rigidly body-start (point) 6))
                               (insert "\n"))))
              (magit-section-hide desc-sect)))
          (when state
            (insert "    Status: "
                    (propertize (cond (merged "merged")
                                     ((string= state "closed") "closed")
                                     (draft "draft")
                                     (t "open"))
                                'face (cond (merged 'magit-tag-face)
                                            ((string= state "closed") 'error)
                                            (draft 'shadow)
                                            (t 'success)))
                    "\n"))
          (when branch
            (insert "    Branch: "
                    (propertize branch 'face 'magit-branch-remote)
                    "\n"))
          (when (and labels (> (length labels) 0))
            (insert "    Labels: "
                    (mapconcat (lambda (l)
                                 (propertize (cdr (assq 'name l))
                                             'face 'magit-tag-face))
                               labels ", ")
                    "\n")))
        (oset section end (point-marker))))))

;;; Activity timeline

(defun shipit-notifications-buffer--adjust-cutoff (cutoff-timestamp margin-seconds)
  "Subtract MARGIN-SECONDS from CUTOFF-TIMESTAMP (ISO 8601).
Returns an adjusted ISO 8601 timestamp string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-subtract (date-to-time cutoff-timestamp)
                                     margin-seconds)
                      t))

(defun shipit-notifications-buffer--filter-events-since (events cutoff-timestamp)
  "Filter EVENTS to those near or after CUTOFF-TIMESTAMP.
Applies a 5-minute margin before the cutoff to catch events that
triggered the notification.  Uses created_at, submitted_at, or
updated_at from each event."
  (let ((adjusted (shipit-notifications-buffer--adjust-cutoff cutoff-timestamp 300)))
    (seq-filter
     (lambda (event)
       (let ((ts (or (cdr (assq 'created_at event))
                     (cdr (assq 'submitted_at event))
                     (cdr (assq 'updated_at event)))))
         (and ts (not (string< ts adjusted)))))
     events)))

(defun shipit-notifications-buffer--insert-activity-section (events cutoff repo pr-number)
  "Insert a notification-activity sub-section with EVENTS since CUTOFF.
REPO and PR-NUMBER provide context for event rendering.
Caller must bind `magit-insert-section--parent' to the parent section.
Body content is indented 2 columns past the heading start."
  (let* ((filtered (shipit-notifications-buffer--filter-events-since events cutoff))
         (count (length filtered))
         (author-mapping (make-hash-table :test 'equal))
         (heading-indent 4)
         (heading (format "%s%s Activity (%d since notification)"
                          (make-string heading-indent ?\s)
                          (shipit--get-pr-field-icon "activity" "💥")
                          count))
)
    (let ((sect (magit-insert-section (notification-activity filtered)
                  (magit-insert-heading heading)
                  (if (> count 0)
                      (dolist (event filtered)
                        (shipit--insert-activity-event event repo pr-number author-mapping heading-indent))
                    (insert (make-string (+ heading-indent 3) ?\s) "(no recent activity)\n")))))
      sect)))

;;; Actions

(defun shipit-notifications-buffer-copy-url ()
  "Copy the notification URL to the kill ring.
If a region is active, copy the region text instead (standard M-w behavior)."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (let* ((repo (get-text-property (point) 'shipit-repo))
           (number (get-text-property (point) 'shipit-pr-number))
           (stored-url (get-text-property (point) 'shipit-notification-browse-url))
           (type (get-text-property (point) 'shipit-notification-type))
           (url (or stored-url
                    (when (and repo number)
                      (if (string= type "issue")
                          (shipit--browse-issue-url repo number)
                        (shipit--browse-pr-url repo number))))))
      (if url
          (progn
            (kill-new url)
            (message "Copied: %s" url))
        ;; No notification — fall back to global M-w binding
        (let ((fallback (global-key-binding (kbd "M-w"))))
          (when fallback
            (call-interactively fallback)))))))

(defun shipit-notifications-buffer-watch ()
  "Open subscription transient for the repo of the notification at point."
  (interactive)
  (let ((activity (shipit-notifications-buffer--activity-at-point)))
    (if activity
        (let ((repo (cdr (assq 'repo activity))))
          (if repo
              (progn
                (setq-local shipit-repo-buffer-repo repo)
                (shipit-repo-subscription))
            (user-error "No repo context for this notification")))
      (user-error "No notification at point"))))

(defun shipit-notifications-buffer--activity-at-point ()
  "Return the notification activity alist at point, or nil.
Walks up from child sections, then searches backward if needed."
  (let ((section (magit-current-section)))
    ;; Walk up parents first
    (let ((s section))
      (while (and s (not (eq (oref s type) 'notification-entry)))
        (setq s (oref s parent)))
      (when s (setq section s)))
    ;; If not found (plain text in section body), search backward
    (when (or (not section) (eq (oref section type) 'notifications-root))
      (save-excursion
        (while (and (not (bobp))
                    (let ((s (get-text-property (point) 'magit-section)))
                      (not (and s (eq (oref s type) 'notification-entry)))))
          (forward-line -1))
        (let ((s (get-text-property (point) 'magit-section)))
          (when (and s (eq (oref s type) 'notification-entry))
            (setq section s)))))
    (when (and section (eq (oref section type) 'notification-entry))
      (oref section value))))

(defun shipit-notifications-buffer--get-notification-at-point ()
  "Get notification data at point as plist (:repo :pr-number :type :notification)."
  (let ((activity (shipit-notifications-buffer--activity-at-point)))
    (when activity
      (list :repo (cdr (assq 'repo activity))
            :pr-number (or (cdr (assq 'number activity))
                           (cdr (assq 'pr-number activity)))
            :type (or (cdr (assq 'type activity)) "pr")
            :notification (cdr (assq 'notification activity))))))

(defun shipit-notifications-buffer--activity-props-at-point ()
  "Return a navigation props plist if point is on an activity line, else nil.
An activity line carries a `shipit-event-type' text property; the returned
plist has the keys consumed by `shipit-pr--dispatch-activity-navigation'."
  (when-let* ((event-type (get-text-property (point) 'shipit-event-type)))
    (list :event-type event-type
          :comment-id (get-text-property (point) 'shipit-activity-comment-id)
          :commit-sha (get-text-property (point) 'shipit-activity-commit-sha)
          :review-state (get-text-property (point) 'shipit-review-state)
          :crossref-repo (get-text-property (point) 'shipit-crossref-repo)
          :crossref-number (get-text-property (point) 'shipit-crossref-number)
          :crossref-url (get-text-property (point) 'shipit-crossref-url)
          :crossref-title (get-text-property (point) 'shipit-crossref-title)
          :inline-comment-path (get-text-property (point) 'shipit-inline-comment-path))))

(defun shipit-notifications-buffer--schedule-activity-nav (buffer props)
  "In BUFFER, dispatch activity navigation using PROPS once the buffer is ready.
Waits on `shipit-buffer-ready-hook' for shipit PR buffers with pending
async sections; on `shipit-issue-buffer-ready-hook' for issue buffers
whose comments are still loading; otherwise dispatches immediately.
The hook handler is one-shot and buffer-local."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((pr-pending (and (boundp 'shipit-buffer--pending-async-sections)
                              shipit-buffer--pending-async-sections))
             (issue-pending (and (derived-mode-p 'shipit-issue-mode)
                                 (boundp 'shipit-issue-buffer-ready-hook)))
             (hook-var (cond (pr-pending 'shipit-buffer-ready-hook)
                             (issue-pending 'shipit-issue-buffer-ready-hook))))
        (if hook-var
            (let ((fn-sym (make-symbol "shipit-nav-on-ready")))
              (fset fn-sym
                    (lambda ()
                      (remove-hook hook-var fn-sym t)
                      (shipit-pr--dispatch-activity-navigation props)))
              (add-hook hook-var fn-sym nil t))
          (shipit-pr--dispatch-activity-navigation props))))))


(defun shipit-notifications-buffer-open ()
  "Open the notification at point directly.
Opens PR/Issue/Discussion buffer, or browser for RSS.
If point is on an activity line within an expanded PR notification,
also navigates to that activity inside the opened PR buffer.
With prefix arg (C-u), show action menu instead."
  (interactive)
  (if current-prefix-arg
      (shipit-notifications-buffer-action)
    (let ((activity (shipit-notifications-buffer--activity-at-point))
          (activity-props (shipit-notifications-buffer--activity-props-at-point)))
      (if (not activity)
          (user-error "No notification at point")
        (let* ((type (or (cdr (assq 'type activity)) "pr"))
               (repo (cdr (assq 'repo activity)))
               (number (or (cdr (assq 'number activity))
                           (cdr (assq 'pr-number activity)))))
          (pcase type
            ("pr"
             (let ((buffer (shipit--open-notification-pr number repo activity)))
               (when (and activity-props (bufferp buffer))
                 (shipit-notifications-buffer--schedule-activity-nav
                  buffer activity-props))))
            ("issue"
             (let ((buffer (shipit--open-notification-issue number repo activity)))
               (when (and activity-props (bufferp buffer))
                 (shipit-notifications-buffer--schedule-activity-nav
                  buffer activity-props))))
            ("discussion" (shipit--open-notification-discussion number repo))
            ("rss" (when-let* ((url (cdr (assq 'browse-url activity))))
                     (browse-url url)))
            (_ (message "Unknown notification type: %s" type))))))))

(defun shipit-notifications-buffer-action ()
  "Show action menu for notification at point.
For RSS entries, opens the link in browser directly."
  (interactive)
  (let ((activity (shipit-notifications-buffer--activity-at-point)))
    (if (not activity)
        (user-error "No notification at point")
      (if (eq (cdr (assq 'source activity)) 'rss)
          (shipit-notifications-buffer--rss-action activity)
        (if (fboundp 'shipit--notification-actions)
            (shipit--notification-actions activity)
          (message "Notification actions not available"))))))

(defun shipit-notifications-buffer--rss-action (activity)
  "Handle action for RSS notification ACTIVITY."
  (let* ((url (cdr (assq 'browse-url activity)))
         (section (magit-current-section))
         (actions (list "Open in browser" "Mark as read"))
         (choice (completing-read "RSS action: " actions nil t)))
    (cond
     ((string= choice "Open in browser")
      (when url (browse-url url)))
     ((string= choice "Mark as read")
      (let ((number (cdr (assq 'number activity)))
            (repo (cdr (assq 'repo activity))))
        (when (fboundp 'shipit--mark-notification-read)
          (shipit--mark-notification-read number repo t "rss"))
        (when section
          (let ((inhibit-read-only t))
            (delete-region (oref section start) (oref section end))
            (when-let* ((parent (oref section parent)))
              (oset parent children
                    (delq section (oref parent children)))))))))))

;;; Mark as read

(defun shipit-notifications-buffer-mark-read ()
  "Mark notification at point as read, or all notifications in region."
  (interactive)
  (if (use-region-p)
      (shipit-notifications-buffer--mark-region-read)
    (shipit-notifications-buffer--mark-single-read)))

(defun shipit-notifications-buffer--mark-single-read ()
  "Mark the single notification at point as read and remove the section."
  (let* ((data (shipit-notifications-buffer--get-notification-at-point))
         (activity (shipit-notifications-buffer--activity-at-point))
         ;; Find the notification-entry section (not root)
         (section (let ((s (magit-current-section)))
                    (while (and s (not (eq (oref s type) 'notification-entry)))
                      (setq s (oref s parent)))
                    s)))
    ;; If walk-up failed, try backward search
    (unless section
      (save-excursion
        (while (and (not (bobp))
                    (let ((s (get-text-property (point) 'magit-section)))
                      (not (and s (eq (oref s type) 'notification-entry)))))
          (forward-line -1))
        (setq section (get-text-property (point) 'magit-section))))
    (if data
        (let ((repo (plist-get data :repo))
              (pr-number (plist-get data :pr-number))
              (type (or (plist-get data :type) "pr")))
          (shipit--debug-log "NOTIF-BUFFER: Marking %s %s#%s as read" type repo pr-number)
          (when (fboundp 'shipit--mark-notification-read)
            (shipit--mark-notification-read pr-number repo t type))
          ;; Remove the section in-place instead of full re-render
          (when (and section (eq (oref section type) 'notification-entry))
            (let ((inhibit-read-only t))
              (delete-region (oref section start) (oref section end))
              (when-let* ((parent (oref section parent)))
                (oset parent children
                      (delq section (oref parent children)))))))
      (user-error "No notification at point"))))

(defun shipit-notifications-buffer--mark-region-read ()
  "Mark all notifications in the active region as read."
  (let ((beg (region-beginning))
        (end (region-end))
        (notifications '())
        (seen (make-hash-table :test 'equal)))
    ;; Walk lines in region, collect unique notifications
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((section (get-text-property (point) 'magit-section))
               (type (when section (oref section type))))
          (shipit--debug-log "mark-region: line=%d section-type=%s"
                             (line-number-at-pos) type)
          (when (and section (eq type 'notification-entry))
            (let* ((activity (oref section value))
                   (repo (cdr (assq 'repo activity)))
                   (number (or (cdr (assq 'number activity))
                               (cdr (assq 'pr-number activity))))
                   (ntype (or (cdr (assq 'type activity)) "pr"))
                   (key (format "%s:%s:%s" repo ntype number)))
              (unless (gethash key seen)
                (puthash key t seen)
                (push (list number repo ntype) notifications)))))
        (forward-line 1)))
    (when notifications
      (let ((count (length notifications)))
        (dolist (notif (nreverse notifications))
          (when (fboundp 'shipit--mark-notification-read)
            (shipit--mark-notification-read (car notif) (cadr notif) t (caddr notif))))
        (deactivate-mark)
        (message "Marked %d notifications as read" count)
        (shipit-notifications-buffer--rerender)))))

(defun shipit-notifications-buffer-mark-all-read ()
  "Mark all visible notifications as read."
  (interactive)
  (let ((notifications '()))
    (when (bound-and-true-p magit-root-section)
      (dolist (child (oref magit-root-section children))
        (when (eq (oref child type) 'notification-entry)
          (let ((activity (oref child value)))
            (push (list (or (cdr (assq 'number activity))
                            (cdr (assq 'pr-number activity)))
                        (cdr (assq 'repo activity))
                        (or (cdr (assq 'type activity)) "pr"))
                  notifications)))))
    (if notifications
        (when (yes-or-no-p (format "Mark %d notification%s as read? "
                                   (length notifications)
                                   (if (= 1 (length notifications)) "" "s")))
          (dolist (notif notifications)
            (when (fboundp 'shipit--mark-notification-read)
              (shipit--mark-notification-read (car notif) (cadr notif) t (caddr notif))))
          (message "Marked %d notifications as read" (length notifications))
          (shipit-notifications-buffer-refresh))
      (message "No notifications to mark as read"))))

;;; Filter

(defun shipit-notifications-buffer-set-filter ()
  "Set or clear the filter for notifications with live updates as you type."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (timer nil)
         (minibuf nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuf (current-buffer))
          (add-hook 'post-command-hook
                    (lambda ()
                      (when timer
                        (cancel-timer timer))
                      (setq timer
                            (run-with-idle-timer 0.05 nil
                                                 (lambda ()
                                                   (condition-case nil
                                                       (when (and (buffer-live-p minibuf)
                                                                  (minibufferp minibuf))
                                                         (let ((current-input (with-current-buffer minibuf
                                                                                (minibuffer-contents-no-properties))))
                                                           (when (buffer-live-p original-buffer)
                                                             (with-current-buffer original-buffer
                                                               (setq shipit-notifications-buffer--filter-text current-input)
                                                               (shipit-notifications-buffer--rerender)
                                                               (redisplay t)))))
                                                     (error nil))))))
                    nil t))
      (let ((new-filter (read-string "Filter notifications: "
                                     shipit-notifications-buffer--filter-text)))
        (setq shipit-notifications-buffer--filter-text new-filter)
        (shipit-notifications-buffer--rerender)))))

(provide 'shipit-notifications-buffer)
;;; shipit-notifications-buffer.el ends here
