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
(require 'transient)
(require 'shipit-core)
(require 'magit-section)

;; Forward declarations
(declare-function shipit--mark-notification-read "shipit-notifications")
(declare-function shipit--notification-pr-actions "shipit-notifications")
(declare-function shipit--notification-actions "shipit-notifications")
(declare-function shipit--notification-activity-key "shipit-notifications")
(declare-function shipit--check-notifications-background "shipit-notifications")
(declare-function shipit--fetch-notifications-total-count-async "shipit-notifications")
(declare-function shipit-subscriptions--backends-with-watched-repos "shipit-subscriptions-buffer")
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

(defvar-local shipit-notifications-buffer--display-scope 'unread
  "Buffer-local display scope for the notifications buffer.
One of `unread' (GitHub's unread feed, usually a single short page)
or `all' (include read notifications, paginated on demand via
`shipit-notifications-buffer-load-more').  Kept buffer-local so
toggling in the buffer does not affect the background poll's
`shipit-notifications-scope' setting.")

(defvar-local shipit-notifications-buffer--page-limit 1
  "Number of pages currently loaded into the notifications buffer.
Meaningful mainly when `shipit-notifications-buffer--display-scope'
is `all'; incremented by `shipit-notifications-buffer-load-more'.
Starts at 1 and resets to 1 on every scope toggle so switching
views doesn't silently fan out 10 pages of network requests.")

(defvar-local shipit-notifications-buffer--total-count nil
  "Last known total notification count for the current scope.
Populated asynchronously by `shipit--fetch-notifications-total-count-async'
after each refresh; nil means the probe has not returned yet (or failed).")

(defvar-local shipit-notifications-buffer--repo-filter nil
  "Buffer-local server-side repo filter for the notifications buffer.
Nil means all repos; a `OWNER/REPO' string targets that repo's
per-repo notifications endpoint on the backend.  Unlike the
text filter (client-side only), this makes both the main fetch
and the total-count probe exact for the selected repo.")

(defvar shipit-notifications-buffer--watched-repos-cache nil
  "Session cache of subscribed/watched repos for the repo-filter picker.
Populated on first open of the repo-filter picker via the backends'
`:fetch-watched-repos'; reused for later picker invocations so the
GraphQL round-trip only happens once per session.  Cleared by
`shipit-notifications-buffer-refresh-watched-repos-cache'.")

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
    (define-key map (kbd "f") #'shipit-notifications-buffer-filter-menu)
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
Fetches fresh data from the API before re-rendering using the
buffer-local `shipit-notifications-buffer--display-scope',
`shipit-notifications-buffer--page-limit', and
`shipit-notifications-buffer--repo-filter'.
Arguments IGNORE-AUTO and NOCONFIRM are for compatibility with `revert-buffer'."
  (interactive)
  (message "Fetching notifications...")
  (let ((scope shipit-notifications-buffer--display-scope)
        (pages shipit-notifications-buffer--page-limit)
        (repo shipit-notifications-buffer--repo-filter)
        (buf (current-buffer)))
    (when (fboundp 'shipit--check-notifications-background)
      (shipit--check-notifications-background t scope pages repo))
    (when (fboundp 'shipit--fetch-notifications-total-count-async)
      (shipit--fetch-notifications-total-count-async
       scope
       (lambda (count)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq shipit-notifications-buffer--total-count count)
             (shipit-notifications-buffer--rerender))))
       repo)))
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
  "Render the notifications buffer content.
Bind `shipit-notifications-buffer--render-pool' once per render so
the three callers that need the repo-filtered activity pool share
a single hash walk + filter."
  (let ((shipit-notifications-buffer--render-pool
         (seq-filter #'shipit-notifications-buffer--matches-repo-filter-p
                     (shipit-notifications-buffer--all-activities))))
    (magit-insert-section (notifications-root)
      (shipit-notifications-buffer--insert-header)
      (shipit-notifications-buffer--insert-notifications))))

(defvar shipit-notifications-buffer--render-pool nil
  "Dynamic cache of the repo-filtered activity pool during one render.
Bound by `shipit-notifications-buffer--render' so header helpers and
the list renderer share a single hash walk per render instead of
repeating it for every count call.  Nil outside the render.")

(defun shipit-notifications-buffer--all-activities ()
  "Return the list of all activities in the global hash (unfiltered)."
  (let ((all '()))
    (when (and (boundp 'shipit--notification-pr-activities)
               shipit--notification-pr-activities)
      (maphash (lambda (_k v) (push v all))
               shipit--notification-pr-activities))
    all))

(defun shipit-notifications-buffer--matches-repo-filter-p (activity)
  "Return non-nil if ACTIVITY passes the buffer-local repo filter.
When no repo filter is set, always returns t.  The comparison is
case-insensitive because GitHub repo names are case-insensitive."
  (let ((filter shipit-notifications-buffer--repo-filter))
    (or (null filter)
        (let ((repo (cdr (assq 'repo activity))))
          (and (stringp repo)
               (string-equal-ignore-case repo filter))))))

(defun shipit-notifications-buffer--repo-filtered-activities ()
  "Activities after the server-side repo filter (still all text-filter states).
Reuses `shipit-notifications-buffer--render-pool' when set so the
hash walk + repo filter happen once per render instead of three
times (header shown-count, header loaded-count, list renderer)."
  (or shipit-notifications-buffer--render-pool
      (seq-filter #'shipit-notifications-buffer--matches-repo-filter-p
                  (shipit-notifications-buffer--all-activities))))

(defun shipit-notifications-buffer--loaded-count ()
  "Return the number of activities visible under the repo filter.
Ignores the text filter — this is the denominator when a text
filter is active, so the user sees `<matches>/<loaded>'."
  (length (shipit-notifications-buffer--repo-filtered-activities)))

(defun shipit-notifications-buffer--shown-count ()
  "Return the number of activities currently rendered in the buffer.
Applies both the repo filter and the text filter."
  (let ((pool (shipit-notifications-buffer--repo-filtered-activities)))
    (if (string-empty-p shipit-notifications-buffer--filter-text)
        (length pool)
      (length (seq-filter #'shipit-notifications-buffer--matches-filter-p pool)))))

(defun shipit-notifications-buffer--insert-header ()
  "Insert the buffer header, including scope, page info, and count.
Fraction semantics: when a text filter is active the denominator is
the number of activities loaded in the buffer (so the user sees how
many local items match); otherwise the denominator is the
probe-derived server total for the current scope, when known."
  (insert (propertize "Notifications" 'font-lock-face 'bold))
  (let* ((shown (shipit-notifications-buffer--shown-count))
         (loaded (shipit-notifications-buffer--loaded-count))
         (total shipit-notifications-buffer--total-count)
         (filter-active (not (string-empty-p
                              shipit-notifications-buffer--filter-text)))
         (count-part (cond
                      ;; Filter is client-side, so the true matches-on-server
                      ;; count is unknowable without pulling every page.
                      ;; Show matches/loaded, plus the server total in parens
                      ;; when we know it, so the user sees why the denominator
                      ;; may seem small.
                      ((and filter-active total)
                       (format ", %d/%d (of %d)" shown loaded total))
                      (filter-active (format ", %d/%d" shown loaded))
                      (total (format ", %d/%d" shown total))
                      (t (format ", %d shown" shown))))
         (pages-part (if (eq shipit-notifications-buffer--display-scope 'all)
                         (format ", pages: %d"
                                 shipit-notifications-buffer--page-limit)
                       "")))
    (insert (propertize
             (format "  [%s%s%s]"
                     (symbol-name shipit-notifications-buffer--display-scope)
                     pages-part count-part)
             'font-lock-face 'font-lock-comment-face)))
  (when shipit-notifications-buffer--repo-filter
    (insert (propertize
             (format "  [repo: %s]" shipit-notifications-buffer--repo-filter)
             'font-lock-face 'font-lock-comment-face)))
  (when (not (string-empty-p shipit-notifications-buffer--filter-text))
    (insert (propertize (format "  [filter: %s]" shipit-notifications-buffer--filter-text)
                        'font-lock-face 'font-lock-comment-face)))
  (insert "\n\n"))

(defun shipit-notifications-buffer--insert-notifications ()
  "Insert all notification entries as magit sections.
Applies the repo filter and then the text filter, using the same
helpers as the header counts so the two stay in sync."
  (require 'shipit-notifications)
  (let ((activities (shipit-notifications-buffer--repo-filtered-activities)))
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
      (insert (propertize "  No notifications\n" 'font-lock-face 'font-lock-comment-face)))))

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
                                        'font-lock-face 'font-lock-constant-face)
                            pr-str
                            spacer
                            (truncate-string-to-width subject title-width nil ?\s)
                            spacer
                            (propertize (truncate-string-to-width reason reason-width nil ?\s)
                                        'font-lock-face 'font-lock-keyword-face)))
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
                        'font-lock-face 'shipit-timestamp-face
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
    (insert "    " (propertize type-label 'font-lock-face 'bold)
            " " number-str " in " repo "\n")
    (insert "    " subject "\n")
    (when (and reason (not (string-empty-p reason)))
      (insert "    Reason: " (propertize reason 'font-lock-face 'font-lock-keyword-face) "\n"))
    (when updated
      (insert "    Updated: " (propertize (shipit--format-timestamp updated)
                                          'font-lock-face 'shipit-timestamp-face) "\n"))
    (when url
      (insert "    " (propertize url 'font-lock-face 'link) "\n"))))

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
      (replace-match (propertize (match-string 1) 'font-lock-face 'bold)))
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
                      (propertize author 'font-lock-face 'shipit-username-face)
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
                                'font-lock-face (cond (merged 'magit-tag-face)
                                            ((string= state "closed") 'error)
                                            (draft 'shadow)
                                            (t 'success)))
                    "\n"))
          (when branch
            (insert "    Branch: "
                    (propertize branch 'font-lock-face 'magit-branch-remote)
                    "\n"))
          (when (and labels (> (length labels) 0))
            (insert "    Labels: "
                    (mapconcat (lambda (l)
                                 (propertize (cdr (assq 'name l))
                                             'font-lock-face 'magit-tag-face))
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

;;; Scope / pagination

(defun shipit-notifications-buffer-toggle-scope ()
  "Flip between `unread' and `all' display scopes.
Resets `shipit-notifications-buffer--page-limit' to 1 so that
switching views never silently fans out 10 pages of API requests,
then refreshes."
  (interactive)
  (setq shipit-notifications-buffer--display-scope
        (if (eq shipit-notifications-buffer--display-scope 'all) 'unread 'all))
  (setq shipit-notifications-buffer--page-limit 1)
  (message "Notifications scope: %s"
           shipit-notifications-buffer--display-scope)
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-load-more ()
  "Fetch one more page of notifications in `all' scope.
Refuses in `unread' scope, where GitHub's own feed is already
scoped to what's unread and pagination is rarely meaningful."
  (interactive)
  (unless (eq shipit-notifications-buffer--display-scope 'all)
    (user-error "Load-more only applies in 'all' scope (current: %s)"
                shipit-notifications-buffer--display-scope))
  (cl-incf shipit-notifications-buffer--page-limit)
  (message "Loading page %d..." shipit-notifications-buffer--page-limit)
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-clear-filter ()
  "Clear the live text filter."
  (interactive)
  (setq shipit-notifications-buffer--filter-text "")
  (shipit-notifications-buffer--rerender))

;;; Repo filter (server-side)

(defun shipit-notifications-buffer--gather-watched-repos ()
  "Fetch watched repos from all PR backends, caching for the session.
Returns a sorted list of `OWNER/REPO' strings.  Uses
`shipit-notifications-buffer--watched-repos-cache' so the first
picker invocation pays the round-trip but later ones are instant."
  (require 'shipit-subscriptions-buffer)
  (unless shipit-notifications-buffer--watched-repos-cache
    (let ((repos '()))
      (dolist (entry (shipit-subscriptions--backends-with-watched-repos))
        (let* ((backend-plist (cdr entry))
               (config (list :repo ""))
               (fetch-fn (plist-get backend-plist :fetch-watched-repos)))
          (dolist (repo (condition-case err
                            (funcall fetch-fn config)
                          (error
                           (shipit--debug-log
                            "Watched-repos fetch failed: %S" err)
                           nil)))
            (let ((name (cdr (assq 'full_name repo))))
              (when (and name (stringp name) (> (length name) 0))
                (push name repos))))))
      (setq shipit-notifications-buffer--watched-repos-cache
            (sort (delete-dups repos) #'string<))))
  shipit-notifications-buffer--watched-repos-cache)

(defun shipit-notifications-buffer--candidate-repos ()
  "Return candidate repos for the repo-filter picker.
Union of subscribed repos (from the PR backends) and repos
currently visible in the notifications hash, so the picker works
even before the backend fetch returns."
  (let ((from-hash '()))
    (dolist (a (shipit-notifications-buffer--all-activities))
      (let ((r (cdr (assq 'repo a))))
        (when (and r (stringp r) (> (length r) 0))
          (push r from-hash))))
    (let ((watched (ignore-errors
                     (shipit-notifications-buffer--gather-watched-repos))))
      (sort (delete-dups (append watched from-hash)) #'string<))))

(defun shipit-notifications-buffer-set-repo-filter ()
  "Prompt for a repo to scope the notifications buffer to server-side.
Candidates come from the PR backends' `:fetch-watched-repos' plus
any repos already present in the hash.  Picking the clear-label
entry unscopes the filter.  Triggers a refresh so both the main
fetch and the total-count probe hit the per-repo endpoint."
  (interactive)
  (let* ((clear-label "<all repos>")
         (candidates (cons clear-label
                           (shipit-notifications-buffer--candidate-repos)))
         (choice (completing-read "Repo: " candidates nil t nil nil
                                  (or shipit-notifications-buffer--repo-filter
                                      clear-label))))
    (setq shipit-notifications-buffer--repo-filter
          (if (string= choice clear-label) nil choice))
    (setq shipit-notifications-buffer--page-limit 1)
    (message "Repo filter: %s"
             (or shipit-notifications-buffer--repo-filter "all repos"))
    (shipit-notifications-buffer-refresh)))

(defun shipit-notifications-buffer-clear-repo-filter ()
  "Clear the server-side repo filter and refresh."
  (interactive)
  (setq shipit-notifications-buffer--repo-filter nil)
  (setq shipit-notifications-buffer--page-limit 1)
  (message "Repo filter cleared")
  (shipit-notifications-buffer-refresh))

(defun shipit-notifications-buffer-refresh-watched-repos-cache ()
  "Force a refetch of the repo-filter candidate cache next time."
  (interactive)
  (setq shipit-notifications-buffer--watched-repos-cache nil)
  (message "Repo-filter candidate cache cleared"))

;;; Filter

(defun shipit-notifications-buffer--scope-description ()
  "Describe the current scope toggle for the transient."
  (format "Scope: %s  (toggle unread/all)"
          shipit-notifications-buffer--display-scope))

(defun shipit-notifications-buffer--load-more-description ()
  "Describe the load-more action for the transient."
  (if (eq shipit-notifications-buffer--display-scope 'all)
      (format "Load more (next page: %d)"
              (1+ shipit-notifications-buffer--page-limit))
    "Load more (unavailable in 'unread' scope)"))

(defun shipit-notifications-buffer--text-filter-description ()
  "Describe the text-filter action for the transient."
  (if (string-empty-p shipit-notifications-buffer--filter-text)
      "Text filter…"
    (format "Text filter: %s" shipit-notifications-buffer--filter-text)))

(defun shipit-notifications-buffer--repo-filter-description ()
  "Describe the repo-filter action for the transient."
  (if shipit-notifications-buffer--repo-filter
      (format "Repo: %s" shipit-notifications-buffer--repo-filter)
    "Repo: <all repos>"))

;;;###autoload (autoload 'shipit-notifications-buffer-filter-menu "shipit-notifications-buffer" nil t)
(transient-define-prefix shipit-notifications-buffer-filter-menu ()
  "Filter and scope controls for the notifications buffer."
  [["Scope"
    ("s" shipit-notifications-buffer-toggle-scope
     :description shipit-notifications-buffer--scope-description)
    ("m" shipit-notifications-buffer-load-more
     :description shipit-notifications-buffer--load-more-description
     :transient t)]
   ["Server-side"
    ("r" shipit-notifications-buffer-set-repo-filter
     :description shipit-notifications-buffer--repo-filter-description)
    ("R" "Clear repo filter" shipit-notifications-buffer-clear-repo-filter)]
   ["Text filter"
    ("t" shipit-notifications-buffer-set-filter
     :description shipit-notifications-buffer--text-filter-description)
    ("c" "Clear text filter" shipit-notifications-buffer-clear-filter)]
   ["Refresh"
    ("g" "Refresh now" shipit-notifications-buffer-refresh)
    ("q" "Quit" transient-quit-one)]])

(defcustom shipit-notifications-filter-live-delay 0.25
  "Idle delay in seconds before re-rendering on live text-filter typing.
Raising this makes fast typing less laggy at the cost of a slightly
delayed preview; lowering it makes the preview feel more immediate
at the cost of extra renders per keystroke."
  :type 'number
  :group 'shipit)

(defun shipit-notifications-buffer-set-filter ()
  "Set or clear the filter for notifications with live updates as you type.
Updates are debounced by `shipit-notifications-filter-live-delay' so
bursty typing re-renders the notifications buffer once per pause,
not once per keystroke; the text filter is also checked against
the cached last-rendered value so unchanged input is a no-op."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (timer nil)
         (minibuf nil)
         (last-rendered nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuf (current-buffer))
          (add-hook 'post-command-hook
                    (lambda ()
                      (when timer (cancel-timer timer))
                      (setq timer
                            (run-with-idle-timer
                             shipit-notifications-filter-live-delay nil
                             (lambda ()
                               (condition-case nil
                                   (when (and (buffer-live-p minibuf)
                                              (minibufferp minibuf))
                                     (let ((current-input
                                            (with-current-buffer minibuf
                                              (minibuffer-contents-no-properties))))
                                       (when (and (buffer-live-p original-buffer)
                                                  (not (equal current-input
                                                               last-rendered)))
                                         (setq last-rendered current-input)
                                         (with-current-buffer original-buffer
                                           (setq shipit-notifications-buffer--filter-text
                                                 current-input)
                                           (shipit-notifications-buffer--rerender)))))
                                 (error nil))))))
                    nil t))
      (let ((new-filter (read-string "Filter notifications: "
                                     shipit-notifications-buffer--filter-text)))
        (setq shipit-notifications-buffer--filter-text new-filter)
        (shipit-notifications-buffer--rerender)))))

(provide 'shipit-notifications-buffer)
;;; shipit-notifications-buffer.el ends here
