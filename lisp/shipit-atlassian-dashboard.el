;;; shipit-atlassian-dashboard.el --- Atlassian/Jira project dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

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
;; Dedicated dashboard buffer for Atlassian/Jira projects.
;; Displays My Open Issues, What's Next, Kanban Board, and
;; Frequently Visited sections using magit-section for collapsible display.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magit-section)
(require 'transient)
(require 'url-util)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-atlassian-board)

;; Forward declarations
(declare-function shipit-issue-jira--api-request "shipit-issue-jira")
(declare-function shipit-issue-jira--browse-url "shipit-issue-jira")
(declare-function shipit-issue-jira--search-page "shipit-issue-jira")
(declare-function shipit-issue-jira--normalize-issue "shipit-issue-jira")
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit-dwim "shipit-pr-actions")
(declare-function shipit-register-dwim-handler "shipit-pr-actions")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-issue-buffer-name "shipit-issues-buffer")
(defvar shipit-issue-buffer-data)
(defvar shipit-issue-buffer-repo)

;;; Section type declarations

(defun atlassian-dashboard (&rest _args)
  "Magit section identifier for Atlassian dashboard root.")
(put 'atlassian-dashboard 'magit-section t)

(defun atlassian-my-issues (&rest _args)
  "Magit section identifier for My Open Issues.")
(put 'atlassian-my-issues 'magit-section t)

(defun atlassian-my-issues-project (&rest _args)
  "Magit section identifier for My Open Issues project group.")
(put 'atlassian-my-issues-project 'magit-section t)

(defun atlassian-whats-next (&rest _args)
  "Magit section identifier for What's Next.")
(put 'atlassian-whats-next 'magit-section t)

(defun atlassian-board (&rest _args)
  "Magit section identifier for Kanban Board.")
(put 'atlassian-board 'magit-section t)

(defun atlassian-board-column (&rest _args)
  "Magit section identifier for a Kanban Board column.")
(put 'atlassian-board-column 'magit-section t)

(defun atlassian-frequently-visited (&rest _args)
  "Magit section identifier for Frequently Visited.")
(put 'atlassian-frequently-visited 'magit-section t)

;;; Customization

(defgroup shipit-atlassian nil
  "Atlassian/Jira dashboard settings."
  :group 'shipit
  :prefix "shipit-atlassian-")

(defcustom shipit-atlassian-board-id nil
  "Jira board ID for the Kanban Board section.
When nil, the board section is omitted."
  :type '(choice (const :tag "None" nil)
                 (integer :tag "Board ID"))
  :group 'shipit-atlassian)

(defcustom shipit-atlassian-frequently-visited-max 10
  "Maximum number of items in the Frequently Visited section."
  :type 'integer
  :group 'shipit-atlassian)

;;; Buffer-local variables

(defvar-local shipit-atlassian-dashboard--repo nil
  "Repository name for this dashboard buffer.")

(defvar-local shipit-atlassian-dashboard--config nil
  "Jira backend config plist for this dashboard buffer.")

(defvar-local shipit-atlassian-dashboard--my-issues nil
  "Cached list of My Open Issues data.")

(defvar-local shipit-atlassian-dashboard--whats-next nil
  "Cached list of What's Next items.")

(defvar-local shipit-atlassian-dashboard--board-data nil
  "Cached Kanban board data.")

(defvar-local shipit-atlassian-dashboard--backend-plist nil
  "Backend plist resolved for this dashboard buffer.")

;;; Keymap

(defvar shipit-atlassian-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'shipit-atlassian-dashboard-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'shipit-atlassian-dashboard--ret-action)
    (define-key map [return] #'shipit-atlassian-dashboard--ret-action)
    (define-key map (kbd "o") #'shipit-atlassian-dashboard--open-in-browser)
    (define-key map (kbd "M-;") #'shipit-atlassian-dashboard-actions)
    map)
  "Keymap for `shipit-atlassian-dashboard-mode'.")

;;; Section visibility

(defun shipit-atlassian-dashboard--section-visibility (section)
  "Return initial visibility for SECTION in dashboard buffers.
All dashboard sections are collapsed by default."
  (when (memq (oref section type)
              '(atlassian-dashboard atlassian-my-issues
                atlassian-whats-next atlassian-board
                atlassian-frequently-visited
                atlassian-board-column atlassian-my-issues-project))
    'hide))

;;; Mode definition

(define-derived-mode shipit-atlassian-dashboard-mode magit-section-mode "Shipit-Atlassian"
  "Major mode for Atlassian/Jira project dashboard buffers.
Displays project overview with My Open Issues, What's Next,
Kanban Board, and Frequently Visited sections.

\\{shipit-atlassian-dashboard-mode-map}"
  :group 'shipit-atlassian
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (shipit-atlassian-dashboard-refresh)))
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (add-hook 'magit-section-set-visibility-hook
            #'shipit-atlassian-dashboard--section-visibility nil t)
  (setq-local magit-root-section nil))

;;; Buffer lifecycle

(defun shipit-atlassian-dashboard--buffer-name (repo)
  "Generate buffer name for Atlassian dashboard of REPO."
  (format "*shipit-atlassian: %s*" repo))

;;;###autoload
(defun shipit-atlassian-dashboard-available-p ()
  "Return non-nil if a Jira-capable backend is configured.
Used as a menu `:if' predicate to hide the Atlassian dashboard entry
when no Jira mapping exists.  Keeps the backend-name reference local
to this module — generic entry points such as `shipit.el' don't need
to know which backend symbol represents Jira."
  (or (eq shipit-issue-backend 'jira)
      (cl-some (lambda (entry)
                 (eq (plist-get (cdr entry) :backend) 'jira))
               shipit-issue-repo-backends)))

(defun shipit-atlassian-dashboard--open (repo)
  "Open Atlassian dashboard buffer for REPO.
Resolves Jira config via `shipit-issue--resolve-for-repo' and
creates or switches to the dashboard buffer.
Returns the buffer."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (config (cdr resolved))
         (buf-name (shipit-atlassian-dashboard--buffer-name repo))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'shipit-atlassian-dashboard-mode)
        (shipit-atlassian-dashboard-mode))
      (setq shipit-atlassian-dashboard--repo repo)
      (setq shipit-atlassian-dashboard--config config)
      (setq shipit-atlassian-dashboard--backend-plist (car resolved))
      (setq shipit-issue-buffer-repo repo)
      (shipit--debug-log "Atlassian dashboard opened for repo: %s" repo)
      (shipit-atlassian-dashboard--refresh-data))
    (pop-to-buffer buf)
    buf))

;;; Interactive entry point

;;;###autoload
(defun shipit-atlassian-dashboard ()
  "Open the Atlassian/Jira project dashboard for the current repository."
  (interactive)
  (let ((repo (shipit--get-repo-from-remote)))
    (unless repo
      (error "Could not detect repository from git remote"))
    (shipit-atlassian-dashboard--open repo)))

;;; JQL builders

(defun shipit-atlassian-dashboard--my-issues-jql (config)
  "Build JQL for my open issues from CONFIG."
  (let ((project-keys (plist-get config :project-keys)))
    (format "assignee=currentUser() AND resolution=Unresolved%s ORDER BY priority DESC, updated DESC"
            (if project-keys
                (format " AND project in (%s)"
                        (mapconcat #'identity project-keys ","))
              ""))))

;;; Issue grouping

(defun shipit-atlassian-dashboard--project-key (issue-id)
  "Extract project key from ISSUE-ID like \"PROJ-123\"."
  (if (string-match "\\`\\([A-Z][A-Z0-9]*\\)-" issue-id)
      (match-string 1 issue-id)
    "OTHER"))

(defun shipit-atlassian-dashboard--group-by-project (issues)
  "Group ISSUES by project key prefix.
Returns list of (project-key . items) alists."
  (let ((groups nil))
    (dolist (issue issues)
      (let* ((key (shipit-atlassian-dashboard--project-key
                   (cdr (assq 'id issue))))
             (existing (assoc key groups)))
        (if existing
            (setcdr existing (append (cdr existing) (list issue)))
          (push (cons key (list issue)) groups))))
    (nreverse groups)))

;;; Data fetching

(defun shipit-atlassian-dashboard--fetch-my-issues ()
  "Fetch my open issues from Jira."
  (let* ((config shipit-atlassian-dashboard--config)
         (jql (shipit-atlassian-dashboard--my-issues-jql config))
         (fields "key,summary,status,priority,issuetype,assignee,updated")
         (raw (shipit-issue-jira--search-page config jql 50 fields nil))
         (issues (append (cdr (assq 'issues raw)) nil)))
    (shipit--debug-log "Atlassian dashboard: fetched %d my-issues" (length issues))
    (mapcar #'shipit-issue-jira--normalize-issue issues)))

;;; Agile API helpers

(defun shipit-atlassian-dashboard--active-sprint-path (board-id)
  "Build path for active sprint of BOARD-ID."
  (format "/rest/agile/1.0/board/%d/sprint?state=active" board-id))

(defun shipit-atlassian-dashboard--sprint-issues-path (sprint-id)
  "Build path for issues in SPRINT-ID assigned to me in To Do status."
  (format "/rest/agile/1.0/sprint/%d/issue?jql=%s&fields=key,summary,status,priority,issuetype,assignee"
          sprint-id
          (url-hexify-string "assignee=currentUser() AND status=\"To Do\"")))

(defun shipit-atlassian-dashboard--normalize-agile-issues (raw)
  "Normalize agile API issue response RAW to shipit format."
  (let ((issues (append (cdr (assq 'issues raw)) nil)))
    (mapcar #'shipit-issue-jira--normalize-issue issues)))

(defun shipit-atlassian-dashboard--fetch-whats-next ()
  "Fetch what's next items from active sprint.
Returns nil when `shipit-atlassian-board-id' is not configured."
  (when shipit-atlassian-board-id
    (let* ((config shipit-atlassian-dashboard--config)
           (sprint-raw (shipit-issue-jira--api-request
                        config
                        (shipit-atlassian-dashboard--active-sprint-path
                         shipit-atlassian-board-id)))
           (sprints (append (cdr (assq 'values sprint-raw)) nil))
           (active-sprint (car sprints)))
      (when active-sprint
        (let* ((sprint-id (cdr (assq 'id active-sprint)))
               (issues-raw (shipit-issue-jira--api-request
                            config
                            (shipit-atlassian-dashboard--sprint-issues-path sprint-id))))
          (shipit--debug-log "Atlassian dashboard: fetched %d whats-next items from sprint %s"
                             (length (append (cdr (assq 'issues issues-raw)) nil))
                             (cdr (assq 'name active-sprint)))
          (shipit-atlassian-dashboard--normalize-agile-issues issues-raw))))))

(defun shipit-atlassian-dashboard--fetch-board ()
  "Fetch board data.
Returns nil when `shipit-atlassian-board-id' is not configured."
  (when shipit-atlassian-board-id
    (shipit-atlassian-board--fetch
     shipit-atlassian-dashboard--config
     shipit-atlassian-board-id)))

;;; Column helpers

(defun shipit-atlassian-dashboard--get-columns ()
  "Return the dashboard columns list from the backend plist."
  (let ((val (plist-get shipit-atlassian-dashboard--backend-plist :dashboard-columns)))
    (cond
     ((symbolp val) (symbol-value val))
     ((listp val) val)
     (t '(work status)))))

(defun shipit-atlassian-dashboard--prepare-widths (issues)
  "Compute widths for ISSUES using the dashboard backend plist.
Returns (COLUMNS . WIDTHS) cons."
  (let* ((columns (shipit-atlassian-dashboard--get-columns))
         (mapped (mapcar #'shipit-issue--normalize-to-work-item issues))
         (widths (shipit-issue--dashboard-widths
                  mapped shipit-atlassian-dashboard--backend-plist 3)))
    (cons columns widths)))

;;; Section rendering

(defun shipit-atlassian-dashboard--insert-issue-line (issue columns widths)
  "Insert a single ISSUE line using shared column renderer.
COLUMNS is a list of column symbols, WIDTHS is the widths alist."
  (let* ((mapped (shipit-issue--normalize-to-work-item issue))
         (key (cdr (assq 'key mapped)))
         (line (concat "   "
                       (shipit-issue--format-work-item-line mapped columns widths)
                       "\n")))
    (insert (propertize line 'shipit-issuelink-key key))))

(defun shipit-atlassian-dashboard--insert-my-issues-section ()
  "Insert My Open Issues section."
  (let ((issues shipit-atlassian-dashboard--my-issues))
    (magit-insert-section (atlassian-my-issues nil t)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "issues" "\U0001f4cb")
                (propertize (format "My Open Issues (%d)" (length issues))
                            'face 'magit-section-heading)))
      (if (null issues)
          (insert (propertize "   No open issues\n" 'face 'magit-dimmed))
        (let* ((prepared (shipit-atlassian-dashboard--prepare-widths issues))
               (columns (car prepared))
               (widths (cdr prepared))
               (grouped (shipit-atlassian-dashboard--group-by-project issues)))
          (dolist (group grouped)
            (magit-insert-section (atlassian-my-issues-project nil t)
              (magit-insert-heading
                (propertize (format "   %s (%d)" (car group) (length (cdr group)))
                            'face 'magit-section-heading))
              (dolist (issue (cdr group))
                (shipit-atlassian-dashboard--insert-issue-line issue columns widths)))))
        (insert "\n")))))

(defun shipit-atlassian-dashboard--insert-whats-next-section ()
  "Insert What's Next section."
  (magit-insert-section (atlassian-whats-next nil t)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "milestone" "\U0001f3af")
              (propertize (format "What's Next (%d)"
                                  (length shipit-atlassian-dashboard--whats-next))
                          'face 'magit-section-heading)))
    (cond
     ((null shipit-atlassian-board-id)
      (insert (propertize "   Press RET to configure board ID\n"
                          'face 'magit-dimmed
                          'shipit-atlassian-configure 'board-id)))
     ((null shipit-atlassian-dashboard--whats-next)
      (insert (propertize "   No items in current sprint\n" 'face 'magit-dimmed)))
     (t
      (let* ((prepared (shipit-atlassian-dashboard--prepare-widths
                        shipit-atlassian-dashboard--whats-next))
             (columns (car prepared))
             (widths (cdr prepared)))
        (dolist (issue shipit-atlassian-dashboard--whats-next)
          (shipit-atlassian-dashboard--insert-issue-line issue columns widths)))))
    (insert "\n")))

(defun shipit-atlassian-dashboard--insert-board-section ()
  "Insert Kanban Board section."
  (magit-insert-section (atlassian-board nil t)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "project" "\U0001f4ca")
              (propertize "Board" 'face 'magit-section-heading)))
    (cond
     ((null shipit-atlassian-board-id)
      (insert (propertize "   Press RET to configure board ID\n"
                          'face 'magit-dimmed
                          'shipit-atlassian-configure 'board-id)))
     ((null shipit-atlassian-dashboard--board-data)
      (insert (propertize "   No board data\n" 'face 'magit-dimmed)))
     (t
      (let* ((all-issues (cl-mapcan (lambda (col)
                                      (copy-sequence (cdr (assq 'issues col))))
                                    shipit-atlassian-dashboard--board-data))
             (prepared (shipit-atlassian-dashboard--prepare-widths all-issues))
             (columns (car prepared))
             (widths (cdr prepared)))
        (dolist (col shipit-atlassian-dashboard--board-data)
          (let ((col-name (cdr (assq 'name col)))
                (col-issues (cdr (assq 'issues col))))
            (magit-insert-section (atlassian-board-column nil t)
              (magit-insert-heading
                (propertize (format "   %s (%d)" col-name (length col-issues))
                            'face 'magit-section-heading))
              (dolist (issue col-issues)
                (shipit-atlassian-dashboard--insert-issue-line
                 issue columns widths))))))))
    (insert "\n")))

;;; Frequently Visited — state and persistence

(defvar shipit-atlassian-dashboard--recent-visits nil
  "List of recently visited issues.
Each entry is an alist with keys `key', `title', and `time'.")

(defvar shipit-atlassian-dashboard--recent-file
  (expand-file-name "shipit-atlassian-recent.el" user-emacs-directory)
  "File for persisting recently visited issues.")

(defun shipit-atlassian-dashboard--load-recent ()
  "Load recent visits from file.
Only loads when the in-memory list is nil and the file exists."
  (when (and (file-exists-p shipit-atlassian-dashboard--recent-file)
             (null shipit-atlassian-dashboard--recent-visits))
    (condition-case nil
        (setq shipit-atlassian-dashboard--recent-visits
              (with-temp-buffer
                (insert-file-contents shipit-atlassian-dashboard--recent-file)
                (read (current-buffer))))
      (error nil))))

(defun shipit-atlassian-dashboard--save-recent ()
  "Save recent visits to file."
  (with-temp-file shipit-atlassian-dashboard--recent-file
    (prin1 shipit-atlassian-dashboard--recent-visits (current-buffer))))

(defun shipit-atlassian-dashboard--record-visit (issue-key title)
  "Record a visit to ISSUE-KEY with TITLE.
Deduplicates, pushes to front, trims to max, and persists."
  (shipit-atlassian-dashboard--load-recent)
  (setq shipit-atlassian-dashboard--recent-visits
        (seq-remove (lambda (v) (equal (cdr (assq 'key v)) issue-key))
                    shipit-atlassian-dashboard--recent-visits))
  (push `((key . ,issue-key) (title . ,title) (time . ,(float-time)))
        shipit-atlassian-dashboard--recent-visits)
  (when (> (length shipit-atlassian-dashboard--recent-visits)
           shipit-atlassian-frequently-visited-max)
    (setq shipit-atlassian-dashboard--recent-visits
          (seq-take shipit-atlassian-dashboard--recent-visits
                    shipit-atlassian-frequently-visited-max)))
  (shipit-atlassian-dashboard--save-recent))

;;; Frequently Visited — section renderer

(defun shipit-atlassian-dashboard--insert-frequently-visited-section ()
  "Insert Frequently Visited section."
  (shipit-atlassian-dashboard--load-recent)
  (let ((visits shipit-atlassian-dashboard--recent-visits))
    (magit-insert-section (atlassian-frequently-visited nil t)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "clock" "\U0001f552")
                (propertize (format "Frequently Visited (%d)" (length visits))
                            'face 'magit-section-heading)))
      (if (null visits)
          (insert (propertize "   No recently visited issues\n" 'face 'magit-dimmed))
        (let* ((fv-columns '(work relative-time))
               (mapped (mapcar #'shipit-issue--normalize-to-work-item visits))
               (widths (shipit-issue--dashboard-widths
                        mapped shipit-atlassian-dashboard--backend-plist 3)))
          (dolist (visit visits)
            (shipit-atlassian-dashboard--insert-issue-line
             visit fv-columns widths))))
      (insert "\n"))))

;;; Render

(defun shipit-atlassian-dashboard--render ()
  "Render all dashboard sections."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (magit-insert-section (atlassian-dashboard)
      ;; Header
      (insert (propertize (format "Atlassian Dashboard — %s\n\n"
                                  shipit-atlassian-dashboard--repo)
                          'face 'magit-header-line))
      ;; Sections
      (shipit-atlassian-dashboard--insert-whats-next-section)
      (shipit-atlassian-dashboard--insert-my-issues-section)
      (shipit-atlassian-dashboard--insert-board-section)
      (shipit-atlassian-dashboard--insert-frequently-visited-section))
    ;; Propagate hidden state to overlays — root section doesn't go
    ;; through magit-section-show, so children need explicit show/hide.
    (magit-section-show magit-root-section)
    (goto-char (min pos (point-max)))))

;;; Refresh

(defun shipit-atlassian-dashboard--refresh-data ()
  "Fetch all dashboard data and re-render."
  (message "Loading Atlassian dashboard...")
  (condition-case err
      (progn
        (setq shipit-atlassian-dashboard--my-issues
              (shipit-atlassian-dashboard--fetch-my-issues))
        (setq shipit-atlassian-dashboard--whats-next
              (shipit-atlassian-dashboard--fetch-whats-next))
        (setq shipit-atlassian-dashboard--board-data
              (shipit-atlassian-dashboard--fetch-board)))
    (error
     (shipit--debug-log "Dashboard fetch error: %s" err)))
  (shipit-atlassian-dashboard--render)
  (message "Atlassian dashboard loaded"))

(defun shipit-atlassian-dashboard-refresh ()
  "Refresh the Atlassian dashboard buffer."
  (interactive)
  (shipit-atlassian-dashboard--refresh-data))

;;; Actions

(defun shipit-atlassian-dashboard--parse-board-input (input)
  "Parse INPUT as a board ID number or extract it from a board URL.
Returns the numeric board ID or nil."
  (cond
   ((string-match "\\`[0-9]+\\'" input)
    (string-to-number input))
   ((string-match "/boards?/\\([0-9]+\\)" input)
    (string-to-number (match-string 1 input)))))

(defun shipit-atlassian-dashboard--configure-board-id ()
  "Prompt for a Jira board ID or URL and set it for the current session."
  (interactive)
  (let* ((prompt (if shipit-atlassian-board-id
                     (format "Board ID or URL (current: %d): " shipit-atlassian-board-id)
                   "Board ID or URL: "))
         (input (read-string prompt))
         (id (shipit-atlassian-dashboard--parse-board-input input)))
    (unless id
      (user-error "Could not parse board ID from: %s" input))
    (setq shipit-atlassian-board-id id)
    (message "Board ID set to %d (use M-x customize-variable RET shipit-atlassian-board-id to persist)"
             id)
    (shipit-atlassian-dashboard--refresh-data)))

(defun shipit-atlassian-dashboard--open-board-in-browser ()
  "Open the configured board in the browser."
  (interactive)
  (if shipit-atlassian-board-id
      (let ((base-url (string-trim-right
                       (plist-get shipit-atlassian-dashboard--config :base-url) "/")))
        (browse-url (format "%s/jira/software/projects/board/%d"
                            base-url shipit-atlassian-board-id)))
    (user-error "No board ID configured")))

(defun shipit-atlassian-dashboard--section-type-at-point ()
  "Return the dashboard section type at point.
Walks up section parents. Returns a symbol like
`atlassian-board', `atlassian-whats-next', `atlassian-my-issues',
`atlassian-frequently-visited', or nil."
  (let ((section (magit-current-section)))
    (while (and section
                (not (memq (oref section type)
                           '(atlassian-board atlassian-whats-next
                             atlassian-my-issues atlassian-frequently-visited))))
      (setq section (oref section parent)))
    (when section (oref section type))))

(transient-define-prefix shipit-atlassian-dashboard-actions ()
  "Actions for Atlassian dashboard."
  ["Board"
   ("b" "Set board ID" shipit-atlassian-dashboard--configure-board-id)
   ("o" "Open board in browser" shipit-atlassian-dashboard--open-board-in-browser)]
  ["Dashboard"
   ("g" "Refresh" shipit-atlassian-dashboard-refresh)
   ("q" "Quit" transient-quit-one)])

(defun shipit-atlassian-dashboard--ret-action ()
  "Open the issue at point, configure board, or show actions menu."
  (interactive)
  (let ((issue-key (get-text-property (point) 'shipit-issuelink-key))
        (configure (get-text-property (point) 'shipit-atlassian-configure)))
    (cond
     (issue-key
      (shipit-issues-open-buffer
       issue-key
       shipit-atlassian-dashboard--repo))
     ((eq configure 'board-id)
      (shipit-atlassian-dashboard--configure-board-id))
     (t
      (let ((section (magit-current-section)))
        (when (and section (not (eq section magit-root-section)))
          (magit-section-toggle section)))))))

(defun shipit-atlassian-dashboard--open-in-browser ()
  "Open the issue at point in the browser, or the project base URL."
  (interactive)
  (let ((issue-key (get-text-property (point) 'shipit-issuelink-key)))
    (if issue-key
        (browse-url (shipit-issue-jira--browse-url
                     shipit-atlassian-dashboard--config issue-key))
      (let ((base-url (plist-get shipit-atlassian-dashboard--config :base-url)))
        (if base-url
            (browse-url base-url)
          (user-error "No URL available"))))))

;;; Visit tracking advice

(defun shipit-atlassian-dashboard--advice-record-visit (orig-fn issue-number &rest args)
  "Advice for `shipit-issues-open-buffer' to record visits.
ORIG-FN is the original function, ISSUE-NUMBER and ARGS are passed through.
Only records visits for Jira issues (string keys like PROJ-123)."
  (let ((result (apply orig-fn issue-number args)))
    (when (and (stringp issue-number)
               (string-match-p "\\`[A-Z][A-Z0-9]*-[0-9]+" issue-number))
      (condition-case nil
          (let* ((repo (or (car args) (shipit--get-repo-from-remote)))
                 (buf-name (shipit-issue-buffer-name repo issue-number)))
            (when-let* ((buf (get-buffer buf-name)))
              (with-current-buffer buf
                (let ((title (or (cdr (assq 'title shipit-issue-buffer-data)) "")))
                  (shipit-atlassian-dashboard--record-visit
                   issue-number title)))))
        (error nil)))
    result))

(advice-add 'shipit-issues-open-buffer :around
            #'shipit-atlassian-dashboard--advice-record-visit)

;;; Dwim handler registration

(with-eval-after-load 'shipit-pr-actions
  (shipit-register-dwim-handler
   'atlassian-dashboard
   (lambda ()
     (derived-mode-p 'shipit-atlassian-dashboard-mode))
   (lambda ()
     (shipit-atlassian-dashboard-actions))))

(provide 'shipit-atlassian-dashboard)
;;; shipit-atlassian-dashboard.el ends here
