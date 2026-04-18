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
(declare-function shipit-issue-create-buffer "shipit-issue-create")
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

(defun atlassian-issues (&rest _args)
  "Magit section identifier for Issues (all project issues).")
(put 'atlassian-issues 'magit-section t)

(defun atlassian-issues-project-group (&rest _args)
  "Magit section identifier for an Issues project group.")
(put 'atlassian-issues-project-group 'magit-section t)

(defun atlassian-issue (&rest _args)
  "Magit section identifier for a single issue line.")
(put 'atlassian-issue 'magit-section t)

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

(defcustom shipit-atlassian-issues-page-size 50
  "Default number of issues loaded per page in the Issues section."
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

(defvar-local shipit-atlassian-dashboard--issues-data nil
  "Cached list of issues loaded for the Issues section.")

(defvar-local shipit-atlassian-dashboard--issues-filter nil
  "Plist of filter criteria currently applied to the Issues section.
Recognized keys: :text :assignee :reporter :type :status :priority
:resolution :comment-text :sort.  Values are strings except :assignee
and :reporter which may also be the symbols `me' or `unassigned'.")

(defvar-local shipit-atlassian-dashboard--issues-next-token nil
  "nextPageToken returned by Jira search for incremental loading.
Nil means no more pages.")

(defvar-local shipit-atlassian-dashboard--issues-last-page-p nil
  "Non-nil once Jira reported isLast=true for the current filter.")

(defvar-local shipit-atlassian-dashboard--pending-restore-path nil
  "Section path captured before a re-render, restored after.
A list of (TYPE . VALUE) pairs from the top-level section down to the
section point was in.")

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
    (define-key map (kbd "f") #'shipit-atlassian-dashboard-filter-dispatch)
    (define-key map (kbd "l") #'shipit-atlassian-dashboard-load-dispatch)
    map)
  "Keymap for `shipit-atlassian-dashboard-mode'.")

;;; Section dispatch

(defun shipit-atlassian-dashboard--top-section-type ()
  "Return the type symbol of the top-level section containing point.
Walks up from `magit-current-section' to the direct child of the root
`atlassian-dashboard' section.  Returns nil when point is on the root
itself or not in any section."
  (let ((section (magit-current-section))
        (result nil))
    (while (and section
                (not (eq (oref section type) 'atlassian-dashboard)))
      (setq result (oref section type))
      (setq section (oref section parent)))
    (and section result)))

(defun shipit-atlassian-dashboard--section-top-ancestor (section)
  "Return the top-level ancestor of SECTION (child of the dashboard root)."
  (while (and section
              (let ((parent (oref section parent)))
                (and parent
                     (not (eq (oref parent type) 'atlassian-dashboard)))))
    (setq section (oref section parent)))
  section)

(defun shipit-atlassian-dashboard--section-menu (property not-found-msg)
  "Look up PROPERTY on the top-level section type at point and call it.
Shows NOT-FOUND-MSG when nothing is registered for the section.
Menus register via (put \='SECTION-TYPE PROPERTY #\='MENU-FN)."
  (let* ((type (shipit-atlassian-dashboard--top-section-type))
         (fn (and type (get type property))))
    (if fn (funcall fn) (message "%s" not-found-msg))))

(defun shipit-atlassian-dashboard-filter-dispatch ()
  "Open the filter transient for the section containing point.
Each section type registers its own filter menu via the
`shipit-atlassian-filter-menu' symbol property."
  (interactive)
  (shipit-atlassian-dashboard--section-menu
   'shipit-atlassian-filter-menu
   "No filter available for this section"))

(defun shipit-atlassian-dashboard-load-dispatch ()
  "Open the load-more transient for the section containing point."
  (interactive)
  (shipit-atlassian-dashboard--section-menu
   'shipit-atlassian-load-menu
   "No load action available for this section"))

;; Issues section registers its filter/load menus.
(put 'atlassian-issues 'shipit-atlassian-filter-menu
     #'shipit-atlassian-dashboard-issues-filter-menu)
(put 'atlassian-issues 'shipit-atlassian-load-menu
     #'shipit-atlassian-dashboard-issues-load-menu)

;;; Section visibility

(defun shipit-atlassian-dashboard--section-visibility (section)
  "Return initial visibility for SECTION in dashboard buffers.
All dashboard sections are collapsed by default."
  (when (memq (oref section type)
              '(atlassian-dashboard atlassian-my-issues
                atlassian-whats-next atlassian-board
                atlassian-frequently-visited
                atlassian-board-column atlassian-my-issues-project
                atlassian-issues atlassian-issues-project-group
                atlassian-issue))
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

(defun shipit-atlassian-dashboard--user-clause (field value)
  "Render a JQL clause for FIELD (assignee or reporter) based on VALUE.
VALUE may be the symbol `me', `unassigned', or a free-text string."
  (pcase value
    ('me (format "%s=currentUser()" field))
    ('unassigned (format "%s is EMPTY" field))
    ((pred stringp) (format "%s=\"%s\"" field value))))

(defun shipit-atlassian-dashboard--build-issues-jql (config filter)
  "Build the JQL query for the Issues section.
CONFIG supplies :project-keys; FILTER is a plist of user filters.
Returns a JQL string."
  (let ((clauses nil)
        (project-keys (plist-get config :project-keys)))
    (when project-keys
      (push (format "project in (%s)" (mapconcat #'identity project-keys ","))
            clauses))
    (when-let* ((val (plist-get filter :assignee)))
      (push (shipit-atlassian-dashboard--user-clause "assignee" val) clauses))
    (when-let* ((val (plist-get filter :reporter)))
      (push (shipit-atlassian-dashboard--user-clause "reporter" val) clauses))
    (when-let* ((val (plist-get filter :type)))
      (push (format "issuetype=\"%s\"" val) clauses))
    (when-let* ((val (plist-get filter :status)))
      (push (format "status=\"%s\"" val) clauses))
    (when-let* ((val (plist-get filter :priority)))
      (push (format "priority=\"%s\"" val) clauses))
    (when-let* ((val (plist-get filter :resolution)))
      (push (format "resolution=\"%s\"" val) clauses))
    (when-let* ((val (plist-get filter :text)))
      (push (format "text ~ \"%s\"" val) clauses))
    (when-let* ((val (plist-get filter :comment-text)))
      (push (format "comment ~ \"%s\"" val) clauses))
    (when (plist-get filter :has-comments)
      ;; Jira's `comment IS NOT EMPTY' is unreliable; this OR pair covers
      ;; every comment content Jira's text index can index.
      (push "(comment ~ \"anything*\" OR comment !~ \"anything*\")" clauses))
    (concat (mapconcat #'identity (nreverse clauses) " AND ")
            (if clauses " " "")
            "ORDER BY "
            (or (plist-get filter :sort) "key DESC"))))

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

;;; Issues section — fetch and pagination

(defconst shipit-atlassian-dashboard--issues-fields
  "key,summary,status,priority,issuetype,assignee,reporter,updated,created"
  "Jira field list for Issues section rows.")

(defun shipit-atlassian-dashboard--fetch-issues-page (token page-size)
  "Fetch one page of issues for the current filter.
TOKEN is the pagination cursor (nil for first page).
PAGE-SIZE caps the number of results.
Returns (ISSUES NEXT-TOKEN IS-LAST-P)."
  (let* ((config shipit-atlassian-dashboard--config)
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config shipit-atlassian-dashboard--issues-filter))
         (raw (shipit-issue-jira--search-page
               config jql page-size
               shipit-atlassian-dashboard--issues-fields token))
         (issues (append (cdr (assq 'issues raw)) nil))
         (next-token (cdr (assq 'nextPageToken raw)))
         (is-last (shipit-issue-jira--is-last-page-p raw)))
    (shipit--debug-log "Atlassian Issues: fetched %d (next=%s last=%s)"
                       (length issues) next-token is-last)
    (list (mapcar #'shipit-issue-jira--normalize-issue issues)
          next-token
          is-last)))

(defun shipit-atlassian-dashboard--fetch-issues-reset ()
  "Reset Issues state and fetch the first page under the current filter."
  (setq shipit-atlassian-dashboard--issues-data nil
        shipit-atlassian-dashboard--issues-next-token nil
        shipit-atlassian-dashboard--issues-last-page-p nil)
  (pcase-let* ((`(,issues ,next ,last-p)
                (shipit-atlassian-dashboard--fetch-issues-page
                 nil shipit-atlassian-issues-page-size)))
    (setq shipit-atlassian-dashboard--issues-data issues
          shipit-atlassian-dashboard--issues-next-token next
          shipit-atlassian-dashboard--issues-last-page-p last-p)))

(defun shipit-atlassian-dashboard--fetch-issues-append (count)
  "Fetch the next batch of issues (up to COUNT more) and append."
  (when (and shipit-atlassian-dashboard--issues-next-token
             (not shipit-atlassian-dashboard--issues-last-page-p))
    (let ((remaining count))
      (while (and (> remaining 0)
                  shipit-atlassian-dashboard--issues-next-token
                  (not shipit-atlassian-dashboard--issues-last-page-p))
        (pcase-let* ((batch-size (min remaining 100))
                     (`(,issues ,next ,last-p)
                      (shipit-atlassian-dashboard--fetch-issues-page
                       shipit-atlassian-dashboard--issues-next-token
                       batch-size)))
          (setq shipit-atlassian-dashboard--issues-data
                (append shipit-atlassian-dashboard--issues-data issues)
                shipit-atlassian-dashboard--issues-next-token next
                shipit-atlassian-dashboard--issues-last-page-p last-p
                remaining (- remaining (length issues)))
          (when (zerop (length issues))
            (setq remaining 0)))))))

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
  "Insert a single ISSUE as a collapsible magit section.
COLUMNS is a list of column symbols, WIDTHS is the widths alist.
The section's value is the issue key so n/p navigation jumps row-by-row
and future expansion can add detail children."
  (let* ((mapped (shipit-issue--normalize-to-work-item issue))
         (key (cdr (assq 'key mapped)))
         (line (concat "   "
                       (shipit-issue--format-work-item-line mapped columns widths)
                       "\n")))
    (magit-insert-section (atlassian-issue key t)
      (magit-insert-heading
        (propertize line 'shipit-issuelink-key key)))))

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

;;; Issues section — renderer

(defun shipit-atlassian-dashboard--issues-heading-suffix ()
  "Return the \"(N shown [+more])\" suffix for the Issues heading."
  (let* ((count (length shipit-atlassian-dashboard--issues-data))
         (more (and shipit-atlassian-dashboard--issues-next-token
                    (not shipit-atlassian-dashboard--issues-last-page-p))))
    (format "(%d%s)" count (if more "+" ""))))

(defun shipit-atlassian-dashboard--insert-issues-section ()
  "Insert the Issues (all project issues, filterable) section."
  (let ((issues shipit-atlassian-dashboard--issues-data))
    (magit-insert-section (atlassian-issues nil t)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "issues" "\U0001f50d")
                (propertize
                 (format "Issues %s%s — f:filter l:load more"
                         (shipit-atlassian-dashboard--issues-heading-suffix)
                         (shipit-atlassian-dashboard--filter-summary
                          shipit-atlassian-dashboard--issues-filter))
                 'face 'magit-section-heading)))
      (if (null issues)
          (insert (propertize "   No issues match the current filter\n"
                              'face 'magit-dimmed))
        (let* ((prepared (shipit-atlassian-dashboard--prepare-widths issues))
               (columns (car prepared))
               (widths (cdr prepared))
               (grouped (shipit-atlassian-dashboard--group-by-project issues)))
          (dolist (group grouped)
            (magit-insert-section (atlassian-issues-project-group (car group) t)
              (magit-insert-heading
                (propertize (format "   %s (%d)" (car group) (length (cdr group)))
                            'face 'magit-section-heading))
              (dolist (issue (cdr group))
                (shipit-atlassian-dashboard--insert-issue-line issue columns widths))))))
      (insert "\n"))))

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
      (shipit-atlassian-dashboard--insert-issues-section)
      (shipit-atlassian-dashboard--insert-board-section)
      (shipit-atlassian-dashboard--insert-frequently-visited-section))
    ;; Propagate hidden state to overlays — root section doesn't go
    ;; through magit-section-show, so children need explicit show/hide.
    (magit-section-show magit-root-section)
    (goto-char (min pos (point-max)))))

;;; Issues section — filter & load transients

(defun shipit-atlassian-dashboard--read-user (prompt _initial _history)
  "Reader for assignee/reporter filter fields.
Accepts @me, @unassigned, or any username."
  (let ((val (read-string prompt)))
    (cond
     ((string-empty-p val) nil)
     ((string= val "@me") "me")
     ((member val '("@unassigned" "none")) "unassigned")
     (t val))))

(defun shipit-atlassian-dashboard--user-value-to-string (val)
  "Render the user-filter VALUE (symbol or string) as a transient arg string."
  (cond ((eq val 'me) "me")
        ((eq val 'unassigned) "unassigned")
        (t val)))

(defconst shipit-atlassian-dashboard--filter-fields
  '((:text         "text"         "text~\"%s\""    plain)
    (:assignee     "assignee"     "assignee=%s"    user)
    (:reporter     "reporter"     "reporter=%s"    user)
    (:type         "type"         "type=%s"        plain)
    (:status       "status"       "status=%s"      plain)
    (:priority     "priority"     "priority=%s"    plain)
    (:resolution   "resolution"   "resolution=%s"  plain)
    (:sort         "sort"         "sort=%s"        plain)
    (:comment-text "comment"      "comment~\"%s\"" plain)
    (:has-comments "has-comments" "has-comments"   flag))
  "Spec for the Issues-section filter plist fields.
Each row is (PLIST-KEY ARG-NAME SUMMARY-TEMPLATE KIND) where KIND is
`plain', `user' (value needs user-string conversion), or `flag' (no
value — presence of a truthy plist entry toggles the flag).")

(defun shipit-atlassian-dashboard--filter-field-string (kind value template)
  "Format VALUE per KIND using TEMPLATE (a `format' string with %s)."
  (pcase kind
    ('flag template)
    ('user (format template
                   (shipit-atlassian-dashboard--user-value-to-string value)))
    (_ (format template value))))

(defun shipit-atlassian-dashboard--filter-to-transient-args (filter)
  "Translate a FILTER plist into a list of transient \"--key=value\" strings."
  (let ((args nil))
    (dolist (row shipit-atlassian-dashboard--filter-fields)
      (let ((key (nth 0 row))
            (arg-name (nth 1 row))
            (kind (nth 3 row)))
        (when-let* ((v (plist-get filter key)))
          (let ((tmpl (if (eq kind 'flag)
                          (format "--%s" arg-name)
                        (format "--%s=%%s" arg-name))))
            (push (shipit-atlassian-dashboard--filter-field-string kind v tmpl)
                  args)))))
    (nreverse args)))

(defun shipit-atlassian-dashboard--filter-summary (filter)
  "Return a short bracketed summary of FILTER, or the empty string if nil."
  (let ((parts nil))
    (dolist (row shipit-atlassian-dashboard--filter-fields)
      (let ((key (nth 0 row))
            (tmpl (nth 2 row))
            (kind (nth 3 row)))
        (when-let* ((v (plist-get filter key)))
          (push (shipit-atlassian-dashboard--filter-field-string kind v tmpl)
                parts))))
    (if parts
        (format " [%s]" (mapconcat #'identity (nreverse parts) " · "))
      "")))

(defun shipit-atlassian-dashboard--filter-from-args (args)
  "Translate transient ARGS (a list of \"--key=value\" strings) into a filter plist."
  (let ((filter nil))
    (dolist (arg args)
      (cond
       ((string-match "\\`--text=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :text (match-string 1 arg))))
       ((string-match "\\`--assignee=\\(.+\\)\\'" arg)
        (let ((v (match-string 1 arg)))
          (setq filter
                (plist-put filter :assignee
                           (cond ((string= v "me") 'me)
                                 ((string= v "unassigned") 'unassigned)
                                 (t v))))))
       ((string-match "\\`--reporter=\\(.+\\)\\'" arg)
        (let ((v (match-string 1 arg)))
          (setq filter
                (plist-put filter :reporter
                           (cond ((string= v "me") 'me)
                                 ((string= v "unassigned") 'unassigned)
                                 (t v))))))
       ((string-match "\\`--type=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :type (match-string 1 arg))))
       ((string-match "\\`--status=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :status (match-string 1 arg))))
       ((string-match "\\`--priority=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :priority (match-string 1 arg))))
       ((string-match "\\`--resolution=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :resolution (match-string 1 arg))))
       ((string-match "\\`--sort=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :sort (match-string 1 arg))))
       ((string-match "\\`--comment=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :comment-text (match-string 1 arg))))
       ((string= arg "--has-comments")
        (setq filter (plist-put filter :has-comments t)))))
    filter))

(defun shipit-atlassian-dashboard--expand-section-by-type (type)
  "Expand the first top-level section whose type is TYPE.
Walks the magit section tree directly via `magit-root-section' rather
than scanning lines, so invisible content and ordering are irrelevant."
  (when-let* ((section (cl-find-if
                        (lambda (s) (eq (oref s type) type))
                        (oref magit-root-section children))))
    (magit-section-show section)))

(defun shipit-atlassian-dashboard--current-section-path ()
  "Return (TYPE . VALUE) pairs from the top-level dashboard child to point.
Root section is excluded.  Returns nil when point isn't in a section."
  (let ((section (magit-current-section))
        (path nil))
    (while (and section
                (not (eq (oref section type) 'atlassian-dashboard)))
      (push (cons (oref section type) (oref section value)) path)
      (setq section (oref section parent)))
    path))

(defun shipit-atlassian-dashboard--expand-section-path (path)
  "Expand sections matching PATH (list of (TYPE . VALUE) from top-level down).
Returns the deepest section expanded, or nil.  Walks the section tree
via `oref children' instead of scanning lines."
  (let ((current magit-root-section)
        (last-shown nil))
    (catch 'done
      (dolist (entry path)
        (let* ((type (car entry))
               (value (cdr entry))
               (match (cl-find-if
                       (lambda (s)
                         (and (eq (oref s type) type)
                              (equal (oref s value) value)))
                       (oref current children))))
          (if match
              (progn
                (magit-section-show match)
                (setq last-shown match
                      current match))
            (throw 'done nil)))))
    last-shown))

(defun shipit-atlassian-dashboard--restore-or-expand (type)
  "Restore the section path captured before re-render; fall back to TYPE.
Returns the deepest section that was expanded."
  (or (shipit-atlassian-dashboard--expand-section-path
       shipit-atlassian-dashboard--pending-restore-path)
      (progn (shipit-atlassian-dashboard--expand-section-by-type type)
             nil)))

(defun shipit-atlassian-dashboard--issues-apply-filter ()
  "Apply the currently configured transient filter and refetch page 1."
  (interactive)
  (let* ((args (transient-args 'shipit-atlassian-dashboard-issues-filter-menu))
         (filter (shipit-atlassian-dashboard--filter-from-args args))
         (path (shipit-atlassian-dashboard--current-section-path)))
    (setq shipit-atlassian-dashboard--issues-filter filter)
    (setq shipit-atlassian-dashboard--pending-restore-path path)
    (message "Fetching issues with filter...")
    (shipit-atlassian-dashboard--fetch-issues-reset)
    (shipit-atlassian-dashboard--render)
    (shipit-atlassian-dashboard--restore-or-expand 'atlassian-issues)
    (setq shipit-atlassian-dashboard--pending-restore-path nil)
    (message "Issues filter applied (%d loaded)"
             (length shipit-atlassian-dashboard--issues-data))))

(defun shipit-atlassian-dashboard--issues-clear-filter ()
  "Clear the Issues filter and refetch with defaults."
  (interactive)
  (let ((path (shipit-atlassian-dashboard--current-section-path)))
    (setq shipit-atlassian-dashboard--issues-filter nil)
    (setq shipit-atlassian-dashboard--pending-restore-path path)
    (message "Clearing issues filter...")
    (shipit-atlassian-dashboard--fetch-issues-reset)
    (shipit-atlassian-dashboard--render)
    (shipit-atlassian-dashboard--restore-or-expand 'atlassian-issues)
    (setq shipit-atlassian-dashboard--pending-restore-path nil)
    (message "Issues filter cleared (%d loaded)"
             (length shipit-atlassian-dashboard--issues-data))))

(defun shipit-atlassian-dashboard--init-issues-filter-value (obj)
  "Initialize OBJ's value from the buffer-local Issues filter."
  (oset obj value
        (shipit-atlassian-dashboard--filter-to-transient-args
         shipit-atlassian-dashboard--issues-filter)))

(transient-define-prefix shipit-atlassian-dashboard-issues-filter-menu ()
  "Filter issues shown in the Issues section."
  :init-value #'shipit-atlassian-dashboard--init-issues-filter-value
  ["Filters"
   ("a" "Assignee (@me / @unassigned / user)" "--assignee="
    :reader shipit-atlassian-dashboard--read-user)
   ("r" "Reporter (@me / @unassigned / user)" "--reporter="
    :reader shipit-atlassian-dashboard--read-user)
   ("y" "Type" "--type="
    :choices ("Bug" "Task" "Story" "Epic" "Sub-task"))
   ("s" "Status" "--status=" :reader read-string)
   ("p" "Priority" "--priority="
    :choices ("Highest" "High" "Medium" "Low" "Lowest"))
   ("R" "Resolution" "--resolution="
    :choices ("Unresolved" "Done" "Won't Do" "Duplicate"))
   ("t" "Text search" "--text=" :reader read-string)
   ("c" "Comment contains" "--comment=" :reader read-string)
   ("h" "Has any comment" "--has-comments")]
  ["Sort"
   ("S" "Sort order" "--sort="
    :choices ("key DESC" "key ASC" "created DESC" "created ASC"
              "updated DESC" "updated ASC" "priority DESC"))]
  ["Actions"
   ("RET" "Apply" shipit-atlassian-dashboard--issues-apply-filter)
   ("C" "Clear filter" shipit-atlassian-dashboard--issues-clear-filter)
   ("q" "Quit" transient-quit-one)])

(defun shipit-atlassian-dashboard--issues-load-more (count)
  "Fetch up to COUNT more issues and append them to the section."
  (if (or (null shipit-atlassian-dashboard--issues-next-token)
          shipit-atlassian-dashboard--issues-last-page-p)
      (message "All issues already loaded")
    (let ((path (shipit-atlassian-dashboard--current-section-path)))
      (setq shipit-atlassian-dashboard--pending-restore-path path)
      (message "Loading more issues...")
      (shipit-atlassian-dashboard--fetch-issues-append count)
      (shipit-atlassian-dashboard--render)
      (shipit-atlassian-dashboard--restore-or-expand 'atlassian-issues)
      (setq shipit-atlassian-dashboard--pending-restore-path nil)
      (message "Loaded — total %d"
               (length shipit-atlassian-dashboard--issues-data)))))

(defun shipit-atlassian-dashboard--issues-load-25 ()
  "Load 25 more issues into the Issues section."
  (interactive) (shipit-atlassian-dashboard--issues-load-more 25))

(defun shipit-atlassian-dashboard--issues-load-50 ()
  "Load 50 more issues into the Issues section."
  (interactive) (shipit-atlassian-dashboard--issues-load-more 50))

(defun shipit-atlassian-dashboard--issues-load-100 ()
  "Load 100 more issues into the Issues section."
  (interactive) (shipit-atlassian-dashboard--issues-load-more 100))

(defun shipit-atlassian-dashboard--issues-load-all ()
  "Load all remaining issues (capped at 500) into the Issues section."
  (interactive) (shipit-atlassian-dashboard--issues-load-more 500))

(transient-define-prefix shipit-atlassian-dashboard-issues-load-menu ()
  "Load more issues into the Issues section."
  ["Load more"
   ("1" "+25" shipit-atlassian-dashboard--issues-load-25)
   ("2" "+50" shipit-atlassian-dashboard--issues-load-50)
   ("3" "+100" shipit-atlassian-dashboard--issues-load-100)
   ("a" "All remaining (cap 500)" shipit-atlassian-dashboard--issues-load-all)]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

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
              (shipit-atlassian-dashboard--fetch-board))
        (shipit-atlassian-dashboard--fetch-issues-reset))
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

(defun shipit-atlassian-dashboard--create-issue ()
  "Open the rich issue creation buffer scoped to this dashboard's repo.
Uses `shipit-atlassian-dashboard--repo' so the issue backend resolves
against the dashboard's known repo context instead of falling back to
remote detection."
  (interactive)
  (require 'shipit-issue-create)
  (let ((repo shipit-atlassian-dashboard--repo))
    (unless repo
      (user-error "No repo context in this dashboard"))
    (shipit-issue-create-buffer repo)))

(transient-define-prefix shipit-atlassian-dashboard-actions ()
  "Actions for Atlassian dashboard."
  ["Board"
   ("b" "Set board ID" shipit-atlassian-dashboard--configure-board-id)
   ("o" "Open board in browser" shipit-atlassian-dashboard--open-board-in-browser)]
  ["Issue"
   ("n" "Create new issue" shipit-atlassian-dashboard--create-issue)]
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
