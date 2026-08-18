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
;; Displays My Open Issues, What's Next, Kanban Boards, and
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
(declare-function shipit-issue-jira--api-request-async "shipit-issue-jira")
(declare-function shipit-atlassian-board--fetch-all-async "shipit-atlassian-board")
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

(defun atlassian-boards (&rest _args)
  "Magit section identifier for the Boards wrapper section.")
(put 'atlassian-boards 'magit-section t)

(defun atlassian-board (&rest _args)
  "Magit section identifier for one Kanban board.")
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

(defcustom shipit-atlassian-board-ids nil
  "List of Jira board IDs for the Boards section.
Each board renders as its own collapsible subsection showing that
board's columns.  When nil, the Boards section shows a
configuration hint and What's Next has no sprint source."
  :type '(repeat (integer :tag "Board ID"))
  :group 'shipit-atlassian)

(make-obsolete-variable 'shipit-atlassian-board-id 'shipit-atlassian-board-ids "1.7")

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
  "Cached Kanban board data: a list of board alists.
Each entry is ((id . ID) (name . NAME) (columns . COLUMNS)) as
returned by `shipit-atlassian-board--fetch-all'.")

(defvar-local shipit-atlassian-dashboard--backend-plist nil
  "Backend plist resolved for this dashboard buffer.")

(defvar-local shipit-atlassian-dashboard--issues-data nil
  "Cached list of issues loaded for the Issues section.")

(defvar-local shipit-atlassian-dashboard--loading-p nil
  "Non-nil while a refresh is in flight.
The renderer prefixes the buffer with a visible `Loading...' banner
while this is set; it clears once all fetches complete.")

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
    (define-key map (kbd "C-x C-q") #'shipit-atlassian-dashboard-toggle-edit-mode)
    (define-key map (kbd "M-n") #'shipit-atlassian-dashboard-issue-forward)
    (define-key map (kbd "M-p") #'shipit-atlassian-dashboard-issue-backward)
    (define-key map (kbd "C-c C-c") #'shipit-atlassian-dashboard-commit-pending)
    (define-key map (kbd "C-c C-k") #'shipit-atlassian-dashboard-discard-pending)
    map)
  "Keymap for `shipit-atlassian-dashboard-mode'.")

(defun shipit-atlassian-dashboard--match-transition (transitions target-name)
  "Find the transition in TRANSITIONS whose target name equals TARGET-NAME.
TRANSITIONS is the list returned by the issue backend\='s
`:get-transitions\=' op.  Each element is an alist; we match on
the nested `to.name\='.  Comparison is case-insensitive.
Returns the transition alist, or nil if none match."
  (when target-name
    (seq-find (lambda (tr)
                (let ((to-name (cdr (assq 'name (cdr (assq 'to tr))))))
                  (and to-name
                       (string-equal-ignore-case to-name target-name))))
              transitions)))

(defun shipit-atlassian-dashboard--issue-at-point ()
  "Return a plist (:key K :status S :on-board P :board-id ID) for point, or nil.
Walks up to the nearest `atlassian-issue\=' section, reads the
key / status text properties stashed on its heading, and notes
whether the issue is inside a board-column section -- plus which
board section it sits under -- so the edit-mode commands can step
within that board's column order instead of the canonical status
list."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'atlassian-issue)))
      (setq section (oref section parent)))
    (when section
      (let* ((start (oref section start))
             (key (get-text-property start 'shipit-issuelink-key))
             (status (get-text-property start 'shipit-issue-status))
             (on-board nil)
             (board-id nil)
             (cur (oref section parent)))
        (while cur
          (pcase (oref cur type)
            ('atlassian-board-column (setq on-board t))
            ('atlassian-board (setq board-id (oref cur value))))
          (setq cur (oref cur parent)))
        (let ((info (and key (list :key key :status status
                                   :on-board on-board :board-id board-id))))
          (shipit--debug-log "edit: --issue-at-point => %S" info)
          info)))))

(defun shipit-atlassian-dashboard--board-columns (board)
  "Return the columns list of BOARD (a board alist)."
  (cdr (assq 'columns board)))

(defun shipit-atlassian-dashboard--board-by-id (board-id)
  "Return the board alist with BOARD-ID from the cached board data."
  (cl-find board-id shipit-atlassian-dashboard--board-data
           :key (lambda (board) (cdr (assq 'id board)))))

(defun shipit-atlassian-dashboard--all-board-columns ()
  "Concatenated columns of every cached board, in configured order."
  (cl-mapcan (lambda (board)
               (copy-sequence (shipit-atlassian-dashboard--board-columns board)))
             shipit-atlassian-dashboard--board-data))

(defun shipit-atlassian-dashboard--column-in (columns status)
  "Return the column alist in COLUMNS accepting STATUS, or nil.
Matches case-insensitively against each column\='s `statuses\=' list."
  (when status
    (cl-find-if
     (lambda (col)
       (cl-some (lambda (s)
                  (and s (string-equal-ignore-case s status)))
                (cdr (assq 'statuses col))))
     columns)))

(defun shipit-atlassian-dashboard--columns-accepting (status)
  "Return the columns of the first cached board accepting STATUS, or nil."
  (cl-loop for board in shipit-atlassian-dashboard--board-data
           for cols = (shipit-atlassian-dashboard--board-columns board)
           when (shipit-atlassian-dashboard--column-in cols status)
           return cols))

(defun shipit-atlassian-dashboard--board-target-status (current direction &optional board-id)
  "Return a target status name one column DIRECTION from CURRENT, or nil.
Steps within a single board's column order: BOARD-ID's board when
given (the board the issue is displayed on), otherwise the first
cached board with a column accepting CURRENT.  Returns the first
status name listed for the destination column.  No-op when CURRENT
isn\='t represented, or when stepping would move past either end --
stepping never crosses a board boundary."
  (when current
    (let* ((cols (if board-id
                     (shipit-atlassian-dashboard--board-columns
                      (shipit-atlassian-dashboard--board-by-id board-id))
                   (shipit-atlassian-dashboard--columns-accepting current)))
           (cur-col (shipit-atlassian-dashboard--column-in cols current))
           (idx (and cur-col (cl-position cur-col cols :test #'eq))))
      (shipit--debug-log
       "edit: --board-target-status current=%S board=%S col=%S idx=%S total-cols=%d"
       current board-id
       (and cur-col (cdr (assq 'name cur-col)))
       idx (length cols))
      (when idx
        (let* ((step-idx (pcase direction
                           ('forward (1+ idx))
                           ('backward (1- idx))))
               (next-col (and (integerp step-idx)
                              (>= step-idx 0)
                              (< step-idx (length cols))
                              (nth step-idx cols)))
               (statuses (and next-col (cdr (assq 'statuses next-col))))
               (target (car statuses)))
          (shipit--debug-log
           "edit: --board-target-status step-idx=%S next-col=%S statuses=%S target=%S"
           step-idx
           (and next-col (cdr (assq 'name next-col)))
           statuses target)
          target)))))

(defvar-local shipit-atlassian-dashboard-edit-mode nil
  "Non-nil while the dashboard buffer is in edit mode.
Toggled via `C-x C-q' (`shipit-atlassian-dashboard-toggle-edit-mode\=').
When on, `M-n' / `M-p' on an issue line advance/regress its
status via Jira transitions; off (the default) leaves those
keys with their global bindings.  The buffer remains
`buffer-read-only\=' either way -- only the navigation keys
change behaviour.")

(defun shipit-atlassian-dashboard--issue-key (issue)
  "Return the work-item key for raw issue ISSUE.
Goes through `shipit-issue--normalize-to-work-item' so the
helper works for either the post-normalised dashboard format
\(`key' field) or the Jira-normalised format (`number' field)."
  (cdr (assq 'key (shipit-issue--normalize-to-work-item issue))))

(defun shipit-atlassian-dashboard--find-issue-in-columns (columns key)
  "Return the issue alist with KEY somewhere in COLUMNS, or nil.
Case-insensitive match.  Normalises each candidate issue via
`shipit-atlassian-dashboard--issue-key' so this works against
both raw Jira agile data and the mapped work-item shape used
in tests."
  (cl-loop for col in columns
           thereis (cl-find-if
                    (lambda (issue)
                      (let ((k (shipit-atlassian-dashboard--issue-key issue)))
                        (and k (string-equal-ignore-case k key))))
                    (cdr (assq 'issues col)))))

(defun shipit-atlassian-dashboard--find-issue-in-board-data (key)
  "Return the issue alist with KEY on any cached board, or nil."
  (cl-loop for board in shipit-atlassian-dashboard--board-data
           thereis (shipit-atlassian-dashboard--find-issue-in-columns
                    (shipit-atlassian-dashboard--board-columns board) key)))

(defun shipit-atlassian-dashboard--column-name-for-status (status-name &optional columns)
  "Return the column name that would accept STATUS-NAME, or nil.
Searches COLUMNS when given (a single board's columns), otherwise
every cached board in configured order.  When the status mapping
comes from the board API (Jira agile boards), this is the right way
to route a canonical-order target like \"In Review\" to whichever
column the user has configured -- which might be \"Code Review\",
\"In QA\", or anything else."
  (cdr (assq 'name
             (shipit-atlassian-dashboard--column-in
              (or columns (shipit-atlassian-dashboard--all-board-columns))
              status-name))))

(defun shipit-atlassian-dashboard--pending-arrivals-into-column (board col-name)
  "Return issue alists whose pending target routes to COL-NAME on BOARD.
Routes via `shipit-atlassian-dashboard--column-name-for-status\='
against BOARD's own columns \(falling back to a direct
case-insensitive match) so a target like \"In Review\" lands in
whichever column BOARD configures to accept it.  Only issues that
BOARD actually displays are reported -- another board's same-named
column must not claim the arrival."
  (when col-name
    (let ((cols (shipit-atlassian-dashboard--board-columns board))
          acc)
      (dolist (entry shipit-atlassian-dashboard--pending-transitions)
        (let* ((target (cdr entry))
               (resolved-col
                (or (shipit-atlassian-dashboard--column-name-for-status target cols)
                    target)))
          (when (and resolved-col
                     (string-equal-ignore-case resolved-col col-name))
            (let ((issue (shipit-atlassian-dashboard--find-issue-in-columns
                          cols (car entry))))
              (when issue (push issue acc))))))
      (nreverse acc))))

(defun shipit-atlassian-dashboard--update-mode-line ()
  "Reflect `shipit-atlassian-dashboard-edit-mode' in the mode line."
  (setq mode-name
        (if shipit-atlassian-dashboard-edit-mode
            "Shipit-Atlassian:Edit"
          "Shipit-Atlassian"))
  (force-mode-line-update))

(defun shipit-atlassian-dashboard-toggle-edit-mode ()
  "Toggle issue-transition edit mode in the dashboard buffer.
While on, `M-n' / `M-p' on a board card stage a move to the
next/previous column, `C-c C-c' commits, `C-c C-k' discards.
The buffer stays read-only -- this only changes the meaning of
the navigation keys.

Exiting with pending transitions asks first: confirming
discards them (so leaving edit mode never silently keeps
ghost moves around), declining stays in edit mode."
  (interactive)
  (let* ((leaving shipit-atlassian-dashboard-edit-mode)
         (pending (length shipit-atlassian-dashboard--pending-transitions))
         (proceed (or (not leaving)
                      (zerop pending)
                      (yes-or-no-p
                       (format "Exit edit mode and discard %d pending transition%s? "
                               pending (if (= pending 1) "" "s"))))))
    (cond
     ((not proceed)
      (message "Stayed in edit mode (%d pending)" pending))
     (t
      (when (and leaving (not (zerop pending)))
        (setq shipit-atlassian-dashboard--pending-transitions nil)
        (when (fboundp 'shipit-atlassian-dashboard--render)
          (shipit-atlassian-dashboard--render)))
      (setq shipit-atlassian-dashboard-edit-mode (not leaving))
      (shipit-atlassian-dashboard--update-mode-line)
      (message "Atlassian dashboard edit mode: %s"
               (if shipit-atlassian-dashboard-edit-mode "on" "off"))))))

(defvar-local shipit-atlassian-dashboard--pending-transitions nil
  "Alist of (KEY . TARGET-STATUS) staged but not yet committed.
Populated by `M-n' / `M-p' in edit mode, drained by `C-c C-c'
\(`shipit-atlassian-dashboard-commit-pending\='), discarded by
`C-c C-k\='.

The display layer reads this so each row shows the staged
target alongside the current status until commit -- standard
Emacs commit semantics: toggle `read-only-mode\=' (`C-x C-q'),
make changes, commit (`C-c C-c') or abort (`C-c C-k\=').")

(defun shipit-atlassian-dashboard--effective-status-for (key actual-status)
  "Return the status to display for KEY -- pending target if any, else ACTUAL-STATUS.
Used by the renderer so a row reflects the staged change before
the user commits."
  (or (cdr (assoc key shipit-atlassian-dashboard--pending-transitions))
      actual-status))

(defun shipit-atlassian-dashboard--update-mode-line ()
  "Reflect edit-mode + pending-count in the mode line and header line.
The header line is the prominent cheat-sheet -- mode-name-only
gets glanced at, but the dedicated row at the top of the buffer
\(visible only in edit mode) keeps the staging keys and commit
key in plain sight."
  (let* ((edit shipit-atlassian-dashboard-edit-mode)
         (n (length shipit-atlassian-dashboard--pending-transitions))
         (suffix (cond ((not edit) "")
                       ((zerop n) ":Edit")
                       (t (format ":Edit/%d" n)))))
    (setq mode-name (concat "Shipit-Atlassian" suffix))
    (setq header-line-format
          (and edit
               (concat
                (propertize " EDIT MODE "
                            'face '(:inherit warning :weight bold :inverse-video t))
                "  "
                (propertize "M-n" 'face 'help-key-binding) " / "
                (propertize "M-p" 'face 'help-key-binding) " stage move   "
                (propertize "C-c C-c" 'face 'help-key-binding) " "
                (propertize "commit" 'face 'success) "   "
                (propertize "C-c C-k" 'face 'help-key-binding) " "
                (propertize "discard" 'face 'shadow) "   "
                (propertize "C-x C-q" 'face 'help-key-binding) " exit"
                (when (> n 0)
                  (propertize (format "   [%d pending]" n)
                              'face '(:inherit warning :weight bold)))))))
  (force-mode-line-update))

(defun shipit-atlassian-dashboard--rerender-and-update-modeline ()
  "Re-render the dashboard from cached data and refresh the mode line.
Cheaper than `shipit-atlassian-dashboard-refresh\=' (no API
calls).  After re-render, expands any board column that has a
pending arrival -- user-toggled visibility for every other
section is preserved by magit\='s section-visibility cache."
  (when (fboundp 'shipit-atlassian-dashboard--render)
    (shipit-atlassian-dashboard--render))
  (shipit-atlassian-dashboard--expand-columns-with-pending-arrivals)
  (shipit-atlassian-dashboard--update-mode-line))

(defun shipit-atlassian-dashboard--goto-issue-on-board (key)
  "Move point to the `atlassian-issue\=' section for KEY under the board.
Walks the new section tree (post-rerender), finds the issue
section whose value equals KEY whose ancestry includes
`atlassian-board\=', and jumps point there.  No-op if the issue
isn\='t present (commit just removed it, etc.)."
  (when key
    (let (target)
      (cl-labels
          ((walk (section under-board)
             (when (and section (not target))
               (let ((type (oref section type)))
                 (cond
                  ((eq type 'atlassian-issue)
                   (when (and under-board
                              (equal (oref section value) key))
                     (setq target section)))
                  (t
                   (let ((next-under (or under-board
                                         (eq type 'atlassian-board))))
                     (dolist (child (oref section children))
                       (walk child next-under)))))))))
        (walk magit-root-section nil))
      (when target
        (push-mark nil t)
        (goto-char (oref target start))))))

(defun shipit-atlassian-dashboard--pending-expand-targets ()
  "Return an alist of (BOARD-ID . COLUMN-NAMES) for staged transitions.
For each pending target, the destination column is resolved within
each board that actually displays the issue, so only that board's
column sections are expanded."
  (let (result)
    (dolist (board shipit-atlassian-dashboard--board-data)
      (let ((cols (shipit-atlassian-dashboard--board-columns board))
            (names nil))
        (dolist (entry shipit-atlassian-dashboard--pending-transitions)
          (when (shipit-atlassian-dashboard--find-issue-in-columns cols (car entry))
            (let ((name (shipit-atlassian-dashboard--column-name-for-status
                         (cdr entry) cols)))
              (when name (push name names)))))
        (when names
          (push (cons (cdr (assq 'id board)) (delete-dups (nreverse names)))
                result))))
    (nreverse result)))

(defun shipit-atlassian-dashboard--expand-columns-with-pending-arrivals ()
  "Make sure every board column that has a pending arrival is shown.
Resolves each pending target to (BOARD-ID . COLUMN-NAMES) pairs,
then walks the section tree showing every matching
`atlassian-board-column\=' \(plus its `atlassian-board\=' and
`atlassian-boards\=' wrappers).  Comparison uses each column
section\='s `value\=' field -- the column name -- under the board
section whose `value\=' is the board ID.  Other sections keep
whatever visibility state magit\='s cache held."
  (let ((targets (shipit-atlassian-dashboard--pending-expand-targets)))
    (when targets
      (cl-labels
          ((walk (section board-names)
             (when section
               (let ((type (oref section type)))
                 (cond
                  ((eq type 'atlassian-boards)
                   (when (oref section hidden)
                     (magit-section-show section))
                   (dolist (child (oref section children))
                     (walk child board-names)))
                  ((eq type 'atlassian-board)
                   (let ((names (cdr (assoc (oref section value) targets))))
                     (when names
                       (when (oref section hidden)
                         (magit-section-show section))
                       (dolist (child (oref section children))
                         (walk child names)))))
                  ((eq type 'atlassian-board-column)
                   (let ((col-name (oref section value)))
                     (when (and col-name
                                (cl-some (lambda (target)
                                           (and target
                                                (string-equal-ignore-case target col-name)))
                                         board-names)
                                (oref section hidden))
                       (magit-section-show section))))
                  (t
                   (dolist (child (oref section children))
                     (walk child board-names))))))))
        (walk magit-root-section nil)))))

(defun shipit-atlassian-dashboard--stage-issue-at-point (direction)
  "Stage DIRECTION for the issue at point.
Only meaningful inside a board-column section -- target is
derived from the next/previous board column\='s first
configured status, computed against the issue\='s EFFECTIVE
status (the already-staged target if there is one, else the
actual status).  That makes consecutive `M-n\=' presses stack:
press twice to skip a column, three times to skip two."
  (let ((info (shipit-atlassian-dashboard--issue-at-point)))
    (unless info (user-error "No issue at point"))
    (let* ((key (plist-get info :key))
           (status (plist-get info :status))
           (on-board (plist-get info :on-board))
           (effective (shipit-atlassian-dashboard--effective-status-for
                       key status))
           (target (and on-board
                        (shipit-atlassian-dashboard--board-target-status
                         effective direction (plist-get info :board-id)))))
      (shipit--debug-log
       "edit: stage %s key=%S status=%S effective=%S on-board=%s target=%S"
       direction key status effective on-board target)
      (unless on-board
        (user-error "Move only works on the Board section -- navigate there first"))
      (unless target
        (user-error "No %s column from %S on this board"
                    direction (or effective "(unknown)")))
      (shipit-atlassian-dashboard--stage-transition-to-target
       key status target)
      (shipit-atlassian-dashboard--rerender-and-update-modeline)
      (shipit-atlassian-dashboard--goto-issue-on-board key))))

(defun shipit-atlassian-dashboard--stage-transition-to-target (key actual-status target)
  "Stage a pending move for KEY (currently ACTUAL-STATUS) to TARGET.
If TARGET equals ACTUAL-STATUS the entry is dropped (net-zero
— the staged moves cancelled out).  Otherwise the pending
entry is created or replaced -- pressing `M-n\=' a second time
just bumps the staged target one column further, which is what
multi-step moves like \"To Do\" -> \"Review & Test\" need."
  (cond
   ((and actual-status (string-equal-ignore-case target actual-status))
    (setq shipit-atlassian-dashboard--pending-transitions
          (assoc-delete-all key shipit-atlassian-dashboard--pending-transitions))
    (shipit--debug-log "edit: net-zero, removed %S from pending" key))
   (t
    (setq shipit-atlassian-dashboard--pending-transitions
          (cons (cons key target)
                (assoc-delete-all key shipit-atlassian-dashboard--pending-transitions)))
    (shipit--debug-log "edit: set %S -> %S; pending=%S"
                       key target
                       shipit-atlassian-dashboard--pending-transitions))))

(defun shipit-atlassian-dashboard-issue-forward ()
  "Stage a move to the next board column for the issue at point.
Only acts on issues inside a board-column section; signals
`user-error\=' elsewhere.  Target = first configured status of
the next column.  No API call until `C-c C-c\='.  Only active
in edit mode (`C-x C-q\=')."
  (interactive)
  (unless shipit-atlassian-dashboard-edit-mode
    (user-error "Press C-x C-q to enter edit mode first"))
  (shipit-atlassian-dashboard--stage-issue-at-point 'forward))

(defun shipit-atlassian-dashboard-issue-backward ()
  "Stage a move to the previous board column for the issue at point.
Mirror of `shipit-atlassian-dashboard-issue-forward\='."
  (interactive)
  (unless shipit-atlassian-dashboard-edit-mode
    (user-error "Press C-x C-q to enter edit mode first"))
  (shipit-atlassian-dashboard--stage-issue-at-point 'backward))

(defun shipit-atlassian-dashboard--column-index-for-status (status columns)
  "Return STATUS\='s column index within COLUMNS, or nil."
  (when status
    (cl-position-if
     (lambda (col)
       (cl-some (lambda (s)
                  (and s (string-equal-ignore-case s status)))
                (cdr (assq 'statuses col))))
     columns)))

(defun shipit-atlassian-dashboard--pick-step-transition (transitions target)
  "Choose a transition that moves toward TARGET column.
Returns the transition whose `to.name\=' matches TARGET directly,
or -- when no direct path exists -- the one whose `to\=' column
is closest to TARGET in the board\='s column order without
overshooting.  Multi-step staged moves rely on this so the
commit walker can step from \"To Do\" to \"Review & Test\" via
\"In Progress\" automatically."
  (let* ((direct (shipit-atlassian-dashboard--match-transition
                  transitions target))
         (cols (shipit-atlassian-dashboard--columns-accepting target))
         (target-idx (and cols
                          (shipit-atlassian-dashboard--column-index-for-status
                           target cols))))
    (or direct
        (and target-idx
             (let (best best-idx)
               (dolist (tr transitions)
                 (let* ((to-name (cdr (assq 'name (cdr (assq 'to tr)))))
                        (idx (shipit-atlassian-dashboard--column-index-for-status
                              to-name cols)))
                   (when (and idx
                              (or (null best-idx)
                                  (< (abs (- idx target-idx))
                                     (abs (- best-idx target-idx)))))
                     (setq best tr best-idx idx))))
               best)))))

(defun shipit-atlassian-dashboard--commit-one (repo key target)
  "Walk transitions for KEY in REPO until it reaches TARGET (or gets stuck).
Returns t on success, signals an error otherwise.  Caps the
walk at 8 steps -- a safety net against pathological workflows
that would otherwise loop indefinitely."
  (let* ((steps 0)
         (last-fired nil)
         (target-cols (shipit-atlassian-dashboard--columns-accepting target))
         (target-idx (shipit-atlassian-dashboard--column-index-for-status
                      target target-cols)))
    (shipit--debug-log
     "commit: %s target=%S target-idx=%S board-cols=%S"
     key target target-idx
     (mapcar (lambda (c)
               (list :name (cdr (assq 'name c))
                     :statuses (cdr (assq 'statuses c))))
             target-cols))
    (catch 'done
      (while t
        (when (>= steps 8)
          (error "Gave up after 8 steps"))
        (let* ((transitions (shipit-issues--get-transitions repo key))
               (_ (shipit--debug-log
                   "commit: %s step=%d available=%S"
                   key steps
                   (mapcar (lambda (tr)
                             (cons (cdr (assq 'name tr))
                                   (cdr (assq 'name (cdr (assq 'to tr))))))
                           transitions)))
               (tr (shipit-atlassian-dashboard--pick-step-transition
                    transitions target))
               (to-name (and tr (cdr (assq 'name (cdr (assq 'to tr)))))))
          (shipit--debug-log "commit: %s picked=%S to-name=%S"
                             key (cdr (assq 'name tr)) to-name)
          (unless tr
            (error "No transition toward %S available" target))
          (when (and last-fired
                     to-name
                     (string-equal-ignore-case to-name last-fired))
            (error "Transition loop detected at %S" last-fired))
          (shipit-issues--transition-status repo key (cdr (assq 'id tr)))
          (cl-incf steps)
          (setq last-fired to-name)
          (when (and to-name target
                     (string-equal-ignore-case to-name target))
            (throw 'done t)))))))

(defun shipit-atlassian-dashboard-commit-pending ()
  "Apply all staged transitions to Jira, then refresh the dashboard.
Each pending (KEY . TARGET) is walked one Jira transition at a
time -- direct first, otherwise the closest move toward TARGET --
until the target column is reached.  This handles multi-step
moves like \"To Do\" -> \"Review & Test\" that Jira workflows
typically expose only as one-column-at-a-time transitions.

Stops on the first failure; the failed entry stays pending so
the user can retry or discard it.  All-success clears pending
and refreshes."
  (interactive)
  (unless shipit-atlassian-dashboard-edit-mode
    (user-error "Not in edit mode (C-x C-q to enter)"))
  (let ((pending shipit-atlassian-dashboard--pending-transitions)
        (repo (or shipit-atlassian-dashboard--repo
                  (user-error "Dashboard buffer is missing repo context")))
        (committed 0)
        (failures '()))
    (unless pending
      (user-error "No pending transitions to commit"))
    (dolist (entry pending)
      (let ((key (car entry))
            (target (cdr entry)))
        (condition-case err
            (progn
              (shipit-atlassian-dashboard--commit-one repo key target)
              (cl-incf committed)
              (setq shipit-atlassian-dashboard--pending-transitions
                    (assoc-delete-all
                     key shipit-atlassian-dashboard--pending-transitions)))
          (error
           (push (format "%s -> %s: %s"
                         key target (error-message-string err))
                 failures)))))
    (cond
     (failures
      (shipit-atlassian-dashboard--update-mode-line)
      (message "Committed %d, failed %d: %s"
               committed (length failures)
               (mapconcat #'identity (nreverse failures) "; ")))
     (t
      (message "Committed %d transition%s"
               committed (if (= committed 1) "" "s"))
      (shipit-atlassian-dashboard-refresh)
      (shipit-atlassian-dashboard--update-mode-line)))))

(defun shipit-atlassian-dashboard-discard-pending ()
  "Drop every staged transition and re-render with actual statuses.
Asks for confirmation when there are 2+ entries -- a single
mistake is cheap to redo, but discarding a multi-issue plan
deserves a confirmation."
  (interactive)
  (let ((n (length shipit-atlassian-dashboard--pending-transitions)))
    (when (zerop n)
      (user-error "No pending transitions to discard"))
    (when (or (= n 1)
              (yes-or-no-p (format "Discard %d pending transition%s? "
                                   n (if (= n 1) "" "s"))))
      (setq shipit-atlassian-dashboard--pending-transitions nil)
      (shipit-atlassian-dashboard--rerender-and-update-modeline)
      (message "Discarded %d pending transition%s"
               n (if (= n 1) "" "s")))))

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
All dashboard sections are collapsed on FIRST render.  Subsequent
renders -- e.g. re-renders triggered by `M-n' / `M-p' staging
during edit mode -- defer to magit\='s per-section cached
visibility so user-expanded sections stay open.

We return `hide' only when there\='s no cached state for this
section; with a cached state we return nil and let magit apply
it."
  (when (memq (oref section type)
              '(atlassian-dashboard atlassian-my-issues
                atlassian-whats-next atlassian-boards atlassian-board
                atlassian-frequently-visited
                atlassian-board-column atlassian-my-issues-project
                atlassian-issues atlassian-issues-project-group
                atlassian-issue))
    (let ((cached (and (boundp 'magit-section-visibility-cache)
                       magit-section-visibility-cache
                       (magit-section-cached-visibility section))))
      (if cached nil 'hide))))

;;; Mode definition

(define-derived-mode shipit-atlassian-dashboard-mode magit-section-mode "Shipit-Atlassian"
  "Major mode for Atlassian/Jira project dashboard buffers.
Displays project overview with My Open Issues, What\='s Next,
Kanban Boards, and Frequently Visited sections.

\\{shipit-atlassian-dashboard-mode-map}"
  :group 'shipit-atlassian
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (shipit-atlassian-dashboard-refresh)))
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  ;; Cache section visibility across re-renders so M-n/M-p staging in
  ;; edit mode doesn\='t collapse every section the user has expanded.
  (setq-local magit-section-cache-visibility t)
  (add-hook 'magit-section-set-visibility-hook
            #'shipit-atlassian-dashboard--section-visibility nil t)
  (setq-local magit-root-section nil)
  (shipit--apply-section-defaults))

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
creates or switches to the dashboard buffer.  Errors loudly when
REPO is not mapped to a Jira-capable issue backend — the dashboard
renders Jira-specific sections (My Open Issues, Kanban board,
workflow statuses) and silently degrades to broken output when
driven by another backend.  Returns the buffer."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend-plist (car resolved))
         (config (cdr resolved)))
    ;; The dashboard renders Jira-specific sections.  Use the presence
    ;; of :fetch-project-statuses — a Jira-only backend capability — as
    ;; the guard, and :base-url in config as a secondary check that
    ;; a real Jira instance is configured.
    (unless (and (plist-get backend-plist :fetch-project-statuses)
                 (plist-get config :base-url))
      (user-error
       "Atlassian dashboard requires a Jira-configured repo; %s resolves to the %s backend.  Add a mapping in `shipit-issue-repo-backends'"
       repo (or (plist-get backend-plist :name) "active")))
    (let* ((buf-name (shipit-atlassian-dashboard--buffer-name repo))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (unless (eq major-mode 'shipit-atlassian-dashboard-mode)
          (shipit-atlassian-dashboard-mode))
        (setq shipit-atlassian-dashboard--repo repo)
        (setq shipit-atlassian-dashboard--config config)
        (setq shipit-atlassian-dashboard--backend-plist backend-plist)
        (setq shipit-issue-buffer-repo repo)
        (shipit--debug-log "Atlassian dashboard opened for repo: %s" repo)
        (shipit-atlassian-dashboard--refresh-data))
      (pop-to-buffer buf)
      buf)))

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
                        (mapconcat (lambda (k) (format "\"%s\"" k))
                                   project-keys ","))
              ""))))

(defun shipit-atlassian-dashboard--user-clause (field value)
  "Render a JQL clause for FIELD (assignee or reporter) based on VALUE.
VALUE may be the symbol `me', `unassigned', or a free-text string."
  (pcase value
    ('me (format "%s=currentUser()" field))
    ('unassigned (format "%s is EMPTY" field))
    ((pred stringp) (format "%s=\"%s\"" field value))))

(defun shipit-atlassian-dashboard--normalize-date-offset (value)
  "Return VALUE with month/year JQL offsets rewritten to days.
JQL only recognises `h m d w' as relative date units — lowercase `m'
is minutes, not months.  This converter lets users type natural `-1M'
(capital M = months) / `-1y' forms — the stored filter string is
preserved for UI display; only the JQL clause uses the day-equivalent.
Absolute dates and JQL functions pass through."
  (let ((case-fold-search nil))
    (cond
     ((null value) value)
     ((string-match "\\`\\(-?[0-9]+\\)M\\'" value)
      (format "%dd" (* 30 (string-to-number (match-string 1 value)))))
     ((string-match "\\`\\(-?[0-9]+\\)y\\'" value)
      (format "%dd" (* 365 (string-to-number (match-string 1 value)))))
     (t value))))

(defun shipit-atlassian-dashboard--build-issues-jql (config filter)
  "Build the JQL query for the Issues section.
CONFIG supplies :project-keys; FILTER is a plist of user filters.
Returns a JQL string."
  (let ((clauses nil)
        (project-keys (plist-get config :project-keys)))
    (when project-keys
      (push (format "project in (%s)"
                    (mapconcat (lambda (k) (format "\"%s\"" k))
                               project-keys ","))
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
    (when-let* ((val (plist-get filter :component)))
      (push (format "component=\"%s\"" val) clauses))
    (when-let* ((val (plist-get filter :updated-since)))
      (push (format "updated >= %s"
                    (shipit-atlassian-dashboard--normalize-date-offset val))
            clauses))
    (when-let* ((val (plist-get filter :updated-before)))
      (push (format "updated <= %s"
                    (shipit-atlassian-dashboard--normalize-date-offset val))
            clauses))
    (when-let* ((val (plist-get filter :created-since)))
      (push (format "created >= %s"
                    (shipit-atlassian-dashboard--normalize-date-offset val))
            clauses))
    (when-let* ((val (plist-get filter :created-before)))
      (push (format "created <= %s"
                    (shipit-atlassian-dashboard--normalize-date-offset val))
            clauses))
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

(defun shipit-atlassian-dashboard--dedup-issues-by-key (issues)
  "Drop later duplicates from ISSUES, comparing work-item keys.
Comparison is case-insensitive; the first occurrence wins.  Issues
without a resolvable key are kept."
  (let (seen acc)
    (dolist (issue issues)
      (let ((key (shipit-atlassian-dashboard--issue-key issue)))
        (unless (and key (member (upcase key) seen))
          (when key (push (upcase key) seen))
          (push issue acc))))
    (nreverse acc)))

(defun shipit-atlassian-dashboard--fetch-whats-next-for-board (board-id)
  "Fetch what's-next items from BOARD-ID's active sprint.
Returns nil when the board has no active sprint."
  (let* ((config shipit-atlassian-dashboard--config)
         (sprint-raw (shipit-issue-jira--api-request
                      config
                      (shipit-atlassian-dashboard--active-sprint-path board-id)))
         (active-sprint (car (append (cdr (assq 'values sprint-raw)) nil))))
    (when active-sprint
      (let* ((sprint-id (cdr (assq 'id active-sprint)))
             (issues-raw (shipit-issue-jira--api-request
                          config
                          (shipit-atlassian-dashboard--sprint-issues-path sprint-id))))
        (shipit--debug-log
         "Atlassian dashboard: fetched %d whats-next items from sprint %s (board %d)"
         (length (append (cdr (assq 'issues issues-raw)) nil))
         (cdr (assq 'name active-sprint)) board-id)
        (shipit-atlassian-dashboard--normalize-agile-issues issues-raw)))))

(defun shipit-atlassian-dashboard--fetch-whats-next ()
  "Fetch what's-next items from every configured board's active sprint.
Aggregates across `shipit-atlassian-board-ids' in order and drops
issues that appear on more than one board.  Returns nil when no
board is configured."
  (shipit-atlassian-dashboard--dedup-issues-by-key
   (cl-mapcan #'shipit-atlassian-dashboard--fetch-whats-next-for-board
              shipit-atlassian-board-ids)))

(defun shipit-atlassian-dashboard--fetch-boards ()
  "Fetch data for every board in `shipit-atlassian-board-ids'.
Returns nil when no board is configured."
  (shipit-atlassian-board--fetch-all
   shipit-atlassian-dashboard--config
   shipit-atlassian-board-ids))

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

(defun shipit-atlassian-dashboard--insert-issue-line (issue columns widths &optional opts)
  "Insert a single ISSUE as a collapsible magit section.
COLUMNS is a list of column symbols, WIDTHS is the widths alist.
OPTS is an optional plist with two tweaks for staged-transition
rendering:

  :face FACE      -- a font-lock-face overlaid across the row.
                     Used by the board renderer when injecting
                     a pending arrival so the moved issue reads
                     visually as \"this is provisional.\"
  :show-pending-arrow BOOL
                  -- when t (default), append the inline
                     `→ TARGET\=' indicator if the issue has a
                     pending transition.  Set nil from the
                     board renderer because the issue has
                     already been moved to the target column;
                     the arrow would be redundant.
  :indent STRING  -- line prefix before the row; defaults to
                     three spaces.  Board columns pass their
                     deeper indent so rows align under the
                     column heading."
  (let* ((mapped (shipit-issue--normalize-to-work-item issue))
         (key (cdr (assq 'key mapped)))
         (status (cdr (assq 'status mapped)))
         (pending-target (cdr (assoc key shipit-atlassian-dashboard--pending-transitions)))
         (show-arrow (if (plist-member opts :show-pending-arrow)
                         (plist-get opts :show-pending-arrow)
                       t))
         (face-overlay (plist-get opts :face))
         (base (shipit-issue--format-work-item-line mapped columns widths))
         (line (concat (or (plist-get opts :indent) "   ") base
                       (when (and show-arrow pending-target)
                         (concat " "
                                 (propertize (format "→ %s" pending-target)
                                             'font-lock-face
                                             'shipit-atlassian-dashboard-pending-face)))
                       "\n")))
    (magit-insert-section (atlassian-issue key)
      (let ((row-start (point)))
        (magit-insert-heading
          (propertize line
                      'shipit-issuelink-key key
                      'shipit-issue-status status))
        (when face-overlay
          (shipit--prepend-font-lock-face row-start (point) face-overlay))))))

(defun shipit-atlassian-dashboard--insert-my-issues-section ()
  "Insert My Open Issues section."
  (let ((issues shipit-atlassian-dashboard--my-issues))
    (magit-insert-section (atlassian-my-issues)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "issues" "\U0001f4cb")
                (propertize (format "My Open Issues (%d)" (length issues))
                            'font-lock-face 'magit-section-heading)))
      (if (null issues)
          (insert (propertize "   No open issues\n" 'font-lock-face 'magit-dimmed))
        (let* ((prepared (shipit-atlassian-dashboard--prepare-widths issues))
               (columns (car prepared))
               (widths (cdr prepared))
               (grouped (shipit-atlassian-dashboard--group-by-project issues)))
          (dolist (group grouped)
            (magit-insert-section (atlassian-my-issues-project (car group))
              (magit-insert-heading
                (propertize (format "   %s (%d)" (car group) (length (cdr group)))
                            'font-lock-face 'magit-section-heading))
              (dolist (issue (cdr group))
                (shipit-atlassian-dashboard--insert-issue-line issue columns widths)))))
        (insert "\n")))))

(defun shipit-atlassian-dashboard--insert-whats-next-section ()
  "Insert What's Next section."
  (magit-insert-section (atlassian-whats-next)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "milestone" "\U0001f3af")
              (propertize (format "What's Next (%d)"
                                  (length shipit-atlassian-dashboard--whats-next))
                          'font-lock-face 'magit-section-heading)))
    (cond
     ((null shipit-atlassian-board-ids)
      (insert (propertize "   Press RET to configure board IDs\n"
                          'font-lock-face 'magit-dimmed
                          'shipit-atlassian-configure 'board-ids)))
     ((null shipit-atlassian-dashboard--whats-next)
      (insert (propertize "   No items in current sprint\n" 'font-lock-face 'magit-dimmed)))
     (t
      (let* ((prepared (shipit-atlassian-dashboard--prepare-widths
                        shipit-atlassian-dashboard--whats-next))
             (columns (car prepared))
             (widths (cdr prepared)))
        (dolist (issue shipit-atlassian-dashboard--whats-next)
          (shipit-atlassian-dashboard--insert-issue-line issue columns widths)))))
    (insert "\n")))

(defconst shipit-atlassian-dashboard--board-indent "   "
  "Indent prefix for a board heading under the Boards section.
Column headings and issue rows derive their deeper indent from
this, keeping every level one step (3 columns) inside its parent.")

(defun shipit-atlassian-dashboard--board-issue-count (board)
  "Total number of issues across BOARD's columns."
  (cl-loop for col in (shipit-atlassian-dashboard--board-columns board)
           sum (length (cdr (assq 'issues col)))))

(defun shipit-atlassian-dashboard--insert-boards-section ()
  "Insert the Boards section, one collapsible subsection per board."
  (magit-insert-section (atlassian-boards)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "project" "\U0001f4ca")
              (propertize (if shipit-atlassian-board-ids
                              (format "Boards (%d)"
                                      (length shipit-atlassian-board-ids))
                            "Boards")
                          'font-lock-face 'magit-section-heading)))
    (cond
     ((null shipit-atlassian-board-ids)
      (insert (propertize "   Press RET to configure board IDs\n"
                          'font-lock-face 'magit-dimmed
                          'shipit-atlassian-configure 'board-ids)))
     ((null shipit-atlassian-dashboard--board-data)
      (insert (propertize "   No board data\n" 'font-lock-face 'magit-dimmed)))
     (t
      (dolist (board shipit-atlassian-dashboard--board-data)
        (shipit-atlassian-dashboard--insert-one-board-section board))))
    (insert "\n")))

(defun shipit-atlassian-dashboard--insert-one-board-section (board)
  "Insert BOARD as a collapsible section containing its columns."
  (let* ((board-id (cdr (assq 'id board)))
         (board-name (or (cdr (assq 'name board)) (format "Board %s" board-id)))
         (cols (shipit-atlassian-dashboard--board-columns board))
         (all-issues (cl-mapcan (lambda (col)
                                  (copy-sequence (cdr (assq 'issues col))))
                                cols))
         (prepared (shipit-atlassian-dashboard--prepare-widths all-issues))
         (columns (car prepared))
         (widths (cdr prepared))
         (indent shipit-atlassian-dashboard--board-indent)
         (col-indent (concat indent "   ")))
    (magit-insert-section (atlassian-board board-id)
      (magit-insert-heading
        (propertize (format "%s%s (%d)" indent board-name
                            (shipit-atlassian-dashboard--board-issue-count board))
                    'font-lock-face 'magit-section-heading))
      (dolist (col cols)
        (shipit-atlassian-dashboard--insert-board-column-section
         board col columns widths col-indent)))))

(defun shipit-atlassian-dashboard--insert-board-column-section (board col columns widths indent)
  "Insert COL of BOARD as a column section with its issue rows.
COLUMNS and WIDTHS drive the row layout; INDENT prefixes the
column heading and each row.  Issues staged away from COL are
hidden while pending arrivals from elsewhere on BOARD render in
the pending face."
  (let* ((col-name (cdr (assq 'name col)))
         (col-issues (cdr (assq 'issues col)))
         (board-cols (shipit-atlassian-dashboard--board-columns board))
         (arrivals (shipit-atlassian-dashboard--pending-arrivals-into-column
                    board col-name))
         (visible-native
          (cl-remove-if
           (lambda (issue)
             (let* ((k (shipit-atlassian-dashboard--issue-key issue))
                    (target (cdr (assoc k shipit-atlassian-dashboard--pending-transitions))))
               (and target
                    (shipit-atlassian-dashboard--column-name-for-status
                     target board-cols))))
           col-issues))
         (visible-count (+ (length visible-native) (length arrivals))))
    (magit-insert-section (atlassian-board-column col-name)
      (magit-insert-heading
        (propertize (format "%s%s (%d)" indent col-name visible-count)
                    'font-lock-face 'magit-section-heading))
      (dolist (issue visible-native)
        (shipit-atlassian-dashboard--insert-issue-line
         issue columns widths (list :indent indent)))
      (dolist (issue arrivals)
        (shipit-atlassian-dashboard--insert-issue-line
         issue columns widths
         (list :face 'shipit-atlassian-dashboard-pending-face
               :show-pending-arrow nil :indent indent))))))

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
    (magit-insert-section (atlassian-issues)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "issues" "\U0001f50d")
                (propertize
                 (format "Issues %s%s — f:filter l:load more"
                         (shipit-atlassian-dashboard--issues-heading-suffix)
                         (shipit-atlassian-dashboard--filter-summary
                          shipit-atlassian-dashboard--issues-filter))
                 'font-lock-face 'magit-section-heading)))
      (if (null issues)
          (insert (propertize "   No issues match the current filter\n"
                              'font-lock-face 'magit-dimmed))
        (let* ((prepared (shipit-atlassian-dashboard--prepare-widths issues))
               (columns (car prepared))
               (widths (cdr prepared))
               (grouped (shipit-atlassian-dashboard--group-by-project issues)))
          (dolist (group grouped)
            (magit-insert-section (atlassian-issues-project-group (car group))
              (magit-insert-heading
                (propertize (format "   %s (%d)" (car group) (length (cdr group)))
                            'font-lock-face 'magit-section-heading))
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
    (magit-insert-section (atlassian-frequently-visited)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "clock" "\U0001f552")
                (propertize (format "Frequently Visited (%d)" (length visits))
                            'font-lock-face 'magit-section-heading)))
      (if (null visits)
          (insert (propertize "   No recently visited issues\n" 'font-lock-face 'magit-dimmed))
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
      (insert (propertize (format "Atlassian Dashboard — %s\n"
                                  shipit-atlassian-dashboard--repo)
                          'font-lock-face 'magit-header-line))
      (when shipit-atlassian-dashboard--loading-p
        (insert (propertize "Loading data from Jira...\n"
                            'font-lock-face 'magit-dimmed)))
      (insert "\n")
      ;; Sections
      (shipit-atlassian-dashboard--insert-whats-next-section)
      (shipit-atlassian-dashboard--insert-my-issues-section)
      (shipit-atlassian-dashboard--insert-issues-section)
      (shipit-atlassian-dashboard--insert-boards-section)
      (shipit-atlassian-dashboard--insert-frequently-visited-section))
    ;; Propagate hidden state to overlays — root section doesn't go
    ;; through magit-section-show, so children need explicit show/hide.
    (magit-section-show magit-root-section)
    (goto-char (min pos (point-max)))))

;;; Issues section — filter & load transients

(defun shipit-atlassian-dashboard--active-dashboard-buffer ()
  "Return the dashboard buffer that invoked the current transient, or nil.
Looks at the window that owns the minibuffer (transient reader runs
from there) before falling back to any live buffer whose local
`shipit-atlassian-dashboard--config' is set."
  (or (and (minibufferp)
           (let ((win (minibuffer-selected-window)))
             (when (window-live-p win)
               (let ((buf (window-buffer win)))
                 (and (buffer-local-value
                       'shipit-atlassian-dashboard--config buf)
                      buf)))))
      (and (bound-and-true-p shipit-atlassian-dashboard--config)
           (current-buffer))
      (cl-find-if (lambda (b)
                    (buffer-local-value
                     'shipit-atlassian-dashboard--config b))
                  (buffer-list))))

(defun shipit-atlassian-dashboard--backend-fetch (fn-key)
  "Invoke the active dashboard backend's FN-KEY with its config.
Returns whatever the backend function returns, or nil when the backend
does not implement the key.  Errors from the fetch propagate."
  (let* ((buf (shipit-atlassian-dashboard--active-dashboard-buffer))
         (config (and buf (buffer-local-value
                           'shipit-atlassian-dashboard--config buf)))
         (backend (and buf (buffer-local-value
                            'shipit-atlassian-dashboard--backend-plist buf)))
         (fetch-fn (and backend (plist-get backend fn-key))))
    (and fetch-fn config (funcall fetch-fn config))))

(defun shipit-atlassian-dashboard--fetch-user-candidates ()
  "Return completion candidates for assignee/reporter filter fields.
Prepends @me / @unassigned sentinels to the users returned by the
backend's `:fetch-assignable-users'."
  (append '("@me" "@unassigned")
          (shipit-atlassian-dashboard--backend-fetch :fetch-assignable-users)))

(defun shipit-atlassian-dashboard--read-user (prompt _initial _history)
  "Reader for assignee/reporter filter fields.
Presents a completing-read over @me, @unassigned, and the assignable
users fetched from the active dashboard's backend.  Users may still
type an unlisted name freely."
  (let ((val (completing-read
              prompt
              (shipit-atlassian-dashboard--fetch-user-candidates)
              nil nil)))
    (cond
     ((or (null val) (string-empty-p val)) nil)
     ((string= val "@me") "me")
     ((member val '("@unassigned" "none")) "unassigned")
     (t val))))

(defcustom shipit-atlassian-dashboard-date-presets
  '("-1d" "-7d" "-14d" "-30d" "-1w" "-2w" "-4w"
    "-1M" "-3M" "-6M" "-1y"
    "startOfDay()" "startOfWeek()" "startOfMonth()"
    "endOfDay()" "endOfWeek()" "endOfMonth()"
    "startOfMonth(-1)" "startOfMonth(-3)"
    "now()")
  "Presets offered by the date reader for timeline filters.
Users may type any JQL-compatible date expression: relative like
\"-7d\", absolute ISO like \"2026-01-01\", or a JQL function.
JQL itself only accepts `h m d w' as relative offset units — `M'
for months and `y' for years are normalised to days at JQL-build
time by `shipit-atlassian-dashboard--normalize-date-offset'."
  :type '(repeat string)
  :group 'shipit-atlassian)

(defun shipit-atlassian-dashboard--read-date (prompt _initial _history)
  "Reader for the timeline filter fields.
Offers JQL-friendly date presets (relative offsets, JQL start-of-period
functions) via completing-read; any free text is accepted so absolute
dates and custom JQL expressions remain typable."
  (let ((val (completing-read
              prompt
              shipit-atlassian-dashboard-date-presets
              nil nil)))
    (unless (string-empty-p val) val)))

(defun shipit-atlassian-dashboard--read-component (prompt _initial _history)
  "Reader for the component filter field.
Completes over the project's components (fetched via the backend's
`:fetch-project-components') while still allowing free text so names
the API does not return remain typable."
  (let ((val (completing-read
              prompt
              (shipit-atlassian-dashboard--backend-fetch
               :fetch-project-components)
              nil nil)))
    (unless (string-empty-p val) val)))

(defun shipit-atlassian-dashboard--read-status (prompt _initial _history)
  "Reader for the status filter field.
Completes over the project's actual status names (fetched via the
backend's `:fetch-project-statuses') while still allowing arbitrary
input so statuses that aren't returned by the API remain typable."
  (let ((val (completing-read
              prompt
              (shipit-atlassian-dashboard--backend-fetch
               :fetch-project-statuses)
              nil nil)))
    (unless (string-empty-p val) val)))

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
    (:component       "component"       "component=%s"       plain)
    (:updated-since   "updated-since"   "updated>=%s"        plain)
    (:updated-before  "updated-before"  "updated<=%s"        plain)
    (:created-since   "created-since"   "created>=%s"        plain)
    (:created-before  "created-before"  "created<=%s"        plain)
    (:sort            "sort"            "sort=%s"            plain)
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
       ((string-match "\\`--component=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :component (match-string 1 arg))))
       ((string-match "\\`--updated-since=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :updated-since (match-string 1 arg))))
       ((string-match "\\`--updated-before=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :updated-before (match-string 1 arg))))
       ((string-match "\\`--created-since=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :created-since (match-string 1 arg))))
       ((string-match "\\`--created-before=\\(.+\\)\\'" arg)
        (setq filter (plist-put filter :created-before (match-string 1 arg))))
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
   ("s" "Status" "--status="
    :reader shipit-atlassian-dashboard--read-status)
   ("p" "Priority" "--priority="
    :choices ("Highest" "High" "Medium" "Low" "Lowest"))
   ("R" "Resolution" "--resolution="
    :choices ("Unresolved" "Done" "Won't Do" "Duplicate"))
   ("m" "Component" "--component="
    :reader shipit-atlassian-dashboard--read-component)
   ("t" "Text search" "--text=" :reader read-string)
   ("c" "Comment contains" "--comment=" :reader read-string)
   ("h" "Has any comment" "--has-comments")]
  ["Timeline"
   ("u" "Updated since"  "--updated-since="
    :reader shipit-atlassian-dashboard--read-date)
   ("U" "Updated before" "--updated-before="
    :reader shipit-atlassian-dashboard--read-date)
   ("n" "Created since"  "--created-since="
    :reader shipit-atlassian-dashboard--read-date)
   ("N" "Created before" "--created-before="
    :reader shipit-atlassian-dashboard--read-date)]
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

(defun shipit-atlassian-dashboard--fetch-my-issues-async (callback)
  "Fetch my open issues asynchronously; call CALLBACK with the list."
  (let* ((config shipit-atlassian-dashboard--config)
         (jql (shipit-atlassian-dashboard--my-issues-jql config))
         (fields "key,summary,status,priority,issuetype,assignee,updated")
         (path (format "/rest/api/3/search/jql?jql=%s&maxResults=50&fields=%s"
                       (url-hexify-string jql) fields)))
    (shipit-issue-jira--api-request-async
     config path
     (lambda (raw)
       (let ((issues (append (cdr (assq 'issues raw)) nil)))
         (funcall callback
                  (mapcar #'shipit-issue-jira--normalize-issue issues)))))))

(defun shipit-atlassian-dashboard--fetch-whats-next-for-board-async (board-id callback)
  "Async variant of `shipit-atlassian-dashboard--fetch-whats-next-for-board'.
Chains two async requests: BOARD-ID's active sprint, then the
sprint's issues.  CALLBACK receives the item list, nil when the
board has no active sprint."
  (let ((config shipit-atlassian-dashboard--config))
    (shipit-issue-jira--api-request-async
     config (shipit-atlassian-dashboard--active-sprint-path board-id)
     (lambda (sprint-raw)
       (let ((active-sprint (car (append (cdr (assq 'values sprint-raw)) nil))))
         (if (null active-sprint)
             (funcall callback nil)
           (shipit-issue-jira--api-request-async
            config (shipit-atlassian-dashboard--sprint-issues-path
                    (cdr (assq 'id active-sprint)))
            (lambda (issues-raw)
              (funcall callback
                       (shipit-atlassian-dashboard--normalize-agile-issues
                        issues-raw))))))))))

(defun shipit-atlassian-dashboard--fetch-whats-next-async (callback)
  "Fetch what's-next items for every configured board asynchronously.
Runs the per-board sprint lookups concurrently and calls CALLBACK
once with the deduplicated list in configured board order, or nil
when no board is configured."
  (let ((board-ids shipit-atlassian-board-ids))
    (if (null board-ids)
        (funcall callback nil)
      (let ((results (make-vector (length board-ids) nil))
            (pending (length board-ids)))
        (cl-loop for board-id in board-ids
                 for i from 0
                 do (let ((idx i))
                      (shipit-atlassian-dashboard--fetch-whats-next-for-board-async
                       board-id
                       (lambda (items)
                         (aset results idx items)
                         (when (zerop (cl-decf pending))
                           (funcall callback
                                    (shipit-atlassian-dashboard--dedup-issues-by-key
                                     (apply #'append (append results nil)))))))))))))

(defun shipit-atlassian-dashboard--fetch-boards-async (callback)
  "Fetch every configured board asynchronously; CALLBACK gets the list.
Delegates to `shipit-atlassian-board--fetch-all-async' — a nil
callback value arrives when no board is configured."
  (shipit-atlassian-board--fetch-all-async
   shipit-atlassian-dashboard--config
   shipit-atlassian-board-ids
   callback))

(defun shipit-atlassian-dashboard--fetch-issues-page-async (token page-size callback)
  "Async version of ``--fetch-issues-page'.
Calls CALLBACK with (ISSUES NEXT-TOKEN IS-LAST-P)."
  (let* ((config shipit-atlassian-dashboard--config)
         (jql (shipit-atlassian-dashboard--build-issues-jql
               config shipit-atlassian-dashboard--issues-filter))
         (fields shipit-atlassian-dashboard--issues-fields)
         (path (format "/rest/api/3/search/jql?jql=%s&maxResults=%d&fields=%s%s"
                       (url-hexify-string jql) page-size fields
                       (if token
                           (format "&nextPageToken=%s" (url-hexify-string token))
                         ""))))
    (shipit-issue-jira--api-request-async
     config path
     (lambda (raw)
       (let ((issues (append (cdr (assq 'issues raw)) nil))
             (next-token (cdr (assq 'nextPageToken raw)))
             (is-last (shipit-issue-jira--is-last-page-p raw)))
         (funcall callback
                  (list (mapcar #'shipit-issue-jira--normalize-issue issues)
                        next-token
                        is-last)))))))

(defun shipit-atlassian-dashboard--fetch-issues-reset-async (callback)
  "Reset Issues state and fetch the first page asynchronously.
Stores the result into the dashboard buffer's locals and then calls
CALLBACK with no arguments.  The HTTP callback fires inside the
url-retrieve response buffer, so the state writes must be tunneled
back via `with-current-buffer'."
  (setq shipit-atlassian-dashboard--issues-data nil
        shipit-atlassian-dashboard--issues-next-token nil
        shipit-atlassian-dashboard--issues-last-page-p nil)
  (let ((buf (current-buffer)))
    (shipit-atlassian-dashboard--fetch-issues-page-async
     nil shipit-atlassian-issues-page-size
     (lambda (result)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (pcase-let ((`(,issues ,next ,last-p) result))
             (setq shipit-atlassian-dashboard--issues-data issues
                   shipit-atlassian-dashboard--issues-next-token next
                   shipit-atlassian-dashboard--issues-last-page-p last-p))))
       (funcall callback)))))

(defun shipit-atlassian-dashboard--run-fetches-async ()
  "Fire all four section fetches concurrently via the Jira async API.
Each callback just updates its slot; the single re-render happens
once all four have reported back — full-buffer renders with SVG icons
are expensive, so rendering after every fetch (4x) was dominating
the load time."
  (let ((pending 4)
        (buf (current-buffer)))
    (cl-labels ((finish ()
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (setq pending (1- pending))
                      (when (zerop pending)
                        (setq shipit-atlassian-dashboard--loading-p nil)
                        (shipit-atlassian-dashboard--render)
                        (message "Atlassian dashboard loaded"))))))
      (shipit-atlassian-dashboard--fetch-my-issues-async
       (lambda (data)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq shipit-atlassian-dashboard--my-issues data)))
         (finish)))
      (shipit-atlassian-dashboard--fetch-whats-next-async
       (lambda (data)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq shipit-atlassian-dashboard--whats-next data)))
         (finish)))
      (shipit-atlassian-dashboard--fetch-boards-async
       (lambda (data)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq shipit-atlassian-dashboard--board-data data)))
         (finish)))
      (shipit-atlassian-dashboard--fetch-issues-reset-async
       (lambda () (finish))))))

(defun shipit-atlassian-dashboard--run-fetches ()
  "Synchronously run the four dashboard fetches and re-render.
Tests and sync callers invoke this directly; `--refresh-data' defers
to it via `run-at-time' so the initial placeholder render paints first."
  (condition-case err
      (progn
        (setq shipit-atlassian-dashboard--my-issues
              (shipit-atlassian-dashboard--fetch-my-issues))
        (setq shipit-atlassian-dashboard--whats-next
              (shipit-atlassian-dashboard--fetch-whats-next))
        (setq shipit-atlassian-dashboard--board-data
              (shipit-atlassian-dashboard--fetch-boards))
        (shipit-atlassian-dashboard--fetch-issues-reset))
    (error
     (shipit--debug-log "Dashboard fetch error: %s" err)))
  (setq shipit-atlassian-dashboard--loading-p nil)
  (shipit-atlassian-dashboard--render)
  (message "Atlassian dashboard loaded"))

(defcustom shipit-atlassian-dashboard-defer-fetches t
  "When non-nil, dashboard fetches run asynchronously via `run-at-time'.
Set to nil in tests so `shipit-atlassian-dashboard--refresh-data' behaves
synchronously."
  :type 'boolean
  :group 'shipit-atlassian)

(defun shipit-atlassian-dashboard--refresh-data ()
  "Render the dashboard skeleton immediately, then fetch data in background.
The buffer becomes visible right away with a `Loading...' banner; the
four Jira fetches run deferred so the placeholder render paints first.
When all fetches complete the buffer re-renders with real content and
the banner disappears."
  (message "Loading Atlassian dashboard...")
  (setq shipit-atlassian-dashboard--my-issues nil
        shipit-atlassian-dashboard--whats-next nil
        shipit-atlassian-dashboard--board-data nil
        shipit-atlassian-dashboard--issues-data nil
        shipit-atlassian-dashboard--issues-next-token nil
        shipit-atlassian-dashboard--issues-last-page-p nil
        shipit-atlassian-dashboard--loading-p t)
  (shipit-atlassian-dashboard--render)
  (if shipit-atlassian-dashboard-defer-fetches
      (shipit-atlassian-dashboard--run-fetches-async)
    (shipit-atlassian-dashboard--run-fetches)))

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

(defun shipit-atlassian-dashboard--parse-board-ids-input (input)
  "Parse INPUT as comma/space-separated board IDs or board URLs.
Returns the list of numeric board IDs in input order; tokens that
parse to nothing are dropped."
  (delq nil (mapcar #'shipit-atlassian-dashboard--parse-board-input
                    (split-string input "[,[:space:]]+" t))))

(defun shipit-atlassian-dashboard--configure-board-ids ()
  "Prompt for Jira board IDs or URLs and set them for the current session."
  (interactive)
  (let* ((current (mapconcat #'number-to-string shipit-atlassian-board-ids ","))
         (prompt (if shipit-atlassian-board-ids
                     (format "Board IDs or URLs (current: %s): " current)
                   "Board IDs or URLs (comma-separated): "))
         (input (read-string prompt))
         (ids (shipit-atlassian-dashboard--parse-board-ids-input input)))
    (unless ids
      (user-error "Could not parse any board ID from: %s" input))
    (setq shipit-atlassian-board-ids ids)
    (message "Board IDs set to %s (use M-x customize-variable RET shipit-atlassian-board-ids to persist)"
             (mapconcat #'number-to-string ids ","))
    (shipit-atlassian-dashboard--refresh-data)))

(defun shipit-atlassian-dashboard--board-id-at-point ()
  "Return the board ID of the `atlassian-board\=' section at point, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'atlassian-board)))
      (setq section (oref section parent)))
    (when section (oref section value))))

(defun shipit-atlassian-dashboard--read-configured-board-id ()
  "Pick one configured board ID, preferring point context over prompting.
With a single configured board it is returned directly; otherwise
the user picks among the configured boards, labelled with their
fetched names when board data is available."
  (or (shipit-atlassian-dashboard--board-id-at-point)
      (when (null (cdr shipit-atlassian-board-ids))
        (car shipit-atlassian-board-ids))
      (let* ((candidates
              (mapcar (lambda (board-id)
                        (let ((board (shipit-atlassian-dashboard--board-by-id board-id)))
                          (cons (if board
                                    (format "%s (%d)" (cdr (assq 'name board)) board-id)
                                  (number-to-string board-id))
                                board-id)))
                      shipit-atlassian-board-ids))
             (choice (completing-read "Open board: " candidates nil t)))
        (cdr (assoc choice candidates)))))

(defun shipit-atlassian-dashboard--open-board-in-browser ()
  "Open a configured board in the browser.
Prefers the board section at point; prompts when several boards
are configured and point gives no answer."
  (interactive)
  (unless shipit-atlassian-board-ids
    (user-error "No board IDs configured"))
  (let ((base-url (string-trim-right
                   (plist-get shipit-atlassian-dashboard--config :base-url) "/"))
        (board-id (shipit-atlassian-dashboard--read-configured-board-id)))
    (browse-url (format "%s/jira/software/projects/board/%d"
                        base-url board-id))))

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
  ["Boards"
   ("b" "Set board IDs" shipit-atlassian-dashboard--configure-board-ids)
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
     ((eq configure 'board-ids)
      (shipit-atlassian-dashboard--configure-board-ids))
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
