;;; shipit-atlassian-board.el --- Jira Agile board support -*- lexical-binding: t; -*-

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
;; Fetches Jira Agile board data, one or several boards at a time.
;; Each board resolves to ((id . ID) (name . NAME) (columns . COLS))
;; where issues are grouped into board columns by matching issue
;; status against each column's configured status list.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'shipit-issue-backends)
(require 'shipit-issue-jira)

;;; Customization
;;
;; The group and the board list live here, at the layer that fetches a
;; board, because both the dashboard and the canvas kanban read them
;; and neither may require the other.

(defgroup shipit-atlassian nil
  "Atlassian and Jira settings."
  :group 'shipit
  :prefix "shipit-atlassian-")

(defcustom shipit-atlassian-board-ids nil
  "List of Jira board IDs to show.
Each board renders as its own section in the dashboard, and
`shipit-atlassian-kanban' draws one of them on a canvas.  When nil,
the dashboard shows a configuration hint and What's Next has no
sprint source."
  :type '(repeat (integer :tag "Board ID"))
  :group 'shipit-atlassian)

(make-obsolete-variable 'shipit-atlassian-board-id 'shipit-atlassian-board-ids "1.7")

(defun shipit-atlassian-board--config-path (board-id)
  "Build path for board configuration of BOARD-ID."
  (format "/rest/agile/1.0/board/%d/configuration" board-id))

(defun shipit-atlassian-board--issues-path (board-id start-at)
  "Build path for board issues of BOARD-ID starting at START-AT.
Uses the Jira-agile max page size (100) to cut round trips in half."
  (format "/rest/agile/1.0/board/%d/issue?maxResults=100&startAt=%d&fields=key,summary,status,priority,issuetype,assignee"
          board-id start-at))

(defun shipit-atlassian-board--statuses-path ()
  "Path for the instance-wide list of Jira statuses (id + name)."
  "/rest/api/2/status")

(defun shipit-atlassian-board--statuses-to-name-map (raw)
  "Convert RAW (a vector / list of `{id, name, ...}' status alists) to id->name.
Tolerates RAW being nil or an empty vector so a 401 / network
fallback simply leaves the map empty -- the issue-derived map
still covers every column that has at least one visible issue,
which is the previous behaviour."
  (let (acc)
    (dolist (s (append (or raw nil) nil))
      (let ((id (cdr (assq 'id s)))
            (name (cdr (assq 'name s))))
        (when (and id name)
          (push (cons id name) acc))))
    acc))

(defun shipit-atlassian-board--fetch-status-names (config)
  "Fetch the Jira instance status catalogue, return id->name alist.
Synchronous variant used by `shipit-atlassian-board--fetch'."
  (shipit-atlassian-board--statuses-to-name-map
   (shipit-issue-jira--api-request
    config (shipit-atlassian-board--statuses-path))))

(defun shipit-atlassian-board--fetch-status-names-async (config callback)
  "Async variant of `--fetch-status-names'.
CALLBACK is invoked with the id->name alist (possibly empty if
the request fails)."
  (shipit-issue-jira--api-request-async
   config (shipit-atlassian-board--statuses-path)
   (lambda (raw)
     (funcall callback
              (shipit-atlassian-board--statuses-to-name-map raw)))))

(defun shipit-atlassian-board--fetch-all-issues (config board-id)
  "Fetch all board issues for BOARD-ID using CONFIG with pagination."
  (let ((all-issues nil)
        (start-at 0)
        (done nil))
    (while (not done)
      (let* ((raw (shipit-issue-jira--api-request
                   config (shipit-atlassian-board--issues-path board-id start-at)))
             (page-issues (append (cdr (assq 'issues raw)) nil))
             (total (cdr (assq 'total raw))))
        (setq all-issues (append all-issues page-issues))
        (setq start-at (+ start-at (length page-issues)))
        (when (or (null page-issues) (>= start-at (or total 0)))
          (setq done t))))
    (shipit--debug-log "Board: fetched %d total issues" (length all-issues))
    all-issues))

(defun shipit-atlassian-board--fetch-all-issues-async (config board-id callback &optional acc start-at)
  "Paginated async fetch of all board issues.
Accumulates issues in ACC starting at START-AT and chains to the next
page until Jira reports no more results; then calls CALLBACK with the
complete list of raw issues."
  (let ((acc (or acc nil))
        (start-at (or start-at 0)))
    (shipit-issue-jira--api-request-async
     config (shipit-atlassian-board--issues-path board-id start-at)
     (lambda (raw)
       (let* ((page-issues (append (cdr (assq 'issues raw)) nil))
              (total (cdr (assq 'total raw)))
              (all-issues (append acc page-issues))
              (new-start (+ start-at (length page-issues))))
         (if (or (null page-issues) (>= new-start (or total 0)))
             (funcall callback all-issues)
           (shipit-atlassian-board--fetch-all-issues-async
            config board-id callback all-issues new-start)))))))

(defun shipit-atlassian-board--make-board (board-id board-config columns)
  "Assemble the board alist for BOARD-ID from BOARD-CONFIG and grouped COLUMNS.
BOARD-CONFIG is the raw board-configuration API response; its `name'
field labels the board in the dashboard, falling back to
\"Board <id>\" when the response carries none."
  `((id . ,board-id)
    (name . ,(or (cdr (assq 'name board-config))
                 (format "Board %d" board-id)))
    (columns . ,columns)))

(defun shipit-atlassian-board--config-columns (board-config)
  "Extract the raw column list from BOARD-CONFIG."
  (append (cdr (assq 'columns (cdr (assq 'columnConfig board-config)))) nil))

(defun shipit-atlassian-board--fetch-one-async (config board-id status-name-map callback)
  "Fetch one board's configuration and issues asynchronously.
STATUS-NAME-MAP is the shared instance status catalogue (see
`shipit-atlassian-board--fetch-all-async').  CALLBACK receives the
assembled board alist ((id ...) (name ...) (columns ...))."
  (shipit-issue-jira--api-request-async
   config (shipit-atlassian-board--config-path board-id)
   (lambda (board-config)
     (let ((columns (shipit-atlassian-board--config-columns board-config)))
       (shipit-atlassian-board--fetch-all-issues-async
        config board-id
        (lambda (raw-issues)
          (let ((issues (mapcar #'shipit-issue-jira--normalize-issue raw-issues)))
            (shipit--debug-log "Board %d async: %d columns, %d issues"
                               board-id (length columns) (length issues))
            (funcall callback
                     (shipit-atlassian-board--make-board
                      board-id board-config
                      (shipit-atlassian-board--group-by-column
                       columns issues status-name-map))))))))))

(defun shipit-atlassian-board--each-async (items fn callback)
  "Run FN over every one of ITEMS at once; CALLBACK gets the results in order.
FN is called with an item and a function to pass that item\='s result to.
CALLBACK runs once every item has reported back, and at once with nil
when ITEMS is empty."
  (if (null items)
      (funcall callback nil)
    (let ((results (make-vector (length items) nil))
          (pending (length items)))
      (cl-loop for item in items
               for i from 0
               do (let ((idx i))
                    (funcall fn item
                             (lambda (result)
                               (aset results idx result)
                               (when (zerop (cl-decf pending))
                                 (funcall callback (append results nil))))))))))

(defun shipit-atlassian-board--fetch-names-async (config board-ids callback)
  "Fetch the name of each board in BOARD-IDS; CALLBACK gets an id-to-name alist.
Reads each board\='s configuration and none of its issues, which is all
that naming a board in a prompt needs."
  (shipit-atlassian-board--each-async
   board-ids
   (lambda (board-id report)
     (shipit-issue-jira--api-request-async
      config (shipit-atlassian-board--config-path board-id)
      (lambda (board-config)
        (funcall report
                 (cons board-id (or (cdr (assq 'name board-config))
                                    (format "Board %d" board-id)))))))
   callback))

(defun shipit-atlassian-board--fetch-all-async (config board-ids callback)
  "Fetch every board in BOARD-IDS concurrently; CALLBACK gets the list.
The instance status catalogue is fetched once and shared across
boards -- it lets columns that have no visible issue (e.g. an empty
`Review & Test') still expose a status name, which the dashboard's
edit mode needs to stage forward / backward moves.  CALLBACK
receives the board alists in BOARD-IDS order once every board has
reported back; it receives nil when BOARD-IDS is empty."
  (if (null board-ids)
      (funcall callback nil)
    (shipit-atlassian-board--fetch-status-names-async
     config
     (lambda (status-name-map)
       (shipit-atlassian-board--each-async
        board-ids
        (lambda (board-id report)
          (shipit-atlassian-board--fetch-one-async
           config board-id status-name-map report))
        callback)))))

(defun shipit-atlassian-board--fetch-one (config board-id status-name-map)
  "Fetch configuration and issues for BOARD-ID synchronously.
STATUS-NAME-MAP is the shared instance status catalogue.  Returns
the board alist; see `shipit-atlassian-board--make-board'."
  (let* ((board-config (shipit-issue-jira--api-request
                        config (shipit-atlassian-board--config-path board-id)))
         (columns (shipit-atlassian-board--config-columns board-config))
         (raw-issues (shipit-atlassian-board--fetch-all-issues config board-id))
         (issues (mapcar #'shipit-issue-jira--normalize-issue raw-issues)))
    (shipit--debug-log "Board %d: %d columns, %d issues"
                       board-id (length columns) (length issues))
    (shipit-atlassian-board--make-board
     board-id board-config
     (shipit-atlassian-board--group-by-column columns issues status-name-map))))

(defun shipit-atlassian-board--fetch-all (config board-ids)
  "Fetch every board in BOARD-IDS synchronously, in order.
Pulls the instance status catalogue once and shares it across
boards so empty columns still resolve their status IDs to names --
otherwise advancing into a zero-issue column fails with `No forward
column' even though the column exists in the board configuration.
Returns nil when BOARD-IDS is empty."
  (when board-ids
    (let ((status-name-map (shipit-atlassian-board--fetch-status-names config)))
      (mapcar (lambda (board-id)
                (shipit-atlassian-board--fetch-one config board-id status-name-map))
              board-ids))))

(defun shipit-atlassian-board--build-status-name-map (issues)
  "Build an alist of status-id -> status-name from ISSUES.
The Jira board-configuration API returns status objects as
`((id . ID) (self . URL))' -- no `name' field.  We get the
names from the issues themselves (which carry both `state-id'
and `state' after normalisation) so consumers can route by
name without an extra API call."
  (let (acc)
    (dolist (issue issues)
      (let ((id (cdr (assq 'state-id issue)))
            (name (cdr (assq 'state issue))))
        (when (and id name (not (assoc id acc)))
          (push (cons id name) acc))))
    acc))

(defun shipit-atlassian-board--group-by-column (columns issues &optional extra-status-names)
  "Group ISSUES into COLUMNS based on status ID matching.
COLUMNS is from the board configuration API.
Returns list of ((name . col-name) (issues . filtered-issues)
                  (statuses . list-of-status-names)).

The board-configuration API\='s status objects expose only
`{id, self}', so status names are resolved from two sources:

1. The fetched ISSUES (each carries `state' + `state-id' after
   normalisation).
2. EXTRA-STATUS-NAMES -- an optional id->name alist supplied by
   the caller, typically via
   `shipit-atlassian-board--fetch-status-names'.  This covers
   columns whose statuses don\='t appear on any visible issue
   (e.g. an empty `Review & Test' column), so the column still
   advertises a usable target name for forward / backward moves.

If neither source resolves a column\='s status IDs, that column
ends up with an empty `statuses' list -- it still renders, but
edit-mode advancement skips past it."
  (let* ((status-names-by-id
          (shipit-atlassian-board--build-status-name-map issues))
         ;; Issue-derived map wins: `state' / `state-id' from the
         ;; live issue is the canonical name as displayed.  Extra
         ;; map fills the gaps.
         (merged (append status-names-by-id
                         (cl-remove-if
                          (lambda (entry) (assoc (car entry) status-names-by-id))
                          (or extra-status-names nil))))
         (result nil))
    (dolist (col columns)
      (let* ((col-name (cdr (assq 'name col)))
             (col-statuses (append (cdr (assq 'statuses col)) nil))
             (status-ids (mapcar (lambda (s) (cdr (assq 'id s))) col-statuses))
             (status-names (delq nil
                                 (mapcar (lambda (id)
                                           (cdr (assoc id merged)))
                                         status-ids)))
             (col-issues (seq-filter
                          (lambda (issue)
                            (member (cdr (assq 'state-id issue)) status-ids))
                          issues)))
        (push `((name . ,col-name)
                (issues . ,col-issues)
                (statuses . ,status-names))
              result)))
    (nreverse result)))

;;; Reading a board's columns
;;
;; Every function below takes the boards it works on as an argument.
;; They used to read the dashboard's buffer-local board data, which
;; kept them in a UI file that the canvas view cannot require.

(defun shipit-atlassian-board--columns (board)
  "Return the columns list of BOARD, a board alist."
  (cdr (assq 'columns board)))

(defun shipit-atlassian-board--by-id (boards board-id)
  "Return the board in BOARDS whose id is BOARD-ID, or nil."
  (cl-find board-id boards :key (lambda (board) (cdr (assq 'id board)))))

(defun shipit-atlassian-board--all-columns (boards)
  "Concatenated columns of every board in BOARDS, in configured order."
  (cl-mapcan (lambda (board)
               (copy-sequence (shipit-atlassian-board--columns board)))
             boards))

(defun shipit-atlassian-board--column-for-status (columns status)
  "Return the column in COLUMNS accepting STATUS, or nil.
Matches case-insensitively against each column\='s `statuses\=' list."
  (when status
    (cl-find-if
     (lambda (col)
       (cl-some (lambda (s)
                  (and s (string-equal-ignore-case s status)))
                (cdr (assq 'statuses col))))
     columns)))

(defun shipit-atlassian-board--columns-accepting (boards status)
  "Return the columns of the first board in BOARDS accepting STATUS, or nil."
  (cl-loop for board in boards
           for cols = (shipit-atlassian-board--columns board)
           when (shipit-atlassian-board--column-for-status cols status)
           return cols))

(defun shipit-atlassian-board--column-index (status columns)
  "Return STATUS\='s column index within COLUMNS, or nil."
  (when-let* ((col (shipit-atlassian-board--column-for-status columns status)))
    (cl-position col columns :test #'eq)))

(defun shipit-atlassian-board--column-name-for-status (status-name columns)
  "Return the name of the column in COLUMNS accepting STATUS-NAME, or nil.
The board-configuration API links a status to a column through the
column\='s `statuses\=' list, so a canonical target like \"In Review\"
routes to whichever column the user configured to accept it."
  (cdr (assq 'name (shipit-atlassian-board--column-for-status
                    columns status-name))))

(defun shipit-atlassian-board--step-column (columns index direction)
  "Return the column one step DIRECTION from INDEX in COLUMNS, or nil.
Holds at both ends, so a step never leaves the board it started on."
  (let ((step (pcase direction
                ('forward (1+ index))
                ('backward (1- index)))))
    (and (integerp step)
         (>= step 0)
         (< step (length columns))
         (nth step columns))))

(defun shipit-atlassian-board--target-status (boards current direction &optional board-id)
  "Return a status name one column DIRECTION from CURRENT, or nil.
Steps within a single board\='s column order: BOARD-ID\='s board when
given, otherwise the first board in BOARDS with a column accepting
CURRENT.  Returns the first status name listed for the destination
column.  Stepping past either end returns nil, so a step never
crosses a board boundary."
  (when current
    (let* ((cols (if board-id
                     (shipit-atlassian-board--columns
                      (shipit-atlassian-board--by-id boards board-id))
                   (shipit-atlassian-board--columns-accepting boards current)))
           (idx (shipit-atlassian-board--column-index current cols))
           (next (and idx (shipit-atlassian-board--step-column cols idx direction)))
           (target (car (cdr (assq 'statuses next)))))
      (shipit--debug-log
       "board: target-status current=%S board=%S idx=%S next=%S target=%S"
       current board-id idx (cdr (assq 'name next)) target)
      target)))

;;; Finding an issue on a board

(defun shipit-atlassian-board--issue-key (issue)
  "Return the work-item key for ISSUE.
Normalizes first, so this answers for the agile shape (`key\=') and
for the mapped work-item shape (`id\=') alike."
  (cdr (assq 'key (shipit-issue--normalize-to-work-item issue))))

(defun shipit-atlassian-board--issue-in-columns (columns key)
  "Return the issue alist with KEY somewhere in COLUMNS, or nil.
Case-insensitive on the key."
  (cl-loop for col in columns
           thereis (cl-find-if
                    (lambda (issue)
                      (let ((k (shipit-atlassian-board--issue-key issue)))
                        (and k (string-equal-ignore-case k key))))
                    (cdr (assq 'issues col)))))

(defun shipit-atlassian-board--issue-in-boards (boards key)
  "Return the issue alist with KEY on any board in BOARDS, or nil."
  (cl-loop for board in boards
           thereis (shipit-atlassian-board--issue-in-columns
                    (shipit-atlassian-board--columns board) key)))

;;; Staging a move between columns

(defun shipit-atlassian-board--stage-move (pending key actual-status target)
  "Return PENDING with a move of KEY from ACTUAL-STATUS to TARGET staged.
PENDING is an alist of issue key to target status name, and a new
alist comes back rather than PENDING being changed.  A TARGET equal
to ACTUAL-STATUS drops the entry, because the staged moves cancelled
out.  Otherwise the entry is created or replaced, so a second step
bumps the staged target one column further."
  (let ((without (assoc-delete-all key (copy-sequence pending))))
    (if (and actual-status (string-equal-ignore-case target actual-status))
        (progn
          (shipit--debug-log "board: net-zero, dropped %S" key)
          without)
      (shipit--debug-log "board: staged %S -> %S" key target)
      (cons (cons key target) without))))

;;; Walking Jira transitions

(defun shipit-atlassian-board--match-transition (transitions target-name)
  "Return the transition in TRANSITIONS landing on TARGET-NAME, or nil.
Each element is an alist; the match is on the nested `to.name\=',
case-insensitively."
  (when target-name
    (seq-find (lambda (tr)
                (let ((to-name (cdr (assq 'name (cdr (assq 'to tr))))))
                  (and to-name (string-equal-ignore-case to-name target-name))))
              transitions)))

(defun shipit-atlassian-board--nearest-transition (transitions columns target-idx)
  "Return the transition in TRANSITIONS landing nearest TARGET-IDX, or nil.
Distance is counted in COLUMNS, the column order of the board holding
the target."
  (let (best best-idx)
    (dolist (tr transitions)
      (let* ((to-name (cdr (assq 'name (cdr (assq 'to tr)))))
             (idx (shipit-atlassian-board--column-index to-name columns)))
        (when (and idx
                   (or (null best-idx)
                       (< (abs (- idx target-idx)) (abs (- best-idx target-idx)))))
          (setq best tr best-idx idx))))
    best))

(defun shipit-atlassian-board--pick-step-transition (boards transitions target)
  "Choose the transition in TRANSITIONS that moves toward TARGET.
Prefers the one landing on TARGET itself.  Without a direct path it
takes the one landing nearest TARGET in the column order of the board
in BOARDS that holds it.  A staged multi-column move relies on this,
because a Jira workflow usually offers one column at a time."
  (let* ((cols (shipit-atlassian-board--columns-accepting boards target))
         (target-idx (and cols (shipit-atlassian-board--column-index target cols))))
    (or (shipit-atlassian-board--match-transition transitions target)
        (and target-idx
             (shipit-atlassian-board--nearest-transition transitions cols target-idx)))))

(defun shipit-atlassian-board--transitions-for (repo key)
  "Return the transitions Jira offers for KEY in REPO."
  (let ((resolved (shipit-issue--resolve-for-repo repo)))
    (shipit-issue--get-transitions (car resolved) (cdr resolved) key)))

(defun shipit-atlassian-board--fire-transition (repo key transition-id)
  "Apply TRANSITION-ID to KEY in REPO through the issue backend."
  (let ((resolved (shipit-issue--resolve-for-repo repo)))
    (shipit-issue--transition-status (car resolved) (cdr resolved) key transition-id)))

(defun shipit-atlassian-board--commit-step (boards repo key target last-fired)
  "Fire one transition for KEY in REPO toward TARGET; return where it landed.
LAST-FIRED is the destination of the previous step, so a workflow that
offers the same move again raises instead of looping.  Raises when no
transition leads toward TARGET at all."
  (let* ((transitions (shipit-atlassian-board--transitions-for repo key))
         (tr (shipit-atlassian-board--pick-step-transition boards transitions target))
         (to-name (and tr (cdr (assq 'name (cdr (assq 'to tr)))))))
    (unless tr
      (error "No transition toward %S available" target))
    (when (and last-fired to-name (string-equal-ignore-case to-name last-fired))
      (error "Transition loop detected at %S" last-fired))
    (shipit--debug-log "board: commit %s fires %S to %S"
                       key (cdr (assq 'id tr)) to-name)
    (shipit-atlassian-board--fire-transition repo key (cdr (assq 'id tr)))
    to-name))

(defconst shipit-atlassian-board--commit-step-limit 8
  "How many transitions one staged move may fire before it gives up.
A safety net against a workflow that would otherwise walk forever.")

(defun shipit-atlassian-board--commit-one (boards repo key target)
  "Walk transitions for KEY in REPO until it reaches TARGET.
BOARDS gives the column order the walk steps through.  Returns t on
success and raises otherwise, so the caller can leave a failed move
staged."
  (let ((steps 0)
        (last-fired nil))
    (catch 'done
      (while t
        (when (>= steps shipit-atlassian-board--commit-step-limit)
          (error "Gave up after %d steps" shipit-atlassian-board--commit-step-limit))
        (let ((to-name (shipit-atlassian-board--commit-step
                        boards repo key target last-fired)))
          (cl-incf steps)
          (setq last-fired to-name)
          (when (and to-name target (string-equal-ignore-case to-name target))
            (throw 'done t)))))))

(defun shipit-atlassian-board--commit-pending (boards repo pending)
  "Apply every staged move in PENDING to Jira; return what happened.
BOARDS gives the column order each walk steps through.  Returns
\(COMMITTED REMAINING FAILURES): how many moves landed, the entries
still staged, and a line per failure.  A failed move stays in
REMAINING, so the caller can leave it staged to retry or discard."
  (let ((committed 0)
        (remaining pending)
        (failures nil))
    (pcase-dolist (`(,key . ,target) pending)
      (condition-case err
          (progn
            (shipit-atlassian-board--commit-one boards repo key target)
            (cl-incf committed)
            (setq remaining (assoc-delete-all key (copy-sequence remaining))))
        (error
         (push (format "%s -> %s: %s" key target (error-message-string err))
               failures))))
    (list committed remaining (nreverse failures))))

(provide 'shipit-atlassian-board)
;;; shipit-atlassian-board.el ends here
