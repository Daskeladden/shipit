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
(require 'shipit-issue-jira)

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
       (let ((results (make-vector (length board-ids) nil))
             (pending (length board-ids)))
         (cl-loop for board-id in board-ids
                  for i from 0
                  do (let ((idx i))
                       (shipit-atlassian-board--fetch-one-async
                        config board-id status-name-map
                        (lambda (board)
                          (aset results idx board)
                          (when (zerop (cl-decf pending))
                            (funcall callback (append results nil))))))))))))

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

(provide 'shipit-atlassian-board)
;;; shipit-atlassian-board.el ends here
