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
;; Fetches and renders Jira Agile board data as columns.
;; Groups issues into board columns by matching issue status
;; against each column's configured status list.

;;; Code:

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

(defun shipit-atlassian-board--fetch-async (config board-id callback)
  "Async version of `shipit-atlassian-board--fetch'.
Fires the board-config request, then paginated issues, and finally
calls CALLBACK with the grouped columns list."
  (shipit-issue-jira--api-request-async
   config (shipit-atlassian-board--config-path board-id)
   (lambda (board-config)
     (let ((columns (append (cdr (assq 'columns
                                       (cdr (assq 'columnConfig board-config)))) nil)))
       (shipit-atlassian-board--fetch-all-issues-async
        config board-id
        (lambda (raw-issues)
          (let ((issues (mapcar #'shipit-issue-jira--normalize-issue raw-issues)))
            (shipit--debug-log "Board async: %d columns, %d issues"
                               (length columns) (length issues))
            (funcall callback
                     (shipit-atlassian-board--group-by-column columns issues)))))))))

(defun shipit-atlassian-board--fetch (config board-id)
  "Fetch board configuration and issues for BOARD-ID using CONFIG.
Returns a list of column alists: ((name . \"col\") (issues . (...)))."
  (let* ((board-config (shipit-issue-jira--api-request
                        config (shipit-atlassian-board--config-path board-id)))
         (columns (append (cdr (assq 'columns
                                     (cdr (assq 'columnConfig board-config)))) nil))
         (raw-issues (shipit-atlassian-board--fetch-all-issues config board-id))
         (issues (mapcar #'shipit-issue-jira--normalize-issue raw-issues)))
    (shipit--debug-log "Board: %d columns, %d issues" (length columns) (length issues))
    (shipit-atlassian-board--group-by-column columns issues)))

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

(defun shipit-atlassian-board--group-by-column (columns issues)
  "Group ISSUES into COLUMNS based on status ID matching.
COLUMNS is from the board configuration API.
Returns list of ((name . col-name) (issues . filtered-issues)
                  (statuses . list-of-status-names)).

The board-configuration API\='s status objects expose only
`{id, self}', so status names are resolved from the issues
\(which carry `state' / `state-id' from the Jira issue
normaliser).  Columns whose statuses don\='t appear on any
fetched issue end up with an empty `statuses' list -- they
still render, they just can\='t be routing targets until an
issue with that status is in view."
  (let ((status-names-by-id
         (shipit-atlassian-board--build-status-name-map issues))
        (result nil))
    (dolist (col columns)
      (let* ((col-name (cdr (assq 'name col)))
             (col-statuses (append (cdr (assq 'statuses col)) nil))
             (status-ids (mapcar (lambda (s) (cdr (assq 'id s))) col-statuses))
             (status-names (delq nil
                                 (mapcar (lambda (id)
                                           (cdr (assoc id status-names-by-id)))
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
