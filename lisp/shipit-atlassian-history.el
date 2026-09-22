;;; shipit-atlassian-history.el --- How a Jira board changed over time -*- lexical-binding: t; -*-

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
;; Jira keeps a changelog on every issue.  This file reads the status
;; changes out of it and works out a board as it stood at an earlier
;; time: each card goes back to the column of the status it had then,
;; and a card made since then sits where it is now, marked as made
;; since, so every change in the time shown is on the board.
;;
;; A history is a plist: `:created' is when the issue was made and
;; `:changes' its status changes, oldest first.  A change is a plist of
;; `:time', `:from', `:to' and `:by', the display name of who made it.
;; Times are seconds since the epoch, as `float-time' gives them.
;;
;; The status at a time is worked out back from the status now, by
;; undoing each change made after that time.  Jira sends at most the
;; last 100 changelog entries of an issue with a board page, so a time
;; before the oldest of those gives the status of that oldest entry.
;;
;; A board's filter can count from now, as a filter keeping resolved
;; issues for a week does with `resolutiondate >= -7d'.  An issue it
;; matched then may not match now, and the board no longer lists it.
;; Such issues are read with a search for the filter as it matched
;; then: each relative date in a comparison counts from then instead.
;; The issues found carry the issue itself in `:issue', so the board of
;; then can draw them.  A relative date inside a function, such as
;; `startOfDay(-1d)', still counts from now, and a status in the filter
;; is the status now.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'shipit-atlassian-board)
(require 'shipit-issue-jira)

;;; Reading the changelog

(defun shipit-atlassian-history--time (iso)
  "Return ISO, a time as Jira writes it, in seconds since the epoch."
  (float-time (date-to-time iso)))

(defun shipit-atlassian-history--status-change (entry)
  "Return the status change of ENTRY, a normalized changelog entry, or nil."
  (when-let* ((item (seq-find (lambda (item) (equal "status" (cdr (assq 'field item))))
                              (cdr (assq 'items entry)))))
    (list :time (shipit-atlassian-history--time (cdr (assq 'created_at entry)))
          :from (cdr (assq 'from item))
          :to (cdr (assq 'to item))
          :by (cdr (assq 'login (cdr (assq 'user entry)))))))

(defun shipit-atlassian-history--status-changes (raw-issue)
  "Return the status changes of RAW-ISSUE, a board issue as Jira sends it.
Oldest first, whatever order Jira lists them in."
  (sort (delq nil (mapcar #'shipit-atlassian-history--status-change
                          (shipit-issue-jira--normalize-changelog
                           (cdr (assq 'changelog raw-issue)))))
        (lambda (a b) (< (plist-get a :time) (plist-get b :time)))))

(defun shipit-atlassian-history--of-issue (raw-issue)
  "Return (KEY . HISTORY) for RAW-ISSUE, a board issue with its changelog."
  (let ((created (cdr (assq 'created (cdr (assq 'fields raw-issue))))))
    (cons (cdr (assq 'key raw-issue))
          (list :created (and created (shipit-atlassian-history--time created))
                :changes (shipit-atlassian-history--status-changes raw-issue)))))

;;; The board at a time

(defun shipit-atlassian-history--moves-since (history time)
  "Return the changes in HISTORY made after TIME, oldest first."
  (seq-filter (lambda (change) (> (plist-get change :time) time))
              (plist-get history :changes)))

(defun shipit-atlassian-history--created-after-p (history time)
  "Return non-nil when HISTORY says its issue was made after TIME."
  (let ((created (plist-get history :created)))
    (and created (> created time))))

(defun shipit-atlassian-history--status-at (status history time)
  "Return the status an issue in STATUS now had at TIME, by its HISTORY.
Nil when the issue did not exist yet.  An issue with no HISTORY keeps
STATUS: it is one the history does not know, added since it was read."
  (unless (shipit-atlassian-history--created-after-p history time)
    (let ((moves (shipit-atlassian-history--moves-since history time)))
      (if moves (plist-get (car moves) :from) status))))

(defun shipit-atlassian-history--with (alist key value)
  "Return a copy of ALIST with KEY set to VALUE; ALIST is not changed."
  (cons (cons key value) (assq-delete-all key (copy-alist alist))))

(defun shipit-atlassian-history--of (issue histories)
  "Return the history of ISSUE in HISTORIES, or nil when it has none."
  (cdr (assoc (shipit-atlassian-board--issue-key issue) histories)))

(defun shipit-atlassian-history--with-moves (issue history time other)
  "Return ISSUE with what its HISTORY says changed since TIME.
The moves since go in `timeline-moves', and OTHER, the status the issue
has on the other side of TIME, in `timeline-other', so a drawing can
show where the card went.  An issue made after TIME carries when it was
made in `timeline-new'.  ISSUE comes back as it is when nothing changed."
  (let ((moves (shipit-atlassian-history--moves-since history time))
        (made (and (shipit-atlassian-history--created-after-p history time)
                   (plist-get history :created))))
    (append (when moves `((timeline-moves . ,moves) (timeline-other . ,other)))
            (when made `((timeline-new . ,made)))
            issue)))

(defun shipit-atlassian-history--issue-at (issue history time)
  "Return ISSUE as it was at TIME by its HISTORY.
The copy has the status of then in `state' and, when the issue moved
since, those moves in `timeline-moves' and its status now in
`timeline-other'.  An issue made after TIME had no status then, so it
keeps its status now and shows where it is now, carrying when it was
made in `timeline-new'.  ISSUE itself is not changed."
  (let* ((now (cdr (assq 'state issue)))
         (status (or (shipit-atlassian-history--status-at now history time) now)))
    (shipit-atlassian-history--with-moves
     (shipit-atlassian-history--with issue 'state status) history time now)))

(defun shipit-atlassian-history--board-issues (board)
  "Return every issue of BOARD, in reading order."
  (cl-mapcan (lambda (column) (append (cdr (assq 'issues column)) nil))
             (shipit-atlassian-board--columns board)))

(defun shipit-atlassian-history--left-issues (board histories)
  "Return the issues HISTORIES carries that BOARD no longer has."
  (let ((on-board (mapcar #'shipit-atlassian-board--issue-key
                          (shipit-atlassian-history--board-issues board))))
    (delq nil (mapcar (lambda (entry)
                        (and (not (member (car entry) on-board))
                             (plist-get (cdr entry) :issue)))
                      histories))))

(defun shipit-atlassian-history--issues-at (board histories time)
  "Return every issue of BOARD as it was at TIME, in reading order.
HISTORIES is an alist of issue key to history.  An issue made since is
as it is now, and one that left the board since comes back."
  (mapcar (lambda (issue)
            (shipit-atlassian-history--issue-at
             issue (shipit-atlassian-history--of issue histories) time))
          (append (shipit-atlassian-history--board-issues board)
                  (shipit-atlassian-history--left-issues board histories))))

(defun shipit-atlassian-history--board-at (board histories time)
  "Return BOARD as it stood at TIME, by HISTORIES.
Each card sits in the column that accepts the status it had then, and
a card made since sits where it is now.  A card whose status then no
column accepts was not on the board, so it is left out."
  (let* ((columns (shipit-atlassian-board--columns board))
         (issues (shipit-atlassian-history--issues-at board histories time)))
    (shipit-atlassian-history--with
     board 'columns
     (mapcar (lambda (column)
               (let ((name (cdr (assq 'name column))))
                 (shipit-atlassian-history--with
                  column 'issues
                  (seq-filter (lambda (issue)
                                (equal name (shipit-atlassian-board--column-name-for-status
                                             (cdr (assq 'state issue)) columns)))
                              issues))))
             columns))))

(defun shipit-atlassian-history--mark-moves (board histories time)
  "Return BOARD with the moves of each card since TIME, by HISTORIES.
Every card stays where it is.  One that moved since TIME carries those
moves in `timeline-moves' and its status at TIME in `timeline-other',
as the board of then does the other way round, so the two can be put
side by side."
  (shipit-atlassian-history--with
   board 'columns
   (mapcar (lambda (column)
             (shipit-atlassian-history--with
              column 'issues
              (mapcar (lambda (issue)
                        (let ((history (shipit-atlassian-history--of issue histories)))
                          (shipit-atlassian-history--with-moves
                           issue history time
                           (shipit-atlassian-history--status-at
                            (cdr (assq 'state issue)) history time))))
                      (cdr (assq 'issues column)))))
           (shipit-atlassian-board--columns board))))

(defun shipit-atlassian-history--moves-between (histories from to)
  "Return the moves in HISTORIES made after FROM and up to TO, newest first.
Each move is a change with the issue it moved in `:key'."
  (sort (cl-mapcan (lambda (entry)
                     (mapcar (lambda (change) (append (list :key (car entry)) change))
                             (seq-filter (lambda (change)
                                           (let ((time (plist-get change :time)))
                                             (and (> time from) (<= time to))))
                                         (plist-get (cdr entry) :changes))))
                   histories)
        (lambda (a b) (> (plist-get a :time) (plist-get b :time)))))

;;; Fetching

(defun shipit-atlassian-history--path (board-id start-at)
  "Return the path of one page of BOARD-ID\\='s issues with their changelogs.
START-AT is the index of the page\\='s first issue."
  (format "/rest/agile/1.0/board/%d/issue?maxResults=100&startAt=%d&fields=key,status,created&expand=changelog"
          board-id start-at))

(defconst shipit-atlassian-history--left-fields
  "key,summary,status,priority,issuetype,assignee,created"
  "The fields of an issue the board has lost since a time on the timeline.
What its card shows, and when it was made.")

(defun shipit-atlassian-history--without-sort (jql)
  "Return JQL without its ORDER BY, so more can be added to it."
  (let ((case-fold-search t))
    (string-trim (replace-regexp-in-string "[ \t\n]+order[ \t\n]+by[ \t\n].*\\'" "" jql))))

(defun shipit-atlassian-history--jql-time (time)
  "Return TIME as JQL writes a time.
In local time: Jira reads a time in JQL in the time zone of the user
it runs as, which is taken to be this one."
  (format-time-string "%Y/%m/%d %H:%M" time))

(defconst shipit-atlassian-history--unit-seconds
  '(("w" . 604800) ("d" . 86400) ("h" . 3600) ("m" . 60))
  "The units of a JQL relative date, and their seconds.")

(defconst shipit-atlassian-history--relative-date-re
  "\\(\\(?:[<>]=?\\|!?=\\)[ \t]*\\)\\([\"']?\\)\\([-+]?[0-9]+\\)\\([wdhm]\\)\\2"
  "A comparison with a relative date, such as `>= -7d'.
The groups are the operator, the quote, the amount and the unit.")

(defun shipit-atlassian-history--moved-date (match time)
  "Return MATCH, a comparison with a relative date, counting from TIME."
  (format "%s\"%s\""
          (match-string 1 match)
          (shipit-atlassian-history--jql-time
           (+ (float-time time)
              (* (string-to-number (match-string 3 match))
                 (cdr (assoc (match-string 4 match)
                             shipit-atlassian-history--unit-seconds)))))))

(defun shipit-atlassian-history--filter-at (jql time)
  "Return JQL, a board filter, as it matched at TIME, without its sort.
Each relative date in a comparison counts from TIME."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     shipit-atlassian-history--relative-date-re
     (lambda (match) (shipit-atlassian-history--moved-date match time))
     (shipit-atlassian-history--without-sort jql) t t)))

(defun shipit-atlassian-history--left-jql (filter time board-keys)
  "Return the JQL for the issues FILTER matched at TIME, less BOARD-KEYS.
BOARD-KEYS are the issues the board has now, whose history is read
with the board."
  (concat (format "(%s)" (shipit-atlassian-history--filter-at filter time))
          (if board-keys
              (format " AND key not in (%s)" (string-join board-keys ", "))
            "")))

(defun shipit-atlassian-history--of-left-issue (raw-issue)
  "Return (KEY . HISTORY) for RAW-ISSUE, which left the board.
The history carries the issue in `:issue', since the board does not."
  (let ((entry (shipit-atlassian-history--of-issue raw-issue)))
    (cons (car entry)
          (append (cdr entry)
                  (list :issue (shipit-issue-jira--normalize-issue raw-issue))))))

(defun shipit-atlassian-history--board-filter-async (config board-id callback)
  "Read the JQL of the filter of BOARD-ID through CONFIG.
CALLBACK gets it, or nil when the board names no filter."
  (shipit-issue-jira--api-request-async
   config (shipit-atlassian-board--config-path board-id)
   (lambda (board-config)
     (if-let* ((filter-id (cdr (assq 'id (cdr (assq 'filter board-config))))))
         (shipit-issue-jira--api-request-async
          config (format "/rest/api/2/filter/%s" filter-id)
          (lambda (filter) (funcall callback (cdr (assq 'jql filter)))))
       (funcall callback nil)))))

(defun shipit-atlassian-history--fetch-left-async (config filter time board-keys callback)
  "Read the history of the issues FILTER matched at TIME, less BOARD-KEYS.
FILTER is the board\\='s filter and CONFIG the Jira config.  CALLBACK
gets an alist of issue key to history, each carrying its issue."
  (shipit-issue-jira--search-all-async
   config (shipit-atlassian-history--left-jql filter time board-keys)
   shipit-atlassian-history--left-fields "changelog"
   (lambda (raw-issues)
     (funcall callback (mapcar #'shipit-atlassian-history--of-left-issue raw-issues)))))

(defun shipit-atlassian-history--fetch-async (config board-id callback)
  "Read the history of every issue on BOARD-ID through CONFIG.
CALLBACK gets an alist of issue key to history, or nil when nothing
came back."
  (shipit-atlassian-board--fetch-pages-async
   config
   (lambda (start-at) (shipit-atlassian-history--path board-id start-at))
   (lambda (raw-issues)
     (funcall callback (mapcar #'shipit-atlassian-history--of-issue raw-issues)))))

(provide 'shipit-atlassian-history)
;;; shipit-atlassian-history.el ends here
