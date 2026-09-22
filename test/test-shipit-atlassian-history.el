;;; test-shipit-atlassian-history.el --- How a board changed over time -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The timeline of the canvas kanban shows a board as it stood at an
;; earlier time.  Jira keeps a changelog on every issue, and these tests
;; drive the data work on it: reading the status changes, working out a
;; card's status at a time, putting each card back in the column it was
;; in, and listing the moves of a range.  Only the Jira request is
;; stood in for.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-atlassian-history)

(defconst test-history--day 86400.0
  "Seconds in a day.")

(defun test-history--iso (seconds-ago)
  "Return the time SECONDS-AGO as Jira writes a time."
  (format-time-string "%FT%T.000%z" (time-subtract nil seconds-ago)))

(defun test-history--change (days-ago by from to)
  "Return a Jira changelog entry: BY moved an issue FROM TO, DAYS-AGO."
  `((id . "1")
    (created . ,(test-history--iso (* days-ago test-history--day)))
    (author . ((displayName . ,by)))
    (items . [((field . "status") (fromString . ,from) (toString . ,to))])))

(defun test-history--raw-issue (key status created-days-ago &rest changes)
  "Return a board issue as Jira sends it with its changelog.
KEY is in STATUS now and was created CREATED-DAYS-AGO.  CHANGES are
changelog entries, newest first, the way Jira lists them."
  `((key . ,key)
    (fields . ((status . ((name . ,status)))
               (created . ,(test-history--iso (* created-days-ago test-history--day)))))
    (changelog . ((histories . ,(vconcat changes))))))

(defun test-history--board ()
  "A board with a card moved yesterday, one never moved, and one made today."
  '((id . 1) (name . "Alpha")
    (columns . (((name . "Open") (statuses . ("Open"))
                 (issues . (((key . "PRJ-2") (title . "Rename the widget") (state . "Open"))
                            ((key . "PRJ-3") (title . "Made today") (state . "Open")))))
                ((name . "Code Review") (statuses . ("In Review"))
                 (issues . ()))
                ((name . "Done") (statuses . ("Done"))
                 (issues . (((key . "PRJ-1") (title . "Ship the parser") (state . "Done")))))))))

(defun test-history--histories ()
  "The history of every issue of `test-history--board'."
  (mapcar #'shipit-atlassian-history--of-issue
          (list (test-history--raw-issue
                 "PRJ-1" "Done" 5
                 (test-history--change 1 "Bob" "In Review" "Done")
                 (test-history--change 3 "Alice" "Open" "In Review"))
                (test-history--raw-issue "PRJ-2" "Open" 5)
                (test-history--raw-issue "PRJ-3" "Open" 0.1))))

(defun test-history--keys (board column)
  "Return the issue keys in COLUMN of BOARD, whatever shape each issue has."
  (mapcar #'shipit-atlassian-board--issue-key
          (cdr (assq 'issues (cl-find column (cdr (assq 'columns board))
                                      :key (lambda (c) (cdr (assq 'name c)))
                                      :test #'equal)))))

;;;; Reading the changelog

(ert-deftest test-history-reads-the-status-changes-oldest-first ()
  "GIVEN an issue whose changelog holds two status changes, newest first,
and a change of assignee
WHEN its history is read
THEN it holds the two status changes oldest first, each with its time,
its two statuses and who made it, and not the change of assignee."
  (let* ((assignee `((id . "9") (created . ,(test-history--iso 3600))
                     (author . ((displayName . "Carol")))
                     (items . [((field . "assignee") (fromString . "Alice") (toString . "Bob"))])))
         (history (cdr (shipit-atlassian-history--of-issue
                        (test-history--raw-issue
                         "PRJ-1" "Done" 5 assignee
                         (test-history--change 1 "Bob" "In Review" "Done")
                         (test-history--change 3 "Alice" "Open" "In Review")))))
         (changes (plist-get history :changes)))
    (should (equal '(("Open" "In Review" "Alice") ("In Review" "Done" "Bob"))
                   (mapcar (lambda (c) (list (plist-get c :from) (plist-get c :to)
                                             (plist-get c :by)))
                           changes)))
    (should (< (abs (- (plist-get (car changes) :time)
                       (- (float-time) (* 3 test-history--day))))
               2))
    (should (< (plist-get history :created) (plist-get (car changes) :time)))))

(ert-deftest test-history-status-at-a-time-is-worked-out-back-from-now ()
  "GIVEN an issue created five days ago, moved three days ago and yesterday
WHEN its status is asked for at several times
THEN it is the status it had then, and nothing before it was created."
  (let* ((history (cdr (assoc "PRJ-1" (test-history--histories))))
         (ago (lambda (days) (- (float-time) (* days test-history--day)))))
    (should-not (shipit-atlassian-history--status-at "Done" history (funcall ago 6)))
    (should (equal "Open" (shipit-atlassian-history--status-at "Done" history (funcall ago 4))))
    (should (equal "In Review" (shipit-atlassian-history--status-at "Done" history (funcall ago 2))))
    (should (equal "Done" (shipit-atlassian-history--status-at "Done" history (float-time))))))

(ert-deftest test-history-an-issue-without-history-stays-as-it-is ()
  "GIVEN an issue the history does not know, one added to the board since
WHEN its status is asked for at an earlier time
THEN it keeps the status it has now."
  (should (equal "Open" (shipit-atlassian-history--status-at "Open" nil 0))))

;;;; The board at a time

(ert-deftest test-history-board-at-puts-each-card-where-it-was ()
  "GIVEN a board with a card moved yesterday and a card made today
WHEN the board is asked for as it stood two days ago
THEN the moved card is back in Code Review with its move since then
and its status now recorded, and the others stay where they are."
  (let ((board (shipit-atlassian-history--board-at
                (test-history--board) (test-history--histories)
                (- (float-time) (* 2 test-history--day)))))
    (should (equal '("PRJ-2" "PRJ-3") (test-history--keys board "Open")))
    (should (equal '("PRJ-1") (test-history--keys board "Code Review")))
    (should (equal '() (test-history--keys board "Done")))
    (let ((moved (car (cdr (assq 'issues (nth 1 (cdr (assq 'columns board))))))))
      (should (equal "In Review" (cdr (assq 'state moved))))
      (should (equal "Done" (cdr (assq 'timeline-other moved))))
      (should (equal '("Bob") (mapcar (lambda (c) (plist-get c :by))
                                      (cdr (assq 'timeline-moves moved))))))
    (should (equal "Alpha" (cdr (assq 'name board))))))

(ert-deftest test-history-lists-the-moves-of-a-range-newest-first ()
  "GIVEN two moves, three days ago and yesterday
WHEN the moves of the last two days, and of the last four, are asked for
THEN the first holds yesterday's move alone, and the second both, newest
first, each with its card."
  (let* ((histories (test-history--histories))
         (now (float-time))
         (recent (shipit-atlassian-history--moves-between
                  histories (- now (* 2 test-history--day)) now))
         (all (shipit-atlassian-history--moves-between
               histories (- now (* 4 test-history--day)) now)))
    (should (equal '(("PRJ-1" "Bob")) (mapcar (lambda (m) (list (plist-get m :key) (plist-get m :by)))
                                              recent)))
    (should (equal '("Bob" "Alice") (mapcar (lambda (m) (plist-get m :by)) all)))))

(defun test-history--issue (board key)
  "Return the issue KEY of BOARD, as the board has it."
  (seq-find (lambda (issue) (equal key (shipit-atlassian-board--issue-key issue)))
            (shipit-atlassian-history--board-issues board)))

(ert-deftest test-history-board-at-shows-a-card-made-since-where-it-is-now ()
  "GIVEN a board with a card made today, never moved, and one made long ago
WHEN the board is asked for as it stood two days ago
THEN the card made today is where it is now, carrying when it was made,
AND the card made long ago carries nothing of the kind."
  (let* ((board (shipit-atlassian-history--board-at
                 (test-history--board) (test-history--histories)
                 (- (float-time) (* 2 test-history--day))))
         (made (test-history--issue board "PRJ-3")))
    (should (equal "Open" (cdr (assq 'state made))))
    (should (< (abs (- (cdr (assq 'timeline-new made))
                       (- (float-time) (* 0.1 test-history--day))))
               2))
    (should-not (assq 'timeline-moves made))
    (should-not (assq 'timeline-new (test-history--issue board "PRJ-2")))))

;;;; The board now, beside the board of then

(ert-deftest test-history-marks-the-cards-made-since ()
  "GIVEN a board with a card made today and one made long ago
WHEN the board now is marked with what changed since two days ago
THEN the card made today carries when it was made, AND the other does not."
  (let ((board (shipit-atlassian-history--mark-moves
                (test-history--board) (test-history--histories)
                (- (float-time) (* 2 test-history--day)))))
    (should (assq 'timeline-new (test-history--issue board "PRJ-3")))
    (should-not (assq 'timeline-new (test-history--issue board "PRJ-2")))))

(ert-deftest test-history-marks-the-cards-that-moved-since ()
  "GIVEN a board with a card moved yesterday
WHEN the board now is marked with the moves since two days ago
THEN every card stays where it is, and the moved one carries its move
and the status it had then."
  (let* ((board (shipit-atlassian-history--mark-moves
                 (test-history--board) (test-history--histories)
                 (- (float-time) (* 2 test-history--day))))
         (moved (car (cdr (assq 'issues (nth 2 (cdr (assq 'columns board))))))))
    (should (equal '("PRJ-2" "PRJ-3") (test-history--keys board "Open")))
    (should (equal '("PRJ-1") (test-history--keys board "Done")))
    (should (equal "Done" (cdr (assq 'state moved))))
    (should (equal "In Review" (cdr (assq 'timeline-other moved))))
    (should (equal '("Bob") (mapcar (lambda (c) (plist-get c :by))
                                    (cdr (assq 'timeline-moves moved)))))
    (should-not (assq 'timeline-moves (car (cdr (assq 'issues (car (cdr (assq 'columns board))))))))))

;;;; Cards that left the board

(defun test-history--left-raw-issue ()
  "PRJ-9 as the search sends it: done since yesterday.
The board's filter has dropped it since, so the board does not have it."
  (let ((raw (test-history--raw-issue
              "PRJ-9" "Done" 5
              (test-history--change 1 "Dana" "In Review" "Done"))))
    (setf (alist-get 'fields raw)
          (append '((summary . "Shipped and gone") (issuetype . ((name . "Story"))))
                  (alist-get 'fields raw)))
    raw))

(ert-deftest test-history-board-at-brings-back-a-card-that-left ()
  "GIVEN a card the board no longer has, whose history says it was in
review two days ago and done since
WHEN the board is asked for as it stood two days ago
THEN the card is back in Code Review, with its move and its status now."
  (let* ((histories (append (test-history--histories)
                            (list (shipit-atlassian-history--of-left-issue
                                   (test-history--left-raw-issue)))))
         (board (shipit-atlassian-history--board-at
                 (test-history--board) histories
                 (- (float-time) (* 2 test-history--day)))))
    (should (equal '("PRJ-1" "PRJ-9") (test-history--keys board "Code Review")))
    (let ((gone (cadr (cdr (assq 'issues (nth 1 (cdr (assq 'columns board))))))))
      (should (equal "Shipped and gone" (cdr (assq 'title gone))))
      (should (equal "Done" (cdr (assq 'timeline-other gone)))))))

(ert-deftest test-history-filter-at-moves-its-relative-dates ()
  "GIVEN a board filter that keeps resolved issues for 7 days, sorted
WHEN it is asked for as it matched a week ago
THEN its relative date counts from then, and its sort is gone, while a
relative date inside a function is left as it is."
  (let ((then (encode-time '(0 30 9 12 9 2026 nil -1 nil))))
    (should (equal (concat "status = Open OR (status = Resolved AND"
                           " resolutiondate >= \"2026/09/05 09:30\")")
                   (shipit-atlassian-history--filter-at
                    "status = Open OR (status = Resolved AND resolutiondate >= -7d) ORDER BY Rank ASC"
                    then)))
    (should (equal "updated > \"2026/09/12 07:30\" AND created < startOfDay(-1d)"
                   (shipit-atlassian-history--filter-at
                    "updated > \"-2h\" AND created < startOfDay(-1d)" then)))))

(ert-deftest test-history-left-query-leaves-out-the-board ()
  "GIVEN the board's filter and the keys the board has now
WHEN the query for the cards that were on the board then is made
THEN it is the filter as it matched then, less the cards on the board."
  (should (equal (concat "(resolutiondate >= \"2026/09/05 09:30\")"
                         " AND key not in (PRJ-1, PRJ-2)")
                 (shipit-atlassian-history--left-jql
                  "resolutiondate >= -7d" (encode-time '(0 30 9 12 9 2026 nil -1 nil))
                  '("PRJ-1" "PRJ-2")))))

(ert-deftest test-history-board-filter-reads-the-filter-of-the-board ()
  "GIVEN a board whose configuration names its filter
WHEN its filter is read
THEN it is the JQL of that filter."
  (let ((got :unset))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-async)
               (lambda (_config path callback)
                 (funcall callback
                          (cond
                           ((string-match-p "/board/7/configuration" path)
                            '((filter . ((id . "77")))))
                           ((string-match-p "/filter/77" path)
                            '((jql . "project = PRJ"))))))))
      (shipit-atlassian-history--board-filter-async
       '(:base-url "https://jira.example.com") 7 (lambda (jql) (setq got jql)))
      (should (equal "project = PRJ" got)))))

(ert-deftest test-history-fetch-left-reads-the-cards-with-their-issue ()
  "GIVEN a board filter, and a search that finds one card the board lost
WHEN the cards that were on the board then are read
THEN the search asks for their changelog, and each comes back with its
history and the issue to draw it from."
  (let ((paths nil)
        (got nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-async)
               (lambda (_config path callback)
                 (push path paths)
                 (funcall callback `((issues . [,(test-history--left-raw-issue)]) (isLast . t))))))
      (shipit-atlassian-history--fetch-left-async
       '(:base-url "https://jira.example.com") "project = PRJ"
       (- (float-time) (* 2 test-history--day)) '("PRJ-1")
       (lambda (left) (setq got left))))
    (should (string-match-p "expand=changelog" (car paths)))
    (should (equal '("PRJ-9") (mapcar #'car got)))
    (should (equal "Done" (plist-get (car (plist-get (cdr (car got)) :changes)) :to)))
    (should (equal "Shipped and gone"
                   (cdr (assq 'title (plist-get (cdr (car got)) :issue)))))))

;;;; Fetching

(ert-deftest test-history-fetch-reads-every-page-with-its-changelog ()
  "GIVEN a board whose issues come in two pages
WHEN its history is fetched
THEN each page is asked for with the changelog and the creation time,
and the history of every issue comes back by key."
  (let ((paths nil)
        (got nil)
        (pages (list `((total . 2) (issues . [,(test-history--raw-issue "PRJ-1" "Done" 5)]))
                     `((total . 2) (issues . [,(test-history--raw-issue "PRJ-2" "Open" 5)])))))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-async)
               (lambda (_config path callback)
                 (push path paths)
                 (funcall callback (pop pages)))))
      (shipit-atlassian-history--fetch-async '(:base-url "https://jira.example.com") 7
                                             (lambda (histories) (setq got histories))))
    (should (equal '("PRJ-1" "PRJ-2") (mapcar #'car got)))
    (should (= 2 (length paths)))
    (dolist (path paths)
      (should (string-match-p "/rest/agile/1.0/board/7/issue" path))
      (should (string-match-p "expand=changelog" path))
      (should (string-match-p "created" path)))))

(provide 'test-shipit-atlassian-history)
;;; test-shipit-atlassian-history.el ends here
