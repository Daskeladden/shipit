;;; test-shipit-atlassian-board-shared.el --- Board column and transition logic -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The logic that reads a board's column order, picks the column one
;; step away and walks Jira transitions used to live in the dashboard,
;; reading the dashboard's own buffer-local state.  The canvas kanban
;; needs the same logic, and two UI files must not require each other,
;; so it moved down here and takes its state as arguments.
;;
;; These tests drive that layer on its own, with no dashboard buffer
;; and no dashboard variables in sight.  Only the backend dispatch is
;; mocked, which is where the HTTP boundary sits.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-atlassian-board)

(defun test-board-shared--boards ()
  "Two boards whose columns would be adjacent in a flat list.
`Alpha' ends at `Done' and `Beta' starts at `Triage', so a step off
the end of one must not land in the other."
  '(((id . 1) (name . "Alpha")
     (columns . (((name . "Open") (statuses . ("Open" "Reopened")))
                 ((name . "Code Review") (statuses . ("In Review" "Awaiting Review")))
                 ((name . "Done") (statuses . ("Done" "Closed"))))))
    ((id . 2) (name . "Beta")
     (columns . (((name . "Triage") (statuses . ("Triage")))
                 ((name . "Building") (statuses . ("Building")))
                 ((name . "Shipped") (statuses . ("Shipped"))))))))

(defun test-board-shared--boards-with-issues ()
  "One board carrying issues, for the lookup tests."
  '(((id . 1) (name . "Alpha")
     (columns . (((name . "In Progress")
                  (statuses . ("In Progress"))
                  (issues . (((key . "PROJ-1") (status . "In Progress"))
                             ((key . "PROJ-2") (status . "In Progress")))))
                 ((name . "Done")
                  (statuses . ("Done"))
                  (issues . (((key . "PROJ-3") (status . "Done"))))))))))

;;;; Columns

(ert-deftest test-board-shared-column-for-status ()
  "GIVEN columns that advertise the statuses they accept
WHEN a status is looked up
THEN the column accepting it is returned, whatever its case."
  (let ((columns (shipit-atlassian-board--all-columns
                  (test-board-shared--boards))))
    (should (equal "Code Review"
                   (cdr (assq 'name (shipit-atlassian-board--column-for-status
                                     columns "in review")))))
    (should (null (shipit-atlassian-board--column-for-status columns "Mythical")))
    (should (null (shipit-atlassian-board--column-for-status columns nil)))))

(ert-deftest test-board-shared-column-name-routes-via-statuses ()
  "GIVEN a column whose name differs from the status it accepts
WHEN the column name for that status is looked up
THEN the link is the column's status list, not its display name."
  (let ((columns (shipit-atlassian-board--all-columns
                  (test-board-shared--boards))))
    (should (equal "Code Review"
                   (shipit-atlassian-board--column-name-for-status
                    "In Review" columns)))
    (should (equal "Done"
                   (shipit-atlassian-board--column-name-for-status
                    "Closed" columns)))
    (should (equal "Building"
                   (shipit-atlassian-board--column-name-for-status
                    "Building" columns)))
    (should (null (shipit-atlassian-board--column-name-for-status
                   "Mythical" columns)))))

(ert-deftest test-board-shared-column-index ()
  "GIVEN a board's columns in configured order
WHEN a status is asked for its column index
THEN the index counts from the first column, and an unknown status has none."
  (let ((columns (shipit-atlassian-board--columns
                  (car (test-board-shared--boards)))))
    (should (equal 0 (shipit-atlassian-board--column-index "Open" columns)))
    (should (equal 2 (shipit-atlassian-board--column-index "closed" columns)))
    (should (null (shipit-atlassian-board--column-index "Mythical" columns)))))

(ert-deftest test-board-shared-columns-accepting ()
  "GIVEN several boards
WHEN the columns accepting a status are asked for
THEN the owning board's columns come back, not the flat list of all of them."
  (let ((boards (test-board-shared--boards)))
    (should (equal 3 (length (shipit-atlassian-board--columns-accepting
                              boards "Building"))))
    (should (equal "Triage"
                   (cdr (assq 'name (car (shipit-atlassian-board--columns-accepting
                                          boards "Building"))))))
    (should (null (shipit-atlassian-board--columns-accepting boards "Mythical")))))

(ert-deftest test-board-shared-by-id ()
  "GIVEN several boards
WHEN one is asked for by its identifier
THEN that board comes back, and an unknown identifier gives nil."
  (let ((boards (test-board-shared--boards)))
    (should (equal "Beta" (cdr (assq 'name (shipit-atlassian-board--by-id boards 2)))))
    (should (null (shipit-atlassian-board--by-id boards 99)))))

;;;; Stepping one column

(ert-deftest test-board-shared-target-status-steps-forward ()
  "GIVEN a status in the middle of a board
WHEN the column one step forward is asked for
THEN the first configured status of that column is returned."
  (let ((boards (test-board-shared--boards)))
    (should (equal "Building"
                   (shipit-atlassian-board--target-status boards "Triage" 'forward 2)))
    (should (equal "Shipped"
                   (shipit-atlassian-board--target-status boards "Building" 'forward)))))

(ert-deftest test-board-shared-target-status-steps-backward ()
  "GIVEN a status in the middle of a board
WHEN the column one step backward is asked for
THEN the previous column's first status is returned."
  (let ((boards (test-board-shared--boards)))
    (should (equal "Triage"
                   (shipit-atlassian-board--target-status boards "Building" 'backward 2)))))

(ert-deftest test-board-shared-target-status-holds-at-the-edges ()
  "GIVEN the first and the last column of a board
WHEN a step would leave the board
THEN nothing is returned, so a step never crosses into the next board."
  (let ((boards (test-board-shared--boards)))
    (should (null (shipit-atlassian-board--target-status boards "Done" 'forward 1)))
    (should (null (shipit-atlassian-board--target-status boards "Open" 'backward 1)))
    (should (null (shipit-atlassian-board--target-status boards nil 'forward)))))

;;;; Issues

(ert-deftest test-board-shared-issue-in-columns ()
  "GIVEN a board carrying issues
WHEN an issue is looked up by key
THEN the issue alist comes back, whatever the case of the key."
  (let ((columns (shipit-atlassian-board--columns
                  (car (test-board-shared--boards-with-issues)))))
    (should (equal "PROJ-2"
                   (cdr (assq 'key (shipit-atlassian-board--issue-in-columns
                                    columns "PROJ-2")))))
    (should (equal "PROJ-3"
                   (cdr (assq 'key (shipit-atlassian-board--issue-in-columns
                                    columns "proj-3")))))
    (should (null (shipit-atlassian-board--issue-in-columns columns "PROJ-99")))))

(ert-deftest test-board-shared-issue-in-boards ()
  "GIVEN several boards
WHEN an issue is looked up across all of them
THEN the issue is found on whichever board holds it."
  (let ((boards (test-board-shared--boards-with-issues)))
    (should (equal "PROJ-1"
                   (cdr (assq 'key (shipit-atlassian-board--issue-in-boards
                                    boards "proj-1")))))
    (should (null (shipit-atlassian-board--issue-in-boards boards "PROJ-99")))))

(ert-deftest test-board-shared-issue-key-reads-either-shape ()
  "GIVEN an issue in the agile shape and one in the work-item shape
WHEN its key is asked for
THEN both answer, because the lookup normalizes first."
  (should (equal "PROJ-1" (shipit-atlassian-board--issue-key '((key . "PROJ-1")))))
  (should (equal "PROJ-2" (shipit-atlassian-board--issue-key '((id . "PROJ-2"))))))

;;;; Staging a move

(ert-deftest test-board-shared-stage-move-adds-an-entry ()
  "GIVEN an empty pending list
WHEN a move is staged
THEN a new list carrying that target comes back, and the old list is untouched."
  (let* ((pending nil)
         (staged (shipit-atlassian-board--stage-move pending "PROJ-1" "Open" "In Review")))
    (should (equal '(("PROJ-1" . "In Review")) staged))
    (should (null pending))))

(ert-deftest test-board-shared-stage-move-replaces-an-entry ()
  "GIVEN a move already staged for an issue
WHEN a further move is staged for the same issue
THEN one entry remains, carrying the newer target."
  (let ((staged (shipit-atlassian-board--stage-move
                 '(("PROJ-1" . "In Review")) "PROJ-1" "Open" "Done")))
    (should (equal '(("PROJ-1" . "Done")) staged))))

(ert-deftest test-board-shared-stage-move-drops-a-net-zero ()
  "GIVEN a move staged away from a status
WHEN a move back to the actual status is staged
THEN the entry is dropped, because the staged moves cancelled out."
  (let ((staged (shipit-atlassian-board--stage-move
                 '(("PROJ-1" . "In Review") ("PROJ-2" . "Done"))
                 "PROJ-1" "Open" "open")))
    (should (equal '(("PROJ-2" . "Done")) staged))))

;;;; Transitions

(ert-deftest test-board-shared-match-transition-by-name ()
  "GIVEN a list of transitions and a target status name
WHEN the transitions are matched by name
THEN the one whose destination equals the target is returned, whatever its case."
  (let ((transitions '(((id . "11") (name . "Start Progress")
                        (to . ((name . "In Progress"))))
                       ((id . "21") (name . "Resolve")
                        (to . ((name . "Done")))))))
    (should (equal "11" (cdr (assq 'id (shipit-atlassian-board--match-transition
                                        transitions "In Progress")))))
    (should (equal "21" (cdr (assq 'id (shipit-atlassian-board--match-transition
                                        transitions "done")))))
    (should (null (shipit-atlassian-board--match-transition transitions "Mythical")))))

(ert-deftest test-board-shared-pick-step-takes-a-direct-transition ()
  "GIVEN a transition straight to the target column
WHEN a step toward that target is picked
THEN the direct transition wins."
  (let ((transitions '(((id . "11") (to . ((name . "In Review"))))
                       ((id . "21") (to . ((name . "Done")))))))
    (should (equal "21" (cdr (assq 'id (shipit-atlassian-board--pick-step-transition
                                        (test-board-shared--boards)
                                        transitions "Done")))))))

(ert-deftest test-board-shared-pick-step-walks-toward-the-target ()
  "GIVEN no transition straight to the target column
WHEN a step toward that target is picked
THEN the transition landing nearest the target is chosen.

This is what lets a staged move from `Open' to `Done' go through
`In Review' one Jira transition at a time."
  (let ((transitions '(((id . "11") (to . ((name . "In Review")))))))
    (should (equal "11" (cdr (assq 'id (shipit-atlassian-board--pick-step-transition
                                        (test-board-shared--boards)
                                        transitions "Done")))))))

(defmacro test-board-shared--with-jira (transitions-by-status fired &rest body)
  "Run BODY with the issue backend mocked at its dispatch boundary.
TRANSITIONS-BY-STATUS maps a status name to the transitions offered
there.  Each fired transition pushes its identifier onto FIRED and
moves the mocked issue to that transition's destination."
  (declare (indent 2))
  `(let ((status "Open"))
     (cl-letf (((symbol-function 'shipit-issue--resolve-for-repo)
                (lambda (_repo) (cons '(:name "jira") '(:base-url "https://x"))))
               ((symbol-function 'shipit-issue--get-transitions)
                (lambda (_backend _config _key)
                  (cdr (assoc status ,transitions-by-status))))
               ((symbol-function 'shipit-issue--transition-status)
                (lambda (_backend _config _key id)
                  (push id ,fired)
                  (setq status
                        (cl-loop for (_s . trs) in ,transitions-by-status
                                 thereis (cl-loop for tr in trs
                                                  when (equal id (cdr (assq 'id tr)))
                                                  return (cdr (assq 'name (cdr (assq 'to tr))))))))))
       ,@body)))

(ert-deftest test-board-shared-commit-walks-to-the-target ()
  "GIVEN a workflow offering only one column at a time
WHEN a move from the first column to the last is committed
THEN every step fires in order until the target column is reached."
  (let ((fired nil)
        (workflow '(("Open" . (((id . "11") (to . ((name . "In Review"))))))
                    ("In Review" . (((id . "21") (to . ((name . "Done"))))))
                    ("Done" . nil))))
    (test-board-shared--with-jira workflow fired
      (should (shipit-atlassian-board--commit-one
               (test-board-shared--boards) "o/r" "PROJ-1" "Done")))
    (should (equal '("11" "21") (nreverse fired)))))

(ert-deftest test-board-shared-commit-errors-without-a-way-forward ()
  "GIVEN a workflow offering no transition toward the target
WHEN the move is committed
THEN an error says so, rather than the move reporting success."
  (let ((fired nil)
        (workflow '(("Open" . nil))))
    (test-board-shared--with-jira workflow fired
      (should-error (shipit-atlassian-board--commit-one
                     (test-board-shared--boards) "o/r" "PROJ-1" "Done")))
    (should (null fired))))

(ert-deftest test-board-shared-commit-pending-reports-each-outcome ()
  "GIVEN two staged moves, one of which Jira refuses
WHEN they are committed
THEN the one that landed is counted and dropped, and the one that
     failed stays staged with a line saying why."
  (let ((fired nil)
        (workflow '(("Open" . (((id . "11") (to . ((name . "In Review"))))))
                    ("In Review" . nil))))
    (test-board-shared--with-jira workflow fired
      (pcase-let ((`(,committed ,remaining ,failures)
                   (shipit-atlassian-board--commit-pending
                    (test-board-shared--boards) "o/r"
                    '(("PROJ-1" . "In Review") ("PROJ-2" . "Done")))))
        (should (equal 1 committed))
        (should (equal '(("PROJ-2" . "Done")) remaining))
        (should (equal 1 (length failures)))
        (should (string-match-p "PROJ-2 -> Done" (car failures)))))))

(ert-deftest test-board-shared-commit-pending-leaves-the-list-alone ()
  "GIVEN a staged list the caller still holds
WHEN the moves are committed
THEN the caller's own list is unchanged, because a new one comes back."
  (let* ((fired nil)
         (workflow '(("Open" . (((id . "11") (to . ((name . "In Review"))))))
                     ("In Review" . nil)))
         (pending '(("PROJ-1" . "In Review"))))
    (test-board-shared--with-jira workflow fired
      (shipit-atlassian-board--commit-pending
       (test-board-shared--boards) "o/r" pending))
    (should (equal '(("PROJ-1" . "In Review")) pending))))

;;;; Fetching several boards at once

(ert-deftest test-board-shared-each-async-keeps-the-order ()
  "GIVEN items whose results come back out of order
WHEN they are run at once
THEN the results are in the order the items were given."
  (let ((reports nil)
        (got 'unset))
    (shipit-atlassian-board--each-async
     '(:a :b :c)
     (lambda (item report) (push (cons item report) reports))
     (lambda (results) (setq got results)))
    ;; Report the last item first; the caller must still see a, b, c.
    (pcase-dolist (`(,item . ,report) reports)
      (funcall report (list item)))
    (should (equal '((:a) (:b) (:c)) got))))

(ert-deftest test-board-shared-each-async-answers-at-once-for-nothing ()
  "GIVEN no items
WHEN they are run at once
THEN the callback still runs, with nothing."
  (let ((got 'unset))
    (shipit-atlassian-board--each-async
     nil #'ignore (lambda (results) (setq got results)))
    (should (null got))))

(ert-deftest test-board-shared-fetch-names-reads-only-the-configuration ()
  "GIVEN two configured boards
WHEN their names are fetched
THEN each board configuration is read once, no issues are fetched, and
     the names come back against their identifiers."
  (let ((paths nil)
        (got nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-async)
               (lambda (_config path callback)
                 (push path paths)
                 (funcall callback '((name . "Alpha"))))))
      (shipit-atlassian-board--fetch-names-async
       nil '(7 9) (lambda (names) (setq got names))))
    (should (equal '((7 . "Alpha") (9 . "Alpha")) got))
    (should (equal 2 (length paths)))
    (should (cl-every (lambda (p) (string-suffix-p "/configuration" p)) paths))))

(ert-deftest test-board-shared-fetch-names-falls-back-to-the-identifier ()
  "GIVEN a board configuration that carries no name
WHEN the names are fetched
THEN the board is named after its identifier rather than nil."
  (let ((got nil))
    (cl-letf (((symbol-function 'shipit-issue-jira--api-request-async)
               (lambda (_config _path callback) (funcall callback nil))))
      (shipit-atlassian-board--fetch-names-async
       nil '(42) (lambda (names) (setq got names))))
    (should (equal '((42 . "Board 42")) got))))

(provide 'test-shipit-atlassian-board-shared)
;;; test-shipit-atlassian-board-shared.el ends here
