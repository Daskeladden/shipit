;;; test-shipit-issues-region-actions.el --- Act on the rows in the region -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; In a buffer that embark exported, the user marks some rows of an
;; issue list with the region and calls embark.  The rows that the
;; region touches are then what the actions of a row work on.  The row
;; finder gives one target that covers those whole rows, and a collector
;; of candidates gives each of them, for "act on all".
;;
;; Embark is not on the load path of the tests.  The tests call the
;; finder and the collector at point, and the actions the way embark
;; calls them: one call with the list of the values.  The rows, the
;; buffer, the kill ring, the user at the prompt and the fake Jira come
;; from the tests of the rows of an issue list and of the change of the
;; state of many issues.

;;; Code:

(require 'ert)
(require 'seq)
(require 'shipit-issues)
(require 'test-shipit-issues-bulk-status)
(require 'test-shipit-issues-row-actions)

;; Embark is not on the load path of the tests.  The tests bind these
;; the way embark keeps them.
(defvar embark-candidate-collectors)
(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-multitarget-actions)
(defvar embark-default-action-overrides)
(defvar embark-general-map)

;;; The buffer of rows

(defun test-region-actions--row-bounds (rows key)
  "Where the buffer shows the row of ROWS that names KEY, as (START . END).
Fails when the buffer does not show that row."
  (let ((text (substring-no-properties (test-row-actions--row-of rows key))))
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward text nil t)
        (ert-fail (list "The buffer does not show the row" text (buffer-string))))
      (cons (match-beginning 0) (match-end 0)))))

(defun test-region-actions--show-rows (keys)
  "Show the rows of the issues KEYS of acme/door, as an export of embark does.
The rows are in the order of KEYS, and the form fails when the buffer
does not show them in that order.  Returns the rows, in that order."
  (let ((rows (test-row-actions--rows "acme/door" keys)))
    (test-row-actions--insert-export rows)
    (should (apply #'< (mapcar (lambda (key)
                                 (car (test-region-actions--row-bounds rows key)))
                               keys)))
    rows))

(defmacro test-region-actions--in-export (rows keys &rest body)
  "Run BODY in a buffer that shows the rows of the issues KEYS of acme/door.
The rows are in the order of KEYS, as an export of embark shows them,
and BODY finds them in the variable ROWS, in that order.  The region
works as in a usual Emacs, with Transient Mark mode on."
  (declare (indent 2))
  `(let ((transient-mark-mode t))
     (with-temp-buffer
       (let ((,rows (test-region-actions--show-rows ,keys)))
         ,@body))))

(defun test-region-actions--span-of-rows (rows first-key last-key)
  "From the start of the row of FIRST-KEY to the end of the row of LAST-KEY.
ROWS are the rows that the buffer shows.  The span is (START . END)."
  (cons (car (test-region-actions--row-bounds rows first-key))
        (cdr (test-region-actions--row-bounds rows last-key))))

(defun test-region-actions--placed (rows keys)
  "The rows of ROWS that name KEYS, each as (TEXT START . END) in the buffer.
TEXT has no text properties.  They are in the order of KEYS."
  (mapcar (lambda (key)
            (cons (substring-no-properties (test-row-actions--row-of rows key))
                  (test-region-actions--row-bounds rows key)))
          keys))

;;; The region

(defun test-region-actions--mark (from to)
  "Make the region active from FROM to TO, with point at TO.
Fails when Emacs does not take it as an active region."
  (set-mark from)
  (goto-char to)
  (should mark-active)
  (should (use-region-p)))

(defun test-region-actions--middle-of (bounds)
  "A position inside the text of BOUNDS, as (START . END), and at neither end."
  (let ((middle (/ (+ (car bounds) (cdr bounds)) 2)))
    (should (< (car bounds) middle (cdr bounds)))
    middle))

(defun test-region-actions--mark-rows (rows from-key to-key)
  "Mark from the middle of the row of FROM-KEY to the middle of that of TO-KEY.
ROWS are the rows that the buffer shows.  The region is active, and
point is at its end."
  (test-region-actions--mark
   (test-region-actions--middle-of (test-region-actions--row-bounds rows from-key))
   (test-region-actions--middle-of (test-region-actions--row-bounds rows to-key))))

(defun test-region-actions--note-after (rows key)
  "The note after the row of KEY, up to the end of its line, as (START . END).
ROWS are the rows that the buffer shows."
  (save-excursion
    (goto-char (cdr (test-region-actions--row-bounds rows key)))
    (cons (point) (line-end-position))))

(defun test-region-actions--mark-inside-note-after (rows key)
  "Mark a part of the note after the row of KEY that touches no end of the note.
ROWS are the rows that the buffer shows.  The region is active, point
is at its end, and the form fails when the region holds text of a row."
  (let ((note (test-region-actions--note-after rows key)))
    (test-region-actions--mark (1+ (car note)) (1- (cdr note)))
    (should-not (text-property-not-all (region-beginning) (region-end)
                                       'shipit-issue-row nil))))

(defun test-region-actions--unmark ()
  "Make the region inactive.  Fails when Emacs still takes it as active."
  (deactivate-mark)
  (should-not (use-region-p)))

;;; Targets and candidates, as embark takes them

(defun test-region-actions--target-at-point ()
  "The target that the row finder gives at point.  Fails when it gives none."
  (or (shipit-issues-embark-target)
      (ert-fail "The row finder gives no target at point")))

(defun test-region-actions--handed (target)
  "What embark hands an action of a row on TARGET: a list of its value.
Embark calls each action of a row once with a list of the values."
  (list (cadr target)))

(defun test-region-actions--candidates-as-text (collected)
  "The candidates of COLLECTED, each as (TEXT START . END).
COLLECTED is (KIND CANDIDATE...).  TEXT has no text properties.  A
candidate that has no bounds is given as it is."
  (mapcar (lambda (candidate)
            (if (consp candidate)
                (cons (substring-no-properties (car candidate)) (cdr candidate))
              candidate))
          (cdr collected)))

(defun test-region-actions--candidate-values (collected)
  "What embark hands an action when it acts on all candidates of COLLECTED.
COLLECTED is (KIND CANDIDATE...), and a candidate with bounds is
\(VALUE START . END)."
  (mapcar (lambda (candidate) (if (consp candidate) (car candidate) candidate))
          (cdr collected)))

(defun test-region-actions--jira-requests-about (key)
  "The requests that the fake Jira got about the issue KEY, oldest first."
  (seq-filter (lambda (request) (member key (split-string (nth 1 request) "/")))
              test-bulk-status--requests))

;;; Embark

(defmacro test-region-actions--with-embark (collectors &rest body)
  "Run BODY with embark as it is set up, with the candidate COLLECTORS.
Embark has a target finder of its own and no other setting.  The
general actions that the issue search takes over are undone after BODY."
  (declare (indent 1))
  `(let ((embark-general-map (make-sparse-keymap))
         (embark-target-finders (list 'embark-target-url-at-point))
         (embark-keymap-alist nil)
         (embark-multitarget-actions nil)
         (embark-default-action-overrides nil)
         (embark-candidate-collectors ,collectors))
     (unwind-protect
         (progn ,@body)
       (set-keymap-parent shipit-issues-embark-map nil))))

;;; The scenarios

(ert-deftest test-shipit-issues-region-actions-the-region-is-one-target-of-whole-rows ()
  "A region over rows is one target that covers each row it touches, whole."
  ;; GIVEN a buffer that shows the rows of PRJ-3, PRJ-2 and PRJ-1 of
  ;; acme/door in this order, as an export of embark does
  ;; AND an active region from the middle of the row of PRJ-3 to the
  ;; middle of the row of PRJ-2
  ;; WHEN embark asks for the target at point
  ;; THEN the target is of the kind `shipit-issue'
  ;; AND it covers the two whole rows, from the start of the row of PRJ-3
  ;; to the end of the row of PRJ-2
  ;; AND its value starts with a line that tells the count, `2 issues:
  ;; PRJ-3 to PRJ-2', which is what embark shows of a target, and then
  ;; holds that text
  ;; AND w on that target copies PRJ-3 and PRJ-2, one per line, and not
  ;; PRJ-1
  (test-row-actions--with-trackers
    (test-region-actions--in-export rows '("PRJ-3" "PRJ-2" "PRJ-1")
      (test-region-actions--mark-rows rows "PRJ-3" "PRJ-2")
      (let ((target (test-region-actions--target-at-point))
            (span (test-region-actions--span-of-rows rows "PRJ-3" "PRJ-2")))
        (should (eq 'shipit-issue (car target)))
        (should (equal span (cddr target)))
        (should (equal (concat "2 issues: PRJ-3 to PRJ-2\n"
                               (buffer-substring-no-properties (car span) (cdr span)))
                       (substring-no-properties (cadr target))))
        (should (equal '("PRJ-3" "PRJ-2")
                       (test-row-actions--lines
                        (test-row-actions--copied-by
                         "w" (test-region-actions--handed target)))))))))

(ert-deftest test-shipit-issues-region-actions-state-change-moves-only-the-rows-in-the-region ()
  "The state action on a region moves the issues of its rows and no other."
  ;; GIVEN a buffer that shows the rows of PRJ-3, PRJ-2 and PRJ-1 of
  ;; acme/door in this order, as an export of embark does
  ;; AND an active region from the middle of the row of PRJ-3 to the
  ;; middle of the row of PRJ-2
  ;; AND a Jira where PRJ-3, PRJ-2 and PRJ-1 each can move to Done, with
  ;; a move that takes no resolution
  ;; WHEN s runs on the target of the region and the user picks
  ;; `Done (2 of 2)'
  ;; THEN PRJ-3 and PRJ-2 are moved to Done
  ;; AND Jira gets no request at all about PRJ-1
  ;; AND the message is `Moved 2 issues to Done'
  (test-bulk-status--with-jira
      '(:moves (("PRJ-1" "Done")
                ("PRJ-2" "Done")
                ("PRJ-3" "Done")))
    (test-region-actions--in-export rows '("PRJ-3" "PRJ-2" "PRJ-1")
      (test-region-actions--mark-rows rows "PRJ-3" "PRJ-2")
      (let ((run (test-bulk-status--change-status
                  (test-region-actions--handed (test-region-actions--target-at-point))
                  '("Done (2 of 2)"))))
        (should (equal '(("PRJ-2" "Done" nil)
                         ("PRJ-3" "Done" nil))
                       (test-bulk-status--jira-moves)))
        (should-not (test-region-actions--jira-requests-about "PRJ-1"))
        (should (equal "Moved 2 issues to Done"
                       (test-bulk-status--last-message run)))))))

(ert-deftest test-shipit-issues-region-actions-a-region-without-rows-is-no-target ()
  "A region that holds no row gives no target of a row."
  ;; GIVEN a buffer that shows the rows of PRJ-3, PRJ-2 and PRJ-1 of
  ;; acme/door in this order, as an export of embark does
  ;; AND an active region that holds no row: it lies inside the note
  ;; after the row of PRJ-3
  ;; WHEN embark asks for the target at point
  ;; THEN the row finder gives nothing
  (test-row-actions--with-trackers
    (test-region-actions--in-export rows '("PRJ-3" "PRJ-2" "PRJ-1")
      (test-region-actions--mark-inside-note-after rows "PRJ-3")
      (should-not (shipit-issues-embark-target)))))

(ert-deftest test-shipit-issues-region-actions-act-on-all-takes-the-rows-of-the-region ()
  "Act on all takes each row in the region, and nothing without a region."
  ;; GIVEN a buffer that shows the rows of PRJ-3, PRJ-2 and PRJ-1 of
  ;; acme/door in this order, as an export of embark does
  ;; AND an active region from the middle of the row of PRJ-3 to the
  ;; middle of the row of PRJ-2
  ;; WHEN embark collects the candidates for "act on all"
  ;; THEN the collector gives the kind `shipit-issue' and exactly two
  ;; candidates, the whole rows of PRJ-3 and PRJ-2 with their bounds in
  ;; the buffer, in this order
  ;; AND w on the two candidates copies PRJ-3 and PRJ-2
  (test-row-actions--with-trackers
    (test-region-actions--in-export rows '("PRJ-3" "PRJ-2" "PRJ-1")
      (test-region-actions--mark-rows rows "PRJ-3" "PRJ-2")
      (let ((collected (shipit-issues-embark-region-candidates)))
        (should (eq 'shipit-issue (car collected)))
        (should (equal (test-region-actions--placed rows '("PRJ-3" "PRJ-2"))
                       (test-region-actions--candidates-as-text collected)))
        (should (equal '("PRJ-3" "PRJ-2")
                       (test-row-actions--lines
                        (test-row-actions--copied-by
                         "w" (test-region-actions--candidate-values collected))))))
      ;; GIVEN no active region in that buffer
      ;; WHEN embark collects the candidates again
      ;; THEN the collector gives nothing
      (test-region-actions--unmark)
      (should-not (shipit-issues-embark-region-candidates)))))

(ert-deftest test-shipit-issues-region-actions-embark-collects-from-the-region-first ()
  "The collector of the rows in the region is the first of embark's collectors."
  ;; GIVEN embark with a candidate collector of its own already in its
  ;; list
  ;; WHEN the issue search sets itself up for embark
  ;; THEN the collector of the rows in the region is the first of
  ;; embark's candidate collectors
  (test-region-actions--with-embark (list 'embark-minibuffer-candidates)
    (shipit-issues--embark-setup)
    (should (eq 'shipit-issues-embark-region-candidates
                (car embark-candidate-collectors)))))

(ert-deftest test-shipit-issues-region-actions-whole-lines-leave-out-the-row-after-them ()
  "A region of whole lines takes their rows, and not the row that point is on."
  ;; GIVEN a buffer that shows the rows of PRJ-3, PRJ-2 and PRJ-1 of
  ;; acme/door in this order, as an export of embark does
  ;; AND an active region of the first two lines: from the start of the
  ;; row of PRJ-3 to the start of the row of PRJ-1, where point is
  ;; WHEN embark asks for the target at point
  ;; THEN the target covers the rows of PRJ-3 and PRJ-2
  ;; AND w on that target copies PRJ-3 and PRJ-2, one per line, and not
  ;; PRJ-1, the row that point is on
  (test-row-actions--with-trackers
    (test-region-actions--in-export rows '("PRJ-3" "PRJ-2" "PRJ-1")
      (test-region-actions--mark
       (car (test-region-actions--row-bounds rows "PRJ-3"))
       (car (test-region-actions--row-bounds rows "PRJ-1")))
      (let ((target (test-region-actions--target-at-point)))
        (should (equal (test-region-actions--span-of-rows rows "PRJ-3" "PRJ-2")
                       (cddr target)))
        (should (equal '("PRJ-3" "PRJ-2")
                       (test-row-actions--lines
                        (test-row-actions--copied-by
                         "w" (test-region-actions--handed target)))))))))

(provide 'test-shipit-issues-region-actions)
;;; test-shipit-issues-region-actions.el ends here
