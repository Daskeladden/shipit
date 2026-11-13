;;; test-shipit-issues-row-actions.el --- Act on the rows of an issue list -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A row of a list of issues that names its issue is a target of embark
;; of its own, ahead of the issue key at its start.  It offers to open,
;; to browse, to copy the web address, to copy the key and to change the
;; state, each for the repository that the row names.  In other text an
;; issue key stays a target as before.
;;
;; Embark is not on the load path of the tests.  The tests call the
;; target finders at point and the actions the way embark calls them:
;; one call with the list of the rows.  The browser and the kill ring
;; are replaced, and no test needs an answer of a tracker.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'browse-url)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-jira)
(require 'shipit-issues)
(require 'shipit-code-refs)

;; Embark is not on the load path of the tests.  The tests bind these
;; the way embark keeps them.
(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-multitarget-actions)
(defvar embark-default-action-overrides)
(defvar embark-general-map)
(defvar embark-candidate-collectors)

;;; The trackers

(defconst test-row-actions--jira-url "https://acme.atlassian.net"
  "The address of the Jira site of acme/door.")

(defmacro test-row-actions--with-trackers (&rest body)
  "Run BODY with the issues of acme/door in Jira and all others at GitHub.
No entry names a tracker of its own for another repository, so its
issues are at GitHub."
  (declare (indent 0))
  `(let ((shipit-issue-backend 'github)
         (shipit-issue-backend-config nil)
         (shipit-issue-repo-backends
          (list (list "acme/door" :backend 'jira
                      :base-url test-row-actions--jira-url
                      :project-keys '("PRJ")))))
     ,@body))

(defun test-row-actions--jira-page (key)
  "The web address of the Jira issue KEY of acme/door."
  (format "%s/browse/%s" test-row-actions--jira-url key))

;;; The rows of an issue list

(defun test-row-actions--issue (number)
  "Issue NUMBER as a search of the tracker finds it."
  `((number . ,number) (title . "Oil the hinge")))

(defun test-row-actions--all-rows (repo numbers)
  "The rows that the issue search of REPO shows for the issues NUMBERS.
They come from the completion table of the search, as embark takes
them from its minibuffer, in the order of the table."
  (all-completions "" (shipit-issues--make-completion-table
                       (mapcar #'test-row-actions--issue numbers)
                       repo
                       (make-hash-table :test 'equal)
                       nil)))

(defun test-row-actions--row-of (rows number)
  "The row of ROWS that names the issue NUMBER.  Fails when there is none."
  (or (seq-find (lambda (row)
                  (equal number (get-text-property 0 'shipit-issue-number row)))
                rows)
      (ert-fail (list "No row names the issue" number rows))))

(defun test-row-actions--rows (repo numbers)
  "The rows of the issues NUMBERS of REPO, in the order of NUMBERS.
The table of the search sorts its rows itself, so each row is looked
up by the issue that it names."
  (let ((rows (test-row-actions--all-rows repo numbers)))
    (mapcar (lambda (number) (test-row-actions--row-of rows number))
            numbers)))

;;; Buffers and point

(defmacro test-row-actions--in-buffer-of (repo &rest body)
  "Run BODY in an empty buffer that is about REPO."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local shipit-buffer-repo ,repo)
     ,@body))

(defun test-row-actions--insert-export (rows)
  "Insert ROWS as an export of embark shows them.
Each row keeps its text properties and has a note after it that has
none, as the annotation of a candidate does."
  (dolist (row rows)
    (insert row)
    (insert "  To Do\n")))

(defun test-row-actions--goto-key (key)
  "Put point at the start of the first KEY in the buffer.
Fails when the buffer does not show KEY."
  (goto-char (point-min))
  (unless (search-forward key nil t)
    (ert-fail (list "The buffer does not show" key (buffer-string))))
  (goto-char (match-beginning 0)))

(defun test-row-actions--text-of (target)
  "The text of the buffer that TARGET, as (KIND VALUE START . END), covers."
  (pcase-let ((`(,_kind ,_value ,start . ,end) target))
    (buffer-substring-no-properties start end)))

;;; The actions, as embark runs them

(defun test-row-actions--act (key rows)
  "Run the action on KEY of a row of the issue search on ROWS, as embark does.
Embark hands the action all ROWS in one call.  Fails when KEY has no
action."
  (let ((action (lookup-key shipit-issues-embark-map (kbd key))))
    (unless (functionp action)
      (ert-fail (list "A row of the issue search has no action on" key action)))
    (funcall action rows)))

(defun test-row-actions--copied-by (key rows)
  "What the action on KEY puts in the kill ring when it runs on ROWS.
The kill ring is one of its own, and the clipboard of the system is
left alone."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil))
    (test-row-actions--act key rows)
    (or (car kill-ring)
        (ert-fail (list "The action put nothing in the kill ring" key)))))

(defun test-row-actions--lines (text)
  "The lines of TEXT.  A newline at its end starts no line of its own."
  (split-string (string-remove-suffix "\n" text) "\n"))

(defun test-row-actions--browsed-by (key rows)
  "The web addresses that the action on KEY shows in the browser for ROWS.
They are in the order in which the browser got them."
  (let* ((browsed nil)
         (browse-url-handlers nil)
         (browse-url-browser-function (lambda (url &rest _) (push url browsed))))
    (test-row-actions--act key rows)
    (reverse browsed)))

(defun test-row-actions--bindings (keys)
  "Each of KEYS of the actions on a row, as (KEY . COMMAND)."
  (mapcar (lambda (key) (cons key (lookup-key shipit-issues-embark-map (kbd key))))
          keys))

;;; The scenarios

(ert-deftest test-shipit-issues-row-actions-a-row-is-the-target-at-point ()
  "On a row of an issue list, the row is the target and not the key."
  ;; GIVEN a buffer that shows the rows of PRJ-1 and PRJ-2 of acme/door,
  ;; as an export of embark does, and point on the row of PRJ-2
  ;; WHEN embark asks shipit for the target at point
  ;; THEN the row finder gives a target of the kind `shipit-issue' that
  ;; covers exactly the text of that row
  ;; AND the copy-key action on that target copies PRJ-2, so the target
  ;; is the row and names its issue
  ;; AND the issue key finder gives nothing there
  (test-row-actions--with-trackers
    (let* ((rows (test-row-actions--rows "acme/door" '("PRJ-1" "PRJ-2")))
           (row (test-row-actions--row-of rows "PRJ-2")))
      (with-temp-buffer
        (test-row-actions--insert-export rows)
        (test-row-actions--goto-key "PRJ-2")
        (let ((target (shipit-issues-embark-target)))
          (should (eq 'shipit-issue (car target)))
          (should (equal (substring-no-properties row)
                         (test-row-actions--text-of target)))
          (should (equal "PRJ-2"
                         (test-row-actions--copied-by "w" (list (cadr target))))))
        (should-not (shipit-code-refs-embark-target))))))

(ert-deftest test-shipit-issues-row-actions-a-key-in-text-stays-a-target ()
  "In plain text an issue key is still the target, and no row is."
  ;; GIVEN a buffer about acme/door with the plain text
  ;; "see PRJ-2 for the door", and point on the key
  ;; WHEN embark asks shipit for the target at point
  ;; THEN the issue key finder gives the key PRJ-2, as before
  ;; AND the row finder gives nothing
  (test-row-actions--with-trackers
    (test-row-actions--in-buffer-of "acme/door"
      (insert "see PRJ-2 for the door")
      (test-row-actions--goto-key "PRJ-2")
      (let ((target (shipit-code-refs-embark-target)))
        (should (eq 'shipit-issue-ref (car target)))
        (should (equal "PRJ-2" (cadr target))))
      (should-not (shipit-issues-embark-target)))))

(ert-deftest test-shipit-issues-row-actions-browse-shows-the-page-of-each-row ()
  "Browse on rows shows the page of each issue, in the repository of its row."
  ;; GIVEN the rows of PRJ-1 and PRJ-2 of acme/door, whose issues are in
  ;; Jira
  ;; AND a current buffer that is about another repository, whose issues
  ;; are at GitHub
  ;; WHEN b runs on both rows
  ;; THEN the browser shows the Jira pages of PRJ-1 and of PRJ-2, and
  ;; nothing else
  (test-row-actions--with-trackers
    (test-row-actions--in-buffer-of "other/place"
      (should (equal (list (test-row-actions--jira-page "PRJ-1")
                           (test-row-actions--jira-page "PRJ-2"))
                     (sort (test-row-actions--browsed-by
                            "b" (test-row-actions--rows
                                 "acme/door" '("PRJ-1" "PRJ-2")))
                           #'string<))))))

(ert-deftest test-shipit-issues-row-actions-copy-url-copies-the-address-of-each-row ()
  "Copy URL on rows copies the web address of each issue, one per line."
  ;; GIVEN the rows of PRJ-1 and PRJ-2 of acme/door, whose issues are in
  ;; Jira
  ;; AND a current buffer that is about another repository, whose issues
  ;; are at GitHub
  (test-row-actions--with-trackers
    (test-row-actions--in-buffer-of "other/place"
      ;; WHEN u runs on the row of PRJ-1
      ;; THEN the kill ring holds the Jira address of PRJ-1
      (should (equal (test-row-actions--jira-page "PRJ-1")
                     (test-row-actions--copied-by
                      "u" (test-row-actions--rows "acme/door" '("PRJ-1")))))
      ;; WHEN u runs on the rows of PRJ-1 and PRJ-2, in this order
      ;; THEN the kill ring holds both addresses, one per line, in the
      ;; order of the rows
      (should (equal (list (test-row-actions--jira-page "PRJ-1")
                           (test-row-actions--jira-page "PRJ-2"))
                     (test-row-actions--lines
                      (test-row-actions--copied-by
                       "u" (test-row-actions--rows
                            "acme/door" '("PRJ-1" "PRJ-2")))))))))

(ert-deftest test-shipit-issues-row-actions-copy-key-copies-the-key-of-each-row ()
  "Copy key on rows copies the key of each issue, as its tracker names it."
  ;; GIVEN the row of PRJ-1 of acme/door, whose issues are in Jira
  ;; WHEN w runs on it
  ;; THEN the kill ring holds PRJ-1
  (test-row-actions--with-trackers
    (should (equal "PRJ-1"
                   (test-row-actions--copied-by
                    "w" (test-row-actions--rows "acme/door" '("PRJ-1")))))
    ;; GIVEN the rows of the GitHub issues 12 and 13 of acme/window
    ;; WHEN w runs on both, in this order
    ;; THEN the kill ring holds #12 and #13, one per line, in the order
    ;; of the rows
    (should (equal '("#12" "#13")
                   (test-row-actions--lines
                    (test-row-actions--copied-by
                     "w" (test-row-actions--rows "acme/window" '(12 13))))))))

(ert-deftest test-shipit-issues-row-actions-embark-offers-the-row-and-its-actions ()
  "Embark asks for a row first, and a row offers its five actions on all rows."
  ;; GIVEN embark with a target finder of its own already in its list
  ;; WHEN the issue search sets itself up for embark
  ;; THEN the row finder is the first of embark's target finders
  ;; AND o, b, u, w and s on a row open, browse, copy the address, copy
  ;; the key and change the state
  ;; AND embark hands all rows to each of the five in one call
  (let ((embark-general-map (make-sparse-keymap))
        (embark-target-finders (list 'embark-target-url-at-point))
        (embark-keymap-alist nil)
        (embark-multitarget-actions nil)
        (embark-default-action-overrides nil)
        (embark-candidate-collectors nil)
        (actions '(("o" . shipit-issues-embark-open)
                   ("b" . shipit-issues-embark-browse)
                   ("u" . shipit-issues-embark-copy-url)
                   ("w" . shipit-issues-embark-copy-key)
                   ("s" . shipit-issues-embark-change-status))))
    (unwind-protect
        (progn
          (shipit-issues--embark-setup)
          (should (eq 'shipit-issues-embark-target (car embark-target-finders)))
          (should (equal actions
                         (test-row-actions--bindings (mapcar #'car actions))))
          (should-not (seq-difference (mapcar #'cdr actions)
                                      embark-multitarget-actions)))
      (set-keymap-parent shipit-issues-embark-map nil))))

(provide 'test-shipit-issues-row-actions)
;;; test-shipit-issues-row-actions.el ends here
