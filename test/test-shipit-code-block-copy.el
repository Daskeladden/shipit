;;; test-shipit-code-block-copy.el --- Copy the code of a code block -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; C-u M-w on a line of a code block in a shipit page copies the code of
;; the block, without the lines of its fences and without the indent
;; that the page gives the block.  Plain M-w copies as it did before.
;; Embark sees the block as a target, and shipit-dwim offers to copy it.
;;
;; The issue is one of the Jira sandbox, opened as by hand.  The comment
;; of a pull request is drawn by the functions of a pull request buffer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-core)
(require 'shipit-buffer)
(require 'shipit-pr-sections)
;; M-w of a pull request buffer copies the address of a check.
(require 'shipit-actions)
(require 'test-shipit-jira-sandbox)

(defvar embark-target-finders)
(defvar embark-keymap-alist)

;;; The pages

(defconst test-code-block-copy--code
  "def brew(cups):\n    water = cups * 2\n\n    return water\n"
  "The code of the block in the Jira issue, as it is written.")

(defconst test-code-block-copy--jira-description
  (concat "Brew the coffee like this:\n\n{code:python}\n"
          test-code-block-copy--code
          "{code}\n\nThen pour it.")
  "The description of the Jira issue, in the wiki markup that Jira reads.")

(defun test-code-block-copy--open-jira-issue ()
  "Start the Jira sandbox, give its first issue the description with code; open it."
  (let ((config (shipit-jira-sandbox-start)))
    (shipit-issue-jira--update-description
     config "PRJ-1" test-code-block-copy--jira-description)
    (shipit-issues-open-buffer "PRJ-1" shipit-jira-sandbox-repo)))

(defmacro test-code-block-copy--copying (&rest body)
  "Run BODY with an empty kill ring that does not reach the clipboard.
A mark that is active shows as a region, as it does for the user."
  `(let ((kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil)
         (interprogram-paste-function nil)
         (transient-mark-mode t))
     ,@body))

(defmacro test-code-block-copy--with-jira-issue (page &rest body)
  "Open a Jira issue whose description holds a code block; run BODY.
PAGE is bound to the buffer of the issue.  No login but the sandbox's
is there."
  (declare (indent 1))
  `(progn
     (skip-unless (executable-find "python3"))
     (let ((auth-sources nil))
       (test-code-block-copy--copying
        (test-jira-sandbox--with-page-opened-by
            (test-code-block-copy--open-jira-issue) ,page
          (with-current-buffer ,page
            ,@body))))))

(defconst test-code-block-copy--pr-code
  "for cup in 1 2; do\n  pour \"$cup\"\ndone\n"
  "The code of the block in the comment on the pull request.")

(defun test-code-block-copy--pr-comment ()
  "A general comment on a pull request, whose body holds a code block."
  `((id . 1)
    (body . ,(concat "Try this:\n\n```sh\n" test-code-block-copy--pr-code
                     "```\n\nIt works here."))
    (user (login . "someone"))
    (shipit-comment-type . "issue")
    (created_at . "2026-01-01T10:00:00Z")))

(defmacro test-code-block-copy--with-pr-comment (&rest body)
  "Run BODY in a pull request buffer that shows a comment with a code block.
The section of the general comments is unfolded.  No avatar and no
reaction is asked for."
  (declare (indent 0))
  `(test-code-block-copy--copying
    (with-temp-buffer
      (shipit-mode)
      (let ((inhibit-read-only t)
            (shipit-show-avatars nil)
            (shipit--reaction-cache (make-hash-table :test 'equal)))
        (setq shipit-buffer-repo "o/r"
              shipit-buffer-pr-number 7
              shipit-current-repo "o/r")
        (magit-insert-section (root)
          (shipit--insert-general-comments-section-placeholder "o/r" 7))
        (shipit--replace-general-comments-section-with-content
         "o/r" 7 (list (test-code-block-copy--pr-comment)))
        (magit-section-show (shipit--find-section-by-type 'general-comments))
        ,@body))))

;;; What the user does

(defun test-code-block-copy--goto (text)
  "Put point at the start of TEXT in this buffer."
  (goto-char (point-min))
  (should (search-forward text nil t))
  (goto-char (match-beginning 0)))

(defun test-code-block-copy--mark (text)
  "Mark TEXT in this buffer as a region, with point at its end."
  (test-code-block-copy--goto text)
  (push-mark (point) t t)
  (goto-char (+ (point) (length text))))

(defun test-code-block-copy--press (keys &optional prefix)
  "Run the command that KEYS is bound to at point, with PREFIX as its argument.
PREFIX is (4) for C-u.  Not `execute-kbd-macro': its command loop looks
keys up in the buffer of the selected window, which need not be this
one in a batch run."
  (let ((current-prefix-arg prefix))
    (call-interactively (key-binding (kbd keys) t))))

(defun test-code-block-copy--menus-of (keys pick)
  "Press KEYS; the choices of each menu that it shows, in order.
In the first menu the user picks PICK, or leaves the menu with C-g when
PICK is nil."
  (let* ((menus nil)
         (completing-read-function
          (lambda (_prompt collection &rest _)
            (push (all-completions "" collection) menus)
            (or pick (signal 'quit nil)))))
    (condition-case nil
        (test-code-block-copy--press keys)
      (quit nil))
    (nreverse menus)))

(defun test-code-block-copy--line ()
  "The line at point, as the buffer shows it."
  (string-trim-right
   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

;;; The scenarios

(ert-deftest test-code-block-copy-c-u-m-w-copies-the-code-of-a-jira-issue ()
  "GIVEN a Jira issue whose description holds a code block, whose lines
have an indent of their own and one of them is blank
WHEN the user presses C-u M-w on a line inside the block
THEN the code is copied without the lines of the fences and without the
indent of the description, AND each line keeps its own indent, AND the
copy ends with a newline."
  (test-code-block-copy--with-jira-issue page
    (test-code-block-copy--goto "water = cups")
    (test-code-block-copy--press "M-w" '(4))
    (should (equal test-code-block-copy--code (current-kill 0)))))

(ert-deftest test-code-block-copy-m-w-copies-a-marked-part-of-the-code ()
  "GIVEN a Jira issue whose description holds a code block
WHEN the user marks a part of a line in the block and presses M-w
THEN only the marked part is copied, as M-w copies anywhere else."
  (test-code-block-copy--with-jira-issue page
    (test-code-block-copy--mark "cups * 2")
    (test-code-block-copy--press "M-w")
    (should (equal "cups * 2" (current-kill 0)))))

(ert-deftest test-code-block-copy-c-u-m-w-copies-the-code-of-a-pull-request-comment ()
  "GIVEN a comment on a pull request whose body holds a code block
WHEN the user presses C-u M-w on a line inside the block
THEN the code is copied without the lines of the fences and without the
indent of the comment
WHEN the user presses M-w on that line
THEN the line is copied, as M-w copies a line of a pull request today."
  (test-code-block-copy--with-pr-comment
    (test-code-block-copy--goto "pour \"$cup\"")
    (test-code-block-copy--press "M-w" '(4))
    (should (equal test-code-block-copy--pr-code (current-kill 0)))
    (test-code-block-copy--press "M-w")
    (should (equal (test-code-block-copy--line) (current-kill 0)))
    (should (string-suffix-p "pour \"$cup\"" (current-kill 0)))))

(ert-deftest test-code-block-copy-embark-sees-the-code-block ()
  "GIVEN a comment on a pull request whose body holds a code block, AND
embark, which finds a word at any point
WHEN embark asks for the target on a line inside the block
THEN its first target is the code block, AND the text of the target is
the code without the fences and without the indent, which embark's w
copies
WHEN embark asks on a line of the comment outside the block
THEN the code block is no target there."
  (let ((embark-target-finders (list (lambda () '(word "said"))))
        (embark-keymap-alist nil))
    (shipit--code-block-embark-register)
    (test-code-block-copy--with-pr-comment
      (test-code-block-copy--goto "pour \"$cup\"")
      (let ((target (run-hook-with-args-until-success 'embark-target-finders)))
        (should (eq 'shipit-code-block (car target)))
        (should (equal test-code-block-copy--pr-code (cadr target))))
      (test-code-block-copy--goto "It works here")
      (should (equal '(word "said")
                     (run-hook-with-args-until-success 'embark-target-finders))))))

(ert-deftest test-code-block-copy-shipit-dwim-offers-to-copy-the-code-first ()
  "GIVEN a Jira issue whose description holds a code block
WHEN the user calls shipit-dwim with M-; on a line of the description
outside the block
THEN the menu shows the actions on a description
WHEN the user calls it on a line inside the block
THEN the menu shows Copy code block first, and then the same actions
WHEN the user picks Copy code block
THEN the code is copied without the lines of the fences and without the
indent of the description."
  (test-code-block-copy--with-jira-issue page
    (test-code-block-copy--goto "Then pour it")
    (let ((outside (car (test-code-block-copy--menus-of "M-;" nil))))
      (should (member "Edit description" outside))
      (test-code-block-copy--goto "water = cups")
      (should (equal (cons "Copy code block" outside)
                     (car (test-code-block-copy--menus-of "M-;" "Copy code block"))))
      (should (equal test-code-block-copy--code (current-kill 0))))))

;;; The flash of a copy

(defvar pulse-momentary-overlay)
(defvar shipit-code-block-copy-functions)

(defun test-code-block-copy--lines-of-code (first last)
  "The start of the line of FIRST and the end of the line of LAST, as a cons.
The end is the start of the next line, so the region holds whole lines."
  (save-excursion
    (cons (progn (test-code-block-copy--goto first) (line-beginning-position))
          (progn (test-code-block-copy--goto last) (line-beginning-position 2)))))

(defun test-code-block-copy--pulsed ()
  "The region that Emacs's pulse highlights now, as a cons, or nil."
  (and (overlayp pulse-momentary-overlay)
       (overlay-buffer pulse-momentary-overlay)
       (cons (overlay-start pulse-momentary-overlay)
             (overlay-end pulse-momentary-overlay))))

(defun test-code-block-copy--face-shown ()
  "The face that shows at point: that of the overlay of the highest priority."
  (get-char-property (point) 'face))

(ert-deftest test-code-block-copy-the-copied-lines-flash ()
  "GIVEN a comment on a pull request whose body holds a code block with a
background color, AND the setting of what a copy of a code block runs
as it comes
WHEN the user presses C-u M-w on a line inside the block
THEN the lines of the code flash with Emacs's pulse, AND the pulse
shows over the background of the block, AND the lines of the fences do
not flash."
  (let ((pulse-momentary-overlay nil)
        (shipit-code-block-background "#272727"))
    (test-code-block-copy--with-pr-comment
      (test-code-block-copy--goto "pour \"$cup\"")
      (test-code-block-copy--press "M-w" '(4))
      (should (equal (test-code-block-copy--lines-of-code "for cup in" "done")
                     (test-code-block-copy--pulsed)))
      (should (eq 'pulse-highlight-start-face (test-code-block-copy--face-shown))))))

(ert-deftest test-code-block-copy-a-function-of-the-user-flashes-instead ()
  "GIVEN a comment on a pull request whose body holds a code block, AND a
function of the user's own in place of the pulse, in the setting of
what a copy of a code block runs
WHEN the user presses C-u M-w on a line inside the block
THEN the function gets the start and the end of the lines of the code,
AND Emacs's pulse does not flash."
  (let* ((pulse-momentary-overlay nil)
         (got nil)
         (shipit-code-block-copy-functions
          (list (lambda (start end) (push (cons start end) got)))))
    (test-code-block-copy--with-pr-comment
      (test-code-block-copy--goto "pour \"$cup\"")
      (test-code-block-copy--press "M-w" '(4))
      (should (equal (list (test-code-block-copy--lines-of-code "for cup in" "done"))
                     got))
      (should-not (test-code-block-copy--pulsed)))))

(ert-deftest test-code-block-copy-copy-code-block-of-shipit-dwim-flashes-too ()
  "GIVEN a Jira issue whose description holds a code block, AND the
setting of what a copy of a code block runs as it comes
WHEN the user picks Copy code block in the menu of M-; on a line inside
the block
THEN the lines of the code flash with Emacs's pulse."
  (let ((pulse-momentary-overlay nil))
    (test-code-block-copy--with-jira-issue page
      (test-code-block-copy--goto "water = cups")
      (test-code-block-copy--menus-of "M-;" "Copy code block")
      (should (equal (test-code-block-copy--lines-of-code "def brew" "return water")
                     (test-code-block-copy--pulsed))))))

(provide 'test-shipit-code-block-copy)
;;; test-shipit-code-block-copy.el ends here
