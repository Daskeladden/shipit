;;; test-shipit-review-queue.el --- The queue of draft review comments -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A draft review comment waits in the queue until the review is
;; submitted.  It shows under its diff line in Files Changed, and in the
;; Pending Review section with the diff lines around it.  The tests draw
;; Files Changed of a pull request in a real buffer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit-pr-sections)
(require 'shipit-buffer)
(require 'shipit-review-queue)
(require 'shipit-review-mode)
(require 'shipit-editor)
(require 'shipit-pr-github)
;; The commands that the menu of a page names, which shipit loads for a
;; user.
(require 'shipit-commands)
(require 'shipit-pr-actions)
(require 'shipit-repo-buffer)

(defconst test-review-queue--pr
  `((number . 1) (head (sha . "abc123") (ref . "work")) (base (ref . "main"))
    (files ((filename . "src/parser.el") (status . "modified")
            (additions . 1) (deletions . 0) (changes . 1)
            (patch . "@@ -1,3 +1,4 @@\n (defun parse ()\n+  (read-header)\n   (read-body))\n ;; end"))))
  "A pull request of one file, with a line added at line 2.")

(defconst test-review-queue--draft
  (list :id 1 :file "src/parser.el" :line 2 :side "RIGHT"
        :body "Read the header only once.")
  "A draft comment on the added line.")

(defun test-review-queue--open-files ()
  "Open the Files Changed section, as TAB on it does.  It starts closed."
  (save-excursion
    (goto-char (or (text-property-any (point-min) (point-max) 'shipit-pr-files t)
                   (error "No Files Changed section in the buffer")))
    (magit-section-show (magit-current-section))))

(defmacro test-review-queue--in-closed-pr-buffer (&rest body)
  "Run BODY in a pull request buffer that shows `test-review-queue--pr'.
The queue holds `test-review-queue--draft', and Files Changed is closed."
  (declare (indent 0))
  `(with-temp-buffer
     (shipit-mode)
     (setq shipit-buffer-repo "o/r"
           shipit-buffer-pr-number 1
           shipit-buffer-pr-data test-review-queue--pr
           shipit-review-queue (list (copy-sequence test-review-queue--draft)))
     (let ((inhibit-read-only t))
       (magit-insert-section (root)
         (shipit--render-files-section (cdr (assq 'files test-review-queue--pr))
                                       "o/r" 1 test-review-queue--pr "abc123")))
     ,@body))

(defmacro test-review-queue--in-pr-buffer (&rest body)
  "As `test-review-queue--in-closed-pr-buffer', with Files Changed open."
  (declare (indent 0))
  `(test-review-queue--in-closed-pr-buffer
     (test-review-queue--open-files)
     ,@body))

(ert-deftest test-review-queue-a-draft-shows-under-its-line ()
  "GIVEN a draft comment on line 2 of a file
WHEN the queue is drawn in Files Changed
THEN the draft shows under that line."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (should (= 1 (length shipit-review-queue--sections)))
    (should (string-match-p "(read-header)\n.*\n?.*Read the header only once"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-review-queue-a-draft-shows-the-code-it-is-on ()
  "GIVEN a draft comment on line 2 of a file
WHEN the Pending Review section asks for the code around it
THEN it gets the diff lines around line 2."
  (test-review-queue--in-pr-buffer
    (let ((context (shipit-review-queue--code-context "src/parser.el" 2)))
      (should context)
      (should (string-match-p "(read-header)" context)))))

(ert-deftest test-review-queue-the-code-of-a-draft-leaves-out-the-draft ()
  "GIVEN a draft comment on line 2 of a file, shown under that line
WHEN the Pending Review section asks for the code around it
THEN it gets the diff lines around line 2 without the text of the draft,
so the Pending Review shows that text once, below the code."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (let ((context (shipit-review-queue--code-context "src/parser.el" 2)))
      (should (string-match-p "(read-header)" context))
      (should (string-match-p "(read-body)" context))
      (should-not (string-match-p "Read the header only once" context)))))

(ert-deftest test-review-queue-the-code-of-a-draft-keeps-the-order-of-the-diff ()
  "GIVEN a draft comment on line 2 of a file
WHEN the Pending Review section asks for the code around it
THEN it gets two diff lines above line 2, the line and two below, in
the order of the diff."
  (test-review-queue--in-pr-buffer
    (should (equal '("@@ -1,3 +1,4 @@" "(defun parse ()" "+  (read-header)"
                     "(read-body))" ";; end")
                   (mapcar #'string-trim
                           (split-string (shipit-review-queue--code-context
                                          "src/parser.el" 2)
                                         "\n"))))))

(ert-deftest test-review-queue-each-line-of-a-draft-is-the-draft ()
  "GIVEN a draft comment shown under its line
WHEN point is on the text of the draft, below its heading
THEN the draft at point is that draft, so `c' and `k' act on it there
too, as the hint below the draft says."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (goto-char (point-min))
    (search-forward "Read the header only once")
    (should (equal 1 (plist-get (shipit-review-queue-at-point) :id)))))

(defun test-review-queue--draft-count ()
  "The number of times the text of the draft shows in this buffer."
  (how-many "Read the header only once" (point-min) (point-max)))

(ert-deftest test-review-queue-a-draft-shows-again-after-its-file-is-redrawn ()
  "GIVEN a draft comment shown under its line
WHEN the diff of its file is drawn again, as after a comment on the
file is posted
THEN the draft shows under its line again, once."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (shipit--refresh-file-inline-comment-section "src/parser.el" 1 "o/r")
    (should (= 1 (test-review-queue--draft-count)))
    (should (string-match-p "(read-header)\n.*\n?.*Read the header only once"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-review-queue-a-draft-lost-in-a-redraw-removes-no-diff ()
  "GIVEN a draft comment whose text a redraw of its file removed, with
nothing that draws the draft again
WHEN the queue is drawn again
THEN the diff of the file stays whole and the draft shows once, because
the queue removes no text that is no longer the draft."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (let ((shipit-file-diff-refreshed-hook nil))
      (shipit--refresh-file-inline-comment-section "src/parser.el" 1 "o/r"))
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (should (string-match-p "(read-body))" (buffer-substring-no-properties
                                            (point-min) (point-max))))
    (should (= 1 (test-review-queue--draft-count)))))

(ert-deftest test-review-queue-a-long-draft-wraps-in-pending-review ()
  "GIVEN a draft comment longer than a line
WHEN the Pending Review section is drawn
THEN the comment wraps below its heading, each line under the same indent."
  (test-review-queue--in-pr-buffer
    (setq shipit-review-queue
          (list (plist-put (copy-sequence test-review-queue--draft)
                           :body (mapconcat #'identity (make-list 30 "word") " "))))
    (let ((fill-column 60)
          (inhibit-read-only t))
      (goto-char (point-max))
      (let ((magit-insert-section--parent magit-root-section))
        (shipit-review-queue--insert-section-at-point)))
    (let ((lines (seq-filter (lambda (line) (string-match-p "\\` +word" line))
                             (split-string (buffer-substring-no-properties (point-min) (point-max))
                                           "\n"))))
      (should (< 1 (length lines)))
      (should (cl-every (lambda (line) (string-prefix-p "         word" line)) lines)))))

(ert-deftest test-review-queue-a-file-with-a-draft-is-marked-when-files-open ()
  "GIVEN a draft comment on a file, and Files Changed closed
WHEN Files Changed is opened
THEN the heading of the file is marked as having a draft."
  (test-review-queue--in-closed-pr-buffer
    (test-review-queue--open-files)
    (should (= 1 (length shipit-review-queue--badge-overlays)))))

(defun test-review-queue--press-k (answer)
  "Press `k' at point and give ANSWER to the question it asks."
  (let ((shipit-review-queue-persistence-file nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) answer)))
      (call-interactively (key-binding (kbd "k"))))))

(defun test-review-queue--goto-draft-text ()
  "Put point on the first text of the draft in this buffer."
  (goto-char (point-min))
  (search-forward "Read the header only once"))

(ert-deftest test-review-queue-the-hint-below-a-draft-names-k ()
  "GIVEN a draft comment shown under its line
WHEN the user reads the hint below it
THEN the hint says that `c' edits the draft and `k' removes it."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (should (string-match-p "`c' to edit the draft, `k' to remove it"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-review-queue-k-on-a-draft-removes-it ()
  "GIVEN a draft comment shown under its line
WHEN `k' is pressed on the text of the draft, and the question answered yes
THEN the draft leaves the queue and the buffer."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (test-review-queue--goto-draft-text)
    (test-review-queue--press-k t)
    (should-not shipit-review-queue)
    (should (= 0 (test-review-queue--draft-count)))))

(ert-deftest test-review-queue-k-on-a-draft-answered-no-keeps-it ()
  "GIVEN a draft comment shown under its line
WHEN `k' is pressed on the text of the draft, and the question answered no
THEN the draft stays, and `k' looks for no posted comment there."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (test-review-queue--goto-draft-text)
    (test-review-queue--press-k nil)
    (should (= 1 (length shipit-review-queue)))
    (should (= 1 (test-review-queue--draft-count)))))

(ert-deftest test-review-queue-k-in-pending-review-removes-the-draft ()
  "GIVEN a draft comment shown in the Pending Review section
WHEN `k' is pressed on the text of the draft there, and the question
answered yes
THEN the draft leaves the queue and the buffer."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((magit-insert-section--parent magit-root-section))
        (shipit-review-queue--insert-section-at-point)))
    (test-review-queue--goto-draft-text)
    (test-review-queue--press-k t)
    (should-not shipit-review-queue)
    (should (= 0 (test-review-queue--draft-count)))))

(ert-deftest test-review-queue-k-beside-a-draft-removes-nothing ()
  "GIVEN a draft comment shown under its line
WHEN `k' is pressed on the diff line above the draft
THEN it says there is no comment at point, and the draft stays."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (goto-char (point-min))
    (search-forward "(read-header)")
    (should (equal "No comment at point"
                   (cadr (should-error (test-review-queue--press-k t)
                                       :type 'user-error))))
    (should (= 1 (length shipit-review-queue)))))

;;; Discard all drafts with K

(defun test-review-queue--press-K (answer)
  "Press `K' in this buffer and give ANSWER to the question it asks.
Return the question, or nil when `K' asked none."
  (let ((shipit-review-queue-persistence-file nil)
        (question nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (prompt) (setq question prompt) answer)))
      (call-interactively (key-binding (kbd "K"))))
    question))

(defun test-review-queue--show-pending-review ()
  "Draw the Pending Review section at the end of this buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (let ((magit-insert-section--parent magit-root-section))
      (shipit-review-queue--insert-section-at-point))))

(defun test-review-queue--menu-command (menu key)
  "The command that KEY runs in the transient MENU."
  (plist-get (cdr (transient-get-suffix menu key)) :command))

(ert-deftest test-review-queue-K-discards-all-drafts ()
  "GIVEN a pull request buffer with a draft comment, shown under its line
WHEN `K' is pressed, and the question answered yes
THEN the queue is empty and the draft leaves the buffer."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (test-review-queue--press-K t)
    (should-not shipit-review-queue)
    (should (= 0 (test-review-queue--draft-count)))))

(ert-deftest test-review-queue-K-answered-no-keeps-the-drafts ()
  "GIVEN a pull request buffer with a draft comment, shown under its line
WHEN `K' is pressed, and the question answered no
THEN it asked whether to discard the one draft, and the draft stays in
the queue and in the buffer."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (should (equal "Discard 1 queued review comment? "
                   (test-review-queue--press-K nil)))
    (should (= 1 (length shipit-review-queue)))
    (should (= 1 (test-review-queue--draft-count)))))

(ert-deftest test-review-queue-D-still-shows-the-diff-with-comments ()
  "GIVEN a pull request buffer with a draft comment, opened after another
pull request buffer
WHEN the user looks up what `D' does there
THEN `D' shows the diff with comments, as before."
  (with-temp-buffer (shipit-mode))
  (test-review-queue--in-pr-buffer
    (should (eq #'shipit--show-diff-with-comments-at-point
                (key-binding (kbd "D"))))))

(ert-deftest test-review-queue-the-header-line-names-S-and-K ()
  "GIVEN a pull request buffer with a draft comment
WHEN the user reads the header line of the buffer
THEN it says `S' to submit and `K' to discard, and names no `D'."
  (test-review-queue--in-pr-buffer
    (shipit-review-queue--refresh-banner)
    (let ((header (substring-no-properties header-line-format)))
      (should (string-match-p "`S' to submit, `K' to discard" header))
      (should-not (string-match-p "`D'" header)))))

(ert-deftest test-review-queue-pending-review-names-S-and-K ()
  "GIVEN a pull request buffer with a draft comment
WHEN the user reads the hint at the top of the Pending Review section
THEN it says `S' to submit and `K' to discard, and names no `D'."
  (test-review-queue--in-pr-buffer
    (test-review-queue--show-pending-review)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Press `S' to submit, `K' to discard" text))
      (should-not (string-match-p "`D'" text)))))

(ert-deftest test-review-queue-the-menu-of-the-buffer-has-S-and-K ()
  "GIVEN the menu of a pull request buffer, which SPC opens
WHEN the user looks for the review drafts in it
THEN `S' submits the review and `K' discards the drafts."
  (should (eq #'shipit-review-submit
              (test-review-queue--menu-command 'shipit-pr-menu "S")))
  (should (eq #'shipit-review-queue-discard
              (test-review-queue--menu-command 'shipit-pr-menu "K"))))

(ert-deftest test-review-queue-the-menu-of-a-draft-discards-with-K ()
  "GIVEN the menu of a draft, which M-; on the draft opens
WHEN the user looks for the discard of all drafts in it
THEN `K' discards all drafts, and `D' is not in the menu."
  (should (eq #'shipit-review-queue-discard
              (test-review-queue--menu-command
               'shipit-review-queue-draft-actions "K")))
  (should-not (ignore-errors
                (transient-get-suffix 'shipit-review-queue-draft-actions "D"))))

;;; A draft written in review mode

(defun test-review-queue--make-clone ()
  "Make a git repository that holds `src/parser.el' of the pull request.
Return its directory."
  (let ((dir (file-name-as-directory (make-temp-file "shipit-review-clone-" t))))
    (make-directory (expand-file-name "src" dir))
    (with-temp-file (expand-file-name "src/parser.el" dir)
      (insert "(defun parse ()\n  (read-header)\n  (read-body))\n;; end\n"))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-q"))
    dir))

(defun test-review-queue--open-in-review-mode (clone repo-name)
  "Open `src/parser.el' of CLONE in review mode, on line 2.
The pull request data names the repository REPO-NAME, as the forge
answers it.  Return the buffer of the file."
  (let ((buffer (find-file-noselect (expand-file-name "src/parser.el" clone))))
    (with-current-buffer buffer
      (setq shipit-review--pr-data
            `((number . 1) (head (sha . "abc123"))
              (base (ref . "main") (repo (full_name . ,repo-name)))))
      (shipit-review-mode 1)
      (goto-char (point-min))
      (forward-line 1))
    buffer))

(defun test-review-queue--write-comment-with-c-a (text)
  "Press `c a' in the legend of review mode, write TEXT, press `C-c C-c'."
  (let ((shipit-review-queue-persistence-file nil))
    (with-current-buffer shipit-review--actions-buffer-name
      (call-interactively (key-binding (kbd "c a"))))
    (with-current-buffer shipit-editor--buffer-name
      (insert text)
      (call-interactively (key-binding (kbd "C-c C-c"))))))

(defun test-review-queue--draft-shown-below-line-p (buffer line text)
  "Non-nil when BUFFER shows a draft with TEXT at the end of LINE.
The draft is the text after an empty overlay there, which
`overlays-at' never returns, so this asks `overlays-in'."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (seq-some (lambda (overlay)
                  (string-match-p (regexp-quote text)
                                  (or (overlay-get overlay 'after-string) "")))
                (overlays-in (line-end-position) (line-end-position))))))

(defun test-review-queue--check-draft-from-review-mode (repo-name)
  "Write a draft with `c a' in review mode, where the forge names the
repository REPO-NAME, and check that it shows."
  (test-review-queue--in-closed-pr-buffer
    (setq shipit-review-queue nil)
    (let* ((pr-buffer (current-buffer))
           (clone (test-review-queue--make-clone))
           (file-buffer (test-review-queue--open-in-review-mode clone repo-name)))
      (unwind-protect
          (progn
            (test-review-queue--write-comment-with-c-a "Read the header once.")
            (should (equal '(("src/parser.el" 2))
                           (mapcar (lambda (entry)
                                     (list (plist-get entry :file)
                                           (plist-get entry :line)))
                                   (buffer-local-value 'shipit-review-queue
                                                       pr-buffer))))
            (should (test-review-queue--draft-shown-below-line-p
                     file-buffer 2 "Read the header once.")))
        (with-current-buffer file-buffer (shipit-review-mode -1))
        (kill-buffer file-buffer)
        (delete-directory clone t)))))

(ert-deftest test-review-queue-c-a-in-review-mode-shows-the-draft ()
  "GIVEN a pull request buffer of o/r, and a file of the pull request
open in review mode, on line 2
WHEN the user presses `c a', writes a comment and presses `C-c C-c'
THEN the draft is in the queue of the pull request, and it shows below
line 2 of the file."
  (test-review-queue--check-draft-from-review-mode "o/r"))

(ert-deftest test-review-queue-c-a-shows-the-draft-when-the-forge-uses-capitals ()
  "GIVEN a pull request buffer of o/r, made from the remote of the clone,
and a file of the pull request open in review mode, where the forge
names the repository O/R, as after a rename on GitHub
WHEN the user presses `c a', writes a comment and presses `C-c C-c'
THEN the draft is in the queue of the pull request, and it shows below
line 2 of the file, because a forge does not tell names apart by case."
  (test-review-queue--check-draft-from-review-mode "O/R"))

(defmacro test-review-queue--in-frame-of-size (columns lines &rest body)
  "Run BODY with the frame COLUMNS wide and LINES high, then size it back."
  (declare (indent 2))
  `(let ((old-columns (frame-width))
         (old-lines (frame-height)))
     (unwind-protect
         (progn (set-frame-size (selected-frame) ,columns ,lines)
                ,@body)
       (set-frame-size (selected-frame) old-columns old-lines))))

(defun test-review-queue--press (keys)
  "Press KEYS in the selected window, as the command loop runs a key.
The command loop makes the buffer of the selected window current
before the command, and again after it."
  (set-buffer (window-buffer (selected-window)))
  (call-interactively (key-binding (kbd keys)))
  (set-buffer (window-buffer (selected-window))))

(ert-deftest test-review-queue-c-a-opens-an-editor-with-room-to-write ()
  "GIVEN a file of a pull request open in review mode, in a frame of 40
lines, with the legend of review mode at the bottom of the frame
WHEN the user presses `c a' in the legend
THEN the editor shows at least 5 lines of text, so the user sees what
they type, and the legend stays at the bottom."
  (test-review-queue--in-frame-of-size 120 40
    (test-review-queue--in-closed-pr-buffer
      (let* ((clone (test-review-queue--make-clone))
             (file-buffer (test-review-queue--open-in-review-mode clone "o/r"))
             (legend (get-buffer shipit-review--actions-buffer-name)))
        (unwind-protect
            (progn
              (select-window (get-buffer-window legend))
              (call-interactively (key-binding (kbd "c a")))
              (should (<= 5 (window-body-height
                             (get-buffer-window shipit-editor--buffer-name))))
              (should (eq 'bottom (window-parameter (get-buffer-window legend)
                                                    'window-side))))
          (when (get-buffer shipit-editor--buffer-name)
            (with-current-buffer shipit-editor--buffer-name
              (shipit-editor-cancel)))
          (with-current-buffer file-buffer (shipit-review-mode -1))
          (kill-buffer file-buffer)
          (delete-directory clone t))))))

(ert-deftest test-review-queue-after-c-c-c-c-the-legend-has-the-focus-again ()
  "GIVEN a file of a pull request open in review mode in the main window,
and the legend of review mode selected
WHEN the user presses `c a', writes a comment and presses `C-c C-c'
THEN the legend is selected again, so the next key of review mode
works, and the file is not changed."
  (test-review-queue--in-frame-of-size 120 40
    (test-review-queue--in-closed-pr-buffer
      (setq shipit-review-queue nil)
      (let* ((clone (test-review-queue--make-clone))
             (file-buffer (test-review-queue--open-in-review-mode clone "o/r"))
             (legend (get-buffer shipit-review--actions-buffer-name)))
        (unwind-protect
            (let ((shipit-review-queue-persistence-file nil))
              (set-window-buffer (window-main-window) file-buffer)
              (select-window (get-buffer-window legend))
              (test-review-queue--press "c a")
              (insert "Read the header once.")
              (test-review-queue--press "C-c C-c")
              (should (eq legend (window-buffer (selected-window))))
              (should-not (buffer-modified-p file-buffer)))
          (when (get-buffer shipit-editor--buffer-name)
            (with-current-buffer shipit-editor--buffer-name
              (shipit-editor-cancel)))
          (with-current-buffer file-buffer (shipit-review-mode -1))
          (kill-buffer file-buffer)
          (delete-directory clone t))))))

(defun test-review-queue--said-since (start regexp)
  "Non-nil when *Messages* has a line that matches REGEXP after START."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char start)
      (re-search-forward regexp nil t))))

(ert-deftest test-review-queue-two-drafts-in-review-mode-say-nothing-odd ()
  "GIVEN a file of a pull request open in review mode, on line 2
WHEN the user writes two drafts there, each with `c a' and `C-c C-c'
THEN both drafts show below line 2, and shipit never says \"File
section not visible\", which is about the pull request buffer."
  (test-review-queue--in-closed-pr-buffer
    (setq shipit-review-queue nil)
    (let* ((clone (test-review-queue--make-clone))
           (file-buffer (test-review-queue--open-in-review-mode clone "o/r"))
           (start (with-current-buffer (messages-buffer) (point-max))))
      (unwind-protect
          (progn
            (test-review-queue--write-comment-with-c-a "Read the header once.")
            (test-review-queue--write-comment-with-c-a "And the body too.")
            (should (test-review-queue--draft-shown-below-line-p
                     file-buffer 2 "Read the header once."))
            (should (test-review-queue--draft-shown-below-line-p
                     file-buffer 2 "And the body too."))
            (should-not (test-review-queue--said-since start "File section not visible")))
        (with-current-buffer file-buffer (shipit-review-mode -1))
        (kill-buffer file-buffer)
        (delete-directory clone t)))))

(defun test-review-queue--menu-lines ()
  "Return the number of lines of the open menu, and of its window."
  (let ((buffer (get-buffer transient--buffer-name)))
    (cons (with-current-buffer buffer
            (count-lines (point-min) (point-max)))
          (window-body-height (get-buffer-window buffer)))))

(ert-deftest test-review-queue-a-menu-shows-all-its-lines-above-the-legend ()
  "GIVEN a file of a pull request open in review mode, in a frame of 40
lines, with the legend of review mode at the bottom of the frame, and
the page of the pull request in the main window
WHEN the user opens the menu of the page with SPC
THEN the window of the menu shows each line of the menu, and the legend
stays at the bottom."
  (test-review-queue--in-frame-of-size 120 40
    (test-review-queue--in-closed-pr-buffer
      (let* ((page (current-buffer))
             (clone (test-review-queue--make-clone))
             (file-buffer (test-review-queue--open-in-review-mode clone "o/r"))
             (legend (get-buffer shipit-review--actions-buffer-name)))
        (unwind-protect
            (progn
              (set-window-buffer (window-main-window) page)
              (select-window (window-main-window))
              (test-review-queue--press "SPC")
              (let ((lines (test-review-queue--menu-lines)))
                (should (<= (car lines) (cdr lines))))
              (should (eq 'bottom (window-parameter (get-buffer-window legend)
                                                    'window-side))))
          (transient--emergency-exit)
          (with-current-buffer file-buffer (shipit-review-mode -1))
          (kill-buffer file-buffer)
          (delete-directory clone t))))))

(provide 'test-shipit-review-queue)
;;; test-shipit-review-queue.el ends here
