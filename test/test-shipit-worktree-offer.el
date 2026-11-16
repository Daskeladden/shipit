;;; test-shipit-worktree-offer.el --- C-RET offers a worktree for review mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; C-RET on a file or on a line of a diff in a pull request page opens
;; that file.  When the pull request has no worktree and the clone is
;; not at its head, C-RET asks whether to make one.  Yes makes it in
;; the background, shows the progress in the Worktree section, and then
;; opens the file from the worktree in review mode, on the line of the
;; diff.  No opens the file from the clone, as before.
;;
;; The page is opened as by hand, and GitHub is answered at url.el.
;; The git commands are real.  The forge is a bare repository on disk,
;; where the pull request exists only as refs/pull/8/head.  The clone
;; names its remote by the address of GitHub, and git rewrites that
;; address to the forge, so the fetch that shipit runs stays on disk.
;; Only the question and the echo area are stood in for.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'shipit-worktree)
(require 'shipit-review-mode)
(require 'test-shipit-page-views)

;;; The kitchen: a forge, a seed and a clone, all on disk

(defconst test-worktree-offer--github-address "https://github.com/o/r.git"
  "The address of o/r at GitHub, which git rewrites to the forge.")

(defconst test-worktree-offer--git-environment
  '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
  "Keep the git settings of whoever runs the tests out of the kitchen.")

(defvar test-worktree-offer--kitchen nil
  "The directory of the scenario, with the seed, the forge and the clone.")

(defun test-worktree-offer--path (name)
  "NAME in the kitchen."
  (expand-file-name name test-worktree-offer--kitchen))

(defun test-worktree-offer--worktree ()
  "The directory of the worktree of pull request 8, in the clone."
  (test-worktree-offer--path "clone/.worktrees/pr-8-coffee/"))

(defun test-worktree-offer--in-worktree (file)
  "FILE in the worktree of pull request 8."
  (expand-file-name file (test-worktree-offer--worktree)))

(defun test-worktree-offer--in-clone (file)
  "FILE in the clone."
  (expand-file-name file (test-worktree-offer--path "clone/")))

(defun test-worktree-offer--git (where &rest args)
  "Run git with ARGS in WHERE, a directory of the kitchen; return its output.
Signal when git fails."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory (test-worktree-offer--path where)))
    (unless (zerop (apply #'process-file "git" nil t nil args))
      (error "git %s failed in %s: %s" (string-join args " ") where (buffer-string)))
    (string-trim (buffer-string))))

(defun test-worktree-offer--sha (branch)
  "The commit of BRANCH in the seed."
  (test-worktree-offer--git "seed" "rev-parse" branch))

(defun test-worktree-offer--commit (text message)
  "Commit TEXT as brew.sh in the seed, with MESSAGE."
  (with-temp-file (test-worktree-offer--path "seed/brew.sh")
    (insert text))
  (test-worktree-offer--git "seed" "add" "brew.sh")
  (test-worktree-offer--git "seed" "commit" "-q" "-m" message))

(defun test-worktree-offer--build-seed ()
  "Make the seed: main fills the pot, and coffee boils two cups and pours."
  (make-directory (test-worktree-offer--path "seed"))
  (test-worktree-offer--git "seed" "init" "-q" "-b" "main")
  (test-worktree-offer--git "seed" "config" "user.email" "cook@example.com")
  (test-worktree-offer--git "seed" "config" "user.name" "Cook")
  (test-worktree-offer--commit "fill the pot\n" "Fill the pot")
  (test-worktree-offer--git "seed" "checkout" "-q" "-b" "coffee")
  (test-worktree-offer--commit "fill the pot\nboil two cups\npour\n" "Boil two cups"))

(defun test-worktree-offer--build-forge ()
  "Make the forge: main as a branch, and coffee only as refs/pull/8/head."
  (test-worktree-offer--git "." "init" "-q" "--bare" "-b" "main" "forge.git")
  (test-worktree-offer--git "forge.git" "fetch" "-q" (test-worktree-offer--path "seed")
                            "main:refs/heads/main" "coffee:refs/pull/8/head"))

(defun test-worktree-offer--build-clone ()
  "Clone main of the forge, without the commits of pull request 8.
The clone names its remote by the address of GitHub, and git rewrites
that address to the forge."
  (test-worktree-offer--git "." "clone" "-q" "--no-local" "-b" "main"
                            (test-worktree-offer--path "forge.git") "clone")
  (test-worktree-offer--git "clone" "config" "remote.origin.url"
                            test-worktree-offer--github-address)
  (test-worktree-offer--git "clone" "config"
                            (format "url.%s.insteadOf" (test-worktree-offer--path "forge.git"))
                            test-worktree-offer--github-address))

(defun test-worktree-offer--build-kitchen ()
  "Make the seed, the forge and the clone."
  (test-worktree-offer--build-seed)
  (test-worktree-offer--build-forge)
  (test-worktree-offer--build-clone))

;;; What each scenario is given, before the page opens

(defun test-worktree-offer--clone-on-main ()
  "Leave the clone on main, and pull request 8 without a worktree."
  nil)

(defun test-worktree-offer--fetch-the-pr ()
  "Fetch the head of pull request 8 from the forge into the clone."
  (test-worktree-offer--git "clone" "fetch" "-q" (test-worktree-offer--path "forge.git")
                            "refs/pull/8/head"))

(defun test-worktree-offer--worktree-at (sha)
  "Give pull request 8 a worktree at SHA."
  (test-worktree-offer--git "clone" "worktree" "add" "-q" ".worktrees/pr-8-coffee" sha))

(defun test-worktree-offer--worktree-at-the-pr-head ()
  "Give pull request 8 a worktree at its head."
  (test-worktree-offer--fetch-the-pr)
  (test-worktree-offer--worktree-at (test-worktree-offer--sha "coffee")))

(defun test-worktree-offer--worktree-behind-the-pr-head ()
  "Give pull request 8 a worktree at main, one commit behind its head."
  (test-worktree-offer--worktree-at (test-worktree-offer--sha "main")))

(defun test-worktree-offer--clone-at-the-pr-head ()
  "Check the clone out at the head of pull request 8."
  (test-worktree-offer--fetch-the-pr)
  (test-worktree-offer--git "clone" "checkout" "-q" "-b" "coffee" "FETCH_HEAD"))

(defun test-worktree-offer--forge-without-the-pr ()
  "Take pull request 8 away from the forge."
  (test-worktree-offer--git "forge.git" "update-ref" "-d" "refs/pull/8/head"))

;;; Pull request 8, as GitHub gives it

(defconst test-worktree-offer--pr
  '(:title "Boil two cups" :checks (("boil water" . "success")))
  "Pull request 8 of o/r: one check that passed, and nothing to discuss.")

(defconst test-worktree-offer--files
  [((filename . "brew.sh") (status . "modified") (sha . "f1")
    (additions . 2) (deletions . 0) (changes . 2)
    (patch . "@@ -1 +1,3 @@\n fill the pot\n+boil two cups\n+pour"))]
  "The change of pull request 8 as GitHub gives it, the same as in the seed.")

(defun test-worktree-offer--pull-request ()
  "Pull request 8 as GitHub gives it, with the head and the base of the seed."
  `((head (sha . ,(test-worktree-offer--sha "coffee")) (ref . "coffee")
          (repo (full_name . "o/r")))
    (base (sha . ,(test-worktree-offer--sha "main")) (ref . "main")
          (repo (full_name . "o/r")))
    ,@(seq-remove (lambda (field) (memq (car field) '(head base)))
                  (test-page-views--pull-request 8 test-worktree-offer--pr))))

(defun test-worktree-offer--github ()
  "GitHub's answers about pull request 8.
The first answer whose pattern matches wins, so the pull request and its
files here come before those of the page views."
  (append (list (list "/repos/o/r/pulls/8\\'" 200 (test-worktree-offer--pull-request))
                (list "/pulls/8/files" 200 test-worktree-offer--files))
          (test-page-views--github (list (cons 8 test-worktree-offer--pr)))))

;;; The question and the echo area

(defconst test-worktree-offer--question
  "No worktree for PR #8. Create one for review mode?"
  "What C-RET asks when pull request 8 has no worktree.")

(defconst test-worktree-offer--warning "differs from PR head"
  "What the warning says when the file may not be the one of the diff.")

(defvar test-worktree-offer--answers nil
  "The answers still to give, `yes' or `no', one for each question.")

(defvar test-worktree-offer--questions nil
  "The questions asked so far, oldest first.")

(defvar test-worktree-offer--echoed nil
  "What the echo area said so far, newest first.")

(defun test-worktree-offer--ask (prompt)
  "Stand-in for `y-or-n-p': note PROMPT and give the next answer."
  (setq test-worktree-offer--questions
        (append test-worktree-offer--questions (list (string-trim prompt))))
  (eq (pop test-worktree-offer--answers) 'yes))

(defun test-worktree-offer--echo (format-string &rest args)
  "Stand-in for `message': note the text of FORMAT-STRING and ARGS."
  (when format-string
    (car (push (apply #'format-message format-string args)
               test-worktree-offer--echoed))))

;;; A scenario, from its kitchen to its clean-up

(defvar test-worktree-offer--page nil
  "The page of pull request 8.")

(defun test-worktree-offer--open-page ()
  "Open the page of pull request 8 from the clone, as by hand."
  (let ((default-directory (test-worktree-offer--path "clone/")))
    (setq test-worktree-offer--page (test-page-views--open-pr 8))))

(defun test-worktree-offer--kill (buffers)
  "Kill each of BUFFERS that is still alive."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun test-worktree-offer--visiting (directory)
  "The buffers that visit a file under DIRECTORY."
  (let ((root (file-name-as-directory (file-truename directory))))
    (seq-filter (lambda (buffer)
                  (let ((file (buffer-file-name buffer)))
                    (and file (string-prefix-p root (file-truename file)))))
                (buffer-list))))

(defun test-worktree-offer--stop-processes (before)
  "Stop each process that is not one of BEFORE, and kill its buffer."
  (dolist (process (process-list))
    (unless (memq process before)
      (let ((buffer (process-buffer process)))
        (delete-process process)
        (test-worktree-offer--kill (list buffer))))))

(defun test-worktree-offer--clean-up (processes)
  "Remove what a scenario left; PROCESSES are those that ran before it."
  (test-worktree-offer--stop-processes processes)
  (test-worktree-offer--kill (list test-worktree-offer--page
                                   (get-buffer shipit-review--actions-buffer-name)))
  (test-worktree-offer--kill (test-worktree-offer--visiting test-worktree-offer--kitchen))
  (delete-directory test-worktree-offer--kitchen t))

(defun test-worktree-offer--in-a-new-kitchen (scenario)
  "Run SCENARIO, a function, in a new kitchen with nothing left from earlier ones.
Remove all it leaves, and give the windows back as they were."
  (let ((test-worktree-offer--kitchen
         (file-name-as-directory (make-temp-file "shipit-kitchen-" t)))
        (test-worktree-offer--page nil)
        (test-worktree-offer--answers nil)
        (test-worktree-offer--questions nil)
        (test-worktree-offer--echoed nil)
        (process-environment (append test-worktree-offer--git-environment
                                     process-environment))
        (shipit-worktree--progress (make-hash-table :test 'equal))
        (shipit--repo-from-remote-cache (make-hash-table :test 'equal))
        (shipit--repo-root-cache (make-hash-table :test 'equal))
        (shipit--detect-backend-cache (make-hash-table :test 'equal))
        (shipit-review--saved-window-config nil)
        (processes (process-list)))
    (unwind-protect
        (save-window-excursion
          (funcall scenario))
      (test-worktree-offer--clean-up processes))))

(defun test-worktree-offer--on-the-page (given scenario)
  "Run SCENARIO on the page of pull request 8, in a kitchen that GIVEN prepared.
GIVEN and SCENARIO are functions.  GIVEN runs before the page opens."
  (test-worktree-offer--in-a-new-kitchen
   (lambda ()
     (test-worktree-offer--build-kitchen)
     (funcall given)
     (test-page-views--as-me
       (shipit-test-http-answering (test-worktree-offer--github)
         (cl-letf (((symbol-function 'y-or-n-p) #'test-worktree-offer--ask)
                   ((symbol-function 'message) #'test-worktree-offer--echo))
           (test-worktree-offer--open-page)
           (funcall scenario)))))))

(defmacro test-worktree-offer--on-the-page-of-pr-8 (given &rest body)
  "Run BODY on the page of pull request 8, in a kitchen that GIVEN prepared.
GIVEN is a function that prepares the forge or the clone."
  (declare (indent 1))
  `(test-worktree-offer--on-the-page ,given (lambda () ,@body)))

;;; Waiting

(defun test-worktree-offer--run-until (done-p seconds)
  "Let processes and timers run until DONE-P holds or SECONDS passed.
Return what DONE-P gave last."
  (let ((until (+ (float-time) seconds))
        (done nil))
    (while (and (not (setq done (funcall done-p)))
                (< (float-time) until))
      (accept-process-output nil 0.05))
    done))

(defun test-worktree-offer--wait-for (what done-p)
  "Let processes and timers run until DONE-P holds; return what it gives.
Fail, naming WHAT, after ten seconds."
  (or (test-worktree-offer--run-until done-p 10)
      (ert-fail (list "Waited ten seconds in vain for" what))))

(defun test-worktree-offer--let-a-moment-pass ()
  "Let processes and timers run for 0.4 seconds.
That is twice the delay of the warning that the file may differ."
  (test-worktree-offer--run-until #'ignore 0.4))

(defun test-worktree-offer--warned-p ()
  "Whether the echo area warned that the file may not be the one of the diff.
The warning comes with a delay, so this lets a moment pass first."
  (test-worktree-offer--let-a-moment-pass)
  (test-worktree-offer--echo-area-said-p test-worktree-offer--warning))

;;; What I do on the page

(defun test-worktree-offer--has (position properties)
  "Whether the text of the current buffer at POSITION has PROPERTIES, a plist."
  (cl-loop for (property value) on properties by #'cddr
           always (equal (get-text-property position property) value)))

(defun test-worktree-offer--position-with (&rest properties)
  "The first position of the page whose text has each of PROPERTIES, a plist."
  (with-current-buffer test-worktree-offer--page
    (let ((position (point-min)))
      (while (and position (not (test-worktree-offer--has position properties)))
        (setq position (next-property-change position)))
      (or position (ert-fail (cons "The page has no text with" properties))))))

(defun test-worktree-offer--unfold-at (position)
  "Unfold the section at POSITION of the page, as TAB there does."
  (with-current-buffer test-worktree-offer--page
    (magit-section-show (magit-section-at position))))

(defun test-worktree-offer--file-line (file)
  "Where the page names FILE among the changed files, which are unfolded first."
  (test-worktree-offer--unfold-at (test-worktree-offer--position-with 'shipit-pr-files t))
  (test-worktree-offer--position-with 'shipit-pr-file t 'shipit-file-path file))

(defun test-worktree-offer--diff-line (file line)
  "Where the diff of FILE shows its LINE; the diff is unfolded first."
  (test-worktree-offer--unfold-at (test-worktree-offer--file-line file))
  (test-worktree-offer--position-with 'shipit-file-path file 'shipit-line-number line))

(defun test-worktree-offer--press-c-ret-at (position answer)
  "Press C-RET at POSITION of the page, shown in my window; ANSWER if asked.
ANSWER is `yes' or `no'.  Nothing runs after the key, so what the page
shows at once can be read."
  (setq test-worktree-offer--answers (and answer (list answer)))
  (switch-to-buffer test-worktree-offer--page)
  (goto-char position)
  (let ((command (key-binding (kbd "C-<return>") t)))
    (should (commandp command))
    (call-interactively command)))

(defun test-worktree-offer--press-c-ret-on-line (line &optional answer)
  "Press C-RET on LINE of the diff of brew.sh; ANSWER the question if it comes."
  (test-worktree-offer--press-c-ret-at (test-worktree-offer--diff-line "brew.sh" line)
                                       answer))

(defun test-worktree-offer--press-c-ret-on-file (&optional answer)
  "Press C-RET on brew.sh among the changed files; ANSWER the question if it comes."
  (test-worktree-offer--press-c-ret-at (test-worktree-offer--file-line "brew.sh") answer))

;;; What I see

(defun test-worktree-offer--asked (times)
  "Check that the question came TIMES times, and no other question."
  (should (equal (make-list times test-worktree-offer--question)
                 test-worktree-offer--questions)))

(defun test-worktree-offer--echo-area-said-p (text)
  "Whether the echo area said something with TEXT in it."
  (seq-some (lambda (said) (string-search text said)) test-worktree-offer--echoed))

(defun test-worktree-offer--worktree-section ()
  "What the Worktree section of the page shows: its text and the lines before it."
  (with-current-buffer test-worktree-offer--page
    (let* ((section (magit-section-at
                     (test-worktree-offer--position-with 'shipit-worktree t)))
           (start (oref section start))
           (end (oref section end)))
      (concat (buffer-substring-no-properties start end)
              (mapconcat (lambda (overlay) (or (overlay-get overlay 'before-string) ""))
                         (overlays-in start end) "")))))

(defun test-worktree-offer--window-of (file)
  "The live window that shows FILE, or nil."
  (let ((wanted (file-truename file)))
    (seq-find (lambda (window)
                (let ((shown (buffer-file-name (window-buffer window))))
                  (and shown (equal (file-truename shown) wanted))))
              (window-list-1 nil 'nomini t))))

(defun test-worktree-offer--shown (file)
  "The window that shows FILE, once one does; ten seconds at most."
  (test-worktree-offer--wait-for (list "a window that shows" file)
                                 (lambda () (test-worktree-offer--window-of file))))

(defun test-worktree-offer--in-review-mode-p (file)
  "Whether FILE is shown in review mode, once it is shown."
  (with-current-buffer (window-buffer (test-worktree-offer--shown file))
    (bound-and-true-p shipit-review-mode)))

(defun test-worktree-offer--line-at-point (file)
  "The line of FILE where the point of the window that shows it is."
  (let ((window (test-worktree-offer--shown file)))
    (with-current-buffer (window-buffer window)
      (line-number-at-pos (window-point window)))))

;;; The scenarios

(ert-deftest test-worktree-offer-yes-opens-the-new-worktree-in-review-mode ()
  "Yes to the offer makes the worktree and opens the file from it in review mode."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on main
  ;; WHEN I press C-RET on line 2 of the diff of brew.sh and answer yes
  ;;   to "No worktree for PR #8. Create one for review mode?"
  ;; THEN the Worktree section shows that the worktree is being made
  ;; AND when it is ready, brew.sh of the new worktree is shown, in
  ;;   review mode, with point on line 2
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--clone-on-main
    (test-worktree-offer--press-c-ret-on-line 2 'yes)
    (test-worktree-offer--asked 1)
    (should (string-match-p "Fetching PR ref\\|Creating worktree"
                            (test-worktree-offer--worktree-section)))
    (let ((brew (test-worktree-offer--in-worktree "brew.sh")))
      (should (test-worktree-offer--in-review-mode-p brew))
      (should (= 2 (test-worktree-offer--line-at-point brew))))))

(ert-deftest test-worktree-offer-yes-after-i-left-the-page-only-says-it-is-ready ()
  "A worktree that gets ready after I left the page opens no file and says it is ready."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on main
  ;; WHEN I press C-RET on brew.sh, answer yes, and switch to another
  ;;   buffer before the worktree is ready
  ;; THEN that buffer stays in my window, no file of the worktree is open
  ;; AND the echo area says "Worktree for PR #8 is ready"
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--clone-on-main
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-worktree-offer--asked 1)
    (let ((elsewhere (switch-to-buffer "*scratch*")))
      (test-worktree-offer--wait-for
       "the echo area to say that the worktree is ready"
       (lambda () (test-worktree-offer--echo-area-said-p "Worktree for PR #8 is ready")))
      (test-worktree-offer--let-a-moment-pass)
      (should (eq elsewhere (window-buffer (selected-window))))
      (should-not (test-worktree-offer--visiting (test-worktree-offer--worktree))))))

(ert-deftest test-worktree-offer-no-opens-the-clone-and-asks-again ()
  "No to the offer opens the file of the clone, and the next C-RET asks again."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on main
  ;; WHEN I press C-RET on brew.sh and answer no
  ;; THEN brew.sh of the clone opens, not in review mode, with the
  ;;   warning that the working tree differs from the PR head
  ;; AND no worktree is made
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--clone-on-main
    (test-worktree-offer--press-c-ret-on-file 'no)
    (test-worktree-offer--asked 1)
    (should-not (test-worktree-offer--in-review-mode-p (test-worktree-offer--in-clone "brew.sh")))
    (should (test-worktree-offer--warned-p))
    (should-not (file-exists-p (test-worktree-offer--worktree)))
    ;; WHEN I press C-RET on brew.sh again
    ;; THEN the question comes again
    (test-worktree-offer--press-c-ret-on-file 'no)
    (test-worktree-offer--asked 2)))

(ert-deftest test-worktree-offer-yes-without-the-pr-on-the-forge-says-it-failed ()
  "A worktree that cannot be made opens no file, and the page and the echo area say so."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is on main
  ;; AND a forge that does not have the commits of the pull request
  ;; WHEN I press C-RET on brew.sh and answer yes
  ;; THEN no file opens, the Worktree section shows that it failed
  ;; AND the echo area says "Could not make the worktree for PR #8"
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--forge-without-the-pr
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (test-worktree-offer--asked 1)
    (test-worktree-offer--wait-for
     "the echo area to say that the worktree could not be made"
     (lambda () (test-worktree-offer--echo-area-said-p "Could not make the worktree for PR #8")))
    (should-not (test-worktree-offer--visiting test-worktree-offer--kitchen))
    (should (string-match-p "Failed" (test-worktree-offer--worktree-section)))))

(ert-deftest test-worktree-offer-a-worktree-at-the-pr-head-opens-in-review-mode ()
  "A worktree at the head of the pull request opens in review mode, with no question."
  ;; GIVEN the page of pull request 8, whose worktree exists and is at
  ;;   the PR head
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of the worktree is shown in review mode
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--worktree-at-the-pr-head
    (test-worktree-offer--press-c-ret-on-file)
    (test-worktree-offer--asked 0)
    (should (test-worktree-offer--in-review-mode-p (test-worktree-offer--in-worktree "brew.sh")))))

(ert-deftest test-worktree-offer-a-clone-at-the-pr-head-opens-with-no-question ()
  "A clone at the head of the pull request opens the plain file, with no question."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   that is checked out at the PR head
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of the clone opens as a plain file, with no warning
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--clone-at-the-pr-head
    (test-worktree-offer--press-c-ret-on-file)
    (test-worktree-offer--asked 0)
    (should-not (test-worktree-offer--in-review-mode-p (test-worktree-offer--in-clone "brew.sh")))
    (should-not (test-worktree-offer--warned-p))))

(ert-deftest test-worktree-offer-a-worktree-behind-the-pr-head-opens-with-a-warning ()
  "A worktree behind the head of the pull request opens the plain file, with a warning."
  ;; GIVEN the page of pull request 8, whose worktree exists but is
  ;;   behind the PR head
  ;; WHEN I press C-RET on brew.sh
  ;; THEN no question comes
  ;; AND brew.sh of that worktree opens as a plain file, with the warning
  ;;   that the working tree differs from the PR head
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--worktree-behind-the-pr-head
    (test-worktree-offer--press-c-ret-on-file)
    (test-worktree-offer--asked 0)
    (should-not (test-worktree-offer--in-review-mode-p (test-worktree-offer--in-worktree "brew.sh")))
    (should (test-worktree-offer--warned-p))))

(defun test-worktree-offer--clone-with-a-gitignore ()
  "Commit a .gitignore on main of the clone, and leave the clone on main."
  (with-temp-file (test-worktree-offer--in-clone ".gitignore")
    (insert "*.log\n"))
  (test-worktree-offer--git "clone" "add" ".gitignore")
  (test-worktree-offer--git "clone" "-c" "user.email=cook@example.com" "-c" "user.name=Cook"
                            "commit" "-q" "-m" "Ignore the logs"))

(ert-deftest test-worktree-offer-a-new-worktree-leaves-the-clone-unchanged ()
  "A worktree that shipit makes changes no file that the clone tracks."
  ;; GIVEN the page of pull request 8, which has no worktree, in a clone
  ;;   on main, whose .gitignore is committed
  ;; WHEN I press C-RET on brew.sh, answer yes, and the worktree is ready
  ;; THEN git status of the clone shows no change
  ;; AND git ignores the folder of the worktrees in the clone
  (test-worktree-offer--on-the-page-of-pr-8 #'test-worktree-offer--clone-with-a-gitignore
    (test-worktree-offer--press-c-ret-on-file 'yes)
    (should (test-worktree-offer--in-review-mode-p (test-worktree-offer--in-worktree "brew.sh")))
    (should (equal "" (test-worktree-offer--git "clone" "status" "--porcelain")))
    (should (test-worktree-offer--git "clone" "check-ignore" ".worktrees/"))))

(provide 'test-shipit-worktree-offer)
;;; test-shipit-worktree-offer.el ends here
