;;; test-shipit-explore-buffer.el --- Tests for the Explore buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the buffer that shows the feed of what the people that the
;; user follows do.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'shipit-explore-buffer)
(require 'shipit-pr-github)
(require 'shipit-notifications-external)
;; The buffers that an entry opens.  The Explore buffer loads them when
;; an entry is opened, which would take the place of what a test put
;; there.
(require 'shipit-repo-buffer)
(require 'shipit-releases-buffer)
;; The command of L.  shipit.el loads it for a user.
(require 'shipit-commands)
(require 'shipit-test-http)
(require 'shipit-test-meow)
(require 'test-shipit-explore)

(defmacro test-explore-buffer--shown (answers &rest body)
  "Run BODY in the Explore buffer, opened with GitHub giving ANSWERS.
There are no SVG icons.  The window is left as it was, and the buffer
is killed afterwards."
  (declare (indent 1))
  `(test-explore--fresh ,answers
     (let ((shipit-use-svglib-icons nil))
       (save-window-excursion
         (shipit-explore)
         (unwind-protect
             (with-current-buffer "*shipit-explore*"
               ,@body)
           (kill-buffer "*shipit-explore*"))))))

(defun test-explore-buffer--column (column)
  "The text of COLUMN of the line at point, and the column where it starts."
  (let ((start (text-property-any (line-beginning-position) (line-end-position)
                                  'shipit-explore-column column)))
    (should start)
    (let ((end (or (text-property-not-all start (line-end-position)
                                          'shipit-explore-column column)
                   (line-end-position))))
      (cons (string-trim (buffer-substring-no-properties start end))
            (save-excursion (goto-char start) (current-column))))))

(defun test-explore-buffer--each-line (function)
  "What FUNCTION gives on each line of this buffer that shows an entry."
  (let ((given nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'shipit-explore-entry)
          (push (funcall function) given))
        (forward-line 1)))
    (nreverse given)))

(defun test-explore-buffer--lines ()
  "The entries that this buffer shows, each as its repository, what was done
and its kind."
  (test-explore-buffer--each-line
   (lambda ()
     (mapcar (lambda (column) (car (test-explore-buffer--column column)))
             '(repo subject kind)))))

(defun test-explore-buffer--times ()
  "The time that each line of this buffer shows."
  (test-explore-buffer--each-line
   (lambda () (car (test-explore-buffer--column 'time)))))

(defun test-explore-buffer--line-ends ()
  "The column where each line of this buffer ends."
  (test-explore-buffer--each-line
   (lambda () (save-excursion (end-of-line) (current-column)))))

(defun test-explore-buffer--go-to (words)
  "Put point on the line of the entry that holds WORDS."
  (goto-char (point-min))
  (should (re-search-forward (regexp-quote words) nil t))
  (beginning-of-line))

(ert-deftest test-explore-buffer-shows-who-did-what-in-columns ()
  "GIVEN the feed of GitHub with one entry of each kind, AND no SVG icons
WHEN Explore is opened
THEN its buffer has a line for each entry, the newest first, that names
the repository, who did what, and the kind of the entry, AND each line
starts with a sign for the kind."
  (test-explore-buffer--shown (test-explore--answers)
    (should (derived-mode-p 'shipit-explore-mode))
    (should (equal (test-explore-buffer--lines)
                   '(("dave/once-private" "dave made public" "public")
                     ("example/tool" "alice released v1.2.0" "release")
                     ("carol/new-thing" "carol created" "create")
                     ("example/tool" "bob forked to bob/tool" "fork")
                     ("example/tool" "alice starred" "star"))))
    (should (equal (test-explore-buffer--each-line
                    (lambda () (string (char-after (line-beginning-position)))))
                   '("🌐" "🚀" "📦" "🍴" "⭐")))))

(ert-deftest test-explore-buffer-its-columns-line-up-as-in-the-notifications-buffer ()
  "GIVEN column widths for the notifications buffer
WHEN Explore is opened
THEN each column starts at the same place on every line, AND the
repository is as wide as the setting says, AND the times end at the same
place, at the right."
  (let ((shipit-notifications-column-widths '((repo . 24) (pr . 4) (title . 40) (reason . 10)))
        (shipit-notifications-column-spacing 2))
    (test-explore-buffer--shown (test-explore--answers)
      (dolist (column '(repo subject kind))
        (should (= 1 (length (delete-dups
                              (test-explore-buffer--each-line
                               (lambda () (cdr (test-explore-buffer--column column)))))))))
      (let ((repo (car (test-explore-buffer--each-line
                        (lambda () (cdr (test-explore-buffer--column 'repo))))))
            (subject (car (test-explore-buffer--each-line
                           (lambda () (cdr (test-explore-buffer--column 'subject)))))))
        (should (= (- subject repo) (+ 24 2))))
      (should (= 1 (length (delete-dups (test-explore-buffer--line-ends))))))))

(defun test-explore-buffer--dates-p ()
  "Whether each line of this buffer shows its time as a date."
  (seq-every-p (lambda (time)
                 (string-match-p "\\`2026-[0-9]+-[0-9]+ [0-9]+:[0-9]+\\'" time))
               (test-explore-buffer--times)))

(defun test-explore-buffer--press-l-twice (dates)
  "Press L twice in this buffer, which shows dates when DATES, else how long ago.
Each time the times change their form, and the lines end where they
ended."
  (let ((ends (test-explore-buffer--line-ends)))
    (should (eq dates (test-explore-buffer--dates-p)))
    (dolist (now (list (not dates) dates))
      (call-interactively (key-binding (kbd "L")))
      (should (= 5 (length (test-explore-buffer--times))))
      (should (eq now (test-explore-buffer--dates-p)))
      (should (equal ends (test-explore-buffer--line-ends))))))

(ert-deftest test-explore-buffer-l-switches-the-form-of-the-times ()
  "GIVEN the Explore buffer of a feed, with each time as a date
WHEN L is pressed, the key of the notifications buffer for it
THEN each line says how long ago its entry was, AND the lines end where
they ended before
WHEN L is pressed again
THEN each line shows the date again, AND the lines end at the same place
still."
  (let ((shipit-timestamp-format 'absolute))
    (test-explore-buffer--shown (test-explore--answers)
      (should (eq (key-binding (kbd "L")) #'shipit-toggle-timestamp-format))
      (test-explore-buffer--press-l-twice t))))

(ert-deftest test-explore-buffer-l-keeps-the-lines-when-the-times-get-longer ()
  "GIVEN the Explore buffer of a feed that says how long ago each entry
was, which is shorter than a date
WHEN L is pressed
THEN each line shows the date, AND the lines end where they ended before
WHEN L is pressed again
THEN each line says how long ago again, AND the lines end at the same
place still."
  (let ((shipit-timestamp-format 'relative))
    (test-explore-buffer--shown (test-explore--answers)
      (test-explore-buffer--press-l-twice nil))))

(ert-deftest test-explore-buffer-has-the-faces-of-the-notifications-buffer ()
  "GIVEN the Explore buffer
THEN on a line the repository has the face of a repository there, who
did it the face of a user, the kind the face of a reason, AND the time
the face of a time."
  (test-explore-buffer--shown (test-explore--answers)
    (test-explore-buffer--go-to "alice starred")
    (let ((face-at (lambda (column &optional words)
                     (save-excursion
                       (goto-char (text-property-any (line-beginning-position)
                                                     (line-end-position)
                                                     'shipit-explore-column column))
                       (when words (search-forward words) (backward-char 1))
                       (get-text-property (point) 'font-lock-face)))))
      (should (eq (funcall face-at 'repo) 'font-lock-constant-face))
      (should (eq (funcall face-at 'subject "alice") 'shipit-username-face))
      (should (eq (funcall face-at 'kind) 'font-lock-keyword-face))
      (should (eq (funcall face-at 'time) 'shipit-timestamp-face)))))

(ert-deftest test-explore-buffer-has-octicons-with-svg-icons ()
  "GIVEN SVG icons
WHEN Explore is opened
THEN each line has an image: the octicon of its kind."
  (let ((asked nil))
    (test-explore--fresh (test-explore--answers)
      ;; `features' is not special, so only `dlet' binds it for `featurep'.
      ;; shipit keeps each icon that it made: with a table of its own, the
      ;; icons of this test are not what a later test finds there.
      (dlet ((shipit-use-svglib-icons t)
             (shipit--svg-lib-icon-cache (make-hash-table :test 'equal))
             (features (cons 'svg-lib features)))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'svg-lib-icon)
                   (lambda (name &rest _)
                     (push name asked)
                     '(image :type svg :data "<svg/>"))))
          (save-window-excursion
            (shipit-explore)
            (unwind-protect
                (with-current-buffer "*shipit-explore*"
                  (test-explore-buffer--go-to "alice starred")
                  (should (cl-some (lambda (pos) (imagep (get-text-property pos 'display)))
                                   (number-sequence (line-beginning-position)
                                                    (line-end-position)))))
              (kill-buffer "*shipit-explore*"))))))
    (dolist (octicon '("star-fill" "repo-forked" "repo" "rocket" "globe"))
      (should (member octicon asked)))))

(ert-deftest test-explore-buffer-an-entry-opens-in-shipit ()
  "GIVEN the Explore buffer
WHEN RET is pressed on the line of a star
THEN shipit opens its page of the repository
WHEN RET is pressed on the line of a release
THEN shipit opens the releases of the repository at the tag."
  (let ((opened nil))
    (cl-letf (((symbol-function 'shipit-open-repo-buffer)
               (lambda (repo &rest _) (push (list 'repo repo) opened)))
              ((symbol-function 'shipit-open-releases-buffer)
               (lambda (repo &optional tag &rest _) (push (list 'releases repo tag) opened))))
      (test-explore-buffer--shown (test-explore--answers)
        (should (eq (key-binding (kbd "RET")) #'shipit-explore-open))
        (test-explore-buffer--go-to "alice starred")
        (shipit-explore-open)
        (test-explore-buffer--go-to "alice released v1.2.0")
        (shipit-explore-open)))
    (should (equal (reverse opened)
                   '((repo "example/tool") (releases "example/tool" "v1.2.0"))))))

(defun test-explore-buffer--new-marks ()
  "Whether each line of this buffer is marked as new, the first line first."
  (test-explore-buffer--each-line
   (lambda () (and (get-text-property (point) 'shipit-explore-new) t))))

(ert-deftest test-explore-buffer-marks-what-is-new-and-then-it-is-seen ()
  "GIVEN a feed whose three oldest entries were seen, AND the notification
of the two that are new
WHEN Explore is opened
THEN the lines of the two new entries are marked as new, AND the other
three are not, AND all entries count as seen from then on, AND the
notification is gone
WHEN the feed is read again in the buffer
THEN the two lines are still marked as new: they are new to this visit
WHEN Explore is opened again
THEN no line is marked as new."
  (test-explore--fresh (test-explore--answers)
    (shipit-explore-mark-seen (last (test-explore--fetch) 3))
    (shipit-explore--check)
    (should (equal (test-explore--notifications) '(("Explore" "2 new in the feed"))))
    (save-window-excursion
      (shipit-explore)
      (unwind-protect
          (with-current-buffer "*shipit-explore*"
            (should (equal (test-explore-buffer--new-marks) '(t t nil nil nil)))
            (should-not (shipit-explore-unseen (test-explore--fetch)))
            (should-not (test-explore--notifications))
            (shipit-explore-refresh)
            (should (equal (test-explore-buffer--new-marks) '(t t nil nil nil)))
            (shipit-explore)
            (should (equal (test-explore-buffer--new-marks) '(nil nil nil nil nil))))
        (kill-buffer "*shipit-explore*")))))

(ert-deftest test-explore-buffer-g-reads-the-feed-again ()
  "GIVEN the Explore buffer of a feed
WHEN the feed has one more entry, and g is pressed
THEN the buffer shows that entry first."
  (test-explore--fresh nil
    (let ((shipit-use-svglib-icons nil))
      (save-window-excursion
        (shipit-test-http-answering (test-explore--answers)
          (shipit-explore))
        (unwind-protect
            (with-current-buffer "*shipit-explore*"
              (should (eq (key-binding (kbd "g")) #'shipit-explore-refresh))
              (shipit-test-http-answering
                  (test-explore--answers
                   (cons (test-explore--event "9" "WatchEvent" "erin" "example/other"
                                              '((action . "started")) "2026-10-01T11:00:00Z")
                         (test-explore--events)))
                (shipit-explore-refresh))
              (should (equal (car (test-explore-buffer--lines))
                             '("example/other" "erin starred" "star"))))
          (kill-buffer "*shipit-explore*"))))))

(ert-deftest test-explore-buffer-an-empty-feed-says-so ()
  "GIVEN a feed without an entry
WHEN Explore is opened
THEN the buffer says that the feed is empty."
  (test-explore-buffer--shown
      (list test-explore--user '("/users/the-reader/received_events" 200 "[]"))
    (should-not (test-explore-buffer--lines))
    (should (string-match-p "Nothing in the feed" (buffer-string)))))

;;; The menu

(ert-deftest test-explore-buffer-spc-opens-a-menu-of-what-is-shown ()
  "GIVEN the Explore buffer
THEN SPC opens its menu, as in a canvas buffer, AND so does f, as in the
notifications buffer, AND the menu has a key for each kind of entry, one
for whose repositories the entries are about, one that reads the feed
again, AND SPC again closes it."
  (test-explore-buffer--shown (test-explore--answers)
    (should (eq (key-binding (kbd "SPC")) #'shipit-explore-menu))
    (should (eq (key-binding (kbd "f")) #'shipit-explore-menu)))
  (dolist (key '("s" "f" "c" "r" "p" "a" "g"))
    (should (transient-get-suffix 'shipit-explore-menu key)))
  (should (eq (plist-get (cdr (transient-get-suffix 'shipit-explore-menu "SPC")) :command)
              #'transient-quit-one)))

(ert-deftest test-explore-buffer-a-kind-is-switched-off-and-on ()
  "GIVEN the Explore buffer with an entry of each kind
WHEN the stars are switched off, as the menu does
THEN the buffer shows no star, AND the setting names the other four kinds
WHEN they are switched on again
THEN the star is back."
  (test-explore-buffer--shown (test-explore--answers)
    (shipit-explore-toggle-stars)
    (should-not (member "star" shipit-explore-kinds))
    (should (= 4 (length shipit-explore-kinds)))
    (should (equal (mapcar #'caddr (test-explore-buffer--lines))
                   '("public" "release" "create" "fork")))
    (shipit-explore-toggle-stars)
    (should (equal (mapcar #'caddr (test-explore-buffer--lines))
                   '("public" "release" "create" "fork" "star")))))

(ert-deftest test-explore-buffer-meow-leaves-spc-to-the-menu ()
  "GIVEN meow, which takes SPC for its keypad in its motion state
WHEN meow loads
THEN it starts an Explore buffer in its insert state, where SPC reaches
the menu."
  (shipit-test-with-meow-loaded
    (should (eq 'insert (alist-get 'shipit-explore-mode meow-mode-state-list)))))

(ert-deftest test-explore-buffer-the-menu-says-how-each-switch-stands ()
  "GIVEN a setting that shows no fork, AND entries about all repositories
THEN the menu says that forks are off and stars are on, AND that the
entries are about all repositories."
  (let ((shipit-explore-kinds '("star" "create" "release" "public"))
        (shipit-explore-about 'all))
    (should (equal "Forks: off" (shipit-explore-buffer--kind-description "fork")))
    (should (equal "Stars: on" (shipit-explore-buffer--kind-description "star")))
    (should (string-match-p "all" (shipit-explore-buffer--about-description)))))

(ert-deftest test-explore-buffer-the-menu-chooses-whose-repositories ()
  "GIVEN the Explore buffer of a feed with one entry about a repository of
the user and five about the repositories of others
WHEN the menu goes on from all repositories
THEN the buffer shows the one entry about the user's own
WHEN it goes on again
THEN the buffer shows the five of the others
WHEN it goes on again
THEN all six are back."
  (test-explore-buffer--shown (test-explore--answers (test-explore--events-with-own))
    (should (= 6 (length (test-explore-buffer--lines))))
    (shipit-explore-cycle-about)
    (should (eq shipit-explore-about 'mine))
    (should (equal (test-explore-buffer--lines) '(("the-reader/thing" "frank starred" "star"))))
    (shipit-explore-cycle-about)
    (should (eq shipit-explore-about 'others))
    (should (= 5 (length (test-explore-buffer--lines))))
    (shipit-explore-cycle-about)
    (should (eq shipit-explore-about 'all))
    (should (= 6 (length (test-explore-buffer--lines))))))

(provide 'test-shipit-explore-buffer)
;;; test-shipit-explore-buffer.el ends here
