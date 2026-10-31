;;; test-shipit-branches-buffer.el --- Tests for the branches buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the buffer that shows the branches of a repository, in the
;; groups of the branches page of GitHub.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'shipit-branches-buffer)
(require 'shipit-pr-github)
;; The buffers that a branch opens.  The branches buffer loads them
;; then, which would take the place of what a test put there.
(require 'shipit-buffer)
(require 'shipit-repo-buffer)
(require 'shipit-commit-comments-buffer)
;; The command of L.  shipit.el loads it for a user.
(require 'shipit-commands)
(require 'shipit-test-http)
(require 'shipit-test-meow)
(require 'test-shipit-branches)

(defvar embark-target-finders)
(defvar embark-keymap-alist)

(defmacro test-branches-buffer--shown (answers &rest body)
  "Run BODY in the branches buffer of example/tool, with GitHub giving ANSWERS.
There are no SVG icons, and the repository is not watched.  The window
is left as it was, and the buffer is killed afterwards."
  (declare (indent 1))
  `(test-branches--fresh ,answers
     (let ((shipit-use-svglib-icons nil)
           (shipit-branches-watched-repos nil))
       (save-window-excursion
         (shipit-branches "example/tool")
         (unwind-protect
             (with-current-buffer "*shipit-branches: example/tool*"
               ,@body)
           (kill-buffer "*shipit-branches: example/tool*"))))))

(defun test-branches-buffer--column (column)
  "The text of COLUMN of the line at point.
An empty cell holds no text, so it is not found, and gives \"\"."
  (if-let* ((start (text-property-any (line-beginning-position) (line-end-position)
                                      'shipit-branches-column column)))
      (string-trim (buffer-substring-no-properties
                    start (or (text-property-not-all start (line-end-position)
                                                     'shipit-branches-column column)
                              (line-end-position))))
    ""))

(defun test-branches-buffer--groups ()
  "The groups of this buffer, each as its heading and the lines of its
branches: the name, the author and the pull request of each."
  (mapcar (lambda (group)
            (cons (save-excursion
                    (goto-char (oref group start))
                    (string-trim (buffer-substring-no-properties
                                  (point) (line-end-position))))
                  (mapcar (lambda (branch)
                            (save-excursion
                              (goto-char (oref branch start))
                              (mapcar #'test-branches-buffer--column '(name author pr))))
                          (oref group children))))
          (seq-filter (lambda (section) (eq (oref section type) 'shipit-branches-group))
                      (oref magit-root-section children))))

(defun test-branches-buffer--go-to (name)
  "Put point on the line of the branch NAME."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal (cdr (assq 'name (magit-section-value-if 'shipit-branches-branch))) name)
          (setq found t)
        (forward-line 1)))
    (should found)))

(ert-deftest test-branches-buffer-shows-the-branches-in-groups ()
  "GIVEN a repository with a default branch, a branch of the user, an
active branch of another and a stale one
WHEN its branches are shown
THEN each group has a heading with its count, AND each branch shows its
name, who made its last commit and its pull request."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (should (equal (test-branches-buffer--groups)
                   '(("Default (1)" ("main" "alice" ""))
                     ("Yours (1)" ("feature/login" "the-reader" "#12 draft"))
                     ("Active (1)" ("fix/typo" "bob" "#9 merged"))
                     ("Stale (1)" ("old/idea" "carol" "")))))))

(ert-deftest test-branches-buffer-the-stale-branches-start-folded ()
  "GIVEN a repository with a stale branch
WHEN its branches are shown
THEN the group of the stale branches is folded, AND its branch is not
to be seen, AND the branches of the other groups are."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (should (equal (mapcar (lambda (section) (oref section hidden))
                           (seq-filter (lambda (section)
                                         (eq (oref section type) 'shipit-branches-group))
                                       (oref magit-root-section children)))
                   '(nil nil nil t)))
    (test-branches-buffer--go-to "old/idea")
    (should (invisible-p (point)))
    (test-branches-buffer--go-to "fix/typo")
    (should-not (invisible-p (point)))))

(defun test-branches-buffer--group (group)
  "The section of GROUP in this buffer."
  (seq-find (lambda (section) (eq (oref section value) group))
            (oref magit-root-section children)))

(ert-deftest test-branches-buffer-g-keeps-the-folds ()
  "GIVEN the branches of a repository, with the group of the user folded
and that of the stale ones unfolded by hand
WHEN g reads the branches again
THEN the group of the user is still folded, AND the stale branch can
still be seen."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (magit-section-hide (test-branches-buffer--group 'yours))
    (magit-section-show (test-branches-buffer--group 'stale))
    (shipit-branches-refresh)
    (test-branches-buffer--go-to "feature/login")
    (should (invisible-p (point)))
    (test-branches-buffer--go-to "old/idea")
    (should-not (invisible-p (point)))))

(ert-deftest test-branches-buffer-each-branch-shows-the-time-of-its-last-commit ()
  "GIVEN a branch whose last commit is three days old
WHEN its branches are shown
THEN its line shows that time, AND L can write it the other way."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (test-branches-buffer--go-to "fix/typo")
    (should (equal (get-text-property
                    (text-property-any (line-beginning-position) (line-end-position)
                                       'shipit-branches-column 'time)
                    'shipit-raw-timestamp)
                   (test-branches--days-ago 3)))
    (should (eq (key-binding (kbd "L")) #'shipit-toggle-timestamp-format))))

(ert-deftest test-branches-buffer-says-whether-new-branches-are-notified ()
  "GIVEN the branches of a repository that is not watched
THEN the buffer says that new branches give no notification
WHEN w is pressed
THEN the repository is watched, AND the buffer says that new branches
give a notification."
  (let* ((custom-file (make-temp-file "shipit-branches-custom-" nil ".el"))
         ;; Customize saves nothing in an Emacs that read no init file.
         (user-init-file custom-file))
    (unwind-protect
        (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
          (should (eq (key-binding (kbd "w")) #'shipit-branches-toggle-watch-here))
          (goto-char (point-min))
          (should (search-forward "New branch notifications: off" nil t))
          (shipit-branches-toggle-watch-here)
          (should (shipit-branches-watched-p "example/tool"))
          (goto-char (point-min))
          (should (search-forward "New branch notifications: on" nil t)))
      (delete-file custom-file))))

(ert-deftest test-branches-buffer-ret-opens-the-branch-in-shipit ()
  "GIVEN the branches of a repository
WHEN RET is pressed on a branch with a pull request
THEN shipit opens that pull request
WHEN RET is pressed on a branch without one
THEN shipit shows how it differs from the default branch
WHEN RET is pressed on the default branch
THEN shipit opens the page of the repository."
  (let ((opened nil))
    (test-branches--opening opened
      (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
        (should (eq (key-binding (kbd "RET")) #'shipit-branches-open))
        (dolist (name '("fix/typo" "old/idea" "main"))
          (test-branches-buffer--go-to name)
          (shipit-branches-open))))
    (should (equal (reverse opened)
                   '((pr 9 "example/tool")
                     (branch "example/tool" "old/idea" "main")
                     (repo "example/tool"))))))

(ert-deftest test-branches-buffer-ret-shows-the-web-page-when-shipit-cannot ()
  "GIVEN a backend that has no comments of commits, so no view of a branch
WHEN RET is pressed on a branch without a pull request
THEN the browser shows how it differs from the default branch."
  (let ((opened nil)
        (shipit-pr-backends (test-branches--backends-without :fetch-commit-comments)))
    (test-branches--opening opened
      (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
        (test-branches-buffer--go-to "old/idea")
        (shipit-branches-open)))
    (should (equal opened
                   '((browser "https://github.com/example/tool/compare/main...old/idea"))))))

(ert-deftest test-branches-buffer-the-comparison-opens-in-the-browser ()
  "GIVEN the branches of a repository
WHEN the comparison of a branch is asked for in the browser
THEN the browser shows how it differs from the default branch."
  (let ((opened nil))
    (test-branches--opening opened
      (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
        (test-branches-buffer--go-to "fix/typo")
        (shipit-branches-browse-compare)))
    (should (equal opened
                   '((browser "https://github.com/example/tool/compare/main...fix/typo"))))))

(ert-deftest test-branches-buffer-copies-the-name-and-the-address ()
  "GIVEN the branches of a repository, and no region
WHEN M-w is pressed on a branch
THEN its name is copied
WHEN its address is copied
THEN the address of its page is copied."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (let ((kill-ring nil))
      (test-branches-buffer--go-to "fix/typo")
      (should (eq (key-binding (kbd "M-w")) #'shipit-branches-copy-name))
      (shipit-branches-copy-name)
      (should (equal (car kill-ring) "fix/typo"))
      (shipit-branches-copy-url)
      (should (equal (car kill-ring) "https://github.com/example/tool/tree/fix/typo")))))

(ert-deftest test-branches-buffer-m-w-copies-a-region-as-usual ()
  "GIVEN the branches of a repository, and a region on the header
WHEN M-w is pressed
THEN the text of the region is copied."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (let ((kill-ring nil)
          (transient-mark-mode t))
      (goto-char (point-min))
      (set-mark (point))
      (forward-word 1)
      (activate-mark)
      (shipit-branches-copy-name)
      (should (equal (car kill-ring) "example")))))

;;; The filter

(defun test-branches-buffer--names ()
  "The names of the branches that this buffer shows, folded or not."
  (cl-loop for group in (oref magit-root-section children)
           nconc (mapcar (lambda (branch) (cdr (assq 'name (oref branch value))))
                         (oref group children))))

(defun test-branches-buffer--filter (text)
  "Filter the branches of this buffer on TEXT, as if typed after f."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) text)))
    (shipit-branches-set-filter)))

(ert-deftest test-branches-buffer-f-filters-on-the-name-and-the-author ()
  "GIVEN the branches of a repository
WHEN f filters on a word of a name
THEN only the branches whose name holds it are shown, AND the buffer
says what the filter is
WHEN f filters on an author
THEN only the branches of that author are shown
WHEN f filters on two words
THEN only the branches that match both are shown
WHEN the filter is emptied
THEN all branches are shown again, AND the buffer says no filter."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (should (eq (key-binding (kbd "f")) #'shipit-branches-set-filter))
    (test-branches-buffer--filter "typo")
    (should (equal (test-branches-buffer--names) '("fix/typo")))
    (goto-char (point-min))
    (should (search-forward "Filter: typo" nil t))
    (test-branches-buffer--filter "CAROL")
    (should (equal (test-branches-buffer--names) '("old/idea")))
    (test-branches-buffer--filter "fix bob")
    (should (equal (test-branches-buffer--names) '("fix/typo")))
    (test-branches-buffer--filter "fix alice")
    (should-not (test-branches-buffer--names))
    (goto-char (point-min))
    (should (search-forward "No branch matches the filter" nil t))
    (test-branches-buffer--filter "")
    (should (= 4 (length (test-branches-buffer--names))))
    (goto-char (point-min))
    (should-not (search-forward "Filter:" nil t))))

(ert-deftest test-branches-buffer-a-group-counts-what-the-filter-shows ()
  "GIVEN the branches of a repository
WHEN f filters on a word that two branches have
THEN the heading of each group counts the branches of the group that
are shown."
  (test-branches-buffer--shown
      (test-branches--answers
       (cons (test-branches--ref "fix/more" "bob" 4 "More") (test-branches--refs)))
    (test-branches-buffer--filter "fix")
    (should (equal (mapcar #'car (test-branches-buffer--groups)) '("Active (2)")))))

;;; The menu and embark

(ert-deftest test-branches-buffer-spc-opens-a-menu-of-actions ()
  "GIVEN the branches of a repository
THEN SPC opens the menu, AND the menu has the actions on a branch and
the filter, AND it says whether new branches give a notification."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (should (eq (key-binding (kbd "SPC")) #'shipit-branches-menu))
    (dolist (key '("RET" "o" "c" "y" "u" "f" "g" "L" "w"))
      (should (transient-get-suffix 'shipit-branches-menu key)))
    (should (equal (shipit-branches-buffer--watch-description)
                   "New branch notifications: off"))))

(ert-deftest test-branches-buffer-meow-leaves-spc-to-the-menu ()
  "GIVEN meow, which takes SPC for its keypad in its motion state
WHEN meow loads
THEN it starts a branches buffer in its insert state, where SPC reaches
the menu."
  (shipit-test-with-meow-loaded
    (should (eq 'insert (alist-get 'shipit-branches-mode meow-mode-state-list)))))

(ert-deftest test-branches-buffer-opening-again-clears-the-filter ()
  "GIVEN the branches of a repository, filtered on an author
WHEN they are shown again
THEN the filter is gone."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (test-branches-buffer--filter "carol")
    (shipit-branches "example/tool")
    (should (= 4 (length (test-branches-buffer--names))))))

(ert-deftest test-branches-buffer-opening-again-takes-the-keys-of-now ()
  "GIVEN a branches buffer that still has a keymap from before, as one
has after `shipit-reload' made the keymap of its mode anew
WHEN the branches are shown again
THEN the buffer has the keys of its mode as they are now."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (use-local-map (make-sparse-keymap))
    (shipit-branches "example/tool")
    (should (eq (key-binding (kbd "f")) #'shipit-branches-set-filter))))

(ert-deftest test-branches-buffer-a-branch-line-is-an-embark-target ()
  "GIVEN the branches of a repository
WHEN embark asks for a target on the line of a branch
THEN it is the branch, by its name, over the whole line
WHEN embark asks on the heading of a group, and on the top of the buffer
THEN there is none."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (test-branches-buffer--go-to "fix/typo")
    (should (equal (shipit-branches-embark-target)
                   `(shipit-branch "fix/typo" ,(line-beginning-position)
                                   . ,(line-end-position))))
    (goto-char (oref (car (oref magit-root-section children)) start))
    (should-not (shipit-branches-embark-target))
    (goto-char (point-min))
    (should-not (shipit-branches-embark-target))))

(ert-deftest test-branches-buffer-embark-offers-the-actions-on-a-branch ()
  "GIVEN embark with a target finder of its own
WHEN the branches register with embark
THEN embark asks the branches buffer first, AND a branch gets the
actions to open it, show it in the browser, compare it there, and copy
its name and its address."
  (let ((embark-target-finders (list 'embark-target-url-at-point))
        (embark-keymap-alist nil))
    (shipit-branches--embark-register)
    (should (eq 'shipit-branches-embark-target (car embark-target-finders)))
    (should (eq 'shipit-branches-embark-map (cdr (assq 'shipit-branch embark-keymap-alist))))
    (dolist (binding '(("RET" . shipit-branches-open) ("o" . shipit-branches-open)
                       ("b" . shipit-branches-browse) ("c" . shipit-branches-browse-compare)
                       ("w" . shipit-branches-copy-name) ("u" . shipit-branches-copy-url)))
      (should (eq (lookup-key shipit-branches-embark-map (kbd (car binding)))
                  (cdr binding))))))

(ert-deftest test-branches-buffer-o-opens-the-page-of-the-branch ()
  "GIVEN the branches of a repository
WHEN o is pressed on a branch
THEN the browser shows the page of that branch."
  (let ((opened nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (push url opened))))
      (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
        (should (eq (key-binding (kbd "o")) #'shipit-branches-browse))
        (test-branches-buffer--go-to "fix/typo")
        (shipit-branches-browse)))
    (should (equal opened '("https://github.com/example/tool/tree/fix/typo")))))

(ert-deftest test-branches-buffer-g-reads-the-branches-again ()
  "GIVEN the branches of a repository
WHEN g is pressed
THEN GitHub is asked for them again."
  (test-branches-buffer--shown (test-branches--answers (test-branches--refs))
    (should (eq (key-binding (kbd "g")) #'shipit-branches-refresh))
    (let ((asked (length shipit-test-http-requests)))
      (shipit-branches-refresh)
      (should (> (length shipit-test-http-requests) asked)))))

(ert-deftest test-branches-buffer-no-branches-says-so ()
  "GIVEN GitHub that refuses the query
WHEN the branches are shown
THEN the buffer says that no branches came."
  (test-branches-buffer--shown (list test-branches--user '("/graphql\\'" 502 ""))
    (goto-char (point-min))
    (should (search-forward "No branches came for this repository" nil t))))

(ert-deftest test-branches-buffer-has-octicons-with-svg-icons ()
  "GIVEN SVG icons
WHEN the branches are shown
THEN the line of a branch has an image, AND the octicons of a branch and
of a draft pull request were made."
  (let ((asked nil))
    (test-branches--fresh (test-branches--answers (test-branches--refs))
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
            (shipit-branches "example/tool")
            (unwind-protect
                (with-current-buffer "*shipit-branches: example/tool*"
                  (test-branches-buffer--go-to "feature/login")
                  (should (cl-some (lambda (pos) (imagep (get-text-property pos 'display)))
                                   (number-sequence (line-beginning-position)
                                                    (line-end-position)))))
              (kill-buffer "*shipit-branches: example/tool*"))))))
    (dolist (octicon '("git-branch" "git-pull-request-draft"))
      (should (member octicon asked)))))

(ert-deftest test-branches-buffer-a-folded-group-shows-no-icon-of-its-branches ()
  "GIVEN SVG icons
WHEN the branches are shown
THEN the body of no group starts with an icon.
Emacs shows an icon that starts where a fold starts, so the icon of the
first branch of a folded group would stand before the next heading."
  (test-branches--fresh (test-branches--answers (test-branches--refs))
    ;; `features' is not special, so only `dlet' binds it for `featurep'.
    (dlet ((shipit-use-svglib-icons t)
           (shipit--svg-lib-icon-cache (make-hash-table :test 'equal))
           (features (cons 'svg-lib features)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'svg-lib-icon)
                 (lambda (&rest _) '(image :type svg :data "<svg/>"))))
        (save-window-excursion
          (shipit-branches "example/tool")
          (unwind-protect
              (with-current-buffer "*shipit-branches: example/tool*"
                (dolist (group (oref magit-root-section children))
                  (should-not (get-text-property (oref group content) 'display))))
            (kill-buffer "*shipit-branches: example/tool*")))))))

(provide 'test-shipit-branches-buffer)
;;; test-shipit-branches-buffer.el ends here
