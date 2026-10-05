;;; test-shipit-imenu.el --- Tests for the imenu index of sections -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the index that imenu gets from the sections of a buffer,
;; and for going to an entry of it.  The index of an issue buffer is
;; tested on the Jira sandbox, in test-shipit-jira-sandbox.el.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'imenu)
(require 'magit-section)
(require 'shipit-sections)
(require 'shipit-buffer)

(defmacro test-imenu--with-sections (&rest body)
  "Run BODY in a buffer of sections.
A description, then a section of a type that is not asked for, which
holds the comments with two comments in them, then a footer."
  (declare (indent 0))
  `(with-temp-buffer
     (magit-section-mode)
     (let ((inhibit-read-only t))
       (magit-insert-section (root)
         (magit-insert-section (description)
           (magit-insert-heading "Description")
           (insert "Some text\n"))
         (magit-insert-section (wrapper)
           (magit-insert-section (comments)
             (magit-insert-heading "Comments (2)")
             (magit-insert-section (comment 1)
               (magit-insert-heading "   alice  10:00")
               (insert "First said\n"))
             (magit-insert-section (comment 2)
               (magit-insert-heading "   bob  11:00")
               (insert "Second said\n"))))
         (magit-insert-section (footer)
           (magit-insert-heading "Footer")
           (insert "The end\n"))))
     ,@body))

(defun test-imenu--entries ()
  "The index of this buffer for descriptions, comments sections and comments."
  (shipit--imenu-entries (oref magit-root-section children)
                         '(description comments comment)))

(defun test-imenu--line-at (position)
  "The text of the line at POSITION, an integer or a marker."
  (save-excursion
    (goto-char position)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(ert-deftest test-imenu-a-section-of-a-type-asked-for-is-an-entry ()
  "GIVEN a buffer with a description, comments and a footer
WHEN the index is made for descriptions, comments sections and comments
THEN the description is an entry named by its heading, at its place,
AND the footer is none."
  (test-imenu--with-sections
    (let ((entries (test-imenu--entries)))
      (should (equal '("Description" "Comments") (mapcar #'car entries)))
      (should (equal "Description"
                     (test-imenu--line-at (cdr (assoc "Description" entries))))))))

(ert-deftest test-imenu-a-section-with-entries-in-it-is-a-submenu ()
  "GIVEN comments with two comments in them, inside a section of a type
that is not asked for
WHEN the index is made
THEN the comments are a submenu in the place of that section, named
without the count, AND its first entry is the comments section itself,
AND the two comments follow in their order, each at its place."
  (test-imenu--with-sections
    (let ((comments (cdr (assoc "Comments" (test-imenu--entries)))))
      (should (equal '("Comments" "alice  10:00" "bob  11:00") (mapcar #'car comments)))
      (should (equal "Comments (2)" (test-imenu--line-at (cdr (nth 0 comments)))))
      (should (equal "   bob  11:00" (test-imenu--line-at (cdr (nth 2 comments))))))))

(ert-deftest test-imenu-an-entry-is-at-a-marker ()
  "GIVEN the index of a buffer of sections
THEN the place of each entry is a marker.  consult-imenu shows an entry
before it is chosen only when its place is one."
  (test-imenu--with-sections
    (let ((entries (test-imenu--entries)))
      (should (markerp (cdr (assoc "Description" entries))))
      (should (cl-every (lambda (entry) (markerp (cdr entry)))
                        (cdr (assoc "Comments" entries)))))))

(ert-deftest test-imenu-a-jump-unfolds-the-sections-around-the-entry ()
  "GIVEN the comments folded, so that no comment shows, AND the function
that unfolds on the hook that imenu runs after a jump
WHEN imenu goes to the second comment, in its own way
THEN point is on that comment, AND the comments are unfolded."
  (test-imenu--with-sections
    (let* ((comments (cdr (assoc "Comments" (test-imenu--entries))))
           (section (progn (goto-char (cdr (nth 0 comments))) (magit-current-section))))
      (add-hook 'imenu-after-jump-hook #'shipit--imenu-unfold-around-point nil t)
      (magit-section-hide section)
      (should (oref section hidden))
      (goto-char (point-min))
      (imenu (nth 2 comments))
      (should (equal "   bob  11:00" (test-imenu--line-at (point))))
      (should-not (oref section hidden)))))

;;; The index of a pull request buffer

(defmacro test-imenu--with-pull-request (&rest body)
  "Run BODY in a pull request buffer with files, and with folded commits.
The files are drawn.  The commits are folded, and draw their one commit
only when they are unfolded, as a pull request buffer folds them; the
index knows the commit by its name, which was stored for it."
  (declare (indent 0))
  `(with-temp-buffer
     (shipit-mode)
     (let ((inhibit-read-only t))
       (magit-insert-section (root)
         (magit-insert-section (pr-files)
           (magit-insert-heading "Files Changed (2)")
           (magit-insert-section (pr-file "a.el")
             (magit-insert-heading "a.el"))
           (magit-insert-section (pr-file "b.el")
             (magit-insert-heading "b.el")))
         (let ((commits (magit-insert-section (pr-commits nil t)
                          (magit-insert-heading "Commits (1)")
                          (magit-insert-section-body
                            (magit-insert-section (shipit-commit "abc123")
                              (magit-insert-heading "abc123 Fix the parser")
                              (insert "The message\n"))))))
           (shipit--imenu-store-children (oref commits start) '("abc123 Fix the parser")
                                         'pr-commits))))
     ,@body))

(ert-deftest test-imenu-a-pull-request-buffer-keeps-the-way-imenu-goes ()
  "GIVEN a pull request buffer
THEN it keeps the way imenu itself goes to an entry, which lets
consult-imenu show an entry before it is chosen, AND it unfolds around
the entry on the hook that imenu runs after a jump."
  (test-imenu--with-pull-request
    (should (eq imenu-default-goto-function #'imenu-default-goto-function))
    (should (memq #'shipit--imenu-unfold-around-point imenu-after-jump-hook))))

(ert-deftest test-imenu-a-drawn-entry-of-a-pull-request-is-at-a-marker ()
  "GIVEN a pull request buffer with two files that are drawn
WHEN imenu asks for the index
THEN the files are a submenu, AND each entry of it is at a marker."
  (test-imenu--with-pull-request
    (let ((files (cdr (assoc "Files Changed" (funcall imenu-create-index-function)))))
      (should (equal '("Files Changed" "a.el" "b.el") (mapcar #'car files)))
      (should (cl-every (lambda (entry) (markerp (cdr entry))) files)))))

(ert-deftest test-imenu-an-entry-of-a-folded-section-unfolds-and-finds-it ()
  "GIVEN a pull request buffer with folded commits that are not drawn
WHEN imenu asks for the index
THEN the commit is in the submenu of the commits
WHEN imenu goes to that entry
THEN the commits are unfolded, AND point is on the commit."
  (test-imenu--with-pull-request
    (let* ((commits (cdr (assoc "Commits" (funcall imenu-create-index-function))))
           (entry (assoc "abc123 Fix the parser" commits)))
      (should entry)
      (goto-char (point-min))
      (imenu entry)
      (should (equal "abc123 Fix the parser" (test-imenu--line-at (point))))
      (should-not (oref (shipit--find-section-by-type 'pr-commits) hidden)))))

(ert-deftest test-imenu-an-entry-of-a-folded-section-needs-no-name-from-the-caller ()
  "GIVEN the entry of a commit in folded commits
WHEN its function is called with another name, as consult-imenu calls
it with the whole path of the entry
THEN point is on the commit all the same: the entry names its section
itself."
  (test-imenu--with-pull-request
    (let* ((commits (cdr (assoc "Commits" (funcall imenu-create-index-function))))
           (entry (assoc "abc123 Fix the parser" commits)))
      (goto-char (point-min))
      (apply (nth 2 entry) "Commits abc123 Fix the parser" (nth 1 entry) (nthcdr 3 entry))
      (should (equal "abc123 Fix the parser" (test-imenu--line-at (point)))))))

(provide 'test-shipit-imenu)
;;; test-shipit-imenu.el ends here
