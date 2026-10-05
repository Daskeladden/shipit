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

(provide 'test-shipit-imenu)
;;; test-shipit-imenu.el ends here
