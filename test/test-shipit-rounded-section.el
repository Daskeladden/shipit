;;; test-shipit-rounded-section.el --- Rounded section backgrounds -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The rounded pill behind a comment is drawn with a `line-prefix' on
;; the first character of every line it covers -- an SVG edge piece one
;; character wide.
;;
;; Collapsing a section makes that fragile.  Magit hides the range
;; between a section's `content' and `end' markers, and `end' is where
;; the next sibling begins, so the screen line that renders the next
;; sibling starts at the hidden `content' position.  Emacs reads
;; `line-prefix' at that starting position, not at the visible text, so
;; whatever the collapse leaves there decides whether the next line
;; keeps its indent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'shipit-rounded-section)

(defun test-srs--insert-comment (id)
  "Insert a comment section named ID carrying an icon color.
The colored heading is what `shipit-rounded--extract-icon-color'
looks for, and so what marks a section as wearing a pill."
  (magit-insert-section (inline-comment id)
    (magit-insert-heading
      (propertize (format "     comment %d" id) 'shipit-icon-color "#ff8c00"))
    (insert "        body\n")
    (insert "\n")))

(defun test-srs--section (id)
  "Return the comment section named ID."
  (seq-find (lambda (section) (equal (oref section value) id))
            (oref magit-root-section children)))

(defun test-srs--prefix (position)
  "Return the line prefix at POSITION."
  (get-text-property (if (markerp position) (marker-position position) position)
                     'line-prefix))

(defmacro test-srs--with-comments (&rest body)
  "Run BODY in a buffer holding two comment sections and a plain line.
Both sections have their rounded background applied.  A graphic
display is faked, since the prefixes are only drawn on one."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create "*test-srs*")))
     (unwind-protect
         (with-current-buffer buffer
           (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
             (let ((inhibit-read-only t))
               (magit-section-mode)
               (erase-buffer)
               (magit-insert-section (root)
                 (test-srs--insert-comment 1)
                 (test-srs--insert-comment 2)
                 (insert "plain trailing line\n"))
               (shipit-rounded--apply-to-section-now (test-srs--section 1))
               (shipit-rounded--apply-to-section-now (test-srs--section 2))
               ,@body)))
       (kill-buffer buffer))))

(ert-deftest test-srs-comment-lines-are-prefixed ()
  "GIVEN a comment section with a rounded background
WHEN its lines are examined
THEN the heading and the body both carry the one-character prefix."
  (test-srs--with-comments
    (let ((section (test-srs--section 1)))
      (should (test-srs--prefix (oref section start)))
      (should (test-srs--prefix (oref section content))))))

(ert-deftest test-srs-collapsing-keeps-the-next-comment-indented ()
  "GIVEN two comment sections, each wearing a rounded background
WHEN the first is collapsed
THEN its hidden content carries the prefix the second one draws with,
because that hidden position is where the second one's screen line
begins -- without it the second comment loses a character of indent."
  (test-srs--with-comments
    (let ((first (test-srs--section 1)))
      (magit-section-hide first)
      (should (equal (test-srs--prefix (oref first content))
                     (test-srs--prefix (oref first end))))
      (should (test-srs--prefix (oref first end))))))

(ert-deftest test-srs-collapsing-keeps-the-heading-visible ()
  "GIVEN a comment section wearing a rounded background
WHEN it is collapsed
THEN its heading keeps its own prefix, so the pill still shows."
  (test-srs--with-comments
    (let ((first (test-srs--section 1)))
      (magit-section-hide first)
      (should (test-srs--prefix (oref first start))))))

(ert-deftest test-srs-collapsing-before-a-plain-line-adds-no-indent ()
  "GIVEN a comment section followed by a line with no background
WHEN the comment is collapsed
THEN nothing is left at its hidden content, so the plain line is not
pushed a character to the right."
  (test-srs--with-comments
    (let ((second (test-srs--section 2)))
      (magit-section-hide second)
      (should (null (test-srs--prefix (oref second content)))))))

(provide 'test-shipit-rounded-section)
;;; test-shipit-rounded-section.el ends here
