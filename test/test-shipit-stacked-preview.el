;;; test-shipit-stacked-preview.el --- Dividing commits into layers -*- lexical-binding: t; -*-

;;; Commentary:
;; Where the divisions go, and what layers they make.  This is the part
;; that decides what becomes a pull request, so it is worked out from
;; data and tested without a buffer.

;;; Code:

(require 'ert)
(require 'shipit-stacked-preview)

(defconst test-stacked-preview--commits
  '((:sha "a1" :first-line "fpga: conventions")
    (:sha "a2" :first-line "fpga: point tests at the skill")
    (:sha "b1" :first-line "uoe: abort path")
    (:sha "b2" :first-line "uoe: tx mux")
    (:sha "c1" :first-line "zhit: formal"))
  "Five commits across three areas, oldest first.")

(ert-deftest test-shipit-stacked-preview-proposes-a-division-where-the-area-changes ()
  "GIVEN commits whose subjects name the area they touch
WHEN divisions are proposed
THEN one falls after the last commit of each run, and never after the
newest: that one ends the top layer, and the top of a pile is not a
division."
  (should (equal '("a2" "b2")
                 (shipit-stacked-preview--proposed-boundaries
                  test-stacked-preview--commits))))

(ert-deftest test-shipit-stacked-preview-proposes-nothing-without-prefixes ()
  "GIVEN subjects with no areas named
WHEN divisions are proposed
THEN there are none, leaving one layer and the dividing to be done by
hand.  Guessing at boundaries would be inventing them."
  (should-not (shipit-stacked-preview--proposed-boundaries
               '((:sha "a" :first-line "first thing")
                 (:sha "b" :first-line "second thing")))))

(ert-deftest test-shipit-stacked-preview-divides-into-layers-bottom-first ()
  "GIVEN divisions
WHEN the layers are worked out
THEN each holds the commits up to its division, oldest first, and the
last keeps the branch already checked out, whose work is there
already."
  (let ((layers (shipit-stacked-preview--layers
                 test-stacked-preview--commits '("a2" "b2") "work")))
    (should (equal 3 (length layers)))
    (should (equal '(2 2 1)
                   (mapcar (lambda (l) (length (cdr (assq 'commits l)))) layers)))
    (should (equal '("work-fpga" "work-uoe" "work")
                   (mapcar (lambda (l) (cdr (assq 'branch l))) layers)))
    (should (equal '("a2" "b2" "c1")
                   (mapcar (lambda (l) (cdr (assq 'tip l))) layers)))
    (should (equal '(nil nil t)
                   (mapcar (lambda (l) (cdr (assq 'existing l))) layers)))))

(ert-deftest test-shipit-stacked-preview-with-no-divisions-is-one-layer ()
  "GIVEN no divisions
WHEN the layers are worked out
THEN there is one, holding everything, and it is the branch already
checked out.  That is an ordinary pull request, which is what this
was before anyone divided it."
  (let ((layers (shipit-stacked-preview--layers test-stacked-preview--commits nil "work")))
    (should (equal 1 (length layers)))
    (should (equal "work" (cdr (assq 'branch (car layers)))))
    (should (cdr (assq 'existing (car layers))))))

(ert-deftest test-shipit-stacked-preview-names-a-layer-after-its-area ()
  "GIVEN a layer whose commits share an area
WHEN it is named
THEN the name says which, so the branch and the pull request it
becomes are recognisable."
  (should (equal "work-uoe"
                 (shipit-stacked-preview--layer-name
                  "work" '((:sha "b1" :first-line "uoe: abort path"))))))

(ert-deftest test-shipit-stacked-preview-keeps-the-names-branches-already-have ()
  "GIVEN layers already marked by branches
WHEN the layers are worked out
THEN each keeps the name its branch gave it, rather than one derived
from its commit subjects.  A stack prepared elsewhere, or divided
here before, should open as it stands."
  (let ((layers (shipit-stacked-preview--layers
                 test-stacked-preview--commits '("a2" "b2") "work"
                 '(("a2" . "PRJ-101-parser-cleanup")
                   ("b2" . "PRJ-102-writer-rework")))))
    (should (equal '("PRJ-101-parser-cleanup"
                     "PRJ-102-writer-rework"
                     "work")
                   (mapcar (lambda (l) (cdr (assq 'branch l))) layers)))))

(ert-deftest test-shipit-stacked-preview-derives-a-name-only-where-there-is-none ()
  "GIVEN one layer marked by a branch and one not
WHEN the layers are worked out
THEN the marked one keeps its name and the other is named from its
commits, so dividing further does not rename what is already there."
  (let ((layers (shipit-stacked-preview--layers
                 test-stacked-preview--commits '("a2" "b2") "work"
                 '(("a2" . "already-here")))))
    (should (equal '("already-here" "work-uoe" "work")
                   (mapcar (lambda (l) (cdr (assq 'branch l))) layers)))))

(ert-deftest test-shipit-stacked-preview-commits-line-up-under-their-layer ()
  "GIVEN a layer and the commits inside it
WHEN both are drawn
THEN a commit's hash begins in the same column as a branch name.

A layer row spends its first columns on the HEAD mark and the layer
number, which a commit has neither of, so without an indent the
commits sit to the left of the layer holding them."
  (let* ((layer-row (format "  %-5s %2d  %s" "HEAD" 6 "branch-name"))
         (branch-column (string-match "branch-name" layer-row))
         ;; the renderer spends one column, then two on the unread mark,
         ;; before the hash
         (hash-column (+ shipit-stacked-preview--commit-indent 1 2)))
    (should (equal branch-column hash-column))))

(ert-deftest test-shipit-stacked-preview-counts-line-up-past-two-digits ()
  "GIVEN layers whose commit counts differ in width
WHEN the column width is worked out
THEN it fits the widest.  A branch left far behind its base holds
hundreds, and a fixed field pushes the word beside it out of line the
moment one does."
  (should (equal 3 (shipit-stacked-preview--count-width
                    (list (list (cons 'commits (make-list 507 nil)))
                          (list (cons 'commits (make-list 7 nil)))))))
  (should (equal 1 (shipit-stacked-preview--count-width
                    (list (list (cons 'commits (make-list 7 nil))))))))

(provide 'test-shipit-stacked-preview)
;;; test-shipit-stacked-preview.el ends here
