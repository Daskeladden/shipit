;;; test-shipit-stack-render.el --- Tests for stack rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; One renderer draws both stacks: the local chain of branches the
;; preview shows before anything is pushed, and the stack of pull
;; requests GitHub reports once they are.  The rows differ; the picture
;; must not.
;;
;; Each layer is its own magit section so it can be expanded to show
;; the commits that layer adds, which is the question a reviewer asks
;; of a stack: what is in this one, as opposed to everything below it.

;;; Code:

(require 'ert)
(require 'magit)
(require 'shipit-sections)

(defconst test-stack-render--rows
  '(((label . "1") (name . "layer-one") (extra . "+2") (current . nil))
    ((label . "2") (name . "layer-two") (extra . "+3") (current . nil)
     (note . "also layer-two-backup"))
    ((label . "3") (name . "layer-three") (extra . "+7") (current . t)))
  "Rows arrive bottom first, the order they merge in.")

(defmacro test-stack-render--draw (rows base &rest body)
  "Draw ROWS over BASE in a temp buffer, then run BODY there."
  (declare (indent 2))
  `(with-temp-buffer
     (magit-section-mode)
     (setq inhibit-read-only t)
     (magit-insert-section (shipit-pr-buffer)
       (shipit-sections-insert-stack-rows ,rows ,base))
     ,@body))

(ert-deftest test-shipit-stack-render-draws-top-first ()
  "GIVEN rows given bottom first
WHEN they are drawn
THEN the top of the stack is the first line and the base is the last,
so the picture on screen matches the word stack."
  (test-stack-render--draw test-stack-render--rows "main"
    (let ((lines (split-string (string-trim (buffer-string)) "\n")))
      (should (string-match-p "layer-three" (nth 0 lines)))
      (should (string-match-p "layer-two" (nth 1 lines)))
      (should (string-match-p "layer-one" (nth 2 lines)))
      (should (string-match-p "main" (car (last lines)))))))

(ert-deftest test-shipit-stack-render-marks-the-current-row ()
  "GIVEN a row flagged as the one being worked on
WHEN the rows are drawn
THEN only that row is marked, so the reader can see where they are."
  (test-stack-render--draw test-stack-render--rows "main"
    (let ((marked (seq-filter (lambda (l) (string-match-p "<-" l))
                              (split-string (buffer-string) "\n"))))
      (should (equal 1 (length marked)))
      (should (string-match-p "layer-three" (car marked))))))

(ert-deftest test-shipit-stack-render-keeps-a-row-note ()
  "GIVEN a row carrying a note, such as a second branch name on the
same commit
WHEN the rows are drawn
THEN the note appears on that row, where the reader is looking when
they decide which name is the layer."
  (test-stack-render--draw test-stack-render--rows "main"
    (let ((line (seq-find (lambda (l) (string-match-p "layer-two\\b" l))
                          (split-string (buffer-string) "\n"))))
      (should (string-match-p "also layer-two-backup" line)))))

(ert-deftest test-shipit-stack-render-aligns-names-of-any-length ()
  "GIVEN branch names of very different lengths
WHEN the rows are drawn
THEN the column after the name starts in the same place on every row,
because a ragged column is what makes a six layer stack unreadable."
  (test-stack-render--draw
      '(((label . "1") (name . "a") (extra . "+1"))
        ((label . "2") (name . "a-very-much-longer-branch-name") (extra . "+2")))
      "main"
    ;; No `string-trim' here: it would strip the first line's indent
    ;; and shift the column being compared.
    (let* ((lines (split-string (buffer-string) "\n"))
           (cols (mapcar (lambda (l) (string-match "\\+[0-9]" l))
                         (seq-take lines 2))))
      (should (equal (nth 0 cols) (nth 1 cols))))))

(ert-deftest test-shipit-stack-render-layer-is-its-own-section ()
  "GIVEN drawn rows
WHEN the sections are inspected
THEN there is one layer section per row, so each can be expanded and
`n' and `p' step between layers."
  (test-stack-render--draw test-stack-render--rows "main"
    (should (eq (get 'shipit-stack-layer 'magit-section) t))
    (should (equal 3 (length (oref magit-root-section children))))))

(ert-deftest test-shipit-stack-render-nests-a-layer-body-under-its-layer ()
  "GIVEN a body function that fills each layer
WHEN the rows are drawn
THEN the content is there and any section it made belongs to its
layer, not to the root.

The body used to be deferred into a magit washer.  A section created
inside one did not nest under the layer, and collapsing then reached
for the root section, which cannot be hidden.  A layer's body is a few
lines of git output, so it is built as the row is drawn."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
    (magit-insert-section (shipit-pr-buffer)
      (shipit-sections-insert-stack-rows
       test-stack-render--rows "main"
       (lambda (row)
         (magit-insert-section (shipit-stack-layer-detail)
           (magit-insert-heading
            (format "      commit for %s" (cdr (assq 'name row))))))))
    (should (string-match-p "commit for layer-three" (buffer-string)))
    (should (string-match-p "commit for layer-one" (buffer-string)))
    (let ((layers (oref magit-root-section children)))
      (should (equal 3 (length layers)))
      (dolist (layer layers)
        (should (oref layer children))
        (dolist (child (oref layer children))
          (should (eq layer (oref child parent)))
          (should-not (eq magit-root-section (oref child parent))))))))

(provide 'test-shipit-stack-render)
;;; test-shipit-stack-render.el ends here
