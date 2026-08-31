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

(ert-deftest test-shipit-stack-render-a-layer-starts-shut-every-time ()
  "GIVEN a layer that was expanded
WHEN the buffer is drawn again
THEN it is shut.

magit decides visibility from what it remembered and from the previous
render's tree, both of which outrank the value asked for at insertion.
One layer opened while reading it therefore stayed open for the rest
of the session, and on a stack of six that is the whole buffer."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
    (magit-insert-section (shipit-pr-buffer)
      (shipit-sections-insert-stack-rows test-stack-render--rows "main"))
    (let ((layer (car (oref magit-root-section children))))
      (should (oref layer hidden))
      (magit-section-show layer)
      (should-not (oref layer hidden)))
    (let ((inhibit-read-only t)) (erase-buffer))
    (magit-insert-section (shipit-pr-buffer)
      (shipit-sections-insert-stack-rows test-stack-render--rows "main"))
    (should (oref (car (oref magit-root-section children)) hidden))))

(ert-deftest test-shipit-stack-render-a-born-shut-body-is-invisible ()
  "GIVEN rows drawn with a body
WHEN a layer is inspected before anyone touches it
THEN its body is invisible, not merely marked hidden: the hidden slot
without the overlay shows the body anyway and TAB then has to be
pressed twice."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
    (magit-insert-section (shipit-pr-buffer)
      (shipit-sections-insert-stack-rows
       test-stack-render--rows "main"
       (lambda (row)
         (insert (format "      about %s\n" (cdr (assq 'name row)))))))
    (dolist (layer (oref magit-root-section children))
      (should (oref layer hidden))
      (should (invisible-p (marker-position (oref layer content)))))))

(ert-deftest test-shipit-stack-render-washer-fills-a-row-on-first-show ()
  "GIVEN rows drawn with a washer instead of a body
WHEN the buffer is drawn
THEN the rows carry nothing yet
WHEN a layer is shown
THEN the washer's content appears, nested under that layer, and
showing again adds nothing: the forge is asked when the reader asks."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
    (let ((washes 0))
      (magit-insert-section (shipit-pr-buffer)
        (shipit-sections-insert-stack-rows
         test-stack-render--rows "main" nil
         (lambda (row)
           (cl-incf washes)
           (magit-insert-section (shipit-stack-layer-detail)
             (magit-insert-heading
               (format "      washed %s" (cdr (assq 'name row))))))))
      (should (equal 0 washes))
      (should-not (string-match-p "washed" (buffer-string)))
      (let ((layer (car (oref magit-root-section children))))
        (magit-section-show layer)
        (should (equal 1 washes))
        (should (string-match-p (format "washed %s" (oref layer value))
                                (buffer-string)))
        (should (equal 1 (length (oref layer children))))
        (should (eq layer (oref (car (oref layer children)) parent)))
        (magit-section-hide layer)
        (magit-section-show layer)
        (should (equal 1 washes))))))

(ert-deftest test-shipit-stack-render-base-is-left-aligned-and-marked ()
  "GIVEN rows over a base, with something to mark the base row
WHEN the stack is drawn
THEN the base starts where the row labels start, not indented under
the names, AND its mark sits in the same column as the rows' states.
WHEN the mark is an icon, a space carrying a display property
THEN it survives on the line rather than being trimmed as whitespace."
  (with-temp-buffer
    (magit-section-mode)
    (setq inhibit-read-only t)
    (magit-insert-section (shipit-pr-buffer)
      (shipit-sections-insert-stack-rows
       '(((label . "#1") (name . "alpha") (extra . "E1"))
         ((label . "#22") (name . "beta-long") (extra . "E2")))
       "main" nil nil "BX"))
    (let* ((text (buffer-string))
           (row (progn (string-match "^.*E1.*$" text) (match-string 0 text)))
           (base (progn (string-match "^.*BX.*$" text) (match-string 0 text))))
      (should (string-match-p "^   main" base))
      (should (equal (cl-search "E1" row) (cl-search "BX" base))))
    (erase-buffer)
    ;; `text-property-any' compares with `eq', so the mark must be the
    ;; same object in the buffer and in the assertion.
    (let ((mark "MARK"))
      (magit-insert-section (shipit-pr-buffer)
        (shipit-sections-insert-stack-rows
         '(((label . "#1") (name . "alpha") (extra . "E1")))
         "main" nil nil (propertize " " 'display mark)))
      (goto-char (point-min))
      (search-forward "main")
      (should (text-property-any (line-beginning-position) (line-end-position)
                                 'display mark)))))

(provide 'test-shipit-stack-render)
;;; test-shipit-stack-render.el ends here
