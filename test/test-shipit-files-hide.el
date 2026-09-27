;;; test-shipit-files-hide.el --- Hiding files in Files Changed -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The file filter shows only the files that match it.  Hiding does
;; the opposite: it takes chosen files out of Files Changed, and it
;; combines with the filter.  The tests draw Files Changed of a pull
;; request in a real buffer and refresh it the way the filter does.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit-pr-sections)
(require 'shipit-buffer)
(require 'shipit-file-filter)
(require 'shipit-preview)
(require 'shipit-sandbox)

(defun test-files-hide--file (name)
  "A changed file of a pull request, called NAME."
  `((filename . ,name) (status . "modified") (additions . 1) (deletions . 0) (changes . 1)))

(defconst test-files-hide--pr
  `((number . 1) (head (sha . "abc123") (ref . "work")) (base (ref . "main"))
    (files ,(test-files-hide--file "src/Button.tsx")
           ,(test-files-hide--file "src/Header.tsx")
           ,(test-files-hide--file "package-lock.json")))
  "A pull request of three files.")

(defun test-files-hide--open-files ()
  "Open the Files Changed section, as TAB on it does.  It starts closed."
  (save-excursion
    (goto-char (or (text-property-any (point-min) (point-max) 'shipit-pr-files t)
                   (error "No Files Changed section in the buffer")))
    (magit-section-show (magit-current-section))))

(defmacro test-files-hide--in-pr-buffer (&rest body)
  "Run BODY in a pull request buffer that shows `test-files-hide--pr'."
  (declare (indent 0))
  `(with-temp-buffer
     (shipit-mode)
     (setq shipit-buffer-repo "o/r"
           shipit-buffer-pr-number 1
           shipit-buffer-pr-data test-files-hide--pr)
     (let ((inhibit-read-only t))
       (magit-insert-section (root)
         (shipit--render-files-section (cdr (assq 'files test-files-hide--pr))
                                       "o/r" 1 test-files-hide--pr "abc123")))
     (test-files-hide--open-files)
     ,@body))

(defun test-files-hide--shown ()
  "The names of the files this buffer shows, in order."
  (let ((names nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((name (get-text-property pos 'shipit-file-path)))
        (when name (cl-pushnew name names :test #'equal)))
      (setq pos (next-single-property-change pos 'shipit-file-path nil (point-max))))
    (nreverse names)))

(defun test-files-hide--goto-file (name)
  "Put point on the line of the file NAME.
Not `text-property-any': it compares with `eq', and a name is a string."
  (goto-char (point-min))
  (while (not (equal name (get-text-property (point) 'shipit-file-path)))
    (when (eobp) (error "No file %s in the buffer" name))
    (goto-char (next-single-property-change (point) 'shipit-file-path nil (point-max)))))

(ert-deftest test-files-filter-by-name-is-plain-text ()
  "GIVEN file names with characters that mean something in a regexp
WHEN the files are filtered by name
THEN the name is matched as it is typed."
  (with-temp-buffer
    (should (shipit--file-matches-filter-p (test-files-hide--file "src/[id].tsx") "[id]"))
    (should-not (shipit--file-matches-filter-p (test-files-hide--file "axb") "a.b"))))

(ert-deftest test-files-hide-the-file-at-point ()
  "GIVEN a pull request of three files
WHEN the file at point is hidden
THEN the other two show, and the filter line says one file is hidden."
  (test-files-hide--in-pr-buffer
    (test-files-hide--goto-file "package-lock.json")
    (call-interactively #'shipit--hide-file)
    (should (equal '("src/Button.tsx" "src/Header.tsx") (test-files-hide--shown)))
    (should (string-match-p "Filter: 1 hidden" (buffer-string)))))

(ert-deftest test-files-hide-a-file-picked-by-name ()
  "GIVEN point on no file
WHEN a file is hidden
THEN it is picked by name from the files shown, and hidden."
  (test-files-hide--in-pr-buffer
    (goto-char (point-min))
    (let ((offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered collection)
                   "src/Header.tsx")))
        (call-interactively #'shipit--hide-file))
      (should (equal '("src/Button.tsx" "src/Header.tsx" "package-lock.json") offered)))
    (should (equal '("src/Button.tsx" "package-lock.json") (test-files-hide--shown)))))

(ert-deftest test-files-hide-the-files-whose-name-contains-a-text ()
  "GIVEN a pull request of three files
WHEN the files whose name contains `TSX' are hidden
THEN only the other one shows, and the filter line says two are hidden."
  (test-files-hide--in-pr-buffer
    (shipit--hide-files-matching "TSX")
    (should (equal '("package-lock.json") (test-files-hide--shown)))
    (should (string-match-p "Filter: 2 hidden" (buffer-string)))))

(ert-deftest test-files-hide-combines-with-the-filter ()
  "GIVEN the filter shows only the files under src/
WHEN one of them is hidden
THEN only the other one shows, and the filter line says both."
  (test-files-hide--in-pr-buffer
    (setq shipit--files-filter-text "src/")
    (shipit--refresh-files-section-only)
    (test-files-hide--goto-file "src/Button.tsx")
    (call-interactively #'shipit--hide-file)
    (should (equal '("src/Header.tsx") (test-files-hide--shown)))
    (should (string-match-p "Filter: src/, 1 hidden" (buffer-string)))))

(ert-deftest test-files-hide-is-cleared-with-the-filter ()
  "GIVEN hidden files
WHEN the filter is cleared
THEN every file shows again."
  (test-files-hide--in-pr-buffer
    (shipit--hide-files-matching "tsx")
    (shipit--clear-files-filter)
    (should (= 3 (length (test-files-hide--shown))))
    (should-not (string-match-p "hidden" (buffer-string)))))

(ert-deftest test-files-hide-in-a-preview ()
  "GIVEN a preview of a branch
WHEN the file at point is hidden
THEN the preview no longer shows it, and says one file is hidden."
  (shipit-sandbox-with-repo
    (let ((shipit-preview--saved-state (make-hash-table :test 'equal)))
      (shipit-preview-branch shipit-sandbox-repo "work" "main")
      (with-current-buffer (shipit-preview--buffer-name shipit-sandbox-repo "work")
        (unwind-protect
            (progn
              (test-files-hide--open-files)
              (test-files-hide--goto-file "writer--emit-the-body.txt")
              (call-interactively #'shipit--hide-file)
              (should-not (member "writer--emit-the-body.txt" (test-files-hide--shown)))
              (should (member "writer--emit-the-header.txt" (test-files-hide--shown)))
              (should (string-match-p "Filter: 1 hidden" (buffer-string))))
          (kill-buffer))))))

(ert-deftest test-files-hide-runs-the-functions-that-follow-a-redraw ()
  "GIVEN a function on the hook that follows a redraw of Files Changed
WHEN files are hidden, which draws Files Changed again
THEN the function runs once, in the pull request buffer."
  (test-files-hide--in-pr-buffer
    (let* ((runs nil)
           (shipit-files-section-refreshed-hook
            (list (lambda () (push (current-buffer) runs)))))
      (shipit--hide-files-matching "TSX")
      (should (equal (list (current-buffer)) runs)))))

(defun test-files-hide--files-section ()
  "The Files Changed section of this buffer."
  (save-excursion
    (goto-char (text-property-any (point-min) (point-max) 'shipit-pr-files t))
    (magit-current-section)))

(ert-deftest test-files-hide-runs-the-functions-that-follow-each-show ()
  "GIVEN a function that follows each show of Files Changed
WHEN Files Changed is closed and opened again, and a file in it opened
THEN the function ran once, for the one show of Files Changed."
  (test-files-hide--in-pr-buffer
    (let* ((runs 0)
           (shipit-files-section-shown-hook (list (lambda () (cl-incf runs))))
           (files (test-files-hide--files-section)))
      (magit-section-hide files)
      (magit-section-show files)
      (magit-section-show (car (oref files children)))
      (should (= 1 runs)))))

(ert-deftest test-files-hide-a-filter-added-to-it-shows-only-its-files ()
  "GIVEN a filter another package adds, for files named Button
WHEN Files Changed shows only what that filter shows
THEN only the Button file shows, and the filter line says what shows."
  (test-files-hide--in-pr-buffer
    (let ((shipit-files-show-only-filters
           (list (list 'buttons "button files"
                       (lambda (name) (string-match-p "Button" name))))))
      (shipit-files-show-only 'buttons)
      (should (equal '("src/Button.tsx") (test-files-hide--shown)))
      (should (string-match-p "Filter: button files" (buffer-string))))))

(provide 'test-shipit-files-hide)
;;; test-shipit-files-hide.el ends here
