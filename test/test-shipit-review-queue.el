;;; test-shipit-review-queue.el --- The queue of draft review comments -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A draft review comment waits in the queue until the review is
;; submitted.  It shows under its diff line in Files Changed, and in the
;; Pending Review section with the diff lines around it.  The tests draw
;; Files Changed of a pull request in a real buffer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit-pr-sections)
(require 'shipit-buffer)
(require 'shipit-review-queue)

(defconst test-review-queue--pr
  `((number . 1) (head (sha . "abc123") (ref . "work")) (base (ref . "main"))
    (files ((filename . "src/parser.el") (status . "modified")
            (additions . 1) (deletions . 0) (changes . 1)
            (patch . "@@ -1,3 +1,4 @@\n (defun parse ()\n+  (read-header)\n   (read-body))\n ;; end"))))
  "A pull request of one file, with a line added at line 2.")

(defconst test-review-queue--draft
  (list :id 1 :file "src/parser.el" :line 2 :side "RIGHT"
        :body "Read the header only once.")
  "A draft comment on the added line.")

(defun test-review-queue--open-files ()
  "Open the Files Changed section, as TAB on it does.  It starts closed."
  (save-excursion
    (goto-char (or (text-property-any (point-min) (point-max) 'shipit-pr-files t)
                   (error "No Files Changed section in the buffer")))
    (magit-section-show (magit-current-section))))

(defmacro test-review-queue--in-closed-pr-buffer (&rest body)
  "Run BODY in a pull request buffer that shows `test-review-queue--pr'.
The queue holds `test-review-queue--draft', and Files Changed is closed."
  (declare (indent 0))
  `(with-temp-buffer
     (shipit-mode)
     (setq shipit-buffer-repo "o/r"
           shipit-buffer-pr-number 1
           shipit-buffer-pr-data test-review-queue--pr
           shipit-review-queue (list (copy-sequence test-review-queue--draft)))
     (let ((inhibit-read-only t))
       (magit-insert-section (root)
         (shipit--render-files-section (cdr (assq 'files test-review-queue--pr))
                                       "o/r" 1 test-review-queue--pr "abc123")))
     ,@body))

(defmacro test-review-queue--in-pr-buffer (&rest body)
  "As `test-review-queue--in-closed-pr-buffer', with Files Changed open."
  (declare (indent 0))
  `(test-review-queue--in-closed-pr-buffer
     (test-review-queue--open-files)
     ,@body))

(ert-deftest test-review-queue-a-draft-shows-under-its-line ()
  "GIVEN a draft comment on line 2 of a file
WHEN the queue is drawn in Files Changed
THEN the draft shows under that line."
  (test-review-queue--in-pr-buffer
    (let ((inhibit-read-only t))
      (shipit-review-queue--render))
    (should (= 1 (length shipit-review-queue--sections)))
    (should (string-match-p "(read-header)\n.*\n?.*Read the header only once"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-review-queue-a-draft-shows-the-code-it-is-on ()
  "GIVEN a draft comment on line 2 of a file
WHEN the Pending Review section asks for the code around it
THEN it gets the diff lines around line 2."
  (test-review-queue--in-pr-buffer
    (let ((context (shipit-review-queue--code-context "src/parser.el" 2)))
      (should context)
      (should (string-match-p "(read-header)" context)))))

(ert-deftest test-review-queue-a-long-draft-wraps-in-pending-review ()
  "GIVEN a draft comment longer than a line
WHEN the Pending Review section is drawn
THEN the comment wraps below its heading, each line under the same indent."
  (test-review-queue--in-pr-buffer
    (setq shipit-review-queue
          (list (plist-put (copy-sequence test-review-queue--draft)
                           :body (mapconcat #'identity (make-list 30 "word") " "))))
    (let ((fill-column 60)
          (inhibit-read-only t))
      (goto-char (point-max))
      (let ((magit-insert-section--parent magit-root-section))
        (shipit-review-queue--insert-section-at-point)))
    (let ((lines (seq-filter (lambda (line) (string-match-p "\\` +word" line))
                             (split-string (buffer-substring-no-properties (point-min) (point-max))
                                           "\n"))))
      (should (< 1 (length lines)))
      (should (cl-every (lambda (line) (string-prefix-p "         word" line)) lines)))))

(ert-deftest test-review-queue-a-file-with-a-draft-is-marked-when-files-open ()
  "GIVEN a draft comment on a file, and Files Changed closed
WHEN Files Changed is opened
THEN the heading of the file is marked as having a draft."
  (test-review-queue--in-closed-pr-buffer
    (test-review-queue--open-files)
    (should (= 1 (length shipit-review-queue--badge-overlays)))))

(provide 'test-shipit-review-queue)
;;; test-shipit-review-queue.el ends here
