;;; test-shipit-run-steps.el --- Drawing the steps of a workflow run -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The jobs and steps of a workflow run, with the log lines of each
;; step, are drawn in two places: the actions buffer of the run, and
;; under a check item in the Checks section of a pull request.  Other
;; packages add sections under the log lines once they are drawn.  The
;; tests draw both in real buffers, with the logs already fetched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit-buffer)
(require 'shipit-actions)
(require 'shipit-checks)

(defconst test-run-steps--job
  '((id . 11) (name . "build") (status . "completed") (conclusion . "failure")
    (started_at . "2026-09-01T10:00:00Z") (completed_at . "2026-09-01T10:01:00Z")
    (steps ((number . 1) (name . "Compile") (status . "completed") (conclusion . "failure")
            (started_at . "2026-09-01T10:00:00Z") (completed_at . "2026-09-01T10:01:00Z"))))
  "A job of one step that failed.")

(defconst test-run-steps--logs
  '((1 ("2026-09-01T10:00:01Z" . "cc main.c")
       ("2026-09-01T10:00:02Z" . "error: parser.h not found")))
  "The log lines of the job's step, by step number.")

(defconst test-run-steps--run
  '((id . 7) (name . "CI") (status . "completed") (conclusion . "failure")
    (event . "pull_request") (run_number . 3)
    (created_at . "2026-09-01T10:00:00Z") (updated_at . "2026-09-01T10:01:00Z"))
  "A run of the job, for pull request 1 of o/r.")

(defmacro test-run-steps--in-actions-buffer (&rest body)
  "Run BODY in the actions buffer of `test-run-steps--run', before it is drawn."
  (declare (indent 0))
  `(with-temp-buffer
     (shipit-actions-mode)
     (setq shipit-actions--repo "o/r"
           shipit-actions--pr-number 1
           shipit-actions--run-id 7
           shipit-actions--run-data test-run-steps--run
           shipit-actions--jobs-data (list test-run-steps--job))
     (puthash 11 test-run-steps--logs shipit-actions--job-logs)
     (puthash 11 t shipit-actions--job-logs-state)
     ,@body))

(defun test-run-steps--insert-check-item ()
  "Insert the job as a check item under its run; return the check item."
  (let ((inhibit-read-only t)
        (item nil))
    (magit-insert-section (root)
      (magit-insert-section (workflow-run '((run-id . 7)))
        (magit-insert-heading "CI")
        (setq item (magit-insert-section (check-item test-run-steps--job)
                     (magit-insert-heading "build")
                     (insert "\n")))))
    item))

(defmacro test-run-steps--in-check-item (item &rest body)
  "Run BODY in pull request 1 of o/r, with ITEM bound to the job's check item.
The steps of the check item are not drawn yet."
  (declare (indent 1))
  `(with-temp-buffer
     (shipit-mode)
     (setq shipit-buffer-repo "o/r"
           shipit-buffer-pr-number 1)
     (puthash 11 test-run-steps--logs shipit--check-item-logs)
     (let ((,item (test-run-steps--insert-check-item)))
       ,@body)))

(ert-deftest test-run-steps-the-actions-buffer-runs-its-functions ()
  "GIVEN a function that follows the drawing of a run's steps
WHEN the actions buffer of a run of a pull request is drawn
THEN the function is called under the root, with the run of the pull
request."
  (test-run-steps--in-actions-buffer
    (let* ((calls nil)
           (shipit-run-steps-drawn-functions
            (list (lambda (root repo pr-number run-id)
                    (push (list root repo pr-number run-id) calls)))))
      (shipit-actions--render)
      (should (equal (list (list magit-root-section "o/r" 1 7)) calls)))))

(ert-deftest test-run-steps-a-check-item-runs-its-functions ()
  "GIVEN a function that follows the drawing of a run's steps
WHEN the steps of a check item of a pull request are drawn
THEN the function is called under the check item, with the run of the
pull request."
  (test-run-steps--in-check-item item
    (let* ((calls nil)
           (shipit-run-steps-drawn-functions
            (list (lambda (root repo pr-number run-id)
                    (push (list root repo pr-number run-id) calls)))))
      (shipit--render-check-item-steps item test-run-steps--job)
      (should (equal (list (list item "o/r" 1 7)) calls)))))

(defun test-run-steps--goto-log-line (text)
  "Put point on the log line that shows TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(defconst test-run-steps--second-line-location
  '(:repo "o/r" :pr-number 1 :run-id 7 :job-id 11 :step 1 :line 1)
  "Where the second log line of the step is, for the pull request.")

(ert-deftest test-run-steps-a-log-line-knows-where-it-is-in-the-actions-buffer ()
  "GIVEN the actions buffer of a run of a pull request, drawn
WHEN the location of the second log line of a step is asked for
THEN it names the pull request, the run, the job, the step and the line."
  (test-run-steps--in-actions-buffer
    (shipit-actions--render)
    (test-run-steps--goto-log-line "error: parser.h not found")
    (should (equal test-run-steps--second-line-location
                   (shipit-actions-log-location-at-point)))))

(ert-deftest test-run-steps-a-log-line-knows-where-it-is-under-a-check-item ()
  "GIVEN the steps of a check item of a pull request, drawn
WHEN the location of the second log line of a step is asked for
THEN it is the same as in the actions buffer of the run."
  (test-run-steps--in-check-item item
    (shipit--render-check-item-steps item test-run-steps--job)
    (test-run-steps--goto-log-line "error: parser.h not found")
    (should (equal test-run-steps--second-line-location
                   (shipit-actions-log-location-at-point)))))

(provide 'test-shipit-run-steps)
;;; test-shipit-run-steps.el ends here
