;;; test-shipit-actions-timeline.el --- An Actions run on a time axis -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The run timeline draws the jobs of one GitHub Actions run on a shared
;; time axis.  Placing jobs and steps in time, choosing the ticks and
;; writing times are data work and run on any Emacs.  The layout, the
;; buffer, its keys and the export need the Emacs 32 canvas, and those
;; tests skip themselves without it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-canvas)
(require 'shipit-actions-list)
(require 'shipit-actions-timeline)

;; Load canvas-diagram now, when this Emacs can: the helpers below set
;; its struct slots, and `load' expands `setf' as it reads the form.
(shipit-canvas-available-p)

(defconst test-timeline--run
  '((id . 100) (workflow_id . 1) (run_number . 5868) (name . "FPGA Diamond")
    (display_title . "DiamondWorkbench: Reintroduce denoise")
    (status . "completed") (conclusion . "failure") (head_branch . "master")
    (html_url . "https://github.com/owner/repo/actions/runs/100")
    (created_at . "2026-09-14T10:00:00Z") (updated_at . "2026-09-14T10:20:00Z"))
  "A finished run whose jobs overlap in time.")

(defconst test-timeline--buffer-name "*shipit-timeline: FPGA Diamond #5868*"
  "The name of the timeline buffer of the fixture run.")

(defun test-timeline--at (clock)
  "Return CLOCK, \"HH:MM:SS\", as an ISO time on the day of the fixture run."
  (and clock (format "2026-09-14T%sZ" clock)))

(defun test-timeline--seconds (clock)
  "Return CLOCK, \"HH:MM:SS\", as seconds since the epoch."
  (float-time (date-to-time (test-timeline--at clock))))

(defun test-timeline--step (number name conclusion started completed)
  "Return step NUMBER called NAME with CONCLUSION, run from STARTED to COMPLETED."
  (list (cons 'number number) (cons 'name name)
        (cons 'status (if completed "completed" "queued"))
        (cons 'conclusion conclusion)
        (cons 'started_at (test-timeline--at started))
        (cons 'completed_at (test-timeline--at completed))))

(defun test-timeline--job (id name conclusion started completed &optional steps)
  "Return job ID called NAME with CONCLUSION, run from STARTED to COMPLETED.
STEPS are its steps.  A job with no STARTED is still queued, and one with
no COMPLETED is still running."
  (list (cons 'id id) (cons 'name name)
        (cons 'status (cond (completed "completed") (started "in_progress") (t "queued")))
        (cons 'conclusion conclusion)
        (cons 'started_at (test-timeline--at started))
        (cons 'completed_at (test-timeline--at completed))
        (cons 'html_url (format "https://github.com/owner/repo/actions/runs/100/job/%d" id))
        (cons 'steps (vconcat steps))))

(defun test-timeline--jobs ()
  "Return the jobs of the fixture run: two side by side, one started late,
one with steps, and one that never started."
  (list (test-timeline--job 200 "Initial Setup" "success" "10:00:00" "10:00:20")
        (test-timeline--job 201 "Run host unit tests" "success" "10:00:20" "10:01:05"
                            (list (test-timeline--step 1 "Set up job" "success" "10:00:20" "10:00:25")
                                  (test-timeline--step 2 "Run tests" "success" "10:00:25" "10:01:05")
                                  (test-timeline--step 3 "Upload results" "skipped" nil nil)))
        (test-timeline--job 202 "Code Analysis" "success" "10:00:20" "10:17:34")
        (test-timeline--job 203 "Run Diamond camera tests" "failure" "10:05:00" "10:09:12")
        (test-timeline--job 204 "Deploy" nil nil nil)))

(defun test-timeline--spans (items)
  "Return the (START . END) of each of ITEMS, rows or steps as the model holds them."
  (mapcar (lambda (item) (cons (plist-get item :start) (plist-get item :end))) items))

;;;; Placing jobs in time

(ert-deftest test-timeline-places-jobs-from-the-first-start ()
  "GIVEN a run whose jobs started at different times
WHEN the jobs are placed in time
THEN each spans from its start to its end, in seconds from the first
start, in the order the run lists them, and a job that never started
has no place."
  (let ((rows (shipit-actions-timeline--rows (test-timeline--jobs)
                                             (test-timeline--seconds "10:30:00"))))
    (should (equal '("Initial Setup" "Run host unit tests" "Code Analysis"
                     "Run Diamond camera tests")
                   (mapcar (lambda (row) (cdr (assq 'name (plist-get row :job)))) rows)))
    (should (equal '((0.0 . 20.0) (20.0 . 65.0) (20.0 . 1054.0) (300.0 . 552.0))
                   (test-timeline--spans rows)))
    (should (equal '("success" "success" "success" "failure")
                   (mapcar (lambda (row) (plist-get row :status)) rows)))))

(ert-deftest test-timeline-a-running-job-ends-now ()
  "GIVEN a job that started and has not finished
WHEN it is placed in time
THEN it reaches as far as now."
  (let ((rows (shipit-actions-timeline--rows
               (list (test-timeline--job 300 "Build" nil "10:00:00" nil))
               (test-timeline--seconds "10:01:30"))))
    (should (equal '((0.0 . 90.0)) (test-timeline--spans rows)))
    (should (equal "in_progress" (plist-get (car rows) :status)))))

(ert-deftest test-timeline-places-steps-inside-their-job ()
  "GIVEN a job with steps, one of them skipped
WHEN the job is placed in time
THEN its steps are placed on the same axis, and the skipped step, which
never started, has no place."
  (let* ((rows (shipit-actions-timeline--rows (test-timeline--jobs)
                                              (test-timeline--seconds "10:30:00")))
         (host (nth 1 rows)))
    (should (equal '((20.0 . 25.0) (25.0 . 65.0))
                   (test-timeline--spans (plist-get host :steps))))))

(ert-deftest test-timeline-ticks-leave-room-for-their-labels ()
  "GIVEN a long run and a short run drawn at their scales
WHEN the time between two ticks is chosen
THEN it is the shortest round time that puts ticks far enough apart
for their labels."
  (should (equal 120 (shipit-actions-timeline--tick-interval (/ 900.0 1054))))
  (should (equal 5 (shipit-actions-timeline--tick-interval 30.0))))

(ert-deftest test-timeline-writes-a-time-the-way-the-list-does ()
  "GIVEN times from none to over an hour
WHEN they are written
THEN they read like the durations in the Actions list, with a whole
minute or hour written without its zero part, as a tick label wants."
  (should (equal "0s" (shipit-actions-timeline--clock 0)))
  (should (equal "45s" (shipit-actions-timeline--clock 45)))
  (should (equal "1m" (shipit-actions-timeline--clock 60)))
  (should (equal "1m 30s" (shipit-actions-timeline--clock 90)))
  (should (equal "1h 5m" (shipit-actions-timeline--clock 3900))))

(ert-deftest test-timeline-buffer-is-named-after-the-workflow-and-run ()
  "GIVEN a run
WHEN its timeline buffer is named
THEN the name carries the workflow and the run number."
  (should (equal test-timeline--buffer-name
                 (shipit-actions-timeline--buffer-name test-timeline--run))))

(ert-deftest test-timeline-colours-a-result-as-the-duration-bars-do ()
  "GIVEN the results a job can have
WHEN their colour is asked for
THEN it is the colour of the duration bars in the Actions list."
  (should (equal "#28a745" (shipit-actions--status-color "success")))
  (should (equal "#cb2431" (shipit-actions--status-color "failure")))
  (should (equal "#fd7e14" (shipit-actions--status-color "in_progress"))))

;;;; The layout

(defun test-timeline--canvas ()
  "Return a canvas spec to measure and draw on."
  (list 'image :type 'canvas :id (make-symbol "test-timeline")
        :data-width 1400 :data-height 600))

(defmacro test-timeline--with-layout (nodes diagram &rest body)
  "Lay the fixture run out, binding DIAGRAM and its NODES, and run BODY.
The run is drawn at one pixel a second.  Skips when this Emacs cannot
draw on a canvas."
  (declare (indent 2))
  `(progn
     (skip-unless (shipit-canvas-available-p))
     (let* ((shipit-actions-timeline-width 1054)
            (,diagram (shipit-actions-timeline--diagram))
            (spec (shipit-actions-timeline--spec test-timeline--run (test-timeline--jobs)
                                                 (test-timeline--seconds "10:30:00")))
            (ctx (canvas-cairo-context (test-timeline--canvas))))
       (unwind-protect
           (progn
             (setf (canvas-diagram-spec ,diagram) spec
                   (canvas-diagram-model ,diagram) (shipit-actions-timeline--build ,diagram spec))
             (let ((,nodes (shipit-actions-timeline--lay-out ,diagram ctx)))
               (setf (canvas-diagram-nodes ,diagram) ,nodes)
               ,@body))
         (canvas-cairo-destroy ctx)))))

(defun test-timeline--node (nodes name)
  "Return the box of NODES for the job called NAME."
  (cl-find name nodes :key #'canvas-diagram-node-label :test #'equal))

(ert-deftest test-timeline-lays-jobs-out-on-one-axis ()
  "GIVEN a run drawn at one pixel a second
WHEN it is laid out
THEN each job is a box on a row of its own, in order, starting where
its job started and as wide as it ran, right of a column of names."
  (test-timeline--with-layout nodes diagram
    (let ((setup (test-timeline--node nodes "Initial Setup"))
          (host (test-timeline--node nodes "Run host unit tests"))
          (code (test-timeline--node nodes "Code Analysis"))
          (camera (test-timeline--node nodes "Run Diamond camera tests")))
      (should (equal 4 (length nodes)))
      (should (> (canvas-diagram-node-x setup) 0))
      (should (= (canvas-diagram-node-x host) (canvas-diagram-node-x code)))
      (should (= 300 (round (- (canvas-diagram-node-x camera) (canvas-diagram-node-x setup)))))
      (should (= 1034 (round (canvas-diagram-node-w code))))
      (should (= 252 (round (canvas-diagram-node-w camera))))
      (should (< (canvas-diagram-node-y setup) (canvas-diagram-node-y host)
                 (canvas-diagram-node-y code) (canvas-diagram-node-y camera))))))

(ert-deftest test-timeline-fills-a-job-by-its-result ()
  "GIVEN a run with jobs that passed and one that failed
WHEN the boxes are filled and the legend is drawn
THEN a box takes the colour of its result, and the legend names the
results in use."
  (test-timeline--with-layout nodes diagram
    (should (equal (canvas-diagram-rgb "#cb2431")
                   (shipit-actions-timeline--node-rgb
                    diagram (test-timeline--node nodes "Run Diamond camera tests"))))
    (should (equal '("success" "failure")
                   (mapcar #'car (shipit-actions-timeline--legend diagram))))))

(ert-deftest test-timeline-card-and-header-say-when-a-job-ran ()
  "GIVEN a job with steps
WHEN its header and its card are shown
THEN they say when it started and how long it took, and the card lists
every step."
  (test-timeline--with-layout nodes diagram
    (let ((host (test-timeline--node nodes "Run host unit tests")))
      (should (string-match-p "Run host unit tests" (shipit-actions-timeline--header diagram host)))
      (should (string-match-p "45s" (shipit-actions-timeline--header diagram host)))
      (pcase-let ((`(,title ,path ,body) (shipit-actions-timeline--card diagram host)))
        (should (equal "Run host unit tests" title))
        (should (string-match-p "#5868" path))
        (should (string-match-p "45s" body))
        (should (string-match-p "Run tests" body))
        (should (string-match-p "Upload results" body))))))

;;;; The buffer and its keys

(defmacro test-timeline--with-buffer (buffer job-id &rest body)
  "Open the fixture run's timeline as BUFFER on JOB-ID, and run BODY there.
Skips when this Emacs cannot draw on a canvas."
  (declare (indent 2))
  `(progn
     (skip-unless (shipit-canvas-available-p))
     (let ((,buffer (shipit-actions-timeline-show "owner/repo" test-timeline--run
                                                  (test-timeline--jobs) ,job-id)))
       (unwind-protect
           (with-current-buffer ,buffer ,@body)
         (kill-buffer ,buffer)))))

(defun test-timeline--press (keys)
  "Run the command KEYS is bound to in the current buffer."
  (call-interactively (key-binding keys)))

(ert-deftest test-timeline-opens-on-the-job-asked-for ()
  "GIVEN a run opened for one of its jobs
WHEN the timeline is shown
THEN the keyboard is on that job."
  (test-timeline--with-buffer buffer 203
    (should (equal test-timeline--buffer-name (buffer-name buffer)))
    (should (derived-mode-p 'shipit-actions-timeline-mode))
    (should (equal "Run Diamond camera tests"
                   (canvas-diagram-node-label (canvas-diagram-selected))))))

(ert-deftest test-timeline-ret-opens-the-job-in-the-run-viewer ()
  "GIVEN the keyboard on a job
WHEN RET is pressed
THEN the run viewer opens on that run with the job expanded."
  (let ((opened nil))
    (test-timeline--with-buffer buffer 203
      (cl-letf (((symbol-function 'shipit-open-actions-run)
                 (lambda (&rest args) (setq opened args))))
        (test-timeline--press (kbd "RET"))
        (should (equal '("owner/repo" 100 "Run Diamond camera tests") opened))))))

(ert-deftest test-timeline-g-reads-the-jobs-again ()
  "GIVEN a timeline
WHEN g is pressed
THEN the run's jobs are fetched again and drawn as they now stand."
  (let ((asked nil))
    (test-timeline--with-buffer buffer nil
      (cl-letf (((symbol-function 'shipit--api-request)
                 (lambda (endpoint _params callback)
                   (setq asked endpoint)
                   (funcall callback
                            `((total_count . 1)
                              (jobs . ,(vector (test-timeline--job
                                                205 "Rerun" "success" "10:30:00" "10:31:00"))))))))
        (test-timeline--press (kbd "g"))
        (should (equal "/repos/owner/repo/actions/runs/100/jobs" asked))
        (should (equal '("Rerun")
                       (mapcar #'canvas-diagram-node-label (canvas-diagram-nodes-shown))))))))

(ert-deftest test-timeline-exports-a-picture ()
  "GIVEN a run
WHEN its timeline is exported
THEN a PNG is written with pixels in it, without a frame or a buffer."
  (skip-unless (shipit-canvas-available-p))
  (let ((file (make-temp-file "test-timeline-" nil ".png")))
    (unwind-protect
        (progn
          (shipit-actions-timeline-export test-timeline--run (test-timeline--jobs) file)
          (should (> (file-attribute-size (file-attributes file)) 0)))
      (delete-file file))))

;;;; From the Actions list

(defmacro test-timeline--in-actions-list (&rest body)
  "Run BODY in an Actions list showing the fixture run, its jobs fetched.
Point starts on the run's line, with the run expanded."
  `(let ((buf (generate-new-buffer " *test-timeline-actions*")))
     (unwind-protect
         (with-current-buffer buf
           (shipit-actions-list-mode)
           (setq shipit-actions-list--repo "owner/repo"
                 shipit-actions-list--workflows '(((id . 1) (name . "FPGA Diamond")
                                                   (state . "active")))
                 shipit-actions-list--all-runs (list test-timeline--run)
                 shipit-actions-list--runners nil)
           (puthash 100 (test-timeline--jobs) shipit-actions-list--run-jobs)
           (shipit-actions-list--render)
           (goto-char (point-min))
           (search-forward "#5868")
           (magit-section-show (magit-current-section))
           ,@body)
       (kill-buffer buf))))

(ert-deftest test-timeline-v-is-bound-in-the-actions-list ()
  "GIVEN the Actions list
WHEN v is looked up
THEN it draws the run at point on a time axis."
  (should (eq 'shipit-actions-list-timeline
              (lookup-key shipit-actions-list-mode-map (kbd "v")))))

(ert-deftest test-timeline-v-on-a-job-opens-its-run-on-that-job ()
  "GIVEN point on a job of a run in the Actions list
WHEN v is pressed
THEN the run's timeline opens with the keyboard on that job."
  (skip-unless (shipit-canvas-available-p))
  (test-timeline--in-actions-list
    (search-forward "Run Diamond camera tests")
    (beginning-of-line)
    (unwind-protect
        (progn
          (call-interactively #'shipit-actions-list-timeline)
          (with-current-buffer test-timeline--buffer-name
            (should (equal "Run Diamond camera tests"
                           (canvas-diagram-node-label (canvas-diagram-selected))))))
      (when (get-buffer test-timeline--buffer-name)
        (kill-buffer test-timeline--buffer-name)))))

(ert-deftest test-timeline-v-away-from-a-run-says-so ()
  "GIVEN point on no run in the Actions list
WHEN v is pressed
THEN it says point is not on a run."
  (test-timeline--in-actions-list
    (goto-char (point-min))
    (should-error (call-interactively #'shipit-actions-list-timeline) :type 'user-error)))

(ert-deftest test-timeline-v-without-a-canvas-says-so ()
  "GIVEN an Emacs without the canvas
WHEN v is pressed on a run
THEN it says the view needs Emacs 32, and signals nothing."
  (test-timeline--in-actions-list
    (let ((said nil))
      (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format-message fmt args)))))
        (call-interactively #'shipit-actions-list-timeline)
        (should (string-match-p "Emacs 32" said))))))

(provide 'test-shipit-actions-timeline)
;;; test-shipit-actions-timeline.el ends here
