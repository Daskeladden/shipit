;;; test-shipit-actions-list.el --- Tests for actions list buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the GitHub Actions workflows listing buffer.

;;; Code:
(require 'ert)
(require 'shipit-actions-list)



(ert-deftest test-actions-list-renders-workflows ()
  "GIVEN workflow and run data set in buffer-local vars
   WHEN the render function is called
   THEN workflow names appear in the buffer."
  (let ((buf (generate-new-buffer "*test-actions-list*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))
                  ((id . 2) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (status . "completed") (conclusion . "success"))
                  ((id . 101) (workflow_id . 2) (status . "completed") (conclusion . "failure"))))
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          (goto-char (point-min))
          (should (search-forward "Actions: owner/repo" nil t))
          (should (search-forward "Workflows" nil t))
          (should (search-forward "All workflows" nil t))
          (should (search-forward "CI" nil t))
          (should (search-forward "Deploy" nil t))
          (should (search-forward "Runners" nil t)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-runs-for-workflow ()
  "GIVEN cached runs for a workflow
   WHEN runs-for-workflow is called
   THEN cached runs are returned."
  (let ((buf (generate-new-buffer "*test-runs-for-wf*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (puthash 10
                   '(((id . 1) (workflow_id . 10))
                     ((id . 3) (workflow_id . 10)))
                   shipit-actions-list--workflow-runs)
          (let ((result (shipit-actions-list--runs-for-workflow 10)))
            (should (= (length result) 2))
            (should (= (cdr (assq 'id (car result))) 1))
            (should (= (cdr (assq 'id (cadr result))) 3))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-renders-empty-workflows ()
  "GIVEN no workflow data
   WHEN the render function is called
   THEN the buffer still renders with headers."
  (let ((buf (generate-new-buffer "*test-actions-list-empty*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows nil)
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          (goto-char (point-min))
          (should (search-forward "Actions: owner/repo" nil t))
          (should (search-forward "Workflows" nil t))
          (should (search-forward "All workflows" nil t))
          (should (search-forward "Runners" nil t)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-workflow-entry-shows-run-count ()
  "GIVEN a workflow with cached runs
   WHEN the workflow entry is rendered
   THEN the recent run count appears in the buffer."
  (let ((buf (generate-new-buffer "*test-actions-list-count*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (status . "completed") (conclusion . "success"))
                  ((id . 101) (workflow_id . 1) (status . "completed") (conclusion . "failure"))))
          (setq shipit-actions-list--runners nil)
          ;; Pre-populate per-workflow cache
          (puthash 1
                   '(((id . 100) (workflow_id . 1) (status . "completed") (conclusion . "success"))
                     ((id . 101) (workflow_id . 1) (status . "completed") (conclusion . "failure")))
                   shipit-actions-list--workflow-runs)
          (shipit-actions-list--render)
          (goto-char (point-min))
          ;; The CI entry should show "2 runs"
          (should (search-forward "2 runs" nil t)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-workflow-shows-active-and-recent ()
  "GIVEN a workflow with active and cached recent runs
   WHEN the workflow entry is rendered
   THEN both active and recent counts appear."
  (let ((buf (generate-new-buffer "*test-actions-active*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          ;; GIVEN one in-progress run in all-runs
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1)
                   (status . "in_progress") (conclusion . nil))))
          (setq shipit-actions-list--runners nil)
          ;; GIVEN 3 cached recent runs
          (puthash 1
                   '(((id . 100) (workflow_id . 1) (status . "in_progress"))
                     ((id . 101) (workflow_id . 1) (status . "completed") (conclusion . "success"))
                     ((id . 102) (workflow_id . 1) (status . "completed") (conclusion . "success")))
                   shipit-actions-list--workflow-runs)
          (shipit-actions-list--render)
          (goto-char (point-min))
          ;; THEN heading shows "1 active | 3 runs"
          (should (search-forward "1 active | 3 runs" nil t)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-refresh-clears-data ()
  "GIVEN a buffer with cached workflow data
   WHEN refresh is called (with mocked fetch)
   THEN cached data is cleared before re-fetching."
  (let ((buf (generate-new-buffer "*test-actions-refresh*"))
        (fetch-called nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows '(((id . 1) (name . "old"))))
          (setq shipit-actions-list--all-runs '(((id . 1))))
          (setq shipit-actions-list--runners '(((id . 1))))
          ;; Mock the fetch to just record it was called
          (cl-letf (((symbol-function 'shipit-actions-list--fetch-and-render)
                     (lambda () (setq fetch-called t))))
            (shipit-actions-list-refresh)
            ;; Data should be cleared
            (should-not shipit-actions-list--workflows)
            (should-not shipit-actions-list--all-runs)
            (should-not shipit-actions-list--runners)
            (should fetch-called)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-browse-opens-url ()
  "GIVEN a buffer with a repo set
   WHEN browse is called
   THEN the correct GitHub Actions URL is opened."
  (let ((buf (generate-new-buffer "*test-actions-browse*"))
        (opened-url nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (cl-letf (((symbol-function 'browse-url)
                     (lambda (url) (setq opened-url url))))
            (shipit-actions-list-browse)
            (should (string= opened-url "https://github.com/owner/repo/actions"))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-insert-run ()
  "GIVEN a run alist
   WHEN shipit-actions-list--insert-runs renders it
   THEN run number, branch, and commit message appear."
  (let ((runs '(((id . 100)
                 (run_number . 42)
                 (head_branch . "main")
                 (workflow_id . 1)
                 (status . "completed")
                 (conclusion . "success")
                 (created_at . "2026-03-09T10:00:00Z")
                 (updated_at . "2026-03-09T10:02:30Z")
                 (head_commit . ((message . "fix login bug")))))))
    (with-temp-buffer
      (require 'magit-section)
      (let ((magit-insert-section--parent
             (magit-insert-section (actions-list-root))))
        (shipit-actions-list--insert-runs runs))
      (goto-char (point-min))
      (should (search-forward "#42" nil t))
      (should (search-forward "main" nil t))
      (should (search-forward "fix login bug" nil t)))))

(ert-deftest test-actions-list-format-age ()
  "GIVEN various timestamps
   WHEN shipit-actions-list--format-age is called
   THEN appropriate relative time strings are returned."
  (should (null (shipit-actions-list--format-age nil)))
  (should (null (shipit-actions-list--format-age :null)))
  ;; A timestamp from ~2 hours ago
  (let* ((two-hours-ago (- (float-time) 7200))
         (ts (format-time-string "%Y-%m-%dT%H:%M:%SZ" (seconds-to-time two-hours-ago) t)))
    (should (string-match-p "2h ago" (shipit-actions-list--format-age ts)))))

(ert-deftest test-actions-list-workflow-contains-runs ()
  "GIVEN workflows with runs
   WHEN the workflows section is rendered
   THEN run entries exist as children inside workflow sections."
  (let ((buf (generate-new-buffer "*test-actions-list-runs*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (run_number . 42) (workflow_id . 1)
                   (head_branch . "main") (status . "completed")
                   (conclusion . "success")
                   (created_at . "2026-03-09T10:00:00Z")
                   (updated_at . "2026-03-09T10:02:30Z")
                   (head_commit . ((message . "test commit"))))))
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          ;; The buffer text should contain the run info
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "#42" text))
            (should (string-match-p "main" text))
            (should (string-match-p "test commit" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-insert-run-jobs ()
  "GIVEN job data with durations
   WHEN shipit-actions-list--insert-run-jobs is called
   THEN job names and duration text appear in the buffer."
  (let ((jobs '(((id . 1) (name . "Build") (status . "completed")
                 (conclusion . "success")
                 (started_at . "2026-03-09T10:00:00Z")
                 (completed_at . "2026-03-09T10:00:30Z"))
                ((id . 2) (name . "Test") (status . "completed")
                 (conclusion . "failure")
                 (started_at . "2026-03-09T10:00:00Z")
                 (completed_at . "2026-03-09T10:01:12Z")))))
    (with-temp-buffer
      (require 'magit-section)
      (let ((magit-insert-section--parent
             (magit-insert-section (actions-list-root))))
        (shipit-actions-list--insert-run-jobs jobs))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Build" text))
        (should (string-match-p "Test" text))
        ;; Duration text should appear
        (should (string-match-p "30s" text))
        (should (string-match-p "1m 12s" text))))))

(ert-deftest test-actions-list-insert-run-jobs-with-zero-duration ()
  "GIVEN job data where all durations are zero
   WHEN shipit-actions-list--insert-run-jobs is called
   THEN job names appear without errors."
  (let ((jobs '(((id . 1) (name . "Setup") (status . "completed")
                 (conclusion . "success")
                 (started_at . "2026-03-09T10:00:00Z")
                 (completed_at . "2026-03-09T10:00:00Z")))))
    (with-temp-buffer
      (require 'magit-section)
      (let ((magit-insert-section--parent
             (magit-insert-section (actions-list-root))))
        (shipit-actions-list--insert-run-jobs jobs))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Setup" text))))))

(ert-deftest test-actions-list-insert-run-jobs-section-type ()
  "GIVEN job data
   WHEN shipit-actions-list--insert-run-jobs is called
   THEN sections have type actions-list-run-job with job alist as value."
  (let ((jobs '(((id . 42) (name . "Lint") (status . "completed")
                 (conclusion . "success")
                 (started_at . "2026-03-09T10:00:00Z")
                 (completed_at . "2026-03-09T10:00:15Z")))))
    (with-temp-buffer
      (require 'magit-section)
      (let* ((root (magit-insert-section (actions-list-root)))
             (magit-insert-section--parent root))
        (shipit-actions-list--insert-run-jobs jobs)
        ;; Find the job section
        (let ((children (oref root children)))
          (should (= (length children) 1))
          (should (eq (oref (car children) type) 'actions-list-run-job))
          (should (equal (cdr (assq 'id (oref (car children) value))) 42)))))))

(ert-deftest test-actions-list-run-renders-cached-jobs ()
  "GIVEN a run with cached jobs in shipit-actions-list--run-jobs
   WHEN shipit-actions-list--insert-run renders the run
   THEN job names appear as children of the run section."
  (let ((buf (generate-new-buffer "*test-cached-jobs*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 500) (run_number . 99) (workflow_id . 1)
                   (head_branch . "main") (status . "completed")
                   (conclusion . "success")
                   (created_at . "2026-03-09T10:00:00Z")
                   (updated_at . "2026-03-09T10:02:30Z")
                   (head_commit . ((message . "test"))))))
          (puthash 500
                   '(((id . 1) (name . "Build") (status . "completed")
                      (conclusion . "success")
                      (started_at . "2026-03-09T10:00:00Z")
                      (completed_at . "2026-03-09T10:00:45Z"))
                     ((id . 2) (name . "Deploy") (status . "completed")
                      (conclusion . "success")
                      (started_at . "2026-03-09T10:01:00Z")
                      (completed_at . "2026-03-09T10:02:00Z")))
                   shipit-actions-list--run-jobs)
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Build" text))
            (should (string-match-p "Deploy" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-fetch-run-jobs ()
  "GIVEN a mocked API that returns jobs
   WHEN shipit-actions-list--fetch-run-jobs is called
   THEN the callback receives the jobs list and they are cached."
  (let ((buf (generate-new-buffer "*test-fetch-jobs*"))
        (received-jobs nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (cl-letf (((symbol-function 'shipit-actions--fetch-jobs)
                     (lambda (_repo _run-id callback)
                       (funcall callback
                                (vector
                                 '((id . 10) (name . "Build"))
                                 '((id . 11) (name . "Test")))))))
            (shipit-actions-list--fetch-run-jobs
             999
             (lambda (jobs) (setq received-jobs jobs)))
            ;; Jobs should be received
            (should (= (length received-jobs) 2))
            (should (equal (cdr (assq 'name (car received-jobs))) "Build"))
            ;; Jobs should be cached
            (should (gethash 999 shipit-actions-list--run-jobs))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-dwim-run-fetches-jobs ()
  "GIVEN a run section with no cached jobs
   WHEN dwim is called on it
   THEN jobs are fetched and inserted into the section in-place."
  (let ((buf (generate-new-buffer "*test-dwim-fetch*"))
        (fetch-called nil)
        (insert-called nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows nil)
          (setq shipit-actions-list--all-runs
                '(((id . 500) (run_number . 1) (workflow_id . 1)
                   (head_branch . "main") (status . "completed")
                   (conclusion . "success")
                   (created_at . "2026-03-09T10:00:00Z")
                   (updated_at . "2026-03-09T10:00:30Z")
                   (head_commit . ((message . "test"))))))
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          ;; Navigate to the run section
          (goto-char (point-min))
          (search-forward "#1" nil t)
          (beginning-of-line)
          ;; Mock fetch and in-place insert
          (cl-letf (((symbol-function 'shipit-actions-list--fetch-run-jobs)
                     (lambda (run-id callback)
                       (setq fetch-called run-id)
                       (funcall callback '(((id . 1) (name . "Build"))))))
                    ((symbol-function 'shipit-actions-list--insert-jobs-into-section)
                     (lambda (_section _jobs) (setq insert-called t))))
            (shipit-actions-list-dwim)
            (should (= fetch-called 500))
            (should insert-called)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-dwim-run-toggles-when-cached ()
  "GIVEN a run section with cached jobs
   WHEN dwim is called on it
   THEN the section is toggled (not re-fetched)."
  (let ((buf (generate-new-buffer "*test-dwim-toggle*"))
        (toggle-called nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows nil)
          (setq shipit-actions-list--all-runs
                '(((id . 500) (run_number . 1) (workflow_id . 1)
                   (head_branch . "main") (status . "completed")
                   (conclusion . "success")
                   (created_at . "2026-03-09T10:00:00Z")
                   (updated_at . "2026-03-09T10:00:30Z")
                   (head_commit . ((message . "test"))))))
          ;; Pre-cache jobs
          (puthash 500 '(((id . 1) (name . "Build")))
                   shipit-actions-list--run-jobs)
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          ;; Navigate to the run section
          (goto-char (point-min))
          (search-forward "#1" nil t)
          (beginning-of-line)
          ;; Mock toggle
          (cl-letf (((symbol-function 'magit-section-toggle)
                     (lambda (_section) (setq toggle-called t))))
            (shipit-actions-list-dwim)
            (should toggle-called)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-renders-runners ()
  "GIVEN runner API data
   WHEN the runners section is rendered
   THEN runners appear with name, OS, and status."
  (let ((buf (generate-new-buffer "*test-runners*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows nil)
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners
                '(((id . 1) (name . "build-01") (os . "Linux")
                   (status . "online") (busy . nil)
                   (labels . [((name . "self-hosted")) ((name . "linux"))]))
                  ((id . 2) (name . "build-02") (os . "Linux")
                   (status . "offline") (busy . nil)
                   (labels . [((name . "self-hosted")) ((name . "linux"))]))))
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Runners" text))
            (should (string-match-p "Self-hosted" text))
            (should (string-match-p "build-01" text))
            (should (string-match-p "online" text))
            (should (string-match-p "build-02" text))
            (should (string-match-p "offline" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-no-runners ()
  "GIVEN no runner data
   WHEN the runners section is rendered
   THEN a placeholder message appears."
  (let ((buf (generate-new-buffer "*test-no-runners*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows nil)
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "no self-hosted runners" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-runner-busy-status ()
  "GIVEN a runner that is online and busy
   WHEN the runners section is rendered
   THEN the runner shows busy status."
  (let ((buf (generate-new-buffer "*test-runners-busy*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows nil)
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners
                '(((id . 3) (name . "build-03") (os . "macOS")
                   (status . "online") (busy . t)
                   (labels . [((name . "self-hosted")) ((name . "macos"))]))))
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "build-03" text))
            (should (string-match-p "busy" text))
            (should (string-match-p "macOS" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-filter-by-text ()
  "GIVEN a rendered buffer with workflows CI and Deploy
   WHEN filter text is set to 'dep'
   THEN only Deploy is shown, All workflows is hidden."
  (let ((buf (generate-new-buffer "*test-filter*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))
                  ((id . 2) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (setq shipit-actions-list--filter-text "dep")
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Deploy" text))
            (should-not (string-match-p "\\bCI\\b" text))
            (should-not (string-match-p "All workflows" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-filter-empty-shows-all ()
  "GIVEN no filter text
   WHEN the buffer is rendered
   THEN all workflows and 'All workflows' entry are shown."
  (let ((buf (generate-new-buffer "*test-filter-clear*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))
                  ((id . 2) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (setq shipit-actions-list--filter-text nil)
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "All workflows" text))
            (should (string-match-p "CI" text))
            (should (string-match-p "Deploy" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-filter-case-insensitive ()
  "GIVEN workflows CI and Deploy
   WHEN filter text is 'CI' (uppercase)
   THEN CI workflow matches case-insensitively."
  (let ((buf (generate-new-buffer "*test-filter-case*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))
                  ((id . 2) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (setq shipit-actions-list--filter-text "ci")
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "CI" text))
            (should-not (string-match-p "Deploy" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-workflow-matches-filter ()
  "GIVEN various workflow names and filter texts
   WHEN shipit-actions-list--workflow-matches-filter-p is called
   THEN it matches case-insensitively on substrings."
  ;; Match substring
  (should (shipit-actions-list--workflow-matches-filter-p
           '((name . "Deploy to Production")) "prod"))
  ;; Case insensitive
  (should (shipit-actions-list--workflow-matches-filter-p
           '((name . "CI")) "ci"))
  ;; No match
  (should-not (shipit-actions-list--workflow-matches-filter-p
               '((name . "CI")) "deploy"))
  ;; Empty filter matches everything
  (should (shipit-actions-list--workflow-matches-filter-p
           '((name . "CI")) ""))
  ;; Nil filter matches everything
  (should (shipit-actions-list--workflow-matches-filter-p
           '((name . "CI")) nil)))

(ert-deftest test-actions-list-clear-filter ()
  "GIVEN an active filter
   WHEN shipit-actions-list-clear-filter is called
   THEN filter text is cleared and all workflows are shown."
  (let ((buf (generate-new-buffer "*test-clear-filter*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))
                  ((id . 2) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (setq shipit-actions-list--filter-text "CI")
          (shipit-actions-list--render)
          ;; Only CI visible
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should-not (string-match-p "Deploy" text)))
          ;; Clear filter
          (shipit-actions-list-clear-filter)
          (should (null shipit-actions-list--filter-text))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "CI" text))
            (should (string-match-p "Deploy" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-hide-inactive-workflows ()
  "GIVEN workflows where only one has active runs
   WHEN hide-inactive is toggled on
   THEN only the workflow with active runs is shown."
  (let ((buf (generate-new-buffer "*test-hide-inactive*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))
                  ((id . 2) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (status . "in_progress")
                   (run_number . 5) (head_branch . "main")
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))
                  ((id . 101) (workflow_id . 2) (status . "completed")
                   (conclusion . "success") (run_number . 3)
                   (head_branch . "main")
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))))
          (setq shipit-actions-list--runners nil)
          ;; Both visible initially
          (shipit-actions-list--render)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "CI" text))
            (should (string-match-p "Deploy" text)))
          ;; Toggle hide inactive
          (shipit-actions-list-toggle-hide-inactive)
          (should shipit-actions-list--hide-inactive)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "CI" text))
            (should-not (string-match-p "Deploy" text)))
          ;; Toggle back
          (shipit-actions-list-toggle-hide-inactive)
          (should-not shipit-actions-list--hide-inactive)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "CI" text))
            (should (string-match-p "Deploy" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-workflow-has-active-runs ()
  "GIVEN runs with various statuses
   WHEN checking if a workflow has active runs
   THEN only in_progress/queued/waiting are considered active."
  (let ((buf (generate-new-buffer "*test-active-runs*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (status . "in_progress"))
                  ((id . 101) (workflow_id . 2) (status . "completed")
                   (conclusion . "success"))))
          ;; Workflow 1 has active runs
          (should (shipit-actions-list--workflow-has-active-runs-p 1))
          ;; Workflow 2 does not
          (should-not (shipit-actions-list--workflow-has-active-runs-p 2))
          ;; Unknown workflow has none
          (should-not (shipit-actions-list--workflow-has-active-runs-p 99)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-fetch-more-appends-runs ()
  "GIVEN existing runs in the buffer
   WHEN fetch-more retrieves additional runs from API
   THEN new runs are appended to existing runs."
  (let ((buf (generate-new-buffer "*test-fetch-more*"))
        (api-called-with nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 10)
                   (status . "completed") (conclusion . "success"))))
          (setq shipit-actions-list--runners nil)
          ;; Mock API request
          (cl-letf (((symbol-function 'shipit--api-request)
                     (lambda (endpoint params callback)
                       (setq api-called-with (list endpoint params))
                       (when callback
                         (funcall callback
                                  '((workflow_runs .
                                     [((id . 200) (workflow_id . 1) (run_number . 9)
                                       (status . "completed") (conclusion . "success"))
                                      ((id . 201) (workflow_id . 1) (run_number . 8)
                                       (status . "completed") (conclusion . "failure"))])))))))
            (shipit-actions-list--fetch-more-runs nil 10)
            ;; Should have 3 runs now (1 existing + 2 new)
            (should (= (length shipit-actions-list--all-runs) 3))
            ;; API should have been called with correct endpoint
            (should (string-match-p "actions/runs" (car api-called-with)))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-fetch-more-with-workflow-filter ()
  "GIVEN a workflow filter is active
   WHEN fetch-more is called
   THEN API request uses the per-workflow endpoint."
  (let ((buf (generate-new-buffer "*test-fetch-more-wf*"))
        (api-endpoint nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1))))
          (setq shipit-actions-list--runners nil)
          ;; Mock API request
          (cl-letf (((symbol-function 'shipit--api-request)
                     (lambda (endpoint _params callback)
                       (setq api-endpoint endpoint)
                       (when callback
                         (funcall callback
                                  '((workflow_runs . [])))))))
            (shipit-actions-list--fetch-more-runs 1 10)
            ;; Endpoint should include workflow id in path
            (should (string-match-p "workflows/1/runs" api-endpoint))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-fetch-more-no-results ()
  "GIVEN no more runs available
   WHEN fetch-more is called
   THEN a message is shown and runs list is unchanged."
  (let ((buf (generate-new-buffer "*test-fetch-more-empty*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1))))
          (setq shipit-actions-list--runners nil)
          ;; Mock API request returning no runs
          (cl-letf (((symbol-function 'shipit--api-request)
                     (lambda (_endpoint _params callback)
                       (when callback
                         (funcall callback
                                  '((workflow_runs . [])))))))
            (shipit-actions-list--fetch-more-runs nil 10)
            ;; Runs list should be unchanged
            (should (= (length shipit-actions-list--all-runs) 1))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-workflows-sorted-alphabetically ()
  "GIVEN workflows in non-alphabetical order
   WHEN the buffer is rendered
   THEN workflows appear sorted alphabetically (case-insensitive)
        AND 'All workflows' remains at the top."
  (let ((buf (generate-new-buffer "*test-actions-list-sort*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 3) (name . "Zebra") (state . "active"))
                  ((id . 1) (name . "Alpha") (state . "active"))
                  ((id . 2) (name . "middle") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          (goto-char (point-min))
          ;; All workflows first
          (should (search-forward "All workflows" nil t))
          ;; Then alphabetical
          (should (search-forward "Alpha" nil t))
          (should (search-forward "middle" nil t))
          (should (search-forward "Zebra" nil t)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-fetch-workflows-paginates ()
  "GIVEN a repo with more workflows than one page (total_count=3, per_page=100)
   WHEN fetching workflows
   THEN page 1 returns 2 items, page 2 returns 1 item, both pages concatenated."
  (let ((call-count 0)
        (result nil))
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (_endpoint params callback)
                 (cl-incf call-count)
                 (let ((page (cadr (assoc "page" params))))
                   (if (= page 1)
                       ;; Simulate first page: 2 of 3 total
                       (funcall callback
                                `((total_count . 3)
                                  (workflows . [((id . 1) (name . "W1"))
                                                ((id . 2) (name . "W2"))])))
                     ;; Second page: remaining 1
                     (funcall callback
                              `((total_count . 3)
                                (workflows . [((id . 3) (name . "W3"))]))))))))
      (shipit-actions-list--fetch-workflows
       "owner/repo"
       (lambda (workflows) (setq result workflows)))
      (should (= call-count 2))
      (should (= (length result) 3))
      (should (equal (cdr (assq 'name (nth 0 result))) "W1"))
      (should (equal (cdr (assq 'name (nth 2 result))) "W3")))))

(ert-deftest test-actions-list-workflow-lazy-fetches-runs ()
  "GIVEN a workflow with 0 runs in the all-runs cache
   WHEN fetch-workflow-runs is called
   THEN runs are fetched via per-workflow API and passed to callback."
  (let ((buf (generate-new-buffer "*test-lazy-wf-runs*"))
        (fetched-endpoint nil)
        (fetched-params nil)
        (result-runs nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflow-runs (make-hash-table :test 'eql))
          ;; Mock API to return runs for workflow 42
          (cl-letf (((symbol-function 'shipit--api-request)
                     (lambda (endpoint params callback)
                       (setq fetched-endpoint endpoint
                             fetched-params params)
                       (funcall callback
                                `((workflow_runs . [((id . 100)
                                                     (workflow_id . 42)
                                                     (run_number . 1)
                                                     (status . "completed")
                                                     (conclusion . "success")
                                                     (head_branch . "main")
                                                     (created_at . "2026-01-01T00:00:00Z")
                                                     (updated_at . "2026-01-01T00:01:00Z"))]))))))
            (shipit-actions-list--fetch-workflow-runs
             42 (lambda (runs) (setq result-runs runs))))
          ;; Verify the per-workflow endpoint was called
          (should (string-match-p "workflows/42/runs" fetched-endpoint))
          (should-not (assoc "workflow_id" fetched-params))
          ;; Verify runs were passed to callback
          (should (= (length result-runs) 1))
          ;; Caller caches — verify puthash works in buffer context
          (puthash 42 result-runs shipit-actions-list--workflow-runs)
          (should (= (length (gethash 42 shipit-actions-list--workflow-runs)) 1)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-workflow-needs-fetch ()
  "GIVEN a workflow section with no runs and no cached data
   WHEN workflow-needs-fetch-p is checked
   THEN it returns non-nil."
  (let ((buf (generate-new-buffer "*test-needs-fetch*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 42) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (setq shipit-actions-list--workflow-runs (make-hash-table :test 'eql))
          (shipit-actions-list--render)
          ;; Find the Deploy workflow section
          (goto-char (point-min))
          (search-forward "Deploy")
          (let ((section (magit-current-section)))
            ;; Should need fetch (no runs anywhere)
            (should (shipit-actions-list--workflow-needs-fetch-p section))
            ;; After caching, should not need fetch
            (puthash 42 '(((id . 100))) shipit-actions-list--workflow-runs)
            (should-not (shipit-actions-list--workflow-needs-fetch-p section))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-workflow-uses-cached-runs ()
  "GIVEN a workflow with runs already in the per-workflow cache
   WHEN rendering the buffer
   THEN cached runs appear under the workflow."
  (let ((buf (generate-new-buffer "*test-cached-wf-runs*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 42) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          ;; Pre-populate per-workflow cache
          (puthash 42
                   '(((id . 100) (workflow_id . 42) (run_number . 5)
                      (status . "completed") (conclusion . "success")
                      (head_branch . "main")
                      (created_at . "2026-01-01T00:00:00Z")
                      (updated_at . "2026-01-01T00:01:00Z")))
                   shipit-actions-list--workflow-runs)
          (shipit-actions-list--render)
          (goto-char (point-min))
          ;; Workflow should show "1 runs" from cache
          (should (search-forward "Deploy" nil t))
          (should (search-forward "1 runs" nil t)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-unfetched-workflow-shows-zero-runs ()
  "GIVEN a workflow with no cached runs
   WHEN the buffer is rendered
   THEN the workflow shows 0 runs (lazy fetch on TAB)."
  (let ((buf (generate-new-buffer "*test-unfetched-runs*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 100) (name . "Alpha") (state . "active"))
                  ((id . 200) (name . "Beta") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 1) (workflow_id . 100) (run_number . 1)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main")
                   (created_at . "2026-03-09T10:00:00Z")
                   (updated_at . "2026-03-09T10:02:30Z"))))
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          ;; Without per-workflow cache, neither shows a run count
          (goto-char (point-min))
          (should (search-forward "Alpha" nil t))
          (should-not (search-forward "runs" (line-end-position) t))
          (should (search-forward "Beta" nil t))
          (should-not (search-forward "runs" (line-end-position) t)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-runs-for-workflow-only-uses-cache ()
  "GIVEN all-runs with matching workflow_id but no per-workflow cache
   WHEN runs-for-workflow is called
   THEN nil is returned (no fallback to all-runs filtering)."
  (let ((buf (generate-new-buffer "*test-no-fallback*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--all-runs
                '(((id . 1) (workflow_id . 5404121))
                  ((id . 2) (workflow_id . 5404121))))
          ;; Without cache, returns nil even though all-runs has matching data
          (should-not (shipit-actions-list--runs-for-workflow 5404121))
          ;; With cache, returns cached data
          (puthash 5404121
                   '(((id . 1) (workflow_id . 5404121)))
                   shipit-actions-list--workflow-runs)
          (should (= (length (shipit-actions-list--runs-for-workflow 5404121)) 1)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-fetch-more-appends-to-workflow-cache ()
  "GIVEN a workflow with cached runs
   WHEN fetch-more is called on that workflow
   THEN new runs are appended to the per-workflow cache."
  (let ((buf (generate-new-buffer "*test-fetch-more-wf*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 42) (name . "Deploy") (state . "active"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          ;; Pre-populate cache with 1 run
          (puthash 42
                   '(((id . 100) (workflow_id . 42) (run_number . 5)
                      (status . "completed") (conclusion . "success")
                      (head_branch . "main")
                      (created_at . "2026-01-01T00:00:00Z")
                      (updated_at . "2026-01-01T00:01:00Z")))
                   shipit-actions-list--workflow-runs)
          (shipit-actions-list--render)
          (goto-char (point-min))
          (search-forward "Deploy")
          (beginning-of-line)
          ;; Mock fetch-more
          (cl-letf (((symbol-function 'shipit--api-request)
                     (lambda (_endpoint _params callback)
                       (funcall callback
                                `((workflow_runs . [((id . 101)
                                                     (workflow_id . 42)
                                                     (run_number . 4)
                                                     (status . "completed")
                                                     (conclusion . "failure")
                                                     (head_branch . "dev")
                                                     (created_at . "2025-12-01T00:00:00Z")
                                                     (updated_at . "2025-12-01T00:01:00Z"))]))))))
            (shipit-actions-list-fetch-more 10))
          ;; Cache should now have 2 runs
          (should (= (length (gethash 42 shipit-actions-list--workflow-runs)) 2)))
      (kill-buffer buf))))

;;; Copy URL tests

(ert-deftest test-actions-list-copy-url-on-run ()
  "GIVEN point on a run section
   WHEN shipit-actions-list-copy-url is called
   THEN the run's html_url is copied to kill ring."
  (let ((buf (generate-new-buffer "*test-copy-url-run*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 5)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main")
                   (html_url . "https://github.com/owner/repo/actions/runs/100")
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))))
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          (goto-char (point-min))
          (search-forward "#5")
          (beginning-of-line)
          (shipit-actions-list-copy-url)
          (should (string= (current-kill 0)
                           "https://github.com/owner/repo/actions/runs/100")))
      (kill-buffer buf))))

(ert-deftest test-actions-list-copy-url-on-job ()
  "GIVEN point on a job section with cached jobs
   WHEN shipit-actions-list-copy-url is called
   THEN the job's html_url is copied to kill ring."
  (let ((buf (generate-new-buffer "*test-copy-url-job*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 5)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main")
                   (html_url . "https://github.com/owner/repo/actions/runs/100")
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))))
          (setq shipit-actions-list--runners nil)
          (puthash 100
                   '(((id . 200) (name . "build")
                      (status . "completed") (conclusion . "success")
                      (html_url . "https://github.com/owner/repo/actions/runs/100/job/200")))
                   shipit-actions-list--run-jobs)
          (shipit-actions-list--render)
          (goto-char (point-min))
          (search-forward "build")
          (beginning-of-line)
          (shipit-actions-list-copy-url)
          (should (string= (current-kill 0)
                           "https://github.com/owner/repo/actions/runs/100/job/200")))
      (kill-buffer buf))))

(ert-deftest test-actions-list-copy-url-on-step ()
  "GIVEN point on a step section
   WHEN shipit-actions-list-copy-url is called
   THEN a constructed step URL with anchor is copied to kill ring."
  (let ((buf (generate-new-buffer "*test-copy-url-step*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 5)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main")
                   (html_url . "https://github.com/owner/repo/actions/runs/100")
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))))
          (setq shipit-actions-list--runners nil)
          ;; Pre-populate jobs
          (puthash 100
                   '(((id . 200) (name . "build")
                      (status . "completed") (conclusion . "success")
                      (html_url . "https://github.com/owner/repo/actions/runs/100/job/200")
                      (steps . (((number . 1) (name . "Set up job")
                                  (status . "completed") (conclusion . "success")
                                  (started_at . "2026-01-01T00:00:00Z")
                                  (completed_at . "2026-01-01T00:00:05Z"))
                                 ((number . 2) (name . "Run tests")
                                  (status . "completed") (conclusion . "success")
                                  (started_at . "2026-01-01T00:00:05Z")
                                  (completed_at . "2026-01-01T00:00:10Z"))))))
                   shipit-actions-list--run-jobs)
          (shipit-actions-list--render)
          ;; Expand the run to show jobs
          (goto-char (point-min))
          (search-forward "#5")
          (beginning-of-line)
          (let ((section (magit-current-section)))
            ;; Manually show the run section
            (magit-section-show section))
          ;; Expand the job to show steps
          (search-forward "build")
          (beginning-of-line)
          (let ((section (magit-current-section)))
            (shipit-actions-list--render-steps-in-section section))
          ;; Navigate to step "Run tests" (step number 2)
          (search-forward "Run tests")
          (beginning-of-line)
          (shipit-actions-list-copy-url)
          (should (string= (current-kill 0)
                           "https://github.com/owner/repo/actions/runs/100/job/200#step:2:1")))
      (kill-buffer buf))))

(ert-deftest test-actions-list-copy-url-on-workflow ()
  "GIVEN point on a workflow section
   WHEN shipit-actions-list-copy-url is called
   THEN the workflow's GitHub Actions URL is copied to kill ring."
  (let ((buf (generate-new-buffer "*test-copy-url-workflow*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active")
                   (path . ".github/workflows/ci.yml"))))
          (setq shipit-actions-list--all-runs nil)
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          (goto-char (point-min))
          (search-forward "CI")
          (beginning-of-line)
          (shipit-actions-list-copy-url)
          (should (string= (current-kill 0)
                           "https://github.com/owner/repo/actions/workflows/ci.yml")))
      (kill-buffer buf))))

;;; Run duration bar tests

(ert-deftest test-actions-list-run-has-duration-bar ()
  "GIVEN runs with different durations
   WHEN the runs are rendered
   THEN each run line contains a duration bar character."
  (let ((buf (generate-new-buffer "*test-run-bar*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 5)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main") (head_commit . ((message . "fix")))
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:10:00Z"))
                  ((id . 101) (workflow_id . 1) (run_number . 6)
                   (status . "completed") (conclusion . "failure")
                   (head_branch . "dev") (head_commit . ((message . "test")))
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:05:00Z"))))
          (setq shipit-actions-list--runners nil)
          (shipit-actions-list--render)
          ;; Expand the workflow section
          (goto-char (point-min))
          (search-forward "CI")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            ;; Bar characters should be present (█ or ░)
            (should (string-match-p "█" text))
            (should (string-match-p "░" text))))
      (kill-buffer buf))))

;;; Run summary tests

(ert-deftest test-actions-list-summary-section-inserted-with-jobs ()
  "GIVEN a run section with jobs inserted
   WHEN the run is expanded
   THEN a collapsed Summary section appears after the jobs."
  (let ((buf (generate-new-buffer "*test-summary-insert*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 5)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main") (head_commit . ((message . "fix")))
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))))
          (setq shipit-actions-list--runners nil)
          ;; Pre-cache jobs so expand doesn't need API
          (puthash 100
                   '(((id . 200) (name . "build") (status . "completed")
                      (conclusion . "success")
                      (started_at . "2026-01-01T00:00:10Z")
                      (completed_at . "2026-01-01T00:01:00Z")))
                   shipit-actions-list--run-jobs)
          (shipit-actions-list--render)
          ;; Expand workflow, then run
          (goto-char (point-min))
          (search-forward "CI")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (search-forward "#5")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          ;; Summary section should be present
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Summary" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-summary-renders-cached-content ()
  "GIVEN cached annotation data for a run
   WHEN the summary section is rendered
   THEN content appears with job name heading."
  (let ((buf (generate-new-buffer "*test-summary-render*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          ;; Cache annotation content
          (puthash 100
                   '(("build" . "Warning: Node.js 20 actions are deprecated"))
                   shipit-actions-list--run-summaries)
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 5)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main") (head_commit . ((message . "fix")))
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))))
          (setq shipit-actions-list--runners nil)
          (puthash 100
                   '(((id . 200) (name . "build") (status . "completed")
                      (conclusion . "success")
                      (started_at . "2026-01-01T00:00:10Z")
                      (completed_at . "2026-01-01T00:01:00Z")))
                   shipit-actions-list--run-jobs)
          (shipit-actions-list--render)
          ;; Expand workflow → run → summary
          (goto-char (point-min))
          (search-forward "CI")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (search-forward "#5")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (search-forward "Summary")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "build" text))
            (should (string-match-p "Node.js 20" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-summary-no-annotations ()
  "GIVEN cached summary data that is empty
   WHEN the summary section is rendered
   THEN 'No annotations available' message appears."
  (let ((buf (generate-new-buffer "*test-summary-none*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          ;; Cache empty summary
          (puthash 100 nil shipit-actions-list--run-summaries)
          (setq shipit-actions-list--workflows
                '(((id . 1) (name . "CI") (state . "active"))))
          (setq shipit-actions-list--all-runs
                '(((id . 100) (workflow_id . 1) (run_number . 5)
                   (status . "completed") (conclusion . "success")
                   (head_branch . "main") (head_commit . ((message . "fix")))
                   (created_at . "2026-01-01T00:00:00Z")
                   (updated_at . "2026-01-01T00:01:00Z"))))
          (setq shipit-actions-list--runners nil)
          (puthash 100
                   '(((id . 200) (name . "build") (status . "completed")
                      (conclusion . "success")
                      (started_at . "2026-01-01T00:00:10Z")
                      (completed_at . "2026-01-01T00:01:00Z")))
                   shipit-actions-list--run-jobs)
          (shipit-actions-list--render)
          ;; Expand workflow → run → summary
          (goto-char (point-min))
          (search-forward "CI")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (search-forward "#5")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (search-forward "Summary")
          (beginning-of-line)
          (magit-section-show (magit-current-section))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "No annotations available" text))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-summary-fetches-annotations ()
  "GIVEN a run with jobs that have annotations
   WHEN the summary section is expanded
   THEN check-run and annotations APIs are called for each job."
  (let ((buf (generate-new-buffer "*test-summary-fetch*"))
        (api-calls nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (puthash 100
                   '(((id . 200) (name . "build") (status . "completed")
                      (conclusion . "success")
                      (started_at . "2026-01-01T00:00:10Z")
                      (completed_at . "2026-01-01T00:01:00Z"))
                     ((id . 201) (name . "test") (status . "completed")
                      (conclusion . "success")
                      (started_at . "2026-01-01T00:00:10Z")
                      (completed_at . "2026-01-01T00:02:00Z")))
                   shipit-actions-list--run-jobs)
          (cl-letf (((symbol-function 'shipit--api-request)
                     (lambda (endpoint _params callback)
                       (push endpoint api-calls)
                       (when callback
                         (if (string-match-p "annotations" endpoint)
                             (funcall callback
                                      '(((annotation_level . "warning")
                                         (title . "")
                                         (message . "Node.js 20 deprecated"))))
                           (funcall callback
                                    '((output . ((summary . :null)
                                                 (annotations_count . 1))))))))))
            (shipit-actions-list--fetch-run-summaries
             100
             (lambda (summaries)
               ;; Should have called check-run + annotations for both jobs
               (should (= (length api-calls) 4))
               ;; Should have 2 results with annotation content
               (should (= (length summaries) 2))
               (should (string-match-p "Node.js 20" (cdar summaries)))))))
      (kill-buffer buf))))

(ert-deftest test-actions-list-summary-skips-jobs-without-content ()
  "GIVEN jobs where some have no annotations or summary
   WHEN summaries are fetched
   THEN jobs without content are excluded."
  (let ((buf (generate-new-buffer "*test-summary-skip*"))
        (result-summaries nil))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          (puthash 100
                   '(((id . 200) (name . "build") (status . "completed")
                      (conclusion . "success"))
                     ((id . 201) (name . "test") (status . "completed")
                      (conclusion . "success")))
                   shipit-actions-list--run-jobs)
          (let ((call-count 0))
            (cl-letf (((symbol-function 'shipit--api-request)
                       (lambda (endpoint _params callback)
                         (cl-incf call-count)
                         (when callback
                           (if (string-match-p "annotations" endpoint)
                               (funcall callback
                                        '(((annotation_level . "warning")
                                           (title . "")
                                           (message . "Deprecation warning"))))
                             (funcall callback
                                      (if (= call-count 1)
                                          '((output . ((summary . :null)
                                                       (annotations_count . 1))))
                                        '((output . ((summary . :null)
                                                     (annotations_count . 0)))))))))))
              (shipit-actions-list--fetch-run-summaries
               100
               (lambda (summaries)
                 (setq result-summaries summaries)))))
          ;; Only the first job had annotations
          (should (= (length result-summaries) 1))
          (should (string= (caar result-summaries) "build")))
      (kill-buffer buf))))

(ert-deftest test-actions-list-summary-caches-results ()
  "GIVEN summaries are fetched for a run
   WHEN the fetch completes
   THEN results are cached in shipit-actions-list--run-summaries."
  (let ((buf (generate-new-buffer "*test-summary-cache*")))
    (unwind-protect
        (with-current-buffer buf
          (shipit-actions-list-mode)
          (setq shipit-actions-list--repo "owner/repo")
          ;; No cached summaries initially
          (should-not (gethash 100 shipit-actions-list--run-summaries))
          ;; Cache some summaries
          (puthash 100
                   '(("build" . "## Results"))
                   shipit-actions-list--run-summaries)
          ;; Should be cached now
          (should (gethash 100 shipit-actions-list--run-summaries))
          (should (= (length (gethash 100 shipit-actions-list--run-summaries)) 1)))
      (kill-buffer buf))))

(ert-deftest test-actions-list-build-job-content-annotations-only ()
  "GIVEN annotations without summary
   WHEN build-job-content is called
   THEN annotations are formatted with level prefix."
  (let ((result (shipit-actions-list--build-job-content
                 nil
                 '(((annotation_level . "warning")
                    (title . "")
                    (message . "Node.js 20 deprecated"))
                   ((annotation_level . "failure")
                    (title . "Build")
                    (message . "Compile error"))))))
    (should (stringp result))
    (should (string-match-p "Warning: Node.js 20" result))
    (should (string-match-p "Error: Build: Compile error" result))))

(ert-deftest test-actions-list-build-job-content-summary-and-annotations ()
  "GIVEN both summary and annotations
   WHEN build-job-content is called
   THEN both are included separated by blank lines."
  (let ((result (shipit-actions-list--build-job-content
                 "All tests passed"
                 '(((annotation_level . "notice")
                    (title . "")
                    (message . "Coverage: 95%"))))))
    (should (stringp result))
    (should (string-match-p "All tests passed" result))
    (should (string-match-p "Note: Coverage: 95%" result))))

(ert-deftest test-actions-list-build-job-content-empty ()
  "GIVEN no summary and no annotations
   WHEN build-job-content is called
   THEN nil is returned."
  (should-not (shipit-actions-list--build-job-content nil nil))
  (should-not (shipit-actions-list--build-job-content "" nil))
  (should-not (shipit-actions-list--build-job-content :null nil)))

;;; test-shipit-actions-list.el ends here
