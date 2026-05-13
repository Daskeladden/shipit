;;; test-shipit-actions-check-suite.el --- Tests for check-suite → actions dispatch -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'shipit-actions-list)

(defun test-shipit-actions--mock-api (responses)
  "Return a stub for `shipit--api-request' that serves RESPONSES.
RESPONSES is an alist mapping endpoint regex -> data."
  (lambda (endpoint _params callback)
    (let ((data (cdr (cl-find-if
                      (lambda (entry)
                        (string-match-p (car entry) endpoint))
                      responses))))
      (funcall callback data))))

(ert-deftest shipit-actions--run-id-from-check-run-url-extracts-id ()
  "GIVEN an Actions-style details URL
WHEN extracting the run-id
THEN the numeric run-id is returned."
  (should (equal 12345
                 (shipit-actions--run-id-from-check-run-url
                  "https://github.com/owner/repo/actions/runs/12345")))
  (should (equal 12345
                 (shipit-actions--run-id-from-check-run-url
                  "https://github.com/owner/repo/actions/runs/12345/job/678")))
  (should (equal nil
                 (shipit-actions--run-id-from-check-run-url
                  "https://thirdparty.tool/results/abc")))
  (should (equal nil
                 (shipit-actions--run-id-from-check-run-url nil))))

(ert-deftest shipit-open-actions-for-check-suite-with-runs ()
  "GIVEN the runs filter returns a workflow run
WHEN the helper is called
THEN it opens the actions list buffer for that repo + run-id."
  ;; GIVEN
  (let ((opened nil)
        (browsed nil))
    (cl-letf (((symbol-function 'shipit--api-request)
               (test-shipit-actions--mock-api
                '(("/actions/runs"
                   . ((workflow_runs . [((id . 9876543210)
                                         (name . "FPGA Diamond"))]))))))
              ((symbol-function 'shipit-open-actions-list)
               (lambda (repo run-id)
                 (setq opened (list repo run-id))))
              ((symbol-function 'browse-url)
               (lambda (url) (setq browsed url))))
      ;; WHEN
      (shipit-open-actions-for-check-suite "acme/acme-project" 42
                                           "https://example.com/fallback")
      ;; THEN
      (should (equal opened '("acme/acme-project" 9876543210)))
      (should (null browsed)))))

(ert-deftest shipit-open-actions-for-check-suite-falls-back-to-check-runs ()
  "GIVEN the runs filter is empty but check-runs has an Actions detail URL
WHEN the helper is called
THEN it extracts the run-id from the check-run's details_url
     AND opens the actions list buffer for that run."
  ;; GIVEN
  (let ((opened nil)
        (browsed nil))
    (cl-letf (((symbol-function 'shipit--api-request)
               (test-shipit-actions--mock-api
                '(("/actions/runs"
                   . ((workflow_runs . [])))
                  ("/check-suites/.*/check-runs"
                   . ((check_runs
                       . [((name . "build")
                           (details_url . "https://github.com/acme/acme-project/actions/runs/55555/job/777"))]))))))
              ((symbol-function 'shipit-open-actions-list)
               (lambda (repo run-id)
                 (setq opened (list repo run-id))))
              ((symbol-function 'browse-url)
               (lambda (url) (setq browsed url))))
      ;; WHEN
      (shipit-open-actions-for-check-suite "acme/acme-project" 23859720360
                                           "https://example.com/fallback")
      ;; THEN
      (should (equal opened '("acme/acme-project" 55555)))
      (should (null browsed)))))

(ert-deftest shipit-open-actions-for-check-suite-non-actions-falls-back-to-browser ()
  "GIVEN both runs filter and check-runs return no Actions URLs
WHEN the helper is called with a fallback URL
THEN it opens the fallback URL in the browser."
  ;; GIVEN
  (let ((opened nil)
        (browsed nil))
    (cl-letf (((symbol-function 'shipit--api-request)
               (test-shipit-actions--mock-api
                '(("/actions/runs"
                   . ((workflow_runs . [])))
                  ("/check-suites/.*/check-runs"
                   . ((check_runs
                       . [((name . "third-party")
                           (details_url . "https://thirdparty.tool/build/42"))]))))))
              ((symbol-function 'shipit-open-actions-list)
               (lambda (&rest args) (setq opened args)))
              ((symbol-function 'browse-url)
               (lambda (url) (setq browsed url))))
      ;; WHEN
      (shipit-open-actions-for-check-suite "owner/repo" 7
                                           "https://example.com/fallback")
      ;; THEN
      (should (null opened))
      (should (equal browsed "https://example.com/fallback")))))

(ert-deftest shipit-open-actions-for-check-suite-uses-correct-endpoint ()
  "GIVEN a repo and check-suite-id
WHEN the helper is called
THEN it requests `/repos/REPO/actions/runs' with `check_suite_id' param."
  ;; GIVEN
  (let ((captured-endpoint nil)
        (captured-params nil))
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (endpoint params _callback)
                 (setq captured-endpoint endpoint
                       captured-params params))))
      ;; WHEN
      (shipit-open-actions-for-check-suite "acme/acme-project" 12345 nil)
      ;; THEN
      (should (equal captured-endpoint "/repos/acme/acme-project/actions/runs"))
      (should (equal captured-params '(("check_suite_id" 12345)))))))

(ert-deftest shipit-actions--api-endpoint-from-url-strips-prefix ()
  "GIVEN a GitHub API URL
WHEN converting to a relative endpoint
THEN the `https://api.github.com' prefix is stripped."
  (should (equal "/repos/owner/repo/check-suites/42"
                 (shipit-actions--api-endpoint-from-url
                  "https://api.github.com/repos/owner/repo/check-suites/42")))
  (should (equal nil
                 (shipit-actions--api-endpoint-from-url
                  "https://github.com/owner/repo")))
  (should (equal nil (shipit-actions--api-endpoint-from-url nil))))

(ert-deftest shipit-open-actions-for-check-suite-subject-follows-url ()
  "GIVEN a notification's `subject.url' for a check-suite
WHEN the subject helper is called
THEN it fetches the resource at SUBJECT-URL
     AND extracts the real check-suite `id' from the response
     AND uses that to find and open the workflow run."
  ;; GIVEN
  (let ((opened nil)
        (browsed nil)
        (captured-endpoints '()))
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (endpoint _params callback)
                 (push endpoint captured-endpoints)
                 (funcall callback
                          (cond
                           ((string-match-p "check-suites/.*$" endpoint)
                            ;; Real check-suite resource: response carries
                            ;; the real id, which differs from the URL
                            ;; segment when GitHub returns a moved resource.
                            '((id . 99999999)
                              (head_sha . "deadbeef")))
                           ((string-match-p "actions/runs" endpoint)
                            '((workflow_runs
                               . [((id . 25794789483)
                                   (name . "FPGA Diamond"))])))))))
              ((symbol-function 'shipit-open-actions-list)
               (lambda (repo run-id)
                 (setq opened (list repo run-id))))
              ((symbol-function 'browse-url)
               (lambda (url) (setq browsed url))))
      ;; WHEN
      (shipit-open-actions-for-check-suite-subject
       "acme/acme-project"
       "https://api.github.com/repos/acme/acme-project/check-suites/23859720360"
       "https://github.com/acme/acme-project/actions")
      ;; THEN
      ;; First call hit the subject URL endpoint
      (should (cl-find-if
               (lambda (e) (string-match-p "/check-suites/23859720360" e))
               captured-endpoints))
      ;; Second call used the real id from the response
      (should (cl-find-if
               (lambda (e) (string-match-p "/actions/runs" e))
               captured-endpoints))
      (should (equal opened '("acme/acme-project" 25794789483)))
      (should (null browsed)))))

(ert-deftest shipit-open-actions-for-check-suite-subject-missing-url-browses ()
  "GIVEN a check-suite notification with no `subject.url'
WHEN the helper is called with a fallback URL
THEN it opens the fallback URL in the browser."
  ;; GIVEN
  (let ((opened nil)
        (browsed nil))
    (cl-letf (((symbol-function 'shipit--api-request)
               (lambda (&rest _) (error "should not be called")))
              ((symbol-function 'shipit-open-actions-list)
               (lambda (&rest args) (setq opened args)))
              ((symbol-function 'browse-url)
               (lambda (url) (setq browsed url))))
      ;; WHEN
      (shipit-open-actions-for-check-suite-subject
       "owner/repo" nil "https://example.com/fallback")
      ;; THEN
      (should (null opened))
      (should (equal browsed "https://example.com/fallback")))))

(ert-deftest shipit-actions--parse-workflow-failure-title-extracts-fields ()
  "GIVEN a CheckSuite notification title in the canonical format
WHEN parsing the title
THEN the plist contains workflow name, status, and branch."
  (let ((parsed (shipit-actions--parse-workflow-failure-title
                 "FPGA Diamond workflow run failed for MISC-2026-05-13-livestream-start-stop-hardening branch")))
    (should (equal "FPGA Diamond" (plist-get parsed :name)))
    (should (equal "failed" (plist-get parsed :status)))
    (should (equal "MISC-2026-05-13-livestream-start-stop-hardening"
                   (plist-get parsed :branch))))
  ;; Status variants
  (should (equal "succeeded"
                 (plist-get
                  (shipit-actions--parse-workflow-failure-title
                   "Build workflow run succeeded for main branch")
                  :status)))
  ;; Title that doesn't match the pattern
  (should (null (shipit-actions--parse-workflow-failure-title
                 "Some other check"))))

(provide 'test-shipit-actions-check-suite)
;;; test-shipit-actions-check-suite.el ends here
