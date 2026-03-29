;;; test-shipit-open-url.el --- Tests for shipit-open-url command -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the shipit-open-url interactive command.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)

;;; Test helpers

(defvar test-open-url--opened nil
  "Plist passed to `shipit--open-classified-url' during tests.")

(defvar test-open-url--messages nil
  "Messages produced during tests.")

(defvar test-open-url--browsed nil
  "URL passed to `browse-url' during tests.")

;;; kill-ring default detection

(ert-deftest test-open-url-default-from-kill-ring-url ()
  "GIVEN kill-ring head is a URL
WHEN shipit-open-url prompts the user
THEN the default value is the kill-ring URL."
  (require 'shipit-commands)
  (let ((kill-ring '("https://github.com/owner/repo/pull/42"))
        (prompted-default nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional _initial _history default)
                 (setq prompted-default default)
                 default))
              ((symbol-function 'shipit--classify-url)
               (lambda (_url) '(:type pr :repo "owner/repo" :number 42)))
              ((symbol-function 'shipit--open-classified-url)
               #'ignore))
      (shipit-open-url)
      (should (equal prompted-default "https://github.com/owner/repo/pull/42")))))

(ert-deftest test-open-url-no-default-when-kill-ring-not-url ()
  "GIVEN kill-ring head is not a URL
WHEN shipit-open-url prompts the user
THEN no default value is offered."
  (require 'shipit-commands)
  (let ((kill-ring '("not a url"))
        (prompted-default :unset))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional _initial _history default)
                 (setq prompted-default default)
                 "https://github.com/owner/repo/pull/1"))
              ((symbol-function 'shipit--classify-url)
               (lambda (_url) '(:type pr :repo "owner/repo" :number 1)))
              ((symbol-function 'shipit--open-classified-url)
               #'ignore))
      (shipit-open-url)
      (should (null prompted-default)))))

;;; Classified URL dispatch

(ert-deftest test-open-url-dispatches-classified-url ()
  "GIVEN user enters a recognized forge URL
WHEN shipit-open-url processes it
THEN it calls shipit--open-classified-url with the classified plist."
  (require 'shipit-commands)
  (let ((test-open-url--opened nil)
        (kill-ring nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _)
                 "https://github.com/owner/repo/pull/99"))
              ((symbol-function 'shipit--classify-url)
               (lambda (_url) '(:type pr :repo "owner/repo" :number 99)))
              ((symbol-function 'shipit--open-classified-url)
               (lambda (classified) (setq test-open-url--opened classified))))
      (shipit-open-url)
      (should test-open-url--opened)
      (should (eq (plist-get test-open-url--opened :type) 'pr))
      (should (equal (plist-get test-open-url--opened :repo) "owner/repo"))
      (should (= (plist-get test-open-url--opened :number) 99)))))

;;; Unrecognized URL handling

(ert-deftest test-open-url-unrecognized-offers-browser-fallback ()
  "GIVEN user enters an unrecognized URL
WHEN shipit-open-url processes it and user accepts browser fallback
THEN browse-url is called with the URL."
  (require 'shipit-commands)
  (let ((test-open-url--browsed nil)
        (kill-ring nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _)
                 "https://example.com/unknown"))
              ((symbol-function 'shipit--classify-url)
               (lambda (_url) nil))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'browse-url)
               (lambda (url) (setq test-open-url--browsed url))))
      (shipit-open-url)
      (should (equal test-open-url--browsed "https://example.com/unknown")))))

(ert-deftest test-open-url-unrecognized-declined-browser ()
  "GIVEN user enters an unrecognized URL
WHEN shipit-open-url processes it and user declines browser fallback
THEN browse-url is NOT called."
  (require 'shipit-commands)
  (let ((test-open-url--browsed nil)
        (kill-ring nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _)
                 "https://example.com/unknown"))
              ((symbol-function 'shipit--classify-url)
               (lambda (_url) nil))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'browse-url)
               (lambda (url) (setq test-open-url--browsed url))))
      (shipit-open-url)
      (should (null test-open-url--browsed)))))

;;; Backend-aware classify dispatch

(ert-deftest test-classify-url-dispatcher-includes-backend-id ()
  "GIVEN a GitHub PR URL
WHEN classified via shipit-pr--classify-url
THEN the result includes :backend-id 'github."
  (let ((result (shipit-pr--classify-url "https://github.com/owner/repo/pull/7")))
    (should result)
    (should (eq (plist-get result :backend-id) 'github))))

(ert-deftest test-classify-url-dispatcher-gitlab-includes-backend-id ()
  "GIVEN a GitLab MR URL
WHEN classified via shipit-pr--classify-url
THEN the result includes :backend-id 'gitlab."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr--classify-url "https://gitlab.com/group/project/-/merge_requests/42")))
    (should result)
    (should (eq (plist-get result :backend-id) 'gitlab))))

(ert-deftest test-open-classified-url-uses-backend-id ()
  "GIVEN a classified URL with :backend-id
WHEN opened via shipit--open-classified-url
THEN shipit-open-pr-buffer receives the backend-id."
  (require 'shipit-render)
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'shipit-open-pr-buffer)
               (lambda (number &optional repo backend-id backend-config)
                 (setq captured-args (list number repo backend-id backend-config)))))
      (shipit--open-classified-url '(:type pr :repo "group/proj" :number 5 :backend-id gitlab))
      (should captured-args)
      (should (= (nth 0 captured-args) 5))
      (should (equal (nth 1 captured-args) "group/proj"))
      (should (eq (nth 2 captured-args) 'gitlab)))))

;;; GitLab URL variants with trailing paths/query params

(ert-deftest test-classify-url-gitlab-mr-with-diffs-and-commit-id ()
  "GIVEN a GitLab MR URL with /diffs?commit_id=...
WHEN classified via shipit-pr--classify-url
THEN it is recognized as a PR with correct repo and number."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr--classify-url
                 "https://gitlab.com/genomedx/labautomation/steps/-/merge_requests/785/diffs?commit_id=280c31aec564c144a8e1b359f7df083aa0e1f12c")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (equal (plist-get result :repo) "genomedx/labautomation/steps"))
    (should (= (plist-get result :number) 785))
    (should (eq (plist-get result :backend-id) 'gitlab))))

(ert-deftest test-classify-url-gitlab-mr-with-diffs-path ()
  "GIVEN a GitLab MR URL with /diffs trailing path
WHEN classified via shipit-pr--classify-url
THEN it is recognized as a PR."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr--classify-url
                 "https://gitlab.com/group/project/-/merge_requests/42/diffs")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (= (plist-get result :number) 42))))

(ert-deftest test-classify-url-gitlab-mr-with-commits-path ()
  "GIVEN a GitLab MR URL with /commits trailing path
WHEN classified via shipit-pr--classify-url
THEN it is recognized as a PR."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr--classify-url
                 "https://gitlab.com/group/project/-/merge_requests/42/commits")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (= (plist-get result :number) 42))))

(ert-deftest test-classify-url-gitlab-issue-with-fragment ()
  "GIVEN a GitLab issue URL with #note_123 fragment
WHEN classified via shipit-pr--classify-url
THEN it is recognized as an issue."
  (require 'shipit-pr-gitlab)
  (let ((result (shipit-pr--classify-url
                 "https://gitlab.com/group/project/-/issues/10#note_123456")))
    (should result)
    (should (eq (plist-get result :type) 'issue))
    (should (= (plist-get result :number) 10))))

;;; Jira/Atlassian URL classification

(ert-deftest test-classify-url-jira-browse-url ()
  "GIVEN a Jira browse URL like https://foo.atlassian.net/browse/PROJ-123
WHEN classified via shipit-issue--classify-url
THEN returns (:type issue :number \"PROJ-123\" :backend-id jira)."
  (require 'shipit-issue-jira)
  (let ((shipit-issue-repo-backends
         '(("myorg/repo" :backend jira
            :base-url "https://foo.atlassian.net"
            :project-keys ("PROJ")))))
    (let ((result (shipit-issue--classify-url
                   "https://foo.atlassian.net/browse/PROJ-123")))
      (should result)
      (should (eq (plist-get result :type) 'issue))
      (should (equal (plist-get result :number) "PROJ-123"))
      (should (eq (plist-get result :backend-id) 'jira)))))

(ert-deftest test-classify-url-jira-returns-repo ()
  "GIVEN a Jira browse URL matching a repo config
WHEN classified via shipit-issue--classify-url
THEN the result includes :repo from the matching config entry."
  (require 'shipit-issue-jira)
  (let ((shipit-issue-repo-backends
         '(("myorg/repo" :backend jira
            :base-url "https://foo.atlassian.net"
            :project-keys ("PROJ")))))
    (let ((result (shipit-issue--classify-url
                   "https://foo.atlassian.net/browse/PROJ-456")))
      (should result)
      (should (equal (plist-get result :repo) "myorg/repo")))))

(ert-deftest test-classify-url-jira-returns-backend-config ()
  "GIVEN a Jira browse URL matching a repo config
WHEN classified via shipit-issue--classify-url
THEN the result includes :backend-config with the config plist."
  (require 'shipit-issue-jira)
  (let ((shipit-issue-repo-backends
         '(("myorg/repo" :backend jira
            :base-url "https://foo.atlassian.net"
            :project-keys ("PROJ")))))
    (let ((result (shipit-issue--classify-url
                   "https://foo.atlassian.net/browse/PROJ-789")))
      (should result)
      (let ((config (plist-get result :backend-config)))
        (should config)
        (should (equal (plist-get config :base-url) "https://foo.atlassian.net"))))))

(ert-deftest test-classify-url-jira-no-match-different-host ()
  "GIVEN a Jira URL whose host doesn't match any configured base-url
WHEN classified via shipit-issue--classify-url
THEN returns nil."
  (require 'shipit-issue-jira)
  (let ((shipit-issue-repo-backends
         '(("myorg/repo" :backend jira
            :base-url "https://foo.atlassian.net"
            :project-keys ("PROJ")))))
    (should-not (shipit-issue--classify-url
                 "https://bar.atlassian.net/browse/PROJ-123"))))

(ert-deftest test-classify-url-jira-with-trailing-slash ()
  "GIVEN a Jira browse URL with trailing slash
WHEN classified via shipit-issue--classify-url
THEN it is still recognized."
  (require 'shipit-issue-jira)
  (let ((shipit-issue-repo-backends
         '(("myorg/repo" :backend jira
            :base-url "https://foo.atlassian.net"
            :project-keys ("PROJ")))))
    (let ((result (shipit-issue--classify-url
                   "https://foo.atlassian.net/browse/PROJ-123/")))
      (should result)
      (should (equal (plist-get result :number) "PROJ-123")))))

(ert-deftest test-classify-url-jira-base-url-with-trailing-slash ()
  "GIVEN a Jira config whose base-url has a trailing slash
WHEN a browse URL is classified
THEN it still matches."
  (require 'shipit-issue-jira)
  (let ((shipit-issue-repo-backends
         '(("myorg/repo" :backend jira
            :base-url "https://foo.atlassian.net/"
            :project-keys ("PROJ")))))
    (let ((result (shipit-issue--classify-url
                   "https://foo.atlassian.net/browse/PROJ-42")))
      (should result)
      (should (equal (plist-get result :number) "PROJ-42")))))

(ert-deftest test-classify-url-top-level-tries-issue-backends ()
  "GIVEN a Jira URL that no PR backend recognizes
WHEN classified via shipit--classify-url (top-level)
THEN falls through to issue backend and returns classified result."
  (require 'shipit-render)
  (require 'shipit-issue-jira)
  (let ((shipit-issue-repo-backends
         '(("myorg/repo" :backend jira
            :base-url "https://test.atlassian.net"
            :project-keys ("PROJ")))))
    (let ((result (shipit--classify-url
                   "https://test.atlassian.net/browse/PROJ-11492")))
      (should result)
      (should (eq (plist-get result :type) 'issue))
      (should (equal (plist-get result :number) "PROJ-11492"))
      (should (eq (plist-get result :backend-id) 'jira)))))

(ert-deftest test-classify-url-github-pr-still-works ()
  "GIVEN a GitHub PR URL
WHEN classified via shipit--classify-url (top-level)
THEN PR backends match first, issue backends are not tried."
  (require 'shipit-render)
  (let ((result (shipit--classify-url
                 "https://github.com/owner/repo/pull/42")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (eq (plist-get result :backend-id) 'github))))

(ert-deftest test-open-classified-url-jira-passes-backend-info ()
  "GIVEN a classified Jira issue URL with :backend-id and :backend-config
WHEN opened via shipit--open-classified-url
THEN shipit-issues-open-buffer receives backend-id and backend-config."
  (require 'shipit-render)
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'shipit-issues-open-buffer)
               (lambda (number &optional repo backend-id backend-config)
                 (setq captured-args (list number repo backend-id backend-config)))))
      (shipit--open-classified-url
       '(:type issue :number "PROJ-123" :repo "myorg/repo"
         :backend-id jira :backend-config (:base-url "https://x.atlassian.net")))
      (should captured-args)
      (should (equal (nth 0 captured-args) "PROJ-123"))
      (should (equal (nth 1 captured-args) "myorg/repo"))
      (should (eq (nth 2 captured-args) 'jira))
      (should (equal (plist-get (nth 3 captured-args) :base-url)
                     "https://x.atlassian.net")))))

;;; GitHub Actions URL classification

(ert-deftest test-classify-url-github-actions-run ()
  "GIVEN a GitHub Actions run URL
WHEN classified via shipit-pr--classify-url
THEN returns (:type actions-run :repo REPO :run-id RUN-ID)."
  (let ((result (shipit-pr--classify-url
                 "https://github.com/acme/acme-project/actions/runs/22821333060")))
    (should result)
    (should (eq (plist-get result :type) 'actions-run))
    (should (equal (plist-get result :repo) "acme/acme-project"))
    (should (equal (plist-get result :run-id) "22821333060"))
    (should-not (plist-get result :job-id))
    (should (eq (plist-get result :backend-id) 'github))))

(ert-deftest test-classify-url-github-actions-run-with-job ()
  "GIVEN a GitHub Actions run URL with /job/JOB_ID
WHEN classified via shipit-pr--classify-url
THEN returns (:type actions-run) with both :run-id and :job-id."
  (let ((result (shipit-pr--classify-url
                 "https://github.com/acme/acme-project/actions/runs/22821333060/job/66194471201")))
    (should result)
    (should (eq (plist-get result :type) 'actions-run))
    (should (equal (plist-get result :repo) "acme/acme-project"))
    (should (equal (plist-get result :run-id) "22821333060"))
    (should (equal (plist-get result :job-id) "66194471201"))
    (should (eq (plist-get result :backend-id) 'github))))

(ert-deftest test-classify-url-github-actions-run-trailing-slash ()
  "GIVEN a GitHub Actions run URL with trailing slash
WHEN classified via shipit-pr--classify-url
THEN it is still recognized."
  (let ((result (shipit-pr--classify-url
                 "https://github.com/owner/repo/actions/runs/123/")))
    (should result)
    (should (eq (plist-get result :type) 'actions-run))
    (should (equal (plist-get result :run-id) "123"))))

(ert-deftest test-classify-url-github-pr-with-trailing-path ()
  "GIVEN a GitHub PR URL with /files trailing path
WHEN classified via shipit-pr--classify-url
THEN it is still recognized as a PR."
  (let ((result (shipit-pr--classify-url
                 "https://github.com/owner/repo/pull/42/files")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (= (plist-get result :number) 42))))

(ert-deftest test-classify-url-github-pr-with-checks-path ()
  "GIVEN a GitHub PR URL with /checks trailing path
WHEN classified via shipit-pr--classify-url
THEN it is still recognized as a PR."
  (let ((result (shipit-pr--classify-url
                 "https://github.com/owner/repo/pull/42/checks")))
    (should result)
    (should (eq (plist-get result :type) 'pr))
    (should (= (plist-get result :number) 42))))

(ert-deftest test-classify-url-github-issue-with-fragment ()
  "GIVEN a GitHub issue URL with #issuecomment fragment
WHEN classified via shipit-pr--classify-url
THEN it is still recognized as an issue."
  (let ((result (shipit-pr--classify-url
                 "https://github.com/owner/repo/issues/42#issuecomment-123456")))
    (should result)
    (should (eq (plist-get result :type) 'issue))
    (should (= (plist-get result :number) 42))))

;;; open-classified-url dispatch for actions-run

(ert-deftest test-open-classified-url-actions-run ()
  "GIVEN a classified actions-run URL
WHEN opened via shipit--open-classified-url
THEN shipit-open-actions-run is called with repo, run-id, and job-id."
  (require 'shipit-render)
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'shipit-open-actions-run)
               (lambda (repo run-id &optional job-id)
                 (setq captured-args (list repo run-id job-id)))))
      (shipit--open-classified-url
       '(:type actions-run :repo "acme/acme-project"
         :run-id "22821333060" :job-id "66194471201"
         :backend-id github))
      (should captured-args)
      (should (equal (nth 0 captured-args) "acme/acme-project"))
      (should (equal (nth 1 captured-args) "22821333060"))
      (should (equal (nth 2 captured-args) "66194471201")))))

;;; Empty input handling

(ert-deftest test-open-url-empty-input-does-nothing ()
  "GIVEN user enters empty string
WHEN shipit-open-url processes it
THEN no classification or browsing occurs."
  (require 'shipit-commands)
  (let ((classified-called nil)
        (kill-ring nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) ""))
              ((symbol-function 'shipit--classify-url)
               (lambda (_url) (setq classified-called t) nil)))
      (shipit-open-url)
      (should (null classified-called)))))

(provide 'test-shipit-open-url)
;;; test-shipit-open-url.el ends here
