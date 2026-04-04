;;; shipit-pr-github.el --- GitHub PR backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; GitHub PR backend for the pluggable PR system.
;; Wraps existing shipit API functions into the backend interface.
;; Pure API wrappers — no caching, no UI side effects.

;;; Code:

(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-http)
(require 'shipit-gh-etag)

;; Forward declarations for functions in other modules
(declare-function shipit--search-prs-with-encoded-query "shipit-commands")
(declare-function shipit--build-advanced-search-query "shipit-commands")
(declare-function shipit--extract-limit-from-args "shipit-commands")
(declare-function shipit--extract-sort-from-args "shipit-commands")

;;; Required operations

(defun shipit-pr-github--fetch-pr (config number)
  "Fetch PR NUMBER using CONFIG.
CONFIG must contain :repo.  Uses ETag caching for performance."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s" repo number)))
    (shipit--debug-log "GitHub PR backend: fetching PR #%s from %s" number repo)
    (let* ((result (shipit-gh-etag-get-json-with-refresh-cache endpoint nil shipit-github-token))
           (json-data (plist-get result :json)))
      json-data)))

(defun shipit-pr-github--search (config args)
  "Search PRs using CONFIG with transient ARGS."
  (let* ((repo (plist-get config :repo))
         (query-parts (shipit--build-advanced-search-query args repo))
         (limit (shipit--extract-limit-from-args args))
         (sort-by (shipit--extract-sort-from-args args))
         (per-page (min 50 limit)))
    (shipit--search-prs-with-encoded-query repo query-parts per-page limit sort-by)))

(defun shipit-pr-github--create-pr (config title body base head)
  "Create a new PR in REPO specified by CONFIG.
TITLE, BODY, BASE branch, HEAD branch."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls" repo))
         (data `((title . ,title) (body . ,body)
                 (base . ,base) (head . ,head))))
    (shipit--debug-log "GitHub PR backend: creating PR in %s" repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--merge-pr (config number method)
  "Merge PR NUMBER using CONFIG with merge METHOD."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/merge" repo number))
         (data `((merge_method . ,method))))
    (shipit--debug-log "GitHub PR backend: merging PR #%s in %s via %s" number repo method)
    (shipit--api-request-post endpoint data "PUT")))

(defvar shipit--merge-methods-cache (make-hash-table :test 'equal)
  "Session cache of allowed merge methods per repo.")

(defun shipit--merge-methods-cache-clear (repo)
  "Clear cached merge methods for REPO."
  (remhash repo shipit--merge-methods-cache))

(defun shipit-pr-github--fetch-merge-methods (config)
  "Fetch allowed merge methods for repo in CONFIG.
Returns a list of strings: \"merge\", \"squash\", \"rebase\".
Results are cached per-repo for the session."
  (let* ((repo (plist-get config :repo))
         (cached (gethash repo shipit--merge-methods-cache)))
    (or cached
        (let* ((endpoint (format "/repos/%s" repo))
               (data (shipit--api-request endpoint))
               (methods nil))
          (when (not (eq (cdr (assq 'allow_rebase_merge data)) :json-false))
            (push "rebase" methods))
          (when (not (eq (cdr (assq 'allow_squash_merge data)) :json-false))
            (push "squash" methods))
          (when (not (eq (cdr (assq 'allow_merge_commit data)) :json-false))
            (push "merge" methods))
          (puthash repo methods shipit--merge-methods-cache)
          methods))))

(defun shipit-pr-github--update-pr (config number data)
  "Update PR NUMBER using CONFIG with DATA alist."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s" repo number)))
    (shipit--debug-log "GitHub PR backend: updating PR #%s in %s" number repo)
    (shipit--api-request-post endpoint data "PATCH")))

(defun shipit-pr-github--fetch-reviews (config number)
  "Fetch reviews for PR NUMBER using CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching reviews for PR #%s from %s" number repo)
    (shipit--get-pr-reviews-for-comments repo number)))

(defun shipit-pr-github--submit-review (config number event body comments)
  "Submit a review for PR NUMBER using CONFIG.
EVENT is the review action, BODY is the review body,
COMMENTS is a list of inline comment alists."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/reviews" repo number))
         (data `((event . ,event) (body . ,(or body "")))))
    (when comments
      (push `(comments . ,comments) data))
    (shipit--debug-log "GitHub PR backend: submitting review for PR #%s in %s" number repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--fetch-review-decision (config number)
  "Fetch review decision for PR NUMBER using CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching review decision for PR #%s" number)
    (shipit--get-pr-review-decision repo number)))

(defun shipit-pr-github--fetch-files (config number)
  "Fetch changed files for PR NUMBER using CONFIG.
Uses ETag caching with fallback to regular paginated request."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/files" repo number)))
    (shipit--debug-log "GitHub PR backend: fetching files for PR #%s from %s" number repo)
    (condition-case err
        (let* ((result (shipit-gh-etag-get-json-with-refresh-cache endpoint nil shipit-github-token))
               (json-data (plist-get result :json)))
          json-data)
      (error
       (shipit--debug-log "ETag request failed for %s: %s, falling back to paginated request"
                          endpoint (error-message-string err))
       (shipit--api-request-paginated endpoint)))))

(defun shipit-pr-github--fetch-commits (config number)
  "Fetch commits for PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/commits" repo number)))
    (shipit--debug-log "GitHub PR backend: fetching commits for PR #%s from %s" number repo)
    (shipit--api-request-paginated endpoint)))

(defun shipit-pr-github--fetch-checks (config ref)
  "Fetch CI checks for commit REF using CONFIG.
GitHub check runs already match the common shipit check shape natively.
The `workflow-name' and `workflow-run-name' grouping fields are added
by the enrichment pipeline in `shipit-checks.el'."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching checks for %s in %s" ref repo)
    (let* ((check-endpoint (format "/repos/%s/commits/%s/check-runs" repo ref))
           (status-endpoint (format "/repos/%s/commits/%s/statuses" repo ref))
           (check-result (condition-case nil
                             (shipit-gh-etag-get-json-with-refresh-cache
                              check-endpoint nil shipit-github-token)
                           (error nil)))
           (check-runs (when check-result
                         (cdr (assq 'check_runs (plist-get check-result :json)))))
           (statuses (condition-case nil
                         (shipit--api-request-paginated status-endpoint)
                       (error nil))))
      (append (append check-runs nil) (append statuses nil)))))

(defun shipit-pr-github--browse-url (config number)
  "Return browser URL for PR NUMBER using CONFIG."
  (format "https://github.com/%s/pull/%s" (plist-get config :repo) number))

;;; Optional operations

(defun shipit-pr-github--fetch-review-threads (config number)
  "Fetch review threads for PR NUMBER using CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching review threads for PR #%s" number)
    (shipit--fetch-review-threads repo number)))

(defun shipit-pr-github--fetch-resolved-threads (config number)
  "Fetch resolved threads for PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (parts (split-string repo "/"))
         (owner (car parts))
         (name (cadr parts)))
    (shipit--debug-log "GitHub PR backend: fetching resolved threads for PR #%s" number)
    (shipit--fetch-resolved-threads-graphql owner name number)))

(defun shipit-pr-github--fetch-timeline (config number)
  "Fetch timeline events for PR NUMBER using CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching timeline for PR #%s" number)
    (shipit--fetch-timeline-events repo number)))

;;; Async backend wrappers
;; These adapt the existing async functions to the (config number callback)
;; backend dispatch signature, allowing the buffer to prefer async when available.

(defun shipit-pr-github--fetch-review-decision-async (config number callback)
  "Fetch review decision for PR NUMBER using CONFIG asynchronously.
Calls CALLBACK with the review-info alist."
  (let ((repo (plist-get config :repo)))
    (shipit--fetch-review-decision-async repo number callback)))

(defun shipit-pr-github--fetch-timeline-async (config number callback)
  "Fetch timeline events for PR NUMBER using CONFIG asynchronously.
Calls CALLBACK with the list of events."
  (let ((repo (plist-get config :repo)))
    (shipit--fetch-timeline-events-async repo number callback)))

(defun shipit-pr-github--fetch-commits-async (config number callback)
  "Fetch commits for PR NUMBER using CONFIG asynchronously.
Calls CALLBACK with the list of commits."
  (let ((repo (plist-get config :repo)))
    (shipit--fetch-commits-async repo number callback)))

(defun shipit-pr-github--fetch-files-async (config number callback)
  "Fetch files for PR NUMBER using CONFIG asynchronously.
Calls CALLBACK with (files . truncated-p)."
  (let ((repo (plist-get config :repo)))
    (shipit--fetch-files-async repo number callback)))

(defun shipit-pr-github--fetch-branch-protection (config base-ref)
  "Fetch branch protection rules for BASE-REF using CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching branch protection for %s" base-ref)
    (shipit--get-branch-protection-requirements repo base-ref)))

(defun shipit-pr-github--fetch-codeowners (config base-ref)
  "Fetch CODEOWNERS content for BASE-REF using CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching CODEOWNERS for %s" base-ref)
    (shipit--fetch-codeowners-from-base-branch repo base-ref)))

(defun shipit-pr-github--add-label (config number label-name)
  "Add LABEL-NAME to PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/labels" repo number))
         (data `((labels . (,label-name)))))
    (shipit--debug-log "GitHub PR backend: adding label %s to PR #%s in %s" label-name number repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--remove-label (config number label-name)
  "Remove LABEL-NAME from PR NUMBER using CONFIG.
Signals error on non-200/204 response.  Returns t on success."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/labels/%s" repo number label-name))
         (url (concat shipit-api-url endpoint))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "GitHub PR backend: removing label %s from PR #%s in %s" label-name number repo)
    (let ((status-code (shipit--url-retrieve-sync url "DELETE" headers nil t)))
      (unless (and (numberp status-code) (memq status-code '(200 204)))
        (error "GitHub remove label failed with status %s" status-code))
      t)))

(defun shipit-pr-github--add-reaction (config number reaction)
  "Add REACTION to PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/reactions" repo number))
         (data `((content . ,reaction))))
    (shipit--debug-log "GitHub PR backend: adding reaction %s to PR #%s in %s" reaction number repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--fetch-reactions (config number)
  "Fetch reactions for PR NUMBER using CONFIG.
Uses ETag-based sync fetch for freshness."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/reactions" repo number)))
    (shipit--debug-log "GitHub PR backend: fetching reactions for PR #%s in %s" number repo)
    (let* ((etag-result (shipit-gh-etag-get-json endpoint nil shipit-github-token t))
           (reactions (plist-get etag-result :json)))
      (or reactions '()))))

(defun shipit-pr-github--dismiss-review (config number message)
  "Dismiss the current user's review on PR NUMBER using CONFIG.
MESSAGE is the dismissal reason.  Finds the most recent APPROVED or
CHANGES_REQUESTED review by the current user and dismisses it.
Returns the API result, or nil if no dismissable review found."
  (let* ((repo (plist-get config :repo))
         (reviews-endpoint (format "/repos/%s/pulls/%s/reviews" repo number))
         (reviews-data (shipit--api-request reviews-endpoint))
         (current-user (shipit--get-current-user))
         (user-reviews (and current-user reviews-data
                            (cl-remove-if-not
                             (lambda (review)
                               (string= (cdr (assq 'login (cdr (assq 'user review))))
                                        current-user))
                             reviews-data)))
         (sorted-reviews (sort user-reviews
                               (lambda (a b)
                                 (> (cdr (assq 'id a)) (cdr (assq 'id b))))))
         (user-review (cl-find-if
                       (lambda (review)
                         (let ((state (cdr (assq 'state review))))
                           (or (string= state "APPROVED")
                               (string= state "CHANGES_REQUESTED"))))
                       sorted-reviews)))
    (shipit--debug-log "GitHub PR backend: dismiss-review user=%S reviews=%d user-reviews=%d"
                       current-user (length reviews-data) (length user-reviews))
    (when user-review
      (let* ((review-id (cdr (assq 'id user-review)))
             (dismiss-endpoint (format "/repos/%s/pulls/%s/reviews/%s/dismissals"
                                       repo number review-id))
             (data `((message . ,(or message "Review dismissed")))))
        (shipit--debug-log "GitHub PR backend: dismissing review %s on PR #%s in %s" review-id number repo)
        (shipit--api-request-post dismiss-endpoint data "PUT")))))

(defun shipit-pr-github--add-reviewer (config number username)
  "Request review from USERNAME on PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/requested_reviewers" repo number))
         (data `((reviewers . [,username]))))
    (shipit--debug-log "GitHub PR backend: adding reviewer %s to PR #%s in %s" username number repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--remove-reviewer (config number username)
  "Remove USERNAME from requested reviewers on PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/requested_reviewers" repo number))
         (data `((reviewers . [,username]))))
    (shipit--debug-log "GitHub PR backend: removing reviewer %s from PR #%s in %s" username number repo)
    (shipit--api-request-post endpoint data "DELETE")))

(defun shipit-pr-github--fetch-requested-reviewers (config number)
  "Fetch requested reviewers for PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/requested_reviewers" repo number)))
    (shipit--debug-log "GitHub PR backend: fetching requested reviewers for PR #%s in %s" number repo)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--add-assignee (config number username)
  "Add USERNAME as assignee to PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/assignees" repo number))
         (data `((assignees . [,username]))))
    (shipit--debug-log "GitHub PR backend: adding assignee %s to PR #%s in %s" username number repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--remove-assignee (config number username)
  "Remove USERNAME from assignees on PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/assignees" repo number))
         (data `((assignees . [,username]))))
    (shipit--debug-log "GitHub PR backend: removing assignee %s from PR #%s in %s" username number repo)
    (shipit--api-request-post endpoint data "DELETE")))

(defun shipit-pr-github--fetch-pr-assignees (config number)
  "Fetch assignees for PR NUMBER using CONFIG.
Returns the assignees list from the issue endpoint."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s" repo number)))
    (shipit--debug-log "GitHub PR backend: fetching assignees for PR #%s in %s" number repo)
    (let ((issue-data (shipit--api-request endpoint)))
      (cdr (assq 'assignees issue-data)))))

(defun shipit-pr-github--fetch-available-assignees (config)
  "Fetch available assignees for repo in CONFIG with pagination."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/assignees" repo))
         (per-page 100)
         (page 1)
         (all-assignees '())
         (continue t))
    (shipit--debug-log "GitHub PR backend: fetching available assignees for %s" repo)
    (while continue
      (let* ((paginated-endpoint (format "%s?per_page=%d&page=%d" endpoint per-page page))
             (result (shipit-gh-etag-get-json-with-refresh-cache paginated-endpoint nil shipit-github-token))
             (assignees (plist-get result :json)))
        (if (and assignees (> (length assignees) 0))
            (progn
              (setq all-assignees (append all-assignees assignees))
              (setq page (1+ page))
              (when (< (length assignees) per-page)
                (setq continue nil)))
          (setq continue nil))))
    all-assignees))

(defun shipit-pr-github--fetch-available-labels (config)
  "Fetch available labels for repo in CONFIG with pagination."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/labels" repo))
         (per-page 100)
         (page 1)
         (all-labels '())
         (continue t))
    (shipit--debug-log "GitHub PR backend: fetching available labels for %s" repo)
    (while continue
      (let* ((paginated-endpoint (format "%s?per_page=%d&page=%d" endpoint per-page page))
             (labels (shipit--api-request paginated-endpoint)))
        (if (and labels (listp labels) (> (length labels) 0))
            (progn
              (setq all-labels (append all-labels labels))
              (setq page (1+ page))
              (when (< (length labels) per-page)
                (setq continue nil)))
          (setq continue nil))))
    all-labels))

(defun shipit-pr-github--fetch-labels (config number)
  "Fetch labels for PR NUMBER using CONFIG.
Returns minimal PR structure with labels."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s" repo number)))
    (shipit--debug-log "GitHub PR backend: fetching labels for PR #%s in %s" number repo)
    (let ((issue-data (shipit--api-request endpoint)))
      `((number . ,number)
        (labels . ,(cdr (assq 'labels issue-data)))
        (title . ,(cdr (assq 'title issue-data)))
        (state . ,(cdr (assq 'state issue-data)))))))

(defun shipit-pr-github--fetch-commit (config sha)
  "Fetch commit data for SHA using CONFIG.
Returns commit data including files list."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/commits/%s" repo sha)))
    (shipit--debug-log "GitHub PR backend: fetching commit %s in %s" sha repo)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--delete-reaction (config number reaction-id)
  "Delete reaction REACTION-ID from PR NUMBER using CONFIG.
Signals error on non-200/204 response.  Returns t on success."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/reactions/%s" repo number reaction-id))
         (url (concat shipit-api-url endpoint))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "GitHub PR backend: deleting reaction %s from PR #%s in %s" reaction-id number repo)
    (let ((status-code (shipit--url-retrieve-sync url "DELETE" headers nil t)))
      (unless (and (numberp status-code) (memq status-code '(200 204)))
        (error "GitHub delete PR reaction failed with status %s" status-code))
      t)))

(defun shipit-pr-github--add-reviewers-batch (config number usernames)
  "Request reviews from USERNAMES (a list) on PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls/%s/requested_reviewers" repo number))
         (data `((reviewers . ,(vconcat usernames)))))
    (shipit--debug-log "GitHub PR backend: adding %d reviewers to PR #%s in %s"
                       (length usernames) number repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--add-assignees-batch (config number usernames)
  "Add USERNAMES (a list) as assignees to PR NUMBER using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/assignees" repo number))
         (data `((assignees . ,(vconcat usernames)))))
    (shipit--debug-log "GitHub PR backend: adding %d assignees to PR #%s in %s"
                       (length usernames) number repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--set-labels (config number label-names)
  "Set LABEL-NAMES (a list) on PR NUMBER using CONFIG.
Uses PUT to replace all labels (POST would add without removing)."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/issues/%s/labels" repo number))
         (data `((labels . ,(vconcat label-names)))))
    (shipit--debug-log "GitHub PR backend: setting %d labels on PR #%s in %s"
                       (length label-names) number repo)
    (shipit--api-request-post endpoint data "PUT")))

(defun shipit-pr-github--fetch-check-suites (config ref page per-page)
  "Fetch check suites for commit REF using CONFIG with pagination."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/commits/%s/check-suites?page=%d&per_page=%d"
                           repo ref page per-page)))
    (shipit--debug-log "GitHub PR backend: fetching check suites for %s page %d" ref page)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--fetch-suite-check-runs (config suite-id)
  "Fetch check runs for check suite SUITE-ID using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/check-suites/%s/check-runs?per_page=100"
                           repo suite-id)))
    (shipit--debug-log "GitHub PR backend: fetching check runs for suite %s" suite-id)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--fetch-workflow-info (config workflow-id)
  "Fetch workflow data for WORKFLOW-ID using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/actions/workflows/%s" repo workflow-id)))
    (shipit--debug-log "GitHub PR backend: fetching workflow info %s" workflow-id)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--fetch-action-run-info (config run-id)
  "Fetch action run data for RUN-ID using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/actions/runs/%s" repo run-id)))
    (shipit--debug-log "GitHub PR backend: fetching action run info %s" run-id)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--fetch-commit-check-runs (config ref page per-page)
  "Fetch check runs for commit REF using CONFIG with pagination."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/commits/%s/check-runs?page=%d&per_page=%d"
                           repo ref page per-page)))
    (shipit--debug-log "GitHub PR backend: fetching commit check runs for %s page %d" ref page)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--fetch-file-content (config file-path ref)
  "Fetch FILE-PATH content at REF via GitHub Contents API.
CONFIG provides :repo.  Returns file content as a string, or nil."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching file content %s at %s from %s" file-path ref repo)
    (shipit--fetch-file-content-from-github repo file-path ref)))

(defun shipit-pr-github--fetch-compare (config base head)
  "Fetch compare data between BASE and HEAD commits using CONFIG."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/compare/%s...%s" repo base head)))
    (shipit--debug-log "GitHub PR backend: fetching compare %s...%s in %s" base head repo)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--search-repos (_config query)
  "Search GitHub repositories matching QUERY.
Returns list of alists with `full_name' and `description' keys."
  (let* ((endpoint (format "/search/repositories?q=%s&per_page=100"
                           (url-hexify-string query)))
         (response (shipit--api-request endpoint)))
    (shipit--debug-log "GitHub repo search for: %s" query)
    (when (and response (listp response))
      (let ((items (cdr (assq 'items response))))
        (mapcar (lambda (repo)
                  `((full_name . ,(cdr (assq 'full_name repo)))
                    (description . ,(cdr (assq 'description repo)))))
                (append items nil))))))

(defun shipit-pr-github--search-repos-request (_config query)
  "Return (URL HEADERS NORMALIZER) for a GitHub repo search.
Used by `shipit--read-repo' to search via curl."
  (let* ((url (format "%s/search/repositories?q=%s&per_page=100"
                      (or shipit-api-url "https://api.github.com")
                      (url-hexify-string query)))
         (headers
          (append
           (let ((token (shipit--github-token)))
             (when token
               `(("Authorization" . ,(format "token %s" token)))))
           '(("Accept" . "application/vnd.github.v3+json"))))
         (normalizer
          (lambda (data)
            (when (and data (listp data))
              (let ((items (cdr (assq 'items data))))
                (mapcar (lambda (repo)
                          `((full_name . ,(cdr (assq 'full_name repo)))
                            (description . ,(cdr (assq 'description repo)))))
                        (if (vectorp items) (append items nil) items)))))))
    (list url headers normalizer)))

(defun shipit-pr-github--search-raw (config query page per-page)
  "Search issues/PRs with raw QUERY string using CONFIG."
  (let ((endpoint (format "/search/issues?q=%s&sort=updated&order=desc&per_page=%d&page=%d"
                          query per-page page)))
    (shipit--debug-log "GitHub PR backend: raw search page %d: %s" page query)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--refspec-for-pr (_config pr-number _head-ref)
  "Return GitHub refspec for PR-NUMBER.
GitHub exposes PR refs at refs/pull/N/head."
  (format "refs/pull/%s/head" pr-number))

(defun shipit-pr-github--remote-for-fetch (_config repo)
  "Return remote for fetching REPO.
Returns HTTPS URL for the repo, or \"origin\" if REPO is nil."
  (if repo
      (format "https://github.com/%s.git" repo)
    "origin"))

(defun shipit-pr-github--get-file-line-content (repo ref path &optional _line-number)
  "Fetch full file content of PATH at REF in REPO via GitHub Contents API.
Returns the decoded file content as a string, or nil if not found.
Caching and line extraction are handled by the common dispatch layer."
  (let* ((endpoint (format "/repos/%s/contents/%s?ref=%s"
                           repo (url-hexify-string path) ref))
         (response (shipit--api-request endpoint))
         (response (or response
                       (progn
                         (shipit--debug-log "STALE-CHECK: Retrying with cache-bust for %s" path)
                         (shipit--api-request (concat endpoint "&_cb=1")))))
         (content-b64 (cdr (assq 'content response))))
    (when content-b64
      (decode-coding-string
       (base64-decode-string
        (replace-regexp-in-string "\n" "" content-b64))
       'utf-8))))

;;; Reference overlays

(declare-function shipit--pr-reference-action-menu "shipit-render")
(declare-function shipit--show-pr-preview "shipit-render")
(declare-function shipit-discussions-open-buffer "shipit-discussions")

(defun shipit-pr-github--create-reference-overlays (_repo search-start search-limit inc-found inc-overlay)
  "Create overlays for GitHub PR, issue, and discussion URLs.
Searches between SEARCH-START and SEARCH-LIMIT.
INC-FOUND and INC-OVERLAY are thunks called to increment counters."
  ;; GitHub PR URLs (https://github.com/owner/repo/pull/123)
  (save-excursion
    (goto-char search-start)
    (while (re-search-forward "https://github\\.com/\\([^/]+/[^/]+\\)/pull/\\([0-9]+\\)" search-limit t)
      (funcall inc-found)
      (let ((url-repo (match-string-no-properties 1))
            (ref-num (match-string-no-properties 2))
            (start (match-beginning 0))
            (end (match-end 0))
            (backtick-count 0))
        (save-excursion
          (beginning-of-line)
          (while (< (point) start)
            (when (= (char-after) ?`)
              (setq backtick-count (1+ backtick-count)))
            (forward-char)))
        (if (= (mod backtick-count 2) 1)
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "Skipping PR URL %s/pull/%s (in code block)" url-repo ref-num))
          (let* ((ref-num-int (string-to-number ref-num))
                 (url-repo-copy url-repo)
                 (ov (make-overlay start end))
                 (keymap (make-sparse-keymap)))
            (set-keymap-parent keymap (current-local-map))
            (funcall inc-overlay)
            (define-key keymap (kbd "RET")
              (lambda ()
                (interactive)
                (shipit--pr-reference-action-menu ref-num-int url-repo-copy 'pr)))
            (define-key keymap (kbd "M-?")
              (lambda ()
                (interactive)
                (shipit--show-pr-preview ref-num-int url-repo-copy)))
            (overlay-put ov 'face 'markdown-plain-url-face)
            (overlay-put ov 'keymap keymap)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'help-echo (format "PR %s#%s - RET: actions, M-?: preview" url-repo ref-num))
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "Created overlay for PR URL %s/pull/%s at %d-%d" url-repo ref-num start end)))))))
  ;; GitHub issue URLs (https://github.com/owner/repo/issues/123)
  (save-excursion
    (goto-char search-start)
    (while (re-search-forward "https://github\\.com/\\([^/]+/[^/]+\\)/issues/\\([0-9]+\\)" search-limit t)
      (funcall inc-found)
      (let ((url-repo (match-string-no-properties 1))
            (ref-num (match-string-no-properties 2))
            (start (match-beginning 0))
            (end (match-end 0))
            (backtick-count 0))
        (save-excursion
          (beginning-of-line)
          (while (< (point) start)
            (when (= (char-after) ?`)
              (setq backtick-count (1+ backtick-count)))
            (forward-char)))
        (if (= (mod backtick-count 2) 1)
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "Skipping issue URL %s/issues/%s (in code block)" url-repo ref-num))
          (let* ((ref-num-int (string-to-number ref-num))
                 (url-repo-copy url-repo)
                 (ov (make-overlay start end))
                 (keymap (make-sparse-keymap)))
            (set-keymap-parent keymap (current-local-map))
            (funcall inc-overlay)
            (define-key keymap (kbd "RET")
              (lambda ()
                (interactive)
                (shipit--pr-reference-action-menu ref-num-int url-repo-copy)))
            (overlay-put ov 'face 'markdown-plain-url-face)
            (overlay-put ov 'keymap keymap)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'help-echo (format "%s#%s - RET: actions" url-repo ref-num))
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "Created overlay for issue URL %s/issues/%s at %d-%d" url-repo ref-num start end)))))))
  ;; GitHub discussion URLs (https://github.com/owner/repo/discussions/123)
  (save-excursion
    (goto-char search-start)
    (while (re-search-forward "https://github\\.com/\\([^/]+/[^/]+\\)/discussions/\\([0-9]+\\)" search-limit t)
      (funcall inc-found)
      (let ((url-repo (match-string-no-properties 1))
            (ref-num (match-string-no-properties 2))
            (start (match-beginning 0))
            (end (match-end 0))
            (backtick-count 0))
        (save-excursion
          (beginning-of-line)
          (while (< (point) start)
            (when (= (char-after) ?`)
              (setq backtick-count (1+ backtick-count)))
            (forward-char)))
        (if (= (mod backtick-count 2) 1)
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "Skipping discussion URL %s/discussions/%s (in code block)" url-repo ref-num))
          (let* ((ref-num-int (string-to-number ref-num))
                 (url-repo-copy url-repo)
                 (ov (make-overlay start end))
                 (keymap (make-sparse-keymap)))
            (set-keymap-parent keymap (current-local-map))
            (funcall inc-overlay)
            (define-key keymap (kbd "RET")
              (lambda ()
                (interactive)
                (if (fboundp 'shipit-discussions-open-buffer)
                    (shipit-discussions-open-buffer ref-num-int url-repo-copy)
                  (browse-url (format "https://github.com/%s/discussions/%d"
                                      url-repo-copy ref-num-int)))))
            (overlay-put ov 'face 'markdown-plain-url-face)
            (overlay-put ov 'keymap keymap)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'help-echo (format "Discussion %s#%s - RET: open" url-repo ref-num))
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log "Created overlay for discussion URL %s/discussions/%s at %d-%d" url-repo ref-num start end))))))))

;;; Code URL expansion

(defun shipit-pr-github--expand-code-urls (text)
  "Expand GitHub code URLs in TEXT to show code snippets with clickable links."
  (let ((result text))
    (while (string-match "https://github\\.com/\\([^/]+\\)/\\([^/]+\\)/blob/\\([^/]+\\)/\\([^#[:space:]]+\\)#L\\([0-9]+\\)\\(?:-L\\([0-9]+\\)\\)?" result)
      (let* ((full-match (match-string 0 result))
             (match-start (match-beginning 0))
             (match-end (match-end 0))
             ;; Extract the actual URL text, handling protected text properties
             (url-text (let ((extracted (substring result match-start match-end)))
                         (if (get-text-property 0 'shipit-github-url extracted)
                             ;; If it's protected, get the original text without properties
                             (substring-no-properties extracted)
                           extracted)))
             (owner (match-string 1 result))
             (repo (match-string 2 result))
             (sha (match-string 3 result))
             (raw-file-path (match-string 4 result))
             (file-path (url-unhex-string raw-file-path))
             (start-line (string-to-number (match-string 5 result)))
             (end-line (when (match-string 6 result)
                         (string-to-number (match-string 6 result)))))
        (condition-case err
            (let ((snippet (shipit--fetch-code-snippet-with-navigation owner repo sha file-path start-line end-line)))
              (setq result (replace-regexp-in-string (regexp-quote full-match) snippet result)))
          (error
           (message "DEBUG: Error in URL expansion: %s" err)))))
    result))

;;; URL classification

(defun shipit-pr-github--classify-url (url)
  "Classify URL as a GitHub resource.
Returns plist (:type TYPE :repo REPO :number N) or nil."
  (cond
   ;; PR: /owner/repo/pull/N (with optional trailing path/fragment)
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/pull/\\([0-9]+\\)\\(?:/[^?#]*\\)?\\(?:[?#].*\\)?\\'"
     url)
    (list :type 'pr
          :repo (match-string 1 url)
          :number (string-to-number (match-string 2 url))))
   ;; Issue: /owner/repo/issues/N (with optional trailing path/fragment)
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/issues/\\([0-9]+\\)\\(?:/[^?#]*\\)?\\(?:[?#].*\\)?\\'"
     url)
    (list :type 'issue
          :repo (match-string 1 url)
          :number (string-to-number (match-string 2 url))))
   ;; Actions run: /owner/repo/actions/runs/RUN_ID (with optional /job/JOB_ID)
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/actions/runs/\\([0-9]+\\)\\(?:/job/\\([0-9]+\\)\\)?/?\\'"
     url)
    (let ((result (list :type 'actions-run
                        :repo (match-string 1 url)
                        :run-id (match-string 2 url))))
      (when (match-string 3 url)
        (setq result (plist-put result :job-id (match-string 3 url))))
      result))
   ;; Repo: /owner/repo (exactly 2 segments)
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/?\\'"
     url)
    (list :type 'repo
          :repo (match-string 1 url)))))

;;; URL builders

(defun shipit-pr-github--browse-issue-url (config number)
  "Return browser URL for issue NUMBER using CONFIG."
  (format "https://github.com/%s/issues/%s" (plist-get config :repo) number))

(defun shipit-pr-github--browse-commit-url (config sha)
  "Return browser URL for commit SHA using CONFIG."
  (format "https://github.com/%s/commit/%s" (plist-get config :repo) sha))

(defun shipit-pr-github--browse-user-url (_config username)
  "Return browser URL for USERNAME's GitHub profile."
  (format "https://github.com/%s" username))

;;; User helpers

(defun shipit-pr-github--get-current-username (_config)
  "Return the authenticated GitHub username."
  (let* ((result (shipit-gh-etag-get-json-with-refresh-cache
                  "/user" nil shipit-github-token))
         (user-data (when result (plist-get result :json))))
    (cdr (assq 'login user-data))))

(defun shipit-pr-github--extract-username-from-email (_config email)
  "Extract GitHub username from noreply EMAIL, or nil."
  (cond
   ((string-match "^\\([^@+]+\\)@users\\.noreply\\.github\\.com$" email)
    (match-string 1 email))
   ((string-match "^[0-9]+\\+\\([^@+]+\\)@users\\.noreply\\.github\\.com$" email)
    (match-string 1 email))
   ((string-match "^\\([^@+]+\\)@github\\.com$" email)
    (match-string 1 email))
   (t nil)))

(defun shipit-pr-github--generate-avatar-url (_config username)
  "Generate avatar URL for GitHub USERNAME."
  (format "https://github.com/%s.png?size=20" username))

;;; Diagnostics

(defun shipit-pr-github--fetch-rate-limit (_config)
  "Fetch GitHub API rate limit data.
Returns alist with resources (core, search, graphql) or signals error."
  (let* ((url (concat (or shipit-api-url "https://api.github.com") "/rate_limit"))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (result (shipit--url-retrieve-sync url "GET" headers nil))
         (data (car result))
         (status-code (cdr result)))
    (if (and status-code (>= status-code 200) (< status-code 300))
        data
      (error "HTTP %s" status-code))))

(defun shipit-pr-github--fetch-current-user (_config)
  "Fetch authenticated GitHub user.
Returns alist with login and name fields or signals error."
  (let* ((url (concat (or shipit-api-url "https://api.github.com") "/user"))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (result (shipit--url-retrieve-sync url "GET" headers nil))
         (data (car result))
         (status-code (cdr result)))
    (if (and status-code (>= status-code 200) (< status-code 300))
        data
      (error "HTTP %s" status-code))))

;;; Async check operations
;; Async variants for the checks pipeline — used by shipit-checks.el
;; to avoid calling shipit--api-request (GitHub HTTP) directly.

(defun shipit-pr-github--fetch-check-suites-async (config ref page per-page callback)
  "Fetch check suites for commit REF using CONFIG asynchronously.
PAGE and PER-PAGE control pagination.  Calls CALLBACK with parsed data."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/commits/%s/check-suites?page=%d&per_page=%d"
                           repo ref page per-page)))
    (shipit--debug-log "GitHub PR backend: async fetch check suites for %s page %d" ref page)
    (shipit--api-request endpoint nil callback)))

(defun shipit-pr-github--fetch-suite-check-runs-async (config suite-id callback)
  "Fetch check runs for check suite SUITE-ID using CONFIG asynchronously.
Calls CALLBACK with parsed data."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/check-suites/%s/check-runs?per_page=100"
                           repo suite-id)))
    (shipit--debug-log "GitHub PR backend: async fetch check runs for suite %s" suite-id)
    (shipit--api-request endpoint nil callback)))

(defun shipit-pr-github--fetch-workflow-info-async (config workflow-id callback)
  "Fetch workflow data for WORKFLOW-ID using CONFIG asynchronously.
Calls CALLBACK with parsed data."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/actions/workflows/%s" repo workflow-id)))
    (shipit--debug-log "GitHub PR backend: async fetch workflow info %s" workflow-id)
    (shipit--api-request endpoint nil callback)))

(defun shipit-pr-github--fetch-action-run-info-async (config run-id callback)
  "Fetch action run data for RUN-ID using CONFIG asynchronously.
Calls CALLBACK with parsed data."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/actions/runs/%s" repo run-id)))
    (shipit--debug-log "GitHub PR backend: async fetch action run info %s" run-id)
    (shipit--api-request endpoint nil callback)))

(defun shipit-pr-github--fetch-commit-check-runs-async (config ref page per-page callback)
  "Fetch check runs for commit REF using CONFIG asynchronously.
PAGE and PER-PAGE control pagination.  Calls CALLBACK with parsed data."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/commits/%s/check-runs?page=%d&per_page=%d"
                           repo ref page per-page)))
    (shipit--debug-log "GitHub PR backend: async fetch commit check runs for %s page %d" ref page)
    (shipit--api-request endpoint nil callback)))

;;; Repository info operations

(defun shipit-pr-github--fetch-repo-info (config)
  "Fetch repository metadata using CONFIG.
Returns the JSON alist from GET /repos/{owner}/{repo}."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s" repo)))
    (shipit--debug-log "GitHub PR backend: fetching repo info for %s" repo)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--get-repo-subscription (config)
  "Get subscription for repo in CONFIG.
Returns subscription alist or nil if not subscribed (404)."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/subscription" repo)))
    (shipit--debug-log "GitHub PR backend: fetching subscription for %s" repo)
    (shipit--api-request endpoint)))

(defun shipit-pr-github--set-repo-subscription (config state)
  "Set subscription STATE for repo in CONFIG.
STATE is \"watching\", \"participating\", or \"ignoring\"."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/subscription" repo)))
    (shipit--debug-log "GitHub PR backend: setting subscription for %s to %s" repo state)
    (cond
     ((equal state "watching")
      (shipit--api-request-post endpoint
                                '((subscribed . t) (ignored . :json-false))
                                "PUT"))
     ((equal state "ignoring")
      (shipit--api-request-post endpoint
                                '((ignored . t))
                                "PUT"))
     ((equal state "participating")
      (shipit--api-request-post endpoint nil "DELETE")))))

(defun shipit-pr-github--mark-repo-notifications-read (config)
  "Mark all notifications for repo in CONFIG as read on GitHub."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/notifications" repo)))
    (shipit--debug-log "GitHub PR backend: marking all notifications read for %s" repo)
    (shipit--api-request-post endpoint nil "PUT")))

(defun shipit-pr-github--fetch-readme (config)
  "Fetch repository README using CONFIG.
Calls GET /repos/{owner}/{repo}/readme, decodes base64 content.
Returns the README text as a string, or nil if not found."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/readme" repo)))
    (shipit--debug-log "GitHub PR backend: fetching README for %s" repo)
    (let ((response (shipit--api-request endpoint)))
      (when response
        (let ((content-b64 (cdr (assq 'content response))))
          (when content-b64
            (decode-coding-string
             (base64-decode-string
              (replace-regexp-in-string "\n" "" content-b64))
             'utf-8)))))))

;;; Language operations

(defun shipit-pr-github--fetch-languages (config)
  "Fetch repository languages using CONFIG.
Returns alist of (LANGUAGE . BYTES)."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/languages" repo)))
    (shipit--debug-log "GitHub PR backend: fetching languages for %s" repo)
    (shipit--api-request endpoint)))

;;; Notification operations

(defun shipit-pr-github--mark-notification-read (config notification-id)
  "Mark notification NOTIFICATION-ID as read using CONFIG.
CONFIG is unused (GitHub uses global auth) but kept for backend API consistency."
  (ignore config)
  (let ((endpoint (format "/notifications/threads/%s" notification-id)))
    (shipit--debug-log "GitHub PR backend: marking notification %s as read" notification-id)
    (shipit-pr-github--api-request-patch-no-json endpoint)))

(defun shipit-pr-github--api-request-patch-no-json (endpoint)
  "Make a PATCH request that doesn't expect JSON response.
Used for marking notifications as read, which returns 205 with empty body."
  (unless (shipit--github-token)
    (error "GitHub token not found.  Set `shipit-github-token' or add github.com to auth-source"))
  (let* ((url (concat shipit-api-url endpoint))
         (headers (list (shipit--get-auth-header)
                        '("Accept" . "application/vnd.github.v3+json")))
         (headers (remove nil headers)))
    (shipit--debug-log "API REQUEST (PATCH-NO-JSON): %s" url)
    (let ((status-code (shipit--url-retrieve-sync url "PATCH" headers nil t)))
      (if (and status-code (>= status-code 200) (< status-code 300))
          t
        (error "API returned HTTP %s" status-code)))))

(defun shipit-pr-github--fetch-notifications (config params &optional force-fresh)
  "Fetch notifications using CONFIG with query PARAMS.
If FORCE-FRESH is non-nil, bypasses ETag caching."
  (ignore config)
  (let* ((result (shipit-gh-etag-get-json-with-refresh-cache
                  "/notifications" params shipit-github-token force-fresh))
         (notifications (when result (plist-get result :json))))
    notifications))

(defun shipit-pr-github--browse-discussion-url (config number)
  "Return browser URL for discussion NUMBER using CONFIG."
  (format "https://github.com/%s/discussions/%s" (plist-get config :repo) number))

(defun shipit-pr-github--fetch-user-teams (_config)
  "Fetch teams the authenticated user belongs to.
Returns list of team data alists."
  (shipit--api-request "/user/teams?per_page=100"))

;;; GitHub user completion for search transients

(defun shipit--get-git-authors ()
  "Get authors from Git repository using git shortlog, similar to Magit."
  (let ((lines (magit-git-lines "shortlog" "-n" "-s" "-e" "HEAD")))
    (shipit--debug-log "Git shortlog found %d authors" (length lines))
    (mapcar (lambda (line)
              ;; Parse format: "    42\tJohn Doe <john@example.com>"
              (save-match-data
                (when (string-match "\\`[\s\t]+[0-9]+\t\\(.+?\\)\\s-*<\\(.+?\\)>" line)
                  (let* ((name (match-string 1 line))
                         (email (match-string 2 line))
                         ;; Extract username from email (before @)
                         (username (if (string-match "\\(.+\\)@" email)
                                      (match-string 1 email)
                                    email))
                         (_display-string (format "%s (%s)" username name)))
                    (shipit--debug-log "Parsed author: username=%s, name=%s, email=%s" username name email)
                    ;; Create user object compatible with our formatting
                    `((login . ,username)
                      (name . ,name)
                      (email . ,email))))))
            lines)))

(defun shipit--search-git-authors (all-authors query)
  "Search Git AUTHORS locally, filtering by QUERY if provided."
  (if (string-empty-p query)
      all-authors
    ;; Filter authors by query (case-insensitive)
    (let ((query-lower (downcase query)))
      (seq-filter (lambda (author)
                    (let* ((login (cdr (assq 'login author)))
                           (name (cdr (assq 'name author)))
                           (email (cdr (assq 'email author))))
                      (or (and login (string-match-p query-lower (downcase login)))
                          (and name (string-match-p query-lower (downcase name)))
                          (and email (string-match-p query-lower (downcase email))))))
                  all-authors))))

(defun shipit--format-user-candidate (user)
  "Format USER data as completion candidate with display name."
  (let* ((login (cdr (assq 'login user)))
         (name (or (cdr (assq 'name user)) (cdr (assq 'display_name user))))
         (display-name (if (and name (not (string-empty-p name)) (not (string= name login)))
                          (format "%s (%s)" login name)
                        login)))
    (propertize display-name 'shipit-user-login login)))

(defun shipit--read-github-user (prompt &optional initial-input history)
  "Read user from Git authors with local filtering."
  (let* ((all-git-authors-cache nil)    ; Cache for Git authors
         (formatted-candidates-cache nil) ; Cache for formatted candidates
         (display-to-login-map (make-hash-table :test 'equal)) ; Map display names to logins
         (completion-function
          (lambda (string predicate action)
            (cond
             ((eq action t)
              ;; Return all completions
              (let* ((query (or string ""))
                     (authors (or all-git-authors-cache
                                  (setq all-git-authors-cache
                                        (shipit--get-git-authors))))
                     (candidates (or formatted-candidates-cache
                                     (setq formatted-candidates-cache
                                           (progn
                                             (shipit--debug-log "Formatting %d authors for completion" (length authors))
                                             (mapcar (lambda (author)
                                                       (let ((candidate (shipit--format-user-candidate author))
                                                             (login (cdr (assq 'login author))))
                                                         (puthash candidate login display-to-login-map)
                                                         candidate))
                                                     authors)))))
                     (filtered-authors (shipit--search-git-authors authors query))
                     (filtered-candidates (if (string-empty-p query)
                                            candidates
                                          ;; Filter cached candidates by matching author logins
                                          (let ((filtered-logins (mapcar (lambda (author) (cdr (assq 'login author))) filtered-authors)))
                                            (seq-filter (lambda (candidate)
                                                          (member (get-text-property 0 'shipit-user-login candidate) filtered-logins))
                                                        candidates)))))
                (shipit--debug-log "Query: '%s', found %d candidates (from %d total)" query (length filtered-candidates) (length candidates))
                (when (and filtered-candidates (< (length filtered-candidates) 5))
                  (shipit--debug-log "Sample candidates: %S" (mapcar (lambda (c) (list c (get-text-property 0 'shipit-user-login c))) (take 3 filtered-candidates))))
                (all-completions string filtered-candidates predicate)))
             ((eq action 'lambda)
              ;; Test validity
              (and string (get-text-property 0 'shipit-user-login string)))
             ((eq action nil)
              ;; Find best match
              (let* ((query (or string ""))
                     (authors (or all-git-authors-cache
                                  (setq all-git-authors-cache
                                        (shipit--get-git-authors))))
                     (candidates (or formatted-candidates-cache
                                     (setq formatted-candidates-cache
                                           (mapcar (lambda (author)
                                                     (let ((candidate (shipit--format-user-candidate author))
                                                           (login (cdr (assq 'login author))))
                                                       (puthash candidate login display-to-login-map)
                                                       candidate))
                                                   authors))))
                     (filtered-authors (shipit--search-git-authors authors query))
                     (filtered-candidates (if (string-empty-p query)
                                            candidates
                                          (let ((filtered-logins (mapcar (lambda (author) (cdr (assq 'login author))) filtered-authors)))
                                            (seq-filter (lambda (candidate)
                                                          (member (get-text-property 0 'shipit-user-login candidate) filtered-logins))
                                                        candidates)))))
                (try-completion string filtered-candidates predicate)))
             ((eq action 'metadata)
              ;; Return metadata
              '(metadata (category . git-author))))))
         (selected (completing-read prompt completion-function nil nil initial-input history)))
    ;; Return the login name, not the display name
    (let ((login-from-property (get-text-property 0 'shipit-user-login selected))
          (login-from-map (gethash selected display-to-login-map)))
      (shipit--debug-log "Selected: '%s', login from property: '%s', login from map: '%s'" selected login-from-property login-from-map)
      (or login-from-property login-from-map selected))))

;; Register the GitHub PR backend
(shipit-pr-register-backend
 'github
 (list :name "GitHub"
       :detect-url-pattern "github\\.com"
       :pr-type-label "Pull Request"
       :pr-type-short-label "PR"
       :emoji-fallback "🐙"
       :icon-spec '("mark-github" "octicons" . "#888888")
       :icon-fallback-text "GH"
       :editor-reference-hints "#: PR"
       :fetch-pr #'shipit-pr-github--fetch-pr
       :search #'shipit-pr-github--search
       :create-pr #'shipit-pr-github--create-pr
       :merge-pr #'shipit-pr-github--merge-pr
       :update-pr #'shipit-pr-github--update-pr
       :fetch-reviews #'shipit-pr-github--fetch-reviews
       :submit-review #'shipit-pr-github--submit-review
       :fetch-review-decision #'shipit-pr-github--fetch-review-decision
       :fetch-files #'shipit-pr-github--fetch-files
       :fetch-commits #'shipit-pr-github--fetch-commits
       :fetch-checks #'shipit-pr-github--fetch-checks
       :browse-url #'shipit-pr-github--browse-url
       :fetch-review-threads #'shipit-pr-github--fetch-review-threads
       :fetch-resolved-threads #'shipit-pr-github--fetch-resolved-threads
       :fetch-timeline #'shipit-pr-github--fetch-timeline
       :fetch-review-decision-async #'shipit-pr-github--fetch-review-decision-async
       :fetch-timeline-async #'shipit-pr-github--fetch-timeline-async
       :fetch-commits-async #'shipit-pr-github--fetch-commits-async
       :fetch-files-async #'shipit-pr-github--fetch-files-async
       :fetch-branch-protection #'shipit-pr-github--fetch-branch-protection
       :fetch-codeowners #'shipit-pr-github--fetch-codeowners
       :add-label #'shipit-pr-github--add-label
       :remove-label #'shipit-pr-github--remove-label
       :dismiss-review #'shipit-pr-github--dismiss-review
       :add-reaction #'shipit-pr-github--add-reaction
       :fetch-reactions #'shipit-pr-github--fetch-reactions
       :delete-reaction #'shipit-pr-github--delete-reaction
       :add-reviewer #'shipit-pr-github--add-reviewer
       :remove-reviewer #'shipit-pr-github--remove-reviewer
       :fetch-requested-reviewers #'shipit-pr-github--fetch-requested-reviewers
       :add-assignee #'shipit-pr-github--add-assignee
       :remove-assignee #'shipit-pr-github--remove-assignee
       :fetch-pr-assignees #'shipit-pr-github--fetch-pr-assignees
       :fetch-available-assignees #'shipit-pr-github--fetch-available-assignees
       :fetch-available-labels #'shipit-pr-github--fetch-available-labels
       :fetch-labels #'shipit-pr-github--fetch-labels
       :fetch-commit #'shipit-pr-github--fetch-commit
       :add-reviewers-batch #'shipit-pr-github--add-reviewers-batch
       :add-assignees-batch #'shipit-pr-github--add-assignees-batch
       :set-labels #'shipit-pr-github--set-labels
       :fetch-check-suites #'shipit-pr-github--fetch-check-suites
       :fetch-suite-check-runs #'shipit-pr-github--fetch-suite-check-runs
       :fetch-workflow-info #'shipit-pr-github--fetch-workflow-info
       :fetch-action-run-info #'shipit-pr-github--fetch-action-run-info
       :fetch-commit-check-runs #'shipit-pr-github--fetch-commit-check-runs
       :fetch-compare #'shipit-pr-github--fetch-compare
       :fetch-file-content #'shipit-pr-github--fetch-file-content
       :search-raw #'shipit-pr-github--search-raw
       :search-repos #'shipit-pr-github--search-repos
       :search-repos-request #'shipit-pr-github--search-repos-request
       :refspec-for-pr #'shipit-pr-github--refspec-for-pr
       :remote-for-fetch #'shipit-pr-github--remote-for-fetch
       :get-file-line-content #'shipit-pr-github--get-file-line-content
       :browse-issue-url #'shipit-pr-github--browse-issue-url
       :browse-commit-url #'shipit-pr-github--browse-commit-url
       :browse-user-url #'shipit-pr-github--browse-user-url
       :get-current-username #'shipit-pr-github--get-current-username
       :extract-username-from-email #'shipit-pr-github--extract-username-from-email
       :generate-avatar-url #'shipit-pr-github--generate-avatar-url
       :fetch-rate-limit #'shipit-pr-github--fetch-rate-limit
       :fetch-current-user #'shipit-pr-github--fetch-current-user
       :fetch-check-suites-async #'shipit-pr-github--fetch-check-suites-async
       :fetch-suite-check-runs-async #'shipit-pr-github--fetch-suite-check-runs-async
       :fetch-workflow-info-async #'shipit-pr-github--fetch-workflow-info-async
       :fetch-action-run-info-async #'shipit-pr-github--fetch-action-run-info-async
       :fetch-commit-check-runs-async #'shipit-pr-github--fetch-commit-check-runs-async
       :mark-notification-read #'shipit-pr-github--mark-notification-read
       :fetch-notifications #'shipit-pr-github--fetch-notifications
       :browse-discussion-url #'shipit-pr-github--browse-discussion-url
       :fetch-user-teams #'shipit-pr-github--fetch-user-teams
       :fetch-repo-info #'shipit-pr-github--fetch-repo-info
       :fetch-readme #'shipit-pr-github--fetch-readme
       :fetch-languages #'shipit-pr-github--fetch-languages
       :get-repo-subscription #'shipit-pr-github--get-repo-subscription
       :set-repo-subscription #'shipit-pr-github--set-repo-subscription
       :mark-repo-notifications-read #'shipit-pr-github--mark-repo-notifications-read
       :classify-url #'shipit-pr-github--classify-url
       :fetch-file-viewed-states #'shipit-pr-github--fetch-file-viewed-states
       :mark-file-viewed #'shipit-pr-github--mark-file-viewed
       :unmark-file-viewed #'shipit-pr-github--unmark-file-viewed
       :expand-code-urls #'shipit-pr-github--expand-code-urls
       :create-reference-overlays #'shipit-pr-github--create-reference-overlays
       :detect-hash-reference (lambda (_config number repo)
                                (shipit--reference-detect-type number repo nil))
       :email-avatar-url (lambda (_config email)
                           (format "https://avatars.githubusercontent.com/u/e?email=%s&s=28"
                                   (url-hexify-string email)))
       :hash-insert-reference-fn #'shipit-editor-insert-pr-reference
       :browse-repo-url (lambda (config)
                          (format "https://github.com/%s" (plist-get config :repo)))
       :build-search-query-parts #'shipit--build-advanced-search-query
       :fetch-merge-methods #'shipit-pr-github--fetch-merge-methods
       :invalidate-pr-cache (lambda (config pr-number)
                              (shipit-gh-etag-invalidate-endpoint
                               (format "/repos/%s/pulls/%s"
                                       (plist-get config :repo) pr-number)))))

;;;
;;; File Viewed State
;;;

(defvar-local shipit-pr-github--pr-node-id-cache nil
  "Cached GraphQL node ID for the current PR.")

(defun shipit-pr-github--get-pr-node-id (config pr-number)
  "Get the GraphQL node ID for PR-NUMBER using CONFIG."
  (or shipit-pr-github--pr-node-id-cache
      (let* ((repo (plist-get config :repo))
             (parts (split-string repo "/"))
             (owner (car parts))
             (name (cadr parts))
             (query (format "{ repository(owner: \"%s\", name: \"%s\") { pullRequest(number: %d) { id } } }"
                            owner name pr-number))
             (result (shipit--graphql-query query nil)))
        (when result
          (let ((id (cdr (assq 'id (cdr (assq 'pullRequest (cdr (assq 'repository result))))))))
            (setq shipit-pr-github--pr-node-id-cache id)
            id)))))

(defun shipit-pr-github--fetch-file-viewed-states (config pr-number)
  "Fetch viewerViewedState for all files in PR-NUMBER using CONFIG.
Returns an alist of (filename . state) where state is
\"VIEWED\", \"UNVIEWED\", or \"DISMISSED\"."
  (let* ((repo (plist-get config :repo))
         (parts (split-string repo "/"))
         (owner (car parts))
         (name (cadr parts))
         (query (format "{ repository(owner: \"%s\", name: \"%s\") { pullRequest(number: %d) { files(first: 100) { nodes { path viewerViewedState } } } } }"
                        owner name pr-number))
         (result (shipit--graphql-query query nil)))
    (when result
      (let* ((pr (cdr (assq 'pullRequest (cdr (assq 'repository result)))))
             (files (cdr (assq 'files pr)))
             (nodes (cdr (assq 'nodes files))))
        (mapcar (lambda (node)
                  (cons (cdr (assq 'path node))
                        (cdr (assq 'viewerViewedState node))))
                (append nodes nil))))))

(defun shipit-pr-github--mark-file-viewed (config pr-number path)
  "Mark file at PATH as viewed in PR-NUMBER using CONFIG."
  (let ((pr-id (shipit-pr-github--get-pr-node-id config pr-number)))
    (when pr-id
      (shipit--graphql-query
       "mutation($input: MarkFileAsViewedInput!) { markFileAsViewed(input: $input) { clientMutationId } }"
       `((input . ((pullRequestId . ,pr-id) (path . ,path)))))
      (shipit--debug-log "Marked %s as viewed in PR #%d" path pr-number))))

(defun shipit-pr-github--unmark-file-viewed (config pr-number path)
  "Unmark file at PATH as viewed in PR-NUMBER using CONFIG."
  (let ((pr-id (shipit-pr-github--get-pr-node-id config pr-number)))
    (when pr-id
      (shipit--graphql-query
       "mutation($input: UnmarkFileAsViewedInput!) { unmarkFileAsViewed(input: $input) { clientMutationId } }"
       `((input . ((pullRequestId . ,pr-id) (path . ,path)))))
      (shipit--debug-log "Unmarked %s as viewed in PR #%d" path pr-number))))

(provide 'shipit-pr-github)
;;; shipit-pr-github.el ends here
