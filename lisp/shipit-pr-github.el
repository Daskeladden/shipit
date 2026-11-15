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

(require 'cl-lib)
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

(defun shipit-pr-github--fetch-pr-async (config number callback)
  "Fetch PR NUMBER using CONFIG in the background, then call CALLBACK.
CALLBACK gets the PR alist, or nil when the request fails, as it does
for the number of an issue.  Uses the same ETag cache as
`shipit-pr-github--fetch-pr'."
  (shipit-gh-etag-get-json-async
   (format "/repos/%s/pulls/%s" (plist-get config :repo) number)
   nil shipit-github-token
   (lambda (result) (funcall callback (plist-get result :json)))))

(defun shipit-pr-github--normalize-search-args (args)
  "Return ARGS as the transient strings the query builders take.
Callers hand over either raw transient arguments or a plist; the
plist crashed the builders, and the crash was swallowed upstream."
  (if (keywordp (car args))
      (cl-loop for (key value) on args by #'cddr
               collect (format "--%s=%s"
                               (substring (symbol-name key) 1) value))
    args))

(defun shipit-pr-github--search (config args)
  "Search PRs using CONFIG with transient ARGS, or a plist of them."
  (let* ((args (shipit-pr-github--normalize-search-args args))
         (repo (plist-get config :repo))
         (query-parts (shipit--build-advanced-search-query args repo))
         (limit (shipit--extract-limit-from-args args))
         (sort-by (shipit--extract-sort-from-args args))
         (per-page (min 50 limit)))
    (shipit--search-prs-with-encoded-query repo query-parts per-page limit sort-by)))

(defun shipit-pr-github--create-pr (config title body base head draft)
  "Create a new PR in REPO specified by CONFIG.
TITLE, BODY, BASE branch, HEAD branch.  When DRAFT is non-nil the PR
is created as a draft rather than ready for review."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/pulls" repo))
         (data `((title . ,title) (body . ,body)
                 (base . ,base) (head . ,head)
                 (draft . ,(if draft t :json-false)))))
    (shipit--debug-log "GitHub PR backend: creating PR in %s (draft=%s)"
                       repo (and draft t))
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

(defun shipit-pr-github--set-pr-draft (config number draft-p)
  "Set the draft state of PR NUMBER to DRAFT-P using CONFIG.
Draft state cannot be changed through the REST update endpoint;
GitHub only exposes it via the GraphQL mutations
convertPullRequestToDraft / markPullRequestReadyForReview, keyed
on the PR's node id.  Returns non-nil on success."
  (let* ((repo (plist-get config :repo))
         (node-id (shipit--get-pr-id repo number)))
    (shipit--debug-log "GitHub PR backend: set draft=%s for PR #%s in %s (node %s)"
                       draft-p number repo node-id)
    (when node-id
      (let* ((mutation
              (if draft-p
                  "mutation($id: ID!) { convertPullRequestToDraft(input: {pullRequestId: $id}) { pullRequest { isDraft } } }"
                "mutation($id: ID!) { markPullRequestReadyForReview(input: {pullRequestId: $id}) { pullRequest { isDraft } } }"))
             (response (shipit--graphql-request mutation `((id . ,node-id))))
             (errors (cdr (assq 'errors response))))
        (when errors
          (message "GitHub draft toggle failed: %s"
                   (or (cdr (assq 'message (car errors))) "GraphQL error")))
        (and (null errors) (cdr (assq 'data response)))))))

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

(defun shipit-pr-github--browse-stack-url (config number)
  "Return the browser URL of stack NUMBER in CONFIG\='s repository.
The API names no web address for a stack anywhere on its resources,
so this mirrors the API\='s stacks path under the web host."
  (format "https://github.com/%s/stacks/%s" (plist-get config :repo) number))

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

(defconst shipit-pr-github--force-push-query
  "query ForcePushes($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      timelineItems(last: 100, itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT]) {
        nodes {
          ... on HeadRefForcePushedEvent {
            beforeCommit { oid }
            afterCommit { oid }
          }
        }
      }
    }
  }
}"
  "GraphQL query for both heads of every force push on a pull request.
The REST timeline names only the new head, in `commit_id\='.  The head
it replaced, which GitHub\='s own page shows as the `from\=' half of
\"force-pushed from A to B\", is only in GraphQL.")

(defun shipit-pr-github--force-push-event-p (event)
  "Return non-nil when EVENT is a head-ref force push."
  (equal (cdr (assq 'event event)) "head_ref_force_pushed"))

(defun shipit-pr-github--force-push-pair (node)
  "Return (NEW-HEAD . OLD-HEAD) for GraphQL NODE, or nil.
GitHub reports a head it can no longer resolve as null, and a push
with only one end is of no use to a comparison."
  (let ((before (cdr (assq 'oid (cdr (assq 'beforeCommit node)))))
        (after (cdr (assq 'oid (cdr (assq 'afterCommit node))))))
    (when (and before after)
      (cons after before))))

(defun shipit-pr-github--force-push-pairs (response)
  "Return the (NEW-HEAD . OLD-HEAD) pairs in GraphQL RESPONSE."
  (let* ((data (cdr (assq 'data response)))
         (repository (cdr (assq 'repository data)))
         (pull-request (cdr (assq 'pullRequest repository)))
         (items (cdr (assq 'timelineItems pull-request)))
         (nodes (append (cdr (assq 'nodes items)) nil)))
    (delq nil (mapcar #'shipit-pr-github--force-push-pair nodes))))

(defun shipit-pr-github--pr-variables (config number)
  "Return the GraphQL variables that name PR NUMBER of the repository in CONFIG."
  (let ((parts (split-string (plist-get config :repo) "/")))
    `((owner . ,(car parts))
      (repo . ,(cadr parts))
      (number . ,number))))

(defun shipit-pr-github--fetch-force-pushes (config number)
  "Return the force pushes on PR NUMBER as (NEW-HEAD . OLD-HEAD) pairs.
Keyed by the new head because that is what the REST timeline event
carries in `commit_id\='."
  (shipit--debug-log "GitHub PR backend: fetching force pushes for PR #%s" number)
  (shipit-pr-github--force-push-pairs
   (shipit--graphql-request shipit-pr-github--force-push-query
                            (shipit-pr-github--pr-variables config number))))

(defun shipit-pr-github--fetch-force-pushes-async (config number callback)
  "Call CALLBACK with the force pushes on PR NUMBER, as pairs."
  (shipit--graphql-request
   shipit-pr-github--force-push-query
   (shipit-pr-github--pr-variables config number)
   (lambda (response)
     (funcall callback (shipit-pr-github--force-push-pairs response)))))

(defun shipit-pr-github--apply-force-push-pairs (events pairs)
  "Return EVENTS with both heads added to every force push among them.
PAIRS maps the new head to the head it replaced.  An event whose new
head is not in PAIRS is left alone, so a row that cannot be compared
still renders."
  (mapcar
   (lambda (event)
     (let ((before (and (shipit-pr-github--force-push-event-p event)
                        (cdr (assoc (cdr (assq 'commit_id event)) pairs)))))
       (if before
           (append `((shipit-force-push-before . ,before)
                     (shipit-force-push-after . ,(cdr (assq 'commit_id event))))
                   event)
         event)))
   events))

(defun shipit-pr-github--annotate-force-pushes (config number events)
  "Add both heads to the force pushes among EVENTS of PR NUMBER.
EVENTS comes back untouched when it holds no force push, so the extra
request is only made for the pull requests that were force-pushed."
  (if (not (seq-some #'shipit-pr-github--force-push-event-p events))
      events
    (shipit-pr-github--apply-force-push-pairs
     events (shipit-pr-github--fetch-force-pushes config number))))

(defun shipit-pr-github--annotate-force-pushes-async (config number events callback)
  "Call CALLBACK with EVENTS of PR NUMBER, force pushes carrying both heads.
The second request stays asynchronous too, so opening a
force-pushed pull request does not block on it."
  (if (not (seq-some #'shipit-pr-github--force-push-event-p events))
      (funcall callback events)
    (shipit-pr-github--fetch-force-pushes-async
     config number
     (lambda (pairs)
       (funcall callback (shipit-pr-github--apply-force-push-pairs events pairs))))))

;;; Edits of the description

(defconst shipit-pr-github--description-edits-query
  "query($owner: String!, $repo: String!, $number: Int!) {
  repository(owner: $owner, name: $repo) {
    pullRequest(number: $number) {
      userContentEdits(first: 50) {
        nodes { id editedAt diff editor { login avatarUrl } }
      }
    }
  }
}"
  "GraphQL query for the versions of the description of a pull request.
The REST timeline has no event for an edited description.  GraphQL
keeps each version, newest first, and `diff' holds its whole text, not
a diff.  The oldest is the text the pull request was opened with.")

(defun shipit-pr-github--description-edit-event (newer older)
  "The activity event of the edit that saved NEWER over OLDER.
Both are versions of a description from GraphQL."
  (let ((editor (cdr (assq 'editor newer))))
    `((event . "description_edited")
      (id . ,(cdr (assq 'id newer)))
      (created_at . ,(cdr (assq 'editedAt newer)))
      (actor . ((login . ,(cdr (assq 'login editor)))
                (avatar_url . ,(cdr (assq 'avatarUrl editor)))))
      (shipit-description-from . ,(or (cdr (assq 'diff older)) ""))
      (shipit-description-to . ,(or (cdr (assq 'diff newer)) "")))))

(defun shipit-pr-github--description-edit-events (response)
  "The edits of the description in GraphQL RESPONSE, as activity events.
Each goes from the version before it to its own.  A RESPONSE of nil,
from a request that failed, has none."
  (let* ((data (cdr (assq 'data response)))
         (repository (cdr (assq 'repository data)))
         (pull-request (cdr (assq 'pullRequest repository)))
         (edits (cdr (assq 'userContentEdits pull-request)))
         (nodes (append (cdr (assq 'nodes edits)) nil)))
    (cl-loop for (newer older) on nodes
             while older
             collect (shipit-pr-github--description-edit-event newer older))))

(defun shipit-pr-github--add-description-edits (config number events)
  "EVENTS of PR NUMBER in CONFIG, with the edits of its description after them."
  (append events
          (shipit-pr-github--description-edit-events
           (shipit--graphql-request shipit-pr-github--description-edits-query
                                    (shipit-pr-github--pr-variables config number)))))

(defun shipit-pr-github--add-description-edits-async (config number events callback)
  "Call CALLBACK with EVENTS of PR NUMBER, and the edits of its description.
It costs one request: the timeline cannot say whether the description
was ever edited."
  (shipit--graphql-request
   shipit-pr-github--description-edits-query
   (shipit-pr-github--pr-variables config number)
   (lambda (response)
     (funcall callback
              (append events (shipit-pr-github--description-edit-events response))))))

(defun shipit-pr-github--fetch-timeline (config number)
  "Fetch timeline events for PR NUMBER using CONFIG."
  (let ((repo (plist-get config :repo)))
    (shipit--debug-log "GitHub PR backend: fetching timeline for PR #%s" number)
    (shipit-pr-github--add-description-edits
     config number
     (shipit-pr-github--annotate-force-pushes
      config number (shipit--fetch-timeline-events repo number)))))

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
    (shipit--fetch-timeline-events-async
     repo number
     (lambda (events)
       (shipit-pr-github--annotate-force-pushes-async
        config number events
        (lambda (annotated)
          (shipit-pr-github--add-description-edits-async
           config number annotated callback)))))))

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

(defconst shipit-pr-github--bot-reviewers
  '(("Copilot" . "copilot-pull-request-reviewer[bot]"))
  "Requestable bot reviewers as (DISPLAY . LOGIN) pairs.
Requesting the copilot-pull-request-reviewer bot asks Copilot code
review for the PR, same as picking \"Copilot\" in the reviewers
menu on github.com.")

(defun shipit-pr-github--reviewer-bot-candidates (_config)
  "Return requestable bot reviewers as (DISPLAY . LOGIN) pairs."
  shipit-pr-github--bot-reviewers)

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

(defun shipit-pr-github--compare-endpoint (config base head)
  "The endpoint of the compare of BASE and HEAD in the repository of CONFIG."
  (format "/repos/%s/compare/%s...%s" (plist-get config :repo) base head))

(defun shipit-pr-github--fetch-compare (config base head)
  "Fetch compare data between BASE and HEAD commits using CONFIG."
  (shipit--debug-log "GitHub PR backend: fetching compare %s...%s in %s"
                     base head (plist-get config :repo))
  (shipit--api-request (shipit-pr-github--compare-endpoint config base head)))

;;; A compare with more files than the compare API lists

(defconst shipit-pr-github--compare-file-limit 300
  "The most changed files that the compare API lists.
GitHub documents this limit with the endpoint that compares two commits.")

(defconst shipit-pr-github--diff-statuses
  '(("new file mode " . "added")
    ("deleted file mode " . "removed")
    ("rename from " . "renamed")
    ("copy from " . "copied"))
  "The status of a file, by the line that tells it in its part of a diff.
A file with none of these lines is modified.")

(defun shipit-pr-github--diff-file-parts (diff)
  "Split DIFF, the text of a git diff, into the part of each file.
A part starts after `diff --git ', with the two paths."
  (split-string (concat "\n" diff) "\ndiff --git " t))

(defun shipit-pr-github--diff-header-value (header prefixes)
  "Return the rest of the first line of HEADER that starts with one of PREFIXES."
  (seq-some (lambda (line)
              (seq-some (lambda (prefix)
                          (when (string-prefix-p prefix line)
                            (substring line (length prefix))))
                        prefixes))
            header))

(defun shipit-pr-github--diff-unquote (path)
  "Return PATH as a diff writes it, without the quotes of an unusual name.
git puts a name with bytes outside ASCII in quotes, each such byte
as an octal escape, which reads as a Lisp string."
  (if (string-prefix-p "\"" path)
      (decode-coding-string (car (read-from-string path)) 'utf-8)
    path))

(defun shipit-pr-github--diff-side-path (value prefix)
  "Return the path that VALUE names without PREFIX, or nil for /dev/null.
VALUE is a side of the diff of a file, such as a/kitchen/cup.txt.  git
ends it with a tab when the name has a space, for the tools that apply
a patch."
  (let ((path (shipit-pr-github--diff-unquote (string-remove-suffix "\t" value))))
    (unless (equal path "/dev/null")
      (string-remove-prefix prefix path))))

(defun shipit-pr-github--diff-git-line-path (line)
  "Return the path of LINE, the paths after `diff --git '.
Only a file whose diff has no other line with its path needs this,
such as a binary file, and then both paths are the same."
  (if (string-prefix-p "\"" line)
      (string-remove-prefix "a/" (shipit-pr-github--diff-unquote line))
    (substring line 2 (+ 2 (/ (- (length line) (length "a/ b/")) 2)))))

(defun shipit-pr-github--diff-path (header moved side prefix)
  "Return one path of the file whose diff has HEADER, nil when there is none.
MOVED is the start of the line that names it in a rename or a copy,
SIDE the start of the line of its side of the diff, and PREFIX the
start of the path on that line."
  (let ((moved-path (shipit-pr-github--diff-header-value header (list moved)))
        (side-path (shipit-pr-github--diff-header-value header (list side))))
    (cond (moved-path (shipit-pr-github--diff-unquote moved-path))
          (side-path (shipit-pr-github--diff-side-path side-path prefix))
          (t (shipit-pr-github--diff-git-line-path (car header))))))

(defun shipit-pr-github--diff-status (header)
  "Return the status of the file whose diff has HEADER."
  (or (seq-some (lambda (entry)
                  (when (shipit-pr-github--diff-header-value header (list (car entry)))
                    (cdr entry)))
                shipit-pr-github--diff-statuses)
      "modified"))

(defun shipit-pr-github--patch-count (patch char)
  "Return how many lines of PATCH start with CHAR; 0 when PATCH is nil."
  (cl-count-if (lambda (line) (string-prefix-p (string char) line))
               (and patch (split-string patch "\n"))))

(defun shipit-pr-github--file-from-diff (part)
  "Return the entry of the file of PART as the compare API lists a file.
PART is the diff of one file, without its `diff --git '.  Its patch
starts at the first hunk, as the patch of the compare API does."
  (let* ((hunks (string-match "^@@" part))
         (header (split-string (substring part 0 hunks) "\n" t))
         (patch (and hunks (string-remove-suffix "\n" (substring part hunks))))
         (old (shipit-pr-github--diff-path header "rename from " "--- " "a/"))
         (new (shipit-pr-github--diff-path header "rename to " "+++ " "b/"))
         (status (shipit-pr-github--diff-status header)))
    `((filename . ,(or new old))
      (status . ,status)
      ,@(when (member status '("renamed" "copied")) `((previous_filename . ,old)))
      (additions . ,(shipit-pr-github--patch-count patch ?+))
      (deletions . ,(shipit-pr-github--patch-count patch ?-))
      ,@(when patch `((patch . ,patch))))))

(defun shipit-pr-github--fetch-compare-diff-files (config base head)
  "Fetch every file that differs between BASE and HEAD, using CONFIG.
They come from the compare as a diff, which has no limit of files."
  (mapcar #'shipit-pr-github--file-from-diff
          (shipit-pr-github--diff-file-parts
           (shipit--api-request-text
            (shipit-pr-github--compare-endpoint config base head)
            "application/vnd.github.diff"))))

(defun shipit-pr-github--with-diff-files (config base head compare)
  "Return COMPARE of BASE and HEAD with the files of its diff, using CONFIG.
When GitHub does not give the diff, as for a diff it finds too big,
COMPARE keeps the files that it lists, and a message says so."
  (condition-case err
      (cons (cons 'files (shipit-pr-github--fetch-compare-diff-files config base head))
            (assq-delete-all 'files (copy-alist compare)))
    (error
     (message "shipit: Showing the files GitHub lists, the first %d changed files, some with no diff: %s"
              shipit-pr-github--compare-file-limit (error-message-string err))
     compare)))

(defun shipit-pr-github--compare-lacks-files-p (compare)
  "Return non-nil when COMPARE leaves out a changed file or its patch.
The compare API lists 300 files at most.  It gives no patch for a file
whose diff is past a size budget, and none for a binary file either:
the two look the same."
  (let ((files (append (cdr (assq 'files compare)) nil)))
    (or (>= (length files) shipit-pr-github--compare-file-limit)
        (seq-some (lambda (file) (null (cdr (assq 'patch file)))) files))))

(defun shipit-pr-github--fetch-compare-with-all-files (config base head)
  "Fetch the compare of BASE and HEAD using CONFIG, with every changed file.
When the compare API leaves out a file or a patch, the files come from
the compare as a diff instead."
  (let ((compare (shipit-pr-github--fetch-compare config base head)))
    (if (shipit-pr-github--compare-lacks-files-p compare)
        (shipit-pr-github--with-diff-files config base head compare)
      compare)))

(defun shipit-pr-github--fetch-commit-comments (config sha)
  "Fetch commit comments for SHA using CONFIG.
Commit comments are the comments GitHub shows on commit pages and
on the compare view of branches without a PR.  Returns a list of
comment alists (id, user, body, path, position, line, created_at,
html_url)."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/commits/%s/comments" repo sha)))
    (shipit--debug-log "GitHub PR backend: fetching commit comments for %s in %s" sha repo)
    (append (shipit--api-request endpoint) nil)))

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

(defun shipit-pr-github--list-prs (_config repo &optional state per-page)
  "List PRs in REPO via the GitHub `/repos/OWNER/REPO/pulls' endpoint.
STATE defaults to \"all\", PER-PAGE to 100.  Returns a vector/list of
PR alists sorted by most-recent-update first.  One page only — callers
needing full history should paginate."
  (let ((endpoint (format "/repos/%s/pulls?state=%s&sort=updated&direction=desc&per_page=%d"
                          repo (or state "all") (or per-page 100))))
    (shipit--debug-log "GitHub PR backend: list PRs in %s" repo)
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
            (overlay-put ov 'shipit-pr-link (cons ref-num-int url-repo-copy))
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
   ;; Actions run: /owner/repo/actions/runs/RUN_ID (optional /job/JOB_ID,
   ;; optional ?query / #fragment).  GitHub appends ?pr=N when the run is
   ;; opened from a PR; capture it so the actions buffer keeps PR context.
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/actions/runs/\\([0-9]+\\)\\(?:/job/\\([0-9]+\\)\\)?/?\\(?:[?#].*\\)?\\'"
     url)
    (let ((result (list :type 'actions-run
                        :repo (match-string 1 url)
                        :run-id (match-string 2 url))))
      (when (match-string 3 url)
        (setq result (plist-put result :job-id (match-string 3 url))))
      (when (string-match "[?&]pr=\\([0-9]+\\)" url)
        (setq result (plist-put result :pr-number
                                (string-to-number (match-string 1 url)))))
      result))
   ;; Discussion: /owner/repo/discussions/N
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/discussions/\\([0-9]+\\)\\(?:[/?#].*\\)?\\'"
     url)
    (list :type 'discussion
          :repo (match-string 1 url)
          :number (string-to-number (match-string 2 url))))
   ;; Blob: /owner/repo/blob/REF/PATH (with optional #fragment)
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/blob/\\([^#]+\\)\\(?:#\\(.*\\)\\)?\\'"
     url)
    (let* ((repo (match-string 1 url))
           (ref-and-path (match-string 2 url))
           (fragment (match-string 3 url))
           (parts (split-string ref-and-path "/"))
           (ref (car parts))
           (path (mapconcat #'identity (cdr parts) "/")))
      (list :type 'blob
            :repo repo
            :ref ref
            :path path
            :fragment fragment)))
   ;; Releases: /owner/repo/releases (optionally /tag/TAG)
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/releases\\(?:/tag/\\([^/?#]+\\)\\)?/?\\(?:[?#].*\\)?\\'"
     url)
    (list :type 'releases
          :repo (match-string 1 url)
          :tag (match-string 2 url)))
   ;; Tags: /owner/repo/tags
   ((string-match
     "\\`https?://github\\.com/\\([^/]+/[^/]+\\)/tags/?\\(?:[?#].*\\)?\\'"
     url)
    (list :type 'tags
          :repo (match-string 1 url)))
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

(defun shipit-pr-github--browse-compare-url (config base head)
  "Return browser URL for the BASE...HEAD compare view using CONFIG."
  (format "https://github.com/%s/compare/%s...%s"
          (plist-get config :repo) base head))

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

;;; Feed
;; The events that the user receives: what the people that the user
;; follows and the repositories that the user watches do.

(defvar shipit-pr-github--login nil
  "The login of the user, once GitHub has said it.")

(defun shipit-pr-github--login ()
  "The login of the user, asked of GitHub the first time."
  (or shipit-pr-github--login
      (setq shipit-pr-github--login (shipit-pr-github--get-current-username nil))
      (error "GitHub did not say who the user is")))

(defconst shipit-pr-github--feed-kinds
  '(("WatchEvent" . "star")
    ("ForkEvent" . "fork")
    ("CreateEvent" . "create")
    ("ReleaseEvent" . "release")
    ("PublicEvent" . "public"))
  "The events of the feed that are news, and the kind of entry that each is.
A `WatchEvent' is a star.")

(defun shipit-pr-github--event-field (event &rest keys)
  "What EVENT holds under KEYS, one inside the other."
  (dolist (key keys event)
    (setq event (cdr (assq key event)))))

(defun shipit-pr-github--feed-event-news-p (event)
  "Whether EVENT of the feed is news.
A new branch or tag is none, and neither is a release that was only
drafted or changed."
  (pcase (cdr (assq 'type event))
    ("CreateEvent"
     (equal "repository" (shipit-pr-github--event-field event 'payload 'ref_type)))
    ("ReleaseEvent"
     (equal "published" (shipit-pr-github--event-field event 'payload 'action)))
    (type (assoc type shipit-pr-github--feed-kinds))))

(defun shipit-pr-github--own-repo-p (repo)
  "Whether REPO, as owner/name, belongs to the user."
  (string-prefix-p (concat (shipit-pr-github--login) "/") repo))

(defun shipit-pr-github--feed-entry (event)
  "EVENT of the feed as an entry of a feed.
The detail of a release is its tag, and that of a fork is the fork."
  (let ((repo (shipit-pr-github--event-field event 'repo 'name))
        (release (shipit-pr-github--event-field event 'payload 'release)))
    `((id . ,(cdr (assq 'id event)))
      (own . ,(shipit-pr-github--own-repo-p repo))
      (kind . ,(cdr (assoc (cdr (assq 'type event)) shipit-pr-github--feed-kinds)))
      (actor . ,(shipit-pr-github--event-field event 'actor 'login))
      (repo . ,repo)
      (detail . ,(or (cdr (assq 'tag_name release))
                     (shipit-pr-github--event-field event 'payload 'forkee 'full_name)))
      (title . ,(cdr (assq 'name release)))
      (url . ,(or (cdr (assq 'html_url release))
                  (concat "https://github.com/" repo)))
      (time . ,(cdr (assq 'created_at event))))))

(defun shipit-pr-github--newer-entry-p (entry other)
  "Whether ENTRY of a feed is newer than OTHER."
  (string> (cdr (assq 'time entry)) (cdr (assq 'time other))))

(defun shipit-pr-github--same-event-p (event other)
  "Whether EVENT and OTHER are one event, which two feeds can both hold."
  (equal (cdr (assq 'id event)) (cdr (assq 'id other))))

(defun shipit-pr-github--feed-entries (events)
  "The entries of a feed with EVENTS, the newest first.
An event that two feeds hold gives one entry.  GitHub gives the events
of a feed in no order of time when the feed mixes the events of many
repositories, so they are put in order here."
  (sort (mapcar #'shipit-pr-github--feed-entry
                (seq-uniq (seq-filter #'shipit-pr-github--feed-event-news-p events)
                          #'shipit-pr-github--same-event-p))
        #'shipit-pr-github--newer-entry-p))

(defconst shipit-pr-github--feed-endpoints
  '("/users/%s/received_events" "/users/%s/received_events/public")
  "The feeds of GitHub that are read, with %s for the login of the user.
The first holds the events of every repository that the user can see.
The pushes and pull requests of a busy private repository fill it in
some hours, and what the people that the user follows do falls out of
it.  The second holds the public events only, and reaches back weeks.")

(defconst shipit-pr-github--feed-pages 3
  "How many pages GitHub gives of a feed: it keeps 300 events, 100 a page.
It refuses a request for a later page.")

(defun shipit-pr-github--feed-requests ()
  "What to ask GitHub for the feeds in full: a list of (ENDPOINT . PAGE)."
  (cl-loop for endpoint in shipit-pr-github--feed-endpoints
           nconc (cl-loop for page from 1 to shipit-pr-github--feed-pages
                          collect (cons (format endpoint (shipit-pr-github--login))
                                        page))))

(defun shipit-pr-github--feed-page-url (request)
  "The address of REQUEST, an (ENDPOINT . PAGE) of a feed."
  (format "%s%s?per_page=100&page=%d" shipit-api-url (car request) (cdr request)))

(defun shipit-pr-github--feed-headers ()
  "The headers of a request for a page of a feed.
url.el keeps a copy of each answer on disk, and then asks only for what
changed since.  GitHub answers that with no page at all when nothing
did.  With `Pragma: no-cache' url.el asks for the page itself."
  (list (or (shipit--get-auth-header) (error "GitHub token not found"))
        '("Accept" . "application/vnd.github+json")
        '("Pragma" . "no-cache")))

(defun shipit-pr-github--fetch-feed-page (request callback)
  "Call CALLBACK with the events of REQUEST, an (ENDPOINT . PAGE) of a feed.
A page that GitHub refuses gives no event, and is logged.  The answer
is not kept for a later request, as the answers of most requests are: a
page of a feed is some hundred kilobytes, and changes with each event."
  (shipit--url-retrieve-async
   (shipit-pr-github--feed-page-url request) "GET" (shipit-pr-github--feed-headers) nil
   callback
   (lambda (refusal)
     (shipit--debug-log "GitHub gave no page %d of %s: %s"
                        (cdr request) (car request) refusal)
     (funcall callback nil))))

(defun shipit-pr-github--fetch-feed-async (_config callback)
  "Call CALLBACK with the entries of the feed of the user, the newest first.
GitHub hands an event out some minutes to some hours after it happened."
  (let* ((requests (shipit-pr-github--feed-requests))
         (waiting (length requests))
         (events nil))
    (dolist (request requests)
      (shipit-pr-github--fetch-feed-page
       request
       (lambda (page)
         (setq events (append page events))
         (when (zerop (cl-decf waiting))
           (funcall callback (shipit-pr-github--feed-entries events))))))))

;;; Branches

(defconst shipit-pr-github--commit-fields
  "target {
          ... on Commit {
            oid
            committedDate
            messageHeadline
            author { name user { login } }
          }
        }"
  "The fields of the last commit of a branch, in a query of branches.")

(defconst shipit-pr-github--pr-fields
  "number title state isDraft isCrossRepository headRefName"
  "The fields of a pull request of a branch, in a query of branches.")

(defconst shipit-pr-github--branches-query
  (concat "query($owner: String!, $name: String!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    defaultBranchRef { name }
    refs(refPrefix: \"refs/heads/\", first: 100, after: $cursor) {
      totalCount
      pageInfo { hasNextPage endCursor }
      nodes {
        name
        " shipit-pr-github--commit-fields "
      }
    }
  }
}")
  "The query of a page of the branches of a repository, with their last commit.")

(defconst shipit-pr-github--branch-prs-query
  (concat "query($owner: String!, $name: String!, $states: [PullRequestState!], $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequests(states: $states, first: 100, after: $cursor,
                 orderBy: {field: UPDATED_AT, direction: DESC}) {
      pageInfo { hasNextPage endCursor }
      nodes { " shipit-pr-github--pr-fields " }
    }
  }
}")
  "The query of a page of the pull requests of a repository in some states.
The pull request that was updated last comes first.")

(defconst shipit-pr-github--parallel-pages 8
  "How many pages of branches are asked for at the same time.
GitHub refuses more than 100 requests of a user at the same time.")

(defun shipit-pr-github--repository-page (query repo variables callback)
  "Call CALLBACK with what GitHub says of REPO for QUERY with VARIABLES.
CALLBACK gets nil when GitHub refuses the query or knows no such
repository."
  (pcase-let ((`(,owner ,name) (split-string repo "/")))
    (shipit--graphql-request
     query `((owner . ,owner) (name . ,name) ,@variables)
     (lambda (response)
       (funcall callback (let-alist response .data.repository))))))

(defun shipit-pr-github--collect-pages (fetch-page cursor nodes callback)
  "Add the nodes of the page at CURSOR and of the pages after it to NODES.
FETCH-PAGE is a function of a cursor and of a function that it calls
with the connection of that page, or with nil when GitHub refused it.
CALLBACK gets all the nodes, or nil when a page was refused."
  (funcall fetch-page cursor
           (lambda (connection)
             (if (null connection)
                 (funcall callback nil)
               (let-alist connection
                 (let ((all (append nodes .nodes)))
                   (if (eq .pageInfo.hasNextPage t)
                       (shipit-pr-github--collect-pages
                        fetch-page .pageInfo.endCursor all callback)
                     (funcall callback all))))))))

(defun shipit-pr-github--run-limited (tasks limit callback)
  "Run TASKS, at most LIMIT at the same time, and call CALLBACK with results.
A task is a function of a function that it calls with its result.
CALLBACK gets the results in the order of TASKS, once the last task
gave its result."
  (let* ((tasks (vconcat tasks))
         (results (make-vector (length tasks) nil))
         (next 0)
         (waiting (length tasks)))
    (cl-labels ((start ()
                  (when (< next (length tasks))
                    (let ((index next))
                      (cl-incf next)
                      (funcall (aref tasks index)
                               (lambda (result)
                                 (aset results index result)
                                 (if (zerop (cl-decf waiting))
                                     (funcall callback (append results nil))
                                   (start))))))))
      (if (zerop waiting)
          (funcall callback nil)
        (dotimes (_ (min limit waiting))
          (start))))))

(defun shipit-pr-github--refs-page (repo cursor callback)
  "Call CALLBACK with the refs of REPO in the page at CURSOR, or nil."
  (shipit-pr-github--repository-page
   shipit-pr-github--branches-query repo `((cursor . ,cursor))
   (lambda (repository) (funcall callback (alist-get 'refs repository)))))

(defun shipit-pr-github--count-cursor-p (cursor count)
  "Whether CURSOR is COUNT, the number of branches before it, in base64.
GitHub writes a cursor of branches so, but does not promise it, so it is
checked before pages are asked for by their count."
  (equal (ignore-errors (base64-decode-string cursor)) (number-to-string count)))

(defun shipit-pr-github--count-cursors (size total)
  "The cursors of the pages of SIZE branches after the first, of TOTAL."
  (cl-loop for count from size below total by size
           collect (base64-encode-string (number-to-string count))))

(defun shipit-pr-github--refs-by-count (repo first callback)
  "Ask for the pages of REPO after FIRST by their count, some at a time.
CALLBACK gets the refs of FIRST and of those pages, or nil when a page
was refused."
  (let-alist first
    (shipit-pr-github--run-limited
     (mapcar (lambda (cursor) (apply-partially #'shipit-pr-github--refs-page repo cursor))
             (shipit-pr-github--count-cursors (length .nodes) .totalCount))
     shipit-pr-github--parallel-pages
     (lambda (pages)
       (funcall callback
                (unless (memq nil pages)
                  (apply #'append .nodes
                         (mapcar (lambda (page) (alist-get 'nodes page)) pages))))))))

(defun shipit-pr-github--rest-of-refs (repo first callback)
  "Call CALLBACK with the refs of FIRST, the first page of REPO, and the rest.
Pages are asked for by their count, some at the same time, when the
cursor of FIRST is the count of its refs.  Else they are asked for one
after the other, which takes some seconds for each 100 branches.
CALLBACK gets nil when a page was refused."
  (let-alist first
    (cond ((not (eq .pageInfo.hasNextPage t))
           (funcall callback .nodes))
          ((shipit-pr-github--count-cursor-p .pageInfo.endCursor (length .nodes))
           (shipit-pr-github--refs-by-count repo first callback))
          (t
           (shipit-pr-github--collect-pages
            (apply-partially #'shipit-pr-github--refs-page repo)
            .pageInfo.endCursor .nodes callback)))))

(defun shipit-pr-github--fetch-refs (repo callback)
  "Call CALLBACK with the default branch and the refs of REPO.
They come as (DEFAULT . REFS), or as nil when GitHub refuses a page or
knows no such repository."
  (shipit-pr-github--repository-page
   shipit-pr-github--branches-query repo '((cursor))
   (lambda (repository)
     (if (null repository)
         (funcall callback nil)
       (let-alist repository
         (shipit-pr-github--rest-of-refs
          repo .refs
          (lambda (refs)
            (funcall callback (and refs (cons .defaultBranchRef.name refs))))))))))

(defun shipit-pr-github--prs-page (repo states cursor callback)
  "Call CALLBACK with the pull requests of REPO in STATES at CURSOR, or nil.
With STATES nil they are in any state."
  (shipit-pr-github--repository-page
   shipit-pr-github--branch-prs-query repo `((states . ,states) (cursor . ,cursor))
   (lambda (repository) (funcall callback (alist-get 'pullRequests repository)))))

(defun shipit-pr-github--fetch-branch-prs (repo callback)
  "Call CALLBACK with the pull requests of REPO that its branches show.
They are the open ones, and the 100 that were updated last.  To ask for
the pull requests of each branch takes longer than to ask for the
branches, so an older pull request that was merged or closed is not
shown."
  (shipit-pr-github--collect-pages
   (apply-partially #'shipit-pr-github--prs-page repo ["OPEN"]) nil nil
   (lambda (open)
     (shipit-pr-github--prs-page
      repo nil nil
      (lambda (recent)
        (funcall callback (append open (alist-get 'nodes recent))))))))

(defun shipit-pr-github--own-pr-p (pr)
  "Whether the head of PR is a branch of its own repository.
The head of a pull request from a fork is a branch of the fork, which
can have the name of a branch of the repository."
  (not (eq (alist-get 'isCrossRepository pr) t)))

(defun shipit-pr-github--pr-before-p (pr other)
  "Whether PR of a branch is shown before OTHER: an open one, else the newer."
  (let ((open (equal (alist-get 'state pr) "OPEN")))
    (if (eq open (equal (alist-get 'state other) "OPEN"))
        (> (alist-get 'number pr) (alist-get 'number other))
      open)))

(defun shipit-pr-github--prs-by-branch (prs)
  "A table of the pull request of each branch, by name, from PRS.
Of two pull requests of one branch, the one that is shown first is
kept."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pr (seq-filter #'shipit-pr-github--own-pr-p prs) table)
      (let ((kept (gethash (alist-get 'headRefName pr) table)))
        (when (or (null kept) (shipit-pr-github--pr-before-p pr kept))
          (puthash (alist-get 'headRefName pr) pr table))))))

(defun shipit-pr-github--branch-pr (pr)
  "PR, as GraphQL gives it, as the pull request of a branch, or nil.
Its state is open, draft, merged or closed.  GitHub keeps a pull request
that was closed as a draft as a draft, but it is closed."
  (when pr
    (let-alist pr
      `((number . ,.number)
        (title . ,.title)
        (state . ,(if (and (equal .state "OPEN") (eq .isDraft t))
                      "draft"
                    (downcase .state)))))))

(defun shipit-pr-github--branch (repo default prs ref)
  "REF, a branch of REPO as GraphQL gives it, as a branch of a backend.
DEFAULT is the name of the default branch of REPO, and PRS the table of
`shipit-pr-github--prs-by-branch'.  Who made the last commit is the
login of that person, or the name in the commit when no user of GitHub
has its address."
  (let-alist ref
    `((name . ,.name)
      (sha . ,.target.oid)
      (time . ,.target.committedDate)
      (subject . ,.target.messageHeadline)
      (author . ,(or .target.author.user.login .target.author.name))
      (own . ,(equal .target.author.user.login (shipit-pr-github--login)))
      (default . ,(equal .name default))
      (base . ,default)
      (pr . ,(shipit-pr-github--branch-pr (gethash .name prs)))
      (url . ,(format "https://github.com/%s/tree/%s" repo .name)))))

(defun shipit-pr-github--newest-first (branches)
  "BRANCHES, the one with the newest last commit first.
GitHub orders branches by the date of their commit only for tags."
  (sort branches (lambda (branch other)
                   (string> (alist-get 'time branch) (alist-get 'time other)))))

(defun shipit-pr-github--branches (repo refs prs)
  "The branches of REPO from REFS, as (DEFAULT . REFS), and from PRS."
  (let ((table (shipit-pr-github--prs-by-branch prs)))
    (shipit-pr-github--newest-first
     (mapcar (apply-partially #'shipit-pr-github--branch repo (car refs) table)
             (cdr refs)))))

(defun shipit-pr-github--fetch-branches-async (config callback)
  "Call CALLBACK with the branches of the repository of CONFIG.
The branch with the newest last commit comes first.  The branches and
their pull requests are asked for at the same time.  CALLBACK gets nil
when GitHub refuses a page of branches or knows no such repository:
each repository has a default branch, so no branch at all says that
nothing is known.  Pull requests that GitHub refuses leave the branches
without one."
  (let ((repo (plist-get config :repo))
        (refs 'waiting)
        (prs 'waiting))
    (cl-flet ((join ()
                (unless (or (eq refs 'waiting) (eq prs 'waiting))
                  (funcall callback
                           (and refs (shipit-pr-github--branches repo refs prs))))))
      (shipit-pr-github--fetch-refs repo (lambda (result) (setq refs result) (join)))
      (shipit-pr-github--fetch-branch-prs repo (lambda (result) (setq prs result) (join))))))

(defun shipit-pr-github--branch-names-headers (etag)
  "The headers of a request of the names of branches, with ETAG if there is one."
  (delq nil (list (shipit--get-auth-header)
                  '("Accept" . "application/vnd.github+json")
                  (and etag (cons "If-None-Match" etag)))))

(defun shipit-pr-github--branch-names (status refs headers)
  "What GitHub's answer of STATUS, REFS and HEADERS says of the names of branches.
It is `unchanged' for 304 Not Modified, (ETAG . NAMES) for a success
with names, and nil for anything else."
  (pcase status
    (304 'unchanged)
    ;; Each repository has a default branch, so a success without a name
    ;; was cut short.  url.el gives status 200 and no body when the
    ;; connection closes after the headers.
    ((guard (null refs)) nil)
    (200 (cons (cdr (assoc-string "ETag" headers t))
               (mapcar (lambda (ref) (string-remove-prefix "refs/heads/" (alist-get 'ref ref)))
                       refs)))))

(defun shipit-pr-github--fetch-branch-names-async (config etag callback)
  "Call CALLBACK with the names of the branches of the repository of CONFIG.
ETAG is the one that came with the names before, or nil.  CALLBACK gets
`unchanged' when the branches did not change since then, else (ETAG
. NAMES), or nil when GitHub refuses or the answer is cut short.  GitHub
gives all names in one answer, and an answer that nothing changed does
not count against its rate limit."
  (shipit--url-retrieve-async-with-status
   (format "%s/repos/%s/git/matching-refs/heads/"
           (or shipit-api-url "https://api.github.com") (plist-get config :repo))
   "GET" (shipit-pr-github--branch-names-headers etag) nil
   (lambda (status refs headers)
     (funcall callback (shipit-pr-github--branch-names status refs headers)))
   (lambda (_error) (funcall callback nil))))

(defconst shipit-pr-github--names-per-query 100
  "How many branches one query asks for by name.")

(defun shipit-pr-github--named-refs-query (count)
  "The query of COUNT branches, each named by a variable $b0, $b1 and so on.
Each comes with its open pull requests."
  ;; GitHub gives a branch also the pull requests from a fork whose branch
  ;; has the same name, so one pull request could hide the own one.
  (let ((indices (number-sequence 0 (1- count))))
    (format "query($owner: String!, $name: String!, %s) {
  repository(owner: $owner, name: $name) {
    defaultBranchRef { name }
%s
  }
}"
            (mapconcat (lambda (i) (format "$b%d: String!" i)) indices ", ")
            (mapconcat (lambda (i)
                         (format "    b%d: ref(qualifiedName: $b%d) {
      name %s
      associatedPullRequests(states: OPEN, first: 10) { nodes { %s } }
    }"
                                 i i shipit-pr-github--commit-fields
                                 shipit-pr-github--pr-fields))
                       indices "\n"))))

(defun shipit-pr-github--named-refs-variables (names)
  "The variables of the query of the branches NAMES, as (bN . refs/heads/NAME)."
  (cl-loop for name in names
           for index from 0
           collect (cons (intern (format "b%d" index)) (concat "refs/heads/" name))))

(defun shipit-pr-github--named-refs (repo names callback)
  "Call CALLBACK with the default branch and the refs of REPO named NAMES.
They come as (DEFAULT . REFS), without a name that is not there, or as
nil when GitHub refuses."
  (let ((variables (shipit-pr-github--named-refs-variables names)))
    (shipit-pr-github--repository-page
     (shipit-pr-github--named-refs-query (length names)) repo variables
     (lambda (repository)
       (funcall callback
                (when repository
                  (cons (let-alist repository .defaultBranchRef.name)
                        (delq nil (mapcar (lambda (variable)
                                            (alist-get (car variable) repository))
                                          variables)))))))))

(defun shipit-pr-github--refs-prs (refs)
  "The pull requests that come with REFS, the branches asked for by name."
  (mapcan (lambda (ref) (append (let-alist ref .associatedPullRequests.nodes) nil))
          refs))

(defun shipit-pr-github--fetch-named-branches-async (config names callback)
  "Call CALLBACK with the branches of the repository of CONFIG named NAMES.
They are as `shipit-pr-github--fetch-branches-async' gives them, but
with an open pull request only.  A name that is not there is left out.
CALLBACK gets nil when GitHub refuses."
  (let ((repo (plist-get config :repo)))
    (shipit-pr-github--run-limited
     (mapcar (lambda (some) (apply-partially #'shipit-pr-github--named-refs repo some))
             (seq-split names shipit-pr-github--names-per-query))
     shipit-pr-github--parallel-pages
     (lambda (parts)
       (funcall callback
                (unless (memq nil parts)
                  (let ((refs (apply #'append (mapcar #'cdr parts))))
                    (shipit-pr-github--branches
                     repo (cons (caar parts) refs) (shipit-pr-github--refs-prs refs)))))))))

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

(defcustom shipit-pr-github-repo-status-cache-ttl 300
  "Seconds to cache `--get-repo-subscription' / `--get-repo-starred' results.
Both endpoints fork sync HTTP on every PR buffer render and
return state that rarely changes (user actions on GitHub, not
PR data).  At 300s (5 min) the cache is fresh enough to pick
up legitimate external changes within a few buffer reloads
while saving ~45ms of round-trip time on every buffer render
in the meantime.  Invalidated automatically when the user
toggles via `--set-repo-subscription' / `--set-repo-starred'."
  :type 'number
  :group 'shipit)

(defvar shipit-pr-github--repo-status-cache (make-hash-table :test 'equal)
  "Hash of (REPO . KIND) -> (TIMESTAMP . VALUE).
KIND is `:subscription' or `:starred'.  Cleared by
`shipit-clear-all-caches'.")

(defun shipit-pr-github--repo-status-cache-key (repo kind)
  "Return the cache key for REPO + KIND."
  (cons repo kind))

(defun shipit-pr-github--repo-status-cache-get (repo kind)
  "Return cached VALUE for REPO + KIND if fresh, else `shipit--miss'."
  (let* ((entry (gethash (shipit-pr-github--repo-status-cache-key repo kind)
                         shipit-pr-github--repo-status-cache))
         (now (float-time)))
    (cond
     ((and entry
           (< (- now (car entry))
              (max 0 shipit-pr-github-repo-status-cache-ttl)))
      (cdr entry))
     (t 'shipit--miss))))

(defun shipit-pr-github--repo-status-cache-put (repo kind value)
  "Store VALUE in the repo-status cache for REPO + KIND."
  (puthash (shipit-pr-github--repo-status-cache-key repo kind)
           (cons (float-time) value)
           shipit-pr-github--repo-status-cache))

(defun shipit-pr-github--repo-status-cache-invalidate (repo &optional kind)
  "Drop cached repo-status entries for REPO.  When KIND given, only that one."
  (cond
   (kind
    (remhash (shipit-pr-github--repo-status-cache-key repo kind)
             shipit-pr-github--repo-status-cache))
   (t
    (dolist (k '(:subscription :starred))
      (remhash (shipit-pr-github--repo-status-cache-key repo k)
               shipit-pr-github--repo-status-cache)))))

(defun shipit-pr-github--get-repo-subscription (config)
  "Get subscription for repo in CONFIG.
Returns subscription alist or nil if not subscribed (404).
Cached for `shipit-pr-github-repo-status-cache-ttl' seconds; the
cache is invalidated on `--set-repo-subscription'."
  (let* ((repo (plist-get config :repo))
         (cached (shipit-pr-github--repo-status-cache-get repo :subscription)))
    (cond
     ((not (eq cached 'shipit--miss)) cached)
     (t
      (let* ((endpoint (format "/repos/%s/subscription" repo))
             (result (progn
                       (shipit--debug-log
                        "GitHub PR backend: fetching subscription for %s" repo)
                       (shipit--api-request endpoint))))
        (shipit-pr-github--repo-status-cache-put repo :subscription result)
        result)))))

(defun shipit-pr-github--set-repo-subscription (config state)
  "Set subscription STATE for repo in CONFIG.
STATE is \"watching\", \"participating\", or \"ignoring\"."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/subscription" repo)))
    (shipit--debug-log "GitHub PR backend: setting subscription for %s to %s" repo state)
    (shipit-pr-github--repo-status-cache-invalidate repo :subscription)
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

(defun shipit-pr-github--thread-variables (repo number)
  "Return GraphQL variables alist for thread NUMBER in REPO.
Signals an error if REPO is not of the form \"owner/name\"."
  (let* ((parts (split-string repo "/"))
         (owner (car parts))
         (name (cadr parts)))
    (unless (and owner name (> (length name) 0))
      (error "Invalid repo %S: expected \"owner/name\"" repo))
    `((owner . ,owner)
      (name . ,name)
      (number . ,number))))

(defun shipit-pr-github--thread-graphql-field (type)
  "Return the GraphQL repository field name for thread TYPE.
TYPE is \"pr\", \"issue\", or \"discussion\"."
  (pcase type
    ("pr" "pullRequest")
    ("issue" "issue")
    ("discussion" "discussion")
    (_ (error "Unknown thread type: %S" type))))

(defun shipit-pr-github--fetch-thread-node-id (repo type number)
  "Fetch the GraphQL node ID for thread NUMBER in REPO of TYPE.
TYPE is \"pr\", \"issue\", or \"discussion\"."
  (let* ((field (shipit-pr-github--thread-graphql-field type))
         (variables (shipit-pr-github--thread-variables repo number))
         (query (format "query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    %s(number: $number) { id }
  }
}" field))
         (result (shipit--graphql-query query variables)))
    (cdr (assq 'id (cdr (assq (intern field)
                              (cdr (assq 'repository result))))))))

(defun shipit-pr-github--get-thread-subscription (_config repo type number)
  "Get thread subscription state for TYPE NUMBER in REPO via GraphQL.
TYPE is \"pr\", \"issue\", or \"discussion\".
Returns \"subscribed\", \"unsubscribed\", or \"ignored\"."
  (let* ((field (shipit-pr-github--thread-graphql-field type))
         (variables (shipit-pr-github--thread-variables repo number))
         (query (format "query($owner: String!, $name: String!, $number: Int!) {
  repository(owner: $owner, name: $name) {
    %s(number: $number) { viewerSubscription id }
  }
}" field))
         (result (shipit--graphql-query query variables))
         (thread (cdr (assq (intern field)
                            (cdr (assq 'repository result)))))
         (state (cdr (assq 'viewerSubscription thread))))
    (shipit--debug-log "GitHub: %s %s#%d subscription: %s"
                       type repo number state)
    (pcase state
      ("SUBSCRIBED" "subscribed")
      ("IGNORED" "ignored")
      (_ "unsubscribed"))))

(defun shipit-pr-github--set-thread-subscription (_config repo type number subscribed)
  "Set subscription for TYPE NUMBER in REPO via GraphQL mutation.
TYPE is \"pr\", \"issue\", or \"discussion\".
SUBSCRIBED is t to subscribe, nil to unsubscribe."
  (let ((node-id (shipit-pr-github--fetch-thread-node-id repo type number)))
    (unless node-id
      (error "Cannot find %s %s#%d" type repo number))
    (let* ((state (if subscribed "SUBSCRIBED" "UNSUBSCRIBED"))
           (mutation "mutation($id: ID!, $state: SubscriptionState!) {
  updateSubscription(input: {subscribableId: $id, state: $state}) {
    subscribable { viewerSubscription }
  }
}")
           (variables `((id . ,node-id)
                        (state . ,state))))
      (shipit--debug-log "GitHub: %s %s %s#%d"
                         (if subscribed "subscribing to" "unsubscribing from")
                         type repo number)
      (shipit--graphql-query mutation variables)
      t)))

(defun shipit-pr-github--get-repo-starred (config)
  "Check if repo in CONFIG is starred by the current user.
Returns t if starred (HTTP 204), nil if not (HTTP 404).
Cached for `shipit-pr-github-repo-status-cache-ttl' seconds; the
cache is invalidated on `--set-repo-starred'."
  (let* ((repo (plist-get config :repo))
         (cached (shipit-pr-github--repo-status-cache-get repo :starred)))
    (cond
     ((not (eq cached 'shipit--miss)) cached)
     (t
      (let* ((url (concat shipit-api-url (format "/user/starred/%s" repo)))
             (headers (list (shipit--get-auth-header)
                            (cons "Accept" "application/vnd.github.v3+json")))
             (result (condition-case nil
                         (let* ((res (shipit--url-retrieve-sync
                                      url "GET" headers nil))
                                (status (cdr res)))
                           (eq status 204))
                       (error nil))))
        (shipit--debug-log "GitHub PR backend: checking star status for %s" repo)
        (shipit-pr-github--repo-status-cache-put repo :starred result)
        result)))))

(defun shipit-pr-github--set-repo-starred (config starred)
  "Star or unstar repo in CONFIG.
STARRED is t to star, nil to unstar."
  (let* ((repo (plist-get config :repo))
         (url (concat shipit-api-url (format "/user/starred/%s" repo)))
         (method (if starred "PUT" "DELETE"))
         (headers (list (shipit--get-auth-header)
                        (cons "Accept" "application/vnd.github.v3+json"))))
    (shipit--debug-log "GitHub PR backend: %s star for %s"
                       (if starred "setting" "removing") repo)
    (shipit-pr-github--repo-status-cache-invalidate repo :starred)
    (shipit--url-retrieve-sync url method headers nil)))

(defconst shipit-pr-github--watched-repos-graphql
  "query($cursor: String) {
  viewer {
    watching(first: 100, after: $cursor) {
      totalCount
      pageInfo { hasNextPage endCursor }
      nodes {
        nameWithOwner
        description
        owner { login }
        viewerSubscription
        viewerHasStarred
      }
    }
  }
}"
  "GraphQL query to fetch watched/ignored repos with star status.")

(defun shipit-pr-github--normalize-watched-repo (node)
  "Normalize a GraphQL repo NODE to the standard alist format."
  (let* ((name (cdr (assq  'nameWithOwner node)))
         (owner-obj (cdr (assq  'owner node)))
         (owner (when owner-obj (cdr (assq  'login owner-obj))))
         (subscription (cdr (assq  'viewerSubscription node)))
         (starred (eq (cdr (assq  'viewerHasStarred node)) t)))
    `((full_name . ,name)
      (owner_name . ,owner)
      (description . ,(cdr (assq  'description node)))
      (subscription . ,subscription)
      (starred . ,starred))))

(defun shipit-pr-github--fetch-viewer-repos (query connection)
  "Fetch every repository of CONNECTION, a field of the viewer, with QUERY.
QUERY takes a cursor, and gives the page info and the nodes of
CONNECTION.  Returns list of normalized repo alists with subscription
and star status."
  (let ((all-repos nil)
        (cursor nil)
        (has-more t))
    (while has-more
      (let* ((result (shipit--graphql-query query `((cursor . ,cursor))))
             (page (cdr (assq connection (cdr (assq 'viewer result)))))
             (page-info (cdr (assq 'pageInfo page))))
        (dolist (node (append (cdr (assq 'nodes page)) nil))
          (push (shipit-pr-github--normalize-watched-repo node) all-repos))
        (setq has-more (eq (cdr (assq 'hasNextPage page-info)) t))
        (setq cursor (cdr (assq 'endCursor page-info)))))
    (nreverse all-repos)))

(defun shipit-pr-github--fetch-watched-repos (_config)
  "Fetch all watched/ignored repos via GraphQL with pagination.
Returns list of normalized repo alists with subscription and star status."
  (shipit--debug-log "GitHub PR backend: fetching watched repos via GraphQL")
  (shipit-pr-github--fetch-viewer-repos shipit-pr-github--watched-repos-graphql
                                        'watching))

(defconst shipit-pr-github--own-repos-graphql
  "query($cursor: String) {
  viewer {
    repositories(first: 100, after: $cursor, ownerAffiliations: OWNER) {
      pageInfo { hasNextPage endCursor }
      nodes {
        nameWithOwner
        description
        owner { login }
        viewerSubscription
        viewerHasStarred
      }
    }
  }
}"
  "GraphQL query to fetch the repositories that the user owns.")

(defun shipit-pr-github--fetch-own-unwatched-repos (_config)
  "Fetch the repositories that the user owns and neither watches nor ignores.
GitHub tells the owner of such a repository of a new issue only when
the owner takes part in it.  Returns list of normalized repo alists."
  (shipit--debug-log "GitHub PR backend: fetching own unwatched repos via GraphQL")
  (seq-filter (lambda (repo) (equal "UNSUBSCRIBED" (cdr (assq 'subscription repo))))
              (shipit-pr-github--fetch-viewer-repos shipit-pr-github--own-repos-graphql
                                                    'repositories)))

(defun shipit-pr-github--mark-repo-notifications-read (config)
  "Mark all notifications for repo in CONFIG as read on GitHub."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/notifications" repo)))
    (shipit--debug-log "GitHub PR backend: marking all notifications read for %s" repo)
    (shipit--api-request-post endpoint nil "PUT")))

(defun shipit-pr-github--fetch-readme (config)
  "Fetch repository README using CONFIG.
Calls GET /repos/{owner}/{repo}/readme, decodes base64 content.
Returns a plist (:filename FILENAME :content TEXT), or nil if not found."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/readme" repo)))
    (shipit--debug-log "GitHub PR backend: fetching README for %s" repo)
    (let ((response (shipit--api-request endpoint)))
      (when response
        (let ((content-b64 (cdr (assq 'content response)))
              (filename (cdr (assq 'name response))))
          (when content-b64
            (list :filename filename
                  :content (decode-coding-string
                            (base64-decode-string
                             (replace-regexp-in-string "\n" "" content-b64))
                            'utf-8))))))))

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
      (cond
       ((and status-code (>= status-code 200) (< status-code 300)) t)
       ;; Mark-as-read is idempotent on the server; a 404 (or 410)
       ;; means the thread is already gone, which is the desired
       ;; end state.  Treat as success so a batch commit doesn't
       ;; abort halfway through when a few stale items race the
       ;; PATCH.
       ((and status-code (memq status-code '(404 410)))
        (shipit--debug-log "PATCH %s: HTTP %d (already gone)" url status-code)
        t)
       (t (error "API returned HTTP %s" status-code))))))

(defun shipit-pr-github--api-request-patch-no-json-async (endpoint)
  "Fire-and-forget async variant of `--api-request-patch-no-json'.
Used by the notifications buffer's deferred mark-as-read so a
batch commit doesn't block the editor on N synchronous PATCHes.
Errors are logged via `shipit--debug-log'; the caller does not
wait on success."
  (when (shipit--github-token)
    (let* ((url (concat shipit-api-url endpoint))
           (headers (list (shipit--get-auth-header)
                          '("Accept" . "application/vnd.github.v3+json")))
           (headers (remove nil headers)))
      (shipit--debug-log "API REQUEST (PATCH-NO-JSON, async): %s" url)
      (shipit--url-retrieve-async
       url "PATCH" headers nil
       (lambda (_data)
         (shipit--debug-log "Async PATCH succeeded: %s" url))
       (lambda (err)
         (shipit--debug-log "Async PATCH failed (%s): %S" url err))))))

(defun shipit-pr-github--mark-notification-read-async (config notification-id)
  "Async variant of `--mark-notification-read'.
Fire-and-forget: returns immediately, the network PATCH happens
in the background.  Used by the notifications buffer's batch
commit so a 50-row mark doesn't serialize 50 synchronous PATCHes."
  (ignore config)
  (let ((endpoint (format "/notifications/threads/%s" notification-id)))
    (shipit--debug-log "GitHub PR backend: marking notification %s as read (async)" notification-id)
    (shipit-pr-github--api-request-patch-no-json-async endpoint)))

(defun shipit-pr-github--fetch-notifications (config params &optional force-fresh)
  "Fetch notifications using CONFIG with query PARAMS.
If FORCE-FRESH is non-nil, bypasses ETag caching.
PARAMS may contain a `:repo' entry (symbol, with value `OWNER/REPO')
that scopes the fetch to a single repo using GitHub's
`/repos/OWNER/REPO/notifications' endpoint.  The entry is
stripped before the query-string is built."
  (ignore config)
  (let* ((repo (cdr (assq :repo params)))
         (clean-params (assq-delete-all :repo (copy-sequence params)))
         (endpoint (if (and repo (stringp repo) (> (length repo) 0))
                       (format "/repos/%s/notifications" repo)
                     "/notifications"))
         (result (shipit-gh-etag-get-json-with-refresh-cache
                  endpoint clean-params shipit-github-token force-fresh))
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

(defun shipit-pr-github--fetch-releases (config &optional page)
  "Fetch releases for repo in CONFIG.
PAGE defaults to 1.  Returns the parsed JSON list."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/releases" repo))
         (params `((per_page 30) (page ,(or page 1)))))
    (shipit--api-request endpoint params)))

(defun shipit-pr-github--fetch-tags (config &optional page)
  "Fetch tags for repo in CONFIG.
PAGE defaults to 1.  Returns the parsed JSON list."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/tags" repo))
         (params `((per_page 30) (page ,(or page 1)))))
    (shipit--api-request endpoint params)))

;; Register the GitHub PR backend
;;; Stacked pull requests

(defun shipit-pr-github--stack-entry (entry current-number)
  "Normalise one ENTRY of a GitHub stack.
CURRENT-NUMBER is the pull request the buffer is showing, which the
renderer marks so the reader can see where they are in the stack."
  (let ((number (cdr (assq 'number entry))))
    `((number . ,number)
      (state . ,(cdr (assq 'state entry)))
      (draft . ,(eq (cdr (assq 'draft entry)) t))
      (merged-at . ,(cdr (assq 'merged_at entry)))
      (head . ,(cdr (assq 'ref (cdr (assq 'head entry)))))
      (title . ,(cdr (assq 'title entry)))
      (author . ,(cdr (assq 'login (cdr (assq 'user entry)))))
      (current . ,(and (equal number current-number) t)))))

(defun shipit-pr-github--normalize-stack (stack membership number)
  "Return the shipit form of STACK as seen from pull request NUMBER.
STACK is the stacks endpoint\='s resource and carries the entries.
MEMBERSHIP is the `stack\=' object the pull request itself carries,
which is where depth and position live: a viewer who can only see
part of a stack still learns how deep it really is.
Entries stay in the order the API gives them, bottom of the stack
first, which is the order they must merge in."
  `((number . ,(cdr (assq 'number stack)))
    (size . ,(cdr (assq 'size membership)))
    (position . ,(cdr (assq 'position membership)))
    (base . ,(cdr (assq 'ref (cdr (assq 'base stack)))))
    (open . ,(eq (cdr (assq 'open stack)) t))
    (entries . ,(mapcar (lambda (entry)
                          (shipit-pr-github--stack-entry entry number))
                        (append (cdr (assq 'pull_requests stack)) nil)))))

(defun shipit-pr-github--create-stack (config numbers)
  "Group pull request NUMBERS in CONFIG\='s repository into a stack.
NUMBERS run from the bottom of the stack upwards, the order they merge
in.  A stack is declared, never inferred: the remote checks that each
pull request is based on the one below it, but will not group them
without being asked, and nothing in a pull request itself says it is
part of one."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/stacks" repo))
         (data `((pull_requests . ,(vconcat numbers)))))
    (shipit--debug-log "GitHub PR backend: grouping %s into a stack in %s"
                       numbers repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--add-to-stack (config stack number)
  "Add pull request NUMBER to STACK in CONFIG\='s repository."
  (let* ((repo (plist-get config :repo))
         (endpoint (format "/repos/%s/stacks/%s/add" repo stack))
         ;; The endpoint takes a list under the plural key; the
         ;; singular came back 422 and the sandbox accepted anything.
         (data `((pull_requests . ,(vector number)))))
    (shipit--debug-log "GitHub PR backend: adding #%s to stack %s in %s"
                       number stack repo)
    (shipit--api-request-post endpoint data)))

(defun shipit-pr-github--dissolve-stack (config stack)
  "Take the pull requests out of STACK in CONFIG\='s repository.
GitHub then dissolves the stack.  It refuses to change the base of a
pull request in a stack, and it adds pull requests only on top, so a
stack that must change in the middle goes first.  Return non-nil when
GitHub took it; its answer then has no body."
  (let ((endpoint (format "/repos/%s/stacks/%s/unstack"
                          (plist-get config :repo) stack)))
    (shipit--debug-log "GitHub PR backend: dissolving stack %s" stack)
    (and (shipit--api-request-post-response endpoint (make-hash-table)) t)))

(declare-function shipit-get-pr-for-branch "shipit-http" (branch &optional repo))
(declare-function shipit--graphql-query "shipit-http" (query variables))

(defun shipit-pr-github--fetch-pr-for-branch (config branch)
  "Return the number of the open pull request whose head is BRANCH.
Backed by the branch lookup the rest of shipit uses, so the answer
and its cache are shared."
  (cdr (assq 'number
             (shipit-get-pr-for-branch branch (plist-get config :repo)))))

(defun shipit-pr-github--fetch-merged-pr-for-branch (config branch)
  "Return the merged pull request whose head was BRANCH, or nil.
The answer is an alist of its `number' and the `sha' that BRANCH stood
on when it merged.  GitHub lists a merged pull request among the closed
ones, and only `merged_at' tells it from one that was closed without a
merge."
  (let* ((repo (plist-get config :repo))
         (owner (car (split-string repo "/")))
         (prs (shipit--api-request (format "/repos/%s/pulls" repo)
                                   `(("state" "closed")
                                     ("head" ,(format "%s:%s" owner branch)))))
         (merged (seq-find (lambda (pr) (cdr (assq 'merged_at pr))) prs)))
    (when merged
      `((number . ,(cdr (assq 'number merged)))
        (sha . ,(cdr (assq 'sha (cdr (assq 'head merged)))))))))

(defun shipit-pr-github--stack-statuses-query (owner name numbers)
  "Return the GraphQL query for the statuses of NUMBERS.
One aliased field per pull request asks for its review decision and
its head commit\='s check rollup, so the whole stack is answered in a
single request."
  (format "query { repository(owner:\"%s\", name:\"%s\") { %s } }"
          owner name
          (mapconcat
           (lambda (number)
             (format "p%d: pullRequest(number:%d) { reviewDecision commits(last:1) { nodes { commit { statusCheckRollup { state } } } } }"
                     number number))
           numbers " ")))

(defun shipit-pr-github--stack-checks-state (pr)
  "Return the downcased rollup state in PR\='s query answer, or nil.
Nil both for a missing pull request and for one whose head commit has
no checks at all, which the API reports as a null rollup."
  (let* ((nodes (cdr (assq 'nodes (cdr (assq 'commits pr)))))
         (commit (and (> (length nodes) 0)
                      (cdr (assq 'commit (elt nodes 0)))))
         (state (cdr (assq 'state (cdr (assq 'statusCheckRollup commit))))))
    (and state (downcase state))))

(defun shipit-pr-github--stack-review-decision (pr)
  "Return the downcased review decision in PR\='s query answer, or nil.
Nil for a missing pull request and for a null decision, which the API
reports when no review is required and none was given."
  (let ((decision (cdr (assq 'reviewDecision pr))))
    (and (stringp decision) (downcase decision))))

(defun shipit-pr-github--stack-statuses (pr)
  "Return the statuses alist in PR\='s query answer, or nil.
Up to two keys: `checks' with the rollup state and `review' with the
review decision.  Nil when the pull request has neither, so the
caller leaves it out of its answer."
  (let ((checks (shipit-pr-github--stack-checks-state pr))
        (review (shipit-pr-github--stack-review-decision pr)))
    (append (and checks (list (cons 'checks checks)))
            (and review (list (cons 'review review))))))

(defun shipit-pr-github--fetch-stack-statuses (config numbers)
  "Return (NUMBER . STATUSES) for each of NUMBERS with any status.
One GraphQL query answers for the whole stack: the rollup and the
review decision are the forge\='s own verdicts, where asking REST
would cost a round of requests per sibling on every render."
  (when numbers
    (let* ((repo (split-string (plist-get config :repo) "/"))
           (data (shipit--graphql-query
                  (shipit-pr-github--stack-statuses-query
                   (car repo) (cadr repo) numbers)
                  nil))
           (answers (cdr (assq 'repository data))))
      (delq nil
            (mapcar
             (lambda (number)
               (let* ((key (intern (format "p%d" number)))
                      (statuses (shipit-pr-github--stack-statuses
                                 (cdr (assq key answers)))))
                 (and statuses (cons number statuses))))
             numbers)))))

(defun shipit-pr-github--fetch-stack (config number &optional pr-data)
  "Return the stack pull request NUMBER belongs to, or nil.
Membership rides on the pull request resource, so PR-DATA is used
when the caller already has it and nothing is requested for an
unstacked pull request at all.  Only a stacked one pays for the
request that lists its siblings.

Without PR-DATA the pull request is fetched.  That is a round trip
even when it is served from the ETag cache, so callers holding the
data should pass it: this runs on every render of the section."
  (let* ((repo (plist-get config :repo))
         (pr (or pr-data (shipit-pr-github--fetch-pr config number)))
         (membership (cdr (assq 'stack pr)))
         (stack-number (cdr (assq 'number membership))))
    (when stack-number
      (let ((stack (shipit--api-request
                    (format "/repos/%s/stacks/%s" repo stack-number))))
        (when stack
          (shipit-pr-github--normalize-stack stack membership number))))))

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
       :fetch-pr-async #'shipit-pr-github--fetch-pr-async
       :search #'shipit-pr-github--search
       :create-pr #'shipit-pr-github--create-pr
       :merge-pr #'shipit-pr-github--merge-pr
       :update-pr #'shipit-pr-github--update-pr
       :set-pr-draft #'shipit-pr-github--set-pr-draft
       :fetch-pr-for-branch #'shipit-pr-github--fetch-pr-for-branch
       :fetch-merged-pr-for-branch #'shipit-pr-github--fetch-merged-pr-for-branch
       :fetch-stack #'shipit-pr-github--fetch-stack
       :fetch-stack-statuses #'shipit-pr-github--fetch-stack-statuses
       :browse-stack-url #'shipit-pr-github--browse-stack-url
       :create-stack #'shipit-pr-github--create-stack
       :add-to-stack #'shipit-pr-github--add-to-stack
       :dissolve-stack #'shipit-pr-github--dissolve-stack
       :fetch-reviews #'shipit-pr-github--fetch-reviews
       :submit-review #'shipit-pr-github--submit-review
       :fetch-review-decision #'shipit-pr-github--fetch-review-decision
       :fetch-files #'shipit-pr-github--fetch-files
       :fetch-commits #'shipit-pr-github--fetch-commits
       :fetch-checks #'shipit-pr-github--fetch-checks
       :fetch-releases #'shipit-pr-github--fetch-releases
       :fetch-tags #'shipit-pr-github--fetch-tags
       :browse-url #'shipit-pr-github--browse-url
       :fetch-review-threads #'shipit-pr-github--fetch-review-threads
       :fetch-resolved-threads #'shipit-pr-github--fetch-resolved-threads
       :fetch-timeline #'shipit-pr-github--fetch-timeline
       :fetch-force-pushes #'shipit-pr-github--fetch-force-pushes
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
       :fetch-commit-comments #'shipit-pr-github--fetch-commit-comments
       :add-reviewers-batch #'shipit-pr-github--add-reviewers-batch
       :reviewer-bot-candidates #'shipit-pr-github--reviewer-bot-candidates
       :add-assignees-batch #'shipit-pr-github--add-assignees-batch
       :set-labels #'shipit-pr-github--set-labels
       :fetch-check-suites #'shipit-pr-github--fetch-check-suites
       :fetch-suite-check-runs #'shipit-pr-github--fetch-suite-check-runs
       :fetch-workflow-info #'shipit-pr-github--fetch-workflow-info
       :fetch-action-run-info #'shipit-pr-github--fetch-action-run-info
       :fetch-commit-check-runs #'shipit-pr-github--fetch-commit-check-runs
       :fetch-compare #'shipit-pr-github--fetch-compare
       :fetch-compare-with-all-files #'shipit-pr-github--fetch-compare-with-all-files
       :fetch-file-content #'shipit-pr-github--fetch-file-content
       :search-raw #'shipit-pr-github--search-raw
       :list-prs #'shipit-pr-github--list-prs
       :search-repos #'shipit-pr-github--search-repos
       :search-repos-request #'shipit-pr-github--search-repos-request
       :refspec-for-pr #'shipit-pr-github--refspec-for-pr
       :remote-for-fetch #'shipit-pr-github--remote-for-fetch
       :get-file-line-content #'shipit-pr-github--get-file-line-content
       :browse-issue-url #'shipit-pr-github--browse-issue-url
       :browse-commit-url #'shipit-pr-github--browse-commit-url
       :browse-compare-url #'shipit-pr-github--browse-compare-url
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
       :mark-notification-read-async #'shipit-pr-github--mark-notification-read-async
       :fetch-notifications #'shipit-pr-github--fetch-notifications
       :fetch-feed-async #'shipit-pr-github--fetch-feed-async
       :fetch-branches-async #'shipit-pr-github--fetch-branches-async
       :fetch-branch-names-async #'shipit-pr-github--fetch-branch-names-async
       :fetch-named-branches-async #'shipit-pr-github--fetch-named-branches-async
       :browse-discussion-url #'shipit-pr-github--browse-discussion-url
       :fetch-user-teams #'shipit-pr-github--fetch-user-teams
       :fetch-repo-info #'shipit-pr-github--fetch-repo-info
       :fetch-readme #'shipit-pr-github--fetch-readme
       :fetch-languages #'shipit-pr-github--fetch-languages
       :get-repo-subscription #'shipit-pr-github--get-repo-subscription
       :set-repo-subscription #'shipit-pr-github--set-repo-subscription
       :get-thread-subscription #'shipit-pr-github--get-thread-subscription
       :set-thread-subscription #'shipit-pr-github--set-thread-subscription
       :get-repo-starred #'shipit-pr-github--get-repo-starred
       :set-repo-starred #'shipit-pr-github--set-repo-starred
       :fetch-watched-repos #'shipit-pr-github--fetch-watched-repos
       :fetch-own-unwatched-repos #'shipit-pr-github--fetch-own-unwatched-repos
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

(defun shipit-pr-github--viewed-states-from-graphql (result)
  "Extract (path . viewerViewedState) alist from GraphQL RESULT."
  (when result
    (let* ((pr (cdr (assq 'pullRequest (cdr (assq 'repository result)))))
           (files (cdr (assq 'files pr)))
           (nodes (cdr (assq 'nodes files))))
      (mapcar (lambda (node)
                (cons (cdr (assq 'path node))
                      (cdr (assq 'viewerViewedState node))))
              (append nodes nil)))))

(defun shipit-pr-github--fetch-file-viewed-states (config pr-number &optional callback)
  "Fetch viewerViewedState for all files in PR-NUMBER using CONFIG.
Returns an alist of (filename . state) where state is
\"VIEWED\", \"UNVIEWED\", or \"DISMISSED\".

When CALLBACK is non-nil, fetch asynchronously and invoke CALLBACK
with the alist (or nil on error).  Returns nil immediately."
  (let* ((repo (plist-get config :repo))
         (parts (split-string repo "/"))
         (owner (car parts))
         (name (cadr parts))
         (query (format "{ repository(owner: \"%s\", name: \"%s\") { pullRequest(number: %d) { files(first: 100) { nodes { path viewerViewedState } } } } }"
                        owner name pr-number)))
    (if callback
        (shipit--graphql-request
         query nil
         (lambda (response)
           (funcall callback
                    (shipit-pr-github--viewed-states-from-graphql
                     (cdr (assq 'data response))))))
      (shipit-pr-github--viewed-states-from-graphql
       (shipit--graphql-query query nil)))))

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

;;; Uploaded videos

(defconst shipit-pr-github--upload-regexp
  (concat "\\`https://github\\.com/"
          "\\(?:user-attachments\\|[^/]+/[^/]+\\)/assets/"
          "\\(?:[0-9]+/\\)?[0-9a-f-]+\\'")
  "Regexp matching the URL of a file uploaded to GitHub.
It is uploaded to a repository, or to a comment.")

(defun shipit-pr-github--upload-video-p (url)
  "Return non-nil when URL is a file uploaded to GitHub.
GitHub shows such a URL that stands alone on a line as a video, in a
section that folds under a header.  A video file of another site it
shows as a link."
  (string-match-p shipit-pr-github--upload-regexp url))

(add-hook 'shipit-video-url-functions #'shipit-pr-github--upload-video-p)
(add-hook 'shipit-video-fold-functions #'shipit-pr-github--upload-video-p)

(provide 'shipit-pr-github)
;;; shipit-pr-github.el ends here
