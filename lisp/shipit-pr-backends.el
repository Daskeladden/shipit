;;; shipit-pr-backends.el --- Pluggable PR backend registry -*- lexical-binding: t; -*-

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
;; Pluggable backend registry for PR operations (GitHub, GitLab, etc.).
;; A backend is a plist registered under a symbol ID.
;;
;; Required plist keys:
;;   :name                 — human-readable name
;;   :fetch-pr             — (config number) → PR alist
;;   :search               — (config args) → list of PR alists
;;   :create-pr            — (config title body base head draft) → PR alist
;;   :merge-pr             — (config number method &optional sha) → merge
;;                           result: alist of `status' ("merged",
;;                           "enqueued", "pending" or "failed"),
;;                           `message' and `id'; nil when refused
;;   :update-pr            — (config number data) → PR alist
;;   :fetch-reviews        — (config number) → list of review alists
;;   :submit-review        — (config number event body comments) → review alist
;;   :fetch-review-decision — (config number) → decision data
;;   :fetch-files          — (config number) → list of file alists
;;   :fetch-commits        — (config number) → list of commit alists
;;   :fetch-checks         — (config ref) → list of check alists
;;   :browse-url           — (config number) → URL string
;;
;; Optional plist keys:
;;   :fetch-review-threads    — (config number) → threaded review data
;;   :fetch-resolved-threads  — (config number) → resolved thread info
;;   :fetch-timeline          — (config number) → timeline events
;;   :fetch-branch-protection — (config base-ref) → protection rules
;;   :fetch-codeowners        — (config base-ref) → CODEOWNERS text
;;   :add-label               — (config number label-name) → list of label alists
;;   :remove-label            — (config number label-name) → t on success
;;   :dismiss-review           — (config number message) → result or nil
;;   :add-reaction            — (config number reaction) → reaction alist
;;   :fetch-reactions         — (config number) → list of reaction alists
;;   :delete-reaction         — (config number reaction-id) → t on success
;;   :add-reviewer            — (config number username) → result
;;   :remove-reviewer         — (config number username) → result
;;   :fetch-requested-reviewers — (config number) → reviewer data
;;   :add-assignee            — (config number username) → result
;;   :remove-assignee         — (config number username) → result
;;   :fetch-pr-assignees      — (config number) → list of assignee alists
;;   :fetch-available-assignees — (config) → list of username strings
;;   :fetch-available-labels  — (config) → list of label alists
;;   :fetch-labels            — (config number) → minimal PR data with labels
;;   :fetch-commit            — (config sha) → commit data with files
;;   :fetch-commit-comments   — (config sha) → list of commit-comment alists [GitHub-only]
;;   :add-reviewers-batch     — (config number usernames) → result
;;   :reviewer-bot-candidates — (config) → alist of (display . login) requestable bot reviewers
;;   :add-assignees-batch     — (config number usernames) → result
;;   :set-labels              — (config number label-names) → result
;;   :fetch-check-suites      — (config ref page per-page) → check suites response
;;   :fetch-suite-check-runs  — (config suite-id) → check runs response
;;   :fetch-workflow-info     — (config workflow-id) → workflow data
;;   :fetch-action-run-info   — (config run-id) → action run data
;;   :fetch-commit-check-runs — (config ref page per-page) → check runs response
;;   :fetch-compare           — (config base head) → compare data with merge_base_commit
;;   :fetch-compare-with-all-files — (config base head) → compare data as
;;                              :fetch-compare gives it, with every changed
;;                              file and its patch, also those that its
;;                              compare API leaves out
;;   :search-repos            — (config query) → list of repo alists ((full_name . "owner/repo") ...)
;;   :search-repos-request    — (config query) → (url headers normalizer) for curl-based live search
;;   :search-raw              — (config query page per-page) → search results
;;   :refspec-for-pr          — (config pr-number head-ref) → refspec string
;;   :remote-for-fetch        — (config repo) → remote name or URL
;;   :fetch-repo-info         — (config) → repository alist as GitHub
;;                              gives it, or nil when the forge did not
;;                              answer.  For a fork, parent and source
;;                              each hold the full_name of the repository
;;                              that it was forked from, and of the first
;;                              repository of its chain
;;   :pr-type-label           — string, e.g. "Pull Request" or "Merge Request"
;;   :editor-extra-keys       — alist of (key-string . command) for editor keybindings
;;   :editor-reference-hints  — string for editor header line, e.g. "#: PR" or "#: Issue, !: MR"
;;   :browse-issue-url        — (config number) → URL string for issue
;;   :browse-commit-url       — (config sha) → URL string for commit
;;   :browse-compare-url      — (config base head) → browser URL for the compare view
;;   :browse-user-url         — (config username) → URL string for user profile
;;   :get-current-username    — (config) → authenticated username string
;;   :extract-username-from-email — (config email) → username or nil
;;   :generate-avatar-url     — (config username) → avatar URL or nil
;;   :fetch-rate-limit        — (config) → rate limit data alist
;;   :fetch-current-user      — (config) → user data alist with login, name
;;
;; Async check operations (used by shipit-checks.el for non-blocking CI fetch):
;;   :fetch-check-suites-async     — (config ref page per-page callback)
;;   :fetch-suite-check-runs-async — (config suite-id callback)
;;   :fetch-workflow-info-async    — (config workflow-id callback) [GitHub-only]
;;   :fetch-action-run-info-async  — (config run-id callback) [GitHub-only]
;;   :fetch-commit-check-runs-async — (config ref page per-page callback)
;;
;; Notification operations:
;;   :mark-notification-read  — (config notification-id) → t on success
;;   :fetch-notifications     — (config params &optional force-fresh) → list
;;   :fetch-feed-async        — (config callback) → calls CALLBACK with the
;;                              entries of the feed of the user, the newest
;;                              first: what the people that the user follows
;;                              do.  An entry is an alist of id, kind (star,
;;                              fork, create, release or public), actor, repo,
;;                              own (whether the repository is the user's),
;;                              detail, title, url and time
;;   :fetch-branches-async    — (config callback) → calls CALLBACK with the
;;                              branches of the repository, the newest last
;;                              commit first, or with nil when nothing is
;;                              known.  A branch is an alist of name, sha,
;;                              time, subject and author of the last commit,
;;                              own (whether the user made that commit),
;;                              default, base (the name of the default
;;                              branch, which the branch is compared
;;                              with), url, and pr: the number, title and
;;                              state (open, draft, merged or closed) of its
;;                              newest pull request, or nil
;;   :fetch-branch-names-async — (config etag callback) → calls CALLBACK
;;                              with `unchanged' when the branches did not
;;                              change since ETAG came with their names,
;;                              else with (ETAG . NAMES) for all branches, or
;;                              with nil when nothing is known
;;   :fetch-named-branches-async — (config names callback) → calls CALLBACK
;;                              with the branches named NAMES that are there,
;;                              as :fetch-branches-async gives them but
;;                              with pr only for an open pull request, or
;;                              with nil when nothing is known
;;   :browse-discussion-url   — (config number) → URL string
;;   :fetch-user-teams        — (config) → list of team data alists
;;   :fetch-merge-methods     — (config) → list of allowed merge method strings
;;   :get-repo-subscription   — (config) → subscription alist or nil
;;   :set-repo-subscription   — (config state) → result
;;   :get-repo-starred        — (config) → t or nil
;;   :set-repo-starred        — (config starred) → result
;;   :fetch-watched-repos     — (config) → list of repo alists
;;   :fetch-own-unwatched-repos — (config) → list of repo alists: the
;;                              repositories that the user owns and
;;                              neither watches nor ignores
;;   :get-thread-subscription — (config repo type number) → state string or nil
;;   :set-thread-subscription — (config repo type number subscribed) → t on success
;;   :set-pr-draft            — (config number draft-p) → non-nil on success
;;   :fetch-pr-async          — (config number callback) → fetches in the
;;                              background, then calls CALLBACK with the
;;                              PR alist, or with nil when it is no PR
;;
;; Stacked pull requests.  Only GitHub reports these; a backend without
;; the operation makes the Stack section elide rather than render empty:
;;   :fetch-stack             — (config number &optional pr-data) → stack
;;                              alist, or nil when
;;                              the pull request is not in a stack
;;   :fetch-stack-statuses    — (config numbers) → alist of
;;                              (number . statuses), statuses an alist
;;                              with `checks' and `review' keys
;;   :browse-stack-url        — (config number) → the stack's web URL
;;   :dissolve-stack          — (config stack) → non-nil when the remote
;;                              took the pull requests out of STACK
;;   :fetch-pr-for-branch     — (config branch) → the number of the open
;;                              pull request whose head is BRANCH, or nil
;;   :fetch-merged-pr-for-branch — (config branch) → alist of `number'
;;                              and `sha' of the merged pull request whose
;;                              head was BRANCH, `sha' being where BRANCH
;;                              stood when it merged; nil when none merged
;;   :fetch-merge-result      — (config number id) → the merge result of
;;                              the pending merge ID, as :merge-pr gives it

;;; Code:

(require 'shipit-core)

(defvar shipit-pr-backends nil
  "Alist of (ID . PLIST) for registered PR backends.")

(defconst shipit-pr--required-keys
  '(:name :fetch-pr :search :create-pr :merge-pr :update-pr
    :fetch-reviews :submit-review :fetch-review-decision
    :fetch-files :fetch-commits :fetch-checks :browse-url)
  "Required keys in a PR backend plist.")

(defun shipit-pr-register-backend (id plist)
  "Register PR backend ID with PLIST.
PLIST must contain all keys in `shipit-pr--required-keys'."
  (dolist (key shipit-pr--required-keys)
    (unless (plist-get plist key)
      (error "PR backend %s missing required key %s" id key)))
  (setq shipit-pr-backends (assq-delete-all id shipit-pr-backends))
  (push (cons id plist) shipit-pr-backends)
  (shipit--debug-log "Registered PR backend: %s (%s)" id (plist-get plist :name)))

(defun shipit-pr--backend-id ()
  "Return the resolved PR backend symbol.
Resolves `auto' to a concrete backend via git remote detection."
  (if (eq shipit-pr-backend 'auto)
      (shipit--detect-backend-from-remote)
    shipit-pr-backend))

(defun shipit-pr--get-backend ()
  "Return the active PR backend plist.
Uses `shipit-pr-backend' defcustom.  When set to `auto', detects
from the git remote URL."
  (let ((id (shipit-pr--backend-id)))
    (or (cdr (assq id shipit-pr-backends))
        (error "No PR backend registered for `%s'" id))))

(defun shipit-pr--resolve-for-repo (repo)
  "Resolve PR backend and config for REPO.
Returns (BACKEND-PLIST . CONFIG-PLIST) with :repo injected into config.
When backend has :inject-project-path, also populates :project-path from REPO."
  (let* ((backend-plist (shipit-pr--get-backend))
         (base-config shipit-pr-backend-config)
         (config (if base-config
                     (plist-put (copy-sequence base-config) :repo repo)
                   (list :repo repo))))
    ;; Auto-populate :project-path from repo slug when backend requests it
    (when (and (plist-get backend-plist :inject-project-path)
               (not (plist-get config :project-path))
               (not (plist-get config :project-id)))
      (setq config (plist-put config :project-path repo)))
    (cons backend-plist config)))

(defun shipit-pr--resolve-for-other-repo (repo)
  "Resolve the PR backend and config for REPO, whatever project the settings name.
As `shipit-pr--resolve-for-repo', but the config names REPO also when
the settings name a project, such as the settings of the page of a
merge request."
  (let ((shipit-pr-backend-config
         (cl-loop for (key value) on shipit-pr-backend-config by #'cddr
                  unless (memq key '(:project-path :project-id))
                  append (list key value))))
    (shipit-pr--resolve-for-repo repo)))

(defun shipit-pr--fetch-remote-and-refspec (repo pr-number head-ref)
  "Where to fetch the pull request PR-NUMBER of REPO from, as (REMOTE . REFSPEC).
REPO is owner/name, or nil when it is not known.  REMOTE is what the
backend's :remote-for-fetch gives for REPO, or \"origin\" when the
backend has none.  REFSPEC is what its :refspec-for-pr gives for
PR-NUMBER and HEAD-REF, the branch of the pull request.  Signal an
error when the backend has no :refspec-for-pr."
  (let* ((resolved (shipit-pr--resolve-for-repo (or repo "")))
         (backend (car resolved))
         (config (cdr resolved))
         (refspec-fn (or (plist-get backend :refspec-for-pr)
                         (error "Backend %s missing :refspec-for-pr" (shipit-pr--backend-id))))
         (remote-fn (plist-get backend :remote-for-fetch)))
    (cons (if remote-fn (funcall remote-fn config repo) "origin")
          (funcall refspec-fn config pr-number head-ref))))

;;; Forks
;;
;; A clone of a fork counts as a clone of the repository that the fork
;; came from, see `shipit--clone-here-p'.  Only the forge knows where a
;; fork came from.  Shipit asks it only about a repository with the name
;; of the other one, so a fork that was given another name does not
;; count.

(defvar shipit--forked-from-cache (make-hash-table :test 'equal)
  "The repositories that a repository was forked from, as the forge said.
The key is the list of the backend and the repository, as owner/name
in lower case.  The value is a list of owner/name paths, or nil for a
repository that is no fork.  A question that the forge did not answer
leaves no entry, so the forge is asked again the next time.  Cleared
by `shipit-clear-all-caches'.")

(defun shipit--repo-name (repo)
  "The name of REPO, the last part of its owner/name path."
  (car (last (split-string repo "/"))))

(defun shipit--same-repo-name-p (repo other)
  "Whether REPO and OTHER, each as owner/name, have the same name, in any case."
  (string-equal-ignore-case (shipit--repo-name repo) (shipit--repo-name other)))

(defun shipit-pr--fetch-repo-info-quietly (repo)
  "The data that the forge gives about REPO, as owner/name, or nil.
Nil when the forge gives no answer, and when the backend has no
:fetch-repo-info.  A failed request goes to the debug log, so that it
does not break what asked."
  (let* ((resolved (shipit-pr--resolve-for-other-repo repo))
         (fetch (plist-get (car resolved) :fetch-repo-info)))
    (when fetch
      (condition-case err
          (or (funcall fetch (cdr resolved))
              (progn (shipit--debug-log "The forge gave no data about %s" repo)
                     nil))
        (error
         (shipit--debug-log "Asking the forge about %s failed: %s"
                            repo (error-message-string err))
         nil)))))

(defun shipit--fork-sources (info)
  "The repositories that the forge's data INFO says its repository came from.
That is its parent and its source, the first repository of its chain,
as owner/name, without nil and without duplicates."
  (delete-dups
   (delq nil (mapcar (lambda (key) (cdr (assq 'full_name (cdr (assq key info)))))
                     '(parent source)))))

(defun shipit--forked-from (repo)
  "The repositories that REPO, as owner/name, was forked from, as the forge says.
That is its parent and the first repository of its chain.  Nil when
REPO is no fork, or when the forge gave no answer.  An answer is
remembered in `shipit--forked-from-cache', and no answer is not."
  (let* ((key (list (shipit-pr--backend-id) (downcase repo)))
         (known (gethash key shipit--forked-from-cache 'shipit--unknown)))
    (if (not (eq known 'shipit--unknown))
        known
      (when-let* ((info (shipit-pr--fetch-repo-info-quietly repo)))
        (puthash key (shipit--fork-sources info) shipit--forked-from-cache)))))

(defun shipit--fork-of-p (fork repo)
  "Whether FORK was forked from REPO, each as owner/name, as the forge says.
REPO must be the parent of FORK or the first repository of its chain.
The forge is asked only when FORK has the name of REPO."
  (and (shipit--same-repo-name-p fork repo)
       (member-ignore-case repo (shipit--forked-from fork))
       t))

;;; Output contracts — define required fields per operation

;; Common shipit check shape — the canonical representation for CI/CD data.
;; Both GitHub and GitLab backends normalize to this shape.
;;
;; Required fields (all backends must provide):
;;   name        — Check/job display name (string)
;;   status      — "queued" | "in_progress" | "completed"
;;   conclusion  — nil | "success" | "failure" | "cancelled" | "skipped" | "action_required"
;;   html_url    — Link to check details page (string)
;;
;; Optional fields:
;;   started_at      — ISO-8601 timestamp when the check started
;;   completed_at    — ISO-8601 timestamp when the check finished
;;   workflow-name   — Grouping: top-level category
;;                     (GitHub: workflow name, GitLab: pipeline stage)
;;   workflow-run-name — Grouping: run instance within category

(defconst shipit-pr--output-contracts
  '((:fetch-pr
     . ((required . (number title body state user head base html_url created_at))
        (user-required . (login))
        (nested . ((head . (ref sha)) (base . (ref))))))
    (:search
     . ((list-of . t)
        (required . (number title state user))
        (user-required . (login))))
    (:fetch-files
     . ((list-of . t)
        (required . (filename status additions deletions))))
    (:fetch-commits
     . ((list-of . t)
        (required . (sha commit))
        (nested . ((commit . (message))))))
    (:fetch-reviews
     . ((list-of . t)
        (required . (user state))
        (user-required . (login))))
    (:fetch-checks
     . ((list-of . t)
        (required . (name status conclusion html_url)))))
  "Output contracts for PR backend operations.
Covers data-returning read operations only (not browse-url, merge, etc.).")

(defun shipit-pr--verify-contract (operation result)
  "Verify RESULT satisfies the output contract for OPERATION.
Signals error listing missing fields.  For list-returning operations,
verifies every element."
  (let* ((spec (cdr (assq operation shipit-pr--output-contracts)))
         (required (cdr (assq 'required spec)))
         (user-required (cdr (assq 'user-required spec)))
         (nested (cdr (assq 'nested spec))))
    (unless spec (error "No contract for %s" operation))
    (if (cdr (assq 'truthy spec))
        (unless result
          (error "Contract %s violated — expected truthy value" operation))
      (let ((items (if (cdr (assq 'list-of spec)) result (list result))))
        (dolist (item items)
          (let ((missing nil))
            (dolist (field required)
              (unless (cdr (assq field item))
                (push field missing)))
            (when user-required
              (let ((user (cdr (assq 'user item))))
                (dolist (f user-required)
                  (unless (cdr (assq f user))
                    (push (intern (format "user.%s" f)) missing)))))
            (dolist (nest nested)
              (let ((sub (cdr (assq (car nest) item))))
                (dolist (f (cdr nest))
                  (unless (and sub (cdr (assq f sub)))
                    (push (intern (format "%s.%s" (car nest) f)) missing)))))
            (when missing
              (error "Contract %s violated — missing: %s" operation missing))))))))

(defconst shipit-pr--parity-operations
  '(:add-label :remove-label :fetch-labels :set-labels :fetch-available-labels
    :add-reviewer :remove-reviewer :add-reviewers-batch :fetch-requested-reviewers
    :add-assignee :remove-assignee :add-assignees-batch
    :fetch-pr-assignees :fetch-available-assignees
    :dismiss-review :fetch-commit)
  "Operations all PR backends should support for feature parity.")

(defun shipit-pr--classify-url (url)
  "Classify URL by trying each registered backend's :classify-url.
Returns plist (:type TYPE :repo REPO :backend-id ID ...) or nil.
The :backend-id key identifies which backend matched."
  (cl-some (lambda (entry)
             (let* ((backend-id (car entry))
                    (fn (plist-get (cdr entry) :classify-url))
                    (result (when fn (funcall fn url))))
               (when result
                 (plist-put result :backend-id backend-id))))
           shipit-pr-backends))

(defun shipit-pr--create-reference-overlays (repo search-start search-limit inc-found inc-overlay)
  "Create reference overlays by calling each backend's :create-reference-overlays.
Iterates ALL registered backends so cross-forge URLs in comments get overlays."
  (dolist (entry shipit-pr-backends)
    (let ((fn (plist-get (cdr entry) :create-reference-overlays)))
      (when fn (funcall fn repo search-start search-limit inc-found inc-overlay)))))

(defun shipit--get-thread-subscription (repo type number)
  "Get thread subscription state for TYPE NUMBER in REPO via backend.
Returns \"subscribed\", \"unsubscribed\", \"ignored\", or nil if unsupported."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :get-thread-subscription)))
    (when fn (funcall fn config repo type number))))

(defun shipit--set-thread-subscription (repo type number subscribed)
  "Set thread subscription for TYPE NUMBER in REPO via backend.
SUBSCRIBED is t to subscribe, nil to unsubscribe.
Returns t on success, nil if unsupported."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :set-thread-subscription)))
    (when fn (funcall fn config repo type number subscribed))))

;;; Stacked pull requests

(defun shipit-pr--stack-supported-p (repo)
  "Return non-nil when REPO's backend can report stacked pull requests.
Callers use this to skip the Stack section entirely rather than
render a heading for a forge that has no such concept."
  (let ((backend (car (shipit-pr--resolve-for-repo repo))))
    (and (plist-get backend :fetch-stack) t)))

(defun shipit-pr--fetch-stack (repo number &optional pr-data)
  "Return the stack pull request NUMBER in REPO belongs to.
PR-DATA, when the caller already holds the pull request, spares the
backend a request for it.
Resolved through the active backend's optional :fetch-stack
operation.  Answers nil both when the backend has no such
operation and when the pull request is not stacked; the caller
draws nothing in either case."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :fetch-stack)))
    (when fn (funcall fn config number pr-data))))

(defun shipit-pr--fetch-pr-async (repo number callback)
  "Fetch pull request NUMBER of REPO in the background.
CALLBACK then gets the alist of the pull request, or nil when NUMBER
is no pull request.  Resolved through the active backend's optional
:fetch-pr-async operation.  Return non-nil when the fetch started, and
nil when the backend has no such operation."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (fn (plist-get (car resolved) :fetch-pr-async)))
    (when fn
      (funcall fn (cdr resolved) number callback)
      t)))

(defun shipit-pr--fetch-merged-pr-for-branch (repo branch)
  "Return the merged pull request whose head was BRANCH in REPO, or nil.
The answer is an alist of its `number' and the `sha' that BRANCH stood
on when it merged.  Resolved through the active backend\='s optional
:fetch-merged-pr-for-branch operation.  Answers nil when the backend
has none, and when no pull request of BRANCH merged."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (fn (plist-get (car resolved) :fetch-merged-pr-for-branch)))
    (when fn (funcall fn (cdr resolved) branch))))

(defun shipit-pr--fetch-stack-statuses (repo numbers)
  "Return (NUMBER . STATUSES) for each pull request of NUMBERS.
STATUSES is an alist with up to two keys: `checks', the forge\='s
rollup verdict over the head commit\='s checks (\"success\",
\"failure\" or \"pending\"), and `review', the forge\='s review
decision (\"approved\", \"changes_requested\" or \"review_required\").
Resolved through the active backend\='s optional :fetch-stack-statuses
operation.  Answers nil when the backend has none, and a number whose
pull request has neither checks nor a review decision is absent from
the answer; the caller draws nothing in either case, the way the
forge shows nothing beside such a pull request."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (fn (plist-get backend :fetch-stack-statuses)))
    (when fn (funcall fn (cdr resolved) numbers))))

(defun shipit-pr--stack-statuses-quietly (repo numbers)
  "Return the stack statuses for NUMBERS in REPO, nil on failure.
Nils among NUMBERS stand for rows without a pull request and are
dropped; with none left the backend is not asked at all.  The
statuses are a nicety on top of a stack view: a forge that cannot
answer for them right now still gets its rows drawn, and the failure
goes to the debug log instead of across the section."
  (let ((numbers (delq nil numbers)))
    (when numbers
      (condition-case err
          (shipit-pr--fetch-stack-statuses repo numbers)
        (error
         (shipit--debug-log "stack statuses fetch failed for %s: %s"
                            repo (error-message-string err))
         nil)))))

(defun shipit-pr--browse-url (repo number)
  "Return the web address of pull request NUMBER in REPO, or nil.
Resolved through the active backend\='s :browse-url operation; nil
when the backend has none, and the caller keeps whatever fallback it
has."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (fn (plist-get (car resolved) :browse-url)))
    (when fn (funcall fn (cdr resolved) number))))

(defun shipit-pr--browse-stack-url (repo number)
  "Return the web address of stack NUMBER in REPO, or nil.
Resolved through the active backend\='s optional :browse-stack-url
operation; nil when the backend has none, and the caller keeps
whatever fallback it has."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (fn (plist-get (car resolved) :browse-stack-url)))
    (when fn (funcall fn (cdr resolved) number))))

;;; Bot reviewers

(defun shipit-pr--reviewer-bot-candidates (repo)
  "Return requestable bot reviewers for REPO as (DISPLAY . LOGIN) pairs.
Resolved through the active backend's optional
:reviewer-bot-candidates operation; nil when unsupported."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :reviewer-bot-candidates)))
    (when fn (funcall fn config))))

(defun shipit-pr--reviewer-add-choices (repo current-logins available-users)
  "Reviewer picker choices for REPO: bot display names, then AVAILABLE-USERS.
Entries whose login is already in CURRENT-LOGINS are dropped."
  (append
   (mapcar #'car
           (seq-filter (lambda (pair)
                         (not (member (cdr pair) current-logins)))
                       (shipit-pr--reviewer-bot-candidates repo)))
   (seq-filter (lambda (user) (not (member user current-logins)))
               available-users)))

(defun shipit-pr--reviewer-choice-to-login (repo choice)
  "Map picker CHOICE to an API login for REPO.
Bot display names translate to their bot login; anything else is
already a login."
  (or (cdr (assoc choice (shipit-pr--reviewer-bot-candidates repo)))
      choice))

(defun shipit-pr--reviewer-login-to-display (repo login)
  "Map LOGIN to its display name when it is a known bot reviewer in REPO."
  (or (car (rassoc login (shipit-pr--reviewer-bot-candidates repo)))
      login))

(provide 'shipit-pr-backends)
;;; shipit-pr-backends.el ends here
