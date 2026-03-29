;;; shipit-lib.el --- Pure utility functions -*- lexical-binding: t; -*-

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

;;; This file is part of shipit — code review integration for Magit.

;;; Commentary:
;; Pure utility functions with no shipit module dependencies.
;; This file should only depend on standard Emacs libraries and magit.
;; Other shipit modules can require this without creating circular dependencies.

;;; Code:
(require 'cl-lib)

;; External dependency - magit-git functions
(declare-function magit-git-exit-code "magit-git")

;;; Git utilities

(defun shipit--sha-exists-locally (sha)
  "Check if SHA exists in the local Git repository."
  (when (and sha (stringp sha) (> (length sha) 0))
    (= 0 (magit-git-exit-code "cat-file" "-e" sha))))

(defun shipit--ref-exists-locally (ref)
  "Check if REF (branch/tag/SHA) exists in the local Git repository.
Uses git cat-file for stricter SHA validation than rev-parse."
  (when (and ref (stringp ref) (> (length ref) 0))
    ;; Use cat-file -e which fails if object doesn't exist (stricter than rev-parse)
    (= 0 (magit-git-exit-code "cat-file" "-e" ref))))

(defun shipit--range-needs-fetch-p (range)
  "Check if RANGE references commits that might not exist locally.
This is a fast heuristic check to avoid unnecessary fetches."
  (and range
       (string-match-p "\\.\\." range)
       (let ((parts (split-string range "\\.\\.")))
         (when (= (length parts) 2)
           (let* ((start-ref (car parts))
                  (end-ref (cadr parts))
                  (start-exists (shipit--ref-exists-locally start-ref))
                  (end-exists (shipit--ref-exists-locally end-ref)))
             ;; Check if either ref doesn't exist locally (quick check)
             (or (not start-exists) (not end-exists)))))))

(defun shipit--fetch-branch-async (branch callback)
  "Fetch BRANCH asynchronously and call CALLBACK with success status.
CALLBACK is called with t for success, nil for failure.
Runs git fetch in the current default-directory."
  (let ((process-name (format "shipit-fetch-%s" branch)))
    (condition-case _err
        ;; start-process uses default-directory for the process working directory
        (let ((proc (start-process process-name nil "git" "fetch" "origin" branch)))
          (set-process-sentinel
           proc
           (lambda (process _event)
             (funcall callback (eq (process-exit-status process) 0)))))
      (error
       (funcall callback nil)))))

(defun shipit--fetch-pr-ref-async (pr-number callback &optional repo head-ref)
  "Fetch PR-NUMBER's head ref asynchronously and call CALLBACK with success status.
Uses backend dispatch for refspec and remote determination.
REPO is the repo (owner/name) to fetch from.  If nil, uses origin.
HEAD-REF is the source branch name (used by GitLab backend).
CALLBACK is called with t for success, nil for failure."
  (let* ((process-name (format "shipit-fetch-pr-%s" pr-number))
         (resolved (shipit-pr--resolve-for-repo (or repo "")))
         (backend (car resolved))
         (config (cdr resolved))
         (refspec-fn (plist-get backend :refspec-for-pr))
         (remote-fn (plist-get backend :remote-for-fetch))
         (refspec (if refspec-fn
                      (funcall refspec-fn config pr-number head-ref)
                    (error "Backend %s missing :refspec-for-pr" (shipit-pr--backend-id))))
         (remote (if remote-fn
                     (funcall remote-fn config repo)
                   (error "Backend %s missing :remote-for-fetch" (shipit-pr--backend-id)))))
    (if (not refspec)
        (progn
          (shipit--debug-log "Cannot fetch PR ref: no refspec for backend")
          (funcall callback nil))
      (message "Fetching PR #%s head ref from %s..." pr-number (if repo repo "origin"))
      (condition-case _err
          (let ((proc (start-process process-name nil "git" "fetch" remote refspec)))
            (set-process-sentinel
             proc
             (lambda (process _event)
               (funcall callback (eq (process-exit-status process) 0)))))
        (error
         (funcall callback nil))))))

(defun shipit--fetch-branches-async (branches callback)
  "Fetch multiple BRANCHES asynchronously and call CALLBACK when all complete.
BRANCHES is a list of branch names (nils are filtered out).
CALLBACK is called with t if all fetches succeeded, nil otherwise."
  (let* ((branches-to-fetch (cl-remove-if-not #'identity branches))
         (total (length branches-to-fetch))
         (completed 0)
         (all-success t))
    (if (zerop total)
        (funcall callback t)
      (dolist (branch branches-to-fetch)
        (shipit--fetch-branch-async
         branch
         (lambda (success)
           (setq completed (1+ completed))
           (unless success
             (setq all-success nil))
           (when (= completed total)
             (funcall callback all-success))))))))

;;; Diff parsing utilities

(defun shipit--extract-multiline-context (diff-hunk start-line end-line)
  "Extract lines from DIFF-HUNK corresponding to START-LINE to END-LINE range.
This replicates GitHub's behavior of showing only the selected range
for multi-line comments."
  (when (and diff-hunk start-line end-line)
    (let ((lines (split-string diff-hunk "\n"))
          (extracted-lines '())
          (current-line-num nil)
          (in-target-range nil)
          (done nil))
      ;; Parse the diff hunk to find the target range
      (while (and lines (not done))
        (let ((line (pop lines)))
          (cond
           ;; Hunk header: extract the starting line number
           ((string-match "^@@ .* \\+\\([0-9]+\\)," line)
            (setq current-line-num (string-to-number (match-string 1 line)))
            (push line extracted-lines))
           ;; Added or context line: increment line counter
           ((or (string-prefix-p "+" line) (string-prefix-p " " line))
            (when current-line-num
              ;; Check if this line is in our target range
              (when (and (>= current-line-num start-line) (<= current-line-num end-line))
                (setq in-target-range t)
                (push line extracted-lines))
              ;; If we've passed the target range, we can stop
              (when (> current-line-num end-line)
                (setq done t))
              (setq current-line-num (1+ current-line-num))))
           ;; Removed line: don't increment counter but include if we're in range
           ((string-prefix-p "-" line)
            (when in-target-range
              (push line extracted-lines)))
           ;; Other lines (context, etc.)
           (t (when in-target-range
                (push line extracted-lines))))))
      ;; Return the extracted lines, or fallback to original if extraction failed
      (if extracted-lines
          (string-join (reverse extracted-lines) "\n")
        diff-hunk))))

;;; Comment utilities

(defun shipit--count-nested-replies (comment-id threads)
  "Recursively count all nested replies for COMMENT-ID in THREADS hash table."
  (let ((direct-replies (gethash comment-id threads)))
    (if direct-replies
        (+ (length direct-replies)
           (apply '+ (mapcar (lambda (reply)
                               (shipit--count-nested-replies (cdr (assq 'id reply)) threads))
                             direct-replies)))
      0)))

;;; Color utilities

(defun shipit--get-contrasting-color (hex-color)
  "Get contrasting text color (black or white) for HEX-COLOR background."
  (let* ((color (substring hex-color 1)) ; Remove #
         (r (string-to-number (substring color 0 2) 16))
         (g (string-to-number (substring color 2 4) 16))
         (b (string-to-number (substring color 4 6) 16))
         (luminance (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))
    (if (> luminance 128) "black" "white")))

;;; Text property utilities

(defun shipit--get-labels-at-point ()
  "Get labels information at point."
  (get-text-property (point) 'shipit-pr-labels))

;;; Deferred refresh utilities

(defvar shipit--pending-async-operations 0
  "Counter of pending async operations that will trigger a refresh.")

(defvar shipit--deferred-refresh-timer nil
  "Timer for deferred refresh to handle async operations.")

(defun shipit--defer-refresh ()
  "Schedule a deferred refresh after all async operations complete.
This is a no-op debouncer that cancels pending timers and schedules a new one."
  (when shipit--deferred-refresh-timer
    (cancel-timer shipit--deferred-refresh-timer))
  (setq shipit--deferred-refresh-timer
        (run-with-timer 0.5 nil
                        (lambda ()
                          (setq shipit--deferred-refresh-timer nil)))))

;;; GitHub API endpoint builders
;; Centralized endpoint definitions for consistency and maintainability.

;; Pull Request endpoints
(defun shipit-api-pr (repo pr-number)
  "Build endpoint for PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s" repo pr-number))

(defun shipit-api-pr-list (repo)
  "Build endpoint for listing PRs in REPO."
  (format "/repos/%s/pulls" repo))

(defun shipit-api-pr-files (repo pr-number)
  "Build endpoint for files changed in PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s/files" repo pr-number))

(defun shipit-api-pr-commits (repo pr-number)
  "Build endpoint for commits in PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s/commits" repo pr-number))

(defun shipit-api-pr-comments (repo pr-number)
  "Build endpoint for inline/review comments on PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s/comments" repo pr-number))

(defun shipit-api-pr-reviews (repo pr-number)
  "Build endpoint for reviews on PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s/reviews" repo pr-number))

(defun shipit-api-pr-review (repo pr-number review-id)
  "Build endpoint for specific REVIEW-ID on PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s/reviews/%s" repo pr-number review-id))

(defun shipit-api-pr-review-dismissal (repo pr-number review-id)
  "Build endpoint to dismiss REVIEW-ID on PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s/reviews/%s/dismissals" repo pr-number review-id))

(defun shipit-api-pr-requested-reviewers (repo pr-number)
  "Build endpoint for requested reviewers on PR PR-NUMBER in REPO."
  (format "/repos/%s/pulls/%s/requested_reviewers" repo pr-number))

;; Issue endpoints (PRs are issues, used for comments, labels, assignees)
(defun shipit-api-issue (repo pr-number)
  "Build endpoint for issue/PR PR-NUMBER in REPO."
  (format "/repos/%s/issues/%s" repo pr-number))

(defun shipit-api-issue-comments (repo pr-number)
  "Build endpoint for general comments on issue/PR PR-NUMBER in REPO."
  (format "/repos/%s/issues/%s/comments" repo pr-number))

(defun shipit-api-issue-comment (repo comment-id)
  "Build endpoint for specific issue COMMENT-ID in REPO."
  (format "/repos/%s/issues/comments/%s" repo comment-id))

(defun shipit-api-issue-comment-reactions (repo comment-id)
  "Build endpoint for reactions on issue COMMENT-ID in REPO."
  (format "/repos/%s/issues/comments/%s/reactions" repo comment-id))

(defun shipit-api-issue-labels (repo pr-number)
  "Build endpoint for labels on issue/PR PR-NUMBER in REPO."
  (format "/repos/%s/issues/%s/labels" repo pr-number))

(defun shipit-api-issue-label (repo pr-number label-name)
  "Build endpoint for specific LABEL-NAME on issue/PR PR-NUMBER in REPO."
  (format "/repos/%s/issues/%s/labels/%s" repo pr-number label-name))

(defun shipit-api-issue-assignees (repo pr-number)
  "Build endpoint for assignees on issue/PR PR-NUMBER in REPO."
  (format "/repos/%s/issues/%s/assignees" repo pr-number))

(defun shipit-api-issue-reactions (repo pr-number)
  "Build endpoint for reactions on issue/PR PR-NUMBER in REPO."
  (format "/repos/%s/issues/%s/reactions" repo pr-number))

(defun shipit-api-issue-timeline (repo pr-number)
  "Build endpoint for timeline events on issue/PR PR-NUMBER in REPO."
  (format "/repos/%s/issues/%s/timeline" repo pr-number))

;; PR comment endpoints (inline/review comments)
(defun shipit-api-pr-comment (repo comment-id)
  "Build endpoint for specific PR/review COMMENT-ID in REPO."
  (format "/repos/%s/pulls/comments/%s" repo comment-id))

(defun shipit-api-pr-comment-reactions (repo comment-id)
  "Build endpoint for reactions on PR COMMENT-ID in REPO."
  (format "/repos/%s/pulls/comments/%s/reactions" repo comment-id))

;; Repository endpoints
(defun shipit-api-repo-labels (repo)
  "Build endpoint for all labels in REPO."
  (format "/repos/%s/labels" repo))

(defun shipit-api-repo-assignees (repo)
  "Build endpoint for available assignees in REPO."
  (format "/repos/%s/assignees" repo))

(defun shipit-api-repo-contents (repo path)
  "Build endpoint for file contents at PATH in REPO."
  (format "/repos/%s/contents/%s" repo path))

(defun shipit-api-repo-compare (repo base head)
  "Build endpoint to compare BASE...HEAD in REPO."
  (format "/repos/%s/compare/%s...%s" repo base head))

;; Commit endpoints
(defun shipit-api-commit (repo sha)
  "Build endpoint for commit SHA in REPO."
  (format "/repos/%s/commits/%s" repo sha))

(defun shipit-api-commit-check-suites (repo sha)
  "Build endpoint for check suites on commit SHA in REPO."
  (format "/repos/%s/commits/%s/check-suites" repo sha))

(defun shipit-api-commit-check-runs (repo sha)
  "Build endpoint for check runs on commit SHA in REPO."
  (format "/repos/%s/commits/%s/check-runs" repo sha))

;; Check suite endpoints
(defun shipit-api-check-suite-runs (repo suite-id)
  "Build endpoint for check runs in SUITE-ID in REPO."
  (format "/repos/%s/check-suites/%s/check-runs" repo suite-id))

;; Actions endpoints
(defun shipit-api-workflow (repo workflow-id)
  "Build endpoint for workflow WORKFLOW-ID in REPO."
  (format "/repos/%s/actions/workflows/%s" repo workflow-id))

(defun shipit-api-workflow-run (repo run-id)
  "Build endpoint for workflow run RUN-ID in REPO."
  (format "/repos/%s/actions/runs/%s" repo run-id))

;; Branch protection endpoints
(defun shipit-api-branch-protection-reviews (repo branch)
  "Build endpoint for required reviews protection on BRANCH in REPO."
  (format "/repos/%s/branches/%s/protection/required_pull_request_reviews"
          repo branch))

;;; CODEOWNERS utilities

(defun shipit--parse-codeowners-patterns (codeowners-text)
  "Parse CODEOWNERS-TEXT and return list of (pattern . owners) pairs.
Each pattern is a glob-style pattern and owners is a list of @user or @team strings."
  (when codeowners-text
    (let ((patterns '()))
      (dolist (line (split-string codeowners-text "\n" t))
        ;; Skip comments and empty lines
        (let ((trimmed (string-trim line)))
          (unless (or (string-empty-p trimmed)
                      (string-prefix-p "#" trimmed))
            ;; Parse line: pattern followed by owners
            (let* ((parts (split-string trimmed "[ \t]+" t))
                   (pattern (car parts))
                   (owners (cdr parts)))
              (when (and pattern owners)
                (push (cons pattern owners) patterns))))))
      (nreverse patterns))))

(defun shipit--codeowners-pattern-to-regexp (pattern)
  "Convert CODEOWNERS PATTERN to an Emacs regexp.
Handles glob patterns like *.js, /foo/bar/, **/*.py, etc."
  (let ((regexp pattern))
    ;; Handle leading / (anchors to repo root)
    (if (string-prefix-p "/" regexp)
        (setq regexp (concat "^" (substring regexp 1)))
      ;; No leading / means it can match anywhere
      (setq regexp (concat "\\(?:^\\|/\\)" regexp)))
    ;; Escape special regexp characters (except *)
    (setq regexp (replace-regexp-in-string "\\." "\\\\." regexp))
    (setq regexp (replace-regexp-in-string "\\?" "." regexp))
    ;; Handle ** (matches any path including /)
    (setq regexp (replace-regexp-in-string "\\*\\*" ".*" regexp))
    ;; Handle * (matches anything except /)
    (setq regexp (replace-regexp-in-string "\\*" "[^/]*" regexp))
    ;; Trailing / means directory (match anything under it)
    (when (string-suffix-p "/" regexp)
      (setq regexp (concat (substring regexp 0 -1) "\\(?:/.*\\)?$")))
    regexp))

(defun shipit--user-matches-owner-p (owner github-user user-teams)
  "Check if OWNER matches GITHUB-USER directly or via USER-TEAMS membership.
OWNER can be @username, @org/team, or username format."
  (cond
   ;; Direct user match: @username or username
   ((or (string= owner (concat "@" github-user))
        (string= owner github-user))
    t)
   ;; Team match: @org/team format - extract team slug and check membership
   ((string-match "^@[^/]+/\\(.+\\)$" owner)
    (let ((team-slug (match-string 1 owner)))
      (member team-slug user-teams)))
   ;; Team match without @ prefix: org/team format
   ((string-match "^[^/]+/\\(.+\\)$" owner)
    (let ((team-slug (match-string 1 owner)))
      (member team-slug user-teams)))
   (t nil)))

(defun shipit--get-github-username ()
  "Get the current username from git config or environment.
Supports both GitHub and GitLab noreply email formats."
  (let ((env-user (getenv "GITHUB_USER")))
    (if (and env-user (not (string-empty-p env-user)))
        env-user
      (let ((git-user (string-trim
                       (shell-command-to-string
                        "git config --get github.user 2>/dev/null"))))
        (if (not (string-empty-p git-user))
            git-user
          (let ((email (string-trim
                        (shell-command-to-string
                         "git config --get user.email 2>/dev/null"))))
            (cond
             ((string-match "@users\\.noreply\\.github\\.com" email)
              (car (split-string email "@")))
             ((string-match "@users\\.noreply\\.gitlab\\.com" email)
              (car (split-string email "@"))))))))))

(provide 'shipit-lib)
;;; shipit-lib.el ends here
