;;; shipit-review-revision.el --- Commit navigation for review mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'subr-x)
(require 'shipit-core)

(defvar shipit--review-current-commit nil
  "SHA of current commit being viewed in review mode.")

(defvar shipit--review-current-file-path nil
  "File path being reviewed in current buffer.")

(defvar shipit--review-pr-commits nil
  "List of PR commits in current review session.")

(defvar shipit--review-all-commits nil
  "Full history of commits (PR + before PR).")

(defvar shipit--review-pr-commit-shas nil
  "Set of commit SHAs that are part of the PR.")

(defun shipit--build-pr-commit-list (pr-data)
  "Build list of commits from PR-DATA.
Returns list of plists with :sha, :message, :pr-member keys."
  (let* ((commits (cdr (assq 'commits pr-data)))
         (commit-list (seq-into commits 'list))
         (result (mapcar (lambda (commit)
                           (list :sha (cdr (assq 'oid commit))
                                 :message (cdr (assq 'message commit))
                                 :pr-member t))
                         commit-list)))
    (shipit--debug-log "Built PR commit list with %d commits" (length result))
    result))

(defun shipit--is-pr-commit-p (sha pr-commits)
  "Check if SHA is in the list of PR-COMMITS."
  (let ((result (seq-some (lambda (commit) (equal sha (plist-get commit :sha)))
                          pr-commits)))
    (shipit--debug-log "Checking if commit %s is in PR: %s" (substring sha 0 7) (if result "yes" "no"))
    result))

(defun shipit--get-file-at-commit (file-path commit-sha)
  "Get content of FILE-PATH at COMMIT-SHA.
Returns string content or signals error if file doesn't exist at that commit."
  (unless commit-sha
    (error "Commit SHA required"))
  (shipit--debug-log "Retrieving file '%s' at commit %s" file-path (substring commit-sha 0 7))
  (condition-case err
      (let ((content (shell-command-to-string
                      (format "git show %s:%s 2>/dev/null"
                              (shell-quote-argument commit-sha)
                              (shell-quote-argument file-path)))))
        (if (string-empty-p content)
            (user-error "File '%s' does not exist at commit %s" file-path (substring commit-sha 0 7))
          content))
    (error (user-error "Failed to retrieve file at commit: %s" (error-message-string err)))))

(defun shipit--format-commit-indicator (sha is-pr-commit)
  "Format commit indicator for mode-line.
Returns formatted string like [Commit abc1234 (PR)] or [Commit xyz9999]."
  (let* ((short-sha (substring sha 0 7))
         (result (if is-pr-commit
                     (format "[Commit %s (PR)]" short-sha)
                   (format "[Commit %s]" short-sha))))
    (shipit--debug-log "Formatted commit indicator for %s (PR: %s): %s" short-sha is-pr-commit result)
    result))

(provide 'shipit-review-revision)
;;; shipit-review-revision.el ends here
