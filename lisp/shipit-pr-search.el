;;; shipit-pr-search.el --- PR search and selection -*- lexical-binding: t; -*-

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
;; PR search, multi-page fetching, and interactive selection via
;; completing-read with dynamic re-query.  Extracted from shipit-magit.el.

;;; Code:
(require 'seq)
(require 'url-util)
(require 'shipit-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)

;; Forward declarations — functions
(declare-function shipit--debug-log "shipit-core")
(declare-function shipit-open-pr-buffer "shipit-buffer")
(declare-function shipit--pr-annotate "shipit-commands")
(declare-function shipit--extract-pr-number-from-candidate "shipit-commands")

;; Forward declarations — variables
(defvar shipit--explicit-pr-operation)
(defvar shipit--pr-completion-table)
(defvar shipit--embark-pr-repo)

(defun shipit--display-selected-pr (pr repo &optional backend-id backend-config)
  "Display the selected PR data using dedicated buffer interface.
BACKEND-ID and BACKEND-CONFIG override the default backend for this buffer."
  (shipit--debug-log "shipit--display-selected-pr called for PR #%s in repo %s (backend: %s)"
                     (cdr (assq 'number pr)) repo (or backend-id "default"))
  ;; Always use dedicated buffer
  (shipit--debug-log "Using dedicated buffer for PR display")
  (require 'shipit-buffer)
  (let ((pr-number (cdr (assq 'number pr))))
    (shipit-open-pr-buffer pr-number repo backend-id backend-config)))

(defun shipit--parse-search-query (query)
  "Parse field syntax and separate from filter terms."
  (if (and query (string-match "^:\\([a-z]+\\) +\\([^ ]+\\)\\(.*\\)$" query))
      (let ((field-name (match-string 1 query))
            (field-value (match-string 2 query))
            (filter-terms (string-trim (match-string 3 query))))
        (list (cons 'field (cons field-name field-value))
              (cons 'filter (if (> (length filter-terms) 0) filter-terms nil))))
    (list (cons 'field nil)
          (cons 'filter (if (and query (> (length query) 0)) query nil)))))

(defun shipit--build-github-query (repo parsed-query)
  "Build GitHub search query from parsed query with + separators."
  (let ((field (cdr (assq 'field parsed-query)))
        (filter-terms (cdr (assq 'filter parsed-query)))
        (query-parts (list (format "repo:%s" (url-hexify-string repo)) "is:pr")))

    (when field
      (let ((field-name (car field))
            (field-value (cdr field)))
        (cond
         ((string= field-name "author")
          (push (format "author:%s" (url-hexify-string field-value)) query-parts))
         ((string= field-name "number")
          (push (url-hexify-string field-value) query-parts)))))

    ;; Add filter terms if provided, restricted to title search
    ;; so results match what the completion framework filters on
    (when filter-terms
      (push (url-hexify-string filter-terms) query-parts)
      (unless field
        (push "in:title" query-parts)))

    (mapconcat 'identity query-parts "+")))

(defun shipit--search-prs-single-page (repo query page &optional per-page)
  "Fetch a single page of PR search results."
  (let ((per-page (or per-page 50))
        (parsed (shipit--parse-search-query query)))
    (condition-case err
        (let* ((github-query (shipit--build-github-query repo parsed))
               (resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (search-result (funcall (plist-get backend :search-raw)
                                       config github-query page per-page))
               (items (cdr (assq 'items search-result))))
          (shipit--debug-log "Single page fetch - query: '%s', total_count=%s, items=%d"
                             github-query
                             (cdr (assq 'total_count search-result))
                             (if items (length items) 0))
          items)
      (error
       (shipit--debug-log "Error fetching single page: %S" err)
       nil))))

(defun shipit--search-prs-multi-page (repo query max-results)
  "Fetch multiple pages of PR search results up to MAX-RESULTS."
  (let ((per-page 50)
        (page 1)
        (all-results '())
        (continue t))

    (shipit--debug-log "Multi-page fetch starting: repo=%s, query=%s, max-results=%d" repo query max-results)

    (while (and continue (< (length all-results) max-results))
      (let ((page-results (shipit--search-prs-single-page repo query page per-page)))
        (shipit--debug-log "Multi-page fetch page %d: got %d results" page (if page-results (length page-results) 0))

        (if (and page-results (> (length page-results) 0))
            (progn
              (setq all-results (append all-results page-results))
              (setq page (1+ page))
              ;; Stop if we got less than a full page (no more results)
              (when (< (length page-results) per-page)
                (setq continue nil)))
          (setq continue nil))))

    (let ((final-results (seq-take all-results max-results)))
      (shipit--debug-log "Multi-page fetch complete: %d pages, %d total results, returning %d"
                         (1- page) (length all-results) (length final-results))
      final-results)))

(defvar shipit--completion-cache nil
  "Legacy completion cache - will be phased out in favor of global cache.
Structure: (cache-key . (raw-results formatted-candidates reserved page timestamp))")

(defun shipit--format-pr-choice (pr)
  "Format a PR for display in completion.
Shows only PR number and title - author/status shown in annotations."
  (let ((number (cdr (assq 'number pr)))
        (title (cdr (assq 'title pr))))
    ;; Don't truncate title - show full title for better readability
    (format "#%-5d %s" number (string-trim (or (shipit--clean-text title) "")))))

;;;###autoload
(defun shipit--select-pr ()
  "Select and display a PR with dynamic completion and merge-based search.
Fetches initial PRs upfront, then dynamically merges new API results
as the user types 3+ characters."
  (interactive)
  (setq shipit--explicit-pr-operation t)
  (shipit--debug-log "PR selection invoked via 's' key")
  (let ((repo (shipit--ensure-repository)))
    (if (not repo)
        (message "Could not determine repository")
      (message "Searching PRs...")
      (let* ((initial-results (shipit--search-prs-multi-page repo "" 100))
             (pr-lookup-table (make-hash-table :test 'equal))
             (all-candidates (shipit--select-pr-build-candidates
                              initial-results repo pr-lookup-table))
             (search-cache (make-hash-table :test 'equal))
             (completion-fn
              (shipit--select-pr-completion-fn
               repo all-candidates pr-lookup-table search-cache)))
        (message "Found %d PRs" (length all-candidates))
        (setq shipit--pr-completion-table pr-lookup-table)
        (setq shipit--embark-pr-repo repo)
        (unwind-protect
            (let ((selected (completing-read "Select PR: " completion-fn nil t)))
              (shipit--select-pr-handle-selection
               selected pr-lookup-table repo))
          (setq shipit--pr-completion-table nil))))))

(defun shipit--select-pr-build-candidates (results repo lookup-table)
  "Build candidate strings from RESULTS for REPO, populating LOOKUP-TABLE.
Returns list of formatted candidate strings sorted by PR number descending."
  (let ((sorted (sort (copy-sequence results)
                      (lambda (a b)
                        (> (or (cdr (assq 'number a)) 0)
                           (or (cdr (assq 'number b)) 0)))))
        (candidates '()))
    (dolist (pr sorted)
      (let ((candidate (shipit--format-pr-choice pr))
            (number (cdr (assq 'number pr))))
        (unless (gethash candidate lookup-table)
          (puthash candidate (list :number number :repo repo :data pr) lookup-table)
          (push candidate candidates))))
    (nreverse candidates)))

(defun shipit--select-pr-completion-fn (repo all-candidates pr-lookup-table search-cache)
  "Create completion function for PR selection.
REPO is the repository. ALL-CANDIDATES is the mutable candidate list.
PR-LOOKUP-TABLE maps candidates to PR data. SEARCH-CACHE prevents duplicate API calls."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (annotation-function . shipit--pr-annotate)
                   (category . shipit-pr))
      ;; Dynamic re-query: when 3+ chars typed and not already cached
      (when (and (>= (length string) 3)
                 (not (gethash string search-cache)))
        (puthash string t search-cache)
        (let ((new-results (shipit--search-prs-single-page repo string 1 50)))
          (when new-results
            (dolist (pr new-results)
              (let ((candidate (shipit--format-pr-choice pr))
                    (number (cdr (assq 'number pr))))
                (unless (gethash candidate pr-lookup-table)
                  (puthash candidate (list :number number :repo repo :data pr)
                           pr-lookup-table)
                  (push candidate all-candidates)))))))
      (complete-with-action action all-candidates string predicate))))

(defun shipit--select-pr-handle-selection (selected pr-lookup-table repo)
  "Handle SELECTED candidate from completing-read.
Looks up PR data in PR-LOOKUP-TABLE, falls back to API fetch for REPO."
  (when (and selected (not (string-empty-p selected)))
    (let* ((pr-number (shipit--extract-pr-number-from-candidate selected))
           (pr-info (when pr-number
                      (catch 'found
                        (maphash (lambda (_key val)
                                   (when (= (plist-get val :number) pr-number)
                                     (throw 'found val)))
                                 pr-lookup-table)
                        nil)))
           (pr-data (plist-get pr-info :data))
           ;; Fall back to API if not in lookup table
           (pr (or pr-data
                   (when pr-number
                     (let* ((resolved (shipit-pr--resolve-for-repo repo))
                            (backend (car resolved))
                            (config (cdr resolved)))
                       (funcall (plist-get backend :fetch-pr) config pr-number))))))
      (cond
       ((not pr-number)
        (message "Could not extract PR number from selection"))
       ((not pr)
        (message "Could not fetch PR #%d" pr-number))
       (t
        (shipit--display-selected-pr pr repo))))))

(provide 'shipit-pr-search)
;;; shipit-pr-search.el ends here
