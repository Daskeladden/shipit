;;; shipit-discussions.el --- GitHub Discussions search and viewing -*- lexical-binding: t; -*-

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
;; GitHub Discussions search and selection for shipit.
;; Provides quick search, advanced search transient, and completing-read selection.

;;; Code:

(require 'shipit-core)
(require 'shipit-discussions-graphql)
(require 'transient)

;; Forward declarations
(declare-function shipit-discussions-open-buffer "shipit-discussions-buffer")
(declare-function shipit--format-time-ago "shipit-notifications")
(declare-function shipit-editor-open "shipit-editor")
(declare-function shipit-issues--read-repo "shipit-issues")
(declare-function shipit--read-github-user "shipit-pr-github")

;;; Completion and Selection

(defvar shipit-discussions--completion-table nil
  "Hash table mapping discussion candidates to their data for annotation.")

(defun shipit-discussions--annotate (candidate)
  "Return annotation for discussion CANDIDATE."
  (when shipit-discussions--completion-table
    (let* ((info (gethash candidate shipit-discussions--completion-table))
           (data (plist-get info :data)))
      (when data
        (let* ((user (cdr (assq 'login (cdr (assq 'user data)))))
               (category (cdr (assq 'category data)))
               (cat-name (cdr (assq 'name category)))
               (cat-emoji (or (cdr (assq 'emoji category)) ""))
               (is-answered (cdr (assq 'is_answered data)))
               (upvotes (or (cdr (assq 'upvote_count data)) 0))
               (comments (or (cdr (assq 'comments_count data)) 0))
               (updated (cdr (assq 'updated_at data)))
               (time-ago (if (and updated (fboundp 'shipit--format-time-ago))
                             (shipit--format-time-ago updated)
                           ""))
               (parts '()))
          (when (and user (not (string-empty-p user)))
            (push (propertize (format "@%s" user)
                              'face 'font-lock-function-name-face)
                  parts))
          (when cat-name
            (push (propertize (format "%s %s" cat-emoji cat-name)
                              'face 'font-lock-type-face)
                  parts))
          (push (propertize (if is-answered "Answered" "Open")
                            'face (if is-answered 'success 'warning))
                parts)
          (when (> upvotes 0)
            (push (propertize (format "👍%d" upvotes)
                              'face 'font-lock-constant-face)
                  parts))
          (when (> comments 0)
            (push (propertize (format "💬%d" comments)
                              'face 'font-lock-comment-face)
                  parts))
          (when (and time-ago (not (string-empty-p time-ago)))
            (push (propertize time-ago 'face 'font-lock-comment-face)
                  parts))
          (when parts
            (concat " " (string-join (nreverse parts) " - "))))))))

(defun shipit-discussions--build-candidates (results repo lookup-table)
  "Build completion candidates from discussion RESULTS for REPO.
Populates LOOKUP-TABLE and returns a list of candidate strings."
  (let ((sorted (sort (copy-sequence results)
                      (lambda (a b)
                        (> (or (cdr (assq 'number a)) 0)
                           (or (cdr (assq 'number b)) 0))))))
    (mapcar (lambda (discussion)
              (let* ((number (cdr (assq 'number discussion)))
                     (title (string-trim
                             (or (cdr (assq 'title discussion)) "")))
                     (candidate (format "#%-5d %s" number title)))
                (puthash candidate
                         (list :number number :repo repo :data discussion)
                         lookup-table)
                candidate))
            sorted)))

(defun shipit-discussions--select-from-results (results repo)
  "Present discussion RESULTS from REPO in completing-read for selection."
  (if (or (null results) (= (length results) 0))
      (message "No discussions found matching the specified criteria")
    (let* ((lookup-table (make-hash-table :test 'equal))
           (candidates (shipit-discussions--build-candidates
                        results repo lookup-table))
           (completion-fn
            (lambda (string predicate action)
              (cond
               ((eq action 'metadata)
                `(metadata (annotation-function
                            . shipit-discussions--annotate)
                           (category . shipit-discussion)))
               (t
                (complete-with-action
                 action candidates string predicate))))))
      (setq shipit-discussions--completion-table lookup-table)
      (unwind-protect
          (let ((selected (completing-read "Select discussion: "
                                           completion-fn nil t)))
            (when selected
              (let ((info (or (gethash selected lookup-table)
                              (shipit-discussions--lookup-by-number
                               selected lookup-table))))
                (if info
                    (shipit-discussions-open-buffer
                     (plist-get info :number) repo)
                  (message "Could not extract discussion from selection")))))
        (setq shipit-discussions--completion-table nil)))))

(defun shipit-discussions--lookup-by-number (selected lookup-table)
  "Find discussion info in LOOKUP-TABLE by extracting number from SELECTED."
  (when (string-match "#\\([0-9]+\\)" selected)
    (let ((number (string-to-number (match-string 1 selected)))
          (found nil))
      (maphash (lambda (_key value)
                 (when (equal (plist-get value :number) number)
                   (setq found value)))
               lookup-table)
      found)))

;;; Search Execution

(defun shipit-discussions--display-search-results (repo args)
  "Search for discussions in REPO with transient ARGS."
  (let ((results (shipit-discussion--search repo args)))
    (shipit--debug-log "Discussion search: results=%d"
                       (if results (length results) 0))
    (if (and results (> (length results) 0))
        (progn
          (message "Found %d discussions matching criteria"
                   (length results))
          (shipit-discussions--select-from-results results repo))
      (message "No discussions found matching the specified criteria"))))

(defun shipit-discussions--execute-search (&optional args)
  "Execute advanced discussion search with ARGS from transient menu."
  (interactive (list (transient-args 'shipit-advanced-discussion-search)))
  (shipit--debug-log "Advanced discussion search raw args: %S" args)
  (let* ((repo-arg (cl-find-if
                    (lambda (a) (string-prefix-p "--repo=" a)) args))
         (repo (or (and repo-arg (substring repo-arg 7))
                   (shipit--get-repo-from-remote)))
         (search-args (cl-remove-if
                       (lambda (a) (string-prefix-p "--repo=" a)) args)))
    (if repo
        (shipit-discussions--display-search-results repo search-args)
      (message "No repo specified and could not detect from remote"))))

;;;###autoload
(defun shipit-discussions--quick-search ()
  "Quick search for discussions in the current repository."
  (interactive)
  (let* ((repo (shipit--get-repo-from-remote))
         (args '("--limit=50")))
    (if repo
        (shipit-discussions--display-search-results repo args)
      (message "Could not determine repository from remote"))))

;;; Category Reader

(defun shipit-discussions--read-category (_prompt _initial-input _history)
  "Read a discussion category with completion.
Fetches categories from the current repo's GraphQL API."
  (let* ((repo (or (ignore-errors (shipit--get-repo-from-remote)) ""))
         (categories (when (not (string-empty-p repo))
                       (condition-case nil
                           (shipit-discussion--fetch-categories repo)
                         (error nil))))
         (names (mapcar (lambda (c)
                          (let ((name (cdr (assq 'name c)))
                                (emoji (or (cdr (assq 'emoji c)) "")))
                            (format "%s %s" emoji name)))
                        (or categories '()))))
    (let ((selected (completing-read "Category: " names nil nil)))
      ;; Strip the leading emoji + space from the selection
      (if (string-match "\\`.+? \\(.+\\)\\'" selected)
          (match-string 1 selected)
        selected))))

;;; Discussion Creation

;;;###autoload
(defun shipit-create-discussion ()
  "Create a new GitHub Discussion."
  (interactive)
  (let ((repo (shipit--get-repo-from-remote)))
    (unless repo
      (user-error "Could not determine repository from remote"))
    (shipit-editor-open
     (list :type 'create-discussion
           :repo repo))))

;;; Transient Menu

(transient-define-prefix shipit-advanced-discussion-search ()
  "Advanced discussion search with multiple filter options."
  :man-page "shipit"
  :value '("--limit=50")
  ["Repository"
   ("r" "Repo" "--repo=" :reader shipit-issues--read-repo)]
  ["Filters"
   ("a" "Author" "--author=" :reader shipit--read-github-user)
   ("c" "Category" "--category="
    :reader shipit-discussions--read-category)
   ("l" "Label" "--label=" :reader read-string)
   ("A" "Answered" "--answered=" :choices ("yes" "no" "all"))]
  ["Text Search"
   ("t" "Title contains" "--title=" :reader read-string)
   ("b" "Body contains" "--body=" :reader read-string)]
  ["Date Range"
   (">" "Created after" "--created-after=" :reader read-string)
   ("<" "Created before" "--created-before=" :reader read-string)
   ("u" "Updated after" "--updated-after=" :reader read-string)
   ("U" "Updated before" "--updated-before=" :reader read-string)]
  ["Options"
   ("L" "Limit results" "--limit=" :reader transient-read-number-N+)]
  ["Actions"
   ("RET" "Search" shipit-discussions--execute-search)
   ("C" "Create discussion" shipit-create-discussion)
   ("q" "Quit" transient-quit-one)])

(provide 'shipit-discussions)
;;; shipit-discussions.el ends here
