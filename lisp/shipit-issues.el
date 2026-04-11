;;; shipit-issues.el --- GitHub Issues search and viewing -*- lexical-binding: t; -*-

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
;; GitHub Issues search and viewing for shipit.
;; Provides quick search, advanced search transient, and completing-read selection.

;;; Code:

(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-issue-backends)
(require 'shipit-commands)
(require 'transient)

;; Forward declarations
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit--format-time-ago "shipit-notifications")
(declare-function shipit-editor-open "shipit-editor")
(declare-function shipit-gh-etag-get-json-paginated "shipit-gh-etag")
(declare-function shipit--read-github-user "shipit-pr-github")
(declare-function shipit-issues--fetch-watched-repos "shipit-issue-github")

;;; API Functions — thin wrappers delegating to active backend

(defun shipit-issues--fetch-issue (repo issue-number)
  "Fetch issue ISSUE-NUMBER from REPO via the active backend."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :fetch-issue) config issue-number)))

(defun shipit-issues--fetch-comments (repo issue-number)
  "Fetch all comments for issue ISSUE-NUMBER from REPO."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :fetch-comments) config issue-number)))

(defun shipit-issues--fetch-comments-async (repo issue-number callback)
  "Fetch comments for issue ISSUE-NUMBER from REPO asynchronously.
Calls CALLBACK with the list of comments when done."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :fetch-comments-async) config issue-number callback)))

(defun shipit-issues--add-comment (repo issue-id body)
  "Add a comment to issue ISSUE-ID in REPO via the active backend.
BODY is the comment text.  Returns the created comment alist."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :add-comment)))
    (unless fn
      (error "Backend does not support :add-comment"))
    (funcall fn config issue-id body)))

(defun shipit-issues--edit-comment (repo comment-id body)
  "Edit comment COMMENT-ID in REPO via the active backend.
BODY is the new comment text.  Returns the updated comment alist."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :edit-comment)))
    (unless fn
      (error "Backend does not support :edit-comment"))
    (funcall fn config comment-id body)))

(defun shipit-issues--toggle-reaction (repo comment-id reaction)
  "Toggle REACTION on comment COMMENT-ID in REPO via the active backend.
Returns the reaction alist or nil."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :toggle-reaction)))
    (unless fn
      (error "Backend does not support :toggle-reaction"))
    (funcall fn config comment-id reaction)))

(defun shipit-issues--update-description (repo issue-number body)
  "Update the description of issue ISSUE-NUMBER in REPO via the active backend.
BODY is the new description text."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :update-description)))
    (unless fn
      (error "Backend does not support :update-description"))
    (funcall fn config issue-number body)))

(defun shipit-issues--fetch-reactions (repo issue-number)
  "Fetch reactions for issue ISSUE-NUMBER in REPO via the active backend.
Caches the result in `shipit--reaction-cache' with key \"pr-ISSUE-NUMBER\"."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :fetch-reactions)))
    (when fn
      (let* ((reactions (funcall fn config issue-number))
             (cache-key (format "pr-%s" issue-number)))
        (puthash cache-key (or reactions '()) shipit--reaction-cache)
        (or reactions '())))))

(defun shipit-issues--fetch-reactions-async (repo issue-number callback)
  "Fetch reactions for ISSUE-NUMBER in REPO asynchronously.
Populates `shipit--reaction-cache' with key \"pr-ISSUE-NUMBER\" when
the fetch completes, then calls CALLBACK with the list of reactions.
Falls back to the synchronous fetch when the backend does not provide
an async implementation.  Returns nil; the result flows via CALLBACK."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (async-fn (plist-get backend :fetch-reactions-async))
         (cache-key (format "pr-%s" issue-number))
         (wrapped-callback (lambda (reactions)
                             (puthash cache-key (or reactions '())
                                      shipit--reaction-cache)
                             (when callback
                               (funcall callback (or reactions '()))))))
    (cond
     (async-fn
      (funcall async-fn config issue-number wrapped-callback))
     ((plist-get backend :fetch-reactions)
      ;; Fall back to sync fetch wrapped in a callback.
      (funcall wrapped-callback
               (shipit-issues--fetch-reactions repo issue-number))))))

(defun shipit-issues--add-reaction (repo issue-number reaction)
  "Add REACTION to issue ISSUE-NUMBER in REPO via the active backend.
Returns the reaction alist."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :add-reaction)))
    (unless fn
      (error "Backend does not support :add-reaction"))
    (funcall fn config issue-number reaction)))

(defun shipit-issues--remove-reaction (repo issue-number reaction-id)
  "Remove reaction REACTION-ID from issue ISSUE-NUMBER in REPO."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :remove-reaction)))
    (unless fn
      (error "Backend does not support :remove-reaction"))
    (funcall fn config issue-number reaction-id)))

(defun shipit-issues--get-transitions (repo issue-id)
  "Fetch available status transitions for ISSUE-ID in REPO.
Returns a list of ((id . ID) (name . NAME)) alists, or nil if unsupported."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (shipit-issue--get-transitions backend config issue-id)))

(defun shipit-issues--transition-status (repo issue-id transition-id)
  "Transition ISSUE-ID in REPO to status via TRANSITION-ID."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (shipit-issue--transition-status backend config issue-id transition-id)))

(defun shipit-issues--create-issue (repo title body)
  "Create a new issue in REPO with TITLE and BODY via the active backend.
Returns the created issue alist."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved)))
    (funcall (plist-get backend :create-issue) config title body)))

(defun shipit-issues--create-issue-extended (repo fields)
  "Create a new issue in REPO with extended FIELDS via the active backend.
FIELDS is an alist with keys like title, body, labels, assignees, etc.
Prefers :create-issue-extended when available, falls back to :create-issue."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (extended-fn (plist-get backend :create-issue-extended)))
    (if extended-fn
        (funcall extended-fn config fields)
      (shipit--debug-log "Backend lacks :create-issue-extended, falling back to simple create")
      (funcall (plist-get backend :create-issue)
               config
               (cdr (assq 'title fields))
               (or (cdr (assq 'body fields)) "")))))

;;; Search Query Building

(defun shipit-issues--build-search-query (args repo)
  "Build GitHub search query from transient ARGS for REPO.
Similar to `shipit--build-advanced-search-query' but uses is:issue
and drops PR-only filters (reviewer, draft, conflicts)."
  (let ((query-parts (list (format "repo:%s" repo) "is:issue")))
    (dolist (arg args)
      (cond
       ((string-prefix-p "--author=" arg)
        (let ((author (substring arg 9)))
          (when (> (length author) 0)
            (let* ((resolved (shipit--resolve-user-placeholder author))
                   (clean (if (string-match "^[0-9]+\\+\\(.+\\)$" resolved)
                              (match-string 1 resolved)
                            resolved)))
              (push (format "author:%s" clean) query-parts)))))
       ((string-prefix-p "--assignee=" arg)
        (let ((assignee (substring arg 11)))
          (when (> (length assignee) 0)
            (let* ((resolved (shipit--resolve-user-placeholder assignee))
                   (clean (if (string-match "^[0-9]+\\+\\(.+\\)$" resolved)
                              (match-string 1 resolved)
                            resolved)))
              (push (format "assignee:%s" clean) query-parts)))))
       ((string-prefix-p "--mentions=" arg)
        (let ((user (substring arg 11)))
          (when (> (length user) 0)
            (let* ((resolved (shipit--resolve-user-placeholder user))
                   (clean (if (string-match "^[0-9]+\\+\\(.+\\)$" resolved)
                              (match-string 1 resolved)
                            resolved)))
              (push (format "mentions:%s" clean) query-parts)))))
       ((string-prefix-p "--milestone=" arg)
        (let ((milestone (substring arg 12)))
          (when (> (length milestone) 0)
            (push (format "milestone:\"%s\"" milestone) query-parts))))
       ((string-prefix-p "--label=" arg)
        (let ((label (substring arg 8)))
          (when (> (length label) 0)
            (push (format "label:\"%s\"" label) query-parts))))
       ((string-prefix-p "--state=" arg)
        (let ((state (substring arg 8)))
          (unless (string= state "all")
            (push (format "state:%s" state) query-parts))))
       ((string-prefix-p "--title=" arg)
        (let ((title (substring arg 8)))
          (when (> (length title) 0)
            (push (format "%s in:title" title) query-parts))))
       ((string-prefix-p "--body=" arg)
        (let ((body (substring arg 7)))
          (when (> (length body) 0)
            (push (format "%s in:body" body) query-parts))))
       ((string-prefix-p "--number=" arg)
        (let ((number (substring arg 9)))
          (when (> (length number) 0)
            (push number query-parts))))
       ((string-prefix-p "--created-after=" arg)
        (let ((date (substring arg 16)))
          (when (> (length date) 0)
            (push (format "created:>%s" date) query-parts))))
       ((string-prefix-p "--created-before=" arg)
        (let ((date (substring arg 17)))
          (when (> (length date) 0)
            (push (format "created:<%s" date) query-parts))))
       ((string-prefix-p "--updated-after=" arg)
        (let ((date (substring arg 16)))
          (when (> (length date) 0)
            (push (format "updated:>%s" date) query-parts))))
       ((string-prefix-p "--updated-before=" arg)
        (let ((date (substring arg 17)))
          (when (> (length date) 0)
            (push (format "updated:<%s" date) query-parts))))
       ((string= "--draft" arg) nil)
       ((string= "--conflicts" arg) nil)
       ((string-prefix-p "--reviewer=" arg) nil)
       ((string-prefix-p "--team-reviewer=" arg) nil)))
    query-parts))

;;; Completion and Selection

(defvar shipit-issues--completion-table nil
  "Hash table mapping issue candidates to their data for annotation.")

(defun shipit-issues--annotate (candidate)
  "Return annotation for issue CANDIDATE."
  (when shipit-issues--completion-table
    (let* ((info (gethash candidate shipit-issues--completion-table))
           (data (plist-get info :data)))
      (when data
        (let* ((user (cdr (assq 'login (cdr (assq 'user data)))))
               (state (cdr (assq 'state data)))
               (updated (cdr (assq 'updated_at data)))
               (labels (cdr (assq 'labels data)))
               (label-names (when labels
                              (mapcar (lambda (l) (cdr (assq 'name l))) labels)))
               (comments (cdr (assq 'comments data)))
               (time-ago (if (and updated (fboundp 'shipit--format-time-ago))
                             (shipit--format-time-ago updated)
                           ""))
               (labels-str (when label-names
                             (mapconcat #'identity (seq-take label-names 3) ",")))
               (comments-str (if (and comments (> comments 0))
                                 (format "💬%d" comments)
                               ""))
               (user-str (if user (format "@%s" user) ""))
               (state-str (capitalize (or state "unknown")))
               (state-face (if (fboundp 'shipit-issue--state-face)
                               (shipit-issue--state-face state)
                             (if (string= state "open") 'success 'error))))
          (let ((parts '()))
            (when (not (string-empty-p user-str))
              (push (propertize user-str 'face 'font-lock-function-name-face) parts))
            (when (not (string-empty-p state-str))
              (push (propertize state-str 'face state-face) parts))
            (when (not (string-empty-p comments-str))
              (push (propertize comments-str 'face 'font-lock-comment-face) parts))
            (when (and time-ago (not (string-empty-p time-ago)))
              (push (propertize time-ago 'face 'font-lock-comment-face) parts))
            (when labels-str
              (push (propertize labels-str 'face 'font-lock-keyword-face) parts))
            (when parts
              (concat " " (string-join (nreverse parts) " - ")))))))))

(defun shipit-issues--format-issue-id (issue-id)
  "Format ISSUE-ID for display.  Handles both integers and strings."
  (if (integerp issue-id)
      (format "#%-5d" issue-id)
    (format "%-10s" issue-id)))

(defun shipit-issues--candidate-sort-key (candidate)
  "Extract numeric sort key from CANDIDATE string.
Parses the leading issue ID from the candidate display text."
  (cond
   ((string-match "\\`#\\([0-9]+\\)" candidate)
    (string-to-number (match-string 1 candidate)))
   ((string-match "\\`[A-Z][A-Z0-9]+-\\([0-9]+\\)" candidate)
    (string-to-number (match-string 1 candidate)))
   (t 0)))

(defun shipit-issues--sort-candidates (candidates)
  "Sort issue CANDIDATES by numeric key descending."
  (sort (copy-sequence candidates)
        (lambda (a b)
          (> (shipit-issues--candidate-sort-key a)
             (shipit-issues--candidate-sort-key b)))))

(defun shipit-issues--sort-key (number)
  "Extract a numeric sort key from issue NUMBER.
For integers, returns the integer directly.
For Jira keys like \"PRJ-42\", extracts the numeric suffix."
  (cond
   ((integerp number) number)
   ((and (stringp number) (string-match "-\\([0-9]+\\)\\'" number))
    (string-to-number (match-string 1 number)))
   (t 0)))

(defun shipit-issues--build-candidates (issues repo lookup-table)
  "Build completion candidates from ISSUES for REPO.
Populates LOOKUP-TABLE and returns a list of candidate strings."
  (let ((sorted (sort (copy-sequence issues)
                      (lambda (a b)
                        (> (shipit-issues--sort-key (cdr (assq 'number a)))
                           (shipit-issues--sort-key (cdr (assq 'number b))))))))
    (mapcar (lambda (issue)
              (let* ((number (cdr (assq 'number issue)))
                     (title (string-trim (or (cdr (assq 'title issue)) "")))
                     (candidate (format "%s %s"
                                        (shipit-issues--format-issue-id number)
                                        title)))
                (puthash candidate
                         (list :number number :repo repo :data issue)
                         lookup-table)
                candidate))
            sorted)))

(defun shipit-issues--match-string (string config)
  "Normalize STRING for candidate matching.
When STRING contains a ticket number, transforms to match the
candidate prefix format (e.g. 42 → #42 or PRJ-42)."
  (let ((number (shipit-issues--extract-number string)))
    (cond
     ;; No number found — plain text, pass through
     ((null number) string)
     ;; Already a Jira key (contains letters) — use as-is
     ((string-match-p "[A-Z]" number) number)
     ;; Bare digits + Jira project keys — construct key
     ((plist-get config :project-keys)
      (format "%s-%s" (car (plist-get config :project-keys)) number))
     ;; Bare digits + GitHub — prefix with #
     (t (format "#%s" number)))))

(defun shipit-issues--extract-number (string)
  "Extract a ticket number from STRING, or nil if none found.
Checks for exact matches first, then embedded patterns.
Returns the raw number/key string (e.g. \"PRJ-42\", \"123\")."
  (cond
   ;; Exact Jira key
   ((string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" string)
    string)
   ;; Exact #number
   ((string-match "\\`#\\([0-9]+\\)\\'" string)
    (match-string 1 string))
   ;; Exact bare digits
   ((string-match-p "\\`[0-9]+\\'" string)
    string)
   ;; Embedded Jira key
   ((string-match "\\b\\([A-Z][A-Z0-9]+-[0-9]+\\)" string)
    (match-string 1 string))
   ;; Embedded #number
   ((string-match "#\\([0-9]+\\)" string)
    (match-string 1 string))
   ;; Embedded bare digits (2+ digits to avoid matching single-digit words)
   ((string-match "\\b\\([0-9][0-9]+\\)\\b" string)
    (match-string 1 string))))

(defun shipit-issues--dynamic-search-args (string)
  "Return search args list for dynamic completion input STRING.
Detects Jira keys (PRJ-42), GitHub numbers (#123), bare digits (42),
or embedded numbers in mixed text.  Falls back to title search."
  (let ((number (shipit-issues--extract-number string)))
    (if number
        (list (format "--number=%s" number) "--limit=50")
      (list (format "--title=%s" string) "--limit=50"))))

(defun shipit-issues--dynamic-fetch (backend config string)
  "Fetch issues matching STRING via the best available method.
For number/key inputs, fetches the issue directly via :fetch-issue.
For bare digits with :project-keys in CONFIG, constructs the full
Jira key (e.g. 42 -> PRJ-42).
For text inputs, searches via the backend's :search function."
  (let ((args (shipit-issues--dynamic-search-args string)))
    (if (cl-find-if (lambda (a) (string-prefix-p "--number=" a)) args)
        (let* ((number-arg (cl-find-if (lambda (a) (string-prefix-p "--number=" a)) args))
               (id (substring number-arg 9))
               (project-keys (plist-get config :project-keys))
               (fetch-id (cond
                          ((and (string-match-p "\\`[0-9]+\\'" id) project-keys)
                           (format "%s-%s" (car project-keys) id))
                          ((string-match-p "\\`[0-9]+\\'" id)
                           (string-to-number id))
                          (t id)))
               (issue (condition-case nil
                          (funcall (plist-get backend :fetch-issue) config fetch-id)
                        (error nil))))
          (when issue (list issue)))
      (condition-case nil
          (funcall (plist-get backend :search) config args)
        (error nil)))))

(defvar shipit-issues--dynamic-search-cache nil
  "Cache of (QUERY . CANDIDATES) for dynamic search.")

(defun shipit-issues--select-from-results (results repo &optional resolved)
  "Present issue RESULTS from REPO in completing-read for selection.
When RESOLVED is non-nil (a backend+config cons from `shipit-issue--resolve-for-repo'),
re-queries the API dynamically as the user types."
  (if (or (null results) (= (length results) 0))
      (message "No issues found matching the specified criteria")
    (let* ((lookup-table (make-hash-table :test 'equal))
           (initial-candidates (shipit-issues--build-candidates results repo lookup-table))
           (all-candidates initial-candidates)
           (search-cache (make-hash-table :test 'equal))
           (completion-fn
            (lambda (string predicate action)
              (cond
               ((eq action 'metadata)
                `(metadata (display-sort-function . shipit-issues--sort-candidates)
                           (cycle-sort-function . identity)
                           (annotation-function . shipit-issues--annotate)
                           (category . shipit-issue)))
               (t
                ;; When user typed enough chars and we have a backend, query the API.
                ;; Digit-only input uses direct fetch (cheap), so threshold is 1.
                ;; Text search uses the search API, so threshold is 3.
                (when (and resolved
                          (>= (length string)
                               (if (string-match-p "\\`[#0-9]" string) 1 3))
                          (not (gethash string search-cache)))
                  (puthash string t search-cache)
                  (let* ((backend (car resolved))
                         (config (cdr resolved))
                         (new-results (shipit-issues--dynamic-fetch
                                       backend config string)))
                    (when new-results
                      (let ((new-candidates (shipit-issues--build-candidates
                                             new-results repo lookup-table)))
                        (dolist (c new-candidates)
                          (unless (member c all-candidates)
                            (push c all-candidates)))))))
                (let ((match-str (if (and resolved (eq action t))
                                     (shipit-issues--match-string
                                      string (cdr resolved))
                                   string)))
                  (complete-with-action action all-candidates match-str predicate)))))))
      (setq shipit-issues--completion-table lookup-table)
      (unwind-protect
          (let ((selected (completing-read "Select issue (type to filter): "
                                           completion-fn nil t)))
            (when selected
              (let ((info (gethash selected lookup-table)))
                (if info
                    (shipit-issues-open-buffer (plist-get info :number) repo)
                  (message "Could not extract issue from selection")))))
        (setq shipit-issues--completion-table nil)))))

;;; Search Execution

(defun shipit-issues--display-search-results (repo args)
  "Search for issues in REPO with transient ARGS via the active backend."
  (let* ((resolved (shipit-issue--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (results (funcall (plist-get backend :search) config args)))
    (shipit--debug-log "Issue search: results=%d"
                       (if results (length results) 0))
    (if (and results (> (length results) 0))
        (progn
          (message "Found %d issues matching criteria" (length results))
          (shipit-issues--select-from-results results repo resolved))
      (message "No issues found matching the specified criteria"))))

(defun shipit-issues--read-repo (_prompt _initial-input _history)
  "Read a repo name with completion.
Candidates come from the current repo, GitHub watched repos,
`shipit-issue-subscribed-repos', and `shipit-issue-repo-backends'.
Free-form input is also accepted."
  (let* ((current (ignore-errors (shipit--get-repo-from-remote)))
         (watched (shipit-issues--fetch-watched-repos))
         (subscribed (copy-sequence shipit-issue-subscribed-repos))
         (backend-repos (cl-remove-if
                        (lambda (k) (not (string= k (regexp-quote k))))
                        (mapcar #'car shipit-issue-repo-backends)))
         (all (delete-dups
               (cl-remove-if #'null
                             (append (when current (list current))
                                     watched
                                     subscribed
                                     backend-repos)))))
    (completing-read "Repo (owner/name): " all nil nil
                     (or current ""))))

(defun shipit-issues--execute-advanced-search (&optional args)
  "Execute advanced issue search with ARGS from transient menu."
  (interactive (list (transient-args 'shipit-advanced-issue-search)))
  (shipit--debug-log "Advanced issue search raw args: %S" args)
  (let* ((repo-arg (cl-find-if (lambda (a) (string-prefix-p "--repo=" a)) args))
         (repo (or (and repo-arg (substring repo-arg 7))
                   (shipit--get-repo-from-remote)))
         (search-args (cl-remove-if (lambda (a) (string-prefix-p "--repo=" a)) args)))
    (if repo
        (shipit-issues--display-search-results repo search-args)
      (message "No repo specified and could not detect from remote"))))

;;;###autoload
(defun shipit-issues--quick-search ()
  "Quick search for issues in the current repository."
  (interactive)
  (let* ((repo (shipit--get-repo-from-remote))
         (args '("--limit=100" "--sort=created")))
    (if repo
        (shipit-issues--display-search-results repo args)
      (message "Could not determine repository from remote"))))

;;; Issue Creation

(declare-function shipit-issue-create-buffer "shipit-issue-create")

;;;###autoload
(defun shipit-create-issue ()
  "Create a new issue via the active backend.
When the backend supports rich creation fields, opens a section-based
creation buffer.  Otherwise falls back to the plain-text editor."
  (interactive)
  (let ((repo (shipit--get-repo-from-remote)))
    (unless repo
      (user-error "Could not determine repository from remote"))
    (let* ((resolved (shipit-issue--resolve-for-repo repo))
           (backend (car resolved)))
      (if (plist-get backend :creation-fields)
          (shipit-issue-create-buffer)
        (shipit-editor-open
         (list :type 'create-issue
               :repo repo))))))

;;; Transient Menu

(transient-define-prefix shipit-advanced-issue-search ()
  "Advanced issue search with multiple filter options."
  :man-page "shipit"
  :value '("--state=open" "--limit=100" "--sort=created")
  ["Repository"
   ("r" "Repo" "--repo=" :reader shipit-issues--read-repo)]
  ["Presets"
   ("p o" "Open issues" transient-preset := ("--state=open"))
   ("p m" "My open issues" transient-preset := ("--state=open" "--author=@me"))
   ("p a" "Assigned to me" transient-preset := ("--state=open" "--assignee=@me"))
   ("p n" "Mentioned me" transient-preset := ("--state=open" "--mentions=@me"))]
  ["Filters"
   ("a" "Author" "--author=" :reader shipit--read-github-user)
   ("A" "Assignee" "--assignee=" :reader shipit--read-github-user)
   ("M" "Mentions" "--mentions=" :reader shipit--read-github-user)
   ("m" "Milestone" "--milestone=" :reader read-string)
   ("l" "Label" "--label=" :reader read-string)]
  ["State"
   ("s" "State" "--state=" :choices ("open" "closed" "all"))]
  ["Text Search"
   ("t" "Title contains" "--title=" :reader read-string)
   ("b" "Body contains" "--body=" :reader read-string)
   ("n" "Issue Number" "--number=" :reader read-string)]
  ["Date Range"
   (">" "Created after" "--created-after=" :reader read-string)
   ("<" "Created before" "--created-before=" :reader read-string)
   ("u" "Updated after" "--updated-after=" :reader read-string)
   ("U" "Updated before" "--updated-before=" :reader read-string)]
  ["Options"
   ("S" "Sort by" "--sort=" :choices ("created" "updated" "comments"))
   ("L" "Limit results" "--limit=" :reader transient-read-number-N+)]
  ["Actions"
   ("RET" "Search" shipit-issues--execute-advanced-search)
   ("c" "Create issue" shipit-create-issue)
   ("q" "Quit" transient-quit-one)])

(provide 'shipit-issues)
;;; shipit-issues.el ends here
