;;; shipit-repo-buffer.el --- Repository landing page buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

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
;; Dedicated buffer for viewing repository information — header metadata
;; and rendered README.md.  Uses magit sections for collapsible display.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magit-section)
(require 'transient)
(require 'shipit-core)
(require 'shipit-pr-backends)

;; Forward declarations
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--render-body "shipit-render")
(declare-function shipit--format-for-readme-filename "shipit-render")
(declare-function shipit--strip-face-on-whitespace "shipit-render")
(declare-function shipit--detect-body-format "shipit-render")
(declare-function shipit--wrap-text "shipit-core")
(declare-function shipit--create-generic-url-overlays "shipit-render")
(declare-function shipit--apply-code-block-backgrounds-in-region "shipit-render")
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit--create-avatar-display "shipit-render")
(declare-function shipit--insert-body-with-details "shipit-render")
(declare-function shipit--insert-lines "shipit-render")
(declare-function shipit--center-aligned-lines-in-region "shipit-render")
(declare-function shipit--lay-out-table-rows "shipit-render")
(declare-function shipit--join-paragraph-lines "shipit-render")
(declare-function shipit--align-table-columns-in-region "shipit-render")
(declare-function shipit--generic-url-action-menu "shipit-render")
(declare-function shipit--classify-url "shipit-render")
(declare-function shipit--open-classified-url "shipit-render")
(declare-function shipit--watch-videos "shipit-render")

;; Forward declarations — issue/discussion backends (optional features)
(declare-function shipit-issue--resolve-for-repo "shipit-issue-backends")
(declare-function shipit-discussion--search "shipit-discussions-graphql")
(declare-function shipit-discussion--search-page-async "shipit-discussions-graphql")
(declare-function shipit-open-pr-buffer "shipit-buffer")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-discussions-open-buffer "shipit-discussions-buffer")
(declare-function shipit--get-github-username "shipit-lib")
(declare-function shipit-notifications-buffer--rerender "shipit-notifications-buffer")

;; Forward variable declarations
(defvar shipit-render-markdown)
(defvar shipit-show-avatars)
(defvar shipit--image-base-repo)
(defvar shipit-issues-enabled)
(defvar shipit-discussions-enabled)
(defvar shipit-repo-buffer-closed-limit)

;;; Section type definitions

(put 'repo-root 'magit-section t)

(put 'repo-header 'magit-section t)

(put 'repo-readme 'magit-section t)

(put 'repo-languages 'magit-section t)

(put 'repo-prs 'magit-section t)

(put 'repo-prs-open 'magit-section t)

(put 'repo-prs-closed 'magit-section t)

(put 'repo-issues 'magit-section t)

(put 'repo-issues-open 'magit-section t)

(put 'repo-issues-closed 'magit-section t)

(put 'repo-discussions 'magit-section t)

;;; Buffer-local variables

(defcustom shipit-readme-fill-column 80
  "Hard-wrap column for the rendered repo README.
An integer (default 80) hard-wraps via `shipit--wrap-text' so
paragraphs read with proper indentation and continuation
alignment.  nil skips the wrap and lets the display engine
visual-wrap -- cheaper but reads less cleanly on long lines.
The wrap path itself is fast now: `fill-region-as-paragraph'
runs with adaptive-fill / change-hooks / paragraph-detection
bypassed, so a long README doesn't show up as a redisplay hot
path."
  :type '(choice (const :tag "Visual wrap (no fill)" nil) integer)
  :group 'shipit)

(defvar-local shipit-repo-buffer-repo nil
  "Repository name (owner/repo) displayed in this buffer.")

(defvar-local shipit-repo-buffer-data nil
  "Cached repo info data for this buffer.")

(defvar-local shipit-repo-buffer-readme nil
  "Cached README content for this buffer.")

(defvar-local shipit-repo-buffer-languages nil
  "Cached languages alist for this buffer.")

(defvar-local shipit-repo-buffer-local-root nil
  "Local git root directory, if the repo is checked out locally.")

(defvar-local shipit-repo-buffer-open-prs nil
  "Cached open PRs for this buffer.")

(defvar-local shipit-repo-buffer-closed-prs nil
  "Cached closed PRs for this buffer.")

(defvar-local shipit-repo-buffer-closed-prs-has-more nil
  "Non-nil when more closed PRs are available to fetch.")

(defvar-local shipit-repo-buffer-open-prs-has-more nil
  "Non-nil when more open PRs are available to fetch.")

(defvar-local shipit-repo-buffer-open-issues nil
  "Cached open issues for this buffer.")

(defvar-local shipit-repo-buffer-closed-issues nil
  "Cached closed issues for this buffer.")

(defvar-local shipit-repo-buffer-closed-issues-has-more nil
  "Non-nil when more closed issues are available to fetch.")

(defvar-local shipit-repo-buffer-open-issues-has-more nil
  "Non-nil when more open issues are available to fetch.")

(defvar-local shipit-repo-buffer-discussions nil
  "Cached recent discussions for this buffer.")

(defvar-local shipit-repo-buffer--filter-text ""
  "Current filter text for repo buffer items.")

(defvar-local shipit-repo-buffer--filter-mode nil
  "Current filter mode for repo buffer items.
nil means text filter, `mine' means authored-by-me filter.")

(defvar-local shipit-repo-buffer--filter-section-type nil
  "The section type being filtered (repo-prs, repo-issues, or repo-discussions).
Captured when filter is initiated so refresh works from minibuffer context.")

(defvar-local shipit-repo-buffer--my-usernames nil
  "Cached list of names/usernames that identify the current user.
Includes GitHub username and git user.name for cross-backend matching.")

(defvar-local shipit-repo-buffer-subscription nil
  "Cached subscription data for this repo buffer.
An alist from the API, nil for participating, or symbol
`not-supported' when the backend lacks :get-repo-subscription.")

(defvar-local shipit-repo-buffer--totals nil
  "Alist of each list of this buffer and how many items it has in all.
A list whose forge does not tell its total has none.")

(defvar-local shipit-repo-buffer--failures nil
  "Alist of each list of this buffer that failed to load, and why.")

(defvar-local shipit-repo-buffer--loading nil
  "The lists of this buffer whose first page has not arrived yet.")

(defvar-local shipit-repo-buffer--fetching-more nil
  "The lists of this buffer whose next page was asked for and has not arrived.")

(defvar-local shipit-repo-buffer--generation 0
  "How many times this buffer was refreshed.
An answer to a request of an earlier refresh is dropped.")

(defvar shipit-repo-buffer--kept-visibility nil
  "Alist of section types and whether each was hidden.
While a section is drawn again, its new sections take the visibility
that the sections of the same type had.")

(defconst shipit-repo-buffer--lists
  '((open-prs :kind pr :state "open" :section repo-prs
              :items shipit-repo-buffer-open-prs
              :has-more shipit-repo-buffer-open-prs-has-more)
    (closed-prs :kind pr :state "closed" :section repo-prs
                :items shipit-repo-buffer-closed-prs
                :has-more shipit-repo-buffer-closed-prs-has-more)
    (open-issues :kind issue :state "open" :section repo-issues
                 :items shipit-repo-buffer-open-issues
                 :has-more shipit-repo-buffer-open-issues-has-more)
    (closed-issues :kind issue :state "closed" :section repo-issues
                   :items shipit-repo-buffer-closed-issues
                   :has-more shipit-repo-buffer-closed-issues-has-more)
    (discussions :kind discussion :section repo-discussions
                 :items shipit-repo-buffer-discussions))
  "The lists of the repo buffer.
Each has its KIND, the STATE of its items, the SECTION it shows in, and
the variables of its ITEMS and of whether it HAS-MORE.")

(defconst shipit-repo-buffer--load-more-lists
  '((open-prs . open-prs) (prs . closed-prs)
    (open-issues . open-issues) (issues . closed-issues))
  "The list of each type of \"load more\" line.")

(defun shipit-repo-buffer--list-get (key property)
  "PROPERTY of the list KEY of the repo buffer."
  (plist-get (or (alist-get key shipit-repo-buffer--lists)
                 (error "No list %s in the repo buffer" key))
             property))

(defun shipit-repo-buffer--list-items (key)
  "The items of the list KEY that this buffer has."
  (symbol-value (shipit-repo-buffer--list-get key :items)))

(defun shipit-repo-buffer--page-size (key)
  "How many items a page of the list KEY has."
  (cond
   ((eq key 'discussions) 25)
   ((equal (shipit-repo-buffer--list-get key :state) "open") 100)
   (t (or shipit-repo-buffer-closed-limit 5))))

;;; Mode definition

(defvar shipit-repo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "$") #'shipit-process-buffer)
    (define-key map (kbd "g") 'shipit-repo-buffer-refresh)
    (define-key map (kbd "r") 'shipit-repo-buffer-refresh)
    (define-key map (kbd "o") 'shipit-repo-buffer--open-in-browser)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "+") #'shipit-repo-buffer--load-more-at-point)
    (define-key map (kbd "f") #'shipit-repo-buffer-filter)
    (define-key map (kbd "RET") #'shipit-repo-buffer--ret-action)
    (define-key map [return] #'shipit-repo-buffer--ret-action)
    ;; Subscription
    (define-key map (kbd "w") #'shipit-repo-subscription)
    ;; Copy URL on header / region elsewhere
    (define-key map [remap kill-ring-save] #'shipit-repo-buffer--kill-ring-save)
    ;; DWIM
    (define-key map (kbd "SPC") #'shipit-repo-buffer-dwim)
    (define-key map (kbd "M-;") #'shipit-repo-buffer-dwim)
    map)
  "Keymap for `shipit-repo-mode'.")

;; Ensure bindings exist even with stale defvar from previous load
(define-key shipit-repo-mode-map (kbd "+") #'shipit-repo-buffer--load-more-at-point)
(define-key shipit-repo-mode-map (kbd "f") #'shipit-repo-buffer-filter)
(define-key shipit-repo-mode-map (kbd "RET") #'shipit-repo-buffer--ret-action)
(define-key shipit-repo-mode-map [return] #'shipit-repo-buffer--ret-action)
(define-key shipit-repo-mode-map [remap kill-ring-save] #'shipit-repo-buffer--kill-ring-save)
(define-key shipit-repo-mode-map (kbd "M-w") #'shipit-repo-buffer--kill-ring-save)

(defun shipit-repo-buffer--section-visibility (section)
  "Return initial visibility for SECTION in repo buffers.
Languages, PRs, issues, and discussions sections are collapsed by default.
A section that is drawn again keeps the visibility it had."
  (let ((kept (assq (oref section type) shipit-repo-buffer--kept-visibility)))
    (cond
     (kept (if (cdr kept) 'hide 'show))
     ((memq (oref section type)
            '(repo-languages repo-prs repo-issues repo-discussions))
      'hide))))

(define-derived-mode shipit-repo-mode magit-section-mode "Shipit-Repo"
  "Major mode for shipit repository landing page buffers.
Displays repository metadata and README.

\\{shipit-repo-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-repo-buffer-refresh)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (add-hook 'magit-section-set-visibility-hook
            #'shipit-repo-buffer--section-visibility nil t)
  (setq-local magit-root-section nil)
  (shipit--apply-section-defaults)
  (shipit--watch-videos))

;;; Data fetching

(defun shipit-repo-buffer--fetch-repo-data (repo)
  "Fetch repo info, README, and languages for REPO via backend dispatch.
Returns plist (:data REPO-DATA :readme README-TEXT :languages LANGS).
Signals error if the backend does not support :fetch-repo-info."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (info-fn (plist-get backend :fetch-repo-info))
         (readme-fn (plist-get backend :fetch-readme))
         (languages-fn (plist-get backend :fetch-languages)))
    (unless info-fn
      (error "Backend %s does not support :fetch-repo-info"
             (plist-get backend :name)))
    (let ((repo-data (funcall info-fn config))
          (readme (when readme-fn
                    (condition-case nil
                        (funcall readme-fn config)
                      (error nil))))
          (languages (when languages-fn
                       (condition-case nil
                           (funcall languages-fn config)
                         (error nil)))))
      (list :data repo-data :readme readme :languages languages))))

(defun shipit-repo-buffer--issue-backend-available-p ()
  "Return non-nil if an issue backend matches the current repo's forge.
Prevents falling back to GitHub issue search for non-GitHub repos."
  (let ((pr-backend-id shipit-pr-backend))
    (cond
     ;; GitHub PR backend — GitHub issue backend is always available
     ((eq pr-backend-id 'github) t)
     ;; For non-GitHub backends, check if the issue backend matches
     ;; the PR backend (not falling back to GitHub default)
     (t (and (boundp 'shipit-issue-backend)
             shipit-issue-backend
             (eq shipit-issue-backend pr-backend-id))))))

(defun shipit-repo-buffer--discussions-available-p ()
  "Return non-nil if discussions are available for the current repo's forge.
Checks if the PR backend has discussion search support."
  (and (boundp 'shipit-discussions-enabled)
       shipit-discussions-enabled
       (fboundp 'shipit-discussion--search)
       (let* ((backend-plist (shipit-pr--get-backend)))
         (and backend-plist
              (plist-get backend-plist :browse-discussion-url)))))

(defun shipit-repo-buffer--issues-offered-p ()
  "Whether this buffer shows the issues of its repository."
  (and (boundp 'shipit-issues-enabled) shipit-issues-enabled
       (fboundp 'shipit-issue--resolve-for-repo)
       (shipit-repo-buffer--issue-backend-available-p)))

(defun shipit-repo-buffer--offered-lists ()
  "The lists that this buffer shows."
  (append '(open-prs closed-prs)
          (when (shipit-repo-buffer--issues-offered-p)
            '(open-issues closed-issues))
          (when (shipit-repo-buffer--discussions-available-p)
            '(discussions))))

(defun shipit-repo-buffer--search-args (key)
  "The search arguments of the list KEY, with the newest items first."
  (list (format "--state=%s" (shipit-repo-buffer--list-get key :state))
        "--sort=created"))

(defun shipit-repo-buffer--search-page-later (search config args page per-page callback)
  "Ask SEARCH with CONFIG for page PAGE of PER-PAGE items that ARGS find.
SEARCH is the `:search' of a backend without `:search-page-async'.
SEARCH waits for its answer, so a timer runs it after the buffer is
drawn.  CALLBACK gets the page, which tells no total."
  (run-at-time
   0 nil
   (lambda ()
     (funcall callback
              (condition-case err
                  (shipit--search-page
                   (seq-drop (funcall search config
                                      (append args (list (format "--limit=%d" (* page per-page)))))
                             (* (1- page) per-page))
                   nil)
                (error (list :error (error-message-string err))))))))

(defun shipit-repo-buffer--backend-for (key)
  "(BACKEND . CONFIG) of the forge or tracker of the list KEY."
  (if (eq (shipit-repo-buffer--list-get key :kind) 'pr)
      (shipit-pr--resolve-for-repo shipit-repo-buffer-repo)
    (shipit-issue--resolve-for-repo shipit-repo-buffer-repo)))

(defun shipit-repo-buffer--request-page (key page callback)
  "Ask in the background for page PAGE of the list KEY.
CALLBACK gets the page, as `shipit--search-page' or
`shipit--search-page-failure' gives it."
  (let ((per-page (shipit-repo-buffer--page-size key))
        (args (shipit-repo-buffer--search-args key)))
    (if (eq key 'discussions)
        (shipit-discussion--search-page-async
         shipit-repo-buffer-repo (list (format "--limit=%d" per-page)) callback)
      (pcase-let* ((`(,backend . ,config) (shipit-repo-buffer--backend-for key))
                   (page-fn (plist-get backend :search-page-async))
                   (search (plist-get backend :search)))
        (cond
         (page-fn (funcall page-fn config args page per-page callback))
         (search (shipit-repo-buffer--search-page-later
                  search config args page per-page callback))
         (t (error "Backend %s can search neither with :search-page-async nor :search"
                   (plist-get backend :name))))))))

(defun shipit-repo-buffer--fetch-page (key page)
  "Ask for page PAGE of the list KEY, and show it when it arrives.
An answer that arrives after the buffer was killed or refreshed is
dropped.  A request that signals as it starts fails its list, so that
the lists after it still load."
  (let* ((buffer (current-buffer))
         (generation shipit-repo-buffer--generation)
         (callback (lambda (result)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (= generation shipit-repo-buffer--generation)
                           (shipit-repo-buffer--take-page key page result)))))))
    (condition-case err
        (shipit-repo-buffer--request-page key page callback)
      (error
       (shipit--debug-log "REPO-BUFFER: request for %s page %d failed: %S" key page err)
       (funcall callback (list :error (error-message-string err)))))))

(defun shipit-repo-buffer--join-items (items more)
  "ITEMS followed by each item of MORE whose number is not among them.
A list sorted by its last update can move an item to a later page
between two requests."
  (let ((numbers (make-hash-table :test 'equal)))
    (dolist (item items)
      (puthash (cdr (assq 'number item)) t numbers))
    (append items (seq-remove (lambda (item) (gethash (cdr (assq 'number item)) numbers))
                              more))))

(defun shipit-repo-buffer--add-items (key page result)
  "Add the items of RESULT, page PAGE of the list KEY, to the list."
  (let* ((arrived (plist-get result :items))
         (items (shipit-repo-buffer--join-items
                 (when (> page 1) (shipit-repo-buffer--list-items key))
                 arrived))
         (total (plist-get result :total))
         (has-more (shipit-repo-buffer--list-get key :has-more)))
    (set (shipit-repo-buffer--list-get key :items) items)
    (setf (alist-get key shipit-repo-buffer--totals) total)
    (when has-more
      (set has-more (and (= (length arrived) (shipit-repo-buffer--page-size key))
                         (or (null total) (< (length items) total)))))))

(defun shipit-repo-buffer--take-page (key page result)
  "Show RESULT, page PAGE of the list KEY, or why it failed.
A first page that failed says so in the heading of its list.  A later
one says so in the echo area, and the list stays as it was."
  (setq shipit-repo-buffer--loading (delq key shipit-repo-buffer--loading)
        shipit-repo-buffer--fetching-more (delq key shipit-repo-buffer--fetching-more))
  (let ((failure (plist-get result :error)))
    (cond
     ((and failure (> page 1))
      (message "Could not load more: %s" failure))
     (failure
      (setf (alist-get key shipit-repo-buffer--failures) failure))
     (t
      (shipit-repo-buffer--add-items key page result))))
  (shipit-repo-buffer--redraw-section (shipit-repo-buffer--list-get key :section)))

(defun shipit-repo-buffer--start-lists ()
  "Forget the lists of this buffer, and mark those it shows as loading."
  (cl-incf shipit-repo-buffer--generation)
  (dolist (entry shipit-repo-buffer--lists)
    (set (plist-get (cdr entry) :items) nil)
    (when-let* ((has-more (plist-get (cdr entry) :has-more)))
      (set has-more nil)))
  (setq shipit-repo-buffer--totals nil
        shipit-repo-buffer--failures nil
        shipit-repo-buffer--fetching-more nil
        shipit-repo-buffer--loading (shipit-repo-buffer--offered-lists)))

(defun shipit-repo-buffer--fetch-lists ()
  "Ask for the first page of each list that is loading."
  (dolist (key (copy-sequence shipit-repo-buffer--loading))
    (shipit-repo-buffer--fetch-page key 1)))

;;; Entry point

(defun shipit-repo-buffer--read-repo ()
  "Read a repository, an owner/name path, in the minibuffer.
The candidates are the repositories of the git remotes of the
directory, with that of origin as the default.  A repository that
no remote names can be typed."
  (let* ((repos (shipit--repos-from-remotes))
         (repo (completing-read (format-prompt "Repository (owner/repo)" (car repos))
                                repos nil nil nil nil (car repos))))
    (when (string-empty-p repo)
      (user-error "No repository given"))
    repo))

;;;###autoload
(defun shipit-open-repo-buffer (repo &optional backend-id backend-config)
  "Open a dedicated landing page buffer for REPO (owner/name).
Creates or reuses the buffer *shipit-repo: REPO*.
BACKEND-ID and BACKEND-CONFIG override auto-detection when provided."
  (interactive (list (shipit-repo-buffer--read-repo)))
  (let* ((buf-name (format "*shipit-repo: %s*" repo))
         (existing (get-buffer buf-name))
         ;; Resolve backend in calling buffer (which has git context)
         (caller-backend (or backend-id (shipit-pr--backend-id)))
         (caller-config (or backend-config shipit-pr-backend-config))
         (caller-root (when (equal (shipit--get-repo-from-remote) repo)
                        (locate-dominating-file default-directory ".git"))))
    (if existing
        (switch-to-buffer existing)
      (let ((buffer (generate-new-buffer buf-name)))
        (with-current-buffer buffer
          (shipit-repo-mode)
          (setq shipit-repo-buffer-repo repo)
          (setq shipit-repo-buffer-local-root caller-root)
          (setq-local shipit-pr-backend caller-backend)
          (setq-local shipit-pr-backend-config caller-config)
          (shipit-repo-buffer-refresh))
        (switch-to-buffer buffer)))))

;;; Refresh

(defun shipit-repo-buffer--fetch-header-data (repo)
  "Fetch what the header and the README of REPO show."
  (let ((result (shipit-repo-buffer--fetch-repo-data repo)))
    (setq shipit-repo-buffer-data (plist-get result :data))
    (setq shipit-repo-buffer-readme (plist-get result :readme))
    (setq shipit-repo-buffer-languages (plist-get result :languages))
    (setq shipit-repo-buffer-subscription
          (let* ((resolved (shipit-pr--resolve-for-repo repo))
                 (fn (plist-get (car resolved) :get-repo-subscription)))
            (if fn
                (funcall fn (cdr resolved))
              'not-supported)))))

(defun shipit-repo-buffer--draw ()
  "Draw this buffer from what it has fetched."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (magit-insert-section (repo-root)
      (shipit-repo-buffer--insert-header-section
       shipit-repo-buffer-data
       (shipit-repo-buffer--format-language-percentages shipit-repo-buffer-languages))
      (shipit-repo-buffer--insert-readme-section shipit-repo-buffer-readme)
      (dolist (type '(repo-prs repo-issues repo-discussions))
        (shipit-repo-buffer--insert-list-section type)))
    (goto-char (min pos (point-max)))))

(defun shipit-repo-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the repo buffer contents.
The header and the README show at once.  The lists of pull requests,
issues and discussions load in the background, and each shows when it
arrives."
  (interactive)
  (let ((repo shipit-repo-buffer-repo))
    (shipit--debug-log "Refreshing repo buffer for %s" repo)
    (message "Loading repo %s..." repo)
    (shipit-repo-buffer--fetch-header-data repo)
    (shipit-repo-buffer--start-lists)
    (shipit-repo-buffer--draw)
    (shipit-repo-buffer--fetch-lists)
    (message "Repo %s loaded" repo)))

;;; Section rendering

(defconst shipit-repo-buffer--visibility-emoji
  '(("public" . "\U0001f310") ("private" . "\U0001f512") ("internal" . "\U0001f3e2"))
  "The emoji for each visibility of a repository, where SVG icons are off.")

(defun shipit-repo-buffer--flag-p (repo-data key)
  "Whether the flag KEY of REPO-DATA is set.  A JSON false is not."
  (eq t (cdr (assq key repo-data))))

(defun shipit-repo-buffer--visibility (repo-data)
  "The visibility of the repository in REPO-DATA, or nil when it is not known.
It is \"public\", \"private\" or \"internal\".  Data without a
visibility can have a private flag.  With neither, the visibility is
not known, and a guess could call a private repository public."
  (or (cdr (assq 'visibility repo-data))
      (when (assq 'private repo-data)
        (if (shipit-repo-buffer--flag-p repo-data 'private) "private" "public"))))

(defun shipit-repo-buffer--administered-p (repo-data)
  "Whether the user administers the repository in REPO-DATA."
  (shipit-repo-buffer--flag-p (cdr (assq 'permissions repo-data)) 'admin))

(defun shipit-repo-buffer--type-notes (repo-data visibility)
  "What the Type line says of the repository in REPO-DATA, as a list.
VISIBILITY is its visibility.  Public is said only of a repository that
the user administers: of the repository of another it is what one
expects, and it says nothing."
  (delq nil
        (list (unless (and (equal visibility "public")
                           (not (shipit-repo-buffer--administered-p repo-data)))
                visibility)
              (and (shipit-repo-buffer--flag-p repo-data 'fork) "fork")
              (and (shipit-repo-buffer--flag-p repo-data 'archived) "archived"))))

(defun shipit-repo-buffer--insert-type-line (repo-data)
  "Insert the Type line of the header from REPO-DATA.
It says whether the repository is public, private or internal, and
whether it is a fork or archived.  Nothing is inserted when the
visibility is not known, or when there is nothing to say."
  (when-let* ((visibility (shipit-repo-buffer--visibility repo-data))
              (notes (shipit-repo-buffer--type-notes repo-data visibility)))
    (insert (format "   %s Type:     %s\n"
                    (shipit--get-pr-field-icon
                     (concat "repo-" visibility)
                     (or (cdr (assoc visibility shipit-repo-buffer--visibility-emoji))
                         "\U0001f4e6"))
                    (upcase-initials (string-join notes ", "))))))

(defun shipit-repo-buffer--insert-stars-line (repo-data)
  "Insert the Stars line of the header from REPO-DATA.
A forge that gives no count of stars gets no line."
  (let ((stars (cdr (assq 'stargazers_count repo-data))))
    (when (integerp stars)
      (insert (format "   %s Stars:    %d\n"
                      (shipit--get-pr-field-icon "repo-stars" "\u2b50")
                      stars)))))

(defun shipit-repo-buffer--insert-header-section (repo-data &optional lang-pcts)
  "Insert repo header section from REPO-DATA.
LANG-PCTS, if non-nil, is a list of (NAME . PERCENT) pairs inserted
as a child languages sub-section."
  (let* ((full-name (or (cdr (assq 'full_name repo-data)) "Unknown"))
         (description (cdr (assq 'description repo-data)))
         (owner-obj (cdr (assq 'owner repo-data)))
         (owner (or (cdr (assq 'login owner-obj)) "Unknown"))
         (avatar-url (cdr (assq 'avatar_url owner-obj)))
         (default-branch (cdr (assq 'default_branch repo-data)))
         (html-url (cdr (assq 'html_url repo-data))))
    (magit-insert-section (repo-header nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "repo" "\U0001f4e6")
                (propertize full-name 'font-lock-face 'magit-section-heading)))
      (insert (format "   %s Owner:    %s%s\n"
                      (shipit--get-pr-field-icon "author" "\U0001f464")
                      (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                               (fboundp 'shipit--create-avatar-display))
                          (concat (shipit--create-avatar-display owner avatar-url 16) " ")
                        "")
                      (propertize owner 'font-lock-face 'shipit-username-face)))
      (when default-branch
        (insert (format "   %s Branch:   %s\n"
                        (shipit--get-pr-field-icon "branch" "\U0001f33f")
                        (propertize default-branch 'font-lock-face 'font-lock-constant-face))))
      (shipit-repo-buffer--insert-type-line repo-data)
      (shipit-repo-buffer--insert-stars-line repo-data)
      (when description
        ;; Wrap long descriptions to the same 80-col envelope the README
        ;; renders into.  Continuation lines align under the start of the
        ;; description text (after the 3-space indent + icon + space).
        (let* ((cont-indent "      ")
               (wrap-width (max 20 (- 80 (length cont-indent))))
               (wrapped (if (fboundp 'shipit--wrap-text)
                            (shipit--wrap-text description wrap-width 0)
                          description))
               (continued (replace-regexp-in-string
                           "\n" (concat "\n" cont-indent) wrapped)))
          (insert (format "   %s %s\n"
                          (shipit--get-pr-field-icon "description" "\U0001f4dd")
                          continued))))
      (shipit-repo-buffer--insert-subscription-line)
      (when html-url
        (add-text-properties (point-min) (point)
                             `(shipit-repo-url ,html-url)))
      (if lang-pcts
          (shipit-repo-buffer--insert-languages-section lang-pcts)
        (insert "\n")))))

(defun shipit-repo-buffer--format-language-percentages (languages)
  "Compute language percentages from LANGUAGES alist.
LANGUAGES is an alist of (NAME . BYTES).
Returns sorted list of (NAME . PERCENT) pairs, sorted by bytes descending."
  (when languages
    (let ((total (apply #'+ (mapcar #'cdr languages))))
      (when (> total 0)
        (let ((pcts (mapcar (lambda (entry)
                              (cons (if (stringp (car entry))
                                        (car entry)
                                      (symbol-name (car entry)))
                                    (* 100.0 (/ (float (cdr entry)) total))))
                            languages)))
          (sort pcts (lambda (a b) (> (cdr a) (cdr b)))))))))

(defun shipit-repo-buffer--insert-languages-section (lang-pcts)
  "Insert collapsible languages section with LANG-PCTS.
LANG-PCTS is a list of (NAME . PERCENT) pairs."
  (magit-insert-section (repo-languages nil nil)
    (magit-insert-heading
      (format "   %s %s"
              (shipit--get-pr-field-icon "language" "\U0001f4bb")
              (propertize (format "Languages (%d)" (length lang-pcts))
                          'font-lock-face 'magit-section-heading)))
    (magit-insert-section-body
      (dolist (entry lang-pcts)
        (insert (format "      %s %s\n"
                        (propertize (car entry) 'font-lock-face 'font-lock-type-face)
                        (propertize (format "%.1f%%" (cdr entry))
                                    'font-lock-face 'font-lock-comment-face))))
      (insert "\n"))))

(defun shipit-repo-buffer--readme-text (readme)
  "Extract README content string from README (plist or bare string)."
  (cond
   ((null readme) nil)
   ((stringp readme) readme)
   ((plist-get readme :content))))

(defun shipit-repo-buffer--readme-format (readme)
  "Determine renderer format for README (plist or bare string).
Prefers the filename on the README plist; falls back to content
detection (so a bare-string README from an old/stale fetch backend
still picks org when the body has org markers like `#+TITLE:')."
  (let* ((filename (when (and readme (not (stringp readme)))
                     (plist-get readme :filename)))
         (text (shipit-repo-buffer--readme-text readme))
         (by-filename (when (and filename
                                 (fboundp 'shipit--format-for-readme-filename))
                        (shipit--format-for-readme-filename filename))))
    (cond
     ((eq by-filename 'org) 'org)
     ((and text (fboundp 'shipit--detect-body-format))
      (shipit--detect-body-format text))
     (t (or by-filename 'markdown)))))

(defun shipit-repo-buffer--render-readme (text format)
  "Return TEXT, a README in FORMAT, rendered.
The lines of each paragraph of a markdown README are joined, as a forge
shows a file.  A comment keeps its lines, as a forge shows it so too."
  (let ((rendered (if (fboundp 'shipit--render-body)
                      (shipit--render-body text format)
                    text)))
    (if (and (eq format 'markdown) (fboundp 'shipit--join-paragraph-lines))
        (shipit--join-paragraph-lines rendered)
      rendered)))

(defun shipit-repo-buffer--fill-readme (rendered)
  "Return RENDERED, the text of a README, filled to `shipit-readme-fill-column'.
The cells of each table row that is marked in it are put side by side
first, each filled to its own column."
  (let ((width shipit-readme-fill-column))
    (shipit--strip-face-on-whitespace
     (shipit--wrap-text (if (fboundp 'shipit--lay-out-table-rows)
                            (shipit--lay-out-table-rows rendered width)
                          rendered)
                        width 0))))

(defun shipit-repo-buffer--lay-out-readme (text format)
  "Return TEXT, a README in FORMAT, rendered and laid out for the buffer."
  ;; Hard-wrapping via `fill-region' was a ~25% slice of redisplay
  ;; profiles.  The buffer has `truncate-lines' nil + `word-wrap' t (set
  ;; in the mode), so the display engine visual-wraps long lines at zero
  ;; allocation cost.  Set `shipit-readme-fill-column' to a number to
  ;; opt back in to hard-wrap.
  (let ((rendered (shipit-repo-buffer--render-readme text format)))
    (if (and (numberp shipit-readme-fill-column) (fboundp 'shipit--wrap-text))
        (shipit-repo-buffer--fill-readme rendered)
      rendered)))

(defun shipit-repo-buffer--insert-markdown-readme (text indent)
  "Insert TEXT, a markdown README, after INDENT spaces.
Its headings become nested sections that fold.  A details block, whose
mark TEXT holds, becomes a section in the section of the heading above
it, see `shipit--insert-body-with-details'."
  (shipit-repo-buffer--insert-markdown-as-sections
   (shipit-repo-buffer--lay-out-readme text 'markdown) indent #'shipit--insert-lines))

(defun shipit-repo-buffer--insert-readme-section (readme)
  "Insert README section.
README is either nil, a plain string of README content, or a plist
with :filename and :content keys (the format returned by backend
:fetch-readme functions)."
  (magit-insert-section (repo-readme nil nil)
    (magit-insert-heading
      (format "%s %s"
              (shipit--get-pr-field-icon "description" "\U0001f4c4")
              (propertize "README" 'font-lock-face 'magit-section-heading)))
    (magit-insert-section-body
      (let ((readme-text (shipit-repo-buffer--readme-text readme)))
        (if (not readme-text)
            (insert (propertize "   No README found\n" 'font-lock-face 'italic))
          (let* ((shipit--image-base-repo shipit-repo-buffer-repo)
                 ;; Measure the wrap-column pixel width in THIS buffer's
                 ;; face so the gif resizer doesn't fall back to the temp
                 ;; render buffer's face (which may be narrower).
                 (shipit--image-target-width
                  (if (fboundp 'string-pixel-width)
                      (string-pixel-width (make-string 80 ?M))
                    (* 80 (frame-char-width))))
                 (format (shipit-repo-buffer--readme-format readme))
                 (readme-start (point)))
            (if (eq format 'markdown)
                (shipit--insert-body-with-details
                 readme-text 3 #'shipit-repo-buffer--insert-markdown-readme)
              (let ((wrapped (shipit-repo-buffer--lay-out-readme readme-text format)))
                (if (eq format 'org)
                    (shipit-repo-buffer--insert-org-as-sections wrapped 3)
                  (insert (concat "   "
                                  (replace-regexp-in-string "\n" "\n   " wrapped)
                                  "\n")))))
            (when (fboundp 'shipit--create-generic-url-overlays)
              (shipit--create-generic-url-overlays readme-start (point)))
            (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
              (shipit--apply-code-block-backgrounds-in-region readme-start (point)))
            (when (fboundp 'shipit--animate-images-in-region)
              (shipit--animate-images-in-region readme-start (point)))
            (when (fboundp 'shipit--center-aligned-lines-in-region)
              (shipit--center-aligned-lines-in-region
               readme-start (point) shipit--image-target-width))
            (when (fboundp 'shipit--align-table-columns-in-region)
              (shipit--align-table-columns-in-region readme-start (point))))))
      (insert "\n"))))

(defun shipit-repo-buffer--pr-type-label ()
  "Return the plural PR type label from the active backend."
  (let* ((backend (shipit-pr--get-backend))
         (singular (or (plist-get backend :pr-type-label) "Pull Request")))
    (concat singular "s")))

(defun shipit-repo-buffer--insert-load-more-line (load-more-type)
  "Insert a \"Type + to load more\" button line for LOAD-MORE-TYPE."
  (insert "   ")
  (insert-text-button
   (substitute-command-keys
    (format "Type \\<%s>\\[%s] to load more"
            'shipit-repo-mode-map
            'shipit-repo-buffer--load-more-at-point))
   'action (lambda (_button) (shipit-repo-buffer--load-more-at-point))
   'follow-link t
   'mouse-face 'magit-section-highlight
   'shipit-repo-load-more load-more-type)
  (insert "\n"))

(defun shipit-repo-buffer--insert-item-line (item repo text-prop)
  "Insert a single ITEM line with number/title, setting TEXT-PROP on the line.
REPO is set as `shipit-repo-item-repo' text property."
  (let* ((number (cdr (assq 'number item)))
         (title (or (cdr (assq 'title item)) ""))
         (line (format "   #%s  %s\n" number title)))
    (insert (propertize line
                        text-prop number
                        'shipit-repo-item-repo repo))))

(defun shipit-repo-buffer--list-shown-p (key items)
  "Whether the list KEY shows, with ITEMS of it after the filter.
It shows while it loads, when it failed, and when it has items.  With a
filter that no item matches, it shows with none."
  (or items
      (memq key shipit-repo-buffer--loading)
      (alist-get key shipit-repo-buffer--failures)
      (and (shipit-repo-buffer--filter-active-p)
           (shipit-repo-buffer--list-items key))))

(defun shipit-repo-buffer--list-heading (label key shown)
  "LABEL of the list KEY with its count, where SHOWN of its items show."
  (let ((failure (alist-get key shipit-repo-buffer--failures))
        (total (alist-get key shipit-repo-buffer--totals)))
    (cond
     (failure (format "%s (failed: %s)" label failure))
     ((memq key shipit-repo-buffer--loading) (format "%s (loading…)" label))
     ((and total (< shown total)) (format "%s (%d of %d)" label shown total))
     (t (format "%s (%d)" label shown)))))

(defun shipit-repo-buffer--group-count (keys)
  "The count after the heading of the section of the lists KEYS.
The first of KEYS is the list of the open items."
  (let ((total (alist-get (car keys) shipit-repo-buffer--totals)))
    (cond
     ((seq-some (lambda (key) (memq key shipit-repo-buffer--loading)) keys)
      " (loading…)")
     (total (format " (%d open)" total))
     (t ""))))

(defun shipit-repo-buffer--insert-list (type key items repo text-prop has-more load-more-type)
  "Insert the subsection TYPE of the list KEY with ITEMS of REPO.
TEXT-PROP holds the number of each item.  HAS-MORE adds a \"load more\"
line of LOAD-MORE-TYPE."
  (when (shipit-repo-buffer--list-shown-p key items)
    (magit-insert-section ((eval type) nil nil)
      (magit-insert-heading
        (propertize (concat "  " (shipit-repo-buffer--list-heading
                                  (capitalize (shipit-repo-buffer--list-get key :state))
                                  key (length items)))
                    'font-lock-face 'magit-section-heading))
      (dolist (item items)
        (shipit-repo-buffer--insert-item-line item repo text-prop))
      (when has-more
        (shipit-repo-buffer--insert-load-more-line load-more-type)))))

(defun shipit-repo-buffer--insert-group-heading (icon fallback label keys)
  "Insert the heading LABEL with the ICON, or FALLBACK, and the count of KEYS."
  (magit-insert-heading
    (format "%s %s%s"
            (shipit--get-pr-field-icon icon fallback)
            (propertize label 'font-lock-face 'magit-section-heading)
            (shipit-repo-buffer--group-count keys))))

(defun shipit-repo-buffer--insert-prs-section (open-prs closed-prs closed-has-more repo
                                                       &optional open-has-more)
  "Insert PRs section with OPEN-PRS and CLOSED-PRS subsections for REPO.
CLOSED-HAS-MORE, when non-nil, adds a \"load more\" line to the closed subsection.
OPEN-HAS-MORE, when non-nil, adds a \"load more\" line to the open subsection."
  (when (or (shipit-repo-buffer--list-shown-p 'open-prs open-prs)
            (shipit-repo-buffer--list-shown-p 'closed-prs closed-prs))
    (magit-insert-section (repo-prs nil nil)
      (shipit-repo-buffer--insert-group-heading
       "pull-request" "\U0001f501" (shipit-repo-buffer--pr-type-label)
       '(open-prs closed-prs))
      (magit-insert-section-body
        (shipit-repo-buffer--insert-list 'repo-prs-open 'open-prs open-prs repo
                                         'shipit-repo-pr-number open-has-more 'open-prs)
        (shipit-repo-buffer--insert-list 'repo-prs-closed 'closed-prs closed-prs repo
                                         'shipit-repo-pr-number closed-has-more 'prs)
        (insert "\n")))))

(defun shipit-repo-buffer--insert-issues-section (open-issues closed-issues closed-has-more repo
                                                             &optional open-has-more)
  "Insert issues section with OPEN-ISSUES and CLOSED-ISSUES subsections for REPO.
CLOSED-HAS-MORE, when non-nil, adds a \"load more\" line to the closed subsection.
OPEN-HAS-MORE, when non-nil, adds a \"load more\" line to the open subsection."
  (when (or (shipit-repo-buffer--list-shown-p 'open-issues open-issues)
            (shipit-repo-buffer--list-shown-p 'closed-issues closed-issues))
    (magit-insert-section (repo-issues nil nil)
      (shipit-repo-buffer--insert-group-heading
       "issue-open" "\U0001f41b" "Issues" '(open-issues closed-issues))
      (magit-insert-section-body
        (shipit-repo-buffer--insert-list 'repo-issues-open 'open-issues open-issues repo
                                         'shipit-repo-issue-number open-has-more 'open-issues)
        (shipit-repo-buffer--insert-list 'repo-issues-closed 'closed-issues closed-issues repo
                                         'shipit-repo-issue-number closed-has-more 'issues)
        (insert "\n")))))

(defun shipit-repo-buffer--insert-discussions-section (discussions repo)
  "Insert collapsible discussions section with DISCUSSIONS data for REPO.
DISCUSSIONS is a list of alists with `number' and `title' keys."
  (when (shipit-repo-buffer--list-shown-p 'discussions discussions)
    (magit-insert-section (repo-discussions nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "discussion" "\U0001f4ac")
                (propertize (shipit-repo-buffer--list-heading
                             "Discussions" 'discussions (length discussions))
                            'font-lock-face 'magit-section-heading)))
      (magit-insert-section-body
        (dolist (disc discussions)
          (shipit-repo-buffer--insert-item-line
           disc repo 'shipit-repo-discussion-number))
        (insert "\n")))))

(defun shipit-repo-buffer--insert-list-section (type)
  "Insert the section TYPE of the lists of this buffer, with the filter applied."
  (let ((repo shipit-repo-buffer-repo))
    (pcase type
      ('repo-prs
       (shipit-repo-buffer--insert-prs-section
        (shipit-repo-buffer--filter-items shipit-repo-buffer-open-prs)
        (shipit-repo-buffer--filter-items shipit-repo-buffer-closed-prs)
        shipit-repo-buffer-closed-prs-has-more repo shipit-repo-buffer-open-prs-has-more))
      ('repo-issues
       (shipit-repo-buffer--insert-issues-section
        (shipit-repo-buffer--filter-items shipit-repo-buffer-open-issues)
        (shipit-repo-buffer--filter-items shipit-repo-buffer-closed-issues)
        shipit-repo-buffer-closed-issues-has-more repo
        shipit-repo-buffer-open-issues-has-more))
      ('repo-discussions
       (shipit-repo-buffer--insert-discussions-section
        (shipit-repo-buffer--filter-items shipit-repo-buffer-discussions) repo)))))

;;; Drawing a section again

(defun shipit-repo-buffer--top-section (type)
  "The section of type TYPE right below the root of this buffer, or nil."
  (when magit-root-section
    (seq-find (lambda (section) (eq (oref section type) type))
              (oref magit-root-section children))))

(defun shipit-repo-buffer--visibility-of (section)
  "Alist of the types of SECTION and its subsections, and whether each is hidden."
  (let ((visibility nil))
    (magit-map-sections (lambda (each)
                          (push (cons (oref each type) (oref each hidden)) visibility))
                        section)
    visibility))

(defun shipit-repo-buffer--hide-hidden (section)
  "Hide each part of SECTION that is to be hidden.
A subsection gets its body when it is inserted, so it shows until it is
hidden."
  (magit-map-sections (lambda (each)
                        (when (oref each hidden)
                          (magit-section-hide each)))
                      section))

(defun shipit-repo-buffer--replace-child (root old index)
  "Put the section that was inserted last below ROOT in the place of OLD.
INDEX is where OLD was among the children of ROOT."
  (let* ((others (delq old (oref root children)))
         (new (car (last others)))
         (rest (butlast others)))
    (oset root children (append (seq-take rest index) (list new) (nthcdr index rest)))))

(defun shipit-repo-buffer--redraw-section (type)
  "Draw the section TYPE again from the lists of this buffer.
It keeps its place, the visibility of its parts, and point in it."
  (when-let* ((old (shipit-repo-buffer--top-section type)))
    (let* ((inhibit-read-only t)
           (root magit-root-section)
           (start (marker-position (oref old start)))
           (end (marker-position (oref old end)))
           (offset (and (<= start (point)) (< (point) end) (- (point) start)))
           (index (seq-position (oref root children) old))
           (count (length (oref root children)))
           (shipit-repo-buffer--kept-visibility (shipit-repo-buffer--visibility-of old)))
      (save-excursion
        (delete-region start end)
        (goto-char start)
        (let ((magit-insert-section--parent root))
          (shipit-repo-buffer--insert-list-section type))
        (when (< (oref root end) (point))
          (set-marker (oref root end) (point))))
      (if (> (length (oref root children)) count)
          (progn
            (shipit-repo-buffer--replace-child root old index)
            (shipit-repo-buffer--hide-hidden (nth index (oref root children))))
        (oset root children (delq old (oref root children))))
      (when offset
        (goto-char (min (+ start offset) (point-max)))))))

;;; Section detection helpers

(defun shipit-repo-buffer--section-type-at-point ()
  "Return the items section type at point, or nil.
Walks up the magit section parent tree looking for repo-prs,
repo-issues, or repo-discussions.  Returns the symbol or nil."
  (when (fboundp 'magit-current-section)
    (let ((section (magit-current-section)))
      (while (and section
                  (not (memq (oref section type)
                             '(repo-prs repo-issues repo-discussions))))
        (setq section (oref section parent)))
      (when section
        (oref section type)))))

(defun shipit-repo-buffer--in-items-section-p ()
  "Return non-nil if point is within a PR, issue, or discussion section."
  (not (null (shipit-repo-buffer--section-type-at-point))))

(defun shipit-repo-buffer--subsection-type-at-point ()
  "Return the subsection type at point, or nil.
Walks up looking for open/closed subsection types like
repo-prs-open, repo-prs-closed, repo-issues-open, repo-issues-closed."
  (when (fboundp 'magit-current-section)
    (let ((section (magit-current-section)))
      (while (and section
                  (not (memq (oref section type)
                             '(repo-prs-open repo-prs-closed
                               repo-issues-open repo-issues-closed))))
        (setq section (oref section parent)))
      (when section
        (oref section type)))))

(defun shipit-repo-buffer--load-more-type-at-point ()
  "Return the load-more type if point is in a section with more items.
Returns `open-prs', `prs', `open-issues', `issues', or nil."
  ;; First check if we're directly on the load-more line
  (let ((direct (get-text-property (point) 'shipit-repo-load-more)))
    (if direct
        direct
      ;; Check subsection first (open vs closed), fall back to parent
      (let ((sub (shipit-repo-buffer--subsection-type-at-point)))
        (pcase sub
          ('repo-prs-open
           (when shipit-repo-buffer-open-prs-has-more 'open-prs))
          ('repo-prs-closed
           (when shipit-repo-buffer-closed-prs-has-more 'prs))
          ('repo-issues-open
           (when shipit-repo-buffer-open-issues-has-more 'open-issues))
          ('repo-issues-closed
           (when shipit-repo-buffer-closed-issues-has-more 'issues))
          (_ ;; Not in a subsection — check parent section
           (let ((section-type (shipit-repo-buffer--section-type-at-point)))
             (pcase section-type
               ('repo-prs
                (or (when shipit-repo-buffer-closed-prs-has-more 'prs)
                    (when shipit-repo-buffer-open-prs-has-more 'open-prs)))
               ('repo-issues
                (or (when shipit-repo-buffer-closed-issues-has-more 'issues)
                    (when shipit-repo-buffer-open-issues-has-more 'open-issues)))
               (_ nil)))))))))

;;; Load more

(defun shipit-repo-buffer--load-more-at-point ()
  "Load more closed items from anywhere in a PR or issue section.
With prefix arg N, load N more items instead of the default."
  (interactive)
  (if-let* ((type (shipit-repo-buffer--load-more-type-at-point)))
      (shipit-repo-buffer--load-more
       type (when current-prefix-arg
              (prefix-numeric-value current-prefix-arg)))
    (message "No \"load more\" available at point")))

(defun shipit-repo-buffer--fetch-more (type &optional count)
  "Ask for the list of the \"load more\" TYPE again, COUNT items longer.
TYPE is `prs', `issues', `open-prs', or `open-issues'.  Without COUNT,
the list grows by its page size.  The forge or tracker answers while
the caller waits."
  (pcase-let* ((key (alist-get type shipit-repo-buffer--load-more-lists))
               (`(,backend . ,config) (shipit-repo-buffer--backend-for key))
               (search (or (plist-get backend :search)
                           (error "Backend %s cannot search" (plist-get backend :name))))
               (limit (+ (length (shipit-repo-buffer--list-items key))
                         (or count (shipit-repo-buffer--page-size key))))
               (items (funcall search config
                               (append (shipit-repo-buffer--search-args key)
                                       (list (format "--limit=%d" limit))))))
    (set (shipit-repo-buffer--list-get key :items) items)
    (set (shipit-repo-buffer--list-get key :has-more) (= (length items) limit))))

;; Backward compat alias
(defalias 'shipit-repo-buffer--fetch-more-closed
  'shipit-repo-buffer--fetch-more)

(defun shipit-repo-buffer--filter-active-p ()
  "Return non-nil if any item filter is currently active."
  (or shipit-repo-buffer--filter-mode
      (not (string-empty-p shipit-repo-buffer--filter-text))))

(defun shipit-repo-buffer--load-next-page (key)
  "Ask for the next page of the list KEY, and add it when it arrives."
  (if (memq key shipit-repo-buffer--fetching-more)
      (message "Still loading more")
    (push key shipit-repo-buffer--fetching-more)
    (message "Loading more...")
    (shipit-repo-buffer--fetch-page
     key (1+ (/ (length (shipit-repo-buffer--list-items key))
                (shipit-repo-buffer--page-size key))))))

(defun shipit-repo-buffer--load-more (type &optional count)
  "Load more items of TYPE.
TYPE is `prs', `open-prs', `issues', or `open-issues'.
Without COUNT, the next page loads in the background.  COUNT asks for
the whole list again, COUNT items longer, and waits for it, because a
page cannot start at any item."
  (if (null count)
      (shipit-repo-buffer--load-next-page
       (alist-get type shipit-repo-buffer--load-more-lists))
    (message "Loading more...")
    (shipit-repo-buffer--fetch-more type count)
    (shipit-repo-buffer--refresh-filtered-section)
    (message "Loaded more items")))

;;; RET handler

(defun shipit-repo-buffer--resolve-relative-url (path)
  "Resolve relative PATH to a full URL using repo buffer context."
  (when-let* ((data shipit-repo-buffer-data)
              (html-url (cdr (assq 'html_url data)))
              (branch (cdr (assq 'default_branch data))))
    (format "%s/blob/%s/%s" html-url branch path)))

(defun shipit-repo-buffer--link-at-point ()
  "Return link info at point as plist, or nil.
Checks URL overlays, markdown/org link text properties, and buffer text.
Returns one of:
  (:type url :url URL)
  (:type file :path PATH :url URL)
  (:type org-heading :target HEADING)
  (:type org-anchor :target ANCHOR)"
  (let ((result nil))
    ;; Check URL overlays (from shipit--create-generic-url-overlays)
    (dolist (ov (overlays-at (point)))
      (when-let ((help (overlay-get ov 'help-echo)))
        (when (and (not result) (stringp help)
                   (string-match "\\`\\(https?://[^ ]+\\) (RET:" help))
          (setq result (list :type 'url :url (match-string 1 help))))))
    ;; Check markdown/org link text property (rendered descriptions)
    (unless result
      (when-let ((help (get-text-property (point) 'help-echo)))
        (when (stringp help)
          (cond
           ;; org renders link descriptions with help-echo "LINK: <target>"
           ((string-match "\\`LINK: \\(.+\\)\\'" help)
            (let ((target (match-string 1 help)))
              (cond
               ((string-match-p "\\`https?://" target)
                (setq result (list :type 'url :url target)))
               ((string-prefix-p "*" target)
                (setq result (list :type 'org-heading
                                   :target (substring target 1))))
               ((string-prefix-p "#" target)
                (setq result (list :type 'org-anchor
                                   :target (substring target 1))))
               (t
                (setq result (list :type 'org-heading :target target))))))
           ((string-match-p "\\`https?://" help)
            (setq result (list :type 'url :url help)))
           (t
            ;; Relative path — check local file first
            (let ((local-file (when shipit-repo-buffer-local-root
                                (expand-file-name help shipit-repo-buffer-local-root))))
              (if (and local-file (file-exists-p local-file))
                  (setq result (list :type 'file :path local-file
                                     :url (shipit-repo-buffer--resolve-relative-url help)))
                (when-let ((url (shipit-repo-buffer--resolve-relative-url help)))
                  (setq result (list :type 'url :url url))))))))))
    ;; Fall back to Emacs built-in URL detection
    (unless result
      (when-let ((url (thing-at-point 'url)))
        (setq result (list :type 'url :url url))))
    result))

(defun shipit-repo-buffer--normalize-anchor (s)
  "Normalize S into the slug form used by org TOC anchors.
Lowercases, replaces every run of non-alphanumeric characters with
a single dash, and trims leading/trailing dashes.  Both an org
heading line and an anchor target normalize to the same string when
they refer to the same heading."
  (let* ((lower (downcase (or s "")))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (trimmed (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug)))
    trimmed))

(defun shipit-repo-buffer--find-org-heading (heading-text)
  "Return buffer position of an `org-heading' section matching HEADING-TEXT.
Walks the buffer for magit-sections of type `org-heading' and matches
on the section's heading-line text under slug-normalization, so anchor
targets like `other-llm-backends' match a section whose heading reads
`Other LLM backends'."
  (let ((target (shipit-repo-buffer--normalize-anchor heading-text))
        (seen (make-hash-table :test 'eq))
        found pos)
    (when (not (string-empty-p target))
      (save-excursion
        (setq pos (point-min))
        (while (and (not found) (< pos (point-max)))
          (let ((s (get-text-property pos 'magit-section)))
            (when (and s
                       (eq (oref s type) 'org-heading)
                       (not (gethash s seen)))
              (puthash s t seen)
              (let* ((start (and (oref s start) (marker-position (oref s start))))
                     (line (and start
                                (save-excursion
                                  (goto-char start)
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
                     (heading-slug (and line
                                        (shipit-repo-buffer--normalize-anchor line))))
                (when (and heading-slug (equal target heading-slug))
                  (setq found start)))))
          (setq pos (or (next-single-property-change pos 'magit-section nil (point-max))
                        (point-max))))))
    found))

(defun shipit-repo-buffer--ret-action ()
  "Handle RET in repo buffer.
If point is on a PR/issue/discussion item, open the appropriate buffer.
If point is on a \"load more\" line, fetch more closed items.
If point is on a local file link, open it directly.
If point is on a URL, open it — classified URLs open in shipit,
others open in browser.  With prefix arg, show action menu instead.
Otherwise, delegate to `magit-section-toggle'."
  (interactive)
  (cond
   ;; Subscription line
   ((get-text-property (point) 'shipit-repo-subscription)
    (shipit-repo-subscription))
   ;; Load more button
   ((get-text-property (point) 'shipit-repo-load-more)
    (shipit-repo-buffer--load-more-at-point))
   ;; PR item
   ((get-text-property (point) 'shipit-repo-pr-number)
    (let ((number (get-text-property (point) 'shipit-repo-pr-number))
          (repo (or (get-text-property (point) 'shipit-repo-item-repo)
                    shipit-repo-buffer-repo)))
      (shipit-open-pr-buffer number repo)))
   ;; Issue item
   ((get-text-property (point) 'shipit-repo-issue-number)
    (let ((number (get-text-property (point) 'shipit-repo-issue-number))
          (repo (or (get-text-property (point) 'shipit-repo-item-repo)
                    shipit-repo-buffer-repo)))
      (shipit-issues-open-buffer number repo)))
   ;; Discussion item
   ((get-text-property (point) 'shipit-repo-discussion-number)
    (let ((number (get-text-property (point) 'shipit-repo-discussion-number))
          (repo (or (get-text-property (point) 'shipit-repo-item-repo)
                    shipit-repo-buffer-repo)))
      (shipit-discussions-open-buffer number repo)))
   ;; URL/link handling
   (t
    (let ((link (shipit-repo-buffer--link-at-point)))
      (if link
          (pcase (plist-get link :type)
            ('file (find-file (plist-get link :path)))
            ('org-heading (shipit-repo-buffer--goto-org-heading (plist-get link :target)))
            ('org-anchor (shipit-repo-buffer--goto-org-heading (plist-get link :target)))
            ('url (let* ((url (plist-get link :url))
                         (classified (shipit--classify-url url)))
                    (if current-prefix-arg
                        (shipit--generic-url-action-menu url t)
                      (if classified
                          (shipit--open-classified-url classified)
                        (browse-url url))))))
        (let ((section (magit-current-section)))
          (when (and section (not (eq section magit-root-section)))
            (magit-section-toggle section))))))))

;;; Helpers

(defun shipit-repo-buffer--open-in-browser ()
  "Open the current repo in the browser."
  (interactive)
  (let ((url (or (get-text-property (point-min) 'shipit-repo-url)
                 (when shipit-repo-buffer-data
                   (cdr (assq 'html_url shipit-repo-buffer-data))))))
    (if url
        (browse-url url)
      (message "No URL available"))))

(defun shipit-repo-buffer--kill-ring-save ()
  "Like `kill-ring-save', but copy the repo URL when on the header.
With no active region and a `shipit-repo-url' text property at point,
copy that URL into the kill ring.  Otherwise delegate to
`kill-ring-save' (which copies the active region)."
  (interactive)
  (let ((url (and (not (use-region-p))
                  (get-text-property (point) 'shipit-repo-url))))
    (if url
        (progn
          (kill-new url)
          (message "Copied %s" url))
      (call-interactively #'kill-ring-save))))

;;; Item filtering

(defun shipit-repo-buffer--item-matches-filter-p (item)
  "Return non-nil if ITEM matches the current filter criteria.
ITEM is an alist with at least `title' and optionally `user' keys."
  (pcase shipit-repo-buffer--filter-mode
    ('mine (shipit-repo-buffer--item-authored-by-me-p item))
    (_ (or (string-empty-p shipit-repo-buffer--filter-text)
           (let ((title (or (cdr (assq 'title item)) "")))
             (string-match-p (regexp-quote (downcase shipit-repo-buffer--filter-text))
                             (downcase title)))))))

(defun shipit-repo-buffer--get-my-usernames ()
  "Get list of names/usernames that identify the current user.
Includes GitHub username and git user.name for cross-backend matching.
Cached in `shipit-repo-buffer--my-usernames'."
  (unless shipit-repo-buffer--my-usernames
    (let ((names nil))
      (when-let* ((gh (shipit--get-github-username)))
        (push gh names))
      (let ((git-name (string-trim
                       (shell-command-to-string
                        "git config --get user.name 2>/dev/null"))))
        (when (not (string-empty-p git-name))
          (push git-name names)))
      (setq shipit-repo-buffer--my-usernames names)))
  shipit-repo-buffer--my-usernames)

(defun shipit-repo-buffer--item-authored-by-me-p (item)
  "Return non-nil if ITEM was authored by the current user.
Checks against both GitHub username and git user.name to support
items from different backends (GitHub PRs, Jira issues, etc.)."
  (when-let* ((login (cdr (assq 'login (cdr (assq 'user item))))))
    (cl-some (lambda (name) (string-equal-ignore-case login name))
             (shipit-repo-buffer--get-my-usernames))))

(defun shipit-repo-buffer--filter-items (items)
  "Return ITEMS that match the current filter."
  (if (and (null shipit-repo-buffer--filter-mode)
           (string-empty-p shipit-repo-buffer--filter-text))
      items
    (seq-filter #'shipit-repo-buffer--item-matches-filter-p items)))

;;; Filter commands

(defun shipit-repo-buffer-filter ()
  "Filter items in the current PR/issue/discussion section.
Only works when point is within one of these sections."
  (interactive)
  (let ((section-type (shipit-repo-buffer--section-type-at-point)))
    (if section-type
        (progn
          (setq shipit-repo-buffer--filter-section-type section-type)
          (shipit-repo-buffer-filter-transient))
      (user-error "Press 'f' on a PR/issue/discussion section to filter"))))

(defun shipit-repo-buffer--set-filter ()
  "Set filter text for repo buffer items with live updates."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (timer nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-command-hook
                    (lambda ()
                      (when timer (cancel-timer timer))
                      (setq timer
                            (run-with-idle-timer 0.1 nil
                                                 (lambda ()
                                                   (condition-case nil
                                                       (let ((input (minibuffer-contents-no-properties)))
                                                         (when (buffer-live-p original-buffer)
                                                           (with-current-buffer original-buffer
                                                             (setq shipit-repo-buffer--filter-text input)
                                                             (setq shipit-repo-buffer--filter-mode nil)
                                                             (shipit-repo-buffer--refresh-filtered-section)
                                                             (redisplay t))))
                                                     (error nil))))))
                    nil t))
      (let ((new-filter (read-string "Filter items (title): "
                                     shipit-repo-buffer--filter-text)))
        (setq shipit-repo-buffer--filter-text new-filter)
        (setq shipit-repo-buffer--filter-mode nil)
        (shipit-repo-buffer--refresh-filtered-section)
        (message "Filter set to: %s"
                 (if (string-empty-p new-filter) "(none)" new-filter))))))

(defun shipit-repo-buffer--filter-mine ()
  "Filter to show only items authored by me."
  (interactive)
  (setq shipit-repo-buffer--filter-mode 'mine)
  (setq shipit-repo-buffer--filter-text "")
  (setq shipit-repo-buffer--my-usernames nil)  ; refresh username cache
  (shipit-repo-buffer--refresh-filtered-section)
  (message "Showing items authored by me"))

(defun shipit-repo-buffer--clear-filter ()
  "Clear all repo buffer item filters."
  (interactive)
  (setq shipit-repo-buffer--filter-mode nil)
  (setq shipit-repo-buffer--filter-text "")
  (setq shipit-repo-buffer--my-usernames nil)
  (shipit-repo-buffer--refresh-filtered-section)
  (message "Filter cleared"))

(transient-define-prefix shipit-repo-buffer-filter-transient ()
  "Filter items in repo buffer section."
  ["Filter"
   [("f" "By title" shipit-repo-buffer--set-filter)
    ("m" "Mine (authored by me)" shipit-repo-buffer--filter-mine)]
   [("x" "Clear filter" shipit-repo-buffer--clear-filter)
    ("q" "Quit" transient-quit-one)]])

;;; Targeted section refresh for filter

(defun shipit-repo-buffer--refresh-filtered-section ()
  "Refresh the items section with current filter applied.
Uses `shipit-repo-buffer--filter-section-type' to determine which section
to refresh, so this works correctly from minibuffer context."
  (let ((section-type (or shipit-repo-buffer--filter-section-type
                          (shipit-repo-buffer--section-type-at-point))))
    (when section-type
      (shipit-repo-buffer--redraw-section section-type))))

;;; Subscription

(defun shipit--subscription-state-from-api (data)
  "Derive subscription state string from API DATA.
Returns \"watching\", \"ignoring\", or \"participating\".
Checks ignored first since it takes precedence regardless of subscribed."
  (cond
   ((null data) "participating")
   ((eq (cdr (assq 'ignored data)) t)
    "ignoring")
   ((eq (cdr (assq 'subscribed data)) t)
    "watching")
   (t "participating")))

(defun shipit--subscription-state-label (state)
  "Return human-readable label for subscription STATE string."
  (cond
   ((equal state "watching") "All Activity")
   ((equal state "participating") "Participating and @mentions")
   ((equal state "ignoring") "Ignoring")))

(defun shipit-repo-buffer--insert-subscription-line ()
  "Insert the Watching: line in the header if subscription data is available.
Skips rendering when `shipit-repo-buffer-subscription' is `not-supported'."
  (unless (eq shipit-repo-buffer-subscription 'not-supported)
    (let* ((state (shipit--subscription-state-from-api
                   shipit-repo-buffer-subscription))
           (label (shipit--subscription-state-label state))
           (repo (or (bound-and-true-p shipit-repo-buffer-repo) ""))
           (resolved (shipit-pr--resolve-for-repo repo))
           (backend (car resolved))
           (star-fn (plist-get backend :get-repo-starred))
           (starred (when star-fn (funcall star-fn (cdr resolved)))))
      (insert (propertize
               (format "   %s Watching:  %s%s\n"
                       (shipit--get-pr-field-icon "notification" "\U0001f514")
                       label
                       (shipit--star-indicator starred))
               'shipit-repo-subscription t)))))

(defun shipit--repo-get-subscription (repo)
  "Get subscription state for REPO via backend.
Returns the raw API data or nil.
Signals user-error if backend doesn't support subscriptions."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :get-repo-subscription)))
    (unless fn
      (user-error "Subscription management not yet supported for %s backend"
                  (plist-get backend :name)))
    (funcall fn config)))

(defun shipit--repo-set-subscription (repo state)
  "Set subscription STATE for REPO via backend."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (fn (plist-get backend :set-repo-subscription)))
    (unless fn
      (user-error "Subscription management not yet supported for %s backend"
                  (plist-get backend :name)))
    (funcall fn config state)))


(defun shipit--current-buffer-repo ()
  "Return the repository string for the current buffer.
Works in repo, PR, issue, and discussion buffers."
  (or (bound-and-true-p shipit-repo-buffer-repo)
      (bound-and-true-p shipit-buffer-repo)
      (bound-and-true-p shipit-issue-buffer-repo)
      (bound-and-true-p shipit-discussion-buffer-repo)))

(defun shipit--refresh-subscription-in-buffer (repo)
  "Refresh the Watching: line in the current buffer for REPO.
Finds the line by text property and updates the label text in place."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (fn (plist-get backend :get-repo-subscription))
         (sub-data (when fn (funcall fn (cdr resolved))))
         (state (shipit--subscription-state-from-api sub-data))
         (label (shipit--subscription-state-label state))
         (star-fn (plist-get backend :get-repo-starred))
         (starred (when star-fn (funcall star-fn (cdr resolved))))
         (full-label (concat label (shipit--star-indicator starred)))
         (inhibit-read-only t))
    ;; Update repo buffer cache if in repo mode
    (when (derived-mode-p 'shipit-repo-mode)
      (setq shipit-repo-buffer-subscription sub-data))
    ;; Find the existing Watching: line and replace just the label
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "Watching:  " nil t)
        (let ((label-start (point))
              (label-end (line-end-position)))
          (delete-region label-start label-end)
          (insert full-label))))))

(defun shipit--remove-repo-notifications (repo)
  "Remove all notifications for REPO from the in-memory cache.
Re-renders the notifications buffer if it exists."
  (when (and (boundp 'shipit--notification-pr-activities)
             shipit--notification-pr-activities)
    (let ((keys-to-remove nil))
      (maphash (lambda (key activity)
                 (when (equal (cdr (assq 'repo activity)) repo)
                   (push key keys-to-remove)))
               shipit--notification-pr-activities)
      (dolist (key keys-to-remove)
        (remhash key shipit--notification-pr-activities)))
    ;; Re-render notifications buffer if open
    (let ((buf (get-buffer "*shipit-notifications*")))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (shipit-notifications-buffer--rerender))))))

(defun shipit-repo-buffer--subscription-confirm (state)
  "Set subscription to STATE and refresh.
Works from repo, PR, and issue buffers."
  (let ((repo (shipit--current-buffer-repo)))
    (shipit--repo-set-subscription repo state)
    (shipit--refresh-subscription-in-buffer repo)
    ;; When ignoring, mark all repo notifications as read on GitHub
    ;; and remove them from the local cache
    (when (equal state "ignoring")
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (fn (plist-get backend :mark-repo-notifications-read)))
        (when fn (funcall fn (cdr resolved))))
      (shipit--remove-repo-notifications repo))
    ;; Refresh subscriptions buffer if open
    (let ((buf (get-buffer "*shipit-subscriptions*")))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (when (fboundp 'shipit-subscriptions-refresh)
            (shipit-subscriptions-refresh)))))
    (message "Subscription changed to: %s"
             (shipit--subscription-state-label state))))

(defun shipit-repo-buffer--sub-watch ()
  "Set subscription to watching (All Activity)."
  (interactive)
  (shipit-repo-buffer--subscription-confirm "watching"))

(defun shipit-repo-buffer--sub-participating ()
  "Set subscription to participating."
  (interactive)
  (shipit-repo-buffer--subscription-confirm "participating"))

(defun shipit-repo-buffer--sub-ignore ()
  "Set subscription to ignoring."
  (interactive)
  (shipit-repo-buffer--subscription-confirm "ignoring"))

(defvar shipit--subscription-transient-data nil
  "Subscription data fetched when the transient opens.
Dynamically bound during transient display.")

(defvar shipit--subscription-transient-repo nil
  "Repo string for the current subscription transient.
Dynamically bound during transient display.")

(defvar shipit--subscription-transient-starred nil
  "Star status fetched when the transient opens.")

(defvar shipit--subscription-transient-thread-ctx nil
  "Thread context plist for the current subscription transient.
Plist with :repo :type :number :subject, or nil if no thread context.")

(defvar shipit--subscription-transient-thread-state nil
  "Thread subscription state for the current transient.")

(defun shipit--current-buffer-thread-context ()
  "Return thread context plist for the current buffer, or nil.
Checks PR, Issue, and Discussion buffer-local variables."
  (cond
   ((bound-and-true-p shipit-buffer-pr-number)
    (list :repo (bound-and-true-p shipit-buffer-repo)
          :type "pr"
          :number shipit-buffer-pr-number
          :subject (cdr (assq 'title
                              (bound-and-true-p shipit-buffer-pr-data)))))
   ((and (bound-and-true-p shipit-issue-buffer-number)
         ;; Thread subscriptions only make sense for GitHub-tracked issues
         ;; (the API expects a numeric $number).  Non-GitHub backends like
         ;; Jira store their key as a string (e.g. "PRJ-12624").
         (integerp shipit-issue-buffer-number))
    (list :repo (bound-and-true-p shipit-issue-buffer-repo)
          :type "issue"
          :number shipit-issue-buffer-number
          :subject (cdr (assq 'title
                              (bound-and-true-p shipit-issue-buffer-data)))))
   ((bound-and-true-p shipit-discussion-buffer-number)
    (list :repo (bound-and-true-p shipit-discussion-buffer-repo)
          :type "discussion"
          :number shipit-discussion-buffer-number
          :subject (cdr (assq 'title
                              (bound-and-true-p
                               shipit-discussion-buffer-data)))))))

(defun shipit--thread-subscription-supported-p (repo)
  "Return non-nil if the backend for REPO supports thread subscriptions."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved)))
    (plist-get backend :get-thread-subscription)))

(defun shipit--populate-thread-transient-state (repo)
  "Populate thread transient defvars for REPO based on current buffer context.
When the current buffer has a thread context AND REPO's backend supports
thread subscriptions, set `shipit--subscription-transient-thread-ctx' to
the context and fetch the current subscription state.  Otherwise, set
both defvars to nil so the thread group is omitted from the transient."
  (let ((thread-ctx (shipit--current-buffer-thread-context)))
    (if (and thread-ctx
             (shipit--thread-subscription-supported-p repo))
        (progn
          (setq shipit--subscription-transient-thread-ctx thread-ctx)
          (setq shipit--subscription-transient-thread-state
                (shipit--get-thread-subscription
                 repo
                 (plist-get thread-ctx :type)
                 (plist-get thread-ctx :number))))
      (setq shipit--subscription-transient-thread-ctx nil)
      (setq shipit--subscription-transient-thread-state nil))))

(defun shipit-repo-buffer--sub-toggle-thread ()
  "Toggle thread subscription for the current thread context.
Signals a `user-error' if the backend call fails (returns nil), so we do
not update local state or print a misleading success message."
  (interactive)
  (let* ((ctx shipit--subscription-transient-thread-ctx)
         (repo (plist-get ctx :repo))
         (type (plist-get ctx :type))
         (number (plist-get ctx :number))
         (currently-subscribed
          (equal shipit--subscription-transient-thread-state "subscribed"))
         (new-subscribed (not currently-subscribed)))
    (unless (shipit--set-thread-subscription repo type number new-subscribed)
      (user-error "Thread subscription failed"))
    (setq shipit--subscription-transient-thread-state
          (if new-subscribed "subscribed" "unsubscribed"))
    (message "%s %s #%d"
             (if new-subscribed "Subscribed to" "Unsubscribed from")
             type number)))

(defun shipit-repo-buffer--sub-toggle-mute-non-authored-pr-noise-thread ()
  "Toggle the current PR thread in `shipit-notifications-mute-non-authored-pr-noise-threads\='.
Uses the (REPO . NUMBER) pair from
`shipit--subscription-transient-thread-ctx\='; only available
when the watch transient was opened on a PR notification entry.
Persists the new list and re-runs the auto-mark rules
immediately so newly-matching unread items disappear."
  (interactive)
  (let* ((repo shipit--subscription-transient-repo)
         (ctx shipit--subscription-transient-thread-ctx)
         (number (and ctx (plist-get ctx :number)))
         (key (and repo (integerp number) (cons repo number)))
         (current shipit-notifications-mute-non-authored-pr-noise-threads)
         (active (and key (member key current)))
         (new (cond
               ((null key) current)
               (active (cl-remove key current :test #'equal))
               (t (append current (list key))))))
    (unless key
      (user-error "No PR thread context in this transient"))
    (customize-save-variable
     'shipit-notifications-mute-non-authored-pr-noise-threads new)
    (let ((marked (and (fboundp 'shipit--auto-mark-rules-apply)
                       (shipit--auto-mark-rules-apply))))
      (when (fboundp 'shipit--rerender-notifications-buffer-if-visible)
        (shipit--rerender-notifications-buffer-if-visible))
      (message "Mute non-authored noise on %s#%d: %s%s"
               repo number
               (if active "off" "on")
               (if (and marked (> marked 0))
                   (format " (marked %d unread as read)" marked)
                 "")))))

(defun shipit-repo-buffer--sub-toggle-mute-non-authored-pr-noise ()
  "Toggle the current repo in `shipit-notifications-mute-non-authored-pr-noise-repos\='.
Adds the repo if absent, removes it if present.  Persists the
new list via `customize-save-variable\=' and immediately re-runs
`shipit--auto-mark-rules-apply\=' so any newly-matching unread
items disappear from the visible buffer."
  (interactive)
  (let* ((repo shipit--subscription-transient-repo)
         (current shipit-notifications-mute-non-authored-pr-noise-repos)
         (active (member repo current))
         (new (if active
                  (remove repo current)
                (append current (list repo)))))
    (unless repo
      (user-error "No repo context in this transient"))
    (customize-save-variable
     'shipit-notifications-mute-non-authored-pr-noise-repos new)
    (let ((marked (and (fboundp 'shipit--auto-mark-rules-apply)
                       (shipit--auto-mark-rules-apply))))
      (when (fboundp 'shipit--rerender-notifications-buffer-if-visible)
        (shipit--rerender-notifications-buffer-if-visible))
      (message "Mute non-authored PR noise for %s: %s%s"
               repo
               (if active "off" "on")
               (if (and marked (> marked 0))
                   (format " (marked %d unread as read)" marked)
                 "")))))

(defun shipit-repo-buffer--sub-toggle-star ()
  "Toggle star status for the current repo."
  (interactive)
  (let* ((repo shipit--subscription-transient-repo)
         (resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (set-fn (plist-get backend :set-repo-starred))
         (new-state (not shipit--subscription-transient-starred)))
    (when set-fn
      (funcall set-fn config new-state)
      (setq shipit--subscription-transient-starred new-state)
      (message "%s %s" (if new-state "Starred" "Unstarred") repo))))

(transient-define-prefix shipit-repo-subscription ()
  "Manage repository notification subscription."
  [:class transient-column
   :description
   (lambda ()
     (let* ((state (shipit--subscription-state-from-api
                    shipit--subscription-transient-data))
            (label (shipit--subscription-state-label state)))
       (format "Subscription for %s%s\n\n  Current: %s"
               (or shipit--subscription-transient-repo "?")
               (shipit--star-indicator shipit--subscription-transient-starred)
               label)))
   :setup-children
   (lambda (_)
     (let* ((current (shipit--subscription-state-from-api
                      shipit--subscription-transient-data))
            (sub-entries
             (mapcar
              (lambda (entry)
                (let* ((key (nth 0 entry))
                       (state (nth 1 entry))
                       (cmd (nth 2 entry))
                       (label (shipit--subscription-state-label state))
                       (label (if (equal state current)
                                  (concat label "  "
                                          (propertize "<- current"
                                                      'face 'success))
                                label)))
                  (transient-parse-suffix
                   transient--prefix
                   (list key label cmd))))
              '(("w" "watching" shipit-repo-buffer--sub-watch)
                ("p" "participating" shipit-repo-buffer--sub-participating)
                ("i" "ignoring" shipit-repo-buffer--sub-ignore))))
            ;; Star toggle suffix (conditional on backend support)
            (star-entry
             (let* ((resolved (shipit-pr--resolve-for-repo
                               (or shipit--subscription-transient-repo "")))
                    (backend (car resolved))
                    (has-star (plist-get backend :set-repo-starred)))
               (when has-star
                 (list (transient-parse-suffix
                        transient--prefix
                        (list "s"
                              (if shipit--subscription-transient-starred
                                  "Unstar repo"
                                "Star repo")
                              'shipit-repo-buffer--sub-toggle-star))))))
            ;; Per-repo noise-mute toggle.
            (noise-entry
             (let* ((repo shipit--subscription-transient-repo)
                    (active (and repo
                                 (member repo shipit-notifications-mute-non-authored-pr-noise-repos))))
               (list (transient-parse-suffix
                      transient--prefix
                      (list "M"
                            (format "Mute non-authored PR noise here [%s]"
                                    (if active "on" "off"))
                            'shipit-repo-buffer--sub-toggle-mute-non-authored-pr-noise)))))
            ;; Thread subscription suffixes (conditional on thread context)
            (thread-entries
             (when shipit--subscription-transient-thread-ctx
               (let* ((ctx shipit--subscription-transient-thread-ctx)
                      (type (plist-get ctx :type))
                      (number (plist-get ctx :number))
                      (subject (plist-get ctx :subject))
                      (repo shipit--subscription-transient-repo)
                      (state shipit--subscription-transient-thread-state)
                      (subscribed (equal state "subscribed"))
                      (thread-noise-active
                       (and (equal type "pr") repo (integerp number)
                            (member (cons repo number)
                                    shipit-notifications-mute-non-authored-pr-noise-threads)))
                      (desc (format "This %s: #%d%s  [%s]"
                                    type
                                    number
                                    (if subject
                                        (format " %s" subject)
                                      "")
                                    (or state "unknown"))))
                 (append
                  (list (transient-parse-suffix
                         transient--prefix
                         (list :info desc))
                        (transient-parse-suffix
                         transient--prefix
                         (list "t"
                               (if subscribed "Unsubscribe" "Subscribe")
                               'shipit-repo-buffer--sub-toggle-thread)))
                  (when (equal type "pr")
                    (list (transient-parse-suffix
                           transient--prefix
                           (list "T"
                                 (format "Mute non-authored noise on this PR [%s]"
                                         (if thread-noise-active "on" "off"))
                                 'shipit-repo-buffer--sub-toggle-mute-non-authored-pr-noise-thread)))))))))
       (append sub-entries star-entry noise-entry thread-entries)))]
  (interactive)
  (let ((repo (shipit--current-buffer-repo)))
    (unless repo
      (user-error "No repository context in this buffer"))
    (let* ((resolved (shipit-pr--resolve-for-repo repo))
           (backend (car resolved))
           (fn (plist-get backend :get-repo-subscription)))
      (unless fn
        (user-error "Subscription management not yet supported for %s backend"
                    (plist-get backend :name)))
      (setq shipit--subscription-transient-repo repo)
      (setq shipit--subscription-transient-data
            (funcall fn (cdr resolved)))
      (setq shipit--subscription-transient-starred
            (let ((star-fn (plist-get backend :get-repo-starred)))
              (when star-fn (funcall star-fn (cdr resolved))))))
    ;; Populate thread transient state (ctx + state) for the thread group.
    ;; If the backend does not support thread subscriptions, both defvars
    ;; are cleared so the thread group is omitted from the transient.
    (shipit--populate-thread-transient-state repo)
    (transient-setup 'shipit-repo-subscription)))

(defun shipit-repo-buffer-dwim ()
  "Context-sensitive action in repo buffer."
  (interactive)
  (cond
   ((get-text-property (point) 'shipit-repo-subscription)
    (shipit-repo-subscription))
   (t
    (shipit-repo-buffer--ret-action))))

(defun shipit-repo-buffer--goto-org-heading (target)
  "Move point to the rendered org heading matching TARGET, if any.
Pushes the previous point onto the mark ring (and the global mark
ring) before moving so navigation back via `pop-mark', `consult-mark',
or \\[set-mark-command] with prefix arg works.  Echoes a message
when no matching heading is found in the buffer."
  (let ((pos (and target (shipit-repo-buffer--find-org-heading target))))
    (if pos
        (progn
          (push-mark nil t)
          (when (fboundp 'xref-push-marker-stack)
            (xref-push-marker-stack))
          (goto-char pos)
          (when (get-buffer-window (current-buffer)) (recenter 0)))
      (message "Heading %S not found in buffer" target))))

(defun shipit-repo-buffer--org-level-from-face (flf)
  "Extract heading level from FLF (a `font-lock-face' value)."
  (let ((sym (cond ((symbolp flf) flf)
                   ((listp flf)
                    (cl-find-if (lambda (s)
                                  (and (symbolp s)
                                       (string-prefix-p "org-level-"
                                                        (symbol-name s))))
                                flf)))))
    (when (and sym (symbolp sym))
      (let ((name (symbol-name sym)))
        (when (string-match "\\`org-level-\\([0-9]+\\)" name)
          (string-to-number (match-string 1 name)))))))

(defun shipit-repo-buffer--org-heading-level-at (pos)
  "Return the org heading level on the line at POS (1-9), or nil.
Scans the whole line for any character carrying an `org-level-N'
`font-lock-face' — necessary because the README pipeline prefixes
each line with literal whitespace that has no face, so the leading
columns of the line do not carry the heading face."
  (save-excursion
    (goto-char pos)
    (let ((bol (line-beginning-position))
          (eol (line-end-position))
          level scan)
      (setq scan bol)
      (while (and (not level) (< scan eol))
        (let ((flf (get-text-property scan 'font-lock-face)))
          (setq level (shipit-repo-buffer--org-level-from-face flf)))
        (setq scan (or (next-single-property-change scan 'font-lock-face nil eol)
                       eol)))
      level)))

(defun shipit-repo-buffer--org-subtree-end (start-level)
  "Return the position where the subtree of START-LEVEL ends.
Scans forward from current point for the next org heading whose level
is <= START-LEVEL.  Returns the position just before that heading, or
`point-max' if no such heading is found."
  (save-excursion
    (forward-line 1)
    (let (found)
      (while (and (not found) (< (point) (point-max)))
        (let ((level (shipit-repo-buffer--org-heading-level-at
                      (line-beginning-position))))
          (if (and level (<= level start-level))
              (setq found (line-beginning-position))
            (forward-line 1))))
      (or found (point-max)))))

(defun shipit-repo-buffer--detect-org-line-level (text pos)
  "Return the org heading level for the line at POS in TEXT, or nil.
A heading line starts with one or more asterisks followed by
whitespace.  Detection runs on text, not text properties, so it
is robust to whatever propertization the renderer + wrapping leaves
behind."
  (let* ((nl (or (string-match "\n" text pos) (length text)))
         (line (substring-no-properties text pos nl)))
    (when (string-match "\\`\\(\\*+\\)[ \t]+" line)
      (length (match-string 1 line)))))

(defun shipit-repo-buffer--strip-org-heading-markup (line)
  "Return LINE with the leading `*+ ' markup and trailing `:tags:' stripped."
  (let ((s line))
    (when (string-match "\\`[ \t]*\\*+[ \t]+\\(.*\\)\\'" s)
      (setq s (match-string 1 s)))
    (when (string-match "\\`\\(.*?\\)[ \t]+:[A-Za-z0-9_@:]+:[ \t]*\\'" s)
      (setq s (match-string 1 s)))
    (string-trim-right s)))

(defun shipit-repo-buffer--detect-markdown-line-level (text pos)
  "Return the markdown ATX heading level for the line at POS in TEXT, or nil.
A heading line starts with one to six `#' characters followed by whitespace
(`# ' through `###### ').  Setext-style underlines are not recognized."
  (let* ((nl (or (string-match "\n" text pos) (length text)))
         (line (substring-no-properties text pos nl)))
    (when (string-match "\\`\\(#\\{1,6\\}\\)[ \t]+" line)
      (length (match-string 1 line)))))

(defun shipit-repo-buffer--strip-markdown-heading-markup (line)
  "Return LINE with the leading `# ' markup and any trailing `#'s stripped.
Trailing `#'s on ATX headings are part of the syntax (`# Title #') and not
part of the title text."
  (let ((s line))
    (when (string-match "\\`[ \t]*#\\{1,6\\}[ \t]+\\(.*\\)\\'" s)
      (setq s (match-string 1 s)))
    (when (string-match "\\`\\(.*?\\)[ \t]+#+[ \t]*\\'" s)
      (setq s (match-string 1 s)))
    (string-trim-right s)))

(defun shipit-repo-buffer--line-end-after (text pos)
  "Return the position just AFTER the newline ending the line at POS in TEXT."
  (let ((nl (string-match "\n" text pos)))
    (if nl (1+ nl) (length text))))

(defun shipit-repo-buffer--next-heading-pos (text start max-level detect-fn)
  "Return position of next heading at or after START in TEXT.
DETECT-FN takes (TEXT POS) and returns the heading level or nil.
If MAX-LEVEL is non-nil, only headings at level <= MAX-LEVEL count.
If MAX-LEVEL is nil, any heading line counts.
Returns the length of TEXT when no matching heading is found."
  (let ((pos start)
        (text-len (length text))
        found)
    (while (and (not found) (< pos text-len))
      (let ((level (funcall detect-fn text pos)))
        (if (and level (or (null max-level) (<= level max-level)))
            (setq found pos)
          (setq pos (shipit-repo-buffer--line-end-after text pos)))))
    (or found text-len)))

(defun shipit-repo-buffer--insert-content-with-indent (text indent)
  "Insert TEXT into the current buffer with INDENT spaces prepended to each line.
Preserves text properties from TEXT.  Always leaves point at BOL so a
subsequent `magit-insert-section' begins on a fresh line — otherwise
trailing whitespace from TEXT (e.g. an indented blank line) would
absorb the next section's heading column."
  (when (and text (> (length text) 0))
    (let* ((indent-str (make-string indent ?\s))
           (lines (split-string text "\n"))
           (indented (mapconcat (lambda (l) (concat indent-str l)) lines "\n")))
      (insert indented)
      (unless (bolp) (insert "\n")))))

(defun shipit-repo-buffer--insert-as-sections
    (text indent section-type detect-fn strip-fn &optional content-fn)
  "Insert TEXT into the current buffer as nested magit sections.
Heading lines split TEXT into sections at the level of the first heading
found.  Deeper headings inside a section's body are processed by a
recursive call.

SECTION-TYPE is the symbol to pass to `magit-insert-section'.
DETECT-FN takes (TEXT POS) → heading level or nil.
STRIP-FN takes a heading line → cleaned title text.
CONTENT-FN, when non-nil, is called as (CONTENT-FN BODY INDENT)
to insert non-heading body text and the preamble before the
first heading.  Defaults to `--insert-content-with-indent' which
preserves the input verbatim."
  (let* ((text-len (length text))
         (first-pos (shipit-repo-buffer--next-heading-pos text 0 nil detect-fn))
         (section-level (and (< first-pos text-len)
                             (funcall detect-fn text first-pos)))
         (insert-content (or content-fn
                             #'shipit-repo-buffer--insert-content-with-indent)))
    (cond
     ((not section-level)
      (when (> (length text) 0)
        (funcall insert-content text indent)))
     (t
      (when (> first-pos 0)
        (let ((preamble (substring text 0 first-pos)))
          (when (> (length preamble) 0)
            (funcall insert-content preamble indent))))
      (let ((pos first-pos))
        (while (< pos text-len)
          (let* ((line-end (or (string-match "\n" text pos) text-len))
                 (next-line (shipit-repo-buffer--line-end-after text pos))
                 (heading-line (substring text pos line-end))
                 (section-end (shipit-repo-buffer--next-heading-pos
                               text next-line section-level detect-fn))
                 (body (substring text next-line section-end))
                 (title (funcall strip-fn heading-line)))
            (magit-insert-section ((eval section-type) nil)
              (magit-insert-heading
                (concat (make-string indent ?\s)
                        (propertize title 'font-lock-face 'magit-section-heading)))
              (magit-insert-section-body
                (when (> (length body) 0)
                  (shipit-repo-buffer--insert-as-sections
                   body (+ indent 2) section-type detect-fn strip-fn content-fn))
                (unless (and (> (point) 1) (eq (char-before) ?\n))
                  (insert "\n"))))
            (setq pos section-end))))))))

(defun shipit-repo-buffer--insert-org-as-sections (text indent)
  "Insert TEXT into the current buffer as nested magit sections of type
`org-heading'.  Each `* heading' line opens a section whose body and
sub-headings are nested inside.  Text before the first heading is
inserted as plain content."
  (shipit-repo-buffer--insert-as-sections
   text indent 'org-heading
   #'shipit-repo-buffer--detect-org-line-level
   #'shipit-repo-buffer--strip-org-heading-markup))

(defun shipit-repo-buffer--insert-markdown-as-sections (text indent &optional content-fn)
  "Insert TEXT into the current buffer as nested magit sections of type
`markdown-heading'.  Each `# heading' line opens a section whose body
and sub-headings are nested inside.  Text before the first heading is
inserted as plain content.  Only ATX-style headings (`# ' through
`###### ') are recognized; setext-style underlines are treated as body.
CONTENT-FN, when supplied, is used in place of the default plain
inserter to render non-heading text (e.g. apply markdown
font-lock + wrapping for issue descriptions)."
  (shipit-repo-buffer--insert-as-sections
   text indent 'markdown-heading
   #'shipit-repo-buffer--detect-markdown-line-level
   #'shipit-repo-buffer--strip-markdown-heading-markup
   content-fn))

(provide 'shipit-repo-buffer)
;;; shipit-repo-buffer.el ends here
