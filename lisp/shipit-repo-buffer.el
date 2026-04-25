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
(declare-function shipit--convert-html-tables-to-markdown "shipit-render")
(declare-function shipit--convert-html-lists-to-markdown "shipit-render")
(declare-function shipit--generic-url-action-menu "shipit-render")
(declare-function shipit--classify-url "shipit-render")
(declare-function shipit--open-classified-url "shipit-render")

;; Forward declarations — issue/discussion backends (optional features)
(declare-function shipit-issue--resolve-for-repo "shipit-issue-backends")
(declare-function shipit-discussion--search "shipit-discussions-graphql")
(declare-function shipit-open-pr-buffer "shipit-buffer")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit-discussions-open-buffer "shipit-discussions-buffer")
(declare-function shipit--find-section-by-type "shipit-sections")
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

(defun repo-root (&rest _args)
  "Magit section identifier for repo root.")
(put 'repo-root 'magit-section t)

(defun repo-header (&rest _args)
  "Magit section identifier for repo header.")
(put 'repo-header 'magit-section t)

(defun repo-readme (&rest _args)
  "Magit section identifier for repo README.")
(put 'repo-readme 'magit-section t)

(defun repo-languages (&rest _args)
  "Magit section identifier for repo languages.")
(put 'repo-languages 'magit-section t)

(defun repo-prs (&rest _args)
  "Magit section identifier for repo PRs.")
(put 'repo-prs 'magit-section t)

(defun repo-prs-open (&rest _args)
  "Magit section identifier for open PRs.")
(put 'repo-prs-open 'magit-section t)

(defun repo-prs-closed (&rest _args)
  "Magit section identifier for closed PRs.")
(put 'repo-prs-closed 'magit-section t)

(defun repo-issues (&rest _args)
  "Magit section identifier for repo issues.")
(put 'repo-issues 'magit-section t)

(defun repo-issues-open (&rest _args)
  "Magit section identifier for open issues.")
(put 'repo-issues-open 'magit-section t)

(defun repo-issues-closed (&rest _args)
  "Magit section identifier for closed issues.")
(put 'repo-issues-closed 'magit-section t)

(defun repo-discussions (&rest _args)
  "Magit section identifier for repo discussions.")
(put 'repo-discussions 'magit-section t)

;;; Buffer-local variables

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

;;; Mode definition

(defvar shipit-repo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
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
Languages, PRs, issues, and discussions sections are collapsed by default."
  (when (memq (oref section type)
              '(repo-languages repo-prs repo-issues repo-discussions))
    'hide))

(define-derived-mode shipit-repo-mode magit-section-mode "Shipit-Repo"
  "Major mode for shipit repository landing page buffers.
Displays repository metadata and README.

\\{shipit-repo-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-repo-buffer-refresh)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (add-hook 'magit-section-set-visibility-hook
            #'shipit-repo-buffer--section-visibility nil t)
  (setq-local magit-root-section nil)
  (shipit--apply-section-defaults))

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

(defun shipit-repo-buffer--fetch-open-prs (repo)
  "Fetch open PRs for REPO via backend :search.
Returns list of PR alists, or nil on failure.
Sets `shipit-repo-buffer-open-prs-has-more' as side effect."
  (condition-case nil
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (search-fn (plist-get backend :search))
             (limit 100))
        (when search-fn
          (let ((result (funcall search-fn config
                                (list "--state=open" "--sort=updated"
                                      (format "--limit=%d" limit)))))
            (setq shipit-repo-buffer-open-prs-has-more
                  (= (length result) limit))
            (shipit--debug-log "REPO-BUFFER: fetch-open-prs count=%d limit=%d has-more=%s"
                               (length result) limit shipit-repo-buffer-open-prs-has-more)
            result)))
    (error nil)))

(defun shipit-repo-buffer--fetch-closed-prs (repo)
  "Fetch closed PRs for REPO, up to `shipit-repo-buffer-closed-limit'.
Returns list of PR alists, or nil on failure.
Sets `shipit-repo-buffer-closed-prs-has-more' as side effect."
  (condition-case nil
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (search-fn (plist-get backend :search))
             (limit (or shipit-repo-buffer-closed-limit 5)))
        (when search-fn
          (let ((result (funcall search-fn config
                                (list "--state=closed" "--sort=updated"
                                      (format "--limit=%d" limit)))))
            (setq shipit-repo-buffer-closed-prs-has-more
                  (= (length result) limit))
            result)))
    (error nil)))

(defun shipit-repo-buffer--fetch-open-issues (repo)
  "Fetch open issues for REPO via issue backend :search.
Returns list of issue alists, or nil on failure.
Sets `shipit-repo-buffer-open-issues-has-more' as side effect."
  (when (and (boundp 'shipit-issues-enabled) shipit-issues-enabled
             (fboundp 'shipit-issue--resolve-for-repo)
             (shipit-repo-buffer--issue-backend-available-p))
    (condition-case nil
        (let* ((resolved (shipit-issue--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (search-fn (plist-get backend :search))
               (limit 100))
          (when search-fn
            (let ((result (funcall search-fn config
                                  (list "--state=open" "--sort=updated"
                                        (format "--limit=%d" limit)))))
              (setq shipit-repo-buffer-open-issues-has-more
                    (= (length result) limit))
              (shipit--debug-log "REPO-BUFFER: fetch-open-issues count=%d limit=%d has-more=%s"
                                 (length result) limit shipit-repo-buffer-open-issues-has-more)
              result)))
      (error nil))))

(defun shipit-repo-buffer--fetch-closed-issues (repo)
  "Fetch closed issues for REPO, up to `shipit-repo-buffer-closed-limit'.
Returns list of issue alists, or nil on failure.
Sets `shipit-repo-buffer-closed-issues-has-more' as side effect."
  (when (and (boundp 'shipit-issues-enabled) shipit-issues-enabled
             (fboundp 'shipit-issue--resolve-for-repo)
             (shipit-repo-buffer--issue-backend-available-p))
    (condition-case nil
        (let* ((resolved (shipit-issue--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved))
               (search-fn (plist-get backend :search))
               (limit (or shipit-repo-buffer-closed-limit 5)))
          (when search-fn
            (let ((result (funcall search-fn config
                                  (list "--state=closed" "--sort=updated"
                                        (format "--limit=%d" limit)))))
              (setq shipit-repo-buffer-closed-issues-has-more
                    (= (length result) limit))
              result)))
      (error nil))))

(defun shipit-repo-buffer--fetch-discussions (repo)
  "Fetch recent discussions for REPO.
Returns list of discussion alists, or nil on failure."
  (when (and (boundp 'shipit-discussions-enabled) shipit-discussions-enabled
             (fboundp 'shipit-discussion--search)
             (shipit-repo-buffer--discussions-available-p))
    (condition-case nil
        (shipit-discussion--search
         repo (list "--limit=25"))
      (error nil))))

;;; Entry point

;;;###autoload
(defun shipit-open-repo-buffer (repo &optional backend-id backend-config)
  "Open a dedicated landing page buffer for REPO (owner/name).
Creates or reuses the buffer *shipit-repo: REPO*.
BACKEND-ID and BACKEND-CONFIG override auto-detection when provided."
  (interactive
   (list (read-string "Repository (owner/repo): "
                      (shipit--get-repo-from-remote))))
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

(defun shipit-repo-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the repo buffer contents."
  (interactive)
  (let ((repo shipit-repo-buffer-repo))
    (shipit--debug-log "Refreshing repo buffer for %s" repo)
    (message "Loading repo %s..." repo)
    (let ((result (shipit-repo-buffer--fetch-repo-data repo)))
      (setq shipit-repo-buffer-data (plist-get result :data))
      (setq shipit-repo-buffer-readme (plist-get result :readme))
      (setq shipit-repo-buffer-languages (plist-get result :languages))
      (setq shipit-repo-buffer-subscription
            (let* ((resolved (shipit-pr--resolve-for-repo repo))
                   (backend (car resolved))
                   (fn (plist-get backend :get-repo-subscription)))
              (if fn
                  (funcall fn (cdr resolved))
                'not-supported)))
      (setq shipit-repo-buffer-open-prs (shipit-repo-buffer--fetch-open-prs repo))
      (setq shipit-repo-buffer-closed-prs (shipit-repo-buffer--fetch-closed-prs repo))
      (setq shipit-repo-buffer-open-issues (shipit-repo-buffer--fetch-open-issues repo))
      (setq shipit-repo-buffer-closed-issues (shipit-repo-buffer--fetch-closed-issues repo))
      (setq shipit-repo-buffer-discussions (shipit-repo-buffer--fetch-discussions repo))
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (magit-insert-section (repo-root)
          (let ((lang-pcts (shipit-repo-buffer--format-language-percentages
                            shipit-repo-buffer-languages)))
            (shipit-repo-buffer--insert-header-section
             shipit-repo-buffer-data lang-pcts))
          (shipit-repo-buffer--insert-readme-section shipit-repo-buffer-readme)
          (shipit-repo-buffer--insert-prs-section
           shipit-repo-buffer-open-prs shipit-repo-buffer-closed-prs
           shipit-repo-buffer-closed-prs-has-more repo
           shipit-repo-buffer-open-prs-has-more)
          (when (and (boundp 'shipit-issues-enabled) shipit-issues-enabled)
            (shipit-repo-buffer--insert-issues-section
             shipit-repo-buffer-open-issues shipit-repo-buffer-closed-issues
             shipit-repo-buffer-closed-issues-has-more repo
             shipit-repo-buffer-open-issues-has-more))
          (when (and (boundp 'shipit-discussions-enabled) shipit-discussions-enabled)
            (shipit-repo-buffer--insert-discussions-section
             shipit-repo-buffer-discussions repo)))
        (goto-char (min pos (point-max)))))
    (message "Repo %s loaded" repo)))

;;; Section rendering

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
      (when description
        (insert (format "   %s %s\n"
                        (shipit--get-pr-field-icon "description" "\U0001f4dd")
                        description)))
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
                 (format (shipit-repo-buffer--readme-format readme))
                 (readme-start (point)))
            (if (and (eq format 'markdown)
                     (string-match-p "<details>" readme-text))
                (shipit--insert-body-with-details readme-text 3)
              (let* ((rendered (if (fboundp 'shipit--render-body)
                                   (shipit--render-body readme-text format)
                                 readme-text))
                     (wrapped (if (fboundp 'shipit--wrap-text)
                                  (shipit--strip-face-on-whitespace (shipit--wrap-text rendered 80 0))
                                rendered)))
                (insert (concat "   "
                                (replace-regexp-in-string "\n" "\n   " wrapped)
                                "\n"))))
            (when (fboundp 'shipit--create-generic-url-overlays)
              (shipit--create-generic-url-overlays readme-start (point)))
            (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
              (shipit--apply-code-block-backgrounds-in-region readme-start (point))))))
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

(defun shipit-repo-buffer--insert-prs-section (open-prs closed-prs closed-has-more repo
                                                       &optional open-has-more)
  "Insert PRs section with OPEN-PRS and CLOSED-PRS subsections for REPO.
CLOSED-HAS-MORE, when non-nil, adds a \"load more\" line to the closed subsection.
OPEN-HAS-MORE, when non-nil, adds a \"load more\" line to the open subsection."
  (when (or open-prs closed-prs)
    (shipit--debug-log "REPO-BUFFER: insert-prs-section open=%d closed=%d closed-has-more=%s open-has-more=%s"
                       (length open-prs) (length closed-prs) closed-has-more open-has-more)
    (let ((label (shipit-repo-buffer--pr-type-label)))
      (magit-insert-section (repo-prs nil nil)
        (magit-insert-heading
          (format "%s %s"
                  (shipit--get-pr-field-icon "pull-request" "\U0001f501")
                  (propertize label 'font-lock-face 'magit-section-heading)))
        (magit-insert-section-body
          (when open-prs
            (magit-insert-section (repo-prs-open nil nil)
              (magit-insert-heading
                (propertize (format "  Open (%d)" (length open-prs))
                            'font-lock-face 'magit-section-heading))
              (dolist (pr open-prs)
                (shipit-repo-buffer--insert-item-line
                 pr repo 'shipit-repo-pr-number))
              (when open-has-more
                (shipit-repo-buffer--insert-load-more-line 'open-prs))))
          (when closed-prs
            (magit-insert-section (repo-prs-closed nil nil)
              (magit-insert-heading
                (propertize (format "  Closed (%d)" (length closed-prs))
                            'font-lock-face 'magit-section-heading))
              (dolist (pr closed-prs)
                (shipit-repo-buffer--insert-item-line
                 pr repo 'shipit-repo-pr-number))
              (when closed-has-more
                (shipit-repo-buffer--insert-load-more-line 'prs))))
          (insert "\n"))))))

(defun shipit-repo-buffer--insert-issues-section (open-issues closed-issues closed-has-more repo
                                                             &optional open-has-more)
  "Insert issues section with OPEN-ISSUES and CLOSED-ISSUES subsections for REPO.
CLOSED-HAS-MORE, when non-nil, adds a \"load more\" line to the closed subsection.
OPEN-HAS-MORE, when non-nil, adds a \"load more\" line to the open subsection."
  (when (or open-issues closed-issues)
    (magit-insert-section (repo-issues nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "issue-open" "\U0001f41b")
                (propertize "Issues" 'font-lock-face 'magit-section-heading)))
      (magit-insert-section-body
        (when open-issues
          (magit-insert-section (repo-issues-open nil nil)
            (magit-insert-heading
              (propertize (format "  Open (%d)" (length open-issues))
                          'font-lock-face 'magit-section-heading))
            (dolist (issue open-issues)
              (shipit-repo-buffer--insert-item-line
               issue repo 'shipit-repo-issue-number))
            (when open-has-more
              (shipit-repo-buffer--insert-load-more-line 'open-issues))))
        (when closed-issues
          (magit-insert-section (repo-issues-closed nil nil)
            (magit-insert-heading
              (propertize (format "  Closed (%d)" (length closed-issues))
                          'font-lock-face 'magit-section-heading))
            (dolist (issue closed-issues)
              (shipit-repo-buffer--insert-item-line
               issue repo 'shipit-repo-issue-number))
            (when closed-has-more
              (shipit-repo-buffer--insert-load-more-line 'issues))))
        (insert "\n")))))

(defun shipit-repo-buffer--insert-discussions-section (discussions repo)
  "Insert collapsible discussions section with DISCUSSIONS data for REPO.
DISCUSSIONS is a list of alists with `number' and `title' keys."
  (when discussions
    (magit-insert-section (repo-discussions nil nil)
      (magit-insert-heading
        (format "%s %s"
                (shipit--get-pr-field-icon "discussion" "\U0001f4ac")
                (propertize (format "Discussions (%d)" (length discussions))
                            'font-lock-face 'magit-section-heading)))
      (magit-insert-section-body
        (dolist (disc discussions)
          (shipit-repo-buffer--insert-item-line
           disc repo 'shipit-repo-discussion-number))
        (insert "\n")))))

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
  "Fetch more items of TYPE.
TYPE is `prs', `issues', `open-prs', or `open-issues'.
COUNT, if non-nil, overrides the default batch size.
Updates the buffer-local item list and has-more flag."
  (let* ((repo shipit-repo-buffer-repo)
         (openp (memq type '(open-prs open-issues)))
         (state (if openp "open" "closed"))
         (default-batch (if openp 100 (or shipit-repo-buffer-closed-limit 5)))
         (batch-size (or count default-batch)))
    (pcase type
      ((or 'prs 'open-prs)
       (let* ((current (if openp shipit-repo-buffer-open-prs
                         shipit-repo-buffer-closed-prs))
              (current-count (length current))
              (resolved (shipit-pr--resolve-for-repo repo))
              (backend (car resolved))
              (config (cdr resolved))
              (search-fn (plist-get backend :search))
              (new-limit (+ current-count batch-size)))
         (when search-fn
           (let ((result (funcall search-fn config
                                 (list (format "--state=%s" state)
                                       "--sort=updated"
                                       (format "--limit=%d" new-limit)))))
             (if openp
                 (progn
                   (setq shipit-repo-buffer-open-prs result)
                   (setq shipit-repo-buffer-open-prs-has-more
                         (= (length result) new-limit)))
               (setq shipit-repo-buffer-closed-prs result)
               (setq shipit-repo-buffer-closed-prs-has-more
                     (= (length result) new-limit)))))))
      ((or 'issues 'open-issues)
       (when (and (boundp 'shipit-issues-enabled) shipit-issues-enabled
                  (fboundp 'shipit-issue--resolve-for-repo))
         (let* ((current (if openp shipit-repo-buffer-open-issues
                           shipit-repo-buffer-closed-issues))
                (current-count (length current))
                (resolved (shipit-issue--resolve-for-repo repo))
                (backend (car resolved))
                (config (cdr resolved))
                (search-fn (plist-get backend :search))
                (new-limit (+ current-count batch-size)))
           (when search-fn
             (let ((result (funcall search-fn config
                                   (list (format "--state=%s" state)
                                         "--sort=updated"
                                         (format "--limit=%d" new-limit)))))
               (if openp
                   (progn
                     (setq shipit-repo-buffer-open-issues result)
                     (setq shipit-repo-buffer-open-issues-has-more
                           (= (length result) new-limit)))
                 (setq shipit-repo-buffer-closed-issues result)
                 (setq shipit-repo-buffer-closed-issues-has-more
                       (= (length result) new-limit)))))))))))

;; Backward compat alias
(defalias 'shipit-repo-buffer--fetch-more-closed
  'shipit-repo-buffer--fetch-more)

(defun shipit-repo-buffer--refresh-closed-subsection (section-type items has-more repo text-prop load-more-type)
  "Targeted refresh of an open or closed subsection.
SECTION-TYPE is the magit section type (e.g., `repo-prs-closed').
ITEMS is the new list of items.
HAS-MORE controls whether to show the load-more line.
REPO is the repository name for text properties.
TEXT-PROP is the text property name for item numbers.
LOAD-MORE-TYPE is the symbol for the load-more text property."
  (when (fboundp 'shipit--find-section-by-type)
    (let ((section (shipit--find-section-by-type section-type)))
      (when section
        (let* ((content-pos (marker-position (oref section content)))
               (end-pos (marker-position (oref section end)))
               (inhibit-read-only t))
          (save-excursion
            ;; Update heading count
            (goto-char (oref section start))
            (when (re-search-forward "\\(?:Open\\|Closed\\) (\\([0-9]+\\))" (line-end-position 2) t)
              (replace-match (number-to-string (length items)) nil nil nil 1))
            ;; Clear children and content
            (oset section children nil)
            (when (slot-boundp section 'washer)
              (oset section washer nil))
            (when (and content-pos end-pos (> end-pos content-pos))
              (remove-overlays content-pos end-pos 'invisible t)
              (delete-region content-pos end-pos))
            ;; Insert new content
            (goto-char content-pos)
            (let ((magit-insert-section--parent section))
              (dolist (item items)
                (shipit-repo-buffer--insert-item-line item repo text-prop))
              (when has-more
                (shipit-repo-buffer--insert-load-more-line load-more-type)))
            ;; Restore magit-section text property so magit-current-section
            ;; can find this section from the newly inserted body text
            (put-text-property content-pos (point) 'magit-section section)
            ;; Update markers
            (oset section end (point-marker))
            (oset section content (copy-marker content-pos))))))))

(defun shipit-repo-buffer--filter-active-p ()
  "Return non-nil if any item filter is currently active."
  (or shipit-repo-buffer--filter-mode
      (not (string-empty-p shipit-repo-buffer--filter-text))))

(defun shipit-repo-buffer--load-more (type &optional count)
  "Load more items of TYPE.
TYPE is `prs', `open-prs', `issues', or `open-issues'.
COUNT, if non-nil, overrides the default batch size.
Always rebuilds the full parent section to keep sibling markers consistent."
  (let ((saved-pos (point)))
    (message "Loading more...")
    (shipit-repo-buffer--fetch-more type count)
    ;; Always rebuild the full parent section — targeted subsection refresh
    ;; corrupts sibling section markers when earlier subsections grow.
    (shipit-repo-buffer--refresh-filtered-section)
    ;; Restore cursor position — the refresh deletes and re-inserts content,
    ;; which causes save-excursion marks to collapse to the section start.
    (goto-char (min saved-pos (point-max)))
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
  "Return buffer position of an org heading matching HEADING-TEXT, or nil.
Walks the buffer for runs of `font-lock-face' matching `org-level-N'
and matches a heading whose line text equals HEADING-TEXT under
slug-normalization (so anchors like `other-llm-backends' match a
heading like `* Other LLM backends')."
  (let ((target (shipit-repo-buffer--normalize-anchor heading-text))
        found pos)
    (when (not (string-empty-p target))
      (save-excursion
        (setq pos (point-min))
        (while (and (not found) (< pos (point-max)))
          (let* ((flf (get-text-property pos 'font-lock-face))
                 (sym (cond ((symbolp flf) flf)
                            ((and (listp flf)
                                  (cl-find-if (lambda (s)
                                                (and (symbolp s)
                                                     (string-prefix-p
                                                      "org-level-"
                                                      (symbol-name s))))
                                              flf)))))
                 (is-heading (and sym (symbolp sym)
                                  (string-prefix-p "org-level-"
                                                   (symbol-name sym)))))
            (when is-heading
              (goto-char pos)
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                     (clean (replace-regexp-in-string "\\`[ \t*]+" "" line))
                     (heading-slug (shipit-repo-buffer--normalize-anchor clean)))
                (when (or (equal target heading-slug)
                          (string-prefix-p (concat target "-") heading-slug)
                          (string-suffix-p (concat "-" target) heading-slug)
                          (string-match-p (concat "\\`" (regexp-quote target) "\\'")
                                          heading-slug))
                  (setq found (line-beginning-position)))))
            (setq pos (or (next-single-property-change pos 'font-lock-face)
                          (point-max)))))))
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
      (pcase section-type
        ('repo-prs
         (shipit-repo-buffer--refresh-prs-section-filtered))
        ('repo-issues
         (shipit-repo-buffer--refresh-issues-section-filtered))
        ('repo-discussions
         (shipit-repo-buffer--refresh-discussions-section-filtered))))))

(defun shipit-repo-buffer--refresh-prs-section-filtered ()
  "Re-render the PRs section body with filtered items."
  (when (fboundp 'shipit--find-section-by-type)
    (let ((section (shipit--find-section-by-type 'repo-prs)))
      (when section
        (let* ((filtered-open (shipit-repo-buffer--filter-items
                               shipit-repo-buffer-open-prs))
               (filtered-closed (shipit-repo-buffer--filter-items
                                 shipit-repo-buffer-closed-prs))
               (content-pos (marker-position (oref section content)))
               (end-pos (marker-position (oref section end)))
               (inhibit-read-only t))
          (save-excursion
            (oset section children nil)
            (when (and content-pos end-pos (> end-pos content-pos))
              (delete-region content-pos end-pos))
            (goto-char content-pos)
            (let ((magit-insert-section--parent section)
                  (repo shipit-repo-buffer-repo)
                  (filter-active (shipit-repo-buffer--filter-active-p)))
              (when (or filtered-open
                        (and filter-active shipit-repo-buffer-open-prs))
                (magit-insert-section (repo-prs-open nil nil)
                  (magit-insert-heading
                    (propertize (format "  Open (%d)" (length filtered-open))
                                'font-lock-face 'magit-section-heading))
                  (dolist (pr filtered-open)
                    (shipit-repo-buffer--insert-item-line
                     pr repo 'shipit-repo-pr-number))
                  (when shipit-repo-buffer-open-prs-has-more
                    (shipit-repo-buffer--insert-load-more-line 'open-prs))))
              (when (or filtered-closed
                        (and filter-active shipit-repo-buffer-closed-prs))
                (magit-insert-section (repo-prs-closed nil nil)
                  (magit-insert-heading
                    (propertize (format "  Closed (%d)" (length filtered-closed))
                                'font-lock-face 'magit-section-heading))
                  (dolist (pr filtered-closed)
                    (shipit-repo-buffer--insert-item-line
                     pr repo 'shipit-repo-pr-number))
                  (when shipit-repo-buffer-closed-prs-has-more
                    (shipit-repo-buffer--insert-load-more-line 'prs))))
              (insert "\n"))
            (oset section end (point-marker))
            (oset section content (copy-marker content-pos))))))))

(defun shipit-repo-buffer--refresh-issues-section-filtered ()
  "Re-render the issues section body with filtered items."
  (when (fboundp 'shipit--find-section-by-type)
    (let ((section (shipit--find-section-by-type 'repo-issues)))
      (when section
        (let* ((filtered-open (shipit-repo-buffer--filter-items
                               shipit-repo-buffer-open-issues))
               (filtered-closed (shipit-repo-buffer--filter-items
                                 shipit-repo-buffer-closed-issues))
               (content-pos (marker-position (oref section content)))
               (end-pos (marker-position (oref section end)))
               (inhibit-read-only t))
          (save-excursion
            (oset section children nil)
            (when (and content-pos end-pos (> end-pos content-pos))
              (delete-region content-pos end-pos))
            (goto-char content-pos)
            (let ((magit-insert-section--parent section)
                  (repo shipit-repo-buffer-repo)
                  (filter-active (shipit-repo-buffer--filter-active-p)))
              (when (or filtered-open
                        (and filter-active shipit-repo-buffer-open-issues))
                (magit-insert-section (repo-issues-open nil nil)
                  (magit-insert-heading
                    (propertize (format "  Open (%d)" (length filtered-open))
                                'font-lock-face 'magit-section-heading))
                  (dolist (issue filtered-open)
                    (shipit-repo-buffer--insert-item-line
                     issue repo 'shipit-repo-issue-number))
                  (when shipit-repo-buffer-open-issues-has-more
                    (shipit-repo-buffer--insert-load-more-line 'open-issues))))
              (when (or filtered-closed
                        (and filter-active shipit-repo-buffer-closed-issues))
                (magit-insert-section (repo-issues-closed nil nil)
                  (magit-insert-heading
                    (propertize (format "  Closed (%d)" (length filtered-closed))
                                'font-lock-face 'magit-section-heading))
                  (dolist (issue filtered-closed)
                    (shipit-repo-buffer--insert-item-line
                     issue repo 'shipit-repo-issue-number))
                  (when shipit-repo-buffer-closed-issues-has-more
                    (shipit-repo-buffer--insert-load-more-line 'issues))))
              (insert "\n"))
            (oset section end (point-marker))
            (oset section content (copy-marker content-pos))))))))

(defun shipit-repo-buffer--refresh-discussions-section-filtered ()
  "Re-render the discussions section body with filtered items."
  (when (fboundp 'shipit--find-section-by-type)
    (let ((section (shipit--find-section-by-type 'repo-discussions)))
      (when section
        (let* ((filtered (shipit-repo-buffer--filter-items
                          shipit-repo-buffer-discussions))
               (content-pos (marker-position (oref section content)))
               (end-pos (marker-position (oref section end)))
               (inhibit-read-only t))
          (save-excursion
            (oset section children nil)
            (when (and content-pos end-pos (> end-pos content-pos))
              (delete-region content-pos end-pos))
            (goto-char content-pos)
            (let ((magit-insert-section--parent section)
                  (repo shipit-repo-buffer-repo)
                  (body-start (point)))
              (dolist (disc filtered)
                (shipit-repo-buffer--insert-item-line
                 disc repo 'shipit-repo-discussion-number))
              (insert "\n")
              ;; Re-apply magit-section property so magit-current-section
              ;; resolves correctly on re-inserted item lines.
              (put-text-property body-start (point) 'magit-section section))
            (oset section end (point-marker))
            (oset section content (copy-marker content-pos))))))))

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
         ;; Jira store their key as a string (e.g. "ZIVID-12624").
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
                                  (concat label "  <- current")
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
            ;; Thread subscription suffixes (conditional on thread context)
            (thread-entries
             (when shipit--subscription-transient-thread-ctx
               (let* ((ctx shipit--subscription-transient-thread-ctx)
                      (type (plist-get ctx :type))
                      (number (plist-get ctx :number))
                      (subject (plist-get ctx :subject))
                      (state shipit--subscription-transient-thread-state)
                      (subscribed (equal state "subscribed"))
                      (desc (format "This %s: #%d%s  [%s]"
                                    type
                                    number
                                    (if subject
                                        (format " %s" subject)
                                      "")
                                    (or state "unknown"))))
                 (list (transient-parse-suffix
                        transient--prefix
                        (list :info desc))
                       (transient-parse-suffix
                        transient--prefix
                        (list "t"
                              (if subscribed "Unsubscribe" "Subscribe")
                              'shipit-repo-buffer--sub-toggle-thread)))))))
       (append sub-entries star-entry thread-entries)))]
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

(provide 'shipit-repo-buffer)
;;; shipit-repo-buffer.el ends here
