;;; shipit-code-refs.el --- Detect issue-key references in code comments -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Highlights issue-key references such as `PRJ-12345' that appear
;; inside code comments and makes them actionable.  RET / mouse-1 on a
;; match opens the corresponding shipit issue buffer for the current
;; repo's issue backend (typically Jira).  The `shipit-code-refs-menu'
;; transient exposes additional actions: browse in the backend's web
;; UI, copy the URL, or copy the key itself.

;; When embark is loaded, a target finder makes the key available as
;; an embark target of category `shipit-issue-ref', so users can
;; trigger the same actions via their embark key (default `C-.').

;; Enable per buffer with `shipit-code-refs-mode' or globally (for
;; `prog-mode' derivatives) with `global-shipit-code-refs-mode'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'shipit-core)

(declare-function shipit-issue--resolve-for-repo "shipit-issue-backends")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit--browse-issue-url "shipit-notifications")
(declare-function shipit-issue-create-buffer "shipit-issue-create")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function consult--read "consult")
(declare-function consult--dynamic-collection "consult")

;; Forward-declare the dynamic variable from pulsar so local `let' bindings
;; aren't flagged as lexically unused.  We rebind it to nil around our
;; programmatic `delete-region' calls to avoid pulsar's
;; `pulsar-pulse-region-functions' advisors pulsing a nil region.
(defvar pulsar-pulse-region-functions)

(defgroup shipit-code-refs nil
  "Detect and act on issue-key references in code comments."
  :group 'shipit
  :prefix "shipit-code-refs-")

(defcustom shipit-code-refs-pattern
  "\\b\\([A-Z][A-Z0-9_]+-[0-9]+\\)\\b"
  "Regex matching issue-key references in code.
Group 1 must capture the full key (e.g. `PRJ-12345').  The default
matches Jira-style keys: one uppercase letter followed by more
uppercase alphanumerics or underscores, a hyphen, and one or more
digits."
  :type 'regexp
  :group 'shipit-code-refs)

(defcustom shipit-code-refs-comments-only t
  "When non-nil, only recognise keys inside syntactic comments.
Disabling this also picks up keys in strings and live code — handy in
buffers (e.g. plain text) where `syntax-ppss' does not classify
comments, but introduces noise in most code."
  :type 'boolean
  :group 'shipit-code-refs)

(defcustom shipit-code-refs-global-modes
  '(prog-mode text-mode conf-mode yaml-ts-mode yaml-mode)
  "Major modes where `global-shipit-code-refs-mode' auto-enables.
Matched with `derived-mode-p', so listing a parent mode also enables
the mode in all descendants.  Tree-sitter modes that do not derive
from `prog-mode' (e.g. `yaml-ts-mode' in some Emacs versions) can be
listed explicitly."
  :type '(repeat symbol)
  :group 'shipit-code-refs)

(defcustom shipit-code-refs-restrict-to-project-keys t
  "When non-nil, only highlight keys whose prefix is a configured project key.
Project keys are read from the `:project-keys' field of the backend
config resolved via `shipit-issue--resolve-for-repo'.  When the
current repo has no matching backend, or the backend exposes no
project keys, nothing is highlighted.  Set to nil to match any
`shipit-code-refs-pattern' occurrence regardless of project."
  :type 'boolean
  :group 'shipit-code-refs)

(defvar-local shipit-code-refs--project-keys 'unset
  "Cached list of project-key prefixes for this buffer.
Sentinel `unset' means resolution has not run; nil means resolution
ran and produced no keys (no backend, or a backend without
`:project-keys').")

(defun shipit-code-refs--project-keys-for-buffer ()
  "Return cached list of project-key prefixes for this buffer.
Resolves once via `shipit-issue--resolve-for-repo' and caches the
result."
  (when (eq shipit-code-refs--project-keys 'unset)
    (setq shipit-code-refs--project-keys
          (let ((repo (shipit-code-refs--current-repo)))
            (when repo
              (require 'shipit-issue-backends nil t)
              (when-let* ((resolved (ignore-errors
                                      (shipit-issue--resolve-for-repo repo)))
                          (config (cdr resolved)))
                (plist-get config :project-keys))))))
  shipit-code-refs--project-keys)

(defun shipit-code-refs--key-allowed-p (key)
  "Return non-nil if KEY's prefix passes the project-key restriction."
  (or (not shipit-code-refs-restrict-to-project-keys)
      (let ((keys (shipit-code-refs--project-keys-for-buffer)))
        (and keys
             (member (car (split-string key "-")) keys)))))

(defun shipit-code-refs--in-comment-p (pos)
  "Return non-nil if POS is inside a comment.
Prefers `treesit-node-at' when a tree-sitter parser is active in the
buffer (fast and incrementally updated); otherwise falls back to
`syntax-ppss'.  Using tree-sitter avoids `syntax-ppss' scans from
`point-min' in tree-sitter modes whose syntax table does not classify
comments, which can make live typing unresponsive."
  (if (and (fboundp 'treesit-parser-list)
           (treesit-parser-list))
      (when-let* ((node (treesit-node-at pos))
                  (type (treesit-node-type node)))
        (string-match-p "comment" type))
    (nth 4 (syntax-ppss pos))))

(defface shipit-code-refs-face
  '((t :inherit link :underline t))
  "Face applied to recognised issue-key references in code."
  :group 'shipit-code-refs)

(defvar shipit-code-refs-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-code-refs-dwim)
    (define-key map [mouse-1] #'shipit-code-refs-dwim)
    (define-key map (kbd "M-;") #'shipit-code-refs-menu)
    map)
  "Keymap applied to issue-ref regions in code buffers.")

;;; Detection

(defun shipit-code-refs--matcher (bound)
  "Font-lock matcher that finds the next issue-key ref up to BOUND.
Honors `shipit-code-refs-comments-only' and
`shipit-code-refs-restrict-to-project-keys'.  Wraps predicate checks
in `save-match-data' so that group 1 remains valid for font-lock to
read after the matcher returns."
  (let ((case-fold-search nil)
        (found nil))
    (while (and (not found)
                (re-search-forward shipit-code-refs-pattern bound t))
      (save-match-data
        (let ((key (match-string-no-properties 1))
              (mb (match-beginning 0)))
          (when (and (shipit-code-refs--key-allowed-p key)
                     (or (not shipit-code-refs-comments-only)
                         (shipit-code-refs--in-comment-p mb)))
            (setq found t)))))
    found))

(defvar shipit-code-refs--font-lock-keywords nil
  "Font-lock keywords added by `shipit-code-refs-mode'.")

(defun shipit-code-refs--build-keywords ()
  "Return font-lock-keywords that tag issue-key matches with properties."
  `((shipit-code-refs--matcher
     (1 (list 'face 'shipit-code-refs-face
              'shipit-issue-ref (match-string-no-properties 1)
              'mouse-face 'highlight
              'keymap shipit-code-refs-keymap
              'help-echo "RET: open issue; M-;: actions")
        prepend))))

(defun shipit-code-refs-key-at-point ()
  "Return the issue key at point, or nil.
Prefers the font-lock-applied text property; falls back to scanning
the current line with `shipit-code-refs-pattern'."
  (or (get-text-property (point) 'shipit-issue-ref)
      (save-excursion
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position))
              (case-fold-search nil)
              (pt (point))
              (found nil))
          (goto-char line-start)
          (while (and (not found)
                      (re-search-forward shipit-code-refs-pattern line-end t))
            (when (and (<= (match-beginning 0) pt)
                       (>= (match-end 0) pt))
              (setq found (match-string-no-properties 1))))
          found))))

;;; Actions

(defun shipit-code-refs--current-repo ()
  "Return the repo (owner/repo) associated with the current buffer, or nil."
  (ignore-errors (shipit--get-repo-from-remote)))

(defun shipit-code-refs--browse-url (key)
  "Return the browse URL for issue KEY under the current repo's backend."
  (let* ((repo (shipit-code-refs--current-repo))
         (resolved (and repo
                        (ignore-errors (shipit-issue--resolve-for-repo repo))))
         (backend (car resolved))
         (config (cdr resolved))
         (browse-fn (and backend (plist-get backend :browse-url))))
    (when browse-fn
      (funcall browse-fn config key))))

;;;###autoload
(defun shipit-code-refs-open-issue (&optional key)
  "Open the shipit issue buffer for KEY (defaults to key at point)."
  (interactive)
  (let ((key (or key (shipit-code-refs-key-at-point)))
        (repo (shipit-code-refs--current-repo)))
    (cond
     ((not key) (user-error "No issue key at point"))
     ((not repo) (user-error "Cannot determine repository from current buffer"))
     (t
      (require 'shipit-issues-buffer)
      (shipit-issues-open-buffer key repo)))))

;;;###autoload
(defun shipit-code-refs-browse-issue (&optional key)
  "Browse issue KEY (defaults to key at point) in the backend's web UI."
  (interactive)
  (let* ((key (or key (shipit-code-refs-key-at-point)))
         (url (and key (shipit-code-refs--browse-url key))))
    (cond
     ((not key) (user-error "No issue key at point"))
     ((not url) (user-error "Cannot resolve browse URL for %s" key))
     (t (browse-url url)))))

;;;###autoload
(defun shipit-code-refs-copy-url (&optional key)
  "Copy the backend browse URL for KEY (defaults to key at point) to the kill ring."
  (interactive)
  (let* ((key (or key (shipit-code-refs-key-at-point)))
         (url (and key (shipit-code-refs--browse-url key))))
    (cond
     ((not key) (user-error "No issue key at point"))
     ((not url) (user-error "Cannot resolve URL for %s" key))
     (t (kill-new url) (message "Copied: %s" url)))))

;;;###autoload
(defun shipit-code-refs-copy-key (&optional key)
  "Copy the issue KEY (defaults to key at point) to the kill ring."
  (interactive)
  (let ((key (or key (shipit-code-refs-key-at-point))))
    (if (not key)
        (user-error "No issue key at point")
      (kill-new key)
      (message "Copied: %s" key))))

(defcustom shipit-code-refs-insert-format 'key-title
  "Format used when inserting an issue reference from the picker/CAPF.

Supported values:
- `key'       : just the bare key (e.g. `PRJ-1234').
- `key-title' : key + colon + title (e.g. `PRJ-1234: Rethink ...').
- A format string with placeholders:
    %k  — the issue key
    %t  — the issue title (trimmed)
    %u  — the backend URL
    %%  — a literal % sign
  Examples:
    \"%k: %t\"           → PRJ-1234: Rethink ...
    \"TODO(%k): %t\"     → TODO(PRJ-1234): Rethink ...
    \"[%k](%u) %t\"      → markdown-link style
- A function taking the propertized key string; its return value
  is inserted verbatim."
  :type '(choice (const :tag "Key only" key)
                 (const :tag "Key: Title" key-title)
                 (string :tag "Format string")
                 (function :tag "Custom formatter"))
  :group 'shipit-code-refs)

(defvar shipit-code-refs--title-by-key (make-hash-table :test 'equal)
  "Side cache of ISSUE-KEY → TITLE populated when building candidates.
Used as a fallback when the completion framework (e.g. consult)
strips text properties from the returned selection string.")

(defun shipit-code-refs--remember-title (key title)
  "Record KEY→TITLE in `shipit-code-refs--title-by-key' for later recovery."
  (when (and (stringp key) (stringp title))
    (puthash (substring-no-properties key) title
             shipit-code-refs--title-by-key)))

(defun shipit-code-refs--title-for-key (key-str)
  "Return the issue title associated with KEY-STR, or nil.
First checks the `shipit-issue-title' text property on KEY-STR;
falls back to `shipit-code-refs--title-by-key' keyed by the bare key."
  (when (stringp key-str)
    (or (get-text-property 0 'shipit-issue-title key-str)
        (gethash (substring-no-properties key-str)
                 shipit-code-refs--title-by-key))))

(defun shipit-code-refs--expand-format (fmt key-str)
  "Expand format string FMT against KEY-STR.
Replaces %k (key), %t (trimmed title), %u (URL), %% (literal %)."
  (let* ((key (substring-no-properties key-str))
         (raw-title (shipit-code-refs--title-for-key key-str))
         (title (or (and raw-title (string-trim raw-title)) ""))
         (url (or (ignore-errors (shipit-code-refs--browse-url key)) "")))
    (replace-regexp-in-string
     "%[ktu%]"
     (lambda (s)
       (pcase (aref s 1)
         (?k key)
         (?t title)
         (?u url)
         (?% "%")
         (_ s)))
     fmt t t)))

(defun shipit-code-refs--format-insertion (key-str)
  "Return the text to insert for KEY-STR per `shipit-code-refs-insert-format'."
  (when (stringp key-str)
    (let ((fmt shipit-code-refs-insert-format))
      (cond
       ((functionp fmt) (funcall fmt key-str))
       ((eq fmt 'key) (substring-no-properties key-str))
       ((eq fmt 'key-title)
        (shipit-code-refs--expand-format "%k: %t" key-str))
       ((stringp fmt) (shipit-code-refs--expand-format fmt key-str))
       (t (substring-no-properties key-str))))))

(defun shipit-code-refs--insertion-suffix (key-str)
  "Return the text that should appear AFTER the key per the format.
Empty when the format contains only the key (or when the key can't be
located in the expanded format)."
  (let* ((full (shipit-code-refs--format-insertion key-str))
         (key (and key-str (substring-no-properties key-str)))
         (idx (and full key (string-search key full))))
    (when idx
      (substring full (+ idx (length key))))))

(defun shipit-code-refs--trim-autopair-close (suffix)
  "Skip over an auto-inserted closing delim when SUFFIX starts with it.
If the character at point is a closing delim (`)', `]', `}', `\"',
`\\='') and SUFFIX begins with the same character, move past the char
and drop it from SUFFIX.  Returns the (possibly trimmed) suffix.

This avoids double-inserting the closing delim when the user typed the
opening delim and electric-pair-mode auto-closed it: the suffix's
leading close is the one already in the buffer, so we skip and don't
re-insert."
  (if (and suffix
           (not (string-empty-p suffix))
           (memq (char-after) '(?\) ?\] ?\} ?\" ?\'))
           (eq (char-after) (aref suffix 0)))
      (progn (forward-char 1)
             (substring suffix 1))
    suffix))

;;; Completion at point

(defcustom shipit-code-refs-enable-completion t
  "When non-nil, `shipit-code-refs-mode' registers a completion-at-point
function that completes issue keys for the repo's configured project
keys.  Candidates are fetched from the issue backend on demand and
cached per project."
  :type 'boolean
  :group 'shipit-code-refs)

(defcustom shipit-code-refs-capf-cache-ttl 300
  "Seconds to cache completion candidates per project.
After the TTL expires the next completion attempt refetches."
  :type 'number
  :group 'shipit-code-refs)

(defcustom shipit-code-refs-capf-state "open"
  "State filter for completion candidates.
Passed to the backend's `:search' as `--state=VALUE'.  Common values:
\"open\" (default) or \"all\"."
  :type 'string
  :group 'shipit-code-refs)

(defcustom shipit-code-refs-capf-limit 100
  "Maximum number of candidates fetched per project."
  :type 'integer
  :group 'shipit-code-refs)

(defvar shipit-code-refs--capf-cache (make-hash-table :test 'equal)
  "Cache of completion candidates keyed by `REPO|PROJECT'.
Each value is a cons (TIMESTAMP . CANDIDATES); candidates are
propertized strings carrying the issue title as `shipit-issue-title'.")

(defun shipit-code-refs--capf-candidate-string (issue)
  "Build a propertized candidate string for normalized ISSUE alist.
Reads the issue key from the `id' (or `number') field of the shipit
normalized issue alist — backends produce Jira-style keys here.
Also records the key→title mapping for later recovery."
  (let ((key (or (cdr (assq 'id issue))
                 (cdr (assq 'number issue))))
        (title (or (cdr (assq 'title issue)) ""))
        (state (cdr (assq 'state issue))))
    (when (and key (stringp key))
      (shipit-code-refs--remember-title key title)
      (propertize key
                  'shipit-issue-title title
                  'shipit-issue-state state))))

(defun shipit-code-refs--fetch-with-args (project repo args)
  "Fetch candidates for PROJECT in REPO via backend `:search' using ARGS.
ARGS is a list of transient-style strings like `--state=open',
`--title=foo', `--limit=50'."
  (require 'shipit-issue-backends nil t)
  (when-let* ((resolved (ignore-errors
                          (shipit-issue--resolve-for-repo repo)))
              (backend (car resolved))
              (config (cdr resolved))
              (search-fn (plist-get backend :search)))
    (let* ((scoped-config (plist-put (copy-sequence config)
                                     :project-keys (list project)))
           (issues (condition-case err
                       (funcall search-fn scoped-config args)
                     (error
                      (message "shipit-code-refs: search failed: %s"
                               (error-message-string err))
                      nil))))
      (delq nil (mapcar #'shipit-code-refs--capf-candidate-string issues)))))

(defun shipit-code-refs--capf-fetch (project repo)
  "Fetch initial (recent open) completion candidates for PROJECT in REPO."
  (shipit-code-refs--fetch-with-args
   project repo
   (list (format "--state=%s" shipit-code-refs-capf-state)
         (format "--limit=%d" shipit-code-refs-capf-limit))))

(defun shipit-code-refs--search-candidates (project repo query)
  "Return candidates for PROJECT in REPO matching QUERY.
An empty QUERY reuses the CAPF cache (recent open issues).  Non-empty
QUERY issues a fresh backend search filtered by title."
  (if (string-empty-p query)
      (shipit-code-refs--capf-cached-candidates project repo)
    (shipit-code-refs--fetch-with-args
     project repo
     (list (format "--state=%s" shipit-code-refs-capf-state)
           (format "--limit=%d" shipit-code-refs-capf-limit)
           (format "--title=%s" query)))))

(defun shipit-code-refs--capf-cached-candidates (project repo)
  "Return cached candidates for PROJECT in REPO, refetching on TTL expiry."
  (let* ((cache-key (format "%s|%s" repo project))
         (entry (gethash cache-key shipit-code-refs--capf-cache))
         (ts (car entry))
         (cached (cdr entry)))
    (if (and ts (< (- (float-time) ts)
                   shipit-code-refs-capf-cache-ttl))
        cached
      (let ((fresh (shipit-code-refs--capf-fetch project repo)))
        (puthash cache-key (cons (float-time) fresh)
                 shipit-code-refs--capf-cache)
        fresh))))

(defun shipit-code-refs--capf-annotation (cand)
  "Annotation function returning the issue title for candidate CAND."
  (when-let* ((title (get-text-property 0 'shipit-issue-title cand)))
    (unless (string-empty-p title)
      (concat "  " title))))

(defun shipit-code-refs--capf-bounds-and-project ()
  "Return (START END PROJECT) when point follows `PROJECT-[digits]', else nil.
Only triggers when the match is inside a comment (if
`shipit-code-refs-comments-only' is non-nil) and PROJECT is in the
buffer's configured project keys."
  (let ((keys (shipit-code-refs--project-keys-for-buffer)))
    (when keys
      (let ((case-fold-search nil))
        (save-excursion
          (when (looking-back
                 (concat "\\(" (regexp-opt keys) "\\)-\\([0-9]*\\)")
                 (line-beginning-position) t)
            (let ((start (match-beginning 0))
                  (end (point))
                  (project (match-string-no-properties 1)))
              (when (or (not shipit-code-refs-comments-only)
                        (shipit-code-refs--in-comment-p start))
                (list start end project)))))))))

(defun shipit-code-refs-capf ()
  "Completion-at-point for Jira-style issue keys.
Triggers when point follows a configured project prefix plus a hyphen,
e.g. `PRJ-'.  Candidates come from the repo's issue backend search
and are cached per project for `shipit-code-refs-capf-cache-ttl'
seconds."
  (when (and (bound-and-true-p shipit-code-refs-mode)
             shipit-code-refs-enable-completion)
    (when-let* ((bnp (shipit-code-refs--capf-bounds-and-project))
                (start (nth 0 bnp))
                (end (nth 1 bnp))
                (project (nth 2 bnp))
                (repo (shipit-code-refs--current-repo)))
      (list start end
            (completion-table-dynamic
             (lambda (_prefix)
               (shipit-code-refs--capf-cached-candidates project repo)))
            :annotation-function #'shipit-code-refs--capf-annotation
            :exit-function #'shipit-code-refs--capf-exit
            :exclusive 'no))))

(defun shipit-code-refs--capf-exit (key-str status)
  "Append the format suffix after KEY-STR when completion STATUS is finished.
If electric-pair inserted a closing delim matching the suffix's first
char, move past it and drop the duplicate from the suffix so formats
like `TODO(%k): %t' merge cleanly with `TODO(|)'."
  (when (eq status 'finished)
    (let ((suffix (shipit-code-refs--insertion-suffix key-str)))
      (setq suffix (shipit-code-refs--trim-autopair-close suffix))
      (when (and suffix (not (string-empty-p suffix)))
        (insert suffix)))))

;;;###autoload
(defun shipit-code-refs-refresh-completions ()
  "Clear the completion candidate cache for this buffer's repo.
Forces a fresh backend fetch on the next completion attempt."
  (interactive)
  (let ((repo (shipit-code-refs--current-repo)))
    (if (not repo)
        (user-error "Cannot determine repository from current buffer")
      (maphash (lambda (key _val)
                 (when (string-prefix-p (format "%s|" repo) key)
                   (remhash key shipit-code-refs--capf-cache)))
               shipit-code-refs--capf-cache)
      (message "shipit-code-refs: completion cache cleared for %s" repo))))

(defun shipit-code-refs--enriched-candidates (candidates)
  "Reformat CANDIDATES as `KEY  TITLE [STATE]' with key as a text property.
Keeps the bare key accessible as `shipit-issue-key' so the caller can
recover it regardless of how the completion framework returns the
selection."
  (mapcar
   (lambda (c)
     (let ((title (or (get-text-property 0 'shipit-issue-title c) ""))
           (state (or (get-text-property 0 'shipit-issue-state c) "")))
       (propertize
        (format "%-14s  %s%s"
                c
                title
                (if (string-empty-p state)
                    ""
                  (format " [%s]"
                          (propertize state
                                      'face 'font-lock-type-face))))
        'shipit-issue-key c)))
   candidates))

(defun shipit-code-refs--choose-project (keys)
  "Return a project key, prompting when KEYS has more than one entry."
  (cond
   ((null keys) nil)
   ((null (cdr keys)) (car keys))
   (t (completing-read "Project: " keys nil t))))

(defun shipit-code-refs--recover-key (choice)
  "Return the issue key from completion CHOICE string."
  (or (get-text-property 0 'shipit-issue-key choice)
      (and (string-match "\\`\\([A-Z][A-Z0-9_]+-[0-9]+\\)" choice)
           (match-string 1 choice))))

(defcustom shipit-code-refs-auto-picker t
  "When non-nil, typing `PROJECT-' in a comment auto-opens the issue picker.
The typed `PROJECT-' prefix is replaced with the selected issue key.
Requires `shipit-code-refs-mode' to be active in the buffer.  Cancel
the picker with \\[keyboard-quit] to keep your typed prefix in place."
  :type 'boolean
  :group 'shipit-code-refs)

(defun shipit-code-refs--consult-available-p ()
  "Return non-nil when consult's dynamic-collection API is available."
  (and (require 'consult nil t)
       (fboundp 'consult--read)
       (fboundp 'consult--dynamic-collection)))

(defun shipit-code-refs--create-label (project)
  "Return the pseudo-candidate label that opens the create buffer."
  (format "+ Create new issue in %s" project))

(defun shipit-code-refs--pick-with-consult (project repo)
  "Pick an issue for PROJECT in REPO via consult.
Returns a propertized key string, the symbol `create' when the user
selected the `+ Create new' entry, or nil on cancel."
  (let* ((create-label (shipit-code-refs--create-label project))
         (fetch-fn
          (lambda (input)
            (cons create-label
                  (shipit-code-refs--enriched-candidates
                   (if (string-empty-p input)
                       (shipit-code-refs--capf-cached-candidates project repo)
                     (shipit-code-refs--search-candidates project repo input))))))
         (choice (consult--read
                  (consult--dynamic-collection fetch-fn
                    :min-input 0
                    :debounce 0.2
                    :throttle 0.4)
                  :prompt (format "Issue (%s): " project)
                  :category 'shipit-issue
                  :require-match t
                  :sort nil)))
    (cond
     ((null choice) nil)
     ((string= choice create-label) 'create)
     (t (shipit-code-refs--recover-key choice)))))

(defun shipit-code-refs--pick-with-two-step (project repo)
  "Pick an issue for PROJECT in REPO via dynamic completing-read.
Re-queries the backend as the user types (cached per query string so
each keystroke doesn't hit the API).  Works under vertico / ido /
fido / default completion without a consult dependency.  Returns a
key string, the symbol `create' when the user picks the create
entry, or nil on cancel."
  (let* ((create-label (shipit-code-refs--create-label project))
         (all-candidates
          (cons create-label
                (shipit-code-refs--enriched-candidates
                 (shipit-code-refs--search-candidates project repo ""))))
         (search-cache (make-hash-table :test 'equal))
         (completion-fn
          (lambda (string predicate action)
            (cond
             ((eq action 'metadata)
              '(metadata (category . shipit-issue)
                         (display-sort-function . identity)
                         (cycle-sort-function . identity)))
             (t
              (when (and (>= (length string) 2)
                         (not (gethash string search-cache)))
                (puthash string t search-cache)
                (let ((fresh (shipit-code-refs--search-candidates
                              project repo string)))
                  (when fresh
                    (dolist (c (shipit-code-refs--enriched-candidates
                                fresh))
                      (unless (member c all-candidates)
                        (setq all-candidates
                              (append all-candidates (list c))))))))
              (complete-with-action action all-candidates
                                    string predicate)))))
         (choice (completing-read (format "Issue (%s): " project)
                                  completion-fn nil t)))
    (cond
     ((null choice) nil)
     ((string= choice create-label) 'create)
     (t (shipit-code-refs--recover-key choice)))))

(defun shipit-code-refs--pick-issue-key (project repo)
  "Prompt for an issue in PROJECT/REPO, returning the selected key or nil."
  (if (shipit-code-refs--consult-available-p)
      (shipit-code-refs--pick-with-consult project repo)
    (shipit-code-refs--pick-with-two-step project repo)))

(defun shipit-code-refs--insert-at-point (key-str keys)
  "Insert the reference for KEY-STR, adapting to existing buffer context.
If point already sits after `PROJECT-' where PROJECT is in KEYS, the
prefix is replaced by the bare key and only the format suffix is
appended (and auto-pair closes are handled).  Otherwise the full
formatted string is inserted at point.

Rebinds `pulsar-pulse-region-functions' to nil to prevent pulsar from
pulsing a nil region when `delete-region' runs programmatically here."
  (let* ((key (substring-no-properties key-str))
         (case-fold-search nil)
         (pulsar-pulse-region-functions nil))
    (if (looking-back (concat "\\(" (regexp-opt keys) "\\)-")
                      (line-beginning-position) t)
        (let ((suffix (shipit-code-refs--insertion-suffix key-str)))
          (delete-region (match-beginning 0) (match-end 0))
          (insert key)
          (setq suffix (shipit-code-refs--trim-autopair-close suffix))
          (when (and suffix (not (string-empty-p suffix)))
            (insert suffix)))
      (let ((text (shipit-code-refs--format-insertion key-str)))
        (when text (insert text))))))

(defun shipit-code-refs--open-create-buffer (repo)
  "Open the shipit issue creation buffer for REPO."
  (require 'shipit-issue-create nil t)
  (if (fboundp 'shipit-issue-create-buffer)
      (shipit-issue-create-buffer repo)
    (user-error "Issue creation buffer not available (shipit-issue-create)")))

;;;###autoload
(defun shipit-code-refs-insert-issue-key (&optional project)
  "Search issues and insert the selected reference at point.
Uses consult's dynamic collection for live server-side search as you
type when consult is available; otherwise falls back to a two-step
prompt (title substring → pick from results).  The picker also offers
a `+ Create new issue' entry that opens `shipit-issue-create-buffer'.

If point already sits right after a typed `PROJECT-' prefix, only the
bare key and format suffix are inserted (preserving any surrounding
context such as `TODO(' already typed).  Otherwise the full formatted
reference is inserted.  Format is controlled by
`shipit-code-refs-insert-format'.

PROJECT defaults to the sole configured project, prompting when
multiple are configured."
  (interactive)
  (let* ((repo (or (shipit-code-refs--current-repo)
                   (user-error
                    "Cannot determine repository from current buffer")))
         (keys (or (shipit-code-refs--project-keys-for-buffer)
                   (user-error "No project keys configured for %s" repo)))
         (project (or project (shipit-code-refs--choose-project keys)))
         (result (shipit-code-refs--pick-issue-key project repo)))
    (cond
     ((eq result 'create) (shipit-code-refs--open-create-buffer repo))
     ((stringp result) (shipit-code-refs--insert-at-point result keys)))))

(defun shipit-code-refs--maybe-auto-picker ()
  "Auto-open the issue picker when `PROJECT-' was just typed in a comment.
Intended for `post-self-insert-hook'.  Only deletes the typed prefix
once a key is actually selected — cancelling the picker leaves your
typing in place."
  (when (and shipit-code-refs-auto-picker
             (eq last-command-event ?-)
             (not executing-kbd-macro))
    (let ((keys (shipit-code-refs--project-keys-for-buffer)))
      (when keys
        (let ((case-fold-search nil))
          (when (looking-back
                 (concat "\\(" (regexp-opt keys) "\\)-")
                 (line-beginning-position) t)
            (let ((start (match-beginning 0))
                  (end (point))
                  (project (match-string-no-properties 1))
                  (repo (shipit-code-refs--current-repo)))
              (when (and repo
                         (or (not shipit-code-refs-comments-only)
                             (shipit-code-refs--in-comment-p start)))
                (let ((key-str (condition-case nil
                                   (shipit-code-refs--pick-issue-key
                                    project repo)
                                 ((quit error) nil))))
                  (cond
                   ((eq key-str 'create)
                    (shipit-code-refs--open-create-buffer repo))
                   ((stringp key-str)
                    (let ((key (substring-no-properties key-str))
                          (suffix (shipit-code-refs--insertion-suffix
                                   key-str))
                          (pulsar-pulse-region-functions nil))
                      (delete-region start end)
                      (insert key)
                      (setq suffix (shipit-code-refs--trim-autopair-close
                                    suffix))
                      (when (and suffix (not (string-empty-p suffix)))
                        (insert suffix))))))))))))))

;;;###autoload (autoload 'shipit-code-refs-menu "shipit-code-refs" nil t)
(transient-define-prefix shipit-code-refs-menu ()
  "Actions for an issue-key reference at point."
  [["Issue"
    ("o" "Open in shipit" shipit-code-refs-open-issue)
    ("b" "Browse in web UI" shipit-code-refs-browse-issue)]
   ["Clipboard"
    ("u" "Copy URL" shipit-code-refs-copy-url)
    ("k" "Copy key" shipit-code-refs-copy-key)]])

;;;###autoload
(defun shipit-code-refs-dwim (&optional arg)
  "Default action on an issue-key reference at point.
With no prefix ARG, open the issue in shipit.  With any prefix ARG
\(e.g. \\[universal-argument] RET), show the `shipit-code-refs-menu'
transient instead."
  (interactive "P")
  (if arg
      (shipit-code-refs-menu)
    (shipit-code-refs-open-issue)))

;;; Minor modes

;;;###autoload
(define-minor-mode shipit-code-refs-mode
  "Highlight and act on issue-key references in this buffer.

When enabled, font-lock tags occurrences of `shipit-code-refs-pattern'
(restricted to comments by default, and to the repo's configured
project keys when `shipit-code-refs-restrict-to-project-keys' is
non-nil) with `shipit-code-refs-face' and an RET / mouse-1 keymap
that opens the matching issue in shipit.

When `shipit-code-refs-enable-completion' is non-nil, also adds
`shipit-code-refs-capf' to `completion-at-point-functions' so typing
`PROJECT-' pops up issue candidates from the backend."
  :lighter " CodeRefs"
  :group 'shipit-code-refs
  (setq shipit-code-refs--font-lock-keywords
        (or shipit-code-refs--font-lock-keywords
            (shipit-code-refs--build-keywords)))
  (when shipit-code-refs-mode
    (setq shipit-code-refs--project-keys 'unset))
  (if shipit-code-refs-mode
      (progn
        (font-lock-add-keywords nil shipit-code-refs--font-lock-keywords 'append)
        (when shipit-code-refs-enable-completion
          (add-hook 'completion-at-point-functions
                    #'shipit-code-refs-capf nil t))
        (add-hook 'post-self-insert-hook
                  #'shipit-code-refs--maybe-auto-picker nil t))
    (font-lock-remove-keywords nil shipit-code-refs--font-lock-keywords)
    (remove-hook 'completion-at-point-functions
                 #'shipit-code-refs-capf t)
    (remove-hook 'post-self-insert-hook
                 #'shipit-code-refs--maybe-auto-picker t))
  (when (fboundp 'font-lock-flush)
    (font-lock-flush)))

;;;###autoload
(define-globalized-minor-mode global-shipit-code-refs-mode
  shipit-code-refs-mode
  (lambda ()
    "Turn on `shipit-code-refs-mode' in `shipit-code-refs-global-modes'."
    (when (apply #'derived-mode-p shipit-code-refs-global-modes)
      (shipit-code-refs-mode 1)))
  :group 'shipit-code-refs)

;;; Embark integration

(declare-function embark-define-keymap "embark")
(defvar embark-target-finders)
(defvar embark-keymap-alist)

(defun shipit-code-refs-embark-target ()
  "Embark target finder: return the issue-key at point.
Category is `shipit-issue-ref', value is the bare key string."
  (when-let* ((key (or (get-text-property (point) 'shipit-issue-ref)
                       (shipit-code-refs-key-at-point))))
    `(shipit-issue-ref ,key . ,(bounds-of-thing-at-point 'symbol))))

(defvar shipit-code-refs-embark-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'shipit-code-refs-open-issue)
    (define-key map (kbd "b") #'shipit-code-refs-browse-issue)
    (define-key map (kbd "u") #'shipit-code-refs-copy-url)
    (define-key map (kbd "w") #'shipit-code-refs-copy-key)
    map)
  "Embark keymap for `shipit-issue-ref' targets.")

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders #'shipit-code-refs-embark-target)
  (add-to-list 'embark-keymap-alist
               '(shipit-issue-ref . shipit-code-refs-embark-keymap)))

;;;###autoload
(defun shipit-code-refs-diagnose ()
  "Report diagnostic information for the current buffer.

Tells whether `shipit-code-refs-mode' is active, how many pattern
matches the buffer contains, how many pass the project-key
restriction, and how many pass the comment check."
  (interactive)
  (let ((mode-on (bound-and-true-p shipit-code-refs-mode))
        (total 0)
        (allowed 0)
        (in-comment 0)
        (first-match nil)
        (max-total 2000)
        (start-time (current-time))
        (project-keys (shipit-code-refs--project-keys-for-buffer))
        (case-fold-search nil))
    ;; Cap the scan so a large buffer can never hang this command.
    (save-excursion
      (goto-char (point-min))
      (while (and (< total max-total)
                  (re-search-forward shipit-code-refs-pattern nil t))
        (cl-incf total)
        (let ((key (match-string-no-properties 1))
              (mb (match-beginning 0)))
          (unless first-match
            (setq first-match key))
          (when (or (not shipit-code-refs-restrict-to-project-keys)
                    (and project-keys
                         (member (car (split-string key "-")) project-keys)))
            (cl-incf allowed))
          (when (shipit-code-refs--in-comment-p mb)
            (cl-incf in-comment)))))
    (let ((elapsed (float-time (time-since start-time))))
      (message
       "shipit-code-refs: mode=%s, major-mode=%s, matches=%s%d, allowed=%d, in-comment=%d, first=%s, project-keys=%s, comments-only=%s, scan=%.3fs"
       (if mode-on "on" "OFF — run `shipit-code-refs-mode'")
       major-mode
       (if (>= total max-total) ">" "")
       total allowed in-comment (or first-match "nil")
       (or project-keys "nil")
       shipit-code-refs-comments-only elapsed))))

(provide 'shipit-code-refs)
;;; shipit-code-refs.el ends here
