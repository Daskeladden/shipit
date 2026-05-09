;;; shipit-review-mode.el --- Minor mode for interactive PR review in worktrees -*- lexical-binding: t; -*-

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
(require 'shipit-render)
(require 'vc-git)
(require 'transient nil t)

(defgroup shipit-review nil
  "Shipit worktree review mode for interactive PR exploration."
  :group 'shipit)

(defun shipit-review--resolve-base-sha (pr-data &optional worktree-path)
  "Return a usable base SHA for review mode, given PR-DATA + WORKTREE-PATH.

Tries, in order:

1. `pr-data\\='s `base.sha\\=' (the canonical value from the GitHub
   API).  This is what shipit fetches for fully-loaded PR
   data.
2. `git merge-base HEAD origin/BASE-REF\\=' inside WORKTREE-PATH,
   where BASE-REF comes from `pr-data\\='s `base.ref\\=' (e.g.
   `master\\=' / `main\\=').  Useful when `pr-data\\=' was loaded from
   a context that strips the base subobject.
3. `git merge-base HEAD origin/HEAD\\=' inside WORKTREE-PATH, as
   a last-resort default-branch fallback.

Returns the resolved SHA string, or nil when nothing produces
a usable answer.  Logs which source matched so the next
`unknown..<sha>\\=' header can be diagnosed quickly."
  (let* ((base-from-data (cdr (assq 'sha (cdr (assq 'base pr-data)))))
         (base-ref (cdr (assq 'ref (cdr (assq 'base pr-data)))))
         (resolved nil)
         (source nil))
    (cond
     ((and base-from-data (stringp base-from-data)
           (> (length base-from-data) 0))
      (setq resolved base-from-data source 'pr-data))
     ((and worktree-path (file-directory-p worktree-path))
      (let* ((default-directory (file-name-as-directory worktree-path))
             (try-merge-base
              (lambda (ref)
                (with-temp-buffer
                  (when (zerop (process-file
                                "git" nil t nil
                                "merge-base" "HEAD" ref))
                    (let ((sha (string-trim (buffer-string))))
                      (and (> (length sha) 0) sha)))))))
        (cond
         ((and base-ref (stringp base-ref) (> (length base-ref) 0)
               (setq resolved (funcall try-merge-base
                                       (concat "origin/" base-ref))))
          (setq source (format "merge-base origin/%s" base-ref)))
         ((setq resolved (funcall try-merge-base "origin/HEAD"))
          (setq source "merge-base origin/HEAD"))))))
    (shipit--debug-log
     "review base-sha resolved: %s (source=%s, pr-data-base=%s base-ref=%s worktree=%s)"
     (if resolved (substring resolved 0 (min 7 (length resolved))) "nil")
     (or source "none")
     (if base-from-data
         (substring base-from-data 0 (min 7 (length base-from-data)))
       "nil")
     (or base-ref "nil")
     (or worktree-path "nil"))
    resolved))

;; Buffer-local variables for review context
(defvar-local shipit-review--base-sha nil
  "SHA of the base branch for diff comparison in review mode.")

(defvar-local shipit-review--head-sha nil
  "SHA of the PR head for commit navigation in review mode.")

(defvar-local shipit-review--pr-data nil
  "Full PR data object for the current review session.")

(defvar-local shipit-review--comparison-target nil
  "Current comparison target SHA. If nil, defaults to base-sha.")

(defvar-local shipit-review--current-commit-index nil
  "Current commit index when navigating through PR commits (1-indexed).")

(defvar-local shipit-review--total-commits nil
  "Total number of commits in PR.")

(defvar-local shipit-review--changed-lines nil
  "List of line ranges that were changed in the diff. Format: ((added-start . added-end) (removed-start . removed-end) ...)")

(defvar-local shipit-review--mode-line-string ""
  "Current mode line string. Updated dynamically when comparison target changes.")

(defvar-local shipit-review--current-commit-index nil
  "Current commit being viewed (0-based index). nil means showing range.")

(defvar-local shipit-review--range-start-index nil
  "Start of current range (0-based index). nil means showing from base.")

(defvar-local shipit-review--range-end-index nil
  "End of current range (0-based index). nil means showing to HEAD.")

(defvar-local shipit-review--displayed-sha nil
  "SHA of the current commit being displayed in the file buffer.")


(defvar shipit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r c") #'shipit-review-compare-menu)
    (define-key map (kbd "C-c r t") #'shipit-review-file-history)
    (define-key map (kbd "C-c r r") #'shipit-review-menu)
    map)
  "Keymap for shipit-review-mode.")

(defvar shipit-review--transient-source-buffer nil
  "Buffer captured at the last `shipit-review-menu' invocation.
Used by `shipit-review--locate-source-buffer' to keep n/p, the
timeline, and the preview all routed to the same review-mode
buffer for the entire transient session -- so a stale timeline
window for a different file can't redirect navigation.")

;; Transient menu for comparison target
(when (fboundp 'transient-define-prefix)
  (transient-define-prefix shipit-review-compare-menu ()
    "Menu to adjust diff comparison target in review mode."
    [["Comparison Target"
      ("b" "Compare vs Base" shipit-review--compare-vs-base)
      ("h" "Compare vs HEAD" shipit-review--compare-vs-head)
      ("p" "Compare vs Previous" shipit-review--compare-vs-previous)]
     ["Advanced"
      ("c" "Custom SHA..." shipit-review--compare-vs-custom)
      ("r" "Reset to Base" shipit-review--reset-comparison)]]))

;; Top-level review-mode transient.  Ties together commit navigation,
;; comparison-target switching, the timeline buffer, and the diff /
;; ediff actions.
(when (fboundp 'transient-define-prefix)
  (transient-define-prefix shipit-review-menu ()
    "Shipit review actions for the current worktree file.
Commit-navigation suffixes (`n', `p', `M', `N', `=') are sticky --
they keep the menu open so you can step through commits while
watching the timeline buffer's highlighted current commit move."
    [["Navigate commits"
      ("n" "Next commit"        shipit-review--next-commit
       :transient t)
      ("p" "Previous commit"    shipit-review--prev-commit
       :transient t)
      ("M" "Extend range left"  shipit-review--move-range-start-prev
       :transient t)
      ("N" "Extend range right" shipit-review--move-range-end-next
       :transient t)
      ("=" "Reset to full range" shipit-review--reset-to-full-range
       :transient t)]
     ["Compare"
      ("c" "Compare target..." shipit-review-compare-menu)
      ("d" "Diff (in main window)" shipit-review--show-diff)
      ("e" "Ediff vs newest file commit" shipit-review--show-ediff)]
     ["Drill down"
      ("t" "File timeline" shipit-review-file-history
       :transient t)
      ("v" "View file at current SHA" shipit-review--show-preview
       :transient t)
      ("b" "Blame (live file)" shipit-review--blame-live)]
     ["Misc"
      ("q" "Quit menu" transient-quit-one)]]))

(when (fboundp 'advice-add)
  (advice-add 'shipit-review-menu :before
              (lambda (&rest _)
                (when (and (bound-and-true-p shipit-review-mode)
                           shipit-review--pr-data)
                  (setq shipit-review--transient-source-buffer
                        (current-buffer))))))

(defun shipit-review--current-diff-range ()
  "Return (FROM-SHA . TO-SHA) for the current comparison/range state.
Falls back to base..head when no specific range is selected."
  (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
         (from nil)
         (to nil))
    (cond
     (shipit-review--current-commit-index
      (let ((commit (nth shipit-review--current-commit-index commits)))
        (setq to (cdr (assq 'sha commit)))
        (if (> shipit-review--current-commit-index 0)
            (setq from (cdr (assq 'sha
                                  (nth (1- shipit-review--current-commit-index)
                                       commits))))
          (setq from shipit-review--base-sha))))
     ((or shipit-review--range-start-index shipit-review--range-end-index)
      (let ((s (or shipit-review--range-start-index 0))
            (e (or shipit-review--range-end-index (1- (length commits)))))
        (setq from (if (= s 0)
                       shipit-review--base-sha
                     (cdr (assq 'sha (nth (1- s) commits)))))
        (setq to (cdr (assq 'sha (nth e commits))))))
     (t
      (setq from (or shipit-review--comparison-target shipit-review--base-sha))
      (setq to shipit-review--head-sha)))
    (and from to (cons from to))))

(defun shipit-review--show-diff ()
  "Show a unified diff of this file for the current comparison range.
Pops up a `*shipit-diff: FILE @ FROM..TO*' buffer in another
window.  Routed through `--with-source-buffer' so the suffix
reads buffer-file-name from the review-mode buffer even if the
user pressed `d' from the timeline / preview window."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let ((range (shipit-review--current-diff-range))
           (file (buffer-file-name)))
       (cond
        ((or (null range) (null file))
         (user-error "Cannot determine diff range"))
        (t
         (let* ((from (car range))
                (to (cdr range))
                (root (shipit--get-repo-root))
                (rel (if root (file-relative-name file root) file))
                (default-directory (if root (file-name-as-directory root)
                                     default-directory))
                (buf-name (format "*shipit-diff: %s @ %s..%s*"
                                  (file-name-nondirectory file)
                                  (substring from 0 (min 7 (length from)))
                                  (substring to 0 (min 7 (length to)))))
                (buf (get-buffer-create buf-name)))
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (erase-buffer)
               (process-file "git" nil t nil "diff" "--unified=3"
                             (format "%s..%s" from to) "--" rel)
               (diff-mode)
               (setq buffer-read-only t)
               (set-buffer-modified-p nil)
               (goto-char (point-min))))
           (display-buffer buf))))))))

(defun shipit-review--show-ediff ()
  "Ediff this file at the newest file-affecting commit against the navigated SHA.
Compares the file at the SHA `n'/`p' last navigated to (the
buffer-local `shipit-review--displayed-sha') against the file
at the *newest commit that actually touched this file* -- not
the worktree HEAD.  The two have identical content when HEAD
itself didn't touch the file, but using the file-history
endpoint means the SHA labelling the right side of the ediff
session is one the user can find in the timeline buffer.

Errors when navigation is still at the file's newest commit
(nothing to compare).  Routed through `--with-source-buffer'
so it reads state from the review-mode file buffer regardless
of which window the user pressed `e' from."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (shipit-review--ensure-file-commits)
     (let* ((file (buffer-file-name))
            (sha (or shipit-review--displayed-sha shipit-review--head-sha))
            (commits shipit-review--file-commits-list)
            (newest (and commits (cdr (assq 'sha (car (last commits))))))
            (newest-effective (or newest shipit-review--head-sha)))
       (cond
        ((or (null file) (null sha) (null newest-effective))
         (user-error "Cannot determine ediff range"))
        ((string= sha newest-effective)
         (user-error
          "Already at newest file commit; navigate with n/p first"))
        (t
         (let* ((root (shipit--get-repo-root))
                (rel (if root (file-relative-name file root) file)))
           (shipit-review-history-ediff-files-at-shas
            sha newest-effective rel
            (or root default-directory)))))))))

(defun shipit-review--blame-live ()
  "Run `magit-blame-addition' on the live worktree file.
Uses the current `displayed-sha' if known, falling back to HEAD."
  (interactive)
  (let ((sha (or shipit-review--displayed-sha
                 shipit-review--head-sha)))
    (cond
     ((and sha (fboundp 'magit-blame-addition))
      (with-no-warnings (magit-blame-addition sha)))
     (t
      (user-error "No revision to blame")))))

(defcustom shipit-review-show-revision-preview nil
  "If non-nil, `n'/`p' navigation auto-opens a revision preview buffer.
Default nil because the auto-pop creates a window split that
the transient menu obscures, which is confusing.  When nil, the
preview is opened only on explicit `v' from the review menu
(after which navigation updates the existing preview window in
place).  Set to t to bring the auto-pop back."
  :type 'boolean
  :group 'shipit)

(defun shipit-review--preview-buffer-name (file-rel)
  "Return the dedicated preview buffer name for FILE-REL."
  (format "*shipit-review preview: %s*" file-rel))

(defun shipit-review--update-revision-preview (sha &optional force-display)
  "Refresh the revision preview buffer for the current file at SHA.
The preview is a single buffer per file (named via
`shipit-review--preview-buffer-name'); consecutive `n'/`p'
presses replace its content in place.  By default the buffer is
only displayed if it is *already* showing in some window --
auto-popping a new split is invasive and tends to land behind
the transient menu.

With non-nil FORCE-DISPLAY (e.g. from the explicit `v' menu
action), pop it up unconditionally.  When
`shipit-review-show-revision-preview' is non-nil, that auto-pop
behaviour is also restored.

No-op when SHA is nil, or when SHA equals the worktree HEAD
(the live file already shows that)."
  (when (and sha
             (not (and shipit-review--head-sha
                       (string= sha shipit-review--head-sha))))
    (let* ((file (buffer-file-name))
           (worktree (and file (shipit--get-repo-root)))
           (rel (and file worktree (file-relative-name file worktree)))
           (file-mode major-mode))
      (when (and rel worktree)
        (let* ((buf-name (shipit-review--preview-buffer-name rel))
               (existing-buf (get-buffer buf-name))
               (visible (and existing-buf (get-buffer-window existing-buf t)))
               (should-display (or force-display
                                   shipit-review-show-revision-preview
                                   visible))
               (should-update (or should-display existing-buf))
               (buf (and should-update (get-buffer-create buf-name)))
               (default-directory (file-name-as-directory worktree))
               (short (substring sha 0 (min 7 (length sha))))
               (commits (cdr (assq 'commits shipit-review--pr-data)))
               (subject
                (cl-loop for c in commits
                         when (and (cdr (assq 'sha c))
                                   (string= sha (cdr (assq 'sha c))))
                         return (cdr (assq 'message c)))))
          (when buf
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (shipit-review-history-insert-file-at-sha sha rel worktree)
                ;; Only run the major mode the *first* time we
                ;; populate this preview buffer; rerunning
                ;; e.g. `c++-ts-mode' on every nav step
                ;; reinitialises treesitter + every global mode
                ;; hook (pulsar, jinx, indent-bars, ...) and was
                ;; the dominant 35% / ~2.4s cost in the n/p
                ;; profile.
                (when (and file-mode (fboundp file-mode)
                           (not (eq major-mode file-mode)))
                  (funcall file-mode))
                (setq-local header-line-format
                            (format " %s @ %s%s"
                                    rel short
                                    (if subject
                                        (format " -- %s"
                                                (truncate-string-to-width
                                                 (car (split-string subject "
"))
                                                 60 nil nil "..."))
                                      "")))
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                (goto-char (point-min)))))
          (when (and should-display buf (not visible))
            (display-buffer
             buf '((display-buffer-reuse-window
                    display-buffer-pop-up-window)))))))))

(defun shipit-review--show-preview ()
  "Open the revision-preview buffer for the current displayed SHA.
After this is called once, navigation (`n'/`p') updates the same
window in place -- so this is the on-demand entry point that
matches the default `shipit-review-show-revision-preview' = nil
configuration."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let ((sha (or shipit-review--displayed-sha
                    shipit-review--head-sha)))
       (cond
        ((null sha)
         (user-error "No revision to preview yet -- step with n/p first"))
        (t
         (shipit-review--update-revision-preview sha t)))))))


;;;###autoload
(define-minor-mode shipit-review-mode
  "Minor mode for interactive PR review in worktrees.

Provides:
- Revision navigation (commit-by-commit through PR history)
- Visual change indicators via diff-hl integration
- Full Emacs integration (LSP, linting, tests, Magit)

When enabled on a file opened from a PR worktree:
- diff-hl shows changed lines
- Side-frame displays comments for current line
- Transient menu provides revision navigation
- Buffer remains editable for exploring changes

\\{shipit-review-mode-map}"
  :lighter (:eval shipit-review--mode-line-string)
  :keymap shipit-review-mode-map
  (if shipit-review-mode
      (shipit-review-mode--setup)
    (shipit-review-mode--teardown)))

(defun shipit-review--mode-line-string ()
  "Generate mode line string showing the current diff range with graphical indicator."
  (if (and shipit-review--base-sha shipit-review--head-sha)
      (let* ((target (or shipit-review--comparison-target shipit-review--base-sha))
             (from-short (if target (substring target 0 7) "unknown"))
             (to-short (if shipit-review--head-sha (substring shipit-review--head-sha 0 7) "HEAD")))
        (format " Review[%s..%s]" from-short to-short))
    " Review"))

(defun shipit-review--update-buffer-mode-line ()
  "Update the buffer's mode line to show the current file SHA."
  (let ((commits (cdr (assq 'commits shipit-review--pr-data)))
        (displayed-sha nil))
    (cond
     ;; Single-commit mode: show that commit's SHA
     (shipit-review--current-commit-index
      (let ((commit (nth shipit-review--current-commit-index commits)))
        (setq displayed-sha (cdr (assq 'sha commit)))))
     ;; Range mode: show the "to" SHA of the range
     ((or shipit-review--range-start-index shipit-review--range-end-index)
      (let ((end-idx (or shipit-review--range-end-index (1- (length commits)))))
        (let ((commit (nth end-idx commits)))
          (setq displayed-sha (cdr (assq 'sha commit))))))
     ;; Default: show head SHA
     (t (setq displayed-sha shipit-review--head-sha)))
    (setq-local shipit-review--displayed-sha displayed-sha)
    (force-mode-line-update)))

(defun shipit-review--set-comparison-target (target-sha description)
  "Change the comparison target to TARGET-SHA and update overlays.
TARGET-SHA should be a git SHA. DESCRIPTION is for logging."
  (setq-local shipit-review--comparison-target target-sha)
  (shipit--debug-log "Changed comparison target to: %s (%s)"
                     (if target-sha (substring target-sha 0 7) "base")
                     description)
  (shipit-review--update-diff-overlays)
  (shipit-review--update-buffer-mode-line)
  (shipit-review--update-mode-line)
  (when (fboundp 'shipit-review-history-rehighlight-for-source)
    (shipit-review-history-rehighlight-for-source (current-buffer)))
  (shipit-review--update-revision-preview shipit-review--displayed-sha)
  (message "Comparing: %s vs HEAD" description))

(defun shipit-review--compare-vs-base ()
  "Compare against the base branch (default)."
  (interactive)
  (shipit-review--set-comparison-target nil "base"))

(defun shipit-review--compare-vs-head ()
  "Compare against HEAD (shows no changes since you're on HEAD)."
  (interactive)
  (shipit-review--set-comparison-target shipit-review--head-sha "HEAD"))

(defun shipit-review--compare-vs-previous ()
  "Compare against the previous commit in the PR."
  (interactive)
  (if (and shipit-review--current-commit-index
           (> shipit-review--current-commit-index 1)
           shipit-review--pr-data)
      (let* ((commits (when (fboundp 'shipit--build-pr-commit-list)
                        (shipit--build-pr-commit-list shipit-review--pr-data)))
             (prev-index (- shipit-review--current-commit-index 2))
             (prev-commit (and (>= prev-index 0) (nth prev-index commits)))
             (prev-sha (and prev-commit (plist-get prev-commit :sha))))
        (if prev-sha
            (shipit-review--set-comparison-target prev-sha
                                                  (format "commit %d/%d"
                                                          (1+ prev-index)
                                                          shipit-review--total-commits))
          (message "No previous commit available")))
    (message "Not navigating commits. Use 'b' to compare against base.")))

(defun shipit-review--compare-vs-custom ()
  "Prompt user for a custom SHA to compare against."
  (interactive)
  (let ((custom-sha (read-string "Enter commit SHA to compare against: ")))
    (when (and custom-sha (not (string-empty-p custom-sha)))
      (shipit-review--set-comparison-target custom-sha
                                            (substring custom-sha 0 7)))))

(defun shipit-review--reset-comparison ()
  "Reset comparison target to base branch."
  (interactive)
  (shipit-review--compare-vs-base))

(defun shipit-review--make-jump-to-commit-fn (commit-num)
  "Create a function to jump to COMMIT-NUM (1-based)."
  (lambda ()
    (interactive)
    (when shipit-review--pr-data
      (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
             (index (1- commit-num)))
        (when (< index (length commits))
          (let ((commit (nth index commits)))
            (shipit-review--view-single-commit index commit)))))))

(defun shipit-review--view-single-commit (index commit)
  "View diff for a single COMMIT at INDEX (0-based)."
  (setq-local shipit-review--current-commit-index index)
  (setq-local shipit-review--range-start-index nil)
  (setq-local shipit-review--range-end-index nil)
  (let ((sha (cdr (assq 'sha commit))))
    (shipit-review--set-comparison-target sha (format "commit %d" (1+ index)))))

(defcustom shipit-review-pre-pr-history-limit nil
  "Max number of commits to include in `n'/`p' file-history navigation.
Defaults to nil = no cap, so navigation walks the file's full
history.  Set to an integer if `git log --follow' is too slow
for the file in question; the cap then applies to the *combined*
PR + pre-PR list."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Limit"))
  :group 'shipit)

(defvar-local shipit-review--file-commits-list nil
  "Commits affecting this buffer's file, oldest -> newest.
Each entry: ((sha . SHA) (message . MSG) (in-pr . t-or-nil)).
Includes both the PR commits that touched this file and (up to
`shipit-review-pre-pr-history-limit') pre-PR commits.  Lazily
populated by `shipit-review--ensure-file-commits'.")

(defun shipit-review--ensure-file-commits ()
  "Populate `shipit-review--file-commits-list' if not already cached.
Runs `git log --follow HEAD -- file' in the worktree, oldest
first, and tags each entry with `in-pr' membership using the
PR's `pr-data.commits' SHA set."
  (unless shipit-review--file-commits-list
    (let* ((file (buffer-file-name))
           (worktree (and file (shipit--get-repo-root)))
           (rel (and file worktree (file-relative-name file worktree)))
           (pr-shas
            (let ((h (make-hash-table :test 'equal)))
              (dolist (c (cdr (assq 'commits shipit-review--pr-data)))
                (let ((s (cdr (assq 'sha c))))
                  (when s (puthash s t h))))
              h))
           (limit (and (integerp shipit-review-pre-pr-history-limit)
                       (> shipit-review-pre-pr-history-limit 0)
                       ;; +30 cushion so PR commits aren't trimmed.
                       (+ 30 shipit-review-pre-pr-history-limit)))
           (result '()))
      (when (and rel worktree)
        (let ((default-directory (file-name-as-directory worktree)))
          (with-temp-buffer
            (let* ((args (append (list "log" "--follow"
                                       "--pretty=format:%H%x09%s")
                                 (when limit
                                   (list (format "-n%d" limit)))
                                 (list "--" rel)))
                   (rc (apply #'process-file "git" nil t nil args)))
              (shipit--debug-log
               "file-commits git log rc=%d cwd=%s args=%S output-bytes=%d"
               rc default-directory args (buffer-size))
              (when (zerop rc)
                (dolist (line (split-string (buffer-string) "\n" t))
                  (let ((parts (split-string line "\t" nil)))
                    (when (and (car parts)
                               (string-match-p "\\`[0-9a-f]\\{40\\}\\'"
                                               (car parts)))
                      (push `((sha . ,(car parts))
                              (message . ,(or (cadr parts) ""))
                              (in-pr . ,(gethash (car parts) pr-shas)))
                            result))))))))
        ;; `git log' streams newest-first; `push' inverts that as it
        ;; accumulates, so `result' is already oldest -> newest.  Use
        ;; it directly so list[0] is the oldest commit and
        ;; list[length-1] is the newest -- matches the
        ;; "increment idx == newer commit" semantics that
        ;; `--prev-commit' / `--next-commit' assume.
        (setq-local shipit-review--file-commits-list result)
        (shipit--debug-log
         "Populated file-commits list: %d total (%d in PR) for %s"
         (length shipit-review--file-commits-list)
         (cl-count-if (lambda (e) (cdr (assq 'in-pr e)))
                      shipit-review--file-commits-list)
         rel)))))

(defun shipit-review--current-file-commit-index ()
  "Return the index in `--file-commits-list' matching the displayed SHA, or nil.
nil means the displayed SHA isn't a file-affecting commit (the
common case at default state when HEAD itself didn't touch this
file).  `--prev-commit' / `--next-commit' interpret nil as
\"position past the newest entry\" so `p' lands on the last
entry instead of skipping it."
  (let* ((commits shipit-review--file-commits-list)
         (current shipit-review--displayed-sha))
    (and current
         (cl-position current commits
                      :key (lambda (e) (cdr (assq 'sha e)))
                      :test 'string=))))

(defun shipit-review--view-file-commit (entry)
  "Navigate the file buffer to ENTRY (alist with sha/message/in-pr keys).
For PR commits, delegates to `--view-single-commit' so the
comparison target tracks the PR.  For pre-PR commits, sets
`displayed-sha' directly and refreshes the preview/timeline,
keeping the comparison target anchored at base (the PR's diff
overlay would be meaningless against an older revision)."
  (let* ((sha (cdr (assq 'sha entry)))
         (msg (cdr (assq 'message entry)))
         (in-pr (cdr (assq 'in-pr entry)))
         (pr-commits (cdr (assq 'commits shipit-review--pr-data)))
         (pr-idx (and in-pr
                      (cl-position sha pr-commits
                                   :key (lambda (c) (cdr (assq 'sha c)))
                                   :test 'string=))))
    (cond
     ((and in-pr pr-idx)
      (shipit-review--view-single-commit pr-idx (nth pr-idx pr-commits)))
     (t
      (setq-local shipit-review--current-commit-index nil)
      (setq-local shipit-review--range-start-index nil)
      (setq-local shipit-review--range-end-index nil)
      (setq-local shipit-review--displayed-sha sha)
      (shipit-review--update-mode-line)
      (when (fboundp 'shipit-review-history-rehighlight-for-source)
        (shipit-review-history-rehighlight-for-source (current-buffer)))
      (shipit-review--update-revision-preview sha)
      (shipit--debug-log "Pre-PR navigation to %s -- %s"
                         (substring sha 0 7) (or msg ""))))))

(defun shipit-review--message-for-entry (idx total entry)
  "Format the echo-area message for ENTRY at IDX of TOTAL."
  (message "%d/%d %s -- %s"
           (1+ idx) total
           (if (cdr (assq 'in-pr entry)) "(in PR)" "(before PR)")
           (or (car (split-string (or (cdr (assq 'message entry)) "") "\n"))
               "")))

(defun shipit-review--locate-source-buffer ()
  "Return a review-mode buffer with PR data, regardless of current focus.
Resolution order:
1. The buffer captured by `shipit-review-menu' on its most
   recent invocation -- so a stale timeline / preview window
   for a different file can\='t redirect n/p.
2. `current-buffer' if it already qualifies.
3. Any visible review-mode buffer in the selected frame.
4. Any live review-mode buffer at all -- last-resort fallback."
  (or
   (and shipit-review--transient-source-buffer
        (buffer-live-p shipit-review--transient-source-buffer)
        (buffer-local-value 'shipit-review--pr-data
                            shipit-review--transient-source-buffer)
        shipit-review--transient-source-buffer)
   (and (bound-and-true-p shipit-review-mode)
        shipit-review--pr-data
        (current-buffer))
   (cl-loop for win in (window-list (selected-frame))
            for buf = (window-buffer win)
            when (and (buffer-local-value 'shipit-review-mode buf)
                      (buffer-local-value 'shipit-review--pr-data buf))
            return buf)
   (cl-loop for buf in (buffer-list)
            when (and (buffer-local-value 'shipit-review-mode buf)
                      (buffer-local-value 'shipit-review--pr-data buf))
            return buf)))

(defun shipit-review--with-source-buffer (action)
  "Call ACTION inside the review source buffer.
ACTION is a thunk run with `current-buffer' bound to the buffer
returned by `shipit-review--locate-source-buffer'.  Errors out if
no such buffer can be located."
  (let ((buf (shipit-review--locate-source-buffer)))
    (cond
     ((null buf)
      (user-error "Cannot find the review-mode buffer for this PR"))
     (t
      (with-current-buffer buf (funcall action))))))

(defun shipit-review--prev-commit ()
  "Go to previous (older) commit affecting this file.
Walks `--file-commits-list', which mixes the PR's file-touching
commits with up to `shipit-review-pre-pr-history-limit' pre-PR
commits -- so navigation continues into the file's history once
the PR's commits are exhausted."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (shipit-review--ensure-file-commits)
     (let* ((commits shipit-review--file-commits-list)
            (total (length commits))
            (idx (shipit-review--current-file-commit-index)))
       (cond
        ((<= total 0)
         (user-error "No commits affect this file"))
        ;; Not on any file-affecting commit (e.g. displayed SHA is
        ;; HEAD and HEAD didn't touch this file): treat as a virtual
        ;; position past the newest entry, so `p' lands on the last
        ;; entry instead of skipping it.
        ((null idx)
         (let* ((last-idx (1- total))
                (entry (nth last-idx commits)))
           (shipit-review--view-file-commit entry)
           (shipit-review--message-for-entry last-idx total entry)))
        ((<= idx 0)
         (message "Already at oldest commit affecting this file (1/%d)" total))
        (t
         (let* ((prev-idx (1- idx))
                (entry (nth prev-idx commits)))
           (shipit-review--view-file-commit entry)
           (shipit-review--message-for-entry prev-idx total entry))))))))

(defun shipit-review--next-commit ()
  "Go to next (newer) commit affecting this file.
Walks `--file-commits-list' (PR + pre-PR file history)."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (shipit-review--ensure-file-commits)
     (let* ((commits shipit-review--file-commits-list)
            (total (length commits))
            (idx (shipit-review--current-file-commit-index)))
       (cond
        ((<= total 0)
         (user-error "No commits affect this file"))
        ;; Not on any file-affecting commit yet (first nav press
        ;; from default state): anchor on the indicator\='s
        ;; position -- the newest file-affecting commit -- so the
        ;; keypress does something visible.  Subsequent `n\=' from
        ;; there will correctly hit the boundary branch.
        ((null idx)
         (let* ((last-idx (1- total))
                (entry (nth last-idx commits)))
           (shipit-review--view-file-commit entry)
           (shipit-review--message-for-entry last-idx total entry)))
        ((>= idx (1- total))
         (message "Already at newest commit affecting this file (%d/%d)"
                  total total))
        (t
         (let* ((next-idx (1+ idx))
                (entry (nth next-idx commits)))
           (shipit-review--view-file-commit entry)
           (shipit-review--message-for-entry next-idx total entry))))))))

(defun shipit-review--move-range-start-prev ()
  "Extend range by moving base to previous (older) commit."
  (interactive)
  (when shipit-review--pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
           ;; If in single-commit mode, initialize range from that commit
           (current-start (if (not (null shipit-review--current-commit-index))
                             shipit-review--current-commit-index
                           (or shipit-review--range-start-index 0)))
           (current-end (if (not (null shipit-review--current-commit-index))
                           shipit-review--current-commit-index
                         (or shipit-review--range-end-index (1- (length commits))))))
      (when (> current-start 0)
        (let ((new-start (1- current-start)))
          (setq-local shipit-review--range-start-index new-start)
          (setq-local shipit-review--range-end-index current-end)
          (setq-local shipit-review--current-commit-index nil)
          (shipit-review--update-diff-overlays)
          (shipit-review--update-buffer-mode-line)
          (shipit-review--update-mode-line)
          (when (fboundp 'shipit-review-history-rehighlight-for-source)
            (shipit-review-history-rehighlight-for-source (current-buffer)))
          (message "Range extended (base moved earlier)"))))))

(defun shipit-review--move-range-end-next ()
  "Extend range by moving head to next (newer) commit."
  (interactive)
  (when shipit-review--pr-data
    (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
           (total (length commits))
           ;; If in single-commit mode, initialize range from that commit
           (current-start (if (not (null shipit-review--current-commit-index))
                             shipit-review--current-commit-index
                           (or shipit-review--range-start-index 0)))
           (current-end (if (not (null shipit-review--current-commit-index))
                           shipit-review--current-commit-index
                         (or shipit-review--range-end-index (1- total)))))
      (when (< current-end (1- total))
        (let ((new-end (1+ current-end)))
          (setq-local shipit-review--range-start-index current-start)
          (setq-local shipit-review--range-end-index new-end)
          (setq-local shipit-review--current-commit-index nil)
          (shipit-review--update-diff-overlays)
          (shipit-review--update-buffer-mode-line)
          (shipit-review--update-mode-line)
          (when (fboundp 'shipit-review-history-rehighlight-for-source)
            (shipit-review-history-rehighlight-for-source (current-buffer)))
          (message "Range extended (head moved later)"))))))

(defun shipit-review--reset-to-full-range ()
  "Reset to full range (base..HEAD)."
  (interactive)
  (setq-local shipit-review--current-commit-index nil)
  (setq-local shipit-review--range-start-index nil)
  (setq-local shipit-review--range-end-index nil)
  (shipit-review--set-comparison-target nil "full range"))

(defun shipit-review--update-mode-line ()
  "Update the mode line string with current comparison information.
When `displayed-sha' differs from HEAD (i.e. the user has stepped
through commits), show ` Review @ SHA' so the lighter and the
timeline highlight always agree on which commit is in focus.
At default state, show the comparison range ` Review[FROM..TO]'."
  (setq-local shipit-review--mode-line-string
              (cond
               ((not (and shipit-review--base-sha shipit-review--head-sha))
                " Review")
               ((and shipit-review--displayed-sha
                     shipit-review--head-sha
                     (not (string= shipit-review--displayed-sha
                                   shipit-review--head-sha)))
                (format " Review @ %s"
                        (substring shipit-review--displayed-sha 0 7)))
               (t
                (let* ((target (or shipit-review--comparison-target
                                   shipit-review--base-sha))
                       (from-short (if target (substring target 0 7) "unknown"))
                       (to-short (if shipit-review--head-sha
                                     (substring shipit-review--head-sha 0 7)
                                   "HEAD")))
                  (format " Review[%s..%s]" from-short to-short)))))
  (force-mode-line-update))

(defun shipit-review--git-commit-list (base-sha head-sha)
  "Return BASE..HEAD commits as an alist list with `sha' and `message' keys.
Order: oldest -> newest, matching how `n'/`p' navigate forward
through PR history.  Returns nil when either SHA is missing or
`git log' fails (caller treats nil as `no commits available')."
  (when (and base-sha head-sha)
    (with-temp-buffer
      (let ((rc (process-file "git" nil t nil "log" "--reverse"
                              "--pretty=format:%H%x09%s"
                              (format "%s..%s" base-sha head-sha))))
        (when (zerop rc)
          (let ((result '()))
            (dolist (line (split-string (buffer-string) "\n" t))
              (let* ((parts (split-string line "\t" nil)))
                (when (and (car parts)
                           (string-match-p "\\`[0-9a-f]\\{40\\}\\'" (car parts)))
                  (push `((sha . ,(car parts))
                          (message . ,(or (cadr parts) "")))
                        result))))
            (nreverse result)))))))

(defun shipit-review--ensure-commits-list ()
  "Make sure `shipit-review--pr-data' has a `commits' LIST (not just a count).
The REST API stores `commits' as an integer count; the
navigation code wants the actual commit objects.  Synthesise the
list locally from `git log base..head' so navigation works
regardless of how the buffer was opened."
  (let ((existing (cdr (assq 'commits shipit-review--pr-data))))
    (when (or (not existing) (numberp existing))
      (let ((git-list (shipit-review--git-commit-list
                       shipit-review--base-sha
                       shipit-review--head-sha)))
        (when git-list
          (setq-local shipit-review--pr-data
                      (cons (cons 'commits git-list)
                            (assq-delete-all 'commits
                                             (copy-tree shipit-review--pr-data))))
          (shipit--debug-log
           "Populated review-mode commits list: %d commits between %s..%s"
           (length git-list)
           (substring (or shipit-review--base-sha "?") 0 7)
           (substring (or shipit-review--head-sha "?") 0 7)))))))

(defun shipit-review-mode--setup ()
  "Initialize shipit review mode for current buffer.
Enables diff-hl and configures it to show PR changes (base..head)."
  (shipit--debug-log "Setting up shipit-review-mode in buffer: %s (base=%s head=%s)"
                     (buffer-name)
                     (if shipit-review--base-sha (substring shipit-review--base-sha 0 7) "unknown")
                     (if shipit-review--head-sha (substring shipit-review--head-sha 0 7) "unknown"))
  (shipit-review--ensure-commits-list)
  ;; Enable diff-hl for this buffer
  (when (and (fboundp 'diff-hl-mode) (not (bound-and-true-p diff-hl-mode)))
    (diff-hl-mode 1))

  ;; Update visual diff overlays to show changed lines
  (shipit-review--update-diff-overlays)


  ;; Update mode line with initial range
  (shipit-review--update-mode-line)
  (shipit-review--update-buffer-mode-line)

  (when (bound-and-true-p diff-hl-mode)
    (shipit--debug-log "Review mode setup complete - diff-hl is active")))

(defun shipit-review--parse-diff-hunk-header (header)
  "Parse a unified diff hunk header like '@@ -10,5 +15,7 @@'.
Returns (start-old count-old start-new count-new) or nil if header is invalid."
  (when (string-match "@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?" header)
    (let ((start-old (string-to-number (match-string 1 header)))
          (count-old (string-to-number (or (match-string 2 header) "1")))
          (start-new (string-to-number (match-string 3 header)))
          (count-new (string-to-number (or (match-string 4 header) "1"))))
      (list start-old count-old start-new count-new))))

(defun shipit-review--get-diff-output (from-sha to-sha file-path)
  "Get unified diff output between FROM-SHA and TO-SHA for FILE-PATH.
Returns the diff as a string."
  (condition-case err
      (shell-command-to-string
       (format "cd %s && git diff --unified=0 %s..%s -- %s 2>/dev/null"
               (shell-quote-argument (file-name-directory (or buffer-file-name default-directory)))
               (shell-quote-argument from-sha)
               (shell-quote-argument to-sha)
               (shell-quote-argument (file-name-nondirectory file-path))))
    (error
     (shipit--debug-log "Error getting diff: %s" (error-message-string err))
     "")))

(defun shipit-review--parse-diff-for-changed-lines (diff-output)
  "Parse unified diff output and extract line ranges that were changed.
Returns list of (line-number . type) where type is 'added, 'removed, or 'modified.
NOTE: We track the NEW file line numbers (what's in the worktree)."
  (let ((lines (split-string diff-output "\n"))
        (current-line-new nil)
        changed-lines)
    (shipit--debug-log "Parsing diff: %d lines" (length lines))
    (dolist (line lines)
      (cond
       ;; Hunk header: @@ -10,5 +15,7 @@
       ((string-match "^@@ -\\([0-9]+\\)" line)
        (let ((parsed (shipit-review--parse-diff-hunk-header line)))
          (when parsed
            (let ((start-new (caddr parsed)))
              (setq current-line-new start-new)))))
       ;; Line removed (only affects old file, skip)
       ((and current-line-new (string-match "^-" line))
        nil)
       ;; Line added (in the new file)
       ((and current-line-new (string-match "^\\+" line))
        (push (cons current-line-new 'added) changed-lines)
        (setq current-line-new (1+ current-line-new)))
       ;; Context line (unchanged, in both files)
       ((and current-line-new (or (string-match "^ " line)
                                  (string-match "^\\\\" line)))
        (setq current-line-new (1+ current-line-new)))
       ;; Anything else (diff headers, etc)
       (t
        nil)))
    (reverse changed-lines)))

(defun shipit-review--update-diff-overlays ()
  "Update visual overlays to mark changed lines based on current diff."
  (interactive)
  (when shipit-review--base-sha
    (condition-case err
        (let* ((commits (cdr (assq 'commits shipit-review--pr-data)))
               (from-sha (if shipit-review--current-commit-index
                           ;; For single commit view: compare against parent
                           (if (> shipit-review--current-commit-index 0)
                               (let ((parent-commit (nth (1- shipit-review--current-commit-index) commits)))
                                 (cdr (assq 'sha parent-commit)))
                             shipit-review--base-sha)
                         ;; For range view: use range start or base
                         (if shipit-review--range-start-index
                             (if (= shipit-review--range-start-index 0)
                                 ;; If starting at commit 0, compare from base
                                 shipit-review--base-sha
                               ;; Otherwise, compare from the parent commit
                               (let ((parent-commit (nth (1- shipit-review--range-start-index) commits)))
                                 (cdr (assq 'sha parent-commit))))
                           shipit-review--base-sha)))
               (to-sha (if shipit-review--current-commit-index
                          ;; For single commit view: use current commit
                          (let ((current-commit (nth shipit-review--current-commit-index commits)))
                            (cdr (assq 'sha current-commit)))
                        ;; For range view: use range end or HEAD
                        (if shipit-review--range-end-index
                            (let ((end-commit (nth shipit-review--range-end-index commits)))
                              (cdr (assq 'sha end-commit)))
                          shipit-review--head-sha)))
               (diff-output (shipit-review--get-diff-output from-sha to-sha (buffer-file-name)))
               (changed-lines (shipit-review--parse-diff-for-changed-lines diff-output)))
          ;; Store changed lines for filtering comments by range
          (setq-local shipit-review--changed-lines changed-lines)
          ;; Remove existing overlays
          (remove-overlays (point-min) (point-max) 'shipit-review-change t)
          ;; Add new overlays for each changed line
          (dolist (line-info changed-lines)
            (let ((line-num (car line-info))
                  (change-type (cdr line-info)))
              (save-excursion
                (goto-line line-num)
                (when (>= line-num 1)
                  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                    (overlay-put ov 'shipit-review-change t)
                    (overlay-put ov 'face (pcase change-type
                                            ('added 'diff-added)
                                            ('removed 'diff-removed)
                                            ('modified 'diff-changed)
                                            (_ 'default)))
                    (overlay-put ov 'help-echo (format "%s in PR diff" change-type))))))))
      (error
       (shipit--debug-log "Error updating diff overlays: %s" (error-message-string err))))))

(defun shipit-review-mode--teardown ()
  "Cleanup shipit review mode when disabled."
  (shipit--debug-log "Tearing down shipit-review-mode from buffer: %s" (buffer-name))
  ;; Remove all review-related overlays
  (remove-overlays (point-min) (point-max) 'shipit-review-change t))

(provide 'shipit-review-mode)
;;; shipit-review-mode.el ends here
