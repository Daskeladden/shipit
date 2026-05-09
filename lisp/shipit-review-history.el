;;; shipit-review-history.el --- Per-file commit timeline (prototype A) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;; Prototype A — a dedicated *shipit-file-history* buffer listing
;; every commit affecting the file currently visited from a PR
;; worktree.  Commits in the PR are grouped at the top, the rest of
;; the file's history is shown underneath.  Built on `magit-section'
;; so navigation / DWIM dispatch follow the rest of shipit's UI
;; conventions.
;;
;; Entry point: `shipit-review-file-history' (bound in
;; `shipit-review-mode-map' to `C-c r h').

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'magit-section)
(require 'shipit-core)
(require 'shipit-faces)

(declare-function magit-blame-addition "magit-blame" (revision))

(defgroup shipit-review-history nil
  "Per-file commit timeline buffer."
  :group 'shipit)

(defcustom shipit-review-history-pre-pr-limit nil
  "Max number of pre-PR commits to fetch per file.
Defaults to nil = no limit so the timeline matches the unlimited
`n'/`p' navigation in the review menu -- otherwise nav can step
to a SHA the timeline doesn't know about and the row-highlight
stops moving.  Set to a positive integer if `git log --follow'
is too slow on a particular file."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Limit"))
  :group 'shipit-review-history)

(defvar-local shipit-review-history--commits nil
  "Cached parsed commit list for the timeline buffer.")

(defvar-local shipit-review-history--pr-shas nil
  "Cached set of PR SHAs (list) used to group `In PR' / `Before PR'.")

(defvar-local shipit-review-history--current-sha nil
  "SHA the source file buffer is currently displaying, if known.")

(defvar-local shipit-review-history--worktree nil
  "Worktree path the timeline was fetched from.")

(defvar-local shipit-review-history--file nil
  "Repo-relative file path the timeline displays.")

(defvar-local shipit-review-history--source-buffer nil
  "Original review-mode buffer that opened this timeline.")

(defvar-local shipit-review-history--mark nil
  "First side of a range comparison; SHA string or nil.
Set by `shipit-review-history-mark-commit'; consumed by
`shipit-review-history-compare-with-mark' as the diff partner.")

;;; Backend ----------------------------------------------------------

(defun shipit-review-history--git-log-args (file-path)
  "Return argument list for the `git log --follow' invocation.
FILE-PATH is the repo-relative path."
  (let ((args (list "log" "--follow"
                    "--pretty=format:COMMIT%H%x09%an%x09%ae%x09%aI%x09%s"
                    "--numstat" "--no-renames")))
    (when (and shipit-review-history-pre-pr-limit
               (integerp shipit-review-history-pre-pr-limit)
               (> shipit-review-history-pre-pr-limit 0))
      ;; +30 over the limit so PR commits aren't accidentally trimmed.
      (setq args (append args
                         (list (format "-n%d"
                                       (+ 30 shipit-review-history-pre-pr-limit))))))
    (append args (list "--" file-path))))

(defun shipit-review-history--parse (output)
  "Parse `git log --follow ... --numstat' OUTPUT into a list of plists.
Each plist: `:sha :short :author :email :date :date-full :subject
:added :removed'.  `--numstat' lines for binary files render as
`-' / `-'; we keep those as nil added/removed."
  (let ((commits '())
        (current nil))
    (dolist (raw (split-string output "\n"))
      (cond
       ((string-prefix-p "COMMIT" raw)
        (when current (push current commits))
        (let* ((rest (substring raw 6))
               (parts (split-string rest "\t"))
               (sha-full (nth 0 parts))
               (date-full (nth 3 parts)))
          (setq current (list :sha sha-full
                              :short (and sha-full
                                          (substring sha-full 0
                                                     (min 7 (length sha-full))))
                              :author (nth 1 parts)
                              :email (nth 2 parts)
                              :date (and date-full
                                         (substring date-full 0
                                                    (min 10 (length date-full))))
                              :date-full date-full
                              :subject (nth 4 parts)
                              :added 0
                              :removed 0))))
       ((and current
             (string-match "\\`\\([0-9-]+\\)\t\\([0-9-]+\\)\t" raw))
        (let ((added (match-string 1 raw))
              (removed (match-string 2 raw)))
          (when (string-match-p "\\`[0-9]+\\'" added)
            (plist-put current :added (string-to-number added)))
          (when (string-match-p "\\`[0-9]+\\'" removed)
            (plist-put current :removed (string-to-number removed)))))))
    (when current (push current commits))
    (nreverse commits)))

(defun shipit-review-history--fetch (worktree-path file-path callback)
  "Run `git log --follow' for FILE-PATH inside WORKTREE-PATH.
On exit, call CALLBACK with the parsed commit list."
  (let* ((default-directory (file-name-as-directory worktree-path))
         (out-buf (generate-new-buffer " *shipit-history-output*")))
    (make-process
     :name "shipit-history"
     :buffer out-buf
     :command (cons "git" (shipit-review-history--git-log-args file-path))
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((commits
                (when (buffer-live-p out-buf)
                  (with-current-buffer out-buf
                    (shipit-review-history--parse (buffer-string))))))
           (when (buffer-live-p out-buf) (kill-buffer out-buf))
           (funcall callback (or commits '()))))))))

;;; Renderer ---------------------------------------------------------

(defvar shipit-review-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'shipit-review-history-view-commit)
    (define-key map (kbd "=") #'shipit-review-history-ediff-vs-head)
    (define-key map (kbd "m") #'shipit-review-history-mark-commit)
    (define-key map (kbd "c") #'shipit-review-history-compare-with-mark)
    (define-key map (kbd "b") #'shipit-review-history-blame)
    (define-key map (kbd "g") #'shipit-review-history-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `shipit-review-history-mode'.")

(defun shipit-review-history--initial-visibility (section)
  "Force timeline sections collapsed on every render.
Magit caches per-section visibility, so a previously-expanded
section would re-open after every render even though we pass
HIDE=t to `magit-insert-section'.  Returning `hide' from
`magit-section-set-visibility-hook' overrides the cache.

Applies to both the per-commit container (`history-commit')
and the inner commit-message description (`history-commit-message')
so the timeline always opens with everything collapsed."
  (when (and (slot-boundp section 'type)
             (memq (oref section type)
                   '(history-commit history-commit-message)))
    'hide))

(define-derived-mode shipit-review-history-mode magit-section-mode "ShipitFileHistory"
  "Major mode for the per-file commit timeline buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;; Honour the project-wide opt-out of magit's left-fringe
  ;; visibility markers (`>' / `v').  Same hook every other shipit
  ;; section-mode buffer uses.
  (when (fboundp 'shipit--apply-section-defaults)
    (shipit--apply-section-defaults))
  (add-hook 'magit-section-set-visibility-hook
            #'shipit-review-history--initial-visibility nil 'local))

(defun shipit-review-history--commit-at-point ()
  "Return the commit plist at point, or nil."
  (let ((sec (and (fboundp 'magit-current-section) (magit-current-section))))
    (when sec
      (let ((v (oref sec value)))
        (when (and (listp v) (plist-get v :sha))
          v)))))

(defun shipit-review-history--insert-diff-line (line)
  "Insert one diff LINE with an appropriate face based on its prefix."
  (let ((face (cond
               ((string-prefix-p "+++" line) 'diff-file-header)
               ((string-prefix-p "---" line) 'diff-file-header)
               ((string-prefix-p "@@"  line) 'diff-hunk-header)
               ((string-prefix-p "diff " line) 'diff-file-header)
               ((string-prefix-p "index " line) 'diff-header)
               ((string-prefix-p "+" line) 'diff-added)
               ((string-prefix-p "-" line) 'diff-removed)
               (t nil))))
    (if face
        (insert (propertize line 'font-lock-face face) "\n")
      (insert line "\n"))))

(defun shipit-review-history--insert-commit-body (sha)
  "Insert the expanded commit body for SHA into the current section.
Reads the timeline buffer's `--worktree' / `--file' buffer-locals
to know where to run `git' and which path to diff (for renamed
files, looks up the path the file had at SHA via the cached
`--historical-path' chain).

The body has two parts:
  1. The full commit message (subject + body), shadowed.
  2. The diff for this file at this commit, with diff-mode-style
     faces on `+' / `-' / `@@' lines.

Called from `magit-insert-section-body', so it only runs on
first expansion of the surrounding section."
  (let* ((wd shipit-review-history--worktree)
         (rel shipit-review-history--file)
         (default-directory (file-name-as-directory
                             (or wd default-directory)))
         (path-at-sha (or (and rel wd
                               (shipit-review-history--historical-path
                                sha rel wd))
                          rel))
         (msg (and sha
                   (with-temp-buffer
                     (when (zerop (process-file
                                   "git" nil '(t nil) nil
                                   "log" "-1" "--format=%B" sha))
                       (string-trim (buffer-string))))))
         (diff (and sha path-at-sha
                    (with-temp-buffer
                      (when (zerop (process-file
                                    "git" nil '(t nil) nil
                                    "show" "-p" "--format="
                                    sha "--" path-at-sha))
                        (buffer-string))))))
    ;; Description: own collapsible magit-section so the heading
    ;; takes one line and the full message is one TAB away.  Body
    ;; is wrapped in `magit-insert-section-body' so the actual
    ;; message text only materialises when the user expands this
    ;; child section.
    (magit-insert-section (history-commit-message msg t)
      (magit-insert-heading
        (propertize "    Description"
                    'font-lock-face 'shipit-review-history-meta-face))
      (magit-insert-section-body
        (cond
         ((and msg (not (string-empty-p msg)))
          (dolist (line (split-string msg "\n"))
            (insert (propertize (concat "      " line "\n")
                                'font-lock-face
                                'shipit-review-history-meta-face))))
         (t
          (insert (propertize "      (no commit message)\n"
                              'font-lock-face
                              'shipit-review-history-meta-face))))))
    ;; Diff: always rendered when the parent is expanded.
    ;; Line-by-line face application so the body looks like
    ;; diff-mode without putting the whole buffer into diff-mode.
    (cond
     ((and diff (not (string-empty-p diff)))
      (dolist (line (split-string diff "\n"))
        (shipit-review-history--insert-diff-line line)))
     (t
      (insert (propertize
               (format "    (no diff for %s at %s)\n"
                       (or rel "this file")
                       (substring sha 0 (min 7 (length sha))))
               'font-lock-face 'shipit-review-history-meta-face))))))

(defun shipit-review-history--insert-commit (commit in-pr-p current-sha)
  "Insert one COMMIT as a collapsible magit-section.
IN-PR-P controls the PR-marker glyph.  When the commit's SHA
matches CURRENT-SHA the heading row is rendered with
`shipit-review-history-current-face' and prefixed with `\u25b6'.

Each commit row is its own `magit-insert-section', so TAB
toggles between the one-line heading and an expanded body that
shows author + email + full ISO timestamp + full SHA.  The
section starts collapsed."
  (let* ((sha (plist-get commit :sha))
         (short (plist-get commit :short))
         (author (plist-get commit :author))
         (email (plist-get commit :email))
         (date (plist-get commit :date))
         (date-full (plist-get commit :date-full))
         (subject (plist-get commit :subject))
         (added (plist-get commit :added))
         (removed (plist-get commit :removed))
         (current-p (and current-sha sha
                         (or (string= current-sha sha)
                             (string-prefix-p sha current-sha)
                             (string-prefix-p current-sha sha))))
         (cursor (if current-p
                     (propertize "\u25b6 " 'font-lock-face
                                 'shipit-review-history-current-face)
                   "  "))
         (dot (if in-pr-p
                  (propertize "\u25cf " 'font-lock-face
                              'shipit-review-history-pr-marker-face)
                (propertize "\u25cb " 'font-lock-face 'shadow)))
         (sha-str (propertize (or short "       ")
                              'font-lock-face 'magit-hash))
         (date-str (propertize (or date "          ")
                               'font-lock-face 'shipit-timestamp-face))
         (author-str (propertize
                      (truncate-string-to-width (or author "?") 14 nil ?\s)
                      'font-lock-face 'shipit-username-face))
         (stats (cond
                 ((and (integerp added) (integerp removed)
                       (or (> added 0) (> removed 0)))
                  (propertize (format "+%d -%d" added removed)
                              'font-lock-face 'shipit-review-history-stats-face))
                 (t (propertize "(binary)"
                                'font-lock-face
                                'shipit-review-history-stats-face))))
         (subject-str (or subject "")))
    (magit-insert-section (history-commit commit t)
      (let ((heading-start (point)))
        (magit-insert-heading
          (format "%s%s%s  %s  %s  %s  %s"
                  cursor dot sha-str date-str author-str
                  (truncate-string-to-width stats 10 nil ?\s)
                  subject-str))
        (when current-p
          (add-face-text-property
           heading-start (point)
           'shipit-review-history-current-face))
        (add-text-properties
         heading-start (point)
         `(shipit-history-sha ,sha shipit-history-in-pr ,in-pr-p)))
      ;; Body is wrapped in `magit-insert-section-body' so it is
      ;; only inserted when the user expands the section.  When
      ;; the section is created hidden (HIDE=t + the visibility
      ;; hook), the body lambda is stashed as the section's
      ;; `washer' and runs lazily on first expansion -- no
      ;; invisibility overlay needed, no body text in the buffer
      ;; until the user asks for it.  The two `git' invocations
      ;; therefore only fire when the user actually expands a
      ;; row, and only once per (section, render).
      (magit-insert-section-body
        (shipit-review-history--insert-commit-body sha)))))

(defun shipit-review-history--render
    (buffer worktree-path file-path commits pr-shas source-buffer
            &optional current-sha)
  "Populate BUFFER with the parsed COMMITS for FILE-PATH.
PR-SHAS is the set of full SHAs that belong to the PR; commits
whose SHA matches go in the `In PR' group, all others in
`Before PR' (truncated by `shipit-review-history-pre-pr-limit').
CURRENT-SHA marks the live position indicator.  When CURRENT-SHA
isn't in COMMITS (e.g. before the user navigates -- HEAD itself
may not touch this file), the indicator falls back to the
newest commit in the list, typically the most recent
file-affecting in-PR commit, so the timeline always shows
*some* current position."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Only call the major mode the *first* time we render this
      ;; timeline; rerunning it re-fires every global mode hook
      ;; (pulsar / jinx / indent-bars / emojify / ...) and was
      ;; the second-largest cost in the n/p navigation profile
      ;; (~7% / ~500ms per step).
      (unless (derived-mode-p 'shipit-review-history-mode)
        (shipit-review-history-mode))
      (setq-local default-directory (file-name-as-directory worktree-path))
      (setq-local shipit-review-history--worktree worktree-path)
      (setq-local shipit-review-history--file file-path)
      (setq-local shipit-review-history--source-buffer source-buffer)
      (setq-local shipit-review-history--commits commits)
      (setq-local shipit-review-history--pr-shas pr-shas)
      (setq-local shipit-review-history--current-sha current-sha)
      (let* ((pr-shas-set (let ((h (make-hash-table :test 'equal)))
                            (dolist (s pr-shas) (puthash s t h))
                            h))
             (in-pr (cl-remove-if-not
                     (lambda (c) (gethash (plist-get c :sha) pr-shas-set))
                     commits))
             (rest (cl-remove-if
                    (lambda (c) (gethash (plist-get c :sha) pr-shas-set))
                    commits))
             (rest (if (and shipit-review-history-pre-pr-limit
                            (integerp shipit-review-history-pre-pr-limit)
                            (> shipit-review-history-pre-pr-limit 0))
                       (seq-take rest shipit-review-history-pre-pr-limit)
                     rest))
             (current-matches
              (and current-sha
                   (cl-some (lambda (c)
                              (string= (plist-get c :sha) current-sha))
                            commits)))
             (effective-current
              (cond
               (current-matches current-sha)
               (commits (plist-get (car commits) :sha))
               (t nil))))
        ;; Display oldest -> newest, top -> bottom, so `n' (= newer)
        ;; reads as moving down and `p' (= older) as moving up.
        ;; "Before PR" group sits above "In PR" because the pre-PR
        ;; history precedes the PR's commits chronologically.
        ;; Pin the file-path / worktree info to the window's
        ;; `header-line-format' instead of into the buffer body so
        ;; it stays visible while scrolling commit rows.
        (setq-local header-line-format
                    (concat
                     (propertize " File history: " 'face 'bold)
                     (or file-path "")
                     "    "
                     (propertize "Worktree: " 'face 'shadow)
                     (propertize
                      (file-name-nondirectory
                       (directory-file-name (or worktree-path "")))
                      'face 'shadow)))
        (let ((in-pr-asc (reverse in-pr))
              (rest-asc (reverse rest)))
          (magit-insert-section (history-root)
            (magit-insert-heading "")
            (cond
             (rest-asc
              (magit-insert-section (history-before)
                (magit-insert-heading
                  (propertize
                   (format "Before PR (%d %s)\n"
                           (length rest-asc)
                           (if (= 1 (length rest-asc)) "commit" "commits"))
                   'font-lock-face 'bold))
                (dolist (c rest-asc)
                  (shipit-review-history--insert-commit c nil effective-current))
                (insert "\n")))
             ((not in-pr-asc)
              (insert (propertize
                       "(no commits found for this file)\n"
                       'font-lock-face 'shadow))))
            (when in-pr-asc
              (magit-insert-section (history-pr)
                (magit-insert-heading
                  (propertize
                   (format "In PR (%d %s)\n"
                           (length in-pr-asc)
                           (if (= 1 (length in-pr-asc)) "commit" "commits"))
                   'font-lock-face 'shipit-review-history-pr-marker-face))
                (dolist (c in-pr-asc)
                  (shipit-review-history--insert-commit c t effective-current))))))
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log
           "TIMELINE-RENDER-PRE: current-sha=%s current-matches=%s effective-current=%s commits=%d in-pr=%d rest=%d"
           (if current-sha (substring current-sha 0 (min 7 (length current-sha))) "nil")
           current-matches
           (if effective-current (substring effective-current 0 (min 7 (length effective-current))) "nil")
           (length commits) (length in-pr) (length rest)))
        (cond
         (effective-current
          (goto-char (point-min))
          ;; `text-property-any' compares with `eq', which fails on
          ;; equal-but-distinct string instances (the SHA in
          ;; `effective-current' came from `displayed-sha' / pr-data
          ;; -- a different string object than the one stored as a
          ;; text-property at insert time).  Scan with `string='
          ;; instead.
          (let ((target
                 (let ((pos (point-min))
                       (found nil))
                   (while (and pos (not found))
                     (let ((sha-here (get-text-property
                                      pos 'shipit-history-sha)))
                       (cond
                        ((and sha-here
                              (stringp sha-here)
                              (string= sha-here effective-current))
                         (setq found pos))
                        (t
                         (setq pos (next-single-property-change
                                    pos 'shipit-history-sha))))))
                   found)))
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log
               "TIMELINE-RENDER-LOOKUP: effective-current=%s target=%s buffer-size=%d"
               (substring effective-current 0 (min 7 (length effective-current)))
               (or target "nil") (buffer-size buffer)))
            (when target
              (goto-char target)
              (beginning-of-line)
              ;; The render runs from the source-file buffer's
              ;; context (rehighlight after `n'/`p').  Sync each
              ;; visible window's point and defer the recenter to
              ;; the next idle tick: transient runs its own
              ;; redisplay after the suffix returns and that resets
              ;; in-line scroll changes, so we wait for it to
              ;; finish before scrolling the timeline window.
              (let ((target-point (point)))
                (dolist (win (get-buffer-window-list buffer nil 'visible))
                  (set-window-point win target-point)
                  (let ((win win)
                        (pt target-point))
                    (run-at-time
                     0 nil
                     (lambda ()
                       (when (and (window-live-p win)
                                  (eq (window-buffer win) buffer))
                         (with-selected-window win
                           (goto-char pt)
                           (recenter)))))))))))
         (t (goto-char (point-min))))))))

;;; Actions ----------------------------------------------------------

(defvar shipit-review-history--path-chain-cache (make-hash-table :test 'equal)
  "Hash mapping (WORKDIR . REL-PATH) -> SHA -> historical path.
Built lazily on the first historical-path lookup for a given
(WORKDIR, REL-PATH) pair: a single `git log --follow' walk
populates the per-SHA hash; subsequent lookups are O(1) instead
of paying the ~30ms `git log' / `with-temp-buffer' cost per
nav step.")

(defun shipit-review-history--build-path-chain (rel-path workdir)
  "Build a SHA -> historical-path hash for REL-PATH in WORKDIR.
Runs one `git log --follow --name-only' walk and parses each
`COMMIT <sha>' + path block.  The returned hash is keyed on the
full lowercased SHA; values are the path string the file had at
that commit.  Also stores the oldest path under the special key
`oldest' so callers can fall back when looking up a SHA that
isn't on the follow chain (typical for SHAs that touched only
the pre-rename name)."
  (let ((cache (make-hash-table :test 'equal))
        (default-directory (or (and workdir (file-name-as-directory workdir))
                               default-directory)))
    (with-temp-buffer
      (let* ((rc (process-file
                  "git" nil '(t nil) nil
                  "log" "--follow" "--name-only" "--format=COMMIT %H"
                  rel-path))
             (lines (split-string (buffer-string) "\n" t))
             (current-sha nil)
             (current-path nil)
             (oldest nil))
        (when (zerop rc)
          (dolist (line lines)
            (cond
             ((string-prefix-p "COMMIT " line)
              ;; Stash the previous block before opening a new one.
              (when (and current-sha current-path)
                (puthash current-sha current-path cache)
                (setq oldest current-path))
              (setq current-sha (downcase (substring line 7))
                    current-path nil))
             (t
              ;; First path in the block wins; later lines (numstat
              ;; etc.) are ignored because we use --name-only.
              (setq current-path (or current-path line)))))
          ;; Final block.
          (when (and current-sha current-path)
            (puthash current-sha current-path cache)
            (setq oldest current-path))
          (when oldest (puthash 'oldest oldest cache)))
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log
           "HIST-PATH-CHAIN: rc=%d rel=%s entries=%d oldest=%s"
           rc rel-path (hash-table-count cache) (or oldest "nil")))))
    cache))

(defun shipit-review-history--historical-path (sha rel-path &optional workdir)
  "Return the path REL-PATH had at SHA, following renames.
Returns the historical path string, or nil when none can be
discovered (or when the file simply didn't exist yet at SHA).
WORKDIR overrides `default-directory' for the `git log'
invocation -- pass the worktree root explicitly when the
caller's buffer-local `default-directory' might be the file's
parent (which would make `git log -- REL-PATH' fail to resolve).

Caches the entire (WORKDIR, REL-PATH) follow chain in
`shipit-review-history--path-chain-cache' on first lookup, so
subsequent calls for any SHA on the same file are O(1) hash
lookups -- avoids paying the ~30ms `git log' walk per nav step
when the user pages through commits that touched the pre-rename
name."
  (let* ((wd (or (and workdir (file-name-as-directory workdir))
                 default-directory))
         (cache-key (cons wd rel-path))
         (chain (or (gethash cache-key
                             shipit-review-history--path-chain-cache)
                    (let ((built (shipit-review-history--build-path-chain
                                  rel-path wd)))
                      (puthash cache-key built
                               shipit-review-history--path-chain-cache)
                      built)))
         (target (downcase sha))
         (hit (gethash target chain))
         (best (or hit (gethash 'oldest chain))))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log
       "HIST-PATH: sha=%s rel=%s hit=%s best=%s chain-size=%d"
       (substring sha 0 (min 7 (length sha)))
       rel-path
       (if hit "yes" "no")
       (or best "nil")
       (hash-table-count chain)))
    best))

(defun shipit-review-history-insert-file-at-sha (sha rel-path &optional workdir)
  "Insert content of REL-PATH at SHA into current buffer.
Returns t on success.  WORKDIR (a directory path) is the
worktree root from which `git' should resolve REL-PATH; pass it
explicitly when calling from a buffer whose `default-directory'
is the file's parent rather than the worktree root.

When `git show' can\='t find REL-PATH at SHA (file renamed
since, or didn\='t exist yet), retries with the historical path
via `--follow' so the file content lands in the buffer
unannotated -- callers can surface the rename in a header-line
if they want.  If even the historical path doesn\='t resolve,
inserts a friendly `(file ... not present at SHA)' placeholder
and returns nil instead of leaking git\='s `fatal: ...' to the
buffer."
  (let ((default-directory (or (and workdir (file-name-as-directory workdir))
                               default-directory)))
    (let ((rc (process-file "git" nil '(t nil) nil
                            "show" (format "%s:%s" sha rel-path))))
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log
         "INSERT-AT-SHA: rc=%d cwd=%s sha=%s rel=%s"
         rc default-directory
         (substring sha 0 (min 7 (length sha))) rel-path))
      (cond
       ((zerop rc) t)
       (t
        (let ((histpath (shipit-review-history--historical-path
                         sha rel-path default-directory)))
        (cond
         ((and histpath (not (string= histpath rel-path)))
          (let ((inhibit-read-only t))
            (erase-buffer))
          (let ((rc2 (process-file "git" nil '(t nil) nil
                                   "show" (format "%s:%s" sha histpath))))
            (cond
             ;; Success via the historical name: leave the buffer
             ;; pure file content so ediff / diff-mode consumers
             ;; aren't fed an extra "(historical path: ...)" line.
             ;; Callers that want to surface the rename can call
             ;; `shipit-review-history--historical-path' themselves
             ;; and put it in a header-line / mode-line.
             ((zerop rc2) t)
             (t
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (propertize
                         (format "(file %s not present at %s)\n"
                                 rel-path
                                 (substring sha 0 (min 7 (length sha))))
                         'font-lock-face 'shadow)))
              nil))))
         (t
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize
                     (format "(file %s not present at %s)\n"
                             rel-path
                             (substring sha 0 (min 7 (length sha))))
                     'font-lock-face 'shadow)))
          nil))))))))

(defun shipit-review-history-ediff-files-at-shas (sha-a sha-b rel-path workdir)
  "Ediff REL-PATH at SHA-A vs SHA-B in WORKDIR with sane teardown.
Builds two temp buffers (`*REL @ SHORT*'), populates them via
`shipit-review-history-insert-file-at-sha' (so renames are
followed), then runs `ediff-buffers'.

On ediff quit, `ediff-after-quit-hook-internal' kills the two
temp buffers and restores the window configuration captured
just before this call -- so the workflow round-trips back to
exactly the layout the user had before pressing `e' / `=' /
`c', without leaving stale `*FILE @ SHA*' buffers behind."
  (let* ((default-directory (file-name-as-directory workdir))
         (filename (file-name-nondirectory rel-path))
         (mode (assoc-default filename auto-mode-alist #'string-match-p))
         (short (lambda (s)
                  (if (and s (>= (length s) 7))
                      (substring s 0 7)
                    (or s "?"))))
         (buf-a (generate-new-buffer
                 (format "*%s @ %s*" filename (funcall short sha-a))))
         (buf-b (generate-new-buffer
                 (format "*%s @ %s*" filename (funcall short sha-b))))
         (saved-config (current-window-configuration))
         ;; If invoked from a transient suffix, capture the prefix
         ;; command so we can reopen the menu after ediff quits --
         ;; transient can't stay active *during* ediff (ediff
         ;; captures focus via `recursive-edit'), but we can
         ;; rehydrate it post-quit so the user is right back where
         ;; they were.
         (transient-prefix (and (boundp 'transient-current-command)
                                transient-current-command)))
    (dolist (pair (list (cons sha-a buf-a) (cons sha-b buf-b)))
      (with-current-buffer (cdr pair)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (shipit-review-history-insert-file-at-sha (car pair) rel-path workdir)
        (when (and mode (fboundp mode)) (funcall mode))
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)))
    ;; `ediff-after-quit-hook-internal' is a hook variable -- a
    ;; *list* of functions, run via `run-hooks'.  Setting it to a
    ;; bare lambda crashes `ediff-really-quit' with a `listp'
    ;; mismatch.  Wrap in a list so it round-trips correctly.
    (setq ediff-after-quit-hook-internal
          (list
           (lambda ()
             (when (buffer-live-p buf-a) (kill-buffer buf-a))
             (when (buffer-live-p buf-b) (kill-buffer buf-b))
             (when saved-config
               (set-window-configuration saved-config))
             ;; Defer the transient re-invocation a tick: ediff
             ;; is still finalising its own cleanup when this
             ;; hook runs, so `call-interactively' immediately
             ;; would race with that teardown.
             (when (and transient-prefix (commandp transient-prefix))
               (run-at-time
                0 nil
                (lambda ()
                  (when (commandp transient-prefix)
                    (call-interactively transient-prefix))))))))
    (ediff-buffers buf-a buf-b)))

(defun shipit-review-history--show-file-at-sha (sha)
  "Open the file at SHA in another window using `git show'.
Read-only, with the same major mode the live file uses."
  (let* ((file shipit-review-history--file)
         (worktree shipit-review-history--worktree)
         (default-directory (file-name-as-directory worktree))
         (filename (file-name-nondirectory file))
         (short (substring sha 0 (min 7 (length sha))))
         (buf (get-buffer-create
               (format "*%s @ %s*" filename short))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (shipit-review-history-insert-file-at-sha sha file worktree))
      (let ((mode (assoc-default filename auto-mode-alist #'string-match-p)))
        (when (and mode (fboundp mode)) (funcall mode)))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun shipit-review-history-view-commit ()
  "View the file at the commit on the current line."
  (interactive)
  (let ((commit (shipit-review-history--commit-at-point)))
    (unless commit (user-error "No commit at point"))
    (shipit-review-history--show-file-at-sha (plist-get commit :sha))))

(defun shipit-review-history-ediff-vs-head ()
  "Ediff the file at the commit on the current line against HEAD."
  (interactive)
  (let ((commit (shipit-review-history--commit-at-point)))
    (unless commit (user-error "No commit at point"))
    (shipit-review-history--ediff (plist-get commit :sha) "HEAD")))

(defun shipit-review-history-mark-commit ()
  "Mark the commit on the current line as the first side of a comparison."
  (interactive)
  (let ((commit (shipit-review-history--commit-at-point)))
    (unless commit (user-error "No commit at point"))
    (setq-local shipit-review-history--mark (plist-get commit :sha))
    (message "Marked %s; press `c' on another commit to compare."
             (plist-get commit :short))))

(defun shipit-review-history-compare-with-mark ()
  "Ediff the marked commit against the commit on the current line."
  (interactive)
  (let ((mark shipit-review-history--mark)
        (commit (shipit-review-history--commit-at-point)))
    (unless mark
      (user-error "No marked commit; press `m' on a commit first"))
    (unless commit (user-error "No commit at point"))
    (shipit-review-history--ediff mark (plist-get commit :sha))))

(defun shipit-review-history--ediff (sha-a sha-b)
  "Run ediff between FILE@SHA-A and FILE@SHA-B with auto-cleanup."
  (shipit-review-history-ediff-files-at-shas
   sha-a sha-b
   shipit-review-history--file
   shipit-review-history--worktree))

(defun shipit-review-history-blame ()
  "Open `magit-blame' on the live file at the commit on the current line."
  (interactive)
  (let ((commit (shipit-review-history--commit-at-point))
        (source shipit-review-history--source-buffer))
    (unless commit (user-error "No commit at point"))
    (unless (and source (buffer-live-p source))
      (user-error "Original file buffer no longer alive"))
    (with-current-buffer source
      (cond
       ((fboundp 'magit-blame-addition)
        (with-no-warnings
          (magit-blame-addition (plist-get commit :sha))))
       (t
        (user-error "magit-blame-addition not available"))))))

(defun shipit-review-history-refresh ()
  "Re-fetch and re-render the timeline."
  (interactive)
  (let ((worktree shipit-review-history--worktree)
        (file shipit-review-history--file)
        (source shipit-review-history--source-buffer))
    (unless (and worktree file)
      (user-error "Timeline buffer is missing context"))
    (shipit-review-history--launch worktree file source (current-buffer))))

;;; Entry point ------------------------------------------------------

(defun shipit-review-history--pr-shas (source-buffer)
  "Return the list of full SHAs in the PR currently displayed in SOURCE-BUFFER."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (let ((commits (and (boundp 'shipit-review--pr-data)
                          shipit-review--pr-data
                          (cdr (assq 'commits shipit-review--pr-data)))))
        (mapcar (lambda (c) (cdr (assq 'sha c))) commits)))))

(defun shipit-review-history--worktree-and-file (source-buffer)
  "Return (WORKTREE-PATH . REPO-RELATIVE-FILE) for SOURCE-BUFFER, or nil."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (let* ((file (buffer-file-name))
             (worktree (and file
                            (cond
                             ((and (fboundp 'shipit--get-repo-root)
                                   (shipit--get-repo-root)))
                             (t (file-name-directory file))))))
        (when (and file worktree)
          (cons worktree (file-relative-name file worktree)))))))

(defun shipit-review-history--current-sha-from (source-buffer)
  "Return the SHA the SOURCE-BUFFER review-mode buffer is showing, or nil."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (and (boundp 'shipit-review--displayed-sha)
           shipit-review--displayed-sha))))

(defun shipit-review-history--launch (worktree file source &optional reuse-buffer)
  "Common launcher: fetch + render into REUSE-BUFFER (or a new one)."
  (let* ((buf (or reuse-buffer
                  (get-buffer-create
                   (format "*shipit-file-history: %s*" file)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading file history...\n"))
      (setq buffer-read-only t))
    (display-buffer buf)
    (shipit-review-history--fetch
     worktree file
     (lambda (commits)
       (shipit-review-history--render
        buf worktree file commits
        (shipit-review-history--pr-shas source) source
        (shipit-review-history--current-sha-from source))))))

;;;###autoload
(defun shipit-review-history-buffer-for-file (file-path)
  "Return the live `*shipit-file-history: FILE-PATH*' buffer, or nil.
Used by review-mode\='s navigation actions to refresh the
highlight after the displayed commit changes."
  (let ((name (format "*shipit-file-history: %s*" file-path)))
    (and (get-buffer name) (get-buffer name))))

;;;###autoload
(defun shipit-review-history-rehighlight-for-source (source-buffer)
  "Re-render the timeline for SOURCE-BUFFER\='s file with the new current SHA.
No-op when no timeline buffer is open for that file (so the
navigation handler can call this unconditionally).  Re-uses the
cached `shipit-review-history--commits' / `--pr-shas' instead of
re-running `git log', so the refresh is essentially free."
  (cond
   ((not (buffer-live-p source-buffer))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log "REHIGHLIGHT: source-buffer dead")))
   (t
    (let* ((info (shipit-review-history--worktree-and-file source-buffer))
           (worktree (car info))
           (file (cdr info))
           (buf (and file (shipit-review-history-buffer-for-file file))))
      (cond
       ((null file)
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "REHIGHLIGHT: no worktree-and-file for source=%s"
                             (buffer-name source-buffer))))
       ((null buf)
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log
           "REHIGHLIGHT: no timeline buffer for file=%s (source=%s)"
           file (buffer-name source-buffer))))
       ((not (buffer-live-p buf))
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log "REHIGHLIGHT: timeline buffer dead for file=%s" file)))
       (t
        (let ((current (shipit-review-history--current-sha-from source-buffer))
              (commits (buffer-local-value 'shipit-review-history--commits buf))
              (pr-shas (buffer-local-value 'shipit-review-history--pr-shas buf)))
          (cond
           (commits
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log
               "REHIGHLIGHT: rendering buf=%s file=%s current=%s commits=%d"
               (buffer-name buf) file
               (if current (substring current 0 7) "nil")
               (length commits)))
            (shipit-review-history--render
             buf worktree file commits pr-shas source-buffer current))
           (t
            (when (fboundp 'shipit--debug-log)
              (shipit--debug-log
               "REHIGHLIGHT: commits cache empty -- skipping (file=%s)"
               file)))))))))))

;;;###autoload
(defun shipit-review-file-history ()
  "Open the per-file commit timeline for the current review-mode buffer.
Lists every commit affecting this file, grouped into `In PR' (the
PR's commits) and `Before PR' (`shipit-review-history-pre-pr-limit'
older commits).  RET on a row views the file at that commit; `='
ediffs against HEAD; `m' / `c' compare two commits; `b' opens
`magit-blame' on the live file.

Resolves the source buffer via
`shipit-review--locate-source-buffer' so the timeline always
matches whichever file the review menu was invoked from --
even if focus has drifted to the preview / timeline window."
  (interactive)
  (let* ((source (or (and (fboundp 'shipit-review--locate-source-buffer)
                          (shipit-review--locate-source-buffer))
                     (current-buffer)))
         (info (shipit-review-history--worktree-and-file source))
         (worktree (car info))
         (file (cdr info)))
    (unless info
      (user-error "Buffer has no associated worktree file"))
    (shipit-review-history--launch worktree file source)))

(provide 'shipit-review-history)
;;; shipit-review-history.el ends here
