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

(defvar-local shipit-review--diff-overlays-hidden nil
  "Non-nil suppresses the PR-range diff overlays in this buffer.
`shipit-review--changed-lines' continues to update so hunk
navigation works regardless.")

(defvar-local shipit-review--commit-message-shown nil
  "Non-nil enables the per-line commit-message overlay.
Refreshes on every nav action that moves point so the overlay
tracks the SHA in focus.")

(defvar-local shipit-review--blame-shown nil
  "Non-nil enables `git blame' annotations on the current hunk.
Refreshes on hunk navigation so the annotations follow `n'/`p'.")

(defvar-local shipit-review--history-shown nil
  "Non-nil enables the Previous-change overlay above the current hunk.
Refreshes on hunk navigation so the summary follows `n'/`p'.")

(defvar-local shipit-review--commit-message-overlay nil
  "Overlay rendering the displayed SHA\\='s commit message, or nil.
Anchored at the line containing point so the overlay floats
next to whichever hunk the user is reading.")



(defvar-local shipit-review--history-side-buffer nil
  "Side-window buffer showing the previous change to the current hunk.
Holds the file content at the previous-change SHA with that
commit\\='s own diff overlays applied.  nil when the toggle is
off.")

(defvar-local shipit-review--side-prev-sha nil
  "PREV_SHA the history side buffer was populated from.
Set in the side buffer.  Used by the blame-mirror path so the
side buffer can run `git blame' at the right revision.")

(defvar-local shipit-review--side-rel-path nil
  "Repo-relative path used to populate the history side buffer.
For renamed files this may differ from the source buffer\\='s
`buffer-file-name' relative path.")

(defvar-local shipit-review--side-worktree nil
  "Worktree root for the history side buffer\\='s git invocations.")

(defvar-local shipit-review--history-chain nil
  "Cached list of pre-PR commits that touched the current hunk\\='s lines.
Newest-first plists with `:sha :short :author :time :ago
:subject'.  Set on `H' toggle / hunk navigation; walked via
`M-n' / `M-p' in the diff transient.  Buffer-local on the
SOURCE review buffer.")

(defvar-local shipit-review--history-index nil
  "Current index into `shipit-review--history-chain', or nil.
0 == newest pre-PR commit (the default the side buffer
shows on H toggle).  Larger indices reach further into the
file\\='s past.  Reset to 0 whenever the chain is rebuilt.")


(defvar shipit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r c") #'shipit-review-compare-menu)
    (define-key map (kbd "C-c r t") #'shipit-review-file-history)
    (define-key map (kbd "C-c r r") #'shipit-review-menu)
    (define-key map (kbd "C-c r d") #'shipit-review-diff-menu)
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
      ("i" "Toggle inline comments" shipit-review-toggle-inline-comments
       :transient t)
      ("b" "Toggle blame (live file)" shipit-review--blame-live
       :transient t)]
     ["Misc"
      ("q" "Quit menu" transient-quit-one)]]))

(when (fboundp 'advice-add)
  (advice-add 'shipit-review-menu :before
              (lambda (&rest _)
                (when (and (bound-and-true-p shipit-review-mode)
                           shipit-review--pr-data)
                  (setq shipit-review--transient-source-buffer
                        (current-buffer))))))

(defun shipit-review--changed-line-hunks ()
  "Group `shipit-review--changed-lines' into (START . END) hunks.
A hunk is a maximal run of consecutive line numbers.  Returns
the list sorted ascending by START, oldest hunk first.  Returns
nil when no PR diff is available (mode setup hasn't run yet, or
the file has no changes in the current comparison range)."
  (let ((nums (sort (mapcar #'car shipit-review--changed-lines) #'<))
        hunks start last)
    (dolist (l nums)
      (cond
       ((null start)             (setq start l last l))
       ((= l (1+ last))          (setq last l))
       (t                        (push (cons start last) hunks)
                                 (setq start l last l))))
    (when start (push (cons start last) hunks))
    (nreverse hunks)))

(defun shipit-review--goto-line (line)
  "Move point to the start of LINE, refresh sticky overlays, recenter.
The commit-message overlay (when toggled on) is re-anchored at
the new line so it floats next to the freshly-navigated hunk
instead of staying behind at the previous position.  The
recenter is deferred to the next idle tick so it survives the
transient prefix's own redisplay (same trick the timeline
buffer uses for its indicator)."
  (goto-char (point-min))
  (forward-line (1- line))
  (shipit-review--apply-commit-message-overlay)
  ;; Blame / history overlays anchor at the current hunk;
  ;; History overlay rebuild: re-renders the side buffer\='s
  ;; content + (when blame is active) re-runs magit-blame on
  ;; the side buffer at its new PREV_SHA.  Magit-blame on the
  ;; source buffer stays valid across hunk nav since the file
  ;; itself didn\='t change, so we don\='t re-run it here.
  (when shipit-review--history-shown
    (shipit-review--apply-history-overlay))
  (when-let ((win (get-buffer-window (current-buffer))))
    (let ((win win))
      (run-at-time
       0 nil
       (lambda ()
         (when (window-live-p win)
           (with-selected-window win (recenter))))))))

(defun shipit-review--next-diff-hunk ()
  "Move point to the next PR-diff hunk in this file.
Walks `shipit-review--changed-lines' (the parse of `git diff
BASE..HEAD -- file' that powers shipit's full-line overlays) so
navigation works for merged PRs too -- where disk == HEAD makes
`diff-hl' show nothing.  Falls back to `diff-hl-next-hunk' when
the shipit overlay layer is empty (mode setup incomplete).

Routed through `--with-source-buffer' so the buffer-local
`--changed-lines' / `--history-shown' / `--blame-shown' flags
are read from the source review buffer regardless of which
window the diff transient was invoked from."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let ((hunks (shipit-review--changed-line-hunks)))
       (cond
        (hunks
         (let* ((current-line (line-number-at-pos))
                (next (cl-find-if (lambda (h) (> (car h) current-line))
                                  hunks)))
           (cond
            (next
             (shipit-review--goto-line (car next))
             (message "Hunk %d (lines %d-%d)"
                      (1+ (cl-position next hunks))
                      (car next) (cdr next)))
            (t (message "No more PR diff hunks below")))))
        ((fboundp 'diff-hl-next-hunk)
         (call-interactively #'diff-hl-next-hunk))
        (t (user-error "No PR diff hunks (overlay layer empty)")))))))

(defun shipit-review--prev-diff-hunk ()
  "Move point to the previous PR-diff hunk in this file.
Same source as `shipit-review--next-diff-hunk' (PR-range diff)
so it works for merged PRs.  Falls back to
`diff-hl-previous-hunk' when shipit's overlay layer is empty.

Routed through `--with-source-buffer' for the same reason as
`--next-diff-hunk'."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let ((hunks (shipit-review--changed-line-hunks)))
       (cond
        (hunks
         (let* ((current-line (line-number-at-pos))
                (prev (cl-find-if (lambda (h) (< (cdr h) current-line))
                                  (reverse hunks))))
           (cond
            (prev
             (shipit-review--goto-line (car prev))
             (message "Hunk %d (lines %d-%d)"
                      (1+ (cl-position prev hunks))
                      (car prev) (cdr prev)))
            (t (message "No more PR diff hunks above")))))
        ((fboundp 'diff-hl-previous-hunk)
         (call-interactively #'diff-hl-previous-hunk))
        (t (user-error "No PR diff hunks (overlay layer empty)")))))))

;; Diff-actions transient: in-buffer hunk navigation, inline-comment
;; toggle, blame, and a shortcut to open the file timeline / main
;; review menu.  Bound to `C-c r d' on the review-mode keymap.
(when (fboundp 'transient-define-prefix)
  (transient-define-prefix shipit-review-diff-menu ()
    "Shipit diff-navigation actions for the current review file."
    [["Navigate hunks"
      ("n" "Next diff hunk"     shipit-review--next-diff-hunk
       :transient t)
      ("p" "Previous diff hunk" shipit-review--prev-diff-hunk
       :transient t)
      ("M-p" "Older change (H side)" shipit-review--history-older
       :transient t)
      ("M-n" "Newer change (H side)" shipit-review--history-newer
       :transient t)]
     ["Overlays"
      ("o" "Diff overlays"
       shipit-review-toggle-diff-overlays
       :transient t)
      ("i" "Inline comments"
       shipit-review-toggle-inline-comments
       :transient t)
      ("m" "Commit message"
       shipit-review-toggle-commit-message
       :transient t)
      ("H" "Previous change"
       shipit-review-toggle-previous-change
       :transient t)]
     ["Drill down"
      ("t" "File timeline"  shipit-review-file-history)
      ("b" "Toggle blame (live file)" shipit-review--blame-live
       :transient t)
      ("r" "Open review menu" shipit-review-menu)]
     ["Misc"
      ("q" "Quit menu" transient-quit-one)]]))

(when (fboundp 'advice-add)
  (advice-add 'shipit-review-diff-menu :before
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
  "Toggle `magit-blame-addition' on the source buffer.
When `H' (the previous-change side window) is also active,
mirrors magit-blame into the side buffer at its PREV_SHA so
both views show full magit-blame annotations (margin SHA +
date + author, with all the magit-blame navigation /
inspection commands).

The side buffer doesn\\='t visit a real file, so
`--apply-history-overlay' sets `magit-buffer-file-name' /
`magit-buffer-revision' on it; magit-blame uses those instead
of `buffer-file-name'."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      (shipit-review--blame-shown
       (setq-local shipit-review--blame-shown nil)
       (shipit-review--blame-quit-source)
       (shipit-review--blame-quit-side)
       (message "Blame: hidden"))
      (t
       (setq-local shipit-review--blame-shown t)
       (shipit-review--blame-source)
       (when shipit-review--history-shown
         (shipit-review--blame-side))
       (message "Blame: shown%s"
                (if shipit-review--history-shown
                    " (source + side window)"
                  "")))))))

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

;; Inline comments overlay -----------------------------------------

(defvar-local shipit-review--inline-comment-overlays nil
  "List of overlays showing PR inline comments in this buffer.
Set by `shipit-review--show-inline-comments'; cleared by
`shipit-review--hide-inline-comments'.  Non-nil = comments are
currently displayed.")

(defun shipit-review--inline-comments-for-file ()
  "Return PR inline comments matching the current file (non-outdated only).
Looks them up by walking the buffer-list for any buffer whose
`shipit--cached-inline-comments' is set -- typically the
PR's magit buffer."
  (let* ((file (buffer-file-name))
         (root (and file (shipit--get-repo-root)))
         (rel (and file root (file-relative-name file root)))
         (magit-buf
          (cl-find-if
           (lambda (b)
             (buffer-local-value 'shipit--cached-inline-comments b))
           (buffer-list))))
    (when (and magit-buf rel)
      (cl-remove-if-not
       (lambda (c)
         (let ((path (cdr (assq 'path c)))
               (outdated (cdr (assq 'outdated c))))
           (and path (string= path rel) (not outdated))))
       (buffer-local-value 'shipit--cached-inline-comments magit-buf)))))

(defun shipit-review--render-inline-comment (comment)
  "Build the multi-line `after-string' for an inline COMMENT overlay."
  (let* ((user (cdr (assq 'login (cdr (assq 'user comment)))))
         (body (or (cdr (assq 'body comment)) ""))
         (timestamp (cdr (assq 'created_at comment)))
         (header
          (concat
           (propertize (format "  💬 %s" (or user "?"))
                       'face 'shipit-inline-comment-username-face)
           (when (and timestamp (stringp timestamp))
             (concat " "
                     (propertize timestamp
                                 'face 'shipit-inline-comment-timestamp-face)))))
         (body-lines
          (mapconcat (lambda (l) (concat "     " l))
                     (split-string body "\n")
                     "\n")))
    (concat "\n" header "\n"
            (propertize body-lines
                        'face 'shipit-inline-comment-body-face)
            "\n")))

(defun shipit-review--show-inline-comments ()
  "Create overlays for PR inline comments in the current file buffer."
  (let ((comments (shipit-review--inline-comments-for-file))
        (count 0))
    (dolist (comment comments)
      (let ((line (cdr (assq 'line comment))))
        (when (and line (numberp line) (> line 0))
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (let* ((eol (line-end-position))
                   (ov (make-overlay eol eol)))
              (overlay-put ov 'after-string
                           (shipit-review--render-inline-comment comment))
              (overlay-put ov 'shipit-inline-comment t)
              (overlay-put ov 'shipit-comment-data comment)
              (overlay-put ov 'priority 100)
              (push ov shipit-review--inline-comment-overlays)
              (cl-incf count))))))
    count))

(defun shipit-review--hide-inline-comments ()
  "Remove inline-comment overlays from the current buffer."
  (dolist (ov shipit-review--inline-comment-overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq-local shipit-review--inline-comment-overlays nil))

(defun shipit-review-toggle-inline-comments ()
  "Toggle the display of PR inline comments overlaid on this file.
Independent of the other overlay toggles -- compose freely
with diff overlays, blame, etc.  Comments are read from the
PR's magit buffer (`shipit--cached-inline-comments') and
rendered as `after-string' overlays at each comment\\='s
target line.  Outdated comments are skipped."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      (shipit-review--inline-comment-overlays
       (shipit-review--hide-inline-comments)
       (message "Inline comments: hidden"))
      (t
       (let ((n (shipit-review--show-inline-comments)))
         (message "Inline comments: %d shown" n)))))))


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
  (shipit-review--apply-commit-message-overlay)
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
comparison target tracks the PR.

For pre-PR commits, sets `displayed-sha' directly, refreshes
the preview/timeline, AND repaints the diff overlays to show
\"this commit vs its first parent\".  Without that overlay
refresh the file would still show the PR-range tints at older
SHAs, which is misleading -- the user is now looking at a
historical commit, so we want the lines THIS commit changed."
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
      (let ((parent (shipit-review--git-parent-sha sha))
            (file (buffer-file-name)))
        (cond
         ((and parent file)
          (shipit-review--apply-diff-overlays parent sha file))
         (t
          ;; No parent or no file -- clear stale overlays so we
          ;; don't show a misleading PR-range tint at this SHA.
          (setq-local shipit-review--changed-lines nil)
          (remove-overlays (point-min) (point-max)
                           'shipit-review-change t))))
      (shipit-review--update-mode-line)
      (when (fboundp 'shipit-review-history-rehighlight-for-source)
        (shipit-review-history-rehighlight-for-source (current-buffer)))
      (shipit-review--update-revision-preview sha)
      (shipit-review--apply-commit-message-overlay)
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
  "Return unified diff between FROM-SHA and TO-SHA for FILE-PATH (string).
Runs from the worktree root so renamed files / paths nested
deeper than the buffer's `default-directory' resolve correctly.
Stderr is discarded so a missing-at-SHA path doesn't leak
`fatal: ...' into the diff parse."
  (condition-case err
      (let* ((root (or (and (fboundp 'shipit--get-repo-root)
                            (shipit--get-repo-root))
                       default-directory))
             (default-directory (file-name-as-directory root))
             (rel (file-relative-name file-path root)))
        (with-temp-buffer
          (process-file "git" nil '(t nil) nil
                        "diff" "--unified=0"
                        (format "%s..%s" from-sha to-sha)
                        "--" rel)
          (buffer-string)))
    (error
     (shipit--debug-log "Error getting diff: %s" (error-message-string err))
     "")))

(defun shipit-review--parse-diff-for-changed-lines (diff-output)
  "Parse unified diff output and extract line markers.
Returns list whose elements are one of:
  (LINE-NUM . `added')             real line in the worktree, full-line tint
  (LINE-NUM `removed' TEXT)        block of removed lines, anchored *before*
                                   LINE-NUM via a `before-string' overlay
LINE-NUM is always a NEW-file line number (what's in the
worktree).  TEXT is a single string with newline-separated
removed-line content (no leading `-')."
  (let ((lines (split-string diff-output "\n"))
        (current-line-new nil)
        ;; Pending removed lines waiting for an anchor.  Flushed
        ;; when we hit the first non-`-' line in a hunk (or the
        ;; next hunk header / EOF).
        (pending-removed nil)
        changed-lines)
    (cl-labels ((flush-removed
                  (anchor)
                  (when pending-removed
                    (push (list anchor 'removed
                                (mapconcat #'identity
                                           (nreverse pending-removed)
                                           "\n"))
                          changed-lines)
                    (setq pending-removed nil))))
      (shipit--debug-log "Parsing diff: %d lines" (length lines))
      (dolist (line lines)
        (cond
         ;; Hunk header: @@ -10,5 +15,7 @@
         ((string-match "^@@ -\\([0-9]+\\)" line)
          ;; Flush any removals pending from the previous hunk
          ;; against its last seen new-file position.  Edge case
          ;; (rare) where the diff ends on a `-' block.
          (flush-removed (or current-line-new 1))
          (let ((parsed (shipit-review--parse-diff-hunk-header line)))
            (when parsed
              (setq current-line-new (caddr parsed)))))
         ;; Line removed: stash content (without the leading `-')
         ;; and don't advance current-line-new.
         ((and current-line-new (string-prefix-p "-" line))
          (push (substring line 1) pending-removed))
         ;; Line added: anchor any pending removals just before
         ;; this added line, then mark the added line itself.
         ((and current-line-new (string-prefix-p "+" line))
          (flush-removed current-line-new)
          (push (cons current-line-new 'added) changed-lines)
          (setq current-line-new (1+ current-line-new)))
         ;; Context line: anchor pending removals just before this
         ;; context line, then advance.
         ((and current-line-new (or (string-prefix-p " " line)
                                    (string-prefix-p "\\" line)))
          (flush-removed current-line-new)
          (setq current-line-new (1+ current-line-new)))))
      ;; Tail: a `-'-only hunk leaves residual pending-removed.
      (flush-removed (or current-line-new 1)))
    (nreverse changed-lines)))

(defun shipit-review--git-parent-sha (sha)
  "Return SHA\='s first parent (`SHA^') resolved against this worktree, or nil.
Used by pre-PR navigation to show \"this commit vs its
predecessor\" overlays without needing an entry in `pr-data.commits'."
  (when sha
    (condition-case nil
        (let* ((root (or (and (fboundp 'shipit--get-repo-root)
                              (shipit--get-repo-root))
                         default-directory))
               (default-directory (file-name-as-directory root)))
          (with-temp-buffer
            (when (zerop (process-file "git" nil '(t nil) nil
                                       "rev-parse" "--verify"
                                       (concat sha "^")))
              (string-trim (buffer-string)))))
      (error nil))))

(defun shipit-review--render-overlays-from-changed-lines (changed-lines)
  "Render `shipit-review-change' overlays from CHANGED-LINES.
Caller is responsible for clearing existing overlays first.
See `shipit-review--parse-diff-for-changed-lines' for the
shape of each entry."
  (dolist (entry changed-lines)
    (cond
     ;; (LINE-NUM . `added') -- full-line tint at LINE-NUM.
     ((and (consp entry) (eq (cdr entry) 'added))
      (let ((line-num (car entry)))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line-num))
          (when (>= line-num 1)
            (let ((ov (make-overlay (line-beginning-position)
                                    (line-end-position))))
              (overlay-put ov 'shipit-review-change t)
              (overlay-put ov 'face 'diff-added)
              (overlay-put ov 'help-echo "added in PR diff"))))))
     ;; (LINE-NUM `removed' TEXT) -- before-string overlay
     ;; surfaces removed lines in place, just above LINE-NUM.
     ((and (listp entry) (eq (cadr entry) 'removed))
      (let* ((anchor (car entry))
             (text (caddr entry))
             (display
              (mapconcat
               (lambda (l)
                 (propertize (concat "-" l "\n")
                             'face 'diff-removed))
               (split-string text "\n")
               "")))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (max 1 anchor)))
          (let ((ov (make-overlay (line-beginning-position)
                                  (line-beginning-position))))
            (overlay-put ov 'shipit-review-change t)
            (overlay-put ov 'before-string display)
            (overlay-put ov 'help-echo "removed in PR diff"))))))))

(defun shipit-review--apply-diff-overlays (from-sha to-sha file-path)
  "Re-render shipit\='s full-line diff overlays for FROM-SHA..TO-SHA on FILE-PATH.
Updates `shipit-review--changed-lines' (used by hunk
navigation regardless of overlay state).  Renders the
line-tint / removed-line overlays unless the user toggled
them off via `shipit-review-toggle-diff-overlays' -- the
layer is independent of the other overlay toggles, so it
composes with comments / blame / etc."
  (let* ((diff-output (shipit-review--get-diff-output from-sha to-sha file-path))
         (changed-lines (shipit-review--parse-diff-for-changed-lines
                         diff-output)))
    (setq-local shipit-review--changed-lines changed-lines)
    (remove-overlays (point-min) (point-max) 'shipit-review-change t)
    (unless shipit-review--diff-overlays-hidden
      (shipit-review--render-overlays-from-changed-lines changed-lines))
    (length changed-lines)))

(defun shipit-review-toggle-diff-overlays ()
  "Toggle the PR-range diff overlays in this buffer.
Independent of the other overlay toggles -- compose freely
with inline comments, blame, etc.  `shipit-review--changed-
lines' is preserved when hidden so hunk navigation continues
to work."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      (shipit-review--diff-overlays-hidden
       (setq-local shipit-review--diff-overlays-hidden nil)
       (shipit-review--render-overlays-from-changed-lines
        shipit-review--changed-lines)
       (message "Diff overlays: shown"))
      (t
       (setq-local shipit-review--diff-overlays-hidden t)
       (remove-overlays (point-min) (point-max) 'shipit-review-change t)
       (message "Diff overlays: hidden"))))))

(defun shipit-review--commit-message-text (sha)
  "Return the formatted commit-message overlay string for SHA, or nil.
Runs `git log -1 --format=%B' (raw subject + body) from the
worktree root and wraps the result with a delimiter line and a
face-propertized header so it visually slots above whichever
file line the overlay is anchored at."
  (when sha
    (let* ((root (or (and (fboundp 'shipit--get-repo-root)
                          (shipit--get-repo-root))
                     default-directory))
           (default-directory (file-name-as-directory root))
           (rc nil)
           (raw nil)
           (msg
            (with-temp-buffer
              (setq rc (process-file
                        "git" nil '(t nil) nil
                        "log" "-1" "--format=%B" sha))
              (setq raw (buffer-string))
              (when (zerop rc)
                (string-trim raw)))))
      (shipit--debug-log
       "COMMIT-MSG-TEXT: sha=%s rc=%s raw-bytes=%d trimmed-bytes=%d"
       (substring sha 0 (min 7 (length sha)))
       rc (length (or raw "")) (length (or msg "")))
      (when (and msg (not (string-empty-p msg)))
        (let* ((rendered (if (fboundp 'shipit--render-markdown)
                             (shipit--render-markdown msg)
                           msg))
               (win (get-buffer-window (current-buffer) t))
               (width (max 60 (min 100
                                   (- (or (and win (window-width win))
                                          80)
                                      2))))
               (short (substring sha 0 (min 7 (length sha))))
               (label (format " Commit %s " short))
               (lpad 4)
               (rpad (max 4 (- width (length label) lpad)))
               (header (propertize
                        (concat (make-string lpad ?─) label
                                (make-string rpad ?─))
                        'face 'shipit-comment-indicator-face))
               (rule   (propertize (make-string width ?─)
                                   'face 'shipit-comment-indicator-face))
               (dim (copy-sequence rendered)))
          (add-face-text-property 0 (length dim) 'shadow t dim)
          (concat header "\n" dim "\n" rule "\n"))))))

(defun shipit-review--clear-commit-message-overlay ()
  "Remove the commit-message overlay if any."
  (when shipit-review--commit-message-overlay
    (delete-overlay shipit-review--commit-message-overlay)
    (setq-local shipit-review--commit-message-overlay nil)))

(defun shipit-review--apply-commit-message-overlay ()
  "Refresh the commit-message overlay near point if it\='s active.
No-op when `shipit-review--commit-message-shown' is nil.

Anchored at the start of the line containing point with a
multi-line `before-string', so the message floats next to
whichever hunk the user is reading.  Re-applied on every nav
action that moves point."
  (when shipit-review--commit-message-shown
    (let ((sha (or shipit-review--displayed-sha shipit-review--head-sha)))
      (shipit-review--clear-commit-message-overlay)
      (let ((text (shipit-review--commit-message-text sha)))
        (when text
          (let* ((anchor (line-beginning-position))
                 (ov (make-overlay anchor anchor)))
            (overlay-put ov 'before-string text)
            (overlay-put ov 'shipit-review-commit-message t)
            (setq-local shipit-review--commit-message-overlay ov)))))))

(defun shipit-review--clear-side-commit-message-overlay ()
  "Remove the commit-message overlay from the history side buffer."
  (let ((side shipit-review--history-side-buffer))
    (when (and side (buffer-live-p side))
      (with-current-buffer side
        (shipit-review--clear-commit-message-overlay)))))

(defun shipit-review--apply-side-commit-message-overlay ()
  "Render the previous-change commit message in the history side buffer.
Anchored at the side buffer\='s current line (set by
`--apply-history-overlay' to the diff-mapped equivalent of
the source\='s point).  Reuses
`shipit-review--commit-message-text' but with
`shipit-review--side-prev-sha' instead of the source\='s
displayed/head SHA -- the side buffer shows the previous
commit\='s file content, so its overlay should describe
that commit.

Caller (the `m' toggle / `--apply-history-overlay') decides
when to invoke -- this function does not check
`--commit-message-shown' itself."
  (let ((side shipit-review--history-side-buffer))
    (when (and side (buffer-live-p side))
      (with-current-buffer side
        (shipit-review--clear-commit-message-overlay)
        (when-let* ((sha shipit-review--side-prev-sha)
                    (text (shipit-review--commit-message-text sha)))
          (let* ((anchor (line-beginning-position))
                 (ov (make-overlay anchor anchor)))
            (overlay-put ov 'before-string text)
            (overlay-put ov 'shipit-review-commit-message t)
            (setq-local shipit-review--commit-message-overlay ov)))))))

(defun shipit-review-toggle-commit-message ()
  "Toggle the commit-message overlay near point.
Independent of the other overlay toggles -- composes with diff
/ comments / blame / history so the user can layer signals.
Refreshes on every nav action that moves point so the overlay
tracks the SHA in focus.

When the previous-change side window (`H') is active, also
toggles a parallel overlay in the side buffer rendering the
previous commit\='s message -- so `m' shows BOTH commits at
once: source\='s HEAD-of-PR commit + side\='s previous-change
commit."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      (shipit-review--commit-message-shown
       (setq-local shipit-review--commit-message-shown nil)
       (shipit-review--clear-commit-message-overlay)
       (when shipit-review--history-shown
         (shipit-review--clear-side-commit-message-overlay))
       (message "Commit message: hidden"))
      (t
       (setq-local shipit-review--commit-message-shown t)
       (shipit-review--apply-commit-message-overlay)
       (when shipit-review--history-shown
         (shipit-review--apply-side-commit-message-overlay))
       (message "Commit message: shown%s"
                (if shipit-review--history-shown
                    " (source + side window)"
                  "")))))))

;; Blame overlay -----------------------------------------------------

(defun shipit-review--blame-range (&optional context)
  "Return (START . END) line range to blame around point.
If point is inside a known PR-diff hunk, use the hunk\='s
extent plus CONTEXT lines on either side; otherwise blame
point ± CONTEXT lines.  CONTEXT defaults to 5."
  (let* ((cur (line-number-at-pos))
         (ctx (or context 5))
         (eob (line-number-at-pos (point-max)))
         (hunks (shipit-review--changed-line-hunks))
         (in-hunk (cl-find-if
                   (lambda (h) (and (>= cur (car h)) (<= cur (cdr h))))
                   hunks)))
    (cond
     (in-hunk
      (cons (max 1 (- (car in-hunk) ctx))
            (min eob (+ (cdr in-hunk) ctx))))
     (t
      (cons (max 1 (- cur ctx))
            (min eob (+ cur ctx)))))))

(defun shipit-review--blame-source ()
  "Activate magit-blame on the source buffer.
The source buffer visits the worktree file -- magit-blame
finds the right file/rev via `buffer-file-name' (HEAD).
Bypasses the `magit-blame-addition' suffix wrapper because its
arg is for transient flags (`-M' / `-w'), not a SHA.

Selects the source window before invoking magit-blame so the
initial line range (`window-start' / `window-end') refers to
the source buffer\='s visible region.  Without this, invoking
`b' from the diff transient with focus in the side window
would seed magit-blame with the side window\='s range against
the source content -- usually still valid but occasionally
out of bounds depending on relative scroll positions."
  (when (fboundp 'magit-blame--run)
    (require 'magit-blame nil t)
    (let ((win (get-buffer-window (current-buffer) t)))
      (cond
       ((window-live-p win)
        (with-selected-window win
          (with-no-warnings
            (magit-blame--pre-blame-assert 'addition)
            (magit-blame--pre-blame-setup  'addition)
            (magit-blame--run nil))))
       (t
        (with-no-warnings
          (magit-blame--pre-blame-assert 'addition)
          (magit-blame--pre-blame-setup  'addition)
          (magit-blame--run nil)))))))

(defun shipit-review--blame-side ()
  "Activate magit-blame on the history side buffer at PREV_SHA.
The side buffer doesn\='t visit a real file -- magit-blame
locates the file/rev via `magit-buffer-file-name' /
`magit-buffer-refname' / `magit-buffer-revision' set in
`--apply-history-overlay'.

Same `--run' bypass as `--blame-source' so nil args isn\='t
misinterpreted as transient flags.

`magit-blame--run' computes its initial line range with
`window-start' / `window-end' against the SELECTED window,
not the buffer\='s window.  Select the side window first so
the range matches the side buffer\='s visible region --
otherwise blame asks git for lines that don\='t exist in
this revision (source / side line numbers diverge after
n/p navigation when the file size has shifted between
PREV_SHA and HEAD) and the process returns
\"Blaming...failed\"."
  (let* ((side shipit-review--history-side-buffer)
         (win (and side (buffer-live-p side)
                   (get-buffer-window side t))))
    (when (and side (buffer-live-p side)
               (fboundp 'magit-blame--run))
      (require 'magit-blame nil t)
      (cond
       ((window-live-p win)
        (with-selected-window win
          (with-no-warnings
            (magit-blame--pre-blame-assert 'addition)
            (magit-blame--pre-blame-setup  'addition)
            (magit-blame--run nil))))
       (t
        (with-current-buffer side
          (with-no-warnings
            (magit-blame--pre-blame-assert 'addition)
            (magit-blame--pre-blame-setup  'addition)
            (magit-blame--run nil))))))))

(defun shipit-review--blame-quit-source ()
  "Quit `magit-blame-mode' in the source buffer if active.
Selects the source window first so
`magit-blame--update-margin' (called from
`magit-blame-mode -1') runs against the right window.  That
function ends with `(set-window-buffer (selected-window)
(current-buffer))' to commit `left-margin-width' changes --
without selecting the source window we\='d clobber whichever
window the user happens to have focused (e.g. the side
window) by replacing its content with the source buffer."
  (when (and (bound-and-true-p magit-blame-mode)
             (fboundp 'magit-blame-quit))
    (let ((win (get-buffer-window (current-buffer) t)))
      (cond
       ((window-live-p win)
        (with-selected-window win
          (with-no-warnings (magit-blame-quit))))
       (t (with-no-warnings (magit-blame-quit)))))))

(defun shipit-review--blame-quit-side ()
  "Quit `magit-blame-mode' in the history side buffer if active.

Selects the side window for the same reason as
`--blame-quit-source': `magit-blame--update-margin' calls
`set-window-buffer' on `(selected-window)'.  If we run this
while the user has focus elsewhere (typical: focus is in
the source window when n/p navigation triggers a re-mirror),
the wrong window\='s buffer gets replaced and its
`window-start' resets -- producing the visible \"side
buffer jumps to the top of file\" symptom.

Beware: this *and* `--blame-side' both run in sequence
during navigation; both must select the side window so the
quit + re-activate margin updates target it correctly."
  (let* ((side shipit-review--history-side-buffer)
         (win (and side (buffer-live-p side)
                   (get-buffer-window side t))))
    (when (and side (buffer-live-p side))
      (cond
       ((window-live-p win)
        (with-selected-window win
          (when (and (bound-and-true-p magit-blame-mode)
                     (fboundp 'magit-blame-quit))
            (with-no-warnings (magit-blame-quit)))))
       (t
        (with-current-buffer side
          (when (and (bound-and-true-p magit-blame-mode)
                     (fboundp 'magit-blame-quit))
            (with-no-warnings (magit-blame-quit)))))))))

;; History overlay: previous change to the current hunk -------------

(defun shipit-review--clear-history-overlay ()
  "Close the side window showing the previous-change file content.
Kills the history side buffer (if any) and removes its window."
  (when (and shipit-review--history-side-buffer
             (buffer-live-p shipit-review--history-side-buffer))
    (let ((win (get-buffer-window shipit-review--history-side-buffer t)))
      (when (window-live-p win)
        (delete-window win)))
    (kill-buffer shipit-review--history-side-buffer))
  (setq-local shipit-review--history-side-buffer nil))

(defun shipit-review--humanize-ago (unix-time)
  "Format UNIX-TIME as a human relative-age string.
Granularity steps:
  < 1 min  -> \"just now\"
  < 1 hr   -> \"Nm ago\"
  < 1 day  -> \"Nh ago\"
  < 1 week -> \"Nd ago\"
  < 1 mo   -> \"Nw ago\"   (weeks, integer)
  < 1 yr   -> \"Nmo ago\"  (months, integer; \"1.1mo\" was awkward)
  >= 1 yr  -> \"Mon YYYY\" (absolute, e.g. \"Mar 2024\") --
              for old commits the precise age stops mattering;
              a calendar anchor is more informative than
              \"1.1y ago\"."
  (let* ((delta (- (float-time) unix-time))
         (mins (/ delta 60))
         (hours (/ mins 60))
         (days (/ hours 24)))
    (cond
     ((< delta 60) "just now")
     ((< mins 60) (format "%dm ago" (round mins)))
     ((< hours 24) (format "%dh ago" (round hours)))
     ((< days 7) (format "%dd ago" (round days)))
     ((< days 30) (format "%dw ago" (max 1 (round (/ days 7.0)))))
     ((< days 365) (format "%dmo ago" (max 1 (round (/ days 30.0)))))
     (t (format-time-string "%b %Y" unix-time)))))

(defun shipit-review--previous-change-info (rel-path range)
  "Plist for most recent commit before BASE that touched RANGE in REL-PATH.
RANGE is (START . END) new-file line numbers.  Plist keys:
:sha :short :author :time :ago :subject.  Returns nil when no
prior commit exists (e.g. lines were introduced by the PR
itself).  `default-directory' must already be a worktree root."
  (let* ((start (car range))
         (end (cdr range))
         (base (or shipit-review--base-sha "HEAD"))
         (output
          (with-temp-buffer
            (when (zerop (process-file
                          "git" nil (list t nil) nil
                          "log" "-1"
                          "--format=%H%x09%an%x09%at%x09%s"
                          (format "-L%d,%d:%s" start end rel-path)
                          base))
              (buffer-string)))))
    (when (and output (not (string-empty-p output)))
      ;; `git log -L' prints the format line first, then the diff.
      ;; We only want the first line.
      (let* ((first (car (split-string output "\n" t)))
             (parts (split-string first "\t")))
        (when (>= (length parts) 4)
          (let* ((sha (nth 0 parts))
                 (author (nth 1 parts))
                 (time (string-to-number (nth 2 parts)))
                 (subject (nth 3 parts)))
            (list :sha sha
                  :short (substring sha 0 (min 7 (length sha)))
                  :author author
                  :time time
                  :ago (shipit-review--humanize-ago time)
                  :subject subject)))))))

(defun shipit-review--history-chain (rel-path range)
  "List of plists for ALL commits before BASE that touched RANGE in REL-PATH.
Newest-first.  Same plist shape as `--previous-change-info'
(`:sha :short :author :time :ago :subject') but without
`-1', so the full history of the line range is captured.
Drives `M-n' / `M-p' navigation through previous changes
in the side window.

`git log -L' interleaves a metadata line per commit with
the diff body for that commit.  We filter the output to
metadata lines (4 tab-separated fields where field 0 is a
40-char hex SHA) and ignore the diff content."
  (let* ((start (car range))
         (end (cdr range))
         (base (or shipit-review--base-sha "HEAD"))
         (output
          (with-temp-buffer
            (when (zerop (process-file
                          "git" nil (list t nil) nil
                          "log"
                          "--format=%H%x09%an%x09%at%x09%s"
                          (format "-L%d,%d:%s" start end rel-path)
                          base))
              (buffer-string)))))
    (when (and output (not (string-empty-p output)))
      (let (chain)
        (dolist (line (split-string output "\n" t))
          (let ((parts (split-string line "\t")))
            (when (and (>= (length parts) 4)
                       (string-match-p "^[0-9a-f]\\{40\\}$"
                                       (nth 0 parts)))
              (let* ((sha (nth 0 parts))
                     (author (nth 1 parts))
                     (time (string-to-number (nth 2 parts)))
                     (subject (nth 3 parts)))
                (push (list :sha sha
                            :short (substring sha 0 (min 7 (length sha)))
                            :author author
                            :time time
                            :ago (shipit-review--humanize-ago time)
                            :subject subject)
                      chain)))))
        (nreverse chain)))))

(defun shipit-review--history-side-buffer-name (rel-path)
  "Return the dedicated history-side-buffer name for REL-PATH."
  (format "*shipit-review previous: %s*"
          (file-name-nondirectory rel-path)))

(defun shipit-review--diff-hunks (from-sha to-sha rel-path)
  "Return list of (OLD-START OLD-COUNT NEW-START NEW-COUNT) for FROM..TO on REL.
Empty list when the diff is empty / git fails / paths don\\='t
match.  `default-directory' must already be a worktree root."
  (let ((output (shipit-review--get-diff-output from-sha to-sha rel-path))
        hunks)
    (dolist (line (split-string (or output "") "\n"))
      (when (string-match
             "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
             line)
        (push (list (string-to-number (match-string 1 line))
                    (string-to-number (or (match-string 2 line) "1"))
                    (string-to-number (match-string 3 line))
                    (string-to-number (or (match-string 4 line) "1")))
              hunks)))
    (nreverse hunks)))

(defun shipit-review--map-head-to-prev (head-line hunks)
  "Map HEAD-LINE to the corresponding line in PREV via HUNKS.
HUNKS is `(OLD-START OLD-COUNT NEW-START NEW-COUNT)' tuples
from a `--unified=0 PREV..HEAD' diff (so OLD = PREV file, NEW =
HEAD file).  Walks the hunks accumulating an offset:

  - HEAD-LINE before a hunk          => head-line + offset
  - HEAD-LINE inside a hunk\\='s NEW range => snap to OLD-START
                                       (the hunk\\='s anchor in PREV)
  - HEAD-LINE after all hunks         => head-line + total offset

The offset for each passed hunk is `(OLD-COUNT - NEW-COUNT)' --
positive when the hunk inserted lines into HEAD, negative when
it removed lines.  Returns the (1-based) PREV line number, or
HEAD-LINE itself when HUNKS is empty (file unchanged)."
  (cond
   ((null hunks) head-line)
   (t
    (let ((offset 0)
          result)
      (catch 'done
        (dolist (h hunks)
          (let* ((old-start (nth 0 h))
                 (old-count (nth 1 h))
                 (new-start (nth 2 h))
                 (new-count (nth 3 h))
                 (new-end (+ new-start (1- (max 1 new-count)))))
            (cond
             ((< head-line new-start)
              (throw 'done (max 1 (+ head-line offset))))
             ((<= head-line new-end)
              (throw 'done (max 1 old-start)))
             (t
              (setq offset (+ offset (- old-count new-count)))))))
        (max 1 (+ head-line offset)))))))

(defun shipit-review--apply-history-overlay (&optional pinned-info)
  "Open / refresh the side window showing the previous change.
The side buffer holds the file content at the most recent
commit (at or before `shipit-review--base-sha') that touched
the current hunk\='s line range, with that commit\='s own
diff overlays applied (added / removed lines tinted vs its
parent).  Reuses the same per-file buffer name across nav so
hunk movement updates the existing window in place.

When PINNED-INFO is supplied (a plist from
`shipit-review--history-chain'), it overrides the
default \"newest pre-PR commit for this hunk\" lookup --
used by `M-n' / `M-p' to walk further back into the
file\='s history without touching the cached chain or
index.

When PINNED-INFO is nil (default; H toggle / n/p call) the
chain is rebuilt from the current hunk range and the index
reset to 0.  This is the right behaviour for hunk
navigation since the range changes per hunk.

Caller (the toggle / `--goto-line' / `--history-step') is
responsible for checking `shipit-review--history-shown'."
  (let* ((file (buffer-file-name))
         ;; Capture the source buffer's current line BEFORE we
         ;; switch to the side buffer -- the side window will
         ;; scroll to the same line so the two views move
         ;; together as the user navigates hunks.
         (source-line (line-number-at-pos))
         (root (and file (shipit--get-repo-root)))
         (rel (and file root (file-relative-name file root)))
         (range (and rel (shipit-review--blame-range 0)))
         (file-mode major-mode))
    (cond
     ((not range)
      (message "Previous change: no hunk near point"))
     (t
      (let* ((default-directory (file-name-as-directory root))
             (info
              (cond
               (pinned-info pinned-info)
               (t
                (let ((chain (shipit-review--history-chain rel range)))
                  (setq-local shipit-review--history-chain chain)
                  (setq-local shipit-review--history-index
                              (and chain 0))
                  (car chain))))))
        (cond
         ((null info)
          (message "Previous change: %s introduced these lines"
                   (if shipit-review--base-sha
                       (substring shipit-review--base-sha 0 7)
                     "PR base"))
          ;; Tear down any stale side buffer from a different
          ;; navigation step.
          (shipit-review--clear-history-overlay))
         (t
          (let* ((sha (plist-get info :sha))
                 (parent
                  (with-temp-buffer
                    (when (zerop (process-file
                                  "git" nil (list t nil) nil
                                  "rev-parse" "--verify"
                                  (concat sha "^")))
                      (string-trim (buffer-string)))))
                 (path-at-sha
                  (or (and (fboundp 'shipit-review-history--historical-path)
                           (shipit-review-history--historical-path
                            sha rel root))
                      rel))
                 (buf-name (shipit-review--history-side-buffer-name rel))
                 (existing (get-buffer buf-name))
                 (buf (get-buffer-create buf-name)))
            (with-current-buffer buf
              (setq-local default-directory
                          (file-name-as-directory root))
              (let ((inhibit-read-only t))
                (erase-buffer)
                (when (fboundp 'shipit-review-history-insert-file-at-sha)
                  (shipit-review-history-insert-file-at-sha
                   sha path-at-sha root))
                (when (and file-mode (fboundp file-mode)
                           (not (eq major-mode file-mode)))
                  (funcall file-mode))
                ;; Apply the previous-change commit's own diff
                ;; overlays (parent..sha for path-at-sha) so the
                ;; reviewer sees what THAT commit did.
                (when parent
                  (shipit-review--apply-diff-overlays
                   parent sha path-at-sha))
                (setq-local header-line-format
                            (concat
                             (propertize " Previous change: "
                                         'face
                                         'shipit-comment-indicator-face)
                             (propertize (plist-get info :short)
                                         'face 'magit-hash)
                             "  "
                             (propertize (plist-get info :author)
                                         'face 'shipit-username-face)
                             "  "
                             (propertize (plist-get info :ago)
                                         'face 'shipit-timestamp-face)
                             "  -- "
                             (or (plist-get info :subject) "")))
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                ;; Map source\='s HEAD line through PREV..HEAD
                ;; diff to the corresponding line in PREV, so the
                ;; side buffer recenters on the same code region
                ;; the reviewer is reading -- not the literal
                ;; line number, which drifts when the file has
                ;; shifted between PREV and HEAD.
                (let* ((hunks (shipit-review--diff-hunks
                               sha "HEAD" path-at-sha))
                       (prev-line (shipit-review--map-head-to-prev
                                   source-line hunks)))
                  (goto-char (point-min))
                  (forward-line (1- (max 1 prev-line))))
                ;; Track the side buffer\='s git context so the
                ;; blame mirror (`b' toggle) can re-render here at
                ;; PREV_SHA / path-at-sha when activated.  Also set
                ;; `magit-buffer-file-name' / `magit-buffer-revision'
                ;; so `magit-blame-addition' can locate the file at
                ;; PREV_SHA even though this buffer doesn\='t visit
                ;; a real file.
                (setq-local shipit-review--side-prev-sha sha)
                (setq-local shipit-review--side-rel-path path-at-sha)
                (setq-local shipit-review--side-worktree root)
                (setq-local magit-buffer-file-name
                            (expand-file-name path-at-sha root))
                (setq-local magit-buffer-revision sha)
                (setq-local magit-buffer-refname sha)))
            (setq-local shipit-review--history-side-buffer buf)
            ;; Pop the side window only on first creation; later
            ;; refreshes update the existing window in place.
            (unless existing
              (display-buffer
               buf '((display-buffer-reuse-window
                      display-buffer-in-side-window)
                     (side . right)
                     (window-width . 0.5))))
            ;; Sync the side window's point + recenter BEFORE
            ;; the blame re-mirror.  After `erase-buffer' the
            ;; window\='s `window-start' / `window-point' markers
            ;; collapse to point-min; without an explicit reset
            ;; the side window shows the top of the file with
            ;; point off-screen on prev-line.  Doing this
            ;; synchronously (not deferred) and before
            ;; `--blame-quit-side' / `--blame-side' ensures the
            ;; magit-blame `set-window-buffer' calls inside
            ;; `magit-blame--update-margin' see a stable
            ;; window-start so they don\='t reset the viewport
            ;; back to the top.
            (when-let ((win (get-buffer-window buf t)))
              (let ((target (with-current-buffer buf (point))))
                (set-window-point win target)
                (with-selected-window win (recenter))))
            ;; If blame is currently active in the source buffer,
            ;; mirror it into the freshly populated side buffer so
            ;; the user doesn\='t have to re-toggle.  The previous
            ;; magit-blame-mode in this buffer (if any) was lost
            ;; when we erased the buffer; quit cleanly first then
            ;; re-activate against PREV_SHA.
            (when shipit-review--blame-shown
              (shipit-review--blame-quit-side)
              (shipit-review--blame-side))
            ;; Mirror the commit-message overlay too so `m' stays
            ;; visible across n/p.  The previous overlay was
            ;; nuked by `erase-buffer'; re-apply at the side\='s
            ;; new point with `--side-prev-sha' as the source.
            (when shipit-review--commit-message-shown
              (shipit-review--apply-side-commit-message-overlay))
            (message "Previous change: %s by %s, %s"
                     (plist-get info :short)
                     (plist-get info :author)
                     (plist-get info :ago))))))))))

(defun shipit-review-toggle-previous-change ()
  "Toggle a side-window preview of the previous change to the current hunk.
Independent of the other overlay toggles -- composes with diff
overlays so the user can layer \"what the PR changes\" and
\"what touched these lines last\" at the same time.

The side buffer holds the file content at the most recent
commit (at or before `shipit-review--base-sha') that touched
the current hunk\\='s line range, with that commit\\='s own
diff overlays applied (added / removed lines tinted vs its
parent).  Pressing `H' again closes the side window.

Hunk navigation (`n'/`p') refreshes the side buffer in place
and scrolls it to the source buffer\\='s line so the two views
move together -- a chronological zoom on the same region:
HEAD on the left, previous change on the right."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      (shipit-review--history-shown
       (setq-local shipit-review--history-shown nil)
       (shipit-review--clear-history-overlay)
       (message "Previous change: hidden"))
      (t
       (setq-local shipit-review--history-shown t)
       (shipit-review--apply-history-overlay))))))

(defun shipit-review--history-step (delta)
  "Step DELTA positions through `shipit-review--history-chain'.
DELTA is +1 for older (further back) or -1 for newer (toward
BASE).  Re-renders the side buffer at the new commit without
rebuilding the chain or shifting the source buffer\='s point.

Errors out clearly when:
  - the chain isn\='t loaded (`H' not active or no commits
    have ever touched this range);
  - the step would land outside the chain (already at the
    first / last commit) -- with a hint about which boundary."
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      ((not shipit-review--history-shown)
       (user-error
        "Previous change (`H') is not active -- nothing to step through"))
      ((null shipit-review--history-chain)
       (user-error
        "No history chain loaded -- press n/p to land on a hunk first"))
      (t
       (let* ((chain shipit-review--history-chain)
              (idx (or shipit-review--history-index 0))
              (new-idx (+ idx delta))
              (total (length chain)))
         (cond
          ((< new-idx 0)
           (message "Already at the most recent pre-PR change (1/%d)"
                    total))
          ((>= new-idx total)
           (message "No older changes to these lines (%d/%d)"
                    total total))
          (t
           (setq-local shipit-review--history-index new-idx)
           (let ((info (nth new-idx chain)))
             (shipit-review--apply-history-overlay info)
             (message "Previous change %d/%d: %s by %s, %s"
                      (1+ new-idx) total
                      (plist-get info :short)
                      (plist-get info :author)
                      (plist-get info :ago)))))))))))

(defun shipit-review--history-older ()
  "Walk the H side window one commit further back into history."
  (interactive)
  (shipit-review--history-step 1))

(defun shipit-review--history-newer ()
  "Walk the H side window one commit forward (toward BASE)."
  (interactive)
  (shipit-review--history-step -1))

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
                          shipit-review--head-sha))))
          (shipit-review--apply-diff-overlays
           from-sha to-sha (buffer-file-name)))
      (error
       (shipit--debug-log "Error updating diff overlays: %s" (error-message-string err))))))

(defun shipit-review-mode--teardown ()
  "Cleanup shipit review mode when disabled."
  (shipit--debug-log "Tearing down shipit-review-mode from buffer: %s" (buffer-name))
  ;; Remove all review-related overlays
  (remove-overlays (point-min) (point-max) 'shipit-review-change t))

(provide 'shipit-review-mode)
;;; shipit-review-mode.el ends here
