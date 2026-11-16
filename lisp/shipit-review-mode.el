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
(require 'shipit-rounded-section)
(require 'shipit-review-history)
(require 'vc-git)
(require 'transient nil t)

(defgroup shipit-review nil
  "Shipit worktree review mode for interactive PR exploration."
  :group 'shipit)

(defun shipit-review--sha-reachable-p (sha worktree-path)
  "Return non-nil when SHA exists in the git object database at WORKTREE-PATH.
A SHA from `pr-data.base.sha\\=' can be perfectly valid on the
server but absent locally -- common after a fresh worktree clone
that hasn\\='t fetched the merge-base.  Using such a SHA later in
`git log SHA..HEAD\\=' silently fails and breaks every downstream
nav site, so the resolver verifies reachability before handing
it back."
  (when (and sha (stringp sha) (> (length sha) 0)
             worktree-path (file-directory-p worktree-path))
    (let ((default-directory (file-name-as-directory worktree-path)))
      (with-temp-buffer
        (zerop (process-file "git" nil t nil
                             "rev-parse" "--verify" "--quiet"
                             (concat sha "^{commit}")))))))

(defun shipit-review--merge-base-against (ref worktree-path)
  "Return `git merge-base HEAD REF\\=' in WORKTREE-PATH, or nil on failure.
REF is something like `origin/master\\=' / `origin/HEAD\\=' -- a
remote-tracking ref that\\='s expected to exist locally.  Returns
nil when the ref doesn\\='t resolve or merge-base can\\='t find a
common ancestor, so the resolver can fall through to the next
candidate."
  (when (and ref (stringp ref) (> (length ref) 0)
             worktree-path (file-directory-p worktree-path))
    (let ((default-directory (file-name-as-directory worktree-path)))
      (with-temp-buffer
        (when (zerop (process-file
                      "git" nil t nil
                      "merge-base" "HEAD" ref))
          (let ((sha (string-trim (buffer-string))))
            (and (> (length sha) 0) sha)))))))

(defun shipit-review--resolve-base-sha (pr-data &optional worktree-path)
  "Return a usable base SHA for review mode, given PR-DATA + WORKTREE-PATH.

Tries, in order:

1. `pr-data\\='s `base.sha\\=' (the canonical value from the GitHub
   API) -- ONLY when it\\='s reachable in WORKTREE-PATH.  A stale
   worktree may not have fetched the base commit yet, in which
   case handing it back would poison every subsequent `git log\\='
   call.
2. `git merge-base HEAD origin/BASE-REF\\=' inside WORKTREE-PATH,
   where BASE-REF comes from `pr-data\\='s `base.ref\\=' (e.g.
   `master\\=' / `main\\=').  Useful when (a) `pr-data\\=' was loaded
   from a context that strips the base subobject, or (b) the
   pr-data sha is unreachable locally.
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
           (> (length base-from-data) 0)
           (shipit-review--sha-reachable-p base-from-data worktree-path))
      (setq resolved base-from-data source 'pr-data))
     ((and worktree-path (file-directory-p worktree-path))
      (cond
       ((and base-ref (stringp base-ref) (> (length base-ref) 0)
             (setq resolved (shipit-review--merge-base-against
                             (concat "origin/" base-ref) worktree-path)))
        (setq source (format "merge-base origin/%s" base-ref)))
       ((setq resolved (shipit-review--merge-base-against
                        "origin/HEAD" worktree-path))
        (setq source "merge-base origin/HEAD")))))
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

(defvar-local shipit-review--showing-historical-sha nil
  "Non-nil when this buffer is showing `git show SHA:path' content.
Set by `shipit-review--present-content-at-sha' when the user
navigates to a SHA other than the worktree HEAD; cleared when
they navigate back.  While non-nil the buffer is forced
read-only so an accidental save can't write historical
content to the worktree file.")

(defvar-local shipit-review--in-content-swap nil
  "Non-nil while the buffer is mid-`erase-buffer' / `insert' swap.
Set by `shipit-review--swap-to-historical-content' and
`--restore-worktree-content' to suppress the
`after-change-functions' edit-tracking hook -- without it,
every line of the swapped-in content gets marked as a
user edit and the post-command sweep then clears them on
every nav step, making commit walks visibly stutter.")

(defvar-local shipit-review--current-commit-index nil
  "Current commit index when navigating through PR commits (1-indexed).")

(defvar-local shipit-review--total-commits nil
  "Total number of commits in PR.")

;; Defined further down, next to the code of the history side.
(defvar shipit-review--active-history-side)
(defvar shipit-review--file-commits-list)

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

;; Mark the PR-context locals as `permanent-local' so they survive
;; `revert-buffer' / major-mode re-runs.  Without this, reverting
;; the file kills the PR data and the next `n' / `c a' fails with
;; \"Cannot find the review-mode buffer for this PR\".  The hooks
;; on shipit-review-mode itself need a separate `change-major-mode-
;; hook' guard, but for the data slots this property is enough.
(dolist (sym '(shipit-review--pr-data
               shipit-review--head-sha
               shipit-review--base-sha
               shipit-review--comparison-target
               shipit-review--showing-historical-sha
               shipit-review--displayed-sha))
  (put sym 'permanent-local t))

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


;; Defined further down, as a transient.
(declare-function shipit-review-menu "shipit-review-mode")

(defvar shipit-review-mode-map (make-sparse-keymap)
  "Keymap for `shipit-review-mode', in the buffer of the file under review.
It binds nothing.  The keys of a review session are in its legend
window, `shipit-review-actions-mode-map'.  Bind your own keys here, for
example to `shipit-review-menu' or `shipit-review-file-history'.")

(defvar shipit-review--transient-source-buffer nil
  "Buffer captured at the last `shipit-review-menu' invocation.
Used by `shipit-review--locate-source-buffer' to keep n/p, the
timeline, and the preview all routed to the same review-mode
buffer for the entire transient session -- so a stale timeline
window for a different file can't redirect navigation.")

;; Transient menu for comparison target
(defconst shipit-review--menu-display-action
  '(display-buffer-at-bottom
    (dedicated . t)
    (inhibit-same-window . t))
  "Where the menus of a review session show.
Not in a side window, as a transient shows by default.  The legend is a
side window at the bottom, and Emacs gives the windows on one side one
height, so a one-line legend cut a menu in its row off.")

(when (fboundp 'transient-define-prefix)
  (transient-define-prefix shipit-review-compare-menu ()
    "Menu to adjust diff comparison target in review mode."
    :display-action shipit-review--menu-display-action
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
    :display-action shipit-review--menu-display-action
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

(defvar-local shipit-review--nav-indicator-overlay nil
  "Buffer-local overlay marking the line of the last navigation jump.
Sticky -- stays until the next jump (or mode teardown) so
the user can look away from the source window and still
find their place.")

(defun shipit-review--pulse-current-line ()
  "Place a sticky fringe indicator on the current line.
Replaces the previous indicator in this buffer so only the
most recent jump is marked.  A momentary pulse was tried
first but PR-range diff overlays kept washing it out on
changed lines (the exact lines you most need to find); a
fringe arrow lives in the fringe column where shipit draws
nothing, so it can\\='t be obscured, and being sticky means
there\\='s no fade-timer race -- the user can scroll, switch
windows, and still come back to a visible marker.

The function name keeps `pulse' for backward compatibility
with existing call sites; the implementation is now a
permanent fringe arrow rather than a momentary face flash."
  (when (and shipit-review--nav-indicator-overlay
             (overlayp shipit-review--nav-indicator-overlay))
    (delete-overlay shipit-review--nav-indicator-overlay))
  (let ((ov (make-overlay (line-beginning-position)
                          (line-beginning-position))))
    (overlay-put ov 'before-string
                 (propertize " " 'display
                             '(left-fringe right-triangle
                                           shipit-comment-indicator-face)))
    (overlay-put ov 'shipit-review-nav-indicator t)
    (setq shipit-review--nav-indicator-overlay ov)))

(defun shipit-review--goto-line (line)
  "Move point to the start of LINE, refresh sticky overlays, recenter.
The commit-message overlay (when toggled on) is re-anchored at
the new line so it floats next to the freshly-navigated hunk
instead of staying behind at the previous position.  The
recenter is deferred to the next idle tick so it survives the
transient prefix's own redisplay (same trick the timeline
buffer uses for its indicator).

`set-window-point' is called unconditionally on every window
showing this buffer so the deferred `with-selected-window'
recenter doesn't snap point back to the window's STALE
saved position -- without this, navigating from the legend
buffer would update the source buffer's point but the later
`with-selected-window' would restore the old window-point,
leaving the next n/p (or c n / c p) reading from line 1
again and looking like the jump didn't advance."
  (goto-char (point-min))
  (forward-line (1- line))
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (set-window-point win (point)))
  (shipit-review--apply-commit-message-overlay)
  (shipit-review--pulse-current-line)
  ;; Don\='t rebuild the H side buffer on every B-hunk nav --
  ;; the user found it confusing that A\='s revision changed
  ;; every time they pressed n/p on B.  Just scroll A\='s
  ;; viewport to track source\='s new line (via the existing
  ;; PREV..HEAD diff-hunk map).  A\='s SHA / content stays
  ;; pinned at whatever the user last chose (initial H, or
  ;; the last M-n/M-p step) until they explicitly move it.
  (when shipit-review--history-shown
    (shipit-review--sync-side-viewport-to-source-line))
  (when-let ((win (get-buffer-window (current-buffer))))
    (let ((win win))
      (run-at-time
       0 nil
       (lambda ()
         (when (window-live-p win)
           (with-selected-window win (recenter))))))))

(defun shipit-review--sync-side-viewport-to-source-line ()
  "Scroll the H side buffer to source\='s current line via PREV..HEAD diff.
Lighter alternative to `--apply-history-overlay': keeps the
side buffer\='s content / SHA untouched, just maps source\='s
HEAD line to the equivalent PREV line and recenters there.

Used by `--goto-line' (B-side n/p navigation) so the user
can scan the same code region in both buffers without A
re-rendering at a different SHA each step.  Initial H
toggle and M-n / M-p still go through
`--apply-history-overlay' / `--history-step', which DO
rebuild the side\='s content."
  (let* ((side shipit-review--history-side-buffer)
         (source-line (line-number-at-pos))
         (sha (and side (buffer-live-p side)
                   (buffer-local-value 'shipit-review--side-prev-sha
                                       side)))
         (path-at-sha (and side (buffer-live-p side)
                           (buffer-local-value
                            'shipit-review--side-rel-path side)))
         (root (and side (buffer-live-p side)
                    (buffer-local-value
                     'shipit-review--side-worktree side))))
    (when (and side (buffer-live-p side) sha path-at-sha root)
      (let* ((default-directory (file-name-as-directory root))
             (hunks (shipit-review--diff-hunks sha "HEAD" path-at-sha))
             (prev-line (shipit-review--map-head-to-prev
                         source-line hunks)))
        (with-current-buffer side
          (goto-char (point-min))
          (forward-line (1- (max 1 prev-line))))
        (when-let ((win (get-buffer-window side t)))
          (set-window-point win
                            (with-current-buffer side (point)))
          (with-selected-window win (recenter)))))))

(defun shipit-review--active-hunk-buffer ()
  "Return the buffer whose hunks `n' / `p' should walk.
With active side \\='history, return the H side buffer (so
n/p walks the parent..sha diff for the previous-change
commit).  Otherwise return the source buffer (walks
BASE..HEAD hunks, the original behaviour)."
  (cond
   ((and (eq shipit-review--active-history-side 'history)
         shipit-review--history-side-buffer
         (buffer-live-p shipit-review--history-side-buffer))
    shipit-review--history-side-buffer)
   (t (current-buffer))))

(defun shipit-review--with-active-window (fn)
  "Run FN with the active-side window selected.
Resolves the source buffer via
`shipit-review--with-source-buffer', picks the active side
(source or H) via `shipit-review--active-hunk-buffer', and
runs FN inside that buffer's window so commands like
`recenter' / `scroll-up-command' / `scroll-down-command'
operate on the buffer the user is actually navigating
rather than on the legend window."
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((buf (shipit-review--active-hunk-buffer))
            (win (and (buffer-live-p buf)
                      (get-buffer-window buf t))))
       (cond
        ((window-live-p win)
         (with-selected-window win (funcall fn)))
        (t (message "No window for active side")))))))

(defun shipit-review-recenter-active ()
  "Recenter the active-side buffer (source or H) via `C-l'."
  (interactive)
  (shipit-review--with-active-window #'recenter))

(defun shipit-review-scroll-active-up ()
  "Page the active-side buffer up (toward older lines, ediff `V')."
  (interactive)
  (shipit-review--with-active-window #'scroll-down-command))

(defun shipit-review-scroll-active-down ()
  "Page the active-side buffer down (toward newer lines, ediff `v')."
  (interactive)
  (shipit-review--with-active-window #'scroll-up-command))

(defun shipit-review-next-line-active ()
  "Move point down one line in the active-side buffer.
Drops the fringe nav indicator on the new line so the
cursor position is visible from the legend window even
with `cursor-in-non-selected-windows' off.  Wrapped in
`ignore-errors' so hitting beginning / end of buffer
doesn't abort the legend command."
  (interactive)
  (shipit-review--with-active-window
   (lambda ()
     (ignore-errors (next-line 1))
     (shipit-review--pulse-current-line))))

(defun shipit-review-prev-line-active ()
  "Move point up one line in the active-side buffer.
Drops the fringe nav indicator; see
`shipit-review-next-line-active'."
  (interactive)
  (shipit-review--with-active-window
   (lambda ()
     (ignore-errors (previous-line 1))
     (shipit-review--pulse-current-line))))

(defun shipit-review--next-diff-hunk ()
  "Move point to the next diff hunk in the ACTIVE side\\='s buffer.
With active side \\='source (default), walks
`shipit-review--changed-lines' on the source (the PR\\='s
BASE..HEAD diff).  With active side \\='history, walks the
H side buffer\\='s changed-lines (its own parent..sha diff).
Falls back to `diff-hl-next-hunk' when the overlay layer
is empty."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let ((active (shipit-review--active-hunk-buffer)))
       (with-current-buffer active
         (let ((hunks (shipit-review--changed-line-hunks)))
           (cond
            (hunks
             (let* ((current-line (line-number-at-pos))
                    (next (cl-find-if (lambda (h) (> (car h) current-line))
                                      hunks)))
               (cond
                (next
                 (shipit-review--goto-line (car next))
                 (let ((win (get-buffer-window (current-buffer) t)))
                   (when (window-live-p win)
                     (set-window-point win (point))))
                 (message "Hunk %d (lines %d-%d) on %s"
                          (1+ (cl-position next hunks))
                          (car next) (cdr next)
                          (if (eq shipit-review--active-history-side 'history)
                              "A (prev)" "B (HEAD)")))
                (t (message "No more diff hunks below")))))
            ((fboundp 'diff-hl-next-hunk)
             (call-interactively #'diff-hl-next-hunk))
            (t (user-error "No diff hunks (overlay layer empty)")))))))))

(defun shipit-review--prev-diff-hunk ()
  "Move point to the previous diff hunk in the ACTIVE side\\='s buffer.
See `shipit-review--next-diff-hunk' for the dispatch on
active side."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let ((active (shipit-review--active-hunk-buffer)))
       (with-current-buffer active
         (let ((hunks (shipit-review--changed-line-hunks)))
           (cond
            (hunks
             (let* ((current-line (line-number-at-pos))
                    (prev (cl-find-if (lambda (h) (< (cdr h) current-line))
                                      (reverse hunks))))
               (cond
                (prev
                 (shipit-review--goto-line (car prev))
                 (let ((win (get-buffer-window (current-buffer) t)))
                   (when (window-live-p win)
                     (set-window-point win (point))))
                 (message "Hunk %d (lines %d-%d) on %s"
                          (1+ (cl-position prev hunks))
                          (car prev) (cdr prev)
                          (if (eq shipit-review--active-history-side 'history)
                              "A (prev)" "B (HEAD)")))
                (t (message "No more diff hunks above")))))
            ((fboundp 'diff-hl-previous-hunk)
             (call-interactively #'diff-hl-previous-hunk))
            (t (user-error "No diff hunks (overlay layer empty)")))))))))

;; Diff-actions legend (ediff-style non-modal control panel)
;; ---------------------------------------------------------
;;
;; A small persistent buffer at the bottom of the frame shows the
;; available diff actions.  Unlike a transient menu it doesn\='t
;; capture focus -- the user can freely scroll / edit the source
;; and side buffers while the legend is visible, then jump to the
;; legend (C-x o or by selecting its window) and press a single
;; key to run a command.  Commands route to the SOURCE review
;; buffer via `shipit-review--with-source-buffer'.

(defconst shipit-review--actions-buffer-name "*shipit diff actions*"
  "Name of the persistent legend buffer.")

(defvar shipit-review-actions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "$") #'shipit-process-buffer)
    ;; Navigate hunks
    (define-key map (kbd "n")   #'shipit-review--next-diff-hunk)
    (define-key map (kbd "p")   #'shipit-review--prev-diff-hunk)
    (define-key map (kbd "M-n") #'shipit-review--history-newer)
    (define-key map (kbd "M-p") #'shipit-review--history-older)
    ;; Overlays
    (define-key map (kbd "o")   #'shipit-review-toggle-diff-overlays)
    (define-key map (kbd "i")   #'shipit-review-toggle-inline-comments)
    (define-key map (kbd "m")   #'shipit-review-toggle-commit-message)
    ;; `|' (mirrors ediff's vert-split toggle) opens / closes the
    ;; A side window showing the previous change.  Was `H'; renamed
    ;; for muscle-memory consistency with ediff control panels.
    (define-key map (kbd "|")   #'shipit-review-toggle-previous-change)
    ;; Drill down
    (define-key map (kbd "t")   #'shipit-review-toggle-timeline)
    (define-key map (kbd "b")   #'shipit-review--blame-live)
    (define-key map (kbd "e")   #'shipit-review--ediff-with-history-side)
    (define-key map (kbd "r")   #'shipit-review-menu)
    ;; Swap which window M-n / M-p navigate when H is active
    (define-key map (kbd "s")   #'shipit-review-toggle-active-side)
    ;; Inline comments live under the `c' chord (c a / c e / c r /
    ;; c R for actions, c n / c p for navigation).  Grouping them
    ;; behind a prefix frees the top-level letters that were
    ;; previously consumed (C / y / +) and makes the legend's
    ;; Comments column readable at a glance.
    (let ((cmap (make-sparse-keymap)))
      (define-key cmap (kbd "n") #'shipit-review-next-comment)
      (define-key cmap (kbd "p") #'shipit-review-prev-comment)
      (define-key cmap (kbd "a") #'shipit-review-add-inline-comment)
      (define-key cmap (kbd "e") #'shipit-review-edit-comment-at-point)
      (define-key cmap (kbd "r") #'shipit-review-reply-to-comment-at-point)
      (define-key cmap (kbd "R") #'shipit-review-react-to-comment-at-point)
      (define-key cmap (kbd "s") #'shipit-review-suggest-from-edits)
      (define-key map  (kbd "c") cmap))
    ;; Recenter / page the M-n / M-p target buffer.  `C-l' matches
    ;; vanilla Emacs; `v' / `V' match ediff's scroll-down / scroll-up
    ;; convention so the user doesn't have to focus the source / H
    ;; window manually just to scroll.
    (define-key map (kbd "C-l") #'shipit-review-recenter-active)
    (define-key map (kbd "C-n") #'shipit-review-next-line-active)
    (define-key map (kbd "C-p") #'shipit-review-prev-line-active)
    (define-key map (kbd "v")   #'shipit-review-scroll-active-down)
    (define-key map (kbd "V")   #'shipit-review-scroll-active-up)
    ;; Toggle between the minimal one-line legend and the full
    ;; grouped view (ediff `?' convention).  Hiding the legend
    ;; entirely is intentionally NOT supported -- the legend's
    ;; keymap is where every action lives, so a hidden legend
    ;; means no keys.
    (define-key map (kbd "?")   #'shipit-review--actions-legend-toggle-expand)
    ;; Quit review session (ediff-style: tear down all temp buffers
    ;; + restore pre-review window layout)
    (define-key map (kbd "q")   #'shipit-review-quit)
    map)
  "Keymap for the shipit-review actions legend buffer.")

(define-derived-mode shipit-review-actions-mode special-mode "Shipit/A"
  "Major mode for the non-modal shipit diff actions legend.
The legend buffer is read-only and shows a fixed set of
single-character bindings.  Each command routes through
`shipit-review--with-source-buffer' so it acts on the
correct review buffer regardless of which window the
legend was invoked from."
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq mode-line-format
        ;; Keep `?' and `q' hints in the mode line so they remain
        ;; visible even when the legend collapses to just the mode-
        ;; line row -- discoverability for "how do I bring the menu
        ;; back / quit" without any content row.
        '(" " (:propertize "shipit diff actions"
                           face shipit-comment-indicator-face)
          "  -- "
          (:propertize "?" face (:weight bold
                                         :inherit font-lock-keyword-face))
          " toggle menu  "
          (:propertize "q" face (:weight bold
                                         :inherit font-lock-keyword-face))
          " quit review session")))

(defvar shipit-review--actions-legend-expanded nil
  "Non-nil when the legend shows its full grouped menu.
Toggled by `?' on the legend buffer.  Default (nil) shows a
single-line minimal legend with the most-used keys plus a
`? expand menu' hint -- enough to stay productive without
filling a third of the frame.  The expanded view is the
ediff-style multi-column layout built from
`shipit-review--actions-legend-sections'.")

(defun shipit-review--actions-legend-minimal ()
  "Build the minimal one-line legend.
Keeps the essentials -- hunk nav, commit walk, the A side, the side
that the commit walk moves, quit --
on a SINGLE row with the `? expand' hint inline so the
signpost to the full menu travels with the shortcuts and
can't be cropped by a short side-window."
  (let ((key-face '(:weight bold :inherit font-lock-keyword-face))
        (hint-face '(:inherit shadow)))
    (concat
     "  "
     (propertize "n" 'face key-face) " next   "
     (propertize "p" 'face key-face) " prev   "
     (propertize "M-n/M-p" 'face key-face) " walk commits   "
     (propertize "|" 'face key-face) " prev side   "
     (propertize "s" 'face key-face) " walk A or B   "
     (propertize "q" 'face key-face) " quit   "
     (propertize "?" 'face key-face)
     (propertize " expand menu" 'face hint-face)
     "\n")))

;; `defconst' rather than `defvar' so every reload re-evaluates
;; the table -- otherwise editing a key/label here and running
;; `shipit-reload' silently keeps the old version of the legend
;; (a `defvar' with an existing binding is a no-op on load).
(defconst shipit-review--actions-legend-sections
  '(("Navigate"
     ("n"   "next hunk")
     ("p"   "prev hunk")
     ("M-n" "newer commit")
     ("M-p" "older commit")
     ("|"   "toggle A/B split")
     ("s"   "walk A or B"))
    ("View"
     ("v"   "page down")
     ("V"   "page up")
     ("C-n" "next line")
     ("C-p" "prev line")
     ("C-l" "recenter"))
    ("Toggle"
     ("o"   "diff overlays")
     ("i"   "inline comments")
     ("m"   "commit message")
     ("b"   "blame")
     ("t"   "timeline"))
    ("Other"
     ("e"   "ediff A vs B")
     ("r"   "review menu")
     ("?"   "compact")
     ("q"   "quit"))
    ("Comments"
     ("c n" "next comment")
     ("c p" "prev comment")
     ("c a" "add")
     ("c e" "edit")
     ("c r" "reply")
     ("c R" "react")
     ("c s" "suggest edits")))
  "Sections shown in the full legend.
Each entry is (HEADING (KEY DESC) ...).  Adding a new
action means appending one (KEY DESC) pair to the relevant
section -- the renderer recomputes column widths so layout
stays aligned.  Add a whole new section by appending another
top-level (HEADING ...) entry.")

(defvar shipit-review-actions-legend-extra-sections nil
  "Sections other packages add to the full legend, after its own.
Each is (HEADING (KEY DESC) ...), as in
`shipit-review--actions-legend-sections'.  A `defvar', unlike that
table, so a reload of this file keeps what the packages added.")

(defun shipit-review--actions-legend-all-sections ()
  "The sections of the full legend: its own, then those added to it."
  (append shipit-review--actions-legend-sections
          shipit-review-actions-legend-extra-sections))

(defun shipit-review--legend-window-lines (text-pixels chrome-pixels line-pixels)
  "The height in lines of a window that shows TEXT-PIXELS of text.
CHROME-PIXELS is what the window spends on its mode line, and
LINE-PIXELS is the height of one line of the frame.  A mode line can
be taller than a line of text, so it cannot be counted as one line: a
window sized that way has no room left for its text."
  (ceiling (+ text-pixels chrome-pixels) (float line-pixels)))

(defun shipit-review--fit-legend-window (window)
  "Give WINDOW, which shows the legend, the height of its text and mode line."
  (let* ((chrome (- (window-pixel-height window) (window-body-height window t)))
         (text (cdr (window-text-pixel-size window nil t)))
         (lines (shipit-review--legend-window-lines
                 text chrome (frame-char-height (window-frame window))))
         (window-min-height 1))
    ;; A side window keeps the height it was made with unless its size
    ;; lock is cleared, and it snaps only to an exact minimum and maximum.
    (with-selected-window window
      (setq window-size-fixed nil)
      (fit-window-to-buffer window lines lines))))

(defun shipit-review--actions-legend-section-width (section)
  "Return the rendered column width (in chars) for SECTION.
Width = max-key + 2 (space + dash) + max-desc.  Used by the
header padding and `===' separator so each column is exactly
its content's width."
  (let* ((entries (cdr section))
         (key-w (if entries
                    (apply #'max (mapcar (lambda (e) (length (car e))) entries))
                  0))
         (desc-w (if entries
                     (apply #'max (mapcar (lambda (e) (length (cadr e))) entries))
                   0)))
    (+ key-w 2 desc-w)))

(defun shipit-review--actions-legend-format-section (section key-face)
  "Format SECTION (HEADING (KEY DESC)...) into a list of row strings.
The list starts with two header rows (centred heading +
`===' underline) followed by one row per entry in the ediff
`KEY -DESC' style with KEY right-padded to the widest key
in the section.  Trailing blanks are NOT appended here --
that\='s left to the caller which knows the global section
height and pads with blank strings of the same width so
column zipping stays trivial."
  (let* ((heading (car section))
         (entries (cdr section))
         (key-w (if entries
                    (apply #'max (mapcar (lambda (e) (length (car e))) entries))
                  0))
         (desc-w (if entries
                     (apply #'max (mapcar (lambda (e) (length (cadr e))) entries))
                   0))
         (col-w (+ key-w 2 desc-w))
         ;; Centre the heading inside col-w.  Floor of the left pad so
         ;; an odd remainder leans right (matches ediff visually).
         (heading-pad-l (max 0 (/ (- col-w (length heading)) 2)))
         (heading-pad-r (max 0 (- col-w (length heading) heading-pad-l)))
         (header-line
          (concat (make-string heading-pad-l ?\s)
                  (propertize heading 'face
                              '(:weight bold
                                        :inherit
                                        shipit-comment-indicator-face))
                  (make-string heading-pad-r ?\s)))
         (rule-line
          (propertize (make-string col-w ?=) 'face 'shadow)))
    (cons header-line
          (cons rule-line
                (mapcar
                 (lambda (e)
                   (let* ((k (car e))
                          (d (cadr e))
                          (key-pad (make-string (- key-w (length k)) ?\s))
                          (desc-pad (make-string (max 0 (- desc-w (length d))) ?\s)))
                     (concat key-pad
                             (propertize k 'face key-face)
                             " -"
                             d
                             desc-pad)))
                 entries)))))

(defun shipit-review--actions-legend-full ()
  "Build the full multi-line legend from `--actions-legend-sections'.
Renders ediff-style: each column has a centred heading, an
`=' rule, and right-aligned `KEY -DESC' rows.  Columns are
separated by a faint `|' divider, mirroring ediff\='s control
panel layout.  Layout is data-driven from the sections
variable so adding actions or columns doesn't require
touching the renderer."
  (let* ((key-face '(:weight bold :inherit font-lock-keyword-face))
         (sections (shipit-review--actions-legend-all-sections))
         (col-widths (mapcar #'shipit-review--actions-legend-section-width
                             sections))
         (formatted (mapcar (lambda (s)
                              (shipit-review--actions-legend-format-section
                               s key-face))
                            sections))
         (max-rows (apply #'max (mapcar #'length formatted)))
         ;; Use a Unicode `│' (U+2502) for the column divider so the
         ;; ASCII `|' bound to `previous change' isn't visually
         ;; indistinguishable from the column-separator glyph.
         (separator (propertize " │ " 'face 'shadow)))
    ;; Pad shorter sections with blank strings of the same width so
    ;; columns can be zipped without per-section length checks.
    (setq formatted
          (cl-mapcar
           (lambda (col w)
             (let ((need (- max-rows (length col)))
                   (blank (make-string w ?\s)))
               (append col (make-list need blank))))
           formatted col-widths))
    (concat
     " "
     (mapconcat
      (lambda (i)
        (mapconcat (lambda (col) (nth i col)) formatted separator))
      (number-sequence 0 (1- max-rows))
      "\n ")
     "\n")))

(defun shipit-review--actions-legend-content ()
  "Build the formatted legend string for the current expand state.
Minimal (one row + inline `? expand' hint) by default; full
grouped layout when `shipit-review--actions-legend-expanded'
is non-nil.  Toggled by `?'."
  (if shipit-review--actions-legend-expanded
      (shipit-review--actions-legend-full)
    (shipit-review--actions-legend-minimal)))

(defun shipit-review--render-actions-legend ()
  "Re-render the legend buffer\\='s content from current expand state.
Adjusts the side-window\\='s height to fit the new content
so the minimal one-line view doesn\\='t leave a tall empty
panel and the expanded view doesn\\='t scroll off the top."
  (let ((buf (get-buffer shipit-review--actions-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (shipit-review--actions-legend-content)))
        (set-buffer-modified-p nil)
        ;; After insert, point sits at end-of-buffer -- if the window
        ;; is shorter than the content the heading + rule rows at
        ;; the top get scrolled out of view.  Reset point and the
        ;; window-start below so the heading is always visible.
        (goto-char (point-min)))
      (let ((win (get-buffer-window buf t)))
        (when (window-live-p win)
          (shipit-review--fit-legend-window win)
          (set-window-point win (point-min))
          (set-window-start win (point-min)))))))

(defun shipit-review--actions-legend-toggle-expand ()
  "Toggle the legend between the minimal and expanded views.
Bound to `?' on the legend buffer.  Side windows resist
`window-resize' and `fit-window-to-buffer', so re-rendering
in place left the minimal layout sitting inside a 9-line
window.  Instead, delete the existing window and let
`--show-actions-legend' re-display it with the new
`window-height' alist parameter -- side windows honour
that on creation."
  (interactive)
  (setq shipit-review--actions-legend-expanded
        (not shipit-review--actions-legend-expanded))
  (let ((win (get-buffer-window shipit-review--actions-buffer-name t)))
    (when (window-live-p win)
      (delete-window win)))
  (shipit-review--show-actions-legend))

;; Compatibility alias: an earlier iteration of this work named the
;; visibility toggle `shipit-review-toggle-actions-legend' and left
;; that symbol embedded in the existing keymap object.  `defvar'
;; doesn\\='t reset a bound keymap, so a partially-reloaded session
;; otherwise raises `Wrong type argument: commandp,
;; shipit-review-toggle-actions-legend' when the user hits `?'.
;; Alias keeps stale bindings working until the next clean reload.
(defalias 'shipit-review-toggle-actions-legend
  'shipit-review--actions-legend-toggle-expand)

(defun shipit-review--actions-legend-buffer ()
  "Return the legend buffer, creating + always refreshing it.
Re-inserts the formatted content on every call (cheap --
just a buffer erase + string concat) so reloads / source
edits to the legend builders show up immediately without
the user having to kill the legend buffer manually.  The
buffer name and major mode are preserved across refreshes."
  (let ((buf (or (get-buffer shipit-review--actions-buffer-name)
                 (get-buffer-create
                  shipit-review--actions-buffer-name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'shipit-review-actions-mode)
        (shipit-review-actions-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shipit-review--actions-legend-content)))
      (set-buffer-modified-p nil)
      ;; Reset point so the subsequent `display-buffer' starts the
      ;; window at the top of the legend, not at end-of-buffer
      ;; where `insert' just left it -- without this the heading +
      ;; `===' rule scroll out of view on first display.
      (goto-char (point-min)))
    buf))

(defun shipit-review-refresh-legend ()
  "Force-rebuild the diff-actions legend buffer.
Kills any cached `*shipit diff actions*' buffer and re-pops
it so freshly-loaded `--actions-legend-content' /
`--actions-legend-minimal' / `--actions-legend-full' code
is used.  Useful after a `shipit-reload' when the legend
window\='s text appears stale even though the function
cells were refreshed."
  (interactive)
  (when-let ((buf (get-buffer shipit-review--actions-buffer-name)))
    (let ((win (get-buffer-window buf t)))
      (when (window-live-p win)
        (delete-window win)))
    (kill-buffer buf))
  (shipit-review--show-actions-legend))

(defvar shipit-review--saved-window-config nil
  "Window configuration captured at review-session start.
Set when the first review-mode buffer\\='s legend pops.
Consumed + cleared by `shipit-review-quit', which uses
ediff-style teardown to restore the user\\='s pre-review
layout.")

;; Windows on the bottom side of a frame share one height, side by
;; side.  A transient menu opens on that side, so while the legend
;; shows, a menu stood beside it, as low as the legend, and showed 2
;; of its 16 lines.  For the time of a review session, a menu opens
;; above the legend instead, below the other windows.

(defun shipit-review--menu-beside-legend-p (buffer-name _action)
  "Non-nil when BUFFER-NAME is the menu of transient, and the legend shows."
  (and (boundp 'transient--buffer-name)
       (equal buffer-name transient--buffer-name)
       (get-buffer-window shipit-review--actions-buffer-name)))

(defconst shipit-review--menu-above-legend
  '(shipit-review--menu-beside-legend-p (display-buffer-at-bottom))
  "The entry of `display-buffer-alist' that opens a menu above the legend.")

(defun shipit-review--show-actions-legend ()
  "Show the diff-actions legend in a small bottom window and focus it.
Idempotent for the window pop (re-displaying when already
visible is a no-op) but always selects the legend window so
the user can press single-key bindings without an explicit
`C-x o' first.  Ediff-style: the control panel grabs focus
on entry and after teardown.

The first call also saves the current window configuration
into `shipit-review--saved-window-config' so
`shipit-review-quit' can restore it on teardown.  Subsequent
calls don\\='t overwrite the saved config -- it should
represent the layout BEFORE the user entered review mode,
not the layout after the H side window opened."
  (interactive)
  (let ((buf (shipit-review--actions-legend-buffer)))
    (unless (get-buffer-window buf t)
      (unless shipit-review--saved-window-config
        (setq shipit-review--saved-window-config
              (current-window-configuration)))
      (add-to-list 'display-buffer-alist shipit-review--menu-above-legend)
      (display-buffer
       buf
       '((display-buffer-in-side-window)
         (side . bottom)
         (slot . 1)
         (window-height . shipit-review--fit-legend-window)
         ;; Don't preserve-size: it blocks the subsequent
         ;; window-resize that fires when the user toggles
         ;; between the minimal and full legend with `?', leaving
         ;; the expanded layout's heading + rule rows scrolled
         ;; off the top.
         (window-parameters
          (no-other-window . nil)
          (no-delete-other-windows . t)))))
    (let ((win (get-buffer-window buf t)))
      (when (window-live-p win)
        ;; Snap the window to point-min so the heading + `===' rule
        ;; are visible on first display.  `display-buffer' may leave
        ;; window-start wherever point happened to be in the buffer,
        ;; which after `insert' is end-of-buffer.
        (set-window-start win (point-min))
        (set-window-point win (point-min))
        (select-window win)))))

(defun shipit-review-quit ()
  "Tear down the review session ediff-style and restore the layout.

Closes all review-mode temp display state and disables
`shipit-review-mode' in every buffer where it\\='s active:

  - kills H side buffers (`--clear-history-overlay'
    per source buffer, plus a sweep for stale ones);
  - disables `shipit-review-mode' in every review buffer
    so overlays / blame state / etc. are torn down;
  - kills the actions legend buffer;
  - restores the window configuration captured when the
    legend first popped (`--saved-window-config').

Bound to `q' on the legend buffer.  The user\\='s worktree
file buffers themselves stay alive -- shipit\\='s decorations
come off, but the actual files don\\='t close (mirror of
ediff\\='s behaviour: ediff kills its scratch diff buffers,
not the original participant files).

Prompts for confirmation before tearing down so a stray
`q' press in the legend doesn\='t lose state -- mirrors
ediff\='s own `q'-with-confirm convention."
  (interactive)
  (when (y-or-n-p "Quit the review session? ")
    (shipit-review--do-quit)))

(defun shipit-review--do-quit ()
  "Tear down the review session and restore the pre-review layout.
Internal helper invoked by `shipit-review-quit' after the
user has confirmed.

Now also kills the worktree file buffers that had
`shipit-review-mode' active -- previously they stayed
open after `q' and the user had to `C-x k' them
manually, which sometimes tripped eglot teardown errors
(`Wrong type argument: number-or-marker-p, nil') because
shipit had already torn down some state eglot\\='s kill
hooks expected.  Killing here unblocks the file buffer
cleanly while shipit\\='s own state is still consistent."
  ;; Collect file buffers first, then disable mode + tear down.
  (let ((review-buffers
         (cl-loop for buf in (buffer-list)
                  when (buffer-local-value 'shipit-review-mode buf)
                  collect buf)))
    ;; Disable review mode in every review buffer.  This removes
    ;; overlays, blame state, H side buffer, etc. via the mode\='s
    ;; own teardown.
    (dolist (buf review-buffers)
      (with-current-buffer buf
        (when (fboundp 'shipit-review--clear-history-overlay)
          (shipit-review--clear-history-overlay))
        (shipit-review-mode -1)))
    ;; Sweep any orphaned history side buffers + file-history
    ;; (timeline) buffers left behind.  Both are review-session
    ;; transients -- keeping them around past `q' clutters the
    ;; buffer list and they\\='ll be wrong against the next session
    ;; anyway since they bind to a specific file + range.
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (and name
                   (or (string-prefix-p "*shipit-review previous: " name)
                       (string-prefix-p "*shipit-file-history: " name)))
          (kill-buffer buf))))
    ;; Kill the legend.
    (when-let ((buf (get-buffer shipit-review--actions-buffer-name)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Kill the review-mode file buffers themselves.  Wrap each
    ;; kill in `condition-case' so a misbehaving kill-buffer-hook
    ;; (eglot, lsp-mode, etc.) on one buffer can\='t abort the
    ;; whole teardown.  The remaining buffers + window restore
    ;; still happen.
    (dolist (buf review-buffers)
      (when (buffer-live-p buf)
        (condition-case err
            (kill-buffer buf)
          (error
           (when (fboundp 'shipit--debug-log)
             (shipit--debug-log
              "QUIT: kill-buffer error on %s: %s"
              (buffer-name buf)
              (error-message-string err)))))))
    (setq display-buffer-alist
          (delete shipit-review--menu-above-legend display-buffer-alist))
    ;; Restore pre-review window layout.
    (when shipit-review--saved-window-config
      (set-window-configuration shipit-review--saved-window-config)
      (setq shipit-review--saved-window-config nil))
    (message "Review session ended.")))

(defun shipit-review--actions-legend-on-mode-entry ()
  "Pop the legend when a `shipit-review-mode' buffer activates.
Captures the source buffer reference so the legend\\='s
commands can route to it via `--with-source-buffer'."
  (when shipit-review--pr-data
    (setq shipit-review--transient-source-buffer (current-buffer)))
  ;; The legend takes focus on purpose, ediff-style, and selecting its
  ;; window changes the current buffer with it.  That is fine for
  ;; someone entering review mode by hand, but this runs from a mode
  ;; hook: anything that switches the mode on and keeps working would
  ;; keep working in the legend.  Restoring the buffer leaves the
  ;; window selected, so the focus behaviour is unchanged.
  (save-current-buffer
    (shipit-review--show-actions-legend)))

(add-hook 'shipit-review-mode-hook
          #'shipit-review--actions-legend-on-mode-entry)

(defun shipit-review--current-diff-range ()
  "Return (FROM-SHA . TO-SHA) for the current comparison/range state.
Falls back to base..head when no specific range is selected."
  (let* ((commits (shipit-review--pr-commits))
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

(defun shipit-review--ediff-with-history-side ()
  "Ediff source at current SHA vs H side buffer at PREV_SHA.
Compares the file the source buffer currently displays
(`shipit-review--displayed-sha', or `--head-sha' if no
single-commit nav happened) on the LEFT against the file
at the H side window's `--side-prev-sha' on the RIGHT.
Requires `H' to be active so the side window's context
vars are populated.

Uses the existing `shipit-review-history-ediff-files-at-
shas' helper which follows renames internally, so the
same rel-path argument works even if the file was renamed
between the two revisions."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      ((not shipit-review--history-shown)
       (user-error
        "A/B split not active -- press `|' to open the prev side first"))
      (t
       (let* ((file (buffer-file-name))
              (root (and file (shipit--get-repo-root)))
              (rel (and file root (file-relative-name file root)))
              (left-sha (or shipit-review--displayed-sha
                            shipit-review--head-sha))
              (side shipit-review--history-side-buffer)
              (right-sha (and side (buffer-live-p side)
                              (buffer-local-value
                               'shipit-review--side-prev-sha side))))
         (cond
          ((not (and rel root left-sha right-sha))
           (user-error
            "Cannot ediff: rel=%s root=%s L=%s R=%s"
            rel root left-sha right-sha))
          (t
           ;; Hide the legend\='s side window before launching
           ;; ediff -- `ediff-setup' calls `delete-other-windows'
           ;; which refuses when the only remaining window would
           ;; be a side window, producing
           ;;   delete-other-windows: Cannot make side window the only window
           ;; The legend buffer is kept alive so we can re-pop
           ;; the window after ediff quits.
           (let ((legend-win (get-buffer-window
                              shipit-review--actions-buffer-name t)))
             (when (window-live-p legend-win)
               (delete-window legend-win)))
           ;; Append a one-shot post-quit re-pop so the legend
           ;; comes back when the user exits the ediff session.
           ;; `ediff-quit-hook' is the user-facing hook (the
           ;; helper sets `ediff-after-quit-hook-internal' with
           ;; `setq', so hooking elsewhere is safer).
           ;;
           ;; Ediff\\='s teardown order is:
           ;;   1. `ediff-quit-hook'             (we hook here)
           ;;   2. `ediff-cleanup-hook'
           ;;   3. `ediff-after-quit-hook-internal'
           ;;      (helper restores window-configuration HERE)
           ;;
           ;; If we showed the legend straight from
           ;; `ediff-quit-hook' (step 1), step 3 would restore
           ;; the saved window-config -- captured BEFORE we
           ;; deleted the legend -- and wipe it again.  Defer
           ;; via `run-at-time 0' so the re-pop fires after all
           ;; synchronous hooks have run and the final window
           ;; layout is in place.
           (letrec ((restore-fn
                     (lambda ()
                       (remove-hook 'ediff-quit-hook restore-fn)
                       (run-at-time
                        0 nil
                        #'shipit-review--show-actions-legend))))
             (add-hook 'ediff-quit-hook restore-fn))
           ;; Ediff convention: A=older/base, B=newer/
           ;; modified.  Pass `--side-prev-sha' (older) as A
           ;; and `--displayed-sha'/HEAD (newer) as B so the
           ;; diff reads naturally \"how A became B\" --
           ;; added lines in HEAD highlight on the B side as
           ;; additions, not as A-only removals.  Side-swap
           ;; vs shipit\='s H layout (where source is on the
           ;; left) is the standard ediff convention any
           ;; reviewer is used to.
           (shipit-review-history-ediff-files-at-shas
            right-sha left-sha rel root)))))))))

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

(defun shipit-review--present-content-at-sha (sha)
  "Swap the source buffer's content to show the file at SHA.
When SHA is nil or matches `shipit-review--head-sha', restore
the on-disk worktree file content (the buffer\\='s normal,
editable state).  Otherwise replace the content with `git show
SHA:historical-path' (rename-following via
`shipit-review-history--historical-path') and mark the buffer
read-only so an accidental save can\\='t clobber the worktree.

`shipit-review--showing-historical-sha' tracks the swapped
state so navigation back to HEAD knows to restore the worktree
file rather than re-fetch the same SHA."
  (let ((head-sha shipit-review--head-sha)
        (file (buffer-file-name))
        (root (and (fboundp 'shipit--get-repo-root)
                   (shipit--get-repo-root))))
    (cond
     ((or (null sha) (and head-sha (string= sha head-sha)))
      (when shipit-review--showing-historical-sha
        (shipit-review--restore-worktree-content)))
     ((and file root)
      (shipit-review--swap-to-historical-content sha file root)))
    ;; Keep B's header label in sync with the swapped state --
    ;; `--apply-source-header-line' reads
    ;; `--showing-historical-sha' to decide between `@ SHA' and
    ;; `Head'.
    (shipit-review--apply-source-header-line)))

(defun shipit-review--swap-to-historical-content (sha file root)
  "Replace this buffer's content with `git show SHA:historical-path'.
FILE is the buffer\\='s visited file (used to compute the
worktree-relative path); ROOT is the worktree root.  After
the swap, `buffer-read-only' is set so accidental saves can\\='t
write historical content over the worktree file."
  (let* ((rel (file-relative-name file root))
         (saved-line (line-number-at-pos))
         (saved-col (current-column))
         ;; Capture the on-screen row of point in each window
         ;; showing the buffer, so we can recenter point to the
         ;; same row after the swap.  Without this, the swap
         ;; lands point on the right line but at a different
         ;; vertical position, making walks feel like the file
         ;; is jumping up / down in the window.
         (saved-rows
          (mapcar (lambda (win)
                    (cons win
                          (with-selected-window win
                            (max 0 (count-screen-lines
                                    (window-start) (point))))))
                  (get-buffer-window-list (current-buffer) nil t)))
         (from-sha (or shipit-review--showing-historical-sha
                       shipit-review--head-sha))
         (target-line
          (cond
           ((or (null from-sha) (string= from-sha sha)) saved-line)
           (t
            (let ((default-directory (file-name-as-directory root)))
              (shipit-review--map-head-to-prev
               saved-line
               (shipit-review--diff-hunks sha from-sha rel))))))
         (inhibit-read-only t)
         (shipit-review--in-content-swap t))
    (shipit-review--clear-edit-overlays)
    (erase-buffer)
    (shipit-review-history-insert-file-at-sha sha rel root)
    (set-buffer-modified-p nil)
    (setq shipit-review--showing-historical-sha sha)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line (1- target-line))
    (move-to-column saved-col)
    (dolist (entry saved-rows)
      (let ((win (car entry)) (row (cdr entry)))
        (when (window-live-p win)
          (set-window-point win (point))
          (with-selected-window win (recenter row)))))))

(defun shipit-review--restore-worktree-content ()
  "Restore the on-disk worktree file content into this buffer.
Re-reads the file from disk, clears
`shipit-review--showing-historical-sha', and re-enables
editing.  Called when the user navigates back to HEAD after
viewing one or more historical revisions."
  (let* ((file (buffer-file-name))
         (root (and file (shipit--get-repo-root)))
         (rel (and file root (file-relative-name file root)))
         (saved-line (line-number-at-pos))
         (saved-col (current-column))
         (saved-rows
          (mapcar (lambda (win)
                    (cons win
                          (with-selected-window win
                            (max 0 (count-screen-lines
                                    (window-start) (point))))))
                  (get-buffer-window-list (current-buffer) nil t)))
         (from-sha shipit-review--showing-historical-sha)
         (to-sha shipit-review--head-sha)
         (target-line
          (cond
           ((or (null from-sha) (null to-sha) (null rel))
            saved-line)
           (t
            (let ((default-directory (file-name-as-directory root)))
              (shipit-review--map-head-to-prev
               saved-line
               (shipit-review--diff-hunks to-sha from-sha rel))))))
         (inhibit-read-only t)
         (shipit-review--in-content-swap t))
    (shipit-review--clear-edit-overlays)
    (when (and file (file-exists-p file))
      (erase-buffer)
      (insert-file-contents file)
      (set-buffer-modified-p nil))
    (setq shipit-review--showing-historical-sha nil)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (forward-line (1- target-line))
    (move-to-column saved-col)
    (dolist (entry saved-rows)
      (let ((win (car entry)) (row (cdr entry)))
        (when (window-live-p win)
          (set-window-point win (point))
          (with-selected-window win (recenter row)))))))

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
               (commits (shipit-review--pr-commits))
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
PR\='s magit buffer.

Each returned comment is tagged with `(is_resolved . t/nil)'
based on the magit buffer\='s
`shipit--cached-resolved-threads' so the overlay renderer
can surface the resolved state in the file view (the magit
view already shows a `[RESOLVED]' suffix; the overlay
rendering had no equivalent before this)."
  (let* ((file (buffer-file-name))
         (root (and file (shipit--get-repo-root)))
         (rel (and file root (file-relative-name file root)))
         (magit-buf
          (cl-find-if
           (lambda (b)
             (buffer-local-value 'shipit--cached-inline-comments b))
           (buffer-list))))
    (when (and magit-buf rel)
      (let* ((effective-sha (or shipit-review--displayed-sha
                                shipit-review--head-sha))
             ;; When the displayed SHA is one of the PR's own
             ;; commits, skip the per-comment ancestor check
             ;; entirely -- comments belong to "the PR" and
             ;; should stay visible while walking through PR
             ;; commits even when the comment's anchor SHA
             ;; (`commit_id' = PR head) is *newer* than the
             ;; commit we're standing on, or when
             ;; `original_commit_id' got force-pushed away.
             (pr-commit-shas
              (let ((h (make-hash-table :test 'equal)))
                (dolist (c (shipit-review--pr-commits))
                  (let ((s (cdr (assq 'sha c))))
                    (when s (puthash s t h))))
                h))
             (displayed-in-pr
              (and effective-sha
                   (gethash effective-sha pr-commit-shas)))
             (worktree (file-name-as-directory
                        (or (shipit--get-repo-root) default-directory)))
             ;; Per-call cache for `git merge-base --is-ancestor'
             ;; lookups -- a typical PR has many comments but only a
             ;; handful of distinct anchor SHAs, so caching saves
             ;; tens of git calls when toggling `i'.
             (anc-cache (make-hash-table :test 'equal))
             (in-range-p
              (lambda (anchor)
                (cond
                 ;; Defensive show: missing data on either side.
                 ((null effective-sha) t)
                 ((null anchor)        t)
                 (t
                  (let* ((key (cons anchor effective-sha))
                         (cached (gethash key anc-cache 'shipit-uncached)))
                    (cond
                     ((not (eq cached 'shipit-uncached)) cached)
                     (t
                      (let* ((default-directory worktree)
                             (rc (process-file
                                  "git" nil nil nil
                                  "merge-base" "--is-ancestor"
                                  anchor effective-sha))
                             (result (zerop rc)))
                        (puthash key result anc-cache)
                        result))))))))
             (filtered
              (cl-remove-if-not
               (lambda (c)
                 (let* ((path (cdr (assq 'path c)))
                        (outdated (cdr (assq 'outdated c)))
                        (commit-id (cdr (assq 'commit_id c)))
                        (orig-id (cdr (assq 'original_commit_id c))))
                   ;; Show the comment when EITHER anchor is
                   ;; reachable from the displayed SHA.  GitHub
                   ;; updates `commit_id' to the current PR head
                   ;; on every push; `original_commit_id' is where
                   ;; the comment was first added.  When the PR
                   ;; gets force-pushed, the original commit may
                   ;; no longer exist locally -- the
                   ;; `merge-base --is-ancestor' check then exits
                   ;; non-zero \("unknown commit\"), which we don\='t
                   ;; want to conflate with \"not in range\".
                   ;; Falling back to `commit_id' (always pointing
                   ;; at a live PR commit) recovers the rebased /
                   ;; force-pushed case.
                   (and path (string= path rel)
                        (not outdated)
                        (or displayed-in-pr
                            (funcall in-range-p orig-id)
                            (funcall in-range-p commit-id)))))
               (buffer-local-value 'shipit--cached-inline-comments
                                   magit-buf))))
        ;; Tag each comment with resolved status looked up in
        ;; the magit buffer\='s resolved-threads cache.  The
        ;; lookup MUST run inside the magit buffer because
        ;; `shipit--cached-resolved-threads' is `defvar-local'.
        (with-current-buffer magit-buf
          (mapcar
           (lambda (c)
             (let* ((id (cdr (assq 'id c)))
                    (resolved (and id
                                   (shipit--is-comment-in-resolved-thread
                                    id))))
               (cons (cons 'is_resolved (and resolved t)) c)))
           filtered))))))

(defun shipit-review--rounded-after-string (lines &optional bg-color)
  "Wrap LINES with rounded SVG edges + filled background.
LINES is a list of strings without trailing newlines.  BG-COLOR
defaults to `shipit-rounded-section-bg-color\\=' so existing
inline-comment callers keep their look; another kind of overlay
can pass its own color.

Mirrors the magit-section rounded-pill treatment from
`shipit-rounded--apply-bg\\=' but for the `after-string\\=' overlay
path used by file-view inline comments.  Magit\\='s implementation
walks buffer text and applies `line-prefix\\=' / overlays per line;
an after-string isn\\='t in the buffer, so we instead build the SVG
edges straight into the after-string content.

The bg face is applied to the line text AND its trailing newline
char so `:extend t\\=' fills the rest of each visual row out to the
window edge (the same trick the PR-buffer pill uses by including
the line\\='s `\\n\\=' in its overlay range).  Without that the pill
stops at the text width and leaves an empty corridor on the right.

Falls back to a plain newline-joined string in non-graphic
displays where SVGs can\\='t render."
  (cond
   ((not (display-graphic-p))
    (mapconcat #'identity lines "\n"))
   (t
    (let* ((bg-color (or bg-color shipit-rounded-section-bg-color))
           (char-height (frame-char-height))
           (radius 10)
           (count (length lines))
           (last-idx (1- count))
           (parts '()))
      (cl-loop for line in lines
               for idx from 0
               do
               (let* ((position (cond
                                 ((and (= idx 0) (= idx last-idx)) 'single)
                                 ((= idx 0) 'top)
                                 ((= idx last-idx) 'bottom)
                                 (t 'middle)))
                      (edge (shipit-rounded--make-edge-svg
                             char-height radius bg-color position 'left))
                      (edge-prefix (propertize " " 'display edge))
                      (line-bg (concat (copy-sequence line) "\n")))
                 (add-face-text-property 0 (length line-bg)
                                         `(:background ,bg-color :extend t)
                                         t line-bg)
                 (push (concat edge-prefix line-bg) parts)))
      (apply #'concat (nreverse parts))))))

(defun shipit-review--render-inline-comment (comment)
  "Build the multi-line `after-string' for an inline COMMENT overlay.
Renders with the same rounded SVG-pill background the magit
PR-buffer view uses (via `shipit-rounded--apply-bg'),
adapted for the after-string path.  Falls back to plain
text on non-graphic displays.

When COMMENT carries `(is_resolved . t)' (set by
`--inline-comments-for-file' from the magit buffer\='s
resolved-threads cache), append a `<svg-check> resolved'
chip after the timestamp and dim the body via `shadow'
face appended to the existing body face."
  (let* ((user (cdr (assq 'login (cdr (assq 'user comment)))))
         (timestamp (cdr (assq 'created_at comment)))
         (resolved (cdr (assq 'is_resolved comment)))
         (resolved-chip
          (when resolved
            (concat " "
                    (if (fboundp 'shipit--get-pr-field-icon)
                        (shipit--get-pr-field-icon "resolved" "✓")
                      "✓")
                    (propertize " resolved"
                                'face '(:inherit shadow
                                                 :weight bold)))))
         (header
          (concat
           "  💬 "
           (propertize (or user "?")
                       'face 'shipit-username-face)
           (when (and timestamp (stringp timestamp))
             (concat (propertize " • " 'face 'shadow)
                     (propertize timestamp
                                 'face 'shipit-timestamp-face)))
           resolved-chip))
         ;; Body rendering is delegated to `shipit--render-comment-body'
         ;; (the same code path the PR-buffer view uses).  That handles
         ;; the CR/CRLF stripping that was leaking `^M' into overlays,
         ;; the line wrapping at `shipit-render-wrap-column', markdown
         ;; rendering, and indentation in one place -- previously this
         ;; function reinvented a less capable subset (raw split, no
         ;; wrap, no clean).
         (rendered-body (shipit--render-comment-body comment 5))
         (body-line-strings
          (mapcar (lambda (l)
                    (let ((s (copy-sequence l)))
                      (add-face-text-property 0 (length s)
                                              'shipit-inline-comment-body-face
                                              t s)
                      (when resolved
                        (add-face-text-property 0 (length s)
                                                'shadow t s))
                      s))
                  (split-string rendered-body "\n")))
         (all-lines (cons header body-line-strings))
         (rendered (shipit-review--rounded-after-string all-lines)))
    (concat "\n" rendered "\n")))

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

(defun shipit-review--refresh-inline-comments-if-shown ()
  "Re-render inline-comment overlays if they were visible.
No-op when the user has them hidden.  Called after navigation
that changes `--displayed-sha' so the per-commit filter in
`--inline-comments-for-file' re-applies."
  (when shipit-review--inline-comment-overlays
    (shipit-review--hide-inline-comments)
    (shipit-review--show-inline-comments)))

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

The keys are in the legend window of the review session, where `r'
opens the review menu and `t' the timeline of the file.  The mode
binds no key in the buffer of the file."
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
  (let ((commits (shipit-review--pr-commits))
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
      (let* ((commits (shipit-review--pr-commits))
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
    ;; Track which PR commit we\\='re viewing in `--displayed-sha'
    ;; too, not just `--comparison-target' / `--current-commit-
    ;; index'.  The inline-comment filter and the timeline
    ;; highlight both key off `--displayed-sha', so without this
    ;; M-n / M-p on B-side PR commits would leave them stuck at
    ;; the PR-HEAD anchor.
    (setq-local shipit-review--displayed-sha sha)
    (shipit-review--present-content-at-sha sha)
    (shipit-review--set-comparison-target sha (format "commit %d" (1+ index))))
  ;; Refresh inline-comment overlays (if visible) so the per-
  ;; commit `commit_id' / `original_commit_id' filter re-applies.
  (shipit-review--refresh-inline-comments-if-shown))

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
              (dolist (c (shipit-review--pr-commits))
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
         (pr-commits (shipit-review--pr-commits))
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
      (shipit-review--present-content-at-sha sha)
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
      (shipit-review--refresh-inline-comments-if-shown)
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
    (let* ((commits (shipit-review--pr-commits))
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
    (let* ((commits (shipit-review--pr-commits))
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

(defun shipit-review--pr-commits ()
  "Return `pr-data.commits' as a list, or nil when unavailable.
Coerces the REST API\\='s integer count into the actual SHA list
via `shipit-review--ensure-commits-list\\=' on the way out, and
flattens a vector value into a list so every navigation /
overlay site sees a consistent shape.  Returns nil cleanly when
the count couldn\\='t be resolved (typically because the base SHA
is unknown) -- callers then see \"no commits\" instead of crashing
with `Wrong type argument: listp, N\\='."
  (shipit-review--ensure-commits-list)
  (let ((raw (cdr (assq 'commits shipit-review--pr-data))))
    (cond
     ((listp raw) raw)
     ((vectorp raw) (append raw nil))
     (t nil))))

(defvar-local shipit-review--edit-line-overlays nil
  "Overlays marking reviewer-edited lines in this review buffer.
Populated by `shipit-review--mark-edited-line' from
`after-change-functions'; cleared on save and on the
content-swap revert path.  Non-nil means there are pending
edits the user can turn into a `c s' suggestion comment.")

(defun shipit-review--mark-edited-line (beg end _len)
  "After-change hook: overlay every line touched by the change.
Skipped while showing historical content (the buffer is
read-only there) so the swap itself isn't mistaken for an
edit.  When the change brings the buffer back to its
on-disk state (e.g.  full undo), every edit overlay is
dropped -- without that, undoing wouldn't clear the
highlight.  Partial undos (some lines reverted, others
not) still leave overlays in place; line-level diff
tracking would be needed to fully reconcile that, and it
hasn't bitten enough yet to justify the cost."
  (unless (or shipit-review--showing-historical-sha
              shipit-review--in-content-swap)
    (cond
     ((not (buffer-modified-p))
      (shipit-review--clear-edit-overlays))
     (t
      (save-excursion
        (goto-char beg)
        (let ((line-beg (line-beginning-position))
              (stop-line (progn (goto-char end) (line-number-at-pos))))
          (goto-char line-beg)
          (while (<= (line-number-at-pos) stop-line)
            (let* ((lb (line-beginning-position))
                   (le (line-beginning-position 2))
                   (existing (cl-find-if
                              (lambda (ov)
                                (and (= (overlay-start ov) lb)
                                     (overlay-get ov 'shipit-review-edit-line)))
                              (overlays-at lb))))
              (unless existing
                (let ((ov (make-overlay lb le nil t nil)))
                  (overlay-put ov 'face 'shipit-review-edit-line-face)
                  (overlay-put ov 'shipit-review-edit-line t)
                  (overlay-put ov 'priority 5)
                  (push ov shipit-review--edit-line-overlays))))
            (forward-line 1))))))))

(defun shipit-review--clear-edit-overlays ()
  "Remove every edit-line overlay from this buffer."
  (dolist (ov shipit-review--edit-line-overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq shipit-review--edit-line-overlays nil))

(defun shipit-review--after-revert ()
  "Restore review-mode overlays after a `revert-buffer'.
`revert-buffer' nukes every overlay in the buffer
(`remove-overlays' with no predicate), so the PR-range
diff tints + inline-comment overlays vanish until the
next toggle.  Re-run the diff-overlay update so the
green/red hunks come back, and drop the edit-overlay list
(the buffer now matches disk -- there are no pending
edits) so a stale list can't hand back overlays that no
longer exist."
  (setq shipit-review--edit-line-overlays nil)
  (when (fboundp 'shipit-review--update-diff-overlays)
    (shipit-review--update-diff-overlays))
  (when shipit-review--inline-comment-overlays
    ;; Comment overlays were live before revert; re-render so
    ;; the user doesn't have to press `i' twice after each
    ;; revert.
    (setq shipit-review--inline-comment-overlays nil)
    (shipit-review--show-inline-comments)))

(defun shipit-review--maybe-clear-edits-on-unmodify ()
  "Drop edit-line overlays when the buffer returns to unmodified state.
`after-change-functions' doesn't reliably fire when `undo'
restores the buffer-modified flag (the bookkeeping path
sets the flag directly without running after-change), so
checking inside that hook alone misses the \"undo back to
disk\" case.  Run from `post-command-hook' instead -- it's
a cheap modified-p read on every command and we only do
work when there are stale overlays to clear."
  (when (and shipit-review--edit-line-overlays
             (not shipit-review--in-content-swap)
             (not (buffer-modified-p)))
    (shipit-review--clear-edit-overlays)))

(defun shipit-review--edit-line-numbers ()
  "Return the sorted unique list of edited line numbers."
  (sort (delete-dups
         (mapcar (lambda (ov)
                   (line-number-at-pos (overlay-start ov)))
                 (cl-remove-if-not #'overlay-buffer
                                   shipit-review--edit-line-overlays)))
        #'<))

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

  ;; Ediff-style header line: identifies this buffer as the
  ;; "B" / HEAD side of the review.  The H side buffer\='s
  ;; header (set in `--apply-history-overlay') labels itself
  ;; "A" / Previous SHA so the two read as a pair across
  ;; windows.
  (shipit-review--apply-source-header-line)

  ;; Track reviewer edits so a) the changed lines are visually
  ;; obvious (background highlight via
  ;; `shipit-review-edit-line-face') and b) `c s' can pull the
  ;; modified content into a suggestion comment.  Cleared on save
  ;; -- once the change is on disk it's committed work, no longer
  ;; a pending suggestion.
  (add-hook 'after-change-functions
            #'shipit-review--mark-edited-line nil t)
  (add-hook 'after-save-hook
            #'shipit-review--clear-edit-overlays nil t)
  (add-hook 'post-command-hook
            #'shipit-review--maybe-clear-edits-on-unmodify nil t)
  ;; Tell `revert-buffer' to keep major + minor modes (and
  ;; therefore the buffer-local hooks below) intact, so reverting
  ;; the file doesn\\='t kill the review-mode session.  Combined
  ;; with the `permanent-local' flags on the PR-context vars
  ;; (`--pr-data', `--head-sha', ...) this means a revert leaves
  ;; the buffer indistinguishable from before the revert except
  ;; for the wiped overlays -- which `--after-revert' restores.
  ;; The variable is Emacs 28+; older versions just skip this
  ;; (the permanent-local flags alone keep nav from blowing up).
  (when (boundp 'revert-buffer-preserve-modes)
    (setq-local revert-buffer-preserve-modes t))
  (add-hook 'after-revert-hook
            #'shipit-review--after-revert nil t)

  (when (bound-and-true-p diff-hl-mode)
    (shipit--debug-log "Review mode setup complete - diff-hl is active")))

(defun shipit-review--apply-source-header-line ()
  "Set the ediff-style header line on the source review buffer.
Renders an `▶' marker + bright `shipit-comment-indicator-
face' when this buffer is the M-n/M-p target (the default,
or after `s' swap when H is off), and dims to `shadow' face
when the H side buffer holds focus.  The active-side
indicator is the user-facing way to tell which buffer
M-n/M-p will affect without having to remember state.

When the content has been swapped to a historical SHA via
`shipit-review--present-content-at-sha', the title shows that
SHA (e.g. `@ f02312b6') instead of the fixed `Head' label so
B\\='s header tracks the actual revision the buffer is
showing.

After the title comes the folder of the worktree that holds the
file, so it is clear which worktree is reviewed."
  (let* ((pr-number (and shipit-review--pr-data
                         (cdr (assq 'number shipit-review--pr-data))))
         (file-name (or (and buffer-file-name
                             (file-name-nondirectory buffer-file-name))
                        (buffer-name)))
         (active-p (eq shipit-review--active-history-side 'source))
         (marker-face (if active-p
                          '(:weight bold
                                    :inherit
                                    shipit-comment-indicator-face)
                        'shadow))
         (title-face (if active-p
                         '(:weight bold
                                   :inherit
                                   shipit-comment-indicator-face)
                       'shadow))
         (marker (if active-p "▶" " "))
         (sha shipit-review--showing-historical-sha)
         (label (if sha
                    (format "@ %s" (substring sha 0 (min 7 (length sha))))
                  "Head"))
         (worktree (shipit-review--worktree-folder)))
    (setq-local header-line-format
                (list
                 " "
                 (propertize marker 'face marker-face)
                 (propertize " B: " 'face '(:weight bold))
                 (propertize "│ " 'face 'shadow)
                 (propertize
                  (format "*PR#%s %s: %s*"
                          (or pr-number "?") label file-name)
                  'face title-face)
                 (if worktree
                     (propertize (format "  in %s" worktree) 'face title-face)
                   "")
                 "    "
                 (propertize "Type ? for help"
                             'face 'shadow)))))

(defun shipit-review--worktree-folder ()
  "The folder of the worktree that holds the file of this buffer, or nil."
  (when-let* ((root (and buffer-file-name
                         (locate-dominating-file buffer-file-name ".git"))))
    (shipit--worktree-folder root)))

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

(defun shipit-review--mirror-diff-overlays-to-side (hidden)
  "Mirror the source\\='s diff-overlay HIDDEN state into the H side buffer.
Re-uses the side\\='s cached `shipit-review--changed-lines'
(populated when the side buffer was built in
`--apply-history-overlay') so we don\\='t need to re-run git
diff -- just toggle the overlays on / off and update the
local flag so a later rebuild stays consistent."
  (let ((side shipit-review--history-side-buffer))
    (when (and side (buffer-live-p side))
      (with-current-buffer side
        (setq-local shipit-review--diff-overlays-hidden hidden)
        (remove-overlays (point-min) (point-max)
                         'shipit-review-change t)
        (unless hidden
          (shipit-review--render-overlays-from-changed-lines
           shipit-review--changed-lines))))))

(defun shipit-review-toggle-diff-overlays ()
  "Toggle the PR-range diff overlays in this buffer.
Independent of the other overlay toggles -- compose freely
with inline comments, blame, etc.  `shipit-review--changed-
lines' is preserved when hidden so hunk navigation continues
to work.

When the previous-change side window (`H') is active,
mirrors the toggle into the side buffer so its parent..sha
diff overlays follow the same show/hide state -- both
buffers always agree on whether the line tints are
visible."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      (shipit-review--diff-overlays-hidden
       (setq-local shipit-review--diff-overlays-hidden nil)
       (shipit-review--render-overlays-from-changed-lines
        shipit-review--changed-lines)
       (when shipit-review--history-shown
         (shipit-review--mirror-diff-overlays-to-side nil))
       (message "Diff overlays: shown%s"
                (if shipit-review--history-shown
                    " (source + side window)" "")))
      (t
       (setq-local shipit-review--diff-overlays-hidden t)
       (remove-overlays (point-min) (point-max) 'shipit-review-change t)
       (when shipit-review--history-shown
         (shipit-review--mirror-diff-overlays-to-side t))
       (message "Diff overlays: hidden%s"
                (if shipit-review--history-shown
                    " (source + side window)" "")))))))

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

(defun shipit-review--render-history-base-fallback (rel range base-sha file-mode src-pr-number source-line)
  "Render the H side buffer at BASE-SHA when no prior change touches RANGE.
Used by `--apply-history-overlay' as the fallback when the
current hunk has no prior commit affecting its lines (so
walking history is degenerate).  Instead of leaving the
side buffer empty with just a placeholder line, render the
file as it existed at BASE-SHA (the PR base) -- gives the
reviewer the pre-PR context for the surrounding code,
which is usually what they want to compare against.

No diff overlays are applied (parent..base shows whatever
shaped the file before the PR, which is unrelated to the
current hunk).

FILE-MODE, SRC-PR-NUMBER and SOURCE-LINE are passed
through so the side buffer can syntax-highlight properly,
label its header, and scroll to the same vertical position
as source."
  (let* ((root (or (shipit--get-repo-root) default-directory))
         (path-at-base
          (or (and base-sha
                   (shipit-review-history--historical-path
                    base-sha rel root))
              rel))
         (buf-name (shipit-review--history-side-buffer-name rel))
         (existing (get-buffer buf-name))
         (buf (get-buffer-create buf-name))
         (base-short (if base-sha
                         (substring base-sha 0 (min 7 (length base-sha)))
                       "BASE")))
    (with-current-buffer buf
      (setq-local default-directory (file-name-as-directory root))
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Drop overlays from the previous hunk\\='s render so the
        ;; fresh BASE content isn\\='t shadowed by stale tints /
        ;; commit-message blocks.
        (remove-overlays (point-min) (point-max)
                         'shipit-review-change t)
        (remove-overlays (point-min) (point-max)
                         'shipit-review-commit-message t)
        (setq-local shipit-review--changed-lines nil)
        (cond
         (base-sha
          (shipit-review-history-insert-file-at-sha
           base-sha path-at-base root))
         (t
          (insert (propertize
                   (format
                    "\n  (No BASE SHA available -- can\\='t show pre-PR state)\n")
                   'font-lock-face 'shadow))))
        (when (and file-mode (fboundp file-mode)
                   (not (eq major-mode file-mode)))
          (funcall file-mode)))
      (setq-local header-line-format
                  (let* ((fname (file-name-nondirectory rel)))
                    (list
                     " "
                     (propertize "  " 'face 'shadow)
                     (propertize " A: " 'face '(:weight bold))
                     (propertize "│ " 'face 'shadow)
                     (propertize
                      (format "*PR#%s Pre-PR %s: %s*"
                              (or src-pr-number "?")
                              base-short fname)
                      'face '(:weight bold
                                      :inherit
                                      shipit-comment-indicator-face))
                     "    "
                     (propertize
                      (format
                       "(lines %d-%d new in HEAD -- no prior touch)"
                       (car range) (cdr range))
                      'face 'shadow))))
      (setq-local shipit-review--side-prev-sha base-sha)
      (setq-local shipit-review--side-rel-path path-at-base)
      (setq-local shipit-review--side-worktree root)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      ;; Scroll to source\\='s line so the windows track each other.
      (goto-char (point-min))
      (forward-line (1- (max 1 (or source-line 1)))))
    (setq-local shipit-review--history-side-buffer buf)
    (unless existing
      (let ((source-win (get-buffer-window (current-buffer) t)))
        (display-buffer
         buf
         `((display-buffer-reuse-window
            display-buffer-in-direction)
           (direction . left)
           ,@(when (window-live-p source-win)
               `((window . ,source-win)))
           (window-width . 0.5)))))
    (when-let ((win (get-buffer-window buf t)))
      (set-window-point win (with-current-buffer buf (point)))
      (with-selected-window win (recenter)))))

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
  "List of plists for ALL commits that touched REL-PATH.
Newest-first, walking from `shipit-review--head-sha' /
HEAD backwards.  RANGE is accepted for signature symmetry
with `--previous-change-info' / future extensions but
currently unused -- the chain itself is FILE-level
(`git log --follow' on REL-PATH, no `-L' restriction).

A previous line-restricted version made M-n / M-p on the
A side run out almost immediately for hunks with sparse
history (the `I am stuck at the oldest commit (outside
PR commits)' report).  File-level lets the user walk the
file''s full pre-PR history while initial H placement
stays hunk-aware: `--previous-change-info' still finds
the line-touching commit, and the caller positions the
chain index there via `cl-position'.

Same plist shape as `--previous-change-info'
(`:sha :short :author :time :ago :subject')."
  (ignore range)
  (let* ((head (or shipit-review--head-sha "HEAD"))
         (output
          (with-temp-buffer
            (when (zerop (process-file
                          "git" nil (list t nil) nil
                          "log" "--follow"
                          "--format=%H%x09%an%x09%at%x09%s"
                          head "--" rel-path))
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

(defun shipit-review--initial-history-index (chain)
  "Pick the initial CHAIN index for `--apply-history-overlay'.
`H' should land on the most-recent commit that\='s NOT one
of the PR\='s own intermediate commits -- i.e. the first
commit at or before BASE that touched these lines.  M-n
then walks forward into the PR\='s commits, M-p walks
further back into pre-PR history.

Returns the index of the first chain entry whose SHA is
NOT reachable in the BASE..HEAD range.  Falls back to 0
when BASE isn\='t known (e.g. no PR data) -- the chain\='s
newest entry is then the natural starting point."
  (cond
   ((null chain) nil)
   ((not shipit-review--base-sha) 0)
   (t
    (let* ((base shipit-review--base-sha)
           (head (or shipit-review--head-sha "HEAD"))
           (pr-shas
            (with-temp-buffer
              (when (zerop (process-file
                            "git" nil (list t nil) nil
                            "rev-list"
                            (format "%s..%s" base head)))
                (split-string (buffer-string) "\n" t))))
           (pr-set (let ((tbl (make-hash-table :test 'equal)))
                     (dolist (s pr-shas) (puthash s t tbl))
                     tbl))
           (idx 0)
           (found nil))
      (while (and (< idx (length chain)) (not found))
        (let ((sha (plist-get (nth idx chain) :sha)))
          (cond
           ((gethash sha pr-set)
            ;; PR-internal commit -- keep walking forward in the chain.
            (setq idx (1+ idx)))
           (t
            (setq found t)))))
      (cond
       (found idx)
       ;; All chain entries are PR commits (new file in this PR, or
       ;; only PR commits touched these lines).  Show chain[1] --
       ;; the change immediately before what source displays at
       ;; chain[0] -- so the side buffer is meaningfully different
       ;; from source instead of duplicating it.
       ((>= (length chain) 2) 1)
       ;; Single-entry chain (only one commit ever touched these
       ;; lines): no \"previous change\" exists.  Return nil so
       ;; the null-info branch closes the side window cleanly.
       (t nil))))))

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
         (file-mode major-mode)
         ;; Source-buffer toggle state captured BEFORE switching to
         ;; the side buffer so the side rendering mirrors source.
         (src-diff-hidden shipit-review--diff-overlays-hidden)
         ;; PR number captured here so the side buffer\='s header
         ;; line can render \"PR#NNN\" -- `shipit-review--pr-data'
         ;; is `defvar-local' on the SOURCE buffer, so reading it
         ;; from inside `with-current-buffer buf' returns nil.
         (src-pr-number (and shipit-review--pr-data
                             (cdr (assq 'number
                                        shipit-review--pr-data))))
         ;; Active-side marker captured here too, for the
         ;; side buffer\='s header line.
         (src-active-side shipit-review--active-history-side))
    (cond
     ((not range)
      (message "Previous change: no hunk near point"))
     (t
      (let* ((default-directory (file-name-as-directory root))
             (info
              (cond
               (pinned-info pinned-info)
               (t
                ;; File-level pick of the A-side initial commit:
                ;; the most recent commit on this file that\='s NOT
                ;; in the PR (= the last commit before the PR
                ;; touched the file).  When the chain is entirely
                ;; inside the PR (file was added by this PR), fall
                ;; back to the oldest in-PR commit -- that\='s the
                ;; commit that introduced the file, the natural
                ;; \"before\" reference for review.
                (let* ((chain (shipit-review--history-chain rel range))
                       (pr-shas
                        (let ((h (make-hash-table :test 'equal)))
                          (dolist (c (cdr (assq 'commits
                                                shipit-review--pr-data)))
                            (let ((s (cdr (assq 'sha c))))
                              (when s (puthash s t h))))
                          h))
                       (initial-info
                        (or (cl-find-if
                             (lambda (e)
                               (not (gethash (plist-get e :sha) pr-shas)))
                             chain)
                            (car (last chain))))
                       (initial-sha (and initial-info
                                         (plist-get initial-info :sha)))
                       (idx (and chain initial-sha
                                 (cl-position
                                  initial-sha chain
                                  :key (lambda (e) (plist-get e :sha))
                                  :test 'string=))))
                  (setq-local shipit-review--history-chain chain)
                  (setq-local shipit-review--history-index idx)
                  initial-info)))))
        (cond
         ((null info)
          (let ((short (if shipit-review--base-sha
                           (substring shipit-review--base-sha 0 7)
                         "PR base")))
            (message "Previous change: %s introduced these lines"
                     short)
            ;; Fall back to rendering the file at BASE_SHA so
            ;; the user still sees the pre-PR file context for
            ;; the surrounding code -- the side window stays
            ;; useful even when the specific hunk has no prior
            ;; commit touching its lines.
            (shipit-review--render-history-base-fallback
             rel range shipit-review--base-sha
             file-mode src-pr-number source-line)))
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
                ;; Direct call: `shipit-review-history' is required
                ;; at the top of this file so the helper is always
                ;; bound by the time this code runs.  A previous
                ;; `(when (fboundp ...))' guard masked a stale-
                ;; bytecode situation where the require hadn\='t
                ;; taken effect and the side buffer stayed empty;
                ;; let any error surface so the bug is visible
                ;; instead of producing a silent zero-byte buffer.
                (shipit-review-history-insert-file-at-sha
                 sha path-at-sha root)
                (shipit--debug-log
                 "H-RENDER A (after insert): sha=%s rel=%s path-at-sha=%s root=%s inserted-bytes=%d"
                 (substring sha 0 (min 7 (length sha)))
                 rel path-at-sha root (- (point-max) (point-min)))
                (when (and file-mode (fboundp file-mode)
                           (not (eq major-mode file-mode)))
                  (funcall file-mode))
                (shipit--debug-log
                 "H-RENDER B (after mode change): mode=%s bytes=%d"
                 major-mode (- (point-max) (point-min)))
                ;; Mirror the source\='s `o' toggle so the side
                ;; buffer\='s own diff overlays follow the same
                ;; show/hide state.  Without this, the side
                ;; always rendered overlays regardless of
                ;; source\='s setting.
                (setq-local shipit-review--diff-overlays-hidden
                            src-diff-hidden)
                ;; Apply the previous-change commit's own diff
                ;; overlays (parent..sha for path-at-sha) so the
                ;; reviewer sees what THAT commit did.
                (when parent
                  (shipit-review--apply-diff-overlays
                   parent sha path-at-sha))
                (shipit--debug-log
                 "H-RENDER C (after diff overlays): bytes=%d parent=%s changed-lines=%d"
                 (- (point-max) (point-min))
                 (if parent (substring parent 0 (min 7 (length parent))) "nil")
                 (length shipit-review--changed-lines))
                (setq-local header-line-format
                            (let* ((pr-num src-pr-number)
                                   (fname (file-name-nondirectory rel))
                                   (active-p
                                    (eq src-active-side 'history))
                                   (marker
                                    (if active-p "▶" " "))
                                   (marker-face
                                    (if active-p
                                        '(:weight bold
                                                  :inherit
                                                  shipit-comment-indicator-face)
                                      'shadow))
                                   (title-face
                                    (if active-p
                                        '(:weight bold
                                                  :inherit
                                                  shipit-comment-indicator-face)
                                      'shadow)))
                              (list
                               " "
                               (propertize marker 'face marker-face)
                               (propertize " A: " 'face '(:weight bold))
                               (propertize "│ " 'face 'shadow)
                               (propertize
                                (format "*PR#%s Prev %s: %s*"
                                        (or pr-num "?")
                                        (plist-get info :short)
                                        fname)
                                'face title-face)
                               "  "
                               (propertize (plist-get info :author)
                                           'face 'shipit-username-face)
                               "  "
                               (propertize (plist-get info :ago)
                                           'face 'shipit-timestamp-face)
                               "  -- "
                               (or (plist-get info :subject) ""))))
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
            ;; Pop the H side window on the LEFT, as a regular
            ;; split (NOT a side window) so timeline / ediff
            ;; can splice windows around it.  Side windows
            ;; can\='t be split via `display-buffer-in-
            ;; direction', which left the timeline always
            ;; under source even when A was active.  A regular
            ;; left-split is splittable while still giving the
            ;; ediff-like A-on-left / B-on-right layout.
            ;;
            ;; Anchor the split on the SOURCE window via
            ;; `(window . SOURCE-WIN)' -- the user is usually
            ;; focused in the legend when `H' fires, so
            ;; `selected-window' (the default anchor) is the
            ;; legend\='s bottom side window, which can\='t be
            ;; split and the new buffer ends up hidden.
            ;;
            ;; Pop only on first creation; later refreshes
            ;; update the existing window in place.
            (unless existing
              (let ((source-win (get-buffer-window
                                 (current-buffer) t)))
                (display-buffer
                 buf
                 `((display-buffer-reuse-window
                    display-buffer-in-direction)
                   (direction . left)
                   ,@(when (window-live-p source-win)
                       `((window . ,source-win)))
                   (window-width . 0.5)))))
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

(defun shipit-review--commit-in-pr-p (sha)
  "Return non-nil if SHA is one of the PR\='s own commits.
A commit is in the PR when it\='s reachable from
`--head-sha' but not from `--base-sha'.  Used by
`--history-step' to label chain entries as either
\"PR commit\" or \"Previous change\" in the echo-area
status."
  (and sha
       shipit-review--base-sha
       (let ((head (or shipit-review--head-sha "HEAD")))
         (and (zerop (process-file "git" nil nil nil
                                   "merge-base" "--is-ancestor"
                                   sha head))
              (not (zerop (process-file
                           "git" nil nil nil
                           "merge-base" "--is-ancestor"
                           sha shipit-review--base-sha)))))))

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
        "A/B split not active -- press `|' to open the prev side first"))
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
           (message "Already at the newest commit touching these lines (1/%d)"
                    total))
          ((>= new-idx total)
           (message "No older changes to these lines (%d/%d)"
                    total total))
          (t
           (setq-local shipit-review--history-index new-idx)
           (let* ((info (nth new-idx chain))
                  (sha (plist-get info :sha))
                  (in-pr (shipit-review--commit-in-pr-p sha))
                  (label (if in-pr "PR commit" "Previous change")))
             (shipit-review--apply-history-overlay info)
             ;; Refresh the timeline highlight so it follows
             ;; the H side\='s walking SHA.  The rehighlight
             ;; helper reads `--current-sha-from' which now
             ;; returns `--side-prev-sha' when active side
             ;; is \='history.
             (shipit-review-history-rehighlight-for-source
              (current-buffer))
             (message "%s %d/%d: %s by %s, %s"
                      label
                      (1+ new-idx) total
                      (plist-get info :short)
                      (plist-get info :author)
                      (plist-get info :ago)))))))))))

(defvar-local shipit-review--active-history-side 'source
  "Which side `M-n' / `M-p' navigate in: \\='source or \\='history.

  - \\='source: walk the SOURCE buffer (the `B' / HEAD side)
    through `--file-commits-list' via `--prev-commit' /
    `--next-commit'.  Default -- works whether or not `H'
    is active.

  - \\='history: walk the H side buffer (the `A' / prev
    side) through `--history-chain' via the existing
    `--history-step' DELTA path.  Only meaningful when `H'
    is active.

Buffer-local on the source review buffer.  Toggled via the
`s' key in the actions legend.")

(defun shipit-review--refresh-active-side-headers ()
  "Re-render header-line on source + side to show active state.
Source\='s header builder reads
`shipit-review--active-history-side' directly; the side
buffer\='s header is built in `--apply-history-overlay'
using the captured `src-active-side' arg, so we call
`--apply-history-overlay' with `pinned-info' to rebuild
the side without re-running git log / re-walking the
chain.

Skipped when H is not active or no review buffer is set
up."
  (shipit-review--apply-source-header-line)
  (when (and shipit-review--history-shown
             shipit-review--history-side-buffer
             (buffer-live-p shipit-review--history-side-buffer))
    (let ((info (and shipit-review--history-chain
                     shipit-review--history-index
                     (nth shipit-review--history-index
                          shipit-review--history-chain))))
      (when info
        (shipit-review--apply-history-overlay info)))))

(defun shipit-review--locate-magit-buffer ()
  "Return the magit PR buffer that owns this review session, or nil.
Identified by holding `shipit-buffer-pr-number' equal to
the source review buffer\='s PR.  Used as the
`:source-buffer' for the shipit-editor so live preview
renders into the PR buffer\='s comment section."
  (let ((target-pr
         (and (boundp 'shipit-review--pr-data)
              shipit-review--pr-data
              (cdr (assq 'number shipit-review--pr-data)))))
    (and target-pr
         (cl-loop for buf in (buffer-list)
                  when (and (buffer-local-value 'shipit-buffer-pr-number buf)
                            (equal (buffer-local-value
                                    'shipit-buffer-pr-number buf)
                                   target-pr))
                  return buf))))

(defun shipit-review--pr-repo ()
  "Return the `owner/repo' slug for the current review buffer."
  (or (and shipit-review--pr-data
           (cdr (assq 'full_name
                      (cdr (assq 'repo
                                 (cdr (assq 'base shipit-review--pr-data)))))))
      (shipit--repo-for-buffer)))

(defun shipit-review--inline-comments-at-line (line)
  "Return inline comments in the source file whose `line' equals LINE."
  (cl-remove-if-not
   (lambda (c) (equal (cdr (assq 'line c)) line))
   (or (shipit-review--inline-comments-for-file) '())))

(defun shipit-review--pick-comment-at-line (line prompt)
  "Pick one inline comment on LINE via PROMPT, or return nil."
  (let ((comments (shipit-review--inline-comments-at-line line)))
    (cond
     ((null comments) nil)
     ((= 1 (length comments)) (car comments))
     (t
      (let* ((labels
              (mapcar
               (lambda (c)
                 (let* ((user (or (cdr (assq 'login
                                             (cdr (assq 'user c))))
                                  "?"))
                        (body (or (cdr (assq 'body c)) ""))
                        (first-line
                         (car (split-string body "\n" t)))
                        (snippet
                         (truncate-string-to-width
                          (or first-line "(empty)") 60 nil nil "...")))
                   (cons (format "%s: %s" user snippet) c)))
               comments))
             (choice (completing-read prompt labels nil t)))
        (cdr (assoc choice labels)))))))

(defun shipit-review--editor-context (extras)
  "Build the shipit-editor context for the source review buffer.
EXTRAS is a plist appended after the common base fields
(`:source-buffer', `:pr-number', `:repo', `:file-path').
The base buffer is the magit PR buffer when available so
the editor\='s live preview renders into the comment
section; otherwise the file review buffer itself."
  (let* ((file (buffer-file-name))
         (root (and file (shipit--get-repo-root)))
         (rel (and file root (file-relative-name file root)))
         (pr-number (and shipit-review--pr-data
                         (cdr (assq 'number shipit-review--pr-data))))
         (repo (shipit-review--pr-repo))
         (magit-buf (shipit-review--locate-magit-buffer)))
    (unless (and rel pr-number repo)
      (user-error
       "Cannot resolve comment context (rel=%s pr=%s repo=%s)"
       rel pr-number repo))
    ;; In review-mode the user is looking at the FILE buffer, not
    ;; the magit PR view -- the live-preview overlay should land
    ;; in the file so they can see where the comment will appear.
    ;; Outside review-mode (e.g. acting on a magit section
    ;; directly) keep the existing preference for the magit buffer.
    (append (list :source-buffer (if (bound-and-true-p shipit-review-mode)
                                     (current-buffer)
                                   (or magit-buf (current-buffer)))
                  :pr-number pr-number
                  :repo repo
                  :file-path rel)
            extras)))

(defun shipit-review--inline-comment-lines ()
  "Return the sorted list of distinct lines with inline comments.
Used by `shipit-review-next-comment' /
`shipit-review-prev-comment' to jump between comments.
Reads from `shipit-review--inline-comments-for-file' so the
same filtering (path, outdated, ancestor / in-PR) that drives
the `i' toggle also drives navigation -- no \"jump to a
comment that isn\='t even visible at this SHA\"."
  (sort (delete-dups
         (delq nil
               (mapcar (lambda (c) (cdr (assq 'line c)))
                       (shipit-review--inline-comments-for-file))))
        #'<))

(defun shipit-review-next-comment ()
  "Move point to the next inline comment after the current line.
Wraps to the first comment when past the last.  Reads the
same comment set the `i' overlay uses, so navigation tracks
visibility-at-displayed-SHA without surprises.  Routes
through `shipit-review--goto-line' so the destination is
recentered and the fringe nav indicator follows."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((lines (shipit-review--inline-comment-lines))
            (cur (line-number-at-pos))
            (next (cl-find-if (lambda (l) (> l cur)) lines))
            (target (or next (car lines))))
       (cond
        ((null lines)
         (message "No inline comments in this file"))
        (t
         (shipit-review--goto-line target)
         (message "Comment %d/%d on line %d"
                  (1+ (cl-position target lines))
                  (length lines) target)))))))

(defun shipit-review-prev-comment ()
  "Move point to the inline comment before the current line.
Wraps to the last comment when above the first.  Routes
through `shipit-review--goto-line' so the destination is
recentered and the fringe nav indicator follows."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((lines (shipit-review--inline-comment-lines))
            (cur (line-number-at-pos))
            (prev (cl-find-if (lambda (l) (< l cur))
                              (reverse lines)))
            (target (or prev (car (last lines)))))
       (cond
        ((null lines)
         (message "No inline comments in this file"))
        (t
         (shipit-review--goto-line target)
         (message "Comment %d/%d on line %d"
                  (1+ (cl-position target lines))
                  (length lines) target)))))))

(defun shipit-review-suggest-from-edits ()
  "Open the inline-comment editor with a `suggestion' block.
Pulls the active region's lines (or the current line when
no region is active) out of the buffer's current edited
state and pre-populates the editor body with a GitHub
suggestion block.  On submit GitHub turns the comment into
a clickable \"Apply suggestion\" affordance for the PR
author.

Errors out when the buffer has no pending edits and no
region -- in that case there's nothing to suggest changing,
and `c a' is the right tool for a plain comment."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((has-region (use-region-p))
            (start-line (if has-region
                            (line-number-at-pos (region-beginning))
                          (line-number-at-pos)))
            (end-line (if has-region
                          (save-excursion
                            (goto-char (region-end))
                            (if (and (> (region-end) (region-beginning))
                                     (bolp))
                                (1- (line-number-at-pos))
                              (line-number-at-pos)))
                        start-line))
            (lines (cl-loop for n from start-line to end-line
                            collect
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line (1- n))
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))))
       (when (and (null shipit-review--edit-line-overlays)
                  (not has-region))
         (user-error
          "No pending edits -- press `c a' for a plain comment instead"))
       (shipit-editor-open
        (shipit-review--editor-context
         (list :type 'inline-comment
               :line-number end-line
               :old-line-number start-line
               :side "RIGHT"
               :initial-content
               (concat "```suggestion\n"
                       (mapconcat #'identity lines "\n")
                       "\n```\n\n")))))))) 

(defun shipit-review-add-inline-comment ()
  "Add a new inline comment at the current source line.
Opens the shipit-editor.  On `C-c C-c' the backend\='s
add-inline-comment endpoint posts the body and the PR
buffer refreshes its inline-comments cache; the file
overlay picks up the new comment on the next `i' toggle."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (shipit-editor-open
      (shipit-review--editor-context
       (list :type 'inline-comment
             :line-number (line-number-at-pos)
             :side "RIGHT"))))))

(defun shipit-review-reply-to-comment-at-point ()
  "Reply to an inline comment on the current line.
Prompts for which comment when multiple exist on the line."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((line (line-number-at-pos))
            (target (shipit-review--pick-comment-at-line
                     line "Reply to comment: ")))
       (unless target
         (user-error "No inline comment on line %d to reply to" line))
       (shipit-editor-open
        (shipit-review--editor-context
         (list :type 'reply
               :line-number line
               :parent-comment-id (cdr (assq 'id target)))))))))

(defun shipit-review-edit-comment-at-point ()
  "Edit one of the inline comments on the current line.
Prompts for which when multiple exist."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((line (line-number-at-pos))
            (target (shipit-review--pick-comment-at-line
                     line "Edit comment: ")))
       (unless target
         (user-error "No inline comment on line %d to edit" line))
       (shipit-editor-open
        (shipit-review--editor-context
         (list :type 'inline-comment
               :line-number line
               :comment-id (cdr (assq 'id target))
               :initial-content (or (cdr (assq 'body target)) ""))))))))

(defun shipit-review-react-to-comment-at-point ()
  "Toggle a reaction on an inline comment at the current line.
Prompts for the reaction emoji name and then for which
comment if multiple exist on the line."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((line (line-number-at-pos))
            (target (shipit-review--pick-comment-at-line
                     line "React to comment: "))
            (repo (shipit-review--pr-repo)))
       (unless target
         (user-error "No inline comment on line %d to react to" line))
       (let* ((reaction
               (completing-read
                "Reaction: "
                '("+1" "-1" "laugh" "hooray"
                  "confused" "heart" "rocket" "eyes")
                nil t))
              (resolved (shipit-comment--resolve-for-repo repo))
              (backend (car resolved))
              (config (cdr resolved))
              (fn (plist-get backend :toggle-reaction)))
         (unless fn
           (user-error "Comment backend does not support reactions"))
         (funcall fn config (cdr (assq 'id target)) reaction t)
         (message "Toggled %s on comment by %s"
                  reaction
                  (or (cdr (assq 'login (cdr (assq 'user target))))
                      "?")))))))

(defun shipit-review-toggle-active-side ()
  "Swap which side `M-n' / `M-p' navigate.
Toggles `shipit-review--active-history-side' between
\\='source and \\='history.  When `H' is not active, locks
to \\='source -- there\\='s no history side to switch to.

Refreshes both buffers\\=' header lines so the active side
shows an `▶' marker in bright `shipit-comment-indicator-
face' and the inactive side dims to `shadow' -- a
persistent visual cue beyond the echo-area message."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      ((not shipit-review--history-shown)
       (setq-local shipit-review--active-history-side 'source)
       (shipit-review--refresh-active-side-headers)
       (message "Active side: B (HEAD) -- prev side not open (`|' to open)"))
      (t
       (setq-local shipit-review--active-history-side
                   (if (eq shipit-review--active-history-side 'source)
                       'history
                     'source))
       (shipit-review--refresh-active-side-headers)
       (message
        "Active side: %s"
        (if (eq shipit-review--active-history-side 'source)
            "B (HEAD)"
          "A (prev)")))))))

(defun shipit-review--timeline-buffer-name ()
  "Return the timeline buffer name for the current source file.
Mirrors `shipit-review-history--launch's
`*shipit-file-history: FILE*' naming so the same buffer is
detected as already-open."
  (let* ((info (shipit-review-history--worktree-and-file
                (current-buffer)))
         (file (cdr info)))
    (and file (format "*shipit-file-history: %s*" file))))

(defun shipit-review--active-side-window ()
  "Return the window for the active side: source or H side buffer."
  (let ((buf (cond
              ((eq shipit-review--active-history-side 'history)
               shipit-review--history-side-buffer)
              (t (current-buffer)))))
    (and buf (buffer-live-p buf) (get-buffer-window buf t))))

(defun shipit-review--show-timeline-near-active ()
  "Display the file timeline in a window adjacent to the active side.
Used by `M-n' / `M-p' so the user can see where they are
in the file\='s commit history without leaving the
keyboard.

Uses `display-buffer-in-direction' with `direction = below'
relative to the active side\='s window so the timeline
appears under whichever buffer the user is stepping
through.  Sits above the bottom-side legend (which is
itself a side window, anchored beneath all regular
windows).  Doesn\='t take focus -- the legend keeps
keyboard input."
  (let* ((name (shipit-review--timeline-buffer-name))
         (existing (and name (get-buffer name)))
         (anchor (shipit-review--active-side-window))
         (action
          `((display-buffer-in-direction)
            (direction . below)
            ,@(when (window-live-p anchor) `((window . ,anchor)))
            (window-height . 0.3)
            (window-parameters
             (no-other-window . nil)))))
    (cond
     (existing
      (unless (get-buffer-window existing t)
        (display-buffer existing action)))
     (t
      ;; Timeline buffer doesn\='t exist yet -- build it.
      ;; Override `display-buffer' rules with our directional
      ;; placement so the initial popup also lands below the
      ;; active side; without this,
      ;; `shipit-review-history--launch's plain
      ;; `display-buffer' uses the frame\='s default rules
      ;; and the timeline lands wherever Emacs picks (often
      ;; the LEFT split, which has nothing to do with the
      ;; active side).
      (let ((display-buffer-overriding-action action))
        (shipit-review-file-history))))))

(defun shipit-review-toggle-timeline ()
  "Toggle the per-file commit timeline window.
If the timeline buffer is visible, hide its window.
Otherwise show it (creating the buffer if needed) and
position it adjacent to the active side via
`--show-timeline-near-active'."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (let* ((name (shipit-review--timeline-buffer-name))
            (existing (and name (get-buffer name)))
            (win (and existing (get-buffer-window existing t))))
       (cond
        (win
         (delete-window win)
         (message "Timeline hidden"))
        (t
         (shipit-review--show-timeline-near-active)))))))

(defun shipit-review--history-older ()
  "Walk the active side one commit further back into history.
The active side is `shipit-review--active-history-side':

  - \\='source -> call `--prev-commit' so the source buffer
    re-renders at the older SHA via `--view-file-commit'.
    Works with `H' off or on.

  - \\='history -> walk the H chain via `--history-step 1'
    (only when `H' is active).

Also opens the per-file commit timeline near the active
side (if not already visible) so the user sees where
they are in history while stepping."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      ((eq shipit-review--active-history-side 'history)
       (shipit-review--history-step 1))
      (t
       (shipit-review--prev-commit)))
     (shipit-review--show-timeline-near-active))))

(defun shipit-review--history-newer ()
  "Walk the active side one commit forward (toward HEAD).
See `--history-older' for the dispatch on
`shipit-review--active-history-side' and the timeline
auto-show behaviour."
  (interactive)
  (shipit-review--with-source-buffer
   (lambda ()
     (cond
      ((eq shipit-review--active-history-side 'history)
       (shipit-review--history-step -1))
      (t
       (shipit-review--next-commit)))
     (shipit-review--show-timeline-near-active))))

(defun shipit-review--update-diff-overlays ()
  "Update visual overlays to mark changed lines based on current diff."
  (interactive)
  (when shipit-review--base-sha
    (condition-case err
        (let* ((commits (shipit-review--pr-commits))
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
  (remove-overlays (point-min) (point-max) 'shipit-review-change t)
  (remove-overlays (point-min) (point-max) 'shipit-review-nav-indicator t)
  (setq shipit-review--nav-indicator-overlay nil)
  ;; If the buffer was showing a historical SHA via content swap,
  ;; restore the worktree file so the user isn't left with stale
  ;; read-only content after toggling review-mode off.
  (when shipit-review--showing-historical-sha
    (shipit-review--restore-worktree-content)))

(provide 'shipit-review-mode)
;;; shipit-review-mode.el ends here
