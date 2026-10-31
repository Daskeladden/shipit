;;; shipit-commit-comments-buffer.el --- PR-style view of branch commit comments -*- lexical-binding: t; -*-

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
;; GitHub allows commenting on commits of branches that have no PR --
;; the comments show up on commit pages and on the compare view
;; (github.com/OWNER/REPO/compare/BRANCH).  Those are "commit
;; comments": attached to a commit SHA, optionally anchored to a
;; path + diff position.
;;
;; This buffer displays a branch the way a PR buffer would -- title,
;; refs, checks, commits, files changed, general comments -- reusing
;; the shared PR section renderers (`shipit--render-commits-section',
;; `shipit--render-files-section') the same way the PR-preview buffer
;; does.  Sections that only make sense for a real PR (description,
;; state, approval, assignees, labels, ...) are omitted.
;;
;; Inline commit comments are woven into the file diffs: a commit
;; comment's `position' is relative to its own commit's diff, so it
;; is resolved to a file line via that commit's patch and marked
;; outdated when the branch's total diff no longer contains the line
;; (mirroring how PR review comments go outdated).  The inline-comment
;; cache variables are set buffer-locally, so PR buffers are
;; unaffected.
;;
;; Two modes:
;; - branch mode: the whole compare range against the default branch
;;   (entry: `shipit-branch-comments', also the fallback when "Open PR
;;   for current branch" finds no PR);
;; - single-commit mode: one commit (the landing spot for "Commit"
;;   notifications).

;;; Code:

(require 'magit-section)
(require 'shipit-core)
(require 'shipit-faces)
(require 'shipit-pr-backends)
(require 'shipit-render)

;; Shared PR section machinery (loaded lazily before rendering; the
;; entry points `require' shipit-pr-sections -- a load-time require
;; would cross-link UI files).
(declare-function shipit--render-commits-section "shipit-pr-sections")
(declare-function shipit--render-files-section "shipit-pr-sections")
(declare-function shipit--process-single-commit-data "shipit-pr-sections")
(declare-function shipit--group-comments-by-api-replies "shipit-pr-sections")
(declare-function shipit--insert-status-hierarchical-comment "shipit-pr-sections")
(declare-function shipit--insert-files-content "shipit-pr-sections")
(declare-function shipit--open-file-at-point "shipit-diff")
(declare-function shipit--ediff-file-at-point "shipit-diff")
(declare-function shipit-dwim "shipit-pr-actions")
(declare-function shipit-files-filter "shipit-file-filter")
(declare-function shipit--imenu-create-index "shipit-buffer")

(defvar shipit-buffer-repo)
(defvar shipit--cached-inline-comments)
(defvar shipit--inline-comments-fetched)
(defvar shipit--cached-general-comments)
(defvar shipit--commit-files-force-api)
(defvar shipit-show-pr-checks)

;;; Buffer-local variables

(defvar-local shipit-commit-comments--repo nil
  "Repository name (owner/repo) displayed in this buffer.")

(defvar-local shipit-commit-comments--branch nil
  "Branch displayed in this buffer, nil in single-commit mode.")

(defvar-local shipit-commit-comments--base nil
  "Base branch the compare range runs against (branch mode only).")

(defvar-local shipit-commit-comments--sha nil
  "Commit SHA displayed in single-commit mode, nil in branch mode.")

(defvar-local shipit-commit-comments--payload nil
  "The rendered payload, cached for targeted section refreshes
\(files filter) that must not refetch branch data.")

;;; Mode

(defvar shipit-commit-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "$") #'shipit-process-buffer)
    (define-key map (kbd "g") #'shipit-commit-comments-buffer-refresh)
    (define-key map (kbd "r") #'shipit-commit-comments-buffer-refresh)
    (define-key map (kbd "RET") #'shipit--open-file-at-point)
    (define-key map (kbd "SPC") #'shipit-dwim)
    (define-key map (kbd "M-;") #'shipit-dwim)
    (define-key map (kbd "e") #'shipit--ediff-file-at-point)
    (define-key map (kbd "f") #'shipit-files-filter)
    (define-key map (kbd "o") #'shipit-commit-comments-browse)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `shipit-commit-comments-mode'.
File/dwim commands are the PR buffer's own -- the sections carry
the same text properties, so they behave identically here.")

(define-derived-mode shipit-commit-comments-mode magit-section-mode "Shipit-Commit-Comments"
  "Major mode for the PR-style view of a branch's commit comments.

\\{shipit-commit-comments-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-commit-comments-buffer-refresh)
  (setq-local shipit-page-url-function #'shipit-commit-comments--url)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local tab-width 8)
  (shipit--apply-section-defaults)
  (font-lock-mode 1)
  (when (fboundp 'shipit--imenu-create-index)
    (setq-local imenu-create-index-function #'shipit--imenu-create-index)))

;;; Backend access

(defun shipit-commit-comments--backend-ops (repo)
  "Resolve the PR backend for REPO into an ops plist.
Returns (:config :compare :comments :commit :checks :browse-commit
:browse-compare).
Signals `user-error' when the backend has no commit-comment
support (the op is GitHub-only for now)."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (config (cdr resolved))
         (comments-fn (plist-get backend :fetch-commit-comments)))
    (unless comments-fn
      (user-error "Commit comments are not supported for this backend"))
    (list :config config
          :compare (plist-get backend :fetch-compare)
          :comments comments-fn
          :commit (plist-get backend :fetch-commit)
          :checks (plist-get backend :fetch-checks)
          :browse-commit (plist-get backend :browse-commit-url)
          :browse-compare (plist-get backend :browse-compare-url))))

;;; Position resolution — commit-comment `position' → file line

(defconst shipit-commit-comments--hunk-header-re
  "\\`@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)"
  "Regexp matching a unified-diff hunk header, capturing old/new start lines.")

(defun shipit-commit-comments--position-to-line (patch position)
  "Resolve POSITION in PATCH to a diff line.
POSITION is GitHub's commit-comment diff position: the first line
after the patch's first @@ hunk header is 1, and every subsequent
patch line (including later @@ headers) increments it.  Returns
\(:line N :side \"RIGHT\") with the new-file line for additions and
context lines, (:line N :side \"LEFT\") with the old-file line for
deletions, or nil when POSITION falls outside PATCH or lands on a
hunk header / no-newline marker."
  (when (and patch position (> position 0))
    (let ((lines (split-string patch "\n"))
          (pos 0) (old 0) (new 0) (started nil) (result nil))
      (while (and lines (not result))
        (let ((line (pop lines)))
          (cond
           ((string-match shipit-commit-comments--hunk-header-re line)
            (when started
              (setq pos (1+ pos))
              (when (= pos position) (setq result 'hunk-header)))
            (setq old (1- (string-to-number (match-string 1 line)))
                  new (1- (string-to-number (match-string 2 line)))
                  started t))
           ((not started) nil)
           (t
            (setq pos (1+ pos))
            (let ((c (if (string-empty-p line) ?\s (aref line 0))))
              (cond
               ((eq c ?+)
                (setq new (1+ new))
                (when (= pos position)
                  (setq result (list :line new :side "RIGHT"))))
               ((eq c ?-)
                (setq old (1+ old))
                (when (= pos position)
                  (setq result (list :line old :side "LEFT"))))
               ((eq c ?\\)
                (when (= pos position) (setq result 'no-newline)))
               (t
                (setq old (1+ old))
                (setq new (1+ new))
                (when (= pos position)
                  (setq result (list :line new :side "RIGHT"))))))))))
      (and (consp result) result))))

(defun shipit-commit-comments--patch-covers-line-p (patch line side)
  "Return non-nil when PATCH touches LINE on SIDE.
SIDE \"RIGHT\" matches the new-file line of additions and context
lines; \"LEFT\" matches the old-file line of deletions and context
lines."
  (when (and patch line)
    (let ((lines (split-string patch "\n"))
          (old 0) (new 0) (started nil) (covered nil))
      (while (and lines (not covered))
        (let ((l (pop lines)))
          (cond
           ((string-match shipit-commit-comments--hunk-header-re l)
            (setq old (1- (string-to-number (match-string 1 l)))
                  new (1- (string-to-number (match-string 2 l)))
                  started t))
           ((not started) nil)
           ((string-prefix-p "+" l)
            (setq new (1+ new))
            (when (and (string= side "RIGHT") (= new line))
              (setq covered t)))
           ((string-prefix-p "-" l)
            (setq old (1+ old))
            (when (and (string= side "LEFT") (= old line))
              (setq covered t)))
           ((string-prefix-p "\\" l) nil)
           (t
            (setq old (1+ old))
            (setq new (1+ new))
            (when (or (and (string= side "RIGHT") (= new line))
                      (and (string= side "LEFT") (= old line)))
              (setq covered t))))))
      covered)))

;;; Comment adaptation — feed commit comments to the PR diff machinery

(defun shipit-commit-comments--file-patch-alist (commit-data)
  "Return an alist of filename → patch for COMMIT-DATA's files."
  (mapcar (lambda (f)
            (cons (cdr (assq 'filename f)) (cdr (assq 'patch f))))
          (append (cdr (assq 'files commit-data)) nil)))

(defun shipit-commit-comments--adapt-comment (comment commit-patches compare-patches)
  "Adapt commit COMMENT for the PR inline-comment machinery.
Resolves the comment's diff position (relative to its own commit's
patch, looked up in COMMIT-PATCHES) to a line and side, then marks
the comment outdated when the displayed diff (COMPARE-PATCHES) no
longer contains that line -- the same way PR review comments go
outdated.  The resolved keys are prepended, so they win over the
API's `line: null'."
  (let* ((path (cdr (assq 'path comment)))
         (position (cdr (assq 'position comment)))
         (commit-patch (cdr (assoc path commit-patches)))
         (resolved (and path position
                        (shipit-commit-comments--position-to-line
                         commit-patch position)))
         (line (plist-get resolved :line))
         (side (plist-get resolved :side))
         (compare-patch (cdr (assoc path compare-patches)))
         (covered (and resolved
                       (shipit-commit-comments--patch-covers-line-p
                        compare-patch line side))))
    (append
     '((shipit-comment-type . "commit"))
     (cond
      ((not resolved) '((outdated . t)))
      ((string= side "LEFT")
       (append `((original_line . ,line) (side . ,side))
               (unless covered '((outdated . t)))))
      (t
       (append `((line . ,line) (side . ,side))
               (unless covered '((outdated . t))))))
     comment)))

;;; Data assembly

(defun shipit-commit-comments--split-comments (comments commit-patches compare-patches)
  "Split COMMENTS into (INLINE . GENERAL), adapting the inline ones.
Comments with a path become inline comments resolved against
COMMIT-PATCHES / COMPARE-PATCHES; pathless ones are general."
  (let ((inline nil)
        (general nil))
    (dolist (comment comments)
      (if (cdr (assq 'path comment))
          (push (shipit-commit-comments--adapt-comment
                 comment commit-patches compare-patches)
                inline)
        (push (append '((shipit-comment-type . "commit")) comment) general)))
    (cons (nreverse inline) (nreverse general))))

(defun shipit-commit-comments--branch-payload (ops branch base)
  "Fetch and assemble everything the branch view renders.
OPS is the backend ops plist; the compare range is BASE...BRANCH.
Returns a plist (:commits :files :ahead :behind :head-sha :inline
:general).  Messages when the compare response truncated the
commit list (250-commit API cap), so a partial scan never poses as
a full one."
  (let* ((config (plist-get ops :config))
         (compare (funcall (plist-get ops :compare) config base branch))
         (commits (append (cdr (assq 'commits compare)) nil))
         (files (append (cdr (assq 'files compare)) nil))
         (ahead (or (cdr (assq 'ahead_by compare)) 0))
         (behind (or (cdr (assq 'behind_by compare)) 0))
         (head-sha (cdr (assq 'sha (car (last commits)))))
         (compare-patches (mapcar (lambda (f)
                                    (cons (cdr (assq 'filename f))
                                          (cdr (assq 'patch f))))
                                  files))
         (inline nil)
         (general nil))
    (when (> ahead (length commits))
      (message "shipit: compare returned %d of %d commits; older commits not checked for comments"
               (length commits) ahead))
    (dolist (c commits)
      (let ((count (or (cdr (assq 'comment_count (cdr (assq 'commit c)))) 0))
            (sha (cdr (assq 'sha c))))
        (when (> count 0)
          (let* ((comments (funcall (plist-get ops :comments) config sha))
                 (commit-fn (plist-get ops :commit))
                 (detail (and commit-fn (funcall commit-fn config sha)))
                 (commit-patches (and detail
                                      (shipit-commit-comments--file-patch-alist detail)))
                 (split (shipit-commit-comments--split-comments
                         comments commit-patches compare-patches)))
            (setq inline (append inline (car split)))
            (setq general (append general (cdr split)))))))
    (list :commits commits :files files :ahead ahead :behind behind
          :head-sha head-sha :inline inline :general general
          :compare-failed (null compare))))

(defun shipit-commit-comments--single-payload (ops sha)
  "Fetch and assemble the single-commit view for SHA via OPS.
The commit's comments are resolved against its own diff, so they
are never outdated here.  Returns the same plist shape as
`shipit-commit-comments--branch-payload' plus :subject."
  (let* ((config (plist-get ops :config))
         (detail (funcall (plist-get ops :commit) config sha))
         (comments (funcall (plist-get ops :comments) config sha))
         (files (append (cdr (assq 'files detail)) nil))
         (patches (shipit-commit-comments--file-patch-alist detail))
         (split (shipit-commit-comments--split-comments comments patches patches))
         (message-text (or (cdr (assq 'message (cdr (assq 'commit detail)))) "")))
    (list :commits (list detail) :files files :head-sha sha
          :subject (car (split-string message-text "\n"))
          :inline (car split) :general (cdr split))))

;;; Section rendering

(defun shipit-commit-comments--branch-heading (branch base ahead behind)
  "Return the branch-mode title heading."
  (format "%s Branch: %s  (%d ahead%s of %s)"
          (shipit--get-pr-field-icon "pull-request" "🔀")
          (propertize branch 'font-lock-face 'magit-branch-remote)
          ahead
          (if (> behind 0) (format ", %d behind" behind) "")
          (propertize base 'font-lock-face 'magit-branch-local)))

(defun shipit-commit-comments--title (payload)
  "Return the title heading for PAYLOAD in the current buffer's mode."
  (if shipit-commit-comments--sha
      (format "%s Commit %s: %s"
              (shipit--get-pr-field-icon "pull-request" "🔀")
              (propertize (substring shipit-commit-comments--sha 0 7)
                          'font-lock-face 'magit-hash)
              (or (plist-get payload :subject) ""))
    (shipit-commit-comments--branch-heading
     shipit-commit-comments--branch shipit-commit-comments--base
     (plist-get payload :ahead) (plist-get payload :behind))))

(defvar shipit-commit-comments-title-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map shipit-commit-comments-mode-map)
    (define-key map (kbd "M-w") #'shipit-commit-comments-copy-url)
    map)
  "Keymap for the title heading (mirrors `shipit-pr-header-keymap').
M-w copies the shareable compare/commit URL.")

(defun shipit-commit-comments--insert-title (repo heading)
  "Insert the title section with HEADING for REPO."
  (magit-insert-section (shipit-branch-title)
    (let ((start (point)))
      (magit-insert-heading heading)
      (add-text-properties start (point)
                           `(shipit-repo ,repo
                             keymap ,shipit-commit-comments-title-keymap
                             help-echo "M-w: copy URL")))))

(defun shipit-commit-comments--insert-refs (base branch)
  "Insert the refs line, mirroring the PR buffer's Refs section."
  (magit-insert-section (pr-refs nil)
    (magit-insert-heading
      (format "%s Refs:      %s ← %s"
              (shipit--get-pr-field-icon "refs" "🔀")
              (propertize base 'font-lock-face 'magit-branch-local)
              (propertize branch 'font-lock-face 'magit-branch-remote)))))

(defun shipit-commit-comments--check-status-icon (check)
  "Return a propertized status glyph for CHECK."
  (let ((status (cdr (assq 'status check)))
        (conclusion (cdr (assq 'conclusion check))))
    (cond
     ((equal conclusion "success")
      (propertize "✓" 'font-lock-face 'success))
     ((member conclusion '("failure" "timed_out" "action_required"))
      (propertize "✗" 'font-lock-face 'error))
     ((equal conclusion "cancelled")
      (propertize "⊘" 'font-lock-face 'shadow))
     ((equal conclusion "skipped")
      (propertize "○" 'font-lock-face 'shadow))
     ((member status '("queued" "in_progress"))
      (propertize "●" 'font-lock-face 'warning))
     (t "?"))))

(defun shipit-commit-comments--insert-checks (ops head-sha)
  "Insert a compact Checks section for HEAD-SHA via OPS.
A fetch error renders as a visible one-line section rather than
killing the whole buffer render."
  (let ((fetch (plist-get ops :checks)))
    (when (and head-sha fetch shipit-show-pr-checks)
      (condition-case err
          (let ((checks (funcall fetch (plist-get ops :config) head-sha)))
            (magit-insert-section (checks nil t)
              (magit-insert-heading
                (format "%s Checks (%s)"
                        (shipit--get-checks-status-emoji-svg
                         checks (shipit--get-checks-status-emoji checks))
                        (shipit--checks-heading-summary checks)))
              (magit-insert-section-body
                (if (null checks)
                    (insert "   No checks found\n")
                  (dolist (check checks)
                    (insert (format "   %s %s\n"
                                    (shipit-commit-comments--check-status-icon check)
                                    (or (cdr (assq 'name check)) "unnamed check")))))
                (insert "\n"))))
        (error
         (magit-insert-section (checks nil)
           (magit-insert-heading
             (format "%s Checks unavailable: %s"
                     (shipit--get-pr-field-icon "checks" "🧪")
                     (error-message-string err)))))))))

(defun shipit-commit-comments--insert-general-comments (repo comments)
  "Insert the General Comments section with pathless COMMENTS for REPO.
Uses the PR buffer's hierarchical comment renderer so the styling
matches general PR comments."
  (magit-insert-section (general-comments nil t)
    (magit-insert-heading
      (format "%s General Comments (%d)"
              (shipit--get-comment-type-icon "comment" "💬")
              (length comments)))
    (magit-insert-section-body
      (if (null comments)
          (insert "   No general comments\n")
        (let* ((threads (shipit--group-comments-by-api-replies comments))
               (roots (gethash 'root threads)))
          (dolist (comment roots)
            (shipit--insert-status-hierarchical-comment
             comment threads nil nil 0 nil nil repo nil))))
      (insert "\n"))))

(defun shipit-commit-comments--render (payload)
  "Render PAYLOAD in the current commit-comments buffer.
Composes the same shared section renderers the PR buffer uses;
the inline-comment cache is set buffer-locally so PR buffers keep
their own state."
  (require 'shipit-pr-sections)
  (let ((repo shipit-commit-comments--repo)
        (ops (shipit-commit-comments--backend-ops shipit-commit-comments--repo))
        (inhibit-read-only t)
        (saved-point (point)))
    (setq-local shipit-commit-comments--payload payload)
    ;; Shared helpers (reaction repo lookup, codeowners filter data)
    ;; fall back to `shipit-buffer-repo'; give them the right repo
    (setq-local shipit-buffer-repo repo)
    (setq-local shipit--cached-inline-comments (plist-get payload :inline))
    (setq-local shipit--inline-comments-fetched t)
    (setq-local shipit--cached-general-comments (plist-get payload :general))
    (setq-local shipit--commit-files-force-api t)
    (erase-buffer)
    (magit-insert-section (root)
      (shipit-commit-comments--insert-title
       repo (shipit-commit-comments--title payload))
      (when shipit-commit-comments--branch
        (shipit-commit-comments--insert-refs shipit-commit-comments--base
                                             shipit-commit-comments--branch))
      (insert "\n")
      (shipit-commit-comments--insert-checks ops (plist-get payload :head-sha))
      ;; Collapsed-section imenu metadata needs shipit-buffer's helper;
      ;; skip it when that UI module isn't loaded (batch tests)
      (let ((store-imenu (fboundp 'shipit--imenu-store-children)))
        (shipit--render-commits-section
         (mapcar #'shipit--process-single-commit-data (plist-get payload :commits))
         repo nil store-imenu)
        (shipit--render-files-section (plist-get payload :files) repo nil nil
                                      (plist-get payload :head-sha) store-imenu))
      (shipit-commit-comments--insert-general-comments
       repo (plist-get payload :general)))
    (goto-char (min saved-point (point-max)))))

;;; Buffer management

(defun shipit-commit-comments--show-buffer (repo name-suffix setup-fn payload)
  "Create or reuse the commit-comments buffer for REPO and render PAYLOAD.
NAME-SUFFIX distinguishes branch and single-commit buffers.
SETUP-FN runs in the buffer to set the mode-specific locals.
Returns the buffer."
  (let ((buffer (get-buffer-create
                 (format "*shipit-commit-comments: %s %s*" repo name-suffix))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'shipit-commit-comments-mode)
        (shipit-commit-comments-mode))
      (setq shipit-commit-comments--repo repo)
      (funcall setup-fn)
      (shipit-commit-comments--render payload))
    (switch-to-buffer buffer)
    buffer))

;;; Entry points

(defun shipit-commit-comments--local-unpushed-count (base branch)
  "Return how many commits local BRANCH has beyond origin/BASE.
nil when the local clone can't answer (different repo, refs
missing) -- callers fall back to a generic message."
  (let ((out (string-trim
              (shell-command-to-string
               (format "git rev-list --count origin/%s..%s 2>/dev/null"
                       (shell-quote-argument base)
                       (shell-quote-argument branch))))))
    (when (string-match-p "\\`[0-9]+\\'" out)
      (string-to-number out))))

(defun shipit-commit-comments--explain-empty-branch (payload branch base)
  "Message why the branch view for BRANCH over BASE has nothing to show.
Distinguishes an unpushed branch (compare failed on GitHub while
local commits exist) from a genuinely up-to-date one, so the user
is never told \"no commits ahead\" about a branch GitHub has not
seen yet.  Returns nil."
  (let ((unpushed (shipit-commit-comments--local-unpushed-count base branch)))
    (cond
     ((and (plist-get payload :compare-failed) unpushed (> unpushed 0))
      (message "Branch '%s' is not on GitHub yet (%d local commit%s) — push it to view it"
               branch unpushed (if (= unpushed 1) "" "s")))
     ((plist-get payload :compare-failed)
      (message "Branch '%s' (or base '%s') not found on GitHub — push the branch first"
               branch base))
     ((and unpushed (> unpushed 0))
      (message "Branch '%s' has %d unpushed commit%s — GitHub only sees pushed commits"
               branch unpushed (if (= unpushed 1) "" "s")))
     (t
      (message "Branch '%s' has no commits ahead of %s" branch base)))
    nil))

(defun shipit-commit-comments-open-branch (repo branch &optional base)
  "Show BRANCH of REPO as a PR-style buffer with its commit comments.
BASE defaults to the upstream default branch.  Returns the buffer,
or nil (with a message) when BRANCH has no commits ahead of BASE."
  (let* ((base (or base (shipit--upstream-default-branch)))
         (ops (shipit-commit-comments--backend-ops repo))
         (payload (shipit-commit-comments--branch-payload ops branch base)))
    (if (null (plist-get payload :commits))
        (shipit-commit-comments--explain-empty-branch payload branch base)
      (shipit-commit-comments--show-buffer
       repo branch
       (lambda ()
         (setq shipit-commit-comments--branch branch
               shipit-commit-comments--base base
               shipit-commit-comments--sha nil))
       payload))))

(defun shipit-commit-comments-open-commit (repo sha)
  "Show commit SHA of REPO as a PR-style single-commit buffer.
Returns the buffer."
  (let* ((ops (shipit-commit-comments--backend-ops repo))
         (payload (shipit-commit-comments--single-payload ops sha)))
    (shipit-commit-comments--show-buffer
     repo (substring sha 0 (min 7 (length sha)))
     (lambda ()
       (setq shipit-commit-comments--sha sha
             shipit-commit-comments--branch nil
             shipit-commit-comments--base nil))
     payload)))

(defun shipit-commit-comments--current-branch ()
  "Return the current git branch name, or nil when detached."
  (let ((branch (string-trim (shell-command-to-string
                              "git symbolic-ref --short HEAD 2>/dev/null"))))
    (unless (string-empty-p branch) branch)))

;;;###autoload
(defun shipit-branch-comments ()
  "Show the current branch as a PR-style buffer with its commit comments.
GitHub allows commenting on commits from the compare page even
when the branch has no PR; this shows the branch's commits, files
changed, and those comments in a PR-buffer-shaped view."
  (interactive)
  (let ((repo (shipit--get-repo-from-remote))
        (branch (shipit-commit-comments--current-branch)))
    (unless repo
      (user-error "Not in a repository with a recognized remote"))
    (unless branch
      (user-error "Not on a branch"))
    (message "Loading branch view for '%s'..." branch)
    (shipit-commit-comments-open-branch repo branch)))

;;; Commands

(defun shipit-commit-comments--sha-at-point ()
  "Return the SHA of the commit section containing point, or nil."
  (let ((section (magit-current-section))
        (sha nil))
    (while (and section (not sha))
      (when (eq (oref section type) 'shipit-commit)
        (setq sha (oref section value)))
      (setq section (oref section parent)))
    sha))

(defun shipit-commit-comments-browse-commit ()
  "Open the commit at point in the browser."
  (interactive)
  (let ((sha (shipit-commit-comments--sha-at-point)))
    (unless sha
      (user-error "No commit at point"))
    (let* ((ops (shipit-commit-comments--backend-ops shipit-commit-comments--repo))
           (browse-fn (plist-get ops :browse-commit)))
      (unless browse-fn
        (user-error "No commit browse URL support for this backend"))
      (browse-url (funcall browse-fn (plist-get ops :config) sha)))))

(defun shipit-commit-comments-browse ()
  "Open the commit at point in the browser, or else the page of this view.
The page of a branch compares it with its base."
  (interactive)
  (if (shipit-commit-comments--sha-at-point)
      (shipit-commit-comments-browse-commit)
    (shipit-open-in-browser)))

(defun shipit-commit-comments--url ()
  "Return the shareable URL for the current buffer's view.
Branch mode: the compare URL for base...branch; single-commit
mode: the commit URL."
  (let* ((ops (shipit-commit-comments--backend-ops shipit-commit-comments--repo))
         (config (plist-get ops :config)))
    (if shipit-commit-comments--sha
        (let ((fn (or (plist-get ops :browse-commit)
                      (user-error "No commit browse URL support for this backend"))))
          (funcall fn config shipit-commit-comments--sha))
      (let ((fn (or (plist-get ops :browse-compare)
                    (user-error "No compare browse URL support for this backend"))))
        (funcall fn config shipit-commit-comments--base
                 shipit-commit-comments--branch)))))

(defun shipit-commit-comments-copy-url ()
  "Copy this view's shareable URL (compare or commit) to the kill ring."
  (interactive)
  (let ((url (shipit-commit-comments--url)))
    (kill-new url)
    (message "Copied %s" url)))

(defun shipit-commit-comments-refresh-files-section ()
  "Re-render the Files Changed section's content from the cached payload.
Called by the files-filter subsystem so filter changes apply
without refetching branch data.  Mirrors the PR buffer's targeted
refresh: the section object and its heading survive -- so root's
children order (which n/p navigation walks) and the neighboring
section boundaries stay intact -- and only the content between the
`content' and `end' markers is regenerated.  A still-collapsed
section is left alone; its deferred body renders on first
expansion."
  (when (and (derived-mode-p 'shipit-commit-comments-mode)
             shipit-commit-comments--payload
             magit-root-section)
    (let ((section (seq-find (lambda (s) (eq (oref s type) 'pr-files))
                             (oref magit-root-section children))))
      (when section
        (let* ((inhibit-read-only t)
               (content-pos (let ((m (oref section content)))
                              (and m (if (markerp m) (marker-position m) m))))
               (end-pos (let ((m (oref section end)))
                          (and m (if (markerp m) (marker-position m) m)))))
          (when (and content-pos end-pos)
            ;; Old children die with the deleted region -- clear the
            ;; list so stale markers can't confuse navigation
            (oset section children nil)
            (save-excursion
              (delete-region content-pos end-pos)
              (goto-char content-pos)
              (let ((magit-insert-section--parent section))
                (shipit--insert-files-content
                 shipit-commit-comments--repo
                 `((files . ,(plist-get shipit-commit-comments--payload :files))
                   (head . ((sha . ,(plist-get shipit-commit-comments--payload :head-sha)))))
                 nil))
              (oset section end (copy-marker (point))))))))))

(defun shipit-commit-comments-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Re-fetch and re-render the commit-comments buffer."
  (interactive)
  (let* ((ops (shipit-commit-comments--backend-ops shipit-commit-comments--repo))
         (payload (if shipit-commit-comments--sha
                      (shipit-commit-comments--single-payload
                       ops shipit-commit-comments--sha)
                    (shipit-commit-comments--branch-payload
                     ops shipit-commit-comments--branch
                     shipit-commit-comments--base))))
    (shipit-commit-comments--render payload)))

(provide 'shipit-commit-comments-buffer)

;;; shipit-commit-comments-buffer.el ends here
