;;; shipit-buffer.el --- Dedicated buffer for shipit PR management -*- lexical-binding: t -*-

;; Copyright (C) 2025 shipit contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This module provides a dedicated buffer for PR management, separate from
;; the magit status buffer. It offers a focused interface for reviewing PRs
;; with better performance and cleaner separation of concerns.

;;; Code:

(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-gh-etag)
(require 'shipit-render)
(require 'shipit-worktree)
(require 'shipit-pr-backends)
(require 'shipit-sections)
(require 'shipit-comment-backends)
(require 'url)
(require 'url-cache)
(require 'imenu)
(require 'transient)

;; Buffer-local hash table for storing imenu children metadata
;; Key: section type symbol, Value: list of (name . (parent-pos . name))
(defvar-local shipit--imenu-children-cache nil
  "Buffer-local hash table for imenu children metadata.
This is more robust than text properties which can be corrupted
when magit sections expand/collapse.")

;; Require magit-section for macros
(eval-when-compile
  (require 'magit-section))
(require 'magit-section)

;; Conditionally require magit-section for batch compilation
(unless (featurep 'magit-section)
  (condition-case nil
      (require 'magit-section)
    (error nil)))

;; Require shipit-pr-sections for section type definitions
(require 'shipit-pr-sections)
(require 'shipit-pr-linked-issue)
(require 'shipit-page-views)
(require 'shipit-pr-overview)

;; Forward declarations
(declare-function shipit-open-repo-buffer "shipit-repo-buffer")
(declare-function shipit-toggle-timestamp-format "shipit-commands")
(declare-function shipit-toggle-file-viewed "shipit-commands")
(declare-function shipit-mark-all-files-viewed "shipit-commands")
(declare-function shipit-actions-copy-url-at-point "shipit-actions")
(declare-function shipit-review-submit "shipit-review-queue")
(declare-function shipit-review-queue-discard "shipit-review-queue")

;; Hook variables for backend-agnostic section operations.
;; Each hook is a list of functions called with (section).
;; Functions return non-nil if they handled the action.

(defvar shipit-buffer-section-expand-functions nil
  "Hook functions for expanding sections that need lazy loading.
Each function is called with SECTION. Return non-nil if handled.")

(defvar shipit-buffer-section-url-functions nil
  "Hook functions for resolving a URL for the section at point.
Each function is called with SECTION. Return a URL string or nil.")

(defvar shipit-buffer-section-log-timestamps-functions nil
  "Hook functions for toggling log timestamps at point.
Each function is called with SECTION. Return non-nil if handled.")
(declare-function shipit--insert-reviews-section "shipit-pr-sections")
(declare-function shipit--insert-assignees-section "shipit-pr-sections")
(declare-function shipit--insert-reviewers-section "shipit-pr-sections")
(declare-function shipit--insert-labels-section "shipit-pr-sections")
(declare-function shipit--insert-commits-section "shipit-pr-sections")
(declare-function shipit--insert-files-section "shipit-pr-sections")
(declare-function shipit--insert-general-comments-section "shipit-pr-sections")
;; Placeholder section functions for incremental rendering
(declare-function shipit--insert-commits-section-placeholder "shipit-pr-sections")
(declare-function shipit--insert-files-section-placeholder "shipit-pr-sections")
(declare-function shipit--insert-general-comments-section-placeholder "shipit-pr-sections")
(declare-function shipit--insert-activity-section-placeholder "shipit-pr-sections")
(declare-function shipit--insert-checks-section-placeholder "shipit-checks")
;; Async section replacement functions
(declare-function shipit--replace-activity-section-with-content "shipit-pr-sections")
(declare-function shipit--replace-general-comments-section-with-content "shipit-pr-sections")
(declare-function shipit--replace-commits-section-with-content "shipit-pr-sections")
(declare-function shipit--replace-files-section-with-content "shipit-pr-sections")
(declare-function shipit--replace-approval-section-with-content "shipit-pr-sections")
(declare-function shipit--set-files-filter "shipit-file-filter")
(declare-function shipit-files-filter "shipit-file-filter")
(declare-function shipit--fetch-timeline-events-async "shipit-http")
(declare-function shipit--fetch-general-comments-async "shipit-http")
(declare-function shipit--fetch-commits-async "shipit-http")
(declare-function shipit--fetch-files-async "shipit-http")
(declare-function shipit--fetch-review-decision-async "shipit-http")
(declare-function shipit-comment--fetch-reactions-batch "shipit-comments")
(declare-function shipit--fetch-pr-reactions-sync "shipit-http")
(declare-function shipit--fetch-pr-reactions-async-with-callback "shipit-http")
(declare-function shipit--update-pr-reactions-display "shipit-http")
(declare-function shipit--fetch-inline-comments "shipit-http")
(declare-function shipit--fetch-more-files-async "shipit-http")
(declare-function shipit--get-notification-source-icon "shipit-render")
(declare-function shipit-pr--backend-id "shipit-pr-backends")

;; Forward declarations for magit section functions
(declare-function magit-current-section "magit-section")
(declare-function magit-section-toggle "magit-section")
(declare-function magit-section-cycle "magit-section")
(declare-function magit-section-post-command-hook "magit-section")
(declare-function magit-section-mode "magit-section")
(declare-function magit-section "magit-section")
(declare-function magit-section-cache-visibility "magit-section")
(declare-function shipit--insert-checks-section "shipit-checks")
(declare-function shipit--display-selected-pr "shipit-pr-search")

;; Forward declarations for shipit command functions
(declare-function shipit-dwim "shipit-commands")
(declare-function shipit-comment "shipit-commands")
(declare-function shipit-worktree "shipit-commands")
(declare-function shipit-approve "shipit-commands")
(declare-function shipit-suggestion-or-approve "shipit-pr-actions")
(declare-function shipit-merge "shipit-pr-actions")
(declare-function shipit-repo-subscription "shipit-repo-buffer")
(declare-function shipit--subscription-state-from-api "shipit-repo-buffer")
(declare-function shipit--subscription-state-label "shipit-repo-buffer")
(declare-function shipit-request-changes "shipit-commands")

;; Forward declarations for helper functions
(declare-function shipit--clean-text "shipit-core")
(declare-function shipit--check-for-new-general-comments "shipit-core")
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--render-body "shipit-render")
(declare-function shipit--get-pr-actual-state "shipit-http")
(declare-function shipit--colorize-pr-state "shipit-http")

;; Forward declarations for worktree functions
(declare-function shipit--get-worktree-status "shipit-worktree")
(declare-function shipit--no-clone-text "shipit-worktree")
(declare-function shipit--find-worktree-for-pr "shipit-worktree")
(declare-function shipit--get-pr-info-from-worktree "shipit-worktree")
(declare-function shipit--get-repo-root "shipit-worktree")
(declare-function shipit--get-repo-from-remote "shipit-core")
(declare-function shipit--get-worktree-status-icon "shipit-render")
(declare-function shipit--get-pr-state-icon "shipit-render")
(declare-function shipit--ediff-file-at-point "shipit-pr-diff")
(declare-function shipit--show-diff-at-point "shipit-pr-diff")
(declare-function shipit-pr-diff-menu "shipit-pr-diff")
(declare-function shipit--show-diff-with-comments-at-point "shipit-pr-diff")
(declare-function shipit--comment-read-post-command-hook "shipit-pr-sections")

;;; Magit section type definitions

(put 'root 'magit-section t)

(put 'shipit-pr-buffer 'magit-section t)

(put 'shipit-worktree-section 'magit-section t)

(put 'shipit-stack-section 'magit-section t)

;;; Configuration

(defcustom shipit-use-dedicated-buffer nil
  "Whether to use dedicated shipit buffer instead of magit integration.
When t, shipit commands will open a dedicated *shipit* buffer.
When nil, shipit sections are embedded in magit status buffer (legacy mode)."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-buffer-side-by-side nil
  "Whether to display shipit buffer side-by-side with magit buffer.
Only effective when `shipit-use-dedicated-buffer' is t."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-buffer-sections
  '(summary approval people metadata commits files comments activity actions)
  "List of sections to display in shipit buffer, in order.
Available sections:
- summary: PR title, description, state
- approval: Review status, teams, checks
- people: Assignees, reviewers
- metadata: Labels, milestone
- commits: Commit history
- files: Changed files with inline comments
- comments: General discussions
- activity: Chronological timeline of PR events
- actions: Merge, approve, comment buttons"
  :type '(repeat (choice (const summary)
                         (const approval)
                         (const people)
                         (const metadata)
                         (const commits)
                         (const files)
                         (const comments)
                         (const activity)
                         (const actions)))
  :group 'shipit)

;;; Buffer-local variables

(defvar-local shipit-buffer-pr-number nil
  "PR number for this shipit buffer.")

(defvar-local shipit-buffer-repo nil
  "Repository for this shipit buffer.")

(defvar-local shipit-buffer-backend-id nil
  "Backend ID for this shipit buffer (e.g. \\='gitlab).
When non-nil, overrides `shipit-pr-backend' for all operations in this buffer.")

(defvar-local shipit-buffer-backend-config nil
  "Backend config plist for this shipit buffer.
When non-nil, overrides `shipit-pr-backend-config' for all operations.")

(defvar-local shipit-buffer--refresh-in-progress nil
  "Flag to prevent recursive buffer refreshes.")

(defvar-local shipit-buffer-pr-data nil
  "Cached PR data for this shipit buffer.")

(defvar-local shipit-buffer-files-truncated nil
  "Non-nil if files list was truncated (more files available).")

(defvar-local shipit-buffer-files-page 10
  "Current page number for files pagination (starts at 10 after initial load).")

;;; Buffer-ready hook — fires once when async section renders have settled

(defcustom shipit-buffer-ready-timeout 10.0
  "Safety timeout in seconds for `shipit-buffer-ready-hook'.
If some expected sections never report back, the hook still fires
after this many seconds so callers waiting on it can proceed."
  :type 'number
  :group 'shipit)

(defvar shipit-buffer-ready-hook nil
  "Hook run (in the PR buffer) when all pending async sections finish rendering.
Fires exactly once per refresh cycle; `shipit-buffer--reset-pending-sections'
clears the fired flag so subsequent refreshes can fire again.
A function for one buffer only is added with t as the LOCAL argument of
`add-hook', which is its fourth argument.
Another package adds a function here to draw a section of its own at
the end of the buffer.  shipit draws its Pending Review section in this
way, so a DEPTH of 90 in `add-hook' draws a section after that one.")

(defvar-local shipit-buffer--pending-async-sections nil
  "Set of section names the current refresh is still waiting on.
Stored as a list used as a set (membership tested with `memq').")

(defvar-local shipit-buffer--ready-hook-fired nil
  "Non-nil once `shipit-buffer-ready-hook' has fired for the current refresh.
Cleared by `shipit-buffer--reset-pending-sections'.")

(defvar-local shipit-buffer--ready-timeout-timer nil
  "Safety timer that fires `shipit-buffer-ready-hook' after a timeout.")

(defvar-local shipit-buffer--section-ready-callbacks nil
  "Alist of (SECTION . CALLBACKS) waiting for specific sections to render.
Each entry's CALLBACKS is a list of zero-argument functions invoked when
SECTION is marked ready (or when the safety timeout fires).")

(defun shipit-buffer--cancel-ready-timeout ()
  "Cancel the buffer-ready safety timer, if any."
  (when (timerp shipit-buffer--ready-timeout-timer)
    (cancel-timer shipit-buffer--ready-timeout-timer))
  (setq shipit-buffer--ready-timeout-timer nil))

(defun shipit-buffer--run-section-callbacks (section)
  "Pop and invoke all callbacks registered for SECTION."
  (let ((callbacks (alist-get section shipit-buffer--section-ready-callbacks)))
    (when callbacks
      (setf (alist-get section shipit-buffer--section-ready-callbacks nil 'remove) nil)
      (dolist (cb callbacks)
        (condition-case err
            (funcall cb)
          (error
           (shipit--debug-log "section-ready callback errored for %s: %s"
                              section (error-message-string err))))))))

(defun shipit-buffer--fire-ready-hook ()
  "Run `shipit-buffer-ready-hook' once.  No-op if already fired.
Also drains any remaining per-section callbacks so callers waiting on
specific sections aren't stranded if the safety timer triggered us."
  (unless shipit-buffer--ready-hook-fired
    (setq shipit-buffer--ready-hook-fired t)
    (setq shipit-buffer--pending-async-sections nil)
    (shipit-buffer--cancel-ready-timeout)
    ;; Drain any remaining section-specific callbacks.  They get the
    ;; current state of the buffer — if their section never rendered,
    ;; they'll fail to find what they want, but at least they won't hang.
    (let ((sections (mapcar #'car shipit-buffer--section-ready-callbacks)))
      (dolist (section sections)
        (shipit-buffer--run-section-callbacks section)))
    (run-hooks 'shipit-buffer-ready-hook)))

(defun shipit-buffer--reset-pending-sections (sections)
  "Start a new refresh cycle waiting on SECTIONS (a list of symbols).
Cancels any prior safety timer and schedules a new one."
  (shipit-buffer--cancel-ready-timeout)
  (setq shipit-buffer--pending-async-sections (copy-sequence sections))
  (setq shipit-buffer--section-ready-callbacks nil)
  (setq shipit-buffer--ready-hook-fired nil)
  (let ((buf (current-buffer)))
    (setq shipit-buffer--ready-timeout-timer
          (run-with-timer
           shipit-buffer-ready-timeout nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (shipit-buffer--fire-ready-hook))))))))

(defun shipit-buffer--mark-section-ready (section)
  "Mark SECTION as rendered.  Fires section-specific callbacks first,
then the ready hook when the pending set empties.  Ignores unknown or
already-completed sections."
  (when (memq section shipit-buffer--pending-async-sections)
    (setq shipit-buffer--pending-async-sections
          (delq section shipit-buffer--pending-async-sections))
    (shipit-buffer--run-section-callbacks section)
    (unless shipit-buffer--pending-async-sections
      (shipit-buffer--fire-ready-hook))))

(defun shipit-buffer--on-section-ready (section callback)
  "Run CALLBACK when SECTION is rendered in the current buffer.
If SECTION is not in `shipit-buffer--pending-async-sections' (already
rendered, or never expected this cycle), invoke CALLBACK immediately.
Otherwise queue it; `shipit-buffer--mark-section-ready' fires it when
the section completes (or `shipit-buffer--fire-ready-hook' drains it
if the safety timeout triggered)."
  (cond
   ((memq section shipit-buffer--pending-async-sections)
    (push callback (alist-get section shipit-buffer--section-ready-callbacks)))
   (t
    (funcall callback))))

;;; Imenu support

(defconst shipit--imenu-section-types
  '(;; PR header sections
    shipit-pr shipit-description pr-state approval
    ;; People sections
    assignees reviewers
    ;; Metadata sections
    labels pr-refs pr-author pr-created pr-draft shipit-worktree-section
    ;; Content sections
    pr-commits pr-files general-comments inline-comments pr-activity
    ;; Checks sections
    checks in-progress-checks pending-checks failing-checks
    successful-checks cancelled-checks skipped-checks)
  "Section types to include in imenu index.")

(defconst shipit--imenu-child-section-types
  '(;; Individual items within parent sections
    pr-file shipit-commit general-comment activity-event
    label assignee reviewer
    ;; Check status groups
    in-progress-checks pending-checks cancelled-checks
    failing-checks skipped-checks successful-checks
    ;; Workflow names within check status groups
    workflow)
  "Child section types to include as nested imenu entries.")

(defun shipit--imenu-create-index ()
  "Create imenu index from shipit buffer sections.
Returns nil if buffer not yet populated (e.g., during mode initialization).
Creates hierarchical index with child sections nested under parents.

Note: Child entries (files, commits, etc.) only appear for expanded sections.
Expand a section first to include its children in the imenu index."
  (when (and magit-root-section (oref magit-root-section children))
    (let ((index nil))
      (shipit--imenu-collect-sections-hierarchical
       (oref magit-root-section children)
       (lambda (entry) (push entry index)))
      (nreverse index))))

(defun shipit--imenu-collect-sections-hierarchical (sections collect-fn)
  "Collect imenu entries from SECTIONS using COLLECT-FN.
COLLECT-FN is called with an imenu entry (cons or nested alist).
Creates hierarchical entries for sections with interesting children.

When a section is collapsed and has no actual children, this function
checks for `shipit-imenu-children' text property which contains cached
child metadata stored when the section was created."
  (dolist (section sections)
    (when (magit-section-p section)
      (let ((type (oref section type)))
        (cond
         ;; This section matches - collect it (possibly with children)
         ((memq type shipit--imenu-section-types)
          (let* ((start (oref section start))
                 (heading (shipit--imenu-get-heading section))
                 (children (when (slot-boundp section 'children)
                             (oref section children)))
                 (child-entries nil))
            ;; Collect child entries - either from actual children or cached metadata
            ;; First try actual children
            (when children
              (dolist (child children)
                (when (and (magit-section-p child)
                           (memq (oref child type) shipit--imenu-child-section-types))
                  (let ((child-heading (shipit--imenu-get-heading child)))
                    (when (and child-heading (not (string-empty-p child-heading)))
                      (push (cons child-heading (oref child start)) child-entries))))))
            ;; If no children found, check for cached imenu metadata
            ;; (section may be collapsed with lazy-loaded body)
            (if child-entries
                ;; Log when we have real children (from actual section children)
                (shipit--debug-log "IMENU-COLLECT: type=%s has %d REAL children from section"
                                   type (length child-entries))
              ;; No real children, check cache
              (let ((cached-children (and shipit--imenu-children-cache
                                          (gethash type shipit--imenu-children-cache))))
                (shipit--debug-log "IMENU-COLLECT: type=%s cached-children=%s hash-contents=%s"
                                   type
                                   (if cached-children (length cached-children) "nil")
                                   (when shipit--imenu-children-cache
                                     (let (items)
                                       (maphash (lambda (k v) (push (cons k (length v)) items)) shipit--imenu-children-cache)
                                       items)))
                (when cached-children
                  ;; For checks section, build hierarchical entries with workflow names
                  (if (eq type 'checks)
                      (setq child-entries
                            (shipit--imenu-build-checks-hierarchy cached-children start))
                    (setq child-entries cached-children)))))
            ;; Create entry
            (when (and heading (not (string-empty-p heading)))
              (if child-entries
                  ;; Create nested menu with parent as first entry
                  ;; Use reverse (not nreverse) to avoid mutating cached entries
                  (funcall collect-fn
                           (cons heading
                                 (cons (cons heading start)
                                       (reverse child-entries))))
                ;; Simple entry without children
                (funcall collect-fn (cons heading start))))
            ;; Also recurse into children for nested parent sections
            (when children
              (shipit--imenu-collect-sections-hierarchical children collect-fn))))
         ;; Container sections (like root) - just check their children
         (t
          (when (slot-boundp section 'children)
            (shipit--imenu-collect-sections-hierarchical
             (oref section children) collect-fn))))))))

(defun shipit--imenu-build-checks-hierarchy (status-group-entries parent-start)
  "Build hierarchical imenu entries for checks section.
STATUS-GROUP-ENTRIES are the cached entries for status groups.
PARENT-START is the start position of the checks section.
Returns a list of entries where each status group contains its workflow names."
  (let ((result '())
        ;; Map display name prefixes to section type symbols
        (name-to-type '(("In Progress" . in-progress-checks)
                        ("Pending" . pending-checks)
                        ("Cancelled" . cancelled-checks)
                        ("Failing checks" . failing-checks)
                        ("Skipped checks" . skipped-checks)
                        ("Successful checks" . successful-checks))))
    (dolist (entry status-group-entries)
      (let* ((display-name (car entry))
             (position (cdr entry))
             (section-type nil))
        ;; Match display name prefix to find section type
        (dolist (mapping name-to-type)
          (when (string-prefix-p (car mapping) display-name)
            (setq section-type (cdr mapping))))
        ;; Look up cached workflow names for this status group
        (let ((workflow-entries (and section-type
                                      shipit--imenu-children-cache
                                      (gethash section-type shipit--imenu-children-cache))))
          (if workflow-entries
              ;; Create nested entry: status group with workflow children
              (push (cons display-name
                          (cons (cons display-name position)
                                workflow-entries))
                    result)
            ;; No workflow children, just use flat entry
            (push entry result)))))
    (nreverse result)))

(defun shipit--imenu-store-children (section-start child-names &optional section-type)
  "Store CHILD-NAMES as imenu metadata for SECTION-TYPE.
SECTION-START is the section start position (used for navigation).
CHILD-NAMES should be a list of display name strings.
SECTION-TYPE is the section type symbol (e.g., 'pr-files, 'general-comments).
Each entry is stored as (NAME PARENT-POS FUNCTION NAME), the form of
imenu for an entry that has a function of its own: imenu calls
`shipit--imenu-goto-child', which unfolds the parent and finds the
child by its name.  This allows imenu to list children even when the
section is collapsed.  The other entries have a plain place, and imenu
goes to them itself, which lets consult-imenu show them before one is
chosen."
  (shipit--debug-log "IMENU-STORE: section-type=%s children-count=%s (backtrace: %s)"
                     section-type (length child-names)
                     (let ((frames nil))
                       (dotimes (i 10)
                         (let ((frame (backtrace-frame (+ i 4))))
                           (when (and frame (car frame))
                             (push (cadr frame) frames))))
                       (reverse frames)))
  (when (and section-start child-names)
    ;; Initialize cache if needed
    (unless shipit--imenu-children-cache
      (setq shipit--imenu-children-cache (make-hash-table :test 'eq)))
    (let ((entries (mapcar (lambda (name)
                             (list name section-start #'shipit--imenu-goto-child name))
                           child-names)))
      ;; Store in hash table by section type
      (when section-type
        (puthash section-type entries shipit--imenu-children-cache))
      (shipit--debug-log "IMENU-STORE: stored %d children for section-type %s"
                         (length child-names) section-type))))

(defun shipit--imenu-child-start (parent child-name)
  "Where the section in PARENT that CHILD-NAME heads starts, or nil."
  (cl-loop for child in (oref parent children)
           when (equal child-name (shipit--imenu-get-heading child))
           return (oref child start)))

(defun shipit--imenu-text-start (parent child-name)
  "Where CHILD-NAME stands as text in the section PARENT, or nil."
  (save-excursion
    (goto-char (oref parent start))
    (and (search-forward child-name (oref parent end) t)
         (match-beginning 0))))

(defun shipit--imenu-goto-child (_name position child-name)
  "Go to the section that CHILD-NAME heads, in the section at POSITION.
This is the function of an imenu entry for a section that was not drawn
when the index was made: a folded section draws what is in it when it
is unfolded.  So the section at POSITION is unfolded first.  CHILD-NAME
comes with the entry, because a completion UI calls this with a name of
its own making."
  (goto-char position)
  (let ((parent (magit-current-section)))
    (when (oref parent hidden)
      (magit-section-show parent))
    (when-let* ((start (or (shipit--imenu-child-start parent child-name)
                           (shipit--imenu-text-start parent child-name))))
      (goto-char start))))

;;; Keymap

(defvar shipit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "TAB") #'shipit-buffer-toggle-section)
    (define-key map (kbd "<backtab>") #'shipit-buffer-cycle-visibility)
    (define-key map (kbd "n") #'magit-section-forward)
    (define-key map (kbd "p") #'magit-section-backward)

    ;; Actions
    (define-key map (kbd "RET") #'shipit--open-file-at-point)
    (define-key map (kbd "C-RET") #'shipit--open-file-from-worktree-at-point)
    (define-key map [C-return] #'shipit--open-file-from-worktree-at-point)
    (define-key map (kbd "c") #'shipit-comment)
    (define-key map (kbd "r") #'shipit-buffer-refresh)
    (define-key map (kbd "g") #'shipit-buffer-refresh)
    (define-key map (kbd "s") #'shipit-buffer-select-pr)

    ;; Quick actions
    (define-key map (kbd "a") #'shipit-suggestion-or-approve)
    (define-key map (kbd "R") #'shipit-request-changes)
    (define-key map (kbd "f") #'shipit-files-filter)
    (define-key map (kbd "m") #'shipit-toggle-file-viewed)
    (define-key map (kbd "M") #'shipit-mark-all-files-viewed)
    ;; Merge
    (define-key map (kbd "M-m") #'shipit-merge)
    ;; Worktree actions transient (`Z' to mirror magit\\='s
    ;; worktree-popup binding).
    (define-key map (kbd "Z") #'shipit-worktree)
    (define-key map (kbd "+") 'shipit-load-more-files)

    ;; Buffer management
    (define-key map (kbd "Q") #'shipit-buffer-quit-and-kill)

    ;; Copy
    (define-key map (kbd "M-w") #'shipit-buffer-copy-dwim)

    ;; Display toggles
    (define-key map (kbd "L") #'shipit-buffer-toggle-timestamps-dwim)

    ;; Help
    (define-key map (kbd "?") #'shipit-buffer-help)

    map)
  "Keymap for shipit-mode.
The keys it shares with an issue buffer come from its parent,
`shipit-page-mode-map'.")

;; Outside the `defvar', so that loading this file again gives a running
;; Emacs the shared keys too.
(set-keymap-parent shipit-mode-map shipit-page-mode-map)
;; SPC is the menu of the page, from the parent.  It was `shipit-dwim',
;; which a map that was made before still has.
(define-key shipit-mode-map (kbd "SPC") nil t)

(defun shipit-buffer--directory-for (repo)
  "The directory that a pull request buffer of REPO runs git in.
A local clone of REPO when one is known, see `shipit--local-clone-of'.
Else the repository here, or the current directory: a pull request of
a repository without a clone still shows what the forge gives."
  (or (shipit--local-clone-of repo)
      (file-name-as-directory (or (shipit--get-repo-root) default-directory))))

(defun shipit-buffer--directory-for-page (repo backend-id backend-config)
  "The directory of a new pull request buffer of REPO.
That is what `shipit-buffer--directory-for' gives for REPO.
BACKEND-ID and BACKEND-CONFIG, when non-nil, are the backend and its
settings for the buffer, as `shipit-open-pr-buffer' takes them.  The
forge of the buffer is the one that says whether a clone of a fork is
a clone of REPO, and the buffer does not exist yet."
  (let ((shipit-pr-backend (or backend-id shipit-pr-backend))
        (shipit-pr-backend-config (or backend-config shipit-pr-backend-config)))
    (shipit-buffer--directory-for repo)))

(defun shipit-buffer--rerender-general-comments ()
  "Draw the general comments of this pull request again, in the order set.
They are drawn from what the buffer holds."
  (shipit--refresh-general-comments-section-targeted
   shipit-buffer-repo shipit-buffer-pr-number))

(transient-define-prefix shipit-pr-menu ()
  "The commands of a pull request buffer, each on the key the buffer has for it.
SPC closes the menu again."
  [["Review"
    ("c" "Comment" shipit-comment)
    ("a" "Approve / apply suggestion" shipit-suggestion-or-approve)
    ("R" "Request changes" shipit-request-changes)
    ("k" "Delete comment" shipit-delete-comment-at-point)
    ("S" "Submit review" shipit-review-submit)
    ("K" "Discard drafts" shipit-review-queue-discard)
    ("M-m" "Merge" shipit-merge)
    ("M-;" "Actions at point" shipit-dwim)]
   ["Files"
    ("e" "Ediff file" shipit--ediff-file-at-point)
    ("d" "Diff file" shipit-pr-diff-menu)
    ("D" "Diff with comments" shipit--show-diff-with-comments-at-point)
    ("f" "Filter files" shipit-files-filter)
    ("m" "Toggle viewed" shipit-toggle-file-viewed)
    ("M" "Toggle all viewed" shipit-mark-all-files-viewed)
    ("+" "Load more files" shipit-load-more-files)
    ("Z" "Worktree" shipit-worktree)]
   ["Page"
    ("g" "Refresh" shipit-buffer-refresh)
    ("G" "Hard refresh" shipit-buffer-refresh-hard)
    ("s" "Select PR" shipit-buffer-select-pr)
    ("L" "Toggle time format" shipit-buffer-toggle-timestamps-dwim)
    ("O" "Toggle sort order" shipit-toggle-section-order)
    ("v" "Next view" shipit-page-view-next)
    ("V" "Choose view" shipit-page-view-choose)
    ("M-w" "Copy URL or text" shipit-buffer-copy-dwim)
    ("o" "Open in browser" shipit-open-in-browser)
    ("w" "Repo subscription" shipit-repo-subscription)
    ("$" "Process buffer" shipit-process-buffer)
    ("?" "Help" shipit-buffer-help)
    ("q" "Quit" quit-window)
    ("Q" "Quit and kill" shipit-buffer-quit-and-kill)
    ("SPC" "Close" transient-quit-one)]])

;; meow takes SPC in its motion state.  The menu gets it in the insert
;; state.
(with-eval-after-load 'meow
  (shipit--start-in-meow-insert-state 'shipit-mode))

;;; Major mode definition

;;;###autoload
(define-derived-mode shipit-mode magit-section-mode "Shipit"
  "Major mode for shipit PR buffers.
Provides a dedicated interface for reviewing pull requests
with focused context and optimized performance.

\\{shipit-mode-map}"
  :group 'shipit
  (shipit--watch-videos)
  (setq-local shipit-page-url-function #'shipit-buffer--pr-url)
  (setq-local shipit-page-menu-command #'shipit-pr-menu)
  (setq-local shipit--comments-rerender-function
              #'shipit-buffer--rerender-general-comments)
  (shipit-page-view-setup :pr)
  (setq-local shipit-page-view-redraw-function #'shipit-pr-overview-redraw)
  ;; The summary and the waiting part need each answer of the forge.
  (add-hook 'shipit-buffer-ready-hook #'shipit-page-view-apply-later nil t)

  ;; Re-bind keys to ensure they override parent bindings
  (define-key shipit-mode-map (kbd "RET") #'shipit--open-file-at-point)
  (define-key shipit-mode-map (kbd "C-RET") #'shipit--open-file-from-worktree-at-point)
  (define-key shipit-mode-map [C-return] #'shipit--open-file-from-worktree-at-point)
  (define-key shipit-mode-map (kbd "e") #'shipit--ediff-file-at-point)
  (define-key shipit-mode-map (kbd "d") #'shipit-pr-diff-menu)
  (define-key shipit-mode-map (kbd "D") #'shipit--show-diff-with-comments-at-point)
  (define-key shipit-mode-map (kbd "g") #'shipit-buffer-refresh)
  (define-key shipit-mode-map (kbd "G") #'shipit-buffer-refresh-hard)
  (define-key shipit-mode-map (kbd "f") #'shipit-files-filter)
  (define-key shipit-mode-map (kbd "+") 'shipit-load-more-files)
  (define-key shipit-mode-map (kbd "k") #'shipit-delete-comment-at-point)
  (define-key shipit-mode-map (kbd "S") #'shipit-review-submit)
  (define-key shipit-mode-map (kbd "K") #'shipit-review-queue-discard)
  (define-key shipit-mode-map (kbd "T f") #'shipit-pr-toggle-fontify-hunks)
  (define-key shipit-mode-map (kbd "T r") #'shipit-pr-toggle-refine-hunks)

  (setq-local revert-buffer-function #'shipit-buffer-refresh)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  ;; Match GitHub's tab rendering (8 spaces per tab)
  (setq-local tab-width 8)
  ;; Initialize magit-root-section to nil - will be set by first magit-insert-section call
  (setq-local magit-root-section nil)
  (shipit--apply-section-defaults)
  (shipit--enable-issue-ref-eldoc)
  ;; Enable font-lock for syntax highlighting in code sections
  (font-lock-mode 1)
  ;; Set up imenu for section navigation (following magit-mode pattern)
  (setq-local imenu-create-index-function #'shipit--imenu-create-index)
  (add-hook 'imenu-after-jump-hook #'shipit--imenu-unfold-around-point nil t)
  ;; Adjust magit-section-highlight to not conflict with code block backgrounds
  (shipit--update-section-highlight-face)
  ;; Suppress highlight on sections with colored content (labels, etc.)
  (add-hook 'magit-section-highlight-hook
            #'shipit--suppress-highlight-for-colored-sections nil t)
  ;; Add hook for marking comments as read when cursor visits them
  (add-hook 'post-command-hook #'shipit--comment-read-post-command-hook nil t))

(defvar-local shipit--section-highlight-remap-cookie nil
  "Cookie for the face-remap of magit-section-highlight in this buffer.")

(defun shipit--suppress-highlight-for-colored-sections (section)
  "Suppress magit-section highlight on sections with colored content.
Returns t if highlight was handled (suppressed), nil otherwise."
  (when (memq (oref section type) '(label labels))
    ;; Remove any existing highlight overlays on this section
    (let ((start (oref section start))
          (end (oref section end)))
      (dolist (ov (overlays-in start end))
        (when (overlay-get ov 'font-lock-face)
          (delete-overlay ov))))
    t))

(defun shipit--update-section-highlight-face ()
  "Update magit-section-highlight face to match current theme.
Use a subtle highlight - lighten for dark themes, darken for light themes.
No-op in batch mode where face remapping is unavailable."
  (when (and (display-graphic-p)
             (fboundp 'face-remap-add-relative))
    ;; Remove old remapping if present
    (when shipit--section-highlight-remap-cookie
      (face-remap-remove-relative shipit--section-highlight-remap-cookie)
      (setq shipit--section-highlight-remap-cookie nil))
    ;; Add new remapping based on current theme
    (let* ((base-bg (face-background 'default nil 'default))
           (subtle-bg (when (and base-bg (not (string-prefix-p "unspecified" base-bg)))
                        (let* ((rgb (color-name-to-rgb base-bg))
                               (luminance (+ (* 0.2126 (nth 0 rgb))
                                             (* 0.7152 (nth 1 rgb))
                                             (* 0.0722 (nth 2 rgb)))))
                          (shipit--adjust-color-brightness base-bg
                                                           (if (< luminance 0.5) 0.04 -0.04))))))
      (when subtle-bg
        (setq shipit--section-highlight-remap-cookie
              (face-remap-add-relative 'magit-section-highlight :background subtle-bg))))))

;;; Buffer management

(defun shipit--refresh-all-buffers-on-theme-change (&optional _theme)
  "Refresh all shipit buffers when theme changes.
This updates face remappings and code block overlays to match new theme colors.
THEME argument is ignored (provided by `enable-theme-functions')."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (eq (buffer-local-value 'major-mode buf) 'shipit-mode))
      (with-current-buffer buf
        ;; Update section highlight face for new theme
        (shipit--update-section-highlight-face)
        ;; Refresh buffer content to update code block overlays
        (shipit-buffer-refresh nil)))))

;; Auto-refresh shipit buffers when theme changes
(add-hook 'enable-theme-functions #'shipit--refresh-all-buffers-on-theme-change)

(defun shipit-buffer-name (repo pr-number)
  "Generate buffer name for REPO and PR-NUMBER."
  (format "*shipit: %s#%s*" repo pr-number))

;;;###autoload
(defun shipit-open-pr-buffer (pr-number &optional repo backend-id backend-config)
  "Open dedicated shipit buffer for PR-NUMBER in REPO.
If REPO is nil, detect from current git repository.
If buffer already exists, switch to it.
BACKEND-ID, when non-nil, overrides `shipit-pr-backend' for this buffer.
BACKEND-CONFIG, when non-nil, overrides `shipit-pr-backend-config'."
  (interactive
   (list (read-number "PR number: ")
         (shipit--repo-for-buffer)))
  (let* ((repo (or repo (shipit--repo-for-buffer)))
         (buffer-name (shipit-buffer-name repo pr-number))
         (existing-buffer (get-buffer buffer-name)))

    (cond
     ;; Buffer already exists - switch to it
     (existing-buffer
      (shipit--debug-log "Switching to existing shipit buffer: %s" buffer-name)
      ;; Ensure global repository context is set when switching to existing buffer
      (with-current-buffer existing-buffer
        (setq shipit-buffer-pr-number pr-number
              shipit-buffer-repo repo)
        (setq shipit-current-repo repo)
        ;; Update backend override if provided
        (when backend-id
          (setq shipit-buffer-backend-id backend-id)
          (setq shipit-buffer-backend-config backend-config)
          (setq-local shipit-pr-backend backend-id)
          (setq-local shipit-pr-backend-config backend-config)))
      (shipit-buffer-display existing-buffer)
      existing-buffer)

     ;; Create new buffer
     (t
      (shipit--debug-log "Creating new shipit buffer: %s (backend: %s)" buffer-name (or backend-id "default"))
      (let ((buffer (generate-new-buffer buffer-name))
            (repo-root (shipit-buffer--directory-for-page repo backend-id backend-config)))
        (with-current-buffer buffer
          (shipit-mode)
          ;; Set default-directory to repo root for git commands
          (setq default-directory repo-root)
          (setq shipit-buffer-pr-number pr-number
                shipit-buffer-repo repo)
          ;; Set global repository context for shipit functions
          (setq shipit-current-repo repo)
          ;; Store backend info as dedicated buffer-local vars (survive mode changes)
          ;; When no explicit backend-id, eagerly detect from remote URL
          ;; so we never re-detect from potentially wrong default-directory later.
          (let ((effective-backend-id (or backend-id
                                         (shipit--detect-backend-from-remote))))
            (setq shipit-buffer-backend-id effective-backend-id)
            (setq shipit-buffer-backend-config backend-config)
            ;; Override the defcustom for backend dispatch in this buffer
            (setq-local shipit-pr-backend effective-backend-id)
            (when backend-config
              (setq-local shipit-pr-backend-config backend-config)))
          ;; Initial buffer setup
          (shipit-buffer-refresh))
        (shipit-buffer-display buffer)
        buffer)))))

(defun shipit-buffer-display (buffer)
  "Display shipit BUFFER according to configuration."
  (cond
   ;; Side-by-side with current window
   (shipit-buffer-side-by-side
    (pop-to-buffer buffer '(display-buffer-in-side-window
                            (side . right)
                            (window-width . 0.5))))
   ;; Replace current buffer
   (t
    (switch-to-buffer buffer))))

(defun shipit-buffer-quit-and-kill ()
  "Quit shipit buffer and kill it."
  (interactive)
  (let ((buffer (current-buffer)))
    (quit-window)
    (kill-buffer buffer)))

;;; PR Sections - Independent sections like original format

(defun shipit--get-commits-behind-base (repo base-ref head-ref)
  "Get the number of commits HEAD-REF is behind BASE-REF via backend.
REPO is the repository (owner/repo format).
Returns nil if unable to determine, 0 if up-to-date, or a positive number."
  (condition-case nil
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (response (funcall (plist-get backend :fetch-compare) config base-ref head-ref)))
        (when response
          (cdr (assq 'behind_by response))))
    (error nil)))

(defun shipit--fetch-out-of-date-async (repo base-ref head-ref target-buffer)
  "Fetch compare data asynchronously and update refs section if behind.
REPO, BASE-REF, HEAD-REF identify the comparison.
TARGET-BUFFER is the shipit buffer to update."
  (run-at-time 0 nil
               (lambda ()
                 (let ((commits-behind (shipit--get-commits-behind-base repo base-ref head-ref)))
                   (when (and commits-behind (> commits-behind 0)
                              (buffer-live-p target-buffer))
                     (with-current-buffer target-buffer
                       (let ((inhibit-read-only t))
                         (save-excursion
                           ;; Find the refs line by text property
                           (goto-char (point-min))
                           (let ((pos (text-property-any (point-min) (point-max) 'shipit-refs-line t)))
                             (when pos
                               (goto-char pos)
                               (end-of-line)
                               (insert (propertize " [out-of-date]"
                                                   'font-lock-face '(:foreground "orange")
                                                   'shipit-refs-line t))))))))))))

(defun shipit--pr-type-label ()
  "Return the PR type label from the active backend.
Falls back to \"Pull Request\" when the backend has no :pr-type-label."
  (or (plist-get (shipit-pr--get-backend) :pr-type-label)
      "Pull Request"))

(defun shipit--insert-pr-title-section (repo pr-data pr-number)
  "Insert just the PR title section."
  (magit-insert-section (shipit-pr (number-to-string (or pr-number 0)))
    (let ((header-start (point)))
      (magit-insert-heading
        (let ((title-raw (cdr (assq 'title pr-data))))
          (format "%s %s %s #%s: %s"
                  (shipit--get-notification-source-icon (shipit-pr--backend-id))
                  (shipit--get-pr-field-icon "pull-request" "🔀")
                  (propertize (shipit--pr-type-label) 'font-lock-face 'magit-section-heading)
                  (or pr-number "unknown")
                  (string-trim (or (shipit--clean-text title-raw) "No title")))))
      ;; Apply interactive properties to PR header (using original keymap if available)
      (add-text-properties header-start (point)
                           `(shipit-pr-header t
                                              shipit-repo ,repo
                                              shipit-pr-number ,pr-number
                                              keymap ,(if (boundp 'shipit-pr-header-keymap) shipit-pr-header-keymap nil)
                                              
                                              help-echo "s: select different PR, M-;: shipit-dwim")))))

;; Create overlays for PR references, commit SHAs, user mentions, and code blocks in description
(defun shipit--decorate-description (repo pr-number start end)
  "Give the description between START and END its link overlays and faces.
REPO and PR-NUMBER are those of the pull request, and PR-NUMBER is nil
in the preview of a pull request that does not exist yet.  The code
block pass can insert pictures, so a marker follows END for the pass
after it.  The passes get the positions as numbers, as their debug
lines format them with %d."
  (let ((end-after-pictures (copy-marker end)))
    (when (fboundp 'shipit--create-pr-reference-overlays)
      (shipit--create-pr-reference-overlays repo pr-number start end))
    (when (fboundp 'shipit--create-commit-sha-overlays)
      (shipit--create-commit-sha-overlays repo start end))
    (when (fboundp 'shipit--create-user-mention-overlays)
      (shipit--create-user-mention-overlays repo start end))
    (when (fboundp 'shipit--create-jira-mention-overlays)
      (shipit--create-jira-mention-overlays start end))
    (when (fboundp 'shipit--create-custom-url-overlays)
      (shipit--create-custom-url-overlays start end))
    (when (fboundp 'shipit--create-generic-url-overlays)
      (shipit--create-generic-url-overlays start end))
    (when (fboundp 'shipit--apply-code-block-backgrounds-in-region)
      (shipit--apply-code-block-backgrounds-in-region start end))
    (when (fboundp 'shipit--apply-strikethrough-faces)
      (shipit--apply-strikethrough-faces start (marker-position end-after-pictures)))
    (set-marker end-after-pictures nil)))

(defun shipit--insert-pr-description-section (repo pr-data pr-number)
  "Insert the PR description section using original magit integration logic.
REPO is the repository name (owner/repo format)."
  (let* ((raw-body (cdr (assq 'body pr-data)))
         (clean-body (when (and raw-body (not (string-empty-p raw-body)))
                       (let ((cleaned (shipit--clean-text raw-body)))
                         (unless (string-match-p "\\`[[:space:]]*\\'" cleaned)
                           cleaned)))))
    (magit-insert-section (shipit-description nil nil)
      (let ((header-start (point)))
        (magit-insert-heading (format "%s %s"
                                      (shipit--get-pr-field-icon "description" "📝")
                                      (propertize "Description:" 'font-lock-face 'markdown-metadata-key-face)))
        (add-text-properties header-start (point)
                             `(shipit-pr-description t
                                                     shipit-pr-number ,pr-number
                                                     shipit-pr-body ,clean-body
                                                     shipit-repo ,repo)))
      (magit-insert-section-body
        (let* ((description-start (point))
               (is-empty (not clean-body))
               ;; Cap inline images to the 80-col wrap envelope used by
               ;; the rest of the description body.  Measured here in
               ;; the destination buffer's face so the renderer's
               ;; `with-temp-buffer' default doesn't shrink it.
               (shipit--image-target-width
                (if (fboundp 'string-pixel-width)
                    (string-pixel-width (make-string 80 ?M))
                  (* 80 (frame-char-width)))))
        (if is-empty
            (insert (propertize "   No description provided\n" 'font-lock-face 'italic))
          (shipit--insert-body-with-details clean-body 3 #'shipit--insert-markdown-text))
        (shipit--decorate-description repo pr-number description-start (point))
        (add-text-properties description-start (point)
                             `(shipit-pr-description t
                                                     shipit-pr-number ,pr-number
                                                     shipit-pr-body ,clean-body
                                                     shipit-repo ,repo))
        ;; Reactions block (blank line + reactions + blank line)
        (shipit--insert-description-reactions
         pr-number repo
         `(shipit-pr-description t
           shipit-pr-number ,pr-number
           shipit-pr-body ,clean-body
           shipit-repo ,repo)))))))

;;; Buffer content rendering

(defun shipit-buffer-refresh-hard ()
  "Hard refresh: clear ALL caches including ETags and fetch fresh data from GitHub."
  (interactive)
  (unless (derived-mode-p 'shipit-mode)
    (user-error "Not in a shipit buffer"))

  (message "Hard refresh: clearing all caches and fetching fresh data...")
  (shipit--debug-log "🔄 HARD REFRESH: Clearing ALL caches including ETags")

  ;; Clear ALL caches including ETags - fail fast if these don't exist
  (shipit-clear-all-caches)
  (shipit-gh-etag-clear-all)

  ;; Clear merge methods cache for this repo
  (when (and (fboundp 'shipit--merge-methods-cache-clear)
             (bound-and-true-p shipit-buffer-repo))
    (shipit--merge-methods-cache-clear shipit-buffer-repo))

  ;; Clear comment caches
  (when (boundp 'shipit--cached-general-comments)
    (setq shipit--cached-general-comments nil))
  (when (boundp 'shipit--cached-inline-comments)
    (setq shipit--cached-inline-comments nil))

  ;; Now do a normal refresh which will fetch fresh data
  (shipit-buffer-refresh))

(defun shipit--buffer-refresh-incremental (repo pr-number)
  "Refresh buffer using incremental approach - render placeholders, then fetch.
REPO is the repository, PR-NUMBER is the PR number."
  (shipit--debug-log "INCREMENTAL: Starting incremental refresh for PR #%s" pr-number)

  ;; Phase 1: Fetch basic PR data (this is fast - just the PR metadata)
  (let ((pr-data (shipit--time-operation "fetch basic PR data"
                   (shipit-get-pull-request pr-number repo))))
    (when pr-data
      (setq shipit-buffer-pr-data pr-data)

      ;; Phase 2: Render immediate sections (title, description, labels, etc.)
      ;; These only need the basic PR data - NO API CALLS here
      (shipit--debug-log "INCREMENTAL: Rendering immediate sections (no API calls)")

      (shipit-pr-overview-insert-parts)
      (shipit--time-operation "insert title section"
        (shipit--insert-pr-title-section repo pr-data pr-number))

      ;; PR reactions: render the description with whatever is in the
      ;; local cache right now (cheap), then kick off an async refetch
      ;; that updates just the reactions line in place when the
      ;; response arrives.  Avoids a ~150ms sync HTTP block on every
      ;; first-time PR open.  `shipit--update-pr-reactions-display'
      ;; locates the reactions line via text properties and replaces
      ;; only that line, so the rest of the buffer keeps its scroll /
      ;; point position.
      (shipit--time-operation "kick off async PR reactions fetch"
        (shipit--fetch-pr-reactions-async-with-callback
         repo pr-number
         (lambda (_reactions)
           (shipit--update-pr-reactions-display pr-number))))

      ;; Fetch inline comments early so file comment counts are available
      (when (condition-case nil (shipit-comment--get-backend) (error nil))
        (shipit--time-operation "fetch inline comments"
          (let ((head-sha (cdr (assq 'sha (cdr (assq 'head pr-data))))))
            (shipit--fetch-inline-comments repo pr-number nil head-sha))))

      (shipit--time-operation "insert description section"
        (shipit--insert-pr-description-section repo pr-data pr-number))

      ;; Insert metadata sections from pr-data (no API calls needed)
      ;; Author section
      (shipit--time-operation "insert author section"
        (let* ((user-obj (cdr (assq 'user pr-data)))
               (user (or (cdr (assq 'login user-obj)) "Unknown"))
               (avatar-url (cdr (assq 'avatar_url user-obj))))
          (magit-insert-section (pr-author nil)
            (magit-insert-heading (format "%s Author:    %s%s"
                            (shipit--get-pr-field-icon "author" "👤")
                            (if (and (boundp 'shipit-show-avatars) shipit-show-avatars
                                     (fboundp 'shipit--create-avatar-display))
                                (concat (shipit--create-avatar-display user avatar-url 16) " ")
                              "")
                            (propertize user 'font-lock-face 'shipit-username-face))))))

      ;; Created date section
      (shipit--time-operation "insert created section"
        (let* ((created (or (cdr (assq 'created_at pr-data)) ""))
               (formatted-date (if (and (fboundp 'shipit--format-timestamp)
                                        (not (string-empty-p created)))
                                   (shipit--format-timestamp created)
                                 created)))
          (magit-insert-section (pr-created nil)
            (magit-insert-heading (format "%s Created:   %s"
                            (shipit--get-pr-field-icon "created" "📅")
                            (propertize formatted-date 'font-lock-face 'shipit-timestamp-face))))))

      ;; Refs/branch section — rendered immediately, out-of-date fetched async
      (shipit--time-operation "insert refs section"
        (let* ((head-ref (cdr (assq 'ref (cdr (assq 'head pr-data)))))
               (base-ref (cdr (assq 'ref (cdr (assq 'base pr-data)))))
               (refs-line (if (and base-ref head-ref)
                              (concat (propertize base-ref 'font-lock-face 'magit-branch-local)
                                      " ← "
                                      (propertize head-ref 'font-lock-face 'magit-branch-remote))
                            "unknown")))
          (magit-insert-section (pr-refs nil)
            (let ((line-start (point)))
              (magit-insert-heading (format "%s Refs:      %s"
                              (shipit--get-pr-field-icon "refs" "🔀")
                              refs-line))
              (put-text-property line-start (point) 'shipit-refs-line t)))))

      ;; Linked issue section (auto-detect from branch, manual override via RET)
      (shipit--time-operation "insert linked issue"
        (shipit-pr-linked-issue--insert-section repo pr-number pr-data))

      ;; Insert simple state line from pr-data (no API call)
      (shipit--time-operation "insert simple state"
        (magit-insert-section (pr-state nil)
          (magit-insert-heading (shipit--pr-state-line pr-data))))

      ;; Insert subscription status (if backend supports it)
      (shipit--time-operation "insert subscription"
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (fn (plist-get backend :get-repo-subscription)))
          (when fn
            (require 'shipit-repo-buffer)
            (let* ((sub-data (funcall fn (cdr resolved)))
                   (state (shipit--subscription-state-from-api sub-data))
                   (label (shipit--subscription-state-label state))
                   (star-fn (plist-get backend :get-repo-starred))
                   (starred (when star-fn (funcall star-fn (cdr resolved)))))
              (magit-insert-section (pr-subscription nil)
                (magit-insert-heading
                  (propertize (format "%s Watching:  %s%s"
                                      (shipit--get-pr-field-icon "notification" "\U0001f514")
                                      label
                                      (shipit--star-indicator starred))
                              'shipit-repo-subscription t)))))))

      (shipit--time-operation "insert URL section"
        (shipit--insert-pr-url-section pr-data))

      (shipit--time-operation "insert stack section"
        (shipit--insert-stack-section repo pr-data))

      ;; Insert approval placeholder (actual data requires API call)
      (shipit--time-operation "insert approval placeholder"
        (magit-insert-section (approval nil t)
          (magit-insert-heading
            (format "%s Approval (loading...)" (shipit--get-pr-field-icon "approval" "❓")))
          (magit-insert-section-body
            (insert "   Fetching review status...\n")
            (insert "\n"))))

      (shipit--time-operation "insert assignees section"
        (shipit--insert-assignees-section repo pr-data))

      (shipit--time-operation "insert labels section"
        (shipit--insert-labels-section pr-data repo))

      (shipit--time-operation "insert worktree section"
        (shipit--insert-worktree-section pr-data))

      ;; Phase 3: Insert placeholder sections for heavy data
      (shipit--debug-log "INCREMENTAL: Inserting placeholder sections")
      (shipit--time-operation "insert commits placeholder"
        (shipit--insert-commits-section-placeholder repo pr-number))

      (shipit--time-operation "insert files placeholder"
        (shipit--insert-files-section-placeholder repo pr-number))

      (shipit--time-operation "insert comments placeholder"
        (shipit--insert-general-comments-section-placeholder repo pr-number))

      ;; Activity section also needs placeholder - it fetches its own data which can be slow
      (shipit--time-operation "insert activity placeholder"
        (shipit--insert-activity-section-placeholder repo pr-number))

      ;; Checks section also needs placeholder - it makes API calls
      (shipit--time-operation "insert checks placeholder"
        (shipit--insert-checks-section-placeholder repo pr-number))

      ;; Mark notification as read
      (when (and (fboundp 'shipit--mark-notification-read)
                 (boundp 'shipit--notification-pr-activities)
                 shipit--notification-pr-activities)
        (condition-case nil
            (shipit--mark-notification-read pr-number repo t)
          (error nil)))

      ;; Phase 4: Start async data fetches to replace placeholders
      (shipit--debug-log "INCREMENTAL: Starting async data fetches")

      ;; Track which sections are still loading; hook fires when set empties.
      (shipit-buffer--reset-pending-sections
       '(general-comments approval activity commits files checks))

      ;; Capture shared state for async callbacks
      (let ((target-buffer (current-buffer))
            (pr-data-copy pr-data)
            ;; Capture root-section for async callback - buffer-local may not be bound
            (root-section magit-root-section))
        (shipit--debug-log "INCREMENTAL: Captured root-section=%s for async callback"
                           (if root-section "yes" "nil"))

        ;; Fetch out-of-date status async — skip for merged/closed MRs (always 404)
        (let ((state (shipit--get-pr-actual-state pr-data)))
          (unless (member state '("merged" "closed"))
            (let ((head-ref (cdr (assq 'ref (cdr (assq 'head pr-data)))))
                  (base-ref (cdr (assq 'ref (cdr (assq 'base pr-data))))))
              (shipit--fetch-out-of-date-async repo base-ref head-ref target-buffer))))

        ;; Fetch general comments async
        (if (condition-case nil (shipit-comment--get-backend) (error nil))
            (shipit--fetch-general-comments-async
             repo pr-number
             (lambda (comments)
               (shipit--debug-log "INCREMENTAL: General comments async callback with %d comments" (length comments))
               (when (buffer-live-p target-buffer)
                 (with-current-buffer target-buffer
                   (let ((magit-root-section (or magit-root-section root-section)))
                     ;; Check for new comments and establish baseline
                     (shipit--check-for-new-general-comments repo pr-number comments)
                     ;; A comment posted in the page joins this cache, and
                     ;; the section is drawn again from it.
                     (shipit--cache-general-comments-with-dedup comments)
                     ;; Render comments immediately — reactions available on next refresh
                     (shipit--replace-general-comments-section-with-content
                      repo pr-number comments)
                     (shipit-buffer--mark-section-ready 'general-comments)
                     ;; Dispatch reaction requests truly non-blocking so
                     ;; buffer-ready callbacks (e.g., activity navigation
                     ;; scheduled by the notifications buffer) can fire
                     ;; without waiting on HTTP.  Reactions display on the
                     ;; next refresh.
                     (when comments
                       (let ((active-comments (cl-remove-if (lambda (c) (cdr (assq 'outdated c))) comments)))
                         (when active-comments
                           (shipit-comment--fetch-reactions-batch
                            active-comments repo nil
                            (lambda () nil))))))))))
          ;; Non-GitHub: clear placeholder with empty comments
          (shipit--replace-general-comments-section-with-content repo pr-number nil)
          (shipit-buffer--mark-section-ready 'general-comments))

        ;; Backend dispatch for review decision, timeline, commits, files
        ;; Prefer async when backend provides it, fall back to sync.
        (let* ((resolved (shipit-pr--resolve-for-repo repo))
               (backend (car resolved))
               (config (cdr resolved)))

          ;; Phase 1: Fetch ALL sync data BEFORE any replacements
          ;; This prevents re-entrant async callbacks during sync HTTP calls
          (let* ((review-async-fn (plist-get backend :fetch-review-decision-async))
                 (timeline-async-fn (plist-get backend :fetch-timeline-async))
                 (commits-async-fn (plist-get backend :fetch-commits-async))
                 (files-async-fn (plist-get backend :fetch-files-async))
                 ;; Fetch sync data upfront (these HTTP calls may trigger event processing)
                 (sync-review-info (unless review-async-fn
                                     (let ((fn (plist-get backend :fetch-review-decision)))
                                       (when fn (funcall fn config pr-number)))))
                 (sync-events (unless timeline-async-fn
                                (let ((fn (plist-get backend :fetch-timeline)))
                                  (when fn (funcall fn config pr-number)))))
                 (sync-commits (unless commits-async-fn
                                 (let ((fn (plist-get backend :fetch-commits)))
                                   (when fn (funcall fn config pr-number)))))
                 (sync-files (unless files-async-fn
                               (let ((fn (plist-get backend :fetch-files)))
                                 (when fn (funcall fn config pr-number))))))

            ;; Phase 2: Do all sync replacements (no HTTP calls, no re-entrancy)
            (unless review-async-fn
              (shipit--replace-approval-section-with-content repo pr-number sync-review-info)
              (shipit-buffer--mark-section-ready 'approval))
            (unless timeline-async-fn
              (shipit--replace-activity-section-with-content repo pr-data-copy pr-number sync-events)
              (shipit-buffer--mark-section-ready 'activity))
            (unless commits-async-fn
              (shipit--replace-commits-section-with-content repo pr-number sync-commits)
              (shipit-buffer--mark-section-ready 'commits))
            (unless files-async-fn
              (when shipit-buffer-pr-data
                (setf (alist-get 'files shipit-buffer-pr-data) sync-files))
              (shipit--replace-files-section-with-content repo pr-data-copy pr-number sync-files nil)
              (shipit-buffer--mark-section-ready 'files))

            ;; Phase 3: Fire async fetches (callbacks will replace when data arrives)
            (when review-async-fn
              (funcall review-async-fn config pr-number
                       (lambda (review-info)
                         (shipit--debug-log "INCREMENTAL: Review decision async callback")
                         (when (buffer-live-p target-buffer)
                           (with-current-buffer target-buffer
                             (let ((magit-root-section (or magit-root-section root-section)))
                               (shipit--replace-approval-section-with-content
                                repo pr-number review-info))
                             (shipit-buffer--mark-section-ready 'approval))))))
            (when timeline-async-fn
              (funcall timeline-async-fn config pr-number
                       (lambda (events)
                         (shipit--debug-log "INCREMENTAL: Timeline async callback, %d events" (length events))
                         (when (buffer-live-p target-buffer)
                           (with-current-buffer target-buffer
                             (let ((magit-root-section (or magit-root-section root-section)))
                               (shipit--replace-activity-section-with-content
                                repo pr-data-copy pr-number events))
                             (shipit-buffer--mark-section-ready 'activity))))))
            (when commits-async-fn
              (funcall commits-async-fn config pr-number
                       (lambda (commits)
                         (shipit--debug-log "INCREMENTAL: Commits async callback, %d commits" (length commits))
                         (when (buffer-live-p target-buffer)
                           (with-current-buffer target-buffer
                             (let ((magit-root-section (or magit-root-section root-section)))
                               (shipit--replace-commits-section-with-content
                                repo pr-number commits))
                             (shipit-buffer--mark-section-ready 'commits))))))
            (when files-async-fn
              (funcall files-async-fn config pr-number
                       (lambda (result)
                         (let ((files (car result))
                               (truncated (cdr result)))
                           (shipit--debug-log "INCREMENTAL: Files async callback, %d files (truncated=%s)"
                                              (length files) truncated)
                           (when (buffer-live-p target-buffer)
                             (with-current-buffer target-buffer
                               (when shipit-buffer-pr-data
                                 (setf (alist-get 'files shipit-buffer-pr-data) files))
                               (setq shipit-buffer-files-truncated truncated)
                               (setq shipit-buffer-files-page 10)
                               (let ((magit-root-section (or magit-root-section root-section)))
                                 (shipit--replace-files-section-with-content
                                  repo pr-data-copy pr-number files truncated)
                                 (shipit-buffer--mark-section-ready 'files))))))))))

        ;; Fetch file viewed states async — independent of the files
        ;; section replacement to avoid re-entrant sync GraphQL calls.
        ;; The states are stored buffer-locally and used on next render.
        (when (and repo pr-number)
          (shipit--fetch-file-viewed-states-async
           repo pr-number target-buffer))

        ;; Fetch resolved review-threads async via GraphQL.  The
        ;; incremental path doesn\='t fetch these by default --
        ;; only the older `parallel-sync' path did -- so the
        ;; `[RESOLVED]' suffix on inline comments was always
        ;; missing.  Fire in parallel with the other fetches and
        ;; on callback (a) cache, (b) invalidate the resolved-
        ;; comments hash, (c) re-render the files section if its
        ;; data is already in `shipit-buffer-pr-data' so the
        ;; resolved markers appear without a manual refresh.
        (when (and repo pr-number)
          (let* ((owner-repo (split-string repo "/"))
                 (owner (car owner-repo))
                 (repo-name (cadr owner-repo)))
            (when (and owner repo-name (fboundp 'shipit--fetch-resolved-threads-graphql))
              (shipit--fetch-resolved-threads-graphql
               owner repo-name pr-number
               (lambda (threads)
                 (shipit--debug-log
                  "INCREMENTAL: Resolved threads callback with %d threads"
                  (if (sequencep threads) (length threads) 0))
                 (when (buffer-live-p target-buffer)
                   (with-current-buffer target-buffer
                     (setq shipit--cached-resolved-threads
                           (or threads '()))
                     (setq shipit--resolved-comments-hash nil)
                     (shipit-page-view-redraw)
                     ;; Re-render the files section if files data
                     ;; landed already.  When resolved-threads
                     ;; arrives first, the next files render reads
                     ;; the populated cache automatically.
                     (let ((files (and shipit-buffer-pr-data
                                       (alist-get 'files
                                                  shipit-buffer-pr-data))))
                       (when files
                         (let ((magit-root-section
                                (or magit-root-section root-section)))
                           (shipit--replace-files-section-with-content
                            repo pr-data-copy pr-number files
                            shipit-buffer-files-truncated)))))))))))

        ;; Fetch checks async - pass pr-data to avoid extra API call
        (shipit--fetch-checks-async
         repo pr-number pr-data-copy
         (lambda (checks)
           (shipit--debug-log "INCREMENTAL: Checks async callback with %s"
                              (cond ((eq checks 'closed) "closed PR")
                                    ((null checks) "disabled/nil")
                                    (t (format "%d checks" (length checks)))))
           (when (buffer-live-p target-buffer)
             (with-current-buffer target-buffer
               (let ((magit-root-section (or magit-root-section root-section)))
                 (shipit--replace-checks-section-with-content
                  repo pr-number checks)
                 (shipit-buffer--mark-section-ready 'checks))))))))))

(defun shipit-load-more-files ()
  "Load more files if the file list was truncated.
Fetches additional pages of files and appends them to the existing list."
  (interactive)
  (unless (derived-mode-p 'shipit-mode)
    (user-error "Not in a shipit buffer"))
  (unless shipit-buffer-files-truncated
    (message "All files already loaded")
    (cl-return-from shipit-load-more-files))
  (let ((repo shipit-buffer-repo)
        (pr-number shipit-buffer-pr-number)
        (current-files (alist-get 'files shipit-buffer-pr-data))
        (start-page (1+ shipit-buffer-files-page))
        (target-buffer (current-buffer)))
    (message "Loading more files (page %d+)..." start-page)
    (shipit--fetch-more-files-async
     repo pr-number start-page
     (lambda (result)
       (let ((new-files (car result))
             (truncated (cdr result))
             (pages-fetched (cadr result)))
         (when (buffer-live-p target-buffer)
           (with-current-buffer target-buffer
             (if (= (length new-files) 0)
                 (progn
                   (setq shipit-buffer-files-truncated nil)
                   (message "No more files to load"))
               ;; Append new files to existing list
               (let ((all-files (append current-files new-files)))
                 (setf (alist-get 'files shipit-buffer-pr-data) all-files)
                 (setq shipit-buffer-files-truncated truncated)
                 (setq shipit-buffer-files-page (+ shipit-buffer-files-page (or pages-fetched 10)))
                 ;; Re-render the files section
                 (shipit--replace-files-section-with-content
                  repo shipit-buffer-pr-data pr-number all-files truncated)
                 (message "Loaded %d more files (total: %d%s)"
                          (length new-files)
                          (length all-files)
                          (if truncated "+" "")))))))))))

(defun shipit--prefetch-pr-commits-async ()
  "Prefetch PR commits in background for faster ediff later.
Checks if base/head commits exist locally, fetches async if missing."
  (when (and shipit-buffer-pr-data shipit-buffer-pr-number shipit-buffer-repo)
    (let* ((base-sha (cdr (assq 'sha (cdr (assq 'base shipit-buffer-pr-data)))))
           (head-sha (cdr (assq 'sha (cdr (assq 'head shipit-buffer-pr-data)))))
           (head-ref (cdr (assq 'ref (cdr (assq 'head shipit-buffer-pr-data)))))
           (pr-number shipit-buffer-pr-number)
           (repo shipit-buffer-repo))
      (when (and base-sha head-sha)
        ;; Check if commits exist locally
        (let ((need-fetch (or (not (shipit--sha-exists-locally-p base-sha))
                              (not (shipit--sha-exists-locally-p head-sha)))))
          (when need-fetch
            (shipit--debug-log "Prefetching PR #%d commits in background" pr-number)
            (shipit--fetch-pr-ref-async
             pr-number
             (lambda (success)
               (if success
                   (shipit--debug-log "Prefetch complete for PR #%d" pr-number)
                 (shipit--debug-log "Prefetch failed for PR #%d" pr-number)))
             repo head-ref)))))))

(defun shipit-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the shipit buffer content.
Arguments are for compatibility with `revert-buffer-function'."
  (interactive)
  (shipit--debug-log 'profiling "🔄 REFRESH CALLED - interactive=%s buffer=%s"
                     (called-interactively-p 'any)
                     (buffer-name))
  (when shipit-debug-log-enabled
    (shipit--debug-log 'profiling "🔄 BACKTRACE:\n%s" (with-output-to-string (backtrace))))
  (unless (derived-mode-p 'shipit-mode)
    (user-error "Not in a shipit buffer"))

  ;; Initialize refresh-level request deduplication cache to prevent stale data
  (when (fboundp 'shipit-gh-etag--init-refresh-cache)
    (shipit-gh-etag--init-refresh-cache))

  ;; Prevent recursive refreshes
  (if shipit-buffer--refresh-in-progress
      (shipit--debug-log "REFRESH-PREVENTION: Avoiding recursive buffer refresh")
    ;; Only proceed if no refresh is in progress
    (setq shipit-buffer--refresh-in-progress t)
    (unwind-protect
        (progn
          (shipit--debug-log "Refreshing shipit buffer: PR#%s in %s"
                             shipit-buffer-pr-number shipit-buffer-repo)

          ;; Restore backend overrides from dedicated buffer-local vars
          ;; (ensures they survive reloads that re-eval defcustom defaults)
          ;; If not yet detected (e.g. buffer created before this fix), detect now.
          (unless shipit-buffer-backend-id
            (setq shipit-buffer-backend-id (shipit--detect-backend-from-remote)))
          (setq-local shipit-pr-backend shipit-buffer-backend-id)
          (when shipit-buffer-backend-config
            (setq-local shipit-pr-backend-config shipit-buffer-backend-config))

          ;; Ensure global repository context is set for shipit functions
          (setq shipit-current-repo shipit-buffer-repo)

          ;; Ensure URL caching is enabled for ETag functionality
          (unless url-automatic-caching
            (setq url-automatic-caching t)
            (setq url-cache-directory (expand-file-name "url/cache" user-emacs-directory))
            ;; Ensure cache directory exists
            (unless (file-exists-p url-cache-directory)
              (make-directory url-cache-directory t)))

          ;; Clear buffer-local caches to force fresh data fetch
          (when (boundp 'shipit--cached-review-decision)
            (setq shipit--cached-review-decision nil))
          ;; Also clear the approval status cache
          (when (boundp 'shipit--cached-approval-status)
            (let ((cache-key (format "%s:%s" shipit-buffer-repo shipit-buffer-pr-number)))
              (remhash cache-key shipit--cached-approval-status)))

          ;; Set up context and render using EXACT same method as magit-status
          (setq shipit--current-displayed-pr (list shipit-buffer-pr-number shipit-buffer-repo))

          (let ((inhibit-read-only t)
                (pos (point))
                (repo shipit-buffer-repo)
                (pr-number shipit-buffer-pr-number))
            (erase-buffer)
            (require 'shipit-pr-sections)

            ;; Wrap in root magit-insert-section like magit-status does
            (magit-insert-section (root)
              (shipit--buffer-refresh-incremental repo pr-number))

            ;; Let magit handle section finalization automatically
            (goto-char (min pos (point-max)))

            (message "Shipit buffer refreshed")
            ;; Prefetch PR commits in background for faster ediff later
            (shipit--prefetch-pr-commits-async)))
      ;; unwind-protect cleanup: ALWAYS reset the flag (error, quit, throw, etc.)
      (setq shipit-buffer--refresh-in-progress nil)))) ; Close the if statement and condition-case




;;; Section interaction

(defun shipit-buffer-toggle-section ()
  "Toggle visibility of the section at point.
Tries registered expand handlers first for lazy-loading sections."
  (interactive)
  (when (require 'magit-section nil t)
    (let ((section (magit-current-section)))
      (when section
        (let ((inhibit-read-only t))
          (or (run-hook-with-args-until-success
               'shipit-buffer-section-expand-functions section)
              (magit-section-toggle section)))))))

(defun shipit-buffer-cycle-visibility ()
  "Cycle through visibility states of the section at point."
  (interactive)
  (when (require 'magit-section nil t)
    (let ((section (magit-current-section)))
      (when section
        (let ((inhibit-read-only t))
          (magit-section-cycle section))))))

(defun shipit-buffer-copy-dwim ()
  "Copy context-aware URL, visible region text, or current line.
With active region, copies only visible text (collapsed sections are skipped).
Without region, copies the URL for the section at point, or the current line."
  (interactive)
  (cond
   ((use-region-p)
    (shipit-buffer--copy-visible-region))
   ((shipit-buffer--copy-section-url))
   (t
    (let ((line (string-trim-right
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))
      (kill-new line)
      (message "Copied: %s" line)))))

(defun shipit-buffer--copy-visible-region ()
  "Copy only visible text in the active region to the kill ring.
Skips text hidden by magit section collapse overlays."
  (let ((beg (region-beginning))
        (end (region-end))
        (parts nil)
        (pos nil))
    (setq pos beg)
    (while (< pos end)
      (let ((next-change (next-single-char-property-change pos 'invisible nil end)))
        (unless (invisible-p pos)
          (push (buffer-substring-no-properties pos (min next-change end)) parts))
        (setq pos next-change)))
    (let ((text (apply #'concat (nreverse parts))))
      (kill-new text)
      (setq deactivate-mark t)
      (message "Copied %d characters" (length text)))))

(defun shipit-buffer--copy-section-url ()
  "Try to copy a URL for the section at point.
Returns non-nil if a URL was copied."
  (when (require 'magit-section nil t)
    (let ((section (magit-current-section)))
      (when section
        (shipit-actions-copy-url-at-point
         (run-hook-with-args-until-success
          'shipit-buffer-section-url-functions section))))))

(defun shipit-buffer-toggle-timestamps-dwim ()
  "Toggle timestamps: log timestamps if inside CI steps, else time format."
  (interactive)
  (let* ((section (and (require 'magit-section nil t)
                       (magit-current-section)))
         (handled (when section
                    (run-hook-with-args-until-success
                     'shipit-buffer-section-log-timestamps-functions section))))
    (unless handled
      (shipit-toggle-timestamp-format))))

;;; Utility functions

(defun shipit-buffer--pr-url ()
  "Return the web address of this buffer's pull request, or nil."
  (and shipit-buffer-repo shipit-buffer-pr-number
       (shipit-pr--browse-url shipit-buffer-repo shipit-buffer-pr-number)))

(defun shipit-buffer-select-pr ()
  "Select a different PR for this buffer."
  (interactive)
  (let ((new-pr-number (read-number "PR number: ")))
    (setq shipit-buffer-pr-number new-pr-number)
    (rename-buffer (shipit-buffer-name shipit-buffer-repo new-pr-number))
    (shipit-buffer-refresh)))

(defun shipit-buffer-help ()
  "Show help for shipit buffer commands."
  (interactive)
  (describe-mode))

;;; Integration point

;;;###autoload
(defun shipit-maybe-use-dedicated-buffer (pr-number repo)
  "Open PR using dedicated buffer if configured, otherwise use magit integration.
This is the main integration point that respects user configuration."
  (if shipit-use-dedicated-buffer
      (shipit-open-pr-buffer pr-number repo)
    ;; Fall back to existing magit integration
    (shipit--display-selected-pr pr-number repo)))

;;; URL section rendering

(defun shipit--insert-repo-url-line (url repo-name)
  "Insert a clickable Repo URL line with URL and REPO-NAME.
RET opens the repo in a shipit repo buffer if REPO-NAME is non-nil,
otherwise opens in browser."
  (when url
    (let ((start (point)))
      (insert (format "%s Repo URL:  %s\n"
                      (shipit--get-pr-field-icon "links" "🔗")
                      (propertize url 'font-lock-face 'link)))
      (let ((ov (make-overlay start (point)))
            (keymap (make-sparse-keymap)))
        (set-keymap-parent keymap (current-local-map))
        (define-key keymap (kbd "RET")
          (lambda () (interactive)
            (if repo-name
                (shipit-open-repo-buffer repo-name)
              (browse-url url))))
        (overlay-put ov 'keymap keymap)
        (overlay-put ov 'mouse-face 'highlight)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'help-echo "RET: open repo buffer")))))

(defun shipit--insert-pr-url-section (pr-data)
  "Insert a clickable URL section for the repo homepage from PR-DATA.
RET opens the repo in a shipit repo buffer."
  (let* ((repo-obj (cdr (assq 'repo (cdr (assq 'base pr-data)))))
         (repo-name (cdr (assq 'full_name repo-obj)))
         (url (or (cdr (assq 'html_url repo-obj))
                  (when repo-name
                    (condition-case nil
                        (let* ((resolved (shipit-pr--resolve-for-repo repo-name))
                               (backend (car resolved))
                               (browse-repo-fn (plist-get backend :browse-repo-url)))
                          (when browse-repo-fn
                            (funcall browse-repo-fn (cdr resolved))))
                      (error nil))))))
    (when url
      (magit-insert-section (pr-url nil)
        (shipit--insert-repo-url-line url repo-name)))))

;;; Worktree section rendering

(defun shipit--stack-entry-state (entry)
  "Return the state to display for stack ENTRY.
Merged and draft are the two states that change what a reader can do
with the entry, so they win over the raw state the API reports."
  (cond
   ((cdr (assq 'merged-at entry)) "merged")
   ((cdr (assq 'draft entry)) "draft")
   (t (or (cdr (assq 'state entry)) "open"))))

(defun shipit--stack-entry-keymap (repo number)
  "Return a keymap where RET opens pull request NUMBER in REPO.
In a shipit buffer the sibling opens as one too, whatever
`shipit-use-dedicated-buffer' says: that option chooses how magit\='s
own views open things, and falling back to them from inside a shipit
buffer looked like RET doing nothing."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
      (lambda ()
        (interactive)
        (if (derived-mode-p 'shipit-mode)
            (shipit-open-pr-buffer number repo)
          (shipit-maybe-use-dedicated-buffer number repo))))
    map))

(defun shipit--stack-entry-state-text (entry &optional statuses)
  "Return the state column for ENTRY: the word in its state\='s colour.
The SVG icon rides in front where those are on; the emoji fallback is
empty because the word is already there.  STATUSES, when given, rides
behind the word as the rollup and review glyphs, so a red or waiting
sibling is seen from here without opening it."
  (let* ((state (shipit--stack-entry-state entry))
         (icon (shipit--get-pr-state-icon state "")))
    (concat icon
            ;; Breathing room after a real icon; the empty fallback
            ;; must not indent the word instead.
            (if (> (length icon) 0) " " "")
            (shipit--colorize-pr-state state)
            (shipit--stack-status-glyphs statuses))))

(defun shipit--stack-entry-row (entry repo statuses)
  "Return the render row for stack ENTRY of REPO.
The title names the row when the forge gave one; a title says what a
sibling is about where a branch name only hints.  The branch moves
into the row\='s body.  STATUSES maps sibling numbers to their check
rollup and review decision; the entry\='s, when there, joins the
state column."
  (let ((number (cdr (assq 'number entry))))
    `((label . ,(format "#%s" number))
      (name . ,(or (cdr (assq 'title entry))
                   (cdr (assq 'head entry))
                   ""))
      (extra . ,(shipit--stack-entry-state-text
                 entry (cdr (assq number statuses))))
      (current . ,(cdr (assq 'current entry)))
      (marker . "this PR")
      (entry . ,entry)
      (repo . ,repo)
      (props . (shipit-stack-pr-number ,number
                shipit-stack-repo ,repo
                keymap ,(shipit--stack-entry-keymap repo number))))))

(defun shipit--insert-stack-body (stack repo)
  "Insert the rows of STACK for REPO through the shared stack renderer."
  (let* ((entries (cdr (assq 'entries stack)))
         (statuses (shipit-pr--stack-statuses-quietly
                    repo (mapcar (lambda (entry) (cdr (assq 'number entry)))
                                 entries))))
    (shipit-sections-insert-stack-rows
     (mapcar (lambda (entry) (shipit--stack-entry-row entry repo statuses))
             entries)
     (or (cdr (assq 'base stack)) "")
     nil
     #'shipit--wash-stack-entry
     (shipit--stack-base-icon))))

(defun shipit--stack-base-icon ()
  "Grey hollow dot marking the stack\='s base, or an empty string.
The base is a branch, not a pull request: it has no state, so a grey
circle sits in the state column the way the forge\='s own view draws
it."
  (let ((use-svglib (and (display-graphic-p)
                         shipit-use-svglib-icons
                         (featurep 'svg-lib)
                         (fboundp 'svg-lib-icon))))
    (if use-svglib
        (condition-case _err
            (let ((icon (shipit--svg-lib-icon-with-bg
                         "dot" nil :collection "octicons"
                         :height 1.0 :width 1.0 :padding 0.1
                         :foreground "#6e7781" :stroke 0)))
              (if (imagep icon) (propertize " " 'display icon) ""))
          (error ""))
      "")))

(defun shipit--stack-section-url-handler (section)
  "Return the address of what the stack SECTION at point names, or nil.
For M-w\='s copy dwim: the stack heading answers with the stack\='s
own address, and a row with its sibling pull request\='s, the same
one the pull request title copies.  Anywhere else in the section nil,
and the dwim keeps its fallback."
  (pcase (oref section type)
    ('shipit-stack-section
     (let ((repo (get-text-property (point) 'shipit-repo))
           (number (get-text-property (point) 'shipit-stack-number)))
       (when (and repo number)
         (shipit-pr--browse-stack-url repo number))))
    ('shipit-stack-layer
     (let ((repo (get-text-property (point) 'shipit-stack-repo))
           (number (get-text-property (point) 'shipit-stack-pr-number)))
       (when (and repo number)
         (shipit-pr--browse-url repo number))))))

(add-hook 'shipit-buffer-section-url-functions
          #'shipit--stack-section-url-handler)

(defun shipit--wash-stack-entry (row)
  "Fill the body of stack ROW: branch, author, description, comments.
Runs on the row\='s first expand.  Six siblings fetched as the buffer
draws would put a dozen requests on every render, so a sibling\='s
description and comments are asked for when the reader asks."
  (let* ((entry (cdr (assq 'entry row)))
         (repo (cdr (assq 'repo row)))
         (number (cdr (assq 'number entry))))
    (shipit--insert-stack-entry-lines entry)
    (condition-case err
        (shipit--insert-stack-description repo number)
      (error
       (insert (format "     could not fetch: %s\n"
                       (error-message-string err)))))))

(defun shipit--insert-stack-entry-lines (entry)
  "Insert the branch and author lines for stack ENTRY.
Two columns deeper than the row\='s own text, as everything washed
into a row is: the ladder is what says these belong to the row above."
  (when-let* ((head (cdr (assq 'head entry))))
    (insert (format "     %s\n"
                    (propertize head 'font-lock-face 'magit-branch-local))))
  (let* ((author (cdr (assq 'author entry)))
         (merged (cdr (assq 'merged-at entry)))
         (byline (delq nil (list (and author
                                      (propertize author 'font-lock-face
                                                  'magit-log-author))
                                 (and merged
                                      (propertize (substring merged 0 10)
                                                  'font-lock-face
                                                  'magit-log-date))))))
    (when byline (insert (format "     %s\n" (string-join byline "  "))))))

(defun shipit--insert-stack-markdown (text)
  "Insert TEXT rendered as markdown, indented under a child heading.
Deeper than the heading\='s own word: the icon widens the heading
past its column, so content at the same depth read as flush with it."
  (let ((beg (point))
        ;; `indent-rigidly' obeys the buffer's tab settings, and a tab
        ;; is however wide the window says: the ladder must be spaces.
        (indent-tabs-mode nil))
    (insert (shipit--render-markdown text))
    (unless (bolp) (insert "\n"))
    (indent-rigidly beg (point) 8)))

(defun shipit--insert-stack-description (repo number)
  "Insert the description of pull request NUMBER in REPO as a section.
Born shut: expanding a row shows a ladder of headings, not everything
the forge knows at once."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (fn (plist-get (car resolved) :fetch-pr))
         (pr (and fn (funcall fn (cdr resolved) number)))
         (body (cdr (assq 'body pr))))
    (magit-insert-section (shipit-stack-description nil t)
      (magit-insert-heading
        (format "     %s Description"
                (shipit--get-pr-field-icon "description" "\U0001F4DD")))
      (if (and body (not (string-empty-p (string-trim body))))
          (shipit--insert-stack-markdown body)
        (insert (propertize "        No description provided\n"
                            'font-lock-face 'italic))))))

(defun shipit--insert-stack-section (repo pr-data)
  "Insert the Stack section for PR-DATA in REPO.
Draws nothing when the backend cannot report stacks, and nothing when
this pull request is in none, so an ordinary pull request looks
exactly as it did before."
  (when (shipit-pr--stack-supported-p repo)
    (let* ((number (cdr (assq 'number pr-data)))
           ;; PR-DATA is already here; refetching it to read one field
           ;; would put a request on every render of this section.
           (stack (shipit-pr--fetch-stack repo number pr-data)))
      (when stack
        (magit-insert-section (shipit-stack-section)
          (let ((heading-start (point)))
            (magit-insert-heading
              (format "%s Stack #%s - %s of %s"
                      (shipit--get-pr-field-icon "stack" "\u26c1")
                      (cdr (assq 'number stack))
                      (cdr (assq 'position stack))
                      (cdr (assq 'size stack))))
            (add-text-properties heading-start (point)
                                 `(shipit-stack t shipit-repo ,repo
                                   shipit-stack-number
                                   ,(cdr (assq 'number stack)))))
          (magit-insert-section-body
            (shipit--insert-stack-body stack repo)))))))

(defun shipit--insert-worktree-section (pr-data)
  "Insert Worktree section for PR-DATA in current buffer.
Shows status (in-sync, out-of-sync, none), path, and last synced time.
The status is that of the worktree in use.  The path of each worktree
of the PR, shipit's own and yours, says whose it is."
  (let* ((pr-number (cdr (assq 'number pr-data)))
         (pr-head-sha (or (cdr (assq 'sha (cdr (assq 'head pr-data)))) (error "PR data missing head SHA")))
         (pr-repo (cdr (assq 'full_name (cdr (assq 'repo (cdr (assq 'base pr-data)))))))
         (status (condition-case _err
                     (shipit--get-worktree-status pr-number pr-head-sha pr-repo pr-data)
                   (error 'different-repo)))
         (worktrees (unless (eq status 'different-repo)
                      (shipit--pr-worktrees pr-number pr-repo pr-data)))
         (worktree-path (shipit--worktree-in-use worktrees))
         (status-icon (shipit--get-worktree-status-icon status
                        (pcase status
                          ('none "⭕")
                          ('in-sync "✓")
                          ('out-of-sync "⚠")
                          ('different-repo "⭕"))))
         (progress (and (fboundp 'shipit-worktree--progress-get)
                        (shipit-worktree--progress-get pr-repo pr-number)))
         (status-text (pcase status
                        ('none "No worktree checked out")
                        ('in-sync (concat (propertize "In Sync" 'font-lock-face '(:foreground "green" :weight bold))
                                          " (PR #" (number-to-string pr-number) " @ "
                                          (propertize (substring pr-head-sha 0 7) 'font-lock-face 'magit-hash)
                                          ")"))
                        ('out-of-sync (let ((local-sha (condition-case nil
                                                           (string-trim-right
                                                            (shell-command-to-string
                                                             (format "cd %s && git rev-parse HEAD"
                                                                     (shell-quote-argument worktree-path))))
                                                         (error ""))))
                                       (concat (propertize "Out of Sync" 'font-lock-face 'font-lock-warning-face)
                                               (if (and local-sha (> (length local-sha) 0))
                                                   (concat " (local: "
                                                           (propertize (substring local-sha 0 7) 'font-lock-face 'magit-hash)
                                                           " upstream: "
                                                           (propertize (substring pr-head-sha 0 7) 'font-lock-face 'magit-hash)
                                                           ")")
                                                 ""))))
                        ('different-repo (shipit--no-clone-text pr-repo)))))
    (magit-insert-section (shipit-worktree-section)
      (let ((heading-start (point)))
        (magit-insert-heading (format "%s Worktree" status-icon))
        (add-text-properties heading-start (point)
                             `(shipit-worktree t
                                               shipit-pr-data ,pr-data
                                               shipit-pr-number ,pr-number
                                               shipit-repo ,pr-repo)))
      (magit-insert-section-body
        (shipit--insert-worktree-body
         pr-data status status-text worktrees progress
         pr-number pr-repo)))))

(defun shipit--insert-worktree-path-line (label path whose)
  "Insert the line LABEL: PATH (WHOSE) of the Worktree section.
PATH is shown relative to the repository root.  RET on it opens it
in Dired."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET")
      (lambda () (interactive)
        (if (file-directory-p path)
            (dired path)
          (user-error "Worktree path not found: %s" path))))
    (insert (format "   %s: " label)
            (propertize (directory-file-name
                         (file-relative-name path (shipit--get-repo-root)))
                        'font-lock-face 'shipit-filename-face
                        'keymap keymap)
            (format " (%s)\n" whose))))

(defun shipit--insert-worktree-paths (worktrees)
  "Insert the path of each of WORKTREES, and whose it is.
WORKTREES is a plist of `shipit--pr-worktrees'.  When there are two,
the first line is that of the worktree in use."
  (let ((own (plist-get worktrees :own))
        (yours (plist-get worktrees :yours)))
    (cond ((and own yours)
           (shipit--insert-worktree-path-line "Path" own "made by shipit, in use")
           (shipit--insert-worktree-path-line "Also" yours "your worktree"))
          (own (shipit--insert-worktree-path-line "Path" own "made by shipit"))
          (yours (shipit--insert-worktree-path-line "Path" yours "your worktree")))))

(defun shipit--insert-worktree-body
    (pr-data status status-text worktrees progress pr-number pr-repo)
  "Insert the body content of the Worktree section.
WORKTREES is a plist of `shipit--pr-worktrees', or nil.
Extracted from `shipit--insert-worktree-section\=' so the body
can be re-rendered in place by
`shipit-worktree--refresh-section-in-buffer\=' without
disturbing the surrounding magit-section identity (which the
section navigator relies on -- a full section recreate
strands the old object in the parent\='s children list and
breaks `n\=' / `p\=')."
  (let ((body-start (point))
        (worktree-path (shipit--worktree-in-use worktrees)))
    ;; Phase line is rendered by an overlay (see
    ;; `shipit-worktree--progress-render') so this body insertion
    ;; doesn't have to know whether a progress operation is in flight.
    (insert (format "   Status: %s\n" status-text))
    (when worktree-path
      (shipit--insert-worktree-paths worktrees)
      (when-let* ((pr-info (shipit--get-pr-info-from-worktree worktree-path)))
        (when-let* ((created-at (plist-get pr-info :created-at)))
          (insert "   Created: ")
          (let ((timestamp-start (point))
                (formatted-ts (if (fboundp 'shipit--format-timestamp)
                                  (shipit--format-timestamp created-at)
                                created-at)))
            (insert formatted-ts)
            (let ((timestamp-end (point)))
              (put-text-property timestamp-start timestamp-end
                                 'font-lock-face 'shipit-timestamp-face))
            (insert "\n"))))
      ;; Skip the LFS indicator while a `pulling-lfs' phase is in
      ;; flight: `shipit-worktree--lfs-status' shells out to
      ;; `git lfs ls-files' synchronously, and on a 3000+ file repo
      ;; that's a 0.5-1s blocking call.  Combined with a 200ms
      ;; refresh throttle from the progress filter it would freeze
      ;; the UI and let re-entrant refreshes corrupt the section.
      ;; The Phase line already shows live counters during the pull.
      (when (and (boundp 'shipit-worktree-show-lfs-indicator)
                 shipit-worktree-show-lfs-indicator
                 (fboundp 'shipit-worktree--lfs-status)
                 (not (and progress
                           (eq (plist-get progress :phase) 'pulling-lfs))))
        (when-let* ((lfs (shipit-worktree--lfs-status worktree-path)))
          (let* ((total (plist-get lfs :total))
                 (present (plist-get lfs :present))
                 (icon (cond ((zerop present) "\u26a0")
                             ((= present total) "\u2713")
                             (t "\u26a0")))
                 (label-face (cond ((zerop present) 'font-lock-warning-face)
                                   ((= present total)
                                    '(:foreground "green" :weight bold))
                                   (t 'font-lock-warning-face))))
            (insert (format "   LFS: %s %s%s\n"
                            icon
                            (propertize
                             (format "%d/%d materialised" present total)
                             'font-lock-face label-face)
                            (cond ((zerop present)
                                   " -- press l to pull changed paths")
                                  (t ""))))))))
    (add-text-properties body-start (point)
                         `(shipit-worktree t
                                           shipit-pr-data ,pr-data
                                           shipit-pr-number ,pr-number
                                           shipit-repo ,pr-repo))
    (insert "\n")))


(provide 'shipit-buffer)
;;; shipit-buffer.el ends here
