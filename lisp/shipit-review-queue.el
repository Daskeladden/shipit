;;; shipit-review-queue.el --- Batch review queue -*- lexical-binding: t; -*-

;;; Commentary:

;; Local-only queue for inline review comments.  Mirrors the GitHub web
;; UI's "Start a review" flow: instead of posting each comment as it is
;; written, the editor stashes comments here, the PR view paints them as
;; overlays under the matching diff line, and a final `Submit review'
;; action POSTs the whole batch via the active backend's
;; `:submit-review' entry point.
;;
;; The queue is buffer-local in the PR view.  Closing the buffer
;; drops the draft -- if you want server-visible drafts (with web UI
;; parity), see the README design notes; this file is the simpler
;; single-submit path.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'shipit-sections)  ; Finding a file or a diff line in Files Changed
(require 'shipit-render)    ; `shipit--wrap-body'

(declare-function shipit-post-review "shipit-http"
                  (pr-number event &optional body comments repo))
(declare-function shipit-buffer-refresh "shipit-buffer" (&optional _ _))
(declare-function shipit-editor-open "shipit-editor" (context))
(defvar shipit-review--pr-data)

(defgroup shipit-review-queue nil
  "Batch review queue for shipit."
  :group 'shipit)

(defface shipit-review-queue-draft-face
  '((t :inherit shadow :slant italic))
  "Face for the body of a queued draft review comment overlay."
  :group 'shipit-review-queue)

(defface shipit-review-queue-draft-label-face
  '((t :inherit warning :weight bold))
  "Face for the `Draft' label inside a queued review overlay."
  :group 'shipit-review-queue)

(defcustom shipit-review-queue-draft-bg-color "#4a3a1f"
  "SVG rounded-pill background color for pending review overlays.
Picked to be visibly different from the posted-comment background
(`shipit-rounded-section-bg-color') so a glance is enough to tell
draft from posted.  Adjust to match your theme."
  :type 'color
  :group 'shipit-review-queue)

(defcustom shipit-review-queue-persistence-file
  (expand-file-name "shipit-review-queue.el" user-emacs-directory)
  "File where pending review-comment queues are persisted.
Stores an alist keyed by (REPO . PR-NUMBER) -> queue-list so a
`shipit-init' reload, an Emacs restart, or a PR buffer rebuild
doesn\\='t drop in-flight drafts.  Set to nil to disable
persistence (drafts then only live as long as the buffer)."
  :group 'shipit-review-queue
  :type '(choice file (const :tag "Disabled" nil)))

(defvar-local shipit-review-queue nil
  "Buffer-local list of pending review comments.
Each entry is a plist:
  (:id ID :file PATH :line LINE :old-line OLD-LINE :side SIDE :body TEXT)
ID is a stable string so overlays can map back to entries when
the user edits or removes them.")

(defvar-local shipit-review-queue--overlays nil
  "Overlays currently painting the queue in this buffer.
Re-built by `shipit-review-queue--render' on every queue change.")

;;;; Cross-session persistence
;;
;; The queue is otherwise pure buffer-local state -- killing or
;; rebuilding the PR buffer (e.g. `shipit-init', a hard refresh, or an
;; Emacs restart) would drop drafts.  The queue is kept on disk: an
;; alist keyed by (REPO . PR-NUMBER),
;; lazily loaded on first PR open and written after every mutation.

(defvar shipit-review-queue--store nil
  "Alist of ((REPO . PR-NUMBER) . QUEUE) loaded from disk.
QUEUE is the same plist-list shape as the buffer-local
`shipit-review-queue'.  Empty queues are pruned by
`shipit-review-queue--persist-current' so closed-out PRs don\\='t
linger in the store.")

(defvar shipit-review-queue--store-loaded nil
  "Non-nil once the persistence file has been read this session.")

(defun shipit-review-queue--store-key (repo pr-number)
  "Stable key for REPO/PR-NUMBER -- normalises PR-NUMBER to an int.
String-vs-int callers then can\\='t fork the store."
  (cons repo
        (cond ((numberp pr-number) pr-number)
              ((stringp pr-number) (string-to-number pr-number))
              (t pr-number))))

(defun shipit-review-queue--store-load ()
  "Read the persistence file once per session into the store.
Errors are logged via `shipit--debug-log' but never raised so a
corrupt file can\\='t block PR buffer creation."
  (unless shipit-review-queue--store-loaded
    (setq shipit-review-queue--store-loaded t)
    (when (and shipit-review-queue-persistence-file
               (file-readable-p shipit-review-queue-persistence-file))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents shipit-review-queue-persistence-file)
            (goto-char (point-min))
            (let ((data (read (current-buffer))))
              (when (listp data)
                (setq shipit-review-queue--store data))))
        (error
         (when (fboundp 'shipit--debug-log)
           (shipit--debug-log "QUEUE-LOAD failed: %S" err)))))))

(defun shipit-review-queue--store-save ()
  "Persist the in-memory store to disk."
  (when shipit-review-queue-persistence-file
    (condition-case err
        (with-temp-file shipit-review-queue-persistence-file
          (let ((print-level nil)
                (print-length nil))
            (insert ";; shipit review queue -- generated, do not edit.\n")
            (prin1 shipit-review-queue--store (current-buffer))
            (insert "\n")))
      (error
       (when (fboundp 'shipit--debug-log)
         (shipit--debug-log "QUEUE-SAVE failed: %S" err))))))

(defun shipit-review-queue--persist-current ()
  "Write the current PR buffer\\='s queue back to the store + disk.
No-op when the buffer isn\\='t a shipit PR buffer.  Empty queues
are removed from the store so cleared PRs don\\='t linger."
  (when (and (bound-and-true-p shipit-buffer-repo)
             (bound-and-true-p shipit-buffer-pr-number))
    (shipit-review-queue--store-load)
    (let* ((key (shipit-review-queue--store-key
                 shipit-buffer-repo shipit-buffer-pr-number))
           (cell (assoc key shipit-review-queue--store)))
      (cond
       ((null shipit-review-queue)
        (when cell
          (setq shipit-review-queue--store
                (delq cell shipit-review-queue--store))))
       (cell (setcdr cell (copy-sequence shipit-review-queue)))
       (t (push (cons key (copy-sequence shipit-review-queue))
                shipit-review-queue--store))))
    (shipit-review-queue--store-save)))

(defun shipit-review-queue--hydrate-from-store ()
  "Repopulate the buffer-local queue from the persisted store.
Called on PR buffer ready when the buffer\\='s queue is empty --
catches the post-reload case where the buffer was rebuilt fresh
but drafts exist on disk for this (REPO . PR-NUMBER)."
  (when (and (bound-and-true-p shipit-buffer-repo)
             (bound-and-true-p shipit-buffer-pr-number)
             (null shipit-review-queue))
    (shipit-review-queue--store-load)
    (let* ((key (shipit-review-queue--store-key
                 shipit-buffer-repo shipit-buffer-pr-number))
           (stored (cdr (assoc key shipit-review-queue--store))))
      (when stored
        (setq shipit-review-queue (copy-sequence stored))
        (when (fboundp 'shipit--debug-log)
          (shipit--debug-log
           "QUEUE-HYDRATE: %s #%s, restored %d draft%s"
           shipit-buffer-repo shipit-buffer-pr-number
           (length stored) (if (= (length stored) 1) "" "s")))))))

(declare-function shipit-review--pr-repo "shipit-review-mode")

(defun shipit-review-queue--effective-count ()
  "Return the draft count relevant to the current buffer.
In a shipit PR buffer that owns the queue, returns its own
length.  In a review-mode worktree file buffer, walks back to the
matching PR buffer and reads its queue so the same number shows
in both views.  Returns 0 when neither path resolves."
  (cond
   ((bound-and-true-p shipit-buffer-pr-number)
    (length shipit-review-queue))
   ((and (bound-and-true-p shipit-review--pr-data)
         (fboundp 'shipit-review--pr-repo))
    (let* ((pr-number (cdr (assq 'number shipit-review--pr-data)))
           (repo (shipit-review--pr-repo))
           (pr-buf (and repo pr-number
                        (shipit-review-queue--locate-pr-buffer
                         repo pr-number))))
      (if pr-buf
          (length (buffer-local-value 'shipit-review-queue pr-buf))
        0)))
   (t 0)))

(defun shipit-review-queue-mode-line ()
  "Return a mode-line segment describing the queue, or empty string."
  (let ((count (shipit-review-queue--effective-count)))
    (if (and count (> count 0))
        (propertize
         (format " 📝Draft:%d " count)
         'face 'shipit-review-queue-draft-label-face
         'help-echo
         (format "%d queued review draft%s -- `S' (in PR buffer) or M-x shipit-review-submit"
                 count (if (= count 1) "" "s")))
      "")))

(defun shipit-review-queue--install-mode-line ()
  "Add the draft-count segment to `mode-line-misc-info' once."
  (let ((entry '(:eval (shipit-review-queue-mode-line))))
    (unless (member entry mode-line-misc-info)
      (setq mode-line-misc-info
            (append mode-line-misc-info (list entry))))))

(shipit-review-queue--install-mode-line)

(defvar-local shipit-review-queue--saved-header-line :unset
  "Stashed `header-line-format' before the banner stole it.
`:unset' means we haven\\='t taken it over yet, so a restore can
distinguish \"never owned\" from \"originally nil\".")

(defun shipit-review-queue--refresh-banner ()
  "Sync the PR-buffer header-line to reflect the current queue.
Shows `N drafts queued — S to submit, K to discard' when the
queue has items; restores the pre-banner header-line otherwise.
Quietly no-ops outside a shipit PR buffer."
  (when (bound-and-true-p shipit-buffer-pr-number)
    (let ((count (length shipit-review-queue)))
      (cond
       ((> count 0)
        (when (eq shipit-review-queue--saved-header-line :unset)
          (setq shipit-review-queue--saved-header-line header-line-format))
        (setq-local
         header-line-format
         (concat
          (propertize " 📝 " 'face
                      'shipit-review-queue-draft-label-face)
          (propertize
           (format "%d review draft%s queued"
                   count (if (= count 1) "" "s"))
           'face 'shipit-review-queue-draft-label-face)
          (propertize
           " — press `S' to submit, `K' to discard, `c' on a draft to edit "
           'face 'shipit-review-queue-draft-face))))
       (t
        (unless (eq shipit-review-queue--saved-header-line :unset)
          (setq-local header-line-format
                      shipit-review-queue--saved-header-line)
          (setq-local shipit-review-queue--saved-header-line :unset)))))))

(defun shipit-review-queue--gen-id ()
  "Return a fresh unique id for a queue entry."
  (format "draft-%d-%d" (truncate (* 1000 (float-time))) (random 100000)))

(defun shipit-review-queue-count ()
  "Return how many drafts are queued in this buffer."
  (length shipit-review-queue))

(defun shipit-review-queue-empty-p ()
  "Return non-nil when no drafts are queued."
  (null shipit-review-queue))

(defun shipit-review-queue--locate-pr-buffer (repo pr-number)
  "Return the shipit PR buffer matching REPO + PR-NUMBER, or nil.
We never want the queue to live on a review-mode worktree-file
buffer -- the overlays render into the magit tree, which only
exists in the shipit PR buffer."
  (cl-loop for buf in (buffer-list)
           when (and (buffer-local-value 'shipit-buffer-pr-number buf)
                     (buffer-local-value 'shipit-buffer-repo buf)
                     (equal (buffer-local-value
                             'shipit-buffer-pr-number buf)
                            pr-number)
                     (equal (buffer-local-value
                             'shipit-buffer-repo buf)
                            repo))
           return buf))

(defun shipit-review-queue-add (file line side body &optional old-line
                                      repo pr-number)
  "Push a draft inline comment onto the queue.
FILE / LINE / SIDE / BODY are the GitHub review-comment fields.
OLD-LINE is set when SIDE is LEFT (deletion-side anchor).  REPO
and PR-NUMBER route the queue into the matching shipit PR buffer
when the caller is in a review-mode file buffer instead -- without
this routing the queue lives on the file buffer, no overlays
render, and the user sees nothing.  Returns the freshly-assigned
id so callers can later remove the same entry."
  (let* ((id (shipit-review-queue--gen-id))
         (target-buf (or (and repo pr-number
                              (shipit-review-queue--locate-pr-buffer
                               repo pr-number))
                         (current-buffer))))
    (when (fboundp 'shipit--debug-log)
      (shipit--debug-log
       "QUEUE-ADD: caller=%s target=%s repo=%S pr=%S file=%S line=%S side=%S"
       (buffer-name)
       (and (buffer-live-p target-buf) (buffer-name target-buf))
       repo pr-number file line side))
    (with-current-buffer target-buf
      (push (list :id id :file file :line line :old-line old-line
                  :side side
                  :body (substring-no-properties (or body "")))
            shipit-review-queue)
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log
         "QUEUE-ADD: stored in %s, queue-size=%d"
         (buffer-name) (length shipit-review-queue)))
      (shipit-review-queue--persist-current)
      (shipit-review-queue--render)
      (shipit-review-queue--render-section)
      (shipit-review-queue--render-badges)
      (shipit-review-queue--refresh-banner)
      (shipit-review-queue--refresh-all-review-buffers)
      (force-mode-line-update t))
    id))

(defun shipit-review-queue-update (id new-body)
  "Replace the body of the queue entry with ID."
  (dolist (entry shipit-review-queue)
    (when (equal (plist-get entry :id) id)
      (plist-put entry :body new-body)))
  (shipit-review-queue--persist-current)
  (shipit-review-queue--render)
  (shipit-review-queue--render-section)
  (shipit-review-queue--render-badges)
  (shipit-review-queue--refresh-banner)
  (shipit-review-queue--refresh-all-review-buffers)
  (force-mode-line-update t))

(defun shipit-review-queue-remove (id)
  "Drop the queue entry with ID."
  (setq shipit-review-queue
        (cl-remove-if (lambda (e) (equal (plist-get e :id) id))
                      shipit-review-queue))
  (shipit-review-queue--persist-current)
  (shipit-review-queue--render)
  (shipit-review-queue--render-section)
  (shipit-review-queue--render-badges)
  (shipit-review-queue--refresh-banner)
  (shipit-review-queue--refresh-all-review-buffers)
  (force-mode-line-update t))

(defun shipit-review-queue-find-by-id (id)
  "Return the queue entry with ID, or nil."
  (cl-find-if (lambda (e) (equal (plist-get e :id) id))
              shipit-review-queue))

(defun shipit-review-queue-at-point ()
  "Return the queue entry at point, or nil.
Looks first at the `shipit-review-queue-id' text property (set
on each line of a draft magit section), then falls back to any
overlay carrying the same id (legacy after-string drafts from
before the section conversion).
Useful for `RET' / transient handlers that want to act on the
draft under point."
  (let ((id (or (get-text-property (point) 'shipit-review-queue-id)
                (cl-some (lambda (ov)
                           (overlay-get ov 'shipit-review-queue-id))
                         (overlays-at (point))))))
    (and id (shipit-review-queue-find-by-id id))))

;;;; Overlay rendering

(declare-function magit-current-section "magit-section")
(declare-function magit-section-show "magit-section" (section))

(defun shipit-review-queue--clear-overlays ()
  "Drop all queue overlays in the current buffer."
  (dolist (ov shipit-review-queue--overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq shipit-review-queue--overlays nil))

(declare-function shipit-review--rounded-after-string "shipit-review-mode"
                  (lines))
(defvar shipit-rounded-section-bg-color)

(defun shipit-review-queue--draft-hint ()
  "Return the hint below a draft, on the keys that act on it."
  (propertize "press `c' to edit the draft, `k' to remove it"
              'font-lock-face 'shipit-review-queue-draft-face))

(defun shipit-review-queue--overlay-text (entry)
  "Render the body of a draft overlay for ENTRY.
Wraps the heading + body + hint with the same rounded-pill SVG
background used for posted comments via
`shipit-review--rounded-after-string', but with the draft-specific
`shipit-review-queue-draft-bg-color' so the user can tell pending
from posted at a glance.  Falls back to plain text on non-graphic
displays."
  (let* ((body (or (plist-get entry :body) ""))
         (label (propertize "PENDING"
                            'font-lock-face
                            'shipit-review-queue-draft-label-face))
         (file (plist-get entry :file))
         (line (plist-get entry :line))
         (loc (cond
               ((and file line) (format " · %s:%s" file line))
               (file (format " · %s" file))
               (t "")))
         (hint (shipit-review-queue--draft-hint))
         (heading-text
          (concat "  📝 " label
                  (propertize " · local · won't publish"
                              'font-lock-face
                              'shipit-review-queue-draft-face)
                  (propertize loc 'font-lock-face 'shipit-filename-face)))
         (body-text
          (cond
           ((string-empty-p (string-trim body)) "")
           (t (shipit-review-queue--wrap-raw-body body "  "))))
         (lines (append (list heading-text)
                        (when (not (string-empty-p body-text))
                          (split-string body-text "\n"))
                        (list (concat "  " hint)))))
    (cond
     ((and (display-graphic-p)
           (fboundp 'shipit-review--rounded-after-string))
      ;; Borrow the rounded-pill renderer but with our distinct
      ;; background colour by dynamically rebinding
      ;; `shipit-rounded-section-bg-color' for the call.
      (dlet ((shipit-rounded-section-bg-color
              shipit-review-queue-draft-bg-color))
        (shipit-review--rounded-after-string lines)))
     (t (mapconcat #'identity lines "\n")))))

(defvar shipit-insert-section--current)
(defvar magit-insert-section--parent)
(defvar magit-insert-section--current)
(defvar magit-insert-section--oldroot)

(defvar-local shipit-review-queue--sections nil
  "Alist of (DRAFT-ID . SECTION) tracking magit sections we inserted.
The section objects let us delete the section region cleanly on
remove / discard / re-render -- text-only tracking would have to
re-walk the buffer for each removal.")

(defun shipit-review-queue--section-text-p (section)
  "Whether the text of the draft SECTION is still in the buffer.
A redraw of Files Changed, or of one file in it, removes the drafts in
it, and the markers of SECTION then point into other text."
  (let ((start (oref section start)))
    (and start (eq (get-text-property start 'magit-section) section))))

(defun shipit-review-queue--clear-sections ()
  "Delete every pending-comment section inserted by the queue.
Removes the buffer text covered by each section and detaches the
section from its parent\\='s `children' list so the magit tree
stays consistent.  A section whose text a redraw already removed
deletes nothing."
  (let ((inhibit-read-only t))
    (dolist (cell shipit-review-queue--sections)
      (let ((section (cdr cell)))
        (when (and section (shipit-review-queue--section-text-p section))
          (let* ((start (let ((m (oref section start)))
                          (if (markerp m) (marker-position m) m)))
                 (end (let ((m (oref section end)))
                        (if (markerp m) (marker-position m) m)))
                 (parent (oref section parent)))
            (when (and start end (<= end (point-max)))
              (delete-region start end))
            (when parent
              (oset parent children
                    (delq section (oref parent children))))))))
    (setq shipit-review-queue--sections nil)))

(defun shipit-review-queue--insert-pending-section (entry file-section target)
  "Insert a pending-comment magit section after diff line at TARGET.
ENTRY is the queue plist, FILE-SECTION is the pr-file section
whose body contains TARGET.  Returns the freshly created magit
section so the caller can stash it for later removal.

The section is created via the standard `magit-insert-section'
macro with `magit-insert-section--parent' bound to FILE-SECTION,
so the magit tree picks up the new node and n/p navigates into
it.  After insertion the parent\\='s children list is sorted by
start position so the in-buffer order matches the tree order,
and a rounded-pill background is applied across the whole
section (heading + body) with the draft-specific colour, the
same way `shipit--insert-status-hierarchical-comment' decorates
posted inline comments."
  (save-excursion
    (goto-char target)
    (forward-line 1)
    (let ((inhibit-read-only t)
          (section nil))
      (dlet ((magit-insert-section--parent file-section)
             (magit-insert-section--current nil)
             (magit-insert-section--oldroot nil))
        (setq section
              (magit-insert-section
                  (shipit-pending-comment (plist-get entry :id))
                (let ((heading-start (point)))
                  (magit-insert-heading
                    (shipit-review-queue--section-heading entry))
                  (add-text-properties
                   heading-start (point)
                   `(shipit-review-queue-id ,(plist-get entry :id)
                                            shipit-file-path ,(plist-get entry :file)
                                            shipit-line-number ,(plist-get entry :line)
                                            shipit-repo ,(bound-and-true-p shipit-buffer-repo)
                                            shipit-pr-number ,(bound-and-true-p shipit-buffer-pr-number))))
                (magit-insert-section-body
                  (insert (shipit-review-queue--section-body entry))
                  (insert "\n"))))
        ;; Keep the parent's children ordered by buffer position so n/p
        ;; walks them in the right order.  magit-insert-section appends
        ;; to the front of the list -- after a few inserts at different
        ;; positions, the front-most isn't necessarily the latest in the
        ;; buffer.
        (oset file-section children
              (sort (oref file-section children)
                    (lambda (a b)
                      (< (marker-position (oref a start))
                         (marker-position (oref b start)))))))
      ;; Apply the rounded-pill background AFTER the section is
      ;; finalised so the start/end markers are settled.  Use the
      ;; draft-specific colour rather than the global pill colour, and
      ;; cover the whole section (heading + body) -- inline comments do
      ;; the same via `shipit-rounded--apply-to-section'.
      (when (and (display-graphic-p)
                 (fboundp 'shipit-rounded--apply-bg)
                 section
                 (slot-boundp section 'start)
                 (slot-boundp section 'end))
        (let ((sect-start (let ((m (oref section start)))
                            (if (markerp m) (marker-position m) m)))
              (sect-end (let ((m (oref section end)))
                          (if (markerp m) (marker-position m) m))))
          (when (and sect-start sect-end (< sect-start sect-end))
            (shipit-rounded--apply-bg sect-start sect-end
                                      shipit-review-queue-draft-bg-color))))
      section)))

(declare-function shipit--svg-lib-icon-with-bg "shipit-render")

(defun shipit-review-queue--icon ()
  "Return SVG pencil icon for pending drafts, or 📝 emoji fallback.
Uses the same `shipit--svg-lib-icon-with-bg' pipeline the rest of
the buffer uses, so the draft icon matches the visual weight of
the surrounding section icons.  Falls back to 📝 on TTY frames or
when svg-lib isn't available."
  (if (and (boundp 'shipit-use-svglib-icons)
           shipit-use-svglib-icons
           (display-graphic-p)
           (featurep 'svg-lib)
           (fboundp 'shipit--svg-lib-icon-with-bg))
      (condition-case _err
          (let ((icon (shipit--svg-lib-icon-with-bg
                       "pencil" nil
                       :collection "octicons"
                       :height 1.0 :width 1.0 :padding 0.1
                       :foreground "#e0a020" :stroke 0)))
            (if (imagep icon)
                (propertize " " 'display icon)
              "📝"))
        (error "📝"))
    "📝"))

(defun shipit-review-queue--section-heading (entry)
  "Render the heading line for a pending-comment magit section."
  (let* ((label (propertize "PENDING"
                            'font-lock-face
                            'shipit-review-queue-draft-label-face))
         (file (plist-get entry :file))
         (line (plist-get entry :line))
         (loc (cond
               ((and file line) (format " · %s:%s" file line))
               (file (format " · %s" file))
               (t ""))))
    (concat "     " (shipit-review-queue--icon) " " label
            (propertize " · local · won't publish"
                        'font-lock-face 'shipit-review-queue-draft-face)
            (propertize loc 'font-lock-face 'shipit-filename-face))))

(declare-function shipit--wrap-text "shipit-core" (text width &optional indent))

(defun shipit-review-queue--wrap-raw-body (body indent)
  "Word-wrap BODY with INDENT, preserving raw markdown.
Differs from `shipit--wrap-body' by skipping markdown
font-lock entirely so suggestion fences (```suggestion ... ```)
and other fence markers stay visible -- the user wrote the body
expecting it to land on GitHub verbatim, so the preview should
match.  Code blocks inside ``` fences are left unwrapped.

Strips text properties from BODY first.  When the editor saves a
draft, the buffer contents come from `*shipit-editor*' running
`markdown-mode', which leaves `invisible markdown-markup' on
fence characters -- without this strip, the PR-buffer render
honors `invisible' and the user sees the code without the
surrounding fence.  We want what we will POST."
  (let* ((raw (substring-no-properties (or body "")))
         (width (max 40 (or fill-column 80)))
         (effective-width (max 20 (- width (length indent))))
         (wrapped (if (fboundp 'shipit--wrap-text)
                      (shipit--wrap-text raw effective-width)
                    raw)))
    (with-temp-buffer
      (insert wrapped)
      (goto-char (point-min))
      (insert indent)
      (while (re-search-forward "\n" nil t)
        (unless (eobp)
          (insert indent)))
      (buffer-string))))

(defun shipit-review-queue--section-body (entry)
  "Render plain body text for a pending-comment magit section.
The rounded-pill background is applied separately by
`shipit-review-queue--insert-pending-section' via
`shipit-rounded--apply-bg', so this just emits the indented body
+ hint as plain text -- no embedded SVG, no markdown font-lock
(so suggestion fences stay visible).  The text carries the id of
the draft, so `c' and `k' act on the draft from any of its lines."
  (let* ((body (or (plist-get entry :body) ""))
         (hint (shipit-review-queue--draft-hint))
         (body-text
          (cond
           ((string-empty-p (string-trim body)) "")
           (t (shipit-review-queue--wrap-raw-body body "        "))))
         (lines (append
                 (when (not (string-empty-p body-text))
                   (split-string body-text "\n"))
                 (list (concat "        " hint)))))
    (propertize (mapconcat #'identity lines "\n")
                'shipit-review-queue-id (plist-get entry :id))))

(defun shipit-review-queue--render ()
  "Insert pending-comment magit sections under matching diff lines.
Clears any previously-inserted sections first so re-renders
don\\='t accumulate duplicates."
  (shipit-review-queue--clear-sections)
  ;; Old overlays from a previous version of this file might still be
  ;; in the buffer -- sweep them so the new section path doesn\\='t end
  ;; up with both representations.
  (shipit-review-queue--clear-overlays)
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log
     "QUEUE-RENDER: buf=%s queue=%d"
     (buffer-name)
     (length shipit-review-queue)))
  (when shipit-review-queue
    ;; Insertion path: temporarily expand each file we need so its
    ;; lazy diff body materializes (text-property scan needs the
    ;; lines to exist), drop the pill, then restore the file's prior
    ;; hidden state.  This keeps file-filter / startup re-renders
    ;; from auto-popping files open as a side effect; users who were
    ;; viewing a file when they queued a draft see no change because
    ;; the file was already shown.
    (let ((pr-files (shipit--find-files-section))
          (file-restorations (make-hash-table :test 'eq)))
      (when pr-files
        (dolist (entry shipit-review-queue)
          (let ((file (plist-get entry :file))
                (line (plist-get entry :line)))
            (when (and file line)
              (let* ((hit (shipit--find-file-in-files-section file pr-files))
                     (file-pos (car-safe hit))
                     (matched-path (cdr-safe hit)))
                (when file-pos
                  (let ((file-section
                         (save-excursion
                           (goto-char file-pos)
                           (magit-current-section))))
                    (when (and file-section
                               (eq (oref file-section type) 'pr-file))
                      ;; Remember the file's original hidden state on
                      ;; first visit only -- later entries on the same
                      ;; file would otherwise overwrite with the
                      ;; post-show value.
                      (let ((cached (gethash file-section
                                             file-restorations 'unset)))
                        (when (eq cached 'unset)
                          (puthash file-section
                                   (oref file-section hidden)
                                   file-restorations)))
                      (magit-section-show file-section)
                      (let ((target
                             (shipit--find-diff-line-in-file-section
                              matched-path line file-section)))
                        (when target
                          (let ((section
                                 (shipit-review-queue--insert-pending-section
                                  entry file-section target)))
                            (push (cons (plist-get entry :id) section)
                                  shipit-review-queue--sections)
                            (when (fboundp 'shipit--debug-log)
                              (shipit--debug-log
                               "QUEUE-RENDER: section inserted id=%S at target=%d"
                               (plist-get entry :id) target))))))))))))
        ;; Re-hide any file we forced open just to materialize the
        ;; body.  Files that were already shown stay shown.
        (maphash (lambda (sec was-hidden)
                   (when was-hidden
                     (magit-section-hide sec)))
                 file-restorations)))))

;;;; Submit

(defcustom shipit-review-queue-events
  '(("Comment only" . "COMMENT")
    ("Approve" . "APPROVE")
    ("Request changes" . "REQUEST_CHANGES"))
  "Mapping of human label -> GitHub review event for submit prompts."
  :type '(alist :key-type string :value-type string)
  :group 'shipit-review-queue)

(defun shipit-review-queue--entry-to-comment (entry)
  "Convert a queue ENTRY into the alist shape expected by the backend."
  (let ((file (plist-get entry :file))
        (line (plist-get entry :line))
        (side (or (plist-get entry :side) "RIGHT"))
        (body (plist-get entry :body)))
    `((path . ,file)
      (line . ,line)
      (side . ,side)
      (body . ,body))))

;;;###autoload
(defun shipit-review-submit ()
  "Submit the buffer-local queue as a single review.
Prompts for the review event (COMMENT / APPROVE / REQUEST_CHANGES)
and an optional review body.  On success clears the queue and
refreshes the PR buffer."
  (interactive)
  (when (shipit-review-queue-empty-p)
    (user-error "No draft comments queued -- write a comment first"))
  (unless (and (bound-and-true-p shipit-buffer-pr-number)
               (bound-and-true-p shipit-buffer-repo))
    (user-error "Not in a shipit PR buffer"))
  (let* ((pr-number shipit-buffer-pr-number)
         (repo shipit-buffer-repo)
         (count (shipit-review-queue-count))
         (event-label
          (completing-read
           (format "Submit %d comment%s -- review event: "
                   count (if (= count 1) "" "s"))
           (mapcar #'car shipit-review-queue-events) nil t))
         (event (or (cdr (assoc event-label shipit-review-queue-events))
                    "COMMENT"))
         (body (read-string
                (format "Review body (optional, %s): " event)))
         (comments (mapcar #'shipit-review-queue--entry-to-comment
                           ;; nreverse so the comments are submitted in
                           ;; the order the user wrote them.
                           (reverse shipit-review-queue))))
    (condition-case err
        (progn
          (shipit-post-review pr-number event body comments repo)
          (setq shipit-review-queue nil)
          (shipit-review-queue--persist-current)
          (shipit-review-queue--clear-sections)
          (shipit-review-queue--clear-overlays)
          (shipit-review-queue--clear-badges)
          (shipit-review-queue--render-section)
          (shipit-review-queue--refresh-banner)
          (shipit-review-queue--refresh-all-review-buffers)
          (force-mode-line-update t)
          (message "shipit: submitted %d comment%s as %s"
                   count (if (= count 1) "" "s") event)
          (when (fboundp 'shipit-buffer-refresh)
            (shipit-buffer-refresh)))
      (error
       (message "shipit: review submission failed: %s"
                (error-message-string err))))))

;;;###autoload
(defun shipit-review-queue-discard ()
  "Drop all queued draft review comments in this buffer."
  (interactive)
  (cond
   ((shipit-review-queue-empty-p)
    (message "Queue is already empty"))
   ((yes-or-no-p
     (format "Discard %d queued review comment%s? "
             (shipit-review-queue-count)
             (if (= (shipit-review-queue-count) 1) "" "s")))
    (setq shipit-review-queue nil)
    (shipit-review-queue--persist-current)
    (shipit-review-queue--clear-sections)
    (shipit-review-queue--clear-overlays)
    (shipit-review-queue--clear-badges)
    (shipit-review-queue--render-section)
    (shipit-review-queue--refresh-banner)
    (shipit-review-queue--refresh-all-review-buffers)
    (force-mode-line-update t)
    (message "Queue cleared"))))

;;;###autoload
(defun shipit-review-queue-edit-at-point ()
  "Open the editor on the queued draft overlay at point.
Lets the user revise the body, then writes the new text back into
the same queue entry (no new entry is created)."
  (interactive)
  (let ((entry (shipit-review-queue-at-point)))
    (unless entry
      (user-error "No queued draft at point"))
    (unless (fboundp 'shipit-editor-open)
      (user-error "shipit-editor not loaded"))
    (let ((id (plist-get entry :id)))
      (shipit-editor-open
       (list :type 'inline-comment
             :source-buffer (current-buffer)
             :pr-number (bound-and-true-p shipit-buffer-pr-number)
             :repo (bound-and-true-p shipit-buffer-repo)
             :file-path (plist-get entry :file)
             :line-number (plist-get entry :line)
             :old-line-number (plist-get entry :old-line)
             :side (plist-get entry :side)
             :initial-content (plist-get entry :body)
             :shipit-review-queue-edit-id id
             :section-marker (point-marker))))))

;;;###autoload
(defun shipit-review-queue-remove-at-point ()
  "Drop the queued draft overlay at point."
  (interactive)
  (let ((entry (shipit-review-queue-at-point)))
    (unless entry
      (user-error "No queued draft at point"))
    (shipit-review-queue-remove (plist-get entry :id))
    (message "Removed draft comment")))

(defun shipit-review-queue--delete-at-point ()
  "Remove the draft at point when the user agrees, for `k'.
Return non-nil on a draft, also when the answer is no, so that `k'
looks for no posted comment there."
  (let ((entry (shipit-review-queue-at-point)))
    (when entry
      (when (y-or-n-p (format "Really remove this draft (%s)? "
                              (truncate-string-to-width
                               (car (split-string (plist-get entry :body) "\n"))
                               50 nil nil "...")))
        (shipit-review-queue-remove-at-point))
      t)))

(add-hook 'shipit-delete-at-point-functions #'shipit-review-queue--delete-at-point)

(declare-function shipit-register-dwim-handler "shipit-pr-actions"
                  (name matcher action))

(require 'transient)

(transient-define-prefix shipit-review-queue-draft-actions ()
  "Actions for the queued draft inline comment at point."
  [["Draft"
    ("c" "Edit (opens editor pre-filled with current body)"
     shipit-review-queue-edit-at-point)
    ("k" "Remove (drop from the queue)"
     shipit-review-queue-remove-at-point)]
   ["Queue"
    ("S" "Submit review (publish all drafts via API)"
     shipit-review-submit)
    ("K" "Discard all queued drafts"
     shipit-review-queue-discard)]])

(with-eval-after-load 'shipit-pr-actions
  (when (fboundp 'shipit-register-dwim-handler)
    ;; Register LAST so it sits at the front of the handler list and
    ;; wins over generic inline-comment handlers when point is on a
    ;; pending draft pill.  The matcher checks the queue-id text
    ;; property added by `--insert-pending-section'.
    (shipit-register-dwim-handler
     'shipit-review-queue-draft
     (lambda () (shipit-review-queue-at-point))
     #'shipit-review-queue-draft-actions)))

;;;###autoload
(defun shipit-comment ()
  "Context-aware comment command bound to `c' in the PR view.
Dispatches by what point is on:
  * a queued draft (pending-comment magit section) -> edit it
    via `shipit-review-queue-edit-at-point';
  * elsewhere -> currently a no-op with a hint, until additional
    contexts are wired in (e.g. add general comment, reply to
    posted thread).

The binding was historically nil (the `c' keymap entry pointed
at a never-defined `shipit-comment' symbol), surfacing as
\"Wrong type argument: commandp\".  This stub fills the slot."
  (interactive)
  (cond
   ((shipit-review-queue-at-point)
    (call-interactively #'shipit-review-queue-edit-at-point))
   (t
    (user-error
     "Nothing to comment on at point (press `c' on a draft to edit it)"))))

(defun shipit-review-queue--maybe-rerender ()
  "Repaint queue overlays, banner, and Pending Review section.
Hooked onto `shipit-buffer-ready-hook' so a refresh that wipes
out the section tree also re-attaches our draft overlays to the
new diff lines and re-installs the header-line banner.  Also
hydrates the buffer-local queue from the on-disk store so a
`shipit-init' reload or an Emacs restart doesn't drop drafts."
  (shipit-review-queue--hydrate-from-store)
  (when shipit-review-queue
    (shipit-review-queue--render))
  (shipit-review-queue--render-section)
  (shipit-review-queue--render-badges)
  (shipit-review-queue--refresh-banner)
  (shipit-review-queue--refresh-all-review-buffers)
  (force-mode-line-update t))

;;;; Review-mode worktree file overlays
;;
;; Mirror the PR-buffer overlay onto the worktree file the user is
;; actually editing in `shipit-review-mode'.  Existing review-mode
;; already paints posted inline comments; the queue paints its
;; pending counterpart so the user can see drafts on the actual code
;; without flipping back to the PR view.

(declare-function shipit--get-repo-root "shipit-lib")

(defvar-local shipit-review-queue--review-mode-overlays nil
  "Per-file-buffer list of draft overlays painted by review-mode.
Cleared and rebuilt on every queue change for that file's PR.")

(defun shipit-review-queue--clear-review-mode-overlays ()
  "Drop draft overlays in the current review-mode file buffer."
  (dolist (ov shipit-review-queue--review-mode-overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq-local shipit-review-queue--review-mode-overlays nil))

(defun shipit-review-queue--review-mode-render ()
  "Paint queued drafts that target this file as overlays.
Reads the queue from the matching shipit PR buffer (via
`shipit-review-queue--locate-pr-buffer'), filters by the file
this review-mode buffer is visiting, and attaches an
`after-string' overlay at each draft\\='s target line."
  (shipit-review-queue--clear-review-mode-overlays)
  (when (and (bound-and-true-p shipit-review-mode)
             (buffer-file-name)
             (bound-and-true-p shipit-review--pr-data)
             (fboundp 'shipit-review--pr-repo)
             (fboundp 'shipit--get-repo-root))
    (let* ((pr-number (cdr (assq 'number shipit-review--pr-data)))
           (repo (shipit-review--pr-repo))
           (pr-buf (and repo pr-number
                        (shipit-review-queue--locate-pr-buffer
                         repo pr-number)))
           (queue (and pr-buf
                       (buffer-local-value 'shipit-review-queue pr-buf)))
           (root (shipit--get-repo-root))
           (rel (and root (file-relative-name (buffer-file-name) root))))
      (when (and queue rel)
        (dolist (entry queue)
          (let ((file (plist-get entry :file))
                (line (plist-get entry :line)))
            (when (and (equal file rel) line (numberp line) (> line 0))
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- line))
                (let* ((eol (line-end-position))
                       (ov (make-overlay eol eol))
                       (text (concat "\n"
                                     (shipit-review-queue--overlay-text
                                      entry))))
                  (overlay-put ov 'after-string text)
                  (overlay-put ov 'shipit-review-queue-id
                               (plist-get entry :id))
                  (overlay-put ov 'shipit-review-queue-draft t)
                  (overlay-put ov 'priority 95)
                  (push ov shipit-review-queue--review-mode-overlays))))))))))

(defun shipit-review-queue--refresh-all-review-buffers ()
  "Repaint queue overlays in every review-mode buffer for any open PR.
Walks the buffer list once per queue change so the file the user
is editing keeps in sync.  Cheaper than hooking each file buffer
to a per-buffer change watcher and works regardless of which
worktree files happen to be open."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p shipit-review-mode)
        (shipit-review-queue--review-mode-render)))))

;;;; Per-file draft badges
;;
;; A small `Draft (N)' tag attached to each pr-file heading so the
;; user can see at a glance which files have queued drafts even when
;; the file's diff is collapsed.

(defvar-local shipit-review-queue--badge-overlays nil
  "File-heading badge overlays painted by the queue.")

(defun shipit-review-queue--clear-badges ()
  "Drop file-heading badge overlays."
  (dolist (ov shipit-review-queue--badge-overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq-local shipit-review-queue--badge-overlays nil))

(add-hook 'shipit-files-section-shown-hook #'shipit-review-queue--render-badges)

(defun shipit-review-queue--render-badges ()
  "Attach a `Draft (N)' badge to each pr-file heading whose file is queued."
  (shipit-review-queue--clear-badges)
  (when (fboundp 'shipit--debug-log)
    (shipit--debug-log
     "QUEUE-BADGE: buf=%s queue=%d root=%S"
     (buffer-name) (length shipit-review-queue)
     (and (boundp 'magit-root-section) (not (null magit-root-section)))))
  (when (and shipit-review-queue
             (boundp 'magit-root-section) magit-root-section)
    (let ((counts (make-hash-table :test 'equal))
          (pr-files-count 0)
          (pr-file-count 0)
          (badges-placed 0))
      (dolist (entry shipit-review-queue)
        (let ((file (plist-get entry :file)))
          (when file
            (puthash file (1+ (gethash file counts 0)) counts))))
      (dolist (child (oref magit-root-section children))
        ;; Only render badges when Files Changed is expanded -- a
        ;; collapsed parent has an invisible overlay over the child
        ;; headings, but `after-string' overlays inside the range
        ;; still render, piling every badge onto the one visual line.
        (when (and (eq (oref child type) 'pr-files)
                   (not (and (slot-boundp child 'hidden)
                             (oref child hidden))))
          (cl-incf pr-files-count)
          (dolist (file-section (oref child children))
            (when (eq (oref file-section type) 'pr-file)
              (cl-incf pr-file-count)
              (let* ((path (save-excursion
                             (goto-char (oref file-section start))
                             (get-text-property (point) 'shipit-file-path)))
                     (count (and path (gethash path counts 0))))
                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log
                   "QUEUE-BADGE: visit pr-file path=%S count=%S" path count))
                (when (and count (> count 0))
                  ;; 1-char overlay on the heading\='s last character.
                  ;; The previous zero-length-at-newline overlay was
                  ;; suppressed by magit\='s heading text-properties.
                  (let* ((bol (save-excursion
                                (goto-char (oref file-section start))
                                (line-beginning-position)))
                         (eol (save-excursion
                                (goto-char (oref file-section start))
                                (line-end-position)))
                         (ov (make-overlay (max bol (1- eol)) eol))
                         (badge (propertize
                                 (format "   📝 Draft (%d)" count)
                                 'font-lock-face
                                 'shipit-review-queue-draft-label-face)))
                    (overlay-put ov 'after-string badge)
                    (overlay-put ov 'shipit-review-queue-badge t)
                    (push ov shipit-review-queue--badge-overlays)
                    (cl-incf badges-placed))))))))
      (when (fboundp 'shipit--debug-log)
        (shipit--debug-log
         "QUEUE-BADGE: pr-files-sections=%d pr-files=%d badges-placed=%d"
         pr-files-count pr-file-count badges-placed)))))

;;;; Pending Review magit section
;;
;; The overlay under each diff line shows the draft *next to* the code
;; it talks about, but it's easy to miss when Files Changed is
;; collapsed.  This section gives the user a single list of every
;; queued draft at the top of the PR buffer with edit / remove / jump
;; actions per entry.

(defun shipit-review-queue--remove-section ()
  "Drop the existing Pending Review section, if any."
  (when (and (boundp 'magit-root-section) magit-root-section)
    (let ((victim nil))
      (dolist (child (oref magit-root-section children))
        (when (eq (oref child type) 'shipit-pending-review)
          (setq victim child)))
      (when victim
        (let* ((start (let ((m (oref victim start)))
                        (if (markerp m) (marker-position m) m)))
               (end (let ((m (oref victim end)))
                      (if (markerp m) (marker-position m) m)))
               (inhibit-read-only t))
          (when (and start end)
            (delete-region start end))
          (oset magit-root-section children
                (delq victim (oref magit-root-section children))))))))

(defun shipit-review-queue--draft-heading (entry)
  "Format the heading line for a draft ENTRY."
  (let* ((file (plist-get entry :file))
         (line (plist-get entry :line))
         (suffix (if line (format ":%s" line) ""))
         (body (or (plist-get entry :body) ""))
         (first-line
          (or (car (split-string (string-trim body) "\n"))
              ""))
         (preview
          (if (> (length first-line) 60)
              (concat (substring first-line 0 60) "…")
            first-line)))
    (concat "      "
            (propertize "PENDING" 'font-lock-face
                        'shipit-review-queue-draft-label-face)
            "  "
            (propertize (or file "(no file)")
                        'font-lock-face 'shipit-filename-face)
            suffix
            (if (string-empty-p preview)
                ""
              (concat "  — "
                      (propertize preview 'font-lock-face
                                  'shipit-review-queue-draft-face))))))

(defun shipit-review-queue--diff-line-p ()
  "Whether the line at point is a line of the diff in Files Changed.
A posted comment or a draft shown below a diff line is not one, so the
code that the Pending Review shows above a draft holds the diff alone."
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (and (text-property-not-all start end 'shipit-pr-file nil)
         (not (text-property-not-all start end 'shipit-comment nil))
         (not (text-property-not-all start end 'shipit-review-queue-id nil)))))

(defun shipit-review-queue--diff-lines-near (target direction count limit)
  "Return up to COUNT diff lines next to the line at TARGET, with faces.
DIRECTION is 1 for the lines below that line and -1 for the lines above
it, and the walk stops at the position LIMIT.  The lines come in the
order of the buffer."
  (save-excursion
    (goto-char target)
    (let ((lines nil))
      (while (and (< (length lines) count)
                  (zerop (forward-line direction))
                  (if (> direction 0) (< (point) limit) (>= (point) limit)))
        (when (shipit-review-queue--diff-line-p)
          (push (buffer-substring (line-beginning-position) (line-end-position))
                lines)))
      (if (> direction 0) (nreverse lines) lines))))

(defun shipit-review-queue--code-context (file line &optional radius)
  "Return a short snippet of the diff at FILE/LINE, or nil.
Looks up the line in the buffer's pr-files section and takes RADIUS
diff lines on each side, 2 by default, so the user can see what the
draft is commenting on without leaving the Pending Review section."
  (let ((radius (or radius 2))
        (pr-files (shipit--find-files-section)))
    (when pr-files
      (let* ((hit (shipit--find-file-in-files-section file pr-files))
             (file-pos (car-safe hit))
             (matched-path (cdr-safe hit))
             (file-section (when file-pos
                             (save-excursion
                               (goto-char file-pos)
                               (magit-current-section)))))
        (when (and file-section (eq (oref file-section type) 'pr-file))
          (shipit--call-with-section-shown
           file-section
           (lambda ()
             (let ((target (shipit--find-diff-line-in-file-section
                            matched-path line file-section)))
               (when target
                 (mapconcat
                  #'identity
                  (append (shipit-review-queue--diff-lines-near
                           target -1 radius (oref file-section content))
                          (list (save-excursion
                                  (goto-char target)
                                  (buffer-substring (line-beginning-position)
                                                    (line-end-position))))
                          (shipit-review-queue--diff-lines-near
                           target 1 radius (oref file-section end)))
                  "\n"))))))))))

(defun shipit-review-queue--insert-section-at-point ()
  "Insert the Pending Review section tree at point.
Caller is responsible for `inhibit-read-only', position, and
`magit-insert-section--parent'."
  (magit-insert-section (shipit-pending-review)
    (let ((heading-start (point)))
      (magit-insert-heading
        (format "%s Pending Review (%d)"
                (shipit-review-queue--icon)
                (length shipit-review-queue)))
      (add-text-properties heading-start (point)
                           `(shipit-review-queue-section t)))
    (magit-insert-section-body
      (insert "   ")
      (insert (propertize
               "Press `S' to submit, `K' to discard.  RET on a draft to jump, `c' to edit, `k' to remove.\n\n"
               'font-lock-face 'shipit-review-queue-draft-face))
      ;; Walk the queue in insertion order (oldest first).
      (dolist (entry (reverse shipit-review-queue))
        (let ((section
               (magit-insert-section (shipit-pending-review-draft
                                      (plist-get entry :id))
                 (let ((heading-start (point)))
                   (magit-insert-heading
                     (shipit-review-queue--draft-heading entry))
                   (add-text-properties
                    heading-start (point)
                    `(shipit-review-queue-id ,(plist-get entry :id)
                                              shipit-file-path ,(plist-get entry :file)
                                              shipit-line-number ,(plist-get entry :line)
                                              shipit-repo ,(bound-and-true-p shipit-buffer-repo)
                                              shipit-pr-number ,(bound-and-true-p shipit-buffer-pr-number))))
                 (magit-insert-section-body
                   ;; Code context: a few diff lines around the anchor
                   ;; so the user can see what they're commenting on.
                   (let ((snippet (shipit-review-queue--code-context
                                   (plist-get entry :file)
                                   (plist-get entry :line))))
                     (when (and snippet
                                (not (string-empty-p (string-trim snippet))))
                       (let ((start (point)))
                         (insert snippet "\n")
                         (add-face-text-property start (point) 'shadow t))))
                   (let* ((body (or (plist-get entry :body) ""))
                          (wrapped
                           (shipit--wrap-body body "         ")))
                     (insert wrapped "\n\n"))))))
          ;; Apply the rounded-pill background -- same treatment
          ;; posted inline comments and the inline pending pill in
          ;; Files Changed get, with the queue-draft color so it
          ;; reads as "pending" at a glance.
          (when (and (display-graphic-p)
                     (fboundp 'shipit-rounded--apply-bg)
                     section
                     (slot-boundp section 'start)
                     (slot-boundp section 'end))
            (let ((sect-start (let ((m (oref section start)))
                                (if (markerp m) (marker-position m) m)))
                  (sect-end (let ((m (oref section end)))
                              (if (markerp m) (marker-position m) m))))
              (when (and sect-start sect-end (< sect-start sect-end))
                (shipit-rounded--apply-bg sect-start sect-end
                                          shipit-review-queue-draft-bg-color))))))
      (insert "\n"))))

(defun shipit-review-queue--render-section ()
  "Sync the Pending Review section to the queue state.
Removes the existing section first so a refresh always reflects
the latest queue.  No-op when queue is empty (the user already
sees the section disappear)."
  (when (bound-and-true-p shipit-buffer-pr-number)
    (let ((inhibit-read-only t))
      (shipit-review-queue--remove-section)
      (when shipit-review-queue
        (save-excursion
          (goto-char (point-max))
          (unless (or (= (point) (point-min))
                      (eq (char-before) ?\n))
            (insert "\n"))
          (let ((magit-insert-section--parent magit-root-section))
            (shipit-review-queue--insert-section-at-point)))))))

(with-eval-after-load 'shipit-buffer
  ;; Strip stale variants of the hook entry -- protects against the
  ;; void-function-after-partial-reload case.
  (dolist (sym (cl-remove-if-not
                (lambda (s)
                  (and (symbolp s)
                       (string-prefix-p
                        "shipit-review-queue--maybe-rerender"
                        (symbol-name s))))
                (and (boundp 'shipit-buffer-ready-hook)
                     shipit-buffer-ready-hook)))
    (remove-hook 'shipit-buffer-ready-hook sym))
  (add-hook 'shipit-buffer-ready-hook
            #'shipit-review-queue--maybe-rerender))

(defun shipit-review-queue--repaint-files ()
  "Draw the drafts and the file badges again after a redraw.
A redraw of Files Changed, or of one file in it, removes the drafts
and the badges in it.  For `shipit-files-section-refreshed-hook' and
`shipit-file-diff-refreshed-hook'."
  (shipit-review-queue--render)
  (shipit-review-queue--render-badges))

(add-hook 'shipit-files-section-refreshed-hook #'shipit-review-queue--repaint-files)
(add-hook 'shipit-file-diff-refreshed-hook #'shipit-review-queue--repaint-files)

(with-eval-after-load 'shipit-review-mode
  ;; Repaint queued drafts whenever review-mode is enabled in a file
  ;; buffer.  Without this, opening a worktree file after queueing a
  ;; comment elsewhere wouldn\\='t show the draft until the next queue
  ;; mutation triggers a refresh-all pass.
  (dolist (sym (cl-remove-if-not
                (lambda (s)
                  (and (symbolp s)
                       (string-prefix-p
                        "shipit-review-queue--review-mode-render"
                        (symbol-name s))))
                (and (boundp 'shipit-review-mode-hook)
                     shipit-review-mode-hook)))
    (remove-hook 'shipit-review-mode-hook sym))
  (add-hook 'shipit-review-mode-hook
            #'shipit-review-queue--review-mode-render))

(provide 'shipit-review-queue)

;;; shipit-review-queue.el ends here
