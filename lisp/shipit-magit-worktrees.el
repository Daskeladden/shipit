;;; shipit-magit-worktrees.el --- Pull request status in magit's worktree section -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit's `Worktrees' section lists a path and a revision per worktree.
;; For the worktrees shipit creates per pull request, the question that
;; decides whether one can go is not in git at all: did the pull request
;; land?  Where a repository squash-merges, the branch's commits never
;; reach master, so `git branch --merged' answers "no" for work that
;; shipped weeks ago.
;;
;; So ask the forge.  One batched GraphQL query covers every worktree on
;; screen; the section renders from cache at once and refreshes itself
;; when the answer lands.  States that cannot change again -- merged,
;; closed -- are cached for the session; open ones expire after
;; `shipit-magit-worktrees-ttl'.  A number the forge has no pull request
;; for is cached like an open one, and a failed request refreshes
;; nothing: a refresh asks about everything still unknown, so either
;; would otherwise be asked about again as fast as the forge replies.
;;
;; The status goes in the right margin, where magit puts commit ages, so
;; magit's own margin toggles under `L' hide and show it and `d' adds
;; each worktree's size on disk.  Rows carry `shipit-pr-number', so
;; `shipit-dwim' offers the pull request's actions on them.
;;
;; Enable with:
;;
;;   (add-hook 'magit-status-sections-hook #'shipit-magit-worktrees-insert-section t)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'transient)
(require 'magit)
(require 'shipit-core)
(require 'shipit-http)
(require 'shipit-worktree)

(declare-function shipit-open-pr-buffer "shipit-buffer")
(declare-function shipit-register-dwim-handler "shipit-pr-actions")

(defvar shipit-magit-worktrees--displaced nil
  "Where `magit-insert-worktrees' sat before this section took over.
An index into `magit-status-sections-hook', or nil when magit\='s own
section was not on the hook at all.  That section is opt-in rather than
one of magit\='s defaults, so handing back must restore only what was
really there.")

(defun shipit-magit-worktrees--restore-at (index)
  "Put `magit-insert-worktrees' back at INDEX on the sections hook."
  (let ((hook (remq #'magit-insert-worktrees magit-status-sections-hook)))
    (setq magit-status-sections-hook
          (append (seq-take hook index)
                  (list #'magit-insert-worktrees)
                  (seq-drop hook index)))))

(defun shipit-magit-worktrees--take-over ()
  "Replace `magit-insert-worktrees', remembering where it sat.
A second call finds it already gone and leaves the remembered position
alone, so installing twice still hands back to the right place."
  (let ((pos (seq-position magit-status-sections-hook
                           #'magit-insert-worktrees)))
    (when pos (setq shipit-magit-worktrees--displaced pos)))
  (remove-hook 'magit-status-sections-hook #'magit-insert-worktrees)
  (add-hook 'magit-status-sections-hook
            #'shipit-magit-worktrees-insert-section t))

(defun shipit-magit-worktrees--hand-back ()
  "Remove this section, putting magit\='s own back where it was."
  (remove-hook 'magit-status-sections-hook
               #'shipit-magit-worktrees-insert-section)
  (when (integerp shipit-magit-worktrees--displaced)
    (shipit-magit-worktrees--restore-at shipit-magit-worktrees--displaced)
    (setq shipit-magit-worktrees--displaced nil)))

(defun shipit-magit-worktrees--install (enable)
  "Take over `magit-status-sections-hook', or hand it back.
This section inserts the same rows `magit-insert-worktrees' does, with
the pull request\='s state alongside, so the two swap places rather than
both running.  Handing back matters: turning this section off should
leave the hook as it was found -- magit\='s own section back at its old
position, or absent, when that is where it started."
  (if enable
      (shipit-magit-worktrees--take-over)
    (shipit-magit-worktrees--hand-back)))

(defcustom shipit-magit-worktrees-ttl 3600
  "Seconds a still-open pull request's state stays fresh.
Merged and closed pull requests are cached without expiry, since those
states cannot change again."
  :type 'natnum
  :group 'shipit)

(defcustom shipit-magit-worktrees-show-size t
  "Whether to measure each worktree's size on disk.
Sizes are measured in the background and shown when magit's margin
details are on, which is `d' under `L'."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-magit-worktrees-margin-function
  #'shipit-magit-worktrees--margin-text
  "Function rendering one worktree's status for the right margin.
Called with a plist, returning the string to show, or nil for nothing.
Keys: `:number', `:state' (`merged', `open', `closed', `draft', or nil
while unknown), `:merged-at', `:title', `:size' in bytes, `:width'
columns available, and `:details' when magit's margin details are on."
  :type 'function
  :group 'shipit)

(defcustom shipit-magit-worktrees-margin-min-width 28
  "Columns the right margin needs for a pull request\='s state, or nil.
Magit sizes that margin for a date and nothing else -- 11 columns for
the `age' style -- and its own WIDTH slot cannot say otherwise, since
`magit-status-margin' types that slot as the function computing the
width rather than as a number.  So the section widens the buffer\='s
margin itself, and leaves the option alone.

A margin already wider than this keeps its width: turning magit\='s
details on with `d' asks for the worktree sizes, and shrinking back
would truncate them.  Nil takes whatever magit computed."
  :type '(choice (const :tag "Whatever magit computed" nil)
                 (natnum :tag "Columns"))
  :group 'shipit)

(defun shipit-magit-worktrees--widen-margin ()
  "Widen the margin to `shipit-magit-worktrees-margin-min-width'.
Returns the new width, or nil when the margin was already wide enough.
Magit recomputes the width every time it sets the margins, so this runs
on each insert rather than once at setup."
  (let ((wanted shipit-magit-worktrees-margin-min-width)
        (config (bound-and-true-p magit--right-margin-config)))
    (when (and wanted config (> wanted (or (nth 2 config) 0)))
      (setf (nth 2 magit--right-margin-config) wanted)
      (magit-set-window-margins)
      wanted)))

(defcustom shipit-magit-worktrees-enabled t
  "Whether shipit annotates magit's worktree section.
The section installs itself when this file loads rather than leaving
that to user configuration: `shipit-reload' strips every `shipit-'
function out of `magit-status-sections-hook' before reloading, so a
hook entry added anywhere else disappears on the first reload and takes
magit's worktree list with it."
  :type 'boolean
  :group 'shipit
  :set (lambda (symbol value)
         (set-default symbol value)
         (shipit-magit-worktrees--install value)))

(defvar shipit-magit-worktrees--states (make-hash-table :test 'equal)
  "Hash \"REPO#NUMBER\" -> plist of what the forge last said.")

(defvar shipit-magit-worktrees--sizes (make-hash-table :test 'equal)
  "Hash worktree path -> size in bytes.")

(defvar shipit-magit-worktrees--pending nil
  "Keys a running query will answer, so a refresh does not queue a second.")

;;;; Reading a worktree

(defun shipit-magit-worktrees--number (path)
  "Pull request number of the worktree at PATH, or nil.
Prefers the `.shipit-pr-info.json' shipit writes into the worktree,
falling back to the `pr-<number>-<branch>' name it gives the directory."
  (or (plist-get (shipit--get-pr-info-from-worktree path) :pr-number)
      (let ((name (file-name-nondirectory (directory-file-name path))))
        (and (string-match "\\`pr-\\([0-9]+\\)-" name)
             (string-to-number (match-string 1 name))))))

(defun shipit-magit-worktrees--key (repo number)
  "Cache key for pull request NUMBER in REPO."
  (format "%s#%d" repo number))

(defun shipit-magit-worktrees--info (repo path)
  "What is known about the worktree at PATH, or nil when it has no PR."
  (when-let* ((repo)
              (number (shipit-magit-worktrees--number path)))
    (append (list :number number)
            (gethash (shipit-magit-worktrees--key repo number)
                     shipit-magit-worktrees--states)
            (when-let* ((bytes (shipit-magit-worktrees--size path)))
              (list :size bytes)))))

;;;; Querying the forge

(defun shipit-magit-worktrees--answered-p (response)
  "Non-nil when RESPONSE carries the repository's answer.
A failed request carries none, and neither does one for a repository
the forge could not resolve."
  (consp (alist-get 'repository (alist-get 'data response))))

(defun shipit-magit-worktrees--missing (numbers found)
  "Plists for those of NUMBERS the forge answered with nothing.
FOUND holds the plists it did answer with.  The forge answers nothing
for a number the repository has no pull request under, and that has to
be remembered like any other state: left unknown, the next refresh asks
about it again, and its answer refreshes again."
  (let ((known (mapcar (lambda (info) (plist-get info :number)) found))
        (now (float-time)))
    (mapcar (lambda (n) (list :number n :state 'missing :fetched now))
            (seq-remove (lambda (n) (memq n known)) numbers))))

(defun shipit-magit-worktrees--query (repo numbers)
  "A GraphQL document asking REPO for the state of each of NUMBERS.
One request answers the whole section; a request per worktree would
take about a second each, far too slow for `magit-status'."
  (let ((owner (car (split-string repo "/")))
        (name (cadr (split-string repo "/"))))
    (concat (format "query{repository(owner:%S,name:%S){" owner name)
            (mapconcat
             (lambda (n)
               (format "p%d:pullRequest(number:%d){number state isDraft mergedAt title}"
                       n n))
             numbers "")
            "}}")))

(defun shipit-magit-worktrees--json-state (pr)
  "State of the parsed pull request PR as a symbol."
  (let ((state (alist-get 'state pr))
        (draft (alist-get 'isDraft pr)))
    (cond ((equal state "MERGED") 'merged)
          ((equal state "CLOSED") 'closed)
          ((and draft (not (eq draft :false))) 'draft)
          ((equal state "OPEN") 'open))))

(defun shipit-magit-worktrees--json-value (value)
  "VALUE from parsed JSON, with JSON null read as nil."
  (and value (not (eq value :null)) value))

(defun shipit-magit-worktrees--parse (response)
  "Pull request plists from a GraphQL RESPONSE, or nil when it holds none."
  (delq nil
        (mapcar
         (lambda (cell)
           (let ((pr (cdr cell)))
             (when-let* (((consp pr))
                         (number (alist-get 'number pr)))
               (list :number number
                     :state (shipit-magit-worktrees--json-state pr)
                     :merged-at (shipit-magit-worktrees--json-value
                                 (alist-get 'mergedAt pr))
                     :title (shipit-magit-worktrees--json-value
                             (alist-get 'title pr))
                     :fetched (float-time)))))
         (alist-get 'repository (alist-get 'data response)))))

(defun shipit-magit-worktrees--stale-p (info)
  "Non-nil when INFO is missing, or old enough to ask about again."
  (cond ((null info) t)
        ((memq (plist-get info :state) '(merged closed)) nil)
        (t (> (- (float-time) (or (plist-get info :fetched) 0))
              shipit-magit-worktrees-ttl))))

(defun shipit-magit-worktrees--remember (repo found)
  "Cache the pull request plists in FOUND for REPO."
  (dolist (info found)
    (puthash (shipit-magit-worktrees--key repo (plist-get info :number))
             info shipit-magit-worktrees--states)))

(defun shipit-magit-worktrees--fetch (repo numbers buffer)
  "Ask REPO about NUMBERS in the background, then refresh BUFFER."
  (when-let* ((wanted (seq-remove
                       (lambda (n)
                         (member (shipit-magit-worktrees--key repo n)
                                 shipit-magit-worktrees--pending))
                       numbers)))
    (dolist (n wanted)
      (push (shipit-magit-worktrees--key repo n)
            shipit-magit-worktrees--pending))
    (shipit--debug-log "Worktree section: asking about %s in %s" wanted repo)
    (shipit--graphql-request
     (shipit-magit-worktrees--query repo wanted) nil
     (lambda (response)
       (shipit-magit-worktrees--receive repo wanted response buffer)))))

(defun shipit-magit-worktrees--receive (repo wanted response buffer)
  "Take in RESPONSE, the forge's answer about WANTED in REPO, and refresh BUFFER.
Only an answer refreshes.  A refresh asks about whatever is still
unknown, so after a failure it would ask again at once, and again when
that answer came, as fast as the forge replies."
  (dolist (n wanted)
    (setq shipit-magit-worktrees--pending
          (delete (shipit-magit-worktrees--key repo n)
                  shipit-magit-worktrees--pending)))
  (when (shipit-magit-worktrees--answered-p response)
    (let ((found (shipit-magit-worktrees--parse response)))
      (shipit-magit-worktrees--remember
       repo (append found (shipit-magit-worktrees--missing wanted found)))
      (shipit-magit-worktrees--refresh-buffer buffer))))

(defun shipit-magit-worktrees--refresh-buffer (buffer)
  "Refresh BUFFER when it is still a live magit buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'magit-mode)
        (magit-refresh-buffer)))))

;;;; Sizes

(defun shipit-magit-worktrees--size (path)
  "Size of PATH in bytes when it has been measured.
Keys drop the trailing slash `magit-list-worktrees' adds, since du
echoes back the argument it was given."
  (gethash (directory-file-name path) shipit-magit-worktrees--sizes))

(defun shipit-magit-worktrees--measure (paths buffer)
  "Measure PATHS in the background, then refresh BUFFER."
  (when-let* ((wanted (seq-remove #'shipit-magit-worktrees--size paths))
              ((executable-find "du")))
    (let ((out (generate-new-buffer " *shipit-worktree-du*")))
      (make-process
       :name "shipit-worktree-du" :buffer out :noquery t
       :command (append (list "du" "-s" "-B1") wanted)
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (shipit-magit-worktrees--record-sizes
            (with-current-buffer out (buffer-string)))
           (kill-buffer out)
           (shipit-magit-worktrees--refresh-buffer buffer)))))))

(defun shipit-magit-worktrees--record-sizes (output)
  "Cache the sizes du reported in OUTPUT."
  (dolist (line (split-string output "\n" t))
    (when (string-match "\\`\\([0-9]+\\)[ \t]+\\(.+\\)\\'" line)
      (puthash (match-string 2 line)
               (string-to-number (match-string 1 line))
               shipit-magit-worktrees--sizes))))

;;;; Rendering

(defvar shipit-magit-worktrees-reference-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mouse-1>" #'shipit-magit-worktrees-menu)
    (keymap-set map "<mouse-2>" #'shipit-magit-worktrees-visit-pr)
    map)
  "Keymap on the pull request reference in the margin.")

(defun shipit-magit-worktrees--human-size (bytes)
  "BYTES as a short human-readable string."
  (let ((units '("B" "K" "M" "G" "T")) (size (float bytes)))
    (while (and (>= size 1024) (cdr units))
      (setq size (/ size 1024) units (cdr units)))
    (format (if (or (>= size 100) (= size (ftruncate size))) "%.0f%s" "%.1f%s")
            size (car units))))

(defun shipit-magit-worktrees--state-text (state)
  "STATE as the coloured word shipit uses for it everywhere else."
  (if state
      (shipit--colorize-pr-state (symbol-name state))
    (propertize "…" 'font-lock-face 'magit-log-date)))

(defun shipit-magit-worktrees--reference (info)
  "The pull request reference in INFO, as a button."
  (let ((number (plist-get info :number)))
    (propertize (format "#%d" number)
                'font-lock-face 'magit-log-date
                'mouse-face 'highlight
                'follow-link t
                'shipit-pr-number number
                'keymap shipit-magit-worktrees-reference-map
                'help-echo (format "Pull request %d%s — mouse-1 for actions"
                                   number
                                   (if-let* ((title (plist-get info :title)))
                                       (concat ": " title)
                                     "")))))

(defun shipit-magit-worktrees--margin-text (info)
  "Render INFO for the right margin, within its `:width'."
  (let* ((width (or (plist-get info :width) 0))
         (at (plist-get info :merged-at))
         (size (and (plist-get info :details) (plist-get info :size)))
         (text (string-join
                (delq nil
                      (list (shipit-magit-worktrees--reference info)
                            (shipit-magit-worktrees--state-text
                             (plist-get info :state))
                            (and at (propertize (substring at 5 10)
                                                'font-lock-face 'magit-log-date))
                            (and size
                                 (propertize
                                  (shipit-magit-worktrees--human-size size)
                                  'font-lock-face 'magit-log-date))))
                " ")))
    (if (> width 0)
        (truncate-string-to-width text width nil ?\s (magit--ellipsis 'margin))
      text)))

(defun shipit-magit-worktrees--margin-config ()
  "The margin's (WIDTH . DETAILS) for this buffer."
  (let ((config (or (bound-and-true-p magit--right-margin-config)
                    (when-let* ((option (magit--right-margin-option)))
                      (symbol-value option)))))
    (pcase-let ((`(,_init ,_style ,width ,details ,_dwidth) config))
      (cons (if (numberp width) width 0) details))))

(defun shipit-magit-worktrees--head (config align)
  "The revision column magit shows for worktree CONFIG, padded to ALIGN."
  (pcase-let* ((`(,directory ,commit ,branch ,bare) config)
               (head (cond
                      (branch
                       (propertize branch 'font-lock-face
                                   (if (equal branch (magit-get-current-branch))
                                       'magit-branch-current
                                     'magit-branch-local)))
                      (commit
                       ;; A worktree can sit on a commit git can no longer
                       ;; resolve -- a force-pushed head, pruned since --
                       ;; and abbreviating that returns nil.
                       (propertize (or (magit-rev-abbrev commit)
                                       (substring commit 0 (min 11 (length commit))))
                                   'font-lock-face
                                   (if (file-equal-p default-directory directory)
                                       '(magit-hash magit-branch-current)
                                     'magit-hash)))
                      (bare "(bare)")
                      (t ""))))
    (if align
        (concat head (make-string (max 1 (- align (string-width head))) ?\s))
      head)))

(defun shipit-magit-worktrees--path-text (path)
  "PATH as magit spells it in the worktree section."
  (let ((relative (file-relative-name path))
        (absolute (abbreviate-file-name path)))
    (if (or (> (string-width relative) (string-width absolute))
            (equal relative "./"))
        absolute
      relative)))

(defun shipit-magit-worktrees--insert-row (config repo align margin)
  "Insert the row for worktree CONFIG in REPO, padded to ALIGN.
MARGIN is the (WIDTH . DETAILS) the margin offers."
  (let* ((path (car config))
         (info (shipit-magit-worktrees--info repo path)))
    (magit-insert-section (worktree path)
      (insert (shipit-magit-worktrees--head config align))
      (insert (if-let* ((number (plist-get info :number)))
                  (propertize (shipit-magit-worktrees--path-text path)
                              'shipit-pr-number number)
                (shipit-magit-worktrees--path-text path)))
      (insert ?\n)
      ;; the margin overlay attaches to the line just ended
      (when-let* ((info)
                  (text (funcall shipit-magit-worktrees-margin-function
                                 (append info (list :width (car margin)
                                                    :details (cdr margin))))))
        (magit-make-margin-overlay text)))))

;;;###autoload
(defun shipit-magit-worktrees-insert-section ()
  "Insert magit's worktree section, with each pull request's state.
Drop-in replacement for `magit-insert-worktrees': same sections, same
keymap, plus the forge's answer in the margin."
  (let ((worktrees (magit-list-worktrees)))
    (when (length> worktrees 1)
      (shipit-magit-worktrees--widen-margin)
      (let ((repo (shipit--get-repo-from-remote))
            (margin (shipit-magit-worktrees--margin-config))
            (align (1+ (apply #'max
                              (mapcar (lambda (config)
                                        (string-width
                                         (shipit-magit-worktrees--head config nil)))
                                      worktrees)))))
        (magit-insert-section (worktrees)
          (magit-insert-heading t "Worktrees")
          (dolist (config worktrees)
            (shipit-magit-worktrees--insert-row config repo align margin))
          (insert ?\n))
        (shipit-magit-worktrees--refresh-missing repo worktrees)))))

(defun shipit-magit-worktrees--refresh-missing (repo worktrees)
  "Ask REPO about the WORKTREES whose state or size is not known yet."
  (when repo
    (let (numbers paths)
      (dolist (config worktrees)
        (let* ((path (car config))
               (number (shipit-magit-worktrees--number path)))
          (when (and number
                     (shipit-magit-worktrees--stale-p
                      (gethash (shipit-magit-worktrees--key repo number)
                               shipit-magit-worktrees--states)))
            (push number numbers))
          (when (and number
                     shipit-magit-worktrees-show-size
                     (null (shipit-magit-worktrees--size path)))
            (push (directory-file-name path) paths))))
      (when numbers
        (shipit-magit-worktrees--fetch repo (nreverse numbers) (current-buffer)))
      (when paths
        (shipit-magit-worktrees--measure (nreverse paths) (current-buffer))))))

;;;; Actions

(defun shipit-magit-worktrees--pr-url (repo number)
  "URL of pull request NUMBER in REPO."
  (format "https://github.com/%s/pull/%d" repo number))

(defun shipit-magit-worktrees--number-at-point ()
  "Pull request number of the worktree at point."
  (or (get-text-property (point) 'shipit-pr-number)
      (when-let* ((section (magit-current-section))
                  ((magit-section-match 'worktree section)))
        (shipit-magit-worktrees--number (oref section value)))))

(defun shipit-magit-worktrees--require-number ()
  "Pull request number of the worktree at point, or an error."
  (or (shipit-magit-worktrees--number-at-point)
      (user-error "No pull request worktree here")))

;;;###autoload
(defun shipit-magit-worktrees-visit-pr ()
  "Open the pull request of the worktree at point in shipit."
  (interactive)
  (shipit-open-pr-buffer (shipit-magit-worktrees--require-number)))

;;;###autoload
(defun shipit-magit-worktrees-browse-pr ()
  "Open the pull request of the worktree at point in a browser."
  (interactive)
  (browse-url (shipit-magit-worktrees--pr-url
               (shipit--get-repo-from-remote)
               (shipit-magit-worktrees--require-number))))

;;;###autoload
(defun shipit-magit-worktrees-copy-pr-url ()
  "Copy the URL of the pull request of the worktree at point."
  (interactive)
  (let ((url (shipit-magit-worktrees--pr-url
              (shipit--get-repo-from-remote)
              (shipit-magit-worktrees--require-number))))
    (kill-new url)
    (message "%s" url)))

;;;###autoload
(defun shipit-magit-worktrees-refresh ()
  "Forget cached states and sizes, then refresh the status buffer."
  (interactive)
  (clrhash shipit-magit-worktrees--states)
  (clrhash shipit-magit-worktrees--sizes)
  (shipit-magit-worktrees--refresh-buffer (current-buffer)))

(defun shipit-magit-worktrees--merged-worktrees (repo)
  "Worktrees of REPO whose pull request has been merged."
  (seq-filter
   (lambda (config)
     (when-let* ((number (shipit-magit-worktrees--number (car config))))
       (eq 'merged (plist-get (gethash (shipit-magit-worktrees--key repo number)
                                       shipit-magit-worktrees--states)
                              :state))))
   (cdr (magit-list-worktrees))))

;;;###autoload
(defun shipit-magit-worktrees-remove-merged (&optional force)
  "Remove every worktree whose pull request has been merged.
Asks first, listing what would go.  With FORCE, remove worktrees
holding uncommitted changes or untracked files too -- build output
usually means every one of them does."
  (interactive "P")
  (let* ((repo (shipit--get-repo-from-remote))
         (merged (and repo (shipit-magit-worktrees--merged-worktrees repo))))
    (cond
     ((null merged) (message "No worktree here has a merged pull request"))
     ((yes-or-no-p
       (format "Remove %d merged worktree%s (%s)? "
               (length merged) (if (cdr merged) "s" "")
               (mapconcat (lambda (config)
                            (file-name-nondirectory
                             (directory-file-name (car config))))
                          merged ", ")))
      (dolist (config merged)
        (magit-run-git "worktree" "remove" (and force "--force") (car config)))
      (magit-run-git "worktree" "prune")))))

;;;###autoload (autoload 'shipit-magit-worktrees-menu "shipit-magit-worktrees" nil t)
(transient-define-prefix shipit-magit-worktrees-menu ()
  "Actions for the pull request worktree at point."
  ["Pull request"
   ("RET" "Open in shipit" shipit-magit-worktrees-visit-pr)
   ("o" "Open in browser" shipit-magit-worktrees-browse-pr)
   ("y" "Copy PR URL" shipit-magit-worktrees-copy-pr-url)]
  ["Worktree"
   ("v" "Visit worktree" magit-worktree-status)
   ("k" "Delete worktree" magit-worktree-delete)
   ("K" "Remove all merged" shipit-magit-worktrees-remove-merged)]
  ["Section"
   ("g" "Refresh states and sizes" shipit-magit-worktrees-refresh)])

(defun shipit-magit-worktrees--dwim-match-p ()
  "Non-nil when point is on a worktree row that has a pull request."
  (and (fboundp 'magit-current-section)
       (magit-current-section)
       (magit-section-match 'worktree (magit-current-section))
       (shipit-magit-worktrees--number-at-point)
       t))

(when shipit-magit-worktrees-enabled
  (shipit-magit-worktrees--install t))

(with-eval-after-load 'shipit-pr-actions
  (shipit-register-dwim-handler
   'magit-worktree
   #'shipit-magit-worktrees--dwim-match-p
   #'shipit-magit-worktrees-menu))

;; The same key shipit uses for `shipit-dwim' in its own buffers, so a
;; worktree row answers to it too.  Scoped to worktree sections, where
;; `comment-dwim' has nothing to offer anyway.
(keymap-set magit-worktree-section-map "M-;" #'shipit-magit-worktrees-menu)

(provide 'shipit-magit-worktrees)
;;; shipit-magit-worktrees.el ends here
