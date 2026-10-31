;;; shipit-branches-buffer.el --- A buffer for the branches of a repository -*- lexical-binding: t; -*-

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
;; `shipit-branches' shows the branches of a repository in the groups
;; of the branches page of GitHub: the default branch, the branches of
;; the user, the active branches of others, and the stale ones.  A line
;; shows the time and the author of the last commit of a branch, and its
;; pull request.  `RET' opens the pull request in shipit, or else shows
;; there how the branch differs from the default branch.  `f' shows only
;; the branches whose name or author holds the words that are typed.
;; `w' watches the repository, so that a new branch gives a
;; notification.  `SPC' opens a menu of all that, and embark offers the
;; actions on the line of a branch.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'transient)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-branches)

(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit--get-pr-state-icon "shipit-render")
(declare-function shipit--get-pr-state-emoji "shipit-render")
(declare-function shipit-open-pr-buffer "shipit-buffer")
(declare-function shipit-open-repo-buffer "shipit-repo-buffer")
(declare-function shipit-toggle-timestamp-format "shipit-commands")
(declare-function shipit--read-repo "shipit-commands")

(put 'shipit-branches-group 'magit-section t)
(put 'shipit-branches-branch 'magit-section t)

(defconst shipit-branches-buffer--group-names
  '((default . "Default") (yours . "Yours") (active . "Active") (stale . "Stale"))
  "The heading of each group of branches.")

(defconst shipit-branches-buffer--widths '((name . 40) (time . 16) (author . 16))
  "The width of each column of the line of a branch, but the last.")

(defvar-local shipit-branches-buffer-repo nil
  "The repository whose branches this buffer shows, as owner/name.")

(defvar-local shipit-branches-buffer--branches nil
  "The branches of the repository of this buffer, as the backend gave them.")

(defvar-local shipit-branches-buffer--filter ""
  "The words that the name or the author of a branch that is shown holds.")

;;; The line of a branch

(defun shipit-branches-buffer--cell (column text &optional face)
  "TEXT as the cell of COLUMN, as wide as the column, in FACE when given."
  (let ((width (alist-get column shipit-branches-buffer--widths)))
    (propertize (if width (truncate-string-to-width text width nil ?\s) text)
                'font-lock-face face
                'shipit-branches-column column)))

(defun shipit-branches-buffer--time-cell (time)
  "TIME, of the last commit of a branch, at the right of its column.
The cell keeps TIME as the backend wrote it, so that a change of
`shipit-timestamp-format' writes the other form into the same width."
  (propertize (string-pad (shipit--format-timestamp time)
                          (alist-get 'time shipit-branches-buffer--widths) nil t)
              'font-lock-face 'shipit-timestamp-face
              'shipit-raw-timestamp time
              'shipit-branches-column 'time))

(defun shipit-branches-buffer--pr-cell (pr)
  "PR, the pull request of a branch, as its icon and a cell of its number
and state.  A branch without one has nothing there."
  (if pr
      (let-alist pr
        (concat (shipit--get-pr-state-icon .state (shipit--get-pr-state-emoji .state)) " "
                (propertize (format "#%d %s" .number .state)
                            'shipit-branches-column 'pr)))
    ""))

(defun shipit-branches-buffer--line (branch)
  "The line of BRANCH: its name, the time and the author of its last
commit, and its pull request.
The line starts with a space before the icon.  Emacs shows an icon that
starts where a fold starts, so the icon of the first branch of a folded
group would stand before the heading of the next group."
  (let-alist branch
    (concat "  " (shipit--get-pr-field-icon "branch" "🌿") " "
            (shipit-branches-buffer--cell 'name .name 'font-lock-constant-face) "  "
            (shipit-branches-buffer--time-cell .time) "  "
            (shipit-branches-buffer--cell 'author .author 'shipit-username-face) "  "
            (shipit-branches-buffer--pr-cell .pr))))

;;; The buffer

(defun shipit-branches-buffer--watch-description ()
  "Whether the new branches of this repository give a notification."
  (format "New branch notifications: %s"
          (if (shipit-branches-watched-p shipit-branches-buffer-repo) "on" "off")))

(defun shipit-branches-buffer--insert-header ()
  "Insert the repository, whether its new branches give a notification,
and the filter."
  (insert (propertize shipit-branches-buffer-repo 'font-lock-face 'magit-section-heading)
          "\n" (shipit-branches-buffer--watch-description) "\n")
  (unless (string-empty-p shipit-branches-buffer--filter)
    (insert (format "Filter: %s\n" shipit-branches-buffer--filter)))
  (insert "\n"))

(defun shipit-branches-buffer--matches-p (branch words)
  "Whether the name or the author of BRANCH holds each of WORDS.
Case does not matter."
  (let ((text (downcase (concat (alist-get 'name branch) " " (alist-get 'author branch)))))
    (seq-every-p (lambda (word) (string-search (downcase word) text)) words)))

(defun shipit-branches-buffer--shown ()
  "The branches of this buffer that the filter lets through."
  (let ((words (split-string shipit-branches-buffer--filter nil t)))
    (seq-filter (lambda (branch) (shipit-branches-buffer--matches-p branch words))
                shipit-branches-buffer--branches)))

(defun shipit-branches-buffer--insert-group (group branches)
  "Insert GROUP with its BRANCHES.  The group of the stale ones is folded."
  (magit-insert-section (shipit-branches-group group (eq group 'stale))
    (magit-insert-heading
      (propertize (alist-get group shipit-branches-buffer--group-names)
                  'font-lock-face 'magit-section-heading)
      (format " (%d)" (length branches)))
    (dolist (branch branches)
      (magit-insert-section (shipit-branches-branch branch)
        (magit-insert-heading (shipit-branches-buffer--line branch))))
    (insert "\n")))

(defun shipit-branches-buffer--render ()
  "Show the branches that this buffer holds, in their groups."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (root)
      (shipit-branches-buffer--insert-header)
      (let ((shown (shipit-branches-buffer--shown)))
        (cond ((null shipit-branches-buffer--branches)
               (insert "No branches came for this repository.\n"))
              ((null shown)
               (insert "No branch matches the filter.\n"))
              (t
               (pcase-dolist (`(,group . ,branches) (shipit-branches-groups shown))
                 (shipit-branches-buffer--insert-group group branches))))))
    ;; magit-section folds a section that starts folded only when it is
    ;; shown, so the root is shown once the groups are in.
    (magit-section-show magit-root-section)))

(defun shipit-branches-buffer--rerender ()
  "Show the branches again, with point on the line where it was."
  (let ((line (line-number-at-pos)))
    (shipit-branches-buffer--render)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun shipit-branches-buffer--fetch ()
  "Fetch the branches of this buffer and show them."
  (let ((buffer (current-buffer)))
    (shipit-branches-fetch-async
     shipit-branches-buffer-repo
     (lambda (branches)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq shipit-branches-buffer--branches branches)
           (shipit-branches-buffer--rerender)))))))

(defun shipit-branches-refresh (&optional _ignore-auto _noconfirm)
  "Read the branches again and show them."
  (interactive)
  (shipit-branches-buffer--fetch))

(defun shipit-branches-set-filter ()
  "Show only the branches whose name or author holds each word typed.
The branches change as you type.  An empty filter shows all branches."
  (interactive)
  (shipit--read-filter-live
   "Filter branches by name or author: " shipit-branches-buffer--filter
   (lambda (text)
     (setq shipit-branches-buffer--filter text)
     (shipit-branches-buffer--rerender))))

;;; What a branch opens

(defun shipit-branches-buffer--branch-at-point ()
  "The branch at point.  No branch there is a user error."
  (or (magit-section-value-if 'shipit-branches-branch)
      (user-error "No branch at point")))

(defun shipit-branches-open ()
  "Open the branch at point in shipit.
A branch with a pull request opens it, and the default branch opens the
page of the repository.  Another branch shows how it differs from the
default branch: its commits and changed files.  On the heading of a
group, the group folds or unfolds."
  (interactive)
  (if (magit-section-match 'shipit-branches-group)
      (magit-section-toggle (magit-current-section))
    (let-alist (shipit-branches-buffer--branch-at-point)
      (cond (.pr (require 'shipit-buffer)
                 (shipit-open-pr-buffer .pr.number shipit-branches-buffer-repo))
            (.default (require 'shipit-repo-buffer)
                      (shipit-open-repo-buffer shipit-branches-buffer-repo))
            (t (shipit-branches-open-branch shipit-branches-buffer-repo .name .base))))))

(defun shipit-branches-browse ()
  "Show the page of the branch at point in the browser."
  (interactive)
  (browse-url (alist-get 'url (shipit-branches-buffer--branch-at-point))))

(defun shipit-branches-browse-compare ()
  "Show in the browser how the branch at point differs from the default."
  (interactive)
  (let-alist (shipit-branches-buffer--branch-at-point)
    (browse-url (shipit-branches-compare-url shipit-branches-buffer-repo .base .name))))

(defun shipit-branches-buffer--copy (text)
  "Copy TEXT to the kill ring, and say so."
  (kill-new text)
  (message "Copied %s" text))

(defun shipit-branches-copy-name ()
  "Copy the name of the branch at point.
With an active region, copy the region, as `kill-ring-save' does."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (shipit-branches-buffer--copy
     (alist-get 'name (shipit-branches-buffer--branch-at-point)))))

(defun shipit-branches-copy-url ()
  "Copy the address of the page of the branch at point."
  (interactive)
  (shipit-branches-buffer--copy
   (alist-get 'url (shipit-branches-buffer--branch-at-point))))

(defun shipit-branches-toggle-watch-here ()
  "Watch the repository of this buffer, or stop watching it.
A watched repository gives a notification for each new branch that
someone else pushes."
  (interactive)
  (shipit-branches-toggle-watch shipit-branches-buffer-repo
                                shipit-branches-buffer--branches)
  (shipit-branches-buffer--rerender)
  (message "%s for %s" (shipit-branches-buffer--watch-description)
           shipit-branches-buffer-repo))

;;; The menu

(defun shipit-branches-buffer--filter-description ()
  "What the menu says of the filter."
  (if (string-empty-p shipit-branches-buffer--filter)
      "Filter"
    (format "Filter: %s" shipit-branches-buffer--filter)))

(transient-define-prefix shipit-branches-menu ()
  "Act on the branch at point, or choose which branches are shown.
SPC closes the menu again."
  [["Branch"
    ("RET" "Open in shipit" shipit-branches-open)
    ("o" "Open in browser" shipit-branches-browse)
    ("c" "Compare in browser" shipit-branches-browse-compare)
    ("y" "Copy name" shipit-branches-copy-name)
    ("u" "Copy URL" shipit-branches-copy-url)]
   ["Branches"
    ("f" shipit-branches-set-filter
     :description shipit-branches-buffer--filter-description)
    ("g" "Refresh" shipit-branches-refresh)
    ("L" "Toggle time format" shipit-toggle-timestamp-format)
    ("w" shipit-branches-toggle-watch-here
     :description shipit-branches-buffer--watch-description)]
   [("SPC" "Close" transient-quit-one)]])

;;; Embark on the line of a branch

(defun shipit-branches-embark-target ()
  "Embark target finder: the branch whose line point is on.
The kind of the target is `shipit-branch' and its value is the name of
the branch."
  (when-let* (((derived-mode-p 'shipit-branches-mode))
              (branch (magit-section-value-if 'shipit-branches-branch)))
    `(shipit-branch ,(alist-get 'name branch)
                    ,(line-beginning-position) . ,(line-end-position))))

(defvar shipit-branches-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-branches-open)
    (define-key map (kbd "o") #'shipit-branches-open)
    (define-key map (kbd "b") #'shipit-branches-browse)
    (define-key map (kbd "c") #'shipit-branches-browse-compare)
    (define-key map (kbd "w") #'shipit-branches-copy-name)
    (define-key map (kbd "u") #'shipit-branches-copy-url)
    map)
  "Embark actions on the line of a branch.
The letters are those of the actions on an issue key in code.")

(defvar embark-target-finders)
(defvar embark-keymap-alist)

(defun shipit-branches--embark-register ()
  "Make embark offer `shipit-branches-embark-map' on the line of a branch."
  (add-to-list 'embark-target-finders #'shipit-branches-embark-target)
  (add-to-list 'embark-keymap-alist '(shipit-branch . shipit-branches-embark-map)))

(with-eval-after-load 'embark
  (shipit-branches--embark-register))

;;; The mode

(defvar shipit-branches-mode-map (make-sparse-keymap)
  "Keymap for `shipit-branches-mode'.")

;; The keys are bound here and not where the map is made, so that they
;; reach a running Emacs when the file is loaded again.
(set-keymap-parent shipit-branches-mode-map magit-section-mode-map)
(define-key shipit-branches-mode-map (kbd "g") #'shipit-branches-refresh)
(define-key shipit-branches-mode-map (kbd "q") #'quit-window)
(define-key shipit-branches-mode-map (kbd "RET") #'shipit-branches-open)
(define-key shipit-branches-mode-map (kbd "o") #'shipit-branches-browse)
(define-key shipit-branches-mode-map (kbd "w") #'shipit-branches-toggle-watch-here)
(define-key shipit-branches-mode-map (kbd "L") #'shipit-toggle-timestamp-format)
(define-key shipit-branches-mode-map (kbd "f") #'shipit-branches-set-filter)
(define-key shipit-branches-mode-map (kbd "M-w") #'shipit-branches-copy-name)
;; SPC as in Explore and the canvas buffers.
(define-key shipit-branches-mode-map (kbd "SPC") #'shipit-branches-menu)

;; meow takes SPC in its motion state.  The menu gets it in the insert
;; state.
(with-eval-after-load 'meow
  (shipit--start-in-meow-insert-state 'shipit-branches-mode))

(define-derived-mode shipit-branches-mode magit-section-mode "Shipit-Branches"
  "Major mode of the buffer that shows the branches of a repository.

\\{shipit-branches-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-branches-refresh)
  (setq-local truncate-lines t)
  (shipit--apply-section-defaults))

(defun shipit-branches-buffer--read-repo ()
  "The repository to show the branches of.
It is the repository of this buffer, unless there is none or a prefix
argument asks for another."
  (or (and (not current-prefix-arg) (shipit--repo-for-buffer))
      (shipit--read-repo "Repository: ")))

;;;###autoload
(defun shipit-branches (repo)
  "Show the branches of REPO, as the branches page of GitHub groups them.
The default branch, your branches, the active branches of others and
the stale ones each have a group, and a line for each branch shows the
time and the author of its last commit and its pull request.

\\<shipit-branches-mode-map>\\[shipit-branches-open] opens the branch at point in shipit: its pull request, or how it
differs from the default branch.  \\[shipit-branches-set-filter] shows only the branches whose name
or author holds the words you type.  \\[shipit-branches-toggle-watch-here] watches the repository, so
that each new branch that someone else pushes gives a notification, and
\\[shipit-branches-menu] opens a menu of all actions.  With a prefix argument, you are
asked which repository.  Each time the branches are opened, the
filter is cleared and the groups fold as they do at first."
  (interactive (list (shipit-branches-buffer--read-repo)))
  (let ((buffer (get-buffer-create (format "*shipit-branches: %s*" repo))))
    (with-current-buffer buffer
      ;; The mode starts each time: `shipit-reload' makes its keymap anew,
      ;; and a buffer from before keeps the old one and its meow state.
      (shipit-branches-mode)
      (setq shipit-branches-buffer-repo repo)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Fetching the branches of " repo "...\n"))
      (shipit-branches-buffer--fetch))
    (switch-to-buffer buffer)
    buffer))

(provide 'shipit-branches-buffer)
;;; shipit-branches-buffer.el ends here
