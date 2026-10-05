;;; shipit-explore-buffer.el --- A buffer for the feed of Explore -*- lexical-binding: t; -*-

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
;; `shipit-explore' shows the feed of what the people that the user
;; follows and the repositories that the user watches do, a line for
;; each entry, the newest first.  The entries that were not seen before
;; stand out.  `RET' opens the repository of an entry in shipit, or its
;; releases at the tag of a release.

;;; Code:

(require 'cl-lib)
(require 'format-spec)
(require 'magit-section)
(require 'transient)
(require 'shipit-core)
(require 'shipit-explore)

(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit-open-repo-buffer "shipit-repo-buffer")
(declare-function shipit-open-releases-buffer "shipit-releases-buffer")
(declare-function shipit-toggle-timestamp-format "shipit-commands")

(put 'shipit-explore-entry 'magit-section t)

(defconst shipit-explore-buffer--name "*shipit-explore*"
  "The name of the buffer that shows the feed.")

(defconst shipit-explore-buffer--icons
  '(("star" "star" "⭐")
    ("fork" "repo-fork" "🍴")
    ("create" "repo" "📦")
    ("release" "repo-release" "🚀")
    ("public" "repo-public" "🌐"))
  "The icon of each kind of entry.
After the kind come the name that `shipit--get-pr-field-icon' knows the
icon by, and the emoji that stands there without SVG icons.")

(defun shipit-explore-buffer--icon (entry)
  "The icon of the kind of ENTRY."
  (pcase-let ((`(,_ ,name ,emoji)
               (or (assoc (cdr (assq 'kind entry)) shipit-explore-buffer--icons)
                   (error "No icon for an entry of the kind %S" (cdr (assq 'kind entry))))))
    (shipit--get-pr-field-icon name emoji)))

(defconst shipit-explore-buffer--deeds
  '(("star" . "starred")
    ("fork" . "forked to %d")
    ("create" . "created")
    ("release" . "released %d")
    ("public" . "made public"))
  "What was done in an entry of each kind, in words, after who did it.
%d is the detail of the entry.  The repository has a column of its own.")

(defconst shipit-explore-buffer--time-width 16
  "The width of the column of the time, as in the notifications buffer.")

(defvar-local shipit-explore-buffer--seen-before nil
  "The time up to which the feed was seen when this visit of Explore began.
An entry after it is new to this visit, however often the feed is read
again.  It is nil when nothing was seen before.")

(defun shipit-explore-buffer--width (column fallback)
  "The width of COLUMN, or FALLBACK.
`shipit-notifications-column-widths' has the width, so that the columns
are those of the notifications buffer."
  (or (cdr (assq column shipit-notifications-column-widths)) fallback))

(defun shipit-explore-buffer--cell (text width column &optional face)
  "TEXT as the cell of COLUMN, WIDTH wide, in FACE when given."
  (let ((cell (truncate-string-to-width text width nil ?\s)))
    (when face
      (setq cell (propertize cell 'font-lock-face face)))
    (propertize cell 'shipit-explore-column column)))

(defun shipit-explore-buffer--subject (entry new)
  "Who did what in ENTRY; with NEW the words stand out."
  (let ((deed (format-spec (cdr (assoc (cdr (assq 'kind entry))
                                       shipit-explore-buffer--deeds))
                           `((?d . ,(or (cdr (assq 'detail entry)) ""))))))
    (concat (propertize (cdr (assq 'actor entry)) 'font-lock-face 'shipit-username-face)
            " "
            (if new (propertize deed 'font-lock-face 'bold) deed))))

(defun shipit-explore-buffer--window-width ()
  "The width of the window that shows this buffer, or 120 when none does."
  (if-let* ((window (get-buffer-window (current-buffer))))
      (window-body-width window)
    120))

(defun shipit-explore-buffer--time-field (entry)
  "The time of ENTRY at the right of a field of the width of a date.
The whole field holds the time as GitHub gave it, so that a change of
`shipit-timestamp-format' writes the other form into the same width."
  (let* ((raw (cdr (assq 'time entry)))
         (time (shipit--format-timestamp raw))
         (lead (max 0 (- shipit-explore-buffer--time-width (string-width time)))))
    (propertize (concat (make-string lead ?\s) time)
                'font-lock-face 'shipit-timestamp-face
                'shipit-raw-timestamp raw
                'shipit-explore-column 'time)))

(defun shipit-explore-buffer--time-cell (entry used)
  "The time of ENTRY, put at the right edge of the window.
USED is the width of what stands before it on the line."
  (let ((pad (max 2 (- (shipit-explore-buffer--window-width) used
                       shipit-explore-buffer--time-width 3))))
    (concat (make-string pad ?\s) (shipit-explore-buffer--time-field entry))))

(defun shipit-explore-buffer--line (entry new)
  "The line of ENTRY, in the columns of the notifications buffer.
The icon of its kind, the repository, who did what, the kind, and at
the right the time.  With NEW the entry is new to this visit, and what
was done stands out."
  (let* ((gap (make-string (or shipit-notifications-column-spacing 2) ?\s))
         (left (concat
                (shipit-explore-buffer--icon entry) " "
                (shipit-explore-buffer--cell (cdr (assq 'repo entry))
                                             (shipit-explore-buffer--width 'repo 30)
                                             'repo 'font-lock-constant-face)
                gap
                (shipit-explore-buffer--cell (shipit-explore-buffer--subject entry new)
                                             (shipit-explore-buffer--width 'title 45)
                                             'subject)
                gap
                (shipit-explore-buffer--cell (cdr (assq 'kind entry))
                                             (shipit-explore-buffer--width 'reason 12)
                                             'kind 'font-lock-keyword-face))))
    (propertize (concat left (shipit-explore-buffer--time-cell entry (string-width left)))
                'shipit-explore-entry t
                'shipit-explore-new (and new t))))

(defun shipit-explore-buffer--insert-entry (entry new)
  "Insert the section of ENTRY; NEW says that it is new to this visit."
  (magit-insert-section (shipit-explore-entry entry)
    (magit-insert-heading (shipit-explore-buffer--line entry new))))

(defun shipit-explore-buffer--new-p (entry)
  "Whether ENTRY came after what was seen when this visit of Explore began."
  (or (null shipit-explore-buffer--seen-before)
      (string> (cdr (assq 'time entry)) shipit-explore-buffer--seen-before)))

(defun shipit-explore-buffer--render (entries)
  "Show ENTRIES in this buffer, those that are new to this visit as new."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (root)
      (if (null entries)
          (insert "Nothing in the feed for what the menu asks for.\n")
        (dolist (entry entries)
          (shipit-explore-buffer--insert-entry
           entry (shipit-explore-buffer--new-p entry)))))
    (goto-char (point-min))))

(defun shipit-explore-refresh (&optional _ignore-auto _noconfirm)
  "Read the feed again and show it.
What is shown counts as seen from then on.  The entries that were new
when Explore was opened still stand out."
  (interactive)
  (let ((buffer (current-buffer)))
    (shipit-explore-fetch-async
     (lambda (entries)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (shipit-explore-buffer--render entries)
           (shipit-explore-mark-seen entries)))))))

(defun shipit-explore-open ()
  "Open the entry at point in shipit.
A release opens the releases of its repository at its tag, and any
other entry the page of its repository."
  (interactive)
  (let* ((entry (or (magit-section-value-if 'shipit-explore-entry)
                    (user-error "No entry of the feed at point")))
         (repo (cdr (assq 'repo entry))))
    (if (equal (cdr (assq 'kind entry)) "release")
        (progn (require 'shipit-releases-buffer)
               (shipit-open-releases-buffer repo (cdr (assq 'detail entry))))
      (require 'shipit-repo-buffer)
      (shipit-open-repo-buffer repo))))

;;; The menu of what is shown

(defconst shipit-explore-buffer--kind-names
  '(("star" . "Stars") ("fork" . "Forks") ("create" . "New repos")
    ("release" . "Releases") ("public" . "Made public"))
  "What the menu calls the entries of each kind.")

(defun shipit-explore-buffer--kind-description (kind)
  "What the menu says of KIND: its name, and whether it is shown.
The words are those of the menu of the notifications buffer."
  (format "%s: %s"
          (cdr (assoc kind shipit-explore-buffer--kind-names))
          (if (member kind shipit-explore-kinds) "on" "off")))

(defun shipit-explore-buffer--about-description ()
  "What the menu says of whose repositories the entries are about."
  (format "Repos: %s"
          (pcase shipit-explore-about
            ('all "all")
            ('mine "mine")
            ('others "others"))))

(defun shipit-explore-buffer--toggle-kind (kind)
  "Show the entries of KIND when they are hidden, and hide them when shown.
The feed is read again."
  (setq shipit-explore-kinds
        (if (member kind shipit-explore-kinds)
            (remove kind shipit-explore-kinds)
          (append shipit-explore-kinds (list kind))))
  (shipit-explore-refresh))

(defun shipit-explore-toggle-stars ()
  "Show or hide the stars of the feed."
  (interactive)
  (shipit-explore-buffer--toggle-kind "star"))

(defun shipit-explore-toggle-forks ()
  "Show or hide the forks of the feed."
  (interactive)
  (shipit-explore-buffer--toggle-kind "fork"))

(defun shipit-explore-toggle-created ()
  "Show or hide the new repositories of the feed."
  (interactive)
  (shipit-explore-buffer--toggle-kind "create"))

(defun shipit-explore-toggle-releases ()
  "Show or hide the releases of the feed."
  (interactive)
  (shipit-explore-buffer--toggle-kind "release"))

(defun shipit-explore-toggle-public ()
  "Show or hide the repositories of the feed that were made public."
  (interactive)
  (shipit-explore-buffer--toggle-kind "public"))

(defun shipit-explore-cycle-about ()
  "Go on to the next choice of whose repositories the entries are about.
All repositories, then those of the user, then those of others."
  (interactive)
  (setq shipit-explore-about
        (pcase shipit-explore-about
          ('all 'mine)
          ('mine 'others)
          (_ 'all)))
  (shipit-explore-refresh))

(transient-define-prefix shipit-explore-menu ()
  "Choose what Explore shows.
A switch stays for the session, and the notification of new entries
follows it too.  SPC closes the menu again."
  [["Kinds"
    ("s" shipit-explore-toggle-stars
     :description (lambda () (shipit-explore-buffer--kind-description "star"))
     :transient t)
    ("f" shipit-explore-toggle-forks
     :description (lambda () (shipit-explore-buffer--kind-description "fork"))
     :transient t)
    ("c" shipit-explore-toggle-created
     :description (lambda () (shipit-explore-buffer--kind-description "create"))
     :transient t)
    ("r" shipit-explore-toggle-releases
     :description (lambda () (shipit-explore-buffer--kind-description "release"))
     :transient t)
    ("p" shipit-explore-toggle-public
     :description (lambda () (shipit-explore-buffer--kind-description "public"))
     :transient t)]
   ["Repositories"
    ("a" shipit-explore-cycle-about
     :description shipit-explore-buffer--about-description
     :transient t)]
   ["Feed"
    ("g" "Refresh" shipit-explore-refresh :transient t)
    ("SPC" "Close" transient-quit-one)]])

(defvar shipit-explore-mode-map (make-sparse-keymap)
  "Keymap for `shipit-explore-mode'.")

;; The keys are bound here and not where the map is made, so that they
;; reach a running Emacs when the file is loaded again.
(set-keymap-parent shipit-explore-mode-map magit-section-mode-map)
(define-key shipit-explore-mode-map (kbd "g") #'shipit-explore-refresh)
(define-key shipit-explore-mode-map (kbd "q") #'quit-window)
(define-key shipit-explore-mode-map (kbd "RET") #'shipit-explore-open)
(define-key shipit-explore-mode-map (kbd "L") #'shipit-toggle-timestamp-format)
;; SPC as in the canvas buffers, and f as in the notifications buffer.
(define-key shipit-explore-mode-map (kbd "SPC") #'shipit-explore-menu)
(define-key shipit-explore-mode-map (kbd "f") #'shipit-explore-menu)

;; meow takes SPC in its motion state.  The menu gets it in the insert
;; state.
(with-eval-after-load 'meow
  (shipit--start-in-meow-insert-state 'shipit-explore-mode))

(define-derived-mode shipit-explore-mode magit-section-mode "Shipit-Explore"
  "Major mode of the buffer that shows the feed of Explore.

\\{shipit-explore-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-explore-refresh)
  (setq-local truncate-lines t)
  (shipit--apply-section-defaults))

;;;###autoload
(defun shipit-explore ()
  "Show what the people that you follow and the repositories that you watch do.
Someone starred, forked or created a repository, a release came out, a
repository went public: a line for each, the newest first, in the
columns of the notifications buffer.  What you have not seen before
stands out.  \\<shipit-explore-mode-map>\\[shipit-explore-open] opens the repository of the line in shipit, \\[shipit-explore-refresh]
reads the feed again, \\[shipit-toggle-timestamp-format] switches the times between a date and how long
ago, and \\[shipit-explore-menu] opens a menu of what is shown."
  (interactive)
  (let ((buffer (get-buffer-create shipit-explore-buffer--name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'shipit-explore-mode)
        (shipit-explore-mode))
      (setq shipit-explore-buffer--seen-before (shipit-explore--seen))
      (shipit-explore-refresh))
    (switch-to-buffer buffer)
    buffer))

(provide 'shipit-explore-buffer)
;;; shipit-explore-buffer.el ends here
