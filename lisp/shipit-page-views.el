;;; shipit-page-views.el --- Views of a pull request page and an issue page -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A view of a page shows some parts of the page and hides the others.
;; The normal view shows the page in full.  `v' shows the next view of
;; `shipit-page-views', and `V' asks for a view by its name.  The view
;; holds for each page that is open or opens later, until Emacs quits.
;;
;; A part is a section at the top level of a page.  A page draws all
;; its parts, also those that only a view shows, and the view hides the
;; parts that it does not name.  So a switch asks the forge for nothing,
;; and the normal view comes back as it was.
;;
;; Each part gets an overlay that the view makes invisible.  The page
;; gets one when magit-section has drawn the part, also when the page
;; draws the part again after an answer of the forge came in.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'shipit-core)

(defcustom shipit-page-views
  '((normal)
    (overview :pr (summary waiting checks) :issue (assignee linked-prs)))
  "The views of a pull request page and an issue page, in the order of `v'.
Each entry is (NAME :pr PARTS :issue PARTS).  PARTS are the parts of
the page that the view shows.  A view that names no parts for a kind
of page shows that page in full.

The parts of a pull request page are summary, waiting, title,
description, author, created, refs, linked-issue, state,
subscription, url, approval, assignees, labels, worktree, commits,
files, comments, activity and checks.  The parts of an issue page are
assignee, metadata, description, attachments, child-items,
linked-items, similar-items, linked-prs, activity and comments.  The
title line of an issue shows in each view.

The summary of a pull request is its title, its state and its review
status.  The waiting part shows the reviews that ask for changes, and
the threads and comments that wait for your answer.  These two, and
the assignee line of an issue, show only in a view that names them."
  :type '(repeat (cons (symbol :tag "Name")
                       (plist :options ((:pr (repeat symbol))
                                        (:issue (repeat symbol))))))
  :group 'shipit)

(defcustom shipit-page-view 'normal
  "The name of the view that pages show, one of `shipit-page-views'.
\\<shipit-page-mode-map>\\[shipit-page-view-next] and \\[shipit-page-view-choose] \
change it until Emacs quits."
  :type 'symbol
  :group 'shipit)

(defconst shipit-page-view--part-of-type
  '((shipit-page-summary . summary)
    (shipit-page-waiting . waiting)
    (shipit-page-assignee . assignee)
    (shipit-pr . title)
    (shipit-description . description)
    (pr-author . author)
    (pr-created . created)
    (pr-refs . refs)
    (pr-linked-issue . linked-issue)
    (pr-state . state)
    (pr-subscription . subscription)
    (pr-url . url)
    (shipit-worktree-section . worktree)
    (pr-commits . commits)
    (pr-files . files)
    (general-comments . comments)
    (pr-activity . activity)
    (shipit-issue-metadata . metadata)
    (shipit-issue-description . description)
    (issue-attachments . attachments)
    (issue-child-items . child-items)
    (issue-linked-items . linked-items)
    (issue-similar-items . similar-items)
    (issue-linked-prs . linked-prs)
    (issue-activity . activity)
    (issue-pinned-comment . comments)
    (issue-comments . comments))
  "The part of a page that each type of section at its top level is.
A section of a type that is not here is the part of its type.")

(defconst shipit-page-view--view-only-parts '(summary waiting assignee)
  "The parts that show only in a view that names them.")

(defvar-local shipit-page-view-kind nil
  "The kind of page in this buffer, :pr or :issue, or nil for no page.
It picks the parts that a view names for the page.")

(defvar-local shipit-page-view-redraw-function nil
  "A function of no arguments that draws the parts that only a view shows.
It draws them again from what the page has, and asks the forge for
nothing, or nil when the page draws them only once.")

(defvar-local shipit-page-view--shown nil
  "The name of the view that this page shows now.")

(defvar shipit-page-view-unfold-functions nil
  "Alist of a part and the function that unfolds it in a view that names it.
The function gets the section of the part.  It folds or unfolds the
sections inside the part with `shipit-page-view-fold'.  A part that is
not here is unfolded with `magit-section-show'.")

(defvar-local shipit-page-view--inner-folds nil
  "Each section inside a part that a view folded or unfolded, with its old fold.
A full view gives them their fold back.")

(defvar-local shipit-page-view--full-folds nil
  "Whether each part of this page was folded when it last left a full view.
A full view shows each part, so it keeps the folds of the user, and
the page comes back to them.")

(defvar-local shipit-page-view--cover-timer nil
  "The timer that gives the parts of this page their overlays again, or nil.")

;;; The views

(defun shipit-page-view--entry (name)
  "The entry of the view NAME in `shipit-page-views'.
Signals a `user-error' when there is no such view."
  (or (assq name shipit-page-views)
      (user-error "There is no view named `%s' in `shipit-page-views'" name)))

(defun shipit-page-view--named-parts (name)
  "The parts that the view NAME names for the page of this buffer."
  (plist-get (cdr (shipit-page-view--entry name)) shipit-page-view-kind))

(defun shipit-page-view--full-p (name)
  "Whether the view NAME shows the page of this buffer in full."
  (null (shipit-page-view--named-parts name)))

(defun shipit-page-view--shows-p (part)
  "Whether the view `shipit-page-view' shows PART of the page of this buffer."
  (let ((named (shipit-page-view--named-parts shipit-page-view)))
    (if named
        (memq part named)
      (not (memq part shipit-page-view--view-only-parts)))))

(defun shipit-page-view--after (name)
  "The name of the view after the view NAME, or the first after the last one."
  (let ((rest (cdr (memq (shipit-page-view--entry name) shipit-page-views))))
    (car (or (car rest) (car shipit-page-views)))))

;;; The parts and their overlays

(defun shipit-page-view--part (section)
  "The part of the page that SECTION, a section at its top level, is."
  (let ((type (oref section type)))
    (alist-get type shipit-page-view--part-of-type type)))

(defun shipit-page-view--top-level-p (section)
  "Whether SECTION is at the top level of its page: its parent is the root."
  (let ((parent (oref section parent)))
    (and parent (null (oref parent parent)))))

(defun shipit-page-view--parts ()
  "The sections at the top level of this page."
  (and magit-root-section (oref magit-root-section children)))

(defun shipit-page-view--hide (overlay hidden)
  "Hide the part under OVERLAY when HIDDEN is non-nil, else show it.
The invisible value is a symbol of its own: magit-section unfolds a
section by removing each overlay whose invisible value is t in it, and
that would show a hidden part.  The symbol gets no entry in
`buffer-invisibility-spec'.  That is t in a page, which hides each
invisible value.  As a list it hides only the values that it names,
and then the page shows the markup that markdown-mode hides with a
value of its own.

Emacs looks at the display property of text before the invisible one,
so the icon at the start of a hidden part, an image in a display
property, would still show.  The empty display string covers it, and
the high priority puts it above each overlay inside the part."
  (overlay-put overlay 'invisible (and hidden 'shipit-page-view-hidden))
  (overlay-put overlay 'display (and hidden ""))
  (overlay-put overlay 'priority 1000))

(defun shipit-page-view--cover (section start end)
  "Give SECTION, from START to END, an overlay that the view can hide.
Text that is put at either end of it is not covered."
  (let ((overlay (make-overlay start end nil t nil))
        (part (shipit-page-view--part section)))
    (overlay-put overlay 'shipit-page-part part)
    (overlay-put overlay 'evaporate t)
    (shipit-page-view--hide overlay (not (shipit-page-view--shows-p part)))
    overlay))

(defun shipit-page-view--overlays ()
  "The overlays of the parts of this page."
  (seq-filter (lambda (overlay) (overlay-get overlay 'shipit-page-part))
              (overlays-in (point-min) (point-max))))

(defun shipit-page-view--cover-all ()
  "Give each part of this page a new overlay, where its section is now."
  (mapc #'delete-overlay (shipit-page-view--overlays))
  (dolist (section (shipit-page-view--parts))
    (shipit-page-view--cover section (oref section start) (oref section end))))

(defun shipit-page-view--cover-later ()
  "Give the parts of this page their overlays again, once the page is idle."
  (unless shipit-page-view--cover-timer
    (let ((buffer (current-buffer)))
      (setq shipit-page-view--cover-timer
            (run-at-time 0 nil #'shipit-page-view--cover-again buffer)))))

(defun shipit-page-view--cover-again (buffer)
  "Give the parts of the page in BUFFER their overlays again."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shipit-page-view--cover-timer nil)
      (shipit-page-view--cover-all))))

(defun shipit-page-view--section-drawn ()
  "Cover the section that magit-section has drawn, for the view of this page.
This runs in `magit-insert-section-hook', at the end of each section.
It is the one place that sees each part that a page draws again after
an answer of the forge, so a hidden part stays hidden.  A section
drawn inside a part that is there already can make the part longer,
so then each part is covered again once the page is idle."
  (let ((section magit-insert-section--current))
    (cond ((null (oref section parent)))
          ((shipit-page-view--top-level-p section)
           (shipit-page-view--cover section (oref section start) (point)))
          (t (shipit-page-view--cover-later)))))

;;; Folds

(defun shipit-page-view--keep-folds ()
  "Keep whether each part of this page is folded, to come back to it."
  (setq shipit-page-view--full-folds
        (mapcar (lambda (section)
                  (cons (shipit-page-view--part section) (oref section hidden)))
                (shipit-page-view--parts))))

(defun shipit-page-view--restore-folds ()
  "Fold each part of this page as it was when the page last left a full view."
  (dolist (section (shipit-page-view--parts))
    (let ((fold (assq (shipit-page-view--part section) shipit-page-view--full-folds)))
      (when fold
        (if (cdr fold)
            (magit-section-hide section)
          (magit-section-show section))))))

(defun shipit-page-view-fold (section hidden)
  "Fold SECTION, inside a part of this page, when HIDDEN, else unfold it.
A full view gives it the fold back that it had before the first change."
  (unless (assq section shipit-page-view--inner-folds)
    (push (cons section (oref section hidden)) shipit-page-view--inner-folds))
  (if hidden
      (magit-section-hide section)
    (magit-section-show section)))

(defun shipit-page-view--in-page-p (section)
  "Whether SECTION is still in this page, and not drawn over by a newer one."
  (let ((parent (oref section parent)))
    (or (null parent)
        (and (memq section (oref parent children))
             (shipit-page-view--in-page-p parent)))))

(defun shipit-page-view--restore-inner-folds ()
  "Give each section inside a part the fold that it had before a view changed it."
  (dolist (fold shipit-page-view--inner-folds)
    (when (shipit-page-view--in-page-p (car fold))
      (if (cdr fold)
          (magit-section-hide (car fold))
        (magit-section-show (car fold)))))
  (setq shipit-page-view--inner-folds nil))

(defun shipit-page-view--unfold-shown-parts ()
  "Unfold each part of this page that the view shows.
See `shipit-page-view-unfold-functions'."
  (dolist (section (shipit-page-view--parts))
    (let ((part (shipit-page-view--part section)))
      (when (shipit-page-view--shows-p part)
        (funcall (alist-get part shipit-page-view-unfold-functions #'magit-section-show)
                 section)))))

;;; Showing a view

(defun shipit-page-view-redraw ()
  "Draw the parts of this page that only a view shows again.
They are drawn from what the page has."
  (when shipit-page-view-redraw-function
    (funcall shipit-page-view-redraw-function)))

(defun shipit-page-view-apply ()
  "Show this page in the view `shipit-page-view'."
  (let ((from (or shipit-page-view--shown shipit-page-view))
        (to shipit-page-view)
        (inhibit-read-only t))
    (when (and (shipit-page-view--full-p from) (not (shipit-page-view--full-p to)))
      (shipit-page-view--keep-folds))
    (shipit-page-view-redraw)
    (shipit-page-view--cover-all)
    (cond ((not (shipit-page-view--full-p to))
           (shipit-page-view--unfold-shown-parts))
          ;; A section that magit unfolds loses each fold inside it,
          ;; so the sections inside the parts get theirs back first.
          ((not (shipit-page-view--full-p from))
           (shipit-page-view--restore-inner-folds)
           (shipit-page-view--restore-folds)))
    (setq shipit-page-view--shown to)))

(defun shipit-page-view--apply-in (buffer)
  "Show the page in BUFFER in the view `shipit-page-view'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (shipit-page-view-apply))))

(defun shipit-page-view-apply-later ()
  "Show this page in the view `shipit-page-view' once the page is idle.
An answer of the forge can come while the page is still being drawn,
and the sections of the page are not done until the drawing ends."
  (run-at-time 0 nil #'shipit-page-view--apply-in (current-buffer)))

(defun shipit-page-view--pages ()
  "The buffers that show a page."
  (seq-filter (lambda (buffer) (buffer-local-value 'shipit-page-view-kind buffer))
              (buffer-list)))

(defun shipit-page-view--show (name)
  "Show the view NAME in each page, and in each page that opens later."
  (shipit-page-view--entry name)
  (setq shipit-page-view name)
  (dolist (page (shipit-page-view--pages))
    (with-current-buffer page
      (shipit-page-view-apply)))
  (message "View: %s" name))

(defun shipit-page-view-next ()
  "Show the next view of `shipit-page-views' in each page."
  (interactive)
  (shipit-page-view--show (shipit-page-view--after shipit-page-view)))

(defun shipit-page-view-choose (name)
  "Show the view NAME of `shipit-page-views' in each page."
  (interactive
   (list (intern (completing-read
                  "View: " (mapcar (lambda (view) (symbol-name (car view))) shipit-page-views)
                  nil t nil nil (symbol-name shipit-page-view)))))
  (shipit-page-view--show name))

(defun shipit-page-view-setup (kind)
  "Make this buffer a page of KIND, :pr or :issue, that shows the views."
  (setq shipit-page-view-kind kind
        shipit-page-view--shown shipit-page-view)
  (add-hook 'magit-insert-section-hook #'shipit-page-view--section-drawn nil t))

;;; Drawing a part again

(defun shipit-page-view--part-of-type (type)
  "The section of TYPE at the top level of this page, or nil."
  (seq-find (lambda (section) (eq (oref section type) type))
            (shipit-page-view--parts)))

(defun shipit-page-view--put-in-place (new old)
  "Put the section NEW where the section OLD was among the parts of this page."
  (let ((parts (delq new (oref magit-root-section children))))
    (oset magit-root-section children
          (if old
              (cl-substitute new old parts :test #'eq)
            (append parts (list new))))))

(defun shipit-page-view-draw-part (type draw)
  "Draw the part of TYPE at the top level of this page again, with DRAW.
DRAW inserts the section of the part at point, and returns it."
  (let ((old (shipit-page-view--part-of-type type))
        (inhibit-read-only t))
    (unless old
      (error "This page has no part of type %s to draw again" type))
    (save-excursion
      (goto-char (oref old start))
      (delete-region (oref old start) (oref old end))
      (let* ((magit-insert-section--parent magit-root-section)
             (new (funcall draw)))
        (shipit-page-view--put-in-place new old)))))

;;; Keys

(define-key shipit-page-mode-map (kbd "v") #'shipit-page-view-next)
(define-key shipit-page-mode-map (kbd "V") #'shipit-page-view-choose)

(provide 'shipit-page-views)
;;; shipit-page-views.el ends here
