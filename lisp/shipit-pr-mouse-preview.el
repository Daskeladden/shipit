;;; shipit-pr-mouse-preview.el --- The preview of a PR on mouse-over -*- lexical-binding: t; -*-

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
;; When the mouse rests on a pull request link in a shipit buffer, the
;; preview of the pull request opens next to it, without taking the
;; keyboard.  It stays while the mouse is on the link or on the
;; preview, so the wheel scrolls it, and it closes when the mouse
;; leaves both.  The keyboard opens the same preview through the menu
;; of the link.

;;; Code:

(require 'cl-lib)
(require 'shipit-pr-backends)
(require 'shipit-render)

(defcustom shipit-pr-mouse-preview-delay 0.5
  "Seconds that the mouse rests on a PR link before its preview opens."
  :type 'number
  :group 'shipit)

(defconst shipit--mouse-preview-poll-interval 0.2
  "Seconds between two looks at the mouse.")

(defvar shipit--mouse-preview-state nil
  "Where the mouse preview is: a plist of :phase, :link and :since, or nil.")

(defvar shipit--mouse-preview-frame nil
  "The preview frame that the mouse opened, or nil.")

(defvar shipit--mouse-preview-timer nil
  "The timer that looks at the mouse while the mode is on, or nil.")

(defun shipit--pr-link-at (pos)
  "The pull request link at POS of the current buffer, or nil.
A plist (:number N :repo REPO :start START :end END), from the overlay
of a PR URL, or from the overlay of a #N reference, which can name an
issue as well."
  (cl-some (lambda (ov)
             (let ((link (overlay-get ov 'shipit-pr-link))
                   (id (overlay-get ov 'shipit-issue-id))
                   (repo (overlay-get ov 'shipit-issue-repo)))
               (cond (link (list :number (car link) :repo (cdr link)
                                 :start (overlay-start ov) :end (overlay-end ov)))
                     ((and (integerp id) repo)
                      (list :number id :repo repo
                            :start (overlay-start ov) :end (overlay-end ov))))))
           (overlays-at pos)))

(defun shipit--mouse-preview-step (state now link on-popup delay)
  "The next state and the action, as (STATE . ACTION), at NOW seconds.
The mouse is on LINK, or on no link when LINK is nil, and ON-POPUP says
that it is on the preview.  ACTION is `fetch', `close' or nil.  A new
link starts a wait, and after DELAY seconds on it, a fetch.  STATE nil
is idle."
  (let ((phase (plist-get state :phase)))
    (cond
     (on-popup (cons state nil))
     ((and link (equal link (plist-get state :link)))
      (if (and (eq phase 'waiting) (>= (- now (plist-get state :since)) delay))
          (cons (plist-put (copy-sequence state) :phase 'fetching) 'fetch)
        (cons state nil)))
     (link (cons (list :phase 'waiting :link link :since now)
                 (and (eq phase 'shown) 'close)))
     (t (cons nil (and (eq phase 'shown) 'close))))))

(defun shipit--mouse-preview-arrived (state link pr-data)
  "The next state and the action, as (STATE . ACTION), when PR-DATA arrives.
PR-DATA is the pull request of LINK, or nil when LINK names none.
ACTION is `show' when LINK is still the link that is fetched, else nil.
A link that names no pull request is not fetched again while the mouse
stays on it."
  (if (and (eq (plist-get state :phase) 'fetching)
           (equal link (plist-get state :link)))
      (cons (plist-put (copy-sequence state) :phase (if pr-data 'shown 'no-pr))
            (and pr-data 'show))
    (cons state nil)))

(defvar x-use-fast-mouse-position)

(defun shipit--mouse-preview-mouse-position ()
  "Where the mouse is, as `mouse-pixel-position' gives it.
The place is the one that Emacs knows from the last motion of the
mouse: X sends Emacs each motion over its frames.  The X server is
asked nothing, as the poll reads the place each 0.2 seconds, and over
ssh a question to the server took 60 to 90 ms."
  (let ((x-use-fast-mouse-position 'really-fast))
    (mouse-pixel-position)))

(defun shipit--mouse-preview-link-under-mouse ()
  "The PR link under the mouse, with its :buffer and :window, or nil.
The preview frame itself holds no link."
  (pcase-let ((`(,frame ,x . ,y) (shipit--mouse-preview-mouse-position)))
    (when (and frame x y (not (eq frame shipit--mouse-preview-frame)))
      (let* ((posn (posn-at-x-y x y frame))
             (window (posn-window posn))
             (pos (posn-point posn)))
        (when (and (windowp window) (integerp pos))
          (with-current-buffer (window-buffer window)
            (when-let* ((link (shipit--pr-link-at pos)))
              (append link (list :buffer (current-buffer) :window window)))))))))

(defun shipit--mouse-preview-on-popup-p ()
  "Whether the mouse is on the preview frame, or the preview has the keyboard."
  (and (frame-live-p shipit--mouse-preview-frame)
       (or (eq (car (shipit--mouse-preview-mouse-position)) shipit--mouse-preview-frame)
           (eq (selected-frame) shipit--mouse-preview-frame))))

(defun shipit--mouse-preview-close ()
  "Close the preview frame that the mouse opened, and kill its buffer."
  (when (frame-live-p shipit--mouse-preview-frame)
    (let ((buffer (window-buffer (frame-root-window shipit--mouse-preview-frame))))
      (delete-frame shipit--mouse-preview-frame)
      (kill-buffer buffer)))
  (setq shipit--mouse-preview-frame nil))

(defun shipit--mouse-preview-receive (link pr-data)
  "Open the preview of PR-DATA, the pull request of LINK, if LINK is still
the link that is fetched.  PR-DATA nil means that LINK names none."
  (let ((step (shipit--mouse-preview-arrived shipit--mouse-preview-state link pr-data)))
    (setq shipit--mouse-preview-state (car step))
    (when (eq (cdr step) 'show)
      (shipit--mouse-preview-close)
      (setq shipit--mouse-preview-frame
            (shipit--show-pr-preview-data (plist-get link :number) (plist-get link :repo) pr-data
                                          (cons (plist-get link :window) (plist-get link :start))
                                          t)))))

(defun shipit--mouse-preview-fetch (link)
  "Fetch the pull request of LINK in the background, then open its preview.
A backend that cannot fetch in the background opens nothing, because
Emacs would wait each time the mouse rests on a link."
  (unless (shipit-pr--fetch-pr-async (plist-get link :repo) (plist-get link :number)
                                     (lambda (pr-data) (shipit--mouse-preview-receive link pr-data)))
    (shipit--mouse-preview-receive link nil)))

(defun shipit--mouse-preview-poll ()
  "Look at the mouse, and fetch, open or close the preview as it says."
  (let ((step (shipit--mouse-preview-step shipit--mouse-preview-state (float-time)
                                          (shipit--mouse-preview-link-under-mouse)
                                          (shipit--mouse-preview-on-popup-p)
                                          shipit-pr-mouse-preview-delay)))
    (setq shipit--mouse-preview-state (car step))
    (pcase (cdr step)
      ('fetch (shipit--mouse-preview-fetch (plist-get (car step) :link)))
      ('close (shipit--mouse-preview-close)))))

(defun shipit--mouse-preview-poll-or-stop ()
  "Run `shipit--mouse-preview-poll'.  An error turns the mode off and is
reported once, as a timer that fails each time would report it every
`shipit--mouse-preview-poll-interval' seconds."
  (condition-case err
      (shipit--mouse-preview-poll)
    (error
     (shipit-pr-mouse-preview-mode -1)
     (message "shipit: the PR preview on mouse-over is off after an error: %s"
              (error-message-string err)))))

;;;###autoload
(define-minor-mode shipit-pr-mouse-preview-mode
  "Open the preview of a pull request when the mouse rests on its link.
In shipit buffers, after `shipit-pr-mouse-preview-delay' seconds on a
PR link, the preview opens next to the link without the keyboard.  It
stays while the mouse is on the link or on the preview, and it closes
when the mouse leaves both."
  :global t
  :group 'shipit
  (when shipit--mouse-preview-timer
    (cancel-timer shipit--mouse-preview-timer)
    (setq shipit--mouse-preview-timer nil))
  (shipit--mouse-preview-close)
  (setq shipit--mouse-preview-state nil)
  (when shipit-pr-mouse-preview-mode
    (setq shipit--mouse-preview-timer
          (run-with-timer shipit--mouse-preview-poll-interval shipit--mouse-preview-poll-interval
                          #'shipit--mouse-preview-poll-or-stop))))

(provide 'shipit-pr-mouse-preview)
;;; shipit-pr-mouse-preview.el ends here
