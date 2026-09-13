;;; shipit-canvas.el --- Whether shipit can draw on a canvas -*- lexical-binding: t; -*-

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
;; Some shipit views draw on the Emacs 32 canvas through canvas-diagram.
;; The canvas image type is not there on every Emacs shipit runs on, and
;; canvas-diagram can be present and still unable to load: its
;; canvas-cairo module is built for one Emacs, and another refuses it.
;; Every canvas view asks here, so all of them give way the same way and
;; none of them breaks loading or compiling shipit.

;;; Code:

(require 'shipit-core)

(defun shipit-canvas--load-diagram ()
  "Load canvas-diagram; return non-nil when it is there to use.
`require' with NOERROR covers only a file that is not found.  A
canvas-diagram that is found and then fails while loading signals from
inside it, as its module does on an Emacs it was not built for, so that
error is caught here and logged."
  (condition-case err
      (require 'canvas-diagram nil t)
    (error
     (shipit--debug-log "canvas-diagram could not load: %S" err)
     nil)))

(defun shipit-canvas-available-p ()
  "Return non-nil when this Emacs can draw on a canvas.
The canvas image type is asked first, so an Emacs without it never
tries to load canvas-diagram at all."
  (and (image-type-available-p 'canvas)
       (shipit-canvas--load-diagram)
       t))

(defun shipit-canvas-require (view fallback)
  "Signal unless this Emacs can draw on a canvas.
VIEW names what wants the canvas, and FALLBACK the command that shows
the same thing without one."
  (unless (shipit-canvas-available-p)
    (user-error "%s needs Emacs 32 and canvas-diagram; use `%s' instead"
                view fallback)))

(provide 'shipit-canvas)
;;; shipit-canvas.el ends here
