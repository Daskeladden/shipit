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
;; Some shipit views draw on the Emacs 32 canvas through canvas-diagram,
;; or through canvas-graph, which is built on it.  The canvas image type
;; is not there on every Emacs shipit runs on, and canvas-diagram can be
;; present and still unable to load: its canvas-cairo module is built
;; for one Emacs, and another refuses it.  Every canvas view asks here,
;; so all of them give way the same way and none of them breaks loading
;; or compiling shipit.

;;; Code:

(require 'cl-lib)
(require 'shipit-core)

(defvar meow-mode-state-list)
(defvar undo-tree-incompatible-major-modes)

(defun shipit-canvas--load (library)
  "Load LIBRARY, a canvas library; return non-nil when it is there to use.
`require' with NOERROR covers only a file that is not found.  A library
that is found and then fails while loading signals from inside it, as
canvas-diagram's module does on an Emacs it was not built for, so that
error is caught here and logged.  canvas-graph fails the same way,
through the canvas-diagram it requires."
  (condition-case err
      (require library nil t)
    (error
     (shipit--debug-log "%s could not load: %S" library err)
     nil)))

(defun shipit-canvas-available-p (&optional library)
  "Return non-nil when this Emacs can draw on a canvas.
LIBRARY is the canvas library the view draws with, `canvas-diagram'
unless given.  The canvas image type is asked first, so an Emacs
without it never tries to load a canvas library at all."
  (and (image-type-available-p 'canvas)
       (shipit-canvas--load (or library 'canvas-diagram))
       t))

(defun shipit-canvas-require (view fallback &optional library)
  "Signal unless this Emacs can draw on a canvas.
VIEW names what wants the canvas, FALLBACK the command that shows the
same thing without one, and LIBRARY the canvas library VIEW draws with,
`canvas-diagram' unless given."
  (let ((library (or library 'canvas-diagram)))
    (unless (shipit-canvas-available-p library)
      (user-error "%s needs Emacs 32 and %s; use `%s' instead"
                  view library fallback))))

(defun shipit-canvas-set-slot (type slot object value)
  "Set SLOT of OBJECT, a struct of TYPE, to VALUE; return VALUE.
Written this way on purpose.  Setting a slot with the struct's own
accessor compiles to an inline write only when the struct is known at
compile time, and to a call to a setter function that does not exist
when it is not.  The canvas libraries are optional here, so which of
the two we get depends on whether they happened to be on the load path
during the compile.  Looking the slot up by name at runtime does not
care.  TYPE is an argument, not a constant in the caller: with a
constant, the compiler tries to inline a type check of a struct it may
not know, and prints the error of that failure."
  (setf (cl-struct-slot-value type slot object) value))

(defun shipit-canvas-keep-other-packages-away (mode)
  "Ask meow and undo-tree to leave the buffers of MODE, a canvas mode, alone.
Both take keys before a major mode map sees them.  meow starts MODE in
insert state, and undo-tree stays out of it.  A package that is not
loaded is left alone; each canvas file asks again once it loads."
  (when (boundp 'meow-mode-state-list)
    (add-to-list 'meow-mode-state-list (cons mode 'insert)))
  (when (boundp 'undo-tree-incompatible-major-modes)
    (add-to-list 'undo-tree-incompatible-major-modes mode)))

(provide 'shipit-canvas)
;;; shipit-canvas.el ends here
