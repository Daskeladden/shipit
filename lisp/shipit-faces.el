;;; shipit-faces.el --- Customizable faces for shipit -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Centralised face definitions for shipit.  This file deliberately
;; depends on no other shipit module so any other file may safely
;; `require' it without risk of circular dependencies -- it sits at
;; the bottom of the dependency graph alongside `shipit-core'.
;;
;; The `:group' references (`shipit', `shipit-code-refs') resolve
;; against the matching `defgroup' forms in `shipit-core' /
;; `shipit-code-refs'; defface stores the group symbol without
;; requiring its definition to be loaded first, so the forward
;; reference is harmless.

;;; Code:

(defmacro shipit-defface (face spec doc &rest args)
  "Like `defface' but re-applies SPEC every time this file loads.
`defface' is idempotent on the face's default-spec slot -- it
only initialises the face the first time it is evaluated.
After that, editing the SPEC and reloading the file does
nothing visible because the face object is already registered.

This macro additionally calls `face-spec-set' with
`face-defface-spec' so the default attributes refresh on every
load, which is what `shipit-reload' / `M-x eval-buffer'
expects.  User customisations made via `M-x customize-face'
live on a separate face-spec slot (`face-customized-spec' /
`face-override-spec') and are preserved -- only the default
moves."
  (declare (indent 2) (doc-string 3))
  `(progn
     (defface ,face ,spec ,doc ,@args)
     (face-spec-set ',face ,spec 'face-defface-spec)))


;; Comment / inline-comment faces ------------------------------------

(shipit-defface shipit-comment-face
  '((t :background "#f8f9fa" :foreground "black" :extend t)
    (((class color) (background dark)) :background "#2d3748" :foreground "white"))
  "Face for highlighting lines with GitHub comments."
  :group 'shipit)

(shipit-defface shipit-comment-indicator-face
  '((t :foreground "#856404" :weight bold))
  "Face for GitHub comment indicators."
  :group 'shipit)

(shipit-defface shipit-inline-comment-face
  '((t :foreground "#ff8c00" :extend t)
    (((class color) (background light)) :foreground "#d2691e")
    (((class color) (background dark)) :foreground "#ff8c00"))
  "Face for inline GitHub comment text with orange foreground."
  :group 'shipit)

(shipit-defface shipit-inline-comment-username-face
  '((t :foreground "#0366d6" :weight bold))
  "Face for inline comment username highlighting (color is set dynamically)."
  :group 'shipit)

(shipit-defface shipit-inline-comment-timestamp-face
  '((t :foreground "#6a737d" :slant italic))
  "Face for inline comment timestamp highlighting (color is set dynamically)."
  :group 'shipit)

(shipit-defface shipit-inline-comment-body-face
  '((t :foreground "black")
    (((class color) (background light)) :foreground "black")
    (((class color) (background dark)) :foreground "white"))
  "Face for inline comment body text."
  :group 'shipit)

(shipit-defface shipit-comment-reactions-placeholder
  '((t :foreground "#999999"
       :weight normal))
  "Face for dimmed reactions placeholder smiley when no reactions exist."
  :group 'shipit)

(shipit-defface shipit-blockquote-face
  '((t :inherit (shadow italic)))
  "Face applied to blockquoted lines (`>'-prefixed) in rendered comment bodies.
Used by `shipit--apply-blockquote-faces' via
`shipit--prepend-font-lock-face' so the styling survives
magit-section's font-lock-mode refontification pass.

Default inherits from `shadow' (a theme-defined muted gray
that adapts to light and dark backgrounds) plus `italic',
which is the conventional cue across markdown viewers, chat
clients, and prose tools that this text is quoted from
elsewhere.  Customize freely if you want a stronger color or
to match `markdown-blockquote-face' instead."
  :group 'shipit)

;; Identity / metadata faces -----------------------------------------

(shipit-defface shipit-username-face
  '((t :foreground "#feacd0" :weight bold))
  "Face for username highlighting throughout shipit (color is customizable)."
  :group 'shipit)

(shipit-defface shipit-timestamp-face
  '((t :foreground "#00d3d0" :slant italic))
  "Face for timestamp highlighting throughout shipit (color is customizable)."
  :group 'shipit)

(shipit-defface shipit-filename-face
  '((t :foreground "#6ae4b9"))
  "Face for filename highlighting throughout shipit (color is customizable)."
  :group 'shipit)

;; Activity-row timestamp gap faces ----------------------------------

(shipit-defface shipit-timestamp-gap-mild-face
  '((t :foreground "#d4a0a0"))
  "Face for timestamps with a mild gap (>= 5s by default)."
  :group 'shipit)

(shipit-defface shipit-timestamp-gap-moderate-face
  '((t :foreground "#e06060"))
  "Face for timestamps with a moderate gap (>= 30s by default)."
  :group 'shipit)

(shipit-defface shipit-timestamp-gap-high-face
  '((t :foreground "#ef3030"))
  "Face for timestamps with a high gap (>= 5m by default)."
  :group 'shipit)

(shipit-defface shipit-timestamp-gap-extreme-face
  '((t :foreground "#ff0000" :weight bold))
  "Face for timestamps with an extreme gap (>= 30m by default)."
  :group 'shipit)

;; Notifications-buffer auto-mark preview faces ----------------------

(shipit-defface shipit-auto-mark-preview-face
  '((t :inherit font-lock-warning-face :strike-through t))
  "Face for notification rows previewed as auto-mark targets.
Strike-through reinforces the visual that these rows would
disappear once the rule fires."
  :group 'shipit)

(shipit-defface shipit-auto-mark-preview-match-face
  '((t :inherit lazy-highlight :weight bold))
  "Face for the actual characters that match the regex in a
row that is being previewed as an auto-mark target.  Inherits
from `lazy-highlight' so the visual cue is familiar from
isearch -- but bolded so it pops over the row's strike-through."
  :group 'shipit)

;; Atlassian dashboard -----------------------------------------------

(shipit-defface shipit-atlassian-dashboard-pending-face
  '((t :inherit (warning italic)))
  "Face for the staged-transition arrow in the Atlassian dashboard.
Edit-mode `M-n' / `M-p' append `-> TARGET' to an issue row so
the user sees what would happen on `C-c C-c'.  Inherits from
`warning' (theme-defined attention color) plus italic to read
as \"this is provisional, not committed\"."
  :group 'shipit-atlassian)

;; Code-refs face ----------------------------------------------------

(shipit-defface shipit-code-refs-face
  '((t :inherit link :underline t))
  "Face applied to recognised issue-key references in code."
  :group 'shipit-code-refs)

;; Per-file commit timeline faces ------------------------------------

(shipit-defface shipit-review-history-pr-marker-face
  '((t :foreground "yellow" :weight bold))
  "Face for the in-PR group heading and PR-commit dots in the file timeline."
  :group 'shipit)

(shipit-defface shipit-review-history-stats-face
  '((t :inherit shadow))
  "Face for `+X -Y' line stats in the file timeline buffer."
  :group 'shipit)

(shipit-defface shipit-review-history-current-face
  '((t :inherit highlight :extend t))
  "Face for the row matching the file buffer's currently-displayed SHA.
Updates as `n' / `p' / range commands change the displayed
revision, so the timeline acts as a live position indicator."
  :group 'shipit)

(shipit-defface shipit-review-history-meta-face
  '((t :inherit shadow))
  "Face for the expanded body lines under each commit row in the timeline."
  :group 'shipit)

(provide 'shipit-faces)

;;; shipit-faces.el ends here
