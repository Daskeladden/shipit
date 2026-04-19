;;; shipit-code-refs.el --- Detect issue-key references in code comments -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Highlights issue-key references such as `ZIVID-12345' that appear
;; inside code comments and makes them actionable.  RET / mouse-1 on a
;; match opens the corresponding shipit issue buffer for the current
;; repo's issue backend (typically Jira).  The `shipit-code-refs-menu'
;; transient exposes additional actions: browse in the backend's web
;; UI, copy the URL, or copy the key itself.

;; When embark is loaded, a target finder makes the key available as
;; an embark target of category `shipit-issue-ref', so users can
;; trigger the same actions via their embark key (default `C-.').

;; Enable per buffer with `shipit-code-refs-mode' or globally (for
;; `prog-mode' derivatives) with `global-shipit-code-refs-mode'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'shipit-core)

(declare-function shipit-issue--resolve-for-repo "shipit-issue-backends")
(declare-function shipit-issues-open-buffer "shipit-issues-buffer")
(declare-function shipit--browse-issue-url "shipit-notifications")

(defgroup shipit-code-refs nil
  "Detect and act on issue-key references in code comments."
  :group 'shipit
  :prefix "shipit-code-refs-")

(defcustom shipit-code-refs-pattern
  "\\b\\([A-Z][A-Z0-9_]+-[0-9]+\\)\\b"
  "Regex matching issue-key references in code.
Group 1 must capture the full key (e.g. `ZIVID-12345').  The default
matches Jira-style keys: one uppercase letter followed by more
uppercase alphanumerics or underscores, a hyphen, and one or more
digits."
  :type 'regexp
  :group 'shipit-code-refs)

(defcustom shipit-code-refs-comments-only t
  "When non-nil, only recognise keys inside syntactic comments.
Disabling this also picks up keys in strings and live code — handy in
buffers (e.g. plain text) where `syntax-ppss' does not classify
comments, but introduces noise in most code."
  :type 'boolean
  :group 'shipit-code-refs)

(defface shipit-code-refs-face
  '((t :inherit link :underline t))
  "Face applied to recognised issue-key references in code."
  :group 'shipit-code-refs)

(defvar shipit-code-refs-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shipit-code-refs-dwim)
    (define-key map [mouse-1] #'shipit-code-refs-dwim)
    (define-key map (kbd "M-;") #'shipit-code-refs-menu)
    map)
  "Keymap applied to issue-ref regions in code buffers.")

;;; Detection

(defun shipit-code-refs--matcher (bound)
  "Font-lock matcher that finds the next issue-key ref up to BOUND.
When `shipit-code-refs-comments-only' is non-nil, skips matches that
are not inside a syntactic comment."
  (let ((case-fold-search nil)
        (found nil))
    (while (and (not found)
                (re-search-forward shipit-code-refs-pattern bound t))
      (if shipit-code-refs-comments-only
          (when (nth 4 (syntax-ppss (match-beginning 0)))
            (setq found t))
        (setq found t)))
    found))

(defvar shipit-code-refs--font-lock-keywords nil
  "Font-lock keywords added by `shipit-code-refs-mode'.")

(defun shipit-code-refs--build-keywords ()
  "Return font-lock-keywords that tag issue-key matches with properties."
  `((shipit-code-refs--matcher
     (1 (list 'face 'shipit-code-refs-face
              'shipit-issue-ref (match-string-no-properties 1)
              'mouse-face 'highlight
              'keymap shipit-code-refs-keymap
              'help-echo "RET: open issue; M-;: actions")
        prepend))))

(defun shipit-code-refs-key-at-point ()
  "Return the issue key at point, or nil.
Prefers the font-lock-applied text property; falls back to scanning
the current line with `shipit-code-refs-pattern'."
  (or (get-text-property (point) 'shipit-issue-ref)
      (save-excursion
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position))
              (case-fold-search nil)
              (pt (point))
              (found nil))
          (goto-char line-start)
          (while (and (not found)
                      (re-search-forward shipit-code-refs-pattern line-end t))
            (when (and (<= (match-beginning 0) pt)
                       (>= (match-end 0) pt))
              (setq found (match-string-no-properties 1))))
          found))))

;;; Actions

(defun shipit-code-refs--current-repo ()
  "Return the repo (owner/repo) associated with the current buffer, or nil."
  (ignore-errors (shipit--get-repo-from-remote)))

(defun shipit-code-refs--browse-url (key)
  "Return the browse URL for issue KEY under the current repo's backend."
  (let* ((repo (shipit-code-refs--current-repo))
         (resolved (and repo
                        (ignore-errors (shipit-issue--resolve-for-repo repo))))
         (backend (car resolved))
         (config (cdr resolved))
         (browse-fn (and backend (plist-get backend :browse-url))))
    (when browse-fn
      (funcall browse-fn config key))))

;;;###autoload
(defun shipit-code-refs-open-issue (&optional key)
  "Open the shipit issue buffer for KEY (defaults to key at point)."
  (interactive)
  (let ((key (or key (shipit-code-refs-key-at-point)))
        (repo (shipit-code-refs--current-repo)))
    (cond
     ((not key) (user-error "No issue key at point"))
     ((not repo) (user-error "Cannot determine repository from current buffer"))
     (t
      (require 'shipit-issues-buffer)
      (shipit-issues-open-buffer key repo)))))

;;;###autoload
(defun shipit-code-refs-browse-issue (&optional key)
  "Browse issue KEY (defaults to key at point) in the backend's web UI."
  (interactive)
  (let* ((key (or key (shipit-code-refs-key-at-point)))
         (url (and key (shipit-code-refs--browse-url key))))
    (cond
     ((not key) (user-error "No issue key at point"))
     ((not url) (user-error "Cannot resolve browse URL for %s" key))
     (t (browse-url url)))))

;;;###autoload
(defun shipit-code-refs-copy-url (&optional key)
  "Copy the backend browse URL for KEY (defaults to key at point) to the kill ring."
  (interactive)
  (let* ((key (or key (shipit-code-refs-key-at-point)))
         (url (and key (shipit-code-refs--browse-url key))))
    (cond
     ((not key) (user-error "No issue key at point"))
     ((not url) (user-error "Cannot resolve URL for %s" key))
     (t (kill-new url) (message "Copied: %s" url)))))

;;;###autoload
(defun shipit-code-refs-copy-key (&optional key)
  "Copy the issue KEY (defaults to key at point) to the kill ring."
  (interactive)
  (let ((key (or key (shipit-code-refs-key-at-point))))
    (if (not key)
        (user-error "No issue key at point")
      (kill-new key)
      (message "Copied: %s" key))))

;;;###autoload (autoload 'shipit-code-refs-menu "shipit-code-refs" nil t)
(transient-define-prefix shipit-code-refs-menu ()
  "Actions for an issue-key reference at point."
  [["Issue"
    ("o" "Open in shipit" shipit-code-refs-open-issue)
    ("b" "Browse in web UI" shipit-code-refs-browse-issue)]
   ["Clipboard"
    ("u" "Copy URL" shipit-code-refs-copy-url)
    ("k" "Copy key" shipit-code-refs-copy-key)]])

;;;###autoload
(defun shipit-code-refs-dwim ()
  "Default action on an issue-key reference at point: open the issue in shipit.
Call `shipit-code-refs-menu' for the full action list."
  (interactive)
  (shipit-code-refs-open-issue))

;;; Minor modes

;;;###autoload
(define-minor-mode shipit-code-refs-mode
  "Highlight and act on issue-key references in this buffer.

When enabled, font-lock tags occurrences of `shipit-code-refs-pattern'
(restricted to comments by default) with `shipit-code-refs-face' and
an RET / mouse-1 keymap that opens the matching issue in shipit."
  :lighter " CodeRefs"
  :group 'shipit-code-refs
  (setq shipit-code-refs--font-lock-keywords
        (or shipit-code-refs--font-lock-keywords
            (shipit-code-refs--build-keywords)))
  (if shipit-code-refs-mode
      (font-lock-add-keywords nil shipit-code-refs--font-lock-keywords 'append)
    (font-lock-remove-keywords nil shipit-code-refs--font-lock-keywords))
  (when (fboundp 'font-lock-flush)
    (font-lock-flush)))

(defun shipit-code-refs--turn-on-maybe ()
  "Enable `shipit-code-refs-mode' in buffers where it makes sense.
Currently: any `prog-mode' descendant."
  (when (derived-mode-p 'prog-mode)
    (shipit-code-refs-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-shipit-code-refs-mode
  shipit-code-refs-mode
  shipit-code-refs--turn-on-maybe
  :group 'shipit-code-refs)

;;; Embark integration

(declare-function embark-define-keymap "embark")
(defvar embark-target-finders)
(defvar embark-keymap-alist)

(defun shipit-code-refs-embark-target ()
  "Embark target finder: return the issue-key at point.
Category is `shipit-issue-ref', value is the bare key string."
  (when-let* ((key (or (get-text-property (point) 'shipit-issue-ref)
                       (shipit-code-refs-key-at-point))))
    `(shipit-issue-ref ,key . ,(bounds-of-thing-at-point 'symbol))))

(defvar shipit-code-refs-embark-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'shipit-code-refs-open-issue)
    (define-key map (kbd "b") #'shipit-code-refs-browse-issue)
    (define-key map (kbd "u") #'shipit-code-refs-copy-url)
    (define-key map (kbd "w") #'shipit-code-refs-copy-key)
    map)
  "Embark keymap for `shipit-issue-ref' targets.")

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders #'shipit-code-refs-embark-target)
  (add-to-list 'embark-keymap-alist
               '(shipit-issue-ref . shipit-code-refs-embark-keymap)))

(provide 'shipit-code-refs)
;;; shipit-code-refs.el ends here
