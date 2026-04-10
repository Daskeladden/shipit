;;; shipit-releases-buffer.el --- Releases and tags buffer -*- lexical-binding: t; -*-

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
;; Dedicated buffer for viewing GitHub releases and tags with magit sections.
;; Shows releases with rendered markdown bodies and download assets,
;; and a separate tags section with abbreviated SHAs.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'shipit-core)
(require 'shipit-pr-backends)

;; Forward declarations
(declare-function shipit--render-markdown "shipit-render")
(declare-function shipit--format-timestamp "shipit-core")
(declare-function shipit-pr--resolve-for-repo "shipit-pr-backends")

;; Forward variable declarations
(defvar shipit-render-markdown)

;;; Section type definitions

(defun shipit-releases-entry (&rest _args)
  "Magit section identifier for a release entry."
  nil)
(put 'shipit-releases-entry 'magit-section t)

(defun shipit-tags-entry (&rest _args)
  "Magit section identifier for a tag entry."
  nil)
(put 'shipit-tags-entry 'magit-section t)

(defun shipit-releases-section (&rest _args)
  "Magit section identifier for the releases section."
  nil)
(put 'shipit-releases-section 'magit-section t)

(defun shipit-tags-section (&rest _args)
  "Magit section identifier for the tags section."
  nil)
(put 'shipit-tags-section 'magit-section t)

;;; Buffer-local variables

(defvar-local shipit-releases-buffer-repo nil
  "Repository name (owner/repo) displayed in this buffer.")

(defvar-local shipit-releases-buffer-scroll-to nil
  "Target to scroll to after rendering.
When symbol `tags', scroll to the Tags section.
When a string, scroll to the release with that tag name.")

;;; Mode

(defvar shipit-releases-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'shipit-releases-buffer-refresh)
    (define-key map (kbd "r") #'shipit-releases-buffer-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `shipit-releases-mode'.")

(define-derived-mode shipit-releases-mode magit-section-mode "Shipit-Releases"
  "Major mode for shipit releases buffer.
Displays repository releases and tags.

\\{shipit-releases-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-releases-buffer-refresh)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil))

;;; Entry point

;;;###autoload
(defun shipit-open-releases-buffer (repo &optional tag scroll-to)
  "Open a releases buffer for REPO (owner/name).
Creates or reuses the buffer *shipit-releases: REPO*.
TAG, if non-nil, scrolls to the release with that tag name.
SCROLL-TO, if `tags', scrolls to the Tags section."
  (let* ((buf-name (format "*shipit-releases: %s*" repo))
         (existing (get-buffer buf-name)))
    (if existing
        (progn
          (switch-to-buffer existing)
          (when (or tag scroll-to)
            (setq shipit-releases-buffer-scroll-to (or tag scroll-to))
            (shipit-releases--apply-scroll-to)))
      (let ((buffer (generate-new-buffer buf-name)))
        (with-current-buffer buffer
          (shipit-releases-mode)
          (setq shipit-releases-buffer-repo repo)
          (setq shipit-releases-buffer-scroll-to (or tag scroll-to))
          (shipit-releases-buffer-refresh))
        (switch-to-buffer buffer)))
    (get-buffer buf-name)))

;;; Refresh

(defun shipit-releases-buffer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the releases buffer contents."
  (interactive)
  (let ((repo shipit-releases-buffer-repo))
    (shipit--debug-log "Refreshing releases buffer for %s" repo)
    (message "Loading releases for %s..." repo)
    (let* ((resolved (shipit-pr--resolve-for-repo repo))
           (backend (car resolved))
           (config (cdr resolved))
           (fetch-releases-fn (plist-get backend :fetch-releases))
           (fetch-tags-fn (plist-get backend :fetch-tags))
           (releases (when fetch-releases-fn
                       (funcall fetch-releases-fn config)))
           (tags (when fetch-tags-fn
                   (funcall fetch-tags-fn config))))
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (magit-insert-section (root)
          (shipit-releases--insert-releases-section releases repo)
          (shipit-releases--insert-tags-section tags repo))
        (shipit-releases--apply-scroll-to)
        (unless shipit-releases-buffer-scroll-to
          (goto-char (min pos (point-max))))))
    (message "Releases for %s loaded" repo)))

;;; Size formatting

(defun shipit-releases--format-size (bytes)
  "Format BYTES as a human-readable size string."
  (cond
   ((null bytes) "?")
   ((< bytes 1024)
    (format "%d B" bytes))
   ((< bytes (* 1024 1024))
    (format "%.1f KB" (/ (float bytes) 1024)))
   ((< bytes (* 1024 1024 1024))
    (format "%.1f MB" (/ (float bytes) (* 1024 1024))))
   (t
    (format "%.1f GB" (/ (float bytes) (* 1024 1024 1024))))))

;;; Release entry rendering

(defun shipit-releases--insert-release-heading (release)
  "Insert the heading line for a single RELEASE."
  (let* ((tag (or (cdr (assq 'tag_name release)) "?"))
         (name (or (cdr (assq 'name release)) ""))
         (author-obj (cdr (assq 'author release)))
         (author (or (cdr (assq 'login author-obj)) ""))
         (published (or (cdr (assq 'published_at release)) ""))
         (prerelease (eq (cdr (assq 'prerelease release)) t))
         (draft (eq (cdr (assq 'draft release)) t))
         (badges (shipit-releases--format-badges draft prerelease))
         (title-part (if (or (string-empty-p name)
                             (equal name tag))
                         ""
                       (format " — %s" name)))
         (timestamp (if (string-empty-p published)
                        ""
                      (shipit--format-timestamp published))))
    (magit-insert-heading
      (format "  %s%s%s"
              (propertize tag 'face 'magit-tag)
              badges
              title-part)
      (format "%s%s  %s\n"
              (make-string (max 1 (- 60 (length tag) (length badges)
                                     (length title-part)))
                           ?\s)
              (if (string-empty-p author)
                  ""
                (propertize author 'face 'shipit-username-face))
              (propertize timestamp 'face 'magit-dimmed)))))

(defun shipit-releases--format-badges (draft prerelease)
  "Format badge string for DRAFT and PRERELEASE flags."
  (let ((parts nil))
    (when draft
      (push (propertize " [draft]" 'face 'warning) parts))
    (when prerelease
      (push (propertize " [pre-release]" 'face 'warning) parts))
    (apply #'concat (nreverse parts))))

;;; Release body rendering

(defun shipit-releases--insert-release-body (release)
  "Insert the body and assets for RELEASE."
  (let ((body (cdr (assq 'body release)))
        (assets (cdr (assq 'assets release))))
    (if (and body (not (string-empty-p body)))
        (shipit-releases--insert-rendered-body body)
      (insert (propertize "    No release notes.
" 'face 'magit-dimmed)))
    (when assets
      (shipit-releases--insert-assets assets))
    (insert "\n")))

(defun shipit-releases--insert-rendered-body (body)
  "Insert BODY text, rendering markdown if available."
  (let* ((rendered (if (and (boundp 'shipit-render-markdown)
                            shipit-render-markdown
                            (fboundp 'shipit--render-markdown))
                       (shipit--render-markdown body)
                     body))
         (lines (split-string rendered "\n")))
    (dolist (line lines)
      (insert (format "    %s\n" line)))))

(defun shipit-releases--insert-assets (assets)
  "Insert asset listing from ASSETS list."
  (insert (format "    Assets (%d)\n" (length assets)))
  (dolist (asset assets)
    (let* ((name (or (cdr (assq 'name asset)) "?"))
           (size (cdr (assq 'size asset)))
           (downloads (or (cdr (assq 'download_count asset)) 0)))
      (insert (format "      %s  (%s)  %d downloads\n"
                      name
                      (shipit-releases--format-size size)
                      downloads)))))

;;; Releases section

(defun shipit-releases--insert-releases-section (releases _repo)
  "Insert the Releases section with RELEASES data.
_REPO is accepted for interface consistency but currently unused."
  (magit-insert-section (shipit-releases-section)
    (magit-insert-heading "Releases")
    (if (null releases)
        (insert "  No releases found.\n\n")
      (dolist (release releases)
        (shipit-releases--insert-release-entry release)))
    (insert "\n")))

(defun shipit-releases--insert-release-entry (release)
  "Insert a single RELEASE entry as a collapsed magit section."
  (let ((tag (or (cdr (assq 'tag_name release)) "?")))
    (magit-insert-section (shipit-releases-entry tag t)
      (shipit-releases--insert-release-heading release)
      (magit-insert-section-body
        (shipit-releases--insert-release-body release)))))

;;; Tags section

(defun shipit-releases--insert-tags-section (tags _repo)
  "Insert the Tags section with TAGS data.
_REPO is accepted for interface consistency but currently unused."
  (magit-insert-section (shipit-tags-section)
    (magit-insert-heading "Tags")
    (if (null tags)
        (insert "  No tags found.\n\n")
      (dolist (tag tags)
        (shipit-releases--insert-tag-entry tag)))
    (insert "\n")))

(defun shipit-releases--insert-tag-entry (tag)
  "Insert a single TAG entry."
  (let* ((name (or (cdr (assq 'name tag)) "?"))
         (commit (cdr (assq 'commit tag)))
         (sha (or (cdr (assq 'sha commit)) ""))
         (short-sha (if (> (length sha) 7)
                        (substring sha 0 7)
                      sha)))
    (magit-insert-section (shipit-tags-entry name)
      (magit-insert-heading
        (format "  %s  %s"
                (propertize name 'face 'magit-tag)
                (propertize short-sha 'face 'magit-hash))))))

;;; Scroll-to-target

(defun shipit-releases--apply-scroll-to ()
  "Scroll to the target set in `shipit-releases-buffer-scroll-to'."
  (when shipit-releases-buffer-scroll-to
    (let ((target shipit-releases-buffer-scroll-to))
      (setq shipit-releases-buffer-scroll-to nil)
      (goto-char (point-min))
      (cond
       ((eq target 'tags)
        (shipit-releases--scroll-to-tags-section))
       ((stringp target)
        (shipit-releases--scroll-to-release target))))))

(defun shipit-releases--scroll-to-tags-section ()
  "Scroll to the Tags section heading."
  (goto-char (point-min))
  (when (search-forward "Tags" nil t)
    (beginning-of-line)))

(defun shipit-releases--scroll-to-release (tag)
  "Scroll to the release with TAG name."
  (goto-char (point-min))
  (when (search-forward tag nil t)
    (beginning-of-line)))

(provide 'shipit-releases-buffer)
;;; shipit-releases-buffer.el ends here
