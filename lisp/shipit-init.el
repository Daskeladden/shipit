;;; shipit-init.el --- Simple shipit initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

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
;; One simple function to initialize shipit. That's it.

;;; Code:

;;;###autoload
(defun shipit-init ()
  "Initialize shipit integration. Call this once in your config."
  (interactive)
  
  ;; Load files in dependency order (core → specific modules) - same as shipit-reload
  (let* ((shipit-lib (locate-library "shipit-init"))
         (lisp-dir (when shipit-lib (file-name-directory shipit-lib)))
         (load-files '("shipit-core.el" "shipit-cache.el" "shipit-http.el" "shipit-gh-etag.el"
                       "shipit-render.el" "shipit-diff.el" "shipit-checks.el" "shipit-pr-sections.el"
                       "shipit-commands.el" "shipit-notifications.el" "shipit-notifications-buffer.el"
                       "shipit-debug.el" "shipit.el")))
    (dolist (file load-files)
      (let ((full-path (expand-file-name file lisp-dir)))
        (when (file-exists-p full-path)
          (message "Loading %s..." file)
          (condition-case err
              (progn
                (load full-path)
                (message "Successfully loaded %s" file))
            (error (message "Error loading %s: %s" file (error-message-string err))))))))
  
  ;; Don't load autoloads - they would override the actual functions we just loaded
  
  ;; Set up global keybinding for shipit menu
  (global-set-key (kbd "C-c C-s") #'shipit)

  ;; Set up keybindings in magit mode maps as well (for consistency)
  (when (featurep 'magit)
    (when (boundp 'magit-diff-mode-map)
      (define-key magit-diff-mode-map (kbd "C-c C-s") #'shipit)
      (define-key magit-diff-mode-map (kbd "C-c C-d") #'shipit-debug))
    (when (boundp 'magit-revision-mode-map)
      (define-key magit-revision-mode-map (kbd "C-c C-s") #'shipit)
      (define-key magit-revision-mode-map (kbd "C-c C-d") #'shipit-debug)))

  ;; Set up keybindings for when magit loads later
  (with-eval-after-load 'magit
    (define-key magit-diff-mode-map (kbd "C-c C-s") #'shipit)
    (define-key magit-diff-mode-map (kbd "C-c C-d") #'shipit-debug)
    (define-key magit-revision-mode-map (kbd "C-c C-s") #'shipit)
    (define-key magit-revision-mode-map (kbd "C-c C-d") #'shipit-debug))

  (message "Shipit initialized: all modules loaded, C-c C-s available globally"))

(provide 'shipit-init)
;;; shipit-init.el ends here