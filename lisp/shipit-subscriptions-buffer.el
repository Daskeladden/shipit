;;; shipit-subscriptions-buffer.el --- Subscription management buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Dedicated buffer for viewing and managing repo subscriptions
;; across all registered backends.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'transient)
(require 'seq)

;; Forward declarations
(declare-function shipit--get-pr-field-icon "shipit-render")
(declare-function shipit-open-repo-buffer "shipit-repo-buffer")
(declare-function shipit-repo-subscription "shipit-repo-buffer")
(declare-function shipit--subscription-state-from-api "shipit-repo-buffer")
(declare-function shipit--subscription-state-label "shipit-repo-buffer")
(declare-function shipit--star-indicator "shipit-core")

;;; Section types

(defun shipit-subscriptions-root (&rest _args)
  "Magit section identifier for subscriptions root."
  nil)
(put 'shipit-subscriptions-root 'magit-section t)

(defun shipit-subscriptions-backend (&rest _args)
  "Magit section identifier for a backend group."
  nil)
(put 'shipit-subscriptions-backend 'magit-section t)

(defun shipit-subscriptions-repo (&rest _args)
  "Magit section identifier for a repo entry."
  nil)
(put 'shipit-subscriptions-repo 'magit-section t)

;;; Buffer constants

(defconst shipit-subscriptions-buffer-name "*shipit-subscriptions*"
  "Name of the subscriptions buffer.")

;;; Mode

(defvar shipit-subscriptions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'shipit-subscriptions-refresh)
    (define-key map (kbd "r") #'shipit-subscriptions-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'shipit-subscriptions--ret-action)
    (define-key map (kbd "M-;") #'shipit-subscriptions--dwim)
    (define-key map (kbd "w") #'shipit-subscriptions--dwim)
    (define-key map (kbd "s") #'shipit-subscriptions--toggle-star)
    (define-key map (kbd "f") #'shipit-subscriptions-filter)
    map)
  "Keymap for `shipit-subscriptions-mode'.")

(define-derived-mode shipit-subscriptions-mode magit-section-mode
  "Shipit-Subscriptions"
  "Major mode for managing repo subscriptions.

\\{shipit-subscriptions-mode-map}"
  :group 'shipit
  (setq-local revert-buffer-function #'shipit-subscriptions-refresh)
  (setq-local truncate-lines t)
  (shipit--apply-section-defaults))

;;; Filter state

(defvar-local shipit-subscriptions--filter-text ""
  "Current text filter for subscriptions buffer.")

(defvar-local shipit-subscriptions--filter-type nil
  "Current type filter: nil (all), watching, or ignoring.")

(defvar-local shipit-subscriptions--cached-data nil
  "Cached fetch results to avoid re-fetching on filter changes.")

;;; Buffer creation

(defun shipit-subscriptions-buffer-create ()
  "Create or return the singleton subscriptions buffer."
  (let ((buf (get-buffer shipit-subscriptions-buffer-name)))
    (unless buf
      (setq buf (get-buffer-create shipit-subscriptions-buffer-name))
      (with-current-buffer buf
        (shipit-subscriptions-mode)))
    buf))

;;; Backend discovery

(defun shipit-subscriptions--backends-with-watched-repos ()
  "Return alist of (ID . PLIST) for backends with :fetch-watched-repos."
  (cl-remove-if-not
   (lambda (entry)
     (plist-get (cdr entry) :fetch-watched-repos))
   shipit-pr-backends))

;;; Data fetching

(defun shipit-subscriptions--fetch-all ()
  "Fetch watched repos from all supported backends.
Returns alist of (BACKEND-ID BACKEND-PLIST REPOS-LIST).
Each backend\'s :fetch-watched-repos handles its own API strategy."
  (let ((results nil))
    (dolist (entry (shipit-subscriptions--backends-with-watched-repos))
      (let* ((backend-id (car entry))
             (backend-plist (cdr entry))
             (config (list :repo ""))
             (fetch-fn (plist-get backend-plist :fetch-watched-repos))
             (repos (condition-case err
                        (funcall fetch-fn config)
                      (error
                       (shipit--debug-log "Subscriptions: fetch failed for %s: %S"
                                          backend-id err)
                       nil))))
        (push (list backend-id backend-plist repos) results)))
    (nreverse results)))

;;; Filtering

(defun shipit-subscriptions--set-text-filter ()
  "Set a text filter with live updates as you type."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (timer nil))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'post-command-hook
                    (lambda ()
                      (when timer (cancel-timer timer))
                      (setq timer
                            (run-with-idle-timer 0.1 nil
                                                 (lambda ()
                                                   (condition-case nil
                                                       (let ((input (minibuffer-contents-no-properties)))
                                                         (when (buffer-live-p original-buffer)
                                                           (with-current-buffer original-buffer
                                                             (setq shipit-subscriptions--filter-text input)
                                                             (shipit-subscriptions--rerender)
                                                             (redisplay t))))
                                                     (error nil))))))
                    nil t))
      (let ((new-filter (read-string "Filter repos: "
                                     shipit-subscriptions--filter-text)))
        (setq shipit-subscriptions--filter-text new-filter)
        (shipit-subscriptions--rerender)
        (message "Filter set to: %s"
                 (if (string-empty-p new-filter) "(none)" new-filter))))))

(defun shipit-subscriptions--filter-watching ()
  "Show only watching repos."
  (interactive)
  (setq shipit-subscriptions--filter-type 'watching)
  (shipit-subscriptions--rerender))

(defun shipit-subscriptions--filter-ignoring ()
  "Show only ignored repos."
  (interactive)
  (setq shipit-subscriptions--filter-type 'ignoring)
  (shipit-subscriptions--rerender))

(defun shipit-subscriptions--clear-filter ()
  "Clear all filters."
  (interactive)
  (setq shipit-subscriptions--filter-text ""
        shipit-subscriptions--filter-type nil)
  (shipit-subscriptions--rerender))

(transient-define-prefix shipit-subscriptions-filter ()
  "Filter subscriptions."
  ["Filter"
   [("f" "By text" shipit-subscriptions--set-text-filter)
    ("w" "Watching only" shipit-subscriptions--filter-watching)
    ("i" "Ignoring only" shipit-subscriptions--filter-ignoring)]
   [("x" "Clear filter" shipit-subscriptions--clear-filter)
    ("q" "Quit" transient-quit-one)]])

(defun shipit-subscriptions--matches-filter-p (repo)
  "Return non-nil if REPO matches the current filters."
  (let ((name (or (cdr (assq 'full_name repo)) ""))
        (subscription (or (cdr (assq 'subscription repo)) "")))
    ;; Check type filter
    (and (pcase shipit-subscriptions--filter-type
           ('watching (equal subscription "SUBSCRIBED"))
           ('ignoring (equal subscription "IGNORED"))
           (_ t))
         ;; Check text filter
         (or (string-empty-p shipit-subscriptions--filter-text)
             (string-match-p (regexp-quote shipit-subscriptions--filter-text)
                             name)))))

(defun shipit-subscriptions--rerender ()
  "Re-render the subscriptions buffer from cached data."
  (let ((buf (shipit-subscriptions-buffer-create)))
    (with-current-buffer buf
      (when shipit-subscriptions--cached-data
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (shipit-subscriptions--render-from-data
           shipit-subscriptions--cached-data)
          (goto-char (min pos (point-max))))))))

;;; Rendering


(defun shipit-subscriptions--insert-backend-section (backend-id backend-plist repos)
  "Insert a magit section for BACKEND-ID with BACKEND-PLIST and REPOS."
  (let* ((name (plist-get backend-plist :name))
         (emoji (or (plist-get backend-plist :emoji-fallback) ""))
         (icon (shipit--get-pr-field-icon
                (symbol-name backend-id) emoji)))
    (magit-insert-section (shipit-subscriptions-backend backend-id t)
      (magit-insert-heading
        (format "%s %s (%d)" icon name (length repos)))
      (magit-insert-section-body
        (if repos
            (let ((sorted (sort (copy-sequence repos)
                                (lambda (a b)
                                  (string< (or (cdr (assq 'full_name a)) "")
                                           (or (cdr (assq 'full_name b)) ""))))))
              (dolist (repo sorted)
                (shipit-subscriptions--insert-repo-line
                 backend-id repo)))
          (insert (propertize "  No watched repos\n"
                              'font-lock-face 'font-lock-comment-face)))
        (insert "\n")))))

(defun shipit-subscriptions--subscription-label (viewer-subscription)
  "Return display label for VIEWER-SUBSCRIPTION GraphQL enum."
  (cond
   ((equal viewer-subscription "SUBSCRIBED") "Watching")
   ((equal viewer-subscription "IGNORED") "Ignoring")
   ((equal viewer-subscription "UNSUBSCRIBED") "Participating")
   (t (or viewer-subscription "Unknown"))))

(defun shipit-subscriptions--insert-repo-line (backend-id repo)
  "Insert a columnar line for REPO under BACKEND-ID."
  (let* ((full-name (or (cdr (assq 'full_name repo)) "?"))
         (owner (or (cdr (assq 'owner_name repo)) ""))
         (subscription (or (cdr (assq 'subscription repo)) "SUBSCRIBED"))
         (sub-label (shipit-subscriptions--subscription-label subscription))
         (starred (eq (cdr (assq 'starred repo)) t))
         (star-icon (if starred
                        (shipit--get-pr-field-icon "star" "\u2b50")
                      "  "))
         (sub-face (cond
                    ((equal subscription "IGNORED") 'font-lock-comment-face)
                    (t nil)))
         (name-col (truncate-string-to-width full-name 40 nil ?\s))
         (owner-col (truncate-string-to-width owner 20 nil ?\s))
         (sub-col (truncate-string-to-width sub-label 16 nil ?\s)))
    (magit-insert-section (shipit-subscriptions-repo repo)
      (magit-insert-heading
        (propertize
         (format "  %s  %s  %s  %s"
                 name-col owner-col
                 (if sub-face (propertize sub-col 'font-lock-face sub-face) sub-col)
                 star-icon)
         'shipit-subscriptions-repo full-name
         'shipit-subscriptions-backend-id backend-id)))))

(defun shipit-subscriptions--render ()
  "Fetch data and render the subscriptions buffer."
  (setq shipit-subscriptions--cached-data (shipit-subscriptions--fetch-all))
  (shipit-subscriptions--render-from-data shipit-subscriptions--cached-data))

(defun shipit-subscriptions--render-from-data (data)
  "Render the subscriptions buffer from DATA, applying current filters."
  (let* ((has-filter (or (not (string-empty-p shipit-subscriptions--filter-text))
                         shipit-subscriptions--filter-type))
         (filter-desc (cond
                       ((and (not (string-empty-p shipit-subscriptions--filter-text))
                             shipit-subscriptions--filter-type)
                        (format " [filter: %s + %s]"
                                shipit-subscriptions--filter-text
                                shipit-subscriptions--filter-type))
                       ((not (string-empty-p shipit-subscriptions--filter-text))
                        (format " [filter: %s]" shipit-subscriptions--filter-text))
                       (shipit-subscriptions--filter-type
                        (format " [filter: %s]" shipit-subscriptions--filter-type))
                       (t ""))))
    (magit-insert-section (shipit-subscriptions-root)
      (insert (propertize (format "Subscriptions%s\n\n" filter-desc)
                          'font-lock-face 'magit-section-heading))
      (if data
          (dolist (entry data)
            (let* ((backend-id (nth 0 entry))
                   (backend-plist (nth 1 entry))
                   (repos (nth 2 entry))
                   (filtered (if has-filter
                                 (seq-filter #'shipit-subscriptions--matches-filter-p repos)
                               repos)))
              (shipit-subscriptions--insert-backend-section
               backend-id backend-plist filtered)))
        (insert (propertize "  No backends with subscription support\n"
                            'font-lock-face 'font-lock-comment-face))))))

;;; Refresh

(defun shipit-subscriptions-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the subscriptions buffer."
  (interactive)
  (message "Fetching subscriptions...")
  (let ((buf (shipit-subscriptions-buffer-create)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (shipit-subscriptions--render)
        (goto-char (min pos (point-max))))))
  (message "Subscriptions loaded"))

;;; Actions

(defun shipit-subscriptions--repo-at-point ()
  "Return (REPO-NAME . BACKEND-ID) at point, or nil."
  (let ((repo (get-text-property (point) 'shipit-subscriptions-repo))
        (backend-id (get-text-property (point) 'shipit-subscriptions-backend-id)))
    (when (and repo backend-id)
      (cons repo backend-id))))

(defun shipit-subscriptions--ret-action ()
  "Open repo buffer for repo at point."
  (interactive)
  (let ((entry (shipit-subscriptions--repo-at-point)))
    (if entry
        (let ((shipit-pr-backend (cdr entry))
              (shipit-pr-backend-config nil))
          (shipit-open-repo-buffer (car entry)))
      (when-let* ((section (magit-current-section)))
        (magit-section-toggle section)))))

(defun shipit-subscriptions--dwim ()
  "Open subscription transient for repo at point."
  (interactive)
  (let ((entry (shipit-subscriptions--repo-at-point)))
    (if entry
        (progn
          (setq-local shipit-pr-backend (cdr entry))
          (setq-local shipit-pr-backend-config nil)
          (setq-local shipit-repo-buffer-repo (car entry))
          (shipit-repo-subscription))
      (shipit-subscriptions--ret-action))))

(defun shipit-subscriptions--toggle-star ()
  "Toggle star for repo at point."
  (interactive)
  (let ((entry (shipit-subscriptions--repo-at-point)))
    (unless entry (user-error "No repo at point"))
    (let* ((repo (car entry))
           (backend-id (cdr entry))
           (resolved (let ((shipit-pr-backend backend-id)
                           (shipit-pr-backend-config nil))
                       (shipit-pr--resolve-for-repo repo)))
           (backend (car resolved))
           (config (cdr resolved))
           (get-fn (plist-get backend :get-repo-starred))
           (set-fn (plist-get backend :set-repo-starred)))
      (unless set-fn (user-error "Starring not supported for this backend"))
      (let* ((currently-starred (when get-fn (funcall get-fn config)))
             (new-state (not currently-starred)))
        (funcall set-fn config new-state)
        (message "%s %s" (if new-state "Starred" "Unstarred") repo)
        ;; Update cached data and re-render
        (when shipit-subscriptions--cached-data
          (dolist (entry shipit-subscriptions--cached-data)
            (let ((repos (nth 2 entry)))
              (dolist (r repos)
                (when (equal (cdr (assq 'full_name r)) repo)
                  (setcdr (assq 'starred r) new-state))))))
        (shipit-subscriptions--rerender)))))

;;; Entry point

;;;###autoload
(defun shipit-subscriptions ()
  "Open the subscriptions management buffer."
  (interactive)
  (let ((buf (shipit-subscriptions-buffer-create)))
    (shipit-subscriptions-refresh)
    (switch-to-buffer buf)))

(provide 'shipit-subscriptions-buffer)
;;; shipit-subscriptions-buffer.el ends here
