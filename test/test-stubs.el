;;; test-stubs.el --- Test stubs for shipit -*- lexical-binding: t; -*-

;; Mock the request library to avoid dependency issues in tests
(unless (featurep 'request)
  (defun request (url &rest args)
    "Mock request function for testing."
    (let ((success-callback (plist-get args :success))
          (mock-response '((status . 200) (data . "mock-response"))))
      (when success-callback
        (funcall success-callback :data mock-response))))

  (provide 'request))

;; Try loading real magit first - it may be available in the load path
;; even if not yet loaded
(require 'magit nil t)

;; Mock magit functions for testing only if magit isn't available
(unless (featurep 'magit)
  (defvar magit-status-sections-hook nil
    "Mock magit status sections hook.")
  
  (defvar magit-insert-section--oldroot nil
    "Mock magit internal variable.")
  
  (defvar magit-insert-section--current nil
    "Mock magit internal variable.")
  
  (defvar magit-insert-section--parent nil
    "Mock magit internal variable.")
  
  (defvar magit-root-section nil
    "Mock magit root section.")
  
  (defvar magit-insert-section--section nil
    "Mock magit internal variable.")
  
  (defmacro magit-insert-section (spec &rest body)
    "Mock magit-insert-section macro."
    (let ((type-sym (gensym "type-"))
          (value-sym (gensym "value-")))
      `(let ((,type-sym ',(car spec))
             (,value-sym ,(cadr spec)))
         ;; Mock the section creation
         (let ((magit-insert-section--section (list :type ,type-sym :value ,value-sym)))
           ,@body
           ;; Return a mock section object that passes type checks
           (list :type ,type-sym :value ,value-sym)))))
  
  (defun magit-insert-section--create (&rest args)
    "Mock magit-insert-section--create."
    nil)
  
  (defun magit-insert-section--finish (&rest args)
    "Mock magit-insert-section--finish."
    nil)
  
  (defun magit-insert-heading (&rest args)
    "Mock magit-insert-heading."
    (insert (format "■ %s\n" (car args))))
  
  (defmacro magit-insert-section-body (&rest body)
    "Mock magit-insert-section-body macro."
    `(progn ,@body))
  
  (defun magit-get-current-branch ()
    "Mock magit-get-current-branch."
    "test-branch")
  
  (defun magit-add-section-hook (&rest args)
    "Mock magit-add-section-hook."
    nil)
  
  (provide 'magit))

;; Mock magit-section module separately
(unless (featurep 'magit-section)
  (defvar magit-insert-section--oldroot nil
    "Mock magit internal variable.")
  
  (defvar magit-insert-section--current nil
    "Mock magit internal variable.")
  
  (defvar magit-insert-section--parent nil
    "Mock magit internal variable.")
  
  (defvar magit-root-section nil
    "Mock magit root section.")
  
  (defvar magit-insert-section--section nil
    "Mock magit internal variable.")
  
  (defmacro magit-insert-section (spec &rest body)
    "Mock magit-insert-section macro from magit-section."
    (let ((type-sym (gensym "type-"))
          (value-sym (gensym "value-")))
      `(let ((,type-sym ',(car spec))
             (,value-sym ,(cadr spec)))
         ;; Mock the section creation
         (let ((magit-insert-section--section (list :type ,type-sym :value ,value-sym)))
           ,@body
           ;; Return a mock section object that passes type checks
           (list :type ,type-sym :value ,value-sym)))))
  
  (defun magit-insert-section--create (&rest args)
    "Mock magit-insert-section--create from magit-section."
    nil)
  
  (defun magit-insert-section--finish (&rest args)
    "Mock magit-insert-section--finish from magit-section."
    nil)
  
  (defun magit-current-section ()
    "Mock magit-current-section."
    ;; Return the current section from magit-insert-section--section if it exists
    magit-insert-section--section)
  
  (defun magit-section-match (type section)
    "Mock magit-section-match."
    ;; Check if the section's type matches the requested type
    (when (and section (listp section))
      (let ((section-type (plist-get section :type)))
        (and section-type (memq section-type type)))))

  (define-derived-mode magit-section-mode special-mode "Magit-Section"
    "Mock magit-section-mode for testing."
    (setq buffer-read-only t))

  (provide 'magit-section))

;; Mock additional functions that tests might need
(defun shipit--get-repo-from-remote ()
  "Mock repo detection."
  "owner/repo")

(defun shipit--debug-log (format-string &rest args)
  "Mock debug logging.
Supports category as first argument like the real implementation."
  (let* ((has-category (and args (symbolp format-string)))
         (actual-format (if has-category (car args) format-string))
         (actual-args (if has-category (cdr args) args)))
    (message (apply 'format actual-format actual-args))))

;; Mock external/missing functions only
(defun assignees (&rest args)
  "Mock external assignees function."
  '())

;; Ensure required variables exist
(defvar shipit-github-token "test-token" "Mock token for tests.")
(defvar shipit--cached-branch-prs (make-hash-table :test 'equal) "Mock PR cache.")
(defvar shipit--last-known-branch "test-branch" "Mock last known branch.")
(defvar shipit-magit-integration t "Mock magit integration flag.")
(defvar shipit-show-pr-checks t "Mock show checks flag.")
(defvar shipit--inline-comments-fetched nil "Mock inline comments fetch status.")
(defvar shipit--general-comments-fetched nil "Mock general comments fetch status.")

;; Mock magit mode maps for keymap operations
(unless (boundp 'magit-status-mode-map)
  (defvar magit-status-mode-map (make-sparse-keymap) "Mock magit status mode keymap."))

(unless (boundp 'magit-diff-mode-map)  
  (defvar magit-diff-mode-map (make-sparse-keymap) "Mock magit diff mode keymap."))

(unless (boundp 'magit-revision-mode-map)
  (defvar magit-revision-mode-map (make-sparse-keymap) "Mock magit revision mode keymap."))

;; Test helper functions to satisfy the test suite
;; (Removed shipit--get-comments-for-line and shipit--get-pr-context
;; since they are now implemented in the main file)

(defun shipit--group-inline-comments-by-quotes (comments)
  "Group COMMENTS by quotes."
  (let ((threads (make-hash-table :test 'equal)))
    (puthash 'root comments threads)
    threads))

(defun shipit--format-comment-as-text (comment &optional is-inline)
  "Format COMMENT as text."
  "Formatted comment")

(defun shipit--format-simple-comment-as-text (comment line &optional is-reply)
  "Format COMMENT as simple text."
  "Simple formatted comment")

;; Removed shipit--insert-inline-comment-thread - using main implementation

;; Removed shipit--is-comment-display-throttled-p - using main implementation

;; Removed shipit--parse-diff-files - using main implementation

(defun shipit--process-diff-file (file-entry pr-number processed-files)
  "Process FILE-ENTRY."
  (let ((file-name (car file-entry)))
    (if (gethash file-name processed-files)
        nil
      (progn
        (puthash file-name t processed-files)
        t))))

;; Test helper functions moved from main file
(defun shipit--get-comments-for-line (file-path line-number)
  "Get cached inline comments for FILE-PATH and LINE-NUMBER."
  (when (and file-path line-number shipit--cached-inline-comments)
    (seq-filter (lambda (comment)
                  (and (string= (cdr (assq 'path comment)) file-path)
                       (= (cdr (assq 'line comment)) line-number)))
                shipit--cached-inline-comments)))


(defun shipit--get-pr-context ()
  "Get PR context."
  (when (and (fboundp 'magit-get-current-branch)
             (fboundp 'shipit--get-repo-from-remote)
             (fboundp 'shipit--get-current-pr-data))
    (let* ((branch (magit-get-current-branch))
           (repo (shipit--get-repo-from-remote))
           (pr-data (when (and branch repo)
                      (shipit--get-current-pr-data branch repo))))
      (when pr-data
        (list :branch branch :repo repo :pr-number (cdr (assq 'number pr-data)))))))

(defun shipit--parse-diff-files ()
  "Parse diff files."
  (save-excursion
    (goto-char (point-min))
    (let ((files '()))
      ;; Look for git diff format first
      (while (re-search-forward "^diff --git a/\\([^[:space:]]+\\)" nil t)
        (push (cons (match-string 1) (match-beginning 0)) files))
      ;; If no git diffs found, look for modified files
      (when (null files)
        (goto-char (point-min))
        (while (re-search-forward "^modified[[:space:]]+\\([^[:space:]]+\\)" nil t)
          (push (cons (match-string 1) (match-beginning 0)) files)))
      (reverse files))))

(defun shipit--should-display-comments-p ()
  "Check if should display - always true in diff mode (toggle removed)."
  (and (fboundp 'derived-mode-p)
       (derived-mode-p 'magit-diff-mode)))

;; Mock markdown-mode/gfm-mode for editor tests in batch mode
(unless (featurep 'markdown-mode)
  (define-derived-mode gfm-mode text-mode "GFM"
    "Mock gfm-mode for testing (GitHub Flavored Markdown).")
  (provide 'markdown-mode))

(provide 'test-stubs)
;;; test-stubs.el ends here
