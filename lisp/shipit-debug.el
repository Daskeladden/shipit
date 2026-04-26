;;; shipit-debug.el --- Debug utilities for shipit -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 shipit contributors

;; This file is part of shipit.

;; shipit is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; shipit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Comprehensive debug utilities for diagnosing shipit issues.
;; Includes text property analysis, workflow tracing, and comment system debugging.

;;; Code:

(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'transient)
(require 'cl-lib)


;; Forward declarations
(declare-function shipit-config "shipit-commands")
(declare-function shipit-cache "shipit-cache")
(declare-function shipit-clear-url-cache "shipit-gh-etag")
(declare-function shipit--get-current-pr-data "shipit-http")
(declare-function magit-get-current-branch "magit-git")

(defgroup shipit-debug nil
  "Debug utilities for shipit."
  :group 'shipit
  :prefix "shipit-debug-")

(defcustom shipit-debug-output-target 'messages
  "Where to output debug information."
  :type '(choice (const :tag "Messages buffer" messages)
                 (const :tag "Debug log file" log)
                 (const :tag "Both messages and log" both))
  :group 'shipit-debug)

(defcustom shipit-debug-verbose-mode nil
  "Enable verbose debug output with additional context."
  :type 'boolean
  :group 'shipit-debug)

;;; Debug output functions

(defun shipit-debug--output (format-string &rest args)
  "Output debug message according to shipit-debug-output-target.
Only outputs if debug logging is enabled via transient or variable."
  (when (shipit-debug--is-enabled)
    (let ((message (apply 'format format-string args)))
      (pcase shipit-debug-output-target
        ('messages (message "%s" message))
        ('log (shipit--debug-log "%s" message))
        ('both 
         (message "%s" message)
         (shipit--debug-log "%s" message))))))

;;; Text property analysis functions

(defun shipit-debug-text-properties-at-point ()
  "Debug text properties at point in shipit magit buffer."
  (interactive)
  (let ((props (text-properties-at (point))))
    (shipit-debug--output "=== TEXT PROPERTIES DEBUG at point %d ===" (point))
    (if props
        (progn
          (shipit-debug--output "Found %d text properties:" (/ (length props) 2))
          (let ((i 0))
            (while (< i (length props))
              (shipit-debug--output "  %s: %s" (nth i props) (nth (1+ i) props))
              (setq i (+ i 2)))))
      (shipit-debug--output "  No text properties found at this position"))
    
    ;; Specifically check shipit properties that matter for inline comments
    (let ((file-path (get-text-property (point) 'shipit-file-path))
          (line-number (get-text-property (point) 'shipit-line-number))
          (comment-id (get-text-property (point) 'shipit-comment-id))
          (comment-body (get-text-property (point) 'shipit-comment-body))
          (repo (get-text-property (point) 'shipit-repo))
          (pr-number (get-text-property (point) 'shipit-pr-number)))
      (shipit-debug--output "=== SHIPIT-SPECIFIC PROPERTIES ===")
      (shipit-debug--output "  shipit-file-path: %s" file-path)
      (shipit-debug--output "  shipit-line-number: %s" line-number)  
      (shipit-debug--output "  shipit-comment-id: %s" comment-id)
      (shipit-debug--output "  shipit-comment-body: %s" (if comment-body (substring comment-body 0 (min 50 (length comment-body))) "nil"))
      (shipit-debug--output "  shipit-repo: %s" repo)
      (shipit-debug--output "  shipit-pr-number: %s" pr-number)
      
      ;; Basic keybinding info for debugging
      (shipit-debug--output "=== KEYBINDING ANALYSIS ===")
      (shipit-debug--output "  shipit-notifications-mode active: %s" (bound-and-true-p shipit-notifications-mode))
      (shipit-debug--output "  M-; binding: %s" (key-binding (kbd "M-;")))
      
      ;; Analyze magit section context
      (shipit-debug--output "=== MAGIT SECTION ANALYSIS ===")
      (let ((section (get-text-property (point) 'magit-section)))
        (if section
            (progn
              (shipit-debug--output "  Section type: %s" (oref section type))
              (shipit-debug--output "  Section value: %s" (oref section value))
              (let ((section-type (oref section type)))
                (cond
                 ((eq section-type 'pr-commits)
                  (shipit-debug--output "  ✓ In COMMITS section - General comments available"))
                 ((eq section-type 'pr-files)
                  (shipit-debug--output "  ✓ In FILES section - Inline comments may be available"))
                 ((memq section-type '(file unstaged staged))
                  (shipit-debug--output "  ✓ In FILE section - Inline comments may be available"))
                 ((eq section-type 'hunk)
                  (shipit-debug--output "  ✓ In HUNK section - Inline comments may be available"))
                 (t
                  (shipit-debug--output "  ? In %s section - Comment behavior depends on context" section-type)))))
          (shipit-debug--output "  No magit section found at this position")))
      
      ;; Decision logic (same as shipit--execute-action)
      (shipit-debug--output "=== DECISION LOGIC ===")
      (if (and file-path line-number)
          (shipit-debug--output "  DECISION: Would use INLINE comment API (file-path AND line-number present)")
        (shipit-debug--output "  DECISION: Would use GENERAL comment API (missing file-path or line-number)")))))

(defun shipit-debug-nearby-properties ()
  "Debug text properties in nearby area around point."
  (interactive)
  (shipit-debug--output "=== NEARBY PROPERTIES SCAN ===")
  (save-excursion
    (let ((start-pos (max (point-min) (- (point) 100)))
          (end-pos (min (point-max) (+ (point) 100)))
          (found-count 0))
      (goto-char start-pos)
      (while (<= (point) end-pos)
        (let ((line-prop (get-text-property (point) 'shipit-line-number))
              (file-prop (get-text-property (point) 'shipit-file-path)))
          (when (or line-prop file-prop)
            (setq found-count (1+ found-count))
            (when (< found-count 10) ; Limit output
              (shipit-debug--output "  pos %d: file=%s line=%s" (point) file-prop line-prop))))
        (forward-char 1))
      (shipit-debug--output "Total positions with shipit properties in ±100 chars: %d" found-count))))

;;; Workflow analysis functions

(defun shipit-debug-workflow-analysis ()
  "Complete debugging of the shipit inline comment workflow."
  (interactive)
  (shipit-debug--output "=== SHIPIT WORKFLOW ANALYSIS ===")
  
  ;; 1. Check current buffer mode
  (shipit-debug--output "Current buffer: %s (mode: %s)" (buffer-name) major-mode)
  
  ;; 2. Check cursor position and text properties
  (shipit-debug-text-properties-at-point)
  
  ;; 3. Check what function would be called
  (shipit-debug--output "=== FUNCTION CALL ANALYSIS ===")
  (let* ((repo (shipit--get-repo-from-remote))
         (pr (when (car-safe shipit--current-displayed-pr)
               (let ((displayed-pr-number (car shipit--current-displayed-pr)))
                 (shipit-get-pull-request displayed-pr-number)))))
    (shipit-debug--output "Repo: %s" repo)
    (shipit-debug--output "Displayed PR: %s" (car-safe shipit--current-displayed-pr))
    (shipit-debug--output "PR data available: %s" (if pr "YES" "NO"))
    
    (if pr
        (shipit-debug--output "Would call: shipit--comment-mini-editor")
      (shipit-debug--output "Would show error: No PR currently displayed"))))

(defun shipit-debug-trace-action-execution ()
  "Trace what happens when shipit action is executed."
  (interactive)
  (shipit-debug--output "=== TRACING ACTION EXECUTION ===")
  
  ;; Mock the execute action to see what parameters it gets
  (let ((original-execute (symbol-function 'shipit--execute-action)))
    (cl-letf (((symbol-function 'shipit--execute-action)
               (lambda (mode input pr-number repo comment-info file-path line-number in-general on-comment side &optional pr-info on-pr-description labels-info on-labels pr-header-info on-pr-header reviewers-info on-reviewers)
                 (shipit-debug--output "shipit--execute-action called with:")
                 (shipit-debug--output "  mode: %s" mode)
                 (shipit-debug--output "  pr-number: %s" pr-number) 
                 (shipit-debug--output "  repo: %s" repo)
                 (shipit-debug--output "  file-path: %s" file-path)
                 (shipit-debug--output "  line-number: %s" line-number)
                 (shipit-debug--output "  in-general: %s" in-general)
                 (shipit-debug--output "  on-comment: %s" on-comment)
                 
                 ;; Also check what get-text-property returns at this moment
                 (let ((stored-file-path (get-text-property (point) 'shipit-file-path))
                       (stored-line-number (get-text-property (point) 'shipit-line-number)))
                   (shipit-debug--output "  REAL-TIME text properties:")
                   (shipit-debug--output "    stored-file-path: %s" stored-file-path)
                   (shipit-debug--output "    stored-line-number: %s" stored-line-number))
                 
                 ;; Call original function
                 (funcall original-execute mode input pr-number repo comment-info file-path line-number in-general on-comment side pr-info on-pr-description labels-info on-labels pr-header-info on-pr-header reviewers-info on-reviewers))))
      
      ;; Now trigger the modern DWIM workflow
      (shipit-debug--output "Triggering shipit-dwim (modern approach)...")
      (condition-case err
          (shipit-dwim)
        (error (shipit-debug--output "Error in workflow: %s" (error-message-string err)))))))

;;; Buffer analysis functions

(defun shipit-debug-scan-all-comments ()
  "Scan entire buffer for all shipit comments and their properties."
  (interactive)
  (shipit-debug--output "=== SCANNING ALL COMMENTS IN BUFFER ===")
  
  (save-excursion
    (goto-char (point-min))
    (let ((comment-count 0)
          (good-inline-count 0)
          (bad-inline-count 0)
          (general-count 0)
          (file-header-count 0))
      
      (while (< (point) (point-max))
        (when (get-text-property (point) 'shipit-comment)
          (let* ((comment-id (get-text-property (point) 'shipit-comment-id))
                 (has-file (get-text-property (point) 'shipit-file-path))
                 (has-line (get-text-property (point) 'shipit-line-number))
                 (line-start (line-beginning-position))
                 (line-text (buffer-substring line-start (min (+ line-start 60) (line-end-position)))))
            
            (setq comment-count (1+ comment-count))
            
            (cond
             ((and has-file has-line)
              (setq good-inline-count (1+ good-inline-count))
              (when shipit-debug-verbose-mode
                (shipit-debug--output "✓ GOOD inline comment at %d: %s" (point) line-text)))
             ((and has-file (not has-line))
              (setq file-header-count (1+ file-header-count))
              (when shipit-debug-verbose-mode
                (shipit-debug--output "- File header at %d: %s" (point) line-text)))
             ((and (not has-file) has-line)
              (setq bad-inline-count (1+ bad-inline-count)) 
              (shipit-debug--output "✗ BAD inline comment (no file) at %d: %s" (point) line-text))
             (t
              (setq general-count (1+ general-count))
              (when shipit-debug-verbose-mode
                (shipit-debug--output "- General comment at %d: %s" (point) line-text))))))
        
        (forward-char 1))
      
      (shipit-debug--output "=== SUMMARY ===")
      (shipit-debug--output "Total shipit comments: %d" comment-count)
      (shipit-debug--output "Good inline comments (file+line): %d" good-inline-count)
      (shipit-debug--output "File headers (file only): %d" file-header-count)
      (shipit-debug--output "Bad inline comments (missing file): %d" bad-inline-count)  
      (shipit-debug--output "General comments: %d" general-count)
      (when (> bad-inline-count 0)
        (shipit-debug--output "⚠️  WARNING: Found %d comments with incomplete properties!" bad-inline-count)))))

(defun shipit-debug-comment-insertion-analysis ()
  "Debug which comment insertion function was used for the comment at point."
  (interactive)
  (shipit-debug--output "=== COMMENT INSERTION FUNCTION DEBUG ===")
  
  (let ((props (text-properties-at (point))))
    (if (not props)
        (shipit-debug--output "No text properties - comment wasn't inserted by shipit functions")
      (let* ((comment-id (get-text-property (point) 'shipit-comment-id))
             (has-file (get-text-property (point) 'shipit-file-path))
             (has-line (get-text-property (point) 'shipit-line-number))
             (has-repo (get-text-property (point) 'shipit-repo))
             (buffer-mode major-mode))
        
        (shipit-debug--output "Comment ID: %s" comment-id)
        (shipit-debug--output "Buffer mode: %s" buffer-mode)
        (shipit-debug--output "Has shipit-file-path: %s" (if has-file "YES" "NO"))
        (shipit-debug--output "Has shipit-line-number: %s" (if has-line "YES" "NO"))
        (shipit-debug--output "Has shipit-repo: %s" (if has-repo "YES" "NO"))
        
        ;; Deduce which function was likely used
        (cond
         ((and (eq buffer-mode 'magit-status-mode) has-file has-line has-repo)
          (shipit-debug--output "LIKELY INSERTED BY: shipit--insert-status-hierarchical-comment (GOOD)"))
         ((and (eq buffer-mode 'magit-diff-mode) has-file has-line has-repo)
          (shipit-debug--output "LIKELY INSERTED BY: shipit--insert-diff-hierarchical-comment (GOOD)"))
         ((and has-file (not has-line))
          (shipit-debug--output "LIKELY INSERTED BY: File header insertion (EXPECTED - no line number)"))
         ((not (or has-file has-line))
          (shipit-debug--output "LIKELY INSERTED BY: General comment insertion (EXPECTED - no file context)"))
         (t
          (shipit-debug--output "LIKELY INSERTED BY: Unknown/partial insertion function (PROBLEMATIC)")))))))

;;; System state functions

(defun shipit-debug-system-state ()
  "Show current shipit system state and configuration."
  (interactive)
  (shipit-debug--output "=== SHIPIT SYSTEM STATE ===")
  (shipit-debug--output "shipit-magit-integration: %s" shipit-magit-integration)
  (shipit-debug--output "shipit-use-magit-sections-for-diff-comments: %s" shipit-use-magit-sections-for-diff-comments)
  (shipit-debug--output "shipit-debug-logging-enabled: %s" (if (boundp 'shipit-debug-logging-enabled) (symbol-value 'shipit-debug-logging-enabled) "not defined"))
  (shipit-debug--output "shipit-debug-log-max-lines: %s" (if (boundp 'shipit-debug-log-max-lines) (symbol-value 'shipit-debug-log-max-lines) "not defined"))
  (shipit-debug--output "shipit--current-displayed-pr: %s" shipit--current-displayed-pr)
  (shipit-debug--output "Current working directory: %s" default-directory)
  (shipit-debug--output "Git repository: %s" (shipit--get-repo-from-remote))
  
  ;; Cache state
  (shipit-debug--output "=== CACHE STATE ===")
  (shipit-debug--output "shipit--cached-inline-comments count: %d" 
                        (if (boundp 'shipit--cached-inline-comments) 
                            (if shipit--cached-inline-comments (length shipit--cached-inline-comments) 0) 0))
  (shipit-debug--output "shipit--cached-general-comments count: %d"
                        (if (boundp 'shipit--cached-general-comments)
                            (if shipit--cached-general-comments (length shipit--cached-general-comments) 0) 0))
  (shipit-debug--output "shipit--inline-comments-fetched: %s" (if (boundp 'shipit--inline-comments-fetched) shipit--inline-comments-fetched "not defined"))
  (shipit-debug--output "shipit--general-comments-fetched: %s" (if (boundp 'shipit--general-comments-fetched) shipit--general-comments-fetched "not defined")))

(defun shipit-debug-pr-data-analysis ()
  "Analyze current PR data structure to debug loading issues."
  (interactive)
  (shipit-debug--output "=== PR DATA ANALYSIS ===")
  
  ;; Check displayed PR context
  (let ((displayed-pr (when (boundp 'shipit--current-displayed-pr) shipit--current-displayed-pr)))
    (shipit-debug--output "Displayed PR: %s" displayed-pr)
    
    (if (not displayed-pr)
        (shipit-debug--output "ERROR: No PR currently displayed")
      (let* ((pr-number (car displayed-pr))
             (repo (cdr displayed-pr)))
        (shipit-debug--output "PR Number: %s" pr-number)
        (shipit-debug--output "Repository: %s" repo)
        
        ;; Try to fetch current PR data (enhanced with commits and files)
        (shipit-debug--output "=== FETCHING PR DATA ===")
        (condition-case err
            (let* ((basic-pr-data (shipit-get-pull-request pr-number))
                   (pr-data (when basic-pr-data
                              (shipit-debug--output "✓ Basic PR data fetched, enhancing with commits/files...")
                              (shipit--fetch-commits-and-files-parallel-sync repo pr-number basic-pr-data))))
              (if pr-data
                  (progn
                    (shipit-debug--output "✓ PR data fetched successfully")
                    (shipit-debug--output "PR title: %s" (cdr (assq 'title pr-data)))
                    (shipit-debug--output "PR state: %s" (cdr (assq 'state pr-data)))
                    
                    ;; Analyze commits
                    (let ((commits (cdr (assq 'commits pr-data))))
                      (shipit-debug--output "=== COMMITS ANALYSIS ===")
                      (shipit-debug--output "Commits type: %s" (type-of commits))
                      (shipit-debug--output "Commits length: %s" 
                                            (cond 
                                             ((null commits) "nil")
                                             ((sequencep commits) (length commits))
                                             (t "not a sequence")))
                      (when (and (sequencep commits) (> (length commits) 0))
                        (shipit-debug--output "First commit keys: %s" 
                                              (mapcar #'car (car commits)))))
                    
                    ;; Analyze files
                    (let ((files (cdr (assq 'files pr-data))))
                      (shipit-debug--output "=== FILES ANALYSIS ===")
                      (shipit-debug--output "Files type: %s" (type-of files))
                      (shipit-debug--output "Files length: %s"
                                            (cond
                                             ((null files) "nil")
                                             ((sequencep files) (length files))
                                             (t "not a sequence")))
                      (when (and (sequencep files) (> (length files) 0))
                        (shipit-debug--output "First file keys: %s"
                                              (mapcar #'car (car files))))))
                (shipit-debug--output "ERROR: Failed to fetch PR data")))
          (error 
           (shipit-debug--output "ERROR fetching PR data: %s" (error-message-string err))))))))

;;; Log management functions

(defun shipit-debug-view-log ()
  "View the shipit debug log file."
  (interactive)
  (if (boundp 'shipit--debug-log-file)
      (if (file-exists-p shipit--debug-log-file)
          (find-file-other-window shipit--debug-log-file)
        (message "Debug log file does not exist: %s" shipit--debug-log-file))
    (message "Debug log file not defined")))

(defun shipit-debug-clear-log ()
  "Clear the shipit debug log file."
  (interactive)
  (when (yes-or-no-p "Clear shipit debug log? ")
    (if (boundp 'shipit--debug-log-file)
        (progn
          (when (file-exists-p shipit--debug-log-file)
            (delete-file shipit--debug-log-file))
          (shipit-debug--output "Debug log cleared"))
      (message "Debug log file not defined"))))

(defun shipit-debug-set-log-rotation-size ()
  "Set the maximum number of lines for debug log rotation."
  (interactive)
  (let* ((current-size (if (boundp 'shipit-debug-log-max-lines) 
                          shipit-debug-log-max-lines 
                          1000))
         (new-size (read-number (format "Log rotation size (current: %d lines): " current-size) 
                               current-size)))
    (when (and new-size (> new-size 0))
      (setq shipit-debug-log-max-lines new-size)
      (shipit-debug--output "Log rotation size set to %d lines" new-size))))

(defun shipit-debug-toggle-verbose-mode ()
  "Toggle verbose debug mode."
  (interactive)
  (setq shipit-debug-verbose-mode (not shipit-debug-verbose-mode))
  (message "Shipit debug verbose mode: %s" (if shipit-debug-verbose-mode "ENABLED" "DISABLED")))

(defun shipit-debug-toggle-output-target ()
  "Toggle between messages, log, and both output targets."
  (interactive)
  (setq shipit-debug-output-target
        (pcase shipit-debug-output-target
          ('messages 'log)
          ('log 'both)
          ('both 'messages)))
  (message "Debug output target: %s" shipit-debug-output-target))

;;; Test functions

(defun shipit-debug-test-text-properties ()
  "Test text property detection on a sample comment."
  (interactive)
  (shipit-debug--output "=== TEXT PROPERTY TEST ===")
  (with-temp-buffer
    (insert "Sample comment text")
    (add-text-properties (point-min) (point-max)
                        `(shipit-comment t
                          shipit-file-path "test.js"
                          shipit-line-number 42
                          shipit-comment-id 12345))
    (goto-char (point-min))
    (let ((file-path (get-text-property (point) 'shipit-file-path))
          (line-number (get-text-property (point) 'shipit-line-number)))
      (shipit-debug--output "Test properties - file: %s, line: %s" file-path line-number)
      (if (and file-path line-number)
          (shipit-debug--output "✓ Text property detection working correctly")
        (shipit-debug--output "✗ Text property detection failed")))))

;;; Transient menu

;; Define variables for transient infixes
;; Use the same variable as the actual debug logging system
;; Keep in sync with the actual debug logging variable
(defvaralias 'shipit-debug-logging-enabled 'shipit-debug-log-enabled)

(defvar shipit-debug-verbose-mode nil  
  "Whether verbose debug mode is currently enabled.")

(defvar shipit-debug-output-target 'messages
  "Target for debug output: 'messages, 'log, or 'both.")

(defun shipit-debug--is-enabled ()
  "Check if debug logging is enabled via transient args or variable."
  (or (and (boundp 'shipit-debug-logging-enabled) shipit-debug-logging-enabled)
      (and (boundp 'transient-current-command)
           (eq transient-current-command 'shipit-debug-menu)
           (member "--debug" (transient-args 'shipit-debug-menu)))))

;; Define proper transient infix classes for shipit options
(transient-define-argument shipit-debug--log-max-lines ()
  :description "Max log lines"
  :class 'transient-option
  :argument "--max-lines="
  :reader (lambda (_ prompt _)
            (let ((value (read-number (or prompt "Max log lines: ") shipit-debug-log-max-lines)))
              (setq shipit-debug-log-max-lines value)
              (number-to-string value))))

(transient-define-argument shipit-debug--logging-switch ()
  :description "Enable debug logging"
  :class 'transient-switch
  :argument "--debug"
  :init-value (lambda (obj)
                (when (and (boundp 'shipit-debug-logging-enabled) 
                           shipit-debug-logging-enabled)
                  (oset obj value "--debug"))))

(transient-define-argument shipit-debug--verbose-switch ()
  :description "Enable verbose mode"
  :class 'transient-switch
  :argument "--verbose")

(transient-define-argument shipit-debug--output-target ()
  :description "Output target"
  :class 'transient-option
  :argument "--output="
  :choices '("messages" "log" "both")
  :init-value (lambda (obj)
                (when (boundp 'shipit-debug-output-target)
                  (oset obj value (format "--output=%s" shipit-debug-output-target))))
  :reader (lambda (_ prompt _)
            (let* ((prompt-text (or prompt "Output target: "))
                   (choice (completing-read prompt-text '("messages" "log" "both") nil t)))
              (setq shipit-debug-output-target (intern choice))
              (message "Debug output target set to: %s" choice)
              (format "--output=%s" choice))))

;; Main menu transient switches
(transient-define-argument shipit-debug--notification-mode-switch ()
  :description "Enable notification mode"
  :class 'transient-switch
  :argument "--notifications"
  :init-value (lambda (obj)
                (when (bound-and-true-p shipit-notifications-mode)
                  (oset obj value "--notifications")))
  :reader (lambda (_ _ _)
            (shipit-notifications-mode 'toggle)
            "--notifications"))

;; Dynamic description functions for transient menu
(defun shipit-debug--toggle-logging-description ()
  "Dynamic description for debug logging toggle."
  (let ((enabled (and (boundp 'shipit-debug-logging-enabled) shipit-debug-logging-enabled)))
    (format "Toggle debug logging%s" 
            (if enabled " --debug" ""))))

(defun shipit-debug-general-comments ()
  "Debug cached general comments to show their types and content."
  (interactive)
  (if (and (boundp 'shipit--cached-general-comments) shipit--cached-general-comments)
      (progn
        (message "=== DEBUG: Cached General Comments ===")
        (message "Total comments: %d" (length shipit--cached-general-comments))
        (dolist (comment shipit--cached-general-comments)
          (let* ((id (cdr (assq 'id comment)))
                 (body (cdr (assq 'body comment)))
                 (user (cdr (assq 'login (cdr (assq 'user comment)))))
                 (comment-type (cdr (assq 'shipit-comment-type comment)))
                 (review-state (cdr (assq 'review-state comment)))
                 (state (cdr (assq 'state comment)))
                 (body-preview (if body (substring body 0 (min 50 (length body))) "NO BODY")))
            (message "Comment ID %s: type=%s, state=%s, review-state=%s, user=%s, body='%s...'"
                     id comment-type state review-state user body-preview)))
        (message "=== END DEBUG ==="))
    (message "No cached general comments found")))

(transient-define-prefix shipit-debug-menu ()
  "Shipit debug utilities menu."
  :man-page "shipit-debug"
  ["Debug Analysis"
   [("p" "Text properties at point" shipit-debug-text-properties-at-point)
    ("n" "Nearby properties" shipit-debug-nearby-properties)
    ("w" "Full workflow analysis" shipit-debug-workflow-analysis)
    ("t" "Trace action execution" shipit-debug-trace-action-execution)]
   [("s" "Scan all comments" shipit-debug-scan-all-comments)
    ("i" "Comment insertion analysis" shipit-debug-comment-insertion-analysis)
    ("S" "System state" shipit-debug-system-state)]
   [("P" "PR data analysis" shipit-debug-pr-data-analysis)
    ("R" "Profile Emacs (write report)" shipit-debug-profile)
    ("L" "List shipit timers" shipit-debug-list-timers)]]
  ["Log Management"
   [("v" "View debug log" shipit-debug-view-log)
    ("c" "Clear debug log" shipit-debug-clear-log)
    ("C" "Configure categories" shipit-debug-select-categories)
    ("-l" shipit-debug--logging-switch)
    ("-r" shipit-debug--log-max-lines)]
   [("-V" shipit-debug--verbose-switch)
    ("-o" shipit-debug--output-target)
    ("!" "Doctor (API/rate limits)" shipit-doctor)]]
  ["Tests"
   [("T" "Test text properties" shipit-debug-test-text-properties)]]
  ["Actions"
   [("C-x C-s" "Save and stay" shipit-debug--save-and-sync)]]
  ["Quit"
   [("q" "Quit" shipit-debug--quit-and-sync)]])

(defun shipit-debug-select-categories ()
  "Interactively select which debug categories to log."
  (interactive)
  (require 'shipit-core)
  (let* ((available-categories '(all file-diff reactions comments api cache buffer notifications checks profiling))
         (current (if (boundp 'shipit-debug-categories) shipit-debug-categories '(all)))
         (choices (mapcar (lambda (cat)
                           (list (symbol-name cat)
                                 (if (memq cat current) t nil)))
                         available-categories))
         (selected (completing-read-multiple
                    (format "Debug categories (current: %s): " current)
                    available-categories nil t)))
    (if (null selected)
        (message "No categories selected, keeping current: %s" shipit-debug-categories)
      (let ((new-categories (mapcar #'intern selected)))
        (setq shipit-debug-categories new-categories)
        (message "Debug categories set to: %s" shipit-debug-categories)))))

;; Cache menu wrapper functions to avoid dependency issues
(defun shipit-debug--clear-branch-cache ()
  "Wrapper for clearing branch cache."
  (interactive)
  (require 'shipit-checks)
  (shipit--clear-branch-cache)
  (message "Branch cache cleared"))

(defun shipit-debug--clear-inline-comments-cache ()
  "Wrapper for clearing inline comments cache."
  (interactive)
  (require 'shipit-cache)
  (shipit--clear-inline-comments-cache))

(defun shipit-debug--clear-general-comments-cache ()
  "Wrapper for clearing general comments cache."
  (interactive)
  (require 'shipit-cache)
  (shipit--clear-general-comments-cache))

(defun shipit-debug--clear-all-caches ()
  "Wrapper for clearing all caches."
  (interactive)
  (require 'shipit-cache)
  (shipit-clear-all-caches))

(defun shipit-debug--clear-team-membership-cache ()
  "Wrapper for clearing team membership cache."
  (interactive)
  (require 'shipit-cache)
  (shipit-clear-team-membership-cache))

(defun shipit-debug--debug-reaction-cache ()
  "Wrapper for debugging reaction cache."
  (interactive)
  (require 'shipit-cache)
  (shipit--debug-reaction-cache))

(defun shipit-debug--clear-etag-cache ()
  "Wrapper for clearing ETag cache."
  (interactive)
  (require 'shipit-gh-etag)
  (shipit-clear-etag-cache))

(defun shipit-debug--clear-etag-cache-for-repo ()
  "Wrapper for clearing ETag cache for repo."
  (interactive)
  (require 'shipit-gh-etag)
  (call-interactively 'shipit-clear-etag-cache-for-repo))

(defun shipit-debug--show-etag-cache-stats ()
  "Wrapper for showing ETag cache stats."
  (interactive)
  (require 'shipit-gh-etag)
  (shipit-show-etag-cache-stats))

(defun shipit-debug--clear-url-cache ()
  "Wrapper for clearing URL cache."
  (interactive)
  (require 'shipit-gh-etag)
  (shipit-clear-url-cache))

(transient-define-prefix shipit-cache-menu ()
  "Cache management menu for shipit."
  :man-page "shipit-cache"
  ["ETag Cache (API Responses)"
   [("c" "Clear all ETag cache" shipit-debug--clear-etag-cache)
    ("r" "Clear repo ETag cache" shipit-debug--clear-etag-cache-for-repo)
    ("s" "Show ETag cache stats" shipit-debug--show-etag-cache-stats)]
   [("u" "Clear URL cache" shipit-debug--clear-url-cache)]]
  ["Application Cache (Comments & Data)"  
   [("b" "Clear branch cache" shipit-debug--clear-branch-cache)
    ("i" "Clear inline comments" shipit-debug--clear-inline-comments-cache)
    ("g" "Clear general comments" shipit-debug--clear-general-comments-cache)]
   [("a" "Clear all caches" shipit-debug--clear-all-caches)
    ("t" "Clear team membership" shipit-debug--clear-team-membership-cache)]]
  ["Cache Information"
   [("?" "Debug reaction cache" shipit-debug--debug-reaction-cache)
    ("q" "Quit" transient-quit-one)]])

(defun shipit-debug--quit-and-sync ()
  "Quit debug transient and sync state to variable."
  (interactive)
  (let ((args (transient-args 'shipit-debug-menu)))
    (setq shipit-debug-logging-enabled (if (member "--debug" args) t nil)))
  (transient-quit-one))

(defun shipit-debug--save-and-sync ()
  "Save debug transient state and sync to variable."
  (interactive)
  (let ((args (transient-args 'shipit-debug-menu)))
    (setq shipit-debug-logging-enabled (if (member "--debug" args) t nil)))
  ;; Save the transient state
  (transient-save)
  (message "Debug settings saved"))


;;; Main shipit transient menu

;;;###autoload
(defun shipit--reload-wrapper ()
  "Wrapper to load and call shipit-reload from dev utils."
  (interactive)
  (let* ((shipit-lib (locate-library "shipit-debug"))
         (lisp-dir (when shipit-lib (file-name-directory shipit-lib)))
         (project-root (when lisp-dir (file-name-directory (directory-file-name lisp-dir))))
         (dev-utils-file (when project-root (expand-file-name "shipit-dev-utils.el" project-root))))
    (if (and dev-utils-file (file-exists-p dev-utils-file))
        (progn
          (load-file dev-utils-file)
          (if (fboundp 'shipit-reload)
              (call-interactively 'shipit-reload)
            (message "shipit-reload function not found after loading dev-utils")))
      (message "Could not locate shipit-dev-utils.el file"))))


;;; Missing function stubs for main menu

;;;###autoload
(defun shipit--open-pr-in-browser (&optional external)
  "Open current PR in browser.
With prefix argument EXTERNAL, force external browser.
Without prefix, uses `browse-url' default behavior."
  (interactive "P")
  (let* ((pr-number (or (get-text-property (point) 'shipit-pr-number)
                        (and (boundp 'shipit--current-pr-number)
                             shipit--current-pr-number)))
         (repo (or (get-text-property (point) 'shipit-repo)
                   (and (boundp 'shipit--current-repo)
                        shipit--current-repo)
                   (shipit--get-repo-from-remote))))
    ;; If still no PR number, try to get from current branch
    (unless pr-number
      (when repo
        (let* ((branch (magit-get-current-branch))
               (pr-data (when branch
                          (shipit--get-current-pr-data branch repo))))
          (when pr-data
            (setq pr-number (cdr (assq 'number pr-data)))))))
    (cond
     ((not pr-number)
      (user-error "No PR found in current context"))
     ((not repo)
      (user-error "Could not determine repository"))
     (t
      (let* ((resolved (shipit-pr--resolve-for-repo repo))
             (backend (car resolved))
             (config (cdr resolved))
             (url (funcall (plist-get backend :browse-url) config pr-number)))
        (if external
            ;; Force external browser
            (browse-url-default-browser url)
          ;; Default behavior (respects browse-url-browser-function)
          (browse-url url))
        (message "Opened PR #%d in browser" pr-number))))))

(defun shipit-help ()
  "Show shipit help."
  (interactive)
  (message "Shipit Help: M-; for context actions, C-c C-s for main menu, C-c C-d for debug tools"))

;;; Debug entry points

;;;###autoload
(defun shipit-debug ()
  "Open shipit debug utilities menu."
  (interactive)
  (shipit-debug-menu))

;;; Section hierarchy debugging

(defun shipit--find-pr-section-recursive (section)
  "Recursively find shipit-pr section starting from SECTION."
  (when section
    (if (magit-section-match '(shipit-pr) section)
        section
      ;; Search children
      (catch 'found
        (dolist (child (oref section children))
          (let ((result (shipit--find-pr-section-recursive child)))
            (when result
              (throw 'found result))))
        nil))))

(defun shipit-debug-pr-section-hierarchy ()
  "Debug function to explore PR section hierarchy - children and siblings."
  (interactive)
  (message "Running PR hierarchy debug - check shipit debug log")
  (condition-case err
      (when (fboundp 'magit-current-section)
        (let ((current-section (magit-current-section)))
          (shipit--debug-log "=== PR HIERARCHY DEBUG START ===")
          (shipit--debug-log "Current section: %s" current-section)
          (when current-section
            (shipit--debug-log "Current section type: %s" (oref current-section type))

            ;; Try to find the shipit-pr section by searching all sections
            (shipit--debug-log "--- Searching for shipit-pr section ---")
            (let ((pr-section nil))
              ;; Walk up the hierarchy to find root, then search down
              (let ((root-section current-section))
                (while (and root-section (oref root-section parent))
                  (setq root-section (oref root-section parent)))
                (shipit--debug-log "Root section: %s (type: %s)" root-section
                                   (when root-section (oref root-section type)))

                ;; Search for shipit-pr section recursively
                (when root-section
                  (setq pr-section (shipit--find-pr-section-recursive root-section))))

              (if pr-section
                  (progn
                    (shipit--debug-log "--- FOUND PR SECTION ---")
                    (shipit--debug-log "PR section: %s" pr-section)
                    (shipit--debug-log "PR section type: %s" (oref pr-section type))

                    ;; List PR section's children
                    (shipit--debug-log "--- PR SECTION CHILDREN ---")
                    (let ((children (oref pr-section children)))
                      (shipit--debug-log "PR section has %d children:" (length children))
                      (dolist (child children)
                        (shipit--debug-log "  Child: %s (type: %s)" child (oref child type))))

                    ;; List PR section's siblings
                    (shipit--debug-log "--- PR SECTION SIBLINGS ---")
                    (let ((parent (oref pr-section parent)))
                      (if parent
                          (let ((siblings (oref parent children)))
                            (shipit--debug-log "PR section parent has %d children (siblings):" (length siblings))
                            (dolist (sibling siblings)
                              (shipit--debug-log "  Sibling: %s (type: %s)" sibling (oref sibling type))))
                        (shipit--debug-log "PR section has no parent - it's at root level"))))
                (shipit--debug-log "--- NO PR SECTION FOUND ---"))))
          (shipit--debug-log "=== PR HIERARCHY DEBUG END ===")))
    (error (shipit--debug-log "HIERARCHY DEBUG ERROR: %s" err))))

(defun shipit-debug-dump-section-tree ()
  "Dump the magit section tree for the current buffer to the debug log.
Shows type, value, hidden state, start/end positions, and nesting depth."
  (interactive)
  (unless (bound-and-true-p magit-root-section)
    (user-error "No magit-root-section in this buffer"))
  (shipit--debug-log "=== SECTION TREE DUMP for %s ===" (buffer-name))
  (shipit--debug-dump-section magit-root-section 0)
  (shipit--debug-log "=== END SECTION TREE ===")
  (message "Section tree dumped to debug log. M-x shipit-view-debug-log to view."))

(defun shipit--debug-dump-section (section depth)
  "Dump SECTION and its children at indent DEPTH to the debug log."
  (let* ((type (oref section type))
         (value (oref section value))
         (hidden (oref section hidden))
         (start (oref section start))
         (end (oref section end))
         (content (oref section content))
         (children (oref section children))
         (washer (oref section washer))
         (indent (make-string (* depth 2) ?\s))
         (value-str (cond
                     ((null value) "nil")
                     ((stringp value) (truncate-string-to-width value 40))
                     ((numberp value) (number-to-string value))
                     (t (format "%S" value))))
         (heading-text (when (and start content (< start content))
                         (buffer-substring-no-properties
                          start (min content (+ start 80))))))
    (shipit--debug-log "%s%s val=%s hidden=%s washer=%s start=%s end=%s content=%s children=%d"
                       indent type value-str hidden (if washer "YES" "no")
                       start end content (length children))
    (when heading-text
      (shipit--debug-log "%s  heading: %s" indent (string-trim heading-text)))
    (dolist (child children)
      (shipit--debug-dump-section child (1+ depth)))))

(defcustom shipit-debug-profile-file
  "/tmp/shipit-debug-profile.txt"
  "Path written by `shipit-debug-profile' alongside the popup report."
  :type 'string :group 'shipit)

(defcustom shipit-debug-profile-mode 'cpu
  "Default profiler mode used by `shipit-debug-profile'.
One of `cpu', `mem', or `cpu+mem'.  CPU is the most useful for
diagnosing wall-clock slowness."
  :type '(choice (const cpu) (const mem) (const cpu+mem))
  :group 'shipit)

;;;###autoload
(defun shipit-debug-profile (seconds)
  "Profile Emacs for SECONDS, then pop AND save a fully-expanded report.
Mode is `shipit-debug-profile-mode' (default `cpu').  The report
buffer is also written to `shipit-debug-profile-file' so it can be
shared with someone debugging via tooling that can only read files
(e.g. an AI assistant).  Every node in the saved tree is expanded."
  (interactive (list (read-number "Profile for seconds: " 5)))
  (require 'profiler)
  (profiler-reset)
  (profiler-start shipit-debug-profile-mode)
  (let ((path shipit-debug-profile-file))
    (run-with-timer
     seconds nil
     (lambda ()
       (profiler-stop)
       (profiler-report)
       (condition-case err
           (let ((buf (cl-find-if
                       (lambda (b)
                         (string-match-p "Profiler-Report"
                                         (buffer-name b)))
                       (buffer-list))))
             (cond
              ((not buf)
               (message "Profile: no report buffer found"))
              (t
               (with-current-buffer buf
                 (goto-char (point-min))
                 (forward-line 1)
                 (let ((iters 0))
                   (while (and (< iters 50)
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward
                                  "^ *[0-9.,]+ +[0-9]+%[ ]*[+]" nil t)))
                     (cl-incf iters)
                     (goto-char (point-min))
                     (while (re-search-forward
                             "^ *[0-9.,]+ +[0-9]+%[ ]*[+]" nil t)
                       (ignore-errors (profiler-report-toggle-entry))
                       (forward-line 1))))
                 (goto-char (point-min))
                 (write-region (point-min) (point-max) path nil 'silent))
               (message "Profile complete (%ds): wrote %s (%d bytes) from %s"
                        seconds path
                        (or (file-attribute-size (file-attributes path)) -1)
                        (buffer-name buf)))))
         (error
          (message "Profile write failed: %s" (error-message-string err)))))))
  (message "Profiling for %d s — do the slow thing now…" seconds))

;;;###autoload
(defun shipit-debug-list-timers ()
  "List active timers whose payload references shipit.
Useful when most CPU samples land in `timer-event-handler' and you
need to know which timer is firing."
  (interactive)
  (require 'timer)
  (let* ((all (append timer-list timer-idle-list))
         (matches (cl-remove-if-not
                   (lambda (tm)
                     (let* ((fn (timer--function tm))
                            (name (cond
                                   ((symbolp fn) (symbol-name fn))
                                   ((byte-code-function-p fn) "<bytecode>")
                                   (t (format "%S" fn)))))
                       (and name (string-match-p "shipit" name))))
                   all)))
    (with-help-window "*shipit-timers*"
      (with-current-buffer "*shipit-timers*"
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Active shipit timers: %d (of %d total)\n\n"
                          (length matches) (length all)))
          (dolist (tm matches)
            (insert (format "  next=%s repeat=%s idle=%s fn=%S\n"
                            (format-time-string "%H:%M:%S"
                                                (timer--time tm))
                            (timer--repeat-delay tm)
                            (timer--idle-delay tm)
                            (timer--function tm)))))))))

(provide 'shipit-debug)
;;; shipit-debug.el ends here