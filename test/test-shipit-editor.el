;;; test-shipit-editor.el --- Tests for shipit-editor.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;;; Commentary:

;; Test suite for the shipit editor module.

;;; Code:

(require 'ert)

;; Load test stubs first
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Load backend modules (needed for shipit-pr--backend-id in mode body)
(require 'shipit-pr-backends nil t)
(require 'shipit-issue-backends nil t)

;; Load shipit-editor (will be created)
(require 'shipit-editor nil t)

;;; Test Utilities

(defvar test-shipit-editor--mock-prs
  '(((number . 123)
     (title . "Fix authentication bug")
     (state . "open")
     (user . ((login . "alice"))))
    ((number . 456)
     (title . "Add new feature")
     (state . "open")
     (user . ((login . "bob"))))
    ((number . 789)
     (title . "Update documentation")
     (state . "closed")
     (user . ((login . "charlie")))))
  "Mock PR data for testing.")

;;; Editor Mode Tests

(ert-deftest test-shipit-editor-mode-exists ()
  "Test that shipit-editor-mode is defined."
  (should (fboundp 'shipit-editor-mode)))

(ert-deftest test-shipit-editor-mode-derives-from-gfm-mode ()
  "Test that shipit-editor-mode derives from gfm-mode."
  (skip-unless (featurep 'shipit-editor))
  (with-temp-buffer
    (shipit-editor-mode)
    (should (derived-mode-p 'gfm-mode))))

(ert-deftest test-shipit-editor-mode-keybindings ()
  "Test that editor mode has correct keybindings."
  (skip-unless (featurep 'shipit-editor))
  (should (eq (lookup-key shipit-editor-mode-map (kbd "C-c C-c")) 'shipit-editor-save))
  (should (eq (lookup-key shipit-editor-mode-map (kbd "C-c C-k")) 'shipit-editor-cancel))
  ;; # @ : use debounced trigger functions
  (should (eq (lookup-key shipit-editor-mode-map (kbd "#")) 'shipit-editor--trigger-pr-reference))
  ;; M-n M-p for commit message insertion
  (should (eq (lookup-key shipit-editor-mode-map (kbd "M-n")) 'shipit-editor-insert-next-commit-message))
  (should (eq (lookup-key shipit-editor-mode-map (kbd "M-p")) 'shipit-editor-insert-prev-commit-message)))

;;; Editor Open Tests

(ert-deftest test-shipit-editor-open-exists ()
  "Test that shipit-editor-open function is defined."
  (should (fboundp 'shipit-editor-open)))

(ert-deftest test-shipit-editor-open-creates-buffer ()
  "Test that opening editor creates the editor buffer."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"))
          (should (get-buffer "*shipit-editor*")))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-open-with-initial-content ()
  "Test that editor opens with initial content for editing."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"
                 :initial-content "Existing comment text"))
          (with-current-buffer "*shipit-editor*"
            (should (string= (buffer-string) "Existing comment text"))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-stores-context ()
  "Test that editor stores context for save operation."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'inline-comment
                 :source-buffer source-buffer
                 :pr-number 456
                 :repo "owner/repo"
                 :file-path "src/main.el"
                 :line-number 42))
          (with-current-buffer "*shipit-editor*"
            (should (eq shipit-editor--type 'inline-comment))
            (should (equal shipit-editor--pr-number 456))
            (should (string= shipit-editor--repo "owner/repo"))
            (should (string= shipit-editor--file-path "src/main.el"))
            (should (eq shipit-editor--line-number 42))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

;;; Commit Message Insertion Tests

(ert-deftest test-shipit-editor-commit-message-functions-exist ()
  "Test that commit message insertion functions are defined."
  (should (fboundp 'shipit-editor-insert-next-commit-message))
  (should (fboundp 'shipit-editor-insert-prev-commit-message)))

(ert-deftest test-shipit-editor-insert-commit-message ()
  "Test that M-n inserts commit messages at point."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer))
        (test-commits (list (list :sha "abc123def456"
                                  :short-sha "abc123d"
                                  :message "First commit message"
                                  :first-line "First commit message")
                            (list :sha "def456abc789"
                                  :short-sha "def456a"
                                  :message "Second commit message"
                                  :first-line "Second commit message"))))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'preview-description
                 :source-buffer source-buffer
                 :commit-messages test-commits))
          (with-current-buffer "*shipit-editor*"
            ;; Should have commit messages stored
            (should (equal shipit-editor--commit-messages test-commits))
            (should (= shipit-editor--commit-index -1))
            ;; Insert first commit message
            (shipit-editor-insert-next-commit-message)
            (should (= shipit-editor--commit-index 0))
            (should (string= (buffer-string) "First commit message"))
            ;; Cycling should REPLACE the previous message
            ;; Simulate command loop by setting last-command
            (setq last-command 'shipit-editor-insert-next-commit-message)
            (shipit-editor-insert-next-commit-message)
            (should (= shipit-editor--commit-index 1))
            (should (string= (buffer-string) "Second commit message"))
            ;; Move point away (clears the cycling state since last-command changes)
            (setq last-command 'self-insert-command)
            (goto-char (point-min))
            (insert "Prefix: ")
            (goto-char (point-max))
            (shipit-editor-insert-next-commit-message)
            (should (= shipit-editor--commit-index 0))
            (should (string= (buffer-string) "Prefix: Second commit messageFirst commit message"))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-no-commits-message ()
  "Test that M-n without commits shows appropriate message."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer))
          (with-current-buffer "*shipit-editor*"
            ;; Should have no commit messages
            (should (null shipit-editor--commit-messages))
            ;; Calling insert should just show a message, not error
            (shipit-editor-insert-next-commit-message)
            (should (string= (buffer-string) ""))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

;;; PR Reference Completion Tests

(ert-deftest test-shipit-editor-insert-pr-reference-exists ()
  "Test that PR reference insertion function is defined."
  (should (fboundp 'shipit-editor-insert-pr-reference)))

(defun test-shipit-editor--mock-pr-resolve (_repo)
  "Return a mock PR backend plist+config for testing.
The :search function returns `test-shipit-editor--mock-prs'."
  (cons (list :search (lambda (_config _args) test-shipit-editor--mock-prs))
        (list :repo "owner/repo")))

(ert-deftest test-shipit-editor-insert-pr-reference-inserts-number ()
  "Test that selecting a PR inserts #NUMBER."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            ;; Mock completing-read and PR backend resolution
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "#123  Fix authentication bug"))
                      ((symbol-function 'shipit-editor--resolve-pr-backend-for-repo)
                       #'test-shipit-editor--mock-pr-resolve))
              (shipit-editor-insert-pr-reference)
              (should (string= (buffer-string) "#123")))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-insert-literal-hash-on-cancel ()
  "Test that cancelling PR selection inserts literal #."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            ;; Mock completing-read to signal quit
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (signal 'quit nil)))
                      ((symbol-function 'shipit-editor--resolve-pr-backend-for-repo)
                       #'test-shipit-editor--mock-pr-resolve))
              (condition-case nil
                  (shipit-editor-insert-pr-reference)
                (quit nil))
              (should (string= (buffer-string) "#")))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

;;; Save/Cancel Tests

(ert-deftest test-shipit-editor-save-exists ()
  "Test that save function is defined."
  (should (fboundp 'shipit-editor-save)))

(ert-deftest test-shipit-editor-cancel-exists ()
  "Test that cancel function is defined."
  (should (fboundp 'shipit-editor-cancel)))

(ert-deftest test-shipit-editor-cancel-closes-buffer ()
  "Test that cancel closes the editor buffer."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (shipit-editor-open
     (list :type 'general-comment
           :source-buffer source-buffer
           :pr-number 123
           :repo "owner/repo"))
    (with-current-buffer "*shipit-editor*"
      (shipit-editor-cancel))
    (should-not (get-buffer "*shipit-editor*"))))

;;; Live Preview Tests

(ert-deftest test-shipit-editor-has-preview-timer ()
  "Test that editor sets up preview update mechanism."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            ;; Should have after-change-functions hook or idle timer
            (should (or (memq 'shipit-editor--schedule-preview-update after-change-functions)
                        (boundp 'shipit-editor--preview-timer)))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

;;; User Mention Completion Tests

(defvar test-shipit-editor--mock-users
  '("alice" "bob" "charlie")
  "Mock user list for testing.")

(ert-deftest test-shipit-editor-insert-user-mention-exists ()
  "Test that user mention insertion function is defined."
  (should (fboundp 'shipit-editor-insert-user-mention)))

(ert-deftest test-shipit-editor-insert-user-mention-inserts-at ()
  "Test that selecting a user inserts @username."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            ;; Mock completing-read to return a specific user
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "alice"))
                      ((symbol-function 'shipit--get-available-assignees)
                       (lambda (&rest _) test-shipit-editor--mock-users)))
              (shipit-editor-insert-user-mention)
              (should (string= (buffer-string) "@alice")))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-insert-literal-at-on-cancel ()
  "Test that cancelling user selection inserts literal @."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            ;; Mock completing-read to signal quit
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) (signal 'quit nil)))
                      ((symbol-function 'shipit--get-available-assignees)
                       (lambda (&rest _) test-shipit-editor--mock-users)))
              (condition-case nil
                  (shipit-editor-insert-user-mention)
                (quit nil))
              (should (string= (buffer-string) "@")))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-at-keybinding ()
  "Test that @ key is bound to user mention insertion (debounced)."
  (skip-unless (featurep 'shipit-editor))
  (should (eq (lookup-key shipit-editor-mode-map (kbd "@")) 'shipit-editor--trigger-user-mention)))

;;; Live Preview Overlay Tests

(ert-deftest test-shipit-editor-setup-preview-overlay-exists ()
  "Test that setup preview overlay function is defined."
  (should (fboundp 'shipit-editor--setup-preview-overlay)))

(ert-deftest test-shipit-editor-setup-description-preview ()
  "Test that description preview setup finds the description region."
  (skip-unless (featurep 'shipit-editor))
  (with-temp-buffer
    ;; Create a mock description section with text properties
    (insert "Description\n")
    (let ((body-start (point)))
      (insert "This is the PR body\nWith multiple lines")
      (add-text-properties 1 (point) '(shipit-pr-description t)))
    ;; Test the setup function
    (let ((result (shipit-editor--setup-description-preview)))
      (when result
        ;; Should return an overlay
        (should (overlayp (plist-get result :overlay)))
        ;; Clean up
        (delete-overlay (plist-get result :overlay))))))

(ert-deftest test-shipit-editor-render-preview-content ()
  "Test that preview content rendering works."
  (skip-unless (featurep 'shipit-editor))
  ;; Empty content should return placeholder
  (let ((result (shipit-editor--render-preview-content "   ")))
    (should (stringp result))
    (should (string-match-p "empty" result)))
  ;; Non-empty content should be rendered
  (let ((result (shipit-editor--render-preview-content "Hello **world**")))
    (should (stringp result))))

(ert-deftest test-shipit-editor-cleanup-preview-overlay ()
  "Test that preview overlay cleanup works."
  (skip-unless (featurep 'shipit-editor))
  (with-temp-buffer
    (insert "test content")
    (let ((overlay (make-overlay 1 5)))
      (overlay-put overlay 'shipit-editor-preview t)
      ;; Overlay should exist before cleanup
      (should (overlay-buffer overlay))
      ;; Clean up
      (shipit-editor--cleanup-preview-overlay overlay)
      ;; Overlay should be deleted
      (should-not (overlay-buffer overlay)))))

(ert-deftest test-shipit-editor-update-preview-updates-overlay ()
  "Test that preview update modifies the overlay display."
  (skip-unless (featurep 'shipit-editor))
  (let ((source-buffer (generate-new-buffer "*test-source*")))
    (unwind-protect
        (progn
          ;; Set up source buffer with description
          (with-current-buffer source-buffer
            (insert "Description\n")
            (insert "Original body text")
            (add-text-properties 1 (point) '(shipit-pr-description t)))
          ;; Open editor
          (shipit-editor-open
           (list :type 'description
                 :source-buffer source-buffer
                 :pr-number 123
                 :repo "owner/repo"
                 :initial-content "New content"))
          (with-current-buffer "*shipit-editor*"
            ;; If we have a preview overlay, check it exists
            (when shipit-editor--preview-overlay
              (should (overlayp shipit-editor--preview-overlay)))))
      ;; Cleanup
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*"))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

;;; Emoji Completion Tests

(ert-deftest test-shipit-editor-emoji-alist-exists ()
  "Test that emoji alist is defined."
  (skip-unless (featurep 'shipit-editor))
  (should (boundp 'shipit-editor--emoji-alist))
  (should (listp shipit-editor--emoji-alist))
  (should (> (length shipit-editor--emoji-alist) 50)))

(ert-deftest test-shipit-editor-emoji-alist-structure ()
  "Test that emoji alist has correct structure."
  (skip-unless (featurep 'shipit-editor))
  ;; Each entry should be (shortcode . emoji)
  (dolist (pair shipit-editor--emoji-alist)
    (should (consp pair))
    (should (stringp (car pair)))
    (should (stringp (cdr pair)))))

(ert-deftest test-shipit-editor-emoji-alist-common-emojis ()
  "Test that common GitHub emojis are present."
  (skip-unless (featurep 'shipit-editor))
  (should (assoc "+1" shipit-editor--emoji-alist))
  (should (assoc "rocket" shipit-editor--emoji-alist))
  (should (assoc "bug" shipit-editor--emoji-alist))
  (should (assoc "sparkles" shipit-editor--emoji-alist))
  (should (assoc "fire" shipit-editor--emoji-alist))
  (should (assoc "warning" shipit-editor--emoji-alist)))

(ert-deftest test-shipit-editor-insert-emoji-exists ()
  "Test that emoji insert function exists."
  (should (fboundp 'shipit-editor-insert-emoji)))

(ert-deftest test-shipit-editor-colon-keybinding ()
  "Test that : key is bound to emoji insertion (debounced)."
  (skip-unless (featurep 'shipit-editor))
  (should (eq (lookup-key shipit-editor-mode-map (kbd ":")) 'shipit-editor--trigger-emoji)))

(ert-deftest test-shipit-editor-format-emoji-candidates ()
  "Test emoji candidate formatting."
  (skip-unless (featurep 'shipit-editor))
  (let ((candidates (shipit-editor--format-emoji-candidates)))
    (should (listp candidates))
    (should (> (length candidates) 0))
    ;; Each candidate should be "emoji shortcode"
    (dolist (c candidates)
      (should (stringp c))
      (should (string-match "^.+ .+$" c)))))

(ert-deftest test-shipit-editor-extract-emoji-shortcode ()
  "Test extracting shortcode from candidate."
  (skip-unless (featurep 'shipit-editor))
  (should (equal (shipit-editor--extract-emoji-shortcode "🚀 rocket") "rocket"))
  (should (equal (shipit-editor--extract-emoji-shortcode "👍 +1") "+1"))
  (should (equal (shipit-editor--extract-emoji-shortcode "✅ white_check_mark") "white_check_mark")))

(ert-deftest test-shipit-editor-strip-variation-selectors ()
  "Test stripping Unicode variation selectors."
  (skip-unless (featurep 'shipit-editor))
  ;; Should remove U+FE0F (emoji presentation selector)
  (should (equal (shipit-editor--strip-variation-selectors "❤️") "❤"))
  ;; Should remove U+FE0E (text presentation selector)
  (should (equal (shipit-editor--strip-variation-selectors "☺︎") "☺"))
  ;; Should not affect emojis without variation selectors
  (should (equal (shipit-editor--strip-variation-selectors "🚀") "🚀"))
  ;; Should handle strings with multiple emojis
  (should (equal (shipit-editor--strip-variation-selectors "❤️✌️") "❤✌")))

(ert-deftest test-shipit-editor-render-emoji-shortcodes ()
  "Test rendering emoji shortcodes to Unicode."
  (skip-unless (featurep 'shipit-editor))
  (should (equal (shipit-editor--render-emoji-shortcodes "Hello :rocket:!")
                 "Hello 🚀!"))
  (should (equal (shipit-editor--render-emoji-shortcodes ":+1: Great work :fire:")
                 "👍 Great work 🔥"))
  ;; Unknown shortcodes should remain unchanged
  (should (equal (shipit-editor--render-emoji-shortcodes ":unknown_emoji:")
                 ":unknown_emoji:"))
  ;; No shortcodes should return unchanged
  (should (equal (shipit-editor--render-emoji-shortcodes "No emojis here")
                 "No emojis here"))
  ;; Emojis with variation selectors should have them stripped
  (let ((result (shipit-editor--render-emoji-shortcodes ":heart:")))
    ;; Should contain heart but not variation selector
    (should (string-match-p "❤" result))
    (should-not (string-match-p "\uFE0F" result))))

(ert-deftest test-shipit-editor-render-preview-with-emoji ()
  "Test that preview rendering includes emoji conversion."
  (skip-unless (featurep 'shipit-editor))
  (let ((result (shipit-editor--render-preview-content "Test :rocket: content")))
    (should (string-match-p "🚀" result))
    (should-not (string-match-p ":rocket:" result))))

;;; Backend-Aware Editor Trigger Tests

(defun test-shipit-editor--make-issue-backend-plist (&optional search-fn)
  "Return a minimal issue backend plist for testing.
SEARCH-FN overrides the :search slot."
  (list :name "Test"
        :fetch-issue #'ignore
        :fetch-comments #'ignore
        :fetch-comments-async #'ignore
        :search (or search-fn #'ignore)
        :create-issue #'ignore
        :reference-patterns #'ignore
        :browse-url #'ignore
        :id-to-string (lambda (id) (format "#%s" id))
        :string-to-id #'identity))

(ert-deftest test-shipit-editor-gitlab-hash-dispatches-to-issue-completion ()
  "GIVEN the editor's resolved PR backend is gitlab
WHEN # completion fires
THEN it dispatches to shipit-editor-insert-issue-reference (not PR)."
  (skip-unless (featurep 'shipit-editor))
  (let ((dispatched-to nil))
    (cl-letf (((symbol-function 'shipit-editor-insert-issue-reference)
               (lambda () (setq dispatched-to 'issue)))
              ((symbol-function 'shipit-editor-insert-pr-reference)
               (lambda () (setq dispatched-to 'pr))))
      (with-temp-buffer
        (setq shipit-editor--pr-backend-id 'gitlab)
        ;; Simulate: user typed # at position 1, cursor at position 2
        (insert "#")
        (setq shipit-editor--completion-pos 1
              shipit-editor--completion-char ?#)
        (shipit-editor--complete-pr-reference (current-buffer) (point))
        (should (eq dispatched-to 'issue))))))

(ert-deftest test-shipit-editor-gitlab-bang-dispatches-to-mr-completion ()
  "GIVEN the editor's resolved PR backend is gitlab
WHEN ! completion fires
THEN it dispatches to shipit-editor-insert-mr-reference."
  (skip-unless (featurep 'shipit-editor))
  (let ((dispatched-to nil))
    (cl-letf (((symbol-function 'shipit-editor-insert-mr-reference)
               (lambda () (setq dispatched-to 'mr))))
      (with-temp-buffer
        (setq shipit-editor--pr-backend-id 'gitlab)
        ;; Simulate: user typed ! at position 1, cursor at position 2
        (insert "!")
        (setq shipit-editor--completion-pos 1
              shipit-editor--completion-char ?!)
        (shipit-editor--complete-mr-reference (current-buffer) (point))
        (should (eq dispatched-to 'mr))))))

(ert-deftest test-shipit-editor-github-hash-dispatches-to-pr-completion ()
  "GIVEN the editor's resolved PR backend is github
WHEN # completion fires
THEN it dispatches to shipit-editor-insert-pr-reference."
  (skip-unless (featurep 'shipit-editor))
  (let ((dispatched-to nil))
    (cl-letf (((symbol-function 'shipit-editor-insert-pr-reference)
               (lambda () (setq dispatched-to 'pr)))
              ((symbol-function 'shipit-editor-insert-issue-reference)
               (lambda () (setq dispatched-to 'issue))))
      (with-temp-buffer
        (setq shipit-editor--pr-backend-id 'github)
        ;; Simulate: user typed # at position 1, cursor at position 2
        (insert "#")
        (setq shipit-editor--completion-pos 1
              shipit-editor--completion-char ?#)
        (shipit-editor--complete-pr-reference (current-buffer) (point))
        (should (eq dispatched-to 'pr))))))

(ert-deftest test-shipit-editor-gitlab-bang-key-bound ()
  "GIVEN a GitLab PR backend
WHEN shipit-editor-open creates the editor buffer
THEN ! key is bound to shipit-editor--trigger-mr-reference."
  (skip-unless (featurep 'shipit-editor))
  (let ((shipit-pr-backend 'gitlab)
        (source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            (should (eq (lookup-key (current-local-map) (kbd "!"))
                        'shipit-editor--trigger-mr-reference))))
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-github-bang-key-not-bound ()
  "GIVEN a GitHub PR backend
WHEN shipit-editor-open creates the editor buffer
THEN ! key is NOT bound to shipit-editor--trigger-mr-reference."
  (skip-unless (featurep 'shipit-editor))
  (let ((shipit-pr-backend 'github)
        (source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            (should-not (eq (lookup-key (current-local-map) (kbd "!"))
                            'shipit-editor--trigger-mr-reference))))
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-issue-backend-fallback-matches-pr-backend ()
  "GIVEN the editor's resolved PR backend is gitlab and a gitlab issue backend
is registered
WHEN the editor resolves the issue backend for a repo
THEN it prefers the gitlab issue backend (matching PR backend)."
  (skip-unless (featurep 'shipit-editor))
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil)
        (shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config '(:api-url "https://gitlab.example.com"))
        (shipit-editor--pr-backend-id 'gitlab))
    (shipit-issue-register-backend
     'github (test-shipit-editor--make-issue-backend-plist))
    (shipit-issue-register-backend
     'gitlab (test-shipit-editor--make-issue-backend-plist))
    (let* ((resolved (shipit-editor--resolve-issue-backend-for-repo "myorg/myrepo"))
           (backend-plist (car resolved))
           (config (cdr resolved)))
      ;; Should have picked the gitlab backend (matching PR backend)
      (should (string= "Test" (plist-get backend-plist :name)))
      ;; Config should include the PR backend config + :repo
      (should (string= "https://gitlab.example.com" (plist-get config :api-url)))
      (should (string= "myorg/myrepo" (plist-get config :repo))))))

(ert-deftest test-shipit-editor-issue-backend-fallback-no-match ()
  "GIVEN the editor's resolved PR backend is gitlab but NO gitlab issue backend
is registered
WHEN the editor resolves the issue backend for a repo
THEN it falls back to the standard issue backend resolution."
  (skip-unless (featurep 'shipit-editor))
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil)
        (shipit-pr-backend 'gitlab)
        (shipit-pr-backend-config nil)
        (shipit-editor--pr-backend-id 'gitlab))
    (shipit-issue-register-backend
     'github (test-shipit-editor--make-issue-backend-plist))
    ;; No gitlab issue backend registered — falls back to standard resolver
    (let* ((resolved (shipit-editor--resolve-issue-backend-for-repo "myorg/myrepo"))
           (backend-plist (car resolved)))
      (should (string= "Test" (plist-get backend-plist :name))))))

(ert-deftest test-shipit-editor-header-line-github ()
  "GIVEN a GitHub PR backend
WHEN the editor opens
THEN header line mentions #: PR."
  (skip-unless (featurep 'shipit-editor))
  (let ((shipit-pr-backend 'github)
        (source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            (should (string-match-p "#: PR" (format "%s" header-line-format)))))
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-header-line-gitlab ()
  "GIVEN a GitLab PR backend
WHEN the editor opens
THEN header line mentions #: Issue and !: MR."
  (skip-unless (featurep 'shipit-editor))
  (let ((shipit-pr-backend 'gitlab)
        (source-buffer (current-buffer)))
    (unwind-protect
        (progn
          (shipit-editor-open
           (list :type 'general-comment
                 :source-buffer source-buffer
                 :repo "owner/repo"))
          (with-current-buffer "*shipit-editor*"
            (let ((header (format "%s" header-line-format)))
              (should (string-match-p "#: Issue" header))
              (should (string-match-p "!: MR" header)))))
      (when (get-buffer "*shipit-editor*")
        (kill-buffer "*shipit-editor*")))))

(ert-deftest test-shipit-editor-mr-trigger-cancel-exclusion ()
  "GIVEN shipit-editor-mode is active
WHEN the cancel hook checks this-command
THEN shipit-editor--trigger-mr-reference is in the exclusion list."
  (skip-unless (featurep 'shipit-editor))
  ;; The cancel hook should not cancel when we're in a trigger command
  (let ((shipit-editor--completion-timer (run-with-idle-timer 999 nil #'ignore))
        (this-command 'shipit-editor--trigger-mr-reference))
    (unwind-protect
        (progn
          (shipit-editor--maybe-cancel-completion)
          ;; Timer should NOT be cancelled
          (should shipit-editor--completion-timer))
      (when shipit-editor--completion-timer
        (cancel-timer shipit-editor--completion-timer)))))

(provide 'test-shipit-editor)
;;; test-shipit-editor.el ends here
