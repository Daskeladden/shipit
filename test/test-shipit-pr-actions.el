;;; test-shipit-pr-actions.el --- Tests for PR action menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for PR header action menu and related operations.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-actions)
(require 'shipit-render)

;;; Test helpers

(defun test-pr-actions--make-minimal-pr-plist ()
  "Return a minimal valid PR backend plist with all required keys."
  (list :name "Test"
        :fetch-pr #'ignore
        :search #'ignore
        :create-pr #'ignore
        :merge-pr #'ignore
        :update-pr #'ignore
        :fetch-reviews #'ignore
        :submit-review #'ignore
        :fetch-review-decision #'ignore
        :fetch-files #'ignore
        :fetch-commits #'ignore
        :fetch-checks #'ignore
        :browse-url #'ignore))

(defmacro test-pr-actions--with-mock-backend (backend-ops &rest body)
  "Run BODY with a mock PR backend registered.
BACKEND-OPS is a plist of operation overrides merged onto the minimal backend."
  (declare (indent 1))
  `(let ((shipit-pr-backends nil)
         (shipit-pr-backend 'mock)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit-current-repo "owner/repo")
         (shipit--current-displayed-pr '(42 "owner/repo")))
     (let ((plist (test-pr-actions--make-minimal-pr-plist)))
       (plist-put plist :name "Mock")
       ,@(cl-loop for (key val) on backend-ops by #'cddr
                  collect `(plist-put plist ,key ,val))
       (shipit-pr-register-backend 'mock plist))
     ,@body))

;;; Tests — shipit--edit-pr-title dispatch

(ert-deftest test-shipit-edit-pr-title-dispatches-to-backend ()
  "GIVEN a mock PR backend with :update-pr
WHEN calling shipit--edit-pr-title
THEN backend :update-pr is called with title data."
  (let ((captured-args nil))
    (test-pr-actions--with-mock-backend
        (:update-pr
         (lambda (config number data)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :data data))
           '((title . "New Title"))))
      (cl-letf (((symbol-function 'shipit--clear-branch-cache) #'ignore)
                ((symbol-function 'shipit-buffer-refresh) #'ignore))
        (shipit--edit-pr-title 42 "New Title" "owner/repo")
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (equal "New Title"
                       (cdr (assq 'title (plist-get captured-args :data)))))))))

;;; Tests — action menu contents

(ert-deftest test-shipit-pr-header-actions-includes-edit-title-for-author ()
  "GIVEN the current user is the PR author
WHEN building the PR header action menu
THEN 'Edit title' is included in the actions list."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'get-text-property)
               (lambda (_pos prop)
                 (pcase prop
                   ('shipit-pr-number 42)
                   ('shipit-repo "owner/repo")
                   (_ nil))))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_num _repo)
                 '((user (login . "testuser"))
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (title . "Old Title"))))
              ((symbol-function 'shipit--get-pr-actual-state)
               (lambda (_data) "open"))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (shipit--pr-header-actions)
      (should (member "Edit title" captured-actions)))))

(ert-deftest test-shipit-pr-header-actions-excludes-edit-title-for-non-author ()
  "GIVEN the current user is NOT the PR author
WHEN building the PR header action menu
THEN 'Edit title' is NOT included in the actions list."
  (let ((captured-actions nil))
    (cl-letf (((symbol-function 'get-text-property)
               (lambda (_pos prop)
                 (pcase prop
                   ('shipit-pr-number 42)
                   ('shipit-repo "owner/repo")
                   (_ nil))))
              ((symbol-function 'shipit-get-pull-request)
               (lambda (_num _repo)
                 '((user (login . "otheruser"))
                   (html_url . "https://github.com/owner/repo/pull/42")
                   (title . "Old Title"))))
              ((symbol-function 'shipit--get-pr-actual-state)
               (lambda (_data) "open"))
              ((symbol-function 'shipit--get-current-user)
               (lambda () "testuser"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _args)
                 (setq captured-actions collection)
                 "Open on GitHub"))
              ((symbol-function 'browse-url) #'ignore))
      (shipit--pr-header-actions)
      (should-not (member "Edit title" captured-actions)))))

;;; Tests — generic URL overlays

(ert-deftest test-shipit-generic-url-overlay-created-for-plain-url ()
  "GIVEN a buffer containing a plain https URL
WHEN shipit--create-generic-url-overlays is called
THEN an overlay with keymap and face is created on the URL."
  (with-temp-buffer
    (insert "Check out https://example.com/some/page for details.\n")
    (shipit--create-generic-url-overlays (point-min) (point-max))
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (= 1 (length overlays)))
      (let ((ov (car overlays)))
        (should (eq 'markdown-plain-url-face (overlay-get ov 'face)))
        (should (overlay-get ov 'keymap))
        (should (overlay-get ov 'evaporate))
        ;; Verify overlay covers exactly the URL
        (should (string= "https://example.com/some/page"
                         (buffer-substring-no-properties
                          (overlay-start ov) (overlay-end ov))))
        ;; Verify both [return] and RET are bound (GUI vs terminal)
        (let ((km (overlay-get ov 'keymap)))
          (should (lookup-key km [return]))
          (should (lookup-key km (kbd "RET"))))))))

(ert-deftest test-shipit-generic-url-overlay-covers-link-text-for-markdown-link ()
  "GIVEN a buffer containing a markdown link [text](url)
WHEN shipit--create-generic-url-overlays is called
THEN the overlay covers the link text, not the URL."
  (with-temp-buffer
    (insert "See [my-project](https://github.com/owner/my-project) for info.\n")
    (shipit--create-generic-url-overlays (point-min) (point-max))
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (= 1 (length overlays)))
      (let ((ov (car overlays)))
        ;; Overlay should cover "my-project" (the link text), not the URL
        (should (string= "my-project"
                         (buffer-substring-no-properties
                          (overlay-start ov) (overlay-end ov))))
        (should (overlay-get ov 'keymap))
        (should (eq 'markdown-plain-url-face (overlay-get ov 'face)))))))

(ert-deftest test-shipit-generic-url-overlay-skips-existing-keymap-overlay ()
  "GIVEN a buffer where a URL already has an overlay with a keymap
WHEN shipit--create-generic-url-overlays is called
THEN no additional overlay is created for that URL."
  (with-temp-buffer
    (insert "See https://github.com/owner/repo/pull/42 for the PR.\n")
    ;; Simulate an existing overlay (from PR reference pass)
    (let ((existing-ov (make-overlay 5 46)))
      (overlay-put existing-ov 'keymap (make-sparse-keymap))
      (shipit--create-generic-url-overlays (point-min) (point-max))
      ;; Should still be just the one pre-existing overlay
      (should (= 1 (length (overlays-in (point-min) (point-max))))))))

(ert-deftest test-shipit-generic-url-overlay-skips-backtick-code ()
  "GIVEN a buffer with a URL inside backticks
WHEN shipit--create-generic-url-overlays is called
THEN no overlay is created for the URL inside code."
  (with-temp-buffer
    (insert "Use `https://example.com/api` for the endpoint.\n")
    (shipit--create-generic-url-overlays (point-min) (point-max))
    (should (= 0 (length (overlays-in (point-min) (point-max)))))))

;;; Tests — overlay action dispatch

(ert-deftest test-shipit-try-overlay-action-dispatches-highest-priority ()
  "GIVEN two overlays at point with different priorities
WHEN shipit--try-overlay-action-at-point is called
THEN the highest-priority overlay's RET binding is invoked."
  (with-temp-buffer
    (insert "some text here\n")
    (let ((called nil)
          (ov-low (make-overlay 1 10))
          (ov-high (make-overlay 1 10))
          (km-low (make-sparse-keymap))
          (km-high (make-sparse-keymap)))
      (define-key km-low (kbd "RET") (lambda () (interactive) (setq called 'low)))
      (define-key km-high (kbd "RET") (lambda () (interactive) (setq called 'high)))
      (overlay-put ov-low 'keymap km-low)
      (overlay-put ov-low 'priority 10)
      (overlay-put ov-high 'keymap km-high)
      (overlay-put ov-high 'priority 100)
      (goto-char 5)
      (should (shipit--try-overlay-action-at-point))
      (should (eq 'high called)))))

(ert-deftest test-shipit-try-overlay-action-returns-nil-without-overlay ()
  "GIVEN no overlays at point
WHEN shipit--try-overlay-action-at-point is called
THEN it returns nil."
  (with-temp-buffer
    (insert "some text here\n")
    (goto-char 5)
    (should-not (shipit--try-overlay-action-at-point))))

(provide 'test-shipit-pr-actions)
;;; test-shipit-pr-actions.el ends here
