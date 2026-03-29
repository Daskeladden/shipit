;;; test-shipit-issue-render.el --- Tests for backend-aware reference overlays -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for backend reference overlay creation in shipit-render.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-render)

;;; Backend Reference Overlay Tests

(ert-deftest test-shipit-backend-reference-overlays-jira-pattern ()
  "GIVEN a Jira backend registered with project key PRJ
WHEN scanning text containing PRJ-123
THEN an overlay is created for the reference."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                        :project-keys ("PRJ")))
        (overlay-count 0)
        (found-count 0))
    ;; Register a Jira-like backend
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns (lambda (config)
                                 (let ((keys (plist-get config :project-keys)))
                                   (mapcar (lambda (k)
                                             (list (format "\\b\\(%s-[0-9]+\\)" (regexp-quote k))
                                                   1 #'identity))
                                           keys)))
           :browse-url (lambda (config id)
                         (format "%s/browse/%s" (plist-get config :base-url) id))
           :id-to-string (lambda (id) (format "%s" id))
           :string-to-id #'identity))
    (with-temp-buffer
      (insert "See PRJ-123 for the fix and PRJ-456 for the feature.\n")
      (shipit--create-backend-reference-overlays
       "owner/repo" (point-min) (point-max)
       (lambda () (setq found-count (1+ found-count)))
       (lambda () (setq overlay-count (1+ overlay-count))))
      (should (= 2 found-count))
      (should (= 2 overlay-count))
      ;; Check overlays exist at the right positions
      (let ((overlays (overlays-in (point-min) (point-max))))
        (should (= 2 (length overlays)))))))

(ert-deftest test-shipit-backend-reference-overlays-skips-github ()
  "GIVEN both GitHub and Jira backends registered
WHEN scanning text containing #42 and PRJ-123
THEN only PRJ-123 gets a backend overlay (GitHub #NNN handled by first pass)."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                        :project-keys ("PRJ")))
        (overlay-count 0))
    ;; Register GitHub
    (shipit-issue-register-backend
     'github
     (list :name "GitHub"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns (lambda (_config) '(("#\\([0-9]+\\)" 1 string-to-number)))
           :browse-url #'ignore
           :id-to-string #'ignore
           :string-to-id #'ignore))
    ;; Register Jira
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns (lambda (config)
                                 (let ((keys (plist-get config :project-keys)))
                                   (mapcar (lambda (k)
                                             (list (format "\\b\\(%s-[0-9]+\\)" (regexp-quote k))
                                                   1 #'identity))
                                           keys)))
           :browse-url (lambda (config id)
                         (format "%s/browse/%s" (plist-get config :base-url) id))
           :id-to-string (lambda (id) (format "%s" id))
           :string-to-id #'identity))
    (with-temp-buffer
      (insert "See #42 and PRJ-123 for details.\n")
      (shipit--create-backend-reference-overlays
       "owner/repo" (point-min) (point-max)
       #'ignore
       (lambda () (setq overlay-count (1+ overlay-count))))
      ;; Only PRJ-123 should get overlay, not #42
      (should (= 1 overlay-count)))))

(ert-deftest test-shipit-backend-reference-overlays-skips-code-blocks ()
  "GIVEN a Jira backend and text with PRJ-123 inside backticks
WHEN scanning text
THEN the reference inside code is skipped."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                        :project-keys ("PRJ")))
        (overlay-count 0))
    (shipit-issue-register-backend
     'jira
     (list :name "Jira"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns (lambda (config)
                                 (let ((keys (plist-get config :project-keys)))
                                   (mapcar (lambda (k)
                                             (list (format "\\b\\(%s-[0-9]+\\)" (regexp-quote k))
                                                   1 #'identity))
                                           keys)))
           :browse-url (lambda (config id)
                         (format "%s/browse/%s" (plist-get config :base-url) id))
           :id-to-string (lambda (id) (format "%s" id))
           :string-to-id #'identity))
    (with-temp-buffer
      (insert "Fix `PRJ-123` but also PRJ-456\n")
      (shipit--create-backend-reference-overlays
       "owner/repo" (point-min) (point-max)
       #'ignore
       (lambda () (setq overlay-count (1+ overlay-count))))
      ;; Only PRJ-456 gets overlay, PRJ-123 is in code
      (should (= 1 overlay-count)))))

(ert-deftest test-shipit-issue-reference-action-menu-browse ()
  "GIVEN an issue reference action menu
WHEN user presses 'b'
THEN browse-url is called with correct URL."
  (let ((opened-url nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?b))
              ((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (shipit--issue-reference-action-menu
       "PRJ-42" "owner/repo" "https://jira.example.com/browse/PRJ-42")
      (should (string= "https://jira.example.com/browse/PRJ-42" opened-url)))))

(ert-deftest test-shipit-issue-reference-action-menu-copy ()
  "GIVEN an issue reference action menu
WHEN user presses 'c'
THEN URL is copied to kill ring."
  (let ((copied nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?c))
              ((symbol-function 'kill-new)
               (lambda (str) (setq copied str))))
      (shipit--issue-reference-action-menu
       "PRJ-42" "owner/repo" "https://jira.example.com/browse/PRJ-42")
      (should (string= "https://jira.example.com/browse/PRJ-42" copied)))))

(ert-deftest test-shipit-issue-reference-action-menu-open ()
  "GIVEN an issue reference action menu
WHEN user presses RET
THEN shipit-issues-open-buffer is called."
  (let ((opened-id nil)
        (opened-repo nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_prompt _chars) ?\r))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (id repo &rest _) (setq opened-id id opened-repo repo))))
      (shipit--issue-reference-action-menu
       "PRJ-42" "owner/repo" "https://jira.example.com/browse/PRJ-42")
      (should (equal "PRJ-42" opened-id))
      (should (equal "owner/repo" opened-repo)))))

(provide 'test-shipit-issue-render)
;;; test-shipit-issue-render.el ends here
