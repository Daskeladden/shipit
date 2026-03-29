;;; test-shipit-issue-create-buffer.el --- Tests for issue creation buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the rich issue creation buffer UI.

;;; Code:

(require 'ert)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-jira)
(require 'shipit-issues)

;; Forward declare — loaded below
(declare-function shipit-issue-create-buffer "shipit-issue-create")
(declare-function shipit-issue-create--build-fields-alist "shipit-issue-create")
(declare-function shipit-issue-create--submit "shipit-issue-create")
(declare-function shipit-issue-create--refresh "shipit-issue-create")
(declare-function shipit-issue-create--save-draft "shipit-issue-create")
(declare-function shipit-issue-create--restore-draft "shipit-issue-create")
(declare-function shipit-issue-create--clear-draft "shipit-issue-create")

(require 'shipit-issue-create)

;;; Helper to create a test buffer

(defun test-shipit-issue-create--setup-buffer (backend-id config)
  "Set up a creation buffer for BACKEND-ID with CONFIG for testing."
  (let ((shipit-issue-backend backend-id)
        (shipit-issue-backend-config config)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer))))

;;; Buffer Mode Tests

(ert-deftest test-shipit-issue-create-buffer-opens-in-mode ()
  "GIVEN a github backend
WHEN opening the issue creation buffer
THEN the buffer is in shipit-issue-create-mode."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (should (eq major-mode 'shipit-issue-create-mode))
        (kill-buffer (current-buffer))))))

(ert-deftest test-shipit-issue-create-buffer-has-title-property ()
  "GIVEN a github backend
WHEN opening the issue creation buffer
THEN the buffer contains text with shipit-issue-create-title property."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (should (text-property-any (point-min) (point-max)
                                     'shipit-issue-create-title t))
        (kill-buffer (current-buffer))))))

(ert-deftest test-shipit-issue-create-buffer-has-description-property ()
  "GIVEN a github backend
WHEN opening the issue creation buffer
THEN the buffer contains text with shipit-issue-create-description property."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (should (text-property-any (point-min) (point-max)
                                     'shipit-issue-create-description t))
        (kill-buffer (current-buffer))))))

(ert-deftest test-shipit-issue-create-buffer-jira-shows-issue-type ()
  "GIVEN the jira backend
WHEN opening the issue creation buffer
THEN the buffer contains an issue-type field."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                       :project-keys ("PRJ")))
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (let ((pos (text-property-any (point-min) (point-max)
                                        'shipit-issue-create-field 'issue-type)))
            (should pos))
        (kill-buffer (current-buffer))))))

(ert-deftest test-shipit-issue-create-buffer-jira-shows-components ()
  "GIVEN the jira backend
WHEN opening the issue creation buffer
THEN the buffer contains a components field."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                       :project-keys ("PRJ")))
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (let ((pos (text-property-any (point-min) (point-max)
                                        'shipit-issue-create-field 'components)))
            (should pos))
        (kill-buffer (current-buffer))))))

(ert-deftest test-shipit-issue-create-buffer-github-no-issue-type ()
  "GIVEN the github backend
WHEN opening the issue creation buffer
THEN the buffer does NOT contain an issue-type field."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (let ((pos (text-property-any (point-min) (point-max)
                                        'shipit-issue-create-field 'issue-type)))
            (should-not pos))
        (kill-buffer (current-buffer))))))

(ert-deftest test-shipit-issue-create-buffer-github-no-components ()
  "GIVEN the github backend
WHEN opening the issue creation buffer
THEN the buffer does NOT contain a components field."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (let ((pos (text-property-any (point-min) (point-max)
                                        'shipit-issue-create-field 'components)))
            (should-not pos))
        (kill-buffer (current-buffer))))))

;;; Submit Validation Tests

(ert-deftest test-shipit-issue-create-submit-validates-empty-title ()
  "GIVEN a creation buffer with no title
WHEN calling submit
THEN an error is signaled."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      ;; Clear any draft state leaked from other tests
      (clrhash shipit-issue-create--drafts)
      (shipit-issue-create-buffer)
      (unwind-protect
          (progn
            (setq-local shipit-issue-create--title nil)
            (should-error (shipit-issue-create--submit) :type 'user-error))
        (progn
          (remove-hook 'kill-buffer-hook #'shipit-issue-create--save-draft t)
          (kill-buffer (current-buffer)))))))

;;; Build Fields Alist Tests

(ert-deftest test-shipit-issue-create-build-fields-alist ()
  "GIVEN a creation buffer with title, body, labels set
WHEN calling build-fields-alist
THEN the alist includes all set fields."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (progn
            (setq-local shipit-issue-create--title "My Title")
            (setq-local shipit-issue-create--body "My Body")
            (setq-local shipit-issue-create--labels '("bug" "urgent"))
            (let ((alist (shipit-issue-create--build-fields-alist)))
              (should (equal "My Title" (cdr (assq 'title alist))))
              (should (equal "My Body" (cdr (assq 'body alist))))
              (should (equal '("bug" "urgent") (cdr (assq 'labels alist))))))
        (kill-buffer (current-buffer))))))

;;; State Persistence Tests

(ert-deftest test-shipit-issue-create-state-save-restore ()
  "GIVEN a creation buffer with state
WHEN saving then restoring draft
THEN the state round-trips correctly."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      ;; Create buffer and set state
      (shipit-issue-create-buffer)
      (setq-local shipit-issue-create--title "Draft Title")
      (setq-local shipit-issue-create--body "Draft Body")
      (setq-local shipit-issue-create--labels '("draft-label"))
      (shipit-issue-create--save-draft)
      (kill-buffer (current-buffer))
      ;; Re-create buffer and check state was restored
      (shipit-issue-create-buffer)
      (unwind-protect
          (progn
            (should (equal "Draft Title" shipit-issue-create--title))
            (should (equal "Draft Body" shipit-issue-create--body))
            (should (equal '("draft-label") shipit-issue-create--labels)))
        (progn
          (shipit-issue-create--clear-draft)
          (remove-hook 'kill-buffer-hook #'shipit-issue-create--save-draft t)
          (kill-buffer (current-buffer)))))))

(ert-deftest test-shipit-issue-create-state-cleared-after-submit ()
  "GIVEN a creation buffer with saved state
WHEN submit succeeds
THEN the saved state is cleared."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-issues--create-issue-extended)
               (lambda (_repo _fields)
                 '((number . 42) (title . "T"))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (_number _repo) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t)))
      (shipit-issue-create-buffer)
      (setq-local shipit-issue-create--title "Submit Title")
      (shipit-issue-create--save-draft)
      (shipit-issue-create--submit)
      ;; Re-create buffer and check state was NOT restored
      (shipit-issue-create-buffer)
      (unwind-protect
          (should (null shipit-issue-create--title))
        (kill-buffer (current-buffer))))))

;;; Full Render Path Tests

(ert-deftest test-shipit-issue-create-full-render-github ()
  "GIVEN the github backend
WHEN rendering the full creation buffer
THEN no errors are signaled."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (progn
            ;; Just verify the buffer has content
            (should (> (buffer-size) 0))
            ;; Verify key sections exist
            (should (text-property-any (point-min) (point-max)
                                       'shipit-issue-create-title t))
            (should (text-property-any (point-min) (point-max)
                                       'shipit-issue-create-description t)))
        (kill-buffer (current-buffer))))))

(ert-deftest test-shipit-issue-create-full-render-jira ()
  "GIVEN the jira backend
WHEN rendering the full creation buffer
THEN no errors are signaled."
  (let ((shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                       :project-keys ("PRJ")))
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo")))
      (shipit-issue-create-buffer)
      (unwind-protect
          (progn
            (should (> (buffer-size) 0))
            (should (text-property-any (point-min) (point-max)
                                       'shipit-issue-create-title t))
            (should (text-property-any (point-min) (point-max)
                                       'shipit-issue-create-description t))
            (should (text-property-any (point-min) (point-max)
                                       'shipit-issue-create-field 'issue-type)))
        (kill-buffer (current-buffer))))))

;;; Submit Safety Tests

(ert-deftest test-shipit-issue-create-submit-keeps-buffer-on-failure ()
  "GIVEN a creation buffer with a title
WHEN submit fails (backend signals error)
THEN the buffer is NOT killed and the draft is NOT cleared."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-issues--create-issue-extended)
               (lambda (_repo _fields)
                 (error "Jira API POST failed (400): bad request")))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t)))
      (shipit-issue-create-buffer)
      (setq-local shipit-issue-create--title "Failing Issue")
      (shipit-issue-create--submit)
      ;; Buffer should still be alive
      (should (buffer-live-p (current-buffer)))
      ;; Title should still be set
      (should (equal "Failing Issue" shipit-issue-create--title))
      ;; Clean up
      (remove-hook 'kill-buffer-hook #'shipit-issue-create--save-draft t)
      (kill-buffer (current-buffer)))))

(ert-deftest test-shipit-issue-create-submit-keeps-buffer-on-nil-result ()
  "GIVEN a creation buffer with a title
WHEN submit returns nil (no issue number)
THEN the buffer is NOT killed and the draft is NOT cleared."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-issues--create-issue-extended)
               (lambda (_repo _fields) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t)))
      (shipit-issue-create-buffer)
      (setq-local shipit-issue-create--title "Nil Issue")
      (shipit-issue-create--submit)
      ;; Buffer should still be alive
      (should (buffer-live-p (current-buffer)))
      ;; Title should still be set
      (should (equal "Nil Issue" shipit-issue-create--title))
      ;; Clean up
      (remove-hook 'kill-buffer-hook #'shipit-issue-create--save-draft t)
      (kill-buffer (current-buffer)))))

(ert-deftest test-shipit-issue-create-submit-kills-buffer-on-success ()
  "GIVEN a creation buffer with a title
WHEN submit succeeds (returns issue with number)
THEN the draft is cleared and the buffer is killed."
  (let ((shipit-issue-backend 'github)
        (shipit-issue-backend-config nil)
        (shipit-issue-repo-backends nil)
        (test-buffer nil))
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "owner/repo"))
              ((symbol-function 'shipit-issues--create-issue-extended)
               (lambda (_repo _fields)
                 '((number . 42) (title . "Success"))))
              ((symbol-function 'shipit-issues-open-buffer)
               (lambda (_number _repo) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t)))
      (shipit-issue-create-buffer)
      (setq test-buffer (current-buffer))
      (setq-local shipit-issue-create--title "Success Issue")
      (shipit-issue-create--save-draft)
      (shipit-issue-create--submit)
      ;; Buffer should be killed
      (should-not (buffer-live-p test-buffer)))))

(provide 'test-shipit-issue-create-buffer)
;;; test-shipit-issue-create-buffer.el ends here
