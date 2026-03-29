;;; test-shipit-backend-parity.el --- Parity tests for backend registries -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies that all registered backends support the curated parity
;; operations defined in each backend registry module.  These tests
;; surface feature gaps by introspecting actual backend registrations.

;;; Code:

(require 'ert)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-pr-gitlab)
(require 'shipit-comment-backends)
(require 'shipit-comment-github)
(require 'shipit-comment-gitlab)
(require 'shipit-issue-backends)
(require 'shipit-issue-github)
(require 'shipit-issue-gitlab)

(ert-deftest test/pr-backend-parity ()
  "All PR backends must support parity operations."
  ;; GIVEN all registered PR backends and the parity operation list
  ;; WHEN checking each backend for every parity operation
  ;; THEN no gaps should exist
  (let ((gaps nil))
    (dolist (entry shipit-pr-backends)
      (let ((id (car entry))
            (plist (cdr entry)))
        (dolist (op shipit-pr--parity-operations)
          (unless (plist-get plist op)
            (push (format "%s missing %s" id op) gaps)))))
    (when gaps
      (ert-fail (format "PR parity gaps:\n  %s"
                        (mapconcat #'identity (nreverse gaps) "\n  "))))))

(ert-deftest test/comment-backend-parity ()
  "All comment backends must support parity operations."
  ;; GIVEN all registered comment backends and the parity operation list
  ;; WHEN checking each backend for every parity operation
  ;; THEN no gaps should exist
  (let ((gaps nil))
    (dolist (entry shipit-comment-backends)
      (let ((id (car entry))
            (plist (cdr entry)))
        (dolist (op shipit-comment--parity-operations)
          (unless (plist-get plist op)
            (push (format "%s missing %s" id op) gaps)))))
    (when gaps
      (ert-fail (format "Comment parity gaps:\n  %s"
                        (mapconcat #'identity (nreverse gaps) "\n  "))))))

(ert-deftest test/issue-backend-parity ()
  "All issue backends must support parity operations."
  ;; GIVEN all registered issue backends and the parity operation list
  ;; WHEN checking each backend for every parity operation
  ;; THEN no gaps should exist
  (let ((gaps nil))
    (dolist (entry shipit-issue-backends)
      (let ((id (car entry))
            (plist (cdr entry)))
        (unless (plist-get plist :notification-only)
          (dolist (op shipit-issue--parity-operations)
            (unless (plist-get plist op)
              (push (format "%s missing %s" id op) gaps))))))
    (when gaps
      (ert-fail (format "Issue parity gaps:\n  %s"
                        (mapconcat #'identity (nreverse gaps) "\n  "))))))

(provide 'test-shipit-backend-parity)
;;; test-shipit-backend-parity.el ends here
