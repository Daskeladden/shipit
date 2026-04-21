;;; test-shipit-issue-eldoc.el --- Tests for issue reference eldoc -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for eldoc-based description display when cursor is on an
;; issue reference overlay.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issue-titles)
(require 'shipit-render)
(require 'shipit-code-refs)

(defun test-shipit-eldoc--register-jira-backend (fetch-fn)
  "Register a Jira-like backend with FETCH-FN as :fetch-issue."
  (shipit-issue-register-backend
   'jira
   (list :name "Jira"
         :fetch-issue fetch-fn
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
         :string-to-id #'identity)))

(ert-deftest test-shipit-issue-ref-overlay-stashes-id-and-repo ()
  "GIVEN text with a Jira reference
WHEN backend reference overlays are created
THEN the overlay carries shipit-issue-id and shipit-issue-repo properties."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                                 :project-keys ("PRJ"))))
    (test-shipit-eldoc--register-jira-backend #'ignore)
    (with-temp-buffer
      (insert "Please check PRJ-123 soon.\n")
      (shipit--create-backend-reference-overlays
       "owner/repo" (point-min) (point-max) #'ignore #'ignore)
      (goto-char (point-min))
      (re-search-forward "PRJ-123")
      (goto-char (match-beginning 0))
      (let ((ov (car (overlays-at (point)))))
        (should ov)
        (should (equal "PRJ-123" (overlay-get ov 'shipit-issue-id)))
        (should (equal "owner/repo" (overlay-get ov 'shipit-issue-repo)))))))

(ert-deftest test-shipit-pr-ref-overlay-stashes-id-and-repo ()
  "GIVEN text with a GitHub-style #NNN reference
WHEN PR reference overlays are created
THEN the overlay carries shipit-issue-id (integer) and shipit-issue-repo."
  (let ((shipit-issue-backends nil)
        (shipit-issue-backend 'github)
        (shipit-issue-backend-config nil))
    (shipit-issue-register-backend
     'github
     (list :name "GitHub"
           :fetch-issue #'ignore
           :fetch-comments #'ignore
           :fetch-comments-async #'ignore
           :search #'ignore
           :create-issue #'ignore
           :reference-patterns (lambda (_config) '(("#\\([0-9]+\\)" 1 string-to-number)))
           :browse-url (lambda (_c id) (format "https://github.com/owner/repo/issues/%s" id))
           :id-to-string (lambda (id) (format "#%s" id))
           :string-to-id #'string-to-number))
    (cl-letf (((symbol-function 'shipit-pr--create-reference-overlays)
               (lambda (&rest _args) nil)))
      (with-temp-buffer
        (insert "See #42 for details.\n")
        (shipit--create-pr-reference-overlays "owner/repo" 1 (point-min) (point-max))
        (goto-char (point-min))
        (re-search-forward "#42")
        (goto-char (match-beginning 0))
        (let ((ov (car (overlays-at (point)))))
          (should ov)
          (should (equal 42 (overlay-get ov 'shipit-issue-id)))
          (should (equal "owner/repo" (overlay-get ov 'shipit-issue-repo))))))))

(ert-deftest test-shipit-issue-ref-eldoc-returns-nil-when-not-on-overlay ()
  "GIVEN point is not on a reference overlay
WHEN shipit--issue-ref-eldoc-function is invoked
THEN it returns nil."
  (with-temp-buffer
    (insert "No references here.\n")
    (goto-char (point-min))
    (should (null (shipit--issue-ref-eldoc-function #'ignore)))))

(ert-deftest test-shipit-issue-ref-eldoc-returns-cached-title ()
  "GIVEN cache contains a fresh entry for the reference at point
WHEN shipit--issue-ref-eldoc-function runs
THEN it returns a formatted string including the title."
  (let ((shipit--issue-title-cache (make-hash-table :test 'equal)))
    (puthash '("owner/repo" . "PRJ-7")
             '(:status fresh :title "Widget is broken" :state "In Progress"
                       :assignee "alice")
             shipit--issue-title-cache)
    (with-temp-buffer
      (insert "See PRJ-7.\n")
      (let ((ov (make-overlay 5 10)))
        (overlay-put ov 'shipit-issue-id "PRJ-7")
        (overlay-put ov 'shipit-issue-repo "owner/repo"))
      (goto-char 5)
      (let ((result (shipit--issue-ref-eldoc-function #'ignore)))
        (should (stringp result))
        (should (string-match-p "Widget is broken" result))
        (should (string-match-p "In Progress" result))))))

(ert-deftest test-shipit-issue-ref-eldoc-cache-miss-triggers-fetch ()
  "GIVEN no cache entry for the reference at point
WHEN shipit--issue-ref-eldoc-function runs
THEN it kicks off a fetch and marks the key as loading."
  (let ((shipit--issue-title-cache (make-hash-table :test 'equal))
        (fetch-args nil))
    (cl-letf (((symbol-function 'shipit--schedule-issue-title-fetch)
               (lambda (repo id _cb) (setq fetch-args (list repo id)))))
      (with-temp-buffer
        (insert "See PRJ-9.\n")
        (let ((ov (make-overlay 5 10)))
          (overlay-put ov 'shipit-issue-id "PRJ-9")
          (overlay-put ov 'shipit-issue-repo "owner/repo"))
        (goto-char 5)
        (let ((result (shipit--issue-ref-eldoc-function #'ignore)))
          (should (stringp result))
          (should (string-match-p "Loading" result))
          (should (equal '("owner/repo" "PRJ-9") fetch-args))
          ;; Cache now marked as loading
          (let ((entry (gethash '("owner/repo" . "PRJ-9") shipit--issue-title-cache)))
            (should (eq 'loading (plist-get entry :status)))))))))

(ert-deftest test-shipit-issue-ref-eldoc-loading-state-not-refetched ()
  "GIVEN cache already marks the key as loading
WHEN shipit--issue-ref-eldoc-function runs
THEN it returns Loading… without triggering another fetch."
  (let ((shipit--issue-title-cache (make-hash-table :test 'equal))
        (fetch-calls 0))
    (puthash '("owner/repo" . "PRJ-9") '(:status loading) shipit--issue-title-cache)
    (cl-letf (((symbol-function 'shipit--schedule-issue-title-fetch)
               (lambda (&rest _) (cl-incf fetch-calls))))
      (with-temp-buffer
        (insert "See PRJ-9.\n")
        (let ((ov (make-overlay 5 10)))
          (overlay-put ov 'shipit-issue-id "PRJ-9")
          (overlay-put ov 'shipit-issue-repo "owner/repo"))
        (goto-char 5)
        (let ((result (shipit--issue-ref-eldoc-function #'ignore)))
          (should (string-match-p "Loading" result))
          (should (= 0 fetch-calls)))))))

(ert-deftest test-shipit-format-issue-eldoc-short-form ()
  "GIVEN an issue cache entry with title, state, and assignee
WHEN shipit--format-issue-eldoc is called
THEN the result includes the id, state, and title on one line."
  (let ((s (shipit--format-issue-eldoc
            "PRJ-42"
            '(:status fresh :title "Fix login bug" :state "Open" :assignee "alice"))))
    (should (string-match-p "PRJ-42" s))
    (should (string-match-p "Open" s))
    (should (string-match-p "Fix login bug" s))
    (should (string-match-p "alice" s))))

(ert-deftest test-shipit-schedule-issue-title-fetch-populates-cache ()
  "GIVEN a backend whose :fetch-issue returns an issue alist
WHEN shipit--schedule-issue-title-fetch fires the deferred fetch
THEN the cache is updated with title/state/assignee and callback invoked."
  (let ((shipit--issue-title-cache (make-hash-table :test 'equal))
        (shipit-issue-backends nil)
        (shipit-issue-backend 'jira)
        (shipit-issue-backend-config '(:base-url "https://jira.example.com"
                                                 :project-keys ("PRJ"))))
    (test-shipit-eldoc--register-jira-backend
     (lambda (_cfg id)
       `((title . "Fix widget")
         (state . "Open")
         (user . ((login . "alice")))
         (number . ,id))))
    ;; run-at-time should run synchronously for the test to be deterministic
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args) (apply fn args))))
      (let ((cb-invoked nil))
        (shipit--schedule-issue-title-fetch
         "owner/repo" "PRJ-42"
         (lambda () (setq cb-invoked t)))
        (let ((entry (gethash '("owner/repo" . "PRJ-42") shipit--issue-title-cache)))
          (should entry)
          (should (eq 'fresh (plist-get entry :status)))
          (should (equal "Fix widget" (plist-get entry :title)))
          (should (equal "Open" (plist-get entry :state)))
          (should cb-invoked))))))

;;; shipit-code-refs eldoc (text-property path)

(ert-deftest test-shipit-code-refs-eldoc-nil-when-no-property ()
  "GIVEN point is not on a shipit-issue-ref text property
WHEN shipit-code-refs--eldoc-function is called
THEN it returns nil."
  (with-temp-buffer
    (insert "plain text with no ref\n")
    (goto-char 5)
    (should (null (shipit-code-refs--eldoc-function #'ignore)))))

(ert-deftest test-shipit-code-refs-eldoc-returns-cached-title ()
  "GIVEN cache has a fresh entry for the key at point
WHEN shipit-code-refs--eldoc-function is called
THEN it returns a formatted string with the title."
  (let ((shipit--issue-title-cache (make-hash-table :test 'equal)))
    (puthash '("acme/repo" . "ZIVID-100")
             '(:status fresh :title "Camera drops frames"
                       :state "In Progress" :assignee "bob")
             shipit--issue-title-cache)
    (cl-letf (((symbol-function 'shipit-code-refs--current-repo)
               (lambda () "acme/repo")))
      (with-temp-buffer
        (insert (propertize "ZIVID-100" 'shipit-issue-ref "ZIVID-100")
                " is broken\n")
        (goto-char 3)
        (let ((result (shipit-code-refs--eldoc-function #'ignore)))
          (should (stringp result))
          (should (string-match-p "ZIVID-100" result))
          (should (string-match-p "Camera drops frames" result))
          (should (string-match-p "In Progress" result)))))))

(ert-deftest test-shipit-code-refs-eldoc-cache-miss-triggers-fetch ()
  "GIVEN cache has no entry for the key at point
WHEN shipit-code-refs--eldoc-function runs
THEN it schedules a fetch and marks the entry as loading."
  (let ((shipit--issue-title-cache (make-hash-table :test 'equal))
        (fetch-args nil))
    (cl-letf (((symbol-function 'shipit-code-refs--current-repo)
               (lambda () "acme/repo"))
              ((symbol-function 'shipit--schedule-issue-title-fetch)
               (lambda (repo id _cb) (setq fetch-args (list repo id)))))
      (with-temp-buffer
        (insert (propertize "ZIVID-7" 'shipit-issue-ref "ZIVID-7")
                "\n")
        (goto-char 2)
        (let ((result (shipit-code-refs--eldoc-function #'ignore)))
          (should (string-match-p "Loading" result))
          (should (equal '("acme/repo" "ZIVID-7") fetch-args))
          (let ((entry (gethash '("acme/repo" . "ZIVID-7")
                                shipit--issue-title-cache)))
            (should (eq 'loading (plist-get entry :status)))))))))

(ert-deftest test-shipit-code-refs-eldoc-no-repo ()
  "GIVEN shipit-code-refs--current-repo returns nil
WHEN shipit-code-refs--eldoc-function runs
THEN it returns nil (no fetch attempted)."
  (cl-letf (((symbol-function 'shipit-code-refs--current-repo)
             (lambda () nil)))
    (with-temp-buffer
      (insert (propertize "PRJ-1" 'shipit-issue-ref "PRJ-1") "\n")
      (goto-char 2)
      (should (null (shipit-code-refs--eldoc-function #'ignore))))))

(provide 'test-shipit-issue-eldoc)
;;; test-shipit-issue-eldoc.el ends here
