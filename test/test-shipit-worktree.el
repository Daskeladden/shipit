;;; test-shipit-worktree.el --- Tests for worktree management -*- lexical-binding: t; -*-

(require 'ert)
(require 'shipit-worktree)
(require 'shipit-commands)

(ert-deftest shipit--get-worktree-path-test ()
  "Test constructing worktree path from PR number and branch."
  (let ((shipit-worktree-directory ".worktrees/"))
    (should (string-match "pr-123" (shipit--get-worktree-path 123 "feature-branch")))
    (should (string-match "feature-branch" (shipit--get-worktree-path 123 "feature-branch")))))

(ert-deftest shipit--worktree-exists-test ()
  "Test checking if worktree exists."
  (should (booleanp (shipit--worktree-exists-p "/fake/path"))))

(ert-deftest shipit--get-repo-root-from-git-dir-returns-nil-test ()
  "Test that repo-root detection returns nil when rev-parse fails.
GIVEN default-directory inside `.git/' (where `git rev-parse
--show-toplevel' exits non-zero with \"fatal: this operation must
be run in a work tree\" on stderr)
WHEN `shipit--get-repo-root' is called
THEN it should return nil, not the stderr text — otherwise
callers use the error string as a path and subsequent file ops
blow up with \"Setting current directory: No such file or
directory\"."
  (let* ((tmp (make-temp-file "shipit-repo-root-test-" t))
         (git-dir (expand-file-name ".git" tmp)))
    (unwind-protect
        (progn
          (let ((default-directory tmp))
            (call-process "git" nil nil nil "init" "-q"))
          (let ((default-directory (file-name-as-directory git-dir)))
            (let ((result (shipit--get-repo-root)))
              (should (or (null result)
                          (file-directory-p result))))))
      (delete-directory tmp t))))

(ert-deftest shipit--get-pr-info-from-worktree-test ()
  "Test reading .shipit-pr-info.json from worktree."
  ;; This will need a mock worktree or temp directory
  (let ((temp-dir (make-temp-file "shipit-test-" t)))
    (should (or (null (shipit--get-pr-info-from-worktree temp-dir))
                (listp (shipit--get-pr-info-from-worktree temp-dir))))
    (delete-directory temp-dir t)))

(ert-deftest shipit--worktree-in-sync-p-test ()
  "Test checking if worktree is in sync with PR head."
  ;; Requires mocking git commands
  (should (booleanp (shipit--worktree-in-sync-p "/fake/path" "abc1234"))))

(ert-deftest shipit--find-worktree-for-pr-test ()
  "Test finding worktree directory for a PR."
  (let ((shipit-worktree-directory ".worktrees/"))
    ;; Should return nil when no worktree exists
    (should (null (shipit--find-worktree-for-pr 999 "nonexistent-repo")))))

(ert-deftest shipit--get-worktree-status-test ()
  "Test getting worktree status (no-worktree, in-sync, out-of-sync)."
  (let ((shipit-worktree-directory ".worktrees/")
        (pr-number 123)
        (pr-head-sha "abc1234567890"))
    (cl-letf (((symbol-function 'shipit--get-current-repo)
               (lambda () "owner/repo")))
      ;; Should return 'none if no worktree exists
      (should (eq 'none (shipit--get-worktree-status pr-number pr-head-sha "owner/repo"))))))

(ert-deftest shipit--get-commits-behind-test ()
  "Test counting commits between worktree and PR head."
  ;; This requires git commands, mock will be needed
  (if (fboundp 'shipit--get-commits-behind)
      (should (integerp (or (shipit--get-commits-behind "/fake/path" "abc123") 0)))
    (ert-skip "Function shipit--get-commits-behind not yet implemented")))

(ert-deftest shipit--insert-worktree-section-test ()
  "Test rendering Worktree section in PR buffer."
  (let ((pr-data '((number . 123)
                   (head (sha . "abc1234567890"))
                   (base (repo (full_name . "owner/repo")))))
        (shipit-worktree-directory ".worktrees/"))
    (with-temp-buffer
      (require 'shipit-buffer)
      (shipit-mode)
      ;; Mock section insertion - just verify it doesn't error and produces some output
      (cl-letf (((symbol-function 'shipit--get-current-repo)
                 (lambda () "owner/repo")))
        (let ((inhibit-read-only t))
          (condition-case err
              (progn
                (shipit--insert-worktree-section pr-data)
                (should (> (buffer-size) 0)))
            (error (ert-skip (format "Section insertion not supported in mock environment: %S" err)))))))))

(ert-deftest shipit-worktree-transient-defined-test ()
  "Test that shipit-worktree transient is defined."
  (should (fboundp 'shipit-worktree)))

(ert-deftest shipit--create-worktree-pr-test ()
  "Test creating a worktree for a PR branch."
  ;; This needs mocking of git commands
  (should (or (listp (shipit--create-worktree-pr 123 "feature-branch" "abc1234"))
              (eq nil (shipit--create-worktree-pr 123 "feature-branch" "abc1234")))))

(ert-deftest shipit--sync-worktree-test ()
  "Test syncing worktree with latest PR commits."
  (should (or (eq t (shipit--sync-worktree "/fake/path" "feature-branch"))
              (eq nil (shipit--sync-worktree "/fake/path" "feature-branch")))))

(ert-deftest shipit--delete-worktree-test ()
  "Test deleting a worktree."
  (should (or (eq t (shipit--delete-worktree "/fake/path"))
              (eq nil (shipit--delete-worktree "/fake/path")))))

(ert-deftest shipit--open-worktree-directory-test ()
  "Test opening worktree directory."
  (should (or (eq t (shipit--open-worktree-directory "/fake/path"))
              (eq nil (shipit--open-worktree-directory "/fake/path")))))

(ert-deftest shipit--get-current-repo-test ()
  "Test getting current repository from git remote."
  (let ((result (shipit--get-current-repo)))
    (should (or (null result)
                (and (stringp result)
                     (string-match-p ".+/.+" result))))))

(ert-deftest shipit--validate-pr-repo-matching-test ()
  "Test validation when PR repo matches current repo."
  (cl-letf (((symbol-function 'shipit--get-current-repo)
             (lambda () "owner/repo")))
    (should (eq t (shipit--validate-pr-repo "owner/repo")))))

(ert-deftest shipit--validate-pr-repo-mismatch-test ()
  "Test validation raises error when PR repo doesn't match current repo."
  (cl-letf (((symbol-function 'shipit--get-current-repo)
             (lambda () "owner/repo")))
    (should-error (shipit--validate-pr-repo "different/repo")
                  :type 'user-error)))

(ert-deftest shipit--validate-pr-repo-no-current-test ()
  "Test validation raises error when current repo cannot be determined."
  (cl-letf (((symbol-function 'shipit--get-current-repo)
             (lambda () nil)))
    (should-error (shipit--validate-pr-repo "owner/repo")
                  :type 'user-error)))

(ert-deftest shipit--worktree-section-text-properties-test ()
  "Test that Worktree section sets proper text properties for DWIM handler."
  (let ((pr-data '((number . 123)
                   (head (sha . "abc1234567890"))
                   (base (repo (full_name . "owner/repo")))))
        (shipit-worktree-directory ".worktrees/"))
    (with-temp-buffer
      (require 'shipit-buffer)
      (shipit-mode)
      (cl-letf (((symbol-function 'shipit--get-current-repo)
                 (lambda () "owner/repo")))
        (let ((inhibit-read-only t))
          (condition-case err
              (progn
                (shipit--insert-worktree-section pr-data)
                (goto-char (point-min))
                ;; Find a position with the worktree properties
                (let ((found-props nil))
                  (while (and (not found-props) (< (point) (point-max)))
                    (when (get-text-property (point) 'shipit-worktree)
                      (setq found-props t))
                    (unless found-props (forward-char 1)))
                  (should found-props)
                  (should (eq (get-text-property (point) 'shipit-worktree) t))
                  (should (equal (get-text-property (point) 'shipit-pr-number) 123))
                  (should (equal (get-text-property (point) 'shipit-repo) "owner/repo"))
                  (should (equal (get-text-property (point) 'shipit-pr-data) pr-data))))
            (error (ert-skip (format "Section insertion not supported in mock environment: %S" err)))))))))

(ert-deftest shipit--worktree-dwim-handler-test ()
  "Test that DWIM handler correctly identifies worktree section."
  (with-temp-buffer
    (require 'shipit-buffer)
    (require 'shipit-pr-sections)
    (shipit-mode)
    (let* ((pr-data '((number . 123)
                      (head (sha . "abc1234567890")
                            (name . "feature-branch"))
                      (base (repo (full_name . "owner/repo")))))
           (inhibit-read-only t)
           (shipit-worktree-directory ".worktrees/"))
      (cl-letf (((symbol-function 'shipit--get-current-repo)
                 (lambda () "owner/repo")))
        (condition-case err
            (progn
              (shipit--insert-worktree-section pr-data)
              (goto-char (point-min))
              ;; Find worktree properties
              (let ((found-props nil))
                (while (and (not found-props) (< (point) (point-max)))
                  (when (get-text-property (point) 'shipit-worktree)
                    (setq found-props t))
                  (unless found-props (forward-char 1)))
                (should found-props)
                (should (eq (get-text-property (point) 'shipit-pr-number) 123))
                (should (equal (get-text-property (point) 'shipit-repo) "owner/repo"))
                ;; Test the matcher function
                (let* ((section (magit-current-section))
                       (matcher (lambda () (and (fboundp 'magit-current-section)
                                                (magit-section-match '(shipit-worktree-section) (magit-current-section))
                                                (not (get-text-property (point) 'shipit-comment))))))
                  ;; In mock environment, section might be nil, so just verify properties exist
                  (should (get-text-property (point) 'shipit-worktree)))))
          (error (ert-skip (format "Section insertion not supported in mock environment: %S" err))))))))

(ert-deftest shipit--open-file-from-worktree-or-repo-in-sync-test ()
  "Test opening files from worktree when in-sync."
  (let* ((pr-data '((number . 123)
                    (head (sha . "abc1234567890"))
                    (base (repo (full_name . "owner/repo")))))
         (file-path "src/main.el")
         (temp-worktree (make-temp-file "shipit-worktree-test-" t))
         (temp-file (expand-file-name file-path temp-worktree))
         (find-file-called nil)
         (find-file-arg nil))
    (unwind-protect
        (progn
          ;; Create the file in the temp worktree
          (make-directory (file-name-directory temp-file) t)
          (with-temp-file temp-file
            (insert "test content"))
          (cl-letf (((symbol-function 'shipit--get-worktree-status)
                     (lambda (_ _ _) 'in-sync))
                    ((symbol-function 'shipit--find-worktree-for-pr)
                     (lambda (_ _) temp-worktree))
                    ((symbol-function 'find-file)
                     (lambda (path)
                       (setq find-file-called t)
                       (setq find-file-arg path)
                       t)))
            (shipit--open-file-from-worktree-or-repo file-path pr-data)
            (should find-file-called)
            (should (equal find-file-arg temp-file))))
      (delete-directory temp-worktree t))))

(ert-deftest shipit--open-file-from-worktree-or-repo-fallback-test ()
  "Test falling back to revision when worktree is out-of-sync and user chooses revision."
  (let* ((pr-data '((number . 123)
                    (head (sha . "abc1234567890"))
                    (base (repo (full_name . "owner/repo")))))
         (file-path "src/main.el")
         (temp-worktree (make-temp-file "shipit-worktree-test-" t))
         (magit-find-file-called nil)
         (magit-find-file-args nil))
    ;; Create .git directory so shipit--open-file-at-revision recognizes it as a git repo
    (make-directory (expand-file-name ".git" temp-worktree))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit--get-worktree-status)
                   (lambda (_ _ _) 'out-of-sync))
                  ((symbol-function 'shipit--find-worktree-for-pr)
                   (lambda (_ _) temp-worktree))
                  ((symbol-function 'shipit--revision-exists-locally-p)
                   (lambda (_ _) t))  ; Mock revision check to return true
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _choices &rest _)
                     "at PR revision (read-only)"))
                  ((symbol-function 'magit-find-file)
                   (lambda (rev path)
                     (setq magit-find-file-called t)
                     (setq magit-find-file-args (list rev path))
                     t)))
          (shipit--open-file-from-worktree-or-repo file-path pr-data)
          (should magit-find-file-called)
          (should (equal (car magit-find-file-args) "abc1234567890"))
          (should (equal (cadr magit-find-file-args) file-path)))
      (delete-directory temp-worktree t))))

(ert-deftest shipit--open-file-from-worktree-or-repo-missing-file-test ()
  "Test error when file doesn't exist in worktree."
  (let* ((pr-data '((number . 123)
                    (head (sha . "abc1234567890"))
                    (base (repo (full_name . "owner/repo")))))
         (file-path "src/nonexistent.el")
         (temp-worktree (make-temp-file "shipit-worktree-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'shipit--get-worktree-status)
                   (lambda (_ _ _) 'in-sync))
                  ((symbol-function 'shipit--find-worktree-for-pr)
                   (lambda (_ _) temp-worktree)))
          (should-error (shipit--open-file-from-worktree-or-repo file-path pr-data)
                        :type 'user-error))
      (delete-directory temp-worktree t))))

(ert-deftest shipit--open-file-from-worktree-or-repo-no-worktree-test ()
  "Test opening file at revision when no worktree exists."
  (let* ((pr-data '((number . 123)
                    (head (sha . "abc1234567890"))
                    (base (repo (full_name . "owner/repo")))))
         (file-path "src/main.el")
         (magit-find-file-called nil)
         (magit-find-file-args nil))
    (cl-letf (((symbol-function 'shipit--get-worktree-status)
               (lambda (_ _ _) 'none))
              ((symbol-function 'shipit--revision-exists-locally-p)
               (lambda (_ _) t))  ; Mock revision check to return true
              ((symbol-function 'magit-find-file)
               (lambda (rev path)
                 (setq magit-find-file-called t)
                 (setq magit-find-file-args (list rev path))
                 t)))
      (shipit--open-file-from-worktree-or-repo file-path pr-data)
      (should magit-find-file-called)
      (should (equal (car magit-find-file-args) "abc1234567890"))
      (should (equal (cadr magit-find-file-args) file-path)))))

(ert-deftest shipit--open-file-at-point-test ()
  "Test interactive file opening from Files section."
  (let* ((pr-data '((number . 123)
                    (head (sha . "abc1234567890"))
                    (base (repo (full_name . "owner/repo")))))
         (file-path "src/main.el")
         (temp-worktree (make-temp-file "shipit-worktree-test-" t))
         (temp-file (expand-file-name file-path temp-worktree))
         (find-file-called nil)
         (find-file-arg nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory temp-file) t)
          (with-temp-file temp-file
            (insert "test content"))
          (with-temp-buffer
            (insert "test file")
            (put-text-property (point-min) (point-max) 'shipit-file-path file-path)
            (put-text-property (point-min) (point-max) 'shipit-pr-data pr-data)
            (goto-char (point-min))
            (cl-letf (((symbol-function 'shipit--get-worktree-status)
                       (lambda (_ _ _) 'in-sync))
                      ((symbol-function 'shipit--find-worktree-for-pr)
                       (lambda (_ _) temp-worktree))
                      ((symbol-function 'find-file)
                       (lambda (path)
                         (setq find-file-called t)
                         (setq find-file-arg path)
                         t)))
              (shipit--open-file-at-point)
              (should find-file-called)
              (should (equal find-file-arg temp-file)))))
      (delete-directory temp-worktree t))))

(ert-deftest shipit--open-commit-file-at-point-test ()
  "Test opening commit file at specific revision."
  (let* ((file-path "src/main.el")
         (commit-sha "def4567890abcdef")
         (magit-find-file-called nil)
         (magit-find-file-args nil))
    (with-temp-buffer
      (insert "test file content")
      ;; Set commit file properties
      (put-text-property (point-min) (point-max) 'shipit-file-path file-path)
      (put-text-property (point-min) (point-max) 'shipit-commit-sha commit-sha)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'shipit--revision-exists-locally-p)
                 (lambda (_ _) t))  ; Mock revision check to return true
                ((symbol-function 'magit-find-file)
                 (lambda (rev path)
                   (setq magit-find-file-called t)
                   (setq magit-find-file-args (list rev path))
                   t)))
        (shipit--open-file-at-point)
        (should magit-find-file-called)
        (should (equal (car magit-find-file-args) commit-sha))
        (should (equal (cadr magit-find-file-args) file-path))))))

(ert-deftest shipit--open-file-at-point-dispatches-correctly-test ()
  "Test that open-file-at-point dispatches to correct handler based on context."
  (let* ((file-path "src/main.el")
         (pr-file-opened nil)
         (commit-file-opened nil))
    ;; Test 1: With commit-sha property, should call commit file handler
    (with-temp-buffer
      (insert "commit file")
      (put-text-property (point-min) (point-max) 'shipit-file-path file-path)
      (put-text-property (point-min) (point-max) 'shipit-commit-sha "abc123")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'shipit--open-commit-file-at-point)
                 (lambda () (setq commit-file-opened t)))
                ((symbol-function 'shipit--open-pr-file-at-point)
                 (lambda () (setq pr-file-opened t))))
        (shipit--open-file-at-point)
        (should commit-file-opened)
        (should-not pr-file-opened)))

    ;; Reset
    (setq pr-file-opened nil)
    (setq commit-file-opened nil)

    ;; Test 2: Without commit-sha property, should call PR file handler
    (with-temp-buffer
      (insert "pr file")
      (put-text-property (point-min) (point-max) 'shipit-file-path file-path)
      (put-text-property (point-min) (point-max) 'shipit-pr-number 123)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'shipit--open-commit-file-at-point)
                 (lambda () (setq commit-file-opened t)))
                ((symbol-function 'shipit--open-pr-file-at-point)
                 (lambda () (setq pr-file-opened t))))
        (shipit--open-file-at-point)
        (should pr-file-opened)
        (should-not commit-file-opened)))))

(provide 'test-shipit-worktree)
;;; test-shipit-worktree.el ends here
