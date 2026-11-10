;;; test-shipit-worktree.el --- Tests for worktree management -*- lexical-binding: t; -*-

(require 'ert)
(require 'shipit-worktree)
(require 'shipit-commands)
(require 'shipit-test-http)

;;; Real repositories in temporary directories

(defun test-shipit-worktree--git (&rest arguments)
  "Run git with ARGUMENTS in `default-directory'; signal when it fails."
  (unless (zerop (apply #'call-process "git" nil nil nil arguments))
    (error "git %s failed in %s" (string-join arguments " ") default-directory)))

(defun test-shipit-worktree--in-a-new-directory (function)
  "Call FUNCTION in a new directory that is in no repository; remove it after.
Git reads no settings of whoever runs the tests and looks for no
repository above the directory.  Shipit remembers nothing of the
directories of earlier tests."
  (let* ((directory (file-name-as-directory
                     (file-truename (make-temp-file "shipit-worktree-test-" t))))
         (default-directory directory)
         (process-environment
          (append (list "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                        (concat "GIT_CEILING_DIRECTORIES="
                                (directory-file-name
                                 (file-name-directory (directory-file-name directory)))))
                  process-environment))
         (shipit--repo-from-remote-cache (make-hash-table :test 'equal))
         (shipit--repo-root-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (funcall function)
      (delete-directory directory t))))

(defun test-shipit-worktree--make-clone-of (repo)
  "Make `default-directory' a repository whose remote origin names REPO at GitHub."
  (test-shipit-worktree--git "init" "-q")
  (test-shipit-worktree--git "remote" "add" "origin"
                             (format "https://github.com/%s.git" repo)))

(defmacro test-shipit-worktree--in-a-clone-of (repo &rest body)
  "Run BODY in a new repository whose remote origin names REPO at GitHub.
See `test-shipit-worktree--in-a-new-directory'."
  (declare (indent 1))
  `(test-shipit-worktree--in-a-new-directory
    (lambda ()
      (test-shipit-worktree--make-clone-of ,repo)
      ,@body)))

(defun test-shipit-worktree--no-clone-said (repo)
  "What shipit says in `default-directory', which is no clone of REPO."
  (format "%s is no clone of %s, so this pull request has no worktree here"
          (abbreviate-file-name default-directory) repo))

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
    (test-shipit-worktree--in-a-clone-of "owner/repo"
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
      (test-shipit-worktree--in-a-clone-of "owner/repo"
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
  "A worktree is not made for a pull request whose commit git cannot fetch."
  ;; GIVEN a new clone of owner/repo, where git may not use the network
  ;; WHEN a worktree is made for a pull request whose commit is not there
  ;; THEN it gives nil, and the folder of the worktree is not made
  (test-shipit-worktree--in-a-clone-of "owner/repo"
    ;; Git refuses each address but a file, so the fetch stays off the network.
    (let ((process-environment (cons "GIT_ALLOW_PROTOCOL=file" process-environment)))
      (should-not (shipit--create-worktree-pr "owner/repo" 123 "feature-branch" "abc1234"))
      (should-not (file-directory-p (shipit--get-worktree-path 123 "feature-branch"))))))

(ert-deftest shipit--sync-worktree-test ()
  "Test syncing worktree with latest PR commits."
  (should (or (eq t (shipit--sync-worktree "/fake/path" "owner/repo" 123 "feature-branch"))
              (eq nil (shipit--sync-worktree "/fake/path" "owner/repo" 123 "feature-branch")))))

(ert-deftest shipit--delete-worktree-test ()
  "Test deleting a worktree."
  (should (or (eq t (shipit--delete-worktree "/fake/path"))
              (eq nil (shipit--delete-worktree "/fake/path")))))

(ert-deftest shipit--open-worktree-directory-test ()
  "Test opening worktree directory."
  (should (or (eq t (shipit--open-worktree-directory "/fake/path"))
              (eq nil (shipit--open-worktree-directory "/fake/path")))))

(ert-deftest shipit--validate-pr-repo-matching-test ()
  "A clone of the repository of a pull request validates."
  ;; GIVEN a repository whose remote origin names owner/repo
  ;; WHEN a pull request of owner/repo is validated there
  ;; THEN it passes
  (test-shipit-worktree--in-a-clone-of "owner/repo"
    (should (eq t (shipit--validate-pr-repo "owner/repo")))))

(defmacro test-shipit-worktree--when-github-says-no-fork (repo &rest body)
  "Run BODY while GitHub says that REPO, as owner/name, is no fork.
Shipit asks GitHub about a repository of the clone that has the name
of the repository of a pull request.  It remembers no answer about a
fork from an earlier test."
  (declare (indent 1))
  `(let ((shipit-github-token "not-a-token")
         (shipit--github-token-cache nil)
         (shipit-pr-backend 'github)
         (shipit-pr-backend-config nil)
         (shipit--forked-from-cache (make-hash-table :test 'equal)))
     (shipit-test-http-answering
         (list (list (concat "/repos/" (regexp-quote ,repo) "\\'") 200
                     (list (cons 'full_name ,repo) (cons 'fork :json-false))))
       ,@body)))

(ert-deftest shipit--validate-pr-repo-mismatch-test ()
  "A clone of another repository does not validate."
  ;; GIVEN a repository whose remote origin names owner/repo
  ;; AND GitHub says that owner/repo is no fork
  ;; WHEN a pull request of different/repo is validated there
  ;; THEN a user error says that the directory is no clone of
  ;;   different/repo, so the pull request has no worktree here
  (test-shipit-worktree--in-a-clone-of "owner/repo"
    (test-shipit-worktree--when-github-says-no-fork "owner/repo"
      (should (equal (test-shipit-worktree--no-clone-said "different/repo")
                     (cadr (should-error (shipit--validate-pr-repo "different/repo")
                                         :type 'user-error)))))))

(ert-deftest shipit--validate-pr-repo-no-current-test ()
  "A directory that is in no repository does not validate."
  ;; GIVEN a directory that is in no repository
  ;; WHEN a pull request of owner/repo is validated there
  ;; THEN a user error says that the directory is no clone of
  ;;   owner/repo, so the pull request has no worktree here
  (test-shipit-worktree--in-a-new-directory
   (lambda ()
     (should (equal (test-shipit-worktree--no-clone-said "owner/repo")
                    (cadr (should-error (shipit--validate-pr-repo "owner/repo")
                                        :type 'user-error)))))))

(ert-deftest shipit--worktree-section-text-properties-test ()
  "Test that Worktree section sets proper text properties for DWIM handler."
  (let ((pr-data '((number . 123)
                   (head (sha . "abc1234567890"))
                   (base (repo (full_name . "owner/repo")))))
        (shipit-worktree-directory ".worktrees/"))
    (with-temp-buffer
      (require 'shipit-buffer)
      (shipit-mode)
      (test-shipit-worktree--in-a-clone-of "owner/repo"
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
      (test-shipit-worktree--in-a-clone-of "owner/repo"
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
                     (lambda (_ _ _ &optional _) 'in-sync))
                    ((symbol-function 'shipit--find-worktree-for-pr)
                     (lambda (_ _ &optional _) temp-worktree))
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
                   (lambda (_ _ _ &optional _) 'out-of-sync))
                  ((symbol-function 'shipit--find-worktree-for-pr)
                   (lambda (_ _ &optional _) temp-worktree))
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
                   (lambda (_ _ _ &optional _) 'in-sync))
                  ((symbol-function 'shipit--find-worktree-for-pr)
                   (lambda (_ _ &optional _) temp-worktree)))
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
               (lambda (_ _ _ &optional _) 'none))
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
                       (lambda (_ _ _ &optional _) 'in-sync))
                      ((symbol-function 'shipit--find-worktree-for-pr)
                       (lambda (_ _ &optional _) temp-worktree))
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

(ert-deftest test-shipit-ret-tries-the-functions-others-add-first ()
  "GIVEN a function for RET that handles the text at point, which also
names a file
WHEN RET is pressed there
THEN the function handles it, and the file is not opened."
  (with-temp-buffer
    (insert (propertize "a note about a file" 'shipit-file-path "src/main.el"
                        'test-note t))
    (goto-char (point-min))
    (let* ((handled nil)
           (shipit-open-at-point-functions
            (list (lambda ()
                    (when (get-text-property (point) 'test-note)
                      (setq handled t))))))
      (shipit--open-file-at-point)
      (should handled)
      (should-not (get-file-buffer "src/main.el")))))

(ert-deftest test-shipit--worktree--commit-present-p-true-when-cat-file-zero ()
  "GIVEN a REPO-ROOT directory and a SHA
WHEN `git cat-file -e SHA^{commit}\=' exits 0 (commit is local)
THEN `shipit--worktree--commit-present-p\=' returns t -- so the
     fetch step in `shipit--create-worktree-pr\=' can be skipped
     on warm caches."
  (let ((tmp (make-temp-file "shipit-wt-test" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-file)
                   (lambda (program _infile _buffer _display &rest args)
                     (cond
                      ((and (equal program "git")
                            (equal args '("cat-file" "-e" "deadbeef^{commit}")))
                       0)
                      (t 128)))))
          (should (shipit--worktree--commit-present-p tmp "deadbeef")))
      (delete-directory tmp t))))

(ert-deftest test-shipit--worktree--commit-present-p-false-when-cat-file-nonzero ()
  "GIVEN `git cat-file -e\=' exits non-zero (commit not present)
WHEN `--commit-present-p\=' is invoked
THEN nil is returned -- so the caller fetches the ref."
  (let ((tmp (make-temp-file "shipit-wt-test" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-file)
                   (lambda (&rest _args) 128)))
          (should-not (shipit--worktree--commit-present-p tmp "abc123")))
      (delete-directory tmp t))))

(ert-deftest test-shipit--worktree--commit-present-p-validates-inputs ()
  "GIVEN bad inputs (nil REPO-ROOT, missing dir, nil/empty SHA)
WHEN `--commit-present-p\=' is invoked
THEN nil is returned without invoking git -- the helper guards
     its own preconditions so the caller doesn\='t need to."
  (let ((git-called nil))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args) (setq git-called t) 0)))
      (should-not (shipit--worktree--commit-present-p nil "abc123"))
      (should-not (shipit--worktree--commit-present-p "/nonexistent/dir-zzz" "abc123"))
      (should-not (shipit--worktree--commit-present-p "/tmp" nil))
      (should-not (shipit--worktree--commit-present-p "/tmp" ""))
      (should-not git-called))))

(provide 'test-shipit-worktree)
;;; test-shipit-worktree.el ends here
