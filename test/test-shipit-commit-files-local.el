;;; test-shipit-commit-files-local.el --- Files of an unpushed commit -*- lexical-binding: t; -*-

;;; Commentary:
;; The files a commit changed are asked of the forge, which cannot
;; answer for a commit it has never seen.  Preparing a stack means
;; looking at exactly those commits, so the answer comes from the
;; repository when the forge has none.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-sections)

(defmacro test-local-files--with-repo (&rest body)
  "Run BODY in a repository with one commit touching two files."
  (declare (indent 0))
  `(let ((dir (make-temp-file "shipit-local-files-" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory dir)))
           (dolist (args '(("-c" "init.defaultBranch=main" "init" "-q")
                           ("config" "user.email" "t@e.invalid")
                           ("config" "user.name" "T")))
             (apply #'call-process "git" nil nil nil args))
           (with-temp-file (expand-file-name "kept.txt" dir) (insert "one\n"))
           (call-process "git" nil nil nil "add" "kept.txt")
           (call-process "git" nil nil nil "commit" "-q" "-m" "first")
           (with-temp-file (expand-file-name "kept.txt" dir) (insert "one\ntwo\n"))
           (with-temp-file (expand-file-name "added.txt" dir) (insert "new\n"))
           (call-process "git" nil nil nil "add" "kept.txt" "added.txt")
           (call-process "git" nil nil nil "commit" "-q" "-m" "second")
           ,@body)
       (delete-directory dir t))))

(ert-deftest test-shipit-commit-files-come-from-git-when-the-forge-has-none ()
  "GIVEN a commit the forge has never seen
WHEN its files are asked for
THEN they come from the repository, with the counts git reports."
  (test-local-files--with-repo
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo) (cons '(:name "Test" :fetch-commit (lambda (&rest _) nil))
                                     '(:repo "o/r")))))
      (let* ((files (shipit--fetch-commit-files "o/r" "HEAD"))
             (names (sort (mapcar (lambda (f) (cdr (assq 'filename f))) files) #'string<)))
        (should (equal '("added.txt" "kept.txt") names))
        (let ((added (seq-find (lambda (f) (equal "added.txt" (cdr (assq 'filename f))))
                               files)))
          (should (equal 1 (cdr (assq 'additions added))))
          (should (equal "added" (cdr (assq 'status added)))))
        (let ((kept (seq-find (lambda (f) (equal "kept.txt" (cdr (assq 'filename f))))
                              files)))
          (should (equal 1 (cdr (assq 'additions kept))))
          (should (equal "modified" (cdr (assq 'status kept)))))))))

(ert-deftest test-shipit-commit-files-prefer-what-the-forge-says ()
  "GIVEN a forge that knows the commit
WHEN its files are asked for
THEN its answer is used unchanged.  It carries the patch and the
review state, which the repository cannot supply."
  (test-local-files--with-repo
    (cl-letf (((symbol-function 'shipit-pr--resolve-for-repo)
               (lambda (_repo)
                 (cons '(:name "Test"
                         :fetch-commit (lambda (&rest _)
                                         '((files . (((filename . "from-forge.txt")))))))
                       '(:repo "o/r")))))
      (should (equal '("from-forge.txt")
                     (mapcar (lambda (f) (cdr (assq 'filename f)))
                             (shipit--fetch-commit-files "o/r" "HEAD")))))))

(provide 'test-shipit-commit-files-local)
;;; test-shipit-commit-files-local.el ends here
