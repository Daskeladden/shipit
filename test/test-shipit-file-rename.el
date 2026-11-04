;;; test-shipit-file-rename.el --- A renamed file in a list of files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; Acceptance tests: a renamed file shows its path as old -> new in each
;; list of changed files.  The tests draw the lists in a real buffer.
;; The forge gives the old path in its answer, and for a commit that
;; only this clone has, a real git repository gives it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit-pr-sections)
(require 'shipit-buffer)
(require 'shipit-file-filter)
(require 'shipit-preview)
(require 'shipit-pr-gitlab)

(defconst test-file-rename--line "R src/old.txt -> src/new.txt (+1 -0)"
  "The line of a file that moved from src/old.txt to src/new.txt, with one line more.")

(defun test-file-rename--github-files ()
  "Two changed files as GitHub gives them: a renamed one and a modified one."
  '(((filename . "src/new.txt") (previous_filename . "src/old.txt")
     (status . "renamed") (additions . 1) (deletions . 0) (patch . "@@ -40 +40,2 @@\n 40\n+41"))
    ((filename . "README.md")
     (status . "modified") (additions . 1) (deletions . 0) (patch . "@@ -1 +1,2 @@\n a\n+b"))))

(defun test-file-rename--gitlab-files ()
  "Two changed files as GitLab gives them: a renamed one and a modified one."
  (mapcar #'shipit-pr-gitlab--normalize-file
          '(((new_path . "src/new.txt") (old_path . "src/old.txt") (renamed_file . t)
             (diff . "@@ -40 +40,2 @@\n 40\n+41"))
            ((new_path . "README.md") (old_path . "README.md")
             (diff . "@@ -1 +1,2 @@\n a\n+b")))))

(defmacro test-file-rename--in-files-section (files &rest body)
  "Run BODY in a pull request buffer whose Files Changed shows FILES, open."
  (declare (indent 1))
  `(with-temp-buffer
     (shipit-mode)
     (setq shipit-buffer-repo "o/r"
           shipit-buffer-pr-number 1)
     (let ((inhibit-read-only t))
       (magit-insert-section (root)
         (shipit--render-files-section ,files "o/r" 1 nil "abc123")))
     (magit-section-show (test-file-rename--section-of-type 'pr-files))
     ,@body))

(defun test-file-rename--section-of-type (type)
  "The first section of TYPE in this buffer, or an error."
  (or (cl-labels ((find-in (section)
                    (if (eq type (oref section type))
                        section
                      (seq-some #'find-in (oref section children)))))
        (find-in magit-root-section))
      (error "No section of type %s" type)))

(defun test-file-rename--line-with (text)
  "The line of this buffer that holds TEXT, as plain text without its indent."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun test-file-rename--open-file ()
  "Open the section of the renamed file, as TAB on it does."
  (goto-char (point-min))
  (search-forward "new.txt")
  (magit-section-show (magit-current-section)))

(defun test-file-rename--git (&rest arguments)
  "Run git with ARGUMENTS in `default-directory', and fail when git fails."
  (unless (zerop (apply #'call-process "git" nil nil nil
                        "-c" "user.name=Tester" "-c" "user.email=tester@example.com"
                        arguments))
    (error "git %s failed" (string-join arguments " "))))

(defun test-file-rename--make-file ()
  "Write src/old.txt in `default-directory', with 40 lines."
  (make-directory "src" t)
  (with-temp-file "src/old.txt"
    (dotimes (i 40) (insert (format "%d\n" (1+ i))))))

(defmacro test-file-rename--in-repo (&rest body)
  "Run BODY in a repository with 2 commits on its branch main.
The second one moves src/old.txt to src/new.txt, and adds line 41 to it."
  (declare (indent 0))
  `(let ((default-directory (file-name-as-directory (make-temp-file "shipit-rename-" t))))
     (unwind-protect
         (progn
           (test-file-rename--git "init" "-q" "-b" "main")
           (test-file-rename--make-file)
           (test-file-rename--git "add" ".")
           (test-file-rename--git "commit" "-q" "-m" "Add the file")
           (test-file-rename--git "mv" "src/old.txt" "src/new.txt")
           (with-temp-buffer
             (insert "41\n")
             (append-to-file (point-min) (point-max) "src/new.txt"))
           (test-file-rename--git "add" ".")
           (test-file-rename--git "commit" "-q" "-m" "Move the file")
           ,@body)
       (delete-directory default-directory t))))

(defun test-file-rename--shows-only-the-change ()
  "Check that the open diff of the renamed file shows line 41, and not the rest."
  (should (string-match-p "41" (buffer-string)))
  (should-not (string-match-p "\\+.*\\b20\\b" (buffer-string))))

(defmacro test-file-rename--in-commit-files (files &rest body)
  "Run BODY in a buffer that shows FILES as the files of a commit."
  (declare (indent 1))
  `(with-temp-buffer
     (shipit-mode)
     (let ((inhibit-read-only t)
           (section nil))
       (magit-insert-section (root)
         (setq section (magit-insert-section (commit-files nil)
                         (magit-insert-heading "Files\n"))))
       (shipit--render-commit-files-section-body section ,files nil))
     ,@body))

;;; The list of a pull request

(ert-deftest test-file-rename-the-line-shows-the-old-and-the-new-path ()
  ;; GIVEN a pull request of GitHub or of GitLab that renames a file and
  ;; modifies another
  (dolist (files (list (test-file-rename--github-files) (test-file-rename--gitlab-files)))
    ;; WHEN Files Changed is opened
    (test-file-rename--in-files-section files
      ;; THEN the renamed file shows old -> new
      (should (equal test-file-rename--line (test-file-rename--line-with "new.txt")))
      ;; AND the modified file shows its path alone
      (should (equal "M README.md (+1 -0)" (test-file-rename--line-with "README.md"))))))

(ert-deftest test-file-rename-the-commands-of-the-line-use-the-new-path ()
  ;; GIVEN Files Changed with a renamed file
  (test-file-rename--in-files-section (test-file-rename--github-files)
    ;; WHEN point is on the old path in its line
    (goto-char (point-min))
    (search-forward "src/old.txt")
    ;; THEN the commands of the line act on the new path
    (should (equal "src/new.txt" (get-text-property (point) 'shipit-file-path)))))

(ert-deftest test-file-rename-the-filter-finds-the-old-path ()
  ;; GIVEN a pull request that renames a file
  (let ((files (test-file-rename--github-files)))
    ;; WHEN the filter of Files Changed holds a word of the old path only
    (with-temp-buffer
      (shipit-mode)
      (setq shipit--files-filter-text "old.txt")
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (shipit--render-files-section files "o/r" 1 nil "abc123")))
      (magit-section-show (test-file-rename--section-of-type 'pr-files))
      ;; THEN the renamed file stays AND the other file does not
      (should (equal test-file-rename--line (test-file-rename--line-with "new.txt")))
      (should-not (string-match-p "README.md" (buffer-string))))))

;;; The list of a commit

(ert-deftest test-file-rename-a-commit-on-the-forge-shows-the-old-and-the-new-path ()
  ;; GIVEN a commit of a pull request that renames a file
  ;; WHEN the files of the commit are opened
  (test-file-rename--in-commit-files (test-file-rename--github-files)
    ;; THEN the renamed file shows old -> new
    (should (equal test-file-rename--line (test-file-rename--line-with "new.txt")))))

(ert-deftest test-file-rename-a-commit-only-this-clone-has-shows-the-old-and-the-new-path ()
  ;; GIVEN a commit that renames a file, and that only this clone has
  (test-file-rename--in-repo
    ;; WHEN the files of the commit are opened
    (test-file-rename--in-commit-files (shipit--get-local-commit-files "HEAD")
      ;; THEN the renamed file shows old -> new, on one line only
      (should (equal test-file-rename--line (test-file-rename--line-with "new.txt")))
      (should (= 1 (how-many "src/old\\.txt" (point-min) (point-max))))
      ;; WHEN its diff is opened
      (test-file-rename--open-file)
      ;; THEN it shows the change, and not the whole file as new
      (test-file-rename--shows-only-the-change))))

;;; The other lists that come from git

(ert-deftest test-file-rename-a-force-push-shows-the-old-and-the-new-path ()
  ;; GIVEN a force push whose new head renames a file
  (test-file-rename--in-repo
    ;; WHEN its comparison is opened
    (let ((files (shipit--range-files-from-git "HEAD~" "HEAD")))
      (with-temp-buffer
        (shipit-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (root)
            (shipit--insert-force-push-files files "o/r" "HEAD~" "HEAD")))
        ;; THEN the renamed file shows old -> new
        (should (equal test-file-rename--line (test-file-rename--line-with "new.txt")))
        ;; WHEN its diff is opened
        (test-file-rename--open-file)
        ;; THEN it shows the change, and not the whole file as new
        (test-file-rename--shows-only-the-change)))))

(ert-deftest test-file-rename-the-preview-shows-the-old-and-the-new-path ()
  ;; GIVEN a branch not yet in a pull request, whose commit renames a file
  (test-file-rename--in-repo
    (test-file-rename--git "branch" "base" "HEAD~")
    (let ((shipit-preview--base-ref "base"))
      ;; WHEN the preview of its pull request draws its files
      (test-file-rename--in-files-section (shipit-preview--get-files)
        ;; THEN the renamed file shows old -> new
        (should (equal test-file-rename--line (test-file-rename--line-with "new.txt")))
        ;; WHEN its diff is opened
        (test-file-rename--open-file)
        ;; THEN it shows the change, and not the whole file as new
        (test-file-rename--shows-only-the-change)))))

(provide 'test-shipit-file-rename)
;;; test-shipit-file-rename.el ends here
