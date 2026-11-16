;;; test-shipit-dev-utils.el --- The reload of shipit from source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; `shipit-reload' compiles each file whose compiled file is old,
;; unbinds every shipit function but those of its own file, and then
;; loads each file again, compiled.  A hook that still names an unbound
;; shipit function, and runs while the files load, makes every load
;; fail.  `load-file' runs `kill-buffer-hook' when it kills the buffer
;; it read a file in.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name
            "../shipit-dev-utils.el"
            (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest test-shipit-dev-reload-empties-the-kill-hook-of-shipit ()
  "GIVEN a shipit function on the global `kill-buffer-hook', beside
another function
WHEN the hooks are emptied of shipit functions, before a reload
unbinds them
THEN the kill hook holds only the other function."
  (let ((kill-buffer-hook (list 'shipit-test--on-kill #'ignore)))
    (shipit-dev--remove-shipit-hooks)
    (should (equal '(ignore) kill-buffer-hook))))

(ert-deftest test-shipit-dev-reload-empties-the-global-kill-hook-from-any-buffer ()
  "GIVEN a shipit function on the global `kill-buffer-hook'
AND a current buffer that has a value of its own for that hook, as a
terminal buffer has
WHEN the hooks are emptied of shipit functions
THEN the global hook holds only the other function: the buffer that a
file is read in runs the global hook."
  (let ((kill-buffer-hook (list 'shipit-test--on-kill #'ignore)))
    (with-temp-buffer
      (add-hook 'kill-buffer-hook #'ignore nil t)
      (shipit-dev--remove-shipit-hooks)
      (should (equal '(ignore) (default-value 'kill-buffer-hook))))))

(ert-deftest test-shipit-dev-reload-empties-the-command-hooks-of-shipit ()
  "GIVEN a shipit function on the global `post-command-hook'
WHEN the hooks are emptied of shipit functions
THEN the command hook holds none, as before."
  (let ((post-command-hook (list 'shipit-test--after-command)))
    (shipit-dev--remove-shipit-hooks)
    (should-not post-command-hook)))

(ert-deftest test-shipit-dev-reload-loads-other-packages-last ()
  "GIVEN a package on top of shipit that listed its files for the reload
WHEN the files of a reload are listed
THEN shipit's own come first, from its directory, and the package's
follow, in the order it gave: its code calls shipit's."
  (let ((shipit-dev-reload-extra-files '("/pkg/lisp/pkg-a.el" "/pkg/lisp/pkg-b.el")))
    (let ((paths (shipit-dev--reload-paths "/shipit/lisp/")))
      (should (equal "/shipit/lisp/shipit-faces.el" (car paths)))
      (should (equal '("/pkg/lisp/pkg-a.el" "/pkg/lisp/pkg-b.el") (last paths 2)))
      (should (member "/shipit/lisp/shipit.el" paths)))))

(defvar test-shipit-dev--ran nil
  "What the forms that waited for a test file gave, newest first.")

(defun test-shipit-dev--write (directory name &rest forms)
  "Write FORMS to the file NAME.el in DIRECTORY, which provides NAME.
Return the name of the file."
  (let ((file (expand-file-name (concat name ".el") directory)))
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (dolist (form (append forms `((provide ',(intern name)))))
        (prin1 form (current-buffer))
        (insert "\n")))
    file))

(defmacro test-shipit-dev--with-two-files (first second &rest body)
  "Run BODY with FIRST and SECOND bound to two files that are loaded.
Each defines one function, named as its file.  What is said goes to
`said', and the forms that BODY has wait for a file are forgotten after."
  (declare (indent 2))
  `(let* ((directory (make-temp-file "shipit-dev-test" t))
          (,first (test-shipit-dev--write
                   directory "shipit-test-reload-first"
                   '(defun shipit-test-reload-first () 'first)))
          (,second (test-shipit-dev--write
                    directory "shipit-test-reload-second"
                    '(defun shipit-test-reload-second () 'second)))
          (after-load-alist after-load-alist)
          (test-shipit-dev--ran nil)
          (said nil))
     (unwind-protect
         (cl-letf (((symbol-function 'message)
                    (lambda (format &rest args) (push (apply #'format format args) said))))
           (load-file ,first)
           (load-file ,second)
           ,@body)
       (delete-directory directory t))))

(ert-deftest test-shipit-dev-reload-runs-what-waits-for-a-file-after-the-last-file ()
  "GIVEN two files that are loaded, the second of a package on top of
the first, AND a form that waits for the first file and calls a function
of the second, as the configuration of a user starts such a package
WHEN a reload has unbound the functions of both, and loads the files
again
THEN no load fails, AND the form runs once, after the second file is
loaded.  It ran at the end of the first file before, where the function
it calls was not there yet."
  (test-shipit-dev--with-two-files first second
    (with-eval-after-load 'shipit-test-reload-first
      (push (shipit-test-reload-second) test-shipit-dev--ran))
    (setq test-shipit-dev--ran nil)
    (fmakunbound 'shipit-test-reload-first)
    (fmakunbound 'shipit-test-reload-second)
    (shipit-dev--load-files (list first second))
    (should-not (cl-some (lambda (line) (string-match-p "Error" line)) said))
    (should (equal '(second) test-shipit-dev--ran))
    (should (fboundp 'shipit-test-reload-first))))

(ert-deftest test-shipit-dev-reload-goes-on-after-a-file-that-fails ()
  "GIVEN two files, and a third one that signals an error when it loads
WHEN the three are loaded again, the failing one in the middle
THEN the failure is said with the name of the file, AND the file after
it is loaded all the same."
  (test-shipit-dev--with-two-files first second
    (let ((broken (test-shipit-dev--write (file-name-directory first) "shipit-test-reload-broken"
                                          '(error "This file is broken"))))
      (fmakunbound 'shipit-test-reload-second)
      (shipit-dev--load-files (list first broken second))
      (should (cl-some (lambda (line)
                         (string-match-p "Error loading shipit-test-reload-broken.el: This file is broken"
                                         line))
                       said))
      (should (fboundp 'shipit-test-reload-second)))))

(ert-deftest test-shipit-dev-reload-loads-a-file-after-the-file-it-registers-with ()
  "GIVEN the files of a reload, in their order
THEN the list of workflow runs comes after the file that holds the
handlers of `M-;': it adds its own handler there when it loads, and a
reload has unbound the function that takes it."
  (should (< (cl-position "shipit-pr-actions.el" shipit-dev--load-files :test #'equal)
             (cl-position "shipit-actions-list.el" shipit-dev--load-files :test #'equal))))

(defun test-shipit-dev--age (file seconds)
  "Make FILE SECONDS older than now."
  (set-file-times file (time-subtract (current-time) seconds)))

(ert-deftest test-shipit-dev-reload-takes-a-compiled-file-that-is-new-enough ()
  "GIVEN a source file with a compiled file as new as it, one with an
older compiled file, AND one with none
WHEN the file to load is picked for each
THEN the first loads compiled, AND the other two load from source: an
old compiled file holds code that the source has changed since."
  (let ((directory (make-temp-file "shipit-dev-test" t)))
    (unwind-protect
        (let ((fresh (expand-file-name "fresh.el" directory))
              (stale (expand-file-name "stale.el" directory))
              (plain (expand-file-name "plain.el" directory)))
          (dolist (file (list fresh stale plain (concat fresh "c") (concat stale "c")))
            (write-region "" nil file))
          (test-shipit-dev--age fresh 10)
          (test-shipit-dev--age (concat stale "c") 10)
          (should (equal (concat fresh "c") (shipit-dev--compiled-path fresh)))
          (should (equal stale (shipit-dev--compiled-path stale)))
          (should (equal plain (shipit-dev--compiled-path plain)))
          (should (equal (list stale plain)
                         (shipit-dev--stale-files (list fresh stale plain)))))
      (delete-directory directory t))))

(ert-deftest test-shipit-dev-reload-compiles-the-stale-files-with-make ()
  "GIVEN two stale files of shipit, AND one of a package on top of it
WHEN the arguments of the compile are made
THEN make compiles those three files alone, with the directory of the
package on the load path, so that its files find each other."
  (should (equal '("compile"
                   "LISP_FILES=/shipit/lisp/shipit-core.el /shipit/lisp/shipit-render.el /pkg/lisp/pkg-a.el"
                   "EXTRA_LOAD_PATH=/pkg/lisp/")
                 (shipit-dev--compile-arguments
                  '("/shipit/lisp/shipit-core.el" "/shipit/lisp/shipit-render.el"
                    "/pkg/lisp/pkg-a.el")
                  "/shipit/lisp/"))))

(ert-deftest test-shipit-dev-reload-loads-a-file-compiled ()
  "GIVEN a file that is loaded, AND a compiled file of it that is new
enough
WHEN a reload has unbound its function, and loads the file again
THEN the function is compiled code, not code that Emacs interprets:
interpreted code runs slower and makes more garbage."
  (test-shipit-dev--with-two-files first _second
    (byte-compile-file first)
    (fmakunbound 'shipit-test-reload-first)
    (shipit-dev--load-files (list first))
    (should (byte-code-function-p (symbol-function 'shipit-test-reload-first)))))

(ert-deftest test-shipit-dev-reload-finds-its-own-file-at-the-root ()
  "GIVEN shipit's lisp directory on the load path, AND this file at the
root of the project, beside that directory, not on the load path
WHEN the reload looks for its own file, to load it first
THEN it finds the file at the root: a reload that does not find it runs
the old code of the reload."
  (should (equal (expand-file-name
                  "../shipit-dev-utils.el"
                  (file-name-directory (locate-library "shipit-debug")))
                 (shipit-dev--file)))
  (should (file-exists-p (shipit-dev--file))))

(ert-deftest test-shipit-dev-reload-loads-its-own-file-once ()
  "GIVEN the mark that the reload binds while it runs the copy of itself
that it just loaded
WHEN the copy asks whether the mark is bound
THEN it sees the binding: the mark is a special variable.  A lexical
binding is not seen there, so the copy loads its file again, and again,
until Emacs stops it at its depth of nesting."
  (should (special-variable-p 'shipit--reload-bootstrapped)))

(ert-deftest test-shipit-dev-reload-keeps-the-commands-of-its-file ()
  "GIVEN the commands of the reload, AND a loaded file of shipit
WHEN the functions that a reload unbinds are listed
THEN the commands of the reload are not in the list, so that
`shipit-reload' can run the next reload, AND the function of the file
is in it."
  (test-shipit-dev--with-two-files _first _second
    (let ((unbound (shipit-dev--functions-to-unbind)))
      (should-not (memq 'shipit-reload unbound))
      (should-not (memq 'shipit-compile-and-reload unbound))
      (should (memq 'shipit-test-reload-first unbound)))))

(ert-deftest test-shipit-dev-reload-makes-the-autoloads-without-a-load-error ()
  "GIVEN the checkout of shipit, whose shipit-preview.el autoloads a
`cl-defun'
WHEN the reload makes the autoloads again, with `make autoloads'
THEN make reports no load error, and the autoload of that function is
written.  The batch Emacs of make has no packages, so a file that it
loads fails at its first require of one."
  (let ((default-directory (file-name-directory (shipit-dev--file))))
    (with-temp-buffer
      (should (zerop (call-process "make" nil t nil "autoloads")))
      (should-not (string-match-p "load error" (buffer-string))))
    (with-temp-buffer
      (insert-file-contents "lisp/shipit-autoloads.el")
      (should (search-forward "(autoload 'shipit-preview-branch" nil t)))))

(provide 'test-shipit-dev-utils)
;;; test-shipit-dev-utils.el ends here
