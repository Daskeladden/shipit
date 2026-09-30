;;; test-shipit-dev-utils.el --- The reload of shipit from source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; `shipit-reload' unbinds every shipit function and then loads each
;; file again.  A hook that still names an unbound shipit function, and
;; runs while the files load, makes every load fail.  `load-file' runs
;; `kill-buffer-hook' when it kills the buffer it read a file in.

;;; Code:

(require 'ert)
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

(provide 'test-shipit-dev-utils)
;;; test-shipit-dev-utils.el ends here
