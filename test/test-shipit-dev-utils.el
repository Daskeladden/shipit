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

(provide 'test-shipit-dev-utils)
;;; test-shipit-dev-utils.el ends here
