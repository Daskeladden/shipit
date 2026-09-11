;;; test-shipit-key-hints.el --- Hints name the real binding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; A message that tells the reader which key to press goes stale the
;; moment the key moves, and a wrong key is worse than none.  The
;; hints read the binding instead of naming a letter.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-buffer)
(require 'shipit-diff)

(ert-deftest test-skh-hint-names-the-binding ()
  "GIVEN a command reachable from a shipit buffer
WHEN a key hint is built for it
THEN it names the key that command is on."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "Q") #'ignore)
    (let ((shipit-mode-map map))
      (should (equal (shipit--key-hint #'ignore) "Q")))))

(defun test-skh--unbound-command ()
  "A command on no key, so the fallback has something to fall back on."
  (interactive))

(ert-deftest test-skh-hint-without-a-binding-names-the-command ()
  "GIVEN a command that is on no key
WHEN a key hint is built for it
THEN it names the command, rather than inventing a key."
  (let ((shipit-mode-map (make-sparse-keymap)))
    (should (equal (shipit--key-hint #'test-skh--unbound-command)
                   "test-skh--unbound-command"))))

(provide 'test-shipit-key-hints)
;;; test-shipit-key-hints.el ends here
