;;; test-shipit-page-menu.el --- Tests for the menu of a page -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for the menu that SPC opens in a pull request buffer and in an
;; issue buffer, and for the lines of those buffers that have keys of
;; their own.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'transient)
(require 'shipit-core)
(require 'shipit-buffer)
(require 'shipit-issues-buffer)
(require 'shipit-pr-sections)
(require 'shipit-checks)
(require 'shipit-pr-diff)
(require 'shipit-render)
;; The commands that the menus name.  shipit.el loads them for a user.
(require 'shipit-commands)
(require 'shipit-pr-actions)
(require 'shipit-test-meow)

(defmacro test-page-menu--in (mode &rest body)
  "Run BODY in a buffer of MODE, the mode of a page."
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

(defun test-page-menu--command-of (menu key)
  "The command that MENU has on KEY."
  (plist-get (cdr (transient-get-suffix menu key)) :command))

(defun test-page-menu--names-the-keys-p (menu keys)
  "Whether MENU has, on each of KEYS, the command this buffer has on that key."
  (cl-every (lambda (key)
              (let ((command (key-binding (kbd key))))
                (should command)
                (should (eq command (test-page-menu--command-of menu key)))
                t))
            keys))

;;; The menus

(ert-deftest test-page-menu-spc-opens-the-menu-of-a-pull-request-buffer ()
  "GIVEN a pull request buffer
THEN SPC opens the menu of the page, which is the menu of a pull request
there, AND that menu has each command of the buffer on the key that the
buffer has for it, AND SPC in the menu closes it."
  (test-page-menu--in #'shipit-mode
    (should (eq (key-binding (kbd "SPC")) #'shipit-page-menu))
    (should (eq shipit-page-menu-command #'shipit-pr-menu))
    (should (test-page-menu--names-the-keys-p
             'shipit-pr-menu
             '("c" "a" "R" "k" "M-m" "M-;"
               "e" "d" "D" "f" "m" "M" "+" "Z"
               "g" "G" "s" "L" "O" "M-w" "o" "w" "$" "?" "q" "Q")))
    (should (eq (test-page-menu--command-of 'shipit-pr-menu "SPC")
                #'transient-quit-one))))

(ert-deftest test-page-menu-spc-opens-the-menu-of-an-issue-buffer ()
  "GIVEN an issue buffer
THEN SPC opens the menu of the page, which is the menu of an issue
there, AND that menu has each command of the buffer on the key that the
buffer has for it, AND SPC in the menu closes it."
  (test-page-menu--in #'shipit-issue-mode
    (should (eq (key-binding (kbd "SPC")) #'shipit-page-menu))
    (should (eq shipit-page-menu-command #'shipit-issue-menu))
    (should (test-page-menu--names-the-keys-p
             'shipit-issue-menu
             '("M-;" "f" "l" "R" "O" "b" "g" "L" "o" "w" "$" "q" "Q")))
    (should (eq (test-page-menu--command-of 'shipit-issue-menu "SPC")
                #'transient-quit-one))))

(ert-deftest test-page-menu-shows-the-menu-that-the-buffer-names ()
  "GIVEN a buffer that names a command as its menu
WHEN the menu of the page is asked for
THEN that command runs."
  (with-temp-buffer
    (let ((shown nil))
      (setq-local shipit-page-menu-command (lambda () (interactive) (setq shown t)))
      (shipit-page-menu)
      (should shown))))

(ert-deftest test-page-menu-does-what-ret-does-in-a-buffer-without-a-menu ()
  "GIVEN a buffer without a menu, AND a line there with keys of its own
WHEN SPC is pressed on the line
THEN it does what RET does there.  SPC was a second RET on such a line
before the pages had menus, and it stays one where there is no menu."
  (with-temp-buffer
    (let ((done nil)
          (map (make-sparse-keymap)))
      (define-key map (kbd "RET") (lambda () (interactive) (setq done 'ret)))
      (define-key map (kbd "SPC") #'shipit-page-menu)
      (insert (propertize "a line" 'keymap map))
      (goto-char (point-min))
      (should-not shipit-page-menu-command)
      (call-interactively (key-binding (kbd "SPC")))
      (should (eq done 'ret)))))

;;; The lines with keys of their own

(ert-deftest test-page-menu-a-line-with-keys-of-its-own-leaves-spc-to-the-menu ()
  "GIVEN the keys of a commit line, of the line that loads the checks, of
the header of a pull request and of the header of an issue
THEN each has the menu of the page on SPC.  A line with keys of its own
is asked before the buffer is, and would keep SPC from the menu."
  (dolist (map (list shipit-commit-keymap shipit-checks-keymap
                     shipit-pr-header-keymap shipit-issue-header-keymap))
    (should (eq (lookup-key map (kbd "SPC")) #'shipit-page-menu))))

(ert-deftest test-page-menu-the-body-of-a-comment-leaves-spc-to-the-menu ()
  "GIVEN the body of a comment, drawn as a page draws it
THEN SPC on it is the menu of the page."
  (with-temp-buffer
    (cl-letf (((symbol-function 'shipit--create-jira-mention-overlays) nil))
      (shipit--insert-comment-body-only
       '((id . 1) (body . "It breaks when the file is empty.")
         (user (login . "someone")) (created_at . "2026-01-01T10:00:00Z"))
       6 nil nil "o/r" 7 nil t))
    (goto-char (point-min))
    (should (search-forward "It breaks" nil t))
    (should (eq (key-binding (kbd "SPC")) #'shipit-page-menu))))

(ert-deftest test-page-menu-a-line-of-a-snippet-leaves-spc-to-the-menu ()
  "GIVEN the lines of a code snippet with their numbers, each of which
opens the file at its line
THEN SPC on such a line is the menu of the page."
  (with-temp-buffer
    (insert (shipit--add-line-numbers '("first" "second") 10 "o" "r" "abc" "f.el"))
    (goto-char (point-min))
    (should (get-text-property (point) 'keymap))
    (should (eq (key-binding (kbd "SPC")) #'shipit-page-menu))))

;;; meow

(ert-deftest test-page-menu-meow-leaves-spc-to-the-menu ()
  "GIVEN meow, which takes SPC for its keypad in its motion state
WHEN meow loads
THEN it starts a pull request buffer and an issue buffer in its insert
state, where SPC reaches the menu."
  (shipit-test-with-meow-loaded
    (should (eq 'insert (alist-get 'shipit-mode meow-mode-state-list)))
    (should (eq 'insert (alist-get 'shipit-issue-mode meow-mode-state-list)))))

(provide 'test-shipit-page-menu)
;;; test-shipit-page-menu.el ends here
