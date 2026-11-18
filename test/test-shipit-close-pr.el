;;; test-shipit-close-pr.el --- Tests for closing a pull request with x -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; Tests for x in the menu of a pull request buffer, which closes the
;; pull request with an optional comment.  GitHub is answered at url.el,
;; and the answer to a yes-or-no question is given in place of the user.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'transient)
(require 'shipit-editor)
(require 'shipit-commands)
(require 'shipit-pr-actions)
(require 'shipit-test-http)
(require 'test-shipit-page-views)

;;; GitHub, with pull request 12 of o/r

(defun test-close-pr--pull-request (author state)
  "GitHub's answer about pull request 12 of o/r, by AUTHOR, first in STATE.
STATE is open, closed or merged.  A request that closes the pull
request closes it, as on GitHub."
  (let ((now state))
    (lambda (sent)
      (when (and sent (string-match-p "\"state\": *\"closed\"" sent))
        (setq now "closed"))
      (append (if (equal now "merged")
                  '((state . "closed") (merged . t) (merged_at . "2026-01-02T10:00:00Z"))
                `((state . ,now)))
              `((user (login . ,author)) (draft . :json-false))
              (test-page-views--pull-request 12 '(:title "Brew the coffee"))))))

(defconst test-close-pr--new-comment
  `("/repos/o/r/issues/12/comments\\'" 200
    ,(lambda (sent)
       (if sent
           '((id . 501) (body . "posted") (user (login . "me")))
         [])))
  "GitHub's answer to a new comment on pull request 12, and to a read of none.")

(defmacro test-close-pr--on-github (author state &rest body)
  "Run BODY as me, with GitHub holding pull request 12 of o/r by AUTHOR in STATE.
Each page and editor that BODY opens is killed at its end, and an open
menu is closed."
  (declare (indent 2))
  `(test-page-views--as-me
     (let ((before (buffer-list)))
       (unwind-protect
           (shipit-test-http-answering
               (append (list (list "/repos/o/r/pulls/12\\'" 200
                                   (test-close-pr--pull-request ,author ,state))
                             test-close-pr--new-comment)
                       (test-page-views--github '((12 :title "Brew the coffee"))))
             ,@body)
         (when transient--prefix (transient-quit-all))
         (dolist (buffer (buffer-list))
           (unless (memq buffer before)
             (when (or (with-current-buffer buffer (derived-mode-p 'shipit-mode))
                       (equal (buffer-name buffer) shipit-editor--buffer-name))
               (kill-buffer buffer))))))))

;;; What the user does

(defun test-close-pr--press-x (page)
  "Press SPC and then x on PAGE, at its first line, the way a user does.
Return the message of a refusal, or nil when shipit refused nothing."
  (set-window-buffer (selected-window) page)
  (with-current-buffer page
    (goto-char (point-min))
    (condition-case err
        (progn (execute-kbd-macro (kbd "SPC x")) nil)
      (user-error (error-message-string err)))))

(defun test-close-pr--editor ()
  "Return the editor of shipit, or nil when it is not open."
  (get-buffer shipit-editor--buffer-name))

(defun test-close-pr--send (text answer)
  "Write TEXT in the editor, press C-c C-c, and answer the question with ANSWER.
Return the question that shipit asked, or nil when it asked none."
  (let ((editor (test-close-pr--editor))
        (questions nil))
    (should editor)
    (set-window-buffer (selected-window) editor)
    (with-current-buffer editor
      (goto-char (point-max))
      (insert text)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (question) (push question questions) answer)))
        (execute-kbd-macro (kbd "C-c C-c"))))
    (test-page-views--settle)
    (car questions)))

(defun test-close-pr--cancel ()
  "Press C-c C-k in the editor."
  (let ((editor (test-close-pr--editor)))
    (should editor)
    (set-window-buffer (selected-window) editor)
    (with-current-buffer editor
      (execute-kbd-macro (kbd "C-c C-k")))))

;;; What the tests look at

(defun test-close-pr--sent ()
  "Return what shipit sent to GitHub about pull request 12, oldest first.
A new comment is (comment BODY), and a change of the state is (state
STATE)."
  (cl-loop for (method address) in shipit-test-http-requests
           for body in shipit-test-http-bodies
           when (and (equal method "POST")
                     (string-match-p "/repos/o/r/issues/12/comments\\'" address))
           collect (list 'comment (cdr (assq 'body (json-read-from-string body))))
           when (and (equal method "PATCH")
                     (string-match-p "/repos/o/r/pulls/12\\'" address))
           collect (list 'state (cdr (assq 'state (json-read-from-string body))))))

(defun test-close-pr--state-shown (page)
  "Return the state that PAGE shows for its pull request."
  (with-current-buffer page
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward "State: +\\([a-z]+\\)" nil t)
           (match-string-no-properties 1)))))

;;; The scenarios

(ert-deftest test-shipit-close-pr-x-closes-with-a-comment ()
  "x closes the pull request after its comment."
  ;; GIVEN the page of my open pull request #12
  ;; WHEN I press SPC x, write "Not needed any more", press C-c C-c and
  ;;   answer yes
  ;; THEN shipit asked "Close PR #12 with this comment?"
  ;; AND it sent GitHub the comment "Not needed any more", and then the
  ;;   state closed
  ;; AND the page shows #12 as closed
  (test-close-pr--on-github "me" "open"
    (let ((page (test-page-views--open-pr 12)))
      (should-not (test-close-pr--press-x page))
      (should (string-match-p "\\`Close PR #12 with this comment\\?"
                              (or (test-close-pr--send "Not needed any more" t) "")))
      (should (equal '((comment "Not needed any more") (state "closed"))
                     (test-close-pr--sent)))
      (should (equal "closed" (test-close-pr--state-shown page))))))

(ert-deftest test-shipit-close-pr-x-closes-without-a-comment ()
  "x closes the pull request without a comment when the editor stays empty."
  ;; GIVEN the page of my open pull request #12
  ;; WHEN I press SPC x, leave the editor empty, press C-c C-c and answer
  ;;   yes
  ;; THEN shipit asked "Close PR #12 without a comment?"
  ;; AND it sent GitHub the state closed and no comment
  ;; AND the page shows #12 as closed
  (test-close-pr--on-github "me" "open"
    (let ((page (test-page-views--open-pr 12)))
      (should-not (test-close-pr--press-x page))
      (should (string-match-p "\\`Close PR #12 without a comment\\?"
                              (or (test-close-pr--send "" t) "")))
      (should (equal '((state "closed")) (test-close-pr--sent)))
      (should (equal "closed" (test-close-pr--state-shown page))))))

(ert-deftest test-shipit-close-pr-no-keeps-the-comment ()
  "No to the question keeps the pull request open and the comment in the editor."
  ;; GIVEN the page of my open pull request #12
  ;; WHEN I press SPC x, write "Not needed any more", press C-c C-c and
  ;;   answer no
  ;; THEN shipit sent GitHub nothing about #12
  ;; AND the page shows #12 as open
  ;; AND the editor is still open, with "Not needed any more" in it
  (test-close-pr--on-github "me" "open"
    (let ((page (test-page-views--open-pr 12)))
      (should-not (test-close-pr--press-x page))
      (test-close-pr--send "Not needed any more" nil)
      (should-not (test-close-pr--sent))
      (should (equal "open" (test-close-pr--state-shown page)))
      (should (test-close-pr--editor))
      (should (string-match-p "Not needed any more"
                              (with-current-buffer (test-close-pr--editor)
                                (buffer-string)))))))

(ert-deftest test-shipit-close-pr-cancel-leaves-it-open ()
  "C-c C-k in the editor leaves the pull request open."
  ;; GIVEN the page of my open pull request #12
  ;; WHEN I press SPC x and then C-c C-k in the editor
  ;; THEN shipit sent GitHub nothing about #12
  ;; AND the page shows #12 as open
  ;; AND the editor is gone
  (test-close-pr--on-github "me" "open"
    (let ((page (test-page-views--open-pr 12)))
      (should-not (test-close-pr--press-x page))
      (test-close-pr--cancel)
      (should-not (test-close-pr--sent))
      (should (equal "open" (test-close-pr--state-shown page)))
      (should-not (test-close-pr--editor)))))

(ert-deftest test-shipit-close-pr-refuses-a-pull-request-of-another-author ()
  "x refuses a pull request by another author."
  ;; GIVEN the page of the open pull request #12 by alice
  ;; WHEN I press SPC x
  ;; THEN shipit says "Only the PR author can close this PR"
  ;; AND no editor opens
  (test-close-pr--on-github "alice" "open"
    (let ((page (test-page-views--open-pr 12)))
      (should (equal "Only the PR author can close this PR"
                     (test-close-pr--press-x page)))
      (should-not (test-close-pr--editor)))))

(ert-deftest test-shipit-close-pr-refuses-a-closed-pull-request ()
  "x refuses a pull request that is closed already."
  ;; GIVEN the page of my closed pull request #12
  ;; WHEN I press SPC x
  ;; THEN shipit says "PR #12 is already closed"
  ;; AND no editor opens
  (test-close-pr--on-github "me" "closed"
    (let ((page (test-page-views--open-pr 12)))
      (should (equal "PR #12 is already closed" (test-close-pr--press-x page)))
      (should-not (test-close-pr--editor)))))

(ert-deftest test-shipit-close-pr-refuses-a-merged-pull-request ()
  "x refuses a merged pull request."
  ;; GIVEN the page of my merged pull request #12
  ;; WHEN I press SPC x
  ;; THEN shipit says "PR #12 is merged — state cannot be changed"
  ;; AND no editor opens
  (test-close-pr--on-github "me" "merged"
    (let ((page (test-page-views--open-pr 12)))
      (should (equal "PR #12 is merged — state cannot be changed"
                     (test-close-pr--press-x page)))
      (should-not (test-close-pr--editor)))))

(ert-deftest test-shipit-close-pr-x-in-the-page-opens-the-editor ()
  "x in the page, without the menu, starts the close too."
  ;; GIVEN the page of my open pull request #12
  ;; WHEN I press x in the page, without the menu
  ;; THEN the editor opens for the comment of the close
  ;; AND x in the menu runs the same command as x in the page
  (test-close-pr--on-github "me" "open"
    (let ((page (test-page-views--open-pr 12)))
      (set-window-buffer (selected-window) page)
      (with-current-buffer page
        (goto-char (point-min))
        (should (eq (key-binding (kbd "x"))
                    (plist-get (cdr (transient-get-suffix 'shipit-pr-menu "x"))
                               :command)))
        (execute-kbd-macro (kbd "x")))
      (should (test-close-pr--editor))
      (should (eq 'close-pr-comment
                  (buffer-local-value 'shipit-editor--type (test-close-pr--editor)))))))

(provide 'test-shipit-close-pr)
;;; test-shipit-close-pr.el ends here
