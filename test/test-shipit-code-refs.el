;;; test-shipit-code-refs.el --- Tests for issue references in code -*- lexical-binding: t; -*-

;;; Commentary:
;; The browse URL for an issue key must come from the repository the
;; buffer is about, not from `default-directory': a shipit buffer's
;; directory is wherever Emacs happened to be, and resolving through
;; it sent a Jira key to the GitHub issue tracker.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'browse-url)
(require 'shipit-core)
(require 'shipit-issue-backends)
(require 'shipit-issues-buffer)
(require 'shipit-code-refs)

(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)

(defconst test-code-refs--jira
  (list :name "Jira"
        :browse-url (lambda (config key)
                      (format "%s/browse/%s" (plist-get config :base-url) key)))
  "A stand-in issue backend that builds Jira-shaped browse URLs.")

(defconst test-code-refs--github
  (list :name "GitHub"
        :browse-url (lambda (config key)
                      (format "https://github.com/%s/issues/%s"
                              (plist-get config :repo) key)))
  "A stand-in for the default backend a repo falls back to.")

(defmacro test-code-refs--with-backends (&rest body)
  "Run BODY with a Jira entry for acme repos and a GitHub default."
  (declare (indent 0))
  `(let ((shipit-issue-backends (list (cons 'jira test-code-refs--jira)
                                      (cons 'github test-code-refs--github)))
         (shipit-issue-backend 'github)
         (shipit-issue-backend-config nil)
         (shipit-issue-repo-backends
          '(("acme/.*" :backend jira
             :base-url "https://acme.atlassian.net"
             :project-keys ("PRJ" "OTH")))))
     ,@body))

(ert-deftest test-code-refs-url-uses-the-buffer-repo-not-the-directory ()
  "GIVEN a buffer about a Jira-backed repository
AND a `default-directory' whose remote names another repository
WHEN the browse URL for a key is built
THEN the buffer's repository decides, and the key goes to Jira."
  (test-code-refs--with-backends
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "someone/elsewhere")))
      (with-temp-buffer
        (setq-local shipit-buffer-repo "acme/product")
        (should (equal "https://acme.atlassian.net/browse/PRJ-11844"
                       (shipit-code-refs--browse-url "PRJ-11844")))))))

(ert-deftest test-code-refs-row-repo-outranks-the-buffer-repo ()
  "GIVEN a row carrying its own repository as a text property
WHEN the browse URL is built at that row
THEN the row's repository decides, since one buffer can list many."
  (test-code-refs--with-backends
    (with-temp-buffer
      (setq-local shipit-buffer-repo "someone/elsewhere")
      (insert (propertize "x" 'shipit-repo "acme/product"))
      (goto-char (point-min))
      (should (equal "https://acme.atlassian.net/browse/PRJ-11844"
                     (shipit-code-refs--browse-url "PRJ-11844"))))))

(ert-deftest test-code-refs-key-finds-its-backend-when-the-repo-does-not-claim-it ()
  "GIVEN a buffer whose repository resolves to a backend that does not
claim the key's project
AND a configured backend whose project keys do claim it
WHEN the browse URL is built
THEN the key's own backend answers, rather than the key being sent to
an issue tracker that never heard of it."
  (test-code-refs--with-backends
    (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
               (lambda () "someone/elsewhere")))
      (with-temp-buffer
        (should (equal "https://acme.atlassian.net/browse/PRJ-11844"
                       (shipit-code-refs--browse-url "PRJ-11844")))))))

(ert-deftest test-code-refs-repo-for-buffer-order ()
  "GIVEN every source of a buffer's repository at once
THEN the row's text property wins, then the buffer's own variable,
and the directory's remote only answers when nothing else does."
  (cl-letf (((symbol-function 'shipit--get-repo-from-remote)
             (lambda () "cwd/repo")))
    (with-temp-buffer
      (should (equal "cwd/repo" (shipit--repo-for-buffer)))
      (setq-local shipit-preview--repo "preview/repo")
      (should (equal "preview/repo" (shipit--repo-for-buffer)))
      (setq-local shipit-buffer-repo "buffer/repo")
      (should (equal "buffer/repo" (shipit--repo-for-buffer)))
      (insert (propertize "x" 'shipit-repo "row/repo"))
      (goto-char (point-min))
      (should (equal "row/repo" (shipit--repo-for-buffer))))))

(ert-deftest test-code-refs-in-comment-p-does-not-move-point ()
  "GIVEN a position somewhere in the buffer
WHEN `shipit-code-refs--in-comment-p' classifies it
THEN point is left where it was.
A `...-p' predicate must not move point: the `syntax-ppss' fallback
otherwise leaves point at the queried spot, and a font-lock matcher
looping over a rejected match then re-matches the same key forever."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; a comment mentioning PRJ-1\n")
    (insert "(setq x \"PRJ-2 in a string\")\n")
    (goto-char (point-max))
    (let ((origin (point)))
      (shipit-code-refs--in-comment-p 5)   ; a spot inside the comment
      (should (= (point) origin))
      (shipit-code-refs--in-comment-p 35)  ; a spot inside the code
      (should (= (point) origin)))))

(ert-deftest test-code-refs-matcher-advances-past-a-rejected-key ()
  "GIVEN a predicate that (like `syntax-ppss') leaves point at the spot
it was asked about and rejects the match
WHEN the font-lock matcher scans past that key
THEN it advances instead of re-matching the same key forever.
The stub caps its own call count so a regression fails loudly rather
than hanging the suite."
  (with-temp-buffer
    (insert "value PRJ-123 here\n")
    (goto-char (point-min))
    (let ((calls 0)
          (shipit-code-refs-comments-only t)
          (shipit-code-refs-restrict-to-project-keys nil))
      (cl-letf (((symbol-function 'shipit-code-refs--in-comment-p)
                 (lambda (pos)
                   (setq calls (1+ calls))
                   (when (> calls 50)
                     (error "matcher looped: in-comment-p called %d times"
                            calls))
                   (goto-char pos)   ; reproduce syntax-ppss's point move
                   nil)))            ; and reject: not a comment
        (should-not (shipit-code-refs--matcher (point-max)))
        (should (<= calls 1))))))

;;; Embark actions on an issue key

(defmacro test-code-refs--in-door-buffer (text &rest body)
  "Run BODY in a buffer about acme/door that holds TEXT, with point at its start.
The repository has the Jira backend of `test-code-refs--with-backends'."
  (declare (indent 1))
  `(test-code-refs--with-backends
     (with-temp-buffer
       (setq-local shipit-buffer-repo "acme/door")
       (insert ,text)
       (goto-char (point-min))
       ,@body)))

(defmacro test-code-refs--with-own-kill-ring (&rest body)
  "Run BODY with an empty kill ring of its own.
The clipboard of the system is left alone."
  (declare (indent 0))
  `(let ((kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil)
         (interprogram-paste-function nil))
     ,@body))

(defun test-code-refs--act (key answer)
  "Run the embark action on KEY, and type ANSWER and RET at its prompt.
This is how embark runs an action: as a command, with the target typed
at the prompt of the command."
  (ert-simulate-keys (append (string-to-list answer) '(?\r))
    (call-interactively (lookup-key shipit-code-refs-embark-keymap (kbd key)))))

(defun test-code-refs--copied-by (key answer)
  "What the embark action on KEY puts in the kill ring when given ANSWER."
  (test-code-refs--with-own-kill-ring
    (test-code-refs--act key answer)
    (car kill-ring)))

(defun test-code-refs--browsed-by (key answer)
  "The addresses that the embark action on KEY shows in the browser for ANSWER."
  (let* ((browsed nil)
         (browse-url-handlers nil)
         (browse-url-browser-function (lambda (url &rest _) (push url browsed))))
    (test-code-refs--act key answer)
    browsed))

(ert-deftest test-code-refs-embark-copy-key-takes-the-key-from-the-prompt ()
  "GIVEN a buffer with no issue key at point, as in the window that a
list of candidates came from
WHEN w in the embark actions of an issue key gets PRJ-95 at its prompt
THEN PRJ-95 is in the kill ring."
  (test-code-refs--in-door-buffer "no key here"
    (should (equal "PRJ-95" (test-code-refs--copied-by "w" "PRJ-95")))))

(ert-deftest test-code-refs-embark-target-wins-over-the-key-at-point ()
  "GIVEN a buffer with point on the issue key PRJ-1
WHEN w in the embark actions of an issue key gets PRJ-95 at its prompt
THEN PRJ-95 is copied, not the key at point."
  (test-code-refs--in-door-buffer "PRJ-1 is the key here"
    (should (equal "PRJ-95" (test-code-refs--copied-by "w" "PRJ-95")))))

(ert-deftest test-code-refs-embark-empty-answer-is-a-user-error ()
  "GIVEN a buffer with point on the issue key PRJ-1
WHEN w in the embark actions of an issue key gets an empty answer, and
then an answer of spaces only
THEN each is a user error, AND the key at point is not copied instead."
  (test-code-refs--in-door-buffer "PRJ-1 is the key here"
    (test-code-refs--with-own-kill-ring
      (should-error (test-code-refs--act "w" "") :type 'user-error)
      (should-error (test-code-refs--act "w" "   ") :type 'user-error)
      (should-not kill-ring))))

(ert-deftest test-code-refs-embark-copy-url-takes-the-key-from-the-prompt ()
  "GIVEN a buffer of a Jira-backed repository with point on PRJ-1
WHEN u in the embark actions of an issue key gets PRJ-95 at its prompt
THEN the Jira address of PRJ-95 is in the kill ring."
  (test-code-refs--in-door-buffer "PRJ-1 is the key here"
    (should (equal "https://acme.atlassian.net/browse/PRJ-95"
                   (test-code-refs--copied-by "u" "PRJ-95")))))

(ert-deftest test-code-refs-embark-browse-takes-the-key-from-the-prompt ()
  "GIVEN a buffer of a Jira-backed repository with point on PRJ-1
WHEN b in the embark actions of an issue key gets PRJ-95 at its prompt
THEN the browser shows the Jira page of PRJ-95, and only that."
  (test-code-refs--in-door-buffer "PRJ-1 is the key here"
    (should (equal '("https://acme.atlassian.net/browse/PRJ-95")
                   (test-code-refs--browsed-by "b" "PRJ-95")))))

(ert-deftest test-code-refs-embark-open-takes-the-key-from-the-prompt ()
  "GIVEN a buffer of acme/door with no issue key at point
AND the issue PRJ-95 of acme/door already open in a buffer of its own
WHEN o, and then RET, in the embark actions of an issue key gets PRJ-95
at its prompt
THEN each time the window shows the buffer of PRJ-95.
An issue buffer that is open already needs no request to the backend,
so the whole way to it runs as it is."
  (test-code-refs--in-door-buffer "no key here"
    (let ((issue (get-buffer-create
                  (shipit-issue-buffer-name "acme/door" "PRJ-95"))))
      (unwind-protect
          (dolist (key '("o" "RET"))
            (save-current-buffer
              (save-window-excursion
                (test-code-refs--act key "PRJ-95")
                (should (eq issue (window-buffer (selected-window)))))))
        (kill-buffer issue)))))

(ert-deftest test-code-refs-embark-offers-the-actions-on-an-issue-key ()
  "GIVEN embark with a target finder, a keymap and a default action of
its own
WHEN the issue keys register with embark
THEN embark asks for an issue key first, AND an issue key gets the
actions of `shipit-code-refs-embark-keymap', AND its default action
opens the issue, also in a list of candidates."
  (let ((embark-target-finders (list 'embark-target-url-at-point))
        (embark-keymap-alist (list '(file . embark-file-map)))
        (embark-default-action-overrides (list '(file . find-file))))
    (shipit-code-refs--embark-register)
    (should (eq 'shipit-code-refs-embark-target (car embark-target-finders)))
    (should (eq 'shipit-code-refs-embark-keymap
                (alist-get 'shipit-issue-ref embark-keymap-alist)))
    (should (eq 'shipit-code-refs-embark-open-issue
                (alist-get 'shipit-issue-ref embark-default-action-overrides)))))

(provide 'test-shipit-code-refs)
;;; test-shipit-code-refs.el ends here
