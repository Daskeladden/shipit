;;; test-shipit-jira-sandbox.el --- shipit's Jira backend against a fake Jira -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The Jira sandbox runs test/fake-jira.py, a small stand-in for Jira
;; Cloud on this machine.  These tests drive shipit's Jira backend
;; through HTTP against it, as it drives a real site: the requests, the
;; login and the answers all go through url.el.  They skip themselves
;; where there is no python3.
;;
;; Nothing here may leave this machine or need a login of the person
;; running it, which the offline guard holds them to.

;;; Code:

(require 'ert)
(require 'shipit-http)
(require 'shipit-issue-jira)
(require 'shipit-jira-sandbox)
(require 'shipit-issues)
(require 'shipit-issues-buffer)
(require 'shipit-pr-github)
(require 'shipit-test-offline)

(defconst test-jira-sandbox--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(defun test-jira-sandbox--in-fresh-emacs (root &rest forms)
  "Evaluate FORMS in a fresh Emacs set up as by hand; what it printed.
That Emacs has the sources under ROOT on its load path, and not the
tests: a sandbox is loaded by its file.  It keeps its state in a
directory of its own, and reads no logins.  The output ends with the
exit code, 0 when every form worked."
  (with-temp-buffer
    (let ((exit (apply #'call-process
                       (expand-file-name invocation-name invocation-directory)
                       nil t nil
                       "-batch" "-Q"
                       "-L" (expand-file-name "lisp" root)
                       "--eval" (prin1-to-string
                                 `(setq package-user-dir
                                        ,(expand-file-name ".packages" test-jira-sandbox--root)
                                        user-emacs-directory
                                        (file-name-as-directory (make-temp-file "shipit-fresh-" t))
                                        auth-sources nil))
                       "--eval" "(package-initialize)"
                       (mapcan (lambda (form) (list "--eval" (prin1-to-string form)))
                               forms))))
      (format "%s\nexit %s" (buffer-string) exit))))

(defun test-jira-sandbox--load-form (root file)
  "The form that loads FILE, under ROOT, by its name."
  `(load-file ,(expand-file-name file root)))

(defconst test-jira-sandbox--sandbox-files
  '("test/shipit-jira-sandbox.el" "test/shipit-sandbox.el")
  "The sandboxes, which are loaded by their files.")

(defun test-jira-sandbox--copy-of-shipit ()
  "A directory holding a copy of shipit's sources and sandboxes.
`shipit-reload' deletes the compiled files of the sources it loads."
  (let ((copy (make-temp-file "shipit-copy-" t)))
    (copy-directory (expand-file-name "lisp" test-jira-sandbox--root)
                    (expand-file-name "lisp" copy))
    (make-directory (expand-file-name "test" copy))
    (dolist (file (append '("shipit-dev-utils.el" "test/shipit-quiet-backend.el")
                          test-jira-sandbox--sandbox-files))
      (copy-file (expand-file-name file test-jira-sandbox--root)
                 (expand-file-name file copy)))
    copy))

(ert-deftest test-jira-sandbox-sandboxes-load-by-file ()
  "GIVEN an Emacs whose load path has shipit's sources but not its tests
WHEN a sandbox is loaded by its file
THEN it loads, finding the helpers beside it by itself."
  (dolist (file test-jira-sandbox--sandbox-files)
    (should (string-suffix-p "exit 0" (test-jira-sandbox--in-fresh-emacs
                                       test-jira-sandbox--root
                                       (test-jira-sandbox--load-form
                                        test-jira-sandbox--root file))))))

(ert-deftest test-jira-sandbox-sandboxes-load-again-after-a-reload ()
  "GIVEN a sandbox loaded by its file, and then `shipit-reload', which
unbinds every shipit function and loads only the sources again
WHEN the sandbox is loaded by its file again
THEN its helpers are back too: they are not among the sources, and a
`require' would find them loaded already and skip them."
  (let ((copy (test-jira-sandbox--copy-of-shipit)))
    (unwind-protect
        (dolist (file test-jira-sandbox--sandbox-files)
          (should (string-match-p
                   "helper t"
                   (test-jira-sandbox--in-fresh-emacs
                    copy
                    (test-jira-sandbox--load-form copy file)
                    (test-jira-sandbox--load-form copy "shipit-dev-utils.el")
                    '(shipit-reload)
                    (test-jira-sandbox--load-form copy file)
                    '(princ (format "helper %S" (fboundp 'shipit-quiet-backend)))))))
      (delete-directory copy t))))

(defmacro test-jira-sandbox--with (config &rest body)
  "Start the Jira sandbox, bind CONFIG to shipit's config of it, run BODY.
The sandbox's login is the only one there is: a lookup in the logins of
the person running the suite decrypts their auth file, and with no
passphrase cached that waits for a prompt nobody answers."
  (declare (indent 1))
  `(progn
     (skip-unless (executable-find "python3"))
     (let ((auth-sources nil))
       (unwind-protect
           (let ((,config (shipit-jira-sandbox-start)))
             ,@body)
         (shipit-jira-sandbox-stop)))))

(defun test-jira-sandbox--image-url (body)
  "The URL of the first image of BODY, markdown as shipit shows it."
  (and (string-match "!\\[[^]]*\\](\\([^)]+\\))" body)
       (match-string 1 body)))

(ert-deftest test-jira-sandbox-issue-shows-its-description-and-image ()
  "GIVEN the sandbox, whose first issue holds an image it has attached
WHEN shipit fetches that issue
THEN it has its summary, AND its description shows the image from the
sandbox's attachment, as it shows one from Jira."
  (test-jira-sandbox--with config
    (let* ((issue (shipit-issue-jira--fetch-issue config "PRJ-1"))
           (url (test-jira-sandbox--image-url (cdr (assq 'body issue)))))
      (should (equal "A bug to try shipit on" (cdr (assq 'title issue))))
      (should url)
      (should (string-prefix-p (concat (plist-get config :base-url)
                                       "/rest/api/3/attachment/content/")
                               url))
      (should (string-suffix-p "?redirect=false" url)))))

(ert-deftest test-jira-sandbox-comment-written-is-read-back ()
  "GIVEN the sandbox
WHEN shipit adds a comment to an issue, and reads the comments
THEN the comment comes back as it was written, from the sandbox's user."
  (test-jira-sandbox--with config
    (let ((added (shipit-issue-jira--add-comment config "PRJ-1" "Hello from shipit")))
      (should (equal "Hello from shipit" (cdr (assq 'body added))))
      (let ((last (car (last (shipit-issue-jira--fetch-comments config "PRJ-1")))))
        (should (equal "Hello from shipit" (cdr (assq 'body last))))
        (should (equal shipit-jira-sandbox-user
                       (cdr (assq 'login (cdr (assq 'user last))))))))))

(ert-deftest test-jira-sandbox-description-written-is-read-back-with-its-image ()
  "GIVEN the sandbox, and wiki markup that embeds the issue's attachment
WHEN shipit writes it as the description, and fetches the issue
THEN the text comes back, AND the embedded attachment comes back as an
image, as Jira turns wiki markup into its document format."
  (test-jira-sandbox--with config
    (shipit-issue-jira--update-description config "PRJ-1" "New text\n\n!fake.png!")
    (let ((body (cdr (assq 'body (shipit-issue-jira--fetch-issue config "PRJ-1")))))
      (should (string-match-p "New text" body))
      (should (test-jira-sandbox--image-url body)))))

(ert-deftest test-jira-sandbox-image-downloads-with-the-login ()
  "GIVEN the sandbox, which answers only a request that logs in
WHEN shipit downloads the image of an issue
THEN it gets the picture, a PNG."
  (test-jira-sandbox--with config
    (let* ((body (cdr (assq 'body (shipit-issue-jira--fetch-issue config "PRJ-1"))))
           (file (shipit--download-and-cache-image (test-jira-sandbox--image-url body))))
      (should file)
      (should (string-prefix-p "\x89PNG"
                               (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally file)
                                 (buffer-string)))))))

(defun test-jira-sandbox--raw-status (config path)
  "The status the sandbox of CONFIG answers a GET of PATH with, sent bare.
Not through url.el: url.el adds by itself the login it finds in
auth-source, which holds the sandbox's."
  (let* ((url (url-generic-parse-url (plist-get config :base-url)))
         (answer "")
         (stream (open-network-stream "jira-sandbox-raw" nil (url-host url) (url-port url))))
    (unwind-protect
        (progn
          (set-process-filter stream (lambda (_process text) (setq answer (concat answer text))))
          (process-send-string stream (format "GET %s HTTP/1.0\r\nHost: %s\r\n\r\n"
                                              path (url-host url)))
          (while (and (process-live-p stream) (not (string-match-p "\r\n\r\n" answer)))
            (accept-process-output stream 0.1)))
      (delete-process stream))
    (and (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\)" answer)
         (string-to-number (match-string 1 answer)))))

(ert-deftest test-jira-sandbox-refuses-a-request-without-a-login ()
  "GIVEN the sandbox
WHEN it is asked who is logged in, without a login
THEN it answers 401, as Jira does."
  (test-jira-sandbox--with config
    (should (equal 401 (test-jira-sandbox--raw-status config "/rest/api/3/myself")))))

(defun test-jira-sandbox--wait-until-ready ()
  "Wait until an issue page says its comments are in, the last of it.
The hook is set by its default value: it is local to each buffer, so
`add-hook' from here would add to this buffer's alone."
  (let* ((ready nil)
         (before (default-value 'shipit-issue-buffer-ready-hook))
         (deadline (+ (float-time) 10)))
    (setf (default-value 'shipit-issue-buffer-ready-hook)
          (cons (lambda () (setq ready t)) before))
    (unwind-protect
        (while (and (not ready) (< (float-time) deadline))
          (accept-process-output nil 0.05))
      (setf (default-value 'shipit-issue-buffer-ready-hook) before))
    (should ready)))

(defmacro test-jira-sandbox--with-page (page &rest body)
  "Open the sandbox's first issue as by hand, bind its buffer to PAGE; run BODY.
The sandbox stops only once the page is whole, so no request of it is
left to fail in a later test."
  (declare (indent 1))
  `(let ((,page nil))
     (unwind-protect
         (progn
           (shipit-jira-sandbox-open)
           (test-jira-sandbox--wait-until-ready)
           (setq ,page (get-buffer (shipit-issue-buffer-name shipit-jira-sandbox-repo "PRJ-1")))
           ,@body)
       (when (buffer-live-p ,page)
         (kill-buffer ,page))
       (shipit-jira-sandbox-close))))

(defun test-jira-sandbox--page-text (token)
  "The text of the sandbox's first issue, opened as by hand with TOKEN.
TOKEN is the GitHub token shipit has, or nil for none."
  (let ((auth-sources nil)
        (shipit-github-token token)
        (shipit--github-token-cache nil))
    (test-jira-sandbox--with-page page
      (with-current-buffer page (buffer-string)))))

(defun test-jira-sandbox--whole-page-p (text)
  "Non-nil when TEXT has the sandbox issue's description and comments."
  (and (string-match-p "An issue of the sandbox" text)
       (string-match-p "No comments" text)))

(ert-deftest test-jira-sandbox-page-needs-no-github ()
  "GIVEN no GitHub token, and no way off this machine
WHEN the sandbox's first issue is opened as by hand
THEN the whole page shows, down to its comments, AND nothing asks for
a token or leaves the machine: the repository of the sandbox exists
nowhere else."
  (skip-unless (executable-find "python3"))
  (should (test-jira-sandbox--whole-page-p (test-jira-sandbox--page-text nil))))

(ert-deftest test-jira-sandbox-page-stays-here-with-a-github-token ()
  "GIVEN a GitHub token, as the Emacs shipit is used in has
WHEN the sandbox's first issue is opened as by hand
THEN the whole page shows all the same, AND nothing leaves the machine:
code that skips GitHub for want of a token would reach it with one."
  (skip-unless (executable-find "python3"))
  (should (test-jira-sandbox--whole-page-p (test-jira-sandbox--page-text "not-a-token"))))

(provide 'test-shipit-jira-sandbox)
;;; test-shipit-jira-sandbox.el ends here
