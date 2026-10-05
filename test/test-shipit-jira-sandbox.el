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
;; The editor is a `gfm-mode', which the suite stands in for, as the
;; editor tests do.
(load-file (expand-file-name "test-stubs.el" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shipit-http)
(require 'shipit-issue-jira)
(require 'shipit-jira-sandbox)
(require 'shipit-issues)
(require 'shipit-issues-buffer)
(require 'shipit-pr-github)
;; The backends of GitHub for issues and comments, which a page that was
;; opened from a pull request of GitHub is handed.
(require 'shipit-issue-github)
(require 'shipit-comment-github)
(require 'shipit-editor)
(require 'shipit-issue-create)
(require 'shipit-test-clipboard)
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

(defun test-jira-sandbox--file-bytes (file)
  "The bytes FILE holds."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun test-jira-sandbox--download (url)
  "The bytes shipit downloads from URL, as it downloads an image."
  (let ((file (shipit--download-and-cache-image url)))
    (should file)
    (test-jira-sandbox--file-bytes file)))

(ert-deftest test-jira-sandbox-image-downloads-with-the-login ()
  "GIVEN the sandbox, which answers only a request that logs in
WHEN shipit downloads the image of an issue
THEN it gets the picture, a PNG."
  (test-jira-sandbox--with config
    (let ((body (cdr (assq 'body (shipit-issue-jira--fetch-issue config "PRJ-1")))))
      (should (string-prefix-p "\x89PNG"
                               (test-jira-sandbox--download
                                (test-jira-sandbox--image-url body)))))))

(defun test-jira-sandbox--comment-body (config)
  "The body of the newest comment of the sandbox's first issue, as shown."
  (cdr (assq 'body (car (last (shipit-issue-jira--fetch-comments config "PRJ-1"))))))

(defun test-jira-sandbox--edit-from-page (comment)
  "Open the editor on COMMENT of the sandbox's first issue, as its page does."
  (with-temp-buffer
    (setq-local shipit-issue-buffer-number "PRJ-1")
    (shipit-issue--edit-comment-interactive
     shipit-jira-sandbox-repo (cdr (assq 'id comment)) (cdr (assq 'body comment)))))

(ert-deftest test-jira-sandbox-comment-edited-from-its-page ()
  "GIVEN a comment of the sandbox's first issue
WHEN it is edited from the issue's page and saved
THEN the comment says the new text.  The page knows a comment by its
own id, and Jira finds a comment only under its issue."
  (test-jira-sandbox--with config
    (test-jira-sandbox--edit-from-page
     (shipit-issue-jira--add-comment config "PRJ-1" "Before"))
    (with-current-buffer shipit-editor--buffer-name
      (erase-buffer)
      (insert "After")
      (shipit-editor-save))
    (should (equal "After" (test-jira-sandbox--comment-body config)))))

;;; Pasting a picture

(defconst test-jira-sandbox--png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")
  "A PNG of one pixel, as a paste gives it: bytes, not text.")

(defun test-jira-sandbox--images (body)
  "The images of BODY, markdown as shipit shows it, as (NAME . URL) pairs."
  (let ((images nil)
        (start 0))
    (while (string-match "!\\[\\([^]]*\\)\\](\\([^)]+\\))" body start)
      (push (cons (match-string 1 body) (match-string 2 body)) images)
      (setq start (match-end 0)))
    (nreverse images)))

(defun test-jira-sandbox--pasted-image (body)
  "The (NAME . URL) of the image pasted into BODY, or nil."
  (cl-find-if (lambda (image) (string-prefix-p "pasted-" (car image)))
              (test-jira-sandbox--images body)))

(defun test-jira-sandbox--embedded (node)
  "The names of the attachments NODE embeds, a document as Jira keeps it.
Read from what Jira keeps rather than from what shipit shows: markdown
sent as text comes back as the same text, which shows as an image too."
  (append (and (equal "media" (cdr (assq 'type node)))
               (list (cdr (assq 'alt (cdr (assq 'attrs node))))))
          (mapcan #'test-jira-sandbox--embedded
                  (append (cdr (assq 'content node)) nil))))

(defun test-jira-sandbox--kept-description (config &optional key)
  "The description of the issue KEY, the sandbox's first by default, as
Jira keeps it."
  (cdr (assq 'description
             (cdr (assq 'fields (shipit-issue-jira--api-request
                                 config (concat "/rest/api/3/issue/" (or key "PRJ-1"))))))))

(defun test-jira-sandbox--kept-comment (config)
  "The newest comment of the sandbox's first issue, as Jira keeps it."
  (cdr (assq 'body (car (last (append (cdr (assq 'comments (shipit-issue-jira--api-request
                                                            config "/rest/api/3/issue/PRJ-1/comment")))
                                      nil))))))

(defun test-jira-sandbox--pasted-p (names)
  "Non-nil when NAMES, of embedded attachments, has a pasted picture."
  (cl-some (lambda (name) (string-prefix-p "pasted-" name)) names))

(defun test-jira-sandbox--paste-and-save ()
  "In the open editor, write a line, paste the PNG, and save."
  (with-current-buffer shipit-editor--buffer-name
    (goto-char (point-max))
    (insert "Look at this: ")
    (shipit-test-clipboard-holding (list (cons 'image/png test-jira-sandbox--png))
      (yank-media))
    (shipit-editor-save)))

(defun test-jira-sandbox--description (config)
  "The description of the sandbox's first issue, as shipit shows it."
  (cdr (assq 'body (shipit-issue-jira--fetch-issue config "PRJ-1"))))

(ert-deftest test-jira-sandbox-upload-keeps-the-picture ()
  "GIVEN the sandbox
WHEN shipit attaches a picture to an issue under a name
THEN the attachment has that name, AND what it holds is the picture."
  (test-jira-sandbox--with config
    (let ((attachment (shipit-issue-jira--upload-attachment
                       config "PRJ-2" "shot.png" "image/png" test-jira-sandbox--png)))
      (should (equal "shot.png" (cdr (assq 'filename attachment))))
      (should (equal test-jira-sandbox--png
                     (test-jira-sandbox--download (cdr (assq 'url attachment))))))))

(ert-deftest test-jira-sandbox-description-keeps-its-image-when-saved ()
  "GIVEN the sandbox's first issue, whose description embeds an image
WHEN the description is saved as shipit shows it, unchanged
THEN Jira still embeds that image, rather than keeping text that looks
like markdown."
  (test-jira-sandbox--with config
    (shipit-issue-jira--update-description
     config "PRJ-1" (test-jira-sandbox--description config))
    (should (equal '("fake.png")
                   (test-jira-sandbox--embedded (test-jira-sandbox--kept-description config))))))

(ert-deftest test-jira-sandbox-paste-into-a-new-comment ()
  "GIVEN a new comment on the sandbox's first issue, and a picture on the
clipboard
WHEN it is pasted, and the comment saved
THEN Jira embeds the picture in the comment, after the text written
before it, AND the picture is what was pasted."
  (skip-unless (fboundp 'yank-media))
  (test-jira-sandbox--with config
    (shipit-editor-open
     (list :type 'issue-comment :repo shipit-jira-sandbox-repo :pr-number "PRJ-1"))
    (test-jira-sandbox--paste-and-save)
    (should (test-jira-sandbox--pasted-p
             (test-jira-sandbox--embedded (test-jira-sandbox--kept-comment config))))
    (let ((shown (test-jira-sandbox--comment-body config)))
      (should (string-match-p "Look at this" shown))
      (should (equal test-jira-sandbox--png
                     (test-jira-sandbox--download
                      (cdr (test-jira-sandbox--pasted-image shown))))))))

(ert-deftest test-jira-sandbox-paste-into-an-edited-comment ()
  "GIVEN a comment of the sandbox's first issue, edited from its page
AND a picture on the clipboard
WHEN it is pasted, and the comment saved
THEN Jira embeds the picture: editing a comment knows its issue."
  (skip-unless (fboundp 'yank-media))
  (test-jira-sandbox--with config
    (test-jira-sandbox--edit-from-page
     (shipit-issue-jira--add-comment config "PRJ-1" "Before"))
    (test-jira-sandbox--paste-and-save)
    (should (test-jira-sandbox--pasted-p
             (test-jira-sandbox--embedded (test-jira-sandbox--kept-comment config))))))

(ert-deftest test-jira-sandbox-paste-into-the-description ()
  "GIVEN the description of the sandbox's first issue in the editor
AND a picture on the clipboard
WHEN it is pasted, and the description saved
THEN Jira embeds the picture, AND the image the description had."
  (skip-unless (fboundp 'yank-media))
  (test-jira-sandbox--with config
    (shipit-editor-open
     (list :type 'issue-description :repo shipit-jira-sandbox-repo :pr-number "PRJ-1"
           :initial-content (test-jira-sandbox--description config)))
    (test-jira-sandbox--paste-and-save)
    (let ((names (test-jira-sandbox--embedded (test-jira-sandbox--kept-description config))))
      (should (member "fake.png" names))
      (should (test-jira-sandbox--pasted-p names)))))

(defmacro test-jira-sandbox--writing-new-issue (&rest body)
  "Run BODY in a buffer that writes a new issue of the sandbox, titled.
Its draft is not kept for a later test."
  (declare (indent 0))
  `(let ((shipit-issue-create--drafts (make-hash-table :test 'equal)))
     (with-current-buffer (shipit-issue-create-buffer shipit-jira-sandbox-repo
                                                      '(:title "A picture"))
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p (current-buffer))
           (remove-hook 'kill-buffer-hook #'shipit-issue-create--save-draft t)
           (kill-buffer))))))

(ert-deftest test-jira-sandbox-paste-into-a-new-issue ()
  "GIVEN a new issue being written, and a picture on the clipboard
WHEN it is pasted into the description, and the issue made
THEN the new issue embeds the picture after the text written before
it, AND the picture is what was pasted."
  (skip-unless (fboundp 'yank-media))
  (test-jira-sandbox--with config
    (let ((opened nil))
      (cl-letf (((symbol-function 'shipit-issues-open-buffer) (lambda (key &rest _) (setq opened key)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (test-jira-sandbox--writing-new-issue
          (shipit-issue-create--edit-description)
          (test-jira-sandbox--paste-and-save)
          (shipit-issue-create--submit)))
      (should opened)
      (should (test-jira-sandbox--pasted-p
               (test-jira-sandbox--embedded (test-jira-sandbox--kept-description config opened))))
      (let ((shown (cdr (assq 'body (shipit-issue-jira--fetch-issue config opened)))))
        (should (string-match-p "Look at this" shown))
        (should (equal test-jira-sandbox--png
                       (test-jira-sandbox--download
                        (cdr (test-jira-sandbox--pasted-image shown)))))))))

(ert-deftest test-jira-sandbox-a-picture-for-a-new-issue-shows-before-it-is-made ()
  "GIVEN a new issue being written, a picture on the clipboard, and a
display that shows images
WHEN it is pasted into the description, and the description saved
THEN the buffer of the new issue shows the picture itself."
  (skip-unless (fboundp 'yank-media))
  (test-jira-sandbox--with _config
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (test-jira-sandbox--writing-new-issue
        (shipit-issue-create--edit-description)
        (test-jira-sandbox--paste-and-save)
        (should (member test-jira-sandbox--png
                        (mapcar (lambda (image)
                                  (test-jira-sandbox--file-bytes (plist-get (cdr image) :file)))
                                (test-jira-sandbox--shown-images (current-buffer)))))))))

(defun test-jira-sandbox--shown-images (buffer)
  "The images BUFFER shows, as the display properties hold them."
  (with-current-buffer buffer
    (let ((images nil)
          (pos (point-min)))
      (while (< pos (point-max))
        (let ((display (get-text-property pos 'display)))
          (when (eq 'image (car-safe display))
            (push display images)))
        (setq pos (or (next-single-property-change pos 'display) (point-max))))
      images)))

(ert-deftest test-jira-sandbox-paste-shows-in-the-preview ()
  "GIVEN the page of the sandbox's first issue, a comment being written on
it, a picture on the clipboard, and a display that shows images
WHEN the picture is pasted, and the preview on the page updates
THEN the preview shows the picture itself, not the markdown for it."
  (skip-unless (and (fboundp 'yank-media) (executable-find "python3")))
  (let ((auth-sources nil))
    (test-jira-sandbox--with-page page
      (with-current-buffer page
        (shipit-editor-open (list :type 'issue-comment :source-buffer page
                                  :repo shipit-jira-sandbox-repo :pr-number "PRJ-1")))
      (unwind-protect
          (with-current-buffer shipit-editor--buffer-name
            (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
              (shipit-test-clipboard-holding (list (cons 'image/png test-jira-sandbox--png))
                (yank-media))
              (shipit-editor--update-preview (current-buffer)))
            (should (member test-jira-sandbox--png
                            (mapcar (lambda (image)
                                      (test-jira-sandbox--file-bytes (plist-get (cdr image) :file)))
                                    (test-jira-sandbox--shown-images page))))
            (should-not (with-current-buffer page
                          (string-match-p "!\\[pasted-" (buffer-string)))))
        (when (get-buffer shipit-editor--buffer-name)
          (with-current-buffer shipit-editor--buffer-name
            (shipit-editor-cancel)))))))

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

(defmacro test-jira-sandbox--with-page-opened-by (open page &rest body)
  "Run OPEN, a form that opens the sandbox's first issue; then BODY.
PAGE is bound to the buffer of the issue.  The sandbox stops only once
the page is whole, so no request of it is left to fail in a later test."
  (declare (indent 2))
  `(let ((,page nil))
     (unwind-protect
         (progn
           ,open
           (test-jira-sandbox--wait-until-ready)
           (setq ,page (get-buffer (shipit-issue-buffer-name shipit-jira-sandbox-repo "PRJ-1")))
           ,@body)
       (when (buffer-live-p ,page)
         (kill-buffer ,page))
       (shipit-jira-sandbox-close))))

(defmacro test-jira-sandbox--with-page (page &rest body)
  "Open the sandbox's first issue as by hand, bind its buffer to PAGE; run BODY."
  (declare (indent 1))
  `(test-jira-sandbox--with-page-opened-by (shipit-jira-sandbox-open) ,page
     ,@body))

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

(defun test-jira-sandbox--open-as-a-pull-request-does ()
  "Start the sandbox, give its first issue a comment, and open the issue.
It is opened as RET on its key in a pull request buffer opens it: with
the backend of the pull request handed over as the backend to use."
  (let ((config (shipit-jira-sandbox-start)))
    (shipit-issue-jira--add-comment config "PRJ-1" "A comment of the sandbox")
    (shipit-issues-open-buffer "PRJ-1" shipit-jira-sandbox-repo 'github nil)))

(ert-deftest test-jira-sandbox-a-key-opened-from-a-pull-request-shows-its-comments ()
  "GIVEN the sandbox, whose repository names Jira for its issues, AND a
comment on its first issue, AND a GitHub token
WHEN the issue is opened as RET on its key in a pull request buffer
opens it, which hands over the backend of the pull request
THEN the page reads through Jira, as the issue itself was fetched, AND
it shows the comment.  A page that took the backend of the pull request
asked GitHub for the reactions of the comments of Jira, and lost all of
its comments on the answer."
  (skip-unless (executable-find "python3"))
  (let ((auth-sources nil)
        (shipit-github-token "not-a-token")
        (shipit--github-token-cache nil))
    (test-jira-sandbox--with-page-opened-by
        (test-jira-sandbox--open-as-a-pull-request-does) page
      (with-current-buffer page
        (should (eq 'jira shipit-issue-backend))
        (should (string-match-p "A comment of the sandbox" (buffer-string)))))))

;;; The order of the comments

(defun test-jira-sandbox--open-with-comments (bodies)
  "Start the sandbox, give its first issue a comment for each of BODIES; open it."
  (let ((config (shipit-jira-sandbox-start)))
    (dolist (body bodies)
      (shipit-issue-jira--add-comment config "PRJ-1" body))
    (shipit-issues-open-buffer "PRJ-1" shipit-jira-sandbox-repo)))

(defun test-jira-sandbox--place-of (text)
  "Where TEXT stands in this buffer."
  (save-excursion
    (goto-char (point-min))
    (should (search-forward text nil t))
    (point)))

(defun test-jira-sandbox--as-shown (bodies)
  "BODIES, comments of this page, in the order that the page shows them."
  (sort (copy-sequence bodies)
        (lambda (a b)
          (< (test-jira-sandbox--place-of a) (test-jira-sandbox--place-of b)))))

(defconst test-jira-sandbox--three '("First said" "Second said" "Third said")
  "The bodies of three comments, in the order they are written in.")

(defmacro test-jira-sandbox--with-comments (bodies page &rest body)
  "Open the sandbox's first issue with a comment for each of BODIES; run BODY.
PAGE is bound to the buffer of the issue.  No login but the sandbox's is
there, and the order of comments is as it is set when the macro starts."
  (declare (indent 2))
  `(progn
     (skip-unless (executable-find "python3"))
     (let ((auth-sources nil)
           (shipit-comments-order shipit-comments-order))
       (test-jira-sandbox--with-page-opened-by
           (test-jira-sandbox--open-with-comments ,bodies) ,page
         ,@body))))

(ert-deftest test-jira-sandbox-comments-show-the-oldest-first ()
  "GIVEN an issue with three comments
WHEN its page is opened
THEN the comments stand in the order they were written in."
  (test-jira-sandbox--with-comments test-jira-sandbox--three page
    (with-current-buffer page
      (should (equal test-jira-sandbox--three
                     (test-jira-sandbox--as-shown test-jira-sandbox--three))))))

(ert-deftest test-jira-sandbox-comments-show-the-newest-first-when-asked ()
  "GIVEN an issue with three comments, AND the setting that asks for the
newest comment first
WHEN its page is opened
THEN the comments stand with the newest first."
  (let ((shipit-comments-order 'reverse-chronological))
    (test-jira-sandbox--with-comments test-jira-sandbox--three page
      (with-current-buffer page
        (should (equal (reverse test-jira-sandbox--three)
                       (test-jira-sandbox--as-shown test-jira-sandbox--three)))))))

(ert-deftest test-jira-sandbox-the-order-of-the-comments-is-toggled-on-the-page ()
  "GIVEN the page of an issue with three comments, the oldest first
WHEN the order of the section is toggled with point on a comment
THEN the comments stand with the newest first
WHEN it is toggled again
THEN the oldest is first again."
  (test-jira-sandbox--with-comments test-jira-sandbox--three page
    (with-current-buffer page
      (dolist (expected (list (reverse test-jira-sandbox--three) test-jira-sandbox--three))
        (goto-char (test-jira-sandbox--place-of "Second said"))
        (shipit-toggle-section-order)
        (test-jira-sandbox--wait-until-ready)
        (should (equal expected (test-jira-sandbox--as-shown test-jira-sandbox--three)))))))

(defconst test-jira-sandbox--five
  '("First said" "Second said" "Third said" "Fourth said" "Fifth said")
  "The bodies of five comments, in the order they are written in.")

(defun test-jira-sandbox--shown (bodies)
  "Those of BODIES that this buffer shows, in the order it shows them."
  (test-jira-sandbox--as-shown
   (seq-filter (lambda (body)
                 (save-excursion (goto-char (point-min)) (search-forward body nil t)))
               bodies)))

(defun test-jira-sandbox--load-the-rest ()
  "Load the comments that the line between the first and the last holds."
  (let ((line (cl-find 'issue-comments-load-more
                       (oref (shipit--find-section-by-type 'issue-comments) children)
                       :key (lambda (section) (oref section type)))))
    (should line)
    (goto-char (oref line start))
    (shipit-issue--load-more-comments)))

(ert-deftest test-jira-sandbox-comments-between-the-first-and-the-last-can-be-loaded ()
  "GIVEN an issue with five comments, AND a page that shows the first and
the last comment
WHEN its page is opened
THEN it shows those two, AND a line between them that loads the others
WHEN that line is used
THEN the page shows all five.  Jira gives all the comments at once, and
shipit keeps those that it does not show at first."
  (let ((shipit-comments-initial-count 1)
        (shipit-comments-tail-count 1)
        (shipit-comments-pagination-threshold 2))
    (test-jira-sandbox--with-comments test-jira-sandbox--five page
      (with-current-buffer page
        (should (equal '("First said" "Fifth said")
                       (test-jira-sandbox--shown test-jira-sandbox--five)))
        (test-jira-sandbox--load-the-rest)
        (should (equal test-jira-sandbox--five
                       (test-jira-sandbox--shown test-jira-sandbox--five)))))))

(ert-deftest test-jira-sandbox-paged-comments-keep-their-order ()
  "GIVEN an issue with five comments, AND a page that shows the first and
the last comment and loads the others when asked
WHEN its page is opened with the oldest first, and then with the newest
THEN the two comments stand in that order, with the line that loads the
others between them
WHEN the others are loaded
THEN all five stand in that order."
  (dolist (order '(chronological reverse-chronological))
    (let* ((shipit-comments-order order)
           (shipit-comments-initial-count 1)
           (shipit-comments-tail-count 1)
           (shipit-comments-pagination-threshold 2)
           (five (if (eq order 'chronological)
                     test-jira-sandbox--five
                   (reverse test-jira-sandbox--five))))
      (test-jira-sandbox--with-comments test-jira-sandbox--five page
        (with-current-buffer page
          (should (equal (list (car five) (car (last five)))
                         (test-jira-sandbox--shown test-jira-sandbox--five)))
          (test-jira-sandbox--load-the-rest)
          (should (equal five (test-jira-sandbox--shown test-jira-sandbox--five))))))))

(provide 'test-shipit-jira-sandbox)
;;; test-shipit-jira-sandbox.el ends here
