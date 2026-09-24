;;; test-shipit-page-keys.el --- Keys the pull request and issue buffers share -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A pull request buffer and an issue buffer each show one item, and the
;; keys that mean the same thing in both come from one parent keymap.
;; These tests hold the two buffers to that: a shared key does the same
;; in both, and `o' opens the item of either in the browser.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-issue-backends)
(require 'shipit-buffer)
(require 'shipit-issues-buffer)

(defmacro test-page-keys--opening (opened &rest body)
  "Run BODY with the browser recording the address it opens in OPENED."
  (declare (indent 1))
  `(let ((,opened nil))
     (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq ,opened url))))
       ,@body)))

(defmacro test-page-keys--in-pull-request (&rest body)
  "Run BODY in a buffer of pull request 7 of o/r, on a forge that gives addresses."
  (declare (indent 0))
  `(let ((shipit-pr-backend 'test-page)
         (shipit-pr-backends
          (list (cons 'test-page
                      (list :name "Forge"
                            :browse-url (lambda (_config number)
                                          (format "https://forge.example/o/r/pull/%s" number)))))))
     (with-temp-buffer
       (shipit-mode)
       (setq shipit-buffer-repo "o/r"
             shipit-buffer-pr-number 7)
       ,@body)))

(defmacro test-page-keys--in-issue (&rest body)
  "Run BODY in a buffer of issue PRJ-3 of o/r, on a tracker that gives addresses."
  (declare (indent 0))
  `(let ((shipit-issue-backends (copy-alist shipit-issue-backends))
         (shipit-issue-repo-backends '(("o/r" :backend test-page))))
     (shipit-issue-register-backend
      'test-page
      (list :name "Tracker"
            :fetch-issue #'ignore :fetch-comments #'ignore
            :fetch-comments-async #'ignore :search #'ignore
            :create-issue #'ignore :reference-patterns #'ignore
            :id-to-string #'ignore :string-to-id #'ignore
            :browse-url (lambda (_config key) (format "https://tracker.example/browse/%s" key))))
     (with-temp-buffer
       (shipit-issue-mode)
       (setq shipit-issue-buffer-repo "o/r"
             shipit-issue-buffer-number "PRJ-3")
       ,@body)))

(ert-deftest test-page-keys-o-opens-the-pull-request ()
  "GIVEN a buffer of a pull request, on a forge that gives its address
WHEN o is pressed
THEN the browser opens the address of that pull request."
  (test-page-keys--in-pull-request
    (test-page-keys--opening opened
      (call-interactively (key-binding (kbd "o")))
      (should (equal "https://forge.example/o/r/pull/7" opened)))))

(ert-deftest test-page-keys-o-opens-the-issue ()
  "GIVEN a buffer of an issue, on a tracker that gives its address
WHEN o is pressed
THEN the browser opens the address of that issue."
  (test-page-keys--in-issue
    (test-page-keys--opening opened
      (call-interactively (key-binding (kbd "o")))
      (should (equal "https://tracker.example/browse/PRJ-3" opened)))))

(ert-deftest test-page-keys-o-says-when-there-is-no-address ()
  "GIVEN a buffer of an issue that has no number yet
WHEN o is pressed
THEN it says so, rather than opening nothing or signalling from deep inside."
  (test-page-keys--in-issue
    (setq shipit-issue-buffer-number nil)
    (should-error (call-interactively (key-binding (kbd "o"))) :type 'user-error)))

(ert-deftest test-page-keys-shared-keys-do-the-same-in-both ()
  "GIVEN a pull request buffer and an issue buffer
WHEN the keys they share are looked up in each
THEN each key runs the same command in both, the one of the shared map,
AND both keymaps take it as their parent."
  (let ((in-pull-request (test-page-keys--in-pull-request
                           (mapcar (lambda (key) (key-binding (kbd key)))
                                   '("o" "q" "$" "M-;" "w"))))
        (in-issue (test-page-keys--in-issue
                    (mapcar (lambda (key) (key-binding (kbd key)))
                            '("o" "q" "$" "M-;" "w")))))
    (should (equal in-pull-request in-issue))
    (should (equal in-pull-request
                   (mapcar (lambda (key) (lookup-key shipit-page-mode-map (kbd key)))
                           '("o" "q" "$" "M-;" "w"))))
    (should (eq shipit-page-mode-map (keymap-parent shipit-mode-map)))
    (should (eq shipit-page-mode-map (keymap-parent shipit-issue-mode-map)))))

(provide 'test-shipit-page-keys)
;;; test-shipit-page-keys.el ends here
