;;; shipit-test-offline.el --- Keep the test suite off the network -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The Makefile loads this before any test.  It refuses every
;; connection to another machine, so no test reaches api.github.com or
;; any other real service.  Tests that did passed or failed with the
;; network of the day, and on Emacs 29 a TLS connection one left half
;; open could hold a later test inside its wait for minutes.
;;
;; The refusal is an error where the connection is opened.  Code that
;; catches errors would hide it there, so a test that tried also fails
;; when it ends, whatever else it did.
;;
;; Two programs shipit runs reach remotes by themselves: curl, for
;; GitLab, which is refused the same way, and git.  Git is told to take
;; no transport but local files, so a fetch from GitHub fails at once
;; rather than going out with the git login of whoever runs the suite.

;;; Code:

(require 'ert)
(require 'seq)

(define-error 'shipit-test-offline "A test tried to reach another machine")

(defconst shipit-test-offline-hosts '("127.0.0.1" "localhost" "::1")
  "The hosts a test may connect to: this machine.")

(defvar shipit-test-offline--attempts nil
  "Where the running test tried to reach: HOST:SERVICE, or a URL for curl.")

(defun shipit-test-offline--local-p (args)
  "Non-nil when ARGS of `make-network-process' stay on this machine.
A server only listens, and no host means a local socket."
  (let ((host (plist-get args :host)))
    (or (plist-get args :server)
        (memq host '(nil local))
        (member host shipit-test-offline-hosts))))

(defun shipit-test-offline--refuse-at (where)
  "Record WHERE the running test tried to reach, and signal."
  (push where shipit-test-offline--attempts)
  (signal 'shipit-test-offline (list where)))

(defun shipit-test-offline--refuse (&rest args)
  "Signal `shipit-test-offline' when ARGS connect to another machine.
ARGS are those of `make-network-process'."
  (unless (shipit-test-offline--local-p args)
    (shipit-test-offline--refuse-at
     (format "%s:%s" (plist-get args :host) (plist-get args :service)))))

(defun shipit-test-offline--remote-url (args)
  "The first of ARGS, given to a program, that is a URL off this machine."
  (seq-find (lambda (arg)
              (and (stringp arg)
                   (string-match "\\`[a-z]+://\\([^/:]+\\)" arg)
                   (not (member (match-string 1 arg) shipit-test-offline-hosts))))
            args))

(defun shipit-test-offline--refuse-curl (program args)
  "Signal when PROGRAM is curl and ARGS name a URL on another machine."
  (when (and (stringp program)
             (equal (file-name-nondirectory program) "curl"))
    (let ((url (shipit-test-offline--remote-url args)))
      (when url
        (shipit-test-offline--refuse-at url)))))

(defun shipit-test-offline--refuse-call-process (program &optional _infile _destination _display &rest args)
  "Refuse PROGRAM with ARGS when it is curl for another machine.
The other arguments are those of `call-process'."
  (shipit-test-offline--refuse-curl program args))

(defun shipit-test-offline--refuse-make-process (&rest plist)
  "Refuse the process PLIST describes when it is curl for another machine."
  (let ((command (plist-get plist :command)))
    (shipit-test-offline--refuse-curl (car command) (cdr command))))

(defun shipit-test-offline--failure (passed attempts)
  "The result that fails a test which PASSED after trying ATTEMPTS."
  (make-ert-test-failed
   :condition (list 'shipit-test-offline (reverse attempts))
   :backtrace nil
   :infos nil
   :messages (ert-test-result-messages passed)
   :should-forms (ert-test-result-should-forms passed)
   :duration (ert-test-result-duration passed)))

(defun shipit-test-offline--run-test (run test)
  "Call RUN on TEST, and fail TEST if it tried to reach another machine."
  (let* ((shipit-test-offline--attempts nil)
         (result (funcall run test)))
    (if (and shipit-test-offline--attempts (ert-test-passed-p result))
        (setf (ert-test-most-recent-result test)
              (shipit-test-offline--failure result shipit-test-offline--attempts))
      result)))

(advice-add 'make-network-process :before #'shipit-test-offline--refuse)
(advice-add 'call-process :before #'shipit-test-offline--refuse-call-process)
(advice-add 'make-process :before #'shipit-test-offline--refuse-make-process)
(setenv "GIT_ALLOW_PROTOCOL" "file")
(setenv "GIT_TERMINAL_PROMPT" "0")
(advice-add 'ert-run-test :around #'shipit-test-offline--run-test)

(provide 'shipit-test-offline)
;;; shipit-test-offline.el ends here
