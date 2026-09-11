;;; test-shipit-gh-etag-exit.el --- A batch Emacs leaves the cache file alone -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The persistent ETag cache is written to disk when Emacs exits.  The
;; file holds more than ETags: which activities and comments have been
;; read lives there too.  A batch Emacs that loads shipit starts with an
;; empty cache, and saving it on the way out wrote that emptiness over
;; the session's file.  Every `make test' did it, and the running
;; session only hid the loss until it next started.

;;; Code:

(require 'ert)
(require 'shipit-gh-etag)

(ert-deftest test-sgee-batch-emacs-does-not-save-at-exit ()
  "GIVEN a batch Emacs, as every test run is
WHEN shipit's ETag layer is loaded
THEN nothing is set to write the cache file on exit, so the session's
cache, read state included, survives the run."
  (should noninteractive)
  (should-not (memq #'shipit-gh-etag--save-cache kill-emacs-hook)))

(ert-deftest test-sgee-tests-keep-to-a-home-of-their-own ()
  "GIVEN the test suite, run the way `make test' runs it
WHEN shipit decides where its per-user state lives
THEN it is not the real user directory.  Tests that mark things read
save the cache as they go, so the exit guard alone did not stop a run
from overwriting the session's file."
  (should-not (file-equal-p user-emacs-directory
                            (expand-file-name "~/.emacs.d/")))
  (should-not (string-prefix-p (file-truename (expand-file-name "~/.emacs.d/"))
                               (file-truename shipit-gh-etag--cache-file))))

(provide 'test-shipit-gh-etag-exit)
;;; test-shipit-gh-etag-exit.el ends here
