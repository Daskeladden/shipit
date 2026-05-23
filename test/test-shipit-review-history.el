;;; test-shipit-review-history.el --- Tests for shipit-review-history -*- lexical-binding: t; -*-

;; Unit tests for the per-file timeline helpers.  Focus: making sure
;; `--pr-shas\\=' survives the REST-API edge case where
;; `pr-data.commits\\=' is the integer count rather than the actual
;; commit list -- the symptom is a process-sentinel crash with
;; `Wrong type argument: sequencep, 4\\='.

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-stubs)

(require 'shipit-core)
(require 'shipit-review-mode)
(require 'shipit-review-history)

(ert-deftest shipit-review--resolve-base-sha-prefers-reachable-pr-data-test ()
  "GIVEN pr-data.base.sha is reachable locally
WHEN `--resolve-base-sha' runs
THEN it returns the pr-data value (no merge-base fallback)."
  (cl-letf (((symbol-function 'shipit-review--sha-reachable-p)
             (lambda (_sha _worktree) t))
            ((symbol-function 'shipit--debug-log) (lambda (&rest _) nil)))
    (should (equal "deadbeefcafe"
                   (shipit-review--resolve-base-sha
                    '((base (sha . "deadbeefcafe") (ref . "master")))
                    "/tmp/whatever")))))

(ert-deftest shipit-review--resolve-base-sha-falls-back-when-pr-data-unreachable-test ()
  "GIVEN pr-data.base.sha is set but NOT reachable in the worktree
WHEN `--resolve-base-sha' runs
THEN it falls through to `merge-base HEAD origin/BASE-REF' instead of
handing back an unresolvable SHA."
  (cl-letf (((symbol-function 'shipit-review--sha-reachable-p)
             (lambda (_sha _worktree) nil))
            ((symbol-function 'shipit-review--merge-base-against)
             (lambda (ref _worktree)
               (and (equal ref "origin/master") "fallback-merge-base-sha")))
            ((symbol-function 'shipit--debug-log) (lambda (&rest _) nil))
            ((symbol-function 'file-directory-p) (lambda (_p) t)))
    (should (equal "fallback-merge-base-sha"
                   (shipit-review--resolve-base-sha
                    '((base (sha . "00000missing") (ref . "master")))
                    "/tmp/whatever")))))

(ert-deftest shipit-review--resolve-base-sha-returns-nil-when-all-fallbacks-fail-test ()
  "GIVEN pr-data.base.sha unreachable AND no origin/BASE-REF AND no origin/HEAD
WHEN `--resolve-base-sha' runs
THEN it returns nil so callers can surface the failure cleanly."
  (cl-letf (((symbol-function 'shipit-review--sha-reachable-p)
             (lambda (_sha _worktree) nil))
            ((symbol-function 'shipit-review--merge-base-against)
             (lambda (_ref _worktree) nil))
            ((symbol-function 'shipit--debug-log) (lambda (&rest _) nil))
            ((symbol-function 'file-directory-p) (lambda (_p) t)))
    (should-not (shipit-review--resolve-base-sha
                 '((base (sha . "00000missing") (ref . "main")))
                 "/tmp/whatever"))))

(ert-deftest shipit-review--pr-commits-returns-list-as-is-test ()
  "GIVEN pr-data.commits is a list
WHEN `shipit-review--pr-commits' is called
THEN the list is returned untouched."
  (with-temp-buffer
    (setq-local shipit-review--pr-data
                '((commits . (((sha . "aaa")) ((sha . "bbb")))) ))
    (should (equal '(((sha . "aaa")) ((sha . "bbb")))
                   (shipit-review--pr-commits)))))

(ert-deftest shipit-review--pr-commits-flattens-vector-test ()
  "GIVEN pr-data.commits is a vector (JSON-decoded shape)
WHEN `shipit-review--pr-commits' is called
THEN the vector is flattened to a list."
  (with-temp-buffer
    (setq-local shipit-review--pr-data
                `((commits . ,[((sha . "aaa")) ((sha . "bbb"))])))
    (should (equal '(((sha . "aaa")) ((sha . "bbb")))
                   (shipit-review--pr-commits)))))

(ert-deftest shipit-review--pr-commits-returns-nil-for-numeric-when-unrecoverable-test ()
  "GIVEN pr-data.commits is the integer count and git-log synthesis fails
WHEN `shipit-review--pr-commits' is called
THEN nil is returned (no `listp, 4' crash downstream)."
  (with-temp-buffer
    (setq-local shipit-review--pr-data '((commits . 4)))
    (cl-letf (((symbol-function 'shipit-review--git-commit-list)
               (lambda (&rest _) nil)))
      (should-not (shipit-review--pr-commits)))))

(ert-deftest shipit-review--pr-commits-recovers-numeric-via-git-log-test ()
  "GIVEN pr-data.commits is the integer count and git-log can rebuild it
WHEN `shipit-review--pr-commits' is called
THEN the synthesized list is returned."
  (with-temp-buffer
    (setq-local shipit-review--pr-data '((commits . 4)))
    (setq-local shipit-review--base-sha "0000000")
    (setq-local shipit-review--head-sha "ffffffff")
    (cl-letf (((symbol-function 'shipit-review--git-commit-list)
               (lambda (&rest _)
                 '(((sha . "ccc") (message . "x"))))))
      (should (equal '(((sha . "ccc") (message . "x")))
                     (shipit-review--pr-commits))))))

(ert-deftest shipit-review-history--pr-shas-handles-list-test ()
  "GIVEN pr-data.commits is the expected list of (sha . ...) alists
WHEN `--pr-shas' is called on the source buffer
THEN it returns the SHAs in order."
  (with-temp-buffer
    (setq-local shipit-review--pr-data
                '((number . 7)
                  (commits . (((sha . "aaaaaaa") (message . "first"))
                              ((sha . "bbbbbbb") (message . "second"))))))
    (should (equal '("aaaaaaa" "bbbbbbb")
                   (shipit-review-history--pr-shas (current-buffer))))))

(ert-deftest shipit-review-history--pr-shas-handles-numeric-commits-test ()
  "GIVEN pr-data.commits is left as the REST integer count
WHEN `--pr-shas' is called
THEN it returns nil instead of crashing with `sequencep, 4'."
  (with-temp-buffer
    (setq-local shipit-review--pr-data
                '((number . 7)
                  (commits . 4)))
    ;; Stub the worktree synthesis so the helper can\='t recover the
    ;; list from `git log\\='; we want to prove the guard alone is enough.
    (cl-letf (((symbol-function 'shipit-review--git-commit-list)
               (lambda (&rest _) nil)))
      (should-not (shipit-review-history--pr-shas (current-buffer))))))

(ert-deftest shipit-review-history--pr-shas-recovers-via-ensure-commits-list-test ()
  "GIVEN pr-data.commits is the integer count but git-log can rebuild the list
WHEN `--pr-shas' is called
THEN it returns the SHAs synthesized via `--ensure-commits-list'."
  (with-temp-buffer
    (setq-local shipit-review--pr-data
                '((number . 7)
                  (commits . 4)
                  (base . ((sha . "0000000")))))
    (setq-local shipit-review--base-sha "0000000")
    (setq-local shipit-review--head-sha "ffffffff")
    (cl-letf (((symbol-function 'shipit-review--git-commit-list)
               (lambda (&rest _)
                 '(((sha . "cccccc1") (message . "x"))
                   ((sha . "cccccc2") (message . "y"))))))
      (should (equal '("cccccc1" "cccccc2")
                     (shipit-review-history--pr-shas (current-buffer)))))))

(provide 'test-shipit-review-history)
;;; test-shipit-review-history.el ends here
