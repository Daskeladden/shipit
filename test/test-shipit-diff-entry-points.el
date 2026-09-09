;;; test-shipit-diff-entry-points.el --- What the diff commands do at point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; The commands behind the `d' menu and `e' read their revisions from
;; the text properties under point.  Two things they have to get right:
;;
;;   * Difftastic is a separate binary.  Its package loads without it,
;;     leaving `difftastic-executable' nil, and the failure then
;;     surfaces far away as "Wrong type argument: stringp, nil" out of
;;     `process-lines'.  Say so here instead.
;;
;;   * Content that is a comparison in its own right, such as the two
;;     heads of a force push, carries its own revisions.  Diffing the
;;     pull request's base and head there shows the wrong change.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-pr-diff)
(require 'shipit-pr-github)

(defvar difftastic-executable nil
  "Declared special so the tests can bind it.
Difftastic itself is not loaded here, and a plain `let' on an unbound
symbol would only bind it lexically, which the code under test cannot
see.")

;;; The difftastic binary

(defmacro test-sdep--with-difftastic (executable &rest body)
  "Run BODY with difftastic loaded and its binary set to EXECUTABLE.
Errors signalled in BODY are available as `raised'."
  (declare (indent 1))
  `(let ((difftastic-executable ,executable)
         (raised nil))
     (ignore raised)
     (cl-letf (((symbol-function 'difftastic-files) (lambda (&rest _) nil)))
       (condition-case err (progn ,@body)
         (error (setq raised (error-message-string err))))
       ,@nil)
     raised))

(defmacro test-sdep--in-file-line (properties &rest body)
  "Run BODY with point on a line carrying PROPERTIES."
  (declare (indent 1))
  `(with-temp-buffer
     (insert (apply #'propertize "     +a line\n" ,properties))
     (goto-char (point-min))
     (forward-char 5)
     ,@body))

(defmacro test-sdep--with-git (present &rest body)
  "Run BODY with PRESENT the SHAs the local clone has, growing as it fetches.
The SHAs asked for are collected in BODY as `fetched\='."
  (declare (indent 1))
  `(let ((local ,present)
         (fetched nil))
     (ignore fetched)
     (cl-letf (((symbol-function 'shipit--sha-exists-locally-p)
                (lambda (sha) (and (member sha local) t)))
               ((symbol-function 'shipit--fetch-sha)
                (lambda (sha &optional _remote)
                  (push sha fetched)
                  (push sha local)
                  t)))
       ,@body)))

(ert-deftest test-sdep-difftastic-without-a-binary-says-which-setting ()
  "GIVEN difftastic loaded but `difftastic-executable' unset
WHEN the difftastic diff runs on a file
THEN it names the setting rather than failing inside `process-lines'."
  (let ((raised (test-sdep--with-difftastic nil
                  (test-sdep--in-file-line '(shipit-file-path "a.c")
                    (shipit--show-difftastic-diff-at-point)))))
    (should (string-match-p "difftastic-executable" raised))))

(ert-deftest test-sdep-difftastic-with-a-missing-binary-says-so ()
  "GIVEN `difftastic-executable' naming a binary that is not installed
WHEN the difftastic diff runs on a file
THEN it says the binary was not found."
  (let ((raised (test-sdep--with-difftastic "difft-that-is-not-installed"
                  (test-sdep--in-file-line '(shipit-file-path "a.c")
                    (shipit--show-difftastic-diff-at-point)))))
    (should (string-match-p "not found" raised))))

;;; Revisions carried by the content

(ert-deftest test-sdep-revisions-at-point-read-both-heads ()
  "GIVEN a line carrying an explicit base and head revision
WHEN the revisions at point are read
THEN both come back as a pair."
  (test-sdep--in-file-line '(shipit-base-sha "aaa" shipit-head-sha "bbb")
    (should (equal (shipit--diff-revisions-at-point) '("aaa" . "bbb")))))

(ert-deftest test-sdep-revisions-at-point-need-both ()
  "GIVEN a line carrying only one of the two revisions
WHEN the revisions at point are read
THEN nothing comes back, so the caller falls through to its own case."
  (test-sdep--in-file-line '(shipit-base-sha "aaa")
    (should (null (shipit--diff-revisions-at-point)))))

(ert-deftest test-sdep-plain-diff-uses-the-carried-revisions ()
  "GIVEN a file line inside a comparison that carries its own revisions
WHEN the plain diff runs
THEN it diffs those two and not the pull request's own base and head."
  (let ((ranges nil))
    (test-sdep--with-git (list "aaa" "bbb")
      (cl-letf (((symbol-function 'magit-diff-range)
                 (lambda (range &rest _) (push range ranges))))
        (test-sdep--in-file-line '(shipit-file-path "a.c"
                                   shipit-pr-number 7
                                   shipit-repo "o/r"
                                   shipit-base-sha "aaa"
                                   shipit-head-sha "bbb")
          (shipit--show-diff-at-point))))
    (should (equal ranges '("aaa..bbb")))))

(ert-deftest test-sdep-difftastic-uses-the-carried-revisions ()
  "GIVEN a file line inside a comparison that carries its own revisions
WHEN the difftastic diff runs
THEN it is handed those two revisions."
  (let ((calls nil))
    (test-sdep--with-git (list "aaa" "bbb")
      (cl-letf (((symbol-function 'shipit--difftastic-diff-shas)
                 (lambda (base head path &rest _) (push (list base head path) calls))))
        (test-sdep--with-difftastic "sh"
          (test-sdep--in-file-line '(shipit-file-path "a.c"
                                     shipit-pr-number 7
                                     shipit-repo "o/r"
                                     shipit-base-sha "aaa"
                                     shipit-head-sha "bbb")
            (shipit--show-difftastic-diff-at-point)))))
    (should (equal calls '(("aaa" "bbb" "a.c"))))))

(ert-deftest test-sdep-ediff-uses-the-carried-revisions ()
  "GIVEN a file line inside a comparison that carries its own revisions
WHEN ediff runs
THEN it compares those two revisions of the file."
  (let ((calls nil))
    (test-sdep--with-git (list "aaa" "bbb")
      (cl-letf (((symbol-function 'shipit--open-file-ediff)
                 (lambda (base head path &rest _) (push (list base head path) calls))))
        (test-sdep--in-file-line '(shipit-file-path "a.c"
                                   shipit-pr-number 7
                                   shipit-repo "o/r"
                                   shipit-base-sha "aaa"
                                   shipit-head-sha "bbb")
          (shipit--ediff-file-at-point))))
    (should (equal calls '(("aaa" "bbb" "a.c"))))))


;;; Fetching the revisions a comparison names

(ert-deftest test-sdep-missing-revisions-are-fetched-by-hash ()
  "GIVEN a comparison whose old head is not in the local clone
WHEN its revisions are made ready
THEN that one is fetched, because no ref points at a head a force push
moved off and a plain `git fetch' therefore does not bring it in."
  (let ((missing nil))
    (test-sdep--with-git (list "bbb")
      (setq missing (shipit--ensure-shas-local '("aaa" "bbb")))
      (should (equal fetched '("aaa"))))
    (should (null missing))))

(ert-deftest test-sdep-revisions-already-present-are-left-alone ()
  "GIVEN a comparison whose revisions are both in the local clone
WHEN its revisions are made ready
THEN nothing is fetched."
  (test-sdep--with-git (list "aaa" "bbb")
    (shipit--ensure-shas-local '("aaa" "bbb"))
    (should (null fetched))))

(ert-deftest test-sdep-unfetchable-revision-is-named ()
  "GIVEN a revision the remote will not serve
WHEN the diff commands make the comparison ready
THEN the error names that revision rather than the whole comparison."
  (let ((raised nil))
    (cl-letf (((symbol-function 'shipit--sha-exists-locally-p) (lambda (_) nil))
              ((symbol-function 'shipit--fetch-sha) (lambda (&rest _) nil)))
      (condition-case err
          (shipit--ensure-diff-revisions '("aaaaaaaaaaaa1111" . "bbb"))
        (error (setq raised (error-message-string err)))))
    (should (string-match-p "aaaaaaaaaaaa" raised))))

(ert-deftest test-sdep-fetch-uses-the-remote-the-backend-names ()
  "GIVEN a comparison in a repository that is not necessarily `origin\='
WHEN a missing revision is fetched
THEN the remote comes from the backend instead of being assumed."
  (let ((remotes nil)
        (shipit-pr-backend 'github))
    (cl-letf (((symbol-function 'shipit--sha-exists-locally-p) (lambda (_) nil))
              ((symbol-function 'shipit--fetch-sha)
               (lambda (_sha &optional remote) (push remote remotes) t)))
      (ignore-errors (shipit--ensure-diff-revisions '("aaa" . "bbb") "o/r")))
    (should (equal (car (last remotes)) "https://github.com/o/r.git"))))

(ert-deftest test-sdep-difftastic-fetches-before-diffing ()
  "GIVEN a force-push comparison whose old head is not local yet
WHEN the difftastic diff runs on it
THEN the old head is fetched and the two revisions are diffed."
  (let ((calls nil))
    (cl-letf (((symbol-function 'shipit--difftastic-diff-shas)
               (lambda (base head path &rest _) (push (list base head path) calls))))
      (test-sdep--with-git (list "bbb")
        (test-sdep--with-difftastic "sh"
          (test-sdep--in-file-line '(shipit-file-path "a.c"
                                     shipit-base-sha "aaa"
                                     shipit-head-sha "bbb")
            (shipit--show-difftastic-diff-at-point)))
        (should (equal fetched '("aaa")))))
    (should (equal calls '(("aaa" "bbb" "a.c"))))))

(provide 'test-shipit-diff-entry-points)
;;; test-shipit-diff-entry-points.el ends here
