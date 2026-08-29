;;; test-shipit-requires.el --- Every file requires what it calls -*- lexical-binding: t; -*-

;;; Commentary:
;; A file that calls a shipit function from another module without
;; requiring it works for as long as something else happens to load
;; that module first.  It fails the moment load order changes: a fresh
;; session, a reload, a test that pulls in one module on its own.
;;
;; One file called `shipit--debug-log' for its whole life without
;; requiring `shipit-core'.  Nothing noticed until a test
;; loaded it alone and its error path ran.
;;
;; The byte-compiler already answers this, if it is asked in a process
;; that has preloaded nothing: it warns that the function is not known
;; to be defined.  Requiring the module silences it, and so does
;; `declare-function', which is the deliberate way to reach across a
;; layer without a require.  Either is a considered choice; neither is
;; an accident.

;;; Code:

(require 'ert)
(require 'seq)

(defconst test-requires--root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(defconst test-requires--lisp-dir
  (expand-file-name "lisp" test-requires--root)
  "Directory holding the shipit sources under test.")

(defun test-requires--source-files ()
  "Return the shipit source files to compile."
  (seq-remove (lambda (f) (string-match-p "autoloads" f))
              (directory-files test-requires--lisp-dir t "\\.el\\'")))

(defun test-requires--compile-isolated (file)
  "Byte-compile FILE in a subprocess that has preloaded nothing.
This is the condition a fresh session and a reload both create, and
the one the accident of load order hides."
  (let ((out (make-temp-file "shipit-requires" nil ".elc")))
    (unwind-protect
        (with-temp-buffer
          (call-process
           (expand-file-name invocation-name invocation-directory)
           nil t nil
           "-batch" "-Q"
           "-L" test-requires--lisp-dir
           "--eval" (prin1-to-string
                     `(setq package-user-dir
                            ,(expand-file-name ".packages" test-requires--root)))
           "--eval" "(package-initialize)"
           "--eval" (prin1-to-string `(byte-compile-file ,file ,out)))
          (buffer-string))
      (ignore-errors (delete-file out)))))

(defun test-requires--unknown-in (output file)
  "Return \"FILE:SYMBOL\" for each shipit function OUTPUT calls unknown."
  (let ((name (file-name-nondirectory file))
        (start 0)
        found)
    (while (string-match "the function .\\(shipit[a-zA-Z0-9-]*\\)." output start)
      (push (format "%s:%s" name (match-string 1 output)) found)
      (setq start (match-end 0)))
    (delete-dups found)))

(defvar test-requires--cache nil
  "Memoised scan.  Compiling every source alone is the expensive part,
and both tests ask the same question of it.")

(defun test-requires--current ()
  "Return every unmet shipit call across the sources, sorted."
  (or test-requires--cache
      (setq test-requires--cache (test-requires--scan))))

(defun test-requires--scan ()
  "Compile every source alone and collect the unmet calls."
  (sort (seq-mapcat
         (lambda (file)
           (test-requires--unknown-in
            (test-requires--compile-isolated file) file))
         (test-requires--source-files))
        #'string<))

(defun test-requires--baseline ()
  "Return the recorded unmet calls that predate this check."
  (let ((path (expand-file-name "test/fixtures/missing-requires-baseline.txt"
                                test-requires--root)))
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (seq-remove #'string-empty-p
                    (split-string (buffer-string) "\n" t))))))

(ert-deftest test-shipit-no-new-missing-requires ()
  "GIVEN every shipit source compiled alone, as a fresh session does
WHEN the compiler reports a shipit function it does not know
THEN that file is missing a require, unless it predates this check.

A hundred and forty such calls were already here when the check was
written and are listed in
`test/fixtures/missing-requires-baseline.txt'.  The list is there to
shrink.  A new one is a bug: it works until load order changes, then
raises `void-function' somewhere unrelated.  Fix it with a require,
or with `declare-function' when reaching across a layer on purpose."
  (let* ((known (test-requires--baseline))
         (new (seq-remove (lambda (entry) (member entry known))
                          (test-requires--current))))
    (should (equal '() new))))

(ert-deftest test-shipit-missing-requires-baseline-is-not-stale ()
  "GIVEN the baseline of unmet calls
WHEN the sources are compiled alone
THEN nothing on it is already fixed, so the list shrinks as files are
repaired instead of outliving the problem."
  (let* ((current (test-requires--current))
         (fixed (seq-remove (lambda (entry) (member entry current))
                            (test-requires--baseline))))
    (should (equal '() fixed))))

(provide 'test-shipit-requires)
;;; test-shipit-requires.el ends here
