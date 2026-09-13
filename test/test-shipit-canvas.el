;;; test-shipit-canvas.el --- Canvas support on any Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:
;; shipit draws some views on the Emacs 32 canvas through canvas-diagram,
;; and loads on any Emacs it supports.  The first version of that check
;; guarded only a missing canvas-diagram.  On Emacs 29, with
;; canvas-diagram checked out beside shipit, its module could not
;; initialise, the error went straight through `require', and shipit
;; stopped compiling.  These tests keep it from coming back on an Emacs
;; 32 run: a stand-in canvas-diagram fails to load the way that module
;; did.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-canvas)

(defconst test-canvas--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(defun test-canvas--broken-diagram-dir ()
  "Return a directory holding a canvas-diagram that fails while loading.
It fails the way canvas-cairo did on Emacs 29: the file is found, and
loading it signals, which `require' with NOERROR does not catch."
  (let ((dir (make-temp-file "shipit-broken-canvas-" t)))
    (with-temp-file (expand-file-name "canvas-diagram.el" dir)
      (insert ";;; canvas-diagram.el --- fails to load -*- lexical-binding: t -*-\n"
              "(signal 'module-init-failed (list \"canvas-cairo.so\" 2))\n"
              "(provide 'canvas-diagram)\n"))
    dir))

(defun test-canvas--run-emacs (load-path-dirs form)
  "Run FORM in a fresh Emacs whose load path starts with LOAD-PATH-DIRS.
Return (EXIT-STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status
           (apply #'call-process
                  (expand-file-name invocation-name invocation-directory)
                  nil t nil
                  (append
                   '("-batch" "-Q")
                   (mapcan (lambda (dir) (list "-L" dir)) load-path-dirs)
                   (list "--eval" (prin1-to-string
                                   `(setq user-emacs-directory
                                          ,(file-name-as-directory
                                            (make-temp-file "shipit-canvas-home-" t))))
                         "--eval" (prin1-to-string
                                   `(setq package-user-dir
                                          ,(expand-file-name ".packages" test-canvas--root)))
                         "--eval" "(package-initialize)"
                         "--eval" (prin1-to-string form))))))
      (cons status (buffer-string)))))

;;;; The predicate

(ert-deftest test-canvas-unavailable-without-the-image-type ()
  "GIVEN an Emacs without the canvas image type
WHEN shipit asks whether it can draw on a canvas
THEN it says no without trying to load canvas-diagram at all."
  (let ((loaded nil))
    (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil))
              ((symbol-function 'shipit-canvas--load-diagram)
               (lambda () (setq loaded t) t)))
      (should-not (shipit-canvas-available-p))
      (should-not loaded))))

(ert-deftest test-canvas-unavailable-when-canvas-diagram-fails-to-load ()
  "GIVEN the canvas image type, and a canvas-diagram that signals as it loads
WHEN shipit asks whether it can draw on a canvas
THEN it says no instead of passing the error on."
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) t))
            ((symbol-function 'require)
             (lambda (feature &rest _)
               (if (eq feature 'canvas-diagram)
                   (signal 'module-init-failed '("canvas-cairo.so" 2))
                 t))))
    (should-not (shipit-canvas-available-p))))

(ert-deftest test-canvas-require-names-emacs-32-and-the-fallback ()
  "GIVEN an Emacs that cannot draw on a canvas
WHEN a canvas view insists on it
THEN the error names Emacs 32 and the view to use instead."
  (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) nil)))
    (let ((raised (condition-case err
                      (shipit-canvas-require "The kanban" "shipit-atlassian-dashboard")
                    (user-error (error-message-string err)))))
      (should (string-match-p "Emacs 32" raised))
      (should (string-match-p "shipit-atlassian-dashboard" raised)))))

;;;; With a canvas-diagram that cannot load

(ert-deftest test-canvas-kanban-loads-beside-a-broken-canvas-diagram ()
  "GIVEN a canvas-diagram on the load path that fails while loading
WHEN shipit's kanban is loaded and asked whether it can draw
THEN it loads, and says it cannot, rather than signalling."
  (let ((result (test-canvas--run-emacs
                 (list (expand-file-name "lisp" test-canvas--root)
                       (test-canvas--broken-diagram-dir))
                 '(progn
                    (require 'shipit-atlassian-kanban)
                    (princ (format "available=%S" (shipit-canvas-available-p)))))))
    (should (equal 0 (car result)))
    (should (string-match-p "available=nil" (cdr result)))))

(ert-deftest test-canvas-kanban-compiles-beside-a-broken-canvas-diagram ()
  "GIVEN a canvas-diagram on the load path that fails while loading
WHEN shipit's kanban is byte-compiled
THEN it compiles.

This is the break that stopped shipit compiling on Emacs 29."
  (let* ((out (make-temp-file "shipit-kanban-" nil ".elc"))
         (result (test-canvas--run-emacs
                  (list (expand-file-name "lisp" test-canvas--root)
                        (test-canvas--broken-diagram-dir))
                  `(progn
                     ;; Loaded first: binding its variable before it loads
                     ;; makes the binding lexical, and loading it then signals.
                     (require 'bytecomp)
                     (let ((byte-compile-dest-file-function (lambda (_) ,out)))
                     (kill-emacs
                      (if (byte-compile-file
                           ,(expand-file-name "lisp/shipit-atlassian-kanban.el"
                                              test-canvas--root))
                          0 1)))))))
    (unwind-protect
        (progn
          (should (equal 0 (car result)))
          (should-not (string-match-p "Error:" (cdr result))))
      (ignore-errors (delete-file out)))))

(ert-deftest test-canvas-dashboard-check-survives-a-broken-canvas-diagram ()
  "GIVEN a canvas-diagram on the load path that fails while loading
WHEN the dashboard asks whether a board can be drawn on a canvas
THEN it says no, so RET on a board heading folds it instead of signalling."
  (let ((result (test-canvas--run-emacs
                 (list (expand-file-name "lisp" test-canvas--root)
                       (test-canvas--broken-diagram-dir))
                 '(progn
                    (require 'shipit-atlassian-dashboard)
                    (princ (format "drawable=%S"
                                   (shipit-atlassian-dashboard--canvas-available-p)))))))
    (should (equal 0 (car result)))
    (should (string-match-p "drawable=nil" (cdr result)))))

(provide 'test-shipit-canvas)
;;; test-shipit-canvas.el ends here
