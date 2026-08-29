;;; shipit-stack-plan.el --- What publishing a stack would do -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Publishing a stack is worked out in full before any of it runs.  The
;; result is a list of steps, one per layer, and the dry run and the
;; submit are the same list read two ways.  A dry run that worked its
;; answer out separately would drift from what actually happens and
;; still be believed.
;;
;; Every step says what it would do rather than what it wants done, so
;; a layer that is already pushed and open reads as nothing to do.  That
;; is what lets a submit be re-run after failing part way: the work
;; already done drops out of the plan, and there is nothing to unwind.

;;; Code:

(require 'shipit-core)
(require 'shipit-stack)
(require 'shipit-pr-backends)

(defun shipit-stack-plan--pushed-p (branch)
  "Return non-nil when BRANCH exists on the remote."
  (and (shipit-stack--rev (concat "origin/" branch)) t))

(defun shipit-stack-plan--pr-for-branch (repo branch)
  "Return the number of the open pull request whose head is BRANCH."
  (let* ((resolved (shipit-pr--resolve-for-repo repo))
         (backend (car resolved))
         (fn (plist-get backend :fetch-pr-for-branch)))
    (when fn (funcall fn (cdr resolved) branch))))

(defun shipit-stack-plan--existing-stack (repo number)
  "Return the stack NUMBER in REPO already belongs to, or nil."
  (when number
    (cdr (assq 'number (shipit-pr--fetch-stack repo number)))))

(defun shipit-stack-plan--step (layer base repo blocked-above)
  "Return the step for LAYER over BASE in REPO.
BLOCKED-ABOVE carries a reason down from a lower layer: a layer whose
base cannot be published cannot be published either."
  (let* ((branch (cdr (assq 'branch layer)))
         (reason (cond
                  (blocked-above (format "waiting on %s" blocked-above))
                  ((cdr (assq 'ambiguous layer))
                   (format "more than one branch name (%s); pick the base first"
                           (string-join (cons branch (cdr (assq 'aliases layer)))
                                        ", "))))))
    `((branch . ,branch)
      (base . ,base)
      (commits . ,(cdr (assq 'commits layer)))
      (push . ,(if (shipit-stack-plan--pushed-p branch) 'done 'needed))
      (pr . ,(or (shipit-stack-plan--pr-for-branch repo branch) 'none))
      (blocked . ,reason))))

(defun shipit-stack-plan--registration (repo steps)
  "Return the registration step for STEPS in REPO.
Chained bases are not a stack on their own: GitHub validates the chain
but will not infer one, so without this the pull requests stay
unrelated in the web UI."
  (let* ((existing (seq-find #'identity
                             (mapcar (lambda (step)
                                       (let ((pr (cdr (assq 'pr step))))
                                         (and (integerp pr)
                                              (shipit-stack-plan--existing-stack repo pr))))
                                     steps)))
         (new (seq-count (lambda (step) (eq 'none (cdr (assq 'pr step)))) steps)))
    (cond
     ((zerop new) '((action . none)))
     (existing `((action . add) (stack . ,existing) (count . ,new)))
     (t `((action . create) (count . ,(length steps)))))))

(defun shipit-stack-plan (stack repo &optional cut)
  "Return what publishing STACK in REPO would do, up to layer CUT.
CUT is a stopping point, not a selection: a layer is based on the one
below, so the layers above a cut cannot be published without it."
  (let* ((layers (cdr (assq 'layers stack)))
         (layers (if cut (seq-take layers cut) layers))
         (base (cdr (assq 'base stack)))
         (blocked nil)
         steps)
    (dolist (layer layers)
      (let ((step (shipit-stack-plan--step layer base repo blocked)))
        (unless blocked
          (when (cdr (assq 'blocked step))
            (setq blocked (cdr (assq 'branch step)))))
        (setq base (cdr (assq 'branch layer)))
        (push step steps)))
    (setq steps (nreverse steps))
    `((base . ,(cdr (assq 'base stack)))
      (cut . ,(or cut (length layers)))
      (steps . ,steps)
      (registration . ,(shipit-stack-plan--registration repo steps)))))

(defun shipit-stack-plan-work-p (plan)
  "Return non-nil when PLAN would do anything at all."
  (or (seq-some (lambda (step)
                  (or (eq 'needed (cdr (assq 'push step)))
                      (eq 'none (cdr (assq 'pr step)))))
                (cdr (assq 'steps plan)))
      (not (eq 'none (cdr (assq 'action (cdr (assq 'registration plan))))))))

(defun shipit-stack-plan--render-step (step index width)
  "Return the lines describing STEP, numbered INDEX, names padded to WIDTH.
The width goes into the format string rather than through a padding
helper, so this stays free of the section rendering and the magit it
would bring with it."
  (let ((branch (cdr (assq 'branch step)))
        (pr (cdr (assq 'pr step)))
        (blocked (cdr (assq 'blocked step))))
    (cond
     (blocked
      (format (format "  %%d  %%-%ds  BLOCKED -- %%s\n" width)
              index branch blocked))
     (t
      (concat
       (format (format "  %%d  %%-%ds  %%s\n" width)
               index branch
               (if (eq 'needed (cdr (assq 'push step)))
                   "push new branch"
                 "already pushed"))
       (format "     %s  %s\n" (make-string width ?\s)
               (if (eq 'none pr)
                   (format "create pull request, base %s" (cdr (assq 'base step)))
                 (format "pull request #%s exists, no change" pr))))))))

(defun shipit-stack-plan--render-registration (registration)
  "Return the line describing REGISTRATION."
  (pcase (cdr (assq 'action registration))
    ('create (format "  -  register the stack from the %s pull requests above\n"
                     (cdr (assq 'count registration))))
    ('add (format "  -  add %s pull request(s) to stack #%s\n"
                  (cdr (assq 'count registration))
                  (cdr (assq 'stack registration))))
    (_ "  -  already a stack, nothing to register\n")))

(defun shipit-stack-plan-render (plan)
  "Return PLAN as text, for reading before anything is done."
  (let* ((steps (cdr (assq 'steps plan)))
         (width (apply #'max 8 (mapcar (lambda (s) (length (cdr (assq 'branch s))))
                                       steps)))
         (index 0))
    (concat
     (format "Submit plan -- %d layer(s) over %s\n\n"
             (length steps) (cdr (assq 'base plan)))
     (mapconcat (lambda (step)
                  (setq index (1+ index))
                  (shipit-stack-plan--render-step step index width))
                steps "")
     (shipit-stack-plan--render-registration (cdr (assq 'registration plan)))
     "\nNothing has been pushed, created or registered.\n")))

(provide 'shipit-stack-plan)
;;; shipit-stack-plan.el ends here
