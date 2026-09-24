;;; test-shipit-stacked-flow.el --- The whole stacked flow -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; Commits to a registered stack, in one go, in a temporary repository
;; whose origin is a bare repository on disk.  The git half is real:
;; the branches are made, the pushes happen, the refs are checked on
;; the remote afterwards.  Only the remote's own API stands in, through
;; the backend registry every operation already goes through.
;;
;; This is the flow a user performs by hand: divide, create branches,
;; push, then submit each layer in turn.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-stacked-preview)
(require 'shipit-preview)
(require 'shipit-sandbox)

(defun test-flow--goto-layer (branch)
  "Put point on the row of BRANCH.
Found by its section, not by searching the text: the header names
the same branch in its Head line, and text can move."
  (let ((layer (seq-find (lambda (s) (equal branch (oref s value)))
                         (shipit--find-all-sections-by-type
                          'shipit-stack-layer))))
    (unless layer (error "No row for %s in %s" branch (buffer-string)))
    (goto-char (oref layer start))))

(defun test-flow--submit (branch)
  "Submit the layer of BRANCH the way pressing RET on it does."
  (test-flow--goto-layer branch)
  (shipit-stacked-preview-visit-layer)
  (with-current-buffer (shipit-preview--buffer-name shipit-sandbox-repo branch)
    (setq shipit-preview--title (format "%s title" branch)
          shipit-preview--description "why this layer exists")
    (shipit-preview--create-pr)))

(ert-deftest test-shipit-stacked-flow-from-commits-to-a-registered-stack ()
  "GIVEN four commits on a branch, in two pieces of work
WHEN they are divided, given branches, pushed and submitted in turn
THEN two pull requests are raised, the second based on the first, and
the two are registered as one stack.  This is the whole flow, with the
git half real and only the remote's API standing in."
  (shipit-sandbox-with-repo
    (shipit-stacked-preview)
    (with-current-buffer (format "*shipit-stacked-preview: %s*" "work")
      ;; divided by what the commit subjects have in common
      (should (equal 2 (length (shipit-stacked-preview--current-layers))))
      (let ((branches (shipit-stacked-preview--branch-names
                       (shipit-stacked-preview--current-layers))))
        (shipit-stacked-preview-create-branches)
        (should (seq-every-p #'shipit--branch-rev branches))

        ;; every layer to the remote, for real
        (shipit-stacked-preview--push-through
         (car (last (shipit-stacked-preview--current-layers))) nil)
        (should (seq-every-p #'shipit--remote-branch-rev branches))

        ;; and then one at a time
        (test-flow--submit (nth 0 branches))
        ;; named plainly: the remote is asked for a branch, not for a
        ;; ref of ours that happens to name one
        (should (equal (list (nth 0 branches) "main"
                             (format "%s title" (nth 0 branches)))
                       (car shipit-sandbox-created)))
        (should-not shipit-sandbox-stack)

        (test-flow--submit (nth 1 branches))
        (should (equal (list (nth 1 branches) (nth 0 branches)
                             (format "%s title" (nth 1 branches)))
                       (car shipit-sandbox-created)))
        (should (equal '(100 101)
                       (mapcar (lambda (e) (cdr (assq 'number e)))
                               (cdr (assq 'entries shipit-sandbox-stack)))))

        ;; and the rows say so, the remembered answer that these
        ;; branches had no pull request having been dropped
        (shipit-stacked-preview--render)
        (should (equal '("PR #100" "PR #101")
                       (mapcar #'shipit-stacked-preview--layer-status
                               (shipit-stacked-preview--current-layers))))))))

(provide 'test-shipit-stacked-flow)
;;; test-shipit-stacked-flow.el ends here
