;;; shipit-quiet-backend.el --- A pull request backend that answers nothing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The sandboxes each hold a repository that exists nowhere else.  When
;; a buffer asks the pull request backend about such a repository, the
;; sandbox answers with this one instead of GitHub, so nothing needs a
;; token or leaves the machine.

;;; Code:

(require 'shipit-pr-backends)

(defun shipit-quiet-backend ()
  "Return the GitHub backend with every operation answering nothing.
A buffer asks for far more than a sandbox has to offer, and an
operation a backend does not carry is reached as nil and called.
Answering nothing gives an empty answer to draw; leaving the key out
gives `void-function nil'.  The keys that are not operations -- icons,
labels -- are kept as they are, since that is what a buffer draws
itself with."
  (require 'shipit-pr-github)
  (let ((real (cdr (assq 'github shipit-pr-backends)))
        (copy nil))
    (while real
      (let ((key (car real))
            (value (cadr real)))
        (setq copy (append copy (list key (if (functionp value)
                                              (lambda (&rest _) nil)
                                            value))))
        (setq real (cddr real))))
    copy))

(provide 'shipit-quiet-backend)
;;; shipit-quiet-backend.el ends here
