;;; test-minimal-indent.el --- Minimal indentation test -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest test-shipit-diff-file-indentation ()
  "Test if shipit-diff.el file has correct indentation."
  (let ((file-path "lisp/shipit-diff.el"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-file-contents file-path)
      (let ((original (buffer-string)))
        (indent-region (point-min) (point-max))
        (let ((indented (buffer-string)))
          (if (string= original indented)
              (message "PASS: File has correct indentation")
            (message "FAIL: File has indentation issues"))
          (should (string= original indented)))))))

(provide 'test-minimal-indent)
;;; test-minimal-indent.el ends here