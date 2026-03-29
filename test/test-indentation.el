;;; test-indentation.el --- Tests for indentation validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to detect and validate indentation issues in shipit code
;; This test file helps identify systematic indentation problems

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun test-indentation--check-file-indentation (file-path)
  "Check if FILE-PATH has proper Emacs Lisp indentation.
Returns list of lines with indentation issues."
  (let ((issues '())
        (original-content))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-file-contents file-path)
      (setq original-content (buffer-string))
      
      ;; Get original line indentations
      (let ((original-lines (split-string original-content "\n")))
        
        ;; Apply automatic indentation
        (indent-region (point-min) (point-max))
        (let ((indented-content (buffer-string))
              (indented-lines (split-string (buffer-string) "\n")))
          
          ;; Compare original vs properly indented
          (cl-loop for orig-line in original-lines
                   for indented-line in indented-lines
                   for line-num from 1
                   unless (string= orig-line indented-line)
                   do (push (list :line line-num
                                  :original orig-line
                                  :expected indented-line
                                  :original-indent (if (string-match "^ *" orig-line) (match-end 0) 0)
                                  :expected-indent (if (string-match "^ *" indented-line) (match-end 0) 0))
                            issues))
          
          ;; Return issues found
          (reverse issues))))))

(defun test-indentation--analyze-parentheses-balance (file-path)
  "Analyze parentheses balance in FILE-PATH.
Returns detailed analysis of parentheses depth issues."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents file-path)
    (let ((issues '())
          (paren-depth 0)
          (line-num 1))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (point))
              (line-indent (current-indentation)))
          (end-of-line)
          (let ((line-content (buffer-substring-no-properties line-start (point))))
            ;; Count parentheses changes on this line
            (let ((opens (cl-count ?\( line-content))
                  (closes (cl-count ?\) line-content))
                  (old-depth paren-depth))
              (setq paren-depth (+ paren-depth opens (- closes)))
              
              ;; Check for suspicious indentation patterns
              (when (and (> line-indent 0)
                         (not (string-match-p "^[ \t]*;;" line-content)) ; Skip comments
                         (not (string-match-p "^[ \t]*$" line-content))) ; Skip empty lines
                ;; Flag lines with unusual indentation for their depth
                (let ((expected-base-indent (* paren-depth 2))) ; Rough estimate
                  (when (> (abs (- line-indent expected-base-indent)) 10) ; Tolerance of 10 spaces
                    (push (list :line line-num
                                :content line-content
                                :actual-indent line-indent
                                :paren-depth paren-depth
                                :old-depth old-depth
                                :opens opens
                                :closes closes)
                          issues))))))
        (forward-line 1)
        (setq line-num (1+ line-num)))
      
      (list :final-depth paren-depth
            :issues (reverse issues)))))

(ert-deftest test-shipit-diff-indentation-consistency ()
  "Test that shipit-diff.el has consistent indentation."
  (let* ((file-path "lisp/shipit-diff.el")
         (indentation-issues (test-indentation--check-file-indentation file-path)))
    
    ;; Report any indentation inconsistencies
    (when indentation-issues
      (message "Found %d indentation issues in %s:" 
               (length indentation-issues) file-path)
      (dolist (issue indentation-issues)
        (message "Line %d: Original indent %d, Expected indent %d"
                 (plist-get issue :line)
                 (plist-get issue :original-indent)
                 (plist-get issue :expected-indent))
        (message "  Original: %s" (plist-get issue :original))
        (message "  Expected: %s" (plist-get issue :expected))))
    
    ;; Test should pass only if no indentation issues found
    (should (null indentation-issues))))

(ert-deftest test-shipit-diff-parentheses-depth-analysis ()
  "Test parentheses depth analysis for shipit-diff.el."
  (let* ((file-path "lisp/shipit-diff.el")
         (analysis (test-indentation--analyze-parentheses-balance file-path))
         (final-depth (plist-get analysis :final-depth))
         (issues (plist-get analysis :issues)))
    
    ;; File should end with balanced parentheses
    (should (= final-depth 0))
    
    ;; Report suspicious indentation patterns
    (when issues
      (message "Found %d suspicious indentation patterns in %s:" 
               (length issues) file-path)
      (dolist (issue (cl-subseq issues 0 (min 10 (length issues)))) ; Show first 10
        (message "Line %d (depth %d->%d): indent %d - %s"
                 (plist-get issue :line)
                 (plist-get issue :old-depth)
                 (plist-get issue :paren-depth)
                 (plist-get issue :actual-indent)
                 (substring (plist-get issue :content) 0 (min 60 (length (plist-get issue :content)))))))
    
    ;; For now, just report issues but don't fail the test
    ;; (We can make this stricter once we understand the patterns)
    t))

(ert-deftest test-shipit-diff-execute-action-function-structure ()
  "Test the specific shipit--execute-action function structure."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents "lisp/shipit-diff.el")
    
    ;; Find the shipit--execute-action function
    (goto-char (point-min))
    (should (re-search-forward "(defun shipit--execute-action" nil t))
    (let ((func-start (match-beginning 0)))
      
      ;; Find the end of the function
      (goto-char func-start)
      (forward-sexp) ; Move past the entire function
      (let ((func-end (point)))
        
        ;; Extract the function and test its indentation
        (let ((func-content (buffer-substring-no-properties func-start func-end)))
          (with-temp-buffer
            (emacs-lisp-mode)
            (insert func-content)
            (let ((original (buffer-string)))
              
              ;; Apply indentation
              (indent-region (point-min) (point-max))
              (let ((indented (buffer-string)))
                
                ;; Check if indentation changed the function
                (unless (string= original indented)
                  (message "shipit--execute-action function has indentation issues")
                  (let ((orig-lines (split-string original "\n"))
                        (indent-lines (split-string indented "\n")))
                    (cl-loop for orig in orig-lines
                             for indent in indent-lines
                             for line-num from 1
                             unless (string= orig indent)
                             do (message "Line %d differs:" line-num)
                             do (message "  Original: %s" orig)
                             do (message "  Expected: %s" indent)
                             when (> line-num 20) ; Limit output
                             do (cl-return))))
                
                ;; Test that function is properly structured
                (should (string= original indented))))))))))

(ert-deftest test-shipit-diff-syntax-validity ()
  "Test that shipit-diff.el has valid syntax."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents "lisp/shipit-diff.el")
    
    ;; Test that check-parens passes
    (should-not-error (check-parens))
    
    ;; Test that the file can be byte-compiled without syntax errors
    (let ((byte-compile-error-on-warn nil))
      (should (byte-compile-buffer)))))

(provide 'test-indentation)
;;; test-indentation.el ends here