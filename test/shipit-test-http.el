;;; shipit-test-http.el --- Answer url.el requests in tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; A test whose code talks HTTP answers it here, at url.el, which every
;; request of shipit passes through.  An answer is built the way url.el
;; leaves a response: a status line, a header, a blank line and the
;; body, in a unibyte buffer with `url-http-end-of-headers' on the blank
;; line.  A request that waits gets that buffer back.  One that does not
;; wait has its callback called at once, so nothing is left to run in a
;; later test.
;;
;; A request no answer covers is an error where it is made.  Code that
;; catches errors would hide it there, so the whole form fails too when
;; it ends.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'url-http)

(defconst shipit-test-http-graphql-empty '("/graphql\\'" 200 ((data)))
  "The answer of a GraphQL query that finds nothing.")

(defconst shipit-test-http-not-starred '("/user/starred/" 404 "")
  "GitHub's answer when the user has not starred the repository.")

(defvar shipit-test-http-requests nil
  "The requests answered, oldest first, as (METHOD ADDRESS).")

(defvar shipit-test-http--unanswered nil
  "The addresses asked for that no answer covered, newest first.")

(defun shipit-test-http--address (url)
  "URL as a string, whether url.el was given a string or a parsed one."
  (if (stringp url) url (url-recreate-url url)))

(defun shipit-test-http--find (answers address)
  "The entry of ANSWERS whose pattern matches ADDRESS.
Records ADDRESS and signals when none does."
  (or (cl-find-if (lambda (answer) (string-match-p (car answer) address)) answers)
      (progn
        (push address shipit-test-http--unanswered)
        (error "shipit-test-http: no answer for %s" address))))

(defun shipit-test-http--body (body)
  "BODY as the bytes of a response: a string as it is, else as JSON."
  (encode-coding-string (if (stringp body) body (json-encode body)) 'utf-8))

(defun shipit-test-http--response (status body)
  "A buffer holding a response of STATUS with BODY, as url.el leaves one."
  (let ((buffer (generate-new-buffer " *shipit-test-http*")))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (insert (format "HTTP/1.1 %d %s\nContent-Type: application/json\n\n"
                      status (if (< status 400) "OK" "Error")))
      (setq-local url-http-end-of-headers (copy-marker (1- (point))))
      (setq-local url-http-response-status status)
      (insert (shipit-test-http--body body)))
    buffer))

(defun shipit-test-http--answer (answers url)
  "The response ANSWERS give to a request of URL, which is recorded."
  (let* ((address (shipit-test-http--address url))
         (answer (shipit-test-http--find answers address)))
    (setq shipit-test-http-requests
          (append shipit-test-http-requests
                  (list (list (or url-request-method "GET") address))))
    (shipit-test-http--response (nth 1 answer) (nth 2 answer))))

(defun shipit-test-http--status (buffer)
  "The status url.el hands a callback for the response in BUFFER."
  (let ((code (buffer-local-value 'url-http-response-status buffer)))
    (when (>= code 400)
      (list :error (list 'error 'http code)))))

(defun shipit-test-http--retrieve (answers)
  "A stand-in for `url-retrieve' that answers from ANSWERS at once."
  (lambda (url callback &optional cbargs &rest _)
    (let ((buffer (shipit-test-http--answer answers url)))
      (with-current-buffer buffer
        (apply callback (shipit-test-http--status buffer) cbargs))
      buffer)))

(defun shipit-test-http--check ()
  "Signal when a request went unanswered, naming each address."
  (when shipit-test-http--unanswered
    (error "shipit-test-http: no answer for %s"
           (string-join (reverse shipit-test-http--unanswered) ", "))))

(defmacro shipit-test-http-answering (answers &rest body)
  "Run BODY with url.el answering each request from ANSWERS.
ANSWERS is a list of (PATTERN STATUS BODY).  A request whose address
matches PATTERN, a regexp, gets STATUS and BODY: a string as it is,
anything else encoded as JSON, so an empty array is written [].  A
request none covers signals, and fails the form when BODY ends."
  (declare (indent 1))
  (let ((table (make-symbol "answers")))
    `(let ((shipit-test-http-requests nil)
           (shipit-test-http--unanswered nil)
           (,table ,answers))
       (prog1
           (cl-letf (((symbol-function 'url-retrieve-synchronously)
                      (lambda (url &rest _) (shipit-test-http--answer ,table url)))
                     ((symbol-function 'url-retrieve)
                      (shipit-test-http--retrieve ,table)))
             ,@body)
         (shipit-test-http--check)))))

(provide 'shipit-test-http)
;;; shipit-test-http.el ends here
