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
;;
;; url.el never kills the buffer of a response: the code that asked
;; must.  Code that does not leaves one buffer for each request, for the
;; life of Emacs.  So the whole form fails as well when a response is
;; still alive at its end.
;;
;; Shipit runs curl for each request to GitLab that waits for its
;; answer.  `shipit-test-http-curl-stand-in' answers curl from the same
;; answers, inside `shipit-test-http-answering'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-http)

(defconst shipit-test-http-graphql-empty '("/graphql\\'" 200 ((data)))
  "The answer of a GraphQL query that finds nothing.")

(defconst shipit-test-http-not-starred '("/user/starred/" 404 "")
  "GitHub's answer when the user has not starred the repository.")

(defvar shipit-test-http-requests nil
  "The requests answered, oldest first, as (METHOD ADDRESS).")

(defvar shipit-test-http-bodies nil
  "The body of each request answered, oldest first; nil for one without a body.
In step with `shipit-test-http-requests'.")

(defvar shipit-test-http-headers nil
  "The headers that each request answered sent, oldest first, as alists.
In step with `shipit-test-http-requests'.")

(defvar shipit-test-http--unanswered nil
  "The addresses asked for that no answer covered, newest first.")

(defvar shipit-test-http-address nil
  "The address of the request that is answered now.
A function that gives a status, a body or headers reads it, so that one
pattern can answer each page of a list.")

(defvar shipit-test-http-hold nil
  "Non-nil when a request that does not wait gets its answer only later.
Such a request is held until `shipit-test-http-release'.")

(defvar shipit-test-http--held nil
  "The requests held, oldest first, as (ADDRESS RESPONSE CALLBACK CBARGS).")

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

(defun shipit-test-http--response (status body headers)
  "A buffer holding a response of STATUS with BODY, as url.el leaves one.
HEADERS is an alist of the headers it has beside its type."
  (let ((buffer (generate-new-buffer " *shipit-test-http*")))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (insert (format "HTTP/1.1 %d %s\nContent-Type: application/json\n"
                      status (if (< status 400) "OK" "Error")))
      (dolist (header headers)
        (insert (format "%s: %s\n" (car header) (cdr header))))
      (insert "\n")
      (setq-local url-http-end-of-headers (copy-marker (1- (point))))
      (setq-local url-http-response-status status)
      (insert (shipit-test-http--body body)))
    buffer))

(defun shipit-test-http-live-responses ()
  "The response buffers that are still alive."
  (seq-filter (lambda (buffer)
                (string-prefix-p " *shipit-test-http*" (buffer-name buffer)))
              (buffer-list)))

(defun shipit-test-http--kill-responses ()
  "Kill the response buffers that are still alive."
  (mapc #'kill-buffer (shipit-test-http-live-responses)))

(defun shipit-test-http--body-for (body)
  "BODY of an answer to the request that is made now.
A function gives it from what the request sent, as a string."
  (if (functionp body) (funcall body url-request-data) body))

(defun shipit-test-http--status-for (status)
  "STATUS of an answer to the request that is made now.
A function gives it, so that one address can answer each kind of
content asked for with its own status."
  (if (functionp status) (funcall status) status))

(defun shipit-test-http--headers-for (headers)
  "HEADERS of an answer to the request that is made now.
A function gives them, as an alist."
  (if (functionp headers) (funcall headers) headers))

(defun shipit-test-http--unchanged-p (headers)
  "Whether the request that is made now has the ETag of HEADERS.
It asks for the body only when that changed, and gets 304 Not Modified."
  (let ((etag (cdr (assoc "ETag" headers))))
    (and etag (equal etag (cdr (assoc "If-None-Match" url-request-extra-headers))))))

(defun shipit-test-http--record (address)
  "Record the request of ADDRESS that is made now."
  (setq shipit-test-http-requests
        (append shipit-test-http-requests
                (list (list (or url-request-method "GET") address)))
        shipit-test-http-bodies
        (append shipit-test-http-bodies (list url-request-data))
        shipit-test-http-headers
        (append shipit-test-http-headers (list url-request-extra-headers))))

(defun shipit-test-http--answer (answers url)
  "The response ANSWERS give to a request of URL, which is recorded."
  (let* ((address (shipit-test-http--address url))
         (shipit-test-http-address address)
         (answer (shipit-test-http--find answers address))
         (headers (shipit-test-http--headers-for (nth 3 answer))))
    (shipit-test-http--record address)
    (if (shipit-test-http--unchanged-p headers)
        (shipit-test-http--response 304 "" headers)
      (shipit-test-http--response (shipit-test-http--status-for (nth 1 answer))
                                  (shipit-test-http--body-for (nth 2 answer))
                                  headers))))

(defun shipit-test-http--status (buffer)
  "The status url.el hands a callback for the response in BUFFER."
  (let ((code (buffer-local-value 'url-http-response-status buffer)))
    (when (>= code 400)
      (list :error (list 'error 'http code)))))

(defun shipit-test-http--deliver (response callback cbargs)
  "Call CALLBACK with CBARGS in RESPONSE, as url.el does when it arrived."
  (with-current-buffer response
    (apply callback (shipit-test-http--status response) cbargs)))

(defun shipit-test-http--retrieve (answers)
  "A stand-in for `url-retrieve' that answers from ANSWERS.
It answers at once, or holds the answer when `shipit-test-http-hold'."
  (lambda (url callback &optional cbargs &rest _)
    (let ((response (shipit-test-http--answer answers url)))
      (if shipit-test-http-hold
          (setq shipit-test-http--held
                (append shipit-test-http--held
                        (list (list (shipit-test-http--address url)
                                    response callback cbargs))))
        (shipit-test-http--deliver response callback cbargs))
      response)))

(defun shipit-test-http-held-addresses ()
  "The addresses of the requests that wait for their answer, oldest first."
  (mapcar #'car shipit-test-http--held))

(defun shipit-test-http-release (&optional pattern)
  "Give the held requests whose address matches PATTERN their answer.
Without PATTERN each held request gets it, oldest first.  A request
whose response buffer the code killed, as it does to drop a request,
gets none, as with url.el."
  (let ((released (seq-filter (lambda (held)
                                (or (null pattern)
                                    (string-match-p pattern (car held))))
                              shipit-test-http--held)))
    (setq shipit-test-http--held
          (seq-difference shipit-test-http--held released #'eq))
    (dolist (held released)
      (when (buffer-live-p (nth 1 held))
        (shipit-test-http--deliver (nth 1 held) (nth 2 held) (nth 3 held))))))

(defun shipit-test-http--check-answered ()
  "Signal when a request went unanswered, naming each address."
  (when shipit-test-http--unanswered
    (error "shipit-test-http: no answer for %s"
           (string-join (reverse shipit-test-http--unanswered) ", "))))

(defun shipit-test-http--check-killed ()
  "Signal when a response is still alive, naming the addresses asked for.
The responses are killed first, so that they do not fail the next form."
  (let ((alive (length (shipit-test-http-live-responses))))
    (shipit-test-http--kill-responses)
    (when (> alive 0)
      (error "shipit-test-http: %d response buffers left alive after %s"
             alive (string-join (mapcar #'cadr shipit-test-http-requests) ", ")))))

(defun shipit-test-http--check-released ()
  "Signal when a request still waits for its answer, naming each address."
  (let ((held (shipit-test-http-held-addresses)))
    (setq shipit-test-http--held nil)
    (when held
      (error "shipit-test-http: no answer given to %s"
             (string-join held ", ")))))

(defun shipit-test-http--check ()
  "Signal when a request went unanswered or a response was left alive."
  (shipit-test-http--check-answered)
  (shipit-test-http--check-released)
  (shipit-test-http--check-killed))

(defmacro shipit-test-http-answering (answers &rest body)
  "Run BODY with url.el answering each request from ANSWERS.
ANSWERS is a list of (PATTERN STATUS BODY HEADERS).  A request whose
address matches PATTERN, a regexp, gets STATUS and BODY: a string as it
is, anything else encoded as JSON, so an empty array is written [].
BODY can be a function too: it gets what the request sent, as a string,
and gives the body, so that the pages of one GraphQL query differ.
STATUS can be a function of no arguments that gives it.
HEADERS, which can be left out, is an alist of the headers of the
answer, or a function of no arguments that gives it.  A request whose
If-None-Match is the ETag of those headers gets 304 Not Modified and no
body, as GitHub answers when nothing changed.  A request none covers
signals, and fails the form when BODY ends.  A response that is still
alive when BODY ends fails the form too.
While `shipit-test-http-hold' is non-nil, a request that does not wait
gets its answer at `shipit-test-http-release'.  One that got none when
BODY ends fails the form."
  (declare (indent 1))
  (let ((table (make-symbol "answers")))
    `(let ((shipit-test-http-requests nil)
           (shipit-test-http-bodies nil)
           (shipit-test-http-headers nil)
           (shipit-test-http--unanswered nil)
           (shipit-test-http--held nil)
           (,table ,answers))
       ;; A form that signalled left its responses: they are not of this one.
       (shipit-test-http--kill-responses)
       (prog1
           (cl-letf (((symbol-function 'url-retrieve-synchronously)
                      (lambda (url &rest _) (shipit-test-http--answer ,table url)))
                     ((symbol-function 'url-retrieve)
                      (shipit-test-http--retrieve ,table)))
             ,@body)
         (shipit-test-http--check)))))

;;; curl

(defun shipit-test-http--curl-option (args option)
  "The value that follows OPTION in ARGS, the arguments of curl, or nil."
  (cadr (member option args)))

(defun shipit-test-http--answer-curl (answers destination args)
  "Answer the request of curl with ARGS from ANSWERS, into the buffer DESTINATION.
A request that asks with -D for the headers gets a status line and the
headers of the answer there.
Curl exits with 0 for each status, as it does without --fail."
  (let* ((url-request-method (shipit-test-http--curl-option args "-X"))
         (url-request-data (shipit-test-http--curl-option args "-d"))
         (url-request-extra-headers nil)
         (address (car (last args)))
         (shipit-test-http-address address)
         (answer (shipit-test-http--find answers address))
         (headers-file (shipit-test-http--curl-option args "-D")))
    (shipit-test-http--record address)
    (when headers-file
      (with-temp-file headers-file
        (insert (format "HTTP/1.1 %d\n" (shipit-test-http--status-for (nth 1 answer))))
        (dolist (header (shipit-test-http--headers-for (nth 3 answer)))
          (insert (format "%s: %s\n" (car header) (cdr header))))))
    (with-current-buffer destination
      (insert (decode-coding-string
               (shipit-test-http--body (shipit-test-http--body-for (nth 2 answer)))
               'utf-8)))
    0))

(defun shipit-test-http-curl-stand-in (answers)
  "A stand-in for `call-process' that answers curl from ANSWERS.
ANSWERS are as `shipit-test-http-answering' takes them, and the stand-in
must run inside it.  Each other program runs, as before."
  (let ((call-process (symbol-function 'call-process)))
    (lambda (program &optional infile destination display &rest args)
      (if (equal program "curl")
          (shipit-test-http--answer-curl answers destination args)
        (apply call-process program infile destination display args)))))

(provide 'shipit-test-http)
;;; shipit-test-http.el ends here
