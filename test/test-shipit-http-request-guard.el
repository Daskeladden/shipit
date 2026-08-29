;;; test-shipit-http-request-guard.el --- Tests for the HTTP request guard -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `shipit--url-http-request-guard', the around-advice on
;; `url-http-create-request' that (1) forces ASCII RFC-7231 dates in
;; headers regardless of LC_TIME / a reset `system-time-locale'
;; (observed live: "Multibyte text in HTTP request ...
;; If-modified-since: sø., 19 juli 2026 ..."), (2) disables url.el's
;; automatic cache headers for shipit's API host, and (3) redacts
;; Authorization credentials from request-construction errors, which
;; otherwise dump the full request -- token included -- into *Messages*,
;; and (4) normalizes multibyte request pieces to unibyte UTF-8 --
;; auth-source-decoded tokens are multibyte strings, and once
;; concatenated with a correctly UTF-8-encoded JSON body the request
;; has more bytes than chars and url.el rejects it (observed live as a
;; failed create-PR whose description contained an em dash).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'url-parse)
(require 'shipit-http)

(ert-deftest test-guard-forces-ascii-http-dates ()
  "GIVEN system-time-locale reset to a non-English locale
WHEN a request is built through the guard
THEN date formatting inside uses the C locale (ASCII English)."
  ;; Only a machine with a non-English time locale installed can show
  ;; the difference.  Where nb_NO is missing, dates come out English
  ;; with or without the guard and the test would pass while proving
  ;; nothing, so it is skipped rather than left to look green.
  (skip-unless (let ((system-time-locale "nb_NO.utf8"))
                 (equal (format-time-string "%a" 0 t) "to.")))
  (let ((system-time-locale "nb_NO.utf8"))
    (should (equal (format-time-string "%a" 0 t) "to."))
    (should (equal (shipit--url-http-request-guard
                    (lambda () (format-time-string "%a, %d %b %Y" 0 t)))
                   "Thu, 01 Jan 1970"))))

(ert-deftest test-guard-disables-url-cache-for-shipit-host ()
  "GIVEN url-automatic-caching enabled globally
WHEN building a request for shipit's API host vs another host
THEN caching is disabled only for the shipit host."
  (let ((url-automatic-caching t)
        (shipit-api-url "https://api.github.com"))
    (with-temp-buffer
      (setq-local url-http-target-url
                  (url-generic-parse-url
                   "https://api.github.com/notifications?per_page=50"))
      (should (null (shipit--url-http-request-guard
                     (lambda () url-automatic-caching)))))
    (with-temp-buffer
      (setq-local url-http-target-url
                  (url-generic-parse-url "https://example.com/feed"))
      (should (eq t (shipit--url-http-request-guard
                     (lambda () url-automatic-caching)))))))

(ert-deftest test-guard-scrubs-authorization-from-errors ()
  "GIVEN request construction fails with the request text in the error
WHEN the error propagates through the guard
THEN Authorization credentials are redacted but the error still signals."
  (let ((err (should-error
              (shipit--url-http-request-guard
               (lambda ()
                 (error "Multibyte text in HTTP request: GET /x HTTP/1.1
Host: api.github.com
Authorization: Bearer ghp_SECRETTOKEN123
Accept: */*")))
              :type 'error)))
    (let ((text (error-message-string err)))
      (should-not (string-match-p "ghp_SECRETTOKEN123" text))
      (should (string-match-p "Authorization: \\*\\*\\*" text))
      ;; The rest of the request stays intact for diagnosis
      (should (string-match-p "Host: api.github.com" text)))))

(ert-deftest test-guard-scrub-handles-token-scheme-and-non-strings ()
  "GIVEN error data with a token-scheme header and non-string elements
WHEN scrubbing
THEN the credential is redacted and non-strings pass through unchanged."
  (should (equal (shipit--scrub-auth-in-error-data
                  '("before Authorization: token abc123 after" 42 nil))
                 '("before Authorization: *** after" 42 nil))))

(ert-deftest test-guard-advice-installed ()
  "GIVEN shipit-http is loaded
WHEN checking url-http-create-request
THEN the request guard advice is installed."
  (should (advice-member-p 'shipit--url-http-request-guard
                           'url-http-create-request)))

(defmacro test-guard--with-request-buffer (headers data &rest body)
  "Run BODY in a buffer set up for a POST built from HEADERS and DATA.
Provides the buffer-local variables `url-http-create-request' reads,
mirroring the connection buffer url.el prepares."
  (declare (indent 2))
  `(with-temp-buffer
     (setq-local url-http-target-url
                 (url-generic-parse-url
                  "https://api.github.com/repos/owner/repo/pulls"))
     (setq-local url-http-method "POST")
     (setq-local url-http-extra-headers ,headers)
     (setq-local url-http-data ,data)
     (setq-local url-http-referer nil)
     (setq-local url-http-proxy nil)
     (setq-local url-http-version "1.1")
     (setq-local url-http-attempt-keepalives nil)
     ;; url.el declares this with no value and only ever let-binds it
     ;; inside `url-http', so calling `url-http-create-request' directly
     ;; finds it unbound.  Supply the environment url.el would.
     (setq-local url-http-extensions-header nil)
     ,@body))

(ert-deftest test-guard-normalizes-multibyte-auth-with-utf8-body ()
  "GIVEN a POST with a correctly UTF-8-encoded body containing an em dash
      and a multibyte Authorization value as delivered by auth-source
      -- the exact shape of the create-PR request that failed live
WHEN the request is assembled by `url-http-create-request'
THEN it builds a unibyte request carrying the token and the UTF-8
     em-dash bytes instead of signaling \"Multibyte text in HTTP request\"
     AND the caller's header alist is not mutated in place."
  (let* ((auth-value (string-to-multibyte "Bearer ghp_unittest"))
         (headers `(("Authorization" . ,auth-value)
                    ("Content-Type" . "application/json"))))
    ;; Sanity: this is the trigger -- a multibyte (though pure-ASCII) token
    (should (multibyte-string-p auth-value))
    (test-guard--with-request-buffer headers
        (encode-coding-string "{\"body\":\"a — b\"}" 'utf-8)
      (let ((request (url-http-create-request)))
        (should-not (multibyte-string-p request))
        (should (string-match-p "ghp_unittest" request))
        (should (string-match-p
                 (regexp-quote (encode-coding-string "—" 'utf-8))
                 request))))
    (should (eq (cdr (assoc "Authorization" headers)) auth-value))
    (should (multibyte-string-p auth-value))))

(ert-deftest test-guard-normalizes-forgotten-multibyte-body ()
  "GIVEN a request body a call site left multibyte (missing its encode)
WHEN the request is assembled through the guard
THEN the body is sent as UTF-8 with a byte-accurate Content-length."
  (let ((expected (encode-coding-string "{\"title\":\"æøå\"}" 'utf-8)))
    (test-guard--with-request-buffer
        '(("Content-Type" . "application/json"))
        "{\"title\":\"æøå\"}"
      (let ((request (url-http-create-request)))
        (should-not (multibyte-string-p request))
        (should (string-suffix-p expected request))
        (should (string-match-p
                 (format "Content-length: %d" (length expected))
                 request))))))

(ert-deftest test-guard-force-unibyte-semantics ()
  "GIVEN multibyte ASCII, multibyte non-ASCII, unibyte and non-string values
WHEN forcing unibyte
THEN ASCII keeps identical bytes, non-ASCII becomes UTF-8, and the
     rest pass through unchanged."
  (let ((result (shipit--force-unibyte (string-to-multibyte "Bearer abc"))))
    (should-not (multibyte-string-p result))
    (should (equal result "Bearer abc")))
  (should (equal (shipit--force-unibyte "på — vei")
                 (encode-coding-string "på — vei" 'utf-8)))
  (should (equal (shipit--force-unibyte "plain") "plain"))
  (should (eq (shipit--force-unibyte 42) 42))
  (should (eq (shipit--force-unibyte nil) nil)))

(provide 'test-shipit-http-request-guard)
;;; test-shipit-http-request-guard.el ends here
