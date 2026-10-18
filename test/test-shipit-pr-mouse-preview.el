;;; test-shipit-pr-mouse-preview.el --- The PR preview on mouse-over -*- lexical-binding: t; -*-

;;; Commentary:
;; When the mouse rests on a pull request link in a shipit buffer, the
;; preview of the pull request opens next to it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'shipit-pr-github)
(require 'shipit-pr-mouse-preview)

(defun test-mouse-preview--with-backend (plist body)
  "Run BODY with a PR backend registered from PLIST."
  (let ((shipit-pr-backend 'test-mouse-preview)
        (shipit-pr-backends (list (cons 'test-mouse-preview plist))))
    (funcall body)))

;;;; Fetching a pull request in the background

(ert-deftest test-shipit-pr-fetch-async-without-the-operation-starts-nothing ()
  "GIVEN a backend with no :fetch-pr-async, as GitLab has none
WHEN a pull request is fetched in the background
THEN nothing starts, the answer is nil, AND the callback is not called."
  (let ((called nil))
    (test-mouse-preview--with-backend
     '(:name "Waits")
     (lambda ()
       (should-not (shipit-pr--fetch-pr-async "o/r" 7 (lambda (_) (setq called t))))
       (should-not called)))))

(ert-deftest test-shipit-pr-fetch-async-passes-the-config-and-the-number ()
  "GIVEN a backend with a :fetch-pr-async
WHEN pull request 7 of o/r is fetched in the background
THEN the operation gets the config of o/r, the number and the callback,
AND the answer is non-nil."
  (let ((got nil)
        (callback (lambda (_) nil)))
    (test-mouse-preview--with-backend
     (list :name "Async"
           :fetch-pr-async (lambda (config number cb) (setq got (list config number cb))))
     (lambda ()
       (should (shipit-pr--fetch-pr-async "o/r" 7 callback))
       (should (equal "o/r" (plist-get (nth 0 got) :repo)))
       (should (equal 7 (nth 1 got)))
       (should (eq callback (nth 2 got)))))))

(ert-deftest test-shipit-pr-github-fetch-async-gives-the-pull-request ()
  "GIVEN the GitHub backend, and a background request that answers with PR 7
WHEN pull request 7 of o/r is fetched in the background
THEN the request asks the pulls endpoint, AND the callback gets the alist."
  (let ((endpoint nil)
        (got 'unset))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (requested _params _token callback &rest _)
                 (setq endpoint requested)
                 (funcall callback '(:json ((number . 7)) :status 200)))))
      (shipit-pr-github--fetch-pr-async '(:repo "o/r") 7 (lambda (pr) (setq got pr)))
      (should (equal "/repos/o/r/pulls/7" endpoint))
      (should (equal '((number . 7)) got)))))

(ert-deftest test-shipit-pr-github-fetch-async-gives-nil-when-the-request-fails ()
  "GIVEN the GitHub backend, and a background request that fails, as it
does for the number of an issue
WHEN the number is fetched in the background
THEN the callback gets nil."
  (let ((got 'unset))
    (cl-letf (((symbol-function 'shipit-gh-etag-get-json-async)
               (lambda (_requested _params _token callback &rest _)
                 (funcall callback '(:error . (error http 404))))))
      (shipit-pr-github--fetch-pr-async '(:repo "o/r") 9 (lambda (pr) (setq got pr)))
      (should-not got))))

;;;; The link under the mouse

(ert-deftest test-shipit-pr-url-overlay-names-its-pull-request ()
  "GIVEN a buffer with the URL of pull request 12 of o/r
WHEN shipit makes the overlays of its references
THEN the overlay of the URL names pull request 12 of o/r."
  (with-temp-buffer
    (insert "See https://github.com/o/r/pull/12 now")
    (shipit-pr-github--create-reference-overlays "o/r" (point-min) (point-max) #'ignore #'ignore)
    (should (equal '(12 . "o/r")
                   (cl-some (lambda (ov) (overlay-get ov 'shipit-pr-link))
                            (overlays-in (point-min) (point-max)))))))

(ert-deftest test-shipit-pr-link-at-finds-a-pr-url ()
  "GIVEN a buffer with an overlay that names pull request 12 of o/r
WHEN asking for the PR link at a position inside it, and outside it
THEN the first gives the number, the repository and the bounds, AND the
second gives nil."
  (with-temp-buffer
    (insert "See the link now")
    (overlay-put (make-overlay 5 13) 'shipit-pr-link '(12 . "o/r"))
    (should (equal '(:number 12 :repo "o/r" :start 5 :end 13) (shipit--pr-link-at 7)))
    (should-not (shipit--pr-link-at 2))))

(ert-deftest test-shipit-pr-link-at-finds-a-hash-reference ()
  "GIVEN a buffer with the overlay of the reference #7 of o/r, which shipit
makes for an issue and a pull request alike
WHEN asking for the PR link at a position inside it
THEN it gives number 7, repository o/r and the bounds."
  (with-temp-buffer
    (insert "See #7 now")
    (let ((ov (make-overlay 5 7)))
      (overlay-put ov 'shipit-issue-id 7)
      (overlay-put ov 'shipit-issue-repo "o/r"))
    (should (equal '(:number 7 :repo "o/r" :start 5 :end 7) (shipit--pr-link-at 6)))))

;;;; What the mouse makes happen

(defconst test-mouse-preview--link '(:number 7 :repo "o/r" :start 5 :end 7 :buffer "b")
  "A link under the mouse.")

(defconst test-mouse-preview--other '(:number 8 :repo "o/r" :start 20 :end 22 :buffer "b")
  "Another link under the mouse.")

(defun test-mouse-preview--state (phase link since)
  "A state of PHASE on LINK since SINCE."
  (list :phase phase :link link :since since))

(ert-deftest test-shipit-mouse-preview-waits-for-the-delay ()
  "GIVEN the mouse arrived on a link at 10.0 s, and a delay of 0.5 s
WHEN it is still there at 10.3 s, and then at 10.5 s
THEN nothing happens at 10.3 s, AND at 10.5 s the pull request is fetched."
  (let ((state (car (shipit--mouse-preview-step nil 10.0 test-mouse-preview--link nil 0.5))))
    (should (eq 'waiting (plist-get state :phase)))
    (should-not (cdr (shipit--mouse-preview-step state 10.3 test-mouse-preview--link nil 0.5)))
    (let ((step (shipit--mouse-preview-step state 10.5 test-mouse-preview--link nil 0.5)))
      (should (eq 'fetch (cdr step)))
      (should (eq 'fetching (plist-get (car step) :phase))))))

(ert-deftest test-shipit-mouse-preview-fetches-once-per-visit ()
  "GIVEN a fetch that started for the link under the mouse
WHEN the mouse stays on the link
THEN no second fetch starts."
  (let ((state (test-mouse-preview--state 'fetching test-mouse-preview--link 10.0)))
    (should (equal (cons state nil)
                   (shipit--mouse-preview-step state 12.0 test-mouse-preview--link nil 0.5)))))

(ert-deftest test-shipit-mouse-preview-shows-what-arrives-for-the-link ()
  "GIVEN a fetch for the link under the mouse
WHEN its pull request arrives
THEN the preview is shown."
  (let ((step (shipit--mouse-preview-arrived
               (test-mouse-preview--state 'fetching test-mouse-preview--link 10.0)
               test-mouse-preview--link '((number . 7)))))
    (should (eq 'show (cdr step)))
    (should (eq 'shown (plist-get (car step) :phase)))))

(ert-deftest test-shipit-mouse-preview-drops-what-arrives-for-a-link-left ()
  "GIVEN the mouse moved on to another link while the first was fetched
WHEN the pull request of the first link arrives
THEN nothing is shown, AND the state is unchanged."
  (let ((state (test-mouse-preview--state 'waiting test-mouse-preview--other 11.0)))
    (should (equal (cons state nil)
                   (shipit--mouse-preview-arrived state test-mouse-preview--link '((number . 7)))))))

(ert-deftest test-shipit-mouse-preview-stops-on-a-link-that-is-no-pr ()
  "GIVEN a fetch for the link under the mouse, whose number is an issue
WHEN the fetch answers nil, and the mouse stays on the link
THEN nothing is shown, AND no second fetch starts."
  (let ((step (shipit--mouse-preview-arrived
               (test-mouse-preview--state 'fetching test-mouse-preview--link 10.0)
               test-mouse-preview--link nil)))
    (should-not (cdr step))
    (should-not (cdr (shipit--mouse-preview-step (car step) 13.0 test-mouse-preview--link nil 0.5)))))

(ert-deftest test-shipit-mouse-preview-stays-while-the-mouse-is-on-it ()
  "GIVEN a preview that is shown
WHEN the mouse leaves the link for the preview itself
THEN the preview stays."
  (let ((state (test-mouse-preview--state 'shown test-mouse-preview--link 10.0)))
    (should (equal (cons state nil) (shipit--mouse-preview-step state 12.0 nil t 0.5)))))

(ert-deftest test-shipit-mouse-preview-closes-when-the-mouse-leaves ()
  "GIVEN a preview that is shown
WHEN the mouse is on neither the link nor the preview
THEN the preview closes, AND the state is idle."
  (let ((step (shipit--mouse-preview-step
               (test-mouse-preview--state 'shown test-mouse-preview--link 10.0) 12.0 nil nil 0.5)))
    (should (eq 'close (cdr step)))
    (should-not (car step))))

(ert-deftest test-shipit-mouse-preview-moves-on-to-another-link ()
  "GIVEN a preview that is shown for one link
WHEN the mouse arrives on another link at 12.0 s
THEN the preview closes, AND the wait for the other link starts."
  (let ((step (shipit--mouse-preview-step
               (test-mouse-preview--state 'shown test-mouse-preview--link 10.0)
               12.0 test-mouse-preview--other nil 0.5)))
    (should (eq 'close (cdr step)))
    (should (equal (test-mouse-preview--state 'waiting test-mouse-preview--other 12.0) (car step)))))

;;;; Fetching and opening

(ert-deftest test-shipit-mouse-preview-stops-where-no-background-fetch-is ()
  "GIVEN a backend with no :fetch-pr-async, and the wait on a link is over
WHEN the fetch of the link starts
THEN the state says that the link names no pull request, AND no
preview opens."
  (let ((shipit--mouse-preview-state (test-mouse-preview--state 'fetching test-mouse-preview--link 10.0))
        (shipit--mouse-preview-frame nil)
        (shown nil))
    (cl-letf (((symbol-function 'shipit--show-pr-preview-data)
               (lambda (&rest args) (setq shown args))))
      (test-mouse-preview--with-backend
       '(:name "Waits")
       (lambda ()
         (shipit--mouse-preview-fetch test-mouse-preview--link)
         (should (eq 'no-pr (plist-get shipit--mouse-preview-state :phase)))
         (should-not shown))))))

(ert-deftest test-shipit-mouse-preview-opens-at-the-link-without-focus ()
  "GIVEN a backend whose background fetch answers at once with pull
request 7, and the wait on its link is over
WHEN the fetch of the link starts
THEN the preview of pull request 7 opens at the start of the link in its
window, without the keyboard, AND it is the preview that the mouse
watches."
  (let* ((window (selected-window))
         (link (list :number 7 :repo "o/r" :start 5 :end 7 :buffer (current-buffer) :window window))
         (shipit--mouse-preview-state (test-mouse-preview--state 'fetching link 10.0))
         (shipit--mouse-preview-frame nil)
         (shown nil))
    (cl-letf (((symbol-function 'shipit--show-pr-preview-data)
               (lambda (&rest args) (setq shown args) 'the-frame)))
      (test-mouse-preview--with-backend
       (list :name "Async"
             :fetch-pr-async (lambda (_config _number callback) (funcall callback '((number . 7)))))
       (lambda ()
         (shipit--mouse-preview-fetch link)
         (should (equal (list 7 "o/r" '((number . 7)) (cons window 5) t) shown))
         (should (eq 'the-frame shipit--mouse-preview-frame)))))))

(ert-deftest test-shipit-mouse-preview-reads-the-mouse-without-asking-the-display ()
  "GIVEN a preview frame that is open
WHEN the poll reads where the mouse is, for the link under it and for
the preview
THEN each read takes the place that Emacs knows from the last motion of
the mouse, and asks the X server nothing.  Over ssh a question to the
server took 60 to 90 ms, and the poll runs each 0.2 seconds, so Emacs
waited for the server most of the time."
  (let ((asked nil))
    (cl-letf (((symbol-function 'mouse-pixel-position)
               (lambda ()
                 (push (bound-and-true-p x-use-fast-mouse-position) asked)
                 (list (selected-frame)))))
      (shipit--mouse-preview-link-under-mouse)
      (let ((shipit--mouse-preview-frame (selected-frame)))
        (shipit--mouse-preview-on-popup-p)))
    (should (equal '(really-fast really-fast) asked))))

(ert-deftest test-shipit-mouse-preview-mode-polls-only-while-on ()
  "GIVEN the mouse preview mode
WHEN it is turned on, and then off
THEN a timer watches the mouse while it is on, AND no timer is left
when it is off."
  (unwind-protect
      (progn
        (shipit-pr-mouse-preview-mode 1)
        (let ((timer shipit--mouse-preview-timer))
          (should (memq timer timer-list))
          (shipit-pr-mouse-preview-mode -1)
          (should-not shipit--mouse-preview-timer)
          (should-not (memq timer timer-list))))
    (shipit-pr-mouse-preview-mode -1)))

(provide 'test-shipit-pr-mouse-preview)
;;; test-shipit-pr-mouse-preview.el ends here
