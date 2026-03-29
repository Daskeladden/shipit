;;; test-shipit-comment-backends.el --- Tests for comment backend registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for pluggable comment backend system.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-comment-backends)
(require 'shipit-http)

;;; Test helpers

(defun test-shipit-comment--make-minimal-plist ()
  "Return a minimal valid comment backend plist with all required keys."
  (list :name "Test"
        :fetch-general-comments #'ignore
        :fetch-inline-comments #'ignore
        :add-general-comment #'ignore
        :add-inline-comment #'ignore
        :reply-to-comment #'ignore
        :edit-comment #'ignore
        :delete-comment #'ignore
        :toggle-reaction #'ignore
        :fetch-reactions #'ignore))

;;; Registration Tests

(ert-deftest test-shipit-comment-register-backend-basic ()
  "GIVEN a valid comment backend plist with all required keys
WHEN registering the backend
THEN it is stored in shipit-comment-backends."
  (let ((shipit-comment-backends nil))
    (shipit-comment-register-backend 'test-be (test-shipit-comment--make-minimal-plist))
    (should (assq 'test-be shipit-comment-backends))))

(ert-deftest test-shipit-comment-register-backend-missing-key ()
  "GIVEN a comment backend plist missing required key :add-general-comment
WHEN registering the backend
THEN an error is signaled."
  (let ((shipit-comment-backends nil))
    (should-error
     (shipit-comment-register-backend
      'bad-be
      (list :name "Bad"
            :fetch-general-comments #'ignore
            :fetch-inline-comments #'ignore
            :add-inline-comment #'ignore
            :reply-to-comment #'ignore
            :edit-comment #'ignore
            :delete-comment #'ignore
            :toggle-reaction #'ignore
            :fetch-reactions #'ignore)))))

(ert-deftest test-shipit-comment-register-backend-replaces-existing ()
  "GIVEN a backend already registered under id 'my-be
WHEN registering a new backend with the same id
THEN the old backend is replaced."
  (let ((shipit-comment-backends nil))
    (let ((plist1 (test-shipit-comment--make-minimal-plist))
          (plist2 (test-shipit-comment--make-minimal-plist)))
      (plist-put plist1 :name "V1")
      (plist-put plist2 :name "V2")
      (shipit-comment-register-backend 'my-be plist1)
      (shipit-comment-register-backend 'my-be plist2)
      (should (string= "V2" (plist-get (cdr (assq 'my-be shipit-comment-backends)) :name)))
      (should (= 1 (length shipit-comment-backends))))))

;;; Lookup Tests

(ert-deftest test-shipit-comment-get-backend-returns-active ()
  "GIVEN 'github comment backend is registered and shipit-pr-backend is 'github
WHEN calling shipit-comment--get-backend
THEN the github backend plist is returned."
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'github))
    (let ((plist (test-shipit-comment--make-minimal-plist)))
      (plist-put plist :name "GitHub")
      (shipit-comment-register-backend 'github plist))
    (let ((backend (shipit-comment--get-backend)))
      (should backend)
      (should (string= "GitHub" (plist-get backend :name))))))

(ert-deftest test-shipit-comment-get-backend-errors-on-unknown ()
  "GIVEN no comment backends registered
WHEN calling shipit-comment--get-backend with pr-backend set to 'unknown
THEN an error is signaled."
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'unknown))
    (should-error (shipit-comment--get-backend))))

;;; Resolve-for-repo Tests

(ert-deftest test-shipit-comment-resolve-for-repo-returns-backend-and-config ()
  "GIVEN a github comment backend registered
WHEN resolving for a repo
THEN returns (BACKEND-PLIST . CONFIG) with :repo injected."
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-comment--make-minimal-plist)))
      (plist-put plist :name "GitHub")
      (shipit-comment-register-backend 'github plist))
    (let ((result (shipit-comment--resolve-for-repo "owner/repo")))
      (should (consp result))
      (should (string= "GitHub" (plist-get (car result) :name)))
      (should (string= "owner/repo" (plist-get (cdr result) :repo))))))

(ert-deftest test-shipit-comment-resolve-for-repo-shares-pr-config ()
  "GIVEN shipit-pr-backend-config has :api-url
WHEN resolving comment backend for a repo
THEN the config includes both :repo and :api-url from PR config."
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'github)
        (shipit-pr-backend-config '(:api-url "https://custom.api.com")))
    (shipit-comment-register-backend 'github (test-shipit-comment--make-minimal-plist))
    (let* ((result (shipit-comment--resolve-for-repo "owner/repo"))
           (config (cdr result)))
      (should (string= "owner/repo" (plist-get config :repo)))
      (should (string= "https://custom.api.com" (plist-get config :api-url))))))

;;; Dispatch Tests

(ert-deftest test-shipit-comment-dispatch-add-general-comment ()
  "GIVEN a mock comment backend with :add-general-comment
WHEN dispatching add-general-comment through the backend
THEN the mock function is called and returns data."
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-comment--make-minimal-plist)))
      (plist-put plist :name "Mock")
      (plist-put plist :add-general-comment
                 (lambda (config number body)
                   `((id . 123)
                     (body . ,body)
                     (pr . ,number)
                     (repo . ,(plist-get config :repo)))))
      (shipit-comment-register-backend 'mock plist))
    (let* ((resolved (shipit-comment--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved))
           (result (funcall (plist-get backend :add-general-comment)
                            config 42 "Hello!")))
      (should (= 123 (cdr (assq 'id result))))
      (should (string= "Hello!" (cdr (assq 'body result))))
      (should (= 42 (cdr (assq 'pr result)))))))

(ert-deftest test-shipit-comment-dispatch-toggle-reaction ()
  "GIVEN a mock comment backend with :toggle-reaction
WHEN dispatching toggle-reaction
THEN the correct arguments are passed through."
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil)
        (captured-args nil))
    (let ((plist (test-shipit-comment--make-minimal-plist)))
      (plist-put plist :name "Mock")
      (plist-put plist :toggle-reaction
                 (lambda (config comment-id reaction &optional is-inline)
                   (setq captured-args
                         (list :config config :comment-id comment-id
                               :reaction reaction :is-inline is-inline))
                   '((id . 1) (content . "+1"))))
      (shipit-comment-register-backend 'mock plist))
    (let* ((resolved (shipit-comment--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (config (cdr resolved)))
      (funcall (plist-get backend :toggle-reaction) config 456 "+1" t)
      (should (= 456 (plist-get captured-args :comment-id)))
      (should (string= "+1" (plist-get captured-args :reaction)))
      (should (eq t (plist-get captured-args :is-inline))))))

(ert-deftest test-shipit-comment-dispatch-optional-reply-to-inline ()
  "GIVEN a backend with optional :reply-to-inline
WHEN checking the optional key
THEN it is accessible."
  (let ((shipit-comment-backends nil)
        (shipit-pr-backend 'mock)
        (shipit-pr-backend-config nil))
    (let ((plist (test-shipit-comment--make-minimal-plist)))
      (plist-put plist :name "Mock")
      (plist-put plist :reply-to-inline
                 (lambda (_config _number _parent-id _body _file-path)
                   '((id . 99))))
      (shipit-comment-register-backend 'mock plist))
    (let* ((resolved (shipit-comment--resolve-for-repo "owner/repo"))
           (backend (car resolved))
           (fn (plist-get backend :reply-to-inline)))
      (should fn)
      (should (equal '((id . 99)) (funcall fn nil 1 2 "body" "file.el"))))))

;;; Orchestrator Dispatch Tests

(defmacro test-shipit-comment--with-mock-backend (backend-ops &rest body)
  "Run BODY with a mock comment backend registered.
BACKEND-OPS is a plist of operation overrides merged onto the minimal backend."
  (declare (indent 1))
  `(let ((shipit-comment-backends nil)
         (shipit-pr-backend 'mock)
         (shipit-pr-backend-config nil)
         (shipit-github-token "test-token")
         (shipit--current-displayed-pr '(42 "owner/repo"))
         (shipit--user-mutated-pr nil))
     (let ((plist (test-shipit-comment--make-minimal-plist)))
       (plist-put plist :name "Mock")
       ,@(cl-loop for (key val) on backend-ops by #'cddr
                  collect `(plist-put plist ,key ,val))
       (shipit-comment-register-backend 'mock plist))
     ,@body))

(ert-deftest test-shipit-add-general-comment-dispatches-to-backend ()
  "GIVEN a mock comment backend
WHEN calling shipit--add-general-comment-to-pr
THEN backend :add-general-comment is called with correct args
     AND new comment is added to cache."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:add-general-comment
         (lambda (config number body)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :number number :body body))
           '((id . 100) (body . "Hello"))))
      (let ((result (shipit--add-general-comment-to-pr 42 "Hello" t)))
        (should result)
        (should (string= "owner/repo" (plist-get captured-args :repo)))
        (should (= 42 (plist-get captured-args :number)))
        (should (string= "Hello" (plist-get captured-args :body)))))))

(ert-deftest test-shipit-reply-to-general-comment-dispatches-to-backend ()
  "GIVEN a mock comment backend
WHEN calling shipit--reply-to-general-comment
THEN backend :reply-to-comment is called with correct args."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:reply-to-comment
         (lambda (config number parent-id body &optional _is-inline)
           (setq captured-args (list :number number :parent-id parent-id :body body))
           '((id . 200) (body . "Reply"))))
      ;; shipit--get-comment-depth is needed for reply-depth calc
      (cl-letf (((symbol-function 'shipit--get-comment-depth) (lambda (_id) 0)))
        (let ((result (shipit--reply-to-general-comment 42 999 "Reply" t)))
          (should result)
          (should (= 42 (plist-get captured-args :number)))
          (should (= 999 (plist-get captured-args :parent-id)))
          (should (string= "Reply" (plist-get captured-args :body))))))))

(ert-deftest test-shipit-reply-to-general-comment-uses-discussion-id-from-cache ()
  "GIVEN a mock comment backend and cached comments with discussion_id
WHEN calling shipit--reply-to-general-comment with a comment-id
THEN backend :reply-to-comment receives the discussion_id instead of comment-id."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:reply-to-comment
         (lambda (config number parent-id body &optional _is-inline)
           (setq captured-args (list :number number :parent-id parent-id :body body))
           '((id . 200) (body . "Reply"))))
      (cl-letf (((symbol-function 'shipit--get-comment-depth) (lambda (_id) 0)))
        ;; Set up cached general comments with discussion_id
        (let ((shipit--cached-general-comments
               (list '((id . 999)
                       (body . "Original comment")
                       (discussion_id . "disc-xyz")
                       (user . ((login . "alice")))))))
          (let ((result (shipit--reply-to-general-comment 42 999 "Reply" t)))
            (should result)
            (should (= 42 (plist-get captured-args :number)))
            ;; Should pass discussion_id, not comment-id
            (should (equal "disc-xyz" (plist-get captured-args :parent-id)))
            (should (string= "Reply" (plist-get captured-args :body)))))))))

(ert-deftest test-shipit-reply-to-general-comment-falls-back-to-comment-id ()
  "GIVEN a mock comment backend and cached comments WITHOUT discussion_id
WHEN calling shipit--reply-to-general-comment with a comment-id
THEN backend :reply-to-comment receives the comment-id (GitHub fallback)."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:reply-to-comment
         (lambda (config number parent-id body &optional _is-inline)
           (setq captured-args (list :number number :parent-id parent-id :body body))
           '((id . 200) (body . "Reply"))))
      (cl-letf (((symbol-function 'shipit--get-comment-depth) (lambda (_id) 0)))
        ;; Set up cached general comments WITHOUT discussion_id (GitHub-style)
        (let ((shipit--cached-general-comments
               (list '((id . 999)
                       (body . "Original comment")
                       (user . ((login . "alice")))))))
          (let ((result (shipit--reply-to-general-comment 42 999 "Reply" t)))
            (should result)
            (should (= 42 (plist-get captured-args :number)))
            ;; Should pass comment-id since no discussion_id
            (should (equal 999 (plist-get captured-args :parent-id)))
            (should (string= "Reply" (plist-get captured-args :body)))))))))

(ert-deftest test-shipit-reply-to-inline-comment-dispatches-to-backend ()
  "GIVEN a mock comment backend
WHEN calling shipit--reply-to-inline-comment
THEN backend :reply-to-comment is called with is-inline=t."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:reply-to-comment
         (lambda (config number parent-id body &optional is-inline)
           (setq captured-args (list :number number :parent-id parent-id
                                     :body body :is-inline is-inline))
           '((id . 300) (body . "Inline reply"))))
      (let ((result (shipit--reply-to-inline-comment 42 888 "Inline reply" "file.el")))
        (should result)
        (should (= 42 (plist-get captured-args :number)))
        (should (= 888 (plist-get captured-args :parent-id)))
        (should (string= "Inline reply" (plist-get captured-args :body)))
        (should (eq t (plist-get captured-args :is-inline)))))))

(ert-deftest test-shipit-add-inline-comment-dispatches-to-backend ()
  "GIVEN a mock comment backend
WHEN calling shipit--add-comment-to-pr
THEN backend :add-inline-comment is called with correct args."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:add-inline-comment
         (lambda (config number file line body side &optional old-line)
           (setq captured-args (list :number number :file file :line line
                                     :body body :side side :old-line old-line))
           '((id . 400) (body . "Inline"))))
      (let ((result (shipit--add-comment-to-pr 42 "src/main.el" 10 "Inline" "RIGHT")))
        (should result)
        (should (= 42 (plist-get captured-args :number)))
        (should (string= "src/main.el" (plist-get captured-args :file)))
        (should (= 10 (plist-get captured-args :line)))
        (should (string= "Inline" (plist-get captured-args :body)))
        (should (string= "RIGHT" (plist-get captured-args :side)))))))

(ert-deftest test-shipit-edit-comment-dispatches-to-backend ()
  "GIVEN a mock comment backend
WHEN calling shipit--edit-comment
THEN backend :edit-comment is called with correct args."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:edit-comment
         (lambda (config comment-id body &optional is-inline pr-number)
           (setq captured-args (list :comment-id comment-id :body body
                                     :is-inline is-inline :pr-number pr-number))
           '((id . 500) (body . "Updated"))))
      (cl-letf (((symbol-function 'shipit--update-comment-body-display) #'ignore))
        (let ((result (shipit--edit-comment 500 "Updated" t)))
          (should result)
          (should (= 500 (plist-get captured-args :comment-id)))
          (should (string= "Updated" (plist-get captured-args :body)))
          (should (eq t (plist-get captured-args :is-inline)))
          (should (= 42 (plist-get captured-args :pr-number))))))))

(ert-deftest test-shipit-delete-comment-dispatches-to-backend ()
  "GIVEN a mock comment backend
WHEN calling shipit--delete-comment
THEN backend :delete-comment is called with correct args
     AND deleted comment is tracked."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:delete-comment
         (lambda (config comment-id &optional is-inline)
           (setq captured-args (list :comment-id comment-id :is-inline is-inline))
           t))
      (let ((shipit--deleted-comment-ids (make-hash-table :test 'equal))
            (shipit--reaction-cache (make-hash-table :test 'equal)))
        (cl-letf (((symbol-function 'shipit--reaction-cache-key) (lambda (&rest _) "key")))
          (let ((result (shipit--delete-comment 600 t)))
            (should result)
            (should (= 600 (plist-get captured-args :comment-id)))
            (should (eq t (plist-get captured-args :is-inline)))
            ;; Verify comment tracked as deleted
            (should (gethash 600 shipit--deleted-comment-ids))))))))

;;; Orchestrator Dispatch Tests — Comment Reactions

(ert-deftest test-shipit-add-reaction-to-comment-rest-dispatches-to-backend ()
  "GIVEN a mock comment backend with :toggle-reaction
WHEN calling shipit--add-reaction-to-comment with a non-review comment
THEN backend :toggle-reaction is called with correct args."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:toggle-reaction
         (lambda (config comment-id reaction &optional is-inline)
           (setq captured-args (list :repo (plist-get config :repo)
                                     :comment-id comment-id
                                     :reaction reaction
                                     :is-inline is-inline))
           '((id . 1) (content . "+1"))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit--update-comment-reactions-display) #'ignore)
                ((symbol-function 'shipit--refresh-ediff-comments) #'ignore)
                ((symbol-function 'shipit--reaction-cache-key) (lambda (&rest _) "key")))
        (let ((shipit--reaction-cache (make-hash-table :test 'equal)))
          (let ((result (shipit--add-reaction-to-comment 123 "+1" "owner/repo" 42 t nil)))
            (should result)
            (should (string= "owner/repo" (plist-get captured-args :repo)))
            (should (= 123 (plist-get captured-args :comment-id)))
            (should (string= "+1" (plist-get captured-args :reaction)))
            (should (eq t (plist-get captured-args :is-inline)))))))))

(ert-deftest test-shipit-add-reaction-to-comment-graphql-dispatches-to-backend ()
  "GIVEN a mock comment backend with :add-review-reaction
WHEN calling shipit--add-reaction-to-comment with a review comment that has node_id
THEN backend :add-review-reaction is called."
  (let ((captured-args nil))
    (test-shipit-comment--with-mock-backend
        (:add-review-reaction
         (lambda (config node-id reaction)
           (setq captured-args (list :node-id node-id :reaction reaction))
           '((id . 1))))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit--update-comment-reactions-display) #'ignore)
                ((symbol-function 'shipit--refresh-ediff-comments) #'ignore)
                ((symbol-function 'shipit--reaction-cache-key) (lambda (&rest _) "key")))
        (let ((shipit--reaction-cache (make-hash-table :test 'equal))
              (comment '((shipit-comment-type . "review")
                         (node_id . "PRR_abc123"))))
          (let ((result (shipit--add-reaction-to-comment 123 "+1" "owner/repo" 42 nil comment)))
            (should result)
            (should (string= "PRR_abc123" (plist-get captured-args :node-id)))
            (should (string= "+1" (plist-get captured-args :reaction)))))))))

(ert-deftest test-shipit-remove-reaction-from-comment-rest-dispatches-to-backend ()
  "GIVEN a mock comment backend with :fetch-reactions and :delete-reaction
WHEN calling shipit--remove-reaction-from-comment with a non-review comment
THEN backend :fetch-reactions is called to find user's reaction
     AND backend :delete-reaction is called with the reaction ID."
  (let ((captured-delete-args nil))
    (test-shipit-comment--with-mock-backend
        (:fetch-reactions
         (lambda (config comment-id &optional is-inline)
           '(((id . 99) (content . "+1") (user . ((login . "testuser"))))))
         :delete-reaction
         (lambda (config comment-id reaction-id &optional is-inline)
           (setq captured-delete-args (list :comment-id comment-id
                                            :reaction-id reaction-id
                                            :is-inline is-inline))
           t))
      (cl-letf (((symbol-function 'shipit--ensure-repository) (lambda () t))
                ((symbol-function 'shipit--get-current-user) (lambda () "testuser"))
                ((symbol-function 'shipit--update-comment-reactions-display) #'ignore)
                ((symbol-function 'shipit--refresh-ediff-comments) #'ignore)
                ((symbol-function 'shipit--reaction-cache-key) (lambda (&rest _) "key"))
                ((symbol-function 'shipit-clear-etag-cache-for-endpoint) #'ignore))
        (let ((shipit--reaction-cache (make-hash-table :test 'equal)))
          (let ((result (shipit--remove-reaction-from-comment 123 "+1" "owner/repo" 42 t nil)))
            (should result)
            (should (= 123 (plist-get captured-delete-args :comment-id)))
            (should (= 99 (plist-get captured-delete-args :reaction-id)))
            (should (eq t (plist-get captured-delete-args :is-inline)))))))))

;;; Reaction Dispatch Wrapper Tests

(ert-deftest test-shipit-comment-fetch-reactions-batch-uses-batch-fn ()
  "GIVEN a backend with :fetch-reactions-batch
WHEN calling shipit-comment--fetch-reactions-batch
THEN the batch function is called (not the per-comment fallback)."
  (let ((batch-called nil)
        (single-called nil))
    (test-shipit-comment--with-mock-backend
        (:fetch-reactions-batch
         (lambda (_config _comments _is-inline)
           (setq batch-called t))
         :fetch-reactions
         (lambda (_config _comment-id &optional _is-inline)
           (setq single-called t)
           nil))
      (shipit-comment--fetch-reactions-batch
       '(((id . 1)) ((id . 2))) "owner/repo" nil)
      (should batch-called)
      (should-not single-called))))

(ert-deftest test-shipit-comment-fetch-reactions-batch-falls-back-to-loop ()
  "GIVEN a backend without :fetch-reactions-batch
WHEN calling shipit-comment--fetch-reactions-batch
THEN :fetch-reactions is called for each comment and results are cached."
  (let ((fetched-ids nil)
        (shipit--reaction-cache (make-hash-table :test 'equal)))
    (test-shipit-comment--with-mock-backend
        (:fetch-reactions
         (lambda (_config comment-id &optional _is-inline)
           (push comment-id fetched-ids)
           `(((id . 1) (content . "+1") (user . ((login . "u")))))))
      (shipit-comment--fetch-reactions-batch
       '(((id . 10)) ((id . 20))) "owner/repo" t)
      ;; Both comments should have been fetched
      (should (memq 10 fetched-ids))
      (should (memq 20 fetched-ids))
      ;; Results should be cached
      (should (gethash (shipit--reaction-cache-key "owner/repo" 10 t)
                       shipit--reaction-cache))
      (should (gethash (shipit--reaction-cache-key "owner/repo" 20 t)
                       shipit--reaction-cache)))))

(ert-deftest test-shipit-comment-fetch-reactions-batch-skips-reviews ()
  "GIVEN comments including a review comment
WHEN calling shipit-comment--fetch-reactions-batch with fallback path
THEN review comments are skipped."
  (let ((fetched-ids nil)
        (shipit--reaction-cache (make-hash-table :test 'equal)))
    (test-shipit-comment--with-mock-backend
        (:fetch-reactions
         (lambda (_config comment-id &optional _is-inline)
           (push comment-id fetched-ids)
           nil))
      (shipit-comment--fetch-reactions-batch
       '(((id . 10))
         ((id . 20) (shipit-comment-type . "review"))
         ((id . 30)))
       "owner/repo" nil)
      (should (memq 10 fetched-ids))
      (should-not (memq 20 fetched-ids))
      (should (memq 30 fetched-ids)))))

(ert-deftest test-shipit-comment-fetch-reactions-single ()
  "GIVEN a mock backend
WHEN calling shipit-comment--fetch-reactions for a single comment
THEN :fetch-reactions is called and result is cached."
  (let ((shipit--reaction-cache (make-hash-table :test 'equal)))
    (test-shipit-comment--with-mock-backend
        (:fetch-reactions
         (lambda (_config comment-id &optional _is-inline)
           `(((id . 1) (content . "+1") (user . ((login . "u")))))))
      (shipit-comment--fetch-reactions "owner/repo" 42 t)
      (let ((cached (gethash (shipit--reaction-cache-key "owner/repo" 42 t)
                             shipit--reaction-cache)))
        (should cached)
        (should (= 1 (length cached)))))))

(provide 'test-shipit-comment-backends)
;;; test-shipit-comment-backends.el ends here
