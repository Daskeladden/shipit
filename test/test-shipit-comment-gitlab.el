;;; test-shipit-comment-gitlab.el --- Tests for GitLab comment backend -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the GitLab MR comment backend.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shipit-core)
(require 'shipit-comment-backends)
(require 'shipit-comment-gitlab)
(require 'shipit-gitlab-http)

;;; Registration Tests

(ert-deftest test-shipit-comment-gitlab-registered ()
  "GIVEN shipit-comment-gitlab is loaded
WHEN checking the comment backend registry
THEN 'gitlab is registered with name \"GitLab\"."
  (should (assq 'gitlab shipit-comment-backends))
  (should (string= "GitLab" (plist-get (cdr (assq 'gitlab shipit-comment-backends)) :name))))

(ert-deftest test-shipit-comment-gitlab-has-all-required-keys ()
  "GIVEN shipit-comment-gitlab is loaded
WHEN checking the backend plist
THEN all required keys from `shipit-comment--required-keys' are present."
  (let ((backend (cdr (assq 'gitlab shipit-comment-backends))))
    (dolist (key shipit-comment--required-keys)
      (should (plist-get backend key)))))

;;; Normalization Tests — General Comments

(ert-deftest test-shipit-comment-gitlab-normalize-note ()
  "GIVEN a GitLab MR note response
WHEN normalizing to shipit format
THEN key fields are mapped correctly."
  (let* ((note '((id . 101)
                 (body . "Looks good to me")
                 (author . ((username . "alice")
                            (avatar_url . "https://gitlab.com/alice.png")))
                 (created_at . "2024-01-16T10:00:00.000Z")
                 (updated_at . "2024-01-16T10:00:00.000Z")
                 (system . :json-false)))
         (normalized (shipit-comment-gitlab--normalize-note note)))
    (should (equal 101 (cdr (assq 'id normalized))))
    (should (equal "Looks good to me" (cdr (assq 'body normalized))))
    (should (equal "alice" (cdr (assq 'login (cdr (assq 'user normalized))))))
    (should (equal "https://gitlab.com/alice.png"
                   (cdr (assq 'avatar_url (cdr (assq 'user normalized))))))
    (should (equal "issue" (cdr (assq 'shipit-comment-type normalized))))))

(ert-deftest test-shipit-comment-gitlab-system-note-p ()
  "GIVEN a GitLab note with system=t
WHEN checking if it's a system note
THEN returns non-nil."
  (should (shipit-comment-gitlab--system-note-p
           '((id . 1) (system . t))))
  (should-not (shipit-comment-gitlab--system-note-p
               '((id . 2) (system . :json-false))))
  (should-not (shipit-comment-gitlab--system-note-p
               '((id . 3)))))

;;; Fetch General Comments Tests

(ert-deftest test-shipit-comment-gitlab-fetch-general-comments ()
  "GIVEN a GitLab MR with general, system, and inline discussions
WHEN fetching general comments
THEN system discussions and inline discussions are filtered out."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config _path)
               `(;; General discussion with one user note
                 ((id . "disc-1")
                  (notes . [((id . 1) (body . "User comment") (system . :json-false)
                              (author . ((username . "alice") (avatar_url . "https://a.png")))
                              (created_at . "2024-01-16T10:00:00.000Z")
                              (updated_at . "2024-01-16T10:00:00.000Z"))]))
                 ;; System-only discussion
                 ((id . "disc-2")
                  (notes . [((id . 2) (body . "system note") (system . t)
                              (author . ((username . "alice")))
                              (created_at . "2024-01-16T11:00:00.000Z")
                              (updated_at . "2024-01-16T11:00:00.000Z"))]))
                 ;; Inline discussion (has position)
                 ((id . "disc-3")
                  (notes . [((id . 3) (body . "Diff comment") (system . :json-false)
                              (author . ((username . "bob")))
                              (position . ((new_path . "file.el")))
                              (created_at . "2024-01-16T12:00:00.000Z")
                              (updated_at . "2024-01-16T12:00:00.000Z"))]))
                 ;; Another general discussion
                 ((id . "disc-4")
                  (notes . [((id . 4) (body . "Another general comment") (system . :json-false)
                              (author . ((username . "carol") (avatar_url . "https://c.png")))
                              (created_at . "2024-01-16T13:00:00.000Z")
                              (updated_at . "2024-01-16T13:00:00.000Z"))]))))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (comments (shipit-comment-gitlab--fetch-general-comments config 42)))
      ;; Only user comments from general discussions should remain
      (should (= 2 (length comments)))
      (should (equal 1 (cdr (assq 'id (nth 0 comments)))))
      (should (equal 4 (cdr (assq 'id (nth 1 comments))))))))

;;; Fetch Inline Comments Tests

(ert-deftest test-shipit-comment-gitlab-fetch-inline-comments ()
  "GIVEN a GitLab MR with discussions containing diff notes
WHEN fetching inline comments
THEN diff notes are extracted and normalized."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config _path)
               `(((id . "disc-1")
                  (notes . [((id . 10)
                              (body . "Line comment")
                              (system . :json-false)
                              (author . ((username . "alice") (avatar_url . "https://a.png")))
                              (position . ((new_path . "main.el")
                                           (old_path . "main.el")
                                           (new_line . 42)
                                           (position_type . "text")))
                              (created_at . "2024-01-16T10:00:00.000Z")
                              (updated_at . "2024-01-16T10:00:00.000Z"))]))
                 ((id . "disc-2")
                  (notes . [((id . 20)
                              (body . "General discussion note")
                              (system . :json-false)
                              (author . ((username . "bob")))
                              (created_at . "2024-01-16T11:00:00.000Z")
                              (updated_at . "2024-01-16T11:00:00.000Z"))]))))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (comments (shipit-comment-gitlab--fetch-inline-comments config 5)))
      ;; Only disc-1 has a diff note
      (should (= 1 (length comments)))
      (let ((c (car comments)))
        (should (equal 10 (cdr (assq 'id c))))
        (should (equal "main.el" (cdr (assq 'path c))))
        (should (equal 42 (cdr (assq 'line c))))
        (should (equal "RIGHT" (cdr (assq 'side c))))
        ;; First note in discussion has nil in_reply_to_id (it's the root)
        (should-not (cdr (assq 'in_reply_to_id c)))))))

(ert-deftest test-shipit-comment-gitlab-fetch-inline-comments-threaded ()
  "GIVEN a GitLab discussion with multiple diff notes
WHEN fetching inline comments
THEN first note has nil in_reply_to_id, replies point to first note id."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config _path)
               `(((id . "disc-1")
                  (notes . [((id . 10)
                              (body . "Root comment")
                              (system . :json-false)
                              (author . ((username . "alice") (avatar_url . "https://a.png")))
                              (position . ((new_path . "main.el")
                                           (old_path . "main.el")
                                           (new_line . 42)
                                           (position_type . "text")))
                              (created_at . "2024-01-16T10:00:00.000Z")
                              (updated_at . "2024-01-16T10:00:00.000Z"))
                             ((id . 11)
                              (body . "Reply comment")
                              (system . :json-false)
                              (author . ((username . "bob") (avatar_url . "https://b.png")))
                              (position . ((new_path . "main.el")
                                           (old_path . "main.el")
                                           (new_line . 42)
                                           (position_type . "text")))
                              (created_at . "2024-01-16T11:00:00.000Z")
                              (updated_at . "2024-01-16T11:00:00.000Z"))]))))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (comments (shipit-comment-gitlab--fetch-inline-comments config 5)))
      (should (= 2 (length comments)))
      ;; First note: root — no in_reply_to_id
      (should-not (cdr (assq 'in_reply_to_id (nth 0 comments))))
      (should (equal 10 (cdr (assq 'id (nth 0 comments)))))
      ;; Second note: reply — in_reply_to_id points to first note's id
      (should (equal 10 (cdr (assq 'in_reply_to_id (nth 1 comments)))))
      (should (equal 11 (cdr (assq 'id (nth 1 comments))))))))

(ert-deftest test-shipit-comment-gitlab-normalize-diff-note-left-side ()
  "GIVEN a GitLab diff note with only old_line set
WHEN normalizing
THEN side is \"LEFT\" and line uses old_line."
  (let* ((note '((id . 30)
                 (body . "Old line comment")
                 (author . ((username . "alice")))
                 (position . ((old_path . "old.el")
                              (new_path . "new.el")
                              (old_line . 10)))
                 (created_at . "2024-01-16T10:00:00.000Z")
                 (updated_at . "2024-01-16T10:00:00.000Z")))
         (normalized (shipit-comment-gitlab--normalize-diff-note note "disc-x")))
    (should (equal "LEFT" (cdr (assq 'side normalized))))
    (should (equal 10 (cdr (assq 'line normalized))))
    (should (equal "new.el" (cdr (assq 'path normalized))))))

(ert-deftest test-shipit-comment-gitlab-normalize-diff-note-includes-discussion-id ()
  "GIVEN a GitLab diff note and a discussion ID
WHEN normalizing with the discussion-id parameter
THEN the normalized comment includes discussion_id."
  (let* ((note '((id . 40)
                 (body . "Inline comment")
                 (author . ((username . "alice")))
                 (position . ((new_path . "file.el")
                              (new_line . 5)))
                 (created_at . "2024-01-16T10:00:00.000Z")
                 (updated_at . "2024-01-16T10:00:00.000Z")))
         (normalized (shipit-comment-gitlab--normalize-diff-note note nil "abc123hex")))
    (should (equal "abc123hex" (cdr (assq 'discussion_id normalized))))
    (should (equal 40 (cdr (assq 'id normalized))))))

;;; Add General Comment Tests

(ert-deftest test-shipit-comment-gitlab-add-general-comment ()
  "GIVEN a GitLab config
WHEN adding a general comment
THEN it calls POST on the correct endpoint."
  (let ((captured-path nil)
        (captured-data nil)
        (captured-method nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data method)
                 (setq captured-path path
                       captured-data data
                       captured-method method)
                 '((id . 201)
                   (body . "My comment")
                   (system . :json-false)
                   (author . ((username . "alice")))
                   (created_at . "2024-01-17T10:00:00.000Z")
                   (updated_at . "2024-01-17T10:00:00.000Z")))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-comment-gitlab--add-general-comment config 42 "My comment")))
        (should (string-match-p "/merge_requests/42/notes" captured-path))
        (should (equal "POST" captured-method))
        (should (equal "My comment" (cdr (assq 'body captured-data))))
        (should (equal 201 (cdr (assq 'id result))))))))

;;; Edit Comment Tests

(ert-deftest test-shipit-comment-gitlab-edit-comment ()
  "GIVEN a GitLab config
WHEN editing a note
THEN it calls PUT on the correct endpoint."
  (let ((captured-path nil)
        (captured-data nil)
        (captured-method nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data method)
                 (setq captured-path path
                       captured-data data
                       captured-method method)
                 '((id . 101)
                   (body . "Updated text")
                   (system . :json-false)
                   (author . ((username . "alice")))
                   (created_at . "2024-01-17T10:00:00.000Z")
                   (updated_at . "2024-01-17T12:00:00.000Z")))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-comment-gitlab--edit-comment config 101 "Updated text" nil 42)))
        (should (string-match-p "/merge_requests/42/notes/101" captured-path))
        (should (equal "PUT" captured-method))
        (should (equal "Updated text" (cdr (assq 'body captured-data))))
        (should (equal 101 (cdr (assq 'id result))))))))

;;; Delete Comment Tests

(ert-deftest test-shipit-comment-gitlab-delete-comment ()
  "GIVEN a GitLab config and active MR context
WHEN deleting a note
THEN it calls DELETE on the correct endpoint."
  (let ((captured-path nil)
        (captured-method nil)
        (shipit--current-displayed-pr '(42 "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path _data method)
                 (setq captured-path path
                       captured-method method)
                 nil)))
      (let ((config '(:project-path "mygroup/myproject")))
        (should (shipit-comment-gitlab--delete-comment config 101))
        (should (string-match-p "/merge_requests/42/notes/101" captured-path))
        (should (equal "DELETE" captured-method))))))

;;; Reply to Comment Tests

(ert-deftest test-shipit-comment-gitlab-reply-to-comment ()
  "GIVEN a GitLab config
WHEN replying to a discussion
THEN it calls POST on the discussion notes endpoint."
  (let ((captured-path nil)
        (captured-data nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data _method)
                 (setq captured-path path
                       captured-data data)
                 '((id . 301)
                   (body . "Reply text")
                   (system . :json-false)
                   (author . ((username . "bob")))
                   (created_at . "2024-01-18T10:00:00.000Z")
                   (updated_at . "2024-01-18T10:00:00.000Z")))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-comment-gitlab--reply-to-comment config 42 "disc-abc" "Reply text")))
        (should (string-match-p "/merge_requests/42/discussions/disc-abc/notes" captured-path))
        (should (equal "Reply text" (cdr (assq 'body captured-data))))
        (should (equal 301 (cdr (assq 'id result))))))))

;;; Toggle Reaction Tests

(ert-deftest test-shipit-comment-gitlab-toggle-reaction ()
  "GIVEN a GitLab config and active MR context
WHEN toggling a reaction on a note
THEN it calls POST on the award_emoji endpoint."
  (let ((captured-path nil)
        (captured-data nil)
        (shipit--current-displayed-pr '(42 "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data _method)
                 (setq captured-path path
                       captured-data data)
                 '((id . 1) (name . "thumbsup")))))
      (let ((config '(:project-path "mygroup/myproject")))
        (shipit-comment-gitlab--toggle-reaction config 101 "thumbsup")
        (should (string-match-p "/notes/101/award_emoji" captured-path))
        (should (equal "thumbsup" (cdr (assq 'name captured-data))))))))

;;; Fetch Reactions Tests

(ert-deftest test-shipit-comment-gitlab-fetch-reactions ()
  "GIVEN a GitLab config and active MR context
WHEN fetching reactions for a note
THEN it calls GET on the award_emoji endpoint
     AND returns normalized reactions with content and user.login keys."
  (let ((captured-path nil)
        (shipit--current-displayed-pr '(42 "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config path)
                 (setq captured-path path)
                 `[((id . 1) (name . "thumbsup") (user . ((username . "alice"))))
                   ((id . 2) (name . "heart") (user . ((username . "bob"))))])))
      (let* ((config '(:project-path "mygroup/myproject"))
             (reactions (shipit-comment-gitlab--fetch-reactions config 101)))
        (should (string-match-p "/notes/101/award_emoji" captured-path))
        (should (= 2 (length reactions)))
        ;; Verify normalization: name → content, user.username → user.login
        (let ((first (car reactions)))
          (should (equal "+1" (cdr (assq 'content first))))
          (should (equal "alice" (cdr (assq 'login (cdr (assq 'user first))))))
          (should (equal 1 (cdr (assq 'id first)))))
        (let ((second (cadr reactions)))
          (should (equal "heart" (cdr (assq 'content second))))
          (should (equal "bob" (cdr (assq 'login (cdr (assq 'user second)))))))))))

;;; Reaction Normalization Tests

(ert-deftest test-shipit-comment-gitlab-normalize-reaction-maps-emoji-names ()
  "GIVEN GitLab award_emoji alists with various emoji names
WHEN normalizing them
THEN GitLab names are mapped to shipit-standard content types."
  (let ((cases '(("thumbsup" . "+1")
                 ("thumbsdown" . "-1")
                 ("laughing" . "laugh")
                 ("tada" . "hooray")
                 ("rocket" . "rocket")
                 ("eyes" . "eyes"))))
    (dolist (pair cases)
      (let* ((emoji `((id . 1) (name . ,(car pair)) (user . ((username . "u")))))
             (normalized (shipit-comment-gitlab--normalize-reaction emoji)))
        (should (equal (cdr pair) (cdr (assq 'content normalized))))))))

(ert-deftest test-shipit-comment-gitlab-normalize-reaction-unknown-name ()
  "GIVEN a GitLab award_emoji with an unknown emoji name
WHEN normalizing
THEN the original name is preserved as content."
  (let* ((emoji '((id . 5) (name . "custom_emoji") (user . ((username . "alice")))))
         (normalized (shipit-comment-gitlab--normalize-reaction emoji)))
    (should (equal "custom_emoji" (cdr (assq 'content normalized))))
    (should (equal "alice" (cdr (assq 'login (cdr (assq 'user normalized))))))))

;;; Delete Reaction Tests

(ert-deftest test-shipit-comment-gitlab-delete-reaction ()
  "GIVEN a GitLab config and active MR context
WHEN deleting a reaction
THEN it calls DELETE on the specific award_emoji endpoint."
  (let ((captured-path nil)
        (captured-method nil)
        (shipit--current-displayed-pr '(42 "mygroup/myproject")))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path _data method)
                 (setq captured-path path
                       captured-method method)
                 nil)))
      (let ((config '(:project-path "mygroup/myproject")))
        (should (shipit-comment-gitlab--delete-reaction config 101 5))
        (should (string-match-p "/notes/101/award_emoji/5" captured-path))
        (should (equal "DELETE" captured-method))))))

;;; Add Inline Comment Tests

(ert-deftest test-shipit-comment-gitlab-add-inline-comment ()
  "GIVEN a GitLab config
WHEN adding an inline comment
THEN it calls POST on the discussions endpoint with position."
  (let ((captured-path nil)
        (captured-data nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (_config _path)
                 ;; GIVEN diff_refs available from MR data
                 '((diff_refs . ((base_sha . "aaa111")
                                 (head_sha . "bbb222")
                                 (start_sha . "ccc333"))))))
              ((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data _method)
                 (setq captured-path path
                       captured-data data)
                 '((id . "disc-new")
                   (notes . [((id . 501)
                               (body . "Inline note")
                               (system . :json-false)
                               (author . ((username . "alice")))
                               (position . ((new_path . "main.el")
                                            (old_path . "main.el")
                                            (new_line . 10)))
                               (created_at . "2024-01-19T10:00:00.000Z")
                               (updated_at . "2024-01-19T10:00:00.000Z"))])))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-comment-gitlab--add-inline-comment
                      config 42 "main.el" 10 "Inline note" "RIGHT")))
        ;; THEN it calls POST on the discussions endpoint
        (should (string-match-p "/merge_requests/42/discussions" captured-path))
        (should (equal "Inline note" (cdr (assq 'body captured-data))))
        ;; THEN position includes diff ref SHAs and line info
        (let ((pos (cdr (assq 'position captured-data))))
          (should (equal 10 (cdr (assq 'new_line pos))))
          (should (equal "aaa111" (cdr (assq 'base_sha pos))))
          (should (equal "bbb222" (cdr (assq 'head_sha pos))))
          (should (equal "ccc333" (cdr (assq 'start_sha pos)))))
        (should (equal 501 (cdr (assq 'id result))))))))

;;; Batch Reactions Tests

(ert-deftest test-shipit-comment-gitlab-fetch-reactions-batch-no-api-calls ()
  "GIVEN a list of 3 GitLab comments
WHEN calling fetch-reactions-batch
THEN no API calls are made (reactions deferred to on-demand)
     AND empty lists are cached for each comment."
  (let* ((shipit--reaction-cache (make-hash-table :test 'equal))
         (config (list :gitlab-url "https://gitlab.com"
                       :project-id "test/project"
                       :gitlab-token "token"))
         (comments (list '((id . 101)) '((id . 102)) '((id . 103))))
         (api-call-count 0))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request)
               (lambda (&rest _) (setq api-call-count (1+ api-call-count)) nil))
              ((symbol-function 'shipit--reaction-cache-key)
               (lambda (_repo comment-id _is-inline) (format "key:%s" comment-id))))
      (shipit-comment-gitlab--fetch-reactions-batch config comments t)
      ;; THEN no API calls were made
      (should (= 0 api-call-count))
      ;; THEN empty lists cached for each comment
      (should (equal '() (gethash "key:101" shipit--reaction-cache)))
      (should (equal '() (gethash "key:102" shipit--reaction-cache)))
      (should (equal '() (gethash "key:103" shipit--reaction-cache))))))

(ert-deftest test-shipit-comment-gitlab-backend-has-fetch-reactions-batch ()
  "GIVEN the GitLab comment backend registration
WHEN checking for :fetch-reactions-batch
THEN it exists on the backend plist."
  (let ((backend (cdr (assq 'gitlab shipit-comment-backends))))
    (should (plist-get backend :fetch-reactions-batch))))

;;; Normalize Note with Threading Fields Tests

(ert-deftest test-shipit-comment-gitlab-normalize-note-with-threading-fields ()
  "GIVEN a GitLab MR note response
WHEN normalizing with in-reply-to-id and discussion-id
THEN output includes in_reply_to_id and discussion_id fields."
  (let* ((note '((id . 201)
                 (body . "A reply")
                 (author . ((username . "bob")
                            (avatar_url . "https://gitlab.com/bob.png")))
                 (created_at . "2024-01-16T11:00:00.000Z")
                 (updated_at . "2024-01-16T11:00:00.000Z")
                 (system . :json-false)))
         (normalized (shipit-comment-gitlab--normalize-note note 100 "disc-abc")))
    (should (equal 201 (cdr (assq 'id normalized))))
    (should (equal "A reply" (cdr (assq 'body normalized))))
    (should (equal 100 (cdr (assq 'in_reply_to_id normalized))))
    (should (equal "disc-abc" (cdr (assq 'discussion_id normalized))))
    (should (equal "issue" (cdr (assq 'shipit-comment-type normalized))))))

(ert-deftest test-shipit-comment-gitlab-normalize-note-without-threading-fields ()
  "GIVEN a GitLab MR note response
WHEN normalizing without in-reply-to-id and discussion-id (root note)
THEN output has nil in_reply_to_id and nil discussion_id."
  (let* ((note '((id . 101)
                 (body . "Root comment")
                 (author . ((username . "alice")
                            (avatar_url . "https://gitlab.com/alice.png")))
                 (created_at . "2024-01-16T10:00:00.000Z")
                 (updated_at . "2024-01-16T10:00:00.000Z")
                 (system . :json-false)))
         (normalized (shipit-comment-gitlab--normalize-note note)))
    (should (equal 101 (cdr (assq 'id normalized))))
    (should-not (cdr (assq 'in_reply_to_id normalized)))
    (should-not (cdr (assq 'discussion_id normalized)))))

;;; Fetch General Comments via Discussions API Tests

(ert-deftest test-shipit-comment-gitlab-fetch-general-comments-via-discussions ()
  "GIVEN a GitLab MR with discussions containing general and inline notes
WHEN fetching general comments
THEN only general (non-position) notes are returned with threading info."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config path)
               ;; Verify it calls /discussions endpoint
               (should (string-match-p "/discussions" path))
               `(;; Discussion 1: general thread with 2 notes
                 ((id . "disc-1")
                  (notes . [((id . 10)
                              (body . "Root general comment")
                              (system . :json-false)
                              (author . ((username . "alice") (avatar_url . "https://a.png")))
                              (created_at . "2024-01-16T10:00:00.000Z")
                              (updated_at . "2024-01-16T10:00:00.000Z"))
                             ((id . 11)
                              (body . "Reply to root")
                              (system . :json-false)
                              (author . ((username . "bob") (avatar_url . "https://b.png")))
                              (created_at . "2024-01-16T11:00:00.000Z")
                              (updated_at . "2024-01-16T11:00:00.000Z"))]))
                 ;; Discussion 2: inline discussion (has position) — should be skipped
                 ((id . "disc-2")
                  (notes . [((id . 20)
                              (body . "Inline comment")
                              (system . :json-false)
                              (author . ((username . "carol")))
                              (position . ((new_path . "file.el") (new_line . 5)))
                              (created_at . "2024-01-16T12:00:00.000Z")
                              (updated_at . "2024-01-16T12:00:00.000Z"))]))
                 ;; Discussion 3: system-only discussion — should be skipped
                 ((id . "disc-3")
                  (notes . [((id . 30)
                              (body . "merged branch")
                              (system . t)
                              (author . ((username . "system")))
                              (created_at . "2024-01-16T13:00:00.000Z")
                              (updated_at . "2024-01-16T13:00:00.000Z"))]))))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (comments (shipit-comment-gitlab--fetch-general-comments config 42)))
      ;; Should have 2 comments from disc-1 (general thread)
      (should (= 2 (length comments)))
      ;; First comment is root (no in_reply_to_id)
      (let ((root (nth 0 comments)))
        (should (equal 10 (cdr (assq 'id root))))
        (should-not (cdr (assq 'in_reply_to_id root)))
        (should (equal "disc-1" (cdr (assq 'discussion_id root)))))
      ;; Second comment is reply (in_reply_to_id = first note's id)
      (let ((reply (nth 1 comments)))
        (should (equal 11 (cdr (assq 'id reply))))
        (should (equal 10 (cdr (assq 'in_reply_to_id reply))))
        (should (equal "disc-1" (cdr (assq 'discussion_id reply))))))))

(ert-deftest test-shipit-comment-gitlab-fetch-general-skips-mixed-discussions ()
  "GIVEN a discussion where first note has position but second doesn't
WHEN fetching general comments
THEN the entire discussion is treated as inline (skipped)."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config _path)
               `(((id . "disc-mixed")
                  (notes . [((id . 40)
                              (body . "Inline root")
                              (system . :json-false)
                              (author . ((username . "alice")))
                              (position . ((new_path . "file.el") (new_line . 10)))
                              (created_at . "2024-01-16T10:00:00.000Z")
                              (updated_at . "2024-01-16T10:00:00.000Z"))
                             ((id . 41)
                              (body . "Reply without position")
                              (system . :json-false)
                              (author . ((username . "bob")))
                              (created_at . "2024-01-16T11:00:00.000Z")
                              (updated_at . "2024-01-16T11:00:00.000Z"))]))))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (comments (shipit-comment-gitlab--fetch-general-comments config 42)))
      ;; Inline discussions should be completely skipped
      (should (= 0 (length comments))))))

(ert-deftest test-shipit-comment-gitlab-fetch-general-filters-system-notes-in-thread ()
  "GIVEN a general discussion with a system note mixed in
WHEN fetching general comments
THEN system notes are excluded but user notes are kept."
  (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
             (lambda (_config _path)
               `(((id . "disc-sys")
                  (notes . [((id . 50)
                              (body . "User comment")
                              (system . :json-false)
                              (author . ((username . "alice") (avatar_url . "https://a.png")))
                              (created_at . "2024-01-16T10:00:00.000Z")
                              (updated_at . "2024-01-16T10:00:00.000Z"))
                             ((id . 51)
                              (body . "changed the description")
                              (system . t)
                              (author . ((username . "alice")))
                              (created_at . "2024-01-16T11:00:00.000Z")
                              (updated_at . "2024-01-16T11:00:00.000Z"))
                             ((id . 52)
                              (body . "Another user reply")
                              (system . :json-false)
                              (author . ((username . "bob") (avatar_url . "https://b.png")))
                              (created_at . "2024-01-16T12:00:00.000Z")
                              (updated_at . "2024-01-16T12:00:00.000Z"))]))))))
    (let* ((config '(:project-path "mygroup/myproject"))
           (comments (shipit-comment-gitlab--fetch-general-comments config 42)))
      ;; 2 user notes, system note filtered
      (should (= 2 (length comments)))
      (should (equal 50 (cdr (assq 'id (nth 0 comments)))))
      (should (equal 52 (cdr (assq 'id (nth 1 comments)))))
      ;; Reply points to first user note (skipping system note)
      (should (equal 50 (cdr (assq 'in_reply_to_id (nth 1 comments))))))))

;;; Reply with Discussion ID Tests

(ert-deftest test-shipit-comment-gitlab-reply-uses-discussion-id ()
  "GIVEN cached general comments with discussion_id
WHEN replying to a general comment
THEN the discussion_id is passed to the backend instead of note id."
  (let ((captured-parent-id nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-method)
               (lambda (_config path data _method)
                 ;; Capture the discussion ID from the path
                 (when (string-match "/discussions/\\([^/]+\\)/notes" path)
                   (setq captured-parent-id (match-string 1 path)))
                 '((id . 301)
                   (body . "Reply text")
                   (system . :json-false)
                   (author . ((username . "bob")))
                   (created_at . "2024-01-18T10:00:00.000Z")
                   (updated_at . "2024-01-18T10:00:00.000Z")))))
      (let* ((config '(:project-path "mygroup/myproject"))
             (result (shipit-comment-gitlab--reply-to-comment config 42 "disc-abc" "Reply text")))
        ;; Backend receives discussion ID, which is used in the URL path
        (should (equal "disc-abc" captured-parent-id))
        (should (equal 301 (cdr (assq 'id result))))))))

;;; Shared Discussions Cache Tests

(defvar test-gitlab-discussions-fixture
  `(;; General discussion
    ((id . "disc-1")
     (notes . [((id . 1) (body . "General note") (system . :json-false)
                (author . ((username . "alice") (avatar_url . "https://a.png")))
                (created_at . "2024-01-16T10:00:00.000Z")
                (updated_at . "2024-01-16T10:00:00.000Z"))]))
    ;; Inline discussion
    ((id . "disc-2")
     (notes . [((id . 2) (body . "Diff note") (system . :json-false)
                (author . ((username . "bob") (avatar_url . "https://b.png")))
                (position . ((new_path . "main.el") (old_path . "main.el")
                             (new_line . 42) (position_type . "text")))
                (created_at . "2024-01-16T11:00:00.000Z")
                (updated_at . "2024-01-16T11:00:00.000Z"))])))
  "Test fixture with one general and one inline discussion.")

(ert-deftest test-shipit-comment-gitlab-fetch-discussions-caches-result ()
  "GIVEN a GitLab MR with discussions
WHEN calling fetch-discussions twice
THEN the API is called only once and the cached result is returned."
  (let ((api-call-count 0)
        (shipit-comment-gitlab--discussions-cache nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
               (lambda (_config _path)
                 (setq api-call-count (1+ api-call-count))
                 test-gitlab-discussions-fixture)))
      (let ((config '(:project-path "mygroup/myproject")))
        ;; First call — hits API
        (let ((result1 (shipit-comment-gitlab--fetch-discussions config 42)))
          (should (= 1 api-call-count))
          (should (= 2 (length result1))))
        ;; Second call — returns from cache
        (let ((result2 (shipit-comment-gitlab--fetch-discussions config 42)))
          (should (= 1 api-call-count))
          (should (= 2 (length result2))))))))

(ert-deftest test-shipit-comment-gitlab-general-and-inline-share-cache ()
  "GIVEN a GitLab MR with discussions
WHEN fetching general then inline comments
THEN only one API call is made total (shared discussions cache)."
  (let ((api-call-count 0)
        (shipit-comment-gitlab--discussions-cache nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
               (lambda (_config _path)
                 (setq api-call-count (1+ api-call-count))
                 test-gitlab-discussions-fixture)))
      (let ((config '(:project-path "mygroup/myproject")))
        ;; Fetch general comments — warms cache
        (let ((general (shipit-comment-gitlab--fetch-general-comments config 42)))
          (should (= 1 api-call-count))
          (should (= 1 (length general)))
          (should (equal 1 (cdr (assq 'id (car general))))))
        ;; Fetch inline comments — uses same cache
        (let ((inline (shipit-comment-gitlab--fetch-inline-comments config 42)))
          (should (= 1 api-call-count))
          (should (= 1 (length inline)))
          (should (equal 2 (cdr (assq 'id (car inline))))))))))

(ert-deftest test-shipit-comment-gitlab-clear-discussions-cache ()
  "GIVEN a warm discussions cache
WHEN clearing the cache
THEN subsequent fetch hits the API again."
  (let ((api-call-count 0)
        (shipit-comment-gitlab--discussions-cache nil))
    (cl-letf (((symbol-function 'shipit-gitlab--api-request-paginated)
               (lambda (_config _path)
                 (setq api-call-count (1+ api-call-count))
                 test-gitlab-discussions-fixture)))
      (let ((config '(:project-path "mygroup/myproject")))
        ;; Warm the cache
        (shipit-comment-gitlab--fetch-discussions config 42)
        (should (= 1 api-call-count))
        ;; Clear the cache
        (shipit-comment-gitlab--clear-discussions-cache)
        (should-not shipit-comment-gitlab--discussions-cache)
        ;; Next fetch hits API again
        (shipit-comment-gitlab--fetch-discussions config 42)
        (should (= 2 api-call-count))))))

(ert-deftest test-shipit-comment-gitlab-backend-has-clear-discussions-cache ()
  "GIVEN the GitLab comment backend
WHEN checking the plist
THEN :clear-discussions-cache is registered."
  (let ((backend (cdr (assq 'gitlab shipit-comment-backends))))
    (should (plist-get backend :clear-discussions-cache))))

;;; Post-Comment Hook Tests

(ert-deftest test-shipit-comment-gitlab-post-add-comment-clears-cache ()
  "GIVEN a warm discussions cache
WHEN the post-add-comment hook fires
THEN the discussions cache is cleared."
  (let ((shipit-comment-gitlab--discussions-cache '(some cached data)))
    (shipit-comment-gitlab--post-add-comment
     '(:project-path "mygroup/myproject") 201 nil "mygroup/myproject" 42)
    (should-not shipit-comment-gitlab--discussions-cache)))

(ert-deftest test-shipit-comment-gitlab-post-edit-comment-clears-cache ()
  "GIVEN a warm discussions cache
WHEN the post-edit-comment hook fires
THEN the discussions cache is cleared."
  (let ((shipit-comment-gitlab--discussions-cache '(some cached data)))
    (shipit-comment-gitlab--post-edit-comment
     '(:project-path "mygroup/myproject") 101 nil "mygroup/myproject" 42)
    (should-not shipit-comment-gitlab--discussions-cache)))

(ert-deftest test-shipit-comment-gitlab-post-delete-comment-clears-cache ()
  "GIVEN a warm discussions cache
WHEN the post-delete-comment hook fires
THEN the discussions cache is cleared."
  (let ((shipit-comment-gitlab--discussions-cache '(some cached data)))
    (shipit-comment-gitlab--post-delete-comment
     '(:project-path "mygroup/myproject") 101 nil "mygroup/myproject" 42)
    (should-not shipit-comment-gitlab--discussions-cache)))

(ert-deftest test-shipit-comment-gitlab-backend-has-post-hooks ()
  "GIVEN the GitLab comment backend
WHEN checking the plist
THEN post-add-comment, post-edit-comment, post-delete-comment are registered."
  (let ((backend (cdr (assq 'gitlab shipit-comment-backends))))
    (should (plist-get backend :post-add-comment))
    (should (plist-get backend :post-edit-comment))
    (should (plist-get backend :post-delete-comment))))

(provide 'test-shipit-comment-gitlab)
;;; test-shipit-comment-gitlab.el ends here
