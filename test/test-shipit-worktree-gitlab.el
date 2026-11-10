;;; test-shipit-worktree-gitlab.el --- The worktree of a GitLab merge request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 shipit contributors

;;; Commentary:

;; The Worktree section and the worktree menu work on the page of a
;; GitLab merge request as they do on the page of a GitHub pull request.
;;
;; The page is opened as by hand, with the GitLab backend, so the merge
;; request goes through the normalizer of that backend.  GitLab is
;; answered at the boundary: at curl, which shipit runs for each request
;; that waits for its answer, and at url.el for the others.  A request
;; of the page to GitHub is answered at url.el too.
;;
;; The git commands are real.  The forge is a bare repository on disk.
;; GitLab fetches the source branch of a merge request by its name, so
;; merge request 8 is the branch coffee there.  The clone has only main.
;; It names its remote by the address of the project kitchen/menu/cups
;; at GitLab, and git rewrites that address to the forge.
;;
;; They use the kitchen, the stand-ins and the helpers of
;; test-shipit-worktree-offer.el, test-shipit-pr-worktree-found.el and
;; test-shipit-pr-worktree-found-cases.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-vars)
(require 'shipit-pr-gitlab)
(require 'shipit-comment-gitlab)
(require 'test-shipit-pr-worktree-found-cases)

;;; The kitchen: a forge of a GitLab project, a seed and a clone

(defconst test-worktree-gitlab--project "kitchen/menu/cups"
  "The GitLab project of merge request 8, in a subgroup.")

(defconst test-worktree-gitlab--address "https://gitlab.example.com/kitchen/menu/cups.git"
  "The address of the project at GitLab, which git rewrites to the forge.")

(defconst test-worktree-gitlab--other-address "https://gitlab.example.com/acme/other.git"
  "The address of the project acme/other at GitLab, which git rewrites to its forge.")

(defconst test-worktree-gitlab--config
  '(:api-url "https://gitlab.example.com" :token "not-a-token")
  "The settings of the GitLab backend for the page.")

(defun test-worktree-gitlab--build-forge (forge)
  "Make the bare repository FORGE, with the branches main and coffee of the seed."
  (test-worktree-offer--git "." "init" "-q" "--bare" "-b" "main" forge)
  (test-worktree-offer--git forge "fetch" "-q" (test-worktree-offer--path "seed")
                            "main:refs/heads/main" "coffee:refs/heads/coffee"))

(defun test-worktree-gitlab--origin-names (address forge)
  "Make the remote origin of the clone name ADDRESS, which git rewrites to FORGE."
  (test-worktree-offer--git "clone" "config" "remote.origin.url" address)
  (test-worktree-offer--git "clone" "config"
                            (format "url.%s.insteadOf" (test-worktree-offer--path forge))
                            address))

(defun test-worktree-gitlab--build-kitchen ()
  "Make the seed, the forge of the project, and a clone of it with only main."
  (test-worktree-offer--build-seed)
  (test-worktree-gitlab--build-forge "forge.git")
  (test-worktree-offer--git "." "clone" "-q" "--no-local" "--single-branch" "-b" "main"
                            (test-worktree-offer--path "forge.git") "clone")
  (test-worktree-gitlab--origin-names test-worktree-gitlab--address "forge.git"))

;;; What each scenario is given, before the page opens

(defun test-worktree-gitlab--clone-of-other-with-pr-8-coffee ()
  "Make the clone a clone of acme/other, with a worktree .worktrees/pr-8-coffee at main."
  (test-worktree-gitlab--build-forge "other.git")
  (test-worktree-gitlab--origin-names test-worktree-gitlab--other-address "other.git")
  (test-worktree-offer--worktree-behind-the-pr-head))

;;; Merge request 8, as GitLab gives it

(defun test-worktree-gitlab--merge-request ()
  "Merge request 8 of the project, from its branch coffee to main, as GitLab gives it."
  (let ((main (test-worktree-offer--sha "main"))
        (coffee (test-worktree-offer--sha "coffee")))
    `((iid . 8) (title . "Boil two cups") (description . "Pour it slowly.")
      (state . "opened") (draft . :json-false) (author (username . "me"))
      (source_branch . "coffee") (target_branch . "main")
      (source_project_id . 5) (target_project_id . 5)
      (sha . ,coffee)
      (diff_refs (base_sha . ,main) (start_sha . ,main) (head_sha . ,coffee))
      (references (short . "!8") (relative . "!8") (full . "kitchen/menu/cups!8"))
      (web_url . "https://gitlab.example.com/kitchen/menu/cups/-/merge_requests/8")
      (labels . []) (assignees . []) (user_notes_count . 0)
      (created_at . "2026-01-01T10:00:00Z") (updated_at . "2026-01-01T10:00:00Z"))))

(defun test-worktree-gitlab--of-project (path)
  "The pattern of the address of PATH in the API of the project at GitLab."
  (concat (regexp-quote "gitlab.example.com/api/v4/projects/kitchen%2Fmenu%2Fcups") path))

(defun test-worktree-gitlab--of-mr (path)
  "The pattern of the address of PATH in the API of merge request 8."
  (test-worktree-gitlab--of-project (concat "/merge_requests/8" path)))

(defun test-worktree-gitlab--answers ()
  "The answers of GitLab about merge request 8 and its project.
The last two answer what the page of a merge request asks GitHub: the
reactions of the description and a query of GraphQL."
  `((,(test-worktree-gitlab--of-mr "\\'") 200 ,(test-worktree-gitlab--merge-request))
    (,(test-worktree-gitlab--of-mr "/approvals\\'") 200
     ((approved . :json-false) (approvals_left . 0) (approved_by . [])))
    (,(test-worktree-gitlab--of-mr "/resource_state_events\\'") 200 [])
    (,(test-worktree-gitlab--of-mr "/notes\\?") 200 [])
    (,(test-worktree-gitlab--of-mr "/discussions\\?") 200 [])
    (,(test-worktree-gitlab--of-mr "/commits\\'") 200 [])
    (,(test-worktree-gitlab--of-mr "/changes\\'") 200 ((changes . [])))
    (,(test-worktree-gitlab--of-project "/pipelines\\?") 200 [])
    (,(test-worktree-gitlab--of-project "/repository/compare\\?") 200 ((commits . []) (diffs . [])))
    (,(test-worktree-gitlab--of-project "/notification_settings\\'") 200 ((level . "global")))
    ("gitlab\\.example\\.com/api/v4/projects\\?starred=true" 200 [])
    ("gitlab\\.example\\.com/api/v4/user\\'" 200 ((id . 1) (username . "me")))
    ("api\\.github\\.com/repos/kitchen/menu/cups/issues/8/reactions\\'" 200 [])
    ,shipit-test-http-graphql-empty))

;;; GitLab, answered at curl

(defun test-worktree-gitlab--option (args option)
  "The value that follows OPTION in ARGS, the arguments of curl, or nil."
  (cadr (member option args)))

(defun test-worktree-gitlab--answer-curl (answers destination args)
  "Answer the request of curl with ARGS from ANSWERS, into the buffer DESTINATION.
A request that asks with -D for the headers gets a status line there.
Curl exits with 0 for each status, as it does without --fail."
  (let* ((url-request-method (test-worktree-gitlab--option args "-X"))
         (url-request-data (test-worktree-gitlab--option args "-d"))
         (url-request-extra-headers nil)
         (address (car (last args)))
         (answer (shipit-test-http--find answers address))
         (headers-file (test-worktree-gitlab--option args "-D")))
    (shipit-test-http--record address)
    (when headers-file
      (with-temp-file headers-file
        (insert (format "HTTP/1.1 %d\n" (nth 1 answer)))))
    (with-current-buffer destination
      (insert (decode-coding-string
               (shipit-test-http--body (shipit-test-http--body-for (nth 2 answer)))
               'utf-8)))
    0))

(defun test-worktree-gitlab--curl-stand-in (answers)
  "A stand-in for `call-process' that answers curl from ANSWERS.
Each other program runs, as before."
  (let ((call-process (symbol-function 'call-process)))
    (lambda (program &optional infile destination display &rest args)
      (if (equal program "curl")
          (test-worktree-gitlab--answer-curl answers destination args)
        (apply call-process program infile destination display args)))))

(defun test-worktree-gitlab--answering (scenario)
  "Run SCENARIO, a function, with GitLab and GitHub answered at curl and url.el.
A request that no answer covers fails it, as in `shipit-test-http-answering'."
  (let ((answers (test-worktree-gitlab--answers)))
    (shipit-test-http-answering answers
      (cl-letf (((symbol-function 'call-process)
                 (test-worktree-gitlab--curl-stand-in answers)))
        (funcall scenario)))))

;;; A scenario

(defun test-worktree-gitlab--open-page ()
  "Open the page of merge request 8 from the clone, as by hand, with the GitLab backend."
  (let ((default-directory (test-worktree-offer--path "clone/")))
    (setq test-worktree-offer--page
          (shipit-open-pr-buffer 8 test-worktree-gitlab--project 'gitlab
                                 test-worktree-gitlab--config))
    (test-page-views--settle)))

(defun test-worktree-gitlab--on-the-page (given scenario)
  "Run SCENARIO on the page of merge request 8, in a kitchen that GIVEN prepared.
GIVEN and SCENARIO are functions.  GIVEN runs before the page opens.
Magit and project.el know no repository, so the page runs git in the
clone of the kitchen, whatever project that is a clone of."
  (let ((project--list nil)
        (magit-repository-directories nil)
        (test-pr-worktree-found--my-commit nil))
    (test-worktree-offer--in-a-new-kitchen
     (lambda ()
       (test-worktree-gitlab--build-kitchen)
       (funcall given)
       (test-page-views--as-me
         (test-worktree-gitlab--answering
          (lambda ()
            (cl-letf (((symbol-function 'y-or-n-p) #'test-worktree-offer--ask)
                      ((symbol-function 'message) #'test-worktree-offer--echo))
              (test-worktree-gitlab--open-page)
              (funcall scenario)))))))))

(defmacro test-worktree-gitlab--on-the-page-of-mr-8 (given &rest body)
  "Run BODY on the page of merge request 8, in a kitchen that GIVEN prepared.
GIVEN is a function that prepares the forge or the clone."
  (declare (indent 1))
  `(test-worktree-gitlab--on-the-page ,given (lambda () ,@body)))

;;; What I see

(defun test-worktree-gitlab--asked-to-delete ()
  "Check that the one question was whether to delete the worktree pr-8-coffee."
  (should (= 1 (length test-worktree-offer--questions)))
  (should (string-match-p "\\`Delete worktree at .*/\\.worktrees/pr-8-coffee"
                          (car test-worktree-offer--questions))))

;;; The scenarios

(ert-deftest test-worktree-gitlab-the-section-says-there-is-no-worktree ()
  "The Worktree section of a merge request without a worktree says that it has none."
  ;; GIVEN the page of merge request 8 of kitchen/menu/cups, which has no
  ;;   worktree, in a clone of that project on main
  ;; THEN the Worktree section says "Status: No worktree checked out"
  ;; AND it does not say that the repository of the merge request is not
  ;;   known
  (test-worktree-gitlab--on-the-page-of-mr-8 #'test-worktree-offer--clone-on-main
    (test-pr-worktree-found--worktree-section-says "Status: No worktree checked out\n")
    (should-not (string-search "is not known" (test-worktree-offer--worktree-section)))))

(ert-deftest test-worktree-gitlab-checkout-makes-shipits-own-worktree ()
  "Checkout in worktree makes shipit's own worktree of a merge request, from the forge."
  ;; GIVEN the page of merge request 8, which has no worktree, in a clone
  ;;   that has only main
  ;; WHEN I run "Checkout in worktree" of the worktree menu
  ;; THEN it does not refuse
  ;; AND once the worktree is ready, git lists .worktrees/pr-8-coffee, at
  ;;   the head of the branch coffee of the forge
  ;; AND the Worktree section says "Path: .worktrees/pr-8-coffee (made by
  ;;   shipit)" on a line of its own
  (test-worktree-gitlab--on-the-page-of-mr-8 #'test-worktree-offer--clone-on-main
    (should-not (test-pr-worktree-found--run-from-the-worktree-menu
                 #'shipit--checkout-pr-worktree))
    (test-pr-worktree-found--wait-until-ready)
    (should (test-pr-worktree-found--listed-by-git-p "pr-8-coffee"))
    (should (equal (test-worktree-offer--sha "coffee")
                   (test-pr-worktree-found-cases--head-of-pr-8-coffee)))
    (test-pr-worktree-found--worktree-section-says
     "Path: .worktrees/pr-8-coffee (made by shipit)\n")))

(ert-deftest test-worktree-gitlab-sync-and-delete-act-on-shipits-own-worktree ()
  "Sync worktree moves shipit's own worktree to the head, and Delete worktree removes it."
  ;; GIVEN the page of merge request 8, whose worktree made by shipit,
  ;;   .worktrees/pr-8-coffee, is at main, behind the head
  (test-worktree-gitlab--on-the-page-of-mr-8 #'test-worktree-offer--worktree-behind-the-pr-head
    ;; WHEN I run "Sync worktree" of the worktree menu
    ;; THEN it does not refuse, and pr-8-coffee is at the head of the
    ;;   branch coffee on the forge
    (should-not (test-pr-worktree-found--run-from-the-worktree-menu
                 #'shipit--sync-pr-worktree))
    (should (equal (test-worktree-offer--sha "coffee")
                   (test-pr-worktree-found-cases--head-of-pr-8-coffee)))
    ;; WHEN I run "Delete worktree" and answer yes
    ;; THEN it asks whether to delete pr-8-coffee
    ;; AND git lists no worktree pr-8-coffee, and its folder is gone
    (setq test-worktree-offer--answers '(yes))
    (should-not (test-pr-worktree-found--run-from-the-worktree-menu
                 #'shipit--delete-pr-worktree))
    (test-worktree-gitlab--asked-to-delete)
    (should-not (test-pr-worktree-found--listed-by-git-p "pr-8-coffee"))
    (should-not (file-exists-p (test-worktree-offer--worktree)))))

(ert-deftest test-worktree-gitlab-my-worktree-on-the-source-branch-is-mine ()
  "My worktree on the source branch of a merge request of the same project counts as mine."
  ;; GIVEN the page of merge request 8, which comes from the branch coffee
  ;;   of its own project
  ;; AND my worktree kitchen-work on a branch coffee of my own, with a
  ;;   commit of mine, so not at the head of the merge request
  ;; THEN the Worktree section says "kitchen-work (your worktree)"
  (test-worktree-gitlab--on-the-page-of-mr-8 #'test-pr-worktree-found--my-own-branch-coffee
    (test-pr-worktree-found--worktree-section-says "kitchen-work (your worktree)")))

(ert-deftest test-worktree-gitlab-a-clone-of-another-project-has-no-worktree ()
  "In a clone of another project, the merge request has no worktree, and Sync worktree refuses."
  ;; GIVEN the page of merge request 8 of kitchen/menu/cups, in a clone of
  ;;   acme/other
  ;; AND that clone has a worktree .worktrees/pr-8-coffee at main
  ;; THEN the Worktree section says that the clone is no clone of
  ;;   kitchen/menu/cups, and does not name pr-8-coffee
  ;; WHEN I run "Sync worktree" of the worktree menu
  ;; THEN it says that the clone is no clone of kitchen/menu/cups, so this
  ;;   pull request has no worktree here
  ;; AND pr-8-coffee is still at main
  (test-worktree-gitlab--on-the-page-of-mr-8
      #'test-worktree-gitlab--clone-of-other-with-pr-8-coffee
    (test-pr-worktree-found--worktree-section-says
     (format "Status: %s\n" (test-pr-worktree-found-cases--no-clone-text
                             test-worktree-gitlab--project)))
    (should-not (string-search "pr-8-coffee" (test-worktree-offer--worktree-section)))
    (should (equal (test-pr-worktree-found-cases--no-clone-said test-worktree-gitlab--project)
                   (test-pr-worktree-found--run-from-the-worktree-menu
                    #'shipit--sync-pr-worktree)))
    (should (equal (test-worktree-offer--sha "main")
                   (test-pr-worktree-found-cases--head-of-pr-8-coffee)))))

(provide 'test-shipit-worktree-gitlab)
;;; test-shipit-worktree-gitlab.el ends here
