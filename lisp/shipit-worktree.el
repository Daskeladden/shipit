;;; shipit-worktree.el --- Worktree management for PR testing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Manages git worktrees for PR branches, tracking sync state and metadata.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'shipit-core)
(require 'shipit-pr-backends)
(require 'json)

(declare-function shipit--get-repo-from-remote "shipit-core")

(defcustom shipit-worktree-fetch-blobless nil
  "When non-nil, `git fetch\=' for PR refs uses `--filter=blob:none\='.
Blobless fetch downloads commits and trees without file contents;
blobs are then expected to stream in lazily on demand at
checkout time.

Defaults to nil because the lazy-fetch path is *slow* during
`git worktree add\=' -- materialising N files triggers up to N
round-trips to the promisor remote, one per missing blob, which
scales worse than a single full fetch on real PRs (a warm LFS-
heavy repo has been measured at ~100s vs ~5s with the flag
off).  It can also fail silently, leaving missing or empty
files in the worktree.

Enable only if you understand the tradeoff and your remote /
git version / network are known good for partial clones.

The first such fetch converts the local repository to a
partial clone -- the filter persists in `.git/config\=' and
affects every subsequent fetch on that repo, even after this
defcustom is flipped back to nil.  Use
`shipit-worktree-undo-partial-clone\=' to remove the persisted
state and backfill missing blobs."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-worktree-skip-lfs t
  "When non-nil, set `GIT_LFS_SKIP_SMUDGE=1\=' for PR worktree creation.
Causes `git worktree add\=' (and the fetch that precedes it) to
leave LFS-tracked files as pointer files instead of running the
smudge filter to materialise their actual content.  For review
workflows on monorepos with sizable LFS assets (binaries,
firmware images, fixtures, design files) this is typically the
single biggest speed win for `shipit--create-worktree-pr\=' --
multi-GB LFS smudges that take \u226590s become seconds.

The pointer files render as short text stubs starting with
`version https://git-lfs.github.com/spec/v1\='.  When the
reviewer actually needs a specific binary, run
  M-x shell-command RET cd <worktree> && git lfs pull --include=<glob>
or its CLI equivalent inside the worktree to materialise just
those files on demand.

Set to nil if your review workflow needs LFS content
materialised up front (e.g. running tests that read binary
fixtures)."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-worktree-lfs-concurrent-transfers 16
  "Number of parallel transfers `git lfs pull\=' uses, via `-c lfs.concurrenttransfers=N\='.
git-lfs defaults to 8 concurrent transfers; on fast home / office
connections that often leaves bandwidth on the table.  Bumping to
16 typically doubles throughput on multi-GB pulls without
overwhelming GitHub\='s LFS rate limits; 32 can help further on
very-fast links.  Lower it (e.g. 4) on flaky / metered networks
where parallel retries hurt more than they help.

Set to nil to leave the option unset and use git-lfs\='s built-in
default."
  :type '(choice (const :tag "Use git-lfs default" nil)
                 (integer :tag "Concurrent transfers"))
  :group 'shipit)

(defcustom shipit-worktree-lfs-poll-interval 0.5
  "Seconds between LFS pull progress poll ticks.
Each tick stats every path in the include set to count how
many are now smudged.  At 0.5s the user sees a snappy %
counter; raise to 1.0+ on very large include sets (10k+ files)
where stat-ing per tick adds noticeable load."
  :type 'number
  :group 'shipit)

(defun shipit-worktree--lfs-pull-prefix-args ()
  "Return git `-c\=' args that should prefix every `git lfs pull\=' invocation.
Picks up `shipit-worktree-lfs-concurrent-transfers\=' if set."
  (when (and shipit-worktree-lfs-concurrent-transfers
             (integerp shipit-worktree-lfs-concurrent-transfers)
             (> shipit-worktree-lfs-concurrent-transfers 0))
    (list "-c" (format "lfs.concurrenttransfers=%d"
                       shipit-worktree-lfs-concurrent-transfers))))

(defvar shipit-worktree--progress (make-hash-table :test 'equal)
  "Hash of REPO/PR-NUMBER -> progress plist.
Populated while an async worktree operation is running so the
PR-buffer\='s Worktree section can render a live phase line
(\"\u23f3 Fetching PR ref ...\"\, etc.).  Cleared once the
operation finishes or fails.

Plist keys:
- `:phase\=' -- one of `fetching\=', `creating\=', `pulling-lfs\=',
  `syncing\=', `failed\='.
- `:detail\=' -- short human-readable string for the phase.
- `:started\=' -- `current-time\=' when the phase began.
- `:buffer\=' -- the PR buffer that originated the operation;
  used by the sentinel to refresh the right buffer.")

(defun shipit-worktree--progress-key (repo pr-number)
  "Return the canonical hash key for REPO + PR-NUMBER."
  (format "%s#%s" (or repo "?") pr-number))

(defun shipit-worktree--progress-get (repo pr-number)
  "Return the progress plist for REPO + PR-NUMBER, or nil."
  (gethash (shipit-worktree--progress-key repo pr-number)
           shipit-worktree--progress))

(defun shipit-worktree--progress-set (repo pr-number phase detail buffer)
  "Set progress for REPO + PR-NUMBER to PHASE / DETAIL / BUFFER.
Preserves the existing `:started\\=' timestamp when the phase
hasn\\='t changed (i.e. when the poll tick is just refreshing
the detail string for the same operation), so the elapsed-time
indicator counts up monotonically instead of resetting on
every update."
  (let* ((key (shipit-worktree--progress-key repo pr-number))
         (prev (gethash key shipit-worktree--progress))
         (prev-phase (and prev (plist-get prev :phase)))
         (prev-started (and prev (plist-get prev :started)))
         (started (if (and prev-started (eq phase prev-phase))
                      prev-started
                    (current-time))))
    (puthash key
             (list :phase phase
                   :detail detail
                   :started started
                   :buffer buffer)
             shipit-worktree--progress)))

(defun shipit-worktree--progress-clear (repo pr-number)
  "Remove progress for REPO + PR-NUMBER."
  (remhash (shipit-worktree--progress-key repo pr-number)
           shipit-worktree--progress))

(defun shipit--worktree--commit-present-p (repo-root sha)
  "Return non-nil when commit SHA is already locally available in REPO-ROOT.
Uses `git cat-file -e SHA^{commit}\=', which exits 0 only when the
commit object is present.  Lets `shipit--create-worktree-pr\='
skip the fetch round-trip on warm caches (the common case for
reopening or rerendering a PR buffer)."
  (when (and (stringp repo-root)
             (file-directory-p repo-root)
             (stringp sha)
             (> (length sha) 0))
    (let ((default-directory repo-root))
      (zerop (process-file "git" nil nil nil
                           "cat-file" "-e"
                           (format "%s^{commit}" sha))))))

(defun shipit--worktree--git (repo-root output-buffer &rest args)
  "Run `git ARGS\=' synchronously in REPO-ROOT, return exit code.
OUTPUT-BUFFER receives stdout (nil discards it; t means
current buffer).  Avoids `shell-command\=' (and the shell
itself) -- profiling showed `shell-command\='s
`*Shell Command Output*\=' buffer firing
`pulsar-global-mode-enable-in-buffer\=' / `jit-lock\=' / mode
hooks accounted for the bulk of `shipit--create-worktree-pr\='
runtime, not the actual git invocation."
  (let ((default-directory (file-name-as-directory repo-root)))
    (apply #'process-file "git" nil output-buffer nil args)))

(defun shipit--get-worktree-path (pr-number branch-name)
  "Construct worktree path for PR-NUMBER and BRANCH-NAME.
Returns absolute path to worktree directory."
  (let ((safe-branch (string-replace "/" "-" branch-name)))
    (expand-file-name
     (format "%s/pr-%d-%s" shipit-worktree-directory pr-number safe-branch)
     (or (shipit--get-repo-root) "."))))

(defvar shipit--repo-root-cache (make-hash-table :test 'equal)
  "Hash of `default-directory\=' -> cached git toplevel.
Memoises `shipit--get-repo-root\=' so PR-buffer construction
doesn\='t fork `git rev-parse\=' on every section render +
sub-helper call; the profile showed multiple calls per
construction.  Cleared by `shipit-clear-all-caches\='.")

(defun shipit--get-repo-root ()
  "Get git repository root directory, or nil if not in a git worktree.
Cached per `default-directory\='.  `git rev-parse --show-toplevel\='
exits non-zero when run from inside the `.git/\=' directory (e.g.
editing `COMMIT_EDITMSG\=')."
  (let* ((dir (or default-directory ""))
         (cached (gethash dir shipit--repo-root-cache 'shipit--miss)))
    (cond
     ((not (eq cached 'shipit--miss)) cached)
     (t
      (let ((result
             (with-temp-buffer
               (when (zerop (process-file "git" nil t nil
                                          "rev-parse" "--show-toplevel"))
                 (let ((root (string-trim (buffer-string))))
                   (and (not (string-empty-p root))
                        (file-directory-p root)
                        root))))))
        (puthash dir result shipit--repo-root-cache)
        result)))))

(defun shipit--get-current-repo ()
  "Get current repository from git remote origin.
Returns the full repo name (owner/repo format), or nil if unable to determine."
  (condition-case err
      (let* ((remote-url (shell-command-to-string "git config --get remote.origin.url"))
             (clean-url (string-trim remote-url)))
        (cond
         ((string-match "github\\.com[:/]\\([^/]+\\)/\\([^/\n]+\\)" clean-url)
          (let ((owner (match-string 1 clean-url))
                (repo (match-string 2 clean-url)))
            (when (string-suffix-p ".git" repo)
              (setq repo (substring repo 0 -4)))
            (format "%s/%s" owner repo)))
         (t nil)))
    (error
     (shipit--debug-log "Failed to get current repo: %s" err)
     nil)))

(defun shipit--validate-pr-repo (pr-repo)
  "Validate that PR-REPO matches the current repository.
Returns t if they match, raises user-error with clear message if not.

When the call site is inside a shipit PR buffer, the buffer-local
`shipit-buffer-repo' is the authoritative answer -- it was
captured at buffer-creation time and never drifts.  Trusting it
first avoids spurious \"different repository\" errors when the
buffer\\='s `default-directory' has been changed by something
unrelated (e.g.  `shipit-reload\\=' from inside the shipit project
itself).  Only when there\\='s no buffer-local context do we fall
back to querying `git remote' via `shipit--get-current-repo'.

Comparison is case-insensitive since GitHub repository names are."
  (let* ((buf-repo (and (boundp 'shipit-buffer-repo) shipit-buffer-repo))
         (current-repo (or buf-repo (shipit--get-current-repo))))
    (cond
     ((not current-repo)
      (user-error "Cannot determine current repository from git remote"))
     ((not (string-equal (downcase current-repo) (downcase pr-repo)))
      (user-error "PR is from repository '%s' but current repository is '%s'" pr-repo current-repo))
     (t t))))

(defun shipit--worktree-exists-p (worktree-path)
  "Check if worktree exists at WORKTREE-PATH."
  (file-directory-p worktree-path))

(defun shipit--get-pr-info-from-worktree (worktree-path)
  "Read .shipit-pr-info.json from WORKTREE-PATH.
Returns plist with pr-number, repo, branch, pr-head-sha, created-at, or nil."
  (let ((info-file (expand-file-name ".shipit-pr-info.json" worktree-path)))
    (when (file-exists-p info-file)
      (condition-case err
          (let* ((json-str (with-temp-buffer
                            (insert-file-contents info-file)
                            (buffer-string)))
                 (json-data (json-read-from-string json-str)))
            (list :pr-number (cdr (assq 'pr_number json-data))
                  :repo (cdr (assq 'repo json-data))
                  :branch (cdr (assq 'branch json-data))
                  :pr-head-sha (cdr (assq 'pr_head_sha json-data))
                  :created-at (cdr (assq 'created_at json-data))))
        (error
         (shipit--debug-log "Failed to read .shipit-pr-info.json: %s" err)
         nil)))))

(defun shipit--worktree-in-sync-p (worktree-path current-pr-head-sha)
  "Check if worktree at WORKTREE-PATH is in sync with CURRENT-PR-HEAD-SHA.
Returns t if in sync, nil otherwise.

Tolerant compare:
  * trims whitespace on both sides,
  * case-insensitive (git SHAs are lowercase but be defensive),
  * accepts short-vs-full SHA when one is a >=7-char prefix of
    the other (so a 7-char abbrev from one source matches a full
    40-char SHA from the other).
Logs both inputs to `shipit--debug-log' on every check so a
falsely-out-of-sync result can be diagnosed."
  ;; Don't gate on `.shipit-pr-info.json' existing -- the file is
  ;; advisory metadata, not load-bearing for the sync check.  The
  ;; comparison uses the worktree's actual `git rev-parse HEAD' vs
  ;; the PR's current head SHA from GitHub, which works fine for a
  ;; worktree created outside shipit (or one whose metadata file got
  ;; deleted).  Previously the early bail when the file was missing
  ;; made the caller report "Out of Sync" even when git was in sync.
  (when (file-directory-p worktree-path)
    (condition-case err
        (let* ((actual-raw (shell-command-to-string
                            (format "cd %s && git rev-parse HEAD"
                                    (shell-quote-argument worktree-path))))
               (actual (and (stringp actual-raw) (string-trim actual-raw)))
               (expected (and (stringp current-pr-head-sha)
                              (string-trim current-pr-head-sha)))
               (in-sync-p
                (and actual expected
                     (not (string-empty-p actual))
                     (not (string-empty-p expected))
                     (let ((a (downcase actual))
                           (b (downcase expected)))
                       (or (string= a b)
                           (and (>= (length a) 7) (>= (length b) 7)
                                (or (string-prefix-p a b)
                                    (string-prefix-p b a))))))))
          (shipit--debug-log
           "Sync check: actual=%S (len=%d) expected=%S (len=%d) match=%s"
           actual (length (or actual ""))
           expected (length (or expected ""))
           in-sync-p)
          in-sync-p)
      (error
       (shipit--debug-log "Sync check error at %s: %s"
                          worktree-path (error-message-string err))
       nil))))

(defun shipit--main-worktree-root ()
  "Return the main worktree\='s root directory, even from a linked worktree.
`shipit--get-repo-root\=' returns whichever working tree the
caller is currently in -- inside a linked worktree, that\='s
the worktree itself, not the main repo.  But the
`.worktrees/\=' subtree (where shipit creates PR worktrees)
lives in the MAIN worktree, so callers that need to enumerate
existing PR worktrees must resolve the main root.

Uses `git rev-parse --git-common-dir\=' (returns the shared
`.git\='); the main worktree is one directory above that.
Returns nil when not in a git working tree."
  (with-temp-buffer
    (when (zerop (process-file "git" nil t nil
                               "rev-parse" "--git-common-dir"))
      (let* ((common (string-trim (buffer-string)))
             (gitdir (directory-file-name (expand-file-name common))))
        (and gitdir
             (string-suffix-p ".git" gitdir)
             (file-name-as-directory (file-name-directory gitdir)))))))

(defun shipit--find-worktree-for-pr (pr-number _pr-repo)
  "Find worktree directory for PR-NUMBER.
Returns path if found, nil otherwise.  Tolerant of PR-NUMBER as
an integer or a numeric string."
  (let* ((pr-num (cond ((integerp pr-number) pr-number)
                       ((and (stringp pr-number)
                             (string-match-p "^[0-9]+$" pr-number))
                        (string-to-number pr-number))
                       (t nil)))
         ;; Always use the MAIN worktree's root -- the `.worktrees/'
         ;; subtree lives there, not in linked worktrees.  Falls back
         ;; to `shipit--get-repo-root' when not in a git working tree
         ;; at all (so the helper still works in tests / non-git
         ;; contexts).
         (repo-root (or (shipit--main-worktree-root)
                        (shipit--get-repo-root)))
         (worktrees-dir (and repo-root
                             (expand-file-name shipit-worktree-directory
                                               repo-root))))
    (shipit--debug-log
     "find-worktree-for-pr: pr=%S repo-root=%s worktrees-dir=%s exists=%s"
     pr-number (or repo-root "nil") (or worktrees-dir "nil")
     (and worktrees-dir (file-directory-p worktrees-dir)))
    (when (and pr-num worktrees-dir (file-directory-p worktrees-dir))
      (let ((pattern (format "pr-%d-" pr-num)))
        (condition-case err
            (let ((hit (car (seq-filter
                             (lambda (dir)
                               (string-prefix-p pattern
                                                (file-name-nondirectory dir)))
                             (directory-files worktrees-dir t "^pr-")))))
              (shipit--debug-log
               "find-worktree-for-pr: pattern=%s -> %s"
               pattern (or hit "no match"))
              hit)
          (error
           (shipit--debug-log
            "find-worktree-for-pr: error scanning %s: %S"
            worktrees-dir err)
           nil))))))

(defun shipit--get-worktree-status (pr-number pr-head-sha pr-repo)
  "Get worktree status for PR-NUMBER.
PR-REPO should be the full repo name from PR data.
Returns one of: \\='none, \\='in-sync, \\='out-of-sync"
  (shipit--validate-pr-repo pr-repo)
  (let ((worktree-path (shipit--find-worktree-for-pr pr-number pr-repo)))
    (if (not worktree-path)
        'none
      (if (shipit--worktree-in-sync-p worktree-path pr-head-sha)
          'in-sync
        'out-of-sync))))


(defun shipit-worktree--list-remotes (repo-root)
  "Return the list of git remote names defined in REPO-ROOT.
One name per element (\"origin\" / \"upstream\" / ...).  nil
when the repository has no remotes or `git\=' fails."
  (with-temp-buffer
    (when (zerop (shipit--worktree--git repo-root t "remote"))
      (split-string (buffer-string) "\n" t "[ \t]+"))))

(defun shipit-worktree--remote-partialclone-active-p (repo-root remote)
  "Return non-nil when REMOTE in REPO-ROOT has partial-clone state set."
  (or (zerop (shipit--worktree--git
              repo-root nil
              "config" "--get"
              (format "remote.%s.partialclonefilter" remote)))
      (zerop (shipit--worktree--git
              repo-root nil
              "config" "--get"
              (format "remote.%s.promisor" remote)))))

;;;###autoload
(defun shipit-worktree-undo-partial-clone (&optional repo-root)
  "Remove partial-clone state in REPO-ROOT and backfill missing blobs.
Walks every remote configured in the repository and unsets its
`partialclonefilter\=' / `promisor\=' options, then runs
`git fetch --refetch <remote>\=' (git \u2265 2.36) for each
remote that had partial-clone state -- which redownloads every
object the previous filter had skipped, leaving the repo in a
fully-populated state again.

Use this after a session where
`shipit-worktree-fetch-blobless\=' was on and turned the repo
into a partial clone.  Symptoms that warrant running it:
worktree creation taking dramatically longer than usual
(every blob fetched on demand), or files showing up missing /
empty in created worktrees.

Interactive: when called with no argument the repo root is
discovered via `shipit--get-repo-root\='."
  (interactive)
  (let* ((root (or repo-root (shipit--get-repo-root))))
    (unless root
      (user-error "Not in a git repository"))
    (let ((remotes (shipit-worktree--list-remotes root))
          (cleared 0)
          (refetched 0)
          (refetch-failures 0))
      (unless remotes
        (user-error "No git remotes in %s" root))
      (dolist (remote remotes)
        (let ((had-state
               (shipit-worktree--remote-partialclone-active-p root remote)))
          (shipit--worktree--git
           root nil "config" "--unset"
           (format "remote.%s.partialclonefilter" remote))
          (shipit--worktree--git
           root nil "config" "--unset"
           (format "remote.%s.promisor" remote))
          (when had-state
            (cl-incf cleared)
            (message "shipit-worktree: refetching from %s (this may take a while)..."
                     remote)
            (let ((exit (shipit--worktree--git
                         root nil "fetch" "--refetch" remote)))
              (cond ((zerop exit) (cl-incf refetched))
                    (t (cl-incf refetch-failures)
                       (shipit--debug-log
                        "git fetch --refetch %s failed (exit=%d); your git may be < 2.36 -- try `git fetch %s\=' manually"
                        remote exit remote)))))))
      (message
       "shipit-worktree: cleared partial-clone state from %d remote%s; refetched %d; %s"
       cleared (if (= cleared 1) "" "s")
       refetched
       (if (zerop refetch-failures)
           "no failures"
         (format "%d refetch failure%s (see *Messages* / debug log)"
                 refetch-failures
                 (if (= refetch-failures 1) "" "s")))))))

(declare-function shipit--insert-worktree-section "shipit-buffer")

(declare-function shipit--get-worktree-status "shipit-worktree")
(declare-function shipit--find-worktree-for-pr "shipit-worktree")
(declare-function shipit--get-worktree-status-icon "shipit-render")
(declare-function shipit--insert-worktree-body "shipit-buffer")

(defvar shipit-worktree--progress-timers (make-hash-table :test 'equal)
  "Hash of REPO/PR-NUMBER -> running poll timer.
A 1Hz `run-at-time\=' timer that polls the local LFS object
store and updates the progress phase\='s detail string with
real percentage progress.")

(defvar shipit-worktree--progress-poll-state (make-hash-table :test 'equal)
  "Hash of REPO/PR-NUMBER -> alist of poll state.
Keys: `:paths\=' (list of repo-relative LFS-tracked paths
matching the pull\='s include patterns -- the set we measure
progress against), `:target\=' (length of `:paths\='),
`:baseline\=' (count of paths whose working-tree file was
already smudged when the pull started), `:worktree-path\='.")

(defun shipit-worktree--lfs-pointer-file-p (abs-path)
  "Non-nil if ABS-PATH looks like an LFS pointer file (not smudged).
Pointer files are <200 bytes and start with the LFS spec
header.  We use size as a cheap first cut so we don\='t open
3000+ files per tick: a smudged binary is essentially always
larger than the pointer."
  (when (file-exists-p abs-path)
    (let ((size (or (file-attribute-size (file-attributes abs-path)) 0)))
      ;; Pointer files are typically ~130 bytes (`version
      ;; https://git-lfs.github.com/spec/v1\\noid sha256:...\\nsize NN\\n\=').
      ;; Real binaries that are smaller than 256 bytes are
      ;; vanishingly rare among LFS-tracked content; use 256 as
      ;; the threshold.
      (< size 256))))

(defun shipit-worktree--count-smudged-paths (worktree-path paths)
  "Count items in PATHS (relative to WORKTREE-PATH) that are smudged.
A path is \"smudged\" when its working-tree file is the actual
binary content rather than the LFS pointer text -- detected
cheaply by file size."
  (let ((count 0))
    (dolist (rel paths)
      (let ((abs (expand-file-name rel worktree-path)))
        (unless (shipit-worktree--lfs-pointer-file-p abs)
          (when (file-exists-p abs)
            (cl-incf count)))))
    count))

(defun shipit-worktree--lfs-paths-matching (worktree-path patterns)
  "Return list of LFS-tracked paths in WORKTREE-PATH matching PATTERNS.
PATTERNS may be a list of gitattributes-style globs OR nil for
\"all LFS files\".  Runs `git lfs ls-files\=' (with `-I CSV\='
when patterns are given) and extracts the path column."
  (with-temp-buffer
    (let ((args (if patterns
                    (list "lfs" "ls-files" "-I"
                          (mapconcat #'identity patterns ","))
                  (list "lfs" "ls-files"))))
      (when (zerop (apply #'shipit--worktree--git
                          worktree-path t args))
        (let (paths)
          (goto-char (point-min))
          (while (re-search-forward
                  "^[0-9a-f]+[ \t]+[*-][ \t]+\\(.+\\)$" nil t)
            (push (match-string 1) paths))
          (nreverse paths))))))

(defun shipit-worktree--progress-poll-base-detail (detail)
  "Strip a trailing ` -- N% (X/Y) [Ns]\=' marker from DETAIL."
  (if (and (stringp detail)
           (string-match
            " -- [0-9]+%[^]]*\\[[0-9]+s\\]\\\'"
            detail))
      (substring detail 0 (match-beginning 0))
    (or detail "")))

(defun shipit-worktree--progress-poll-tick (repo pr-number pr-buffer)
  "One tick of the LFS pull progress poller.
Reads the cached PATHS / BASELINE / TARGET / WORKTREE-PATH for
REPO/PR-NUMBER, counts how many paths are now smudged in the
working tree, computes a percentage of pending work completed,
and updates the overlay.  Stops itself when progress is
cleared."
  (let ((progress (shipit-worktree--progress-get repo pr-number))
        (state (gethash (shipit-worktree--progress-key repo pr-number)
                        shipit-worktree--progress-poll-state)))
    (cond
     ((or (not progress) (not state))
      (shipit-worktree--progress-poll-stop repo pr-number))
     (t
      (let* ((paths (alist-get :paths state))
             (target (alist-get :target state))
             (baseline (alist-get :baseline state))
             (worktree-path (alist-get :worktree-path state))
             (current (shipit-worktree--count-smudged-paths
                       worktree-path paths))
             (done (max 0 (- current baseline)))
             (pending (max 1 (- target baseline)))
             (pct (min 100 (truncate (* 100.0 (/ (float done) pending)))))
             (started (or (plist-get progress :started) (current-time)))
             (elapsed (max 0 (truncate
                              (float-time
                               (time-subtract (current-time) started)))))
             (base (shipit-worktree--progress-poll-base-detail
                    (plist-get progress :detail)))
             (new-detail (format "%s -- %d%% (%d/%d) [%ds]"
                                 base pct done (- target baseline) elapsed)))
        (shipit-worktree--progress-set
         repo pr-number
         (plist-get progress :phase)
         new-detail
         pr-buffer)
        (when (and (fboundp 'shipit-worktree--progress-render)
                   (buffer-live-p pr-buffer))
          (shipit-worktree--progress-render pr-buffer repo pr-number)))))))

(defun shipit-worktree--progress-poll-start
    (repo pr-number worktree-path patterns pr-buffer)
  "Start the progress poller for an LFS pull on WORKTREE-PATH.
Captures the include-set PATHS once (via `git lfs ls-files
-I PATTERNS\=') plus a BASELINE smudge count, so each tick can
just stat-and-count the cached path list -- cheap, ~ms even
for a few thousand files.  Fires one immediate tick so the user
sees `0%% (0/N) [0s]\=' right away, then continues at the
interval set by `shipit-worktree-lfs-poll-interval\='.  Errors
degrade gracefully: the pull still runs, just without live %."
  (condition-case err
      (let* ((paths (shipit-worktree--lfs-paths-matching
                     worktree-path patterns))
             (target (length paths))
             (baseline (shipit-worktree--count-smudged-paths
                        worktree-path paths))
             (key (shipit-worktree--progress-key repo pr-number))
             (interval (max 0.1 shipit-worktree-lfs-poll-interval)))
        (puthash key
                 (list (cons :paths paths)
                       (cons :target target)
                       (cons :baseline baseline)
                       (cons :worktree-path worktree-path))
                 shipit-worktree--progress-poll-state)
        (shipit--debug-log
         "lfs-poll start: target=%d baseline=%d interval=%.2fs patterns=%S"
         target baseline interval patterns)
        ;; Render the first tick synchronously so the overlay is
        ;; populated immediately rather than blank for INTERVAL ms.
        (shipit-worktree--progress-poll-tick repo pr-number pr-buffer)
        (unless (gethash key shipit-worktree--progress-timers)
          (puthash key
                   (run-at-time
                    interval interval
                    #'shipit-worktree--progress-poll-tick
                    repo pr-number pr-buffer)
                   shipit-worktree--progress-timers)))
    (error
     (shipit--debug-log "lfs-poll-start failed: %S" err))))

(defun shipit-worktree--progress-poll-stop (repo pr-number)
  "Cancel the LFS pull progress poller for REPO/PR-NUMBER, if running."
  (let* ((key (shipit-worktree--progress-key repo pr-number))
         (timer (gethash key shipit-worktree--progress-timers)))
    (when timer
      (cancel-timer timer)
      (remhash key shipit-worktree--progress-timers))
    (remhash key shipit-worktree--progress-poll-state)))

(defvar-local shipit-worktree--progress-overlay nil
  "Overlay rendering the live progress phase line on the Worktree section.
A floating before-string overlay anchored at the worktree
section\='s start position; pure presentation -- it does NOT
modify buffer text or section structure, so navigation
(`n\=' / `p\=') and DWIM dispatch (`M-;\=') keep working
through every phase update.  Created by
`shipit-worktree--progress-render\=', cleared by
`shipit-worktree--progress-clear-overlay\='.")

(declare-function shipit--find-section-by-type "shipit-sections")
(declare-function shipit-buffer-refresh "shipit-buffer")

(defun shipit-worktree--progress-line-string (progress)
  "Format the propertised phase line for PROGRESS plist."
  (let* ((phase (plist-get progress :phase))
         (detail (plist-get progress :detail))
         (icon (pcase phase
                 ('failed "\u26a0")
                 (_ "\u23f3")))
         (label (pcase phase
                  ('fetching "Fetching PR ref")
                  ('creating "Creating worktree")
                  ('pulling-lfs "Pulling LFS")
                  ('syncing "Syncing")
                  ('failed "Failed")
                  (_ (format "%s" phase)))))
    (format "   Phase: %s %s%s\n"
            icon
            (propertize label 'font-lock-face
                        (if (eq phase 'failed)
                            'font-lock-warning-face
                          '(:weight bold)))
            (if (and detail (> (length detail) 0))
                (concat " -- "
                        (propertize detail 'font-lock-face
                                    'font-lock-comment-face))
              ""))))

(defun shipit-worktree--progress-render (buffer repo pr-number)
  "Render or update the progress overlay for REPO/PR-NUMBER in BUFFER.
Reads the current progress plist for the key and either creates
the overlay (on the line right after the worktree section\='s
heading), or updates the existing overlay\='s `before-string\='.
Pure overlay manipulation -- no buffer text, no section
structure changes, so this is safe to call from a process
filter without re-entrancy or navigation breakage."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((progress (shipit-worktree--progress-get repo pr-number))
             (line (and progress
                        (shipit-worktree--progress-line-string progress)))
             (section (and (fboundp 'shipit--find-section-by-type)
                           (shipit--find-section-by-type
                            'shipit-worktree-section))))
        (cond
         ;; Progress active and section visible: ensure overlay shows it.
         ((and progress section line)
          ;; Anchor at the position right after the heading\='s newline so
          ;; the before-string lands on the line below the heading.
          (let* ((anchor (save-excursion
                           (goto-char (oref section start))
                           (forward-line 1)
                           (point))))
            (cond
             ((overlayp shipit-worktree--progress-overlay)
              (move-overlay shipit-worktree--progress-overlay anchor anchor)
              (overlay-put shipit-worktree--progress-overlay
                           'before-string line))
             (t
              (setq shipit-worktree--progress-overlay
                    (make-overlay anchor anchor nil t nil))
              (overlay-put shipit-worktree--progress-overlay
                           'before-string line)
              (overlay-put shipit-worktree--progress-overlay
                           'shipit-worktree-progress t)))))
         ;; Progress cleared: drop the overlay.
         (t
          (shipit-worktree--progress-clear-overlay buffer)))))))

(defun shipit-worktree--progress-clear-overlay (buffer)
  "Delete the progress overlay in BUFFER if present."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (overlayp shipit-worktree--progress-overlay)
        (delete-overlay shipit-worktree--progress-overlay)
        (setq shipit-worktree--progress-overlay nil)))))

(defun shipit-worktree--refresh-section-in-buffer (buffer)
  "Update the Worktree section\='s progress overlay in BUFFER.
While a progress phase is active, this is just an overlay
update -- never touches buffer text, never modifies section
structure, so navigation and DWIM dispatch keep working.
When progress is cleared and the worktree state has actually
changed (fetch / worktree-add / lfs pull just finished),
schedules a normal `shipit-buffer-refresh\=' through
`run-with-timer\=' to repaint the section\='s body cleanly via
the standard render path -- no in-place section surgery."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((section (and (fboundp 'shipit--find-section-by-type)
                           (shipit--find-section-by-type
                            'shipit-worktree-section)))
             (pr-data (and section
                           (get-text-property
                            (oref section start)
                            'shipit-pr-data)))
             (pr-number (and pr-data (cdr (assq 'number pr-data))))
             (pr-repo (and pr-data
                           (cdr (assq 'full_name
                                      (cdr (assq 'repo
                                                 (cdr (assq 'base pr-data))))))))
             (progress (and pr-repo pr-number
                            (shipit-worktree--progress-get pr-repo pr-number))))
        (cond
         (progress
          (shipit-worktree--progress-render buffer pr-repo pr-number))
         (t
          (shipit-worktree--progress-clear-overlay buffer)
          ;; Defer to the next idle tick so we\='re out of the
          ;; sentinel\='s call stack, then update only the worktree
          ;; section\='s body in place via local filesystem queries
          ;; (status / path / .shipit-pr-info.json / lfs ls-files).
          ;; No API refetch -- the section\='s data is all local --
          ;; so no UI block.
          (run-with-timer
           0 nil
           (lambda ()
             (condition-case err
                 (shipit-worktree--refresh-body-locally buffer)
               (error
                (shipit--debug-log
                 "worktree body refresh failed: %S" err)))))))))))

(defun shipit-worktree--refresh-body-locally (buffer)
  "Re-render the Worktree section\='s body in BUFFER from local data.
Does NOT recreate the section -- the heading + section identity
stay intact -- only the body content between `content\=' and
`end\=' markers is replaced via `shipit--insert-worktree-body\='.
After re-insertion, the `magit-section\=' text property is
re-asserted across the new body so the section navigator
finds it.  All inputs come from local sources (`git rev-parse\=',
the worktree dir, `.shipit-pr-info.json\=', `git lfs ls-files\=')
so this never blocks on the network."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((section (and (fboundp 'shipit--find-section-by-type)
                           (shipit--find-section-by-type
                            'shipit-worktree-section)))
             (pr-data (and section
                           (get-text-property
                            (oref section start)
                            'shipit-pr-data))))
        (when (and section pr-data)
          (let* ((inhibit-read-only t)
                 (pr-number (cdr (assq 'number pr-data)))
                 (pr-head-sha (cdr (assq 'sha (cdr (assq 'head pr-data)))))
                 (pr-repo (cdr (assq 'full_name
                                     (cdr (assq 'repo
                                                (cdr (assq 'base pr-data)))))))
                 (status (condition-case _err
                             (shipit--get-worktree-status
                              pr-number pr-head-sha pr-repo)
                           (error 'different-repo)))
                 (worktree-path
                  (when (not (eq status 'different-repo))
                    (shipit--find-worktree-for-pr
                     pr-number (shipit--get-repo-from-remote))))
                 (status-text
                  (pcase status
                    ('none "No worktree checked out")
                    ('in-sync (concat (propertize "In Sync"
                                                  'font-lock-face
                                                  '(:foreground "green"
                                                                :weight bold))
                                      " (PR #" (number-to-string pr-number)
                                      " @ "
                                      (propertize (substring pr-head-sha 0 7)
                                                  'font-lock-face 'magit-hash)
                                      ")"))
                    ('out-of-sync (concat (propertize "Out of Sync"
                                                      'font-lock-face
                                                      'font-lock-warning-face)
                                          " (upstream: "
                                          (propertize (substring pr-head-sha 0 7)
                                                      'font-lock-face 'magit-hash)
                                          ")"))
                    ('different-repo "PR is from a different repository")))
                 (progress nil)
                 (content-pos (and (oref section content)
                                   (marker-position (oref section content))))
                 (end-pos (and (oref section end)
                               (marker-position (oref section end)))))
            (when (and content-pos end-pos)
              (save-excursion
                (when (> end-pos content-pos)
                  (delete-region content-pos end-pos))
                (oset section children nil)
                (when (slot-boundp section 'washer)
                  (oset section washer nil))
                (goto-char content-pos)
                (shipit--insert-worktree-body
                 pr-data status status-text worktree-path
                 progress pr-number pr-repo)
                (oset section end (point-marker))
                (oset section content (copy-marker content-pos))
                ;; Reapply the section's text property so DWIM /
                ;; navigator can find it again.
                (let ((s-start (marker-position (oref section start)))
                      (s-end (marker-position (oref section end))))
                  (when (and s-start s-end (< s-start s-end))
                    (put-text-property s-start s-end
                                       'magit-section section)))))))))))

(defun shipit-worktree--make-progress-sentinel (repo pr-number on-success on-failure)
  "Return a process sentinel that updates progress for REPO/PR-NUMBER.
On success (exit 0) calls ON-SUCCESS with no args; on failure
calls ON-FAILURE with the exit code and the captured stderr
buffer string.  Either way it refreshes the originating PR
buffer\='s Worktree section."
  (lambda (process event)
    (when (memq (process-status process) '(exit signal))
      (let* ((exit (process-exit-status process))
             (stderr-buf (process-get process 'shipit-stderr))
             (stderr-text (when (buffer-live-p stderr-buf)
                            (with-current-buffer stderr-buf
                              (buffer-string))))
             (progress (shipit-worktree--progress-get repo pr-number))
             (origin (and progress (plist-get progress :buffer))))
        (shipit--debug-log
         "worktree async %s: exit=%d event=%s stderr=%s"
         (process-name process) exit (string-trim event)
         (or stderr-text ""))
        (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
        (cond
         ((zerop exit)
          (when on-success (funcall on-success)))
         (t
          (shipit-worktree--progress-set
           repo pr-number 'failed
           (format "exit=%d %s" exit
                   (if stderr-text (string-trim stderr-text) ""))
           origin)
          (shipit-worktree--refresh-section-in-buffer origin)
          (when on-failure (funcall on-failure exit stderr-text))))))))

(defun shipit-worktree--start-git-async
    (repo pr-number repo-root args env on-success on-failure
          &optional filter-fn)
  "Launch `git ARGS\=' asynchronously in REPO-ROOT, return the process.
ENV is an extra alist of (\"NAME=VALUE\") strings prepended to
`process-environment\=' for the child.  ON-SUCCESS / ON-FAILURE
are called from the sentinel (see
`shipit-worktree--make-progress-sentinel\=' for arg shapes).

When FILTER-FN is non-nil, stdout and stderr are merged into
the process buffer so the filter can parse progress output that
git emits to stderr (e.g. git-lfs).  FILTER-FN is called with
(PROCESS CHUNK) for each output chunk; the wrapping `:filter\='
also performs the default `insert\='-into-process-buffer
behaviour."
  (let* ((default-directory (file-name-as-directory repo-root))
         (process-environment (append env process-environment))
         (merged (and filter-fn t))
         (stderr-buf (unless merged
                       (generate-new-buffer
                        (format " *shipit-worktree-stderr %s*"
                                (shipit-worktree--progress-key repo pr-number)))))
         (out-buf (generate-new-buffer
                   (format " *shipit-worktree-stdout %s*"
                           (shipit-worktree--progress-key repo pr-number))))
         (filter (when filter-fn
                   (lambda (proc chunk)
                     (when (buffer-live-p (process-buffer proc))
                       (with-current-buffer (process-buffer proc)
                         (let ((moving (= (point) (process-mark proc))))
                           (save-excursion
                             (goto-char (process-mark proc))
                             (insert chunk)
                             (set-marker (process-mark proc) (point)))
                           (when moving
                             (goto-char (process-mark proc))))))
                     (condition-case err
                         (funcall filter-fn proc chunk)
                       (error
                        (shipit--debug-log
                         "lfs progress filter error: %S" err))))))
         (proc (apply #'make-process
                      (append
                       (list :name (format "shipit-worktree-%s-%s"
                                           (or pr-number "?")
                                           (car args))
                             :command (cons "git" args)
                             :buffer out-buf
                             :noquery t
                             :sentinel (shipit-worktree--make-progress-sentinel
                                        repo pr-number on-success on-failure))
                       (unless merged (list :stderr stderr-buf))
                       (when filter (list :filter filter))))))
    (process-put proc 'shipit-stderr (or stderr-buf out-buf))
    proc))

(defun shipit--create-worktree-pr (pr-number branch-name pr-head-sha)
  "Create a worktree for PR-NUMBER with BRANCH-NAME at PR-HEAD-SHA.
Returns (path . success) on success, or nil on error."
  (let* ((repo-root (shipit--get-repo-root))
         (worktree-path (shipit--get-worktree-path pr-number branch-name)))
    (unless repo-root
      (user-error "Not in a git repository"))

    ;; Verify .gitignore includes worktree directory
    (shipit--ensure-worktree-in-gitignore repo-root)

    ;; Create worktree
    (condition-case err
        (let ((t-total (current-time))
              (process-environment
               (if shipit-worktree-skip-lfs
                   (cons "GIT_LFS_SKIP_SMUDGE=1" process-environment)
                 process-environment)))
          (when shipit-worktree-skip-lfs
            (shipit--debug-log
             "LFS smudge skipped (GIT_LFS_SKIP_SMUDGE=1); LFS files left as pointers"))
          (shipit--debug-log "Creating worktree: %s" worktree-path)
          (make-directory (file-name-directory worktree-path) t)

          ;; Handle existing worktree directory carefully
          (when (file-directory-p worktree-path)
            (cond
             ;; If it's a valid git worktree, use it as-is
             ((file-exists-p (expand-file-name ".git" worktree-path))
              (shipit--debug-log "Worktree already exists with valid git state at: %s" worktree-path))
             ;; If it's an empty directory, remove it so git can create fresh
             ((seq-empty-p (directory-files worktree-path nil "^[^.]"))
              (delete-directory worktree-path t)
              (shipit--debug-log "Removed empty leftover directory at: %s" worktree-path))
             ;; Otherwise, error - don't delete directories with files
             (t
              (error "Worktree directory exists at %s with files but is not a git worktree. Please remove it manually." worktree-path))))

          ;; Fetch PR ref to ensure commit is available locally.
          ;; Two optimisations vs the obvious `git fetch\=':
          ;; 1. Skip the fetch entirely when the commit is already
          ;;    present locally (re-opening / re-rendering the PR
          ;;    buffer, common case after the first creation).
          ;; 2. Pass `--filter=blob:none\=' when
          ;;    `shipit-worktree-fetch-blobless\=' is non-nil so we
          ;;    pull commits + trees without blobs; blobs stream in
          ;;    lazily during worktree checkout / file open.
          (let* ((repo-slug (shipit--get-repo-from-remote))
                 (resolved (shipit-pr--resolve-for-repo (or repo-slug "")))
                 (backend (car resolved))
                 (config (cdr resolved))
                 (refspec-fn (plist-get backend :refspec-for-pr))
                 (remote-fn (plist-get backend :remote-for-fetch))
                 (refspec (if refspec-fn
                              (funcall refspec-fn config pr-number branch-name)
                            (error "Backend %s missing :refspec-for-pr" (shipit-pr--backend-id))))
                 (remote (if remote-fn
                             (funcall remote-fn config repo-slug)
                           "origin")))
            (cond
             ((shipit--worktree--commit-present-p repo-root pr-head-sha)
              (shipit--debug-log
               "PR commit %s already local; skipping fetch" pr-head-sha))
             (t
              (shipit--debug-log "Fetching PR ref: git fetch%s %s %s"
                                 (if shipit-worktree-fetch-blobless
                                     " --filter=blob:none"
                                   "")
                                 remote refspec)
              (let* ((t0 (current-time))
                     (args (append (list "fetch")
                                   (and shipit-worktree-fetch-blobless
                                        (list "--filter=blob:none"))
                                   (list remote refspec)))
                     (exit (apply #'shipit--worktree--git
                                  repo-root nil args)))
                (shipit--debug-log
                 "PHASE: fetch done in %.2fs (exit=%d)"
                 (float-time (time-subtract (current-time) t0)) exit)
                (unless (zerop exit)
                  (error "Git fetch failed with exit code %d" exit))))))

          ;; Create worktree at the exact PR commit (detached HEAD).
          ;; Detached avoids branch-name conflicts when an existing
          ;; local branch with the PR\='s name already exists.
          (let ((output-buffer (get-buffer-create "*shipit-git-output*"))
                (t-add (current-time)))
            (with-current-buffer output-buffer
              (erase-buffer))
            (shipit--debug-log "git worktree add %s %s" worktree-path pr-head-sha)
            (let ((exit (shipit--worktree--git
                         repo-root output-buffer
                         "worktree" "add" worktree-path pr-head-sha)))
              (shipit--debug-log
               "PHASE: worktree-add done in %.2fs (exit=%d)"
               (float-time (time-subtract (current-time) t-add)) exit)
              (unless (zerop exit)
                (let ((err (with-current-buffer output-buffer
                             (buffer-string))))
                  (shipit--debug-log "Git error output: %s" err)
                  (error "Git worktree creation failed with exit code %d: %s"
                         exit err))))
            ;; Post-checkout sanity report: file count + size + LFS
            ;; pointer count.  Cheap, runs once, gives the next
            ;; profile a starting point if the slowness is
            ;; downstream of `git worktree add\='.
            (let ((t-report (current-time)))
              (with-temp-buffer
                (when (zerop (shipit--worktree--git
                              worktree-path t "ls-files"))
                  (let* ((all (split-string (buffer-string) "\n" t))
                         (n (length all)))
                    (shipit--debug-log
                     "POST-CHECKOUT: %d tracked files in worktree" n))))
              (with-temp-buffer
                (when (and (executable-find "git")
                           (zerop (shipit--worktree--git
                                   worktree-path t "lfs" "ls-files")))
                  (let ((n (length
                            (split-string (buffer-string) "\n" t))))
                    (when (> n 0)
                      (shipit--debug-log
                       "POST-CHECKOUT: %d LFS-tracked files in worktree" n)))))
              (shipit--debug-log
               "PHASE: post-checkout report in %.3fs"
               (float-time (time-subtract (current-time) t-report)))))

          ;; Write PR metadata
          (shipit--write-pr-info-to-worktree worktree-path
                                             (list :pr-number pr-number
                                                   :branch branch-name
                                                   :pr-head-sha pr-head-sha
                                                   :created-at (format-time-string "%FT%T%z")))

          (shipit--debug-log
           "PHASE: TOTAL create-worktree-pr in %.2fs"
           (float-time (time-subtract (current-time) t-total)))
          (cons worktree-path t))
      (error
       (shipit--debug-log "Failed to create worktree: %s" err)
       nil))))

(defun shipit--create-worktree-pr-async (pr-number branch-name pr-head-sha pr-buffer
                                                    &optional on-done)
  "Async variant of `shipit--create-worktree-pr\='.
Kicks off the fetch (if needed) and the `git worktree add\=' as
chained `make-process\=' calls, updating
`shipit-worktree--progress\=' and refreshing the Worktree
section in PR-BUFFER between phases.  Returns immediately;
callers should check / observe progress via the section.

ON-DONE, when non-nil, is a function called (inside PR-BUFFER
if still live) once the worktree is ready and metadata is
written -- used to resume work that needed the worktree once a
freshly-created worktree is available."
  (let* ((repo-root (or (shipit--get-repo-root)
                        (user-error "Not in a git repository")))
         (worktree-path (shipit--get-worktree-path pr-number branch-name))
         (repo (shipit--get-repo-from-remote))
         (env (and shipit-worktree-skip-lfs (list "GIT_LFS_SKIP_SMUDGE=1")))
         (resolved (shipit-pr--resolve-for-repo (or repo "")))
         (backend (car resolved))
         (config (cdr resolved))
         (refspec-fn (plist-get backend :refspec-for-pr))
         (remote-fn (plist-get backend :remote-for-fetch))
         (refspec (if refspec-fn
                      (funcall refspec-fn config pr-number branch-name)
                    (error "Backend %s missing :refspec-for-pr"
                           (shipit-pr--backend-id))))
         (remote (if remote-fn
                     (funcall remote-fn config repo)
                   "origin"))
         ;; Step 3 of the chain: write metadata + clear progress + final refresh.
         (finish
          (lambda ()
            (condition-case err
                (shipit--write-pr-info-to-worktree
                 worktree-path
                 (list :pr-number pr-number
                       :branch branch-name
                       :pr-head-sha pr-head-sha
                       :created-at (format-time-string "%FT%T%z")))
              (error
               (shipit--debug-log
                "Failed to write .shipit-pr-info.json: %S" err)))
            (shipit-worktree--progress-clear repo pr-number)
            (shipit-worktree--refresh-section-in-buffer pr-buffer)
            (when (buffer-live-p pr-buffer)
              (with-current-buffer pr-buffer
                (message "Worktree ready at %s" worktree-path)
                (when (functionp on-done)
                  (condition-case err
                      (funcall on-done)
                    (error
                     (shipit--debug-log
                      "create-worktree on-done failed: %S" err))))))))
         ;; Step 2: kick off `git worktree add\='.
         (start-add
          (lambda ()
            (shipit-worktree--progress-set
             repo pr-number 'creating
             (format "git worktree add %s" pr-head-sha) pr-buffer)
            (shipit-worktree--refresh-section-in-buffer pr-buffer)
            (shipit-worktree--start-git-async
             repo pr-number repo-root
             (list "worktree" "add" worktree-path pr-head-sha)
             env finish nil)))
         ;; Step 1a: handle a pre-existing worktree dir, deciding
         ;; whether to short-circuit to FINISH or fall through.  Returns
         ;; non-nil when the caller should proceed with fetch / add.
         (handle-existing
          (lambda ()
            (cond
             ;; Already a valid worktree (created earlier) -- no-op +
             ;; finish so the section refreshes to the in-sync state.
             ((and (file-directory-p worktree-path)
                   (file-exists-p (expand-file-name ".git" worktree-path)))
              (shipit--debug-log
               "Worktree already exists with valid git state at: %s"
               worktree-path)
              (funcall finish)
              nil)
             ;; Empty leftover dir -- nuke it, proceed.
             ((and (file-directory-p worktree-path)
                   (seq-empty-p (directory-files worktree-path nil "^[^.]")))
              (delete-directory worktree-path t)
              t)
             ;; Dir exists with files but isn't a worktree -- bail.
             ((file-directory-p worktree-path)
              (shipit-worktree--progress-set
               repo pr-number 'failed
               (format "%s exists but is not a git worktree" worktree-path)
               pr-buffer)
              (shipit-worktree--refresh-section-in-buffer pr-buffer)
              (user-error
               "Worktree dir exists at %s with files but is not a git worktree"
               worktree-path))
             (t t))))
         ;; Step 1b: ensure parent dir + handle existing worktree dir,
         ;; then either skip-fetch -> start-add, or fetch -> start-add.
         (prep
          (lambda ()
            (shipit--ensure-worktree-in-gitignore repo-root)
            (make-directory (file-name-directory worktree-path) t)
            (when (funcall handle-existing)
              ;; Fetch if needed.
              (cond
               ((shipit--worktree--commit-present-p repo-root pr-head-sha)
                (shipit--debug-log
                 "PR commit %s already local; skipping fetch" pr-head-sha)
                (funcall start-add))
               (t
                (shipit-worktree--progress-set
                 repo pr-number 'fetching
                 (format "git fetch %s %s" remote refspec) pr-buffer)
                (shipit-worktree--refresh-section-in-buffer pr-buffer)
                (let ((args (append (list "fetch")
                                    (and shipit-worktree-fetch-blobless
                                         (list "--filter=blob:none"))
                                    (list remote refspec))))
                  (shipit-worktree--start-git-async
                   repo pr-number repo-root args env start-add nil))))))))
    (funcall prep)))

(defun shipit--lfs-pull-all-worktree-async (pr-number repo worktree-path pr-buffer)
  "Async `git lfs pull\=' (no `--include\=') inside WORKTREE-PATH.
Materialises every LFS-tracked file in the worktree -- the
typical \"I want to actually build / run the code\" path,
since most LFS fixtures the code reads weren\='t modified by
the PR diff.  Updates the Worktree section progress phase the
same way the changed-paths variant does."
  (shipit-worktree--progress-set
   repo pr-number 'pulling-lfs
   "git lfs pull (all LFS)" pr-buffer)
  (shipit-worktree--refresh-section-in-buffer pr-buffer)
  (shipit-worktree--progress-poll-start
   repo pr-number worktree-path nil pr-buffer)
  (shipit-worktree--start-git-async
   repo pr-number worktree-path
   (append (shipit-worktree--lfs-pull-prefix-args)
           (list "lfs" "pull"))
   nil
   (lambda ()
     (shipit-worktree--progress-poll-stop repo pr-number)
     (shipit-worktree--progress-clear repo pr-number)
     (shipit-worktree--lfs-status-invalidate worktree-path)
     (shipit-worktree--refresh-section-in-buffer pr-buffer)
     (when (buffer-live-p pr-buffer)
       (with-current-buffer pr-buffer
         (message "LFS pull complete (full)"))))
   (lambda (exit _stderr)
     (shipit-worktree--progress-poll-stop repo pr-number)
     (when (buffer-live-p pr-buffer)
       (with-current-buffer pr-buffer
         (message "git lfs pull failed (exit=%d); see debug log"
                  exit))))
   (shipit-worktree--lfs-progress-filter repo pr-number pr-buffer)))

(defun shipit-worktree--lfs-progress-filter (repo pr-number pr-buffer)
  "Return a process filter that updates LFS pull progress in PR-BUFFER.
Parses `git lfs pull' status lines (`Downloading LFS objects:
N%% (X/Y), Z KB | W KB/s') -- emitted to stderr -- and updates
`shipit-worktree--progress' so the Worktree section shows
live counters.  Throttled to one refresh per 200ms so a tight
download loop doesn't flood section re-renders."
  (let ((last-update 0)
        (last-key nil)
        (logged-first nil))
    (lambda (_proc chunk)
      ;; Diagnostic: log a slice of the raw chunk so we can verify the
      ;; regex matches the user's git-lfs output format.  Logs the
      ;; first non-empty chunk per filter invocation only.
      (when (and (not logged-first)
                 (> (length chunk) 0)
                 (boundp 'shipit-debug-log-enabled)
                 shipit-debug-log-enabled)
        (setq logged-first t)
        (shipit--debug-log
         "lfs-filter: first chunk (%d bytes) sample=%S"
         (length chunk)
         (substring chunk 0 (min 200 (length chunk)))))
      ;; Find the LAST progress line in the chunk (chunks may contain
      ;; many CR-separated updates from a single repaint cycle).
      (let ((re "Downloading LFS objects:[^\n]*?\\([0-9]+\\)%[^\n]*?(\\([0-9]+\\)/\\([0-9]+\\))[^\n]*")
            (pos 0)
            (match nil))
        (while (string-match re chunk pos)
          (setq match (list (match-string 1 chunk)
                            (match-string 2 chunk)
                            (match-string 3 chunk)
                            (match-end 0)))
          (setq pos (nth 3 match)))
        (when match
          (let* ((pct (nth 0 match))
                 (current (nth 1 match))
                 (total (nth 2 match))
                 (key (concat pct "/" current "/" total))
                 (now (float-time)))
            (when (and (not (string= key (or last-key "")))
                       (> (- now last-update) 0.2))
              (setq last-update now last-key key)
              (shipit-worktree--progress-set
               repo pr-number 'pulling-lfs
               (format "git lfs pull -- %s%% (%s/%s)" pct current total)
               pr-buffer)
              (shipit-worktree--refresh-section-in-buffer pr-buffer))))))))

(defun shipit--lfs-pull-pr-worktree-async (pr-number repo worktree-path paths pr-buffer)
  "Async variant of `git lfs pull --include=PATHS\=' inside WORKTREE-PATH.
Updates `shipit-worktree--progress\=' to `pulling-lfs\=' for the
duration, threads a live-progress filter that updates the
percentage/count detail string as `git lfs\=' reports it, and
refreshes the Worktree section in PR-BUFFER on completion."
  (when paths
    (shipit-worktree--progress-set
     repo pr-number 'pulling-lfs
     (format "git lfs pull (%d path%s)"
             (length paths) (if (= 1 (length paths)) "" "s"))
     pr-buffer)
    (shipit-worktree--refresh-section-in-buffer pr-buffer)
    (shipit-worktree--progress-poll-start
     repo pr-number worktree-path paths pr-buffer)
    (let ((csv (mapconcat #'identity paths ",")))
      (shipit-worktree--start-git-async
       repo pr-number worktree-path
       (append (shipit-worktree--lfs-pull-prefix-args)
               (list "lfs" "pull" "--include" csv))
       nil
       (lambda ()
         (shipit-worktree--progress-poll-stop repo pr-number)
         (shipit-worktree--progress-clear repo pr-number)
         (shipit-worktree--lfs-status-invalidate worktree-path)
         (shipit-worktree--refresh-section-in-buffer pr-buffer)
         (when (buffer-live-p pr-buffer)
           (with-current-buffer pr-buffer
             (message "LFS pull complete (%d path%s)"
                      (length paths)
                      (if (= 1 (length paths)) "" "s")))))
       (lambda (exit _stderr)
         (shipit-worktree--progress-poll-stop repo pr-number)
         (when (buffer-live-p pr-buffer)
           (with-current-buffer pr-buffer
             (message "git lfs pull failed (exit=%d); see debug log"
                      exit))))
       (shipit-worktree--lfs-progress-filter repo pr-number pr-buffer)))))

(defun shipit--worktree-pattern-in-file-p (file-path pattern)
  "Check if PATTERN is in FILE-PATH."
  (and (file-exists-p file-path)
       (with-temp-buffer
         (insert-file-contents file-path)
         (string-match-p (regexp-quote pattern) (buffer-string)))))

(defun shipit--ensure-worktree-in-gitignore (repo-root)
  "Ensure shipit-worktree-directory is ignored in REPO-ROOT.
Only adds to .gitignore if not already in .gitignore or .git/info/exclude."
  (let ((gitignore-path (expand-file-name ".gitignore" repo-root))
        (exclude-path (expand-file-name ".git/info/exclude" repo-root))
        (pattern shipit-worktree-directory))
    ;; Skip if already in private exclude file
    (unless (shipit--worktree-pattern-in-file-p exclude-path pattern)
      ;; Skip if already in .gitignore
      (unless (shipit--worktree-pattern-in-file-p gitignore-path pattern)
        ;; Add to .gitignore
        (if (file-exists-p gitignore-path)
            (with-temp-buffer
              (insert-file-contents gitignore-path)
              (goto-char (point-max))
              (insert "\n" pattern)
              (write-region (point-min) (point-max) gitignore-path))
          ;; Create .gitignore if it doesn't exist
          (with-temp-file gitignore-path
            (insert pattern)))))))

(defun shipit--worktree-exclude-pr-info (worktree-path)
  "Ensure `.shipit-pr-info.json' is in the worktree\\='s git exclude file.
Git worktrees keep their own private exclude at
`<repo>/.git/worktrees/<name>/info/exclude'; without an entry there
the JSON shipit drops at the worktree root shows up untracked in
`git status', which makes `shipit--sync-worktree' abort because it
treats any porcelain output as `worktree has uncommitted changes'."
  (let* ((default-directory (file-name-as-directory worktree-path))
         (git-dir (ignore-errors
                    (string-trim
                     (shell-command-to-string "git rev-parse --git-dir"))))
         (exclude (and git-dir
                       (expand-file-name "info/exclude"
                                         (expand-file-name git-dir
                                                           worktree-path))))
         (pattern ".shipit-pr-info.json"))
    (when (and exclude (file-exists-p exclude))
      (unless (shipit--worktree-pattern-in-file-p exclude pattern)
        (with-temp-buffer
          (insert-file-contents exclude)
          (goto-char (point-max))
          (unless (or (= (point) (point-min))
                      (eq (char-before) ?\n))
            (insert "\n"))
          (insert pattern "\n")
          (write-region (point-min) (point-max) exclude))))))

(defun shipit--write-pr-info-to-worktree (worktree-path pr-info)
  "Write .shipit-pr-info.json to WORKTREE-PATH with PR-INFO plist."
  (let* ((info-file (expand-file-name ".shipit-pr-info.json" worktree-path))
         (json-data `((pr_number . ,(plist-get pr-info :pr-number))
                      (branch . ,(plist-get pr-info :branch))
                      (pr_head_sha . ,(plist-get pr-info :pr-head-sha))
                      (created_at . ,(plist-get pr-info :created-at))
                      (repo . ,(shipit--get-repo-from-remote))))
         (json-str (json-encode json-data)))
    (with-temp-file info-file
      (insert json-str))
    ;; Quiet `git status' inside the worktree -- our metadata file
    ;; should never count as untracked work that blocks sync.
    (shipit--worktree-exclude-pr-info worktree-path)))

(defun shipit--sync-worktree (worktree-path branch-name)
  "Sync WORKTREE-PATH with latest changes from BRANCH-NAME.
Aborts when the worktree has uncommitted changes -- the sync
runs `git reset --hard' which would silently destroy them.

Strategy: `git fetch origin BRANCH' then `git reset --hard
FETCH_HEAD'.  No merge, no rebase, no conflict resolution -- the
worktree is treated as a read-only mirror of what the PR author
pushed.  This eliminates the merge-conflict class that
`git pull' produced whenever the author force-pushed (rewrote
history), since reset just moves the branch pointer to the new
tip and the team\\='s merge-vs-rebase preference is irrelevant
when we don\\='t have local commits to integrate.

Returns t on success, nil on error.  Failures surface via
`message' as well as the debug log so the user can see why a
sync didn\\='t happen."
  (condition-case err
      (let* ((default-directory (file-name-as-directory worktree-path))
             (raw-status (string-trim
                          (shell-command-to-string "git status --porcelain")))
             ;; Filter our own metadata file from the porcelain output --
             ;; worktrees created before the exclude-file fix still
             ;; surface `.shipit-pr-info.json\=' as untracked, and we
             ;; shouldn\='t refuse to sync over a file shipit itself
             ;; writes on every PR open.
             (status
              (string-trim
               (mapconcat
                #'identity
                (cl-remove-if
                 (lambda (line)
                   (string-match-p
                    "\\`\\(\\?\\?\\|[ ACDMRTU][ ACDMRTU]\\) +\\.shipit-pr-info\\.json\\\'"
                    line))
                 (split-string raw-status "\n" t))
                "\n"))))
        (shipit--debug-log "Syncing worktree: %s" worktree-path)
        (unless (string-empty-p status)
          (error
           "Worktree has uncommitted changes -- commit / stash / discard before sync:\n%s"
           status))
        ;; Make sure the worktree exclude has our metadata file so
        ;; future syncs + the user\='s own `git status\=' calls don\='t
        ;; surface it.
        (shipit--worktree-exclude-pr-info worktree-path)
        ;; Fetch the PR branch.  FETCH_HEAD points to the freshly
        ;; fetched tip regardless of whether a tracking ref exists.
        (let ((exit (call-process "git" nil nil nil
                                  "fetch" "origin" branch-name)))
          (unless (= exit 0)
            (error "git fetch origin %s failed (exit %d)"
                   branch-name exit)))
        ;; Move the worktree's branch pointer to the fetched tip.
        ;; No merge step, so a force-push can't produce conflicts.
        (let ((exit (call-process "git" nil nil nil
                                  "reset" "--hard" "FETCH_HEAD")))
          (unless (= exit 0)
            (error "git reset --hard FETCH_HEAD failed (exit %d)" exit)))
        ;; Update PR head SHA in metadata file.
        (when-let* ((pr-info (shipit--get-pr-info-from-worktree worktree-path)))
          (let ((latest-sha (string-trim
                             (shell-command-to-string "git rev-parse HEAD"))))
            (shipit--write-pr-info-to-worktree
             worktree-path
             (plist-put pr-info :pr-head-sha latest-sha))))
        t)
    (error
     (shipit--debug-log "Failed to sync worktree: %s" err)
     (message "shipit: sync failed -- %s" (error-message-string err))
     nil)))

(defcustom shipit-worktree-show-lfs-indicator t
  "When non-nil, the Worktree section shows an `LFS: X/Y\=' indicator.
The indicator counts LFS-tracked files that are materialised
locally (`*\=' flag from `git lfs ls-files\=') vs the total LFS
file count.  Set to nil on very large monorepos where the
sub-second `git lfs ls-files\=' call adds visible latency to
every PR-buffer render -- the indicator is informational only,
the `l\=' action still works without it."
  :type 'boolean
  :group 'shipit)

(defcustom shipit-worktree-lfs-status-cache-ttl 30
  "Seconds to cache `shipit-worktree--lfs-status\=' results per worktree path.
The status helper shells out to `git lfs ls-files\=' which on a
3000+ file worktree takes ~0.5-1s and runs once per Worktree
section render.  An incremental PR buffer refresh re-renders
the section several times during one fetch cycle; without
caching that\='s seconds of `git lfs\=' subprocess overhead per
PR open.  The cache is invalidated automatically when a pull
completes via `shipit-worktree--lfs-status-invalidate\='."
  :type 'number
  :group 'shipit)

(defvar shipit-worktree--lfs-status-cache (make-hash-table :test 'equal)
  "Hash of WORKTREE-PATH -> (TIMESTAMP . RESULT) for `--lfs-status\='.
TIMESTAMP is `float-time\='; RESULT is the return value (`:total\='
plist or nil).  Entries older than
`shipit-worktree-lfs-status-cache-ttl\=' are recomputed.")

(defun shipit-worktree--lfs-status-invalidate (worktree-path)
  "Drop the cached `--lfs-status\=' entry for WORKTREE-PATH.
Called by the LFS pull async sentinels on completion so the
next section render reflects newly-materialised files."
  (when worktree-path
    (remhash (file-name-as-directory worktree-path)
             shipit-worktree--lfs-status-cache)))

(defun shipit-worktree--lfs-status-uncached (worktree-path)
  "Compute `--lfs-status\=' for WORKTREE-PATH without consulting the cache.
Forks `git lfs ls-files\=' and counts `*\=' / `-\=' flag lines."
  (when (and worktree-path (file-directory-p worktree-path))
    (with-temp-buffer
      (let ((exit (shipit--worktree--git
                   worktree-path t "lfs" "ls-files")))
        (when (zerop exit)
          (let* ((raw-lines (split-string (buffer-string) "\n" t))
                 (total (length raw-lines))
                 (present 0))
            (dolist (line raw-lines)
              (when (string-match "^[0-9a-f]+[ \t]+\\([*-]\\)" line)
                (when (string= (match-string 1 line) "*")
                  (cl-incf present))))
            (when (> total 0)
              (list :total total :present present))))))))

(defun shipit-worktree--lfs-status (worktree-path)
  "Return (:total N :present M) for LFS files in WORKTREE-PATH, or nil.
Memoised for `shipit-worktree-lfs-status-cache-ttl\=' seconds per
WORKTREE-PATH.  The Worktree section can re-render several
times during one PR buffer construction (incremental refresh
cycle); without the cache each render shells out to `git lfs
ls-files\=' which on big worktrees costs ~0.5-1s.  Cache is
invalidated on LFS pull completion."
  (when worktree-path
    (let* ((key (file-name-as-directory worktree-path))
           (now (float-time))
           (cached (gethash key shipit-worktree--lfs-status-cache))
           (cached-time (and cached (car cached)))
           (cached-fresh (and cached-time
                              (< (- now cached-time)
                                 (max 1 shipit-worktree-lfs-status-cache-ttl)))))
      (cond
       (cached-fresh (cdr cached))
       (t
        (let ((result (shipit-worktree--lfs-status-uncached worktree-path)))
          (puthash key (cons now result) shipit-worktree--lfs-status-cache)
          result))))))

(defun shipit--worktree--object-reachable-p (worktree-path sha)
  "Return non-nil when SHA is locally reachable from WORKTREE-PATH.
Cheap precheck before `git diff SHA..HEAD\='; the diff fails
hard when SHA is missing, which on detached worktrees of
`pull/N/head\=' is the common case (base-branch tip never
fetched into the local object store)."
  (zerop (shipit--worktree--git
          worktree-path nil
          "cat-file" "-e" (format "%s^{commit}" sha))))

(defun shipit--worktree-changed-paths-since (worktree-path base-sha &optional base-branch)
  "Return paths changed in WORKTREE-PATH between BASE-SHA and HEAD.
Runs `git diff --name-only DIFF-BASE..HEAD\=' inside the
worktree, where DIFF-BASE is whichever of these is locally
reachable, in order:

1. BASE-SHA -- the SHA returned by GitHub for the PR\='s base
   commit.  Often missing on a worktree created from `pull/N/
   head\=' alone.
2. `origin/BASE-BRANCH\=' -- when BASE-BRANCH is supplied
   (typically \"main\" / \"master\" from PR data).
   `git fetch\=' on the original `git worktree add\=' may have
   pulled this transitively.
3. `BASE-BRANCH\=' -- a local branch with that name, last resort.

Errors out with the actionable hint when none reach.  Returns
nil for an empty diff (no path changed)."
  (let ((diff-base
         (cond
          ((shipit--worktree--object-reachable-p worktree-path base-sha)
           base-sha)
          ((and base-branch
                (shipit--worktree--object-reachable-p
                 worktree-path (concat "origin/" base-branch)))
           (concat "origin/" base-branch))
          ((and base-branch
                (shipit--worktree--object-reachable-p
                 worktree-path base-branch))
           base-branch)
          (t nil))))
    (unless diff-base
      (error
       "Cannot find a diff base for the PR -- base SHA %s not local%s.  Try `git fetch origin %s\=' in the worktree first"
       (substring base-sha 0 (min 7 (length base-sha)))
       (if base-branch (format ", `origin/%s\=' not local either" base-branch) "")
       (or base-branch "<base-branch>")))
    (shipit--debug-log
     "lfs-changed-paths: diff-base=%s in %s" diff-base worktree-path)
    (with-temp-buffer
      (let ((exit (shipit--worktree--git
                   worktree-path t
                   "diff" "--name-only"
                   (format "%s..HEAD" diff-base))))
        (unless (zerop exit)
          (error "git diff %s..HEAD failed (exit=%d)"
                 diff-base exit))
        (split-string (buffer-string) "\n" t)))))

(defun shipit-worktree--lfs-tracked-paths (worktree-path)
  "Return list of LFS-tracked paths in WORKTREE-PATH, or nil.
Runs `git lfs ls-files\=' and extracts the path column from each
line.  Used by `shipit--lfs-pull-pr-worktree\=' to filter the
PR\='s changed-paths set down to just LFS-tracked ones, so we
can both (a) skip the pull entirely when no LFS file actually
needs materialising and (b) tell the user how many candidates
exist."
  (when (and worktree-path (file-directory-p worktree-path))
    (with-temp-buffer
      (let ((exit (shipit--worktree--git
                   worktree-path t "lfs" "ls-files")))
        (when (zerop exit)
          (let ((paths nil))
            (goto-char (point-min))
            (while (re-search-forward
                    "^[0-9a-f]+[ \t]+[*-][ \t]+\\(.+\\)$"
                    nil t)
              (push (match-string 1) paths))
            (nreverse paths)))))))

(defun shipit-worktree--lfs-glob-candidates (worktree-path)
  "Return completion candidates for the interactive LFS pull prompt.
Combines, in order:

1. Every LFS-tracked path verbatim (full path -- `tests/data/x.bin\=').
2. A `<dir>/**\=' anchor for each unique directory containing
   an LFS-tracked file (one per dir, e.g. `tests/data/**\=' to
   pull every LFS file under that subtree).
3. Generic file-extension globs (`**/*.bin\=', `**/*.png\=', ...) for
   each distinct extension that appears among the LFS paths.

`git lfs pull --include\=' uses gitattributes-style globs (`*\='
within a segment, `**\=' recursive, `?\=' single char) so any of
these strings is directly usable as the include pattern.  Free-
form input is still accepted at the prompt -- this just gives
the user good starting points."
  (let ((paths (shipit-worktree--lfs-tracked-paths worktree-path))
        (seen (make-hash-table :test 'equal)))
    (dolist (p paths)
      (puthash p t seen))
    (dolist (p paths)
      ;; <dir>/**
      (let ((dir (file-name-directory p)))
        (when (and dir (> (length dir) 0))
          (puthash (concat dir "**") t seen)))
      ;; **/*.<ext>
      (let ((ext (file-name-extension p)))
        (when (and ext (> (length ext) 0))
          (puthash (concat "**/*." ext) t seen))))
    (sort (hash-table-keys seen) #'string<)))

(defun shipit--worktree-lfs-pull-paths (worktree-path paths)
  "Run `git lfs pull --include=PATHS\=' in WORKTREE-PATH.
PATHS is a list of repo-relative paths.  Joins them with `,\='
\(LFS\='s `--include\=' separator).  Returns the exit code.
No-op when PATHS is empty."
  (when paths
    (let* ((csv (mapconcat #'identity paths ","))
           (exit (shipit--worktree--git
                  worktree-path nil
                  "lfs" "pull" "--include" csv)))
      (shipit--debug-log
       "git lfs pull --include=%s exit=%d (%d path%s)"
       csv exit (length paths) (if (= 1 (length paths)) "" "s"))
      exit)))

(defun shipit--delete-worktree (worktree-path)
  "Delete worktree at WORKTREE-PATH.
Returns t on success, nil on error."
  (condition-case err
      (progn
        (shipit--debug-log "Deleting worktree: %s" worktree-path)
        (let ((cmd (format "cd %s && git worktree remove %s"
                          (shell-quote-argument (shipit--get-repo-root))
                          (shell-quote-argument worktree-path))))
          (let ((exit-code (shell-command cmd)))
            (unless (= exit-code 0)
              (error "Git worktree removal failed with exit code %d" exit-code))))
        t)
    (error
     (shipit--debug-log "Failed to delete worktree: %s" err)
     nil)))

(defun shipit--open-worktree-directory (worktree-path)
  "Open WORKTREE-PATH in dired buffer.
Returns t on success, nil on error."
  (condition-case err
      (progn
        (shipit--debug-log "Opening worktree directory: %s" worktree-path)
        (if (file-directory-p worktree-path)
            (dired worktree-path)
          (user-error "Worktree path not found: %s" worktree-path))
        t)
    (error
     (shipit--debug-log "Failed to open worktree: %s" err)
     nil)))

(provide 'shipit-worktree)
;;; shipit-worktree.el ends here
