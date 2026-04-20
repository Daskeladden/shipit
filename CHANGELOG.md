# Changelog

## v1.4.0 (2026-04-20)

### ⚠️ Breaking: dependency bump

- **Package-Requires** now demands `emacs 28.1`, `magit 4.5.0` and the newly split-out `magit-section 4.5.0` (previously `emacs 27.1` / `magit 3.0.0`). The bump unlocks magit's paint/visibility-indicator APIs that shipit already relies on, and plugs a regression in magit 4.6 that was clearing diff `face` text properties during refontification.
- **Optional**: install `consult` to get live server-side issue search for the new code-refs picker. Without consult, the non-consult fallback uses vanilla `completing-read` with dynamic re-fetching — no dependency required, still interactive.

### Features

- **Code-refs** — new module that highlights Jira-style issue keys in code comments and makes them actionable:
  - `shipit-code-refs-mode` (and `global-shipit-code-refs-mode` for `prog-mode`/`text-mode`/`yaml-ts-mode` etc.) underlines `PRJ-1234` refs with RET/M-; actions to open in shipit, browse in the web UI, copy URL/key.
  - `completion-at-point` kicks in after `PRJ-` inside a comment; candidates come from the repo's backend `:search` and are cached per project.
  - `shipit-code-refs-auto-picker` auto-opens the picker on `PRJ-`, replacing the typed prefix with the selected key. Works with `electric-pair-mode` (skips auto-closed delims) and with your preferred insert format via `shipit-code-refs-insert-format` (`key`, `key-title`, or a format string using `%k`/`%t`/`%u`).
  - Picker includes a `+ Create new issue` entry that opens `shipit-issue-create-buffer`.
  - Embark target finder exposes the key as `shipit-issue-ref` so users can bind their own actions.
  - Narrowing: only prefixes in `:project-keys` are highlighted; tree-sitter-aware comment detection keeps typing responsive in `yaml-ts-mode`; `shipit-code-refs-dwim` opens the menu on `C-u RET`.
- **PR diff rendering** — opt-in language syntax inside unified diff hunks (`shipit-pr-fontify-hunks`) plus ediff-style intra-line refinement (`shipit-pr-refine-hunks`). Both have runtime toggles (`T f`, `T r`) that re-lay the existing buffer in place — no API refetch.
- **C-RET on a file or hunk** opens the working-tree file via plain `find-file` so LSP, xref, flycheck attach normally. When the working tree's HEAD differs from the PR head SHA, the echo area prints a short warning.
- **Linked PRs section on issue buffers** — auto-detects PRs that reference the issue (reverse of the PR-buffer "Linked Issue") with SVG icons and collapsible per-PR sections.
- **Activity navigation from notifications** now works on issue buffers too — RET on an activity line in an expanded notification jumps to the comment/commit/review and pulses the landing line.
- **Section visibility indicators** — new `shipit-section-visibility-indicators` defcustom that forwards magit's `>`/`v` folding indicator preference to all shipit sections.

### Bug fixes

- **Diff line colors** — bulk face → `font-lock-face` migration across shipit-pr-sections/shipit-render/shipit-pr-linked-issue fixes line coloring under magit 4.6's new repaint cycle. Context lines now also get `magit-diff-context`. Header +/- stats are colored again.
- **General comment headers** — username uses `shipit-username-face` and timestamp uses `shipit-timestamp-face` again. The whole-header `font-lock-face 'default` override (introduced by the bulk face→font-lock-face migration) was hiding them behind `magit-section-heading`.
- **Marginalia annotator** — `shipit--select-pr` no longer crashes with `invalid-function marginalia--fields` when shipit is byte-compiled without marginalia loaded. The macro is now expanded at runtime via `eval`.
- **Globalized code-refs mode** — `global-shipit-code-refs-mode` uses an inline turn-on lambda so activation works regardless of when the file was byte-compiled.
- **Code-refs diagnose** — no longer hangs on large buffers; uses a scan cap and lazy syntax-ppss.

---

## v1.3.0 (2026-04-19)

### Performance

- **Large issue buffers open in seconds, not minutes.** Popular GitHub issues (e.g. one with 500+ comments and 300+ reactions per comment) went from ~2.5 min to ~5 s to fully loaded. Combined wins:
  - Issue activity uses `/issues/N/events` (state changes only) async with a 3-page cap, instead of sync `/issues/N/timeline` paginating through every comment-as-event.
  - Reaction counts come from the `reactions` summary object on issue/PR/comment GET responses; the per-comment reactor fetch stops at page 1 (tooltip shows first 5 names + "+N more") instead of paginating 300+ reactors per comment.
  - Avatar downloads prefetch in parallel when head+tail metadata arrives, so the sync `shipit--download-and-cache-avatar` call during each comment render becomes a file-cache hit.
  - `shipit--wrap-text` reuses a single persistent scratch buffer instead of allocating a fresh `with-temp-buffer` per wrapped line — a 22 KB comment dropped from ~5.8 s to ~0.15 s to render.
  - Comments section replacement rewrites only the body via targeted `replace-match` on the heading count and a marker-preserving body swap, instead of deleting the whole section. The body insert skips intermediate redisplay yields so the user's window-start stays anchored where they had it.
  - Removed debug-log calls that were writing ~700 KB per comment to disk (full reaction alists dumped via `%S`) plus hundreds of per-reaction `Processing reaction...` lines.

### Bug fixes

- `shipit--extract-reaction-summary` now looks up GitHub reaction keys (`+1`, `-1`, `laugh`, …) via `(intern "+1")` at runtime. In Elisp source `+1` and `-1` read as *integers* — the previous quoted-symbol lookup silently dropped thumbs-up and thumbs-down counts and pushed callers onto the paginated fallback.
- `shipit--api-request-paginated-async` no longer drops the callback when `max-pages` is hit on a full final page. The capped events fetch (3 pages × 100 items) hit this path on popular issues and left the activity section stuck on "Loading…" forever.
- Added a `condition-case` + backtrace capture around the pinned-comment async insertion so any void-variable error in the insertion path lands in the debug log instead of a silent process-filter failure.

---

## v1.2.0 (2026-04-18)

### Features

- **Atlassian dashboard** — new `M-x shipit-atlassian-dashboard` buffer for Jira projects with My Open Issues, What's Next, Kanban Board, Frequently Visited, and a filterable Issues section. Refuses to open when the active backend isn't Jira.
- **Issues filters** — rich filter transient covering status, assignee, reporter, type, priority, resolution, component, text/comment search, sort, plus timeline filters (Updated since/before, Created since/before). Assignee, reporter, status, and component filters use dynamic completion against the Jira backend.
- **Create issue from dashboard** — `M-; n` on the dashboard opens the rich create buffer with Jira fields (issue type, components, labels, assignee) pre-wired to the dashboard's repo.
- **Linked tracker issue on PR buffer** — new "Issue" section on each PR auto-detects the key from the branch name (e.g. `ZIVID-12624-foo`) with a transient for link/clear/open/browse/copy-URL/transition/create-and-link. Manual overrides persist per (repo, pr-number) in `~/.emacs.d/shipit-pr-linked-issues.el`.
- **Activity navigation from notifications** — pressing RET on an activity line in an expanded PR notification opens the PR and jumps directly to the comment/commit/review; a pulse highlights the landing line.
- **Jira comment reactions** — issue comments render reaction rows via the internal DC/Server endpoint (Cloud stays placeholder-only, tracked by JRACLOUD-78153).
- **Shebang language detection** — code blocks without a fence language tag get syntax highlighting inferred from `#!/usr/bin/env python` style shebang lines.

### Performance

- Atlassian dashboard now fetches the four sections concurrently via the Jira async API. Render happens once at completion instead of four times (each render regenerates every SVG icon — ~5× faster end-to-end on large projects).
- Memoised Jira issue-type / priority icons; rendering ~450 rows now calls svg-lib ~10 times instead of ~450. Dashboards with many issues load in ~1 s rather than ~15 s.
- Board pagination bumped to `maxResults=100`, halving round trips.

### Fixes

- **Empty Issues section** — JQL now quotes project keys (`project in ("ZIVID")` instead of `project in (ZIVID)`); Jira's `/search/jql` endpoint rejects the unquoted form with HTTP 400.
- **Duplicated comments** — head-and-tail pagination fallback no longer renders the same comment twice when the total fits in the window.
- **Thread subscription on Jira issues** — `w` on a Jira issue buffer (string key like `ZIVID-12624`) no longer raises a wrong-type-argument error; thread subscription is GitHub-only and now elides cleanly for string IDs.
- **Stale commit sub-sections** — commit detail sub-sections are collapsed by default.

### Docs

- README: correct Jira config key (`:base-url`, not `:host`), add `shipit-issue-repo-backends` example, mention the dashboard guard, and list the new features in the top-level summary.

## v1.1.0 (2026-04-12)

### Features

- **Thread subscriptions** — subscribe to individual PRs, Issues, and Discussions via `w` then `t` in the subscription transient
- **Pinned comments** — shown prominently in issue buffers with SVG pin icon, truncated preview, collapsible full body, and rounded background
- **Comment filters** — press `f` to filter by author, date, text search, hide bots, min reactions; filters stack with AND logic
- **Comment pagination** — large issues show first and last N comments with a Load more transient (`l`); supports recent-first and oldest-first direction toggle
- **Comment position indicator** — mode line shows current position when navigating comments
- **Issue notification timeline** — expanding an issue notification shows recent activity

### Performance

- Lazy comment fetching via Link header (2 API calls instead of N for large issues)
- Reactions fetched only for visible comments, not all
- Async description reactions
- Recursive reaction pagination with no cap
- Markdown render cache by content hash
- Last-page timeline fetch via Link header for notifications

## v1.0.0 (2026-02-16)

Initial public release.

### Features

- **Code review workflow** — submit reviews, approve or request changes, reply to comment threads, resolve conversations
- **Inline diff comments** — view and post comments on specific lines in diff buffers, with full threading support
- **Notifications** — modeline indicator with mention/total counts, dedicated notification buffer, mark as read
- **PR browsing** — description, labels, assignees, reviewers, file changes with syntax highlighting
- **Comment editor** — multi-line editing with `#` PR refs, `@` mentions, `:` emoji completion, and live markdown preview
- **CI/Checks** — view check suite results inline with hierarchical status display
- **Worktrees** — checkout PR branches into isolated worktrees with automatic context tracking
- **Issues** — search, view, and create issues with full backend support
- **Discussions** — GitHub Discussions search, viewing, and creation
- **Rich rendering** — inline images, avatars, syntax-highlighted code blocks, mermaid diagrams
- **Backend abstraction** — pluggable backend registries for PR, comment, and issue operations
