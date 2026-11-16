# Changelog

## v1.8.0 (2026-11-16)

### Features

#### Review mode and worktrees

- `C-RET` on a file or on a diff line of a pull request opens the file in review mode. The file comes from the worktree of the pull request. Without a worktree, it asks to create one. shipit then makes the worktree in the background and opens the file at the line. If the clone is at the head of the pull request, `C-RET` does not ask.
- shipit also uses a worktree that you made yourself: one with the head branch of the pull request checked out, or one at its head commit. The Worktree section names the worktree in use and says whether it is yours or made by shipit. "Sync worktree" and "Delete worktree" act only on a worktree that shipit made.
- The worktree commands also work in a clone of your fork that has no remote for the repository of the pull request. "Checkout in worktree", `C-RET` and "Sync worktree" fetch the pull request from its own repository.

#### Pull requests and issues

- Views of a page. `v` shows the next view, and `V` asks for one by its name. The overview of a pull request shows the title, the State and Approval lines, the reviews and comments that need your answer, and the checks. The overview of an issue shows its title line, its assignees and the pull requests that link to it. `shipit-page-views` names the views.
- `SPC` in a pull request buffer or an issue buffer opens a menu of the commands of the buffer, and `SPC` closes it again.
- `O` toggles the order of the section at point between the oldest first and the newest first: the activity, the comments of an issue, or the general comments of a pull request. `shipit-comments-order` sets the order that a session starts with.
- A review row under Activity opens to the line comments of the review, each one below its file and line.
- A renamed file shows its old and its new path, as `R src/old.txt -> src/new.txt`. A text filter also finds the file by its old path.
- `k` removes the draft comment at point, after a question.
- The issue buffer changes the title, the state, the labels and the assignee of a GitHub issue. An open issue closes as completed or as not planned, and a closed issue opens again.
- The issue buffer has an imenu index. In a pull request buffer, consult-imenu shows an entry before you choose it.
- `C-u M-w` on a line of a code block copies the code of the block, without its fences and without the indent of the page. `M-;` offers Copy code block, and the `w` of embark copies the code too.
- The preview of a pull request opens while the mouse rests on a link to it. The preview leaves the keyboard where it is. `shipit-pr-mouse-preview-delay` sets the wait. To leave this off, set `shipit-pr-mouse-preview-auto-enable` to nil. Only the GitHub backend has it.

#### Branches

- `shipit-branches`, `B` in the menu of shipit, shows the branches of a repository in the groups of the branches page of GitHub. `RET` opens the pull request of a branch, or it shows how the branch differs from the default branch. `f` filters the branches as you type. Only the GitHub backend lists branches.
- `w` in that buffer watches the repository. A new branch whose last commit someone else made then gives a notification. `shipit-branches-watched-repos` holds the watched repositories.
- In the view of a branch, `o` opens its compare page in the browser.

#### Explore

- `shipit-explore`, `e` in the menu of shipit, shows what the people you follow and the repositories you watch do on GitHub: stars, forks, new repositories and releases. `RET` opens the repository in shipit, and `SPC` or `f` opens a menu of what is shown.
- With `shipit-explore-notify`, one notification says how many entries of the feed are new.

#### Notifications

- An expanded notification stays expanded after a redraw, and the cursor stays on its notification.
- `SPC` opens and closes the filter menu of the notifications buffer.
- Another package can put a notification among those of shipit with `shipit-notifications-push`. Such a notification can count as a mention, and shipit keeps the newest read ones for the session.
- The subscriptions buffer lists your own repositories that you do not watch, in the section "Yours, not watched".

#### Stacked pull requests

- The stacked preview fetches from `origin` each time it opens, and on `g`. A row says "rebased on the remote", "new commits on the remote" or "merged into" its base.
- `b` in the stacked preview opens a branch menu, and `r` opens a rebase menu, as in magit. The branch menu holds what `c`, `r` and `a` did before, so `b c` puts a branch on each layer and `b m` renames a layer.
- The rebase menu shows its plan first: which layers move onto the base, and which ones it leaves out as merged. It rebases the stack onto the newest base. It also follows the copies on the remote after a bot rebased them, and folds fixup commits into their layers. shipit works out each rebase and magit runs it with `--update-refs`, so a stop on a conflict goes to the usual flow of magit.
- If GitHub or GitLab says that the pull request of a layer merged, the layer counts as merged. Else git decides. So a layer that was squash-merged no longer shows as "not pushed".
- `RET` on the base, on the `Base:` line or on the base row, picks another base. The preview then divides the commits again, as it does at the start.
- A `Next:` line under the base names the next step of making the stack: `b c`, then `P`, then `RET` on the lowest layer without a pull request.
- `s` divides a layer at the commit at point, or joins the layer with the one above. `C-/`, `C-?`, undo-tree and vundo take back each `s` and each change of the base.
- `s` also splits a layer that has a pull request. When you submit the new lower layer, or press `R`, shipit takes the pull requests out of the stack on GitHub. Then it sets the new base of the pull request above and makes the stack again. The stack then has a new number.
- `RET` on a layer offers to push a base that the remote does not have. Before, GitHub refused the pull request of the bottom layer with "HTTP 422: Validation Failed (base invalid)".
- If the width of its window changes, the preview draws again. It asks GitHub for the checks and the reviews once, and again on `g`. So `s` and undo no longer wait about 0.5 s for GitHub.

#### Merge

- `M-m` on GitHub merges through the asynchronous merge API, so a pull request in a stack merges too. shipit says whether the pull request merged, went into the merge queue, or failed, with the reason of GitHub. For a pull request in a stack, the question names the open pull requests below it, because GitHub merges them too.
- `M-m` on a draft asks whether to mark it ready for review and merge it. For a pull request in a stack, the question also names the drafts below it, because GitHub merges no draft. Yes marks them ready too.

#### Confluence

- If someone else changes a page of Confluence Cloud that mentions you or that you watch, the change shows as a notification. An entry with `:backend confluence` in `shipit-issue-repo-backends` switches this on. The README section Notifications from Confluence tells the limits.
- `shipit-confluence-search`, `w` in the menu of shipit, finds the pages of Confluence that hold the words that you type. `RET` opens the page in the browser.
- With marginalia on, each page that the search of Confluence finds has three columns beside its title. They show the space, the user who made the last change, and the time. `marginalia-cycle` adds the text around the words found, cut to the room in the window.

#### Issues with embark

- With embark, a row of the issue search has five actions: `o` opens the issue, `b` shows it in the browser, `u` copies its address, `w` copies its key and `s` changes its status. Each action works on one row, the marked rows, all rows or the rows of the region. This also works in a buffer that embark exported.
- `s` on several rows asks one time for the new status. If Jira wants a resolution, `s` asks for it too. The last message says how many issues moved and which ones did not.

#### Rendering

- A markdown heading hides its hashes, a rule draws as a horizontal line, and the marks of a quote draw as bars.
- The HTML of a README shows as a forge shows it: paragraphs, headings, lists, quotes, centered blocks and links around pictures. An HTML table draws as a table, with its cells side by side and placed as the table says.
- A README joins the lines of each paragraph, as a forge does.
- If the package canvas-mermaid loads, it draws a mermaid block in the colors of the Emacs theme. Without it, `mmdc` draws the block as before.
- A video that was uploaded to GitHub gets a Play button, in a section that folds as on GitHub. The repository buffer shows the videos of a README as the other buffers do.
- The GIFs of a buffer move only while they are in view, at most `shipit-image-max-frame-rate` times a second.

#### Other

- The page of a repository shows the number of its stars.
- With embark, a link to a pull request or an issue in any buffer is a target, and `RET` or `o` opens it in shipit. The line of a move on a kanban board is a target too.
- The README has a section Extending shipit. It names the hook points for other packages, and a wiki page builds a small package with them.
- New hook points: `shipit-delete-at-point-functions`, `shipit-file-diff-refreshed-hook`, `shipit-notifications-poll-hook`, `shipit-code-block-copy-functions`, `shipit-video-url-functions` and `shipit-video-fold-functions`. A backend can have the new operations `:fetch-pr-async` and `:fetch-review-comments`, and `shipit-issue-register-notification-source` registers a backend that only gives notifications. A notification of a backend can carry a `revision`. If a poll brings a read notification with another revision, it comes back as unread.
- `shipit-reload` loads shipit compiled, so shipit runs faster after a reload.

### Performance

- An answer "304 Not Modified" from GitHub no longer leaves a buffer with a copy of the earlier answer. After four and a half days, one Emacs held 379 such buffers with 662 MB of text.
- shipit kills the buffer of each answer to a cached request. A session that polls its notifications held more than 500 of these buffers after one day.
- The auto-mark rules of the notifications no longer stall Emacs for about 2.6 seconds after each poll.
- Four GIFs of a README took 99% of a CPU on a display over ssh. They now take 30% in view and none out of view.

### Bug fixes

- A pull request buffer runs its git commands in a clone of its own repository. Before, a buffer that opened from the notifications ran them in the directory of Emacs, and a force push there downloaded history into another repository.
- A command on a page acts on the repository of the page. Before, a search, a new issue, the list of workflows and the comment actions went to the repository of the directory.
- shipit knows the repository of a clone whose `origin` has an `ssh://` address.
- A refused edit of an issue comment or of an issue description keeps the editor open with your text. Before, shipit said "Comment updated" and the text was lost.
- A function on `shipit-issue-created-hook` that fails no longer makes shipit say that the creation failed.
- Jira: an issue that opens from its key in a pull request buffer shows its comments. A long issue has the line that loads the comments between the first and the last. A reply on an issue goes to Jira. `RET` opens a linked pull request. Tables, bold, italic, code, quotes and code blocks in Jira text show as such.
- The heading Linked PRs of an issue says why its lookup failed.
- Review: each draft shows once in Pending Review, and the drafts stay in view after a comment is posted.
- The stacked preview keeps every layer of a submitted stack that opens again.
- The view of a branch shows every changed file with its diff, also for more than 300 files. A removed file has the mark D.
- GitLab: a merge request has its Worktree section and its worktree commands.
- Notifications: a redraw no longer fills the undo list, and a late answer no longer shows above the list.
- A comment that GitHub does not know no longer leaves the comments section empty.
- The first answer "304 Not Modified" no longer stops a request for good.
- The preview draws the code blocks and the long lines of its description as the pull request buffer does. `RET` edits the description and opens its links.
- Rendering: a table and a wrapped line inside a list item keep their place. A `<details>` block shows as the text around it. An HTML comment does not show. The background of a code block ends at its fence. A web address keeps each `_` of its text. An `<img>` tag is read with a width in px and with an address in single quotes. Without gifsicle, ImageMagick resizes a GIF. Pictures show in a graphic frame after a terminal frame. Tags show in the tag face of magit.
- The preview popups keep their thin border, and `q` closes a popup under meow.
- `shipit-reload` runs the after-load forms once every file is loaded, and it cleans the global hooks from any buffer.
- `M-m` on GitHub no longer says "merged" after GitHub refused the merge.
- A comment from `c a` in review mode shows as a draft below its line. Before, if GitHub spelled the name of the repository with other capitals than the git remote, as after a rename, the draft was lost. `C-c C-c` said "Draft queued" all the same.
- The editor of `c a` in review mode opens above the legend, with room to write. Before, it opened beside the legend, two lines high, and showed only its header line.
- `K` in a pull request buffer discards all review drafts, after a question. Before, the hints said "press `D` to discard", but `D` showed the diff with comments, and no key discarded the drafts. `D` keeps that meaning. The `SPC` menu now has `S` and `K`.
- A description or a comment whose first line has a colon, such as "Request to the kitchen: make more coffee", shows that line as text. Before, the words before the colon were bold.
- A poll of the notifications while a pull request page loads no longer puts the notifications into that page. Before, later polls failed with "Marker points into wrong buffer".
- The authors and dates of commit rows line up in a narrow window.
- After an aborted rebase, the stacked preview shows its old layers again. Before, it showed the whole stack as one layer.
- Embark: an action on an issue key in a list acts on that key. The general actions of embark, such as export, stay on an issue key. Open goes to the tracker that claims the key.
- Jira: a refused request shows the words of Jira, as in "failed (400): resolution: Field 'resolution' cannot be set." shipit sends the default resolution only with a transition that takes one. Before, Jira refused the other moves to a closed status with a 400.
- While a review session shows its legend, a menu opens above the legend with all its lines. Before, it opened beside the legend at its height: `SPC` showed 4 of its 17 lines, and `Z` hid Delete.
- After `C-c C-c` in the editor of review mode, the legend has the focus again, so the next key of review mode works. Before, the focus went to the file, and the next `c a` typed "ca" into it.
- A review that only comments goes to your own pull request too. Before, `S` with "Comment only" said "Cannot review your own PR", although GitHub refuses only an approval or a request for changes from the author.
- A pull request that is asked for after its page was drawn shows what GitHub says now. Before, shipit kept each answer from the drawing of a page until the next drawing. So the stacked preview made a new stack for the third layer, and GitHub refused it with "are already part of a stack".
- After a layer is submitted from the stacked preview, the stacked preview shows in one window. Before, it showed in two windows side by side.
- A new worktree of a pull request leaves the clone without a change. shipit adds the folder of the worktrees to the exclude file of the clone. Before, it wrote the folder into the `.gitignore` of the repository, so each repository where you reviewed got a change to commit.
- A draft says "PENDING · not sent yet". Before, it said "local · won't publish", which reads like a draft that never goes to GitHub.
- A draft in review mode no longer makes shipit say "File section not visible".
- An uploaded video above a table shows as a video. Before, pandoc aligned the table by rewriting the whole text, which put the address of the video in angle brackets. Now only the rows of each table go to pandoc.
- `TAB` and `RET` on the header of a video in a comment show and play the video. Before, the keymap of the comment covered the header, so `TAB` folded the comment.
- Each column of a table is as wide as the widest text that it shows. Before, a column of links was as wide as their addresses, and a table of pictures wrapped.
- A `<picture>` inside a link shows its picture, for the color scheme of the frame. Before, the link showed only its brackets.
- `shipit-reload` no longer warns "loaddefs-gen: load error for shipit-preview".
- When GitHub refuses `M-m` at once, shipit gives the reason, as in "API error: HTTP 400: Pull request head branch was modified.". Before, it said only "API error: HTTP 400".

### Changed

- In the stacked preview, `c` is now `b c`, `r` is now `b m`, `a` is now `b a`, and `b` is now `r o`. `u` is gone: `s` on the end of a layer joins it with the layer above, and `C-/` takes back the last `s`.

### Removed

- The user option `shipit-dwim-hook`. No code called it, so a configuration that sets it works as before.

---

## v1.7.0 (2026-10-01)

shipit now requires Emacs 29.1. The code already used functions of Emacs 29.1, and Emacs 29 is the oldest version that the tests run on.

### Features

#### Stacked pull requests

- `shipit-stacked-preview`, `S` in the preview of a pull request, opens the commits of a branch and divides them into layers, one pull request for each layer. `s` puts a division at the commit at point, `u` takes one away, and `c` puts a branch on each layer. Nothing is rebased, so you can divide again at no cost.
- The preview knows the stack that its branch sits in. It says what the branch stacks on, drafts a stack, and shows how far each layer has got, with its checks and its review decision.
- A stack goes to the remote in one atomic push, so a refused push leaves the remote as it was. `shipit-push-max-refs` sets how many branches one push updates. Before a push, shipit works out what the push will do.
- A pull request shows the stack that it belongs to. Each row of the stack opens, names and expands its siblings, and `M-w` copies the address of the stack or of a sibling.
- shipit refuses to submit branches that are not a stack, and it registers a stack of pull requests that was made elsewhere.

#### Review

- A review session for one file. When the pull request has a worktree, `RET` on a file under Files Changed opens the file with a legend of its keys. `n` and `p` step through the hunks, and `M-n` and `M-p` step through the commits. `|` shows the previous version beside the file, and `s` picks which of the two sides `M-n` and `M-p` walk. `e` runs ediff on the two sides, and `t` opens a timeline of the commits that touched the file.
- Review mode no longer binds `C-c r c`, because Emacs keeps `C-c` and a letter for your own keys. The compare menu is `c` in the review menu, which `r` opens from the legend.
- A review queue keeps inline comments as drafts under their diff lines and submits them as one review. The queue is kept on disk for each pull request, so a refresh or a restart does not lose it.
- Inline comments show only where they apply. Resolved threads are marked in every view, and inline comments have a rounded background.
- Files Changed can hide files, such as a lock file or generated code. You hide the file at point, or every file whose name contains some text. The name filter now matches its text as typed, not as a regexp.

#### Pull requests

- The Activity section shows a force push by its commits, without the changes that a rebase brought in. It also shows each edit of the description as a word diff.
- `M-;` on the title or the state line reopens a closed pull request and toggles its draft state.
- The reviewers menu can request a Copilot code review.
- The collapsed Checks heading shows the status of all checks. The Activity section has one sort order and a menu on its heading.
- `o` opens the pull request in the browser.
- Commit comments on a branch without a pull request show in the style of a pull request.
- A commit SHA in text has a menu. When the forge cannot give the files of a commit, shipit reads them from the local repository.

#### Issues

- A git-bug backend. shipit lists, searches, reads, creates and comments on the bugs that git-bug keeps in the repository. A repository that keeps bugs this way uses the backend without a setting.
- The issue buffer edits the title and the labels, links work items and pull requests, and shows the priority. An event that changed the description opens to a word diff of the change.
- You can paste a picture, or yank it with `C-y`, into a comment, a description or a new issue.
- The images and videos of Jira issues show in the buffer. An issue lists its attachments, each with a thumbnail.
- A new issue takes defaults for its repository from `:create-defaults`.

#### Jira boards

- The dashboard shows several Jira boards, and an edit mode moves issues across the columns of a board.
- On Emacs 32, a board draws as cards on a canvas. `M-<right>` and `M-<left>` stage a move, and `C-c C-c` sends the staged moves. `M-p` and `M-n` step the board back and forth in time. A card shows its priority and its type.

#### Notifications

- Mute rules for pull requests that are not yours: for every repository, for one repository or for one thread. Notifications that your own actions caused are muted without a rule.
- Auto-mark rules can match labels, file paths and replies to you. Mark-by-regex also matches the name of the repository.
- `RET` on a check suite notification opens the Actions list.

#### Actions

- On Emacs 32, `v` on a run draws its jobs on a time axis, so you see which jobs ran side by side.
- `o` opens the item at point. An Actions buffer remembers the pull request that it came from.

#### Worktrees

- A worktree is created in the background, and LFS files are pulled only where they are needed.
- A new command moves the work of a checkout, with its uncommitted changes, to a worktree. `Z` in a pull request buffer opens the worktree actions.
- The worktree section of magit shows the state of the pull request of each worktree.

#### Other

- `H` in the main menu opens the landing page of a repository, with its data and its README. The prompt offers the repository of each git remote of the checkout, with that of `origin` as the default.
- The page of a repository has a Type line. The line says that the repository is private or internal, a fork, or archived. On a repository that you administer, the line also says that it is public.
- `$` opens a buffer of the git commands that shipit runs, with their output, as magit does.
- Other packages can build on shipit. A handler of `M-;` can go before the handlers of shipit, and `shipit-reload` can reload the files of such a package.
- The README names the optional canvas packages that the canvas views need.

### Performance

- Opening a pull request no longer waits for reactions and repository lookups.

### Bug fixes

- Without marginalia, the list of pull requests in the minibuffer failed for a pull request without labels.
- When the draft flag is set, the pull request is created as a draft.
- The preview finds an existing pull request by its branch, and it decides that a push worked by the exit code. It keeps a description that you edited since it last saved, and it shows the changes from where the branch left its base.
- Jira: the activity names the person who made the latest change. Empty board columns resolve, and every assignable user is listed. A comment can be edited from its issue page, and pasted images and card links show.
- Requests through url.el are guarded against multibyte text and against a token in a message. Async requests keep the time locale that they need.
- Rendering: strikethrough shows in comments. When a comment collapses, the next comment keeps its indentation. When the font size changes, icons keep their size. Each `+` and `-` marker of a diff keeps its colour under the highlight.
- Review mode leaves the buffer of the caller alone. When the worktree lacks the base commit, review mode works without it. Review mode is also on for a worktree that is out of sync.
- The assignee picker no longer fails for a user with a display name.
- A reload no longer fails on a shipit function in `kill-buffer-hook`.
- A test run no longer writes to your shipit state.

---

## v1.6.0 (2026-05-01)

### Features

- **Undo window for all mark-as-read commands.** Single (`m m`), region, all-in-buffer (`M`), actionable (`m A`), non-actionable (`m N`), resolved (`m Z`), and the new mark-by-regex (`m r`) all defer the actual server PATCH for `shipit-notifications-mark-undo-window-seconds` (default 30s).  Affected rows hide immediately; any undo key (vanilla, undo-tree, undo-fu, undo-tree-visualize) cancels the batch via a custom `(apply ...)' entry on `buffer-undo-list'.  The eventual commit fires the PATCHes asynchronously and concurrently — a 50-row mark used to block the editor for 50× RTT.
- **`m r` mark-by-regex with live preview.** Reads a regex from the minibuffer with a strike-through preview of matching rows that updates as you type, plus a live `(N matches)' suffix on the prompt.  RET marks the matched rows (with the same undo window).
- **Snooze transient toggles the snoozed-section visibility** (`z t`).  The header bracket `[snoozed: N]` always shows; this just hides the inline list of snoozed rows.
- **`w` works on group headings and entries.** On a `notification-repo` group heading the watch transient opens for the repo; on an entry it additionally surfaces the per-thread subscription group so individual PRs / issues / discussions can be subscribed / unsubscribed / ignored without leaving the notifications buffer.
- **Live match count + pending-mark count in the header.** The mark-by-regex minibuffer shows `(N matches)`; the buffer header carries `[pending mark: N]` whenever a batch is in flight (clears on commit / undo).
- **`<- current` marker in the subscription transient gets a color** (the `success` face) so the active subscription state stands out.
- **Release notifications open in the in-Emacs releases buffer.** RET / `o` on a release notification opens `shipit-open-releases-buffer` for the repo + tag instead of bouncing to a browser.  Markdown links in the release body are clickable; URL clicks route through `shipit--classify-url` so blob / PR / issue URLs land in their shipit buffer.
- **Markdown / org blob viewer with collapsible sections.** Blob URLs to prose files (`.md`, `.markdown`, `.mdown`, `.org`, plus extension-less `CHANGELOG` / `CHANGES` / `HISTORY` / `NEWS` / `README`, plus files with a `-*- mode: org -*-` first line) render as a magit-section-mode buffer.  Each `# heading` / `* heading` becomes a navigable, collapsible section.  `#NNN` issue/PR references and bare URLs are clickable; commit SHAs render in `magit-hash` face and `RET` fetches the commit on demand into `shipit-commit-cache-directory` (default `~/.emacs.d/shipit-commit-cache/`) via `git fetch --depth=1` and opens it in a magit-revision buffer — no local clone of the source repo required.  Inline `~code~` / `*bold*` keeps its rendered styling.

### Performance

- **Cut GC + render time on large notifications inboxes** (~1000 unread).  Four changes against the 30s profile:
  - Async-callback rerenders are debounced (`shipit-notifications-rerender-debounce-seconds`, default 0.3) so a poll cycle's worth of GraphQL enrichments coalesce into one render.
  - The etag-cache hot path uses native `json-parse-buffer` (Emacs 27+) instead of the elisp `json-read` — ~5× faster + a fraction of the conses.
  - Per-row defcustom lookups + `window-body-width` are hoisted into a render-cache plist bound for one render's dynamic extent.
  - `shipit-notifications-unread-max-pages` defcustom (default 30, was hardcoded 10) raises the visible cap from 500 to 1500 items.
- **Fast README hard-wrap.** `shipit--wrap-text` was a ~25% slice of repo-buffer redisplay profiles -- `fill-region` per input line, with paragraph-detection / `adaptive-fill-mode` / change-hooks all kicking in.  Switched to `fill-region-as-paragraph` inside a context that disables those code paths; the wrap drops out of the redisplay hot frame.  New defcustom `shipit-readme-fill-column` (default 80) controls the wrap column; nil opts into pure visual wrap.

### Bug fixes

- **Page nav in `all` scope is actually windowed.** `M-n` no longer accumulates: page-forward / back / first / last / goto-page in `all` scope clear the GitHub entries from the hash before fetching the new page so the buffer holds one page at a time, not N stacked.
- **Auto-mark live preview walks group-by-repo trees.** `m r` (and the `m +` rule preview) now strike-through matching rows in group-by-repo mode, not just flat layout.
- **Icon-leak fix runs on every hide.** Section icons no longer leak through invisibility overlays when collapsing via the magit level digits (`1` / `2` / ...) — the `:after` advice on `magit-section-hide` covers all hide paths, not only TAB.
- **Activity row width cap is configurable** (`shipit-notifications-activity-line-width`).  Default `nil` tracks `window-body-width` so activity timestamps line up with the notification list above; an integer caps the target on wide windows.
- **Buffer state survives rerenders.** Collapsed groups stay collapsed across an unrelated rerender (snooze, mark, poll); active region is deactivated before regeneration so the next redisplay doesn't crash with `(wrong-type-argument ... nil)` from a stale region pointing at sections that just got wiped.
- **Async batch mark-as-read is resilient.** 404 / 410 from already-gone threads are tolerated; backend activities (Jira / RSS / GitLab) dispatch through their own backend's `:mark-notification-read` instead of being misdirected at the GitHub PR endpoint; the async error callback no longer errors when the HTTP status line is unparseable.

---

## v1.5.2 (2026-04-29)

### Bug fixes

- **Deny-list filters were invisible in the header.** Toggling a value into a deny list with `C-u` narrowed the buffer silently — no `[!repo: ...]` bracket. Each selector now renders a sibling `!`-prefixed bracket whenever its deny list is non-empty.
- **Multi-value filter brackets rendered as `(a b)`** (Lisp list notation from `%s`) instead of comma-joined values. Now read as `a, b`.
- **Snoozed bracket disagreed with the snoozed group.** The `[snoozed: 1]` count walked the snooze list and the activity hash; the rendered group additionally hid rows the user had locally marked read. Bracket and group now apply the same exclusion, so they agree.
- **`shown / total` was misleading when client filters narrowed the view.** The fraction now switches to `shown / loaded (of total)` whenever a structural filter or text filter reduces what's visible, so the visible numerator and denominator match the buffer state with the server total kept as context.

### Polish

- Filter-bracket entries are suffixed with the matching activity count, so a deny like `[!repo: acme/acme-project (6)]` makes it obvious how much the filter is hiding.

---

## v1.5.1 (2026-04-28)

### Bug fixes

- **Switching from `all` back to `unread` scope kept showing read items.** The buffer rendered everything left in the activity hash from the previous `all` poll, even though those items were no longer unread. The render pipeline now filters by `notification.unread` while in `unread` scope; `all` scope is unaffected.
- **Modeline bell disagreed with the buffer header.** The background poll's inline counter and the buffer's manual-refresh counter were two different functions that disagreed on whether to subtract snoozed and locally-marked-read items. Both paths now route through a single canonical counter so the bell stays in sync with the header.

---

## v1.5.0 (2026-04-28)

### Features

- **Notifications buffer overhaul.** Major rework of the unread queue:
  - **Filter transient (`f`)** with scope toggle, pagination, and exact server totals.
  - **Selectors vs filters.** Selectors decide what to fetch (repo, scope). Filters narrow on the client (type, state, reason, component). Each selector has an allow list and a deny list. `C-u` on a toggle flips a value into deny.
  - **Auto-mark rules engine.** `m a` adds a rule prefilled from the row at point, with a live strike-through preview of which rows the rule would mark. Rules support `:not`, AND-compose, and edit-in-place. They apply on add — no separate `u` step.
  - **`m` transient** for mark actions. Bulk shortcuts mark only actionable or only non-actionable rows. `m M` shows a strike-through preview before committing. `m M` on the snoozed-group heading scopes to that group.
  - **Snooze on rows.** `z` snoozes (prefix sets hours, fractions allowed). `Z` lists snoozes and unsnoozes the one at point. Permanent variant via the `z` transient's `p`. Snoozed rows live in a collapsible group with a time-left column. Counts show in the header bracket and the group footer. The modeline bell ignores snoozes. Snoozes persist across Emacs restarts and auto-clean when the item is resolved.
  - **Jira component is a first-class dimension.** Filter selector value and auto-mark rule condition. Auto-mark rules apply to Jira activities too.
  - **Expanded Jira body** shows Author, components, and the most recent comment or status change.
  - **Async backends + icons** for new notification types (workflow runs, discussions). `RET` on a workflow run opens its actions buffer.
  - **Modeline bell** counts unread, not total, and stays visible in all-scope.

- **Difftastic via `d` transient.** `d` on a file opens a diff-mode transient: plain, with comments, or difftastic. Difftastic only shows when the package is installed. Missing PR SHAs are fetched on confirmation.

- **READMEs render as outlined documents.** Markdown and org headings nest as collapsible magit sections. Inline images cap at the wrap-column width and honor `<img width>`. SVG badges, animated GIFs, and `/blob/` images render correctly. Mp4 video links become clickable buttons. The same heading-as-section rendering also applies to issue descriptions.

- **Branch creation from an issue.** `b` opens a transient that creates a git branch named from the issue (project key + slugged title) in the configured repo dir, off a configurable base ref. The working tree is left alone.

- **Eldoc preview on issue references.** Hovering on `PRJ-1234` shows the issue title in the echo area.

- **Show Jira components on the issue buffer.**

- **Deep-link to a workflow run.** `M-x shipit-open-actions-run` jumps directly to a run.

### Performance

- **SVG icon cache** survives rerenders.
- **Re-entry guard** on the rerender hook stops a spinning loop on large buffers.
- **Live filter** debounces input and dedupes hash walks.

### Bug fixes

- **Markdown file hangs** in the PR diff (regression from v1.4.0) fixed.
- **Linked PRs** now resolve under Jira-backed issues.
- **Assignee label** aligned with other metadata rows.
- **Worktree probe** returns `nil` when run outside any clone.
- **Nav to a comment** loads all unread pages and refreshes the issue buffer if the comment is still not visible.
- **Repo-buffer About line** wraps at 80 columns.
- **Pagination** refuses to advance past the last page in all-scope. The buffer always uses 50 per page (GitHub's cap).
- **Activity-nav** unblocks when opening a PR from notifications.
- **`z` replaces a snooze** instead of toggling it off.
- **GitHub edge-cache flicker.** Items that vanish for one poll and reappear on the next no longer flicker out.
- **Snooze list** survives buffer kill and reopen.
- **Snooze group** starts expanded so `n` enters it, with a reinforced invisibility overlay so the group icon doesn't leak through on collapse.
- **Interactive `g`** bypasses the ETag cache, so a manual refresh actually round-trips.

---

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
- **Linked tracker issue on PR buffer** — new "Issue" section on each PR auto-detects the key from the branch name (e.g. `PRJ-12624-foo`) with a transient for link/clear/open/browse/copy-URL/transition/create-and-link. Manual overrides persist per (repo, pr-number) in `~/.emacs.d/shipit-pr-linked-issues.el`.
- **Activity navigation from notifications** — pressing RET on an activity line in an expanded PR notification opens the PR and jumps directly to the comment/commit/review; a pulse highlights the landing line.
- **Jira comment reactions** — issue comments render reaction rows via the internal DC/Server endpoint (Cloud stays placeholder-only, tracked by JRACLOUD-78153).
- **Shebang language detection** — code blocks without a fence language tag get syntax highlighting inferred from `#!/usr/bin/env python` style shebang lines.

### Performance

- Atlassian dashboard now fetches the four sections concurrently via the Jira async API. Render happens once at completion instead of four times (each render regenerates every SVG icon — ~5× faster end-to-end on large projects).
- Memoised Jira issue-type / priority icons; rendering ~450 rows now calls svg-lib ~10 times instead of ~450. Dashboards with many issues load in ~1 s rather than ~15 s.
- Board pagination bumped to `maxResults=100`, halving round trips.

### Fixes

- **Empty Issues section** — JQL now quotes project keys (`project in ("PRJ")` instead of `project in (PRJ)`); Jira's `/search/jql` endpoint rejects the unquoted form with HTTP 400.
- **Duplicated comments** — head-and-tail pagination fallback no longer renders the same comment twice when the total fits in the window.
- **Thread subscription on Jira issues** — `w` on a Jira issue buffer (string key like `PRJ-12624`) no longer raises a wrong-type-argument error; thread subscription is GitHub-only and now elides cleanly for string IDs.
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
