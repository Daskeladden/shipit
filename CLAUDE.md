## AI Guidance

- Always write short functions, split into multiple smaller functions when implementing a larger feature.
- Always follow the DRY principle
- Always do TDD, add tests first before implementing new features.
- Always run the tests after edits.
- **CRITICAL**: Always run `make test` immediately after making any code changes to catch parsing errors, compilation warnings, and runtime issues before stopping work.
- Never add stubs to production code.
- Always apply "offensive programming" to catch errors early.

## Architecture and File Structure

The codebase follows a layered architecture:

- **Entry point**: `lisp/shipit.el` — autoloads and `shipit-enable`
- **Core**: `lisp/shipit-core.el` — defgroup/defcustoms, shared helpers
- **HTTP**: `lisp/shipit-http.el` — API plumbing, GraphQL, ETag caching
- **Backends**: `lisp/shipit-pr-backends.el`, `shipit-issue-backends.el`, etc. — registry + dispatch
- **UI**: `lisp/shipit-buffer.el`, `shipit-pr-sections.el`, `shipit-notifications-buffer.el`, etc.
- **Rendering**: `lisp/shipit-render.el` — markdown, SVG icons, avatars

### Dependency Layering — UI Files Must Not Require Each Other

```
    shipit-core.el
         |
    shipit-*-backends.el   (registry + shared utilities)
         |            |
    ui-buffer-a.el    ui-buffer-b.el   (UI -- never cross-require)
```

Place shared functions at the lowest common ancestor in the
dependency tree.  If two UI files need the same function, extract
it to the backends or core layer.

## Co-authorship

- Add co-author trailer to all commits:
  `Co-Authored-By: Claude <noreply@anthropic.com>`

## Debugging

### Debug Logging System

Use `shipit--debug-log` instead of `message` for debug output.
Writes to `~/.emacs.d/shipit-debug.log` with timestamps and
automatic log rotation.

```emacs-lisp
(shipit--debug-log "Debug: %s" value)
```

### Debugging Approach — Use Backtraces, Not Defensive Programming

When encountering errors, add ONE strategic `condition-case` with
backtrace logging.  Let the error happen and capture the full call
stack.  Never add layers of parameter validation or incremental
defensive checks — that hides bugs instead of revealing them.

### Debugging Syntax Errors

The formatter's indentation is ALWAYS correct.  If code is not
indented correctly, it means there is a syntax issue (unbalanced
parens/quotes), not a formatter bug.

## Testing

### Integration Tests Over Unit Tests

Prefer integration tests that exercise full code paths.  Mock only
at the boundary (HTTP responses, filesystem, external processes).
Never mock internal functions.

### Configuration Variant Testing

When a feature has multiple modes, every integration test must run
under each variant (e.g., GitHub vs GitLab, emoji vs SVG icons,
sync vs async).

### No Stubs in Production Code

Never add stub functions.  Implement actual functionality or fail
fast with clear errors.

## Code Quality

- Run `make test` after changes
- Run `make compile` to check for byte-compilation warnings
- Follow Emacs Lisp conventions: proper prefixes, docstrings, lexical binding

## Committing Code

- Never commit changes unless the user explicitly asks you to.
- Never restore files from git unless the user explicitly asks you to.

## Backend Dispatch — No Inline Conditionals

Always dispatch through the backend registry.  Never write inline
`cond`/`if` branches that check backend type.

```emacs-lisp
;; WRONG
(cond
 ((eq source 'jira) (jira-fn ...))
 ((eq source 'gitlab) (gitlab-fn ...)))

;; CORRECT
(let* ((resolved (shipit-issue--resolve-for-repo repo))
       (fn (plist-get (car resolved) :operation)))
  (when fn (funcall fn (cdr resolved) args)))
```

### File Placement — Backend Code Stays in Backend Files

Backend-specific defcustoms, defvars, and functions belong in
their backend file, not in common/shared files.

## Common Patterns

### Section Location — Use Text Properties, Not Pattern Matching

Never use regex to find magit sections.  Section headings can have
emoji OR SVG icons.  Always use text properties:

```emacs-lisp
;; WRONG
(re-search-forward "Files Changed" nil t)

;; CORRECT
(text-property-any (point-min) (point-max) 'shipit-pr-files t)
```

### magit-insert-section-body Scope Boundary

Variables defined in `let*` inside `magit-insert-section` are NOT
visible inside `magit-insert-section-body` — the macro creates a
lambda that captures only function parameters and outer bindings.
Use the function parameter directly, not a local variable.

### Indentation

Handled automatically by the PostToolUse formatter hook which runs
`indent-region` on every edited file.  Never manually adjust
indentation — if it looks wrong, the code has a syntax error.
