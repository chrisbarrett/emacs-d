## Why

The doc-first pivot (`pivot-presentation-to-document-mode`, now shipped)
proved that a single markdown document is the right unit for a
walk-through.  Once the document is the artefact, the deck-of-slides
machinery surrounding it is dead weight: nine MCP tools, four slide
kinds, a hash-keyed session registry, a render-state book-keeper, an
effect planner for tmux pane shaping, and a channel-notification
back-signal that doesn't even work on the user's current Claude
Code build.

Replacing that machinery with a single function — `+present-markdown
FILE` — and a buffer-local minor mode collapses the implementation
to the parts that actually carry weight: heading-by-heading
narrowing, link-preview overlays, and click-to-real-source.  Agents
invoke it via `emacsclient -e`; tmux geometry is the caller's
problem.  The deck disappears because the document is the deck and
narrowing is the navigator.

## What Changes

- **BREAKING:** Remove all MCP tool registrations.  The presentation
  capability no longer exposes `start_presentation`,
  `present_document`, `get_presentation`, `end_presentation`,
  `push_slide`, `replace_slide`, `truncate_after`, `goto_slide`, or
  `get_deck`.  The single public entry point becomes the elisp
  function `+present-markdown FILE`, callable via `emacsclient -e
  '(+present-markdown PATH)'`.
- **BREAKING:** Remove slide kinds `file`, `diff`, and `layout`.  All
  presentation content lives in the markdown body.  `narrative` ceases
  to exist as a kind because there is no longer a sum type — there is
  only a markdown buffer.
- **BREAKING:** Remove the `+presentation--sessions` hash and the
  session-key indirection.  All per-presentation state lives buffer-
  local on the document buffer.  Killing the buffer (or quitting via
  `C-c q`) ends the presentation.
- **BREAKING:** Remove channel back-signal entirely
  (`notifications/claude/channel`, the initialize-response capability
  injection, and all advice on `claude-code-ide-mcp--handle-initialize`).
  Not supported on the user's current Claude Code build; not worth
  the complexity.
- **BREAKING:** Remove tmux pane management from elisp code (frame
  discovery via `tmux list-panes`, pane spawning, `pane_layout` reshape,
  saved-layout restore, `kill-pane` teardown).  The caller is
  responsible for placing emacsclient where they want it (e.g. `tmux
  split-window emacsclient -t -e '(+present-markdown PATH)'`).
- **ADD:** Heading-narrowed slide model.  Top-level `#` headings are
  slides.  `+presentation-mode` (buffer-local minor mode) narrows to
  the heading region containing point.  `C-n`/`C-f` advance to the
  next H1, `C-p`/`C-b` retreat, `C-c q` ends the presentation.
- **ADD:** Live link-preview overlays.  Two URL forms inside the
  document render as fenced previews when `+presentation-mode` is on:
  - `<path>#L<start>-L<end>`: head-only fenced block (≤10 lines from
    the file's current contents) with a "+N more" footer when the
    requested range exceeds the cap.  Fence info-string carries the
    label and `path:start-end`.
  - `diff:<base>...<head>` (with optional `#<path>` scope):
    head-only ```diff fence (≤10 lines of `git diff` output) with the
    same "+N more" footer treatment.
  Previews render on slide entry only — no file watchers, no manual
  refresh command.
- **ADD:** Click escapes to real buffer.  `RET` on a `path#L..` preview
  pushes mark, then opens the file via `find-file` (so
  `other-window-prefix` controls split), narrowed to the requested
  range with the existing focus-overlay + read-only restorer
  machinery.  `RET` on a `diff:..` preview pushes mark, then calls
  `magit-diff-range` with the parsed base/head and optional path
  scope.  Both are reachable from the user's standard back-jump
  bindings (e.g. evil's `C-o`).
- **ADD:** Heading-text in-doc links.  Standard markdown
  `[label](#some-heading)` resolves by heading text within the same
  document; in `+presentation-mode`, following the link narrows to the
  enclosing H1 of the matched heading.  Outside the mode, the link
  follows markdown-mode's default handler.
- **ADD:** Robustness against in-place document edits.  When the
  document is reverted (auto-revert on disk change, e.g. agent calls
  `Edit` while the user is presenting), `+presentation-mode`
  re-narrows by the previously-visible H1's slug, falling back to its
  ordinal index when the slug is gone.  Point and `window-start`
  restore via fingerprint substring search inside the new narrowing.

Out of scope:

- File-notify watchers for live preview refresh (decided against;
  refresh-on-narrow is sufficient).
- HR (`---`) as soft slide-break inside an H1 (rejected as a knob).
- A handler-registry for additional link kinds (the v1 set is
  hardcoded in a `pcase`; lift to data later if a third kind appears).
- Any mechanism for the agent to learn where the user is in the
  presentation (channel back-signal stays gone; the agent does not
  need this signal to be useful).

## Capabilities

### New Capabilities

<!-- none — modifies the existing presentation capability -->

### Modified Capabilities

- `presentation`: the capability is rewritten end-to-end.  All
  MCP-tool requirements, slide-kind requirements, session-key
  requirements, and channel-notification requirements are removed.
  New requirements cover the `+present-markdown` entry point, the
  `+presentation-mode` minor mode, heading-narrowed navigation, the
  `path#L` and `diff:` link-preview overlays and their click actions,
  heading-text in-doc link follow, and revert-resilient narrowing.

## Impact

- Modified files (substantial deletions, new additions):
  - `modules/presentation/init.el` — strip all `claude-code-ide-make-
    tool` registrations; remove `+presentation--register-channel-
    capability` wiring; remove the `delete-frame-functions` hook
    (no frame ownership left to track).  Result is a small file that
    autoloads `+present-markdown` and defines the minor mode's
    keymap.
  - `modules/presentation/lib.el` — major rewrite.  Delete: hash
    registry (`--sessions`, `--register-session`, `--get-session`,
    `--make-key`, `--frame-deleted-h`); effect-record runner
    (`--effect-shell`, `--effect-elisp`, `--run-effects`); tmux
    helpers (`--cmd-list-panes`, `--cmd-split-window`,
    `--cmd-kill-pane`, `--parse-list-panes-output`, `--diff-panes`,
    `--cmd-window-layout`, `--cmd-select-layout`, `--pane-layout-
    effects`, `--apply-pane-layout`); session lifecycle
    (`+presentation-start`, `+presentation-end`, `+presentation-info`,
    `+presentation-deck-info`, `+presentation-present-document`,
    `--plan-spawn`, `--plan-reuse`, `--default-socket`, `--find-frame-
    by-tty`, `--find-existing-frame`, `--make-splash-buffer`,
    `--narrative-buffer`, `--document-path`, `--validate-slug`); deck
    operations (`--deck`, `--deck-push`, `--deck-replace`, `--deck-
    truncate`, `--deck-goto`, `--save-current-point`, `--restore-
    saved-point`); slide rendering except `--render-file`
    (`--render-narrative`, `--render-diff`, `--render-layout`,
    `--render-current`, `--dispatch-slide`, `--render-slide`,
    `--cleanup-render-state`, `--render-state-add`, `--apply-
    annotations`, the three annotation renderers, `--margin-side`,
    `--ensure-margin-width`); validation
    (`--validate-slide`, `--validate-annotation`, `--coerce-slide`,
    `--alist-to-plist`, `--alistp`); channel back-signal
    (`--emit-nav-channel`, `--inject-channel-capability`,
    `--register-channel-capability`); the existing link-dispatch
    helpers gain a preview-overlay sibling and the deck-search arm
    is replaced by direct file actions.  Add: `+present-markdown`,
    minor-mode definition, heading-region helpers, narrowing
    navigation commands, link-preview overlay machinery, revert
    hooks, fingerprint-based point restore.
  - `modules/presentation/spec.md` — rewritten to describe the new
    surface.
  - `modules/presentation/tests.el` — major rewrite; current 3099-line
    test file collapses to the kept renderer plus new heading-nav,
    preview-overlay, and revert-restore tests.
  - `openspec/specs/presentation/spec.md` — replaced via the delta
    in `specs/presentation/spec.md` of this change (whole-capability
    rewrite expressed as REMOVED + ADDED requirement blocks).
  - `~/.claude/rules/...presentation.md` (Nix-managed, out of tree) —
    rewrite to describe the new shape: agent writes a markdown file,
    invokes `emacsclient -e '(+present-markdown PATH)'`, edits the
    file in place via the `Edit` tool.  Called out for completeness
    even though it lives outside the repo.
- No external package additions or removals.  `magit` is required at
  click time only; runtime guard via `fboundp` keeps the load order
  cheap.
- Predecessor: `pivot-presentation-to-document-mode` (shipped, with
  smoke tests 3.10 and 5.2 still pending).  This change does not
  block on those smoke tests, but archiving the predecessor cleanly
  before this change merges is recommended.
