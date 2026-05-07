## Why

Dogfooding the slide-deck model produced a recurring failure mode:
agents push many small narrative slides interspersed with whole-file
slides, and the result reads like fragments rather than an explanation.
The user has no anchored prose to walk; the agent has no way to express
information architecture beyond linear progression.

A second pressure: channel notifications (the back-signal that made
agent-paced decks workable) are not yet supported in Claude Code.
Without back-signalling, the natural model is "agent prepares an
artefact, user reads at their own pace, chat happens through the IDE
side-channel."

The right unit is a *blog-post-shaped document* on disk: prose with
embedded code fences, links to source, and per-link annotated source
slides for the cases where seeing real source matters.  Existing
infrastructure (frame discovery, tmux geometry, file-slide rendering,
annotation overlays, deck navigation) already supports this — the
change is mostly editorial plus a small extension to narrative slides
and one convenience tool.

## What Changes

- **`narrative` slide gains `:path`.**  In addition to today's inline
  `:markdown`, a narrative slide MAY carry `:path` (relative to the
  worktree, or absolute).  When `:path` is given, the slide's buffer
  is `find-file-noselect`'d on the path so that the doc is a real file
  with a real project root — IDE selections and chat route through
  the project's claude-code-ide session unchanged.  When neither
  `:path` nor `:markdown` is given, validation fails.  When both are
  given, validation fails (one or the other).
- **`present_document` MCP tool.**  Convenience that wraps three
  steps the agent would otherwise stitch by hand:
  1. Write `markdown` to
     `<worktree>/.claude/presentations/<ISO-date>T<HH-MM>-<slug>.md`.
  2. `start_presentation` against that path with the document as
     `initial_slide` of kind `narrative`.
  3. `push_slide` each entry in an optional `file_slides` array, in
     order, after slide 0.
  Returns `{ key, path, slide_count }`.  Encodes the new norm as a
  single tool call.
- **Markdown link click-handler.**  Inside `narrative` buffers (only),
  `+presentation-mode` intercepts link follows.  Two URL schemes
  dispatch to the deck:
  - `slide:N` → `goto_slide(N)`.  Explicit, unambiguous.
  - `path#L<start>` or `path#L<start>-L<end>` → searched against the
    deck for an *exact* match (path equality, line-range equality).
    On hit: `goto_slide(<that-index>)`.  On miss: fall back to
    `find-file` + `goto-line` with no overlays.
  Other URL schemes (http, mailto, plain paths without anchors) are
  not intercepted.  File-kind buffers do NOT carry the click-handler
  — agents shouldn't be embedding markdown links in source views.
- **Spec & rule guidance.**  The presentation rule under
  `~/.claude/rules/` (and the spec) is rewritten to encode the new
  pattern: write one fat doc, push annotated file slides for sources
  the doc references, *don't* push many narrative slides.  Decks of
  more than one narrative slide are explicitly discouraged.

Out of scope:

- New slide kinds.
- Removing or deprecating `:markdown` (synthetic narrative buffers
  remain useful for ephemeral / test cases).
- Changes to `file` / `diff` / `layout` slide kinds, deck mutation
  tools, or annotation rendering.
- Channel notifications (already deferred upstream).

## Capabilities

### New Capabilities

<!-- none — extends the existing presentation capability -->

### Modified Capabilities

- `presentation`: narrative slides accept `:path`; new
  `present_document` MCP tool wraps the doc-first flow; narrative
  buffers gain a click-handler that dispatches `slide:N` and
  `path#Lx-Ly` links to `goto_slide` when the deck contains an exact
  match.

## Impact

- Modified files:
  - `modules/presentation/lib.el` — narrative validator accepts
    `:path` (and rejects both-or-neither); `+presentation--render-
    narrative` switches between `find-file-noselect` and the
    synthetic buffer; new link-click handler scoped to narrative
    buffers; deck-search helper for `path#Lx-Ly` lookups.
  - `modules/presentation/init.el` — coerces `path` from snake_case
    on the narrative branch; registers `present_document` MCP tool;
    documents the doc-first norm in tool descriptions.
  - `modules/presentation/spec.md` — narrative requirement extended;
    new requirement for `present_document`; new requirement for the
    click-handler dispatch; validation requirement updated.
  - `modules/presentation/tests.el` — tests for `:path` rendering,
    validator rejection cases, `present_document` plan/effect
    assertions, and click-handler dispatch logic.
  - `~/.claude/rules/...presentation.md` (Nix-managed) —
    rewrite to recommend doc-first usage.  Out-of-tree from
    openspec but called out here so the cut is complete.
- No breaking changes: existing `:markdown` narrative slides keep
  rendering; existing decks keep working; `goto_slide` /
  `push_slide` / etc. unchanged.
- No new external packages.
- File location convention `<project>/.claude/presentations/` is
  already gitignored by the repo's `.gitignore` (`.claude/*` with
  selective allow-listing).
