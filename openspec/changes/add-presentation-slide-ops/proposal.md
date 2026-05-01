## Why

The presentation surface today supports only `start_presentation` /
`get_presentation` / `end_presentation` and a single `narrative` slide
carried in the start payload.  An agent presenting work has nowhere to
push subsequent slides, no way to anchor commentary to real source code,
and no way to backtrack to a prior point when the user asks a question.
That reduces presentations to a one-shot markdown blurb, which is barely
better than chat.

This change fills the slide-ops gap: deck management lets the agent walk
the user through a sequence (and rewind when needed); new slide kinds
turn the surface from "markdown carrier" into "anchored visual context"
— real buffers in real major modes, real diffs, and inline overlay
commentary attached to specific lines.

## What Changes

- Four new MCP tools registered via `claude-code-ide-make-tool`:
  - `push_slide` — append a slide to the deck and advance to it.
    Returns the new slide index.
  - `replace_slide` — replace the slide at index `i` and re-render if
    `i` is the current slide.
  - `truncate_after` — drop slides after index `i` (inclusive of `i+1`),
    so an agent can prune speculative slides before pushing the right
    one.
  - `goto_slide` — re-render the slide at index `i` as the current slide.
- `get_presentation` SHALL be extended to include `slide_count` and
  `current_slide_index` fields.  `get_deck` is added as a separate tool
  returning the full deck (index, kind, optional title) so an agent can
  recover state after compaction.
- Three new slide kinds joining the existing `narrative`:
  - `file` — open `path` (relative to the session worktree by default,
    or absolute) in its natural major-mode, optionally narrowing the
    visible region to `[start_line, end_line]` and highlighting a
    `focus` sub-range.
  - `diff` — render a git diff: working-tree by default, or
    `base..head` when both refs are provided, optionally scoped to
    `path`.  Rendered in `diff-mode`.
  - `layout` — compose two child slides side-by-side or stacked
    inside the presentation frame.  `split` is `"horizontal"` or
    `"vertical"`; `panes` is a 2-element array of slide specs.
- `annotation` is **not** a top-level slide kind.  Instead, every
  buffer-bearing slide kind (`file`, `diff`, and `narrative`) accepts
  an optional `annotations` array of `{ line, text, position }`
  records.  Annotations are rendered as Emacs overlays
  (`before-string`/`after-string`) attached to the slide's buffer.
  Overlays are wiped when the slide is left or the deck is mutated past
  it ("tied to slide" lifetime).
- Display routing: each slide kind has a render function that returns
  the buffer(s) to display in the presentation frame.  The frame's
  window-configuration is set directly by the renderer (bypassing
  `display-buffer`, consistent with the existing v1 protection).
- `await_feedback` and other bidirectional ops are explicitly out of
  scope — deferred to a follow-up change.

## Capabilities

### New Capabilities

<!-- none — all additions extend the existing presentation capability -->

### Modified Capabilities

- `presentation`: adds deck management ops, three new slide kinds
  (`file`, `diff`, `layout`), and per-slide annotation overlays;
  extends `get_presentation` with deck state.

## Impact

- Modified files:
  - `modules/presentation/init.el` — register four new MCP tools
    (`push_slide`, `replace_slide`, `truncate_after`, `goto_slide`,
    plus `get_deck`); extend `get_presentation` payload.
  - `modules/presentation/lib.el` — slide rendering dispatch, deck
    state on the session plist, overlay manager, layout impl.
  - `modules/presentation/tests.el` — unit tests for deck mutation,
    slide rendering per kind, annotation overlay placement, layout
    pane composition.
  - `openspec/specs/presentation/spec.md` (after archive) — picks up
    the deltas in this change's `specs/presentation/spec.md`.
- No new external dependencies.  `diff-mode` and `markdown-mode` are
  already used; `magit` is **not** required.
- No breaking changes to the existing three tools beyond additive
  fields on `get_presentation`.
