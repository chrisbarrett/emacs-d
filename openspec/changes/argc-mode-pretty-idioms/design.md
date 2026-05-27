## Context

`argc-mode` (lisp/argc-mode/argc-mode.el) draws Unicode boxes around
contiguous runs of argc directive lines. Its overlay rendering predates
the `gfm-pretty` family (lisp/gfm/gfm-pretty-borders.el,
lisp/gfm/gfm-pretty-engine.el) and lacks four idioms that landed there:

- Per-window border sizing via `(space :align-to right)`. argc currently
  uses a fixed `max(80, max-line-length + 4)` width with
  `(space :align-to ,align-col)` (lisp/argc-mode/argc-mode.el:266), so
  in a narrower window the box overruns the window edge, and in a
  wider window it stops short.
- `wrap-prefix` on per-line overlays so soft-wrapped continuation
  visual rows continue the left rail. argc currently sets only
  `before-string` (lisp/argc-mode/argc-mode.el:273-275), which only
  paints the first visual row of each buffer line.
- Past-EOL mask: a `(space :align-to right)` tail in the `default`
  face on the right-edge after-string. Sibling overlays with
  `:extend t` (`region`, `hl-line`, `diff-added`) currently bleed past
  the right `│`. The gfm-pretty idiom is documented at
  lisp/gfm/gfm-pretty-borders.el:117-155.
- Normalised border face. Border chars currently carry plain
  `argc-box-face` (lisp/argc-mode/argc-mode.el:226-241), so prose
  font-lock attributes (`:slant italic`, `:underline t`) leak through
  face composition and slant the box edges on GUI frames. The
  gfm-pretty normaliser is at
  lisp/gfm/gfm-pretty-borders.el:52-68.

## Goals / Non-Goals

**Goals:**
- Per-window border width: top, bottom, and right edges flush to each
  displaying window's right edge.
- Soft-wrapped lines retain the left rail on continuation visual rows.
- No past-EOL background bleed from sibling overlays.
- No text-style leak from prose font-lock into border glyphs.

**Non-Goals:**
- Adopting the `gfm-pretty` engine, decorator registry, anchor/display
  split, per-window display overlays, scoped rebuilds, reveal-on-cursor,
  or selection-aware variant swap. argc-mode keeps its existing
  lifecycle (`after-change-functions` + 0.2 s idle-timer debounce +
  full rebuild).
- Aligning the `wrap-prefix` to each directive's content-start column
  (e.g. column after `# @arg name`). Flat `│ ` is good enough; per-
  directive alignment is deferred.
- Renaming or relocating the engine to be neutral and shared with
  argc. Out of scope; revisit when a second mode wants the same
  treatment (e.g. an OpenSpec viewer, a TOML annotator).

## Decisions

### Decision: Inline the four idioms rather than depend on `gfm-pretty-*`

`argc-mode` is a shell-script minor mode; depending on libraries
named `gfm-pretty-*` would be a namespace wart. The four idioms are
small enough to inline (one normalised-face helper, one wrap-prefix
property assignment, one after-string tail append, one `:align-to
right` swap) without taking on the markdown-flavoured dependency.

**Alternatives considered:**

- *Extract a neutral engine* (path B in the exploration): rename
  `gfm-pretty-engine.el` + `gfm-pretty-borders.el` to a neutral
  prefix and have both argc-mode and gfm-pretty register decorators.
  Rejected for this change: rename churn across ~10 files + tests
  for what's currently a one-consumer benefit. Revisit if a third
  consumer appears.
- *`(require 'gfm-pretty-borders)` from argc-mode* (path C):
  cosmetic wart for less code. Rejected: the four primitives we
  need are simple enough to inline cleanly; the gfm-pretty borders
  toolkit also carries selection-variant machinery argc does not
  use, which would be dead weight in the dependency.

### Decision: Per-window sizing via `:align-to right` on a single overlay

Emacs resolves the `right` keyword in a `(space :align-to right)`
display spec per displaying window at redisplay time (Emacs Lisp
Manual: "Pixel Specification for Spaces"). A single buffer-wide
overlay carrying `:align-to right` therefore renders flush to each
window's own right edge — no per-window display overlay needed.

This sidesteps the gfm-pretty engine's per-window display-overlay
machinery while still getting per-window adaptation. The cost: a
horizontal scroll past the window edge would show the border at a
fixed column rather than scrolling with the buffer. Acceptable;
argc directive blocks are read-only-feeling decoration and unlikely
to be horizontally scrolled.

**Alternatives considered:**

- *Per-window display overlays* (gfm-pretty's approach): handles
  buffer split across two windows of different widths. Rejected for
  this change as overkill for argc's use case; would require
  adopting `gfm-pretty--make-display`, `window-configuration-
  change-hook` reconciliation, and per-window cleanup.

### Decision: Flat `│ ` wrap-prefix, not per-directive content alignment

Continuation rows continue the rail at column 2, regardless of
which directive's prose is wrapping. Matches
`gfm-pretty-blockquotes--wrap-prefix-string` (lisp/gfm/gfm-pretty-
blockquotes.el:153-158) which uses the same flat-rail approach.

**Alternatives considered:**

- *Align continuation to the post-directive column* so wrapped
  description text lines up with the original description's start
  column. Rejected: column varies per directive (`@arg name X`,
  `@option --flag X`), would require parsing each line to compute,
  and the visual win is small compared to baseline rail
  continuation. Defer until users complain.

### Decision: Keep existing lifecycle; do not adopt the engine

argc-mode's `after-change-functions` + 0.2 s idle debounce + full
rebuild is correct and adequate for typical argc usage (script
files with handful of directive blocks). The engine's scoped-
rebuild routing, dirty-region tracking, and `:full-rebuild-
required-p` predicate are wins for markdown buffers with many
blocks of significant size; for shell scripts with O(10) tiny
blocks the full rebuild is cheap enough that the engine's
machinery would be pure overhead.

**Alternatives considered:**

- *Adopt engine for scoped rebuilds*. Rejected: no observed
  performance problem; rebuilding all argc overlays on every
  buffer change at 0.2 s idle is currently sub-millisecond on
  realistic files. Revisit if argc-mode is ever applied to a
  generated multi-thousand-line script.

## Risks / Trade-offs

- **Risk:** `:align-to right` produces an unbounded-width box when the
  window is wider than the longest directive line, which on a 200-
  column window will look comically wide.
  → Mitigation: Acceptable — the box is decoration, not data; matches
  fence behaviour in gfm-pretty.
- **Risk:** Top border's function-name label currently right-aligns
  within fixed-width dashes (lisp/argc-mode/argc-mode.el:223-241);
  rewriting to `:align-to (- right N)` for label positioning is more
  fiddly than the current `make-string` approach.
  → Mitigation: Build the top border as `┌` + dashes + `(space
  :align-to (- right LABEL-W 2))` + `<label>` + ` ┐`, mirroring
  `gfm-pretty--top-strings` (lisp/gfm/gfm-pretty-borders.el:70-102).
- **Risk:** `wrap-prefix` only fires under `visual-line-mode` or
  `word-wrap`; in `truncate-lines` mode there are no continuation
  rows so the change is invisible.
  → Mitigation: This is expected behaviour, not a defect — no
  mitigation needed. Document in the spec.
- **Risk:** `(space :align-to right)` tail in `default` face overrides
  any caller-supplied right-edge highlight (e.g. a future selection
  variant). Argc does not paint selection variants today.
  → Mitigation: Accept the limitation; revisit if/when argc grows
  selection-aware variants.
