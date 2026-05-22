## Context

`gfm-pretty` already ships three decorators (`callouts`, `fences`, `tables`) that plug into a shared engine via `gfm-pretty-define-decorator`. Each decorator provides `:collect-fn`, `:apply-block-fn`, `:full-rebuild-required-p`, and lifecycle hooks; the engine orchestrates per-window rebuild, reveal, and overlay registry teardown.

Plain blockquotes (`> ` lines without a `[!TYPE]` marker) fall through `markdown-mode`'s default fontification: the markup char `>` is left visible, the line gets `markdown-blockquote-face`, and the `wrap-prefix` text property is set to `"> "` so soft-wrapped visual continuation rows show the `>` again. The visual effect is busy compared with modern clients (Gmail, Slack, Discord) which collapse the `>` into a thin vertical rail.

The callouts decorator already solves the rail problem for typed blockquotes via:

- a per-line anchor overlay (face + `wrap-prefix "│ "`) — see `lisp/gfm/gfm-pretty-callouts.el:289-315`,
- a per-window display overlay over the leading `> ` (or bare `>`) substituting `│ ` (or `│`) — see `lisp/gfm/gfm-pretty-callouts.el:407-431`.

Plain blockquotes can use the same shape, minus the borders and width math.

Confirmed source of the wrapped `>`: `markdown-mode` sets `wrap-prefix "> "` as a text property on blockquote lines. Verified by inspecting the live buffer:

```
:wrap-prefix "> "  :face (markdown-markup-face markdown-blockquote-face)
```

Overlays' `wrap-prefix` wins over text-property `wrap-prefix` for redisplay because `get-char-property` consults overlays before text properties.

## Goals / Non-Goals

**Goals:**

- Render plain GFM blockquotes as a continuous left rail (`│`) including soft-wrapped continuation rows.
- Reuse the engine seam — no new engine concepts.
- Mirror the callouts reveal contract: per-window source reveal when point or region overlaps the block.
- Partition cleanly with the callouts decorator: any block headed by `> [!TYPE]` belongs to callouts; everything else `^>` belongs to blockquotes.

**Non-Goals:**

- Nested blockquotes (`>>`, `>>>`). Treated as plain blockquotes (rail painted over the outer `>` only) or ignored entirely; either is acceptable — the user has not encountered them in their corpus.
- Continuity across blank lines. Two `>`-prefixed runs separated by a blank line render as two distinct rails (matches source semantics).
- Right-edge rail, tinted background, border decoration. None of those — blockquotes get a single hairline rail only.
- Width math, window-fitted layout. Blockquotes do not depend on window width.

## Decisions

### Decision: New decorator registered with the existing engine

Register a `blockquotes` decorator via `gfm-pretty-define-decorator`, structurally parallel to `callouts`. Provides:

- `:registry` — fresh registry via `gfm-pretty--registry-for`.
- `:collect-fn` — scan `^>`-prefixed runs, subtract callout block ranges.
- `:apply-block-fn` — anchor overlay (face + `wrap-prefix`), per-window display overlay for the prefix swap.
- `:full-rebuild-required-p` — dirty region overlaps the first `>` of any blockquote line, or a line directly above/below a blockquote block.
- `:on-enable-fn` / `:on-disable-fn` — no-op (engine handles overlay teardown).

**Alternatives considered:**

- *Extend the callouts decorator to handle plain blockquotes too.* Rejected: callouts already has bordered-block, width math, type-faces, reveal, and font-lock keywords tied to type markers. Mixing in an unrelated render path would couple two contracts.
- *Pure font-lock + `wrap-prefix` text property override.* Rejected: text-property `wrap-prefix` set by markdown-mode wins over font-lock-applied text properties layered later (both are text properties at the same priority); only an overlay reliably overrides it. Also defeats per-window reveal.

### Decision: Plain-blockquote discovery subtracts callout ranges

Walk the buffer line by line. A blockquote run is a maximal sequence of consecutive lines whose first character is `>`. After collection, drop any run that contains a `> [!TYPE]` marker line — that run belongs to the callouts decorator.

A run containing a `[!TYPE]` marker mid-block (atypical) is dropped wholesale; the callouts decorator owns the marker line and its continuation lines anyway, so leaving the surrounding non-callout `>` lines undecorated is acceptable for v1.

**Alternatives considered:**

- *Subtract callout ranges by per-line subtraction (so a marker mid-run splits the run into two blockquote pieces).* Deferred — adds discovery complexity for a case nobody has reported.

### Decision: Anchor + display overlay split mirrors callouts

Per source line of a blockquote block:

1. **Anchor overlay** over `[bol, eol]`:
   - `face` — optional; leave nil for v1 (no tinted background). The display string carries the rail glyph face directly.
   - `wrap-prefix` — propertised `"│ "` in `gfm-pretty-blockquotes-rail-face`.
2. **Display overlay** (per-window, registry-tagged `kind 'rail-prefix`) over the prefix chars:
   - `[bol, bol+2]` for a `> ` line — `display "│ "`.
   - `[bol, bol+1]` for a bare `>` continuation line — `display "│"`.
   - Carries `gfm-pretty-blockquotes-revealable t` so the engine reveal walker can swap to the bare variant per window.

Per-window display is required because reveal is per-window (matches callouts).

### Decision: Use a single rail face inheriting `shadow`

Define `gfm-pretty-blockquotes-rail-face` with `'((t :inherit shadow))`. The rail glyph and `wrap-prefix` both use it. No theme-driven recomputation needed — `shadow` already tracks theme.

**Alternatives considered:**

- *Reuse `gfm-pretty-callouts-box-face`.* Rejected: that face exists as a fallback inside callouts. A dedicated face makes the rail customisable without affecting callouts.
- *Tinted background panel like callouts.* Rejected per scope (Q3 in exploration).

### Decision: Reveal is per-window, scoped to selection

The display overlay carries `gfm-pretty-blockquotes-revealable t`. The engine's existing reveal walker — driven by `gfm-pretty--range-selected-p` — toggles between masked (`display "│ "`) and bare (`nil` display, source `> ` visible) variants per window.

The anchor's `wrap-prefix` stays as `│ ` even when revealed in a window. Rationale: `wrap-prefix` is a visual aid for soft-wrap continuation rows, not source content; keeping it consistent avoids disorienting jump-shifts when revealing. Acceptable v1 deviation from "show exact source"; revisit if it confuses.

### Decision: Hook from `lang-markdown` module

`modules/lang-markdown/init.el` already requires and installs callouts font-lock via `gfm-pretty-callouts-install-font-lock` (no font-lock for blockquotes — purely overlay-driven, so just `(require 'gfm-pretty-blockquotes)`).

## Risks / Trade-offs

- **`wrap-prefix` override race.** If `markdown-mode` refontifies a line and resets `wrap-prefix` after our anchor lays down, the rail vanishes on continuation rows.
  → Mitigation: overlays' `wrap-prefix` is consulted via `get-char-property`, which prefers overlays over text properties regardless of when the text property was set. Verify with a test that wraps a long blockquote line in an 80-col window and asserts the visual prefix on visual row 2.

- **Decorator-partition drift on edit.** A user edits a plain `> Pain:` line into `> [!IMPORTANT]`. The blockquotes decorator must release the line and the callouts decorator must claim it in the same rebuild.
  → Mitigation: both `:full-rebuild-required-p` predicates already fire on marker-line edits (callouts has structural-line check; blockquotes will treat any `> ` → `> [!` transition the same way via adjacency / structural-line overlap). The engine sequences decorator rebuilds; if both partitions are deterministic functions of buffer contents, they converge.

- **Empty `>` continuation lines.** A bare `>` with nothing after it is part of a blockquote block. The 1-char display swap must produce `│` (one cell) so the rail stays continuous; an accidental `│ ` (two cells) would make the row "wider" than the source and confuse cursor motion at line end.
  → Mitigation: dispatch on `(- lend lbeg)` exactly as callouts already does at `lisp/gfm/gfm-pretty-callouts.el:411-431`.

- **Narrowing under `gfm-present-mode`.** Discovery and teardown must widen, matching the callouts narrowing-resilience contract.
  → Mitigation: reuse `save-restriction (widen ...)` envelopes from callouts `:collect-fn` and `:apply-block-fn`. Add a regression test under the `:narrowing-regression` tag (per `CLAUDE.md`).

- **Markdown-mode `markdown-blockquote-face` italic leak.** Theme may put `:slant italic` on blockquote text. Inside the rail, this is fine (we render the body unchanged), but if a future iteration paints a tinted background we'd hit the same italic-leak problem callouts solved by clearing `markdown-blockquote-face` attributes (`lisp/gfm/gfm-pretty-callouts.el:882-887`).
  → Mitigation: not relevant for v1 (no bg); note for future tinted-panel iteration.
