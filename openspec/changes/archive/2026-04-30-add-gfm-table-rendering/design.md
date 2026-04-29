## Context

The `lang-markdown` module already runs two overlay-driven decorators when
`markdown-mode` is active: `gfm-code-fences-mode` and `gfm-callouts-mode`.
Both follow the same shape — block discovery via regexes, overlay application
with `evaporate`-able display strings, debounced idle-timer rebuild, and
cursor-driven reveal of source under point. Tables are the obvious next
target.

A short discovery phase confirmed:

- The user's themes (`modus-operandi-tinted` light, `catppuccin` mocha dark)
  expose `+theme-default-background` as the cached default-bg colour;
  `face-background 'default …` returns `unspecified-bg` for non-active
  frames so we must use the cached value.
- `org-column` provides an established stripe convention worth mirroring:
  `#efe9dd` on light, `#313244` on dark.
- A live spike compared three rendering strategies (per-row + per-pipe
  overlays with priority layering, per-cell bg overlays, per-row composed
  display string). All three produce the desired visual; the per-row
  composed display string ("Approach C") wins on architectural clarity
  given the padding requirement.
- Performance benchmarking showed all three strategies are sub-millisecond
  per typical-doc table; cursor motion is unaffected by overlay count;
  the per-row composed-display approach uses ~8× fewer overlays at the
  cost of ~5–15× per-apply CPU. Worst-case (200×8 stress table): 35 ms
  apply, debounced. Acceptable.

## Goals / Non-Goals

**Goals:**

- Render GFM tables visually as bordered, zebra-striped grids with a
  continuous rule line separating header from body
- Make column structure legible regardless of how unevenly the source is
  aligned (agent-generated tables are typically misaligned)
- Match the visual language of `+gfm-code-fences` and `+gfm-callouts`
  (same border face, same idle-rebuild idiom, same reveal-on-cursor pattern)
- Keep the source buffer fully editable: cursor entering a row reveals the
  raw markdown so the user can edit normally
- Bake in performance instrumentation so future regressions are visible

**Non-Goals:**

- Support for cell-level reveal; row-level reveal is sufficient
- Visual-line wrapping inside cells when the table exceeds window width;
  the table overflows horizontally
- Alignment markers (`:--`, `--:`, `:-:`) influencing rendered text
  alignment within cells; v1 left-aligns everything
- Tables nested inside fenced code blocks; those are skipped entirely
- Window-local rendering; overlays are buffer-level (same limitation as
  the sibling modules)
- Direct integration with `markdown-table-mode` editing commands beyond
  what idle-rebuild already handles

## Decisions

### Render strategy: per-row composed display string ("Approach C")

For each row, build a single overlay covering the source line, with `display`
set to a composed string assembled from propertized substrings: outer `│`,
padded cells (alt-bg or default-bg per zebra position), and gap chars
(default-bg).

**Why over alternatives:**

- *Layered overlays with priority trick* (row bg overlay + higher-priority
  gap overlays painting default-bg over `|` regions): works, but the
  padding requirement makes it awkward — we'd need before/after-string
  padding layered on per-cell overlays, multiplying overlays and edge
  cases. Also depends on overlay face-merge precedence holding across
  themes; a small risk we don't need to take.
- *Per-cell bg overlays*: clean for striping but offers no path to
  per-column padding without additional before/after-string scaffolding.

The per-row composed-display approach makes padding native (it's just string
concatenation), removes the layering puzzle entirely (each substring carries
its own `face` text property), and produces ~8× fewer overlays.

### Stripe face mirrors `org-column`

```elisp
(defface gfm-tables-row-alt-face
  '((((background light)) :background "#efe9dd")
    (((background dark))  :background "#313244"))
  "Stripe colour for alternating GFM table body rows."
  :group 'gfm-tables)
```

Light value taken from `modus-operandi-tinted` `org-column`; dark value
from `catppuccin` mocha `org-column` (= `ctp-surface0`). Tunable via
`customize-face`.

### Border face reuses `parenthesis`

Same face used by `gfm-code-fences` for the curved fence border. Keeps
visual chrome consistent across all GFM block decorators.

### Padding strategy

- Trim source cell content (leading/trailing whitespace inside the cell)
- Compute per-column max content width across header + body rows
- Each rendered cell = `" " + content + (max-width - content-width spaces) + " "`
  → minimum 1 space of left/right padding within the cell

### Gap rendering

Between adjacent cells, emit a single space propertized with `face =
(:background <default-bg>)`. Adjacent cells are propertized with the row's
bg (alt or default). The 1-char gap punches through the alt-bg, exposing
default-bg, which makes column boundaries visible on striped rows.

### Outer box decoration

- Top border: `before-string` on the header row's overlay = `┌─…─┐\n`
- Rule line: the delimiter row's overlay `display` = `├─…─┤`
- Bottom border: `after-string` on the last body row's overlay = `\n└─…─┘`

Width = 1 + Σ(rendered-cell-width) + (cols - 1) + 1, where rendered-cell-width
= max-col-content-width + 2 (the two padding spaces).

### Header detection

GFM-strict: the row immediately preceding the delimiter row is the header.
Delimiter row regex: `^\| *:?-+:? *(\| *:?-+:? *)*\|$`. Single-row header.

### Cell parsing must respect code spans and escapes

A naïve split on `|` is wrong: `\|` is an escaped literal pipe, and
`` `code with | inside` `` contains a literal pipe inside a code span.
Cell parser walks chars, tracks backtick-delimited spans (single, double,
and arbitrary backtick run lengths per CommonMark) and `\` escapes,
splits on top-level unescaped `|`.

### Skip ranges from `gfm-code-fences`

Tables inside fenced code blocks are not real tables. Before applying
overlays, gather fenced-block ranges via `gfm-code-fences--find-blocks`
and skip any candidate table overlapping those ranges. Mirrors how
`gfm-code-fences--find-indent-blocks` excludes already-fenced ranges.

### Reveal on cursor entry — per row

Add `'gfm-tables-revealable t` to each row's display overlay. A
`post-command-hook` checks whether point is inside any revealable
overlay; if so, suppresses its `display` (storing the original under
`'gfm-tables-saved-display`) and restores on cursor-leave. Same shape
as `gfm-code-fences--reveal`.

### Debounced rebuild

0.2 s idle timer triggered by `after-change-functions` and
`window-configuration-change-hook`. Same idiom as the sibling modules.
Skips indirect buffers (`buffer-base-buffer`).

### Performance instrumentation

A per-buffer-local stats struct accumulates:

- rebuild count
- total time spent in `gfm-tables--rebuild`
- last rebuild duration
- max single rebuild duration
- table count (last rebuild)

Each rebuild wraps its work in a `current-time` delta. If the duration
exceeds a configurable threshold (`gfm-tables-slow-rebuild-threshold`,
default 0.05 s), emit a `message` warning. `M-x gfm-tables-stats`
displays the current buffer's stats.

## Risks / Trade-offs

- **Reveal granularity is per-row, not per-cell** → editing a cell shows
  the entire row's source; padding/zebra disappear while editing.
  Acceptable; matches the natural unit of edit.
- **Cursor movement inside displayed regions can feel opaque** → Emacs
  treats display strings as atoms for navigation. Reveal on entry
  mitigates; the cursor lands on real source positions while editing.
- **Multi-window display** → overlays are buffer-level, so reveal in one
  window suppresses the display in all windows showing that buffer.
  Same limitation as `gfm-code-fences`. Live with it.
- **Stress-case rebuild cost (200-row tables): ~35 ms** → debounced and
  rare. Telemetry will surface it if it ever becomes a problem.
- **Cell parser correctness** → backtick-aware tokenizer is the most
  bug-prone part of the implementation. Mitigate with focused unit tests
  covering escape, single-backtick code, double-backtick code, and
  unbalanced backtick degenerate cases.
- **Theme switches** → stripe colour and border colour come from face
  resolution (good — Emacs handles this). The composed display strings
  embed face *symbols* via propertize, not concrete colours, so theme
  changes apply naturally to existing overlays without rebuild. Default-bg
  for the gap is the one place we use a concrete colour from
  `+theme-default-background`; rebuild on theme change keeps it correct.
