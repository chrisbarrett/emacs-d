## Context

The configuration already owns the rendering of three GFM block
classes via shared infrastructure in
`modules/lang-markdown/lib/+gfm-block-borders.el`:

- `gfm-callouts-mode` — `> [!TYPE]` blockquotes → titled bordered box
- `gfm-code-fences-mode` — fenced + indented + YAML helmet code →
  bordered box with language icon and tinted body fill
- `gfm-tables-mode` — pipe tables → boxed grid

The shared primitives provide: an overlay registry keyed by mode tag,
a `(get-buffer-window-list)`-based per-window reconciler with
debounced rebuild, a `wrap-prefix` continuation simulator, and a
`gfm-block-borders--available-width` helper that returns
`window-max-chars-per-line` for a window (or `fill-column`, or 80).

Markdown thematic breaks have no such treatment.  markdown-mode
already runs `markdown-match-hr` (`markdown-mode.el:1782-1792`),
which scans `markdown-regex-hr` (`markdown-mode.el:819-825`) for any
of `***`, `---`, `___` (with optional spaces between markers), and
puts a `markdown-hr` text property on the match — but *only after*
excluding lines that are heading underlines or inside code blocks.
That property is the cleanest classification signal available;
re-implementing it would duplicate edge-case handling for setext
headings, fenced/indented code blocks, and YAML helmet.

## Goals / Non-Goals

**Goals:**

- Replace dash-only HR lines (`---`, `----`, `-----`, …) with a
  single full-width unicode horizontal bar that spans the displaying
  window's character width.
- Track per-window width: the same buffer in two windows of
  different widths renders bars at each window's own width.
- Reveal source on the HR line when point lands there.
- Reuse the shared block-borders primitives so the new mode behaves
  identically under narrowing, window splits, and edits to the
  callouts / code-fences / tables modes.

**Non-Goals:**

- Decorating non-dash thematic breaks (`***`, `___`).  Per the
  proposal, only `-`-form HRs are in scope.  Asterisk and underscore
  forms remain untouched — they pass through markdown-mode's
  font-lock.
- Decorating dash lines that markdown-mode classifies as a setext-2
  heading underline.  We never override those.
- Customising bar style (single/double/heavy) via user-facing
  options.  Pin one rendering choice; revisit if needed.
- Per-block face overrides.  One face for all HR bars.

## Decisions

### Decision: Discover HRs via the `markdown-hr` text property

Scan the buffer for ranges where `markdown-hr` is set as a text
property by markdown-mode's font-lock pass.  Filter to ranges whose
underlying source starts with `-` (dash form only) so `***` and
`___` HRs are passed through.

**Rationale:** `markdown-match-hr` already excludes setext-2 heading
underlines and code-block contents
(`markdown-mode.el:1789-1790`).  Reading its output is cheap and
correct; re-implementing the classification would duplicate that
logic and drift over time.

**Alternatives considered:**

- *Run our own regex scan and reproduce filters.*  Would need to
  detect setext-2 (non-blank text on the preceding line), fenced /
  indented code blocks (need overlap with code-fences discovery),
  and YAML helmet (overlap with code-fences helmet discovery).
  Rejected: complexity outweighs the cost of reading a text
  property.

### Decision: Render as a single full-width bar via per-window display overlay

For each discovered HR line:

- Build a display string `(make-string WIDTH ?─)` propertized with
  the bar face, where `WIDTH = (gfm-block-borders--available-width
  window)`.
- Wrap the buffer's HR line in a per-window display overlay (carries
  the `window` property) whose `'display` is that string.
- The bar character is `─` (U+2500 BOX DRAWINGS LIGHT HORIZONTAL).
  Single-line, light weight — consistent with the corners callouts
  use (`┌`, `└`, `┐`, `┘`).
- Face: `+markdown-gfm-hrule-face`, inheriting `shadow` by default so
  the bar sits at a calm de-emphasised weight, falling out of focus
  the same way callout borders do.

**Rationale:** Single-line, single-overlay-per-window mirrors how
callouts handle their top/bottom borders.  No anchor split is needed
because there is no body text — the entire decoration is
width-dependent.

**Alternatives considered:**

- *Anchor + display split (mirroring callouts exactly).*  The anchor
  would carry no useful width-independent props; the line has no
  body to wrap.  Rejected as gratuitous structure.
- *Hang the bar off the line's end as an `after-string`.*  Wouldn't
  hide the original `---` source chars on the same visual row.
  Rejected.
- *Use `=` or `━` (heavy) for emphasis.*  Heavier weight competes
  visually with code-fence borders.  Light fits the surrounding
  decoration vocabulary.

### Decision: Reveal-on-cursor uses the shared `revealable` protocol

Tag the display overlay with `gfm-hrule-revealable` and register it
with the shared block-borders reveal flow.  When point is on the HR
line in the selected window, the overlay's `'display` is suppressed;
when point leaves, it's restored.  Per-window: cursor in window A
doesn't expose source in window B.

**Rationale:** Identical to callout reveal behaviour
(`+gfm-callouts.el:482-508`).  Users editing markdown should not
have to fight the decoration to delete or extend a separator.

### Decision: Reuse the shared scheduler and reconciler

Wire `gfm-hrule-mode` to the same `after-change-functions` /
`window-configuration-change-hook` / `post-command-hook` plumbing
the other modes use, via `gfm-block-borders-make-reconciler` and
`gfm-block-borders--arm-rebuild-timer`.  Scoped rebuild on edit:
when the dirty region touches an HR line or its immediate
neighbours, rebuild just that block; otherwise full rebuild.

**Rationale:** Already proven by callouts, fences, tables.  Adding a
fourth user of the same primitives doesn't grow the surface area.

### Decision: Mutual exclusion with callout / fence / table interiors

HRs discovered inside a callout body line (technically the
underlying text could be a `> ---` blockquote, which CommonMark
permits) SHALL NOT be decorated.  Discovery filters out HR ranges
whose `markdown-hr` text property starts after a `>` character —
markdown-mode already handles this via `markdown-match-hr`'s
context check, but as a belt-and-braces measure exclude any HR line
whose first non-whitespace character is `>`.

Code fence / YAML helmet overlap is already excluded by
markdown-mode (`markdown-code-block-at-point-p`); no extra work
needed.

## Risks / Trade-offs

- [`markdown-hr` text property not yet populated on first rebuild]
  → markdown-mode's font-lock runs `markdown-syntax-propertize` on
  buffer load and after edits via `after-change-functions`.  Our
  mode's initial rebuild fires after the major mode is set, so the
  property is populated.  If we observe an empty initial set in
  practice, force `font-lock-ensure` over the buffer once in
  `gfm-hrule-mode`'s setup.
- [Window width changes mid-session don't trigger rebuild on the HR
  line] → The shared
  `window-configuration-change-hook` →
  `gfm-block-borders--reconcile-windows` path already covers width
  changes for callouts; same path applies here.
- [HR sitting at EOB has no trailing newline]  →  Discovery records
  the line as `(BEG . EOL)`; display overlay covers that exact
  range.  Same handling as a callout's last body line.
- [User configures custom HR markers (e.g. `markdown-hr-strings`)]
  → markdown-mode's `markdown-regex-hr` is fixed (asterisk / dash /
  underscore forms); `markdown-hr-strings` only affects insertion,
  not detection.  Out of scope.

## Migration Plan

Single-commit change.  Adding a new minor mode and hook entry; no
data migration; rollback is a revert.

## Open Questions

None.
