# gfm-pretty Specification

## Purpose

Lives at `lisp/gfm/gfm-pretty.el` (library axis). Behaviour-facing:
defines the umbrella minor mode (`gfm-pretty-mode`), the decorator
registration protocol (`gfm-pretty-define-decorator`), the public
block-introspection API (`gfm-pretty-block-at-point`,
`gfm-pretty-edit-block-at-point`), per-decorator toggling
(`gfm-pretty-toggle-decorator`), and the rendering / scheduling
contracts of the five built-in decorators (callouts, fences, tables,
hrule, links).

Cross-references the `gfm-present` axis for the slide walkthrough mode
that operates over `gfm-pretty`-decorated buffers.

---

## Requirements
<!-- ── Engine: umbrella mode and registration ─────────────────────── -->

### Requirement: Umbrella minor mode

The system SHALL provide a buffer-local minor mode `gfm-pretty-mode`
that enables every registered decorator with a non-nil enable bit and
removes every overlay it created when disabled.

`gfm-pretty-mode` SHALL be enabled in every `gfm-mode` buffer via
`gfm-mode-hook` (added by the `lang-markdown` composition module,
which `(require 'gfm-pretty)` and enables the mode).

#### Scenario: Enabling the umbrella mode in a markdown buffer

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing GFM
  callouts, fenced code blocks, tables, hrules, and links
- **THEN** every registered decorator's anchor and display overlays
  are applied
- **AND** the callout font-lock keywords are installed
- **AND** the buffer renders with all visual decorations

#### Scenario: Disabling the umbrella mode

- **WHEN** `gfm-pretty-mode` is disabled in a decorated buffer
- **THEN** every overlay created by every decorator is removed
- **AND** every decorator's `:on-disable` callback runs (e.g. removing
  the links xref backend and eldoc function)
- **AND** the buffer visually matches its raw markdown source

### Requirement: Decorator registration via `gfm-pretty-define-decorator`

The system SHALL expose `gfm-pretty-define-decorator` as the public
registration entry point. A decorator SHALL declare itself by name and
contribute pure functions; the engine SHALL own the lifecycle hooks
(`after-change-functions`, `window-configuration-change-hook`,
`post-command-hook`, the idle rebuild timer, the dirty-region tracker,
the window-state snapshot).

A registration form SHALL accept:

- `NAME` (symbol) — decorator identifier.
- `:collect FN` — returns the buffer's list of blocks (under widening).
- `:range FN` — returns `(BEG . END)` for a block.
- `:apply-anchors FN` — applies a block's width-independent overlays.
- `:apply-display FN` — applies a block's width-dependent per-window
  overlays.
- `:font-lock KEYWORDS` (optional) — font-lock keywords contribution.
- `:revealable-p FN` (optional) — predicate for cursor reveal.
- `:block-at-point FN` (optional) — predicate for the public block
  introspection API.
- `:edit-at-point FN` (optional) — command for the public edit
  dispatcher.
- `:on-enable FN` (optional) — decorator extras to install when the
  umbrella mode (or the decorator's per-decorator toggle) enables.
- `:on-disable FN` (optional) — symmetric teardown.

#### Scenario: Registering a new decorator

- **WHEN** a new visual-decoration feature is added
- **THEN** it SHALL declare itself via
  `(gfm-pretty-define-decorator 'NAME …)` with the contributions it
  needs
- **AND** the engine SHALL invoke its functions during rebuild, reveal,
  and reconcile without further wiring

### Requirement: Per-decorator toggle via `gfm-pretty-toggle-decorator`

The system SHALL expose `gfm-pretty-toggle-decorator NAME` (interactive)
which flips a buffer-local enable bit for the named decorator. When the
bit is nil, the engine SHALL skip that decorator during rebuild,
reveal, and reconcile, and the decorator's overlays SHALL be removed.
When toggled back on, the decorator's overlays SHALL be rebuilt.

#### Scenario: Toggling the tables decorator off

- **GIVEN** `gfm-pretty-mode` is enabled with the tables decorator
  active
- **WHEN** the user calls `(gfm-pretty-toggle-decorator 'tables)`
- **THEN** every table overlay is removed
- **AND** other decorators' overlays remain untouched
- **AND** the leader binding `SPC m t` (configured to call this toggle)
  reports the new state

### Requirement: Public block introspection

The system SHALL expose `gfm-pretty-block-at-point` and
`gfm-pretty-edit-block-at-point` as the only public block-aware
entry points. `gfm-pretty-block-at-point` SHALL return
`(DECORATOR-NAME . BLOCK)` for the first registered decorator whose
`:block-at-point` returns non-nil, or nil if none matches.
`gfm-pretty-edit-block-at-point` SHALL dispatch to the matched
decorator's `:edit-at-point`.

No `gfm-pretty--*` private symbol SHALL be relied upon by code outside
the `gfm-pretty` module.

#### Scenario: Leader binding queries the public API

- **GIVEN** `modules/leader/init.el` provides the local-leader `t`
  binding for "edit block at point"
- **WHEN** point is inside a GFM table
- **THEN** the binding calls `gfm-pretty-edit-block-at-point`
- **AND** the tables decorator's `:edit-at-point` is invoked
- **AND** no private `gfm-pretty--*` symbol is read

### Requirement: Rendering primitives are public

The system SHALL expose the following primitives for decorator authors:

- `gfm-pretty-available-width &optional WINDOW` — char width
  primitive.
- `gfm-pretty-top-border WIDTH FACE BUFFER-WIDTH &optional ICON` —
  top-border (LEADING . TRAILING) split.
- `gfm-pretty-bottom-border WIDTH FACE BUFFER-WIDTH` — bottom-border
  split.
- `gfm-pretty-right-after BOX-WIDTH FACE &optional BG` — right-edge
  after-string.
- `gfm-pretty-simulate-wrap TEXT WIDTH &optional CONT-PREFIX-W` —
  wrap-position simulator.
- `gfm-pretty-make-anchor BEG END &rest PROPS` — anchor-overlay
  factory bound to the calling decorator's registry.
- `gfm-pretty-make-display BEG END WINDOW &rest PROPS` —
  display-overlay factory restricted to WINDOW when non-nil.

Every other engine symbol SHALL be `gfm-pretty--` private.

#### Scenario: Decorator builds a bordered block

- **GIVEN** a decorator's `:apply-display` callback
- **WHEN** it constructs a border for the current window
- **THEN** it calls `gfm-pretty-available-width` and
  `gfm-pretty-top-border` / `gfm-pretty-bottom-border` /
  `gfm-pretty-right-after`
- **AND** it does NOT reference `gfm-pretty--*` internal helpers

<!-- ── Engine: cross-cutting contracts ────────────────────────────── -->

### Requirement: Debounced rebuild scheduler

The engine SHALL run at most one idle rebuild timer per buffer. The
timer SHALL fire after a 0.2 second idle delay following a buffer
modification, iterate every enabled decorator, and rebuild within the
buffer's accumulated dirty region.

#### Scenario: Burst of edits causes one rebuild

- **GIVEN** `gfm-pretty-mode` is enabled
- **WHEN** the user types ten characters in rapid succession
- **THEN** the engine SHALL cancel and re-arm the timer on each edit
- **AND** the rebuild SHALL run once, 0.2 s after the last edit
- **AND** every decorator's `:apply-anchors` / `:apply-display`
  contributions SHALL be invoked over the dirty region

### Requirement: Per-window decorator rendering

The engine SHALL produce one display-overlay set per window currently
showing the buffer, restricted via the `window` overlay property, for
every decorator that applies width-dependent display. A buffer split
across two windows of different widths SHALL render at each window's
own width.

#### Scenario: Buffer shown in two windows of different widths

- **GIVEN** `gfm-pretty-mode` is enabled in a buffer containing a
  fenced code block
- **AND** the buffer is displayed in two windows W1 (width 80) and W2
  (width 120)
- **THEN** W1 SHALL render the fence box at width 80
- **AND** W2 SHALL render the fence box at width 120

### Requirement: Per-window cursor reveal

The engine SHALL install one `post-command-hook` handler that consults
every enabled decorator's `:revealable-p` predicate (or its default —
overlays carrying the engine's `revealable` flag). Reveal SHALL be
scoped to the selected window: an overlay restricted to a non-selected
window SHALL NOT have its source revealed regardless of where point is
in the selected window.

#### Scenario: Point on a fence marker reveals the source in selected window only

- **GIVEN** a fenced code block decorated in W1 (selected) and W2
- **WHEN** point moves onto the opening fence marker in W1
- **THEN** W1 SHALL show the raw `\`\`\`lang` line
- **AND** W2 SHALL continue to show the rendered box

### Requirement: Selective per-window reconciliation

When `window-configuration-change-hook` fires, the engine SHALL compare
the prior `(window . width)` snapshot against the current set. Removed
windows SHALL have their display overlays deleted synchronously.
Added or resized windows SHALL be rebuilt on the next idle tick,
visible blocks first, off-screen blocks after, one block per idle tick.
Unchanged windows SHALL keep their overlays.

#### Scenario: Splitting the window

- **GIVEN** a decorated buffer in W1
- **WHEN** the user runs `C-x 3` producing W2 of the same width
- **THEN** the engine SHALL pace W2's rebuild one block per idle tick
- **AND** W1's overlays SHALL remain untouched

### Requirement: Visible-first prioritised rebuild

When a width change triggers a per-window rebuild, the engine SHALL
render blocks intersecting the window's `(window-start . window-end)`
range first, then off-screen blocks one per idle tick, until the queue
is exhausted.

#### Scenario: Large file with many blocks

- **GIVEN** a 1000-line markdown file with 30 fenced blocks
- **WHEN** the window width changes
- **THEN** the blocks visible in the current window SHALL be rendered
  in the first idle ticks
- **AND** off-screen blocks SHALL render in later ticks

### Requirement: Wrap simulation always terminates

`gfm-pretty-simulate-wrap` SHALL terminate for every input. When the
effective per-line width is less than or equal to the continuation
wrap-prefix width, the simulator SHALL clamp the line width to at
least 1 so the position counter advances on every loop iteration.

This guards against tiny-window transients (e.g. `C-x 3` splits where
one window briefly drops below the wrap-prefix width).

#### Scenario: Three-column window

- **GIVEN** a callout body line of 200 characters
- **WHEN** the window is 3 columns wide (less than the 2-char
  wrap-prefix)
- **THEN** `gfm-pretty-simulate-wrap` SHALL return a result
- **AND** the function SHALL NOT loop forever

<!-- ── Callouts decorator ─────────────────────────────────────────── -->

### Requirement: Callouts decorator registration and toggle

The system SHALL register a decorator named `callouts` that applies a
bordered-callout overlay decoration plus a font-lock keyword
contribution colouring `> [!TYPE]` markers and bodies.

`(gfm-pretty-toggle-decorator 'callouts)` SHALL flip both layers
together — overlays and font-lock keywords.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer with callouts
- **THEN** each recognised callout (NOTE, TIP, IMPORTANT, WARNING,
  CAUTION, CRITICAL) is decorated with a curved-border box, a
  type-coloured top label, and a tinted background
- **AND** the `> [!TYPE]` marker text is fontified by the callouts
  font-lock keywords

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'callouts)` flips the enable
  bit off
- **THEN** every callout overlay is removed
- **AND** the callout font-lock keywords are removed
- **AND** the marker text reverts to default markdown-mode
  fontification

### Requirement: Callout block discovery

The callouts decorator's `:collect` SHALL recognise a callout block as
a marker line of the form `> [!TYPE]` where `TYPE` is one of `NOTE`,
`TIP`, `IMPORTANT`, `WARNING`, `CAUTION`, `CRITICAL`, followed by zero
or more `>`-prefixed continuation lines. The block range SHALL run
from the marker line's BOL to the last continuation line's EOL.

Discovery SHALL widen before scanning so a narrowed buffer (e.g. under
`gfm-present-mode`) still sees the full block boundaries.

#### Scenario: Single-line marker

- **GIVEN** a buffer line `> [!NOTE]`
- **WHEN** `:collect` runs
- **THEN** the returned block range covers the line only

#### Scenario: Multi-line callout

- **GIVEN** a marker `> [!IMPORTANT]` followed by `> body line 1` and
  `> body line 2`
- **THEN** the block range covers all three lines

### Requirement: Callout bordered-block rendering

The callouts decorator's `:apply-display` SHALL render a bordered
callout box with:

- A top border using the type's coloured face and the type label as
  the upper-right caption.
- A `│ ` substitution for the `> ` prefix on each body line, using
  the border face.
- A right-edge `│` painted via `gfm-pretty-right-after` (or its
  overflow variant on wrapped body lines) so the right border
  aligns to the box width regardless of body-line wrapping.
- A bottom border using the type's coloured face.
- A tinted body background using the type's body-face background
  computed by `gfm-pretty-callouts--tint-bg` (10% from the type
  face foreground toward the theme background).

Anchor overlays SHALL carry the body-face property and a
`wrap-prefix` of `│ `; display overlays SHALL carry the borders and
right-edge after-strings.

#### Scenario: NOTE callout

- **GIVEN** `> [!NOTE]\n> hello`
- **WHEN** the decorator renders
- **THEN** the top border shows `┌── NOTE ──┐` (right-aligned label)
- **AND** the body line shows `│ hello` with a blue-tinted background
- **AND** the bottom border shows `└──────────┘`

### Requirement: Callout box width sizing

Box width SHALL be `min(text-width, max(80, max-content-width + 4))`
per window, where `text-width` is `gfm-pretty-available-width` for the
window and `max-content-width` is the longest body line's visible
width minus the 2-char `> ` prefix.

#### Scenario: Wide content, narrow window

- **GIVEN** a callout whose longest body line is 60 cells
- **AND** a window 100 cells wide
- **THEN** the box width SHALL be `min(100, max(80, 64)) = 80`

#### Scenario: Narrow content, wide window

- **GIVEN** a callout whose longest body line is 20 cells
- **AND** a window 100 cells wide
- **THEN** the box width SHALL be `min(100, max(80, 24)) = 80`

#### Scenario: Wide content, very wide window

- **GIVEN** a callout whose longest body line is 200 cells
- **AND** a window 250 cells wide
- **THEN** the box width SHALL be `min(250, max(80, 204)) = 204`

### Requirement: Callout wrapped right-edge alignment

For a body line wrapping in the window, the right-edge `│` SHALL be
padded by simulating wrap via `gfm-pretty-simulate-wrap` so the
border lands at the box width on the final visual row, regardless of
how the line wraps.

#### Scenario: Long body line wraps mid-line

- **GIVEN** a 200-char body line in an 80-col window
- **WHEN** the right-edge after-string is computed
- **THEN** the `│` lands at column 80 on the final wrapped visual row

### Requirement: Callout marker line and body prefix reveal

The callouts decorator's `:revealable-p` predicate SHALL return
non-nil for display overlays carrying the engine's revealable flag.
When point lies on the marker line, the engine SHALL hide the marker's
display overlay so the raw `> [!TYPE]` text shows. When point lies on
a body line, the engine SHALL hide that body line's `│ ` prefix
substitution so the raw `> ` shows.

Reveal SHALL be scoped to the selected window.

#### Scenario: Point on marker

- **GIVEN** a NOTE callout with point on the marker line in W1
  (selected)
- **THEN** W1 shows the raw `> [!NOTE]` text
- **AND** the top-border decoration is visible
- **AND** other windows showing the buffer continue to show the
  marker decoration

### Requirement: Callout block-discovery cache

The callouts decorator SHALL memoise `:collect` results by
`buffer-chars-modified-tick` so repeated calls within an unmodified
buffer SHALL NOT rescan.

#### Scenario: Repeated reveal calls

- **GIVEN** a buffer with 20 callouts
- **WHEN** `post-command-hook` invokes reveal twice between edits
- **THEN** the second invocation SHALL hit the cache and NOT rescan

### Requirement: Callout narrowing-resilient discovery and teardown

`:collect` SHALL widen the buffer for the duration of its scan. Overlay
teardown via the engine's bulk-cleanup helper SHALL also widen so the
registry list and on-buffer overlay set stay in lockstep regardless of
any current narrowing.

When the engine rebuilds within a sub-region (e.g. under
`gfm-present-mode` narrowing), the decorator's overlay set SHALL
converge with the same overlays that would be present in a clean
widened rebuild — the suite
`(narrow → rebuild → widen → rebuild)` SHALL produce a steady state.

#### Scenario: Narrowed rebuild then widen

- **GIVEN** a buffer with three callouts narrowed to the second
- **WHEN** `gfm-pretty--rebuild` runs, then the buffer is widened, then
  rebuilt again
- **THEN** the final overlay set SHALL match what a fresh widened
  rebuild produces
- **AND** no `args-out-of-range` SHALL signal

### Requirement: Callout scoped post-edit rebuild

The engine SHALL rebuild only the callout block whose marker or
continuation lines the dirty region intersects, plus any adjacent
callout that the dirty region extends into. When the dirty region
does not intersect any callout, the callouts decorator SHALL NOT
contribute to the rebuild iteration.

#### Scenario: Edit inside one callout body

- **GIVEN** two non-adjacent callouts
- **WHEN** the user edits inside callout #1's body
- **THEN** only callout #1 is rebuilt
- **AND** callout #2's overlays are untouched

### Requirement: Theme change responsiveness

The callouts decorator's `:on-enable` SHALL add
`+markdown-gfm-callout-refresh-body-faces` to `+theme-changed-hook`.
`:on-disable` SHALL remove it. The refresh function SHALL recompute
each callout body face's `:background` from the current theme by
tinting 10% toward the theme background.

#### Scenario: Theme switch

- **WHEN** the user switches from a light to a dark theme
- **THEN** `+theme-changed-hook` fires
- **AND** each `+markdown-gfm-callout-*-body-face` background is
  recomputed
- **AND** the next redisplay shows correctly tinted body backgrounds

<!-- ── Fences decorator ───────────────────────────────────────────── -->

### Requirement: Fences decorator registration and toggle

The system SHALL register a decorator named `fences` that renders GFM
fenced code blocks, YAML front-matter helmets, and tab-/4-space
indented code blocks with a curved border, a language icon, and
reveal-on-cursor for fence markers.

`(gfm-pretty-toggle-decorator 'fences)` SHALL flip the decorator on
and off.

#### Scenario: Toggling on

- **WHEN** the fences decorator is enabled in a buffer with a fenced
  code block
- **THEN** the block is rendered inside a curved border `┌ … ┐ … └ … ┘`
- **AND** the language icon appears at the upper-right of the top
  border when `nerd-icons` is available

### Requirement: Fenced block discovery

The fences decorator's `:collect` SHALL discover opening fences of the
form `^\s*```+\s*({\s*)?(LANG)?\s*…$` and matching closing fences
`^\s*```+\s*$`. The block range covers BOL of the opener to EOL of the
closer. The lang capture extends to icon resolution.

#### Scenario: Standard fenced block

- **GIVEN** `\`\`\`bash\necho hi\n\`\`\``
- **THEN** `:collect` returns the range BOL of the opener to EOL of
  the closer
- **AND** the language is `bash`

### Requirement: YAML helmet discovery

The fences decorator's `:collect` SHALL recognise leading YAML
front-matter delimited by `^---\s*$` on the first non-blank line and
another `^---\s*$` line below it. Helmets SHALL be treated as
code-fence blocks for rendering purposes.

#### Scenario: YAML frontmatter

- **GIVEN** a buffer starting with `---\ntitle: Hello\n---\n# body`
- **THEN** the YAML helmet between the two `---` lines SHALL be
  collected as a block

### Requirement: Indented block discovery

The fences decorator's `:collect` SHALL recognise 4-space or
tab-indented code blocks preceded and followed by blank lines, except
where they fall inside a fenced block's range.

#### Scenario: Indented block between paragraphs

- **GIVEN** a paragraph, a blank line, four-space-indented code, a
  blank line, and another paragraph
- **THEN** the indented region SHALL be collected as a block

### Requirement: Mutual exclusion of indent inside fence

`:collect` SHALL NOT emit an indented-block entry whose range is
already covered by a fenced or YAML block.

#### Scenario: Indented lines inside a fence

- **GIVEN** a fenced block whose body happens to be 4-space indented
- **THEN** only the fenced block SHALL be returned

### Requirement: Code-fence bordered-block rendering

`:apply-display` SHALL render a curved-border box with:

- A top border using the border face and (when resolvable) a
  language icon at the upper-right.
- A `│ ` wrap-prefix and right-edge `│` so wrapped lines align to
  the box width.
- A bottom border.

Anchor overlays carry width-independent props (wrap-prefix, body
background fill). Display overlays carry the borders and right-edge
after-strings, restricted to `WINDOW`.

#### Scenario: Fence with language

- **GIVEN** `\`\`\`bash\necho hi\n\`\`\``
- **THEN** the top border renders `┌──── …  ┐` with the bash icon
  right-aligned
- **AND** body line renders `│ echo hi` with a default-bg fill behind
  it
- **AND** bottom border `└──── …  ┘`

### Requirement: Code-fence body background fill

The fences decorator SHALL paint a background fill on the body region
of every fenced/YAML/indented block using `gfm-code-fences--face-extend-bg`
to lift the underlying default-bg cleanly under `:extend t` text-property
faces (e.g. `diff-added`).

#### Scenario: Body line with diff face

- **GIVEN** a fenced block one of whose lines has a `diff-added`
  text-property face
- **THEN** the body-background fill is visible up to the right border
- **AND** the diff face's background does not leak past the right
  border

### Requirement: Code-fence box width sizing

Box width SHALL be `gfm-pretty-available-width` for the rendering
window. Wrapping is handled via the body wrap-prefix and the right-edge
overflow after-string.

#### Scenario: Narrow window

- **GIVEN** a fenced block in a 60-cell window
- **THEN** the box width is 60

### Requirement: Code-fence marker line reveal

The fences decorator's `:revealable-p` SHALL fire for display overlays
covering the opening or closing fence marker. When point lies on a
marker, the engine SHALL hide that overlay so the raw `\`\`\`lang`
shows in the selected window.

#### Scenario: Point on opening fence

- **GIVEN** point on `\`\`\`bash`
- **THEN** the selected window shows `\`\`\`bash` (raw)
- **AND** other windows continue to show the top-border decoration

### Requirement: Language icon resolution

When `nerd-icons` is loaded, the fences decorator SHALL resolve the
language tag against `markdown-code-lang-modes` (lang → mode → icon)
and render the icon as the top border's right-aligned glyph. When
unresolved or `nerd-icons` is absent, the top border SHALL render
without an icon.

#### Scenario: Known language

- **GIVEN** a `\`\`\`go` fence and `nerd-icons` available
- **THEN** the top border ends with the Go icon glyph

#### Scenario: Unknown language

- **GIVEN** `\`\`\`flubber` for which no entry exists in
  `markdown-code-lang-modes`
- **THEN** the top border renders without an icon

### Requirement: YAML body fontification

The fences decorator SHALL fontify the body of a YAML helmet using
`yaml-ts-mode`'s font-lock (when its tree-sitter grammar is loadable)
so keys and values colourise inside the helmet's border.

#### Scenario: YAML helmet fontification

- **GIVEN** a YAML helmet with `title: Hello`
- **AND** `yaml-ts-mode` grammar is available
- **THEN** `title` renders with a key face and `Hello` with a value
  face

### Requirement: Code-fence block-discovery cache

The fences decorator SHALL memoise `:collect` results by
`buffer-chars-modified-tick`.

#### Scenario: Repeated reveal calls

- **GIVEN** a buffer with 20 fenced blocks
- **WHEN** reveal runs three times between edits
- **THEN** the scan executes once

### Requirement: Code-fence narrowing-resilient discovery and teardown

`:collect` SHALL widen for the duration of its scan; bulk-cleanup
widens for its deletion sweep. `(narrow → rebuild → widen → rebuild)`
SHALL converge.

#### Scenario: Narrowed rebuild then widen

- **GIVEN** a buffer with three fenced blocks narrowed to the second
- **WHEN** the engine rebuilds, then widens, then rebuilds
- **THEN** the final overlay set matches a fresh widened rebuild
- **AND** no `args-out-of-range` signals

### Requirement: Code-fence scoped post-edit rebuild

When the dirty region intersects a fence line, the engine SHALL
rebuild that block. When the dirty region is a blank line adjacent to
an indented block, the engine SHALL rebuild any adjacent indented
block whose extent the edit may have changed.

#### Scenario: Edit inside fenced body

- **GIVEN** two fenced blocks
- **WHEN** the user edits inside block #1's body (not a marker)
- **THEN** only block #1 is rebuilt by the fences decorator

### Requirement: Code-fence performance instrumentation

The fences decorator SHALL track per-rebuild duration and accumulate
phase-level totals (`collect`, `apply-anchors`, `apply-display`,
`reconcile`). When a single rebuild exceeds
`gfm-pretty-fences-slow-rebuild-threshold` (default 0.05 s) the
decorator SHALL emit a `display-warning` of severity `:warning`.

`gfm-pretty-fences-stats` SHALL be an interactive command that
displays the accumulated totals.

#### Scenario: Slow rebuild logged

- **GIVEN** a buffer where rendering a single fence takes 0.1 s
- **WHEN** that rebuild completes
- **THEN** a warning is logged via `display-warning`

<!-- ── Tables decorator ───────────────────────────────────────────── -->

### Requirement: Tables decorator registration and toggle

The system SHALL register a decorator named `tables` that renders GFM
pipe tables as bordered, zebra-striped grids with cursor-aware cell
highlighting and indirect-edit commands.

`(gfm-pretty-toggle-decorator 'tables)` flips the decorator on/off.
The local-leader binding `t` SHALL call this toggle.

#### Scenario: Toggling

- **WHEN** the tables decorator is enabled in a buffer with a table
- **THEN** the table renders with `┌ … ┐`, body rows, and a closing
  `└ … ┘` border
- **AND** alternating body rows have a striped background

### Requirement: Table block discovery

`:collect` SHALL recognise a GFM table as a header line followed by an
alignment line (`|---|---|`) followed by zero or more body rows, each
delimited by `|`.

#### Scenario: Two-column table

- **GIVEN** `| a | b |\n|---|---|\n| 1 | 2 |`
- **THEN** the block range covers all three lines

### Requirement: Cell parser

The tables decorator SHALL parse each row into cells delimited by
`|`. Leading and trailing `|` (exterior pipes) SHALL be optional. A
literal `\|` SHALL be preserved as cell content, not a delimiter.

#### Scenario: Escaped pipe

- **GIVEN** a cell `a \| b`
- **THEN** the cell contains the text `a | b`

### Requirement: Column width normalisation

Column widths SHALL be normalised to the maximum visible width per
column across header + body, then padded with spaces in each cell.

#### Scenario: Variable cell widths

- **GIVEN** column 1 cells `a`, `bb`, `ccc`
- **THEN** all rendered cells in column 1 are width 3

### Requirement: Auto-composition does not skew column widths

Cell width measurements SHALL use `string-width` over the source text,
ignoring composed glyphs. Italicised letters, inline-link replacements,
and other composition products SHALL NOT inflate measured widths
beyond the underlying source character count.

#### Scenario: Italic cell

- **GIVEN** a cell `*hello*`
- **THEN** the cell width measurement uses `string-width` over the
  source string, not the rendered glyph composition

### Requirement: Window-fitted column widths

The tables decorator SHALL cap column widths proportionally so the
rendered table fits within the window's `gfm-pretty-available-width`
without horizontal scroll, when the natural total table width would
otherwise exceed it.

#### Scenario: Narrow window

- **GIVEN** a four-column table with natural width 200 cells
- **AND** a 100-cell window
- **THEN** column widths are capped so total width ≤ 100

### Requirement: Cell wrapping for capped columns

Cell content SHALL wrap inside its cell when the column is capped
below the cell's natural width, padded so subsequent rows remain
aligned to the column boundaries.

#### Scenario: Long cell in capped column

- **GIVEN** a cell `the quick brown fox` capped to 10 cells
- **THEN** the cell renders across multiple visual lines, each padded
  to column width

### Requirement: Cell-edit commit preserves the row

After an indirect-edit commit, the underlying source row SHALL be
re-built with the new cell content, preserving the row's column
boundaries and other cells.

#### Scenario: Commit edited cell

- **GIVEN** indirect edit replaces cell 2 with `xyz`
- **WHEN** the user commits
- **THEN** the source row's cell 2 reads `xyz`
- **AND** other cells are byte-identical to their pre-edit content

### Requirement: Table border and rule decoration

The tables decorator SHALL render:

- A top border `┌────────┐` above the header row.
- A mid rule `├────────┤` replacing the alignment line.
- A bottom border `└────────┘` below the last body row.

#### Scenario: Rendered borders

- **GIVEN** a one-row body table
- **THEN** the rendering shows top border, header, mid rule, body
  row, and bottom border in that order

### Requirement: Exterior pipe rendering

Outer `|` characters in each row SHALL render as `│`, painting the
left and right edges of the box. Optional exterior pipes in source
SHALL render identically to the explicit case.

#### Scenario: No exterior pipes in source

- **GIVEN** source `a | b\n---|---\n1 | 2`
- **THEN** the rendered rows show `│ a │ b │` with the left and right
  `│` synthesised

### Requirement: Interior column gap rendering

Interior `|` separators SHALL render with a one-cell default-bg gap on
either side and a `│` border between cells, distinct from the row
background fill.

#### Scenario: Two body rows side-by-side

- **GIVEN** a two-column table
- **THEN** the column gap renders as ` │ ` between adjacent cells
- **AND** the gap colour is default-bg, not the row's stripe colour

### Requirement: Header emphasis

The header row SHALL render with a header-style face emphasising its
text relative to body rows.

#### Scenario: Header row weight

- **GIVEN** a table
- **THEN** the header row's face renders the text at a stronger weight
  than body rows

### Requirement: Body row zebra striping

Alternating body rows SHALL render with a stripe-coloured background.
Odd-indexed rows (1-based, header excluded) use the stripe face;
even-indexed rows use the default-bg.

#### Scenario: Four-row body

- **GIVEN** four body rows
- **THEN** rows 1 and 3 render with the stripe face background
- **AND** rows 2 and 4 render with default-bg

### Requirement: Stripe face

The tables decorator SHALL expose `gfm-pretty-tables-row-alt-face` and
`gfm-pretty-tables-row-alt-cap-face` as the stripe and stripe-cap
faces. The cap face's `:foreground` SHALL mirror the stripe face's
`:background` so half-block caps (`▐` / `▌`) extend the stripe to the
header box's vertical edges.

#### Scenario: Custom stripe colour

- **WHEN** the user customises `gfm-pretty-tables-row-alt-face` to a
  different background
- **THEN** the next rebuild renders stripes in the new colour
- **AND** the stripe-cap face's foreground updates to match

### Requirement: Active-cell highlight

The cell containing point SHALL render with a blue-tinted background
(`gfm-pretty-tables-active-cell-face`). The highlight SHALL track
point movement within and between cells.

#### Scenario: Moving between cells

- **WHEN** point moves from cell (row 1, col 1) to cell (row 1, col 2)
- **THEN** the highlight shifts to the new cell on the next redisplay

### Requirement: Cursor anchoring inside cells

Cursor motion within a cell SHALL be confined to the cell's visible
character range: cursor SHALL NOT land on the rendered `│` separator
or the column-gap padding.

#### Scenario: Right-arrow past cell end

- **WHEN** point is at the last character of cell 1 and user presses
  right-arrow
- **THEN** point jumps to the first character of cell 2 (skipping the
  separator)

### Requirement: Cell-entry key hints

When point enters a cell, the echo-area SHALL display a hint with the
key bindings for cell motion and edit commands.

#### Scenario: Hint on cell entry

- **WHEN** point moves into a cell
- **THEN** the echo area shows the hint string within one
  post-command tick

### Requirement: Structural cell motion

The tables decorator SHALL bind cell-motion commands (next-cell,
previous-cell, next-row-same-column, previous-row-same-column) on its
mode keymap.

#### Scenario: Tab moves to next cell

- **WHEN** point is in cell 1 and user presses TAB
- **THEN** point moves to the first character of cell 2

### Requirement: Snap-to-cell on row entry

Point SHALL snap to the first cell's first character on row entry from
outside the table (vertical motion), rather than landing on a separator
or column gap.

#### Scenario: Down-arrow into table

- **WHEN** point is on the line above a table and user presses
  down-arrow
- **THEN** point lands on the first character of the header row's
  first cell

### Requirement: In-place edit commands divert to indirect editor

The tables decorator SHALL open the current cell in an indirect buffer
and replay the keystroke there, whenever the user invokes a destructive
editing command (insert, delete, yank) on a rendered cell.

#### Scenario: Insert key in cell

- **WHEN** point is in cell `abc` and user presses `i` (evil insert)
- **THEN** an indirect-edit popup opens with the cell content
- **AND** subsequent keys edit the indirect buffer

### Requirement: Cell-only indirect edit

A user command SHALL be available to open just the current cell in an
indirect editor, exiting via `C-c C-c` to commit or `C-c C-k` to
abort.

#### Scenario: Commit

- **GIVEN** an open cell indirect editor with content `xyz`
- **WHEN** the user presses `C-c C-c`
- **THEN** the source row's cell is replaced with `xyz`
- **AND** the indirect buffer closes

### Requirement: Whole-table indirect edit

A user command SHALL be available to open the entire table in an
indirect editor for free-form pipe-syntax editing, with commit /
abort bindings.

#### Scenario: Whole-table edit

- **WHEN** the user invokes the whole-table edit command
- **THEN** an indirect buffer opens with the table's raw pipe-syntax
  source
- **AND** commit replaces the source range with the edited content

### Requirement: Header column swap

A user command SHALL move a column left or right (swapping with its
neighbour). The swap SHALL update header, alignment line, and every
body row consistently.

#### Scenario: Move column right

- **GIVEN** a three-column table, point in column 2
- **WHEN** the user invokes "move column right"
- **THEN** columns 2 and 3 are swapped in every row of the source

### Requirement: Table performance instrumentation

The tables decorator SHALL track per-rebuild duration and accumulate
phase totals. Slow rebuilds (exceeding
`gfm-pretty-tables-slow-rebuild-threshold`, default 0.05 s) SHALL emit
a `display-warning`. `gfm-pretty-tables-stats` SHALL be an
interactive command displaying totals.

#### Scenario: Slow rebuild

- **GIVEN** a rebuild taking 0.08 s
- **THEN** a warning is logged

### Requirement: Table narrowing-resilient discovery and teardown

`:collect` SHALL widen for its scan. Bulk-cleanup widens. The
`(narrow → rebuild → widen → rebuild)` sequence SHALL converge.

#### Scenario: Narrowed rebuild

- **GIVEN** a buffer with two tables narrowed to the second
- **WHEN** the engine rebuilds, then widens, then rebuilds
- **THEN** the final overlay set matches a fresh widened rebuild
- **AND** no zombie overlays remain in the formerly-narrowed range

<!-- ── Hrule decorator ────────────────────────────────────────────── -->

### Requirement: Hrule decorator registration and toggle

The system SHALL register a decorator named `hrule` that replaces
qualifying GFM dash-form thematic break lines (`---`, `----`, …) with
a single window-width unicode horizontal bar.

`(gfm-pretty-toggle-decorator 'hrule)` flips on/off.

#### Scenario: Toggling

- **WHEN** the hrule decorator is enabled in a buffer with `---`
- **THEN** the line renders as a window-wide unicode bar `────…`

### Requirement: HR block discovery

`:collect` SHALL read ranges with the `markdown-hr` text property set
by `markdown-syntax-propertize-hrs`, filter to those whose
first non-whitespace character is `-` (dash form), and exclude lines
whose first non-whitespace character is `>` (blockquote-nested
HR-like text). `***` and `___` forms SHALL pass through to markdown's
font-lock unchanged (not collected here).

The `:collect` function SHALL ensure `markdown-syntax-propertize-hrs`
has been applied over the scanned region by calling
`syntax-propertize` before reading the property.

#### Scenario: Dash-form HR

- **GIVEN** a `---` line outside any code block
- **THEN** `:collect` returns the line's range

#### Scenario: Star-form HR

- **GIVEN** a `***` line
- **THEN** `:collect` returns nothing for it
- **AND** markdown-mode's font-lock decoration renders it normally

#### Scenario: Setext heading underline excluded

- **GIVEN** a `Title\n---` setext-2 heading
- **THEN** `:collect` SHALL NOT return the `---` line as an HR

### Requirement: HR rendering

`:apply-display` SHALL replace each collected HR line's display with
`(make-string WIDTH ?─)` propertised with the hrule face. WIDTH comes
from `gfm-pretty-available-width` for the rendering window.

#### Scenario: HR in 100-cell window

- **GIVEN** an HR line in a 100-cell window
- **THEN** the display string is 100 `─` characters

### Requirement: HR cursor reveal

The hrule decorator's `:revealable-p` SHALL fire for the HR display
overlay. When point lies on the HR line, the engine SHALL hide the
overlay so the raw `---` source shows in the selected window only.

#### Scenario: Point on HR

- **WHEN** point moves to the HR line
- **THEN** the selected window shows `---` (raw)
- **AND** the unicode bar reappears when point leaves

### Requirement: HR narrowing-resilient discovery and teardown

`:collect` SHALL widen for its scan; bulk-cleanup widens. The
`(narrow → rebuild → widen → rebuild)` sequence SHALL converge.

#### Scenario: Narrowed rebuild

- **GIVEN** a buffer with three HRs narrowed to the second
- **WHEN** the engine rebuilds, then widens, then rebuilds
- **THEN** the final overlay set matches a fresh widened rebuild

<!-- ── Links decorator ────────────────────────────────────────────── -->

### Requirement: Links decorator registration and toggle

The system SHALL register a decorator named `links` that decorates
Markdown / GFM links with per-window overlays, replacing the bracket
scaffolding with a title label and a host-aware icon. The decorator
SHALL be enabled iff `markdown-hide-urls` is non-nil, and SHALL
auto-toggle when `markdown-hide-urls` changes.

#### Scenario: hide-urls on

- **GIVEN** `(setq-local markdown-hide-urls t)`
- **WHEN** `gfm-pretty-mode` enables
- **THEN** the links decorator's enable bit is t
- **AND** link overlays render

#### Scenario: hide-urls off

- **GIVEN** `(setq-local markdown-hide-urls nil)`
- **THEN** the links decorator's enable bit is nil
- **AND** link source renders raw

### Requirement: Link shape discovery

`:collect` SHALL recognise inline links `[title](url)`, reference
links `[title][label]`, autolinks `<url>`, bare URLs, and
wiki-style links. Image links (`![alt](url)`), reference definition
lines (`[label]: url`), and footnote markers (`[^id]`) SHALL be
excluded.

#### Scenario: Inline link

- **GIVEN** `[Anthropic](https://anthropic.com)`
- **THEN** `:collect` returns one link with title `Anthropic` and url
  `https://anthropic.com`

#### Scenario: Image link excluded

- **GIVEN** `![alt](pic.png)`
- **THEN** `:collect` does not emit a link for it

### Requirement: Title-side overlay rendering

`:apply-display` SHALL replace the `[title]` span (brackets included)
with the title text in `gfm-pretty-links-title-face` (default
`markdown-link-face`), per window.

#### Scenario: Title rendering

- **GIVEN** `[Anthropic](https://anthropic.com)`
- **THEN** the title-side overlay displays `Anthropic` in
  `markdown-link-face`

### Requirement: URL-side icon rendering

`:apply-display` SHALL replace the URL span (`(url)`, `[label]`, the
autolink span, …) with a single nerd-icons glyph resolved from the
target host or scheme. When `nerd-icons` is unavailable, the URL-side
overlay SHALL be omitted (URL shows raw).

#### Scenario: Github URL

- **GIVEN** `[code](https://github.com/user/repo)` and `nerd-icons`
  available
- **THEN** the URL-side renders as the GitHub icon

#### Scenario: Unknown host

- **GIVEN** a link to an unrecognised host
- **THEN** the URL-side renders as a generic web icon

### Requirement: Reference link resolution

Reference links SHALL resolve through a buffer-local definition alist
keyed on the lower-cased label. Unresolved labels SHALL fall back to
rendering the title side only (no icon).

#### Scenario: Resolved reference

- **GIVEN** `[name][label]\n\n[label]: https://example.com`
- **THEN** the URL-side resolves via the alist and renders the
  example.com icon

### Requirement: Suppression of built-in URL composition

The links decorator's `:on-enable` SHALL install `:around` advice on
`markdown-fontify-inline-links` and `markdown-fontify-reference-links`
suppressing markdown-mode's `markdown-hide-urls` compose-region URL
collapse. The advice SHALL be gated on the decorator's enable bit so
disabling the decorator (without disabling `gfm-pretty-mode`) restores
markdown-mode's collapse.

#### Scenario: Advice gated on decorator

- **GIVEN** `markdown-hide-urls` is t but the links decorator's enable
  bit is nil
- **THEN** markdown-mode's compose-region collapse runs unchanged

### Requirement: Whole-link cursor reveal

The links decorator's `:revealable-p` SHALL group the title-side and
URL-side overlays via a shared `gfm-pretty-links-id` property. When
point lies anywhere inside either span, the engine SHALL hide BOTH
overlays in the selected window so the raw `[title](url)` shows.

#### Scenario: Point inside title

- **WHEN** point is at the `t` in `[title](url)`
- **THEN** both overlays hide in the selected window
- **AND** other windows continue to show the decoration

### Requirement: RET follows the link when point is on the decoration

The links decorator SHALL install an overlay keymap binding `RET` to
follow the link target via `markdown-follow-thing-at-point`. The
binding SHALL fire when point is on the rendered overlay (not on the
raw revealed source).

#### Scenario: RET on rendered title

- **WHEN** point is on the rendered title and user presses `RET`
- **THEN** the link target opens via `markdown-follow-thing-at-point`

### Requirement: Reference goto-definition via xref

The links decorator's `:on-enable` SHALL register an xref backend on
`xref-backend-functions` that resolves a reference label at point to
its `[label]:` definition line.

#### Scenario: M-. on reference label

- **GIVEN** `[name][label]` and a definition line `[label]: url`
- **WHEN** point is on the rendered reference link and user presses
  `M-.`
- **THEN** xref jumps to the definition line

### Requirement: Eldoc URL exposure

The links decorator's `:on-enable` SHALL install an eldoc
documentation function that surfaces the resolved URL when point is
on a rendered link overlay.

#### Scenario: Eldoc on link

- **WHEN** point is on a rendered link
- **THEN** eldoc displays the resolved URL string

### Requirement: Overlay decoration does not skew column widths

The links decorator SHALL NOT collect or render a link whose source
falls inside a GFM table cell. When the tables decorator's
`:collect` includes a cell containing what looks like a link, the
link decorator SHALL defer — the table cell's natural text content
governs column width.

#### Scenario: Link inside table cell

- **GIVEN** a table cell `[name](url)`
- **THEN** the links decorator does NOT decorate that link
- **AND** the cell's column-width measurement uses the raw text
  width
