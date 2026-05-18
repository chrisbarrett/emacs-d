## REMOVED Requirements

### Requirement: Callout mode toggle

**Reason:** Visual decoration moves to `gfm-pretty`. Per-decorator toggle
is now `(gfm-pretty-toggle-decorator 'callouts)`; the umbrella
`gfm-pretty-mode` enables all decorators by default.
**Migration:** See `gfm-pretty/spec.md` requirement "Callouts decorator
registration and toggle".

### Requirement: Callout block discovery

**Reason:** Block discovery is the callouts decorator's `:collect` /
`:range` contribution to the engine.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout block
discovery".

### Requirement: Callout bordered-block rendering

**Reason:** Overlay rendering moves with the callouts decorator.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout
bordered-block rendering".

### Requirement: Callout box width sizing

**Reason:** Width sizing is a callouts-decorator display contract; the
engine provides the width primitive via
`gfm-pretty-available-width`.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout box width
sizing".

### Requirement: Callout wrapped right-edge alignment

**Reason:** Wrap-alignment uses engine primitive
`gfm-pretty-simulate-wrap`; behaviour preserved.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout wrapped
right-edge alignment".

### Requirement: Callout marker line and body prefix reveal

**Reason:** Reveal protocol is engine-owned (`post-command-hook`); the
callouts decorator declares `:revealable-p`.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout marker
line and body prefix reveal".

### Requirement: Callout per-window rendering

**Reason:** Per-window display is an engine invariant applied uniformly
to every decorator.
**Migration:** See `gfm-pretty/spec.md` requirement "Per-window
decorator rendering" (cross-cutting).

### Requirement: Callout per-window cursor reveal

**Reason:** Reveal scoping is engine-owned.
**Migration:** See `gfm-pretty/spec.md` requirement "Per-window cursor
reveal" (cross-cutting).

### Requirement: Callout block-discovery cache

**Reason:** Discovery caching keyed on `buffer-chars-modified-tick` is
preserved per decorator under the engine.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout
block-discovery cache".

### Requirement: Callout narrowing-resilient discovery and teardown

**Reason:** Narrowing safety is the load-bearing engine contract;
behaviour preserved verbatim.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout
narrowing-resilient discovery and teardown".

### Requirement: Callout debounced rebuild

**Reason:** Engine owns one idle timer servicing all decorators; the
0.2s debounce is preserved.
**Migration:** See `gfm-pretty/spec.md` requirement "Debounced rebuild
scheduler" (cross-cutting).

### Requirement: Callout scoped post-edit rebuild

**Reason:** Engine consults each decorator's
`:apply-anchors` / `:apply-display` over the dirty region.
**Migration:** See `gfm-pretty/spec.md` requirement "Callout scoped
post-edit rebuild".

### Requirement: Callout selective per-window reconciliation

**Reason:** Engine-owned reconciler iterates registered decorators.
**Migration:** See `gfm-pretty/spec.md` requirement "Selective
per-window reconciliation" (cross-cutting).

### Requirement: Callout visible-first prioritised rebuild

**Reason:** Visible-first ordering is engine-level pacing applied to all
decorators.
**Migration:** See `gfm-pretty/spec.md` requirement "Visible-first
prioritised rebuild" (cross-cutting).

### Requirement: Code-fence mode toggle

**Reason:** Moves to the `fences` decorator under `gfm-pretty`.
**Migration:** See `gfm-pretty/spec.md` requirement "Fences decorator
registration and toggle".

### Requirement: Fenced block discovery

**Reason:** Decorator `:collect` contribution.
**Migration:** See `gfm-pretty/spec.md` requirement "Fenced block
discovery".

### Requirement: YAML helmet discovery

**Reason:** Part of fences decorator's block coverage.
**Migration:** See `gfm-pretty/spec.md` requirement "YAML helmet
discovery".

### Requirement: Indented block discovery

**Reason:** Part of fences decorator's block coverage.
**Migration:** See `gfm-pretty/spec.md` requirement "Indented block
discovery".

### Requirement: Mutual exclusion of indent inside fence

**Reason:** Internal coordination inside the fences decorator.
**Migration:** See `gfm-pretty/spec.md` requirement "Mutual exclusion of
indent inside fence".

### Requirement: Code-fence bordered-block rendering

**Reason:** Decorator `:apply-display` contribution.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence
bordered-block rendering".

### Requirement: Code-fence body background fill

**Reason:** Decorator-level rendering detail; preserved.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence body
background fill".

### Requirement: Code-fence box width sizing

**Reason:** Uses engine width primitive.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence box
width sizing".

### Requirement: Wrap simulation always terminates

**Reason:** Engine primitive `gfm-pretty-simulate-wrap` carries this
invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Wrap simulation
always terminates" (engine-level).

### Requirement: Code-fence marker line reveal

**Reason:** Engine reveal + decorator `:revealable-p`.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence marker
line reveal".

### Requirement: Language icon resolution

**Reason:** Fences-decorator concern; reads `markdown-code-lang-modes`
from `lang-markdown`.
**Migration:** See `gfm-pretty/spec.md` requirement "Language icon
resolution".

### Requirement: YAML body fontification

**Reason:** Fences-decorator concern.
**Migration:** See `gfm-pretty/spec.md` requirement "YAML body
fontification".

### Requirement: Code-fence per-window rendering

**Reason:** Subsumed by engine cross-cutting per-window contract.
**Migration:** See `gfm-pretty/spec.md` requirement "Per-window
decorator rendering" (cross-cutting).

### Requirement: Code-fence block-discovery cache

**Reason:** Per-decorator cache preserved.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence
block-discovery cache".

### Requirement: Code-fence narrowing-resilient discovery and teardown

**Reason:** Engine narrowing-safety contract applies uniformly.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence
narrowing-resilient discovery and teardown".

### Requirement: Code-fence debounced rebuild

**Reason:** Subsumed by engine scheduler.
**Migration:** See `gfm-pretty/spec.md` requirement "Debounced rebuild
scheduler" (cross-cutting).

### Requirement: Code-fence scoped post-edit rebuild

**Reason:** Per-decorator scoping retained under engine.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence scoped
post-edit rebuild".

### Requirement: Code-fence selective per-window reconciliation

**Reason:** Subsumed by engine reconciler.
**Migration:** See `gfm-pretty/spec.md` requirement "Selective
per-window reconciliation" (cross-cutting).

### Requirement: Code-fence visible-first prioritised rebuild

**Reason:** Subsumed by engine pacing.
**Migration:** See `gfm-pretty/spec.md` requirement "Visible-first
prioritised rebuild" (cross-cutting).

### Requirement: Code-fence performance instrumentation

**Reason:** Decorator-level stats; moved verbatim.
**Migration:** See `gfm-pretty/spec.md` requirement "Code-fence
performance instrumentation".

### Requirement: Table mode toggle

**Reason:** Moves to the `tables` decorator under `gfm-pretty`.
**Migration:** See `gfm-pretty/spec.md` requirement "Tables decorator
registration and toggle".

### Requirement: Table block discovery

**Reason:** Decorator `:collect` contribution.
**Migration:** See `gfm-pretty/spec.md` requirement "Table block
discovery".

### Requirement: Cell parser

**Reason:** Tables-decorator internal contract; preserved.
**Migration:** See `gfm-pretty/spec.md` requirement "Cell parser".

### Requirement: Column width normalisation

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Column width
normalisation".

### Requirement: Auto-composition does not skew column widths

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Auto-composition
does not skew column widths".

### Requirement: Window-fitted column widths

**Reason:** Tables-decorator rendering invariant using engine width
primitive.
**Migration:** See `gfm-pretty/spec.md` requirement "Window-fitted
column widths".

### Requirement: Cell wrapping for capped columns

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Cell wrapping for
capped columns".

### Requirement: Cell-edit commit preserves the row

**Reason:** Tables-decorator editing invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Cell-edit commit
preserves the row".

### Requirement: Table border and rule decoration

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Table border and
rule decoration".

### Requirement: Exterior pipe rendering

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Exterior pipe
rendering".

### Requirement: Interior column gap rendering

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Interior column
gap rendering".

### Requirement: Header emphasis

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Header emphasis".

### Requirement: Body row zebra striping

**Reason:** Tables-decorator rendering invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Body row zebra
striping".

### Requirement: Stripe face

**Reason:** Tables-decorator face contract.
**Migration:** See `gfm-pretty/spec.md` requirement "Stripe face".

### Requirement: Active-cell highlight

**Reason:** Tables-decorator cursor-aware rendering.
**Migration:** See `gfm-pretty/spec.md` requirement "Active-cell
highlight".

### Requirement: Cursor anchoring inside cells

**Reason:** Tables-decorator cursor invariant.
**Migration:** See `gfm-pretty/spec.md` requirement "Cursor anchoring
inside cells".

### Requirement: Cell-entry key hints

**Reason:** Tables-decorator key-binding contract.
**Migration:** See `gfm-pretty/spec.md` requirement "Cell-entry key
hints".

### Requirement: Structural cell motion

**Reason:** Tables-decorator command set.
**Migration:** See `gfm-pretty/spec.md` requirement "Structural cell
motion".

### Requirement: Snap-to-cell on row entry

**Reason:** Tables-decorator cursor behaviour.
**Migration:** See `gfm-pretty/spec.md` requirement "Snap-to-cell on
row entry".

### Requirement: In-place edit commands divert to indirect editor

**Reason:** Tables-decorator editing model.
**Migration:** See `gfm-pretty/spec.md` requirement "In-place edit
commands divert to indirect editor".

### Requirement: Cell-only indirect edit

**Reason:** Tables-decorator editing model.
**Migration:** See `gfm-pretty/spec.md` requirement "Cell-only
indirect edit".

### Requirement: Whole-table indirect edit

**Reason:** Tables-decorator editing model.
**Migration:** See `gfm-pretty/spec.md` requirement "Whole-table
indirect edit".

### Requirement: Header column swap

**Reason:** Tables-decorator structural command.
**Migration:** See `gfm-pretty/spec.md` requirement "Header column
swap".

### Requirement: Table debounced rebuild

**Reason:** Subsumed by engine scheduler; behaviour preserved.
**Migration:** See `gfm-pretty/spec.md` requirement "Debounced rebuild
scheduler" (cross-cutting).

### Requirement: Per-window table rendering

**Reason:** Subsumed by engine cross-cutting per-window contract.
**Migration:** See `gfm-pretty/spec.md` requirement "Per-window
decorator rendering" (cross-cutting).

### Requirement: Table prioritised window rebuild

**Reason:** Subsumed by engine pacing.
**Migration:** See `gfm-pretty/spec.md` requirement "Visible-first
prioritised rebuild" (cross-cutting).

### Requirement: Table selective per-window reconciliation

**Reason:** Subsumed by engine reconciler.
**Migration:** See `gfm-pretty/spec.md` requirement "Selective
per-window reconciliation" (cross-cutting).

### Requirement: Table performance instrumentation

**Reason:** Decorator-level stats; moved verbatim.
**Migration:** See `gfm-pretty/spec.md` requirement "Table performance
instrumentation".

### Requirement: Theme change responsiveness

**Reason:** Decorator-level theme hook moved into callouts decorator's
`:on-enable` / `:on-disable`.
**Migration:** See `gfm-pretty/spec.md` requirement "Theme change
responsiveness".

### Requirement: Table narrowing-resilient discovery and teardown

**Reason:** Engine narrowing-safety contract applies uniformly.
**Migration:** See `gfm-pretty/spec.md` requirement "Table
narrowing-resilient discovery and teardown".

### Requirement: Link mode toggle

**Reason:** Moves to the `links` decorator under `gfm-pretty`.
**Migration:** See `gfm-pretty/spec.md` requirement "Links decorator
registration and toggle".

### Requirement: Link shape discovery

**Reason:** Decorator `:collect` contribution.
**Migration:** See `gfm-pretty/spec.md` requirement "Link shape
discovery".

### Requirement: Title-side overlay rendering

**Reason:** Decorator `:apply-display` contribution.
**Migration:** See `gfm-pretty/spec.md` requirement "Title-side
overlay rendering".

### Requirement: URL-side icon rendering

**Reason:** Decorator `:apply-display` contribution.
**Migration:** See `gfm-pretty/spec.md` requirement "URL-side icon
rendering".

### Requirement: Reference link resolution

**Reason:** Decorator-level resolution policy.
**Migration:** See `gfm-pretty/spec.md` requirement "Reference link
resolution".

### Requirement: Suppression of built-in URL composition

**Reason:** Decorator `:on-enable` installs the advice gated on its
enable bit.
**Migration:** See `gfm-pretty/spec.md` requirement "Suppression of
built-in URL composition".

### Requirement: Whole-link cursor reveal

**Reason:** Decorator-specific reveal grouping; preserved via decorator
`:revealable-p`.
**Migration:** See `gfm-pretty/spec.md` requirement "Whole-link cursor
reveal".

### Requirement: RET follows the link when point is on the decoration

**Reason:** Decorator-level keymap on overlays.
**Migration:** See `gfm-pretty/spec.md` requirement "RET follows the
link when point is on the decoration".

### Requirement: Reference goto-definition via xref

**Reason:** Decorator `:on-enable` installs the xref backend.
**Migration:** See `gfm-pretty/spec.md` requirement "Reference
goto-definition via xref".

### Requirement: Eldoc URL exposure

**Reason:** Decorator `:on-enable` installs the eldoc function.
**Migration:** See `gfm-pretty/spec.md` requirement "Eldoc URL
exposure".

### Requirement: Overlay decoration does not skew column widths

**Reason:** Decorator-level interaction with tables decorator.
**Migration:** See `gfm-pretty/spec.md` requirement "Overlay
decoration does not skew column widths".

### Requirement: Per-window link rendering

**Reason:** Subsumed by engine cross-cutting per-window contract.
**Migration:** See `gfm-pretty/spec.md` requirement "Per-window
decorator rendering" (cross-cutting).

### Requirement: Link debounced rebuild

**Reason:** Subsumed by engine scheduler.
**Migration:** See `gfm-pretty/spec.md` requirement "Debounced rebuild
scheduler" (cross-cutting).

### Requirement: HR mode toggle

**Reason:** Moves to the `hrule` decorator under `gfm-pretty`.
**Migration:** See `gfm-pretty/spec.md` requirement "Hrule decorator
registration and toggle".

### Requirement: HR block discovery

**Reason:** Decorator `:collect` contribution reading the `markdown-hr`
text property.
**Migration:** See `gfm-pretty/spec.md` requirement "HR block
discovery".

### Requirement: HR rendering

**Reason:** Decorator `:apply-display` contribution.
**Migration:** See `gfm-pretty/spec.md` requirement "HR rendering".

### Requirement: HR per-window rendering

**Reason:** Subsumed by engine cross-cutting per-window contract.
**Migration:** See `gfm-pretty/spec.md` requirement "Per-window
decorator rendering" (cross-cutting).

### Requirement: HR cursor reveal

**Reason:** Engine reveal + decorator `:revealable-p`.
**Migration:** See `gfm-pretty/spec.md` requirement "HR cursor reveal".

### Requirement: HR debounced rebuild

**Reason:** Subsumed by engine scheduler.
**Migration:** See `gfm-pretty/spec.md` requirement "Debounced rebuild
scheduler" (cross-cutting).

### Requirement: HR narrowing-resilient discovery and teardown

**Reason:** Engine narrowing-safety contract applies uniformly.
**Migration:** See `gfm-pretty/spec.md` requirement "HR
narrowing-resilient discovery and teardown".

## ADDED Requirements

### Requirement: Visual behaviour delegated to gfm-pretty

The `lang-markdown` axis SHALL NOT define visual-behaviour requirements
for GFM buffers. Overlay decoration, font-lock callout fontification,
callout faces, header-face weight styling, and
`markdown-blockquote-face` neutralisation are defined exclusively under
the `gfm-pretty` axis.

`modules/lang-markdown/` retains responsibility for:

- File associations (`auto-mode-alist`) routing `.md` /
  `.markdown` / `.mkd` / `.mdown` / `.mkdn` and `/prompt` files to
  `gfm-mode`.
- `major-mode-remap-alist` mapping `markdown-mode` → `gfm-mode`.
- `markdown-code-lang-modes` enumeration (used by `markdown-mode` for
  native fontification AND read by the `gfm-pretty` fences decorator
  for icon lookup; the variable lives in `markdown-mode` but its value
  is configured here).
- `apheleia` formatter wiring for `gfm-mode-local-vars-hook`
  (`deno-markdown` or `prettier-markdown`).
- `+markdown-tab-dwim` command and its insert-state binding.
- `markdown-get-lang-mode` memoise advice (`+markdown--memoise-lang-mode`).
- `markdown-syntax-propertize-extend-region` clamp advice
  (`+markdown--clamp-extend-region`).
- Local-leader bindings for `markdown-toggle-url-hiding`,
  `markdown-insert-footnote`, and `markdown-narrow-to-subtree`.

#### Scenario: Opening a markdown file routes to gfm-mode

- **WHEN** the user opens a file named `notes.md`
- **THEN** `gfm-mode` is the major mode
- **AND** `gfm-pretty-mode` is enabled via `gfm-mode-hook` (defined in
  `gfm-pretty`)

#### Scenario: Visual decoration concerns are out of scope here

- **GIVEN** a proposal adding a new GFM overlay decorator
- **WHEN** spec placement is decided
- **THEN** the requirement SHALL be filed under
  `openspec/specs/gfm-pretty/spec.md`
- **AND** `openspec/specs/lang-markdown/spec.md` SHALL NOT gain a
  visual-behaviour requirement

#### Scenario: Formatter wiring stays here

- **GIVEN** an edit to the `apheleia-formatters` registration for
  markdown
- **WHEN** spec placement is decided
- **THEN** the change targets `openspec/specs/lang-markdown/spec.md`
