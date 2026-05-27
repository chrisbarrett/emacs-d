## Why

Callout box borders misalign when body lines contain prettified inline
links: the engine measures line widths in raw buffer characters, so a
body line like `> [proposal](/very/long/path.md#L1-L36),` is sized as
~75 cols even though its rendered visual width (after the `links`
decorator collapses the URL into an icon + label) is more like 15 cols.
The box ends up clamped to the window edge with ~60 cols of empty
tinted band, or the overflow branch fires and the right `│` lands on a
phantom wrapped row that does not actually exist.

The root cause is engine-wide, not callouts-specific: the decorator
iteration order is load-order-dependent (a flat alist iterated by
`dolist`), and the width helpers (`gfm-pretty--max-line-width`,
`gfm-pretty--last-visual-col`) operate on raw chars and raw substrings.
Any width-measuring decorator (callouts, fences, tables) can race any
width-changing decorator (links today; `prettify-symbols-mode` or
similar tomorrow), and even when ordering is correct the measurement
itself reads through `display` overlays as if they were not there.

## What Changes

- **BREAKING** Add `:phase` slot to `gfm-pretty-define-decorator` with
  enum `(atoms containers overlays)`. Engine iterates decorators
  phase-ordered (`atoms` → `containers` → `overlays`) at every
  dispatch site. Existing decorators declare their phase.
- **BREAKING** Delete `gfm-pretty--max-line-width`. Callers migrate to
  the new visual-width primitive.
- Add `gfm-pretty--visual-line-width LBEG LEND` engine primitive: walks
  overlays + text-props, returns visual cell count honouring
  `display` substitutions and `invisible`.
- Add `gfm-pretty--visual-max-line-width BEG END &optional INDENT`.
- Atom-phase decorators tag their visual-width-changing display
  overlays / text-props with `gfm-pretty-atomic t`.
- `gfm-pretty--simulate-wrap` honours `gfm-pretty-atomic`: spaces
  inside an atomic span are not wrap points.
- Callouts width math (`box-width` clamp, per-body-line overflow check,
  right-edge alignment) switches to the visual primitives.
- Fences and tables width math audited and switched where the same
  shape applies.
- Links declares `:phase 'atoms` and tags its display overlays atomic.
- Blockquotes, hrule, link-previews declare `:phase 'overlays`.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: decorator registration grows the `:phase` contract;
  the engine guarantees phase-ordered iteration; visual-width
  primitives replace raw-char ones; callouts, fences, and tables
  measure visual width; atom-phase decorators mark display
  contributions atomic for wrap simulation.

## Impact

- `lisp/gfm/gfm-pretty-engine.el` — phase slot on decorator struct,
  phase-ordered iteration in every dispatch path, visual-width
  primitives, wrap-sim atomic-span handling, deletion of the
  raw-char `gfm-pretty--max-line-width`.
- `lisp/gfm/gfm-pretty-callouts.el` — `:phase 'containers`, switch
  to visual width math, short-circuit non-overflow path when visual
  width fits in budget.
- `lisp/gfm/gfm-pretty-fences.el` — `:phase 'containers`, audit and
  switch width math.
- `lisp/gfm/gfm-pretty-tables.el` — `:phase 'containers`, audit and
  switch width math.
- `lisp/gfm/gfm-pretty-links.el` — `:phase 'atoms`, tag display
  overlays / text-props `gfm-pretty-atomic`.
- `lisp/gfm/gfm-pretty-blockquotes.el`,
  `lisp/gfm/gfm-pretty-hrule.el`,
  `lisp/gfm/gfm-pretty-link-previews.el` — `:phase 'overlays`.
- `openspec/specs/gfm-pretty/spec.md` — delta in this change updates
  the decorator registration requirement (adds `:phase`), the
  rendering primitives requirement (renames the public width
  helper), and the callout box width sizing requirement (mandates
  visual-width measurement).
- No external callers — `gfm-pretty--max-line-width` is private.
