## Why

Following a source-range link (`<path>#L<a>-L<b>`) currently
narrows the destination buffer to the requested line range and
paints `gfm-present-focus-face` over the sub-range.  In practice
the narrowing-plus-focus-face combo gets in the way: the reader
loses surrounding context, and a stale focus highlight lingers
until the buffer is killed.  A momentary pulse over the range
signals "here it is" without taking the buffer hostage.

## What Changes

- `gfm-present-follow-link` on a `<path>#L<a>-L<b>` link SHALL open
  the file (widened, no narrowing), place point at the beginning of
  line `<a>`, and pulse lines `<a>..<b>` via `pulsar`.
- The destination buffer SHALL NOT be marked read-only, narrowed,
  or have any persistent overlay applied.
- The reusable narrowed-source renderer and `gfm-present-focus-face`
  are removed.  Nothing else uses them.

## Capabilities

### Modified Capabilities

- `gfm-present`: drops the narrowed-source renderer and the focus
  face; replaces the source-link click action with a pulse-and-goto.

## Impact

- `lisp/gfm/gfm-present.el`:
  - Remove `defface gfm-present-focus-face`.
  - Remove `gfm-present--render-narrowed-source`,
    `gfm-present--cleanup-source-render`,
    `gfm-present--source-overlays`, `gfm-present--source-restorer`.
  - Rewrite `gfm-present--follow-source-link` to open the file
    widened, goto line `<a>`, pulse `<a>..<b>` via
    `pulsar-highlight-pulse`.
- `lisp/gfm/gfm-present-tests.el`:
  - Remove `§13 Detached narrowed-source renderer` test block.
  - Remove `§9 Click escape: source-range link` assertions about
    narrowing, read-only, and focus overlays.  Replace with: file is
    opened, point sits at the start line, no narrowing applied, mark
    pushed at the click site.
  - Remove `focus-face-defined` and `focus-face-no-extend` surface
    tests.
- `openspec/specs/gfm-present/spec.md`: remove the two requirements
  `Reusable narrowed-source renderer` and `Focus highlight bounded
  to text glyphs`; modify the source-link click scenarios in
  `Source-range link follow` (or wherever the click path is spec'd)
  to assert pulse-and-goto.
