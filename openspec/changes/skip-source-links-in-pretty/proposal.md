## Why

`gfm-present-mode` renders `path#L<a>-L<b>` and `diff:<base>...<head>`
links as source/diff previews via its own overlays. Pretty-links is
unaware of these URL forms — it classifies the path-prefixed ones as
`file` and the `diff:` ones as `web` (any scheme), then decorates them
with title-side overlays and overlay keymaps. Two concrete failures
result:

1. **Stacked display overlays.** Pretty-links' title-side overlay and
   gfm-present's preview overlay both carry `display` properties on the
   same buffer region. The visual output garbles — the path appears
   twice, with the fence header starting mid-link
   (live evidence: overlapping overlays at buf positions 976..1061 and
   976..1146 in `2026-05-22T06-33-tg-native-drift-filters.md`).

2. **RET navigation hijack.** Overlay keymaps outrank
   `minor-mode-overriding-map-alist`, so pretty-links'
   `gfm-pretty-links-follow-link-at-point` fires first. For `file`
   class it `find-file`s the path widened, ignoring `#L<a>-L<b>`
   semantics entirely. `gfm-present-follow-link`'s source/diff branch
   (which narrows to the line range and applies focus overlays) never
   runs.

## What Changes

- The links decorator's `:collect` SHALL skip any link whose resolved
  URL matches a source-range form (`path#L<digits>[-L<digits>]$`) or a
  diff form (`diff:<base>...<head>[#<path>]`). Skipped links produce
  no title-side overlay, no URL-side overlay, and no overlay keymap.
- Effect on display: only gfm-present's preview overlay (when present)
  renders the link.
- Effect on RET: no overlay keymap intercepts, so the buffer's
  active minor-mode RET binding takes over —
  `gfm-present-follow-link` runs in presentation buffers, plain
  `markdown-follow-thing-at-point` runs elsewhere.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `gfm-pretty`: link decoration discovery gains an exclusion for
  source-range and diff URL forms. Same shape as the existing "skip
  code regions" exclusion — discovery filter, not classification
  change.

## Impact

- `lisp/gfm/gfm-pretty-links.el` — new regexes for source-range and
  diff URL forms; predicate consulted in scan helpers (`scan-inline`,
  `scan-reference`, `scan-autolinks`, `scan-bare-urls`).
- `lisp/gfm/gfm-pretty-links-tests.el` — ERT tests asserting these
  URL forms produce no overlay records.
- No new dependencies. No user-facing key changes. Behaviour change
  is additive (more exclusions, no existing-link behaviour altered).
- Unblocks `fontify-source-preview-body` change — without skipping,
  pretty-links' overlay continues to stack on top of any improved
  gfm-present preview.
