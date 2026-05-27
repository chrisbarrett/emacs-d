## Why

`gfm-pretty-links` hides the URL span for `web` (icon) and `anchor` (empty
display) classes, but leaves `file`-class URLs rendered raw. Readers of
markdown buffers see `[`pretty label`](../../long/relative/path.hcl)` with
the parens-wrapped path bleeding through next to the prettified label,
which defeats the decorator's reading-surface goal. The current commentary
even contradicts itself on this point — L21-25 says file is "title-only",
L535 says the URL renders raw.

## What Changes

- `file`-class links SHALL hide the raw URL span by replacing it with
  a single nerd-icons file-type glyph resolved from the URL's basename
  via `nerd-icons-icon-for-file` (mirroring `web`'s icon treatment).
- When `nerd-icons` is unavailable, the file URL-side overlay SHALL
  fall back to `display` = `""` (URL hidden, no icon).
- `gfm-pretty-links--decorate-link` SHALL create the URL-side overlay for
  `file` (in addition to `web` and `anchor`).
- `gfm-pretty-links--make-overlay` SHALL resolve the URL display for
  `file` via the same icon-resolution path as `web`. `anchor` retains
  its empty-string display.
- Commentary at the top of `gfm-pretty-links.el` and on
  `decorate-link` SHALL be reconciled to describe the new behaviour.
- The title-side overlay SHALL strip a single pair of wrapping
  backticks from the displayed label (e.g. `` `pretty` `` → `pretty`)
  for all link classes. Labels with interior backticks display
  unchanged. The overlay's `gfm-pretty-links-label` metadata SHALL
  retain the original unstripped label.

## Capabilities

### Modified Capabilities

- `gfm-pretty`: requirement *URL-side icon rendering* changes — `file`
  class now creates a URL-side overlay whose `display` is a nerd-icons
  file-type glyph (with `""` fallback when `nerd-icons` is unavailable).
  Scenario *File link omits icon* updates to assert the icon overlay
  exists; a new scenario covers the `nerd-icons`-unavailable fallback.
- `gfm-pretty`: requirement *Title-side overlay rendering* gains
  backtick-stripping behaviour for fully wrapped labels, with two new
  scenarios covering wrap and interior-backtick cases.

## Impact

- Code: `lisp/gfm/gfm-pretty-links.el` (decorate-link guard, make-overlay
  branch, commentary).
- Tests: `lisp/gfm/gfm-pretty-links-tests.el` — extend / add a case for
  `file`-class URL hiding. Narrowing-regression invariant unchanged.
- No keymap, RET, eldoc, or xref behaviour changes — those already read
  overlay properties regardless of `display`.
