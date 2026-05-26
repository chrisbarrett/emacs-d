## Why

`gfm-pretty-links` hides the URL span for `web` (icon) and `anchor` (empty
display) classes, but leaves `file`-class URLs rendered raw. Readers of
markdown buffers see `[`pretty label`](../../long/relative/path.hcl)` with
the parens-wrapped path bleeding through next to the prettified label,
which defeats the decorator's reading-surface goal. The current commentary
even contradicts itself on this point — L21-25 says file is "title-only",
L535 says the URL renders raw.

## What Changes

- `file`-class links SHALL hide the URL span the same way `anchor` does:
  a URL-side overlay with `display` = `""`, metadata preserved.
- `gfm-pretty-links--decorate-link` SHALL create the URL-side overlay for
  `file` (in addition to `web` and `anchor`).
- `gfm-pretty-links--make-overlay` SHALL return `""` for the URL side when
  class is `file` (same branch as `anchor`).
- Commentary at the top of `gfm-pretty-links.el` and on
  `decorate-link` SHALL be reconciled to describe the new behaviour.

## Capabilities

### Modified Capabilities

- `gfm-pretty`: requirement *URL-side icon rendering* changes — `file`
  class now creates an empty-display URL overlay instead of omitting the
  overlay. Scenario *File link omits icon* updates to assert the
  empty-display overlay exists.

## Impact

- Code: `lisp/gfm/gfm-pretty-links.el` (decorate-link guard, make-overlay
  branch, commentary).
- Tests: `lisp/gfm/gfm-pretty-links-tests.el` — extend / add a case for
  `file`-class URL hiding. Narrowing-regression invariant unchanged.
- No keymap, RET, eldoc, or xref behaviour changes — those already read
  overlay properties regardless of `display`.
