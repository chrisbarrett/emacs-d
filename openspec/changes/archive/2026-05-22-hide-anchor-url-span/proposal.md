## Why

Decorated anchor links currently render the `(#slug)` span raw because
the URL-side overlay is intentionally omitted for the `anchor` class
(gfm-pretty/spec.md:1727-1755). The result reads as
`The problem(#the-problem)` instead of the clean `The problem` the
title-side overlay is trying to deliver. The leftover `(#the-problem)`
is visual clutter — every TOC entry in a presentation document carries
a duplicate slug after its label.

The decorator also installs `:around` advice on
`markdown-fontify-inline-links` that suppresses markdown-mode's own
`markdown-hide-urls` collapse (gfm-pretty/spec.md:1774), so the
clutter is not hidden by any existing fallback either.

## What Changes

- The links decorator's `:apply-block-fn` SHALL create a URL-side
  overlay for `anchor`-class links whose `display` property is the
  empty string, hiding the `(#slug)` span from view.
- File-class links continue to render the URL span raw (unchanged).
- Web-class URL-side icon rendering is unchanged.
- RET dispatch on the URL span still finds the link record via the
  overlay's metadata (covered by existing scenarios — overlays carry
  `gfm-pretty-links-url` regardless of `display`).

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `gfm-pretty`: the `URL-side icon rendering` requirement changes
  behaviour for the `anchor` class — overlay is now created with empty
  display instead of being omitted. The "Anchor link omits icon"
  scenario is replaced by "Anchor link hides URL span".

## Impact

- `lisp/gfm/gfm-pretty-links.el` —
  `gfm-pretty-links--decorate-link` extends its class gate to create
  a url-side overlay for `anchor`;
  `gfm-pretty-links--make-overlay` returns an empty `display` string
  when `side` is `url` and `class` is `anchor`.
- `lisp/gfm/gfm-pretty-links-tests.el` — new ERT covering the empty
  display for anchor URL spans; existing "anchor link omits icon"
  test updates to assert overlay-with-empty-display.
- No new dependencies. No key changes. Behaviour change is purely
  visual.
