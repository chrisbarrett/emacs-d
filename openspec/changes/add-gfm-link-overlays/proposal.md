## Why

When `markdown-hide-urls` is on, markdown-mode replaces the URL portion
of `[title](url)` with a single composition glyph but leaves the
square-bracket scaffolding around `title` intact. The result reads as
`[title]▷` — visually noisy and inconsistent with how every other GFM
construct (callouts, code fences, tables) is rendered through richer
overlay decoration in this config. Reference-link navigation also
conflates "go to the URL" with "go to the definition" under one
command. We want a link reading experience that matches the rest of the
GFM module family: brackets evaporate, the URL becomes a host-aware
icon, and RET / goto-definition split semantics cleanly.

## What Changes

- Add a new buffer-local minor mode `gfm-links-mode` (under
  `modules/lang-markdown/lib/+gfm-links.el`) that decorates inline,
  reference (full / collapsed / shortcut), autolink, bare-URL, and
  wiki-link forms with per-window overlays whenever
  `markdown-hide-urls` is on.
- Title-side overlay: replace `[title]` (brackets included) with
  `title` rendered in `markdown-link-face`. For autolinks and bare
  URLs which have no `[title]`, the visible label is the host
  extracted from the URL.
- URL-side overlay: replace the URL region (`(url)`, `[label]`, or
  the autolink/bare-URL span itself) with an icon resolved through
  `nerd-icons-icon-for-url` for http(s) targets and
  `nerd-icons-icon-for-file` for relative file paths. No local
  scheme→glyph table; users extend `nerd-icons-url-alist` to
  customise.
- Image links `![alt](url)` are explicitly left untouched — the
  filename / path stays visible.
- Suppress the built-in `compose-region` URL collapsing while
  `gfm-links-mode` is on, via around-advice on
  `markdown-fontify-inline-links` and
  `markdown-fontify-reference-links` that lets the body run with
  `markdown-hide-urls` let-bound to nil. The mode and the built-in
  variable do not interfere with each other.
- Cursor reveal: when point lands anywhere inside a decorated link
  region, the whole link (title + URL) reveals to raw source until
  point leaves. Mirrors `gfm-callouts-mode` / `gfm-code-fences-mode`
  reveal behaviour.
- Echo area: register an `eldoc-documentation-functions` entry that
  returns the resolved URL (and the inline title attribute when
  present) when point is on a decorated link. Do not change
  `eldoc-documentation-strategy`.
- `RET` while point is on a decorated link follows the link via
  `markdown--browse-url`; off a link, RET falls through to
  `markdown-enter-key`. Bound through the overlay's `keymap`
  property so the rebinding is spatially scoped.
- Goto-definition for reference-style links jumps to the
  `[label]: url` definition line. Implemented as an
  `xref-backend-functions` entry so `M-.` works out of the box.
- **BREAKING (internals only)**: extend
  `gfm-tables--visible-width--compute` to walk overlays in the source
  buffer alongside text properties, so cells containing link overlays
  measure correctly. Existing call sites stay working when the new
  arguments default to nil. The accompanying spec requirement
  "Auto-composition does not skew column widths" widens to also
  require overlay-aware measurement; rename it accordingly.

## Capabilities

### New Capabilities

(none — this folds into the existing `lang-markdown` axis per the
one-spec-per-axis rule.)

### Modified Capabilities

- `lang-markdown`: adds requirements covering the new `gfm-links-mode`
  (mode toggle, link-shape discovery, icon resolution, reference-def
  resolution, cursor reveal, RET semantics, goto-definition, eldoc
  exposure, suppression of the built-in compose path), and widens the
  existing column-width measurement requirement to be overlay-aware.

## Impact

- New file: `modules/lang-markdown/lib/+gfm-links.el`.
- Modified file: `modules/lang-markdown/lib/+gfm-tables.el`
  (`gfm-tables--visible-width` / `--compute` gain optional source-buffer
  + region parameters; `gfm-tables--fontify-cell` measures in the
  source buffer rather than the scratch).
- Modified file: `modules/lang-markdown/init.el`
  (load + autoload registration for the new mode; wiring to
  `markdown-mode-hook` so the mode follows `markdown-hide-urls`).
- Modified file: `modules/lang-markdown/tests.el` (per-shape decoration
  tests, ref-def resolution tests, width-walker overlay-path test,
  narrowing-regression suite entry for the new mode).
- Modified spec: `openspec/specs/lang-markdown/spec.md`.
- External dependencies: `nerd-icons` (already a dependency via
  `nerd-icons-dired`); no new packages.
- No CI / build changes. `make test` is the single entry point for the
  full check suite.
