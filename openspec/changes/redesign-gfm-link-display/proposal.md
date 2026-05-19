## Why

The `gfm-pretty-links` decorator currently reveals the raw `[title](url)`
source whenever point enters a decorated link. That fights reading flow:
moving the cursor across a paragraph causes layout reflow as overlays
collapse and re-expand. It also conflates "I want to inspect the URL"
with "I want to edit it" — the only way to *see* the target without
disturbing rendering is hover-via-eldoc, which today returns just the
URL string with no markup context.

A second gap: every decorated link looks like a web link. Buffer-internal
anchors (`#heading`) and project-relative file paths (`./scripts/foo.sh`,
`/etc/x`) are visually indistinguishable from external URLs, and `RET`
on them runs `markdown--browse-url` — which works for anchors only by
luck and fails for paths.

## What Changes

- **BREAKING** Remove cursor-driven whole-link reveal. The
  `:reveal-fn`, `gfm-pretty-links-revealable` overlay property, and
  `gfm-pretty-links-saved-display` machinery come off — decorated links
  stay decorated regardless of point position.
- Replace the eldoc handler so it returns the raw `[title](url)` source
  with shadow on bracket / paren scaffolding, `markdown-link-face` on
  the title, and `markdown-url-face` on the URL.
- Classify each link's URL into one of three target classes — **web**,
  **anchor**, **file** — driven by the URL prefix on the resolved
  target (so reference-style links classify via their `[label]:`
  definition's URL, not the source shape).
- Add two faces: `gfm-pretty-links-anchor-face` and
  `gfm-pretty-links-file-face`, both inheriting `markdown-link-face`
  with `:underline nil`. Title-side overlays of anchor / file links
  use the matching face instead of `markdown-link-face`.
- Suppress the url-side icon overlay for anchor and file links —
  title-only rendering.
- `RET` on a decorated link dispatches by target class: anchors jump
  to the matching heading in-buffer, file paths open via `find-file`
  (relative to the buffer's directory), web URLs go through
  `markdown--browse-url`.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `gfm-pretty`: link decorator requirements change — title-side face
  varies by URL class, url-side icon is suppressed for local targets,
  whole-link reveal is removed, RET dispatches per class, eldoc
  returns formatted source.

## Impact

- `lisp/gfm/gfm-pretty-links.el` — URL classifier, two new faces,
  per-class title-side face selection, url-side suppression for local
  targets, reveal teardown, RET dispatch, eldoc formatter.
- `lisp/gfm/gfm-pretty-engine.el` — no signature changes; the engine's
  default reveal path simply finds no `gfm-pretty-links-revealable`
  overlays to act on.
- `openspec/specs/gfm-pretty/spec.md` — five `### Requirement:` entries
  modified, one removed.
- `lisp/gfm/gfm-pretty-tests.el` — reveal-coverage tests retired;
  new tests for classifier, face selection, RET dispatch, eldoc
  formatting.
- No external dependencies change. `nerd-icons` remains optional.
