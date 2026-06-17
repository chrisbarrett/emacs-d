## Why

Agents and humans routinely write a source-range reference wrapped in brackets — `[/Users/chris/src/.../pattern.rs#L234-L267]` — or in angle brackets — `</Users/.../pattern.rs#L234-L267>`. Both read as "this is a reference", but neither is valid markdown: `[foo]` with no `(url)` or `[foo]: …` definition is a CommonMark shortcut-reference link with no referent, and `<…>` is only an autolink when its content is a recognised URI scheme. GFM renders both as literal text, and the existing `link-previews` decorator recognises only the unbracketed bare-line token (`path#L1-L2`) and the full markdown-link form (`[label](url)`). The most natural-looking reference shapes get no box-bordered preview.

## What Changes

- Extend the `link-previews` decorator's bare-line pass to recognise two new standalone wrapper shapes around an existing source-range / diff-range URL token:
  - **bracket-wrapped**: `[<url>]` on its own line (optional leading indent / list-marker / blockquote-marker).
  - **angle-wrapped**: `<<url>>` on its own line under the same prefix rules.
- The wrapped token's inner content MUST parse as a source-range URL (`<path>#L<a>[-L<b>]`) or diff-range URL (`diff:<base>...<head>[#<path>]`); the existing `/`-in-path gate and preformatted-context exclusions apply unchanged.
- The overlay covers the full wrapper span (including the surrounding `[ ]` / `< >`), and reuses the existing payload, render pipeline, RET-follow dispatch, and broken-preview sentinels — wrapped matches produce overlays indistinguishable from bare and bracketed-link matches.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: the `link-previews` decorator's bare-line discovery requirement gains two recognised wrapper shapes (`[<url>]`, `<<url>>`) alongside the existing unwrapped bare-line token, with the same standalone gating, `/`-in-path gate, and preformatted-context exclusions.

## Impact

- **Code**: `lisp/gfm/gfm-pretty-link-previews.el` — extend the bare-line pass in `--collect-blocks` (and/or a token-unwrap helper) to strip a single matched `[ ]` or `< >` wrapper before parsing, and widen the overlay range to the full wrapper span.
- **Tests**: `lisp/gfm/gfm-pretty-link-previews-tests.el` — add coverage for bracket- and angle-wrapped recognition, marker-prefixed variants, the `/`-gate, fence/indent/inline-code exclusion, link-reference-definition non-collision (`[…]: url`), and overlay-range-covers-wrapper.
- **Specs**: delta to `openspec/specs/gfm-pretty/spec.md` extending the bare-line discovery and standalone-gating requirements.
- **Out of scope**: paren-only wrappers (`(url)`), hyperlinking refs without a `#L<n>` anchor, and any change to the unwrapped bare-line or `[label](url)` behaviour.
