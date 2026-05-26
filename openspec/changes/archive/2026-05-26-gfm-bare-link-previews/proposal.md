## Why

Agents and humans routinely paste file references as bare lines — e.g. `/Users/chris/src/.../main.tf#L52-L93` standing alone on its own line — but the existing `link-previews` decorator only triggers when the reference is wrapped as a markdown `[label](url)` link.  Readers lose the box-bordered source preview for the most common form references actually appear in.  Bracketing every path by hand defeats the point of having the previews at all.

## What Changes

- Extend the `link-previews` decorator to also recognise standalone bare-line references in `gfm-pretty-mode` buffers:
  - source-range form: `<path>#L<start>[-L<end>]` occupying a whole line (optional leading indent / list-marker / blockquote-marker, same prefixes the bracketed form already accepts).
  - diff-range form: `diff:<base>...<head>[#<path>]` occupying a whole line under the same prefix rules.
- Skip bare-line matches that fall inside a preformatted context: triple-backtick fences, GFM indented code blocks, or wrapped in a single inline-code span (`` `…` `` on its own line).
- Reuse the existing preview rendering, overlay registry, RET-follow dispatch, and broken-preview sentinels — bare-line matches produce overlays indistinguishable from bracketed matches.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: the `link-previews` decorator gains a second recognised input shape (bare line) alongside the existing bracketed `[label](url)` shape, with preformatted-context exclusion rules.

## Impact

- **Code**: `lisp/gfm/gfm-pretty-link-previews.el` — extend `--collect-blocks` with a bare-line pass and add helpers for preformatted-context detection (fence/indent-block range computation, inline-code wrap check).
- **Tests**: `lisp/gfm/gfm-pretty-link-previews-tests.el` — add coverage for bare-line recognition, marker-prefixed variants, fence/indent-block exclusion, inline-code exclusion, and absolute/`~`/relative path resolution.
- **Specs**: delta to `openspec/specs/gfm-pretty/spec.md` adding bare-line requirements under the existing link-previews section.
- **Out of scope**: hyperlinking bare paths without anchors (no `#L<n>`), and any change to the bracketed form's behaviour.
