## Why

Inline source-range links (`[label](/path/foo.el#L42)` in the middle of
prose) currently render raw — both decorators decline them. The
`gfm-pretty-links` decorator skips every source-range URL on the
assumption that `link-previews` will claim them, but `link-previews`
only claims links that occupy a whole line. The result is bracket-and-
parens markdown bleeding through inline, which the pretty-links
decorator was built to hide. Reference-style source-range links
(`[label][src]` with `[src]: /path#L42`) hit the same gap.

## What Changes

- `gfm-pretty-links` SHALL decorate inline `[label](url)` source-range
  links (URLs matching `<path>#L<n>[-L<n>]`) using the `file` class:
  prettified title, file-icon URL overlay, hidden URL span.
- The "skip source-range / diff URLs" rule SHALL narrow to *only*
  inline-shaped `[label](url)` source-range links that occupy a
  standalone line. Diff URLs SHALL remain unconditionally skipped.
  Reference / shortcut source-range links SHALL always be decorated.
- `gfm-pretty` SHALL expose a generic `gfm-pretty-standalone-span-p`
  helper. `gfm-pretty-link-previews` SHALL delegate its standalone
  check to that helper; `gfm-pretty-links` SHALL consult the same
  helper from its skip predicate.
- The file-class RET handler SHALL parse an optional `#L<n>[-L<n>]`
  suffix on the URL and, after `find-file`, jump point to line `<n>`.
- The URL-side icon resolver SHALL strip a `#…` fragment from the
  URL before computing the basename, so `nerd-icons-icon-for-file`
  resolves the major-mode glyph for `foo.el#L42` as it would for
  `foo.el`.

## Capabilities

### Modified Capabilities

- `gfm-pretty`: requirement *Link decoration skips source-range and
  diff URL forms* changes — title and body update to "defers
  source-range and diff URL forms". Source-range skip narrows to
  inline kind on standalone lines; reference-style and inline-in-
  prose source-range links now get normal `file` decoration. Scenario
  *Reference link to source-range URL skipped* (L2052) is removed
  and replaced with the inverse assertion.
- `gfm-pretty`: requirement *URL-side icon rendering* gains a
  fragment-stripping clause for `file`-class basenames.
- `gfm-pretty`: requirement *RET follows the link when point is on
  the decoration* — file-class row updates to honour an optional
  `#L<n>[-L<n>]` URL suffix via `goto-line` after `find-file`.
- `gfm-pretty`: requirement *Umbrella minor mode* (engine) gains a
  public helper `gfm-pretty-standalone-span-p` (generic document
  layout predicate shared by decorators).

## Impact

- Code: `lisp/gfm/gfm-pretty.el` (new helper),
  `lisp/gfm/gfm-pretty-links.el` (skip predicate, icon resolver, RET
  file handler, decorate-link guard for `file` source-range URLs),
  `lisp/gfm/gfm-pretty-link-previews.el` (delegate standalone check
  to engine helper).
- Tests: `lisp/gfm/gfm-pretty-links-tests.el` — inline source-range
  decorated, standalone source-range deferred (incl. list/blockquote
  markers), reference-style source-range decorated, icon resolver
  strips fragment, RET on inline source-range jumps to line.
  Narrowing-regression block (per `AGENTS.md`) covering inline
  source-range overlays under narrow → rebuild → widen → rebuild.
- No new external deps. No keymap, eldoc, or xref API changes —
  those consume overlay metadata that the new path already populates.
