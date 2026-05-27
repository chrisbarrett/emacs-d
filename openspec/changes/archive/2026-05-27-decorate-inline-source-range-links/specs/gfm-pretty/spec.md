## ADDED Requirements

### Requirement: Standalone span predicate

The engine SHALL expose a public helper `gfm-pretty-standalone-span-p
(beg end)` that returns non-nil iff the line containing `[beg, end)`,
with the span between `beg` and `end` removed, matches the shape:

```
^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$
```

The helper is a pure function of buffer text between `(line-beginning
-position)` of `beg` and `(line-end-position)` of `end`. It does not
modify match-data observable to callers. It is the single source of
truth for "this span occupies a standalone line, optionally inside a
list-item or blockquote marker".

Decorators that need standalone-line gating SHALL delegate to this
helper rather than re-implement the regex.

#### Scenario: Whole-line span is standalone

- **GIVEN** a buffer line whose only content is a span `S`
- **WHEN** `gfm-pretty-standalone-span-p` is called with `S`'s range
- **THEN** it returns non-nil

#### Scenario: List-item-only span is standalone

- **GIVEN** a buffer line `- S` where `S` is the span
- **WHEN** the helper is called with `S`'s range
- **THEN** it returns non-nil

#### Scenario: Blockquote-marker-only span is standalone

- **GIVEN** a buffer line `> S` where `S` is the span
- **WHEN** the helper is called with `S`'s range
- **THEN** it returns non-nil

#### Scenario: Span embedded in prose is not standalone

- **GIVEN** a buffer line `See S for details.`
- **WHEN** the helper is called with `S`'s range
- **THEN** it returns nil

## MODIFIED Requirements

### Requirement: Link decoration defers source-range and diff URL forms to link-previews

The links decorator SHALL defer rendering to the `link-previews`
decorator for spans that `link-previews` will actually claim. The
deferral SHALL match `link-previews`' own claim rules so spans that
neither decorator would render do not silently disappear.

Concretely, the links decorator SHALL NOT collect or render a link
when ALL of the following hold:

1. The link's `kind` is `inline` (a `[label](url)` shape — not
   reference, shortcut, autolink, wiki, or bare-URL).
2. The resolved URL matches the source-range shape
   `<path>#L<digits>` optionally followed by `-L<digits>`.
3. `gfm-pretty-standalone-span-p` returns non-nil for the link's
   full `[label](url)` source span.

Independently, the links decorator SHALL NOT collect or render any
link whose resolved URL matches the diff shape
`diff:<base>...<head>` optionally followed by `#<path>`. Diff URLs
are unconditionally deferred regardless of `kind` or standalone
status.

| Form          | Shape                                          | Example                       |
| :------------ | :--------------------------------------------- | :---------------------------- |
| source-range  | `<path>#L<digits>` optionally `-L<digits>`     | `/repo/foo.yml#L13-L22`       |
| diff          | `diff:<base>...<head>` optionally `#<path>`    | `diff:main...HEAD#README.md`  |

The check SHALL be applied during the discovery filter pass alongside
the existing code-region exclusion. Skipped links produce no
title-side overlay, no URL-side overlay, and no overlay keymap.

All other source-range links — inline source-range links embedded in
prose, reference-style source-range links (`[label][src]` with
`[src]: /p#L1`), shortcut links, autolinks, and bare-URL forms —
SHALL be collected and decorated using the normal classification
path. Source-range URLs starting with `/`, `./`, `../`, `~/`, or
`file:` classify as `file`; other source-range URLs classify by the
usual rules. Title-side, URL-side icon, and RET behaviour follow the
resolved class.

The deferral exists because `link-previews` owns rendering and RET
dispatch for spans it claims via its own preview overlays and
overlay-keymap. Pretty-links overlays on a claimed span would either
stack incompatibly (display garbling) or shadow `link-previews`' RET
via overlay-keymap precedence. Spans that `link-previews` does not
claim have no ownership conflict.

#### Scenario: Standalone inline source-range link is deferred

- **GIVEN** a buffer line whose only content is
  `[snippet](/repo/foo.yml#L13-L22)`
- **THEN** the links decorator does NOT create any overlay for that
  span
- **AND** the buffer text renders raw (whatever `link-previews` does
  with it)

#### Scenario: Standalone inline source-range link with single line is deferred

- **GIVEN** a buffer line whose only content is `[line](/repo/x.el#L42)`
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Inline source-range link in prose is decorated as file

- **GIVEN** a buffer line `See [snippet](/repo/foo.yml#L13-L22) for context.`
- **THEN** the links decorator creates a title-side overlay with the
  `file` class
- **AND** a URL-side overlay covers the `(/repo/foo.yml#L13-L22)`
  span with the file icon (or `""` when `nerd-icons` is unavailable)

#### Scenario: List-item-only inline source-range link is deferred

- **GIVEN** a buffer line `- [snippet](/repo/foo.yml#L13-L22)`
- **THEN** the links decorator does NOT create any overlay
- **AND** `link-previews` is the only decorator that may render the
  span

#### Scenario: Reference link to source-range URL is decorated as file

- **GIVEN** a buffer containing `[snippet][src]` with a definition line
  `[src]: /repo/foo.yml#L13-L22`
- **THEN** the links decorator creates its usual title-side overlay
  with the `file` class
- **AND** the URL-side overlay (when the reference label appears
  inline) is created per the standard reference-link rules

#### Scenario: Inline diff link is deferred regardless of standalone

- **GIVEN** `[changed](diff:main...feature)` either inline in prose
  or on a line by itself
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Inline diff link with file scope is deferred

- **GIVEN** `[changed](diff:main...feature#path/to/file.el)` either
  inline in prose or on a line by itself
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Plain file link without line range unaffected

- **GIVEN** `[ops](./scripts/x.sh)` (no `#L...` suffix)
- **THEN** the links decorator creates its usual title-side overlay
  with the `file` class

#### Scenario: Plain anchor link unaffected

- **GIVEN** `[Setup](#setup)`
- **THEN** the links decorator creates its usual title-side overlay
  with the `anchor` class

### Requirement: URL-side icon rendering

The links decorator's `:apply-block-fn` SHALL replace the URL span
(`(url)`, `[label]`, the autolink span, …) with a single
nerd-icons glyph for `web` and `file` classes: the glyph is resolved
from the target host or scheme for `web`, and from the URL's basename
via `nerd-icons-icon-for-file` for `file`.

For `file`-class URLs the icon resolver SHALL strip any `#…`
fragment from the URL before computing the basename, so a URL of the
form `<path>#L<n>[-L<n>]` resolves the glyph for the underlying
file extension (e.g. `foo.el#L42` resolves the elisp glyph rather
than falling back to a generic glyph on the synthetic extension
`el#L42`). Fragment-stripping applies only to the file-resolution
branches; `web`-class URLs and `anchor` classification are
unaffected.

For the `anchor` class the URL-side overlay SHALL be created with its
`display` property set to the empty string, hiding the `(#slug)` span
from view while keeping the overlay's metadata
(`gfm-pretty-links-class`, `gfm-pretty-links-url`, etc.) available to
RET dispatch, eldoc, and xref.

When `nerd-icons` is unavailable, the URL-side overlay SHALL be
omitted for `web` links (URL shows raw). For `file` links the
overlay SHALL still be created with `display` = `""` (path hidden,
no icon). Anchor-class behaviour is unaffected.

#### Scenario: Github URL

- **GIVEN** `[code](https://github.com/user/repo)` and `nerd-icons`
  available
- **THEN** the URL-side renders as the GitHub icon

#### Scenario: Unknown host

- **GIVEN** a link to an unrecognised host (web class)
- **THEN** the URL-side renders as a generic web icon

#### Scenario: Anchor link hides URL span

- **GIVEN** `[Setup](#setup)`
- **THEN** a URL-side overlay covers the `(#setup)` span
- **AND** its `display` property is the empty string
- **AND** the overlay carries `gfm-pretty-links-class` = `anchor` and
  `gfm-pretty-links-url` = `#setup`

#### Scenario: File link renders icon for URL span

- **GIVEN** `[ops](./scripts/x.sh)` and `nerd-icons` available
- **THEN** a URL-side overlay covers the `(./scripts/x.sh)` span
- **AND** its `display` property is the nerd-icons glyph resolved by
  `nerd-icons-icon-for-file` on `x.sh`
- **AND** the overlay carries `gfm-pretty-links-class` = `file` and
  `gfm-pretty-links-url` = `./scripts/x.sh`

#### Scenario: File link with source-range fragment resolves by extension

- **GIVEN** an inline-in-prose link `[snippet](/path/foo.el#L42-L48)`
  and `nerd-icons` available
- **THEN** a URL-side overlay covers the `(/path/foo.el#L42-L48)`
  span
- **AND** its `display` property is the nerd-icons glyph that
  `nerd-icons-icon-for-file` returns for basename `foo.el`
- **AND** the overlay carries `gfm-pretty-links-class` = `file` and
  `gfm-pretty-links-url` = `/path/foo.el#L42-L48`

#### Scenario: Parent-relative file link with code-styled label hides URL span

- **GIVEN** ``[`dev/global/iam-roles/`](../../dev/global/iam-roles/terragrunt.stack.hcl)``
- **THEN** the title-side overlay displays `dev/global/iam-roles/`
  (wrapping backticks stripped) under `gfm-pretty-links-file-face`
- **AND** a URL-side overlay covers the parenthesised path with a
  non-nil `display` (an icon when `nerd-icons` resolves one, `""`
  otherwise)

#### Scenario: File link without `nerd-icons` still hides path

- **GIVEN** a file-class link and `nerd-icons` unavailable
- **THEN** a URL-side overlay covers the path span
- **AND** its `display` property is the empty string
- **AND** the overlay carries `gfm-pretty-links-class` = `file`

### Requirement: RET follows the link when point is on the decoration

The links decorator SHALL install an overlay keymap binding `RET` to
a class-dispatched follow handler. The handler SHALL read the link's
target class and URL from the overlay under point and act per class:

| Class    | Action                                                                  |
| :------- | :---------------------------------------------------------------------- |
| `web`    | call `markdown--browse-url` on the URL                                  |
| `anchor` | jump to the heading whose generated slug matches the anchor portion    |
| `file`   | `find-file` on the URL's path component (URL with any trailing `#L<n>[-L<n>]` fragment stripped), expanded relative to `buffer-file-name`'s directory (or `default-directory` when the buffer has no file); when the URL carried an `#L<n>[-L<n>]` source-range fragment, the handler SHALL then jump point to line `<n>` (range-end is ignored) |

The binding SHALL fire when point is on the rendered overlay.

For the `anchor` class the handler SHALL search the entire buffer for
the matching heading regardless of the current narrowing, and on
match SHALL widen the buffer before moving point to the heading.
After a successful jump the handler SHALL run the abnormal hook
`gfm-pretty-links-after-anchor-jump-functions` with the target buffer
position as its single argument. Subscribers can use the hook to
restore their preferred narrowing or apply additional decoration.

#### Scenario: RET on web link

- **WHEN** point is on a rendered `web` link's title and user presses
  `RET`
- **THEN** `markdown--browse-url` opens the URL

#### Scenario: RET on anchor link

- **GIVEN** a heading `## Setup Steps` in the buffer
- **AND** point is on a rendered `[go](#setup-steps)` link
- **WHEN** user presses `RET`
- **THEN** point moves to the `Setup Steps` heading

#### Scenario: RET on anchor link in narrowed buffer

- **GIVEN** a buffer narrowed to a region that does not include the
  `## Setup Steps` heading
- **AND** point is on a rendered `[go](#setup-steps)` link inside the
  narrowed region
- **WHEN** user presses `RET`
- **THEN** the buffer is widened
- **AND** point moves to the `Setup Steps` heading

#### Scenario: anchor jump hook fires with target position

- **GIVEN** a function `F` registered on
  `gfm-pretty-links-after-anchor-jump-functions`
- **WHEN** RET on an anchor link successfully jumps to a heading at
  buffer position `P`
- **THEN** `F` is called once with `P` as its sole argument
- **AND** `F` is called after point has moved and the buffer has been
  widened

#### Scenario: RET on anchor with no matching heading

- **GIVEN** a rendered `[go](#missing)` link
- **AND** no heading in the buffer has slug `missing`
- **WHEN** user presses `RET`
- **THEN** the handler raises `user-error` with a message naming the
  missing anchor
- **AND** `gfm-pretty-links-after-anchor-jump-functions` is not run

#### Scenario: RET on file link

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer
  whose file lives at `/repo/README.md`
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/repo/scripts/x.sh`

#### Scenario: RET on file link in fileless buffer

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer with
  no `buffer-file-name`
- **WHEN** user presses `RET`
- **THEN** `find-file` resolves the path against `default-directory`

#### Scenario: RET on inline source-range file link jumps to line

- **GIVEN** a rendered inline-in-prose link
  `[snippet](/path/foo.el#L42-L48)` in a buffer
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/path/foo.el`
- **AND** point lands on line 42 of that buffer

#### Scenario: RET on inline source-range file link with single line jumps to line

- **GIVEN** a rendered inline-in-prose link
  `[line](/path/foo.el#L42)` in a buffer
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/path/foo.el`
- **AND** point lands on line 42
