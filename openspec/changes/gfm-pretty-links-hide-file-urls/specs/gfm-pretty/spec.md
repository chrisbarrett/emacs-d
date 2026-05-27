## MODIFIED Requirements

### Requirement: URL-side icon rendering

The links decorator's `:apply-block-fn` SHALL replace the URL span
(`(url)`, `[label]`, the autolink span, …) with a single
nerd-icons glyph for `web` and `file` classes: the glyph is resolved
from the target host or scheme for `web`, and from the URL's basename
via `nerd-icons-icon-for-file` for `file`.

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

#### Scenario: Parent-relative file link with code-styled label hides URL span

- **GIVEN** ``[`dev/global/iam-roles/`](../../dev/global/iam-roles/terragrunt.stack.hcl)``
- **THEN** the title-side overlay displays the label (backticks
  included) under `gfm-pretty-links-file-face`
- **AND** a URL-side overlay covers the parenthesised path with a
  non-nil `display` (an icon when `nerd-icons` resolves one, `""`
  otherwise)

#### Scenario: File link without `nerd-icons` still hides path

- **GIVEN** a file-class link and `nerd-icons` unavailable
- **THEN** a URL-side overlay covers the path span
- **AND** its `display` property is the empty string
- **AND** the overlay carries `gfm-pretty-links-class` = `file`
