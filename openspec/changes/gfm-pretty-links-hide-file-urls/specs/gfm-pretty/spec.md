## MODIFIED Requirements

### Requirement: URL-side icon rendering

The links decorator's `:apply-block-fn` SHALL replace the URL span
(`(url)`, `[label]`, the autolink span, …) with a single
nerd-icons glyph resolved from the target host or scheme, ONLY when
the link's target class is `web`.

For the `anchor` and `file` classes the URL-side overlay SHALL be
created with its `display` property set to the empty string, hiding
the `(#slug)` or `(path)` span from view while keeping the overlay's
metadata (`gfm-pretty-links-class`, `gfm-pretty-links-url`, etc.)
available to RET dispatch, eldoc, and xref.

When `nerd-icons` is unavailable, the URL-side overlay SHALL be
omitted for `web` links (URL shows raw). Anchor-class and file-class
URL hiding do not depend on `nerd-icons`.

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

#### Scenario: File link hides URL span

- **GIVEN** `[ops](./scripts/x.sh)`
- **THEN** a URL-side overlay covers the `(./scripts/x.sh)` span
- **AND** its `display` property is the empty string
- **AND** the overlay carries `gfm-pretty-links-class` = `file` and
  `gfm-pretty-links-url` = `./scripts/x.sh`

#### Scenario: Parent-relative file link with code-styled label hides URL span

- **GIVEN** ``[`dev/global/iam-roles/`](../../dev/global/iam-roles/terragrunt.stack.hcl)``
- **THEN** the title-side overlay displays the label (backticks
  included) under `gfm-pretty-links-file-face`
- **AND** a URL-side overlay covers the parenthesised path with
  `display` set to the empty string
