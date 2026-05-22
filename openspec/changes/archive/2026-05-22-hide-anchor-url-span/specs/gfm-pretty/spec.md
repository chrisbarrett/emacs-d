## MODIFIED Requirements

### Requirement: URL-side icon rendering

The links decorator's `:apply-block-fn` SHALL replace the URL span
(`(url)`, `[label]`, the autolink span, …) with a single
nerd-icons glyph resolved from the target host or scheme, ONLY when
the link's target class is `web`.

For the `anchor` class the URL-side overlay SHALL be created with
its `display` property set to the empty string, hiding the
`(#slug)` span from view while keeping the overlay's metadata
(`gfm-pretty-links-class`, `gfm-pretty-links-url`, etc.) available
to RET dispatch, eldoc, and xref.

For the `file` class the URL-side overlay SHALL be omitted; the URL
span renders raw (or collapses via markdown-mode's own composition
if the decorator's advice is not active).

When `nerd-icons` is unavailable, the URL-side overlay SHALL be
omitted for `web` links (URL shows raw). Anchor-class URL hiding
does not depend on `nerd-icons`.

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

#### Scenario: File link omits icon

- **GIVEN** `[ops](./scripts/x.sh)`
- **THEN** no url-side overlay is created
