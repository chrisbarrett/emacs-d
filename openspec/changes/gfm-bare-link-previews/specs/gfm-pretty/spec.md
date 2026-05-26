## MODIFIED Requirements

### Requirement: Link previews decorator registration and toggle

The system SHALL register a decorator named `link-previews` that renders box-bordered previews for standalone source-range (`<path>#L<a>-L<b>`) and diff-range (`diff:<base>...<head>[#<path>]`) references in `gfm-pretty-mode` buffers.  Both markdown link references — `[label](url)` — and bare-line references — the URL written alone on a line — SHALL be recognised.

`(gfm-pretty-toggle-decorator 'link-previews)` SHALL flip the decorator on and off.  The default-on state matches the other decorators registered under `gfm-pretty`.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing a standalone source-range link
- **THEN** a preview overlay covers the link's region with a box-bordered rendered preview

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'link-previews)` flips the enable bit off
- **THEN** every preview overlay is removed
- **AND** the raw `[label](url)` markdown text is visible

#### Scenario: bare-line source reference gets a preview

- **GIVEN** a line whose only content is `modules/auth.rs#L42-L48` (with no surrounding `[...](...)` syntax)
- **WHEN** previews are rendered
- **THEN** a preview overlay covers the bare reference's region with a box-bordered rendered preview

### Requirement: Link previews — standalone link gating

The `link-previews` decorator SHALL decorate only standalone references — references that occupy a whole line, optionally inside a single list-item or blockquote marker.  This gating applies uniformly to bracketed `[label](url)` references and to bare-line URL-only references.

A `[label](url)` link SHALL be considered standalone when the line containing the link, with the `[label](url)` token removed, matches:

```
^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$
```

A bare-line reference SHALL be considered standalone when the line containing the URL token, with the URL token removed, matches the same regex.  The URL token is the contiguous non-whitespace run that parses as a source-range URL (`<path>#L<a>[-L<b>]`) or a diff-range URL (`diff:<base>...<head>[#<path>]`).  For bare lines, the path component of a source-range URL MUST contain at least one `/` — basename-only tokens (e.g. `auth.rs#L1-L5`) SHALL NOT match, to suppress false positives on agent-generated prose.

The surrounding line content is whitespace and at most one of: an unordered-list marker (`- `, `* `, `+ `), an ordered-list marker (`<n>. `), or a blockquote marker (`> `).

References failing this check SHALL be left undecorated.  This gating SHALL apply to both source-range and diff-range previews; inline-classification SHALL NOT depend on URL form.

#### Scenario: whole-line link gets a preview

- **GIVEN** a line whose only content is `[fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: list-item-only link gets a preview

- **GIVEN** a line `- [fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: blockquote link gets a preview

- **GIVEN** a line `> [fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: inline link in prose is left undecorated

- **GIVEN** a line `See [fn](modules/auth.rs#L42-L48) for details.`
- **WHEN** previews are rendered
- **THEN** no preview overlay covers the link's region
- **AND** the original markdown text `[fn](modules/auth.rs#L42-L48)` is visible (possibly via the `gfm-pretty-links` decorator's title-side display, but no source body content is shown)

#### Scenario: bare-line list-item reference gets a preview

- **GIVEN** a line `- modules/auth.rs#L42-L48`
- **WHEN** previews are rendered
- **THEN** an overlay covers the bare URL token's region with a rendered preview

#### Scenario: bare-line blockquote reference gets a preview

- **GIVEN** a line `> modules/auth.rs#L42-L48`
- **WHEN** previews are rendered
- **THEN** an overlay covers the bare URL token's region with a rendered preview

#### Scenario: bare reference embedded in prose is left undecorated

- **GIVEN** a line `See modules/auth.rs#L42-L48 for details.`
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that reference

#### Scenario: bare basename-only reference is left undecorated

- **GIVEN** a line whose only content is `auth.rs#L1-L5` (no `/` in the path)
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that reference

## ADDED Requirements

### Requirement: Link previews — bare-line reference discovery

The `link-previews` decorator SHALL discover bare-line references by scanning the widened buffer for lines whose sole significant content (after stripping any leading whitespace and at most one list-item or blockquote marker) is a URL token parsable as a source-range URL (`<path>#L<a>[-L<b>]`) or a diff-range URL (`diff:<base>...<head>[#<path>]`).

For bare-line source-range references, the `<path>` component MUST contain at least one `/`.  Absolute paths (`/…`), tilde-prefixed paths (`~/…`), and relative paths containing a `/` SHALL all be accepted; tilde-prefixed paths SHALL be expanded via `expand-file-name`; relative paths SHALL be resolved against `default-directory` (matching the existing behaviour for bracketed source links).

A bare-line reference's overlay range SHALL cover the URL token only (not the leading whitespace or marker prefix), so the marker continues to render in its native form to the left of the box.  The payload, rendering pipeline, RET-follow dispatch, and broken-preview sentinel behaviour SHALL be identical to the bracketed form once the block is collected.

When a line could match both a bracketed `[label](url)` pattern and a bare-line pattern (theoretically impossible given the brackets, but stated for clarity), the bracketed form SHALL take precedence; bare-line matching SHALL skip any span already claimed by a bracketed match.

#### Scenario: absolute bare path is recognised

- **GIVEN** a line whose only content is `/Users/chris/src/foo/main.rs#L10-L20`
- **WHEN** previews are rendered
- **THEN** an overlay covers the URL token with a rendered preview whose top border embeds the abbreviated path and range

#### Scenario: tilde-prefixed bare path is expanded and recognised

- **GIVEN** a line whose only content is `~/src/foo/main.rs#L10-L20`
- **WHEN** previews are rendered
- **THEN** an overlay covers the URL token with a rendered preview reading from the expanded absolute path

#### Scenario: bare diff-range reference is recognised

- **GIVEN** a line whose only content is `diff:abc1234...def5678#main.tf`
- **WHEN** previews are rendered
- **THEN** an overlay covers the URL token with a diff-range preview rendered exactly as a bracketed `[…](diff:abc1234...def5678#main.tf)` would render

#### Scenario: overlay covers token only, not marker

- **GIVEN** a line `  - /abs/path/main.rs#L1-L3`
- **WHEN** previews are rendered
- **THEN** the overlay's range starts at the `/` of the URL token and ends at the trailing digit `3`
- **AND** the leading `  - ` continues to render unchanged

### Requirement: Link previews — preformatted-context exclusion for bare references

Bare-line references SHALL be excluded from preview decoration when they fall inside any of the following preformatted contexts:

1. **Fenced code blocks** — lines between an opening triple-backtick (or longer-than-three-backtick) fence and its matching closing fence, inclusive of the fence lines themselves.  The fence pattern matches `gfm-pretty-fences`' opening (`gfm-pretty-fences--open-re`) and closing (`gfm-pretty-fences--close-re`) shape; the decorator MAY recompute these ranges locally rather than depending on `gfm-pretty-fences' enabled state.
2. **GFM indented code blocks** — lines whose leading-whitespace indent is at least 4 columns, when not preceded on a contiguous prior line by a list-item marker whose continuation indent would naturally absorb the 4-space indent.  An approximation that treats any line with ≥4 leading spaces as preformatted (regardless of list-item context) SHALL be acceptable, given that bare-line references inside list-item bodies normally use ≤2 spaces of indent in agent-generated prose.
3. **Inline-code wrap on its own line** — lines whose entire significant content is a single inline-code span (`` `<token>` ``), with optional leading marker.  Treating these as preformatted preserves the writer's intent of suppressing decoration.

These exclusion rules apply only to bare-line references; bracketed `[label](url)` references are already gated by markdown's existing handling of code spans and SHALL remain unaffected.

#### Scenario: bare reference inside a fenced code block is left undecorated

- **GIVEN** a buffer containing
  ````
  ```
  /abs/path/main.rs#L1-L3
  ```
  ````
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for the line inside the fence

#### Scenario: bare reference in 4-space indented block is left undecorated

- **GIVEN** a line beginning with 4 leading spaces followed by `/abs/path/main.rs#L1-L3`
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that line

#### Scenario: bare reference inside inline-code wrap is left undecorated

- **GIVEN** a line whose only content is `` `/abs/path/main.rs#L1-L3` ``
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that line

#### Scenario: bare reference inside a fence with 4-backtick delimiter is left undecorated

- **GIVEN** a buffer containing a `~~~~`-delimited or 4-backtick-delimited fence whose body contains a bare source-range line
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for the line inside the fence (the recogniser MAY restrict matching to triple-backtick fences; in that case 4-backtick fences are still treated as preformatted because the decorator's opening-fence regex matches a run of ≥3 backticks)
