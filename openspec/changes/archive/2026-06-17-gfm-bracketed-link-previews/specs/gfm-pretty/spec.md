## ADDED Requirements

### Requirement: Link previews — wrapped bare-line reference shapes

The `link-previews` decorator SHALL additionally recognise a bare-line reference whose URL token is wrapped in a single matched delimiter pair:

- **bracket-wrapped**: `[<url>]`
- **angle-wrapped**: `<<url>>`

where `<url>` parses as a source-range URL (`<path>#L<a>[-L<b>]`) or a diff-range URL (`diff:<base>...<head>[#<path>]`).  Recognition SHALL strip exactly one leading delimiter and one trailing delimiter of a matched pair before parsing the inner token; an unmatched or mismatched pair (e.g. `[<url>` or `<<url>]`) SHALL NOT be recognised.

The wrapped token SHALL be subject to the same gates as the unwrapped bare-line token:

- **Standalone gating** — the line, with the entire wrapped token removed, MUST match the standalone-line regex (whitespace plus at most one list-item or blockquote marker).  Wrapped tokens embedded in prose SHALL be left undecorated.
- **`/`-in-path gate** — for source-range references, the inner `<path>` MUST contain at least one `/`; basename-only inner paths SHALL NOT match.
- **Preformatted-context exclusion** — wrapped tokens inside fenced code blocks, ≥4-column indented blocks, or a single inline-code span on their own line SHALL be left undecorated.

A wrapped reference's overlay range SHALL cover the full wrapped token, including the surrounding `[ ]` or `< >` delimiters (not the leading whitespace or marker prefix).  The payload, rendering pipeline, RET-follow dispatch, and broken-preview sentinel behaviour SHALL be identical to the unwrapped bare-line and `[label](url)` forms once the block is collected.

A bracket-wrapped token SHALL NOT be mistaken for a `[label](url)` markdown link: the `[label](url)` form requires a parenthesised destination, which the wrapped shapes lack.  A line that is a markdown link reference definition (`[<text>]: <destination>`) SHALL NOT be recognised as a wrapped reference, because the trailing `: <destination>` leaves the line non-standalone after the wrapped token is removed.

#### Scenario: bracket-wrapped reference on its own line gets a preview

- **GIVEN** a line whose only content is `[modules/auth.rs#L42-L48]`
- **WHEN** previews are rendered
- **THEN** an overlay covers the full `[modules/auth.rs#L42-L48]` token (including brackets) with a rendered preview

#### Scenario: angle-wrapped reference on its own line gets a preview

- **GIVEN** a line whose only content is `<modules/auth.rs#L42-L48>`
- **WHEN** previews are rendered
- **THEN** an overlay covers the full `<modules/auth.rs#L42-L48>` token (including angle brackets) with a rendered preview

#### Scenario: wrapped list-item reference gets a preview

- **GIVEN** a line `- [modules/auth.rs#L42-L48]`
- **WHEN** previews are rendered
- **THEN** an overlay covers the wrapped token's region, and the `- ` marker renders in its native form to the left of the box

#### Scenario: wrapped diff reference gets a preview

- **GIVEN** a line whose only content is `[diff:main...HEAD#modules/auth.rs]`
- **WHEN** previews are rendered
- **THEN** an overlay covers the full wrapped token with a rendered diff preview

#### Scenario: wrapped reference embedded in prose is left undecorated

- **GIVEN** a line `See [modules/auth.rs#L42-L48] for details.`
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that reference

#### Scenario: wrapped basename-only reference is left undecorated

- **GIVEN** a line whose only content is `[auth.rs#L1-L5]` (no `/` in the inner path)
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that reference

#### Scenario: wrapped reference inside a fenced code block is left undecorated

- **GIVEN** a `[modules/auth.rs#L1-L5]` line inside a triple-backtick fence
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for the line inside the fence

#### Scenario: link reference definition is not mistaken for a wrapped reference

- **GIVEN** a line `[modules/auth.rs#L1-L5]: https://example.com/x`
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that line

#### Scenario: mismatched delimiters are not recognised

- **GIVEN** a line whose only content is `[modules/auth.rs#L1-L5>`
- **WHEN** previews are rendered
- **THEN** no preview overlay is created for that line
