## MODIFIED Requirements

### Requirement: Source-range link preview overlay

The system SHALL render source previews for source-range links
inside the current slide narrowing.

For each link whose URL matches `<path>#L<start>` or
`<path>#L<start>-L<end>`, the system SHALL place an overlay on the
link's full markdown expression (`[label](url)`) whose `display`
property is a propertised string composed of:

1. A **header line**: `<label> · <path>:<start>-<end>`, propertised
   with `markdown-code-face` (or equivalent) so it visually
   separates from surrounding prose.
2. A **body**: up to 10 lines from `<path>` (resolved relative to
   the buffer's `default-directory` when not absolute), starting at
   line `<start>`. The body SHALL be fontified by the major-mode
   resolved for `<path>` via `auto-mode-alist`. Fontification uses
   `font-lock-ensure` in a temp buffer; the resulting propertised
   substring (with `face` text properties) is embedded directly in
   the display string.
3. An optional **footer line**: when the requested range exceeds
   10 lines, the footer SHALL read `+N more lines · click to open`
   (where N is `(end - start + 1) - 10`), propertised with `shadow`.

When `<end>` is omitted, the range SHALL be `<start>` to `<start>`
(single line). When the file does not exist or the range is
invalid, the body SHALL be a single line `(file not found: <path>)`
or `(invalid range)` propertised with `shadow`.

The major-mode resolution SHALL be a pure function of `<path>` via
`auto-mode-alist`; when no entry matches, fontification SHALL fall
back to `fundamental-mode` (yielding an unfontified body). The
system SHALL set `buffer-file-name` to `<path>` inside the temp
buffer before activating the major-mode, so tree-sitter modes that
key off `buffer-file-name` activate correctly. The temp buffer
SHALL NOT trigger any file-saving side effects.

The underlying buffer text SHALL NOT be modified — overlays use
`display` properties only, so saving the buffer writes the
original markdown source.

#### Scenario: small range renders fontified body

- **WHEN** a link `[fn](modules/auth.rs#L42-L48)` is inside the
  current narrowing
- **AND** lines 42-48 of `modules/auth.rs` are 7 lines of Rust code
- **THEN** an overlay covers the link's region
- **AND** the overlay's `display` string contains the 7 source lines
- **AND** at least one character in the body carries a `face` text
  property assigned by `rust-mode` (or `rust-ts-mode` per
  `auto-mode-alist`)

#### Scenario: yaml extension resolves via auto-mode-alist

- **GIVEN** the user's `auto-mode-alist` maps `\\.ya?ml\\'` to
  `yaml-ts-mode`
- **WHEN** a link `[snip](config.yml#L1-L5)` is inside the current
  narrowing
- **THEN** the overlay's `display` body lines carry `face` text
  properties assigned by `yaml-ts-mode`

#### Scenario: unknown extension yields plain body

- **WHEN** a link points at a file whose extension matches no entry
  in `auto-mode-alist`
- **THEN** the overlay still renders with the header and body lines
- **AND** body lines carry no major-mode-assigned `face` properties

#### Scenario: oversized range renders head-only with footer

- **WHEN** a link `[fn](modules/auth.rs#L42-L80)` is inside the
  current narrowing
- **AND** lines 42-80 exist in the file
- **THEN** the overlay's body contains lines 42-51 (the first 10
  lines of the range)
- **AND** the overlay's display ends with a `shadow`-faced line
  `+29 more lines · click to open`

#### Scenario: missing file renders sentinel

- **WHEN** a link to `nonexistent.rs#L1-L10` is inside the current
  narrowing
- **THEN** the overlay's `display` string body is the single line
  `(file not found: nonexistent.rs)` propertised with `shadow`

#### Scenario: original buffer text unchanged

- **WHEN** preview overlays are rendered on a slide
- **AND** the buffer is saved to disk
- **THEN** the on-disk file contains the original markdown link
  syntax `[label](url)` with no preview content
