## ADDED Requirements

### Requirement: Standalone link gating for preview overlays

Preview overlays (source-range and diff-range) SHALL decorate only
standalone links — links that occupy a whole line, optionally
inside a single list-item or blockquote marker.

A link `[label](url)` SHALL be considered standalone when the
line containing the link, with the `[label](url)` token removed,
matches:

```
^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$
```

That is — the surrounding line content is whitespace and at most
one of: an unordered-list marker (`- `, `* `, `+ `), an ordered-
list marker (`<n>. `), or a blockquote marker (`> `).

Links failing this check SHALL be left undecorated: no preview
overlay, the original `[label](url)` text shows through as
ordinary prose.

This gating SHALL apply to both source-range and diff-range
previews; inline-classification SHALL NOT depend on URL form.

#### Scenario: whole-line link gets a preview

- **GIVEN** a slide line whose only content is `[fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: list-item-only link gets a preview

- **GIVEN** a slide line `- [fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: blockquote link gets a preview

- **GIVEN** a slide line `> [fn](modules/auth.rs#L42-L48)`
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region with a rendered preview

#### Scenario: inline link in prose is left undecorated

- **GIVEN** a slide line `See [fn](modules/auth.rs#L42-L48) for details.`
- **WHEN** previews are rendered
- **THEN** no preview overlay covers the link's region
- **AND** the original markdown text `[fn](modules/auth.rs#L42-L48)` is
  visible (possibly via the gfm-pretty-links decorator's title-side
  display, but no source body content is shown)

#### Scenario: link followed by trailing text on its own line is left undecorated

- **GIVEN** a slide line `[fn](modules/auth.rs#L42-L48) and that's the bit`
- **WHEN** previews are rendered
- **THEN** no preview overlay covers the link's region

## MODIFIED Requirements

### Requirement: Source-range link preview overlay

The system SHALL render box-bordered source previews for
standalone source-range links inside the current slide
narrowing.

For each standalone (per `Standalone link gating for preview
overlays`) link whose URL matches `<path>#L<start>` or
`<path>#L<start>-L<end>`, the system SHALL place an overlay on
the link's full markdown expression (`[label](url)`) whose
`display` property is a propertised multi-line string composed
of:

1. A **top border** in `gfm-pretty-border-face` of the form
   `┌─ <abbrev-path>:<start>-<end> ──…──┐`. The abbreviated path
   is computed by:
   - If the absolute source path is inside a project (resolved
     via `project-current` with `default-directory` bound to the
     source file's directory), the path SHALL be made relative
     to the project root.
   - Otherwise, the path SHALL be passed through
     `abbreviate-file-name` (`~/`-style replacement).
   - If the resulting label still does not fit the box's top
     border, the path SHALL be left-truncated with a leading
     ellipsis `…/` preserving the trailing basename.
   The label is right-padded with the `─` border character to
   fill the top edge.
2. A **body** of up to 10 lines from `<path>`, starting at
   `<start>`, each prefixed with `│ ` and suffixed with ` │` in
   `gfm-pretty-border-face`. Body lines SHALL be fontified by
   the major-mode resolved for `<path>` via `auto-mode-alist`.
   Fontification uses `font-lock-ensure` in a temp buffer; the
   resulting propertised substring (with `face` text
   properties) is embedded directly in the display string. The
   temp buffer SHALL have `buffer-file-name` set to `<path>`
   before activating the major-mode (so tree-sitter modes that
   key off the file name activate). The temp buffer SHALL NOT
   trigger any file-saving side effects.
3. Body-line truncation: if a body line's `string-width`
   exceeds the box's interior width, the line SHALL be
   truncated to the interior width minus one cell and suffixed
   with `…`. Truncation is measured in cells via
   `string-width`, not characters.
4. A **bottom border** in `gfm-pretty-border-face`.
   - When the requested range exceeds the 10-line cap, the
     bottom border SHALL embed `+N more lines` (where N is
     `(end - start + 1) - 10`), e.g.
     `└─ +29 more lines ─…──┘`.
   - Otherwise, the bottom border SHALL be a bare
     `└──…──┘`.

The box width SHALL be
`min(available-width, max(80, longest-body-line + decoration-w))`,
where `decoration-w` is 4 (matching `│ ` + ` │`) and
`available-width` is computed via
`gfm-pretty--available-width` of the selected window (falling
back to `fill-column` or 80 when the buffer is not displayed).

The markdown label (`[label]` portion of the link) SHALL NOT
appear anywhere in the rendered preview surface.

When `<end>` is omitted, the range SHALL be `<start>` to
`<start>` (single line).

When the file does not exist or the range is invalid, the
overlay SHALL render as a bare single-line sentinel — no box,
no border — propertised with `shadow` face, of the form:

```
[broken preview] <abbrev-path>:<start>-<end> — <reason>
```

where `<reason>` is `file not found` or `invalid range`.

The major-mode resolution SHALL be a pure function of `<path>`
via `auto-mode-alist`; when no entry matches, fontification
SHALL fall back to `fundamental-mode` (yielding an unfontified
body).

The underlying buffer text SHALL NOT be modified — overlays use
`display` properties only, so saving the buffer writes the
original markdown source.

#### Scenario: small range renders fontified body inside a box

- **GIVEN** a standalone link `[fn](modules/auth.rs#L42-L48)` on its own line
- **AND** lines 42-48 of `modules/auth.rs` are 7 lines of Rust code
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region
- **AND** the overlay's `display` string contains the 7 source lines
- **AND** the display string begins with `┌` and ends with `┘`
- **AND** at least one character in the body carries a `face` text
  property assigned by `rust-mode` (or `rust-ts-mode` per
  `auto-mode-alist`)

#### Scenario: path appears abbreviated in the top border

- **GIVEN** the source file is at `<project-root>/.github/workflows/drift.yml`
- **AND** the project is detected via `project-current`
- **AND** a standalone link references `.github/workflows/drift.yml#L13-L22`
- **WHEN** previews are rendered
- **THEN** the overlay's top border contains the substring
  `.github/workflows/drift.yml:13-22`
- **AND** the top border does not contain the project root prefix

#### Scenario: out-of-project path uses tilde-abbreviation

- **GIVEN** the source file is at `$HOME/notes/scratch.txt` (not in any project)
- **AND** a standalone link references that path
- **WHEN** previews are rendered
- **THEN** the overlay's top border contains the substring
  `~/notes/scratch.txt:<start>-<end>`

#### Scenario: long path left-truncated to fit

- **GIVEN** an abbreviated path longer than the box's top border can fit
- **WHEN** previews are rendered
- **THEN** the label in the top border begins with `…/`
- **AND** the basename of the path is present in the label

#### Scenario: oversize range renders head-only with footer in bottom border

- **GIVEN** a standalone link `[fn](modules/auth.rs#L42-L80)`
- **AND** lines 42-80 exist in the file
- **WHEN** previews are rendered
- **THEN** the overlay's body contains lines 42-51 (the first 10
  lines of the range)
- **AND** the overlay's bottom border embeds `+29 more lines`
- **AND** the overlay's `display` string contains no `· click to open` text

#### Scenario: bottom border is bare when not truncated

- **GIVEN** a standalone link whose requested range is within the 10-line cap
- **WHEN** previews are rendered
- **THEN** the overlay's bottom border embeds no `+N more lines` text

#### Scenario: long body line truncated with ellipsis

- **GIVEN** a body line whose `string-width` exceeds the box's interior width
- **WHEN** previews are rendered
- **THEN** the rendered body line for that source line ends with `…`
- **AND** the right border `│` appears at the box width column

#### Scenario: markdown label not present in preview

- **GIVEN** a standalone link `[my custom label](path#L1-L5)`
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string does not contain the
  substring `my custom label`

#### Scenario: missing file renders bare sentinel, no box

- **GIVEN** a standalone link to `nonexistent.rs#L1-L10`
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string is a single line
- **AND** the display string contains `[broken preview]`
- **AND** the display string contains `file not found`
- **AND** the display string does not contain `┌` or `└`

#### Scenario: invalid range renders bare sentinel, no box

- **GIVEN** a standalone link to a real file with a range past EOF
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string is a single line
- **AND** the display string contains `[broken preview]`
- **AND** the display string contains `invalid range`

#### Scenario: original buffer text unchanged

- **WHEN** preview overlays are rendered on a slide
- **AND** the buffer is saved to disk
- **THEN** the on-disk file contains the original markdown link
  syntax `[label](url)` with no preview content

### Requirement: Diff-range link preview overlay

The system SHALL render box-bordered diff previews for
standalone diff-range links inside the current slide narrowing.

For each standalone (per `Standalone link gating for preview
overlays`) link whose URL matches `diff:<base>...<head>` (with
optional `#<path>` fragment), the system SHALL place an overlay
on the link's full markdown expression whose `display` property
is a propertised multi-line string composed of:

1. A **top border** in `gfm-pretty-border-face` of the form
   `┌─ <base>...<head> ──…──┐` when the URL has no path
   qualifier, or `┌─ <base>...<head> — <path> ──…──┐` when
   path-scoped. Refs matching `(rx bos (= 40 hex) eos)` SHALL
   be shortened to their first 7 characters before being
   embedded in the label; branch names and tags pass through
   unchanged.
2. A **body** of up to 10 lines from `git diff <base>...<head>
   [-- <path>]` executed from the buffer's worktree.
   Body lines SHALL render in **LHS-margin mode**: each line is
   prefixed with a single `│` (no left padding) and suffixed
   with `│`, so the first body column is the `+`/`-`/` ` diff
   indicator. The box's interior decoration width is 2 cols
   (matching authored ` ```diff ` fences via
   `gfm-pretty-fences--lhs-margin-langs`).
3. Body-line truncation as for source previews, applied to the
   interior width minus 1 cell with a trailing `…`.
4. A **bottom border** in `gfm-pretty-border-face`.
   - When the diff exceeds the 10-line cap, the bottom border
     SHALL embed `+N more lines`.
   - Otherwise, the bottom border SHALL be a bare
     `└──…──┘`.

When `git diff` produces no output, the overlay SHALL render as
a bare single-line sentinel — no box — propertised with `shadow`
face, of the form:

```
[broken preview] <base>...<head>[<path>] — no changes
```

When `git` exits non-zero, the overlay SHALL render as a bare
single-line sentinel of the form:

```
[broken preview] <base>...<head>[<path>] — git error: <first-error-line>
```

The markdown label SHALL NOT appear anywhere in the rendered
preview surface.

The box width formula is identical to source-range previews,
substituting `decoration-w` 2 for LHS-margin mode.

#### Scenario: diff link renders box-bordered preview

- **GIVEN** a standalone link `[change](diff:HEAD~1...HEAD#auth.rs)`
- **AND** `git diff HEAD~1...HEAD -- auth.rs` produces 6 lines of
  output
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region
- **AND** the overlay's `display` string begins with `┌` and ends with `┘`
- **AND** the display contains those 6 diff lines
- **AND** the body lines begin directly with `│+`, `│-`, or `│ ` (no
  left padding between `│` and the indicator)

#### Scenario: diff top border embeds base...head

- **GIVEN** a standalone link `[c](diff:main...feature)`
- **WHEN** previews are rendered
- **THEN** the overlay's top border contains the substring `main...feature`

#### Scenario: diff top border embeds path qualifier with em-dash

- **GIVEN** a standalone link `[c](diff:main...feature#src/foo.rs)`
- **WHEN** previews are rendered
- **THEN** the overlay's top border contains the substring
  `main...feature — src/foo.rs`

#### Scenario: SHA refs shortened to 7 chars

- **GIVEN** a standalone link `[c](diff:abcdef0123456789abcdef0123456789abcdef01...0123456789abcdef0123456789abcdef01234567)`
  (two 40-char hex SHAs)
- **WHEN** previews are rendered
- **THEN** the overlay's top border contains `abcdef0...0123456`
- **AND** does not contain either 40-char SHA in full

#### Scenario: empty diff renders bare sentinel, no box

- **WHEN** a standalone diff link's `git diff` invocation produces no output
- **THEN** the overlay's `display` string is a single line
- **AND** contains `[broken preview]`
- **AND** contains `no changes`

#### Scenario: git error renders bare sentinel, no box

- **WHEN** a standalone diff link's `git diff` invocation exits non-zero
- **THEN** the overlay's `display` string is a single line
- **AND** contains `[broken preview]`
- **AND** contains `git error:`

#### Scenario: diff oversize renders with bottom-border footer

- **GIVEN** a standalone diff link whose diff output exceeds 10 lines
- **WHEN** previews are rendered
- **THEN** the overlay's body contains the first 10 lines of diff output
- **AND** the bottom border embeds `+N more lines`

#### Scenario: markdown label not present in diff preview

- **GIVEN** a standalone diff link `[my label](diff:a...b)`
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string does not contain the
  substring `my label`
