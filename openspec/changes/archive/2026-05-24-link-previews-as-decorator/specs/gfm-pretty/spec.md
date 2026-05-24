## ADDED Requirements

### Requirement: Link previews decorator registration and toggle

The system SHALL register a decorator named `link-previews` that renders box-bordered previews for standalone source-range (`<path>#L<a>-L<b>`) and diff-range (`diff:<base>...<head>[#<path>]`) markdown links anywhere `gfm-pretty-mode` is enabled.

`(gfm-pretty-toggle-decorator 'link-previews)` SHALL flip the decorator on and off.  The default-on state matches the other decorators registered under `gfm-pretty`.

#### Scenario: Enabling

- **WHEN** `gfm-pretty-mode` is enabled in a buffer containing a standalone source-range link
- **THEN** a preview overlay covers the link's region with a box-bordered rendered preview

#### Scenario: Toggling off

- **WHEN** `(gfm-pretty-toggle-decorator 'link-previews)` flips the enable bit off
- **THEN** every preview overlay is removed
- **AND** the raw `[label](url)` markdown text is visible

### Requirement: Link previews — standalone link gating

The `link-previews` decorator SHALL decorate only standalone links — links that occupy a whole line, optionally inside a single list-item or blockquote marker.

A link `[label](url)` SHALL be considered standalone when the line containing the link, with the `[label](url)` token removed, matches:

```
^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$
```

The surrounding line content is whitespace and at most one of: an unordered-list marker (`- `, `* `, `+ `), an ordered-list marker (`<n>. `), or a blockquote marker (`> `).

Links failing this check SHALL be left undecorated.  This gating SHALL apply to both source-range and diff-range previews; inline-classification SHALL NOT depend on URL form.

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

### Requirement: Source-range link preview overlay rendering

For each standalone link whose URL matches `<path>#L<start>` or `<path>#L<start>-L<end>`, the `link-previews` decorator SHALL place an overlay on the link's full markdown expression (`[label](url)`) whose `display` property is a propertised multi-line string composed of:

1. A **top border** in `gfm-pretty-border-face` of the form `┌─ <abbrev-path>:<start>-<end> ──…──┐`.  The abbreviated path is computed by:
   - If the absolute source path is inside a project (resolved via `project-current` with `default-directory` bound to the source file's directory), the path SHALL be made relative to the project root.
   - Otherwise, the path SHALL be passed through `abbreviate-file-name` (`~/`-style replacement).
   - If the resulting label still does not fit the box's top border, the path SHALL be left-truncated with a leading ellipsis `…/` preserving the trailing basename.
   The label is right-padded with the `─` border character to fill the top edge.
2. A **body** of up to 10 lines from `<path>`, starting at `<start>`, each prefixed with `│ ` and suffixed with ` │` in `gfm-pretty-border-face`.  Body lines SHALL be fontified by the major-mode resolved for `<path>` via `auto-mode-alist`.  Fontification uses `font-lock-ensure` in a temp buffer; the temp buffer SHALL have `buffer-file-name` set to `<path>` before activating the major-mode (so tree-sitter modes that key off the file name activate).  The temp buffer SHALL NOT trigger any file-saving side effects.
3. Body-line truncation: if a body line's `string-width` exceeds the box's interior width, the line SHALL be truncated to the interior width minus one cell and suffixed with `…`.  Truncation is measured in cells via `string-width`, not characters.
4. A **bottom border** in `gfm-pretty-border-face`.
   - When the requested range exceeds the 10-line cap, the bottom border SHALL embed `+N more lines` (where N is `(end - start + 1) - 10`).
   - Otherwise, the bottom border SHALL be a bare `└──…──┘`.

Box width SHALL be `min(available-width, max(80, longest-body-line + decoration-w))`, where `decoration-w` is 4 (matching `│ ` + ` │`) and `available-width` is the selected window's `gfm-pretty--available-width` (falling back to `fill-column` or 80).

The markdown label (`[label]` portion of the link) SHALL NOT appear anywhere in the rendered preview surface.

When `<end>` is omitted, the range SHALL be `<start>` to `<start>` (single line).

When the file does not exist or the range is invalid, the overlay SHALL render as a bare single-line sentinel — no box, no border — propertised with `shadow` face, of the form `[broken preview] <abbrev-path>:<start>-<end> — <reason>` where `<reason>` is `file not found` or `invalid range`.

The major-mode resolution SHALL be a pure function of `<path>` via `auto-mode-alist`; when no entry matches, fontification SHALL fall back to `fundamental-mode`.

The underlying buffer text SHALL NOT be modified — overlays use `display` properties only.

#### Scenario: small range renders fontified body inside a box

- **GIVEN** a standalone link `[fn](modules/auth.rs#L42-L48)` on its own line
- **AND** lines 42-48 of `modules/auth.rs` are 7 lines of Rust code
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region
- **AND** the overlay's `display` string contains the 7 source lines
- **AND** the display string begins with `┌` and ends with `┘`
- **AND** at least one character in the body carries a `face` text property assigned by `rust-mode` (or `rust-ts-mode` per `auto-mode-alist`)

#### Scenario: file not found renders bare sentinel

- **GIVEN** a standalone link `[x](does-not-exist.rs#L1-L5)`
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string is a single line
- **AND** contains `[broken preview]`
- **AND** contains `file not found`

#### Scenario: oversize range renders with bottom-border footer

- **GIVEN** a standalone link `[x](file.rs#L1-L40)` whose file has at least 40 lines
- **WHEN** previews are rendered
- **THEN** the overlay's body contains the first 10 lines of the file
- **AND** the bottom border embeds `+30 more lines`

### Requirement: Diff-range link preview overlay rendering

For each standalone link whose URL matches `diff:<base>...<head>` (with optional `#<path>` fragment), the `link-previews` decorator SHALL place an overlay on the link's full markdown expression whose `display` property is a propertised multi-line string composed of:

1. A **top border** in `gfm-pretty-border-face` of the form `┌─ <base>...<head> ──…──┐` when the URL has no path qualifier, or `┌─ <base>...<head> — <path> ──…──┐` when path-scoped.  Refs matching `(rx bos (= 40 hex) eos)` SHALL be shortened to their first 7 characters before being embedded in the label; branch names and tags pass through unchanged.
2. A **body** of up to 10 lines from `git diff <base>...<head> [-- <path>]` executed from the buffer's worktree.  Body lines SHALL render in **LHS-margin mode**: each line is prefixed with a single `│` (no left padding) and suffixed with `│`, so the first body column is the `+`/`-`/` ` diff indicator.  Box interior decoration width is 2 cols (matching authored ` ```diff ` fences via `gfm-pretty-fences--lhs-margin-langs`).
3. Body-line truncation as for source previews, applied to the interior width minus 1 cell with a trailing `…`.
4. A **bottom border** in `gfm-pretty-border-face`.
   - When the diff exceeds the 10-line cap, the bottom border SHALL embed `+N more lines`.
   - Otherwise, the bottom border SHALL be a bare `└──…──┘`.

When `git diff` produces no output, the overlay SHALL render as a bare single-line sentinel — no box — propertised with `shadow` face, of the form `[broken preview] <base>...<head>[<path>] — no changes`.

When `git` exits non-zero, the overlay SHALL render as a bare single-line sentinel of the form `[broken preview] <base>...<head>[<path>] — git error: <first-error-line>`.

The markdown label SHALL NOT appear anywhere in the rendered preview surface.

The box width formula is identical to source-range previews, substituting `decoration-w` 2 for LHS-margin mode.

#### Scenario: diff link renders box-bordered preview

- **GIVEN** a standalone link `[change](diff:HEAD~1...HEAD#auth.rs)`
- **AND** `git diff HEAD~1...HEAD -- auth.rs` produces 6 lines of output
- **WHEN** previews are rendered
- **THEN** an overlay covers the link's region
- **AND** the overlay's `display` string begins with `┌` and ends with `┘`
- **AND** the display contains those 6 diff lines
- **AND** the body lines begin directly with `│+`, `│-`, or `│ ` (no left padding between `│` and the indicator)

#### Scenario: SHA refs shortened to 7 chars

- **GIVEN** a standalone link with two 40-char hex SHAs as base and head
- **WHEN** previews are rendered
- **THEN** the overlay's top border contains the first 7 chars of each SHA
- **AND** does not contain either 40-char SHA in full

#### Scenario: empty diff renders bare sentinel, no box

- **WHEN** a standalone diff link's `git diff` invocation produces no output
- **THEN** the overlay's `display` string is a single line
- **AND** contains `[broken preview]` and `no changes`

### Requirement: Link previews refresh on edit and window-configuration change

The `link-previews` decorator SHALL participate in the engine's standard rebuild lifecycle:

- `after-change-functions` triggers `:full-rebuild-required-p`; the decorator returns non-nil for any edit (link detection is buffer-position-sensitive enough that incremental scoping isn't worth the bookkeeping).
- `window-configuration-change-hook` triggers per-window display overlay re-rendering so box widths track the active window's `gfm-pretty--available-width`.

The decorator SHALL NOT install file-system watchers, idle timers (beyond the engine's existing rebuild debounce), or any other passive refresh mechanism.

#### Scenario: edit triggers preview rebuild

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone source-range link rendered
- **WHEN** the user edits the link's URL to point at a different file
- **THEN** the engine schedules a rebuild
- **AND** the next redisplay shows the updated preview

#### Scenario: window resize triggers preview re-render

- **GIVEN** a rendered preview overlay in window W of width N
- **WHEN** W's width changes to M
- **THEN** the preview overlay's box width re-clamps to `min(M, max(80, longest-body-line + decoration-w))`
