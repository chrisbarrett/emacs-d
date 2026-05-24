## MODIFIED Requirements

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

**Marker-aware indent.**  When the link span begins at a buffer position whose preceding characters on the same line are non-empty (the line carries a list-item marker `- `, `* `, `+ `, `<n>. `, or a blockquote marker `> ` before the link), every line of the display string — top border, every body line, and bottom border — SHALL be prefixed with `(make-string indent ?\s)` where `indent = (link-start - (line-beginning-position))`.  This aligns the box top, body, and bottom border at the same column, immediately to the right of the marker glyph.  Lines whose link span starts at the line's beginning (`indent = 0`) SHALL render without leading indent (unchanged from prior behaviour).

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

#### Scenario: list-item preview aligns under marker

- **GIVEN** a line `- [fn](modules/auth.rs#L1-L3)`
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string starts with two leading spaces before `┌`
- **AND** every body line starts with two leading spaces before `│`
- **AND** the bottom border starts with two leading spaces before `└`

#### Scenario: blockquote preview aligns under marker

- **GIVEN** a line `> [fn](modules/auth.rs#L1-L3)`
- **WHEN** previews are rendered
- **THEN** the overlay's `display` string starts with two leading spaces before `┌`
- **AND** every body line starts with two leading spaces before `│`
- **AND** the bottom border starts with two leading spaces before `└`


### Requirement: Diff-range link preview overlay rendering

For each standalone link whose URL matches `diff:<base>...<head>` (with optional `#<path>` fragment), the `link-previews` decorator SHALL place an overlay on the link's full markdown expression whose `display` property is a propertised multi-line string composed of:

1. A **top border** in `gfm-pretty-border-face` of the form `┌─ <base>...<head> ──…──┐` when the URL has no path qualifier, or `┌─ <base>...<head> — <path> ──…──┐` when path-scoped.  Refs matching `(rx bos (= 40 hex) eos)` SHALL be shortened to their first 7 characters before being embedded in the label; branch names and tags pass through unchanged.
2. A **body** of up to 10 lines from `git diff <base>...<head> [-- <path>]` executed from the buffer's worktree.  Body lines SHALL render in **LHS-margin mode**: each line is prefixed with a single `│` (no left padding) and suffixed with `│`, so the first body column is the `+`/`-`/` ` diff indicator.  Box interior decoration width is 2 cols (matching authored ` ```diff ` fences via `gfm-pretty-fences--lhs-margin-langs`).  Body lines SHALL be fontified by activating `diff-mode` in a temp buffer, inserting the joined lines, running `font-lock-ensure`, and capturing the resulting `face` text properties.  Added lines SHALL carry `diff-added` (or its mode-resolved family), removed lines `diff-removed`, hunk headers `diff-hunk-header`, file headers `diff-file-header`, and context lines `diff-context` — the `diff-mode` defaults.
3. Body-line truncation as for source previews, applied to the interior width minus 1 cell with a trailing `…`.
4. A **bottom border** in `gfm-pretty-border-face`.
   - When the diff exceeds the 10-line cap, the bottom border SHALL embed `+N more lines`.
   - Otherwise, the bottom border SHALL be a bare `└──…──┘`.

When `git diff` produces no output, the overlay SHALL render as a bare single-line sentinel — no box — propertised with `shadow` face, of the form `[broken preview] <base>...<head>[<path>] — no changes`.

When `git` exits non-zero, the overlay SHALL render as a bare single-line sentinel of the form `[broken preview] <base>...<head>[<path>] — git error: <first-error-line>`.

The markdown label SHALL NOT appear anywhere in the rendered preview surface.

The box width formula is identical to source-range previews, substituting `decoration-w` 2 for LHS-margin mode.

**Marker-aware indent** applies identically to diff previews: when the link span follows a list-item or blockquote marker on the same line, the top border, every body line, and the bottom border SHALL be prefixed with `(make-string indent ?\s)`.

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

#### Scenario: diff body carries diff-mode faces

- **GIVEN** a standalone diff link whose `git diff` output contains at least one `+`-prefixed line and one `-`-prefixed line
- **WHEN** previews are rendered
- **THEN** at least one position inside an added line carries a `face` text property derived from `diff-added`
- **AND** at least one position inside a removed line carries a `face` text property derived from `diff-removed`


### Requirement: Blockquote left-rail rendering

The blockquotes decorator's `:apply-block-fn` SHALL render the rail with an inset gutter:

- A per-line anchor overlay whose `before-string` is `<inset-spaces>` in the default face, painting the leading gutter.  The before-string MUST live on the anchor (not on the per-window prefix display) so reveal exposing the raw `> ` source does NOT also drop the gutter — without this, point-on-line would visually unshift the body text by `inset-cols` columns.
- A per-line anchor overlay whose `wrap-prefix` is the propertised string `<inset-spaces>▌<space>` (inset in default face, `▌` in `gfm-pretty-blockquotes-rail-face`, trailing space unfaced) so soft-wrapped visual continuation rows show the inset gutter and rail.  Continuation visual rows have no buffer char at column 0 to host a before-string, so the inset is baked into the wrap-prefix.
- A per-window display overlay substituting the two-char `> ` prefix on each blockquote line with `▌<space>` (rail in `gfm-pretty-blockquotes-rail-face`, trailing space unfaced).
- A per-window display overlay substituting the one-char bare `>` form with `▌` (no trailing space — matches the 1-char source).

**Suppression on link-previews lines.**  When a blockquote source line is fully covered by a `link-previews` display overlay (detected by walking `overlays-in` for the line and checking for the property name returned by `(gfm-pretty--registry-display gfm-pretty-link-previews--registry)`), the blockquotes decorator SHALL NOT lay any overlay (anchor or display) for that line.  Plain blockquote lines without a link-previews overlay keep all rail overlays as specified.  This avoids the visual duplication of two left borders (`▌` rail + `┌` box edge) on a `> [link](url)` preview line.

The decorator SHALL NOT paint a tinted background on the blockquote rectangle, a top / bottom border, or a right-edge terminator after-string.  An earlier draft applied a tinted background spanning each line's content + an after-string padding the bg out to the window margin — it was removed after the user found that Emacs' `:extend t` paints past EOL only on the visual row containing EOL, so a word-wrapped blockquote's intermediate continuation rows were left with default-bg cells between content and the intended right edge (a ragged stepped appearance).  Without per-visual-row padding mechanics (which Emacs does not expose), no clean rectangle is reachable; the rail-only treatment side-steps the issue.

The decorator's `:apply-block-fn` SHALL call `gfm-pretty-borders--apply-with-anchors` (or an equivalent engine seam) so anchor overlays are laid at most once per (block, rebuild pass) while per-window display overlays apply once per window.

#### Scenario: Plain blockquote

- **GIVEN** `> Pain: clutter` with `gfm-pretty-blockquotes-inset-cols` = 4
- **WHEN** the decorator renders
- **THEN** the line shows four columns of leading gutter (default face), then `▌ Pain: clutter` with the rail in `gfm-pretty-blockquotes-rail-face`
- **AND** no raw `>` is visible on that line

#### Scenario: Bare-`>` line in middle of block

- **GIVEN** `> first paragraph\n>\n> second paragraph`
- **WHEN** the decorator renders
- **THEN** the middle line shows the inset gutter followed by `▌` at column `inset-cols` with no raw `>` visible

#### Scenario: Soft-wrapped long line

- **GIVEN** a single source line `> ` followed by 200 characters of text, displayed in a window with `visual-line-mode` enabled
- **WHEN** the decorator renders
- **THEN** visual row 1 shows the inset gutter then `▌ ` then the first chunk of text
- **AND** continuation visual rows show the inset gutter then `▌ ` then the wrapped continuation (the `wrap-prefix` overlay wins over `markdown-mode`'s `wrap-prefix "> "` text property)

#### Scenario: blockquote line covered by link-previews overlay omits rail

- **GIVEN** a line `> [fn](modules/auth.rs#L1-L3)` with both `gfm-pretty-blockquotes` and `gfm-pretty-link-previews` enabled
- **WHEN** the decorators render
- **THEN** no `gfm-pretty-blockquotes` overlay (anchor or display) covers any part of that line
- **AND** the `link-previews` overlay's box is the only left edge visible on that line
- **AND** adjacent `>` lines without a preview overlay still carry the rail


## ADDED Requirements

### Requirement: Link previews — RET dispatch

Every `link-previews` decorator overlay SHALL carry a `keymap` text property binding `RET` (and `<return>`) to `gfm-pretty-link-previews-follow-link-at-point`.

`gfm-pretty-link-previews-follow-link-at-point` SHALL:

- Identify the preview overlay at point via the `gfm-pretty-link-previews-display` overlay property; signal a `user-error` when none is found.
- For source-range overlays (`gfm-pretty-link-previews-kind` is `source`): resolve the path against the buffer's `default-directory` if relative, then `find-file` the target, narrow nothing, move point to the start line, and pulse the requested range when `pulsar-highlight-pulse` is `fboundp`.
- For diff-range overlays (`gfm-pretty-link-previews-kind` is `diff`): `(require 'magit nil t)`; when `magit-diff-range` is `fboundp` call it with `<base>...<head>` (no extra args) and, when the parsed URL has a `:path`, pass `(list path)` as the files argument.  When `magit-diff-range` is absent, fall back to populating a `*Diff*` buffer with `git -C <worktree> diff <base>...<head> [-- <path>]` output and `pop-to-buffer` it in `diff-mode`.
- Before navigating, push the current point onto the local mark ring via `push-mark`.

The overlay-borne keymap SHALL be active only while point is inside the overlay; outside the overlay, the buffer's normal RET binding (typically `markdown-follow-link-at-point`) SHALL run unmodified.

`gfm-present-follow-link` (presentation-mode's RET) MAY delegate to `gfm-pretty-link-previews-follow-link-at-point` when the link at point is a preview-eligible standalone source-range or diff-range link, OR continue to use its own dispatch — both routes SHALL produce the same observable outcome.

#### Scenario: RET on source preview opens target

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone source-range preview overlay rendered
- **WHEN** the user presses RET with point inside the overlay's span
- **THEN** the target file is opened in another window
- **AND** point in the target buffer is at the start of the requested start line
- **AND** the start position is pushed onto the originating buffer's mark ring

#### Scenario: RET on diff preview dispatches to magit when available

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone diff-range preview overlay rendered
- **AND** `magit-diff-range` is `fboundp`
- **WHEN** the user presses RET with point inside the overlay's span
- **THEN** `magit-diff-range` is called with `<base>...<head>`
- **AND** when the link has a `:path`, the call includes `(list <path>)` as the files argument

#### Scenario: RET on diff preview falls back to `*Diff*` buffer when magit is absent

- **GIVEN** a buffer with `gfm-pretty-mode` enabled and a standalone diff-range preview overlay rendered
- **AND** `magit-diff-range` is NOT `fboundp`
- **WHEN** the user presses RET with point inside the overlay's span
- **THEN** a `*Diff*` buffer is populated from `git diff <base>...<head> [-- <path>]` run in the worktree
- **AND** the buffer is shown in `diff-mode`

#### Scenario: RET outside any preview overlay falls through

- **GIVEN** a buffer with `gfm-pretty-mode` enabled, a preview overlay rendered, and point on a non-overlay link in the same buffer
- **WHEN** the user presses RET
- **THEN** the preview decorator's keymap does not fire
- **AND** the buffer's normal RET binding handles the link
