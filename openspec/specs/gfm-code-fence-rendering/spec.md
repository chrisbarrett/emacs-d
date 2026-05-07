# gfm-code-fence-rendering Specification

## Purpose
TBD - created by archiving change gfm-code-fences-quality-parity. Update Purpose after archive.
## Requirements
### Requirement: Mode toggle

The system SHALL provide a buffer-local minor mode `gfm-code-fences-mode`
that applies and removes the fenced-block, YAML-helmet, and
indented-block decoration overlays as a unit.

#### Scenario: Enabling the mode in a markdown buffer

- **WHEN** `gfm-code-fences-mode` is enabled in a buffer containing
  fenced code blocks
- **THEN** every fenced block, YAML helmet, and indent block is
  decorated with a curved-border box and any applicable language icon

#### Scenario: Disabling the mode

- **WHEN** `gfm-code-fences-mode` is disabled in a buffer where it had
  decorated blocks
- **THEN** all overlays created by the mode are removed and the buffer
  visually matches its raw markdown source

### Requirement: Fenced block discovery

The system SHALL identify fenced code blocks by locating an opening
fence matching `^[[:blank:]]*```+` (optionally followed by `{` and a
language tag of `[A-Za-z0-9_-]+`), and a closing fence on a later line
whose backtick run is at least as long as the opening fence's.  The
language tag, when present, is captured for icon resolution and
language-mode lookup.

#### Scenario: Fence with language tag

- **WHEN** the buffer contains an opening fence `` ```bash `` followed
  by body lines and a closing `` ``` ``
- **THEN** the system identifies one block with language `"bash"`

#### Scenario: Fence without language tag

- **WHEN** the buffer contains an opening fence `` ``` `` followed by
  body lines and a closing `` ``` ``
- **THEN** the system identifies one block with no language tag

#### Scenario: Closing fence backtick count must match or exceed opening

- **WHEN** the buffer contains an opening four-backtick fence and a
  three-backtick line later in the body
- **THEN** the three-backtick line does not close the block; closure
  requires four or more backticks

### Requirement: YAML helmet discovery

The system SHALL identify a leading YAML frontmatter block when the
buffer's first non-empty line is `---` (with optional trailing
whitespace) and a later line is `---` of the same shape, treating the
intervening lines as the YAML body.

#### Scenario: Leading YAML frontmatter

- **WHEN** the buffer begins with a `---` line, key/value lines, and a
  closing `---` line
- **THEN** the system identifies the region as a YAML helmet with the
  intervening text as its body

#### Scenario: Mid-buffer `---` does not start a helmet

- **WHEN** the first non-empty line of the buffer is prose, and a
  later region is bracketed by `---` lines
- **THEN** the system does not treat that region as a YAML helmet

### Requirement: Indented block discovery

The system SHALL identify indented code blocks as runs of lines whose
first character is either four leading spaces or a single tab,
preceded by a blank line (or the start of the buffer), terminating
at the next blank or non-indented line.  Indent width is recorded as
4 for spaces or 1 for tabs.

#### Scenario: Standard 4-space indent block

- **WHEN** a paragraph is followed by a blank line, then several
  4-space-indented lines, then another blank line
- **THEN** the system identifies the indented run as one block with
  indent width 4

#### Scenario: Tab-indented block

- **WHEN** the indented run uses a single leading tab on each line
- **THEN** the system identifies one block with indent width 1

### Requirement: Mutual exclusion of indent inside fence

The system SHALL skip indented-block discovery for any line whose
position falls inside an already-discovered fenced or YAML block, so
indented-looking content inside a fenced code block is not double-counted.

#### Scenario: Indented-looking text inside a fenced block

- **WHEN** a fenced block contains a body line beginning with four
  spaces
- **THEN** the system does not produce a separate indented block for
  that line

### Requirement: Bordered-block rendering

The system SHALL adorn every discovered block (fenced, YAML helmet,
or indented) with a curved-border box composed of:

- a top border (`┌─…─┐`) on or above the opening line
- a left edge `│ ` prefix on every body line, applied as a
  `before-string` for fenced and YAML blocks and as a covering
  `display` for indent blocks (which have no marker line)
- a right edge sized to the block's box width via an `after-string`
  with `display: space :align-to`
- a bottom border (`└─…─┘`) on or below the closing line
- a `wrap-prefix` of `⋱ ` on continuation visual lines so wrapped
  content stays visually inside the box

The border face is `+markdown-overlay-border-face` for fenced and
indent blocks, and `font-lock-constant-face` for YAML helmets.

#### Scenario: Fenced block renders with curved box and icon

- **WHEN** a fenced block with a recognised language tag is decorated
- **THEN** the rendered output has a top border whose right-aligned
  position carries the nerd-icons icon for that language's major
  mode, body lines bracketed by `│ ` and right-aligned `│`, and a
  matching bottom border

#### Scenario: YAML helmet renders with `meta` label

- **WHEN** a YAML helmet is decorated
- **THEN** the rendered output has a top border whose right-aligned
  position carries the bold label `meta`, body lines bracketed by `│
  ` and a right-aligned `│`, and a matching bottom border

#### Scenario: Indent block renders with full-width borders

- **WHEN** an indent block is decorated
- **THEN** the rendered output has a top border whose entire width
  precedes the first body line (no marker line to share), and a
  matching bottom border after the last body line

### Requirement: Box width sizing

The system SHALL size each block's border box width as
`min(text-width, max(80, max-content + 4))`, where `text-width` is
`window-max-chars-per-line` for the target window (falling back to
`fill-column` and then 80), and `max-content` is the longest body
line's visible width minus the block's indent.  Body lines whose
visible width exceeds the inner content budget are wrapped natively
by Emacs using the overlay's `wrap-prefix`.

#### Scenario: Box clamps to window width on a narrow split

- **WHEN** a block whose natural content fits 100 columns is rendered
  in a window with `window-max-chars-per-line = 60`
- **THEN** the border box renders at width 60, body lines wrap with
  the `⋱ ` prefix, and the right-edge `│` aligns to column 60

#### Scenario: Box floors at 80 columns when content is short

- **WHEN** a block's `max-content + 4 = 30` and `text-width = 120`
- **THEN** the border box renders at width 80

### Requirement: Wrap simulation always terminates

The system SHALL guarantee that `gfm-code-fences--simulate-wrap` makes
forward progress on every iteration, even when invoked with a
`text-width` smaller than the wrap-prefix width.  The function SHALL
clamp the per-iteration `line-width` to at least 1 so the position
counter advances by at least one character every loop body.

#### Scenario: Simulate-wrap with text-width 0

- **WHEN** `gfm-code-fences--simulate-wrap` is called with
  `width = 0` and a non-empty text
- **THEN** the function returns rather than entering an infinite loop

#### Scenario: Simulate-wrap with text-width 1 and longer wrap-prefix

- **WHEN** the function is called with `width = 1` and
  `cont-prefix-w = 2` (so the unguarded continuation-line width
  would be `-1`)
- **THEN** the function returns with a wrap-position list rather
  than spinning

### Requirement: Marker line reveal on cursor entry

The system SHALL suppress the display of the top and bottom marker
overlays of a fenced or YAML block when point lies on that marker
line, exposing the underlying source ` ``` ` or `---` for editing.
The display SHALL be restored when point leaves the marker line.
Indent blocks have no marker line and therefore no reveal.

#### Scenario: Entering a fence marker reveals source

- **WHEN** point moves onto the line containing an opening or
  closing ` ``` ` of a decorated fenced block
- **THEN** the marker overlay's display is suppressed and the raw
  backticks (with any language tag) become visible

#### Scenario: Leaving a fence marker restores decoration

- **WHEN** point moves off a previously-revealed fence marker line
- **THEN** the marker overlay's display is restored

### Requirement: Language icon resolution

The system SHALL right-align a `nerd-icons` icon in the top border of
a fenced block when its language tag resolves to a known major mode.
Language tags SHALL be matched case-insensitively against
`markdown-code-lang-modes`, then against a `<lang>-ts-mode` tree-sitter
variant when one is `fboundp`, then to a fallback `<lang>-mode` symbol.

#### Scenario: Recognised alias resolves to a language icon

- **WHEN** the language tag is `py`, `rb`, `cs`, or `cpp`
- **THEN** the resolved major mode is `python-mode`, `ruby-mode`,
  `csharp-mode`, or `c++-mode` respectively, and the rendered icon
  is the nerd-icons glyph for that mode

#### Scenario: Case-insensitive matching

- **WHEN** the language tag is `Python`, `C#`, or `JavaScript`
- **THEN** the resolved major mode matches the lowercase alias

#### Scenario: Unknown tag falls back to `<lang>-mode`

- **WHEN** the language tag is `totally-made-up`
- **THEN** the resolved major mode is `totally-made-up-mode`

### Requirement: YAML body fontification

The system SHALL apply `face` overlays to a YAML helmet's body using
font-lock from a yaml major mode, preferring `yaml-ts-mode` when its
tree-sitter grammar is available, then `yaml-mode` when `fboundp`,
otherwise leaving the body unfontified.

#### Scenario: Tree-sitter yaml available

- **WHEN** the active Emacs has `yaml-ts-mode` and a registered yaml
  tree-sitter grammar
- **THEN** YAML body characters carry face overlays produced by
  `yaml-ts-mode` font-lock

#### Scenario: Empty YAML body does not error

- **WHEN** a buffer contains `---\n---\n` (a helmet with no body)
- **THEN** enabling `gfm-code-fences-mode` completes without raising

### Requirement: Per-window rendering

The system SHALL produce display overlays sized to each window
currently showing the buffer, so the same buffer split across
multiple windows (or shared between graphical and terminal frames)
renders at each window's own width.  Anchor overlays carry
width-independent props (`before-string '│ '`, `wrap-prefix '⋱ '`,
`cursor-intangible` on indent body) and are shared across windows.
Display overlays carry width-dependent props (top/bottom border
splits, right-edge `:align-to` after-strings) and SHALL be restricted
to a single window via Emacs's `window` overlay property.

When no window currently displays the buffer (e.g. background
buffer), the system SHALL produce a single unrestricted display
overlay set at a sane fallback width and replace it with
window-restricted overlays as soon as a window starts showing the
buffer.

#### Scenario: Buffer split across windows of different widths

- **WHEN** the buffer is shown in two windows whose
  `window-max-chars-per-line` differ
- **THEN** each window renders the block sized to its own width,
  with one set of display overlays per window

#### Scenario: Anchor overlays are shared across windows

- **WHEN** the buffer is shown in N windows
- **THEN** each body line is covered by exactly one anchor overlay
  carrying the `│ ` `before-string` and `⋱ ` wrap-prefix, regardless
  of N

### Requirement: Block-discovery cache

The system SHALL memoise each of the three block-discovery functions
(`--find-blocks`, `--find-yaml-helmet`, `--find-indent-blocks`) by
`buffer-chars-modified-tick`, so repeat calls without an intervening
edit reuse the cached scan.  Indented-block discovery still accepts
its excluded-fenced-ranges parameter as call-site data, not part of
the cache key.

#### Scenario: Repeated find-blocks calls without edits return cached result

- **WHEN** `--find-blocks` is called twice with no buffer modification
  in between
- **THEN** both calls return `eq` block lists

#### Scenario: Edit invalidates cache

- **WHEN** the buffer is modified between two calls to `--find-blocks`
- **THEN** the second call returns a fresh block list reflecting the
  new state

### Requirement: Debounced rebuild

The system SHALL respond to buffer modifications and window-state
changes via a 0.2-second idle timer, SHALL skip rebuilds in indirect
buffers (those with a non-nil `buffer-base-buffer`), and SHALL react
to window-state changes only when an actual rendering input has
changed (width of an existing window, or arrival/departure of a
window showing the buffer).

#### Scenario: Indirect buffer skipped

- **WHEN** the current buffer has a base buffer
- **THEN** the rebuild scheduler does not schedule a rebuild for the
  indirect buffer

#### Scenario: Window-config event with no width effect

- **WHEN** a window-configuration event fires that does not change
  any window's `window-max-chars-per-line` and does not add or remove
  a window showing the buffer (e.g. minibuffer activity, focus shift)
- **THEN** no rebuild is scheduled

### Requirement: Scoped post-edit rebuild

The system SHALL rebuild only the decorated block whose source range
fully contains the dirty region when the dirty region intersects
exactly one block.  The system SHALL fall back to a full-buffer
rebuild when:

- the changed region overlaps a fence opening or closing line, or
- the changed region overlaps a blank line adjacent to an indent
  block, or
- the changed region intersects more than one block.

When the changed region intersects no decorated block, the system
SHALL NOT rebuild.

#### Scenario: Edit inside one fenced block

- **WHEN** the user modifies a body line inside one decorated fenced
  block and waits for idle
- **THEN** only that block's overlays are rebuilt; other decorated
  blocks in the buffer retain their existing overlay objects unchanged

#### Scenario: Edit on a fence boundary triggers full rebuild

- **WHEN** the user inserts or deletes characters on the opening
  ` ``` ` line of a fenced block and waits for idle
- **THEN** the system performs a full-buffer rebuild

#### Scenario: Edit on a blank line adjacent to an indent block triggers full rebuild

- **WHEN** the user edits a blank line whose adjacency gates an
  indent block's discovery and waits for idle
- **THEN** the system performs a full-buffer rebuild

#### Scenario: Edit outside every decorated block

- **WHEN** the user inserts text in a region that does not intersect
  any decorated block and waits for idle
- **THEN** no overlays are rebuilt

### Requirement: Selective per-window reconciliation

The system SHALL act on a per-window diff when reconciling display
overlays after a window-state change: only added or resized windows
trigger fresh rendering, and only removed windows have their display
overlays deleted.  Display overlays belonging to windows whose width
is unchanged SHALL retain their existing overlay objects.

#### Scenario: Resizing one of two windows leaves the other untouched

- **WHEN** the buffer is shown in two windows and only one window's
  width changes
- **THEN** the resized window's display overlays are replaced with
  fresh ones, while the other window's display overlay objects
  survive `eq` comparison

#### Scenario: Closing a window cleans up only its overlays

- **WHEN** a window showing the buffer is deleted
- **THEN** the deleted window's display overlays are removed at the
  next reconciliation, while overlays for surviving windows are not
  disturbed

#### Scenario: Opening a new window on the buffer renders just that window

- **WHEN** a fresh window starts showing a buffer that already has
  decorated blocks
- **THEN** display overlays for the new window are produced (visible
  blocks first, off-screen on the next idle); existing display
  overlays for already-shown windows are not rebuilt

### Requirement: Visible-first prioritised rebuild

The system SHALL prioritise visible-window blocks when rebuilding
display overlays in response to a window-state change, rendering
blocks that intersect any visible window range immediately and
deferring off-screen blocks to the next idle tick.

#### Scenario: Visible blocks render before off-screen blocks

- **WHEN** a window resize fires a rebuild and the buffer contains
  blocks both inside and outside the visible window range
- **THEN** display overlays for blocks intersecting any visible
  window range are recreated synchronously, and overlays for the
  remaining blocks are recreated on a follow-up idle timer

### Requirement: Performance instrumentation

The system SHALL maintain per-buffer performance statistics covering
rebuild count, total duration, last duration, max duration, block
count from the most recent rebuild, and a per-phase breakdown
(`find-fenced`, `find-yaml`, `find-indent`, `compose-borders`,
`compose-overflow`, `apply`).  The system SHALL surface these
statistics via a command `gfm-code-fences-stats`.

#### Scenario: Stats accumulate across rebuilds

- **WHEN** the rebuild has run multiple times in a buffer
- **THEN** `gfm-code-fences-stats` reports the cumulative count,
  total duration, last and max single-rebuild durations, and the
  block count from the most recent rebuild

#### Scenario: Phase breakdown surfaced

- **WHEN** `gfm-code-fences-stats` is invoked after one or more
  rebuilds
- **THEN** the report includes a per-phase total covering at least
  `find-fenced`, `find-yaml`, `find-indent`, `compose-borders`,
  `compose-overflow`, and `apply`

#### Scenario: Slow rebuild emits a warning

- **WHEN** a single rebuild duration exceeds
  `gfm-code-fences-slow-rebuild-threshold` (default 0.05 s)
- **THEN** the system emits a `message` line identifying the buffer
  and the duration

