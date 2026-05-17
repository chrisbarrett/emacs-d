# lang-markdown Specification

## Purpose

Govern how GitHub Flavored Markdown elements — callout blockquotes, fenced
code blocks, YAML front-matter helmets, indented code blocks, and pipe
tables — are rendered in Emacs buffers using overlays. Covers block
discovery, overlay decoration, per-window sizing, cursor reveal, cell
editing, and rebuild scheduling for all GFM minor modes in
`modules/lang-markdown`.

---

## Requirements

<!-- ── Callouts ─────────────────────────────────────────────────────────── -->

### Requirement: Callout mode toggle

The system SHALL provide a buffer-local minor mode `gfm-callouts-mode`
that applies and removes the bordered-callout decoration overlays as
a unit.

#### Scenario: Enabling the mode in a markdown buffer

- **WHEN** `gfm-callouts-mode` is enabled in a buffer containing GFM
  callout blockquotes
- **THEN** every recognised callout (NOTE, TIP, IMPORTANT, WARNING,
  CAUTION, CRITICAL) is decorated with a curved-border box, a
  type-coloured top label, and a tinted background

#### Scenario: Disabling the mode

- **WHEN** `gfm-callouts-mode` is disabled in a buffer where it had
  decorated callouts
- **THEN** every overlay created by the mode is removed and the
  buffer visually matches its raw markdown source

### Requirement: Callout block discovery

The system SHALL identify a callout block as a marker line of the
form `> [!TYPE]` where `TYPE` is one of `NOTE`, `TIP`, `IMPORTANT`,
`WARNING`, `CAUTION`, or `CRITICAL`, followed by zero or more
contiguous blockquote lines beginning with `>`.

#### Scenario: Marker followed by body lines

- **WHEN** the buffer contains `> [!IMPORTANT]\n> Lorem ipsum.\n`
- **THEN** the system identifies one block of type `IMPORTANT`
  whose body covers `> Lorem ipsum.`

#### Scenario: Marker with no body

- **WHEN** the buffer contains `> [!NOTE]\n` followed by a blank
  line
- **THEN** the system identifies one block of type `NOTE` with no
  body lines

#### Scenario: Unknown marker is ignored

- **WHEN** the buffer contains `> [!FOO]\n> body\n`
- **THEN** the system does not identify a callout block

### Requirement: Callout bordered-block rendering

The system SHALL adorn every discovered callout with a curved
border composed of:

- a top border `┌─ TYPE ─…─┐` covering the marker line and
  extending across a window-sized width
- a left edge `│ ` substituting the `> ` prefix on every body line
- a right edge sized to the box width via an after-string
  containing `space :align-to`
- a bottom border `└─…─┘` on its own visual row beneath the last
  body line (or beneath the marker line when the body is empty)
- a `wrap-prefix` of `│ ` on continuation visual lines so wrapped
  content stays visually inside the box
- a tinted background face derived by blending the type face's
  foreground 10 % toward the default background, painted across
  every line of the callout
- a per-line right-edge fill that paints the visual line to the
  window edge in the default face, so any `:extend t` background —
  whether carried by a text property or by another mode's overlay
  (such as `hl-line` or `region`) — has no past-EOL region left to
  fill and cannot extend past the right border

#### Scenario: Callout renders with curved box and label

- **WHEN** an `[!IMPORTANT]` callout with two body lines is
  decorated
- **THEN** the rendered output has a top border whose left segment
  reads `┌─ IMPORTANT ─` and right segment is dash fill ending in
  `┐`, body lines bracketed by `│ ` and a right-aligned `│`, and a
  matching bottom border on its own row

#### Scenario: Body-less callout renders with bottom border on marker line

- **WHEN** an `[!NOTE]` callout with no body lines is decorated
- **THEN** the bottom border attaches to the marker line's trailing
  after-string so the box still closes

#### Scenario: Body line with an extend background does not leak past the border

- **WHEN** a callout body line carries a face with `:extend t` — for
  example `hl-line` while point is on that line
- **THEN** the `:extend` background is confined to the box interior and
  does not paint past the right-edge `│` to the window edge

### Requirement: Callout box width sizing

The system SHALL size each callout's border box width as
`min(text-width, max(80, max-content + 4))`, where `text-width` is
`window-max-chars-per-line` for the target window (falling back to
`fill-column` and then 80), and `max-content` is the longest body
line's visible width minus the 2-character `> ` prefix.  Body
lines whose visible width exceeds the inner content budget SHALL
wrap natively via the overlay's `wrap-prefix`.

#### Scenario: Box clamps to window width on a narrow split

- **WHEN** a callout whose natural content fits 100 columns is
  rendered in a window with `window-max-chars-per-line = 60`
- **THEN** the border box renders at width 60, body lines wrap with
  the `│ ` prefix, and the right-edge `│` aligns to column 60

#### Scenario: Box floors at 80 columns when content is short

- **WHEN** a callout's `max-content + 4 = 30` and `text-width = 120`
- **THEN** the border box renders at width 80

### Requirement: Callout wrapped right-edge alignment

The system SHALL pad the right-edge after-string of every body line
to the box width even when the line wraps, by simulating word-wrap
of `│ ` + the body content (account for the 2-column wrap-prefix
on continuation visual lines) and computing the residual padding
needed to reach `box-width - 1`.

#### Scenario: Long body line wraps and closes on the wrapped row

- **WHEN** a body line whose visible width exceeds `box-width - 4`
  is rendered
- **THEN** the line wraps with the `│ ` wrap-prefix on each
  continuation visual row and the right-edge `│` lands at column
  `box-width - 1` of the final wrapped row

### Requirement: Callout marker line and body prefix reveal

The system SHALL suppress the display of the marker-line top-border
overlay and the per-body-line `│ ` prefix overlay when point lies
on that line, exposing the underlying source `> [!TYPE]` or `> `
for editing.  The display SHALL be restored when point leaves the
line.

#### Scenario: Entering the marker line reveals source

- **WHEN** point moves onto the marker line of a decorated callout
- **THEN** the marker overlay's display is suppressed and the raw
  `> [!TYPE]` becomes visible

#### Scenario: Entering a body line reveals the `> ` prefix

- **WHEN** point moves onto a decorated body line within the
  callout
- **THEN** the `│ ` substitution overlay's display is suppressed
  and the raw `> ` becomes visible

#### Scenario: Leaving the line restores decoration

- **WHEN** point moves off a previously-revealed marker or body
  line
- **THEN** the overlay's display is restored

### Requirement: Callout per-window rendering

The system SHALL produce display overlays sized to each window
currently showing the buffer, so the same buffer split across
multiple windows (or shared between graphical and terminal frames)
renders at each window's own width.  Anchor overlays carry
width-independent props (background tint, `│ ` substitution,
`wrap-prefix`) and are shared across windows.  Display overlays
carry width-dependent props (top-border splits, right-edge
after-strings, bottom border) and SHALL be restricted to a single
window via Emacs's `window` overlay property.

When no window currently displays the buffer, the system SHALL
produce a single unrestricted display overlay set at a sane
fallback width and replace it with window-restricted overlays as
soon as a window starts showing the buffer.

#### Scenario: Buffer split across windows of different widths

- **WHEN** the buffer is shown in two windows whose
  `window-max-chars-per-line` differ
- **THEN** each window renders the callout sized to its own width,
  with one set of display overlays per window

#### Scenario: Anchor overlays are shared across windows

- **WHEN** the buffer is shown in N windows
- **THEN** each body line is covered by exactly one anchor overlay
  carrying the `│ ` substitution and one anchor overlay carrying
  the tinted background face, regardless of N

### Requirement: Callout per-window cursor reveal

The system SHALL suppress display only on the selected window's
revealable overlays when point enters a marker or body line.
Revealable overlays whose `window` property does not match the
selected window SHALL keep their display.

#### Scenario: Cursor in window A does not reveal source in window B

- **WHEN** the buffer is shown in two windows A and B with the
  selected window A and point on a marker line
- **THEN** window A shows the raw `> [!TYPE]` source while window
  B continues to display the bordered top label

### Requirement: Callout block-discovery cache

The system SHALL memoise `gfm-callouts--find-blocks` by
`buffer-chars-modified-tick` so repeat calls without an
intervening edit reuse the cached scan.  The cache SHALL be a pure
function of buffer contents — narrowing or widening the buffer
SHALL NOT invalidate or alter cached results.

#### Scenario: Repeated find-blocks calls without edits return cached result

- **WHEN** `gfm-callouts--find-blocks` is called twice with no
  buffer modification in between
- **THEN** both calls return `eq` block lists

#### Scenario: Edit invalidates cache

- **WHEN** the buffer is modified between two calls to
  `gfm-callouts--find-blocks`
- **THEN** the second call returns a fresh block list reflecting
  the new state

#### Scenario: Narrowing does not invalidate cache

- **WHEN** `gfm-callouts--find-blocks` is called once in the widened
  buffer and the buffer is then narrowed without any modification
- **THEN** the next call returns the same `eq` block list (the
  widened scan, including callouts outside the current restriction)

### Requirement: Callout narrowing-resilient discovery and teardown

The system SHALL discover callouts and tear down its overlays
identically regardless of the buffer's current restriction.
`gfm-callouts--find-blocks-1` SHALL widen the restriction for the
duration of its scan; the buffer-wide teardown path used by the
mode's full rebuild and by `gfm-callouts-mode` disable SHALL clear
all overlays tagged by the mode regardless of restriction.

#### Scenario: Narrowed rebuild does not duplicate overlays

- **WHEN** `gfm-callouts-mode` is enabled in a widened buffer
  containing callouts in two separate top-level regions
- **AND** the buffer is then narrowed to a region containing only
  the first callout
- **AND** `gfm-callouts--rebuild` is invoked
- **AND** the buffer is subsequently widened
- **THEN** the count of overlays tagged `gfm-callouts` on the
  buffer equals the length of `gfm-callouts--overlays`

#### Scenario: Cached block list is narrowing-independent

- **WHEN** `gfm-callouts--find-blocks` is called once in the
  widened buffer and once after narrowing to a sub-region
- **THEN** the second call returns the same data as the first for
  the same `buffer-chars-modified-tick`

#### Scenario: Full teardown leaves no off-narrowing zombies

- **WHEN** the buffer is narrowed and
  `gfm-callouts--remove-overlays` is invoked with no
  `BEG`/`END` arguments
- **AND** the buffer is subsequently widened
- **THEN** no overlay tagged `gfm-callouts` remains on the buffer

### Requirement: Callout debounced rebuild

The system SHALL respond to buffer modifications and window-state
changes via a 0.2-second idle timer, SHALL skip rebuilds in
indirect buffers (those with a non-nil `buffer-base-buffer`), and
SHALL react to window-state changes only when an actual rendering
input has changed (width of an existing window, or
arrival/departure of a window showing the buffer).

#### Scenario: Indirect buffer skipped

- **WHEN** the current buffer has a base buffer
- **THEN** the rebuild scheduler does not schedule a rebuild for
  the indirect buffer

#### Scenario: Window-config event with no width effect

- **WHEN** a window-configuration event fires that does not change
  any window's `window-max-chars-per-line` and does not add or
  remove a window showing the buffer (e.g. minibuffer activity,
  focus shift)
- **THEN** no rebuild is scheduled

### Requirement: Callout scoped post-edit rebuild

The system SHALL rebuild only the decorated callout whose source
range fully contains the dirty region when the dirty region
intersects exactly one callout's body.  The system SHALL fall
back to a full-buffer rebuild when:

- the changed region overlaps a marker line, or
- the changed region overlaps a line adjacent to a callout's first
  or last line (could create or destroy block boundaries), or
- the changed region intersects more than one callout.

When the changed region intersects no decorated callout and is
not adjacent to one, the system SHALL NOT rebuild.

#### Scenario: Edit inside one callout body

- **WHEN** the user modifies a body line inside one decorated
  callout and waits for idle
- **THEN** only that callout's overlays are rebuilt; other
  decorated callouts in the buffer retain their existing overlay
  objects unchanged

#### Scenario: Edit on the marker line triggers full rebuild

- **WHEN** the user inserts or deletes characters on the
  `> [!TYPE]` marker line of a callout and waits for idle
- **THEN** the system performs a full-buffer rebuild

#### Scenario: Edit on a line adjacent to a callout triggers full rebuild

- **WHEN** the user edits a line immediately above or below a
  callout's source range and waits for idle
- **THEN** the system performs a full-buffer rebuild

#### Scenario: Edit outside every decorated callout

- **WHEN** the user inserts text in a region that does not
  intersect or border any decorated callout and waits for idle
- **THEN** no overlays are rebuilt

### Requirement: Callout selective per-window reconciliation

The system SHALL act on a per-window diff when reconciling display
overlays after a window-state change: only added or resized
windows trigger fresh rendering, and only removed windows have
their display overlays deleted.  Display overlays belonging to
windows whose width is unchanged SHALL retain their existing
overlay objects.

#### Scenario: Resizing one of two windows leaves the other untouched

- **WHEN** the buffer is shown in two windows and only one
  window's width changes
- **THEN** the resized window's display overlays are replaced with
  fresh ones, while the other window's display overlay objects
  survive `eq` comparison

#### Scenario: Closing a window cleans up only its overlays

- **WHEN** a window showing the buffer is deleted
- **THEN** the deleted window's display overlays are removed at
  the next reconciliation, while overlays for surviving windows
  are not disturbed

#### Scenario: Opening a new window on the buffer renders just that window

- **WHEN** a fresh window starts showing a buffer that already has
  decorated callouts
- **THEN** display overlays for the new window are produced
  (visible callouts first, off-screen on the next idle); existing
  display overlays for already-shown windows are not rebuilt

### Requirement: Callout visible-first prioritised rebuild

The system SHALL prioritise visible-window callouts when
rebuilding display overlays in response to a window-state change,
rendering callouts that intersect any visible window range
immediately and deferring off-screen callouts to the next idle
tick.

#### Scenario: Visible callouts render before off-screen callouts

- **WHEN** a window resize fires a rebuild and the buffer contains
  callouts both inside and outside the visible window range
- **THEN** display overlays for callouts intersecting any visible
  window range are recreated synchronously, and overlays for the
  remaining callouts are recreated on a follow-up idle timer

---

<!-- ── Code fences ─────────────────────────────────────────────────────── -->

### Requirement: Code-fence mode toggle

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

### Requirement: Code-fence bordered-block rendering

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
- a per-line right-edge fill that paints the visual line to the
  window edge in the default face, so any `:extend t` background —
  whether carried by a text property (such as the `diff-added` /
  `diff-removed` faces applied by native fontification of a
  ` ```diff ` block) or by another mode's overlay (such as `hl-line`
  or `region`) — has no past-EOL region left to fill and cannot
  extend past the right border

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

#### Scenario: Diff body line background does not leak past the border

- **WHEN** a ` ```diff ` fenced block is decorated and native
  fontification has applied the `:extend t` `diff-added` face to a `+`
  line, including its trailing newline
- **THEN** the `diff-added` background is confined to the box interior
  and does not paint past the right-edge `│` to the window edge

### Requirement: Code-fence body background fill

The system SHALL paint a fenced or indent code-block body line's
right-edge after-string padding with that line's background whenever the
line carries a face with both `:extend t` and a `:background` — for
example the `diff-added` / `diff-removed` faces that native fontification
applies inside a fenced `diff` block — so the highlighted band fills the
box interior up to the right border `│`. When a body line carries no
such face, the right-edge padding SHALL retain the border face's default
appearance.

This applies to the wrapped (overflow) right-edge after-string as well
as the non-wrapped one. It does not apply to callouts, whose right-edge
padding is already filled with the callout tint, nor to YAML helmets,
whose body the decorator fontifies itself.

#### Scenario: Diff added line fills to the border

- **WHEN** a fenced `diff` block is decorated and native
  fontification has applied the `:extend t` `diff-added` face to a `+`
  body line
- **THEN** that line's right-edge after-string padding is painted with
  `diff-added`'s background, so the highlight band reaches the
  right-edge `│`

#### Scenario: Plain code line keeps the default gap

- **WHEN** a fenced block body line carries no face with `:extend t` and
  a `:background`
- **THEN** that line's right-edge after-string padding renders with the
  border face, unchanged from prior behaviour

#### Scenario: Background absent until fontification runs

- **WHEN** a fenced block is decorated before native fontification has
  applied any `:extend t` background to its body
- **THEN** the right-edge padding renders with the default border
  appearance, and a later rebuild repaints it once the `:extend t`
  background is present

### Requirement: Code-fence box width sizing

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

### Requirement: Code-fence marker line reveal

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

### Requirement: Code-fence per-window rendering

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

### Requirement: Code-fence block-discovery cache

The system SHALL memoise each of the three block-discovery functions
(`--find-blocks`, `--find-yaml-helmet`, `--find-indent-blocks`) by
`buffer-chars-modified-tick`, so repeat calls without an intervening
edit reuse the cached scan.  Indented-block discovery still accepts
its excluded-fenced-ranges parameter as call-site data, not part of
the cache key.  The cache SHALL be a pure function of buffer
contents — narrowing or widening the buffer SHALL NOT invalidate or
alter cached results.

#### Scenario: Repeated find-blocks calls without edits return cached result

- **WHEN** `--find-blocks` is called twice with no buffer modification
  in between
- **THEN** both calls return `eq` block lists

#### Scenario: Edit invalidates cache

- **WHEN** the buffer is modified between two calls to `--find-blocks`
- **THEN** the second call returns a fresh block list reflecting the
  new state

#### Scenario: Narrowing does not invalidate cache

- **WHEN** `--find-blocks` is called once in the widened buffer and
  the buffer is then narrowed without any modification
- **THEN** the next `--find-blocks` call returns the same `eq` block
  list (the widened scan, including blocks outside the current
  restriction)

### Requirement: Code-fence narrowing-resilient discovery and teardown

The system SHALL discover fenced blocks, the YAML helmet, and
indented blocks identically regardless of the buffer's current
restriction.  Each `--find-*-1` scanner SHALL widen the
restriction for the duration of its scan; the buffer-wide
teardown path SHALL clear all overlays tagged by the mode
regardless of restriction.

#### Scenario: Narrowed rebuild does not duplicate overlays

- **WHEN** `gfm-code-fences-mode` is enabled in a widened buffer
  containing fenced code blocks in two separate top-level regions
- **AND** the buffer is then narrowed to a region containing only
  the first fence
- **AND** `gfm-code-fences--rebuild` is invoked
- **AND** the buffer is subsequently widened
- **THEN** the count of overlays tagged `gfm-code-fences` on the
  buffer equals the length of `gfm-code-fences--overlays`

#### Scenario: Cached block lists are narrowing-independent

- **WHEN** each of `gfm-code-fences--find-blocks`,
  `gfm-code-fences--find-yaml-helmet`, and
  `gfm-code-fences--find-indent-blocks` is called once in the
  widened buffer and once after narrowing to a sub-region
- **THEN** the second call returns the same data as the first for
  the same `buffer-chars-modified-tick`

#### Scenario: Full teardown leaves no off-narrowing zombies

- **WHEN** the buffer is narrowed and
  `gfm-code-fences--remove-overlays` is invoked with no
  `BEG`/`END` arguments
- **AND** the buffer is subsequently widened
- **THEN** no overlay tagged `gfm-code-fences` remains on the
  buffer

#### Scenario: Fence opening outside the narrowing is discovered

- **WHEN** a fenced code block's opening line sits before the
  current narrowing's start position and its closing line sits
  inside the narrowing
- **THEN** `gfm-code-fences--find-blocks` returns a single block
  whose opening and closing positions match the source

### Requirement: Code-fence debounced rebuild

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

### Requirement: Code-fence scoped post-edit rebuild

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

### Requirement: Code-fence selective per-window reconciliation

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

### Requirement: Code-fence visible-first prioritised rebuild

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

### Requirement: Code-fence performance instrumentation

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

---

<!-- ── Tables ──────────────────────────────────────────────────────────── -->

### Requirement: Table mode toggle

The system SHALL provide a buffer-local minor mode `gfm-tables-mode` that
applies and removes the table decoration overlays as a unit.

#### Scenario: Enabling the mode in a markdown buffer

- **WHEN** `gfm-tables-mode` is enabled in a buffer containing GFM tables
- **THEN** each table is decorated with row overlays, an outer box, a rule
  line, header emphasis, and zebra striping

#### Scenario: Disabling the mode

- **WHEN** `gfm-tables-mode` is disabled in a buffer where it had decorated
  tables
- **THEN** all overlays created by the mode are removed and the buffer
  visually matches its raw markdown source

### Requirement: Table block discovery

The system SHALL identify GFM tables by locating a delimiter row matching
`^\| *:?-+:? *(\| *:?-+:? *)*\|$`, treating the line immediately above it
as the header row and consecutive `|`-prefixed lines below it as body rows.

#### Scenario: Standard GFM table

- **WHEN** the buffer contains a header row, a delimiter row, and one or
  more body rows
- **THEN** the system identifies the contiguous block as one table with
  the correct header, delimiter, and body partitioning

#### Scenario: Tables inside fenced code blocks are skipped

- **WHEN** a sequence of `|`-prefixed lines lies entirely inside a fenced
  code block discovered by `gfm-code-fences`
- **THEN** the system does not decorate it as a table

#### Scenario: Lone delimiter-shaped line is not a table

- **WHEN** a line matching the delimiter regex appears with no preceding
  pipe-prefixed line
- **THEN** the system does not treat it as a table

### Requirement: Cell parser

The system SHALL split each table row into cells on top-level unescaped `|`
characters, treating `\|` as a literal pipe inside a cell and ignoring `|`
characters that occur inside a backtick-delimited code span.

#### Scenario: Escaped pipe inside cell

- **WHEN** a row contains `| a \| b | c |`
- **THEN** the parser yields two cells: `a | b` and `c`

#### Scenario: Pipe inside single-backtick code span

- **WHEN** a row contains `` | a | `b|c` | d | ``
- **THEN** the parser yields three cells, with the second cell content
  including the literal pipe inside the code span

#### Scenario: Pipe inside double-backtick code span

- **WHEN** a row contains `` | a | ``b|c`` | d | ``
- **THEN** the parser yields three cells; the double-backtick span is
  recognised as a code span and its interior pipe is not a delimiter

### Requirement: Column width normalisation

The system SHALL compute a per-column maximum content width across the
header and all body rows, and SHALL render every cell padded to that width
with at least one space of padding inside the cell on each side.

#### Scenario: Unaligned source renders as a uniform grid

- **WHEN** body rows have varying cell widths in source (typical of
  agent-generated tables)
- **THEN** every rendered cell in a given column has identical visual
  width and at least one space of padding on each side

#### Scenario: Source is already aligned

- **WHEN** body rows already have uniform cell widths in source
- **THEN** the rendered table preserves the source's apparent column widths
  without adding extra padding beyond the per-cell minimum

### Requirement: Auto-composition does not skew column widths

The system SHALL ignore auto-compositions (those produced by
`composition-function-table` rather than an explicit `composition`
text property) when measuring a cell's visible width.  Overlay
display strings do not run auto-composition during redisplay, so
counting them would under-pad the cell and push the closing border
off the box's grid.

#### Scenario: Cells containing ligature-prone text align with the box

- **WHEN** a cell contains a sequence the active font would
  auto-compose into a single glyph (e.g. `fl`, `--`) and is rendered
  inside a decorated table
- **THEN** the row's closing `│` aligns with the box's right edge,
  matching the rendered visible width

### Requirement: Window-fitted column widths

The system SHALL cap natural column widths so the rendered box fits
within the available character width of each window showing the
buffer.  The available width is `window-max-chars-per-line` for the
target window, falling back to `fill-column` and then 80 when no
window shows the buffer.  The budget for cell content equals
available-width minus the per-row overhead of `2` (outer pipes) +
`2 * n-cols` (per-cell padding) + `(n-cols - 1)` (inter-cell gaps).

Capping uses water-filling: columns whose natural width is at or
below the cap stay at their natural width; columns above the cap
are reduced to the cap.  Any integer slack remaining after capping
is distributed `+1` at a time across capped columns until the
budget is exhausted.  The cap floors at `1` so no column collapses
to zero width.

#### Scenario: Wide table fits a narrower window

- **WHEN** a table's natural total width exceeds the window's
  available width
- **THEN** at least one column is reduced so that the rendered box
  does not exceed the window's available width

#### Scenario: Slack distributed to capped columns

- **WHEN** capping leaves integer slack between the capped total
  and the budget
- **THEN** the slack is distributed `+1` to columns that were both
  capped *and* whose natural width exceeds the cap, until exhausted

#### Scenario: Narrow table preserves natural widths

- **WHEN** a table's natural total width fits within the window's
  available width
- **THEN** every column renders at its natural max-content width
  with no extra padding from the fit step

#### Scenario: Per-window widths can differ

- **WHEN** a buffer is shown in two windows of different
  `window-max-chars-per-line`
- **THEN** the same table may render with different per-column
  widths in each window, each fitted to that window's budget

### Requirement: Cell wrapping for capped columns

The system SHALL wrap any cell whose content exceeds its column's
fitted width across multiple visual lines.  Wrapping splits on
whitespace runs; tokens longer than the column width are
hard-broken at the visible-width boundary.  Text properties on the
cell content SHALL be preserved on each wrapped piece.

A row's visual height SHALL equal the tallest wrapped cell in that
row.  Cells shorter than the row's visual height SHALL be padded
with empty visual lines so column boundaries stay aligned across
every visual line of the row.

#### Scenario: Long cell wraps on whitespace

- **WHEN** a cell contains multiple words whose combined width
  exceeds its column's fitted width
- **THEN** the cell renders across multiple visual lines, broken at
  whitespace, with each visual line within the column's width

#### Scenario: Long token hard-breaks

- **WHEN** a cell contains a single token whose visible width
  exceeds the column's fitted width
- **THEN** the token is sliced into chunks each of visible-width
  ≤ column width and rendered across the resulting visual lines

#### Scenario: Row height equals tallest cell

- **WHEN** one cell in a row wraps to N visual lines and other
  cells in the same row wrap to fewer
- **THEN** every cell in that row contributes N visual lines to
  the rendered output, with shorter cells padded by empty lines

### Requirement: Cell-edit commit preserves the row

The system SHALL preserve the surrounding table row when committing
an indirect cell-edit.  Specifically, when the buffer's edit-indirect
machinery would otherwise leave a stray newline at the end of the
committed cell content (because `markdown--edit-indirect-after-commit-function`
treats every committed region as a code block), the system SHALL
strip that trailing newline before redisplay.

#### Scenario: Committing an unchanged cell edit leaves the row intact

- **WHEN** the user opens an indirect cell edit, makes no changes,
  and commits
- **THEN** the table row's source contents are byte-for-byte
  identical to before the edit; in particular, no newline is
  inserted between the committed cell and the next cell on the same
  row

### Requirement: Table border and rule decoration

The system SHALL bracket each table with a synthesised top border
(`┌─…─┐`), a continuous rule line (`├─…─┤`) replacing the delimiter row,
and a synthesised bottom border (`└─…─┘`).

#### Scenario: Outer box brackets the table

- **WHEN** a table is decorated
- **THEN** the rendered output begins with a top border line above the
  header row and ends with a bottom border line below the last body row

#### Scenario: Delimiter row replaced with continuous rule

- **WHEN** a table contains a delimiter row in source
- **THEN** the rendered output replaces that line with a single
  `├─…─┤` line of the same width as the box

### Requirement: Exterior pipe rendering

The system SHALL render the leading and trailing `|` of every row as `│`
(box-drawing vertical), aligned with the corners of the outer box.

#### Scenario: Leading and trailing pipes rendered as box edges

- **WHEN** a body or header row is rendered
- **THEN** the first and last visible characters of the row are `│`
  characters that line up with the `┌`/`└` and `┐`/`┘` corners

### Requirement: Interior column gap rendering

The system SHALL render the position of every interior `|` as a single
space using the default theme background, so that on rows with the alt-bg
stripe, the gap punches through the stripe and reveals column boundaries.

#### Scenario: Gap punches through stripe on alt-bg rows

- **WHEN** a body row has the alt-bg stripe applied
- **THEN** the column-gap positions render with the default theme
  background, producing visible vertical channels through the stripe

#### Scenario: Gap is contiguous with cell padding on default-bg rows

- **WHEN** a body row has no stripe (default-bg row)
- **THEN** the column-gap positions render at the default background and
  are visually continuous with the surrounding cell padding

### Requirement: Header emphasis

The system SHALL render the row immediately preceding the delimiter row
with bold weight, on the default theme background.

#### Scenario: Header row renders bold without distinct background

- **WHEN** a table has a header row identified by the delimiter row's
  position
- **THEN** the header row's text renders bold and the row's background
  matches the default theme background (no stripe)

### Requirement: Body row zebra striping

The system SHALL apply the `gfm-tables-row-alt-face` background to
even-numbered body rows (counting from 1 below the rule line), leaving
odd-numbered body rows on the default background.

#### Scenario: Alternating row backgrounds

- **WHEN** a table has at least two body rows
- **THEN** the first body row renders at the default background, the
  second body row renders with `gfm-tables-row-alt-face`, the third at
  default, and so on

### Requirement: Stripe face

The system SHALL define a face `gfm-tables-row-alt-face` with distinct
backgrounds for light and dark themes, and SHALL allow customisation
through `customize-face`.

#### Scenario: Light theme stripe colour

- **WHEN** the active theme has `light` background type
- **THEN** `gfm-tables-row-alt-face` uses background `#efe9dd` by default

#### Scenario: Dark theme stripe colour

- **WHEN** the active theme has `dark` background type
- **THEN** `gfm-tables-row-alt-face` uses background `#313244` by default

### Requirement: Active-cell highlight

When point sits inside a decorated table cell, the system SHALL
repaint the row's display overlay so the active cell's segment
carries `gfm-tables-active-cell-face`.  The highlight SHALL track
point: moving to a different cell repaints both the previously
highlighted cell (restoring its original face) and the new cell.
Leaving the table SHALL restore the original display string.

For rows that wrap to multiple visual lines, the highlight SHALL
paint the active cell's segment on every visual line of the row,
not just the first.

The system SHALL stash the row's original `display` under
`gfm-tables-saved-display` before painting, and SHALL restore from
that stash on cell exit, so the restore is byte-identical to the
pre-highlight display string.

#### Scenario: Entering a cell paints it

- **WHEN** point moves into a cell of a decorated table
- **THEN** that cell's segment of the row's display string carries
  `gfm-tables-active-cell-face`

#### Scenario: Crossing cells repaints both

- **WHEN** point moves from cell A to cell B in the same row
- **THEN** cell A's original face is restored and cell B is
  painted with `gfm-tables-active-cell-face`, in a single repaint

#### Scenario: Leaving the table restores the row

- **WHEN** point moves out of every decorated table
- **THEN** the previously highlighted row's `display` is
  byte-identical to the value composed at the most recent rebuild

#### Scenario: Highlight survives motion onto the same cell

- **WHEN** a command moves point but leaves it on the same cell as
  before
- **THEN** no display repaint runs (the highlight key is unchanged)

#### Scenario: Highlight spans every visual line of a wrapped row

- **WHEN** the active cell occupies a row whose source spans
  multiple visual lines
- **THEN** the active-cell highlight paints each visual line of
  that cell, not just the first

### Requirement: Cursor anchoring inside cells

While point is inside a decorated table cell, the system SHALL
suppress the visible buffer cursor by setting `cursor-type` to nil
buffer-locally, and SHALL place a `cursor` text property on the
first character of the active cell's source range so the terminal
cursor anchors there rather than at the right edge of the overlay's
display range.

The system SHALL stash the previous `cursor-type` value (and
whether it was buffer-local) before suppressing, and SHALL restore
it when point leaves every decorated table.  The active-cell face
alone conveys cell selection while the cursor is hidden.

#### Scenario: Cursor hides on entry, restores on exit

- **WHEN** point moves into a decorated table cell, then later
  moves out of every decorated table
- **THEN** `cursor-type` is nil while inside, and on exit is
  restored to the exact value (including local / global scope)
  it had before entry

#### Scenario: Cursor anchor moves with the active cell

- **WHEN** point crosses from cell A to cell B
- **THEN** the `cursor` text property is removed from cell A's
  anchor position and placed on the first character of cell B's
  source range

### Requirement: Cell-entry key hints

When point enters a decorated table cell, the system SHALL echo a
single-line, transient-style summary of the cell-aware key
bindings into the echo area.  The hint SHALL NOT be logged to
`*Messages*` (uses `message-log-max` bound to nil).

Hint groups SHALL support a `:when` predicate so a group renders
only in contexts where its bindings apply (e.g. the column-swap
group renders only on the header row).

The hint SHALL be re-echoed only when point crosses to a different
cell.  Motion that lands point on the same cell as before SHALL
NOT re-echo, so the hint does not clobber a message left by another
command (e.g. `Copied:` from cell-copy).

#### Scenario: Header row shows column-swap hints

- **WHEN** point enters a header-row cell
- **THEN** the echoed hint includes the column-swap group

#### Scenario: Body row hides column-swap hints

- **WHEN** point enters a body-row cell
- **THEN** the echoed hint omits the column-swap group

#### Scenario: Same-cell motion does not re-echo

- **WHEN** a command runs that moves point but leaves it on the
  same cell as before
- **THEN** the echo area is not overwritten with the hint line

### Requirement: Structural cell motion

Inside a decorated table, the system SHALL provide cell-wise motion
operating on the row anchor's recorded `gfm-tables-cell-bounds`
rather than on character positions.  Motion commands SHALL place
point at the start of the destination cell's content range, never
inside the column-gap or outer-pipe positions.

The system SHALL provide:

- forward / backward one cell within a row
- up / down to the same column index in the adjacent row, confined
  to the current table block (motion does not jump into a
  neighbouring table)
- first / last cell of the current row
- TAB to advance to the next cell, wrapping at row end to the next
  row's first cell, and inserting a fresh empty row when invoked
  on the last cell of the last row
- Shift-TAB to retreat to the previous cell, wrapping at row start
  to the previous row's last cell

#### Scenario: Cell-forward stops at row end

- **WHEN** point is on the last cell of a row and the user invokes
  cell-forward
- **THEN** point does not move and the command returns nil

#### Scenario: Row-down respects table boundary

- **WHEN** point is in the last body row of one table and the user
  invokes row-down
- **THEN** point does not jump into a subsequent table; the command
  returns nil

#### Scenario: TAB at last cell of last row inserts a row

- **WHEN** point is on the last cell of the last body row and the
  user presses TAB
- **THEN** a new empty row is inserted below with the same column
  count, and point lands in its first cell

### Requirement: Snap-to-cell on row entry

The system SHALL move point to the start of cell 0 when point
lands on a decorated table row but outside any cell's recorded
bounds (e.g. on the leading `│`, a column gap, or trailing
border).  The snap SHALL NOT fire when the row is currently
invisible (folded outline, narrowed-out region) so motion through
hidden text does not surface point inside hidden tables.

#### Scenario: Beginning-of-line lands inside cell 0

- **WHEN** the user invokes a beginning-of-line command on a
  decorated row
- **THEN** point lands at the first character of cell 0's content,
  not on the leading `│`

### Requirement: In-place edit commands divert to indirect editor

The system SHALL divert the standard insert / append / change /
replace / substitute / open-line family of evil commands to
`gfm-tables-edit-table-at-point` when `evil-mode` is loaded and
point is inside a decorated table.  The user does not type into the
source row directly; instead the whole table opens in an indirect
edit buffer.

#### Scenario: Pressing `i` opens the indirect table editor

- **WHEN** point is in a decorated table cell and the user presses
  the key bound to `evil-insert`
- **THEN** an indirect edit buffer opens covering the table block,
  with `markdown-mode` and `orgtbl-mode` active, and point in the
  indirect buffer matches the cell point was on in the source

#### Scenario: Edit commands outside a table behave normally

- **WHEN** point is not inside any decorated table and the user
  invokes one of the diverted evil commands
- **THEN** the command runs unchanged

### Requirement: Cell-only indirect edit

The system SHALL provide `gfm-tables-edit-cell-at-point`, which
opens just the trimmed content of the active cell in an indirect
edit buffer.  The indirect buffer SHALL run `markdown-mode` and
SHALL display a header line indicating commit / abort key bindings.

On commit, the system SHALL sanitise the committed content by
replacing every embedded newline with a single space and escaping
every unescaped `|` as `\|`, so committing cannot break the
surrounding row's structure.

#### Scenario: Newlines collapse to spaces on commit

- **WHEN** the user inserts a newline inside a cell-edit buffer
  and commits
- **THEN** the source cell receives a single space at that position

#### Scenario: Pipes are escaped on commit

- **WHEN** the user types a literal `|` in a cell-edit buffer and
  commits
- **THEN** the source cell contains `\|`, not a structural pipe

### Requirement: Whole-table indirect edit

The system SHALL provide `gfm-tables-edit-table-at-point`, which
opens the entire table block in an indirect edit buffer running
`markdown-mode` with `orgtbl-mode` active.  Point in the indirect
buffer SHALL start on the same cell point was on in the source.
The committed region SHALL include the trailing newline of the last
body row so `markdown-mode`'s edit-indirect after-commit hook does
not append a spurious newline.

After commit, point in the source buffer SHALL be moved to the cell
point was on in the indirect buffer (matched by line offset and
cell index).  When that point is no longer visible in the source
window, the window SHALL recenter so the cell is on screen.

#### Scenario: Point preserved across commit

- **WHEN** the user moves point to a different cell inside the
  indirect edit buffer and commits
- **THEN** point in the source buffer lands on that same cell

#### Scenario: Off-screen target recentres

- **WHEN** the indirect commit's restored point lies outside the
  source window's visible range
- **THEN** the source window is recentred so point is visible

### Requirement: Header column swap

The system SHALL provide commands to swap the column at point with
its left or right neighbour, applied symmetrically to the header
row, the delimiter row, and every body row.  The commands SHALL be
silent no-ops when point is not on the header row, or when the
neighbour column index is out of range.  After swap, point SHALL
land on the moved column's new position.

#### Scenario: Swap right re-orders all rows

- **WHEN** point is in column N of the header (with N+1 in range)
  and the user invokes swap-right
- **THEN** every row of the table has its cell N exchanged with
  cell N+1, and point lands on the new column N+1

#### Scenario: Swap on body row is a no-op

- **WHEN** point is in a body row and the user invokes swap-left
  or swap-right
- **THEN** the table is unchanged

### Requirement: Table debounced rebuild

The system SHALL respond to buffer modifications and window-state
changes via a 0.2-second idle timer, SHALL skip rebuilds in indirect
buffers, SHALL scope post-edit rebuilds to the table whose source
range intersects the changed region, and SHALL react to
window-state changes only when an actual rendering input has changed
(width of an existing window, or arrival/departure of a window
showing the buffer).

When the changed region intersects no decorated table, the system
SHALL NOT rebuild. When the changed region intersects exactly one
table, the system SHALL tear down and re-decorate only that table.
When the changed region intersects multiple tables, crosses a table
boundary, or overlaps a fenced-code-block fence line, the system
SHALL fall back to a full-buffer rebuild.

When `window-configuration-change-hook` fires but the per-window
(window . max-chars-per-line) snapshot is unchanged from the
previous rebuild, the system SHALL NOT schedule a rebuild.

#### Scenario: Edit inside a single decorated table

- **WHEN** the user modifies a cell inside one decorated table and
  waits for idle
- **THEN** only that table's overlays are rebuilt; other decorated
  tables in the buffer retain their existing overlay objects
  unchanged

#### Scenario: Edit outside every decorated table

- **WHEN** the user inserts text in a region that does not intersect
  any decorated table and waits for idle
- **THEN** no overlays are rebuilt

#### Scenario: Edit spans two tables

- **WHEN** the user deletes a region that spans more than one
  decorated table and waits for idle
- **THEN** all overlays in the buffer are rebuilt

#### Scenario: Edit overlaps a code fence boundary

- **WHEN** the changed region overlaps the opening or closing fence
  line of a fenced code block and the buffer becomes idle
- **THEN** all overlays in the buffer are rebuilt

#### Scenario: Window-state change with no width effect

- **WHEN** a window-configuration event fires that does not change any
  window's `window-max-chars-per-line` and does not add or remove a
  window showing the buffer (e.g. minibuffer activity, focus shift)
- **THEN** no rebuild is scheduled

#### Scenario: Indirect buffer skipped

- **WHEN** the current buffer has a base buffer
- **THEN** the rebuild scheduler does not schedule a rebuild for the
  indirect buffer

### Requirement: Per-window table rendering

The system SHALL produce display overlays sized to each window
currently showing the buffer, so that the same buffer split across
multiple windows (or shared between a graphical and terminal frame)
renders at each window's own width.  Source-side bookkeeping (cell
boundaries, the row keymap) SHALL be carried by anchor overlays
shared across windows; rendering (display strings, borders, packed
display-cell-bounds) SHALL be carried by display overlays restricted
to a single window via Emacs's `window` overlay property.

When no window currently displays the buffer (e.g. background
buffer), the system SHALL produce a single unrestricted display
overlay per row at a sane fallback width and replace it with
window-restricted overlays as soon as a window starts showing the
buffer.

#### Scenario: Buffer split across windows of different widths

- **WHEN** the buffer is shown in two windows whose
  `window-max-chars-per-line` differ
- **THEN** each window renders the table sized to its own width,
  with one display overlay per row per window

#### Scenario: Anchor overlays are shared across windows

- **WHEN** the buffer is shown in N windows
- **THEN** each source row is covered by exactly one anchor overlay
  carrying `gfm-tables-cell-bounds` and the row keymap, regardless of
  N

#### Scenario: Active-cell highlight targets the selected window

- **WHEN** the user holds point in a cell while the buffer is shown
  in two windows
- **THEN** the active-cell highlight repaints only the selected
  window's display overlay; the other window's display overlay is
  left untouched

### Requirement: Table prioritised window rebuild

The system SHALL prioritise visible-window tables when rebuilding
display overlays in response to a window-state change, rendering
visible blocks immediately and deferring off-screen blocks to the
next idle tick.

#### Scenario: Visible blocks render before off-screen blocks

- **WHEN** a window resize fires a rebuild
- **THEN** display overlays for blocks intersecting any visible
  window range are recreated synchronously, and overlays for the
  remaining blocks are recreated on a follow-up idle timer

### Requirement: Table selective per-window reconciliation

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
  decorated tables
- **THEN** display overlays for the new window are produced (visible
  blocks first, off-screen on the next idle); existing display
  overlays for already-shown windows are not rebuilt

### Requirement: Table performance instrumentation

The system SHALL maintain per-buffer performance statistics covering
rebuild count, total duration, last duration, max duration, table
count from the most recent rebuild, and a per-phase breakdown
(block discovery, row parsing, layout, display composition, overlay
application). The system SHALL surface these statistics via a command
`gfm-tables-stats`.

#### Scenario: Stats accumulate across rebuilds

- **WHEN** the rebuild has run multiple times in a buffer
- **THEN** `gfm-tables-stats` reports the cumulative count, total
  duration, last and max single-rebuild durations, and the table
  count from the most recent rebuild

#### Scenario: Phase breakdown surfaced

- **WHEN** `gfm-tables-stats` is invoked after one or more rebuilds
- **THEN** the report includes a per-phase total covering at least
  block discovery, row parsing, layout, display composition, and
  overlay application

#### Scenario: Slow rebuild emits a warning

- **WHEN** a single rebuild duration exceeds
  `gfm-tables-slow-rebuild-threshold` (default 0.05 s)
- **THEN** the system emits a `message` line identifying the buffer
  and the duration

### Requirement: Theme change responsiveness

The system SHALL re-evaluate any cached default-bg colour used for
column-gap rendering when the active theme changes, so that gaps remain
visually correct across theme switches.

#### Scenario: Theme switch refreshes gap colour

- **WHEN** the active theme changes from light to dark (or vice versa)
- **THEN** subsequent rebuilds use the new theme's default background
  for column-gap rendering

### Requirement: Table narrowing-resilient discovery and teardown

The system SHALL discover tables and tear down its overlays
identically regardless of the buffer's current restriction.  Block
discovery SHALL scan the full buffer (widening the restriction for
the duration of the scan); the buffer-wide teardown path used by
the mode's full rebuild and by `gfm-tables-mode` disable SHALL
clear all overlays tagged by the mode regardless of restriction.

#### Scenario: Narrowed rebuild does not signal

- **WHEN** `gfm-tables-mode` is enabled in a widened buffer
  containing tables in two separate top-level regions
- **AND** the buffer is then narrowed to a region containing only
  the first table
- **AND** `gfm-tables--rebuild` is invoked (e.g. via a
  window-configuration change)
- **THEN** the rebuild completes without signalling an error

#### Scenario: Cached block list is narrowing-independent

- **WHEN** `gfm-tables--find-blocks` is called once in the widened
  buffer (caching a block list spanning the whole document) and
  once after narrowing to a sub-region
- **THEN** both calls return block lists with the same source
  positions for the same `buffer-chars-modified-tick`

#### Scenario: Full teardown leaves no off-narrowing zombies

- **WHEN** the buffer is narrowed and
  `gfm-tables--remove-overlays` is invoked with no `BEG`/`END`
  arguments (the full-clear branch)
- **AND** the buffer is subsequently widened
- **THEN** no overlay tagged `gfm-tables` remains on the buffer

#### Scenario: Tracking list and on-buffer overlays stay in lockstep

- **WHEN** `gfm-tables-mode` performs a rebuild while the buffer
  is narrowed
- **AND** the buffer is subsequently widened
- **THEN** the count of overlays tagged `gfm-tables` on the buffer
  equals the length of `gfm-tables--overlays`

---

<!-- ── Links ───────────────────────────────────────────────────────────── -->

### Requirement: Link mode toggle

The system SHALL provide a buffer-local minor mode `gfm-links-mode`
that applies and removes the link-decoration overlays as a unit. The
mode SHALL be enabled automatically in `markdown-mode-hook` whenever
`markdown-hide-urls` is non-nil at mode-init time, and SHALL track
subsequent changes to `markdown-hide-urls` via a buffer-local
variable watcher.

#### Scenario: Enabling the mode in a markdown buffer with hide-urls

- **WHEN** `markdown-mode` is initialised in a buffer with
  `markdown-hide-urls` set to t
- **THEN** `gfm-links-mode` is enabled in that buffer
- **AND** every recognised link form is decorated with the
  title-side and url-side overlays

#### Scenario: Toggling hide-urls off while the mode is on

- **WHEN** `markdown-hide-urls` is set to nil in a buffer where
  `gfm-links-mode` is on
- **THEN** `gfm-links-mode` is disabled
- **AND** every overlay created by the mode is removed
- **AND** the buffer visually matches its raw markdown source

#### Scenario: Disabling the mode directly

- **WHEN** `gfm-links-mode` is disabled in a buffer where it had
  decorated links
- **THEN** every overlay created by the mode is removed and the
  buffer visually matches its raw markdown source

### Requirement: Link shape discovery

The system SHALL discover and decorate the following link shapes:

- inline links `[title](url)` and `[title](url "title-attr")`
- reference links in full form `[title][label]`, collapsed form
  `[title][]`, and shortcut form `[title]` (the last only when a
  matching `[label]: url` definition exists in the buffer)
- autolinks `<url>` where `url` parses as an absolute URL
- GFM bare URLs (`https?://…` outside any other markup)
- wiki links `[[Page]]` and `[[alias|Page]]`, only when
  `markdown-enable-wiki-links` is non-nil

The system SHALL NOT decorate:

- image links `![alt](url)` or `![alt][label]`
- reference definition lines `[label]: url`
- footnote markers `[^id]` or footnote text lines

#### Scenario: Inline link with title attribute

- **WHEN** the buffer contains `[Anthropic](https://anthropic.com "official")`
- **THEN** the system decorates the link as one inline-link
  unit and the title attribute is available for eldoc surfacing

#### Scenario: Shortcut reference matches only with definition

- **WHEN** the buffer contains the prose `[design]` with no
  matching `[design]: …` definition elsewhere
- **THEN** the system does NOT decorate `[design]`
- **AND** the raw bracketed text remains visible

#### Scenario: Image link is left raw

- **WHEN** the buffer contains `![alt](./diagram.png)`
- **THEN** the system does not create any link-decoration overlay
  for that text
- **AND** the raw markdown remains visible

### Requirement: Title-side overlay rendering

The system SHALL replace the visible bracket scaffolding around the
title with an overlay whose `display` property is the title text in
`markdown-link-face`. For inline and reference links the overlay
covers the region `[title]` (brackets inclusive). For autolinks
`<url>` and bare URLs the overlay covers the URL span itself and
the visible label is the host extracted from the URL (the
`url-host` portion). For wiki links the overlay covers `[[…]]` and
the visible label is the page name (alias if present).

#### Scenario: Inline link title rendering

- **WHEN** an inline link `[Anthropic](https://anthropic.com)` is
  decorated
- **THEN** the visible text for the title region is the four
  characters `Anthropic` in `markdown-link-face`
- **AND** the surrounding square brackets are no longer visible

#### Scenario: Autolink host extraction

- **WHEN** an autolink `<https://anthropic.com/path>` is decorated
- **THEN** the visible label is `anthropic.com` in
  `markdown-link-face`

### Requirement: URL-side icon rendering

The system SHALL replace the URL-bearing region of each decorated
link with an overlay whose `display` property is a single icon
glyph. The URL-bearing region is `(url)` for inline links, the
trailing `[label]` (or its position) for reference links, the
autolink/bare-URL span itself, and the wiki-link target half for
wiki links.

The icon SHALL be resolved as follows, deferring entirely to
`nerd-icons`:

- if the URL starts with `http://` or `https://` (or is an
  unrecognised non-relative scheme), call `nerd-icons-icon-for-url`
  on the URL
- if the URL is a relative path or begins with `file:`, call
  `nerd-icons-icon-for-file` on the basename
- if the URL is a same-document anchor (`#…`), call
  `nerd-icons-icon-for-url` on the anchor (which yields the
  globe fallback)

The system SHALL NOT maintain its own URL→glyph table; users
extend `nerd-icons-url-alist` for customisation.

The system SHALL NOT pass a `:height` override to nerd-icons.

#### Scenario: GitHub URL resolves to the github octicon

- **WHEN** an inline link `[repo](https://github.com/foo/bar)` is
  decorated
- **THEN** the url-side overlay's display string is the glyph
  returned by `nerd-icons-icon-for-url`
- **AND** the user sees a github mark icon in the buffer

#### Scenario: Relative file path resolves via icon-for-file

- **WHEN** an inline link `[config](./modules/init.el)` is
  decorated
- **THEN** the url-side overlay's display string is the glyph
  returned by `nerd-icons-icon-for-file` applied to `init.el`

### Requirement: Reference link resolution

The system SHALL maintain a buffer-local reference-definition
alist mapping label → (url, title-attr). The alist SHALL be
populated by scanning the buffer for `^ {0,3}\[label\]: url`
definitions and SHALL be invalidated and recomputed at the start
of each rebuild. When duplicate definitions exist for a label,
the first definition in buffer order SHALL win, matching
`markdown-reference-definition`.

For each reference-style link the system SHALL look up the URL via
this alist and resolve the icon from that URL using the same
branches as URL-side icon rendering.

The system SHALL NOT decorate a reference link whose label has no
matching definition; the raw `[title][label]` text remains visible
and any `markdown-missing-link-face` applied by markdown-mode is
preserved.

#### Scenario: Collapsed reference resolves through alist

- **WHEN** the buffer contains `[design][]` and elsewhere
  `[design]: ./docs/adr-001.md`
- **THEN** the url-side icon is resolved through
  `nerd-icons-icon-for-file` on `adr-001.md`

#### Scenario: Broken reference is not decorated

- **WHEN** the buffer contains `[design][missing]` with no
  matching `[missing]: …` definition
- **THEN** no overlay is created for this link
- **AND** the raw bracketed text remains visible

### Requirement: Suppression of built-in URL composition

The system SHALL suppress markdown-mode's built-in
`compose-region` URL collapsing while `gfm-links-mode` is enabled
in a buffer, so that the built-in's URL-glyph composition does not
coexist with the new overlays.

The suppression SHALL be implemented via `:around` advice on
`markdown-fontify-inline-links` and
`markdown-fontify-reference-links`. The advice SHALL be installed
at module load and SHALL be inert (passing through to the original
function) in any buffer where `gfm-links-mode` is not active. When
`gfm-links-mode` is active, the advice SHALL invoke the original
function with `markdown-hide-urls` let-bound to nil, so all face
application proceeds normally but the `compose-region` branch is
skipped.

#### Scenario: Compose region branch skipped when mode is on

- **WHEN** `gfm-links-mode` is on and `markdown-fontify-inline-links`
  runs on a region containing a decorated link
- **THEN** no `composition` text property is applied to the URL
  region by the built-in fontifier

#### Scenario: Compose region branch runs in other buffers

- **WHEN** a buffer has `markdown-hide-urls` set to t but
  `gfm-links-mode` is not enabled
- **THEN** the built-in compose-region collapse proceeds unchanged

### Requirement: Whole-link cursor reveal

The system SHALL reveal the entire link (both the title-side and
url-side overlays together) to raw source whenever point lands
anywhere inside either overlay in the selected window, and SHALL
restore the decoration when point leaves both overlays. Title-side
and url-side overlays of the same link SHALL share a common
identifier so the reveal hook can find the partner overlay.

#### Scenario: Point on the title reveals the URL too

- **WHEN** point moves onto the visible label of a decorated
  inline link
- **THEN** the title-side overlay's display property is suppressed
- **AND** the url-side overlay's display property is also
  suppressed
- **AND** the raw `[title](url)` text is visible until point
  leaves the link region

#### Scenario: Reveal is per-window

- **WHEN** the same buffer is shown in two windows and point is
  inside a link in one window only
- **THEN** the link reveals only in that window
- **AND** the link remains decorated in the other window

### Requirement: RET follows the link when point is on the decoration

The system SHALL bind `RET` to `gfm-links-follow-link-at-point`
through the title-side overlay's `keymap` property, so the binding
applies only when point is inside a decorated link region. Off any
decorated link, `RET` SHALL fall through to the existing global
binding (`markdown-enter-key`). `gfm-links-follow-link-at-point`
SHALL invoke `markdown--browse-url` on the resolved URL.

#### Scenario: RET on a link follows the URL

- **WHEN** point is on a decorated inline link
  `[Anthropic](https://anthropic.com)` and the user presses RET
- **THEN** `markdown--browse-url` is invoked with
  `https://anthropic.com`

#### Scenario: RET off a link falls through to markdown-enter-key

- **WHEN** point is on a list item with no decorated link at point
  and the user presses RET
- **THEN** `markdown-enter-key` runs (smart list-item continuation)

### Requirement: Reference goto-definition via xref

The system SHALL register an `xref-backend-functions` entry for
markdown buffers with `gfm-links-mode` enabled. The backend SHALL
recognise reference-style links at point (full, collapsed, or
shortcut forms) and SHALL return an `xref-item` pointing at the
`[label]: …` definition line. The backend SHALL return nil for any
other position, allowing other xref backends to handle the call.

#### Scenario: M-. jumps to definition line

- **WHEN** point is on the title region of a reference link
  `[design][adr-001]` and the user invokes `xref-find-definitions`
- **THEN** point moves to the line beginning with `[adr-001]:`

#### Scenario: Backend defers off reference link

- **WHEN** point is on an inline link (not a reference link) and
  the user invokes `xref-find-definitions`
- **THEN** the gfm-links xref backend returns nil
- **AND** other xref backends are consulted

### Requirement: Eldoc URL exposure

The system SHALL register a buffer-local
`eldoc-documentation-functions` entry that returns a one-line
string of the resolved URL (and the inline title attribute when
present) whenever point is on a decorated link, and returns nil
otherwise. The system SHALL NOT modify
`eldoc-documentation-strategy`; composition with other providers
follows the user's existing strategy.

#### Scenario: Eldoc surfaces the URL when point is on a link

- **WHEN** point is on a decorated inline link
  `[Anthropic](https://anthropic.com "official")`
- **THEN** the eldoc function returns a string including
  `https://anthropic.com` and `official`

#### Scenario: Eldoc returns nil when point is off any link

- **WHEN** point is on plain prose with no decorated link
- **THEN** the eldoc function returns nil
- **AND** other eldoc providers are not blocked

### Requirement: Overlay decoration does not skew column widths

The system SHALL extend `gfm-tables--visible-width--compute` so
its caller can pass an optional source buffer and `(beg, end)`
range over which overlays in that buffer are also consulted for
width-affecting properties. When source-buffer arguments are
provided, the walker SHALL honour `display`, `invisible`, and
`composition` properties on overlays intersecting the range using
the same rules it currently applies to text properties. When the
new arguments are nil, the walker SHALL behave exactly as before.

`gfm-tables--fontify-cell` (and any code path that measures a
cell's visible width) SHALL measure cell width against the source
buffer overlays, so a table cell containing a decorated link
renders with a column width that matches the decoration's visible
width — not the underlying raw markdown width.

#### Scenario: Cell containing a decorated link aligns with the box

- **WHEN** a table cell contains a decorated inline link
  `[Anthropic](https://anthropic.com)` and the cell is rendered
  inside a decorated table
- **THEN** the column width is computed against the visible
  width of `Anthropic ` plus the URL icon — not against the
  raw markdown length
- **AND** the row's closing `│` aligns with the box's right edge

#### Scenario: Existing callers without overlay args still work

- **WHEN** code outside the new module calls
  `gfm-tables--visible-width` with the old single-argument form
- **THEN** the function returns the same result as before this
  change

### Requirement: Per-window link rendering

The system SHALL render link decoration overlays per-window using
the same window-property mechanism the other GFM decorators use,
so multiple windows showing the same buffer at different points
each see independent decoration and reveal state. Title-side and
url-side overlays SHALL each carry a `window` property scoped to
one window.

#### Scenario: Two windows show different reveal state

- **WHEN** the same markdown buffer is shown in two windows, with
  point on a link in window A and elsewhere in window B
- **THEN** window A shows the raw link source
- **AND** window B shows the decorated link

### Requirement: Link debounced rebuild

The system SHALL rebuild link overlays on a debounced timer
following buffer changes, mirroring the debounced-rebuild pattern
in `gfm-callouts-mode`. The rebuild SHALL recompute the
reference-definition alist before re-creating overlays. The
rebuild SHALL be narrowing-resilient: rebuilding within a
narrowed region SHALL produce the same final overlay set as
rebuilding the widened buffer, with no leaked overlays outside
the narrowed range.

#### Scenario: Rebuild after editing a definition line

- **WHEN** a `[label]: url` definition is edited in the buffer
- **THEN** the debounced rebuild fires
- **AND** every reference link to that label re-resolves to the
  new URL

#### Scenario: Rebuild while narrowed leaves no zombies

- **WHEN** the buffer is narrowed to a region containing some but
  not all decorated links, the rebuild runs, and the buffer is
  widened and rebuilt again
- **THEN** the overlay set after the widen-rebuild matches the
  overlay set produced by a clean widened rebuild
- **AND** no overlays from the narrowed-rebuild remain outside
  the narrowed range
