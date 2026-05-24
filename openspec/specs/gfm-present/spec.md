# gfm-present Specification

## Purpose

Lives at `lisp/gfm/gfm-present.el` (library axis). Cross-references
the `gfm-pretty` axis for the visual decoration the slides rely on.

The `gfm-present` capability lets a Claude Code agent and the user
walk through a single markdown document slide-by-slide.  The agent
authors a markdown file and invokes `(gfm-present-markdown PATH)` via
`emacsclient -e`; Emacs enables a buffer-local minor mode
(`gfm-present-mode`) that narrows the buffer to the H1 region
containing point and binds keys for stepping through headings.

There is no deck data structure: the document is the deck and
narrowing is the navigator.  There is no session registry: per-
presentation state lives buffer-local on the document buffer.  There
are no MCP tools and no channel back-signal.  Tmux geometry is the
caller's problem.

Inside the narrowed region, two link forms render as fenced previews
without modifying buffer text: `<path>#L<start>-L<end>` shows a
source-range preview, and `diff:<base>...<head>[#<path>]` shows a
git-diff preview.  Following a preview link pushes a mark and escapes
to a real source buffer (or a magit diff buffer) so the user retains
their standard back-jump bindings.
## Requirements
### Requirement: Public entry point `gfm-present-markdown`

The system SHALL expose a single public function
`gfm-present-markdown FILE`, autoloaded, that opens FILE via `find-file`
and enables `gfm-present-mode` in the resulting buffer.  The
function SHALL accept FILE as a string path (absolute or relative to
`default-directory` at call time) and SHALL signal a `user-error`
when FILE does not exist or cannot be opened as a regular file.

The function SHALL be the only public elisp entry point for starting
a presentation.  The presentation module SHALL NOT register any MCP
tools.

#### Scenario: emacsclient invocation enables the mode

- **WHEN** a caller evaluates `(gfm-present-markdown "/abs/path/doc.md")`
- **AND** the file exists and is readable
- **THEN** the buffer visiting `/abs/path/doc.md` becomes the current
  buffer
- **AND** `gfm-present-mode` is enabled in that buffer
- **AND** the buffer is narrowed to the first H1 region (or remains
  widened if the document has no H1 headings)

#### Scenario: missing file rejected

- **WHEN** `gfm-present-markdown` is called with a path that does not
  exist
- **THEN** a `user-error` is signalled whose message names the path
- **AND** no buffer is created
- **AND** `gfm-present-mode` is not enabled

#### Scenario: no MCP tools registered

- **WHEN** the `presentation` module is initialised
- **THEN** no entry whose `:name` is `start_presentation`,
  `present_document`, `get_presentation`, `end_presentation`,
  `push_slide`, `replace_slide`, `truncate_after`, `goto_slide`, or
  `get_deck` is added to `claude-code-ide-mcp-server-tools`

### Requirement: `gfm-present-mode` buffer-local minor mode

The system SHALL define `gfm-present-mode` as a buffer-local minor
mode.  Enabling the mode SHALL narrow the buffer to the H1 region
containing point (or the first H1 region when point is before the
first H1, or the last H1 region when point is after the last H1).
Disabling the mode SHALL widen the buffer.

The mode keymap SHALL bind:

| Key            | Command                          |
| :------------- | :------------------------------- |
| `C-n` / `C-f`  | `gfm-present-next-slide`       |
| `C-p` / `C-b`  | `gfm-present-previous-slide`   |
| `C-c q`        | `gfm-present-quit`             |
| `RET`          | `gfm-present-follow-link`      |

The keymap SHALL take precedence over the buffer's major-mode
bindings for the keys it owns.  Bindings SHALL be callable from any
evil state.

#### Scenario: enabling narrows to first heading

- **WHEN** `gfm-present-mode` is enabled in a buffer containing
  three H1 headings and point is at `point-min`
- **THEN** the buffer is narrowed to the region from the first H1's
  beginning to the second H1's beginning (exclusive)

#### Scenario: enabling narrows to enclosing heading when point is mid-slide

- **WHEN** point is on a line inside the second H1's region
- **AND** `gfm-present-mode` is enabled
- **THEN** the buffer is narrowed to the second H1's region

#### Scenario: disabling widens

- **WHEN** `gfm-present-mode` is enabled and the buffer is
  narrowed
- **AND** the user disables the mode
- **THEN** the buffer is widened

#### Scenario: keymap is callable from evil normal state

- **WHEN** point is in a `gfm-present-mode` buffer
- **AND** the active evil state is `normal`
- **AND** the user types `C-n`
- **THEN** `gfm-present-next-slide` is invoked

### Requirement: Heading-narrowed slide model

A slide SHALL be defined as the buffer region from the beginning of
one top-level heading line (matching `^# `) up to (but not including)
the next top-level heading line, or `point-max` for the last slide.
Sub-headings (`^##`, `^###`, etc.) SHALL flow within the slide and
SHALL NOT cause additional slide breaks.  HR lines (`^---$`) SHALL
NOT cause slide breaks.

Heading detection SHALL ignore lines inside fenced code blocks.

#### Scenario: H2 inside slide does not break

- **WHEN** a slide region contains both `^# Auth` and `^## Tokens`
- **THEN** the slide region extends from `^# Auth` until the next
  `^# ` line (or `point-max`)
- **AND** the H2 line is part of the slide

#### Scenario: HR inside slide does not break

- **WHEN** a slide region contains `^---` between paragraphs
- **THEN** the slide region is unaffected by the HR

#### Scenario: H1 inside fenced block ignored

- **WHEN** a fenced code block contains a line beginning with `# `
- **THEN** that line is not treated as a slide boundary

### Requirement: Heading navigation commands

`gfm-present-next-slide` SHALL widen the buffer, locate the H1
that follows the current narrowing's H1, and re-narrow to that H1's
region.  Calling the command at the last slide SHALL be a silent
no-op.

`gfm-present-previous-slide` SHALL widen the buffer, locate the
H1 that precedes the current narrowing's H1, and re-narrow to that
H1's region.  Calling the command at the first slide SHALL be a
silent no-op.

After re-narrowing, both commands SHALL place point at the start of
the new narrowing.

#### Scenario: next-slide advances narrowing

- **WHEN** the buffer is narrowed to slide N (0-based)
- **AND** the document has more than N+1 H1 headings
- **AND** the user invokes `gfm-present-next-slide`
- **THEN** the buffer is narrowed to slide N+1

#### Scenario: next-slide at last slide is a no-op

- **WHEN** the buffer is narrowed to the last H1's region
- **AND** the user invokes `gfm-present-next-slide`
- **THEN** the narrowing is unchanged
- **AND** no error is signalled

#### Scenario: previous-slide retreats narrowing

- **WHEN** the buffer is narrowed to slide N (N >= 1)
- **AND** the user invokes `gfm-present-previous-slide`
- **THEN** the buffer is narrowed to slide N-1

#### Scenario: previous-slide at first slide is a no-op

- **WHEN** the buffer is narrowed to slide 0
- **AND** the user invokes `gfm-present-previous-slide`
- **THEN** the narrowing is unchanged
- **AND** no error is signalled

### Requirement: Quit command

`gfm-present-quit` SHALL disable `gfm-present-mode` in the
current buffer (which widens it) and SHALL bury the buffer.  When the
buffer was created by `gfm-present-markdown` with no prior visit (i.e.
the buffer-local `gfm-present--owned-buffer` flag is non-nil), the
command SHALL kill the buffer instead of burying it.

#### Scenario: quit widens and buries

- **WHEN** the user invokes `gfm-present-quit` in a buffer that
  was already visiting the file before `gfm-present-markdown` ran
- **THEN** `gfm-present-mode` is disabled in that buffer
- **AND** the buffer is widened
- **AND** the buffer is buried (not killed)

#### Scenario: quit kills owned buffer

- **WHEN** the user invokes `gfm-present-quit` in a buffer that
  was opened solely by `gfm-present-markdown`
- **THEN** the buffer is killed

### Requirement: Heading-text in-doc link follow

The system SHALL resolve in-doc heading links by slug match against
the document's headings.

When the follow-link command is invoked on a markdown link of the
form `[label](#<slug>)`, the system SHALL search the (widened)
document for an H1, H2, H3, etc. heading whose slugified text matches
`<slug>`, push a mark at the click site, then re-narrow to the
enclosing H1 of the matched heading and place point at the matched
heading.

When no heading matches `<slug>`, the system SHALL fall through to
the markdown major mode's default link handler.

When `gfm-pretty-links` is active and its overlay keymap handles RET
on a decorated anchor link, the present-mode buffer SHALL register a
function on `gfm-pretty-links-after-anchor-jump-functions` (buffer-
local) that re-narrows the buffer to the H1 region enclosing the
jump target. The function SHALL be added on mode enable and removed
on mode disable. After the re-narrow, the function SHALL refresh
link-preview overlays for the new visible region.

#### Scenario: link to H1 narrows to that slide

- **WHEN** the document has H1 `# Token validation`
- **AND** the user follows a link with URL `#token-validation`
- **THEN** a mark is pushed at the click site
- **AND** the buffer is narrowed to the `Token validation` H1's region
- **AND** point is at the start of the heading line

#### Scenario: link to H2 narrows to enclosing H1

- **WHEN** the document has H1 `# Auth flow` containing H2
  `## Refresh tokens`
- **AND** the user follows a link with URL `#refresh-tokens`
- **THEN** a mark is pushed at the click site
- **AND** the buffer is narrowed to the `Auth flow` H1's region
- **AND** point is at the start of the `Refresh tokens` heading line

#### Scenario: missing slug falls through

- **WHEN** the user follows a link whose `#<slug>` matches no
  heading
- **THEN** the narrowing is unchanged
- **AND** `markdown-mode`'s default link handler is invoked

#### Scenario: pretty-links anchor jump re-narrows to enclosing H1

- **GIVEN** `gfm-present-mode` and `gfm-pretty-mode` both active in
  the buffer
- **AND** the buffer is narrowed to slide 1 which does not contain
  the heading `# Filter shape change`
- **AND** point is on a rendered `[Filter shape change](#filter-shape-change)`
  link inside slide 1
- **WHEN** the user presses `RET`
- **THEN** the buffer is narrowed to the `Filter shape change` H1's
  region (not widened, not slide 1)
- **AND** point is at the start of that heading line
- **AND** link-preview overlays for the new slide have been rendered

#### Scenario: hook is removed when present-mode disables

- **GIVEN** `gfm-present-mode` enabled in a buffer where
  `gfm-pretty-links-after-anchor-jump-functions` contained the
  present-mode subscriber
- **WHEN** `gfm-present-mode` is disabled
- **THEN** the present-mode subscriber is no longer present on the
  buffer-local hook

### Requirement: Heading slug normalisation

The system SHALL slugify heading text by lowercasing it, replacing
each run of non-alphanumeric characters with a single hyphen, and
stripping leading and trailing hyphens.  In-doc link targets and
heading texts SHALL be compared via this slugifier.

#### Scenario: punctuation collapses to hyphen

- **WHEN** the heading text is `# Auth & Tokens (v2)`
- **THEN** its slug is `auth-tokens-v2`

#### Scenario: leading and trailing punctuation stripped

- **WHEN** the heading text is `# !! Setup !!`
- **THEN** its slug is `setup`

### Requirement: Source-range link click action

The system SHALL escape to a real source buffer when a source-range
link is followed.

The follow-link command invoked on a `<path>#L<a>-L<b>` link SHALL
push a mark at the click site, then `find-file` the path (resolved
against the buffer's `default-directory`).  The destination buffer
SHALL be displayed via `display-buffer`, honouring
`other-window-prefix` for split control.

The destination buffer SHALL be left widened (no narrowing applied)
and editable (no `buffer-read-only` flip).  Point SHALL be placed
at the beginning of line `<a>`.  The line range `<a>..<b>` (or
`<a>..<a>` when no range was given) SHALL be momentarily pulsed via
`pulsar-highlight-pulse` so the reader can locate the range without
losing surrounding context; the pulse SHALL fade per the user's
`pulsar-delay` / `pulsar-iterations` settings and leave no
persistent overlay.

`gfm-present-mode` SHALL NOT be enabled in the destination buffer.

#### Scenario: click opens widened file at start of range

- **WHEN** the user clicks a `[fn](modules/auth.rs#L42-L67)` link
- **THEN** a mark is pushed at the click site
- **AND** the file `modules/auth.rs` is opened
- **AND** the destination buffer is NOT narrowed
- **AND** point is at the beginning of line 42
- **AND** the destination buffer is NOT read-only

#### Scenario: click pulses the requested line range

- **WHEN** the user clicks a `[fn](modules/auth.rs#L42-L67)` link
- **AND** `pulsar-mode` is enabled
- **THEN** lines 42-67 of the destination buffer are pulsed once
- **AND** no persistent overlay remains after the pulse fades

#### Scenario: other-window-prefix splits

- **WHEN** the user invokes `other-window-prefix` then clicks a
  `path#L` link
- **THEN** the destination buffer opens in a split window per
  `other-window-prefix` semantics

#### Scenario: back-jump returns to click site

- **WHEN** the user clicks a `path#L` link
- **AND** then invokes the standard back-mark binding (e.g. evil
  `C-o` or `C-x C-SPC`)
- **THEN** point returns to the link site in the doc buffer

### Requirement: Diff-range link click action

The system SHALL escape to a magit diff buffer when a diff-range
link is followed.

The follow-link command invoked on a `diff:<base>...<head>` link
(optionally scoped via `#<path>`) SHALL push a mark at the click
site, then call `magit-diff-range` with arguments
`(format "%s...%s" base head)` and a file-args list containing
`<path>` when given.  The destination buffer SHALL be displayed via
`display-buffer`, honouring `other-window-prefix`.

When `magit` is not loaded and `(require 'magit nil t)` returns nil,
the system SHALL signal a `user-error` whose message indicates that
magit is required.

#### Scenario: click opens magit diff

- **WHEN** the user clicks a `[change](diff:main...HEAD)` link
- **AND** magit is available
- **THEN** a mark is pushed at the click site
- **AND** `magit-diff-range` is invoked with range `"main...HEAD"`

#### Scenario: scoped diff passes path

- **WHEN** the user clicks a `[change](diff:main...HEAD#auth.rs)`
  link
- **THEN** `magit-diff-range` receives a file-args list containing
  `"auth.rs"`

#### Scenario: missing magit signals user-error

- **WHEN** the user clicks a diff link
- **AND** magit is not loaded and cannot be required
- **THEN** a `user-error` is signalled
- **AND** no buffer is opened

### Requirement: Other link forms pass through

The system SHALL delegate unrecognised link URLs to the markdown
major mode's default link handler.

When the follow-link command is invoked on a markdown link whose
URL is not one of `#<slug>`, `<path>#L<a>[-L<b>]`, or
`diff:<base>...<head>[#<path>]`, the system SHALL invoke
the markdown major mode's default link-following behaviour with no
presentation-side dispatch.

#### Scenario: http link passes through

- **WHEN** the user follows a `[example](https://example.com)` link
  in a `gfm-present-mode` buffer
- **THEN** `markdown-mode`'s default link handler runs
- **AND** no narrowing change occurs in the doc buffer

#### Scenario: plain path passes through

- **WHEN** the user follows a `[file](modules/auth.rs)` link (no
  `#L` anchor) in a `gfm-present-mode` buffer
- **THEN** `markdown-mode`'s default link handler runs

### Requirement: Document revert resilience

The system SHALL install buffer-local hooks on `before-revert-hook`
and `after-revert-hook` while `gfm-present-mode` is enabled.

The `before-revert-hook` handler SHALL capture, into a buffer-local
plist `gfm-present--revert-anchor`:

- `:slug` — slugified text of the H1 currently containing the
  narrowing.
- `:index` — ordinal of that H1 among all H1s in the (widened)
  document, 0-based.
- `:fingerprint` — a substring of up to 80 characters starting at
  point.
- `:window-start-offset` — `(- (window-start) (point))` for the
  selected window showing the buffer, or 0 when not displayed.

The `after-revert-hook` handler SHALL widen the buffer, then narrow
according to the captured anchor:

1. Search for an H1 whose slug equals `:slug`; if found, narrow to
   its region.
2. Else, narrow to the H1 at ordinal `:index` if one exists.  When
   the document has fewer H1s than `:index + 1`, narrow to the last
   H1.
3. Else, narrow to the first H1 if any exist.  Otherwise, leave the
   buffer widened.

After narrowing, the handler SHALL search for `:fingerprint` inside
the new narrowing.  If found, it SHALL set point to the match start
and call `set-window-start` with `(+ point :window-start-offset)`.
Otherwise, point SHALL be left at the start of the narrowing.

After point and scroll restore, preview overlays SHALL be rebuilt as
specified in `Preview refresh on slide entry`.

#### Scenario: slug match restores narrowing across rename-elsewhere

- **WHEN** the user is narrowed to slide `# Auth flow`
- **AND** the agent edits the doc to insert a new slide `# Setup`
  before it
- **AND** auto-revert reloads the buffer
- **THEN** the buffer is narrowed to the `# Auth flow` slide
- **AND** the slide's ordinal index has changed but the user's view
  of `Auth flow` is preserved

#### Scenario: index fallback when slug renamed

- **WHEN** the user is narrowed to slide N
- **AND** the agent renames that slide's heading from
  `# Auth flow` to `# Authorization`
- **AND** auto-revert reloads the buffer
- **THEN** the buffer is narrowed to slide N (the renamed
  `Authorization` slide)

#### Scenario: fingerprint restores point

- **WHEN** point is on a unique 80-character substring inside slide
  3 before revert
- **AND** revert keeps that substring intact
- **THEN** after revert, point is at the start of that substring

#### Scenario: window scroll restored

- **WHEN** before revert, `(- (window-start) (point))` is `-200`
- **AND** fingerprint matching succeeds after revert
- **THEN** the post-revert window-start is `(+ (point) -200)`

### Requirement: Click escape preserves back-jump

The system SHALL push a mark at the click site before initiating
any link navigation that leaves the current buffer or changes the
narrowing.

For every follow-link action that navigates away from the current
buffer (heading-text in-doc, source-range, diff-range), the system
SHALL call `push-mark` at the click site (without activating the
region) before initiating the navigation.

#### Scenario: heading link pushes mark

- **WHEN** the user follows a `[label](#some-heading)` link in the
  doc
- **THEN** the mark ring contains a mark at the click site

#### Scenario: path link pushes mark

- **WHEN** the user follows a `[label](path#L42)` link
- **THEN** the mark ring contains a mark at the click site

#### Scenario: diff link pushes mark

- **WHEN** the user follows a `[label](diff:main...HEAD)` link
- **THEN** the mark ring contains a mark at the click site
