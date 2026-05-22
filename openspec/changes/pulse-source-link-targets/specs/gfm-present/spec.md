## MODIFIED Requirements

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

## REMOVED Requirements

### Requirement: Reusable narrowed-source renderer

**Reason**: Source-link click no longer narrows the destination
buffer.  Pulse-and-goto replaces the narrowed-source view; the
reusable renderer has no remaining caller.

**Migration**: callers that relied on the helper (none in tree)
should switch to `pulsar-highlight-pulse` plus an explicit
`goto-char` / `find-file`.

### Requirement: Focus highlight bounded to text glyphs

**Reason**: No focus overlay is painted any more — the momentary
pulse is the locator.  `gfm-present-focus-face` is removed along
with the per-line overlay machinery, so the glyph-bounding
guarantee is moot.
