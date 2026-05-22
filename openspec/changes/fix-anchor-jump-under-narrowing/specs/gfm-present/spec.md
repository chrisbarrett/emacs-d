## MODIFIED Requirements

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
