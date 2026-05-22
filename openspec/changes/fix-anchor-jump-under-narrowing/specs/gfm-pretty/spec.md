## MODIFIED Requirements

### Requirement: RET follows the link when point is on the decoration

The links decorator SHALL install an overlay keymap binding `RET` to
a class-dispatched follow handler. The handler SHALL read the link's
target class and URL from the overlay under point and act per class:

| Class    | Action                                                                  |
| :------- | :---------------------------------------------------------------------- |
| `web`    | call `markdown--browse-url` on the URL                                  |
| `anchor` | jump to the heading whose generated slug matches the anchor portion    |
| `file`   | `find-file` on the URL, expanded relative to `buffer-file-name`'s directory (or `default-directory` when the buffer has no file) |

The binding SHALL fire when point is on the rendered overlay.

For the `anchor` class the handler SHALL search the entire buffer for
the matching heading regardless of the current narrowing, and on
match SHALL widen the buffer before moving point to the heading.
After a successful jump the handler SHALL run the abnormal hook
`gfm-pretty-links-after-anchor-jump-functions` with the target buffer
position as its single argument. Subscribers can use the hook to
restore their preferred narrowing or apply additional decoration.

#### Scenario: RET on web link

- **WHEN** point is on a rendered `web` link's title and user presses
  `RET`
- **THEN** `markdown--browse-url` opens the URL

#### Scenario: RET on anchor link

- **GIVEN** a heading `## Setup Steps` in the buffer
- **AND** point is on a rendered `[go](#setup-steps)` link
- **WHEN** user presses `RET`
- **THEN** point moves to the `Setup Steps` heading

#### Scenario: RET on anchor link in narrowed buffer

- **GIVEN** a buffer narrowed to a region that does not include the
  `## Setup Steps` heading
- **AND** point is on a rendered `[go](#setup-steps)` link inside the
  narrowed region
- **WHEN** user presses `RET`
- **THEN** the buffer is widened
- **AND** point moves to the `Setup Steps` heading

#### Scenario: anchor jump hook fires with target position

- **GIVEN** a function `F` registered on
  `gfm-pretty-links-after-anchor-jump-functions`
- **WHEN** RET on an anchor link successfully jumps to a heading at
  buffer position `P`
- **THEN** `F` is called once with `P` as its sole argument
- **AND** `F` is called after point has moved and the buffer has been
  widened

#### Scenario: RET on anchor with no matching heading

- **GIVEN** a rendered `[go](#missing)` link
- **AND** no heading in the buffer has slug `missing`
- **WHEN** user presses `RET`
- **THEN** the handler raises `user-error` with a message naming the
  missing anchor
- **AND** `gfm-pretty-links-after-anchor-jump-functions` is not run

#### Scenario: RET on file link

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer
  whose file lives at `/repo/README.md`
- **WHEN** user presses `RET`
- **THEN** `find-file` opens `/repo/scripts/x.sh`

#### Scenario: RET on file link in fileless buffer

- **GIVEN** a rendered `[ops](./scripts/x.sh)` link in a buffer with
  no `buffer-file-name`
- **WHEN** user presses `RET`
- **THEN** `find-file` resolves the path against `default-directory`
