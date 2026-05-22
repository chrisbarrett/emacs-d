## ADDED Requirements

### Requirement: Link decoration skips source-range and diff URL forms

The links decorator SHALL NOT collect or render any link whose
resolved URL matches one of these two forms:

| Form          | Shape                                          | Example                       |
| :------------ | :--------------------------------------------- | :---------------------------- |
| source-range  | `<path>#L<digits>` optionally `-L<digits>`     | `/repo/foo.yml#L13-L22`       |
| diff          | `diff:<base>...<head>` optionally `#<path>`    | `diff:main...HEAD#README.md`  |

The check SHALL be a pure function of the resolved URL string,
applied during the discovery filter pass alongside the existing
code-region exclusion. Skipped links produce no title-side overlay,
no URL-side overlay, and no overlay keymap. Reference links are
checked against their resolved URL (from the `[label]:` definition),
not the source shape.

The exclusion exists because `gfm-present-mode` owns rendering and
RET dispatch for these URL forms via its own overlays and minor-
mode keymap. Pretty-links overlays on the same span would either
stack incompatibly (display garbling) or shadow the gfm-present RET
binding via overlay-keymap precedence.

#### Scenario: Inline source-range link skipped

- **GIVEN** `[snippet](/repo/foo.yml#L13-L22)`
- **THEN** the links decorator does NOT create any overlay for that
  span
- **AND** the buffer text renders raw (whatever other modes do with
  it)

#### Scenario: Inline source-range link with single line

- **GIVEN** `[line](/repo/x.el#L42)`
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Inline diff link skipped

- **GIVEN** `[changed](diff:main...feature)`
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Inline diff link with file scope skipped

- **GIVEN** `[changed](diff:main...feature#path/to/file.el)`
- **THEN** the links decorator does NOT create any overlay

#### Scenario: Reference link to source-range URL skipped

- **GIVEN** `[snippet][src]` with definition line
  `[src]: /repo/foo.yml#L13-L22`
- **THEN** the links decorator does NOT create any overlay for the
  reference link

#### Scenario: Plain file link without line range unaffected

- **GIVEN** `[ops](./scripts/x.sh)` (no `#L...` suffix)
- **THEN** the links decorator creates its usual title-side overlay
  with the `file` class

#### Scenario: Plain anchor link unaffected

- **GIVEN** `[Setup](#setup)`
- **THEN** the links decorator creates its usual title-side overlay
  with the `anchor` class
