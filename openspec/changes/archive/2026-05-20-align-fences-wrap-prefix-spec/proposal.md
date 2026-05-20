## Why

The stable `gfm-pretty` spec at `Requirement: Code-fence bordered-block rendering` describes fences' wrap-prefix as `│ ` — but the implementation has always used a distinct continuation glyph (`⋱`, now `↪`) for the wrap-prefix. `│ ` is the body-line *before-string* / lhs decoration, not the wrap-prefix. The two are visually and semantically distinct (the before-string runs on every body line; the wrap-prefix appears only on continuation visual lines of a wrapped body line). The spec conflates them, so a reader reconciling spec against code would either rewrite the code or chase a phantom regression.

## What Changes

- Correct the fences bordered-block rendering requirement to describe the body-line decoration as `│ ` *before-string* plus an `↪ ` *wrap-prefix* on continuation visual lines.
- Leave the callouts requirement (line 631) unchanged — for callouts the wrap-prefix really is `│ ` (the box's left edge continues onto wrapped rows).
- Do not constrain the specific continuation glyph in the spec — `↪ ` is the current choice, but spec text should leave room for future glyph swaps without re-issuing a spec change.

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
- `gfm-pretty`: clarify Code-fence bordered-block rendering requirement to distinguish the body before-string (`│ `) from the continuation wrap-prefix glyph.

## Impact

- `openspec/specs/gfm-pretty/spec.md`: edit one requirement body.
- No code changes. Implementation already matches the corrected wording.
- No downstream breakage; this is a spec hygiene fix.
