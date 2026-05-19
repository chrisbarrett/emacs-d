## 1. Spec edit

- [ ] 1.1 In `openspec/specs/gfm-pretty/spec.md`, replace the second bullet of the `Code-fence bordered-block rendering` requirement so it distinguishes the `│ ` body-line *before-string* from the continuation `wrap-prefix` glyph, matching the wording in `openspec/changes/align-fences-wrap-prefix-spec/specs/gfm-pretty/spec.md` (the MODIFIED block).
- [ ] 1.2 Insert the new `Scenario: Wrapped body line shows continuation glyph` from the delta into the same requirement, after the existing `Scenario: Fence with language`.
- [ ] 1.3 Add the trailing paragraph from the delta noting that the wrap-prefix glyph is an implementation-owned styling choice.

## 2. Verification

- [ ] 2.1 `openspec validate gfm-pretty --strict` passes against the edited stable spec.
- [ ] 2.2 `make test` passes (sanity — no code changed; this confirms the spec edit did not collide with anything tooling reads).
- [ ] 2.3 Re-read the edited requirement against `lisp/gfm/gfm-pretty-fences.el:329-332` and `lisp/gfm/gfm-pretty-borders.el:187-192` and confirm the wording matches what the code does (`│ ` before-string + separate wrap-prefix glyph).
