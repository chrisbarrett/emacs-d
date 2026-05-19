## 1. Predicate

- [ ] 1.1 Write a failing test asserting
      `gfm-pretty-links--in-code-p POS` is non-nil for positions
      inside a fenced block, an indented block, and an inline
      code span; nil for prose and for code-adjacent positions
- [ ] 1.2 Implement `gfm-pretty-links--in-code-p` delegating to
      `markdown-code-block-at-pos` and
      `markdown-inline-code-at-pos-p`

## 2. Filter integration

- [ ] 2.1 Write failing tests asserting:
      - `[foo](bar)` inside a fenced block produces zero link
        records from `:collect`
      - same for an indented (4-space) block
      - same for an inline code span
      - a link adjacent to but outside code still decorates
- [ ] 2.2 In `gfm-pretty-links--blocks-in-range`, reject any
      record whose record-span start position satisfies
      `gfm-pretty-links--in-code-p`; apply alongside the existing
      ref-def-range exclusion

## 3. Verification

- [ ] 3.1 `make test-quick` passes
- [ ] 3.2 `make test` passes
- [ ] 3.3 Manual check in a real markdown buffer with a fenced
      block of example syntax — confirm `[x](y)` written inside
      the fence renders raw
