## 1. Token-unwrap helper (red → green)

- [x] 1.1 Write failing tests for `gfm-pretty-link-previews--unwrap-token`: `[X]`→inner `X`, `<X>`→inner `X`, unwrapped `X`→nil-strip (returns token unchanged with 0 offsets), mismatched `[X>`→no unwrap, single-sided `[X` / `X]`→no unwrap, empty/`[]`→no inner.
- [x] 1.2 Implement `--unwrap-token` to strip exactly one matched `[ ]` or `< >` pair and report the delimiter offsets; make 1.1 pass.

## 2. Wire wrapped recognition into the bare-line pass

- [x] 2.1 Write failing recognition tests (use the `--rebuild` + overlay-introspection harness already used by the bare-line tests): bracket-wrapped source on its own line creates one overlay; angle-wrapped source on its own line creates one overlay; wrapped diff (`[diff:main...HEAD#path]`) creates an LHS-margin overlay.
- [x] 2.2 Extend Pass 2 of `--collect-blocks` to attempt `--unwrap-token` before parsing, parsing the inner token while keeping the original (delimiter-spanning) token bounds as the block range; make 2.1 pass.
- [x] 2.3 Add a test asserting the overlay range covers the FULL wrapped token including the `[ ]` / `< >` delimiters (no stray delimiter char beside the box).

## 3. Gating parity with the unwrapped form

- [x] 3.1 Test + verify the `/`-in-path gate rejects wrapped basename-only tokens (`[auth.rs#L1-L5]`, `<auth.rs#L1-L5>`).
- [x] 3.2 Test + verify standalone gating: wrapped token embedded in prose (`See [a/b.rs#L1-L2] here.`) creates no overlay; marker-prefixed wrapped token (`- [a/b.rs#L1-L2]`, `> [a/b.rs#L1-L2]`) is decorated with the marker rendering to the left.
- [x] 3.3 Test + verify preformatted-context exclusion for wrapped tokens: inside triple-backtick fence, inside ≥4-space indent, wrapped-in-inline-code on its own line.

## 4. Collision / precedence regression coverage

- [x] 4.1 Test that a link reference definition line (`[a/b.rs#L1-L5]: https://example.com`) creates no overlay (non-standalone trailing destination).
- [x] 4.2 Test that a real `[label](url)` bracketed link is still claimed by Pass 1 and not double-claimed or altered by the new wrapped path.
- [x] 4.3 Test that mismatched delimiters (`[a/b.rs#L1-L5>`) create no overlay.

## 5. Spec sync + checks

- [x] 5.1 Run `make test` (in the nix devShell) and confirm the link-previews suite passes, including the new cases.
- [x] 5.2 Confirm the delta spec's scenarios each map to a passing test; adjust either to keep them in lock-step.
