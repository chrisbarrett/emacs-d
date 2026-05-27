## 1. Numeric detection

- [ ] 1.1 Add `gfm-pretty-tables--numeric-cell-rx` defconst with the
      regex `(rx bos (? (any "+-")) (+ digit) (? "." (* digit)) (? (any "eE") (? (any "+-")) (+ digit)) eos)`.
- [ ] 1.2 Add `gfm-pretty-tables--numeric-cell-p` taking a trimmed
      source string, returning non-nil iff it matches the regex.
- [ ] 1.3 Add `gfm-pretty-tables--column-alignment` taking `body-rows`
      (list of cell-lists) and `n-cols`, returning a vector of
      `'left` / `'right` symbols of length `n-cols`. Right iff
      every body cell is numeric-or-empty AND at least one is
      numeric (per the spec scenarios).

## 2. Failing-test scaffold (TDD red)

- [ ] 2.1 In `lisp/gfm/gfm-pretty-tests.el` add a tag like
      `:tags '(tables-align)` and write tests covering every
      spec scenario:
      `all-numeric`, `sparse-numeric`, `mixed`, `all-empty`,
      `signed-decimal-scientific`, `commas-stay-left`,
      `percent-stays-left`, `wrapped-numeric-pads-leading`,
      and a `bounds-vec parity` test.
- [ ] 2.2 Run `make test-quick` (or eldev tag selector) and
      confirm the new tests fail for the right reason
      (pad still trailing).

## 3. Compose-row alignment

- [ ] 3.1 Extend `gfm-pretty-tables--compose-row` with a
      `col-align` parameter; replace the `concat " " cell pad " "`
      form with the `pcase`-on-align variant from design.md.
- [ ] 3.2 Extend `gfm-pretty-tables--compose-row-from-layout`
      with a `col-align` parameter; pass it through to
      `--compose-row`.
- [ ] 3.3 Extend `gfm-pretty-tables--compose-multiline-row`
      with a `col-align` parameter for symmetry (it's still
      used by the public rendering primitives spec).

## 4. Thread alignment through apply-table-display

- [ ] 4.1 Inside `gfm-pretty-tables--apply-table-display`,
      after the parse and before the compose phase, compute
      `col-align` via `--column-alignment` over `body-rows`.
- [ ] 4.2 Pass `col-align` to every
      `--compose-row-from-layout` call (header, body-default,
      body-alt).
- [ ] 4.3 Confirm `col-align` is computed inside
      `gfm-pretty-tables--time-phase 'layout` so the per-table
      timing buckets stay accurate.

## 5. Bounds-vec invariant

- [ ] 5.1 Confirm `--row-char-bounds` requires no change — the
      segment length `(+ 2 cell-len pad)` is independent of
      alignment.
- [ ] 5.2 Add (or extend) the bounds parity test from 2.1 so
      it renders the same parsed table twice (once forced
      left, once forced right via let-bound col-align) and
      asserts byte-equal bounds.

## 6. Verification

- [ ] 6.1 Run `make test` and confirm all suites pass, including
      the narrowing-regression suite under
      `modules/lang-markdown/tests.el`.
- [ ] 6.2 In a sandbox Emacs (`emacs-claude-sandbox-…`), load
      `gfm-pretty-mode` on a buffer containing the LOC table
      from the proposal motivation and visually confirm the
      LOC column is right-aligned, header included.
- [ ] 6.3 Confirm the user-reported sample table renders
      with `script` and `role` left-aligned, `LOC`
      right-aligned, no shift in border or rule positions.
