## 1. Recognisers and preformatted-context helpers

- [ ] 1.1 Add `gfm-pretty-link-previews--bare-line-rx` (rx form): leading blanks, optional list/blockquote marker, blanks, captured non-blank token, trailing blanks, eol.
- [ ] 1.2 Add `gfm-pretty-link-previews--fence-ranges` — widened buffer scan returning a list of `(open-bol . close-eol)` pairs using locally-defined fence open/close regexes mirroring `gfm-pretty-fences--open-re` / `--close-re` (don't `require` the fences decorator).
- [ ] 1.3 Add `gfm-pretty-link-previews--inside-any-range-p` (pos, ranges) — small helper, or reuse `gfm-pretty--in-ranges-p` if its signature fits.
- [ ] 1.4 Add `gfm-pretty-link-previews--line-indent-cols` — count leading spaces on the current line.
- [ ] 1.5 Add `gfm-pretty-link-previews--inline-code-wrap-p` — true when the line's significant content (after marker strip) is `` `<token>` `` only.
- [ ] 1.6 Add `gfm-pretty-link-previews--preformatted-line-p (pos fence-ranges)` — combines fence-range membership, indent ≥ 4, and inline-code-wrap.

## 2. Bare-line block collection

- [ ] 2.1 Add `gfm-pretty-link-previews--bare-source-path-ok-p (path)` — true iff PATH contains at least one `/`.
- [ ] 2.2 Add `gfm-pretty-link-previews--token-bounds-on-line` — given the bare-line rx match-data, return `(token-start . token-end)`.
- [ ] 2.3 Add a bare-line collection pass to `gfm-pretty-link-previews--collect-blocks`: after the existing bracketed pass, compute fence-ranges once, then walk lines (`forward-line` loop on widened buffer), match each against the bare-line rx, parse the token via the existing `--parse-source-link` / `--parse-diff-link`, apply the source-path `/` guard, skip preformatted lines, and push a `--block` whose `range` is the token bounds and `payload` matches the bracketed source/diff payload shape.
- [ ] 2.4 Verify that `apply-block` does not need changes — the bare-line block's `range` is a token span; `--apply-block` already builds its overlay from `range` alone.
- [ ] 2.5 Verify that `--continuation-prefix` handles bare-line ranges correctly (the prefix logic keys off the line's leading characters relative to `(car range)`).

## 3. Tests — bare-line discovery

- [ ] 3.1 Test: bare absolute path with `#L<a>-L<b>` on its own line → overlay with display string covering the token, top border embeds abbreviated path and range.
- [ ] 3.2 Test: bare tilde-prefixed path `~/foo/bar.rs#L1-L3` → overlay reads from `(expand-file-name "~/foo/bar.rs")`.
- [ ] 3.3 Test: bare relative path `modules/auth.rs#L42-L48` → overlay resolves against `default-directory`.
- [ ] 3.4 Test: bare basename-only `auth.rs#L1-L5` → no overlay (path lacks `/`).
- [ ] 3.5 Test: bare diff-range `diff:abc1234...def5678#main.tf` → overlay with diff-range preview (LHS-margin box).
- [ ] 3.6 Test: bare reference under list-item marker `- /abs/path.rs#L1-L3` → overlay covers token only, continuation prefix indents box rows under the `┌`.
- [ ] 3.7 Test: bare reference under blockquote marker `> /abs/path.rs#L1-L3` → overlay with blockquote-rail continuation prefix when `gfm-pretty-blockquotes` is enabled.

## 4. Tests — gating and preformatted exclusion

- [ ] 4.1 Test: bare reference inside ``` `````` fence → no overlay.
- [ ] 4.2 Test: bare reference inside a 4-backtick fence → no overlay (open regex covers `≥ 3` backticks).
- [ ] 4.3 Test: bare reference with 4-space leading indent → no overlay.
- [ ] 4.4 Test: bare reference wrapped in inline code `` `…` `` on its own line → no overlay.
- [ ] 4.5 Test: bare reference embedded in prose `See /abs/path.rs#L1 for details.` → no overlay.
- [ ] 4.6 Test: bracketed and bare forms on adjacent lines both produce overlays, no double-decoration on either.
- [ ] 4.7 Test: bracketed-form regressions (existing tests in `gfm-pretty-link-previews-tests.el`) continue to pass without modification.

## 5. Verification

- [ ] 5.1 Run `make test-quick` and `make test-integration`; ensure all `gfm-pretty-link-previews*` tests pass.
- [ ] 5.2 Manually sanity-check in a scratch markdown buffer: paste agent-style bare references, toggle `gfm-pretty-mode`, confirm previews render exactly as the bracketed equivalents.
- [ ] 5.3 Toggle `gfm-pretty-toggle-decorator 'link-previews` off and back on; confirm bare-line overlays tear down and rebuild cleanly.
- [ ] 5.4 `openspec validate gfm-bare-link-previews` passes; archive-readiness check (`openspec status --change gfm-bare-link-previews`) shows all tasks complete.
