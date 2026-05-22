## 1. Implement anchor URL hiding

- [ ] 1.1 In `lisp/gfm/gfm-pretty-links.el`, extend the class gate in
  `gfm-pretty-links--decorate-link` so anchor-class records also
  receive a URL-side overlay.
- [ ] 1.2 In `gfm-pretty-links--make-overlay`, when `side` is `url`
  and the record's class is `anchor`, set `display` to the empty
  string (skip the `gfm-pretty-links--icon-for-target` lookup).

## 2. Update tests

- [ ] 2.1 In `lisp/gfm/gfm-pretty-links-tests.el`, locate any test
  asserting "anchor link omits URL overlay" and update it to assert
  "anchor link has URL overlay with empty `display`".
- [ ] 2.2 Add a new ERT verifying the URL-side overlay's metadata
  (`gfm-pretty-links-class` = `anchor`, `gfm-pretty-links-url` =
  `#<slug>`) survives the empty-display rendering.

## 3. Verify

- [ ] 3.1 Run `make test` and confirm green.
- [ ] 3.2 Open `/private/tmp/presentations/2026-05-22T06-33-tg-native-drift-filters.md`
  in presentation mode; confirm TOC entries render as `The problem`
  not `The problem(#the-problem)`.
- [ ] 3.3 Verify RET on a hidden anchor still jumps (overlay metadata
  preserved) — point on the rendered `The problem`, press `RET`,
  land on the target heading.
- [ ] 3.4 Run `openspec validate hide-anchor-url-span --strict` and
  confirm zero errors.
