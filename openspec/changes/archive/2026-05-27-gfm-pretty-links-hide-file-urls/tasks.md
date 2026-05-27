## 1. Tests (red)

- [x] 1.1 Add `gfm-pretty-links/file-link-url-overlay-hides-span` to
      `lisp/gfm/gfm-pretty-links-tests.el`. After `--rebuild` on `[ops](./scripts/x.sh)`,
      assert there is a URL-side overlay over `(./scripts/x.sh)` with `display` = `""`
      carrying `gfm-pretty-links-class` = `file` and `gfm-pretty-links-url` = `./scripts/x.sh`.
- [x] 1.2 Add `gfm-pretty-links/file-link-with-code-label-hides-url-span` for
      `[`pretty`](../../path/x.hcl)` — same assertions; ensure title overlay's
      `display` string is `` `pretty` `` (backticks preserved) faced with
      `gfm-pretty-links-file-face`.
- [x] 1.3 Run `make test-quick` — both new tests fail.

## 2. Implementation (green)

- [x] 2.1 `lisp/gfm/gfm-pretty-links.el`: in
      `gfm-pretty-links--make-overlay`, change the empty-display branch
      from `((eq class 'anchor) "")` to `((memq class '(anchor file)) "")`.
- [x] 2.2 In `gfm-pretty-links--decorate-link`, widen the URL-overlay
      guard from `'(web anchor)` to `'(web anchor file)`.
- [x] 2.3 Run `make test-quick` — new tests pass; existing tests still
      pass.

## 3. Documentation / commentary

- [x] 3.1 Update top-of-file commentary (around L21-25) to say `anchor`
      and `file` links hide the URL span via an empty-display overlay;
      remove the "file links render title-only" phrasing that implied
      no URL overlay.
- [x] 3.2 Update `gfm-pretty-links--decorate-link` docstring (around
      L529-540) — replace "File links produce only a title-side
      overlay; the URL span renders raw" with the matching empty-display
      description.

## 4. File icons (extension)

- [x] 4.1 Update existing file-link tests to no longer assert
      `display` = `""`: in `gfm-pretty-links-tests.el`, change
      `file-link-url-overlay-hides-span` and
      `file-link-with-code-label-hides-url-span` to assert the URL-side
      overlay exists with metadata (`class` = `file`, `url` = path) and
      `display` is a non-nil string. In `gfm-pretty-tests.el`, update
      `gfm-pretty-links-file-link-hides-url-span` likewise.
- [x] 4.2 Add `gfm-pretty-links/file-link-icon-fallback-without-nerd-icons`
      to `gfm-pretty-links-tests.el`: `cl-letf` `fboundp` on
      `nerd-icons-icon-for-file` to return nil; assert the URL-side
      overlay's `display` is `""` and metadata is preserved.
- [x] 4.3 In `gfm-pretty-links--make-overlay`, drop `file` from the
      empty-display branch so it falls into the icon branch
      (`((eq class 'anchor) "")`).
- [x] 4.4 Update commentary (top-of-file, around L19-25) and
      `--decorate-link` docstring to describe file icons + `""`
      fallback.
- [x] 4.5 Run `make test` — full suite green.

## 5. Strip wrapping backticks from title display

- [x] 5.0 Test: title overlay strips wrapping backticks
      (`gfm-pretty-links/title-strips-wrapping-backticks`):
      `` [`pretty`](./x.hcl) `` → title `display` = `pretty`;
      `gfm-pretty-links-label` overlay prop = `` `pretty` `` (unmodified).
- [x] 5.0a Test: title overlay keeps interior backticks
      (`gfm-pretty-links/title-keeps-interior-backticks`):
      `` [say `hi` world](./x.md) `` → title `display` = `say \`hi\` world`.
- [x] 5.0b Update existing `file-link-with-code-label-hides-url-span`
      to assert title `display` = `pretty` (no backticks).
- [x] 5.0c Add helper `gfm-pretty-links--strip-wrapping-backticks` and
      apply it in `--make-overlay`'s title branch.
- [x] 5.0d Run `make test` — full suite green.

## 6. Verify

- [x] 6.1 `make test` — full suite green.
- [x] 6.2 Open the original repro file in a live Emacs session and
      confirm the EOF list renders with file icons and stripped
      backticks:
      `~/src/gotracksuit/platform/live__worktrees/sre-569-extra-gh-environment-support/_stacks/tf-repos/README.md`.
- [x] 6.3 `openspec validate gfm-pretty-links-hide-file-urls` still clean.
