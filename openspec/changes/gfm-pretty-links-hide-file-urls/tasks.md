## 1. Tests (red)

- [ ] 1.1 Add `gfm-pretty-links/file-link-url-overlay-hides-span` to
      `lisp/gfm/gfm-pretty-links-tests.el`. After `--rebuild` on `[ops](./scripts/x.sh)`,
      assert there is a URL-side overlay over `(./scripts/x.sh)` with `display` = `""`
      carrying `gfm-pretty-links-class` = `file` and `gfm-pretty-links-url` = `./scripts/x.sh`.
- [ ] 1.2 Add `gfm-pretty-links/file-link-with-code-label-hides-url-span` for
      `[`pretty`](../../path/x.hcl)` — same assertions; ensure title overlay's
      `display` string is `` `pretty` `` (backticks preserved) faced with
      `gfm-pretty-links-file-face`.
- [ ] 1.3 Run `make test-quick` — both new tests fail.

## 2. Implementation (green)

- [ ] 2.1 `lisp/gfm/gfm-pretty-links.el`: in
      `gfm-pretty-links--make-overlay`, change the empty-display branch
      from `((eq class 'anchor) "")` to `((memq class '(anchor file)) "")`.
- [ ] 2.2 In `gfm-pretty-links--decorate-link`, widen the URL-overlay
      guard from `'(web anchor)` to `'(web anchor file)`.
- [ ] 2.3 Run `make test-quick` — new tests pass; existing tests still
      pass.

## 3. Documentation / commentary

- [ ] 3.1 Update top-of-file commentary (around L21-25) to say `anchor`
      and `file` links hide the URL span via an empty-display overlay;
      remove the "file links render title-only" phrasing that implied
      no URL overlay.
- [ ] 3.2 Update `gfm-pretty-links--decorate-link` docstring (around
      L529-540) — replace "File links produce only a title-side
      overlay; the URL span renders raw" with the matching empty-display
      description.

## 4. Verify

- [ ] 4.1 `make test` — full suite green.
- [ ] 4.2 Open the original repro file in a live Emacs session and
      confirm the EOF list renders title-only:
      `~/src/gotracksuit/platform/live__worktrees/sre-569-extra-gh-environment-support/_stacks/tf-repos/README.md`.
- [ ] 4.3 `openspec validate gfm-pretty-links-hide-file-urls` still clean.
