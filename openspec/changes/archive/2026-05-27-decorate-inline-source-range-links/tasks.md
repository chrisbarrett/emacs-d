## 1. Engine helper

- [x] 1.1 Add `gfm-pretty-standalone-span-p (beg end)` to `lisp/gfm/gfm-pretty.el`; pure positional check, no match-data leakage to callers, returns non-nil for "line content with span removed matches `^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$`"
- [x] 1.2 Add ERTs in `lisp/gfm/gfm-pretty-tests.el` (or sibling) covering: whole-line span, list-item-only span, blockquote-marker-only span, ordered-list-item span, span embedded in prose
- [x] 1.3 Refactor `gfm-pretty-link-previews--standalone-link-p` (`lisp/gfm/gfm-pretty-link-previews.el:84-108`) to delegate to the engine helper; keep the wrapper symbol for internal call sites; run existing link-previews tests to confirm no regression

## 2. Pretty-links skip predicate

- [x] 2.1 Add a `kind` parameter to the skip path (or accept the full link record) so the predicate can gate on `kind = inline` — record threading flows through `gfm-pretty-links--collect`'s filter pass (`lisp/gfm/gfm-pretty-links.el:480-500`)
- [x] 2.2 Update `gfm-pretty-links--skip-url-p` (or replace its call site) to: defer iff `(kind = inline) AND (source-range URL) AND (gfm-pretty-standalone-span-p record-span)`; diff URLs continue to defer unconditionally regardless of kind/standalone
- [x] 2.3 Reconcile the docstrings on `--skip-url-p` and `gfm-pretty-links--decorate-link` to describe the new semantics; remove the now-stale "owned by gfm-present-mode" framing

## 3. Icon resolver fragment stripping

- [x] 3.1 In `gfm-pretty-links--icon-for-target` (`lisp/gfm/gfm-pretty-links.el:244-264`), strip any `#…` suffix from the URL before computing the basename for the `file:` and relative-path branches; leave `http(s):`, `#`-prefixed anchor, and other-scheme branches untouched
- [x] 3.2 Add an ERT asserting `nerd-icons-icon-for-file` is invoked with `foo.el` for URL `/path/foo.el#L42-L48` (use a stub or `cl-letf`)

## 4. RET handler for source-range fragments

- [x] 4.1 In the file-class RET branch of `gfm-pretty-links`' overlay-keymap follow handler, parse an optional `#L<n>[-L<n>]` suffix from the overlay's `gfm-pretty-links-url`: `find-file` on the path portion, then `goto-line <n>` when the suffix is present
- [x] 4.2 Ensure the path expansion still resolves against `buffer-file-name`'s directory (or `default-directory` when fileless) per spec L2287; the fragment strip happens before path expansion

## 5. Behavioural tests

- [x] 5.1 ERT: inline-in-prose `[snippet](/path/foo.el#L42-L48)` produces a title-side overlay with class `file` and a URL-side overlay covering the parens
- [x] 5.2 ERT: standalone `[snippet](/path/foo.el#L42-L48)` (whole line) produces no pretty-links overlay
- [x] 5.3 ERT: list-item standalone `- [snippet](/path/foo.el#L42-L48)` produces no pretty-links overlay
- [x] 5.4 ERT: blockquote standalone `> [snippet](/path/foo.el#L42-L48)` produces no pretty-links overlay
- [x] 5.5 ERT: reference-style `[snippet][src]` with `[src]: /path/foo.el#L42` produces a pretty-links title-side overlay with class `file`
- [x] 5.6 ERT: inline diff link `[changed](diff:main...feature)` produces no pretty-links overlay (regression of unconditional diff skip)
- [x] 5.7 ERT: RET on inline source-range link calls `find-file` on path and `goto-line` on the start line

## 6. Narrowing regression

- [x] 6.1 Add a `:tags '(narrowing-regression)` block to `modules/lang-markdown/tests.el` (per `AGENTS.md`) covering inline source-range overlays: narrow → `gfm-pretty-links--rebuild` → widen → rebuild converges with the clean widened set
- [x] 6.2 Sanity-run the existing `gfm-pretty-fences`, `gfm-pretty-callouts`, `gfm-pretty-tables` narrowing-regression suites to confirm no engine-helper fallout

## 7. Verification

- [x] 7.1 Run `make test-quick` from inside the nix devShell; confirm green
- [x] 7.2 Run `make test` from inside the nix devShell; confirm green
- [x] 7.3 Manual sanity in a live Emacs sandbox: render the proposal's reproducer markdown and confirm pretty-links treats the inline `[path#L…](path#L…)` link as a file link with hidden URL + file icon; `RET` jumps to the right line
