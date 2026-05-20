## 1. Failing Test

- [x] 1.1 Add `lang-markdown/gfm-pretty-callouts-bare-blockquote-body-prefix`
      ert deftest in `lisp/gfm/gfm-pretty-tests.el`. Fixture: a callout source
      containing `> [!IMPORTANT]\n> first\n>\n> second`. Enable
      `gfm-pretty-mode`, then assert (a) a `body-prefix` display overlay
      exists at the bare-`>` line, (b) its `display` property is `"│ "`, and
      (c) it carries `gfm-pretty-callouts-revealable`.
- [x] 1.2 Run `make test-quick` and confirm the new test fails.

## 2. Implementation

- [x] 2.1 In `lisp/gfm/gfm-pretty-callouts.el`, extend the body-prefix branch
      inside `gfm-pretty-callouts--apply-block-display` so a bare-`>` line
      (`lend - lbeg == 1`, `char-after lbeg == ?>`) also emits a body-prefix
      display overlay with `display edge`. Keep the existing `> `-prefix
      branch unchanged. Tag both branches identically
      (`gfm-pretty-callouts-kind 'body-prefix`, revealable, `evaporate t`).
- [x] 2.2 Run `make test-quick` and confirm the new test passes.

## 3. Verification

- [x] 3.1 Run `make test`.
- [x] 3.2 Open
      `/Users/chris/.config/nix-configuration/home/config/programs/agents/skills/bash-scripting/body.md`
      with `gfm-pretty-mode` enabled; confirm no raw `>` is visible inside
      the IMPORTANT callout's blank body row, and the right-edge `│` aligns
      with surrounding rows.
- [x] 3.3 With point on the bare-`>` line in the selected window, confirm
      reveal exposes the raw `>` and the box edge returns when point leaves.
