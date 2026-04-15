## 1. Core API

- [ ] 1.1 Add `+code-fences-host-config` alist and `+code-fences-register` function to `+code-fences.el`
- [ ] 1.2 Add `+code-fences--config` helper that resolves host mode from `pm/polymode` and returns the plist
- [ ] 1.3 Rename `+polymode-shell-interpolation-face` to `+polymode-interpolation-face` with obsolete alias

## 2. Extract shell-specific code

- [ ] 2.1 Move `+polymode--heredoc-unquoted-p` to `lang-shscript/init.el` (rename to `+bash--heredoc-unquoted-p`)
- [ ] 2.2 Move `+polymode--add-shell-interpolation-overlays` to `lang-shscript/init.el` (rename to `+bash--add-interpolation-overlays`), update face reference
- [ ] 2.3 Move `+polymode--heredoc-opener-re` and `+polymode--count-heredoc-openers` to `lang-shscript/init.el`, create `+bash--heredoc-head-valid-p`
- [ ] 2.4 Add `+code-fences-register` call in `lang-shscript/init.el` with all three callbacks

## 3. Genericise core dispatch

- [ ] 3.1 Replace `+polymode--heredoc-unquoted-p` call in `+polymode-refontify-inner-spans` with `:unquoted-p` dispatch via `+code-fences--config`
- [ ] 3.2 Replace direct shell interpolation call in `+polymode-refontify-inner-spans` with `:interpolation-fn` dispatch
- [ ] 3.3 Replace heredoc-opener-re validation in `+polymode-update-head-connectors` with `:head-valid-p` dispatch
- [ ] 3.4 Replace shell interpolation recompute in `+polymode--after-change-refontify` with `:interpolation-fn` dispatch

## 4. Nix interpolation

- [ ] 4.1 Write `+nix--add-interpolation-overlays` in `lang-nix/init.el` with `${...}` matching and single-quote parity escape detection
- [ ] 4.2 Write `+nix--multiline-head-valid-p` in `lang-nix/init.el`
- [ ] 4.3 Add `+code-fences-register` call in `lang-nix/init.el` with `:head-valid-p`, `:unquoted-p` (always t), and `:interpolation-fn`

## 5. Tests

- [ ] 5.1 Update existing tests in `+code-fences-tests.el` for face rename (`+polymode-interpolation-face`)
- [ ] 5.2 Add test for unknown host fallback (generic rendering, no interpolation)
- [ ] 5.3 Add Nix interpolation test fixtures and tests: simple `${...}`, escaped `''${...}`, parity edge cases (`'''${...}`, `''''${...}`)
- [ ] 5.4 Run `make test` — all green
