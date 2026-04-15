## Why

`+code-fences.el` mixes generic overlay rendering (box drawing, dimming, separedit) with shell-specific heredoc knowledge (unquoted detection, `$VAR` interpolation, `<<` stale detection). This prevents reuse for other polymode hosts like Nix, which has its own interpolation syntax (`${...}` with `''$` escaping). Separating the core renderer from host-specific syntax enables each language module to own its parsing while sharing the visual machinery.

## What Changes

- Extract shell-specific functions from `+code-fences.el` into `lang-shscript/`
- Add a registration API (`+code-fences-register`) with an alist (`+code-fences-host-config`) keyed by host major-mode, carrying `:head-valid-p`, `:unquoted-p`, and `:interpolation-fn` callbacks
- Replace all inline shell-specific logic in `+code-fences.el` with dispatch through the host config alist
- Add Nix interpolation highlighting (`${...}`) with `''$` escape handling (even/odd single-quote parity) in `lang-nix/`
- Rename `+polymode-shell-interpolation-face` to `+polymode-interpolation-face`  **BREAKING**

## Capabilities

### New Capabilities
- `code-fences-host-api`: Registration alist and dispatch for host-specific polymode fence callbacks
- `nix-interpolation`: Nix `${...}` interpolation overlay highlighting with `''$` escape detection

### Modified Capabilities

## Impact

- `modules/treesit/config/+code-fences.el` — core refactor, all shell refs removed
- `modules/treesit/config/+code-fences-tests.el` — face rename, possibly new dispatch tests
- `modules/lang-shscript/init.el` — gains extracted shell functions + registration call
- `modules/lang-nix/init.el` — gains interpolation fn + registration call
- Face rename affects any user customisation of `+polymode-shell-interpolation-face`
