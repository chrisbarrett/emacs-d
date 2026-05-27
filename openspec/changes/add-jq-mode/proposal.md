## Why

The user works with `jq` filter scripts (`.jq` files) but has no editor
support for them — `.jq` files currently fall through to `fundamental-mode`.
`jq-mode` (ljos/jq-mode, MELPA) provides a major mode for editing jq
scripts plus an interactive `jq-interactively` command that runs filters
live over a JSON buffer.

## What Changes

- Add a new `modules/lang-jq/` module wiring up the third-party `jq-mode`
  package.
- Register `\\.jq\\'` in `auto-mode-alist` so opening a jq script selects
  `jq-mode`.
- Add a `jq-interactively` keybinding under `json-mode-map` /
  `json-ts-mode-map` for live filter evaluation in JSON buffers.
- Add `jq-mode` to the module's `packages.eld` so elpaca installs it.

## Capabilities

### New Capabilities

- `lang-jq`: behaviour-facing axis covering jq script editing — file
  association for `.jq`, third-party `jq-mode` activation, and the
  `jq-interactively` keybinding under JSON major modes for live filter
  evaluation.

### Modified Capabilities

_None._

## Impact

- `modules/lang-jq/init.el` — new, registers `jq-mode` and the
  `jq-interactively` keybinding.
- `modules/lang-jq/packages.eld` — new, declares the `jq-mode` package.
- No changes to existing modules or specs.
