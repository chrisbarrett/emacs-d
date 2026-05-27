## Context

The user edits `jq` filter scripts (`.jq` files) and frequently runs jq
filters over JSON buffers. With no major mode bound to `.jq`, those
buffers fall through to `fundamental-mode` — no fontification, no
indentation, no interactive filter evaluation. The `jq-mode` package
(ljos/jq-mode, MELPA) supplies a major mode plus an interactive
`jq-interactively` command intended to be invoked from a JSON buffer.

The existing module layout has a thin `modules/lang-<lang>/` per
language family (`lang-nix`, `lang-rust`, `lang-zig`, …). Most of these
do not have a corresponding spec, since they are pure composition glue
around third-party packages. The OpenSpec validator, however, requires
at least one delta per change. This change therefore introduces a small
`lang-jq` axis spec to host the two behaviour requirements being
asserted.

## Goals / Non-Goals

**Goals:**

- Open `.jq` files in `jq-mode`.
- Make `jq-interactively` reachable from JSON buffers under a stable
  binding.
- Install `jq-mode` via elpaca with the rest of the lang modules.

**Non-Goals:**

- Customising `jq-mode`'s fontification or indentation rules.
- Enabling `ob-jq` org-babel integration. (Can be added later if the
  user wants jq source blocks in org documents.)
- LSP / eglot integration for jq (no mature server exists).
- A `tree-sitter` grammar for jq (none used here; relying on
  `jq-mode`'s built-in font-lock).
- Adding `jq-mode` itself to the recognised-axes list in
  `spec-conventions` — only the new spec directory is added.

## Decisions

### Decision: new axis `lang-jq`

Add `openspec/specs/lang-jq/spec.md`. Naming matches the module
directory `modules/lang-jq/` per the axis rule in `spec-conventions`.
The spec is behaviour-facing and covers file association, mode
activation, the package install entry, and the `jq-interactively`
keybinding.

Alternatives considered:

- **Fold into an existing axis.** No existing axis matches — closest is
  `lang-markdown`, which is unrelated. Rejected.
- **Skip the spec.** Matches the lang-nix / lang-rust precedent but the
  OpenSpec validator rejects changes with no deltas. Rejected.
- **Add to `contributor-internals`.** jq is end-user behaviour, not
  contributor tooling. Rejected.

### Decision: use the MELPA `jq-mode` package as-is

No fork, no advice. The package is small and well-scoped; customisation
is unnecessary for the user's workflow.

### Decision: keybinding location for `jq-interactively`

Bind under `json-mode-map` AND `json-ts-mode-map` so the command is
reachable regardless of which JSON major mode is active. The user's
config routes JSON buffers through `json-ts-mode` by default, but
`json-mode` may still be loaded transitively (e.g. via
`jq-mode` itself).

Key choice: `C-c C-j` — matches the EmacsWiki example for `jq-mode` and
does not collide with existing `json-(ts-)mode-map` bindings.

### Decision: file association

Register `\\.jq\\'` in `auto-mode-alist`. `jq-mode` already does this
via its autoload cookie, but declaring it explicitly in the module keeps
the association visible alongside the rest of the module's
configuration and survives load-order surprises.

## Risks / Trade-offs

- **`jq-mode` is third-party and lightly maintained.** → Vendor pin via
  elpaca's lockfile (existing project convention). No further mitigation
  needed for a hobby-grade integration.
- **`C-c C-j` collision.** → Verified at design time against
  `json-mode` (no binding) and `json-ts-mode` (no binding). Future
  upstream changes could collide; the binding is local to those keymaps
  so blast radius is contained.
- **No spec coverage for the package's internals.** → Spec is
  behaviour-facing only. Implementation details of `jq-mode` live
  upstream and are out of scope.
