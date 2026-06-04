## Why

`argc-mode`'s fontifier covers the common argc directive forms but misses
several that argc's own parser (`sigoden/argc`, `src/parser.rs`) accepts:
environment bindings (`$$` / `$NAME`), the multi-value delimiter char after
`*` / `+`, and backtick function references inside defaults and choice
lists. These appear in real argc scripts and currently render as plain
prose or with the wrong span, so the box looks inconsistent and the cue
that "this token is special to argc" is lost. Tier 1 (the `@env`
default/choice and `@version` gaps) is already shipped; this closes the
remaining parser-coverage gaps in one pass.

## What Changes

- **Bind-env highlighting**: `$$` and `$NAME` (the env-binding suffix valid
  on `@arg`, `@option`, `@flag`, `@env`) gain a dedicated face. The leading
  ` $` and the uppercase/underscore name are faced as a unit.
- **Multi-value delimiter**: the optional delimiter char (`,`, `:`, `;`,
  `@`, `|`, `/`) that may follow a `*` / `+` modifier is captured into the
  modifier face, so `--tags*,` faces `*,` rather than just `*`.
- **Backtick function references**: function-backed defaults
  (`` =`fn` ``) and function-backed choice lists (`` [`fn`] ``,
  `` [?`fn`] ``) sub-highlight the backtick-delimited function name with a
  dedicated face, distinct from a literal default/choice value.
- **Activation gate**: `@version` is added to the `+argc-maybe-enable`
  trigger regex in `lang-shscript` so a script whose only directive is
  `@version` still enables `argc-mode`.
- **Recorded non-change**: argc restricts `@env` names to uppercase ASCII
  + underscore (`is_env_name_char`), which the current regex already
  matches — the earlier "env names with digits/lowercase" suspicion is
  not a real gap and is documented as such, not implemented.

## Capabilities

### New Capabilities

<!-- none — this extends an existing axis -->

### Modified Capabilities

- `argc-mode`: the per-directive fontification requirement gains bind-env,
  multi-value-delimiter, and backtick-function-reference faces (and two new
  face definitions); the activation requirement adds `@version` to the
  trigger set.

## Impact

- `lisp/argc-mode/argc-mode.el` — two new `defface`s; new / extended entries
  in `argc--face-rules`.
- `lisp/argc-mode/argc-mode-tests.el` — coverage for each new form.
- `modules/lang-shscript/init.el` — `@version` added to the
  `+argc-maybe-enable` directive regex.
- `openspec/specs/argc-mode/spec.md` — delta on the fontification and
  activation requirements.
- No change to box geometry, overlay lifecycle, or existing faces.
