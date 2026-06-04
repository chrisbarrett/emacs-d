## Context

`argc-mode` fontifies argc directive comments with an ordered cascade of
regex → face rules in `argc--face-rules` (`lisp/argc-mode/argc-mode.el`).
Each rule is `(REGEXP (GROUP FACE)…)`; later rules create higher-priority
overlays that visually override earlier ones on overlapping ranges, so a
broad rule (e.g. "face the whole `[…]` choice list") can be refined by a
later, narrower rule (e.g. "re-face the inner `` `fn` ``").

Tier 1 (shipped) added `@env` defaults/choices and the `@version` tag. The
authority for what argc accepts is its parser, `sigoden/argc`
`src/parser.rs`. Reading it surfaced three forms we still miss and one
suspected gap that turned out not to be real:

```
parse_bind_env   : preceded(tag(" $"), alt((tag("$"), parse_bind_env_name)))
parse_value_fn   : delimited('`', parse_fn_name, '`')          // =`fn`, [`fn`]
parse_multi_char : one_of(",:;@|/")                            // *,  +:  *|
is_env_name_char : c.is_ascii_uppercase() || c == '_'          // @env NAME charset
```

## Goals / Non-Goals

**Goals:**

- Face the env-binding suffix `$$` / `$NAME` on `@arg`, `@option`,
  `@flag`, `@env`.
- Capture the optional multi-value delimiter char after a `*` / `+`
  modifier into the modifier span.
- Sub-highlight backtick function references inside `` =`fn` `` defaults
  and `` [`fn`] `` / `` [?`fn`] `` choice lists.
- Enable `argc-mode` on a script whose only directive is `@version`.

**Non-Goals:**

- Relaxing the `@env` name charset — argc's `is_env_name_char` is
  uppercase + underscore, which the current `(any upper ?_)` already
  matches exactly. No change; recorded here so it is not revisited.
- Validating that a meta key, choice, or function name is *real* — this
  is fontification, not a linter.
- Any change to box geometry, overlay lifecycle, or the rebuild scheduler.

## Decisions

### Decision: Two new faces, not reuse

Add `argc-bind-env-face` (inheriting `font-lock-variable-name-face`, since
`$NAME` names an environment variable) and `argc-fn-face` (inheriting
`font-lock-function-name-face`, since the backtick names a completion /
default-producing function). Reusing `argc-param-name-face` for bind-env or
`argc-choice-face` for the fn name would conflate distinct concepts and
deny them independent theming — inconsistent with the existing one-face-per-
concept design. This grows the "fixed set of faces" requirement.

### Decision: Bind-env as its own cascade rule

A single rule scans a directive line (`@arg`/`@option`/`@flag`/`@env`) for
` $$` or ` $NAME` and faces the ` $`-led token with `argc-bind-env-face`.
The leading space is required by argc's grammar (`tag(" $")`) and keeps the
rule from matching a bare `$` inside prose. The name charset is `[A-Z_]+`,
mirroring `parse_bind_env_name`. `$$` (anonymous bind) is matched as a
literal alternative.

### Decision: Fold the multi-value delimiter into the modifier group

Today the `@arg` / `@option` / `@flag` modifier groups match a single char.
Replace the multi-value branch so `*` / `+` may be followed by one optional
delimiter from `,:;@|/`, e.g. `(seq (any ?* ?+) (? (any ?, ?: ?\; ?@ ?| ?/)))`,
while `!` and `~` remain single-char alternatives. The delimiter only ever
follows the modifier immediately after the param name, before any space or
bracketed choice list, so it cannot swallow a `|` from a `[a|b|c]` choice.

### Decision: Backtick fn refs are a refinement rule, ordered last

Because the cascade lets later rules win, the existing broad rules can stay:
the choice rule still faces all of `` [`fn`] `` as a choice, and the default
rule still faces `` =`fn` `` as a default — then a final rule re-faces just
the inner `` `…` `` span (including its backticks) with `argc-fn-face`. The
optional `?` in `` [?`fn`] `` stays choice-faced. Anchoring the fn rule to
directive lines avoids matching backticks in ordinary shell comments.

### Decision: `@version` activation gate is code-only

The `+argc-maybe-enable` trigger regex lives in
`modules/lang-shscript/init.el` and is explicitly out of scope for the
`argc-mode` spec (it is composition glue, per that spec's Purpose). Adding
`@version` to its `(or …)` alternation is therefore a tasks-level code edit
with a `lang-shscript` test, not a spec delta.

## Risks / Trade-offs

- **Backtick rule false-matches a literal backtick in a directive
  description** (e.g. `@arg x Use `foo` here`) → Anchor to the
  default/choice context (`=` or `[`-adjacent) rather than any backtick on
  the line, so prose backticks are untouched.
- **Delimiter chars overlap choice pipes** (`|` is both a delimiter and the
  choice separator) → The modifier-delimiter match is positionally bound to
  immediately after the name's `*`/`+`; choice pipes live inside `[…]`. No
  overlap in practice; covered by a test with both on one line.
- **Bind-env ` $` requires a preceding space** → matches argc, but a
  malformed `@option --foo$$` (no space) is left unfaced. Acceptable: argc
  itself would not parse it as a bind-env.

## Migration Plan

Additive only. New faces inherit sensible built-ins; existing buffers gain
extra highlighting on next rebuild. No data migration, no settings change.
Rollback = revert the commit.

## Open Questions

None.
