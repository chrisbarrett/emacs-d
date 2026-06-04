## 1. New faces

- [x] 1.1 Add `defface argc-bind-env-face` (inherit `font-lock-variable-name-face`) and `argc-fn-face` (inherit `font-lock-function-name-face`) in `lisp/argc-mode/argc-mode.el`, in the `argc` group
- [x] 1.2 Extend the faces test (or add one) asserting both new faces are defined and inherit the documented built-ins

## 2. Bind-env highlighting ($$ / $NAME)

- [x] 2.1 RED: test that `# @flag -v --verbose $VERBOSE` faces `$VERBOSE` and `# @option --port $$` faces `$$` with `argc-bind-env-face`
- [x] 2.2 GREEN: add a cascade rule matching ` $$` or ` $` + `[A-Z_]+` on `@arg`/`@option`/`@flag`/`@env` lines → `argc-bind-env-face`
- [x] 2.3 Confirm a bare `$` in a directive description is not faced (anchored to the ` $` token form)

## 3. Multi-value modifier delimiter

- [x] 3.1 RED: test that `# @option --tags*,` faces the `*,` run with `argc-modifier-face`
- [x] 3.2 GREEN: widen the `@arg` / `@option` / `@flag` modifier groups so `*` / `+` may carry one optional delimiter from `,:;@|/`, keeping `!` / `~` as single-char alternatives
- [x] 3.3 Add a test with a delimiter and a choice list on one line (`--tags*, [a|b|c]`) proving the delimiter does not swallow the choice `|`

## 4. Backtick function references

- [x] 4.1 RED: test that `` [`_choice_fn`] `` and `` =`_default_fn` `` each face the backtick `` `fn` `` span with `argc-fn-face`
- [x] 4.2 GREEN: add a final (highest-priority) cascade rule re-facing the `` `…` `` span inside a default or choice context → `argc-fn-face`
- [x] 4.3 Confirm the `?` in `` [?`fn`] `` keeps `argc-choice-face` and a prose backtick is untouched

## 5. Activation gate (@version)

- [x] 5.1 RED: test (in `lang-shscript`) that a buffer whose only directive is `# @version 1.0.0` enables `argc-mode` via `+argc-maybe-enable`
- [x] 5.2 GREEN: add `@version` to the `+argc-maybe-enable` trigger regex in `modules/lang-shscript/init.el`

## 6. Regression + sync

- [x] 6.1 Run the full `argc-mode` ERT suite and `lang-shscript` tests; byte-compile both files warning-clean
- [x] 6.2 Reload `argc-mode.el` into the live daemon and rebuild open argc buffers; eyeball bind-env / delimiter / fn highlighting
- [x] 6.3 `openspec sync` the delta into `openspec/specs/argc-mode/spec.md`; record the `@env` name-charset non-change so it is not revisited
- [x] 6.4 `openspec validate argc-mode --type spec`; archive the change
