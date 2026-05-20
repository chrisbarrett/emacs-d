## ADDED Requirements

### Requirement: `may-i-config-mode` lives in a self-contained library

The `may-i` library SHALL live at `lisp/may-i/may-i.el` and provide
the symbol `may-i`. The library SHALL define `may-i-config-mode`, a
major mode derived from `lisp-data-mode`, plus the faces, font-lock
matchers, formatter wiring, indent rules, and imenu function the
mode needs. No other module SHALL contain may-i-specific code.

#### Scenario: Library file exists and provides may-i

- **GIVEN** the working tree at HEAD
- **WHEN** `lisp/may-i/may-i.el` is loaded
- **THEN** `(featurep 'may-i)` SHALL be non-nil
- **AND** `(fboundp 'may-i-config-mode)` SHALL be non-nil
- **AND** no source file other than `lisp/may-i/may-i.el` and its
  test file SHALL contain `may-i-` symbol definitions

### Requirement: `may-i-config-mode` activates on may-i config paths

The library SHALL register `may-i-config-mode` in `auto-mode-alist`
for files matching `/may-i/<any>.lisp` and for files named
`.may-i.lisp` or `.may-i.local.lisp` (with or without preceding path
components). Registration SHALL be `;;;###autoload`-cookied so it
survives autoload harvesting and applies at startup without an
explicit `require`.

#### Scenario: Files under a `may-i/` directory activate the mode

- **GIVEN** a file path `/tmp/proj/may-i/foo.lisp`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `may-i-config-mode`

#### Scenario: Dotted may-i config files activate the mode

- **GIVEN** a file path ending in `.may-i.lisp` or `.may-i.local.lisp`
- **WHEN** the file is visited and `set-auto-mode` runs
- **THEN** the buffer's major mode SHALL be `may-i-config-mode`

#### Scenario: `lisp-data-mode` features remain available

- **GIVEN** a buffer in `may-i-config-mode`
- **WHEN** `derived-mode-p 'lisp-data-mode` is called
- **THEN** the result SHALL be non-nil

### Requirement: Font-lock highlights may-i surface tokens

The library SHALL apply font-lock to may-i configs covering the
following surface tokens:

- **Def heads** (`define-arg-style`, `define`, `parser`, `rule`) — head
  carries `font-lock-keyword-face`; the defined name (the symbol
  immediately following the head, when present) carries
  `font-lock-function-name-face`.
- **Top-level forms** (`check`, `load`, `safe-env-vars`) — head
  carries `font-lock-keyword-face`.
- **Rule argument strings** — for `(rule …)`, the first argument is
  either a bare string or an `(or "x" "y" …)` form; each string
  literal in that position carries `font-lock-function-name-face`.
- **Control / logical heads** (`and`, `cond`, `if`, `not`, `or`,
  `unless`, `when`, `with-facts`) — head carries
  `font-lock-keyword-face`.
- **Decision verbs** — `allow` carries `may-i-allow-face`, `ask`
  carries `may-i-ask-face`, `deny` carries `may-i-deny-face`. Inside
  `(check …)` forms (claims, not decisions) the same verbs SHALL
  retain their colour but SHALL drop bold weight.
- **Reason strings** — the string literal immediately following a
  `(allow|ask|deny …)` head carries `may-i-reason-face`, EXCEPT
  inside `(check …)` forms where the string is a command-line under
  test.
- **Built-ins** (`authorise`, `regex`) — head carries
  `font-lock-builtin-face`.
- **Parser / arg-style attribute heads** (`after`,
  `combined-shorts`, `first-token-bundle`, `long-prefix`,
  `many-till`, `overrides`, `pun`, `separators`, `short-prefix`,
  `style`) — head carries `font-lock-type-face`.
- **Style names** (`gnu`, `key-value`, `legacy-bundle`,
  `single-dash-long`) — bare symbol carries `font-lock-constant-face`.
- **Constants** (`else`, `*`) — bare symbol carries
  `font-lock-constant-face`.
- **Fact keywords** — any symbol of the form `:<word>` (including
  `:env`, `:via`, `:ssh/host`) carries `font-lock-constant-face`.

The faces `may-i-allow-face`, `may-i-ask-face`, `may-i-deny-face`,
and `may-i-reason-face` SHALL be defined as `defface` forms in the
`may-i` group.

#### Scenario: Decision verb outside `check` is bold

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(allow "do thing")`
- **WHEN** `font-lock-ensure` runs
- **THEN** the `allow` token SHALL carry `may-i-allow-face`
- **AND** the string `"do thing"` SHALL carry `may-i-reason-face`

#### Scenario: Decision verb inside `check` drops bold weight

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(check (allow "do thing"))`
- **WHEN** `font-lock-ensure` runs
- **THEN** the inner `allow` token SHALL carry `may-i-allow-face`
- **AND** the inner token SHALL carry an override that sets
  `:weight normal`
- **AND** the inner string SHALL NOT carry `may-i-reason-face`

#### Scenario: Rule with `or` highlights every alternative

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(rule (or "foo" "bar") …)`
- **WHEN** `font-lock-ensure` runs
- **THEN** both `"foo"` and `"bar"` SHALL carry
  `font-lock-function-name-face`

### Requirement: Apheleia formatter wiring is registered when apheleia loads

After `apheleia` is loaded, the library SHALL add an entry
`(may-i . ("may-i" "fmt" "-"))` to `apheleia-formatters` and an entry
`(may-i-config-mode . may-i)` to `apheleia-mode-alist`. Registration
SHALL be guarded by `with-eval-after-load 'apheleia` so the library
does not error when apheleia is absent.

#### Scenario: Formatter is registered post-apheleia-load

- **GIVEN** a session where the library is loaded and `apheleia` is
  subsequently loaded
- **WHEN** `apheleia-mode-alist` is queried for `may-i-config-mode`
- **THEN** the result SHALL be `may-i`
- **AND** `apheleia-formatters` SHALL contain
  `(may-i . ("may-i" "fmt" "-"))`

### Requirement: may-i indent rules are scoped to the library

The library SHALL install `lisp-indent-function` properties for the
heads `define`, `define-arg-style`, `parser`, `rule`, `when`,
`unless`, `with-facts`, `cond`, and `defcontext` so that may-i
configs indent consistently. The same library SHALL NOT install
indent rules unrelated to may-i; conversely, no other module SHALL
install may-i-specific indent rules.

#### Scenario: `rule` form indents its body by one column

- **GIVEN** the library is loaded
- **WHEN** `(get 'rule 'lisp-indent-function)` is evaluated
- **THEN** the result SHALL be `1`

#### Scenario: `with-facts` indent put lives in `may-i.el`, not `lang-lisp/init.el`

- **GIVEN** the working tree at HEAD
- **WHEN** `modules/lang-lisp/init.el` is searched for `with-facts`
  and `defcontext`
- **THEN** neither symbol SHALL appear there
- **AND** both SHALL appear with `lisp-indent-function` properties
  set in `lisp/may-i/may-i.el`

### Requirement: Imenu surfaces may-i structural anchors

`may-i-config-mode` SHALL install an `imenu-create-index-function`
whose result is a nested alist with the following sections, each
present only when the buffer contains at least one matching form:
`Rules`, `Parsers`, `Arg styles`, `Definitions`, `Checks`, `Loads`,
`Safe env vars`. Section labels SHALL appear in this order.

Section contents:

- **Rules**: one entry per named `(rule …)`. For `(rule "name" …)`
  the entry is keyed by the literal string. For
  `(rule (or "a" "b" …) …)` each `or` alternative that is a string
  literal SHALL produce its own entry, all pointing to the same
  `(rule` open-paren position.
- **Parsers**: one entry per `(parser <name> …)` keyed by the
  symbol name.
- **Arg styles**: one entry per `(define-arg-style <name> …)` keyed
  by the symbol name.
- **Definitions**: one entry per `(define <name> …)` keyed by the
  symbol name.
- **Checks**: one entry per `(check …)`. When the first argument is
  a string literal, the entry is keyed by that string; otherwise it
  is keyed by `check (N)` where N is the 1-based index of this
  check among all checks in the buffer.
- **Loads**: one entry per `(load <path>)` keyed by the path string
  literal.
- **Safe env vars**: one entry per `(safe-env-vars …)` keyed by the
  literal text `safe-env-vars` (positional; per-arg breakdown is
  out of scope).

Each entry SHALL point to the buffer position of the open
parenthesis of its enclosing form. Forms that appear inside string
literals or comments SHALL NOT produce entries.

#### Scenario: Named rule appears under Rules

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(rule "foo" (allow "x"))`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** the result SHALL contain a `Rules` section
- **AND** that section SHALL contain an entry keyed `"foo"` pointing
  at the `(rule` open paren

#### Scenario: `(rule (or "a" "b"))` produces one entry per alternative

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(rule (or "a" "b") (allow "x"))`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** the `Rules` section SHALL contain two entries keyed `"a"`
  and `"b"`
- **AND** both SHALL point at the same `(rule` open paren

#### Scenario: Parsers, arg-styles, and definitions appear under their sections

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(parser p1 …)`, `(define-arg-style s1 …)`, and `(define d1 …)`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** `Parsers` SHALL contain `p1`
- **AND** `Arg styles` SHALL contain `s1`
- **AND** `Definitions` SHALL contain `d1`

#### Scenario: Anonymous check uses positional label

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(check (allow "x"))` as the only `(check …)` form
- **WHEN** `imenu--make-index-alist` is called
- **THEN** the `Checks` section SHALL contain an entry keyed
  `"check (1)"`

#### Scenario: Loads and safe-env-vars surface

- **GIVEN** a buffer in `may-i-config-mode` containing
  `(load "shared.lisp")` and `(safe-env-vars "FOO" "BAR")`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** `Loads` SHALL contain `"shared.lisp"`
- **AND** `Safe env vars` SHALL contain `safe-env-vars`

#### Scenario: Forms inside strings or comments do not produce entries

- **GIVEN** a buffer in `may-i-config-mode` containing only the
  string literal `"(rule \"fake\" (allow \"x\"))"` and the comment
  `;; (parser fake …)`
- **WHEN** `imenu--make-index-alist` is called
- **THEN** the result SHALL contain no `Rules` or `Parsers` section
