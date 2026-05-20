## MODIFIED Requirements

### Requirement: One spec per axis; spec name matches lib or module name

Every spec under `openspec/specs/` SHALL cover exactly one configuration
axis. An axis is a self-contained config domain. The spec directory
name SHALL equal the name of the corresponding source directory.

An axis SHALL correspond to ONE of:

- A **library** at `lisp/<family>/<lib>.el` (or `lisp/<lib>.el`) —
  self-contained elisp code with no composition concerns of its own
  (no `init.el` to run at config load, no global hooks to install).
  Spec name = lib base name (e.g. `gfm-pretty` for
  `lisp/gfm/gfm-pretty.el`).
- A **module** at `modules/<name>/` — composition unit that wires
  libraries into the running config via `init.el`, mode hooks,
  `auto-mode-alist`, formatter registration, leader bindings, etc.
  Spec name = module directory name.

A spec that crosses multiple axes SHALL be split at the axis boundary
before merge; a spec that sub-divides a single axis SHALL be folded
into the axis spec.

The recognised axes are:

- **lang-markdown** (module) — `gfm-mode` / `markdown-mode` integration:
  file associations, `major-mode-remap-alist`,
  `markdown-code-lang-modes`, formatters, `+markdown-tab-dwim`,
  `markdown-get-lang-mode` memoise advice,
  `markdown-syntax-propertize-extend-region` clamp advice, and the
  `gfm-mode-hook` that loads and enables `gfm-pretty-mode`. Does NOT
  cover overlay-based visual decoration — see `gfm-pretty`.
- **gfm-pretty** (lib) — `lisp/gfm/gfm-pretty.el` and its impl
  files. Overlay-based and font-lock-based visual decoration of GFM
  buffers: umbrella minor mode, decorator extension protocol,
  rendering primitives, callouts / code fences / tables / hrules /
  links decorators, narrowing safety, rebuild scheduling, per-window
  sizing.
- **gfm-present** (lib) — `lisp/gfm/gfm-present.el`. Buffer-local
  slide-walkthrough mode over markdown documents: heading-narrowed
  slides, navigation, source-range and diff-range link previews,
  click actions. Declares load-order dep on `gfm-pretty`.
- **bats-mode** (lib) — `lisp/bats-mode/bats-mode.el`. Major mode
  derived from `bash-ts-mode` for `.bats` test files: activation on
  the `.bats` extension and the `bats` interpreter, layered regex
  font-lock for bats directives and helper-library assertions,
  profile-gated assertion fontification, `$BATS_*` variable
  highlighting, and an imenu composing Tests, Fixtures, and inherited
  bash functions.
- **argc-mode** (lib) — `lisp/argc-mode/argc-mode.el`. Minor mode
  for [argc](https://github.com/sigoden/argc) CLI directive
  fontification in shell-script comments: face overlays for the
  `@cmd`/`@arg`/`@option`/`@flag`/`@env`/`@meta`/`@alias`/`@describe`
  directives, Unicode box overlays around directive blocks with a
  function-name label, `spell-fu` advice that skips directive
  lines, and a debounced rebuild scheduler that skips indirect
  buffers. Does NOT cover the activation gate
  `+argc-maybe-enable` — that lives in `lang-shscript` as
  composition glue.
- **dune-mode** (lib) — `lisp/dune-mode/dune-mode.el`. Major mode
  derived from `lisp-data-mode` for Dune build configuration files
  (`dune`, `dune-workspace`, `dune-project`): mode-line label,
  `comment-add` set to 0, and the path-pattern `auto-mode-alist`
  registration.
- **opam-mode** (lib) — `lisp/opam-mode/opam-mode.el`. Major mode
  derived from `conf-colon-mode` for OPAM package configuration
  files (`*.opam`): mode-line label and the extension-based
  `auto-mode-alist` registration. The read-only-on-generated-files
  hook is composition policy and lives in
  `modules/lang-ocaml/init.el`.
- **may-i** (lib) — `lisp/may-i/may-i.el`. Major mode
  (`may-i-config-mode`) derived from `lisp-data-mode` for `may-i`
  config files: activation on `/may-i/<any>.lisp` and
  `.may-i(.local)?.lisp`, faces for decision verbs and reason
  strings, font-lock for def heads / top-level forms / parser
  attributes / style names / fact keywords, apheleia formatter
  wiring, may-i-scoped indent rules, and an imenu composing Rules,
  Parsers, Arg styles, Definitions, Checks, Loads, and Safe env vars.
- **eglot** (module) — LSP integration, diagnostics.
- **evil** (module) — vi emulation, keybindings.
- **org** (module) — org-mode, agenda, capture.
- **vcs** (module) — git / magit integration.
- **completion** (module) — completion-at-point, corfu, cape.
- **theme** (module) — visual theming, faces, colours.
- **contributor-internals** (meta) — spec conventions, test
  infrastructure, code-quality rules; not user-observable behaviour.

A new axis SHALL be justified in the spec's Purpose before merge; no
silent expansion of this list.

#### Scenario: Three GFM sub-specs fold into gfm-pretty

- **GIVEN** specs `gfm-callout-rendering`, `gfm-code-fence-rendering`,
  `gfm-table-rendering` exist under `openspec/specs/`
- **WHEN** the one-spec-per-axis rule is applied
- **THEN** all three SHALL be consolidated into `gfm-pretty/spec.md`
- **AND** the three directories SHALL be removed

#### Scenario: New spec for a recognised axis uses the source name

- **GIVEN** a proposal adding a spec for git integration
- **WHEN** the bucket assignment is reviewed
- **THEN** the spec SHALL be filed as `openspec/specs/vcs/spec.md`
- **AND** no new axis SHALL be created

#### Scenario: New library spec proposes a lib axis

- **GIVEN** a proposal adding a substantial, self-contained elisp
  library
- **WHEN** placement is decided
- **THEN** the library SHALL be filed at `lisp/<family>/<lib>.el`
- **AND** the spec at `openspec/specs/<lib>/spec.md`
- **AND** the recognised-axes list SHALL gain a `<lib> (lib)` entry

#### Scenario: New module spec proposes a module axis

- **GIVEN** a proposal adding a composition unit that wires existing
  libraries into the config
- **WHEN** placement is decided
- **THEN** the module SHALL be filed at `modules/<name>/`
- **AND** the spec at `openspec/specs/<name>/spec.md`
- **AND** the recognised-axes list SHALL gain a `<name> (module)`
  entry

#### Scenario: New axis requires Decision in design.md

- **GIVEN** a proposal adding a spec whose coverage does not fit any
  recognised axis
- **WHEN** the change is opened
- **THEN** the spec's Purpose section SHALL explain why no existing
  axis covers it
- **AND** the design.md SHALL include a "Decision: new axis X" entry

#### Scenario: Visual-behaviour spec uses gfm-pretty, not lang-markdown

- **GIVEN** a proposal adding requirements for a new overlay decorator
  on markdown buffers
- **WHEN** the bucket assignment is reviewed
- **THEN** the requirement SHALL be filed under
  `openspec/specs/gfm-pretty/spec.md`
- **AND** `openspec/specs/lang-markdown/spec.md` SHALL NOT be the
  target
