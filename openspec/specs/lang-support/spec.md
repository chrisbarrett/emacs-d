# lang-support Specification

## Purpose

Internals-facing. Documents the `+lang` helper library (`lisp/+lang.el`)
through which a language module declaratively wires a major mode to LSP
activation and/or an apheleia formatter, and the requirement that language
modules express such wiring through the helper rather than hand-rolling it.

## Requirements

### Requirement: Declarative language wiring helper

Internals-facing. The `+lang` library (`lisp/+lang.el`) SHALL provide
a helper through which a language module declares, for a major mode:
LSP activation, and/or an apheleia formatter. The helper SHALL be
callable from a module `init.el` (which `require`s `+lang`) without
requiring eglot or apheleia to be loaded at call time.

#### Scenario: declaration is lazy

- **WHEN** a language module declares wiring for a mode at startup
- **THEN** neither `eglot` nor `apheleia` is loaded as a result of the
  declaration alone

### Requirement: LSP activation attaches to the local-vars hook

When a declaration requests LSP, the helper SHALL arrange for
`eglot-ensure` to run from the mode's `<mode>-local-vars-hook` (the
hook that fires after directory-local variables are applied), not from
`<mode>-hook`.

#### Scenario: eglot starts after dir-locals

- **GIVEN** a mode declared with LSP activation
- **WHEN** a buffer enters that mode in a project with dir-locals
- **THEN** `eglot-ensure` runs from the mode's local-vars hook, after
  directory-local variables are in effect

### Requirement: Formatter declarations register definition and association together

When a declaration supplies a formatter, the helper SHALL register the
formatter command in `apheleia-formatters` (when a command spec is
given) and associate the mode in `apheleia-mode-alist`, applying both
once apheleia loads. A declaration naming an existing formatter SHALL
add only the mode association.

#### Scenario: new formatter registered as a pair

- **GIVEN** a declaration with a mode, a formatter name, and a command
- **WHEN** apheleia loads
- **THEN** `apheleia-formatters` contains the named command spec
- **AND** `apheleia-mode-alist` maps the mode to that formatter

#### Scenario: existing formatter reused

- **GIVEN** a declaration naming a formatter already defined by
  apheleia
- **WHEN** apheleia loads
- **THEN** `apheleia-mode-alist` maps the mode to it
- **AND** `apheleia-formatters` is not modified by the declaration

### Requirement: Language modules use the helper

`modules/lang-*/init.el` files SHALL NOT hand-wire
`(<mode>-local-vars-hook . eglot-ensure)` hook entries nor mutate
`apheleia-formatters`/`apheleia-mode-alist` directly; language modules
SHALL express that wiring through the `+lang` helper.

A module whose wiring the helper cannot express (a genuine variation,
e.g. associating a mode with a formatter *chain* rather than a single
formatter) MAY keep that wiring hand-rolled, provided the exception is
documented at the enforcement point.

#### Scenario: no hand-rolled wiring remains

- **WHEN** `modules/lang-*/init.el` files are searched for
  `eglot-ensure` hook pairs and direct
  `apheleia-formatters`/`apheleia-mode-alist` mutation
- **THEN** no match remains in `modules/lang-*/init.el` apart from the
  documented exceptions (the helper itself lives in `lisp/+lang.el`,
  outside the searched module tree)
