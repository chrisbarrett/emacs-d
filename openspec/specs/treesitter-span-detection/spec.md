### Requirement: Annotation-based language detection via tree-sitter
The system SHALL detect embedded language from comment children of binding nodes
in the Nix AST. Both `/* lang */` block comments and `# lang` line comments
SHALL be recognized. Resolution order: `+nix-lang-mode-alist` lookup first,
then `fboundp` probe of `(intern (concat COMMENT "-mode"))`.

#### Scenario: Block comment annotation
- **GIVEN** a Nix file containing `buildPhase = /* bash */ ''...''`
- **WHEN** tree-sitter span scanning runs
- **THEN** a span is cached with mode `bash-ts-mode`, head from comment start through opening `''`

#### Scenario: Line comment annotation
- **GIVEN** a Nix file containing `configurePhase =\n  # python\n  ''...''`
- **WHEN** tree-sitter span scanning runs
- **THEN** a span is cached with mode `python-ts-mode`

#### Scenario: Unknown annotation with fboundp fallback
- **GIVEN** a Nix file containing `extraConfig =\n  # may-i-config\n  ''...''`
- **WHEN** `may-i-config` is not in `+nix-lang-mode-alist` but `may-i-config-mode` is `fboundp`
- **THEN** a span is cached with mode `may-i-config-mode`

#### Scenario: Unknown annotation, no fboundp match
- **GIVEN** a Nix file containing `foo = /* nonexistent */ ''...''`
- **WHEN** `nonexistent` is not in the alist and `nonexistent-mode` is not `fboundp`
- **THEN** no span from annotation; falls through to attrpath matching

### Requirement: Attribute-name-based language detection
The system SHALL detect mode for `indented_string_expression` values by matching
the full reconstructed attrpath against `+nix-attrpath-mode-alist`, an alist of
`(REGEXP . MODE-SYMBOL)`. This replaces the flat `+nix-bash-attrs` list.

#### Scenario: shellHook without annotation
- **GIVEN** `shellHook = ''...'';` with no comment annotation
- **WHEN** tree-sitter span scanning runs
- **THEN** a span is cached with mode `bash-ts-mode`, head from `shellHook` identifier through opening `''`

#### Scenario: buildPhase without annotation
- **GIVEN** `buildPhase = ''...'';`
- **WHEN** tree-sitter span scanning runs
- **THEN** a span is cached with mode `bash-ts-mode`

#### Scenario: Nested attrpath
- **GIVEN** `packages.x86.default.installPhase = ''...'';`
- **WHEN** tree-sitter span scanning runs
- **THEN** `installPhase` matches at end of attrpath, span cached with mode `bash-ts-mode`

#### Scenario: Unknown attribute, no annotation
- **GIVEN** `description = ''Just text'';` where no regexp matches `description`
- **WHEN** tree-sitter span scanning runs
- **THEN** no span is cached for that binding

#### Scenario: Annotation overrides attr-name
- **GIVEN** `shellHook = /* python */ ''...'';`
- **WHEN** tree-sitter span scanning runs
- **THEN** annotation wins — span cached with mode `python-ts-mode`, not `bash-ts-mode`

### Requirement: Span tuple includes attrpath
Each cached span SHALL be a 6-tuple:
`(HEAD-BEG HEAD-END TAIL-BEG TAIL-END MODE-SYMBOL ATTRPATH-STRING)`.

#### Scenario: Attrpath stored in cache
- **GIVEN** `programs.may-i.extraConfig = ''...'';`
- **WHEN** span scanning caches this binding
- **THEN** the 6th element of the span tuple is `"programs.may-i.extraConfig"`

### Requirement: Eager span caching
Span data SHALL be cached in a buffer-local sorted list and rebuilt when the
tree-sitter parser notifies of changes.

#### Scenario: Initial buffer open
- **WHEN** `poly-nix-ts-mode` activates
- **THEN** `+nix-ts--cached-spans` is populated with all detected spans

#### Scenario: Buffer edit adds new annotated string
- **GIVEN** user inserts `checkPhase = ''...'';`
- **WHEN** tree-sitter parse notification fires
- **THEN** cache is rebuilt, new span appears in `+nix-ts--cached-spans`

### Requirement: Polymode function matchers
`:head-matcher`, `:tail-matcher`, and `:mode-matcher` SHALL be functions that
read from the cached span list.

#### Scenario: Forward head search
- **GIVEN** point is before a cached span's head-beg
- **WHEN** polymode calls `(head-matcher 1)` (search forward)
- **THEN** returns `(HEAD-BEG . HEAD-END)` of the next span

#### Scenario: Backward head search
- **GIVEN** point is after a cached span's head-beg
- **WHEN** polymode calls `(head-matcher -1)` (search backward)
- **THEN** returns `(HEAD-BEG . HEAD-END)` of the preceding span

#### Scenario: Mode-matcher at head position
- **GIVEN** point is at a span's head-beg
- **WHEN** polymode calls `(mode-matcher)`
- **THEN** returns mode name string (e.g. "bash-ts") for `pm-get-mode-symbol-from-name`

### Requirement: Head boundary includes context
The head span SHALL extend from the annotation comment start (if present) or
attr identifier start (for attr-name matches) through the opening `''` of the
`indented_string_expression`.

#### Scenario: Annotated string head
- **GIVEN** `foo = /* bash */ ''...''` where `/* bash */` starts at col 6
- **THEN** head-beg = start of `/* bash */`, head-end = 2 chars into `indented_string_expression`

#### Scenario: Attr-name string head
- **GIVEN** `shellHook = ''...''` where `shellHook` starts at col 4
- **THEN** head-beg = start of `shellHook`, head-end = 2 chars into `indented_string_expression`

### Requirement: Code-fences integration preserved
The `+code-fences-register` call for `nix-ts-mode` SHALL continue to provide
`:head-valid-p`, `:unquoted-p`, and `:interpolation-fn` callbacks.

#### Scenario: Head validation with tree-sitter
- **WHEN** `+nix--multiline-head-valid-p` is called for a head overlay
- **THEN** it checks tree-sitter AST (not regex) for binding validity

#### Scenario: Count-openers via cache
- **WHEN** `:count-openers` is called
- **THEN** returns length of `+nix-ts--cached-spans`
