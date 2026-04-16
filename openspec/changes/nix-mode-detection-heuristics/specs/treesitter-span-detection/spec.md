## MODIFIED Requirements

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
