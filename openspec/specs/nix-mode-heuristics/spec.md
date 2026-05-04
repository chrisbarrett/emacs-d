## ADDED Requirements

### Requirement: Comment fboundp probe fallback
When a comment annotation (e.g. `# may-i-config`) does not match any entry in
`+nix-lang-mode-alist`, the scanner SHALL try `(intern (concat COMMENT "-mode"))`
and accept the symbol if `fboundp` returns non-nil.

#### Scenario: Direct mode name in comment
- **GIVEN** a Nix file containing `extraConfig = \n  # may-i-config\n  ''...''`
- **WHEN** `may-i-config` is not in `+nix-lang-mode-alist` but `may-i-config-mode` is `fboundp`
- **THEN** a span is cached with mode `may-i-config-mode`

#### Scenario: Alist takes priority over probe
- **GIVEN** a Nix file containing `script = \n  # bash\n  ''...''`
- **WHEN** `bash` maps to `bash-ts-mode` in `+nix-lang-mode-alist` and `bash-mode` is also `fboundp`
- **THEN** a span is cached with mode `bash-ts-mode` (alist wins)

#### Scenario: Neither alist nor probe matches
- **GIVEN** a Nix file containing `data = \n  # nonexistent-xyz\n  ''...''`
- **WHEN** `nonexistent-xyz` is not in the alist and `nonexistent-xyz-mode` is not `fboundp`
- **THEN** no span is cached for that binding (falls through to attrpath matching)

### Requirement: Full attrpath regexp matching
The scanner SHALL maintain `+nix-attrpath-mode-alist`, an alist of
`(REGEXP . MODE-SYMBOL)` entries. For each binding with an
`indented_string_expression` and no comment-resolved mode, the full
dotted attrpath string SHALL be matched against each regexp in order.
First match wins.

#### Scenario: Leaf attr matches bash pattern
- **GIVEN** `shellHook = ''...'';` with no comment annotation
- **WHEN** `+nix-attrpath-mode-alist` contains a regexp matching `shellHook` at end of string
- **THEN** a span is cached with mode `bash-ts-mode`

#### Scenario: Full attrpath matches domain pattern
- **GIVEN** `+nix-attrpath-mode-alist` contains `("programs\\.may-i\\." . may-i-config-mode)`
- **WHEN** a binding has full attrpath `programs.may-i.extraConfig` and no comment annotation
- **THEN** a span is cached with mode `may-i-config-mode`

#### Scenario: No regexp matches
- **GIVEN** `description = ''Just text'';` with no comment
- **WHEN** no regexp in `+nix-attrpath-mode-alist` matches `description`
- **THEN** no span is cached for that binding

#### Scenario: Comment annotation overrides attrpath
- **GIVEN** `shellHook = \n  # python\n  ''...''`
- **WHEN** `shellHook` would match bash via attrpath alist and `python` resolves via lang-mode alist
- **THEN** comment wins — span cached with `python-ts-mode`

### Requirement: Full attrpath reconstruction via tree-sitter parent walk
The scanner SHALL reconstruct full dotted attrpaths by walking up the
tree-sitter AST from each binding node, collecting attrpath text from ancestor
binding nodes. This SHALL produce identical results for flat
(`programs.may-i.extraConfig = ...`) and nested
(`programs.may-i = { extraConfig = ...; }`) styles.

#### Scenario: Flat attrpath
- **GIVEN** `programs.may-i.extraConfig = ''...'';`
- **WHEN** the scanner extracts the attrpath
- **THEN** the full attrpath is `programs.may-i.extraConfig`

#### Scenario: Nested attrpath
- **GIVEN** `programs.may-i = { extraConfig = ''...''; };`
- **WHEN** the scanner extracts the attrpath
- **THEN** the full attrpath is `programs.may-i.extraConfig`

#### Scenario: Let-binding (no nesting)
- **GIVEN** `let myScript = ''...''; in myScript`
- **WHEN** the scanner extracts the attrpath
- **THEN** the full attrpath is `myScript`

#### Scenario: Deeply nested attrpath
- **GIVEN** `services = { nginx = { virtualHosts."example.com" = { locations."/" = { extraConfig = ''...''; }; }; }; };`
- **WHEN** the scanner extracts the attrpath
- **THEN** the full attrpath is `services.nginx.virtualHosts."example.com".locations."/".extraConfig`
