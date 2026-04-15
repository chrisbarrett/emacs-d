## ADDED Requirements

### Requirement: Nix interpolation highlighting
`lang-nix` SHALL register an `:interpolation-fn` that creates overlays for `${...}` interpolation expressions in Nix multiline string bodies.

#### Scenario: Simple interpolation
- **WHEN** Nix multiline string body contains `${pkgs.hello}`
- **THEN** overlay with `+polymode-interpolation-face` spans from `$` to closing `}`

#### Scenario: Nested braces
- **WHEN** body contains `${if foo then "bar" else "baz"}`
- **THEN** overlay spans the entire `${...}` expression including nested content

### Requirement: Nix escape detection
The interpolation function SHALL skip `${` sequences that are escaped with `''$`. Escape detection uses single-quote parity: count consecutive `'` characters immediately before `$`. Even count means escaped; odd count means real interpolation.

#### Scenario: Escaped interpolation
- **WHEN** body contains `''${notInterpolated}`
- **THEN** no interpolation overlay is created (2 quotes → even → escaped)

#### Scenario: Escaped quote then real interpolation
- **WHEN** body contains `'''${realInterp}`
- **THEN** interpolation overlay IS created (3 quotes → odd → real)

#### Scenario: Double-escaped then escaped interpolation
- **WHEN** body contains `''''${escaped}`
- **THEN** no interpolation overlay is created (4 quotes → even → escaped)

#### Scenario: No preceding quotes
- **WHEN** body contains `${normalInterp}` with no preceding quotes
- **THEN** interpolation overlay is created (0 quotes → even... wait, 0 is even but means not escaped)

### Requirement: Nix unquoted-p always true
Nix multiline strings always support interpolation. The `:unquoted-p` callback SHALL always return non-nil.

#### Scenario: Any Nix multiline string
- **WHEN** polymode detects a Nix `/* bash */ ''...''` body span
- **THEN** `:unquoted-p` returns non-nil, interpolation fn is called

### Requirement: Nix head validation
`lang-nix` SHALL register a `:head-valid-p` callback that validates head overlays still match the Nix multiline string annotation pattern (`/* lang */` or `# lang` followed by `''`).

#### Scenario: Valid head
- **WHEN** head overlay start matches `/* bash */ ''` or `# bash\n  ''`
- **THEN** `:head-valid-p` returns non-nil

#### Scenario: Edited away
- **WHEN** user deletes the `/* bash */` annotation
- **THEN** `:head-valid-p` returns nil, stale overlay removed
