## ADDED Requirements

### Requirement: Host config alist
`+code-fences.el` SHALL maintain `+code-fences-host-config`, an alist of `(HOST-MODE . PLIST)` entries, where HOST-MODE is a major-mode symbol (e.g. `bash-ts-mode`, `nix-ts-mode`).

#### Scenario: Empty config
- **WHEN** no hosts have registered
- **THEN** `+code-fences-host-config` is nil

#### Scenario: Host registers
- **WHEN** a host calls `(+code-fences-register 'bash-ts-mode :head-valid-p #'fn)`
- **THEN** `(alist-get 'bash-ts-mode +code-fences-host-config)` returns the plist

### Requirement: Host mode resolution
The system SHALL resolve the current host mode from `pm/polymode` via `(oref (oref pm/polymode -hostmode) :mode)` and look up the corresponding entry in `+code-fences-host-config`.

#### Scenario: Known host
- **WHEN** polymode base buffer has host mode `bash-ts-mode` and that mode is registered
- **THEN** dispatch functions use the registered plist callbacks

#### Scenario: Unknown host
- **WHEN** polymode base buffer has a host mode not in the alist
- **THEN** generic rendering (box drawing, dimming) still works; stale detection and interpolation are skipped

### Requirement: Stale detection dispatch
`+polymode-update-head-connectors` SHALL call the registered `:head-valid-p` callback to determine if a head overlay is still valid. If no callback registered, all head overlays are treated as valid.

#### Scenario: Shell head becomes invalid
- **WHEN** user edits a heredoc opener so it no longer matches `<<-?` pattern
- **THEN** `:head-valid-p` returns nil, overlay is removed, redecoration scheduled

#### Scenario: No head-valid-p registered
- **WHEN** host has no `:head-valid-p` callback
- **THEN** head overlays are never removed by stale detection

### Requirement: Unquoted detection dispatch
`+polymode-refontify-inner-spans` SHALL call the registered `:unquoted-p` callback for each head span to determine if the body supports interpolation.

#### Scenario: Quoted shell heredoc
- **WHEN** heredoc head is `<<'DELIM'`
- **THEN** `:unquoted-p` returns nil, no interpolation overlays created

#### Scenario: Unquoted shell heredoc
- **WHEN** heredoc head is `<<DELIM`
- **THEN** `:unquoted-p` returns non-nil, interpolation fn called for body

### Requirement: Interpolation dispatch
When `:unquoted-p` returns non-nil for a body span, the system SHALL call the registered `:interpolation-fn` with (BEG END BASE-BUF) to create interpolation overlays.

#### Scenario: Shell interpolation
- **WHEN** unquoted heredoc body contains `$VAR`
- **THEN** `:interpolation-fn` creates overlay with `+polymode-interpolation-face`

#### Scenario: No interpolation-fn registered
- **WHEN** `:unquoted-p` returns non-nil but no `:interpolation-fn` exists
- **THEN** no interpolation overlays are created (no error)

### Requirement: Face rename
`+polymode-shell-interpolation-face` SHALL be renamed to `+polymode-interpolation-face`. The old name SHALL be preserved as an obsolete alias.

#### Scenario: Old face alias
- **WHEN** user customisation references `+polymode-shell-interpolation-face`
- **THEN** customisation applies to `+polymode-interpolation-face` via alias

### Requirement: No host-specific code in core
`+code-fences.el` SHALL NOT contain any shell heredoc regexes, shell interpolation patterns, or Nix-specific syntax. All such logic MUST reside in the respective lang module.

#### Scenario: Grep for shell patterns
- **WHEN** searching `+code-fences.el` for `<<`, `heredoc`, `\$VAR`, `\${`
- **THEN** no matches found (except in generic comments/docstrings)
