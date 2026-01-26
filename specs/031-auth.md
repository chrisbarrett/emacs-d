# auth

1Password integration for Emacs credential management via auth-source.

## Files

| File             | Purpose                          |
| :--------------- | :------------------------------- |
| init/init-auth.el | auth-source-op backend configuration |

## External Packages

| Package        | Purpose                              |
| :------------- | :----------------------------------- |
| auth-source-op | 1Password backend for auth-source API |

## Behavior

### Auth Sources

| Setting        | Value         | Effect                              |
| :------------- | :------------ | :---------------------------------- |
| auth-sources   | `'(1password)` | Use 1Password as sole auth backend  |
| auth-source-op-vaults | `'("Emacs")` | Restrict to Emacs vault only     |

### Integration Points

The auth-source-op backend enables credential retrieval for:

- **Forge**: GitHub/GitLab authentication tokens
- **TRAMP**: Remote host credentials
- **SMTP**: Email sending credentials
- **Any auth-source consumer**: Generic secret lookup via `auth-source-search`

### 1Password CLI Requirements

The `op` CLI must be:
1. Installed and available in PATH
2. Authenticated (via `op signin` or biometric)
3. Have an "Emacs" vault with credentials stored as Login items

### Credential Lookup

Credentials are matched by:
- `:host` - The website/URL field in 1Password
- `:user` - The username field (optional)
- `:port` - Protocol/service (optional)

## API

### Variables

| Variable               | Default        | Purpose                        |
| :--------------------- | :------------- | :----------------------------- |
| auth-sources           | `'(1password)` | Backend list for auth-source   |
| auth-source-op-vaults  | `'("Emacs")`   | 1Password vaults to search     |

### Functions

| Function               | Purpose                           |
| :--------------------- | :-------------------------------- |
| auth-source-op-enable  | Register 1Password backend        |

## Testable Properties

1. `auth-sources` contains `1password` symbol
2. `auth-source-op-vaults` set to `'("Emacs")`
3. `auth-source-op-enable` called during init (backend registered)
4. `auth-source-search` can retrieve credentials when op CLI available
5. Package loads after `auth-source` feature

## Design Decisions

### Single Vault Restriction

Using a dedicated "Emacs" vault:
- Prevents accidental exposure of non-Emacs credentials
- Allows clear separation of machine-accessible secrets
- Simplifies credential management in 1Password

### auth-source as Backend

Using the built-in auth-source API rather than direct op calls:
- Leverages existing Emacs credential infrastructure
- No changes needed to consuming packages (Forge, TRAMP)
- Follows Emacs conventions for credential management
