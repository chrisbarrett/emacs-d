# tty Delta

## REMOVED Requirements

### Requirement: theme background is cached for colour arithmetic

**Reason**: Ownership of `+theme-default-background` moves to the
`theme` axis, which owns `+theme-changed-hook` and the rest of
theme-change state. tty was defining another module's seam.

**Migration**: The requirement's full content — including the
depth-ordering constraint that the cache refresh runs before tty's
sentinel clear — is preserved as "theme module owns the
effective-background cache" in the `theme` spec. tty's own
requirements (sentinel clear, cross-stamp prohibition, ornamentation
painting) continue to read `+theme-default-background`; only the
variable's defining module changes.
