## REMOVED Requirements

### Requirement: Source-range link preview overlay

**Reason:** Preview rendering is now owned by the `link-previews` decorator under `gfm-pretty`.  The decorator fires wherever `gfm-pretty-mode` is enabled (so in plain `gfm-mode` buffers in addition to presentation mode), with the same standalone-link gating, box rendering, and broken-preview sentinel behaviour.

**Migration:** See the new `Source-range link preview overlay rendering` requirement in `openspec/specs/gfm-pretty/spec.md`.  No behavioural change in presentation mode; presentation enables the decorator implicitly via `gfm-pretty-mode`.

### Requirement: Diff-range link preview overlay

**Reason:** As above — diff-range previews are now part of the `link-previews` decorator under `gfm-pretty`.

**Migration:** See the new `Diff-range link preview overlay rendering` requirement in `openspec/specs/gfm-pretty/spec.md`.  The diff-command argv (`git -C WORKTREE diff B...H [-- P]`), LHS-margin rendering, SHA shortening, and sentinel forms are preserved verbatim.

### Requirement: Standalone link gating for preview overlays

**Reason:** The standalone-link gating rule applies to the decorator now, not to a present-mode-private pass.

**Migration:** See the new `Link previews — standalone link gating` requirement in `openspec/specs/gfm-pretty/spec.md`.  The gating regex (`^[[:space:]]*(?:[-*+] |\d+\. |> )?[[:space:]]*$` with the link token removed) is unchanged.

### Requirement: Preview refresh on slide entry

**Reason:** Refresh is now driven by the engine's after-change / window-configuration-change machinery (covered by the new `Link previews refresh on edit and window-configuration change` requirement under `gfm-pretty`).  Present-mode-specific slide-change and anchor-jump hooks still fire — they call into the decorator's rebuild entry point (`(gfm-pretty--rebuild (gfm-pretty--get 'link-previews))`) instead of a present-mode-private function.

**Migration:** See the new `Link previews refresh on edit and window-configuration change` requirement under `gfm-pretty` for the generic rebuild contract.  Present-mode slide-change triggering of that rebuild is implementation detail (no separate spec requirement needed).
