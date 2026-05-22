## Context

Two GFM minor modes touch RET on heading-anchor links:

- `gfm-pretty-links` (lisp/gfm/gfm-pretty-links.el:614) installs an
  overlay keymap binding `RET` →
  `gfm-pretty-links-follow-link-at-point`. Overlay-attached keymaps
  win over `minor-mode-overriding-map-alist`, so this handler always
  takes priority when point lies on a decorated link.
- `gfm-present-mode` (lisp/gfm/gfm-present.el:560) binds `RET` →
  `gfm-present-follow-link`. Its slug branch (gfm-present.el:244-256)
  correctly widens, narrows to the heading region, and goes to the
  heading position. But it never fires for decorated links because the
  overlay keymap wins.

Pretty-links' actual jump implementation,
`gfm-pretty-links--jump-to-anchor` (gfm-pretty-links.el:580-601),
walks headings from `(point-min)` to `(point-max)` without widening.
Under narrowing (presentation mode or plain `narrow-to-region`),
off-slide headings are unreachable: the handler signals
`user-error "No heading matches anchor: #<slug>"`.

Live repro in a narrowed presentation buffer (slide 1 = positions
1..672; target heading at 2565):

```
emacsclient -e '(gfm-pretty-links--jump-to-anchor "#filter-shape-change")'
→ user-error "No heading matches anchor: #filter-shape-change"
```

## Goals / Non-Goals

**Goals:**

- Anchor links resolve regardless of current narrowing in any buffer
  that has `gfm-pretty-links` decorations.
- In `gfm-present-mode`, RET on an anchor lands point at the target
  heading narrowed to its H1 region (preserves slide framing).
- Modules outside `gfm-pretty-links` can react to a successful anchor
  jump without `gfm-pretty-links` knowing about them.
- Preserve current behaviour for widened buffers.

**Non-Goals:**

- Reworking overlay-keymap precedence between the two modes.
- Changing `gfm-present-follow-link`'s slug branch — it remains the
  fallback for buffers where `gfm-pretty-links` is off or where point
  is on an undecorated link.
- Touching `web` / `file` dispatch in
  `gfm-pretty-links-follow-link-at-point`.

## Decisions

### Decision: widen for the heading scan inside `--jump-to-anchor`

Wrap the heading search in `(save-restriction (widen) ...)` so the scan
covers the whole buffer, then widen the visible buffer before
`goto-char` so point can land at a position outside the prior
narrowing.

Alternatives considered:

- **Search within current narrowing, fall back to widening on miss.**
  Rejected: a heading whose slug also exists inside the narrowed
  region would short-circuit and never reach the widened search. The
  semantics get harder to reason about for a saving of one search
  pass.
- **Leave search bounded; widen only if narrowed and heading missing.**
  Rejected for the same reason: introduces two distinct code paths
  for the same observable outcome.

### Decision: abnormal hook fires post-jump, not via inversion of control

Introduce a public abnormal hook
`gfm-pretty-links-after-anchor-jump-functions`. Each function takes
one argument — the target buffer position — and runs after widen +
`goto-char` finishes. Other modes register a function on this hook to
implement their own post-jump behaviour (e.g. narrow).

Alternatives considered:

- **Reverse coupling: `gfm-pretty-links` checks
  `(bound-and-true-p gfm-present-mode)` and dispatches.** Rejected:
  pretty-links would gain a hard reference to a sibling module. Any
  future consumer (org-narrow-to-subtree, follow-mode, custom modes)
  would require another `cond` arm.
- **`gfm-present-mode` strips the overlay RET binding on enable.**
  Rejected: pretty-links overlays rebuild on edits and theme changes;
  the strip would need to re-run on every rebuild via more hooks,
  and the strip target lives in another module's data structure.
- **`gfm-present-mode` adds a higher-priority overlay with its own RET
  keymap.** Rejected: same lifecycle-tangle problem; the priority
  overlay would need rebuild parity with pretty-links' overlays.

The abnormal hook keeps both modules ignorant of each other. Pretty-
links exposes one extension point; present-mode consumes it.

### Decision: present-mode subscribes via buffer-local hook

In `gfm-present-mode`'s enable branch, add
`gfm-present--narrow-to-heading-at` to
`gfm-pretty-links-after-anchor-jump-functions` with `LOCAL=t`. On
disable, remove it. This matches the existing pattern for
`before-revert-hook` and `after-revert-hook` (gfm-present.el:596-597).

The hook function is `gfm-present--narrow-to-heading-at`, which
already does `(widen)` + `(narrow-to-region ...)` for the H1 region
containing a given position. No new helper needed.

After the hook runs, present-mode also wants
`gfm-present--render-link-previews` so source-range and diff overlays
refresh on the new slide. The hook function can call it directly, or
present-mode can register a second hook function. Choose direct
call inside a thin wrapper to keep the responsibility named:
`gfm-present--after-anchor-jump`.

### Decision: keep `gfm-present-follow-link` slug branch

Two situations leave it useful:

- `gfm-pretty-mode` is off → no overlay keymap → `RET` falls through
  to `gfm-present-mode-map`'s binding → `gfm-present-follow-link`.
- Point sits on a `[label](#slug)` written in a context where the
  links decorator chose not to render (e.g. inside a code block —
  pretty-links skips code regions per gfm-pretty/spec.md:1590).

Both paths must still widen + narrow correctly. The slug branch
already does. Leave it.

## Risks / Trade-offs

- **Risk:** Other code calling `gfm-pretty-links--jump-to-anchor`
  directly may rely on the function leaving narrowing intact.
  → Mitigation: the function is private (double-dash). The only
  public caller is `gfm-pretty-links-follow-link-at-point` (RET
  handler) where widening is the desired UX. Grep confirms no
  external callers in this repo.

- **Risk:** Abnormal hook signature change later breaks present-mode.
  → Mitigation: the signature is one positional arg (target pos);
  documented in the docstring and pinned by a scenario in
  `gfm-pretty/spec.md`. Tests cover the contract.

- **Trade-off:** Hook execution order across multiple subscribers is
  `add-hook` order. Acceptable — only one subscriber expected for the
  foreseeable future. If multiple modes ever subscribe and conflict,
  document priority via `add-hook`'s `DEPTH` argument.

- **Risk:** A widen on jump surprises users of plain narrowed buffers
  who expected narrowing to be preserved on miss.
  → Mitigation: pre-fix behaviour on miss was `user-error` (no
  navigation at all). Post-fix on success the user lands at the
  target heading widened — strictly more useful. The hook lets any
  mode that cares (present-mode does) restore narrowing.
