## Why

After `tighten-gfm-pretty-protocol` landed, the engine-level requirements in
`openspec/specs/gfm-pretty/spec.md` were rewritten to the narrowed protocol
(`:apply-block-fn`, `:full-rebuild-required-p`, registry-derived prop names,
naming-convention block/edit-at-point) but the decorator-specific requirements
in the same spec (Callouts / Fences / Tables / Hrule / Links / public
introspection / engine-owned generic rebuild stats / debounced rebuild
scheduler) still reference the dropped keys (`:apply-anchors`, `:apply-display`,
`:structural-line-ranges`, `:edit-adjacency`, `:revealable-prop`,
`:saved-display-prop`, `:reconcile-windows`, `:block-at-point`,
`:edit-at-point`). The spec is internally inconsistent with itself and lies
about the registration surface.

## What Changes

- Update every requirement / scenario body that names a dropped protocol key
  to use the surviving key (`:apply-block-fn`, `:full-rebuild-required-p`,
  `:rebuild-fn`) or the naming-convention pair
  (`<name>--block-at-point` / `<name>--edit-at-point`).
- Where a decorator's spec section describes *what* it does in terms of
  `:apply-display` / `:apply-anchors`, restate the behaviour in terms of
  `:apply-block-fn` plus, where the decorator opts in, the
  `gfm-pretty-borders--apply-with-anchors` helper.
- Where a decorator's spec section mentions registering
  `:structural-line-ranges` + `:edit-adjacency`, collapse the prose to a single
  `:full-rebuild-required-p` reference.
- No behaviour change; no code change. Pure spec / requirement hygiene.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `gfm-pretty`: requirement bodies in the Callouts, Fences, Tables, Hrule,
  Links, Public block introspection, Engine-owned generic rebuild stats, and
  Debounced rebuild scheduler sections are rewritten to use the surviving
  protocol keys.

## Impact

- **Affected files**: `openspec/specs/gfm-pretty/spec.md` only.
- **No code changes**, no test changes. The change is documentation-only.
- **Risk**: low. The implementation already matches the narrowed protocol;
  the spec is what's stale. Verification is `openspec validate gfm-pretty
  --strict` and a grep for the dropped keys returning zero hits inside
  requirement bodies.
