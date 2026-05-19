## 1. Public commands + cross-cutting wording

- [x] 1.1 In `openspec/specs/gfm-pretty/spec.md`, update the
  *Public block introspection* requirement body and scenario to drop
  `:block-at-point` / `:edit-at-point` slot references and describe
  the `gfm-pretty-<name>--block-at-point` / `gfm-pretty-<name>--edit-at-point`
  naming convention (per the delta).
- [x] 1.2 Update the *Rendering primitives are public* requirement's
  "Decorator builds a bordered block" scenario to use `:apply-block-fn`.
- [x] 1.3 Update the *Debounced rebuild scheduler* requirement body
  (drop `window-configuration-change-hook` from the list of hooks
  decorators may NOT install — tables now does install one — and
  rewrite the burst-of-edits scenario to use `:apply-block-fn`).

## 2. Callouts section

- [x] 2.1 Update *Callout bordered-block rendering* to describe
  `:apply-block-fn` plus `gfm-pretty-borders--apply-with-anchors`.
- [x] 2.2 Rewrite *Callout marker line and body prefix reveal* to
  derive the revealable property from the registry tag instead of
  declaring `:revealable-p`.
- [x] 2.3 Rewrite *Callout scoped post-edit rebuild* around a single
  `:full-rebuild-required-p` predicate (drop
  `:structural-line-ranges` + `:edit-adjacency`); update both
  scenarios to match.
- [x] 2.4 Update *Theme change responsiveness* to use the `-fn`
  suffix on `:on-enable-fn` / `:on-disable-fn`.

## 3. Fences section

- [x] 3.1 Update *Code-fence bordered-block rendering* to describe
  `:apply-block-fn` + the borders helper.
- [x] 3.2 Rewrite *Code-fence marker line reveal* to derive the
  revealable property from the registry tag.
- [x] 3.3 Rewrite *Code-fence scoped post-edit rebuild* around a
  single `:full-rebuild-required-p`; update both scenarios.

## 4. Hrule section

- [x] 4.1 Update *HR rendering* to use `:apply-block-fn`.
- [x] 4.2 Rewrite *HR cursor reveal* to derive the revealable
  property from the registry tag.

## 5. Links section

- [x] 5.1 Update *Title-side overlay rendering* to use
  `:apply-block-fn`.
- [x] 5.2 Update *URL-side icon rendering* to use `:apply-block-fn`.
- [x] 5.3 Rewrite *Whole-link cursor reveal* to describe the
  registry-derived revealable property + the bespoke `:reveal-fn`.

## 6. Verification

- [x] 6.1 Grep the stable spec under
  `openspec/specs/gfm-pretty/spec.md` for the dropped keys
  (`:apply-anchors`, `:apply-display`, `:structural-line-ranges`,
  `:edit-adjacency`, `:revealable-prop`, `:saved-display-prop`,
  `:revealable-p`, `:reconcile-windows`, `:block-at-point`,
  `:edit-at-point` not followed by `-fn`). Hits inside the
  *Decorator registration via `gfm-pretty-define-decorator`*
  "SHALL NOT include keys for" list are expected; everywhere else
  SHALL be zero.
- [x] 6.2 Run `openspec validate gfm-pretty --strict` and confirm it
  passes against the post-sync stable spec.
- [x] 6.3 Run `openspec validate align-gfm-pretty-decorator-specs
  --strict` and confirm it passes.
- [x] 6.4 Run `make test` and confirm green (no code changed; the
  test suite should be unchanged, but run as a sanity gate against
  accidental edits).
