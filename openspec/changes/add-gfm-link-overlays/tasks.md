## 1. Width-walker extension (foundation)

- [ ] 1.1 Add optional `(buffer beg end)` parameters to
      `gfm-tables--visible-width--compute`; default to nil for
      backwards compatibility
- [ ] 1.2 Implement the overlay-walk branch: when source-buffer
      args are provided, walk overlays in `[beg, end)` and honour
      `display`, `invisible`, `composition` overlay properties
      with the same rules used for text properties
- [ ] 1.3 Update `gfm-tables--visible-width` (the memoised wrapper)
      to thread the new args through
- [ ] 1.4 Restructure `gfm-tables--fontify-cell` to measure cell
      width in the source buffer before copying the cell text to
      the scratch fontify buffer
- [ ] 1.5 Add a focused test in `tests.el` for the overlay path:
      construct a source buffer with an overlay carrying a
      `display` string, verify the walker counts the display
      width and not the underlying text width

## 2. New module: +gfm-links.el skeleton

- [ ] 2.1 Create `modules/lang-markdown/lib/+gfm-links.el`
      with header, license / commentary block, and required
      requires (`markdown-mode`, `nerd-icons`, `xref`, `eldoc`)
- [ ] 2.2 Declare buffer-local state: `gfm-links--ref-def-alist`,
      `gfm-links--overlay-registry`, `gfm-links--rebuild-timer`
- [ ] 2.3 Declare per-link `gfm-links-id` property generator and
      shared face/keymap defcustoms
- [ ] 2.4 Define `gfm-links-mode` as a buffer-local minor mode
      with on/off lifecycle stubs

## 3. Link shape discovery

- [ ] 3.1 Implement scanner for inline links using
      `markdown-regex-link-inline` (reuse markdown-mode match
      data; reject image links via group 1)
- [ ] 3.2 Implement scanner for reference links via
      `markdown-regex-link-reference` (handle full, collapsed,
      shortcut forms; reject image variants)
- [ ] 3.3 Implement scanner for autolinks (`<https?://…>`)
- [ ] 3.4 Implement scanner for GFM bare URLs outside other markup
- [ ] 3.5 Implement scanner for wiki links gated on
      `markdown-enable-wiki-links`, using
      `markdown-convert-wiki-link-to-filename` for target
      resolution
- [ ] 3.6 Combine into a single `gfm-links--blocks-in-range` that
      returns the link records in buffer order

## 4. Reference-definition alist

- [ ] 4.1 Implement `gfm-links--build-ref-def-alist` scanning the
      buffer for `markdown-regex-reference-definition` matches
      (first definition wins)
- [ ] 4.2 Wire alist recomputation into the rebuild entry point
- [ ] 4.3 Add resolution helper that looks up the URL for a label
      (collapsed form falls back to the title as label) and
      returns nil for missing labels

## 5. Icon resolution

- [ ] 5.1 Implement `gfm-links--icon-for-target URL`: branch on
      `http(s)://`, relative path / `file:`, `#` anchor, or other
      scheme; dispatch to `nerd-icons-icon-for-url` or
      `nerd-icons-icon-for-file`
- [ ] 5.2 Implement `gfm-links--label-for-naked-url URL` that
      extracts the host via `url-host` for autolinks / bare URLs

## 6. Overlay construction

- [ ] 6.1 Implement `gfm-links--make-title-overlay` (covers title
      bracket region; `display` = title with
      `markdown-link-face`; `keymap` carries RET binding;
      `window`-scoped; registers in registry)
- [ ] 6.2 Implement `gfm-links--make-url-overlay` (covers URL
      region; `display` = icon glyph; partner of title overlay
      via shared `gfm-links-id`; `window`-scoped)
- [ ] 6.3 Implement `gfm-links--make-naked-url-overlay` (for
      autolinks / bare URLs: one title-side overlay over the
      URL span with host label; one url-side overlay over the
      same span — coordinate so display strings don't overlap;
      or one combined overlay with composed display)
- [ ] 6.4 Implement reference-link branch that pulls the URL
      from the alist before building overlays; skip when the
      label has no definition

## 7. Suppression of built-in compose path

- [ ] 7.1 Add `:around` advice to `markdown-fontify-inline-links`
      that let-binds `markdown-hide-urls` to nil when
      `gfm-links-mode` is active in the current buffer
- [ ] 7.2 Add the same `:around` advice to
      `markdown-fontify-reference-links`
- [ ] 7.3 Confirm via test that no `composition` text property is
      added to URL regions when the mode is on, and that face
      application is otherwise unchanged

## 8. Cursor reveal (post-command-hook)

- [ ] 8.1 Implement `gfm-links--reveal` mirroring
      `gfm-callouts--reveal`: walk overlays at point in the
      selected window, suppress `display` on both partner
      overlays (matched by `gfm-links-id`)
- [ ] 8.2 Install / remove the hook in the mode lifecycle
- [ ] 8.3 Test: reveal in one window does not affect the other
      window showing the same buffer

## 9. RET / follow-link

- [ ] 9.1 Implement `gfm-links-follow-link-at-point` that
      resolves the URL at point and calls `markdown--browse-url`
- [ ] 9.2 Bind `RET` to the command through the title-side
      overlay's `keymap` property
- [ ] 9.3 Test: RET on a decorated link follows the URL; RET on
      plain prose falls through to `markdown-enter-key`

## 10. Reference goto-definition via xref

- [ ] 10.1 Implement `gfm-links--xref-backend` returning the
       backend symbol when point is on a reference-style link
       (full / collapsed / shortcut), nil otherwise
- [ ] 10.2 Implement `xref-backend-definitions` method that
       returns one `xref-item` pointing at the `[label]: …`
       definition line
- [ ] 10.3 Register the backend on buffer-local
       `xref-backend-functions` in the mode lifecycle
- [ ] 10.4 Test: `xref-find-definitions` on a reference link
       jumps to the definition line

## 11. Eldoc URL exposure

- [ ] 11.1 Implement `gfm-links--eldoc-function` returning the
       resolved URL (and title attribute when present) when
       point is on a decorated link, nil otherwise
- [ ] 11.2 Register on buffer-local
       `eldoc-documentation-functions` in the mode lifecycle
- [ ] 11.3 Test: eldoc returns nil off any link; non-nil with
       expected content on a link

## 12. Mode lifecycle wiring

- [ ] 12.1 Wire `markdown-mode-hook` to enable
       `gfm-links-mode` when `markdown-hide-urls` is non-nil
- [ ] 12.2 Install buffer-local `add-variable-watcher` on
       `markdown-hide-urls` so the mode toggles in response
- [ ] 12.3 On mode disable: remove all overlays, kill the
       rebuild timer, deregister hooks, deregister xref
       backend, deregister eldoc function
- [ ] 12.4 Add the new module to `modules/lang-markdown/init.el`
       (load + autoload registration)

## 13. Debounced rebuild + narrowing safety

- [ ] 13.1 Implement debounced-rebuild timer mirroring
       `gfm-callouts--arm-rebuild-timer`
- [ ] 13.2 Hook `after-change-functions` to arm the rebuild
       (scoped: only when an edit touches a region that may
       contain link syntax or a definition line)
- [ ] 13.3 Implement narrowing-aware rebuild: walk visible
       window ranges via the existing `gfm-block-borders`
       helpers; do not leak overlays outside narrowed range

## 14. Tests

- [ ] 14.1 Per-shape decoration tests: one ERT test per link
       shape covered in the spec (inline w/ title attr,
       reference full / collapsed / shortcut, autolink, bare
       URL, wiki link; plus a negative test for image links)
- [ ] 14.2 Reference resolution tests: alist build, first-def-
       wins, broken-reference no-decoration, alist invalidation
       on definition edit
- [ ] 14.3 Suppression test: confirm no `composition` text
       property under our mode; confirm composition still
       applied in buffers where the mode is off
- [ ] 14.4 Reveal tests: per-window reveal independence; whole-
       link reveal when point on either overlay
- [ ] 14.5 RET test: follows URL on link; falls through off link
- [ ] 14.6 xref backend test: jumps to definition line; defers
       off reference links
- [ ] 14.7 Eldoc test: returns URL on link; nil off link
- [ ] 14.8 Width-walker overlay-path test (from task 1.5);
       table-cell-with-decorated-link test confirming column
       width matches visible width including icon
- [ ] 14.9 Narrowing-regression test under
       `:tags '(narrowing-regression)` per CLAUDE.md: narrow →
       rebuild → widen → rebuild produces the same overlay set
       as a clean widened rebuild

## 15. Verification

- [ ] 15.1 Run `make test-quick`
- [ ] 15.2 Run `make test` (full suite)
- [ ] 15.3 Run `nix develop --command bash -c 'make test'` to
       confirm the prek hook passes in the devShell (per
       CLAUDE.md devShell requirement for tree-sitter grammars)
- [ ] 15.4 Manual smoke: open a markdown file with a mix of
       link shapes inside and outside tables; toggle
       `markdown-hide-urls`; verify decoration appears and
       disappears; verify reveal-on-cursor; verify RET and
       `M-.` semantics; verify table column widths stay
       aligned with decorated links inside cells
- [ ] 15.5 Run `openspec validate add-gfm-link-overlays` and
       confirm green
