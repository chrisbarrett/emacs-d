## 1. Module scaffold

- [x] 1.1 Create `modules/lang-markdown/lib/+gfm-tables.el` with file header,
  `defgroup gfm-tables`, and the `provide` form
- [x] 1.2 Add the `gfm-tables-row-alt-face` defface with light (`#efe9dd`)
  and dark (`#313244`) variants
- [x] 1.3 Add the `gfm-tables-slow-rebuild-threshold` defcustom (default 0.05)

## 2. Cell parser (TDD)

- [x] 2.1 Add failing test for splitting a simple `| a | b | c |` row
- [x] 2.2 Implement minimal walker tracking position, escape state, and
  backtick-span depth
- [x] 2.3 Add failing test for `\|` escape; extend parser to skip escaped pipes
- [x] 2.4 Add failing test for single-backtick code span containing `|`;
  extend parser to track backtick spans
- [x] 2.5 Add failing test for double-backtick code span containing `|`;
  extend backtick-span tracking to support arbitrary backtick run lengths
- [x] 2.6 Add failing test for unbalanced/degenerate backticks; document
  the parser's behaviour on malformed input

## 3. Table block discovery (TDD)

- [x] 3.1 Add failing test that finds a single GFM table given header,
  delimiter, and body rows
- [x] 3.2 Implement `gfm-tables--find-blocks` returning
  `(HEADER-BEG DELIM-BEG BODY-BEG BODY-END)` per table
- [x] 3.3 Add failing test that ignores tables whose lines fall inside a
  fenced code block; implement skip-ranges via
  `gfm-code-fences--find-blocks`
- [x] 3.4 Add failing test that rejects a delimiter-shaped line with no
  preceding pipe-prefixed line

## 4. Column width computation (TDD)

- [x] 4.1 Add failing test that computes per-column max widths across
  header + body for an unaligned table
- [x] 4.2 Implement `gfm-tables--column-widths` returning a vector of
  `string-width`-derived per-column maxima

## 5. Per-row composed display string

- [x] 5.1 Implement `gfm-tables--compose-row` returning the propertized
  display string for one row given (cells, col-widths, row-bg, role) where
  role ∈ {header, body, rule}
- [x] 5.2 Header row composition: bold weight via `'face '(:weight bold)`
  on cell substrings, default-bg row background
- [x] 5.3 Body row composition: alt-bg or default-bg per zebra position;
  1-char default-bg gap between cells
- [x] 5.4 Rule row composition: continuous `├─…─┤` of width = box width
- [x] 5.5 Outer pipes as `│` propertized with the border face

## 6. Outer box decoration

- [x] 6.1 Compute box width from column widths
- [x] 6.2 Build top border string `┌─…─┐` with the border face
- [x] 6.3 Build bottom border string `└─…─┘` with the border face
- [x] 6.4 Attach top border as `before-string` on the header row's overlay
- [x] 6.5 Attach bottom border as `after-string` on the last body row's
  overlay

## 7. Overlay application

- [x] 7.1 Add `gfm-tables--register` and `gfm-tables--remove-overlays`
  helpers mirroring the gfm-code-fences pattern
- [x] 7.2 Implement `gfm-tables--apply-table` creating one display overlay
  per row (header, rule, body), with `evaporate t` and a
  `gfm-tables-revealable t` flag
- [x] 7.3 Implement `gfm-tables--apply-overlays` iterating over discovered
  blocks and applying decoration to each, skipping fenced ranges

## 8. Cursor-driven reveal

- [x] 8.1 Implement `gfm-tables--reveal` mirroring
  `gfm-code-fences--reveal`: hide display on revealable overlays
  containing point, restore others
- [x] 8.2 Wire `gfm-tables--reveal` into `post-command-hook` when the
  mode is enabled
- [x] 8.3 Add test exercising entry/leave reveal cycle

## 9. Debounced rebuild

- [x] 9.1 Implement `gfm-tables--rebuild` (remove + apply)
- [x] 9.2 Implement `gfm-tables--schedule-rebuild` with the 0.2 s idle
  timer + indirect-buffer guard, mirroring sibling modules
- [x] 9.3 Wire scheduler into `after-change-functions` and
  `window-configuration-change-hook`
- [x] 9.4 Add a hook on theme change (or explicit advice on
  `enable-theme`) to trigger a rebuild so cached default-bg refreshes

## 10. Performance instrumentation

- [x] 10.1 Define `gfm-tables--stats` buffer-local alist storing
  rebuild-count, total-time, last-time, max-time, table-count
- [x] 10.2 Wrap `gfm-tables--rebuild` in a `current-time` delta and
  update `gfm-tables--stats`
- [x] 10.3 Emit a `message` warning when a single rebuild exceeds
  `gfm-tables-slow-rebuild-threshold`
- [x] 10.4 Implement `M-x gfm-tables-stats` displaying the current
  buffer's stats

## 11. Minor mode and integration

- [x] 11.1 Define `gfm-tables-mode` minor mode (lighter ` gfm-tb`) that
  toggles overlays, hooks, and timers
- [x] 11.2 Wire `gfm-tables-mode` into the markdown-mode setup alongside
  `gfm-code-fences-mode` and `gfm-callouts-mode` (likely in
  `modules/lang-markdown/init.el` or the file registering those hooks)
- [x] 11.3 Verify the existing `lang-markdown` `make test` suite still
  passes

## 12. End-to-end verification

- [x] 12.1 Open the table at
  `nix-configuration/openspec/changes/integrate-mattpocock-skills/design.md`
  with the mode enabled; confirm the visual matches the locked design
  (outer box, continuous rule, zebra body, header bold, gap-trick visible)
- [x] 12.2 Verify cursor reveal on a striped row
- [x] 12.3 Verify rebuild after edit and after window resize
- [x] 12.4 Confirm `M-x gfm-tables-stats` reports plausible numbers
- [x] 12.5 Toggle the active theme between `modus-operandi-tinted` and
  `catppuccin` (mocha) and confirm gap colour and stripe both follow

## 13. Documentation

- [x] 13.1 Update the lang-markdown spec doc(s) under `specs/` to
  reference the new mode (alongside callouts and code-fences entries)
- [x] 13.2 Add a brief section to the module's existing documentation
  explaining the customisable face and the slow-rebuild threshold
