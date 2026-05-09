## 1. Predecessor cleanup

- [x] 1.1 Run / waive smoke tests 3.10 and 5.2 of `pivot-presentation-to-document-mode`
- [x] 1.2 Archive `pivot-presentation-to-document-mode` via `openspec archive`
- [x] 1.3 Confirm `openspec/specs/presentation/spec.md` reflects the post-archive baseline before this change deltas it

## 2. Strip the deck and MCP surface

- [x] 2.1 Failing test: module init no longer registers any of `start_presentation`, `present_document`, `get_presentation`, `end_presentation`, `push_slide`, `replace_slide`, `truncate_after`, `goto_slide`, `get_deck`
- [x] 2.2 Delete the eight tool registrations from `modules/presentation/init.el`
- [x] 2.3 Delete the `with-eval-after-load 'claude-code-ide-mcp` block that registers channel capability
- [x] 2.4 Delete `+presentation-start`, `+presentation-end`, `+presentation-info`, `+presentation-deck-info`, `+presentation-present-document` from `lib.el`
- [x] 2.5 Delete `+presentation--validate-slide`, `+presentation--validate-annotation`, `+presentation--coerce-slide`, `+presentation--alist-to-plist`, `+presentation--alistp` from `lib.el`
- [x] 2.6 Delete `+presentation--validate-slug`, `+presentation--document-path`, `+presentation--narrative-first-heading`, `+presentation--slide-title` from `lib.el`
- [x] 2.7 Delete deck operations: `+presentation--deck`, `--deck-push`, `--deck-replace`, `--deck-truncate`, `--deck-goto`, `--save-current-point`, `--restore-saved-point`
- [x] 2.8 Delete render dispatch: `+presentation--dispatch-slide`, `--render-slide`, `--render-current`, `--cleanup-render-state`, `--render-state-add`
- [x] 2.9 Delete renderers we no longer need: `+presentation--render-narrative`, `--render-diff`, `--render-layout`, `--apply-annotations`, `--render-inline-annotation`, `--render-callout-annotation`, `--render-margin-annotation`, `--margin-side`, `--ensure-margin-width`
- [x] 2.10 Delete tmux machinery: `+presentation-effect-shell`, `--effect-elisp`, `--run-effects`, `--cmd-list-panes`, `--cmd-split-window`, `--cmd-kill-pane`, `--cmd-window-layout`, `--cmd-select-layout`, `--parse-list-panes-output`, `--diff-panes`, `--pane-layout-effects`, `--apply-pane-layout`
- [x] 2.11 Delete frame discovery + spawn: `+presentation--default-socket`, `--find-frame-by-tty`, `--find-existing-frame`, `--make-splash-buffer`, `--narrative-buffer`, `--plan-spawn`, `--plan-reuse`
- [x] 2.12 Delete session state: `+presentation--sessions`, `--register-session`, `--get-session`, `--make-key`, `--frame-deleted-h`, `--session-set`, `--session-prop`, `--effect-results`
- [x] 2.13 Delete channel back-signal: `+presentation--emit-nav-channel`, `--inject-channel-capability`, `--register-channel-capability`, `--severity-faces`, `--severity-labels` and the related faces if not used elsewhere
- [x] 2.14 Delete `delete-frame-functions` hook installation in `init.el`
- [x] 2.15 Delete the corresponding test groups in `tests.el`: every test for the symbols deleted above
- [x] 2.16 `make test` passes after the strip

## 3. Heading-narrowed slide model

- [x] 3.1 Failing test: helper `+presentation--heading-region (point)` returns `(start . end)` of the H1 region containing point, treating fenced code blocks as opaque
- [x] 3.2 Failing test: helper `+presentation--all-h1-positions ()` returns a list of buffer positions of every `^# ` line (excluding fenced)
- [x] 3.3 Failing test: `+presentation--heading-slug TEXT` slugifies per spec (downcase, runs of non-alnum to single hyphen, strip ends)
- [x] 3.4 Implement the three helpers
- [x] 3.5 Failing test: `+presentation--narrow-to-heading-at POINT` widens then narrows to the region returned by `--heading-region`
- [x] 3.6 Failing test: with point before the first H1, narrowing falls back to the first H1's region; with point after the last H1, narrowing falls back to the last H1's region
- [x] 3.7 Implement the narrow helper
- [x] 3.8 Failing test: `+presentation-next-slide` advances to the next H1 and narrows there; at the last slide it is a silent no-op
- [x] 3.9 Failing test: `+presentation-previous-slide` retreats to the prior H1 and narrows there; at the first slide it is a silent no-op
- [x] 3.10 Implement the navigation commands

## 4. Minor mode + entry point

- [x] 4.1 Failing test: `+presentation-mode` is a buffer-local minor mode and its keymap binds `C-n`, `C-f`, `C-p`, `C-b`, `C-c q`, `RET`
- [x] 4.2 Failing test: enabling `+presentation-mode` narrows to the heading region containing point
- [x] 4.3 Failing test: disabling `+presentation-mode` widens the buffer
- [x] 4.4 Failing test: keymap is registered with `evil-make-overriding-map` so bindings work in evil normal state
- [x] 4.5 Implement the minor-mode definition and keymap (use `defvar-keymap` per the global rule)
- [x] 4.6 Failing test: `+present-markdown FILE` calls `find-file` then enables the mode; missing files signal a `user-error`
- [x] 4.7 Failing test: when the buffer was opened solely by `+present-markdown` (no prior visit), buffer-local `+presentation--owned-buffer` is set; otherwise unset
- [x] 4.8 Implement `+present-markdown` (autoloaded)
- [x] 4.9 Failing test: `+presentation-quit` widens, disables the mode, and buries the buffer when not owned; kills the buffer when owned
- [x] 4.10 Implement `+presentation-quit`

## 5. Heading-text in-doc links

- [x] 5.1 Failing test: parser `+presentation--parse-heading-link URL` returns the slug for `#some-slug`; nil otherwise
- [x] 5.2 Failing test: dispatch on `#some-slug` finds the heading whose slug matches and computes the enclosing H1 region
- [x] 5.3 Failing test: dispatch on a non-matching slug returns the symbol `'pass-through`
- [x] 5.4 Implement parser + dispatch helper
- [x] 5.5 Failing test: `+presentation-follow-link` on a heading link pushes a mark, then re-narrows to the enclosing H1 with point on the matched heading
- [x] 5.6 Failing test: `+presentation-follow-link` on a non-matching heading link calls the markdown-mode default handler
- [x] 5.7 Implement the heading-link arm of `+presentation-follow-link`

## 6. Source-range link preview overlay

- [x] 6.1 Failing test: parser `+presentation--parse-source-link URL` returns `(path . (start . end))` for `path#L42`, `path#L42-L67`; nil for plain paths and other URLs
- [x] 6.2 Failing test: helper `+presentation--read-line-range PATH START END` returns up to 10 lines plus an "extra-count" integer indicating how many lines were truncated
- [x] 6.3 Failing test: helper `+presentation--language-from-extension PATH` returns `"rust"` for `.rs`, `"elisp"` for `.el`, `"python"` for `.py`, `"markdown"` for `.md`, `"text"` otherwise
- [x] 6.4 Failing test: helper `+presentation--build-preview-fence` composes `\`\`\`<lang> <label> · <path>:<start>-<end>\n<body>\n\`\`\`` with optional `+N more lines · click to open` footer
- [x] 6.5 Failing test: missing file produces `(file not found: <path>)` body; invalid range produces `(invalid range)` body
- [x] 6.6 Implement the four helpers
- [x] 6.7 Failing test: `+presentation--render-link-previews ()` scans the current narrowing for source-range links and creates one overlay per link, storing them in buffer-local `+presentation--preview-overlays`
- [x] 6.8 Failing test: overlays use `display` property only — saving the buffer writes the original markdown link syntax, not the fence
- [x] 6.9 Implement the preview renderer
- [x] 6.10 Failing test: `+presentation--clear-link-previews ()` deletes all overlays in the buffer-local list and resets the list to nil

## 7. Diff-range link preview overlay

- [x] 7.1 Failing test: parser `+presentation--parse-diff-link URL` returns `(:base "B" :head "H" :path nil)` or `(:base "B" :head "H" :path "P")` for `diff:B...H` and `diff:B...H#P`; nil otherwise
- [x] 7.2 Failing test: command builder `+presentation--diff-preview-argv WORKTREE BASE HEAD PATH` emits the correct argv (`("git" "-C" WORKTREE "diff" "B...H" "--" "P")` when path; without `--` when nil)
- [x] 7.3 Failing test: helper that runs `git diff` and truncates to 10 lines + extra-count works against fixture worktrees with three fixed scenarios: empty diff, small diff, oversized diff
- [x] 7.4 Implement the parser, argv builder, and runner
- [x] 7.5 Failing test: empty diff produces `(no changes)` body; non-zero exit produces `(git error: <first line>)` body
- [x] 7.6 Failing test: rendering walks the current narrowing, applies preview overlays for diff links, stores them in `+presentation--preview-overlays`
- [x] 7.7 Implement the diff-preview overlay renderer

## 8. Preview refresh on slide entry

- [x] 8.1 Failing test: enabling `+presentation-mode` clears any existing preview overlays then rebuilds them in the new narrowing
- [x] 8.2 Failing test: each navigation command (next/prev) clears + rebuilds previews
- [x] 8.3 Failing test: heading-text in-doc link follow clears + rebuilds previews
- [x] 8.4 Implement the clear+rebuild call at the four entry points
- [x] 8.5 Failing test: no `file-notify-add-watch` calls anywhere in the module (negative assertion)
- [x] 8.6 Failing test: no idle-timer registrations for preview refresh

## 9. Click escape: source-range

- [x] 9.1 Failing test: clicking a source-range preview pushes a mark at point of click before navigation
- [x] 9.2 Failing test: clicking a source-range preview calls `find-file` on the resolved path then narrows the destination buffer to the range
- [x] 9.3 Failing test: clicking honours `other-window-prefix` for split control via `display-buffer`
- [x] 9.4 Failing test: the destination buffer has a focus overlay covering the requested range, painted by the existing `+presentation-focus-face`, with no `:extend t` and no painting past EOL
- [x] 9.5 Failing test: the destination buffer is set read-only with a restorer keyed off `kill-buffer-hook`
- [x] 9.6 Failing test: `+presentation-mode` is NOT enabled in the destination buffer
- [x] 9.7 Implement the source-range arm of `+presentation-follow-link`, reusing the existing file-render machinery in detached form

## 10. Click escape: diff-range

- [x] 10.1 Failing test: clicking a diff-range preview pushes a mark and invokes `magit-diff-range` with `(format "%s...%s" base head)`; with file-args `(<path>)` when scoped
- [x] 10.2 Failing test: when magit is unavailable and `(require 'magit nil t)` returns nil, a `user-error` is signalled
- [x] 10.3 Implement the diff-range arm of `+presentation-follow-link`

## 11. Other links pass through

- [x] 11.1 Failing test: `https://`, `mailto:`, plain path, and unanchored URLs dispatch to the markdown-mode default handler
- [x] 11.2 Implement the pass-through arm

## 12. Document revert resilience

- [x] 12.1 Failing test: `before-revert-hook` handler captures `:slug`, `:index`, `:fingerprint` (≤80 chars), `:window-start-offset` into buffer-local `+presentation--revert-anchor`
- [x] 12.2 Failing test: `:slug` is the slugified text of the H1 currently containing the narrowing
- [x] 12.3 Failing test: `:index` is the 0-based ordinal of that H1
- [x] 12.4 Failing test: `:fingerprint` is up to 80 characters starting at point (or fewer near `point-max`)
- [x] 12.5 Failing test: `:window-start-offset` is `(- (window-start) (point))` for the displayed window, or 0 when not displayed
- [x] 12.6 Implement the before-revert handler
- [x] 12.7 Failing test: `after-revert-hook` handler narrows by slug match when present
- [x] 12.8 Failing test: handler falls back to ordinal index when slug is gone; falls back to last H1 when index exceeds H1 count; falls back to first H1 when neither holds; leaves widened when no H1s exist
- [x] 12.9 Failing test: handler restores point and `window-start` via fingerprint substring search inside the new narrowing
- [x] 12.10 Failing test: handler rebuilds preview overlays after restore
- [x] 12.11 Implement the after-revert handler
- [x] 12.12 Failing test: hooks are installed buffer-local on `+presentation-mode` enable and removed on disable

## 13. Detached file renderer

- [x] 13.1 Failing test: `+presentation--render-narrowed-source BUFFER START-LINE END-LINE &optional FOCUS-START FOCUS-END` narrows the buffer, applies focus overlays per-line bounded to text glyphs, sets `buffer-read-only` t, and registers a kill-buffer-hook restorer
- [x] 13.2 Failing test: re-applying the renderer to the same buffer first cleans up prior overlays and restorers before applying the new ones
- [x] 13.3 Failing test: focus face does not extend past EOL on lines shorter than the window width
- [x] 13.4 Implement the detached renderer (factor out from existing `+presentation--render-file` body)
- [x] 13.5 Confirm the source-range click action (task 9.7) calls into this helper

## 14. Module-level cleanup

- [x] 14.1 Failing test: `modules/presentation/init.el` is small (no MCP registrations, no hook installs except those required by the new design)
- [x] 14.2 Failing test: `modules/presentation/lib.el` exports `+present-markdown`, `+presentation-mode`, the navigation commands, and the follow-link command — and exports nothing that no longer makes sense
- [x] 14.3 Failing test: no references remain to `+presentation--sessions`, `--effect-shell`, `--effect-elisp`, `--run-effects`, `--register-session`, `--get-session`, `--make-key`, `--deck-*`, `--render-current`, `--render-slide`, `--dispatch-slide`, `--validate-slide`, `--coerce-slide`, `--emit-nav-channel`, `--inject-channel-capability`, `--register-channel-capability`
- [x] 14.4 Update `modules/presentation/spec.md` to describe the new shape (entry point + minor mode + link forms + revert hooks + reusable narrowed-source renderer)
- [x] 14.5 Run `eldev lint` (or equivalent) and fix any new warnings

## 15. Spec sync + rule update

- [x] 15.1 Update `openspec/specs/presentation/spec.md` to reflect the merged delta (this happens automatically via `openspec archive` after merge — verify no manual edits needed)
- [x] 15.2 Rewrite `~/.claude/rules/.../presentation.md` (Nix-managed, out of tree) to describe the new pattern: agent writes a markdown file, invokes `tmux split-window -h emacsclient -t -e '(+present-markdown PATH)'` (or graphical variant), edits the file in place via `Edit`, ends via buffer kill or `C-c q` — done out-of-tree in `nix-configuration` (uncommitted): rewrote `home/config/programs/agents/skills/presentation/body.md`, simplified `default.nix` to drop the wrapper script, deleted `scripts/emacs-presentation.sh`
- [x] 15.3 Update `CLAUDE.md` if it references the old slide-deck pattern in any worked example — N/A (no references found)

## 16. Verification

- [x] 16.1 `make test` passes after all of §2–§14
- [ ] 16.2 Manual smoke: `+present-markdown` on a fixture doc with three H1s — narrowing on enable, `C-n`/`C-p` advance/retreat, `C-c q` quits — **SKIPPED**: requires interactive user
- [ ] 16.3 Manual smoke: a slide with a `path#L42-L67` link renders the preview; clicking opens the source narrowed to that range with focus overlay; `C-o` (evil) returns to the doc — **SKIPPED**: requires interactive user
- [ ] 16.4 Manual smoke: a slide with a `diff:HEAD~1...HEAD` link renders the preview; clicking opens `magit-diff-range` — **SKIPPED**: requires interactive user
- [ ] 16.5 Manual smoke: a slide with a `[label](#some-heading)` link narrows to the matching slide on follow — **SKIPPED**: requires interactive user
- [ ] 16.6 Manual smoke: agent edits the doc via `Write` while the user is on slide 3; auto-revert + slug match keeps the user on slide 3 with point near where it was — **SKIPPED**: requires interactive user
- [ ] 16.7 Manual smoke: `tmux split-window -h emacsclient -t -e '(+present-markdown PATH)'` from inside a tmux pane spawns a presentation pane; killing the doc buffer ends the presentation cleanly — **SKIPPED**: requires interactive user
