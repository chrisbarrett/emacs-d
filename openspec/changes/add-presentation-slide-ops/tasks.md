## 1. Validation & deck state

- [ ] 1.1 Add failing tests for `+presentation--validate-slide`: covers
      each kind's required fields, nested-layout rejection, two-pane
      layout requirement, annotation line/position validation
- [ ] 1.2 Implement `+presentation--validate-slide`; tests green
- [ ] 1.3 Add failing tests for deck mutation helpers
      (`+presentation--deck-push`, `--deck-replace`, `--deck-truncate`,
      `--deck-goto`) operating on a session plist; cover bounds errors
- [ ] 1.4 Implement deck mutation helpers; tests green; refactor for
      clarity

## 2. Render dispatch & narrative migration

- [ ] 2.1 Add failing test that `+presentation--render-slide` dispatches
      on `:kind` and clears the prior slide's `:render-state` overlays
      before rendering the next
- [ ] 2.2 Refactor existing narrative rendering into
      `+presentation--render-narrative` returning a buffer and updating
      `:render-state`; tests green
- [ ] 2.3 Add failing test that `start_presentation` with
      `initial_slide` populates `:deck` at index 0 and sets
      `:current-slide-index` to 0
- [ ] 2.4 Wire `start_presentation` to the deck model; tests green

## 3. file slide kind

- [ ] 3.1 Add failing tests for `+presentation--render-file`: relative
      path resolution against `:worktree`; narrowing to range; focus
      overlay placement; read-only toggle stash & restore
- [ ] 3.2 Implement `+presentation--render-file`; tests green

## 4. diff slide kind

- [ ] 4.1 Add failing tests for the diff command planner: working-tree
      argv, range argv, path-scoped argv, half-specified-range error
- [ ] 4.2 Implement diff planner + `+presentation--render-diff`
      (consumes effect runner output, inserts into per-session diff
      buffer in `diff-mode`); tests green

## 5. layout slide kind

- [ ] 5.1 Add failing tests for `+presentation--render-layout`: panes
      rendered to their target buffers; window split direction; nested
      layout rejected at validation (covered already in 1.1, assert via
      the public push path here)
- [ ] 5.2 Implement `+presentation--render-layout`; tests green

## 6. Annotation overlays

- [ ] 6.1 Add failing tests for `+presentation--apply-annotations`:
      overlay at correct line, before/after positioning, face
      application, overlays accumulate into `:render-state`
- [ ] 6.2 Implement `+presentation--apply-annotations`; tests green
- [ ] 6.3 Add failing test that overlays are deleted on slide-leave
      (push, goto, replace-current, truncate-past-current,
      end_presentation)
- [ ] 6.4 Wire overlay cleanup into the render dispatch & teardown
      paths; tests green

## 7. MCP tool registration

- [ ] 7.1 Add failing test that `claude-code-ide-mcp-server-tools` lists
      `push_slide`, `replace_slide`, `truncate_after`, `goto_slide`,
      `get_deck` after module init
- [ ] 7.2 Register the five new tools in `modules/presentation/init.el`
      with snake_case → kebab-case plist conversion via the existing
      helper; tests green

## 8. get_presentation extension

- [ ] 8.1 Add failing test that `+presentation-info` returns
      `slide_count` and `current_slide_index`
- [ ] 8.2 Extend `+presentation-info`; tests green

## 9. Documentation & polish

- [ ] 9.1 Update module `spec.md` if it documents the public tool
      surface; ensure tool descriptions in `init.el` cover failure
      modes (out-of-range index, half-specified diff range, nested
      layout)
- [ ] 9.2 Run `make test`; fix lints/byte-compile warnings
- [ ] 9.3 `openspec validate add-presentation-slide-ops --strict`
