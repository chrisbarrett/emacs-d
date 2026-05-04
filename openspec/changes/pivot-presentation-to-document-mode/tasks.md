## 1. Narrative slide accepts `:path`

- [x] 1.1 Failing test: `+presentation--validate-slide` accepts a
      narrative slide with only `:path`; rejects narrative with
      neither `:path` nor `:markdown`; rejects narrative with both
- [x] 1.2 Failing test: validation rejects `:path` that is not a
      string with a named-value error message
- [x] 1.3 Failing test: rendering a narrative slide with `:path`
      uses `find-file-noselect` against the worktree-resolved path
      and the resulting buffer becomes the displayed buffer
- [x] 1.4 Failing test: rendering a narrative slide with `:markdown`
      continues to use the synthetic `*presentation: KEY*` buffer
- [x] 1.5 Failing test: when the file at `:path` does not exist,
      rendering signals a `user-error` naming the missing path
- [x] 1.6 Implement: extend validator with the path/markdown XOR
      rule; add path branch to `+presentation--render-narrative`
- [x] 1.7 Update slide-spec coercion in `init.el` to accept `path`
      from snake_case payloads on the narrative branch

## 2. `present_document` MCP tool

- [x] 2.1 Failing test: planner/path helper produces the expected
      ISO-date-prefixed filename for a fixed clock and slug input
      (e.g. `2026-05-04T14-22-auth-walkthrough.md`)
- [x] 2.2 Failing test: when `<worktree>/.claude/presentations/`
      does not exist, the tool's effect plan includes its creation
- [x] 2.3 Failing test: tool composition emits a `Write` of the
      markdown body to the computed path, then a
      `start_presentation` whose `initial_slide` is a narrative
      with `:path` matching the doc, then one `push_slide` per
      entry in `file_slides` in order
- [x] 2.4 Failing test: tool returns an alist with `key`, `path`,
      and `slide_count` matching `1 + (length file_slides)`
- [x] 2.5 Failing test: invalid `slug` (empty, contains `/`, or
      contains whitespace) signals a `user-error` without writing
      any file
- [x] 2.6 Failing test: any entry in `file_slides` whose `kind`
      is not `"file"` signals a `user-error`; no file is written;
      no session is created
- [x] 2.7 Implement: register `present_document` via
      `claude-code-ide-make-tool`; argument coercion mirrors
      `start_presentation`
- [x] 2.8 Update `get_deck`-style title behaviour: narrative slide
      with `:path` reports its `:title` field (or the doc's
      first-heading line as a fallback) so deck inspection stays
      useful

## 3. Markdown link click-handler

- [x] 3.1 Failing test for deck-search helper: an exact path-and-
      range match returns the slide index; non-match returns nil
- [x] 3.2 Failing test: `slide:N` URL parses to integer N; invalid
      forms (`slide:foo`, `slide:`) parse to nil
- [x] 3.3 Failing test: `path#L42-L67` URL parses to
      `(path . (42 . 67))`; `path#L42` parses to
      `(path . (42 . 42))`; missing anchor parses to nil
- [x] 3.4 Failing test: dispatch fn receives URL + session key;
      returns symbol describing dispatch (`'goto-slide` /
      `'find-file-fallback` / `'pass-through`) without performing
      I/O
- [x] 3.5 Failing test: `slide:2` against a session with deck
      length 3 dispatches to `'goto-slide` with index 2
- [x] 3.6 Failing test: `path.el#L42-L67` against a deck containing
      a file slide with that exact path/range dispatches to
      `'goto-slide` with that index; deck without a match
      dispatches to `'find-file-fallback`
- [x] 3.7 Failing test: `https://...` and plain `path.el` (no
      anchor) dispatch to `'pass-through`
- [x] 3.8 Failing test: dispatch is gated by buffer-local
      `+presentation--session-key` AND major mode `markdown-mode`
      — narrative buffers in unrelated markdown contexts pass
      through
- [x] 3.9 Implement: bind `RET` (and optionally `mouse-1`) in
      `+presentation-mode-map` to a command that calls
      `markdown-link-at-pos` (or equivalent), runs the dispatcher,
      and invokes `goto_slide` / `find-file` accordingly
- [ ] 3.10 Manual smoke: from a narrative slide rendered via
      `present_document`, follow a `slide:1` link → annotated
      file slide; follow a `path#Lx-Ly` link with a deck match →
      same; follow a non-match → plain jump

## 4. Spec & rule updates

- [x] 4.1 Update `modules/presentation/spec.md`: extend Narrative
      slide rendering requirement with `:path` branch; add
      validation cases; add `present_document` requirement; add
      click-handler dispatch requirement
- [x] 4.2 Update tool descriptions in `init.el` to recommend
      doc-first usage and reference `present_document`
- [x] 4.3 Rewrite `~/.claude/rules/...presentation.md` (Nix
      module) so the trigger guidance prefers `present_document`
      over many `push_slide` calls; explicitly discourages
      multiple narrative slides per session
- [x] 4.4 Update `CLAUDE.md` if it references the slide-deck
      pattern in any worked example

## 5. Verification

- [x] 5.1 `make test` passes
- [ ] 5.2 End-to-end manual: agent calls `present_document` with
      one narrative + two annotated file slides; user reads doc,
      clicks both links, navigates back via `C-p`, ends session
      with `C-c q`; presentation frame teardown matches origin
- [x] 5.3 Confirm `.claude/presentations/` is gitignored in a
      fresh checkout
