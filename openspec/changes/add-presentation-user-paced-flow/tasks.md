## 1. push_slide semantics flip

- [ ] 1.1 Failing test: `+presentation--deck-push` without
      `:set-current` leaves the session's `:current-slide-index`
      unchanged and does not call render
- [ ] 1.2 Failing test: `+presentation--deck-push` with
      `:set-current` t advances the index and renders
- [ ] 1.3 Failing test: returns the appended index in both cases
- [ ] 1.4 Update `+presentation--deck-push` to take a
      `:set-current` keyword arg (default nil); tests green
- [ ] 1.5 Failing test: `push_slide` MCP tool coerces snake_case
      `set_current` → `:set-current`; default false
- [ ] 1.6 Update `push_slide` registration in `init.el` to thread
      `set_current` through; tests green
- [ ] 1.7 Update `push_slide` tool description to document the new
      semantics and the `set_current` opt-in

## 2. Channel capability registration

- [ ] 2.1 Investigate upstream `claude-code-ide-mcp.el` for an
      existing extension point on the initialize-response
      capabilities; record findings inline in design.md
- [ ] 2.2 If no extension point exists, draft a small patch adding
      `claude-code-ide-mcp-additional-capabilities` defcustom;
      submit as upstream PR (track URL in tasks)
- [ ] 2.3 Implement the local-advice fallback: advise the
      initialize-response handler `:filter-return` to splice
      `experimental.claude/channel: {}` into the capabilities
- [ ] 2.4 Failing test: the advised initialize-response output
      contains `experimental.claude/channel` as `:json-empty`
- [ ] 2.5 Failing test: when the underlying function isn't loadable
      (bare batch), capability registration silently no-ops
- [ ] 2.6 Wire registration into the presentation module's init
      sequence; tests green

## 3. Channel notification emission

- [ ] 3.1 Failing test for `+presentation--emit-nav-channel`:
      composes the correct content string for forward and backward
      moves; meta carries key, current_slide, prior_slide, kind,
      title (when present)
- [ ] 3.2 Failing test: meta values are all strings (no integers,
      no symbols)
- [ ] 3.3 Failing test: title meta key absent when slide has no
      title
- [ ] 3.4 Failing test: emission is best-effort — when
      `claude-code-ide-mcp--send-notification` is unbound or
      signals, the function returns nil without re-signalling
- [ ] 3.5 Implement `+presentation--emit-nav-channel`; tests green

## 4. Wire emission into nav commands

- [ ] 4.1 Failing test: `+presentation-next-slide` calls the
      emitter with prior=current, current=current+1 after the goto
- [ ] 4.2 Failing test: `+presentation-previous-slide` calls the
      emitter with prior=current, current=current-1
- [ ] 4.3 Failing test: emitter is NOT called when nav is at deck
      end (no-op move)
- [ ] 4.4 Failing test: `goto_slide`, `push_slide` with
      `set_current: true`, and `replace_slide` of the current index
      do NOT call the emitter
- [ ] 4.5 Update the nav commands to call the emitter post-render;
      tests green

## 5. Documentation & rollout

- [ ] 5.1 Update `modules/presentation/spec.md`: document the
      user-paced model, `set_current` opt-in, channel notification
      format with example payload, and the research-preview
      caveats (Claude Code v2.1.80+, claude.ai login,
      `--dangerously-load-development-channels` flag)
- [ ] 5.2 Update `push_slide` and `replace_slide` MCP tool
      descriptions in `init.el`
- [ ] 5.3 Run `make test`; fix byte-compile warnings
- [ ] 5.4 `openspec validate add-presentation-user-paced-flow --strict`
- [ ] 5.5 Manual end-to-end check: launch claude with
      `--dangerously-load-development-channels server:<name>`,
      start a presentation, push 3 slides without `set_current`,
      navigate via `C-n` and confirm a `<channel source="presentation">`
      tag appears in the session
