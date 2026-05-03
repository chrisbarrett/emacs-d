## Why

Dogfooding the slide-ops MCP surface revealed that pushing a deck of
slides feels like a takeover: the agent fires `push_slide` repeatedly,
each call yanks the user's frame to a new slide, and the user has no
breathing room to read.  By the time the agent stops, the user is
several slides ahead of where they wanted to be.

The model should be inverted: the agent *prepares* slides, the user
*walks* the deck at their own pace.  `add-presentation-layout-and-nav`
already gives the user `C-n` / `C-f` / `C-p` / `C-b` keybindings.
What's missing:

1. `push_slide` shouldn't drag the user.  It should append silently,
   leaving the current slide put.
2. The agent needs to know when the user has moved, so it can decide
   whether to push more, replace pending slides, or stop pushing
   altogether.

Polling via `get_presentation` works but is wasteful and slow.  Claude
Code now has a *channels* mechanism (research preview, Claude Code
v2.1.80+) for MCP servers to push notifications into the agent's
running context — exactly the right primitive for "the user just
moved".

## What Changes

- **`push_slide` no longer auto-advances.**  Today it sets
  `:current-slide-index` to the new index and renders.  After this
  change, `push_slide` only appends and validates; the rendered slide
  for the user is unchanged.  This is a **BREAKING** behavioural
  change for existing agents that relied on push-as-navigate.
- **`start_presentation`'s `initial_slide` remains the auto-render
  trigger** for the very first frame, so the user always sees
  something on session start.  When `initial_slide` is omitted, the
  splash buffer is shown until the user navigates into the deck.
- **First `push_slide` against an empty deck does NOT auto-render**
  either — the agent should always pair an empty session with either
  `initial_slide` or a follow-up `goto_slide(0)`.  Documented
  explicitly so this isn't a footgun.
- **New optional `set_current` flag on `push_slide`** (default
  `false`).  When `true`, the slide IS rendered and `current_slide_
  index` is set.  Reserved for "stop, look here" moments — pushing
  speculative work-in-progress slides should leave it false.
- **MCP server declares the experimental `claude/channel` capability**
  so Claude Code's channel listener registers.  Implementation lives
  in either an upstream patch to `claude-code-ide-mcp.el`'s
  hardcoded capabilities object, or a local `advice` wrap from the
  presentation module.  The change documents both options and picks
  one in design.md.
- **Elisp side fires `notifications/claude/channel` events** when the
  current slide index changes due to user-driven navigation
  (`+presentation-next-slide` / `+presentation-previous-slide`,
  introduced in `add-presentation-layout-and-nav`).  Agent-driven
  changes (`goto_slide`, `push_slide` with `set_current: true`,
  `replace_slide` on current) do NOT fire — the agent already knows.
- **Notification payload** carries:
  - `content`: human-readable line for Claude's context
    (e.g. `"User advanced to slide 3 of 7."`).
  - `meta.key`: session key.
  - `meta.current_slide`: new index.
  - `meta.prior_slide`: old index.
  - `meta.kind`: slide kind at the new index (`narrative`/`file`/
    `diff`/`layout`).
  - `meta.title`: slide title when present.
  Arrives in the agent's context as
  `<channel source="presentation" key="…" current_slide="3"
  prior_slide="2" kind="file" title="…">User advanced to slide 3 of
  7.</channel>`.

## Capabilities

### New Capabilities

<!-- none — extends the existing presentation capability -->

### Modified Capabilities

- `presentation`: changes `push_slide` semantics to non-mutating-of-
  current-index by default; adds opt-in `set_current` flag; adds
  `claude/channel` notification emission on user-driven slide
  navigation.

## Impact

- Modified files:
  - `modules/presentation/init.el` — `push_slide` tool description
    documents new semantics; coerce `set_current` from snake_case;
    declare or advise channel capability into the MCP server's
    initialize-response payload.
  - `modules/presentation/lib.el` — `+presentation--deck-push` no
    longer mutates `:current-slide-index` unless `:set-current` is
    truthy; navigation commands fire channel notifications via
    `claude-code-ide-mcp--send-notification`.
  - `modules/presentation/spec.md` — document the user-paced model,
    the `set_current` opt-in, the channel notification format, and
    the research-preview caveats.
  - `modules/presentation/tests.el` — assertions that `push_slide`
    leaves current index unchanged; that `set_current: true`
    advances; that user-driven nav emits a channel notification with
    correct meta; that agent-driven mutation does NOT fire one.
- Depends on:
  - `add-presentation-slide-ops` (deck model).
  - `add-presentation-layout-and-nav` (user-side `C-n`/`C-p`
    bindings — without those, this change has no user-driven nav to
    emit notifications from).
- Independent of `fix-presentation-overlay-rendering`.
- **Research-preview caveats** (called out in design.md and
  user-facing docs):
  - Requires Claude Code v2.1.80 or later.
  - Requires claude.ai login; API-key auth not supported.
  - During the preview, the presentation MCP server isn't on
    Anthropic's allowlist, so the user must launch claude with
    `--dangerously-load-development-channels server:claude-code-ide-mcp`
    (or whatever the registered server name is).  This SHALL be
    documented prominently in the module spec.
- **Upstream dependency**: declaring the `experimental.claude/channel`
  capability requires either a patch to `claude-code-ide-mcp.el`
  (upstream PR) or local `advice` from the presentation module.
  Design picks one; an upstream PR is preferred but the local-advice
  fallback is acceptable while waiting.
