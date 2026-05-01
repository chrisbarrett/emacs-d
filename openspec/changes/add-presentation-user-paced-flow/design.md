## Context

The presentation surface today is agent-piloted: every `push_slide`
sets `:current-slide-index` to the new index and re-renders.  When
the agent is mid-thought and pushes a sequence of slides, the user's
frame races forward, leaving them no time to read.  Reviewing
becomes "ask the agent to slow down" rather than "navigate at my own
pace".

`add-presentation-layout-and-nav` introduces user-side keybindings
(`C-n`/`C-f`/`C-p`/`C-b`) that walk the deck.  This change inverts
the pacing: the agent prepares slides asynchronously; the user is
the only thing that moves the cursor through the deck; the agent
learns of the move via Claude Code's *channels* mechanism.

Claude Code channels (research preview) are MCP servers declaring an
experimental `claude/channel` capability.  When the server emits a
`notifications/claude/channel` event, it appears in the agent's
context as a `<channel source="…">` tag, ready for the agent to react
on its next turn.  No polling, no blocking await.

Constraints:
- The MCP server in `claude-code-ide-mcp.el` is third-party.  We
  must extend it without forking or vendoring.
- The notification path must not block.  Sending a notification from
  Emacs should be best-effort: if the server isn't connected, no-op.
- Channels are research preview.  The change must be feature-flagged
  or gracefully degrade so that users on older Claude Code (or with
  channels disabled) see no behaviour change.

## Goals / Non-Goals

**Goals:**
- `push_slide` defaults to non-disruptive: appends, validates,
  returns the new index; user's view unchanged.
- Opt-in `set_current` flag for the rare "drag the user here" case.
- User-driven navigation emits a single channel notification per
  movement, carrying enough metadata that the agent can plan the
  next push without calling `get_presentation` to round-trip.
- Agent-driven mutation does NOT fire the channel — the agent
  already knows.
- Graceful no-op when channels aren't available (older Claude Code,
  capability never registered, MCP not connected).

**Non-Goals:**
- A blocking `await_slide_change` MCP tool.  Channels obviate it.
- Notifying on slide *content* changes (replace_slide of a non-
  current index) — only the user's *position* in the deck matters
  for pacing.
- Supporting non-Claude-Code MCP clients.  The capability is Claude-
  Code-specific; clients without channel support simply ignore the
  notification.
- Wraparound, beep, or other UX flourishes on navigation; those
  belong with the keymap (in `add-presentation-layout-and-nav`).

## Decisions

### `push_slide` semantics flip (BREAKING)

Today `+presentation--deck-push` does:

```elisp
(+presentation--session-set key :deck new-deck)
(+presentation--session-set key :current-slide-index new-idx)
(+presentation--render-current key)
new-idx
```

After this change it does only the deck mutation + index return:

```elisp
(+presentation--session-set key :deck new-deck)
(when set-current
  (+presentation--session-set key :current-slide-index new-idx)
  (+presentation--render-current key))
new-idx
```

This is breaking for any agent that relied on push-as-navigate.  The
mitigation is: the only such agent is *us*, and the new model is
strictly more controllable.  Tool description for `push_slide`
documents the new behaviour clearly.

The first push against an empty session deck still does NOT auto-
render — even though there's nothing displayed yet, we keep the rule
simple ("push appends; only `start_presentation` initial_slide,
`goto_slide`, `replace_slide` of current, or `push_slide` with
`set_current: true` cause render").  The agent that wants the
first slide visible should pass `set_current: true` on that push, or
use `start_presentation`'s `initial_slide`.

### `set_current` opt-in (not the default)

The default is "don't disrupt the user".  `set_current: true` is
reserved for situations like:
- The user asked a question and the agent wants to focus the
  reader on a specific slide it just generated.
- The agent is correcting course mid-presentation and needs the
  user's eyes on the new content immediately.

If we made it default-true, we'd be back to today's noise.

### Channel capability registration via upstream patch (preferred) or local advice (fallback)

The `claude-code-ide-mcp.el` initialize-response handler hardcodes
capabilities at the source.  Two ways to inject `experimental.claude/
channel`:

**Option A — upstream patch** (preferred):
Add a `claude-code-ide-mcp-additional-capabilities` defcustom that
the initialize handler merges into its response.  Submit a small PR
to the upstream repo.  The presentation module sets the defcustom
at load-time:

```elisp
(with-eval-after-load 'claude-code-ide-mcp
  (setf (alist-get 'experimental claude-code-ide-mcp-additional-capabilities)
        '((claude/channel . :json-empty))))
```

**Option B — local advice** (fallback):
Advise `claude-code-ide-mcp--handle-initialize` (or whichever
function builds the capabilities alist) `:filter-return` to splice
in the channel capability.  Local-only; works without an upstream
release.

Lean toward A as the durable solution.  Ship B as the v0
implementation so this change isn't blocked on the upstream
maintainer; cut over to A once the PR lands.  Both produce
identical wire output.

### Channel notification timing & content

Fire the channel notification from inside
`+presentation-next-slide` / `+presentation-previous-slide`
*after* the deck-goto succeeds and renders.  Firing post-render
means the user has already seen the slide, so the agent reading
the channel event can assume the user is now looking at slide N.

Notification fields:

```
content: "User advanced to slide 3 of 7."  (or "retreated to ...")
meta.key:           "presentation-1981"
meta.current_slide: "3"
meta.prior_slide:   "2"
meta.kind:          "file"
meta.title:         "Deck mutation helpers"
```

`meta` values must be strings (the channel protocol restricts meta
to `Record<string, string>`).  `current_slide` and `prior_slide` are
the integer indices stringified.  `title` is omitted when the slide
has none — meta keys must be valid identifiers (letters/digits/
underscores), so `title` it is, but only when present.

Why include `kind` and `title`?  So the agent can react without a
round-trip to `get_presentation`/`get_deck`.  "User just moved to
the file slide" is more actionable than "user moved to slide 3"
when the agent is deciding what to push next.

### Agent-driven moves do NOT emit

`goto_slide`, `replace_slide` of the current index, `push_slide`
with `set_current: true` — all of these change the rendered slide
but originate from the agent.  Emitting a channel for these would
echo the agent's own action back to it, wasting context.

Implementation: notification emission lives in the user-facing nav
commands only.  `+presentation--deck-goto` (the underlying helper)
does not emit; the user-facing wrappers `+presentation-next-slide`
/ `-previous-slide` call the helper then emit.  Composes cleanly.

### Graceful degradation

`claude-code-ide-mcp--send-notification` (or its public equivalent)
is a private function but stable in practice.  When the MCP server
isn't connected, it logs and no-ops.  We wrap our emission call in
`condition-case` to swallow any error class, so navigation still
works when channels are unavailable, when the user is on an older
Claude Code, when the session has been disconnected, etc.

Capability registration is similarly best-effort: if the upstream
defcustom doesn't exist (advice fallback also not in place), the
capability isn't declared and the agent simply doesn't get
notifications.  No error.

### Research-preview surface area

Spec'd under the existing `presentation` capability, but the
notification path is gated by the user running Claude Code with
`--dangerously-load-development-channels server:<MCP server name>`
during the preview.  Document this prominently in
`modules/presentation/spec.md` and in the tool descriptions.

The MCP server name registered with claude-code-ide-mcp is
discoverable via the connection — we'll surface it in the spec
once confirmed (likely `claude-code-ide-mcp`).

## Risks / Trade-offs

- **Breaking change to `push_slide`.** → The only consumer of the
  old behaviour is us (and any docs we wrote).  The transition is
  one-shot: bump the tool description, update agent prompts, done.

- **Upstream patch requires maintainer cycle.** → Ship local
  `advice` first; cut to upstream when accepted.  No user-visible
  regression in either path.

- **Research-preview gating.** Channels require a specific Claude
  Code version, claude.ai login, and a `--dangerously-load-
  development-channels` flag.  → Documented prominently;
  presentation flow still functions without channels (just no
  user-move notifications), so the experience degrades smoothly to
  "agent must call get_presentation if it wants to know where the
  user is".

- **Notification volume.** A user mashing `C-n` through a deck
  could fire many notifications quickly.  → Each is small (~150
  bytes content + meta).  If volume becomes a problem, debounce in
  the elisp emission path.  Not anticipated for normal use.

- **Channel format stability.** The protocol is research preview;
  field names or wire format could change.  → Notification
  emission is one function (`+presentation--emit-nav-channel`);
  cheap to update if the format shifts.  We monitor the
  `claude-code/channels-reference` doc for changes.

- **Private function dependency.**
  `claude-code-ide-mcp--send-notification` is technically internal.
  → If renamed, our `condition-case` makes the call a no-op rather
  than a crash.  We pin a specific upstream version range in the
  module's `packages.eld` and bump after testing.
