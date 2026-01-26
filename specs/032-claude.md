# Claude Code IDE Integration

Claude Code runs inside Emacs with MCP bridge for editor integration.

## Files

| File                  | Purpose                                |
| :-------------------- | :------------------------------------- |
| `init/init-claude.el` | claude-code-ide package, MCP, env vars |

## External Packages

| Package        | Purpose                        |
| :------------- | :----------------------------- |
| claude-code-ide | Emacs integration for Claude Code |

## Dependencies

- eat (terminal backend)
- evil (buffer exclusion)
- +corelib
- +first-input-hook, +first-file-hook, +switch-window-hook, +switch-buffer-hook

## Behaviors

### Terminal Backend

| Setting                          | Value |
| :------------------------------- | :---- |
| claude-code-ide-terminal-backend | eat   |

### MCP Server

| Aspect              | Behavior                          |
| :------------------ | :-------------------------------- |
| MCP server enabled  | t (claude-code-ide-enable-mcp-server) |
| IDE diff            | Disabled (too interruptive)       |
| Emacs tools         | Registered via claude-code-ide-emacs-tools-setup |

### Evil Mode Integration

Evil is disabled in claude-code-ide buffers:

```elisp
(pushnew! evil-buffer-regexps `(,(rx bol "*claude-code")))
```

Buffer name pattern: `*claude-code*`

### Auto-Scroll

Claude-code-ide buffers auto-scroll to bottom on:
- Window switch (+switch-window-hook)
- Buffer switch (+switch-buffer-hook)

Implementation:
1. Iterate visible windows
2. If buffer matches `*claude-code*` pattern
3. Move point to end
4. Recenter with -1 (bottom)

### Non-Breaking Space Display

Non-breaking space characters in Claude Code prompts are hidden:
- Hook: eat-exec-hook
- Face remap: nobreak-space inherits default face
- Only applies to claude-code-ide buffers

### Mise Environment Integration

When `.mise.toml` exists in project:
1. Check for mise executable
2. Run `mise env` to get environment variables
3. Prepend to `process-environment` for terminal session

This ensures Claude Code sessions inherit project-specific tool versions.

## API

### Functions

| Function                            | Purpose                              |
| :---------------------------------- | :----------------------------------- |
| +claude-code-ide-active-buffer-p    | Predicate for claude-code buffers    |
| +claude-code-ide-scroll-to-bottom-h | Scroll visible claude buffers        |
| +eat-remap-nbsp                     | Hide non-breaking space in eat       |

### Buffer Pattern

```elisp
(rx bol "*claude-code")
```

## Testable Properties

1. claude-code-ide-terminal-backend equals eat
2. claude-code-ide-enable-mcp-server is t
3. claude-code-ide-use-ide-diff is nil
4. evil-buffer-regexps includes `*claude-code*` pattern
5. +switch-window-hook includes +claude-code-ide-scroll-to-bottom-h
6. +switch-buffer-hook includes +claude-code-ide-scroll-to-bottom-h
7. eat-exec-hook includes +eat-remap-nbsp
