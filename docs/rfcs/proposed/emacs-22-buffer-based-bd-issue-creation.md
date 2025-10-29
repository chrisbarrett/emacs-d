---
title: "RFC: emacs-22 - Buffer-Based Input System for BD Issue Creation"
date: 2025-10-29
author: Chris Barrett
agent: report-writer
ticket_id: emacs-22
tags: [rfc, emacs, beads, worktree, ux]
---

# RFC: emacs-22 - Buffer-Based Input System for BD Issue Creation

> Ticket: emacs-22 - Status: Proposed

## Abstract

This RFC proposes a buffer-based interface for creating beads (bd) issues
directly from within Emacs. The system will provide a git-commit-like editing
experience, integrate with the existing worktree transient menu, and use Claude
Code CLI with the beads MCP server to create issues programmatically. Users will
compose issue details in a dedicated buffer with helpful prompts, then press C-c
C-c to create the issue or C-c C-k to cancel.

## Motivation

### Problem Statement

Currently, creating bd issues requires dropping to a shell and invoking the `bd`
CLI:

```bash
bd create "Issue title" --type feature --priority 1 --description "Details..."
```

This workflow has several pain points:

1. **Context switching**: Users must leave Emacs and their current workflow to
   create issues
2. **Poor multi-line input**: Shell commands are awkward for composing
   multi-line descriptions with markdown formatting
3. **No editing support**: Once entered, the command must be retyped if there
   are mistakes
4. **Breaks flow**: Interrupts the development workflow unnecessarily

### Why This Matters

The project emphasizes using beads for all task tracking (as documented in
CLAUDE.md). However, the friction of creating issues via CLI discourages
developers from capturing work items promptly, leading to:

- Forgotten tasks and lost context
- Reluctance to break down work into smaller issues
- Incomplete issue tracking despite having good tooling

By providing a comfortable, Emacs-native interface, we can:

- Reduce friction for issue creation
- Encourage better task decomposition
- Maintain workflow continuity
- Leverage Emacs' editing capabilities for issue descriptions

### Use Cases

1. **Developer working on a feature branch** discovers a bug and wants to
   quickly file it without losing context
2. **Developer in a worktree tab** wants to create a follow-up issue while
   reviewing completed work
3. **Developer composing a complex issue** needs to write a detailed description
   with markdown formatting and code examples
4. **Power user** wants to leverage Emacs' text editing features (yasnippet,
   auto-fill, spell-check) when writing issue descriptions

## Proposal

### Overview

Implement a major mode (`+bd-issue-mode`) that provides a buffer-based interface
for creating bd issues. The interface will:

- Open a dedicated buffer with a template for issue fields (title, type,
  priority, description)
- Provide intuitive keybindings (C-c C-c to create, C-c C-k to cancel)
- Parse buffer contents to extract structured issue data
- Invoke Claude Code CLI with the beads MCP server to create the issue
- Display success/error feedback to the user

The system will integrate with the existing worktree transient menu, making
issue creation a single keypress away from the worktree workflow.

### Key Design Decisions

<!-- prettier-ignore-start -->
> [!NOTE]
> **Design Decision**: Free-form buffer input with parsing
>
> **Reasoning**: Rather than forcing users to fill in structured forms or prompts, we allow them to compose issue details naturally in a buffer with helpful field markers. The system parses the buffer contents to extract fields. This approach:
> - Feels familiar to Emacs users (similar to git-commit-mode)
> - Allows flexible editing and composition
> - Supports multi-line descriptions with markdown
> - Enables use of Emacs text editing features (snippets, templates, auto-fill)
>
> **Confidence**: High
>
> **Alternatives considered**: Completing-read prompts (poor for multi-line input), structured forms (less flexible), org-capture integration (too complex for this use case)
<!-- prettier-ignore-end -->

<!-- prettier-ignore-start -->
> [!NOTE]
> **Design Decision**: Claude Code CLI for issue creation
>
> **Reasoning**: Rather than invoking `bd` CLI directly, we use Claude Code with the beads MCP server. This approach:
> - Leverages existing MCP infrastructure for agent integration
> - Provides AI-assisted issue creation and validation
> - Enables future enhancements (e.g., "analyze this error and create an issue")
> - Aligns with the project's Claude-first workflow
>
> **Confidence**: High
>
> **Alternatives considered**: Direct `bd` CLI invocation (bypasses MCP benefits), direct MCP protocol implementation (too complex)
<!-- prettier-ignore-end -->

## Design Details

### Architecture

The system consists of four main components:

```
┌──────────────────────────────────────────────────────────────┐
│  Worktree Transient Menu                                     │
│  ["New Issue" key] ──────────┐                              │
└──────────────────────────────┼───────────────────────────────┘
                               │
                               ▼
┌──────────────────────────────────────────────────────────────┐
│  BD Issue Input Buffer (+bd-issue-mode)                     │
│  - Template with field prompts                               │
│  - C-c C-c / C-c C-k keybindings                            │
└──────────────────────────────┬───────────────────────────────┘
                               │
                   ┌───────────┴───────────┐
                   │                       │
                   ▼                       ▼
         C-c C-c (Accept)          C-c C-k (Cancel)
                   │                       │
                   ▼                       │
┌──────────────────────────────────────────┼───────────────────┐
│  Claude Code Integration                 │                   │
│  - Parse buffer → Build MCP request      │                   │
│  - Invoke `claude` CLI                   │                   │
└──────────────────────────────────────────┼───────────────────┘
                   │                       │
                   ▼                       ▼
            Issue Created            Buffer Killed
```

### Component Responsibilities

#### 1. BD Issue Input Buffer

**Purpose**: Provide editing environment for composing issue details

**Key Features**:

- Derived from `text-mode` for basic editing
- Blank space for natural language input
- Help text in comments (stripped before sending to Claude)
- Buffer-local variables to store context (worktree path)
- Kill-buffer protection to prevent accidental closure

**Template Structure** (git-commit style):

```markdown


# Please describe the issue you want to create. Lines starting
# with '#' will be ignored.
#
# Just write naturally - Claude will interpret your description
# to create the issue with an appropriate title, type, and priority.
#
# Examples:
#   high priority bug: icons not rendering consistently on work macbook
#   feature: add markdown preview support
#   fix the compilation error in mod-evil.el
#
# C-c C-c to create issue
# C-c C-k to cancel
```

#### 2. Accept/Cancel Handlers

**Purpose**: Process or discard buffer contents

**Accept Handler** (`+bd-issue-finish`):

1. Parse buffer to extract fields
2. Validate required fields (title minimum)
3. Call Claude Code integration
4. Display success/error message
5. Clean up buffer and restore window configuration

**Cancel Handler** (`+bd-issue-cancel`):

1. Confirm cancellation with user
2. Kill buffer without creating issue
3. Restore window configuration

#### 3. Claude Code Integration

**Purpose**: Create bd issue via Claude Code MCP

**Implementation**:

- Constructs natural language prompt for Claude with issue details
- Writes prompt to temporary file (avoids shell escaping issues)
- Invokes `claude` CLI with `--no-input` flag
- Captures and displays output or errors
- Sets `default-directory` to worktree path for correct context

**Example prompt**:

```
Please use the beads MCP server to create a new issue with these details:

Title: Add buffer-based input for bd issues
Type: feature
Priority: 1
Description:
Create a comfortable way to create bd issues from Emacs.
Should follow git-commit-mode patterns for UX.

Use the mcp__beads__create_issue function to create this issue.
Just create the issue and confirm it was created - no other output needed.
```

#### 4. Worktree Transient Integration

**Purpose**: Provide easy access from worktree workflow

**Integration Point**: Add "New issue" command to worktree transient menu

- Automatically captures current worktree context
- Single key press to open issue creation buffer

### Data Model

**Parsed Issue Structure** (Elisp alist):

```elisp
'((title . "Issue title string")
  (type . "feature")           ; one of: bug, feature, task, epic, chore
  (priority . 2)                ; integer 0-4
  (description . "Multi-line\ndescription text"))
```

**Field Validation Rules**:

- **Title**: Required, non-empty string
- **Type**: Must be one of: bug, feature, task, epic, chore
- **Priority**: Must be integer 0-4
- **Description**: Optional, markdown supported

### User Experience Flow

1. User invokes command from worktree transient (press 'n') or directly
2. Buffer opens with template, point at title field
3. User fills in issue details using normal Emacs editing
4. User presses C-c C-c to create issue
5. System parses buffer and validates fields
6. Claude Code invoked to create issue via MCP
7. Success message shown: "✓ Issue created: [title]"
8. Buffer closed, window configuration restored
9. User continues work in worktree

Alternative flow (cancel):

- User presses C-c C-k at any time
- Confirmation prompt shown
- Buffer discarded without creating issue

## Implementation Plan

### Phase 1: Core Functionality (MVP)

1. Create `/lisp/+bd-issue.el` file
2. Implement `+bd-issue-mode` major mode with basic keybindings
3. Implement `+bd-issue-create` entry point
4. Implement buffer template with field prompts
5. Implement `+bd-issue--parse-buffer` function with validation
6. Implement `+bd-issue-finish` (C-c C-c handler)
7. Implement `+bd-issue-cancel` (C-c C-k handler)
8. Implement `+bd-issue--create-via-claude` integration
9. Add command to worktree transient menu
10. Test basic workflow manually

**Estimated effort**: 4-6 hours

### Phase 2: Polish and Error Handling

1. Add comprehensive field validation with helpful error messages
2. Add kill-buffer-query protection
3. Add window configuration restoration
4. Add error buffer for Claude CLI failures (`*bd-issue-errors*`)
5. Write ERT tests for parsing logic
6. Test error cases (missing Claude, bad input, MCP failures)
7. Add user-facing documentation (docstrings)

**Estimated effort**: 2-3 hours

### Phase 3: Optional Enhancements

These features can be added later based on user feedback:

1. **Posframe support**: Display buffer in floating child frame for modal-like
   UX
2. **Evil-mode keybindings**: Add `:wq` (accept) and `:q!` (cancel) for Evil
   users
3. **Type-specific templates**: Different templates for bug reports vs feature
   requests
4. **Async issue creation**: Use `start-process` to avoid blocking Emacs
5. **Template customization**: Allow users to customize the buffer template

**Estimated effort**: 1-2 hours per enhancement

### Testing Strategy

#### Unit Tests

```elisp
;; Test parsing valid buffer
(ert-deftest +bd-issue-test-parse-valid ()
  (with-temp-buffer
    (insert "# Title: Test issue\n")
    (insert "# Type: feature\n")
    (insert "# Priority: 2\n\n")
    (insert "This is a description.\n")
    (let ((parsed (+bd-issue--parse-buffer)))
      (should (equal (alist-get 'title parsed) "Test issue"))
      (should (equal (alist-get 'type parsed) "feature"))
      (should (= (alist-get 'priority parsed) 2)))))

;; Test missing required field
(ert-deftest +bd-issue-test-parse-missing-title ()
  (with-temp-buffer
    (insert "# Type: feature\n")
    (should-error (+bd-issue--parse-buffer))))

;; Test invalid type
(ert-deftest +bd-issue-test-parse-invalid-type ()
  (with-temp-buffer
    (insert "# Title: Test\n")
    (insert "# Type: invalid-type\n")
    (insert "# Priority: 2\n")
    (should-error (+bd-issue--parse-buffer))))
```

#### Integration Testing

Manual testing checklist:

1. Open worktree transient and invoke "New issue" command
2. Verify buffer opens with template and point at title field
3. Fill in valid data and press C-c C-c
4. Verify issue created in bd (`bd list --json`)
5. Verify success message shown
6. Verify buffer killed and window config restored
7. Test cancel with C-c C-k
8. Test validation errors (missing title, invalid type, etc.)
9. Test with invalid worktree path
10. Test with missing Claude CLI or MCP server

### Prerequisites

Before implementation can begin:

1. **Claude Code CLI** must be installed and accessible in PATH
2. **Beads MCP server** must be installed: `pip install beads-mcp`
3. **MCP configuration** must exist in `~/.config/claude/config.json`:
   ```json
   {
     "beads": {
       "command": "beads-mcp",
       "args": []
     }
   }
   ```
4. **Worktree must be initialized** for beads: `bd init`

### Deployment

**File Location**: `/Users/chris/.config/emacs/lisp/+bd-issue.el` (new file)

**Integration**: Add `(require '+bd-issue)` to `modules/mod-worktrees.el`

**No package dependencies**: Pure Elisp implementation using only built-in
features

## Alternatives Considered

### Alternative 1: Direct `bd` CLI Invocation

**Description**: Instead of using Claude Code MCP, directly invoke `bd create`
command.

**Implementation**:

```elisp
(defun +bd-issue--create-direct (issue-data worktree-path)
  (let ((default-directory worktree-path))
    (call-process "bd" nil nil nil
                  "create"
                  (alist-get 'title issue-data)
                  "--type" (alist-get 'type issue-data)
                  "--priority" (number-to-string (alist-get 'priority issue-data))
                  "--description" (alist-get 'description issue-data))))
```

**Pros**:

- Simpler implementation (no Claude CLI dependency)
- Faster execution (no LLM overhead)
- More predictable (direct command)

**Cons**:

- Bypasses MCP server (duplicates functionality)
- Loses benefits of agent-optimized tracking
- No AI-assisted validation or enhancement
- Misses opportunity for future AI features

**Why not chosen**: The project heavily uses Claude Code and has beads MCP
server specifically for agent integration. Using Claude Code aligns with the
existing workflow and enables future enhancements like "Claude, analyze this
error and create an issue for it."

### Alternative 2: Using `with-editor` Library

**Description**: Use the `with-editor` package (like `git-commit` does) for
accept/cancel pattern.

**Pros**:

- Battle-tested pattern from Magit
- Handles edge cases well
- Familiar to git-commit users

**Cons**:

- Requires external package dependency
- Designed for editor→subprocess communication (we're doing the opposite)
- Over-engineered for our simpler use case

**Why not chosen**: Our use case is simpler than git-commit. We're calling a
subprocess, not being called as an editor. A simple buffer + keybindings
approach is sufficient and avoids unnecessary dependencies.

### Alternative 3: Completing-Read Interface

**Description**: Use `completing-read` with prompts for each field instead of a
buffer.

**Implementation**:

```elisp
(defun +bd-issue-create-prompt ()
  (interactive)
  (let* ((title (read-string "Title: "))
         (type (completing-read "Type: " '("bug" "feature" "task" "epic" "chore")))
         (priority (read-number "Priority (0-4): " 2))
         (description (read-string "Description (optional): ")))
    ;; Create issue...
    ))
```

**Pros**:

- Very simple implementation
- Fast for simple issues
- No buffer management needed

**Cons**:

- Poor UX for multi-line descriptions
- No markdown support
- Can't see all fields at once
- Difficult to edit/correct mistakes

**Why not chosen**: Multi-line descriptions with markdown are common for issues.
A buffer-based approach provides much better UX and is more familiar to Emacs
users (similar to git commits, org capture, etc.).

### Alternative 4: Org-Capture Integration

**Description**: Use org-capture templates to create issues.

**Pros**:

- Leverages existing org-mode infrastructure
- Power users already familiar with org-capture
- Rich template system

**Cons**:

- Requires org-mode knowledge
- More complex setup
- Doesn't integrate as cleanly with worktree workflow
- Overkill for simple issue creation

**Why not chosen**: While org-capture is powerful, it's too heavyweight for this
use case. A purpose-built interface is simpler and more accessible to all users,
not just org-mode experts.

## Trade-offs and Considerations

### Performance Impact

- **Buffer creation**: Instantaneous (simple template insertion)
- **Parsing**: O(n) where n is buffer size, typically <1KB, negligible
- **Issue creation**: Synchronous call to `claude` CLI, typically 1-3 seconds
  - Acceptable for occasional use
  - Could be made async if users report blocking issues

### Security Implications

- **Command injection**: Using temp file for prompt avoids shell escaping
  vulnerabilities
- **File permissions**: Temp files created with default umask
- **No credential exposure**: No credentials stored or passed
- **Dependency trust**: Relies on Claude CLI and beads-mcp (both trusted tools
  from this project's ecosystem)

### Compatibility

- **Emacs version**: Requires Emacs 27+ (minimum for project)
- **Platform**: Cross-platform (macOS, Linux, Windows with proper shell)
- **Terminal vs GUI**: Works in both
- **Evil mode**: Uses Emacs-style keybindings (C-c C-c, C-c C-k)
  - Could add Evil bindings later if requested

### Maintenance

- **Low maintenance burden**: Pure Elisp, no complex dependencies
- **Easy to test**: ERT tests for parsing, manual tests for integration
- **Self-documenting**: Code has docstrings and follows project conventions
- **Extensible**: Easy to add features (templates, async, posframe)
  incrementally

## Open Questions

1. **Should we add Evil-mode keybindings** (`:wq`, `:q!`) in addition to Emacs
   ones?
   - **Context**: Configuration uses Evil mode, but follows Emacs conventions
     for special modes
   - **Recommendation**: Start with Emacs bindings, add Evil if users request it

2. **Should issue creation be asynchronous** to avoid blocking Emacs during
   Claude CLI call?
   - **Context**: Claude CLI calls take 1-3 seconds
   - **Recommendation**: Start synchronous (simpler), add async if users report
     problems

3. **Should we add issue templates** for different types (bug report, feature
   request)?
   - **Context**: Different issue types might benefit from different template
     structures
   - **Recommendation**: Start with single template, add type-specific templates
     based on usage patterns

4. **What is the exact MCP function name** for creating beads issues?
   - **Context**: Documentation doesn't specify the exact function name
   - **Recommendation**: Test interactively with
     `claude "Use beads MCP to create a test issue"` and update prompt
     accordingly

## Timeline

- **Phase 1 (MVP)**: 1 week
- **Phase 2 (Polish)**: 3-5 days
- **Phase 3 (Optional)**: As needed based on feedback

**Total estimated time**: 2-3 weeks for complete implementation

## References

### Official Documentation

- [Emacs Lisp Reference: Modes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Modes.html)
- [Emacs Lisp Reference: Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html)

### Source Code References

- [git-commit.el (Magit)](https://github.com/magit/magit/blob/main/lisp/git-commit.el) -
  Pattern for buffer-based input with C-c C-c/C-c C-k
- [with-editor.el](https://github.com/magit/with-editor/blob/main/lisp/with-editor.el) -
  Accept/cancel pattern for editor integration

### Related Documentation

- [Beads GitHub](https://github.com/steveyegge/beads) - Issue tracker
  integration
- [Claude Code IDE](https://github.com/manzaltu/claude-code-ide.el) - Existing
  Claude integration in this configuration
- [CLAUDE.md](../../CLAUDE.md) - Project-specific beads workflow documentation

## Revision History

See git history for document changes. Use
`git log -- docs/rfcs/proposed/emacs-22-buffer-based-bd-issue-creation.md` to
view revision history.
