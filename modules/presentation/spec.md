# Presentation

Buffer-local minor-mode walk-through over a markdown document. Top-level H1
headings are slides; navigation narrows to the heading region containing
point. Markdown link forms inside the document render as inline previews
when the mode is on, with click-to-real-buffer escape hatches.

## Files

| File           | Purpose                                                 |
| :------------- | :------------------------------------------------------ |
| `init.el`      | Loads `lib.el`; nothing else                            |
| `lib.el`       | Mode, navigation, link parsing/dispatch, previews       |
| `tests.el`     | ERT tests covering helpers, mode, links, revert, render |
| `packages.eld` | Empty (no external deps)                                |

## External Packages

None at load time. `magit` is required only at click time on a `diff:` link;
checked via `(require 'magit nil t)` and signals `user-error` on absence.

## API

### Public elisp surface

| Symbol                          | Purpose                                          |
| :------------------------------ | :----------------------------------------------- |
| `+present-markdown FILE`        | Open FILE and enable `+presentation-mode`        |
| `+presentation-mode`            | Buffer-local minor mode (narrows on enable)      |
| `+presentation-next-slide`      | Advance to next H1 (silent no-op at last)        |
| `+presentation-previous-slide`  | Retreat to prior H1 (silent no-op at first)      |
| `+presentation-quit`            | Disable mode; bury (or kill, when owned)         |
| `+presentation-follow-link`     | Dispatch link at point (heading / src / diff)    |
| `+presentation-focus-face`      | Face for focus highlight in narrowed source      |

There are no MCP tools. The single agent invocation pattern is:

```
emacsclient -t -e '(+present-markdown "/abs/path/doc.md")'
```

Tmux pane geometry is the caller's concern.

### Slide model

A slide is the buffer region from one `^# ` line up to (but not including)
the next `^# ` line, or `point-max`. Sub-headings (`^##` etc.) flow inside
the slide; HR lines (`^---`) do not break. Heading detection ignores fenced
code blocks (delimited by `^[ \t]*\`\`\`` lines).

### Link forms

The follow-link command (bound to `RET` and reachable from the standard
markdown-mode click bindings) dispatches by URL form:

| URL form                                | Action                                        |
| :-------------------------------------- | :-------------------------------------------- |
| `#<slug>`                               | Push mark; widen + re-narrow to enclosing H1  |
| `<path>#L<a>` / `<path>#L<a>-L<b>`      | Push mark; `find-file`; narrow + focus        |
| `diff:<base>...<head>[#<path>]`         | Push mark; `magit-diff-range`                 |
| Anything else                           | Pass through to markdown-mode default         |

Slug equality uses `+presentation--heading-slug`: lowercase, runs of
non-alphanumeric → single hyphen, strip ends. First heading whose slug
matches wins.

### Inline previews

When the mode is on, every link in the current narrowing whose URL matches
a previewable form is covered by an overlay whose `display` property is a
fenced code block. Source-range previews show up to 10 lines from the file
(with a `+N more lines · click to open` footer when truncated); diff
previews show up to 10 lines of `git diff base...head [-- path]`. Overlays
use `display` only — saving the buffer writes the original markdown.

Previews refresh on slide entry only:

- `+presentation-mode` enable
- `+presentation-next-slide` / `+presentation-previous-slide`
- Heading-text in-doc link follow
- After-revert (auto-revert on disk change)

There are no file-watchers or idle timers.

### Click escapes

- Source-range click: `find-file` (via `pop-to-buffer`, honouring
  `other-window-prefix`); applies a focus overlay over the requested range
  via `+presentation--render-narrowed-source`; sets `buffer-read-only` and
  installs a `kill-buffer-hook` restorer; `+presentation-mode` is NOT
  enabled in the destination.
- Diff-range click: `(require 'magit nil t)`; on failure, signal a
  `user-error`. On success, call `magit-diff-range "B...H"` (with file-args
  list when scoped).

### Revert resilience

While `+presentation-mode` is on, buffer-local `before-revert-hook` and
`after-revert-hook` capture / restore narrowing context.

`before-revert-hook` records into `+presentation--revert-anchor` a plist
with `:slug` (current H1's slug), `:index` (0-based ordinal among H1s),
`:fingerprint` (≤80 chars starting at point), and `:window-start-offset`
(`(- (window-start) (point))`).

`after-revert-hook` restores in priority order: slug match → ordinal index
(clamped to last H1) → first H1 → leave widened. Then it searches for the
fingerprint inside the new narrowing to restore point and `window-start`,
falling back to start-of-narrowing on miss. Finally it rebuilds preview
overlays.

### Reusable narrowed-source renderer

`+presentation--render-narrowed-source BUFFER START-LINE END-LINE
&optional FOCUS-START FOCUS-END` is the workhorse behind source-range
clicks. It narrows the buffer, applies one focus overlay per line bounded
to text glyphs (no `:extend t`), sets `buffer-read-only`, and installs a
`kill-buffer-hook` restorer. Re-applying on the same buffer first runs the
prior restorer.

## Keymap

`+presentation-mode-map` (defined via `defvar-keymap`) is installed via
`minor-mode-overriding-map-alist` so its bindings beat evil's state maps;
additionally registered with `evil-make-overriding-map` and
`evil-define-key*` for `(normal motion visual)`. Keys:

| Key            | Command                          |
| :------------- | :------------------------------- |
| `C-n` / `C-f`  | `+presentation-next-slide`       |
| `C-p` / `C-b`  | `+presentation-previous-slide`   |
| `C-c q`        | `+presentation-quit`             |
| `RET`          | `+presentation-follow-link`      |
