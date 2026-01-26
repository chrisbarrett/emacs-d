# lang-markdown

Markdown and GitHub Flavored Markdown (GFM) editing support with visual
enhancements, smart formatting, and callout highlighting.

## Files

| Path                      | Purpose                        |
| :------------------------ | :----------------------------- |
| `init/init-markdown.el`   | Mode setup, GFM callouts, TAB  |
| `templates/markdown.eld`  | Tempel snippets for GFM        |

## Dependencies

- **External:** markdown-mode, apheleia
- **Internal:** +corelib

## Behavior

### Mode Remapping

| Pattern             | Mode     | Notes                     |
| :------------------ | :------- | :------------------------ |
| `*.md`              | gfm-mode | Via major-mode-remap-alist |
| `/prompt` (at end)  | gfm-mode | Claude prompt files       |

GFM mode is preferred over basic markdown-mode for GitHub compatibility.

### Display Settings

| Setting                            | Value | Effect                         |
| :--------------------------------- | :---- | :----------------------------- |
| markdown-fontify-code-blocks-natively | t   | Syntax highlight fenced blocks |
| markdown-hide-urls                 | t     | Hide inline URL targets        |

Visual-line-mode enabled in gfm-mode for soft wrapping.

### GitHub Flavored Markdown Callouts

Font-lock highlighting for GFM alert/callout syntax:

| Callout     | Face                                 | Inherits From        |
| :---------- | :----------------------------------- | :------------------- |
| `[!NOTE]`   | +markdown-gfm-callout-note-face      | font-lock-operator   |
| `[!TIP]`    | +markdown-gfm-callout-tip-face       | font-lock-keyword    |
| `[!IMPORTANT]` | +markdown-gfm-callout-important-face | font-lock-warning |
| `[!WARNING]` | +markdown-gfm-callout-warning-face  | warning              |
| `[!CAUTION]` | +markdown-gfm-callout-caution-face  | error                |
| `[!CRITICAL]` | +markdown-gfm-callout-caution-face | error                |

Additionally, `<!-- prettier-ignore-start -->` and `<!-- prettier-ignore-end -->`
comments are dimmed via `+markdown-prettier-ignore-comment-face`.

### TAB Key Behavior

In insert state, TAB calls `+markdown-tab-dwim`:
1. Attempt tempel snippet expansion
2. Fall back to `markdown-cycle` (fold/unfold headings)

## API

### Commands

| Command                    | Description                        |
| :------------------------- | :--------------------------------- |
| +markdown-tab-dwim         | Smart TAB with snippet/cycle       |
| +markdown-fontify-gfm-callouts | Add callout font-lock keywords |

### Faces

| Face                                    | Purpose                   |
| :-------------------------------------- | :------------------------ |
| +markdown-gfm-callout-note-face         | [!NOTE] callout marker    |
| +markdown-gfm-callout-tip-face          | [!TIP] callout marker     |
| +markdown-gfm-callout-important-face    | [!IMPORTANT] marker       |
| +markdown-gfm-callout-warning-face      | [!WARNING] marker         |
| +markdown-gfm-callout-caution-face      | [!CAUTION]/[!CRITICAL]    |
| +markdown-prettier-ignore-comment-face  | prettier-ignore comments  |

### Keybindings

| Key       | State  | Keymap            | Command                     |
| :-------- | :----- | :---------------- | :-------------------------- |
| TAB       | insert | markdown-mode-map | +markdown-tab-dwim          |
| TAB       | insert | gfm-mode-map      | +markdown-tab-dwim          |
| C-c f     | all    | markdown-mode-map | markdown-insert-footnote    |
| SPC n s   | normal | markdown-mode-map | markdown-narrow-to-subtree  |
| , l       | normal | (local leader)    | markdown-toggle-url-hiding  |
| , f       | normal | (local leader)    | markdown-insert-footnote    |

## Formatting

Formatter selection via apheleia based on deno availability:

| Formatter        | Command                                    | Used When        |
| :--------------- | :----------------------------------------- | :--------------- |
| deno-markdown    | `deno fmt --prose-wrap always --ext=md -`  | deno available   |
| prettier-markdown| `prettier --parser=markdown --prose-wrap always` | fallback  |

Both formatters:
- Wrap prose at fill-column
- Handle YAML frontmatter
- Deno handles GFM callouts better

## Templates

11 Tempel snippets in `templates/markdown.eld`:

| Key  | Expansion                         |
| :--- | :-------------------------------- |
| s    | Fenced code block                 |
| js   | JavaScript code block             |
| ts   | TypeScript code block             |
| sh   | Shell code block                  |
| nb   | [!NOTE] callout                   |
| note | [!NOTE] callout (alias)           |
| tip  | [!TIP] callout                    |
| imp  | [!IMPORTANT] callout              |
| wa   | [!WARNING] callout                |
| ca   | [!CAUTION] callout                |

## Testable Properties

1. Opening `*.md` file activates gfm-mode (not markdown-mode)
2. Opening file ending in `/prompt` activates gfm-mode
3. `markdown-fontify-code-blocks-natively` is t
4. `[!NOTE]` in GFM buffer has +markdown-gfm-callout-note-face
5. TAB in insert state expands tempel snippets before markdown-cycle
6. `apheleia-formatter` set to deno-markdown when deno available
7. C-c f calls markdown-insert-footnote
8. Local leader `l` toggles URL hiding
9. 11 tempel snippets available in markdown-mode
