# Diff Feature Spec

Diff viewing and interactive merge tools.

## Files

| File              | Purpose                               |
| :---------------- | :------------------------------------ |
| `init/init-diff.el` | diff and ediff configuration         |

## External Packages

None. Uses built-in `diff` and `ediff` packages.

## Behaviors

### Diff Mode

| Setting                     | Value        | Effect                           |
| :-------------------------- | :----------- | :------------------------------- |
| `diff-default-read-only`    | t            | Diff buffers are read-only       |
| `diff-advance-after-apply-hunk` | t        | Move to next hunk after apply    |
| `diff-font-lock-prettify`   | t            | Enhanced visual display          |
| `diff-font-lock-syntax`     | `hunk-also`  | Syntax highlighting in hunks     |

### Ediff Mode

| Setting                      | Value                          | Effect                           |
| :--------------------------- | :----------------------------- | :------------------------------- |
| `ediff-diff-options`         | `-w`                           | Ignore whitespace differences    |
| `ediff-split-window-function`| `split-window-horizontally`    | Side-by-side comparison          |
| `ediff-window-setup-function`| `ediff-setup-windows-plain`    | No separate control frame        |
| `ediff-show-clashes-only`    | t                              | Show only conflicting regions    |

### Org-Mode Integration

When navigating ediff hunks in org-mode buffers:
- `ediff-next-difference` reveals org heading around hunk
- `ediff-previous-difference` reveals org heading around hunk
- Works on all three ediff buffers (A, B, C)
- Calls `org-reveal` with siblings visible

## API

### Advice Functions

| Function                                    | Advised              | When   |
| :------------------------------------------ | :------------------- | :----- |
| `+ad-ediff-reveal-org-content-around-hunk`  | `ediff-next-difference` | :after |
| `+ad-ediff-reveal-org-content-around-hunk`  | `ediff-previous-difference` | :after |

## Testable Properties

1. `diff-default-read-only` is t
2. `diff-font-lock-syntax` is `hunk-also`
3. `ediff-diff-options` contains `-w`
4. `ediff-split-window-function` is `split-window-horizontally`
5. `ediff-window-setup-function` is `ediff-setup-windows-plain`
6. `ediff-show-clashes-only` is t
7. `ediff-next-difference` has advice `+ad-ediff-reveal-org-content-around-hunk`
