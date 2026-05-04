## Effect-interpreter for tmux

All tmux interaction is modelled as data. Pure planning functions return
lists of effects; a single runner executes them. Tests assert on the
emitted effects.

```elisp
(cl-defstruct +presentation-effect-shell
  "A shell command to execute. ARGV is a list of strings (no shell quoting)."
  argv)

(cl-defstruct +presentation-effect-elisp
  "Internal state mutation. THUNK is a zero-arg function."
  thunk)

;; Pure: builds the effect list for a "create new pane" path
(defun +presentation--plan-spawn (session window split socket)
  ...)

;; Impure: the only place that calls `process-file' / `make-process'
(defun +presentation--run-effects (effects)
  ...)
```

Tmux commands the planner emits:

| Purpose          | argv                                                                         |
| :--------------- | :--------------------------------------------------------------------------- |
| List panes       | `tmux list-panes -t SESS:WIN -F #{pane_id}\t#{pane_tty}`                     |
| Split (h)        | `tmux split-window -h -t SESS:WIN -- emacsclient -t -s SOCK`                 |
| Split (v)        | `tmux split-window -v -t SESS:WIN -- emacsclient -t -s SOCK`                 |
| Kill pane        | `tmux kill-pane -t PANE_ID`                                                  |

`list-panes` output is parsed in a separate pure function and joined against
`(frame-list)` filtered on `(frame-parameter f 'tty)`.

## State

```elisp
(defvar +presentation--sessions (make-hash-table :test 'equal)
  "Map session-key (string) -> plist.")
```

Plist shape:

| Key             | Type                | Notes                                  |
| :-------------- | :------------------ | :------------------------------------- |
| `:frame`        | frame               | The presentation frame                 |
| `:origin`       | `created` `reused`  | Determines tear-down path              |
| `:saved-config` | window-configuration | Only for `reused`; nil for `created`  |
| `:tmux-pane`    | string              | Pane id from tmux (only for `created`) |
| `:worktree`     | string (path)       | From the start payload                 |
| `:started-at`   | float-time          | Diagnostics                            |

The frame itself carries:

```elisp
(modify-frame-parameters frame
  '((presentation-key . "g42")
    (presentation-origin . created)))
```

This lets the `delete-frame-functions` hook resolve frame → key without
scanning the hash, and lets `display-buffer-alist` predicates check
`(frame-parameter (selected-frame) 'presentation-key)`.

## Frame discovery (tty match)

```
   tmux list-panes -t SESS:WIN -F '#{pane_id}\t#{pane_tty}'
        │
        ▼
   parse → ((pane-id . tty) ...)
        │
        ▼
   for each frame in (frame-list):
     if (frame-parameter f 'tty) matches a pane tty
        AND that frame belongs to this daemon
     → reuse it
```

A tty-frame's `'tty` parameter is exactly the device path tmux reports as
`pane_tty` (e.g. `/dev/ttys012`). No other identification is needed.

If multiple frames somehow match (shouldn't happen — one frame per tty),
take the first.

## Spawn path

```
1. emit `tmux split-window` effect with the daemon's socket
2. install one-shot hook on `after-make-frame-functions`:
       (lambda (f)
         (when (equal (frame-parameter f 'tty) <expected tty>)
           (resolve! f)))
   — but we don't know the tty in advance.
3. simpler: install one-shot that captures the *next* tty-frame whose
   tmux pane is a child of the just-spawned split. Use `tmux list-panes`
   diff: panes-after minus panes-before = the new pane id and its tty.
4. set frame parameters; create splash buffer; display it; install
   delete-frame-functions cleanup
```

Concretely, the orchestration is:

```
list-panes (before)  →  split-window  →  poll/list-panes (after)
                                                      │
                                       new = after \ before  → (pane, tty)
                                                      │
                                  find frame by tty → tag it
```

A small bounded retry (e.g. 10 × 50 ms) is acceptable for the post-split
poll. This is the only place we tolerate timing fuzz.

## Splash buffer

`*presentation: <key>*` in fundamental-mode (or a minimal derived mode for
keymap purposes). Initial content:

```
Presentation session <key>
Worktree: /path/to/wt
Started: 2026-05-01 12:34:56

Awaiting first slide…
```

If the start payload includes `initial_slide`, this placeholder is
replaced by the rendered slide *atomically* before the frame is shown
(no flicker).

## Narrative slide rendering (v1 only)

A `narrative` slide is `{ kind: "narrative", markdown: "…" }`. Rendered
into the `*presentation: <key>*` buffer using `markdown-mode` for
fontification. Future slide kinds (file panel, diff panel, overlay
annotations) are out of scope here.

## Tear-down

```
end_presentation(key):
  state = lookup or error
  case state.origin:
    'reused:
      with-selected-frame state.frame
        set-window-configuration state.saved-config
      remove frame params
      remove from hash
    'created:
      emit `tmux kill-pane -t state.tmux-pane`
      delete-frame-functions hook will fire when the frame dies
      and clean the hash entry
```

## delete-frame-functions hook

```elisp
(defun +presentation--frame-deleted-h (frame)
  (when-let* ((key (frame-parameter frame 'presentation-key)))
    (remhash key +presentation--sessions)))

(add-hook 'delete-frame-functions #'+presentation--frame-deleted-h)
```

Idempotent: if `end_presentation` already cleaned up, `remhash` is a
no-op.

## Register escape hatch

When a `reused`-origin session captures `current-window-configuration`,
the same configuration is also pushed into register `?P` via
`(set-register ?P (list 'window-configuration <wc> <point-marker>))`.
The user can then `C-x r j P` if anything goes wrong. Last-write-wins
across multiple presentations is acceptable — this is purely a
recovery aid.

## display-buffer protection

`modules/ui/init.el` gains one rule that runs *before* the existing
side-window rules:

```elisp
(cons (lambda (_buf _action)
        (frame-parameter (selected-frame) 'presentation-key))
      '((display-buffer-no-window) (allow-no-window . t)))
```

This prevents `display-buffer` from ever auto-popping into a presentation
frame from elsewhere. The agent's slide ops bypass `display-buffer` and
display directly into the known frame.

## Concurrency

Multiple presentations are supported — each has a unique key, a distinct
frame, and independent state. There is no global lock. Two agents
presenting from two worktrees produce two frames; the human attends to
whichever is in front.

## Out of scope (deferred)

- Slide ops beyond the initial-slide carrier in `start_presentation`.
- Bidirectional event channel beyond chat (no `await_feedback` long-poll
  tool yet).
- Slide kinds: file panel, diff panel, overlay annotations, hyperlinks.
- Presentation minor-mode for user navigation (`n` / `p`).
- Per-frame display-buffer customisation beyond the no-popup rule.
