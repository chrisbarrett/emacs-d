;;; init.el --- Presentation sessions module -*- lexical-binding: t; -*-

;;; Commentary:

;; Registers the presentation MCP tools and wires the
;; `delete-frame-functions' cleanup hook.  All implementation lives in
;; `lib.el'; this file only handles registration.

;;; Code:

(require '+autoloads)
(require 'claude-code-ide-mcp-server nil t)

(when (fboundp 'claude-code-ide-make-tool)
  (claude-code-ide-make-tool
   :name "start_presentation"
   :description
   "Start a presentation session in an Emacs frame served by the daemon.

Prefer `present_document' for walk-throughs — that tool writes a real
markdown doc, opens it as the first narrative slide, and pushes any
supporting `file' slides in one call.  Use `start_presentation'
directly only for cases that don't fit the doc-first shape (e.g. an
ephemeral synthetic narrative slide via `:markdown', or starting an
empty deck).

Discovers an existing emacsclient frame in the target tmux window by
joining `tmux list-panes' output against the daemon's frames.  If a
matching frame is found it is reused (its window-configuration is
saved).  Otherwise a new pane is split off the target window running
`emacsclient -t', and the resulting frame is captured.

The current tmux window layout is captured so that `end_presentation'
can restore the user's pre-session pane geometry.

Returns a session-key string to be passed back to `end_presentation'."
   :args
   '((:name "worktree"      :type string :description "Absolute path to the worktree the session is associated with.")
     (:name "tmux_session"  :type string :description "Tmux session name or id of the target window.")
     (:name "tmux_window"   :type string :description "Tmux window id or index.")
     (:name "split"         :type string :optional t :enum ["horizontal" "vertical"]
            :description "Split direction when spawning a new pane (default: horizontal).")
     (:name "initial_slide" :type object :optional t
            :description "Optional initial slide spec; becomes deck entry 0."))
   :function
   (lambda (worktree tmux_session tmux_window &optional split initial_slide)
     (+presentation-start
      :worktree worktree
      :tmux-session tmux_session
      :tmux-window tmux_window
      :split (pcase split ("vertical" 'vertical) (_ 'horizontal))
      :initial-slide (and initial_slide
                          (+presentation--coerce-slide initial_slide)))))

  (claude-code-ide-make-tool
   :name "get_presentation"
   :description
   "Return state for a presentation session.

Returns an object with: key, origin (\"created\"/\"reused\"), frame_live
(boolean), tmux_pane (the spawned pane id, or null for reused
sessions), worktree, started_at (float seconds-since-epoch),
slide_count (integer), current_slide_index (integer or null).

Use this to verify a session is still alive and inspect deck position.
Signals an error if the key is unknown."
   :args
   '((:name "key" :type string :description "The session key returned by `start_presentation'."))
   :function
   (lambda (key) (+presentation-info key)))

  (claude-code-ide-make-tool
   :name "end_presentation"
   :description
   "End the presentation session identified by KEY.

If the session reused an existing frame, the saved window-configuration
is restored.  If the session spawned a new pane, the pane is killed via
`tmux kill-pane'.  All overlays attached to slides in the deck are
deleted as part of teardown."
   :args
   '((:name "key" :type string :description "The session key returned by `start_presentation'."))
   :function
   (lambda (key) (+presentation-end key)))

  (claude-code-ide-make-tool
   :name "push_slide"
   :description
   "Append a slide to the deck and return its integer index.

Prefer `present_document' for the initial doc + supporting file slides.
Use `push_slide' to add more annotated source views to a session that
was opened with `present_document', not to assemble a deck out of many
narrative slides — multi-narrative decks are discouraged.

The user's currently rendered slide is left untouched by default — pushing
slides does NOT drag the user's view forward.  Pass `set_current: true'
to opt in to setting the new slide as current and re-rendering; reserve
this for \"please look here now\" moments.

Slide spec must include a `kind' field; supported kinds are `narrative',
`file', `diff', and `layout'.  Every kind also accepts an optional
`pane_layout' string of `tall' (claude-code on top, presentation below)
or `wide' (claude-code on the left, presentation on the right) that
reshapes the tmux window before rendering; absent leaves geometry
unchanged.

`narrative', `file', and `diff' slides accept an `annotations' array of
`{ line, text, kind?, severity?, position? }' records:
  - `kind' (default `inline') is `inline' / `callout' / `margin'.
    `inline' tags one line; `callout' draws a box-drawn note block
    after the target line; `margin' renders text in the buffer margin.
  - `severity' (default `note') is `note' / `tip' / `warning' and
    drives the colour and label.
  - `position' for `inline' is `before' / `after' (default `after',
    anchored at EOL); for `margin' is `left' / `right' (default
    `right'); `callout' annotations must omit `position'.

Example: `{ line: 12, text: \"watch out\", kind: \"callout\", severity: \"warning\" }'.

Raises a user-error on validation failure (missing required
fields, nested layout, half-specified diff range, bad annotation line,
invalid `pane_layout', unknown `kind' / `severity', or `position' on
the wrong kind)."
   :args
   '((:name "key" :type string :description "Session key.")
     (:name "slide" :type object :description "Slide spec object.")
     (:name "set_current" :type boolean :optional t
            :description
            "When true, set the new slide as current and render it.  Default false."))
   :function
   (lambda (key slide &optional set_current)
     (+presentation--deck-push key (+presentation--coerce-slide slide)
                               :set-current (and set_current t))))

  (claude-code-ide-make-tool
   :name "replace_slide"
   :description
   "Replace the slide at INDEX.  Re-renders only when INDEX is current.

The user's view is dragged forward only when INDEX equals the session's
current slide index — replacing a non-current slide updates the deck
silently, in keeping with the user-paced flow.  Slide spec accepts the
same annotation `kind' / `severity' / `position' fields as `push_slide'.
Raises a user-error on out-of-range INDEX or when the deck is empty."
   :args
   '((:name "key" :type string :description "Session key.")
     (:name "index" :type integer :description "Zero-based slide index.")
     (:name "slide" :type object :description "Replacement slide spec."))
   :function
   (lambda (key index slide)
     (+presentation--deck-replace key index (+presentation--coerce-slide slide))))

  (claude-code-ide-make-tool
   :name "truncate_after"
   :description
   "Drop slides past INDEX.  Pass -1 to clear the deck entirely.

When the prior current slide index was greater than INDEX, the current
slide index becomes INDEX and that slide is re-rendered.  Raises a
user-error on out-of-range INDEX."
   :args
   '((:name "key" :type string :description "Session key.")
     (:name "index" :type integer :description "Zero-based pivot index; -1 clears the deck."))
   :function
   (lambda (key index) (+presentation--deck-truncate key index)))

  (claude-code-ide-make-tool
   :name "goto_slide"
   :description
   "Re-render the slide at INDEX as the current slide.  Does not mutate the deck.

Raises a user-error on out-of-range INDEX."
   :args
   '((:name "key" :type string :description "Session key.")
     (:name "index" :type integer :description "Zero-based target index."))
   :function
   (lambda (key index) (+presentation--deck-goto key index)))

  (claude-code-ide-make-tool
   :name "present_document"
   :description
   "Write a markdown document and open it as the first (narrative) slide.

Preferred entry point for walk-throughs.  Pattern: write one fat
document with embedded code fences and links to source, then push
annotated `file' slides for the sources the doc references.  Avoid
pushing many `narrative' slides — the convention is one document.

Path is computed, not configurable:

  `<worktree>/.claude/presentations/<YYYY>-<MM>-<DD>T<HH>-<MM>-<slug>.md'

The directory is created if absent.  The document path is returned so
the agent can iterate via `Edit'; the buffer auto-reverts on disk
change.  Inside the document, two link forms route through the deck:

- `[label](slide:N)' — navigate to slide N.
- `[label](path#L<start>-L<end>)' — navigate to the matching file
  slide if one exists; falls back to `find-file' otherwise.

Returns an object with `key' (session key), `path' (document path),
and `slide_count' (1 + length of `file_slides')."
   :args
   '((:name "worktree"     :type string :description "Absolute worktree path.")
     (:name "tmux_session" :type string :description "Tmux session name or id.")
     (:name "tmux_window"  :type string :description "Tmux window id or index.")
     (:name "slug"         :type string
            :description "Kebab-case slug; no `/' or whitespace.")
     (:name "markdown"     :type string :description "Document body.")
     (:name "split"        :type string :optional t :enum ["horizontal" "vertical"]
            :description "Split direction when spawning a new pane (default: horizontal).")
     (:name "file_slides"  :type array :optional t
            :description "Optional array of slide specs, each with kind=\"file\"."))
   :function
   (lambda (worktree tmux_session tmux_window slug markdown
                     &optional split file_slides)
     (+presentation-present-document
      :worktree worktree
      :tmux-session tmux_session
      :tmux-window tmux_window
      :slug slug
      :markdown markdown
      :split (pcase split ("vertical" 'vertical) (_ 'horizontal))
      :file-slides (and file_slides
                        (mapcar #'+presentation--coerce-slide
                                (append file_slides nil))))))

  (claude-code-ide-make-tool
   :name "get_deck"
   :description
   "Return the deck structure for a session.

Returns an object with `key', `current_slide_index' (integer or null),
and `slides' — an ordered array of `{ index, kind, title }' records.
Slide bodies (markdown, file content, annotations) are NOT echoed back;
this op is for structural recovery, not content recovery."
   :args
   '((:name "key" :type string :description "Session key."))
   :function
   (lambda (key) (+presentation-deck-info key))))

(add-hook 'delete-frame-functions #'+presentation--frame-deleted-h)

(with-eval-after-load 'claude-code-ide-mcp
  (+presentation--register-channel-capability))

;;; init.el ends here
