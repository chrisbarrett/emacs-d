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

Discovers an existing emacsclient frame in the target tmux window by
joining `tmux list-panes' output against the daemon's frames.  If a
matching frame is found it is reused (its window-configuration is
saved).  Otherwise a new pane is split off the target window running
`emacsclient -t', and the resulting frame is captured.

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
   "Append a slide to the deck, set it as the current slide, render it.

Returns the new slide's integer index.  Slide spec must include a
`kind' field; supported kinds are `narrative', `file', `diff', and
`layout'.  Raises a user-error on validation failure (missing required
fields, nested layout, half-specified diff range, bad annotation line)."
   :args
   '((:name "key" :type string :description "Session key.")
     (:name "slide" :type object :description "Slide spec object."))
   :function
   (lambda (key slide)
     (+presentation--deck-push key (+presentation--coerce-slide slide))))

  (claude-code-ide-make-tool
   :name "replace_slide"
   :description
   "Replace the slide at INDEX.  Re-renders only when INDEX is current.

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

;;; init.el ends here
