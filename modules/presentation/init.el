;;; init.el --- Presentation sessions module -*- lexical-binding: t; -*-

;;; Commentary:

;; Registers the `start_presentation' / `end_presentation' MCP tools and
;; wires the `delete-frame-functions' cleanup hook.  All implementation
;; lives in `lib.el'; this file only handles registration.

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
            :description "Optional initial slide spec: { kind: 'narrative', markdown: '…' }."))
   :function
   (lambda (worktree tmux_session tmux_window &optional split initial_slide)
     (+presentation-start
      :worktree worktree
      :tmux-session tmux_session
      :tmux-window tmux_window
      :split (pcase split ("vertical" 'vertical) (_ 'horizontal))
      :initial-slide (and initial_slide
                          (+presentation--alist-to-plist initial_slide)))))

  (claude-code-ide-make-tool
   :name "get_presentation"
   :description
   "Return state for a presentation session.

Returns an object with: key, origin (\"created\"/\"reused\"), frame_live
(boolean), tmux_pane (the spawned pane id, or null for reused
sessions), worktree, started_at (float seconds-since-epoch).

Use this to verify a session is still alive before pushing slides, or
to inspect what backing resources are in play (frame, tmux pane).
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
`tmux kill-pane'."
   :args
   '((:name "key" :type string :description "The session key returned by `start_presentation'."))
   :function
   (lambda (key) (+presentation-end key))))

(add-hook 'delete-frame-functions #'+presentation--frame-deleted-h)

;;; init.el ends here
