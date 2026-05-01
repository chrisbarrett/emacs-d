;;; lib.el --- Presentation sessions library -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure planners, parsers, command builders, runner, and state-store for
;; presentation sessions.  Tmux interaction is modelled as data: planners
;; emit lists of effect records; a single runner executes them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup +presentation nil
  "Agent-driven presentation sessions in Emacs frames."
  :group 'tools)

(defcustom +presentation-narrative-major-mode 'markdown-mode
  "Major mode used to render `narrative' slides."
  :type 'function
  :group '+presentation)

(defcustom +presentation-spawn-poll-attempts 10
  "Maximum attempts when polling for the post-split pane."
  :type 'integer
  :group '+presentation)

(defcustom +presentation-spawn-poll-interval 0.05
  "Seconds to wait between post-split pane polls."
  :type 'number
  :group '+presentation)

(defvar +presentation--sessions (make-hash-table :test 'equal)
  "Map presentation session-key (string) to plist state.")

(defvar +presentation--effect-results nil
  "Dynamic: results accumulated by `+presentation--run-effects'.
Each elisp thunk runs with this bound to the list of prior results
in execution order.  Used to thread shell-output through orchestration.")

;;; Effect records

(cl-defstruct +presentation-effect-shell
  "Shell command to execute.  ARGV is a list of strings (no shell quoting)."
  argv)

(cl-defstruct +presentation-effect-elisp
  "Internal state mutation.  THUNK is a zero-arg function."
  thunk)

;;;###autoload
(defun +presentation--run-effects (effects)
  "Execute EFFECTS in order; return collected results.
Shell effects are dispatched via `process-file'; elisp effects call
their thunk.  Each elisp thunk runs with `+presentation--effect-results'
dynamically bound to the list of prior results."
  (let ((+presentation--effect-results nil))
    (dolist (e effects)
      (let ((result
             (cond
              ((+presentation-effect-shell-p e)
               (let ((argv (+presentation-effect-shell-argv e)))
                 (with-temp-buffer
                   (apply #'process-file (car argv) nil t nil (cdr argv))
                   (buffer-string))))
              ((+presentation-effect-elisp-p e)
               (funcall (+presentation-effect-elisp-thunk e)))
              (t (error "Unknown effect: %S" e)))))
        (setq +presentation--effect-results
              (append +presentation--effect-results (list result)))))
    +presentation--effect-results))

;;; Tmux command builders (pure)

(defun +presentation--cmd-list-panes (sess win)
  "Build a `tmux list-panes' effect targeting SESS:WIN.
Output format is `pane_id\\tpane_tty'."
  (make-+presentation-effect-shell
   :argv (list "tmux" "list-panes"
               "-t" (format "%s:%s" sess win)
               "-F" "#{pane_id}\t#{pane_tty}")))

(defun +presentation--cmd-split-window (sess win split socket)
  "Build a `tmux split-window' effect for SESS:WIN with SPLIT direction.
SPLIT is the symbol `horizontal' or `vertical'.  The new pane runs
`emacsclient -t -s SOCKET'."
  (make-+presentation-effect-shell
   :argv (list "tmux" "split-window"
               (pcase split
                 ('horizontal "-h")
                 ('vertical "-v")
                 (_ (error "Unknown split direction: %S" split)))
               "-t" (format "%s:%s" sess win)
               "--" "emacsclient" "-t" "-s" socket)))

(defun +presentation--cmd-kill-pane (pane-id)
  "Build a `tmux kill-pane' effect for PANE-ID."
  (make-+presentation-effect-shell
   :argv (list "tmux" "kill-pane" "-t" pane-id)))

(defun +presentation--parse-list-panes-output (output)
  "Parse OUTPUT of `tmux list-panes -F #{pane_id}\\t#{pane_tty}'.
Return alist `((PANE-ID . TTY) ...)' in input order."
  (delq nil
        (mapcar (lambda (line)
                  (let ((parts (split-string line "\t" t)))
                    (when (= (length parts) 2)
                      (cons (nth 0 parts) (nth 1 parts)))))
                (split-string (or output "") "\n" t))))

(defun +presentation--diff-panes (before after)
  "Return entries in AFTER whose pane-id is not in BEFORE.
BEFORE and AFTER are alists `((PANE-ID . TTY) ...)'."
  (cl-remove-if (lambda (p) (assoc (car p) before)) after))

;;; State store

(defun +presentation--make-key ()
  "Return a fresh, unique session-key string."
  (symbol-name (gensym "presentation-")))

(defun +presentation--register-session (key plist)
  "Store PLIST under KEY in `+presentation--sessions' and tag the frame.
Sets the frame parameters `presentation-key' and `presentation-origin'."
  (puthash key plist +presentation--sessions)
  (when-let* ((frame (plist-get plist :frame)))
    (when (frame-live-p frame)
      (modify-frame-parameters
       frame
       `((presentation-key . ,key)
         (presentation-origin . ,(plist-get plist :origin))))))
  plist)

(defun +presentation--get-session (key)
  "Return the session plist for KEY or signal a `user-error'."
  (or (gethash key +presentation--sessions)
      (user-error "Unknown presentation session key: %s" key)))

;;; Frame discovery

(defun +presentation--find-frame-by-tty (tty)
  "Return the first daemon frame whose `tty' parameter equals TTY."
  (cl-find-if (lambda (f) (equal (frame-parameter f 'tty) tty))
              (frame-list)))

(defun +presentation--find-existing-frame (panes)
  "Return the first frame whose tty matches a pane in PANES, or nil.
PANES is an alist `((PANE-ID . TTY) ...)'."
  (cl-some (lambda (p) (+presentation--find-frame-by-tty (cdr p)))
           panes))

;;; Splash buffer & rendering

(defun +presentation--make-splash-buffer (key worktree)
  "Create and return the `*presentation: KEY*' buffer.
The buffer contains the session key, WORKTREE path, start time, and a
placeholder line indicating no slide has yet been pushed."
  (let ((buf (get-buffer-create (format "*presentation: %s*" key))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Presentation session %s\n" key))
        (insert (format "Worktree: %s\n" worktree))
        (insert (format-time-string "Started: %F %T\n"))
        (insert "\nAwaiting first slide…\n")))
    buf))

(defun +presentation--render-slide (buffer slide)
  "Render SLIDE into BUFFER, replacing prior contents.
SLIDE is a plist with `:kind' (string) and kind-specific fields.  For
kind `\"narrative\"' the `:markdown' field is inserted and the
`+presentation-narrative-major-mode' major mode is enabled."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pcase (plist-get slide :kind)
        ("narrative"
         (insert (or (plist-get slide :markdown) ""))
         (when (fboundp +presentation-narrative-major-mode)
           (funcall +presentation-narrative-major-mode)))
        (other
         (error "Unknown slide kind: %S" other)))))
  buffer)

;;; Reuse path planner

(defun +presentation--plan-reuse (frame key worktree)
  "Return effects that register a reuse-origin session keyed by KEY.
FRAME is the matched daemon frame; WORKTREE is the start payload's
worktree path.  The plan captures `current-window-configuration',
pushes it to register `?P', and stores it on the session plist."
  (list
   (make-+presentation-effect-elisp
    :thunk
    (lambda ()
      (let ((wc (with-selected-frame frame
                  (current-window-configuration))))
        (set-register ?P (list 'window-configuration wc (point-marker)))
        (+presentation--register-session
         key
         (list :frame frame
               :origin 'reused
               :saved-config wc
               :tmux-pane nil
               :worktree worktree
               :started-at (float-time))))))))

;;; Spawn path planner

(defun +presentation--plan-spawn (key sess win split socket worktree)
  "Return effects that spawn a new pane and register a created-origin session.
KEY is the session key to assign.  SESS, WIN, SPLIT, SOCKET are passed
to the tmux command builders.  WORKTREE is recorded on the session plist.

Effects, in order:
  1. `tmux list-panes' (before snapshot — runner records output).
  2. `tmux split-window'.
  3. Elisp: poll `tmux list-panes' (after), diff to find the new pane,
     resolve the corresponding frame.
  4. Elisp: tag the frame, register the session."
  (let ((discovered (list nil)))                 ; (PANE-ID . TTY) | nil
    (list
     (+presentation--cmd-list-panes sess win)
     (+presentation--cmd-split-window sess win split socket)
     (make-+presentation-effect-elisp
      :thunk
      (lambda ()
        (let* ((before-output (nth 0 +presentation--effect-results))
               (before (+presentation--parse-list-panes-output before-output))
               (new nil)
               (attempts +presentation-spawn-poll-attempts))
          (while (and (null new) (> attempts 0))
            (let* ((after-output
                    (car (+presentation--run-effects
                          (list (+presentation--cmd-list-panes sess win)))))
                   (after (+presentation--parse-list-panes-output after-output))
                   (diff (+presentation--diff-panes before after)))
              (when diff (setq new (car diff))))
            (unless new
              (sleep-for +presentation-spawn-poll-interval)
              (cl-decf attempts)))
          (setcar discovered new)
          new)))
     (make-+presentation-effect-elisp
      :thunk
      (lambda ()
        (let* ((new (car discovered))
               (pane-id (car-safe new))
               (tty (cdr-safe new))
               (frame nil)
               (attempts +presentation-spawn-poll-attempts))
          (unless new
            (error "Spawn: no new pane discovered after split"))
          (while (and (null frame) (> attempts 0))
            (setq frame (+presentation--find-frame-by-tty tty))
            (unless frame
              (sleep-for +presentation-spawn-poll-interval)
              (cl-decf attempts)))
          (unless frame
            (error "Spawn: no frame appeared for tty %s" tty))
          (+presentation--register-session
           key
           (list :frame frame
                 :origin 'created
                 :saved-config nil
                 :tmux-pane pane-id
                 :worktree worktree
                 :started-at (float-time)))))))))

;;; Top-level orchestration

;;;###autoload
(cl-defun +presentation-start (&key worktree tmux-session tmux-window
                                    (split 'horizontal) initial-slide socket)
  "Start a presentation session.
Discover an emacsclient frame in TMUX-SESSION:TMUX-WINDOW (joining
`tmux list-panes' against `(frame-parameter f \\='tty)') and either
reuse it or spawn a new pane via `tmux split-window' with SPLIT
direction (`horizontal' or `vertical').  WORKTREE is the originating
worktree path.  INITIAL-SLIDE, when non-nil, is a slide plist rendered
into the splash buffer before the frame is shown.  SOCKET overrides the
emacsclient socket path; defaults to `server-socket-dir'/`server-name'.

Return the generated session-key string."
  (unless worktree (user-error "Missing :worktree"))
  (unless tmux-session (user-error "Missing :tmux-session"))
  (unless tmux-window (user-error "Missing :tmux-window"))
  (let* ((key (+presentation--make-key))
         (sock (or socket (+presentation--default-socket)))
         (panes-out
          (car (+presentation--run-effects
                (list (+presentation--cmd-list-panes
                       tmux-session tmux-window)))))
         (panes (+presentation--parse-list-panes-output panes-out))
         (existing (+presentation--find-existing-frame panes)))
    (if existing
        (+presentation--run-effects
         (+presentation--plan-reuse existing key worktree))
      (+presentation--run-effects
       (+presentation--plan-spawn key tmux-session tmux-window
                                  split sock worktree)))
    (let* ((session (+presentation--get-session key))
           (frame (plist-get session :frame))
           (buf (+presentation--make-splash-buffer key worktree)))
      (when initial-slide
        (+presentation--render-slide buf initial-slide))
      (when (frame-live-p frame)
        (with-selected-frame frame
          (switch-to-buffer buf))))
    key))

;;;###autoload
(defun +presentation-end (key)
  "Tear down the presentation session identified by KEY.
For `reused'-origin sessions: restore the saved window-configuration,
remove the frame parameters, and drop the hash entry.  For `created'-
origin sessions: emit a `tmux kill-pane' for the recorded pane id; the
resulting frame deletion fires `delete-frame-functions' which removes
the hash entry."
  (let* ((session (+presentation--get-session key))
         (origin (plist-get session :origin))
         (frame (plist-get session :frame)))
    (pcase origin
      ('reused
       (+presentation--run-effects
        (list
         (make-+presentation-effect-elisp
          :thunk
          (lambda ()
            (when (frame-live-p frame)
              (with-selected-frame frame
                (set-window-configuration
                 (plist-get session :saved-config)))
              (modify-frame-parameters
               frame
               '((presentation-key . nil)
                 (presentation-origin . nil))))
            (remhash key +presentation--sessions))))))
      ('created
       (+presentation--run-effects
        (list (+presentation--cmd-kill-pane
               (plist-get session :tmux-pane)))))
      (other (error "Unknown session origin: %S" other)))
    'done))

;;;###autoload
(defun +presentation--display-buffer-suppress-p (_buffer _action)
  "Return non-nil if the selected frame is a presentation frame.
Used in `display-buffer-alist' to suppress auto-popups inside a
presentation surface."
  (frame-parameter (selected-frame) 'presentation-key))

;;;###autoload
(defun +presentation-info (key)
  "Return an alist describing the presentation session identified by KEY.
Signals a `user-error' for unknown keys.  The returned alist is
JSON-encodable: booleans use `t' / `:json-false', the frame value is
omitted, and `started-at' is rendered as a float seconds-since-epoch."
  (let* ((session (+presentation--get-session key))
         (frame (plist-get session :frame))
         (origin (plist-get session :origin)))
    `((key . ,key)
      (origin . ,(when origin (symbol-name origin)))
      (frame_live . ,(if (and frame (frame-live-p frame)) t :json-false))
      (tmux_pane . ,(plist-get session :tmux-pane))
      (worktree . ,(plist-get session :worktree))
      (started_at . ,(plist-get session :started-at)))))

;;;###autoload
(defun +presentation--frame-deleted-h (frame)
  "Remove the session entry whose `:frame' is FRAME.
By the time `delete-frame-functions' fires for a tty-client
disconnect the frame is already dead and its parameters are gone, so
matching by `frame-parameter' is unreliable.  Match on identity
against the `:frame' value stored on the session plist instead."
  (let (matched-key)
    (maphash (lambda (k plist)
               (when (eq (plist-get plist :frame) frame)
                 (setq matched-key k)))
             +presentation--sessions)
    (when matched-key
      (remhash matched-key +presentation--sessions))))

;;;###autoload
(defun +presentation--alist-to-plist (alist)
  "Convert ALIST with symbol or string keys to a plist with `:'-prefixed keys."
  (let (acc)
    (dolist (cell alist (nreverse acc))
      (let* ((k (car cell))
             (v (cdr cell))
             (sym (cond ((keywordp k) k)
                        ((symbolp k) (intern (concat ":" (symbol-name k))))
                        ((stringp k) (intern (concat ":" k)))
                        (t (error "Unsupported alist key: %S" k)))))
        (push sym acc)
        (push v acc)))))

(defun +presentation--default-socket ()
  "Return the running daemon's socket path."
  (when (and (boundp 'server-name) server-name)
    (expand-file-name server-name
                      (or (and (boundp 'server-socket-dir) server-socket-dir)
                          temporary-file-directory))))

;;; lib.el ends here
