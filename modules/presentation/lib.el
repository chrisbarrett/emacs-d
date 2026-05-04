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

;;; Faces

(defface +presentation-focus-face
  '((((background dark))  :background "#2d3142")
    (((background light)) :background "#e8e6df")
    (t :inherit highlight))
  "Face for the file-slide focus highlight.
Painted per-line from BOL to EOL only; never extends to window width."
  :group '+presentation)

(defface +presentation-annotation-note-face
  '((t :inherit (+markdown-gfm-callout-note-face)))
  "Face for `note' severity presentation annotations."
  :group '+presentation)

(defface +presentation-annotation-tip-face
  '((t :inherit (+markdown-gfm-callout-tip-face)))
  "Face for `tip' severity presentation annotations."
  :group '+presentation)

(defface +presentation-annotation-warning-face
  '((t :inherit (+markdown-gfm-callout-warning-face)))
  "Face for `warning' severity presentation annotations."
  :group '+presentation)

(defconst +presentation--severity-faces
  '(("note"    . +presentation-annotation-note-face)
    ("tip"     . +presentation-annotation-tip-face)
    ("warning" . +presentation-annotation-warning-face))
  "Map annotation severity string to face symbol.")

(defconst +presentation--severity-labels
  '(("note" . "NOTE") ("tip" . "TIP") ("warning" . "WARNING"))
  "Map annotation severity string to display label.")

(defun +presentation--annotation-face (severity)
  "Return face symbol for SEVERITY (defaults to `note')."
  (alist-get (or severity "note") +presentation--severity-faces
             '+presentation-annotation-note-face nil #'string=))

(defun +presentation--severity-label (severity)
  "Return display label for SEVERITY (defaults to `note')."
  (alist-get (or severity "note") +presentation--severity-labels
             "NOTE" nil #'string=))

(defun +presentation--blend-toward-fg (bg fg ratio)
  "Blend hex colour BG by RATIO toward hex FG; return hex string."
  (require 'color)
  (let ((bg-rgb (color-name-to-rgb bg))
        (fg-rgb (color-name-to-rgb fg)))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (b f) (+ b (* ratio (- f b))))
                              bg-rgb fg-rgb)
                   '(2)))))

(defun +presentation--make-border (width corner-l corner-r face)
  "Build a horizontal box border of WIDTH chars using FACE.
CORNER-L and CORNER-R are the corner characters."
  (let ((fill (propertize (make-string (max 1 (- width 2)) ?─)
                          'face face)))
    (concat (propertize (string corner-l) 'face face)
            fill
            (propertize (string corner-r) 'face face))))

(defcustom +presentation-narrative-major-mode 'gfm-mode
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

(defun +presentation--cmd-window-layout (sess win)
  "Build a `tmux display-message -p #{window_layout}' effect for SESS:WIN."
  (make-+presentation-effect-shell
   :argv (list "tmux" "display-message" "-p"
               "-t" (format "%s:%s" sess win)
               "#{window_layout}")))

(defun +presentation--cmd-select-layout (sess win layout)
  "Build a `tmux select-layout LAYOUT' effect targeting SESS:WIN."
  (make-+presentation-effect-shell
   :argv (list "tmux" "select-layout"
               "-t" (format "%s:%s" sess win)
               layout)))

(defun +presentation--pane-layout-effects (desired current tmux-session tmux-window)
  "Return effects that reshape DESIRED on tmux TMUX-SESSION:TMUX-WINDOW.
DESIRED and CURRENT are `tall', `wide', or nil.  Returns nil when the
layouts already match (or DESIRED is nil)."
  (when (and desired (not (eq desired current)))
    (let ((target (format "%s:%s" tmux-session tmux-window)))
      (pcase desired
        ('tall
         (list
          (+presentation--cmd-select-layout tmux-session tmux-window
                                            "main-horizontal")
          (make-+presentation-effect-shell
           :argv (list "tmux" "set-window-option"
                       "-t" target "main-pane-height" "25%"))))
        ('wide
         (list
          (+presentation--cmd-select-layout tmux-session tmux-window
                                            "main-vertical")
          (make-+presentation-effect-shell
           :argv (list "tmux" "set-window-option"
                       "-t" target "main-pane-width" "33%"))))
        (_ (error "Unknown pane layout: %S" desired))))))

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

(defun +presentation--narrative-buffer (key)
  "Return (creating if needed) the per-session narrative buffer for KEY."
  (get-buffer-create (format "*presentation: %s*" key)))

;;; Reuse path planner

(defun +presentation--plan-reuse (frame key worktree tmux-session tmux-window)
  "Return effects that register a reuse-origin session keyed by KEY.
FRAME is the matched daemon frame; WORKTREE is the start payload's
worktree path.  TMUX-SESSION and TMUX-WINDOW are stashed on the session
plist so subsequent renders can target the same window for geometry
changes and teardown can restore the saved layout.  The plan captures
`current-window-configuration', pushes it to register `?P', and stores
it on the session plist."
  (list
   (+presentation--cmd-window-layout tmux-session tmux-window)
   (make-+presentation-effect-elisp
    :thunk
    (lambda ()
      (let ((wc (with-selected-frame frame
                  (current-window-configuration)))
            (saved-layout (string-trim
                           (or (nth 0 +presentation--effect-results) ""))))
        (set-register ?P (list 'window-configuration wc (point-marker)))
        (+presentation--register-session
         key
         (list :frame frame
               :origin 'reused
               :saved-config wc
               :tmux-pane nil
               :tmux-session tmux-session
               :tmux-window tmux-window
               :tmux-saved-layout saved-layout
               :worktree worktree
               :started-at (float-time))))))))

;;; Spawn path planner

(defun +presentation--plan-spawn (key sess win split socket worktree)
  "Return effects that spawn a new pane and register a created-origin session.
KEY is the session key to assign.  SESS, WIN, SPLIT, SOCKET are passed
to the tmux command builders.  WORKTREE is recorded on the session plist.

Effects, in order:
  1. `tmux display-message -p #{window_layout}' (saved-layout capture).
  2. `tmux list-panes' (before snapshot — runner records output).
  3. `tmux split-window'.
  4. Elisp: poll `tmux list-panes' (after), diff to find the new pane,
     resolve the corresponding frame.
  5. Elisp: tag the frame, register the session including the captured
     saved layout under `:tmux-saved-layout'."
  (let ((discovered (list nil)))                 ; (PANE-ID . TTY) | nil
    (list
     (+presentation--cmd-window-layout sess win)
     (+presentation--cmd-list-panes sess win)
     (+presentation--cmd-split-window sess win split socket)
     (make-+presentation-effect-elisp
      :thunk
      (lambda ()
        (let* ((before-output (nth 1 +presentation--effect-results))
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
                 :tmux-session sess
                 :tmux-window win
                 :tmux-saved-layout
                 (string-trim
                  (or (nth 0 +presentation--effect-results) ""))
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
         (+presentation--plan-reuse existing key worktree
                                    tmux-session tmux-window))
      (+presentation--run-effects
       (+presentation--plan-spawn key tmux-session tmux-window
                                  split sock worktree)))
    (+presentation--session-set key :deck [])
    (+presentation--session-set key :current-slide-index nil)
    (if initial-slide
        (+presentation--deck-push key initial-slide :set-current t)
      (+presentation--render-current key))
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
         (frame (plist-get session :frame))
         (saved-layout (plist-get session :tmux-saved-layout))
         (tmux-session (plist-get session :tmux-session))
         (tmux-window (plist-get session :tmux-window))
         (restore-effects
          (when (and saved-layout
                     (stringp saved-layout)
                     (not (string-empty-p saved-layout))
                     tmux-session tmux-window)
            (list (+presentation--cmd-select-layout
                   tmux-session tmux-window saved-layout)))))
    (+presentation--cleanup-render-state key)
    (pcase origin
      ('reused
       (+presentation--run-effects
        (append
         restore-effects
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
             (remhash key +presentation--sessions)))))))
      ('created
       (+presentation--run-effects
        (append
         restore-effects
         (list (+presentation--cmd-kill-pane
                (plist-get session :tmux-pane))))))
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
         (origin (plist-get session :origin))
         (deck (or (plist-get session :deck) []))
         (cur (plist-get session :current-slide-index)))
    `((key . ,key)
      (origin . ,(when origin (symbol-name origin)))
      (frame_live . ,(if (and frame (frame-live-p frame)) t :json-false))
      (tmux_pane . ,(plist-get session :tmux-pane))
      (worktree . ,(plist-get session :worktree))
      (started_at . ,(plist-get session :started-at))
      (slide_count . ,(length deck))
      (current_slide_index . ,cur))))

;;;###autoload
(defun +presentation-deck-info (key)
  "Return a JSON-encodable snapshot of session KEY's deck.
Result has fields `key', `current_slide_index', and `slides' — a
vector of `((index . I) (kind . K) (title . T-or-nil))' alists."
  (let* ((sess (+presentation--get-session key))
         (deck (or (plist-get sess :deck) []))
         (cur (plist-get sess :current-slide-index)))
    `((key . ,key)
      (current_slide_index . ,cur)
      (slides . ,(apply #'vector
                        (cl-loop for s across deck
                                 for i from 0
                                 collect `((index . ,i)
                                           (kind . ,(plist-get s :kind))
                                           (title . ,(plist-get s :title)))))))))

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
  "Convert ALIST with symbol or string keys to a plist with `:'-prefixed keys.
Underscores in non-keyword keys are translated to hyphens so that JSON
fields like `start_line' / `pane_layout' arrive as `:start-line' /
`:pane-layout'."
  (let (acc)
    (dolist (cell alist (nreverse acc))
      (let* ((k (car cell))
             (v (cdr cell))
             (name (cond ((keywordp k) nil)
                         ((symbolp k) (symbol-name k))
                         ((stringp k) k)
                         (t (error "Unsupported alist key: %S" k))))
             (sym (if name
                      (intern (concat ":" (replace-regexp-in-string
                                           "_" "-" name)))
                    k)))
        (push sym acc)
        (push v acc)))))

(defun +presentation--default-socket ()
  "Return the running daemon's socket path."
  (when (and (boundp 'server-name) server-name)
    (expand-file-name server-name
                      (or (and (boundp 'server-socket-dir) server-socket-dir)
                          temporary-file-directory))))


;;; Slide-spec coercion (deep alist→plist for MCP payloads)

(defun +presentation--alistp (val)
  "Return non-nil when VAL is an alist of `(SYM . _)' pairs.
Symbol keys are non-keyword to disambiguate from plists."
  (and (consp val)
       (consp (car val))
       (let ((k (caar val)))
         (and k (symbolp k) (not (keywordp k))))))

;;;###autoload
(defun +presentation--coerce-slide (input)
  "Recursively normalise an MCP slide spec INPUT.
Converts symbol-keyed alists to keyword-keyed plists, walks vectors,
leaves scalars untouched.  Used at the MCP boundary because
`json-parse-string' returns nested alists/vectors that the renderer
expects as plists."
  (cond
   ((vectorp input)
    (apply #'vector
           (mapcar #'+presentation--coerce-slide (append input nil))))
   ((+presentation--alistp input)
    (+presentation--alist-to-plist
     (mapcar (lambda (cell)
               (cons (car cell)
                     (+presentation--coerce-slide (cdr cell))))
             input)))
   (t input)))


;;; Slide validation

(defconst +presentation--annotation-allowed-keys
  '(:line :text :position :kind :severity)
  "Plist keys allowed on a slide annotation.")

(defun +presentation--validate-annotation (ann)
  "Validate annotation plist ANN; signal `user-error' on failure."
  (let* ((line (plist-get ann :line))
         (text (plist-get ann :text))
         (kind (plist-get ann :kind))
         (severity (plist-get ann :severity))
         (pos-present (plist-member ann :position))
         (pos (plist-get ann :position)))
    (unless (and (integerp line) (> line 0))
      (user-error "Annotation :line must be positive integer, got: %S" line))
    (unless (stringp text)
      (user-error "Annotation :text must be string, got: %S" text))
    (when kind
      (unless (member kind '("inline" "callout" "margin"))
        (user-error
         "Annotation :kind must be \"inline\" / \"callout\" / \"margin\", got: %S"
         kind)))
    (when severity
      (unless (member severity '("note" "tip" "warning"))
        (user-error
         "Annotation :severity must be \"note\" / \"tip\" / \"warning\", got: %S"
         severity)))
    (let ((effective-kind (or kind "inline")))
      (pcase effective-kind
        ("inline"
         (when pos-present
           (unless (member pos '("before" "after"))
             (user-error
              "inline annotation :position must be \"before\" or \"after\", got: %S"
              pos))))
        ("callout"
         (when pos-present
           (user-error
            "callout annotation must not have :position field, got: %S" pos)))
        ("margin"
         (when pos-present
           (unless (member pos '("left" "right" "before" "after"))
             (user-error
              "margin annotation :position must be \"left\" / \"right\" / \"before\" / \"after\", got: %S"
              pos))))))
    (let ((keys (cl-loop for (k _v) on ann by #'cddr collect k)))
      (dolist (k keys)
        (unless (memq k +presentation--annotation-allowed-keys)
          (user-error "Unknown annotation field: %S" k))))))

;;;###autoload
(defun +presentation--validate-slide (slide &optional context)
  "Validate SLIDE; signal `user-error' on failure, return SLIDE on success.
CONTEXT may be the symbol `pane' to forbid nested layout slides."
  (unless (and slide (listp slide))
    (user-error "Slide must be a plist, got: %S" slide))
  (let ((kind (plist-get slide :kind)))
    (unless (stringp kind)
      (user-error "Slide :kind must be a string, got: %S" kind))
    (pcase kind
      ("narrative"
       (unless (stringp (plist-get slide :markdown))
         (user-error "narrative slide requires string :markdown")))
      ("file"
       (unless (stringp (plist-get slide :path))
         (user-error "file slide requires string :path"))
       (let ((sl (plist-get slide :start-line))
             (el (plist-get slide :end-line)))
         (when sl
           (unless (and (integerp sl) (> sl 0))
             (user-error "file :start-line must be positive integer")))
         (when el
           (unless (and (integerp el) (> el 0))
             (user-error "file :end-line must be positive integer"))))
       (when-let* ((focus (plist-get slide :focus)))
         (unless (and (sequencep focus)
                      (= (length focus) 2)
                      (cl-every #'integerp (append focus nil)))
           (user-error "file :focus must be a 2-element integer range"))))
      ("diff"
       (let ((b (plist-get slide :base))
             (h (plist-get slide :head)))
         (when (or (and b (not h)) (and h (not b)))
           (user-error
            "diff slide requires both :base and :head, or neither"))))
      ("layout"
       (when (eq context 'pane)
         (user-error "layout slide may not be nested inside another layout"))
       (let ((split (plist-get slide :split)))
         (unless (member split '("horizontal" "vertical"))
           (user-error
            "layout :split must be \"horizontal\" or \"vertical\"")))
       (let ((panes (plist-get slide :panes)))
         (unless (and (sequencep panes) (= (length panes) 2))
           (user-error "layout :panes must contain exactly 2 slides"))
         (mapc (lambda (p) (+presentation--validate-slide p 'pane))
               (append panes nil))))
      (other (user-error "Unknown slide kind: %S" other)))
    (when (member kind '("narrative" "file" "diff"))
      (when-let* ((anns (plist-get slide :annotations)))
        (mapc #'+presentation--validate-annotation (append anns nil))))
    (when (plist-member slide :pane-layout)
      (let ((pl (plist-get slide :pane-layout)))
        (unless (member pl '("tall" "wide"))
          (user-error
           "Slide :pane-layout must be \"tall\" or \"wide\", got: %S" pl))))
    slide))


;;; Session plist helpers

(defun +presentation--session-set (key prop val)
  "Set PROP to VAL on session KEY; persist back to the store."
  (let ((sess (+presentation--get-session key)))
    (puthash key (plist-put sess prop val) +presentation--sessions)))

(defun +presentation--session-prop (key prop)
  "Return PROP from session KEY's plist."
  (plist-get (+presentation--get-session key) prop))


;;; Deck mutation helpers

(defun +presentation--deck (key)
  "Return the deck vector for session KEY (defaults to empty)."
  (or (+presentation--session-prop key :deck) []))

;;;###autoload
(cl-defun +presentation--deck-push (key slide &key set-current)
  "Append SLIDE to session KEY's deck and return the new slide's index.
Default appends without changing the session's current slide index or
rendering.  When SET-CURRENT is non-nil, the session's current slide
index is set to the new index and the slide is rendered."
  (+presentation--validate-slide slide)
  (let* ((deck (+presentation--deck key))
         (new-deck (vconcat deck (vector slide)))
         (new-idx (1- (length new-deck))))
    (+presentation--session-set key :deck new-deck)
    (when set-current
      (+presentation--session-set key :current-slide-index new-idx)
      (+presentation--render-current key))
    new-idx))

;;;###autoload
(defun +presentation--deck-replace (key index slide)
  "Replace SLIDE at INDEX in session KEY's deck.
Re-renders only when INDEX matches the current slide index.  Signals
`user-error' on out-of-range INDEX or empty deck."
  (+presentation--validate-slide slide)
  (let ((deck (+presentation--deck key)))
    (when (zerop (length deck))
      (user-error "Cannot replace slide on empty deck"))
    (unless (and (integerp index) (>= index 0) (< index (length deck)))
      (user-error "Slide index %S out of range [0,%d)" index (length deck)))
    (let ((new-deck (copy-sequence deck)))
      (aset new-deck index slide)
      (+presentation--session-set key :deck new-deck))
    (when (equal index (+presentation--session-prop key :current-slide-index))
      (+presentation--render-current key))
    index))

;;;###autoload
(defun +presentation--deck-truncate (key index)
  "Drop slides past INDEX in session KEY's deck.
INDEX of -1 clears the deck entirely.  When the prior current slide
index is greater than INDEX, the current slide index becomes INDEX and
that slide is re-rendered.  Signals `user-error' when INDEX is out of
range."
  (let* ((deck (+presentation--deck key))
         (len (length deck))
         (cur (+presentation--session-prop key :current-slide-index)))
    (unless (and (integerp index) (>= index -1) (< index len))
      (user-error "Truncate index %S out of range [-1,%d)" index len))
    (let ((new-deck (if (= index -1) []
                      (substring deck 0 (1+ index)))))
      (+presentation--session-set key :deck new-deck)
      (cond
       ((= index -1)
        (+presentation--session-set key :current-slide-index nil)
        (+presentation--render-current key))
       ((and cur (> cur index))
        (+presentation--session-set key :current-slide-index index)
        (+presentation--render-current key))))
    index))

(defun +presentation--save-current-point (key)
  "Stash point of session KEY's current slide into `:slide-points'.
Reads the point of the frame's selected window so the value reflects
the user's cursor when leaving the slide.  No-op when there is no
current index, no live frame, or no live window."
  (let* ((sess (+presentation--get-session key))
         (cur (plist-get sess :current-slide-index))
         (frame (plist-get sess :frame)))
    (when (and (integerp cur) (frame-live-p frame))
      (let ((win (frame-selected-window frame)))
        (when (window-live-p win)
          (let ((pt (window-point win))
                (alist (copy-alist (plist-get sess :slide-points))))
            (setf (alist-get cur alist) pt)
            (+presentation--session-set key :slide-points alist)))))))

(defun +presentation--restore-saved-point (key)
  "Restore point for session KEY's current slide from `:slide-points'.
Clamps to the destination buffer's `point-max' so a stale position
from a shorter post-edit buffer still lands inside bounds.  No-op
when no entry is saved for the current index."
  (let* ((sess (+presentation--get-session key))
         (cur (plist-get sess :current-slide-index))
         (frame (plist-get sess :frame))
         (alist (plist-get sess :slide-points))
         (pt (alist-get cur alist)))
    (when (and pt (integerp cur) (frame-live-p frame))
      (let ((win (frame-selected-window frame)))
        (when (window-live-p win)
          (let* ((buf (window-buffer win))
                 (clamped (with-current-buffer buf (min pt (point-max)))))
            (set-window-point win clamped)))))))

;;;###autoload
(defun +presentation--deck-goto (key index)
  "Render slide at INDEX in session KEY's deck and set it as current.
Saves the prior slide's cursor position into `:slide-points' before
flipping the index, and restores any previously-saved cursor position
for the destination slide after rendering.  Signals `user-error' on
out-of-range INDEX."
  (let* ((deck (+presentation--deck key))
         (len (length deck)))
    (unless (and (integerp index) (>= index 0) (< index len))
      (user-error "Goto index %S out of range [0,%d)" index len))
    (+presentation--save-current-point key)
    (+presentation--session-set key :current-slide-index index)
    (+presentation--render-current key)
    (+presentation--restore-saved-point key)
    index))


;;; Channel capability registration

;;;###autoload
(defun +presentation--inject-channel-capability (response)
  "Splice `experimental.claude/channel: {}' into RESPONSE.
RESPONSE is the alist returned by
`claude-code-ide-mcp--handle-initialize'.  Used as `:filter-return'
advice so the MCP server's initialize-response advertises the
channel capability without requiring an upstream patch.  Returns the
mutated RESPONSE."
  (let* ((result (alist-get 'result response))
         (caps (alist-get 'capabilities result))
         (exp (or (alist-get 'experimental caps) '())))
    (when caps
      (setf (alist-get 'claude/channel exp) :json-empty)
      (setf (alist-get 'experimental caps) exp)
      (setf (alist-get 'capabilities result) caps)
      (setf (alist-get 'result response) result)))
  response)

;;;###autoload
(defun +presentation--register-channel-capability (&optional fn)
  "Install `:filter-return' advice on the MCP initialize handler.
FN defaults to `claude-code-ide-mcp--handle-initialize'.  When FN is
unbound, registration silently no-ops and returns nil.  Otherwise
returns the symbol that was advised."
  (let ((sym (or fn 'claude-code-ide-mcp--handle-initialize)))
    (when (fboundp sym)
      (advice-add sym :filter-return
                  #'+presentation--inject-channel-capability)
      sym)))


;;; Channel notification emission

;;;###autoload
(defun +presentation--emit-nav-channel (key prior current)
  "Send a `notifications/claude/channel' event for session KEY.
PRIOR and CURRENT are integer indices into the session's deck.
Composes a human-readable advance / retreat content string and a
string-valued meta record.  Emission is best-effort: errors and a
missing `claude-code-ide-mcp--send-notification' silently no-op."
  (condition-case _err
      (when (fboundp 'claude-code-ide-mcp--send-notification)
        (let* ((sess (+presentation--get-session key))
               (deck (or (plist-get sess :deck) []))
               (count (length deck))
               (slide (and (integerp current)
                           (>= current 0)
                           (< current count)
                           (aref deck current)))
               (kind (plist-get slide :kind))
               (title (plist-get slide :title))
               (verb (if (and (integerp prior) (integerp current)
                              (< prior current))
                         "advanced" "retreated"))
               (content (format "User %s to slide %d of %d."
                                verb current count))
               (meta (list (cons 'key key)
                           (cons 'current_slide (number-to-string current))
                           (cons 'prior_slide (number-to-string prior))
                           (cons 'kind (or kind "")))))
          (when (and title (stringp title) (not (string-empty-p title)))
            (setq meta (append meta (list (cons 'title title)))))
          (funcall #'claude-code-ide-mcp--send-notification
                   "notifications/claude/channel"
                   `((source . "presentation")
                     (content . ,content)
                     (meta . ,meta)))
          nil))
    (error nil)))


;;; Render-state cleanup

(defun +presentation--cleanup-render-state (key)
  "Delete overlays and run restorers from session KEY's `:render-state'."
  (let* ((sess (+presentation--get-session key))
         (rs (plist-get sess :render-state)))
    (when rs
      (dolist (ov (plist-get rs :overlays))
        (when (and (overlayp ov) (overlay-buffer ov))
          (delete-overlay ov)))
      (dolist (fn (plist-get rs :restorers))
        (condition-case _ (funcall fn) (error nil))))
    (+presentation--session-set key :render-state nil)))

(defun +presentation--render-state-add (key prop val)
  "Cons VAL onto PROP of session KEY's `:render-state' plist."
  (let* ((rs (or (+presentation--session-prop key :render-state) '()))
         (cur (plist-get rs prop)))
    (+presentation--session-set
     key :render-state (plist-put rs prop (cons val cur)))))


;;; Per-kind renderers

(defun +presentation--render-narrative (key slide)
  "Render the narrative SLIDE for session KEY into its narrative buffer."
  (let ((buf (+presentation--narrative-buffer key)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or (plist-get slide :markdown) ""))
        (when (fboundp +presentation-narrative-major-mode)
          (funcall +presentation-narrative-major-mode))))
    (+presentation--enable-mode-in buf key)
    buf))


(defun +presentation--render-file (key slide)
  "Render the file SLIDE for session KEY via `find-file-noselect'.
Honours `:start-line'/`:end-line' (narrowing), `:focus' (region
overlay + recenter), and stashes restorers so cleanup widens the
buffer and restores its prior read-only state."
  (let* ((sess (+presentation--get-session key))
         (worktree (plist-get sess :worktree))
         (path (plist-get slide :path))
         (resolved (if (file-name-absolute-p path)
                       path
                     (expand-file-name path worktree)))
         (buf (find-file-noselect resolved))
         (start-line (plist-get slide :start-line))
         (end-line (plist-get slide :end-line))
         (focus (plist-get slide :focus)))
    (with-current-buffer buf
      (let ((prev-ro buffer-read-only))
        (when (or start-line end-line)
          (let ((p1 (save-excursion
                      (goto-char (point-min))
                      (when start-line (forward-line (1- start-line)))
                      (point)))
                (p2 (save-excursion
                      (goto-char (point-min))
                      (if end-line (progn (forward-line end-line) (point))
                        (point-max)))))
            (narrow-to-region p1 p2)
            (let ((b buf))
              (+presentation--render-state-add
               key :restorers
               (lambda () (with-current-buffer b (widen)))))))
        (when focus
          (let* ((fs (elt focus 0))
                 (fe (elt focus 1))
                 (start (+presentation--line-bol fs)))
            (cl-loop for ln from fs to fe
                     for bol = (+presentation--line-bol ln)
                     for eol = (+presentation--line-eol ln)
                     for ov = (make-overlay bol eol)
                     do (overlay-put ov 'face '+presentation-focus-face)
                     do (+presentation--render-state-add
                         key :overlays ov))
            (goto-char start)
            (condition-case _ (recenter) (error nil))))
        (setq buffer-read-only t)
        (let ((b buf))
          (+presentation--render-state-add
           key :restorers
           (lambda () (with-current-buffer b
                        (setq buffer-read-only prev-ro)))))))
    (+presentation--enable-mode-in buf key)
    buf))


(defun +presentation--diff-argv (worktree base head &optional path)
  "Build argv for `git -C WORKTREE diff [BASE..HEAD] [-- PATH]'.
Signals `user-error' when exactly one of BASE and HEAD is supplied."
  (when (or (and base (not head)) (and head (not base)))
    (user-error "diff slide requires both :base and :head, or neither"))
  (let ((argv (list "git" "-C" worktree "diff")))
    (when (and base head)
      (setq argv (append argv (list (format "%s..%s" base head)))))
    (when path
      (setq argv (append argv (list "--" path))))
    argv))

(defun +presentation--render-diff (key slide)
  "Render the diff SLIDE for session KEY.
Runs `git diff' via the effect runner, inserts output into
`*presentation-diff: KEY*' in `diff-mode'."
  (require 'diff-mode)
  (let* ((sess (+presentation--get-session key))
         (worktree (plist-get sess :worktree))
         (argv (+presentation--diff-argv
                worktree
                (plist-get slide :base)
                (plist-get slide :head)
                (plist-get slide :path)))
         (output (car (+presentation--run-effects
                       (list (make-+presentation-effect-shell :argv argv)))))
         (buf (get-buffer-create (format "*presentation-diff: %s*" key))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or output ""))
        (when (fboundp 'diff-mode) (diff-mode))
        (setq buffer-read-only t)))
    (+presentation--enable-mode-in buf key)
    buf))


(defun +presentation--render-layout (key slide)
  "Render the layout SLIDE for session KEY by composing two pane buffers."
  (let* ((split (plist-get slide :split))
         (panes (append (plist-get slide :panes) nil))
         (buf1 (+presentation--dispatch-slide key (nth 0 panes)))
         (buf2 (+presentation--dispatch-slide key (nth 1 panes)))
         (frame (+presentation--session-prop key :frame)))
    (+presentation--enable-mode-in buf1 key)
    (+presentation--enable-mode-in buf2 key)
    (when (and frame (frame-live-p frame))
      (with-selected-frame frame
        (delete-other-windows)
        (let* ((side (pcase split
                       ("horizontal" 'right)
                       ("vertical"   'below)
                       (_ (error "Unknown split: %S" split))))
               (win1 (selected-window))
               (win2 (split-window win1 nil side)))
          (set-window-buffer win1 buf1)
          (set-window-buffer win2 buf2))))
    (list :layout split buf1 buf2)))


;;; Render dispatch

(defun +presentation--dispatch-slide (key slide)
  "Dispatch SLIDE to its per-kind renderer for session KEY.
Returns the buffer (or composite) the slide produced.  Does NOT clear
prior render-state — call `+presentation--render-slide' for that."
  (pcase (plist-get slide :kind)
    ("narrative" (+presentation--render-narrative key slide))
    ("file"      (+presentation--render-file key slide))
    ("diff"      (+presentation--render-diff key slide))
    ("layout"    (+presentation--render-layout key slide))
    (other (error "Unknown slide kind: %S" other))))

(defun +presentation--line-bol (line)
  "Return point-at-bol for LINE (1-based) in current buffer."
  (save-excursion (goto-char (point-min))
                  (forward-line (1- line))
                  (line-beginning-position)))

(defun +presentation--line-eol (line)
  "Return point-at-eol for LINE (1-based) in current buffer."
  (save-excursion (goto-char (point-min))
                  (forward-line (1- line))
                  (line-end-position)))

(defun +presentation--render-inline-annotation (key ann)
  "Render inline ANN in current buffer, attach overlay to session KEY."
  (let* ((line (plist-get ann :line))
         (text (plist-get ann :text))
         (pos (or (plist-get ann :position) "after"))
         (face (+presentation--annotation-face (plist-get ann :severity)))
         (anchor (if (equal pos "before")
                     (+presentation--line-bol line)
                   (+presentation--line-eol line)))
         (decorated (propertize
                     (if (equal pos "before")
                         (format "▸ %s\n" text)
                       (format "  ▸ %s" text))
                     'face face))
         (ov (make-overlay anchor anchor)))
    (overlay-put ov
                 (if (equal pos "before") 'before-string 'after-string)
                 decorated)
    (+presentation--render-state-add key :overlays ov)))

(defun +presentation--render-callout-annotation (key ann)
  "Render callout ANN in current buffer, attach overlay to session KEY."
  (let* ((line (plist-get ann :line))
         (text (or (plist-get ann :text) ""))
         (severity (plist-get ann :severity))
         (face (+presentation--annotation-face severity))
         (label (+presentation--severity-label severity))
         (lines (split-string text "\n"))
         (max-content (apply #'max
                             (length label)
                             (mapcar #'length lines)))
         (box-width (max 80 (+ max-content 4)))
         (top (+presentation--make-border box-width ?┌ ?┐ face))
         (bot (+presentation--make-border box-width ?└ ?┘ face))
         (label-line
          (let ((pad (max 1 (- box-width 4 (length label)))))
            (concat (propertize "│ " 'face face)
                    (propertize label 'face face)
                    (propertize (make-string pad ?\s) 'face face)
                    (propertize " │" 'face face))))
         (body-lines
          (mapcar
           (lambda (l)
             (let ((pad (max 1 (- box-width 4 (length l)))))
               (concat (propertize "│ " 'face face)
                       l
                       (propertize (make-string pad ?\s) 'face face)
                       (propertize " │" 'face face))))
           lines))
         (body-block (mapconcat #'identity
                                (cons label-line body-lines) "\n"))
         (after (concat "\n" top "\n" body-block "\n" bot))
         (anchor (+presentation--line-eol line))
         (ov (make-overlay anchor anchor)))
    (overlay-put ov 'after-string after)
    (+presentation--render-state-add key :overlays ov)))

(defun +presentation--margin-side (position)
  "Return `left-margin' / `right-margin' symbol from POSITION string."
  (pcase position
    ("left" 'left-margin)
    (_ 'right-margin)))

(defun +presentation--ensure-margin-width (key buffer side)
  "Set BUFFER's margin width on SIDE to at least 12, restorer on KEY.
SIDE is `left-margin' or `right-margin'.  Idempotent per (buffer,side):
the prior value is captured only on the first call."
  (with-current-buffer buffer
    (let* ((var (if (eq side 'left-margin)
                    'left-margin-width
                  'right-margin-width))
           (cur (symbol-value var)))
      (when (< cur 12)
        (let ((prior cur)
              (b buffer))
          (set var 12)
          (+presentation--render-state-add
           key :restorers
           (lambda ()
             (when (buffer-live-p b)
               (with-current-buffer b
                 (set var prior)
                 (dolist (w (get-buffer-window-list b nil t))
                   (set-window-buffer w b)))))))
        (dolist (w (get-buffer-window-list buffer nil t))
          (set-window-buffer w buffer))))))

(defun +presentation--render-margin-annotation (key ann)
  "Render margin ANN in current buffer, attach overlay to session KEY."
  (let* ((line (plist-get ann :line))
         (text (plist-get ann :text))
         (raw-pos (or (plist-get ann :position) "right"))
         (side (+presentation--margin-side raw-pos))
         (face (+presentation--annotation-face (plist-get ann :severity)))
         (display-str (propertize text 'face face))
         (decorated (propertize " "
                                'display `((margin ,side) ,display-str)))
         (anchor (+presentation--line-bol line))
         (ov (make-overlay anchor anchor)))
    (+presentation--ensure-margin-width key (current-buffer) side)
    (overlay-put ov 'before-string decorated)
    (+presentation--render-state-add key :overlays ov)))

(defun +presentation--apply-annotations (key buffer annotations)
  "Place overlays on BUFFER for ANNOTATIONS of session KEY.
ANNOTATIONS is a sequence of plists `(:line N :text S :position P
:kind K :severity SEV)'.  Dispatches on `:kind' (default `inline').
Overlays are appended to session KEY's `:render-state' `:overlays'."
  (with-current-buffer buffer
    (mapc
     (lambda (ann)
       (pcase (or (plist-get ann :kind) "inline")
         ("inline"  (+presentation--render-inline-annotation key ann))
         ("callout" (+presentation--render-callout-annotation key ann))
         ("margin"  (+presentation--render-margin-annotation key ann))
         (other (error "Unknown annotation kind: %S" other))))
     (append annotations nil))))

(defun +presentation--slide-pane-layout (slide)
  "Return SLIDE's `:pane-layout' coerced to a symbol, or nil."
  (when-let* ((s (plist-get slide :pane-layout)))
    (and (stringp s) (not (string-empty-p s)) (intern s))))

(defun +presentation--apply-pane-layout (key slide)
  "Apply SLIDE's pane-layout hint to session KEY.
Compares the slide's `:pane-layout' to the session's `:pane-layout'
slot.  When they differ, runs the geometry effects via
`+presentation--run-effects' and updates the slot to the new value."
  (let* ((sess (+presentation--get-session key))
         (desired (+presentation--slide-pane-layout slide))
         (current (plist-get sess :pane-layout))
         (tmux-session (plist-get sess :tmux-session))
         (tmux-window (plist-get sess :tmux-window))
         (effects (+presentation--pane-layout-effects
                   desired current tmux-session tmux-window)))
    (when effects
      (+presentation--run-effects effects)
      (+presentation--session-set key :pane-layout desired))))

(defun +presentation--render-slide (key slide)
  "Render SLIDE as the visible slide for session KEY.
Cleans up any prior `:render-state', applies the slide's pane-layout
hint, dispatches to the per-kind renderer, applies annotations, and
switches the session frame to the produced buffer."
  (+presentation--cleanup-render-state key)
  (+presentation--apply-pane-layout key slide)
  (let* ((buf (+presentation--dispatch-slide key slide))
         (frame (+presentation--session-prop key :frame))
         (anns (plist-get slide :annotations)))
    (when (and anns (bufferp buf))
      (+presentation--apply-annotations key buf anns))
    (when (and (bufferp buf) (frame-live-p frame))
      (with-selected-frame frame (switch-to-buffer buf)))
    buf))

(defun +presentation--render-current (key)
  "Render the slide at session KEY's current index, or the splash buffer."
  (let* ((sess (+presentation--get-session key))
         (deck (or (plist-get sess :deck) []))
         (cur (plist-get sess :current-slide-index))
         (frame (plist-get sess :frame)))
    (cond
     ((and cur (< cur (length deck)))
      (+presentation--render-slide key (aref deck cur)))
     (t
      (+presentation--cleanup-render-state key)
      (let ((buf (+presentation--make-splash-buffer
                  key (plist-get sess :worktree))))
        (+presentation--enable-mode-in buf key)
        (when (and (bufferp buf) (frame-live-p frame))
          (with-selected-frame frame (switch-to-buffer buf)))
        buf)))))

;;; +presentation-mode minor mode

(defvar-local +presentation--session-key nil
  "Buffer-local session key consulted by `+presentation-mode' commands.")

(defvar +presentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'+presentation-next-slide)
    (define-key map (kbd "C-f") #'+presentation-next-slide)
    (define-key map (kbd "C-p") #'+presentation-previous-slide)
    (define-key map (kbd "C-b") #'+presentation-previous-slide)
    (define-key map (kbd "C-c q") #'+presentation-quit)
    map)
  "Keymap for `+presentation-mode'.")

;;;###autoload
(define-minor-mode +presentation-mode
  "Minor mode for navigating presentation slides.
Bindings advance / retreat the session's current slide via
`+presentation--deck-goto'.  The session is resolved from the
buffer-local `+presentation--session-key' set by the renderer."
  :lighter " Pres"
  :keymap +presentation-mode-map)

(with-eval-after-load 'evil
  (evil-make-overriding-map +presentation-mode-map)
  (add-hook '+presentation-mode-hook
            (lambda ()
              (when (fboundp 'evil-normalize-keymaps)
                (evil-normalize-keymaps)))))

;;;###autoload
(defun +presentation-next-slide ()
  "Advance the current presentation session by one slide.
No-op when the session is already at the last slide.  Emits a
`claude/channel' notification post-render."
  (interactive)
  (when-let* ((key +presentation--session-key)
              (sess (+presentation--get-session key))
              (deck (or (plist-get sess :deck) []))
              (cur (plist-get sess :current-slide-index)))
    (let ((nxt (1+ cur)))
      (when (< nxt (length deck))
        (+presentation--deck-goto key nxt)
        (+presentation--emit-nav-channel key cur nxt)))))

;;;###autoload
(defun +presentation-previous-slide ()
  "Retreat the current presentation session by one slide.
No-op when the session is already at slide 0.  Emits a
`claude/channel' notification post-render."
  (interactive)
  (when-let* ((key +presentation--session-key)
              (sess (+presentation--get-session key))
              (cur (plist-get sess :current-slide-index)))
    (let ((prv (1- cur)))
      (when (>= prv 0)
        (+presentation--deck-goto key prv)
        (+presentation--emit-nav-channel key cur prv)))))

;;;###autoload
(defun +presentation-quit ()
  "End the presentation session owning the current buffer.
Resolves the session via the buffer-local `+presentation--session-key'
and invokes `+presentation-end'.  No-op when the key is nil or no
session for that key is registered — typing the binding in a stale
buffer is equivalent to \"session is already gone\"."
  (interactive)
  (when-let* ((key +presentation--session-key))
    (when (gethash key +presentation--sessions)
      (+presentation-end key))))

(defun +presentation--enable-mode-in (buffer key)
  "Enable `+presentation-mode' in BUFFER tagged with session KEY.
Sets the buffer-local `+presentation--session-key' before activating
the mode so its bindings can resolve the session."
  (when (bufferp buffer)
    (with-current-buffer buffer
      (setq-local +presentation--session-key key)
      (+presentation-mode 1))))

;;; lib.el ends here
