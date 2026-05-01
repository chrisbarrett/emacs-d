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

(defun +presentation--narrative-buffer (key)
  "Return (creating if needed) the per-session narrative buffer for KEY."
  (get-buffer-create (format "*presentation: %s*" key)))

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
    (+presentation--session-set key :deck [])
    (+presentation--session-set key :current-slide-index nil)
    (if initial-slide
        (+presentation--deck-push key initial-slide)
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
         (frame (plist-get session :frame)))
    (+presentation--cleanup-render-state key)
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

(defun +presentation--validate-annotation (ann)
  "Validate annotation plist ANN; signal `user-error' on failure."
  (let ((line (plist-get ann :line))
        (text (plist-get ann :text))
        (pos (or (plist-get ann :position) "after")))
    (unless (and (integerp line) (> line 0))
      (user-error "Annotation :line must be positive integer, got: %S" line))
    (unless (stringp text)
      (user-error "Annotation :text must be string, got: %S" text))
    (unless (member pos '("before" "after"))
      (user-error "Annotation :position must be \"before\" or \"after\", got: %S"
                  pos))))

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
(defun +presentation--deck-push (key slide)
  "Append SLIDE to session KEY's deck, set current to the new index, render.
Returns the new slide's index."
  (+presentation--validate-slide slide)
  (let* ((deck (+presentation--deck key))
         (new-deck (vconcat deck (vector slide)))
         (new-idx (1- (length new-deck))))
    (+presentation--session-set key :deck new-deck)
    (+presentation--session-set key :current-slide-index new-idx)
    (+presentation--render-current key)
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

;;;###autoload
(defun +presentation--deck-goto (key index)
  "Render slide at INDEX in session KEY's deck and set it as current.
Signals `user-error' on out-of-range INDEX."
  (let* ((deck (+presentation--deck key))
         (len (length deck)))
    (unless (and (integerp index) (>= index 0) (< index len))
      (user-error "Goto index %S out of range [0,%d)" index len))
    (+presentation--session-set key :current-slide-index index)
    (+presentation--render-current key)
    index))


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
                 (p1 (save-excursion (goto-char (point-min))
                                     (forward-line (1- fs))
                                     (point)))
                 (p2 (save-excursion (goto-char (point-min))
                                     (forward-line fe)
                                     (point)))
                 (ov (make-overlay p1 p2)))
            (overlay-put ov 'face 'region)
            (+presentation--render-state-add key :overlays ov)
            (goto-char p1)
            (condition-case _ (recenter) (error nil))))
        (setq buffer-read-only t)
        (let ((b buf))
          (+presentation--render-state-add
           key :restorers
           (lambda () (with-current-buffer b
                        (setq buffer-read-only prev-ro)))))))
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
    buf))


(defun +presentation--render-layout (key slide)
  "Render the layout SLIDE for session KEY by composing two pane buffers."
  (let* ((split (plist-get slide :split))
         (panes (append (plist-get slide :panes) nil))
         (buf1 (+presentation--dispatch-slide key (nth 0 panes)))
         (buf2 (+presentation--dispatch-slide key (nth 1 panes)))
         (frame (+presentation--session-prop key :frame)))
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

(defun +presentation--apply-annotations (key buffer annotations)
  "Place overlays on BUFFER for ANNOTATIONS of session KEY.
ANNOTATIONS is a sequence of plists `(:line N :text S :position P)'.
Overlays are appended to session KEY's `:render-state' `:overlays'."
  (with-current-buffer buffer
    (mapc
     (lambda (ann)
       (let* ((line (plist-get ann :line))
              (text (plist-get ann :text))
              (pos (or (plist-get ann :position) "after"))
              (target (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- line))
                        (point)))
              (ov (make-overlay target target))
              (decorated
               (propertize
                (if (equal pos "before")
                    (format "▸ %s\n" text)
                  (format "\n▸ %s" text))
                'face 'font-lock-comment-face)))
         (overlay-put ov
                      (if (equal pos "before") 'before-string 'after-string)
                      decorated)
         (+presentation--render-state-add key :overlays ov)))
     (append annotations nil))))

(defun +presentation--render-slide (key slide)
  "Render SLIDE as the visible slide for session KEY.
Cleans up any prior `:render-state', dispatches to the per-kind
renderer, applies annotations, and switches the session frame to the
produced buffer."
  (+presentation--cleanup-render-state key)
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
        (when (and (bufferp buf) (frame-live-p frame))
          (with-selected-frame frame (switch-to-buffer buf)))
        buf)))))

;;; lib.el ends here
