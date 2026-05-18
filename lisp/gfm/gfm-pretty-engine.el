;;; gfm-pretty-engine.el --- Lifecycle engine for GFM pretty decorators -*- lexical-binding: t; -*-

;;; Commentary:

;; Lifecycle engine shared across every gfm-pretty decorator.  Owns
;; the decorator registry, the overlay registry primitives, the
;; reconciler, the scoped-rebuild router, the generic rebuild-stats
;; wrapper, and the single set of lifecycle hooks per buffer:
;; one `after-change-functions' handler, one
;; `window-configuration-change-hook' handler, one `post-command-hook'
;; reveal handler, and one idle rebuild timer.
;;
;; Decorators register intent (block discovery, range, anchor/display
;; application, optional structural-line-ranges, optional edit-adjacency
;; predicate, optional reveal participation, on-enable / on-disable
;; extras) via `gfm-pretty-define-decorator'; the engine drives the
;; lifecycle.  The graphics toolkit lives in `gfm-pretty-borders.el'.

;;; Code:

(require 'cl-lib)

(defgroup gfm-pretty nil
  "Engine controls for GFM pretty decorators."
  :group 'markdown-faces)

(defcustom gfm-pretty-slow-rebuild-threshold 0.05
  "Threshold in seconds above which a single rebuild emits a warning.
Engine-level; applies uniformly to every registered decorator."
  :type 'number
  :group 'gfm-pretty)

;;; Window-list helper

(defun gfm-pretty--display-windows ()
  "Return windows currently displaying the buffer."
  (get-buffer-window-list (current-buffer) nil t))

;;; Range / overlap predicates

(defun gfm-pretty--in-ranges-p (pos ranges)
  "Non-nil if POS lies in any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-pretty--region-overlaps-p (a b)
  "Non-nil if (BEG . END) ranges A and B overlap."
  (and (<= (car a) (cdr b)) (>= (cdr a) (car b))))

;;; Width primitives

(defconst gfm-pretty--wrap-prefix-w 2
  "Visual width of the wrap-prefix shown on continuation visual lines.")

(defun gfm-pretty--available-width (&optional window)
  "Return the available char width for a block in WINDOW.
Falls back to a window currently showing the buffer, then to
`fill-column' or 80."
  (let ((win (or window
                 (get-buffer-window (current-buffer))
                 (get-buffer-window (current-buffer) t))))
    (or (and win (window-max-chars-per-line win))
        fill-column
        80)))

(defun gfm-pretty--text-width (&optional window)
  "Return WINDOW's max chars per visual line.
Compatibility alias for `gfm-pretty--available-width'."
  (gfm-pretty--available-width window))

(defun gfm-pretty--max-line-width (beg end &optional indent)
  "Maximum line width between BEG and END, subtracting INDENT from each line."
  (let ((max-col 0)
        (indent (or indent 0)))
    (save-excursion
      (goto-char beg)
      (while (and (not (eobp)) (<= (line-beginning-position) end))
        (let* ((lbeg (line-beginning-position))
               (lend (line-end-position))
               (len (max 0 (- (- lend lbeg) indent))))
          (setq max-col (max max-col len)))
        (forward-line 1)))
    max-col))

;;; Buffer-local engine state

(defvar-local gfm-pretty--state nil
  "Alist (NAME . PLIST) of per-decorator buffer-local state.
PLIST keys:
- `enabled-p'         decorator on in this buffer
- `dirty-region'      (BEG . END) of accumulated unrebuilt edits
- `last-window-state' window-state snapshot used by the reconciler
- `blocks-cache'      (TICK . BLOCKS) memoised collect result (pass 3)
- `overlays'          decorator's overlays (pass 4)
- `hidden-ovs'        revealable overlays whose display is suppressed (pass 4)
- `rebuild-stats'     plist (:count :total :last :max) of generic timing
- `phase-totals'      decorator-published phase timings (alist)")

(defvar-local gfm-pretty--rebuild-timer nil
  "Single idle rebuild timer driving engine-scheduled rebuilds.")

(defun gfm-pretty--state-get (name slot)
  "Return SLOT from decorator NAME's state plist."
  (plist-get (alist-get name gfm-pretty--state) slot))

(defun gfm-pretty--state-set (name slot val)
  "Set SLOT to VAL in decorator NAME's state plist."
  (setf (alist-get name gfm-pretty--state)
        (plist-put (alist-get name gfm-pretty--state) slot val)))

;;; Decorator registry

(cl-defstruct gfm-pretty--decorator
  "Metadata for a registered gfm-pretty decorator.
Fields populated via `gfm-pretty-define-decorator'."
  name
  registry
  block-at-point-fn
  edit-at-point-fn
  collect-fn
  range-fn
  apply-anchors-fn
  apply-display-fn
  revealable-prop
  saved-display-prop
  revealable-p-fn
  on-enable-fn
  on-disable-fn
  rebuild-fn
  structural-line-ranges-fn
  edit-adjacency-fn
  reconcile-windows-fn
  reveal-fn)

(defvar gfm-pretty--decorators nil
  "Alist of NAME -> `gfm-pretty--decorator' struct.")

(defun gfm-pretty--get (name)
  "Return the registered decorator for NAME, or signal a `user-error'."
  (or (alist-get name gfm-pretty--decorators)
      (user-error "gfm-pretty: unknown decorator %s" name)))

;;;###autoload
(defmacro gfm-pretty-define-decorator (name &rest plist)
  "Register decorator NAME with PLIST keys.

Recognised keys:

  :registry            `gfm-pretty--registry' for the decorator's
                       overlay tag and per-decorator buffer-locals.
  :collect-fn          (no-arg) returns the buffer's blocks, widened.
  :range-fn            (block) returns the block's (BEG . END) source range.
  :apply-anchors-fn    (block) applies width-independent anchor overlays.
  :apply-display-fn    (block window) applies WINDOW's display overlays.
  :rebuild-fn          optional (no-arg) full rebuild; engine-generic
                       teardown + reapply when nil.
  :structural-line-ranges-fn
                       optional (no-arg) returns ((BEG . END) ...) ranges
                       whose intersection with a dirty region forces a
                       full rebuild.
  :edit-adjacency-fn   optional (region) returns non-nil when the dirty
                       region overlaps a line whose edit could create
                       or destroy an adjacency-gated block.
  :revealable-prop     overlay property symbol carried by revealable
                       overlays; nil opts the decorator out of reveal.
  :saved-display-prop  overlay property symbol used to stash the
                       overlay's `display' while revealed.
  :revealable-p-fn     optional predicate (overlay) overriding the
                       default property-presence check.
  :on-enable-fn        optional (no-arg) thunk run when decorator turns
                       on (font-lock install, theme hook, etc.).
  :on-disable-fn       optional (no-arg) thunk run on disable.
  :reconcile-windows-fn optional (no-arg) bespoke window-state reconciler.
  :reveal-fn           optional (no-arg) bespoke reveal handler.
  :block-at-point-fn   optional (no-arg) returns enclosing block, or nil.
  :edit-at-point-fn    optional (no-arg) opens an editor for the block."
  (declare (indent 1))
  `(setf (alist-get ,name gfm-pretty--decorators)
         (make-gfm-pretty--decorator
          :name              ,name
          :registry          ,(plist-get plist :registry)
          :block-at-point-fn ,(plist-get plist :block-at-point-fn)
          :edit-at-point-fn  ,(plist-get plist :edit-at-point-fn)
          :collect-fn        ,(plist-get plist :collect-fn)
          :range-fn          ,(plist-get plist :range-fn)
          :apply-anchors-fn  ,(plist-get plist :apply-anchors-fn)
          :apply-display-fn  ,(plist-get plist :apply-display-fn)
          :revealable-prop   ,(plist-get plist :revealable-prop)
          :saved-display-prop ,(plist-get plist :saved-display-prop)
          :revealable-p-fn   ,(plist-get plist :revealable-p-fn)
          :on-enable-fn      ,(plist-get plist :on-enable-fn)
          :on-disable-fn     ,(plist-get plist :on-disable-fn)
          :rebuild-fn        ,(plist-get plist :rebuild-fn)
          :structural-line-ranges-fn ,(plist-get plist :structural-line-ranges-fn)
          :edit-adjacency-fn ,(plist-get plist :edit-adjacency-fn)
          :reconcile-windows-fn ,(plist-get plist :reconcile-windows-fn)
          :reveal-fn         ,(plist-get plist :reveal-fn))))

;;; Overlay registry

(cl-defstruct (gfm-pretty--registry
               (:constructor gfm-pretty--make-registry)
               (:copier nil))
  "Per-mode overlay registry context.

TAG is the symbol used as both the bulk overlay-property tag and as
the prefix for derived properties (TAG-anchor, TAG-display,
TAG-revealable, TAG-saved-display).

OVERLAYS-SYMBOL names a buffer-local variable holding the list of
this mode's overlays; HIDDEN-OVS-SYMBOL names the optional buffer-local
list used for revealable bookkeeping (may be nil).

ANCHOR/DISPLAY/REVEALABLE/SAVED-DISPLAY are pre-interned to avoid
recomputing the symbol on every overlay creation."
  tag
  overlays-symbol
  hidden-ovs-symbol
  anchor
  display
  revealable
  saved-display)

(defun gfm-pretty--registry-for (tag overlays-symbol
                                           &optional hidden-ovs-symbol)
  "Build a `gfm-pretty--registry' from TAG and OVERLAYS-SYMBOL.
Sub-property symbols are derived by suffixing TAG (`<tag>-anchor', etc.).
HIDDEN-OVS-SYMBOL is the symbol of the buffer-local list used by reveal."
  (let ((name (symbol-name tag)))
    (gfm-pretty--make-registry
     :tag tag
     :overlays-symbol overlays-symbol
     :hidden-ovs-symbol hidden-ovs-symbol
     :anchor (intern (concat name "-anchor"))
     :display (intern (concat name "-display"))
     :revealable (intern (concat name "-revealable"))
     :saved-display (intern (concat name "-saved-display")))))

(defun gfm-pretty--register (registry ov)
  "Tag OV with REGISTRY's tag and remember it for bulk cleanup."
  (overlay-put ov (gfm-pretty--registry-tag registry) t)
  (push ov (symbol-value (gfm-pretty--registry-overlays-symbol registry)))
  ov)

(defun gfm-pretty--remove-overlays (registry &optional beg end)
  "Remove all REGISTRY-tagged overlays between BEG and END.
With both BEG and END nil, widen for the duration of the clear so the
registry list and the on-buffer overlay set stay in lockstep regardless
of any current narrowing; also clear the registry's overlay list and
hidden-ovs list (full reset).  Scoped calls (BEG and/or END non-nil)
operate on the literal range — callers pass real buffer positions."
  (let ((tag (gfm-pretty--registry-tag registry))
        (list-sym (gfm-pretty--registry-overlays-symbol registry))
        (hidden-sym (gfm-pretty--registry-hidden-ovs-symbol registry)))
    (cond
     ((or beg end)
      (remove-overlays (or beg (point-min)) (or end (point-max)) tag t)
      (set list-sym (cl-remove-if-not #'overlay-buffer (symbol-value list-sym))))
     (t
      (save-restriction
        (widen)
        (remove-overlays (point-min) (point-max) tag t))
      (set list-sym nil)
      (when hidden-sym (set hidden-sym nil))))))

(defun gfm-pretty--prune-dead-overlays (registry)
  "Drop overlays from REGISTRY whose buffer is gone."
  (let ((list-sym (gfm-pretty--registry-overlays-symbol registry)))
    (set list-sym (cl-remove-if-not #'overlay-buffer (symbol-value list-sym)))))

(defun gfm-pretty--remove-display-overlays-in-range
    (registry beg end window)
  "Delete display overlays in [BEG, END] for WINDOW under REGISTRY.
WINDOW non-nil matches only that window's overlays; nil matches every
display overlay.  The registry list is NOT pruned here — call
`gfm-pretty--prune-dead-overlays' once after a batch."
  (let ((display-prop (gfm-pretty--registry-display registry)))
    (dolist (ov (overlays-in beg end))
      (when (and (overlay-get ov display-prop)
                 (or (null window)
                     (eq (overlay-get ov 'window) window)))
        (delete-overlay ov)))))

(defun gfm-pretty--remove-display-overlays-for-window (registry window)
  "Delete every display overlay restricted to WINDOW under REGISTRY."
  (let ((display-prop (gfm-pretty--registry-display registry))
        (list-sym (gfm-pretty--registry-overlays-symbol registry)))
    (dolist (ov (symbol-value list-sym))
      (when (and (overlay-buffer ov)
                 (overlay-get ov display-prop)
                 (eq (overlay-get ov 'window) window))
        (delete-overlay ov)))
    (gfm-pretty--prune-dead-overlays registry)))

(defun gfm-pretty--make-anchor (registry beg end &rest props)
  "Make an anchor overlay over [BEG, END] under REGISTRY with PROPS."
  (let ((ov (make-overlay beg end nil nil t))
        (tag (gfm-pretty--registry-tag registry))
        (anchor (gfm-pretty--registry-anchor registry))
        (list-sym (gfm-pretty--registry-overlays-symbol registry)))
    (overlay-put ov tag t)
    (overlay-put ov anchor t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    (push ov (symbol-value list-sym))
    ov))

(defun gfm-pretty--make-display (registry beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW under REGISTRY with PROPS.
WINDOW non-nil restricts the overlay to that window only."
  (let ((ov (make-overlay beg end nil nil t))
        (tag (gfm-pretty--registry-tag registry))
        (display (gfm-pretty--registry-display registry))
        (list-sym (gfm-pretty--registry-overlays-symbol registry)))
    (overlay-put ov tag t)
    (overlay-put ov display t)
    (when window (overlay-put ov 'window window))
    (while props
      (overlay-put ov (pop props) (pop props)))
    (push ov (symbol-value list-sym))
    ov))

;;; Scheduler primitives

(defun gfm-pretty--extend-dirty-region (region-symbol beg end)
  "Extend the buffer-local cons cell in REGION-SYMBOL to cover BEG..END."
  (let ((current (symbol-value region-symbol)))
    (cond
     ((null current)
      (set region-symbol (cons beg end)))
     (t
      (setcar current (min (car current) beg))
      (setcdr current (max (cdr current) end))))))

(defun gfm-pretty--arm-rebuild-timer (timer-symbol mode-symbol callback)
  "Cancel the timer in TIMER-SYMBOL and schedule CALLBACK after idle.
The timer fires CALLBACK in the originating buffer iff MODE-SYMBOL's
value is non-nil there.  CALLBACK takes no arguments."
  (when (timerp (symbol-value timer-symbol))
    (cancel-timer (symbol-value timer-symbol)))
  (set timer-symbol
       (run-with-idle-timer
        0.2 nil
        (lambda (buf cb)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (symbol-value mode-symbol)
                (funcall cb)))))
        (current-buffer) callback)))

;;; Window-state tracking

(defun gfm-pretty--window-state ()
  "Return the (WINDOW . WIDTH) snapshot used to detect rendering drift."
  (mapcar (lambda (w) (cons w (gfm-pretty--available-width w)))
          (gfm-pretty--display-windows)))

(defun gfm-pretty--range-visible-p (range ranges)
  "Non-nil if (BEG . END) RANGE overlaps any (BEG . END) range in RANGES."
  (cl-some (lambda (r)
             (and (<= (car range) (cdr r))
                  (>= (cdr range) (car r))))
           ranges))

(defun gfm-pretty--block-visible-p (block ranges range-fn)
  "Non-nil if BLOCK's source range overlaps any range in RANGES.
RANGE-FN extracts (BEG . END) from BLOCK."
  (gfm-pretty--range-visible-p (funcall range-fn block) ranges))

(defun gfm-pretty--visible-window-ranges ()
  "Return (VSTART . VEND) pairs for every window currently showing this buffer.
Uses cached `window-end' (no force-update) since forcing redisplay on
a brand-new split window with stale overlays is the dominant cost
during a `C-x 3' transient.  Windows whose end is nil are dropped."
  (delq nil
        (mapcar (lambda (w)
                  (let ((s (window-start w))
                        (e (window-end w)))
                    (and s e (cons s e))))
                (get-buffer-window-list (current-buffer) nil t))))

;;; Block discovery memoisation

(defun gfm-pretty--collect (decorator)
  "Return DECORATOR's blocks, memoised by `buffer-chars-modified-tick'.
Calls the decorator's `:collect-fn' under `save-restriction' + `widen'
on a cache miss; returns the cached list on a hit.  The cache lives
in `gfm-pretty--state' under the decorator's `blocks-cache' slot."
  (let* ((name (gfm-pretty--decorator-name decorator))
         (collect-fn (gfm-pretty--decorator-collect-fn decorator))
         (cache (gfm-pretty--state-get name 'blocks-cache))
         (tick (buffer-chars-modified-tick)))
    (cond
     ((and cache (eq tick (car cache)))
      (cdr cache))
     (t
      (let ((blocks (save-restriction
                      (widen)
                      (funcall collect-fn))))
        (gfm-pretty--state-set name 'blocks-cache (cons tick blocks))
        blocks)))))

;;; Generic rebuild stats wrapper

(defun gfm-pretty--stats-record (name duration)
  "Update DECORATOR NAME's `rebuild-stats' slot with DURATION (seconds).
Emits a `display-warning' of severity `:warning' when DURATION exceeds
`gfm-pretty-slow-rebuild-threshold'."
  (let* ((cur (or (gfm-pretty--state-get name 'rebuild-stats)
                  (list :count 0 :total 0.0 :last 0.0 :max 0.0)))
         (count (1+ (plist-get cur :count)))
         (total (+ duration (plist-get cur :total)))
         (mx    (max duration (plist-get cur :max))))
    (gfm-pretty--state-set
     name 'rebuild-stats
     (list :count count :total total :last duration :max mx)))
  (when (> duration gfm-pretty-slow-rebuild-threshold)
    (display-warning
     'gfm-pretty
     (format "%s: slow rebuild in %s: %.3fs"
             name (buffer-name) duration)
     :warning)))

(defmacro gfm-pretty--with-stats (name &rest body)
  "Time BODY and accumulate the result into decorator NAME's stats."
  (declare (indent 1) (debug (form body)))
  (let ((start (make-symbol "start")))
    `(let ((,start (current-time)))
       (prog1 (progn ,@body)
         (gfm-pretty--stats-record
          ,name (float-time (time-since ,start)))))))

(defun gfm-pretty-accum-phase (name phase delta)
  "Accumulate DELTA seconds into PHASE for decorator NAME's phase totals."
  (let* ((cur (gfm-pretty--state-get name 'phase-totals))
         (existing (or (alist-get phase cur) 0.0)))
    (setf (alist-get phase cur) (+ delta existing))
    (gfm-pretty--state-set name 'phase-totals cur)))

(defmacro gfm-pretty-time-phase (name phase &rest body)
  "Run BODY, accumulating its wall-time into PHASE for decorator NAME."
  (declare (indent 2) (debug (form form body)))
  (let ((start (make-symbol "start")))
    `(let ((,start (current-time)))
       (prog1 (progn ,@body)
         (gfm-pretty-accum-phase
          ,name ,phase (float-time (time-since ,start)))))))

(defun gfm-pretty--format-phase-totals (totals)
  "Return a phase-by-phase summary string for TOTALS, sorted by total desc."
  (let ((sorted (sort (copy-sequence totals)
                      (lambda (a b) (> (cdr a) (cdr b))))))
    (mapconcat (lambda (p) (format "%s=%.3fs" (car p) (cdr p)))
               sorted " ")))

;;;###autoload
(defun gfm-pretty-stats (&optional decorator)
  "Display rebuild stats for DECORATOR (interactive: completing-read)."
  (interactive
   (list (intern
          (completing-read
           "gfm-pretty decorator: "
           (mapcar (lambda (e) (symbol-name (car e)))
                   gfm-pretty--decorators)
           nil t))))
  (let* ((stats (gfm-pretty--state-get decorator 'rebuild-stats))
         (phases (gfm-pretty--state-get decorator 'phase-totals)))
    (cond
     ((null stats)
      (message "gfm-pretty[%s]: no stats yet" decorator))
     (t
      (message "gfm-pretty[%s] %s: rebuilds=%d total=%.3fs last=%.3fs max=%.3fs%s"
               decorator
               (buffer-name)
               (plist-get stats :count)
               (plist-get stats :total)
               (plist-get stats :last)
               (plist-get stats :max)
               (if phases
                   (concat " | " (gfm-pretty--format-phase-totals phases))
                 ""))))))

;;; Per-decorator reconciler (engine-driven)

(defun gfm-pretty--rebuild-blocks (decorator blocks)
  "Tear down and re-apply BLOCKS using DECORATOR's apply functions."
  (let ((registry (gfm-pretty--decorator-registry decorator))
        (range-fn (gfm-pretty--decorator-range-fn decorator))
        (apply-anchors (gfm-pretty--decorator-apply-anchors-fn decorator))
        (apply-display (gfm-pretty--decorator-apply-display-fn decorator))
        (windows (or (gfm-pretty--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (funcall range-fn block)))
        (gfm-pretty--remove-overlays
         registry (car range) (cdr range)))
      (when apply-anchors (funcall apply-anchors block))
      (dolist (window windows)
        (funcall apply-display block window)))))

(defun gfm-pretty--rebuild-block-for-window (decorator block window)
  "Replace WINDOW's display overlays for BLOCK at the current width."
  (let* ((registry (gfm-pretty--decorator-registry decorator))
         (range-fn (gfm-pretty--decorator-range-fn decorator))
         (apply-display (gfm-pretty--decorator-apply-display-fn decorator))
         (range (funcall range-fn block)))
    (gfm-pretty--remove-display-overlays-in-range
     registry (car range) (cdr range) window)
    (funcall apply-display block window)
    (gfm-pretty--prune-dead-overlays registry)))

(defun gfm-pretty--rebuild-block (decorator block)
  "Tear down BLOCK's overlays and reapply just that block.
Times the operation into DECORATOR NAME's `rebuild-stats' slot."
  (let* ((name (gfm-pretty--decorator-name decorator))
         (registry (gfm-pretty--decorator-registry decorator))
         (range-fn (gfm-pretty--decorator-range-fn decorator))
         (apply-anchors (gfm-pretty--decorator-apply-anchors-fn decorator))
         (apply-display (gfm-pretty--decorator-apply-display-fn decorator))
         (range (funcall range-fn block))
         (windows (or (gfm-pretty--display-windows) (list nil))))
    (gfm-pretty--with-stats name
      (gfm-pretty--remove-overlays registry (car range) (cdr range))
      (when apply-anchors (funcall apply-anchors block))
      (dolist (window windows)
        (funcall apply-display block window)))))

(defun gfm-pretty--pace-window-rebuild (decorator buf queue window)
  "Render the next block in QUEUE for WINDOW in BUF using DECORATOR.
One block per idle tick keeps `C-x 3' / resize transients responsive."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (when (and (buffer-live-p buf) (window-live-p window))
       (with-current-buffer buf
         (when (gfm-pretty--state-get
                (gfm-pretty--decorator-name decorator) 'enabled-p)
           (let ((b (car queue))
                 (rest (cdr queue)))
             (gfm-pretty--rebuild-block-for-window decorator b window)
             (cond
              (rest
               (gfm-pretty--pace-window-rebuild decorator buf rest window))
              (t
               (gfm-pretty--prune-dead-overlays
                (gfm-pretty--decorator-registry decorator)))))))))))

(defun gfm-pretty--rebuild-window-prioritised (decorator window)
  "Per-block, idle-paced rebuild of WINDOW's display overlays for DECORATOR.
Visible blocks first, off-screen ones after, one per idle tick."
  (when (window-live-p window)
    (let* ((range-fn (gfm-pretty--decorator-range-fn decorator))
           (blocks (gfm-pretty--collect decorator))
           (vstart (window-start window))
           (vend (window-end window))
           (ranges (and vstart vend (list (cons vstart vend))))
           (visible (and ranges
                         (cl-remove-if-not
                          (lambda (b)
                            (gfm-pretty--block-visible-p
                             b ranges range-fn))
                          blocks)))
           (offscreen (cl-set-difference blocks visible))
           (queue (append visible offscreen)))
      (when queue
        (gfm-pretty--pace-window-rebuild
         decorator (current-buffer) queue window)))))

(defun gfm-pretty--reconcile-windows (decorator)
  "Reconcile display overlays with current window state for DECORATOR.
Removed windows have their display overlays deleted synchronously;
added or resized windows have their rebuild deferred to the next idle
tick.  Falls through to a full rebuild when no anchors are present
yet (first call) or no prior state was recorded."
  (let* ((name (gfm-pretty--decorator-name decorator))
         (registry (gfm-pretty--decorator-registry decorator))
         (anchor-prop (gfm-pretty--registry-anchor registry))
         (overlays (symbol-value
                    (gfm-pretty--registry-overlays-symbol registry)))
         (prev (gfm-pretty--state-get name 'last-window-state)))
    (cond
     ((or (null prev)
          (null (cl-some (lambda (o) (overlay-get o anchor-prop))
                         overlays)))
      (gfm-pretty--rebuild decorator))
     (t
      (let* ((curr (gfm-pretty--window-state))
             (prev-keys (mapcar #'car prev))
             (curr-keys (mapcar #'car curr))
             (added (cl-remove-if (lambda (e) (memq (car e) prev-keys)) curr))
             (removed (cl-remove-if (lambda (w) (memq w curr-keys)) prev-keys))
             (resized (cl-remove-if-not
                       (lambda (e)
                         (let ((old (assq (car e) prev)))
                           (and old (not (eql (cdr e) (cdr old))))))
                       curr))
             (touched-windows (mapcar #'car (append added resized))))
        (dolist (w removed)
          (gfm-pretty--remove-display-overlays-for-window registry w))
        (gfm-pretty--prune-dead-overlays registry)
        (gfm-pretty--state-set name 'last-window-state curr)
        (when touched-windows
          (run-with-idle-timer
           0 nil
           (lambda (buf wins)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (gfm-pretty--state-get name 'enabled-p)
                   (dolist (w wins)
                     (when (window-live-p w)
                       (gfm-pretty--rebuild-window-prioritised
                        decorator w)))))))
           (current-buffer) touched-windows)))))))

(defun gfm-pretty--rebuild (decorator)
  "Full rebuild of DECORATOR.
Calls the decorator's `:rebuild-fn' if registered; otherwise performs
a generic teardown + re-apply using `:collect-fn', `:apply-anchors-fn',
and `:apply-display-fn'.  Times the call into the engine's per-decorator
`rebuild-stats' slot."
  (let* ((rebuild-fn (gfm-pretty--decorator-rebuild-fn decorator))
         (name (gfm-pretty--decorator-name decorator)))
    (gfm-pretty--with-stats name
      (cond
       (rebuild-fn (funcall rebuild-fn))
       (t
        (let ((registry (gfm-pretty--decorator-registry decorator))
              (apply-anchors (gfm-pretty--decorator-apply-anchors-fn decorator))
              (apply-display (gfm-pretty--decorator-apply-display-fn decorator)))
          (gfm-pretty--remove-overlays registry)
          (let ((blocks (gfm-pretty--collect decorator))
                (windows (or (gfm-pretty--display-windows) (list nil))))
            (dolist (block blocks)
              (when apply-anchors (funcall apply-anchors block)))
            (dolist (window windows)
              (dolist (block blocks)
                (funcall apply-display block window))))))))
    (gfm-pretty--state-set name 'dirty-region nil)
    (gfm-pretty--state-set name 'last-window-state
                           (gfm-pretty--window-state))))

;;; Scoped-rebuild routing (engine-owned)

(defun gfm-pretty--dirty-forces-full-rebuild-p (decorator dirty)
  "Non-nil if DIRTY warrants a full DECORATOR rebuild via structural / adjacency.
Checks DECORATOR's `:structural-line-ranges-fn' first; falls back to
its `:edit-adjacency-fn'.  Either returning a positive answer wins."
  (let ((struct-fn (gfm-pretty--decorator-structural-line-ranges-fn decorator))
        (adj-fn    (gfm-pretty--decorator-edit-adjacency-fn decorator)))
    (or (and struct-fn
             (let ((ranges (save-restriction (widen) (funcall struct-fn))))
               (cl-some (lambda (r) (gfm-pretty--region-overlaps-p dirty r))
                        ranges)))
        (and adj-fn
             (save-restriction (widen) (funcall adj-fn dirty))))))

(defun gfm-pretty--rebuild-scoped-by-block (decorator dirty)
  "Scoped DECORATOR rebuild driven solely by block-range containment.
Falls back to a full rebuild when no block contains DIRTY, when more
than one block overlaps, or when the overlapping block does not fully
contain DIRTY."
  (let* ((range-fn (gfm-pretty--decorator-range-fn decorator))
         (blocks (gfm-pretty--collect decorator))
         (matching (cl-loop for b in blocks
                            when (gfm-pretty--region-overlaps-p
                                  dirty (funcall range-fn b))
                            collect b)))
    (cond
     ((null matching) nil)
     ((and (null (cdr matching))
           (let ((r (funcall range-fn (car matching))))
             (and (>= (car dirty) (car r))
                  (<= (cdr dirty) (cdr r)))))
      (gfm-pretty--rebuild-block decorator (car matching)))
     (t (gfm-pretty--rebuild decorator)))))

;;; Engine lifecycle hooks

(defvar gfm-pretty-mode)

(defun gfm-pretty--enabled-decorators ()
  "Return decorators whose engine-tracked `enabled-p' is non-nil."
  (cl-loop for (name . d) in gfm-pretty--decorators
           when (gfm-pretty--state-get name 'enabled-p)
           collect d))

(defun gfm-pretty--after-change (beg end _len)
  "Engine `after-change-functions' handler.
Extends each enabled decorator's dirty region to cover BEG..END and
arms the single engine idle timer."
  (unless (buffer-base-buffer)
    (dolist (decorator (gfm-pretty--enabled-decorators))
      (let* ((name (gfm-pretty--decorator-name decorator))
             (cur (gfm-pretty--state-get name 'dirty-region)))
        (gfm-pretty--state-set
         name 'dirty-region
         (cond
          ((null cur) (cons beg end))
          (t (cons (min (car cur) beg) (max (cdr cur) end)))))))
    (gfm-pretty--arm-engine-timer)))

(defun gfm-pretty--arm-engine-timer ()
  "Cancel and re-arm the engine idle rebuild timer."
  (when (timerp gfm-pretty--rebuild-timer)
    (cancel-timer gfm-pretty--rebuild-timer))
  (setq gfm-pretty--rebuild-timer
        (run-with-idle-timer
         0.2 nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when gfm-pretty-mode
                 (gfm-pretty--scheduled-rebuild)))))
         (current-buffer))))

(defun gfm-pretty--scheduled-rebuild ()
  "Engine timer callback.
For each enabled decorator: if its dirty region intersects any
structural-line range or its edit-adjacency predicate returns non-nil,
do a full rebuild; otherwise scope to the fully-contained block when
one exists, falling back to a full rebuild.  Clears the dirty region
per decorator afterwards."
  (dolist (decorator (gfm-pretty--enabled-decorators))
    (let* ((name (gfm-pretty--decorator-name decorator))
           (dirty (gfm-pretty--state-get name 'dirty-region)))
      (gfm-pretty--state-set name 'dirty-region nil)
      (cond
       ((null dirty)
        (gfm-pretty--rebuild decorator))
       ((gfm-pretty--dirty-forces-full-rebuild-p decorator dirty)
        (gfm-pretty--rebuild decorator))
       (t
        (gfm-pretty--rebuild-scoped-by-block decorator dirty))))))

(defun gfm-pretty--wcc (&rest _)
  "Engine `window-configuration-change-hook' handler.
Reconciles per-decorator display overlays with the current window
state, deferring rebuilds for added or resized windows by one idle
tick.  Decorators with a `:reconcile-windows-fn' take over the
reconcile entirely (e.g. tables)."
  (unless (buffer-base-buffer)
    (let ((state (gfm-pretty--window-state)))
      (dolist (decorator (gfm-pretty--enabled-decorators))
        (let* ((name (gfm-pretty--decorator-name decorator))
               (custom (gfm-pretty--decorator-reconcile-windows-fn decorator))
               (prev (gfm-pretty--state-get name 'last-window-state)))
          (unless (equal state prev)
            (cond
             (custom (funcall custom))
             (t (gfm-pretty--reconcile-windows decorator)))))))))

(defun gfm-pretty--reveal-for (decorator prop saved-prop)
  "Run the standard reveal algorithm for DECORATOR using PROP / SAVED-PROP.
Restores overlays that point has just left, then suppresses the
`display' of any revealable overlay covering point in the selected
window — saving the prior value under SAVED-PROP first.  Per-decorator
`hidden-ovs' bookkeeping lives in `gfm-pretty--state'."
  (let* ((name (gfm-pretty--decorator-name decorator))
         (custom-pred (gfm-pretty--decorator-revealable-p-fn decorator))
         (pos (point))
         (win (selected-window))
         (hidden (gfm-pretty--state-get name 'hidden-ovs)))
    (setq hidden
          (cl-loop for ov in hidden
                   if (and (overlay-buffer ov)
                           (>= pos (overlay-start ov))
                           (<= pos (overlay-end ov)))
                   collect ov
                   else do (when (overlay-buffer ov)
                             (overlay-put ov 'display
                                          (overlay-get ov saved-prop))
                             (overlay-put ov saved-prop nil))))
    (dolist (ov (overlays-in pos (1+ pos)))
      (when (and (if custom-pred
                     (funcall custom-pred ov)
                   (overlay-get ov prop))
                 (overlay-get ov 'display)
                 (let ((w (overlay-get ov 'window)))
                   (or (null w) (eq w win)))
                 (not (memq ov hidden)))
        (overlay-put ov saved-prop (overlay-get ov 'display))
        (overlay-put ov 'display nil)
        (push ov hidden)))
    (gfm-pretty--state-set name 'hidden-ovs hidden)))

(defun gfm-pretty--reveal ()
  "Engine `post-command-hook' reveal handler.
For each enabled decorator: a `:reveal-fn' takes the whole reveal
(decorators with bespoke cursor semantics — e.g. links use a
shared link-id grouping); otherwise the engine walks the decorator's
revealable overlays via `gfm-pretty--reveal-for' using its
registered `:revealable-prop' + `:saved-display-prop'."
  (dolist (decorator (gfm-pretty--enabled-decorators))
    (let ((reveal-fn (gfm-pretty--decorator-reveal-fn decorator))
          (prop (gfm-pretty--decorator-revealable-prop decorator))
          (saved (gfm-pretty--decorator-saved-display-prop decorator)))
      (cond
       (reveal-fn (funcall reveal-fn))
       ((and prop saved) (gfm-pretty--reveal-for decorator prop saved))))))

;;; Public lifecycle entry points (used by `gfm-pretty-mode')

(defun gfm-pretty--install-engine-hooks ()
  "Install the engine's three lifecycle hooks once per buffer."
  (add-hook 'after-change-functions #'gfm-pretty--after-change nil t)
  (add-hook 'window-configuration-change-hook #'gfm-pretty--wcc nil t)
  (add-hook 'post-command-hook #'gfm-pretty--reveal nil t))

(defun gfm-pretty--remove-engine-hooks ()
  "Remove the engine's three lifecycle hooks; cancel the rebuild timer."
  (remove-hook 'after-change-functions #'gfm-pretty--after-change t)
  (remove-hook 'window-configuration-change-hook #'gfm-pretty--wcc t)
  (remove-hook 'post-command-hook #'gfm-pretty--reveal t)
  (when (timerp gfm-pretty--rebuild-timer)
    (cancel-timer gfm-pretty--rebuild-timer)
    (setq gfm-pretty--rebuild-timer nil)))

(defun gfm-pretty--enable-decorator (decorator)
  "Mark DECORATOR enabled, run its `:on-enable-fn', schedule initial rebuild."
  (let ((name (gfm-pretty--decorator-name decorator))
        (on-enable (gfm-pretty--decorator-on-enable-fn decorator)))
    (gfm-pretty--state-set name 'enabled-p t)
    (when on-enable (funcall on-enable))
    (gfm-pretty--rebuild decorator)))

(defun gfm-pretty--disable-decorator (decorator)
  "Mark DECORATOR disabled, tear down overlays, run its `:on-disable-fn'."
  (let ((name (gfm-pretty--decorator-name decorator))
        (on-disable (gfm-pretty--decorator-on-disable-fn decorator))
        (registry (gfm-pretty--decorator-registry decorator)))
    (gfm-pretty--state-set name 'enabled-p nil)
    (gfm-pretty--state-set name 'dirty-region nil)
    (gfm-pretty--state-set name 'last-window-state nil)
    (gfm-pretty--state-set name 'hidden-ovs nil)
    (gfm-pretty--state-set name 'blocks-cache nil)
    (when on-disable (funcall on-disable))
    (when registry (gfm-pretty--remove-overlays registry))))

(provide 'gfm-pretty-engine)

;;; gfm-pretty-engine.el ends here
