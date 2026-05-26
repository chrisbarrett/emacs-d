;;; gfm-pretty-engine.el --- Lifecycle engine for GFM pretty decorators -*- lexical-binding: t; -*-

;;; Commentary:

;; Lifecycle engine shared across every gfm-pretty decorator.  Owns
;; the decorator registry, the overlay registry primitives, the
;; reconciler, the scoped-rebuild router, the generic rebuild-stats
;; wrapper, the per-decorator state plist (overlays, hidden-ovs,
;; rebuild stats, anchors-laid sentinel), and the single set of
;; lifecycle hooks per buffer: one `after-change-functions' handler,
;; one `window-configuration-change-hook' handler, one
;; `post-command-hook' reveal handler, and one idle rebuild timer.
;;
;; Decorators register intent (block discovery, range, per-block
;; apply, optional full-rebuild-required predicate, optional reveal
;; participation, on-enable / on-disable extras) via
;; `gfm-pretty-define-decorator'; the engine drives the lifecycle.
;; The graphics toolkit lives in `gfm-pretty-borders.el'.

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
- `blocks-cache'      (TICK . BLOCKS) memoised collect result
- `overlays'          decorator's overlays (engine-owned)
- `hidden-ovs'        revealable overlays whose display is suppressed
- `anchors-laid'      list of block ranges with anchors laid this pass
- `rebuild-stats'     plist (:count :total :last :max) of generic timing
- `phase-totals'      decorator-published phase timings (alist)
- `last-rebuild-tick' `buffer-chars-modified-tick' captured at the
                      end of the last completed rebuild, or nil
                      (decorator disabled / freshly toggled / revert
                      teardown).  Lets dependents detect overlay
                      state older than the current buffer via
                      `gfm-pretty--stale-p'.")

(defvar-local gfm-pretty--rebuild-timer nil
  "Single idle rebuild timer driving engine-scheduled rebuilds.")

(defun gfm-pretty--state-get (name slot)
  "Return SLOT from decorator NAME's state plist."
  (plist-get (alist-get name gfm-pretty--state) slot))

(defun gfm-pretty--state-set (name slot val)
  "Set SLOT to VAL in decorator NAME's state plist."
  (setf (alist-get name gfm-pretty--state)
        (plist-put (alist-get name gfm-pretty--state) slot val)))

(defun gfm-pretty--state-push (name slot val)
  "Cons VAL onto the front of decorator NAME's SLOT list."
  (gfm-pretty--state-set name slot (cons val (gfm-pretty--state-get name slot))))

(defun gfm-pretty--overlays-for (name)
  "Return decorator NAME's currently tracked overlay list.
Thin accessor for ad-hoc inspection from `M-:'."
  (gfm-pretty--state-get name 'overlays))

;;; Decorator registry

(cl-defstruct gfm-pretty--decorator
  "Metadata for a registered gfm-pretty decorator.
Fields populated via `gfm-pretty-define-decorator'."
  name
  registry
  collect-fn
  range-fn
  apply-block-fn
  on-enable-fn
  on-disable-fn
  rebuild-fn
  full-rebuild-required-p
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
                       overlay tag.  The engine derives every overlay
                       property name (anchor, display, revealable,
                       saved-display) from this registry's `tag'.
  :collect-fn          (no-arg) returns the buffer's blocks, widened.
  :range-fn            (block) returns the block's (BEG . END) source range.
  :apply-block-fn      (block window) applies every overlay required to
                       render BLOCK in WINDOW.  Decorators sharing
                       width-independent state across windows MAY call
                       `gfm-pretty-borders--apply-with-anchors' to
                       factor the anchor / display split internally.
  :rebuild-fn          optional (no-arg) full rebuild; engine-generic
                       teardown + reapply when nil.
  :full-rebuild-required-p
                       optional (region) predicate; non-nil forces a
                       full rebuild for that dirty region.
  :on-enable-fn        optional (no-arg) thunk run on enable.
  :on-disable-fn       optional (no-arg) thunk run on disable.
  :reveal-fn           optional (no-arg) bespoke reveal handler."
  (declare (indent 1))
  `(setf (alist-get ,name gfm-pretty--decorators)
         (make-gfm-pretty--decorator
          :name              ,name
          :registry          ,(plist-get plist :registry)
          :collect-fn        ,(plist-get plist :collect-fn)
          :range-fn          ,(plist-get plist :range-fn)
          :apply-block-fn    ,(plist-get plist :apply-block-fn)
          :on-enable-fn      ,(plist-get plist :on-enable-fn)
          :on-disable-fn     ,(plist-get plist :on-disable-fn)
          :rebuild-fn        ,(plist-get plist :rebuild-fn)
          :full-rebuild-required-p ,(plist-get plist :full-rebuild-required-p)
          :reveal-fn         ,(plist-get plist :reveal-fn))))

;;; Overlay registry

(cl-defstruct (gfm-pretty--registry
               (:constructor gfm-pretty--make-registry)
               (:copier nil))
  "Per-decorator overlay registry context.

TAG is the symbol used as both the bulk overlay-property tag and as
the prefix for derived properties (TAG-anchor, TAG-display,
TAG-revealable, TAG-saved-display).

NAME is the decorator's short identifier (e.g. `'callouts').  The
engine uses it to read and write the decorator's `overlays' and
`hidden-ovs' lists in `gfm-pretty--state'.

ANCHOR/DISPLAY/REVEALABLE/SAVED-DISPLAY are pre-interned to avoid
recomputing the symbol on every overlay creation."
  tag
  name
  anchor
  display
  revealable
  saved-display)

(defun gfm-pretty--registry-for (name tag)
  "Build a `gfm-pretty--registry' for decorator NAME with overlay TAG.
Sub-property symbols are derived by suffixing TAG (`<tag>-anchor', etc.).
`saved-display' is a shared overlay-property name across all decorators
so the selection-aware variant walker in `gfm-pretty-borders.el' can
detect reveal-hidden overlays without per-registry plumbing."
  (let ((tag-name (symbol-name tag)))
    (gfm-pretty--make-registry
     :tag tag
     :name name
     :anchor (intern (concat tag-name "-anchor"))
     :display (intern (concat tag-name "-display"))
     :revealable (intern (concat tag-name "-revealable"))
     :saved-display 'gfm-pretty-saved-display)))

(defun gfm-pretty--register (registry ov)
  "Tag OV with REGISTRY's tag and remember it for bulk cleanup."
  (overlay-put ov (gfm-pretty--registry-tag registry) t)
  (gfm-pretty--state-push (gfm-pretty--registry-name registry) 'overlays ov)
  ov)

(defun gfm-pretty--remove-overlays (registry &optional beg end)
  "Remove all REGISTRY-tagged overlays between BEG and END.
With both BEG and END nil, widen for the duration of the clear so the
state's overlay list and the on-buffer overlay set stay in lockstep
regardless of any current narrowing; also clear the decorator's
overlays + hidden-ovs + anchors-laid state slots (full reset).
Scoped calls (BEG and/or END non-nil) operate on the literal range —
callers pass real buffer positions — and prune the anchors-laid
sentinel for ranges overlapping that scope."
  (let ((tag (gfm-pretty--registry-tag registry))
        (name (gfm-pretty--registry-name registry)))
    (cond
     ((or beg end)
      (remove-overlays (or beg (point-min)) (or end (point-max)) tag t)
      (gfm-pretty--state-set
       name 'overlays
       (cl-remove-if-not #'overlay-buffer (gfm-pretty--state-get name 'overlays)))
      (let ((scope (cons (or beg (point-min)) (or end (point-max)))))
        (gfm-pretty--state-set
         name 'anchors-laid
         (cl-remove-if (lambda (r) (gfm-pretty--region-overlaps-p r scope))
                       (gfm-pretty--state-get name 'anchors-laid)))))
     (t
      (save-restriction
        (widen)
        (remove-overlays (point-min) (point-max) tag t))
      (gfm-pretty--state-set name 'overlays nil)
      (gfm-pretty--state-set name 'hidden-ovs nil)
      (gfm-pretty--state-set name 'anchors-laid nil)))))

(defun gfm-pretty--prune-dead-overlays (registry)
  "Drop overlays from REGISTRY whose buffer is gone."
  (let ((name (gfm-pretty--registry-name registry)))
    (gfm-pretty--state-set
     name 'overlays
     (cl-remove-if-not #'overlay-buffer (gfm-pretty--state-get name 'overlays)))))

(defun gfm-pretty--remove-display-overlays-in-range
    (registry beg end window)
  "Delete display overlays in [BEG, END] for WINDOW under REGISTRY.
WINDOW non-nil matches only that window's overlays; nil matches every
display overlay.  The state's overlay list is NOT pruned here — call
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
        (name (gfm-pretty--registry-name registry)))
    (dolist (ov (gfm-pretty--state-get name 'overlays))
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
        (name (gfm-pretty--registry-name registry)))
    (overlay-put ov tag t)
    (overlay-put ov anchor t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    (gfm-pretty--state-push name 'overlays ov)
    ov))

(defun gfm-pretty--make-display (registry beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW under REGISTRY with PROPS.
WINDOW non-nil restricts the overlay to that window only."
  (let ((ov (make-overlay beg end nil nil t))
        (tag (gfm-pretty--registry-tag registry))
        (display (gfm-pretty--registry-display registry))
        (name (gfm-pretty--registry-name registry)))
    (overlay-put ov tag t)
    (overlay-put ov display t)
    (when window (overlay-put ov 'window window))
    (while props
      (overlay-put ov (pop props) (pop props)))
    (gfm-pretty--state-push name 'overlays ov)
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
  "Tear down and re-apply BLOCKS using DECORATOR's `:apply-block-fn'."
  (let ((registry (gfm-pretty--decorator-registry decorator))
        (range-fn (gfm-pretty--decorator-range-fn decorator))
        (apply-block (gfm-pretty--decorator-apply-block-fn decorator))
        (windows (or (gfm-pretty--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (funcall range-fn block)))
        (gfm-pretty--remove-overlays registry (car range) (cdr range)))
      (dolist (window windows)
        (funcall apply-block block window)))))

(defun gfm-pretty--rebuild-block-for-window (decorator block window)
  "Replace WINDOW's display overlays for BLOCK at the current width."
  (let* ((registry (gfm-pretty--decorator-registry decorator))
         (range-fn (gfm-pretty--decorator-range-fn decorator))
         (apply-block (gfm-pretty--decorator-apply-block-fn decorator))
         (range (funcall range-fn block)))
    (gfm-pretty--remove-display-overlays-in-range
     registry (car range) (cdr range) window)
    (funcall apply-block block window)
    (gfm-pretty--prune-dead-overlays registry)))

(defun gfm-pretty--rebuild-block (decorator block)
  "Tear down BLOCK's overlays and reapply just that block.
Times the operation into DECORATOR NAME's `rebuild-stats' slot.
Stamps `last-rebuild-tick' on completion: the rest of the buffer's
overlays are unchanged and therefore still aligned with the current
buffer tick, so the decorator's overall freshness is restored."
  (let* ((name (gfm-pretty--decorator-name decorator))
         (registry (gfm-pretty--decorator-registry decorator))
         (range-fn (gfm-pretty--decorator-range-fn decorator))
         (apply-block (gfm-pretty--decorator-apply-block-fn decorator))
         (range (funcall range-fn block))
         (windows (or (gfm-pretty--display-windows) (list nil))))
    (gfm-pretty--with-stats name
      (gfm-pretty--remove-overlays registry (car range) (cdr range))
      (dolist (window windows)
        (funcall apply-block block window)))
    (gfm-pretty--state-set name 'last-rebuild-tick
                           (buffer-chars-modified-tick))))

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
         (overlays (gfm-pretty--state-get name 'overlays))
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
a generic teardown + re-apply using `:collect-fn' and `:apply-block-fn'.
Times the call into the engine's per-decorator `rebuild-stats' slot."
  (let* ((rebuild-fn (gfm-pretty--decorator-rebuild-fn decorator))
         (name (gfm-pretty--decorator-name decorator)))
    (gfm-pretty--with-stats name
      (cond
       (rebuild-fn (funcall rebuild-fn))
       (t
        (let ((registry (gfm-pretty--decorator-registry decorator))
              (apply-block (gfm-pretty--decorator-apply-block-fn decorator)))
          (gfm-pretty--remove-overlays registry)
          (let ((blocks (gfm-pretty--collect decorator))
                (windows (or (gfm-pretty--display-windows) (list nil))))
            (dolist (window windows)
              (dolist (block blocks)
                (funcall apply-block block window))))))))
    (gfm-pretty--state-set name 'dirty-region nil)
    (gfm-pretty--state-set name 'last-window-state
                           (gfm-pretty--window-state))
    (gfm-pretty--state-set name 'last-rebuild-tick
                           (buffer-chars-modified-tick))))

(defun gfm-pretty--stale-p (name)
  "Non-nil when decorator NAME's overlays are older than the current buffer.
Returns t when no rebuild has completed (slot nil) or when the slot's
tick is strictly less than `buffer-chars-modified-tick'.  Returns nil
when the slot equals the current tick (overlays in sync)."
  (let ((tick (gfm-pretty--state-get name 'last-rebuild-tick)))
    (or (null tick) (< tick (buffer-chars-modified-tick)))))

;;; Scoped-rebuild routing (engine-owned)

(defun gfm-pretty--dirty-forces-full-rebuild-p (decorator dirty)
  "Non-nil if DIRTY warrants a full DECORATOR rebuild.
Delegates to the decorator's `:full-rebuild-required-p' predicate (run
under `widen').  Returns nil when the decorator did not register one."
  (let ((pred (gfm-pretty--decorator-full-rebuild-required-p decorator)))
    (and pred (save-restriction (widen) (funcall pred dirty)))))

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
For each enabled decorator: if its dirty region triggers the
decorator's `:full-rebuild-required-p', do a full rebuild;
otherwise scope to the fully-contained block when one exists,
falling back to a full rebuild.  Clears the dirty region per
decorator afterwards."
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
tick."
  (unless (buffer-base-buffer)
    (let ((state (gfm-pretty--window-state)))
      (dolist (decorator (gfm-pretty--enabled-decorators))
        (let* ((name (gfm-pretty--decorator-name decorator))
               (prev (gfm-pretty--state-get name 'last-window-state)))
          (unless (equal state prev)
            (gfm-pretty--reconcile-windows decorator)))))))

(defun gfm-pretty--reveal-for (decorator)
  "Run the standard reveal algorithm for DECORATOR.
The revealable and saved-display property names are derived from
DECORATOR's registered `:registry'.  Restores overlays that point has
just left, then suppresses the `display' of any revealable overlay
covering point in the selected window — saving the prior value under
the saved-display property first.  Per-decorator `hidden-ovs'
bookkeeping lives in `gfm-pretty--state'."
  (let* ((name (gfm-pretty--decorator-name decorator))
         (registry (gfm-pretty--decorator-registry decorator))
         (prop (gfm-pretty--registry-revealable registry))
         (saved-prop (gfm-pretty--registry-saved-display registry))
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
      (when (and (overlay-get ov prop)
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
(decorators with bespoke cursor semantics — e.g. links use a shared
link-id grouping); otherwise the engine walks the decorator's
revealable overlays via `gfm-pretty--reveal-for', which derives the
property names from the decorator's `:registry'."
  (dolist (decorator (gfm-pretty--enabled-decorators))
    (let ((reveal-fn (gfm-pretty--decorator-reveal-fn decorator)))
      (cond
       (reveal-fn (funcall reveal-fn))
       (t (gfm-pretty--reveal-for decorator))))))

(defun gfm-pretty--before-revert ()
  "Engine `before-revert-hook' handler.
For each enabled decorator: bulk-remove its overlays via the
registry and nil every position-bearing state slot
\(`dirty-region', `last-window-state', `hidden-ovs', `anchors-laid',
`last-rebuild-tick').  Also cancel the engine's pending idle rebuild
timer so nothing fires against the about-to-be-replaced contents."
  (unless (buffer-base-buffer)
    (dolist (decorator (gfm-pretty--enabled-decorators))
      (let ((name (gfm-pretty--decorator-name decorator))
            (registry (gfm-pretty--decorator-registry decorator)))
        (when registry (gfm-pretty--remove-overlays registry))
        (gfm-pretty--state-set name 'dirty-region nil)
        (gfm-pretty--state-set name 'last-window-state nil)
        (gfm-pretty--state-set name 'hidden-ovs nil)
        (gfm-pretty--state-set name 'anchors-laid nil)
        (gfm-pretty--state-set name 'last-rebuild-tick nil)))
    (when (timerp gfm-pretty--rebuild-timer)
      (cancel-timer gfm-pretty--rebuild-timer)
      (setq gfm-pretty--rebuild-timer nil))))

(defun gfm-pretty--after-revert ()
  "Engine `after-revert-hook' handler.
Runs the scheduled-rebuild routine synchronously so every enabled
decorator has a fresh overlay set — and `last-rebuild-tick' aligned
with the current buffer tick — before control returns to the user.
Any `dirty-region' set by `insert-file-contents REPLACE=t' is cleared
first so the scheduler routes through the full-rebuild path."
  (unless (buffer-base-buffer)
    (dolist (decorator (gfm-pretty--enabled-decorators))
      (gfm-pretty--state-set
       (gfm-pretty--decorator-name decorator) 'dirty-region nil))
    (gfm-pretty--scheduled-rebuild)))

;;; Public lifecycle entry points (used by `gfm-pretty-mode')

(defun gfm-pretty--install-engine-hooks ()
  "Install the engine's lifecycle hooks once per buffer.
The selection-aware variant walker (in `gfm-pretty-borders.el') is
installed here so any decorator that stashes masked / bare variants
on its overlays gets the swap automatically — no per-decorator
post-command-hook wiring required."
  (add-hook 'after-change-functions #'gfm-pretty--after-change nil t)
  (add-hook 'window-configuration-change-hook #'gfm-pretty--wcc nil t)
  (add-hook 'post-command-hook #'gfm-pretty--reveal nil t)
  (add-hook 'post-command-hook #'gfm-pretty--update-selection nil t)
  (add-hook 'before-revert-hook #'gfm-pretty--before-revert nil t)
  (add-hook 'after-revert-hook #'gfm-pretty--after-revert nil t))

(defun gfm-pretty--remove-engine-hooks ()
  "Remove the engine's lifecycle hooks; cancel the rebuild timer."
  (remove-hook 'after-change-functions #'gfm-pretty--after-change t)
  (remove-hook 'window-configuration-change-hook #'gfm-pretty--wcc t)
  (remove-hook 'post-command-hook #'gfm-pretty--reveal t)
  (remove-hook 'post-command-hook #'gfm-pretty--update-selection t)
  (remove-hook 'before-revert-hook #'gfm-pretty--before-revert t)
  (remove-hook 'after-revert-hook #'gfm-pretty--after-revert t)
  (setq gfm-pretty--last-selection-bounds nil)
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
    (gfm-pretty--state-set name 'anchors-laid nil)
    (gfm-pretty--state-set name 'blocks-cache nil)
    (gfm-pretty--state-set name 'last-rebuild-tick nil)
    (when on-disable (funcall on-disable))
    (when registry (gfm-pretty--remove-overlays registry))))

(provide 'gfm-pretty-engine)

;;; gfm-pretty-engine.el ends here
