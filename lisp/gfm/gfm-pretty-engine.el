;;; gfm-pretty-engine.el --- Lifecycle engine for GFM pretty decorators -*- lexical-binding: t; -*-

;;; Commentary:

;; Lifecycle engine shared across every gfm-pretty decorator: overlay
;; registry, scheduler primitives, window-state tracking, and the
;; per-window reconciler.  The graphics toolkit
;; (`gfm-pretty-borders.el') is decorator-neutral and depended upon
;; here for window-width helpers.
;;
;; Decorators register an overlay tag and reconciler callbacks via the
;; structures defined in this file; future passes will absorb the
;; remaining per-decorator lifecycle (after-change hooks,
;; post-command-hook reveal, idle rebuild timer) into the engine.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty-borders)

;;; Window-list helper

(defun gfm-pretty--display-windows ()
  "Return windows currently displaying the buffer."
  (get-buffer-window-list (current-buffer) nil t))

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

(cl-defstruct (gfm-pretty--reconciler
               (:constructor gfm-pretty--make-reconciler)
               (:copier nil))
  "Per-mode reconciliation context.

REGISTRY is the mode's overlay registry.  STATE-SYMBOL,
DIRTY-REGION-SYMBOL, and TIMER-SYMBOL are buffer-local variable names.
MODE-SYMBOL names the minor mode toggle.

COLLECT-FN returns the buffer's blocks (a list).  RANGE-FN extracts
\(BEG . END) from a block.  APPLY-DISPLAY-FN takes (block window) and
materialises that window's display overlays for the block.  REBUILD-FN
takes no args and performs a full rebuild (anchors + displays).
APPLY-ANCHORS-FN applies a single block's anchors (used in scoped or
visible-first rebuilds)."
  registry
  state-symbol
  dirty-region-symbol
  timer-symbol
  mode-symbol
  collect-fn
  range-fn
  apply-anchors-fn
  apply-display-fn
  rebuild-fn)

(defun gfm-pretty--rebuild-blocks (rec blocks)
  "Tear down and re-apply BLOCKS using reconciler REC.
Removes overlays in each block's range, then re-applies anchors and
per-window displays.  No stats; consumers track those themselves."
  (let ((registry (gfm-pretty--reconciler-registry rec))
        (range-fn (gfm-pretty--reconciler-range-fn rec))
        (apply-anchors (gfm-pretty--reconciler-apply-anchors-fn rec))
        (apply-display (gfm-pretty--reconciler-apply-display-fn rec))
        (windows (or (gfm-pretty--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (funcall range-fn block)))
        (gfm-pretty--remove-overlays
         registry (car range) (cdr range)))
      (funcall apply-anchors block)
      (dolist (window windows)
        (funcall apply-display block window)))))

(defun gfm-pretty--rebuild-block-for-window (rec block window)
  "Replace WINDOW's display overlays for BLOCK at the current width."
  (let* ((registry (gfm-pretty--reconciler-registry rec))
         (range-fn (gfm-pretty--reconciler-range-fn rec))
         (apply-display (gfm-pretty--reconciler-apply-display-fn rec))
         (range (funcall range-fn block)))
    (gfm-pretty--remove-display-overlays-in-range
     registry (car range) (cdr range) window)
    (funcall apply-display block window)
    (gfm-pretty--prune-dead-overlays registry)))

(defun gfm-pretty--pace-window-rebuild (rec buf queue window)
  "Render the next BLOCK in QUEUE for WINDOW in BUF, then re-arm idle.
One block per idle tick keeps `C-x 3' / resize transients responsive."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (when (and (buffer-live-p buf) (window-live-p window))
       (with-current-buffer buf
         (when (symbol-value (gfm-pretty--reconciler-mode-symbol rec))
           (let ((b (car queue))
                 (rest (cdr queue)))
             (gfm-pretty--rebuild-block-for-window rec b window)
             (cond
              (rest
               (gfm-pretty--pace-window-rebuild rec buf rest window))
              (t
               (gfm-pretty--prune-dead-overlays
                (gfm-pretty--reconciler-registry rec)))))))))))

(defun gfm-pretty--rebuild-window-prioritised (rec window)
  "Per-block, idle-paced rebuild of WINDOW's display overlays via REC.
Visible blocks first, off-screen ones after, one per idle tick.
`window-end' is read WITHOUT the force-redisplay flag — forcing
redisplay on a freshly-split window is itself expensive."
  (when (window-live-p window)
    (let* ((collect (gfm-pretty--reconciler-collect-fn rec))
           (range-fn (gfm-pretty--reconciler-range-fn rec))
           (blocks (funcall collect))
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
         rec (current-buffer) queue window)))))

(defun gfm-pretty--reconcile-windows (rec)
  "Reconcile display overlays with current window state via REC.
Removed windows have their display overlays deleted synchronously;
added or resized windows have their rebuild deferred to the next idle
tick.  Falls through to a full rebuild when no anchors are present
yet (first call) or no prior state was recorded."
  (let* ((registry (gfm-pretty--reconciler-registry rec))
         (anchor-prop (gfm-pretty--registry-anchor registry))
         (state-sym (gfm-pretty--reconciler-state-symbol rec))
         (overlays (symbol-value
                    (gfm-pretty--registry-overlays-symbol registry))))
    (cond
     ((or (null (symbol-value state-sym))
          (null (cl-some (lambda (o) (overlay-get o anchor-prop))
                         overlays)))
      (funcall (gfm-pretty--reconciler-rebuild-fn rec)))
     (t
      (let* ((prev (symbol-value state-sym))
             (curr (gfm-pretty--window-state))
             (prev-keys (mapcar #'car prev))
             (curr-keys (mapcar #'car curr))
             (added (cl-remove-if (lambda (e) (memq (car e) prev-keys)) curr))
             (removed (cl-remove-if (lambda (w) (memq w curr-keys)) prev-keys))
             (resized (cl-remove-if-not
                       (lambda (e)
                         (let ((old (assq (car e) prev)))
                           (and old (not (eql (cdr e) (cdr old))))))
                       curr))
             (touched-windows (mapcar #'car (append added resized)))
             (mode-symbol (gfm-pretty--reconciler-mode-symbol rec)))
        (dolist (w removed)
          (gfm-pretty--remove-display-overlays-for-window registry w))
        (gfm-pretty--prune-dead-overlays registry)
        (set state-sym curr)
        (when touched-windows
          (run-with-idle-timer
           0 nil
           (lambda (buf wins)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (symbol-value mode-symbol)
                   (dolist (w wins)
                     (when (window-live-p w)
                       (gfm-pretty--rebuild-window-prioritised
                        rec w)))))))
           (current-buffer) touched-windows)))))))

(provide 'gfm-pretty-engine)

;;; gfm-pretty-engine.el ends here
