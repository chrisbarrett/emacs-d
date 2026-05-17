;;; +gfm-block-borders.el --- Shared block-border primitives -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared primitives for the GFM markdown decorators
;; (`+gfm-code-fences.el', `+gfm-callouts.el').  Hosts the box-drawing
;; primitives (top/bottom border builders, right-edge after-strings,
;; wrap simulator), an overlay registry parameterised by a tag prefix,
;; the debounced rebuild scheduler, and the per-window state
;; reconciler.
;;
;; Consumers create a `gfm-block-borders-registry' instance bound to
;; their own buffer-local overlay list and pass it through to the
;; registry helpers.  Window reconciliation is parameterised via a
;; `gfm-block-borders-reconciler' carrying the mode-specific callbacks
;; (`collect-blocks', `rebuild-window-block', `rebuild-all') so the
;; same machinery serves both consumers without leaking either one's
;; data shape.

;;; Code:

(require 'cl-lib)

(defgroup gfm-block-borders nil
  "Shared border-drawing primitives for GFM block decorators."
  :group 'markdown-faces)

(defconst gfm-block-borders--wrap-prefix-w 2
  "Visual width of the wrap-prefix shown on continuation visual lines.")

(defcustom gfm-block-borders-icon-gui-nudge 0.25
  "Fractional columns to shift the language icon leftward on GUI frames.
Compensates for icon-font glyphs whose pixel width exceeds the
`string-width' cell count.  Ignored on TTY frames."
  :type 'number
  :group 'gfm-block-borders)

;;; Range helpers

(defun gfm-block-borders--in-ranges-p (pos ranges)
  "Non-nil if POS lies in any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-block-borders--region-overlaps-p (a b)
  "Non-nil if (BEG . END) ranges A and B overlap."
  (and (<= (car a) (cdr b)) (>= (cdr a) (car b))))

;;; Width helpers

(defun gfm-block-borders--available-width (&optional window)
  "Return the available char width for a block in WINDOW.
Falls back to a window currently showing the buffer, then to
`fill-column' or 80."
  (let ((win (or window
                 (get-buffer-window (current-buffer))
                 (get-buffer-window (current-buffer) t))))
    (or (and win (window-max-chars-per-line win))
        fill-column
        80)))

(defun gfm-block-borders--text-width (&optional window)
  "Return WINDOW's max chars per visual line.
Compatibility alias for `gfm-block-borders--available-width'."
  (gfm-block-borders--available-width window))

(defun gfm-block-borders--display-windows ()
  "Return windows currently displaying the buffer."
  (get-buffer-window-list (current-buffer) nil t))

(defun gfm-block-borders--max-line-width (beg end &optional indent)
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

;;; Border primitives

(defun gfm-block-borders--normalised-border-face (face)
  "Return a face spec that inherits FACE but resets text-styling attrs.
Border glyphs share buffer regions with prose whose font-lock face
carries `:slant italic', `:underline t', etc.  Without an explicit
override, those attrs leak through face composition on GUI frames
and visually slant the box edges."
  `(:inherit ,face
    :slant normal :weight normal
    :underline nil :overline nil :strike-through nil :box nil))

(defun gfm-block-borders--top-strings (width face buffer-width &optional icon)
  "Return (LEADING . TRAILING) split of the top border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols (matching the marker line's char count) so
the buffer text shows in place of LEADING when it is revealed; TRAILING
covers the remaining decoration. ICON, if non-nil, is right-aligned."
  (let* ((face (gfm-block-borders--normalised-border-face face))
         (l (propertize "┌" 'face face))
         (r (propertize "┐" 'face face))
         (gap (propertize " " 'face face))
         (leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat l (propertize (make-string leading-dash-w ?─)
                                        'face face))))
    (ignore gap)
    (cond
     (icon
      (let* ((icon-w (string-width icon))
             (nudge (if (display-graphic-p)
                        (max 0 (min 0.99 gfm-block-borders-icon-gui-nudge))
                      0))
             (total-fill-w (max 1 (- width 4 icon-w)))
             (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
             (rem-fill (propertize (make-string rem-fill-w ?─) 'face face))
             (icon-pad (propertize " " 'display
                                   `(space :align-to ,(- width 2 icon-w nudge))
                                   'face face))
             (snap (propertize " " 'display `(space :align-to ,(1- width))
                               'face face)))
        (cons leading (concat rem-fill icon-pad icon snap r))))
     (t
      (let* ((total-fill-w (max 1 (- width 2)))
             (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
             (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
        (cons leading (concat rem-fill r)))))))

(defun gfm-block-borders--bottom-strings (width face buffer-width)
  "Return (LEADING . TRAILING) split of the bottom border WIDTH cols wide.
LEADING covers BUFFER-WIDTH cols matching the marker line's char count."
  (let* ((face (gfm-block-borders--normalised-border-face face))
         (leading-dash-w (max 0 (1- buffer-width)))
         (leading (concat (propertize "└" 'face face)
                          (propertize (make-string leading-dash-w ?─)
                                      'face face)))
         (total-fill-w (max 1 (- width 2)))
         (rem-fill-w (max 1 (- total-fill-w leading-dash-w)))
         (rem-fill (propertize (make-string rem-fill-w ?─) 'face face)))
    (cons leading (concat rem-fill (propertize "┘" 'face face)))))

(defun gfm-block-borders--right-after (box-width face &optional bg)
  "Build the after-string that draws the right border at column BOX-WIDTH.
When BG is non-nil, the padding before the border `│' is painted
with `:background BG' so a body line's highlight band fills up to
the right-side inner gap; nil leaves it on the border face.

The separator immediately before `│' is left on the border face so
the band stops one column short of the border, mirroring the
left-side gap (the space in the `│ ' before-string).  The band thus
sits *inside* the box rather than abutting the right border.

The after-string ends with `(space :align-to right)' painted in the
default face — this fills the visual line from `│' to the window's
right edge with the default background, masking any `:extend t'
past-EOL fill that an underlying face (`diff-added' / `diff-removed'
text-property faces, `hl-line' / `region' overlay faces) would
otherwise paint past the border.  `:extend nil' on an overlay does
NOT clip such a leak — the C-level past-EOL face merge in
`face_at_buffer_position' simply skips faces that opt out, leaving
the leaking face's background intact.  Filling the visual line so
there is no past-EOL region to fill is the working idiom."
  (let* ((face (gfm-block-borders--normalised-border-face face))
         (pad-face (if bg (append face (list :background bg)) face))
         ;; pad ends at `box-width - 3'; the two `sep' cols form the
         ;; default-bg gap so the band sits inset from the right `│'
         ;; (col `box-width - 1') by 2 cols.
         (align-col (- box-width 3))
         (pad (propertize " " 'display `(space :align-to ,align-col)
                          'face pad-face))
         (sep (propertize "  " 'face face))
         (pipe (propertize "│" 'face face))
         (tail (propertize " " 'display '(space :align-to right)
                           'face 'default))
         (str (concat pad sep pipe tail)))
    (put-text-property 0 1 'cursor t str)
    str))

(defun gfm-block-borders--simulate-wrap (text width &optional cont-prefix-w)
  "Simulate word-wrap of TEXT in a WIDTH-col window.
CONT-PREFIX-W is the width of the wrap-prefix shown on continuation
visual lines (default 0).  Returns (LAST-COL . WRAP-POSITIONS).

The per-iteration `line-width' is clamped to at least 1 so the position
counter advances on every loop body, even when WIDTH ≤ CONT-PREFIX-W —
this keeps the function terminating during tiny-window transients
\(e.g. `C-x 3' splits where one window briefly drops below the wrap-prefix
width)."
  (let* ((cont-prefix-w (or cont-prefix-w 0))
         (pos 0)
         (col 0)
         (len (length text))
         (first-line t)
         (wraps nil))
    (while (< pos len)
      (let* ((raw-line-width (if first-line width (- width cont-prefix-w)))
             (line-width (max 1 raw-line-width))
             (remaining (- len pos))
             (space-left (max 1 (- line-width col))))
        (cond
         ((<= remaining space-left)
          (setq col (+ col remaining)
                pos len))
         (t
          (let* ((slice (substring text pos (+ pos space-left)))
                 (wrap-at (cl-position ?\s slice :from-end t))
                 (next-pos (if wrap-at (+ pos wrap-at 1) (+ pos space-left))))
            (push next-pos wraps)
            (setq pos next-pos
                  col 0
                  first-line nil))))))
    (cons (if first-line col (+ col cont-prefix-w))
          (nreverse wraps))))

(defun gfm-block-borders--last-visual-col (text width &optional cont-prefix-w)
  "Estimate last visual column TEXT reaches.
See `gfm-block-borders--simulate-wrap'."
  (car (gfm-block-borders--simulate-wrap text width cont-prefix-w)))

(defun gfm-block-borders--wrap-prefix (face &optional glyph)
  "Wrap-prefix string for continuation lines using FACE.
GLYPH defaults to `⋱ ' (fences).  Callers wanting the box's left edge
on continuation rows pass `│ '."
  (propertize (or glyph "⋱ ") 'face
              (gfm-block-borders--normalised-border-face face)))

(defun gfm-block-borders--right-after-overflow (face line-text window
                                                     &optional cont-prefix-w bg)
  "After-string padding the right border to WINDOW's edge for a wrapped line.
LINE-TEXT is the line's buffer content; the function simulates word-wrap
to work out where the line ends visually, accounting for the wrap-prefix
on continuation lines.  WINDOW selects the width; nil falls back to a
sane default.  CONT-PREFIX-W defaults to
`gfm-block-borders--wrap-prefix-w'.  When BG is non-nil the padding is
painted with `:background BG' so the highlight band fills the gap up
to the border.

A trailing `(space :align-to right)' in the default face fills the
last wrapped visual row from `│' to the window's right edge — see
`gfm-block-borders--right-after' for why."
  (let* ((face (gfm-block-borders--normalised-border-face face))
         (pad-face (if bg (append face (list :background bg)) face))
         (text-width (gfm-block-borders--available-width window))
         ;; Fences/indent use a 3-col left decoration (`│  ') and a
         ;; matching 3-col continuation wrap-prefix.
         (cpw (or cont-prefix-w 3))
         (visual-col (gfm-block-borders--last-visual-col
                      (concat "│  " line-text) text-width cpw))
         ;; Leave a 2-col default-bg gap before the right `│' (at
         ;; col `text-width - 1') by stopping the bg-painted pad at
         ;; col `text-width - 3'.
         (target-col (- text-width 3))
         (pad-len (max 0 (- target-col visual-col)))
         (pad (propertize (make-string pad-len ?\s) 'face pad-face))
         (gap (propertize "  " 'face face))
         (pipe (propertize "│" 'face face))
         (tail (propertize " " 'display '(space :align-to right)
                           'face 'default))
         (str (concat pad gap pipe tail)))
    (put-text-property 0 1 'cursor t str)
    str))

;;; Overlay registry

(cl-defstruct (gfm-block-borders-registry
               (:constructor gfm-block-borders-make-registry)
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

(defun gfm-block-borders-registry-for (tag overlays-symbol
                                           &optional hidden-ovs-symbol)
  "Build a `gfm-block-borders-registry' from TAG and OVERLAYS-SYMBOL.
Sub-property symbols are derived by suffixing TAG (`<tag>-anchor', etc.).
HIDDEN-OVS-SYMBOL is the symbol of the buffer-local list used by reveal."
  (let ((name (symbol-name tag)))
    (gfm-block-borders-make-registry
     :tag tag
     :overlays-symbol overlays-symbol
     :hidden-ovs-symbol hidden-ovs-symbol
     :anchor (intern (concat name "-anchor"))
     :display (intern (concat name "-display"))
     :revealable (intern (concat name "-revealable"))
     :saved-display (intern (concat name "-saved-display")))))

(defun gfm-block-borders--register (registry ov)
  "Tag OV with REGISTRY's tag and remember it for bulk cleanup."
  (overlay-put ov (gfm-block-borders-registry-tag registry) t)
  (push ov (symbol-value (gfm-block-borders-registry-overlays-symbol registry)))
  ov)

(defun gfm-block-borders--remove-overlays (registry &optional beg end)
  "Remove all REGISTRY-tagged overlays between BEG and END.
With both BEG and END nil, widen for the duration of the clear so the
registry list and the on-buffer overlay set stay in lockstep regardless
of any current narrowing; also clear the registry's overlay list and
hidden-ovs list (full reset).  Scoped calls (BEG and/or END non-nil)
operate on the literal range — callers pass real buffer positions."
  (let ((tag (gfm-block-borders-registry-tag registry))
        (list-sym (gfm-block-borders-registry-overlays-symbol registry))
        (hidden-sym (gfm-block-borders-registry-hidden-ovs-symbol registry)))
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

(defun gfm-block-borders--prune-dead-overlays (registry)
  "Drop overlays from REGISTRY whose buffer is gone."
  (let ((list-sym (gfm-block-borders-registry-overlays-symbol registry)))
    (set list-sym (cl-remove-if-not #'overlay-buffer (symbol-value list-sym)))))

(defun gfm-block-borders--remove-display-overlays-in-range
    (registry beg end window)
  "Delete display overlays in [BEG, END] for WINDOW under REGISTRY.
WINDOW non-nil matches only that window's overlays; nil matches every
display overlay.  The registry list is NOT pruned here — call
`gfm-block-borders--prune-dead-overlays' once after a batch."
  (let ((display-prop (gfm-block-borders-registry-display registry)))
    (dolist (ov (overlays-in beg end))
      (when (and (overlay-get ov display-prop)
                 (or (null window)
                     (eq (overlay-get ov 'window) window)))
        (delete-overlay ov)))))

(defun gfm-block-borders--remove-display-overlays-for-window (registry window)
  "Delete every display overlay restricted to WINDOW under REGISTRY."
  (let ((display-prop (gfm-block-borders-registry-display registry))
        (list-sym (gfm-block-borders-registry-overlays-symbol registry)))
    (dolist (ov (symbol-value list-sym))
      (when (and (overlay-buffer ov)
                 (overlay-get ov display-prop)
                 (eq (overlay-get ov 'window) window))
        (delete-overlay ov)))
    (gfm-block-borders--prune-dead-overlays registry)))

(defun gfm-block-borders--make-anchor (registry beg end &rest props)
  "Make an anchor overlay over [BEG, END] under REGISTRY with PROPS."
  (let ((ov (make-overlay beg end nil nil t))
        (tag (gfm-block-borders-registry-tag registry))
        (anchor (gfm-block-borders-registry-anchor registry))
        (list-sym (gfm-block-borders-registry-overlays-symbol registry)))
    (overlay-put ov tag t)
    (overlay-put ov anchor t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    (push ov (symbol-value list-sym))
    ov))

(defun gfm-block-borders--make-display (registry beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW under REGISTRY with PROPS.
WINDOW non-nil restricts the overlay to that window only."
  (let ((ov (make-overlay beg end nil nil t))
        (tag (gfm-block-borders-registry-tag registry))
        (display (gfm-block-borders-registry-display registry))
        (list-sym (gfm-block-borders-registry-overlays-symbol registry)))
    (overlay-put ov tag t)
    (overlay-put ov display t)
    (when window (overlay-put ov 'window window))
    (while props
      (overlay-put ov (pop props) (pop props)))
    (push ov (symbol-value list-sym))
    ov))

;;; Scheduler primitives

(defun gfm-block-borders--extend-dirty-region (region-symbol beg end)
  "Extend the buffer-local cons cell in REGION-SYMBOL to cover BEG..END."
  (let ((current (symbol-value region-symbol)))
    (cond
     ((null current)
      (set region-symbol (cons beg end)))
     (t
      (setcar current (min (car current) beg))
      (setcdr current (max (cdr current) end))))))

(defun gfm-block-borders--arm-rebuild-timer (timer-symbol mode-symbol callback)
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

(defun gfm-block-borders--window-state ()
  "Return the (WINDOW . WIDTH) snapshot used to detect rendering drift."
  (mapcar (lambda (w) (cons w (gfm-block-borders--available-width w)))
          (gfm-block-borders--display-windows)))

(defun gfm-block-borders--range-visible-p (range ranges)
  "Non-nil if (BEG . END) RANGE overlaps any (BEG . END) range in RANGES."
  (cl-some (lambda (r)
             (and (<= (car range) (cdr r))
                  (>= (cdr range) (car r))))
           ranges))

(defun gfm-block-borders--block-visible-p (block ranges range-fn)
  "Non-nil if BLOCK's source range overlaps any range in RANGES.
RANGE-FN extracts (BEG . END) from BLOCK."
  (gfm-block-borders--range-visible-p (funcall range-fn block) ranges))

(defun gfm-block-borders--visible-window-ranges ()
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

(cl-defstruct (gfm-block-borders-reconciler
               (:constructor gfm-block-borders-make-reconciler)
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

(defun gfm-block-borders--rebuild-blocks (rec blocks)
  "Tear down and re-apply BLOCKS using reconciler REC.
Removes overlays in each block's range, then re-applies anchors and
per-window displays.  No stats; consumers track those themselves."
  (let ((registry (gfm-block-borders-reconciler-registry rec))
        (range-fn (gfm-block-borders-reconciler-range-fn rec))
        (apply-anchors (gfm-block-borders-reconciler-apply-anchors-fn rec))
        (apply-display (gfm-block-borders-reconciler-apply-display-fn rec))
        (windows (or (gfm-block-borders--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (funcall range-fn block)))
        (gfm-block-borders--remove-overlays
         registry (car range) (cdr range)))
      (funcall apply-anchors block)
      (dolist (window windows)
        (funcall apply-display block window)))))

(defun gfm-block-borders--rebuild-block-for-window (rec block window)
  "Replace WINDOW's display overlays for BLOCK at the current width."
  (let* ((registry (gfm-block-borders-reconciler-registry rec))
         (range-fn (gfm-block-borders-reconciler-range-fn rec))
         (apply-display (gfm-block-borders-reconciler-apply-display-fn rec))
         (range (funcall range-fn block)))
    (gfm-block-borders--remove-display-overlays-in-range
     registry (car range) (cdr range) window)
    (funcall apply-display block window)
    (gfm-block-borders--prune-dead-overlays registry)))

(defun gfm-block-borders--pace-window-rebuild (rec buf queue window)
  "Render the next BLOCK in QUEUE for WINDOW in BUF, then re-arm idle.
One block per idle tick keeps `C-x 3' / resize transients responsive."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (when (and (buffer-live-p buf) (window-live-p window))
       (with-current-buffer buf
         (when (symbol-value (gfm-block-borders-reconciler-mode-symbol rec))
           (let ((b (car queue))
                 (rest (cdr queue)))
             (gfm-block-borders--rebuild-block-for-window rec b window)
             (cond
              (rest
               (gfm-block-borders--pace-window-rebuild rec buf rest window))
              (t
               (gfm-block-borders--prune-dead-overlays
                (gfm-block-borders-reconciler-registry rec)))))))))))

(defun gfm-block-borders--rebuild-window-prioritised (rec window)
  "Per-block, idle-paced rebuild of WINDOW's display overlays via REC.
Visible blocks first, off-screen ones after, one per idle tick.
`window-end' is read WITHOUT the force-redisplay flag — forcing
redisplay on a freshly-split window is itself expensive."
  (when (window-live-p window)
    (let* ((collect (gfm-block-borders-reconciler-collect-fn rec))
           (range-fn (gfm-block-borders-reconciler-range-fn rec))
           (blocks (funcall collect))
           (vstart (window-start window))
           (vend (window-end window))
           (ranges (and vstart vend (list (cons vstart vend))))
           (visible (and ranges
                         (cl-remove-if-not
                          (lambda (b)
                            (gfm-block-borders--block-visible-p
                             b ranges range-fn))
                          blocks)))
           (offscreen (cl-set-difference blocks visible))
           (queue (append visible offscreen)))
      (when queue
        (gfm-block-borders--pace-window-rebuild
         rec (current-buffer) queue window)))))

(defun gfm-block-borders--reconcile-windows (rec)
  "Reconcile display overlays with current window state via REC.
Removed windows have their display overlays deleted synchronously;
added or resized windows have their rebuild deferred to the next idle
tick.  Falls through to a full rebuild when no anchors are present
yet (first call) or no prior state was recorded."
  (let* ((registry (gfm-block-borders-reconciler-registry rec))
         (anchor-prop (gfm-block-borders-registry-anchor registry))
         (state-sym (gfm-block-borders-reconciler-state-symbol rec))
         (overlays (symbol-value
                    (gfm-block-borders-registry-overlays-symbol registry))))
    (cond
     ((or (null (symbol-value state-sym))
          (null (cl-some (lambda (o) (overlay-get o anchor-prop))
                         overlays)))
      (funcall (gfm-block-borders-reconciler-rebuild-fn rec)))
     (t
      (let* ((prev (symbol-value state-sym))
             (curr (gfm-block-borders--window-state))
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
             (mode-symbol (gfm-block-borders-reconciler-mode-symbol rec)))
        (dolist (w removed)
          (gfm-block-borders--remove-display-overlays-for-window registry w))
        (gfm-block-borders--prune-dead-overlays registry)
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
                       (gfm-block-borders--rebuild-window-prioritised
                        rec w)))))))
           (current-buffer) touched-windows)))))))

(provide '+gfm-block-borders)

;;; +gfm-block-borders.el ends here
