;;; gfm-pretty-hrule.el --- Full-width unicode bar overlays for GFM thematic breaks -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that replaces qualifying GFM dash-form thematic break
;; lines (`---`, `----`, …) with a single window-width unicode
;; horizontal bar.  Sibling to `gfm-pretty-callouts-mode',
;; `gfm-pretty-fences-mode', and `gfm-pretty-tables-mode'; reuses the shared
;; primitives in `gfm-pretty-borders.el' for the overlay registry,
;; per-window display, debounced rebuild, and cursor-aware reveal.
;;
;; Discovery reads the `markdown-hr' text property set by
;; `markdown-syntax-propertize-hrs', which already excludes setext-2
;; heading underlines and code-block contents, then filters to ranges
;; whose underlying source starts with `-' so `***' and `___' forms
;; pass through to markdown-mode's font-lock unchanged.
;;
;; Per-window display: one overlay per (window × HR line) whose
;; `display' is `(make-string WIDTH ?─)' propertized with
;; `+markdown-gfm-pretty-hrule-face'.  WIDTH comes from
;; `gfm-pretty--available-width' so the bar tracks each
;; displaying window's character width.
;;
;; Reveal-on-cursor uses the shared protocol (`gfm-pretty-hrule-revealable'
;; flag on each display overlay).  Reveal is scoped to the selected
;; window so a cursor in window A does not expose the source in
;; window B.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty-borders)

(defgroup gfm-pretty-hrule nil
  "Full-width unicode bar overlays for GFM thematic breaks."
  :group 'markdown-faces)

(defface +markdown-gfm-pretty-hrule-face
  '((t :inherit shadow))
  "Face for the unicode bar rendered over GFM dash-form HR lines."
  :group 'gfm-pretty-hrule)

;;; Block discovery (cached)

(defvar-local gfm-pretty-hrule--blocks-cache nil
  "Pair (TICK . BLOCKS) memoising `gfm-pretty-hrule--find-blocks'.
TICK is `buffer-chars-modified-tick' at the time of scan.")

(defun gfm-pretty-hrule--find-blocks-1 ()
  "Scan the widened buffer for dash-form HR blocks (uncached).
Each entry is (BEG END) covering BOL to EOL of the HR line.

Reads ranges with the `markdown-hr' text property set by
`markdown-syntax-propertize-hrs', filters to those whose first
non-whitespace character is `-' (dash form), and explicitly
excludes lines whose first non-whitespace character is `>' as a
belt-and-braces guard against blockquote-nested HR-like text.

Widens for the duration of its body so the cache key
\(`buffer-chars-modified-tick') is a pure function of buffer
contents regardless of any current narrowing."
  (let (blocks)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (let ((pos (point-min))
                (limit (point-max)))
            (while (and pos (< pos limit))
              (setq pos (text-property-not-all pos limit 'markdown-hr nil))
              (when pos
                (let* ((end (or (next-single-property-change
                                 pos 'markdown-hr nil limit)
                                limit))
                       (bol (save-excursion
                              (goto-char pos) (line-beginning-position)))
                       (eol (save-excursion
                              (goto-char pos) (line-end-position)))
                       (first-non-ws
                        (save-excursion
                          (goto-char bol)
                          (skip-chars-forward " \t" eol)
                          (point)))
                       (first-char (and (< first-non-ws eol)
                                        (char-after first-non-ws))))
                  (when (and first-char
                             (eq first-char ?-))
                    (push (list bol eol) blocks))
                  (setq pos (max end (1+ pos))))))))))
    (nreverse blocks)))

(defun gfm-pretty-hrule--find-blocks ()
  "Return all dash-form HR blocks in the current buffer.
Memoised by `buffer-chars-modified-tick' so repeat calls without an
intervening edit reuse the cached scan."
  (let ((tick (buffer-chars-modified-tick)))
    (cond
     ((and gfm-pretty-hrule--blocks-cache
           (= tick (car gfm-pretty-hrule--blocks-cache)))
      (cdr gfm-pretty-hrule--blocks-cache))
     (t
      (let ((blocks (gfm-pretty-hrule--find-blocks-1)))
        (setq gfm-pretty-hrule--blocks-cache (cons tick blocks))
        blocks)))))

;;; Overlay registry

(defvar gfm-pretty-hrule-mode)

(defvar-local gfm-pretty-hrule--overlays nil
  "All gfm-pretty-hrule overlays currently in this buffer.")

(defvar-local gfm-pretty-hrule--hidden-ovs nil
  "Revealable overlays whose display is currently suppressed.")

(defconst gfm-pretty-hrule--registry
  (gfm-pretty--registry-for
   'gfm-pretty-hrule
   'gfm-pretty-hrule--overlays
   'gfm-pretty-hrule--hidden-ovs)
  "Shared overlay-registry context for HR bars.")

(defsubst gfm-pretty-hrule--make-display (beg end window &rest props)
  "Make a display overlay over [BEG, END] for WINDOW with PROPS."
  (apply #'gfm-pretty--make-display
         gfm-pretty-hrule--registry beg end window props))

(defsubst gfm-pretty-hrule--remove-overlays (&optional beg end)
  "Remove all gfm-pretty-hrule overlays between BEG and END."
  (gfm-pretty--remove-overlays gfm-pretty-hrule--registry beg end))

;;; Block enumeration

(cl-defstruct (gfm-pretty-hrule--block
               (:constructor gfm-pretty-hrule--make-block)
               (:copier nil))
  "Tagged HR block for unified rebuild dispatch.
RANGE is (LINE-BEG . LINE-END+1) covering the full source range,
used for visibility + scoped-rebuild containment.  PAYLOAD is the
raw (BEG END) pair."
  range payload)

(defun gfm-pretty-hrule--collect-blocks ()
  "Return tagged HR blocks for the buffer."
  (mapcar (lambda (b)
            (gfm-pretty-hrule--make-block
             :range (cons (nth 0 b) (1+ (nth 1 b)))
             :payload b))
          (gfm-pretty-hrule--find-blocks)))

;;; Rendering

(defun gfm-pretty-hrule--apply-block-anchors (_block)
  "HR blocks carry no anchor overlays; placeholder for the reconciler.")

(defun gfm-pretty-hrule--apply-block-display (block window)
  "Apply a per-WINDOW display overlay for HR BLOCK.
The overlay's `display' is `(make-string WIDTH ?─)' propertized with
`+markdown-gfm-pretty-hrule-face', where WIDTH is the window's available
character width.

Widens for the duration so a BLOCK whose source range lies outside
the current restriction can still be decorated; display under
narrowing is naturally clipped by Emacs' overlay engine."
  (save-restriction
    (widen)
    (cl-destructuring-bind (beg end) (gfm-pretty-hrule--block-payload block)
      (let* ((width (gfm-pretty--available-width window))
             (bar (propertize (make-string (max 1 width) ?─)
                              'face '+markdown-gfm-pretty-hrule-face)))
        (gfm-pretty-hrule--make-display
         beg end window
         'gfm-pretty-hrule-kind 'bar
         'gfm-pretty-hrule-revealable t
         'evaporate t
         'display bar)))))

(defun gfm-pretty-hrule--apply-overlays ()
  "Create per-window display overlays for every HR block in the buffer.
Returns the block count."
  (save-excursion
    (let* ((blocks (gfm-pretty-hrule--collect-blocks))
           (windows (or (gfm-pretty--display-windows) (list nil))))
      (dolist (window windows)
        (dolist (block blocks)
          (gfm-pretty-hrule--apply-block-display block window)))
      (length blocks))))

;;; Cursor-driven reveal (selected-window aware)

(defun gfm-pretty-hrule--reveal ()
  "Suppress display on the selected window's revealable overlays at point.
Restores overlays no longer at point.  Per-window: only overlays
whose `window' property is nil or matches the selected window are
toggled, so cursor in window A does not expose source in window B."
  (let ((pos (point))
        (win (selected-window)))
    (setq gfm-pretty-hrule--hidden-ovs
          (cl-loop for ov in gfm-pretty-hrule--hidden-ovs
                   if (and (overlay-buffer ov)
                           (>= pos (overlay-start ov))
                           (<= pos (overlay-end ov)))
                   collect ov
                   else do (when (overlay-buffer ov)
                             (overlay-put ov 'display
                                          (overlay-get ov 'gfm-pretty-hrule-saved-display))
                             (overlay-put ov 'gfm-pretty-hrule-saved-display nil))))
    (dolist (ov (overlays-in pos (1+ pos)))
      (when (and (overlay-get ov 'gfm-pretty-hrule-revealable)
                 (overlay-get ov 'display)
                 (let ((w (overlay-get ov 'window)))
                   (or (null w) (eq w win)))
                 (not (memq ov gfm-pretty-hrule--hidden-ovs)))
        (overlay-put ov 'gfm-pretty-hrule-saved-display
                     (overlay-get ov 'display))
        (overlay-put ov 'display nil)
        (push ov gfm-pretty-hrule--hidden-ovs)))))

;;; Rebuild scheduler state

(defvar-local gfm-pretty-hrule--last-window-state nil
  "Snapshot of the windows showing the buffer at the last rebuild.")

(defvar-local gfm-pretty-hrule--dirty-region nil
  "Buffer-local (BEG . END) covering all unrebuilt edits.")

(defvar-local gfm-pretty-hrule--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defun gfm-pretty-hrule--ensure-syntax-propertize ()
  "Force `syntax-propertize' over the widened buffer.
`markdown-hr' is set by `markdown-syntax-propertize-hrs', which runs
lazily; we depend on it being populated before discovery."
  (save-restriction
    (widen)
    (syntax-propertize (point-max))))

(defun gfm-pretty-hrule--rebuild ()
  "Remove and recreate all gfm-pretty-hrule overlays."
  (gfm-pretty-hrule--remove-overlays)
  (setq gfm-pretty-hrule--dirty-region nil)
  (gfm-pretty-hrule--ensure-syntax-propertize)
  (gfm-pretty-hrule--apply-overlays)
  (setq gfm-pretty-hrule--last-window-state
        (gfm-pretty--window-state)))

(defun gfm-pretty-hrule--rebuild-block (block)
  "Tear down BLOCK's overlays and re-apply just that block."
  (let ((range (gfm-pretty-hrule--block-range block)))
    (gfm-pretty-hrule--remove-overlays (car range) (cdr range))
    (dolist (window (or (gfm-pretty--display-windows) (list nil)))
      (gfm-pretty-hrule--apply-block-display block window))))

(defun gfm-pretty-hrule--rebuild-blocks (blocks)
  "Tear down each block in BLOCKS and re-apply them in one pass."
  (let ((windows (or (gfm-pretty--display-windows) (list nil))))
    (dolist (block blocks)
      (let ((range (gfm-pretty-hrule--block-range block)))
        (gfm-pretty-hrule--remove-overlays (car range) (cdr range)))
      (dolist (window windows)
        (gfm-pretty-hrule--apply-block-display block window)))))

(defconst gfm-pretty-hrule--reconciler
  (gfm-pretty--make-reconciler
   :registry gfm-pretty-hrule--registry
   :state-symbol 'gfm-pretty-hrule--last-window-state
   :dirty-region-symbol 'gfm-pretty-hrule--dirty-region
   :timer-symbol 'gfm-pretty-hrule--rebuild-timer
   :mode-symbol 'gfm-pretty-hrule-mode
   :collect-fn #'gfm-pretty-hrule--collect-blocks
   :range-fn #'gfm-pretty-hrule--block-range
   :apply-anchors-fn #'gfm-pretty-hrule--apply-block-anchors
   :apply-display-fn #'gfm-pretty-hrule--apply-block-display
   :rebuild-fn #'gfm-pretty-hrule--rebuild)
  "Shared reconciler context for HR bars.")

(defun gfm-pretty-hrule--reconcile-windows ()
  "Reconcile display overlays with current window state."
  (gfm-pretty--reconcile-windows gfm-pretty-hrule--reconciler))

(defsubst gfm-pretty-hrule--extend-dirty-region (beg end)
  "Extend the buffer's dirty region to cover BEG..END."
  (gfm-pretty--extend-dirty-region
   'gfm-pretty-hrule--dirty-region beg end))

(defsubst gfm-pretty-hrule--arm-rebuild-timer (callback)
  "Cancel any pending rebuild timer and schedule CALLBACK after idle."
  (gfm-pretty--arm-rebuild-timer
   'gfm-pretty-hrule--rebuild-timer 'gfm-pretty-hrule-mode callback))

(defun gfm-pretty-hrule--schedule-rebuild (&optional beg end _len)
  "Merge BEG..END into the dirty region and arm the rebuild timer."
  (unless (buffer-base-buffer)
    (when (and beg end)
      (gfm-pretty-hrule--extend-dirty-region beg end))
    (gfm-pretty-hrule--arm-rebuild-timer #'gfm-pretty-hrule--rebuild)))

(defun gfm-pretty-hrule--schedule-full-rebuild (&rest _)
  "Schedule a window reconciliation if window state has changed."
  (unless (buffer-base-buffer)
    (let ((state (gfm-pretty--window-state)))
      (unless (equal state gfm-pretty-hrule--last-window-state)
        (gfm-pretty-hrule--arm-rebuild-timer
         #'gfm-pretty-hrule--reconcile-windows)))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-pretty-hrule-mode
  "Render GFM dash-form thematic breaks as full-width unicode bars."
  :lighter " gfm-hr"
  (if gfm-pretty-hrule-mode
      (progn
        (gfm-pretty-hrule--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-pretty-hrule--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-pretty-hrule--schedule-full-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-pretty-hrule--reveal nil t))
    (remove-hook 'after-change-functions
                 #'gfm-pretty-hrule--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-pretty-hrule--schedule-full-rebuild t)
    (remove-hook 'post-command-hook #'gfm-pretty-hrule--reveal t)
    (when (timerp gfm-pretty-hrule--rebuild-timer)
      (cancel-timer gfm-pretty-hrule--rebuild-timer))
    (gfm-pretty-hrule--remove-overlays)))

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty
  (gfm-pretty-define-decorator 'hrule
    :enable-fn    (lambda () (gfm-pretty-hrule-mode 1))
    :disable-fn   (lambda () (gfm-pretty-hrule-mode -1))
    :enabled-p-fn (lambda () (bound-and-true-p gfm-pretty-hrule-mode))))

(provide 'gfm-pretty-hrule)

;;; gfm-pretty-hrule.el ends here
