;;; gfm-pretty-hrule.el --- Full-width unicode bar overlays for GFM thematic breaks -*- lexical-binding: t; -*-

;;; Commentary:

;; Decorator that replaces qualifying GFM dash-form thematic break
;; lines (`---`, `----`, …) with a single window-width unicode
;; horizontal bar.  Lifecycle (hooks, idle timer, dirty-region
;; tracking) is owned by `gfm-pretty-engine.el'; this file registers
;; intent (discovery + per-window display) and the engine drives the
;; rest.
;;
;; Discovery reads the `markdown-hr' text property set by
;; `markdown-syntax-propertize-hrs', which already excludes setext-2
;; heading underlines and code-block contents, then filters to ranges
;; whose underlying source starts with `-' so `***' and `___' forms
;; pass through to markdown-mode's font-lock unchanged.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty-engine)

(defgroup gfm-pretty-hrule nil
  "Full-width unicode bar overlays for GFM thematic breaks."
  :group 'markdown-faces)

(defface gfm-pretty-hrule-face
  '((t :inherit shadow))
  "Face for the unicode bar rendered over GFM dash-form HR lines."
  :group 'gfm-pretty-hrule)

(define-obsolete-face-alias '+markdown-gfm-pretty-hrule-face
  'gfm-pretty-hrule-face "29.1")

;;; Block discovery

(defun gfm-pretty-hrule--find-blocks-1 ()
  "Scan the widened buffer for dash-form HR blocks (uncached).
Each entry is (BEG END) covering BOL to EOL of the HR line.

Reads ranges with the `markdown-hr' text property set by
`markdown-syntax-propertize-hrs', filters to those whose first
non-whitespace character is `-' (dash form)."
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

(defvar-local gfm-pretty-hrule--blocks-cache nil
  "Pair (TICK . BLOCKS) memoising `gfm-pretty-hrule--find-blocks'.
TICK is `buffer-chars-modified-tick' at the time of scan.")

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

;;; Block enumeration

(cl-defstruct (gfm-pretty-hrule--block
               (:constructor gfm-pretty-hrule--make-block)
               (:copier nil))
  "Tagged HR block for unified rebuild dispatch."
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
`gfm-pretty-hrule-face', where WIDTH is the window's available
character width."
  (save-restriction
    (widen)
    (cl-destructuring-bind (beg end) (gfm-pretty-hrule--block-payload block)
      (let* ((width (gfm-pretty--available-width window))
             (bar (propertize (make-string (max 1 width) ?─)
                              'face 'gfm-pretty-hrule-face)))
        (gfm-pretty--make-display
         gfm-pretty-hrule--registry
         beg end window
         'gfm-pretty-hrule-kind 'bar
         'gfm-pretty-hrule-revealable t
         'evaporate t
         'display bar)))))

(defun gfm-pretty-hrule--ensure-syntax-propertize ()
  "Force `syntax-propertize' over the widened buffer.
`markdown-hr' is set by `markdown-syntax-propertize-hrs', which runs
lazily; we depend on it being populated before discovery."
  (save-restriction
    (widen)
    (syntax-propertize (point-max))))

(defun gfm-pretty-hrule--rebuild ()
  "Remove and recreate all gfm-pretty-hrule overlays."
  (gfm-pretty--remove-overlays gfm-pretty-hrule--registry)
  (gfm-pretty-hrule--ensure-syntax-propertize)
  (let* ((blocks (gfm-pretty-hrule--collect-blocks))
         (windows (or (gfm-pretty--display-windows) (list nil))))
    (dolist (window windows)
      (dolist (block blocks)
        (gfm-pretty-hrule--apply-block-display block window)))))

;;; Cursor-driven reveal (selected-window aware)

(defun gfm-pretty-hrule--reveal ()
  "Suppress display on the selected window's revealable overlays at point."
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

;;; gfm-pretty decorator registration

;;;###autoload
(define-minor-mode gfm-pretty-hrule-mode
  "Render GFM dash-form thematic breaks as full-width unicode bars.
Compatibility shim around the engine — prefer `gfm-pretty-mode'
plus `gfm-pretty-toggle-decorator' for per-decorator control."
  :lighter " gfm-hr"
  (cond
   (gfm-pretty-hrule-mode
    (gfm-pretty--install-engine-hooks)
    (gfm-pretty--enable-decorator (gfm-pretty--get 'hrule)))
   (t
    (gfm-pretty--disable-decorator (gfm-pretty--get 'hrule))
    (unless (cl-some (lambda (entry)
                       (gfm-pretty--state-get (car entry) 'enabled-p))
                     gfm-pretty--decorators)
      (gfm-pretty--remove-engine-hooks)))))

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'hrule
    :registry           gfm-pretty-hrule--registry
    :collect-fn         #'gfm-pretty-hrule--collect-blocks
    :range-fn           #'gfm-pretty-hrule--block-range
    :apply-anchors-fn   #'gfm-pretty-hrule--apply-block-anchors
    :apply-display-fn   #'gfm-pretty-hrule--apply-block-display
    :rebuild-fn         #'gfm-pretty-hrule--rebuild
    :revealable-prop    'gfm-pretty-hrule-revealable
    :saved-display-prop 'gfm-pretty-hrule-saved-display
    :reveal-fn          #'gfm-pretty-hrule--reveal))

(provide 'gfm-pretty-hrule)

;;; gfm-pretty-hrule.el ends here
