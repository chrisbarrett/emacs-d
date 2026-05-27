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

(defun gfm-pretty-hrule--find-blocks ()
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

;;; Overlay registry

(defconst gfm-pretty-hrule--registry
  (gfm-pretty--registry-for 'hrule 'gfm-pretty-hrule)
  "Shared overlay-registry context for HR bars.")

;;; Block enumeration

(cl-defstruct (gfm-pretty-hrule--block
               (:constructor gfm-pretty-hrule--make-block)
               (:copier nil))
  "Tagged HR block for unified rebuild dispatch."
  range payload)

(defun gfm-pretty-hrule--collect-blocks ()
  "Uncached widened scan returning tagged HR blocks.
The engine memoises this via `gfm-pretty--collect'."
  (mapcar (lambda (b)
            (gfm-pretty-hrule--make-block
             :range (cons (nth 0 b) (1+ (nth 1 b)))
             :payload b))
          (gfm-pretty-hrule--find-blocks)))

;;; Rendering

(defun gfm-pretty-hrule--apply-block (block window)
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
  (let* ((blocks (gfm-pretty--collect (gfm-pretty--get 'hrule)))
         (windows (or (gfm-pretty--display-windows) (list nil))))
    (dolist (window windows)
      (dolist (block blocks)
        (gfm-pretty-hrule--apply-block block window)))))

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'hrule
    :phase              'overlays
    :registry           gfm-pretty-hrule--registry
    :collect-fn         #'gfm-pretty-hrule--collect-blocks
    :range-fn           #'gfm-pretty-hrule--block-range
    :apply-block-fn     #'gfm-pretty-hrule--apply-block
    :rebuild-fn         #'gfm-pretty-hrule--rebuild))

(provide 'gfm-pretty-hrule)

;;; gfm-pretty-hrule.el ends here
