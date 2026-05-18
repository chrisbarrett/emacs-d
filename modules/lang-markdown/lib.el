;;; lang-markdown/lib.el --- Markdown library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Composition-layer helpers for the lang-markdown module.  Visual
;; behaviour (callouts, faces, font-lock) lives in `gfm-pretty' under
;; `lisp/gfm/'.

;;; Code:

(require 'markdown-mode)

;;;###autoload
(defun +markdown-tab-dwim ()
  "Try to expand a snippet, otherwise fall back to `markdown-cycle'.
This function first attempts tempel snippet expansion. If no snippet
expansion occurs, it falls back to the default `markdown-cycle' behavior."
  (interactive)
  (or (when (fboundp 'tempel-expand)
        (condition-case nil
            (tempel-expand t)
          (user-error nil)))
      (markdown-cycle)))

;;; Workarounds

(defvar +markdown--lang-mode-cache (make-hash-table :test 'equal)
  "Memo for `markdown-get-lang-mode': maps LANG string → mode symbol.
Markdown calls the lookup once per native-fontified code block per
redisplay; on a buffer with many code blocks this dominates CPU.")

(defun +markdown--memoise-lang-mode (orig lang)
  "Around-advice on `markdown-get-lang-mode' caching results by LANG.
ORIG is the underlying function.  Treat the absence of an entry, not a
nil value, as the miss signal so blocks tagged with an unknown LANG
also short-circuit on subsequent calls."
  (let ((hit (gethash lang +markdown--lang-mode-cache 'miss)))
    (if (eq hit 'miss)
        (let ((mode (funcall orig lang)))
          (puthash lang mode +markdown--lang-mode-cache)
          mode)
      hit)))

(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-get-lang-mode :around #'+markdown--memoise-lang-mode))

(defun +markdown--clamp-extend-region (result)
  "Filter-return advice for `markdown-syntax-propertize-extend-region'.
Clamp the returned (NEW-START . NEW-END) cons to the buffer's accessible
portion.  During an undo, jit-lock's after-change handler invokes the
extender for each undone hunk; markdown's heuristic occasionally returns
NEW-END > `point-max' (the buffer is transiently shorter mid-undo),
which `syntax-propertize' rejects with \"Cannot syntax-propertize ...
because of narrowing!\".  Clamping keeps the extender honest without
changing semantics on a fully-restored buffer."
  (when result
    (cons (max (point-min) (car result))
          (min (point-max) (cdr result)))))

(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-syntax-propertize-extend-region
              :filter-return #'+markdown--clamp-extend-region))

;;; lib.el ends here
