;;; gfm-pretty.el --- Umbrella mode + decorator protocol for GFM visual decoration -*- lexical-binding: t; -*-

;;; Commentary:

;; Single user-facing toggle (`gfm-pretty-mode') plus a decorator
;; registry that exposes a public API for block introspection and
;; per-decorator toggling.
;;
;; The umbrella mode installs the engine's lifecycle hooks
;; (`gfm-pretty-engine.el') and runs every registered decorator's
;; `:on-enable' hook in registration order.  Disabling reverses the
;; order and removes the hooks.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty-engine)

(defgroup gfm-pretty nil
  "Overlay + font-lock visual decoration for GFM markdown buffers."
  :group 'markdown-faces)


;;; Lazy-load all decorators

(defun gfm-pretty--require-all ()
  "Load every decorator library so registrations populate the registry."
  (require 'gfm-pretty-borders)
  (require 'gfm-pretty-engine)
  (require 'gfm-pretty-callouts)
  (require 'gfm-pretty-blockquotes)
  (require 'gfm-pretty-fences)
  (require 'gfm-pretty-tables)
  (require 'gfm-pretty-hrule)
  (require 'gfm-pretty-links)
  (require 'gfm-pretty-link-previews))


;;; Per-decorator toggle

;;;###autoload
(defun gfm-pretty-toggle-decorator (name)
  "Toggle the gfm-pretty decorator named NAME in the current buffer.

Without prefix arg, prompts for NAME from the registry."
  (interactive
   (progn (gfm-pretty--require-all)
          (list (intern (completing-read
                         "Toggle decorator: "
                         (mapcar (lambda (e) (symbol-name (car e)))
                                 gfm-pretty--decorators)
                         nil t)))))
  (gfm-pretty--require-all)
  (let* ((d (gfm-pretty--get name))
         (enabled (gfm-pretty--state-get name 'enabled-p)))
    (cond
     (enabled (gfm-pretty--disable-decorator d))
     (t (gfm-pretty--enable-decorator d)))
    (message "gfm-pretty decorator %s: %s"
             name (if enabled "off" "on"))))


;;; Public block introspection

;;;###autoload
(defun gfm-pretty-block-at-point ()
  "Return (DECORATOR-NAME . BLOCK) for the block at point, or nil.

Iterates active decorators; the first whose registered
`:block-at-point-fn' returns non-nil wins."
  (interactive)
  (gfm-pretty--require-all)
  (cl-loop for d in (gfm-pretty--decorators-by-phase)
           for name = (gfm-pretty--decorator-name d)
           for fn = (gfm-pretty--decorator-block-at-point-fn d)
           when (and fn (gfm-pretty--state-get name 'enabled-p))
           for block = (funcall fn)
           when block
           return (cons name block)))

;;;###autoload
(defun gfm-pretty-edit-block-at-point ()
  "Invoke the editor for the gfm-pretty block at point.

Dispatches to the matching decorator's registered `:edit-at-point-fn'.
Signals a `user-error' when point is not inside any decorator's block,
or the matching decorator does not provide an editor."
  (interactive)
  (gfm-pretty--require-all)
  (let ((hit (gfm-pretty-block-at-point)))
    (unless hit
      (user-error "gfm-pretty: no decorator block at point"))
    (let ((fn (gfm-pretty--decorator-edit-at-point-fn
               (gfm-pretty--get (car hit)))))
      (unless fn
        (user-error "gfm-pretty: decorator %s has no editor" (car hit)))
      (funcall fn))))


;;; TAB-key dispatch

(declare-function markdown-on-heading-p "markdown-mode")
(declare-function markdown-cycle "markdown-mode")
(declare-function markdown-table-at-point-p "markdown-mode")
(declare-function markdown-table-forward-cell "markdown-mode")
(declare-function markdown-indent-line "markdown-mode")
(declare-function evil-insert-state-p "evil-states")

(defun gfm-pretty--at-list-item-prefix-p ()
  "Non-nil when point is at or before a list-item marker's content slot.
Matches the prefix `^\\s-*(?:[-*+]|[0-9]+[.)])\\s-+' on the current
line and requires `(point)' to be no greater than the match end."
  (save-excursion
    (let ((p (point)))
      (beginning-of-line)
      (and (looking-at (rx bol
                           (zero-or-more (syntax whitespace))
                           (or (any "-*+")
                               (seq (one-or-more digit) (any ".)")))
                           (one-or-more (syntax whitespace))))
           (<= p (match-end 0))))))

(defun gfm-pretty-tab-dwim ()
  "Context-aware `TAB' dispatch for `gfm-pretty-mode'.

Dispatches by point context, and otherwise does nothing.  Importantly
this never silently inserts whitespace — `markdown-cycle' would
otherwise fall through to `indent-for-tab-command' on callout, fence
and paragraph lines, corrupting block source signatures.

- On a heading line (`markdown-on-heading-p'): invoke `markdown-cycle'
  interactively so heading-visibility cycling keeps its
  `last-command' state machine intact.
- Inside a table (`markdown-table-at-point-p'): invoke
  `markdown-table-forward-cell'.
- On a list-item marker prefix slot AND in evil insert state: invoke
  `markdown-indent-line' once.
- Everywhere else: no-op."
  (interactive)
  (cond
   ((markdown-on-heading-p)
    (call-interactively #'markdown-cycle))
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-forward-cell))
   ((and (gfm-pretty--at-list-item-prefix-p)
         (bound-and-true-p evil-mode)
         (fboundp 'evil-insert-state-p)
         (evil-insert-state-p))
    (markdown-indent-line))))

(defvar-keymap gfm-pretty-mode-map
  :doc "Keymap activated by `gfm-pretty-mode'."
  "TAB" #'gfm-pretty-tab-dwim)


;;; Umbrella mode

;;;###autoload
(define-minor-mode gfm-pretty-mode
  "Umbrella mode for GFM visual decoration.

Installs the engine's lifecycle hooks once per buffer and enables every
registered decorator (callouts, fences, tables, hrule, links) in
registration order.  Disabling tears them down in reverse order and
removes the hooks."
  :lighter " gfmp"
  :keymap gfm-pretty-mode-map
  (gfm-pretty--require-all)
  (cond
   (gfm-pretty-mode
    (gfm-pretty--install-engine-hooks)
    ;; Mark every decorator enabled before any rebuild fires, so cross-decorator state queries (link-previews → blockquotes) succeed regardless of registration order.
    (dolist (d (gfm-pretty--decorators-by-phase))
      (gfm-pretty--state-set (gfm-pretty--decorator-name d) 'enabled-p t))
    (dolist (d (gfm-pretty--decorators-by-phase))
      (gfm-pretty--enable-decorator d)))
   (t
    (dolist (d (reverse (gfm-pretty--decorators-by-phase)))
      (gfm-pretty--disable-decorator d))
    (gfm-pretty--remove-engine-hooks))))

(provide 'gfm-pretty)
;;; gfm-pretty.el ends here
