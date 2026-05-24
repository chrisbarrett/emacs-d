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

(defun gfm-pretty--block-at-point-fn (name)
  "Return decorator NAME's `<name>--block-at-point' function, or nil.
The protocol exposes this as a naming convention: a decorator that
wants to participate in `gfm-pretty-block-at-point' exports a
function named `gfm-pretty-<name>--block-at-point' with no args."
  (let ((sym (intern (format "gfm-pretty-%s--block-at-point" name))))
    (and (fboundp sym) sym)))

(defun gfm-pretty--edit-at-point-fn (name)
  "Return decorator NAME's `<name>--edit-at-point' function, or nil.
Naming convention twin of `gfm-pretty--block-at-point-fn'."
  (let ((sym (intern (format "gfm-pretty-%s--edit-at-point" name))))
    (and (fboundp sym) sym)))

;;;###autoload
(defun gfm-pretty-block-at-point ()
  "Return (DECORATOR-NAME . BLOCK) for the block at point, or nil.

Iterates active decorators; the first whose
`gfm-pretty-<name>--block-at-point' returns non-nil wins.  The
contract is a naming convention rather than a registry slot — see
`gfm-pretty--block-at-point-fn'."
  (interactive)
  (gfm-pretty--require-all)
  (cl-loop for (name . _d) in gfm-pretty--decorators
           for fn = (gfm-pretty--block-at-point-fn name)
           when (and fn (gfm-pretty--state-get name 'enabled-p))
           for block = (funcall fn)
           when block
           return (cons name block)))

;;;###autoload
(defun gfm-pretty-edit-block-at-point ()
  "Invoke the editor for the gfm-pretty block at point.

Dispatches to the matching decorator's
`gfm-pretty-<name>--edit-at-point'.  Signals a `user-error' when
point is not inside any decorator's block, or the matching decorator
does not provide an editor."
  (interactive)
  (gfm-pretty--require-all)
  (let ((hit (gfm-pretty-block-at-point)))
    (unless hit
      (user-error "gfm-pretty: no decorator block at point"))
    (let ((fn (gfm-pretty--edit-at-point-fn (car hit))))
      (unless fn
        (user-error "gfm-pretty: decorator %s has no editor" (car hit)))
      (funcall fn))))


;;; Umbrella mode

;;;###autoload
(define-minor-mode gfm-pretty-mode
  "Umbrella mode for GFM visual decoration.

Installs the engine's lifecycle hooks once per buffer and enables every
registered decorator (callouts, fences, tables, hrule, links) in
registration order.  Disabling tears them down in reverse order and
removes the hooks."
  :lighter " gfmp"
  (gfm-pretty--require-all)
  (cond
   (gfm-pretty-mode
    (gfm-pretty--install-engine-hooks)
    (dolist (entry gfm-pretty--decorators)
      (gfm-pretty--enable-decorator (cdr entry))))
   (t
    (dolist (entry (reverse gfm-pretty--decorators))
      (gfm-pretty--disable-decorator (cdr entry)))
    (gfm-pretty--remove-engine-hooks))))

(provide 'gfm-pretty)
;;; gfm-pretty.el ends here
