;;; gfm-pretty.el --- Umbrella mode + decorator protocol for GFM visual decoration -*- lexical-binding: t; -*-

;;; Commentary:

;; Single user-facing toggle (`gfm-pretty-mode') plus a decorator
;; registry that exposes a public API for block introspection and
;; per-decorator toggling.
;;
;; The umbrella mode enables / disables the five built-in decorators
;; (callouts, fences, tables, hrule, links) — currently realised as
;; per-decorator minor modes registered via `gfm-pretty-define-decorator'.
;; Future work may collapse the shared lifecycle hooks
;; (`after-change-functions', `window-configuration-change-hook',
;; `post-command-hook', idle timer) into a single engine.

;;; Code:

(require 'cl-lib)

(defgroup gfm-pretty nil
  "Overlay + font-lock visual decoration for GFM markdown buffers."
  :group 'markdown-faces)


;;; Decorator registry

(cl-defstruct gfm-pretty--decorator
  "Metadata for a registered gfm-pretty decorator."
  name
  enable-fn
  disable-fn
  enabled-p-fn
  block-at-point-fn
  edit-at-point-fn)

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

  :enable-fn         function called to enable the decorator in the
                     current buffer.
  :disable-fn        function called to disable it.
  :enabled-p-fn      predicate returning non-nil when the decorator is
                     active in the current buffer.
  :block-at-point-fn optional; returns the decorator's block enclosing
                     point, or nil.
  :edit-at-point-fn  optional; opens an editor on the block at point."
  (declare (indent 1))
  `(setf (alist-get ,name gfm-pretty--decorators)
         (make-gfm-pretty--decorator
          :name ,name
          :enable-fn         ,(plist-get plist :enable-fn)
          :disable-fn        ,(plist-get plist :disable-fn)
          :enabled-p-fn      ,(plist-get plist :enabled-p-fn)
          :block-at-point-fn ,(plist-get plist :block-at-point-fn)
          :edit-at-point-fn  ,(plist-get plist :edit-at-point-fn))))


;;; Lazy-load all decorators

(defun gfm-pretty--require-all ()
  "Load every decorator library so registrations populate the registry."
  (require 'gfm-pretty-borders)
  (require 'gfm-pretty-engine)
  (require 'gfm-pretty-callouts)
  (require 'gfm-pretty-fences)
  (require 'gfm-pretty-tables)
  (require 'gfm-pretty-hrule)
  (require 'gfm-pretty-links))


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
         (enabled (funcall (gfm-pretty--decorator-enabled-p-fn d))))
    (if enabled
        (funcall (gfm-pretty--decorator-disable-fn d))
      (funcall (gfm-pretty--decorator-enable-fn d)))
    (message "gfm-pretty decorator %s: %s"
             name (if enabled "off" "on"))))


;;; Public block introspection

;;;###autoload
(defun gfm-pretty-block-at-point ()
  "Return (DECORATOR-NAME . BLOCK) for the block at point, or nil.

Iterates active decorators; the first whose `:block-at-point-fn'
returns non-nil wins."
  (interactive)
  (gfm-pretty--require-all)
  (cl-loop for (name . d) in gfm-pretty--decorators
           for fn = (gfm-pretty--decorator-block-at-point-fn d)
           for enabled-p = (gfm-pretty--decorator-enabled-p-fn d)
           when (and fn enabled-p (funcall enabled-p))
           for block = (funcall fn)
           when block
           return (cons name block)))

;;;###autoload
(defun gfm-pretty-edit-block-at-point ()
  "Invoke the editor for the gfm-pretty block at point.

Dispatches to the matching decorator's `:edit-at-point-fn'.  Signals a
`user-error' when point is not inside any decorator's block, or the
matching decorator does not provide an editor."
  (interactive)
  (gfm-pretty--require-all)
  (let ((hit (gfm-pretty-block-at-point)))
    (unless hit
      (user-error "gfm-pretty: no decorator block at point"))
    (let* ((d (gfm-pretty--get (car hit)))
           (fn (gfm-pretty--decorator-edit-at-point-fn d)))
      (unless fn
        (user-error "gfm-pretty: decorator %s has no editor" (car hit)))
      (funcall fn))))


;;; Umbrella mode

;;;###autoload
(define-minor-mode gfm-pretty-mode
  "Umbrella mode for GFM visual decoration.

Enables every registered decorator (callouts, fences, tables, hrule,
links) in the current buffer.  Disabling reverses everything."
  :lighter " gfmp"
  (gfm-pretty--require-all)
  (dolist (entry gfm-pretty--decorators)
    (let* ((d (cdr entry))
           (enable  (gfm-pretty--decorator-enable-fn d))
           (disable (gfm-pretty--decorator-disable-fn d)))
      (if gfm-pretty-mode
          (when enable (funcall enable))
        (when disable (funcall disable))))))

(provide 'gfm-pretty)
;;; gfm-pretty.el ends here
